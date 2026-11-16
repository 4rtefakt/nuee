// File › Export a reproducibility package… and Deposit on Zenodo… (commandes.js: paquet,
// paquet_zenodo). The dialog asks for the package's title, its authors (given names, family name,
// optional ORCID), description, keywords and the licence of its data; the answers are kept in the
// project (moteur/R/paquet.R: p$paquet), so the next export starts from them. Then every model of
// the project not fitted yet is fitted (app.calculerDiscret, as for a document that cites it), each
// document becomes a reproducible Quarto file (documents.js: documentPourPaquet), and the engine
// writes the package (request `paquet`): the project (.nuee), its data as CSV with a data
// dictionary, one R script that redoes every model, the documents, a README in the interface's
// language, the licence with its text, CITATION.cff, codemeta.json and the SHA-256 of every file,
// in one .zip (where the user says on the desktop; downloaded in a browser).
// Deposit on Zenodo (desktop only): the same package, written in the engine's session folder, then
// a draft on Zenodo with it and its description (main.js, zenodo.js), with the user's personal
// access token, which the main process keeps (jetons.js) and never gives back here. Nuée never
// publishes: the user checks the draft on Zenodo, whose page opens, and publishes it there.
// sandbox.zenodo.org, Zenodo's test server, is under Advanced.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander, bureau } from './api.js';
import { VERSION } from './nouveautes.js';
import { documentPourPaquet } from './documents.js';

const LICENCES = { 'CC-BY-4.0': 'cc_by', 'CC0-1.0': 'cc0', 'CC-BY-SA-4.0': 'cc_by_sa', 'CC-BY-NC-4.0': 'cc_by_nc' };
const HOTES = ['zenodo.org', 'sandbox.zenodo.org'];

// An ORCID as the engine keeps it (0000-0002-1825-0097), from its address or without dashes, checked
// by its last digit (ISO 7064 11,2, as paquet.R: orcid_paquet); '' for none, null for one that isn't.
export function lireOrcid(x) {
  const brut = String(x || '').trim();
  if (!brut) return '';
  const c = brut.replace(/^(https?:\/\/)?(www\.)?orcid\.org\//i, '').replace(/[-\s]/g, '').toUpperCase();
  if (!/^\d{15}[\dX]$/.test(c)) return null;
  let total = 0;
  for (let k = 0; k < 15; k += 1) total = (total + Number(c[k])) * 2;
  const cle = (12 - (total % 11)) % 11;
  if (c[15] !== (cle === 10 ? 'X' : String(cle))) return null;
  return `${c.slice(0, 4)}-${c.slice(4, 8)}-${c.slice(8, 12)}-${c.slice(12)}`;
}

// A file name from the package's title (the .zip, and the folder it holds)
const nomFichier = (titre) => String(titre).replace(/[^\p{L}\p{N} _().-]+/gu, '_').trim().slice(0, 60).trim() || 'package';

export function dialoguePaquet(app, p, { zenodo = false } = {}) {
  const garde = p.paquet || {};
  const auteurs = [].concat(garde.auteurs || []).map((a) => ({ prenom: a.prenom || '', nom: a.nom || '', orcid: a.orcid || '' }));
  if (!auteurs.length) auteurs.push({ prenom: '', nom: '', orcid: '' });
  let licence = LICENCES[garde.licence] ? garde.licence : 'CC-BY-4.0';
  const titre = el('input', { class: 'paquet-titre', value: garde.titre || p.nom.replace(/\.[^.]+$/, ''), 'aria-label': t('paquet_titre_champ') });
  const description = el('textarea', { rows: 5, 'aria-label': t('paquet_description') }, garde.description || '');
  const mots = el('input', { class: 'paquet-mots', value: [].concat(garde.mots_cles || []).join(', '), placeholder: t('paquet_mots_aide'), 'aria-label': t('paquet_mots') });
  const lignes = el('tbody');
  const dessinerAuteurs = () => {
    const entree = (a, cle, i, attrs = {}) => el('input', { value: a[cle], placeholder: t(`paquet_${cle}`), 'aria-label': `${t(`paquet_${cle}`)} · ${i + 1}`,
      oninput: (e) => { a[cle] = e.target.value; }, ...attrs });
    lignes.replaceChildren(...auteurs.map((a, i) => el('tr', {},
      el('td', {}, entree(a, 'prenom', i)), el('td', {}, entree(a, 'nom', i)),
      el('td', {}, entree(a, 'orcid', i, { class: 'paquet-orcid', placeholder: '0000-0000-0000-0000', spellcheck: 'false' })),
      el('td', {}, el('button', { class: 'icone-seule', title: t('paquet_retirer_auteur'), 'aria-label': t('paquet_retirer_auteur'), disabled: auteurs.length === 1,
        onclick: () => { auteurs.splice(i, 1); dessinerAuteurs(); } }, icone('x'))))));
  };
  dessinerAuteurs();
  const licences = el('div', { class: 'paquet-licences' }, Object.entries(LICENCES).map(([id, cle]) => el('label', { class: 'choix-paquet' },
    el('input', { type: 'radio', name: 'paquet-licence', value: id, checked: id === licence, onchange: () => { licence = id; } }),
    el('span', {}, el('span', { class: 'titre-choix' }, t(`paquet_licence_${cle}`)), el('span', { class: 'note' }, t(`paquet_licence_${cle}_aide`))))));
  const n = p.modeles.length;
  const nd = [].concat(p.documents || []).length;
  const erreur = el('p', { class: 'erreur' });
  const contenu = el('div', { class: 'dialogue-paquet' },
    el('p', {}, t('paquet_texte', { modeles: n ? t(n === 1 ? 'paquet_texte_modele' : 'paquet_texte_modeles', { n }) : '',
      documents: nd ? t(nd === 1 ? 'paquet_texte_document' : 'paquet_texte_documents', { n: nd }) : '' })),
    el('div', { class: 'grille-champs' },
      el('label', {}, t('paquet_titre_champ')), titre,
      el('label', {}, t('paquet_auteurs')), el('div', {},
        el('table', { class: 'paquet-auteurs' }, el('thead', {}, el('tr', {}, ['prenom', 'nom', 'orcid', ''].map((c) => el('th', {}, c ? t(`paquet_${c}`) : '')))), lignes),
        el('button', { onclick: () => { auteurs.push({ prenom: '', nom: '', orcid: '' }); dessinerAuteurs(); lignes.lastChild.querySelector('input').focus(); } },
          icone('plus'), ` ${t('paquet_ajouter_auteur')}`)),
      el('label', {}, t('paquet_description')), el('div', {}, description, el('p', { class: 'note' }, t('paquet_description_aide'))),
      el('label', {}, t('paquet_mots')), mots,
      el('label', {}, t('paquet_licence')), licences),
    el('p', { class: 'note' }, t('paquet_garde')), erreur);

  // the answers, as the engine keeps them; null (and why, in the dialog) when one is missing
  const lire = (pourZenodo) => {
    erreur.textContent = '';
    const meta = { titre: titre.value.trim(), auteurs: [], description: description.value.trim(),
      mots_cles: mots.value.split(/[,;\n]/).map((x) => x.trim()).filter(Boolean), licence };
    if (!meta.titre) { erreur.textContent = t('paquet_sans_titre'); titre.focus(); return null; }
    for (const a of auteurs) {
      const x = { prenom: a.prenom.trim(), nom: a.nom.trim(), orcid: lireOrcid(a.orcid) };
      if (!x.prenom && !x.nom && !a.orcid.trim()) continue;
      if (x.orcid === null) { erreur.textContent = t('paquet_orcid_faux', { orcid: a.orcid.trim() }); return null; }
      if (!x.nom) { erreur.textContent = t('paquet_auteur_sans_nom'); return null; }
      meta.auteurs.push(x);
    }
    if (!meta.auteurs.length) { erreur.textContent = t('paquet_sans_auteur'); return null; }
    if (pourZenodo && !meta.description) { erreur.textContent = t('paquet_sans_description'); description.focus(); return null; }
    return meta;
  };
  const exporter = () => {
    const meta = lire(false);
    if (!meta) return false;
    (async () => {
      let chemin = null;
      if (bureau) {
        chemin = await bureau.enregistrerSous(`${nomFichier(meta.titre)}.zip`, [{ name: t('paquet_format'), extensions: ['zip'] }]);
        if (!chemin) return;
      }
      d.fermer();
      const r = await ecrirePaquet(app, p.id, meta, chemin);
      if (!bureau) window.location.href = `/api/fichier?chemin=${encodeURIComponent(r.chemin)}`;
      app.statut(t('paquet_ecrit', { chemin: r.chemin, n: [].concat(r.fichiers).length }));
      const x = remarques(r);
      if (x) dialogue({ titre: t('paquet_remarques_titre'), contenu: x, boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
    })().catch((e) => app.statut(`${t('erreur')} : ${e.message}`, true));
    return false;
  };
  const deposer = () => {
    const meta = lire(true);
    if (!meta) return false;
    dialogueZenodo(app, p, meta);
    return true;
  };
  const sortie = { libelle: bureau ? t('choisir_emplacement') : t('telecharger'), principal: !zenodo, action: exporter };
  const d = dialogue({ titre: t('paquet_titre', { nom: p.nom }), large: true, contenu,
    boutons: [{ libelle: t('annuler'), action: () => true },
      ...(bureau ? (zenodo ? [sortie, { libelle: t('paquet_zenodo'), principal: true, action: deposer }] : [{ libelle: t('paquet_zenodo'), action: deposer }, sortie]) : [sortie])] });
}

// Every model of the project fitted (the latest fit of each, in the order of their numbers), each
// document made a reproducible Quarto file, then the package written by the engine; `chemin`:
// where (null: in the engine's session folder). Returns the engine's answer, with what could not
// be done (`notes`) and the documents whose references don't all resolve.
async function ecrirePaquet(app, pid, meta, chemin) {
  return app.occupe(async () => {
    if (app.viderDocuments) await app.viderDocuments(pid);  // (what is being typed in a document first)
    if (app.viderFigures) await app.viderFigures(pid);
    const p = app.projet(pid);
    const nonAjustes = [];
    const ajustes = new Set(app.modelesSeance(pid).map((m) => m.numero));
    for (const d of p.modeles) {
      if (d.numero === null || d.numero === undefined || ajustes.has(d.numero)) continue;
      app.statut(t('paquet_ajustement', { nom: d.nom }));
      try { await app.calculerDiscret(pid, d.numero); } catch (e) { nonAjustes.push({ nom: d.nom, erreur: e.message }); }
    }
    const documents = [];
    const notes = [];
    const pris = new Set();
    for (const doc of [].concat(app.projet(pid).documents || [])) {
      const base = String(doc.titre || t('document_sans_titre')).replace(/[^\p{L}\p{N} _.()-]+/gu, '_').trim() || 'document';
      let nom = base;
      for (let k = 2; pris.has(nom.toLowerCase()); k += 1) nom = `${base} (${k})`;
      pris.add(nom.toLowerCase());
      app.statut(t('paquet_document', { nom: doc.titre || nom }));
      try {
        const x = await documentPourPaquet(app, pid, doc.id, nom);
        documents.push(x);
        if (x.erreurs) notes.push(t('paquet_document_erreurs', { nom: doc.titre || nom, n: x.erreurs }));
      } catch (e) { notes.push(t('paquet_document_absent', { nom: doc.titre || nom, erreur: e.message })); }
    }
    app.statut(t('paquet_ecriture'));
    const r = await demander('paquet', { projet: pid, meta, modeles: app.modelesSeance(pid).map((m) => m.id), non_ajustes: nonAjustes,
      documents: JSON.stringify(documents), session: app.sessionProjet(pid), chemin, version: VERSION, langue: langueActuelle() });
    app.majProjet(r.projet);
    app.diffuser('modeles');
    return { ...r, notes };
  });
}

// What to know of a package once written (the README says it too): the models its script doesn't
// redo and why, those whose script doesn't give back Nuée's results exactly, the documents; null
// when there is nothing.
function remarques(r) {
  const dp = langueActuelle() === 'fr' ? ' : ' : ': ';
  const sans = [].concat(r.modeles || []).filter((x) => x.erreur);
  const ecarts = [].concat(r.ecarts || []);
  const docs = [...[].concat(r.notes || []), ...[].concat(r.documents || []).filter((x) => x.note).map((x) => `${x.titre || x.nom}${dp}${x.note}`)];
  if (!sans.length && !ecarts.length && !docs.length) return null;
  const liste = (cle, x) => (x.length ? [el('p', {}, t(cle)), el('ul', {}, x.map((y) => el('li', {}, y)))] : []);
  return el('div', {}, ...liste('paquet_non_refaits', sans.map((x) => `${x.nom}${dp}${x.erreur}`)), ...liste('paquet_ecarts', ecarts),
    ...liste('paquet_remarques_documents', docs), el('p', { class: 'note' }, t('paquet_remarques_readme')));
}

// The deposit's dialog: the token (pasted once, then kept by the main process), the host, then
// the package written and the draft created; its page opens (main.js). Stays open with what went
// wrong, if something did.
async function dialogueZenodo(app, p, meta) {
  let hote = HOTES[0];
  let etat = { jeton: false, persistant: false };
  let enCours = false;
  const champ = el('input', { type: 'password', class: 'zenodo-champ', autocomplete: 'off', spellcheck: 'false', 'aria-label': t('zenodo_jeton') });
  const zoneJeton = el('div', { class: 'zenodo-jeton' });
  const erreur = el('div', { class: 'erreur' });
  const dessiner = async () => {
    etat = await bureau.zenodoEtat(hote);
    zoneJeton.replaceChildren(...(etat.jeton
      ? [el('div', { class: 'ligne' }, el('span', {}, t('zenodo_jeton_garde', { hote })),
        el('button', { onclick: async () => { await bureau.zenodoOublier(hote); erreur.replaceChildren(); await dessiner(); } }, t('zenodo_oublier')))]
      : [el('label', { class: 'champ' }, el('span', {}, t('zenodo_jeton')), champ),
        el('p', { class: 'note' }, t('zenodo_jeton_aide', { hote }), ' ',
          el('a', { href: `https://${hote}/account/settings/applications/tokens/new/`, target: '_blank', rel: 'noopener' }, t('zenodo_creer_jeton', { hote }))),
        el('p', { class: 'note' }, t(etat.persistant ? 'zenodo_persistant' : 'zenodo_session', { hote }))]));
  };
  const creer = async () => {
    if (enCours) return;
    erreur.replaceChildren();
    if (!etat.jeton) {
      const j = champ.value.trim();
      if (!j) { erreur.textContent = t('zenodo_coller_jeton', { hote }); champ.focus(); return; }
      const g = await bureau.zenodoJeton(hote, j);
      champ.value = '';
      if (g.erreur) { erreur.textContent = t('zenodo_jeton_invalide'); return; }
      await dessiner();
    }
    enCours = true;
    try {
      const r = await ecrirePaquet(app, p.id, meta, null);
      app.statut(t('zenodo_en_cours', { hote, taille: (r.taille / 1e6).toFixed(1) }));
      const z = await app.occupe(() => bureau.zenodoDeposer(hote, r.chemin, { ...(r.projet.paquet || meta), langue: langueActuelle() }));
      if (z.erreur) {
        erreur.replaceChildren(el('p', {}, messageZenodo(z, hote)), z.page ? el('button', { onclick: () => window.open(z.page) }, t('zenodo_ouvrir')) : '');
        app.statut('');
        if (z.erreur === 'jeton' || z.erreur === 'droits') await dessiner();
        return;
      }
      d.fermer();
      app.statut(t('zenodo_fait_statut', { page: z.page }));
      dialogue({ titre: t('zenodo_fait_titre'), contenu: el('div', {}, el('p', {}, t('zenodo_fait')), el('p', { class: 'note' }, z.page), remarques(r)),
        boutons: [{ libelle: t('zenodo_ouvrir'), action: () => { window.open(z.page); return false; } }, { libelle: t('fermer'), principal: true, action: () => true }] });
    } catch (e) {
      erreur.textContent = e.message;
    } finally {
      enCours = false;
    }
  };
  const bac = el('input', { type: 'checkbox', onchange: (e) => { hote = HOTES[e.target.checked ? 1 : 0]; champ.value = ''; erreur.replaceChildren(); dessiner(); } });
  const d = dialogue({ titre: t('zenodo_titre'),
    contenu: el('div', { class: 'dialogue-zenodo' }, el('p', {}, t('zenodo_texte')), zoneJeton,
      el('details', {}, el('summary', {}, t('zenodo_avance')), el('label', { class: 'champ' }, bac, el('span', {}, t('zenodo_bac_a_sable')))), erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('zenodo_creer'), principal: true, action: () => { creer(); return false; } }] });
  await dessiner();
  if (!etat.jeton) champ.focus();
}

const messageZenodo = (z, hote) => {
  const cles = { jeton: 'zenodo_erreur_jeton', droits: 'zenodo_erreur_droits', refus: 'zenodo_erreur_refus', taille: 'zenodo_erreur_taille',
    reseau: 'zenodo_erreur_reseau', sans_jeton: 'zenodo_sans_jeton' };
  return t(cles[z.erreur] || 'zenodo_erreur_autre', { detail: z.detail || z.erreur, hote }) + (z.page ? ` ${t('zenodo_erreur_brouillon')}` : '');
};

ajouterTextes({
  paquet: 'Exporter un paquet de reproductibilité…', paquet_zenodo: 'Déposer sur Zenodo…',
  paquet_titre: 'Paquet de reproductibilité — {nom}', paquet_format: 'Archive zip',
  paquet_texte: 'Une archive .zip que quiconque a R, ou Nuée, peut relancer : le projet, ses données en CSV avec leur dictionnaire, {modeles}{documents}un README, la licence des données et son texte, CITATION.cff, codemeta.json et l’empreinte SHA-256 de chaque fichier.',
  paquet_texte_modeles: 'un seul script R qui refait chacun de ses {n} modèles, ', paquet_texte_modele: 'un script R qui refait son modèle, ',
  paquet_texte_documents: 'ses {n} documents en fichiers Quarto reproductibles, ', paquet_texte_document: 'son document en fichier Quarto reproductible, ',
  paquet_titre_champ: 'Titre', paquet_auteurs: 'Auteurs', paquet_prenom: 'Prénoms', paquet_nom: 'Nom', paquet_orcid: 'ORCID (facultatif)',
  paquet_ajouter_auteur: 'Ajouter un auteur', paquet_retirer_auteur: 'Retirer cet auteur',
  paquet_description: 'Description', paquet_description_aide: 'Ce que sont les données et l’analyse ; une ligne vide sépare deux paragraphes.',
  paquet_mots: 'Mots-clés', paquet_mots_aide: 'séparés par des virgules', paquet_licence: 'Licence des données',
  paquet_licence_cc_by: 'CC BY 4.0', paquet_licence_cc_by_aide: 'Chacun peut les réutiliser en vous citant. Celle que Zenodo propose d’abord.',
  paquet_licence_cc0: 'CC0 1.0', paquet_licence_cc0_aide: 'Dans le domaine public, sans condition (Dryad la demande, GBIF la propose).',
  paquet_licence_cc_by_sa: 'CC BY-SA 4.0', paquet_licence_cc_by_sa_aide: 'En vous citant, et sous la même licence.',
  paquet_licence_cc_by_nc: 'CC BY-NC 4.0', paquet_licence_cc_by_nc_aide: 'En vous citant, et pas pour un usage commercial.',
  paquet_garde: 'Vos réponses sont gardées dans le projet : le prochain paquet repart d’elles.',
  paquet_sans_titre: 'Donnez un titre au paquet.', paquet_sans_auteur: 'Donnez au moins un auteur, avec son nom.',
  paquet_auteur_sans_nom: 'Chaque auteur a besoin d’un nom de famille.', paquet_sans_description: 'Zenodo demande une description.',
  paquet_orcid_faux: 'Ce n’est pas un ORCID : {orcid}. Un ORCID a 16 chiffres, comme 0000-0002-1825-0097, dont le dernier (parfois X) vérifie les autres.',
  paquet_ajustement: 'Paquet : ajustement de {nom}…', paquet_document: 'Paquet : le document « {nom} »…',
  paquet_ecriture: 'Paquet : ses scripts refaits et vérifiés, ses fichiers écrits…',
  paquet_ecrit: 'Paquet de reproductibilité écrit : {chemin} ({n} fichiers).', paquet_remarques_titre: 'Le paquet est écrit, avec des remarques',
  paquet_non_refaits: 'Pas refaits par son script R :', paquet_ecarts: 'Son script ne redonne pas exactement les résultats de Nuée pour :',
  paquet_remarques_documents: 'Documents :', paquet_remarques_readme: 'Le README du paquet le dit aussi.',
  paquet_document_absent: '« {nom} » n’est pas dans le paquet : {erreur}', paquet_document_erreurs: '« {nom} » : {n} référence(s) ne se résolvent pas, écrites comme dans l’aperçu',
  zenodo_titre: 'Déposer sur Zenodo',
  zenodo_texte: 'Nuée écrit le paquet, puis crée sur Zenodo un brouillon avec lui et sa description. Il ne publie jamais : vous vérifiez le brouillon sur Zenodo, dont la page s’ouvre, et le publiez vous-même ; Zenodo lui donne alors un DOI.',
  zenodo_jeton: 'Jeton d’accès personnel', zenodo_jeton_garde: 'Un jeton pour {hote} est gardé sur cet ordinateur.', zenodo_oublier: 'L’oublier',
  zenodo_jeton_aide: 'Sur {hote} : Compte › Applications › Personal access tokens › New token, avec les droits deposit:write et deposit:actions.',
  zenodo_creer_jeton: 'Créer un jeton sur {hote}',
  zenodo_persistant: 'Il sera gardé sur cet ordinateur, chiffré par le système, et ne quittera Nuée que vers {hote}.',
  zenodo_session: 'Le système n’a pas de trousseau pour le chiffrer : il sera gardé jusqu’à la fermeture de Nuée seulement.',
  zenodo_avance: 'Avancé', zenodo_bac_a_sable: 'Essayer sur sandbox.zenodo.org, le serveur d’essai de Zenodo : ses dépôts ne comptent pas, et il a ses propres comptes et jetons.',
  zenodo_creer: 'Créer le brouillon', zenodo_coller_jeton: 'Collez votre jeton de {hote}.', zenodo_jeton_invalide: 'Ce n’est pas un jeton de Zenodo.',
  zenodo_en_cours: 'Zenodo : le brouillon sur {hote}, le paquet ({taille} Mo), sa description…',
  zenodo_fait_titre: 'Brouillon créé sur Zenodo', zenodo_fait_statut: 'Brouillon créé sur Zenodo : {page}',
  zenodo_fait: 'Le brouillon a le paquet et sa description, et Nuée comme logiciel qui l’a fait. Vérifiez-le sur Zenodo, complétez-le au besoin, puis publiez-le là : rien n’est public avant, et Nuée ne publie jamais.',
  zenodo_ouvrir: 'Ouvrir le brouillon',
  zenodo_erreur_jeton: 'Zenodo refuse le jeton (HTTP 401) : vérifiez-le, ou oubliez-le et collez-en un autre.',
  zenodo_erreur_droits: 'Le jeton n’a pas les droits qu’il faut (HTTP 403) : deposit:write et deposit:actions.',
  zenodo_erreur_refus: 'Zenodo n’a pas pris le dépôt : {detail}', zenodo_erreur_taille: 'Le paquet est trop gros pour Zenodo (HTTP 413).',
  zenodo_erreur_reseau: 'Zenodo ne répond pas : {detail}', zenodo_erreur_autre: 'Zenodo : {detail}',
  zenodo_erreur_brouillon: 'Le brouillon existe : finissez-le sur Zenodo, ou supprimez-le là.', zenodo_sans_jeton: 'Pas de jeton pour {hote}.',
}, {
  paquet: 'Export a reproducibility package…', paquet_zenodo: 'Deposit on Zenodo…',
  paquet_titre: 'Reproducibility package — {nom}', paquet_format: 'Zip archive',
  paquet_texte: 'A .zip archive that anyone with R, or with Nuée, can rerun: the project, its data as CSV with a data dictionary, {modeles}{documents}a README, the licence of the data with its text, CITATION.cff, codemeta.json and the SHA-256 hash of every file.',
  paquet_texte_modeles: 'one R script that redoes each of its {n} models, ', paquet_texte_modele: 'an R script that redoes its model, ',
  paquet_texte_documents: 'its {n} documents as reproducible Quarto files, ', paquet_texte_document: 'its document as a reproducible Quarto file, ',
  paquet_titre_champ: 'Title', paquet_auteurs: 'Authors', paquet_prenom: 'Given names', paquet_nom: 'Family name', paquet_orcid: 'ORCID (optional)',
  paquet_ajouter_auteur: 'Add an author', paquet_retirer_auteur: 'Remove this author',
  paquet_description: 'Description', paquet_description_aide: 'What the data and the analysis are; an empty line separates two paragraphs.',
  paquet_mots: 'Keywords', paquet_mots_aide: 'separated by commas', paquet_licence: 'Licence of the data',
  paquet_licence_cc_by: 'CC BY 4.0', paquet_licence_cc_by_aide: 'Anyone may reuse them, citing you. The one Zenodo offers first.',
  paquet_licence_cc0: 'CC0 1.0', paquet_licence_cc0_aide: 'In the public domain, with no condition (Dryad asks for it, GBIF offers it).',
  paquet_licence_cc_by_sa: 'CC BY-SA 4.0', paquet_licence_cc_by_sa_aide: 'Citing you, and under the same licence.',
  paquet_licence_cc_by_nc: 'CC BY-NC 4.0', paquet_licence_cc_by_nc_aide: 'Citing you, and not for commercial use.',
  paquet_garde: 'Your answers are kept in the project: the next package starts from them.',
  paquet_sans_titre: 'Give the package a title.', paquet_sans_auteur: 'Give at least one author, with their family name.',
  paquet_auteur_sans_nom: 'Each author needs a family name.', paquet_sans_description: 'Zenodo asks for a description.',
  paquet_orcid_faux: 'This isn’t an ORCID: {orcid}. An ORCID has 16 digits, as 0000-0002-1825-0097, the last one (sometimes X) checking the others.',
  paquet_ajustement: 'Package: fitting {nom}…', paquet_document: 'Package: the document “{nom}”…',
  paquet_ecriture: 'Package: its scripts redone and checked, its files written…',
  paquet_ecrit: 'Reproducibility package written: {chemin} ({n} files).', paquet_remarques_titre: 'The package is written, with remarks',
  paquet_non_refaits: 'Not redone by its R script:', paquet_ecarts: 'Its script doesn’t give back Nuée’s results exactly for:',
  paquet_remarques_documents: 'Documents:', paquet_remarques_readme: 'The package’s README says so too.',
  paquet_document_absent: '“{nom}” isn’t in the package: {erreur}', paquet_document_erreurs: '“{nom}”: {n} reference(s) can’t be resolved, written as in the preview',
  zenodo_titre: 'Deposit on Zenodo',
  zenodo_texte: 'Nuée writes the package, then creates a draft on Zenodo with it and its description. It never publishes: you check the draft on Zenodo, whose page opens, and publish it yourself; Zenodo then gives it a DOI.',
  zenodo_jeton: 'Personal access token', zenodo_jeton_garde: 'A token for {hote} is kept on this computer.', zenodo_oublier: 'Forget it',
  zenodo_jeton_aide: 'On {hote}: Account › Applications › Personal access tokens › New token, with the scopes deposit:write and deposit:actions.',
  zenodo_creer_jeton: 'Create a token on {hote}',
  zenodo_persistant: 'It will be kept on this computer, encrypted by the system, and will leave Nuée only for {hote}.',
  zenodo_session: 'The system has no keyring to encrypt it: it will be kept until Nuée closes only.',
  zenodo_avance: 'Advanced', zenodo_bac_a_sable: 'Try on sandbox.zenodo.org, Zenodo’s test server: its deposits don’t count, and it has accounts and tokens of its own.',
  zenodo_creer: 'Create the draft', zenodo_coller_jeton: 'Paste your token from {hote}.', zenodo_jeton_invalide: 'This isn’t a Zenodo token.',
  zenodo_en_cours: 'Zenodo: the draft on {hote}, the package ({taille} MB), its description…',
  zenodo_fait_titre: 'Draft created on Zenodo', zenodo_fait_statut: 'Draft created on Zenodo: {page}',
  zenodo_fait: 'The draft has the package and its description, and Nuée as the software that made it. Check it on Zenodo, complete it if needed, then publish it there: nothing is public before, and Nuée never publishes.',
  zenodo_ouvrir: 'Open the draft',
  zenodo_erreur_jeton: 'Zenodo refuses the token (HTTP 401): check it, or forget it and paste another one.',
  zenodo_erreur_droits: 'The token doesn’t have the scopes it needs (HTTP 403): deposit:write and deposit:actions.',
  zenodo_erreur_refus: 'Zenodo didn’t take the deposit: {detail}', zenodo_erreur_taille: 'The package is too big for Zenodo (HTTP 413).',
  zenodo_erreur_reseau: 'Zenodo doesn’t answer: {detail}', zenodo_erreur_autre: 'Zenodo: {detail}',
  zenodo_erreur_brouillon: 'The draft exists: finish it on Zenodo, or delete it there.', zenodo_sans_jeton: 'No token for {hote}.',
});
