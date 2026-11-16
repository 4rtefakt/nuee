// Living reports, the data side (Edit › Data steps…, Edit › Update the data…). A project made from
// a data file keeps that file, its source, and every change made to its data in Nuée as a step
// that can be replayed (moteur/R/etapes.R). Data steps shows the source and the steps, and removes
// one; Update the data reads the file again, or next year's, replays the steps, replaces the data
// as one change (Ctrl+Z), fits every model again and shows What changed (changements.js), with the
// values cited in the documents that changed, marked in their previews (documents.js). A notice in
// the status bar says when the source file changed since it was read. What the engine's answers
// say is read by etapes_texte.js.
import { t } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander, bureau } from './api.js';
import { resumerProjet, afficherChangements, sectionsRapport } from './changements.js';
import { valeursCitees } from './documents.js';
import { liste, noms, dateLisible, tailleLisible, nomFichier, etatSource, ICONES_ETAPES, genreEtape, lireRapport, modelesAReajuster, resumeApres,
  valeursChangees, descriptionVersion } from './etapes_texte.js';

const VUE_SEULE = new URLSearchParams(location.search).get('vue');
// (an element's children made again, those that are null or false left out, as el() does)
const remplir = (x, ...enfants) => x.replaceChildren(...enfants.flat(Infinity).filter((e) => e !== null && e !== undefined && e !== false));

export function installerEtapes(app) {
  Object.assign(app, {
    etapesDonnees: (pid) => dialogueEtapes(app, pid),
    miseAJourDonnees: (pid) => dialogueMiseAJour(app, pid),
    verifierSource: (pid) => verifierSource(app, pid),
    avisDonnees: () => avisDonnees(app),
  });
  // (a file changed by another program while Nuée was behind it: seen when it comes back)
  if (!VUE_SEULE) window.addEventListener('focus', () => verifierSources(app));
}

// ------------------------------------------------------------ the notice of a changed source

// What the latest etapes request said of each project's source (etatSource), and the notices
// dismissed, by project and by the file as it was then: a file that changes again brings its
// notice back. This window only.
const sources = new Map();
const ignorees = new Set();
let derniereVerification = 0;

function noterSource(app, pid, r) {
  const e = etatSource(r && r.source);
  const avant = sources.get(pid);
  sources.set(pid, e);
  if (!avant || avant.etat !== e.etat || avant.cle !== e.cle) app.majStatut();
  return e;
}

async function verifierSource(app, pid) {
  if (!app.projet(pid)) return;
  try { noterSource(app, pid, await demander('etapes', { projet: pid }, { silencieux: true })); } catch { /* asked again when the window comes back */ }
}

// The window back in front: each open project's source asked again, at most every ten seconds (a
// file dialog closing brings the window back too).
function verifierSources(app) {
  if (Date.now() - derniereVerification < 10000) return;
  derniereVerification = Date.now();
  for (const p of app.projets()) verifierSource(app, p.id);
}

// The status bar's notice, like that of a new version: for the active project first, then the
// others; its link opens Update the data, its × hides it until the file changes again.
function avisDonnees(app) {
  const actif = app.projetActif();
  const ids = app.projets().map((p) => p.id).sort((a, b) => (b === actif) - (a === actif));
  for (const pid of ids) {
    const e = sources.get(pid);
    if (!e || e.etat !== 'changee' || ignorees.has(`${pid}|${e.cle}`)) continue;
    const p = app.projet(pid);
    return el('span', { class: 'avis-donnees' },
      el('a', { href: '#', title: t('etapes_avis_aide'), onclick: (ev) => { ev.preventDefault(); app.miseAJourDonnees(pid); } },
        icone('refresh-cw'), app.projets().length > 1 ? t('etapes_avis_projet', { nom: p.nom }) : t('etapes_avis')),
      el('button', { class: 'icone-seule', title: t('etapes_avis_ignorer'), 'aria-label': t('etapes_avis_ignorer'),
        onclick: () => { ignorees.add(`${pid}|${e.cle}`); app.majStatut(); } }, icone('x')));
  }
  return null;
}

// ------------------------------------------------------------ pieces of both dialogs

const ETATS = { introuvable: 'etapes_etat_introuvable', changee: 'etapes_etat_changee', lue: 'etapes_etat_lue' };
const badgeEtat = (etat) => (ETATS[etat] ? el('span', { class: `etat-fichier ${etat}` }, t(ETATS[etat])) : null);
const LECTEURS = { archive: 'etapes_lecteur_archive', collecte: 'etapes_lecteur_collecte' };

// A file the project read (its source, its XLSForm form, a step's table or raster): its name, its
// state, where it is, and when it was read.
function ficheFichier(f, { titre = null, grand = false } = {}) {
  const e = etatSource(f);
  const a = f.actuel || {};
  return el('div', { class: `fiche-fichier ${e.etat}${grand ? ' grand' : ''}` },
    el('div', { class: 'ligne' }, icone(f.lecteur === 'collecte' || f.lecteur === 'archive' ? 'smartphone' : 'file-spreadsheet'),
      titre ? el('span', { class: 'doux' }, titre) : null, el('b', {}, nomFichier(f.chemin)), badgeEtat(e.etat)),
    el('p', { class: 'chemin-fichier' }, f.trouve || f.chemin),
    e.ailleurs ? el('p', { class: 'note' }, t('etapes_ailleurs', { chemin: f.chemin })) : null,
    el('p', { class: 'note' }, [f.table ? t('etapes_table', { table: f.table }) : '', LECTEURS[f.lecteur] ? t(LECTEURS[f.lecteur]) : '',
      t('etapes_lu_le', { date: dateLisible(f.lu), taille: tailleLisible(f.taille), modifie: dateLisible(f.date) })].filter(Boolean).join(' · ')),
    e.etat === 'changee' ? el('p', { class: 'note attention' }, t('etapes_maintenant', { taille: tailleLisible(a.taille), modifie: dateLisible(a.date) })) : null);
}

// Data that changed outside the steps (the R console, a version before 1.1), or a project from
// before 1.1: what the engine says of it.
function avisConformes(r) {
  if (r.conformes === false) return el('div', { class: 'alerte-donnees' }, el('p', {}, r.raison));
  return r.conformes === null && r.raison ? el('p', { class: 'note' }, r.raison) : null;
}

const confirmer = (titre, texte, note, bouton) => new Promise((resolve) => {
  let oui = false;
  dialogue({ titre, contenu: el('div', {}, el('p', {}, texte), note ? el('p', { class: 'note' }, note) : null),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: bouton, principal: true, action: () => { oui = true; return true; } }],
    surFermeture: () => resolve(oui) });
});

// ------------------------------------------------------------ Data steps

// The project's source and its steps: each with its label, date and the files it read, and
// Remove (after a question): the engine reads the source again and replays the other steps, one
// change that Ctrl+Z undoes; refused without the source file, with its reason.
async function dialogueEtapes(app, pid) {
  const p = app.projet(pid);
  if (!p) { app.statut(t('choisir_projet'), true); return; }
  const contenu = el('div', { class: 'contenu-etapes' }, el('p', { class: 'infos attente' }, t('calcul')));
  const message = el('div', { class: 'message-etapes', 'aria-live': 'polite' });
  let fermee = false;
  dialogue({ titre: `${t('etapes_titre')} — ${p.nom}`, large: true, contenu: el('div', { class: 'dialogue-etapes' }, message, contenu),
    boutons: [{ libelle: t('fermer'), action: () => true },
      { libelle: t('etapes_mettre_a_jour'), principal: true, action: () => { app.miseAJourDonnees(pid); return true; } }],
    surFermeture: () => { fermee = true; } });

  const retirer = async (e) => {
    const quoi = e.etiquette || genreEtape(e.demande);
    // (the source is read as it is now: changed since, removing a step also brings what changed in it)
    const changee = (sources.get(pid) || {}).etat === 'changee';
    const note = changee ? `${t('etapes_retirer_note')} ${t('etapes_retirer_source_changee')}` : t('etapes_retirer_note');
    if (!(await confirmer(t('etapes_retirer_titre'), t('etapes_retirer_question', { n: e.n, etiquette: quoi }), note, t('retirer')))) return;
    const n = (app.projet(pid) || p).observations.uid.length;
    message.replaceChildren(el('p', { class: 'infos attente' }, t('etapes_retrait')));
    let r;
    try { r = await app.changerDonnees('retirer_etape', pid, { n: e.n }); } catch (err) { message.replaceChildren(el('p', { class: 'erreur' }, err.message)); return; }
    // (the source's table is no longer in its file: only an update can read another)
    if (r.choisir) { message.replaceChildren(el('p', { class: 'erreur' }, t('etapes_table_absente'))); return; }
    const lu = lireRapport(r, n);
    message.replaceChildren(el('div', { class: 'retrait-etape' }, el('p', {}, el('b', {}, t('etapes_retiree', { n: e.n, etiquette: quoi })), ' ', lu.resume), sectionsRapport(lu)));
    app.statut(t('etapes_retiree_statut', { n: e.n }));
    charger();
  };

  async function charger() {
    let r;
    try { r = await demander('etapes', { projet: pid }); } catch (e) { contenu.replaceChildren(el('p', { class: 'erreur' }, e.message)); return; }
    noterSource(app, pid, r);
    if (fermee) return;
    const etapes = liste(r.etapes);
    remplir(contenu,
      el('h3', {}, t('etapes_source')),
      r.source ? ficheFichier(r.source, { grand: true }) : el('p', { class: 'note' }, t('etapes_sans_source')),
      r.source && r.source.formulaire ? ficheFichier(r.source.formulaire, { titre: t('etapes_formulaire') }) : null,
      avisConformes(r),
      el('h3', {}, t('etapes_liste', { n: etapes.length })),
      etapes.length ? el('div', { class: 'defile-etapes' }, el('table', { class: 'tableau compact liste-etapes' },
        el('thead', {}, el('tr', {}, ['', t('etapes_col_etape'), t('etapes_col_date'), t('etapes_col_fichiers'), ''].map((h) => el('th', {}, h)))),
        el('tbody', {}, etapes.map((e) => el('tr', {},
          el('td', { class: 'num doux' }, String(e.n)),
          el('td', {}, el('span', { class: 'ligne etape' }, icone(ICONES_ETAPES[e.demande] || 'pencil'), el('span', {}, e.etiquette || genreEtape(e.demande)),
            e.etiquette ? el('span', { class: 'doux' }, genreEtape(e.demande)) : null)),
          el('td', { class: 'doux' }, dateLisible(e.date)),
          el('td', {}, liste(e.fichiers).map((f) => el('span', { class: 'fichier-etape', title: f.trouve || f.chemin }, nomFichier(f.chemin), ' ', badgeEtat(etatSource(f).etat)))),
          el('td', {}, el('button', { title: t('etapes_retirer_aide'), onclick: () => retirer(e) }, icone('trash-2'), t('retirer')))))))) : el('p', { class: 'note' }, t('etapes_aucune')),
      el('p', { class: 'note' }, t('etapes_note')));
  }
  await charger();
}

// ------------------------------------------------------------ Update the data

// Whether the project keeps versions (versions.js): saved to a file whose versions are on.
async function versionsActives(p) {
  if (!bureau || !bureau.versions || !p.chemin_enregistrement) return false;
  try { return (await bureau.versions.etat(p.chemin_enregistrement)).actif === true; } catch { return false; }
}

// The file to read (the source, or another one: next year's), its preview (the engine's report,
// nothing changed yet), the models asked whether they take the new observations, and a version
// saved first; then Update (executer).
async function dialogueMiseAJour(app, pid) {
  const p = app.projet(pid);
  if (!p) { app.statut(t('choisir_projet'), true); return; }
  let info;
  try { info = await app.occupe(() => demander('etapes', { projet: pid })); } catch { return; }
  noterSource(app, pid, info);
  const s = info.source;
  const es = etatSource(s);
  const avecSource = es.etat === 'lue' || es.etat === 'changee';
  const st = { quel: avecSource ? 'source' : 'autre', autre: null, tables: null, table: null, rapport: null, lu: null, exclus: new Set(),
    version: false, garderVersion: true, numero: 0, enCours: false };
  const n = p.observations.uid.length;
  const zoneFichier = el('div', { class: 'fichier-maj' });
  const zoneApercu = el('div', { class: 'apercu-maj', 'aria-live': 'polite' });
  const zoneVersion = el('div', { class: 'version-maj' });
  const cle = (c) => (c.numero !== null && c.numero !== undefined ? c.numero : c.nom);

  const rendreFichier = () => {
    const radio = (valeur, libelle, desactive = false) => el('label', { class: 'champ' },
      el('input', { type: 'radio', name: 'fichier-maj', checked: st.quel === valeur, disabled: desactive, onchange: () => {
        st.quel = valeur;
        st.tables = null;
        st.table = null;
        rendreFichier();
        if (valeur === 'autre' && !st.autre) choisirAutre(); else previsualiser();
      } }), libelle);
    remplir(zoneFichier, el('h3', {}, t('etapes_maj_le_fichier')),
      es.etat === 'aucune' ? el('p', { class: 'note' }, t('etapes_maj_sans_source')) : el('div', { class: 'ligne' },
        radio('source', t('etapes_maj_source', { nom: nomFichier(s.chemin) }), !avecSource), badgeEtat(es.etat),
        el('span', { class: 'chemin-fichier', title: s.trouve || s.chemin }, s.trouve || s.chemin)),
      el('div', { class: 'ligne' },
        es.etat === 'aucune' ? el('span', {}, t('etapes_maj_fichier')) : radio('autre', t('etapes_maj_autre')),
        el('button', { onclick: () => choisirAutre() }, icone('folder-open'), t('etapes_maj_choisir')),
        st.autre ? el('span', { class: 'chemin-fichier', title: st.autre }, st.autre) : null),
      st.tables ? el('div', { class: 'ligne' }, el('span', {}, t('etapes_maj_table')),
        el('select', { onchange: (e) => { st.table = e.target.value || null; previsualiser(); } },
          el('option', { value: '' }, t('etapes_maj_table_choisir')), st.tables.map((x) => el('option', { value: x, selected: x === st.table }, x)))) : null);
  };
  const choisirAutre = async () => {
    const c = await app.choisirTableau();
    if (c) Object.assign(st, { autre: c, quel: 'autre', tables: null, table: null });
    else if (!st.autre && avecSource) st.quel = 'source';
    rendreFichier();
    previsualiser();
  };

  const rendreApercu = () => {
    const r = st.rapport;
    const lu = st.lu;
    const f = r.fichier || {};
    const nouvelles = Number((r.observations || {}).ajoutees) > 0;
    remplir(zoneApercu, el('h3', {}, t('etapes_maj_apercu')),
      f.table ? el('p', { class: 'doux fichier-lu' }, t('etapes_table', { table: f.table })) : null,
      el('p', { class: `resume-maj${lu.rien ? ' rien' : ''}` }, lu.resume),
      sectionsRapport(lu),
      nouvelles && lu.choix.length ? el('div', { class: 'choix-maj' }, el('h4', {}, icone('circle-plus'), t('etapes_maj_nouvelles_obs')),
        el('p', { class: 'note' }, t('etapes_maj_nouvelles_note')),
        lu.choix.map((c) => el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: !st.exclus.has(cle(c)), onchange: (e) => {
          if (e.target.checked) st.exclus.delete(cle(c)); else st.exclus.add(cle(c));
        } }), c.texte))) : null);
  };
  const previsualiser = async () => {
    const k = ++st.numero;
    st.rapport = null;
    majBouton();
    if (st.quel === 'autre' && !st.autre) { zoneApercu.replaceChildren(el('p', { class: 'doux' }, t('etapes_maj_choisir_fichier'))); return; }
    if (st.tables && !st.table) { zoneApercu.replaceChildren(el('p', { class: 'doux' }, t('etapes_maj_choisir_table'))); return; }
    zoneApercu.replaceChildren(el('p', { class: 'infos attente' }, t('etapes_maj_lecture')));
    let r;
    try {
      r = await demander('mettre_a_jour', { projet: pid, chemin: st.quel === 'autre' ? st.autre : undefined, table: st.table || undefined, apercu: true });
    } catch (e) {
      if (k === st.numero) zoneApercu.replaceChildren(el('p', { class: 'erreur' }, e.message));
      return;
    }
    if (k !== st.numero) return;
    if (r.choisir) {  // (a file of several tables, its sheet or table to choose)
      st.tables = liste(r.choisir);
      rendreFichier();
      zoneApercu.replaceChildren(el('p', { class: 'doux' }, t('etapes_maj_choisir_table')));
      return;
    }
    st.rapport = r;
    st.lu = lireRapport(r, n);
    st.exclus = new Set();  // (another file: each model asked again, ticked)
    rendreApercu();
    majBouton();
  };

  const rendreVersion = () => zoneVersion.replaceChildren(...(st.version ? [
    el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: st.garderVersion, onchange: (e) => { st.garderVersion = e.target.checked; } }), t('etapes_maj_version')),
    el('p', { class: 'note' }, t('etapes_maj_version_note', { description: descriptionVersion() }))] : []));

  const lancer = async () => {
    if (!st.rapport || st.enCours) return;
    st.enCours = true;
    majBouton();
    const nouvelles = Number((st.rapport.observations || {}).ajoutees) > 0;
    const q = { chemin: st.quel === 'autre' ? st.autre : undefined, table: st.table || undefined,
      sans_nouvelles: nouvelles ? st.lu.choix.filter((c) => st.exclus.has(cle(c))).map(cle) : [],
      version: st.version && st.garderVersion ? p.chemin_enregistrement : null };
    d.fermer();
    await executer(app, pid, q);
  };
  const d = dialogue({ titre: `${t('etapes_maj_titre')} — ${p.nom}`, large: true,
    contenu: el('div', { class: 'dialogue-maj' }, avisConformes(info), zoneFichier, zoneApercu, zoneVersion, el('p', { class: 'note' }, t('etapes_maj_note'))),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('etapes_maj_bouton'), principal: true, action: () => { lancer(); return false; } }] });
  const bouton = d.boite.querySelector('.pied-dialogue button.principal');
  function majBouton() { if (bouton) bouton.disabled = !st.rapport || st.enCours; }
  rendreFichier();
  if (st.quel === 'autre' && es.etat === 'aucune') zoneApercu.replaceChildren(el('p', { class: 'doux' }, t('etapes_maj_choisir_fichier')));
  else previsualiser();
  versionsActives(p).then((oui) => { st.version = oui; rendreVersion(); });
}

// The progress of an update, in a dialog: what is being done, a bar, and Stop: no more models
// fitted again after the one being fitted (before the data change, no update at all). Closing the
// dialog stops the same way.
function progression(titre) {
  const etape = el('p', { class: 'etape-maj' });
  const barre = el('progress', { class: 'barre-maj' });
  const detail = el('p', { class: 'note' });
  let arret = false;
  const d = dialogue({ titre, contenu: el('div', { class: 'progression-maj' }, etape, barre, detail),
    boutons: [{ libelle: t('etapes_arreter'), action: () => { arreter(); return false; } }],
    surFermeture: () => { arret = true; } });
  const bouton = d.boite.querySelector('.pied-dialogue button');
  function arreter() {
    arret = true;
    bouton.disabled = true;
    bouton.textContent = t('etapes_arret_demande');
  }
  return {
    arrete: () => arret,
    // (total: the bar's length, none for a step that takes as long as it takes)
    montrer(texte, fait = null, total = null, { arretable = true, note = '' } = {}) {
      etape.textContent = texte;
      if (total) { barre.max = total; barre.value = fait; } else barre.removeAttribute('value');
      detail.textContent = note;
      if (!arret) bouton.disabled = !arretable;
    },
    fermer: () => d.fermer(),
  };
}

// The version saved before the update (versions.js), once the project is saved, when the layer
// makes one on request (marquer) and the project keeps versions.
async function marquerVersion(app, pid, chemin, quand) {
  if (!(await app.enregistrer(pid, false))) throw new Error(t('etapes_p_non_enregistre'));
  const v = bureau && bureau.versions;
  if (!v || typeof v.marquer !== 'function' || (await v.etat(chemin)).actif !== true) return;
  await v.marquer(chemin, descriptionVersion(quand));
}

// The update: a version first if asked; the models' key results and the values the documents cite
// before; the data updated by the engine (one Ctrl+Z); every model fitted again
// (app.ajusterDeNouveau) but those the report says can't be; the key results and cited values
// after; then What changed, and the values that changed marked in the documents. A model not
// fitted again goes into What changed with its reason (resumeApres), its cited values as not updated.
async function executer(app, pid, { chemin, table, sans_nouvelles: sansNouvelles, version }) {
  const p = app.projet(pid);
  const quand = new Date();
  const pr = progression(t('etapes_p_titre', { nom: p.nom }));
  try {
    if (version) {
      pr.montrer(t('etapes_p_version'), null, null, { arretable: false });
      await marquerVersion(app, pid, version, quand);
    }
    const suivre = (cle) => (x) => pr.montrer(t(cle, { n: Math.min(x.fait + 1, x.total), total: x.total }), x.fait, x.total, { note: x.nom || '' });
    const avant = await resumerProjet(app, pid, { surProgression: suivre('etapes_p_avant') });
    const citeesAvant = await valeursCitees(app, pid);
    if (pr.arrete()) { pr.fermer(); app.statut(t('etapes_p_arrete_avant')); return; }
    pr.montrer(t('etapes_p_donnees'), null, null, { arretable: false });
    const n = app.projet(pid).observations.uid.length;
    const r = await app.changerDonnees('mettre_a_jour', pid, { chemin, table, sans_nouvelles: sansNouvelles });
    verifierSource(app, pid);
    const modeles = modelesAReajuster(r.projet, r);
    const refaits = new Map();
    for (const [k, m] of modeles.entries()) {
      if (pr.arrete()) break;
      pr.montrer(t('etapes_p_reajuster', { n: k + 1, total: modeles.length }), k, modeles.length, { note: `${m.nom} · ${m.type}` });
      if (m.bloque) continue;
      try { refaits.set(m.numero, { id: await app.ajusterDeNouveau(pid, m.numero) }); } catch (e) { refaits.set(m.numero, { erreur: e.message }); }
    }
    const apres = resumeApres(await resumerProjet(app, pid, { surProgression: suivre('etapes_p_apres') }), modeles, refaits);
    const restes = modeles.filter((m) => !(refaits.get(m.numero) || {}).id);
    const documents = valeursChangees(citeesAvant, await valeursCitees(app, pid), restes.map((m) => m.nom));
    pr.fermer();
    const date = dateLisible(quand);
    app.marquerChangements(pid, documents, date);
    const lu = lireRapport(r, n);
    await afficherChangements(app, { titre: t('etapes_changements', { date }), pid, avant, apres, documents,
      donnees: { resume: lu.resume, rien: lu.rien, sections: lu.sections, fichier: (r.fichier || {}).chemin || '' } });
    app.statut(restes.length ? t('etapes_p_fini_sauf', { n: modeles.length - restes.length, total: modeles.length, noms: noms(restes.map((m) => m.nom)) })
      : t('etapes_p_fini', { n: modeles.length }), restes.length > 0);
  } catch (e) {
    pr.fermer();
    app.statut(`${t('erreur')} : ${e.message}`, true);
  }
}
