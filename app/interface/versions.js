// File › Versions…: the saved states of a project, kept in its folder by the desktop app
// (app/versions.js, which preload.js gives the page as window.bureau.versions) and sent to or got
// from GitHub or GitLab, in a panel of the project. People read versions, names, send and get,
// never the words of the tool underneath. Each step asks the main process how the project file
// stands (etat) and decides from it; what the main process still refuses comes back as an error
// whose message, in the interface's language, is shown as it is.
//   - a version opened beside the project, read-only (app.ouvrirLectureSeule; the engine keeps
//     its source, oid and date: serveur.R, ouvrir), which Save refuses (app.refuserEnregistrement);
//   - a version compared with the project now: opened in the engine only, its models fitted
//     quietly, then What changed (changements.js);
//   - a version restored, then the project read again from its file (app.rechargerProjet);
//   - the status bar's indicator of the versions to send and to get (app.indicateurVersions).
import { t } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander, bureau } from './api.js';
import { VUES } from './panneaux.js';
import { dateLongue, dateVersion, titreVersion, nomDeChemin, memeFichier, nomHote, liensConnexion, indicateurVersions, resumeDivergence,
  phraseDivergence, descriptionVersion, nombreVersions } from './versions_texte.js';
import { resumerProjet, afficherChangements, installerChangements } from './changements.js';

// The main process's functions; none in a browser
const couche = () => (bureau && bureau.versions && typeof bureau.versions.etat === 'function' ? bureau.versions : null);

// The file whose versions are kept: a project saved by Nuée (.nuee), not an old version opened read-only
const fichierProjet = (p) => (p && !p.lecture_seule && p.chemin_enregistrement && /\.nuee$/i.test(p.chemin_enregistrement) ? p.chemin_enregistrement : null);

// What the main process said of each project file lately: the status bar asks at most once a
// minute (and never twice at a time), a panel each time it is drawn, an action right after it.
const memo = new Map();  // file → { etat, erreur, quand, promesse }
function lireEtat(chemin) {
  const promesse = couche().etat(chemin).then(
    (e) => { memo.set(chemin, { etat: e, quand: Date.now() }); return e; },
    (err) => { memo.set(chemin, { etat: null, erreur: err.message, quand: Date.now() }); throw err; });
  memo.set(chemin, { ...(memo.get(chemin) || {}), quand: Date.now(), promesse });
  return promesse;
}
const oublier = (chemin) => memo.delete(chemin);

export function installerVersions(app) {
  installerChangements(app);
  VUES.versions = {
    rendre: (conteneur, ctx, params) => rendreVersions(app, conteneur, params),
    titre: (_, p) => `${p.nom || ''} · ${t('versions_vue')}`,
    projet: true,
  };
  Object.assign(app, {
    versions: (pid) => {
      const id = pid || app.projetActif();
      if (!app.projet(id)) { app.statut(t('choisir_projet'), true); return; }
      app.ouvrirVue('versions', null, null, id);
    },
    indicateurVersions: () => indicateur(app),
    refuserEnregistrement: (pid) => refuserEnregistrement(app, pid),
  });
}

// ------------------------------------------------------------ the status bar

function indicateur(app) {
  const pid = app.projetActif();
  const chemin = fichierProjet(pid && app.projet(pid));
  if (!couche() || !chemin) return null;
  const m = memo.get(chemin);
  if (!m || (!m.promesse && Date.now() - m.quand > 60000)) lireEtat(chemin).then(() => app.majStatut(), () => {});
  const i = indicateurVersions(m && m.etat);
  if (!i) return null;
  return el('button', { class: 'icone-seule indicateur-versions', title: i.titre, 'aria-label': i.titre, onclick: () => app.versions(pid) },
    icone('history'), i.texte ? el('span', {}, i.texte) : null);
}

// ------------------------------------------------------------ the panel

// The project's panels drawn again, and the status bar, after an action (the file's state asked again)
function apresAction(app, pid) {
  const p = app.projet(pid);
  const chemin = fichierProjet(p);
  if (chemin) oublier(chemin);
  redessinerVersions(pid);
  if (chemin) lireEtat(chemin).then(() => app.majStatut(), () => {});
}
function redessinerVersions(pid) {
  for (const c of document.querySelectorAll('[data-versions]')) if (c.dataset.versions === pid && c._redessiner) c._redessiner();
}

async function rendreVersions(app, conteneur, params) {
  const p = app.projet(params.projet);
  if (!p) return;
  conteneur.dataset.versions = p.id;
  conteneur._redessiner = () => rendreVersions(app, conteneur, params);
  const montrer = (...enfants) => {
    const defile = conteneur.querySelector('.liste-versions');
    const haut = defile ? defile.scrollTop : 0;
    conteneur.replaceChildren(el('div', { class: 'versions' }, ...enfants));
    const nouveau = conteneur.querySelector('.liste-versions');
    if (nouveau) nouveau.scrollTop = haut;
  };
  const c = couche();
  if (!c) { montrer(el('p', { class: 'note' }, t('versions_navigateur'))); return; }
  if (p.lecture_seule) { montrer(...panneauLecture(app, p)); return; }
  const chemin = fichierProjet(p);
  if (!chemin) {
    montrer(el('p', {}, t(p.chemin_enregistrement ? 'versions_pas_nuee' : 'versions_jamais_enregistre')),
      el('div', { class: 'ligne' }, el('button', { class: 'principal', onclick: async () => { if (await app.enregistrer(p.id, !!p.chemin_enregistrement)) redessinerVersions(p.id); } },
        icone('save'), t(p.chemin_enregistrement ? 'enregistrer_sous_bouton' : 'versions_enregistrer_projet'))));
    return;
  }
  // (each drawing asks again; one started later wins)
  const numero = (conteneur.numeroVersions || 0) + 1;
  conteneur.numeroVersions = numero;
  if (!conteneur.querySelector('.versions')) montrer(el('p', { class: 'infos attente' }, t('calcul')));
  let e;
  let liste;
  try {
    e = await lireEtat(chemin);
    liste = e.suivi ? await c.liste(chemin) : [];
  } catch (err) {
    if (numero === conteneur.numeroVersions) {
      montrer(el('p', { class: 'erreur' }, err.message), el('button', { onclick: () => rendreVersions(app, conteneur, params) }, icone('refresh-cw'), t('versions_actualiser')));
    }
    return;
  }
  if (numero !== conteneur.numeroVersions || app.projet(p.id) !== p) {
    if (app.projet(p.id) && app.projet(p.id) !== p && numero === conteneur.numeroVersions) rendreVersions(app, conteneur, params);
    return;
  }
  if (!e.suivi) {
    montrer(el('p', {}, t('versions_explication')),
      e.depot ? el('p', { class: 'note' }, t('versions_dossier_partage')) : null,
      el('div', { class: 'ligne' }, el('button', { class: 'principal', onclick: () => activerVersions(app, p.id) }, icone('history'), t('versions_activer'))));
    return;
  }
  const modifie = e.modifie || p.modifie;
  // (the newest version is the project as it is when nothing changed since: no comparing or restoring it)
  const actuelle = !modifie && liste.length ? liste[0].oid : null;
  montrer(
    el('div', { class: 'outils' },
      el('button', { class: 'principal', onclick: () => enregistrerUneVersion(app, p.id) }, icone('save'), t('versions_enregistrer')),
      el('button', { onclick: () => apresAction(app, p.id) }, icone('refresh-cw'), t('versions_actualiser')),
      el('span', { class: 'infos', title: e.racine || '' }, `${nombreVersions(liste.length)} · ${t('versions_dans', { dossier: e.racine || '' })}`)),
    modifie ? el('p', { class: 'note attention-versions' }, icone('circle-alert'), t('versions_non_versionne')) : null,
    el('div', { class: 'liste-versions' }, liste.length ? liste.map((v) => ligneVersion(app, p, v, v.oid === actuelle)) : el('p', { class: 'doux' }, t('versions_aucune'))),
    sectionEnvoi(app, p, e),
    el('p', { class: 'note' }, e.auteur ? t('versions_auteur', e.auteur) : t('versions_auteur_titre'), ' ',
      el('a', { href: '#', onclick: (ev) => { ev.preventDefault(); assurerAuteur(e, true).then(() => apresAction(app, p.id)); } }, t('versions_changer'))));
}

function ligneVersion(app, p, v, actuelle) {
  const auteur = (v.auteur && v.auteur.nom) || '';
  return el('div', { class: `version${actuelle ? ' actuelle' : ''}` },
    el('div', { class: 'version-texte' },
      el('div', { class: 'version-tete' }, el('b', {}, descriptionVersion(v)),
        [].concat(v.noms || []).map((n) => el('span', { class: 'badge nom-version' }, n)),
        actuelle ? el('span', { class: 'badge bon' }, t('versions_actuelle')) : null),
      el('div', { class: 'doux', title: v.auteur && v.auteur.email ? `${auteur} <${v.auteur.email}>` : auteur }, `${dateVersion(v.date)}${auteur ? ` · ${auteur}` : ''}`)),
    el('div', { class: 'version-actions' },
      el('button', { title: t('versions_ouvrir_aide'), onclick: () => ouvrirVersion(app, p.id, v) }, icone('folder-open'), t('versions_ouvrir')),
      el('button', { title: t('versions_comparer_aide'), disabled: actuelle, onclick: () => comparerVersion(app, p.id, v) }, icone('diff'), t('versions_comparer')),
      el('button', { onclick: () => nommerVersion(app, p.id, v) }, icone('bookmark'), t('versions_nommer')),
      el('button', { disabled: actuelle, onclick: () => restaurerVersion(app, p.id, v) }, icone('rotate-ccw'), t('versions_restaurer'))));
}

// Send and receive: connect GitHub or GitLab, or send, get, check, change the address or token
function sectionEnvoi(app, p, e) {
  const s = el('div', { class: 'envoi-versions' }, el('h3', {}, t('versions_envoi')));
  if (!e.distant) {
    s.append(el('p', { class: 'note' }, t('versions_envoi_aide')),
      el('button', { onclick: () => dialogueConnexion(app, p.id) }, icone('cloud'), t('versions_connecter')));
    return s;
  }
  const hote = nomHote(e.distant.hote) || e.distant.url;
  const avance = Number(e.avance) || 0;
  const retard = Number(e.retard) || 0;
  s.append(el('p', {}, t('versions_connecte', { hote }), ' ', el('span', { class: 'adresse-versions' }, e.distant.url)));
  if (!e.distant.hote) s.append(el('p', { class: 'note attention-versions' }, t('versions_hors_https')));
  if (avance && retard) {
    s.append(el('div', { class: 'banniere-versions' }, el('span', {}, t('versions_divergence_banniere', { hote })),
      el('button', { onclick: () => recevoirVersions(app, p.id) }, t('versions_choisir'))));
  }
  s.append(el('div', { class: 'ligne' },
    el('button', { disabled: !e.distant.hote, onclick: () => envoyerVersions(app, p.id) }, icone('cloud-upload'), t('versions_envoyer'), avance ? el('span', { class: 'badge' }, avance) : null),
    el('button', { disabled: !e.distant.hote, onclick: () => recevoirVersions(app, p.id) }, icone('cloud-download'), t('versions_recevoir'), retard ? el('span', { class: 'badge' }, retard) : null),
    el('button', { onclick: () => verifierConnexion(app, p.id) }, icone('circle-check'), t('versions_verifier')),
    el('button', { onclick: () => dialogueConnexion(app, p.id) }, icone('link'), t('versions_modifier_connexion'))),
  el('p', { class: 'note' }, [avance ? t('versions_a_envoyer', { n: avance }) : '', retard ? t('versions_a_recevoir', { n: retard }) : ''].filter(Boolean).join(', ') || t('versions_a_jour_distant')),
  e.distant.hote ? el('p', { class: 'note' }, e.jeton ? t(e.jetonSession ? 'versions_jeton_session' : 'versions_jeton_garde') : t('versions_jeton_aucun'),
    e.jeton ? [' ', el('a', { href: '#', onclick: (ev) => { ev.preventDefault(); oublierJeton(app, p.id, e.distant.hote); } }, t('versions_oublier_jeton'))] : null) : null);
  return s;
}

// An old version opened read-only: what it is, Restore, Save as, and its project's versions
function panneauLecture(app, p) {
  const l = p.lecture_seule;
  const source = app.projets().find((x) => !x.lecture_seule && memeFichier(x.chemin_enregistrement, l.source));
  return [el('p', {}, icone('lock'), ' ', t('versions_lecture_panneau', { date: dateLongue(l.date), source: nomDeChemin(l.source) })),
    el('div', { class: 'ligne' },
      el('button', { onclick: () => restaurerDepuisLecture(app, p.id) }, icone('rotate-ccw'), t('versions_restaurer')),
      el('button', { onclick: () => app.enregistrer(p.id, true) }, icone('save'), t('versions_enregistrer_sous')),
      el('button', { onclick: () => (source ? app.versions(source.id) : app.ouvrirChemin(l.source)) }, icone('history'), t('versions_voir_source', { source: nomDeChemin(l.source) })))];
}

// ------------------------------------------------------------ small dialogs

// A question with its buttons (none: a message to close); resolves to the chosen button's value,
// or null when closed.
function demanderChoix({ titre, texte, boutons }) {
  return new Promise((resolve) => {
    let fait = false;
    dialogue({ titre, contenu: el('div', {}, [].concat(texte).map((x) => (typeof x === 'string' ? el('p', {}, x) : x))),
      boutons: [{ libelle: t(boutons.length ? 'annuler' : 'fermer'), action: () => true },
        ...boutons.map((b) => ({ libelle: b.libelle, principal: b.principal, action: () => { fait = true; resolve(b.valeur); return true; } }))],
      surFermeture: () => { if (!fait) resolve(null); } });
  });
}

// Who saves the versions: asked the first time (every version keeps it), or to change it.
// Resolves to true once given.
function assurerAuteur(e, changer = false) {
  if (e && e.auteur && !changer) return Promise.resolve(true);
  return new Promise((resolve) => {
    const nom = el('input', { value: (e && e.auteur && e.auteur.nom) || '', style: 'width:22em', autocomplete: 'name' });
    const email = el('input', { type: 'email', value: (e && e.auteur && e.auteur.email) || '', style: 'width:22em', autocomplete: 'email' });
    const erreur = el('p', { class: 'erreur' });
    let fait = false;
    const valider = async () => {
      erreur.textContent = '';
      if (!nom.value.trim() || !email.value.trim()) { erreur.textContent = t('versions_auteur_manque'); return; }
      try { await couche().auteur({ nom: nom.value, email: email.value }); } catch (err) { erreur.textContent = err.message; return; }
      fait = true;
      d.fermer();
      resolve(true);
    };
    email.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') valider(); });
    const d = dialogue({ titre: t('versions_auteur_titre'),
      contenu: el('div', {}, el('p', {}, t('versions_auteur_texte')),
        el('div', { class: 'grille-champs' }, el('label', {}, t('versions_auteur_nom')), nom, el('label', {}, t('versions_auteur_email')), email), erreur),
      boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('versions_auteur_bouton'), principal: true, action: () => { valider(); return false; } }],
      surFermeture: () => { if (!fait) resolve(false); } });
  });
}

// The few words of a version; resolves to them, or null.
function demanderDescription(p) {
  return new Promise((resolve) => {
    const champ = el('input', { style: 'width:100%', placeholder: t('versions_description_exemple'), maxlength: 500 });
    const erreur = el('p', { class: 'erreur' });
    let fait = false;
    const valider = () => {
      if (!champ.value.trim()) { erreur.textContent = t('versions_description_vide'); return; }
      fait = true;
      d.fermer();
      resolve(champ.value.trim());
    };
    champ.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') valider(); });
    const d = dialogue({ titre: t('versions_description_titre', { nom: p.nom }),
      contenu: el('div', { class: 'dialogue-version' }, el('p', {}, t('versions_description_aide')), champ,
        p.modifie ? el('p', { class: 'note' }, t('versions_enregistre_avant')) : null, erreur),
      boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('versions_bouton_enregistrer'), principal: true, action: () => { valider(); return false; } }],
      surFermeture: () => { if (!fait) resolve(null); } });
  });
}

// ------------------------------------------------------------ the actions

// What was just typed in the project's documents and figures goes into it first (it may then
// have changes to save)
async function vider(app, pid) {
  if (app.viderDocuments) await app.viderDocuments(pid);
  if (app.viderFigures) await app.viderFigures(pid);
}

// The project's state, or null with its message in the status bar
async function etatPour(app, pid) {
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return null;
  try { return { chemin, e: await lireEtat(chemin) }; } catch (err) { app.statut(err.message, true); return null; }
}

// (app.occupe shows a refusal's message in the status bar; false then)
const essayer = (app, f) => app.occupe(f).then((r) => r ?? true, () => false);

async function activerVersions(app, pid) {
  const x = await etatPour(app, pid);
  if (!x || !(await assurerAuteur(x.e))) return;
  await vider(app, pid);
  // (the first version is the file: what isn't saved yet goes in first)
  if (app.projet(pid).modifie && !(await app.enregistrer(pid, false))) return;
  if (!(await essayer(app, () => couche().activer(x.chemin)))) return;
  app.statut(t('versions_activees'));
  apresAction(app, pid);
}

// Save a version: a few words, the project saved first if it has unsaved changes. Resolves to true
// once the version is saved.
async function enregistrerUneVersion(app, pid, description = null) {
  await vider(app, pid);
  const x = await etatPour(app, pid);
  if (!x) return false;
  const p = app.projet(pid);
  if (!x.e.modifie && !p.modifie) {
    await demanderChoix({ titre: t('versions_enregistrer').replace('…', ''), texte: t('versions_inchange'), boutons: [] });
    return false;
  }
  if (!(await assurerAuteur(x.e))) return false;
  const texte = description || await demanderDescription(p);
  if (!texte) return false;
  if (app.projet(pid).modifie && !(await app.enregistrer(pid, false))) return false;
  if (!(await essayer(app, () => couche().enregistrerVersion(x.chemin, texte)))) { apresAction(app, pid); return false; }
  app.statut(t('versions_enregistree', { description: texte }));
  apresAction(app, pid);
  return true;
}

// Open: the version beside the project, read-only, under "<name> — version of <date>"
async function ouvrirVersion(app, pid, v) {
  const p = app.projet(pid);
  const chemin = fichierProjet(p);
  if (!chemin) return;
  const deja = app.projets().find((x) => x.lecture_seule && x.lecture_seule.oid === v.oid && memeFichier(x.lecture_seule.source, chemin));
  if (deja) { app.montrerProjet(deja.id); app.statut(t('versions_deja_ouverte')); return; }
  const lu = await essayer(app, () => couche().lire(chemin, v.oid));
  if (!lu) return;
  await app.ouvrirLectureSeule(lu.fichier, titreVersion(p.nom, v.date), { source: chemin, oid: v.oid, date: v.date });
  app.statut(t('versions_ouverte', { date: dateLongue(v.date) }));
}

// Compare: the version against the project as it is now
async function comparerVersion(app, pid, v) {
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return;
  const lu = await essayer(app, () => couche().lire(chemin, v.oid));
  if (!lu) return;
  const date = dateLongue(v.date);
  await comparerFichier(app, pid, lu.fichier, { titre: t('versions_comparaison_titre', { date }), date, avant: true });
}

// A project file (an old version, or theirs) and the project now: the file opened in the engine
// only (serveur.R: ouvrir, cache), its models fitted quietly and summed up, then closed; the
// project's own; then What changed. avant: the file is the side before (else after).
async function comparerFichier(app, pid, fichier, { titre, date, avant }) {
  let autre;
  try {
    const cache = await demander('ouvrir', { chemin: fichier, cache: true });
    try {
      autre = await resumerProjet(app, cache.id, { surProgression: ({ fait, total, nom }) => { if (nom) app.statut(t('versions_progression_avant', { date, nom, k: fait + 1, n: total })); } });
    } finally {
      demander('fermer_projet', { projet: cache.id }).catch(() => {});
    }
  } catch (err) { app.statut(`${t('erreur')} : ${err.message}`, true); return null; }
  if (!app.projet(pid)) return null;
  const actuel = await resumerProjet(app, pid, { surProgression: ({ fait, total, nom }) => { if (nom) app.statut(t('versions_progression_apres', { nom, k: fait + 1, n: total })); } });
  app.statut(t('versions_comparaison_faite'));
  return afficherChangements(app, { titre, pid, avant: avant ? autre : actuel, apres: avant ? actuel : autre });
}

// Name…: a name for the version, or one of its names removed
async function nommerVersion(app, pid, v) {
  const x = await etatPour(app, pid);
  if (!x || !(await assurerAuteur(x.e))) return;
  let noms = [].concat(v.noms || []);
  const champ = el('input', { style: 'width:100%', maxlength: 100 });
  const erreur = el('p', { class: 'erreur' });
  const liste = el('div', { class: 'noms-version' });
  const dessiner = () => liste.replaceChildren(...(noms.length ? [el('span', { class: 'doux' }, t('versions_noms')), ...noms.map((n) => el('span', { class: 'badge nom-version' }, n,
    el('button', { class: 'icone-seule', title: t('versions_retirer_nom', { nom: n }), 'aria-label': t('versions_retirer_nom', { nom: n }), onclick: async () => {
      erreur.textContent = '';
      try { await couche().retirerNom(x.chemin, n); } catch (err) { erreur.textContent = err.message; return; }
      noms = noms.filter((y) => y !== n);
      dessiner();
      app.statut(t('versions_nom_retire', { nom: n }));
      apresAction(app, pid);
    } }, icone('x'))))] : []));
  dessiner();
  const valider = async () => {
    erreur.textContent = '';
    if (!champ.value.trim()) { erreur.textContent = t('versions_nom_vide'); return; }
    let r;
    try { r = await couche().nommer(x.chemin, v.oid, champ.value); } catch (err) { erreur.textContent = err.message; return; }
    d.fermer();
    app.statut(t('versions_nommee', { nom: r.nom }));
    apresAction(app, pid);
  };
  champ.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') valider(); });
  const d = dialogue({ titre: t('versions_nommer_titre', { date: dateLongue(v.date) }),
    contenu: el('div', { class: 'dialogue-version' }, el('p', {}, el('b', {}, descriptionVersion(v))), el('p', { class: 'note' }, t('versions_nommer_aide')),
      el('label', { class: 'champ' }, t('versions_nom')), champ, liste, erreur),
    boutons: [{ libelle: t('fermer'), action: () => true }, { libelle: t('versions_bouton_nommer'), principal: true, action: () => { valider(); return false; } }] });
}

// Restore…: asked first; changes that are in no version saved in one before (the main process
// refuses to restore over them); then the project read again from its file.
async function restaurerVersion(app, pid, v) {
  await vider(app, pid);
  let x = await etatPour(app, pid);
  if (!x || !(await assurerAuteur(x.e))) return false;
  const date = dateLongue(v.date);
  const oui = await demanderChoix({ titre: t('versions_restaurer_titre', { date }),
    texte: [el('p', {}, el('b', {}, descriptionVersion(v))), t('versions_restaurer_texte', { longue: dateLongue(v.date, { style: 'long' }) })],
    boutons: [{ libelle: t('versions_bouton_restaurer'), principal: true, valeur: true }] });
  if (!oui) return false;
  const p = app.projet(pid);
  if (x.e.modifie || p.modifie) {
    // (unsaved changes alone can be let go; changes saved since the last version must go in one)
    const choix = await demanderChoix({ titre: t('versions_avant_titre'),
      texte: x.e.modifie ? t('versions_avant_fichier', { date }) : t('versions_avant_texte'),
      boutons: x.e.modifie ? [{ libelle: t('versions_enregistrer_restaurer'), principal: true, valeur: 'version' }]
        : [{ libelle: t('versions_sans_garder'), valeur: 'laisser' }, { libelle: t('versions_enregistrer_dabord'), principal: true, valeur: 'version' }] });
    if (!choix) return false;
    if (choix === 'version') {
      if (app.projet(pid).modifie && !(await app.enregistrer(pid, false))) return false;
      oublier(x.chemin);
      x = await etatPour(app, pid);
      if (!x) return false;
      if (x.e.modifie && !(await essayer(app, () => couche().enregistrerVersion(x.chemin, t('versions_avant_description', { date }))))) return false;
    }
  }
  if (!(await essayer(app, () => couche().restaurer(x.chemin, v.oid)))) { apresAction(app, pid); return false; }
  oublier(x.chemin);
  await app.rechargerProjet(pid);
  app.statut(t('versions_restauree', { date }));
  return true;
}

// Restore from an old version opened read-only: in its project when it is open (the same steps),
// otherwise in its file, which then opens
async function restaurerDepuisLecture(app, pid) {
  const l = app.projet(pid).lecture_seule;
  const v = { oid: l.oid, date: l.date, description: '' };
  let source = app.projets().find((x) => !x.lecture_seule && memeFichier(x.chemin_enregistrement, l.source));
  if (!source) {
    await app.ouvrirChemin(l.source);
    source = app.projets().find((x) => !x.lecture_seule && memeFichier(x.chemin_enregistrement, l.source));
    if (!source) return;
  }
  const liste = await couche().liste(l.source).catch(() => []);
  const trouvee = liste.find((x) => x.oid === l.oid);
  await restaurerVersion(app, source.id, trouvee || v);
}

// Save on an old version opened read-only: refused, with Save as… and Restore… (app.js:
// enregistrer). Resolves to true once a copy is saved.
async function refuserEnregistrement(app, pid) {
  const p = app.projet(pid);
  const choix = await demanderChoix({ titre: t('versions_lecture_titre'),
    texte: t('versions_lecture_texte', { nom: p.nom, source: nomDeChemin(p.lecture_seule.source) }),
    boutons: [{ libelle: t('versions_restaurer'), valeur: 'restaurer' }, { libelle: t('versions_enregistrer_sous'), principal: true, valeur: 'sous' }] });
  if (choix === 'sous') return app.enregistrer(pid, true);
  if (choix === 'restaurer') await restaurerDepuisLecture(app, pid);
  return false;
}

// ------------------------------------------------------------ GitHub and GitLab

// Connect: the repository's address and a token that can write to it, with where to make both;
// Check keeps them and asks the repository whether the token may send versions there.
async function dialogueConnexion(app, pid) {
  const x = await etatPour(app, pid);
  if (!x) return;
  const e = x.e;
  const adresse = el('input', { type: 'url', value: (e.distant && e.distant.url) || '', placeholder: t('versions_adresse_exemple'), style: 'width:100%', spellcheck: 'false' });
  const jeton = el('input', { type: 'password', style: 'width:100%', autocomplete: 'off', spellcheck: 'false',
    placeholder: e.distant && e.jeton ? t('versions_jeton_garder', { hote: nomHote(e.distant.hote) || e.distant.hote }) : '' });
  const message = el('p', { class: 'message-connexion', 'aria-live': 'polite' });
  const liens = el('div', { class: 'liens-connexion' });
  const lien = (href, texte) => el('a', { href, target: '_blank', rel: 'noopener' }, icone('external-link'), texte);
  const dessinerLiens = () => {
    const l = liensConnexion(adresse.value);
    liens.replaceChildren(el('ol', {},
      el('li', {}, t('versions_etape_depot'), ' ', l.github ? lien(l.github.depot, t('versions_lien_github_depot')) : null, ' ', l.gitlab ? lien(l.gitlab.depot, t('versions_lien_gitlab_depot')) : null),
      el('li', {}, t('versions_etape_jeton'), ' ', l.github ? lien(l.github.jeton, t('versions_lien_github_jeton')) : null, ' ', l.gitlab ? lien(l.gitlab.jeton, t('versions_lien_gitlab_jeton')) : null),
      el('li', {}, t('versions_etape_coller'))));
  };
  adresse.addEventListener('input', dessinerLiens);
  dessinerLiens();
  let enCours = false;
  const verifier = async () => {
    if (enCours) return;
    enCours = true;
    message.className = 'message-connexion doux';
    message.textContent = t('versions_verification');
    try {
      const c = couche();
      const d = await c.distant(x.chemin, adresse.value);
      // (the address as the main process keeps it: a page's address cut to its repository)
      adresse.value = d.url;
      dessinerLiens();
      if (jeton.value.trim() && d.hote) await c.jeton(d.hote, jeton.value.trim());
      jeton.value = '';
      const r = await c.verifierDistant(x.chemin);
      message.className = `message-connexion ${r.ok ? 'ok-connexion' : 'erreur'}`;
      message.textContent = r.message;
      apresAction(app, pid);
      if (r.ok) {
        const apres = await lireEtat(x.chemin).catch(() => null);
        if (apres && apres.jetonSession) message.textContent = `${r.message} ${t('versions_jeton_session')}`;
        app.statut(t('versions_connexion_ok', { hote: nomHote(d.hote) || d.hote }));
      }
    } catch (err) {
      message.className = 'message-connexion erreur';
      message.textContent = err.message;
    } finally { enCours = false; }
  };
  dialogue({ titre: t('versions_connexion_titre'), large: false,
    contenu: el('div', { class: 'dialogue-connexion' }, el('p', {}, t('versions_connexion_texte')), liens,
      el('div', { class: 'grille-champs' },
        el('label', {}, t('versions_adresse')), adresse,
        el('label', {}, t('versions_jeton')), el('div', {}, jeton, el('p', { class: 'note' }, t('versions_jeton_aide')))),
      message),
    boutons: [{ libelle: t('fermer'), action: () => true }, { libelle: t('versions_verifier'), principal: true, action: () => { verifier(); return false; } }] });
}

async function verifierConnexion(app, pid) {
  const x = await etatPour(app, pid);
  if (!x) return;
  const r = await essayer(app, () => couche().verifierDistant(x.chemin));
  if (r && r.message) app.statut(r.message, !r.ok);
}

async function oublierJeton(app, pid, hote) {
  if (!(await essayer(app, () => couche().jeton(hote, '')))) return;
  app.statut(t('versions_jeton_oublie'));
  apresAction(app, pid);
}

async function envoyerVersions(app, pid) {
  const x = await etatPour(app, pid);
  if (!x) return;
  const ok = await essayer(app, () => couche().envoyer(x.chemin));
  if (ok) app.statut(t('versions_envoyees', { hote: nomHote(x.e.distant.hote) || x.e.distant.hote }));
  apresAction(app, pid);
}

// Get versions: refused while the project has changes in no version (a version is offered first);
// up to date; ahead, the project read again from its file; both sides saved versions, the choice.
async function recevoirVersions(app, pid) {
  await vider(app, pid);
  let x = await etatPour(app, pid);
  if (!x) return;
  const hote = nomHote(x.e.distant && x.e.distant.hote) || (x.e.distant && x.e.distant.url) || '';
  if (x.e.modifie || app.projet(pid).modifie) {
    const oui = await demanderChoix({ titre: t('versions_avant_titre'), texte: t('versions_recevoir_refuse', { hote }),
      boutons: [{ libelle: t('versions_enregistrer_dabord'), principal: true, valeur: true }] });
    if (!oui || !(await enregistrerUneVersion(app, pid))) return;
    x = await etatPour(app, pid);
    if (!x) return;
  }
  const r = await essayer(app, () => couche().recevoir(x.chemin));
  if (!r) { apresAction(app, pid); return; }
  oublier(x.chemin);
  if (r.resultat === 'a_jour') { app.statut(t('versions_recu_a_jour', { hote })); apresAction(app, pid); return; }
  if (r.resultat === 'avance') {
    await app.rechargerProjet(pid);
    app.statut(t('versions_recu_avance', { hote }));
    return;
  }
  apresAction(app, pid);
  await dialogueDivergence(app, pid, r, hote);
}

// Both sides saved versions since a common one: each side's, then Keep mine, Take theirs, or
// Compare first (their file read from what was received)
async function dialogueDivergence(app, pid, r, hote) {
  const chemin = fichierProjet(app.projet(pid));
  const liste = await couche().liste(chemin).catch(() => []);
  const d = resumeDivergence(r, liste);
  const e = await lireEtat(chemin).catch(() => null);
  const colonne = (titre, versions) => el('div', { class: 'cote-divergence' }, el('h4', {}, titre),
    el('ul', {}, versions.map((v) => el('li', {}, el('b', {}, descriptionVersion(v)), el('span', { class: 'doux' }, ` — ${dateVersion(v.date)}${v.auteur && v.auteur.nom ? `, ${v.auteur.nom}` : ''}`)))));
  const choix = await demanderChoix({ titre: t('versions_divergence_titre'),
    texte: [phraseDivergence(d, e && e.distant ? e.distant.hote : hote), el('div', { class: 'cotes-divergence' }, colonne(t('versions_ici'), d.mien), colonne(t('versions_la_bas', { hote }), d.leur))],
    boutons: [{ libelle: t('versions_comparer_dabord'), valeur: 'comparer' }, { libelle: t('versions_prendre_leurs'), valeur: 'leur' }, { libelle: t('versions_garder_miennes'), valeur: 'mien' }] });
  if (choix === 'comparer') {
    const leur = d.leur[0];
    if (!leur) return;
    const lu = await essayer(app, () => couche().lire(chemin, leur.oid));
    if (!lu) return;
    const date = dateLongue(leur.date);
    await comparerFichier(app, pid, lu.fichier, { titre: t('versions_comparaison_leur', { hote, date }), date, avant: false });
  } else if (choix) await choisirCote(app, pid, choix, hote);
}

async function choisirCote(app, pid, cote, hote) {
  await vider(app, pid);
  const x = await etatPour(app, pid);
  if (!x || !(await assurerAuteur(x.e))) return;
  const ok = await essayer(app, () => couche().choisir(x.chemin, cote));
  oublier(x.chemin);
  // (theirs: the project file is theirs now; a failed send after the choice keeps it, said in the message)
  if (cote === 'leur' && (ok || (await lireEtat(x.chemin).catch(() => ({}))).avance)) await app.rechargerProjet(pid);
  else apresAction(app, pid);
  if (ok) app.statut(t(cote === 'mien' ? 'versions_gardees_miennes' : 'versions_prises_leurs', { hote }));
}
