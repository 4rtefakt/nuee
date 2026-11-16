// File › History…: the versions Nuée keeps of each project by itself, in a hidden folder beside it
// (app/versions.js, which preload.js gives the page as window.bureau.versions), and the project
// shared with GitHub or GitLab, kept in step in the background. People read history, versions,
// named versions, send and get: never the words of the tool underneath. What the main process
// refuses comes back as an error whose message, in the interface's language, is shown as it is.
//   - the main process told when a project opens and when its unsaved state flips (ouvert), after
//     each save (apresEnregistrement, with the former path after Save as) and when it closes
//     (aLaFermeture): the hooks app.js calls (suivreProjet, apresEnregistrementVersions,
//     fermetureVersions);
//   - what the background did (surVersions): a version received reloads the open project and says
//     so, with What changed; a conflict, a wait or an error shows in the status bar;
//   - a version opened beside the project, read-only (app.ouvrirLectureSeule; the engine keeps its
//     source, oid and date: serveur.R, ouvrir), which Save refuses (app.refuserEnregistrement);
//   - a version compared with the project now: opened in the engine only, its models fitted
//     quietly, then What changed (changements.js);
//   - a version restored, then the project read again from its file (app.rechargerProjet);
//   - a conflict: both sides' versions, the engine's merge by meaning when it has one (requests
//     fusionner and fusionner_finir), else Keep mine or Take theirs; Compare first;
//   - a project on a shared folder changed on disk by someone else (request fichier_change):
//     their changes merged into the open project, when the engine can.
import { t, langueActuelle, deuxPoints } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander, bureau } from './api.js';
import { VUES } from './panneaux.js';
import { dateLongue, dateVersion, heureVersion, grouperParJour, tailleLisible, titreVersion, nomDeChemin, memeFichier, nomHote, hoteAdresse, liensConnexion,
  placeDansDepot, etatPartage, phraseRecu, autreAuteur, titreDeVersion, descriptionVersion, nombreVersions, demandeAbsente, sansExtension } from './versions_texte.js';
import { resumerProjet, afficherChangements, installerChangements } from './changements.js';

// The main process's functions; none in a browser
const couche = () => (bureau && bureau.versions && typeof bureau.versions.etat === 'function' && typeof bureau.versions.ouvert === 'function' ? bureau.versions : null);
// (a view moved to its own window: no hooks, no background, no dialogs)
const VUE_SEULE = new URLSearchParams(location.search).get('vue');

// The file whose versions are kept: a project saved by Nuée (.nuee), not an old version opened read-only
const fichierProjet = (p) => (p && !p.lecture_seule && p.chemin_enregistrement && /\.nuee$/i.test(p.chemin_enregistrement) ? p.chemin_enregistrement : null);
const projetDuFichier = (app, chemin) => app.projets().find((x) => memeFichier(fichierProjet(x), chemin)) || null;

// What the main process said of each project file lately: the status bar asks at most once a
// minute (and never twice at a time), a panel each time it is drawn, an action right after it.
const memo = new Map();  // file → { etat, erreur, quand, promesse }
function lireEtat(chemin) {
  const promesse = couche().etat(chemin).then(
    (e) => {
      memo.set(chemin, { etat: e, quand: Date.now() });
      if (e && e.derniere) dernieres.set(chemin, e.derniere.oid);
      return e;
    },
    (err) => { memo.set(chemin, { etat: null, erreur: err.message, quand: Date.now() }); throw err; });
  memo.set(chemin, { ...(memo.get(chemin) || {}), quand: Date.now(), promesse });
  return promesse;
}
const oublier = (chemin) => memo.delete(chemin);
// What the background last told of each file, until the next exchange says otherwise: { type:
// 'envoi' | 'attente' | 'conflit' | 'erreur', message, mien, leur, fichiers }
const vus = new Map();
// The last version known of each file, before the background brings another: What changed, after
const dernieres = new Map();
// The open projects as the main process was told (ouvert): pid → { chemin, modifie }
const suivis = new Map();
// The notice of the status bar: { texte, actions: [{ libelle, action }] }, until closed or replaced
let avis = null;

export function installerVersions(app) {
  installerChangements(app);
  VUES.versions = {
    rendre: (conteneur, ctx, params) => rendreHistorique(app, conteneur, params),
    titre: (_, p) => `${p.nom || ''} · ${t('versions_vue')}`,
    projet: true,
  };
  Object.assign(app, {
    versions: (pid) => {
      const id = pid || app.projetActif();
      if (!app.projet(id)) { app.statut(t('choisir_projet'), true); return; }
      app.ouvrirVue('versions', null, null, id);
    },
    partagerProjet: (pid) => dialoguePartage(app, pid || app.projetActif()),
    ouvrirDepuisDistant: () => dialogueDistant(app),
    indicateurVersions: () => indicateur(app),
    refuserEnregistrement: (pid) => refuserEnregistrement(app, pid),
    // (app.js's hooks: a project's summary received, a save done, a project closed)
    suivreProjet: (p) => suivreProjet(app, p),
    apresEnregistrementVersions: (pid, ancien) => apresEnregistrement(app, pid, ancien),
    fermetureVersions: (p) => fermeture(p),
    // (Save that met a file changed elsewhere: its changes merged first, when the engine can)
    fusionnerFichier: (pid) => fusionnerFichierChange(app, pid, { avantEnregistrer: true }),
    fusionPossible: () => moteurFusion(),
  });
  const c = couche();
  if (!c || VUE_SEULE) return;
  if (typeof c.surVersions === 'function') c.surVersions((e) => { evenement(app, e).catch(() => {}); });
  // a shared folder: every 30 seconds, whether an open project's file was changed by someone else
  setInterval(() => surveillerDossiers(app).catch(() => {}), 30000);
}

// ------------------------------------------------------------ the hooks

// A project's summary received (app.js: projetRecu): the main process told the first time it opens,
// and each time its unsaved state flips (a background receive never writes over unsaved changes)
function suivreProjet(app, p) {
  const c = couche();
  if (!c || !p || VUE_SEULE) return;
  const chemin = fichierProjet(p);
  const avant = suivis.get(p.id);
  if (!chemin) { if (avant) suivis.delete(p.id); return; }
  const modifie = !!p.modifie;
  if (avant && memeFichier(avant.chemin, chemin) && avant.modifie === modifie) return;
  suivis.set(p.id, { chemin, modifie });
  c.ouvert(chemin, { modifie }).then(() => { if (!avant) lireEtat(chemin).then(() => app.majStatut(), () => {}); }, () => {});
}

// After a successful save: a version when the last is an hour old, then sent in the background
// when shared; Save as copies the history (`ancien`: the former file)
async function apresEnregistrement(app, pid, ancien) {
  const c = couche();
  const p = app.projet(pid);
  const chemin = fichierProjet(p);
  if (!c || !chemin) return;
  const avecAncien = ancien && /\.nuee$/i.test(ancien) && !memeFichier(ancien, chemin);
  suivis.set(pid, { chemin, modifie: false });
  try {
    const r = await c.apresEnregistrement(chemin, avecAncien ? { ancien } : {});
    if (r && r.version) dernieres.set(chemin, r.version.oid);
  } catch { /* the history can't be written now: the next save tries again */ }
  apresAction(app, pid);
}

// A project closed: its last version if the file changed since, sent when shared
function fermeture(p) {
  const c = couche();
  const chemin = fichierProjet(p);
  if (p) suivis.delete(p.id);
  if (c && chemin) c.aLaFermeture(chemin).catch(() => {});
}

// ------------------------------------------------------------ what the background did

// (several main windows: the first one acts, as it writes the recovery copies: app.js)
async function fenetreQuiAgit() {
  try {
    const principales = (await bureau.fenetres()).filter((f) => f.principale).sort((a, b) => a.id - b.id);
    return !principales.length || principales[0].actuelle;
  } catch { return true; }
}

async function evenement(app, e) {
  const chemin = e && e.chemin;
  if (!chemin) return;
  const connu = (memo.get(chemin) || {}).etat;
  const hote = nomHote(connu && connu.partage && connu.partage.hote) || (connu && connu.partage && connu.partage.url) || 'GitHub';
  oublier(chemin);
  if (e.type === 'envoi') vus.set(chemin, { type: 'envoi' });
  else if (['envoye', 'recu'].includes(e.type)) vus.delete(chemin);
  else if (e.type === 'erreur') vus.set(chemin, { type: 'erreur', message: e.message });
  else if (e.type === 'attente') vus.set(chemin, { type: 'attente' });
  else if (e.type === 'conflit') vus.set(chemin, { type: 'conflit', mien: e.mien, leur: e.leur, fichiers: e.fichiers });
  const p = projetDuFichier(app, chemin);
  if (p && (await fenetreQuiAgit())) {
    if (e.type === 'recu') await recuEnArrierePlan(app, p, e.version);
    else if (e.type === 'conflit') {
      montrerAvis(app, t('versions_conflit_avis', { nom: sansExtension(p.nom) }), [{ libelle: t('versions_choisir'), action: () => resoudreConflit(app, p.id) }]);
    } else if (e.type === 'attente') {
      montrerAvis(app, t('versions_attente', { nom: sansExtension(p.nom), hote }), [{ libelle: t('versions_enregistrer_recevoir'), action: () => recevoirMaintenant(app, p.id) }]);
    }
  }
  if (p) redessinerHistorique(p.id);
  lireEtat(chemin).then(() => app.majStatut(), () => app.majStatut());
}

// A version written to an open project's file by the background: the project read again, when it
// has no unsaved change, and said so, with What changed against the version it had
async function recuEnArrierePlan(app, p, version) {
  const chemin = fichierProjet(p);
  const avant = dernieres.get(chemin) || null;
  if (version && version.oid) dernieres.set(chemin, version.oid);
  if (p.modifie) {
    montrerAvis(app, t('versions_recu_non_recharge', { nom: sansExtension(p.nom) }), [{ libelle: t('versions_recharger'), action: () => app.rechargerProjet(p.id) }]);
    return;
  }
  const pid = await app.rechargerProjet(p.id);
  annoncerRecu(app, pid, version, avant);
}

function annoncerRecu(app, pid, version, avant) {
  const texte = phraseRecu(version || {});
  const titre = t('versions_comparaison_recu', { nom: autreAuteur(version) });
  montrerAvis(app, texte, avant && pid ? [{ libelle: t('versions_voir_changements'), action: () => comparerVersion(app, pid, { oid: avant, date: null }, { titre }) }] : []);
  app.statut(texte);
}

function montrerAvis(app, texte, actions = []) {
  avis = { texte, actions };
  app.majStatut();
}

// ------------------------------------------------------------ the status bar

// The clock of History, with the shared state (up to date, sending, n to send, newer version,
// conflict, error with a retry), and the notice of what the background did
function indicateur(app) {
  const pid = app.projetActif();
  const p = pid && app.projet(pid);
  const chemin = fichierProjet(p);
  const elements = [];
  if (couche() && chemin) {
    const m = memo.get(chemin);
    if (!m || (!m.promesse && Date.now() - m.quand > 60000)) lireEtat(chemin).then(() => app.majStatut(), () => {});
    const e = m && m.etat;
    if (e) {
      const etat = etatPartage(e, vus.get(chemin));
      const titre = [!e.actif ? t('versions_indicateur_desactive') : e.versions ? t(e.versions === 1 ? 'versions_indicateur_local_une' : 'versions_indicateur_local', { n: e.versions })
        : t('versions_indicateur_aucune'), etat ? etat.titre : ''].filter(Boolean).join(' · ');
      elements.push(el('button', { class: `icone-seule indicateur-versions${etat ? ` ${etat.cle}` : ''}`, title: titre, 'aria-label': titre,
        onclick: () => (etat && etat.cle === 'conflit' ? resoudreConflit(app, pid) : app.versions(pid)) },
      icone(etat && etat.cle === 'erreur' ? 'circle-alert' : 'history'), etat ? el('span', {}, etat.court) : null));
      if (etat && etat.cle === 'erreur') {
        elements.push(el('button', { class: 'icone-seule reessayer-versions', title: t('versions_reessayer'), onclick: () => envoyerMaintenant(app, pid) }, icone('refresh-cw')));
      }
    }
  }
  if (avis) {
    elements.push(el('span', { class: 'avis-versions' }, el('span', {}, avis.texte),
      avis.actions.map((a) => el('a', { href: '#', onclick: (ev) => { ev.preventDefault(); avis = null; app.majStatut(); a.action(); } }, a.libelle)),
      el('button', { class: 'icone-seule', title: t('fermer'), 'aria-label': t('fermer'), onclick: () => { avis = null; app.majStatut(); } }, icone('x'))));
  }
  return elements.length ? el('span', { class: 'statut-versions' }, elements) : null;
}

// ------------------------------------------------------------ the panel

// The project's panels drawn again, and the status bar, after an action (the file's state asked again)
function apresAction(app, pid) {
  const chemin = fichierProjet(app.projet(pid));
  if (chemin) oublier(chemin);
  redessinerHistorique(pid);
  if (chemin) lireEtat(chemin).then(() => app.majStatut(), () => {});
}
function redessinerHistorique(pid) {
  for (const c of document.querySelectorAll('[data-versions]')) if (c.dataset.versions === pid && c._redessiner) c._redessiner();
}

async function rendreHistorique(app, conteneur, params) {
  const p = app.projet(params.projet);
  if (!p) return;
  conteneur.dataset.versions = p.id;
  conteneur._redessiner = () => rendreHistorique(app, conteneur, params);
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
      el('div', { class: 'ligne' }, el('button', { class: 'principal', onclick: async () => { if (await app.enregistrer(p.id, !!p.chemin_enregistrement)) redessinerHistorique(p.id); } },
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
    liste = await c.liste(chemin);
  } catch (err) {
    if (numero === conteneur.numeroVersions) {
      montrer(el('p', { class: 'erreur' }, err.message), el('button', { onclick: () => rendreHistorique(app, conteneur, params) }, icone('refresh-cw'), t('versions_actualiser')));
    }
    return;
  }
  if (numero !== conteneur.numeroVersions) return;
  // (the newest version is the project as it is when nothing changed since: no comparing or restoring it)
  const actuelle = !p.modifie && liste.length && e.derniere && liste[0].oid === e.derniere.oid ? liste[0].oid : null;
  const partage = !!e.partage;
  montrer(
    el('div', { class: 'outils' },
      el('button', { class: 'principal', disabled: !e.actif, onclick: () => enregistrerUneVersion(app, p.id) }, icone('save'), t('versions_maintenant')),
      el('button', { onclick: () => apresAction(app, p.id) }, icone('refresh-cw'), t('versions_actualiser')),
      el('button', { onclick: () => dialogueReglages(app, p.id) }, icone('sliders-horizontal'), t('versions_reglages')),
      el('label', { class: 'champ interrupteur-versions' }, el('input', { type: 'checkbox', checked: !!e.actif, onchange: (ev) => activer(app, p.id, ev.target.checked) }), t('versions_garder_projet'))),
    el('p', { class: 'note tete-versions', title: e.dossier || '' }, icone('lock'),
      !e.actif ? t('versions_desactivees') : e.versions ? t('versions_tete', { taille: tailleLisible(e.taille) }) : t('versions_tete_vide')),
    sectionPartage(app, p, e),
    el('div', { class: 'liste-versions' }, liste.length ? grouperParJour(liste).map((g) => el('section', { class: 'jour-versions' },
      el('h4', {}, g.titre), g.versions.map((v) => ligneVersion(app, p, v, v.oid === actuelle, partage))))
      : el('p', { class: 'doux' }, t('versions_aucune_encore'))),
    el('p', { class: 'note' }, liste.length ? `${nombreVersions(liste.length)} · ` : '', signature(), ' ',
      el('a', { href: '#', onclick: (ev) => { ev.preventDefault(); dialogueReglages(app, p.id); } }, t('versions_reglages'))));
}

// "Versions signed Marie Essai <marie@example.org>", from the settings
function signature() {
  const s = el('span', {});
  couche().reglages().then((r) => {
    const a = r && r.auteur;
    if (a) s.textContent = t('versions_auteur', { nom: a.email ? t('versions_auteur_email_court', a) : a.nom });
  }, () => {});
  return s;
}

function ligneVersion(app, p, v, actuelle, partage) {
  const auteur = (v.auteur && v.auteur.nom) || '';
  const noms = [].concat(v.noms || []);
  return el('div', { class: `version${actuelle ? ' actuelle' : ''}${v.auto && !noms.length ? ' auto' : ''}` },
    el('span', { class: 'heure-version' }, heureVersion(v.date)),
    el('div', { class: 'version-texte' },
      el('div', { class: 'version-tete' },
        noms.map((n) => el('span', { class: 'badge nom-version' }, icone('bookmark'), n)),
        el(noms.length ? 'span' : 'b', { class: noms.length ? 'doux' : '' }, descriptionVersion(v)),
        actuelle ? el('span', { class: 'badge bon' }, t('versions_actuelle')) : null),
      el('div', { class: 'doux', title: v.auteur && v.auteur.email ? `${auteur} <${v.auteur.email}>` : auteur },
        auteur, v.auto ? [' · ', el('span', { class: 'marque-version', title: t('versions_auto_aide') }, t('versions_auto'))] : null,
        partage && v.envoyee ? [' · ', el('span', { class: 'marque-version envoyee', title: t('versions_envoyee_aide') }, icone('cloud'), t('versions_envoyee'))] : null)),
    el('div', { class: 'version-actions' },
      el('button', { title: t('versions_ouvrir_aide'), onclick: () => ouvrirVersion(app, p.id, v) }, icone('folder-open'), t('versions_ouvrir')),
      el('button', { title: t('versions_comparer_aide'), disabled: actuelle, onclick: () => comparerVersion(app, p.id, v) }, icone('diff'), t('versions_comparer')),
      el('button', { onclick: () => nommerVersion(app, p.id, v) }, icone('bookmark'), t('versions_nommer')),
      el('button', { disabled: actuelle, onclick: () => restaurerVersion(app, p.id, v) }, icone('rotate-ccw'), t('versions_restaurer'))));
}

// Shared with GitHub or GitLab: its state, Send now, Get now, the token, Stop sharing; or Share
function sectionPartage(app, p, e) {
  const chemin = fichierProjet(p);
  if (!e.partage) {
    return el('div', { class: 'partage-versions' }, el('button', { disabled: !e.actif, onclick: () => dialoguePartage(app, p.id) }, icone('cloud-upload'), t('versions_partager')),
      el('span', { class: 'note' }, t('versions_partager_texte')));
  }
  const x = e.partage;
  const etat = etatPartage(e, vus.get(chemin));
  const hote = nomHote(x.hote) || x.url;
  return el('div', { class: 'partage-versions' },
    el('div', { class: 'ligne' },
      el('span', {}, icone('cloud'), ' ', t('versions_partage_avec', { hote })), el('span', { class: 'adresse-versions' }, x.url),
      etat ? el('span', { class: `badge etat-partage ${etat.cle}` }, etat.court) : null),
    x.chemin && x.chemin !== nomDeChemin(chemin) ? el('p', { class: 'note' }, placeDansDepot(nomDeChemin(chemin), x.chemin)) : null,
    e.conflit || (vus.get(chemin) || {}).type === 'conflit' ? el('div', { class: 'banniere-versions' },
      el('span', {}, t('versions_conflit_avis', { nom: sansExtension(p.nom) })), el('button', { class: 'principal', onclick: () => resoudreConflit(app, p.id) }, t('versions_choisir'))) : null,
    etat && etat.cle === 'erreur' ? el('div', { class: 'banniere-versions erreur-versions' }, el('span', {}, (vus.get(chemin) || {}).message || x.erreur),
      el('button', { onclick: () => envoyerMaintenant(app, p.id) }, icone('refresh-cw'), t('versions_reessayer'))) : null,
    el('div', { class: 'ligne' },
      el('button', { disabled: !x.hote, onclick: () => envoyerMaintenant(app, p.id) }, icone('cloud-upload'), t('versions_envoyer_maintenant')),
      el('button', { disabled: !x.hote, onclick: () => recevoirMaintenant(app, p.id) }, icone('cloud-download'), t('versions_recevoir_maintenant')),
      el('button', { onclick: () => verifierPartage(app, p.id) }, icone('circle-check'), t('versions_verifier')),
      el('button', { onclick: () => dialoguePartage(app, p.id) }, icone('link'), t('versions_changer_jeton')),
      connexionDisponible('ouvrirInvitation') ? el('button', { onclick: () => inviter(app, x) }, icone('user-round'), t('versions_inviter')) : null,
      el('button', { onclick: () => arreterPartage(app, p.id, hote) }, icone('x'), t('versions_arreter_partage'))),
    el('p', { class: 'note' }, x.jeton ? t(x.jetonSession ? 'versions_jeton_session' : 'versions_jeton_garde') : t('versions_jeton_aucun'),
      x.dernierEchange ? ` · ${t('versions_dernier_echange', { date: dateVersion(x.dernierEchange) })}` : ''));
}

// An old version opened read-only: what it is, Restore, Save as, and its project's history
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

// The author's fields (name, email), filled with the settings' author
function champsAuteur(auteur) {
  const nom = el('input', { value: (auteur && auteur.nom) || '', style: 'width:22em', autocomplete: 'name' });
  const email = el('input', { type: 'email', value: (auteur && auteur.email) || '', style: 'width:22em', autocomplete: 'email' });
  return { nom, email, lignes: [el('label', {}, t('versions_auteur_nom')), nom, el('label', {}, t('versions_auteur_email')), email] };
}

// Settings: keep versions of my projects, and who saves them
async function dialogueReglages(app, pid) {
  const c = couche();
  let r;
  try { r = await c.reglages(); } catch (err) { app.statut(err.message, true); return; }
  const auto = el('input', { type: 'checkbox', checked: r.auto !== false });
  const a = champsAuteur(r.auteur);
  const erreur = el('p', { class: 'erreur' });
  const valider = async () => {
    erreur.textContent = '';
    const q = { auto: auto.checked };
    const nom = a.nom.value.trim();
    const email = a.email.value.trim();
    // (the author only once given whole: a name alone is the system's user, kept as it is)
    if (email || nom !== ((r.auteur && r.auteur.nom) || '')) {
      if (!nom || !email) { erreur.textContent = t('versions_auteur_manque'); return; }
      q.auteur = { nom, email };
    }
    try { await c.regler(q); } catch (err) { erreur.textContent = err.message; return; }
    d.fermer();
    app.statut(t('versions_reglages_gardes'));
    if (pid) apresAction(app, pid);
  };
  const d = dialogue({ titre: t('versions_reglages_titre'),
    contenu: el('div', { class: 'dialogue-version' },
      el('label', { class: 'champ' }, auto, el('b', {}, t('versions_reglage_auto'))), el('p', { class: 'note' }, t('versions_reglage_auto_aide')),
      el('h4', {}, t('versions_auteur_titre')), el('p', { class: 'note' }, t('versions_auteur_texte')),
      el('div', { class: 'grille-champs' }, a.lignes), erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('appliquer'), principal: true, action: () => { valider(); return false; } }] });
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

// Unsaved changes saved first, when the user agrees; true when there are none left
async function enregistrerDabord(app, pid, texte, bouton) {
  await vider(app, pid);
  if (!app.projet(pid).modifie) return true;
  const oui = await demanderChoix({ titre: t('versions_avant_titre'), texte, boutons: [{ libelle: bouton, principal: true, valeur: true }] });
  return !!oui && app.enregistrer(pid, false);
}

async function activer(app, pid, oui) {
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return;
  // (turned on: the first version is the file, what isn't saved yet goes in first)
  if (oui && app.projet(pid).modifie && !(await app.enregistrer(pid, false))) { apresAction(app, pid); return; }
  await essayer(app, () => couche().activer(chemin, oui));
  apresAction(app, pid);
}

// Save a version…: a few words, the project saved first if it has unsaved changes. The main
// process makes no version of a file as its last one (marquer then gives that one back): said so.
async function enregistrerUneVersion(app, pid) {
  await vider(app, pid);
  const p = app.projet(pid);
  const chemin = fichierProjet(p);
  if (!chemin) return false;
  const texte = await demanderDescription(p);
  if (!texte) return false;
  if (app.projet(pid).modifie && !(await app.enregistrer(pid, false))) return false;
  const x = await etatPour(app, pid);
  if (!x) return false;
  const r = await essayer(app, () => couche().marquer(chemin, texte));
  if (!r) { apresAction(app, pid); return false; }
  const pareil = r.oid && x.e.derniere && r.oid === x.e.derniere.oid;
  if (pareil) await demanderChoix({ titre: t('versions_maintenant').replace('…', ''), texte: t('versions_inchange'), boutons: [] });
  else app.statut(t('versions_enregistree', { description: texte }));
  if (r.oid) dernieres.set(chemin, r.oid);
  apresAction(app, pid);
  return !pareil;
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
async function comparerVersion(app, pid, v, { titre = null } = {}) {
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return;
  const lu = await essayer(app, () => couche().lire(chemin, v.oid));
  if (!lu) return;
  const date = v.date ? dateLongue(v.date) : '';
  await comparerFichier(app, pid, lu.fichier, { titre: titre || t('versions_comparaison_titre', { date }), date, avant: true });
}

// A project file (an old version, or theirs) and the project now: the file opened in the engine
// only (serveur.R: ouvrir, cache), its models fitted quietly and summed up, then closed; the
// project's own; then What changed. avant: the file is the side before (else after).
async function comparerFichier(app, pid, fichier, { titre, date, avant }) {
  let autre;
  try {
    const cache = await demander('ouvrir', { chemin: fichier, cache: true });
    try {
      autre = await resumerProjet(app, cache.id, { surProgression: ({ fait, total, nom }) => {
        if (nom) app.statut(t(date ? 'versions_progression_avant' : 'versions_progression_autre', { date, nom, k: fait + 1, n: total }));
      } });
    } finally {
      demander('fermer_projet', { projet: cache.id }).catch(() => {});
    }
  } catch (err) { app.statut(`${t('erreur')}${deuxPoints()}${err.message}`, true); return null; }
  if (!app.projet(pid)) return null;
  const actuel = await resumerProjet(app, pid, { surProgression: ({ fait, total, nom }) => { if (nom) app.statut(t('versions_progression_apres', { nom, k: fait + 1, n: total })); } });
  app.statut(t('versions_comparaison_faite'));
  return afficherChangements(app, { titre, pid, avant: avant ? autre : actuel, apres: avant ? actuel : autre });
}

// Name…: a name for the version, or one of its names removed
async function nommerVersion(app, pid, v) {
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return;
  let noms = [].concat(v.noms || []);
  const champ = el('input', { style: 'width:100%', maxlength: 100 });
  const erreur = el('p', { class: 'erreur' });
  const liste = el('div', { class: 'noms-version' });
  const dessiner = () => liste.replaceChildren(...(noms.length ? [el('span', { class: 'doux' }, t('versions_noms')), ...noms.map((n) => el('span', { class: 'badge nom-version' }, n,
    el('button', { class: 'icone-seule', title: t('versions_retirer_nom', { nom: n }), 'aria-label': t('versions_retirer_nom', { nom: n }), onclick: async () => {
      erreur.textContent = '';
      try { await couche().retirerNom(chemin, n); } catch (err) { erreur.textContent = err.message; return; }
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
    try { r = await couche().nommer(chemin, v.oid, champ.value); } catch (err) { erreur.textContent = err.message; return; }
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

// Restore…: asked first; unsaved changes saved, or let go (what the file has that no version has,
// the main process keeps in one first); then the project read again from its file.
async function restaurerVersion(app, pid, v) {
  await vider(app, pid);
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return false;
  const date = dateLongue(v.date);
  const oui = await demanderChoix({ titre: t('versions_restaurer_titre', { date }),
    texte: [el('p', {}, el('b', {}, titreDeVersion(v))), t('versions_restaurer_texte', { longue: dateLongue(v.date, { style: 'long' }) })],
    boutons: [{ libelle: t('versions_bouton_restaurer'), principal: true, valeur: true }] });
  if (!oui) return false;
  if (app.projet(pid).modifie) {
    const choix = await demanderChoix({ titre: t('versions_avant_titre'), texte: t('versions_avant_texte'),
      boutons: [{ libelle: t('versions_sans_garder'), valeur: 'laisser' }, { libelle: t('versions_enregistrer_restaurer'), principal: true, valeur: 'enregistrer' }] });
    if (!choix) return false;
    if (choix === 'enregistrer' && !(await app.enregistrer(pid, false))) return false;
  }
  if (!(await essayer(app, () => couche().restaurer(chemin, v.oid)))) { apresAction(app, pid); return false; }
  oublier(chemin);
  await app.rechargerProjet(pid);
  app.statut(t('versions_restauree', { date }));
  return true;
}

// Restore from an old version opened read-only: in its project when it is open (the same steps),
// otherwise in its file, which then opens
async function restaurerDepuisLecture(app, pid) {
  const l = app.projet(pid).lecture_seule;
  let source = app.projets().find((x) => !x.lecture_seule && memeFichier(x.chemin_enregistrement, l.source));
  if (!source) {
    await app.ouvrirChemin(l.source);
    source = app.projets().find((x) => !x.lecture_seule && memeFichier(x.chemin_enregistrement, l.source));
    if (!source) return;
  }
  const liste = await couche().liste(l.source).catch(() => []);
  await restaurerVersion(app, source.id, liste.find((x) => x.oid === l.oid) || { oid: l.oid, date: l.date, description: '' });
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

// The sign-in dialogs (connexion.js: ouvrirConnexion, ouvrirInvitation), when they are there
let connexion = null;
import('./connexion.js').then((m) => { connexion = m; }, () => { connexion = null; });
const connexionDisponible = (f) => !!(connexion && typeof connexion[f] === 'function');

// Share on GitHub or GitLab: signed in (connexion.js, when there) the repository made for the
// project; or a repository's address and a token that can write to it, with where to make both.
// The author's name and email asked the first time (GitHub and GitLab show who did what by it).
async function dialoguePartage(app, pid) {
  const p = app.projet(pid);
  const chemin = fichierProjet(p);
  if (!chemin) { app.versions(pid); return; }
  const c = couche();
  let e;
  let r;
  try { [e, r] = await Promise.all([lireEtat(chemin), c.reglages()]); } catch (err) { app.statut(err.message, true); return; }
  const a = r.auteur && r.auteur.email ? null : champsAuteur(r.auteur);
  const adresse = el('input', { type: 'url', value: (e.partage && e.partage.url) || '', placeholder: t('versions_adresse_exemple'), style: 'width:100%', spellcheck: 'false' });
  const jeton = el('input', { type: 'password', style: 'width:100%', autocomplete: 'off', spellcheck: 'false',
    placeholder: e.partage && e.partage.jeton ? t('versions_jeton_garder', { hote: nomHote(e.partage.hote) || e.partage.hote }) : '' });
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
  // (the author first, when not given yet)
  const auteurDonne = async () => {
    if (!a) return true;
    if (!a.nom.value.trim() || !a.email.value.trim()) { message.className = 'message-connexion erreur'; message.textContent = t('versions_auteur_manque'); return false; }
    await c.regler({ auteur: { nom: a.nom.value, email: a.email.value } });
    return true;
  };
  let enCours = false;
  const partager = async (url) => {
    if (enCours) return;
    enCours = true;
    message.className = 'message-connexion doux';
    message.textContent = t('versions_verification');
    try {
      if (!(await auteurDonne())) return;
      // (the token first when the address names its server: the first exchange needs it)
      const hote = hoteAdresse(url);
      if (jeton.value.trim() && hote) await c.jeton(hote, jeton.value.trim());
      const d = await c.partager(chemin, url);
      // (the address as the main process keeps it: a page's address cut to its repository)
      adresse.value = d.url;
      dessinerLiens();
      if (jeton.value.trim() && d.hote && d.hote !== hote) await c.jeton(d.hote, jeton.value.trim());
      jeton.value = '';
      const v = await c.verifierPartage(chemin);
      const place = placeDansDepot(nomDeChemin(chemin), d.chemin);
      message.className = `message-connexion ${v.ok ? 'ok-connexion' : 'erreur'}`;
      message.textContent = [v.message, place].filter(Boolean).join(' ');
      apresAction(app, pid);
      if (v.ok) app.statut(t('versions_partage_ok', { hote: nomHote(d.hote) || d.hote }));
    } catch (err) {
      message.className = 'message-connexion erreur';
      message.textContent = err.message;
    } finally { enCours = false; }
  };
  // (signed in: the service's account makes the repository, connexion.js and bureau.comptes)
  const compte = connexionDisponible('ouvrirConnexion') ? el('div', { class: 'ligne connexion-compte' }, ['github.com', 'gitlab.com'].map((h) => el('button', { onclick: async () => {
    const s = await connexion.ouvrirConnexion(app, { hote: h }).catch(() => null);
    if (!s || !s.ok) return;
    const comptes = bureau.comptes;
    if (comptes && typeof comptes.creerDepot === 'function') {
      try {
        const depot = await comptes.creerDepot({ hote: h, nom: sansExtension(p.nom), prive: true });
        if (depot && depot.url) { adresse.value = depot.url; await partager(depot.url); }
      } catch (err) { message.className = 'message-connexion erreur'; message.textContent = err.message; }
    }
  } }, icone('user-round'), t('versions_compte', { service: nomHote(h) })))) : null;
  dialogue({ titre: t('versions_partager_titre'),
    contenu: el('div', { class: 'dialogue-connexion' }, el('p', {}, t('versions_partager_texte')),
      a ? [el('h4', {}, t('versions_auteur_titre')), el('p', { class: 'note' }, t('versions_auteur_texte')), el('div', { class: 'grille-champs' }, a.lignes)] : null,
      compte ? [compte, el('p', { class: 'note' }, t('versions_ou_jeton'))] : null,
      liens,
      el('div', { class: 'grille-champs' },
        el('label', {}, t('versions_adresse')), adresse,
        el('label', {}, t('versions_jeton')), el('div', {}, jeton, el('p', { class: 'note' }, t('versions_jeton_aide')))),
      message),
    boutons: [{ libelle: t('fermer'), action: () => true }, { libelle: t('versions_verifier'), principal: true, action: () => { partager(adresse.value); return false; } }] });
}

async function inviter(app, partage) {
  try { await connexion.ouvrirInvitation(app, { hote: partage.hote, url: partage.url }); } catch (err) { app.statut(err.message, true); }
}

async function verifierPartage(app, pid) {
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return;
  const r = await essayer(app, () => couche().verifierPartage(chemin));
  if (r && r.message) app.statut(r.message, !r.ok);
  apresAction(app, pid);
}

async function arreterPartage(app, pid, hote) {
  const p = app.projet(pid);
  const chemin = fichierProjet(p);
  const oui = await demanderChoix({ titre: t('versions_arreter_partage'), texte: t('versions_arreter_texte', { nom: sansExtension(p.nom), hote }),
    boutons: [{ libelle: t('versions_arreter_partage'), principal: true, valeur: true }] });
  if (!oui || !(await essayer(app, () => couche().arreterPartage(chemin)))) return;
  vus.delete(chemin);
  app.statut(t('versions_partage_arrete'));
  apresAction(app, pid);
}

// What an exchange gave: up to date, received (the project read again, with What changed), a
// conflict (its dialog), a wait for unsaved changes, sent
async function apresEchange(app, pid, chemin, r, avant, hote) {
  vus.delete(chemin);
  if (r.resultat === 'recu') {
    if (r.version && r.version.oid) dernieres.set(chemin, r.version.oid);
    oublier(chemin);
    const nouveau = await app.rechargerProjet(pid);
    annoncerRecu(app, nouveau, r.version, avant);
    return;
  }
  if (r.resultat === 'conflit') {
    vus.set(chemin, { type: 'conflit', mien: r.mien, leur: r.leur, fichiers: r.fichiers });
    apresAction(app, pid);
    await dialogueConflit(app, pid, r);
    return;
  }
  if (r.resultat === 'attente') {
    vus.set(chemin, { type: 'attente' });
    app.statut(t('versions_attente', { nom: sansExtension(app.projet(pid).nom), hote }));
  } else if (r.resultat === 'envoye') app.statut(t('versions_envoyees', { hote }));
  else app.statut(t('versions_recu_a_jour', { hote }));
  apresAction(app, pid);
}

// An exchange asked by the user (Send now, Get now): its answer, or null when it failed, the
// failure kept for the status bar's indicator (with its retry) and said in the status bar
async function echangeDemande(app, chemin, f) {
  try { return await app.occupe(f); } catch (err) {
    vus.set(chemin, { type: 'erreur', message: err.message });
    return null;
  }
}

// Get now: unsaved changes saved first (the main process never writes over them)
async function recevoirMaintenant(app, pid) {
  const x = await etatPour(app, pid);
  if (!x || !x.e.partage) return;
  const hote = nomHote(x.e.partage.hote) || x.e.partage.url;
  if (!(await enregistrerDabord(app, pid, t('versions_recevoir_avant'), t('versions_enregistrer_recevoir')))) return;
  const avant = dernieres.get(x.chemin) || (x.e.derniere && x.e.derniere.oid) || null;
  const r = await echangeDemande(app, x.chemin, () => couche().recevoir(x.chemin));
  if (!r) { apresAction(app, pid); return; }
  await apresEchange(app, pid, x.chemin, r, avant, hote);
}

// Send now (also the retry after an error): what the file has that no version has goes in one
// first; the remote's newer versions come in before, as for Get now
async function envoyerMaintenant(app, pid) {
  const x = await etatPour(app, pid);
  if (!x || !x.e.partage) return;
  const hote = nomHote(x.e.partage.hote) || x.e.partage.url;
  await vider(app, pid);
  if (app.projet(pid).modifie && !(await app.enregistrer(pid, false))) return;
  const avant = dernieres.get(x.chemin) || (x.e.derniere && x.e.derniere.oid) || null;
  vus.set(x.chemin, { type: 'envoi' });
  app.majStatut();
  const r = await echangeDemande(app, x.chemin, () => couche().envoyer(x.chemin));
  if (!r) { apresAction(app, pid); return; }
  await apresEchange(app, pid, x.chemin, r, avant, hote);
}

// ------------------------------------------------------------ conflicts

// The engine's merge by meaning (requests fusionner, fusionner_finir): whether it has it, asked
// once (a request it doesn't know is answered "Unknown request")
let fusionConnue = null;
async function moteurFusion() {
  if (fusionConnue !== null) return fusionConnue;
  try { await demander('fusionner', {}); fusionConnue = true; } catch (e) { fusionConnue = !demandeAbsente(e); }
  return fusionConnue;
}

// The conflict of a shared project, from the History panel or the status bar: its versions and
// files, as Get now gives them
async function resoudreConflit(app, pid) {
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return;
  const v = vus.get(chemin);
  if (v && v.type === 'conflit' && v.fichiers) { await dialogueConflit(app, pid, v); return; }
  await recevoirMaintenant(app, pid);
}

// Both sides changed the project: the two latest versions; the engine's merge when it has one
// (with nothing clashing, Merge both and what each side brought; otherwise a choice per clash);
// Keep mine and Take theirs for the whole project; Compare first
async function dialogueConflit(app, pid, { mien, leur, fichiers }) {
  const p = app.projet(pid);
  const chemin = fichierProjet(p);
  if (!chemin) return;
  const qui = autreAuteur(leur);
  const e = memo.get(chemin) && memo.get(chemin).etat;
  const hote = nomHote(e && e.partage && e.partage.hote) || 'GitHub';
  const carte = (titre, v) => el('div', { class: 'cote-conflit' }, el('h4', {}, titre),
    v ? [el('p', {}, el('b', {}, titreDeVersion(v))), el('p', { class: 'doux' }, `${dateVersion(v.date)}${v.auteur && v.auteur.nom ? ` · ${v.auteur.nom}` : ''}`)] : null);
  const fusion = el('div', { class: 'fusion-conflit' }, el('p', { class: 'infos attente' }, t('versions_fusion_calcul')));
  const choix = {};
  let proposition = null;
  const boutons = [{ libelle: t('annuler'), action: () => true },
    { libelle: t('versions_comparer_conflit'), action: () => { comparerLeur(app, pid, leur, qui); return true; } },
    { libelle: t('versions_garder_miennes'), action: () => { choisirCote(app, pid, 'mien', null, hote); return true; } },
    { libelle: t('versions_prendre_leurs'), action: () => { choisirCote(app, pid, 'leur', null, hote); return true; } },
    { libelle: t('versions_fusionner'), principal: true, action: () => { fusionner(); return false; } }];
  const d = dialogue({ titre: t('versions_conflit_titre'),
    contenu: el('div', { class: 'dialogue-conflit' }, el('p', {}, t('versions_conflit_texte', { nom: qui })),
      p.modifie ? el('p', { class: 'note attention-versions' }, t('versions_conflit_enregistrer')) : null,
      el('div', { class: 'cotes-conflit' }, carte(t('versions_conflit_vous'), mien), carte(t('versions_conflit_leur', { nom: qui }), leur)), fusion),
    boutons });
  // (both sides side by side, wider than a question, not as tall as a large dialog)
  d.boite.classList.add('dialogue-moyen');
  const boutonFusion = [...d.boite.querySelectorAll('.pied-dialogue button')].pop();
  boutonFusion.disabled = true;
  const fusionner = async () => {
    if (!proposition) return;
    const fin = await essayer(app, () => demander('fusionner_finir', { fusion: proposition.fusion, choix }));
    if (!fin || !fin.chemin) return;
    d.fermer();
    await choisirCote(app, pid, 'fusion', fin.chemin, hote, t('versions_fusion_description', { nom: qui }));
  };
  // (the engine's merge, or none: Keep mine, Take theirs)
  let r = null;
  if (fichiers && fichiers.mien && fichiers.leur && (await moteurFusion())) {
    try { r = await demander('fusionner', { base: fichiers.base || null, mien: fichiers.mien, leur: fichiers.leur }); } catch { r = null; }
  }
  if (!d.boite.isConnected) return;
  if (!r) { fusion.remove(); boutonFusion.remove(); return; }
  proposition = r;
  const apports = (titre, phrases) => ([].concat(phrases || []).length ? el('div', { class: 'apports-conflit' }, el('h4', {}, titre), el('ul', {}, [].concat(phrases).map((x) => el('li', {}, x)))) : null);
  const conflits = [].concat(r.conflits || []);
  for (const x of conflits) choix[x.id] = 'mien';
  fusion.replaceChildren(
    el('p', { class: conflits.length ? '' : 'ok-connexion' }, t(conflits.length ? 'versions_fusion_conflits' : 'versions_fusion_auto')),
    conflits.length ? el('div', { class: 'liste-conflits' }, conflits.map((x) => el('div', { class: 'conflit-fusion' },
      el('div', { class: 'conflit-titre' }, el('b', {}, x.titre || x.cle || ''), x.details ? el('span', { class: 'doux' }, ` — ${[].concat(x.details).join(' ')}`) : null),
      ['mien', 'leur'].map((cote) => el('label', { class: 'champ' },
        el('input', { type: 'radio', name: `conflit-${x.id}`, checked: cote === 'mien', onchange: () => { choix[x.id] = cote; } }),
        el('span', {}, el('b', {}, t(cote === 'mien' ? 'versions_fusion_choix_mien' : 'versions_fusion_choix_leur')), ` — ${x[cote] || ''}`)))))) : null,
    el('div', { class: 'cotes-conflit' }, apports(t('versions_fusion_apports_mien'), r.apports && r.apports.mien), apports(t('versions_fusion_apports_leur', { nom: qui }), r.apports && r.apports.leur)));
  boutonFusion.textContent = t(conflits.length ? 'versions_fusion_appliquer' : 'versions_fusionner');
  boutonFusion.disabled = false;
}

async function comparerLeur(app, pid, leur, qui) {
  const chemin = fichierProjet(app.projet(pid));
  if (!leur || !chemin) return;
  const lu = await essayer(app, () => couche().lire(chemin, leur.oid));
  if (!lu) return;
  const date = dateLongue(leur.date);
  await comparerFichier(app, pid, lu.fichier, { titre: t('versions_comparaison_leur', { nom: qui, date }), date, avant: false });
}

// A conflict resolved: mine (the file stays), theirs or the merge (the file takes it, and the
// project is read again); sent in the background
async function choisirCote(app, pid, cote, fichier, hote, description = null) {
  await vider(app, pid);
  const chemin = fichierProjet(app.projet(pid));
  if (!chemin) return false;
  if (cote !== 'mien' && app.projet(pid).modifie && !(await app.enregistrer(pid, false))) return false;
  const ok = await essayer(app, () => couche().choisir(chemin, cote, fichier, description));
  if (!ok) { apresAction(app, pid); return false; }
  vus.delete(chemin);
  oublier(chemin);
  if (cote !== 'mien') await app.rechargerProjet(pid);
  else apresAction(app, pid);
  app.statut(t(cote === 'mien' ? 'versions_gardees_miennes' : cote === 'leur' ? 'versions_prises_leurs' : 'versions_fusion_faite', { hote }));
  return true;
}

// ------------------------------------------------------------ a shared folder

// Every 30 seconds, an open project whose file someone else changed on disk (a shared folder, a
// NAS): their changes merged into the project (engine: fichier_change, fusionner with projet,
// fusionner_finir), when the engine has these requests; asked once per change of the file
let surveillance = true;
const proposes = new Map();
async function surveillerDossiers(app) {
  if (!surveillance || document.querySelector('.fond-dialogue')) return;
  for (const p of app.projets()) {
    if (!fichierProjet(p)) continue;
    let r;
    try { r = await demander('fichier_change', { projet: p.id }, { silencieux: true }); } catch (e) {
      if (demandeAbsente(e)) { surveillance = false; return; }
      continue;
    }
    const cle = r && (r.change || r.changed) ? JSON.stringify([r.date || null, r.taille || null]) : null;
    if (!cle || proposes.get(p.id) === cle) continue;
    proposes.set(p.id, cle);
    if (await fenetreQuiAgit()) await fusionnerFichierChange(app, p.id, { change: r });
  }
}

// Someone else's changes to the project's file merged into the open project: asked first (unless
// before a save that met them), with the engine's choices per clash; true once merged
async function fusionnerFichierChange(app, pid, { avantEnregistrer = false, change = null } = {}) {
  if (!(await moteurFusion())) return false;
  const p = app.projet(pid);
  const chemin = fichierProjet(p) || (p && p.chemin_enregistrement);
  if (!chemin) return false;
  let r;
  try {
    r = await demander('fusionner', { projet: pid, leur: (change && change.chemin) || chemin, ...(change && change.base ? { base: change.base } : {}) });
  } catch (e) { app.statut(e.message, true); return false; }
  const conflits = [].concat(r.conflits || []);
  const choix = {};
  for (const x of conflits) choix[x.id] = 'mien';
  const texte = [el('p', {}, t('versions_dossier_change', { nom: sansExtension(p.nom) })),
    [].concat((r.apports && r.apports.leur) || []).length ? el('ul', {}, [].concat(r.apports.leur).map((x) => el('li', {}, x))) : null,
    conflits.length ? el('div', { class: 'liste-conflits' }, el('p', {}, t('versions_fusion_conflits')), conflits.map((x) => el('div', { class: 'conflit-fusion' },
      el('div', { class: 'conflit-titre' }, el('b', {}, x.titre || x.cle || '')),
      ['mien', 'leur'].map((cote) => el('label', { class: 'champ' },
        el('input', { type: 'radio', name: `fichier-${x.id}`, checked: cote === 'mien', onchange: () => { choix[x.id] = cote; } }),
        el('span', {}, el('b', {}, t(cote === 'mien' ? 'versions_fusion_choix_mien' : 'versions_fusion_choix_leur')), ` — ${x[cote] || ''}`)))))) : null];
  if (!avantEnregistrer || conflits.length) {
    const oui = await demanderChoix({ titre: t('versions_conflit_titre'), texte, boutons: [{ libelle: t('versions_dossier_fusionner'), principal: true, valeur: true }] });
    if (!oui) return false;
  }
  try { await app.changerDonnees('fusionner_finir', pid, { fusion: r.fusion, choix }); } catch { return false; }
  if (!avantEnregistrer) app.statut(t('versions_dossier_fusionne'));
  return true;
}

// ------------------------------------------------------------ Open from GitHub or GitLab

// A colleague's project: the repository's address (and a token if it is private), its projects,
// the folder to put the one chosen in; then it opens, shared with that repository
function dialogueDistant(app) {
  const c = couche();
  if (!c) { app.statut(t('versions_navigateur'), true); return; }
  const adresse = el('input', { type: 'url', placeholder: t('versions_adresse_exemple'), style: 'width:100%', spellcheck: 'false' });
  const jeton = el('input', { type: 'password', style: 'width:100%', autocomplete: 'off', spellcheck: 'false' });
  const ligneJeton = el('div', { class: 'grille-champs', hidden: true }, el('label', {}, t('versions_jeton')), el('div', {}, jeton, el('p', { class: 'note' }, t('versions_distant_jeton'))));
  const dossier = el('input', { style: 'width:100%', spellcheck: 'false' });
  const projets = el('div', { class: 'projets-distants' });
  const message = el('p', { class: 'message-connexion', 'aria-live': 'polite' });
  let fichiers = [];
  let choisi = null;
  const chercher = async () => {
    message.className = 'message-connexion doux';
    message.textContent = t('versions_verification');
    try {
      const hote = hoteAdresse(adresse.value);
      if (jeton.value.trim() && hote) { await c.jeton(hote, jeton.value.trim()); jeton.value = ''; }
      const r = await c.listerDistant(adresse.value);
      if (r.jeton) { ligneJeton.hidden = false; message.className = 'message-connexion erreur'; message.textContent = t('versions_distant_jeton'); jeton.focus(); return; }
      fichiers = [].concat(r.fichiers || []);
      choisi = fichiers.length === 1 ? fichiers[0] : null;
      message.textContent = fichiers.length ? '' : t('versions_distant_aucun');
      projets.replaceChildren(...fichiers.map((f) => el('label', { class: 'champ projet-distant' },
        el('input', { type: 'radio', name: 'projet-distant', checked: f === choisi, onchange: () => { choisi = f; } }), icone('file-text'), el('span', {}, f))));
    } catch (err) { message.className = 'message-connexion erreur'; message.textContent = err.message; }
  };
  const ouvrir = async () => {
    if (!choisi) { message.className = 'message-connexion erreur'; message.textContent = t('versions_distant_pas_projet'); return; }
    if (!dossier.value.trim()) { message.className = 'message-connexion erreur'; message.textContent = t('versions_distant_pas_dossier'); return; }
    message.className = 'message-connexion doux';
    message.textContent = t('versions_verification');
    try {
      const r = await c.ouvrirDistant(adresse.value, choisi, dossier.value.trim());
      d.fermer();
      await app.ouvrirChemin(r.chemin);
      app.statut(t('versions_distant_ouvert', { nom: nomDeChemin(r.chemin), hote: nomHote(hoteAdresse(adresse.value)) || hoteAdresse(adresse.value) }));
    } catch (err) { message.className = 'message-connexion erreur'; message.textContent = err.message; }
  };
  adresse.addEventListener('keydown', (ev) => { if (ev.key === 'Enter') chercher(); });
  const d = dialogue({ titre: t('versions_distant_titre'),
    contenu: el('div', { class: 'dialogue-connexion dialogue-distant' }, el('p', {}, t('versions_distant_texte')),
      el('div', { class: 'grille-champs' }, el('label', {}, t('versions_adresse')), el('div', { class: 'ligne' }, adresse,
        el('button', { onclick: chercher }, icone('search'), t('versions_distant_chercher')))),
      ligneJeton,
      el('div', { class: 'grille-champs' }, el('label', {}, t('versions_distant_projets')), projets,
        el('label', {}, t('versions_distant_dossier')), el('div', { class: 'ligne' }, dossier,
          el('button', { disabled: !bureau.choisirDossier, onclick: async () => { const x = await bureau.choisirDossier(); if (x) dossier.value = x; } }, icone('folder-open'), t('versions_distant_choisir_dossier')))),
      message),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('versions_distant_ouvrir'), principal: true, action: () => { ouvrir(); return false; } }] });
}
