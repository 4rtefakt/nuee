// Main window: menu bar, toolbar, project explorer, dockable panels and status bar.
// ?vue=<view>&modele=<id> shows a single view instead (a panel moved to its own window).
// All windows share the R server's state and keep selections in step over a BroadcastChannel.
import { t, changerLangue as changerLangueTexte, langueActuelle, lire, ecrire, deuxPoints } from './i18n.js';
import { demander, televerser, bureau, debutAttente } from './api.js';
import { VUES, seulementCourbes } from './panneaux.js';
import { el, icone, menuContextuel, ouvrirMenuSous, barreMenus, dialogue } from './ui.js';
import { creerCommandes, menus, menuExemples, barreOutils, commandeVue, ICONES_VUES, combinaison, afficherRaccourci } from './commandes.js';
import { rendreExplorateur } from './explorateur.js';
import { dialogueModele, dialogueAide, dialogueFormat, dialogueVariable, dialogueColonnes, dialogueResume } from './dialogues.js';
import { dialogueRaster } from './raster.js';
import { dialogueRapport, partiesModele, imageVue } from './rapport.js';
import { rendreConsole } from './console.js';
import { NOUVEAUTES, VERSION } from './nouveautes.js';
import { ECRITURE, filtresOuverture, acceptOuverture, filtresSpectres, acceptSpectres } from './formats.js';
import { ouvrirPalette } from './palette.js';
import { zip } from './zip.js';
// Analyses kept in modules of their own (vues_*.js): imported here, each adds its views to VUES
// (panneaux.js), their icons (commandes.js: ICONES_VUES), its views per model type
// (explorateur.js: VUES_TYPES) and its strings (i18n.js: ajouterTextes).
import './vues_ordination.js';
import './vues_indicatrices.js';
import './vues_rarefaction.js';
import './vues_communautes.js';
import './vues_classement.js';
import './vues_carte.js';
import './vues_par_reponse.js';
import './vues_trim.js';
import './vues_forets.js';
import './vues_distance.js';
import { installerCode } from './vues_code.js';
import { installerGreffons } from './vues_greffons.js';
import { installerDocuments } from './documents.js';
import { installerFigures } from './figures.js';
import { installerVersions } from './versions.js';
import { nomFichier } from './versions_texte.js';
import { dialogueQuelleAnalyse } from './quelle_analyse.js';
import { dialogueRemplacer, dialogueCategories, dialogueVariables, actionsVariable } from './atelier.js';
import { dialogueKobo, filtresKobo, deposerKobo } from './kobo.js';
import { installerEtapes } from './etapes.js';
import { installerReglages } from './reglages.js';
import { installerAide, prechargerGuide } from './aide.js';
import { messagesDock, ongletDock, donneesGraphe } from './accessibilite.js';

const params = new URLSearchParams(location.search);
const VUE_SEULE = params.get('vue');
// (the hidden page that draws a document's tables and plots in its language: demarrerHorsEcran)
const HORS_ECRAN = VUE_SEULE === 'hors_ecran';
const ID_FENETRE = params.get('fenetre') || 'principale';
const canal = new BroadcastChannel('nuee');
const DV = window['dockview-core'];

const etat = { projets: {}, modeles: {}, selection: {}, variables: {}, groupesHCA: {}, choixVIP: {}, locInitial: {}, panneaux: new Map(), noeud: null,
  replies: new Set(), occupe: 0, message: '', erreur: false, selectionLiee: lire('selection_liee') !== '0',
  // the last session's projects (main.js: their files, still there or not), reopened once; this session's, as told to main.js
  seance: { precedente: [], rouverte: false, notee: null } };
// The window's panels. `dock` holds the explorer and, in one central group of tabs, a page per
// model and the project views (data, statistics...). A model's page is a dock of its own
// (`pages`) holding that model's views: summary, scores and loadings first, then the others.
let dock = null;
const pages = new Map();           // model id -> dock of its page
const elementsPages = new Map();   // model id -> element of its page
const derniers = new Map();        // model id -> last view used in its page (other than the summary)
const dispositionsAttente = {};    // model id -> saved layout of its page, applied when the page opens
let restauration = false;  // while a saved layout replaces the panels
const EXTERNES = ['explorateur', 'accueil', 'console:', 'aide:'];  // the panels around the central tabs (aide: the guide, aide.js)
let dernierContenu = null;  // the central tab last in front (while typing in the console)
VUES.console = { rendre: rendreConsole, titre: () => t('console_r'), global: true };
let recents = [];
let fenetres = [];

const diffuser = (type, donnees = {}) => canal.postMessage({ type, ...donnees });

// A view's own window asks the main window it came from (`de`) for what needs that window's tabs
// (a project read again from its file, a model's page, a model's dialog), over the channel as
// Reattach does (pourPrincipale, there); resolves to its answer, or rejects with its error.
const attentesPrincipale = new Map();
function demanderPrincipale(action, donnees = {}, delai = 60000) {
  const jeton = `${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 8)}`;
  return new Promise((resolve, reject) => {
    const minuterie = setTimeout(() => { attentesPrincipale.delete(jeton); reject(new Error(t('fenetre_principale_muette'))); }, delai);
    attentesPrincipale.set(jeton, { resolve, reject, minuterie });
    diffuser('pour_principale', { vers: params.get('de') || 'principale', jeton, action, ...donnees });
  });
}

// (a model's results kept: garder(), below; the object's methods call it through this)
const garder_ = (res) => garder(res);

// ------------------------------------------------------------ application object

const app = {
  bureau,
  projets: () => Object.values(etat.projets),
  projet: (id) => etat.projets[id],
  modele: (id) => etat.modeles[id],
  modelesDe: (pid) => Object.values(etat.modeles).filter((m) => m.projet === pid),
  tousModeles: () => Object.values(etat.modeles),
  selection: (id) => etat.selection[id] || [],
  choixVIP: (id) => etat.choixVIP[id] || null,
  replies: etat.replies,
  recents: () => recents,
  fenetres: () => fenetres,
  noeudActif: () => etat.noeud,

  // Observation uids are those of the project, so a selection can apply to every model of
  // the project at once (Édition › Sélection commune aux modèles du projet). A project view
  // that shows the selection (Series: VUES[...].selection) keeps it under the project's id,
  // shared with the models in the same way.
  choisir(id, uids) {
    const u = [...new Set(uids)];
    const m = etat.modeles[id];
    const pid = m ? m.projet : etat.projets[id] ? id : null;
    const lies = etat.selectionLiee && pid ? [pid, ...app.modelesDe(pid).map((x) => x.id)] : [];
    for (const x of new Set([id, ...lies])) {
      etat.selection[x] = u;
      diffuser('selection', { modele: x, uids: u });
      rafraichirModele(x);
    }
    majEtat();
  },
  // Variables (term names) picked on one of a model's graphs, highlighted on all of them.
  variablesChoisies: (id) => etat.variables[id] || [],
  choisirVariables(id, termes) {
    etat.variables[id] = [...new Set(termes)];
    diffuser('variables', { modele: id, termes: etat.variables[id] });
    rafraichirModele(id);
    majEtat();
  },
  effacerSelection(id) {
    if (app.selection(id).length) app.choisir(id, []);
    if (app.variablesChoisies(id).length) app.choisirVariables(id, []);
  },
  selectionLiee: () => etat.selectionLiee,
  basculerSelectionLiee(propager = true) {
    etat.selectionLiee = !etat.selectionLiee;
    ecrire('selection_liee', etat.selectionLiee ? '1' : '0');
    if (propager) diffuser('selection_liee', { lie: etat.selectionLiee });
  },

  modeleActif() {
    if (VUE_SEULE) return params.get('modele') || null;
    const c = panneauCourant();
    const e = c && etat.panneaux.get(c.id);
    if (e && e.params.modele && etat.modeles[e.params.modele]) return e.params.modele;
    const p = dock && dock.activePanel;
    if (estPage(p) && etat.modeles[p.id.slice(5)]) return p.id.slice(5);
    const n = /\/m\/([^/]+)/.exec(etat.noeud || '');
    // (a view of a project in front, its data, a figure or a document: the model chosen in the
    // explorer only if it is of that project)
    if (n && etat.modeles[n[1]] && e && etat.projets[e.params.projet] && etat.modeles[n[1]].projet !== e.params.projet) return null;
    return n && etat.modeles[n[1]] ? n[1] : null;
  },

  // The project the menus act on (Save, New model, Which analysis?, New document...): the active
  // model's, else that of the view in front, else the one chosen in the explorer
  projetActif() {
    const m = app.modeleActif();
    if (m) return etat.modeles[m].projet;
    const c = panneauCourant();
    const e = c && etat.panneaux.get(c.id);
    if (e && etat.projets[e.params.projet]) return e.params.projet;
    const n = (etat.noeud || '').split('/')[0];
    if (etat.projets[n]) return n;
    const tous = Object.keys(etat.projets);
    return tous.length === 1 ? tous[0] : null;
  },

  panneauActif: () => panneauCourant(),

  grapheActif() {
    const p = VUE_SEULE ? { id: 'seul' } : panneauCourant();
    const e = p && etat.panneaux.get(p.id);
    return e && e.graphe && e.graphe.isConnected ? e.graphe : null;
  },

  choisirNoeud(cle) {
    etat.noeud = cle;
    const n = /\/m\/([^/]+)$/.exec(cle);  // a model chosen in the explorer: its page comes to the front
    const page = n && dock && dock.getPanel(`page:${n[1]}`);
    if (page) page.api.setActive();
    // (else the explorer comes to the front, as when a click brings the focus into it: the focus
    // may already be there, left on the node that opened the view in front, and dockview then sees
    // nothing; the menus act on what was chosen, not on that view's project)
    else {
      const ex = dock && dock.getPanel('explorateur');
      if (ex && !ex.api.isActive) ex.api.setActive();
    }
    app.rafraichirExplorateur();
    majEtat();
  },

  rafraichirExplorateur() {
    for (const id of ['explorateur', 'accueil']) {
      const e = etat.panneaux.get(id);
      if (e) rendrePanneau(e);
    }
  },

  // ---- files
  async ouvrir() {
    if (bureau) {
      const chemin = await bureau.choisirFichier(filtresOuverture());
      if (chemin) await app.ouvrirChemin(chemin);
      return;
    }
    const f = await choisirFichierNavigateur(acceptOuverture());
    if (!f) return;
    const res = await choisirTable(await occupe(() => televerser(f)));
    if (res) await apresOuverture(res);
  },

  // A table copied in a spreadsheet or a statistics program (cells separated by tabs)
  // becomes a new project, to save like any other.
  async coller() {
    let texte = '';
    try { texte = bureau ? await bureau.lirePressePapiers() : await navigator.clipboard.readText(); } catch { texte = ''; }
    if (!texte || !texte.trim()) { statut(t('presse_papiers_vide'), true); return; }
    etat.collages = (etat.collages || 0) + 1;
    const res = await occupe(() => demander('coller', { texte, nom: `${t('tableau_colle')} ${etat.collages}` }));
    await apresOuverture(res);
    if ([].concat(res.note || []).length) statut([].concat(res.note).join(' ; '), true);  // cells read as missing
  },

  // Spectra from one or more files (JCAMP-DX, SPC, text) made into one project, with an
  // optional table of information on the samples, joined by file name (fichiers_spectres.R).
  importerSpectres() {
    const st = { fichiers: [], infos: null };
    // desktop: the paths; browser: the files are sent to the engine first
    const choisir = async (spectres) => {
      if (bureau) return spectres ? bureau.choisirFichiers(filtresSpectres()) : [await bureau.choisirFichier(filtresOuverture(true))].filter(Boolean);
      const fichiers = await choisirFichiersNavigateur(spectres ? acceptSpectres() : acceptOuverture(true), spectres);
      return Promise.all(fichiers.map(async (f) => (await (await fetch('/api/deposer?nom=' + encodeURIComponent(f.name), { method: 'POST', body: f })).json()).chemin));
    };
    const nom = (c) => c.split(/[\\/]/).pop();
    const contenu = el('div', { class: 'import-spectres' });
    const erreur = el('p', { class: 'erreur' });
    const rendre = () => contenu.replaceChildren(el('p', {}, t('importer_spectres_texte')),
      el('div', { class: 'grille-champs' },
        el('label', {}, t('fichiers_spectres')), el('div', {},
          el('button', { onclick: async () => { const f = await choisir(true); if (f.length) { st.fichiers = f; rendre(); } } }, icone('folder-open'), t('choisir_fichiers')),
          el('p', { class: 'note' }, st.fichiers.length
            ? t('n_fichiers', { n: st.fichiers.length, noms: st.fichiers.slice(0, 3).map(nom).join(', ') + (st.fichiers.length > 3 ? '…' : '') })
            : t('aucun_fichier'))),
        el('label', {}, `${t('infos_echantillons')} (${t('facultatif')})`), el('div', {},
          el('div', { class: 'ligne' },
            el('button', { onclick: async () => { const [f] = await choisir(false); if (f) { st.infos = f; rendre(); } } }, icone('file-spreadsheet'), t('choisir_tableau')),
            st.infos ? el('span', {}, nom(st.infos)) : null,
            st.infos ? el('button', { onclick: () => { st.infos = null; rendre(); } }, t('retirer')) : null),
          el('p', { class: 'note' }, t('infos_note')))),
      erreur);
    rendre();
    dialogue({ titre: t('importer_spectres_titre'), contenu, boutons: [{ libelle: t('annuler'), action: () => true },
      { libelle: t('importer'), principal: true, action: () => {
        if (!st.fichiers.length) { erreur.textContent = t('aucun_fichier'); return false; }
        const nomProjet = st.fichiers.length === 1 ? nom(st.fichiers[0]) : t('spectres_importes', { n: st.fichiers.length });
        occupe(() => demander('importer_spectres', { chemins: st.fichiers, infos: st.infos, nom: nomProjet })).then(async (res) => {
          await apresOuverture(res);
          if ([].concat(res.note || []).length) statut([].concat(res.note).join(' ; '));
        }, () => {});
        return true;
      } }] });
  },

  // File › Import from KoboToolbox or ODK: an export (Excel, CSV, ODK Central's .zip), then the
  // import's dialog (kobo.js); in the browser the file is sent to the engine first
  async importerKobo() {
    const chemin = bureau ? await bureau.choisirFichier(filtresKobo()) : await deposerKobo();
    if (!chemin) return;
    if (bureau) { await app.ouvrirChemin(chemin, { collecte: true }); return; }
    const res = await choisirTable(await occupe(() => demander('ouvrir', { chemin, collecte: true })));
    if (res) await apresOuverture(res);
  },

  // Help › Example projects (made-up or simulated data), opened as new projects that Save puts where
  // asked: the apples, with their first model open, scores coloured by variety; the skylark counts
  // and transects of the monitoring tutorial, then the trout of the BACI tutorial, with their models
  // fitted and their views open as their sessions say (exemple/creer_exemples_ecologie.js), the
  // counts' TRIM in front, and their tutorial in the status bar while they are in front (aide.js)
  async ouvrirExemple(cle = 'pommes') {
    if (cle === 'pommes') {
      const res = await occupe(() => demander('exemple', { nom: 'pommes' }));
      await apresOuverture(res);
      if (!res.modeles.length) return;
      const id = await app.calculer(res.id, res.modeles[0].numero);
      const variete = res.variables.find((v) => v.nom === 'variety' || v.nom === 'variete');  // (orchards.json, vergers.json)
      if (variete) app.ouvrirVue('scores', id, { couleur: 'v' + variete.uid });
      return;
    }
    const { projets, tutoriel } = { alouettes: { projets: ['alouettes_comptages', 'alouettes_transects'], tutoriel: 'suivi' }, baci: { projets: ['baci'], tutoriel: 'baci' } }[cle];
    const ids = [];
    for (const nom of projets) {
      const res = await occupe(() => demander('exemple', { nom }));
      if (app.noterTutoriel) app.noterTutoriel(res.id, tutoriel);
      await apresOuverture(res);
      ids.push(res.id);
    }
    const premier = app.modelesDe(ids[0])[0];
    if (ids.length > 1 && premier) app.ouvrirModele(premier.id);
  },

  // collecte: File › Import from KoboToolbox or ODK (kobo.js), the import asked for (another file is refused)
  async ouvrirChemin(chemin, { collecte = false } = {}) {
    // already open (a recent file, a file dropped or double-clicked again): shown, not opened
    // twice, which would make two copies that both save to the same file
    const cle = (c) => (c ? (/^Win/.test(navigator.platform) ? c.replace(/\\/g, '/').toLowerCase() : c) : null);
    const ouvert = !collecte && Object.values(etat.projets).find((p) => cle(p.chemin_enregistrement) === cle(chemin) || (!p.table && cle(p.chemin) === cle(chemin)));
    if (ouvert) {
      etat.noeud = ouvert.id;
      app.rafraichirExplorateur();
      majEtat();
      statut(t('deja_ouvert', { nom: ouvert.nom }));
      return;
    }
    const res = await choisirTable(await occupe(() => demander('ouvrir', { chemin, collecte: collecte || undefined })));
    if (!res) return;
    await apresOuverture(res);
    if ([].concat(res.note || []).length) statut([].concat(res.note).join(' ; '), true);  // cells read as missing, a project from a newer Nuée
    if (bureau) recents = await bureau.ajouterRecent(chemin);
    else {
      recents = [chemin, ...recents.filter((r) => r !== chemin)].slice(0, 10);
      ecrire('recents', JSON.stringify(recents));
    }
  },

  // true once saved; false if the user cancelled the choice of a file
  async enregistrer(pid, sous) {
    if (!etat.projets[pid]) { statut(t('choisir_projet'), true); return false; }
    // (an old version opened read-only is never saved over the temporary file it was read from:
    // versions.js offers Save as… or Restore…)
    if (!sous && etat.projets[pid].lecture_seule && app.refuserEnregistrement) return app.refuserEnregistrement(pid);
    if (app.viderDocuments) await app.viderDocuments(pid);  // (a document's text still being typed goes in first)
    if (app.viderFigures) await app.viderFigures(pid);  // (and a figure's last change)
    const p = etat.projets[pid];
    let chemin = !sous && p.chemin_enregistrement ? p.chemin_enregistrement : null;
    if (bureau && !chemin) {
      // (an old version's name has its date, whose dot and colon a file's name can't take as they are)
      const nom = p.lecture_seule ? nomFichier(p.nom) : p.nom.replace(/\.[^.]+$/, '');
      chemin = await bureau.enregistrerSous(`${nom}.nuee`, [{ name: t('format_projet'), extensions: ['nuee'] }]);
      if (!chemin) return false;
    }
    const session = VUE_SEULE ? undefined : JSON.stringify(collecterSession(pid));
    // (who saves it, written in the file: a colleague who has it open sees who changed it)
    const auteur = app.auteurVersions ? await app.auteurVersions() : null;
    const q = { projet: pid, chemin, session, version: VERSION, ...(auteur ? { auteur } : {}) };
    const sauver = (x) => occupe(() => demander('enregistrer', x).catch((e) => { if (e.reponse && e.reponse.conflit) return e.reponse; throw e; }));
    let r = await sauver(q);
    let sansFusion = false;
    while (r.conflit) {  // changed on disk since it was read: their changes merged in, overwrite, save elsewhere, or leave it
      // (merged: the engine's merge by meaning when it kept the file as it was read, versions.js;
      // otherwise, as after a restart, the question alone)
      let choix = r.fusion && !sansFusion && app.fusionnerFichier ? await app.fusionnerFichier(pid) : 'repli';
      if (choix === 'repli') {
        sansFusion = true;
        choix = await new Promise((resolve) => dialogue({ titre: t('conflit_titre'),
          contenu: el('p', {}, t('conflit_texte', { nom: chemin.split(/[\\/]/).pop(), date: new Date(r.date * 1000).toLocaleString(langueActuelle()) })),
          boutons: [{ libelle: t('annuler'), action: () => resolve(null) },
            { libelle: t('ecraser'), action: () => resolve('ecraser') },
            { libelle: t('enregistrer_sous_bouton'), principal: true, action: () => resolve('sous') }],
          surFermeture: () => resolve(null) }));
      }
      if (choix === 'sous') return app.enregistrer(pid, true);
      if (choix !== 'ecraser' && choix !== 'fusion') return false;
      // (merged: saved as it is now, asked again if the file changed once more meanwhile)
      r = await sauver({ ...q, session: VUE_SEULE ? undefined : JSON.stringify(collecterSession(pid)), ...(choix === 'ecraser' ? { ecraser: true } : {}) });
    }
    if (!bureau && !chemin) telecharger(r.chemin);
    statut(`${t('enregistre')} ${r.chemin}`);
    // (the project file, with its models, in File › Open recent and on the start page, where only the
    // table it came from was)
    if (bureau) recents = await bureau.ajouterRecent(r.chemin);
    await rafraichirProjet(pid);
    if (p.lecture_seule) retitrer(pid);  // (an old version saved elsewhere: a project like any other, its tabs unmarked)
    // (its versions: one when the last is an hour old; after Save as, the former file's history
    // copied first, versions.js. Not that of an old version's temporary file.)
    if (app.apresEnregistrementVersions) {
      app.apresEnregistrementVersions(pid, !p.lecture_seule && p.chemin_enregistrement && p.chemin_enregistrement !== r.chemin ? p.chemin_enregistrement : null);
    }
    sauvegarderSecours();  // its recovery copy, now older than the file, leaves the index
    diffuser('modeles');
    return true;
  },
  // every project with unsaved changes (closing with "Save and close")
  async enregistrerTout() {
    for (const p of Object.values(etat.projets).filter((x) => x.modifie)) {
      if (!(await app.enregistrer(p.id, false).catch(() => false))) return false;
    }
    return true;
  },

  // A model's results (scores, loadings, VIP...), a project's data, a prediction: in the
  // format chosen in the export dialog.
  async exporter(id) {
    const m = etat.modeles[id];
    if (!m) return;
    const choix = await choisirExport(t('exporter_resultats_titre', { nom: m.nom }), nomFichierModele(m), true, !!(etat.projets[m.projet] || {}).coordonnees);
    if (choix) apresExport(await occupe(() => demander('exporter', { modele: id, ...choix })));
  },
  // the coefficients side by side shown in Compare models (coefficientsActifs): the table as
  // copied, and each model's coefficients in numbers (moteur/R/coefficients_modeles.R)
  async exporterCoefficients(q) {
    const m = etat.modeles[[].concat(q.modeles || [])[0]];  // (the regressions of one project)
    const choix = await choisirExport(t('exporter_coefficients_titre'), m ? sansCaracteresInterdits(`${sansExtension((etat.projets[m.projet] || {}).nom)} - coefficients`) : 'coefficients', true);
    if (choix) apresExport(await occupe(() => demander('exporter_coefficients', { ...q, ...choix })));
  },
  async exporterDonnees(pid) {
    const p = etat.projets[pid];
    if (!p) { statut(t('choisir_projet'), true); return; }
    const choix = await choisirExport(t('exporter_donnees_titre', { nom: p.nom }), p.nom.replace(/\.[^.]+$/, ''), false, !!p.coordonnees);
    if (choix) apresExport(await occupe(() => demander('exporter_donnees', { projet: pid, ...choix })));
  },

  async fermerProjet(pid) {
    if (!etat.projets[pid]) return;
    if (app.viderDocuments) await app.viderDocuments(pid);  // (what was just typed in its documents makes it modified)
    if (app.viderFigures) await app.viderFigures(pid);
    const p = etat.projets[pid];
    if (p.modifie && !window.confirm(t('confirmer_fermer_projet', { nom: p.nom }))) return;
    await demander('fermer_projet', { projet: pid });
    oublierProjet(pid);
    diffuser('modeles');
    if (app.fermetureVersions) app.fermetureVersions(p);  // (its last version, if its file changed since: versions.js)
  },

  // A project read again from its file, which changed under it (versions.js: a version restored,
  // versions received), then reopened with the views it had, as a saved session brings them back:
  // the models it still has, fitted with the number of components its file's session gives, and
  // their views; without fitted models, the file's own session. What this window still holds is
  // sent first; then, unless the caller let them go (perdre), unsaved changes, as the engine has
  // them from every window, keep the project as it is: null then. The other windows follow it to
  // its new id (projet_recharge: their documents and figures, a view in a window of its own). In a
  // view's own window, its main window does it. Resolves to the new project's id.
  async rechargerProjet(pid, { perdre = false } = {}) {
    if (VUE_SEULE) return demanderPrincipale('recharger', { projet: pid, perdre }, 600000);
    const p = etat.projets[pid];
    if (!p || !p.chemin_enregistrement) return null;
    if (app.viderDocuments) await app.viderDocuments(pid);
    if (app.viderFigures) await app.viderFigures(pid);
    if (!perdre) {
      const r = await demander('projet', { projet: pid });
      projetRecu(r);
      if (r.modifie) { majEtat(); return null; }
    }
    const session = dock ? collecterSession(pid) : null;
    const anciens = app.modelesDe(pid).filter((m) => m.numero !== null && m.numero !== undefined).map((m) => ({ id: m.id, numero: m.numero }));
    // (without fitted models, a session brings nothing back, restaurerSession: the file's own, then
    // the project's views open again one by one)
    const avecModeles = session && session.modeles.length ? session : null;
    await demander('fermer_projet', { projet: pid });
    oublierProjet(pid);
    const res = await occupe(() => demander('ouvrir', { chemin: p.chemin_enregistrement }));
    // (the other windows' documents and figures to the new id at once, before 'modeles' closes
    // their panels of the project gone: what is typed there meanwhile goes to it)
    diffuser('projet_recharge', { ancien: pid, nouveau: res.id });
    if (avecModeles && res.session) {
      try {
        const fichier = JSON.parse((await demander('session', { projet: res.id })).session);
        const A = new Map([].concat(fichier.modeles || []).map((m) => [m.numero, m.A]));
        avecModeles.modeles = avecModeles.modeles.map((m) => (A.has(m.numero) ? { ...m, A: A.get(m.numero) } : m));
      } catch { /* the session's own */ }
    }
    await apresOuverture(res, avecModeles);
    if (session && !avecModeles) for (const v of session.vues) if (!v.modele && VUES[v.vue]) app.ouvrirVue(v.vue, null, v.loc, res.id);
    // (then a model's view in a window of its own: each fit before by its model's new fit, by their numbers)
    const nouveaux = new Map(modelesSeance(res.id).map((m) => [m.numero, m.id]));
    const modeles = Object.fromEntries(anciens.filter((m) => nouveaux.has(m.numero)).map((m) => [m.id, nouveaux.get(m.numero)]));
    diffuser('projet_recharge', { ancien: pid, nouveau: res.id, modeles });
    diffuser('modeles');
    return res.id;
  },
  // An old version of a project (versions.js), written to a temporary file by the main process:
  // opened read-only under its name (the engine keeps `lecture_seule`: its source, oid and date),
  // with its views as its session saved them; without one, its data, so that it shows.
  async ouvrirLectureSeule(fichier, nom, lecture) {
    const res = await occupe(() => demander('ouvrir', { chemin: fichier, nom, lecture_seule: lecture }));
    await apresOuverture(res);
    if (!res.session) app.ouvrirVue('donnees', null, null, res.id);
    return res.id;
  },
  // A project open already, chosen in the explorer.
  montrerProjet(pid) {
    if (!etat.projets[pid]) return;
    etat.noeud = pid;
    app.rafraichirExplorateur();
    majEtat();
  },
  // The status bar drawn again (versions.js, its indicator once the file's state is known;
  // etapes.js, the notice of a data file that changed).
  majStatut: () => majEtat(),
  // A project's summary read again from the engine (versions.js: its unsaved state, whichever
  // window made the changes, before a version is written to its file)
  relireProjet: async (pid) => { await rafraichirProjet(pid); majEtat(); },

  quitter: () => bureau && bureau.quitter(),

  // The last session's projects (File › Settings… › At start-up, or the start page's Reopen last
  // session): the files of the saved projects open when Nuée last closed, opened again with their
  // sessions, once; a file moved or deleted since, or one that doesn't open, is named.
  async rouvrirSeance() {
    const s = etat.seance;
    if (s.rouverte || !s.precedente.length) return;
    s.rouverte = true;
    app.rafraichirExplorateur();  // (the start page's button goes)
    const absents = [];
    const illisibles = [];
    let n = 0;
    for (const { chemin, existe } of s.precedente) {
      if (!existe) { absents.push(chemin); continue; }
      try { await app.ouvrirChemin(chemin); n += 1; } catch { illisibles.push(chemin); }
    }
    const noms = (l) => l.map((c) => c.split(/[\\/]/).pop()).join(', ');
    statut([t(n === 1 ? 'seance_rouverte_un' : 'seance_rouverte', { n }), absents.length ? t('seance_absents', { noms: noms(absents) }) : '',
      illisibles.length ? t('seance_illisibles', { noms: noms(illisibles) }) : ''].filter(Boolean).join(' '), illisibles.length > 0);
  },

  // ---- models
  async calculer(pid, numero) {
    const res = await occupe(() => demander('ajuster', { projet: pid, numero }));
    await garder(res);
    vuesParDefaut(res.id);
    return res.id;
  },
  // A model of the project computed without opening its views (a document that refers to it,
  // documents.js), as a saved session brings its models back (restaurerSession); A: its number of
  // components, if not the definition's. A project the interface doesn't show (an old version
  // compared with the project now, opened in the engine only: changements.js) keeps it there only.
  async calculerDiscret(pid, numero, A) {
    // (an R-code model whose code isn't approved here: it waits for Run it, in the bar over its project)
    // (a plugin's model likewise: its plugin isn't approved here, vues_greffons.js)
    if (codeEnAttente(pid, numero)) {
      const d = [].concat(etat.projets[pid].modeles).find((x) => x.numero === numero) || {};
      throw new Error(t(d.type === 'PLUGIN' ? 'greffon_attente' : 'code_attente', { nom: d.nom }));
    }
    const res = await demander('ajuster', { projet: pid, numero, A });
    if (!etat.projets[pid]) return res.id;
    etat.modeles[res.id] = res;
    await rafraichirProjet(pid);
    diffuser('modeles');
    majEtat();
    return res.id;
  },
  // A model of the project fitted again on its data as they are now (Edit › Update the data:
  // etapes.js), its definition unchanged. Not fitted in this session: fitted without opening its
  // views (calculerDiscret). Fitted there: the new fit takes the place of those fits, which leave
  // the session (their results were those of the data before): their page shows it, with its
  // layout and its views' settings, and the selection and highlighted variables go with it. The
  // new fit's id.
  async ajusterDeNouveau(pid, numero) {
    const anciens = app.modelesDe(pid).filter((m) => m.numero === numero);
    if (!anciens.length) return app.calculerDiscret(pid, numero);
    const rang = (id) => Number(String(id).replace(/\D/g, '')) || 0;
    const dernier = anciens.reduce((a, b) => (rang(b.id) > rang(a.id) ? b : a));
    // (with the number of components of its latest fit: a .usp model's changed in the session stays)
    const res = await demander('ajuster', { projet: pid, numero, A: dernier.A || undefined });
    await remplacerAjustements(pid, anciens, res);
    await rafraichirProjet(pid);
    diffuser('modeles');
    majEtat();
    return res.id;
  },
  // A project's summary after a change that leaves its data and models as they are (a document
  // written): the explorer and the status bar, no panel drawn again.
  majProjet(res) {
    projetRecu(res);
    app.rafraichirExplorateur();
    majEtat();
  },
  // The title of an open panel's tab (a document renamed).
  titrePanneau(cle, titre) {
    const e = etat.panneaux.get(cle);
    if (e && e.api) e.api.setTitle(titre);
  },
  // (type: the one New model starts with, Find a command's "New model: TRIM…", palette.js)
  nouveauModele(pid, type = null) {
    const p = etat.projets[pid || app.projetActif()];
    if (!p) return statut(t('choisir_projet'), true);
    dialogueModele(app, p, null, undefined, null, type);
  },
  // Model › Which analysis?: a few questions, then New model already filled in, or the right view
  quelleAnalyse(pid) {
    const p = etat.projets[pid || app.projetActif()];
    if (!p) return statut(t('choisir_projet'), true);
    dialogueQuelleAnalyse(app, p);
  },
  modifierModele(id) {
    const m = etat.modeles[id];
    if (!m) return;
    // (in a view's own window: the dialog in its main window, where the new fit takes the old one's
    // page, and its views follow it here: ajustement_remplace)
    if (VUE_SEULE) { demanderPrincipale('modifierModele', { modele: id }).catch((e) => statut(e.message, true)); return; }
    // (an R-code model: its code edited, then fitted in its place under its name: vues_code.js)
    if (m.analyse && m.type === 'CODE') dialogueModele(app, etat.projets[m.projet], { ...m.reglages, remplace: m.id }, `${t('code_modifier')} — ${m.nom}`);
    // (a plugin's model: the dialog its plugin's manifest describes, that of the model's own version: vues_greffons.js)
    else if (m.analyse && m.type === 'PLUGIN' && app.modifierGreffon) app.modifierGreffon(m);
    else dialogueModele(app, etat.projets[m.projet], m.reglages, t('modifier_modele'));
  },
  // a new model from a model's settings, changed (the Profiles view's wavelength ranges)
  nouveauModeleDepuis(id, reglages) {
    const m = etat.modeles[id];
    if (!m) return;
    if (VUE_SEULE) { demanderPrincipale('nouveauModeleDepuis', { modele: id, reglages }).catch((e) => statut(e.message, true)); return; }
    dialogueModele(app, etat.projets[m.projet], reglages);
  },
  // (the new model's id, for what follows it: Which analysis?'s one model per response).
  // remplace: the fit whose place it takes, its page and views (an R-code model's code edited under
  // the same name: vues_code.js), as Update the data does (ajusterDeNouveau).
  async ajusterDefinition(q, remplace = null) {
    if (q.type === 'CLASSES') {  // one PCA per class, then their Coomans plot
      const r = await occupe(() => demander('modeles_classe', q));
      const liste = [].concat(r.modeles);
      for (const res of liste) await garder(res);
      vuesParDefaut(liste[0].id);
      app.ouvrirVue('coomans', liste[0].id);
      return liste[0].id;
    }
    const res = await occupe(() => demander('ajuster', q));
    if (remplace && etat.modeles[remplace]) {
      await remplacerAjustement(remplace, res);
      return res.id;
    }
    await garder(res);
    vuesParDefaut(res.id);
    return res.id;
  },
  async reajuster(id, q) {
    const res = await occupe(() => demander('reajuster', { modele: id, ...q }));
    if (etat.variables[id]) etat.variables[res.id] = etat.variables[id];  // the refit keeps the highlighted variables
    await garder(res);
    vuesParDefaut(res.id);
  },
  // Statistical tests › BACI › Fit the mixed model: its two 0/1 variables (new ones are undone by
  // Ctrl+Z, as a New variable) and the model, which joins the project; the views of the project's
  // data are drawn again (the BACI view then shows the model's effect), and the model's views open
  async ajusterBaci(q) {
    const r = await occupe(() => demander('baci_modele', q));
    await garder(r.modele);
    projetChange(etat.projets[r.modele.projet]);
    vuesParDefaut(r.modele.id);
    const nouvelles = [].concat(r.nouvelles || []);
    const entre = (n) => (langueActuelle() === 'fr' ? `« ${n} »` : `“${n}”`);
    statut(t(nouvelles.length ? 'baci_modele_variables' : 'baci_modele_ajoute', { nom: r.modele.nom, variables: nouvelles.map(entre).join(', ') }));
  },
  // Removing a computed model asks first. Its definition stays in the project (it can be
  // computed again), unless it was made in the application and the box is ticked.
  supprimerModele(id) {
    const m = etat.modeles[id];
    if (!m) return;
    const aussi = el('input', { type: 'checkbox' });
    dialogue({ titre: t('retirer_modele'),
      contenu: el('div', {}, el('p', {}, t('retirer_modele_question', { nom: m.nom })),
        m.app ? el('label', { class: 'champ' }, aussi, t('retirer_definition')) : el('p', { class: 'note' }, t('definition_reste'))),
      boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('retirer'), principal: true, action: () => {
        demander('supprimer_modele', { modele: id, definition: !!m.app && aussi.checked }).then((r) => {
          oublierModele(id);
          if (r.projet) projetChange(r.projet);
          diffuser('modeles');
        }).catch((e) => statut(`${t('erreur')}${deuxPoints()}${e.message}`, true));
        return true;
      } }] });
  },
  async annulerAction(pid) {
    // (what is being typed in a document goes in first: Undo may give its references back, a variable renamed)
    if (app.viderDocuments) await app.viderDocuments(pid);
    const r = await occupe(() => demander('annuler', { projet: pid }));
    projetChange(r.projet);
    app.relireDocuments(pid, [].concat(r.documents || []));
    statut(t('annule', { quoi: r.etiquette }));
    if (app.verifierSource) app.verifierSource(pid);  // (an update undone: the source read before, which may have changed since)
    // (an update of the data or a step removed, undone: the models fitted again on the data back, etapes.js)
    if (app.apresAnnulation) await app.apresAnnulation(pid, r);
  },
  // Edit › Redo (Ctrl+Y, Ctrl+Shift+Z): the change Undo took back, made again as it was (its data,
  // source and steps); an update of the data redone fits the models again, as its undo did
  async retablirAction(pid) {
    if (app.viderDocuments) await app.viderDocuments(pid);
    const r = await occupe(() => demander('retablir', { projet: pid }));
    projetChange(r.projet);
    app.relireDocuments(pid, [].concat(r.documents || []));
    statut(t('retabli', { quoi: r.etiquette }));
    if (app.verifierSource) app.verifierSource(pid);
    if (app.apresAnnulation) await app.apresAnnulation(pid, r, true);
  },
  async noterModele(id, note) {
    let r;
    try { r = await demander('note_modele', { modele: id, note }); } catch (e) { statut(`${t('erreur')}${deuxPoints()}${e.message}`, true); return; }
    if (etat.modeles[id]) etat.modeles[id].note = r.note;
    projetRecu(r.projet);
    app.rafraichirExplorateur();
    majEtat();
    diffuser('modeles');
  },
  // A new R engine (after it stopped, or on request): the projects reopen with the session they
  // had: from their recovery copy when they had unsaved changes, otherwise from the file they
  // are saved to (or, never saved, the file they came from). Those with neither are named.
  // copies (another R chosen in File › Settings…, the engine answering): recovery copies made
  // first, so that the changes since the last ones reopen too. Afterwards, if the R chosen didn't
  // start and Nuée's own took over, the window says so (reglages.js).
  async redemarrerMoteur({ copies = false } = {}) {
    if (!bureau) { statut(t('moteur_navigateur'), true); return; }
    if (copies) {
      statut(t('moteur_redemarrage'));
      await sauvegarderSecours();
    }
    const aRouvrir = [];
    const perdus = [];
    for (const p of Object.values(etat.projets)) {
      // (this launch's own copy of the project: those of an earlier stop, also in the index,
      // had projects numbered P1, P2... too)
      const s = p.modifie && derniersSecours.get(p.id);
      const source = p.chemin_enregistrement || p.chemin;
      const session = dock ? collecterSession(p.id) : null;
      // (an old version opened read-only stays one, under its name: versions.js)
      const lecture = p.lecture_seule ? { nom: p.nom, lecture_seule: p.lecture_seule } : {};
      if (s) aRouvrir.push({ nom: p.nom, session, copie: s, q: { chemin: s.fichier, secours: true, origine: s.origine || null, nom: p.nom, lu: s.lu || null, ...lecture } });
      // (an import from KoboToolbox or ODK is redone with its settings: kobo.js)
      // (opened as is, it is opened as is again: an export would otherwise come back as an import to make)
      else if (source) aRouvrir.push({ nom: p.nom, session, q: p.chemin_enregistrement ? { chemin: source, ...lecture } : { chemin: source, table: p.table || undefined, kobo: p.kobo || undefined, brut: p.brut || undefined } });
      else perdus.push(p.nom);
    }
    statut(t('moteur_redemarrage'));
    const erreur = await bureau.redemarrerMoteur();
    moteurArrete = false;
    if (erreur) { statut(`${t('erreur')}${deuxPoints()}${erreur}`, true); return; }
    if (dock) for (const p of [...dock.panels]) if (!EXTERNES.includes(p.id)) p.api.close();
    for (const k of ['projets', 'modeles', 'selection', 'variables', 'choixVIP', 'groupesHCA']) etat[k] = {};
    app.rafraichirExplorateur();
    derniersSecours.clear();  // the new engine numbers its projects from P1 again,
    moteurs += 1;             // and its copies get names of their own (moteurs)
    let n = 0;
    for (const r of aRouvrir) {
      try {
        await apresOuverture(await occupe(() => demander('ouvrir', r.q)), r.session);
        n += 1;
      } catch {
        perdus.push(r.nom);
        if (r.copie) secoursEnAttente.push(r.copie);  // its recovery copy stays, for another try
      }
    }
    diffuser('modeles');
    statut(t('moteur_redemarre', { n }) + (perdus.length ? ` ${t('non_rouverts', { noms: perdus.join(', ') })}` : ''), perdus.length > 0);
    if (n) sauvegarderSecours();  // the reopened projects get copies of their own at once
    app.signalerRepliR();
  },
  confirmerRedemarrage() {
    dialogue({ titre: t('redemarrer_moteur'), contenu: el('p', {}, t('redemarrer_confirmation')),
      boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('redemarrer'), principal: true, action: () => { app.redemarrerMoteur(); return true; } }] });
  },
  quoiDeNeuf: (tout) => quoiDeNeuf(tout),
  // One-click update (installed copy): what it does, then the download, checked against its
  // SHA-256, and the installation; Nuée closes and starts again (main.js).
  mettreAJour() {
    const v = etat.nouvelleVersion;
    if (!v || !v.installateur || !bureau || !bureau.mettreAJour) return;
    dialogue({ titre: t('maj_titre', { v: v.version }), contenu: el('p', {}, t('maj_texte', { taille: Math.round(v.installateur.taille / 1e6) })),
      boutons: [{ libelle: t('maj_nouveautes'), action: () => { window.open(v.page); return false; } },
        { libelle: t('plus_tard'), action: () => true },
        { libelle: t('mettre_a_jour'), principal: true, action: () => {
          statut(t('maj_telechargement', { v: v.version, p: 0 }));
          bureau.mettreAJour().then((r) => { if (r && r.erreur) statut(t(`maj_${r.erreur}`, { d: r.detail || '' }), r.erreur !== 'annulee'); },
            (e) => statut(t('maj_reseau', { d: e.message }), true));
          return true;
        } }] });
  },
  // Aide › Chercher une mise à jour: the start-up check, now (installed copy only)
  async chercherMiseAJour() {
    statut(t('maj_recherche'));
    const v = await bureau.nouvelleVersion(true);
    if (v && v.erreur) { statut(t('maj_injoignable', { d: v.erreur }), true); return; }
    statut('');
    etat.nouvelleVersion = v;
    majEtat();
    if (v && v.installateur) { app.mettreAJour(); return; }
    dialogue({ titre: t('chercher_maj'), contenu: el('p', {}, v ? t('nouvelle_version', { v: v.version }) : t('maj_a_jour', { v: VERSION })),
      boutons: (v ? [{ libelle: t('maj_nouveautes'), action: () => { window.open(v.page); return true; } }] : [])
        .concat([{ libelle: t('fermer'), principal: true, action: () => true }]) });
  },
  // Aide › Informations de diagnostic: the versions of Nuée, the system, R and its packages,
  // to paste into a GitHub issue (no paths, no data)
  async diagnostic() {
    const v = await app.version();
    const d = bureau && bureau.diagnostic ? await bureau.diagnostic() : { systeme: navigator.userAgent };
    // (which R, File › Settings…; not where it is)
    const quelR = d.r && { nuee: /^linux/.test(d.systeme) ? 'the computer’s (PATH)' : 'Nuée’s own', autre: 'another R of this computer (File › Settings)',
      organisation: 'another R of this computer (set by the organisation’s policy)', rscript: 'RSCRIPT' }[d.r.type];
    const texte = [`Nuée ${VERSION}${!bureau ? ' (browser)' : d.installe ? ' (installed)' : ' (from the sources)'}`,
      `System: ${d.systeme}`,
      d.electron ? `Electron ${d.electron}, Chromium ${d.chrome}` : null,
      `${v.R}, ${v.plateforme}, ${v.systeme}`,
      quelR ? `R used: ${quelR}${d.r.repli ? ` (the R ${d.r.repli === 'organisation' ? 'set by the organisation' : 'chosen in Settings'} couldn’t start)` : ''}` : null,
      `R locale: ${v.locale}`,
      `R packages: ${Object.entries(v.paquets).map(([k, x]) => `${k} ${x}`).join(', ')}`,
      `Interface: ${langueActuelle()}`].filter(Boolean).join('\n');
    dialogue({ titre: t('diagnostic'), contenu: el('div', {}, el('p', { class: 'note' }, t('diagnostic_aide')), el('pre', { class: 'diagnostic' }, texte)),
      boutons: [{ libelle: t('signaler_probleme'), action: () => { window.open('https://github.com/4rtefakt/nuee/issues/new/choose'); return false; } },
        { libelle: t('rapport_probleme'), action: () => { app.rapportProbleme(texte); return true; } },
        { libelle: t('copier_texte'), action: () => { navigator.clipboard.writeText(texte).then(() => statut(t('texte_copie'))); return true; } },
        { libelle: t('fermer'), principal: true, action: () => true }] });
  },
  // One model per response (Model › One model per response, moteur/R/par_reponse.R): the
  // regression `id` fitted to each response ticked, the p-values corrected over the responses;
  // optionally each model kept in the project. The results go to the view (vues_par_reponse.js).
  // reponses: other responses ticked from the start (Which analysis?: the species chosen there).
  unModeleParReponse(id, reponses = []) {
    const m = etat.modeles[id];
    if (!m || !m.regression) return;
    const p = etat.projets[m.projet];
    const x = new Set([].concat((m.reglages && m.reglages.x) || []).map(Number));
    const y = Number([].concat((m.reglages && m.reglages.y) || [])[0]);
    const vars = p.variables.filter((v) => !v.qualitative && !x.has(v.uid));
    const coches = new Set([y, ...[].concat(reponses).map(Number).filter((u) => vars.some((v) => v.uid === u))]);
    let correction = 'BH';
    const garder = el('input', { type: 'checkbox' });
    const filtre = el('input', { placeholder: t('filtrer_variables'), style: 'width:14em' });
    const liste = el('div', { class: 'liste-cases', style: 'max-height:18em;overflow:auto;columns:2' });
    const dessiner = () => {
      const f = filtre.value.trim().toLowerCase();
      liste.replaceChildren(...vars.filter((v) => !f || v.nom.toLowerCase().includes(f)).map((v) => el('label', { class: 'champ', style: 'display:block' },
        el('input', { type: 'checkbox', checked: coches.has(v.uid), onchange: (e) => { if (e.target.checked) coches.add(v.uid); else coches.delete(v.uid); } }), v.nom)));
    };
    filtre.addEventListener('input', dessiner);
    dessiner();
    const tous = (oui) => { const f = filtre.value.trim().toLowerCase(); for (const v of vars) if (!f || v.nom.toLowerCase().includes(f)) { if (oui) coches.add(v.uid); else coches.delete(v.uid); } dessiner(); };
    dialogue({ titre: t('par_reponse_titre', { nom: m.nom }), large: true, contenu: el('div', {},
      el('p', { class: 'note' }, t('par_reponse_aide')),
      el('div', { class: 'outils' }, filtre, el('button', { onclick: () => tous(true) }, t('tout_cocher')), el('button', { onclick: () => tous(false) }, t('aucune_cocher')),
        el('label', { class: 'champ' }, el('span', {}, t('correction')), el('select', { onchange: (e) => { correction = e.target.value; } },
          [['BH', t('correction_bh')], ['holm', 'Holm'], ['bonferroni', 'Bonferroni']].map(([v, n]) => el('option', { value: v }, n))))),
      liste, el('label', { class: 'champ' }, garder, t('par_reponse_garder'))),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('ajuster'), principal: true, action: () => {
      if (!coches.size) return false;
      (async () => {
        const q = { modele: id, reponses: [...coches], correction, garder: garder.checked };
        const r = await occupe(() => demander('par_reponse', q));
        if (r.projet) projetChange(r.projet);
        for (const res of [].concat(r.resultats || [])) await garder_(res);
        app.ouvrirVue('par_reponse', id, { reponses: q.reponses, correction, cache: { cle: JSON.stringify([q.reponses, correction, langueActuelle()]), res: r } });
      })().catch((e) => statut(e.message, true));
      return true;
    } }] });
  },
  // A problem report: a .zip to send with a message, written where the user chooses (nothing is
  // sent): the diagnostic text, the session's journal (each action's call to the engine, the
  // engine's errors and the R console's commands, without its images) and, if ticked, the active
  // project as Save would write it (R: projet_texte), with its data.
  async rapportProbleme(diagnostic) {
    const pid = app.projetActif();
    const avecProjet = el('input', { type: 'checkbox' });
    const octets = (texte) => new TextEncoder().encode(texte);
    const ecrire = async () => {
      const j = await demander('journal', { depuis: 0 });
      const journal = [].concat(j.entrees || []).filter((e) => e.type !== 'image').map((e) => `[${e.heure}] ${e.type}\n${e.texte}`).join('\n\n');
      const fichiers = [['diagnostic.txt', octets(diagnostic)], ['journal.txt', octets(journal)]];
      if (pid && avecProjet.checked) {
        const r = await demander('projet_texte', { projet: pid, version: VERSION });
        fichiers.push([`${r.nom}.nuee`, octets(r.texte)]);
      }
      const contenu = zip(fichiers);
      const nom = `nuee-rapport-${new Date().toISOString().slice(0, 10)}.zip`;
      if (bureau) {
        const chemin = await bureau.enregistrerSous(nom, [{ name: 'zip', extensions: ['zip'] }]);
        if (!chemin) return;
        await bureau.ecrireFichier(chemin, contenu);
        statut(t('rapport_probleme_ecrit', { chemin }));
      } else {
        const a = el('a', { href: URL.createObjectURL(new Blob([contenu], { type: 'application/zip' })), download: nom });
        a.click();
        setTimeout(() => URL.revokeObjectURL(a.href), 10000);
      }
    };
    dialogue({ titre: t('rapport_probleme'), contenu: el('div', {}, el('p', {}, t('rapport_probleme_texte')),
      pid ? el('label', { class: 'champ' }, avecProjet, t('rapport_probleme_projet', { nom: etat.projets[pid].nom })) : null),
    boutons: [{ libelle: t('annuler'), action: () => true },
      { libelle: bureau ? t('choisir_emplacement') : t('telecharger'), principal: true, action: () => { occupe(ecrire).catch((e) => statut(e.message, true)); return true; } }] });
  },
  // looking for a new version at start-up (installed copy only; null otherwise)
  majAuto: () => (etat.majAuto === undefined ? null : etat.majAuto),
  async basculerMajAuto() {
    etat.majAuto = await bureau.majAuto(!etat.majAuto);
    if (!etat.majAuto) etat.nouvelleVersion = null;
    majEtat();
  },
  valeurs: (projet, variable, observations, identifiant) => demander('valeurs', { projet, variable, observations, identifiant }),

  // ---- second batch: predictions from a file, score variables, classes, R scripts
  async predireFichier(id) {
    let chemin;
    if (bureau) {
      chemin = await bureau.choisirFichier(filtresOuverture(true));
    } else {
      const f = await choisirFichierNavigateur(acceptOuverture(true));
      if (f) chemin = (await (await fetch('/api/deposer?nom=' + encodeURIComponent(f.name), { method: 'POST', body: f })).json()).chemin;
    }
    if (!chemin) return null;
    return occupe(() => demander('predire_fichier', { modele: id, chemin }));
  },
  async predireFichierVue(id) {
    const res = await app.predireFichier(id);
    if (res) app.ouvrirVue('predictions', id, { res, jeu: 'fichier', cle: 'fichier' });
  },
  async exporterPrediction(id) {
    const m = etat.modeles[id];
    const choix = await choisirExport(t('exporter_prediction_titre', { nom: m.nom }), `${nomFichierModele(m)} - ${t('predictions')}`, false);
    if (choix) apresExport(await occupe(() => demander('exporter_prediction', { modele: id, ...choix })));
  },
  async scoresEnVariables(id) {
    const r = await occupe(() => demander('scores_en_variables', { modele: id }));
    projetChange(r.projet);
    const noms = [].concat(r.noms);
    const modeles = [].concat(r.modeles).filter((x) => x !== etat.modeles[id].nom);
    statut(t('scores_ajoutes', { noms: noms.slice(0, 3).join(', ') + (noms.length > 3 ? '…' : '') })
      + (modeles.length ? t('modeles_a_reajuster', { modeles: modeles.join(', ') }) : ''));
  },
  // One model, or all the models of its project's session (the latest fit of each, as Ctrl+S
  // saves them) in one .R script: PCA, PLS, OPLS and regressions only. Another analysis is
  // exported on its own (serveur.R: script_demande leaves it out of a session's script), so its
  // dialog doesn't offer the session, and the session's list doesn't name it.
  async exporterScript(id) {
    const m = etat.modeles[id];
    const seance = modelesSeance(m.projet).filter((x) => !x.analyse);
    const choix = await choisirScript(m.nom, seance.length > 1 && seance.some((x) => x.id === id) ? { modeles: seance, nom: sansCaracteresInterdits(sansExtension(etat.projets[m.projet].nom)) } : null,
      nomFichierModele(m));
    if (!choix) return;
    const qui = choix.seance ? { modeles: seance.map((x) => x.id) } : { modele: id };
    const r = await occupe(() => demander('script_r', { ...qui, version: VERSION, langue: langueActuelle(), ...choix }));
    const chemins = [].concat(r.chemins);
    if (bureau) statut(chemins.length > 1 ? t('exporte_fichiers', { n: chemins.length, chemin: r.script }) : `${t('script_ecrit')} ${r.script}`);
    else chemins.forEach((c, i) => setTimeout(() => telecharger(c), i * 800));
    if (r.ecart) {
      dialogue({ titre: t('script_ecart_titre'), contenu: el('p', {}, t('script_ecart', { ecart: r.ecart })),
        boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
    }
  },
  // The Methods paragraph written from the model, in French or English, with its references.
  async texteMethodes(id) {
    const m = etat.modeles[id];
    let langue = langueActuelle();
    const corps = el('div', { class: 'texte-methodes', tabindex: 0 });  // (a text that scrolls: the keyboard scrolls it)
    const montrer = async () => {
      const [r, res] = await occupe(() => Promise.all([demander('methodes', { modele: id, langue, version: VERSION }),
        demander('resultats_texte', { modele: id, langue }).catch(() => ({ paragraphes: [] }))]));
      corps.replaceChildren(...[].concat(r.paragraphes).map((x) => el('p', {}, x)), el('h4', {}, t('references')),
        el('ol', {}, [].concat(r.references).map((x) => el('li', {}, x))),
        ...([].concat(res.paragraphes || []).filter(Boolean).length ? [el('h4', {}, t('resultats_titre')), ...[].concat(res.paragraphes).filter(Boolean).map((x) => el('p', { class: 'resultat' }, x))] : []));
    };
    const langues = el('div', { class: 'boutons-langue' }, [['fr', 'Français'], ['en', 'English']].map(([l, n]) => el('label', { class: 'champ' },
      el('input', { type: 'radio', name: 'langue-methodes', checked: l === langue, onchange: () => { langue = l; montrer(); } }), el('span', {}, n))));
    await montrer();
    dialogue({ titre: t('texte_methodes_titre', { nom: m.nom }), contenu: el('div', {}, el('p', { class: 'note' }, t('texte_methodes_note')), langues, corps),
      boutons: [{ libelle: t('copier_texte'), action: () => {
        const texte = [...corps.querySelectorAll('p:not(.resultat)')].map((x) => x.textContent).join('\n\n') + '\n\n'
          + [...corps.querySelectorAll('li')].map((x) => x.textContent).join('\n')
          + [...corps.querySelectorAll('p.resultat')].map((x) => `\n\n${x.textContent}`).join('');
        navigator.clipboard.writeText(texte).then(() => statut(t('texte_copie')));
        return false;
      } }, { libelle: t('fermer'), principal: true, action: () => true }] });
  },
  classeSelection(id) {
    const m = etat.modeles[id];
    const sel = app.selection(id);
    if (!m || !sel.length) return;
    const p = etat.projets[m.projet];
    const existantes = p.variables.filter((v) => v.qualitative).map((v) => v.nom);
    const variable = el('input', { value: 'Groupes', list: 'variables-classes', style: 'width:16em' });
    const classe = el('input', { value: `${t('groupe')} 1`, style: 'width:16em' });
    dialogue({ titre: t('classe_selection').replace('…', ''),
      contenu: el('div', { class: 'grille-champs' },
        el('label', {}, t('classe_variable')), el('div', {}, variable, el('datalist', { id: 'variables-classes' }, existantes.map((n) => el('option', { value: n })))),
        el('label', {}, t('classe_nom')), classe,
        el('label', {}, ''), el('span', { class: 'infos' }, `${sel.length} ${t('obs')}`)),
      boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('ajouter'), principal: true, action: () => {
        if (!variable.value.trim() || !classe.value.trim()) return false;
        occupe(() => demander('classe_selection', { projet: m.projet, variable: variable.value.trim(), etiquette: classe.value.trim(), observations: sel }))
          .then((res) => {
            projetChange(res);
            statut(t('classe_creee', { n: sel.length, classe: classe.value.trim(), variable: variable.value.trim() }));
          });
        return true;
      } }] });
  },
  verifier: (q) => demander('verifier', q),
  jeuTest: (q) => demander('jeu_test', q),
  statistiquesVariables: (pid) => demander('statistiques_variables', { projet: pid }),
  rapport: (id) => dialogueRapport(app, id),
  nouvelleVariable: (pid) => { const p = etat.projets[pid || app.projetActif()]; if (p) dialogueVariable(app, p); },
  async ajouterVariable(pid, q) {
    const nom = q.imputation ? 'imputer' : q.formule !== undefined ? 'variable_calculee' : 'classes_valeurs';
    const res = await occupe(() => demander(nom, { projet: pid, ...q }));
    projetChange(res);
    const im = res.imputation;
    statut(q.imputation ? t(im.composantes === 0 ? 'imputation_moyennes' : im.choisies ? 'imputation_faite_choisies' : 'imputation_faite', { n: im.valeurs, k: im.composantes, tours: im.tours })
      : t('variable_ajoutee', { nom: q.nom }));
  },
  // Edit › Add columns from a table: the dialog, then the columns added by R (Ctrl+Z removes them)
  ajouterColonnes: (pid) => { const p = etat.projets[pid || app.projetActif()]; if (p) dialogueColonnes(app, p); },
  async joindreColonnes(pid, q) {
    const res = await occupe(() => demander('ajouter_colonnes', { projet: pid, ...q }));
    projetChange(res);
    statut([].concat(res.jointure.phrases)[0]);
    return res;
  },
  // a table to read (Add columns from a table): its path, or null; in the browser the file is sent to the engine first
  async choisirTableau() {
    if (bureau) return bureau.choisirFichier(filtresOuverture());
    const f = await choisirFichierNavigateur(acceptOuverture());
    if (!f) return null;
    return (await (await fetch('/api/deposer?nom=' + encodeURIComponent(f.name), { method: 'POST', body: f })).json()).chemin;
  },
  async lirePressePapiers() {
    try { return (bureau ? await bureau.lirePressePapiers() : await navigator.clipboard.readText()) || ''; } catch { return ''; }
  },
  // Edit › Add raster values (raster.js): the dialog, then the values added by R, one variable per band (Ctrl+Z removes them)
  valeursRaster: (pid) => { const p = etat.projets[pid || app.projetActif()]; if (p) dialogueRaster(app, p); },
  async ajouterRaster(pid, q) {
    const res = await occupe(() => demander('ajouter_raster', { projet: pid, ...q }));
    projetChange(res);
    statut([].concat(res.raster.phrases)[0]);
    return res;
  },
  // Edit › Summarize by group: a new project beside this one, one row per group; its data open
  resumerGroupes: (pid) => { const p = etat.projets[pid || app.projetActif()]; if (p) dialogueResume(app, p); },
  async resumer(pid, q) {
    const res = await occupe(() => demander('resumer_groupes', { projet: pid, ...q }));
    await apresOuverture(res);
    statut(res.resume.phrase);
  },
  async modifierValeur(pid, observation, variable, valeur) {
    const res = await occupe(() => demander('modifier_valeur', { projet: pid, observation, variable, valeur }));
    projetChange(res);
    if (app.modelesDe(pid).length) statut(t('donnees_modifiees'));
  },
  // The data workbench (atelier.js, moteur/R/atelier.R): Edit › Find and replace, Categories, Units
  // and descriptions, also from the Data view's toolbar and a column's header (`uid`: that variable)
  remplacerDonnees: (pid, uid) => { const p = etat.projets[pid || app.projetActif()]; if (p) dialogueRemplacer(app, p, uid); },
  categoriesVariable: (pid, uid) => { const p = etat.projets[pid || app.projetActif()]; if (p) dialogueCategories(app, p, uid); },
  variablesProjet: (pid, uid) => { const p = etat.projets[pid || app.projetActif()]; if (p) dialogueVariables(app, p, uid); },
  // (a column's header: rename, treat as categories or as numbers, delete; atelier.js)
  actionsVariable: (pid, uid) => actionsVariable(app, etat.projets[pid], uid),
  // the documents whose text the engine changed (references that follow a variable renamed, undone,
  // redone): drawn again in their editors here, and in the other windows
  relireDocuments(pid, ids) {
    for (const id of ids) {
      if (app.surDocument) app.surDocument({ projet: pid, id });
      diffuser('document', { projet: pid, id });
    }
  },
  // one of its changes (the request `nom`), undone by a single Ctrl+Z; the views of the data are drawn again
  // (an answer that is a report holds the project's summary under `projet`: mettre_a_jour,
  // retirer_etape; one that asks which table of a file to read, none: nothing changed)
  async changerDonnees(nom, pid, q) {
    const res = await occupe(() => demander(nom, { projet: pid, ...q }));
    const resume = res.projet && res.projet.id ? res.projet : res.id ? res : null;
    if (resume) projetChange(resume);
    return res;
  },
  occupe: (f) => occupe(f),
  statut: (message, erreur) => statut(message, erreur),
  diffuser: (type, donnees) => diffuser(type, donnees),
  // for the report and the documents: a view's settings in its open panel, and a context to draw it
  // off screen (in the light theme, whatever the window's)
  locVue: (vue, id) => (etat.panneaux.get(`${vue}:${id}`) || {}).loc || null,
  // (what a model's plots show of this window beside its results: its selection, its highlighted
  // variables, its VIP choice, its dendrogram's groups; a document's plot in another language takes
  // them to the page that draws it, documents.js)
  etatModele: (id) => ({ selection: etat.selection[id] || null, variables: etat.variables[id] || null, choixVIP: etat.choixVIP[id] || null,
    groupesHCA: etat.groupesHCA[id] || null }),
  contexteHorsEcran: (loc, surGraphe) => ({ ...contexte({ loc }), rafraichir: () => {}, definirGraphe: surGraphe, menuGraphique: () => {}, clair: true }),
  // for a figure (figures.js): the views open in this window that show a plot (in a tab behind
  // another too: dockview takes it out of the page meanwhile), and a view's settings as a
  // session keeps them
  vuesGraphiques: () => [...etat.panneaux.values()].filter((e) => e.graphe && e.graphe.data && VUES[e.vue] && !EXTERNES.includes(e.vue)),
  locEnregistrable: (loc) => locEnregistrable(loc),
  // (a model's files' default name, "<project> - M1 TRIM": its report, rapport.js; a project's name
  // without its extension, and whether several are open: the figures' list, figures.js)
  nomFichierModele: (m) => nomFichierModele(m),
  nomProjetCourt: (pid) => nomProjetCourt(pid),
  plusieursProjets: () => plusieursProjets(),
  version: async () => ({ ...(await demander('version')), application: VERSION, electron: bureau && bureau.versions.electron }),

  // ---- views and windows
  // A view of a model opens in the model's page (as a tab beside the last view used there);
  // a project view (data, statistics...) as a tab of the central group.
  ouvrirVue(vue, id, loc, pid, position) {
    if (VUE_SEULE) return app.ouvrirFenetreVue(vue, id, pid, suffixeVue(vue, loc) ? loc : null);
    const m = id ? etat.modeles[id] : null;
    const projet = m ? m.projet : pid;
    const cle = `${vue}:${id || projet || ''}${suffixeVue(vue, loc)}`;
    const existant = trouverPanneau(cle);
    if (existant) {
      if (m) ouvrirPage(id).page.api.setActive();
      existant.api.setActive();
      const e = etat.panneaux.get(cle);
      if (e && loc) { Object.assign(e.loc, loc); rendrePanneau(e); }
      return;
    }
    if (loc) etat.locInitial[cle] = loc;
    const titre = VUES[vue].titre(m, etat.projets[projet] || {}, loc || {});
    if (m) {
      const { page, interne } = ouvrirPage(id);
      interne.addPanel({ id: cle, component: vue, title: titreCourt(titre, m), params: { modele: id, projet },
        position: position || positionDansPage(interne, id) });
      page.api.setActive();
    } else {
      dock.addPanel({ id: cle, component: vue, title: marqueLecture(projet) + titre, params: { modele: id, projet }, position: positionCentrale() });
      fermerAccueil();
    }
  },
  ouvrirModele(id) {
    const page = dock && dock.getPanel(`page:${id}`);
    if (page) page.api.setActive(); else vuesParDefaut(id);
  },

  // (loc: the settings the view starts with, a document's: which one it shows)
  ouvrirFenetreVue(vue, id, pid, loc = null) {
    const m = id ? etat.modeles[id] : null;
    // (de: the main window it belongs to, also when a view's own window opens it)
    const de = VUE_SEULE ? params.get('de') || ID_FENETRE : ID_FENETRE;
    const q = new URLSearchParams({ vue, modele: id || '', projet: pid || (m && m.projet) || '', de, ...(loc ? { loc: JSON.stringify(loc) } : {}) });
    window.open('./?' + q, '_blank', 'popup,width=1000,height=720');
  },

  detacher(panelId) {
    let p = panelId ? trouverPanneau(panelId) : app.panneauActif();
    if (estPage(p)) p = pages.get(p.id.slice(5)) && pages.get(p.id.slice(5)).activePanel;  // a page: its view in front
    if (!p) return;
    const e = etat.panneaux.get(p.id);
    if (!e) return;
    app.ouvrirFenetreVue(e.vue, e.params.modele, e.params.projet, suffixeVue(e.vue, e.loc) ? e.loc : null);
    p.api.close();
  },

  fermerVue(cle) {
    const p = trouverPanneau(cle);
    if (p) p.api.close();
  },

  fermerPanneau() {
    // (the guide's panel when it has the keyboard: panneauActif is then the view it is beside)
    const p = (dock && dock.activePanel && dock.activePanel.id === 'aide:' && dock.activePanel) || app.panneauActif() || (dock && dock.activePanel);
    if (p) p.api.close();
  },
  // Window › Close all views: every tab of the central group, models' pages and project views (the
  // explorer and the console stay; the start page comes back). And a tab's menu › Close all tabs of
  // its project: its models' pages and its views.
  avecVues: () => !!dock && dock.panels.some((p) => !EXTERNES.includes(p.id)),
  fermerVues() {
    if (dock) for (const p of [...dock.panels]) if (!EXTERNES.includes(p.id)) p.api.close();
  },
  fermerVuesProjet(pid) {
    if (!dock) return;
    for (const p of [...dock.panels]) {
      const e = etat.panneaux.get(p.id);
      const m = estPage(p) ? etat.modeles[p.id.slice(5)] : null;
      if ((m && m.projet === pid) || (!estPage(p) && e && e.params.projet === pid)) p.api.close();
    }
  },

  reinitialiser() {
    for (const p of [...dock.panels]) if (p.id !== 'explorateur') p.api.close();
    if (!dock.getPanel('explorateur')) app.basculerExplorateur();
    const m = app.modeleActif() || Object.keys(etat.modeles).pop();
    if (m) vuesParDefaut(m); else accueil();
  },

  nouvelleFenetre() {
    window.open('./?fenetre=' + Math.random().toString(36).slice(2, 8), '_blank', 'popup,width=1300,height=850');
  },
  focusFenetre: (id) => bureau && bureau.focusFenetre(id),

  consoleVisible: () => !!(dock && dock.getPanel('console:')),
  basculerConsole() {
    const c = dock.getPanel('console:');
    if (c) { c.api.close(); return; }
    const ref = dock.panels.find((p) => !EXTERNES.includes(p.id)) || dock.getPanel('accueil');
    const panel = dock.addPanel({ id: 'console:', component: 'console', title: t('console_r'), params: {},
      position: ref ? { referencePanel: ref.id, direction: 'below' } : { direction: 'below' } });
    panel.api.group.api.setSize({ height: 250 });
    const e = etat.panneaux.get('console:');
    if (e) setTimeout(() => { const s = e.element.querySelector('.saisie-r'); if (s) s.focus(); });
  },
  explorateurVisible: () => !!(dock && dock.getPanel('explorateur')),
  basculerExplorateur() {
    const p = dock.getPanel('explorateur');
    if (p) { p.api.close(); return; }
    dock.addPanel({ id: 'explorateur', component: 'explorateur', title: t('explorateur'), position: { direction: 'left' }, initialWidth: 300 });
  },

  themeSombre: () => document.documentElement.dataset.theme === 'sombre',
  changerTheme(sombre, propager = true) {
    document.documentElement.dataset.theme = sombre ? 'sombre' : 'clair';
    ecrire('theme', sombre ? 'sombre' : 'clair');
    for (const d of tousDocks()) d.updateOptions({ theme: sombre ? DV.themeDark : DV.themeLight });
    if (propager) diffuser('theme', { sombre });
    toutRedessiner();
  },
  changerLangue(l, propager = true) {
    changerLangueTexte(l);
    if (bureau && bureau.langue) bureau.langue(l);
    if (propager) diffuser('langue', { l });
    for (const d of tousDocks()) d.updateOptions({ messages: messagesDock() });
    toutRedessiner();
  },
  zoom: (d) => bureau && bureau.zoom(d),
  // (the guide in a panel beside the work, at what is in front: ouvrirAide; in a window of its own,
  // a detached view, in a dialog; About: aide.js)
  aide(sujet) {
    if (sujet === 'guide' && dock) return ouvrirAide();
    return sujet === 'apropos' ? app.apropos() : dialogueAide(app, sujet);
  },
  // the full guide: the text file shipped with the desktop app, or on GitHub
  ouvrirGuide() {
    if (bureau && bureau.ouvrirGuide) bureau.ouvrirGuide();
    else window.open(`https://github.com/4rtefakt/nuee/blob/main/${langueActuelle() === 'en' ? 'USER-GUIDE.txt' : 'LISEZ-MOI-APPLICATION.txt'}`);
  },
};

installerDocuments(app);  // (the documents: their view, app.ouvrirDocument, app.nouveauDocument...)
installerFigures(app);  // (the figures for publication: their view, app.composerFigure, app.ouvrirFigure...)
installerVersions(app);  // (File › History and What changed: their views, sharing, the status bar's clock...)
const C = creerCommandes(app);
// (Help › Example projects as a menu under a button: the start page's, the empty explorer's)
app.menuExemples = (bouton) => ouvrirMenuSous(bouton, menuExemples(C).sous);
installerEtapes(app);  // (the data's source and steps: Edit › Data steps, Update the data, the notice of a changed file)
installerReglages(app);  // (File › Settings…: the R the engine runs on; the message when Nuée's own took over)
installerCode(app);  // (R-code models: the bar over a project whose code was written elsewhere, app.barresCode)
installerAide(app);  // (Help: the tutorials and the guide online, About)
installerGreffons(app);  // (plugins: File › Plugins…, Model › Plugins, the bar over a project whose plugin isn't approved here)

// ------------------------------------------------------------ state helpers

function statut(message, erreur = false) {
  etat.message = message || '';
  etat.erreur = erreur;
  majEtat();
  // what a screen reader says: an error at once (role alert), the rest when it can; emptied first,
  // so that the same message said twice is said again
  const a = document.getElementById(erreur ? 'alerte' : 'annonce');
  if (!a) return;
  a.textContent = '';
  setTimeout(() => { a.textContent = etat.message; }, 30);
}

async function occupe(f) {
  if (!etat.occupe) {
    etat.debutOccupe = Date.now();
    clearTimeout(etat.minuterieLongue);
    etat.minuterieLongue = setTimeout(majEtat, 61000);  // after a minute, say how to get out of it
  }
  etat.occupe += 1;
  etat.erreur = false;
  majEtat();
  document.body.classList.add('occupe');
  try {
    const r = await f();
    etat.message = '';
    return r;
  } catch (e) {
    // (Stop pressed: not an error, nothing changed)
    if (e.reponse && e.reponse.arrete) { statut(t('calcul_arrete')); throw e; }
    console.error(e);
    statut(`${t('erreur')}${deuxPoints()}${e.message}`, true);
    throw e;
  } finally {
    etat.occupe -= 1;
    if (!etat.occupe) document.body.classList.remove('occupe');
    majEtat();
  }
}

// Stop: a request the user waits for (api.js), after a second and a half, gets a Stop button over
// the status bar, after the engine's state; main.js then writes the file the engine's long loops
// look at between two rounds (moteur/R/arret.R), and the request comes back "stopped", nothing
// changed. It is an element of its own beside #appli, which a dialog doesn't make inert (aria-live):
// New model fits with its dialog open. Desktop app only: in a browser nothing writes the file.
const ATTENTE_ARRET = 1500;
let minuterieArret = null;
let minuterieArretLent = null;
function majArret() {
  if (!bureau || !bureau.arreterCalcul) return;
  clearTimeout(minuterieArret);
  let b = document.getElementById('arret-calcul');
  const debut = debutAttente();
  const visible = debut !== null && Date.now() - debut >= ATTENTE_ARRET;
  if (debut !== null && !visible) minuterieArret = setTimeout(majArret, debut + ATTENTE_ARRET - Date.now() + 20);
  if (!visible) {
    etat.arretDemande = etat.arretLent = false;
    clearTimeout(minuterieArretLent);
    if (b && !b.hidden) { b.hidden = true; majEtat(); }
    return;
  }
  if (!b) {
    b = el('div', { id: 'arret-calcul', 'aria-live': 'polite', hidden: true }, el('button', { onclick: arreterCalcul }, icone('circle-stop'), el('span', { class: 'libelle' })));
    document.body.append(b);
  }
  const apparait = b.hidden;
  b.hidden = false;
  if (apparait) majEtat();
  // (after a minute, the status bar says how to get out of a computation that doesn't stop)
  if (Date.now() - debut < 61000) minuterieArret = setTimeout(() => { majEtat(); majArret(); }, debut + 61000 - Date.now());
  const bouton = b.querySelector('button');
  bouton.disabled = !!etat.arretDemande;
  bouton.title = t(etat.arretLent ? 'arret_lent' : 'arreter_calcul_aide');
  b.querySelector('.libelle').textContent = t(etat.arretLent ? 'arret_lent' : etat.arretDemande ? 'arret_demande' : 'arreter_calcul');
  placerArret();
}
// (after the engine's state in the status bar, whose text changes: majEtat places it again; in a
// view's own window, at the bottom left)
function placerArret() {
  const b = document.getElementById('arret-calcul');
  if (!b || b.hidden) return;
  const m = document.querySelector('#statut .moteur');
  b.style.left = `${m && m.getClientRects().length ? Math.round(m.getBoundingClientRect().right) + 10 : 10}px`;
}
async function arreterCalcul() {
  etat.arretDemande = true;
  majArret();
  try { await bureau.arreterCalcul(); } catch { etat.arretDemande = false; majArret(); return; }
  // (a computation that doesn't check between rounds, a single fit in compiled code: said after a while)
  clearTimeout(minuterieArretLent);
  minuterieArretLent = setTimeout(() => { if (etat.arretDemande) { etat.arretLent = true; majArret(); } }, 8000);
}
window.addEventListener('requetes', majArret);

function choisirFichierNavigateur(accept) {
  return new Promise((resolve) => {
    const i = el('input', { type: 'file', accept, onchange: () => resolve(i.files[0]) });
    i.click();
  });
}

function choisirFichiersNavigateur(accept, plusieurs) {
  return new Promise((resolve) => {
    const i = el('input', { type: 'file', accept, multiple: plusieurs, onchange: () => resolve([...i.files]) });
    i.click();
  });
}

function telecharger(chemin) {
  window.location.href = '/api/fichier?chemin=' + encodeURIComponent(chemin);
}

// A file with several tables (Excel sheets, objects of an .RData file): the one to open.
// Resolves to the opened project, or null when the choice is cancelled. An export of
// KoboToolbox or ODK Central comes with the preview of its import: the import's dialog.
function choisirTable(res) {
  if (res && res.kobo) return dialogueKobo(app, res);
  if (!res || !res.choisir) return Promise.resolve(res);
  const tables = [].concat(res.choisir);
  return new Promise((resolve) => {
    const liste = el('select', { class: 'liste-tables', size: Math.min(12, Math.max(4, tables.length)) },
      tables.map((x, i) => el('option', { value: x, selected: i === 0 }, x)));
    const ouvrir = () => {
      occupe(() => demander('ouvrir', { chemin: res.chemin, table: liste.value })).then(resolve, () => resolve(null));
      return true;
    };
    const d = dialogue({ titre: t('choisir_table'), contenu: el('div', {}, el('p', {}, t('choisir_table_texte', { nom: res.nom })), liste),
      boutons: [{ libelle: t('annuler'), action: () => { resolve(null); return true; } }, { libelle: t('ouvrir_table'), principal: true, action: ouvrir }] });
    liste.addEventListener('dblclick', () => { d.fermer(); ouvrir(); });
    liste.focus();
  });
}

// The export dialog: a format, then (desktop) where to write it. Resolves to { chemin } or
// (browser) { format }, or null when cancelled. The last format chosen is offered again.
// coordonnees: the project has coordinates (moteur/R/spatial.R), so GeoPackage is offered.
function choisirExport(titre, nom, plusieursTables, coordonnees = false) {
  return new Promise((resolve) => {
    const formats = ECRITURE.filter((f) => coordonnees || !f.coordonnees);
    let choix = lire('format_export');
    if (!formats.some((f) => f.ext === choix)) choix = 'xlsx';
    const note = el('p', { class: 'note' });
    const maj = () => { note.textContent = plusieursTables && formats.find((f) => f.ext === choix).plusieurs ? t('export_plusieurs_fichiers') : ''; };
    const liste = el('div', { class: 'formats-export' }, formats.map((f) => el('label', { class: 'champ' },
      el('input', { type: 'radio', name: 'format-export', value: f.ext, checked: f.ext === choix, onchange: () => { choix = f.ext; maj(); } }),
      el('span', {}, t(f.cle)), el('span', { class: 'doux' }, '.' + f.ext))));
    maj();
    dialogue({ titre, contenu: el('div', {}, liste, note), boutons: [
      { libelle: t('annuler'), action: () => { resolve(null); return true; } },
      { libelle: bureau ? t('choisir_emplacement') : t('telecharger'), principal: true, action: () => {
        ecrire('format_export', choix);
        if (!bureau) { resolve({ format: choix }); return true; }
        const f = ECRITURE.find((x) => x.ext === choix);
        bureau.enregistrerSous(`${nom}.${choix}`, [{ name: t(f.cle), extensions: [choix] }]).then((chemin) => resolve(chemin ? { chemin } : null));
        return true;
      } }] });
  });
}

// The R script dialog: where the data go (in the script, or a CSV file next to it) and the
// files written next to the script (renv.lock, the data and fitted model in .RData, an RStudio
// project). Resolves to the choices, with { chemin } on the desktop, or null. Remembered.
// seance: { modeles, nom } when the project has other models, offering them all in one .R script
// (its file named after the project's: nom); fichier: the model's file's name.
function choisirScript(nom, seance = null, fichier = nom) {
  return new Promise((resolve) => {
    const choix = { donnees: lire('script_donnees') === 'csv' ? 'csv' : 'script', format: ['R', 'Rmd', 'qmd'].includes(lire('script_format')) ? lire('script_format') : 'R',
      seance: false };
    for (const k of ['renv', 'rdata', 'rproj']) choix[k] = lire(`script_${k}`) === 'oui';
    const radio = (cle, valeur, note) => el('label', { class: 'champ' },
      el('input', { type: 'radio', name: `script-${cle}`, value: valeur, checked: choix[cle] === valeur, onchange: () => { choix[cle] = valeur; } }),
      el('span', {}, t(`script_${cle}_${valeur}`)), note ? el('span', { class: 'doux' }, note) : null);
    const coche = (cle) => el('label', { class: 'champ' },
      el('input', { type: 'checkbox', checked: choix[cle], onchange: (e) => { choix[cle] = e.target.checked; } }),
      el('span', {}, t(`script_${cle}`)), el('span', { class: 'doux' }, t(`script_${cle}_note`)));
    const formats = el('div', {}, radio('format', 'R', '.R'), radio('format', 'Rmd', '.Rmd'), radio('format', 'qmd', '.qmd'));
    const portee = (oui) => {  // the whole session: a .R script only
      choix.seance = oui;
      formats.querySelectorAll('input').forEach((i) => {
        i.disabled = oui && i.value !== 'R';
        if (oui) i.checked = i.value === 'R';
        else i.checked = i.value === choix.format;
      });
    };
    const quels = seance ? [el('h4', {}, t('script_modeles')),
      el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'script-portee', checked: true, onchange: () => portee(false) }), el('span', {}, t('script_ce_modele', { nom }))),
      el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'script-portee', onchange: () => portee(true) }),
        el('span', {}, t('script_seance', { n: seance.modeles.length })), el('span', { class: 'doux' }, seance.modeles.map((x) => x.nom).join(', ')))] : [];
    dialogue({ titre: t('script_titre', { nom }), contenu: el('div', { class: 'options-script' },
      el('p', {}, t('script_texte')), ...quels,
      el('h4', {}, t('script_format')), formats,
      el('h4', {}, t('script_donnees')), radio('donnees', 'script'), radio('donnees', 'csv'),
      el('h4', {}, t('script_fichiers')), coche('renv'), coche('rdata'), coche('rproj'),
      el('p', { class: 'note' }, t('script_note'))), boutons: [
      { libelle: t('annuler'), action: () => { resolve(null); return true; } },
      { libelle: bureau ? t('choisir_emplacement') : t('telecharger'), principal: true, action: () => {
        ecrire('script_donnees', choix.donnees);
        ecrire('script_format', choix.format);
        for (const k of ['renv', 'rdata', 'rproj']) ecrire(`script_${k}`, choix[k] ? 'oui' : 'non');
        const r = choix.seance ? { ...choix, format: 'R' } : choix;
        if (!bureau) { resolve(r); return true; }
        bureau.enregistrerSous(`${choix.seance ? seance.nom : fichier}.${r.format}`, [{ name: t(`script_format_${r.format}`), extensions: [r.format] }])
          .then((chemin) => resolve(chemin ? { ...r, chemin } : null));
        return true;
      } }] });
  });
}

function apresExport(r) {
  const chemins = [].concat(r.chemins || r.chemin);
  if (!bureau) { chemins.forEach((c, i) => setTimeout(() => telecharger(c), i * 800)); return; }
  statut(chemins.length > 1 ? t('exporte_fichiers', { n: chemins.length, chemin: chemins[0] }) : `${t('exporte')} ${chemins[0]}`);
}

async function apresOuverture(res, session = null) {
  // (the explorer follows: the other projects' fitted models folded, a line each without their views,
  // and the project opened brought into view, explorateur.js)
  for (const m of Object.values(etat.modeles)) if (m.projet !== res.id) etat.replies.add(`${m.projet}/m/${m.id}`);
  projetRecu(res);
  etat.noeud = res.id;
  diffuser('modeles');
  app.rafraichirExplorateur();
  majEtat();
  if (app.verifierSource) app.verifierSource(res.id);  // (its data file changed since it was read: a notice, etapes.js)
  if (dock && (session || res.session)) await restaurerSession(res.id, session);
  // a table (a project without models): its data open, in front, so that the menus act on it
  // (New model, Which analysis?, Save) rather than on the model shown before
  if (dock && ![].concat(res.modeles || []).length) app.ouvrirVue('donnees', null, null, res.id);
}

// ------------------------------------------------------------ recovery copies (desktop app)

// Every two minutes, a copy of each project with unsaved changes, with its session, goes to
// the application's recovery folder (deleted when the application closes normally). After an
// unexpected stop, the next start offers to reopen them. The index lists the copies to keep
// (main.js deletes the others): those of this launch, named after it so that they never
// replace one of an earlier stop, and those of an earlier stop that haven't been recovered or
// set aside yet (secoursEnAttente). A project whose copy fails this time keeps its last one.
// One main window writes them: the first one open, then, once it is closed, the next one
// (several main windows share the projects; each counts the two minutes).
const LANCEMENT = Date.now().toString(36);
let moteurs = 0;  // engines started by this launch: each numbers its projects from P1
let secoursEnAttente = [];
const derniersSecours = new Map();
async function sauvegarderSecours() {
  if (!bureau || VUE_SEULE || !dock) return;
  try {
    const principales = (await bureau.fenetres()).filter((f) => f.principale).sort((a, b) => a.id - b.id);
    if (principales.length && !principales[0].actuelle) return;
    // (a window taking over from the first one: the copies that one kept waiting are those the
    // index marks so, proposerRecuperation)
    if (ID_FENETRE !== 'principale') secoursEnAttente = [].concat(await bureau.secoursListe()).filter((e) => e && e.attente);
    const dossier = await bureau.secoursDossier();
    const entrees = [];
    for (const p of Object.values(etat.projets)) {
      if (!p.modifie) continue;
      const fichier = `${dossier}/${LANCEMENT}-${moteurs}-${p.id}.json`;
      try {
        // (lu: the state of the project's file when it was read or saved, for the question
        // before overwriting it once the copy is recovered)
        const r = await demander('sauvegarde_secours', { projet: p.id, chemin: fichier, session: JSON.stringify(collecterSession(p.id)), version: VERSION });
        // (an old version opened read-only comes back as one: versions.js)
        derniersSecours.set(p.id, { fichier, projet: p.id, nom: p.nom, origine: p.chemin_enregistrement || p.chemin || null, lu: r.lu || null, date: new Date().toISOString(),
          ...(p.lecture_seule ? { lecture_seule: p.lecture_seule } : {}) });
      } catch { /* this one is tried again two minutes later; the others go on */ }
      if (derniersSecours.has(p.id)) entrees.push(derniersSecours.get(p.id));
    }
    // (attente: kept when Nuée closes normally, to be offered at the next start; main.js)
    await bureau.secoursIndex([...secoursEnAttente.map((e) => ({ ...e, attente: true })), ...entrees]);
  } catch { /* tried again two minutes later */ }
}

// The copies of an earlier stop: reopened as the projects they hold (their name, the file they
// save to, if any, and unsaved changes), or set aside (deleted). Closing the question without
// answering keeps them, to be offered again next time.
async function proposerRecuperation() {
  if (!bureau || VUE_SEULE) return;
  let entrees = [];
  try { entrees = await bureau.secoursListe(); } catch { entrees = []; }
  if (!entrees.length) return;
  secoursEnAttente = entrees;
  bureau.secoursIndex(entrees.map((e) => ({ ...e, attente: true }))).catch(() => {});  // (marked at once, for another main window: sauvegarderSecours)
  const quand = (d) => new Date(d).toLocaleString(langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB', { dateStyle: 'medium', timeStyle: 'short' });
  dialogue({ titre: t('recuperation'),
    contenu: el('div', {}, el('p', {}, t('recuperation_question')), el('ul', {}, entrees.map((e) => el('li', {}, `${e.nom} — ${quand(e.date)}`)))),
    boutons: [
      { libelle: t('ignorer'), action: () => { secoursEnAttente = []; bureau.secoursIndex([]); return true; } },
      { libelle: t('recuperer'), principal: true, action: () => {
        (async () => {
          const illisibles = [];
          for (const e of entrees) {
            // before 0.9.1, a project without a file had its own copy as its origin
            const origine = e.origine && e.origine !== e.fichier ? e.origine : null;
            try { await apresOuverture(await occupe(() => demander('ouvrir', { chemin: e.fichier, secours: true, origine, nom: e.nom, lu: e.lu || null, lecture_seule: e.lecture_seule || undefined }))); } catch { illisibles.push(e); }
          }
          secoursEnAttente = illisibles;  // kept for another try
          if (illisibles.length) statut(t('recuperation_echec', { noms: illisibles.map((e) => e.nom).join(', ') }), true);
          sauvegarderSecours();  // the recovered projects get copies of their own at once
        })();
        return true;
      } }] });
}

// ------------------------------------------------------------ the R engine stopped

let moteurArrete = false;
function signalerMoteurArrete() {
  if (moteurArrete || VUE_SEULE) return;
  moteurArrete = true;
  dialogue({ titre: t('moteur_arrete'), contenu: el('p', {}, bureau ? t('moteur_arrete_texte') : t('moteur_navigateur')),
    boutons: bureau ? [{ libelle: t('plus_tard'), action: () => { moteurArrete = false; return true; } },
      { libelle: t('redemarrer_moteur'), principal: true, action: () => { app.redemarrerMoteur(); return true; } }]
      : [{ libelle: t('fermer'), principal: true, action: () => { moteurArrete = false; return true; } }] });
}
window.addEventListener('moteur-injoignable', signalerMoteurArrete);
if (bureau && bureau.surMoteurArrete) bureau.surMoteurArrete(signalerMoteurArrete);

// ------------------------------------------------------------ what's new, once after an update

const numerosVersion = (v) => String(v || '0').split('.').map(Number);
const plusRecente = (a, b) => {
  const x = numerosVersion(a);
  const y = numerosVersion(b);
  for (let i = 0; i < Math.max(x.length, y.length); i += 1) if ((x[i] || 0) !== (y[i] || 0)) return (x[i] || 0) > (y[i] || 0);
  return false;
};

function quoiDeNeuf(tout = false) {
  const vue = lire('version_vue');
  // (after an update only: a first start, with no version seen yet, opens on the start page alone,
  // not on the changes of every version since the first)
  const liste = tout ? NOUVEAUTES : vue ? NOUVEAUTES.filter((n) => plusRecente(n.version, vue)) : [];
  ecrire('version_vue', VERSION);
  if (!liste.length) return;
  const date = (d) => new Date(d).toLocaleDateString(langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB', { dateStyle: 'long' });
  dialogue({ titre: t('quoi_de_neuf'), large: true,
    contenu: el('div', { class: 'nouveautes' }, liste.map((n) => el('section', {},
      el('h3', {}, `${t('version')} ${n.version} — ${date(n.date)}`),
      el('ul', {}, (n[langueActuelle()] || n.fr).map((x) => el('li', {}, x)))))),
    boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
}

// ------------------------------------------------------------ session (saved with the project)

// What comes back when the project is reopened: its fitted models (by their number in the
// project), the panels showing them with their settings, selections, VIP choices and
// dendrogram groups. Ids change from one session to the next, so models are written '@<number>'
// and the project '@P'.
const NON_ENREGISTRES = new Set(['res', 'cle', 'cache', 'arbre', 'd', 'minuterie', 'focus', 'valeurs', 'cleValeurs']);

function locEnregistrable(loc) {
  const l = Object.fromEntries(Object.entries(loc).filter(([k, v]) => !NON_ENREGISTRES.has(k) && typeof v !== 'function'));
  if (l.jeu === 'fichier') l.jeu = '';  // a file's predictions are not kept
  return JSON.parse(JSON.stringify(l));
}

function remplacerIds(x, f) {
  if (typeof x === 'string') return f(x) ?? x;
  if (Array.isArray(x)) return x.map((y) => remplacerIds(y, f));
  if (x && typeof x === 'object') return Object.fromEntries(Object.entries(x).map(([k, v]) => [f(k) ?? k, remplacerIds(v, f)]));
  return x;
}

// The latest fit of each model of the project, in the order of their numbers.
function modelesSeance(pid) {
  const parNumero = new Map();
  for (const m of app.modelesDe(pid)) if (m.numero !== null && m.numero !== undefined) parNumero.set(m.numero, m);
  return [...parNumero.entries()].sort((a, b) => a[0] - b[0]).map(([, m]) => m);
}

function collecterSession(pid) {
  const parNumero = new Map();  // the latest fit of each project model
  for (const m of app.modelesDe(pid)) if (m.numero !== null && m.numero !== undefined) parNumero.set(m.numero, m);
  const code = new Map([...parNumero.values()].map((m) => [m.id, '@' + m.numero]));
  const panneaux = [...etat.panneaux.values()].filter((e) => !EXTERNES.includes(e.vue));
  // (a view that can't be made again from its settings, What changed: changements.js, isn't kept)
  const aMoi = (e) => e.params.projet === pid && (!e.params.modele || code.has(e.params.modele)) && !(VUES[e.vue] || {}).passager;
  const codeVue = (e) => (e.params.modele ? code.get(e.params.modele) : null);
  const vues = panneaux.filter(aMoi).map((e) => ({ vue: e.vue, modele: codeVue(e), loc: locEnregistrable(e.loc) }));
  let disposition = null;
  const externes = dock.panels.filter((p) => !EXTERNES.includes(p.id));
  const externeAMoi = (p) => (estPage(p) ? code.has(p.id.slice(5)) : !!etat.panneaux.get(p.id) && aMoi(etat.panneaux.get(p.id)));
  if (externes.length && externes.every(externeAMoi)) {  // the layout too, when it only shows this project
    const ids = new Map(panneaux.map((e) => [e.id, `${e.vue}:${codeVue(e) || '@P'}${suffixeVue(e.vue, e.loc)}`]));
    for (const p of externes) if (estPage(p)) ids.set(p.id, `page:${code.get(p.id.slice(5))}`);
    const internes = {};
    for (const [mid, d] of pages) if (code.has(mid)) internes[code.get(mid)] = remplacerIds(d.toJSON(), (s) => ids.get(s));
    disposition = { externe: remplacerIds(dock.toJSON(), (s) => ids.get(s)), internes };
  }
  const parModele = (o) => Object.fromEntries(Object.entries(o).filter(([id]) => code.has(id)).map(([id, v]) => [code.get(id), v]));
  return { version: 2, modeles: [...parNumero.values()].map((m) => ({ numero: m.numero, A: m.A })), vues, disposition,
    selection: parModele(etat.selection), variables: parModele(etat.variables), choixVIP: parModele(etat.choixVIP), groupesHCA: parModele(etat.groupesHCA) };
}

// Is the model `numero` of the project an R-code model whose code isn't approved on this computer
// (the engine says so in the project's summary: moteur/serveur.R, resume_modele)?
function codeEnAttente(pid, numero) {
  const d = [].concat((etat.projets[pid] || {}).modeles || []).find((x) => x.numero === numero);
  return !!(d && d.code_r && d.code_r.approuve === false);
}

async function restaurerSession(pid, donnee = null) {
  let s = donnee;
  if (!s) try { s = JSON.parse((await demander('session', { projet: pid })).session); } catch { return; }
  if (!s || !s.modeles || !s.modeles.length) return;
  const ids = {};
  let arrete = false;
  await occupe(async () => {
    for (const m of s.modeles) {
      // (an R-code model whose code isn't approved here: not even asked, the engine would refuse it;
      // the bar over the project offers to read it, then run it: vues_code.js)
      if (codeEnAttente(pid, m.numero)) continue;
      try {
        const res = await demander('ajuster', { projet: pid, numero: m.numero, A: m.A });
        etat.modeles[res.id] = res;
        ids['@' + m.numero] = res.id;
      } catch (e) {  // (a model that no longer fits is left out; Stop: the others too, they wait in the explorer)
        if (e.reponse && e.reponse.arrete) { arrete = true; break; }
      }
    }
  });
  if (arrete) statut(t('session_arretee', { n: Object.keys(ids).length }));
  await rafraichirProjet(pid);
  diffuser('modeles');
  for (const cle of ['selection', 'variables', 'choixVIP', 'groupesHCA']) {
    for (const [code, v] of Object.entries(s[cle] || {})) if (ids[code]) etat[cle][ids[code]] = v;
  }
  const idPanneau = (code, vue, loc) => `${vue}:${code === '@P' || !code ? pid : ids[code]}${suffixeVue(vue, loc)}`;
  const vues = (s.vues || []).filter((v) => !v.modele || ids[v.modele]);
  for (const v of vues) etat.locInitial[idPanneau(v.modele, v.vue, v.loc)] = v.loc;
  let fait = false;
  const libre = !dock.panels.some((p) => !EXTERNES.includes(p.id));
  if (s.disposition && s.disposition.externe && libre && vues.length === s.vues.length) {
    const nouvel = (x) => {
      const r = /^([a-z_0-9]+):(@P|@\d+)(~[\w-]+)?$/.exec(x);
      if (!r) return undefined;
      return r[1] === 'page' ? `page:${ids[r[2]]}` : idPanneau(r[2], r[1]) + (r[3] || '');
    };
    const externe = remplacerIds(s.disposition.externe, nouvel);
    for (const p of Object.values(externe.panels || {})) p.params = { modele: estPage(p) ? p.id.slice(5) : null, projet: pid };
    for (const [code, d] of Object.entries(s.disposition.internes || {})) {
      if (!ids[code]) continue;
      const interne = remplacerIds(d, nouvel);
      for (const p of Object.values(interne.panels || {})) p.params = { modele: ids[code], projet: pid };
      dispositionsAttente[ids[code]] = interne;
    }
    try {
      restauration = true;
      dock.fromJSON(externe);
      fait = true;
    } catch { /* an unreadable layout: the views open one by one below */ } finally { restauration = false; }
  }
  if (!fait) {  // each model's page as for a new model, then the saved views with their settings
    for (const code of new Set(vues.map((v) => v.modele).filter(Boolean))) vuesParDefaut(ids[code]);
    for (const v of vues) app.ouvrirVue(v.vue, v.modele ? ids[v.modele] : null, v.loc, pid);
  }
  for (const v of vues) delete etat.locInitial[idPanneau(v.modele, v.vue, v.loc)];
  for (const k of Object.keys(dispositionsAttente)) delete dispositionsAttente[k];
  app.rafraichirExplorateur();
  rafraichirModele(null);
  statut(t('session_restauree', { n: Object.keys(ids).length }));
}

// The views that keep a copy of a project's data, or results computed from it (data table,
// variable statistics, statistical tests, series, the map and the spatial autocorrelation; the history too, for a project replaced from
// the R console): computed again once the data change.
function oublierCalculs(pid) {
  for (const e of etat.panneaux.values()) {
    if (e.params.projet === pid && ['donnees', 'statistiques', 'tests', 'series', 'historique', 'carte', 'autocorrelation_spatiale', 'rarefaction'].includes(e.vue)) {
      for (const k of ['d', 'res', 'cle', 'cleValeurs', 'cache']) delete e.loc[k];
    }
  }
}

// A project's summary from R, kept. R counts each change of its data, made in this window or
// another, as a revision: when the revision differs from the summary kept so far, the views
// computed from the data are forgotten (every summary goes through here, so that a revision is
// never kept without them).
function projetRecu(res) {
  const avant = etat.projets[res.id];
  etat.projets[res.id] = res;
  if (avant && avant.revision !== res.revision) oublierCalculs(res.id);
  // (its versions told it is open, and each time its unsaved state flips: versions.js)
  if (app.suivreProjet) app.suivreProjet(res);
}

// After the project changed (its data, or its models): its summary, the explorer, the colour
// and label lists, and the views that keep a copy of its data when the data changed.
function projetChange(res) {
  projetRecu(res);
  app.rafraichirExplorateur();
  rafraichirModele(null);
  diffuser('modeles');
  majEtat();
}

async function rafraichirProjet(pid) {
  projetRecu(await demander('projet', { projet: pid }));
  app.rafraichirExplorateur();
}

async function garder(res) {
  etat.modeles[res.id] = res;
  if (etat.selectionLiee) {  // a new model of the project takes up its current selection (that of a Series view too)
    const autre = app.modelesDe(res.projet).find((m) => m.id !== res.id && (etat.selection[m.id] || []).length);
    const courante = autre ? etat.selection[autre.id] : etat.selection[res.projet] || [];
    const dans = new Set(res.observations.uid);
    if (courante.length) etat.selection[res.id] = courante.filter((u) => dans.has(u));
  }
  etat.noeud = `${res.projet}/m/${res.id}`;
  etat.replies.delete(`${res.projet}/calcules`);
  await rafraichirProjet(res.projet);
  // (the project's views that list its models, the map's Colour and Size and Compare models, drawn
  // again: a model fitted after they were opened shows in them at once)
  rafraichirModele(res.projet);
  diffuser('modeles');
  majEtat();
}

// A new fit `res` in the place of the fit `ancien` (an edited R-code model, which keeps its number:
// ajusterDefinition, dialogues.js): as Update the data does, every fit of its model leaves the
// session for it (remplacerAjustements), the first one's page with its layout and views' settings.
async function remplacerAjustement(ancien, res) {
  const vieux = etat.modeles[ancien];
  const memes = vieux && vieux.numero !== null && vieux.numero !== undefined
    ? app.modelesDe(vieux.projet).filter((m) => m.id !== ancien && m.id !== res.id && m.numero === vieux.numero) : [];
  await remplacerAjustements(res.projet, [vieux, ...memes].filter(Boolean), res);
  if (!pages.get(res.id)) vuesParDefaut(res.id);
  etat.noeud = `${res.projet}/m/${res.id}`;
  await rafraichirProjet(res.projet);
  rafraichirModele(res.projet);
  diffuser('modeles');
  majEtat();
}

// The fit `res` in the place of the fits `anciens` of the same model (Update the data, an R-code
// model's code edited): here (suivreAjustement), then in the other windows (ajustement_remplace,
// before 'modeles'), then they leave the session, in the engine too.
async function remplacerAjustements(pid, anciens, res) {
  etat.modeles[res.id] = res;
  for (const m of anciens) {
    suivreAjustement(pid, m, res);
    // (a view's own window that fitted it, an undo pressed there: its view shows the new fit)
    const seul = VUE_SEULE && etat.panneaux.get('seul');
    if (seul && seul.params.modele === m.id) await lierVue(seul, { modele: res.id });
    diffuser('ajustement_remplace', { ancien: m.id, nouveau: res.id });
    // (its page closed at once, so that its tab and the new one's never show together)
    oublierModele(m.id);
    await demander('supprimer_modele', { modele: m.id }).catch(() => {});
  }
}

// What the fit `m` had in this window, taken by `res`: its selection (the observations it still
// has), highlighted variables and VIP choice, its fold in the explorer and its node, its page with
// its layout and views' settings (remplacerPage). The caller then forgets `m`.
function suivreAjustement(pid, m, res) {
  const dans = new Set([].concat((res.observations && res.observations.uid) || []));
  if (etat.selection[m.id] && !etat.selection[res.id]) etat.selection[res.id] = etat.selection[m.id].filter((u) => dans.has(u));
  for (const k of ['variables', 'choixVIP']) if (etat[k][m.id] && !etat[k][res.id]) etat[k][res.id] = etat[k][m.id];
  if (etat.replies.has(`${pid}/m/${m.id}`)) etat.replies.add(`${pid}/m/${res.id}`);  // (folded in the explorer, it stays so)
  remplacerPage(m.id, res.id);
  const n = (etat.noeud || '').split('/');
  if (n[1] === 'm' && n[2] === m.id) { n[2] = res.id; etat.noeud = n.join('/'); }
}

function oublierModele(id) {
  delete etat.modeles[id];
  delete etat.selection[id];
  delete etat.variables[id];
  const page = dock && dock.getPanel(`page:${id}`);
  if (page) page.api.close();
  app.rafraichirExplorateur();
  rafraichirModele(null);
  majEtat();
}

function oublierProjet(pid) {
  for (const m of app.modelesDe(pid)) oublierModele(m.id);
  delete etat.projets[pid];
  if (dock) {
    for (const p of [...dock.panels]) {
      const e = etat.panneaux.get(p.id);
      if ((e && e.params.projet === pid) || (estPage(p) && !etat.modeles[p.id.slice(5)])) p.api.close();
    }
  }
  app.rafraichirExplorateur();
  majEtat();
}

// Everything the R server holds (for a new window, or after another window changed it).
async function synchroniser() {
  const e = await demander('etat');
  app.codeInterdit = !!e.code_interdit;  // (R-code models turned off by a policy: vues_code.js doesn't offer them)
  app.greffonsEtat = e.greffons || null;  // (the plugins installed, for the menus and New model: vues_greffons.js)
  if (app.majGreffons) app.majGreffons();
  const avant = etat.projets;
  etat.projets = Object.fromEntries(e.projets.map((p) => [p.id, p]));
  // (data changed in another window: R counts each change as a revision of the project)
  for (const [id, p] of Object.entries(etat.projets)) if (avant[id] && avant[id].revision !== p.revision) oublierCalculs(id);
  // (their unsaved state as the engine says it, also for the versions: versions.js, suivreProjet)
  if (app.suivreProjet) for (const p of e.projets) app.suivreProjet(p);
  const ids = new Set(e.modeles.map((m) => m.id));
  for (const m of e.modeles) etat.modeles[m.id] = m;
  for (const id of Object.keys(etat.modeles)) if (!ids.has(id)) { if (dock) oublierModele(id); else delete etat.modeles[id]; }
  if (dock) for (const p of [...dock.panels]) {
    const x = etat.panneaux.get(p.id);
    if (x && x.params.projet && !etat.projets[x.params.projet] && !x.params.modele) p.api.close();
  }
  app.rafraichirExplorateur();
  rafraichirModele(null);
  majEtat();
}

// A model's page: its summary on the left, scores and loadings on the right, one above the
// other. The page is a new tab of the central group; in a view's own window (a model fitted from
// there: Exclude and fit again, BACI), of its main window.
function vuesParDefaut(id) {
  if (!dock) { demanderPrincipale('ouvrirModele', { modele: id }).catch((e) => statut(e.message, true)); return; }
  const { page, interne } = ouvrirPage(id);
  const r = `resume:${id}`;
  page.api.setActive();
  dimensionnerPage(id);  // split at the page's real size
  if (!interne.getPanel(r)) {
    app.ouvrirVue('resume', id, null, null, interne.panels.length ? { referencePanel: interne.panels[0].id, direction: 'left' } : undefined);
  }
  // a regression: its coefficients and residuals (no scores or loadings: regression.R), a GAM made
  // only of curves its effects (it has no coefficient but the intercept); another analysis: the two
  // views it names (m.vues_defaut)
  const m = etat.modeles[id];
  const [haut, bas] = m.vues_defaut ? [].concat(m.vues_defaut)
    : m.regression ? (seulementCourbes(m) ? ['effets', 'residus'] : ['coefficients', m.type === 'GAM' ? 'effets' : 'residus']) : ['scores', null];
  if (!interne.getPanel(`${haut}:${id}`)) app.ouvrirVue(haut, id, null, null, { referencePanel: r, direction: 'right' });
  // spectra: their loadings as lines rather than a scatter of hundreds of variables
  const vue = bas || ([].concat((etat.modeles[id].reglages || {}).spectres || []).length ? 'profils' : 'loadings');
  if (!interne.getPanel(`${vue}:${id}`)) app.ouvrirVue(vue, id, null, null, { referencePanel: `${haut}:${id}`, direction: 'below' });
  interne.getPanel(r).api.setActive();
  derniers.set(id, `${haut}:${id}`);  // other views join it as tabs
  if (interne.width > 200) interne.getPanel(r).api.group.api.setSize({ width: Math.round(interne.width * 0.36) });  // summary narrower
}

// A page's dock takes the size of its tab: when the page is made, comes to the front, or the
// window changes size (dockview only notices size changes of a visible element).
function dimensionnerPage(id) {
  const d = pages.get(id);
  const e = elementsPages.get(id);
  if (d && e && e.clientWidth && (e.clientWidth !== d.width || e.clientHeight !== d.height)) d.layout(e.clientWidth, e.clientHeight);
}

// ------------------------------------------------------------ pages and panels

const tousDocks = () => (dock ? [dock, ...pages.values()] : []);
const estPage = (p) => !!p && typeof p.id === 'string' && p.id.startsWith('page:');
// (the tabs of an old version opened read-only, versions.js: marked, its model pages beside the
// project's own of the same names)
const marqueLecture = (pid) => (etat.projets[pid] && etat.projets[pid].lecture_seule ? '🔒 ' : '');
// A project's name without its file's extension ("skylark_counts"); with several projects open, a
// model's tab starts with it, as Compare models' bars do: "skylark_counts · M1 · TRIM"
// (retitrerPages, from majEtat, when that changes)
const sansExtension = (nom) => String(nom || '').replace(/\.[^.]+$/, '');
const nomProjetCourt = (pid) => {
  const nom = String((etat.projets[pid] || {}).nom || '');
  // (two projects of the same name, a table and its project: with their extensions)
  return Object.values(etat.projets).some((p) => p.id !== pid && sansExtension(p.nom) === sansExtension(nom)) ? nom : sansExtension(nom);
};
const plusieursProjets = () => Object.keys(etat.projets).length > 1;
const titrePage = (m) => `${marqueLecture(m.projet)}${plusieursProjets() ? `${nomProjetCourt(m.projet)} · ` : ''}${m.nom} · ${m.type}`;
function retitrerPages() {
  const signature = plusieursProjets() ? Object.values(etat.projets).map((p) => `${p.id}:${p.nom}`).join('|') : '';
  if (!dock || signature === etat.titresPages) return;
  etat.titresPages = signature;
  for (const id of pages.keys()) {
    const page = dock.getPanel(`page:${id}`);
    if (page && etat.modeles[id]) page.api.setTitle(titrePage(etat.modeles[id]));
  }
}
// A model's files, by default: "<project> - M1 TRIM" (the M1 of two projects no longer take the same
// name in a folder), without the characters a file's name can't have
const sansCaracteresInterdits = (s) => s.replace(/[\\/:*?"<>|]+/g, '_').trim();
const nomFichierModele = (m) => sansCaracteresInterdits(`${sansExtension((etat.projets[m.projet] || {}).nom)} - ${m.nom} ${m.type}`);
// The tabs of a project titled again (an old version saved elsewhere is read-only no more)
function retitrer(pid) {
  for (const m of app.modelesDe(pid)) {
    const page = dock && dock.getPanel(`page:${m.id}`);
    if (page) page.api.setTitle(titrePage(m));
  }
  for (const e of etat.panneaux.values()) if (e.params.projet === pid && !e.params.modele) rendrePanneau(e);
}
const titreCourt = (titre, m) => (m && titre.startsWith(`${m.nom} · `) ? titre.slice(m.nom.length + 3) : titre);

// A view shown once per thing of its own (a document) adds it to its panel's id: document:P1~D2.
function suffixeVue(vue, loc) {
  const v = VUES[vue];
  const s = v && v.suffixe && loc ? v.suffixe(loc) : '';
  return s ? `~${s}` : '';
}

function trouverPanneau(id) {
  for (const d of tousDocks()) {
    const p = d.getPanel(id);
    if (p) return p;
  }
  return null;
}

// The view in focus: the view in front in the active page, or the active project view (while the
// keyboard is in the R console or the guide, the one in front before).
function panneauCourant() {
  let p = dock && dock.activePanel;
  if (p && (p.id === 'console:' || p.id === 'aide:')) p = dernierContenu && dock.getPanel(dernierContenu);
  if (!p || EXTERNES.includes(p.id)) return null;
  if (estPage(p)) {
    const d = pages.get(p.id.slice(5));
    return (d && d.activePanel) || null;
  }
  return p;
}

// A new tab of the central group: beside the active page or view, or in place of the welcome page.
function positionCentrale() {
  const contenu = (p) => p && !EXTERNES.includes(p.id);
  const ref = (contenu(dock.activePanel) && dock.activePanel) || dock.panels.find(contenu) || dock.getPanel('accueil');
  if (ref) return { referencePanel: ref.id, direction: 'within' };
  return dock.getPanel('explorateur') ? { referencePanel: 'explorateur', direction: 'right' } : undefined;
}

// Help › User guide (F1): the guide's panel at the window's right, at the section of what is in
// front (aide.js); already open, it goes there. The keyboard in its search field.
function ouvrirAide() {
  const contexte = contexteAide();
  let panel = dock.getPanel('aide:');
  if (panel) {
    const e = etat.panneaux.get('aide:');
    if (e && e.element._aide) e.element._aide.aller(contexte);
    panel.api.setActive();
  } else {
    etat.locInitial['aide:'] = { contexte };
    panel = dock.addPanel({ id: 'aide:', component: 'aide', title: t('guide'), params: {}, position: { direction: 'right' } });
    panel.api.group.api.setSize({ width: Math.min(460, Math.max(320, Math.round(window.innerWidth * 0.3))) });
  }
  setTimeout(() => { const e = etat.panneaux.get('aide:'); if (e && e.element._aide) e.element._aide.focus(); });
}

// What is in front, for the guide (aide_texte.js: sectionPour, blocCible): the view and the active
// model's type; the view's name and, when a menu has it, its place there ("Plots › AIC selection"),
// as the guide writes them.
function contexteAide() {
  const p = panneauCourant();
  const e = p && etat.panneaux.get(p.id);
  const m = etat.modeles[app.modeleActif()];
  const vue = e ? e.vue : null;
  const menu = vue && C[vue] ? (['predictions', 'comparaison'].includes(vue) ? 'm_prediction' : 'm_graphiques') : null;
  const libelle = vue && vue !== 'resume' ? t(vue === 'predictions' ? 'predire' : vue) : null;
  return { vue, type: m ? m.type : null, langue: langueActuelle(), libelle: libelle !== vue ? libelle : null, chemin: menu && libelle ? `${t(menu)} › ${libelle}` : null };
}

function fermerAccueil() {
  const a = dock.getPanel('accueil');
  if (a && dock.panels.some((p) => !EXTERNES.includes(p.id))) a.api.close();
}

// In a page, a new view joins the last view used there (or the scores) as a tab.
function positionDansPage(interne, id) {
  const dernier = derniers.get(id);
  const ref = (dernier && interne.getPanel(dernier)) || interne.getPanel(`scores:${id}`) || interne.activePanel || interne.panels[0];
  return ref ? { referencePanel: ref.id, direction: 'within' } : undefined;
}

function ouvrirPage(id) {
  let page = dock.getPanel(`page:${id}`);
  if (!page) {
    const m = etat.modeles[id];
    page = dock.addPanel({ id: `page:${id}`, component: 'page', title: titrePage(m), params: { modele: id, projet: m.projet }, position: positionCentrale() });
    fermerAccueil();
    dimensionnerPage(id);
  }
  return { page, interne: pages.get(id) };
}

// The page of model `ancien` made again for model `nouveau`, a fit of the same definition
// (app.ajusterDeNouveau): beside it among the tabs, in front if it was, with its views, their
// layout and their settings, as a saved session brings them back (dispositionsAttente,
// locInitial). The caller then closes the old one. Without a page, nothing.
function remplacerPage(ancien, nouveau) {
  const page = dock && dock.getPanel(`page:${ancien}`);
  const interne = pages.get(ancien);
  // (two fits of the definition in the session, each with its page: the first one's page is kept)
  if (!page || !interne || dock.getPanel(`page:${nouveau}`)) return;
  const m = etat.modeles[nouveau];
  const ids = new Map();
  for (const e of etat.panneaux.values()) {
    if (e.params.modele !== ancien) continue;
    const cle = `${e.vue}:${nouveau}${suffixeVue(e.vue, e.loc)}`;
    ids.set(e.id, cle);
    etat.locInitial[cle] = locEnregistrable(e.loc);
  }
  const disposition = remplacerIds(interne.toJSON(), (s) => ids.get(s));
  for (const p of Object.values(disposition.panels || {})) p.params = { modele: nouveau, projet: m.projet };
  dispositionsAttente[nouveau] = disposition;
  const rang = page.group ? page.group.panels.indexOf(page) : -1;
  try {
    restauration = true;
    dock.addPanel({ id: `page:${nouveau}`, component: 'page', title: titrePage(m), params: { modele: nouveau, projet: m.projet },
      position: { referencePanel: page.id, direction: 'within', ...(rang >= 0 ? { index: rang } : {}) }, inactive: dock.activePanel !== page });
  } finally {
    restauration = false;
    delete dispositionsAttente[nouveau];
    for (const cle of ids.values()) delete etat.locInitial[cle];
  }
  if (derniers.has(ancien) && ids.has(derniers.get(ancien))) derniers.set(nouveau, ids.get(derniers.get(ancien)));
  dimensionnerPage(nouveau);
  // (a layout that couldn't be read: the page's usual views)
  if (!pages.get(nouveau) || !pages.get(nouveau).panels.length) vuesParDefaut(nouveau);
}

// The panels of both kinds of dock: a view (drawn by panneaux.js) or a model's page.
function composantVue(options) {
  const element = el('div', { class: 'panneau' + (options.name === 'explorateur' ? ' explorateur' : '') });
  const entree = { id: options.id, element, vue: options.name, params: {}, loc: etat.locInitial[options.id] || {} };
  delete etat.locInitial[options.id];
  return {
    element,
    init(parameters) {
      entree.params = parameters.params || {};
      entree.api = parameters.api;
      etat.panneaux.set(options.id, entree);
      rendrePanneau(entree);
    },
    dispose() {
      if (etat.panneaux.get(options.id) === entree) etat.panneaux.delete(options.id);
      const v = VUES[options.name];
      if (v && v.fermer) v.fermer(element);  // (a document: what was just typed is kept)
    },
  };
}

function composantPage() {
  const element = el('div', { class: 'page-modele' });
  let id = null;
  let interne = null;
  return {
    element,
    init(parameters) {
      id = parameters.params.modele;
      interne = DV.createDockview(element, optionsDock(true));
      pages.set(id, interne);
      elementsPages.set(id, element);
      new ResizeObserver(() => dimensionnerPage(id)).observe(element);
      interne.onDidActivePanelChange((ev) => {
        const p = ev && 'panel' in ev ? ev.panel : ev;  // an event { panel, origin } in this version of dockview
        if (p && !p.id.startsWith('resume:')) derniers.set(id, p.id);
        const page = dock.getPanel(`page:${id}`);
        if (p && page && !restauration && dock.activePanel !== page) page.api.setActive();
        majEtat();
      });
      interne.onDidRemovePanel(() => {
        if (restauration || interne.panels.length) return majEtat();
        setTimeout(() => { const page = dock.getPanel(`page:${id}`); if (page && pages.get(id) === interne && !interne.panels.length) page.api.close(); });
      });
      const attente = dispositionsAttente[id];
      if (attente) {
        delete dispositionsAttente[id];
        try { interne.fromJSON(attente); } catch { /* the views reopen from the saved list */ }
      }
    },
    dispose() {
      if (pages.get(id) === interne) { pages.delete(id); elementsPages.delete(id); }
      derniers.delete(id);
      if (interne) interne.dispose();
    },
  };
}

function actionsGroupe(groupe) {
  const element = el('div', { class: 'actions-groupe' });
  return {
    element,
    init() {
      element.append(
        el('button', { class: 'icone-seule', title: t('detacher'), 'aria-label': t('detacher'), onclick: () => groupe.activePanel && app.detacher(groupe.activePanel.id) }, icone('external-link')),
        el('button', { class: 'icone-seule', title: t('agrandir'), 'aria-label': t('agrandir'), onclick: () => (groupe.api.isMaximized() ? groupe.api.exitMaximized() : groupe.api.maximize()) }, icone('maximize-2')));
    },
    dispose() {},
  };
}

// (a tab's menu: menuOnglet, below; its tab: accessibilite.js, ongletDock)
function optionsDock() {
  return {
    theme: app.themeSombre() ? DV.themeDark : DV.themeLight,
    createComponent: (options) => (options.name === 'page' ? composantPage(options) : composantVue(options)),
    createTabComponent: () => ongletDock(),
    defaultTabComponent: 'onglet',
    createRightHeaderActionComponent: actionsGroupe,
    messages: messagesDock(),
  };
}

// A tab's menu, from the mouse or the keyboard (Shift+F10 or the menu key on a tab): ui.js's menus,
// which the keyboard goes through, rather than dockview's. After a tab is closed, the keyboard is on
// its group's tab in front.
function menuOnglet(e) {
  const onglet = e.target.closest && e.target.closest('.dv-tab');
  const marque = onglet && onglet.querySelector('[data-panneau]');
  const panel = marque && trouverPanneau(marque.dataset.panneau);
  if (!panel) return;
  e.preventDefault();
  e.stopPropagation();
  const group = panel.api.group;
  const d = tousDocks().find((x) => x.groups.includes(group));
  const ensuite = () => setTimeout(() => focusOnglet(group.element && group.element.isConnected ? group : d && d.activeGroup));
  const agrandir = { libelle: group.api.isMaximized() ? t('restaurer') : t('agrandir'),
    action: () => (group.api.isMaximized() ? group.api.exitMaximized() : group.api.maximize()) };
  // (a tab of a project, a model's page or view, a project view: all the tabs of that project too)
  const entree = etat.panneaux.get(panel.id);
  const pid = estPage(panel) ? (etat.modeles[panel.id.slice(5)] || {}).projet : entree && (entree.params.projet || (etat.modeles[entree.params.modele] || {}).projet);
  const fermer = [{ libelle: t('fermer_panneau'), action: () => { panel.api.close(); ensuite(); } },
    { libelle: t('fermer_autres'), action: () => { group.panels.filter((x) => x.id !== panel.id).forEach((x) => x.api.close()); ensuite(); } }]
    .concat(pid && etat.projets[pid] ? [{ libelle: t('fermer_onglets_projet', { nom: etat.projets[pid].nom }), action: () => { app.fermerVuesProjet(pid); ensuite(); } }] : [],
      [{ libelle: t('fermer_vues'), action: () => { app.fermerVues(); ensuite(); } }]);
  menuContextuel(e.clientX, e.clientY, panel.id === 'explorateur' ? [fermer[0]] : estPage(panel) ? [agrandir, '-', ...fermer]
    : [{ libelle: t('detacher'), action: () => app.detacher(panel.id) }, agrandir, '-', ...fermer]);
}

// The tab in front of a group, with the keyboard on it (its tab list's one Tab stop)
function focusOnglet(group) {
  const o = group && group.element && group.element.querySelector('.dv-tab[aria-selected="true"]');
  if (o) o.focus();
}

// The group of panels the keyboard is in (the innermost: a view's, in a model's page), else the
// main dock's active group.
function groupeEnCours() {
  const a = document.activeElement;
  const dedans = tousDocks().flatMap((d) => d.groups).filter((g) => g.element && g.element.contains(a));
  dedans.sort((x, y) => (x.element.contains(y.element) ? 1 : -1));
  return dedans[0] || (dock && dock.activeGroup) || null;
}

// Ctrl+Tab and Ctrl+Shift+Tab: the next or previous tab of the group in use comes to the front.
app.ongletVoisin = (sens) => {
  const g = groupeEnCours();
  if (!g || g.panels.length < 2) return;
  const i = g.panels.indexOf(g.activePanel);
  g.panels[(i + sens + g.panels.length) % g.panels.length].api.setActive();
  setTimeout(() => focusOnglet(g));
};

// F6 and Shift+F6: from area to area of the window, the toolbar, each group of panels as laid out
// (on its tab in front), the status bar. (The menu bar is Alt's or F10's.)
app.regionVoisine = (sens) => {
  const visible = (x) => x && x.getClientRects().length > 0;
  const groupes = tousDocks().flatMap((d) => d.groups).map((g) => g.element)
    .filter((x) => visible(x) && !x.querySelector('.dv-groupview'))
    .sort((x, y) => (x.compareDocumentPosition(y) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1));
  const regions = [document.getElementById('barre-outils'), ...groupes, document.getElementById('statut')].filter(visible);
  let i = regions.findIndex((r) => r.contains(document.activeElement));
  if (i < 0) i = sens > 0 ? -1 : regions.length;  // (from outside them: the first area, or the last)
  const r = regions[(i + sens + regions.length) % regions.length];
  const cible = r.matches('.dv-groupview') ? r.querySelector('[role="tree"] [tabindex="0"]') || r.querySelector('.dv-tab[aria-selected="true"]')
    : r.querySelector('[tabindex="0"]:not(:disabled)') || r.querySelector('button:not(:disabled), select, a[href]');
  if (cible) cible.focus();
};

// ------------------------------------------------------------ panels

function contexte(entree) {
  return {
    modele: app.modele, projet: app.projet, tousModeles: app.tousModeles, selection: app.selection,
    choisir: app.choisir, variablesChoisies: app.variablesChoisies, choisirVariables: app.choisirVariables,
    groupesHCA: etat.groupesHCA, demander, valeurs: app.valeurs,
    ouvrirVue: app.ouvrirVue, reajuster: app.reajuster, ajusterBaci: app.ajusterBaci, statut,
    rafraichir: () => rendrePanneau(entree),
    definirGraphe: (div) => { entree.graphe = div; },
    grapheCourant: () => entree.graphe,
    menuGraphique: (e, div) => menuGraphique(e, div, entree),
    diffuserGroupes: (id) => diffuser('groupes', { modele: id, groupes: etat.groupesHCA[id] }),
    definirChoixVIP: (id, termes) => { etat.choixVIP[id] = termes; majBarreOutils(); },
    formatPanneau: () => entree.loc,
    predireFichier: app.predireFichier,
    exporterPrediction: app.exporterPrediction,
    nouvelleVariable: app.nouvelleVariable,
    valeursRaster: app.valeursRaster,
    modifierValeur: app.modifierValeur,
    remplacerDonnees: app.remplacerDonnees,
    categoriesVariable: app.categoriesVariable,
    variablesProjet: app.variablesProjet,
    actionsVariable: app.actionsVariable,
    changerDonnees: app.changerDonnees,
    lirePressePapiers: app.lirePressePapiers,
    modeleActif: () => app.modeleActif(),
    projetActif: () => app.projetActif(),
    occupe: (f) => occupe(f),
    apresConsole: async (ids) => { for (const id of ids) if (etat.projets[id]) projetChange(await demander('projet', { projet: id })); },
    noterModele: (id, note) => app.noterModele(id, note),
    nouveauModeleDepuis: (id, reglages) => app.nouveauModeleDepuis(id, reglages),
  };
}

function rendrePanneau(entree) {
  if (entree.vue === 'explorateur') { rendreExplorateur(entree.element, app); return; }
  if (entree.vue === 'accueil') { rendreAccueil(entree.element); return; }
  const v = VUES[entree.vue];
  const m = entree.params.modele ? etat.modeles[entree.params.modele] : null;
  if (!v.global && !v.projet && !m) {
    // (a view's own window whose model left the session: said, rather than its results still shown)
    if (VUE_SEULE && entree.id === 'seul' && entree.params.modele) {
      entree.element.replaceChildren(el('p', { class: 'note' }, t('vue_modele_parti')));
      if (entree.graphe) entree.graphe = null;
    }
    return;
  }
  let rendu;
  try { rendu = v.rendre(entree.element, contexte(entree), entree.params, entree.loc); } catch (e) { rendu = Promise.reject(e); }
  Promise.resolve(rendu).catch((e) => {
    // (Stop pressed while the view computed: said as such, not as an error)
    if (e.reponse && e.reponse.arrete) { entree.element.replaceChildren(el('p', { class: 'infos' }, t('calcul_arrete'))); return; }
    entree.element.replaceChildren(el('p', { class: 'erreur' }, `${t('erreur')}${deuxPoints()}${e.message}`));
  });
  const titre = v.titre(m, etat.projets[entree.params.projet] || {}, entree.loc);
  // (in its model's page, the model's name goes without saying, and the page's tab has the mark)
  if (entree.api) entree.api.setTitle(m ? titreCourt(titre, m) : marqueLecture(entree.params.projet) + titre);
  // (a model's view in a window of its own: its project's name first, "skylark_counts · M1 · Indices")
  if (VUE_SEULE) document.title = marqueLecture(entree.params.projet) + (m && nomProjetCourt(m.projet) ? `${nomProjetCourt(m.projet)} · ` : '') + titre;
}

function rafraichirModele(id) {
  // (a project id: the project views that show its selection)
  const deProjet = (e) => !e.params.modele && e.params.projet === id && !!(VUES[e.vue] || {}).selection;
  for (const e of etat.panneaux.values()) {
    if (!id || e.params.modele === id || e.vue === 'comparaison' || deProjet(e)) if (e.vue !== 'explorateur' && e.vue !== 'accueil') rendrePanneau(e);
  }
}

function toutRedessiner() {
  if (!VUE_SEULE) { rendreMenus(); rendreBarreOutils(); }
  for (const e of etat.panneaux.values()) rendrePanneau(e);
  if (dock) for (const p of dock.panels) if (p.id === 'explorateur') p.api.setTitle(t('explorateur')); else if (p.id === 'accueil') p.api.setTitle(t('titre'));
  majEtat();
}

function menuGraphique(e, div, entree) {
  // (a project view's selection is under the project's id: it can be cleared, the rest needs a model)
  const id = entree.params.modele || ((VUES[entree.vue] || {}).selection ? entree.params.projet : null);
  const modele = !!etat.modeles[id];
  const sel = id ? app.selection(id) : [];
  const vars = id ? app.variablesChoisies(id) : [];
  menuContextuel(e.clientX, e.clientY, [
    { libelle: t('mise_en_forme'), icone: 'palette', action: () => dialogueFormat(app, entree, div) },
    '-',
    { libelle: t('copier_image'), icone: 'copy', action: () => copierImage(div) },
    { libelle: t('image_png'), icone: 'image-down', action: () => enregistrerImage(div, 'png') },
    { libelle: t('image_svg'), action: () => enregistrerImage(div, 'svg') },
    // (figures.js: this plot beside others, for a journal)
    { libelle: t('figure_ajouter'), icone: 'layout-grid', sous: app.sousMenuFigure(entree) },
    '-',
    { libelle: t('effacer_selection'), actif: sel.length + vars.length > 0, action: () => app.effacerSelection(id) },
    { libelle: t('exclure_reajuster'), icone: 'eye-off', actif: modele && sel.length > 0, action: () => app.reajuster(id, { exclues: sel }) },
    { libelle: t('contributions'), icone: 'sliders-horizontal', actif: modele && sel.length > 0 && !etat.modeles[id].regression && !etat.modeles[id].analyse,
      action: () => app.ouvrirVue('contributions', id) },
    { libelle: t('classe_selection'), icone: 'tag', actif: modele && sel.length > 0, action: () => app.classeSelection(id) },
    '-',
    VUE_SEULE ? { libelle: t('rattacher'), icone: 'undo-2', action: rattacher }
      : { libelle: t('detacher'), icone: 'external-link', action: () => app.detacher(entree.id) },
  ]);
}

async function copierImage(div = app.grapheActif()) {
  if (!div) return;
  const url = await window.Plotly.toImage(div, { format: 'png', scale: 2 });
  const blob = await (await fetch(url)).blob();
  await navigator.clipboard.write([new ClipboardItem({ 'image/png': blob })]);
  statut(t('copie'));
}

function enregistrerImage(div = app.grapheActif(), format = 'png') {
  if (!div) return;
  const nom = ((div.layout && div.layout.title && div.layout.title.text) || 'graphique').replace(/[^\p{L}\p{N} _.-]+/gu, '_');
  window.Plotly.downloadImage(div, { format, filename: nom, scale: format === 'png' ? 2 : 1 });
}
app.copierImage = () => copierImage();

// Ctrl+C in a panel that shows a table (and no text selected): the table as tab-separated
// text, for Excel. A table of panneaux.js's tableau() copies all its rows at full precision (its
// copy button), whether it fills the panel or sits under a plot; a view's own copy comes first;
// any other table as shown.
app.tableauActif = () => {
  const p = VUE_SEULE ? { id: 'seul' } : panneauCourant();
  const e = p && etat.panneaux.get(p.id);
  const table = e && e.element && e.element.querySelector('table');
  // (a plot without a table: its numbers, accessibilite.js)
  if (!table) return e && e.graphe && e.graphe.isConnected && e.graphe.data ? () => navigator.clipboard.writeText(donneesGraphe(e.graphe)).then(() => statut(t('graphe_copie'))) : null;
  if (e.element._copierTableau) return e.element._copierTableau;
  for (let x = table.parentElement; x && x !== e.element; x = x.parentElement) if (x._copierTableau) return x._copierTableau;
  return () => {
    const texte = (c) => { const x = c.cloneNode(true); x.querySelectorAll('.marque').forEach((m) => m.remove()); return x.textContent.replace(/[▲▼]/g, '').trim(); };
    navigator.clipboard.writeText([...table.rows].map((r) => [...r.cells].map(texte).join('\t')).join('\n')).then(() => statut(t('table_copiee')));
  };
};
app.copierTableau = () => { const f = app.tableauActif(); if (f) f(); };
// The coefficients side by side of the active panel (Compare models, in that mode: panneaux.js,
// rendreCoteACote), for File › Export model results: { modeles, erreur }, or null.
app.coefficientsActifs = () => {
  const p = VUE_SEULE ? { id: 'seul' } : panneauCourant();
  const e = p && etat.panneaux.get(p.id);
  return (e && e.vue === 'comparaison' && e.element && e.element._coefficients) || null;
};
app.palette = () => ouvrirPalette(app, C);
app.miseEnForme = () => {
  const p = VUE_SEULE ? { id: 'seul' } : panneauCourant();
  const e = p && etat.panneaux.get(p.id);
  if (e && e.graphe) dialogueFormat(app, e, e.graphe);
};
app.redessiner = (entree) => (entree ? rendrePanneau(entree) : toutRedessiner());
app.enregistrerImage = (f) => enregistrerImage(undefined, f);

// ------------------------------------------------------------ chrome

// (ui.js, barreMenus: the mouse, and the keyboard from Alt, F10 or a menu's letter)
function rendreMenus() {
  barreMenus(document.getElementById('menus'), menus(app, C).map((d) => ({ libelle: t(d.cle), items: async () => {
    if (d.cle === 'm_fenetre' && bureau) fenetres = await bureau.fenetres();
    return menus(app, C).find((x) => x.cle === d.cle).items();
  } })), [el('img', { class: 'logo', src: 'nuee.svg', alt: '' })]);
}

// The toolbar is one Tab stop: left and right (Home, End) go from button to button. Its views follow
// the model in front (commandes.js: barreOutils): drawn again when they change.
const vuesBarre = new Map();  // (the commands of views that have none of their own: commandeVue)
const commandeBarre = (id) => C[id] || vuesBarre.get(id) || vuesBarre.set(id, commandeVue(app, id)).get(id);
const signatureBarre = () => barreOutils(app).map((g) => g.join(' ')).join('|');
function rendreBarreOutils() {
  const barre = document.getElementById('barre-outils');
  const avant = barre.contains(document.activeElement) ? document.activeElement.dataset.commande ?? '' : null;
  barre._signature = signatureBarre();
  barre.setAttribute('aria-label', t('barre_outils'));
  barre.replaceChildren(...barreOutils(app).map((groupe) => el('div', { class: 'groupe-outils', role: 'group' }, groupe.map((id) => {
    const c = commandeBarre(id);
    const court = t('court_' + id);  // (a view without a short name: its name)
    return el('button', { class: 'outil', 'data-commande': id, title: t(id) + (c.raccourci ? ` (${afficherRaccourci(c.raccourci)})` : ''), tabindex: -1,
      'aria-keyshortcuts': c.raccourci ? c.raccourci.replace('Ctrl', 'Control').replace('=', 'Plus') : null,
      onclick: () => { if (!c.actif || c.actif()) c.action(); } }, icone(c.icone || ICONES_VUES[id]), el('span', {}, court === `court_${id}` ? t(id) : court));
  }))));
  if (!barre._clavier) {
    barre._clavier = true;
    barre.addEventListener('keydown', (e) => {
      const boutons = [...barre.querySelectorAll('.outil:not(:disabled)')];
      const i = boutons.indexOf(document.activeElement);
      if (i < 0) return;
      const k = { ArrowRight: i + 1, ArrowLeft: i - 1, Home: 0, End: boutons.length - 1 }[e.key];
      if (k === undefined) return;
      e.preventDefault();
      const b = boutons[(k + boutons.length) % boutons.length];
      barre.querySelectorAll('.outil').forEach((x) => { x.tabIndex = x === b ? 0 : -1; });
      b.focus();
    });
  }
  majBarreOutils();
  // (the keyboard on the toolbar while it changed: on the same command, else its Tab stop)
  if (avant !== null) {
    const b = barre.querySelector(`.outil[data-commande="${avant}"]:not(:disabled)`) || barre.querySelector('.outil[tabindex="0"]');
    if (b) b.focus();
  }
}

function majBarreOutils() {
  const barre = document.getElementById('barre-outils');
  if (barre && barre._signature !== undefined && barre._signature !== signatureBarre()) { rendreBarreOutils(); return; }
  document.querySelectorAll('#barre-outils .outil').forEach((b) => {
    const c = commandeBarre(b.dataset.commande);
    b.disabled = c.actif ? !c.actif() : false;
  });
  // (the toolbar's Tab stop on an enabled button)
  const boutons = [...document.querySelectorAll('#barre-outils .outil')];
  if (boutons.length && !boutons.some((b) => b.tabIndex === 0 && !b.disabled)) {
    const b = boutons.find((x) => !x.disabled) || boutons[0];
    boutons.forEach((x) => { x.tabIndex = x === b ? 0 : -1; });
  }
}

// The window's title, which the taskbar and Window's list of windows show: the project in front,
// the one the menus act on (a window of its own, a detached view, has its view's: rendrePanneau)
function majTitre() {
  const p = etat.projets[app.projetActif()];
  const titre = p ? `${t('titre')} — ${p.nom}` : t('titre');
  if (document.title !== titre) document.title = titre;
}

// The files of the saved projects open now, told to main.js as they change: the last session's
// projects at the next start (rouvrirSeance). A window that has had none yet tells nothing, so that
// a start where nothing opens keeps the session before.
function noterSeance() {
  if (!bureau || !bureau.seance) return;
  const chemins = Object.values(etat.projets).filter((p) => p.chemin_enregistrement && !p.lecture_seule).map((p) => p.chemin_enregistrement);
  const cle = JSON.stringify(chemins);
  if (cle === etat.seance.notee || (!chemins.length && etat.seance.notee === null)) return;
  etat.seance.notee = cle;
  bureau.seance.noter(chemins).catch(() => {});
}

function majEtat() {
  if (!VUE_SEULE) majBarreOutils();
  if (!VUE_SEULE) retitrerPages();  // (a project opened or closed: the models' tabs name theirs, or no longer)
  if (!VUE_SEULE) majTitre();
  if (!VUE_SEULE) noterSeance();
  if (!VUE_SEULE && app.barresCode) app.barresCode();  // (a project's R code written elsewhere: vues_code.js)
  if (!VUE_SEULE && app.barresGreffons) app.barresGreffons();  // (a project's plugin not approved here: vues_greffons.js)
  const c = etat.panneaux.get('console:');
  if (c && c.element._console) c.element._console.entete();  // m and p follow the model shown
  const s = document.getElementById('statut');
  if (!s) return;
  const id = app.modeleActif();
  const m = id && etat.modeles[id];
  const n = id ? app.selection(id).length : 0;
  const nv = id ? app.variablesChoisies(id).length : 0;
  // (a view's request that takes long computes too, under Stop: majArret)
  const arret = document.getElementById('arret-calcul');
  const calcule = etat.occupe || !!(arret && !arret.hidden);
  const debut = etat.occupe ? etat.debutOccupe : debutAttente();
  s.replaceChildren(...[
    el('span', { class: 'moteur' + (calcule ? ' occupe' : etat.erreur ? ' erreur' : '') }, el('span', { class: 'point' }),
      calcule ? (debut !== null && Date.now() - debut > 60000 ? t('calcul_long') : t('calcul')) : etat.message || t('pret')),
    el('span', { class: 'espace' }),
    etat.nouvelleVersion && lire('version_ignoree') !== etat.nouvelleVersion.version ? el('span', { class: 'nouvelle-version' },
      el('a', { href: '#', title: etat.nouvelleVersion.page, onclick: (e) => { e.preventDefault(); if (etat.nouvelleVersion.installateur) app.mettreAJour(); else window.open(etat.nouvelleVersion.page); } },
        icone('download'), t(etat.nouvelleVersion.installateur ? 'nouvelle_version_maj' : 'nouvelle_version', { v: etat.nouvelleVersion.version })),
      el('button', { class: 'icone-seule', title: t('ignorer_version'), 'aria-label': t('ignorer_version'), onclick: () => { ecrire('version_ignoree', etat.nouvelleVersion.version); majEtat(); } }, icone('x'))) : null,
    // (a project's data file changed since it was read: etapes.js)
    !VUE_SEULE && app.avisDonnees ? app.avisDonnees() : null,
    // (the active project's versions to send and to get: versions.js)
    !VUE_SEULE && app.indicateurVersions ? app.indicateurVersions() : null,
    // (an example project in front: its tutorial, aide.js)
    !VUE_SEULE && app.lienTutoriel ? app.lienTutoriel(app.projetActif()) : null,
    m ? el('span', {}, `${plusieursProjets() ? `${nomProjetCourt(m.projet)} · ` : ''}${m.nom} · ${m.type}${m.regression || m.analyse ? '' : ` · A = ${m.A}`} · N = ${m.N} · K = ${m.K}`)
      : el('span', { class: 'doux' }, t('aucun_modele_actif')),
    n ? el('span', { class: 'sel' }, `${n} ${t('selectionnees')}`) : null,
    nv ? el('span', { class: 'sel' }, t('variables_en_evidence', { n: nv })) : null,
    el('button', { class: 'icone-seule', title: t('theme_sombre'), 'aria-label': t('theme_sombre'), 'aria-pressed': String(app.themeSombre()),
      onclick: () => app.changerTheme(!app.themeSombre()) }, icone(app.themeSombre() ? 'sun' : 'moon')),
    el('select', { class: 'langue', title: t('langue'), 'aria-label': t('langue'), onchange: (e) => app.changerLangue(e.target.value) },
      [['fr', 'FR'], ['en', 'EN']].map(([v, x]) => el('option', { value: v, selected: v === langueActuelle() }, x)))].filter(Boolean));
  placerArret();
}

function rendreAccueil(conteneur) {
  conteneur.replaceChildren(el('div', { class: 'accueil' },
    el('div', { class: 'accueil-tete' }, el('img', { class: 'logo', src: 'nuee.svg', alt: '' }), el('h1', {}, t('titre')), el('p', {}, t('sous_titre'))),
    el('div', { class: 'accueil-actions' },
      el('button', { class: 'principal', onclick: () => app.ouvrir() }, icone('folder-open'), t('ouvrir')),
      // (the projects open when Nuée last closed: rouvrirSeance)
      etat.seance.precedente.length && !etat.seance.rouverte ? el('button', { onclick: () => app.rouvrirSeance(),
        title: etat.seance.precedente.map((x) => x.chemin).join('\n') }, icone('rotate-ccw'), t('seance_bouton')) : null,
      el('button', { onclick: () => app.coller(), title: t('coller_aide') }, icone('clipboard-paste'), t('coller')),
      el('button', { onclick: () => app.importerSpectres() }, icone('audio-waveform'), t('importer_spectres')),
      el('button', { onclick: (e) => app.menuExemples(e.currentTarget), title: t('projet_exemple_aide') }, icone('apple'), t('projet_exemple'), icone('chevron-down')),
      el('button', { onclick: () => app.nouveauModele(), disabled: !app.projetActif() }, icone('file-plus'), t('nouveau_modele')),
      el('button', { onclick: () => app.quelleAnalyse(), disabled: !app.projetActif() }, icone('signpost'), t('quelle_analyse')),
      el('button', { onclick: () => app.aide('guide') }, icone('circle-help'), t('guide'))),
    recents.length ? el('div', { class: 'accueil-recents' }, el('h3', {}, t('recents')),
      recents.map((r) => el('a', { href: '#', title: r, onclick: (e) => { e.preventDefault(); app.ouvrirChemin(r); } }, icone('folder'), r.split(/[\\/]/).pop(),
        el('span', { class: 'doux' }, r)))) : null,
    el('p', { class: 'note' }, t('aide_vide'))));
}

function accueil() {
  if (!dock.getPanel('accueil')) {
    dock.addPanel({ id: 'accueil', component: 'accueil', title: t('titre'),
      position: dock.getPanel('explorateur') ? { referencePanel: 'explorateur', direction: 'right' } : undefined });
    dock.getPanel('explorateur') && dock.getPanel('explorateur').api.group.api.setSize({ width: 300 });
  }
}

function rattacher() {
  const seul = etat.panneaux.get('seul') || {};
  const loc = suffixeVue(VUE_SEULE, seul.loc) ? seul.loc : null;  // (a document's or a figure's: which one)
  // (a model no longer in the session: nothing to bring back, the window just closes)
  const modele = params.get('modele');
  if (!modele || etat.modeles[modele]) diffuser('rattacher', { vers: params.get('de'), vue: VUE_SEULE, modele, projet: params.get('projet'), loc });
  window.close();
}

// A view's own window following its model or its project to new ids (lierVue): its address (so that
// Reattach and a reload take them), its title, the view drawn again
async function lierVue(entree, { modele = entree.params.modele, projet = entree.params.projet }) {
  if (modele && !etat.modeles[modele]) etat.modeles[modele] = await demander('modele', { modele });
  if (projet && !etat.projets[projet]) etat.projets[projet] = await demander('projet', { projet });
  entree.params = { ...entree.params, modele: modele || null, projet: projet || null };
  params.set('modele', modele || '');
  params.set('projet', projet || '');
  history.replaceState(null, '', `./?${params}`);
  rendrePanneau(entree);
  const titre = document.querySelector('#tete-seul .titre-seul');
  if (titre) titre.textContent = document.title;
}

// A project read again from its file in another window (rechargerProjet): the documents and figures
// open here follow it to its new id, what was typed meanwhile sent there; a view in a window of its
// own too, a model's view once its model's new fit is known (`modeles`: old fit → new), else it says
// its model left.
async function suivreRechargement({ ancien, nouveau, modeles }) {
  // (the project as reopened, known here first: its documents' references are read against it)
  if (!etat.projets[nouveau]) {
    try { projetRecu(await demander('projet', { projet: nouveau })); } catch { return; }
  }
  if (app.reattacherDocuments) await app.reattacherDocuments(ancien, nouveau);
  if (app.reattacherFigures) await app.reattacherFigures(ancien, nouveau);
  const seul = VUE_SEULE && etat.panneaux.get('seul');
  if (!seul) return;
  if (seul.params.modele) {
    if (modeles && modeles[seul.params.modele]) await lierVue(seul, { modele: modeles[seul.params.modele], projet: nouveau });
    else if (modeles) rendrePanneau(seul);
  } else if (seul.params.projet === ancien) await lierVue(seul, { projet: nouveau });
}

// A fit replaced by another of the same model in another window (ajustement_remplace: Update the data,
// an R-code model's code edited, an undo that fits the models again): here too its page shows the new
// fit with its layout, its selection kept (suivreAjustement); a view in a window of its own shows it.
async function surAjustementRemplace({ ancien, nouveau }) {
  if (!etat.modeles[ancien]) return;
  if (!etat.modeles[nouveau]) {
    try { etat.modeles[nouveau] = await demander('modele', { modele: nouveau }); } catch { return; }
  }
  const seul = VUE_SEULE && etat.panneaux.get('seul');
  if (seul) {
    if (seul.params.modele === ancien) await lierVue(seul, { modele: nouveau });
    delete etat.modeles[ancien];
    return;
  }
  suivreAjustement(etat.modeles[nouveau].projet, etat.modeles[ancien], etat.modeles[nouveau]);
  oublierModele(ancien);
}

// What a view's own window asked of this one (demanderPrincipale), done, then answered
async function pourPrincipale(msg) {
  let valeur = null;
  let erreur = null;
  try {
    if (msg.action === 'recharger') valeur = await app.rechargerProjet(msg.projet, { perdre: !!msg.perdre });
    else {
      await synchroniser();
      const m = etat.modeles[msg.modele];
      if (!m) throw new Error(t('vue_modele_parti'));
      if (msg.action === 'ouvrirModele') app.ouvrirModele(m.id);
      else {
        // (a model's dialog: this window comes to the front)
        if (msg.action === 'modifierModele') app.modifierModele(m.id);
        else if (msg.action === 'nouveauModeleDepuis') app.nouveauModeleDepuis(m.id, msg.reglages);
        if (bureau) bureau.focusFenetre('moi');
      }
    }
  } catch (e) { erreur = e.message; }
  diffuser('reponse_principale', { jeton: msg.jeton, valeur, erreur });
}

// ------------------------------------------------------------ messages from other windows

// (the requests between a view's window and its main one answered at once; the other messages one
// at a time, in the order sent: a fit replaced is followed before 'modeles' forgets the old one)
let suiteMessages = Promise.resolve();
canal.onmessage = ({ data: msg }) => {
  // (the hidden page of a document's language follows no window: its language stays its own)
  if (!msg || HORS_ECRAN) return;
  if (msg.type === 'reponse_principale') {
    const a = attentesPrincipale.get(msg.jeton);
    if (!a) return;
    attentesPrincipale.delete(msg.jeton);
    clearTimeout(a.minuterie);
    if (msg.erreur) a.reject(new Error(msg.erreur)); else a.resolve(msg.valeur);
    return;
  }
  if (msg.type === 'pour_principale') {
    if (!VUE_SEULE && msg.vers === ID_FENETRE) pourPrincipale(msg);
    return;
  }
  suiteMessages = suiteMessages.then(() => surMessage(msg)).catch((e) => console.error(e));
};

async function surMessage(msg) {
  if (msg.type === 'selection') {
    etat.selection[msg.modele] = msg.uids;
    rafraichirModele(msg.modele);
    majEtat();
  } else if (msg.type === 'variables') {
    etat.variables[msg.modele] = msg.termes;
    rafraichirModele(msg.modele);
    majEtat();
  } else if (msg.type === 'selection_liee') {
    if (etat.selectionLiee !== msg.lie) app.basculerSelectionLiee(false);
  } else if (msg.type === 'groupes') {
    etat.groupesHCA[msg.modele] = msg.groupes;
    rafraichirModele(msg.modele);
  } else if (msg.type === 'modeles') {
    await synchroniser();
  } else if (msg.type === 'theme') {
    app.changerTheme(msg.sombre, false);
  } else if (msg.type === 'langue') {
    app.changerLangue(msg.l, false);
  } else if (msg.type === 'rattacher' && !VUE_SEULE && msg.vers === ID_FENETRE) {
    await synchroniser();
    // (its model, or its project, gone meanwhile: said, nothing opened)
    if ((msg.modele && !etat.modeles[msg.modele]) || (!msg.modele && msg.projet && !etat.projets[msg.projet])) { statut(t('vue_modele_parti'), true); return; }
    app.ouvrirVue(msg.vue, msg.modele || null, msg.loc || null, msg.projet || null);
    if (bureau) bureau.focusFenetre(null);
  } else if (msg.type === 'document') {
    if (app.surDocument) app.surDocument(msg);  // (a document written in another window: documents.js)
  } else if (msg.type === 'projet_recharge') {
    await suivreRechargement(msg);
  } else if (msg.type === 'ajustement_remplace') {
    await surAjustementRemplace(msg);
  }
}

// ------------------------------------------------------------ keyboard

document.addEventListener('keydown', (e) => {
  if (document.querySelector('.fond-dialogue')) return;
  const combo = combinaison(e);
  const saisie = e.target.closest && e.target.closest('input, textarea, select');
  // (in the Data view, Ctrl+V pastes a block of cells from the cell chosen: panneaux.js, collerCellules)
  const cellules = combo === 'Ctrl+V' && !saisie && e.target.closest && e.target.closest('[data-coller-cellules]');
  if (cellules && cellules._collerCellules) { e.preventDefault(); cellules._collerCellules(); return; }
  const edition = ['Ctrl+Z', 'Ctrl+Y', 'Ctrl+Shift+Z', 'Ctrl+A', 'Ctrl+C', 'Ctrl+V', 'Ctrl+X'];
  if (saisie && !/^(Shift\+)?F\d+$/.test(combo) && (!combo.startsWith('Ctrl') || edition.includes(combo))) return;
  // (a command may have a second shortcut: Redo, Ctrl+Y and Ctrl+Shift+Z)
  const c = Object.values(C).find((x) => x.raccourci === combo || (x.autres || []).includes(combo));
  if (!c) return;
  if (c.actif && !c.actif()) return;
  e.preventDefault();
  c.action();
});

// ------------------------------------------------------------ start

// Before Nuée closes (main.js asks every window): what this window's documents and figures still
// hold, sent after a pause in typing, goes to the engine now, so that it knows it is unsaved.
const viderEditions = () => Promise.all([app.viderDocuments && app.viderDocuments(), app.viderFigures && app.viderFigures()]);

async function demarrerVue() {
  document.body.classList.add('seul');
  const id = params.get('modele');
  if (id) etat.modeles[id] = await demander('modele', { modele: id });
  const pid = params.get('projet') || (id && etat.modeles[id].projet);
  if (pid) etat.projets[pid] = await demander('projet', { projet: pid });
  if (VUES[VUE_SEULE].global) await synchroniser();
  const tete = document.getElementById('tete-seul');
  const element = el('div', { class: 'panneau' });
  let loc = {};
  try { loc = JSON.parse(params.get('loc') || '{}') || {}; } catch { loc = {}; }
  const entree = { id: 'seul', element, vue: VUE_SEULE, params: { modele: id || null, projet: pid || null }, loc };
  tete.replaceChildren(el('span', { class: 'logo' }, icone(ICONES_VUES[VUE_SEULE] || 'sigma')), el('span', { class: 'titre-seul' }),
    el('span', { class: 'espace' }),
    el('button', { class: 'icone-seule', title: t('copier_image'), onclick: () => copierImage() }, icone('copy')),
    el('button', { class: 'icone-seule', title: t('image_png'), onclick: () => enregistrerImage(undefined, 'png') }, icone('image-down')),
    el('button', { title: t('rattacher'), onclick: rattacher }, icone('undo-2'), t('rattacher')));
  document.getElementById('seul').append(element);
  etat.panneaux.set('seul', entree);
  rendrePanneau(entree);
  if (bureau && bureau.surEnregistrerTout) bureau.surEnregistrerTout(() => app.enregistrerTout());  // if it is the last window
  if (bureau && bureau.surVider) bureau.surVider(viderEditions);
  tete.querySelector('.titre-seul').textContent = document.title;
  majEtat();
}

// The hidden page of a document's language (?vue=hors_ecran&langue=…, documents.js: dansLaLangue): a
// model's table or plot made here, in this page's language, for the window that holds it; the model
// and its project asked of the engine as they are now, the view's settings and the model's selection
// given by that window. Nothing else runs here: no menus, no channel (app.js's own above), no hooks.
function demarrerHorsEcran() {
  window.addEventListener('message', async (ev) => {
    if (ev.source !== window.parent || ev.origin !== location.origin || !ev.data || ev.data.type !== 'hors_ecran') return;
    const { id, demande: q } = ev.data;
    let resultat = null;
    let erreur = null;
    try {
      const m = await demander('modele', { modele: q.modele });
      etat.modeles[m.id] = m;
      etat.projets[m.projet] = await demander('projet', { projet: m.projet });
      for (const k of ['selection', 'variables', 'choixVIP', 'groupesHCA']) {
        if (q.etat && q.etat[k]) etat[k][m.id] = q.etat[k]; else delete etat[k][m.id];
      }
      if (q.genre === 'tableau') {
        const x = partiesModele(m).find((p) => p.cle === q.cle);
        resultat = x && x.tableau ? await x.tableau(m) : null;
      } else resultat = await imageVue({ ...app, locVue: () => q.loc || null }, q.vue, m, q.variante || {});
    } catch (e) { erreur = e.message; }
    window.parent.postMessage({ type: 'hors_ecran_fait', id, resultat, erreur }, location.origin);
  });
  window.parent.postMessage({ type: 'hors_ecran_pret' }, location.origin);
}

async function demarrerPrincipal() {
  try { recents = bureau ? await bureau.recents() : JSON.parse(lire('recents') || '[]'); } catch { recents = []; }
  try { etat.seance.precedente = bureau && bureau.seance ? [].concat(await bureau.seance.precedente()) : []; } catch { etat.seance.precedente = []; }
  rendreMenus();
  rendreBarreOutils();
  dock = DV.createDockview(document.getElementById('dock'), optionsDock());
  document.getElementById('dock').addEventListener('contextmenu', menuOnglet);
  dock.onDidActivePanelChange(() => {
    const a = dock.activePanel;
    if (a && !EXTERNES.includes(a.id)) dernierContenu = a.id;
    setTimeout(() => { for (const id of pages.keys()) dimensionnerPage(id); });  // a page coming to the front
    majEtat();
  });
  window.addEventListener('resize', () => setTimeout(() => { for (const id of pages.keys()) dimensionnerPage(id); }));
  dock.onDidRemovePanel(() => {
    if (!restauration && !dock.panels.some((x) => !EXTERNES.includes(x.id))) accueil();
    majEtat();
  });
  dock.addPanel({ id: 'explorateur', component: 'explorateur', title: t('explorateur') });
  accueil();
  await synchroniser();
  if (bureau && bureau.surEnregistrerTout) bureau.surEnregistrerTout(() => app.enregistrerTout());
  if (bureau && bureau.surVider) bureau.surVider(viderEditions);
  if (ID_FENETRE === 'principale') {
    quoiDeNeuf();
    proposerRecuperation();
    app.signalerRepliR();
  }
  if (bureau) setInterval(sauvegarderSecours, 120000);  // (every main window: the first one open writes them)
  setTimeout(prechargerGuide, 3000);  // (the guide read while R waits: F1 then shows it at once)
  if (bureau && bureau.langue) bureau.langue(langueActuelle());  // for the dialogs of the desktop app
  if (bureau && bureau.majAuto) {  // a newer version on GitHub: a link in the status bar, and the update
    etat.majAuto = await bureau.majAuto();
    bureau.nouvelleVersion().then((v) => { etat.nouvelleVersion = v; majEtat(); }).catch(() => {});
    if (bureau.surProgressionMaj) bureau.surProgressionMaj((f) => statut(t('maj_telechargement', { v: etat.nouvelleVersion.version, p: Math.round(100 * f) })));
  }
  // the last session's projects, when File › Settings… says so (not after an unexpected stop: its
  // recovery copies are offered instead, proposerRecuperation), before a file given at start
  if (ID_FENETRE === 'principale' && bureau && bureau.seance && etat.seance.precedente.length && !Object.keys(etat.projets).length
    && await bureau.seance.rouvrir().catch(() => false) && !(await bureau.secoursListe().catch(() => [])).length) await app.rouvrirSeance();
  if (params.get('ouvrir')) await app.ouvrirChemin(params.get('ouvrir'));
  if (!VUE_SEULE) {
    document.addEventListener('dragover', (e) => {
      if (![...e.dataTransfer.types].includes('Files')) return;  // not the panels' own tabs
      e.preventDefault();
      document.body.classList.add('depot');
    });
    document.addEventListener('dragleave', (e) => { if (!e.relatedTarget) document.body.classList.remove('depot'); });
    document.addEventListener('drop', (e) => {
      document.body.classList.remove('depot');
      if (!e.dataTransfer.files.length) return;
      e.preventDefault();
      deposerFichiers([...e.dataTransfer.files]).catch(() => {});
    });
    if (bureau && bureau.surOuvrirFichier) bureau.surOuvrirFichier((chemin) => app.ouvrirChemin(chemin));
  }
}

// Files dropped on the window: spectra (JCAMP-DX, SPC) all together make one project, as
// File › Import spectra; any other file opens as with File › Open.
async function deposerFichiers(fichiers) {
  const ext = (f) => f.name.split('.').pop().toLowerCase();
  const chemin = async (f) => (bureau && bureau.cheminFichier ? bureau.cheminFichier(f)
    : (await (await fetch('/api/deposer?nom=' + encodeURIComponent(f.name), { method: 'POST', body: f })).json()).chemin);
  if (fichiers.every((f) => ['jdx', 'dx', 'jcm', 'spc'].includes(ext(f)))) {
    const chemins = await Promise.all(fichiers.map(chemin));
    const res = await occupe(() => demander('importer_spectres', { chemins, nom: chemins.length === 1 ? fichiers[0].name : t('spectres_importes', { n: chemins.length }) }));
    await apresOuverture(res);
    if ([].concat(res.note || []).length) statut([].concat(res.note).join(' ; '));
    return;
  }
  for (const f of fichiers) {
    if (bureau && bureau.cheminFichier) await app.ouvrirChemin(bureau.cheminFichier(f));
    else {
      const res = await choisirTable(await occupe(() => televerser(f)));
      if (res) await apresOuverture(res);
    }
  }
}

document.documentElement.dataset.theme = lire('theme') === 'sombre' ? 'sombre' : 'clair';
document.documentElement.lang = langueActuelle();
document.title = t('titre');
if (HORS_ECRAN) demarrerHorsEcran(); else if (VUE_SEULE) demarrerVue(); else demarrerPrincipal();
