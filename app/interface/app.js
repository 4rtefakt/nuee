// Main window: menu bar, toolbar, project explorer, dockable panels and status bar.
// ?vue=<view>&modele=<id> shows a single view instead (a panel moved to its own window).
// All windows share the R server's state and keep selections in step over a BroadcastChannel.
import { t, changerLangue as changerLangueTexte, langueActuelle, lire, ecrire } from './i18n.js';
import { demander, televerser, bureau } from './api.js';
import { VUES } from './panneaux.js';
import { el, icone, menuContextuel, ouvrirMenuSous, fermerMenus, dialogue } from './ui.js';
import { creerCommandes, menus, BARRE_OUTILS, ICONES_VUES, combinaison, afficherRaccourci } from './commandes.js';
import { rendreExplorateur } from './explorateur.js';
import { dialogueModele, dialogueAide, dialogueFormat, dialogueVariable } from './dialogues.js';
import { dialogueRapport } from './rapport.js';
import { rendreConsole } from './console.js';
import { NOUVEAUTES, VERSION } from './nouveautes.js';
import { ECRITURE, filtresOuverture, acceptOuverture, filtresSpectres, acceptSpectres } from './formats.js';
import { ouvrirPalette } from './palette.js';

const params = new URLSearchParams(location.search);
const VUE_SEULE = params.get('vue');
const ID_FENETRE = params.get('fenetre') || 'principale';
const canal = new BroadcastChannel('nuee');
const DV = window['dockview-core'];

const etat = { projets: {}, modeles: {}, selection: {}, variables: {}, groupesHCA: {}, choixVIP: {}, locInitial: {}, panneaux: new Map(), noeud: null,
  replies: new Set(), occupe: 0, message: '', erreur: false, selectionLiee: lire('selection_liee') !== '0' };
// The window's panels. `dock` holds the explorer and, in one central group of tabs, a page per
// model and the project views (data, statistics...). A model's page is a dock of its own
// (`pages`) holding that model's views: summary, scores and loadings first, then the others.
let dock = null;
const pages = new Map();           // model id -> dock of its page
const elementsPages = new Map();   // model id -> element of its page
const derniers = new Map();        // model id -> last view used in its page (other than the summary)
const dispositionsAttente = {};    // model id -> saved layout of its page, applied when the page opens
let restauration = false;  // while a saved layout replaces the panels
const EXTERNES = ['explorateur', 'accueil', 'console:'];  // the panels around the central tabs
let dernierContenu = null;  // the central tab last in front (while typing in the console)
VUES.console = { rendre: rendreConsole, titre: () => t('console_r'), global: true };
let recents = [];
let fenetres = [];

const diffuser = (type, donnees = {}) => canal.postMessage({ type, ...donnees });

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
  // the project at once (Édition › Sélection commune aux modèles du projet).
  choisir(id, uids) {
    const u = [...new Set(uids)];
    const m = etat.modeles[id];
    const lies = etat.selectionLiee && m ? app.modelesDe(m.projet).map((x) => x.id) : [];
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
    return n && etat.modeles[n[1]] ? n[1] : null;
  },

  projetActif() {
    const m = app.modeleActif();
    if (m) return etat.modeles[m].projet;
    const n = (etat.noeud || '').split('/')[0];
    if (etat.projets[n]) return n;
    const c = panneauCourant();
    const e = c && etat.panneaux.get(c.id);
    if (e && etat.projets[e.params.projet]) return e.params.projet;
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

  // the example project (made-up data), with its first model open, scores coloured by variety
  async ouvrirExemple() {
    const res = await occupe(() => demander('exemple'));
    await apresOuverture(res);
    if (!res.modeles.length) return;
    const id = await app.calculer(res.id, res.modeles[0].numero);
    const variete = res.variables.find((v) => v.nom === 'variete');
    if (variete) app.ouvrirVue('scores', id, { couleur: 'v' + variete.uid });
  },

  async ouvrirChemin(chemin) {
    // already open (a recent file, a file dropped or double-clicked again): shown, not opened
    // twice, which would make two copies that both save to the same file
    const cle = (c) => (c ? (/^Win/.test(navigator.platform) ? c.replace(/\\/g, '/').toLowerCase() : c) : null);
    const ouvert = Object.values(etat.projets).find((p) => cle(p.chemin_enregistrement) === cle(chemin) || (!p.table && cle(p.chemin) === cle(chemin)));
    if (ouvert) {
      etat.noeud = ouvert.id;
      app.rafraichirExplorateur();
      majEtat();
      statut(t('deja_ouvert', { nom: ouvert.nom }));
      return;
    }
    const res = await choisirTable(await occupe(() => demander('ouvrir', { chemin })));
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
    const p = etat.projets[pid];
    if (!p) { statut(t('choisir_projet'), true); return false; }
    let chemin = !sous && p.chemin_enregistrement ? p.chemin_enregistrement : null;
    if (bureau && !chemin) {
      chemin = await bureau.enregistrerSous(p.nom.replace(/\.[^.]+$/, '') + '.nuee', [{ name: t('format_projet'), extensions: ['nuee'] }]);
      if (!chemin) return false;
    }
    const session = VUE_SEULE ? undefined : JSON.stringify(collecterSession(pid));
    const q = { projet: pid, chemin, session, version: VERSION };
    let r = await occupe(() => demander('enregistrer', q).catch((e) => { if (e.reponse && e.reponse.conflit) return e.reponse; throw e; }));
    if (r.conflit) {  // changed on disk since it was read: overwrite, save elsewhere, or leave it
      const choix = await new Promise((resolve) => dialogue({ titre: t('conflit_titre'),
        contenu: el('p', {}, t('conflit_texte', { nom: chemin.split(/[\\/]/).pop(), date: new Date(r.date * 1000).toLocaleString(langueActuelle()) })),
        boutons: [{ libelle: t('annuler'), action: () => resolve(null) },
          { libelle: t('ecraser'), action: () => resolve('ecraser') },
          { libelle: t('enregistrer_sous_bouton'), principal: true, action: () => resolve('sous') }],
        surFermeture: () => resolve(null) }));
      if (choix === 'sous') return app.enregistrer(pid, true);
      if (choix !== 'ecraser') return false;
      r = await occupe(() => demander('enregistrer', { ...q, ecraser: true }));
    }
    if (!bureau && !chemin) telecharger(r.chemin);
    statut(`${t('enregistre')} ${r.chemin}`);
    await rafraichirProjet(pid);
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
    const choix = await choisirExport(t('exporter_resultats_titre', { nom: m.nom }), m.nom, true);
    if (choix) apresExport(await occupe(() => demander('exporter', { modele: id, ...choix })));
  },
  async exporterDonnees(pid) {
    const p = etat.projets[pid];
    if (!p) { statut(t('choisir_projet'), true); return; }
    const choix = await choisirExport(t('exporter_donnees_titre', { nom: p.nom }), p.nom.replace(/\.[^.]+$/, ''), false);
    if (choix) apresExport(await occupe(() => demander('exporter_donnees', { projet: pid, ...choix })));
  },

  async fermerProjet(pid) {
    const p = etat.projets[pid];
    if (!p) return;
    if (p.modifie && !window.confirm(t('confirmer_fermer_projet', { nom: p.nom }))) return;
    await demander('fermer_projet', { projet: pid });
    oublierProjet(pid);
    diffuser('modeles');
  },

  quitter: () => bureau && bureau.quitter(),

  // ---- models
  async calculer(pid, numero) {
    const res = await occupe(() => demander('ajuster', { projet: pid, numero }));
    await garder(res);
    vuesParDefaut(res.id);
    return res.id;
  },
  nouveauModele(pid) {
    const p = etat.projets[pid || app.projetActif()];
    if (!p) return statut(t('choisir_projet'), true);
    dialogueModele(app, p);
  },
  modifierModele(id) {
    const m = etat.modeles[id];
    if (m) dialogueModele(app, etat.projets[m.projet], m.reglages, t('modifier_modele'));
  },
  // a new model from a model's settings, changed (the Profiles view's wavelength ranges)
  nouveauModeleDepuis(id, reglages) {
    const m = etat.modeles[id];
    if (m) dialogueModele(app, etat.projets[m.projet], reglages);
  },
  async ajusterDefinition(q) {
    if (q.type === 'CLASSES') {  // one PCA per class, then their Coomans plot
      const r = await occupe(() => demander('modeles_classe', q));
      const liste = [].concat(r.modeles);
      for (const res of liste) await garder(res);
      vuesParDefaut(liste[0].id);
      app.ouvrirVue('coomans', liste[0].id);
      return;
    }
    const res = await occupe(() => demander('ajuster', q));
    await garder(res);
    vuesParDefaut(res.id);
  },
  async reajuster(id, q) {
    const res = await occupe(() => demander('reajuster', { modele: id, ...q }));
    if (etat.variables[id]) etat.variables[res.id] = etat.variables[id];  // the refit keeps the highlighted variables
    await garder(res);
    vuesParDefaut(res.id);
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
        }).catch((e) => statut(`${t('erreur')} : ${e.message}`, true));
        return true;
      } }] });
  },
  async annulerAction(pid) {
    const r = await occupe(() => demander('annuler', { projet: pid }));
    projetChange(r.projet);
    statut(t('annule', { quoi: r.etiquette }));
  },
  async noterModele(id, note) {
    let r;
    try { r = await demander('note_modele', { modele: id, note }); } catch (e) { statut(`${t('erreur')} : ${e.message}`, true); return; }
    if (etat.modeles[id]) etat.modeles[id].note = r.note;
    etat.projets[r.projet.id] = r.projet;
    app.rafraichirExplorateur();
    majEtat();
    diffuser('modeles');
  },
  // A new R engine (after it stopped, or on request): the projects reopen with the session they
  // had: from their recovery copy when they had unsaved changes, otherwise from the file they
  // are saved to (or, never saved, the file they came from). Those with neither are named.
  async redemarrerMoteur() {
    if (!bureau) { statut(t('moteur_navigateur'), true); return; }
    let secours = [];
    try { secours = await bureau.secoursListe(); } catch { secours = []; }
    const aRouvrir = [];
    const perdus = [];
    for (const p of Object.values(etat.projets)) {
      const s = p.modifie && secours.find((e) => e.projet === p.id);
      const source = p.chemin_enregistrement || p.chemin;
      const session = dock ? collecterSession(p.id) : null;
      if (s) aRouvrir.push({ nom: p.nom, session, copie: s, q: { chemin: s.fichier, secours: true, origine: s.origine || null, nom: p.nom } });
      else if (source) aRouvrir.push({ nom: p.nom, session, q: p.chemin_enregistrement ? { chemin: source } : { chemin: source, table: p.table || undefined } });
      else perdus.push(p.nom);
    }
    statut(t('moteur_redemarrage'));
    const erreur = await bureau.redemarrerMoteur();
    moteurArrete = false;
    if (erreur) { statut(`${t('erreur')} : ${erreur}`, true); return; }
    if (dock) for (const p of [...dock.panels]) if (!EXTERNES.includes(p.id)) p.api.close();
    for (const k of ['projets', 'modeles', 'selection', 'variables', 'choixVIP', 'groupesHCA']) etat[k] = {};
    app.rafraichirExplorateur();
    derniersSecours.clear();  // the new engine numbers its projects from P1 again
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
    const texte = [`Nuée ${VERSION}${!bureau ? ' (browser)' : d.installe ? ' (installed)' : ' (from the sources)'}`,
      `System: ${d.systeme}`,
      d.electron ? `Electron ${d.electron}, Chromium ${d.chrome}` : null,
      `${v.R}, ${v.plateforme}, ${v.systeme}`,
      `R locale: ${v.locale}`,
      `R packages: ${Object.entries(v.paquets).map(([k, x]) => `${k} ${x}`).join(', ')}`,
      `Interface: ${langueActuelle()}`].filter(Boolean).join('\n');
    dialogue({ titre: t('diagnostic'), contenu: el('div', {}, el('p', { class: 'note' }, t('diagnostic_aide')), el('pre', { class: 'diagnostic' }, texte)),
      boutons: [{ libelle: t('signaler_probleme'), action: () => { window.open('https://github.com/4rtefakt/nuee/issues/new/choose'); return false; } },
        { libelle: t('copier_texte'), action: () => { navigator.clipboard.writeText(texte).then(() => statut(t('texte_copie'))); return true; } },
        { libelle: t('fermer'), principal: true, action: () => true }] });
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
    const choix = await choisirExport(t('exporter_prediction_titre', { nom: m.nom }), `${t('predictions')} ${m.nom}`, false);
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
  // saves them) in one .R script.
  async exporterScript(id) {
    const m = etat.modeles[id];
    const seance = modelesSeance(m.projet);
    const choix = await choisirScript(m.nom, seance.length > 1 && seance.some((x) => x.id === id) ? { modeles: seance, nom: etat.projets[m.projet].nom } : null);
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
    const corps = el('div', { class: 'texte-methodes' });
    const montrer = async () => {
      const r = await occupe(() => demander('methodes', { modele: id, langue, version: VERSION }));
      corps.replaceChildren(...[].concat(r.paragraphes).map((x) => el('p', {}, x)), el('h4', {}, t('references')),
        el('ol', {}, [].concat(r.references).map((x) => el('li', {}, x))));
    };
    const langues = el('div', { class: 'boutons-langue' }, [['fr', 'Français'], ['en', 'English']].map(([l, n]) => el('label', { class: 'champ' },
      el('input', { type: 'radio', name: 'langue-methodes', checked: l === langue, onchange: () => { langue = l; montrer(); } }), el('span', {}, n))));
    await montrer();
    dialogue({ titre: t('texte_methodes_titre', { nom: m.nom }), contenu: el('div', {}, el('p', { class: 'note' }, t('texte_methodes_note')), langues, corps),
      boutons: [{ libelle: t('copier_texte'), action: () => {
        const texte = [...corps.querySelectorAll('p')].map((x) => x.textContent).join('\n\n') + '\n\n'
          + [...corps.querySelectorAll('li')].map((x) => x.textContent).join('\n');
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
    const res = await occupe(() => demander(q.formule !== undefined ? 'variable_calculee' : 'classes_valeurs', { projet: pid, ...q }));
    projetChange(res);
    statut(t('variable_ajoutee', { nom: q.nom }));
  },
  async modifierValeur(pid, observation, variable, valeur) {
    const res = await occupe(() => demander('modifier_valeur', { projet: pid, observation, variable, valeur }));
    projetChange(res);
    if (app.modelesDe(pid).length) statut(t('donnees_modifiees'));
  },
  occupe: (f) => occupe(f),
  statut: (message, erreur) => statut(message, erreur),
  // for the report: a view's settings in its open panel, and a context to draw it off screen
  locVue: (vue, id) => (etat.panneaux.get(`${vue}:${id}`) || {}).loc || null,
  contexteHorsEcran: (loc, surGraphe) => ({ ...contexte({ loc }), rafraichir: () => {}, definirGraphe: surGraphe, menuGraphique: () => {} }),
  version: async () => ({ ...(await demander('version')), application: VERSION, electron: bureau && bureau.versions.electron }),

  // ---- views and windows
  // A view of a model opens in the model's page (as a tab beside the last view used there);
  // a project view (data, statistics...) as a tab of the central group.
  ouvrirVue(vue, id, loc, pid, position) {
    if (VUE_SEULE) return app.ouvrirFenetreVue(vue, id, pid);
    const m = id ? etat.modeles[id] : null;
    const projet = m ? m.projet : pid;
    const cle = `${vue}:${id || projet || ''}`;
    const existant = trouverPanneau(cle);
    if (existant) {
      if (m) ouvrirPage(id).page.api.setActive();
      existant.api.setActive();
      const e = etat.panneaux.get(cle);
      if (e && loc) { Object.assign(e.loc, loc); rendrePanneau(e); }
      return;
    }
    if (loc) etat.locInitial[cle] = loc;
    const titre = VUES[vue].titre(m, etat.projets[projet] || {});
    if (m) {
      const { page, interne } = ouvrirPage(id);
      interne.addPanel({ id: cle, component: vue, title: titreCourt(titre, m), params: { modele: id, projet },
        position: position || positionDansPage(interne, id) });
      page.api.setActive();
    } else {
      dock.addPanel({ id: cle, component: vue, title: titre, params: { modele: id, projet }, position: positionCentrale() });
      fermerAccueil();
    }
  },
  ouvrirModele(id) {
    const page = dock.getPanel(`page:${id}`);
    if (page) page.api.setActive(); else vuesParDefaut(id);
  },

  ouvrirFenetreVue(vue, id, pid) {
    const m = id ? etat.modeles[id] : null;
    const q = new URLSearchParams({ vue, modele: id || '', projet: pid || (m && m.projet) || '', de: ID_FENETRE });
    window.open('./?' + q, '_blank', 'popup,width=1000,height=720');
  },

  detacher(panelId) {
    let p = panelId ? trouverPanneau(panelId) : app.panneauActif();
    if (estPage(p)) p = pages.get(p.id.slice(5)) && pages.get(p.id.slice(5)).activePanel;  // a page: its view in front
    if (!p) return;
    const e = etat.panneaux.get(p.id);
    if (!e) return;
    app.ouvrirFenetreVue(e.vue, e.params.modele, e.params.projet);
    p.api.close();
  },

  fermerPanneau() {
    const p = app.panneauActif() || (dock && dock.activePanel);
    if (p) p.api.close();
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
    toutRedessiner();
  },
  zoom: (d) => bureau && bureau.zoom(d),
  aide: (sujet) => dialogueAide(app, sujet),
  // the full guide: the text file shipped with the desktop app, or on GitHub
  ouvrirGuide() {
    if (bureau && bureau.ouvrirGuide) bureau.ouvrirGuide();
    else window.open(`https://github.com/4rtefakt/nuee/blob/main/${langueActuelle() === 'en' ? 'USER-GUIDE.txt' : 'LISEZ-MOI-APPLICATION.txt'}`);
  },
};

const C = creerCommandes(app);

// ------------------------------------------------------------ state helpers

function statut(message, erreur = false) {
  etat.message = message || '';
  etat.erreur = erreur;
  majEtat();
  const a = document.getElementById('annonce');  // what a screen reader says
  if (a) a.textContent = etat.message;
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
    console.error(e);
    statut(`${t('erreur')} : ${e.message}`, true);
    throw e;
  } finally {
    etat.occupe -= 1;
    if (!etat.occupe) document.body.classList.remove('occupe');
    majEtat();
  }
}

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
// Resolves to the opened project, or null when the choice is cancelled.
function choisirTable(res) {
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
function choisirExport(titre, nom, plusieursTables) {
  return new Promise((resolve) => {
    let choix = lire('format_export');
    if (!ECRITURE.some((f) => f.ext === choix)) choix = 'xlsx';
    const note = el('p', { class: 'note' });
    const maj = () => { note.textContent = plusieursTables && ECRITURE.find((f) => f.ext === choix).plusieurs ? t('export_plusieurs_fichiers') : ''; };
    const liste = el('div', { class: 'formats-export' }, ECRITURE.map((f) => el('label', { class: 'champ' },
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
// seance: { modeles, nom } when the project has other models, offering them all in one .R script.
function choisirScript(nom, seance = null) {
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
        bureau.enregistrerSous(`${choix.seance ? seance.nom : nom}.${r.format}`, [{ name: t(`script_format_${r.format}`), extensions: [r.format] }])
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
  etat.projets[res.id] = res;
  etat.noeud = res.id;
  diffuser('modeles');
  app.rafraichirExplorateur();
  majEtat();
  document.title = `${t('titre')} — ${res.nom}`;
  if (dock && (session || res.session)) await restaurerSession(res.id, session);
}

// ------------------------------------------------------------ recovery copies (desktop app)

// Every two minutes, a copy of each project with unsaved changes, with its session, goes to
// the application's recovery folder (deleted when the application closes normally). After an
// unexpected stop, the next start offers to reopen them. The index lists the copies to keep
// (main.js deletes the others): those of this launch, named after it so that they never
// replace one of an earlier stop, and those of an earlier stop that haven't been recovered or
// set aside yet (secoursEnAttente). A project whose copy fails this time keeps its last one.
// One main window writes them: the first one open (several main windows share the projects).
const LANCEMENT = Date.now().toString(36);
let secoursEnAttente = [];
const derniersSecours = new Map();
async function sauvegarderSecours() {
  if (!bureau || VUE_SEULE || !dock) return;
  try {
    const principales = (await bureau.fenetres()).filter((f) => f.principale).sort((a, b) => a.id - b.id);
    if (principales.length && !principales[0].actuelle) return;
    const dossier = await bureau.secoursDossier();
    const entrees = [];
    for (const p of Object.values(etat.projets)) {
      if (!p.modifie) continue;
      const fichier = `${dossier}/${LANCEMENT}-${p.id}.json`;
      try {
        await demander('sauvegarde_secours', { projet: p.id, chemin: fichier, session: JSON.stringify(collecterSession(p.id)), version: VERSION });
        derniersSecours.set(p.id, { fichier, projet: p.id, nom: p.nom, origine: p.chemin_enregistrement || p.chemin || null, date: new Date().toISOString() });
      } catch { /* this one is tried again two minutes later; the others go on */ }
      if (derniersSecours.has(p.id)) entrees.push(derniersSecours.get(p.id));
    }
    await bureau.secoursIndex([...secoursEnAttente, ...entrees]);
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
  const quand = (d) => new Date(d).toLocaleString(langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB', { dateStyle: 'medium', timeStyle: 'short' });
  dialogue({ titre: t('recuperation'),
    contenu: el('div', {}, el('p', {}, t('recuperation_question')), el('ul', {}, entrees.map((e) => el('li', {}, `${e.nom} — ${quand(e.date)}`)))),
    boutons: [
      { libelle: t('ignorer'), action: () => { secoursEnAttente = []; bureau.secoursIndex([]); return true; } },
      { libelle: t('recuperer'), principal: true, action: () => {
        (async () => {
          const illisibles = [];
          for (const e of entrees) {
            // before 1.0, a project without a file had its own copy as its origin
            const origine = e.origine && e.origine !== e.fichier ? e.origine : null;
            try { await apresOuverture(await occupe(() => demander('ouvrir', { chemin: e.fichier, secours: true, origine, nom: e.nom }))); } catch { illisibles.push(e); }
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
  const liste = tout ? NOUVEAUTES : NOUVEAUTES.filter((n) => !vue || plusRecente(n.version, vue));
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
  const aMoi = (e) => e.params.projet === pid && (!e.params.modele || code.has(e.params.modele));
  const codeVue = (e) => (e.params.modele ? code.get(e.params.modele) : null);
  const vues = panneaux.filter(aMoi).map((e) => ({ vue: e.vue, modele: codeVue(e), loc: locEnregistrable(e.loc) }));
  let disposition = null;
  const externes = dock.panels.filter((p) => !EXTERNES.includes(p.id));
  const externeAMoi = (p) => (estPage(p) ? code.has(p.id.slice(5)) : !!etat.panneaux.get(p.id) && aMoi(etat.panneaux.get(p.id)));
  if (externes.length && externes.every(externeAMoi)) {  // the layout too, when it only shows this project
    const ids = new Map(panneaux.map((e) => [e.id, `${e.vue}:${codeVue(e) || '@P'}`]));
    for (const p of externes) if (estPage(p)) ids.set(p.id, `page:${code.get(p.id.slice(5))}`);
    const internes = {};
    for (const [mid, d] of pages) if (code.has(mid)) internes[code.get(mid)] = remplacerIds(d.toJSON(), (s) => ids.get(s));
    disposition = { externe: remplacerIds(dock.toJSON(), (s) => ids.get(s)), internes };
  }
  const parModele = (o) => Object.fromEntries(Object.entries(o).filter(([id]) => code.has(id)).map(([id, v]) => [code.get(id), v]));
  return { version: 2, modeles: [...parNumero.values()].map((m) => ({ numero: m.numero, A: m.A })), vues, disposition,
    selection: parModele(etat.selection), variables: parModele(etat.variables), choixVIP: parModele(etat.choixVIP), groupesHCA: parModele(etat.groupesHCA) };
}

async function restaurerSession(pid, donnee = null) {
  let s = donnee;
  if (!s) try { s = JSON.parse((await demander('session', { projet: pid })).session); } catch { return; }
  if (!s || !s.modeles || !s.modeles.length) return;
  const ids = {};
  await occupe(async () => {
    for (const m of s.modeles) {
      try {
        const res = await demander('ajuster', { projet: pid, numero: m.numero, A: m.A });
        etat.modeles[res.id] = res;
        ids['@' + m.numero] = res.id;
      } catch { /* a model that no longer fits is left out */ }
    }
  });
  await rafraichirProjet(pid);
  diffuser('modeles');
  for (const cle of ['selection', 'variables', 'choixVIP', 'groupesHCA']) {
    for (const [code, v] of Object.entries(s[cle] || {})) if (ids[code]) etat[cle][ids[code]] = v;
  }
  const idPanneau = (code, vue) => `${vue}:${code === '@P' || !code ? pid : ids[code]}`;
  const vues = (s.vues || []).filter((v) => !v.modele || ids[v.modele]);
  for (const v of vues) etat.locInitial[idPanneau(v.modele, v.vue)] = v.loc;
  let fait = false;
  const libre = !dock.panels.some((p) => !EXTERNES.includes(p.id));
  if (s.disposition && s.disposition.externe && libre && vues.length === s.vues.length) {
    const nouvel = (x) => {
      const r = /^([a-z_0-9]+):(@P|@\d+)$/.exec(x);
      if (!r) return undefined;
      return r[1] === 'page' ? `page:${ids[r[2]]}` : idPanneau(r[2], r[1]);
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
  for (const v of vues) delete etat.locInitial[idPanneau(v.modele, v.vue)];
  for (const k of Object.keys(dispositionsAttente)) delete dispositionsAttente[k];
  app.rafraichirExplorateur();
  rafraichirModele(null);
  statut(t('session_restauree', { n: Object.keys(ids).length }));
}

// After the project's data changed: its summary, the explorer, the colour and label lists,
// and the views that keep a copy of its data (data table, variable statistics).
function projetChange(res) {
  etat.projets[res.id] = res;
  for (const e of etat.panneaux.values()) {
    if (e.params.projet === res.id && ['donnees', 'statistiques'].includes(e.vue)) {
      delete e.loc.d;
      delete e.loc.res;
      delete e.loc.cleValeurs;
    }
  }
  app.rafraichirExplorateur();
  rafraichirModele(null);
  diffuser('modeles');
  majEtat();
}

async function rafraichirProjet(pid) {
  etat.projets[pid] = await demander('projet', { projet: pid });
  app.rafraichirExplorateur();
}

async function garder(res) {
  etat.modeles[res.id] = res;
  if (etat.selectionLiee) {  // a new model of the project takes up its current selection
    const autre = app.modelesDe(res.projet).find((m) => m.id !== res.id && (etat.selection[m.id] || []).length);
    const dans = new Set(res.observations.uid);
    if (autre) etat.selection[res.id] = etat.selection[autre.id].filter((u) => dans.has(u));
  }
  etat.noeud = `${res.projet}/m/${res.id}`;
  etat.replies.delete(`${res.projet}/calcules`);
  await rafraichirProjet(res.projet);
  diffuser('modeles');
  majEtat();
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
  for (const p of [...dock.panels]) {
    const e = etat.panneaux.get(p.id);
    if ((e && e.params.projet === pid) || (estPage(p) && !etat.modeles[p.id.slice(5)])) p.api.close();
  }
  app.rafraichirExplorateur();
  majEtat();
}

// Everything the R server holds (for a new window, or after another window changed it).
async function synchroniser() {
  const e = await demander('etat');
  etat.projets = Object.fromEntries(e.projets.map((p) => [p.id, p]));
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
// other. The page is a new tab of the central group.
function vuesParDefaut(id) {
  const { page, interne } = ouvrirPage(id);
  const r = `resume:${id}`;
  page.api.setActive();
  dimensionnerPage(id);  // split at the page's real size
  if (!interne.getPanel(r)) {
    app.ouvrirVue('resume', id, null, null, interne.panels.length ? { referencePanel: interne.panels[0].id, direction: 'left' } : undefined);
  }
  if (!interne.getPanel(`scores:${id}`)) app.ouvrirVue('scores', id, null, null, { referencePanel: r, direction: 'right' });
  // spectra: their loadings as lines rather than a scatter of hundreds of variables
  const vue = [].concat((etat.modeles[id].reglages || {}).spectres || []).length ? 'profils' : 'loadings';
  if (!interne.getPanel(`${vue}:${id}`)) app.ouvrirVue(vue, id, null, null, { referencePanel: `scores:${id}`, direction: 'below' });
  interne.getPanel(r).api.setActive();
  derniers.set(id, `scores:${id}`);  // other views join the scores as tabs
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
const titrePage = (m) => `${m.nom} · ${m.type}`;
const titreCourt = (titre, m) => (m && titre.startsWith(`${m.nom} · `) ? titre.slice(m.nom.length + 3) : titre);

function trouverPanneau(id) {
  for (const d of tousDocks()) {
    const p = d.getPanel(id);
    if (p) return p;
  }
  return null;
}

// The view in focus: the view in front in the active page, or the active project view.
function panneauCourant() {
  let p = dock && dock.activePanel;
  if (p && p.id === 'console:') p = dernierContenu && dock.getPanel(dernierContenu);
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
    dispose() { if (etat.panneaux.get(options.id) === entree) etat.panneaux.delete(options.id); },
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
        el('button', { class: 'icone-seule', title: t('detacher'), onclick: () => groupe.activePanel && app.detacher(groupe.activePanel.id) }, icone('external-link')),
        el('button', { class: 'icone-seule', title: t('agrandir'), onclick: () => (groupe.api.isMaximized() ? groupe.api.exitMaximized() : groupe.api.maximize()) }, icone('maximize-2')));
    },
    dispose() {},
  };
}

function optionsDock() {
  const agrandir = (group) => ({ label: group.api.isMaximized() ? t('restaurer') : t('agrandir'),
    action: () => (group.api.isMaximized() ? group.api.exitMaximized() : group.api.maximize()) });
  const fermer = (panel, group) => [{ label: t('fermer_panneau'), action: () => panel.api.close() },
    { label: t('fermer_autres'), action: () => group.panels.filter((x) => x.id !== panel.id).forEach((x) => x.api.close()) }];
  return {
    theme: app.themeSombre() ? DV.themeDark : DV.themeLight,
    createComponent: (options) => (options.name === 'page' ? composantPage(options) : composantVue(options)),
    createRightHeaderActionComponent: actionsGroupe,
    getTabContextMenuItems: ({ panel, group }) => {
      if (panel.id === 'explorateur') return [{ label: t('fermer_panneau'), action: () => panel.api.close() }];
      if (estPage(panel)) return [agrandir(group), 'separator', ...fermer(panel, group)];
      return [{ label: t('detacher'), action: () => app.detacher(panel.id) }, agrandir(group), 'separator', ...fermer(panel, group)];
    },
  };
}

// ------------------------------------------------------------ panels

function contexte(entree) {
  return {
    modele: app.modele, projet: app.projet, tousModeles: app.tousModeles, selection: app.selection,
    choisir: app.choisir, variablesChoisies: app.variablesChoisies, choisirVariables: app.choisirVariables,
    groupesHCA: etat.groupesHCA, demander, valeurs: app.valeurs,
    ouvrirVue: app.ouvrirVue, reajuster: app.reajuster, statut,
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
    modifierValeur: app.modifierValeur,
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
  if (!v.global && !v.projet && !m) return;
  let rendu;
  try { rendu = v.rendre(entree.element, contexte(entree), entree.params, entree.loc); } catch (e) { rendu = Promise.reject(e); }
  Promise.resolve(rendu).catch((e) => {
    entree.element.replaceChildren(el('p', { class: 'erreur' }, `${t('erreur')} : ${e.message}`));
  });
  const titre = v.titre(m, etat.projets[entree.params.projet] || {});
  if (entree.api) entree.api.setTitle(m ? titreCourt(titre, m) : titre);  // in its model's page, the model's name goes without saying
  if (VUE_SEULE) document.title = titre;
}

function rafraichirModele(id) {
  for (const e of etat.panneaux.values()) {
    if (!id || e.params.modele === id || e.vue === 'comparaison') if (e.vue !== 'explorateur' && e.vue !== 'accueil') rendrePanneau(e);
  }
}

function toutRedessiner() {
  document.title = t('titre');
  if (!VUE_SEULE) { rendreMenus(); rendreBarreOutils(); }
  for (const e of etat.panneaux.values()) rendrePanneau(e);
  if (dock) for (const p of dock.panels) if (p.id === 'explorateur') p.api.setTitle(t('explorateur')); else if (p.id === 'accueil') p.api.setTitle(t('titre'));
  majEtat();
}

function menuGraphique(e, div, entree) {
  const id = entree.params.modele;
  const sel = id ? app.selection(id) : [];
  const vars = id ? app.variablesChoisies(id) : [];
  menuContextuel(e.clientX, e.clientY, [
    { libelle: t('mise_en_forme'), icone: 'palette', action: () => dialogueFormat(app, entree, div) },
    '-',
    { libelle: t('copier_image'), icone: 'copy', action: () => copierImage(div) },
    { libelle: t('image_png'), icone: 'image-down', action: () => enregistrerImage(div, 'png') },
    { libelle: t('image_svg'), action: () => enregistrerImage(div, 'svg') },
    '-',
    { libelle: t('effacer_selection'), actif: sel.length + vars.length > 0, action: () => app.effacerSelection(id) },
    { libelle: t('exclure_reajuster'), icone: 'eye-off', actif: sel.length > 0, action: () => app.reajuster(id, { exclues: sel }) },
    { libelle: t('contributions'), icone: 'sliders-horizontal', actif: sel.length > 0, action: () => app.ouvrirVue('contributions', id) },
    { libelle: t('classe_selection'), icone: 'tag', actif: sel.length > 0, action: () => app.classeSelection(id) },
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
// text, for Excel. A list copies all its rows at full precision (its copy button), any other
// table as shown.
app.tableauActif = () => {
  const p = VUE_SEULE ? { id: 'seul' } : panneauCourant();
  const e = p && etat.panneaux.get(p.id);
  const table = e && e.element && e.element.querySelector('table');
  if (!table) return null;
  if (e.element._copierTableau) return e.element._copierTableau;
  return () => {
    const texte = (c) => { const x = c.cloneNode(true); x.querySelectorAll('.marque').forEach((m) => m.remove()); return x.textContent.replace(/[▲▼]/g, '').trim(); };
    navigator.clipboard.writeText([...table.rows].map((r) => [...r.cells].map(texte).join('\t')).join('\n')).then(() => statut(t('table_copiee')));
  };
};
app.copierTableau = () => { const f = app.tableauActif(); if (f) f(); };
app.palette = () => ouvrirPalette(app, C);
app.miseEnForme = () => {
  const p = VUE_SEULE ? { id: 'seul' } : panneauCourant();
  const e = p && etat.panneaux.get(p.id);
  if (e && e.graphe) dialogueFormat(app, e, e.graphe);
};
app.redessiner = (entree) => (entree ? rendrePanneau(entree) : toutRedessiner());
app.enregistrerImage = (f) => enregistrerImage(undefined, f);

// ------------------------------------------------------------ chrome

function rendreMenus() {
  const nav = document.getElementById('menus');
  nav.replaceChildren(el('img', { class: 'logo', src: 'nuee.svg', alt: '' }), ...menus(app, C).map((d) => {
    const b = el('button', { class: 'menu-haut' }, t(d.cle));
    const ouvrir = async () => {
      if (d.cle === 'm_fenetre' && bureau) fenetres = await bureau.fenetres();
      ouvrirMenuSous(b, menus(app, C).find((x) => x.cle === d.cle).items());
    };
    b.addEventListener('mousedown', (e) => {
      e.preventDefault();
      if (b.classList.contains('ouvert')) fermerMenus(); else ouvrir();
    });
    b.addEventListener('mouseenter', () => { if (document.querySelector('.menu-haut.ouvert') && !b.classList.contains('ouvert')) ouvrir(); });
    return b;
  }));
}

function rendreBarreOutils() {
  const barre = document.getElementById('barre-outils');
  barre.replaceChildren(...BARRE_OUTILS.map((groupe) => el('div', { class: 'groupe-outils' }, groupe.map((id) => {
    const c = C[id];
    return el('button', { class: 'outil', 'data-commande': id, title: t(id) + (c.raccourci ? ` (${afficherRaccourci(c.raccourci)})` : ''),
      onclick: () => { if (!c.actif || c.actif()) c.action(); } }, icone(c.icone || ICONES_VUES[id]), el('span', {}, t('court_' + id)));
  }))));
  majBarreOutils();
}

function majBarreOutils() {
  document.querySelectorAll('#barre-outils .outil').forEach((b) => {
    const c = C[b.dataset.commande];
    b.disabled = c.actif ? !c.actif() : false;
  });
}

function majEtat() {
  if (!VUE_SEULE) majBarreOutils();
  const c = etat.panneaux.get('console:');
  if (c && c.element._console) c.element._console.entete();  // m and p follow the model shown
  const s = document.getElementById('statut');
  if (!s) return;
  const id = app.modeleActif();
  const m = id && etat.modeles[id];
  const n = id ? app.selection(id).length : 0;
  const nv = id ? app.variablesChoisies(id).length : 0;
  s.replaceChildren(...[
    el('span', { class: 'moteur' + (etat.occupe ? ' occupe' : etat.erreur ? ' erreur' : '') }, el('span', { class: 'point' }),
      etat.occupe ? (Date.now() - etat.debutOccupe > 60000 ? t('calcul_long') : t('calcul')) : etat.message || t('pret')),
    el('span', { class: 'espace' }),
    etat.nouvelleVersion && lire('version_ignoree') !== etat.nouvelleVersion.version ? el('span', { class: 'nouvelle-version' },
      el('a', { href: '#', title: etat.nouvelleVersion.page, onclick: (e) => { e.preventDefault(); if (etat.nouvelleVersion.installateur) app.mettreAJour(); else window.open(etat.nouvelleVersion.page); } },
        icone('download'), t(etat.nouvelleVersion.installateur ? 'nouvelle_version_maj' : 'nouvelle_version', { v: etat.nouvelleVersion.version })),
      el('button', { class: 'icone-seule', title: t('ignorer_version'), onclick: () => { ecrire('version_ignoree', etat.nouvelleVersion.version); majEtat(); } }, icone('x'))) : null,
    m ? el('span', {}, `${m.nom} · ${m.type} · A = ${m.A} · N = ${m.N} · K = ${m.K}`) : el('span', { class: 'doux' }, t('aucun_modele_actif')),
    n ? el('span', { class: 'sel' }, `${n} ${t('selectionnees')}`) : null,
    nv ? el('span', { class: 'sel' }, t('variables_en_evidence', { n: nv })) : null,
    el('button', { class: 'icone-seule', title: t('theme_sombre'), onclick: () => app.changerTheme(!app.themeSombre()) }, icone(app.themeSombre() ? 'sun' : 'moon')),
    el('select', { class: 'langue', title: t('langue'), onchange: (e) => app.changerLangue(e.target.value) },
      [['fr', 'FR'], ['en', 'EN']].map(([v, x]) => el('option', { value: v, selected: v === langueActuelle() }, x)))].filter(Boolean));
}

function rendreAccueil(conteneur) {
  conteneur.replaceChildren(el('div', { class: 'accueil' },
    el('div', { class: 'accueil-tete' }, el('img', { class: 'logo', src: 'nuee.svg', alt: '' }), el('h1', {}, t('titre')), el('p', {}, t('sous_titre'))),
    el('div', { class: 'accueil-actions' },
      el('button', { class: 'principal', onclick: () => app.ouvrir() }, icone('folder-open'), t('ouvrir')),
      el('button', { onclick: () => app.coller(), title: t('coller_aide') }, icone('clipboard-paste'), t('coller')),
      el('button', { onclick: () => app.importerSpectres() }, icone('audio-waveform'), t('importer_spectres')),
      el('button', { onclick: () => app.ouvrirExemple(), title: t('projet_exemple_aide') }, icone('apple'), t('projet_exemple')),
      el('button', { onclick: () => app.nouveauModele(), disabled: !app.projetActif() }, icone('file-plus'), t('nouveau_modele')),
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
  diffuser('rattacher', { vers: params.get('de'), vue: VUE_SEULE, modele: params.get('modele'), projet: params.get('projet') });
  window.close();
}

// ------------------------------------------------------------ messages from other windows

canal.onmessage = async ({ data: msg }) => {
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
    app.ouvrirVue(msg.vue, msg.modele || null, null, msg.projet || null);
    if (bureau) bureau.focusFenetre(null);
  }
};

// ------------------------------------------------------------ keyboard

document.addEventListener('keydown', (e) => {
  if (document.querySelector('.fond-dialogue')) return;
  const combo = combinaison(e);
  const saisie = e.target.closest && e.target.closest('input, textarea, select');
  const edition = ['Ctrl+Z', 'Ctrl+Y', 'Ctrl+A', 'Ctrl+C', 'Ctrl+V', 'Ctrl+X'];
  if (saisie && !/^F\d+$/.test(combo) && (!combo.startsWith('Ctrl') || edition.includes(combo))) return;
  const c = Object.values(C).find((x) => x.raccourci === combo);
  if (!c) return;
  if (c.actif && !c.actif()) return;
  e.preventDefault();
  c.action();
});

// ------------------------------------------------------------ start

async function demarrerVue() {
  document.body.classList.add('seul');
  const id = params.get('modele');
  if (id) etat.modeles[id] = await demander('modele', { modele: id });
  const pid = params.get('projet') || (id && etat.modeles[id].projet);
  if (pid) etat.projets[pid] = await demander('projet', { projet: pid });
  if (VUES[VUE_SEULE].global) await synchroniser();
  const tete = document.getElementById('tete-seul');
  const element = el('div', { class: 'panneau' });
  const entree = { id: 'seul', element, vue: VUE_SEULE, params: { modele: id || null, projet: pid || null }, loc: {} };
  tete.replaceChildren(el('span', { class: 'logo' }, icone(ICONES_VUES[VUE_SEULE] || 'sigma')), el('span', { class: 'titre-seul' }),
    el('span', { class: 'espace' }),
    el('button', { class: 'icone-seule', title: t('copier_image'), onclick: () => copierImage() }, icone('copy')),
    el('button', { class: 'icone-seule', title: t('image_png'), onclick: () => enregistrerImage(undefined, 'png') }, icone('image-down')),
    el('button', { title: t('rattacher'), onclick: rattacher }, icone('undo-2'), t('rattacher')));
  document.getElementById('seul').append(element);
  etat.panneaux.set('seul', entree);
  rendrePanneau(entree);
  if (bureau && bureau.surEnregistrerTout) bureau.surEnregistrerTout(() => app.enregistrerTout());  // if it is the last window
  tete.querySelector('.titre-seul').textContent = document.title;
  majEtat();
}

async function demarrerPrincipal() {
  try { recents = bureau ? await bureau.recents() : JSON.parse(lire('recents') || '[]'); } catch { recents = []; }
  rendreMenus();
  rendreBarreOutils();
  dock = DV.createDockview(document.getElementById('dock'), optionsDock());
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
  if (ID_FENETRE === 'principale') {
    quoiDeNeuf();
    proposerRecuperation();
    if (bureau) setInterval(sauvegarderSecours, 120000);
  }
  if (bureau && bureau.langue) bureau.langue(langueActuelle());  // for the dialogs of the desktop app
  if (bureau && bureau.majAuto) {  // a newer version on GitHub: a link in the status bar, and the update
    etat.majAuto = await bureau.majAuto();
    bureau.nouvelleVersion().then((v) => { etat.nouvelleVersion = v; majEtat(); }).catch(() => {});
    if (bureau.surProgressionMaj) bureau.surProgressionMaj((f) => statut(t('maj_telechargement', { v: etat.nouvelleVersion.version, p: Math.round(100 * f) })));
  }
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
if (VUE_SEULE) demarrerVue(); else demarrerPrincipal();
