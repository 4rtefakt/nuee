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

const params = new URLSearchParams(location.search);
const VUE_SEULE = params.get('vue');
const ID_FENETRE = params.get('fenetre') || 'principale';
const canal = new BroadcastChannel('nuee');
const DV = window['dockview-core'];

const etat = { projets: {}, modeles: {}, selection: {}, groupesHCA: {}, choixVIP: {}, locInitial: {}, panneaux: new Map(), noeud: null,
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
      const chemin = await bureau.choisirFichier();
      if (chemin) await app.ouvrirChemin(chemin);
      return;
    }
    const f = await new Promise((resolve) => {
      const i = el('input', { type: 'file', accept: '.usp,.json,.csv,.txt,.xlsx', onchange: () => resolve(i.files[0]) });
      i.click();
    });
    if (f) await apresOuverture(await occupe(() => televerser(f)));
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
    const res = await occupe(() => demander('ouvrir', { chemin }));
    await apresOuverture(res);
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
      chemin = await bureau.enregistrerSous(p.nom.replace(/\.[^.]+$/, '') + '.json', [{ name: 'Projet', extensions: ['json'] }]);
      if (!chemin) return false;
    }
    const session = VUE_SEULE ? undefined : JSON.stringify(collecterSession(pid));
    const r = await occupe(() => demander('enregistrer', { projet: pid, chemin, session }));
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

  async exporter(id) {
    const m = etat.modeles[id];
    if (!m) return;
    if (bureau) {
      const chemin = await bureau.enregistrerSous(`${m.nom}.xlsx`, [{ name: 'Excel', extensions: ['xlsx'] }]);
      if (!chemin) return;
      await occupe(() => demander('exporter', { modele: id, chemin }));
      statut(`${t('exporte')} ${chemin}`);
    } else {
      telecharger((await occupe(() => demander('exporter', { modele: id }))).chemin);
    }
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
  async ajusterDefinition(q) {
    const res = await occupe(() => demander('ajuster', q));
    await garder(res);
    vuesParDefaut(res.id);
  },
  async reajuster(id, q) {
    const res = await occupe(() => demander('reajuster', { modele: id, ...q }));
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
    const r = await demander('note_modele', { modele: id, note });
    if (etat.modeles[id]) etat.modeles[id].note = r.note;
    etat.projets[r.projet.id] = r.projet;
    app.rafraichirExplorateur();
    majEtat();
    diffuser('modeles');
  },
  // A new R engine (after it stopped, or on request): the projects reopen, from their recovery
  // copy when they had unsaved changes, with the session they had.
  async redemarrerMoteur() {
    if (!bureau) { statut(t('moteur_navigateur'), true); return; }
    let secours = [];
    try { secours = await bureau.secoursListe(); } catch { secours = []; }
    const aRouvrir = Object.values(etat.projets).filter((p) => p.chemin).map((p) => {
      const s = p.modifie && secours.find((e) => e.origine === p.chemin);
      return { chemin: s ? s.fichier : p.chemin, origine: s ? p.chemin : null, nom: p.nom, session: dock ? collecterSession(p.id) : null };
    });
    statut(t('moteur_redemarrage'));
    const erreur = await bureau.redemarrerMoteur();
    moteurArrete = false;
    if (erreur) { statut(`${t('erreur')} : ${erreur}`, true); return; }
    if (dock) for (const p of [...dock.panels]) if (!EXTERNES.includes(p.id)) p.api.close();
    for (const k of ['projets', 'modeles', 'selection', 'choixVIP', 'groupesHCA']) etat[k] = {};
    app.rafraichirExplorateur();
    for (const r of aRouvrir) {
      try {
        const res = await occupe(() => demander('ouvrir', r.origine ? { chemin: r.chemin, origine: r.origine, nom: r.nom } : { chemin: r.chemin }));
        await apresOuverture(res, r.session);
      } catch { /* a project that cannot be read any more stays closed */ }
    }
    diffuser('modeles');
    statut(t('moteur_redemarre', { n: aRouvrir.length }));
  },
  confirmerRedemarrage() {
    dialogue({ titre: t('redemarrer_moteur'), contenu: el('p', {}, t('redemarrer_confirmation')),
      boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('redemarrer'), principal: true, action: () => { app.redemarrerMoteur(); return true; } }] });
  },
  quoiDeNeuf: (tout) => quoiDeNeuf(tout),
  valeurs: (projet, variable, observations, identifiant) => demander('valeurs', { projet, variable, observations, identifiant }),

  // ---- second batch: predictions from a file, score variables, classes, R scripts
  async predireFichier(id) {
    let chemin;
    if (bureau) {
      chemin = await bureau.choisirFichier();
    } else {
      const f = await choisirFichierNavigateur('.csv,.txt,.xlsx');
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
    if (bureau) {
      const chemin = await bureau.enregistrerSous(`${t('predictions')} ${m.nom}.xlsx`, [{ name: 'Excel', extensions: ['xlsx'] }]);
      if (!chemin) return;
      await occupe(() => demander('exporter_prediction', { modele: id, chemin }));
      statut(`${t('exporte')} ${chemin}`);
    } else {
      telecharger((await occupe(() => demander('exporter_prediction', { modele: id }))).chemin);
    }
  },
  async scoresEnVariables(id) {
    const r = await occupe(() => demander('scores_en_variables', { modele: id }));
    projetChange(r.projet);
    const noms = [].concat(r.noms);
    const modeles = [].concat(r.modeles).filter((x) => x !== etat.modeles[id].nom);
    statut(t('scores_ajoutes', { noms: noms.slice(0, 3).join(', ') + (noms.length > 3 ? '…' : '') })
      + (modeles.length ? t('modeles_a_reajuster', { modeles: modeles.join(', ') }) : ''));
  },
  async exporterScript(id) {
    const m = etat.modeles[id];
    if (bureau) {
      const chemin = await bureau.enregistrerSous(`${m.nom}.R`, [{ name: 'Script R', extensions: ['R'] }]);
      if (!chemin) return;
      const r = await occupe(() => demander('script_r', { modele: id, chemin }));
      statut(`${t('script_ecrit')} ${r.script}`);
    } else {
      const r = await occupe(() => demander('script_r', { modele: id }));
      telecharger(r.script);
      setTimeout(() => telecharger(r.donnees), 1000);
    }
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
    if (propager) diffuser('langue', { l });
    toutRedessiner();
  },
  zoom: (d) => bureau && bureau.zoom(d),
  aide: (sujet) => dialogueAide(app, sujet),
};

const C = creerCommandes(app);

// ------------------------------------------------------------ state helpers

function statut(message, erreur = false) {
  etat.message = message || '';
  etat.erreur = erreur;
  majEtat();
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

function telecharger(chemin) {
  window.location.href = '/api/fichier?chemin=' + encodeURIComponent(chemin);
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
// unexpected stop, the next start offers to reopen them.
async function sauvegarderSecours() {
  if (!bureau || VUE_SEULE || !dock) return;
  try {
    const dossier = await bureau.secoursDossier();
    const entrees = [];
    for (const p of Object.values(etat.projets)) {
      if (!p.modifie) continue;
      const fichier = `${dossier}/${p.id}.json`;
      await demander('sauvegarde_secours', { projet: p.id, chemin: fichier, session: JSON.stringify(collecterSession(p.id)) });
      entrees.push({ fichier, nom: p.nom, origine: p.chemin || fichier, date: new Date().toISOString() });
    }
    await bureau.secoursIndex(entrees);
  } catch { /* tried again two minutes later */ }
}

async function proposerRecuperation() {
  if (!bureau || VUE_SEULE) return;
  let entrees = [];
  try { entrees = await bureau.secoursListe(); } catch { entrees = []; }
  if (!entrees.length) return;
  const quand = (d) => new Date(d).toLocaleString(langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB', { dateStyle: 'medium', timeStyle: 'short' });
  dialogue({ titre: t('recuperation'),
    contenu: el('div', {}, el('p', {}, t('recuperation_question')), el('ul', {}, entrees.map((e) => el('li', {}, `${e.nom} — ${quand(e.date)}`)))),
    boutons: [
      { libelle: t('ignorer'), action: () => { bureau.secoursIndex([]); return true; } },
      { libelle: t('recuperer'), principal: true, action: () => {
        (async () => {
          for (const e of entrees) {
            try { await apresOuverture(await occupe(() => demander('ouvrir', { chemin: e.fichier, origine: e.origine, nom: e.nom }))); } catch { /* unreadable copy */ }
          }
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
    selection: parModele(etat.selection), choixVIP: parModele(etat.choixVIP), groupesHCA: parModele(etat.groupesHCA) };
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
  for (const cle of ['selection', 'choixVIP', 'groupesHCA']) {
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
  if (!interne.getPanel(`loadings:${id}`)) app.ouvrirVue('loadings', id, null, null, { referencePanel: `scores:${id}`, direction: 'below' });
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
    choisir: app.choisir, groupesHCA: etat.groupesHCA, demander, valeurs: app.valeurs,
    ouvrirVue: app.ouvrirVue, reajuster: app.reajuster, statut,
    rafraichir: () => rendrePanneau(entree),
    definirGraphe: (div) => { entree.graphe = div; },
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
  menuContextuel(e.clientX, e.clientY, [
    { libelle: t('mise_en_forme'), icone: 'palette', action: () => dialogueFormat(app, entree, div) },
    '-',
    { libelle: t('copier_image'), icone: 'copy', action: () => copierImage(div) },
    { libelle: t('image_png'), icone: 'image-down', action: () => enregistrerImage(div, 'png') },
    { libelle: t('image_svg'), action: () => enregistrerImage(div, 'svg') },
    '-',
    { libelle: t('effacer_selection'), actif: sel.length > 0, action: () => app.choisir(id, []) },
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
  s.replaceChildren(...[
    el('span', { class: 'moteur' + (etat.occupe ? ' occupe' : etat.erreur ? ' erreur' : '') }, el('span', { class: 'point' }),
      etat.occupe ? (Date.now() - etat.debutOccupe > 60000 ? t('calcul_long') : t('calcul')) : etat.message || t('pret')),
    el('span', { class: 'espace' }),
    m ? el('span', {}, `${m.nom} · ${m.type} · A = ${m.A} · N = ${m.N} · K = ${m.K}`) : el('span', { class: 'doux' }, t('aucun_modele_actif')),
    n ? el('span', { class: 'sel' }, `${n} ${t('selectionnees')}`) : null,
    el('button', { class: 'icone-seule', title: t('theme_sombre'), onclick: () => app.changerTheme(!app.themeSombre()) }, icone(app.themeSombre() ? 'sun' : 'moon')),
    el('select', { class: 'langue', title: t('langue'), onchange: (e) => app.changerLangue(e.target.value) },
      [['fr', 'FR'], ['en', 'EN']].map(([v, x]) => el('option', { value: v, selected: v === langueActuelle() }, x)))].filter(Boolean));
}

function rendreAccueil(conteneur) {
  conteneur.replaceChildren(el('div', { class: 'accueil' },
    el('div', { class: 'accueil-tete' }, el('img', { class: 'logo', src: 'nuee.svg', alt: '' }), el('h1', {}, t('titre')), el('p', {}, t('sous_titre'))),
    el('div', { class: 'accueil-actions' },
      el('button', { class: 'principal', onclick: () => app.ouvrir() }, icone('folder-open'), t('ouvrir')),
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
  if (params.get('ouvrir')) await app.ouvrirChemin(params.get('ouvrir'));
}

document.documentElement.dataset.theme = lire('theme') === 'sombre' ? 'sombre' : 'clair';
document.documentElement.lang = langueActuelle();
document.title = t('titre');
if (VUE_SEULE) demarrerVue(); else demarrerPrincipal();
