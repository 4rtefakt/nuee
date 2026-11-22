// Every action of the application, used by the menu bar, the toolbar, context menus and
// keyboard shortcuts. `app` is the main window object built in app.js.
import { t, langueActuelle } from './i18n.js';
import { vuesDe } from './explorateur.js';
import { MOTEUR_WEBR } from './api.js';

export function creerCommandes(app) {
  const m = () => app.modeleActif();
  const avecModele = () => !!m();
  // (a regression has no components, scores or loadings: regression.R)
  const avecRegression = () => { const x = app.modele(m()); return !!x && !!x.regression; };
  // a model of NIPALS components (PCA, PLS, OPLS): not a regression nor another analysis (x.analyse)
  const avecLV = () => { const x = app.modele(m()); return !!x && !x.regression && !x.analyse; };
  const avecPLS = () => { const x = app.modele(m()); return !!x && x.type !== 'PCA' && !x.regression && !x.analyse; };
  // (another analysis predicts new observations only when it says so: x.prediction)
  const avecPrediction = () => { const x = app.modele(m()); return !!x && (!x.analyse || x.prediction === true); };
  const avecSelection = () => avecModele() && app.selection(m()).length > 0;
  const avecPLSDA = () => { const x = app.modele(m()); return !!x && /-DA$/.test(x.type); };
  const avecOPLS = () => { const x = app.modele(m()); return !!x && !!x.opls; };
  const avecGroupe = () => { const x = app.modele(m()); return !!x && !!x.groupe; };
  const choixVIP = () => app.choixVIP(m()) || [].concat((app.modele(m()).seuil_vip || {}).retenues || []);
  // a view: another analysis has those its module lists (vues_*.js), the other models those `cond` allows
  const vue = (nom, cond = avecModele) => ({ icone: ICONES_VUES[nom], action: () => app.ouvrirVue(nom, m()),
    actif: () => { const x = app.modele(m()); return x && x.analyse ? vuesDe(x).includes(nom) : cond(); } });

  const C = {
    ouvrir: { icone: 'folder-open', raccourci: 'Ctrl+O', action: () => app.ouvrir() },
    coller: { icone: 'clipboard-paste', raccourci: 'Ctrl+V', action: () => app.coller() },
    importer_spectres: { icone: 'audio-waveform', action: () => app.importerSpectres() },
    // (kobo.js: an export of KoboToolbox or ODK Central, the import's dialog)
    kobo_importer: { icone: 'smartphone', action: () => app.importerKobo() },
    enregistrer: { icone: 'save', raccourci: 'Ctrl+S', actif: () => !!app.projetActif(), action: () => app.enregistrer(app.projetActif(), false) },
    enregistrer_sous: { raccourci: 'Ctrl+Shift+S', actif: () => !!app.projetActif(), action: () => app.enregistrer(app.projetActif(), true) },
    // (versions.js: the project's history, kept by itself beside it; shared with GitHub or GitLab;
    // a colleague's project opened from there)
    versions: { icone: 'history', actif: () => !!app.projetActif(), action: () => app.versions(app.projetActif()) },
    // (in the browser version, they say that they need the desktop application: versions.js)
    versions_partager: { icone: 'cloud-upload', actif: () => (!!app.bureau || MOTEUR_WEBR) && !!app.projetActif(), action: () => app.partagerProjet(app.projetActif()) },
    versions_ouvrir_distant: { icone: 'cloud-download', actif: () => !!app.bureau || MOTEUR_WEBR, action: () => app.ouvrirDepuisDistant() },
    exporter_donnees: { icone: 'file-output', actif: () => !!app.projetActif(), action: () => app.exporterDonnees(app.projetActif()) },
    // (from Compare models showing the coefficients side by side: that table)
    exporter: { icone: 'file-down', raccourci: 'Ctrl+E', actif: () => avecModele() || !!app.coefficientsActifs(),
      action: () => (app.coefficientsActifs() ? app.exporterCoefficients(app.coefficientsActifs()) : app.exporter(m())) },
    script_r: { icone: 'file-code', actif: avecModele, action: () => app.exporterScript(m()) },
    // the project's reproducibility package: its data, one R script for every model, its documents,
    // its citation, in one .zip; and the same deposited on Zenodo as a draft (paquet.js; desktop only)
    paquet: { icone: 'package', actif: () => !!app.projetActif(), action: () => app.exporterPaquet(app.projetActif()) },
    paquet_zenodo: { icone: 'cloud-upload', actif: () => !!app.projetActif() && !!app.bureau, action: () => app.deposerZenodo(app.projetActif()) },
    // a reproducibility package opened again: its files checked, every model fitted again and compared
    // with the results it deposited (relancer.js)
    paquet_ouvrir: { icone: 'package-open', action: () => app.choisirPaquet() },
    // a document of the project: text that refers to its results (documents.js)
    nouveau_document: { icone: 'file-text', actif: () => !!app.projetActif(), action: () => app.nouveauDocument(app.projetActif()) },
    rapport: { icone: 'file-text', actif: avecModele, action: () => app.rapport(m()) },
    texte_methodes: { icone: 'quote', actif: avecModele, action: () => app.texteMethodes(m()) },
    fermer_projet: { actif: () => !!app.projetActif(), action: () => app.fermerProjet(app.projetActif()) },
    // (reglages.js: the R the engine runs on)
    reglages: { actif: () => !!app.bureau, action: () => app.reglages() },
    // (vues_greffons.js: the plugins installed, Install from file…)
    greffons: { icone: 'puzzle', action: () => app.gererGreffons() },
    quitter: { raccourci: 'Ctrl+Q', actif: () => !!app.bureau, action: () => app.quitter() },

    copier_image: { icone: 'copy', raccourci: 'Ctrl+Shift+C', actif: () => !!app.grapheActif(), action: () => app.copierImage() },
    // (with text selected, Ctrl+C copies the text as usual)
    copier_tableau: { icone: 'copy', raccourci: 'Ctrl+C', actif: () => !!app.tableauActif() && !String(window.getSelection()), action: () => app.copierTableau() },
    image_png: { icone: 'image-down', actif: () => !!app.grapheActif(), action: () => app.enregistrerImage('png') },
    image_svg: { actif: () => !!app.grapheActif(), action: () => app.enregistrerImage('svg') },
    mise_en_forme: { icone: 'palette', actif: () => !!app.grapheActif(), action: () => app.miseEnForme() },
    effacer_selection: { raccourci: 'Escape', actif: () => avecSelection() || (avecModele() && app.variablesChoisies(m()).length > 0),
      action: () => app.effacerSelection(m()) },
    exclure_reajuster: { icone: 'eye-off', raccourci: 'Ctrl+Delete', actif: avecSelection, action: () => app.reajuster(m(), { exclues: app.selection(m()) }) },
    classe_selection: { icone: 'tag', actif: avecSelection, action: () => app.classeSelection(m()) },
    annuler_action: { icone: 'undo-2', raccourci: 'Ctrl+Z', actif: () => { const p = app.projet(app.projetActif()); return !!(p && p.annulable); },
      action: () => app.annulerAction(app.projetActif()) },
    // (`autres`: its other shortcut, app.js)
    retablir_action: { icone: 'redo-2', raccourci: 'Ctrl+Y', autres: ['Ctrl+Shift+Z'], actif: () => { const p = app.projet(app.projetActif()); return !!(p && p.retablissable); },
      action: () => app.retablirAction(app.projetActif()) },
    selection_liee: { coche: () => app.selectionLiee(), action: () => app.basculerSelectionLiee() },
    nouvelle_variable: { icone: 'calculator', actif: () => !!app.projetActif(), action: () => app.nouvelleVariable(app.projetActif()) },
    ajouter_colonnes: { icone: 'file-spreadsheet', actif: () => !!app.projetActif(), action: () => app.ajouterColonnes(app.projetActif()) },
    // (raster.js: a raster's values at each observation, from the project's coordinates)
    raster_valeurs: { icone: 'layers', actif: () => !!app.projetActif(), action: () => app.valeursRaster(app.projetActif()) },
    resumer_groupes: { icone: 'sigma', actif: () => !!app.projetActif(), action: () => app.resumerGroupes(app.projetActif()) },
    // the data workbench (atelier.js)
    atelier_remplacer: { icone: 'search', raccourci: 'Ctrl+H', actif: () => !!app.projetActif(), action: () => app.remplacerDonnees(app.projetActif()) },
    atelier_categories: { icone: 'tag', actif: () => { const p = app.projet(app.projetActif()); return !!p && p.variables.some((v) => v.qualitative); },
      action: () => app.categoriesVariable(app.projetActif()) },
    atelier_variables: { icone: 'ruler', actif: () => !!app.projetActif(), action: () => app.variablesProjet(app.projetActif()) },
    // the data's source and steps, and the data read again from it or from next year's file (etapes.js)
    etapes_donnees: { icone: 'list', actif: () => !!app.projetActif(), action: () => app.etapesDonnees(app.projetActif()) },
    etapes_mettre_a_jour: { icone: 'refresh-cw', actif: () => !!app.projetActif(), action: () => app.miseAJourDonnees(app.projetActif()) },

    nouveau_modele: { icone: 'file-plus', raccourci: 'Ctrl+N', actif: () => !!app.projetActif(), action: () => app.nouveauModele(app.projetActif()) },
    // (a few questions, then New model already filled in: quelle_analyse.js, choix_analyse.js)
    quelle_analyse: { icone: 'signpost', actif: () => !!app.projetActif(), action: () => app.quelleAnalyse(app.projetActif()) },
    modifier_modele: { icone: 'square-pen', raccourci: 'Ctrl+M', actif: avecModele, action: () => app.modifierModele(m()) },
    autoajuster: { icone: 'wand-sparkles', actif: avecLV, action: () => app.reajuster(m(), { A: 0, auto: true }) },
    plus_comp: { icone: 'circle-plus', raccourci: 'Ctrl+=', actif: avecLV, action: () => app.reajuster(m(), { A: app.modele(m()).A + 1 }) },
    moins_comp: { icone: 'circle-minus', raccourci: 'Ctrl+-', actif: () => avecLV() && app.modele(m()).A > 1, action: () => app.reajuster(m(), { A: app.modele(m()).A - 1 }) },
    reajuster_vip: { icone: 'filter', actif: () => avecPLS() && choixVIP().length > 0,
      action: () => app.reajuster(m(), { garder: choixVIP() }) },
    supprimer_modele: { icone: 'trash-2', actif: avecModele, action: () => app.supprimerModele(m()) },
    scores_variables: { icone: 'git-merge', actif: avecLV, action: () => app.scoresEnVariables(m()) },
    par_reponse: { icone: 'layers', actif: avecRegression, action: () => app.unModeleParReponse(m()) },

    resume: vue('resume'), scores: vue('scores', avecLV), loadings: vue('loadings', avecLV), profils: vue('profils', avecLV), dmodx: vue('dmodx', avecLV), t2: vue('t2', avecLV),
    vip: vue('vip', avecPLS), coefficients: vue('coefficients', () => avecPLS() || avecRegression()), obspred: vue('obspred', () => avecPLS() || avecRegression()),
    ipls: vue('ipls', avecPLS), residus: vue('residus', avecRegression), acf: vue('acf', avecRegression), apports: vue('apports', avecRegression), effets: vue('effets', avecRegression),
    selection_aic: vue('selection_aic', avecRegression),
    // (every regression: a GLM or GAM is refused with the reason, in the view)
    puissance: vue('puissance', avecRegression),
    // (a regression with a qualitative variable, moyennes.R)
    moyennes: vue('moyennes', () => avecRegression() && [].concat(app.modele(m()).qualitatives || []).length > 0),
    dendrogramme: vue('dendrogramme', avecLV), liste_obs: vue('liste_obs'), liste_vars: vue('liste_vars', avecLV),
    permutations: vue('permutations', avecPLS), cvanova: vue('cvanova', avecPLS), classification: vue('classification', avecPLSDA),
    contributions: vue('contributions', avecLV), biplot: vue('biplot', avecLV), scores3d: vue('scores3d', avecLV), splot: vue('splot', avecOPLS), coomans: vue('coomans', avecGroupe),
    predire_fichier: { icone: 'file-input', actif: () => avecPrediction(), action: () => app.predireFichierVue(m()) },
    donnees: { icone: 'database', actif: () => !!app.projetActif(), action: () => app.ouvrirVue('donnees', null, null, app.projetActif()) },
    statistiques: { icone: 'chart-column-big', actif: () => !!app.projetActif(), action: () => app.ouvrirVue('statistiques', null, null, app.projetActif()) },
    tests: { icone: 'flask-conical', actif: () => !!app.projetActif(), action: () => app.ouvrirVue('tests', null, null, app.projetActif()) },
    series: { icone: ICONES_VUES.series, actif: () => !!app.projetActif(), action: () => app.ouvrirVue('series', null, null, app.projetActif()) },
    // (vues_carte.js: the project's map; the spatial autocorrelation, of a regression's residuals from its model)
    carte: { icone: 'map', actif: () => !!app.projetActif(), action: () => app.ouvrirVue('carte', null, null, app.projetActif()) },
    autocorrelation_spatiale: { icone: 'radar', actif: () => !!app.projetActif(),
      action: () => (avecRegression() ? app.ouvrirVue('autocorrelation_spatiale', m()) : app.ouvrirVue('autocorrelation_spatiale', null, null, app.projetActif())) },
    rarefaction: { icone: 'chart-spline', actif: () => !!app.projetActif(), action: () => app.ouvrirVue('rarefaction', null, null, app.projetActif()) },
    // (figures.js: plots of the project side by side, at a journal's size; starts with the plot in front)
    figure_composer: { icone: 'layout-grid', actif: () => !!app.projetActif(), action: () => app.composerFigure(app.projetActif()) },
    predire: { icone: 'target', actif: () => avecPrediction(), action: () => app.ouvrirVue('predictions', m()) },
    comparaison: { icone: 'columns-2', action: () => app.ouvrirVue('comparaison') },

    nouvelle_fenetre: { icone: 'app-window', raccourci: 'Ctrl+Shift+N', action: () => app.nouvelleFenetre() },
    detacher: { icone: 'external-link', raccourci: 'Ctrl+Shift+D', actif: () => !!app.panneauActif(), action: () => app.detacher() },
    fermer_panneau: { raccourci: 'Ctrl+W', actif: () => !!app.panneauActif(), action: () => app.fermerPanneau() },
    fermer_vues: { actif: () => app.avecVues(), action: () => app.fermerVues() },
    // the keyboard from tab to tab of the group in use, and from area to area (app.js; accessibilite.js's strings)
    onglet_suivant: { raccourci: 'Ctrl+Tab', action: () => app.ongletVoisin(1) },
    onglet_precedent: { raccourci: 'Ctrl+Shift+Tab', action: () => app.ongletVoisin(-1) },
    region_suivante: { raccourci: 'F6', action: () => app.regionVoisine(1) },
    region_precedente: { raccourci: 'Shift+F6', action: () => app.regionVoisine(-1) },
    reinitialiser: { icone: 'layout-dashboard', action: () => app.reinitialiser() },

    chercher_commande: { icone: 'search', raccourci: 'Ctrl+K', action: () => app.palette() },
    explorateur: { icone: 'panel-left', raccourci: 'Ctrl+B', coche: () => app.explorateurVisible(), action: () => app.basculerExplorateur() },
    console_r: { icone: 'square-terminal', raccourci: 'Ctrl+J', coche: () => app.consoleVisible(), action: () => app.basculerConsole() },
    theme_sombre: { coche: () => app.themeSombre(), action: () => app.changerTheme(!app.themeSombre()) },
    zoom_plus: { raccourci: 'Ctrl+Alt+=', actif: () => !!app.bureau, action: () => app.zoom(1) },
    zoom_moins: { raccourci: 'Ctrl+Alt+-', actif: () => !!app.bureau, action: () => app.zoom(-1) },
    zoom_normal: { raccourci: 'Ctrl+0', actif: () => !!app.bureau, action: () => app.zoom(0) },

    guide: { icone: 'circle-help', raccourci: 'F1', action: () => app.aide('guide') },
    // (aide.js: the documentation site, in the browser)
    tutoriels: { icone: 'book-open', action: () => app.tutoriels() },
    guide_en_ligne: { icone: 'external-link', action: () => app.guideEnLigne() },
    quoi_de_neuf: { action: () => app.quoiDeNeuf(true) },
    maj_auto: { coche: () => app.majAuto() === true, action: () => app.basculerMajAuto() },
    chercher_maj: { icone: 'refresh-ccw-dot', action: () => app.chercherMiseAJour() },
    diagnostic: { icone: 'stethoscope', action: () => app.diagnostic() },
    // Help › Example projects (app.js: ouvrirExemple): made-up apples; the tutorials' simulated skylarks and trout
    exemple_pommes: { icone: 'apple', action: () => app.ouvrirExemple('pommes') },
    exemple_alouettes: { icone: 'trending-up', action: () => app.ouvrirExemple('alouettes') },
    exemple_baci: { icone: 'chart-column', action: () => app.ouvrirExemple('baci') },
    redemarrer_moteur: { icone: 'refresh-cw', actif: () => !!app.bureau, action: () => app.confirmerRedemarrage() },
    differences: { action: () => app.aide('differences') },
    apropos: { icone: 'info', action: () => app.aide('apropos') },
  };
  for (const [id, c] of Object.entries(C)) c.id = id;
  return C;
}

export const ICONES_VUES = {
  resume: 'table-2', scores: 'chart-scatter', loadings: 'scan-search', profils: 'chart-spline', dmodx: 'chart-line', t2: 'ruler',
  vip: 'chart-bar', coefficients: 'chart-column', residus: 'chart-scatter', acf: 'chart-column', apports: 'chart-bar', effets: 'chart-spline', ipls: 'chart-column-big', obspred: 'sigma', dendrogramme: 'network',
  selection_aic: 'filter', puissance: 'chart-line', moyennes: 'scale',
  permutations: 'shuffle', cvanova: 'scale', classification: 'grid-3x3', contributions: 'sliders-horizontal',
  liste_obs: 'list', liste_vars: 'list', donnees: 'database', predictions: 'target', comparaison: 'columns-2',
  statistiques: 'chart-column-big', tests: 'flask-conical', coomans: 'shapes', biplot: 'crosshair', scores3d: 'axis-3d', splot: 'chart-scatter',
  series: 'trending-up', document: 'file-text', figure: 'layout-grid', versions: 'history', changements: 'diff',
};

// A menu item for a command, evaluated now (enabled state, check mark).
export function item(C, id) {
  const c = C[id];
  return { libelle: t(id), raccourci: c.raccourci ? afficherRaccourci(c.raccourci) : '', icone: c.icone,
    actif: c.actif ? c.actif() : true, coche: c.coche ? c.coche() : undefined, action: c.action };
}

export function menus(app, C) {
  const it = (id) => item(C, id);
  const recents = app.recents();
  const annuler = () => {  // "Undo" names what it undoes, "Redo" what it redoes
    const p = app.projet(app.projetActif());
    const entre = (n) => (langueActuelle() === 'fr' ? `« ${n} »` : `“${n}”`);
    return { ...it('annuler_action'), libelle: p && p.annulable ? `${t('annuler_action')} ${entre(p.annulable)}` : t('annuler_action') };
  };
  const retablir = () => {
    const p = app.projet(app.projetActif());
    const entre = (n) => (langueActuelle() === 'fr' ? `« ${n} »` : `“${n}”`);
    return { ...it('retablir_action'), libelle: p && p.retablissable ? `${t('retablir_action')} ${entre(p.retablissable)}` : t('retablir_action') };
  };
  return [
    { cle: 'm_fichier', items: () => [it('ouvrir'), it('paquet_ouvrir'), it('coller'), it('kobo_importer'), it('importer_spectres'),
      { libelle: t('recents'), sous: recents.length ? recents.map((r) => ({ libelle: r, action: () => app.ouvrirChemin(r) }))
        : [{ libelle: t('aucun_recent'), actif: false, action: () => {} }] },
      '-', it('enregistrer'), it('enregistrer_sous'), '-', it('versions'), it('versions_partager'), it('versions_ouvrir_distant'), '-', it('exporter_donnees'), it('exporter'), it('script_r'), it('rapport'), '-',
      it('paquet'), ...(app.bureau ? [it('paquet_zenodo')] : []), '-', it('nouveau_document'), '-', it('fermer_projet')]
      .concat(app.bureau ? ['-', it('reglages'), it('greffons'), '-', it('quitter')] : ['-', it('greffons')]) },
    { cle: 'm_edition', items: () => [annuler(), retablir(), '-', it('copier_tableau'), it('copier_image'), it('image_png'), it('image_svg'), it('mise_en_forme'), '-', it('effacer_selection'), it('exclure_reajuster'),
      it('classe_selection'), '-', it('selection_liee'), '-', it('atelier_remplacer'), it('atelier_categories'), it('atelier_variables'), '-',
      it('nouvelle_variable'), it('ajouter_colonnes'), it('raster_valeurs'), it('resumer_groupes'), '-', it('etapes_donnees'), it('etapes_mettre_a_jour')] },
    // (Model › Plugins: a New model of each plugin installed, then File › Plugins…: vues_greffons.js)
    { cle: 'm_modele', items: () => [it('quelle_analyse'), it('nouveau_modele'), { libelle: t('greffons_menu'), icone: 'puzzle', sous: app.menuGreffons ? app.menuGreffons() : [] },
      it('modifier_modele'), '-', it('autoajuster'), it('plus_comp'), it('moins_comp'),
      it('reajuster_vip'), '-', it('scores_variables'), it('par_reponse'), it('texte_methodes'), '-', it('supprimer_modele')] },
    { cle: 'm_graphiques', items: () => itemsGraphiques(app, C, it) },
    { cle: 'm_prediction', items: () => [it('predire'), it('predire_fichier'), '-', it('comparaison')] },
    { cle: 'm_fenetre', items: () => [it('nouvelle_fenetre'), it('detacher'), it('fermer_panneau'), it('fermer_vues'), '-', it('onglet_suivant'), it('onglet_precedent'),
      it('region_suivante'), it('region_precedente'), '-', it('reinitialiser')]
      .concat(app.fenetres().length > 1 ? ['-', ...app.fenetres().map((f) => ({ libelle: f.titre, coche: f.actuelle, action: () => app.focusFenetre(f.id) }))] : []) },
    { cle: 'm_affichage', items: () => [it('chercher_commande'), '-', it('explorateur'), it('console_r'), it('theme_sombre'),
      { libelle: t('langue'), sous: [['fr', 'Français'], ['en', 'English']].map(([l, n]) => ({ libelle: n, coche: langueActuelle() === l, action: () => app.changerLangue(l) })) },
      '-', it('zoom_plus'), it('zoom_moins'), it('zoom_normal')] },
    { cle: 'm_aide', items: () => [it('guide'), it('tutoriels'), it('guide_en_ligne'), '-', menuExemples(C), it('quoi_de_neuf')].concat(app.majAuto() !== null ? [it('chercher_maj'), it('maj_auto')] : [])
      .concat([it('differences'), '-', it('redemarrer_moteur'), it('diagnostic'), '-', it('apropos')]) },
  ];
}

// The Plots menu: the views of the active model, then the project's. Another analysis's own views
// (TRIM's indices, the detection function...) come after its summary; the model views the active
// model doesn't have (all of them, with no model in front) are folded under Other plots.
const GRAPHIQUES_MODELE = ['resume', '-', 'scores', 'loadings', 'profils', 'ipls', 'biplot', 'scores3d', 'splot', 'dmodx', 't2', 'contributions', '-',
  'vip', 'coefficients', 'obspred', 'effets', 'moyennes', 'residus', 'acf', 'apports', 'selection_aic', 'puissance', '-', 'permutations', 'cvanova', 'classification', '-',
  'dendrogramme', 'coomans'];
const GRAPHIQUES_PROJET = ['donnees', 'statistiques', 'tests', 'series', 'carte', 'autocorrelation_spatiale', 'rarefaction'];
function itemsGraphiques(app, C, it) {
  const id = app.modeleActif();
  const x = app.modele(id);
  const propres = x ? vuesDe(x).filter((v) => !C[v] && v !== 'predictions')
    .map((v) => ({ libelle: t(v), icone: ICONES_VUES[v], action: () => app.ouvrirVue(v, id) })) : [];
  const ici = [];
  const ailleurs = [];
  for (const k of GRAPHIQUES_MODELE) {
    if (k === '-') { ici.push('-'); ailleurs.push('-'); continue; }
    const i = it(k);
    (i.actif ? ici : ailleurs).push(i);
    if (k === 'resume' && propres.length) ici.push('-', ...propres);
  }
  const listes = ['liste_obs', 'liste_vars'].map(it);
  ailleurs.push('-', ...listes.filter((i) => !i.actif));
  const autres = sansSeparateursEnTrop(ailleurs);
  return sansSeparateursEnTrop([...ici, ...(autres.length ? ['-', { libelle: t('autres_graphiques'), sous: autres }] : []), '-',
    ...listes.filter((i) => i.actif), ...GRAPHIQUES_PROJET.map(it), '-', it('figure_composer')]);
}
// (a menu's separators between items only, one at a time)
const sansSeparateursEnTrop = (l) => l.filter((x, i) => x !== '-' || (i > 0 && l[i - 1] !== '-' && l.slice(i + 1).some((y) => y !== '-')));

// A view of the active model without a command of its own (another analysis's: TRIM's indices,
// the detection function...), for the toolbar.
export function commandeVue(app, nom) {
  return { id: nom, icone: ICONES_VUES[nom] || 'chart-scatter', action: () => app.ouvrirVue(nom, app.modeleActif()),
    actif: () => { const x = app.modele(app.modeleActif()); return !!x && vuesDe(x).includes(nom); } };
}

// Help › Example projects, and the start page's button (app.js)
export const menuExemples = (C) => ({ libelle: t('projet_exemple'), icone: 'apple', sous: ['exemple_pommes', 'exemple_alouettes', 'exemple_baci'].map((id) => item(C, id)) });

export const BARRE_OUTILS = [
  ['ouvrir', 'enregistrer'],
  ['nouveau_modele', 'modifier_modele', 'autoajuster', 'plus_comp', 'moins_comp'],
  ['resume', 'scores', 'loadings', 'dmodx', 't2', 'contributions', 'vip', 'coefficients', 'obspred', 'dendrogramme'],
  ['permutations', 'classification', 'predire', 'comparaison', 'exporter'],
];
// The toolbar for the model in front. A PCA, PLS or OPLS, or no model, has BARRE_OUTILS as it is
// (SIMCA's buttons, where hands know them); a regression or another analysis has its own views in
// the middle (vuesDe, explorateur.js: its lists left out, at most 12), without the buttons of
// components and of views it never has.
const HORS_BARRE = ['liste_obs', 'liste_vars', 'predictions'];
export function barreOutils(app) {
  const x = app.modele(app.modeleActif());
  if (!x || (!x.regression && !x.analyse)) return BARRE_OUTILS;
  return [BARRE_OUTILS[0], ['nouveau_modele', 'modifier_modele'], vuesDe(x).filter((v) => !HORS_BARRE.includes(v)).slice(0, 12), ['predire', 'comparaison', 'exporter']];
}

const MAC = /Mac/.test(navigator.platform);
export function afficherRaccourci(r) {
  const en = langueActuelle() === 'en';
  return r.replace('Ctrl', MAC ? '⌘' : 'Ctrl').replace('Escape', en ? 'Esc' : 'Échap').replace('Delete', en ? 'Del' : 'Suppr').replace('=', '+');
}

// "Ctrl+Shift+S" for a keyboard event. "+" and "=" (and "_" and "-") count as the same key,
// whatever the keyboard needs Shift for.
export function combinaison(e) {
  let k = e.key.length === 1 ? e.key.toUpperCase() : e.key;
  if (k === '+') k = '=';
  if (k === '_') k = '-';
  const shift = e.shiftKey && !['=', '-'].includes(k);
  return [(e.ctrlKey || e.metaKey) && 'Ctrl', e.altKey && 'Alt', shift && 'Shift', k].filter(Boolean).join('+');
}
