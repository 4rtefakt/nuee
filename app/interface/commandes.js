// Every action of the application, used by the menu bar, the toolbar, context menus and
// keyboard shortcuts. `app` is the main window object built in app.js.
import { t, langueActuelle } from './i18n.js';
import { vuesDe } from './explorateur.js';

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
    enregistrer: { icone: 'save', raccourci: 'Ctrl+S', actif: () => !!app.projetActif(), action: () => app.enregistrer(app.projetActif(), false) },
    enregistrer_sous: { raccourci: 'Ctrl+Shift+S', actif: () => !!app.projetActif(), action: () => app.enregistrer(app.projetActif(), true) },
    exporter_donnees: { icone: 'file-output', actif: () => !!app.projetActif(), action: () => app.exporterDonnees(app.projetActif()) },
    // (from Compare models showing the coefficients side by side: that table)
    exporter: { icone: 'file-down', raccourci: 'Ctrl+E', actif: () => avecModele() || !!app.coefficientsActifs(),
      action: () => (app.coefficientsActifs() ? app.exporterCoefficients(app.coefficientsActifs()) : app.exporter(m())) },
    script_r: { icone: 'file-code', actif: avecModele, action: () => app.exporterScript(m()) },
    rapport: { icone: 'file-text', actif: avecModele, action: () => app.rapport(m()) },
    texte_methodes: { icone: 'quote', actif: avecModele, action: () => app.texteMethodes(m()) },
    fermer_projet: { actif: () => !!app.projetActif(), action: () => app.fermerProjet(app.projetActif()) },
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
    selection_liee: { coche: () => app.selectionLiee(), action: () => app.basculerSelectionLiee() },
    nouvelle_variable: { icone: 'calculator', actif: () => !!app.projetActif(), action: () => app.nouvelleVariable(app.projetActif()) },
    ajouter_colonnes: { icone: 'file-spreadsheet', actif: () => !!app.projetActif(), action: () => app.ajouterColonnes(app.projetActif()) },
    resumer_groupes: { icone: 'sigma', actif: () => !!app.projetActif(), action: () => app.resumerGroupes(app.projetActif()) },

    nouveau_modele: { icone: 'file-plus', raccourci: 'Ctrl+N', actif: () => !!app.projetActif(), action: () => app.nouveauModele(app.projetActif()) },
    modifier_modele: { icone: 'square-pen', raccourci: 'Ctrl+M', actif: avecModele, action: () => app.modifierModele(m()) },
    autoajuster: { icone: 'wand-sparkles', actif: avecLV, action: () => app.reajuster(m(), { A: 0, auto: true }) },
    plus_comp: { icone: 'circle-plus', raccourci: 'Ctrl+=', actif: avecLV, action: () => app.reajuster(m(), { A: app.modele(m()).A + 1 }) },
    moins_comp: { icone: 'circle-minus', raccourci: 'Ctrl+-', actif: () => avecLV() && app.modele(m()).A > 1, action: () => app.reajuster(m(), { A: app.modele(m()).A - 1 }) },
    reajuster_vip: { icone: 'filter', actif: () => avecPLS() && choixVIP().length > 0,
      action: () => app.reajuster(m(), { garder: choixVIP() }) },
    supprimer_modele: { icone: 'trash-2', actif: avecModele, action: () => app.supprimerModele(m()) },
    scores_variables: { icone: 'git-merge', actif: avecLV, action: () => app.scoresEnVariables(m()) },

    resume: vue('resume'), scores: vue('scores', avecLV), loadings: vue('loadings', avecLV), profils: vue('profils', avecLV), dmodx: vue('dmodx', avecLV), t2: vue('t2', avecLV),
    vip: vue('vip', avecPLS), coefficients: vue('coefficients', () => avecPLS() || avecRegression()), obspred: vue('obspred', () => avecPLS() || avecRegression()),
    ipls: vue('ipls', avecPLS), residus: vue('residus', avecRegression), acf: vue('acf', avecRegression), apports: vue('apports', avecRegression), effets: vue('effets', avecRegression),
    selection_aic: vue('selection_aic', avecRegression),
    // (every regression: a GLM or GAM is refused with the reason, in the view)
    puissance: vue('puissance', avecRegression),
    dendrogramme: vue('dendrogramme', avecLV), liste_obs: vue('liste_obs'), liste_vars: vue('liste_vars', avecLV),
    permutations: vue('permutations', avecPLS), cvanova: vue('cvanova', avecPLS), classification: vue('classification', avecPLSDA),
    contributions: vue('contributions', avecLV), biplot: vue('biplot', avecLV), scores3d: vue('scores3d', avecLV), splot: vue('splot', avecOPLS), coomans: vue('coomans', avecGroupe),
    predire_fichier: { icone: 'file-input', actif: () => avecPrediction(), action: () => app.predireFichierVue(m()) },
    donnees: { icone: 'database', actif: () => !!app.projetActif(), action: () => app.ouvrirVue('donnees', null, null, app.projetActif()) },
    statistiques: { icone: 'chart-column-big', actif: () => !!app.projetActif(), action: () => app.ouvrirVue('statistiques', null, null, app.projetActif()) },
    tests: { icone: 'flask-conical', actif: () => !!app.projetActif(), action: () => app.ouvrirVue('tests', null, null, app.projetActif()) },
    series: { icone: ICONES_VUES.series, actif: () => !!app.projetActif(), action: () => app.ouvrirVue('series', null, null, app.projetActif()) },
    predire: { icone: 'target', actif: () => avecPrediction(), action: () => app.ouvrirVue('predictions', m()) },
    comparaison: { icone: 'columns-2', action: () => app.ouvrirVue('comparaison') },

    nouvelle_fenetre: { icone: 'app-window', raccourci: 'Ctrl+Shift+N', action: () => app.nouvelleFenetre() },
    detacher: { icone: 'external-link', raccourci: 'Ctrl+Shift+D', actif: () => !!app.panneauActif(), action: () => app.detacher() },
    fermer_panneau: { raccourci: 'Ctrl+W', actif: () => !!app.panneauActif(), action: () => app.fermerPanneau() },
    reinitialiser: { icone: 'layout-dashboard', action: () => app.reinitialiser() },

    chercher_commande: { icone: 'search', raccourci: 'Ctrl+K', action: () => app.palette() },
    explorateur: { icone: 'panel-left', raccourci: 'Ctrl+B', coche: () => app.explorateurVisible(), action: () => app.basculerExplorateur() },
    console_r: { icone: 'square-terminal', raccourci: 'Ctrl+J', coche: () => app.consoleVisible(), action: () => app.basculerConsole() },
    theme_sombre: { coche: () => app.themeSombre(), action: () => app.changerTheme(!app.themeSombre()) },
    zoom_plus: { raccourci: 'Ctrl+Alt+=', actif: () => !!app.bureau, action: () => app.zoom(1) },
    zoom_moins: { raccourci: 'Ctrl+Alt+-', actif: () => !!app.bureau, action: () => app.zoom(-1) },
    zoom_normal: { raccourci: 'Ctrl+0', actif: () => !!app.bureau, action: () => app.zoom(0) },

    guide: { icone: 'circle-help', raccourci: 'F1', action: () => app.aide('guide') },
    quoi_de_neuf: { action: () => app.quoiDeNeuf(true) },
    maj_auto: { coche: () => app.majAuto() === true, action: () => app.basculerMajAuto() },
    chercher_maj: { icone: 'refresh-ccw-dot', action: () => app.chercherMiseAJour() },
    diagnostic: { icone: 'stethoscope', action: () => app.diagnostic() },
    projet_exemple: { icone: 'apple', action: () => app.ouvrirExemple() },
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
  selection_aic: 'filter', puissance: 'chart-line',
  permutations: 'shuffle', cvanova: 'scale', classification: 'grid-3x3', contributions: 'sliders-horizontal',
  liste_obs: 'list', liste_vars: 'list', donnees: 'database', predictions: 'target', comparaison: 'columns-2',
  statistiques: 'chart-column-big', tests: 'flask-conical', coomans: 'shapes', biplot: 'crosshair', scores3d: 'axis-3d', splot: 'chart-scatter',
  series: 'trending-up',
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
  const annuler = () => {  // "Undo" names what it undoes
    const p = app.projet(app.projetActif());
    return { ...it('annuler_action'), libelle: p && p.annulable ? `${t('annuler_action')} « ${p.annulable} »` : t('annuler_action') };
  };
  return [
    { cle: 'm_fichier', items: () => [it('ouvrir'), it('coller'), it('importer_spectres'),
      { libelle: t('recents'), sous: recents.length ? recents.map((r) => ({ libelle: r, action: () => app.ouvrirChemin(r) }))
        : [{ libelle: t('aucun_recent'), actif: false, action: () => {} }] },
      '-', it('enregistrer'), it('enregistrer_sous'), '-', it('exporter_donnees'), it('exporter'), it('script_r'), it('rapport'), '-', it('fermer_projet')]
      .concat(app.bureau ? ['-', it('quitter')] : []) },
    { cle: 'm_edition', items: () => [annuler(), '-', it('copier_tableau'), it('copier_image'), it('image_png'), it('image_svg'), it('mise_en_forme'), '-', it('effacer_selection'), it('exclure_reajuster'),
      it('classe_selection'), '-', it('selection_liee'), '-', it('nouvelle_variable'), it('ajouter_colonnes'), it('resumer_groupes')] },
    { cle: 'm_modele', items: () => [it('nouveau_modele'), it('modifier_modele'), '-', it('autoajuster'), it('plus_comp'), it('moins_comp'),
      it('reajuster_vip'), '-', it('scores_variables'), it('texte_methodes'), '-', it('supprimer_modele')] },
    { cle: 'm_graphiques', items: () => [it('resume'), '-', it('scores'), it('loadings'), it('profils'), it('ipls'), it('biplot'), it('scores3d'), it('splot'), it('dmodx'), it('t2'), it('contributions'), '-',
      it('vip'), it('coefficients'), it('obspred'), it('effets'), it('residus'), it('acf'), it('apports'), it('selection_aic'), it('puissance'), '-', it('permutations'), it('cvanova'), it('classification'), '-',
      it('dendrogramme'), it('coomans'), '-', it('liste_obs'), it('liste_vars'), it('donnees'), it('statistiques'), it('tests'), it('series')] },
    { cle: 'm_prediction', items: () => [it('predire'), it('predire_fichier'), '-', it('comparaison')] },
    { cle: 'm_fenetre', items: () => [it('nouvelle_fenetre'), it('detacher'), it('fermer_panneau'), '-', it('reinitialiser')]
      .concat(app.fenetres().length > 1 ? ['-', ...app.fenetres().map((f) => ({ libelle: f.titre, coche: f.actuelle, action: () => app.focusFenetre(f.id) }))] : []) },
    { cle: 'm_affichage', items: () => [it('chercher_commande'), '-', it('explorateur'), it('console_r'), it('theme_sombre'),
      { libelle: t('langue'), sous: [['fr', 'Français'], ['en', 'English']].map(([l, n]) => ({ libelle: n, coche: langueActuelle() === l, action: () => app.changerLangue(l) })) },
      '-', it('zoom_plus'), it('zoom_moins'), it('zoom_normal')] },
    { cle: 'm_aide', items: () => [it('guide'), it('projet_exemple'), it('quoi_de_neuf')].concat(app.majAuto() !== null ? [it('chercher_maj'), it('maj_auto')] : [])
      .concat([it('differences'), '-', it('redemarrer_moteur'), it('diagnostic'), '-', it('apropos')]) },
  ];
}

export const BARRE_OUTILS = [
  ['ouvrir', 'enregistrer'],
  ['nouveau_modele', 'modifier_modele', 'autoajuster', 'plus_comp', 'moins_comp'],
  ['resume', 'scores', 'loadings', 'dmodx', 't2', 'contributions', 'vip', 'coefficients', 'obspred', 'dendrogramme'],
  ['permutations', 'classification', 'predire', 'comparaison', 'exporter'],
];

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
