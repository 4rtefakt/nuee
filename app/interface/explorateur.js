// Project explorer: projects, their data, the models stored in them (imported from a .usp
// file or saved by this application), the models fitted in this session and their views.
import { t } from './i18n.js';
import { el, fmt, icone, menuContextuel } from './ui.js';
import { ICONES_VUES } from './commandes.js';

const VUES_MODELE = ['resume', 'scores', 'loadings', 'profils', 'biplot', 'splot', 'scores3d', 'dmodx', 't2', 'contributions', 'vip', 'coefficients', 'obspred', 'permutations',
  'cvanova', 'classification', 'coomans', 'dendrogramme', 'liste_obs', 'liste_vars', 'predictions'];
const PLS_SEULEMENT = ['vip', 'coefficients', 'obspred', 'permutations', 'cvanova'];
// (a regression has no components, scores or loadings: its own views, regression.R)
const VUES_REGRESSION = ['resume', 'coefficients', 'effets', 'obspred', 'residus', 'apports', 'liste_obs', 'predictions'];
const REGRESSIONS = ['LM', 'LMM', 'GLM', 'GAM'];
// the views of another analysis, by its type (filled by its module, vues_*.js)
export const VUES_TYPES = {};
// a model's views, as the explorer lists them (and the Plots menu offers another analysis's)
export const vuesDe = (m) => (VUES_TYPES[m.type] || (m.regression ? VUES_REGRESSION : VUES_MODELE.filter((v) => (m.type !== 'PCA' || !PLS_SEULEMENT.includes(v))
  && (v !== 'classification' || /-DA$/.test(m.type)) && (v !== 'splot' || m.opls) && (v !== 'coomans' || m.groupe))));
// R2 and Q2 of a fitted model, for its line in the explorer
const chiffresModele = (m) => (m.chiffres ? m.chiffres : m.regression
  ? `${m.qualite.marginal !== undefined ? `R2m ${fmt(m.qualite.marginal, 2)} · R2c ${fmt(m.qualite.conditionnel, 2)}`
    : `R2 ${fmt(m.qualite.r2 ?? m.qualite.r2_ajuste, 2)}`} · Q2 ${fmt(m.qualite.Q2, 2)} · AIC ${fmt(m.qualite.AIC, 1)}`
  : `R2X ${fmt(m.composantes.R2Xcum[m.A - 1], 2)}${m.composantes.R2Ycum ? ' · R2Y ' + fmt(m.composantes.R2Ycum[m.A - 1], 2) : ''} · Q2 ${fmt(m.composantes.Q2cum[m.A - 1], 2)}`);

export function rendreExplorateur(conteneur, app) {
  const projets = app.projets();
  if (!projets.length) {
    conteneur.replaceChildren(el('div', { class: 'accueil-explorateur' },
      el('p', {}, t('aucun_projet')),
      el('button', { class: 'principal', onclick: () => app.ouvrir() }, icone('folder-open'), t('ouvrir')),
      el('button', { onclick: () => app.coller(), title: t('coller_aide') }, icone('clipboard-paste'), t('coller')),
      el('button', { onclick: () => app.importerSpectres() }, icone('audio-waveform'), t('importer_spectres')),
      el('button', { onclick: () => app.ouvrirExemple(), title: t('projet_exemple_aide') }, icone('apple'), t('projet_exemple')),
      el('p', { class: 'note' }, t('aide_vide'))));
    return;
  }
  const replie = app.replies;
  const actif = app.noeudActif();

  const noeud = ({ cle, niveau, ic, libelle, details, titre, enfants, clic, double, menu, classe = '' }) => {
    const ouvert = enfants ? !replie.has(cle) : false;
    const ligne = el('div', {
      class: `noeud n${niveau} ${classe}` + (actif === cle ? ' actif' : ''), title: titre || '', tabindex: 0,
      onclick: (e) => { e.stopPropagation(); app.choisirNoeud(cle); if (clic) clic(); },
      ondblclick: (e) => { e.stopPropagation(); if (double) double(); else if (enfants) basculer(); },
      oncontextmenu: (e) => { e.preventDefault(); e.stopPropagation(); app.choisirNoeud(cle); if (clic) clic(); if (menu) menuContextuel(e.clientX, e.clientY, menu()); },
      onkeydown: (e) => { if (e.key === 'Enter') { if (double) double(); else if (enfants) basculer(); } },
    },
    el('span', { class: 'fleche', onclick: (e) => { e.stopPropagation(); if (enfants) basculer(); } },
      enfants ? icone(ouvert ? 'chevron-down' : 'chevron-right') : ''),
    icone(ic), el('span', { class: 'libelle' }, libelle), details ? el('span', { class: 'details' }, details) : null);
    function basculer() {
      if (replie.has(cle)) replie.delete(cle); else replie.add(cle);
      app.rafraichirExplorateur();
    }
    return [ligne, ouvert && enfants ? enfants() : null];
  };

  const arbre = projets.map((p) => {
    const calcules = app.modelesDe(p.id);
    const importes = p.modeles.filter((d) => d.version_origine);
    const propres = p.modeles.filter((d) => !d.version_origine).sort((a, b) => a.numero - b.numero);
    const n = p.observations.uid.length;
    const def = (d) => {
      const s = d.stocke;
      const der = (v) => (v && v.length ? v[d.A - 1] : null);
      const chiffres = s ? `R2X ${fmt(der(s.R2Xcum), 2)}${s.R2Ycum && der(s.R2Ycum) !== null ? ' · R2Y ' + fmt(der(s.R2Ycum), 2) : ''} · Q2 ${fmt(der(s.Q2cum), 2)}` : '';
      return noeud({
        cle: `${p.id}/def/${d.numero}`, niveau: 3, ic: 'box', classe: d.ajuste ? '' : 'inactif',
        libelle: [el('b', {}, d.nom), ' ', el('span', { class: 'badge' }, d.type), REGRESSIONS.includes(d.type) || VUES_TYPES[d.type] ? '' : ` A=${d.A}`, d.note ? el('span', { class: 'note-icone' }, icone('pencil')) : null],
        details: d.ajuste ? chiffres : t('non_ajuste'), titre: [d.titre, d.note, d.ajuste ? '' : t('non_ajuste')].filter(Boolean).join(' — '),
        double: d.ajuste ? () => app.calculer(p.id, d.numero) : null,
        menu: () => [{ libelle: t('calculer_modele'), icone: 'refresh-cw', actif: !!d.ajuste, action: () => app.calculer(p.id, d.numero) }]
          .concat(p.historique ? [{ libelle: t('historique_modele'), icone: 'history', action: () => app.ouvrirVue('historique', null, { modele: String(d.numero) }, p.id) }] : []),
      });
    };
    const modele = (m) => noeud({
      cle: `${p.id}/m/${m.id}`, niveau: 3, ic: 'box', classe: 'calcule',
      libelle: [el('b', {}, m.nom), ' ', el('span', { class: 'badge' }, m.type), m.regression || m.analyse ? '' : ` A=${m.A}`, m.note ? el('span', { class: 'note-icone' }, icone('pencil')) : null],
      details: chiffresModele(m),
      titre: [m.titre, m.note].filter(Boolean).join(' — '), double: () => app.ouvrirModele(m.id),
      enfants: () => vuesDe(m).map((v) => noeud({
        cle: `${p.id}/m/${m.id}/${v}`, niveau: 4, ic: ICONES_VUES[v], libelle: t(v),
        double: () => app.ouvrirVue(v, m.id), menu: () => [
          { libelle: t(v), icone: ICONES_VUES[v], action: () => app.ouvrirVue(v, m.id) },
          { libelle: t('detacher'), icone: 'external-link', action: () => app.ouvrirFenetreVue(v, m.id) }],
      })),
      menu: () => [
        { libelle: t('resume'), icone: 'table-2', action: () => app.ouvrirVue('resume', m.id) },
        { libelle: t('m_graphiques'), sous: vuesDe(m).map((v) => ({ libelle: t(v), icone: ICONES_VUES[v], action: () => app.ouvrirVue(v, m.id) })) },
        '-',
        { libelle: t('modifier_modele'), icone: 'square-pen', action: () => app.modifierModele(m.id) },
      ].concat(m.regression || m.analyse ? [] : [
        { libelle: t('autoajuster'), icone: 'wand-sparkles', action: () => app.reajuster(m.id, { A: 0, auto: true }) },
        { libelle: t('plus_comp'), icone: 'circle-plus', action: () => app.reajuster(m.id, { A: m.A + 1 }) },
        { libelle: t('moins_comp'), icone: 'circle-minus', actif: m.A > 1, action: () => app.reajuster(m.id, { A: m.A - 1 }) },
      ], [
        { libelle: t('exclure_reajuster'), icone: 'eye-off', actif: app.selection(m.id).length > 0, action: () => app.reajuster(m.id, { exclues: app.selection(m.id) }) },
      ], m.regression || m.analyse ? [] : [
        '-',
        { libelle: t('scores_variables'), icone: 'git-merge', action: () => app.scoresEnVariables(m.id) },
      ], [
        '-',
        { libelle: t('exporter'), icone: 'file-down', action: () => app.exporter(m.id) },
        { libelle: t('script_r'), icone: 'file-code', action: () => app.exporterScript(m.id) },
        { libelle: t('rapport'), icone: 'file-text', action: () => app.rapport(m.id) },
      ], [
        { libelle: t('supprimer_modele'), icone: 'trash-2', action: () => app.supprimerModele(m.id) },
      ]),
    });
    const groupe = (cle, libelle, ic, liste, f) => (liste.length ? noeud({ cle: `${p.id}/${cle}`, niveau: 2, ic, libelle: [libelle, el('span', { class: 'compte' }, liste.length)], enfants: () => liste.map(f) }) : null);
    return noeud({
      cle: p.id, niveau: 1, ic: 'folder', libelle: [el('b', {}, p.nom), p.modifie ? el('span', { class: 'badge alerte', title: t('modifie') }, '●') : null],
      titre: p.source,
      menu: () => [
        { libelle: t('nouveau_modele'), icone: 'file-plus', action: () => app.nouveauModele(p.id) },
        { libelle: t('donnees'), icone: 'database', action: () => app.ouvrirVue('donnees', null, null, p.id) },
        { libelle: t('statistiques'), icone: 'chart-column-big', action: () => app.ouvrirVue('statistiques', null, null, p.id) },
        { libelle: t('tests_variables'), icone: 'flask-conical', action: () => app.ouvrirVue('tests', null, null, p.id) },
        '-',
        { libelle: t('enregistrer'), icone: 'save', action: () => app.enregistrer(p.id, false) },
        { libelle: t('enregistrer_sous'), action: () => app.enregistrer(p.id, true) },
        { libelle: t('exporter_donnees'), icone: 'file-output', action: () => app.exporterDonnees(p.id) },
        '-',
        { libelle: t('fermer_projet'), icone: 'x', action: () => app.fermerProjet(p.id) },
      ],
      enfants: () => [
        noeud({ cle: `${p.id}/donnees`, niveau: 2, ic: 'database', libelle: t('donnees'), details: `${n} ${t('obs')} × ${p.variables.length} ${t('vars')}`,
          double: () => app.ouvrirVue('donnees', null, null, p.id),
          menu: () => [{ libelle: t('donnees'), icone: 'database', action: () => app.ouvrirVue('donnees', null, null, p.id) },
            { libelle: t('statistiques'), icone: 'chart-column-big', action: () => app.ouvrirVue('statistiques', null, null, p.id) },
            { libelle: t('nouvelle_variable'), icone: 'calculator', action: () => app.nouvelleVariable(p.id) },
            { libelle: t('nouveau_modele'), icone: 'file-plus', action: () => app.nouveauModele(p.id) },
            { libelle: t('exporter_donnees'), icone: 'file-output', action: () => app.exporterDonnees(p.id) }] }),
        p.historique ? noeud({ cle: `${p.id}/historique`, niveau: 2, ic: 'history', libelle: [t('historique'), el('span', { class: 'compte' }, p.historique)],
          titre: t('historique_aide'), double: () => app.ouvrirVue('historique', null, { modele: '' }, p.id),
          menu: () => [{ libelle: t('historique'), icone: 'history', action: () => app.ouvrirVue('historique', null, { modele: '' }, p.id) }] }) : null,
        noeud({ cle: `${p.id}/statistiques`, niveau: 2, ic: 'chart-column-big', libelle: t('statistiques'),
          double: () => app.ouvrirVue('statistiques', null, null, p.id),
          menu: () => [{ libelle: t('statistiques'), icone: 'chart-column-big', action: () => app.ouvrirVue('statistiques', null, null, p.id) }] }),
        noeud({ cle: `${p.id}/tests`, niveau: 2, ic: 'flask-conical', libelle: t('tests_variables'),
          double: () => app.ouvrirVue('tests', null, null, p.id),
          menu: () => [{ libelle: t('tests_variables'), icone: 'flask-conical', action: () => app.ouvrirVue('tests', null, null, p.id) }] }),
        groupe('importes', t('modeles_fichier'), 'layout-dashboard', importes, def),
        groupe('propres', t('modeles_projet'), 'layout-dashboard', propres, def),
        groupe('calcules', t('modeles_r'), 'sigma', calcules, modele),
        groupe('predictions', t('jeux_prediction'), 'target', p.predictions || [], (s, i) => noeud({
          cle: `${p.id}/ps/${i}`, niveau: 3, ic: 'list', libelle: s.nom || `${t('jeu_prediction')} ${i + 1}`, details: `${s.n_obs} ${t('obs')}` })),
      ],
    });
  });
  conteneur.replaceChildren(el('div', { class: 'arbre', role: 'tree' }, arbre));
}
