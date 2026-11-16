// Random forests (FOREST) and boosted trees (BOOST) (moteur/R/forets.R, boosting.R): their
// settings in the model dialog, their views (summary, variable importance, partial dependence
// computed on request, observed against predicted or the classification, the error against the
// number of trees), their report parts and strings. Imported by app.js.
import { t, ajouterTextes, deuxPoints, pourcent } from './i18n.js';
import { el, fmt, decimal } from './ui.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, controleCouleur, controleEtiquettes, pointsObservations, tableau,
  notesModele, ficheTest, ficheCondition, avecUnite, uniteAxe, transformeeDans, etendue } from './panneaux.js';
import { ANALYSES } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const estEnsemble = (m) => !!m && !!m.analyse && ['FOREST', 'BOOST'].includes(m.type);
const foret = (m) => m.type === 'FOREST';
const chiffre = (k, v, d = 3) => el('span', {}, el('small', {}, k), ' ', el('b', {}, typeof v === 'number' ? fmt(v, d) : v ?? '—'));
const pct = (v, d = 1) => (v === null || v === undefined ? '—' : `${fmt(100 * v, d)}${pourcent()}`);
const titreFiche = (m) => el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, m.type), m.titre ? el('span', { class: 'doux' }, m.titre) : null);
const groupesVC = (m) => (m.vc_par ? `${m.G} (${m.vc_par})` : String(m.G));
// a classification: a forest of classes, or boosted trees of a 0/1 response (classified at 0.5)
const classe = (m) => (foret(m) ? !!m.classement : m.loi === 'binomiale');

// ------------------------------------------------------------ importance

// A forest: permutation importance (the rise of the out-of-bag error when the variable's values
// are permuted) or impurity importance; boosted trees: relative influence, in % of its sum.
function rendreImportance(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  loc.quoi ??= 'permutation';
  const df = foret(m) ? m.importance : m.influence;
  const cle = foret(m) ? loc.quoi : 'pourcentage';
  const n = liste(df.variable).length;
  const ordre = Array.from({ length: n }, (_, i) => i).sort((a, b) => liste(df[cle])[b] - liste(df[cle])[a]);
  const vars = ordre.map((i) => liste(df.variable)[i]);
  const vals = ordre.map((i) => liste(df[cle])[i]);
  const blocs = ordre.map((i) => liste(df.bloc)[i] || '—');
  const noms = [...new Set(blocs)];
  const note = foret(m) ? t(loc.quoi === 'permutation' ? (m.reglages_foret.importance === 'echelle' ? 'ens_perm_note_echelle' : 'ens_perm_note') : 'ens_impurete_note')
    : t('ens_influence_note');
  conteneur.replaceChildren(foret(m) ? outils(choix('', [['permutation', t('ens_permutation')], ['impurete', t('ens_impurete')]], loc.quoi,
    (v) => { loc.quoi = v; ctx.rafraichir(); })) : el('span'), el('p', { class: 'note' }, note), graphe, bas);
  const unite = foret(m) ? '' : pourcent();
  tracer(ctx, graphe, noms.map((b, k) => {
    const idx = vars.map((_, i) => i).filter((i) => blocs[i] === b);
    return { type: 'bar', orientation: 'h', name: b === '—' ? t('aucun') : b, y: idx.map((i) => vars[i]), x: idx.map((i) => vals[i]),
      marker: { color: PALETTE[k % PALETTE.length] }, hovertemplate: `<b>%{y}</b><br>%{x:.4g}${unite}<extra></extra>` };
  }), { title: { text: `${m.nom} · ${foret(m) ? t(loc.quoi === 'permutation' ? 'ens_permutation' : 'ens_impurete') : t('ens_influence')}` }, dragmode: false,
    showlegend: noms.length > 1, barmode: 'stack', yaxis: { categoryorder: 'array', categoryarray: [...vars].reverse(), automargin: true },
    xaxis: { title: { text: foret(m) ? '' : '%' }, zeroline: true } });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n, nom: 'Table',
    colonnes: [{ cle: 'variable', titre: t('variable_arbre') }, { cle: 'bloc', titre: t('bloc') }].concat(foret(m)
      ? [{ cle: 'permutation', titre: t('ens_permutation') }, { cle: 'impurete', titre: t('ens_impurete') }]
      : [{ cle: 'influence', titre: t('ens_reduction') }, { cle: 'pourcentage', titre: '%', decimales: 1 }]),
    valeur: (i, k) => liste(df[k])[i],
  });
}

// ------------------------------------------------------------ partial dependence

// The partial dependence of each variable (Friedman 2001), computed by the engine on request and
// kept with the model: the mean prediction over the model's observations when the variable takes
// each value of a grid; under it, the observed values (a rug), or each category's count. The six
// most important variables side by side (by default), all of them, or one; a forest's class, or
// boosted trees' scale (link or response).
const PREMIERES = 6;
async function rendreDependance(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const vars = liste(m.variables_x);
  const imp = foret(m) ? m.importance : m.influence;  // (the engine sends them most important first)
  const premieres = liste(imp.variable).slice(0, PREMIERES);
  loc.variable ??= '';
  if (loc.variable && loc.variable !== '*' && !vars.includes(loc.variable)) loc.variable = '';
  const voulues = loc.variable === '*' ? vars : loc.variable ? [loc.variable] : premieres.filter((v) => vars.includes(v));
  loc.cache ??= {};
  if (loc.id !== m.id) { loc.cache = {}; loc.id = m.id; }
  const manquent = voulues.filter((v) => !loc.cache[v]);
  if (manquent.length) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('ens_calcul_dependance', { n: manquent.length })));
    const r = await ctx.demander('dependance_partielle', { modele: m.id, variables: manquent });
    for (const d of liste(r.dependance)) loc.cache[d.variable] = d;
  }
  const classes = liste(m.classes);
  loc.classe ??= classes[classes.length - 1] || '';
  loc.echelle ??= 'reponse';
  const lien = !foret(m) && m.loi !== 'gaussienne';
  const ds = voulues.map((v) => loc.cache[v]).filter(Boolean);
  const k = ds.length;
  const colonnes = k <= 1 ? 1 : k <= 4 ? 2 : 3;
  const lignes = Math.ceil(k / colonnes);
  // several variables: a plot tall enough for each row, in a pane that scrolls
  const graphe = el('div', { class: 'graphe', style: k > 1 ? `flex: none; height: ${Math.max(260, 200 * lignes + 50)}px` : '' });
  conteneur.replaceChildren(outils(
    choix(t('variable_arbre'), [['', t('ens_premieres', { n: Math.min(PREMIERES, vars.length) })], ['*', t('ens_toutes')]].concat(vars.map((v) => [v, v])), loc.variable,
      (v) => { loc.variable = v; ctx.rafraichir(); }),
    foret(m) && m.classement ? choix(t('classe'), classes.map((k) => [k, k]), loc.classe, (v) => { loc.classe = v; ctx.rafraichir(); }) : null,
    lien ? choix('', [['reponse', t(m.loi === 'binomiale' ? 'ens_echelle_proba' : 'ens_echelle_moyenne')], ['lien', t(m.loi === 'binomiale' ? 'ens_echelle_logit' : 'ens_echelle_log')]],
      loc.echelle, (v) => { loc.echelle = v; ctx.rafraichir(); }) : null),
  el('p', { class: 'note' }, t(foret(m) ? (m.classement ? 'ens_dependance_note_classe' : 'ens_dependance_note') : 'ens_dependance_note_boost')),
  k > 1 ? el('div', { class: 'defile' }, graphe) : graphe);
  const c = couleursTheme(ctx);
  const valeurs = (d) => (foret(m) && m.classement ? liste(d.probabilites[loc.classe]) : lien && loc.echelle === 'reponse' ? liste(d.reponse) : liste(d.y));
  const nomY = foret(m) && m.classement ? `P(${loc.classe})` : lien ? t(loc.echelle === 'reponse' ? (m.loi === 'binomiale' ? 'ens_echelle_proba' : 'ens_echelle_moyenne')
    : (m.loi === 'binomiale' ? 'ens_echelle_logit' : 'ens_echelle_log')) : avecUnite(ctx, m.projet, m.noms_y[0], transformeeDans(ctx, m, m.noms_y[0]));
  const traces = [];
  const layout = { title: { text: `${m.nom} · ${t('ens_dependance')}` }, dragmode: 'zoom', showlegend: false,
    margin: { l: 60, r: 18, t: 40, b: 40 } };
  if (k > 1) layout.grid = { rows: lignes, columns: colonnes, pattern: 'independent', xgap: 0.1, ygap: 0.35 };
  ds.forEach((d, i) => {
    const s = i ? String(i + 1) : '';
    const y = valeurs(d);
    const x = liste(d.x);
    const bas = Math.min(...y);
    const haut = Math.max(...y);
    const marge = (haut - bas || Math.abs(haut) || 1) * 0.08;
    if (d.qualitative) {
      const n = liste(d.tapis);
      traces.push({ type: 'scatter', mode: 'markers', x, y, xaxis: `x${s}`, yaxis: `y${s}`, marker: { size: 10, color: PALETTE[0] },
        text: n.map((v) => `n = ${v}`), hovertemplate: `<b>%{x}</b><br>%{y:.4g}<br>%{text}<extra>${d.variable}</extra>` });
    } else {
      traces.push({ type: 'scatter', mode: 'lines', x, y, xaxis: `x${s}`, yaxis: `y${s}`, line: { color: PALETTE[0], width: 2, shape: 'linear' },
        hovertemplate: `%{x:.4g} → %{y:.4g}<extra>${d.variable}</extra>` });
      // the observed values, as a rug under the curve
      const tapis = liste(d.tapis);
      traces.push({ type: 'scatter', mode: 'markers', x: tapis, y: tapis.map(() => bas - marge), xaxis: `x${s}`, yaxis: `y${s}`, hoverinfo: 'skip',
        marker: { symbol: 'line-ns-open', size: 8, color: c.doux, line: { width: 1, color: c.doux } } });
    }
    const ax = { gridcolor: c.grille, zerolinecolor: c.doux, linecolor: c.grille, automargin: true, ...(k > 1 ? { nticks: 5, tickfont: { size: 9 } } : {}) };
    layout[`xaxis${s}`] = { ...ax, title: { text: avecUnite(ctx, m.projet, d.variable), font: { size: k > 1 ? 11 : undefined }, standoff: 4 }, type: d.qualitative ? 'category' : '-' };
    layout[`yaxis${s}`] = { ...ax, title: { text: i % colonnes === 0 ? nomY : '', font: { size: k > 1 ? 10 : undefined } }, range: [bas - 2 * marge, haut + marge] };
  });
  if (!k) { graphe.replaceChildren(el('p', { class: 'infos' }, t('ens_sans_dependance'))); return; }
  tracer(ctx, graphe, traces, layout);
}

// ------------------------------------------------------------ observed / predicted

// The predictions each kind of model has: a forest's out-of-bag ones (each observation predicted
// by the trees whose draw doesn't hold it), cross-validated (by a model rebuilt without its group)
// and fitted (every tree); boosted trees' fitted and cross-validated ones.
const MODES_PREDICTION = { FOREST: ['oob', 'vc', 'ajuste'], BOOST: ['vc', 'ajuste'] };
const COLONNE = { oob: 'prédit (OOB)', vc: 'prédit (VC)', ajuste: 'ajusté' };

function rendreObsPred(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const modes = MODES_PREDICTION[m.type];
  if (!modes.includes(loc.mode)) loc.mode = modes[0];
  loc.couleur ??= '';
  const y = m.noms_y[0];
  const o = m.observations;
  const graphe = el('div', { class: 'graphe' });
  const q = m.qualite;
  const infos = foret(m) ? `R2 OOB ${fmt(q.r2_oob, 3)} · Q2 ${fmt(q.Q2, 3)} · RMSEcv ${fmt(q.RMSEcv, 4)}`
    : m.loi === 'gaussienne' ? `R2 ${fmt(q.r2, 3)} · Q2 ${fmt(q.Q2, 3)} · RMSEcv ${fmt(q.RMSEcv, 4)}` : `D2 ${fmt(q.D2, 3)} · D2 ${t('vc_court')} ${fmt(q.D2_vc, 3)}`;
  conteneur.replaceChildren(outils(choix('', modes.map((k) => [k, t(`ens_mode_${k}`)]), loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    controleCouleur(ctx, m, loc), controleEtiquettes(ctx, m, loc), el('span', { class: 'infos' }, infos)),
  el('p', { class: 'note' }, t(`ens_mode_${loc.mode}_note${!foret(m) && loc.mode === 'ajuste' ? '_boost' : ''}`)), graphe);
  const xo = liste(o[`${y} observé`]);
  const yp = liste(o[`${y} ${COLONNE[loc.mode]}`]);
  const vals = xo.concat(yp).filter((v) => v !== null);
  const points = pointsObservations(ctx, m, loc, xo, yp, { survol: '%{x:.3f} → %{y:.3f}' });
  if (m.test && m.test.observations && m.test.observations[`${y} observé`]) {
    const to = m.test.observations;
    const [xt, yt] = [liste(to[`${y} observé`]), liste(to[`${y} prédit`])];
    vals.push(...xt.concat(yt).filter((v) => v !== null));
    points.push({ type: 'scatter', mode: 'markers', name: t('jeu_test'), x: xt, y: yt, text: liste(to.nom),
      marker: { symbol: 'diamond-open', size: 9, color: '#d1495b', line: { width: 1.5 } }, hovertemplate: `<b>%{text}</b> (${t('test')})<br>%{x:.3f} → %{y:.3f}<extra></extra>` });
  }
  const [lo, hi] = etendue(vals);
  tracer(ctx, graphe, points, { showlegend: points.length > 1, title: { text: `${m.nom} · ${y} : ${t('observe')} / ${t(`ens_mode_${loc.mode}`)}` },
    shapes: [{ type: 'line', x0: lo, y0: lo, x1: hi, y1: hi, line: { color: '#888', dash: 'dot', width: 1 } }],
    xaxis: { title: { text: `${y} ${t('observe')}${uniteAxe(ctx, m.projet, y, transformeeDans(ctx, m, y))}` } },
    yaxis: { title: { text: `${y} · ${t(`ens_mode_${loc.mode}`)}${uniteAxe(ctx, m.projet, y, transformeeDans(ctx, m, y))}` } } });
  brancherSelection(graphe, ctx, m.id);
}

// ------------------------------------------------------------ classification

// A forest's classification table (out of bag, cross-validated or fitted) and ROC curves from the
// share of each class's votes; boosted trees of a 0/1 response, classified at a probability of 0.5.
function rendreClassement(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const modes = foret(m) ? ['oob', 'vc', 'ajuste'] : ['vc', 'ajuste'];
  if (!modes.includes(loc.mode)) loc.mode = modes[0];
  const r = m.classification[loc.mode];
  const classes = liste(r.classes);
  const tab = r.table;
  const somme = (v) => v.reduce((a, b) => a + b, 0);
  const table = el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, el('th', {}, `${t('observee')} \\ ${t('predite')}`), classes.map((k) => el('th', { class: 'num' }, k)),
      el('th', { class: 'num' }, t('total')), el('th', { class: 'num' }, t('bien_classes')))),
    el('tbody', {}, classes.map((k, i) => el('tr', {}, el('td', {}, k),
      tab[i].map((x, j) => el('td', { class: 'num' + (i === j ? ' diagonale' : x ? ' erreur-classe' : '') }, x)),
      el('td', { class: 'num' }, somme(tab[i])), el('td', { class: 'num' }, pct(liste(r.correct)[i])))),
    el('tr', { class: 'total' }, el('td', {}, t('total')), classes.map((_, j) => el('td', { class: 'num' }, somme(tab.map((l) => l[j])))),
      el('td', { class: 'num' }, somme(tab.map(somme))), el('td', { class: 'num' }, pct(r.global)))));
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(choix('', modes.map((k) => [k, t(`ens_mode_${k}`)]), loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    el('span', { class: 'infos' }, `${t('bien_classes')}${deuxPoints()}${pct(r.global)}`)),
  el('p', { class: 'note' }, t(foret(m) ? `ens_classement_note_${loc.mode}` : 'ens_classement_note_boost')), el('div', { class: 'tableau-court' }, table), graphe);
  const traces = r.roc.map((roc, j) => ({ type: 'scatter', mode: 'lines', name: `${classes[j]} (AUC ${fmt(roc.auc, 3)})`,
    x: roc.fpr, y: roc.tpr, line: { color: PALETTE[j % PALETTE.length], width: 2, shape: 'hv' } }));
  traces.push({ type: 'scatter', mode: 'lines', x: [0, 1], y: [0, 1], line: { color: '#999', dash: 'dot', width: 1 }, showlegend: false, hoverinfo: 'skip' });
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ROC` }, dragmode: 'zoom', showlegend: true,
    xaxis: { title: { text: t('faux_positifs') }, range: [0, 1] }, yaxis: { title: { text: t('vrais_positifs') }, range: [0, 1.02] } });
}

// ------------------------------------------------------------ error against the number of trees

// A forest: the out-of-bag error after 1, 2… trees (it should have levelled off by the last one);
// boosted trees: the deviance of the observations they were fitted on and the cross-validated one,
// and the number of trees chosen where the latter is smallest.
function rendreErreur(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const c = m.courbe;
  const arbres = liste(c.arbres);
  const graphe = el('div', { class: 'graphe' });
  const c2 = couleursTheme(ctx);
  if (foret(m)) {
    conteneur.replaceChildren(el('p', { class: 'note' }, t(m.classement ? 'ens_erreur_note_classe' : 'ens_erreur_note')), graphe);
    const e = liste(c.erreur);
    // (the first trees' error, out of bag over very few trees, would flatten the rest: the axis
    // starts from the tenth tree; zooming out shows them)
    const suite = e.length > 50 ? e.slice(9) : e;
    const [bas, haut] = [Math.min(...suite), Math.max(...suite)];
    tracer(ctx, graphe, [{ type: 'scatter', mode: 'lines', name: t('ens_erreur_oob'), x: arbres, y: e, line: { color: PALETTE[0], width: 1.5 },
      hovertemplate: `%{x} ${t('ens_arbres_mot')}<br>%{y:.4g}<extra></extra>` }],
    { title: { text: `${m.nom} · ${t('ens_erreur')}` }, dragmode: 'zoom', xaxis: { title: { text: t('ens_nb_arbres') } },
      yaxis: { title: { text: t(m.classement ? 'ens_part_mal_classees' : 'ens_mse_oob') }, range: [bas - 0.05 * (haut - bas || 1), haut + 0.05 * (haut - bas || 1)] } });
    return;
  }
  conteneur.replaceChildren(el('p', { class: 'note' }, t('ens_deviance_note', { n: m.meilleur, max: m.reglages_boost.arbres })
    + (m.au_maximum ? ` ${t('ens_au_maximum')}` : '')), graphe);
  const vc = liste(c.validation);
  tracer(ctx, graphe, [
    { type: 'scatter', mode: 'lines', name: t('ens_deviance_apprentissage'), x: arbres, y: liste(c.apprentissage), line: { color: c2.doux, width: 1.5, dash: 'dot' },
      hovertemplate: `%{x} ${t('ens_arbres_mot')}<br>%{y:.4g}<extra></extra>` },
    { type: 'scatter', mode: 'lines', name: t('ens_deviance_vc'), x: arbres, y: vc, line: { color: PALETTE[0], width: 2 },
      hovertemplate: `%{x} ${t('ens_arbres_mot')}<br>%{y:.4g}<extra></extra>` },
    { type: 'scatter', mode: 'markers', name: t('ens_retenu'), x: [m.meilleur], y: [vc[m.meilleur - 1]], hoverinfo: 'skip',
      marker: { size: 12, color: 'rgba(0,0,0,0)', line: { width: 2.5, color: PALETTE[1] } } }],
  { title: { text: `${m.nom} · ${t('ens_erreur')}` }, dragmode: 'zoom', showlegend: true, xaxis: { title: { text: t('ens_nb_arbres') } },
    yaxis: { title: { text: t('ens_deviance_moyenne') } },
    shapes: [{ type: 'line', yref: 'paper', y0: 0, y1: 1, x0: m.meilleur, x1: m.meilleur, line: { color: PALETTE[1], width: 1, dash: 'dash' } }] });
}

// ------------------------------------------------------------ summary

function noteReglages(m) {
  if (foret(m)) {
    const r = m.reglages_foret;
    return t('for_reglages', { arbres: r.arbres, mtry: r.mtry_utilise, min: r.min_noeud_utilise, fraction: fmt(100 * r.fraction, 1),
      importance: t(r.importance === 'echelle' ? 'for_importance_echelle' : 'for_importance_brute') });
  }
  const r = m.reglages_boost;
  return t('bst_reglages', { loi: t(`famille_${r.loi}`), max: r.arbres, profondeur: r.profondeur, min: r.min_noeud, taux: decimal(r.taux), fraction: fmt(100 * r.fraction, 1) });
}

function rendreResume(conteneur, ctx, m) {
  const q = m.qualite;
  const qualite = foret(m)
    ? (m.classement ? [chiffre(t('ens_bien_classes_oob'), pct(q.bien_classes_oob)), chiffre(t('bien_classes_vc'), pct(q.bien_classes_vc))]
      : [chiffre('R2 OOB', q.r2_oob), chiffre('MSE OOB', q.mse_oob, 4), chiffre('Q2', q.Q2), chiffre('RMSEcv', q.RMSEcv, 4)])
    : [m.loi === 'gaussienne' ? [chiffre('R2', q.r2), chiffre('Q2', q.Q2), chiffre('RMSEcv', q.RMSEcv, 4)] : [chiffre('D2', q.D2), chiffre(`D2 ${t('vc_court')}`, q.D2_vc)],
      m.loi === 'binomiale' ? chiffre(`AUC ${t('vc_court')}`, q.auc_vc) : null, chiffre(t('ens_deviance_vc'), q.deviance_vc, 4), chiffre(t('ens_arbres_titre'), m.meilleur, 0)];
  const fiche = el('div', { class: 'fiche' }, titreFiche(m),
    el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('variables'), m.K, 0), chiffre(t('groupes_vc'), groupesVC(m)),
      m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null),
    el('div', { class: 'fiche-chiffres' }, qualite), ficheTest(m), ficheCondition(m));
  const notes = notesModele(conteneur, ctx, m);
  const textes = [t(foret(m) ? (m.classement ? 'for_resume_classement' : 'for_resume_regression') : `bst_resume_${m.loi}`, { y: m.noms_y[0] }), noteReglages(m),
    t(foret(m) ? 'for_vc_note' : 'bst_vc_note')];
  if (m.manquantes_x) textes.push(t(foret(m) ? 'ens_manquantes' : 'ens_manquantes_boost', { n: m.manquantes_x }));
  if (m.echecs_vc) textes.push(t('ens_echecs_vc', { n: m.echecs_vc }));
  if (!foret(m) && m.au_maximum) textes.push(t('ens_au_maximum'));
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ, textes.map((x) => el('p', { class: 'note' }, x))));
  notes.retrouver();
}

// ------------------------------------------------------------ the model dialog

const nombreOption = (st, cle, defaut, attrs = {}) => el('input', { type: 'number', value: st.options[cle] ?? defaut, style: 'width:6em', ...attrs,
  oninput: (e) => { st.options[cle] = e.target.value === '' ? null : Number(e.target.value); } });
const quantitative = (p, u) => !(p.variables.find((v) => v.uid === u) || {}).qualitative;
const vide = (v) => v === undefined || v === null;
const entier = (v, min, max) => vide(v) || (Number.isInteger(v) && v >= min && v <= max);
const nombre = (v, min, max) => vide(v) || (Number.isFinite(v) && v >= min && v <= max);
const valeur = (o, k, d) => (Number.isFinite(o[k]) ? o[k] : d);

ANALYSES.FOREST = {
  libelle: () => t('type_forest'),
  note: () => t('for_note'),
  roles: ['x', 'y', 'classe'],
  echelle: 'none',
  test: true,  // (a test set, predicted after the fit: forets.R, tester_forest)
  champs: (st) => [
    el('label', {}, t('for_arbres')), el('div', { class: 'ligne' }, nombreOption(st, 'arbres', 500, { min: 10, max: 5000, step: 10 }),
      el('label', { class: 'champ' }, 'mtry', nombreOption(st, 'mtry', '', { min: 1, step: 1, placeholder: t('auto_court'), title: t('for_mtry_aide') }))),
    el('label', {}, t('for_min_noeud')), el('div', { class: 'ligne' }, nombreOption(st, 'min_noeud', '', { min: 1, step: 1, placeholder: t('auto_court'), title: t('for_min_noeud_aide') }),
      el('label', { class: 'champ' }, t('for_fraction'), nombreOption(st, 'fraction', 1, { min: 0.1, max: 1, step: 0.05 }))),
    el('label', {}, t('for_importance')), el('select', { onchange: (e) => { st.options.importance = e.target.value; } },
      [['brute', t('for_importance_brute')], ['echelle', t('for_importance_echelle')]].map(([k, n]) => el('option', { value: k, selected: k === (st.options.importance || 'brute') }, n))),
    el('label', {}, ''), el('p', { class: 'note' }, t('for_reglages_note')),
  ],
  valider: (st, cles, p) => {
    const y = cles('y');
    const c = cles('classe');
    if (y.length + c.length !== 1) return t('for_reponse_requise');
    if (y.length && !quantitative(p, y[0])) return t('for_y_quantitative');
    if (st.croises.length) return t('ens_croises');
    const o = st.options;
    if (!entier(o.arbres, 10, 5000) || !entier(o.mtry, 1, 10000) || !entier(o.min_noeud, 1, 1e6) || !nombre(o.fraction, 0.1, 1)) return t('for_reglages_invalides');
    return '';
  },
  options: (st) => {
    const o = st.options;
    return { arbres: valeur(o, 'arbres', 500), mtry: Number.isFinite(o.mtry) ? o.mtry : null, min_noeud: Number.isFinite(o.min_noeud) ? o.min_noeud : null,
      fraction: valeur(o, 'fraction', 1), importance: o.importance === 'echelle' ? 'echelle' : 'brute' };
  },
};
const LOIS = ['gaussienne', 'binomiale', 'poisson'];
ANALYSES.BOOST = {
  libelle: () => t('type_boost'),
  note: () => t('bst_note'),
  roles: ['x', 'y'],
  echelle: 'none',
  test: true,  // (boosting.R, tester_boost)
  champs: (st) => [
    el('label', {}, t('loi')), el('select', { onchange: (e) => { st.options.loi = e.target.value; } },
      LOIS.map((k) => el('option', { value: k, selected: k === (st.options.loi || 'gaussienne') }, t(`famille_${k}`)))),
    // (option names a forest doesn't use: a model made again as another type doesn't take them)
    el('label', {}, t('bst_arbres')), el('div', { class: 'ligne' }, nombreOption(st, 'arbres_max', 2000, { min: 10, max: 20000, step: 100 }),
      el('label', { class: 'champ', title: t('bst_taux_aide') }, t('bst_taux'), nombreOption(st, 'taux', 0.01, { min: 0.0001, max: 1, step: 0.005 }))),
    el('label', {}, t('bst_profondeur')), el('div', { class: 'ligne' }, nombreOption(st, 'profondeur', 3, { min: 1, max: 10, step: 1, title: t('bst_profondeur_aide') }),
      el('label', { class: 'champ' }, t('bst_min_noeud'), nombreOption(st, 'min_feuille', 10, { min: 1, step: 1 })),
      el('label', { class: 'champ' }, t('for_fraction'), nombreOption(st, 'part', 0.5, { min: 0.05, max: 1, step: 0.05 }))),
    el('label', {}, ''), el('p', { class: 'note' }, t('bst_reglages_note')),
  ],
  valider: (st, cles, p) => {
    const y = cles('y');
    if (cles('classe').length) return t('bst_pas_de_classe');
    if (y.length !== 1) return t('bst_reponse_requise');
    if (!quantitative(p, y[0])) return t('bst_y_quantitative');
    if (st.croises.length) return t('ens_croises');
    const o = st.options;
    if (!entier(o.arbres_max, 10, 20000) || !entier(o.profondeur, 1, 10) || !entier(o.min_feuille, 1, 1e6) || !nombre(o.taux, 0.0001, 1)
      || !nombre(o.part, 0.05, 1)) return t('bst_reglages_invalides');
    return '';
  },
  options: (st) => {
    const o = st.options;
    return { loi: LOIS.includes(o.loi) ? o.loi : 'gaussienne', arbres_max: valeur(o, 'arbres_max', 2000), profondeur: valeur(o, 'profondeur', 3),
      min_feuille: valeur(o, 'min_feuille', 10), taux: valeur(o, 'taux', 0.01), part: valeur(o, 'part', 0.5) };
  },
};

// ------------------------------------------------------------ the report

const blocsResume = (m) => {
  const q = m.qualite;
  const lignes = foret(m)
    ? (m.classement ? { entetes: [t('ens_bien_classes_oob'), t('bien_classes_vc'), t('ens_arbres_titre'), 'mtry'],
      lignes: [[pct(q.bien_classes_oob), pct(q.bien_classes_vc), String(m.reglages_foret.arbres), String(m.reglages_foret.mtry_utilise)]] }
      : { entetes: ['R2 OOB', 'MSE OOB', 'Q2', 'RMSEcv', t('ens_arbres_titre'), 'mtry'],
        lignes: [[fmt(q.r2_oob, 3), fmt(q.mse_oob, 4), fmt(q.Q2, 3), fmt(q.RMSEcv, 4), String(m.reglages_foret.arbres), String(m.reglages_foret.mtry_utilise)]] })
    : { entetes: (m.loi === 'gaussienne' ? ['R2', 'Q2', 'RMSEcv'] : ['D2', `D2 ${t('vc_court')}`]).concat(m.loi === 'binomiale' ? [`AUC ${t('vc_court')}`] : [], [t('ens_arbres_titre')]),
      lignes: [(m.loi === 'gaussienne' ? [fmt(q.r2, 3), fmt(q.Q2, 3), fmt(q.RMSEcv, 4)] : [fmt(q.D2, 3), fmt(q.D2_vc, 3)])
        .concat(m.loi === 'binomiale' ? [fmt(q.auc_vc, 3)] : [], [`${m.meilleur} / ${m.reglages_boost.arbres}`])] };
  return [{ texte: `${t(foret(m) ? 'type_forest' : 'type_boost')} · N = ${m.N} · ${t('variables')} ${m.K} · ${t('groupes_vc')} ${groupesVC(m)}` }, lignes,
    { texte: noteReglages(m) }];
};
const blocsImportance = (m) => (foret(m)
  ? [{ entetes: [t('variable_arbre'), t('ens_permutation'), t('ens_impurete')],
    lignes: liste(m.importance.variable).map((v, i) => [v, fmt(liste(m.importance.permutation)[i], 4), fmt(liste(m.importance.impurete)[i], 4)]) }]
  : [{ entetes: [t('variable_arbre'), t('ens_influence')], lignes: liste(m.influence.variable).map((v, i) => [v, `${fmt(liste(m.influence.pourcentage)[i], 1)}${pourcent()}`]) }]);
const blocsClassement = (m) => {
  const r = m.classification[foret(m) ? 'oob' : 'vc'];
  const classes = liste(r.classes);
  return [{ texte: `${t(foret(m) ? 'ens_mode_oob' : 'ens_mode_vc')} : ${t('bien_classes')} ${pct(r.global)}` },
    { entetes: [`${t('observee')} \\ ${t('predite')}`, ...classes, t('bien_classes')], lignes: classes.map((k, i) => [k, ...r.table[i].map(String), pct(liste(r.correct)[i])]) }];
};
const partiesEnsemble = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'ens_importance', vue: 'ens_importance', defaut: true, tableau: blocsImportance },
  { cle: 'ens_erreur', vue: 'ens_erreur', defaut: true },
  { cle: 'ens_obspred', vue: 'ens_obspred', defaut: true, si: (m) => !classe(m) || !foret(m) },
  { cle: 'ens_classement', vue: 'ens_classement', defaut: true, si: classe, tableau: blocsClassement },
  { cle: 'ens_dependance', vue: 'ens_dependance', variantes: async (m) => liste(m.variables_x).map((variable) => ({ variable })) },
];
PARTIES_TYPES.FOREST = partiesEnsemble;
PARTIES_TYPES.BOOST = partiesEnsemble;

// ------------------------------------------------------------ registration

const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estEnsemble(m) ? rendreResume(c, ctx, m) : resume(c, ctx, p, loc);
} };
Object.assign(VUES, {
  ens_importance: { rendre: rendreImportance, titre: (m) => `${m.nom} · ${t(m.type === 'FOREST' ? 'ens_importance' : 'ens_influence')}` },
  ens_dependance: { rendre: rendreDependance, titre: (m) => `${m.nom} · ${t('ens_dependance')}` },
  ens_obspred: { rendre: rendreObsPred, titre: (m) => `${m.nom} · ${t('obspred')}` },
  ens_classement: { rendre: rendreClassement, titre: (m) => `${m.nom} · ${t('classement')}` },
  ens_erreur: { rendre: rendreErreur, titre: (m) => `${m.nom} · ${t('ens_erreur')}` },
});
Object.assign(ICONES_VUES, { ens_importance: 'chart-bar', ens_dependance: 'chart-spline', ens_obspred: 'sigma', ens_classement: 'grid-3x3', ens_erreur: 'chart-line' });
VUES_TYPES.FOREST = (m) => ['resume', 'ens_importance', 'ens_dependance', 'ens_erreur', m.classement ? 'ens_classement' : 'ens_obspred', 'liste_obs', 'predictions'];
VUES_TYPES.BOOST = (m) => ['resume', 'ens_importance', 'ens_dependance', 'ens_erreur', 'ens_obspred'].concat(m.loi === 'binomiale' ? ['ens_classement'] : [], ['liste_obs', 'predictions']);

ajouterTextes({
  type_forest: 'Forêt aléatoire (régression ou classement)', type_boost: 'Arbres boostés (BRT)',
  ens_importance: 'Importance des variables', ens_influence: 'Influence relative', ens_dependance: 'Dépendance partielle', ens_erreur: 'Erreur selon le nombre d’arbres',
  ens_permutation: 'Par permutation', ens_impurete: 'Par l’impureté', ens_reduction: 'Réduction de l’erreur',
  ens_perm_note: 'Importance par permutation : la hausse de l’erreur hors sac (MSE, ou part mal classée) quand les valeurs de la variable sont permutées au hasard. Proche de 0 ou négative : la variable n’aide pas à prédire.',
  ens_perm_note_echelle: 'Importance par permutation, divisée par son erreur-type : la hausse de l’erreur hors sac quand les valeurs de la variable sont permutées, en nombre d’erreurs-types.',
  ens_impurete_note: 'Importance par l’impureté : la somme des baisses d’impureté (variance, ou indice de Gini) de ses coupures sur tous les arbres. Elle favorise les variables qui ont beaucoup de valeurs distinctes ; l’importance par permutation ne le fait pas.',
  ens_influence_note: 'Influence relative (Friedman, 2001) : la somme des réductions d’erreur de ses coupures sur les arbres du modèle, ramenée à 100 %.',
  ens_obspred: 'Observé / prédit', ens_classement: 'Classement', ens_premieres: 'les {n} plus importantes',
  ens_toutes: 'toutes', ens_calcul_dependance: 'Calcul : la dépendance partielle de {n} variable(s)…', ens_sans_dependance: 'Pas de dépendance partielle.',
  ens_dependance_note: 'Dépendance partielle (Friedman, 2001) : la prédiction moyenne de la forêt sur ses observations quand la variable prend chaque valeur de la grille, les autres X inchangés. Sous la courbe, les valeurs observées.',
  ens_dependance_note_classe: 'Dépendance partielle (Friedman, 2001) : la part moyenne des votes de la classe sur les observations quand la variable prend chaque valeur de la grille, les autres X inchangés. Sous la courbe, les valeurs observées.',
  ens_dependance_note_boost: 'Dépendance partielle (Friedman, 2001) : la valeur moyenne du modèle sur ses observations quand la variable prend chaque valeur de la grille, les autres X inchangés (calculée à l’échelle du lien, puis à celle de la réponse). Sous la courbe, les valeurs observées.',
  ens_echelle_proba: 'probabilité', ens_echelle_logit: 'logit', ens_echelle_moyenne: 'moyenne', ens_echelle_log: 'log de la moyenne',
  ens_mode_oob: 'Hors sac (OOB)', ens_mode_vc: 'Validation croisée', ens_mode_ajuste: 'Ajusté',
  ens_mode_oob_note: 'Hors sac : chaque observation prédite par les seuls arbres dont le tirage ne la contient pas, une prédiction honnête.',
  ens_mode_vc_note: 'Validation croisée : chaque groupe prédit par le modèle refait sans lui.',
  ens_mode_ajuste_note: 'Ajusté : le modèle sur les observations qui l’ont fait ; une forêt les prédit presque parfaitement, ce n’est pas sa qualité de prédiction.',
  ens_mode_ajuste_note_boost: 'Ajusté : le modèle (ses arbres jusqu’au nombre retenu) sur les observations qui l’ont fait ; sa qualité de prédiction est celle de la validation croisée.',
  ens_classement_note_oob: 'Hors sac : chaque observation classée par les seuls arbres dont le tirage ne la contient pas (à égalité de votes, ranger tire au sort). Les courbes ROC suivent la part des votes de chaque classe.',
  ens_classement_note_vc: 'Validation croisée : chaque groupe classé par une forêt refaite sans lui. Les courbes ROC suivent la part des votes de chaque classe.',
  ens_classement_note_ajuste: 'Ajusté : la forêt entière sur les observations qui l’ont faite, presque toujours bien classées ; ce n’est pas sa qualité de prédiction.',
  ens_classement_note_boost: 'Une réponse 0/1 classée à une probabilité de 0,5 ; les courbes ROC suivent la probabilité prédite (l’AUC ne dépend pas du seuil).',
  ens_erreur_note: 'L’erreur hors sac (MSE) après 1, 2… arbres : elle doit s’être stabilisée bien avant le dernier ; sinon, plus d’arbres.',
  ens_erreur_note_classe: 'La part mal classée hors sac après 1, 2… arbres : elle doit s’être stabilisée bien avant le dernier ; sinon, plus d’arbres.',
  ens_erreur_oob: 'Erreur hors sac', ens_arbres_mot: 'arbres', ens_nb_arbres: 'Nombre d’arbres', ens_part_mal_classees: 'Part mal classée (hors sac)', ens_mse_oob: 'MSE hors sac',
  ens_deviance_note: 'La déviance moyenne des observations du modèle (en pointillé) et celle de la validation croisée, chaque groupe prédit par le boosting refait sans lui. Le nombre d’arbres retenu est celui de la plus petite déviance de validation croisée : {n} sur {max}.',
  ens_au_maximum: 'La déviance de validation croisée baissait encore au dernier arbre : augmenter le nombre d’arbres ou le taux d’apprentissage.',
  ens_deviance_apprentissage: 'Déviance (ajustement)', ens_deviance_vc: 'Déviance (VC)', ens_retenu: 'Retenu', ens_deviance_moyenne: 'Déviance moyenne',
  ens_bien_classes_oob: 'Bien classées (OOB)', ens_arbres_titre: 'Arbres', vc_court: 'VC',
  ens_manquantes: '{n} observations où une valeur X manque sont laissées de côté (ranger ne prend pas de valeurs manquantes).',
  ens_manquantes_boost: '{n} observations où une valeur X manque sont laissées de côté (comme pour une forêt : les deux portent ainsi sur les mêmes observations).',
  ens_echecs_vc: '{n} groupes de validation croisée sans prédiction (sans eux, une seule valeur de la réponse reste).',
  ens_croises: 'Les arbres font eux-mêmes leurs interactions : retirer les termes croisés.',
  for_note: 'Une variable Y quantitative (forêt de régression) ou une variable de classe (forêt de classement), et les variables X par leurs valeurs brutes (ranger). Chaque arbre est grandi sur un tirage bootstrap des observations, chaque coupure choisie parmi mtry variables tirées au hasard ; erreur hors sac, et validation croisée par les groupes de validation croisée (un site, par exemple). Les observations où une valeur X manque sont laissées de côté.',
  for_arbres: 'Nombre d’arbres', for_mtry_aide: 'Variables candidates à chaque coupure ; par défaut, la racine du nombre de variables X arrondie en dessous (ranger).',
  for_min_noeud: 'Taille des nœuds', for_min_noeud_aide: 'La taille minimale d’un nœud (min.node.size de ranger) ; par défaut 5 en régression, 1 en classement.',
  for_fraction: 'part tirée', for_importance: 'Importance par permutation', for_importance_brute: 'non mise à l’échelle', for_importance_echelle: 'divisée par son erreur-type',
  for_reglages_note: 'Par défaut : 500 arbres, mtry et la taille des nœuds de ranger, des tirages de 100 % des observations avec remise ; graine 1, un seul fil de calcul (les mêmes arbres à chaque fois).',
  for_reponse_requise: 'Une forêt demande une réponse : une variable Y quantitative (régression) ou une variable de classe (classement), pas les deux.',
  for_y_quantitative: 'La réponse d’une forêt de régression est quantitative ; une variable qualitative prend le rôle Classe (forêt de classement).',
  for_reglages_invalides: 'Réglages de la forêt : de 10 à 5000 arbres, mtry et la taille des nœuds des entiers positifs (ou vides), une part tirée entre 0,1 et 1.',
  for_reglages: '{arbres} arbres, mtry = {mtry}, nœuds de {min} observations, tirages de {fraction} % avec remise ; importance par permutation {importance}.',
  for_resume_regression: 'Forêt de régression de {y} : chaque arbre prédit la moyenne de sa feuille, la forêt la moyenne de ses arbres.',
  for_resume_classement: 'Forêt de classement de {y} : chaque arbre vote pour une classe, la forêt prédit la plus votée.',
  for_vc_note: 'Hors sac : chaque observation prédite par les arbres dont le tirage ne la contient pas. Validation croisée : chaque groupe prédit par une forêt refaite sans lui.',
  bst_note: 'Une variable Y quantitative : une mesure (loi normale), une présence-absence 0/1 (binomiale) ou des comptages (Poisson), et les variables X par leurs valeurs brutes. Des arbres de régression ajoutés un à un au gradient de la déviance (gradient boosting, comme gbm) ; le nombre d’arbres choisi par la validation croisée par les groupes de validation croisée (un site, par exemple). Les observations où une valeur X manque sont laissées de côté.',
  bst_arbres: 'Arbres (au plus)', bst_taux: 'taux d’apprentissage', bst_taux_aide: 'La part de chaque arbre ajoutée au modèle (shrinkage) : plus petite, il faut plus d’arbres, mais le modèle prédit mieux (au moins 1000 arbres, conseillent Elith et al., 2008).',
  bst_profondeur: 'Coupures par arbre', bst_profondeur_aide: 'La profondeur d’interaction de gbm : le nombre de coupures de chaque arbre, la meilleure d’abord. 1 : un modèle additif ; 2 : des interactions de deux variables…',
  bst_min_noeud: 'par feuille (au moins)',
  bst_reglages_note: 'Par défaut : au plus 2000 arbres, un taux d’apprentissage de 0,01, 3 coupures par arbre, au moins 10 observations par feuille, des tirages de 50 % sans remise (graine 1). La validation croisée refait le boosting sans chaque groupe : le calcul est d’autant plus long qu’il y a de groupes et d’arbres.',
  bst_pas_de_classe: 'Des arbres boostés prennent une variable Y quantitative (une variable 0/1 pour la loi binomiale), pas une classe.',
  bst_reponse_requise: 'Des arbres boostés demandent une réponse : une variable Y quantitative.',
  bst_y_quantitative: 'La réponse d’arbres boostés est quantitative : une mesure, une variable 0/1 (loi binomiale) ou des comptages (Poisson).',
  bst_reglages_invalides: 'Réglages du boosting : de 10 à 20000 arbres, de 1 à 10 coupures, une taille de feuille entière positive, un taux entre 0,0001 et 1, une part tirée entre 0,05 et 1.',
  bst_reglages: 'Loi : {loi} ; au plus {max} arbres de {profondeur} coupures, au moins {min} observations par feuille, un taux d’apprentissage de {taux}, des tirages de {fraction} % sans remise.',
  bst_resume_gaussienne: 'Arbres boostés de {y} (perte quadratique) : le modèle est la somme de ses arbres.',
  bst_resume_binomiale: 'Arbres boostés de {y}, une présence-absence (déviance de Bernoulli) : la somme des arbres est le logit de la probabilité.',
  bst_resume_poisson: 'Arbres boostés de {y}, des comptages (déviance de Poisson) : la somme des arbres est le log de la moyenne.',
  bst_vc_note: 'Validation croisée : chaque groupe prédit après chaque arbre par le boosting refait sans lui ; le nombre d’arbres est celui de la plus petite déviance (gbm.perf, méthode « cv »).',
}, {
  type_forest: 'Random forest (regression or classification)', type_boost: 'Boosted trees (BRT)',
  ens_importance: 'Variable importance', ens_influence: 'Relative influence', ens_dependance: 'Partial dependence', ens_erreur: 'Error against the number of trees',
  ens_permutation: 'By permutation', ens_impurete: 'By impurity', ens_reduction: 'Error reduction',
  ens_perm_note: 'Permutation importance: the rise of the out-of-bag error (MSE, or share misclassified) when the variable’s values are randomly permuted. Near 0 or negative: the variable doesn’t help to predict.',
  ens_perm_note_echelle: 'Permutation importance divided by its standard error: the rise of the out-of-bag error when the variable’s values are permuted, in standard errors.',
  ens_impurete_note: 'Impurity importance: the sum of the impurity decreases (variance, or Gini index) of its splits over all the trees. It favours variables with many distinct values; permutation importance doesn’t.',
  ens_influence_note: 'Relative influence (Friedman, 2001): the sum of the error reductions of its splits over the model’s trees, scaled to 100%.',
  ens_obspred: 'Observed vs predicted', ens_classement: 'Classification', ens_premieres: 'the {n} most important',
  ens_toutes: 'all', ens_calcul_dependance: 'Computing: the partial dependence of {n} variable(s)…', ens_sans_dependance: 'No partial dependence.',
  ens_dependance_note: 'Partial dependence (Friedman, 2001): the forest’s mean prediction over its observations when the variable takes each value of the grid, the other X unchanged. Under the curve, the observed values.',
  ens_dependance_note_classe: 'Partial dependence (Friedman, 2001): the class’s mean share of the votes over the observations when the variable takes each value of the grid, the other X unchanged. Under the curve, the observed values.',
  ens_dependance_note_boost: 'Partial dependence (Friedman, 2001): the model’s mean value over its observations when the variable takes each value of the grid, the other X unchanged (computed on the link scale, then on the response’s). Under the curve, the observed values.',
  ens_echelle_proba: 'probability', ens_echelle_logit: 'logit', ens_echelle_moyenne: 'mean', ens_echelle_log: 'log of the mean',
  ens_mode_oob: 'Out of bag (OOB)', ens_mode_vc: 'Cross-validation', ens_mode_ajuste: 'Fitted',
  ens_mode_oob_note: 'Out of bag: each observation predicted by the trees whose draw doesn’t hold it, an honest prediction.',
  ens_mode_vc_note: 'Cross-validation: each group predicted by the model rebuilt without it.',
  ens_mode_ajuste_note: 'Fitted: the model on the observations it was built on; a forest predicts them almost perfectly, which isn’t its predictive ability.',
  ens_mode_ajuste_note_boost: 'Fitted: the model (its trees up to the number retained) on the observations it was fitted on; its predictive ability is the cross-validation’s.',
  ens_classement_note_oob: 'Out of bag: each observation classified by the trees whose draw doesn’t hold it (ranger draws lots between tied votes). The ROC curves follow each class’s share of the votes.',
  ens_classement_note_vc: 'Cross-validation: each group classified by a forest rebuilt without it. The ROC curves follow each class’s share of the votes.',
  ens_classement_note_ajuste: 'Fitted: the whole forest on the observations it was built on, nearly always correctly classified; not its predictive ability.',
  ens_classement_note_boost: 'A 0/1 response classified at a probability of 0.5; the ROC curves follow the predicted probability (the AUC doesn’t depend on the threshold).',
  ens_erreur_note: 'The out-of-bag error (MSE) after 1, 2… trees: it should have levelled off well before the last one; otherwise, more trees.',
  ens_erreur_note_classe: 'The out-of-bag share misclassified after 1, 2… trees: it should have levelled off well before the last one; otherwise, more trees.',
  ens_erreur_oob: 'Out-of-bag error', ens_arbres_mot: 'trees', ens_nb_arbres: 'Number of trees', ens_part_mal_classees: 'Share misclassified (out of bag)', ens_mse_oob: 'Out-of-bag MSE',
  ens_deviance_note: 'The mean deviance of the model’s observations (dotted) and the cross-validated one, each group predicted by the boosting redone without it. The number of trees retained is that of the smallest cross-validated deviance: {n} of {max}.',
  ens_au_maximum: 'The cross-validated deviance was still falling at the last tree: raise the number of trees or the learning rate.',
  ens_deviance_apprentissage: 'Deviance (fit)', ens_deviance_vc: 'Deviance (CV)', ens_retenu: 'Retained', ens_deviance_moyenne: 'Mean deviance',
  ens_bien_classes_oob: 'Correct (OOB)', ens_arbres_titre: 'Trees', vc_court: 'CV',
  ens_manquantes: '{n} observations with a missing X value are left out (ranger takes no missing values).',
  ens_manquantes_boost: '{n} observations with a missing X value are left out (as for a forest: both then use the same observations).',
  ens_echecs_vc: '{n} cross-validation groups without a prediction (without them, a single response value remains).',
  ens_croises: 'Trees make their own interactions: remove the cross terms.',
  for_note: 'A quantitative Y variable (regression forest) or a class variable (classification forest), and the X variables by their raw values (ranger). Each tree is grown on a bootstrap draw of the observations, each split chosen among mtry randomly drawn variables; out-of-bag error, and cross-validation by the cross-validation groups (a site, say). Observations with a missing X value are left out.',
  for_arbres: 'Number of trees', for_mtry_aide: 'Candidate variables at each split; by default, the square root of the number of X variables rounded down (ranger).',
  for_min_noeud: 'Node size', for_min_noeud_aide: 'The minimum node size (ranger’s min.node.size); by default 5 for regression, 1 for classification.',
  for_fraction: 'share drawn', for_importance: 'Permutation importance', for_importance_brute: 'unscaled', for_importance_echelle: 'divided by its standard error',
  for_reglages_note: 'By default: 500 trees, ranger’s mtry and node size, draws of 100% of the observations with replacement; seed 1, a single thread (the same trees every time).',
  for_reponse_requise: 'A forest needs a response: a quantitative Y variable (regression) or a class variable (classification), not both.',
  for_y_quantitative: 'A regression forest’s response is quantitative; a qualitative variable takes the Class role (classification forest).',
  for_reglages_invalides: 'Forest settings: from 10 to 5000 trees, mtry and the node size positive whole numbers (or empty), a share drawn between 0.1 and 1.',
  for_reglages: '{arbres} trees, mtry = {mtry}, nodes of {min} observations, draws of {fraction}% with replacement; permutation importance {importance}.',
  for_resume_regression: 'Regression forest of {y}: each tree predicts its leaf’s mean, the forest the mean of its trees.',
  for_resume_classement: 'Classification forest of {y}: each tree votes for a class, the forest predicts the most voted.',
  for_vc_note: 'Out of bag: each observation predicted by the trees whose draw doesn’t hold it. Cross-validation: each group predicted by a forest rebuilt without it.',
  bst_note: 'A quantitative Y variable: a measurement (normal distribution), a 0/1 presence-absence (binomial) or counts (Poisson), and the X variables by their raw values. Regression trees added one by one to the gradient of the deviance (gradient boosting, as gbm); the number of trees chosen by cross-validation by the cross-validation groups (a site, say). Observations with a missing X value are left out.',
  bst_arbres: 'Trees (at most)', bst_taux: 'learning rate', bst_taux_aide: 'The share of each tree added to the model (shrinkage): smaller, more trees are needed, but the model predicts better (at least 1000 trees, Elith et al., 2008, advise).',
  bst_profondeur: 'Splits per tree', bst_profondeur_aide: 'gbm’s interaction depth: the number of splits of each tree, the best first. 1: an additive model; 2: interactions of two variables…',
  bst_min_noeud: 'per leaf (at least)',
  bst_reglages_note: 'By default: at most 2000 trees, a learning rate of 0.01, 3 splits per tree, at least 10 observations per leaf, draws of 50% without replacement (seed 1). Cross-validation redoes the boosting without each group: the more groups and trees, the longer it takes.',
  bst_pas_de_classe: 'Boosted trees take a quantitative Y variable (a 0/1 variable for the binomial distribution), not a class.',
  bst_reponse_requise: 'Boosted trees need a response: a quantitative Y variable.',
  bst_y_quantitative: 'The response of boosted trees is quantitative: a measurement, a 0/1 variable (binomial distribution) or counts (Poisson).',
  bst_reglages_invalides: 'Boosting settings: from 10 to 20000 trees, from 1 to 10 splits, a positive whole leaf size, a rate between 0.0001 and 1, a share drawn between 0.05 and 1.',
  bst_reglages: 'Distribution: {loi}; at most {max} trees of {profondeur} splits, at least {min} observations per leaf, a learning rate of {taux}, draws of {fraction}% without replacement.',
  bst_resume_gaussienne: 'Boosted trees of {y} (squared-error loss): the model is the sum of its trees.',
  bst_resume_binomiale: 'Boosted trees of {y}, a presence-absence (Bernoulli deviance): the sum of the trees is the logit of the probability.',
  bst_resume_poisson: 'Boosted trees of {y}, counts (Poisson deviance): the sum of the trees is the log of the mean.',
  bst_vc_note: 'Cross-validation: each group predicted after each tree by the boosting redone without it; the number of trees is that of the smallest deviance (gbm.perf, method “cv”).',
});
