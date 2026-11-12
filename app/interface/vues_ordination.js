// Redundancy analysis (RDA) and correspondence analysis (CA) (moteur/R/ordination.R): their
// settings in the model dialog, their views (the ordination plot, eigenvalues, permutation tests,
// contributions, summary), their report parts and their strings. Imported by app.js.
import { t, ajouterTextes } from './i18n.js';
import { el, fmt } from './ui.js';
import { demander } from './api.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, controleCouleur, controleEtiquettes, pointsObservations, tableau,
  notesModele, valeurP } from './panneaux.js';
import { ANALYSES } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const ETIQUETTES_MAX = 100;  // labels written by each point up to this many
const GRIS = '#9aa7b8';
const estOrdination = (m) => !!m && !!m.analyse && ['RDA', 'CA'].includes(m.type);
const axes = (m) => liste(m.axes);
const libelleAxe = (m, k) => `${axes(m)[k]} (${fmt(100 * liste(m.valeurs_propres.part)[k], 1)} %)`;
const caseACocher = (libelle, valeur, changer) => el('label', { class: 'champ' },
  el('input', { type: 'checkbox', checked: valeur, onchange: (e) => changer(e.target.checked) }), libelle);
const chiffre = (k, v, d = 3) => el('span', {}, el('small', {}, k), ' ', el('b', {}, typeof v === 'number' ? fmt(v, d) : v ?? '—'));
const pValeur = (v) => (v === null || v === undefined ? '—' : valeurP(v));
// a small fixed table: headers, rows of cells, `alerte(i, j)` for a cell in red (p ≤ 0.05)
function tableCompacte(entetes, lignes, alerte) {
  return el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, entetes.map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
    el('tbody', {}, lignes.map((l, i) => el('tr', {}, l.map((v, j) => el('td', { class: (j ? 'num' : '') + (alerte && alerte(i, j) ? ' alerte' : '') }, v))))));
}
// the rows of a data.frame sent by R (columns), as arrays of values
const lignesDe = (df, cles) => { const n = liste((df || {})[cles[0]]).length; return Array.from({ length: n }, (_, i) => cles.map((k) => liste(df[k])[i])); };

// ------------------------------------------------------------ the ordination plot

// RDA: a triplot (observations, responses Y, arrows of the quantitative X and centroids of the
// categories); CA: the biplot of rows and columns. Scaling 1 or 2 (3 for a CA) as vegan draws
// them; the arrows are lengthened to fill the plot (vegan: ordiArrowMul), by the factor shown.
function rendreOrdination(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const rda = m.type === 'RDA';
  const noms = axes(m);
  if (!m.ordination[String(loc.echelle)]) loc.echelle = 2;
  loc.a ??= 1;
  loc.b ??= Math.min(2, noms.length);
  loc.sites ??= rda ? 'wa' : 'oui';
  loc.reponses ??= true;
  loc.x ??= true;
  const o = m.ordination[String(loc.echelle)];
  const a = Math.min(loc.a, noms.length) - 1;
  const b = Math.min(loc.b, noms.length) - 1;
  const col = (df, k) => liste((df || {})[noms[k]]);
  const options = noms.map((n, i) => [i + 1, libelleAxe(m, i)]);
  const echelles = rda ? [[1, t('ord_echelle_1')], [2, t('ord_echelle_2')]] : [[1, t('ord_ca_echelle_1')], [2, t('ord_ca_echelle_2')], [3, t('ord_ca_echelle_3')]];
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix(t('ord_echelle'), echelles, loc.echelle, (v) => { loc.echelle = +v; ctx.rafraichir(); }),
    choix(t('axe_x'), options, loc.a, (v) => { loc.a = +v; ctx.rafraichir(); }),
    noms.length > 1 ? choix(t('axe_y'), options, loc.b, (v) => { loc.b = +v; ctx.rafraichir(); }) : null,
    rda ? choix(t('observations'), [['wa', t('ord_wa')], ['lc', t('ord_lc')], ['', t('aucune')]], loc.sites, (v) => { loc.sites = v; ctx.rafraichir(); })
      : caseACocher(t('observations'), !!loc.sites, (v) => { loc.sites = v ? 'oui' : ''; ctx.rafraichir(); }),
    caseACocher(rda ? t('ord_reponses') : t('variables'), loc.reponses, (v) => { loc.reponses = v; ctx.rafraichir(); }),
    rda ? caseACocher(t('ord_x'), loc.x, (v) => { loc.x = v; ctx.rafraichir(); }) : null,
    loc.sites ? controleCouleur(ctx, m, loc) : null,
    loc.sites ? controleEtiquettes(ctx, m, loc) : null), graphe);
  const un = noms.length < 2;  // (a single axis: the points along it)
  const y = (v) => (un ? v.map(() => 0) : v);
  const sites = rda && loc.sites === 'lc' ? o.lc : o.sites;
  const traces = [];
  const etendue = [[], []];  // the coordinates shown, for the length of the arrows
  if (loc.sites) {
    const xs = col(sites, a);
    const ys = y(col(sites, b));
    traces.push(...pointsObservations(ctx, { ...m, nom: t('observations') }, loc, xs, ys));  // (named for the legend)
    etendue[0].push(...xs); etendue[1].push(...ys);
  }
  const nomsR = liste(o.noms_reponses);
  if (loc.reponses && nomsR.length) {
    const xr = col(o.reponses, a);
    const yr = y(col(o.reponses, b));
    traces.push({ type: 'scatter', mode: nomsR.length <= ETIQUETTES_MAX ? 'markers+text' : 'markers', name: rda ? t('ord_reponses') : t('variables'),
      x: xr, y: yr, text: nomsR, meta: 'variables', customdata: nomsR, textposition: 'top center', textfont: { size: 10, color: PALETTE[1] },
      marker: { color: PALETTE[1], symbol: 'diamond', size: 8 }, hovertemplate: '<b>%{text}</b><br>%{x:.3f}, %{y:.3f}<extra></extra>' });
    etendue[0].push(...xr); etendue[1].push(...yr);
  }
  const nomsC = liste(o.noms_centroides);
  if (rda && loc.x && nomsC.length) {
    const xc = col(o.centroides, a);
    const yc = y(col(o.centroides, b));
    traces.push({ type: 'scatter', mode: 'markers+text', name: t('ord_centroides'), x: xc, y: yc, text: nomsC, meta: 'variables', customdata: nomsC,
      textposition: 'bottom center', textfont: { size: 10, color: PALETTE[0] }, marker: { symbol: 'triangle-up', size: 10, color: PALETTE[0] },
      hovertemplate: '<b>%{text}</b><br>%{x:.3f}, %{y:.3f}<extra></extra>' });
    etendue[0].push(...xc); etendue[1].push(...yc);
  }
  const annotations = [];
  const nomsB = liste(o.noms_biplot);
  if (rda && loc.x && nomsB.length) {
    const bx = col(o.biplot, a);
    const by = y(col(o.biplot, b));
    const max = (v) => Math.max(0, ...v.filter(Number.isFinite).map(Math.abs));
    // the arrows fill three quarters of the plot (vegan: ordiArrowMul)
    let mul = 0.75 * Math.min(max(bx) > 0 && max(etendue[0]) > 0 ? max(etendue[0]) / max(bx) : Infinity,
      max(by) > 0 && max(etendue[1]) > 0 ? max(etendue[1]) / max(by) : Infinity);
    if (!Number.isFinite(mul) || mul <= 0) mul = 1;
    nomsB.forEach((_, i) => annotations.push({ x: mul * bx[i], y: mul * by[i], ax: 0, ay: 0, xref: 'x', yref: 'y', axref: 'x', ayref: 'y', text: '',
      showarrow: true, arrowhead: 2, arrowsize: 1.2, arrowwidth: 1.6, arrowcolor: PALETTE[0] }));
    traces.push({ type: 'scatter', mode: 'text', name: t('ord_fleches', { k: fmt(mul, 2) }), x: bx.map((v) => 1.1 * mul * v), y: by.map((v) => 1.1 * mul * v),
      text: nomsB, meta: 'variables', customdata: nomsB.map((_, i) => [fmt(bx[i], 3), fmt(by[i], 3)]), textfont: { size: 11, color: PALETTE[0] },
      hovertemplate: '<b>%{text}</b><br>%{customdata[0]}, %{customdata[1]}<extra></extra>' });
  }
  // (the same scale on both axes, as an ordination is read: the plot area narrows rather than the
  // axes stretching)
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${rda ? 'RDA' : 'CA'} · ${t('ord_echelle')} ${loc.echelle}` }, annotations,
    xaxis: { title: { text: libelleAxe(m, a) }, zeroline: true, constrain: 'domain' },
    yaxis: { title: { text: un ? '' : libelleAxe(m, b) }, zeroline: true, scaleanchor: 'x', scaleratio: 1, constrain: 'domain' } });
  if (loc.sites) brancherSelection(graphe, ctx, m.id);
}

// ------------------------------------------------------------ eigenvalues

function rendreValeursPropres(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const vp = m.valeurs_propres;
  const ax = liste(vp.axe);
  const part = liste(vp.part);
  const cumul = liste(vp.cumul);
  const contraint = liste(vp.contraint);
  const n = Math.min(ax.length, 30);
  const rda = m.type === 'RDA';
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t(rda ? 'ord_valeurs_propres_rda' : 'ord_valeurs_propres_ca', { r: m.rang_contraint ?? 0 })), graphe, bas);
  const c = couleursTheme();
  tracer(ctx, graphe, [
    { type: 'bar', name: t('ord_part_inertie'), x: ax.slice(0, n), y: part.slice(0, n).map((v) => 100 * v),
      marker: { color: contraint.slice(0, n).map((k) => (k || !rda ? PALETTE[0] : GRIS)) }, hovertemplate: '<b>%{x}</b><br>%{y:.2f} %<extra></extra>' },
    { type: 'scatter', mode: 'lines+markers', name: t('ord_cumul'), x: ax.slice(0, n), y: cumul.slice(0, n).map((v) => 100 * v), yaxis: 'y2',
      line: { color: c.doux, width: 1.5 }, marker: { size: 5, color: c.doux }, hovertemplate: '<b>%{x}</b><br>%{y:.1f} %<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('valeurs_propres')}` }, dragmode: false, yaxis: { title: { text: t('ord_part_inertie') } }, margin: { r: 54 },
    yaxis2: { overlaying: 'y', side: 'right', range: [0, 101], tickmode: 'array', tickvals: [0, 25, 50, 75, 100], showgrid: false, zeroline: false,
      title: { text: `${t('ord_cumul')} (%)` } } });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: ax.length, nom: 'Eigenvalues',
    colonnes: [{ cle: 'axe', titre: t('ord_axe') }, { cle: 'valeur_propre', titre: t('valeur_propre'), num: true, decimales: 5 },
      { cle: 'part', titre: t('ord_part_inertie'), num: true, decimales: 2 }, { cle: 'cumul', titre: `${t('ord_cumul')} (%)`, num: true, decimales: 2 }]
      .concat(rda ? [{ cle: 'contraint', titre: t('ord_contraint') }] : []),
    valeur: (i, k) => (k === 'part' || k === 'cumul' ? 100 * liste(vp[k])[i] : k === 'contraint' ? (contraint[i] ? t('ord_oui') : '') : liste(vp[k])[i]),
  });
}

// ------------------------------------------------------------ the tests of an RDA

// The global test (computed with the model), the tests by axis and by term (computed when the
// view opens), the forward selection and the variance partitioning (on request). The engine
// keeps them with the model: the Methods text, Export and the R script then have them too.
async function rendreTestsRDA(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const cache = ((loc.cache ??= {}).tests ??= {});
  if (cache.id !== m.id) Object.assign(cache, { id: m.id, res: null, selection: null, partition: null });
  const B = m.test.permutations || 0;
  const tg = m.test;
  const fiche = el('div', { class: 'fiche' },
    el('div', { class: 'fiche-titre' }, el('b', {}, t('ord_test_global'))),
    el('div', { class: 'fiche-chiffres' }, chiffre(t('ord_pseudo_f'), tg.F, 3), chiffre(t('ddl'), `${liste(tg.df).join(', ')}`), chiffre('p', pValeur(tg.p)),
      chiffre(t('nb_permutations'), B, 0), m.strates ? chiffre(t('ord_dans'), m.strates) : null, chiffre(t('ord_graine'), (m.options || {}).graine ?? 1, 0)));
  if (!B) { conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, el('p', { class: 'note' }, t('ord_sans_permutations')))); return; }
  if (!cache.res) {
    conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, el('p', { class: 'infos attente' }, t('ord_calcul_tests', { n: B }))));
    cache.res = await ctx.demander('rda_tests', { modele: m.id });
  }
  const r = cache.res;
  const alerteP = (k) => (rows) => (i, j) => j === k && rows[i][k] !== null && rows[i][k] !== undefined && Number(rows[i][k]) <= 0.05;
  const tableTests = (df, premiere) => {
    const rows = lignesDe(df, [premiere, 'ddl', 'inertie', 'F', 'p']);
    return tableCompacte([premiere === 'axe' ? t('ord_axe') : t('terme'), t('ddl'), t('ord_inertie'), t('ord_pseudo_f'), 'p'],
      rows.map((l) => [l[0], fmt(l[1], 0), fmt(l[2], 5), l[3] === null ? '' : fmt(l[3], 3), l[4] === null ? '' : pValeur(l[4])]), alerteP(4)(rows));
  };
  // the forward selection and the partitioning, asked for by their buttons
  const demande = async (cle, nom) => {
    try { cache[cle] = await ctx.demander(nom, { modele: m.id }); } catch (e) { cache[cle] = { erreur: e.message }; }
    ctx.rafraichir();
  };
  const bouton = (cle, nom, libelle) => el('button', { onclick: (e) => { e.target.disabled = true; e.target.textContent = t('calcul'); demande(cle, nom); } }, libelle);
  const s = cache.selection;
  const selection = !s ? bouton('selection', 'rda_selection', t('ord_lancer_selection')) : s.erreur ? el('p', { class: 'erreur' }, s.erreur) : [
    el('p', { class: 'note' }, t('ord_selection_note', { r2: fmt(s.r2_ajuste_tous, 3), seuil: fmt(s.seuil, 2) })),
    s.etapes ? tableCompacte([t('terme'), t('r2_ajuste'), t('ord_pseudo_f'), 'p', t('ord_retenu')],
      lignesDe(s.etapes, ['terme', 'r2_ajuste', 'F', 'p', 'retenu']).map((l) => [l[0], fmt(l[1], 4), l[2] === null ? '' : fmt(l[2], 3), pValeur(l[3]), l[4] ? t('ord_oui') : t('ord_non')])) : null,
    el('p', { class: 'note' }, `${t('ord_arret_' + s.arret)}${liste(s.retenus).length ? ` ${t('ord_retenus', { termes: liste(s.retenus).join(', ') })}` : ''}`)];
  const q = cache.partition;
  const fractions = q && !q.erreur ? lignesDe(q.fractions, ['fraction', 'sens', 'r2_ajuste']) : [];
  const graphe = q && !q.erreur ? el('div', { class: 'graphe petit' }) : null;
  const ensembles = liste(m.blocs).length + (m.sans_bloc ? 1 : 0);  // (the blocks of the X, and the terms without one)
  const partition = liste(m.covariables).length ? el('p', { class: 'note' }, t('ord_partition_z'))
    : ensembles < 2 || ensembles > 3 ? el('p', { class: 'note' }, t('ord_partition_blocs'))
      : !q ? bouton('partition', 'rda_partition', t('ord_lancer_partition')) : q.erreur ? el('p', { class: 'note' }, q.erreur) : [
    el('p', { class: 'note' }, t('ord_partition_note')),
    tableCompacte([t('ord_ensemble'), t('ddl'), 'R2', t('r2_ajuste')], lignesDe(q.ensembles, ['ensemble', 'ddl', 'R2', 'r2_ajuste']).map((l) => [l[0], fmt(l[1], 0), fmt(l[2], 4), fmt(l[3], 4)])),
    tableCompacte([t('ord_fraction'), '', t('r2_ajuste')], fractions.map((l) => [l[0], l[1], fmt(l[2], 4)])), graphe];
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche,
    el('h4', {}, t('ord_tests_axes')), tableTests(r.axes, 'axe'),
    el('h4', {}, t('ord_tests_termes')), tableTests(r.termes, 'terme'),
    el('p', { class: 'note' }, t('ord_tests_note')),
    el('h4', {}, t('selection_rda')), selection,
    el('h4', {}, t('partition_rda')), partition));
  if (graphe) {
    tracer(ctx, graphe, [{ type: 'bar', x: fractions.map((l) => `${l[0]} ${l[1]}`), y: fractions.map((l) => l[2]),
      marker: { color: fractions.map((l, i) => (i === fractions.length - 1 ? GRIS : PALETTE[i % PALETTE.length])) }, hovertemplate: '<b>%{x}</b><br>%{y:.4f}<extra></extra>' }],
    { title: { text: `${m.nom} · ${t('partition_rda')}` }, dragmode: false, showlegend: false, yaxis: { title: { text: t('r2_ajuste') } }, margin: { b: 90 } });
  }
}

// ------------------------------------------------------------ contributions of a CA

function rendreContributionsCA(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.cote ??= 'colonnes';
  const noms = axes(m);
  loc.axe = Math.min(loc.axe ?? 1, noms.length);
  const d = m.contributions[loc.cote];
  const ax = noms[loc.axe - 1];
  const nom = liste(d.nom);
  const ctr = liste(d[`ctr_${ax}`]);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(outils(
    choix('', [['colonnes', t('ord_colonnes')], ['lignes', t('ord_lignes')]], loc.cote, (v) => { loc.cote = v; ctx.rafraichir(); }),
    choix(t('ord_axe'), noms.map((n, i) => [i + 1, libelleAxe(m, i)]), loc.axe, (v) => { loc.axe = +v; ctx.rafraichir(); })),
  el('p', { class: 'note' }, t('ord_contributions_note')), graphe, bas);
  const ordre = nom.map((_, i) => i).sort((i, j) => ctr[j] - ctr[i]).slice(0, 40);
  const moyenne = 100 / Math.max(1, nom.length);
  const c = couleursTheme();
  tracer(ctx, graphe, [{ type: 'bar', x: ordre.map((i) => nom[i]), y: ordre.map((i) => ctr[i]),
    marker: { color: ordre.map((i) => (ctr[i] > moyenne ? PALETTE[0] : GRIS)) }, hovertemplate: '<b>%{x}</b><br>%{y:.2f} %<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('ord_ctr')} · ${ax}` }, dragmode: false, showlegend: false, xaxis: { tickangle: -40 }, yaxis: { title: { text: t('ord_ctr') } },
    shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: moyenne, y1: moyenne, line: { color: c.doux, width: 1, dash: 'dot' } }] });
  loc.table ??= {};
  const uids = liste(m.observations.uid);
  const premiers = noms.slice(0, 2);
  tableau(bas, ctx, loc.table, {
    n: nom.length, nom: 'Contributions', idModele: loc.cote === 'lignes' ? m.id : undefined, uid: loc.cote === 'lignes' ? (i) => uids[i] : undefined,
    colonnes: [{ cle: 'nom', titre: '' }, { cle: 'masse', titre: t('ord_masse'), num: true, decimales: 4 }, { cle: 'inertie', titre: t('ord_inertie_pct'), num: true, decimales: 2 }]
      .concat(noms.flatMap((n) => [{ cle: `ctr_${n}`, titre: `${t('ord_ctr')} ${n}`, num: true, decimales: 2 }, { cle: `cos2_${n}`, titre: `cos² ${n}`, num: true, decimales: 3 }]))
      .concat(premiers.length === 2 ? [{ cle: 'plan', titre: `cos² ${premiers.join('+')}`, num: true, decimales: 3 }] : []),
    valeur: (i, k) => (k === 'plan' ? liste(d[`cos2_${premiers[0]}`])[i] + liste(d[`cos2_${premiers[1]}`])[i] : liste(d[k])[i]),
  });
}

// ------------------------------------------------------------ summary

function rendreResumeOrdination(conteneur, ctx, m) {
  const titre = el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, m.type), m.titre ? el('span', { class: 'doux' }, m.titre) : null);
  const i = m.inertie;
  const pct = (v) => `${fmt(100 * v / i.totale, 1)} %`;
  let fiche;
  let table;
  const textes = [];
  if (m.type === 'RDA') {
    const o = m.options || {};
    fiche = el('div', { class: 'fiche' }, titre,
      el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('ord_reponses'), liste(m.noms_y).length, 0), chiffre(t('nb_termes'), liste(m.termes).length, 0),
        liste(m.covariables).length ? chiffre(t('ord_covariables'), liste(m.covariables).join(', ')) : null,
        m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null),
      el('div', { class: 'fiche-chiffres' }, chiffre('R2', m.r2), chiffre(t('r2_ajuste'), m.r2_ajuste), chiffre(t('ord_pseudo_f'), m.test.F, 2),
        chiffre('p', m.test.permutations ? pValeur(m.test.p) : '—'), chiffre(t('nb_permutations'), m.test.permutations || 0, 0)));
    table = tableCompacte([t('ord_inertie'), '', '%'], [[t('ord_inertie_totale'), fmt(i.totale, 5), '100 %']]
      .concat(i.conditionnelle > 0 ? [[t('ord_inertie_conditionnelle'), fmt(i.conditionnelle, 5), pct(i.conditionnelle)]] : [],
        [[t('ord_inertie_contrainte'), fmt(i.contrainte, 5), pct(i.contrainte)], [t('ord_inertie_residuelle'), fmt(i.residuelle, 5), pct(i.residuelle)]]));
    textes.push(t('ord_reglages_rda', { tr: t('ord_tr_' + (o.transformation || 'aucune')), centrage: t(o.reduire ? 'ord_reduites' : 'ord_centrees') })
      + (m.strates ? ` ${t('ord_permutations_dans', { g: m.strates })}` : ''));
    if (liste(m.alias).length) textes.push(t('ord_alias', { noms: liste(m.alias).join(', ') }));
    textes.push(t('ord_resume_rda'));
  } else {
    const vp = liste(m.valeurs_propres.part);
    fiche = el('div', { class: 'fiche' }, titre,
      el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('variables'), m.K, 0), chiffre(t('ord_total_tableau'), m.total, 0),
        m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null),
      el('div', { class: 'fiche-chiffres' }, chiffre(t('ord_inertie_totale'), i.totale, 4), ...axes(m).slice(0, 3).map((n, k) => chiffre(n, `${fmt(100 * vp[k], 1)} %`))));
    if (liste(m.variables_vides).length) textes.push(t('ord_variables_vides', { noms: liste(m.variables_vides).join(', ') }));
    textes.push(t('ord_resume_ca'));
  }
  const notes = notesModele(conteneur, ctx, m);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ, table, textes.map((x) => el('p', { class: 'note' }, x))));
  notes.retrouver();
}

// ------------------------------------------------------------ the model dialog

const optionsDe = (st) => st.options;
function champsRDA(st, rendre, p) {
  const o = optionsDe(st);
  // (a model being edited: its covariates' transformations, kept in its options)
  if (o.z_transformations && !o.z_lues) {
    for (const [u, v] of Object.entries(o.z_transformations)) if (!st.transfos[u]) st.transfos[u] = v;
    o.z_lues = true;
  }
  const nombre = (cle, defaut, attrs) => el('input', { type: 'number', value: o[cle] ?? defaut, style: 'width:6em', ...attrs,
    oninput: (e) => { o[cle] = e.target.value === '' ? null : Number(e.target.value); } });
  const ids = [].concat(p.identifiants || []).map((i) => i.nom);
  const qualitatives = p.variables.filter((v) => v.qualitative);
  const strate = !o.strates ? '' : o.strates.by === 'id' ? `id:${o.strates.id}` : `var:${o.strates.variable}`;
  const option = (v, n) => el('option', { value: v, selected: v === strate }, n);
  return [
    el('label', {}, t('ord_transformation')), el('select', { onchange: (e) => { o.transformation = e.target.value; } },
      ['aucune', 'hellinger', 'corde', 'log1p'].map((k) => el('option', { value: k, selected: k === (o.transformation || 'aucune') }, t('ord_tr_' + k)))),
    el('label', {}, t('ord_reponses')), el('select', { onchange: (e) => { o.reduire = e.target.value === '1'; } },
      [['0', t('ord_centrees')], ['1', t('ord_reduites')]].map(([k, n]) => el('option', { value: k, selected: k === (o.reduire ? '1' : '0') }, n))),
    el('label', {}, t('nb_permutations')), el('div', { class: 'ligne' }, nombre('permutations', 999, { min: 0, max: 99999, step: 1 }),
      el('label', { class: 'champ' }, t('ord_graine'), nombre('graine', 1, { step: 1 }))),
    el('label', {}, t('ord_strates')), el('select', { onchange: (e) => {
      const v = e.target.value;
      o.strates = !v ? null : v.startsWith('id:') ? { by: 'id', id: v.slice(3) } : { by: 'variable', variable: Number(v.slice(4)) };
    } }, option('', t('ord_strates_aucune')),
    ids.length ? el('optgroup', { label: t('vc_par_identifiant') }, ids.map((n) => option(`id:${n}`, n))) : null,
    qualitatives.length ? el('optgroup', { label: t('vc_par_variable') }, qualitatives.map((v) => option(`var:${v.uid}`, v.nom))) : null),
    el('label', {}, ''), el('p', { class: 'note' }, t('ord_reglages_note')),
  ];
}
const nomVariable = (p, u) => (p.variables.find((v) => v.uid === u) || {}).nom;
const qualitativesDe = (p, us) => us.filter((u) => (p.variables.find((v) => v.uid === u) || {}).qualitative);
ANALYSES.RDA = {
  libelle: () => t('type_rda'),
  note: () => t('ord_rda_note'),
  roles: ['x', 'y', 'z'],
  echelle: 'none',
  vc: false,
  champs: champsRDA,
  valider: (st, cles, p) => {
    if (!cles('y').length) return t('ord_y_requis');
    const q = qualitativesDe(p, cles('y'));
    if (q.length) return t('ord_quantitatives', { noms: q.map((u) => nomVariable(p, u)).join(', ') });
    const B = optionsDe(st).permutations;
    if (B !== undefined && B !== null && !(Number.isInteger(B) && B >= 0 && B <= 99999)) return t('ord_permutations_invalides');
    return '';
  },
  options: (st, cles) => {
    const o = optionsDe(st);
    const z = cles('z');
    return { transformation: o.transformation || 'aucune', reduire: !!o.reduire, permutations: Number.isFinite(o.permutations) ? o.permutations : 999,
      graine: Number.isFinite(o.graine) ? Math.round(o.graine) : 1, strates: o.strates || null, z,
      z_transformations: Object.fromEntries(z.filter((u) => st.transfos[u] && st.transfos[u] !== 'autre').map((u) => [u, st.transfos[u]])) };
  },
};
ANALYSES.CA = {
  libelle: () => t('type_ca'),
  note: () => t('ord_ca_note'),
  roles: ['x'],
  echelle: 'none',
  vc: false,
  valider: (st, cles, p) => {
    if (cles('x').length < 2) return t('ord_ca_x_requis');
    const q = qualitativesDe(p, cles('x'));
    return q.length ? t('ord_quantitatives', { noms: q.map((u) => nomVariable(p, u)).join(', ') }) : '';
  },
  options: () => ({}),
};

// ------------------------------------------------------------ the report

const blocsResume = (m) => {
  const i = m.inertie;
  if (m.type === 'CA') {
    return [{ texte: `${t('type_ca')} · N = ${m.N} · ${t('variables')} ${m.K} · ${t('ord_inertie_totale')} ${fmt(i.totale, 4)}` },
      { entetes: [t('ord_axe'), t('valeur_propre'), t('ord_part_inertie')], lignes: lignesDe(m.valeurs_propres, ['axe', 'valeur_propre', 'part']).slice(0, 5)
        .map((l) => [l[0], fmt(l[1], 5), fmt(100 * l[2], 2)]) }];
  }
  const pct = (v) => fmt(100 * v / i.totale, 1);
  return [{ texte: `${t('type_rda')} · N = ${m.N} · ${t('ord_reponses')} ${liste(m.noms_y).length} · ${t('nb_termes')} ${liste(m.termes).length}`
    + (liste(m.covariables).length ? ` · ${t('ord_covariables')} : ${liste(m.covariables).join(', ')}` : '') },
  { entetes: ['R2', t('r2_ajuste'), t('ord_pseudo_f'), 'p', t('nb_permutations')],
    lignes: [[fmt(m.r2, 4), fmt(m.r2_ajuste, 4), fmt(m.test.F, 3), m.test.permutations ? pValeur(m.test.p) : '—', String(m.test.permutations || 0)]] },
  { entetes: [t('ord_inertie'), '', '%'], lignes: [[t('ord_inertie_totale'), fmt(i.totale, 5), '100']]
    .concat(i.conditionnelle > 0 ? [[t('ord_inertie_conditionnelle'), fmt(i.conditionnelle, 5), pct(i.conditionnelle)]] : [],
      [[t('ord_inertie_contrainte'), fmt(i.contrainte, 5), pct(i.contrainte)], [t('ord_inertie_residuelle'), fmt(i.residuelle, 5), pct(i.residuelle)]]) },
  { texte: t('ord_reglages_rda', { tr: t('ord_tr_' + ((m.options || {}).transformation || 'aucune')), centrage: t((m.options || {}).reduire ? 'ord_reduites' : 'ord_centrees') })
    + (m.strates ? ` ${t('ord_permutations_dans', { g: m.strates })}` : '') }];
};
const blocsValeursPropres = (m) => [{ entetes: [t('ord_axe'), t('valeur_propre'), t('ord_part_inertie'), `${t('ord_cumul')} (%)`],
  lignes: lignesDe(m.valeurs_propres, ['axe', 'valeur_propre', 'part', 'cumul']).slice(0, 10).map((l) => [l[0], fmt(l[1], 5), fmt(100 * l[2], 2), fmt(100 * l[3], 2)]) }];
// a part asked for from the engine (computed then if it wasn't yet): its error as text
const demandeRapport = async (nom, m, f) => { try { return f(await demander(nom, { modele: m.id })); } catch (e) { return [{ texte: e.message }]; } };
const lignesTests = (df, premiere) => lignesDe(df, [premiere, 'ddl', 'inertie', 'F', 'p']).map((l) => [l[0], fmt(l[1], 0), fmt(l[2], 5), l[3] === null ? '' : fmt(l[3], 3), l[4] === null ? '' : pValeur(l[4])]);
const blocsTests = (m) => (!m.test.permutations ? [{ texte: t('ord_sans_permutations') }] : demandeRapport('rda_tests', m, (r) => [
  { texte: `${t('ord_test_global')} : ${t('ord_pseudo_f')} = ${fmt(m.test.F, 3)}, p = ${pValeur(m.test.p)} (${m.test.permutations} ${t('ord_permutations_mot')}${m.strates ? `, ${t('ord_permutations_dans', { g: m.strates })}` : ''})` },
  { texte: t('ord_tests_axes') }, { entetes: [t('ord_axe'), t('ddl'), t('ord_inertie'), t('ord_pseudo_f'), 'p'], lignes: lignesTests(r.axes, 'axe') },
  { texte: t('ord_tests_termes') }, { entetes: [t('terme'), t('ddl'), t('ord_inertie'), t('ord_pseudo_f'), 'p'], lignes: lignesTests(r.termes, 'terme') }]));
const blocsSelection = (m) => (!m.test.permutations ? [{ texte: t('ord_sans_permutations') }] : demandeRapport('rda_selection', m, (s) => [
  { texte: t('ord_selection_note', { r2: fmt(s.r2_ajuste_tous, 3), seuil: fmt(s.seuil, 2) }) },
  s.etapes ? { entetes: [t('terme'), t('r2_ajuste'), t('ord_pseudo_f'), 'p', t('ord_retenu')],
    lignes: lignesDe(s.etapes, ['terme', 'r2_ajuste', 'F', 'p', 'retenu']).map((l) => [l[0], fmt(l[1], 4), l[2] === null ? '' : fmt(l[2], 3), pValeur(l[3]), l[4] ? t('ord_oui') : t('ord_non')]) } : null,
  { texte: `${t('ord_arret_' + s.arret)}${liste(s.retenus).length ? ` ${t('ord_retenus', { termes: liste(s.retenus).join(', ') })}` : ''}` }].filter(Boolean)));
const blocsPartition = (m) => demandeRapport('rda_partition', m, (q) => [{ texte: t('ord_partition_note') },
  { entetes: [t('ord_ensemble'), t('ddl'), 'R2', t('r2_ajuste')], lignes: lignesDe(q.ensembles, ['ensemble', 'ddl', 'R2', 'r2_ajuste']).map((l) => [l[0], fmt(l[1], 0), fmt(l[2], 4), fmt(l[3], 4)]) },
  { entetes: [t('ord_fraction'), '', t('r2_ajuste')], lignes: lignesDe(q.fractions, ['fraction', 'sens', 'r2_ajuste']).map((l) => [l[0], l[1], fmt(l[2], 4)]) }]);
const blocsVariables = (m) => {
  const n = axes(m).slice(0, 3);
  const bloc = (df, titre) => (df ? [{ texte: titre }, { entetes: [t('variables'), ...n], lignes: lignesDe(df, ['terme', ...n]).map((l) => [l[0], ...l.slice(1).map((v) => fmt(v, 4))]) }] : []);
  return m.type === 'RDA' ? [...bloc(m.variables_y, `${t('ord_reponses')} (${t('ord_echelle')} 2)`), ...bloc(m.variables, `${t('ord_x')} (${t('ord_echelle')} 2)`)]
    : bloc(m.variables, `${t('variables')} (${t('ord_echelle')} 2)`);
};
const blocsContributions = (m) => {
  const n = axes(m).slice(0, 2);
  return [{ texte: t('ord_contributions_note') },
    { entetes: [t('variables'), t('ord_masse'), ...n.flatMap((a) => [`${t('ord_ctr')} ${a}`, `cos² ${a}`])],
      lignes: lignesDe(m.contributions.colonnes, ['nom', 'masse', ...n.flatMap((a) => [`ctr_${a}`, `cos2_${a}`])]).map((l) => [l[0], fmt(l[1], 4), ...l.slice(2).map((v, k) => fmt(v, k % 2 ? 3 : 2))]) }];
};
PARTIES_TYPES.RDA = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'ordination', vue: 'ordination', defaut: true },
  { cle: 'valeurs_propres', vue: 'valeurs_propres', defaut: true, tableau: blocsValeursPropres },
  { cle: 'tests_rda', defaut: true, tableau: blocsTests },
  { cle: 'selection_rda', tableau: blocsSelection },
  { cle: 'partition_rda', tableau: blocsPartition },
  { cle: 'liste_vars', tableau: blocsVariables },
];
PARTIES_TYPES.CA = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'ordination', vue: 'ordination', defaut: true },
  { cle: 'valeurs_propres', vue: 'valeurs_propres', defaut: true, tableau: blocsValeursPropres },
  { cle: 'contributions_ca', defaut: true, tableau: blocsContributions },
  { cle: 'liste_vars', tableau: blocsVariables },
];

// ------------------------------------------------------------ registration

const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estOrdination(m) ? rendreResumeOrdination(c, ctx, m) : resume(c, ctx, p, loc);
} };
VUES.ordination = { rendre: rendreOrdination, titre: (m) => `${m.nom} · ${t('ordination')}` };
VUES.valeurs_propres = { rendre: rendreValeursPropres, titre: (m) => `${m.nom} · ${t('valeurs_propres')}` };
VUES.tests_rda = { rendre: rendreTestsRDA, titre: (m) => `${m.nom} · ${t('tests_rda')}` };
VUES.contributions_ca = { rendre: rendreContributionsCA, titre: (m) => `${m.nom} · ${t('contributions_ca')}` };
Object.assign(ICONES_VUES, { ordination: 'chart-scatter', valeurs_propres: 'chart-column', tests_rda: 'shuffle', contributions_ca: 'sliders-horizontal' });
VUES_TYPES.RDA = ['resume', 'ordination', 'valeurs_propres', 'tests_rda', 'liste_obs', 'liste_vars'];
VUES_TYPES.CA = ['resume', 'ordination', 'valeurs_propres', 'contributions_ca', 'liste_obs', 'liste_vars'];

ajouterTextes({
  type_rda: 'RDA (analyse de redondance)', type_ca: 'Analyse des correspondances (CA)',
  ordination: 'Ordination', valeurs_propres: 'Valeurs propres', tests_rda: 'Tests par permutation', contributions_ca: 'Contributions et cos²',
  selection_rda: 'Sélection pas à pas', partition_rda: 'Partition de la variance',
  ord_rda_note: 'Y : les réponses (des abondances d’espèces, des mesures) ; X : les variables explicatives ; Z : des covariables dont l’effet est retiré d’abord (RDA partielle). Une variable qualitative X ou Z entre par ses catégories ; la colonne Bloc fait les ensembles de la partition de la variance. La mise à l’échelle ne s’applique pas : les réponses sont centrées, ou centrées-réduites, ci-dessous.',
  ord_ca_note: 'X : les colonnes du tableau, des effectifs ou des abondances (positifs ou nuls), une observation par ligne. Les observations et les variables sans aucune valeur sont laissées de côté.',
  ord_transformation: 'Transformation des réponses', ord_tr_aucune: 'aucune', ord_tr_hellinger: 'Hellinger (abondances)', ord_tr_corde: 'corde (abondances)', ord_tr_log1p: 'log(x + 1)',
  ord_reponses: 'Réponses Y', ord_centrees: 'centrées (RDA sur les covariances)', ord_reduites: 'centrées-réduites (RDA sur les corrélations)',
  ord_graine: 'Graine', ord_strates: 'Permuter à l’intérieur de', ord_strates_aucune: '— (librement)',
  ord_reglages_note: 'Le test permute les observations (999 fois par défaut ; 0 : aucun test), à l’intérieur des groupes choisis s’il y en a (un site suivi plusieurs années...). La graine rend les permutations reproductibles : les mêmes donnent les mêmes valeurs p.',
  ord_y_requis: 'Une RDA demande des réponses Y.', ord_ca_x_requis: 'Une analyse des correspondances demande au moins deux variables X (les colonnes du tableau).',
  ord_quantitatives: 'Des variables quantitatives seulement : {noms}.', ord_permutations_invalides: 'Le nombre de permutations est un entier de 0 à 99 999.',
  ord_echelle: 'Échelle', ord_echelle_1: '1 · distances entre observations', ord_echelle_2: '2 · corrélations entre réponses',
  ord_ca_echelle_1: '1 · lignes (distances du khi² entre observations)', ord_ca_echelle_2: '2 · colonnes (distances du khi² entre variables)', ord_ca_echelle_3: '3 · symétrique',
  ord_wa: 'scores wa (sommes pondérées des réponses)', ord_lc: 'scores lc (combinaisons linéaires des X)', ord_x: 'Variables X',
  ord_fleches: 'X (flèches × {k})', ord_centroides: 'Catégories',
  ord_axe: 'Axe', ord_part_inertie: '% de l’inertie', ord_cumul: 'cumul', ord_contraint: 'Contraint', ord_oui: 'oui', ord_non: 'non',
  ord_valeurs_propres_rda: 'Les {r} axes contraints (en bleu) décrivent ce que les X expliquent des réponses ; les axes non contraints (en gris), ce qui reste.',
  ord_valeurs_propres_ca: 'La part de l’inertie totale (le khi² du tableau divisé par son effectif) portée par chaque axe.',
  ord_inertie: 'Inertie', ord_inertie_totale: 'totale', ord_inertie_conditionnelle: 'conditionnelle (covariables Z)', ord_inertie_contrainte: 'contrainte (X)',
  ord_inertie_residuelle: 'non contrainte (résidus)', ord_inertie_pct: 'Inertie (%)', ord_covariables: 'Covariables Z',
  ord_pseudo_f: 'pseudo-F', ord_test_global: 'Test du modèle', ord_dans: 'Permutations dans', ord_permutations_mot: 'permutations',
  ord_permutations_dans: 'Permutations à l’intérieur de chaque « {g} ».',
  ord_tests_axes: 'Par axe contraint (chacun après les précédents)', ord_tests_termes: 'Par terme (chacun après tous les autres)',
  ord_tests_note: 'p ≤ 0,05 en rouge. Le pseudo-F rapporte l’inertie expliquée par degré de liberté à l’inertie résiduelle par degré de liberté ; p compte les permutations qui font au moins aussi bien. Un terme contenu dans un terme croisé n’est pas testé seul.',
  ord_calcul_tests: 'Tests par axe et par terme : {n} permutations en cours…', ord_sans_permutations: 'Aucune permutation n’a été demandée : choisissez-en un nombre (Modifier le modèle).',
  ord_lancer_selection: 'Lancer la sélection pas à pas', ord_lancer_partition: 'Partager la variance entre les blocs',
  ord_selection_note: 'À chaque pas entre le terme au plus grand R² ajusté, s’il l’augmente sans dépasser celui de tous les termes ({r2}) et si son test (le modèle avec lui contre le modèle sans lui) a p ≤ {seuil} (Blanchet et al. 2008).',
  ord_arret_tous: 'Tous les termes sont entrés.', ord_arret_r2: 'Arrêt : plus aucun terme n’augmente le R² ajusté.',
  ord_arret_r2_tous: 'Arrêt : le terme suivant dépasserait le R² ajusté de tous les termes.', ord_arret_p: 'Arrêt : le terme suivant n’est pas significatif.',
  ord_retenus: 'Retenus : {termes}.', ord_retenu: 'Retenu',
  ord_partition_note: 'Les ensembles sont les blocs de la colonne Bloc (les termes sans bloc ensemble). Les fractions propres à chaque ensemble ([a], [b]…), puis communes, et les résidus, en R² ajustés (Peres-Neto et al. 2006) ; une fraction commune peut être négative.',
  ord_ensemble: 'Ensemble', ord_fraction: 'Fraction',
  ord_partition_z: 'La partition de la variance se fait sans covariables Z : refaites le modèle sans elles, ou mettez-les en X dans un bloc.',
  ord_partition_blocs: 'Pour partager la variance entre deux ou trois ensembles de variables X, donnez-leur un bloc (colonne Bloc de l’onglet Variables, Modifier le modèle) ; les termes sans bloc font un ensemble.',
  ord_lignes: 'Observations (lignes)', ord_colonnes: 'Variables (colonnes)', ord_masse: 'Masse', ord_ctr: 'Contribution (%)',
  ord_contributions_note: 'La contribution de chaque ligne ou colonne à l’inertie de l’axe (au-dessus de la moyenne, en pointillés : en bleu) et la qualité de sa représentation (cos², 1 : entièrement sur l’axe).',
  ord_total_tableau: 'Effectif total', ord_variables_vides: 'Sans aucune valeur, laissées de côté : {noms}.',
  ord_reglages_rda: 'Réponses : transformation {tr}, {centrage}.', ord_alias: 'Termes redondants avec d’autres, laissés de côté : {noms}.',
  ord_resume_rda: 'L’inertie est la variance totale des réponses (transformées) ; le R² ajusté corrige le R² du nombre de termes. Le test compare le pseudo-F du modèle à celui des permutations.',
  ord_resume_ca: 'L’inertie totale est le khi² du tableau divisé par son effectif total. Des observations proches ont des profils semblables, de même pour les variables.',
}, {
  type_rda: 'RDA (redundancy analysis)', type_ca: 'Correspondence analysis (CA)',
  ordination: 'Ordination', valeurs_propres: 'Eigenvalues', tests_rda: 'Permutation tests', contributions_ca: 'Contributions and cos²',
  selection_rda: 'Forward selection', partition_rda: 'Variation partitioning',
  ord_rda_note: 'Y: the responses (species abundances, measurements); X: the explanatory variables; Z: covariates whose effect is removed first (partial RDA). A qualitative X or Z enters through its categories; the Block column makes the sets of the variation partitioning. Scaling doesn’t apply: the responses are centred, or standardized, below.',
  ord_ca_note: 'X: the columns of the table, counts or abundances (positive or zero), one observation per row. Observations and variables without any value are left out.',
  ord_transformation: 'Transformation of the responses', ord_tr_aucune: 'none', ord_tr_hellinger: 'Hellinger (abundances)', ord_tr_corde: 'chord (abundances)', ord_tr_log1p: 'log(x + 1)',
  ord_reponses: 'Responses Y', ord_centrees: 'centred (covariance RDA)', ord_reduites: 'standardized (correlation RDA)',
  ord_graine: 'Seed', ord_strates: 'Permute within', ord_strates_aucune: '— (freely)',
  ord_reglages_note: 'The test permutes the observations (999 times by default; 0: no test), within the groups chosen if any (a site followed over several years...). The seed makes the permutations reproducible: the same ones give the same p-values.',
  ord_y_requis: 'An RDA needs responses Y.', ord_ca_x_requis: 'A correspondence analysis needs at least two X variables (the columns of the table).',
  ord_quantitatives: 'Quantitative variables only: {noms}.', ord_permutations_invalides: 'The number of permutations is a whole number from 0 to 99,999.',
  ord_echelle: 'Scaling', ord_echelle_1: '1 · distances between observations', ord_echelle_2: '2 · correlations between responses',
  ord_ca_echelle_1: '1 · rows (chi-square distances between observations)', ord_ca_echelle_2: '2 · columns (chi-square distances between variables)', ord_ca_echelle_3: '3 · symmetric',
  ord_wa: 'wa scores (weighted sums of the responses)', ord_lc: 'lc scores (linear combinations of the X)', ord_x: 'X variables',
  ord_fleches: 'X (arrows × {k})', ord_centroides: 'Categories',
  ord_axe: 'Axis', ord_part_inertie: '% of the inertia', ord_cumul: 'cumulative', ord_contraint: 'Constrained', ord_oui: 'yes', ord_non: 'no',
  ord_valeurs_propres_rda: 'The {r} constrained axes (in blue) describe what the X explain of the responses; the unconstrained axes (in grey), what is left.',
  ord_valeurs_propres_ca: 'The share of the total inertia (the chi-square of the table divided by its grand total) carried by each axis.',
  ord_inertie: 'Inertia', ord_inertie_totale: 'total', ord_inertie_conditionnelle: 'conditional (covariates Z)', ord_inertie_contrainte: 'constrained (X)',
  ord_inertie_residuelle: 'unconstrained (residuals)', ord_inertie_pct: 'Inertia (%)', ord_covariables: 'Covariates Z',
  ord_pseudo_f: 'pseudo-F', ord_test_global: 'Test of the model', ord_dans: 'Permutations within', ord_permutations_mot: 'permutations',
  ord_permutations_dans: 'Permutations within each “{g}”.',
  ord_tests_axes: 'By constrained axis (each after the previous ones)', ord_tests_termes: 'By term (each after all the others)',
  ord_tests_note: 'p ≤ 0.05 in red. The pseudo-F is the inertia explained per degree of freedom over the residual inertia per degree of freedom; p counts the permutations that do at least as well. A term contained in a cross term isn’t tested alone.',
  ord_calcul_tests: 'Tests by axis and by term: {n} permutations running…', ord_sans_permutations: 'No permutations were asked for: choose a number of them (Edit model).',
  ord_lancer_selection: 'Run the forward selection', ord_lancer_partition: 'Partition the variance between the blocks',
  ord_selection_note: 'At each step, the term with the largest adjusted R² enters if it increases it without going beyond that of all the terms ({r2}) and if its test (the model with it against the model without it) has p ≤ {seuil} (Blanchet et al. 2008).',
  ord_arret_tous: 'All the terms entered.', ord_arret_r2: 'Stop: no term increases the adjusted R² any more.',
  ord_arret_r2_tous: 'Stop: the next term would go beyond the adjusted R² of all the terms.', ord_arret_p: 'Stop: the next term isn’t significant.',
  ord_retenus: 'Kept: {termes}.', ord_retenu: 'Kept',
  ord_partition_note: 'The sets are the blocks of the Block column (the terms without a block together). The fractions of each set alone ([a], [b]…), then shared, and the residuals, as adjusted R² (Peres-Neto et al. 2006); a shared fraction can be negative.',
  ord_ensemble: 'Set', ord_fraction: 'Fraction',
  ord_partition_z: 'Variation partitioning is done without covariates Z: fit the model again without them, or put them in X in a block.',
  ord_partition_blocs: 'To partition the variance between two or three sets of X variables, give them a block (Block column of the Variables tab, Edit model); the terms without a block make one set.',
  ord_lignes: 'Observations (rows)', ord_colonnes: 'Variables (columns)', ord_masse: 'Mass', ord_ctr: 'Contribution (%)',
  ord_contributions_note: 'The contribution of each row or column to the inertia of the axis (above the mean, dotted: in blue) and the quality of its representation (cos², 1: entirely on the axis).',
  ord_total_tableau: 'Grand total', ord_variables_vides: 'Without any value, left out: {noms}.',
  ord_reglages_rda: 'Responses: transformation {tr}, {centrage}.', ord_alias: 'Terms redundant with others, left out: {noms}.',
  ord_resume_rda: 'The inertia is the total variance of the (transformed) responses; the adjusted R² corrects the R² for the number of terms. The test compares the model’s pseudo-F with that of the permutations.',
  ord_resume_ca: 'The total inertia is the chi-square of the table divided by its grand total. Close observations have similar profiles, and likewise for variables.',
});
