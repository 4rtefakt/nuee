// Community analyses on a distance between observations (moteur/R/communautes.R): principal
// coordinates analysis (PCOA), non-metric multidimensional scaling (NMDS), PERMANOVA with the
// multivariate dispersion of its groups, and Mantel tests. Their settings in the model dialog,
// their views (the ordination with the species and the fitted X, the eigenvalues, the Shepard
// diagram, the fitted variables, the PERMANOVA table, the dispersion, the Mantel tests and the
// correlogram), their summaries, report parts and strings. Imported by app.js.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, fmt, entreNombres } from './ui.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, controleCouleur, controleEtiquettes, pointsObservations, tableau,
  notesModele, valeurP, ficheCondition } from './panneaux.js';
import { ANALYSES } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';

// a label that also goes inside a sentence, with a capital where it starts a line (a heading, an option)
const initiale = (s) => s.charAt(0).toUpperCase() + s.slice(1);

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const TYPES = ['PCOA', 'NMDS', 'PERMANOVA', 'MANTEL'];
const estCommunaute = (m) => !!m && !!m.analyse && TYPES.includes(m.type);
const ETIQUETTES_MAX = 100;  // labels written by each point up to this many
const GRIS = '#9aa7b8';
const chiffre = (k, v, d = 3) => el('span', {}, el('small', {}, k), ' ', el('b', {}, typeof v === 'number' ? fmt(v, d) : v ?? '—'));
const pValeur = (v) => (v === null || v === undefined ? '—' : valeurP(v));
const significatif = (v) => v !== null && v !== undefined && Number(v) <= 0.05;
const deuxPoints = () => (langueActuelle() === 'fr' ? ' : ' : ': ');  // (a space before the colon in French only)
const caseACocher = (libelle, valeur, changer) => el('label', { class: 'champ' },
  el('input', { type: 'checkbox', checked: valeur, onchange: (e) => changer(e.target.checked) }), libelle);
// the rows of a data.frame sent by R (columns), as arrays of values
const lignesDe = (df, cles) => { const n = liste((df || {})[cles[0]]).length; return Array.from({ length: n }, (_, i) => cles.map((k) => liste(df[k])[i])); };
// a small fixed table: headers, rows of cells, `alerte(i, j)` for a cell in red
function tableCompacte(entetes, lignes, alerte) {
  return el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, entetes.map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
    el('tbody', {}, lignes.map((l, i) => el('tr', {}, l.map((v, j) => el('td', { class: (j ? 'num' : '') + (alerte && alerte(i, j) ? ' alerte' : '') }, v))))));
}
const titreFiche = (m) => el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, m.type), m.titre ? el('span', { class: 'doux' }, m.titre) : null);
// the distance's name (Bray–Curtis on presence–absence is the Sørensen index), and its settings, in words
const nomDistance = (d) => (d.binaire && d.distance === 'bray' ? 'Sørensen'
  : `${t('com_d_' + d.distance)}${d.binaire ? ` (${t('com_binaire')})` : ''}`);
const texteDistance = (m) => {
  const d = m.distance || {};
  return [t('com_distance_mot', { d: nomDistance(d) }), d.transformation && d.transformation !== 'aucune' ? t('com_tr_' + d.transformation) : null,
    d.correction && d.correction !== 'aucune' && ['PCOA', 'PERMANOVA'].includes(m.type) ? t('com_correction_appliquee', { c: t('com_corr_' + d.correction), k: fmt(d.constante, 4) }) : null]
    .filter(Boolean).join(' · ');
};
const textePermutations = (m) => `${(m.distance || {}).permutations ?? 0} ${t('ord_permutations_mot')} (${t('ord_graine').toLowerCase()} ${(m.distance || {}).graine ?? 1})`
  + (m.strates ? `, ${t('ord_permutations_dans', { g: m.strates })}` : '');
const axes = (m) => liste(m.axes);
const partAxe = (m, k) => (m.type === 'PCOA' ? liste(m.valeurs_propres.part)[k] : null);
const libelleAxe = (m, k) => (m.type === 'PCOA' ? `${axes(m)[k]} (${fmt(100 * partAxe(m, k), 1)} %)` : axes(m)[k]);

// ------------------------------------------------------------ the ordination plot (PCOA, NMDS)

// The observations, the species' weighted-average scores, the centroids of the categories of the
// qualitative X and an arrow per quantitative X (its direction × √R², lengthened to fill the plot
// by the factor shown, as vegan draws envfit), the same scale on both axes. The arrows exist on the
// axes the X were fitted on (PCoA: the first two; NMDS: all its dimensions); those with p > 0.05
// are grey.
function rendreOrdinationDist(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const noms = axes(m);
  loc.a ??= 1;
  loc.b ??= Math.min(2, noms.length);
  loc.sites ??= true;
  loc.especes ??= true;
  loc.x ??= true;
  const a = Math.min(loc.a, noms.length) - 1;
  const b = Math.min(loc.b, noms.length) - 1;
  const col = (df, k) => liste((df || {})[noms[k]]);
  const options = noms.map((n, i) => [i + 1, libelleAxe(m, i)]);
  const aj = m.ajustement;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix(t('axe_x'), options, loc.a, (v) => { loc.a = +v; ctx.rafraichir(); }),
    noms.length > 1 ? choix(t('axe_y'), options, loc.b, (v) => { loc.b = +v; ctx.rafraichir(); }) : null,
    caseACocher(t('observations'), loc.sites, (v) => { loc.sites = v; ctx.rafraichir(); }),
    m.especes ? caseACocher(t('com_especes'), loc.especes, (v) => { loc.especes = v; ctx.rafraichir(); }) : null,
    aj ? caseACocher(t('ord_x'), loc.x, (v) => { loc.x = v; ctx.rafraichir(); }) : null,
    loc.sites ? controleCouleur(ctx, m, loc) : null,
    loc.sites ? controleEtiquettes(ctx, m, loc) : null), graphe);
  const un = noms.length < 2;  // (a single axis: the points along it)
  const y = (v) => (un ? v.map(() => 0) : v);
  const traces = [];
  const etendue = [[], []];  // the coordinates shown, for the length of the arrows
  if (loc.sites) {
    const xs = col(m.observations, a);
    const ys = y(col(m.observations, b));
    traces.push(...pointsObservations(ctx, { ...m, nom: t('observations') }, loc, xs, ys));  // (named for the legend)
    etendue[0].push(...xs); etendue[1].push(...ys);
  }
  const nomsE = liste((m.especes || {}).terme);
  if (loc.especes && nomsE.length) {
    const xe = col(m.especes, a);
    const ye = y(col(m.especes, b));
    traces.push({ type: 'scatter', mode: nomsE.length <= ETIQUETTES_MAX ? 'markers+text' : 'markers', name: t('com_especes'), x: xe, y: ye, text: nomsE,
      meta: 'variables', customdata: nomsE, textposition: 'top center', textfont: { size: 10, color: PALETTE[1] },
      marker: { color: PALETTE[1], symbol: 'diamond', size: 8 }, hovertemplate: `<b>%{text}</b><br>%{x:.3f}${entreNombres()}%{y:.3f}<extra></extra>` });
    etendue[0].push(...xe.filter(Number.isFinite)); etendue[1].push(...ye.filter(Number.isFinite));
  }
  const nomsC = liste(aj && aj.centroides && aj.centroides.terme);
  if (loc.x && nomsC.length) {
    const xc = col(aj.centroides, a);
    const yc = y(col(aj.centroides, b));
    traces.push({ type: 'scatter', mode: 'markers+text', name: t('ord_centroides'), x: xc, y: yc, text: nomsC, meta: 'variables', customdata: nomsC,
      textposition: 'bottom center', textfont: { size: 10, color: PALETTE[0] }, marker: { symbol: 'triangle-up', size: 10, color: PALETTE[0] },
      hovertemplate: `<b>%{text}</b><br>%{x:.3f}${entreNombres()}%{y:.3f}<extra></extra>` });
    etendue[0].push(...xc); etendue[1].push(...yc);
  }
  const annotations = [];
  const nomsV = liste(aj && aj.vecteurs && aj.vecteurs.terme);
  const ajustes = liste(aj && aj.axes);
  const surAxes = ajustes.includes(noms[a]) && (un || ajustes.includes(noms[b]));
  if (loc.x && nomsV.length && surAxes) {
    const bx = liste(aj.vecteurs[noms[a]]);
    const by = un ? bx.map(() => 0) : liste(aj.vecteurs[noms[b]]);
    const ps = liste(aj.vecteurs.p);
    const r2 = liste(aj.vecteurs.r2);
    const max = (v) => Math.max(0, ...v.filter(Number.isFinite).map(Math.abs));
    // the arrows fill three quarters of the plot (vegan: ordiArrowMul)
    let mul = 0.75 * Math.min(max(bx) > 0 && max(etendue[0]) > 0 ? max(etendue[0]) / max(bx) : Infinity,
      max(by) > 0 && max(etendue[1]) > 0 ? max(etendue[1]) / max(by) : Infinity);
    if (!Number.isFinite(mul) || mul <= 0) mul = 1;
    const couleur = (i) => (significatif(ps[i]) ? PALETTE[0] : GRIS);
    nomsV.forEach((_, i) => annotations.push({ x: mul * bx[i], y: mul * by[i], ax: 0, ay: 0, xref: 'x', yref: 'y', axref: 'x', ayref: 'y', text: '',
      showarrow: true, arrowhead: 2, arrowsize: 1.2, arrowwidth: 1.6, arrowcolor: couleur(i) }));
    traces.push({ type: 'scatter', mode: 'text', name: t('ord_fleches', { k: fmt(mul, 2) }), x: bx.map((v) => 1.1 * mul * v), y: by.map((v) => 1.1 * mul * v),
      text: nomsV, meta: 'variables', customdata: nomsV.map((_, i) => [fmt(r2[i], 3), pValeur(ps[i])]), textfont: { size: 11, color: nomsV.map((_, i) => couleur(i)) },
      hovertemplate: '<b>%{text}</b><br>R² %{customdata[0]} · p %{customdata[1]}<extra></extra>' });
  }
  const titre = m.type === 'NMDS' ? `${m.nom} · NMDS · stress ${fmt(m.stress, 3)}` : `${m.nom} · PCoA`;
  // (the same scale on both axes, as an ordination is read: the plot area narrows rather than the
  // axes stretching)
  tracer(ctx, graphe, traces, { title: { text: titre }, annotations,
    xaxis: { title: { text: libelleAxe(m, a) }, zeroline: true, constrain: 'domain' },
    yaxis: { title: { text: un ? '' : libelleAxe(m, b) }, zeroline: true, scaleanchor: 'x', scaleratio: 1, constrain: 'domain' } });
  if (loc.sites) brancherSelection(graphe, ctx, m.id);
  if (loc.x && nomsV.length && !surAxes) graphe.after(el('p', { class: 'note' }, t('com_fleches_axes', { axes: ajustes.join(', ') })));
}

// ------------------------------------------------------------ eigenvalues of a PCoA

function rendreValeursPropresPCoA(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const vp = m.valeurs_propres;
  const ax = liste(vp.axe);
  const part = liste(vp.part);
  const cumul = liste(vp.cumul);
  const neg = liste(vp.negative);
  const n = Math.min(ax.length, 30);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  const ng = m.negatives || {};
  conteneur.replaceChildren(el('p', { class: 'note' }, t('com_vp_note'), ng.n ? ` ${t('com_vp_negatives', { n: ng.n, part: fmt(100 * ng.part, 1) })}` : ` ${t('com_vp_euclidienne')}`), graphe, bas);
  const c = couleursTheme(ctx);
  tracer(ctx, graphe, [
    { type: 'bar', name: t('ord_part_inertie'), x: ax.slice(0, n), y: part.slice(0, n).map((v) => 100 * v),
      marker: { color: neg.slice(0, n).map((k) => (k ? PALETTE[1] : PALETTE[0])) }, hovertemplate: '<b>%{x}</b><br>%{y:.2f} %<extra></extra>' },
    { type: 'scatter', mode: 'lines+markers', name: t('ord_cumul'), x: ax.slice(0, n), y: cumul.slice(0, n).map((v) => 100 * v), yaxis: 'y2',
      line: { color: c.doux, width: 1.5 }, marker: { size: 5, color: c.doux }, hovertemplate: '<b>%{x}</b><br>%{y:.1f} %<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('valeurs_propres')}` }, dragmode: false, yaxis: { title: { text: t('ord_part_inertie') } }, margin: { r: 54 },
    yaxis2: { overlaying: 'y', side: 'right', showgrid: false, zeroline: false, title: { text: `${t('ord_cumul')} (%)` } } });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: ax.length, nom: 'Eigenvalues',
    colonnes: [{ cle: 'axe', titre: t('ord_axe') }, { cle: 'valeur_propre', titre: t('valeur_propre'), num: true, decimales: 5 },
      { cle: 'part', titre: t('ord_part_inertie'), num: true, decimales: 2 }, { cle: 'cumul', titre: `${t('ord_cumul')} (%)`, num: true, decimales: 2 },
      { cle: 'negative', titre: t('com_negative') }],
    valeur: (i, k) => (k === 'part' || k === 'cumul' ? 100 * liste(vp[k])[i] : k === 'negative' ? (neg[i] ? t('ord_oui') : '') : liste(vp[k])[i]),
  });
}

// ------------------------------------------------------------ the Shepard diagram of an NMDS

// The ordination distances against the dissimilarities, and the monotone regression whose
// residuals make the stress (MASS::Shepard); then the stress of each start.
function rendreShepard(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const s = m.shepard;
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  const notes = [t('com_shepard_note', { stress: fmt(m.stress, 3), r2: fmt(1 - m.stress * m.stress, 3) })];
  if (s.total > liste(s.dissimilarite).length) notes.push(t('com_shepard_extrait', { n: liste(s.dissimilarite).length, total: s.total }));
  conteneur.replaceChildren(el('p', { class: 'note' }, notes.join(' ')), graphe, bas);
  tracer(ctx, graphe, [
    { type: 'scattergl', mode: 'markers', name: t('com_paires'), x: liste(s.dissimilarite), y: liste(s.distance),
      marker: { size: 4, color: PALETTE[0], opacity: 0.45 }, hovertemplate: '%{x:.3f} → %{y:.3f}<extra></extra>' },
    { type: 'scatter', mode: 'lines', name: t('com_monotone'), x: liste(s.dissimilarite), y: liste(s.ajustee), line: { color: PALETTE[1], width: 2, shape: 'hv' },
      hoverinfo: 'skip' }],
  { title: { text: `${m.nom} · ${t('shepard')} · stress ${fmt(m.stress, 3)}` }, dragmode: 'zoom', showlegend: true,
    xaxis: { title: { text: t('com_dissimilarite') } }, yaxis: { title: { text: t('com_distance_ordination') } } });
  const d = m.stress_departs;
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: liste(d.depart).length, nom: 'Starts', choisie: (i) => liste(d.depart)[i] === m.meilleur,
    avant: [el('span', { class: 'infos' }, texteRetrouvee(m))],
    colonnes: [{ cle: 'depart', titre: t('com_depart') }, { cle: 'stress', titre: 'stress', num: true, decimales: 4 }],
    valeur: (i, k) => (k === 'depart' ? departTexte(liste(d.depart)[i]) : liste(d[k])[i]),
  });
}
const departTexte = (k) => (k === 0 ? t('com_depart_pcoa') : `${t('com_depart_hasard')} ${k}`);
// Whether the NMDS's best solution was found again; not found again by Procrustes but reached in
// stress by other starts (meme_stress: some observations placed slightly differently), it isn't
// called a local minimum.
const texteRetrouvee = (m) => (m.retrouvee > 0 ? t('com_retrouvee', { n: m.retrouvee, meilleur: departTexte(m.meilleur) })
  : m.departs > 0 && m.meme_stress > 0 ? t('com_meme_stress', { n: m.meme_stress, meilleur: departTexte(m.meilleur) })
    : m.departs > 0 ? t('com_pas_retrouvee', { meilleur: departTexte(m.meilleur) }) : t('com_sans_depart'));

// ------------------------------------------------------------ the fitted variables (PCOA, NMDS)

function rendreAjustement(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const aj = m.ajustement;
  if (!aj) { conteneur.replaceChildren(el('div', { class: 'defile resume' }, el('p', { class: 'note' }, t('com_sans_x')))); return; }
  const ax = liste(aj.axes);
  const alerteP = (k) => (rows) => (i, j) => j === k && significatif(rows[i][k]);
  const blocs = [el('p', { class: 'note' }, t('com_ajustement_note', { axes: ax.join(', '), n: aj.n, perm: textePermutations(m) })
    + (aj.exclues ? ` ${t('com_ajustement_exclues', { n: aj.exclues })}` : ''))];
  if (aj.vecteurs) {
    const rows = lignesDe(aj.directions, ['terme', ...ax]).map((l, i) => [l[0], liste(aj.vecteurs.r2)[i], liste(aj.vecteurs.p)[i], ...l.slice(1)]);
    blocs.push(el('h4', {}, t('com_vecteurs')), tableCompacte([t('variables'), 'R²', 'p', ...ax.map((a) => `cos ${a}`)],
      rows.map((l) => [l[0], fmt(l[1], 4), pValeur(l[2]), ...l.slice(3).map((v) => fmt(v, 4))]), alerteP(2)(rows)));
  }
  if (aj.facteurs) {
    const rows = lignesDe(aj.facteurs, ['variable', 'r2', 'p']);
    blocs.push(el('h4', {}, t('com_facteurs')), tableCompacte([t('variables'), 'R²', 'p'], rows.map((l) => [l[0], fmt(l[1], 4), pValeur(l[2])]), alerteP(2)(rows)));
    const n = axes(m).slice(0, 3);
    blocs.push(tableCompacte([t('ord_centroides'), 'n', ...n], lignesDe(aj.centroides, ['terme', 'n', ...n]).map((l) => [l[0], String(l[1]), ...l.slice(2).map((v) => fmt(v, 4))])));
  }
  blocs.push(el('p', { class: 'note' }, t('com_p_rouge')));
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, ...blocs));
}

// ------------------------------------------------------------ the PERMANOVA table

function rendrePermanova(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const rows = lignesDe(m.table, ['terme', 'ddl', 'ss', 'R2', 'F', 'p']);
  const mo = m.modele;
  const graphe = el('div', { class: 'graphe petit' });
  const inegales = liste(m.dispersions).filter((d) => significatif(d.p_permutation)).map((d) => d.variable);
  conteneur.replaceChildren(el('div', { class: 'defile resume' },
    el('div', { class: 'fiche' }, el('div', { class: 'fiche-titre' }, el('b', {}, initiale(t('com_par_' + m.par)))),
      el('div', { class: 'fiche-chiffres' }, chiffre('R2', mo.R2), chiffre(t('ord_pseudo_f'), mo.F, 3), chiffre('p', pValeur(mo.p)),
        chiffre(t('nb_permutations'), (m.distance || {}).permutations ?? 0, 0), m.strates ? chiffre(t('ord_dans'), m.strates) : null)),
    tableCompacte([t('terme'), t('ddl'), t('com_ss'), 'R²', t('ord_pseudo_f'), 'p'],
      rows.map((l) => [l[0], fmt(l[1], 0), fmt(l[2], 5), fmt(l[3], 4), l[4] === null ? '' : fmt(l[4], 3), l[5] === null ? '' : pValeur(l[5])]),
      (i, j) => j === 5 && significatif(rows[i][5])),
    el('p', { class: 'note' }, t('com_permanova_note', { perm: textePermutations(m) })),
    liste(m.alias).length ? el('p', { class: 'note' }, t('ord_alias', { noms: liste(m.alias).join(', ') })) : null,
    inegales.length ? el('p', { class: 'note erreur' }, t('com_dispersion_alerte', { vars: inegales.join(', ') })) : null, graphe));
  const termes = rows.slice(0, rows.length - 1);  // (without the total)
  tracer(ctx, graphe, [{ type: 'bar', x: termes.map((l) => l[0]), y: termes.map((l) => l[3]),
    marker: { color: termes.map((l, i) => (i === termes.length - 1 ? GRIS : PALETTE[0])) }, hovertemplate: '<b>%{x}</b><br>R² %{y:.4f}<extra></extra>' }],
  { title: { text: `${m.nom} · R²` }, dragmode: false, showlegend: false, yaxis: { title: { text: 'R²' }, rangemode: 'tozero' }, xaxis: { automargin: true } });
}

// ------------------------------------------------------------ the multivariate dispersion

// Each grouping's distances to the centre of its group (vegan: betadisper), as box plots or on the
// first two axes of its PCoA; its ANOVA, permutation test and Tukey pairs.
function rendreDispersion(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const ds = liste(m.dispersions);
  if (!ds.length) { conteneur.replaceChildren(el('div', { class: 'defile resume' }, el('p', { class: 'note' }, t('com_sans_dispersion')))); return; }
  loc.groupement = Math.min(loc.groupement ?? 0, ds.length - 1);
  loc.mode ??= 'boites';
  const d = ds[loc.groupement];
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas defile' });
  conteneur.replaceChildren(outils(
    ds.length > 1 ? choix(t('com_groupement'), ds.map((x, i) => [i, x.variable]), loc.groupement, (v) => { loc.groupement = +v; ctx.rafraichir(); }) : null,
    choix('', [['boites', t('com_boites')], ['pcoa', t('com_plan_pcoa')]], loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    el('span', { class: 'infos' }, `F ${fmt(d.F, 3)} · p ${pValeur(d.p_permutation)} (${t('ord_permutations_mot')})`)), graphe, bas);
  const groupes = liste(d.groupes.groupe);
  const g = liste(d.groupe);
  const z = liste(d.distance);
  const uids = liste(d.uid);
  const noms = new Map(liste(m.observations.uid).map((u, i) => [u, liste(m.observations.nom)[i]]));
  const sel = new Set(ctx.selection(m.id));
  const c = couleursTheme(ctx);
  const centre = t(d.type === 'mediane' ? 'com_mediane' : 'com_centroide');
  if (loc.mode === 'pcoa') {
    const cx = liste(d.coord.PCoA1);
    const cy = d.coord.PCoA2 ? liste(d.coord.PCoA2) : cx.map(() => 0);
    const traces = groupes.map((k, j) => {
      const idx = g.map((_, i) => i).filter((i) => g[i] === k);
      return { type: 'scatter', mode: 'markers', name: k, x: idx.map((i) => cx[i]), y: idx.map((i) => cy[i]), customdata: idx.map((i) => uids[i]),
        hovertext: idx.map((i) => noms.get(uids[i])), marker: { color: idx.map((i) => (sel.has(uids[i]) ? '#e4572e' : PALETTE[j % PALETTE.length])), size: 7 },
        hovertemplate: `<b>%{hovertext}</b><br>%{x:.3f}${entreNombres()}%{y:.3f}<extra>%{fullData.name}</extra>` };
    });
    traces.push({ type: 'scatter', mode: 'markers+text', name: centre, x: liste(d.centres.PCoA1), y: d.centres.PCoA2 ? liste(d.centres.PCoA2) : groupes.map(() => 0),
      text: liste(d.centres.groupe), textposition: 'top center', marker: { symbol: 'x-thin', size: 14, line: { width: 2.5, color: c.texte } },
      textfont: { size: 11, color: c.texte }, hovertemplate: '<b>%{text}</b><extra></extra>' });
    tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${t('dispersion')} · ${d.variable}` },
      xaxis: { title: { text: 'PCoA1' }, zeroline: true, constrain: 'domain' }, yaxis: { title: { text: 'PCoA2' }, zeroline: true, scaleanchor: 'x', scaleratio: 1, constrain: 'domain' } });
  } else {
    tracer(ctx, graphe, groupes.map((k, j) => {
      const idx = g.map((_, i) => i).filter((i) => g[i] === k);
      return { type: 'box', name: k, y: idx.map((i) => z[i]), customdata: idx.map((i) => uids[i]), text: idx.map((i) => noms.get(uids[i])),
        boxpoints: 'all', jitter: 0.4, pointpos: 0, marker: { color: PALETTE[j % PALETTE.length], size: 5 }, line: { width: 1 },
        hovertemplate: '<b>%{text}</b><br>%{y:.4f}<extra>%{fullData.name}</extra>' };
    }), { title: { text: `${m.nom} · ${t('dispersion')} · ${d.variable}` }, showlegend: false, dragmode: 'select',
      yaxis: { title: { text: t('com_distance_centre', { c: centre }) }, rangemode: 'tozero' } });
  }
  brancherSelection(graphe, ctx, m.id);
  const av = lignesDe(d.anova, ['source', 'ddl', 'ss', 'ms', 'F', 'p', 'p_permutation']);
  const tk = lignesDe(d.tukey, ['paire', 'difference', 'bas', 'haut', 'p']);
  bas.replaceChildren(...[  // (replaceChildren would write null as text)
    el('p', { class: 'note' }, t('com_dispersion_note', { c: centre })),
    d.negatives ? el('p', { class: 'note' }, t('com_dispersion_negatives', { n: d.negatives })) : null,
    tableCompacte([t('groupe'), 'n', t('moyenne'), t('com_mediane_mot')], lignesDe(d.groupes, ['groupe', 'n', 'moyenne', 'mediane']).map((l) => [l[0], String(l[1]), fmt(l[2], 4), fmt(l[3], 4)])),
    tableCompacte(['', t('ddl'), t('com_ss'), t('com_cm'), 'F', 'p (F)', t('com_p_permutation')],
      av.map((l) => [l[0], fmt(l[1], 0), fmt(l[2], 5), fmt(l[3], 5), l[4] === null ? '' : fmt(l[4], 3), l[5] === null ? '' : pValeur(l[5]), l[6] === null ? '' : pValeur(l[6])]),
      (i, j) => j === 6 && significatif(av[i][6])),
    el('h4', {}, t('com_tukey')),
    tableCompacte([t('com_paire'), t('com_difference'), t('com_ic_bas'), t('com_ic_haut'), t('com_p_ajustee')],
      tk.map((l) => [l[0], fmt(l[1], 4), fmt(l[2], 4), fmt(l[3], 4), pValeur(l[4])]), (i, j) => j === 4 && significatif(tk[i][4])),
    significatif(d.p_permutation) ? el('p', { class: 'note erreur' }, t('com_dispersion_alerte', { vars: d.variable })) : null].filter(Boolean));
}

// ------------------------------------------------------------ Mantel tests and the correlogram

function rendreMantel(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const s = m.test;
  const pa = m.paires;
  const graphe = el('div', { class: 'graphe' });
  const fiche = el('div', { class: 'fiche' }, el('div', { class: 'fiche-titre' }, el('b', {}, t('com_mantel_' + m.methode))),
    el('div', { class: 'fiche-chiffres' }, chiffre('r', s.r, 4), chiffre('p', pValeur(s.p)), chiffre(t('nb_permutations'), s.permutations, 0), m.strates ? chiffre(t('ord_dans'), m.strates) : null),
    s.r_partiel !== undefined && s.r_partiel !== null ? el('div', { class: 'fiche-chiffres' }, chiffre(t('com_r_partiel'), s.r_partiel, 4), chiffre('p', pValeur(s.p_partiel)),
      chiffre(t('com_z'), liste(m.termes_z).join(', '))) : null);
  const notes = [t('com_mantel_note', { termes: liste(m.termes_x).join(', '), echelles: liste(m.echelles_x).map((e) => t('echelle_' + e)).join(', ') })];
  if (pa.total > liste(pa.y).length) notes.push(t('com_paires_extrait', { n: liste(pa.y).length, total: pa.total }));
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, el('p', { class: 'note' }, notes.join(' ')), graphe));
  tracer(ctx, graphe, [{ type: 'scattergl', mode: 'markers', name: t('com_paires'), x: liste(pa.x), y: liste(pa.y),
    marker: { size: 4, color: PALETTE[0], opacity: 0.4 }, hovertemplate: 'X %{x:.3f} · Y %{y:.3f}<extra></extra>' }],
  { title: { text: `${m.nom} · Mantel · r = ${fmt(s.r, 3)}` }, dragmode: 'zoom', xaxis: { title: { text: t(m.distance_x === 'geographique' ? 'com_distance_x_km' : 'com_distance_x_axe') } }, yaxis: { title: { text: t('com_distance_y', { d: nomDistance(m.distance || {}) }) } } });
}

function rendreCorrelogramme(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const cg = m.correlogramme;
  if (!cg) { conteneur.replaceChildren(el('p', { class: 'note' }, t('com_sans_correlogramme'))); return; }
  const tb = cg.table;
  const centres = liste(tb.centre);
  const r = liste(tb.r);
  const pc = liste(tb.p_corrige);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('com_correlogramme_note', { n: cg.classes }) + (cg.sturges ? ` ${t('com_sturges')}` : '')), graphe, bas);
  const c = couleursTheme(ctx);
  const idx = r.map((_, i) => i).filter((i) => r[i] !== null);
  tracer(ctx, graphe, [
    { type: 'scatter', mode: 'lines+markers', name: t('com_r_mantel'), x: idx.map((i) => centres[i]), y: idx.map((i) => r[i]),
      customdata: idx.map((i) => [liste(tb.paires)[i], pValeur(pc[i])]), line: { color: c.doux, width: 1.2 },
      marker: { size: 10, symbol: idx.map((i) => (significatif(pc[i]) ? 'square' : 'square-open')), color: PALETTE[0], line: { width: 1.5, color: PALETTE[0] } },
      hovertemplate: '%{x:.3f}<br>r %{y:.3f} · %{customdata[0]} ' + t('com_paires_mot') + '<br>p ' + t('com_corrige_mot') + ' %{customdata[1]}<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('correlogramme')}` }, dragmode: false, showlegend: false, xaxis: { title: { text: t(m.distance_x === 'geographique' ? 'com_classe_distance_km' : 'com_classe_distance_x') } },
    yaxis: { title: { text: t('com_r_mantel') }, zeroline: true } });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: centres.length, nom: 'Correlogram',
    colonnes: [{ cle: 'classe', titre: t('com_classe'), decimales: 0 }, { cle: 'centre', titre: t(m.distance_x === 'geographique' ? 'com_centre_classe_km' : 'com_centre_classe'), num: true, decimales: 4 },
      { cle: 'paires', titre: t('com_paires'), num: true, decimales: 0 }, { cle: 'r', titre: 'r', num: true, decimales: 4 },
      { cle: 'p', titre: 'p', num: true, format: (v) => pValeur(v) }, { cle: 'p_corrige', titre: t('com_p_holm'), num: true, format: (v) => pValeur(v), alerte: (i) => significatif(pc[i]) },
      { cle: 'teste', titre: t('com_teste') }],
    valeur: (i, k) => (k === 'teste' ? (liste(tb.teste)[i] ? t('ord_oui') : t('ord_non')) : liste(tb[k])[i]),
  });
}

// ------------------------------------------------------------ summary

function rendreResumeCommunaute(conteneur, ctx, m) {
  const ligne1 = el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('com_variables_y'), m.K, 0),
    m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null);
  let ligne2 = null;
  const textes = [texteDistance(m)];
  if (m.type === 'PCOA') {
    const part = liste(m.valeurs_propres.part);
    ligne2 = el('div', { class: 'fiche-chiffres' }, chiffre(t('com_inertie_totale'), m.inertie.totale, 4), ...axes(m).slice(0, 3).map((a, k) => chiffre(a, `${fmt(100 * part[k], 1)} %`)),
      m.negatives && m.negatives.n ? chiffre(t('com_negatives'), m.negatives.n, 0) : null);
    textes.push(t('com_resume_pcoa'));
  } else if (m.type === 'NMDS') {
    ligne2 = el('div', { class: 'fiche-chiffres' }, chiffre('stress', m.stress, 4), chiffre('k', m.k, 0), chiffre(t('com_departs'), m.departs, 0),
      chiffre(t('com_retrouvee_mot'), m.retrouvee, 0), !m.retrouvee && m.meme_stress > 0 ? chiffre(t('com_meme_stress_mot'), m.meme_stress, 0) : null);
    if (m.departs > 0 && !m.retrouvee) textes.push(texteRetrouvee(m));
    if (m.nuls) textes.push(t('com_nuls', { n: m.nuls }));
    textes.push(t('com_resume_nmds'));
  } else if (m.type === 'PERMANOVA') {
    const mo = m.modele;
    ligne2 = el('div', { class: 'fiche-chiffres' }, chiffre('R2', mo.R2), chiffre(t('ord_pseudo_f'), mo.F, 3), chiffre('p', pValeur(mo.p)), chiffre(t('nb_termes'), liste(m.termes).length, 0));
    liste(m.dispersions).forEach((d) => textes.push(t('com_dispersion_resume', { v: d.variable, f: fmt(d.F, 3), p: pValeur(d.p_permutation) })));
    textes.push(t('com_resume_permanova', { par: t('com_par_' + m.par) }));
  } else {
    const s = m.test;
    ligne2 = el('div', { class: 'fiche-chiffres' }, chiffre('r', s.r, 4), chiffre('p', pValeur(s.p)),
      s.r_partiel !== undefined && s.r_partiel !== null ? [chiffre(t('com_r_partiel'), s.r_partiel, 4), chiffre('p', pValeur(s.p_partiel))] : null);
    const cg = m.correlogramme;
    if (cg) textes.push(t('com_correlogramme_resume', { n: liste(cg.table.p_corrige).filter(significatif).length, classes: cg.classes }));
    textes.push(t('com_resume_mantel'));
  }
  if (['PERMANOVA', 'MANTEL'].includes(m.type) || m.ajustement) textes.push(t('com_permutations_texte', { perm: textePermutations(m) }));
  const fiche = el('div', { class: 'fiche' }, titreFiche(m), ligne1, ligne2, ficheCondition(m));
  const notes = notesModele(conteneur, ctx, m);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ, textes.map((x) => el('p', { class: 'note' }, x))));
  notes.retrouver();
}

// ------------------------------------------------------------ the model dialog

const DISTANCES = ['bray', 'jaccard', 'gower', 'euclidean', 'manhattan', 'canberra', 'kulczynski', 'hellinger', 'chord'];
const TRANSFORMATIONS = ['aucune', 'racine2', 'racine4', 'log1p', 'presence', 'relative', 'wisconsin', 'racine_wisconsin'];
const entier = (v, min, max) => v === undefined || v === null || (Number.isInteger(v) && v >= min && v <= max);

// The fields every type shares (the distance and its transformation, the permutations), then
// those of each type; their values kept in st.options.
function champsCommunaute(type) {
  return (st, rendre, p) => {
    const o = st.options;
    // (a Mantel test being edited: its covariates' transformations and scalings, kept in its options)
    if (type === 'MANTEL' && !o.z_lues) {
      for (const [u, v] of Object.entries(o.z_transformations || {})) if (!st.transfos[u]) st.transfos[u] = v;
      for (const [u, v] of Object.entries(o.z_echelles || {})) if (!st.echelles[u]) st.echelles[u] = v;
      o.z_lues = true;
    }
    const nombre = (cle, defaut, attrs) => el('input', { type: 'number', value: o[cle] ?? defaut, style: 'width:6em', ...attrs,
      oninput: (e) => { o[cle] = e.target.value === '' ? null : Number(e.target.value); } });
    const select = (cle, valeurs, defaut, libelle) => el('select', { onchange: (e) => { o[cle] = e.target.value; } },
      valeurs.map((k) => el('option', { value: k, selected: k === (o[cle] || defaut) }, libelle(k))));
    const ids = [].concat(p.identifiants || []).map((i) => i.nom);
    const qualitatives = p.variables.filter((v) => v.qualitative);
    const strate = !o.strates ? '' : o.strates.by === 'id' ? `id:${o.strates.id}` : `var:${o.strates.variable}`;
    const option = (v, n) => el('option', { value: v, selected: v === strate }, n);
    return [
      el('label', {}, t('com_distance')), el('div', { class: 'ligne' }, select('distance', DISTANCES, 'bray', (k) => t('com_d_' + k)),
        el('label', { class: 'champ', title: t('com_binaire_aide') }, el('input', { type: 'checkbox', checked: !!o.binaire, onchange: (e) => { o.binaire = e.target.checked; } }), t('com_binaire'))),
      el('label', {}, t('com_transformation')), select('transformation', TRANSFORMATIONS, 'aucune', (k) => t('com_tr_' + k)),
      ['PCOA', 'PERMANOVA'].includes(type) ? [el('label', {}, t('com_correction')), select('correction', ['aucune', 'lingoes', 'cailliez'], 'aucune', (k) => t('com_corr_' + k))] : null,
      type === 'NMDS' ? [el('label', {}, t('com_dimensions')), el('div', { class: 'ligne' }, nombre('k', 2, { min: 1, max: 4, step: 1 }),
        el('label', { class: 'champ' }, t('com_departs_champ'), nombre('departs', 20, { min: 0, max: 200, step: 1 })))] : null,
      type === 'PERMANOVA' ? [el('label', {}, t('com_tests')), select('par', ['termes', 'marges', 'modele'], 'termes', (k) => initiale(t('com_par_' + k))),
        el('label', {}, t('com_centre_dispersion')), select('dispersion', ['centroide', 'mediane'], 'centroide', (k) => t('com_' + k))] : null,
      type === 'MANTEL' ? [el('label', { title: t('com_distance_x_aide') }, t('com_distance_x')),
        el('select', { disabled: !p.coordonnees && o.distance_x !== 'geographique', onchange: (e) => { o.distance_x = e.target.value; rendre(); } },
          [['variables', t('com_distance_x_variables')], ['geographique', p.coordonnees ? t('com_distance_x_geo', { x: p.coordonnees.nom_x, y: p.coordonnees.nom_y }) : t('com_distance_x_sans')]]
            .map(([k, n]) => el('option', { value: k, selected: k === (o.distance_x || 'variables') }, n))),
        el('label', {}, t('com_correlation')), select('methode', ['pearson', 'spearman'], 'pearson', (k) => t('com_mantel_' + k)),
        el('label', {}, t('com_classes')), el('div', { class: 'ligne' }, nombre('classes', 0, { min: 0, max: 50, step: 1 }), el('span', { class: 'doux' }, t('com_classes_aide')))] : null,
      el('label', {}, t('nb_permutations')), el('div', { class: 'ligne' }, nombre('permutations', 999, { min: 0, max: 99999, step: 1 }),
        el('label', { class: 'champ' }, t('ord_graine'), nombre('graine', 1, { step: 1 }))),
      el('label', {}, t('ord_strates')), el('select', { onchange: (e) => {
        const v = e.target.value;
        o.strates = !v ? null : v.startsWith('id:') ? { by: 'id', id: v.slice(3) } : { by: 'variable', variable: Number(v.slice(4)) };
      } }, option('', t('ord_strates_aucune')),
      ids.length ? el('optgroup', { label: t('vc_par_identifiant') }, ids.map((n) => option(`id:${n}`, n))) : null,
      qualitatives.length ? el('optgroup', { label: t('vc_par_variable') }, qualitatives.map((v) => option(`var:${v.uid}`, v.nom))) : null),
      el('label', {}, ''), el('p', { class: 'note' }, t('com_reglages_note_' + type.toLowerCase())),
    ].flat().filter(Boolean);
  };
}
const nomVariable = (p, u) => (p.variables.find((v) => v.uid === u) || {}).nom;
const qualitativesDe = (p, us) => us.filter((u) => (p.variables.find((v) => v.uid === u) || {}).qualitative);
function validerCommunaute(type) {
  return (st, cles, p) => {
    if (!cles('y').length) return t('com_y_requis');
    const q = qualitativesDe(p, cles('y'));
    if (q.length) return t('ord_quantitatives', { noms: q.map((u) => nomVariable(p, u)).join(', ') });
    if (['PERMANOVA', 'MANTEL'].includes(type) && !cles('x').length && !(type === 'MANTEL' && st.options.distance_x === 'geographique')) return t('com_x_requis_' + type.toLowerCase());
    if (['PCOA', 'NMDS'].includes(type) && st.croises.length) return t('com_croises');
    const o = st.options;
    if (!entier(o.permutations, 0, 99999)) return t('ord_permutations_invalides');
    if (type === 'NMDS' && (!entier(o.k, 1, 4) || !entier(o.departs, 0, 200))) return t('com_nmds_invalide');
    if (type === 'MANTEL' && !entier(o.classes, 0, 50)) return t('com_classes_invalides');
    return '';
  };
}
function optionsCommunaute(type) {
  return (st, cles) => {
    const o = st.options;
    const v = (k, d) => (Number.isFinite(o[k]) ? Math.round(o[k]) : d);
    const out = { distance: o.distance || 'bray', binaire: !!o.binaire, transformation: o.transformation || 'aucune', permutations: v('permutations', 999),
      graine: v('graine', 1), strates: o.strates || null };
    if (['PCOA', 'PERMANOVA'].includes(type)) out.correction = o.correction || 'aucune';
    if (type === 'NMDS') Object.assign(out, { k: v('k', 2), departs: v('departs', 20) });
    if (type === 'PERMANOVA') Object.assign(out, { par: o.par || 'termes', dispersion: o.dispersion || 'centroide' });
    if (type === 'MANTEL') {
      const z = cles('z');
      Object.assign(out, { methode: o.methode || 'pearson', classes: v('classes', 0), z, distance_x: o.distance_x === 'geographique' ? 'geographique' : 'variables',
        z_transformations: Object.fromEntries(z.filter((u) => st.transfos[u] && st.transfos[u] !== 'autre').map((u) => [u, st.transfos[u]])),
        z_echelles: Object.fromEntries(z.map((u) => [u, st.echelles[u] || st.echelle || 'UV'])) });
    }
    return out;
  };
}
for (const type of TYPES) {
  ANALYSES[type] = {
    libelle: () => t('type_' + type.toLowerCase()),
    note: () => t('com_note_' + type.toLowerCase()),
    roles: { PCOA: ['x', 'y'], NMDS: ['x', 'y'], PERMANOVA: ['x', 'y', 'classe'], MANTEL: ['x', 'y', 'z'] }[type],
    echelle: type === 'MANTEL' ? 'UV' : 'none',
    vc: false,
    // (X fitted onto the ordination, if any; a Mantel test on the geographic distance needs none)
    xFacultatif: ['PCOA', 'NMDS'].includes(type) || (type === 'MANTEL' ? (st) => st.options.distance_x === 'geographique' : false),
    champs: champsCommunaute(type),
    valider: validerCommunaute(type),
    options: optionsCommunaute(type),
  };
}

// ------------------------------------------------------------ the report

const blocsResume = (m) => {
  const b = [{ texte: `${t('type_' + m.type.toLowerCase())} · N = ${m.N} · ${t('com_variables_y')} ${m.K}${m.exclues_manquantes ? ` · ${t('obs_incompletes')} ${m.exclues_manquantes}` : ''}` },
    { texte: texteDistance(m) }];
  if (m.type === 'PCOA') {
    b.push({ entetes: [t('ord_axe'), t('valeur_propre'), t('ord_part_inertie'), `${t('ord_cumul')} (%)`],
      lignes: lignesDe(m.valeurs_propres, ['axe', 'valeur_propre', 'part', 'cumul']).slice(0, 5).map((l) => [l[0], fmt(l[1], 5), fmt(100 * l[2], 2), fmt(100 * l[3], 2)]) });
  } else if (m.type === 'NMDS') {
    const memeStress = !m.retrouvee && m.meme_stress > 0;
    b.push({ entetes: ['stress', 'k', t('com_departs'), t('com_retrouvee_mot')].concat(memeStress ? [t('com_meme_stress_mot')] : []),
      lignes: [[fmt(m.stress, 4), String(m.k), String(m.departs), String(m.retrouvee)].concat(memeStress ? [String(m.meme_stress)] : [])] });
  } else if (m.type === 'PERMANOVA') {
    b.push({ entetes: ['R2', t('ord_pseudo_f'), 'p', t('nb_permutations')], lignes: [[fmt(m.modele.R2, 4), fmt(m.modele.F, 3), pValeur(m.modele.p), String(m.distance.permutations)]] });
  } else {
    b.push({ entetes: ['', 'r', 'p'], lignes: [['Mantel', fmt(m.test.r, 4), pValeur(m.test.p)]]
      .concat(m.test.r_partiel !== undefined && m.test.r_partiel !== null ? [[t('com_r_partiel'), fmt(m.test.r_partiel, 4), pValeur(m.test.p_partiel)]] : []) });
  }
  return b;
};
const blocsValeursPropres = (m) => [{ entetes: [t('ord_axe'), t('valeur_propre'), t('ord_part_inertie'), `${t('ord_cumul')} (%)`],
  lignes: lignesDe(m.valeurs_propres, ['axe', 'valeur_propre', 'part', 'cumul']).slice(0, 10).map((l) => [l[0], fmt(l[1], 5), fmt(100 * l[2], 2), fmt(100 * l[3], 2)]) }]
  .concat(m.negatives && m.negatives.n ? [{ texte: t('com_vp_negatives', { n: m.negatives.n, part: fmt(100 * m.negatives.part, 1) }) }] : []);
const blocsShepard = (m) => [{ texte: t('com_shepard_note', { stress: fmt(m.stress, 3), r2: fmt(1 - m.stress * m.stress, 3) }) },
  { texte: texteRetrouvee(m) }];
const blocsAjustement = (m) => {
  const aj = m.ajustement;
  if (!aj) return [{ texte: t('com_sans_x') }];
  const b = [{ texte: t('com_ajustement_note', { axes: liste(aj.axes).join(', '), n: aj.n, perm: textePermutations(m) }) }];
  if (aj.vecteurs) b.push({ entetes: [t('variables'), 'R²', 'p'], lignes: lignesDe(aj.vecteurs, ['terme', 'r2', 'p']).map((l) => [l[0], fmt(l[1], 4), pValeur(l[2])]) });
  if (aj.facteurs) b.push({ entetes: [t('variables'), 'R²', 'p'], lignes: lignesDe(aj.facteurs, ['variable', 'r2', 'p']).map((l) => [l[0], fmt(l[1], 4), pValeur(l[2])]) });
  return b;
};
const blocsPermanova = (m) => [{ texte: `${initiale(t('com_par_' + m.par))} · ${textePermutations(m)}` },
  { entetes: [t('terme'), t('ddl'), t('com_ss'), 'R²', t('ord_pseudo_f'), 'p'],
    lignes: lignesDe(m.table, ['terme', 'ddl', 'ss', 'R2', 'F', 'p']).map((l) => [l[0], fmt(l[1], 0), fmt(l[2], 5), fmt(l[3], 4), l[4] === null ? '' : fmt(l[4], 3), l[5] === null ? '' : pValeur(l[5])]) }]
  .concat(liste(m.dispersions).filter((d) => significatif(d.p_permutation)).length
    ? [{ texte: t('com_dispersion_alerte', { vars: liste(m.dispersions).filter((d) => significatif(d.p_permutation)).map((d) => d.variable).join(', ') }) }] : []);
const blocsDispersion = (m) => liste(m.dispersions).flatMap((d) => [{ texte: `${d.variable}${deuxPoints()}F = ${fmt(d.F, 3)}, p = ${pValeur(d.p_permutation)} (${t('ord_permutations_mot')})` },
  { entetes: [t('groupe'), 'n', t('moyenne')], lignes: lignesDe(d.groupes, ['groupe', 'n', 'moyenne']).map((l) => [l[0], String(l[1]), fmt(l[2], 4)]) },
  { entetes: [t('com_paire'), t('com_difference'), t('com_ic_bas'), t('com_ic_haut'), t('com_p_ajustee')],
    lignes: lignesDe(d.tukey, ['paire', 'difference', 'bas', 'haut', 'p']).map((l) => [l[0], fmt(l[1], 4), fmt(l[2], 4), fmt(l[3], 4), pValeur(l[4])]) }]);
const blocsMantel = (m) => [{ texte: t('com_mantel_note', { termes: liste(m.termes_x).join(', '), echelles: liste(m.echelles_x).map((e) => t('echelle_' + e)).join(', ') }) },
  { texte: `r = ${fmt(m.test.r, 4)}, p = ${pValeur(m.test.p)} · ${textePermutations(m)}` }]
  .concat(m.test.r_partiel !== undefined && m.test.r_partiel !== null
    ? [{ texte: `${t('com_r_partiel')} (${liste(m.termes_z).join(', ')})${deuxPoints()}r = ${fmt(m.test.r_partiel, 4)}, p = ${pValeur(m.test.p_partiel)}` }] : []);
const blocsCorrelogramme = (m) => (!m.correlogramme ? [{ texte: t('com_sans_correlogramme') }] : [{ texte: t('com_correlogramme_note', { n: m.correlogramme.classes }) },
  { entetes: [t('com_classe'), t(m.distance_x === 'geographique' ? 'com_centre_classe_km' : 'com_centre_classe'), t('com_paires'), 'r', 'p', t('com_p_holm')],
    lignes: lignesDe(m.correlogramme.table, ['classe', 'centre', 'paires', 'r', 'p', 'p_corrige']).map((l) => [String(l[0]), fmt(l[1], 4), String(l[2]),
      l[3] === null ? '' : fmt(l[3], 4), l[4] === null ? '' : pValeur(l[4]), l[5] === null ? '' : pValeur(l[5])]) }]);
PARTIES_TYPES.PCOA = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'ordination_dist', vue: 'ordination_dist', defaut: true },
  { cle: 'valeurs_propres_pcoa', vue: 'valeurs_propres_pcoa', defaut: true, tableau: blocsValeursPropres },
  { cle: 'ajustement_env', defaut: true, si: (m) => !!m.ajustement, tableau: blocsAjustement },
];
PARTIES_TYPES.NMDS = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'ordination_dist', vue: 'ordination_dist', defaut: true },
  { cle: 'shepard', vue: 'shepard', defaut: true, tableau: blocsShepard },
  { cle: 'ajustement_env', defaut: true, si: (m) => !!m.ajustement, tableau: blocsAjustement },
];
PARTIES_TYPES.PERMANOVA = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'permanova', vue: 'permanova', defaut: true, tableau: blocsPermanova },
  { cle: 'dispersion', vue: 'dispersion', defaut: true, si: (m) => liste(m.dispersions).length > 0, tableau: blocsDispersion,
    variantes: async (m) => liste(m.dispersions).map((_, i) => ({ groupement: i, mode: 'boites' })) },
];
PARTIES_TYPES.MANTEL = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'mantel', vue: 'mantel', defaut: true, tableau: blocsMantel },
  { cle: 'correlogramme', vue: 'correlogramme', defaut: true, si: (m) => !!m.correlogramme, tableau: blocsCorrelogramme },
];

// ------------------------------------------------------------ registration

const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estCommunaute(m) ? rendreResumeCommunaute(c, ctx, m) : resume(c, ctx, p, loc);
} };
Object.assign(VUES, {
  ordination_dist: { rendre: rendreOrdinationDist, titre: (m) => `${m.nom} · ${t('ordination_dist')}` },
  valeurs_propres_pcoa: { rendre: rendreValeursPropresPCoA, titre: (m) => `${m.nom} · ${t('valeurs_propres_pcoa')}` },
  shepard: { rendre: rendreShepard, titre: (m) => `${m.nom} · ${t('shepard')}` },
  ajustement_env: { rendre: rendreAjustement, titre: (m) => `${m.nom} · ${t('ajustement_env')}` },
  permanova: { rendre: rendrePermanova, titre: (m) => `${m.nom} · ${t('permanova')}` },
  dispersion: { rendre: rendreDispersion, titre: (m) => `${m.nom} · ${t('dispersion')}` },
  mantel: { rendre: rendreMantel, titre: (m) => `${m.nom} · ${t('mantel')}` },
  correlogramme: { rendre: rendreCorrelogramme, titre: (m) => `${m.nom} · ${t('correlogramme')}` },
});
Object.assign(ICONES_VUES, { ordination_dist: 'chart-scatter', valeurs_propres_pcoa: 'chart-column', shepard: 'chart-spline', ajustement_env: 'crosshair',
  permanova: 'scale', dispersion: 'shapes', mantel: 'shuffle', correlogramme: 'chart-line' });
VUES_TYPES.PCOA = ['resume', 'ordination_dist', 'valeurs_propres_pcoa', 'ajustement_env', 'liste_obs', 'liste_vars'];
VUES_TYPES.NMDS = ['resume', 'ordination_dist', 'shepard', 'ajustement_env', 'liste_obs', 'liste_vars'];
VUES_TYPES.PERMANOVA = (m) => ['resume', 'permanova'].concat(liste(m.dispersions).length ? ['dispersion'] : [], ['liste_obs']);
VUES_TYPES.MANTEL = (m) => ['resume', 'mantel'].concat(m.correlogramme ? ['correlogramme'] : [], ['liste_obs']);

ajouterTextes({
  com_distance_x: 'Distance X', com_distance_x_variables: 'Des variables X (euclidienne)', com_distance_x_geo: 'Géographique ({x}, {y})',
  com_distance_x_sans: 'Géographique : choisissez d’abord les coordonnées (Graphiques › Carte)',
  com_distance_x_aide: 'Ce à quoi la distance du tableau Y est comparée : celle des variables X, ou la distance géographique entre les observations (en km), pour voir si des relevés proches se ressemblent plus.',
  type_pcoa: 'PCoA (analyse en coordonnées principales)', type_nmds: 'NMDS (positionnement multidimensionnel non métrique)',
  type_permanova: 'PERMANOVA (analyse de variance par permutation)', type_mantel: 'Test de Mantel',
  ordination_dist: 'Ordination', valeurs_propres_pcoa: 'Valeurs propres', shepard: 'Diagramme de Shepard', ajustement_env: 'Variables ajustées',
  permanova: 'Table de la PERMANOVA', dispersion: 'Dispersion multivariée', mantel: 'Test de Mantel', correlogramme: 'Corrélogramme de Mantel',
  com_note_pcoa: 'Y : le tableau des espèces (ou des variables dont on calcule la distance) ; X, s’il y en a : des variables ajustées sur les deux premiers axes (une flèche par variable quantitative, un centroïde par catégorie d’une qualitative ; comme vegan::envfit). Une observation où une valeur Y manque est laissée de côté.',
  com_note_nmds: 'Y : le tableau des espèces ; X, s’il y en a : des variables ajustées sur les dimensions du NMDS. MASS::isoMDS depuis la configuration de la PCoA puis des départs au hasard (graine fixe), comme vegan::metaMDS : la solution de plus faible stress est gardée.',
  com_note_permanova: 'Y : le tableau des espèces ; X : les facteurs ou gradients testés (qualitatifs ou quantitatifs, termes croisés compris, dans leur ordre) ; Classe : un groupement de plus pour la dispersion. La dispersion de chaque variable qualitative est aussi comparée (comme vegan::betadisper).',
  com_note_mantel: 'Y : le tableau des espèces ; X : les variables dont la distance euclidienne (mises à l’échelle comme le dit l’onglet Variables, UV par défaut) est comparée à celle de Y ; Z : des covariables d’un test partiel.',
  com_distance: 'Distance', com_transformation: 'Transformation avant la distance', com_correction: 'Correction des valeurs propres négatives',
  com_d_bray: 'Bray-Curtis', com_d_jaccard: 'Jaccard (2B / (1 + B))', com_d_gower: 'Gower', com_d_euclidean: 'euclidienne', com_d_manhattan: 'Manhattan',
  com_d_canberra: 'Canberra', com_d_kulczynski: 'Kulczynski', com_d_hellinger: 'Hellinger', com_d_chord: 'corde',
  com_binaire: 'présences-absences', com_binaire_aide: 'La distance sur les présences-absences : Bray-Curtis devient l’indice de Sørensen, Jaccard le Jaccard binaire.',
  com_tr_aucune: 'aucune', com_tr_racine2: 'racine carrée', com_tr_racine4: 'racine quatrième', com_tr_log1p: 'log(x + 1)', com_tr_presence: 'présences-absences (0/1)',
  com_tr_relative: 'abondances relatives (÷ total de l’observation)', com_tr_wisconsin: 'Wisconsin (÷ maximum de l’espèce, puis ÷ total de l’observation)',
  com_tr_racine_wisconsin: 'racine carrée puis Wisconsin (comme metaMDS)',
  com_corr_aucune: 'aucune', com_corr_lingoes: 'Lingoes (√(d² + 2c))', com_corr_cailliez: 'Cailliez (d + c)',
  com_correction_appliquee: 'correction {c}, constante {k}', com_distance_mot: 'Distance : {d}',
  com_dimensions: 'Dimensions (k)', com_departs_champ: 'départs au hasard', com_tests: 'Termes testés',
  com_par_termes: 'chaque terme après ceux qui le précèdent (séquentiels)', com_par_marges: 'chaque terme après tous les autres (marginaux)', com_par_modele: 'le modèle entier',
  com_centre_dispersion: 'Centre de la dispersion', com_centroide: 'centroïde', com_mediane: 'médiane spatiale',
  com_correlation: 'Corrélation', com_mantel_pearson: 'Mantel (Pearson)', com_mantel_spearman: 'Mantel (rangs de Spearman)',
  com_classes: 'Classes du corrélogramme', com_classes_aide: '0 : règle de Sturges',
  com_reglages_note_pcoa: 'Les permutations (999 par défaut, 0 : aucune) testent les variables ajustées ; la graine les rend reproductibles. Lingoes et Cailliez ajoutent une constante à la distance pour qu’aucune valeur propre ne soit négative.',
  com_reglages_note_nmds: 'Le premier départ est la configuration de la PCoA, les suivants sont tirés au hasard (graine fixe) ; les permutations testent les variables ajustées. Avec plusieurs centaines d’observations, chaque départ prend quelques secondes.',
  com_reglages_note_permanova: 'Les permutations (999 par défaut) testent le pseudo-F de chaque terme, et la dispersion ; à l’intérieur des groupes choisis s’il y en a (un site suivi plusieurs années).',
  com_reglages_note_mantel: 'Les permutations (999 par défaut) des observations de la distance Y donnent les valeurs p ; à l’intérieur des groupes choisis s’il y en a.',
  com_y_requis: 'Ces analyses demandent un tableau Y : les espèces, ou les variables dont on calcule la distance.',
  com_x_requis_permanova: 'Une PERMANOVA demande des variables X : les facteurs ou gradients testés.',
  com_x_requis_mantel: 'Un test de Mantel demande des variables X : leur distance est comparée à celle de Y.',
  com_croises: 'Les variables ajustées sur une ordination sont des variables : retirez les termes croisés.',
  com_nmds_invalide: 'Le NMDS a de 1 à 4 dimensions et de 0 à 200 départs au hasard.', com_classes_invalides: 'Le nombre de classes est un entier de 0 à 50.',
  com_especes: 'Espèces (Y)', com_fleches_axes: 'Les flèches des X n’existent que sur les axes où elles ont été ajustées : {axes}.',
  com_vp_note: 'La part de l’inertie totale (la somme de toutes les valeurs propres) portée par chaque axe.',
  com_vp_negatives: 'La distance n’est pas euclidienne : {n} valeurs propres sont négatives (en rouge ; leur somme vaut {part} % de celle des positives), et les axes positifs portent plus de 100 % de l’inertie. Lingoes ou Cailliez (Modifier le modèle) les suppriment.',
  com_vp_euclidienne: 'Aucune valeur propre négative : la distance est euclidienne.', com_negative: 'Négative', com_negatives: 'Valeurs propres négatives',
  com_shepard_note: 'Chaque paire d’observations : sa dissimilarité et sa distance dans l’ordination ; la ligne, leur régression monotone, dont les écarts font le stress de Kruskal ({stress} ; R² non métrique = 1 − stress² = {r2}). Un stress sous 0,1 est bon, au-delà de 0,2 l’ordination se lit avec prudence.',
  com_shepard_extrait: 'Le graphique montre {n} des {total} paires, prises régulièrement dans l’ordre des dissimilarités.',
  com_paires: 'Paires', com_paires_mot: 'paires', com_monotone: 'Régression monotone', com_dissimilarite: 'Dissimilarité', com_distance_ordination: 'Distance dans l’ordination',
  com_retrouvee: 'La meilleure solution ({meilleur}) a été retrouvée {n} fois (Procrustes : erreur quadratique < 0,005, plus grand résidu < 0,01).',
  com_pas_retrouvee: 'La meilleure solution ({meilleur}) n’a été retrouvée par aucun autre départ : ce peut être un minimum local (augmentez les départs).',
  com_meme_stress: 'Aucun autre départ n’a retrouvé la configuration de la meilleure solution ({meilleur}) à l’analyse procrustéenne (erreur quadratique < 0,005, plus grand résidu < 0,01), mais {n} ont atteint son stress à 0,0001 près, quelques observations placées un peu autrement : le stress est atteint, la place de ces observations est incertaine.',
  com_meme_stress_mot: 'Même stress',
  com_sans_depart: 'Un seul ajustement, depuis la configuration de la PCoA.', com_depart: 'Départ', com_depart_pcoa: 'PCoA', com_depart_hasard: 'hasard',
  com_departs: 'Départs', com_retrouvee_mot: 'Retrouvée', com_nuls: '{n} dissimilarités nulles ont été remplacées par la moitié de la plus petite non nulle (isoMDS les refuse).',
  com_sans_x: 'Aucune variable X : ajoutez-en (Modifier le modèle) pour les ajuster sur l’ordination.',
  com_ajustement_note: 'Ajustées sur {axes} comme vegan::envfit, sur {n} observations ; {perm}. Une variable quantitative : sa direction (cosinus avec les axes) et son R² ; une qualitative : les centroïdes de ses catégories et son R².',
  com_ajustement_exclues: 'Observations sans valeur X, laissées de côté : {n}.', com_vecteurs: 'Variables quantitatives', com_facteurs: 'Variables qualitatives',
  com_p_rouge: 'p ≤ 0,05 en rouge.', com_ss: 'Somme des carrés', com_cm: 'Carré moyen',
  com_permanova_note: 'p ≤ 0,05 en rouge. Le pseudo-F d’un terme rapporte sa somme des carrés (des distances, autour des centroïdes) par degré de liberté à celle des résidus ; p compte les permutations qui font au moins aussi bien ({perm}), comme vegan::adonis2.',
  com_dispersion_alerte: 'La dispersion diffère entre les groupes de {vars} : la PERMANOVA peut alors refléter la dispersion autant que la position des groupes, et se lit avec prudence (Anderson 2006).',
  com_sans_dispersion: 'Aucun groupement : la dispersion se compare entre les catégories d’une variable X qualitative, ou de la variable de classe.',
  com_groupement: 'Groupement', com_boites: 'Boîtes', com_plan_pcoa: 'Plan de la PCoA', com_distance_centre: 'Distance au {c}', com_mediane_mot: 'Médiane',
  com_dispersion_note: 'La distance de chaque observation au {c} de son groupe dans l’espace de la PCoA, les axes de valeur propre négative retranchés (vegan::betadisper) ; l’ANOVA de ces distances, ses résidus permutés (vegan::permutest.betadisper), et les paires de Tukey.',
  com_dispersion_negatives: 'Distances au carré négatives, ramenées à 0 : {n}.',
  com_p_permutation: 'p (permutations)', com_tukey: 'Paires de groupes (Tukey)', com_paire: 'Paire', com_difference: 'Différence',
  com_ic_bas: 'IC 95 % bas', com_ic_haut: 'IC 95 % haut', com_p_ajustee: 'p ajustée',
  com_dispersion_resume: 'Dispersion de « {v} » : F = {f}, p = {p} (permutations).',
  com_r_partiel: 'r partiel', com_z: 'Covariables Z',
  com_mantel_note: 'La distance X : euclidienne sur {termes} ({echelles}). Chaque point : une paire d’observations. Le test permute les observations de la distance Y (vegan::mantel) ; unilatéral.',
  com_paires_extrait: 'Le graphique montre {n} des {total} paires, tirées au hasard.', com_distance_x_axe: 'Distance X (euclidienne)', com_distance_x_km: 'Distance géographique (km)', com_distance_y: 'Distance Y ({d})',
  com_correlogramme_note: 'La distance X découpée en {n} classes d’égale largeur ; dans chacune, la corrélation de Mantel entre les paires de la classe et la distance Y, de signe changé (positive : des observations proches plus semblables). Carré plein : p ≤ 0,05 après la correction progressive de Holm ; au-delà de la moitié, une classe n’est testée que si chaque observation y a une paire (vegan::mantel.correlog).',
  com_sturges: '(règle de Sturges)', com_sans_correlogramme: 'Pas de corrélogramme : les distances X ne varient pas.', com_r_mantel: 'r de Mantel',
  com_classe_distance_x: 'Distance X (centre de la classe)', com_classe_distance_km: 'Distance géographique en km (centre de la classe)', com_classe: 'Classe', com_centre_classe: 'Centre', com_centre_classe_km: 'Centre (km)', com_p_holm: 'p (Holm)', com_corrige_mot: 'corrigée', com_teste: 'Testée',
  com_correlogramme_resume: 'Corrélogramme : {n} classes significatives sur {classes}.',
  com_variables_y: 'Variables Y', com_inertie_totale: 'Inertie totale', com_permutations_texte: 'Tests par permutation : {perm}.',
  com_resume_pcoa: 'La PCoA représente au mieux, sur peu d’axes, les distances entre observations ; les espèces sont à la moyenne pondérée de leurs observations.',
  com_resume_nmds: 'Le NMDS respecte l’ordre des dissimilarités, pas leur valeur : seules les positions relatives se lisent, et le stress dit à quel point l’ordre est respecté.',
  com_resume_permanova: 'Termes : {par}. Une différence de position des groupes (PERMANOVA) se lit avec leur dispersion (Dispersion multivariée).',
  com_resume_mantel: 'r mesure à quel point des observations éloignées en X le sont aussi en Y ; le corrélogramme dit à quelles distances X.',
}, {
  com_distance_x: 'X distance', com_distance_x_variables: 'Of the X variables (Euclidean)', com_distance_x_geo: 'Geographic ({x}, {y})',
  com_distance_x_sans: 'Geographic: choose the coordinates first (Plots › Map)',
  com_distance_x_aide: 'What the distance of the table Y is compared with: that of the X variables, or the geographic distance between the observations (in km), to see whether nearby samples are more alike.',
  type_pcoa: 'PCoA (principal coordinates analysis)', type_nmds: 'NMDS (non-metric multidimensional scaling)',
  type_permanova: 'PERMANOVA (permutational analysis of variance)', type_mantel: 'Mantel test',
  ordination_dist: 'Ordination', valeurs_propres_pcoa: 'Eigenvalues', shepard: 'Shepard diagram', ajustement_env: 'Fitted variables',
  permanova: 'PERMANOVA table', dispersion: 'Multivariate dispersion', mantel: 'Mantel test', correlogramme: 'Mantel correlogram',
  com_note_pcoa: 'Y: the table of species (or of the variables whose distance is computed); X, if any: variables fitted onto the first two axes (an arrow per quantitative variable, a centroid per category of a qualitative one; as vegan::envfit). An observation with a missing Y value is left out.',
  com_note_nmds: 'Y: the table of species; X, if any: variables fitted onto the dimensions of the NMDS. MASS::isoMDS from the PCoA configuration then random starts (fixed seed), as vegan::metaMDS: the lowest-stress solution is kept.',
  com_note_permanova: 'Y: the table of species; X: the factors or gradients tested (qualitative or quantitative, cross terms included, in their order); Class: one more grouping for the dispersion. The dispersion of each qualitative variable is compared too (as vegan::betadisper).',
  com_note_mantel: 'Y: the table of species; X: the variables whose Euclidean distance (scaled as the Variables tab says, UV by default) is compared with that of Y; Z: covariates of a partial test.',
  com_distance: 'Distance', com_transformation: 'Transformation before the distance', com_correction: 'Correction of negative eigenvalues',
  com_d_bray: 'Bray–Curtis', com_d_jaccard: 'Jaccard (2B / (1 + B))', com_d_gower: 'Gower', com_d_euclidean: 'Euclidean', com_d_manhattan: 'Manhattan',
  com_d_canberra: 'Canberra', com_d_kulczynski: 'Kulczynski', com_d_hellinger: 'Hellinger', com_d_chord: 'chord',
  com_binaire: 'presence–absence', com_binaire_aide: 'The distance on presence–absence: Bray–Curtis becomes the Sørensen index, Jaccard the binary Jaccard.',
  com_tr_aucune: 'none', com_tr_racine2: 'square root', com_tr_racine4: 'fourth root', com_tr_log1p: 'log(x + 1)', com_tr_presence: 'presence–absence (0/1)',
  com_tr_relative: 'relative abundances (÷ the observation’s total)', com_tr_wisconsin: 'Wisconsin (÷ the species’ maximum, then ÷ the observation’s total)',
  com_tr_racine_wisconsin: 'square root then Wisconsin (as metaMDS)',
  com_corr_aucune: 'none', com_corr_lingoes: 'Lingoes (√(d² + 2c))', com_corr_cailliez: 'Cailliez (d + c)',
  com_correction_appliquee: '{c} correction, constant {k}', com_distance_mot: 'Distance: {d}',
  com_dimensions: 'Dimensions (k)', com_departs_champ: 'random starts', com_tests: 'Terms tested',
  com_par_termes: 'each term after those before it (sequential)', com_par_marges: 'each term after all the others (marginal)', com_par_modele: 'the whole model',
  com_centre_dispersion: 'Centre of the dispersion', com_centroide: 'centroid', com_mediane: 'spatial median',
  com_correlation: 'Correlation', com_mantel_pearson: 'Mantel (Pearson)', com_mantel_spearman: 'Mantel (Spearman ranks)',
  com_classes: 'Classes of the correlogram', com_classes_aide: '0: Sturges’ rule',
  com_reglages_note_pcoa: 'The permutations (999 by default, 0: none) test the fitted variables; the seed makes them reproducible. Lingoes and Cailliez add a constant to the distance so that no eigenvalue is negative.',
  com_reglages_note_nmds: 'The first start is the PCoA configuration, the next ones are drawn at random (fixed seed); the permutations test the fitted variables. With several hundred observations, each start takes a few seconds.',
  com_reglages_note_permanova: 'The permutations (999 by default) test the pseudo-F of each term, and the dispersion; within the groups chosen if any (a site followed over several years).',
  com_reglages_note_mantel: 'The permutations (999 by default) of the observations of the Y distance give the p-values; within the groups chosen if any.',
  com_y_requis: 'These analyses need a table Y: the species, or the variables whose distance is computed.',
  com_x_requis_permanova: 'A PERMANOVA needs X variables: the factors or gradients tested.',
  com_x_requis_mantel: 'A Mantel test needs X variables: their distance is compared with that of Y.',
  com_croises: 'The variables fitted onto an ordination are variables: remove the cross terms.',
  com_nmds_invalide: 'The NMDS has 1 to 4 dimensions and 0 to 200 random starts.', com_classes_invalides: 'The number of classes is a whole number from 0 to 50.',
  com_especes: 'Species (Y)', com_fleches_axes: 'The arrows of the X exist only on the axes they were fitted on: {axes}.',
  com_vp_note: 'The share of the total inertia (the sum of all the eigenvalues) carried by each axis.',
  com_vp_negatives: 'The distance is not Euclidean: {n} eigenvalues are negative (in red; their sum is {part}% of that of the positive ones), and the positive axes carry more than 100% of the inertia. Lingoes or Cailliez (Edit model) remove them.',
  com_vp_euclidienne: 'No negative eigenvalue: the distance is Euclidean.', com_negative: 'Negative', com_negatives: 'Negative eigenvalues',
  com_shepard_note: 'Each pair of observations: its dissimilarity and its distance in the ordination; the line, their monotone regression, whose deviations make Kruskal’s stress ({stress}; non-metric R² = 1 − stress² = {r2}). A stress below 0.1 is good; beyond 0.2 the ordination is to be read with care.',
  com_shepard_extrait: 'The plot shows {n} of the {total} pairs, taken regularly in the order of the dissimilarities.',
  com_paires: 'Pairs', com_paires_mot: 'pairs', com_monotone: 'Monotone regression', com_dissimilarite: 'Dissimilarity', com_distance_ordination: 'Distance in the ordination',
  com_retrouvee: 'The best solution ({meilleur}) was found again {n} times (Procrustes: root mean square error < 0.005, largest residual < 0.01).',
  com_pas_retrouvee: 'The best solution ({meilleur}) was not found again by any other start: it may be a local minimum (raise the starts).',
  com_meme_stress: 'No other start found the configuration of the best solution ({meilleur}) again by Procrustes (root mean square error < 0.005, largest residual < 0.01), but {n} reached its stress to 0.0001, with some observations placed slightly differently: the stress is reached, the place of those observations is uncertain.',
  com_meme_stress_mot: 'Same stress',
  com_sans_depart: 'A single fit, from the PCoA configuration.', com_depart: 'Start', com_depart_pcoa: 'PCoA', com_depart_hasard: 'random',
  com_departs: 'Starts', com_retrouvee_mot: 'Found again', com_nuls: '{n} zero dissimilarities were replaced by half the smallest non-zero one (isoMDS refuses them).',
  com_sans_x: 'No X variable: add some (Edit model) to fit them onto the ordination.',
  com_ajustement_note: 'Fitted onto {axes} as vegan::envfit, on {n} observations; {perm}. A quantitative variable: its direction (cosines with the axes) and its R²; a qualitative one: the centroids of its categories and its R².',
  com_ajustement_exclues: 'Observations without an X value, left out: {n}.', com_vecteurs: 'Quantitative variables', com_facteurs: 'Qualitative variables',
  com_p_rouge: 'p ≤ 0.05 in red.', com_ss: 'Sum of squares', com_cm: 'Mean square',
  com_permanova_note: 'p ≤ 0.05 in red. The pseudo-F of a term is its sum of squares (of the distances, around the centroids) per degree of freedom over that of the residuals; p counts the permutations that do at least as well ({perm}), as vegan::adonis2.',
  com_dispersion_alerte: 'The dispersion differs between the groups of {vars}: the PERMANOVA may then reflect the dispersion as much as the position of the groups, and is to be read with care (Anderson 2006).',
  com_sans_dispersion: 'No grouping: the dispersion is compared between the categories of a qualitative X variable, or of the class variable.',
  com_groupement: 'Grouping', com_boites: 'Box plots', com_plan_pcoa: 'PCoA plane', com_distance_centre: 'Distance to the {c}', com_mediane_mot: 'Median',
  com_dispersion_note: 'The distance of each observation to the {c} of its group in the PCoA space, the axes of negative eigenvalue subtracted (vegan::betadisper); the ANOVA of these distances, its residuals permuted (vegan::permutest.betadisper), and Tukey’s pairs.',
  com_dispersion_negatives: 'Negative squared distances, set to 0: {n}.',
  com_p_permutation: 'p (permutations)', com_tukey: 'Pairs of groups (Tukey)', com_paire: 'Pair', com_difference: 'Difference',
  com_ic_bas: '95% CI low', com_ic_haut: '95% CI high', com_p_ajustee: 'adjusted p',
  com_dispersion_resume: 'Dispersion of “{v}”: F = {f}, p = {p} (permutations).',
  com_r_partiel: 'partial r', com_z: 'Covariates Z',
  com_mantel_note: 'The X distance: Euclidean on {termes} ({echelles}). Each point: a pair of observations. The test permutes the observations of the Y distance (vegan::mantel); one-sided.',
  com_paires_extrait: 'The plot shows {n} of the {total} pairs, drawn at random.', com_distance_x_axe: 'X distance (Euclidean)', com_distance_x_km: 'Geographic distance (km)', com_distance_y: 'Y distance ({d})',
  com_correlogramme_note: 'The X distance cut into {n} classes of equal width; in each, the Mantel correlation between the pairs of the class and the Y distance, its sign changed (positive: close observations more alike). Filled square: p ≤ 0.05 after Holm’s progressive correction; beyond half, a class is tested only if every observation has a pair in it (vegan::mantel.correlog).',
  com_sturges: '(Sturges’ rule)', com_sans_correlogramme: 'No correlogram: the X distances don’t vary.', com_r_mantel: 'Mantel r',
  com_classe_distance_x: 'X distance (centre of the class)', com_classe_distance_km: 'Geographic distance in km (centre of the class)', com_classe: 'Class', com_centre_classe: 'Centre', com_centre_classe_km: 'Centre (km)', com_p_holm: 'p (Holm)', com_corrige_mot: 'corrected', com_teste: 'Tested',
  com_correlogramme_resume: 'Correlogram: {n} significant classes out of {classes}.',
  com_variables_y: 'Y variables', com_inertie_totale: 'Total inertia', com_permutations_texte: 'Permutation tests: {perm}.',
  com_resume_pcoa: 'The PCoA shows as well as it can, on few axes, the distances between observations; the species sit at the weighted average of their observations.',
  com_resume_nmds: 'The NMDS keeps the order of the dissimilarities, not their values: only relative positions are read, and the stress says how well the order is kept.',
  com_resume_permanova: 'Terms: {par}. A difference in the position of the groups (PERMANOVA) is read with their dispersion (Multivariate dispersion).',
  com_resume_mantel: 'r measures how much observations far apart in X are also far apart in Y; the correlogram says at which X distances.',
});
