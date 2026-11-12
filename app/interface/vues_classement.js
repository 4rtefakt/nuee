// Decision trees (TREE), linear discriminant analysis (LDA) and k-means (KMEANS)
// (moteur/R/arbres.R, discrimination.R, kmoyennes.R): their settings in the model dialog, their
// views (the tree drawn, its pruning, leaf rules, variable importance and first-split stability;
// discriminant scores and coefficients, posterior probabilities; clusters on the first two
// components, silhouettes, the choice of k, centres), their summaries, report parts and strings.
// Imported by app.js.
import { t, ajouterTextes } from './i18n.js';
import { el, fmt } from './ui.js';
import { demander } from './api.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, controleCouleur, controleEtiquettes, pointsObservations, tableau,
  notesModele, ficheTest } from './panneaux.js';
import { ANALYSES } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const TYPES = ['TREE', 'LDA', 'KMEANS'];
const estClassement = (m) => !!m && !!m.analyse && TYPES.includes(m.type);
const chiffre = (k, v, d = 3) => el('span', {}, el('small', {}, k), ' ', el('b', {}, typeof v === 'number' ? fmt(v, d) : v ?? '—'));
const pct = (v, d = 1) => (v === null || v === undefined ? '—' : `${fmt(100 * v, d)} %`);
// the rows of a data.frame sent by R (columns), as arrays of values
const lignesDe = (df, cles) => { const n = liste((df || {})[cles[0]]).length; return Array.from({ length: n }, (_, i) => cles.map((k) => liste(df[k])[i])); };
// a small fixed table: headers, rows of cells
function tableCompacte(entetes, lignes) {
  return el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, entetes.map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
    el('tbody', {}, lignes.map((l) => el('tr', {}, l.map((v, j) => el('td', { class: j ? 'num' : '' }, v))))));
}
const titreFiche = (m) => el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, m.type), m.titre ? el('span', { class: 'doux' }, m.titre) : null);
const groupesVC = (m) => (m.vc_par ? `${m.G} (${m.vc_par})` : m.G);
// the observations a click picked, as the model's selection
const choisir = (ctx, m, uids) => ctx.choisir(m.id, uids);

// ------------------------------------------------------------ trees

// The tree's layout: its leaves side by side in their order, each node above the middle of its
// two children (rpart numbers them 2n and 2n + 1), one level per depth.
function dispositionArbre(a) {
  const ids = liste(a.id);
  const feuille = liste(a.feuille);
  const index = new Map(ids.map((id, i) => [id, i]));
  const x = new Array(ids.length).fill(0);
  let suivant = 0;
  const placer = (id) => {
    const i = index.get(id);
    if (i === undefined) return suivant;
    if (feuille[i]) { x[i] = suivant; suivant += 1; return x[i]; }
    x[i] = (placer(2 * id) + placer(2 * id + 1)) / 2;
    return x[i];
  };
  placer(1);
  return { ids, x, prof: ids.map((id) => Math.floor(Math.log2(id) + 1e-9)), index, largeur: Math.max(1, suivant) };
}
// is node `id` on the path to node `bas` (rpart: the parent of n is n / 2, rounded down)?
const dansSousArbre = (id, bas) => { let n = bas; while (n > id) n = Math.floor(n / 2); return n === id; };
// the node (rpart number) of each observation's leaf
const noeudsObservations = (m) => { const noeuds = liste(m.regles.noeud); return liste(m.observations.feuille).map((f) => noeuds[f - 1]); };
const valeurNoeud = (m, i) => {
  const a = m.arbre;
  return m.classement ? `${liste(a.valeur)[i]} (${fmt(100 * liste(a.part)[i], 0)} %)` : fmt(liste(a.valeur)[i], 3);
};

function rendreArbre(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const a = m.arbre;
  const d = dispositionArbre(a);
  const c = couleursTheme();
  const graphe = el('div', { class: 'graphe' });
  const q = m.qualite;
  conteneur.replaceChildren(outils(el('span', { class: 'infos' }, [
    `${liste(m.regles.feuille).length} ${t('arb_feuilles')}`,
    m.premiere && m.premiere.regle ? ` · ${t('arb_premiere')} : ${m.premiere.regle}` : '',
    m.classement ? ` · ${t('bien_classes_vc')} ${pct(q.bien_classes_vc)}` : ` · R2 ${fmt(q.r2, 2)} · Q2 ${fmt(q.Q2, 2)}`].join('')),
  el('span', { class: 'infos' }, t('arb_clic_noeud'))), graphe);
  const ids = d.ids;
  const feuille = liste(a.feuille);
  const parents = ids.map((id) => (id > 1 ? d.index.get(Math.floor(id / 2)) : undefined));
  const aretes = { x: [], y: [] };
  ids.forEach((id, i) => { if (parents[i] !== undefined) { aretes.x.push(d.x[parents[i]], d.x[i], null); aretes.y.push(-d.prof[parents[i]], -d.prof[i], null); } });
  const regles = new Map(lignesDe(m.regles, ['noeud', 'regle']).map(([n, r]) => [n, r]));
  const classes = liste(m.classes);
  const valeurs = liste(a.valeur);
  // the nodes (a click picks their observations): the internal ones hollow, the leaves coloured by
  // their class, or by their mean on a colour scale
  const survol = (id, i) => `${feuille[i] ? t('arb_feuille') : liste(a.variable)[i]} · n = ${liste(a.n)[i]}<br>${valeurNoeud(m, i)}`
    + (feuille[i] && regles.get(id) ? `<br>${regles.get(id).replace(/ & /g, '<br>')}` : '');
  const noeuds = (garder, marker) => {
    const idx = ids.map((_, i) => i).filter(garder);
    return { type: 'scatter', mode: 'markers', name: t('arb_noeuds'), x: idx.map((i) => d.x[i]), y: idx.map((i) => -d.prof[i]), customdata: idx.map((i) => ids[i]),
      text: idx.map((i) => survol(ids[i], i)), hovertemplate: '%{text}<extra></extra>', marker: marker(idx) };
  };
  const internes = noeuds((i) => !feuille[i], () => ({ size: 16, symbol: 'square', color: c.fond, line: { width: 1.5, color: c.doux } }));
  const feuilles = noeuds((i) => feuille[i], (idx) => (m.classement
    ? { size: 16, symbol: 'square', color: idx.map((i) => PALETTE[Math.max(0, classes.indexOf(valeurs[i])) % PALETTE.length]), line: { width: 1, color: c.texte } }
    : { size: 16, symbol: 'square', color: idx.map((i) => valeurs[i]), colorscale: 'Viridis', showscale: true,
      colorbar: { title: { text: m.noms_y[0] }, thickness: 12 }, line: { width: 1, color: c.texte } }));
  const annotations = [];
  ids.forEach((id, i) => {
    annotations.push({ x: d.x[i], y: -d.prof[i], yshift: -20, text: `n = ${liste(a.n)[i]}<br><b>${valeurNoeud(m, i)}</b>`, showarrow: false,
      font: { size: 10, color: c.texte }, yanchor: 'top' });
    if (parents[i] !== undefined) {
      const gauche = id % 2 === 0;
      annotations.push({ x: (d.x[parents[i]] + d.x[i]) / 2, y: -(d.prof[parents[i]] + d.prof[i]) / 2, text: liste(a.branche)[i], showarrow: false,
        xanchor: gauche ? 'right' : 'left', xshift: gauche ? -4 : 4, font: { size: 10, color: c.doux } });
    }
  });
  const prof = Math.max(...d.prof);
  tracer(ctx, graphe, [{ type: 'scatter', mode: 'lines', x: aretes.x, y: aretes.y, line: { color: c.doux, width: 1.2 }, hoverinfo: 'skip', showlegend: false },
    internes, feuilles], {
    title: { text: `${m.nom} · ${t('arbre')}` }, dragmode: 'pan', showlegend: false, annotations,
    margin: { l: 20, r: 20, t: 40, b: 20 },
    xaxis: { visible: false, range: [-0.8, d.largeur - 0.2] }, yaxis: { visible: false, range: [-prof - 0.9, 0.5] } });
  // a click on a node: its observations become the selection
  graphe.on('plotly_click', (ev) => {
    const pt = (ev.points || []).find((x) => x.customdata !== undefined && x.data.name === t('arb_noeuds'));
    if (!pt) return;
    const n = noeudsObservations(m);
    choisir(ctx, m, liste(m.observations.uid).filter((_, i) => dansSousArbre(pt.customdata, n[i])));
  });
}

function rendreElagage(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const e = m.elagage;
  const tab = e.table;
  const feuilles = liste(tab.feuilles);
  const retenu = feuilles.indexOf(e.feuilles);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t(e.regle === 'min' ? 'arb_elagage_note_min' : 'arb_elagage_note',
    { seuil: fmt(e.seuil, 3), minimum: fmt(e.minimum, 3), feuilles: e.feuilles })), graphe, bas);
  const c = couleursTheme();
  tracer(ctx, graphe, [
    { type: 'scatter', mode: 'lines+markers', name: t('arb_erreur_vc'), x: feuilles, y: liste(tab.xerror),
      error_y: { type: 'data', array: liste(tab.xstd), visible: true, color: PALETTE[0], thickness: 1 }, line: { color: PALETTE[0] }, marker: { size: 6 },
      hovertemplate: `%{x} ${t('arb_feuilles')}<br>xerror %{y:.3f}<extra></extra>` },
    { type: 'scatter', mode: 'lines', name: t('arb_erreur_relative'), x: feuilles, y: liste(tab.erreur_relative), line: { color: c.doux, dash: 'dot' },
      hovertemplate: `%{x} ${t('arb_feuilles')}<br>%{y:.3f}<extra></extra>` },
    retenu >= 0 ? { type: 'scatter', mode: 'markers', name: t('arb_retenu'), x: [feuilles[retenu]], y: [liste(tab.xerror)[retenu]],
      marker: { size: 14, color: 'rgba(0,0,0,0)', line: { width: 2.5, color: PALETTE[1] } }, hoverinfo: 'skip' } : null].filter(Boolean),
  { title: { text: `${m.nom} · ${t('elagage')}` }, dragmode: false, showlegend: true,
    xaxis: { title: { text: t('arb_nb_feuilles') }, type: feuilles[feuilles.length - 1] > 30 ? 'log' : 'linear' }, yaxis: { title: { text: t('arb_erreur_rel_axe') } },
    shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: e.regle === 'min' ? e.minimum : e.seuil, y1: e.regle === 'min' ? e.minimum : e.seuil,
      line: { color: PALETTE[1], width: 1, dash: 'dash' } }] });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: feuilles.length, nom: 'Table', choisie: (i) => i === retenu,
    colonnes: [{ cle: 'cp', titre: 'cp', decimales: 5 }, { cle: 'coupures', titre: t('arb_coupures'), decimales: 0 }, { cle: 'feuilles', titre: t('arb_feuilles_titre'), decimales: 0 },
      { cle: 'erreur_relative', titre: t('arb_erreur_relative') }, { cle: 'xerror', titre: 'xerror' }, { cle: 'xstd', titre: 'xstd' }],
    valeur: (i, k) => liste(tab[k])[i],
  });
}

function rendreRegles(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const r = m.regles;
  const n = liste(r.feuille).length;
  const feuilles = liste(m.observations.feuille);
  const uids = liste(m.observations.uid);
  const sel = new Set(ctx.selection(m.id));
  tableau(conteneur, ctx, loc, {
    n, nom: 'Table',
    avant: [el('span', { class: 'infos' }, t('arb_regles_note'))],
    cliquer: (i) => choisir(ctx, m, uids.filter((_, j) => feuilles[j] === liste(r.feuille)[i])),
    choisie: (i) => uids.some((u, j) => feuilles[j] === liste(r.feuille)[i] && sel.has(u)),
    colonnes: [{ cle: 'feuille', titre: t('arb_feuille'), decimales: 0 }, { cle: 'n', titre: 'n', decimales: 0 },
      m.classement ? { cle: 'valeur', titre: t('classe') } : { cle: 'valeur', titre: `${t('arb_moyenne')} ${m.noms_y[0]}` },
      m.classement ? { cle: 'part', titre: t('arb_part'), format: (v) => pct(v, 0) } : null,
      { cle: 'regle', titre: t('arb_regle') }].filter(Boolean),
    valeur: (i, k) => liste(r[k])[i],
  });
}

function rendreImportance(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const imp = m.importance;
  const vars = liste(imp.variable);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('arb_importance_note')), graphe, bas);
  if (!vars.length) { graphe.replaceChildren(el('p', { class: 'infos' }, t('arb_sans_coupure'))); return; }
  const blocs = liste(imp.bloc);
  const noms = [...new Set(blocs.map((b) => b || '—'))];
  tracer(ctx, graphe, noms.map((b, k) => {
    const idx = vars.map((_, i) => i).filter((i) => (blocs[i] || '—') === b);
    return { type: 'bar', orientation: 'h', name: b === '—' ? t('aucun') : b, y: idx.map((i) => vars[i]), x: idx.map((i) => liste(imp.pourcentage)[i]),
      marker: { color: PALETTE[k % PALETTE.length] }, hovertemplate: '<b>%{y}</b><br>%{x:.1f} %<extra></extra>' };
  }), { title: { text: `${m.nom} · ${t('importance_arbre')}` }, dragmode: false, showlegend: noms.length > 1, barmode: 'stack',
    yaxis: { categoryorder: 'array', categoryarray: [...vars].reverse(), automargin: true }, xaxis: { title: { text: '%' } } });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: vars.length, nom: 'Table',
    colonnes: [{ cle: 'variable', titre: t('variable_arbre') }, { cle: 'bloc', titre: t('bloc') }, { cle: 'importance', titre: t('importance_arbre') },
      { cle: 'pourcentage', titre: '%', decimales: 1 }],
    valeur: (i, k) => liste(imp[k])[i],
  });
}

async function rendreStabilite(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const n = m.reglages_arbre.tirages;
  if (!loc.res || loc.id !== m.id) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('arb_calcul_stabilite', { n })));
    loc.res = await ctx.demander('arbre_stabilite', { modele: m.id });
    loc.id = m.id;
  }
  const s = loc.res.stabilite;
  const vars = liste(s.variable);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('arb_stabilite_note', { n })
    + (loc.res.echecs ? ` ${t('arb_stabilite_echecs', { n: loc.res.echecs })}` : '')), graphe, bas);
  tracer(ctx, graphe, [{ type: 'bar', x: vars, y: liste(s.frequence), marker: { color: PALETTE[0] },
    text: liste(s.seuil_median).map((v) => (v === null ? '' : `${t('arb_seuil_median')} ${+v.toPrecision(4)}`)), textposition: 'outside', cliponaxis: false,
    hovertemplate: '<b>%{x}</b><br>%{y:.1f} %<br>%{text}<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('stabilite_arbre')}` }, dragmode: false, yaxis: { title: { text: t('arb_frequence') }, range: [0, 115] } });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: vars.length, nom: 'Table',
    colonnes: [{ cle: 'variable', titre: t('arb_premiere') }, { cle: 'tirages', titre: t('arb_tirages'), decimales: 0 },
      { cle: 'frequence', titre: t('arb_frequence'), decimales: 1 }, { cle: 'seuil_median', titre: t('arb_seuil_median') }],
    valeur: (i, k) => liste(s[k])[i],
  });
}

// A tree's settings in words: the minimum per leaf used (at most a sixth of the observations,
// moteur/R/arbres.R: minimum_feuille), and the one asked when it was lowered.
function noteReglagesArbre(m) {
  const r = m.reglages_arbre;
  const mb = m.minbucket_utilise ?? r.minbucket;
  return t('arb_reglages', { minbucket: mb, maxdepth: r.maxdepth, cp: r.cp, maxsurrogate: r.maxsurrogate,
    regle: t(r.elagage === 'min' ? 'arb_regle_min' : 'arb_regle_1se'), manquantes: m.manquantes_x })
    + (mb < r.minbucket ? ` ${t('arb_minbucket_ramene', { demande: r.minbucket, minbucket: mb, n: m.N })}` : '');
}

function rendreResumeArbre(conteneur, ctx, m) {
  const q = m.qualite;
  const r = m.reglages_arbre;
  const fiche = el('div', { class: 'fiche' }, titreFiche(m),
    el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('variables'), m.K, 0), chiffre(t('groupes_vc'), groupesVC(m)),
      chiffre(t('arb_feuilles_titre'), liste(m.regles.feuille).length, 0), chiffre('cp', m.elagage.cp, 4),
      m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null),
    el('div', { class: 'fiche-chiffres' }, m.classement
      ? [chiffre(t('bien_classes'), pct(q.bien_classes)), chiffre(t('bien_classes_vc'), pct(q.bien_classes_vc))]
      : [chiffre('R2', q.r2), chiffre('Q2', q.Q2), chiffre('RMSEE', q.RMSEE, 4), chiffre('RMSEcv', q.RMSEcv, 4)],
    m.premiere && m.premiere.regle ? chiffre(t('arb_premiere'), m.premiere.regle) : null),
    ficheTest(m));
  const notes = notesModele(conteneur, ctx, m);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ,
    el('p', { class: 'note' }, t(m.classement ? 'arb_resume_classement' : 'arb_resume_regression', { y: m.noms_y[0] })),
    el('p', { class: 'note' }, noteReglagesArbre(m)),
    el('p', { class: 'note' }, t('arb_vc_note'))));
  notes.retrouver();
}

// ------------------------------------------------------------ LDA

const libelleLD = (m, k) => `${liste(m.axes)[k]} (${fmt(100 * liste(m.part)[k], 1)} %)`;

function rendreScoresLDA(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const axes = liste(m.axes);
  loc.a ??= 1;
  loc.b ??= Math.min(2, axes.length);
  loc.couleur ??= 'classe';
  const a = Math.min(loc.a, axes.length) - 1;
  const b = Math.min(loc.b, axes.length) - 1;
  const o = m.observations;
  const options = axes.map((_, i) => [i + 1, libelleLD(m, i)]);
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    axes.length > 1 ? choix(t('axe_x'), options, loc.a, (v) => { loc.a = +v; ctx.rafraichir(); }) : null,
    axes.length > 1 ? choix(t('axe_y'), options, loc.b, (v) => { loc.b = +v; ctx.rafraichir(); }) : null,
    axes.length > 1 ? controleCouleur(ctx, m, loc) : null, axes.length > 1 ? controleEtiquettes(ctx, m, loc) : null,
    el('span', { class: 'infos' }, `${t('bien_classes_vc')} ${pct(m.classification.vc.global)}`)), graphe);
  const c = couleursTheme();
  if (axes.length === 1) {  // two classes: one discriminant function, its scores by class
    const classes = liste(m.classes);
    const cl = liste(o.classe);
    const ld = liste(o[axes[0]]);
    tracer(ctx, graphe, classes.map((k, j) => {
      const idx = cl.map((_, i) => i).filter((i) => cl[i] === k);
      return { type: 'box', orientation: 'h', name: k, x: idx.map((i) => ld[i]), text: idx.map((i) => liste(o.nom)[i]), customdata: idx.map((i) => liste(o.uid)[i]),
        boxpoints: 'all', jitter: 0.5, pointpos: 0, marker: { color: PALETTE[j % PALETTE.length], size: 5 }, line: { width: 1 },
        hovertemplate: '<b>%{text}</b><br>%{x:.3f}<extra>%{fullData.name}</extra>' };
    }), { title: { text: `${m.nom} · ${t('lda_scores')}` }, showlegend: false, dragmode: 'select', xaxis: { title: { text: libelleLD(m, 0) }, zeroline: true } });
    brancherSelection(graphe, ctx, m.id);
    return;
  }
  const traces = pointsObservations(ctx, m, loc, liste(o[axes[a]]), liste(o[axes[b]]));
  const ce = m.centroides;
  traces.push({ type: 'scatter', mode: 'markers+text', name: t('lda_centroides'), x: liste(ce[axes[a]]), y: liste(ce[axes[b]]), text: liste(ce.classe),
    textposition: 'top center', textfont: { size: 11, color: c.texte }, marker: { symbol: 'x-thin', size: 14, line: { width: 2.5, color: c.texte } },
    hovertemplate: '<b>%{text}</b><br>%{x:.3f}, %{y:.3f}<extra></extra>' });
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${t('lda_scores')}` },
    xaxis: { title: { text: libelleLD(m, a) }, zeroline: true }, yaxis: { title: { text: libelleLD(m, b) }, zeroline: true } });
  brancherSelection(graphe, ctx, m.id);
}

function rendreCoefficientsLDA(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const axes = liste(m.axes);
  loc.quoi ??= 'structure';
  loc.axe ??= 1;
  const k = Math.min(loc.axe, axes.length) - 1;
  const df = m[loc.quoi];
  const termes = liste(df.terme);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(outils(
    choix('', [['structure', t('lda_structure')], ['coefficients', t('lda_coefficients_bruts')], ['coefficients_std', t('lda_coefficients_std')]], loc.quoi,
      (v) => { loc.quoi = v; ctx.rafraichir(); }),
    axes.length > 1 ? choix(t('lda_fonction'), axes.map((_, i) => [i + 1, libelleLD(m, i)]), loc.axe, (v) => { loc.axe = +v; ctx.rafraichir(); }) : null),
  el('p', { class: 'note' }, t(`lda_${loc.quoi}_note`)), graphe, bas);
  const v = liste(df[axes[k]]);
  tracer(ctx, graphe, [{ type: 'bar', x: termes, y: v, marker: { color: v.map((x) => (x >= 0 ? PALETTE[0] : PALETTE[1])) },
    hovertemplate: '<b>%{x}</b><br>%{y:.4f}<extra></extra>' }],
  { title: { text: `${m.nom} · ${t({ structure: 'lda_structure', coefficients: 'lda_coefficients_bruts', coefficients_std: 'lda_coefficients_std' }[loc.quoi])} · ${axes[k]}` },
    dragmode: false, xaxis: { automargin: true }, yaxis: { zeroline: true, range: loc.quoi === 'structure' ? [-1.05, 1.05] : undefined } });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: termes.length, nom: 'Table',
    colonnes: [{ cle: 'terme', titre: t('terme') }].concat(axes.map((ax) => ({ cle: ax, titre: ax }))),
    valeur: (i, c) => liste(df[c])[i],
  });
}

function rendreProbabilitesLDA(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const o = m.observations;
  const proba = Object.keys(o).filter((k) => k.startsWith('probabilité '));
  const pred = o['classe ajustée'];
  const vc = o['classe prédite (VC)'];
  tableau(conteneur, ctx, loc, {
    n: liste(o.uid).length, idModele: m.id, uid: (i) => o.uid[i], nom: 'Table',
    avant: [el('span', { class: 'infos' }, t('lda_probabilites_note'))],
    colonnes: [{ cle: 'nom', titre: 'observation' }, { cle: 'classe', titre: t('observee') }, { cle: 'classe ajustée', titre: t('lda_ajustee'), alerte: (i) => pred[i] !== o.classe[i] },
      { cle: 'classe prédite (VC)', titre: t('lda_predite_vc'), alerte: (i) => vc[i] !== null && vc[i] !== o.classe[i] }]
      .concat(proba.map((k) => ({ cle: k, titre: `P(${k.slice('probabilité '.length)})`, decimales: 3 }))),
    valeur: (i, k) => liste(o[k])[i],
  });
}

function rendreResumeLDA(conteneur, ctx, m) {
  const q = m.qualite;
  const pr = m.priors;
  const fiche = el('div', { class: 'fiche' }, titreFiche(m),
    el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('lda_termes'), m.K, 0), chiffre(t('lda_classes'), liste(m.classes).length, 0),
      chiffre(t('groupes_vc'), groupesVC(m)), m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null),
    el('div', { class: 'fiche-chiffres' }, chiffre(t('bien_classes'), pct(q.bien_classes)), chiffre(t('bien_classes_vc'), pct(q.bien_classes_vc)),
      liste(m.axes).slice(0, 3).map((a, k) => chiffre(a, pct(liste(m.part)[k])))),
    ficheTest(m));
  const textes = [t('lda_resume_note', { priors: t(m.reglages_lda.priors === 'egales' ? 'lda_priors_egales' : 'lda_priors_proportionnelles') })];
  if (liste(m.termes_retires).length) textes.push(t('lda_retires', { termes: liste(m.termes_retires).join(', ') }));
  if (m.colineaires) textes.push(t('lda_colineaires'));
  if (m.echecs_vc) textes.push(t('lda_echecs_vc', { n: m.echecs_vc }));
  const notes = notesModele(conteneur, ctx, m);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ,
    tableCompacte([t('classe'), 'n', t('lda_prior')], lignesDe(pr, ['classe', 'n', 'prior']).map((l) => [l[0], String(l[1]), fmt(l[2], 3)])),
    textes.map((x) => el('p', { class: 'note' }, x))));
  notes.retrouver();
}

// ------------------------------------------------------------ k-means

const nomGroupe = (g) => `${t('groupe')} ${g}`;

// Saving the clusters as a qualitative variable of the project (undone by Edit › Undo).
function enregistrerGroupes(ctx, m) {
  const nom = el('input', { value: `${m.nom} ${t('km_groupes_mot')}`, style: 'width:12em', title: t('km_nom_variable') });
  const bouton = el('button', { title: t('km_enregistrer_aide'), onclick: async () => {
    try {
      await ctx.occupe(() => ctx.demander('kmeans_variable', { modele: m.id, nom: nom.value }));
      await ctx.apresConsole([m.projet]);
      ctx.statut(t('km_variable_ajoutee', { nom: nom.value }));
    } catch (e) { ctx.statut(e.message); }
  } }, t('km_enregistrer'));
  return el('span', { class: 'ligne' }, nom, bouton);
}

function rendrePlanKmeans(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.couleur ??= 'classe';
  const o = m.observations;
  const part = liste(m.acp.part);
  const deux = o.CP2 !== undefined;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(controleCouleur(ctx, m, loc), controleEtiquettes(ctx, m, loc), enregistrerGroupes(ctx, m)), graphe);
  const xs = liste(o.CP1);
  const ys = deux ? liste(o.CP2) : xs.map(() => 0);
  const traces = pointsObservations(ctx, m, loc, xs, ys);
  if (loc.couleur === 'classe') traces.forEach((tr) => { tr.name = nomGroupe(tr.name); });
  const ce = m.acp.centres;
  const c = couleursTheme();
  traces.push({ type: 'scatter', mode: 'markers+text', name: t('km_centres_mot'), x: liste(ce.CP1), y: deux ? liste(ce.CP2) : liste(ce.CP1).map(() => 0),
    text: liste(ce.groupe).map(String), textposition: 'top center', textfont: { size: 12, color: c.texte },
    marker: { symbol: 'x-thin', size: 16, line: { width: 3, color: c.texte } }, hovertemplate: `${t('groupe')} %{text}<extra></extra>` });
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${t('km_plan')}` },
    xaxis: { title: { text: `PC1 (${fmt(100 * part[0], 1)} %)` }, zeroline: true },
    yaxis: { title: { text: deux ? `PC2 (${fmt(100 * part[1], 1)} %)` : '' }, zeroline: true } });
  brancherSelection(graphe, ctx, m.id);
}

function rendreSilhouette(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const o = m.observations;
  if (o.silhouette === undefined) {
    conteneur.replaceChildren(el('p', { class: 'infos' }, t('km_sans_silhouette', { n: m.silhouette_max })));
    return;
  }
  const g = liste(o.groupe);
  const s = liste(o.silhouette);
  const voisin = liste(o.voisin);
  const ordre = g.map((_, i) => i).sort((a, b) => g[a] - g[b] || s[b] - s[a]);
  const position = new Map(ordre.map((i, r) => [i, r]));
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('km_silhouette_note', { s: fmt(m.silhouette, 3) })), graphe);
  const tailles = m.tailles;
  tracer(ctx, graphe, liste(tailles.groupe).map((k, j) => {
    const idx = ordre.filter((i) => g[i] === k);
    return { type: 'bar', orientation: 'h', name: `${nomGroupe(k)} (${fmt(liste(tailles.silhouette)[j], 2)})`, x: idx.map((i) => s[i]), y: idx.map((i) => position.get(i)),
      customdata: idx.map((i) => liste(o.uid)[i]), hovertext: idx.map((i) => `<b>${liste(o.nom)[i]}</b><br>${t('km_voisin')} : ${nomGroupe(voisin[i])}`),
      marker: { color: PALETTE[j % PALETTE.length] }, hovertemplate: '%{hovertext}<br>%{x:.3f}<extra>%{fullData.name}</extra>' };
  }), { title: { text: `${m.nom} · ${t('km_silhouette')}` }, bargap: 0, showlegend: true, dragmode: 'select',
    yaxis: { autorange: 'reversed', showticklabels: false, title: { text: t('observations') } }, xaxis: { title: { text: t('km_largeur') }, zeroline: true },
    shapes: [{ type: 'line', yref: 'paper', y0: 0, y1: 1, x0: m.silhouette, x1: m.silhouette, line: { color: '#d1495b', dash: 'dash', width: 1 } }] });
  brancherSelection(graphe, ctx, m.id);
}

async function rendreChoixK(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  if (!loc.res || loc.id !== m.id) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('km_calcul_choix')));
    loc.res = await ctx.demander('kmeans_choix', { modele: m.id });
    loc.id = m.id;
  }
  const ch = loc.res.choix;
  const k = liste(ch.k);
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('km_choix_note')), graphe, bas);
  const c = couleursTheme();
  const ici = k.indexOf(m.k);
  tracer(ctx, graphe, [
    { type: 'scatter', mode: 'lines+markers', name: t('km_intra'), x: k, y: liste(ch.intra), line: { color: PALETTE[0] }, hovertemplate: 'k = %{x}<br>%{y:.4g}<extra></extra>' },
    { type: 'scatter', mode: 'lines+markers', name: t('km_silhouette_moyenne'), x: k, y: liste(ch.silhouette), yaxis: 'y2', line: { color: PALETTE[1] },
      hovertemplate: 'k = %{x}<br>%{y:.3f}<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('km_choix')}` }, dragmode: false, showlegend: true, margin: { r: 60 },
    xaxis: { title: { text: 'k' }, dtick: 1 }, yaxis: { title: { text: t('km_intra') } },
    yaxis2: { overlaying: 'y', side: 'right', title: { text: t('km_silhouette_moyenne') }, showgrid: false, zeroline: false },
    shapes: ici >= 0 ? [{ type: 'line', yref: 'paper', y0: 0, y1: 1, x0: m.k, x1: m.k, line: { color: c.doux, dash: 'dot', width: 1 } }] : [] });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: k.length, nom: 'Table', choisie: (i) => k[i] === m.k,
    colonnes: [{ cle: 'k', titre: 'k', decimales: 0 }, { cle: 'intra', titre: t('km_intra') }, { cle: 'r2', titre: 'R2', decimales: 3 },
      { cle: 'silhouette', titre: t('km_silhouette_moyenne'), decimales: 3 }],
    valeur: (i, cle) => liste(ch[cle])[i],
  });
}

function rendreCentres(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const cu = m.centres_unites;
  const ce = m.centres;
  const termes = Object.keys(ce).filter((k) => k !== 'groupe');
  const unites = Object.keys(cu).filter((k) => k !== 'groupe');
  const groupes = liste(ce.groupe);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('km_centres_note')), graphe, bas);
  tracer(ctx, graphe, [{ type: 'heatmap', x: termes, y: groupes.map(nomGroupe), z: groupes.map((_, i) => termes.map((tm) => liste(ce[tm])[i])),
    colorscale: 'RdBu', reversescale: true, zmid: 0, hovertemplate: '%{y} · %{x}<br>%{z:.3f}<extra></extra>', colorbar: { thickness: 12 } }],
  { title: { text: `${m.nom} · ${t('km_centres')}` }, dragmode: false, xaxis: { automargin: true }, yaxis: { autorange: 'reversed' } });
  const tailles = m.tailles;
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: groupes.length, nom: 'Table',
    colonnes: [{ cle: 'groupe', titre: t('groupe'), decimales: 0 }, { cle: 'n', titre: 'n', decimales: 0 }, { cle: 'intra', titre: t('km_intra') }]
      .concat(unites.map((u) => ({ cle: `u:${u}`, titre: u }))),
    valeur: (i, k) => (k.startsWith('u:') ? liste(cu[k.slice(2)])[i] : liste(tailles[k])[i]),
  });
}

function rendreResumeKmeans(conteneur, ctx, m) {
  const ta = m.tailles;
  const fiche = el('div', { class: 'fiche' }, titreFiche(m),
    el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('lda_termes'), m.K, 0), chiffre('k', m.k, 0),
      m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null),
    el('div', { class: 'fiche-chiffres' }, chiffre('R2', m.inertie.r2), chiffre(t('km_silhouette_moyenne'), m.silhouette),
      chiffre(t('km_intra'), m.inertie.intra, 4), chiffre(t('km_totale'), m.inertie.totale, 4)),
    ficheTest(m));
  const notes = notesModele(conteneur, ctx, m);
  const silhouettes = liste(ta.silhouette);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ,
    tableCompacte([t('groupe'), 'n', '%', t('km_intra'), t('km_silhouette_moyenne')],
      lignesDe(ta, ['groupe', 'n', 'part', 'intra']).map((l, i) => [nomGroupe(l[0]), String(l[1]), fmt(100 * l[2], 1), fmt(l[3], 4), fmt(silhouettes[i], 3)])),
    el('div', { class: 'outils' }, enregistrerGroupes(ctx, m)),
    el('p', { class: 'note' }, t('km_resume_note', { echelles: liste(m.echelles).map((e) => t('echelle_' + e)).join(', ') })),
    liste(m.avertissements).length ? el('p', { class: 'note erreur' }, liste(m.avertissements).join(' ; ')) : null));
  notes.retrouver();
}

// ------------------------------------------------------------ the model dialog

const nombreOption = (st, cle, defaut, attrs = {}) => el('input', { type: 'number', value: st.options[cle] ?? defaut, style: 'width:6em', ...attrs,
  oninput: (e) => { st.options[cle] = e.target.value === '' ? null : Number(e.target.value); } });
const quantitative = (p, u) => !(p.variables.find((v) => v.uid === u) || {}).qualitative;
const entier = (v, min, max) => v === undefined || v === null || (Number.isInteger(v) && v >= min && v <= max);

ANALYSES.TREE = {
  libelle: () => t('type_tree'),
  note: () => t('arb_note'),
  roles: ['x', 'y', 'classe'],
  echelle: 'none',
  test: true,  // (a test set, predicted after the fit: arbres.R, tester_tree)
  champs: (st) => [
    el('label', {}, t('arb_minbucket')), el('div', { class: 'ligne' }, nombreOption(st, 'minbucket', 20, { min: 1, step: 1 }),
      el('label', { class: 'champ' }, t('arb_maxdepth'), nombreOption(st, 'maxdepth', 5, { min: 1, max: 30, step: 1 }))),
    el('label', {}, 'cp'), el('div', { class: 'ligne' }, nombreOption(st, 'cp', 0.001, { min: 0, max: 1, step: 0.001 }),
      el('label', { class: 'champ' }, t('arb_maxsurrogate'), nombreOption(st, 'maxsurrogate', 5, { min: 0, max: 20, step: 1 }))),
    el('label', {}, t('arb_elagage')), el('div', { class: 'ligne' }, el('select', { onchange: (e) => { st.options.elagage = e.target.value; } },
      [['1se', t('arb_regle_1se')], ['min', t('arb_regle_min')]].map(([k, n]) => el('option', { value: k, selected: k === (st.options.elagage || '1se') }, n))),
    el('label', { class: 'champ' }, t('arb_tirages_champ'), nombreOption(st, 'tirages', 200, { min: 10, max: 2000, step: 10 }))),
    el('label', {}, ''), el('p', { class: 'note' }, t('arb_reglages_note')),
  ],
  valider: (st, cles, p) => {
    const y = cles('y');
    const c = cles('classe');
    if (y.length + c.length !== 1) return t('arb_reponse_requise');
    if (y.length && !quantitative(p, y[0])) return t('arb_y_quantitative');
    if (st.croises.length) return t('arb_croises');
    const o = st.options;
    if (!entier(o.minbucket, 1, 1e6) || !entier(o.maxdepth, 1, 30) || !entier(o.maxsurrogate, 0, 20) || !entier(o.tirages, 10, 2000)
      || (o.cp !== undefined && o.cp !== null && !(o.cp >= 0 && o.cp <= 1))) return t('arb_reglages_invalides');
    return '';
  },
  options: (st) => {
    const o = st.options;
    const v = (k, d) => (Number.isFinite(o[k]) ? o[k] : d);
    return { minbucket: v('minbucket', 20), cp: v('cp', 0.001), maxdepth: v('maxdepth', 5), maxsurrogate: v('maxsurrogate', 5),
      elagage: o.elagage === 'min' ? 'min' : '1se', tirages: v('tirages', 200) };
  },
};
ANALYSES.LDA = {
  libelle: () => t('type_lda'),
  note: () => t('lda_note'),
  roles: ['x', 'classe'],
  echelle: 'none',
  test: true,  // (discrimination.R, tester_lda)
  champs: (st) => [
    el('label', {}, t('lda_prior')), el('select', { onchange: (e) => { st.options.priors = e.target.value; } },
      [['proportionnelles', t('lda_priors_proportionnelles')], ['egales', t('lda_priors_egales')]]
        .map(([k, n]) => el('option', { value: k, selected: k === (st.options.priors || 'proportionnelles') }, n))),
  ],
  valider: (st, cles) => (cles('classe').length !== 1 ? t('lda_classe_requise') : st.croises.length > 10 ? t('lda_trop_croises') : ''),
  options: (st) => ({ priors: st.options.priors === 'egales' ? 'egales' : 'proportionnelles' }),
};
ANALYSES.KMEANS = {
  libelle: () => t('type_kmeans'),
  note: () => t('km_note'),
  roles: ['x'],
  echelle: 'UV',
  vc: false,
  champs: (st) => [el('label', {}, t('km_k')), nombreOption(st, 'k', 3, { min: 2, max: 50, step: 1 })],
  valider: (st) => (entier(st.options.k, 2, 50) ? '' : t('km_k_invalide')),
  options: (st) => ({ k: Number.isFinite(st.options.k) ? st.options.k : 3 }),
};

// ------------------------------------------------------------ the report

const demandeRapport = async (nom, m, f) => { try { return f(await demander(nom, { modele: m.id })); } catch (e) { return [{ texte: e.message }]; } };
const blocsClassement = (m) => {
  const r = m.classification.vc;
  const classes = liste(r.classes);
  return [{ texte: `${t('validation_croisee')} : ${t('bien_classes')} ${pct(r.global)}` },
    { entetes: [`${t('observee')} \\ ${t('predite')}`, ...classes, t('bien_classes')],
      lignes: classes.map((k, i) => [k, ...r.table[i].map(String), pct(liste(r.correct)[i])]) }];
};
const blocsResumeArbre = (m) => {
  const q = m.qualite;
  return [{ texte: `${t(m.classement ? 'arb_classement' : 'arb_regression')} · N = ${m.N} · ${t('variables')} ${m.K} · ${t('groupes_vc')} ${groupesVC(m)}` },
    m.classement ? { entetes: [t('bien_classes'), t('bien_classes_vc'), t('arb_feuilles_titre'), 'cp'], lignes: [[pct(q.bien_classes), pct(q.bien_classes_vc), String(liste(m.regles.feuille).length), fmt(m.elagage.cp, 4)]] }
      : { entetes: ['R2', 'Q2', 'RMSEE', 'RMSEcv', t('arb_feuilles_titre'), 'cp'], lignes: [[fmt(q.r2, 3), fmt(q.Q2, 3), fmt(q.RMSEE, 4), fmt(q.RMSEcv, 4), String(liste(m.regles.feuille).length), fmt(m.elagage.cp, 4)]] },
    m.premiere && m.premiere.regle ? { texte: `${t('arb_premiere')} : ${m.premiere.regle}` } : null,
    { texte: noteReglagesArbre(m) }].filter(Boolean);
};
const blocsRegles = (m) => [{ entetes: [t('arb_feuille'), 'n', m.classement ? t('classe') : t('arb_moyenne'), t('arb_regle')],
  lignes: lignesDe(m.regles, ['feuille', 'n', 'valeur', 'regle', 'part']).map((l) => [String(l[0]), String(l[1]),
    m.classement ? `${l[2]} (${pct(l[4], 0)})` : fmt(l[2], 3), l[3]]) }];
const blocsImportance = (m) => [{ entetes: [t('variable_arbre'), t('bloc'), '%'],
  lignes: lignesDe(m.importance, ['variable', 'bloc', 'pourcentage']).map((l) => [l[0], l[1] || '', fmt(l[2], 1)]) }];
const blocsElagage = (m) => [{ texte: t(m.elagage.regle === 'min' ? 'arb_elagage_note_min' : 'arb_elagage_note', { seuil: fmt(m.elagage.seuil, 3), minimum: fmt(m.elagage.minimum, 3), feuilles: m.elagage.feuilles }) }];
const blocsStabilite = (m) => demandeRapport('arbre_stabilite', m, (r) => [{ texte: t('arb_stabilite_note', { n: m.reglages_arbre.tirages }) },
  { entetes: [t('arb_premiere'), t('arb_tirages'), t('arb_frequence'), t('arb_seuil_median')],
    lignes: lignesDe(r.stabilite, ['variable', 'tirages', 'frequence', 'seuil_median']).map((l) => [l[0], String(l[1]), fmt(l[2], 1), l[3] === null ? '' : String(+l[3].toPrecision(4))]) }]);
PARTIES_TYPES.TREE = [
  { cle: 'resume', defaut: true, tableau: blocsResumeArbre },
  { cle: 'arbre', vue: 'arbre', defaut: true },
  { cle: 'regles_arbre', defaut: true, tableau: blocsRegles },
  { cle: 'elagage', vue: 'elagage', defaut: true, tableau: blocsElagage },
  { cle: 'importance_arbre', vue: 'importance_arbre', defaut: true, tableau: blocsImportance },
  { cle: 'stabilite_arbre', vue: 'stabilite_arbre', tableau: blocsStabilite },
  { cle: 'obspred', vue: 'obspred', defaut: true, si: (m) => !m.classement },
  { cle: 'residus', vue: 'residus', si: (m) => !m.classement, variantes: async () => [{ mode: 'ajustes' }, { mode: 'qq' }] },
  { cle: 'classement', vue: 'classement', defaut: true, si: (m) => m.classement, tableau: blocsClassement },
];
const blocsResumeLDA = (m) => [{ texte: `${t('type_lda')} · N = ${m.N} · ${t('lda_termes')} ${m.K} · ${t('lda_classes')} ${liste(m.classes).length} · ${t('groupes_vc')} ${groupesVC(m)}` },
  { entetes: [t('bien_classes'), t('bien_classes_vc')].concat(liste(m.axes)), lignes: [[pct(m.qualite.bien_classes), pct(m.qualite.bien_classes_vc)].concat(liste(m.part).map((v) => pct(v)))] },
  { entetes: [t('classe'), 'n', t('lda_prior')], lignes: lignesDe(m.priors, ['classe', 'n', 'prior']).map((l) => [l[0], String(l[1]), fmt(l[2], 3)]) }];
const blocsCoefficientsLDA = (m) => {
  const axes = liste(m.axes);
  const bloc = (df, titre) => [{ texte: titre }, { entetes: [t('terme'), ...axes], lignes: lignesDe(df, ['terme', ...axes]).map((l) => [l[0], ...l.slice(1).map((v) => fmt(v, 4))]) }];
  return [...bloc(m.structure, t('lda_structure')), ...bloc(m.coefficients, t('lda_coefficients_bruts'))];
};
PARTIES_TYPES.LDA = [
  { cle: 'resume', defaut: true, tableau: blocsResumeLDA },
  { cle: 'lda_scores', vue: 'lda_scores', defaut: true },
  { cle: 'lda_coefficients', vue: 'lda_coefficients', defaut: true, tableau: blocsCoefficientsLDA },
  { cle: 'classement', vue: 'classement', defaut: true, tableau: blocsClassement },
];
const blocsResumeKmeans = (m) => [{ texte: `${t('type_kmeans')} · N = ${m.N} · ${t('lda_termes')} ${m.K} · k = ${m.k} · R2 ${fmt(m.inertie.r2, 3)}`
  + (m.silhouette !== null ? ` · ${t('km_silhouette_moyenne')} ${fmt(m.silhouette, 3)}` : '') },
  { entetes: [t('groupe'), 'n', '%', t('km_intra'), t('km_silhouette_moyenne')],
    lignes: lignesDe(m.tailles, ['groupe', 'n', 'part', 'intra', 'silhouette']).map((l) => [nomGroupe(l[0]), String(l[1]), fmt(100 * l[2], 1), fmt(l[3], 4), l[4] === null ? '' : fmt(l[4], 3)]) }];
const blocsCentres = (m) => {
  const u = Object.keys(m.centres_unites).filter((k) => k !== 'groupe');
  return [{ texte: t('km_centres_note') }, { entetes: [t('groupe'), ...u], lignes: lignesDe(m.centres_unites, ['groupe', ...u]).map((l) => [nomGroupe(l[0]), ...l.slice(1).map((v) => fmt(v, 3))]) }];
};
PARTIES_TYPES.KMEANS = [
  { cle: 'resume', defaut: true, tableau: blocsResumeKmeans },
  { cle: 'km_plan', vue: 'km_plan', defaut: true },
  { cle: 'km_silhouette', vue: 'km_silhouette', defaut: true, si: (m) => m.observations.silhouette !== undefined },
  { cle: 'km_centres', vue: 'km_centres', defaut: true, tableau: blocsCentres },
  { cle: 'km_choix', vue: 'km_choix' },
];

// ------------------------------------------------------------ registration

const RESUMES = { TREE: rendreResumeArbre, LDA: rendreResumeLDA, KMEANS: rendreResumeKmeans };
const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estClassement(m) ? RESUMES[m.type](c, ctx, m) : resume(c, ctx, p, loc);
} };
Object.assign(VUES, {
  // (the PLS-DA's classification view, under a name that says nothing of PLS-DA)
  classement: { rendre: VUES.classification.rendre, titre: (m) => `${m.nom} · ${t('classement')}` },
  arbre: { rendre: rendreArbre, titre: (m) => `${m.nom} · ${t('arbre')}` },
  elagage: { rendre: rendreElagage, titre: (m) => `${m.nom} · ${t('elagage')}` },
  regles_arbre: { rendre: rendreRegles, titre: (m) => `${m.nom} · ${t('regles_arbre')}` },
  importance_arbre: { rendre: rendreImportance, titre: (m) => `${m.nom} · ${t('importance_arbre')}` },
  stabilite_arbre: { rendre: rendreStabilite, titre: (m) => `${m.nom} · ${t('stabilite_arbre')}` },
  lda_scores: { rendre: rendreScoresLDA, titre: (m) => `${m.nom} · ${t('lda_scores')}` },
  lda_coefficients: { rendre: rendreCoefficientsLDA, titre: (m) => `${m.nom} · ${t('lda_coefficients')}` },
  lda_probabilites: { rendre: rendreProbabilitesLDA, titre: (m) => `${m.nom} · ${t('lda_probabilites')}` },
  km_plan: { rendre: rendrePlanKmeans, titre: (m) => `${m.nom} · ${t('km_plan')}` },
  km_silhouette: { rendre: rendreSilhouette, titre: (m) => `${m.nom} · ${t('km_silhouette')}` },
  km_choix: { rendre: rendreChoixK, titre: (m) => `${m.nom} · ${t('km_choix')}` },
  km_centres: { rendre: rendreCentres, titre: (m) => `${m.nom} · ${t('km_centres')}` },
});
Object.assign(ICONES_VUES, { classement: 'grid-3x3', arbre: 'network', elagage: 'scissors', regles_arbre: 'list', importance_arbre: 'chart-bar', stabilite_arbre: 'shuffle',
  lda_scores: 'chart-scatter', lda_coefficients: 'chart-column', lda_probabilites: 'table-2', km_plan: 'chart-scatter', km_silhouette: 'chart-bar',
  km_choix: 'chart-line', km_centres: 'grid-3x3' });
const VUES_ARBRE = ['resume', 'arbre', 'elagage', 'regles_arbre', 'importance_arbre', 'stabilite_arbre'];
VUES_TYPES.TREE = (m) => VUES_ARBRE.concat(m.classement ? ['classement'] : ['obspred', 'residus'], ['liste_obs', 'predictions']);
VUES_TYPES.LDA = ['resume', 'lda_scores', 'lda_coefficients', 'classement', 'lda_probabilites', 'liste_obs', 'predictions'];
VUES_TYPES.KMEANS = (m) => ['resume', 'km_plan'].concat(m.observations && m.observations.silhouette === undefined ? [] : ['km_silhouette'],
  ['km_choix', 'km_centres', 'liste_obs', 'predictions']);

ajouterTextes({
  type_tree: 'Arbre de décision (régression ou classement)', type_lda: 'Analyse discriminante linéaire (LDA)', type_kmeans: 'k-moyennes',
  classement: 'Classement', arb_feuilles_titre: 'Feuilles',
  arbre: 'Arbre', elagage: 'Élagage', regles_arbre: 'Règles des feuilles', importance_arbre: 'Importance des variables', stabilite_arbre: 'Stabilité de la première coupure',
  lda_scores: 'Scores discriminants', lda_coefficients: 'Coefficients discriminants', lda_probabilites: 'Probabilités a posteriori',
  km_plan: 'Groupes (deux premières composantes)', km_silhouette: 'Silhouettes', km_choix: 'Choix de k', km_centres: 'Centres des groupes',
  classification_note_proba: 'Chaque observation est rangée dans la classe de plus grande probabilité. En validation croisée, chaque groupe est classé par un modèle ajusté sans lui.',
  arb_note: 'Une variable Y quantitative (arbre de régression) ou une variable de classe (arbre de classement), et les variables X par leurs valeurs brutes : les seuils se lisent dans leurs unités, les valeurs manquantes sont prises en charge par des variables de substitution (rpart). Élagué par une validation croisée interne par les groupes de validation croisée (par exemple un site), et validé en prédisant chaque groupe par un arbre refait sans lui.',
  arb_minbucket: 'Observations par feuille (au moins)', arb_maxdepth: 'niveaux (au plus)', arb_maxsurrogate: 'substitutions (au plus)',
  arb_elagage: 'Élagage', arb_regle_1se: 'règle 1-ET (le plus petit arbre à une erreur-type du minimum)', arb_regle_min: 'erreur minimale',
  arb_tirages_champ: 'tirages bootstrap (stabilité)',
  arb_reglages_note: 'Par défaut : 20 observations par feuille, mais au plus le sixième des observations (un nœud n’est coupé que s’il en compte trois fois plus), 5 niveaux, cp = 0,001, 5 variables de substitution, règle 1-ET. Il faut au moins trois groupes de validation croisée.',
  arb_minbucket_ramene: '{demande} observations par feuille demandées, ramenées à {minbucket}, le sixième des {n} observations, pour que l’arbre puisse se couper.',
  arb_reponse_requise: 'Un arbre demande une réponse : une variable Y quantitative (arbre de régression) ou une variable de classe (arbre de classement), pas les deux.',
  arb_y_quantitative: 'La réponse d’un arbre de régression est quantitative ; une variable qualitative prend le rôle Classe (arbre de classement).',
  arb_croises: 'Un arbre fait lui-même ses interactions : retirer les termes croisés.',
  arb_reglages_invalides: 'Réglages de l’arbre : des nombres entiers positifs (niveaux de 1 à 30, substitutions de 0 à 20, tirages de 10 à 2000) et un cp entre 0 et 1.',
  arb_feuilles: 'feuilles', arb_feuille: 'Feuille', arb_premiere: 'Première coupure', arb_noeuds: 'nœuds', arb_clic_noeud: 'Clic sur un nœud : ses observations sont choisies.',
  arb_erreur_vc: 'Erreur de validation croisée (± erreur-type)', arb_erreur_relative: 'Erreur relative (ajustement)', arb_retenu: 'Arbre retenu',
  arb_nb_feuilles: 'Nombre de feuilles', arb_erreur_rel_axe: 'Erreur relative', arb_coupures: 'Coupures',
  arb_elagage_note: 'Erreur de validation croisée interne de chaque taille d’arbre, relative à celle de la racine. Règle 1-ET : l’arbre retenu ({feuilles} feuilles) est le plus petit dont l’erreur ne dépasse pas l’erreur minimale plus son erreur-type ({seuil}, en pointillé).',
  arb_elagage_note_min: 'Erreur de validation croisée interne de chaque taille d’arbre, relative à celle de la racine. L’arbre retenu ({feuilles} feuilles) est celui de l’erreur minimale ({minimum}, en pointillé).',
  arb_regles_note: 'Clic sur une feuille : ses observations sont choisies.', arb_moyenne: 'Moyenne', arb_part: 'Part', arb_regle: 'Règle',
  arb_importance_note: 'L’importance d’une variable (rpart) additionne ce qu’elle apporte à chaque coupure, comme variable de la coupure ou comme substitution : une variable importante n’est pas forcément dans l’arbre.',
  arb_sans_coupure: 'L’arbre retenu n’a pas de coupure.', variable_arbre: 'Variable',
  arb_calcul_stabilite: 'Calcul : l’arbre refait sur {n} tirages bootstrap des groupes…',
  arb_stabilite_note: 'Sur {n} tirages bootstrap des groupes de validation croisée (avec remise, graine 2026), l’arbre refait et élagué : la part des tirages où chaque variable fait la première coupure, et son seuil médian.',
  arb_stabilite_echecs: '({n} tirages sans arbre.)', arb_seuil_median: 'seuil médian', arb_frequence: 'Part des tirages (%)', arb_tirages: 'Tirages',
  arb_resume_regression: 'Arbre de régression de {y} : chaque feuille prédit la moyenne de ses observations.',
  arb_resume_classement: 'Arbre de classement de {y} : chaque feuille prédit sa classe la plus fréquente (indice de Gini).',
  arb_reglages: 'Au moins {minbucket} observations par feuille, au plus {maxdepth} niveaux, cp = {cp}, au plus {maxsurrogate} variables de substitution ; élagage : {regle}. {manquantes} observations ont au moins une valeur X manquante.',
  arb_vc_note: 'Validation croisée externe : chaque groupe est prédit par un arbre refait entièrement sans lui (sa validation croisée interne et son élagage compris).',
  arb_regression: 'Arbre de régression', arb_classement: 'Arbre de classement',
  lda_note: 'Une variable de classe et des variables X, par leurs valeurs brutes (MASS::lda) : les fonctions discriminantes qui séparent le mieux les classes ; chaque observation va dans la classe de plus grande probabilité a posteriori. Validation croisée par les groupes de validation croisée.',
  lda_prior: 'Probabilités a priori', lda_priors_proportionnelles: 'proportionnelles aux effectifs', lda_priors_egales: 'égales',
  lda_classe_requise: 'Une LDA demande une variable de classe (rôle Classe, une variable qualitative).', lda_trop_croises: 'Trop de termes croisés pour une LDA.',
  lda_centroides: 'centres des classes', lda_structure: 'Corrélations de structure', lda_coefficients_bruts: 'Coefficients (unités des variables)',
  lda_coefficients_std: 'Coefficients (variables d’écart-type 1)', lda_fonction: 'Fonction',
  lda_structure_note: 'La corrélation de chaque terme avec les scores de la fonction : ce qui la porte.',
  lda_coefficients_note: 'Le score d’une observation est la somme de ses valeurs multipliées par ces coefficients (centrée).',
  lda_coefficients_std_note: 'Les coefficients pour des variables ramenées à un écart-type de 1 : comparables d’une variable à l’autre.',
  lda_probabilites_note: 'En rouge : une classe ajustée ou prédite qui n’est pas la classe observée.', lda_ajustee: 'Ajustée', lda_predite_vc: 'Prédite (VC)',
  lda_termes: 'Termes', lda_classes: 'Classes',
  lda_resume_note: 'Les X entrent par leurs valeurs brutes ; probabilités a priori {priors}. Chaque observation va dans la classe de plus grande probabilité a posteriori ; en validation croisée, chaque groupe est classé par le modèle ajusté sans lui.',
  lda_retires: 'Laissés de côté (constants, ou déduits des autres) : {termes}.', lda_colineaires: 'Des variables sont colinéaires : MASS a réduit le rang.',
  lda_echecs_vc: '{n} groupes de validation croisée sans prédiction (sans eux, une seule classe reste, ou le modèle ne s’ajuste pas).',
  km_note: 'Des variables X, mises à l’échelle comme le dit l’onglet Variables (UV par défaut) : k groupes d’observations qui rendent minimale la somme des carrés des écarts à leur centre (algorithme de Hartigan et Wong, 25 départs, graine 1).',
  km_k: 'Nombre de groupes (k)', km_k_invalide: 'Le nombre de groupes k est un entier de 2 à 50.',
  km_groupes_mot: 'groupes', km_nom_variable: 'Nom de la nouvelle variable', km_enregistrer: 'Groupes → variable',
  km_enregistrer_aide: 'Ajouter les groupes au projet comme une variable qualitative (Édition › Annuler la retire).', km_variable_ajoutee: 'Variable « {nom} » ajoutée au projet.',
  km_centres_mot: 'centres', km_sans_silhouette: 'Pas de silhouettes au-delà de {n} observations.',
  km_silhouette_note: 'La silhouette d’une observation compare sa distance moyenne à son groupe et au groupe voisin : proche de 1, bien placée ; négative, plus proche du voisin. Moyenne : {s} (en pointillé).',
  km_voisin: 'voisin', km_largeur: 'Silhouette', km_calcul_choix: 'Calcul : les k-moyennes de k = 2 à 10…',
  km_choix_note: 'Pour chaque k : la somme des carrés intra-groupes (un coude marque un bon k) et la silhouette moyenne (la plus grande est la meilleure). En pointillé : le k du modèle.',
  km_intra: 'Somme des carrés intra', km_totale: 'Somme des carrés totale', km_silhouette_moyenne: 'Silhouette moyenne',
  km_centres_note: 'La carte : les centres dans l’espace mis à l’échelle (écarts à la moyenne). Le tableau : les centres dans les unités des variables (pour une catégorie, sa part dans le groupe).',
  km_resume_note: 'Échelle des termes : {echelles}. Les groupes sont numérotés du plus grand au plus petit ; une nouvelle observation va au groupe du centre le plus proche (Prédire).',
}, {
  type_tree: 'Decision tree (regression or classification)', type_lda: 'Linear discriminant analysis (LDA)', type_kmeans: 'k-means',
  classement: 'Classification', arb_feuilles_titre: 'Leaves',
  arbre: 'Tree', elagage: 'Pruning', regles_arbre: 'Leaf rules', importance_arbre: 'Variable importance', stabilite_arbre: 'First-split stability',
  lda_scores: 'Discriminant scores', lda_coefficients: 'Discriminant coefficients', lda_probabilites: 'Posterior probabilities',
  km_plan: 'Clusters (first two components)', km_silhouette: 'Silhouettes', km_choix: 'Choice of k', km_centres: 'Cluster centres',
  classification_note_proba: 'Each observation goes to the class of largest probability. Cross-validated: each group is classified by a model fitted without it.',
  arb_note: 'A quantitative Y variable (regression tree) or a class variable (classification tree), and the X variables by their raw values: the thresholds read in their units, missing values are handled by surrogate variables (rpart). Pruned by an internal cross-validation by the cross-validation groups (a site, say), and validated by predicting each group with a tree rebuilt without it.',
  arb_minbucket: 'Observations per leaf (at least)', arb_maxdepth: 'levels (at most)', arb_maxsurrogate: 'surrogates (at most)',
  arb_elagage: 'Pruning', arb_regle_1se: '1-SE rule (the smallest tree within one standard error of the minimum)', arb_regle_min: 'minimum error',
  arb_tirages_champ: 'bootstrap draws (stability)',
  arb_reglages_note: 'By default: 20 observations per leaf, but at most a sixth of the observations (a node is split only if it holds three times as many), 5 levels, cp = 0.001, 5 surrogate variables, 1-SE rule. At least three cross-validation groups are needed.',
  arb_minbucket_ramene: '{demande} observations per leaf requested, lowered to {minbucket}, a sixth of the {n} observations, so that the tree can split.',
  arb_reponse_requise: 'A tree needs a response: a quantitative Y variable (regression tree) or a class variable (classification tree), not both.',
  arb_y_quantitative: 'A regression tree’s response is quantitative; a qualitative variable takes the Class role (classification tree).',
  arb_croises: 'A tree makes its own interactions: remove the cross terms.',
  arb_reglages_invalides: 'Tree settings: positive whole numbers (levels from 1 to 30, surrogates from 0 to 20, draws from 10 to 2000) and a cp between 0 and 1.',
  arb_feuilles: 'leaves', arb_feuille: 'Leaf', arb_premiere: 'First split', arb_noeuds: 'nodes', arb_clic_noeud: 'Click a node: its observations are selected.',
  arb_erreur_vc: 'Cross-validated error (± standard error)', arb_erreur_relative: 'Relative error (fit)', arb_retenu: 'Tree retained',
  arb_nb_feuilles: 'Number of leaves', arb_erreur_rel_axe: 'Relative error', arb_coupures: 'Splits',
  arb_elagage_note: 'Internal cross-validated error of each tree size, relative to that of the root. 1-SE rule: the tree retained ({feuilles} leaves) is the smallest whose error is within the minimum error plus its standard error ({seuil}, dashed).',
  arb_elagage_note_min: 'Internal cross-validated error of each tree size, relative to that of the root. The tree retained ({feuilles} leaves) is that of the minimum error ({minimum}, dashed).',
  arb_regles_note: 'Click a leaf: its observations are selected.', arb_moyenne: 'Mean', arb_part: 'Share', arb_regle: 'Rule',
  arb_importance_note: 'A variable’s importance (rpart) adds up what it brings to each split, as the split’s variable or as a surrogate: an important variable is not necessarily in the tree.',
  arb_sans_coupure: 'The tree retained has no split.', variable_arbre: 'Variable',
  arb_calcul_stabilite: 'Computing: the tree rebuilt on {n} bootstrap draws of the groups…',
  arb_stabilite_note: 'Over {n} bootstrap draws of the cross-validation groups (with replacement, seed 2026), the tree rebuilt and pruned: the share of the draws where each variable makes the first split, and its median threshold.',
  arb_stabilite_echecs: '({n} draws without a tree.)', arb_seuil_median: 'median threshold', arb_frequence: 'Share of the draws (%)', arb_tirages: 'Draws',
  arb_resume_regression: 'Regression tree of {y}: each leaf predicts the mean of its observations.',
  arb_resume_classement: 'Classification tree of {y}: each leaf predicts its most frequent class (Gini index).',
  arb_reglages: 'At least {minbucket} observations per leaf, at most {maxdepth} levels, cp = {cp}, at most {maxsurrogate} surrogate variables; pruning: {regle}. {manquantes} observations have at least one missing X value.',
  arb_vc_note: 'External cross-validation: each group is predicted by a tree entirely rebuilt without it (its internal cross-validation and pruning included).',
  arb_regression: 'Regression tree', arb_classement: 'Classification tree',
  lda_note: 'A class variable and X variables, by their raw values (MASS::lda): the discriminant functions that best separate the classes; each observation goes to the class of largest posterior probability. Cross-validated by the cross-validation groups.',
  lda_prior: 'Prior probabilities', lda_priors_proportionnelles: 'proportional to the sizes', lda_priors_egales: 'equal',
  lda_classe_requise: 'An LDA needs a class variable (Class role, a qualitative variable).', lda_trop_croises: 'Too many cross terms for an LDA.',
  lda_centroides: 'class centres', lda_structure: 'Structure correlations', lda_coefficients_bruts: 'Coefficients (units of the variables)',
  lda_coefficients_std: 'Coefficients (variables of standard deviation 1)', lda_fonction: 'Function',
  lda_structure_note: 'The correlation of each term with the function’s scores: what carries it.',
  lda_coefficients_note: 'An observation’s score is the sum of its values times these coefficients (centred).',
  lda_coefficients_std_note: 'The coefficients for variables brought to a standard deviation of 1: comparable from one variable to another.',
  lda_probabilites_note: 'In red: a fitted or predicted class that is not the observed class.', lda_ajustee: 'Fitted', lda_predite_vc: 'Predicted (CV)',
  lda_termes: 'Terms', lda_classes: 'Classes',
  lda_resume_note: 'The X enter by their raw values; prior probabilities {priors}. Each observation goes to the class of largest posterior probability; cross-validated, each group is classified by the model fitted without it.',
  lda_retires: 'Left out (constant, or implied by the others): {termes}.', lda_colineaires: 'Some variables are collinear: MASS reduced the rank.',
  lda_echecs_vc: '{n} cross-validation groups without a prediction (without them, a single class remains, or the model doesn’t fit).',
  km_note: 'X variables, scaled as the Variables tab says (UV by default): k clusters of observations that minimize the sum of squared deviations from their centre (Hartigan and Wong algorithm, 25 starts, seed 1).',
  km_k: 'Number of clusters (k)', km_k_invalide: 'The number of clusters k is a whole number from 2 to 50.',
  km_groupes_mot: 'clusters', km_nom_variable: 'Name of the new variable', km_enregistrer: 'Clusters → variable',
  km_enregistrer_aide: 'Add the clusters to the project as a qualitative variable (Edit › Undo removes it).', km_variable_ajoutee: 'Variable “{nom}” added to the project.',
  km_centres_mot: 'centres', km_sans_silhouette: 'No silhouettes beyond {n} observations.',
  km_silhouette_note: 'An observation’s silhouette compares its mean distance to its cluster and to the neighbouring cluster: near 1, well placed; negative, closer to the neighbour. Mean: {s} (dashed).',
  km_voisin: 'neighbour', km_largeur: 'Silhouette', km_calcul_choix: 'Computing: the k-means from k = 2 to 10…',
  km_choix_note: 'For each k: the within-cluster sum of squares (an elbow marks a good k) and the mean silhouette (the largest is the best). Dotted: the model’s k.',
  km_intra: 'Within sum of squares', km_totale: 'Total sum of squares', km_silhouette_moyenne: 'Mean silhouette',
  km_centres_note: 'The map: the centres in the scaled space (deviations from the mean). The table: the centres in the units of the variables (for a category, its share in the cluster).',
  km_resume_note: 'Scaling of the terms: {echelles}. The clusters are numbered from the largest to the smallest; a new observation goes to the cluster of the nearest centre (Predict).',
});
