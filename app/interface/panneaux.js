// Views. Each draws one model result (or the project data) into a panel and is re-run when
// the language, the theme or the shared selection changes. `ctx` comes from app.js.
import { t, lire, ecrire } from './i18n.js';
import { el, fmt, icone } from './ui.js';

export const PALETTES = {
  defaut: ['#2f6fb5', '#d1495b', '#3b9a5b', '#e09f3e', '#7b5ea7', '#2a9d8f', '#8c564b', '#c2549d', '#6b6b6b', '#a3a322'],
  daltonien: ['#0072b2', '#e69f00', '#009e73', '#d55e00', '#cc79a7', '#56b4e9', '#f0e442', '#000000'],  // Okabe & Ito
  gris: ['#000000', '#7a7a7a', '#b5b5b5', '#3d3d3d', '#9a9a9a', '#5c5c5c'],
};
const PALETTE = [...PALETTES.defaut];
const SELECTION = '#e4572e';
// names written by each point only up to this many variables (spectra have hundreds; a box shows them)
const ETIQUETTES_MAX = 100;

// Plot formatting chosen in Graphiques › Mise en forme, for every plot, kept from one session
// to the next. A plot's own title and axis titles are in its panel's settings (loc).
export const FORMAT = { police: 'Segoe UI', taille: 12, points: 1, palette: 'defaut', grille: true, legende: true,
  largeur: 1200, hauteur: 800, echelle: 2, format: 'png' };
try { Object.assign(FORMAT, JSON.parse(lire('format') || '{}')); } catch { /* defaults */ }
PALETTE.splice(0, PALETTE.length, ...(PALETTES[FORMAT.palette] || PALETTES.defaut));

export function changerFormat(f) {
  Object.assign(FORMAT, f);
  ecrire('format', JSON.stringify(FORMAT));
  PALETTE.splice(0, PALETTE.length, ...(PALETTES[FORMAT.palette] || PALETTES.defaut));
}

// ------------------------------------------------------------ helpers

function choix(libelle, options, valeur, changer) {
  return el('label', { class: 'champ' }, libelle ? el('span', {}, libelle) : null,
    el('select', { onchange: (e) => changer(e.target.value) },
      options.map(([v, txt]) => el('option', { value: v, selected: String(v) === String(valeur) }, txt))));
}

function case_(libelle, valeur, changer) {
  return el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: valeur, onchange: (e) => changer(e.target.checked) }), libelle);
}

const outils = (...enfants) => el('div', { class: 'outils' }, enfants);
const espace = () => el('span', { class: 'espace' });

function couleursTheme() {
  const cs = getComputedStyle(document.documentElement);
  const v = (n, d) => cs.getPropertyValue(n).trim() || d;
  return { texte: v('--texte', '#222'), doux: v('--texte-doux', '#666'), grille: v('--grille', '#e5e5e5'), fond: v('--fond-panneau', '#fff'),
    ligne: v('--ligne-graphe', '#9bb5d6') };
}

function tracer(ctx, div, traces, layout = {}) {
  const c = couleursTheme();
  const F = FORMAT;
  const police = `${F.police}, system-ui, sans-serif`;
  const base = {
    margin: { l: 60, r: 18, t: 34, b: 48 }, paper_bgcolor: c.fond, plot_bgcolor: c.fond,
    font: { family: police, size: F.taille, color: c.texte },
    title: { font: { size: F.taille + 1 }, x: 0.01, xanchor: 'left' },
    xaxis: { gridcolor: c.grille, zerolinecolor: c.doux, automargin: true, linecolor: c.grille, showgrid: F.grille },
    yaxis: { gridcolor: c.grille, zerolinecolor: c.doux, automargin: true, linecolor: c.grille, showgrid: F.grille },
    hovermode: 'closest', dragmode: 'select', showlegend: traces.length > 1 && F.legende,
    // under the axis title, whatever the plot's height
    legend: { orientation: 'h', yref: 'container', y: 0, yanchor: 'bottom', x: 0.5, xanchor: 'center', font: { size: F.taille - 1 } },
    hoverlabel: { font: { family: police } },
  };
  const perso = ctx.formatPanneau ? ctx.formatPanneau() : {};  // this plot's own titles
  const titre = (defaut, texte) => (texte ? { ...(defaut || {}), text: texte } : defaut);
  const lay = { ...base, ...layout, title: { ...base.title, ...titre(layout.title, perso.titre) },
    xaxis: { ...base.xaxis, ...(layout.xaxis || {}), title: titre((layout.xaxis || {}).title, perso.titreX) },
    yaxis: { ...base.yaxis, ...(layout.yaxis || {}), title: titre((layout.yaxis || {}).title, perso.titreY) } };
  if (!F.legende) lay.showlegend = false;
  // A redraw of the same plot (after a selection) keeps the zoom; double-click resets it
  const avant = ctx.grapheCourant ? ctx.grapheCourant() : null;
  const cle = JSON.stringify([lay.title.text, lay.xaxis.title, lay.yaxis.title]);
  if (avant && avant !== div && avant._zoom && avant._cle === cle) {
    div._zoom = avant._zoom;
    if (avant._zoom.x) lay.xaxis = { ...lay.xaxis, range: avant._zoom.x, autorange: false };
    if (avant._zoom.y) lay.yaxis = { ...lay.yaxis, range: avant._zoom.y, autorange: false };
  }
  div._cle = cle;
  const echelle = (x, f) => (Array.isArray(x) ? x.map((v) => v * f) : typeof x === 'number' ? x * f : x);
  traces = traces.map((tr) => {
    const x = { ...tr };  // (no key added where the trace has none: Plotly rejects marker: undefined)
    if (tr.marker && tr.marker.size !== undefined && F.points !== 1) x.marker = { ...tr.marker, size: echelle(tr.marker.size, F.points) };
    if (tr.textfont && tr.textfont.size) x.textfont = { ...tr.textfont, size: tr.textfont.size * F.taille / 12 };
    return x;
  });
  window.Plotly.react(div, traces, lay, {
    responsive: true, displaylogo: false, scrollZoom: true,
    modeBarButtonsToRemove: ['toImage', 'autoScale2d', 'toggleSpikelines', 'hoverClosestCartesian', 'hoverCompareCartesian'],
  });
  if (!div._observe) {
    div._observe = new ResizeObserver(() => { if (div.offsetWidth) window.Plotly.Plots.resize(div); });
    div._observe.observe(div);
    div.addEventListener('contextmenu', (e) => { e.preventDefault(); ctx.menuGraphique(e, div); });
    div.on('plotly_relayout', () => {
      const l = div.layout;
      const zoom = (a) => (l[a] && l[a].autorange === false && l[a].range ? [...l[a].range] : null);
      div._zoom = zoom('xaxis') || zoom('yaxis') ? { x: zoom('xaxis'), y: zoom('yaxis') } : null;
    });
  }
  ctx.definirGraphe(div);
}

// Box / lasso selection and clicks on points whose customdata is the observation uid.
function brancherSelection(div, ctx, idModele) {
  if (div._branche) return;
  div._branche = true;
  const obs = (points) => points.filter((p) => p.data.meta !== 'variables' && p.customdata !== undefined);
  div.on('plotly_selected', (ev) => {
    if (ev) ctx.choisir(idModele, obs(ev.points).map((p) => p.customdata));
  });
  div.on('plotly_click', (ev) => {
    const [p] = obs(ev.points);
    if (!p) return;
    const sel = new Set(ctx.selection(idModele));
    if (sel.has(p.customdata)) sel.delete(p.customdata); else sel.add(p.customdata);
    ctx.choisir(idModele, [...sel]);
  });
}

// Variables picked on one of a model's graphs are highlighted on all of them: a click on a bar,
// point or line (traces whose meta is 'variables', customdata the variable's name), or a box
// on the scatter plots of variables.
function brancherVariables(div, ctx, idModele, { boite = false } = {}) {
  const vars = (points) => points.filter((p) => p.data.meta === 'variables' && p.customdata !== null && p.customdata !== undefined);
  div.on('plotly_click', (ev) => {
    const [p] = vars(ev.points);
    if (!p) return;
    const s = new Set(ctx.variablesChoisies(idModele));
    if (s.has(p.customdata)) s.delete(p.customdata); else s.add(p.customdata);
    ctx.choisirVariables(idModele, [...s]);
  });
  if (boite) div.on('plotly_selected', (ev) => { if (ev) ctx.choisirVariables(idModele, vars(ev.points).map((p) => p.customdata)); });
}

function marqueurs(uids, sel, couleur, taille = 7) {
  const actif = sel.size > 0;
  return {
    color: uids.map((u) => (sel.has(u) ? SELECTION : couleur)),
    size: uids.map((u) => (sel.has(u) ? taille + 2 : taille)),
    opacity: actif ? uids.map((u) => (sel.has(u) ? 1 : 0.3)) : 0.85,
    line: { width: 0 },
  };
}

// Bars of variables: the highlighted ones in the selection colour, the others paler.
function barresVariables(termes, choisies, couleurs) {
  const color = termes.map((x, i) => (choisies.has(x) ? SELECTION : Array.isArray(couleurs) ? couleurs[i] : couleurs));
  return choisies.size ? { color, opacity: termes.map((x) => (choisies.has(x) ? 1 : 0.35)) } : { color };
}

// Marks on the highlighted variables of a plot whose colours already mean something
// (the S-plot's p(corr), the lines of the profiles).
function marquesVariables(x, y, termes, choisies, symbole = 'circle-open', taille = 15) {
  const i = termes.map((_, k) => k).filter((k) => choisies.has(termes[k]) && y[k] !== null);
  return { type: 'scatter', mode: 'markers', x: i.map((k) => x[k]), y: i.map((k) => y[k]), hoverinfo: 'skip', showlegend: false,
    marker: { symbol: symbole, size: taille, color: SELECTION, line: { width: 2, color: SELECTION } } };
}

// An OPLS has m.Ap predictive components (1...Ap), then orthogonal ones.
const libelleComp = (m, a) => (!m.opls ? `${t('comp')} ${a}` : a > (m.Ap || 1) ? `${t('orthogonale')} ${a - (m.Ap || 1)}`
  : (m.Ap || 1) > 1 ? `${t('predictive')} ${a}` : t('predictive'));
const composantes = (m) => Array.from({ length: m.A }, (_, i) => [i + 1, libelleComp(m, i + 1)]);
// Name of component a of a vector (t, p, w*...): t[2], or for an OPLS t[1] (predictive) and to[1]...
export const nomComp = (m, base, a) => (m.opls && a > (m.Ap || 1) ? `${base}o[${a - (m.Ap || 1)}]` : `${base}[${a}]`);
export const estDA = (m) => /-DA$/.test(m.type);

function groupesCouleur(m, loc, ctx) {
  const o = m.observations;
  let cles = null;
  if (loc.couleur === 'classe' && o.classe) cles = o.classe;
  else if (loc.couleur === 'hca' && ctx.groupesHCA[m.id]) cles = o.uid.map((u) => `${t('groupe')} ${ctx.groupesHCA[m.id][u] ?? '?'}`);
  else if (loc.couleur && loc.couleur.startsWith('v')) cles = valeursSource(ctx, m, loc, loc.couleur);
  if (!cles) return null;
  if (cles.every((v) => v === null || typeof v === 'number')) return { continu: cles };
  const g = new Map();
  cles.forEach((c, i) => {
    const k = c === null ? '—' : String(c);
    if (!g.has(k)) g.set(k, []);
    g.get(k).push(i);
  });
  return { groupes: [...g.entries()].sort((a, b) => a[0].localeCompare(b[0], undefined, { numeric: true })) };
}

// Values of a project variable ('v<uid>') or secondary ID ('i<rank>') for the model's
// observations. Fetched from R once, and again when the model's observations change; null
// until they arrive.
function valeursSource(ctx, m, loc, source) {
  const uid = m.observations.uid;
  const cle = uid.join(',');
  loc.cache ??= {};
  if (loc.cache[source]?.cle === cle) return loc.cache[source].valeurs;
  loc.cache[source] = { cle, valeurs: null };
  const k = +source.slice(1);
  ctx.valeurs(m.projet, source[0] === 'v' ? k : null, uid, source[0] === 'i' ? k : undefined).then((r) => {
    loc.cache[source].valeurs = r.valeurs;
    ctx.rafraichir();
  });
  return null;
}

// Colour control shared by the observation plots: none, PLS-DA class, dendrogram groups, or
// any variable of the project.
function controleCouleur(ctx, m, loc) {
  const o = m.observations;
  const proj = ctx.projet(m.projet);
  const options = [['', t('aucune')]].concat(o.classe ? [['classe', t('classe')]] : [])
    .concat(ctx.groupesHCA[m.id] ? [['hca', t('dendrogramme')]] : [])
    .concat((proj ? proj.variables : []).map((v) => ['v' + v.uid, v.nom]));
  return choix(t('couleur'), options, loc.couleur, (v) => { loc.couleur = v; ctx.rafraichir(); });
}

// Label control of the same plots: none, the observation name, a secondary ID of the
// project, or the value of any variable.
function controleEtiquettes(ctx, m, loc) {
  if (loc.etiquettes === true) loc.etiquettes = 'nom';
  const proj = ctx.projet(m.projet);
  const options = [['', t('aucune')], ['nom', t('nom_obs')]]
    .concat((proj?.identifiants || []).map((d) => ['i' + d.rang, d.nom]))
    .concat((proj ? proj.variables : []).map((v) => ['v' + v.uid, v.nom]));
  return choix(t('etiquettes'), options, loc.etiquettes || '', (v) => { loc.etiquettes = v; ctx.rafraichir(); });
}

function textesEtiquettes(ctx, m, loc) {
  const e = loc.etiquettes === true ? 'nom' : loc.etiquettes;
  if (!e) return null;
  if (e === 'nom') return m.observations.nom;
  const v = valeursSource(ctx, m, loc, e);
  return v && v.map((x) => (x === null ? '' : typeof x === 'number' ? String(+x.toPrecision(4)) : x));
}

// Observation markers, one value of xs and ys per observation of the model: one trace per
// colour group (or a colour scale for a numeric variable), selected observations in red,
// optional labels.
function pointsObservations(ctx, m, loc, xs, ys, { type = 'scattergl', survol = '%{x:.3f}, %{y:.3f}' } = {}) {
  const o = m.observations;
  const sel = new Set(ctx.selection(m.id));
  const g = groupesCouleur(m, loc, ctx);
  const textes = textesEtiquettes(ctx, m, loc);
  const point = (idx, nom, couleur) => ({
    type, mode: textes ? 'markers+text' : 'markers', name: nom,
    x: idx.map((i) => xs[i]), y: idx.map((i) => ys[i]), customdata: idx.map((i) => o.uid[i]),
    hovertext: idx.map((i) => o.nom[i]), text: textes ? idx.map((i) => textes[i]) : undefined,
    textposition: 'top center', textfont: { size: 10 },
    hovertemplate: `<b>%{hovertext}</b><br>${survol}<extra>%{fullData.name}</extra>`,
    marker: marqueurs(idx.map((i) => o.uid[i]), sel, couleur),
  });
  if (g && g.groupes) return g.groupes.map(([nom, idx], k) => point(idx, nom, PALETTE[k % PALETTE.length]));
  const trace = point(o.uid.map((_, i) => i), m.nom, PALETTE[0]);
  if (g && g.continu) Object.assign(trace.marker, { color: g.continu, colorscale: 'Viridis', showscale: true });
  return [trace];
}

// Sortable, filterable table. colonnes: [{ cle, titre, num }]; valeur(i, cle); uid(i) links
// rows to the selection of model `idModele`.
// Rows not tied to observations can instead call cliquer(i), with choisie(i) marking the
// current one; a column's alerte(i) shows that cell in red. Cells of `modifiable` columns
// are edited by double-click, then modifier(i, cle, texte) (Enter or leaving the cell: keep;
// Escape: cancel).
function tableau(conteneur, ctx, loc, { colonnes, n, valeur, uid, idModele, nom, avant = [], cliquer, choisie, modifier }) {
  loc.filtre ??= '';
  const f = loc.filtre.toLowerCase();
  let idx = Array.from({ length: n }, (_, i) => i);
  if (f) idx = idx.filter((i) => colonnes.some((c) => String(valeur(i, c.cle) ?? '').toLowerCase().includes(f)));
  if (loc.tri) {
    const { cle, sens } = loc.tri;
    idx.sort((a, b) => {
      const x = valeur(a, cle);
      const y = valeur(b, cle);
      if (x === null || x === undefined) return 1;
      if (y === null || y === undefined) return -1;
      return (typeof x === 'number' && typeof y === 'number' ? x - y : String(x).localeCompare(String(y), undefined, { numeric: true })) * sens;
    });
  }
  const sel = new Set(idModele ? ctx.selection(idModele) : []);
  const max = 2000;
  const cellule = (i, c) => {
    const v = valeur(i, c.cle);
    const td = el('td', { class: (c.num || typeof v === 'number' ? 'num' : '') + (c.alerte && c.alerte(i) ? ' alerte' : '') },
      typeof v === 'number' ? (c.format ? c.format(v) : fmt(v, c.decimales ?? 4)) : v ?? '');
    if (modifier && c.modifiable) {
      td.title = t('double_clic_modifier');
      td.addEventListener('dblclick', (e) => {
        e.stopPropagation();
        const saisie = el('input', { value: v ?? '', class: 'edition-cellule' });
        td.replaceChildren(saisie);
        saisie.focus();
        saisie.select();
        let fini = false;
        const garder = async () => {
          if (fini) return;
          fini = true;
          if (saisie.value !== String(v ?? '')) await modifier(i, c.cle, saisie.value).catch(() => {});
          ctx.rafraichir();
        };
        saisie.addEventListener('keydown', (k) => {
          k.stopPropagation();
          if (k.key === 'Enter') garder();
          else if (k.key === 'Escape') { fini = true; ctx.rafraichir(); }
        });
        saisie.addEventListener('blur', garder);
      });
    }
    return td;
  };
  const tete = el('tr', {}, colonnes.map((c) => el('th', {
    class: (c.num ? 'num' : '') + (loc.tri && loc.tri.cle === c.cle ? ' trie' : ''),
    onclick: () => { loc.tri = { cle: c.cle, sens: loc.tri && loc.tri.cle === c.cle ? -loc.tri.sens : 1 }; ctx.rafraichir(); } },
  c.titre, loc.tri && loc.tri.cle === c.cle ? (loc.tri.sens > 0 ? ' ▲' : ' ▼') : '')));
  const corps = el('tbody', {}, idx.slice(0, max).map((i) => el('tr', {
    class: (uid && sel.has(uid(i))) || (choisie && choisie(i)) ? 'choisie' : '',
    onclick: uid ? () => {
      const s = new Set(ctx.selection(idModele));
      const u = uid(i);
      if (s.has(u)) s.delete(u); else s.add(u);
      ctx.choisir(idModele, [...s]);
    } : cliquer ? () => cliquer(i) : null }, colonnes.map((c) => cellule(i, c)))));
  const copier = () => {
    const txt = [colonnes.map((c) => c.titre).join('\t')]
      .concat(idx.map((i) => colonnes.map((c) => { const v = valeur(i, c.cle); return v === null || v === undefined ? '' : v; }).join('\t'))).join('\n');
    navigator.clipboard.writeText(txt).then(() => ctx.statut(t('table_copiee')));
  };
  conteneur._copierTableau = copier;  // Ctrl+C (app.js)
  const filtre = el('input', { type: 'search', placeholder: t('recherche'), value: loc.filtre, class: 'filtre',
    oninput: (e) => { loc.filtre = e.target.value; clearTimeout(loc.minuterie); loc.minuterie = setTimeout(() => ctx.rafraichir(), 200); } });
  conteneur.replaceChildren(
    outils(...avant, filtre, el('span', { class: 'infos' }, `${idx.length} ${t('lignes')}${idx.length > max ? ` (${max})` : ''}`), espace(),
      el('button', { class: 'icone-seule', title: t('copier_tableau'), onclick: copier }, icone('copy'))),
    el('div', { class: 'defile' }, el('table', { class: 'tableau' }, el('thead', {}, tete), corps)));
  if (loc.focus) { filtre.focus(); filtre.setSelectionRange(filtre.value.length, filtre.value.length); }
  filtre.addEventListener('focus', () => { loc.focus = true; });
  filtre.addEventListener('blur', () => { loc.focus = false; });
}

// ------------------------------------------------------------ views

function rendreResume(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const c = m.composantes;
  const pls = m.type !== 'PCA';
  const cols = [['R2X', 'R2X'], ['R2Xcum', 'R2X(cum)']].concat(pls ? [['R2Y', 'R2Y'], ['R2Ycum', 'R2Y(cum)']] : [])
    .concat([['Q2', 'Q2'], ['Q2cum', 'Q2(cum)'], ['valeur_propre', t('valeur_propre')]]);
  const s = m.stocke;
  const marque = (v, ref) => {
    if (ref === null || ref === undefined) return null;
    const d = Math.abs(v - ref);
    const [cls, txt, titre] = d < 1e-4 ? ['egal', '=', t('egal')] : d < 0.01 ? ['proche', '≈', t('proche')] : ['diff', '≠', t('different')];
    return el('span', { class: 'marque ' + cls, title: `${titre} — ${t('stocke')} ${fmt(ref, 4)}` }, txt);
  };
  const fiche = el('div', { class: 'fiche' },
    el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, m.type), m.titre ? el('span', { class: 'doux' }, m.titre) : null),
    el('div', { class: 'fiche-chiffres' },
      [['A', m.A], ['N', m.N], ['K', m.K], [t('groupes_vc'), m.G]].map(([k, v]) => el('span', {}, el('small', {}, k), ' ', el('b', {}, v))),
      m.reglages && [].concat(m.reglages.spectres || []).length
        ? el('span', {}, el('small', {}, t('spectres')), ' ', el('b', {}, libelleSpectres(m.reglages.spectres))) : null,
      pls && m.RMSEE ? Object.entries(m.RMSEE).map(([y, v]) => el('span', {}, el('small', {}, `RMSEE ${y}`), ' ', el('b', {}, fmt(v, 4)))) : null,
      pls && m.RMSEcv ? Object.entries(m.RMSEcv).map(([y, v]) => el('span', {}, el('small', {}, `RMSEcv ${y}`), ' ', el('b', {}, fmt(v, 4)))) : null,
      pls && m.cv_anova ? Object.entries(m.cv_anova).map(([y, a]) => el('span', { title: 'CV-ANOVA' }, el('small', {}, `CV-ANOVA p ${y}`), ' ',
        el('b', {}, a.p[1] === null ? '—' : a.p[1] < 0.001 ? Number(a.p[1]).toExponential(1) : fmt(a.p[1], 3)))) : null,
      m.classification ? el('span', {}, el('small', {}, t('bien_classes_vc')), ' ', el('b', {}, `${fmt(100 * m.classification.vc.global, 1)} %`)) : null),
    m.test ? el('div', { class: 'fiche-chiffres', title: t('jeu_test_aide') },
      el('span', {}, el('small', {}, t('jeu_test')), ' ', el('b', {}, m.test.n)),
      m.test.statistiques ? [].concat(m.test.statistiques.y).map((y, j) => [['RMSEP', 'RMSEP', 4], ['biais', t('biais'), 4], ['Q2ext', 'Q2ext', 3]]
        .map(([k, n, d]) => el('span', {}, el('small', {}, `${n}${[].concat(m.test.statistiques.y).length > 1 ? ` ${y}` : ''}`), ' ', el('b', {}, fmt([].concat(m.test.statistiques[k])[j], d))))) : null,
      m.test.classement ? el('span', {}, el('small', {}, t('bien_classes_test')), ' ', el('b', {}, `${fmt(100 * m.test.classement.bien_classees, 1)} %`)) : null,
      el('span', {}, el('small', {}, t('hors_domaine_court')), ' ', el('b', {}, m.test.hors_domaine))) : null);
  const table = el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, el('th', {}, t('comp')), cols.map(([, n]) => el('th', { class: 'num' }, n)))),
    el('tbody', {}, c.composante.map((a, i) => el('tr', {}, el('td', {}, m.opls ? (a > (m.Ap || 1) ? `O${a - (m.Ap || 1)}` : (m.Ap || 1) > 1 ? `P${a}` : 'P') : a),
      cols.map(([k]) => el('td', { class: 'num' }, fmt(c[k][i]), s && s[k] && k !== 'valeur_propre' ? marque(c[k][i], s[k][i]) : null))))));
  const note = s ? el('p', { class: 'note' }, el('span', { class: 'marque egal' }, '='), ' ', el('span', { class: 'marque proche' }, '≈'), ' ',
    el('span', { class: 'marque diff' }, '≠'), ' ', t('valeurs_fichier'), '. ' + t('note_q2')) : null;
  const graphe = el('div', { class: 'graphe petit' });
  // Notes on the model, saved with its definition in the project (while typing, a redraw keeps
  // the text and the cursor)
  const avant = conteneur.querySelector('.notes-modele');
  const enCours = avant && document.activeElement === avant ? [avant.value, avant.selectionStart, avant.selectionEnd] : null;
  const notes = el('textarea', { class: 'notes-modele', rows: 2, placeholder: t('notes_placeholder'), oninput: (e) => {
    clearTimeout(conteneur._minuterieNotes);
    const texte = e.target.value;
    conteneur._minuterieNotes = setTimeout(() => ctx.noterModele(m.id, texte), 700);
  } }, enCours ? enCours[0] : m.note || '');
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes, table, note, graphe));
  if (enCours) { notes.focus(); notes.setSelectionRange(enCours[1], enCours[2]); }
  const x = c.composante.map((a) => libelleComp(m, a));
  tracer(ctx, graphe, [
    { type: 'bar', name: pls ? 'R2Y(cum)' : 'R2X(cum)', x, y: pls ? c.R2Ycum : c.R2Xcum, marker: { color: '#3b9a5b' } },
    { type: 'bar', name: 'Q2(cum)', x, y: c.Q2cum, marker: { color: '#2f6fb5' } }],
  { barmode: 'group', yaxis: { range: [Math.min(0, ...c.Q2cum.filter((v) => v !== null)), 1] }, dragmode: false, margin: { t: 10, b: 40 } });
}

function rendreScores(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.a ??= 1;
  loc.b ??= Math.min(2, m.A);
  loc.couleur ??= estDA(m) ? 'classe' : '';
  const o = m.observations;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix(t('axe_x'), composantes(m), loc.a, (v) => { loc.a = +v; ctx.rafraichir(); }),
    m.A > 1 ? choix(t('axe_y'), composantes(m), loc.b, (v) => { loc.b = +v; ctx.rafraichir(); }) : null,
    controleCouleur(ctx, m, loc),
    controleEtiquettes(ctx, m, loc)), graphe);
  const un = m.A === 1 || loc.a === loc.b;
  const xs = un ? o.uid.map((_, i) => i + 1) : o['t' + loc.a];
  const ys = un ? o['t' + loc.a] : o['t' + loc.b];
  const traces = pointsObservations(ctx, m, loc, xs, ys);
  const formes = [];
  if (!un) {  // Hotelling T² 95 % ellipse
    const rx = Math.sqrt(m.variance_t[loc.a - 1] * m.ellipse['95']);
    const ry = Math.sqrt(m.variance_t[loc.b - 1] * m.ellipse['95']);
    formes.push({ type: 'circle', x0: -rx, x1: rx, y0: -ry, y1: ry, line: { color: '#888', width: 1, dash: 'dot' } });
  }
  const r2 = (a) => `R2X[${a}] = ${fmt(m.composantes.R2X[a - 1], 3)}`;
  tracer(ctx, graphe, traces, {
    shapes: formes, title: { text: `${m.nom} · ${t('scores')}` },
    xaxis: { title: { text: un ? 'observation' : `${nomComp(m, 't', loc.a)}  (${r2(loc.a)})` } },
    yaxis: { title: { text: nomComp(m, 't', un ? loc.a : loc.b) + (un ? '' : `  (${r2(loc.b)})`) } } });
  brancherSelection(graphe, ctx, m.id);
}

function rendreLoadings(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.a ??= 1;
  loc.b ??= Math.min(2, m.A);
  const v = m.variables;
  const pls = m.type !== 'PCA';
  const cle = pls ? 'w*' : 'p';
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix(t('axe_x'), composantes(m), loc.a, (x) => { loc.a = +x; ctx.rafraichir(); }),
    m.A > 1 ? choix(t('axe_y'), composantes(m), loc.b, (x) => { loc.b = +x; ctx.rafraichir(); }) : null,
    case_(t('etiquettes'), loc.etiquettes ?? v.terme.length <= ETIQUETTES_MAX, (x) => { loc.etiquettes = x; ctx.rafraichir(); })), graphe);
  const etiq = loc.etiquettes ?? v.terme.length <= ETIQUETTES_MAX;
  const un = m.A === 1 || loc.a === loc.b;
  const choisies = new Set(ctx.variablesChoisies(m.id));
  const traces = [];
  if (un) {
    traces.push({ type: 'bar', name: 'X', x: v.terme, y: v[cle + loc.a], meta: 'variables', customdata: v.terme, marker: barresVariables(v.terme, choisies, PALETTE[0]),
      error_y: v[cle + loc.a + '_IC'] ? { type: 'data', array: v[cle + loc.a + '_IC'], visible: true, color: '#999', thickness: 1 } : undefined });
  } else {
    traces.push({ type: 'scatter', mode: etiq ? 'markers+text' : 'markers', name: 'X', x: v[cle + loc.a], y: v[cle + loc.b], text: v.terme,
      meta: 'variables', customdata: v.terme, textposition: 'top center', textfont: { size: 10 }, marker: marqueurs(v.terme, choisies, PALETTE[0]),
      hovertemplate: '<b>%{text}</b><br>%{x:.3f}, %{y:.3f}<extra></extra>' });
    if (pls && m.variables_y) {
      const y = m.variables_y;
      traces.push({ type: 'scatter', mode: etiq ? 'markers+text' : 'markers', name: 'Y', x: y['c' + loc.a], y: y['c' + loc.b], text: y.terme,
        meta: 'variables', customdata: y.terme, textposition: 'top center', textfont: { size: 10 }, marker: { ...marqueurs(y.terme, choisies, PALETTE[1], 9), symbol: 'diamond' },
        hovertemplate: '<b>%{text}</b><br>%{x:.3f}, %{y:.3f}<extra></extra>' });
    }
  }
  const nom = pls ? (un ? 'w*' : 'w*c') : 'p';
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${t('loadings')} ${nom}` }, dragmode: un ? 'pan' : 'select',
    xaxis: { title: { text: un ? '' : nomComp(m, nom, loc.a) } }, yaxis: { title: { text: nomComp(m, nom, un ? loc.a : loc.b) } } });
  brancherVariables(graphe, ctx, m.id, { boite: !un });
}

// Biplot: scores t(corr) and loadings p(corr) (pc(corr) with the Y of a
// PLS) in one plot, inside the correlation circle; computed in R.
// The steps of a model's spectral preprocessing, for the summary: "SNV → 1st derivative 11/2".
export const libelleSpectres = (etapes) => [].concat(etapes || []).map((e) => (e.type === 'sg'
  ? `${t('etape_sg' + (e.derivee || 0))} ${e.fenetre}/${e.degre}` : t('etape_' + e.type))).join(' → ');

// Position of each variable along the X axis of a line plot: the number in its name (a
// wavelength, "1450 nm") when every name has a different one, its rank otherwise.
function abscisses(noms) {
  const n = noms.map((s) => { const r = /-?\d+(?:[.,]\d+)?/.exec(String(s)); return r ? parseFloat(r[0].replace(',', '.')) : NaN; });
  return n.every(Number.isFinite) && new Set(n).size === n.length ? n : noms.map((_, i) => i + 1);
}

// Where a line plot of variables breaks: after a jump of the abscissa more than 1.5 times the
// usual step (variables left out of the model). couper() puts a gap (null) after those points.
function coupures(x) {
  const pas = x.slice(1).map((v, k) => Math.abs(v - x[k]));
  const median = [...pas].sort((a, b) => a - b)[Math.floor(pas.length / 2)] || 0;
  return new Set(pas.map((d, k) => (d > 1.5 * median ? k : -1)).filter((k) => k >= 0));
}
const couper = (valeurs, coupe) => valeurs.flatMap((v, k) => (coupe.has(k) ? [v, null] : [v]));

// Variables in the order of the table, as lines (spectra): the model's loadings, coefficients
// or VIP, or the spectra themselves, as read and after their preprocessing. Clicking a
// spectrum selects its observation. "Choose ranges": dragging marks wavelength ranges, for a
// new model without them or with them only.
async function rendreProfils(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const v = m.variables;
  const pls = m.type !== 'PCA';
  const quoi = [['p', 'Loadings p']].concat(pls ? [['w', 'Loadings w*'], ['coeff', t('coefficients')], ['vip', 'VIP']] : [])
    .concat([['spectres', t('profil_spectres')]]);
  if (!quoi.some(([k]) => k === loc.quoi)) loc.quoi = 'p';
  if (loc.quoi === 'spectres' && (!loc.res || loc.cle !== m.id)) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('spectres', { modele: m.id });
    loc.cle = m.id;
  }
  const graphe = el('div', { class: 'graphe' });
  const avecEtapes = m.reglages && [].concat(m.reglages.spectres || []).length > 0;
  loc.zones ??= [];
  const dansZones = (a) => loc.zones.some(([z0, z1]) => a >= z0 && a <= z1);
  const affichees = abscisses(loc.quoi === 'spectres' ? loc.res.noms : v.terme);
  // the spectrum's X variables outside the ranges (garder = false) or inside them; the model's other X stay
  const modeleZones = async (garder) => {
    const r = loc.res && loc.cle === m.id ? loc.res : await ctx.demander('spectres', { modele: m.id });
    const ax = abscisses(r.noms);
    const spectre = [].concat(r.variables);
    const retenues = spectre.filter((_, i) => dansZones(ax[i]) === garder);
    if (!retenues.length) { ctx.statut(t('zones_vides'), true); return; }
    ctx.nouveauModeleDepuis(m.id, { ...m.reglages, x: [...[].concat(m.reglages.x || []).filter((u) => !spectre.includes(u)), ...retenues] });
  };
  conteneur.replaceChildren(outils(
    choix(t('profil_quoi'), quoi, loc.quoi, (x) => { loc.quoi = x; ctx.rafraichir(); }),
    loc.quoi === 'spectres' && avecEtapes ? case_(t('spectres_bruts'), !!loc.bruts, (x) => { loc.bruts = x; ctx.rafraichir(); }) : null,
    avecEtapes ? el('span', { class: 'infos' }, libelleSpectres(m.reglages.spectres)) : null,
    case_(t('choisir_zones'), !!loc.choixZones, (x) => { loc.choixZones = x; ctx.rafraichir(); }),
    loc.choixZones && !loc.zones.length ? el('span', { class: 'infos' }, t('zones_aide')) : null,
    loc.zones.length ? [el('span', { class: 'infos' }, t('zones_n', { n: loc.zones.length, k: affichees.filter(dansZones).length })),
      el('button', { onclick: () => modeleZones(false) }, t('modele_sans_zones')),
      el('button', { onclick: () => modeleZones(true) }, t('modele_zones')),
      el('button', { onclick: () => { loc.zones = []; ctx.rafraichir(); } }, t('effacer_zones'))] : null), graphe);
  const traces = [];
  const marques = [];  // the highlighted variables: on the lines, or a dotted line across the spectra
  const choisies = new Set(ctx.variablesChoisies(m.id));
  let titre = '';
  if (loc.quoi === 'spectres') {
    const r = loc.res;
    const x = abscisses(r.noms);
    [].concat(r.noms).forEach((n, k) => { if (choisies.has(n)) marques.push({ type: 'line', xref: 'x', yref: 'paper', x0: x[k], x1: x[k], y0: 0, y1: 1, line: { color: SELECTION, width: 1, dash: 'dot' } }); });
    const coupe = coupures(x);
    const lignes = loc.bruts ? r.bruts : r.pretraites;
    const nomObs = new Map(m.observations.uid.map((u, i) => [u, m.observations.nom[i]]));
    const sel = new Set(ctx.selection(m.id));
    // one trace per group (spectra separated by gaps): fast with hundreds of spectra
    const groupe = (indices, couleur, largeur, nom) => {
      const xs = []; const ys = []; const txt = []; const ids = [];
      for (const i of indices) {
        lignes[i].forEach((y, k) => {
          xs.push(x[k]); ys.push(y); txt.push(nomObs.get(r.uid[i])); ids.push(r.uid[i]);
          if (coupe.has(k)) { xs.push(null); ys.push(null); txt.push(''); ids.push(null); }
        });
        xs.push(null); ys.push(null); txt.push(''); ids.push(null);
      }
      return { type: 'scattergl', mode: 'lines', name: nom, x: xs, y: ys, text: txt, customdata: ids, line: { color: couleur, width: largeur },
        hovertemplate: '<b>%{text}</b><br>%{x}: %{y:.4g}<extra></extra>' };
    };
    const tous = r.uid.map((_, i) => i);
    traces.push(groupe(tous.filter((i) => !sel.has(r.uid[i])), sel.size ? '#b8c2cf' : PALETTE[0], 1, t('observations')));
    if (sel.size) traces.push(groupe(tous.filter((i) => sel.has(r.uid[i])), SELECTION, 1.6, t('selection')));
    titre = `${m.nom} · ${t('profil_spectres')}${avecEtapes ? ` (${loc.bruts ? t('spectres_bruts').toLowerCase() : libelleSpectres(m.reglages.spectres)})` : ''}`;
  } else {
    const x = abscisses(v.terme);
    const mode = v.terme.length <= 40 ? 'lines+markers' : 'lines';
    const ligne = (y, nom, i, xs = x, termes = v.terme) => {
      const coupe = coupures(xs);
      if (choisies.size) marques.push(marquesVariables(xs, y, termes, choisies, 'circle', 8));
      return { type: 'scatter', mode, name: nom, x: couper(xs, coupe), y: couper(y, coupe), text: couper(termes, coupe),
        meta: 'variables', customdata: couper(termes, coupe),
        line: { color: PALETTE[i % PALETTE.length], width: 1.5 }, marker: { size: 5 }, hovertemplate: `<b>%{text}</b><br>${nom} %{y:.4g}<extra></extra>` };
    };
    if (loc.quoi === 'p' || loc.quoi === 'w') {
      const base = loc.quoi === 'p' ? 'p' : 'w*';
      for (let a = 1; a <= m.A; a += 1) traces.push(ligne(v[base + a], nomComp(m, base, a), a - 1));
      titre = `${m.nom} · Loadings ${base}`;
    } else if (loc.quoi === 'coeff') {
      const c = m.coefficients;
      [].concat(m.noms_y).forEach((y, i) => { if (c[`CoeffCS ${y}`]) traces.push(ligne(c[`CoeffCS ${y}`], y, i, abscisses(c.terme), c.terme)); });
      titre = `${m.nom} · ${t('coefficients')} (CoeffCS)`;
    } else {
      traces.push(ligne(v.VIP, 'VIP', 0));
      titre = `${m.nom} · VIP`;
    }
  }
  const lignes = traces.length;
  traces.push(...marques.filter((x) => x.type === 'scatter'));
  tracer(ctx, graphe, traces, { title: { text: titre }, dragmode: loc.choixZones ? 'select' : 'zoom', selectdirection: 'h', showlegend: lignes > 1,
    xaxis: { title: { text: t('variables') } },
    shapes: (loc.quoi === 'vip' ? [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: 1, y1: 1, line: { color: '#999', dash: 'dot', width: 1 } }] : [])
      .concat(loc.zones.map(([z0, z1]) => ({ type: 'rect', xref: 'x', yref: 'paper', x0: z0, x1: z1, y0: 0, y1: 1, layer: 'below',
        fillcolor: 'rgba(242, 142, 43, 0.18)', line: { width: 0 } })))
      .concat(marques.filter((x) => x.type === 'line')) });
  if (loc.quoi !== 'spectres') brancherVariables(graphe, ctx, m.id);
  graphe.on('plotly_selected', (ev) => {  // a range dragged (lines have no points to select: the box itself)
    if (!loc.choixZones) return;
    const boite = (graphe.layout.selections || []).slice(-1)[0];
    const s = (ev && ev.range && ev.range.x) || (boite && [boite.x0, boite.x1]);
    if (!s) return;
    loc.zones.push([Math.min(...s), Math.max(...s)]);
    ctx.rafraichir();
  });
  if (loc.quoi === 'spectres' && !graphe._clicSpectre) {
    graphe._clicSpectre = true;
    graphe.on('plotly_click', (ev) => {
      const u = ev.points[0] && ev.points[0].customdata;
      if (u !== null && u !== undefined) ctx.choisir(m.id, [u]);
    });
  }
}

// iPLS (Nørgaard et al. 2000): the spectrum preprocessed once, whole, then cut into intervals,
// each a PLS on its own (ipls.R). A bar per interval: its smallest RMSEcv, with the number of
// components that gives it; dashed line: the whole spectrum; grey line: the mean spectrum after
// preprocessing. Clicked bars make a new model with those intervals only, or without them.
async function rendreIPLS(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.n ??= 20;
  loc.A ??= m.A;
  const cle = `${m.id}:${loc.n}:${loc.A}`;
  if (!loc.res || loc.cle !== cle) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('ipls', { modele: m.id, n: loc.n, A: loc.A });
    loc.cle = cle;
    loc.choisis = [];
  }
  const r = loc.res;
  const iv = [].concat(r.intervalles);
  const choisis = new Set(loc.choisis || []);
  const spectre = [].concat(r.spectre);
  const x = abscisses(spectre);
  const position = new Map(spectre.map((n, k) => [n, x[k]]));
  const pas = Math.abs(x[Math.min(1, x.length - 1)] - x[0]) || 1;
  const bornes = iv.map((z) => { const a = [].concat(z.termes).map((n) => position.get(n)); return [Math.min(...a), Math.max(...a)]; });
  const meilleur = (z) => { const v = [].concat(z.rmsecv).filter(Number.isFinite); return v.length ? Math.min(...v) : null; };  // null: the interval could not be fitted
  const complet = meilleur(r.complet);
  const nouveau = (avec) => {  // the spectrum's variables in the chosen intervals (avec) or outside them; the model's other X stay
    const toutes = iv.flatMap((z) => [].concat(z.variables));
    const dans = new Set(iv.filter((_, i) => choisis.has(i)).flatMap((z) => [].concat(z.variables)));
    const retenues = toutes.filter((u) => dans.has(u) === avec);
    if (!retenues.length) { ctx.statut(t('zones_vides'), true); return; }
    ctx.nouveauModeleDepuis(m.id, { ...m.reglages, x: [...[].concat(m.reglages.x || []).filter((u) => !toutes.includes(u)), ...retenues] });
  };
  const nombre = (libelle, valeur, min, max, changer) => el('label', { class: 'champ' }, el('span', {}, libelle),
    el('input', { type: 'number', min, max, step: 1, value: valeur, style: 'width:4.5em',
      onchange: (e) => { const v = Math.round(+e.target.value); if (v >= min && v <= max) changer(v); } }));
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    nombre(t('ipls_intervalles'), loc.n, 2, Math.min(100, spectre.length), (v) => { loc.n = v; ctx.rafraichir(); }),
    nombre(t('ipls_composantes'), loc.A, 1, 15, (v) => { loc.A = v; ctx.rafraichir(); }),
    el('span', { class: 'infos' }, t('ipls_complet', { r: fmt(complet, 4), a: r.complet.A })),
    iv.some((z) => meilleur(z) === null) ? el('span', { class: 'infos' }, t('ipls_vides', { n: iv.filter((z) => meilleur(z) === null).length })) : null,
    choisis.size ? [el('span', { class: 'infos' }, t('ipls_choisis', { n: choisis.size })),
      el('button', { onclick: () => nouveau(true) }, t('modele_intervalles')),
      el('button', { onclick: () => nouveau(false) }, t('modele_sans_intervalles')),
      el('button', { onclick: () => { loc.choisis = []; ctx.rafraichir(); } }, t('effacer_zones'))] : null),
  el('p', { class: 'note' }, t('ipls_aide')), graphe);
  const c = couleursTheme();
  const nomIntervalle = (i) => `${[].concat(iv[i].termes)[0]} – ${[].concat(iv[i].termes).slice(-1)[0]}`;
  const choisies = new Set(ctx.variablesChoisies(m.id));
  tracer(ctx, graphe, [
    { type: 'bar', x: bornes.map(([a, b]) => (a + b) / 2), y: iv.map(meilleur), width: bornes.map(([a, b]) => b - a + pas), customdata: iv.map((_, i) => i),
      text: iv.map((z) => String(z.A)), textposition: 'inside', insidetextanchor: 'start', textfont: { size: 10, color: '#fff' },
      hovertext: iv.map((z, i) => `${nomIntervalle(i)}<br>RMSEcv ${fmt(meilleur(z), 4)} (${z.A} ${t('comp').toLowerCase()})`),
      hovertemplate: '%{hovertext}<extra></extra>',
      marker: { color: iv.map((z, i) => (choisis.has(i) ? SELECTION : meilleur(z) < complet ? '#3b9a5b' : PALETTE[0])), line: { color: c.fond, width: 1 } } },
    { type: 'scatter', mode: 'lines', x, y: [].concat(r.moyen), yaxis: 'y2', line: { color: c.doux, width: 1 }, hoverinfo: 'skip' }],
  { title: { text: `${m.nom} · iPLS` }, dragmode: 'zoom', showlegend: false, bargap: 0,
    xaxis: { title: { text: t('variables') } }, yaxis: { title: { text: 'RMSEcv' }, rangemode: 'tozero' },
    yaxis2: { overlaying: 'y', side: 'right', showgrid: false, showticklabels: false, zeroline: false },
    shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: complet, y1: complet, line: { color: '#d1495b', dash: 'dash', width: 1.5 } }]
      .concat(spectre.filter((n) => choisies.has(n)).map((n) => ({ type: 'line', xref: 'x', yref: 'paper', x0: position.get(n), x1: position.get(n), y0: 0, y1: 1,
        line: { color: SELECTION, width: 1, dash: 'dot' } }))) });
  graphe.on('plotly_click', (ev) => {
    const pt = ev.points.find((q) => q.data.type === 'bar');
    if (!pt) return;
    if (choisis.has(pt.customdata)) choisis.delete(pt.customdata); else choisis.add(pt.customdata);
    loc.choisis = [...choisis];
    ctx.rafraichir();
  });
}

async function rendreBiplot(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  if (m.A < 2) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('deux_composantes'))); return; }
  loc.a ??= 1;
  loc.b ??= 2;
  loc.couleur ??= estDA(m) ? 'classe' : '';
  if (!loc.res || loc.cle !== m.id) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('biplot', { modele: m.id });
    loc.cle = m.id;
  }
  const r = loc.res;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix(t('axe_x'), composantes(m), loc.a, (v) => { loc.a = +v; ctx.rafraichir(); }),
    choix(t('axe_y'), composantes(m), loc.b, (v) => { loc.b = +v; ctx.rafraichir(); }),
    controleCouleur(ctx, m, loc),
    controleEtiquettes(ctx, m, loc),
    case_(t('noms_variables'), loc.nomsVar ?? true, (v) => { loc.nomsVar = v; ctx.rafraichir(); })), graphe);
  const a = loc.a - 1;
  const b = loc.b - 1;
  const traces = pointsObservations(ctx, m, loc, r.t[a], r.t[b]);
  const nomsVar = loc.nomsVar ?? true;
  const choisies = new Set(ctx.variablesChoisies(m.id));
  const variables = (vals, noms, nom, couleur, symbole) => ({ type: 'scatter', mode: nomsVar ? 'markers+text' : 'markers', name: nom,
    x: vals[a], y: vals[b], text: [].concat(noms), meta: 'variables', customdata: [].concat(noms), textposition: 'top center', textfont: { size: 10, color: couleur },
    marker: { ...marqueurs([].concat(noms), choisies, couleur, 8), symbol: symbole }, hovertemplate: '<b>%{text}</b><br>%{x:.3f}, %{y:.3f}<extra></extra>' });
  traces.push(variables(r.p, r.noms_x, 'X', '#2a9d8f', 'triangle-up'));
  if (r.c) traces.push(variables(r.c, r.noms_y, 'Y', '#d1495b', 'diamond'));
  const cercle = (rayon) => ({ type: 'circle', x0: -rayon, x1: rayon, y0: -rayon, y1: rayon, line: { color: '#888', width: 1, dash: rayon < 1 ? 'dot' : 'solid' } });
  const nom = m.type === 'PCA' ? 'p(corr)' : 'pc(corr)';
  tracer(ctx, graphe, traces, { shapes: [cercle(1), cercle(0.5)], title: { text: `${m.nom} · Biplot t(corr) / ${nom}` },
    xaxis: { title: { text: `[${loc.a}]` }, range: [-1.1, 1.1], scaleanchor: 'y' }, yaxis: { title: { text: `[${loc.b}]` }, range: [-1.1, 1.1] } });
  brancherSelection(graphe, ctx, m.id);
  brancherVariables(graphe, ctx, m.id, { boite: true });
}

// OPLS S-plot (Wiklund et al. 2008): for each X variable, its predictive
// loading p[1] (size of its link with Y) against p(corr)[1] (its reliability, between -1 and 1).
// Variables far out at both ends are the ones that separate the classes or drive Y.
async function rendreSplot(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  if (!loc.res || loc.cle !== m.id) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('biplot', { modele: m.id });
    loc.cle = m.id;
  }
  const v = m.variables;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(case_(t('etiquettes'), loc.etiquettes ?? v.terme.length <= ETIQUETTES_MAX, (x) => { loc.etiquettes = x; ctx.rafraichir(); })), graphe);
  const pc = loc.res.p[0];
  const choisies = new Set(ctx.variablesChoisies(m.id));
  tracer(ctx, graphe, [{ type: 'scatter', mode: (loc.etiquettes ?? v.terme.length <= ETIQUETTES_MAX) ? 'markers+text' : 'markers', name: 'X', x: v.p1, y: pc, text: v.terme,
    meta: 'variables', customdata: v.terme, textposition: 'top center', textfont: { size: 10 },
    marker: { color: pc.map((r) => r), colorscale: 'RdBu', reversescale: true, cmin: -1, cmax: 1, size: 8,
      ...(choisies.size ? { opacity: v.terme.map((x) => (choisies.has(x) ? 1 : 0.35)) } : {}) },
    hovertemplate: '<b>%{text}</b><br>p[1] %{x:.3f}<br>p(corr)[1] %{y:.3f}<extra></extra>' }, marquesVariables(v.p1, pc, v.terme, choisies)],
  { title: { text: `${m.nom} · S-plot` }, dragmode: 'select', showlegend: false,
    xaxis: { title: { text: 'p[1]' } }, yaxis: { title: { text: 'p(corr)[1]' }, range: [-1.05, 1.05] } });
  brancherVariables(graphe, ctx, m.id, { boite: true });
}

// Scores in three dimensions (turn with the mouse); a click picks or drops an observation.
function rendreScores3D(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  if (m.A < 3) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('trois_composantes'))); return; }
  loc.a ??= 1;
  loc.b ??= 2;
  loc.c ??= 3;
  loc.couleur ??= estDA(m) ? 'classe' : '';
  const o = m.observations;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    ...['a', 'b', 'c'].map((k) => choix(k.toUpperCase(), composantes(m), loc[k], (v) => { loc[k] = +v; ctx.rafraichir(); })),
    controleCouleur(ctx, m, loc),
    controleEtiquettes(ctx, m, loc)), graphe);
  const sel = new Set(ctx.selection(m.id));
  const g = groupesCouleur(m, loc, ctx);
  const textes = textesEtiquettes(ctx, m, loc);
  const cols = ['a', 'b', 'c'].map((k) => o['t' + loc[k]]);
  const trace = (idx, nom, couleur) => ({ type: 'scatter3d', mode: textes ? 'markers+text' : 'markers', name: nom,
    x: idx.map((i) => cols[0][i]), y: idx.map((i) => cols[1][i]), z: idx.map((i) => cols[2][i]),
    customdata: idx.map((i) => o.uid[i]), hovertext: idx.map((i) => o.nom[i]), text: textes ? idx.map((i) => textes[i]) : undefined,
    textfont: { size: 9 }, hovertemplate: '<b>%{hovertext}</b><extra>%{fullData.name}</extra>',
    marker: { size: idx.map((i) => (sel.has(o.uid[i]) ? 6 : 3.5)), color: idx.map((i) => (sel.has(o.uid[i]) ? SELECTION : couleur)), opacity: 0.9, line: { width: 0 } } });
  let traces;
  if (g && g.groupes) traces = g.groupes.map(([nom, idx], k) => trace(idx, nom, PALETTE[k % PALETTE.length]));
  else {
    traces = [trace(o.uid.map((_, i) => i), m.nom, PALETTE[0])];
    if (g && g.continu) Object.assign(traces[0].marker, { color: g.continu.map((v, i) => (sel.has(o.uid[i]) ? null : v)), colorscale: 'Viridis', showscale: true });
  }
  const c = couleursTheme();
  const axe = (k) => ({ title: { text: nomComp(m, 't', loc[k]) }, gridcolor: c.grille, zerolinecolor: c.doux, backgroundcolor: c.fond, color: c.texte });
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${t('scores_3d')}` }, margin: { l: 0, r: 0, t: 34, b: 0 },
    scene: { xaxis: axe('a'), yaxis: axe('b'), zaxis: axe('c') } });
  brancherSelection(graphe, ctx, m.id);
}

function rendreLigneObs(conteneur, ctx, p, loc, vue) {
  const m = ctx.modele(p.modele);
  const o = m.observations;
  const recent = m.version_origine === null || m.version_origine === undefined || m.version_origine >= 13;
  loc.pondere ??= recent && o.DModX_pondere !== undefined;
  const cle = vue === 't2' ? 'T2' : loc.pondere ? 'DModX_pondere' : 'DModX';
  loc.couleur ??= estDA(m) ? 'classe' : '';
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    vue === 'dmodx' ? choix('', [['1', t('ponderee')], ['0', t('normale')]], loc.pondere ? '1' : '0', (v) => { loc.pondere = v === '1'; ctx.rafraichir(); }) : null,
    controleCouleur(ctx, m, loc),
    controleEtiquettes(ctx, m, loc),
    vue === 't2' ? el('span', { class: 'infos' }, `T²Crit 95 % = ${fmt(m.limites['T2Crit(95%)'], 3)} · 99 % = ${fmt(m.limites['T2Crit(99%)'], 3)}`)
      : el('span', { class: 'infos' }, `DCrit (0,05) = ${fmt(m.limites['DCrit(5%)'], 3)}`)), graphe);
  const x = o.uid.map((_, i) => i + 1);
  const c = couleursTheme();
  const points = pointsObservations(ctx, m, loc, x, o[cle], { type: 'scatter', survol: '%{y:.3f}' });
  const traces = [{ type: 'scatter', mode: 'lines', x, y: o[cle], line: { color: c.ligne, width: 1 }, hoverinfo: 'skip', showlegend: false }, ...points];
  const lim = vue === 't2' ? [['T2Crit(95%)', '#e09f3e'], ['T2Crit(99%)', '#d1495b']] : [['DCrit(5%)', '#d1495b']];
  const presentes = lim.filter(([k]) => m.limites[k] !== undefined);
  tracer(ctx, graphe, traces, {
    showlegend: points.length > 1,
    shapes: presentes.map(([k, col]) => ({ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: m.limites[k], y1: m.limites[k], line: { color: col, width: 1.5, dash: 'dash' } })),
    annotations: presentes.map(([k, col]) => ({ xref: 'paper', x: 1, y: m.limites[k], text: `${k.replace('(5%)', '')} ${fmt(m.limites[k], 3)}`, showarrow: false, xanchor: 'right', yanchor: 'bottom', font: { color: col, size: 11 } })),
    title: { text: `${m.nom} · ${vue === 't2' ? t('t2') : 'DModX' + (loc.pondere ? ` (${t('ponderee').split(' ')[0]})` : '')}` },
    xaxis: { title: { text: 'observation' } }, yaxis: { title: { text: cle.replace('_pondere', '') }, rangemode: 'tozero' } });
  brancherSelection(graphe, ctx, m.id);
}

// VIP sorted in decreasing order, with jack-knife intervals, the usual VIP = 1 line, the
// 0.8 rule of thumb, and the cut-off: by default at the largest step near 0.8 (computed in
// R); typed in, or dragged with the red line, otherwise. Clicking a bar keeps or drops that
// variable whatever the cut-off. The variables kept feed "Refit with VIP variables".
function rendreVIP(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const v = m.variables;
  const sv = m.seuil_vip || { seuil: 0.8 };
  loc.forces ??= {};
  const auto = loc.seuil === null || loc.seuil === undefined;
  const seuil = auto ? sv.seuil : loc.seuil;
  const garde = (i) => (v.terme[i] in loc.forces ? loc.forces[v.terme[i]] : v.VIP[i] >= seuil);
  const ordre = v.terme.map((_, i) => i).sort((a, b) => v.VIP[b] - v.VIP[a]);
  const retenues = ordre.filter(garde).map((i) => v.terme[i]);
  const nForces = Object.keys(loc.forces).length;
  ctx.definirChoixVIP(m.id, retenues);
  const graphe = el('div', { class: 'graphe' });
  const origine = auto ? (sv.chute ? `${t('chute')} ${fmt(sv.chute, 2)}${sv.marche ? '' : ', ' + t('marche_faible')}` : t('auto_court')) : t('seuil_choisi');
  conteneur.replaceChildren(outils(
    el('label', { class: 'champ' }, el('span', {}, t('seuil_vip')), el('input', { type: 'number', step: 0.05, min: 0, value: fmt(seuil, 2), style: 'width:5.5em',
      onchange: (e) => { const x = parseFloat(String(e.target.value).replace(',', '.')); if (Number.isFinite(x)) { loc.seuil = x; ctx.rafraichir(); } } })),
    el('button', { title: t('seuil_auto'), disabled: auto && !nForces, onclick: () => { loc.seuil = null; loc.forces = {}; ctx.rafraichir(); } }, icone('rotate-ccw'), t('auto_court')),
    el('span', { class: 'infos' }, `(${origine}) · ${retenues.length} / ${v.terme.length} ${t('vip_retenues')}${nForces ? ` · ${nForces} ${t('choisies_main')}` : ''}`),
    espace(),
    el('button', { onclick: () => ctx.reajuster(m.id, { garder: retenues }), disabled: !retenues.length }, icone('filter'), t('reajuster_vip'))),
  el('p', { class: 'note' }, t('vip_aide'), ' ', t('vip_note')), graphe);
  const ligne = (y, couleur, dash, editable = false) => ({ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: y, y1: y, editable,
    line: { color: couleur, dash, width: editable ? 2.5 : 1 } });
  const termes = ordre.map((i) => v.terme[i]);
  tracer(ctx, graphe, [{ type: 'bar', x: termes, y: ordre.map((i) => v.VIP[i]), customdata: termes,
    marker: { ...barresVariables(termes, new Set(ctx.variablesChoisies(m.id)), ordre.map((i) => (garde(i) ? PALETTE[0] : '#b8c2cf'))),
      line: { color: ordre.map((i) => (v.terme[i] in loc.forces ? '#d1495b' : 'rgba(0,0,0,0)')), width: 2 } },
    error_y: v.VIP_IC ? { type: 'data', array: ordre.map((i) => v.VIP_IC[i]), visible: true, color: '#888', thickness: 1 } : undefined,
    hovertemplate: '<b>%{x}</b><br>VIP %{y:.3f}<extra></extra>' }],
  { title: { text: `${m.nom} · VIP` }, dragmode: false, showlegend: false,
    shapes: [ligne(1, '#999', 'dot'), ligne(0.8, '#999', 'dash'), ligne(seuil, '#d1495b', 'solid', true)],
    annotations: [{ xref: 'paper', x: 1, y: seuil, text: `${t('seuil_vip')} ${fmt(seuil, 2)}`, showarrow: false, xanchor: 'right', yanchor: 'bottom', font: { color: '#d1495b', size: 11 } }] });
  graphe.on('plotly_click', (ev) => {  // a click highlights the variable, Ctrl+click keeps or drops it
    const terme = ev.points[0] && ev.points[0].customdata;
    if (terme === undefined) return;
    if (!(ev.event && (ev.event.ctrlKey || ev.event.metaKey))) {
      const s = new Set(ctx.variablesChoisies(m.id));
      if (s.has(terme)) s.delete(terme); else s.add(terme);
      ctx.choisirVariables(m.id, [...s]);
      return;
    }
    const i = v.terme.indexOf(terme);
    const parSeuil = v.VIP[i] >= seuil;
    if (terme in loc.forces) delete loc.forces[terme];
    else loc.forces[terme] = !parSeuil;
    ctx.rafraichir();
  });
  graphe.on('plotly_relayout', (ev) => {  // the red line was dragged
    const y = ev['shapes[2].y0'];
    if (y === undefined) return;
    loc.seuil = Math.max(0, Math.round(y * 100) / 100);
    ctx.rafraichir();
  });
}

function rendreCoefficients(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.y ??= 0;
  loc.cs ??= true;
  const c = m.coefficients;
  const y = m.noms_y[loc.y];
  const cle = (loc.cs ? 'CoeffCS ' : 'Coeff ') + y;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix(t('y_var'), m.noms_y.map((n, i) => [i, n]), loc.y, (v) => { loc.y = +v; ctx.rafraichir(); }),
    choix('', [['1', t('echelle_cs')], ['0', t('echelle_donnees')]], loc.cs ? '1' : '0', (v) => { loc.cs = v === '1'; ctx.rafraichir(); })), graphe);
  const idx = c.terme.map((_, i) => i).filter((i) => c.terme[i] !== 'Constante');
  const termes = idx.map((i) => c.terme[i]);
  tracer(ctx, graphe, [{ type: 'bar', x: termes, y: idx.map((i) => c[cle][i]), meta: 'variables', customdata: termes,
    marker: barresVariables(termes, new Set(ctx.variablesChoisies(m.id)), idx.map((i) => (c[cle][i] >= 0 ? PALETTE[0] : PALETTE[1]))),
    error_y: { type: 'data', array: idx.map((i) => c[cle + ' IC'][i]), visible: true, color: '#888', thickness: 1 },
    hovertemplate: '<b>%{x}</b><br>%{y:.4f}<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('coefficients')} ${y}` }, dragmode: 'pan', showlegend: false });
  brancherVariables(graphe, ctx, m.id);
}

function rendreObsPred(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.y ??= 0;
  const y = m.noms_y[loc.y];
  const o = m.observations;
  loc.couleur ??= estDA(m) ? 'classe' : '';
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix(t('y_var'), m.noms_y.map((n, i) => [i, n]), loc.y, (v) => { loc.y = +v; ctx.rafraichir(); }),
    controleCouleur(ctx, m, loc),
    controleEtiquettes(ctx, m, loc),
    el('span', { class: 'infos' }, `RMSEE ${fmt(m.RMSEE[y], 4)} · RMSEcv ${fmt(m.RMSEcv[y], 4)}`
      + (m.test && m.test.statistiques ? ` · RMSEP ${fmt([].concat(m.test.statistiques.RMSEP)[[].concat(m.test.statistiques.y).indexOf(y)], 4)}` : ''))), graphe);
  const xo = o[`${y} observé`];
  const yf = o[`${y} ajusté`];
  const vals = xo.concat(yf).filter((v) => v !== null);
  const points = pointsObservations(ctx, m, loc, xo, yf, { survol: '%{x:.3f} → %{y:.3f}' });
  if (m.test && m.test.observations[`${y} observé`]) {  // the test set, predicted by the model
    const to = m.test.observations;
    const [xt, yt] = [[].concat(to[`${y} observé`]), [].concat(to[`${y} prédit`])];
    vals.push(...xt.concat(yt).filter((v) => v !== null));
    points.push({ type: 'scatter', mode: 'markers', name: t('jeu_test'), x: xt, y: yt, text: [].concat(to.nom),
      marker: { symbol: 'diamond-open', size: 9, color: '#d1495b', line: { width: 1.5 } }, hovertemplate: `<b>%{text}</b> (${t('test')})<br>%{x:.3f} → %{y:.3f}<extra></extra>` });
  }
  const lo = Math.min(...vals);
  const hi = Math.max(...vals);
  tracer(ctx, graphe, points,
  { showlegend: points.length > 1, title: { text: `${m.nom} · ${y} : ${t('observe')} / ${t('ajuste')}` },
    shapes: [{ type: 'line', x0: lo, y0: lo, x1: hi, y1: hi, line: { color: '#888', dash: 'dot', width: 1 } }],
    xaxis: { title: { text: `${y} ${t('observe')}` } }, yaxis: { title: { text: `${y} ${t('ajuste')}` } } });
  brancherSelection(graphe, ctx, m.id);
}

async function rendreDendrogramme(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.k ??= 3;
  loc.comp ??= m.A;
  const cleCache = `${loc.comp}:${loc.k}`;
  if (!loc.arbre || loc.cle !== cleCache) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.arbre = await ctx.demander('hca', { modele: m.id, composantes: loc.comp, groupes: loc.k });
    loc.cle = cleCache;
  }
  const h = loc.arbre;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix(`${t('composante')} 1 →`, composantes(m), loc.comp, (v) => { loc.comp = +v; ctx.rafraichir(); }),
    el('label', { class: 'champ' }, el('span', {}, t('groupes')), el('input', { type: 'number', min: 1, max: 20, value: loc.k, style: 'width:4.5em',
      onchange: (e) => { loc.k = Math.max(1, +e.target.value || 1); ctx.rafraichir(); } })),
    espace(),
    el('button', { onclick: () => {
      ctx.groupesHCA[m.id] = Object.fromEntries(h.uid.map((u, i) => [u, h.groupes[i]]));
      ctx.diffuserGroupes(m.id);
      ctx.ouvrirVue('scores', m.id, { couleur: 'hca' });
    } }, icone('chart-scatter'), t('colorer_scores'))), graphe);
  // hclust layout: leaves at their position in `ordre`, a merge at the mean of its children
  const n = h.noms.length;
  const pos = new Array(n);
  h.ordre.forEach((leaf, i) => { pos[leaf - 1] = i; });
  const X = [];
  const Y = [];
  const grp = [];
  const coords = (j) => (j < 0 ? [pos[-j - 1], 0, h.groupes[-j - 1]] : [X[j - 1], Y[j - 1], grp[j - 1]]);
  const segments = new Map();
  h.merge.forEach(([a, b], i) => {
    const [xa, ya, ga] = coords(a);
    const [xb, yb, gb] = coords(b);
    const y = h.hauteurs[i];
    const g = ga === gb ? ga : 0;
    X.push((xa + xb) / 2); Y.push(y); grp.push(g);
    if (!segments.has(g)) segments.set(g, { x: [], y: [] });
    const s = segments.get(g);
    s.x.push(xa, xa, xb, xb, null); s.y.push(ya, y, y, yb, null);
  });
  const traces = [...segments.entries()].sort((a, b) => a[0] - b[0]).map(([g, s]) => ({ type: 'scatter', mode: 'lines', x: s.x, y: s.y, hoverinfo: 'skip',
    line: { color: g ? PALETTE[(g - 1) % PALETTE.length] : '#888', width: 1 }, name: g ? `${t('groupe')} ${g}` : '', showlegend: !!g }));
  traces.push({ type: 'scatter', mode: 'markers', x: h.ordre.map((_, i) => i), y: h.ordre.map(() => 0), text: h.ordre.map((leaf) => h.noms[leaf - 1]),
    marker: { size: 3, color: '#777' }, hovertemplate: '%{text}<extra></extra>', showlegend: false });
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${t('dendrogramme')} (Ward, t1…t${loc.comp})` }, dragmode: 'zoom',
    xaxis: { showticklabels: n <= 60, tickvals: h.ordre.map((_, i) => i), ticktext: h.ordre.map((leaf) => h.noms[leaf - 1]), tickangle: -60, showgrid: false },
    yaxis: { title: { text: t('hauteur') } } });
}

// Predictions: the project's observations, one of its prediction sets, or a file (CSV or
// Excel; its columns are matched to the model's variables by name, in R). The model's own
// observations stay in grey behind; predictions outside the model's domain (DModX above
// DCrit or T2 above T2Crit 95 %) are marked.
async function rendrePredictions(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const proj = ctx.projet(m.projet);
  loc.jeu ??= '';
  if (loc.jeu !== 'fichier' && (!loc.res || loc.cle !== loc.jeu)) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('predire', loc.jeu === '' ? { modele: m.id } : { modele: m.id, jeu: +loc.jeu });
    loc.cle = loc.jeu;
  }
  const r = loc.res;
  const o = r.observations;
  const cols = Object.keys(o).filter((k) => !['uid', 'nom', 'dans_modele', 'hors_domaine'].includes(k));
  const haut = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  const fichier = loc.jeu === 'fichier';
  const options = [['', t('jeu_complet')]].concat(((proj && proj.predictions) || []).map((s, i) => [i + 1, s.nom || `${t('jeu_prediction')} ${i + 1}`]))
    .concat(fichier ? [['fichier', r.fichier]] : []).concat([['choisir', t('predire_fichier')]]);
  const jeu = choix(t('jeu_prediction'), options, loc.jeu, async (v) => {
    if (v === 'choisir') {
      const res = await ctx.predireFichier(m.id);
      if (res) { loc.res = res; loc.jeu = 'fichier'; loc.cle = 'fichier'; }
    } else loc.jeu = v;
    ctx.rafraichir();
  });
  const hors = o.hors_domaine ? o.hors_domaine.filter(Boolean).length : 0;
  const infos = el('span', { class: 'infos' }, `${o.nom.length} ${t('obs')}`,
    hors ? ` · ${hors} ${t('hors_domaine')}` : '',
    fichier && r.absentes && [].concat(r.absentes).length ? el('span', { class: 'erreur' }, ` · ${t('variables_absentes')} : ${[].concat(r.absentes).join(', ')}`) : '',
    // values of the file read as missing (text in a quantitative column: <0.5, nd...)
    fichier && r.note && [].concat(r.note).length ? el('span', { class: 'erreur' }, ` · ${[].concat(r.note).join(' ; ')}`) : '');
  loc.graphe ??= 'scores';
  const vueGraphe = o.DModX ? choix('', [['scores', t('scores')], ['dmodx', 'DModXPS']], loc.graphe, (v) => { loc.graphe = v; ctx.rafraichir(); }) : null;
  conteneur.replaceChildren(el('div', { class: 'outils' }, vueGraphe, infos, el('span', { class: 'espace' }),
    el('button', { onclick: () => ctx.exporterPrediction(m.id) }, icone('file-down'), t('exporter_court'))), haut, bas);
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    avant: [jeu], n: o.nom.length, nom: 'Table',
    colonnes: [{ cle: 'nom', titre: 'observation' }, { cle: 'dans_modele', titre: t('dans_modele') }, { cle: 'hors_domaine', titre: t('hors_domaine_court') }]
      .concat(cols.map((k) => ({ cle: k, titre: k }))),
    valeur: (i, k) => (k === 'dans_modele' ? (o.dans_modele[i] ? '✓' : '') : k === 'hors_domaine' ? (o.hors_domaine && o.hors_domaine[i] ? '⚠' : '') : o[k][i]),
  });
  const mo = m.observations;
  const yk = m.A > 1 ? 't2' : 'T2';
  const ii = o.nom.map((_, i) => i).filter((i) => !o.dans_modele[i]);
  const couleur = ii.map((i) => (o.hors_domaine && o.hors_domaine[i] ? '#d1495b' : PALETTE[0]));
  if (loc.graphe === 'dmodx' && o.DModX) {  // DModXPS: distance of each prediction to the model, against DCrit
    const recent = m.version_origine === null || m.version_origine === undefined || m.version_origine >= 13;
    const cle = recent && o.DModX_pondere ? 'DModX_pondere' : 'DModX';
    const dcrit = m.limites['DCrit(5%)'];
    const y = ii.map((i) => o[cle][i]);
    const dans = o.nom.map((_, i) => i).filter((i) => o.dans_modele[i]);
    tracer(ctx, haut, [
      { type: 'scatter', mode: 'markers', name: t('dans_modele'), x: dans.map((i) => i + 1), y: dans.map((i) => o[cle][i]), text: dans.map((i) => o.nom[i]),
        marker: { color: '#b8c2cf', size: 5 }, hovertemplate: '<b>%{text}</b><br>%{y:.3f}<extra>%{fullData.name}</extra>' },
      { type: 'scatter', mode: 'markers', name: t('predictions'), x: ii.map((i) => i + 1), y, text: ii.map((i) => o.nom[i]),
        marker: { color: y.map((v) => (v !== null && v > dcrit ? '#d1495b' : PALETTE[0])), size: 6 },
        hovertemplate: '<b>%{text}</b><br>%{y:.3f}<extra>%{fullData.name}</extra>' }],
    { title: { text: `${m.nom} · DModXPS${recent ? ` · ${t('ponderee')}` : ''}${fichier ? ' · ' + r.fichier : ''}` }, dragmode: 'zoom', margin: { t: 30, b: 36 },
      shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: dcrit, y1: dcrit, line: { color: '#d1495b', width: 1, dash: 'dash' } }],
      annotations: [{ xref: 'paper', x: 1, y: dcrit, text: `DCrit ${fmt(dcrit, 3)}`, showarrow: false, xanchor: 'right', yanchor: 'bottom', font: { size: 10, color: '#d1495b' } }],
      xaxis: { title: { text: 'observation' } }, yaxis: { title: { text: 'DModXPS' }, rangemode: 'tozero' } });
    return;
  }
  tracer(ctx, haut, [
    { type: 'scattergl', mode: 'markers', name: t('dans_modele'), x: mo.t1, y: mo[yk], text: mo.nom,
      marker: { color: '#b8c2cf', size: 5 }, hovertemplate: '<b>%{text}</b><extra>%{fullData.name}</extra>' },
    { type: 'scattergl', mode: 'markers', name: t('predictions'), x: ii.map((i) => o.t1[i]), y: ii.map((i) => o[yk][i]),
      text: ii.map((i) => o.nom[i]), marker: { color: couleur, size: 7, opacity: 0.9 }, hovertemplate: '<b>%{text}</b><extra>%{fullData.name}</extra>' }],
  { title: { text: `${m.nom} · ${t('predictions')}${fichier ? ' · ' + r.fichier : ''}` }, xaxis: { title: { text: 't[1]' } },
    yaxis: { title: { text: yk === 't2' ? 't[2]' : 'T2' } }, dragmode: 'zoom', margin: { t: 30, b: 36 } });
}

// Contributions: why the selected observation, or the mean of the
// selected observations, differs from the average one: DModX residuals, or score / T2
// differences weighted by the components' weights. Computed in R.
async function rendreContributions(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.type ??= 'DModX';
  loc.comp ??= 1;
  loc.pondere ??= true;
  const sel = ctx.selection(m.id);
  const barre = outils(
    choix(t('contribution_a'), [['DModX', 'DModX'], ['score', t('score')], ['T2', 'T²']], loc.type, (v) => { loc.type = v; ctx.rafraichir(); }),
    loc.type === 'score' ? choix(t('comp'), composantes(m), loc.comp, (v) => { loc.comp = +v; ctx.rafraichir(); }) : null,
    loc.type === 'DModX' ? case_(t('pondere_mp'), loc.pondere, (v) => { loc.pondere = v; ctx.rafraichir(); }) : null,
    case_(t('trier'), loc.trier, (v) => { loc.trier = v; ctx.rafraichir(); }));
  if (!sel.length) {
    conteneur.replaceChildren(barre, el('p', { class: 'infos attente' }, t('contrib_selection')));
    return;
  }
  const cle = JSON.stringify([loc.type, loc.comp, loc.pondere, [...sel].sort()]);
  if (loc.cle !== cle) {
    conteneur.replaceChildren(barre, el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('contributions', { modele: m.id, observations: sel, type: loc.type, composante: loc.comp, ponderee: loc.pondere });
    loc.cle = cle;
  }
  const r = loc.res;
  const noms = m.observations.uid.map((u, i) => (sel.includes(u) ? m.observations.nom[i] : null)).filter((x) => x !== null);
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(barre,
    el('p', { class: 'note' }, sel.length === 1 ? t('contrib_une', { nom: noms[0] }) : t('contrib_groupe', { n: sel.length, noms: noms.slice(0, 6).join(', ') + (noms.length > 6 ? '…' : '') })),
    graphe);
  let idx = r.terme.map((_, i) => i);
  if (loc.trier) idx.sort((a, b) => Math.abs(r.contribution[b]) - Math.abs(r.contribution[a]));
  const termes = idx.map((i) => r.terme[i]);
  tracer(ctx, graphe, [{ type: 'bar', x: termes, y: idx.map((i) => r.contribution[i]), meta: 'variables', customdata: termes,
    marker: barresVariables(termes, new Set(ctx.variablesChoisies(m.id)), idx.map((i) => (r.contribution[i] >= 0 ? PALETTE[0] : PALETTE[1]))),
    hovertemplate: '<b>%{x}</b><br>%{y:.3f}<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('contributions')} ${loc.type === 'score' ? 't[' + loc.comp + ']' : loc.type}` }, dragmode: 'pan', showlegend: false });
  brancherVariables(graphe, ctx, m.id);
}

function rendreListeObs(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const o = m.observations;
  const cles = Object.keys(o).filter((k) => !['uid', 'nom'].includes(k));
  tableau(conteneur, ctx, loc, {
    n: o.uid.length, idModele: m.id, uid: (i) => o.uid[i], nom: 'Table',
    colonnes: [{ cle: 'nom', titre: 'observation' }].concat(cles.map((k) => ({ cle: k, titre: k.replace('_pondere', ' (pond.)').replace('_absolu', ' (abs.)') }))),
    valeur: (i, k) => o[k][i],
  });
}

function rendreListeVars(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const x = m.variables;
  const y = m.variables_y;
  const c = m.coefficients;
  const nx = x.terme.length;
  const ny = y ? y.terme.length : 0;
  const cles = [...new Set(Object.keys(x).concat(y ? Object.keys(y) : []).concat(c ? Object.keys(c) : []))].filter((k) => k !== 'terme' && k !== 'bloc');
  const val = (i, k) => {
    if (k === 'terme') return i < nx ? x.terme[i] : y.terme[i - nx];
    if (k === 'bloc') return i < nx ? 'X' : 'Y';
    if (i < nx) return k in x ? x[k][i] : c && k in c ? c[k][i] : null;
    return k in y ? y[k][i - nx] : null;
  };
  const choisies = new Set(ctx.variablesChoisies(m.id));
  tableau(conteneur, ctx, loc, { n: nx + ny, nom: 'Table', colonnes: [{ cle: 'terme', titre: t('variables') }, { cle: 'bloc', titre: '' }]
    .concat(cles.map((k) => ({ cle: k, titre: k.replace(/_IC$/, ' IC').replace('_cum', '(cum)') }))), valeur: val,
    choisie: (i) => choisies.has(val(i, 'terme')),
    cliquer: (i) => {  // a row highlights its variable on the model's graphs
      const s = new Set(choisies);
      if (s.has(val(i, 'terme'))) s.delete(val(i, 'terme')); else s.add(val(i, 'terme'));
      ctx.choisirVariables(m.id, [...s]);
    } });
}

async function rendreDonnees(conteneur, ctx, p, loc) {
  if (!loc.d) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.d = await ctx.demander('donnees', { projet: p.projet });
  }
  const d = loc.d;
  // A wide table (spectra: a column per wavelength) shows its first columns, or those whose name
  // holds the text typed: thousands of columns would freeze the window.
  let k = d.variables.map((_, i) => i);
  const avant = [el('button', { onclick: () => ctx.nouvelleVariable(p.projet) }, icone('calculator'), t('nouvelle_variable'))];
  let filtreColonnes = null;
  if (k.length > COLONNES_MAX) {
    const f = (loc.filtreColonnes || '').toLowerCase();
    if (f) k = k.filter((i) => String(d.variables[i]).toLowerCase().includes(f));
    filtreColonnes = el('input', { type: 'search', class: 'filtre', placeholder: t('filtrer_variables'), value: loc.filtreColonnes || '',
      oninput: (e) => { loc.filtreColonnes = e.target.value; clearTimeout(loc.minuterieColonnes); loc.minuterieColonnes = setTimeout(() => ctx.rafraichir(), 300); },
      onfocus: () => { loc.focusColonnes = true; }, onblur: () => { loc.focusColonnes = false; } });
    avant.push(filtreColonnes, el('span', { class: 'infos' }, t('colonnes_montrees', { n: Math.min(k.length, COLONNES_MAX), total: d.variables.length })));
    k = k.slice(0, COLONNES_MAX);
  }
  tableau(conteneur, ctx, loc, {
    n: d.observations.length, nom: 'Table', avant,
    colonnes: [{ cle: -1, titre: '' }].concat(k.map((i) => ({ cle: i, titre: d.variables[i], modifiable: true }))),
    valeur: (i, j) => (j === -1 ? d.observations[i] : d.valeurs[j][i]),
    modifier: (i, j, texte) => ctx.modifierValeur(p.projet, d.uid_observations[i], d.uid_variables[j], texte),
  });
  if (filtreColonnes && loc.focusColonnes) { filtreColonnes.focus(); filtreColonnes.setSelectionRange(filtreColonnes.value.length, filtreColonnes.value.length); }
}
const COLONNES_MAX = 200;

// Name of a transformation as the model dialog writes it ("log10:C2" is log10(x + C2)).
export const libelleTransfo = (tr) => (!tr ? '' : tr.startsWith('log10:') ? `log10(x + ${tr.slice(6)})` : t('tr_' + tr));

// x transformed as in R (moteur/R/pretraitement.R, transformer), null where undefined.
function transformer(x, tr) {
  if (!tr) return x;
  if (tr.startsWith('log10')) {
    const c2 = tr.includes(':') ? +tr.split(':')[1] : 0;
    return x.map((v) => (v === null || v + c2 <= 0 ? null : Math.log10(v + c2)));
  }
  const puissance = { racine2: 0.5, racine4: 0.25 }[tr];
  return x.map((v) => (v === null || v < 0 ? null : v ** puissance));
}

// Variable statistics of the workset, computed in R: Min/Max and skewness in red where a
// transformation is advised, and the log10 the automatic transform would apply. Below, the histogram of the
// chosen variable, as is and transformed, or the map of missing values.
async function rendreStatistiques(conteneur, ctx, p, loc) {
  if (!loc.res) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('statistiques_variables', { projet: p.projet });
  }
  const s = loc.res;
  const proj = ctx.projet(p.projet);
  loc.mode ??= 'histogramme';
  loc.k ??= Math.max(0, s.qualitative.findIndex((q) => !q));
  const k = Math.min(loc.k, s.uid.length - 1);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  const nSugg = s.suggestion.filter(Boolean).length;
  if (loc.k !== loc.kApercu) { loc.apercu = s.suggestion[k] || ''; loc.kApercu = loc.k; }
  conteneur.replaceChildren(outils(
    choix('', [['histogramme', t('histogramme')], ['manquantes', t('carte_manquantes')]], loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    loc.mode === 'histogramme' && !s.qualitative[k] ? choix(t('apercu'), [['', t('tr_aucune')], ['racine2', t('tr_racine2')], ['racine4', t('tr_racine4')], ['log10', 'log10']]
      .concat(s.suggestion[k] && s.suggestion[k] !== 'log10' ? [[s.suggestion[k], libelleTransfo(s.suggestion[k])]] : []), loc.apercu, (v) => { loc.apercu = v; ctx.rafraichir(); }) : null,
    el('span', { class: 'infos' }, t('stats_note', { n: nSugg }))), graphe, bas);
  loc.table ??= {};
  const col = (cle, titre, decimales, alerte) => ({ cle, titre, num: true, decimales, alerte });
  tableau(bas, ctx, loc.table, {
    n: s.uid.length, nom: 'Table',
    colonnes: [{ cle: 'nom', titre: t('variables') }, col('N', 'N', 0), col('manquantes', t('manquantes_col'), 1), col('min', 'Min'), col('max', 'Max'),
      col('min_max', 'Min/Max', 4, (i) => s.rouge_min_max[i]), col('moyenne', t('moyenne')), col('mediane', t('mediane')), col('ecart_type', t('ecart_type')),
      col('et_moyenne', t('et_moyenne'), 3), col('asymetrie', t('asymetrie'), 3, (i) => s.rouge_asymetrie[i]),
      col('test_asymetrie', t('test_asymetrie'), 2, (i) => s.rouge_asymetrie[i]), col('aplatissement', t('aplatissement'), 3),
      { cle: 'suggestion', titre: t('suggestion') }],
    valeur: (i, c) => (c === 'nom' ? s.nom[i] + (s.qualitative[i] ? ` (${s.categories[i]} ${t('categories')})` : '')
      : c === 'suggestion' ? libelleTransfo(s.suggestion[i]) : s[c][i]),
    cliquer: (i) => { loc.k = i; loc.mode = 'histogramme'; ctx.rafraichir(); },
    choisie: (i) => loc.mode === 'histogramme' && i === k,
  });
  if (loc.mode === 'manquantes') {
    if (!loc.d) loc.d = await ctx.demander('donnees', { projet: p.projet });
    const d = loc.d;
    const avec = d.variables.map((_, j) => j).filter((j) => d.valeurs[j].some((v) => v === null));
    if (!avec.length) { graphe.replaceChildren(el('p', { class: 'infos' }, t('aucune_manquante'))); return; }
    tracer(ctx, graphe, [{ type: 'heatmap', x: avec.map((j) => d.variables[j]), y: d.observations,
      z: d.observations.map((_, i) => avec.map((j) => (d.valeurs[j][i] === null ? 1 : 0))),
      colorscale: [[0, couleursTheme().fond], [1, '#d1495b']], showscale: false, xgap: 1,
      hovertemplate: '%{y} · %{x}<extra></extra>' }],
    { title: { text: `${t('carte_manquantes')} (${avec.length} ${t('vars')})` }, dragmode: 'zoom',
      yaxis: { autorange: 'reversed', showticklabels: d.observations.length <= 60 }, xaxis: { tickangle: -45, tickfont: { size: 9 } } });
    return;
  }
  const cle = `${s.uid[k]}`;
  if (loc.cleValeurs !== cle) {
    loc.valeurs = (await ctx.valeurs(p.projet, s.uid[k], proj.observations.uid)).valeurs;
    loc.cleValeurs = cle;
  }
  const x = loc.valeurs;
  const titre = { text: `${s.nom[k]}${s.suggestion[k] ? ` · ${t('suggestion')} : ${libelleTransfo(s.suggestion[k])}` : ''}` };
  if (s.qualitative[k]) {
    const n = new Map();
    x.forEach((v) => { if (v !== null) n.set(v, (n.get(v) || 0) + 1); });
    tracer(ctx, graphe, [{ type: 'bar', x: [...n.keys()], y: [...n.values()], marker: { color: PALETTE[0] } }], { title: titre, dragmode: false });
    return;
  }
  const traces = [{ type: 'histogram', x, name: t('tr_aucune'), marker: { color: PALETTE[0] }, nbinsx: 30 }];
  const lay = { title: titre, dragmode: 'zoom', bargap: 0.04, showlegend: false };
  if (loc.apercu) {
    const c = couleursTheme();
    traces.push({ type: 'histogram', x: transformer(x, loc.apercu), name: libelleTransfo(loc.apercu), marker: { color: PALETTE[2] }, nbinsx: 30, xaxis: 'x2', yaxis: 'y2' });
    Object.assign(lay, { grid: { rows: 1, columns: 2, pattern: 'independent' },
      xaxis: { title: { text: s.nom[k] } }, xaxis2: { title: { text: libelleTransfo(loc.apercu) }, gridcolor: c.grille, automargin: true },
      yaxis2: { gridcolor: c.grille, automargin: true } });
  }
  tracer(ctx, graphe, traces, lay);
}

// Class models (one PCA per class, Wold 1976): every observation's distance (DModX) to two of
// the group's models, with each model's critical limit (Coomans et al., 1984). Below the
// limit of a model, the observation belongs to its class: to one class, several, or none. The
// table gives, for each known class, the share of its observations each model accepts, then
// every observation's distances and classes. Computed in R (modeles_classe.R).
async function rendreCoomans(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  if (!loc.res || loc.cle !== m.id) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('appartenance', { modele: m.id });
    loc.cle = m.id;
  }
  const r = loc.res;
  const classes = [].concat(r.classes);
  const dcrit = [].concat(r.dcrit);  // in the order of the classes
  const o = r.observations;
  const noms = [].concat(o.nom);
  const vraie = [].concat(o.classe);
  if (!classes.includes(loc.x)) loc.x = m.classe_modele && classes.includes(m.classe_modele) ? m.classe_modele : classes[0];
  if (!classes.includes(loc.y) || loc.y === loc.x) loc.y = classes.find((c) => c !== loc.x) || classes[0];
  const dx = [].concat(o[`DModX ${loc.x}`]);
  const dy = [].concat(o[`DModX ${loc.y}`]);
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  const tableParts = el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, el('th', {}, r.variable), el('th', { class: 'num' }, 'n'), classes.map((c) => el('th', { class: 'num' }, `${t('modele_court')} ${c}`)))),
    el('tbody', {}, [].concat(r.table).map((l) => el('tr', {}, el('td', {}, l.classe), el('td', { class: 'num' }, l.n),
      [].concat(l.parts).map((v, k) => el('td', { class: 'num' + (l.classe === classes[k] ? ' choisie' : '') }, `${fmt(100 * v, 0)} %`))))));
  conteneur.replaceChildren(outils(
    choix(t('axe_x'), classes.map((c) => [c, c]), loc.x, (v) => { loc.x = v; ctx.rafraichir(); }),
    choix(t('axe_y'), classes.filter((c) => c !== loc.x).map((c) => [c, c]), loc.y, (v) => { loc.y = v; ctx.rafraichir(); }),
    el('span', { class: 'infos' }, t('coomans_note'))), graphe, el('div', { class: 'defile', style: 'max-height:30%' }, tableParts), bas);
  const c = couleursTheme();
  const groupes = [...new Set(vraie.map((v) => v ?? '?'))];
  const traces = groupes.map((g, k) => {
    const idx = vraie.map((v, i) => i).filter((i) => (vraie[i] ?? '?') === g);
    return { type: 'scatter', mode: 'markers', name: g, x: idx.map((i) => dx[i]), y: idx.map((i) => dy[i]), text: idx.map((i) => noms[i]),
      marker: { color: g === '?' ? c.doux : PALETTE[k % PALETTE.length], size: 7 * FORMAT.points },
      hovertemplate: `%{text} (${g})<br>DModX ${loc.x} = %{x:.2f}<br>DModX ${loc.y} = %{y:.2f}<extra></extra>` };
  });
  const lx = dcrit[classes.indexOf(loc.x)];
  const ly = dcrit[classes.indexOf(loc.y)];
  tracer(ctx, graphe, traces, { title: { text: `Coomans · ${loc.x} / ${loc.y}` }, dragmode: 'zoom', showlegend: true,
    xaxis: { title: { text: `DModX ${loc.x}` }, rangemode: 'tozero' },
    yaxis: { title: { text: `DModX ${loc.y}` }, rangemode: 'tozero' },
    shapes: [{ type: 'line', yref: 'paper', y0: 0, y1: 1, x0: lx, x1: lx, line: { dash: 'dot', color: '#d1495b', width: 1 } },
      { type: 'line', xref: 'paper', x0: 0, x1: 1, y0: ly, y1: ly, line: { dash: 'dot', color: '#d1495b', width: 1 } }] });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: noms.length, nom: 'Classes',
    colonnes: [{ cle: 'nom', titre: t('observations') }, { cle: 'classe', titre: r.variable }]
      .concat(classes.map((k, j) => ({ cle: `DModX ${k}`, titre: `DModX ${k}`, num: true, decimales: 2, alerte: (i) => [].concat(o[`DModX ${k}`])[i] > dcrit[j] })),
        [{ cle: 'classes', titre: t('classes_acceptees') }]),
    valeur: (i, k) => (k === 'nom' ? noms[i] : [].concat(o[k])[i]),
  });
}

// Tests variable by variable, computed in R: each quantitative variable compared between the
// classes of a qualitative one, p-values corrected for multiple testing. Two classes: t-test
// or Mann–Whitney, and a volcano plot (effect against -log10 p, the variables that pass both
// cut-offs coloured); all classes: ANOVA or Kruskal–Wallis, and the variables ranked.
const valeurP = (v) => (v === null || v === undefined ? '' : v < 1e-4 ? v.toExponential(1) : v.toFixed(4));
async function rendreTests(conteneur, ctx, p, loc) {
  const proj = ctx.projet(p.projet);
  const quals = proj.variables.filter((v) => v.qualitative && [].concat(v.categories).length >= 2);
  if (!quals.length) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('tests_sans_classe'))); return; }
  const v = quals.find((x) => x.uid === loc.classe) || quals[0];
  loc.classe = v.uid;
  const cats = [].concat(v.categories);
  loc.comparaison ??= 'deux';
  const deux = loc.comparaison === 'deux';
  if (!cats.includes(loc.a)) loc.a = cats[0];
  if (!cats.includes(loc.b) || loc.b === loc.a) loc.b = cats.find((c) => c !== loc.a);
  const tests = deux ? [['welch', t('test_welch')], ['student', t('test_student')], ['mann_whitney', 'Mann–Whitney']]
    : [['anova', 'ANOVA'], ['kruskal', 'Kruskal–Wallis']];
  if (!tests.some(([k]) => k === loc.test)) loc.test = tests[0][0];
  loc.correction ??= 'BH';
  loc.seuil ??= 0.05;
  const cle = JSON.stringify([v.uid, deux ? [loc.a, loc.b] : null, loc.test, loc.correction]);
  if (loc.cle !== cle) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('tests_variables', { projet: p.projet, classe: v.uid, groupes: deux ? [loc.a, loc.b] : null,
      test: loc.test, correction: loc.correction });
    loc.cle = cle;
  }
  const tab = loc.res.tableau;
  const col = (k) => [].concat(tab[k]);
  const noms = col('nom');
  const n = noms.length;
  const pv = col('p');
  const pa = col('p_ajuste');
  const log2 = deux ? col('log2_rapport') : [];
  const log2Complet = log2.every((x) => x !== null);
  if (deux && !(loc.axe === 'd' || (loc.axe === 'log2' && log2Complet))) loc.axe = log2Complet ? 'log2' : 'd';
  const effet = deux ? (loc.axe === 'log2' ? log2 : col('d')) : [];
  const seuilsEffet = loc.axe === 'log2' ? [[0, t('aucun')], [0.585, '× 1,5'], [1, '× 2']] : [[0, t('aucun')], [0.5, '0,5'], [0.8, '0,8']];
  if (!seuilsEffet.some(([k]) => k === loc.effet)) loc.effet = seuilsEffet[0][0];
  const retenue = (i) => pa[i] !== null && pa[i] <= loc.seuil && (!deux || (effet[i] !== null && Math.abs(effet[i]) >= loc.effet));
  const nRetenues = noms.filter((_, i) => retenue(i)).length;
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  const nomTest = Object.fromEntries(tests)[loc.test];
  const nomCorrection = { BH: t('correction_bh'), bonferroni: 'Bonferroni', aucune: t('aucune') }[loc.correction];
  conteneur.replaceChildren(outils(
    choix(t('classe_variable'), quals.map((q) => [q.uid, q.nom]), v.uid, (x) => { loc.classe = Number(x); ctx.rafraichir(); }),
    choix('', [['deux', t('deux_classes')], ['toutes', t('toutes_classes')]], loc.comparaison, (x) => { loc.comparaison = x; ctx.rafraichir(); }),
    deux ? choix('A', cats.map((c) => [c, c]), loc.a, (x) => { loc.a = x; ctx.rafraichir(); }) : null,
    deux ? choix('B', cats.filter((c) => c !== loc.a).map((c) => [c, c]), loc.b, (x) => { loc.b = x; ctx.rafraichir(); }) : null,
    choix(t('test'), tests, loc.test, (x) => { loc.test = x; ctx.rafraichir(); }),
    choix(t('correction'), [['BH', t('correction_bh')], ['bonferroni', 'Bonferroni'], ['aucune', t('aucune')]], loc.correction, (x) => { loc.correction = x; ctx.rafraichir(); }),
    choix(t('seuil_p'), [[0.01, '0,01'], [0.05, '0,05'], [0.1, '0,1']], loc.seuil, (x) => { loc.seuil = Number(x); ctx.rafraichir(); }),
    deux ? choix(t('effet'), [['log2', t('effet_log2')], ['d', t('effet_d')]], loc.axe, (x) => { loc.axe = x; ctx.rafraichir(); }) : null,
    deux ? choix(t('seuil_effet'), seuilsEffet, loc.effet, (x) => { loc.effet = Number(x); ctx.rafraichir(); }) : null),
    el('p', { class: 'note' }, t(deux ? 'tests_note_deux' : 'tests_note_toutes', { test: nomTest, correction: nomCorrection, a: loc.a, b: loc.b, n: nRetenues, total: n })),
    graphe, bas);
  const c = couleursTheme();
  if (deux) {
    const y = pv.map((x) => (x === null ? null : -Math.log10(Math.max(x, 1e-300))));
    const couleur = noms.map((_, i) => (!retenue(i) ? c.doux : effet[i] > 0 ? '#d1495b' : PALETTE[0]));
    const etiquettes = noms.map((_, i) => i).filter(retenue).sort((i, j) => pv[i] - pv[j]).slice(0, 12);
    const ligneP = Math.max(...noms.map((_, i) => (pa[i] !== null && pa[i] <= loc.seuil ? pv[i] : -1)));
    const formes = [];
    if (ligneP > 0) formes.push({ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: -Math.log10(ligneP), y1: -Math.log10(ligneP), line: { dash: 'dot', color: c.doux, width: 1 } });
    for (const s of loc.effet > 0 ? [-loc.effet, loc.effet] : []) formes.push({ type: 'line', yref: 'paper', y0: 0, y1: 1, x0: s, x1: s, line: { dash: 'dot', color: c.doux, width: 1 } });
    tracer(ctx, graphe, [{ type: 'scatter', mode: 'markers', x: effet, y, text: noms, marker: { color: couleur, size: 7 * FORMAT.points },
      customdata: noms.map((_, i) => [valeurP(pv[i]), valeurP(pa[i])]),
      hovertemplate: `%{text}<br>${loc.axe === 'log2' ? 'log2(B/A)' : 'd'} = %{x:.3f}<br>p = %{customdata[0]} · ${t('p_ajuste')} = %{customdata[1]}<extra></extra>` },
    { type: 'scatter', mode: 'text', x: etiquettes.map((i) => effet[i]), y: etiquettes.map((i) => y[i]), text: etiquettes.map((i) => noms[i]),
      textposition: 'top center', textfont: { size: FORMAT.taille - 2 }, hoverinfo: 'skip' }],
    { title: { text: `${t('volcan')} · ${loc.b} / ${loc.a}` }, showlegend: false, dragmode: 'zoom', shapes: formes,
      xaxis: { title: { text: loc.axe === 'log2' ? t('effet_log2_axe', { a: loc.a, b: loc.b }) : t('effet_d_axe', { a: loc.a, b: loc.b }) } },
      yaxis: { title: { text: '-log10(p)' } } });
  } else {
    const ordre = noms.map((_, i) => i).filter((i) => pa[i] !== null).sort((i, j) => pa[i] - pa[j]).slice(0, 40);
    tracer(ctx, graphe, [{ type: 'bar', x: ordre.map((i) => noms[i]), y: ordre.map((i) => -Math.log10(Math.max(pa[i], 1e-300))),
      marker: { color: ordre.map((i) => (retenue(i) ? '#d1495b' : c.doux)) }, hovertemplate: `%{x}<br>${t('p_ajuste')} = %{customdata}<extra></extra>`,
      customdata: ordre.map((i) => valeurP(pa[i])) }],
    { title: { text: `${nomTest} · ${v.nom}` }, dragmode: false, xaxis: { tickangle: -45, tickfont: { size: 9 } },
      yaxis: { title: { text: `-log10(${t('p_ajuste')})` } },
      shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: -Math.log10(loc.seuil), y1: -Math.log10(loc.seuil), line: { dash: 'dot', color: c.doux, width: 1 } }] });
  }
  loc.table ??= { tri: { cle: 'p', sens: 1 } };
  const num = (cle, titre, decimales, format) => ({ cle, titre, num: true, decimales, format, alerte: cle === 'p_ajuste' ? retenue : undefined });
  const moyennes = deux ? [num('moyenne_a', `${t('moyenne')} A`), num('moyenne_b', `${t('moyenne')} B`)]
    : cats.map((k) => num(`moyenne ${k}`, `${t('moyenne')} ${k}`));
  tableau(bas, ctx, loc.table, {
    n, nom: 'Tests',
    colonnes: [{ cle: 'nom', titre: t('variables') }].concat(deux ? [num('n_a', 'n A', 0), num('n_b', 'n B', 0)] : [], moyennes,
      deux ? [num('log2_rapport', 'log2(B/A)', 3), num('d', 'd', 3)] : [], [num('statistique', t('statistique'), 3), num('p', 'p', 4, valeurP), num('p_ajuste', t('p_ajuste'), 4, valeurP)]),
    valeur: (i, k) => (k === 'nom' ? noms[i] : [].concat(tab[k])[i]),
  });
}

// The project's audit trail, kept in the .usp: when each model was made or changed, in which
// version of the program, by whom. Can be narrowed to one model.
async function rendreHistorique(conteneur, ctx, p, loc) {
  if (!loc.res) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    loc.res = await ctx.demander('historique', { projet: p.projet });
  }
  const h = loc.res;
  if (!h.date || !h.date.length) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('historique_vide'))); return; }
  loc.modele ??= '';
  const numeros = [...new Set(h.modele.filter((x) => x !== null))].sort((a, b) => a - b);
  const idx = h.date.map((_, i) => i).filter((i) => loc.modele === '' || h.modele[i] === Number(loc.modele));
  loc.table ??= {};
  tableau(conteneur, ctx, loc.table, {
    avant: [choix(t('modele_court'), [['', t('tous')]].concat(numeros.map((k) => [String(k), `M${k}`])), loc.modele, (v) => { loc.modele = v; ctx.rafraichir(); })],
    n: idx.length, nom: 'Table',
    colonnes: [{ cle: 'date', titre: t('date') }, { cle: 'modele', titre: t('modele_court') }, { cle: 'evenement', titre: t('evenement') },
      { cle: 'details', titre: t('details') }, { cle: 'logiciel', titre: t('logiciel') }, { cle: 'utilisateur', titre: t('utilisateur') }],
    valeur: (i, c) => {
      const j = idx[i];
      if (c === 'modele') return h.modele[j] === null ? '' : `M${h.modele[j]}`;
      return c === 'details' ? h.details[j].replace(/\s*\n\s*/g, ' · ') : h[c][j];
    },
  });
}

function rendreComparaison(conteneur, ctx, p, loc) {
  const ms = ctx.tousModeles();
  const cum = (v) => (v && v.length ? v[v.length - 1] : null);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(graphe, bas);
  const lignes = ms.map((m) => {
    const c = m.composantes;
    const s = m.stocke;
    return { id: m.id, nom: m.nom, projet: (ctx.projet(m.projet) || {}).nom, type: m.type, A: m.A, N: m.N, K: m.K,
      R2X: cum(c.R2Xcum), R2Y: c.R2Ycum ? cum(c.R2Ycum) : null, Q2: cum(c.Q2cum),
      RMSEE: m.RMSEE ? Object.values(m.RMSEE)[0] : null, RMSEcv: m.RMSEcv ? Object.values(m.RMSEcv)[0] : null,
      sR2X: s ? s.R2Xcum[m.A - 1] : null, sR2Y: s && s.R2Ycum ? s.R2Ycum[m.A - 1] : null, sQ2: s ? s.Q2cum[m.A - 1] : null };
  });
  tableau(bas, ctx, loc, {
    n: lignes.length, nom: 'Table',
    colonnes: [['nom', ''], ['projet', t('projets')], ['type', t('type')], ['A', 'A'], ['N', 'N'], ['K', 'K'], ['R2X', 'R2X(cum)'],
      ['R2Y', 'R2Y(cum)'], ['Q2', 'Q2(cum)'], ['RMSEE', 'RMSEE'], ['RMSEcv', 'RMSEcv'],
      ['sR2X', `R2X(cum) ${t('stocke')}`], ['sR2Y', `R2Y(cum) ${t('stocke')}`], ['sQ2', `Q2(cum) ${t('stocke')}`]].map(([cle, titre]) => ({ cle, titre })),
    valeur: (i, k) => (typeof lignes[i][k] === 'number' && !['A', 'N', 'K'].includes(k) ? Number(lignes[i][k].toFixed(4)) : lignes[i][k]),
  });
  if (!ms.length) return;
  const noms = lignes.map((l) => l.nom);
  tracer(ctx, graphe, [
    { type: 'bar', name: 'R2X(cum)', x: noms, y: lignes.map((l) => l.R2X), marker: { color: '#9aa7b8' } },
    { type: 'bar', name: 'R2Y(cum)', x: noms, y: lignes.map((l) => l.R2Y), marker: { color: '#3b9a5b' } },
    { type: 'bar', name: 'Q2(cum)', x: noms, y: lignes.map((l) => l.Q2), marker: { color: '#2f6fb5' } }],
  { barmode: 'group', dragmode: false, title: { text: t('comparaison') }, margin: { t: 30, b: 50 } });
}

// Permutation test: R² and Q² of models refitted with the order of one Y
// shuffled, against the correlation between shuffled and original Y; intercepts from R.
async function rendrePermutations(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.y ??= 0;
  loc.n ??= 20;
  const cle = `${loc.y}:${loc.n}`;
  if (!loc.res || loc.cle !== cle) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul_permutations', { n: loc.n })));
    loc.res = await ctx.demander('permutations', { modele: m.id, y: loc.y + 1, n: loc.n });
    loc.cle = cle;
  }
  const r = loc.res;
  const pts = r.points;
  const valide = r.ordonnee_Q2 < 0.05 && r.ordonnee_R2 < 0.4;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    m.noms_y.length > 1 ? choix(t('y_var'), m.noms_y.map((n, i) => [i, n]), loc.y, (v) => { loc.y = +v; ctx.rafraichir(); }) : null,
    el('label', { class: 'champ' }, el('span', {}, t('nb_permutations')), el('input', { type: 'number', min: 1, max: 999, value: loc.n, style: 'width:5em',
      onchange: (e) => { loc.n = Math.max(1, Math.min(999, Math.round(+e.target.value) || 20)); ctx.rafraichir(); } })),
    el('span', { class: 'infos' }, `${t('ordonnees')} : R2 = ${fmt(r.ordonnee_R2, 3)} · Q2 = ${fmt(r.ordonnee_Q2, 3)}`),
    el('span', { class: 'badge ' + (valide ? 'bon' : 'alerte') }, valide ? t('pas_surajuste') : t('surajustement_possible'))),
  el('p', { class: 'note' }, t('permutations_note')), graphe);
  const idx = (orig) => pts.correlation.map((_, i) => i).filter((i) => pts.origine[i] === orig);
  const nuage = (cle2, couleur, symbole) => [false, true].map((orig) => ({
    type: 'scatter', mode: 'markers', name: orig ? `${cle2} (${t('modele_origine')})` : cle2, showlegend: !orig,
    x: idx(orig).map((i) => pts.correlation[i]), y: idx(orig).map((i) => pts[cle2][i]),
    marker: { color: couleur, symbol: symbole, size: orig ? 12 : 8, line: { width: orig ? 1.5 : 0, color: '#333' } },
    hovertemplate: `${cle2} %{y:.3f}<br>r = %{x:.3f}<extra></extra>` }));
  const droite = (a, b, nom, couleur) => ({ type: 'scatter', mode: 'lines', name: `${nom} : ${fmt(a, 3)} + ${fmt(b, 3)} r`,
    x: [0, 1], y: [a, a + b], line: { color: couleur, dash: 'dash', width: 1.5 }, hoverinfo: 'skip' });
  tracer(ctx, graphe, [...nuage('R2', '#3b9a5b', 'circle'), ...nuage('Q2', '#2f6fb5', 'square'),
    droite(r.ordonnee_R2, r.pente_R2, 'R2', '#3b9a5b'), droite(r.ordonnee_Q2, r.pente_Q2, 'Q2', '#2f6fb5')],
  { title: { text: `${m.nom} · ${t('permutations')} · ${r.y} · ${r.n}` }, dragmode: 'zoom', showlegend: true,
    xaxis: { title: { text: t('correlation_permutee') }, range: [-0.05, 1.05], zeroline: true }, yaxis: { title: { text: 'R2 / Q2' } } });
}

// CV-ANOVA (Eriksson, Trygg and Wold 2008), one table per Y, computed in R.
function rendreCVANOVA(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const pf = (x) => (x === null || x === undefined ? '' : x < 0.001 ? Number(x).toExponential(2) : fmt(x, 4));
  conteneur.replaceChildren(el('div', { class: 'defile resume' },
    Object.entries(m.cv_anova || {}).map(([y, a]) => el('section', {},
      el('h4', { class: 'titre-section' }, `CV-ANOVA · ${y}`),
      el('table', { class: 'tableau compact' },
        el('thead', {}, el('tr', {}, ['', 'SS', 'DF', 'MS', 'F', 'p', 'SD'].map((h, k) => el('th', { class: k ? 'num' : '' }, h)))),
        el('tbody', {}, a.source.map((_, i) => el('tr', {}, el('td', {}, t('anova_' + i)),
          el('td', { class: 'num' }, fmt(a.SS[i], 3)), el('td', { class: 'num' }, a.DF[i]), el('td', { class: 'num' }, fmt(a.MS[i], 4)),
          el('td', { class: 'num' }, a.F[i] === null ? '' : fmt(a.F[i], 3)), el('td', { class: 'num' }, pf(a.p[i])),
          el('td', { class: 'num' }, fmt(a.SD[i], 4)))))),
      el('p', { class: 'note' }, a.p[1] !== null && a.p[1] < 0.05 ? t('anova_significatif') : t('anova_non_significatif')))),
    el('p', { class: 'note' }, t('anova_note'))));
}

// PLS-DA classification: misclassification table and ROC
// curves, with fitted values or cross-validated predictions.
function rendreClassification(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const c = m.classification;
  loc.mode ??= 'vc';
  const r = c[loc.mode];
  const classes = [].concat(r.classes);
  const tab = r.table;
  const somme = (v) => v.reduce((a, b) => a + b, 0);
  const pct = (x) => `${fmt(100 * x, 1)} %`;
  const table = el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, el('th', {}, `${t('observee')} \\ ${t('predite')}`), classes.map((k) => el('th', { class: 'num' }, k)),
      el('th', { class: 'num' }, t('total')), el('th', { class: 'num' }, t('bien_classes')))),
    el('tbody', {}, classes.map((k, i) => el('tr', {}, el('td', {}, k),
      tab[i].map((n, j) => el('td', { class: 'num' + (i === j ? ' diagonale' : n ? ' erreur-classe' : '') }, n)),
      el('td', { class: 'num' }, somme(tab[i])), el('td', { class: 'num' }, pct([].concat(r.correct)[i])))),
    el('tr', { class: 'total' }, el('td', {}, t('total')), classes.map((_, j) => el('td', { class: 'num' }, somme(tab.map((l) => l[j])))),
      el('td', { class: 'num' }, somme(tab.map(somme))), el('td', { class: 'num' }, pct(r.global)))));
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix('', [['vc', t('validation_croisee')], ['ajuste', t('valeurs_ajustees')]], loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    el('span', { class: 'infos' }, `${t('bien_classes')} : ${pct(r.global)}`)),
  el('p', { class: 'note' }, t('classification_note')), el('div', { class: 'tableau-court' }, table), graphe);
  const traces = r.roc.map((roc, j) => ({ type: 'scatter', mode: 'lines', name: `${classes[j]} (AUC ${fmt(roc.auc, 3)})`,
    x: roc.fpr, y: roc.tpr, line: { color: PALETTE[j % PALETTE.length], width: 2, shape: 'hv' } }));
  traces.push({ type: 'scatter', mode: 'lines', x: [0, 1], y: [0, 1], line: { color: '#999', dash: 'dot', width: 1 }, showlegend: false, hoverinfo: 'skip' });
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ROC` }, dragmode: 'zoom', showlegend: true,
    xaxis: { title: { text: t('faux_positifs') }, range: [0, 1] }, yaxis: { title: { text: t('vrais_positifs') }, range: [0, 1.02] } });
}

export const VUES = {
  resume: { rendre: rendreResume, titre: (m) => `${m.nom} · ${t('resume')}` },
  scores: { rendre: rendreScores, titre: (m) => `${m.nom} · ${t('scores')}` },
  loadings: { rendre: rendreLoadings, titre: (m) => `${m.nom} · ${t('loadings')}` },
  profils: { rendre: rendreProfils, titre: (m) => `${m.nom} · ${t('profils')}` },
  ipls: { rendre: rendreIPLS, titre: (m) => `${m.nom} · iPLS` },
  coomans: { rendre: rendreCoomans, titre: (m) => `${m.groupe || m.nom} · Coomans` },
  dmodx: { rendre: (c, x, p, l) => rendreLigneObs(c, x, p, l, 'dmodx'), titre: (m) => `${m.nom} · DModX` },
  t2: { rendre: (c, x, p, l) => rendreLigneObs(c, x, p, l, 't2'), titre: (m) => `${m.nom} · T²` },
  vip: { rendre: rendreVIP, titre: (m) => `${m.nom} · VIP` },
  coefficients: { rendre: rendreCoefficients, titre: (m) => `${m.nom} · ${t('coefficients')}` },
  obspred: { rendre: rendreObsPred, titre: (m) => `${m.nom} · ${t('obspred')}` },
  dendrogramme: { rendre: rendreDendrogramme, titre: (m) => `${m.nom} · ${t('dendrogramme')}` },
  predictions: { rendre: rendrePredictions, titre: (m) => `${m.nom} · ${t('predictions')}` },
  liste_obs: { rendre: rendreListeObs, titre: (m) => `${m.nom} · ${t('observations')}` },
  liste_vars: { rendre: rendreListeVars, titre: (m) => `${m.nom} · ${t('variables')}` },
  permutations: { rendre: rendrePermutations, titre: (m) => `${m.nom} · ${t('permutations')}` },
  contributions: { rendre: rendreContributions, titre: (m) => `${m.nom} · ${t('contributions')}` },
  biplot: { rendre: rendreBiplot, titre: (m) => `${m.nom} · Biplot` },
  splot: { rendre: rendreSplot, titre: (m) => `${m.nom} · S-plot` },
  scores3d: { rendre: rendreScores3D, titre: (m) => `${m.nom} · ${t('scores_3d')}` },
  cvanova: { rendre: rendreCVANOVA, titre: (m) => `${m.nom} · CV-ANOVA` },
  classification: { rendre: rendreClassification, titre: (m) => `${m.nom} · ${t('classification')}` },
  donnees: { rendre: rendreDonnees, titre: (_, p) => `${p.nom || ''} · ${t('donnees')}`, projet: true },
  statistiques: { rendre: rendreStatistiques, titre: (_, p) => `${p.nom || ''} · ${t('statistiques')}`, projet: true },
  tests: { rendre: rendreTests, titre: (_, p) => `${p.nom || ''} · ${t('tests_variables')}`, projet: true },
  historique: { rendre: rendreHistorique, titre: (_, p) => `${p.nom || ''} · ${t('historique')}`, projet: true },
  comparaison: { rendre: rendreComparaison, titre: () => t('comparaison'), global: true },
};
