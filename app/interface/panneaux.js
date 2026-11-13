// Views. Each draws one model result (or the project data) into a panel and is re-run when
// the language, the theme or the shared selection changes. `ctx` comes from app.js.
import { t, lire, ecrire, langueActuelle } from './i18n.js';
import { el, fmt, icone } from './ui.js';

export const PALETTES = {
  defaut: ['#2f6fb5', '#d1495b', '#3b9a5b', '#e09f3e', '#7b5ea7', '#2a9d8f', '#8c564b', '#c2549d', '#6b6b6b', '#a3a322'],
  daltonien: ['#0072b2', '#e69f00', '#009e73', '#d55e00', '#cc79a7', '#56b4e9', '#f0e442', '#000000'],  // Okabe & Ito
  gris: ['#000000', '#7a7a7a', '#b5b5b5', '#3d3d3d', '#9a9a9a', '#5c5c5c'],
};
export const PALETTE = [...PALETTES.defaut];
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

export function choix(libelle, options, valeur, changer) {
  return el('label', { class: 'champ' }, libelle ? el('span', {}, libelle) : null,
    el('select', { onchange: (e) => changer(e.target.value) },
      options.map(([v, txt]) => el('option', { value: v, selected: String(v) === String(valeur) }, txt))));
}

function case_(libelle, valeur, changer) {
  return el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: valeur, onchange: (e) => changer(e.target.checked) }), libelle);
}

export const outils = (...enfants) => el('div', { class: 'outils' }, enfants);
const espace = () => el('span', { class: 'espace' });

export function couleursTheme() {
  const cs = getComputedStyle(document.documentElement);
  const v = (n, d) => cs.getPropertyValue(n).trim() || d;
  return { texte: v('--texte', '#222'), doux: v('--texte-doux', '#666'), grille: v('--grille', '#e5e5e5'), fond: v('--fond-panneau', '#fff'),
    ligne: v('--ligne-graphe', '#9bb5d6') };
}

export function tracer(ctx, div, traces, layout = {}) {
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
export function brancherSelection(div, ctx, idModele) {
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
export function controleCouleur(ctx, m, loc) {
  const o = m.observations;
  const proj = ctx.projet(m.projet);
  const options = [['', t('aucune')]].concat(o.classe ? [['classe', t('classe')]] : [])
    .concat(ctx.groupesHCA[m.id] ? [['hca', t('dendrogramme')]] : [])
    .concat((proj ? proj.variables : []).map((v) => ['v' + v.uid, v.nom]));
  return choix(t('couleur'), options, loc.couleur, (v) => { loc.couleur = v; ctx.rafraichir(); });
}

// Label control of the same plots: none, the observation name, a secondary ID of the
// project, or the value of any variable.
export function controleEtiquettes(ctx, m, loc) {
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
export function pointsObservations(ctx, m, loc, xs, ys, { type = 'scattergl', survol = '%{x:.3f}, %{y:.3f}' } = {}) {
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
export function tableau(conteneur, ctx, loc, { colonnes, n, valeur, uid, idModele, nom, avant = [], cliquer, choisie, modifier }) {
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

// The test set's line of a summary (jeu_test.R).
export function ficheTest(m) {
  if (!m.test) return null;
  return el('div', { class: 'fiche-chiffres', title: t('jeu_test_aide') },
    el('span', {}, el('small', {}, t('jeu_test')), ' ', el('b', {}, m.test.n)),
    m.test.statistiques ? [].concat(m.test.statistiques.y).map((y, j) => [['RMSEP', 'RMSEP', 4], ['biais', t('biais'), 4], ['Q2ext', 'Q2ext', 3]]
      .map(([k, n, d]) => el('span', {}, el('small', {}, `${n}${[].concat(m.test.statistiques.y).length > 1 ? ` ${y}` : ''}`), ' ', el('b', {}, fmt([].concat(m.test.statistiques[k])[j], d))))) : null,
    m.test.classement ? el('span', {}, el('small', {}, t('bien_classes_test')), ' ', el('b', {}, `${fmt(100 * m.test.classement.bien_classees, 1)} %`)) : null,
    m.regression ? null : el('span', {}, el('small', {}, t('hors_domaine_court')), ' ', el('b', {}, m.test.hors_domaine)));
}

// Notes on the model, saved with its definition in the project. While typing, a redraw keeps
// the text and the cursor: retrouver() once the field is back in the page.
export function notesModele(conteneur, ctx, m) {
  const avant = conteneur.querySelector('.notes-modele');
  const enCours = avant && document.activeElement === avant ? [avant.value, avant.selectionStart, avant.selectionEnd] : null;
  const champ = el('textarea', { class: 'notes-modele', rows: 2, placeholder: t('notes_placeholder'), oninput: (e) => {
    clearTimeout(conteneur._minuterieNotes);
    const texte = e.target.value;
    conteneur._minuterieNotes = setTimeout(() => ctx.noterModele(m.id, texte), 700);
  } }, enCours ? enCours[0] : m.note || '');
  return { champ, retrouver: () => { if (enCours) { champ.focus(); champ.setSelectionRange(enCours[1], enCours[2]); } } };
}

function rendreResume(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  if (m.regression) { rendreResumeRegression(conteneur, ctx, m); return; }
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
      [['A', m.A], ['N', m.N], ['K', m.K], [t('groupes_vc'), m.vc_par ? `${m.G} (${m.vc_par})` : m.G]].map(([k, v]) => el('span', {}, el('small', {}, k), ' ', el('b', {}, v))),
      m.reglages && [].concat(m.reglages.spectres || []).length
        ? el('span', {}, el('small', {}, t('spectres')), ' ', el('b', {}, libelleSpectres(m.reglages.spectres))) : null,
      pls && m.RMSEE ? Object.entries(m.RMSEE).map(([y, v]) => el('span', {}, el('small', {}, `RMSEE ${y}`), ' ', el('b', {}, fmt(v, 4)))) : null,
      pls && m.RMSEcv ? Object.entries(m.RMSEcv).map(([y, v]) => el('span', {}, el('small', {}, `RMSEcv ${y}`), ' ', el('b', {}, fmt(v, 4)))) : null,
      pls && m.cv_anova ? Object.entries(m.cv_anova).map(([y, a]) => el('span', { title: 'CV-ANOVA' }, el('small', {}, `CV-ANOVA p ${y}`), ' ',
        el('b', {}, a.p[1] === null ? '—' : a.p[1] < 0.001 ? Number(a.p[1]).toExponential(1) : fmt(a.p[1], 3)))) : null,
      m.classification ? el('span', {}, el('small', {}, t('bien_classes_vc')), ' ', el('b', {}, `${fmt(100 * m.classification.vc.global, 1)} %`)) : null),
    ficheTest(m));
  const table = el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, el('th', {}, t('comp')), cols.map(([, n]) => el('th', { class: 'num' }, n)))),
    el('tbody', {}, c.composante.map((a, i) => el('tr', {}, el('td', {}, m.opls ? (a > (m.Ap || 1) ? `O${a - (m.Ap || 1)}` : (m.Ap || 1) > 1 ? `P${a}` : 'P') : a),
      cols.map(([k]) => el('td', { class: 'num' }, fmt(c[k][i]), s && s[k] && k !== 'valeur_propre' ? marque(c[k][i], s[k][i]) : null))))));
  const note = s ? el('p', { class: 'note' }, el('span', { class: 'marque egal' }, '='), ' ', el('span', { class: 'marque proche' }, '≈'), ' ',
    el('span', { class: 'marque diff' }, '≠'), ' ', t('valeurs_fichier'), '. ' + t('note_q2')) : null;
  const graphe = el('div', { class: 'graphe petit' });
  const notes = notesModele(conteneur, ctx, m);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ, table, note, graphe));
  notes.retrouver();
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
  if (m.regression) { rendreCoefficientsRegression(conteneur, ctx, m, loc); return; }
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
    hors ? ` · ${hors} ${t(m.regression ? 'extrapolations' : 'hors_domaine')}` : '',
    fichier && r.absentes && [].concat(r.absentes).length ? el('span', { class: 'erreur' }, ` · ${t('variables_absentes')} : ${[].concat(r.absentes).join(', ')}`) : '',
    // values of the file read as missing (text in a quantitative column: <0.5, nd...)
    fichier && r.note && [].concat(r.note).length ? el('span', { class: 'erreur' }, ` · ${[].concat(r.note).join(' ; ')}`) : '');
  loc.graphe ??= 'scores';
  const vueGraphe = o.DModX ? choix('', [['scores', t('scores')], ['dmodx', 'DModXPS']], loc.graphe, (v) => { loc.graphe = v; ctx.rafraichir(); }) : null;
  conteneur.replaceChildren(el('div', { class: 'outils' }, vueGraphe, infos, el('span', { class: 'espace' }),
    el('button', { onclick: () => ctx.exporterPrediction(m.id) }, icone('file-down'), t('exporter_court'))),
  m.regression ? el('p', { class: 'note' }, t('prediction_regression_note')) : null, haut, bas);
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
  // a regression (or another analysis that predicts its Y: a regression tree): Y predicted, against Y
  // observed when it is known, else by observation; another analysis's predictions: the table only
  if (m.regression || (m.analyse && m.noms_y && cols.includes(`${[].concat(m.noms_y)[0]} prédit`))) {
    const yp = cols.find((k) => k.endsWith(' prédit'));
    const yo = cols.find((k) => k.endsWith(' observé'));
    const avecObs = !!yo && o.nom.some((_, i) => o[yo][i] !== null && o[yp][i] !== null);
    const abscisse = (i) => (avecObs ? o[yo][i] : i + 1);
    const dans = o.nom.map((_, i) => i).filter((i) => o.dans_modele[i]);
    const survol = `<b>%{text}</b><br>${avecObs ? '%{x:.4g} → ' : ''}%{y:.4g}<extra>%{fullData.name}</extra>`;
    const vals = avecObs ? o.nom.map((_, i) => i).flatMap((i) => [o[yo][i], o[yp][i]]).filter((v) => v !== null) : [];
    tracer(ctx, haut, [
      { type: 'scattergl', mode: 'markers', name: t('dans_modele'), x: dans.map(abscisse), y: dans.map((i) => o[yp][i]), text: dans.map((i) => o.nom[i]),
        marker: { color: '#b8c2cf', size: 5 }, hovertemplate: survol },
      { type: 'scattergl', mode: 'markers', name: t('predictions'), x: ii.map(abscisse), y: ii.map((i) => o[yp][i]), text: ii.map((i) => o.nom[i]),
        marker: { color: couleur, size: 7, opacity: 0.9 }, hovertemplate: survol }],
    { title: { text: `${m.nom} · ${t('predictions')}${fichier ? ' · ' + r.fichier : ''}` }, dragmode: 'zoom', margin: { t: 30, b: 36 },
      shapes: vals.length ? [{ type: 'line', x0: Math.min(...vals), y0: Math.min(...vals), x1: Math.max(...vals), y1: Math.max(...vals), line: { color: '#888', dash: 'dot', width: 1 } }] : [],
      xaxis: { title: { text: avecObs ? yo : 'observation' } }, yaxis: { title: { text: yp } } });
    return;
  }
  if (m.analyse) { haut.remove(); return; }
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

// Transformations as the model dialog writes them (moteur/R/nouveau.R, lire_transformation):
// 'racine2', 'racine4', 'log10', 'log10:C2' (log10(x + C2)), 'type:C1:C2' for log10, ln, neglog
// (-log10), exp, lineaire (C1·x + C2) and logit (ln((x - C1) / (C2 - x)), between the bounds C1
// and C2), 'puissance:C1:C2:C3' for (C1·x + C2)^C3. 'autre': a type of a .usp file the engine
// doesn't know.
export const TYPES_TRANSFO = ['log10', 'ln', 'neglog', 'logit', 'exp', 'puissance', 'lineaire'];
export function lireTransfo(code) {
  if (!code) return null;
  const nommees = { racine2: [1, 0, 0.5], racine4: [1, 0, 0.25] };
  if (nommees[code]) return { type: 'puissance', c1: nommees[code][0], c2: nommees[code][1], c3: nommees[code][2] };
  if (code === 'log10') return { type: 'log10', c1: 1, c2: 0 };
  const [type, ...n] = String(code).split(':');
  const x = n.map(Number);
  if (!TYPES_TRANSFO.includes(type) || x.some((v) => !Number.isFinite(v))) return null;
  if (type === 'puissance') return x.length === 1 ? { type, c1: 1, c2: 0, c3: x[0] } : x.length === 3 ? { type, c1: x[0], c2: x[1], c3: x[2] } : null;
  if (type === 'log10' && x.length === 1) return { type, c1: 1, c2: x[0] };
  return x.length === 2 ? { type, c1: x[0], c2: x[1] } : null;
}
// The code of a transformation, under its usual name when it has one ('' for none: x^1).
export function codeTransfo(s) {
  if (s.type === 'puissance' && s.c1 === 1 && s.c2 === 0) {
    if (s.c3 === 1) return '';
    if (s.c3 === 0.5) return 'racine2';
    if (s.c3 === 0.25) return 'racine4';
  }
  if (s.type === 'log10' && s.c1 === 1) return s.c2 === 0 ? 'log10' : `log10:${s.c2}`;
  return s.type === 'puissance' ? `puissance:${s.c1}:${s.c2}:${s.c3}` : `${s.type}:${s.c1}:${s.c2}`;
}

// Name of a transformation: its usual name, or its formula ("ln(2·x + 1)").
export function libelleTransfo(tr) {
  if (!tr) return '';
  if (['racine2', 'racine4', 'log10', 'autre'].includes(tr)) return t('tr_' + tr);
  const s = lireTransfo(tr);
  if (!s) return tr;
  const nb = (v) => String(+Number(v).toPrecision(6)).replace('.', langueActuelle() === 'fr' ? ',' : '.');
  let a = s.c1 === 1 ? 'x' : s.c1 === -1 ? '−x' : `${nb(s.c1)}·x`;
  if (s.c2 > 0) a += ` + ${nb(s.c2)}`;
  if (s.c2 < 0) a += ` − ${nb(-s.c2)}`;
  switch (s.type) {
    case 'puissance': return s.c1 === 1 && s.c2 === 0 ? `x^${nb(s.c3)}` : `(${a})^${nb(s.c3)}`;
    case 'neglog': return `−log10(${a})`;
    case 'lineaire': return a;
    case 'logit': return s.c1 === 0 ? `ln(x / (${nb(s.c2)} − x))` : `ln((x − ${nb(s.c1)}) / (${nb(s.c2)} − x))`;
    default: return `${s.type}(${a})`;
  }
}

// x transformed as in R (moteur/R/pretraitement.R, transformer), null where undefined.
function transformer(x, tr) {
  const s = lireTransfo(tr);
  if (!s) return x;
  if (s.type === 'logit') return x.map((v) => (v === null || !(v > s.c1 && v < s.c2) ? null : Math.log((v - s.c1) / (s.c2 - v))));
  const f = {
    log10: (b) => (b > 0 ? Math.log10(b) : null), ln: (b) => (b > 0 ? Math.log(b) : null), neglog: (b) => (b > 0 ? -Math.log10(b) : null),
    exp: (b) => (Number.isFinite(Math.exp(b)) ? Math.exp(b) : null), lineaire: (b) => b,
    puissance: (b) => ((b < 0 && s.c3 !== Math.round(s.c3)) || (b === 0 && s.c3 < 0) ? null : b ** s.c3),
  }[s.type];
  return x.map((v) => (v === null ? null : f(s.c1 * v + s.c2)));
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

// Series in time (Plots › Series): a variable over time, a line per group (the sector) and a
// small plot per larger group (the site), in a grid with shared axes: the sectors of each site
// compared year by year. R gives the long data (moteur/R/series.R), one row per observation with
// a known time and value. Within a line, times one step apart (the line's own step, the smallest
// gap between its times) are joined, and a missing time breaks the line; a line that has the same
// time twice is no series: its points are left unjoined (R counts them, a note says so).
// Colours go by line within each panel (the palette, then dashes), the legend grouped by panel,
// so that 30 sectors stay readable. Optionally the mean of each panel's lines (a thicker line), a
// log scale, and events: times drawn as dashed lines in every panel. A click or a box selects
// observations, kept under the project's id (with the shared selection, its models' too: app.js).
const TIRETS = ['solid', 'dot', 'dash', 'dashdot'];
async function rendreSeries(conteneur, ctx, p, loc) {
  const proj = ctx.projet(p.projet);
  const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
  const ids = liste(proj.identifiants).map((i) => ({ valeur: `id:${i.rang}`, nom: i.nom, libelle: `${i.nom} (${t('identifiant')})` }));
  const vars = proj.variables.map((v) => ({ valeur: v.uid, nom: v.nom, libelle: v.nom, qualitative: v.qualitative }));
  const mesures = vars.filter((v) => !v.qualitative);
  if (!mesures.length) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('series_sans_mesure'))); return; }
  // the time: a quantitative variable or an ID (the year); lines and panels: a qualitative
  // variable or an ID, guessed from their names the first time (year, sector, site), then kept
  const temps = [...mesures, ...ids];
  const existe = (os, x) => os.some((o) => o.valeur === x);
  const cherche = (os, re) => (os.find((o) => re.test(o.nom)) || {}).valeur;
  if (!existe(temps, loc.temps)) loc.temps = cherche(temps, /ann[ée]e|year|date|temps|time|^an$/i) ?? temps[0].valeur;
  const variables = mesures.filter((o) => o.valeur !== loc.temps);
  if (!variables.length) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('series_sans_mesure'))); return; }
  if (!existe(variables, loc.variable)) loc.variable = variables[0].valeur;
  const lignes = [...vars.filter((v) => v.qualitative), ...ids].filter((o) => o.valeur !== loc.temps);
  if (loc.lignes === undefined) loc.lignes = cherche(lignes, /secteur|sector/i) ?? null;
  if (!existe(lignes, loc.lignes)) loc.lignes = null;
  const panneaux = lignes.filter((o) => o.valeur !== loc.lignes);
  if (loc.panneaux === undefined) loc.panneaux = cherche(panneaux, /site/i) ?? null;
  if (!existe(panneaux, loc.panneaux)) loc.panneaux = null;
  const lire = (x) => (x === '' ? null : String(x).startsWith('id:') ? x : Number(x));
  const options = (os) => os.map((o) => [o.valeur, o.libelle]);
  const evenements = String(loc.evenements || '').split(/[;\s]+/).filter(Boolean).map((x) => Number(x.replace(',', '.'))).filter(Number.isFinite);
  const barre = outils(
    choix(t('series_variable'), options(variables), loc.variable, (x) => { loc.variable = lire(x); ctx.rafraichir(); }),
    choix(t('series_temps'), options(temps), loc.temps, (x) => { loc.temps = lire(x); ctx.rafraichir(); }),
    choix(t('series_lignes'), [['', t('aucune')]].concat(options(lignes)), loc.lignes ?? '', (x) => { loc.lignes = lire(x); ctx.rafraichir(); }),
    choix(t('series_panneaux'), [['', t('aucun')]].concat(options(panneaux)), loc.panneaux ?? '', (x) => { loc.panneaux = lire(x); ctx.rafraichir(); }),
    case_(t('series_moyenne'), !!loc.moyenne, (x) => { loc.moyenne = x; ctx.rafraichir(); }),
    case_(t('series_log'), !!loc.log, (x) => { loc.log = x; ctx.rafraichir(); }),
    el('label', { class: 'champ', title: t('series_evenements_aide') }, el('span', {}, t('series_evenements')),
      el('input', { value: loc.evenements || '', placeholder: '2015 ; 2019', style: 'width:8em',
        onchange: (e) => { loc.evenements = e.target.value; ctx.rafraichir(); } })));
  const cache = (loc.cache ??= {});  // (the results: loc.cache isn't saved with the session)
  const cle = JSON.stringify([loc.variable, loc.temps, loc.lignes, loc.panneaux, langueActuelle()]);
  if (cache.cle !== cle) {
    conteneur.replaceChildren(barre, el('p', { class: 'infos attente' }, t('calcul')));
    const q = { projet: p.projet, variable: loc.variable, temps: loc.temps,
      ...(loc.lignes !== null ? { lignes: loc.lignes } : {}), ...(loc.panneaux !== null ? { panneaux: loc.panneaux } : {}) };
    try { cache.res = await ctx.demander('series', q); cache.erreur = null; } catch (e) { cache.res = null; cache.erreur = e.message; }
    cache.cle = cle;
  }
  if (!cache.res) { conteneur.replaceChildren(barre, el('p', { class: 'erreur' }, `${t('erreur')} : ${cache.erreur}`)); return; }
  const r = cache.res;
  const T = liste(r.points.temps);
  const Y = liste(r.points.valeur);
  const LI = liste(r.points.ligne);
  const PA = liste(r.points.panneau);
  const U = liste(r.points.uid);
  const N = liste(r.points.nom);
  if (!T.length) { conteneur.replaceChildren(barre, el('p', { class: 'infos' }, t('series_vide'))); return; }
  const parLignes = !!r.lignes;
  const parPanneaux = !!r.panneaux;
  const nomsP = parPanneaux ? liste(r.niveaux_panneaux) : [null];
  const nomsL = parLignes ? liste(r.niveaux_lignes) : [null];
  const montre = (y) => y !== null && (!loc.log || y > 0);  // (a log scale has no place for values ≤ 0)
  // a line's points (indices, sorted by time), with a null where it breaks: its own step, the
  // smallest gap between its times (a sector surveyed every other year stays joined while another
  // is surveyed every year), a larger gap breaks it
  const avecTrous = (idx, tps) => {
    const pas = idx.slice(1).reduce((min, i, m) => { const g = tps[i] - tps[idx[m]]; return g > 0 && g < min ? g : min; }, Infinity);
    return idx.flatMap((i, m) => (m && !(Number.isFinite(pas) && Math.abs(tps[i] - tps[idx[m - 1]] - pas) <= 1e-9 * Math.max(1, pas)) ? [null, i] : [i]));
  };
  const de = (valeurs, suite) => suite.map((i) => (i === null ? null : valeurs[i]));
  // the grid: the panels row by row; the bottom-left one has the main axes, and their titles
  const nP = nomsP.length;
  const C = nP <= 3 ? nP : Math.ceil(Math.sqrt(nP));
  const R = Math.ceil(nP / C);
  let numero = 1;
  const suffixe = nomsP.map((_, k) => (k === (R - 1) * C ? '' : String(++numero)));
  const gx = C > 1 ? 0.04 : 0;
  const gy = R > 1 ? Math.min(0.12, 0.35 / R) : 0;
  const c = couleursTheme();
  const sel = new Set(ctx.selection(p.projet));
  const axe = { gridcolor: c.grille, zerolinecolor: c.doux, linecolor: c.grille, showgrid: FORMAT.grille, automargin: true };
  const lay = { title: { text: [r.variable.nom, r.temps.nom, parLignes ? r.lignes.nom : '', parPanneaux ? r.panneaux.nom : ''].filter(Boolean).join(' · ') },
    showlegend: parLignes || !!loc.moyenne, margin: { l: 60, r: 18, t: parPanneaux ? 56 : 34, b: 48 }, annotations: [], shapes: [],
    legend: { orientation: 'v', x: 1.01, xanchor: 'left', y: 1, yanchor: 'top', traceorder: 'grouped', groupclick: 'toggleitem', font: { size: FORMAT.taille - 1 } } };
  const traces = [];
  const moyennes = [];
  const MP = liste(r.moyennes.panneau);
  const MT = liste(r.moyennes.temps);
  const MV = liste(r.moyennes.valeur);
  const MN = liste(r.moyennes.n);
  nomsP.forEach((nomP, k) => {
    const s = suffixe[k];
    const rang = Math.floor(k / C);
    const col = k % C;
    const dx = [col / C + (col ? gx / 2 : 0), (col + 1) / C - (col < C - 1 ? gx / 2 : 0)];
    const dy = [1 - (rang + 1) / R + (rang < R - 1 ? gy / 2 : 0), 1 - rang / R - (rang ? gy / 2 : 0)];
    // (the times under the lowest panel of each column, the values left of the first column)
    lay['xaxis' + s] = { ...axe, domain: dx, anchor: 'y' + s, showticklabels: k + C >= nP, ...(s ? { matches: 'x' } : { title: { text: r.temps.nom } }) };
    lay['yaxis' + s] = { ...axe, domain: dy, anchor: 'x' + s, showticklabels: col === 0, ...(loc.log ? { type: 'log' } : {}),
      ...(s ? { matches: 'y' } : { title: { text: r.variable.nom } }) };
    if (parPanneaux) lay.annotations.push({ text: nomP, xref: 'paper', yref: 'paper', x: dx[0], y: dy[1], xanchor: 'left', yanchor: 'bottom', showarrow: false, font: { size: FORMAT.taille } });
    for (const e of evenements) lay.shapes.push({ type: 'line', xref: 'x' + s, yref: `y${s} domain`, x0: e, x1: e, y0: 0, y1: 1, line: { dash: 'dash', color: c.doux, width: 1 } });
    const extra = parLignes ? '%{fullData.name}' : parPanneaux ? nomP : '';
    const dansP = T.map((_, i) => i).filter((i) => (!parPanneaux || PA[i] === nomP) && montre(Y[i]));
    nomsL.filter((nomL) => dansP.some((i) => !parLignes || LI[i] === nomL)).forEach((nomL, j) => {
      const points = dansP.filter((i) => !parLignes || LI[i] === nomL);
      // (a line with the same time twice isn't a series: its points, not joined)
      const doublee = points.some((i, m) => m && T[i] === T[points[m - 1]]);
      const suite = doublee ? points : avecTrous(points, T);
      const couleur = PALETTE[j % PALETTE.length];
      traces.push({ type: 'scatter', mode: doublee ? 'markers' : 'lines+markers', xaxis: 'x' + s, yaxis: 'y' + s, name: parLignes ? nomL : r.variable.nom,
        legendgroup: `p${k}`, showlegend: parLignes, ...(parPanneaux && parLignes && j === 0 ? { legendgrouptitle: { text: nomP } } : {}),
        x: de(T, suite), y: de(Y, suite), customdata: de(U, suite), hovertext: de(N, suite),
        hovertemplate: `<b>%{hovertext}</b><br>${r.temps.nom} = %{x}<br>${r.variable.nom} = %{y:.4~g}<extra>${extra}</extra>`,
        line: { color: couleur, width: 1.5, dash: TIRETS[Math.floor(j / PALETTE.length) % TIRETS.length] },
        marker: marqueurs(de(U, suite), sel, couleur, 6) });
    });
    if (loc.moyenne) {
      const suite = avecTrous(MT.map((_, i) => i).filter((i) => (!parPanneaux || MP[i] === nomP) && montre(MV[i])), MT);
      moyennes.push({ type: 'scatter', mode: 'lines', xaxis: 'x' + s, yaxis: 'y' + s, name: t('series_moyenne'), legendgroup: 'moyenne', showlegend: k === 0,
        x: de(MT, suite), y: de(MV, suite), text: de(MN, suite), line: { color: c.texte, width: 3.5 },
        hovertemplate: `${t('series_moyenne')} · ${r.temps.nom} = %{x}<br>${r.variable.nom} = %{y:.4~g} (n = %{text})<extra>${parPanneaux ? nomP : ''}</extra>` });
    }
  });
  const nonMontres = loc.log ? Y.filter((y) => !(y > 0)).length : 0;
  const d = r.doubles || {};
  const notes = [t('series_note', { n: T.length, l: parLignes ? nomsL.length : 1, p: nP }),
    r.sans_temps_valeur ? t('series_sans_temps', { n: r.sans_temps_valeur }) : '',
    r.sans_groupe ? t('series_sans_groupe', { n: r.sans_groupe }) : '',
    d.n ? t('series_doubles', { n: d.n, exemple: [d.panneau, d.ligne, d.temps].filter((x) => x !== null && x !== undefined).join(' · ') }) : '',
    nonMontres ? t('series_log_note', { n: nonMontres }) : '', sel.size ? `${sel.size} ${t('selectionnees')}` : ''].filter(Boolean).join(' ');
  // many panels: a taller plot, which scrolls
  const graphe = el('div', { class: 'graphe', ...(R > 3 ? { style: { height: `${R * 190 + 90}px`, flex: 'none' } } : {}) });
  conteneur.replaceChildren(barre, el('p', { class: 'note' }, notes), R > 3 ? el('div', { class: 'defile' }, graphe) : graphe);
  tracer(ctx, graphe, traces.concat(moyennes), lay);
  brancherSelection(graphe, ctx, p.projet);
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

// Statistical tests, computed in R, in four modes: between the classes of a qualitative
// variable, variable by variable; two variables, with the test their types call for; the
// correlation matrix of the quantitative variables; before/after with a control (BACI).
export const valeurP = (v) => (v === null || v === undefined ? '' : v === 0 ? '< 1e-16' : v < 1e-4 ? v.toExponential(1) : v.toFixed(4));  // (0 : sous la précision de R)
async function rendreTests(conteneur, ctx, p, loc) {
  loc.mode ??= 'classes';
  const modes = choix('', [['classes', t('tests_mode_classes')], ['deux', t('tests_mode_deux')], ['correlations', t('tests_mode_correlations')],
    ['baci', t('tests_mode_baci')]], loc.mode, (x) => { loc.mode = x; ctx.rafraichir(); });
  if (loc.mode === 'deux') return rendreTestDeux(conteneur, ctx, p, loc, modes);
  if (loc.mode === 'correlations') return rendreCorrelations(conteneur, ctx, p, loc, modes);
  if (loc.mode === 'baci') return rendreBaci(conteneur, ctx, p, loc, modes);
  return rendreTestsClasses(conteneur, ctx, p, loc, modes);
}

// Each quantitative variable compared between the classes of a qualitative one, p-values
// corrected for multiple testing. Two classes: t-test or Mann–Whitney, and a volcano plot
// (effect against -log10 p, the variables that pass both cut-offs coloured); all classes:
// ANOVA or Kruskal–Wallis, and the variables ranked. Two classes can be paired by a key (the
// year, for two sectors followed the same years): for each variable, the paired t-test or
// Wilcoxon's signed-rank test on the keys both classes have, d_z for d, the pairs counted.
async function rendreTestsClasses(conteneur, ctx, p, loc, modes) {
  const proj = ctx.projet(p.projet);
  const quals = proj.variables.filter((v) => v.qualitative && [].concat(v.categories).length >= 2);
  if (!quals.length) { conteneur.replaceChildren(outils(modes), el('p', { class: 'infos' }, t('tests_sans_classe'))); return; }
  const v = quals.find((x) => x.uid === loc.classe) || quals[0];
  loc.classe = v.uid;
  const cats = [].concat(v.categories);
  loc.comparaison ??= 'deux';
  const deux = loc.comparaison === 'deux';
  if (!cats.includes(loc.a)) loc.a = cats[0];
  if (!cats.includes(loc.b) || loc.b === loc.a) loc.b = cats.find((c) => c !== loc.a);
  // the pairing key: another variable or an observation ID
  const ids = [].concat(proj.identifiants || []).map((i) => [`id:${i.rang}`, `${i.nom} (${t('identifiant')})`]);
  const cles = deux ? proj.variables.filter((x) => x.uid !== v.uid).map((x) => [x.uid, x.nom]).concat(ids) : [];
  const apparie = cles.some(([u]) => u === loc.apparier);
  const tests = !deux ? [['anova', 'ANOVA'], ['kruskal', 'Kruskal–Wallis']]
    : apparie ? [['t_apparie', t('test_t_apparie')], ['wilcoxon', t('test_wilcoxon')]]
    : [['welch', t('test_welch')], ['student', t('test_student')], ['mann_whitney', 'Mann–Whitney']];
  // (the rank test stays the rank test when the pairing changes)
  if (apparie && loc.test === 'mann_whitney') loc.test = 'wilcoxon';
  if (!apparie && loc.test === 'wilcoxon') loc.test = 'mann_whitney';
  if (!tests.some(([k]) => k === loc.test)) loc.test = tests[0][0];
  loc.correction ??= 'BH';
  loc.seuil ??= 0.05;
  const lireCle = (x) => (String(x).startsWith('id:') ? x : Number(x));
  const barre = (...fin) => outils(modes,
    choix(t('classe_variable'), quals.map((q) => [q.uid, q.nom]), v.uid, (x) => { loc.classe = Number(x); ctx.rafraichir(); }),
    choix('', [['deux', t('deux_classes')], ['toutes', t('toutes_classes')]], loc.comparaison, (x) => { loc.comparaison = x; ctx.rafraichir(); }),
    deux ? choix('A', cats.map((c) => [c, c]), loc.a, (x) => { loc.a = x; ctx.rafraichir(); }) : null,
    deux ? choix('B', cats.filter((c) => c !== loc.a).map((c) => [c, c]), loc.b, (x) => { loc.b = x; ctx.rafraichir(); }) : null,
    deux ? choix(t('apparier_par'), [['', t('apparie_aucun')]].concat(cles), apparie ? loc.apparier : '',
      (x) => { loc.apparier = x === '' ? null : lireCle(x); ctx.rafraichir(); }) : null,
    choix(t('test'), tests, loc.test, (x) => { loc.test = x; ctx.rafraichir(); }),
    choix(t('correction'), [['BH', t('correction_bh')], ['bonferroni', 'Bonferroni'], ['aucune', t('aucune')]], loc.correction, (x) => { loc.correction = x; ctx.rafraichir(); }),
    ...fin);
  const cle = JSON.stringify([v.uid, deux ? [loc.a, loc.b] : null, loc.test, loc.correction, apparie ? loc.apparier : null, langueActuelle()]);
  if (loc.cle !== cle) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    // (an error, a key repeated within a class, shows under the selectors, which stay)
    try {
      loc.res = await ctx.demander('tests_variables', { projet: p.projet, classe: v.uid, groupes: deux ? [loc.a, loc.b] : null,
        test: loc.test, correction: loc.correction, ...(apparie ? { appariement: loc.apparier } : {}) });
    } catch (e) { loc.res = { erreur: e.message }; }
    loc.cle = cle;
  }
  if (loc.res.erreur) { conteneur.replaceChildren(barre(), el('p', { class: 'erreur' }, `${t('erreur')} : ${loc.res.erreur}`)); return; }
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
  const nomCle = apparie ? loc.res.cle.nom : '';
  const nomD = apparie ? 'd_z' : 'd';
  conteneur.replaceChildren(barre(
    choix(t('seuil_p'), [[0.01, '0,01'], [0.05, '0,05'], [0.1, '0,1']], loc.seuil, (x) => { loc.seuil = Number(x); ctx.rafraichir(); }),
    deux ? choix(t('effet'), [['log2', t('effet_log2')], ['d', t(apparie ? 'effet_dz' : 'effet_d')]], loc.axe, (x) => { loc.axe = x; ctx.rafraichir(); }) : null,
    deux ? choix(t('seuil_effet'), seuilsEffet, loc.effet, (x) => { loc.effet = Number(x); ctx.rafraichir(); }) : null),
    el('p', { class: 'note' }, t(!deux ? 'tests_note_toutes' : apparie ? 'tests_note_apparie' : 'tests_note_deux',
      { test: nomTest, correction: nomCorrection, a: loc.a, b: loc.b, n: nRetenues, total: n, cle: nomCle })),
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
      hovertemplate: `%{text}<br>${loc.axe === 'log2' ? 'log2(B/A)' : nomD} = %{x:.3f}<br>p = %{customdata[0]} · ${t('p_ajuste')} = %{customdata[1]}<extra></extra>` },
    { type: 'scatter', mode: 'text', x: etiquettes.map((i) => effet[i]), y: etiquettes.map((i) => y[i]), text: etiquettes.map((i) => noms[i]),
      textposition: 'top center', textfont: { size: FORMAT.taille - 2 }, hoverinfo: 'skip' }],
    { title: { text: `${t('volcan')} · ${loc.b} / ${loc.a}${apparie ? ` (${t('paires_par', { cle: nomCle })})` : ''}` }, showlegend: false, dragmode: 'zoom', shapes: formes,
      xaxis: { title: { text: loc.axe === 'log2' ? t('effet_log2_axe', { a: loc.a, b: loc.b }) : t(apparie ? 'effet_dz_axe' : 'effet_d_axe', { a: loc.a, b: loc.b }) } },
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
    colonnes: [{ cle: 'nom', titre: t('variables') }].concat(!deux ? [] : apparie ? [num('n_paires', t('n_paires'), 0)] : [num('n_a', 'n A', 0), num('n_b', 'n B', 0)], moyennes,
      deux ? [num('log2_rapport', 'log2(B/A)', 3), num('d', nomD, 3)] : [], [num('statistique', t('statistique'), 3), num('p', 'p', 4, valeurP), num('p_ajuste', t('p_ajuste'), 4, valeurP)]),
    valeur: (i, k) => (k === 'nom' ? noms[i] : [].concat(tab[k])[i]),
  });
}

// Two variables, with the test their types call for (R chooses it and checks its conditions):
// a contingency table and the chi-square or Fisher's exact test; a measurement compared
// between groups (t-test, ANOVA or their rank versions, and the pairwise comparisons); or a
// correlation. The recommended test's result comes as a sentence, ready for a report; with
// three groups or more, clicking another test shows its pairwise comparisons. A measurement
// and groups can also be paired by a third variable or ID (the year, for two sectors): the
// paired t-test and Wilcoxon's signed-rank test between two levels A and B, on the keys both
// have.
const NOMS_TESTS = () => ({ khi2: t('test_khi2'), fisher: t('test_fisher'), welch: t('test_welch'), student: t('test_student'), mann_whitney: 'Mann–Whitney',
  anova: 'ANOVA', welch_anova: t('test_welch_anova'), kruskal: 'Kruskal–Wallis', pearson: t('test_pearson'), spearman: t('test_spearman'),
  t_apparie: t('test_t_apparie'), wilcoxon: t('test_wilcoxon') });
const NOMS_EFFETS = () => ({ V: t('effet_v'), OR: t('effet_or'), d: t('effet_d'), dz: t('effet_dz'), r_rb: t('effet_rb'), eta2: 'η²', epsilon2: 'ε²', r: 'r', rho: 'ρ' });
const ddlTexte = (x) => (x === null || x === undefined ? '' : Math.abs(x - Math.round(x)) < 1e-8 ? String(Math.round(x)) : fmt(x, 1));
// (normality checked: p ≥ 0.05, or more than 5,000 observations; R's rule, comparaisons.R)
const normaliteDouteuse = (p, n) => p.map((v, i) => !(n[i] > 5000 || (v !== null && v >= 0.05)));

async function rendreTestDeux(conteneur, ctx, p, loc, modes) {
  const L = (loc.deux ??= {});
  const cache = ((loc.cache ??= {}).deux ??= {});  // (the results: loc.cache isn't saved with the session)
  const proj = ctx.projet(p.projet);
  const vars = proj.variables;
  // the observation IDs (site, sector, year...) make groups too, as a qualitative variable
  const ids = [].concat(proj.identifiants || []).map((i) => [`id:${i.rang}`, `${i.nom} (${t('identifiant')})`]);
  const options = vars.map((v) => [v.uid, v.qualitative ? `${v.nom} (${[].concat(v.categories).length} ${t('categories')})` : v.nom]).concat(ids);
  if (options.length < 2) { conteneur.replaceChildren(outils(modes), el('p', { class: 'infos' }, t('tests_deux_peu'))); return; }
  const existe = (v) => options.some(([u]) => u === v);
  if (!existe(L.x)) L.x = (vars.find((v) => v.qualitative) || {}).uid ?? (ids.length ? ids[0][0] : vars[0].uid);
  if (!existe(L.y) || L.y === L.x) L.y = (vars.find((v) => !v.qualitative && v.uid !== L.x) || {}).uid ?? options.find(([u]) => u !== L.x)[0];
  const lire = (x) => (String(x).startsWith('id:') ? x : Number(x));
  // a measurement and groups (a qualitative variable or an ID) can be paired by a third
  // variable; two measurements, by the observation (the same row holds both: before, after)
  const qualitative = (u) => String(u).startsWith('id:') || Boolean((vars.find((v) => v.uid === u) || {}).qualitative);
  const mesures = !qualitative(L.x) && !qualitative(L.y);
  const groupe = qualitative(L.x) === qualitative(L.y) ? null : qualitative(L.x) ? L.x : L.y;
  const candidates = groupe !== null ? options.filter(([u]) => u !== L.x && u !== L.y) : mesures ? [['observation', t('apparie_observation')]] : [];
  const apparie = candidates.some(([u]) => u === L.apparier);
  if (!apparie) L.apparier = null;  // (a pairing that no longer applies is forgotten, not kept for later)
  const parNiveaux = apparie && groupe !== null;
  if (parNiveaux && cache.groupe !== String(groupe)) {
    // the levels A and B are chosen from, there even when a pair of levels can't be tested
    try { cache.niveaux = [].concat((await ctx.demander('niveaux', { projet: p.projet, variable: groupe })).niveaux); } catch { cache.niveaux = []; }
    cache.groupe = String(groupe);
  }
  const niveaux = parNiveaux ? cache.niveaux : [];
  if (parNiveaux && !niveaux.includes(L.a)) L.a = niveaux[0];
  if (parNiveaux && (!niveaux.includes(L.b) || L.b === L.a)) L.b = niveaux.find((c) => c !== L.a);
  // with three levels or more, the table of all their pairs can keep to the levels of the same
  // group (the sectors of a site): a qualitative variable or an ID other than the three above
  const candidatsDans = parNiveaux && niveaux.length >= 3 ? options.filter(([u]) => u !== L.x && u !== L.y && u !== L.apparier && qualitative(u)) : [];
  const dans = candidatsDans.some(([u]) => u === L.dans) ? L.dans : null;
  L.dans = dans;
  const barre = outils(modes,
    choix(t('tests_var1'), options, L.x, (x) => { L.x = lire(x); ctx.rafraichir(); }),
    choix(t('tests_var2'), options.filter(([u]) => u !== L.x), L.y, (x) => { L.y = lire(x); ctx.rafraichir(); }),
    candidates.length ? choix(t('apparier_par'), [['', t(mesures ? 'apparie_aucun_cor' : 'apparie_aucun')]].concat(candidates), apparie ? L.apparier : '',
      (x) => { L.apparier = x === '' ? null : x === 'observation' ? x : lire(x); ctx.rafraichir(); }) : null,
    candidatsDans.length ? choix(t('paires_dans'), [['', t('paires_dans_aucun')]].concat(candidatsDans), dans ?? '',
      (x) => { L.dans = x === '' ? null : lire(x); if (L.dans !== null) L.tableauPaires = 'toutes'; ctx.rafraichir(); }) : null,
    parNiveaux ? choix('A', niveaux.map((c) => [c, c]), L.a, (x) => { L.a = x; ctx.rafraichir(); }) : null,
    parNiveaux ? choix('B', niveaux.filter((c) => c !== L.a).map((c) => [c, c]), L.b, (x) => { L.b = x; ctx.rafraichir(); }) : null);
  // (the language too: R writes the sentence)
  const cle = JSON.stringify(apparie ? [L.x, L.y, L.apparier, L.a, L.b, dans, langueActuelle()] : [L.x, L.y, langueActuelle()]);
  if (cache.cle !== cle) {
    conteneur.replaceChildren(barre, el('p', { class: 'infos attente' }, t('calcul')));
    const q = { projet: p.projet, x: L.x, y: L.y, ...(apparie ? { appariement: L.apparier, niveaux: parNiveaux ? [L.a, L.b] : null, ...(dans !== null ? { dans } : {}) } : {}) };
    try { cache.res = await ctx.demander('test_deux_variables', q); cache.erreur = null; } catch (e) { cache.res = null; cache.erreur = e.message; }
    cache.cle = cle;
    // (another pair A, B, or a change of the data, keeps the test chosen for the table of all
    // pairs: kept out of the cache, which a change of the data empties)
    const base = JSON.stringify([L.x, L.y, apparie ? L.apparier : null]);
    if (L.baseTest !== base) L.test = null;
    L.baseTest = base;
  }
  if (!cache.res) { conteneur.replaceChildren(barre, el('p', { class: 'erreur' }, `${t('erreur')} : ${cache.erreur}`)); return; }
  const r = cache.res;
  const liste = (xs) => (xs.length > 8 ? `${xs.slice(0, 8).join(', ')}… (${xs.length})` : xs.join(', '));
  // every pair of levels (within each group, if one is chosen), with the columns of the test
  // chosen above and why a pair wasn't tested; a click on a row compares that pair
  const nomToutes = r.dans ? t('paires_dans_groupe', { groupe: r.dans.nom }) : t('toutes_paires');
  const tableauToutes = (bas, avant) => {
    const tp = (k) => [].concat(r.toutes[k]);
    const wilcoxon = L.test === 'wilcoxon';
    // (the same keys for both tests: a table sorted by p stays so when the test changes)
    const champ = { difference: wilcoxon ? 'mediane' : 'difference', p: wilcoxon ? 'p_w' : 'p_t', p_holm: wilcoxon ? 'p_w_holm' : 'p_t_holm' };
    const valeur = (i, k) => tp(champ[k] || k)[i];
    const num = (cle, titreCol, decimales, format, alerte) => ({ cle, titre: titreCol, num: true, decimales, format, alerte });
    const colonnes = [...(r.dans ? [{ cle: 'groupe', titre: r.dans.nom }] : []), { cle: 'a', titre: 'A' }, { cle: 'b', titre: 'B' }, num('n', 'n', 0),
      num('difference', t(wilcoxon ? 'difference_mediane_ba' : 'difference_ba'), 3),
      ...(wilcoxon ? [] : [num('bas', t('ic_bas'), 3), num('haut', t('ic_haut'), 3)]),
      num('p', 'p', 4, valeurP), num('p_holm', t('p_holm'), 4, valeurP, (i) => valeur(i, 'p_holm') !== null && valeur(i, 'p_holm') <= 0.05),
      { cle: 'non_teste', titre: t('non_teste') }];
    L.tableToutes ??= {};
    if (L.tableToutes.tri && !colonnes.some((c) => c.cle === L.tableToutes.tri.cle)) delete L.tableToutes.tri;
    tableau(bas, ctx, L.tableToutes, {
      n: tp('a').length, nom: nomToutes, avant, colonnes, valeur,
      cliquer: (i) => { L.a = tp('a')[i]; L.b = tp('b')[i]; ctx.rafraichir(); },
      choisie: (i) => [tp('a')[i], tp('b')[i]].sort().join('\u0000') === [r.a, r.b].sort().join('\u0000'),
    });
  };
  if (r.type === 'apparie' && r.erreur) {
    // the pair A–B can't be tested: why, and the table of all pairs still there
    const bas = el('div', { class: 'bas' });
    conteneur.replaceChildren(barre, el('p', { class: 'erreur' }, `${t('erreur')} : ${r.erreur}`),
      r.sans_groupe ? el('p', { class: 'note' }, t('paires_sans_groupe', { groupe: r.dans.nom, niveaux: liste([].concat(r.sans_groupe)) })) : '', bas);
    tableauToutes(bas, [el('b', {}, nomToutes)]);
    return;
  }
  const col = (k) => [].concat(r.tests[k]);
  const cles = col('test');
  if (!cles.includes(L.test)) L.test = r.conseille;
  const noms = NOMS_TESTS();
  const effets = NOMS_EFFETS();
  const tableTests = el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, [t('test'), t('statistique'), t('ddl'), 'p', t('effet'), ''].map((h, j) => el('th', { class: j >= 1 && j <= 4 ? 'num' : '' }, h)))),
    el('tbody', {}, cles.map((k, i) => el('tr', { class: k === L.test ? 'choisie' : '', onclick: r.posthoc || r.toutes ? () => { L.test = k; ctx.rafraichir(); } : null },
      el('td', {}, noms[k]),
      el('td', { class: 'num' }, fmt(col('statistique')[i], 3)),
      el('td', { class: 'num' }, [ddlTexte(col('ddl1')[i]), ddlTexte(col('ddl2')[i])].filter(Boolean).join(', ')),
      el('td', { class: 'num' }, valeurP(col('p')[i])),
      el('td', { class: 'num' }, col('effet')[i] === null ? '' : `${effets[col('nom_effet')[i]] || ''} = ${fmt(col('effet')[i], 3)}`),
      el('td', {}, k === r.conseille ? el('b', {}, t('conseille')) : '')))));
  // why this test: its conditions, checked in R
  const groupes = r.groupes ? [].concat(r.groupes.groupe) : [];
  const decimal = (s) => (langueActuelle() === 'fr' ? s.replace('.', ',') : s);  // (in a sentence)
  let raison;
  if (r.type === 'apparie') {
    // (normality not tested: constant differences, more than 5,000 pairs, Shapiro–Wilk impossible)
    const cleRaison = { normalite: 'raison_normalite_apparie', constantes: 'raison_constantes_apparie', nombre: 'raison_nombre_apparie',
      non_testee: 'raison_non_testee_apparie' }[r.raison] || 'raison_ok_apparie';
    raison = t(cleRaison, { p: r.shapiro_p === null ? '—' : decimal(valeurP(r.shapiro_p)) });
  } else if (r.raison === 'attendus') raison = t('raison_attendus', { part: `${fmt(100 * r.part_attendus_faibles, 0)} %`, min: decimal(fmt(r.attendu_min, 2)) });
  else if (r.raison === 'variances') raison = t('raison_variances', { p: decimal(valeurP(r.variances.p)) });
  else if (r.raison === 'normalite' && r.type === 'groupes') {
    const d = normaliteDouteuse([].concat(r.groupes.shapiro_p), [].concat(r.groupes.n));
    raison = t('raison_normalite_groupes', { groupes: liste(groupes.filter((_, i) => d[i])) });
  } else if (r.raison === 'normalite') {
    const d = normaliteDouteuse([].concat(r.normalite), [r.n, r.n]);
    raison = t('raison_normalite_cor', { variables: [r.x.nom, r.y.nom].filter((_, i) => d[i]).join(', ') });
  } else {
    raison = r.type === 'contingence' ? t('raison_ok_contingence') : r.type === 'correlation' ? t('raison_ok_correlation')
      : groupes.length === 2 ? t('raison_ok_deux_groupes') : t('raison_ok_groupes', { p: decimal(valeurP(r.variances.p)) });
  }
  const ecartes = [].concat(r.ecartes ?? []);
  // paired: how many pairs, the rows left without a partner (with their keys), and without a value
  const seules = r.type === 'apparie' ? [[r.a, [].concat(r.sans_partenaire.a)], [r.b, [].concat(r.sans_partenaire.b)]].filter(([, l]) => l.length) : [];
  const nSeules = seules.reduce((n, [, l]) => n + l.length, 0);
  const notes = (r.type === 'apparie' && r.ligne ? [raison, r.manquantes ? t('tests_manquantes', { n: r.manquantes }) : '']
    : r.type === 'apparie'
    ? [raison, t('apparie_paires', { n: r.n, cle: r.cle.nom, a: r.a, b: r.b }),
      nSeules ? t('apparie_seules', { n: nSeules, detail: seules.map(([g, l]) => `${g} (${liste(l)})`).join(langueActuelle() === 'fr' ? ' ; ' : '; ') }) : '',
      r.manquantes ? t('apparie_manquantes', { n: r.manquantes, a: r.a, b: r.b }) : '',
      r.sans_groupe ? t('paires_sans_groupe', { groupe: r.dans.nom, niveaux: liste([].concat(r.sans_groupe)) }) : '', r.note_comparaisons || '']
    : [raison, r.manquantes ? t('tests_manquantes', { n: r.manquantes }) : '', ecartes.length ? t('tests_ecartes', { groupes: liste(ecartes) }) : '',
      r.note_comparaisons || '']).filter(Boolean).join(' ');
  const copier = el('button', { class: 'icone-seule', title: t('copier_phrase'),
    onclick: () => navigator.clipboard.writeText(r.phrase).then(() => ctx.statut(t('phrase_copiee'))) }, icone('copy'));
  const graphe = el('div', { class: 'graphe' });
  const details = el('div', { class: 'defile', style: { flex: '0 1 auto', maxHeight: '40%' } });
  const bas = el('div', { class: 'bas' });
  const ph = r.posthoc && r.posthoc[L.test];
  conteneur.replaceChildren(...[barre, el('div', { class: 'note' }, el('b', {}, r.phrase), ' ', copier, el('br'), notes), graphe, details,
    ph || r.type === 'apparie' ? bas : null].filter(Boolean));

  if (r.type === 'apparie') {
    // the two levels' boxes, each pair joined by a line: red where B is higher, blue where lower
    const pc = (k) => [].concat(r.paires[k]);
    const cl = pc('cle');
    const va = pc('a');
    const vb = pc('b');
    const dif = pc('difference');
    const nomsObs = [pc('nom_a'), pc('nom_b')];
    // (two measurements of the same rows: the two categories are their names)
    const mesure = r.ligne ? '' : r.x.qualitative ? r.y.nom : r.x.nom;
    const facteur = r.ligne ? '' : r.x.qualitative ? r.x.nom : r.y.nom;
    const c = couleursTheme();
    const traces = [[r.a, va], [r.b, vb]].map(([g, v]) => ({ type: 'box', name: g, x: v.map(() => g), y: v, boxpoints: false, boxmean: true,
      fillcolor: 'rgba(0,0,0,0)', line: { width: 1, color: c.doux }, showlegend: false, hoverinfo: 'skip' }));
    for (const [garde, couleur, signe] of [[(d) => d > 0, '#d1495b', '>'], [(d) => d < 0, PALETTE[0], '<'], [(d) => d === 0, c.doux, '=']]) {
      const idx = dif.map((_, i) => i).filter((i) => garde(dif[i]));
      if (!idx.length) continue;
      traces.push({ type: 'scatter', mode: 'lines+markers', name: `${r.b} ${signe} ${r.a} (${idx.length})`,
        x: idx.flatMap(() => [r.a, r.b, null]), y: idx.flatMap((i) => [va[i], vb[i], null]),
        text: idx.flatMap((i) => [cl[i], cl[i], null]), customdata: idx.flatMap((i) => [nomsObs[0][i], nomsObs[1][i], null]),
        line: { color: couleur, width: 1 }, marker: { color: couleur, size: 6 * FORMAT.points },
        hovertemplate: r.ligne ? `%{customdata}<br>%{x} = %{y}<extra></extra>` : `%{customdata} · ${r.cle.nom} %{text}<br>${mesure} = %{y}<extra></extra>` });
    }
    tracer(ctx, graphe, traces, { title: { text: `${mesure ? `${mesure} · ` : ''}${r.b} − ${r.a}` }, showlegend: true, dragmode: 'zoom',
      xaxis: { title: { text: facteur }, type: 'category' }, yaxis: { title: { text: mesure } } });
    details.replaceChildren(tableTests);
    // below: the pairs of A and B, or, with three levels or more, every pair of levels (within
    // each group, if one is chosen) with the chosen test (a click on a test above), a click on a
    // row comparing that pair
    L.tableauPaires ??= 'paires';
    const avant = r.toutes ? [choix('', [['paires', t('paires_ab', { a: r.a, b: r.b })], ['toutes', nomToutes]], L.tableauPaires,
      (x) => { L.tableauPaires = x; ctx.rafraichir(); })] : [el('b', {}, t('paires'))];
    if (r.toutes && L.tableauPaires === 'toutes') {
      tableauToutes(bas, avant);
      return;
    }
    L.tablePaires ??= {};
    const colonnes = { cle: cl, a: va, b: vb, difference: dif };
    tableau(bas, ctx, L.tablePaires, {
      n: cl.length, nom: t('paires'), avant,
      colonnes: [{ cle: 'cle', titre: r.cle.nom }, { cle: 'a', titre: r.a, num: true, decimales: 3 }, { cle: 'b', titre: r.b, num: true, decimales: 3 },
        { cle: 'difference', titre: `${r.b} − ${r.a}`, num: true, decimales: 3 }],
      valeur: (i, k) => colonnes[k][i],
    });
    return;
  }

  if (r.type === 'contingence') {
    const lignes = [].concat(r.lignes);
    const colonnes = [].concat(r.colonnes);
    const eff = r.effectifs;
    const totalL = eff.map((l) => l.reduce((a, b) => a + b, 0));
    const totalC = colonnes.map((_, j) => eff.reduce((a, l) => a + l[j], 0));
    const signe = (z) => (z > 1.96 ? { color: '#d1495b', fontWeight: '600' } : z < -1.96 ? { color: PALETTE[0], fontWeight: '600' } : {});
    details.replaceChildren(tableTests, el('table', { class: 'tableau compact', style: { marginTop: '10px' } },
      el('thead', {}, el('tr', {}, el('th', {}, `${r.x.nom} / ${r.y.nom}`), colonnes.map((c) => el('th', { class: 'num' }, c)), el('th', { class: 'num' }, t('total')))),
      el('tbody', {}, lignes.map((l, i) => el('tr', {}, el('td', {}, l),
        colonnes.map((_, j) => el('td', { class: 'num', style: signe(r.residus[i][j]),
          title: t('case_contingence', { attendu: fmt(r.attendus[i][j], 1), residu: fmt(r.residus[i][j], 2) }) }, String(eff[i][j]))),
        el('td', { class: 'num' }, String(totalL[i])))),
      el('tr', {}, el('td', {}, el('b', {}, t('total'))), totalC.map((n) => el('td', { class: 'num' }, String(n))), el('td', { class: 'num' }, String(r.n))))),
    el('p', { class: 'note' }, t('contingence_note')));
    tracer(ctx, graphe, colonnes.map((c, j) => ({ type: 'bar', name: c, x: lignes, y: eff.map((l, i) => (100 * l[j]) / totalL[i]), customdata: eff.map((l) => l[j]),
      marker: { color: PALETTE[j % PALETTE.length] }, hovertemplate: `%{x} · ${c}<br>%{y:.1f} % (%{customdata})<extra></extra>` })),
    { barmode: 'stack', showlegend: true, dragmode: false, title: { text: t('repartition', { x: r.x.nom, y: r.y.nom }) },
      xaxis: { title: { text: r.x.nom } }, yaxis: { title: { text: '%' }, range: [0, 100] } });
    return;
  }

  if (r.type === 'correlation') {
    const x = [].concat(r.valeurs.x);
    const y = [].concat(r.valeurs.y);
    const xmin = Math.min(...x);
    const xmax = Math.max(...x);
    const traces = [{ type: 'scatter', mode: 'markers', x, y, text: [].concat(r.valeurs.nom), marker: { color: PALETTE[0], size: 6 },
      hovertemplate: `%{text}<br>${r.x.nom} = %{x}<br>${r.y.nom} = %{y}<extra></extra>` }];
    if (r.pente !== null) {
      traces.push({ type: 'scatter', mode: 'lines', x: [xmin, xmax], y: [r.ordonnee + r.pente * xmin, r.ordonnee + r.pente * xmax],
        line: { color: '#d1495b', width: 1.5 }, hoverinfo: 'skip' });
    }
    tracer(ctx, graphe, traces, { title: { text: `${r.y.nom} · ${r.x.nom}` }, showlegend: false, dragmode: 'zoom',
      xaxis: { title: { text: r.x.nom } }, yaxis: { title: { text: r.y.nom } } });
    details.replaceChildren(tableTests);
    return;
  }

  // a measurement between groups: box plots (the mean as a dashed line), the groups, and the
  // pairwise comparisons of the chosen test
  const g = r.groupes;
  const gc = (k) => [].concat(g[k]);
  const doute = normaliteDouteuse(gc('shapiro_p'), gc('n'));
  details.replaceChildren(tableTests, el('table', { class: 'tableau compact', style: { marginTop: '10px' } },
    el('thead', {}, el('tr', {}, [t('groupe'), 'n', t('moyenne'), t('ecart_type'), t('mediane'), 'Q1', 'Q3', t('shapiro_p')].map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
    el('tbody', {}, groupes.map((nom, i) => el('tr', {}, el('td', {}, nom), el('td', { class: 'num' }, String(gc('n')[i])),
      ['moyenne', 'ecart_type', 'mediane', 'q1', 'q3'].map((k) => el('td', { class: 'num' }, fmt(gc(k)[i], 3))),
      el('td', { class: 'num' + (doute[i] ? ' alerte' : '') }, valeurP(gc('shapiro_p')[i])))))));
  const mesure = r.x.qualitative ? r.y.nom : r.x.nom;
  const facteur = r.x.qualitative ? r.x.nom : r.y.nom;
  const grp = [].concat(r.valeurs.groupe);
  const val = [].concat(r.valeurs.valeur);
  const nm = [].concat(r.valeurs.nom);
  tracer(ctx, graphe, groupes.map((gr, k) => {
    const idx = grp.map((_, i) => i).filter((i) => grp[i] === gr);
    return { type: 'box', name: gr, y: idx.map((i) => val[i]), text: idx.map((i) => nm[i]), boxpoints: idx.length <= 500 ? 'all' : 'outliers',
      jitter: 0.4, pointpos: 0, boxmean: true, marker: { color: PALETTE[k % PALETTE.length], size: 4 }, line: { width: 1 } };
  }), { title: { text: `${mesure} · ${facteur}` }, showlegend: false, dragmode: 'zoom', xaxis: { title: { text: facteur } }, yaxis: { title: { text: mesure } } });
  if (!ph) return;
  const pc = (k) => [].concat(ph[k]);
  const dunn = L.test === 'kruskal';
  const titre = t(dunn ? 'posthoc_dunn' : L.test === 'welch_anova' ? 'posthoc_gh' : 'posthoc_tukey');
  const num = (cle, titreCol, decimales, format, alerte) => ({ cle, titre: titreCol, num: true, decimales, format, alerte });
  L.table ??= {};
  tableau(bas, ctx, L.table, {
    n: pc('a').length, nom: titre, avant: [el('b', {}, titre)],
    colonnes: [{ cle: 'a', titre: 'A' }, { cle: 'b', titre: 'B' }].concat(dunn
      ? [num('z', 'z', 3), num('p', 'p', 4, valeurP), num('p_ajuste', t('p_holm'), 4, valeurP, (i) => pc('p_ajuste')[i] <= 0.05)]
      : [num('difference', t('difference_ba'), 3), num('bas', t('ic_bas'), 3), num('haut', t('ic_haut'), 3), num('p', t('p_ajuste'), 4, valeurP, (i) => pc('p')[i] <= 0.05)]),
    valeur: (i, k) => pc(k)[i],
  });
}

// The correlation matrix of the quantitative variables (Pearson or Spearman), p-values
// corrected for the number of pairs. Clicking a pair, in the table or the plot, tests it in
// the "Two variables" mode.
async function rendreCorrelations(conteneur, ctx, p, loc, modes) {
  const C = (loc.cor ??= {});
  const cache = ((loc.cache ??= {}).cor ??= {});  // (the results: loc.cache isn't saved with the session)
  C.methode ??= 'pearson';
  C.correction ??= 'BH';
  C.seuil ??= 0.05;
  const seuils = [[0.01, '0,01'], [0.05, '0,05'], [0.1, '0,1']];
  const barre = outils(modes,
    choix(t('methode'), [['pearson', 'Pearson'], ['spearman', 'Spearman']], C.methode, (x) => { C.methode = x; ctx.rafraichir(); }),
    choix(t('correction'), [['BH', t('correction_bh')], ['bonferroni', 'Bonferroni'], ['aucune', t('aucune')]], C.correction, (x) => { C.correction = x; ctx.rafraichir(); }),
    choix(t('seuil_p'), seuils, C.seuil, (x) => { C.seuil = Number(x); ctx.rafraichir(); }));
  const cle = `${C.methode}|${C.correction}`;
  if (cache.cle !== cle) {
    conteneur.replaceChildren(barre, el('p', { class: 'infos attente' }, t('calcul')));
    try { cache.res = await ctx.demander('correlations_variables', { projet: p.projet, methode: C.methode, correction: C.correction }); cache.erreur = null; } catch (e) { cache.res = null; cache.erreur = e.message; }
    cache.cle = cle;
  }
  if (!cache.res) { conteneur.replaceChildren(barre, el('p', { class: 'erreur' }, `${t('erreur')} : ${cache.erreur}`)); return; }
  const r = cache.res;
  const noms = [].concat(r.noms);
  const uid = [].concat(r.uid);
  const K = noms.length;
  const paires = [];
  for (let i = 0; i < K; i += 1) for (let j = i + 1; j < K; j += 1) paires.push([i, j]);
  const significative = (k) => { const [i, j] = paires[k]; return r.p_ajuste[i][j] !== null && r.p_ajuste[i][j] <= C.seuil; };
  const nSig = paires.filter((_, k) => significative(k)).length;
  // (a correlation: no pairing left over from an earlier test)
  const tester = (i, j) => { loc.mode = 'deux'; Object.assign((loc.deux ??= {}), { x: uid[i], y: uid[j], apparier: null, dans: null }); ctx.rafraichir(); };
  const lettre = C.methode === 'pearson' ? 'r' : 'ρ';
  const nomCorrection = { BH: t('correction_bh'), bonferroni: 'Bonferroni', aucune: t('aucune') }[C.correction];
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(barre, el('p', { class: 'note' }, t('cor_note', { k: K, paires: paires.length, n: nSig,
    seuil: Object.fromEntries(seuils)[C.seuil], correction: nomCorrection })), graphe, bas);
  const survol = r.r.map((l, i) => l.map((v, j) => (i === j ? noms[i]
    : `${noms[i]} × ${noms[j]}<br>${lettre} = ${fmt(v, 3)} · n = ${r.n[i][j]}<br>${t('p_ajuste')} = ${valeurP(r.p_ajuste[i][j])}`)));
  tracer(ctx, graphe, [{ type: 'heatmap', x: noms, y: noms, z: r.r, zmin: -1, zmax: 1, text: survol, hovertemplate: '%{text}<extra></extra>',
    colorscale: [[0, '#2166ac'], [0.5, '#f7f7f7'], [1, '#b2182b']], xgap: K <= 40 ? 1 : 0, ygap: K <= 40 ? 1 : 0, colorbar: { title: { text: lettre }, thickness: 12 } }],
  { title: { text: t('cor_titre', { methode: C.methode === 'pearson' ? 'Pearson' : 'Spearman' }) }, dragmode: 'zoom',
    xaxis: { tickangle: -45, tickfont: { size: 9 }, showticklabels: K <= 80 }, yaxis: { autorange: 'reversed', tickfont: { size: 9 }, showticklabels: K <= 80 } });
  graphe.on('plotly_click', (e) => {
    const pt = e.points && e.points[0];
    const i = pt ? noms.indexOf(pt.y) : -1;
    const j = pt ? noms.indexOf(pt.x) : -1;
    if (i >= 0 && j >= 0 && i !== j) tester(i, j);
  });
  C.table ??= { tri: { cle: 'p_ajuste', sens: 1 } };
  tableau(bas, ctx, C.table, {
    n: paires.length, nom: 'Correlations',
    colonnes: [{ cle: 'a', titre: t('tests_var1') }, { cle: 'b', titre: t('tests_var2') }, { cle: 'r', titre: lettre, num: true, decimales: 3 },
      { cle: 'n', titre: 'n', num: true, decimales: 0 }, { cle: 'p', titre: 'p', num: true, format: valeurP },
      { cle: 'p_ajuste', titre: t('p_ajuste'), num: true, format: valeurP, alerte: significative }],
    valeur: (k, c) => { const [i, j] = paires[k]; return c === 'a' ? noms[i] : c === 'b' ? noms[j] : r[c][i][j]; },
    cliquer: (k) => tester(...paires[k]),
  });
}

// Before/after with a control (BACI), as her script 13 (section 4): at each time (the year), the
// gap d_t between the mean of the impact units' rows and that of the control units' rows; the
// gaps after the event (its first "after" time, up to an optional end) compared with those
// before, by Welch's t (the default), Student's t (her script's lm on the period) or
// Mann–Whitney, with the change in the mean gap, its 95 % interval and a sentence for a report.
// The plot shows d_t over time, the event and each period's mean; until an event is entered,
// the gaps alone.
const NOMS_PERIODES = () => ({ avant: t('baci_avant'), apres: t('baci_apres'), hors: t('baci_hors') });
const texteTemps = (x) => (x === null || x === undefined ? '' : Number.isInteger(x) ? String(x) : String(+x.toPrecision(6)));
async function rendreBaci(conteneur, ctx, p, loc, modes) {
  const B = (loc.baci ??= {});
  const cache = ((loc.cache ??= {}).baci ??= {});  // (the results: loc.cache isn't saved with the session)
  const proj = ctx.projet(p.projet);
  const vars = proj.variables;
  const ids = [].concat(proj.identifiants || []).map((i) => [`id:${i.rang}`, `${i.nom} (${t('identifiant')})`]);
  const lire = (x) => (String(x).startsWith('id:') ? x : Number(x));
  const existe = (options, u) => options.some(([x]) => x === u);
  // a quantitative response; a time (another quantitative variable or an ID: the year); a unit
  // (a qualitative variable or an ID: the sector); first choices named like them, the sector
  // before the site
  const temporel = /^(ann[ée]e|an|year|date|temps|time|jour|day|mois|month|saison|season)\b/i;
  const spatial = /unit|station|parcelle|plot|transect|zone|site/i;
  const reponses = vars.filter((v) => !v.qualitative).map((v) => [v.uid, v.nom]);
  if (!existe(reponses, B.y) && reponses.length) B.y = (reponses.find(([, n]) => !temporel.test(n)) || reponses[0])[0];
  const tempsPossibles = reponses.filter(([u]) => u !== B.y).concat(ids);
  if (!existe(tempsPossibles, B.temps) && tempsPossibles.length) B.temps = (tempsPossibles.find(([, n]) => temporel.test(n)) || ids[0] || tempsPossibles[0])[0];
  const unitesPossibles = vars.filter((v) => v.qualitative).map((v) => [v.uid, `${v.nom} (${[].concat(v.categories).length} ${t('categories')})`])
    .concat(ids).filter(([u]) => u !== B.temps);
  if (!existe(unitesPossibles, B.unite) && unitesPossibles.length) {
    B.unite = (unitesPossibles.find(([, n]) => /secteur|sector/i.test(n)) || unitesPossibles.find(([, n]) => spatial.test(n)) || unitesPossibles[0])[0];
  }
  if (!reponses.length || !tempsPossibles.length || !unitesPossibles.length) {
    conteneur.replaceChildren(outils(modes), el('p', { class: 'infos' }, t('baci_peu')));
    return;
  }
  if (cache.unite !== String(B.unite)) {
    try { cache.niveaux = [].concat((await ctx.demander('niveaux', { projet: p.projet, variable: B.unite })).niveaux); } catch { cache.niveaux = []; }
    cache.unite = String(B.unite);
  }
  const niveaux = cache.niveaux;
  B.impact = [].concat(B.impact ?? []).filter((n) => niveaux.includes(n));
  B.temoins = [].concat(B.temoins ?? []).filter((n) => niveaux.includes(n) && !B.impact.includes(n));
  B.test ??= 'welch';
  const nombre = (libelle, aide, valeur, changer) => el('label', { class: 'champ', title: aide }, el('span', {}, libelle),
    el('input', { type: 'number', step: 'any', value: valeur ?? '', style: 'width:6.5em',
      onchange: (e) => {
        const s = String(e.target.value).trim();
        const x = s === '' ? null : Number(s);
        if (x === null || Number.isFinite(x)) { changer(x); ctx.rafraichir(); }
      } }));
  const barre = outils(modes,
    choix(t('baci_reponse'), reponses, B.y, (x) => { B.y = Number(x); ctx.rafraichir(); }),
    choix(t('baci_temps'), tempsPossibles, B.temps, (x) => { B.temps = lire(x); ctx.rafraichir(); }),
    choix(t('baci_unite'), unitesPossibles, B.unite, (x) => { B.unite = lire(x); ctx.rafraichir(); }),
    nombre(t('baci_evenement'), t('baci_evenement_aide'), B.evenement, (x) => { B.evenement = x; }),
    nombre(t('baci_fin'), t('baci_fin_aide'), B.fin, (x) => { B.fin = x; }),
    choix(t('test'), [['welch', t('test_welch')], ['student', t('test_student')], ['mann_whitney', 'Mann–Whitney']], B.test, (x) => { B.test = x; ctx.rafraichir(); }));
  // the unit's levels, each ticked as impact or as control (not both)
  const cocher = (role, autre) => el('div', { class: 'unites' }, el('span', { class: 'role' }, t(role === 'impact' ? 'baci_impact' : 'baci_temoins')),
    niveaux.map((n) => el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: B[role].includes(n), disabled: B[autre].includes(n),
      onchange: (e) => { B[role] = niveaux.filter((x) => (x === n ? e.target.checked : B[role].includes(x))); ctx.rafraichir(); } }), n)),
    role === 'temoins' ? el('button', { disabled: niveaux.every((n) => B.impact.includes(n) || B.temoins.includes(n)),
      onclick: () => { B.temoins = niveaux.filter((n) => !B.impact.includes(n)); ctx.rafraichir(); } }, t('baci_tous_autres')) : null);
  const unites = el('div', { class: 'outils' }, cocher('impact', 'temoins'), cocher('temoins', 'impact'));
  if (!B.impact.length || !B.temoins.length) { conteneur.replaceChildren(barre, unites, el('p', { class: 'infos' }, t('baci_choisir_unites'))); return; }
  const evenement = Number.isFinite(B.evenement) ? B.evenement : null;
  const q = { projet: p.projet, y: B.y, temps: B.temps, unite: B.unite, impact: B.impact, temoins: B.temoins, test: B.test,
    evenement, fin: evenement !== null && Number.isFinite(B.fin) ? B.fin : null };
  // (the language too: R writes the sentence)
  const cle = JSON.stringify([q.y, q.temps, q.unite, q.impact, q.temoins, q.evenement, q.fin, q.test, langueActuelle()]);
  if (cache.cle !== cle) {
    conteneur.replaceChildren(barre, unites, el('p', { class: 'infos attente' }, t('calcul')));
    try { cache.res = await ctx.demander('baci', q); cache.erreur = null; } catch (e) { cache.res = null; cache.erreur = e.message; }
    cache.cle = cle;
  }
  if (!cache.res) { conteneur.replaceChildren(barre, unites, el('p', { class: 'erreur' }, `${t('erreur')} : ${cache.erreur}`)); return; }
  const r = cache.res;
  const s = (k) => [].concat(r.serie[k]);
  const xs = s('temps');
  const ecarts = s('ecart');
  const imp = s('impact');
  const tem = s('temoins');
  const ni = s('n_impact');
  const nt = s('n_temoins');
  const fait = Boolean(r.periodes);  // (the test, once an event is entered)
  const periode = fait ? s('periode') : xs.map(() => '');
  const pr = (k) => (fait ? [].concat(r.periodes[k]) : []);
  const P = NOMS_PERIODES();
  const fr = langueActuelle() === 'fr';
  const liste = (v) => (v.length > 8 ? `${v.slice(0, 8).join(', ')}… (${v.length})` : v.join(', '));
  // what the gaps are, and what was left out: times with one side only, rows without a value
  const sansTemoin = [].concat(r.seuls.impact ?? []).map(texteTemps);
  const sansImpact = [].concat(r.seuls.temoins ?? []).map(texteTemps);
  const repetees = r.repetees ? [].concat(r.repetees.unite).map((u, i) => `${u}${fr ? ' : ' : ': '}${[].concat(r.repetees.temps)[i]}`) : [];
  const douteuses = pr('periode').filter((_, i) => pr('shapiro_p')[i] !== null && pr('shapiro_p')[i] < 0.05).map((g) => P[g]);
  const notes = [t('baci_note_ecart', { n: xs.length, temps: r.temps.nom }),
    sansTemoin.length ? t('baci_sans_temoin', { temps: liste(sansTemoin) }) : '',
    sansImpact.length ? t('baci_sans_impact', { temps: liste(sansImpact) }) : '',
    r.manquantes ? t('baci_manquantes', { n: r.manquantes, y: r.y.nom, temps: r.temps.nom }) : '',
    repetees.length ? t('baci_repetees', { detail: repetees.join(fr ? ' ; ' : '; ') }) : '',
    r.hors ? t('baci_hors_note', { n: r.hors }) : '',
    douteuses.length ? t('baci_normalite', { periodes: douteuses.join(', ') }) : '',
    fait ? t('baci_note_autocorrelation') : t('baci_choisir_evenement')].filter(Boolean).join(' ');
  const copier = el('button', { class: 'icone-seule', title: t('copier_phrase'),
    onclick: () => navigator.clipboard.writeText(r.phrase).then(() => ctx.statut(t('phrase_copiee'))) }, icone('copy'));
  const graphe = el('div', { class: 'graphe' });
  const details = el('div', { class: 'defile', style: { flex: '0 1 auto', maxHeight: '40%' } });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(barre, unites, el('div', { class: 'note' }, ...(fait ? [el('b', {}, r.phrase), ' ', copier, el('br')] : []), notes),
    graphe, ...(fait ? [details] : []), bas);

  // the gaps joined in time order, each period in its colour, the event line and each period's mean
  const c = couleursTheme();
  const couleurs = { avant: PALETTE[0], apres: '#d1495b', hors: c.doux, '': PALETTE[0] };
  const traces = [{ type: 'scatter', mode: 'lines', x: xs, y: ecarts, line: { color: c.ligne, width: 1 }, hoverinfo: 'skip', showlegend: false }];
  for (const g of fait ? ['avant', 'apres', 'hors'] : ['']) {
    const idx = xs.map((_, i) => i).filter((i) => periode[i] === g);
    if (!idx.length) continue;
    traces.push({ type: 'scatter', mode: 'markers', name: g ? P[g] : t('baci_ecart_court'), x: idx.map((i) => xs[i]), y: idx.map((i) => ecarts[i]),
      marker: { color: couleurs[g], size: 8 }, customdata: idx.map((i) => [fmt(imp[i], 3), ni[i], fmt(tem[i], 3), nt[i]]),
      hovertemplate: `${r.temps.nom} %{x}<br>d_t = %{y:.3f}<br>${t('baci_impact')} %{customdata[0]} (n = %{customdata[1]}) − ${t('baci_temoins')} %{customdata[2]} (n = %{customdata[3]})<extra></extra>` });
  }
  const formes = [];
  const annotations = [];
  if (fait) {
    // (the line between the last time before and the event: half a year before it, for years)
    const dernier = Math.max(...xs.filter((_, i) => periode[i] === 'avant'));
    const xe = r.evenement - Math.min(0.5, (r.evenement - dernier) / 2);
    formes.push({ type: 'line', yref: 'paper', y0: 0, y1: 1, x0: xe, x1: xe, line: { dash: 'dash', color: c.texte, width: 1 } });
    annotations.push({ x: xe, y: 1, yref: 'paper', xanchor: 'left', yanchor: 'top', showarrow: false, text: ` ${t('baci_evenement_court')} ${texteTemps(r.evenement)}`,
      font: { size: FORMAT.taille - 2, color: c.doux } });
    ['avant', 'apres'].forEach((g, j) => {
      const m = pr('moyenne')[j];
      traces.push({ type: 'scatter', mode: 'lines', name: t('baci_moyenne_periode', { periode: P[g], m: fmt(m, 2) }), x: j === 0 ? [pr('debut')[0], xe] : [xe, pr('fin')[1]],
        y: [m, m], line: { color: couleurs[g], width: 2, dash: 'dot' }, hoverinfo: 'skip' });
    });
  }
  tracer(ctx, graphe, traces, { title: { text: `${r.y.nom} · ${t('baci_titre')}` }, showlegend: true, dragmode: 'zoom', shapes: formes, annotations,
    xaxis: { title: { text: r.temps.nom } }, yaxis: { title: { text: t('baci_axe_y') } } });

  // the three tests (a click chooses the one the sentence gives) and the two periods
  if (fait) {
    const tc = (k) => [].concat(r.tests[k]);
    const noms = NOMS_TESTS();
    const effets = NOMS_EFFETS();
    const difference = [].concat(r.difference)[0];
    details.replaceChildren(el('table', { class: 'tableau compact' },
      el('thead', {}, el('tr', {}, [t('test'), t('statistique'), t('ddl'), 'p', t('effet'), t('baci_ic_changement')].map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
      el('tbody', {}, tc('test').map((k, i) => el('tr', { class: k === r.test ? 'choisie' : '', onclick: () => { B.test = k; ctx.rafraichir(); } },
        el('td', {}, noms[k]),
        el('td', { class: 'num' }, fmt(tc('statistique')[i], 3)),
        el('td', { class: 'num' }, ddlTexte(tc('ddl1')[i])),
        el('td', { class: 'num' }, valeurP(tc('p')[i])),
        el('td', { class: 'num' }, tc('effet')[i] === null ? '' : `${effets[tc('nom_effet')[i]] || ''} = ${fmt(tc('effet')[i], 3)}`),
        el('td', { class: 'num' }, tc('bas')[i] === null ? '' : `${fmt(difference, 3)} [${fmt(tc('bas')[i], 3)} ; ${fmt(tc('haut')[i], 3)}]`))))),
    el('table', { class: 'tableau compact', style: { marginTop: '10px' } },
      el('thead', {}, el('tr', {}, [t('baci_periode'), 'n', t('baci_de'), t('baci_a'), t('baci_ecart_moyen'), t('ecart_type'), t('baci_ecart_median'), t('shapiro_p')]
        .map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
      el('tbody', {}, pr('periode').map((g, i) => el('tr', {}, el('td', {}, P[g]), el('td', { class: 'num' }, String(pr('n')[i])),
        el('td', { class: 'num' }, texteTemps(pr('debut')[i])), el('td', { class: 'num' }, texteTemps(pr('fin')[i])),
        ['moyenne', 'ecart_type', 'mediane'].map((k) => el('td', { class: 'num' }, fmt(pr(k)[i], 3))),
        el('td', { class: 'num' + (pr('shapiro_p')[i] !== null && pr('shapiro_p')[i] < 0.05 ? ' alerte' : '') }, valeurP(pr('shapiro_p')[i])))))));
  }

  // below: the gaps time by time
  B.table ??= {};
  const valeurs = { temps: xs, impact: imp, temoins: tem, ecart: ecarts, n_impact: ni, n_temoins: nt, periode: periode.map((g) => (g ? P[g] : '')) };
  tableau(bas, ctx, B.table, {
    n: xs.length, nom: t('baci_serie'), avant: [el('b', {}, t('baci_serie'))],
    colonnes: [{ cle: 'temps', titre: r.temps.nom, num: true, format: texteTemps }, { cle: 'impact', titre: t('baci_moyenne_impact'), num: true, decimales: 3 },
      { cle: 'temoins', titre: t('baci_moyenne_temoins'), num: true, decimales: 3 }, { cle: 'ecart', titre: t('baci_ecart_col'), num: true, decimales: 3 },
      { cle: 'n_impact', titre: t('baci_n_impact'), num: true, decimales: 0 }, { cle: 'n_temoins', titre: t('baci_n_temoins'), num: true, decimales: 0 }]
      .concat(fait ? [{ cle: 'periode', titre: t('baci_periode') }] : []),
    valeur: (i, k) => valeurs[k][i],
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

// ------------------------------------------------------------ regressions (regression.R)

// A regression's summary: its fit (R², AIC from the ML fit), its cross-validation, its random
// effects and its autocorrelation, the terms left out.
function rendreResumeRegression(conteneur, ctx, m) {
  const q = m.qualite;
  const y = m.noms_y[0];
  const chiffre = (k, v, d = 3) => el('span', {}, el('small', {}, k), ' ', el('b', {}, typeof v === 'number' ? fmt(v, d) : v ?? '—'));
  const mixte = q.marginal !== undefined;
  const fiche = el('div', { class: 'fiche' },
    el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, m.type), m.titre ? el('span', { class: 'doux' }, m.titre) : null),
    el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('nb_termes'), m.K, 0),
      chiffre(t('groupes_vc'), m.vc_par ? `${m.G} (${m.vc_par})` : `${m.G}`), m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null),
    m.famille && ['GLM', 'GAM'].includes(m.type) ? el('div', { class: 'fiche-chiffres' }, chiffre(t('loi'), t('famille_' + m.famille)), chiffre(t('lien'), m.lien),
      m.parametre_loi ? chiffre(m.parametre_loi.nom === 'theta' ? 'θ' : 'p', m.parametre_loi.valeur) : null,
      m.decalage ? chiffre(t('decalage'), `log(${m.decalage})`) : null) : null,
    el('div', { class: 'fiche-chiffres' },
      mixte ? [chiffre(t('r2_marginal'), q.marginal), chiffre(t('r2_conditionnel'), q.conditionnel)]
        : q.deviance_expliquee !== undefined ? [chiffre(t('r2_ajuste'), q.r2_ajuste), chiffre(t('deviance_expliquee'), `${fmt(100 * q.deviance_expliquee, 1)} %`)]
          : [chiffre('R2', q.r2), chiffre(t('r2_ajuste'), q.r2_ajuste)],
      chiffre('Q2', q.Q2), chiffre(`RMSEE ${y}`, m.RMSEE[y], 4), chiffre(`RMSEcv ${y}`, q.RMSEcv, 4)),
    el('div', { class: 'fiche-chiffres', title: t('aic_aide') },
      chiffre('AIC', q.AIC, 1), chiffre('AICc', q.AICc, 1), chiffre('BIC', q.BIC, 1), chiffre('logLik', q.logLik, 2), chiffre(t('parametres'), q.parametres, 0)),
    ficheTest(m));
  const v = m.variances;
  const variances = v ? [].concat(v.variance) : [];
  const total = variances.reduce((a, b) => a + b, 0);
  // (the share of each variance: only with the residual one, nlme)
  const parts = v && [].concat(v.composante).length > [].concat(m.niveaux || []).length;
  const table = v ? el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, [t('effet_aleatoire'), t('variance'), t('ecart_type'), parts ? t('part') : null].filter(Boolean)
      .map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
    el('tbody', {}, [].concat(v.composante).map((c, i) => el('tr', {}, el('td', {}, c), el('td', { class: 'num' }, fmt(variances[i], 4)),
      el('td', { class: 'num' }, fmt(Math.sqrt(variances[i]), 4)), parts ? el('td', { class: 'num' }, `${fmt((100 * variances[i]) / total, 1)} %`) : null)))) : null;
  const cb = m.courbes;
  // (with each curve's concurvity: in red above 0.8, a curve confounded with the other terms)
  const concurvite = cb && cb.concurvite !== undefined;
  const courbes = cb ? el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, [t('courbe'), 'edf', t('ddl'), t('statistique'), 'p'].map((h, j) => el('th', { class: j ? 'num' : '' }, h)),
      concurvite ? el('th', { class: 'num', title: t('concurvite_aide') }, t('concurvite')) : null)),
    el('tbody', {}, [].concat(cb.terme).map((n, i) => el('tr', {}, el('td', {}, n), el('td', { class: 'num' }, fmt([].concat(cb.edf)[i], 2)),
      el('td', { class: 'num' }, fmt([].concat(cb.ref_ddl)[i], 2)), el('td', { class: 'num' }, fmt([].concat(cb.statistique)[i], 2)),
      el('td', { class: 'num' + ([].concat(cb.p)[i] <= 0.05 ? ' alerte' : '') }, valeurP([].concat(cb.p)[i])),
      concurvite ? el('td', { class: 'num' + ([].concat(cb.concurvite)[i] > 0.8 ? ' erreur' : ''), title: t('concurvite_aide') }, fmt([].concat(cb.concurvite)[i], 2)) : null)))) : null;
  const retires = [].concat(m.termes_retires || []);
  // (a GLM without curves or random effects: plain maximum likelihood)
  const textes = [m.gamm ? t('gamm_note') : mixte ? t('regression_note_mixte') : ['GLM', 'GAM'].includes(m.type) ? t(cb || v ? 'generalise_note' : 'generalise_note_mv') : t('regression_note'),
    notePhi(m),
    retires.length ? t('termes_retires', { termes: retires.join(', ') }) : '',
    q.echecs_vc ? t('echecs_vc', { n: q.echecs_vc }) : '',
    m.dispersion > 1.5 ? t('surdispersion', { d: fmt(m.dispersion, 1) }) : '',
    m.K ? indiceSelectionAIC(ctx, m) : ''].filter(Boolean);
  const notes = notesModele(conteneur, ctx, m);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ, courbes, table, textes.map((x) => el('p', { class: 'note' }, x))));
  notes.retrouver();
}

// The Summary's pointer to the AIC selection: its words, with the view's name as a link to it.
function indiceSelectionAIC(ctx, m) {
  const [avant, apres] = t('selection_aic_indice').split('{lien}');
  return [avant, el('a', { href: '#', style: { color: 'var(--accent)' }, onclick: (e) => { e.preventDefault(); ctx.ouvrirVue('selection_aic', m.id); } },
    `${t('m_graphiques')} › ${t('selection_aic')}`), apres];
}

// The coefficients of a regression: each term's effect on Y with its 95 % interval (a forest
// plot, red when p ≤ 0.05), and their table.
function rendreCoefficientsRegression(conteneur, ctx, m, loc) {
  const c = m.coefficients_regression;
  const col = (k) => [].concat(c[k]);
  const termes = col('terme');
  const est = col('estimation');
  const bas = col('bas');
  const haut = col('haut');
  const pv = col('p');
  const graphe = el('div', { class: 'graphe' });
  const table = el('div', { class: 'bas' });
  const retires = [].concat(m.termes_retires || []);
  const multiplicatif = ['log', 'logit'].includes(m.lien);
  conteneur.replaceChildren(el('p', { class: 'note' }, [t('coefficients_regression_note', { y: m.noms_y[0] }),
    m.lien && m.lien !== 'identity' ? t(multiplicatif ? `coefficients_lien_${m.lien}` : 'coefficients_lien', { lien: m.lien }) : '',
    retires.length ? t('termes_retires', { termes: retires.join(', ') }) : ''].filter(Boolean).join(' ')), graphe, table);
  const idx = termes.map((_, i) => i).filter((i) => termes[i] !== '(Intercept)');
  tracer(ctx, graphe, [{ type: 'scatter', mode: 'markers', y: idx.map((i) => termes[i]), x: idx.map((i) => est[i]),
    error_x: { type: 'data', symmetric: false, array: idx.map((i) => haut[i] - est[i]), arrayminus: idx.map((i) => est[i] - bas[i]), color: '#888', thickness: 1.5 },
    marker: { size: 9, color: idx.map((i) => (pv[i] !== null && pv[i] <= 0.05 ? '#d1495b' : PALETTE[0])) },
    customdata: idx.map((i) => [fmt(bas[i], 4), fmt(haut[i], 4), valeurP(pv[i])]),
    hovertemplate: '<b>%{y}</b><br>%{x:.4f} [%{customdata[0]} ; %{customdata[1]}]<br>p = %{customdata[2]}<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('coefficients')} ${m.noms_y[0]}` }, dragmode: 'zoom', showlegend: false,
    xaxis: { title: { text: t('estimation_ic') } }, yaxis: { autorange: 'reversed' },
    shapes: [{ type: 'line', yref: 'paper', y0: 0, y1: 1, x0: 0, x1: 0, line: { color: '#888', width: 1, dash: 'dot' } }] });
  loc.table ??= {};
  const num = (cle, titre, decimales, format, alerte) => ({ cle, titre, num: true, decimales, format, alerte });
  tableau(table, ctx, loc.table, {
    n: termes.length, nom: 'Coefficients',
    colonnes: [{ cle: 'terme', titre: t('terme') }, num('estimation', t('estimation'), 4), num('erreur_type', t('erreur_type'), 4), num('ddl', t('ddl'), 0),
      num('t', 't', 3), num('p', 'p', 4, valeurP, (i) => pv[i] !== null && pv[i] <= 0.05), num('bas', t('ic_bas'), 4), num('haut', t('ic_haut'), 4)]
      .concat(multiplicatif ? [num('exp', t(m.lien === 'logit' ? 'rapport_cotes' : 'effet_multiplicatif'), 4), num('exp_bas', t('ic_bas'), 4), num('exp_haut', t('ic_haut'), 4)] : []),
    valeur: (i, k) => (k === 'exp' ? Math.exp(est[i]) : k === 'exp_bas' ? Math.exp(bas[i]) : k === 'exp_haut' ? Math.exp(haut[i]) : col(k)[i]),
  });
}

// The effect curves of a regression (regression.R: effets_regression): each quantitative term's
// effect on the link scale over its values, a curve (GAM) or a line, with its 95 % band and the
// observed values along the axis (computed in R on request, then kept).
// A curve's thresholds (regression.R: seuils_courbe), in words: its edf and shape, the ranges
// where its slope is clearly rising or falling, and where the effect is clearly above or below
// its mean.
const PENTE = { hausse: '#2a9d8f', baisse: '#d1495b' };
const nombreSeuil = (v) => fmt(v, Math.abs(v) >= 100 ? 0 : Math.abs(v) >= 10 ? 1 : 2);
const plagesSeuils = (pl) => [].concat((pl && pl.sens) || []).map((sens, i) => ({ sens, de: [].concat(pl.de)[i], a: [].concat(pl.a)[i] }));
// A model's CAR1 autocorrelation: φ along its time, within each group (moteur/R/regression.R:
// the "within each" level, or else the last random effect).
export function notePhi(m) {
  if (m.phi === null || m.phi === undefined) return '';
  const groupe = m.dans || [].concat(m.niveaux || []).slice(-1)[0];
  return t(groupe ? 'phi_note_dans' : 'phi_note', { phi: fmt(m.phi, 3), temps: m.temps, groupe });
}

export function resumeSeuils(s) {
  const pente = plagesSeuils(s.plages_pente).map((z) => t('seuils_de_a', { sens: t(z.sens), de: nombreSeuil(z.de), a: nombreSeuil(z.a) }));
  const effet = plagesSeuils(s.plages_effet).map((z) => t('seuils_de_a', { sens: t(z.sens), de: nombreSeuil(z.de), a: nombreSeuil(z.a) }));
  return t('seuils_resume', { edf: fmt(s.edf, 2), forme: t(s.lineaire ? 'seuils_lineaire' : 'seuils_courbe'),
    pente: pente.length ? pente.join(' ; ') : t('seuils_aucune'), effet: effet.length ? effet.join(' ; ') : t('seuils_aucune') });
}

// The curve and its slope, one above the other, with their 95 % simultaneous bands; the ranges
// where the slope's band excludes 0 shaded (rising, falling).
function tracerSeuils(ctx, graphe, m, e, s, lien) {
  const c = couleursTheme();
  const bandeGrise = (xs, bas, haut, axe) => ({ type: 'scatter', mode: 'lines', x: xs.concat([...xs].reverse()), y: bas.concat([...haut].reverse()), yaxis: axe,
    fill: 'toself', fillcolor: 'rgba(128, 128, 128, 0.22)', line: { width: 0 }, hoverinfo: 'skip', name: t('seuils_bande') });
  const x = [].concat(s.x);
  const xp = [].concat(s.x_pente);
  const valeurs = [].concat(s.valeurs);
  const bas = [].concat(s.bas);
  const basMin = Math.min(...bas);
  const zones = plagesSeuils(s.plages_pente);
  tracer(ctx, graphe, [
    bandeGrise(x, bas, [].concat(s.haut), 'y'),
    { type: 'scatter', mode: 'lines', x, y: [].concat(s.effet), line: { color: PALETTE[0], width: 2 }, name: e.terme,
      hovertemplate: `${e.terme} = %{x:.3f}<br>${t('effet')} = %{y:.3f}<extra></extra>` },
    { type: 'scatter', mode: 'markers', x: valeurs, y: valeurs.map(() => basMin), marker: { symbol: 'line-ns-open', size: 8, color: c.doux }, hoverinfo: 'skip', name: t('observations') },
    bandeGrise(xp, [].concat(s.pente_bas), [].concat(s.pente_haut), 'y2'),
    { type: 'scatter', mode: 'lines', x: xp, y: [].concat(s.pente), yaxis: 'y2', line: { color: c.texte, width: 1.5 }, name: t('pente'),
      hovertemplate: `${e.terme} = %{x:.3f}<br>${t('pente')} = %{y:.4g}<extra></extra>` }],
  { title: { text: `${m.nom} · ${t('seuils')} · ${e.terme}` }, showlegend: false, dragmode: 'zoom',
    xaxis: { title: { text: e.terme }, anchor: 'y2' },
    yaxis: { domain: [0.4, 1], title: { text: lien === 'identity' ? t('effet_sur', { y: m.noms_y[0] }) : t('effet_lien', { lien }) } },
    yaxis2: { domain: [0, 0.3], title: { text: t('pente') } },
    shapes: zones.map((z) => ({ type: 'rect', xref: 'x', yref: 'paper', x0: z.de, x1: z.a, y0: 0, y1: 1, fillcolor: PENTE[z.sens], opacity: 0.14, line: { width: 0 }, layer: 'below' }))
      .concat([{ type: 'line', xref: 'paper', x0: 0, x1: 1, yref: 'y', y0: 0, y1: 0, line: { color: c.doux, width: 1, dash: 'dot' } },
        { type: 'line', xref: 'paper', x0: 0, x1: 1, yref: 'y2', y0: 0, y1: 0, line: { color: c.doux, width: 1, dash: 'dot' } }]) });
}

async function rendreEffets(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const cache = ((loc.cache ??= {}).effets ??= {});  // (loc.cache isn't saved with the session)
  if (cache.id !== m.id) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    cache.res = await ctx.demander('effets', { modele: m.id });
    cache.id = m.id;
    cache.seuils = {};
  }
  const effets = [].concat(cache.res.effets || []);
  if (!effets.length) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('effets_vide'))); return; }
  if (!effets.some((e) => e.terme === loc.terme)) loc.terme = (effets.find((e) => e.courbe) || effets[0]).terme;
  const e = effets.find((x) => x.terme === loc.terme);
  const graphe = el('div', { class: 'graphe' });
  const lien = cache.res.lien || 'identity';
  // a curve's thresholds (regression.R: seuils_courbe): computed on demand, kept per term
  const avecSeuils = e.courbe && !!loc.seuils;
  const seuils = (cache.seuils ??= {});
  if (avecSeuils && !seuils[e.terme]) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    seuils[e.terme] = await ctx.demander('seuils', { modele: m.id, terme: e.terme });
  }
  const s = avecSeuils ? seuils[e.terme] : null;
  conteneur.replaceChildren(outils(choix(t('terme'), effets.map((x) => [x.terme, x.courbe ? `${x.terme} (${t('courbe').toLowerCase()})` : x.terme]), loc.terme,
    (v) => { loc.terme = v; ctx.rafraichir(); }),
    e.courbe ? el('label', { class: 'champ', title: t('seuils_aide') }, el('input', { type: 'checkbox', checked: avecSeuils,
      onchange: (ev) => { loc.seuils = ev.target.checked; ctx.rafraichir(); } }), t('seuils')) : null,
    el('span', { class: 'infos' }, s ? resumeSeuils(s) : t(lien === 'identity' ? 'effets_note' : 'effets_note_lien', { lien }))), graphe);
  if (s) { tracerSeuils(ctx, graphe, m, e, s, lien); return; }
  const x = [].concat(e.x);
  const c = couleursTheme();
  const valeurs = [].concat(e.valeurs);
  const bas = [].concat(e.bas);
  const basMin = Math.min(...bas);
  tracer(ctx, graphe, [
    { type: 'scatter', mode: 'lines', x: x.concat([...x].reverse()), y: bas.concat([...[].concat(e.haut)].reverse()), fill: 'toself',
      fillcolor: 'rgba(47, 111, 181, 0.18)', line: { width: 0 }, hoverinfo: 'skip', name: t('ic_95') },
    { type: 'scatter', mode: 'lines', x, y: [].concat(e.effet), line: { color: PALETTE[0], width: 2 }, name: e.terme,
      hovertemplate: `${e.terme} = %{x:.3f}<br>${t('effet')} = %{y:.3f}<extra></extra>` },
    { type: 'scatter', mode: 'markers', x: valeurs, y: valeurs.map(() => basMin), marker: { symbol: 'line-ns-open', size: 8, color: c.doux }, hoverinfo: 'skip', name: t('observations') }],
  { title: { text: `${m.nom} · ${t('effets')} · ${e.terme}` }, showlegend: false, dragmode: 'zoom',
    xaxis: { title: { text: e.terme } }, yaxis: { title: { text: lien === 'identity' ? t('effet_sur', { y: m.noms_y[0] }) : t('effet_lien', { lien }) } },
    shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: 0, y1: 0, line: { color: c.doux, width: 1, dash: 'dot' } }] });
}

// The residuals of a regression: against the fitted values, on a normal Q-Q plot, by group of
// each random effect, and Cook's distances (linear model). Points are linked to the selection. A
// GLM or GAM of counts, presence-absence or positive values (Poisson, negative binomial, binomial,
// gamma) also has its randomized quantile residuals (moteur/R/regression_diagnostics.R:
// residus_quantiles), normal when the model is right, which the deviance residuals of counts aren't
// even then: a choice beside the residuals, those by default (loc.residus).
function rendreResidus(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const o = m.observations;
  const y = m.noms_y[0];
  const rq = m.residus_quantiles;
  const quantiles = !!rq && loc.residus === 'quantiles';
  const residu = [].concat(quantiles ? rq.residu : o.residu);
  const niveaux = [].concat(m.niveaux || []);
  const modes = [['ajustes', t('residus_ajustes')], ['qq', t('residus_qq')]].concat(niveaux.map((n) => [`g:${n}`, t('residus_par', { groupe: n })]),
    o.cook ? [['cook', t('distance_cook')]] : []);
  if (!modes.some(([k]) => k === loc.mode)) loc.mode = 'ajustes';
  loc.couleur ??= '';
  const graphe = el('div', { class: 'graphe' });
  const parGroupe = loc.mode.startsWith('g:');
  const nomResidus = quantiles ? t('residus_quantiles') : t('residus_std');
  const suffixe = quantiles ? ` · ${t('residus_quantiles')}` : '';
  conteneur.replaceChildren(outils(choix('', modes, loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    rq ? el('span', { title: t('residus_quantiles_aide') }, choix('', [['standard', t('residus_deviance_std')], ['quantiles', t('residus_quantiles')]],
      quantiles ? 'quantiles' : 'standard', (v) => { loc.residus = v; ctx.rafraichir(); })) : null,
    parGroupe ? null : controleCouleur(ctx, m, loc), parGroupe ? null : controleEtiquettes(ctx, m, loc),
    el('span', { class: 'infos' }, t(quantiles ? 'residus_note_quantiles' : niveaux.length ? 'residus_note_mixte' : 'residus_note'))), graphe);
  const c = couleursTheme();
  const lignes = (valeurs) => valeurs.map((v) => ({ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: v, y1: v,
    line: { color: v ? '#d1495b' : c.doux, width: 1, dash: 'dot' } }));
  if (parGroupe) {
    const g = [].concat(o[loc.mode.slice(2)]);
    const valeurs = [...new Set(g)];
    tracer(ctx, graphe, valeurs.map((v, k) => {
      const idx = g.map((_, i) => i).filter((i) => g[i] === v);
      return { type: 'box', name: v, y: idx.map((i) => residu[i]), text: idx.map((i) => o.nom[i]), boxpoints: idx.length <= 300 ? 'all' : 'outliers',
        jitter: 0.4, pointpos: 0, marker: { color: PALETTE[k % PALETTE.length], size: 4 }, line: { width: 1 } };
    }), { title: { text: `${m.nom} · ${t('residus_par', { groupe: loc.mode.slice(2) })}${suffixe}` }, showlegend: false, dragmode: 'zoom',
      yaxis: { title: { text: nomResidus } }, shapes: lignes([0, -2, 2]) });
    return;
  }
  const [xs, ys, titreX, formes] = loc.mode === 'qq' ? [[].concat(quantiles ? rq.quantile_normal : o.quantile_normal), residu, t('quantiles_normaux'), []]
    : loc.mode === 'cook' ? [[].concat(o[`${y} ajusté`]), [].concat(o.cook), t('valeurs_ajustees'), lignes([4 / m.N])]
      : [[].concat(o[`${y} ajusté`]), residu, t('valeurs_ajustees'), lignes([0, -2, 2])];
  if (loc.mode === 'qq') {  // the line of a normal distribution: through the quartiles
    const tri = (v) => v.filter((x) => x !== null).sort((a, b) => a - b);
    const q = (v, f) => v[Math.floor(f * (v.length - 1))];
    const [rx, ry] = [tri(xs), tri(ys)];
    const pente = (q(ry, 0.75) - q(ry, 0.25)) / (q(rx, 0.75) - q(rx, 0.25));
    const origine = q(ry, 0.25) - pente * q(rx, 0.25);
    formes.push({ type: 'line', x0: rx[0], x1: rx[rx.length - 1], y0: origine + pente * rx[0], y1: origine + pente * rx[rx.length - 1],
      line: { color: '#d1495b', width: 1, dash: 'dot' } });
  }
  tracer(ctx, graphe, pointsObservations(ctx, m, loc, xs, ys), { title: { text: `${m.nom} · ${modes.find(([k]) => k === loc.mode)[1]}${suffixe}` },
    xaxis: { title: { text: titreX } }, yaxis: { title: { text: loc.mode === 'cook' ? t('distance_cook') : nomResidus } }, shapes: formes });
  brancherSelection(graphe, ctx, m.id);
}

// The autocorrelation of a regression's residuals along time, within each group, the groups pooled
// (moteur/R/regression_diagnostics.R: autocorrelation_residus, as nlme::ACF): a bar per lag (k steps
// apart in each group's time order) and the band ±1.96/√n of each lag, n its pairs. A model with
// CAR1 brings its time and groups, and two series: its residuals without the autocorrelation, and
// the normalized ones, with none left when CAR1 is enough. For another model, the time (an ID or a
// quantitative variable) and the groups (its random effects, or an ID or a qualitative variable and
// a finer level within it) are chosen here, and for a GLM or a GAM its residuals (Pearson or
// deviance). Computed in R, then kept. Within the groups of its random effects, "Fit with CAR1"
// makes the same model with a CAR1 autocorrelation along this time (reajuster: car1).
const RESIDUS_ACF = { pearson: 'acf_pearson', deviance: 'acf_deviance', normalises: 'acf_normalises' };
// what lag 1 says: beyond the band, CAR1 may be needed (with "Fit with CAR1" at hand, `bouton`, said
// without pointing to New model); the normalized residuals within it, CAR1 is enough
function noteACF(r, bande, series, bouton) {
  const r1 = [].concat(series[0].acf)[1];
  const b1 = bande[1];
  const valeurs = { r: fmt(r1, 2), b: fmt(b1, 2), phi: fmt(r.phi, 3) };
  const z1 = r.car1 ? [].concat(series[1].acf)[1] : null;
  const verdict = r.car1 ? t(Math.abs(z1) <= b1 ? 'acf_car1_suffit' : 'acf_car1_reste', { ...valeurs, z: fmt(z1, 2) })
    : t(r1 > b1 ? (bouton ? 'acf_car1_conseil_court' : 'acf_car1_conseil') : r1 < -b1 ? 'acf_negative' : 'acf_dans_bande', valeurs);
  return [t(r.groupe ? 'acf_note' : 'acf_note_un', { temps: r.temps, groupe: r.groupe, g: r.groupes, n: r.n }),
    r.laissees ? t('acf_laissees', { n: r.laissees }) : null, verdict].filter(Boolean).join(' ');
}

async function rendreACF(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const proj = ctx.projet(m.projet) || {};
  const car1 = m.phi !== null && m.phi !== undefined;
  const mgcv = ['GLM', 'GAM'].includes(m.type) && !m.gamm;
  const niveaux = [].concat(m.niveaux || []);
  const ids = [].concat(proj.identifiants || []).map((i) => [`id:${i.nom}`, `${i.nom} (${t('identifiant')})`]);
  const variables = [].concat(proj.variables || []);
  const temps = ids.concat(variables.filter((v) => !v.qualitative).map((v) => [`var:${v.uid}`, v.nom]));
  const simples = ids.concat(variables.filter((v) => v.qualitative).map((v) => [`var:${v.uid}`, v.nom]));
  // (a grouping's own name: an ID's entry reads "site (ID)"; the variable or the ID of a single
  // random effect is the same groups as "its random effects", and a random effect's level adds
  // nothing within them)
  const nomDe = ([k, n]) => (k.startsWith('id:') ? k.slice(3) : n);
  const groupes = [['', t('aucun')]].concat(niveaux.length ? [['effets', niveaux.join(' / ')]] : [],
    simples.filter((s) => !(niveaux.length === 1 && nomDe(s) === niveaux[0])));
  // by default a time named like one (a year, a date...), and the groups of the model's random effects;
  // a group can take a finer level within it (the sector within its site, when sector names repeat)
  if (!temps.some(([k]) => k === loc.temps)) loc.temps = (temps.find(([, n]) => /ann[ée]e|year|date|temps|time|saison|season/i.test(n)) || [''])[0];
  if (!groupes.some(([k]) => k === loc.groupe)) loc.groupe = niveaux.length ? 'effets' : '';
  const emboitable = !!loc.groupe;
  const fins = [['', '—']].concat(simples.filter((s) => s[0] !== loc.groupe && !(loc.groupe === 'effets' && niveaux.includes(nomDe(s)))));
  if (!emboitable || !fins.some(([k]) => k === loc.groupe_fin)) loc.groupe_fin = '';
  if (!['pearson', 'deviance'].includes(loc.type)) loc.type = 'pearson';
  const spec = (v) => (v.startsWith('id:') ? { by: 'id', id: v.slice(3) } : { by: 'variable', variable: Number(v.slice(4)) });
  const barre = outils(car1 ? null : choix(t('acf_temps'), [['', '—']].concat(temps), loc.temps, (v) => { loc.temps = v; ctx.rafraichir(); }),
    car1 ? null : choix(t('acf_groupes'), groupes, loc.groupe, (v) => { loc.groupe = v; ctx.rafraichir(); }),
    !car1 && emboitable ? el('span', { title: t('acf_groupe_fin_aide') }, choix('/', fins, loc.groupe_fin, (v) => { loc.groupe_fin = v; ctx.rafraichir(); })) : null,
    mgcv ? el('span', { title: t('acf_type_aide') }, choix(t('residus'), [['pearson', t('acf_pearson')], ['deviance', t('acf_deviance')]], loc.type,
      (v) => { loc.type = v; ctx.rafraichir(); })) : null);
  if (!car1 && !loc.temps) { conteneur.replaceChildren(barre, el('p', { class: 'infos' }, t('acf_choisir_temps'))); return; }
  // (with the language, for an engine error, and the project's revision: without CAR1, the time and
  // the groups are read in the project's current data)
  const cle = JSON.stringify([...(car1 ? [m.id] : [m.id, loc.temps, loc.groupe, loc.groupe_fin, mgcv ? loc.type : '']), langueActuelle(), proj.revision ?? null]);
  const cache = ((loc.cache ??= {}).acf ??= {});  // (loc.cache isn't saved with the session)
  if (cache.cle !== cle) {
    conteneur.replaceChildren(barre, el('p', { class: 'infos attente' }, t('calcul')));
    try {
      cache.res = await ctx.demander('autocorrelation_residus', car1 ? { modele: m.id } : { modele: m.id, temps: spec(loc.temps), type: loc.type,
        groupe: loc.groupe === 'effets' ? [{ by: 'effets' }].concat(loc.groupe_fin ? [spec(loc.groupe_fin)] : [])
          : emboitable ? [loc.groupe, loc.groupe_fin].filter(Boolean).map(spec) : null });
      cache.erreur = null;
    } catch (e) { cache.res = null; cache.erreur = e.message; }
    cache.cle = cle;
  }
  const r = cache.res;
  if (!r) { conteneur.replaceChildren(barre, el('p', { class: 'erreur' }, cache.erreur)); return; }
  const decalages = [].concat(r.decalage);
  const bande = [].concat(r.bande);
  const series = [].concat(r.series);
  const c = couleursTheme();
  const graphe = el('div', { class: 'graphe' });
  // "Fit with CAR1": only where the engine allows it (moteur/R/regression.R: definition_car1): CAR1
  // within the groups of the random effects (or a finer level within them) of an LMM, or of a GLM
  // or GAM with the normal distribution (then fitted by mgcv::gamm). A new model, selected with its
  // default views, as after Fit this model; a refusal is shown here. Whether it compares with this
  // one by AIC (regression.R: signature): an LMM on the same observations (none left out for want of
  // a time or a group) is in its AIC set; mgcv::gamm's likelihood isn't mgcv::gam's.
  const bouton = !car1 && niveaux.length > 0 && loc.groupe === 'effets' && (m.type === 'LMM' || m.famille === 'gaussienne');
  const refus = cache.car1 && cache.car1.cle === cle ? cache.car1.erreur : null;
  const ajusterCar1 = () => ctx.reajuster(m.id, { car1: { temps: spec(loc.temps), groupe: [{ by: 'effets' }].concat(loc.groupe_fin ? [spec(loc.groupe_fin)] : []) } })
    .catch((e) => { cache.car1 = { cle, erreur: e.message }; ctx.rafraichir(); });
  const ligneCar1 = bouton ? el('div', { class: 'ligne' },
    el('button', { onclick: ajusterCar1, title: t('acf_ajuster_car1_aide'), style: { flex: 'none' } }, icone('file-plus'), t('acf_ajuster_car1')),
    el('span', { class: 'note' }, t(m.type !== 'LMM' ? 'acf_car1_gamm' : r.laissees ? 'acf_car1_laissees' : 'acf_car1_aic', { temps: r.temps, groupe: r.groupe, n: r.laissees }),
      refus ? el('span', { class: 'erreur' }, ` ${refus}`) : null)) : null;
  conteneur.replaceChildren(...[barre, el('p', { class: 'note' }, noteACF(r, bande, series, bouton)), ligneCar1, graphe].filter(Boolean));
  const apres = decalages.slice(1);
  tracer(ctx, graphe, series.map((s, j) => ({ type: 'bar', x: decalages, y: [].concat(s.acf), customdata: [].concat(r.paires),
    name: t(r.car1 && s.residus !== 'normalises' ? 'acf_sans_car1' : RESIDUS_ACF[s.residus]), marker: { color: PALETTE[j ? 2 : 0] },
    hovertemplate: `${t('acf_decalage')} %{x} : %{y:.3f}<br>n = %{customdata}<extra>%{fullData.name}</extra>` }))
    .concat([1, -1].map((signe) => ({ type: 'scatter', mode: 'lines', x: apres, y: apres.map((k) => signe * bande[k]), name: t('acf_bande'),
      showlegend: signe > 0, line: { color: c.doux, width: 1, dash: 'dash' }, hoverinfo: 'skip' }))),
  { title: { text: `${m.nom} · ${t('acf')}` }, barmode: 'group', dragmode: 'zoom', showlegend: true,
    xaxis: { title: { text: t('acf_decalage') }, dtick: 1 }, yaxis: { title: { text: t('autocorrelation') } },
    shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: 0, y1: 0, line: { color: c.doux, width: 1 } }] });
}

// The contribution of each group of terms (a block, or a variable): the model refitted
// without it, by ML. ΔAIC above 2: the group clearly improves the model (computed in R on
// request, then kept).
async function rendreApports(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const cache = ((loc.cache ??= {}).apports ??= {});  // (loc.cache isn't saved with the session)
  if (cache.id !== m.id) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul_apports')));
    cache.res = (await ctx.demander('apports', { modele: m.id })).apports;
    cache.id = m.id;
  }
  const a = cache.res || {};
  const col = (k) => [].concat(a[k] ?? []);
  const groupes = col('groupe');
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('apports_note')), graphe, bas);
  if (!groupes.length) { graphe.replaceChildren(el('p', { class: 'infos' }, t('apports_vide'))); return; }
  const delta = col('delta_aic');
  const ordre = groupes.map((_, i) => i).sort((i, j) => (delta[j] ?? -Infinity) - (delta[i] ?? -Infinity));
  const c = couleursTheme();
  tracer(ctx, graphe, [{ type: 'bar', x: ordre.map((i) => groupes[i]), y: ordre.map((i) => delta[i]),
    marker: { color: ordre.map((i) => (delta[i] > 2 ? '#3b9a5b' : delta[i] < 0 ? '#d1495b' : c.doux)) },
    hovertemplate: '<b>%{x}</b><br>ΔAIC = %{y:.2f}<extra></extra>' }],
  { title: { text: `${m.nom} · ${t('apports')}` }, dragmode: false, xaxis: { tickangle: -30 }, yaxis: { title: { text: 'ΔAIC' } },
    shapes: [0, 2].map((v) => ({ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: v, y1: v, line: { color: v ? '#3b9a5b' : c.doux, width: 1, dash: 'dot' } })) });
  loc.table ??= { tri: { cle: 'delta_aic', sens: -1 } };
  const num = (cle, titre, decimales, format, alerte) => ({ cle, titre, num: true, decimales, format, alerte });
  tableau(bas, ctx, loc.table, {
    n: groupes.length, nom: 'AIC',
    colonnes: [{ cle: 'groupe', titre: t('groupe') }, num('termes', t('nb_termes'), 0), num('delta_aic', 'ΔAIC', 2, undefined, (i) => delta[i] > 2),
      num('khi2', 'χ²', 2), num('ddl', t('ddl'), 0), num('p', 'p', 4, valeurP)],
    valeur: (i, k) => col(k)[i],
  });
}

// The selection of the fixed effects by AICc (regression.R: selection_aic): every combination of
// the model's groups of terms, from the null model to the full one, each fitted by ML on the
// model's observations (beyond 8 groups, a backward elimination). The suggested models, each
// group's importance, the best models, and any of them fitted as a new model (reajuster:
// groupes_aic). Computed in R on request, then kept with the model.
export const SELECTION_AIC_MAX = 20;
// the rows shown: the 20 best, or those within ΔAICc < 10 (failed fits left out)
export const lignesSelectionAIC = (s, montrer = 'meilleurs') => {
  const aicc = [].concat(s.modeles.AICc);
  const delta = [].concat(s.modeles.delta);
  return aicc.map((_, i) => i).filter((i) => aicc[i] !== null && (montrer === 'delta10' ? delta[i] < 10 : i < SELECTION_AIC_MAX));
};
// the groups of the i-th model of the ranking
export const groupesSelectionAIC = (s, i) => [].concat(s.groupes).filter((_, j) => [].concat(s.contient[j])[i] === 1);
export const nombreParametres = (k) => (k === null || k === undefined ? '' : Number.isInteger(k) ? String(k) : fmt(k, 1));

async function rendreSelectionAIC(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const cache = ((loc.cache ??= {}).selection ??= {});  // (loc.cache isn't saved with the session)
  if (cache.id !== m.id) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul_selection_aic')));
    cache.res = await ctx.demander('selection_aic', { modele: m.id });
    cache.id = m.id;
  }
  const s = cache.res;
  if (!s.K) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('selection_aic_vide'))); return; }
  const col = (k) => [].concat(s.modeles[k] ?? []);
  const [aicc, delta, poids, k] = [col('AICc'), col('delta'), col('poids'), col('k')];
  const groupes = [].concat(s.groupes);
  const nom = (i) => groupesSelectionAIC(s, i).join(' + ') || t('aic_modele_nul');
  const ajuster = (i) => ctx.reajuster(m.id, { groupes_aic: groupesSelectionAIC(s, i) });
  const bouton = (i) => el('button', { onclick: () => ajuster(i), title: `${t('ajuster_ce_modele_aide')}\n${nom(i)}` }, icone('file-plus'), t('ajuster_ce_modele'));
  const suggestion = (titre, i, details) => el('div', { class: 'ligne' }, el('span', {}, el('small', {}, titre), ' ', el('b', {}, nom(i))),
    el('span', { class: 'doux' }, details), bouton(i));
  const pa = s.parcimonieux - 1;  // (the engine's rows count from 1)
  const fiche = el('div', { class: 'fiche' },
    suggestion(t('selection_aic_meilleur'), 0, `k ${nombreParametres(k[0])} · AICc ${fmt(aicc[0], 2)} · ${t('poids_akaike')} ${fmt(poids[0], 2)}`),
    pa > 0 ? suggestion(t('selection_aic_parcimonieux'), pa, `k ${nombreParametres(k[pa])} · ΔAICc ${fmt(delta[pa], 2)} · ${t('poids_akaike')} ${fmt(poids[pa], 2)}`)
      : el('p', { class: 'note' }, t('selection_aic_aussi')));
  const retires = s.chemin ? [].concat(s.chemin.retire).slice(1) : [];
  const note = [t('selection_aic_note', { n: s.N }),
    s.methode === 'toutes' ? t('selection_aic_toutes', { n: aicc.length })
      : t('selection_aic_descendante', { k: s.K, limite: s.limite, n: aicc.length, retires: retires.length ? retires.join(', ') : t('aic_aucun_retire') }),
    s.echecs ? t('selection_aic_echecs', { n: s.echecs }) : ''].filter(Boolean).join(' ');
  const graphe = el('div', { class: 'graphe petit', style: { flex: '0 0 180px', minHeight: '150px' } });  // (the table keeps room in a small panel)
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, note), fiche, graphe, bas);
  const gi = [].concat(s.importance.groupe);
  const vi = [].concat(s.importance.importance);
  const ordre = gi.map((_, j) => j).sort((a, b) => vi[b] - vi[a]);
  tracer(ctx, graphe, [{ type: 'bar', x: ordre.map((j) => gi[j]), y: ordre.map((j) => vi[j]), marker: { color: PALETTE[0] },
    hovertemplate: `<b>%{x}</b><br>${t('aic_importance')} = %{y:.3f}<extra></extra>` }],
  { title: { text: `${m.nom} · ${t('aic_importance_titre')}` }, dragmode: false, xaxis: { tickangle: -30 }, yaxis: { title: { text: t('aic_importance') }, range: [0, 1.05] } });
  // the table: one column per group (● when the model has it); a click on a row chooses it
  loc.montrer ??= 'meilleurs';
  const lignes = lignesSelectionAIC(s, loc.montrer);
  const choisie = Math.max(0, lignes.findIndex((i) => nom(i) === loc.choisi));
  loc.table ??= {};
  const num = (cle, titre, decimales, format) => ({ cle, titre, num: true, decimales, format });
  tableau(bas, ctx, loc.table, {
    avant: [choix(t('aic_montrer'), [['meilleurs', t('aic_20_meilleurs', { n: SELECTION_AIC_MAX })], ['delta10', 'ΔAICc < 10']], loc.montrer, (v) => { loc.montrer = v; ctx.rafraichir(); }),
      el('span', { class: 'infos' }, t('aic_ligne_choisie', { modele: nom(lignes[choisie]) })), bouton(lignes[choisie])],
    n: lignes.length, nom: 'AICc',
    colonnes: [num('rang', t('aic_rang'), 0), ...groupes.map((g, j) => ({ cle: `g${j}`, titre: g })), num('k', 'k', 1, nombreParametres), num('logLik', 'logLik', 2),
      num('AICc', 'AICc', 2), num('delta', 'ΔAICc', 2), num('poids', t('poids_akaike'), 3), num('poids_cumule', t('aic_poids_cumule'), 3)]
      .concat(s.methode === 'toutes' ? [] : [num('etape', t('aic_etape'), 0)]),
    valeur: (r, c) => {
      const i = lignes[r];
      if (c === 'rang') return i + 1;
      if (c[0] === 'g') return [].concat(s.contient[Number(c.slice(1))])[i] === 1 ? '●' : '';
      return col(c)[i];
    },
    cliquer: (r) => { loc.choisi = nom(lignes[r]); ctx.rafraichir(); },
    choisie: (r) => r === choisie,
  });
}

// Every fitted model side by side. Q2 and RMSEcv compare all models validated by the same
// groups; AIC, ΔAIC and Akaike weights only the regressions of the same response on the same
// observations (an "AIC set", regression.R : signature).
function rendreComparaison(conteneur, ctx, p, loc) {
  const ms = ctx.tousModeles();
  const cum = (v) => (v && v.length ? v[v.length - 1] : null);
  const graphe = el('div', { class: 'graphe petit' });
  const bas = el('div', { class: 'bas' });
  const emboites = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('comparaison_note')), graphe, bas, emboites);
  const lignes = ms.map((m) => {
    // another analysis (RDA, LDA...): no components nor Q2, the figures it gives (m.comparaison)
    if (m.analyse) return { id: m.id, nom: m.nom, projet: (ctx.projet(m.projet) || {}).nom, type: m.type, A: null, N: m.N, K: m.K, ...(m.comparaison || {}) };
    if (m.regression) {
      const q = m.qualite;
      return { id: m.id, nom: m.nom, projet: (ctx.projet(m.projet) || {}).nom, type: m.type, A: null, N: m.N, K: m.K,
        R2X: null, R2Y: null, R2: q.marginal !== undefined ? q.marginal : q.r2 ?? q.r2_ajuste, Q2: q.Q2,
        RMSEE: Object.values(m.RMSEE)[0], RMSEcv: q.RMSEcv, AIC: q.AIC, signature: m.signature };
    }
    const c = m.composantes;
    const s = m.stocke;
    return { id: m.id, nom: m.nom, projet: (ctx.projet(m.projet) || {}).nom, type: m.type, A: m.A, N: m.N, K: m.K,
      R2X: cum(c.R2Xcum), R2Y: c.R2Ycum ? cum(c.R2Ycum) : null, Q2: cum(c.Q2cum),
      RMSEE: m.RMSEE ? Object.values(m.RMSEE)[0] : null, RMSEcv: m.RMSEcv ? Object.values(m.RMSEcv)[0] : null,
      sR2X: s ? s.R2Xcum[m.A - 1] : null, sR2Y: s && s.R2Ycum ? s.R2Ycum[m.A - 1] : null, sQ2: s ? s.Q2cum[m.A - 1] : null };
  });
  // the AIC sets, lettered A, B... in the order of their first model
  const ensembles = [...new Set(lignes.filter((l) => l.AIC !== undefined && l.AIC !== null).map((l) => l.signature))];
  ensembles.forEach((sig, k) => {
    const ici = lignes.filter((l) => l.signature === sig && l.AIC !== null);
    const min = Math.min(...ici.map((l) => l.AIC));
    const w = ici.map((l) => Math.exp(-(l.AIC - min) / 2));
    const somme = w.reduce((a, b) => a + b, 0);
    ici.forEach((l, i) => Object.assign(l, { ensemble: String.fromCharCode(65 + (k % 26)), dAIC: l.AIC - min, poids: w[i] / somme }));
  });
  const regressions = ensembles.length > 0;
  tableau(bas, ctx, loc, {
    n: lignes.length, nom: 'Table',
    colonnes: [['nom', ''], ['projet', t('projets')], ['type', t('type')], ['A', 'A'], ['N', 'N'], ['K', 'K'], ['R2X', 'R2X(cum)'],
      ['R2Y', 'R2Y(cum)'], ['Q2', 'Q2(cum)'], ['RMSEE', 'RMSEE'], ['RMSEcv', 'RMSEcv']]
      .concat(regressions ? [['R2', t('r2_regression')], ['ensemble', t('ensemble_aic')], ['AIC', 'AIC'], ['dAIC', 'ΔAIC'], ['poids', t('poids_akaike')]] : [],
        [['sR2X', `R2X(cum) ${t('stocke')}`], ['sR2Y', `R2Y(cum) ${t('stocke')}`], ['sQ2', `Q2(cum) ${t('stocke')}`]])
      .map(([cle, titre]) => ({ cle, titre, decimales: ['A', 'N', 'K'].includes(cle) ? 0 : undefined })),
    valeur: (i, k) => (typeof lignes[i][k] === 'number' && !['A', 'N', 'K'].includes(k) ? Number(lignes[i][k].toFixed(4)) : lignes[i][k]),
  });
  rendreEmboites(emboites, ctx, loc, lignes);
  if (!ms.length) return;
  const noms = lignes.map((l) => l.nom);
  tracer(ctx, graphe, [
    { type: 'bar', name: 'R2X(cum)', x: noms, y: lignes.map((l) => l.R2X), marker: { color: '#9aa7b8' } },
    { type: 'bar', name: regressions ? `R2Y(cum) / ${t('r2_regression')}` : 'R2Y(cum)', x: noms, y: lignes.map((l) => l.R2Y ?? l.R2 ?? null), marker: { color: '#3b9a5b' } },
    { type: 'bar', name: 'Q2(cum)', x: noms, y: lignes.map((l) => l.Q2), marker: { color: '#2f6fb5' } }],
  { barmode: 'group', dragmode: false, title: { text: t('comparaison') }, margin: { t: 30, b: 50 } });
}

// Under the AIC table, the likelihood-ratio tests between the nested regressions of each AIC set
// (moteur/R/regression_diagnostics.R: rapports_vraisemblance, as anova(m1, m2) on ML fits): the
// bigger and the smaller model, Δdf, χ² and p, approximate with curves or mgcv's random effects.
// Computed in R again when the models change; 50 pairs at most, the search box finds a model's.
function rendreEmboites(conteneur, ctx, loc, lignes) {
  const regressions = lignes.filter((l) => l.ensemble);
  const parEnsemble = {};
  regressions.forEach((l) => { parEnsemble[l.ensemble] = (parEnsemble[l.ensemble] || 0) + 1; });
  if (!Object.values(parEnsemble).some((n) => n > 1)) { conteneur.remove(); return; }
  const cle = regressions.map((l) => `${l.id}:${l.AIC}`).join('|');
  const cache = ((loc.cache ??= {}).emboites ??= {});  // (loc.cache isn't saved with the session)
  if (cache.cle !== cle) {
    Object.assign(cache, { cle, res: null, erreur: null });
    ctx.demander('rapports_vraisemblance', { modeles: regressions.map((l) => l.id) })
      .then((r) => { if (cache.cle === cle) { cache.res = r; ctx.rafraichir(); } })
      .catch((e) => { if (cache.cle === cle) { cache.erreur = e.message; ctx.rafraichir(); } });
  }
  const titre = el('h4', { class: 'titre-section' }, t('rv_titre'));
  if (!cache.res) { conteneur.replaceChildren(titre, el('p', { class: cache.erreur ? 'erreur' : 'infos attente' }, cache.erreur || t('calcul'))); return; }
  const r = cache.res.paires || {};
  const col = (k) => [].concat(r[k] ?? []);
  const grand = col('grand');
  if (!grand.length) { conteneur.replaceChildren(titre, el('p', { class: 'note' }, t('rv_aucune'))); return; }
  const petit = col('petit');
  const p = col('p');
  const approche = col('approche');
  const parId = Object.fromEntries(lignes.map((l) => [l.id, l]));
  const table = el('div', { class: 'bas' });
  conteneur.replaceChildren(titre, el('p', { class: 'note' }, t('rv_note'),
    cache.res.total > grand.length ? ` ${t('rv_limite', { total: cache.res.total, max: grand.length })}` : ''), table);
  loc.emboites ??= {};
  tableau(table, ctx, loc.emboites, {
    n: grand.length, nom: 'LRT',
    colonnes: [{ cle: 'ensemble', titre: t('ensemble_aic') }, { cle: 'grand', titre: t('rv_grand') }, { cle: 'petit', titre: t('rv_petit') },
      { cle: 'ddl', titre: t('rv_ddl'), num: true, format: (v) => (Number.isInteger(v) ? String(v) : fmt(v, 2)) },
      { cle: 'khi2', titre: 'χ²', num: true, decimales: 3 }, { cle: 'p', titre: 'p', num: true, format: valeurP, alerte: (i) => p[i] !== null && p[i] <= 0.05 },
      { cle: 'test', titre: t('rv_test') }],
    valeur: (i, k) => (k === 'ensemble' ? (parId[grand[i]] || {}).ensemble : k === 'grand' ? (parId[grand[i]] || {}).nom
      : k === 'petit' ? (parId[petit[i]] || {}).nom : k === 'test' ? t(approche[i] ? 'rv_approche' : 'rv_ml') : col(k)[i]),
  });
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
  el('p', { class: 'note' }, t(m.analyse ? 'classification_note_proba' : 'classification_note')), el('div', { class: 'tableau-court' }, table), graphe);
  const traces = r.roc.map((roc, j) => ({ type: 'scatter', mode: 'lines', name: `${classes[j]} (AUC ${fmt(roc.auc, 3)})`,
    x: roc.fpr, y: roc.tpr, line: { color: PALETTE[j % PALETTE.length], width: 2, shape: 'hv' } }));
  traces.push({ type: 'scatter', mode: 'lines', x: [0, 1], y: [0, 1], line: { color: '#999', dash: 'dot', width: 1 }, showlegend: false, hoverinfo: 'skip' });
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ROC` }, dragmode: 'zoom', showlegend: true,
    xaxis: { title: { text: t('faux_positifs') }, range: [0, 1] }, yaxis: { title: { text: t('vrais_positifs') }, range: [0, 1.02] } });
}

// The views, by name. A view kept in a module of its own (vues_*.js) is added by app.js at start-up.
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
  residus: { rendre: rendreResidus, titre: (m) => `${m.nom} · ${t('residus')}` },
  acf: { rendre: rendreACF, titre: (m) => `${m.nom} · ${t('acf')}` },
  effets: { rendre: rendreEffets, titre: (m) => `${m.nom} · ${t('effets')}` },
  apports: { rendre: rendreApports, titre: (m) => `${m.nom} · ${t('apports')}` },
  selection_aic: { rendre: rendreSelectionAIC, titre: (m) => `${m.nom} · ${t('selection_aic')}` },
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
  // (selection: the view shows the observations selected under the project's id, app.js)
  series: { rendre: rendreSeries, titre: (_, p) => `${p.nom || ''} · ${t('series')}`, projet: true, selection: true },
  historique: { rendre: rendreHistorique, titre: (_, p) => `${p.nom || ''} · ${t('historique')}`, projet: true },
  comparaison: { rendre: rendreComparaison, titre: () => t('comparaison'), global: true },
};
