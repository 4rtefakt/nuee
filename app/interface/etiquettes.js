// Labels that don't overlap (Plots › Format plot › Spread the labels): the names written by the
// points of a scores, loadings or biplot plot, or of an ordination (any trace drawn 'markers+text'
// on the x and y axes), placed where they cover neither another label nor a point, as ggrepel does
// in R. Each label tries the places beside its point first, its own (the trace's textposition)
// before the others, then rings further and further away, where a line joins it to its point; one
// that finds no free place takes the one it overlaps least. They are written as annotations
// (named ETIQUETTE), so that the plot drawn again at another size (a figure's cell, a report's
// picture, a resized panel) places them again from those. placerEtiquettes and repousserFigure need
// no page (tests/essai_etiquettes.mjs); the functions after them read the plot drawn.

export const ETIQUETTE = 'etiquette_repoussee';
const ETIQUETTES_MAX = 500;  // (beyond, the labels are a texture: written as Plotly writes them)
const ECART = 2;             // px between a label and its point, or another label

// Places `etiquettes` [{ x, y (px in the plot area, y going down), largeur, hauteur (its text, px),
// rayon (its point's, px), position (its trace's textposition: 'top center'...) }] in a plot area
// of `cadre` { largeur, hauteur } px, among `obstacles` [{ x, y, rayon }: a point; { x, y,
// largeur, hauteur }: a fixed box centred there; `propre`: the label whose point it is]. Gives, for
// each label, its box's centre { x, y } and `ligne`: true when it stands away from its point.
export function placerEtiquettes(etiquettes, obstacles, cadre, { anneaux = 12 } = {}) {
  const grille = new Grille(32);
  obstacles.forEach((o, k) => {
    const b = o.largeur !== undefined ? boite(o.x, o.y, o.largeur, o.hauteur) : boite(o.x, o.y, 2 * o.rayon, 2 * o.rayon);
    grille.ajouter({ ...b, propre: o.propre, id: `o${k}` });
  });
  return etiquettes.map((e, i) => {
    let meilleure = null;
    for (const c of candidats(e, anneaux)) {
      const b = boite(c.x, c.y, e.largeur + 2 * ECART, e.hauteur + 2 * ECART);
      if (b.x0 < 0 || b.y0 < 0 || b.x1 > cadre.largeur || b.y1 > cadre.hauteur) continue;
      const recouvre = grille.recouvrement(b, i);
      if (!meilleure || recouvre < meilleure.recouvre) meilleure = { ...c, recouvre };
      if (recouvre === 0) break;
    }
    // (no room inside the plot at all: its own place, as Plotly writes it)
    if (!meilleure) meilleure = candidats(e, 0)[0];
    grille.ajouter({ ...boite(meilleure.x, meilleure.y, e.largeur + 2 * ECART, e.hauteur + 2 * ECART), id: `e${i}` });
    return { x: meilleure.x, y: meilleure.y, ligne: meilleure.anneau > 0 };
  });
}

const boite = (x, y, l, h) => ({ x0: x - l / 2, y0: y - h / 2, x1: x + l / 2, y1: y + h / 2 });
const surface = (a, b) => Math.max(0, Math.min(a.x1, b.x1) - Math.max(a.x0, b.x0)) * Math.max(0, Math.min(a.y1, b.y1) - Math.max(a.y0, b.y0));

// The places a label tries, in order, each its box's centre { x, y, anneau }: beside its point, its
// own place first (the trace's textposition: 'top center' above it, its text on it for 'middle
// center'), then the others around it; then rings further away, every 22.5°, the side of its own
// place first.
const PLACES = {  // (the eight places beside a point: to the right, below, as -1, 0 or 1)
  'top center': [0, -1], 'bottom center': [0, 1], 'middle right': [1, 0], 'middle left': [-1, 0],
  'top right': [1, -1], 'top left': [-1, -1], 'bottom right': [1, 1], 'bottom left': [-1, 1],
};
function candidats(e, anneaux) {
  const r = e.rayon + ECART;
  const pres = ([dx, dy]) => {
    const k = dx && dy ? 0.7 : 1;  // (in a corner, the box's corner by the point)
    return { x: e.x + dx * (k * r + e.largeur / 2), y: e.y + dy * (k * r + e.hauteur / 2), anneau: 0 };
  };
  const propre = PLACES[e.position] || null;
  const liste = [propre ? pres(propre) : { x: e.x, y: e.y, anneau: 0 }].concat(Object.values(PLACES).filter((d) => d !== propre).map(pres));
  const sens = propre && propre[1] > 0 ? -1 : 1;  // (a label below its point looks below first)
  const angles = [90, 270, 0, 180, 45, 135, 315, 225, 67.5, 112.5, 22.5, 157.5, 292.5, 247.5, 337.5, 202.5];
  const pas = Math.max(8, e.hauteur);
  for (let k = 1; k <= anneaux; k += 1) {
    for (const a of angles) {
      const c = Math.cos((a * Math.PI) / 180);
      const s = sens * Math.sin((a * Math.PI) / 180);
      liste.push({ x: e.x + c * (r + e.largeur / 2 + k * pas), y: e.y - s * (r + e.hauteur / 2 + k * pas), anneau: k });
    }
  }
  return liste;
}

// Boxes filed by the cells of a grid they cover, to find quickly those a box meets.
class Grille {
  constructor(taille) { this.taille = taille; this.cases = new Map(); }
  cellules(b, f) {
    const t = this.taille;
    for (let i = Math.floor(b.x0 / t); i <= Math.floor(b.x1 / t); i += 1) for (let j = Math.floor(b.y0 / t); j <= Math.floor(b.y1 / t); j += 1) f(`${i},${j}`);
  }
  ajouter(b) { this.cellules(b, (c) => { if (!this.cases.has(c)) this.cases.set(c, []); this.cases.get(c).push(b); }); }
  // the area of `b` the boxes already filed cover (the point of the label `i` itself left out)
  recouvrement(b, i) {
    const vues = new Set();
    let s = 0;
    this.cellules(b, (c) => {
      for (const x of this.cases.get(c) || []) {
        if (vues.has(x.id) || x.propre === i) continue;
        vues.add(x.id);
        s += surface(b, x);
      }
    });
    return s;
  }
}

// ------------------------------------------------------------ a Plotly figure

const cartesienne = (tr) => (!tr.type || tr.type === 'scatter' || tr.type === 'scattergl') && (!tr.xaxis || tr.xaxis === 'x') && (!tr.yaxis || tr.yaxis === 'y')
  && tr.visible !== false && tr.visible !== 'legendonly';
const modes = (tr) => String(tr.mode || 'markers').split('+');
const valeur = (v, i) => (Array.isArray(v) || ArrayBuffer.isView(v) ? v[i] : v);
// a trace that writes names by its points
export const aEtiquettes = (tr) => !!tr && cartesienne(tr) && modes(tr).includes('text') && modes(tr).includes('markers') && Array.isArray(tr.text);
// a figure whose labels are spread already
export const etiquettesRepoussees = (layout) => !!layout && [].concat(layout.annotations || []).some((a) => String((a && a.name) || '').startsWith(ETIQUETTE));
// the traces drawn without their names (kept for their hover)
export const sansTextes = (data) => data.map((tr) => (aEtiquettes(tr) ? { ...tr, mode: modes(tr).filter((m) => m !== 'text').join('+') } : tr));

// The labels of a figure: those its traces write, and those it has as annotations already (placed
// again at another size). [{ x, y (data), texte, police, position, taille (its point's) }]
export function etiquettesDe(data, layout) {
  const liste = [];
  for (const tr of data || []) {
    if (!aEtiquettes(tr)) continue;
    const f = tr.textfont || {};
    const taille = tr.marker && tr.marker.size !== undefined ? tr.marker.size : 6;
    for (let i = 0; i < tr.text.length; i += 1) {
      const texte = tr.text[i];
      const x = valeur(tr.x, i);
      const y = valeur(tr.y, i);
      if (texte === null || texte === undefined || texte === '' || !Number.isFinite(x) || !Number.isFinite(y)) continue;
      liste.push({ x, y, texte: String(texte), police: { family: valeur(f.family, i), size: valeur(f.size, i), color: valeur(f.color, i) },
        position: valeur(tr.textposition, i) || 'middle center', taille: Number(valeur(taille, i)) || 6 });
    }
  }
  for (const a of [].concat((layout && layout.annotations) || [])) {
    if (!a || !String(a.name || '').startsWith(ETIQUETTE)) continue;
    liste.push({ x: a.x, y: a.y, texte: String(a.text), police: a.font || {}, position: String(a.name).slice(ETIQUETTE.length + 1) || 'top center', taille: null });
  }
  return liste;
}

// The figure with its labels spread: the traces that wrote them drawn without their texts, the
// labels as annotations placed in the plot area of `geometrie` ({ largeur, hauteur, x, y: { range,
// type } }: geometrieDe), `mesurer(texte, police)` giving a text's { largeur, hauteur } in px; its
// other annotations kept. { data, layout }, or null when there is nothing to spread, or too much,
// or an axis isn't linear or logarithmic.
export function repousserFigure(data, layout, geometrie, mesurer) {
  const etiquettes = etiquettesDe(data, layout);
  if (!etiquettes.length || etiquettes.length > ETIQUETTES_MAX || !geometrie || !(geometrie.largeur > 0) || !(geometrie.hauteur > 0)) return null;
  const echelle = (a, longueur, inverse) => {
    if (!a || !Array.isArray(a.range) || !['linear', 'log', '-', undefined].includes(a.type)) return null;
    const v = a.type === 'log' ? (z) => (z > 0 ? Math.log10(z) : NaN) : (z) => z;
    const [r0, r1] = a.range;
    if (!(r1 !== r0)) return null;
    return (z) => (inverse ? (r1 - v(z)) / (r1 - r0) : (v(z) - r0) / (r1 - r0)) * longueur;
  };
  const px = echelle(geometrie.x, geometrie.largeur, false);
  const py = echelle(geometrie.y, geometrie.hauteur, true);
  if (!px || !py) return null;
  const base = (layout && layout.font) || {};
  const police = (p) => ({ family: (p && p.family) || base.family, size: Number(p && p.size) || Number(base.size) || 12, color: (p && p.color) || base.color });
  // each label in px, its point's own marker known by its place
  const places = new Map();
  const cle = (x, y) => `${Math.round(x * 2)},${Math.round(y * 2)}`;
  const enPx = etiquettes.map((e, i) => {
    const x = px(e.x);
    const y = py(e.y);
    if (!places.has(cle(x, y))) places.set(cle(x, y), i);
    return { x, y };
  });
  // the points are obstacles, and the texts other traces write where they are (an arrow's name)
  const obstacles = [];
  const rayons = new Map();
  for (const tr of data || []) {
    if (!cartesienne(tr) || !(Array.isArray(tr.x) || ArrayBuffer.isView(tr.x))) continue;
    const m = modes(tr);
    const taille = tr.marker && tr.marker.size !== undefined ? tr.marker.size : 6;
    for (let i = 0; i < tr.x.length; i += 1) {
      const x = px(tr.x[i]);
      const y = py(valeur(tr.y, i));
      if (!Number.isFinite(x) || !Number.isFinite(y)) continue;
      if (m.includes('markers')) {
        const rayon = (Number(valeur(taille, i)) || 6) / 2 + 1;
        const propre = places.get(cle(x, y));
        if (propre !== undefined && !rayons.has(propre)) rayons.set(propre, rayon);
        obstacles.push({ x, y, rayon, propre });
      } else if (m.includes('text') && tr.text && tr.text[i]) {
        const d = mesurer(String(tr.text[i]), police(tr.textfont));
        obstacles.push({ x, y, largeur: d.largeur, hauteur: d.hauteur });
      }
    }
  }
  const tailles = etiquettes.map((e) => mesurer(e.texte, police(e.police)));
  const placees = placerEtiquettes(etiquettes.map((e, i) => ({ x: enPx[i].x, y: enPx[i].y, largeur: tailles[i].largeur, hauteur: tailles[i].hauteur,
    rayon: rayons.get(i) ?? ((e.taille || 6) / 2 + 1), position: e.position })), obstacles, { largeur: geometrie.largeur, hauteur: geometrie.hauteur });
  const annotations = etiquettes.map((e, i) => {
    const dx = placees[i].x - enPx[i].x;
    const dy = placees[i].y - enPx[i].y;
    const a = { name: `${ETIQUETTE}|${e.position}`, x: e.x, y: e.y, xref: 'x', yref: 'y', text: e.texte, font: police(e.police), xanchor: 'center', yanchor: 'middle' };
    // (away from its point: a thin line to it, from the edge of its marker)
    return placees[i].ligne ? { ...a, showarrow: true, ax: dx, ay: dy, axref: 'pixel', ayref: 'pixel', arrowhead: 0, arrowwidth: 0.7, arrowcolor: 'rgba(128, 128, 128, 0.85)',
      standoff: rayons.get(i) ?? 4 } : { ...a, showarrow: false, xshift: dx, yshift: -dy };
  });
  const autres = [].concat((layout && layout.annotations) || []).filter((a) => !a || !String(a.name || '').startsWith(ETIQUETTE));
  return { data: sansTextes(data || []), layout: { ...layout, annotations: [...autres, ...annotations] } };
}

// ------------------------------------------------------------ in the page

// A plot's area drawn by Plotly: its size (px) and its axes' ranges.
export function geometrieDe(gd) {
  const f = gd && gd._fullLayout;
  if (!f || !f._size || !f.xaxis || !f.yaxis || !Array.isArray(f.xaxis.range) || !Array.isArray(f.yaxis.range)) return null;
  return { largeur: f._size.w, hauteur: f._size.h, x: { range: [...f.xaxis.range], type: f.xaxis.type }, y: { range: [...f.yaxis.range], type: f.yaxis.type } };
}

// A text's size in px in its font (a line per <br>, the other tags left out, as Plotly writes it)
let toile = null;
export function mesurerTexte(texte, police) {
  toile = toile || document.createElement('canvas').getContext('2d');
  const taille = Number(police && police.size) || 12;
  toile.font = `${taille}px ${(police && police.family) || 'sans-serif'}`;
  const lignes = String(texte).split(/<br\s*\/?>/i).map((l) => l.replace(/<[^>]*>/g, ''));
  return { largeur: Math.max(...lignes.map((l) => toile.measureText(l).width)) + 2, hauteur: lignes.length * taille * 1.25 };
}

// The plot `div` whose labels were spread, placed again in it (its panel resized): drawn first
// without them (whose places at the old size could widen its axes), then with them. encore(): false
// when the plot was drawn anew meanwhile (nothing more to do).
export async function replacerDansGraphe(div, encore = () => true) {
  if (!etiquettesRepoussees(div.layout)) return;
  const toutes = [].concat(div.layout.annotations || []);
  const sans = { ...div.layout, annotations: toutes.filter((a) => !String((a && a.name) || '').startsWith(ETIQUETTE)) };
  await window.Plotly.react(div, div.data, sans);
  if (!encore()) return;
  const r = repousserFigure(div.data, { ...sans, annotations: toutes }, geometrieDe(div), mesurerTexte);
  if (r && encore()) await window.Plotly.react(div, r.data, r.layout);
}

// A figure { data, layout } whose labels were spread, placed again at its layout's width and height
// (a figure's cell, a report's picture): its plot's area measured off screen, without the labels'
// annotations (which would widen the axes). The figure as it was when nothing is to be done.
export async function replacerEtiquettes(fig) {
  // (a plot still drawn by WebGL, of thousands of points, would wait off screen for frames the
  // browser doesn't draw: its labels keep their places)
  if (!etiquettesRepoussees(fig.layout) || fig.data.some((tr) => tr.type === 'scattergl')) return fig;
  const sans = { ...fig.layout, annotations: [].concat(fig.layout.annotations || []).filter((a) => !String((a && a.name) || '').startsWith(ETIQUETTE)) };
  const div = document.createElement('div');
  div.style.cssText = `position:fixed;left:-30000px;top:0;width:${fig.layout.width || 900}px;height:${fig.layout.height || 600}px`;
  document.body.append(div);
  try {
    await window.Plotly.newPlot(div, structuredClone(fig.data), structuredClone(sans), { staticPlot: true });
    return repousserFigure(fig.data, fig.layout, geometrieDe(div), mesurerTexte) || fig;
  } catch {
    return fig;
  } finally {
    window.Plotly.purge(div);
    div.remove();
  }
}
