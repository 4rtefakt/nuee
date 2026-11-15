// A figure for publication, its pure parts (no page, no engine: figures.js draws and exports it,
// app/tests/essai_figures.mjs checks these): its size in millimetres turned into the pixels the
// plots are laid out in (96 per inch, CSS's and Plotly's), the panels placed in the grid in their
// order (one can take two columns or two rows), their letters, a view's plot adapted to its cell
// (the text in points, the line width, a white background, no WebGL where vectors are wanted)
// and the figure's SVG put together from the plots' own, the letters over them.

export const PX_PAR_MM = 96 / 25.4;
export const PX_PAR_PT = 96 / 72;

// The widths journals print figures at, in millimetres: one column, one and a half, two.
export const LARGEURS = [{ mm: 85, colonnes: 1 }, { mm: 90, colonnes: 1 }, { mm: 114, colonnes: 1.5 }, { mm: 140, colonnes: 1.5 },
  { mm: 174, colonnes: 2 }, { mm: 190, colonnes: 2 }];
export const STYLES_LETTRES = ['A', 'a', '(A)', '(a)', ''];
export const COINS = ['hg', 'hd', 'bg', 'bd'];  // top left, top right, bottom left, bottom right
export const GRILLE_MAX = 6;
// More points than this in a figure's WebGL plots and they stay WebGL (a picture inside the SVG):
// as vectors they would make a file too heavy to open.
export const MAX_POINTS_SVG = 50000;

// A figure as kept in the project (moteur/serveur.R: p$figures, the JSON text of `contenu`):
// its width and height in mm (height 0: from the panels' aspect), its grid, the text size and
// line width in points, its letters, whether the plots keep their titles and legends, its panels
// in their order, and how it was last exported.
export const DEFAUTS = {
  version: 1, largeur: 174, hauteur: 0, lignes: 1, colonnes: 2, police: 8, ligne: 1,
  lettres: { style: 'A', gras: true, coin: 'hg', taille: 10 }, titres: false, legendes: true,
  panneaux: [], format: 'pdf', dpi: 300, compression: 'lzw',
};

const borne = (x, min, max, defaut) => (Number.isFinite(Number(x)) && x !== null && x !== '' ? Math.min(max, Math.max(min, Number(x))) : defaut);

// A figure read back (or new): every setting present and within its bounds, whatever the file held.
export function completer(spec = {}) {
  const s = { ...DEFAUTS, ...(spec && typeof spec === 'object' ? spec : {}) };
  const l = { ...DEFAUTS.lettres, ...(spec && spec.lettres) };
  return {
    ...s,
    version: 1,
    largeur: borne(s.largeur, 20, 400, DEFAUTS.largeur),
    hauteur: Number(s.hauteur) > 0 ? borne(s.hauteur, 10, 400, 0) : 0,
    lignes: Math.round(borne(s.lignes, 1, GRILLE_MAX, 1)),
    colonnes: Math.round(borne(s.colonnes, 1, GRILLE_MAX, 2)),
    police: borne(s.police, 4, 24, DEFAUTS.police),
    ligne: borne(s.ligne, 0.1, 4, DEFAUTS.ligne),
    lettres: { style: STYLES_LETTRES.includes(l.style) ? l.style : 'A', gras: l.gras !== false, coin: COINS.includes(l.coin) ? l.coin : 'hg',
      taille: borne(l.taille, 4, 36, DEFAUTS.lettres.taille) },
    titres: !!s.titres,
    legendes: s.legendes !== false,
    panneaux: (Array.isArray(s.panneaux) ? s.panneaux : []).filter((p) => p && typeof p.vue === 'string')
      .map((p) => ({ ...p, lignes: p.lignes === 2 ? 2 : 1, colonnes: p.colonnes === 2 ? 2 : 1 })),
    format: ['pdf', 'svg', 'png', 'tiff'].includes(s.format) ? s.format : 'pdf',
    dpi: [300, 600].includes(Number(s.dpi)) ? Number(s.dpi) : 300,
    compression: s.compression === 'aucune' ? 'aucune' : 'lzw',
  };
}

// The grid proposed for n panels in a figure `largeur` mm wide: side by side up to three, then in
// rows of two or three, with no more columns than cells of 40 mm (three plots side by side in an
// 85 mm column would be unreadable, and Plotly no longer fits their axes there).
export function grilleProposee(n, largeur = DEFAUTS.largeur) {
  let g = n <= 3 ? { lignes: 1, colonnes: Math.max(1, n) } : n === 4 ? { lignes: 2, colonnes: 2 }
    : n <= 6 ? { lignes: 2, colonnes: 3 } : n <= 9 ? { lignes: 3, colonnes: 3 } : { lignes: Math.ceil(n / 4), colonnes: 4 };
  const colonnesMax = Math.max(1, Math.floor(largeur / 40));
  if (g.colonnes > colonnesMax) g = { lignes: Math.ceil(n / colonnesMax), colonnes: colonnesMax };
  return { lignes: Math.min(GRILLE_MAX, g.lignes), colonnes: g.colonnes };
}

// The panels placed in the grid in their order: each at the first place where it fits, row by
// row, from the place after the previous one (a panel taking two columns or two rows needs both
// cells free); null for a panel the grid has no room for. The height: given, or from the panels'
// aspect (their height over their width on screen, `aspect`, kept between 0.55 and 1.2: a panel
// docked wide and flat on screen isn't a shape for print), the rows all as high, in whole mm.
// Rectangles in millimetres, from the top left corner.
export function disposer({ largeur, hauteur = 0, lignes, colonnes, panneaux = [] }) {
  const libre = Array.from({ length: lignes }, () => Array(colonnes).fill(true));
  const places = [];
  let curseur = 0;
  for (const p of panneaux) {
    const pl = Math.min(p.lignes === 2 ? 2 : 1, lignes);
    const pc = Math.min(p.colonnes === 2 ? 2 : 1, colonnes);
    let place = null;
    for (let k = curseur; k < lignes * colonnes && !place; k += 1) {
      const l = Math.floor(k / colonnes);
      const c = k % colonnes;
      if (l + pl > lignes || c + pc > colonnes) continue;
      let ok = true;
      for (let i = 0; i < pl && ok; i += 1) for (let j = 0; j < pc && ok; j += 1) ok = libre[l + i][c + j];
      if (ok) place = { ligne: l, colonne: c, lignes: pl, colonnes: pc };
    }
    if (place) {
      for (let i = 0; i < pl; i += 1) for (let j = 0; j < pc; j += 1) libre[place.ligne + i][place.colonne + j] = false;
      curseur = place.ligne * colonnes + place.colonne + pc;
    }
    places.push(place);
  }
  const lc = largeur / colonnes;
  let h = Number(hauteur) > 0 ? Number(hauteur) : 0;
  if (!h) {
    const aspect = (p) => borne(p.aspect, 0.55, 1.2, 0.7);
    const parLigne = places.map((pl, i) => (pl ? (lc * pl.colonnes * aspect(panneaux[i])) / pl.lignes : null)).filter((x) => x !== null);
    const hc = parLigne.length ? parLigne.reduce((a, b) => a + b, 0) / parLigne.length : lc * 0.7;
    h = Math.min(260, Math.max(20, Math.round(hc * lignes)));
  }
  const hc = h / lignes;
  return {
    largeur, hauteur: h,
    cellules: places.map((pl) => pl && { ...pl, x: pl.colonne * lc, y: pl.ligne * hc, largeur: pl.colonnes * lc, hauteur: pl.lignes * hc }),
  };
}

// A rectangle in mm as whole pixels: the edges rounded, so that cells side by side meet exactly.
export function enPixels(r) {
  const x0 = Math.round(r.x * PX_PAR_MM);
  const y0 = Math.round(r.y * PX_PAR_MM);
  return { x: x0, y: y0, largeur: Math.round((r.x + r.largeur) * PX_PAR_MM) - x0, hauteur: Math.round((r.y + r.hauteur) * PX_PAR_MM) - y0 };
}

// The i-th panel's letter (0: A), in a style: A, a, (A), (a); after Z, AA, AB...
export function lettre(i, style = 'A') {
  if (!style) return '';
  const base = style.includes('a') ? 'abcdefghijklmnopqrstuvwxyz' : 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  const x = i < 26 ? base[i] : base[Math.floor(i / 26) - 1] + base[i % 26];
  return style.startsWith('(') ? `(${x})` : x;
}

// Where a letter of `taille` px goes in its panel's rectangle (px): in the corner chosen, its
// baseline under the top edge by about a capital's height; `place`, the room it takes at the top
// of the plot (its margin), `largeur` about the width it takes (for a title beside it).
export function placeLettre(r, coin, taille, texte = 'A') {
  const marge = 0.2 * taille;
  const haut = coin[0] === 'h';
  const gauche = coin[1] === 'g';
  return {
    x: gauche ? r.x + marge : r.x + r.largeur - marge,
    y: haut ? r.y + marge + 0.74 * taille : r.y + r.hauteur - marge - 0.22 * taille,
    ancre: gauche ? 'start' : 'end',
    haut,
    place: 2 * marge + 0.98 * taille,
    largeur: 2 * marge + 0.66 * taille * String(texte).length,
  };
}

const TYPES_POINTS = new Set(['scatter', 'scattergl', 'scattermap', 'scattermapbox', 'scattergeo', 'scatterpolar', 'scatterpolargl', 'scatterternary', 'scatter3d',
  'box', 'violin']);
const TYPES_ECHELLE = new Set(['heatmap', 'contour', 'histogram2d', 'histogram2dcontour', 'surface']);
const NON_CARTESIENS = new Set(['pie', 'scattermap', 'scattermapbox', 'densitymap', 'choroplethmap', 'scattergeo', 'choropleth', 'scatter3d', 'surface', 'mesh3d',
  'scatterpolar', 'scatterpolargl', 'barpolar', 'scatterternary', 'parcoords', 'sankey', 'table', 'sunburst', 'treemap', 'indicator']);
const longueur = (x) => (x && typeof x.length === 'number' ? x.length : 0);

// A view's plot (its data and layout as Plotly holds them) at its cell's size for print. `o`:
// largeur, hauteur (px); police (the figure's text size, px): every tick label, axis title,
// legend and colour bar at that size, the plot's own smaller or larger texts (labels by the
// points, notes) and its markers and margins in proportion, from `base`, the text size the view
// was drawn with (Plots › Formatting); ligne (px): a line Plotly draws 2 px wide by default
// becomes this wide, the others in proportion (axes, grid and zero lines, 1 px by default: half
// of it), none drawn where there was none; titres: the plot's title kept or not; legende; zoom:
// the axes' ranges kept from its panel; lettre: { haut, place, largeur, coin } the room its
// letter takes; source: { largeur, hauteur } of the plot's area as drawn, for a map's zoom;
// vectoriel: WebGL scatter plots made SVG ones (up to MAX_POINTS_SVG points). The plot given
// is not changed.
export function adapterFigure(data, layout, o) {
  const k = o.police / (o.base || 12);
  const f = o.ligne / 2;
  const L = structuredClone(layout || {});
  const D = structuredClone(data || []);
  const police = (p) => ({ ...(p || {}), size: o.police });
  const enProportion = (p) => (p && typeof p.size === 'number' ? { ...p, size: p.size * k } : p);
  const largeurLigne = (w, defaut) => (w === 0 ? 0 : (typeof w === 'number' ? w : defaut) * f);
  const titreA = (x) => (x && typeof x === 'object' ? { ...x, font: police(x.font) } : typeof x === 'string' ? { text: x, font: police() } : x);

  Object.assign(L, { width: o.largeur, height: o.hauteur, autosize: false, paper_bgcolor: '#ffffff', plot_bgcolor: '#ffffff' });
  L.font = police(L.font);
  const titre = L.title && typeof L.title === 'object' ? L.title : { text: typeof L.title === 'string' ? L.title : '' };
  L.title = o.titres ? { ...titre, font: police(titre.font) } : { ...titre, text: '' };

  const axe = (a) => ({ ...a, tickfont: police(a.tickfont), title: titreA(a.title), automargin: a.automargin ?? true,
    linewidth: largeurLigne(a.linewidth, 1), zerolinewidth: largeurLigne(a.zerolinewidth, 1), gridwidth: largeurLigne(a.gridwidth, 1),
    tickwidth: largeurLigne(a.tickwidth, 1), ticklen: (typeof a.ticklen === 'number' ? a.ticklen : 5) * k });
  // (also the axes Plotly makes for a trace's subplot that the layout doesn't name)
  for (const tr of D) {
    if (NON_CARTESIENS.has(tr.type)) continue;
    for (const [cle, initiale] of [['xaxis', 'x'], ['yaxis', 'y']]) {
      const ref = typeof tr[cle] === 'string' ? tr[cle] : initiale;
      if (/^[xy]\d*$/.test(ref) && !(`${cle}${ref.slice(1)}` in L)) L[`${cle}${ref.slice(1)}`] = {};
    }
  }
  for (const cle of Object.keys(L)) {
    if (/^[xy]axis\d*$/.test(cle)) L[cle] = axe(L[cle] || {});
    else if (/^(scene|polar|ternary)\d*$/.test(cle) && L[cle] && typeof L[cle] === 'object') {
      L[cle] = { ...L[cle] };
      for (const sous of Object.keys(L[cle])) if (/axis$/.test(sous)) L[cle][sous] = axe(L[cle][sous] || {});
    }
  }
  L.legend = { ...(L.legend || {}), font: police((L.legend || {}).font) };
  if (L.legend.title && typeof L.legend.title === 'object') L.legend.title = { ...L.legend.title, font: police(L.legend.title.font) };
  if (o.legende === false) L.showlegend = false;
  if (Array.isArray(L.annotations)) L.annotations = L.annotations.map((a) => ({ ...a, font: enProportion(a.font) }));
  if (Array.isArray(L.shapes)) L.shapes = L.shapes.map((s) => (s && s.line ? { ...s, line: { ...s.line, width: largeurLigne(s.line.width, 2) } } : s));
  const barre = (c) => ({ ...c, thickness: (typeof c.thickness === 'number' ? c.thickness : 30) * k, tickfont: police(c.tickfont),
    title: titreA(c.title), outlinewidth: largeurLigne(c.outlinewidth, 1) });
  for (const cle of Object.keys(L)) if (/^coloraxis\d*$/.test(cle) && L[cle] && L[cle].colorbar) L[cle] = { ...L[cle], colorbar: barre(L[cle].colorbar) };

  // margins: those the view gave, in proportion to the text (the axes widen them for their labels);
  // without its title, the top one loses the title's room (34 px in tracer, panneaux.js) and keeps
  // what the view added above it (the names of small multiples, 56 px)
  const m = L.margin || {};
  const marge = (v, d) => Math.round((typeof v === 'number' ? v : d) * k);
  L.margin = { ...m, l: marge(m.l, 80), r: marge(m.r, 80), t: marge(m.t, 100), b: marge(m.b, 80), pad: marge(m.pad, 0) };
  if (!o.titres) L.margin.t = Math.min(L.margin.t, Math.round((Math.max(0, (typeof m.t === 'number' ? m.t : 100) - 34) + 12) * k));
  if (o.lettre && o.lettre.haut) L.margin.t = Math.max(L.margin.t, Math.ceil(o.lettre.place));
  // a title beside a letter in the top left corner
  if (o.titres && o.lettre && o.lettre.haut && o.lettre.coin === 'hg') {
    L.title = { ...L.title, xanchor: 'left', pad: { ...(L.title.pad || {}), l: Math.ceil(o.lettre.largeur) } };
  }
  if (o.zoom) {
    for (const [cle, r] of [['xaxis', o.zoom.x], ['yaxis', o.zoom.y]]) {
      if (Array.isArray(r) && r.length === 2 && r.every(Number.isFinite)) L[cle] = { ...(L[cle] || {}), range: [...r], autorange: false };
    }
  }
  // a map on tiles: the same area as in the view, whatever the cell's size (its zoom is a
  // power of 2 of the size: 512 px per world at zoom 0)
  for (const cle of Object.keys(L)) {
    if (/^map(box)?\d*$/.test(cle) && L[cle] && typeof L[cle].zoom === 'number' && o.source && o.source.largeur > 0 && o.source.hauteur > 0) {
      const r = Math.min((o.largeur - L.margin.l - L.margin.r) / o.source.largeur, (o.hauteur - L.margin.t - L.margin.b) / o.source.hauteur);
      if (r > 0 && Number.isFinite(r)) L[cle] = { ...L[cle], zoom: Math.max(0, L[cle].zoom + Math.log2(r)) };
    }
  }

  const pointsGL = D.reduce((n, tr) => n + (tr.type === 'scattergl' ? Math.max(longueur(tr.x), longueur(tr.y)) : 0), 0);
  const versSVG = o.vectoriel !== false && pointsGL <= MAX_POINTS_SVG;
  for (const tr of D) {
    if (tr.type === 'scattergl' && versSVG) tr.type = 'scatter';
    const type = tr.type || 'scatter';
    if (tr.textfont) tr.textfont = enProportion(tr.textfont);
    if (tr.insidetextfont) tr.insidetextfont = enProportion(tr.insidetextfont);
    if (tr.outsidetextfont) tr.outsidetextfont = enProportion(tr.outsidetextfont);
    if (TYPES_POINTS.has(type)) {
      tr.line = { ...(tr.line || {}), width: largeurLigne((tr.line || {}).width, 2) };
      if (tr.marker) {
        tr.marker = { ...tr.marker };
        if (typeof tr.marker.size === 'number') tr.marker.size *= k;
        else if (Array.isArray(tr.marker.size) || ArrayBuffer.isView(tr.marker.size)) tr.marker.size = Array.from(tr.marker.size, (v) => (typeof v === 'number' ? v * k : v));
      }
    }
    if (tr.marker && tr.marker.line && typeof tr.marker.line.width === 'number') tr.marker = { ...tr.marker, line: { ...tr.marker.line, width: largeurLigne(tr.marker.line.width, 0) } };
    if (tr.marker && (tr.marker.showscale || tr.marker.colorbar)) tr.marker = { ...tr.marker, colorbar: barre(tr.marker.colorbar || {}) };
    if (TYPES_ECHELLE.has(type) || tr.colorbar) tr.colorbar = barre(tr.colorbar || {});
    for (const e of ['error_x', 'error_y']) {
      if (tr[e]) tr[e] = { ...tr[e], thickness: largeurLigne(tr[e].thickness, 2), width: (typeof tr[e].width === 'number' ? tr[e].width : 4) * k };
    }
  }
  return { data: D, layout: L };
}

// ------------------------------------------------------------ the SVG

const echapper = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
const nombre = (x) => String(Math.round(x * 1000) / 1000);

// A plot's SVG with its ids made its own (`prefixe`), and every reference to them: two plots may
// name a clip path or a gradient alike, and one figure holds them all.
export function isolerIds(svg, prefixe) {
  const ids = new Set([...svg.matchAll(/\sid="([^"]+)"/g)].map((r) => r[1]));
  if (!ids.size) return svg;
  const nouveau = (id) => (ids.has(id) ? prefixe + id : id);
  return svg
    .replace(/(\sid=")([^"]+)"/g, (_, a, id) => `${a}${nouveau(id)}"`)
    .replace(/url\((&quot;|['"]?)#([^'")&]+)\1\)/g, (_, q, id) => `url(${q}#${nouveau(id)}${q})`)
    .replace(/((?:xlink:)?href=")#([^"]+)"/g, (_, a, id) => `${a}#${nouveau(id)}"`);
}

// A plot's SVG (Plotly's toImage) nested in the figure's at rectangle `r` (px).
export function imbriquer(svg, r, prefixe) {
  const s = isolerIds(String(svg).replace(/^\s*<\?xml[^>]*\?>\s*/, ''), prefixe);
  const debut = s.indexOf('<svg');
  const fin = s.indexOf('>', debut);
  if (debut < 0 || fin < 0) return '';
  const tete = s.slice(debut, fin + 1);
  const interieur = tete.endsWith('/>') ? '' : s.slice(fin + 1, s.lastIndexOf('</svg>'));
  const attribut = (n) => (new RegExp(`\\s${n}="([^"]*)"`).exec(tete) || [])[1];
  const w = parseFloat(attribut('width')) || r.largeur;
  const h = parseFloat(attribut('height')) || r.hauteur;
  const vb = attribut('viewBox') || `0 0 ${nombre(w)} ${nombre(h)}`;
  return `<svg x="${nombre(r.x)}" y="${nombre(r.y)}" width="${nombre(r.largeur)}" height="${nombre(r.hauteur)}" viewBox="${vb}" overflow="hidden">${interieur}</svg>`;
}

// The figure's SVG: a white page of largeur × hauteur mm (its user units the plots' pixels, 96
// per inch), the plots at their places, the letters over them; `pixels`: { largeur, hauteur }
// for a picture to draw at that size (PNG, TIFF), otherwise sized in millimetres. `panneaux`:
// [{ svg, x, y, largeur, hauteur }] (px; svg null: a grey cell, `vide`'s text in it); `lettres`:
// [{ texte, x, y, ancre, taille (px), gras, police }].
export function composerSVG({ largeur, hauteur, panneaux = [], lettres = [], pixels = null }) {
  const W = largeur * PX_PAR_MM;
  const H = hauteur * PX_PAR_MM;
  const taille = pixels ? `width="${pixels.largeur}" height="${pixels.hauteur}"` : `width="${nombre(largeur)}mm" height="${nombre(hauteur)}mm"`;
  const corps = panneaux.map((p, i) => (p.svg ? imbriquer(p.svg, p, `f${i + 1}-`)
    : `<g><rect x="${nombre(p.x + 2)}" y="${nombre(p.y + 2)}" width="${nombre(Math.max(0, p.largeur - 4))}" height="${nombre(Math.max(0, p.hauteur - 4))}" fill="#f1f3f5" stroke="#c3c8cf" stroke-dasharray="4 3"/>`
      + (p.vide ? `<text x="${nombre(p.x + p.largeur / 2)}" y="${nombre(p.y + p.hauteur / 2)}" text-anchor="middle" font-family="Arial, sans-serif" font-size="11" fill="#5f6773">${echapper(p.vide)}</text>` : '')
      + '</g>')).join('');
  const textes = lettres.filter((l) => l.texte).map((l) => `<text x="${nombre(l.x)}" y="${nombre(l.y)}" text-anchor="${l.ancre}" font-family="${echapper(l.police || 'Arial, sans-serif')}"`
    + ` font-size="${nombre(l.taille)}"${l.gras ? ' font-weight="bold"' : ''} fill="#000000">${echapper(l.texte)}</text>`).join('');
  return `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" ${taille} viewBox="0 0 ${nombre(W)} ${nombre(H)}">`
    + `<rect x="0" y="0" width="${nombre(W)}" height="${nombre(H)}" fill="#ffffff"/>${corps}${textes}</svg>`;
}

// The page a PDF of the figure is printed from: exactly its size, no margin (app/main.js prints
// it at that size; a browser's print dialog reads @page).
export function pagePDF(svg, largeur, hauteur, titre = '', langue = 'fr') {
  return `<!doctype html><html lang="${langue}"><head><meta charset="utf-8"><title>${echapper(titre)}</title><style>
@page { size: ${nombre(largeur)}mm ${nombre(hauteur)}mm; margin: 0; }
html, body { margin: 0; padding: 0; background: #ffffff; -webkit-print-color-adjust: exact; print-color-adjust: exact; }
body { width: ${nombre(largeur)}mm; height: ${nombre(hauteur)}mm; overflow: hidden; }
body > svg { display: block; width: ${nombre(largeur)}mm; height: ${nombre(hauteur)}mm; }
</style></head><body>${svg.replace(/^\s*<\?xml[^>]*\?>\s*/, '')}</body></html>`;
}

// The pixels of a picture of the figure at `dpi` dots per inch.
export const pixelsImage = (largeur, hauteur, dpi) => ({ largeur: Math.round((largeur / 25.4) * dpi), hauteur: Math.round((hauteur / 25.4) * dpi) });
