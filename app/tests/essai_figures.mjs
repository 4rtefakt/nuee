// Checks interface/composition.js and interface/images.js (the figures for publication, figures.js),
// without the application: the panels placed in the grid, the letters, a plot adapted to print
// size, a map on tiles made a plane plot and its tiles chosen for a resolution within the caps,
// the figure's SVG put together; then PNG and TIFF files written, read back by readers
// written here from the formats' specifications (zlib for PNG's Deflate), their pixels, sizes
// and resolutions compared with what was asked.
//   node tests/essai_figures.mjs [folder where the files written are kept]
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import zlib from 'node:zlib';
import { PX_PAR_MM, PX_PAR_PT, completer, grilleProposee, disposer, enPixels, lettre, placeLettre, adapterFigure, isolerIds, composerSVG, pagePDF,
  pixelsImage, MAX_POINTS_SVG, fondDeCarte, mercatorX, mercatorY, carteEnPlan, cadrer, tuilesPour, adresseTuile, placeTuile, TUILES_MAX, ZOOM_EN_PLUS,
  ZOOM_SERVEUR } from '../interface/composition.js';
import { crc32, ecrirePNG, ecrireTIFF, lzw, pixelsParMetre } from '../interface/images.js';

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);
const proche = (a, b, e = 1e-6) => Math.abs(a - b) <= e;
const garde = process.argv[2] || null;
const dossier = garde || fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-figures-'));
if (garde) fs.mkdirSync(garde, { recursive: true });

console.log('Les panneaux dans la grille');
const placesDe = (d) => d.cellules.map((c) => c && [c.ligne, c.colonne, c.lignes, c.colonnes]);
egal('2 × 2, le premier sur deux colonnes : B et C dessous', placesDe(disposer({ largeur: 174, lignes: 2, colonnes: 2, panneaux: [{ colonnes: 2 }, {}, {}] })),
  [[0, 0, 1, 2], [1, 0, 1, 1], [1, 1, 1, 1]]);
egal('2 × 2, le deuxième sur deux lignes : C sous A', placesDe(disposer({ largeur: 174, lignes: 2, colonnes: 2, panneaux: [{}, { lignes: 2 }, {}] })),
  [[0, 0, 1, 1], [0, 1, 2, 1], [1, 0, 1, 1]]);
egal('un panneau de trop dans 1 × 2 : pas de place', placesDe(disposer({ largeur: 85, lignes: 1, colonnes: 2, panneaux: [{}, {}, {}] })),
  [[0, 0, 1, 1], [0, 1, 1, 1], null]);
egal('deux colonnes dans une grille d’une colonne : une seule', placesDe(disposer({ largeur: 85, lignes: 2, colonnes: 1, panneaux: [{ colonnes: 2 }, {}] })),
  [[0, 0, 1, 1], [1, 0, 1, 1]]);
egal('l’ordre de lecture : après un panneau sur deux lignes, la place suivante', placesDe(disposer({ largeur: 174, lignes: 2, colonnes: 3, panneaux: [{ lignes: 2 }, {}, {}, {}, {}] })),
  [[0, 0, 2, 1], [0, 1, 1, 1], [0, 2, 1, 1], [1, 1, 1, 1], [1, 2, 1, 1]]);
const auto = disposer({ largeur: 174, lignes: 1, colonnes: 2, panneaux: [{ aspect: 0.75 }, { aspect: 0.75 }] });
egal('hauteur tirée de l’aspect des panneaux (87 mm × 0,75)', auto.hauteur, 65);
egal('hauteur donnée', disposer({ largeur: 174, hauteur: 120, lignes: 2, colonnes: 2, panneaux: [{}] }).hauteur, 120);
egal('rectangles en mm', disposer({ largeur: 174, hauteur: 120, lignes: 2, colonnes: 2, panneaux: [{}, {}, { colonnes: 2 }] }).cellules.map((c) => [c.x, c.y, c.largeur, c.hauteur]),
  [[0, 0, 87, 60], [87, 0, 87, 60], [0, 60, 174, 60]]);
const trois = disposer({ largeur: 174, hauteur: 60, lignes: 1, colonnes: 3, panneaux: [{}, {}, {}] }).cellules.map(enPixels);
verifier('en pixels, les cellules voisines se touchent et finissent au bord', trois[0].x === 0 && trois[0].x + trois[0].largeur === trois[1].x
  && trois[1].x + trois[1].largeur === trois[2].x && trois[2].x + trois[2].largeur === Math.round(174 * PX_PAR_MM), JSON.stringify(trois));
egal('grilles proposées', [1, 2, 3, 4, 5, 7, 11].map((n) => { const g = grilleProposee(n); return `${g.lignes}×${g.colonnes}`; }), ['1×1', '1×2', '1×3', '2×2', '2×3', '3×3', '3×4']);
egal('… dans une colonne de 85 mm : pas plus de deux cases côte à côte', [1, 2, 3, 4, 5].map((n) => { const g = grilleProposee(n, 85); return `${g.lignes}×${g.colonnes}`; }), ['1×1', '1×2', '2×2', '2×2', '3×2']);
egal('lettres', [lettre(0), lettre(2, 'a'), lettre(1, '(A)'), lettre(3, '(a)'), lettre(26), lettre(4, '')], ['A', 'c', '(B)', '(d)', 'AA', '']);
const c = completer({ largeur: 1000, hauteur: -3, lignes: 9, police: 'x', lettres: { coin: 'zz', style: 'a' }, dpi: 450, panneaux: [{ vue: 'scores', colonnes: 3 }, null, { x: 1 }] });
egal('réglages relus dans leurs bornes', [c.largeur, c.hauteur, c.lignes, c.police, c.lettres.coin, c.lettres.style, c.dpi, c.panneaux.length, c.panneaux[0].colonnes],
  [400, 0, 6, 8, 'hg', 'a', 300, 1, 1]);
const pl = placeLettre({ x: 10, y: 20, largeur: 300, hauteur: 200 }, 'hg', 13.33, 'A');
const pr = placeLettre({ x: 10, y: 20, largeur: 300, hauteur: 200 }, 'bd', 13.33, 'A');
verifier('lettre en haut à gauche, en bas à droite', pl.ancre === 'start' && pl.x > 10 && pl.y > 20 && pl.y < 20 + pl.place && pl.haut
  && pr.ancre === 'end' && pr.x < 310 && pr.y < 220 && pr.y > 200 && !pr.haut, JSON.stringify([pl, pr]));
egal('pixels d’une image : 85 × 60 mm à 300 et 600 points par pouce', [pixelsImage(85, 60, 300), pixelsImage(85, 60, 600)],
  [{ largeur: 1004, hauteur: 709 }, { largeur: 2008, hauteur: 1417 }]);

console.log('Un graphique à la taille de l’impression');
const data = [
  { type: 'scattergl', mode: 'markers+text', x: [1, 2, 3], y: [3, 1, 2], marker: { size: [7, 9, 7], color: 'red', showscale: true }, textfont: { size: 10 } },
  { type: 'scatter', mode: 'lines', x: [0, 1], y: [0, 1], line: { width: 0 }, fill: 'tonexty' },
  { type: 'scatter', mode: 'lines', x: [0, 1], y: [1, 0], line: { color: '#000' }, xaxis: 'x2', yaxis: 'y2' },
  { type: 'bar', x: ['a'], y: [1], error_y: { array: [0.1], thickness: 1 } },
];
const layout = { font: { family: 'Arial', size: 12 }, title: { text: 'M1 · Scores', font: { size: 13 }, x: 0.01, xanchor: 'left' }, xaxis: { title: { text: 't[1]' } },
  yaxis: { title: 't[2]', gridcolor: '#eee' }, legend: { font: { size: 11 } }, margin: { l: 60, r: 18, t: 34, b: 48 },
  annotations: [{ text: 'n = 3', font: { size: 10 } }], shapes: [{ type: 'circle', line: { width: 1, dash: 'dot' } }], paper_bgcolor: '#1d2025' };
const avant = JSON.stringify([data, layout]);
const police = 8 * PX_PAR_PT;
const k = police / 12;
const lettreA = placeLettre({ x: 0, y: 0, largeur: 300, hauteur: 220 }, 'hg', 10 * PX_PAR_PT, 'A');
const a = adapterFigure(data, layout, { largeur: 300, hauteur: 220, police, base: 12, ligne: 1 * PX_PAR_PT, titres: false, legende: true, lettre: { ...lettreA, coin: 'hg' } });
verifier('le graphique donné n’est pas changé', JSON.stringify([data, layout]) === avant);
egal('taille et fond blanc', [a.layout.width, a.layout.height, a.layout.paper_bgcolor, a.layout.plot_bgcolor], [300, 220, '#ffffff', '#ffffff']);
verifier('texte, graduations, titres d’axes et légende à la taille de la figure', [a.layout.font.size, a.layout.xaxis.tickfont.size, a.layout.xaxis.title.font.size,
  a.layout.yaxis.title.font.size, a.layout.legend.font.size, a.layout.xaxis2.tickfont.size].every((x) => proche(x, police)) && a.layout.yaxis.title.text === 't[2]',
JSON.stringify([a.layout.font, a.layout.xaxis, a.layout.yaxis.title, a.layout.legend]));
verifier('étiquettes des points, notes et marqueurs en proportion', proche(a.data[0].textfont.size, 10 * k) && proche(a.layout.annotations[0].font.size, 10 * k)
  && proche(a.data[0].marker.size[1], 9 * k), JSON.stringify([a.data[0].textfont, a.layout.annotations, a.data[0].marker.size]));
verifier('WebGL devenu SVG', a.data[0].type === 'scatter' && a.data[3].type === 'bar');
verifier('largeurs de ligne : 2 px par défaut → la largeur de la figure, 0 reste 0, axes et formes à la moitié',
  proche(a.data[2].line.width, PX_PAR_PT) && a.data[1].line.width === 0 && proche(a.layout.shapes[0].line.width, PX_PAR_PT / 2)
  && proche(a.layout.xaxis.gridwidth, PX_PAR_PT / 2) && proche(a.data[3].error_y.thickness, PX_PAR_PT / 2) && a.layout.shapes[0].line.dash === 'dot',
JSON.stringify([a.data[2].line, a.data[1].line, a.layout.shapes[0].line, a.layout.xaxis.gridwidth, a.data[3].error_y]));
verifier('barre de couleur : graduations à la taille de la figure, épaisseur en proportion', proche(a.data[0].marker.colorbar.tickfont.size, police)
  && proche(a.data[0].marker.colorbar.thickness, 30 * k), JSON.stringify(a.data[0].marker.colorbar));
egal('titre caché, marges en proportion, place pour la lettre en haut', [a.layout.title.text, a.layout.margin.l, a.layout.margin.b, a.layout.margin.t],
  ['', Math.round(60 * k), Math.round(48 * k), Math.ceil(lettreA.place)]);
const b = adapterFigure(data, layout, { largeur: 300, hauteur: 220, police, base: 12, ligne: 2, titres: true, legende: false, lettre: { ...lettreA, coin: 'hg' },
  zoom: { x: [0, 2], y: null } });
verifier('titre gardé, décalé après la lettre ; légende cachée ; zoom de la vue gardé', b.layout.title.text === 'M1 · Scores' && b.layout.title.pad.l === Math.ceil(lettreA.largeur)
  && proche(b.layout.title.font.size, police) && b.layout.showlegend === false && b.layout.xaxis.range.join() === '0,2' && b.layout.xaxis.autorange === false
  && b.layout.yaxis.autorange === undefined, JSON.stringify([b.layout.title, b.layout.showlegend, b.layout.xaxis.range]));
const multiples = adapterFigure([], { margin: { l: 60, r: 18, t: 56, b: 48 }, annotations: [{ text: 'site A', yanchor: 'bottom', font: { size: 12 } }] },
  { largeur: 300, hauteur: 220, police, base: 12, ligne: 1, titres: false });
egal('sans titre, la place au-dessus des petits graphiques (séries) reste', multiples.layout.margin.t, Math.round((56 - 34 + 12) * k));
const nuage = { type: 'scattergl', x: new Float64Array(MAX_POINTS_SVG + 1), y: new Float64Array(MAX_POINTS_SVG + 1) };
verifier(`plus de ${MAX_POINTS_SVG} points : WebGL gardé`, adapterFigure([nuage], {}, { largeur: 100, hauteur: 100, police, ligne: 1 }).data[0].type === 'scattergl');
const carte = adapterFigure([{ type: 'scattermap', lon: [1], lat: [2] }], { map: { zoom: 10, center: { lon: 1, lat: 2 } }, margin: { l: 0, r: 0, t: 0, b: 0 } },
  { largeur: 400, hauteur: 300, police: 12, base: 12, ligne: 2, titres: true, source: { largeur: 800, hauteur: 600 } });
verifier('carte sur fond : la même étendue à la taille de la cellule (zoom − 1 pour une cellule moitié moins grande)', proche(carte.layout.map.zoom, 9), String(carte.layout.map.zoom));

console.log('Une carte sur fond de tuiles');
const IGN = { version: 8, sources: { fond: { type: 'raster', tileSize: 256, attribution: '© IGN – Orthophotographies',
  tiles: ['https://data.geopf.fr/wmts?SERVICE=WMTS&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}&FORMAT=image/jpeg'] } },
layers: [{ id: 'fond', type: 'raster', source: 'fond' }] };
const osm = fondDeCarte('open-street-map');
const ign = fondDeCarte(IGN);
verifier('fonds : OpenStreetMap (le style de Plotly), une source raster (IGN), pas de tuiles sinon', osm.tuiles === 'https://tile.openstreetmap.org/{z}/{x}/{y}.png'
  && osm.attribution === '© OpenStreetMap contributors' && !osm.photo && ign.attribution === '© IGN – Orthophotographies' && ign.photo && ign.taille === 256
  && fondDeCarte('white-bg') === null && fondDeCarte(undefined) === null, JSON.stringify([osm, ign]));
verifier('Web Mercator : 180° de longitude → π, 85,05° de latitude → π, l’équateur → 0', proche(mercatorX(180), Math.PI) && proche(mercatorY(85.0511287798), Math.PI, 1e-9)
  && proche(mercatorY(0), 0) && proche(mercatorY(-89), -Math.PI, 1e-9));
const vueCarte = { data: [
  { type: 'scattermap', mode: 'markers', lon: [4.6, 4.65, null], lat: [43.66, 43.69, 43.7], marker: { size: [12, 12, 12], color: '#ffffff', opacity: 0.9 }, hoverinfo: 'skip' },
  { type: 'scattermap', mode: 'markers', lon: [4.6, 4.65, null], lat: [43.66, 43.69, 43.7], marker: { size: [8, 8, 8], color: [1, 2, 3], showscale: true, colorbar: { thickness: 12 } } }],
layout: { font: { size: 12 }, margin: { l: 8, r: 8, t: 34, b: 8 }, map: { style: 'open-street-map', center: { lon: 0, lat: 0 }, zoom: 0 }, legend: { x: 1, xanchor: 'right' } } };
const avantPlan = JSON.stringify(vueCarte);
const plan = carteEnPlan(vueCarte.data, vueCarte.layout, { largeur: 512, hauteur: 512 });
verifier('le graphique donné n’est pas changé', JSON.stringify(vueCarte) === avantPlan);
verifier('à zoom 0, 512 px : le monde entier, de −π à π', plan.layout.xaxis.range.every((v, i) => proche(v, [-Math.PI, Math.PI][i]))
  && plan.layout.yaxis.range.every((v, i) => proche(v, [-Math.PI, Math.PI][i])) && proche(plan.vue.dx, Math.PI) && plan.vue.cx === 0,
JSON.stringify([plan.layout.xaxis, plan.layout.yaxis]));
const zoome = carteEnPlan(vueCarte.data, { ...vueCarte.layout, map: { style: IGN, center: { lon: 90, lat: 0 }, zoom: 1 } }, { largeur: 512, hauteur: 256 });
verifier('à zoom 1, 512 × 256 px autour de 90° est : la moitié du monde en largeur, un quart en hauteur', proche(zoome.layout.xaxis.range[0], 0)
  && proche(zoome.layout.xaxis.range[1], Math.PI) && proche(zoome.layout.yaxis.range[0], -Math.PI / 4) && proche(zoome.layout.yaxis.range[1], Math.PI / 4)
  && zoome.fond.photo, JSON.stringify([zoome.layout.xaxis.range, zoome.layout.yaxis.range]));
verifier('points : WebGL aux x et y de Web Mercator, sans contour (les cercles de MapLibre n’en ont pas), une position manquante reste manquante',
  plan.data.every((tr) => tr.type === 'scattergl' && !('lon' in tr) && !('lat' in tr) && tr.marker.line.width === 0)
  && proche(plan.data[1].x[1], mercatorX(4.65)) && proche(plan.data[1].y[0], mercatorY(43.66)) && plan.data[1].x[2] === null
  && plan.data[1].marker.colorbar.thickness === 12 && plan.data[0].hoverinfo === 'skip', JSON.stringify(plan.data[1]));
verifier('axes cachés, plus de sous-graphique carte ; l’attribution en bas à droite, un peu plus petite que le texte', !('map' in plan.layout)
  && plan.layout.xaxis.visible === false && plan.layout.yaxis.visible === false && plan.layout.annotations.length === 1
  && plan.layout.annotations[0].text === '© OpenStreetMap contributors' && plan.layout.annotations[0].x === 1 && plan.layout.annotations[0].y === 0
  && plan.layout.annotations[0].xanchor === 'right' && proche(plan.layout.annotations[0].font.size, 9.6), JSON.stringify(plan.layout.annotations));
verifier('sans fond de tuiles : pas de plan', carteEnPlan(vueCarte.data, { map: { style: 'white-bg', center: { lon: 0, lat: 0 }, zoom: 3 } }, { largeur: 100, hauteur: 100 }) === null
  && carteEnPlan(data, layout, { largeur: 100, hauteur: 100 }) === null);
const imprime = adapterFigure(plan.data, plan.layout, { largeur: 300, hauteur: 220, police, base: 12, ligne: PX_PAR_PT, titres: false });
verifier('adaptée à sa cellule : points SVG à la taille de la figure, attribution en proportion', imprime.data.every((tr) => tr.type === 'scatter')
  && proche(imprime.data[1].marker.size[0], 8 * k) && imprime.data[1].marker.line.width === 0 && proche(imprime.layout.annotations[0].font.size, 0.8 * police),
JSON.stringify([imprime.data[1].marker, imprime.layout.annotations[0].font]));
const cadre = cadrer({ cx: 1, cy: 2, dx: 0.5, dy: 0.25 }, 400, 400);
verifier('cadrer : l’étendue de la vue entière et centrée, échelles égales (ici plus haute que demandé)', proche(cadre.x[0], 0.5) && proche(cadre.x[1], 1.5)
  && proche(cadre.y[0], 1.5) && proche(cadre.y[1], 2.5), JSON.stringify(cadre));
const monde = { x: [-Math.PI, Math.PI], y: [-Math.PI, Math.PI] };
const tz = (densite, largeur = 256, etendue = monde) => tuilesPour({ ...etendue, largeur, densite });
egal('le monde sur 256 px : à l’écran une tuile ; à 2 pixels par px, le niveau 1 et ses 4 tuiles',
  [tz(1), tz(2)].map((r) => [r.zoom, r.nombre, r.densite, r.limite]), [[0, 1, 1, null], [1, 4, 2, null]]);
egal('300 points par pouce (3,125 pixels par px) : le premier niveau qui en donne au moins autant (4, sur 4 tuiles par côté)',
  [tz(300 / 96)].map((r) => [r.zoom, r.colonnes, r.lignes, r.densite, r.limite]), [[2, [0, 3], [0, 3], 4, null]]);
egal(`au plus ${ZOOM_EN_PLUS} niveaux au-dessus de l’écran : 600 points par pouce donnent 4 pixels par px, dits limités`,
  [tz(600 / 96)].map((r) => [r.zoom, r.densite, r.limite]), [[2, 4, 'zoom']]);
const grand = tz(1, 4096);
verifier(`au plus ${TUILES_MAX} tuiles : un niveau de moins (64 au lieu de 256)`, grand.zoom === 3 && grand.nombre === 64 && grand.densite === 0.5
  && grand.limite === 'nombre' && TUILES_MAX < 256, JSON.stringify(grand));
const petit = { x: [0, (2 * Math.PI) / 2 ** 18.5 * (300 / 256)], y: [0, (2 * Math.PI) / 2 ** 18.5 * (300 / 256)] };
const tresPres = tz(300 / 96, 300, petit);
verifier(`pas au-delà du niveau ${ZOOM_SERVEUR}`, tresPres.zoom === ZOOM_SERVEUR && tresPres.limite === 'serveur' && tresPres.densite < 300 / 96, JSON.stringify(tresPres));
// a map of Arles in a cell: its tiles cover the plot area, no column or row more
const arles = { x: [mercatorX(4.6), mercatorX(4.66)], y: [mercatorY(43.66), mercatorY(43.692)] };
const ta = tuilesPour({ ...arles, largeur: 239, densite: 300 / 96 });
const bord = (c, z) => (c / 2 ** z) * 2 * Math.PI - Math.PI;
const bordY = (l, z) => Math.PI - (l / 2 ** z) * 2 * Math.PI;
verifier('Arles sur 239 px à 300 points par pouce : les tuiles couvrent la zone, sans colonne ni ligne de trop', ta.densite >= 300 / 96 && ta.densite < 2 * 300 / 96
  && bord(ta.colonnes[0], ta.zoom) <= arles.x[0] && bord(ta.colonnes[0] + 1, ta.zoom) > arles.x[0] && bord(ta.colonnes[1] + 1, ta.zoom) >= arles.x[1]
  && bord(ta.colonnes[1], ta.zoom) < arles.x[1] && bordY(ta.lignes[0], ta.zoom) >= arles.y[1] && bordY(ta.lignes[0] + 1, ta.zoom) < arles.y[1]
  && bordY(ta.lignes[1] + 1, ta.zoom) <= arles.y[0] && bordY(ta.lignes[1], ta.zoom) > arles.y[0], JSON.stringify(ta));
const image300 = { ...arles, largeur: Math.round(239 * 300 / 96), hauteur: 600 };
const places = [];
for (let c = ta.colonnes[0]; c <= ta.colonnes[1]; c += 1) for (let l = ta.lignes[0]; l <= ta.lignes[1]; l += 1) places.push([c, l, placeTuile(ta.zoom, c, l, image300)]);
const voisine = (c, l) => (places.find((p) => p[0] === c && p[1] === l) || [])[2];
verifier('tuiles placées : bord à bord, l’image entière couverte', places.every(([c, l, p]) => (!voisine(c + 1, l) || p.x + p.largeur === voisine(c + 1, l).x)
  && (!voisine(c, l + 1) || p.y + p.hauteur === voisine(c, l + 1).y))
  && Math.min(...places.map(([, , p]) => p.x)) <= 0 && Math.max(...places.map(([, , p]) => p.x + p.largeur)) >= image300.largeur
  && Math.min(...places.map(([, , p]) => p.y)) <= 0 && Math.max(...places.map(([, , p]) => p.y + p.hauteur)) >= image300.hauteur);
egal('tuile placée : le monde au niveau 1 sur 512 × 512 pixels', [placeTuile(1, 0, 0, { ...monde, largeur: 512, hauteur: 512 }), placeTuile(1, 1, 1, { ...monde, largeur: 512, hauteur: 512 })],
  [{ x: 0, y: 0, largeur: 256, hauteur: 256 }, { x: 256, y: 256, largeur: 256, hauteur: 256 }]);
egal('adresse d’une tuile : niveau, colonne (modulo le monde), ligne', [adresseTuile(osm, 3, -1, 2), adresseTuile(ign, 15, 16801, 11953).replace(/^.*TILEMATRIX/, 'TILEMATRIX')],
  ['https://tile.openstreetmap.org/3/7/2.png', 'TILEMATRIX=15&TILEROW=11953&TILECOL=16801&FORMAT=image/jpeg']);

console.log('Le SVG de la figure');
const svgA ='<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="300" height="220" viewBox="0 0 300 220" class="main-svg">'
  + '<defs><clipPath id="clip1xy"><rect width="10" height="10"/></clipPath><linearGradient id="g1"/></defs>'
  + '<g clip-path="url(#clip1xy)"><path style="fill: url(\'#g1\'); stroke: url(&quot;#g1&quot;)" d="M0,0L1,1"/></g><use xlink:href="#clip1xy"/><g clip-path="url(#ailleurs)"/></svg>';
const iso = isolerIds(svgA, 'f1-');
verifier('identifiants rendus propres au graphique, et leurs renvois', iso.includes('id="f1-clip1xy"') && iso.includes('url(#f1-clip1xy)') && iso.includes('url(\'#f1-g1\')')
  && iso.includes('url(&quot;#f1-g1&quot;)') && iso.includes('xlink:href="#f1-clip1xy"') && iso.includes('url(#ailleurs)'), iso);
const cellules = disposer({ largeur: 174, hauteur: 60, lignes: 1, colonnes: 2, panneaux: [{}, {}] }).cellules.map(enPixels);
const lettres = cellules.map((r, i) => ({ texte: lettre(i), ...placeLettre(r, 'hg', 10 * PX_PAR_PT, lettre(i)), taille: 10 * PX_PAR_PT, gras: true, police: "'Segoe UI', Arial" }));
const figure = composerSVG({ largeur: 174, hauteur: 60, panneaux: [{ svg: svgA, ...cellules[0] }, { svg: svgA, ...cellules[1] }], lettres });
const ids = [...figure.matchAll(/\sid="([^"]+)"/g)].map((r) => r[1]);
verifier('deux fois le même graphique : aucun identifiant en double', ids.length === 4 && new Set(ids).size === 4, ids.join(', '));
verifier('taille en millimètres, unités en pixels à 96 par pouce', /^<svg [^>]*width="174mm" height="60mm" viewBox="0 0 657\.638 226\.772"/.test(figure), figure.slice(0, 220));
verifier('graphiques imbriqués à leur place', figure.includes(`<svg x="${cellules[1].x}" y="0" width="${cellules[1].largeur}" height="${cellules[1].hauteur}" viewBox="0 0 300 220"`), figure.slice(0, 600));
verifier('lettres en gras, A puis B', /<text [^>]*font-weight="bold" fill="#000000">A<\/text><text [^>]*>B<\/text><\/svg>$/.test(figure));
// (a reader of XML's tags: each opened is closed, in order)
const bienForme = (s) => {
  const pile = [];
  for (const [, fin, nom, auto] of [...s.replace(/<\?xml[^>]*\?>/, '').matchAll(/<(\/?)([A-Za-z][\w:-]*)[^>]*?(\/?)>/g)]) {
    if (fin) { if (pile.pop() !== nom) return false; } else if (!auto) pile.push(nom);
  }
  return pile.length === 0;
};
verifier('SVG bien formé', bienForme(figure));
const image = composerSVG({ largeur: 85, hauteur: 60, panneaux: [{ svg: null, vide: 'B : <absent>', x: 0, y: 0, largeur: 100, hauteur: 80 }], pixels: pixelsImage(85, 60, 300) });
verifier('pour une image : taille en pixels ; un panneau absent reste une case grise avec sa raison', /width="1004" height="709"/.test(image) && image.includes('B : &lt;absent&gt;') && bienForme(image));
const page = pagePDF(figure, 174, 60, 'Figure 1', 'fr');
verifier('page du PDF à la taille de la figure, sans marge', page.includes('@page { size: 174mm 60mm; margin: 0; }') && page.includes('<body><svg ') && !page.includes('<?xml'));

console.log('PNG et TIFF écrits puis relus');
// a figure-like picture: white, a frame, coloured blocks, a gradient, some noise
function dessin(largeur, hauteur) {
  const rgba = new Uint8Array(largeur * hauteur * 4).fill(255);
  let graine = 11;
  const alea = () => { graine = (graine * 16807) % 2147483647; return graine / 2147483647; };
  for (let y = 0; y < hauteur; y += 1) {
    for (let x = 0; x < largeur; x += 1) {
      const i = 4 * (y * largeur + x);
      if (x === 2 || y === 2 || x === largeur - 3 || y === hauteur - 3) rgba.set([31, 35, 40], i);
      else if (x > largeur * 0.1 && x < largeur * 0.3 && y > hauteur * 0.2 && y < hauteur * 0.6) rgba.set([47, 111, 181], i);
      else if (x > largeur * 0.4 && x < largeur * 0.6) rgba.set([Math.round((255 * x) / largeur), Math.round((255 * y) / hauteur), 120], i);
      else if (x > largeur * 0.7 && y > hauteur * 0.5) rgba.set([Math.floor(alea() * 256), Math.floor(alea() * 256), Math.floor(alea() * 256)], i);
    }
  }
  return rgba;
}
const rgbDe = (rgba) => { const o = new Uint8Array((rgba.length / 4) * 3); for (let i = 0, j = 0; i < rgba.length; i += 4, j += 3) o.set(rgba.subarray(i, i + 3), j); return o; };
const memes = (x, y) => x.length === y.length && x.every((v, i) => v === y[i]);

// PNG: chunks and their CRC, IHDR, pHYs, then the rows unfiltered
const crcZlib = zlib.crc32 || ((o) => crc32(o));
function lirePNG(o) {
  const signature = [137, 80, 78, 71, 13, 10, 26, 10].every((v, i) => o[i] === v);
  const v = new DataView(o.buffer, o.byteOffset, o.byteLength);
  const morceaux = [];
  for (let k = 8; k < o.length;) {
    const n = v.getUint32(k);
    const type = String.fromCharCode(...o.subarray(k + 4, k + 8));
    const donnees = o.subarray(k + 8, k + 8 + n);
    morceaux.push({ type, donnees, crcBon: v.getUint32(k + 8 + n) === crcZlib(o.subarray(k + 4, k + 8 + n)) });
    k += 12 + n;
  }
  const ihdr = morceaux.find((m) => m.type === 'IHDR').donnees;
  const d = new DataView(ihdr.buffer, ihdr.byteOffset, 13);
  const largeur = d.getUint32(0);
  const hauteur = d.getUint32(4);
  const phys = morceaux.find((m) => m.type === 'pHYs');
  const p = phys && new DataView(phys.donnees.buffer, phys.donnees.byteOffset, 9);
  const brut = zlib.inflateSync(Buffer.concat(morceaux.filter((m) => m.type === 'IDAT').map((m) => m.donnees)));
  const n = largeur * 3;
  const rgb = new Uint8Array(n * hauteur);
  for (let y = 0; y < hauteur; y += 1) {
    const f = brut[y * (n + 1)];
    for (let i = 0; i < n; i += 1) {
      const x = brut[y * (n + 1) + 1 + i];
      const a = i >= 3 ? rgb[y * n + i - 3] : 0;
      const b = y ? rgb[(y - 1) * n + i] : 0;
      const c = i >= 3 && y ? rgb[(y - 1) * n + i - 3] : 0;
      const pp = a + b - c;
      const pa = Math.abs(pp - a); const pb = Math.abs(pp - b); const pc = Math.abs(pp - c);
      rgb[y * n + i] = (x + [0, a, b, (a + b) >> 1, pa <= pb && pa <= pc ? a : pb <= pc ? b : c][f]) & 255;
    }
  }
  return { signature, types: morceaux.map((m) => m.type), crcBons: morceaux.every((m) => m.crcBon), largeur, hauteur,
    bits: ihdr[8], couleur: ihdr[9], ppmX: p && p.getUint32(0), ppmY: p && p.getUint32(4), unite: p && p.getUint8(8), rgb };
}

// TIFF: the directory's tags, then each strip, LZW read as the specification says
function delzw(o, attendus) {
  const sortie = new Uint8Array(attendus);
  let s = 0;
  const prefixe = new Int32Array(4096); const dernier = new Uint8Array(4096); const longueurs = new Int32Array(4096); const premier = new Uint8Array(4096);
  for (let i = 0; i < 256; i += 1) { prefixe[i] = -1; dernier[i] = i; longueurs[i] = 1; premier[i] = i; }
  let suivant = 258; let largeur = 9; let precedent = -1; let bit = 0;
  const lire = (n) => { let x = 0; for (let i = 0; i < n; i += 1, bit += 1) x = (x << 1) | ((o[bit >> 3] >> (7 - (bit & 7))) & 1); return x; };
  const ecrire = (code) => { const n = longueurs[code]; let c = code; for (let i = n - 1; i >= 0; i -= 1) { sortie[s + i] = dernier[c]; c = prefixe[c]; } s += n; };
  for (;;) {
    if (bit + largeur > o.length * 8) throw new Error('pas de code de fin');
    const code = lire(largeur);
    if (code === 256) { suivant = 258; largeur = 9; precedent = -1; continue; }
    if (code === 257) break;
    if (code < suivant && (code < 256 || code >= 258)) {
      ecrire(code);
      if (precedent >= 0) { prefixe[suivant] = precedent; dernier[suivant] = premier[code]; longueurs[suivant] = longueurs[precedent] + 1; premier[suivant] = premier[precedent]; suivant += 1; }
    } else if (code === suivant && precedent >= 0) {
      prefixe[suivant] = precedent; dernier[suivant] = premier[precedent]; longueurs[suivant] = longueurs[precedent] + 1; premier[suivant] = premier[precedent]; suivant += 1;
      ecrire(code);
    } else throw new Error(`code inconnu ${code}`);
    precedent = code;
    if (suivant + 1 === 1 << largeur && largeur < 12) largeur += 1;  // early change
  }
  if (s !== attendus) throw new Error(`${s} octets au lieu de ${attendus}`);
  return sortie;
}
function lireTIFF(o) {
  const v = new DataView(o.buffer, o.byteOffset, o.byteLength);
  const entete = o[0] === 0x49 && o[1] === 0x49 && v.getUint16(2, true) === 42;
  const ifd = v.getUint32(4, true);
  const n = v.getUint16(ifd, true);
  const tags = {};
  const TAILLES = { 2: 1, 3: 2, 4: 4, 5: 8 };
  for (let i = 0; i < n; i += 1) {
    const a = ifd + 2 + 12 * i;
    const tag = v.getUint16(a, true); const type = v.getUint16(a + 2, true); const nombre = v.getUint32(a + 4, true);
    const ou = TAILLES[type] * nombre <= 4 ? a + 8 : v.getUint32(a + 8, true);
    const valeurs = [];
    for (let j = 0; j < nombre; j += 1) {
      if (type === 3) valeurs.push(v.getUint16(ou + 2 * j, true));
      else if (type === 4) valeurs.push(v.getUint32(ou + 4 * j, true));
      else if (type === 5) valeurs.push(v.getUint32(ou + 8 * j, true) / v.getUint32(ou + 8 * j + 4, true));
      else valeurs.push(o[ou + j]);
    }
    tags[tag] = type === 2 ? String.fromCharCode(...valeurs).replace(/\0$/, '') : valeurs;
    if (i && v.getUint16(a - 12, true) >= tag) tags.desordre = true;
  }
  const [largeur] = tags[256]; const [hauteur] = tags[257]; const [parBande] = tags[278];
  const parties = tags[273].map((debut, b) => {
    const donnees = o.subarray(debut, debut + tags[279][b]);
    const lignes = Math.min(parBande, hauteur - b * parBande);
    return tags[259][0] === 5 ? delzw(donnees, lignes * largeur * 3) : donnees;
  });
  const rgb = new Uint8Array(largeur * hauteur * 3);
  let k = 0;
  for (const p of parties) { rgb.set(p, k); k += p.length; }
  return { entete, tags, largeur, hauteur, rgb, ifdSuivant: v.getUint32(ifd + 2 + 12 * n, true), decalagesPairs: tags[273].every((d) => d % 2 === 0) };
}

const { largeur, hauteur } = pixelsImage(85, 60, 300);
const rgba = dessin(largeur, hauteur);
const rgb = rgbDe(rgba);
const png = await ecrirePNG({ largeur, hauteur, rgba, dpi: 300 });
fs.writeFileSync(path.join(dossier, 'essai-300dpi.png'), png);
const lp = lirePNG(png);
verifier('PNG : signature, morceaux dans l’ordre, CRC justes', lp.signature && lp.types[0] === 'IHDR' && lp.types[1] === 'pHYs' && lp.types.at(-1) === 'IEND' && lp.crcBons, lp.types.join(' '));
egal('PNG : 1004 × 709, RGB 8 bits', [lp.largeur, lp.hauteur, lp.bits, lp.couleur], [1004, 709, 8, 2]);
egal('PNG : 300 points par pouce dans pHYs (11 811 par mètre)', [lp.ppmX, lp.ppmY, lp.unite, Math.round(lp.ppmX * 0.0254)], [11811, 11811, 1, 300]);
verifier('PNG : les pixels relus sont ceux écrits', memes(lp.rgb, rgb));
const png600 = lirePNG(await ecrirePNG({ largeur: 3, hauteur: 2, rgba: Uint8Array.from([255, 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255, 1, 2, 3, 255, 4, 5, 6, 255, 7, 8, 9, 255]), dpi: 600 }));
verifier('PNG : 3 × 2 pixels à 600 points par pouce', png600.largeur === 3 && png600.hauteur === 2 && png600.ppmX === pixelsParMetre(600)
  && memes(png600.rgb, Uint8Array.from([255, 0, 0, 0, 255, 0, 0, 0, 255, 1, 2, 3, 4, 5, 6, 7, 8, 9])));

for (const compression of ['lzw', 'aucune']) {
  const tiff = ecrireTIFF({ largeur, hauteur, rgba, dpi: 300, compression });
  fs.writeFileSync(path.join(dossier, `essai-300dpi-${compression}.tif`), tiff);
  const lt = lireTIFF(tiff);
  const t = lt.tags;
  verifier(`TIFF ${compression} : en-tête, une seule image, étiquettes dans l’ordre, données à des adresses paires`, lt.entete && lt.ifdSuivant === 0 && !t.desordre && lt.decalagesPairs);
  egal(`TIFF ${compression} : taille, RVB 8 bits, compression`, [lt.largeur, lt.hauteur, t[258], t[262][0], t[277][0], t[284][0], t[259][0]],
    [1004, 709, [8, 8, 8], 2, 3, 1, compression === 'lzw' ? 5 : 1]);
  egal(`TIFF ${compression} : 300 points par pouce (XResolution, YResolution, ResolutionUnit = pouce)`, [t[282][0], t[283][0], t[296][0]], [300, 300, 2]);
  verifier(`TIFF ${compression} : les pixels relus sont ceux écrits`, memes(lt.rgb, rgb));
  if (compression === 'lzw') verifier(`TIFF LZW : plus petit que sans compression (${tiff.length} octets, ${rgb.length} sans)`, tiff.length < rgb.length / 2);
}
const t600 = lireTIFF(ecrireTIFF({ largeur: 3, hauteur: 2, rgba: new Uint8Array(24).fill(200), dpi: 600, compression: 'lzw' }));
egal('TIFF : 3 × 2 pixels à 600 points par pouce', [t600.largeur, t600.hauteur, t600.tags[282][0], [...t600.rgb]], [3, 2, 600, Array(18).fill(200)]);

// LZW alone: the widths of 9 to 12 bits, a full table, a last code that widens the End of
// information, an empty strip
let graine = 5;
const bruit = Uint8Array.from({ length: 300000 }, () => { graine = (graine * 48271) % 2147483647; return graine & 0x0f; });
verifier('LZW : 300 000 octets de bruit (table remplie et vidée bien des fois) relus tels quels', memes(delzw(lzw(bruit), bruit.length), bruit));
let largeursOk = true;
for (let n = 1; n <= 1300 && largeursOk; n += 1) {
  const d = Uint8Array.from({ length: n }, (_, i) => (i * 7 + (i >> 3)) & 255);
  try { largeursOk = memes(delzw(lzw(d), n), d); } catch { largeursOk = false; }
  if (!largeursOk) console.log(`    (longueur ${n})`);
}
verifier('LZW : chaque longueur de 1 à 1 300 octets (le code de fin à chaque changement de largeur)', largeursOk);
// (256 then 257 on 9 bits: 100000000 100000001, then zeros up to the byte)
egal('LZW : rien, puis Clear et End of information seuls', [...lzw(new Uint8Array(0))], [128, 64, 64]);

if (!garde) fs.rmSync(dossier, { recursive: true, force: true });
else console.log(`Fichiers gardés dans ${dossier}`);
console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
