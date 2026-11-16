// Checks interface/etiquettes.js (Plots › Format plot › Spread the labels), without the application:
// labels placed off each other and off the points, inside the plot, at their own place when it is
// free, a line to those away from their point; a Plotly figure's labels made annotations, placed
// again from those at another size; what isn't spread (nothing to, too much, other axes).
//   node tests/essai_etiquettes.mjs
import { placerEtiquettes, repousserFigure, etiquettesDe, sansTextes, aEtiquettes, etiquettesRepoussees, ETIQUETTE } from '../interface/etiquettes.js';

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);

let graine = 11;
const hasard = () => { graine = (graine * 16807) % 2147483647; return graine / 2147483647; };
const boite = (c, e) => ({ x0: c.x - e.largeur / 2, y0: c.y - e.hauteur / 2, x1: c.x + e.largeur / 2, y1: c.y + e.hauteur / 2 });
const recouvrent = (a, b) => Math.min(a.x1, b.x1) - Math.max(a.x0, b.x0) > 0.01 && Math.min(a.y1, b.y1) - Math.max(a.y0, b.y0) > 0.01;
const chevauchements = (etiquettes, places) => {
  let n = 0;
  for (let i = 0; i < places.length; i += 1) for (let j = i + 1; j < places.length; j += 1) if (recouvrent(boite(places[i], etiquettes[i]), boite(places[j], etiquettes[j]))) n += 1;
  return n;
};
// a point covered by a label other than its own
const pointsCouverts = (etiquettes, places, points) => points.filter((p, k) => places.some((c, i) => i !== p.propre
  && recouvrent(boite(c, etiquettes[i]), { x0: p.x - p.rayon, y0: p.y - p.rayon, x1: p.x + p.rayon, y1: p.y + p.rayon }))).length;
const dedans = (etiquettes, places, cadre) => places.every((c, i) => { const b = boite(c, etiquettes[i]); return b.x0 >= -0.01 && b.y0 >= -0.01 && b.x1 <= cadre.largeur + 0.01 && b.y1 <= cadre.hauteur + 0.01; });

console.log('Des étiquettes qui ne se chevauchent pas : placerEtiquettes');
{
  // two points far apart: each label above its point, as Plotly writes it, no line
  const etiquettes = [{ x: 100, y: 100, largeur: 40, hauteur: 12, rayon: 4, position: 'top center' }, { x: 300, y: 200, largeur: 50, hauteur: 12, rayon: 4, position: 'bottom center' }];
  const places = placerEtiquettes(etiquettes, [{ x: 100, y: 100, rayon: 4, propre: 0 }, { x: 300, y: 200, rayon: 4, propre: 1 }], { largeur: 500, hauteur: 400 });
  verifier('chacune à sa place quand elle est libre (au-dessus, au-dessous)', places[0].x === 100 && places[0].y < 100 - 4 - 6 && places[1].x === 300 && places[1].y > 200 + 4 + 6
    && !places[0].ligne && !places[1].ligne, JSON.stringify(places));
  egal('juste au-dessus du point', Math.round(100 - places[0].y), 4 + 2 + 6);
}
{
  // weight and diameter, colour_b and storage: two pairs of loadings almost on each other
  const etiquettes = [[200, 150, 'weight'], [203, 151, 'diameter'], [320, 90, 'colour_b'], [322, 92, 'storage']]
    .map(([x, y, t]) => ({ x, y, largeur: 7 * t.length, hauteur: 12, rayon: 4, position: 'top center' }));
  const points = etiquettes.map((e, i) => ({ x: e.x, y: e.y, rayon: 4, propre: i }));
  const places = placerEtiquettes(etiquettes, points, { largeur: 500, hauteur: 300 });
  egal('deux paires de loadings presque confondues : aucun chevauchement', chevauchements(etiquettes, places), 0);
  egal('ni sur un point', pointsCouverts(etiquettes, places, points), 0);
  verifier('la première garde sa place', places[0].x === 200 && !places[0].ligne, JSON.stringify(places[0]));
}
{
  // a cluster of 40 points in a corner of a plot with room: no overlap, every label inside, those
  // pushed away with a line
  const etiquettes = Array.from({ length: 40 }, () => ({ x: 380 + hasard() * 60, y: 40 + hasard() * 40, largeur: 30 + hasard() * 40, hauteur: 12, rayon: 3.5, position: 'top center' }));
  const points = etiquettes.map((e, i) => ({ x: e.x, y: e.y, rayon: 3.5, propre: i }));
  const cadre = { largeur: 640, hauteur: 420 };
  const places = placerEtiquettes(etiquettes, points, cadre);
  egal('40 étiquettes serrées : aucun chevauchement', chevauchements(etiquettes, places), 0);
  egal('aucune sur un point', pointsCouverts(etiquettes, places, points), 0);
  verifier('toutes dans le graphique', dedans(etiquettes, places, cadre));
  verifier('un trait pour celles écartées de leur point', places.some((p) => p.ligne) && places.every((p, i) => p.ligne || Math.hypot(p.x - etiquettes[i].x, p.y - etiquettes[i].y) < 60));
  egal('le même résultat d’une fois à l’autre', placerEtiquettes(etiquettes, points, cadre), places);
}
{
  // a label by the plot's edge goes inside
  const e = [{ x: 250, y: 3, largeur: 60, hauteur: 12, rayon: 4, position: 'top center' }, { x: 498, y: 150, largeur: 60, hauteur: 12, rayon: 4, position: 'middle right' }];
  const places = placerEtiquettes(e, [], { largeur: 500, hauteur: 300 });
  verifier('au bord du graphique : dedans (dessous, à gauche)', dedans(e, places, { largeur: 500, hauteur: 300 }) && places[0].y > 3 && places[1].x < 498, JSON.stringify(places));
}
{
  // no room at all: its own place, overlapping as Plotly would write it
  const e = Array.from({ length: 30 }, () => ({ x: 20, y: 10, largeur: 30, hauteur: 12, rayon: 3, position: 'top center' }));
  const places = placerEtiquettes(e, [], { largeur: 40, hauteur: 20 });
  verifier('pas de place du tout : chacune quelque part, sans erreur', places.length === 30 && places.every((p) => Number.isFinite(p.x) && Number.isFinite(p.y)));
}
{
  // 300 labels on 800 × 600: quick enough to be done at each drawing
  const e = Array.from({ length: 300 }, () => ({ x: 50 + hasard() * 700, y: 30 + hasard() * 540, largeur: 40, hauteur: 12, rayon: 3.5, position: 'top center' }));
  const debut = performance.now();
  placerEtiquettes(e, e.map((x, i) => ({ x: x.x, y: x.y, rayon: 3.5, propre: i })), { largeur: 800, hauteur: 600 });
  const ms = performance.now() - debut;
  verifier('300 étiquettes en moins de 400 ms', ms < 400, `${Math.round(ms)} ms`);
}

console.log('Une figure de Plotly : repousserFigure');
const mesurer = (texte, police) => ({ largeur: 0.6 * (police.size || 12) * String(texte).length + 2, hauteur: 1.25 * (police.size || 12) });
const figure = () => ({
  data: [
    { type: 'scatter', mode: 'markers+text', x: [0.1, 0.11, 0.5, -0.4], y: [0.2, 0.205, -0.3, 0.1], text: ['weight', 'diameter', 'storage', ''],
      textposition: 'top center', textfont: { size: 10 }, marker: { size: 7 }, hovertemplate: '%{text}' },
    { type: 'scatter', mode: 'lines', x: [-1, 1], y: [0, 0] },
  ],
  layout: { font: { family: 'Arial', size: 12, color: '#222' }, annotations: [{ text: 'une note', xref: 'paper', yref: 'paper', x: 0, y: 1 }] },
});
const geometrie = { largeur: 600, hauteur: 400, x: { range: [-0.6, 0.6], type: 'linear' }, y: { range: [-0.4, 0.4], type: 'linear' } };
{
  const f = figure();
  const r = repousserFigure(f.data, f.layout, geometrie, mesurer);
  const etiquettes = r.layout.annotations.filter((a) => String(a.name || '').startsWith(ETIQUETTE));
  egal('ses noms deviennent des annotations (un nom vide : aucune)', etiquettes.map((a) => a.text), ['weight', 'diameter', 'storage']);
  egal('la trace sans ses textes, gardés pour le survol', [r.data[0].mode, r.data[0].text.length, r.data[1].mode], ['markers', 4, 'lines']);
  egal('ses autres annotations gardées', r.layout.annotations[0].text, 'une note');
  egal('la figure donnée inchangée', [f.data[0].mode, f.layout.annotations.length], ['markers+text', 1]);
  verifier('chacune à son point (x, y), dans la police de sa trace et sa couleur par défaut', etiquettes.every((a, i) => a.x === f.data[0].x[i] && a.y === f.data[0].y[i]
    && a.font.size === 10 && a.font.family === 'Arial' && a.font.color === '#222' && a.xref === 'x' && a.yref === 'y'));
  // their boxes, in px
  const centre = (a) => { const x = ((a.x + 0.6) / 1.2) * 600; const y = ((0.4 - a.y) / 0.8) * 400; return a.showarrow ? { x: x + a.ax, y: y + a.ay } : { x: x + a.xshift, y: y - a.yshift }; };
  const boites = etiquettes.map((a) => ({ ...centre(a), ...mesurer(a.text, a.font) }));
  egal('weight et diameter, presque confondus : séparés', chevauchements(boites, boites), 0);
  verifier('storage, seul, à sa place sans trait', !etiquettes[2].showarrow && etiquettes[2].xshift === 0 && etiquettes[2].yshift > 0, JSON.stringify(etiquettes[2]));
  egal('etiquettesRepoussees', [etiquettesRepoussees(r.layout), etiquettesRepoussees(f.layout)], [true, false]);
  // placed again from its annotations, at the same size: the same places
  const r2 = repousserFigure(r.data, r.layout, geometrie, mesurer);
  egal('replacées depuis leurs annotations, à la même taille : les mêmes places', r2.layout.annotations, r.layout.annotations);
  // at a smaller size (a figure's cell), smaller text: placed again, still apart
  const petite = { ...geometrie, largeur: 240, hauteur: 160 };
  const r3 = repousserFigure(r.data, { ...r.layout, annotations: r.layout.annotations.map((a) => (a.name ? { ...a, font: { ...a.font, size: 7 } } : a)) }, petite, mesurer);
  const centre3 = (a) => { const x = ((a.x + 0.6) / 1.2) * 240; const y = ((0.4 - a.y) / 0.8) * 160; return a.showarrow ? { x: x + a.ax, y: y + a.ay } : { x: x + a.xshift, y: y - a.yshift }; };
  const b3 = r3.layout.annotations.filter((a) => a.name).map((a) => ({ ...centre3(a), ...mesurer(a.text, a.font) }));
  egal('à une autre taille, en plus petit : replacées, toujours séparées', [b3.length, chevauchements(b3, b3)], [3, 0]);
  egal('etiquettesDe lit les traces et les annotations', etiquettesDe(r.data, r.layout).map((e) => [e.texte, e.position]), [['weight', 'top center'], ['diameter', 'top center'], ['storage', 'top center']]);
}
{
  const f = figure();
  egal('rien à écarter : null', repousserFigure([f.data[1]], {}, geometrie, mesurer), null);
  const beaucoup = { ...f.data[0], x: Array.from({ length: 600 }, (_, i) => i), y: Array.from({ length: 600 }, (_, i) => i), text: Array.from({ length: 600 }, (_, i) => `o${i}`) };
  egal('plus de 500 étiquettes : null (écrites comme Plotly les écrit)', repousserFigure([beaucoup], {}, geometrie, mesurer), null);
  egal('un axe de catégories : null', repousserFigure(f.data, f.layout, { ...geometrie, x: { range: [0, 3], type: 'category' } }, mesurer), null);
  egal('sans géométrie : null', repousserFigure(f.data, f.layout, null, mesurer), null);
  const log = repousserFigure([{ type: 'scatter', mode: 'markers+text', x: [1, 10, 100], y: [1, 2, 3], text: ['a', 'b', 'c'], textposition: 'top center' }], {},
    { largeur: 300, hauteur: 200, x: { range: [0, 2], type: 'log' }, y: { range: [0, 4], type: 'linear' } }, mesurer);
  verifier('un axe logarithmique', !!log && log.layout.annotations.length === 3);
  egal('aEtiquettes : des points et leurs noms seulement, sur les axes x et y', [aEtiquettes(f.data[0]), aEtiquettes(f.data[1]), aEtiquettes({ ...f.data[0], mode: 'text' }),
    aEtiquettes({ ...f.data[0], type: 'scatter3d' }), aEtiquettes({ ...f.data[0], xaxis: 'x2' }), aEtiquettes({ ...f.data[0], visible: 'legendonly' })], [true, false, false, false, false, false]);
  egal('sansTextes', sansTextes(f.data).map((tr) => tr.mode), ['markers', 'lines']);
}

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
