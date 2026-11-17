// Numbers as the interface shows them to the reader: with a decimal comma in French, as the
// engine's texts write them ("D 44,41 /km² (CV 9,7 %)"), with a point in English. Plotly's axes
// and hover formats get the same from tracer (panneaux.js: separators).
// What is read back keeps the point: values sent to the engine, <input type=number> values, CSS
// sizes and plot coordinates, the files the interface writes, keys and ids. A table's copy has
// the interface's decimal mark, at full precision (panneaux.js: tableau).
import { langueActuelle } from './i18n.js';

// A number's text written with a point (toFixed, toPrecision, String): its decimal point made
// a comma in French. Only a point between two digits changes, so a text already French is left
// as it is.
export const decimal = (texte) => (langueActuelle() === 'fr' ? String(texte).replace(/(\d)\.(\d)/g, '$1,$2') : String(texte));

// A number with d decimals ('' when missing).
export const fmt = (v, d = 3) => (v === null || v === undefined || Number.isNaN(v) ? '' : decimal(Number(v).toFixed(d)));

// Between two numbers side by side (coordinates, two degrees of freedom): ", ", or " ; " in
// French, where the comma is the decimal mark.
export const entreNombres = () => (langueActuelle() === 'fr' ? ' ; ' : ', ');

// The decimals a column of values is shown with (the Data view): those its values have, up to
// `max` (an altitude 3.2: 1; a GPS longitude 4.6153271: 6), the most any of them has, decided on a
// sample of `echantillon` values spread over the column, quick on a table of 100,000 rows. A whole
// number has none, and a computation's tail (0.30000000000000004) isn't one. What isn't a finite
// number (text, a missing value) is left out.
export function decimalesValeurs(valeurs, { max = 6, echantillon = 2000 } = {}) {
  const pas = Math.max(1, Math.floor(valeurs.length / echantillon));
  let d = 0;
  for (let i = 0; i < valeurs.length && d < max; i += pas) d = Math.max(d, decimalesDe(valeurs[i], max));
  return d;
}
function decimalesDe(v, max) {
  if (typeof v !== 'number' || !Number.isFinite(v) || Number.isInteger(v)) return 0;
  const ecart = 1e-12 * Math.max(1, Math.abs(v));
  for (let d = 1; d < max; d += 1) if (Math.abs(v - Math.round(v * 10 ** d) / 10 ** d) <= ecart) return d;
  return max;
}
