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
