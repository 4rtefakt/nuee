// Checks interface/qr.js without the application: the QR code of a reproducibility package's link,
// as lean-qr draws it for the deposit's dialog and the package's SVG file (code-qr.svg, qr-code.svg),
// read back by a decoder of its own, jsQR (a devDependency, for the checks only): each module of the
// SVG's path filled as SVG fills it (its outline's vertical edges crossed, even-odd), at four pixels a
// module with its margin, decodes to the exact link; black on white, four modules of margin, level M.
//   node tests/essai_qr.mjs
import jsQR from 'jsqr';
import { generate, correction } from 'lean-qr';
import { toSvgSource } from 'lean-qr/extras/svg';
import { codeQR } from '../interface/qr.js';

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};

// The SVG's pixels: its viewBox (the margin included), its path's subpaths (M x y L x y … Z), each
// module black when a ray from its centre to the right crosses an odd number of their vertical edges.
function pixelsSvg(svg, echelle = 4) {
  const [x0, y0, w, h] = /viewBox="([^"]+)"/.exec(svg)[1].trim().split(/\s+/).map(Number);
  const aretes = [];
  for (const sous of /<path d="([^"]+)"/.exec(svg)[1].split('Z').filter((s) => s.trim())) {
    const points = [...sous.matchAll(/[ML]\s*(-?\d+(?:\.\d+)?)[\s,]+(-?\d+(?:\.\d+)?)/g)].map((m) => [Number(m[1]), Number(m[2])]);
    for (let i = 0; i < points.length; i += 1) {
      const [a, b] = [points[i], points[(i + 1) % points.length]];
      if (a[0] === b[0] && a[1] !== b[1]) aretes.push({ x: a[0], bas: Math.min(a[1], b[1]), haut: Math.max(a[1], b[1]) });
    }
  }
  const noir = (cx, cy) => aretes.filter((e) => e.x > cx && cy > e.bas && cy < e.haut).length % 2 === 1;
  const largeur = w * echelle;
  const data = new Uint8ClampedArray(largeur * h * echelle * 4).fill(255);
  let modules = 0;
  for (let j = 0; j < h; j += 1) {
    for (let i = 0; i < w; i += 1) {
      if (!noir(x0 + i + 0.5, y0 + j + 0.5)) continue;
      modules += 1;
      for (let dy = 0; dy < echelle; dy += 1) {
        for (let dx = 0; dx < echelle; dx += 1) {
          const k = ((j * echelle + dy) * largeur + i * echelle + dx) * 4;
          data[k] = 0; data[k + 1] = 0; data[k + 2] = 0;
        }
      }
    }
  }
  return { data, largeur, hauteur: h * echelle, modules, marge: -x0, cote: w };
}

const qr = { generate, correction, toSvgSource };
const liens = ['https://nuee.io/app/?doi=10.5281/zenodo.24263015', 'https://nuee.io/app/?doi=10.5072/zenodo.4242',
  'https://nuee.io/app/?doi=10.5281/zenodo.1234567890123', 'https://nuee.io/app/?record=24263015'];
console.log('The QR code of a package’s link');
for (const lien of liens) {
  const c = await codeQR(lien, { qr });
  const p = pixelsSvg(c.svg);
  const lu = jsQR(p.data, p.largeur, p.hauteur);
  verifier(`${lien}: its SVG decodes to the link (${c.taille}×${c.taille} modules)`, !!lu && lu.data === lien, lu ? lu.data : 'not decoded');
  verifier('… black on white, four modules of margin, an SVG document', /^<\?xml[^>]*\?><svg[\s>]/.test(c.svg) && /<rect[^>]* fill="#fff"/.test(c.svg)
    && /<path [^>]*fill="#000"/.test(c.svg) && p.marge === 4 && p.cote === c.taille + 8 && !/<script|href=|\son[a-z]+=/i.test(c.svg), c.svg.slice(0, 160));
}
// (level M at least: the same link at level L would take fewer modules, or as many)
const m = generate(liens[0], { minCorrectionLevel: correction.M });
const l = generate(liens[0], { minCorrectionLevel: correction.L, maxCorrectionLevel: correction.L });
verifier('error correction level M at least', (await codeQR(liens[0], { qr })).taille === m.size && m.size >= l.size, `${m.size} / ${l.size}`);

console.log(echecs ? `${echecs} échec(s)` : 'Tout est bon');
process.exit(echecs ? 1 : 0);
