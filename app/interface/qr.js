// The QR code of a link: a reproducibility package's, in its deposit's dialog (paquet.js) and as an
// SVG file in the package (moteur/R/paquet.R: code-qr.svg, qr-code.svg). Drawn by lean-qr (MIT
// licence; copier-vendor.js: vendor/qr.js), imported when first needed: error correction level M at
// least (a printed code still reads with 15% of it lost), four modules of white around it, as the
// standard asks, black on white.
//   codeQR(texte)           → { taille (modules), svg (an SVG document), png(echelle) → Uint8Array }
//   codeQR(texte, { qr })   the same with the library given (the checks, in Node: tests/essai_qr.mjs)
let biblio = null;

export async function codeQR(texte, { qr = null } = {}) {
  const q = qr || await (biblio ||= import('./vendor/qr.js'));
  const code = q.generate(String(texte), { minCorrectionLevel: q.correction.M });
  return {
    taille: code.size,
    svg: q.toSvgSource(code, { on: '#000', off: '#fff', pad: 4, scale: 8, xmlDeclaration: true }),
    // (a PNG of `echelle` pixels per module, drawn on a canvas: in the page only)
    png: (echelle = 16) => {
      const url = code.toDataURL({ type: 'image/png', scale: echelle, pad: 4, on: [0, 0, 0, 255], off: [255, 255, 255, 255] });
      return Uint8Array.from(atob(url.slice(url.indexOf(',') + 1)), (c) => c.charCodeAt(0));
    },
  };
}
