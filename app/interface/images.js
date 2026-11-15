// Pictures for print, written here without a library (figures.js; checked by
// app/tests/essai_figures.mjs): PNG and TIFF in RGB, 8 bits per channel, from a canvas's RGBA
// pixels, with their resolution in dots per inch where journals' checks read it: the PNG's pHYs
// chunk, the TIFF's XResolution, YResolution and ResolutionUnit (inch). The PNG is compressed by
// Deflate (CompressionStream: the browser's own zlib); the TIFF is uncompressed or LZW, the
// compression journals name, written here.

const TABLE_CRC = (() => {
  const t = new Uint32Array(256);
  for (let n = 0; n < 256; n += 1) {
    let c = n;
    for (let k = 0; k < 8; k += 1) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
    t[n] = c >>> 0;
  }
  return t;
})();

// CRC-32 of PNG's chunks (ISO 3309, as zlib computes it).
export function crc32(o, debut = 0, fin = o.length) {
  let c = 0xffffffff;
  for (let i = debut; i < fin; i += 1) c = TABLE_CRC[(c ^ o[i]) & 0xff] ^ (c >>> 8);
  return (c ^ 0xffffffff) >>> 0;
}

// zlib's format (RFC 1950: Deflate with its header and Adler-32), which PNG's IDAT holds.
export async function deflate(octets) {
  const flux = new Blob([octets]).stream().pipeThrough(new CompressionStream('deflate'));
  return new Uint8Array(await new Response(flux).arrayBuffer());
}

// Dots per inch as PNG's pixels per metre.
export const pixelsParMetre = (dpi) => Math.round(dpi / 0.0254);

// Each row of the picture in RGB behind its filter byte, the filter of PNG that gives the
// smallest sum of its bytes read as signed numbers (libpng's choice), row by row.
function lignesPNG(rgba, largeur, hauteur) {
  const n = largeur * 3;
  const sortie = new Uint8Array(hauteur * (n + 1));
  let prec = new Uint8Array(n);
  let cour = new Uint8Array(n);
  const abs = (v) => (v < 128 ? v : 256 - v);
  for (let y = 0; y < hauteur; y += 1) {
    for (let x = 0, s = y * largeur * 4, d = 0; x < largeur; x += 1, s += 4) {
      cour[d] = rgba[s]; cour[d + 1] = rgba[s + 1]; cour[d + 2] = rgba[s + 2];
      d += 3;
    }
    let s0 = 0; let s1 = 0; let s2 = 0; let s3 = 0; let s4 = 0;
    for (let i = 0; i < n; i += 1) {
      const x = cour[i];
      const a = i >= 3 ? cour[i - 3] : 0;
      const b = prec[i];
      const c = i >= 3 ? prec[i - 3] : 0;
      const p = a + b - c;
      const pa = Math.abs(p - a); const pb = Math.abs(p - b); const pc = Math.abs(p - c);
      const pr = pa <= pb && pa <= pc ? a : pb <= pc ? b : c;
      s0 += abs(x);
      s1 += abs((x - a) & 255);
      s2 += abs((x - b) & 255);
      s3 += abs((x - ((a + b) >> 1)) & 255);
      s4 += abs((x - pr) & 255);
    }
    const sommes = [s0, s1, s2, s3, s4];
    const filtre = sommes.indexOf(Math.min(...sommes));
    let o = y * (n + 1);
    sortie[o] = filtre;
    o += 1;
    for (let i = 0; i < n; i += 1, o += 1) {
      const x = cour[i];
      const a = i >= 3 ? cour[i - 3] : 0;
      const b = prec[i];
      if (filtre === 0) sortie[o] = x;
      else if (filtre === 1) sortie[o] = x - a;
      else if (filtre === 2) sortie[o] = x - b;
      else if (filtre === 3) sortie[o] = x - ((a + b) >> 1);
      else {
        const c = i >= 3 ? prec[i - 3] : 0;
        const p = a + b - c;
        const pa = Math.abs(p - a); const pb = Math.abs(p - b); const pc = Math.abs(p - c);
        sortie[o] = x - (pa <= pb && pa <= pc ? a : pb <= pc ? b : c);
      }
    }
    [prec, cour] = [cour, prec];
  }
  return sortie;
}

function morceau(type, donnees) {
  const o = new Uint8Array(12 + donnees.length);
  const v = new DataView(o.buffer);
  v.setUint32(0, donnees.length);
  for (let i = 0; i < 4; i += 1) o[4 + i] = type.charCodeAt(i);
  o.set(donnees, 8);
  v.setUint32(8 + donnees.length, crc32(o, 4, 8 + donnees.length));
  return o;
}

const assembler = (parties) => {
  const o = new Uint8Array(parties.reduce((n, p) => n + p.length, 0));
  let k = 0;
  for (const p of parties) { o.set(p, k); k += p.length; }
  return o;
};

// A PNG of `largeur` × `hauteur` pixels (rgba: 4 bytes per pixel, row by row, as a canvas gives
// them; the alpha left out, the picture being drawn on white), at `dpi` dots per inch.
export async function ecrirePNG({ largeur, hauteur, rgba, dpi }) {
  const ihdr = new Uint8Array(13);
  const v = new DataView(ihdr.buffer);
  v.setUint32(0, largeur);
  v.setUint32(4, hauteur);
  ihdr.set([8, 2, 0, 0, 0], 8);  // 8 bits, RGB, Deflate, PNG's filters, not interlaced
  const phys = new Uint8Array(9);
  const w = new DataView(phys.buffer);
  w.setUint32(0, pixelsParMetre(dpi));
  w.setUint32(4, pixelsParMetre(dpi));
  phys[8] = 1;  // per metre
  const z = await deflate(lignesPNG(rgba, largeur, hauteur));
  const idat = [];
  for (let k = 0; k < z.length; k += 1 << 20) idat.push(morceau('IDAT', z.subarray(k, Math.min(z.length, k + (1 << 20)))));
  return assembler([Uint8Array.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]), morceau('IHDR', ihdr), morceau('pHYs', phys), ...idat,
    morceau('IEND', new Uint8Array(0))]);
}

// ------------------------------------------------------------ TIFF

// (prefix code × 256 + byte) -> the code of that string, 0 for none: the LZW table, emptied by
// the entries it holds (`cles`), not the whole of it
const TABLE_LZW = new Uint16Array(4096 * 256);

// TIFF's LZW (TIFF 6.0, section 13): codes of 9 to 12 bits written most significant bit first,
// Clear (256) at the start and whenever the table is full (4094 entries, as libtiff), End of
// information (257) at the end; each code's width grows one code early, as TIFF readers expect
// ("early change"), the End of information's too, after the entry the reader adds for the last
// code.
export function lzw(donnees) {
  const sortie = new Uint8Array(Math.ceil(donnees.length * 1.5) + 16);
  let o = 0;
  let tampon = 0;
  let nBits = 0;
  const ecrire = (code, largeur) => {
    tampon = (tampon << largeur) | code;
    nBits += largeur;
    while (nBits >= 8) {
      nBits -= 8;
      sortie[o] = (tampon >>> nBits) & 0xff;
      o += 1;
    }
    tampon &= (1 << nBits) - 1;
  };
  const cles = new Int32Array(4096);
  let nCles = 0;
  const vider = () => { for (let i = 0; i < nCles; i += 1) TABLE_LZW[cles[i]] = 0; nCles = 0; };
  let suivant = 258;
  let largeur = 9;
  ecrire(256, 9);
  let apresClear = true;
  if (donnees.length) {
    let w = donnees[0];
    for (let i = 1; i < donnees.length; i += 1) {
      const c = donnees[i];
      const cle = (w << 8) | c;
      const code = TABLE_LZW[cle];
      if (code) { w = code; continue; }
      ecrire(w, largeur);
      apresClear = false;
      TABLE_LZW[cle] = suivant;
      cles[nCles] = cle;
      nCles += 1;
      suivant += 1;
      if (suivant === 4094) {
        ecrire(256, largeur);
        vider();
        suivant = 258;
        largeur = 9;
        apresClear = true;
      } else if (suivant === 1 << largeur) largeur += 1;
      w = c;
    }
    ecrire(w, largeur);
    // (the reader adds an entry for this last code unless it is the first after a Clear)
    if (!apresClear) {
      suivant += 1;
      if (suivant === 1 << largeur && largeur < 12) largeur += 1;
    }
  }
  ecrire(257, largeur);
  if (nBits > 0) { sortie[o] = (tampon << (8 - nBits)) & 0xff; o += 1; }
  vider();
  return sortie.slice(0, o);
}

// A TIFF (baseline, little-endian, one plane of RGB) of the rgba pixels, at `dpi` dots per inch,
// in strips of 64 KiB at most before compression ('lzw' or 'aucune').
export function ecrireTIFF({ largeur, hauteur, rgba, dpi, compression = 'lzw', logiciel = 'Nuee' }) {
  const ligne = largeur * 3;
  const lignesParBande = Math.max(1, Math.floor(65536 / ligne));
  const bandes = [];
  for (let y0 = 0; y0 < hauteur; y0 += lignesParBande) {
    const y1 = Math.min(hauteur, y0 + lignesParBande);
    const brut = new Uint8Array((y1 - y0) * ligne);
    for (let y = y0, d = 0; y < y1; y += 1) {
      for (let x = 0, s = y * largeur * 4; x < largeur; x += 1, s += 4, d += 3) {
        brut[d] = rgba[s]; brut[d + 1] = rgba[s + 1]; brut[d + 2] = rgba[s + 2];
      }
    }
    bandes.push(compression === 'lzw' ? lzw(brut) : brut);
  }
  const texte = `${String(logiciel).replace(/[^\x20-\x7e]/g, '?')}\0`;
  // the entries, by tag: [tag, type (3 SHORT, 4 LONG, 5 RATIONAL, 2 ASCII), values]
  const n = bandes.length;
  const entrees = [
    [256, 4, [largeur]], [257, 4, [hauteur]], [258, 3, [8, 8, 8]], [259, 3, [compression === 'lzw' ? 5 : 1]], [262, 3, [2]],
    [273, 4, Array(n).fill(0)], [277, 3, [3]], [278, 4, [lignesParBande]], [279, 4, bandes.map((b) => b.length)],
    [282, 5, [[Math.round(dpi), 1]]], [283, 5, [[Math.round(dpi), 1]]], [284, 3, [1]], [296, 3, [2]], [305, 2, texte],
  ];
  const taille = { 2: 1, 3: 2, 4: 4, 5: 8 };
  const nombreDe = (e) => e[2].length;  // (values, or the text's characters with its final 0)
  const debutIFD = 8;
  const finIFD = debutIFD + 2 + 12 * entrees.length + 4;
  // the values longer than 4 bytes after the directory, each at an even offset
  let k = finIFD;
  const decalages = entrees.map((e) => {
    const octets = taille[e[1]] * nombreDe(e);
    if (octets <= 4) return null;
    const d = k;
    k += octets + (octets % 2);
    return d;
  });
  const debutBandes = k;
  const total = debutBandes + bandes.reduce((s, b) => s + b.length + (b.length % 2), 0);
  const o = new Uint8Array(total);
  const v = new DataView(o.buffer);
  o.set([0x49, 0x49, 42, 0], 0);
  v.setUint32(4, debutIFD, true);
  // where each strip starts
  const departs = [];
  let p = debutBandes;
  for (const b of bandes) { departs.push(p); o.set(b, p); p += b.length + (b.length % 2); }
  entrees[5][2] = departs;
  v.setUint16(debutIFD, entrees.length, true);
  const ecrireValeurs = (e, a) => {
    if (e[1] === 2) { for (let i = 0; i < e[2].length; i += 1) o[a + i] = e[2].charCodeAt(i); return; }
    e[2].forEach((x, i) => {
      if (e[1] === 3) v.setUint16(a + 2 * i, x, true);
      else if (e[1] === 4) v.setUint32(a + 4 * i, x, true);
      else { v.setUint32(a + 8 * i, x[0], true); v.setUint32(a + 8 * i + 4, x[1], true); }
    });
  };
  entrees.forEach((e, i) => {
    const a = debutIFD + 2 + 12 * i;
    v.setUint16(a, e[0], true);
    v.setUint16(a + 2, e[1], true);
    v.setUint32(a + 4, nombreDe(e), true);
    ecrireValeurs(e, decalages[i] === null ? a + 8 : decalages[i]);
    if (decalages[i] !== null) v.setUint32(a + 8, decalages[i], true);
  });
  v.setUint32(finIFD - 4, 0, true);  // no other image
  return o;
}
