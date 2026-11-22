// A zip archive with its files stored as they are (no compression), enough for a .docx: the
// model report (rapport.js) and the documents (rendu_document.js) write theirs with it. And a
// zip archive read (lireZip): a reproducibility package's, which the link reads in the page before
// R starts (relancer.js: lirePaquetZip; lecture.js).
const TABLE_CRC = Array.from({ length: 256 }, (_, n) => {
  let c = n;
  for (let k = 0; k < 8; k += 1) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
  return c >>> 0;
});
function crc32(octets) {
  let c = 0xffffffff;
  for (const o of octets) c = TABLE_CRC[(c ^ o) & 0xff] ^ (c >>> 8);
  return (c ^ 0xffffffff) >>> 0;
}

// `fichiers`: [[name, Uint8Array]...] -> the archive's bytes. `maintenant`: the files' date (now by
// default; a fixed one gives the same bytes at each build: the plugins of the documentation site,
// docs/site/construire.js).
export function zip(fichiers, maintenant = new Date()) {
  const morceaux = [];
  const centre = [];
  let decalage = 0;
  const heure = (maintenant.getHours() << 11) | (maintenant.getMinutes() << 5) | (maintenant.getSeconds() >> 1);
  const jour = ((maintenant.getFullYear() - 1980) << 9) | ((maintenant.getMonth() + 1) << 5) | maintenant.getDate();
  for (const [nom, octets] of fichiers) {
    const n = new TextEncoder().encode(nom);
    const crc = crc32(octets);
    const local = new DataView(new ArrayBuffer(30));
    [[0, 0x04034b50, 4], [4, 20, 2], [6, 0x0800, 2], [8, 0, 2], [10, heure, 2], [12, jour, 2], [14, crc, 4],
      [18, octets.length, 4], [22, octets.length, 4], [26, n.length, 2], [28, 0, 2]]
      .forEach(([k, v, l]) => (l === 4 ? local.setUint32(k, v, true) : local.setUint16(k, v, true)));
    const c = new DataView(new ArrayBuffer(46));
    [[0, 0x02014b50, 4], [4, 20, 2], [6, 20, 2], [8, 0x0800, 2], [10, 0, 2], [12, heure, 2], [14, jour, 2], [16, crc, 4],
      [20, octets.length, 4], [24, octets.length, 4], [28, n.length, 2], [30, 0, 2], [32, 0, 2], [34, 0, 2], [36, 0, 2], [38, 0, 4], [42, decalage, 4]]
      .forEach(([k, v, l]) => (l === 4 ? c.setUint32(k, v, true) : c.setUint16(k, v, true)));
    morceaux.push(new Uint8Array(local.buffer), n, octets);
    centre.push(new Uint8Array(c.buffer), n);
    decalage += 30 + n.length + octets.length;
  }
  const tailleCentre = centre.reduce((s, x) => s + x.length, 0);
  const fin = new DataView(new ArrayBuffer(22));
  [[0, 0x06054b50, 4], [4, 0, 2], [6, 0, 2], [8, fichiers.length, 2], [10, fichiers.length, 2], [12, tailleCentre, 4], [16, decalage, 4], [20, 0, 2]]
    .forEach(([k, v, l]) => (l === 4 ? fin.setUint32(k, v, true) : fin.setUint16(k, v, true)));
  const tout = [...morceaux, ...centre, new Uint8Array(fin.buffer)];
  const sortie = new Uint8Array(tout.reduce((s, x) => s + x.length, 0));
  let k = 0;
  for (const x of tout) { sortie.set(x, k); k += x.length; }
  return sortie;
}

// An archive's files, from its central directory (found from its end, after which a comment may
// follow): `fichiers`, their names, and `lire(nom)`, a file's bytes, stored as they are or deflated
// (as R's zip package writes them: moteur/R/paquet.R), inflated by the browser
// (DecompressionStream). null when it can't be read here: not a zip archive, or one beyond what this
// reads (ZIP64, past 4 GB or 65,535 files; another compression; encrypted files).
export function lireZip(octets) {
  const v = new DataView(octets.buffer, octets.byteOffset, octets.byteLength);
  let fin = -1;
  for (let i = octets.length - 22; i >= Math.max(0, octets.length - 22 - 0xffff); i -= 1) {
    if (v.getUint32(i, true) === 0x06054b50) { fin = i; break; }
  }
  if (fin < 0) return null;
  const n = v.getUint16(fin + 10, true);
  let p = v.getUint32(fin + 16, true);
  if (n === 0xffff || p === 0xffffffff) return null;
  const entrees = new Map();
  const texte = new TextDecoder('utf-8');
  for (let k = 0; k < n; k += 1) {
    if (p + 46 > octets.length || v.getUint32(p, true) !== 0x02014b50) return null;
    const drapeaux = v.getUint16(p + 8, true);
    const e = { methode: v.getUint16(p + 10, true), compresse: v.getUint32(p + 20, true), taille: v.getUint32(p + 24, true), decalage: v.getUint32(p + 42, true) };
    const ln = v.getUint16(p + 28, true);
    if ((drapeaux & 1) || ![0, 8].includes(e.methode) || [e.compresse, e.taille, e.decalage].includes(0xffffffff)) return null;
    entrees.set(texte.decode(octets.subarray(p + 46, p + 46 + ln)), e);
    p += 46 + ln + v.getUint16(p + 30, true) + v.getUint16(p + 32, true);
  }
  return {
    fichiers: [...entrees.keys()].filter((x) => !x.endsWith('/')),
    async lire(nom) {
      const e = entrees.get(nom);
      if (!e || e.decalage + 30 > octets.length || v.getUint32(e.decalage, true) !== 0x04034b50) return null;
      const debut = e.decalage + 30 + v.getUint16(e.decalage + 26, true) + v.getUint16(e.decalage + 28, true);
      const brut = octets.subarray(debut, debut + e.compresse);
      if (e.methode === 0) return brut;
      const flux = new Blob([brut]).stream().pipeThrough(new DecompressionStream('deflate-raw'));
      return new Uint8Array(await new Response(flux).arrayBuffer());
    },
  };
}
