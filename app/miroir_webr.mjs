// The browser version's own copy of webR's repository, for a site that serves R's packages itself
// (nuee.io/app/: vitrine/construire.mjs). Into a site built by construire_navigateur.js: the webR
// binaries of the packages the page installs (interface/paquets_webr.js: PAQUETS_WEBR, with what
// they depend on as webr::install follows it, Depends and Imports, less what webR's R has), for
// the site's R, from repo.r-wasm.org, in <site>/repo/bin/emscripten/contrib/<R>/, with their own
// index (PACKAGES, PACKAGES.gz and PACKAGES.rds, as R's write_PACKAGES writes them; R reads the
// .rds first). Then the site's navigateur.js says so (window.NUEE_MIROIR_WEBR): the page installs
// them from its own site, and from repo.r-wasm.org only what this copy lacks, such as an R-code
// model's own package (interface/moteur_webr.js; moteur/R/navigateur.R).
// Each binary is downloaded once into node_modules/.cache/nuee-webr/miroir/<R> and checked against
// repo.r-wasm.org's index, read again at each build (the copy kept from the last one when
// repo.r-wasm.org can't be reached): the version its name says, and its MD5.
//   node app/miroir_webr.mjs [<site folder>]        (dist/navigateur unless given)
import { createHash } from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import zlib from 'node:zlib';
import { fileURLToPath } from 'node:url';

const ICI = path.dirname(fileURLToPath(import.meta.url));
// (the interface's module, read as a module: Node would read a .js of this folder as CommonJS first)
const { PAQUETS_WEBR } = await import(`data:text/javascript,${encodeURIComponent(fs.readFileSync(path.join(ICI, 'interface', 'paquets_webr.js'), 'utf8'))}`);
const DEPOT_WEBR = 'https://repo.r-wasm.org';

const site = path.resolve(process.argv[2] || path.join(ICI, '..', 'dist', 'navigateur'));
const arreter = (message) => { console.error(`miroir_webr.mjs: ${message}`); process.exit(1); };
const navigateur = path.join(site, 'navigateur.js');
const bibliotheque = path.join(site, 'webr', 'vfs', 'usr', 'lib', 'R', 'library');
if (!fs.existsSync(navigateur) || !fs.existsSync(bibliotheque)) arreter(`${site} isn't a site built by construire_navigateur.js`);

// The site's R, as its webR has it (R's own translations package bears R's version): "4.6"
const versionR = /^Version:\s*(\d+\.\d+)\./m.exec(fs.readFileSync(path.join(bibliotheque, 'translations', 'DESCRIPTION'), 'utf8'))?.[1];
if (!versionR) arreter(`no R version in ${path.join(bibliotheque, 'translations', 'DESCRIPTION')}`);
// the packages webR's R has (a folder, or a file system image: <package>.data.gz)
const deWebr = new Set(fs.readdirSync(bibliotheque).map((f) => f.replace(/\.(data\.gz|js\.metadata)$/, '')));
const contrib = `bin/emscripten/contrib/${versionR}`;
const cache = path.join(ICI, 'node_modules', '.cache', 'nuee-webr', 'miroir', versionR);
fs.mkdirSync(cache, { recursive: true });

// repo.r-wasm.org's index: its records as they are written (for this copy's PACKAGES) and their fields,
// as R's read.dcf reads them (a line that starts with a space continues the field above it)
let index;
try {
  const r = await fetch(`${DEPOT_WEBR}/${contrib}/PACKAGES.gz`);
  if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
  index = zlib.gunzipSync(Buffer.from(await r.arrayBuffer())).toString('utf8');
  fs.writeFileSync(path.join(cache, 'PACKAGES'), index);
} catch (e) {
  if (!fs.existsSync(path.join(cache, 'PACKAGES'))) arreter(`can't read ${DEPOT_WEBR}/${contrib}/PACKAGES.gz (${e.message})`);
  console.warn(`miroir_webr.mjs: ${DEPOT_WEBR} out of reach (${e.message}): its index as read last time`);
  index = fs.readFileSync(path.join(cache, 'PACKAGES'), 'utf8');
}
const enregistrements = new Map();
for (const brut of index.replace(/\r\n/g, '\n').split(/\n[ \t]*\n/).map((b) => b.trim()).filter(Boolean)) {
  const champs = {};
  let cle = null;
  for (const ligne of brut.split('\n')) {
    if (/^[ \t]/.test(ligne)) { if (cle) champs[cle] += `\n${ligne.trim()}`; continue; }
    const m = /^([^:\s]+):\s*(.*)$/.exec(ligne);
    if (m) { cle = m[1]; champs[cle] = m[2].trim(); }
  }
  if (champs.Package) enregistrements.set(champs.Package, { brut, champs });
}

// PAQUETS_WEBR and what they depend on, as webr::install takes it
const noms = (x) => (x || '').split(',').map((n) => n.replace(/\(.*?\)/s, '').trim()).filter((n) => n && n !== 'R');
const paquets = [];
for (const file = [...PAQUETS_WEBR]; file.length;) {
  const p = file.shift();
  if (deWebr.has(p) || paquets.includes(p)) continue;
  const e = enregistrements.get(p);
  if (!e) arreter(`${p} isn't in ${DEPOT_WEBR}'s index for R ${versionR}`);
  paquets.push(p);
  file.push(...noms(e.champs.Depends), ...noms(e.champs.Imports));
}
paquets.sort((a, b) => a.localeCompare(b, 'en'));

// each binary from the cache, or downloaded into it, under the name webr::install asks for:
// <package>_<version>.tgz, with the index's MD5
const md5 = (octets) => createHash('md5').update(octets).digest('hex');
async function binaire(p) {
  const { Version, MD5sum, File } = enregistrements.get(p).champs;
  const nom = `${p}_${Version}.tgz`;
  if (File && File !== nom) arreter(`${p}: the index names its file ${File}, not ${nom} (version ${Version})`);
  const f = path.join(cache, nom);
  if (fs.existsSync(f) && (!MD5sum || md5(fs.readFileSync(f)) === MD5sum)) return { nom, f };
  const url = `${DEPOT_WEBR}/${contrib}/${nom}`;
  const r = await fetch(url).catch((e) => arreter(`${url}: ${e.message}`));
  if (!r.ok) arreter(`${url}: ${r.status} ${r.statusText}`);
  const octets = Buffer.from(await r.arrayBuffer());
  if (MD5sum && md5(octets) !== MD5sum) arreter(`${nom}: MD5 ${md5(octets)}, the index says ${MD5sum}`);
  fs.writeFileSync(`${f}.part`, octets);
  fs.renameSync(`${f}.part`, f);
  return { nom, f, telecharge: true };
}
const binaires = [];
const enCours = [...paquets];
await Promise.all(Array.from({ length: 6 }, async () => {
  for (let p = enCours.shift(); p; p = enCours.shift()) binaires.push(await binaire(p));
}));

// The index as R's write_PACKAGES saves it in PACKAGES.rds: a character matrix, a row per package
// and a column per field (NA where a package has none), in R's serialization (version 2, XDR), gzipped
function rds(lignes, colonnes) {
  const morceaux = [Buffer.from('X\n')];
  const entier = (n) => { const b = Buffer.alloc(4); b.writeInt32BE(n); morceaux.push(b); };
  const texte = (s) => {  // (a CHARSXP: NA, ASCII or UTF-8)
    if (s === undefined) { entier(9); entier(-1); return; }
    const o = Buffer.from(s, 'utf8');
    entier(9 | ((/^[\x00-\x7f]*$/.test(s) ? 64 : 8) << 12));
    entier(o.length);
    morceaux.push(o);
  };
  const textes = (x, attributs) => { entier(16 | (attributs ? 1 << 9 : 0)); entier(x.length); x.forEach(texte); };
  const attribut = (nom) => { entier(2 | (1 << 10)); entier(1); texte(nom); };  // (a pairlist's node, its tag a symbol)
  entier(2); entier(0x040600); entier(0x020300);  // (version 2, written by R 4.6.0, read by R 2.3.0 and later)
  textes(colonnes.flatMap((c) => lignes.map((l) => l[c])), true);
  attribut('dim'); entier(13); entier(2); entier(lignes.length); entier(colonnes.length);
  attribut('dimnames'); entier(19); entier(2); entier(254); textes(colonnes, false);
  entier(254);
  return zlib.gzipSync(Buffer.concat(morceaux));
}

// the copy, in the site
const dest = path.join(site, 'repo', ...contrib.split('/'));
fs.rmSync(path.join(site, 'repo'), { recursive: true, force: true });
fs.mkdirSync(dest, { recursive: true });
for (const { nom, f } of binaires) fs.copyFileSync(f, path.join(dest, nom));
const texteIndex = `${paquets.map((p) => enregistrements.get(p).brut).join('\n\n')}\n`;
fs.writeFileSync(path.join(dest, 'PACKAGES'), texteIndex);
fs.writeFileSync(path.join(dest, 'PACKAGES.gz'), zlib.gzipSync(texteIndex));
const champs = paquets.map((p) => enregistrements.get(p).champs);
fs.writeFileSync(path.join(dest, 'PACKAGES.rds'), rds(champs, [...new Set(champs.flatMap((c) => Object.keys(c)))]));
if (!fs.readFileSync(navigateur, 'utf8').includes('NUEE_MIROIR_WEBR')) {
  fs.appendFileSync(navigateur, "// R's packages from this site first (miroir_webr.mjs): its copy of webR's repository\nwindow.NUEE_MIROIR_WEBR = 'repo/';\n");
}

const tailles = binaires.map(({ nom, f }) => [nom, fs.statSync(f).size]).sort((a, b) => b[1] - a[1]);
const mio = (n) => `${(n / 2 ** 20).toFixed(1)} MiB`;
console.log(`webR's repository copied: ${paquets.length} packages for R ${versionR} (${mio(tailles.reduce((s, [, n]) => s + n, 0))}, the largest ${tailles[0][0]} at ${mio(tailles[0][1])}; `
  + `${binaires.filter((b) => b.telecharge).length} downloaded) in ${dest}`);
