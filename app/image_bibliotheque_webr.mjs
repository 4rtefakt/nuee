// Feasibility: R's packages for the browser version as ONE library image, the format of
// Emscripten's file_packager (library.data: the files end to end; library.js.metadata: each
// file's path, start and end), built here without Emscripten, then mounted in webR (WORKERFS)
// and the engine's packages loaded from it.
//   node image_bibliotheque.mjs <installed library folder> <output folder>
import fs from 'node:fs';
import path from 'node:path';
import zlib from 'node:zlib';
import { pathToFileURL } from 'node:url';

const [bib, sortie] = process.argv.slice(2);
fs.mkdirSync(sortie, { recursive: true });
const SANS = new Set(['DBI', 'blob', 'memoise', 'cachem', 'fastmap']);  // (left from RSQLite)
const fichiers = [];
const parcourir = (d, rel) => {
  for (const e of fs.readdirSync(d, { withFileTypes: true })) {
    if (!rel && (SANS.has(e.name) || e.name.startsWith('.'))) continue;
    const r = `${rel}/${e.name}`;
    if (e.isDirectory()) parcourir(path.join(d, e.name), r); else fichiers.push([path.join(d, e.name), r]);
  }
};
const t0 = Date.now();
parcourir(bib, '');
const morceaux = [];
const meta = { files: [], remote_package_size: 0 };
let debut = 0;
for (const [f, r] of fichiers) {
  const o = fs.readFileSync(f);
  morceaux.push(o);
  meta.files.push({ filename: r, start: debut, end: debut + o.length });
  debut += o.length;
}
meta.remote_package_size = debut;
const data = Buffer.concat(morceaux);
fs.writeFileSync(path.join(sortie, 'library.data'), data);
fs.writeFileSync(path.join(sortie, 'library.js.metadata'), JSON.stringify(meta));
const gz = zlib.gzipSync(data, { level: 9 });
const br = zlib.brotliCompressSync(data, { params: { [zlib.constants.BROTLI_PARAM_QUALITY]: 9 } });
console.log(`image : ${fichiers.length} fichiers, ${(data.length / 1e6).toFixed(1)} Mo (gzip ${(gz.length / 1e6).toFixed(1)} Mo, brotli ${(br.length / 1e6).toFixed(1)} Mo), faite en ${Date.now() - t0} ms`);

// mounted in webR (Node), and the engine's packages loaded from it
const { WebR } = await import(pathToFileURL(path.join(process.env.WEBR_DIST, 'webr.mjs')).href);
const webR = new WebR({ interactive: false });
await webR.init();
const t1 = Date.now();
await webR.FS.mkdir('/nuee-lib');
await webR.FS.mount('WORKERFS', { packages: [{ blob: new Blob([data]), metadata: meta }] }, '/nuee-lib');
const t2 = Date.now();
const sh = await new webR.Shelter();
const r = await sh.captureR(`.libPaths(c("/nuee-lib", .libPaths()))
  p <- c("jsonlite", "nlme", "mgcv", "MASS", "rpart", "cluster", "foreign", "openxlsx", "readxl", "haven", "zip")
  t <- system.time(ok <- vapply(p, requireNamespace, TRUE, quietly = TRUE))[["elapsed"]]
  m <- mgcv::gam(y ~ s(x), data = data.frame(x = 1:50, y = sin(1:50 / 5) + (1:50 %% 3) / 10))
  cat(sprintf("%s : %s\n", p, ok), sprintf("chargés en %.1f s ; un GAM : edf %.2f\n", t, sum(m$edf)), sep = "")`, { captureStreams: true });
for (const o of r.output) console.log(o.data);
console.log(`monté en ${t2 - t1} ms`);
webR.close();
process.exit(0);
