// Builds the browser version of Nuée: a static site where R runs in the page (webR), served by
// any web server that sends Cross-Origin-Opener-Policy: same-origin and
// Cross-Origin-Embedder-Policy: require-corp (webR then shares memory with its worker);
// servir_navigateur.js serves it so on this computer.
//   node app/construire_navigateur.js [<output folder>] [<webR folder>]
// The output folder is dist/navigateur unless given; webR is the npm package's (devDependencies:
// its browser build, webr.js, as webr.mjs; R.js, R.wasm, BLAS and LAPACK, and the base R library
// in vfs/), or a folder with the same files, as on webr.r-wasm.org/v<version>/.
// The site: the interface (its page saying window.NUEE_MOTEUR = 'webr', and a security policy
// that lets webR compile R, fetch its packages from repo.r-wasm.org and start its worker), the
// engine's files (moteur/, without its tests, and exemple/) with their list
// (moteur-fichiers.json), webR in webr/, and _headers (the two headers, for hosts that read it).
const fs = require('fs');
const path = require('path');

const depot = path.join(__dirname, '..');
const [sortie, webr] = [process.argv[2] || path.join(depot, 'dist', 'navigateur'), process.argv[3] || path.join(__dirname, 'node_modules', 'webr', 'dist')]
  .map((x) => path.resolve(x));
const interfaceDir = path.join(__dirname, 'interface');
if (!fs.existsSync(path.join(interfaceDir, 'vendor', 'plotly.min.js'))) { console.error('interface/vendor manque : node copier-vendor.js'); process.exit(1); }
// webR's browser build: npm's webr.js (its webr.mjs is for Node: it imports Node's modules), or
// the webr.mjs of a folder like webr.r-wasm.org's
const moduleWebr = ['webr.js', 'webr.mjs'].find((f) => fs.existsSync(path.join(webr, f)) && !/^import \{ createRequire \}/.test(fs.readFileSync(path.join(webr, f), 'utf8').slice(0, 100)));
for (const f of ['webr-worker.js', 'R.js', 'R.wasm']) {
  if (!fs.existsSync(path.join(webr, f))) { console.error(`webR : ${f} manque dans ${webr} (npm install dans app)`); process.exit(1); }
}
if (!moduleWebr) { console.error(`webR : pas de module pour le navigateur dans ${webr} (webr.js de npm, ou le webr.mjs de webr.r-wasm.org)`); process.exit(1); }

fs.rmSync(sortie, { recursive: true, force: true });
fs.mkdirSync(sortie, { recursive: true });
fs.cpSync(interfaceDir, sortie, { recursive: true });

// the page: the engine in the page, webR's needs in the security policy
const page = path.join(sortie, 'index.html');
let html = fs.readFileSync(page, 'utf8');
const avant = html;
html = html.replace("script-src 'self' 'unsafe-eval';", "script-src 'self' 'unsafe-eval' 'wasm-unsafe-eval';")
  .replace('connect-src \'self\' data: blob:', 'connect-src \'self\' data: blob: https://repo.r-wasm.org')
  .replace('<script type="module" src="app.js"></script>', '<script src="navigateur.js"></script>\n  <script type="module" src="app.js"></script>');
if (html === avant || !html.includes('navigateur.js') || !html.includes('https://repo.r-wasm.org') || !html.includes('wasm-unsafe-eval') || !/worker-src 'self' blob:/.test(html)) {
  console.error('index.html : la politique de sécurité ou le script de l’application a changé, à revoir ici');
  process.exit(1);
}
fs.writeFileSync(page, html);
fs.writeFileSync(path.join(sortie, 'navigateur.js'), "// Nuée in the browser: R runs in the page (moteur_webr.js)\nwindow.NUEE_MOTEUR = 'webr';\n");
fs.writeFileSync(path.join(sortie, '_headers'), '/*\n  Cross-Origin-Opener-Policy: same-origin\n  Cross-Origin-Embedder-Policy: require-corp\n');

// the engine's files and the example, listed for moteur_webr.js
const liste = [];
const copier = (rel) => {
  const de = path.join(depot, rel);
  for (const e of fs.readdirSync(de, { withFileTypes: true })) {
    const r = path.posix.join(rel, e.name);
    if (e.isDirectory()) { if (r !== 'moteur/tests') copier(r); continue; }
    fs.mkdirSync(path.join(sortie, rel), { recursive: true });
    fs.copyFileSync(path.join(de, e.name), path.join(sortie, r));
    liste.push(r);
  }
};
copier('moteur');
copier('exemple');
fs.writeFileSync(path.join(sortie, 'moteur-fichiers.json'), JSON.stringify(liste.sort()));

// webR: its module, worker, R and the base R library (not npm's REPL, tests and types)
const dest = path.join(sortie, 'webr');
fs.mkdirSync(dest, { recursive: true });
fs.copyFileSync(path.join(webr, moduleWebr), path.join(dest, 'webr.mjs'));
for (const f of fs.readdirSync(webr)) {
  if (['webr.js', 'webr.mjs', 'webr.cjs', 'repl', 'tests', 'webR', 'esbuild.d.ts'].includes(f) || f.endsWith('.map')) continue;
  fs.cpSync(path.join(webr, f), path.join(dest, f), { recursive: true });
}
const taille = (d) => fs.readdirSync(d, { withFileTypes: true }).reduce((s, e) => s + (e.isDirectory() ? taille(path.join(d, e.name)) : fs.statSync(path.join(d, e.name)).size), 0);
let version = '';
try { version = ` ${JSON.parse(fs.readFileSync(path.join(webr, '..', 'package.json'), 'utf8')).version}`; } catch { /* a folder of its own */ }
console.log(`Site : ${sortie} (${(taille(sortie) / 1e6).toFixed(1)} Mo, ${liste.length} fichiers du moteur ; webR${version})`);
