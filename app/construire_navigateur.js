// Builds the browser version of Nuée: a static site where R runs in the page (webR), served by
// any web server that sends Cross-Origin-Opener-Policy: same-origin and
// Cross-Origin-Embedder-Policy: require-corp (webR then shares memory with its worker).
//   node construire_navigateur.js <output folder> <webR folder>
// <webR folder>: webR's browser build (R.wasm, R.js, webr.mjs, webr-worker.js, vfs/...), as on
// webr.r-wasm.org/v<version>/: npm's dist with webr.mjs, webr-worker.js and R.js from there (npm's
// start with a header for Node that browsers refuse).
// The site: the interface (its page saying window.NUEE_MOTEUR = 'webr', and a security policy
// that lets webR fetch its packages and start its worker), the engine's files (moteur/, without
// its tests, and exemple/) with their list (moteur-fichiers.json), and webR in webr/.
const fs = require('fs');
const path = require('path');

const [sortie, webr] = process.argv.slice(2).map((x) => x && path.resolve(x));
if (!sortie || !webr) { console.error('node construire_navigateur.js <dossier de sortie> <dossier de webR>'); process.exit(1); }
const depot = path.join(__dirname, '..');
const interfaceDir = path.join(__dirname, 'interface');
if (!fs.existsSync(path.join(interfaceDir, 'vendor', 'plotly.min.js'))) { console.error('interface/vendor manque : node copier-vendor.js'); process.exit(1); }
for (const f of ['webr.mjs', 'webr-worker.js', 'R.js', 'R.wasm']) {
  if (!fs.existsSync(path.join(webr, f))) { console.error(`webR : ${f} manque dans ${webr}`); process.exit(1); }
}
if (/^import \{ createRequire \}/.test(fs.readFileSync(path.join(webr, 'webr.mjs'), 'utf8').slice(0, 100))) {
  console.error('webR : webr.mjs est celui de npm (pour Node) ; prendre celui de webr.r-wasm.org');
  process.exit(1);
}

fs.rmSync(sortie, { recursive: true, force: true });
fs.mkdirSync(sortie, { recursive: true });
fs.cpSync(interfaceDir, sortie, { recursive: true });

// the page: the engine in the page, webR's needs in the security policy
const page = path.join(sortie, 'index.html');
let html = fs.readFileSync(page, 'utf8');
const avant = html;
html = html.replace("script-src 'self' 'unsafe-eval';", "script-src 'self' 'unsafe-eval' 'wasm-unsafe-eval'; worker-src 'self' blob:;")
  .replace("connect-src 'self' data: blob:;", "connect-src 'self' https://repo.r-wasm.org data: blob:;")
  .replace('<script type="module" src="app.js"></script>', '<script src="navigateur.js"></script>\n  <script type="module" src="app.js"></script>');
if (html === avant || !html.includes('navigateur.js') || !html.includes('repo.r-wasm.org') || !html.includes('wasm-unsafe-eval')) {
  console.error('index.html : la politique de sécurité ou le script de l’application a changé, à revoir ici');
  process.exit(1);
}
fs.writeFileSync(page, html);
fs.writeFileSync(path.join(sortie, 'navigateur.js'), "// Nuée in the browser: R runs in the page (moteur_webr.js)\nwindow.NUEE_MOTEUR = 'webr';\n");

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

fs.cpSync(webr, path.join(sortie, 'webr'), { recursive: true });
const taille = (d) => fs.readdirSync(d, { withFileTypes: true }).reduce((s, e) => s + (e.isDirectory() ? taille(path.join(d, e.name)) : fs.statSync(path.join(d, e.name)).size), 0);
console.log(`Site : ${sortie} (${(taille(sortie) / 1e6).toFixed(1)} Mo, ${liste.length} fichiers du moteur)`);
