// Serves the browser version's site (construire_navigateur.js) on this computer, with the two
// headers webR needs to share memory with its worker (Cross-Origin-Opener-Policy: same-origin,
// Cross-Origin-Embedder-Policy: require-corp): to try it, and for its checks
// (tests/essai_navigateur.mjs). Only 127.0.0.1 listens.
//   node app/servir_navigateur.js [<site folder>] [<port>]      (dist/navigateur, 8080)
const fs = require('fs');
const http = require('http');
const path = require('path');

const TYPES = {
  '.html': 'text/html; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.mjs': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8', '.json': 'application/json', '.svg': 'image/svg+xml', '.wasm': 'application/wasm',
  '.png': 'image/png', '.csv': 'text/csv; charset=utf-8',
};
// (webR's .data.gz and .so go as they are: webR reads them itself, a Content-Encoding would undo them)

function servir(dossier, port = 8080) {
  const racine = path.resolve(dossier);
  const serveur = http.createServer((req, res) => {
    let chemin;
    try { chemin = decodeURIComponent(new URL(req.url, 'http://127.0.0.1').pathname); } catch { res.writeHead(400).end(); return; }
    let f = path.resolve(racine, `.${chemin}`);
    if (f !== racine && !f.startsWith(racine + path.sep)) { res.writeHead(403).end(); return; }
    if (fs.existsSync(f) && fs.statSync(f).isDirectory()) f = path.join(f, 'index.html');
    if (!fs.existsSync(f)) { res.writeHead(404).end(); return; }
    res.writeHead(200, {
      'Content-Type': TYPES[path.extname(f).toLowerCase()] || 'application/octet-stream',
      'Content-Length': fs.statSync(f).size,
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Embedder-Policy': 'require-corp',
      'Cache-Control': 'no-cache',
    });
    if (req.method === 'HEAD') { res.end(); return; }
    fs.createReadStream(f).pipe(res);
  });
  return new Promise((ok, ko) => {
    serveur.once('error', ko);
    serveur.listen(port, '127.0.0.1', () => ok(serveur));
  });
}

module.exports = { servir };

if (require.main === module) {
  const dossier = process.argv[2] || path.join(__dirname, '..', 'dist', 'navigateur');
  const port = Number(process.argv[3]) || 8080;
  servir(dossier, port).then((s) => console.log(`Nuée dans le navigateur : http://127.0.0.1:${s.address().port}/ (${dossier})`));
}
