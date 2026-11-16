// Serves GitHub release files at http://127.0.0.1:<port>/<owner>/<repo>/<tag>/<file>, fetched
// through GitHub's API with the job's token. Electron's and electron-builder's downloaders fetch
// release files anonymously, which GitHub throttles at times (HTTP 504 for hours on 2026-11-09);
// ELECTRON_MIRROR and ELECTRON_BUILDER_BINARIES_MIRROR send them here instead. Both still check
// what they get against their own checksums.
//   GITHUB_TOKEN=... node .github/miroir-github.js <port> [command...]
// With a command, it runs it once listening (ELECTRON_MIRROR set, the token left out) and ends with
// it, so that nothing has to outlive a step (on Windows, one started a step earlier was gone).
const http = require('http');
const { spawn } = require('child_process');

const [port, ...commande] = [Number(process.argv[2]), ...process.argv.slice(3)];
const auth = { Authorization: `Bearer ${process.env.GITHUB_TOKEN}`, 'User-Agent': 'nuee-ci', 'X-GitHub-Api-Version': '2022-11-28' };
const versions = new Map();

async function fichier(owner, repo, tag, nom) {
  const cle = `${owner}/${repo}/${tag}`;
  if (!versions.has(cle)) {
    const r = await fetch(`https://api.github.com/repos/${owner}/${repo}/releases/tags/${encodeURIComponent(tag)}`, { headers: auth });
    if (!r.ok) throw new Error(`${cle}: HTTP ${r.status}`);
    versions.set(cle, (await r.json()).assets);
  }
  const a = versions.get(cle).find((x) => x.name === nom);
  if (!a) return null;
  // the API answers with a redirection to the file, which is fetched without the token
  const r = await fetch(a.url, { headers: { ...auth, Accept: 'application/octet-stream' }, redirect: 'manual' });
  const suite = r.headers.get('location');
  const f = suite ? await fetch(suite) : r;
  if (!f.ok) throw new Error(`${nom}: HTTP ${f.status}`);
  return { taille: a.size, corps: f.body };
}

const serveur = http.createServer(async (req, res) => {
  try {
    const [owner, repo, tag, nom] = decodeURIComponent(new URL(req.url, 'http://h').pathname).split('/').filter(Boolean);
    const f = await fichier(owner, repo, tag, nom);
    if (!f) { res.writeHead(404).end(); return; }
    res.writeHead(200, { 'Content-Type': 'application/octet-stream', 'Content-Length': f.taille });
    if (req.method === 'HEAD') { await f.corps.cancel(); res.end(); return; }
    for await (const morceau of f.corps) res.write(morceau);
    res.end();
  } catch (e) {
    console.error(req.url, e.message);
    if (!res.headersSent) res.writeHead(502);
    res.end();
  }
}).listen(port, '127.0.0.1', () => {
  console.log(`http://127.0.0.1:${port}/`);
  if (!commande.length) return;
  const env = { ...process.env, ELECTRON_MIRROR: `http://127.0.0.1:${port}/electron/electron/` };
  delete env.GITHUB_TOKEN;
  // (it ends by itself once the server is closed: process.exit() with fetch's connections still open
  // stops Node on Windows with a libuv assertion, exit code 127)
  const finir = (code) => { process.exitCode = code; serveur.close(); serveur.closeAllConnections(); };
  spawn(commande[0], commande.slice(1), { stdio: 'inherit', env })
    .on('error', (e) => { console.error(e.message); finir(1); })
    .on('exit', (code) => finir(code === null ? 1 : code));
});
