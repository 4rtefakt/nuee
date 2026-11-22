// Checks nuee.io's relay (src/relais.js) through the Worker's own fetch handler (src/worker.js),
// served on this computer by Node, or as wrangler dev serves it (--wrangler: workerd itself),
// against stand-ins on 127.0.0.1 that receive what it sends on (RELAIS_ESSAI): nothing reaches
// github.com or gitlab.com.
// - A git server (the system git's http-backend behind Node's http; every repository private, as
//   GitHub answers: 401 without a token, Basic authentication, a cookie set) reached by
//   isomorphic-git (the application's, app/node_modules) through the relay as a page of another
//   origin uses it: its Origin, the preflights a browser makes, what it may read of the answers,
//   the redirects followed. A project pushed, cloned, changed, pushed and fetched on github.com
//   (owner/name) and on gitlab.com (a subgroup), with a 30 MB file; git's protocol 2; GitLab's
//   address without .git redirected through the relay; the bodies streamed both ways (each side
//   waits for the other to have the first part before sending the rest).
// - The device flow of GitHub and of GitLab: the forms sent on, the answers given back as they are
//   (GitHub's errors in a 200, GitLab's in a 400), a GitLab token renewed.
// - What passes: Accept, Authorization, Content-Type and Git-Protocol, under the relay's
//   User-Agent; never the cookies, addresses, Origin, Referer and Cloudflare's headers sent to it.
//   Back: never Set-Cookie nor WWW-Authenticate; Cache-Control: no-store, Vary: Origin, the origin
//   in Access-Control-Allow-Origin, the preflights' methods and headers.
// - Refused, and nothing sent on: other hosts (the stand-in's own address among them), other
//   paths, methods and queries, paths that climb out; other origins, and requests without one from
//   another site; a redirect elsewhere; the limiter's 429 (a stand-in for it).
// - RELAIS_ESSAI read only when the relay is reached on this computer, and set by no
//   configuration; wrangler.jsonc keeps no log of each request; the relay writes nothing.
// - In Chrome or Edge (playwright-core, from app/node_modules), when there is one: a page of
//   another origin and one of the relay's own make isomorphic-git's requests and the device flow's,
//   a redirect followed, a 401 without its WWW-Authenticate; a page of a third origin refused.
//   node vitrine/tests/essai_relais.mjs
//   node vitrine/tests/essai_relais.mjs --wrangler[=<wrangler.js>] [--port=8796]
// It needs git on the PATH and the application's npm packages (npm install, in app/).

import http from 'node:http';
import { spawn, execFile, execFileSync } from 'node:child_process';
import { createRequire } from 'node:module';
import { createHash, randomBytes } from 'node:crypto';
import { Readable } from 'node:stream';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import worker from '../src/worker.js';

const ICI = path.dirname(fileURLToPath(import.meta.url));
const VITRINE = path.resolve(ICI, '..');
const DEPOT = path.resolve(VITRINE, '..');
const depuisApp = createRequire(path.join(DEPOT, 'app', 'package.json'));
const git = depuisApp('isomorphic-git');

const options = Object.fromEntries(process.argv.slice(2).map((a) => {
  const m = /^--([\w-]+)(?:=(.*))?$/.exec(a);
  return m ? [m[1], m[2] === undefined ? true : m[2]] : [a, true];
}));

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${typeof detail === 'string' ? detail : JSON.stringify(detail)}`}`);
  if (!ok) echecs += 1;
};
const pause = (ms) => new Promise((ok) => setTimeout(ok, ms));
const empreinte = (b) => createHash('sha256').update(b).digest('hex');
const portLibre = () => new Promise((ok) => {
  const s = http.createServer();
  s.listen(0, '127.0.0.1', () => { const p = s.address().port; s.close(() => ok(p)); });
});
// a promise, and what resolves it
const signal = () => { let ok; const p = new Promise((r) => { ok = r; }); p.ok = ok; return p; };

const racine = fs.mkdtempSync(path.join(os.tmpdir(), 'essai-relais-'));
const AGENT = 'git/nuee.io-relais';
const JETON = `ghp_essai${randomBytes(12).toString('hex')}`;  // the stand-in's git token
const BASIC = `Basic ${Buffer.from(`x-access-token:${JETON}`).toString('base64')}`;
const JETONS = { github: `gho_essai${randomBytes(12).toString('hex')}`, gitlab: `glo_essai${randomBytes(12).toString('hex')}`,
  rafraichir: `glr_essai${randomBytes(12).toString('hex')}`, renouvele: `glo_essai${randomBytes(12).toString('hex')}`,
  appareil: `appareil${randomBytes(12).toString('hex')}` };
// What a browser sends beside the page's own headers, and Cloudflare in front of the Worker: none
// of it may reach GitHub or GitLab (found by its name, or by LEURRE and IP in its value: Node's
// fetch, the relay's in Node, adds an Accept-Language and a Sec-Fetch-Mode of its own)
const LEURRE = 'leurre';
const IP = '203.0.113.7';
const LEURRES = { Cookie: `session=${LEURRE}`, Referer: `http://127.0.0.1:1/${LEURRE}`, 'User-Agent': `Mozilla/5.0 (${LEURRE})`,
  'Accept-Language': `fr-${LEURRE}`, 'X-Essai': LEURRE, 'Sec-Fetch-Site': 'cross-site', 'Sec-Fetch-Dest': `${LEURRE}`,
  'X-Forwarded-For': IP, 'X-Forwarded-Proto': `https-${LEURRE}`, 'X-Real-IP': IP, 'CF-Connecting-IP': IP, 'CF-IPCountry': LEURRE, 'CF-Ray': `${LEURRE}-ray`,
  'CF-Visitor': `{"scheme":"${LEURRE}"}` };
const INTERDITS = ['cookie', 'origin', 'referer', 'x-essai', 'sec-fetch-site', 'sec-fetch-dest', 'x-forwarded-for', 'x-forwarded-proto', 'x-real-ip',
  'cf-connecting-ip', 'cf-ipcountry', 'cf-ray', 'cf-visitor'];

// JSON with comments (wrangler.jsonc): its comments left out
function lireJsonc(fichier) {
  const t = fs.readFileSync(fichier, 'utf8');
  let s = '';
  for (let i = 0; i < t.length;) {
    if (t[i] === '"') {
      let j = i + 1;
      while (t[j] !== '"') j += t[j] === '\\' ? 2 : 1;
      s += t.slice(i, j + 1);
      i = j + 1;
    } else if (t.startsWith('//', i)) {
      while (i < t.length && t[i] !== '\n') i += 1;
    } else if (t.startsWith('/*', i)) {
      i = t.indexOf('*/', i + 2) + 2;
    } else {
      s += t[i];
      i += 1;
    }
  }
  return JSON.parse(s.replace(/,(\s*[}\]])/g, '$1'));
}

// ---------------------------------------------------------------- git

const configVide = path.join(racine, 'gitconfig');
fs.writeFileSync(configVide, '');
const envGit = { ...process.env, GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: configVide, GIT_TERMINAL_PROMPT: '0' };
const gitSys = (args, cwd = racine) => execFileSync('git', args, { cwd, env: envGit, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
const DEPOTS = path.join(racine, 'depots');
// a bare repository of the stand-ins, at <host>/<path>
function depotNu(chemin) {
  const d = path.join(DEPOTS, ...chemin.split('/'));
  fs.mkdirSync(d, { recursive: true });
  gitSys(['init', '-q', '--bare', d]);
  gitSys(['symbolic-ref', 'HEAD', 'refs/heads/main'], d);
  gitSys(['config', 'http.receivepack', 'true'], d);
  return d;
}

// ---------------------------------------------------------------- the stand-ins

// One server for both hosts: the relay sends it /<host>/<path> (RELAIS_ESSAI). It answers as they
// do, with a cookie and headers of its own that the relay must not give back, and records what it
// received: { hote, methode, chemin, entetes, corps (a form) }.
const recues = [];
const attentes = {};  // the streaming checks' signals
const FLUX = { premier: randomBytes(256 * 1024), second: randomBytes(256 * 1024) };
const sondages = { 'github.com': 0, 'gitlab.com': 0 };
const SAUTS = {  // github.com's repositories that redirect, and where
  'ailleurs/depot.git/info/refs': [302, 'https://evil.example/depot.git/info/refs?service=git-upload-pack'],
  'clair/depot.git/info/refs': [302, 'http://github.com/clair/depot.git/info/refs?service=git-upload-pack'],
  'connexion/depot.git/info/refs': [302, 'https://github.com/login'],
  'poste/depot.git/git-upload-pack': [307, 'https://github.com/poste/autre.git/git-upload-pack'],
  'poste301/depot.git/git-upload-pack': [301, 'https://github.com/poste301/autre.git/git-upload-pack'],
};

const banc = http.createServer((req, res) => {
  const u = new URL(req.url, 'http://banc');
  const [, hote = '', chemin = ''] = /^\/([^/]+)\/(.*)$/.exec(u.pathname) || [];
  const r = { hote, methode: req.method, chemin: `${chemin}${u.search}`, entetes: { ...req.headers } };
  recues.push(r);
  const propres = { 'Set-Cookie': `_gh_sess=${LEURRE}-session; Path=/; HttpOnly`, 'X-GitHub-Request-Id': `${LEURRE}-id`, 'Strict-Transport-Security': 'max-age=31536000' };
  const ecrire = (statut, entetes, corps) => { res.writeHead(statut, { ...propres, ...entetes }); res.end(corps); };

  // the device flow (GitHub's errors in a 200, GitLab's in a 400; JSON when asked for, else a form)
  if (req.method === 'POST' && !u.search && ['github.com/login/device/code', 'github.com/login/oauth/access_token',
    'gitlab.com/oauth/authorize_device', 'gitlab.com/oauth/token'].includes(`${hote}/${chemin}`)) {
    const morceaux = [];
    req.on('data', (d) => morceaux.push(d));
    req.on('end', () => {
      r.corps = Buffer.concat(morceaux).toString('utf8');
      const champs = Object.fromEntries(new URLSearchParams(r.corps));
      const json = /application\/json/.test(req.headers.accept || '');
      const repondre = (statut, o) => ecrire(statut, { 'Content-Type': json ? 'application/json; charset=utf-8' : 'application/x-www-form-urlencoded' },
        json ? JSON.stringify(o) : new URLSearchParams(o).toString());
      const attendre = { error: 'authorization_pending', error_description: 'The authorization request is still pending.' };
      if (chemin === 'login/device/code') return repondre(200, { device_code: JETONS.appareil, user_code: 'WDJB-MJHT', verification_uri: 'https://github.com/login/device', expires_in: 899, interval: 5 });
      if (chemin === 'login/oauth/access_token') {
        sondages['github.com'] += 1;
        return repondre(200, sondages['github.com'] === 1 ? attendre : { access_token: JETONS.github, token_type: 'bearer', scope: 'repo' });
      }
      if (chemin === 'oauth/authorize_device') {
        return repondre(200, { device_code: JETONS.appareil, user_code: 'GLAB1234', verification_uri: 'https://gitlab.com/oauth/device',
          verification_uri_complete: 'https://gitlab.com/oauth/device?user_code=GLAB1234', expires_in: 300, interval: 5 });
      }
      if (champs.grant_type === 'refresh_token') {
        return repondre(200, { access_token: JETONS.renouvele, token_type: 'Bearer', expires_in: 7200, refresh_token: `${JETONS.rafraichir}2`, created_at: 1 });
      }
      sondages['gitlab.com'] += 1;
      return sondages['gitlab.com'] === 1 ? repondre(400, attendre)
        : repondre(200, { access_token: JETONS.gitlab, token_type: 'Bearer', expires_in: 7200, refresh_token: JETONS.rafraichir, scope: 'api read_user write_repository', created_at: 1 });
    });
    return;
  }

  // redirects: GitHub's test repositories, GitLab's address without .git
  const page = '<html><body>You are being <a href="/">redirected</a>.</body></html>';
  if (hote === 'github.com' && SAUTS[chemin]) {
    req.resume();
    return ecrire(SAUTS[chemin][0], { Location: SAUTS[chemin][1], 'Content-Type': 'text/html; charset=utf-8' }, page);
  }
  const g = /^(.+?)(\/info\/refs|\/git-upload-pack|\/git-receive-pack)$/.exec(chemin);
  if (hote === 'gitlab.com' && g && !g[1].endsWith('.git')) {
    req.resume();
    return ecrire(301, { Location: `https://gitlab.com/${g[1]}.git${g[2]}${u.search}`, 'Content-Type': 'text/html; charset=utf-8' }, page);
  }

  // streaming: the first part back, the rest once it was read; a body counted as it comes
  if (hote === 'github.com' && chemin === 'flux/lent.git/info/refs') {
    res.writeHead(200, { ...propres, 'Content-Type': 'application/x-git-upload-pack-advertisement' });
    res.write(FLUX.premier);
    Promise.race([attentes.lu, pause(15000)]).then(() => res.end(FLUX.second));
    return;
  }
  if (hote === 'github.com' && chemin === 'flux/lent.git/git-receive-pack') {
    let n = 0;
    req.on('data', (d) => { n += d.length; if (n >= FLUX.premier.length && attentes.recu) attentes.recu.ok(); });
    req.on('end', () => ecrire(200, { 'Content-Type': 'application/x-git-receive-pack-result' }, String(n)));
    return;
  }

  // git: a token needed, as for a private repository
  const m = /^Basic (.+)$/i.exec(req.headers.authorization || '');
  const motDePasse = m ? Buffer.from(m[1], 'base64').toString('utf8').replace(/^[^:]*:/, '') : null;
  const depot = /^(.+\.git)\/(?:info\/refs|git-upload-pack|git-receive-pack)$/.exec(chemin);
  if (!['github.com', 'gitlab.com'].includes(hote) || !depot) { req.resume(); return ecrire(404, { 'Content-Type': 'text/plain' }, 'Not Found\n'); }
  if (motDePasse !== JETON) { req.resume(); return ecrire(401, { 'WWW-Authenticate': `Basic realm="${hote}"`, 'Content-Type': 'text/plain; charset=UTF-8' }, 'Authentication required\n'); }
  if (!fs.existsSync(path.join(DEPOTS, hote, depot[1]))) { req.resume(); return ecrire(404, { 'Content-Type': 'text/plain' }, 'Repository not found.\n'); }
  const env = { ...envGit, GIT_PROJECT_ROOT: path.join(DEPOTS, hote), GIT_HTTP_EXPORT_ALL: '1', PATH_INFO: `/${chemin}`, REQUEST_METHOD: req.method,
    QUERY_STRING: u.search.replace(/^\?/, ''), CONTENT_TYPE: req.headers['content-type'] || '', REMOTE_USER: 'essai', REMOTE_ADDR: '127.0.0.1' };
  if (req.headers['content-length']) env.CONTENT_LENGTH = req.headers['content-length'];
  if (req.headers['git-protocol']) env.GIT_PROTOCOL = req.headers['git-protocol'];
  const cgi = spawn('git', ['http-backend'], { env, windowsHide: true });
  req.pipe(cgi.stdin);
  let tete = Buffer.alloc(0);
  let commence = false;
  cgi.stdout.on('data', (morceau) => {
    if (commence) { res.write(morceau); return; }
    tete = Buffer.concat([tete, morceau]);
    const fin = tete.indexOf('\r\n\r\n');
    if (fin < 0) return;
    let statut = 200;
    const entetes = { ...propres };
    for (const ligne of tete.subarray(0, fin).toString('latin1').split('\r\n')) {
      const i = ligne.indexOf(':');
      if (/^status$/i.test(ligne.slice(0, i).trim())) statut = parseInt(ligne.slice(i + 1), 10);
      else entetes[ligne.slice(0, i).trim()] = ligne.slice(i + 1).trim();
    }
    res.writeHead(statut, entetes);
    commence = true;
    res.write(tete.subarray(fin + 4));
  });
  cgi.on('close', () => { if (!commence) res.writeHead(500); res.end(); });
  cgi.on('error', () => { if (!commence) res.writeHead(500); res.end(); });
});

// ---------------------------------------------------------------- the relay

// A page that asks the relay with fetch, as Nuée's would: the status, the headers it may read and
// the text, or the browser's refusal
const PAGE = `<!doctype html>
<meta charset="utf-8">
<title>Relais</title>
<script>
window.essai = async (adresse, init) => {
  try {
    const r = await fetch(adresse, init);
    const entetes = {};
    r.headers.forEach((v, k) => { entetes[k] = v; });
    return { status: r.status, redirected: r.redirected, url: r.url, entetes, texte: await r.text() };
  } catch (e) {
    return { erreur: String(e) };
  }
};
</script>
`;

// The Worker's fetch handler behind Node's http (the page above at /essai.html, as the site's
// assets would serve it)
async function relaisNode(portBanc) {
  const env = { RELAIS_ESSAI: `http://127.0.0.1:${portBanc}`,
    ASSETS: { fetch: async () => new Response('<!doctype html><title>404</title>', { status: 404, headers: { 'Content-Type': 'text/html' } }) } };
  const s = http.createServer(async (req, res) => {
    if (req.url === '/essai.html') { res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' }); res.end(PAGE); return; }
    try {
      const h = new Headers();
      for (const [k, v] of Object.entries(req.headers)) {
        if (!['host', 'connection', 'content-length', 'transfer-encoding', 'keep-alive'].includes(k)) h.set(k, Array.isArray(v) ? v.join(', ') : v);
      }
      const corps = req.method !== 'GET' && req.method !== 'HEAD';
      const r = await worker.fetch(new Request(`http://${req.headers.host}${req.url}`, { method: req.method, headers: h,
        body: corps ? Readable.toWeb(req) : undefined, duplex: 'half' }), env, { waitUntil() {}, passThroughOnException() {} });
      const e = {};
      r.headers.forEach((v, k) => { e[k] = v; });
      res.writeHead(r.status, r.statusText || undefined, e);
      if (r.body) Readable.fromWeb(r.body).pipe(res); else res.end();
    } catch (e) {
      verifier('the handler answered', false, String(e && e.stack));
      res.writeHead(500);
      res.end();
    }
  });
  await new Promise((ok) => s.listen(0, '127.0.0.1', ok));
  return { adresse: `http://127.0.0.1:${s.address().port}`, serveur: s, sortie: [], arreter: async () => { s.closeAllConnections(); await new Promise((ok) => s.close(ok)); } };
}

// wrangler dev, on a configuration of its own: wrangler.jsonc's, without its build (the whole
// site), its domains and its name, the stand-in's address in its vars, its assets the page above
async function relaisWrangler(portBanc, port) {
  const wrangler = typeof options.wrangler === 'string' ? options.wrangler : process.env.WRANGLER || path.join(VITRINE, 'node_modules', 'wrangler', 'bin', 'wrangler.js');
  if (!fs.existsSync(wrangler)) throw new Error(`no wrangler at ${wrangler}: npm install in vitrine/, or --wrangler=<…/wrangler/bin/wrangler.js>`);
  const dossier = path.join(racine, 'wrangler');
  const actifs = path.join(dossier, 'actifs');
  fs.mkdirSync(actifs, { recursive: true });
  fs.writeFileSync(path.join(actifs, '404.html'), '<!doctype html><title>404</title>');
  fs.writeFileSync(path.join(actifs, 'essai.html'), PAGE);
  const config = lireJsonc(path.join(VITRINE, 'wrangler.jsonc'));
  for (const k of ['$schema', 'build', 'routes']) delete config[k];
  Object.assign(config, { name: 'nuee-relais-essai', main: path.join(VITRINE, 'src', 'worker.js'), assets: { ...config.assets, directory: actifs },
    vars: { ...config.vars, RELAIS_ESSAI: `http://127.0.0.1:${portBanc}` } });
  const fichier = path.join(dossier, 'wrangler.jsonc');
  fs.writeFileSync(fichier, JSON.stringify(config, null, 2));
  const sortie = [];
  const p = spawn(process.execPath, [wrangler, 'dev', '--config', fichier, '--ip', '127.0.0.1', '--port', String(port), '--inspector-port', String(await portLibre()),
    '--log-level', 'log', '--show-interactive-dev-session=false'], { cwd: dossier, env: { ...process.env, WRANGLER_SEND_METRICS: 'false' }, windowsHide: true });
  p.stdout.on('data', (d) => sortie.push(String(d)));
  p.stderr.on('data', (d) => sortie.push(String(d)));
  const arreter = async () => {
    if (p.exitCode !== null) return;
    if (process.platform === 'win32') await new Promise((ok) => execFile('taskkill', ['/pid', String(p.pid), '/T', '/F'], { windowsHide: true }, () => ok()));
    else p.kill('SIGTERM');
  };
  const adresse = `http://127.0.0.1:${port}`;
  for (const fin = Date.now() + 120000; ;) {
    if (p.exitCode !== null) throw new Error(`wrangler dev stopped:\n${sortie.join('')}`);
    const r = await fetch(`${adresse}/relais/`).catch(() => null);
    if (r && r.status === 404 && /relay/.test(await r.text())) break;
    if (Date.now() > fin) { await arreter(); throw new Error(`wrangler dev didn't start in 2 minutes:\n${sortie.join('')}`); }
    await pause(500);
  }
  return { adresse, sortie, arreter };
}

// ---------------------------------------------------------------- a browser, as the checks see it

// The CORS-safelisted response headers, which any page may read
const SURS = ['cache-control', 'content-language', 'content-length', 'content-type', 'expires', 'last-modified', 'pragma'];
// a request header a page may send without a preflight
const simple = (n, v) => (n === 'accept' && v.length <= 128 && !/["():<>?@[\\\]{}\u0000-\u0008\u000a-\u001f\u007f]/.test(v))
  || (n === 'content-type' && ['application/x-www-form-urlencoded', 'multipart/form-data', 'text/plain'].includes(v.split(';')[0].trim().toLowerCase()));

// A request as a browser makes it for a page at `origine` (another origin than the relay's): its
// preflight when it isn't a simple one, its Origin and what the browser adds (LEURRES), a refusal
// thrown as the browser's TypeError, redirects followed (20 at most: a POST becomes a GET after
// 303, 301 and 302). → { url, redirige, status, statusText, lisibles (the headers the page may
// read), reponse }
async function fetchCors(origine, adresse, { method = 'GET', headers = {}, body } = {}) {
  let url = adresse;
  let methode = method;
  let corps = body;
  const h = Object.fromEntries(Object.entries(headers).map(([k, v]) => [k.toLowerCase(), String(v)]));
  for (let saut = 0; saut <= 20; saut += 1) {
    const auteur = Object.keys(h).filter((k) => !simple(k, h[k])).sort();
    if (!['GET', 'HEAD', 'POST'].includes(methode) || auteur.length) {
      const p = await fetch(url, { method: 'OPTIONS', headers: { Origin: origine, 'Access-Control-Request-Method': methode,
        ...(auteur.length ? { 'Access-Control-Request-Headers': auteur.join(',') } : {}), 'Sec-Fetch-Mode': 'cors' } });
      await p.arrayBuffer();
      const liste = (n) => (p.headers.get(n) || '').toLowerCase().split(',').map((x) => x.trim());
      if (p.status < 200 || p.status > 299 || p.headers.get('access-control-allow-origin') !== origine
        || (!['GET', 'HEAD', 'POST'].includes(methode) && !liste('access-control-allow-methods').includes(methode.toLowerCase()))
        || auteur.some((n) => !liste('access-control-allow-headers').includes(n))) throw new TypeError(`Failed to fetch (preflight ${p.status})`);
    }
    const r = await fetch(url, { method: methode, headers: { ...LEURRES, ...h, Origin: origine }, body: corps, redirect: 'manual' });
    if (r.headers.get('access-control-allow-origin') !== origine) { await r.arrayBuffer(); throw new TypeError(`Failed to fetch (${r.status}, no Access-Control-Allow-Origin)`); }
    if ([301, 302, 303, 307, 308].includes(r.status) && r.headers.get('location')) {
      await r.arrayBuffer();
      url = new URL(r.headers.get('location'), url).href;
      if (r.status === 303 || ((r.status === 301 || r.status === 302) && methode === 'POST')) { methode = 'GET'; corps = undefined; delete h['content-type']; }
      continue;
    }
    const exposes = (r.headers.get('access-control-expose-headers') || '').toLowerCase().split(',').map((x) => x.trim());
    const lisibles = {};
    r.headers.forEach((v, k) => { if (SURS.includes(k) || exposes.includes(k)) lisibles[k] = v; });
    return { url, redirige: saut > 0, status: r.status, statusText: r.statusText, lisibles, reponse: r };
  }
  throw new TypeError('Failed to fetch (too many redirects)');
}

// isomorphic-git's HTTP client, its requests made as fetchCors makes them (isomorphic-git's own
// for the web collects a request's body too)
function clientNavigateur(origine) {
  return {
    async request({ url, method = 'GET', headers = {}, body }) {
      let corps;
      if (body) {
        const morceaux = [];
        for await (const m of body) morceaux.push(Buffer.from(m));
        corps = Buffer.concat(morceaux);
      }
      const r = await fetchCors(origine, url, { method, headers, body: corps });
      return { url: r.url, method, statusCode: r.status, statusMessage: r.statusText, headers: r.lisibles, body: r.reponse.body || [] };
    },
  };
}

// ---------------------------------------------------------------- checks

function configuration() {
  console.log('The configuration');
  const config = lireJsonc(path.join(VITRINE, 'wrangler.jsonc'));
  verifier('wrangler.jsonc: no log of each request (invocation_logs false)', config.observability && config.observability.logs && config.observability.logs.invocation_logs === false,
    config.observability);
  verifier('wrangler.jsonc: RELAIS_ESSAI set nowhere', !JSON.stringify(config).includes('RELAIS_ESSAI'));
  verifier('wrangler.jsonc: the limiter RELAIS_LIMITE', (config.ratelimits || []).some((x) => x.name === 'RELAIS_LIMITE' && x.simple && x.simple.limit > 0), config.ratelimits);
  verifier('src/relais.js writes nothing (no console)', !/\bconsole\./.test(fs.readFileSync(path.join(VITRINE, 'src', 'relais.js'), 'utf8')));
}

// The handler alone, fetch stood in for: where it sends, and its limiter
async function seul() {
  console.log('The relay alone (its fetch stood in for)');
  const vues = [];
  const avant = globalThis.fetch;
  globalThis.fetch = async (url, init) => {
    vues.push({ url: String(url), init });
    return new Response('ok', { status: 200, headers: { 'Content-Type': 'text/plain', 'Set-Cookie': 'a=b' } });
  };
  const demander = (adresse, env, init = {}) => worker.fetch(new Request(adresse, init), env, { waitUntil() {} });
  const refs = 'relais/github.com/proprietaire/projet.git/info/refs?service=git-upload-pack';
  const reel = 'https://github.com/proprietaire/projet.git/info/refs?service=git-upload-pack';
  try {
    await demander(`https://nuee.io/${refs}`, { RELAIS_ESSAI: 'http://127.0.0.1:9' });
    verifier('on nuee.io, RELAIS_ESSAI is never read: github.com itself', vues.at(-1).url === reel, vues.at(-1).url);
    await demander(`http://127.0.0.1:8796/${refs}`, { RELAIS_ESSAI: 'https://evil.example' });
    verifier('… nor anywhere when it isn’t an address on this computer', vues.at(-1).url === reel, vues.at(-1).url);
    await demander(`http://127.0.0.1:8796/${refs}`, {});
    verifier('… and without it, github.com', vues.at(-1).url === reel, vues.at(-1).url);
    await demander(`http://localhost:8796/${refs}`, { RELAIS_ESSAI: 'http://127.0.0.1:9' });
    verifier('reached on this computer, with it: the stand-in', vues.at(-1).url === 'http://127.0.0.1:9/github.com/proprietaire/projet.git/info/refs?service=git-upload-pack', vues.at(-1).url);
    const o = vues.at(-1).init;
    verifier('no redirect followed, no cache, GET without a body', o.redirect === 'manual' && o.cache === 'no-store' && o.method === 'GET' && o.body === undefined, o);

    const cles = [];
    const limiteur = (n) => ({ limit: async ({ key }) => { cles.push(key); return { success: cles.length <= n }; } });
    const env = { RELAIS_LIMITE: limiteur(1) };
    const n = vues.length;
    const r1 = await demander(`https://nuee.io/${refs}`, env, { headers: { 'CF-Connecting-IP': '198.51.100.4' } });
    const r2 = await demander(`https://nuee.io/${refs}`, env, { headers: { 'CF-Connecting-IP': '198.51.100.4', Origin: 'https://nuee.io' } });
    const r3 = await demander(`https://nuee.io/${refs}`, env, { method: 'OPTIONS', headers: { Origin: 'https://nuee.io', 'Access-Control-Request-Method': 'GET' } });
    verifier('the limiter, by address: past it, 429 with Retry-After (readable), nothing sent on; never for a preflight',
      r1.status === 200 && r2.status === 429 && r2.headers.get('retry-after') === '60' && /Retry-After/.test(r2.headers.get('access-control-expose-headers'))
      && r3.status === 204 && vues.length === n + 1 && cles.length === 2 && cles.every((k) => k === '198.51.100.4'), { r1: r1.status, r2: r2.status, r3: r3.status, cles });
    const r4 = await demander(`https://nuee.io/${refs}`, { RELAIS_LIMITE: { limit: async () => { throw new Error('no limiter'); } } });
    verifier('a limiter that fails: no limit', r4.status === 200, r4.status);
  } finally {
    globalThis.fetch = avant;
  }
  const r = await demander('http://127.0.0.1:8796/relais/github.com/a/b.git/info/refs?service=git-upload-pack', { RELAIS_ESSAI: `http://127.0.0.1:${await portLibre()}` });
  verifier('a host out of reach: 502, said', r.status === 502 && /couldn’t be reached/.test(await r.text()), r.status);
}

// isomorphic-git through the relay, as a page of another origin uses it
async function partager(RELAIS, ORIGINE, nus) {
  console.log('isomorphic-git through the relay');
  const commun = { fs, http: clientNavigateur(ORIGINE), corsProxy: RELAIS, onAuth: () => ({ username: 'x-access-token', password: JETON }) };
  const [a, b, c] = ['a', 'b', 'c'].map((x) => path.join(racine, 'travail', x));
  for (const d of [a, b, c]) fs.mkdirSync(d, { recursive: true });
  const auteur = { name: 'Essai', email: 'essai@example.org' };
  await git.init({ fs, dir: a, defaultBranch: 'main' });
  const gros = randomBytes(30 * 1024 * 1024);
  fs.writeFileSync(path.join(a, 'projet.nuee'), JSON.stringify({ format: 3, nom: 'Essai' }));
  fs.writeFileSync(path.join(a, 'donnees.bin'), gros);
  for (const f of ['projet.nuee', 'donnees.bin']) await git.add({ fs, dir: a, filepath: f });
  const v1 = await git.commit({ fs, dir: a, message: 'Première version', author: auteur });
  await git.addRemote({ fs, dir: a, remote: 'origin', url: 'https://github.com/proprietaire/projet.git' });
  let t = Date.now();
  const p1 = await git.push({ ...commun, dir: a, remote: 'origin', ref: 'main' });
  verifier(`pushed to github.com/proprietaire/projet, a 30 MB file in it (${((Date.now() - t) / 1000).toFixed(1)} s)`,
    p1.ok && gitSys(['rev-parse', 'main'], nus.github) === v1, p1);
  t = Date.now();
  await git.clone({ ...commun, dir: b, url: 'https://github.com/proprietaire/projet.git', singleBranch: true });
  verifier(`cloned back: the same files (${((Date.now() - t) / 1000).toFixed(1)} s)`, empreinte(fs.readFileSync(path.join(b, 'donnees.bin'))) === empreinte(gros)
    && fs.readFileSync(path.join(b, 'projet.nuee'), 'utf8') === fs.readFileSync(path.join(a, 'projet.nuee'), 'utf8'));
  fs.writeFileSync(path.join(a, 'projet.nuee'), JSON.stringify({ format: 3, nom: 'Essai', modeles: ['M1'] }));
  await git.add({ fs, dir: a, filepath: 'projet.nuee' });
  const v2 = await git.commit({ fs, dir: a, message: 'Un modèle', author: auteur });
  const p2 = await git.push({ ...commun, dir: a, remote: 'origin', ref: 'main' });
  const f = await git.fetch({ ...commun, dir: b, ref: 'main', singleBranch: true });
  verifier('changed, pushed again, fetched', p2.ok && f.fetchHead === v2 && (await git.resolveRef({ fs, dir: b, ref: 'refs/remotes/origin/main' })) === v2, { p2, f });
  const n = recues.length;
  const refs = await git.listServerRefs({ ...commun, url: 'https://github.com/proprietaire/projet.git', protocolVersion: 2, prefix: 'refs/heads/' });
  verifier('git’s protocol 2 (Git-Protocol sent on): the branches listed', refs.some((x) => x.ref === 'refs/heads/main' && x.oid === v2)
    && recues.slice(n).some((x) => x.methode === 'POST' && x.entetes['git-protocol'] === 'version=2'), refs);

  await git.addRemote({ fs, dir: a, remote: 'gitlab', url: 'https://gitlab.com/groupe/sous-groupe/projet.git' });
  const p3 = await git.push({ ...commun, dir: a, remote: 'gitlab', ref: 'main' });
  await git.clone({ ...commun, dir: c, url: 'https://gitlab.com/groupe/sous-groupe/projet.git', singleBranch: true });
  verifier('gitlab.com, a project in a subgroup: pushed and cloned', p3.ok && gitSys(['rev-parse', 'main'], nus.gitlab) === v2
    && empreinte(fs.readFileSync(path.join(c, 'donnees.bin'))) === empreinte(gros), p3);
  const info = await git.getRemoteInfo({ ...commun, url: 'https://gitlab.com/groupe/sous-groupe/projet' });
  verifier('GitLab’s address without .git: its redirect followed, through the relay', info.refs && info.refs.heads && info.refs.heads.main === v2, info.refs);
  return v2;
}

// The device flow of both, through the relay, as a page of another origin
async function connexions(RELAIS, ORIGINE) {
  console.log('The device flow through the relay');
  const formulaire = (champs) => ({ method: 'POST', headers: { Accept: 'application/json', 'Content-Type': 'application/x-www-form-urlencoded' }, body: new URLSearchParams(champs).toString() });
  const poster = async (chemin, champs) => {
    const r = await fetchCors(ORIGINE, `${RELAIS}/${chemin}`, formulaire(champs));
    return { status: r.status, type: r.lisibles['content-type'], json: await r.reponse.json(), recue: recues.at(-1) };
  };
  const code = await poster('github.com/login/device/code', { client_id: 'Ov23liEssai', scope: 'repo' });
  verifier('GitHub: the code asked for, the form sent on as it is, its JSON given back',
    code.status === 200 && code.json.user_code === 'WDJB-MJHT' && /json/.test(code.type) && code.recue.corps === 'client_id=Ov23liEssai&scope=repo'
    && code.recue.entetes['content-type'] === 'application/x-www-form-urlencoded' && code.recue.entetes.accept === 'application/json', code);
  const sonde = { client_id: 'Ov23liEssai', device_code: code.json.device_code, grant_type: 'urn:ietf:params:oauth:grant-type:device_code' };
  const s1 = await poster('github.com/login/oauth/access_token', sonde);
  const s2 = await poster('github.com/login/oauth/access_token', sonde);
  verifier('GitHub: pending (in a 200), then the token', s1.status === 200 && s1.json.error === 'authorization_pending' && s2.json.access_token === JETONS.github
    && s2.recue.corps === new URLSearchParams(sonde).toString(), { s1: s1.json, s2: s2.status });
  const gl = await poster('gitlab.com/oauth/authorize_device', { client_id: 'a'.repeat(64), scope: 'api read_user write_repository' });
  const g1 = await poster('gitlab.com/oauth/token', { client_id: 'a'.repeat(64), device_code: gl.json.device_code, grant_type: 'urn:ietf:params:oauth:grant-type:device_code' });
  const g2 = await poster('gitlab.com/oauth/token', { client_id: 'a'.repeat(64), device_code: gl.json.device_code, grant_type: 'urn:ietf:params:oauth:grant-type:device_code' });
  const g3 = await poster('gitlab.com/oauth/token', { client_id: 'a'.repeat(64), refresh_token: g2.json.refresh_token, grant_type: 'refresh_token' });
  verifier('GitLab: the code, pending (in a 400), the token, then renewed with its refresh token', gl.status === 200 && gl.json.user_code === 'GLAB1234'
    && g1.status === 400 && g1.json.error === 'authorization_pending' && g2.json.access_token === JETONS.gitlab && g3.json.access_token === JETONS.renouvele
    && g3.recue.corps.includes(`refresh_token=${JETONS.rafraichir}`), { gl: gl.status, g1: g1.status, g2: g2.status, g3: g3.status });
}

// What passes, what is refused, the redirects: requests made by hand
async function regles(RELAIS, portBanc) {
  console.log('What passes and what doesn’t');
  const brut = (chemin, init = {}) => fetch(`${RELAIS}/${chemin}`, { redirect: 'manual', ...init });
  const refs = 'github.com/proprietaire/projet.git/info/refs?service=git-upload-pack';
  const nuee = { Origin: 'https://nuee.io' };

  let n = recues.length;
  const r = await brut(refs, { headers: { ...LEURRES, ...nuee, Authorization: BASIC, Accept: '*/*' } });
  const texte = await r.text();
  const vue = recues[n];
  const noms = vue ? Object.keys(vue.entetes) : [];
  verifier('sent on: Accept, Authorization, the relay’s User-Agent; nothing else the request had', recues.length === n + 1 && vue.entetes.authorization === BASIC
    && vue.entetes.accept === '*/*' && vue.entetes['user-agent'] === AGENT && !noms.some((x) => INTERDITS.includes(x)), noms);
  const valeurs = [...r.headers.values()].join('\n');
  verifier('given back: the status, Content-Type and the body; never Set-Cookie, nor the server’s own headers', r.status === 200
    && r.headers.get('content-type') === 'application/x-git-upload-pack-advertisement' && texte.startsWith('001e# service=git-upload-pack')
    && !r.headers.has('set-cookie') && !r.headers.has('x-github-request-id') && !r.headers.has('strict-transport-security') && !valeurs.includes(LEURRE), [...r.headers.keys()]);
  verifier('Cache-Control: no-store, Vary: Origin, nosniff, a sandbox; the origin allowed (https://nuee.io)', r.headers.get('cache-control') === 'no-store'
    && /\bOrigin\b/.test(r.headers.get('vary')) && r.headers.get('x-content-type-options') === 'nosniff' && /sandbox/.test(r.headers.get('content-security-policy'))
    && r.headers.get('access-control-allow-origin') === 'https://nuee.io', Object.fromEntries(r.headers));
  const r401 = await brut(refs, { headers: nuee });
  await r401.arrayBuffer();
  verifier('a 401 given back without its WWW-Authenticate (the browser would ask for a password)', r401.status === 401 && !r401.headers.has('www-authenticate'), r401.status);

  for (const o of ['https://www.nuee.io', 'http://localhost:5173', 'http://127.0.0.1', `http://127.0.0.1:${portBanc}`]) {
    const x = await brut(refs, { headers: { Origin: o, Authorization: BASIC } });
    await x.arrayBuffer();
    verifier(`allowed: ${o}`, x.status === 200 && x.headers.get('access-control-allow-origin') === o, x.status);
  }
  for (const [site, attendu] of [[null, 200], ['same-origin', 200], ['none', 200], ['cross-site', 403], ['same-site', 403]]) {
    n = recues.length;
    const x = await brut(refs, { headers: { Authorization: BASIC, ...(site ? { 'Sec-Fetch-Site': site } : {}) } });
    await x.arrayBuffer();
    verifier(`no Origin, ${site ? `Sec-Fetch-Site: ${site}` : 'no Sec-Fetch-Site (not a browser)'}: ${attendu === 200 ? 'sent on' : 'refused, nothing sent on'}`,
      x.status === attendu && recues.length === n + (attendu === 200 ? 1 : 0) && !x.headers.has('access-control-allow-origin'), x.status);
  }

  n = recues.length;
  const p = await brut(refs, { method: 'OPTIONS', headers: { Origin: 'http://localhost:5173', 'Access-Control-Request-Method': 'GET', 'Access-Control-Request-Headers': 'authorization,git-protocol' } });
  const liste = (k) => (p.headers.get(k) || '').toLowerCase();
  verifier('a preflight answered by the relay itself (204): the origin, GET and POST, the headers git and the sign-in send, for two hours', p.status === 204
    && p.headers.get('access-control-allow-origin') === 'http://localhost:5173' && /get/.test(liste('access-control-allow-methods')) && /post/.test(liste('access-control-allow-methods'))
    && ['accept', 'authorization', 'content-type', 'git-protocol'].every((x) => liste('access-control-allow-headers').includes(x))
    && p.headers.get('access-control-max-age') === '7200' && /\bOrigin\b/.test(p.headers.get('vary')) && recues.length === n, Object.fromEntries(p.headers));

  // refused: with Nuée's origin, nothing sent on
  const refuser = async (nom, chemin, init, statut = 404, brute = false) => {
    const avant = recues.length;
    const x = brute ? await brutHttp(RELAIS, chemin, init) : await brut(chemin, { ...init, headers: { ...nuee, Authorization: BASIC, ...(init && init.headers) } });
    if (!brute) await x.arrayBuffer();
    verifier(nom, x.status === statut && recues.length === avant, `${x.status}, ${recues.length - avant} sent on`);
  };
  const POST = (headers = {}) => ({ method: 'POST', headers: { 'Content-Type': 'application/x-git-upload-pack-request', ...headers }, body: '0000' });
  for (const [nom, chemin, init] of [
    ['another host: example.com', 'example.com/proprietaire/projet.git/info/refs?service=git-upload-pack'],
    ['… api.github.com (which answers pages itself)', 'api.github.com/user'],
    ['… gist.github.com', 'gist.github.com/proprietaire/projet.git/info/refs?service=git-upload-pack'],
    ['… raw.githubusercontent.com', 'raw.githubusercontent.com/proprietaire/projet/main/projet.nuee'],
    ['… github.com.evil.example', 'github.com.evil.example/proprietaire/projet.git/info/refs?service=git-upload-pack'],
    ['… github.com:443', 'github.com:443/proprietaire/projet.git/info/refs?service=git-upload-pack'],
    ['… GitHub.com', 'GitHub.com/proprietaire/projet.git/info/refs?service=git-upload-pack'],
    ['… the stand-in’s own address', `127.0.0.1:${portBanc}/github.com/proprietaire/projet.git/info/refs?service=git-upload-pack`],
    ['… localhost', 'localhost/proprietaire/projet.git/info/refs?service=git-upload-pack'],
    ['no host', ''],
    ['another path: github.com/login', 'github.com/login'],
    ['… a repository’s page', 'github.com/proprietaire/projet'],
    ['… info/refs without its service', 'github.com/proprietaire/projet.git/info/refs'],
    ['… another service (git-upload-archive)', 'github.com/proprietaire/projet.git/info/refs?service=git-upload-archive'],
    ['… a query more', 'github.com/proprietaire/projet.git/info/refs?service=git-upload-pack&x=1'],
    ['… three parts on GitHub', 'github.com/a/b/projet.git/info/refs?service=git-upload-pack'],
    ['… one part', 'github.com/projet.git/info/refs?service=git-upload-pack'],
    ['… git’s dumb protocol', 'github.com/proprietaire/projet.git/HEAD'],
    ['… LFS', 'github.com/proprietaire/projet.git/info/lfs/objects/batch', POST()],
    ['… GitLab’s groups through /-/', 'gitlab.com/groupe/-/projet.git/info/refs?service=git-upload-pack'],
    ['… GitLab’s API', 'gitlab.com/api/v4/user'],
    ['another method: POST on info/refs', refs, POST()],
    ['… GET on git-upload-pack', 'github.com/proprietaire/projet.git/git-upload-pack'],
    ['… PUT on git-receive-pack', 'github.com/proprietaire/projet.git/git-receive-pack', { method: 'PUT', body: '0000' }],
    ['… DELETE', refs, { method: 'DELETE' }],
    ['… HEAD', refs, { method: 'HEAD' }],
    ['… a preflight for POST on info/refs', refs, { method: 'OPTIONS', headers: { 'Access-Control-Request-Method': 'POST' } }],
    ['… OPTIONS that isn’t a preflight', refs, { method: 'OPTIONS' }],
    ['the device flow: GET', 'github.com/login/device/code'],
    ['… with a query', 'github.com/login/device/code?client_id=x', { method: 'POST' }],
    ['… authorize, not the device’s', 'github.com/login/oauth/authorize', { method: 'POST' }],
    ['… GitLab’s authorize', 'gitlab.com/oauth/authorize', { method: 'POST' }],
    ['… GitLab’s revoke', 'gitlab.com/oauth/revoke', { method: 'POST' }],
    ['… GitHub’s path on GitLab', 'gitlab.com/login/device/code', { method: 'POST' }],
    ['… GitLab’s path on GitHub', 'github.com/oauth/token', { method: 'POST' }],
  ]) await refuser(nom, chemin, init);
  await refuser('a path that climbs out (%2e%2e)', '/relais/github.com/proprietaire/%2e%2e/%2e%2e/example.com/projet.git/info/refs?service=git-upload-pack', {}, 404, true);
  await refuser('… into one part (..)', '/relais/github.com/proprietaire/../projet.git/info/refs?service=git-upload-pack', {}, 404, true);
  await refuser('… a slash in a part (%2F)', '/relais/github.com/proprietaire%2Fautre/projet.git/info/refs?service=git-upload-pack', {}, 404, true);
  await refuser('… an empty part (//)', '/relais/github.com//proprietaire/projet.git/info/refs?service=git-upload-pack', {}, 404, true);
  await refuser('/relais itself', '/relais', {}, 404, true);

  for (const o of ['https://evil.example', 'https://nuee.io.evil.example', 'http://nuee.io', 'https://nuee.io:8443', 'https://app.nuee.io', 'null',
    'http://127.0.0.1.evil.example', 'http://localhost.evil.example:80', 'https://localhost:8443', 'http://[::1]:5173']) {
    const avant = recues.length;
    const x = await brut(refs, { headers: { Origin: o, Authorization: BASIC } });
    const y = await brut('github.com/login/device/code', { method: 'POST', headers: { Origin: o, 'Content-Type': 'application/x-www-form-urlencoded' }, body: 'client_id=x' });
    const z = await brut(refs, { method: 'OPTIONS', headers: { Origin: o, 'Access-Control-Request-Method': 'GET', 'Access-Control-Request-Headers': 'authorization' } });
    await Promise.all([x, y, z].map((w) => w.arrayBuffer()));
    verifier(`another origin, refused (403), nothing sent on, nothing it may read: ${o}`, [x, y, z].every((w) => w.status === 403 && !w.headers.has('access-control-allow-origin'))
      && recues.length === avant, [x.status, y.status, z.status]);
  }

  console.log('Redirects');
  n = recues.length;
  const s1 = await brut('gitlab.com/groupe/sous-groupe/projet/info/refs?service=git-upload-pack', { headers: nuee });
  verifier('GitLab’s 301 to the address with .git: passed on, through the relay', s1.status === 301 && (await s1.text()) === ''
    && s1.headers.get('location') === '/relais/gitlab.com/groupe/sous-groupe/projet.git/info/refs?service=git-upload-pack'
    && s1.headers.get('access-control-allow-origin') === 'https://nuee.io', [s1.status, s1.headers.get('location')]);
  for (const [nom, chemin, init] of [
    ['to another host: 502, not passed on', 'github.com/ailleurs/depot.git/info/refs?service=git-upload-pack'],
    ['to http: 502', 'github.com/clair/depot.git/info/refs?service=git-upload-pack'],
    ['to another of github.com’s pages: 502', 'github.com/connexion/depot.git/info/refs?service=git-upload-pack'],
    ['a POST that 301 makes a GET of git-upload-pack: 502', 'github.com/poste301/depot.git/git-upload-pack', POST()],
    ['… GitLab’s, without .git: 502', 'gitlab.com/groupe/sous-groupe/projet/git-upload-pack', POST()],
  ]) {
    const x = await brut(chemin, { ...init, headers: { ...nuee, ...(init && init.headers) } });
    await x.arrayBuffer();
    verifier(nom, x.status === 502 && !x.headers.has('location'), [x.status, x.headers.get('location')]);
  }
  const s2 = await brut('github.com/poste/depot.git/git-upload-pack', POST(nuee));
  await s2.arrayBuffer();
  verifier('a POST’s 307 to another repository’s git-upload-pack: passed on, through the relay', s2.status === 307
    && s2.headers.get('location') === '/relais/github.com/poste/autre.git/git-upload-pack', [s2.status, s2.headers.get('location')]);
}

// A request by hand with its path as it is (fetch would resolve "..", "%2e%2e" and "//" first)
function brutHttp(RELAIS, chemin) {
  const u = new URL(RELAIS);
  return new Promise((ok, non) => {
    const q = http.request({ host: u.hostname, port: u.port, path: chemin, method: 'GET', headers: { Origin: 'https://nuee.io', Authorization: BASIC } }, (r) => {
      r.resume();
      r.on('end', () => ok({ status: r.statusCode }));
    });
    q.on('error', non);
    q.end();
  });
}

// The bodies streamed both ways: each side waits for the other to have the first part
async function flux(RELAIS, ORIGINE) {
  console.log('Streaming');
  attentes.lu = signal();
  let t = Date.now();
  const r = await fetch(`${RELAIS}/github.com/flux/lent.git/info/refs?service=git-upload-pack`, { headers: { Origin: ORIGINE } });
  const lecteur = r.body.getReader();
  let recu = 0;
  let premier = null;
  for (;;) {
    const { value, done } = await lecteur.read();
    if (done) break;
    recu += value.length;
    if (premier === null && recu >= FLUX.premier.length) { premier = Date.now() - t; attentes.lu.ok(); }
  }
  verifier(`an answer given back as it comes: its first part read before the server sends the rest (${premier} ms)`,
    r.status === 200 && premier !== null && premier < 10000 && recu === FLUX.premier.length + FLUX.second.length, { premier, recu });

  attentes.recu = signal();
  let attendu = false;
  const corps = new ReadableStream({
    async start(c) {
      c.enqueue(new Uint8Array(FLUX.premier));
      attendu = await Promise.race([attentes.recu.then(() => true), pause(15000).then(() => false)]);
      c.enqueue(new Uint8Array(FLUX.second));
      c.close();
    },
  });
  t = Date.now();
  const p = await fetch(`${RELAIS}/github.com/flux/lent.git/git-receive-pack`, { method: 'POST', duplex: 'half', body: corps,
    headers: { Origin: ORIGINE, 'Content-Type': 'application/x-git-receive-pack-request' } });
  const n = Number(await p.text());
  verifier(`a request’s body sent on as it comes: the server has its first part before the rest is sent (${Date.now() - t} ms)`,
    p.status === 200 && attendu && n === FLUX.premier.length + FLUX.second.length, { status: p.status, attendu, n });
}

// In Chrome or Edge: a page of another origin, one of the relay's own (as nuee.io/app/ and
// nuee.io/relais/ are), one of a third origin (autre.test)
async function navigateur(RELAIS, portPage, oid) {
  console.log('In a browser');
  let pw;
  try { pw = depuisApp('playwright-core'); } catch { console.log('  (no playwright-core in app/node_modules: no browser)'); return; }
  const exe = [process.env.CHROME, 'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe', 'C:/Program Files/Google/Chrome/Application/chrome.exe',
    '/usr/bin/google-chrome', '/usr/bin/chromium', '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'].find((x) => x && fs.existsSync(x));
  if (!exe) { console.log('  (no Chrome or Edge found: no browser; CHROME=… to say which)'); return; }
  const b = await pw.chromium.launch({ executablePath: exe, headless: true, args: ['--host-resolver-rules=MAP autre.test 127.0.0.1'] });
  try {
    const ouvrir = async (adresse) => { const p = await b.newPage(); await p.goto(adresse); return p; };
    const autre = await ouvrir(`http://127.0.0.1:${portPage}/essai.html`);
    const meme = await ouvrir(`${new URL(RELAIS).origin}/essai.html`);
    const tiers = await ouvrir(`http://autre.test:${portPage}/essai.html`);
    const essai = (page, chemin, init) => page.evaluate(([a, i]) => window.essai(a, i), [`${RELAIS}/${chemin}`, init]);
    const refs = 'github.com/proprietaire/projet.git/info/refs?service=git-upload-pack';
    const auth = { headers: { Authorization: BASIC } };
    const demande = `0032want ${oid}\n00000009done\n`;
    const pack = { method: 'POST', body: demande, headers: { 'Content-Type': 'application/x-git-upload-pack-request', Accept: 'application/x-git-upload-pack-result', Authorization: BASIC } };
    const code = { method: 'POST', body: 'client_id=Ov23liEssai&scope=repo', headers: { Accept: 'application/json', 'Content-Type': 'application/x-www-form-urlencoded' } };

    for (const [nom, page] of [['a page of another origin', autre], ['a page of the relay’s own origin (nuee.io/app/)', meme]]) {
      const x = await essai(page, refs, auth);
      verifier(`${nom}: info/refs with a token`, x.status === 200 && x.entetes['content-type'] === 'application/x-git-upload-pack-advertisement'
        && x.texte.startsWith('001e# service=git-upload-pack'), x.erreur || x.status);
      const v2 = await essai(page, refs, { headers: { Authorization: BASIC, 'Git-Protocol': 'version=2' } });
      verifier(`${nom}: … in git’s protocol 2`, v2.status === 200 && /version 2/.test(v2.texte), v2.erreur || v2.status);
      const y = await essai(page, 'github.com/proprietaire/projet.git/git-upload-pack', pack);
      verifier(`${nom}: git-upload-pack, its pack`, y.status === 200 && y.entetes['content-type'] === 'application/x-git-upload-pack-result' && y.texte.startsWith('0008NAK'), y.erreur || y.status);
      const z = await essai(page, refs);
      verifier(`${nom}: without a token, the 401 read, no WWW-Authenticate`, z.status === 401 && !('www-authenticate' in z.entetes), z.erreur || z);
      const c = await essai(page, 'github.com/login/device/code', code);
      verifier(`${nom}: the device flow’s code`, c.status === 200 && JSON.parse(c.texte || '{}').user_code === 'WDJB-MJHT', c.erreur || c.status);
      const s = await essai(page, 'gitlab.com/groupe/sous-groupe/projet/info/refs?service=git-upload-pack', auth);
      verifier(`${nom}: GitLab’s address without .git, its redirect followed`, s.status === 200 && s.redirected
        && s.url.endsWith('/relais/gitlab.com/groupe/sous-groupe/projet.git/info/refs?service=git-upload-pack'), s.erreur || s);
      const q = await essai(page, 'github.com/login');
      verifier(`${nom}: a path the relay refuses, its 404 read`, q.status === 404, q.erreur || q.status);
    }
    for (const [nom, chemin, init] of [['info/refs with a token (preflighted)', refs, auth], ['the device flow’s code (no preflight)', 'github.com/login/device/code', code],
      ['info/refs without one (no preflight)', refs, undefined]]) {
      const avant = recues.length;
      const x = await essai(tiers, chemin, init);
      verifier(`a page of a third origin (autre.test): ${nom} refused by the browser, nothing sent on`, !!x.erreur && recues.length === avant, x.erreur || x.status);
    }
  } finally {
    await b.close();
  }
}

// ---------------------------------------------------------------- the run

let relais = null;
const page = http.createServer((req, res) => { res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' }); res.end(PAGE); });
try {
  configuration();
  await seul();
  try { gitSys(['--version']); } catch { throw new Error('git isn’t on the PATH: the stand-in needs its http-backend'); }
  const nus = { github: depotNu('github.com/proprietaire/projet.git'), gitlab: depotNu('gitlab.com/groupe/sous-groupe/projet.git') };
  const portBanc = await new Promise((ok) => banc.listen(0, '127.0.0.1', () => ok(banc.address().port)));
  const portPage = await new Promise((ok) => page.listen(0, '127.0.0.1', () => ok(page.address().port)));
  relais = options.wrangler ? await relaisWrangler(portBanc, Number(options.port) || 8796) : await relaisNode(portBanc);
  console.log(`The relay: ${options.wrangler ? 'wrangler dev' : 'the Worker’s fetch handler in Node'}, ${relais.adresse}/relais/`);
  const RELAIS = `${relais.adresse}/relais`;
  const ORIGINE = `http://127.0.0.1:${portPage}`;
  const oid = await partager(RELAIS, ORIGINE, nus);
  await connexions(RELAIS, ORIGINE);
  await regles(RELAIS, portBanc);
  await flux(RELAIS, ORIGINE);
  await navigateur(RELAIS, portPage, oid);

  console.log('What the stand-ins received');
  const fuites = recues.filter((x) => Object.entries(x.entetes).some(([k, v]) => INTERDITS.includes(k) || String(v).includes(LEURRE) || String(v).includes(IP)));
  verifier(`none of their ${recues.length} requests carried a cookie, an address, an Origin, a Referer or Cloudflare’s headers`, !fuites.length,
    fuites.slice(0, 2).map((x) => [x.methode, x.chemin, x.entetes]));
  const agents = [...new Set(recues.map((x) => x.entetes['user-agent']))];
  verifier('each under the relay’s User-Agent', agents.length === 1 && agents[0] === AGENT, agents);
  const hotes = [...new Set(recues.map((x) => x.hote))];
  verifier('only github.com and gitlab.com were asked', hotes.every((h) => h === 'github.com' || h === 'gitlab.com'), hotes);
  const postes = recues.filter((x) => x.methode === 'POST');
  console.log(`  (the headers they received: ${[...new Set(recues.flatMap((x) => Object.keys(x.entetes)))].sort().join(', ')}; of ${postes.length} POSTs, `
    + `${postes.filter((x) => x.entetes['content-length']).length} with Content-Length, ${postes.filter((x) => x.entetes['transfer-encoding'] === 'chunked').length} chunked)`);
  if (options.wrangler) {
    const sortie = relais.sortie.join('');
    const secrets = [JETON, BASIC.slice(6), ...Object.values(JETONS)];
    verifier('wrangler dev’s output: no token, no device code, nothing the requests carried', !secrets.some((x) => sortie.includes(x)) && !sortie.includes(LEURRE));
  }
} catch (e) {
  verifier('the run went to its end', false, String(e && e.stack));
} finally {
  if (relais) await relais.arreter();
  for (const s of [banc, page]) {
    if (s.listening) { s.closeAllConnections(); await new Promise((ok) => s.close(ok)); }
  }
  // (workerd, just stopped, may still hold a file of its own there for a moment on Windows)
  try { fs.rmSync(racine, { recursive: true, force: true, maxRetries: 8, retryDelay: 250 }); } catch { console.log(`  (${racine} left: still in use)`); }
}
console.log(echecs ? `${echecs} vérification(s) en échec` : 'Tout est bon');
process.exitCode = echecs ? 1 : 0;
