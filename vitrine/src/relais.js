// nuee.io's relay, for Nuée's browser version (/app/): what a page can't ask GitHub and GitLab
// itself, since git's endpoints and those of the device flow send no CORS headers (api.github.com
// and gitlab.com/api/v4 answer any page: the page asks them directly). In the form of
// isomorphic-git's `corsProxy`, https://nuee.io/relais/<host>/<path> goes to https://<host>/<path>,
// for these requests alone (anything else: 404):
//   github.com, gitlab.com   git's smart HTTP: GET <repository>/info/refs?service=git-upload-pack or
//                            git-receive-pack, POST <repository>/git-upload-pack or git-receive-pack;
//                            <repository> an owner and a name on GitHub, groups and a name on
//                            GitLab, with or without .git
//   github.com               the device flow: POST login/device/code and login/oauth/access_token
//   gitlab.com               the device flow: POST oauth/authorize_device and oauth/token
// - CORS for Nuée's pages alone: https://nuee.io and https://www.nuee.io (the same site), and
//   http://127.0.0.1 and http://localhost on any port, for the checks. A request from anywhere else
//   is refused (403) before anything is sent on: another Origin, or none from another site's page
//   (Sec-Fetch-Site). The page's own requests on nuee.io are of the same origin.
// - Sent on: Accept, Authorization, Content-Type and Git-Protocol, under the relay's own User-Agent;
//   never a cookie, the visitor's address or Cloudflare's headers. Sent back: the status,
//   Content-Type and Retry-After; never Set-Cookie, nor WWW-Authenticate (on nuee.io's own pages,
//   the browser would ask for a password itself).
// - The bodies stream both ways, never held. Cloudflare refuses a request body over 100 MB (413:
//   the free plan's limit), which a push of more than 100 MB at once would be.
// - A redirect is passed on only to one of these requests, through the relay again; any other:
//   502. Cache-Control: no-store. Nothing is logged: nothing written here, and no log of each
//   request (wrangler.jsonc: invocation_logs).
// - RELAIS_LIMITE (wrangler.jsonc): each address may have 600 requests a minute sent on, in each
//   Cloudflare location; then 429.
// - RELAIS_ESSAI, for tests/essai_relais.mjs alone: a stand-in on this computer
//   (http://127.0.0.1:<port>) that receives /<host>/<path> instead. Read only when the relay itself
//   is reached on this computer (wrangler dev), never on nuee.io; only the check sets it.

const AGENT = 'git/nuee.io-relais';
const ORIGINES = /^(?:https:\/\/(?:www\.)?nuee\.io|http:\/\/(?:127\.0\.0\.1|localhost)(?::\d{1,5})?)$/;
// a repository's path: its number of parts (GitHub: owner and name; GitLab: up to 20 groups, then
// the name) and the device flow's two endpoints
const HOTES = {
  'github.com': { parties: [2, 2], connexion: ['login/device/code', 'login/oauth/access_token'] },
  'gitlab.com': { parties: [2, 21], connexion: ['oauth/authorize_device', 'oauth/token'] },
};
// (a part as both name them: letters, digits, "_", "." and "-"; never "." nor "..")
const PARTIE = /^[A-Za-z0-9_.][A-Za-z0-9_.-]{0,254}$/;
const DEMANDE = ['accept', 'authorization', 'content-type', 'git-protocol'];
const REPONSE = ['content-type', 'retry-after'];
const REDIRECTIONS = [301, 302, 303, 307, 308];
const TOUJOURS = { 'Cache-Control': 'no-store', Vary: 'Origin', 'X-Content-Type-Options': 'nosniff',
  'Content-Security-Policy': "default-src 'none'; frame-ancestors 'none'; sandbox" };

const depot = (hote, chemin) => {
  const p = chemin.split('/');
  const [min, max] = HOTES[hote].parties;
  return p.length >= min && p.length <= max && p.every((x) => PARTIE.test(x) && x !== '.' && x !== '..');
};

// Whether the relay passes this request on: its host, method, path (without its first "/") and query
function relaye(hote, methode, chemin, requete) {
  if (!Object.hasOwn(HOTES, hote)) return false;
  if (methode === 'POST' && !requete && HOTES[hote].connexion.includes(chemin)) return true;
  const refs = /^(.+)\/info\/refs$/.exec(chemin);
  if (refs) return methode === 'GET' && /^\?service=git-(?:upload|receive)-pack$/.test(requete) && depot(hote, refs[1]);
  const service = /^(.+)\/git-(?:upload|receive)-pack$/.exec(chemin);
  return !!service && methode === 'POST' && !requete && depot(hote, service[1]);
}

// Where a redirect sends the browser, through the relay again, when the request it makes then
// (a POST becomes a GET after 303, and after 301 or 302) is one the relay passes on; else null
function redirection(location, depart, statut, methode) {
  let u;
  try { u = new URL(location, depart); } catch { return null; }
  const ensuite = statut === 303 || ((statut === 301 || statut === 302) && methode === 'POST') ? 'GET' : methode;
  return u.protocol === 'https:' && !u.port && !u.username && !u.password && relaye(u.hostname, ensuite, u.pathname.slice(1), u.search)
    ? `/relais/${u.hostname}${u.pathname}${u.search}` : null;
}

// The checks' stand-in (RELAIS_ESSAI), only when the relay is reached on this computer
function banc(env, url) {
  const a = env && env.RELAIS_ESSAI;
  return typeof a === 'string' && /^http:\/\/(?:127\.0\.0\.1|localhost):\d{1,5}$/.test(a)
    && /^(?:127\.0\.0\.1|localhost)$/.test(url.hostname) ? a : null;
}

export async function relayer(request, env, url) {
  const origine = request.headers.get('Origin');
  const admise = origine !== null && ORIGINES.test(origine);
  const cors = admise ? { 'Access-Control-Allow-Origin': origine, 'Access-Control-Expose-Headers': 'Retry-After' } : {};
  const texte = (statut, message, entetes = {}) => new Response(`${message}\n`, { status: statut,
    headers: { ...TOUJOURS, ...cors, 'Content-Type': 'text/plain; charset=utf-8', ...entetes } });

  // another site's page: nothing sent on, nothing it may read
  const site = request.headers.get('Sec-Fetch-Site');
  if (origine !== null ? !admise : site === 'cross-site' || site === 'same-site') {
    return texte(403, 'Forbidden: nuee.io’s relay answers Nuée’s own pages only.');
  }
  const m = /^\/relais\/([^/]+)\/(.+)$/.exec(url.pathname);
  const preliminaire = request.method === 'OPTIONS';  // a preflight, for the request it announces
  const methode = preliminaire ? request.headers.get('Access-Control-Request-Method') : request.method;
  if (!m || !relaye(m[1], methode, m[2], url.search)) {
    return texte(404, 'Not found: nuee.io’s relay passes on git’s requests and the sign-in to github.com and gitlab.com, nothing else.');
  }
  const [, hote, chemin] = m;
  if (preliminaire) {
    return new Response(null, { status: 204, headers: { ...TOUJOURS, ...cors, 'Access-Control-Allow-Methods': 'GET, POST',
      'Access-Control-Allow-Headers': 'Accept, Authorization, Content-Type, Git-Protocol', 'Access-Control-Max-Age': '7200' } });
  }

  if (env && env.RELAIS_LIMITE) {
    let libre = true;
    try { ({ success: libre } = await env.RELAIS_LIMITE.limit({ key: request.headers.get('CF-Connecting-IP') || '' })); } catch { /* no limiter: no limit */ }
    if (!libre) return texte(429, 'Too many requests: try again in a minute.', { 'Retry-After': '60' });
  }

  const depart = `https://${hote}/${chemin}${url.search}`;
  const essai = banc(env, url);
  const entetes = new Headers({ 'User-Agent': AGENT });
  for (const n of DEMANDE) {
    const v = request.headers.get(n);
    if (v !== null) entetes.set(n, v);
  }
  let amont;
  try {
    amont = await fetch(essai ? `${essai}/${hote}/${chemin}${url.search}` : depart, { method: request.method, headers: entetes,
      body: request.method === 'POST' ? request.body : undefined, duplex: 'half', redirect: 'manual', cache: 'no-store' });
  } catch {
    return texte(502, `Bad gateway: ${hote} couldn’t be reached.`);
  }
  const retour = new Headers({ ...TOUJOURS, ...cors });
  for (const n of REPONSE) {
    const v = amont.headers.get(n);
    if (v !== null) retour.set(n, v);
  }
  if (REDIRECTIONS.includes(amont.status)) {
    const location = amont.headers.get('Location');
    const vers = location && redirection(location, depart, amont.status, request.method);
    if (amont.body) amont.body.cancel().catch(() => {});
    if (!vers) return texte(502, `Bad gateway: ${hote} redirects elsewhere than the relay passes on.`);
    retour.set('Location', vers);
    return new Response(null, { status: amont.status, headers: retour });
  }
  const vide = amont.status === 204 || amont.status === 205 || amont.status === 304;
  return new Response(vide ? null : amont.body, { status: amont.status, statusText: amont.statusText, headers: retour });
}
