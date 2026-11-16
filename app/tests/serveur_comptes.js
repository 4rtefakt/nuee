// A stand-in for GitHub and GitLab (tests/essai_comptes.js; also for trying the sign-in dialogs in a
// window): their device flow (RFC 8628) with its answers chosen by the test, a GitLab token's
// renewal, the user API, a repository created, a collaborator invited or a member added, the
// accounts' pictures. One local HTTP server answers for every server: https://<host>/<path> is asked
// as http://127.0.0.1:<port>/<host>/<path> through the fetch it gives (simule.fetch), which refuses
// any other host, so that nothing reaches the real services. Every request is recorded.
const crypto = require('crypto');
const http = require('http');
const zlib = require('zlib');

const HOTES = ['github.com', 'api.github.com', 'avatars.githubusercontent.com', 'gitlab.com', 'gitlab.example.org'];

// A 64 × 64 PNG of five by five squares, symmetric, as GitHub draws an account without a picture
function identicon(graine, [r, v, b]) {
  const h = crypto.createHash('sha256').update(graine).digest();
  const lignes = [];
  for (let y = 0; y < 64; y += 1) {
    const ligne = Buffer.alloc(1 + 64 * 3);
    for (let x = 0; x < 64; x += 1) {
      const i = Math.floor((x - 2) / 12);
      const j = Math.floor((y - 2) / 12);
      const plein = x >= 2 && y >= 2 && i < 5 && j < 5 && (h[j * 3 + Math.min(i, 4 - i)] & 1);
      ligne.set(plein ? [r, v, b] : [240, 240, 240], 1 + x * 3);
    }
    lignes.push(ligne);
  }
  const crc = (b) => { let c = ~0; for (const o of b) { c ^= o; for (let k = 0; k < 8; k += 1) c = (c >>> 1) ^ (0xedb88320 & -(c & 1)); } return ~c >>> 0; };
  const morceau = (type, donnees) => {
    const t = Buffer.concat([Buffer.from(type, 'ascii'), donnees]);
    const n = Buffer.alloc(4); n.writeUInt32BE(donnees.length);
    const c = Buffer.alloc(4); c.writeUInt32BE(crc(t));
    return Buffer.concat([n, t, c]);
  };
  const ihdr = Buffer.alloc(13);
  ihdr.writeUInt32BE(64, 0); ihdr.writeUInt32BE(64, 4); ihdr[8] = 8; ihdr[9] = 2;
  return Buffer.concat([Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]), morceau('IHDR', ihdr), morceau('IDAT', zlib.deflateSync(Buffer.concat(lignes))), morceau('IEND', Buffer.alloc(0))]);
}

async function serveurComptes() {
  let port = null;
  const aleatoire = (n) => crypto.randomBytes(n).toString('hex');
  const etat = {
    // the client ids of Nuée's applications on each server
    ids: { 'github.com': 'Ov23liEssaiNuee0001', 'gitlab.com': aleatoire(32), 'gitlab.example.org': aleatoire(32) },
    // the answers of the next device code's polls, in order ('attente', 'lent', 'ok', 'refuse', 'expire');
    // once they are used, 'ok' if approved (approuver), 'refuse' if refused, else 'attente'
    prochain: [],
    codes: new Map(),          // device code → { hote, code, client, portee, reponses, sondages, accorde, refuse, intervalle }
    jetons: new Map(),         // token → { hote, login, portee }
    renouvellements: new Map(),  // refresh token → { hote, login, client }
    // the accounts on each server
    comptes: {
      'github.com': [
        { login: 'octocat', id: 583231, name: 'Mona Octocat', type: 'User', couleur: [65, 131, 196] },
        { login: 'hubot', id: 480938, name: 'Hubot Robot', type: 'User', couleur: [196, 92, 65] },
        { login: 'alice-martin', id: 900001, name: 'Alice Martin', type: 'User', couleur: [80, 160, 90] },
        { login: 'github', id: 9919, name: 'GitHub', type: 'Organization', couleur: [30, 30, 30] },
      ],
      'gitlab.com': [
        { login: 'alice', id: 1, name: 'Alice Durand', couleur: [226, 67, 41] },
        { login: 'bob', id: 2, name: 'Bob Leroy', couleur: [252, 109, 38] },
      ],
      'gitlab.example.org': [
        { login: 'chercheuse', id: 11, name: 'Claire Chercheuse', couleur: [107, 79, 187] },
        { login: 'collegue', id: 12, name: 'Colin Collègue', couleur: [84, 130, 53] },
      ],
    },
    depots: new Map(),         // 'host/path' → { hote, chemin, prive, proprietaire, membres: Set }
    ancien: new Set(),         // GitLabs without the device flow (before 17.3: 404)
    fluxDesactive: false,      // the application's device flow not enabled (GitHub's Enable Device Flow, GitLab's Device authorization grant)
    bavard: false,             // the user API failing with the token in its message
    redirige: false,           // the user API sending elsewhere (a redirect that would carry the token)
    coupes: new Set(),         // servers the network doesn't reach
    horsListe: [],             // addresses of other servers asked (never fetched)
    requetes: [],
    secrets: [],               // every token, refresh token and device code given
  };
  const secret = (s) => { etat.secrets.push(s); return s; };
  const comptesDe = (hote) => etat.comptes[hote === 'api.github.com' || hote === 'avatars.githubusercontent.com' ? 'github.com' : hote] || [];
  const nouveauCode = (hote) => (hote === 'github.com' ? `${aleatoire(2).toUpperCase()}-${aleatoire(2).toUpperCase()}` : aleatoire(4).toUpperCase());

  const lireCorps = (req) => new Promise((ok) => {
    const m = [];
    req.on('data', (d) => m.push(d));
    req.on('end', () => ok(Buffer.concat(m).toString('utf8')));
  });
  const json = (res, statut, valeur, entetes = {}) => {
    res.writeHead(statut, { 'Content-Type': 'application/json; charset=utf-8', ...entetes });
    res.end(valeur === undefined ? '' : JSON.stringify(valeur));
  };
  const vide = (res, statut) => { res.writeHead(statut); res.end(); };

  // The account of a request's token on a server: { compte, jeton } or null
  const porteur = (req, hote) => {
    const m = /^Bearer (\S+)$/.exec(req.headers.authorization || '');
    const j = m && etat.jetons.get(m[1]);
    const serveur = hote === 'api.github.com' ? 'github.com' : hote;
    if (!j || j.hote !== serveur) return null;
    return { compte: comptesDe(serveur).find((c) => c.login === j.login), jeton: m[1], portee: j.portee };
  };
  const donnerJeton = (hote, login, portee) => {
    const jeton = secret(hote === 'github.com' ? `gho_${aleatoire(18)}` : `glo-${aleatoire(24)}`);
    etat.jetons.set(jeton, { hote, login, portee });
    return jeton;
  };

  // ---------------------------------------------------------------- the device flow

  async function oauth(req, res, hote, chemin, corps) {
    const gh = hote === 'github.com';
    const f = Object.fromEntries(new URLSearchParams(corps));
    const erreur = (code, description) => json(res, gh ? 200 : 400, { error: code, error_description: description });
    if (chemin === (gh ? '/login/device/code' : '/oauth/authorize_device')) {
      if (!gh && etat.ancien.has(hote)) { res.writeHead(404, { 'Content-Type': 'text/html' }); res.end('<html><body>404 Page Not Found</body></html>'); return; }
      if (gh && etat.fluxDesactive) { erreur('device_flow_disabled', 'Device Flow must be explicitly enabled for this App'); return; }
      if (f.client_id !== etat.ids[hote]) { json(res, 401, { error: gh ? 'incorrect_client_credentials' : 'invalid_client', error_description: 'Client authentication failed' }); return; }
      if (!gh && etat.fluxDesactive) { erreur('access_denied', 'The resource owner or authorization server denied the request.'); return; }
      const appareil = secret(aleatoire(20));
      const code = nouveauCode(hote);
      etat.codes.set(appareil, { hote, code, client: f.client_id, portee: f.scope, reponses: etat.prochain.splice(0), sondages: 0, accorde: false, refuse: false, intervalle: 5 });
      json(res, 200, { device_code: appareil, user_code: code, verification_uri: `https://${hote}/${gh ? 'login/device' : 'oauth/device'}`,
        ...(gh ? {} : { verification_uri_complete: `https://${hote}/oauth/device?user_code=${code}` }), expires_in: gh ? 900 : 300, interval: 5 });
      return;
    }
    if (chemin === (gh ? '/login/oauth/access_token' : '/oauth/token')) {
      if (f.grant_type === 'refresh_token' && !gh) {
        const r = etat.renouvellements.get(f.refresh_token);
        if (!r || r.hote !== hote || r.client !== f.client_id) { erreur('invalid_grant', 'The provided authorization grant is invalid, expired, revoked'); return; }
        // (the old token and refresh token stop working, as GitLab does)
        etat.renouvellements.delete(f.refresh_token);
        for (const [j, x] of etat.jetons) if (x.renouvellement === f.refresh_token) etat.jetons.delete(j);
        const rafraichir = secret(`glr-${aleatoire(24)}`);
        const jeton = donnerJeton(hote, r.login, r.portee);
        etat.jetons.get(jeton).renouvellement = rafraichir;
        etat.renouvellements.set(rafraichir, { ...r });
        json(res, 200, { access_token: jeton, token_type: 'Bearer', expires_in: 7200, refresh_token: rafraichir, scope: r.portee, created_at: Math.floor(Date.now() / 1000) });
        return;
      }
      if (f.grant_type !== 'urn:ietf:params:oauth:grant-type:device_code') { erreur('unsupported_grant_type', 'The grant type is not supported'); return; }
      const c = etat.codes.get(f.device_code);
      if (!c || c.hote !== hote || c.client !== f.client_id) { erreur(gh ? 'incorrect_device_code' : 'invalid_grant', 'The device code is not valid'); return; }
      c.sondages += 1;
      const r = c.reponses.length ? c.reponses.shift() : c.accorde ? 'ok' : c.refuse ? 'refuse' : 'attente';
      if (r === 'attente') erreur('authorization_pending', 'The authorization request is still pending');
      else if (r === 'lent') { c.intervalle += 5; json(res, gh ? 200 : 400, { error: 'slow_down', error_description: 'Too many requests', ...(gh ? { interval: c.intervalle } : {}) }); }
      else if (r === 'refuse') { etat.codes.delete(f.device_code); erreur('access_denied', 'The end user denied the request'); }
      else if (r === 'expire') { etat.codes.delete(f.device_code); erreur('expired_token', 'The device code has expired'); }
      else {
        etat.codes.delete(f.device_code);
        const login = (c.login || comptesDe(hote)[0].login);
        const jeton = donnerJeton(hote, login, c.portee);
        if (gh) { json(res, 200, { access_token: jeton, token_type: 'bearer', scope: c.portee }); return; }
        const rafraichir = secret(`glr-${aleatoire(24)}`);
        etat.jetons.get(jeton).renouvellement = rafraichir;
        etat.renouvellements.set(rafraichir, { hote, login, client: c.client, portee: c.portee });
        json(res, 200, { access_token: jeton, token_type: 'Bearer', expires_in: 7200, refresh_token: rafraichir, scope: c.portee, created_at: Math.floor(Date.now() / 1000) });
      }
      return;
    }
    res.writeHead(404, { 'Content-Type': 'text/html' });
    res.end('<html><body>Not Found</body></html>');
  }

  // ---------------------------------------------------------------- the REST APIs

  function apiGithub(req, res, chemin, corps) {
    const p = porteur(req, 'api.github.com');
    if (!p) { json(res, 401, { message: 'Bad credentials', documentation_url: 'https://docs.github.com/rest' }); return; }
    const avatar = (c) => `https://avatars.githubusercontent.com/u/${c.id}?v=4`;
    const vu = (c) => ({ login: c.login, id: c.id, name: c.name, type: c.type, avatar_url: avatar(c) });
    let m;
    if (req.method === 'GET' && chemin === '/user') {
      if (etat.bavard) { json(res, 500, { message: `Internal error while reading the account of ${p.jeton}` }); return; }
      if (etat.redirige) { res.writeHead(302, { Location: `http://127.0.0.1:${port}/ailleurs/user` }); res.end(); return; }
      json(res, 200, vu(p.compte), { 'X-OAuth-Scopes': p.portee || '' });
      return;
    }
    if (req.method === 'POST' && chemin === '/user/repos') {
      const d = JSON.parse(corps || '{}');
      const cle = `github.com/${p.compte.login}/${d.name}`;
      if (etat.depots.has(cle)) { json(res, 422, { message: 'Repository creation failed.', errors: [{ resource: 'Repository', code: 'custom', field: 'name', message: 'name already exists on this account' }] }); return; }
      etat.depots.set(cle, { hote: 'github.com', chemin: `${p.compte.login}/${d.name}`, prive: d.private !== false, proprietaire: p.compte.login, membres: new Set() });
      json(res, 201, { name: d.name, full_name: `${p.compte.login}/${d.name}`, private: d.private !== false,
        html_url: `https://github.com/${p.compte.login}/${d.name}`, clone_url: `https://github.com/${p.compte.login}/${d.name}.git` });
      return;
    }
    if (req.method === 'GET' && (m = /^\/users\/([^/]+)$/.exec(chemin))) {
      const c = comptesDe('github.com').find((x) => x.login.toLowerCase() === decodeURIComponent(m[1]).toLowerCase());
      if (!c) { json(res, 404, { message: 'Not Found' }); return; }
      json(res, 200, vu(c));
      return;
    }
    if (req.method === 'PUT' && (m = /^\/repos\/([^/]+)\/([^/]+)\/collaborators\/([^/]+)$/.exec(chemin))) {
      const d = etat.depots.get(`github.com/${decodeURIComponent(m[1])}/${decodeURIComponent(m[2])}`);
      const qui = decodeURIComponent(m[3]);
      if (!d || d.proprietaire !== p.compte.login) { json(res, 404, { message: 'Not Found' }); return; }
      if (d.membres.has(qui)) { vide(res, 204); return; }
      d.membres.add(qui);
      json(res, 201, { id: 1, invitee: { login: qui }, permissions: (JSON.parse(corps || '{}').permission === 'push' ? 'write' : 'read') });
      return;
    }
    json(res, 404, { message: 'Not Found' });
  }

  function apiGitlab(req, res, hote, chemin, corps) {
    const p = porteur(req, hote);
    if (!p) { json(res, 401, { message: '401 Unauthorized' }); return; }
    // (a token for git only, as Git Credential Manager's: write_repository)
    if (p.portee === 'write_repository') { json(res, 403, { error: 'insufficient_scope', error_description: 'The request requires higher privileges than provided by the access token.', scope: 'api read_api read_user' }); return; }
    const u = new URL(chemin, 'http://x');
    let m;
    if (req.method === 'GET' && u.pathname === '/api/v4/user') {
      json(res, 200, { id: p.compte.id, username: p.compte.login, name: p.compte.name, state: 'active', avatar_url: `https://${hote}/uploads/-/system/user/avatar/${p.compte.id}/avatar.png` });
      return;
    }
    if (req.method === 'POST' && u.pathname === '/api/v4/projects') {
      const d = JSON.parse(corps || '{}');
      const cle = `${hote}/${p.compte.login}/${d.path}`;
      if (etat.depots.has(cle)) { json(res, 400, { message: { name: ['has already been taken'], path: ['has already been taken'] } }); return; }
      etat.depots.set(cle, { hote, chemin: `${p.compte.login}/${d.path}`, nom: d.name, prive: d.visibility === 'private', proprietaire: p.compte.login, membres: new Set() });
      json(res, 201, { id: etat.depots.size + 100, name: d.name, path: d.path, path_with_namespace: `${p.compte.login}/${d.path}`, visibility: d.visibility,
        http_url_to_repo: `https://${hote}/${p.compte.login}/${d.path}.git`, web_url: `https://${hote}/${p.compte.login}/${d.path}` });
      return;
    }
    if (req.method === 'GET' && u.pathname === '/api/v4/users') {
      const nom = (u.searchParams.get('username') || '').toLowerCase();
      json(res, 200, comptesDe(hote).filter((c) => c.login.toLowerCase() === nom).map((c) => ({ id: c.id, username: c.login, name: c.name, state: 'active' })));
      return;
    }
    if (req.method === 'POST' && (m = /^\/api\/v4\/projects\/([^/]+)\/members$/.exec(u.pathname))) {
      const d = etat.depots.get(`${hote}/${decodeURIComponent(m[1])}`);
      if (!d || d.proprietaire !== p.compte.login) { json(res, 404, { message: '404 Project Not Found' }); return; }
      const corpsJ = JSON.parse(corps || '{}');
      const c = comptesDe(hote).find((x) => x.id === corpsJ.user_id);
      if (!c) { json(res, 404, { message: '404 User Not Found' }); return; }
      if (d.membres.has(c.login)) { json(res, 409, { message: 'Member already exists' }); return; }
      d.membres.add(c.login);
      json(res, 201, { id: c.id, username: c.login, name: c.name, access_level: corpsJ.access_level });
      return;
    }
    json(res, 404, { message: '404 Not Found' });
  }

  function image(res, hote, id) {
    const c = comptesDe(hote).find((x) => String(x.id) === String(id));
    if (!c) { json(res, 404, { message: 'Not Found' }); return; }
    res.writeHead(200, { 'Content-Type': 'image/png' });
    res.end(identicon(c.login, c.couleur));
  }

  const serveur = http.createServer(async (req, res) => {
    const corps = await lireCorps(req);
    const m = /^\/([^/]+)(\/.*)?$/.exec(req.url);
    const hote = m ? m[1] : '';
    const chemin = (m && m[2]) || '/';
    etat.requetes.push({ hote, methode: req.method, chemin, authorization: req.headers.authorization || '', corps });
    try {
      if (hote === 'avatars.githubusercontent.com') { image(res, 'github.com', (/^\/u\/(\d+)/.exec(chemin) || [])[1]); return; }
      if (hote === 'api.github.com') { apiGithub(req, res, chemin.split('?')[0], corps); return; }
      if (/^\/uploads\/-\/system\/user\/avatar\/(\d+)\//.test(chemin)) { image(res, hote, /avatar\/(\d+)\//.exec(chemin)[1]); return; }
      if (hote !== 'github.com' && chemin.startsWith('/api/v4/')) { apiGitlab(req, res, hote, chemin, corps); return; }
      await oauth(req, res, hote, chemin.split('?')[0], corps);
    } catch (e) {
      json(res, 500, { message: e.message });
    }
  });
  await new Promise((ok) => serveur.listen(0, '127.0.0.1', ok));
  port = serveur.address().port;
  // the stand-in's address for a real one, or null for a server it doesn't answer for
  const rediriger = (url) => {
    const u = new URL(String(url));
    if (u.protocol !== 'https:' || !HOTES.includes(u.host)) return null;
    return `http://127.0.0.1:${port}/${u.host}${u.pathname}${u.search}`;
  };

  return {
    port, etat, identicon, rediriger,
    // a fetch (Node's, or Electron's net.fetch) that asks the stand-in instead, and nothing else
    fetch: (reel) => async (url, init) => {
      const adresse = rediriger(url);
      if (!adresse) { etat.horsListe.push(String(url)); throw new TypeError(`fetch failed (not asked: ${String(url).slice(0, 40)})`); }
      if (etat.coupes.has(new URL(String(url)).host)) throw new TypeError('fetch failed', { cause: { code: 'ENOTFOUND' } });
      return reel(adresse, init);
    },
    // a sign-in approved or refused on the server's page, by its code
    approuver(code, login = null) { for (const c of etat.codes.values()) if (c.code === code) { c.accorde = true; if (login) c.login = login; } },
    refuser(code) { for (const c of etat.codes.values()) if (c.code === code) c.refuse = true; },
    // a token as the server would give it (a token made by hand, or git's), with its scopes
    jeton: (hote, login, portee = hote === 'github.com' ? 'repo' : 'api read_user write_repository') => donnerJeton(hote, login, portee),
    revoquer: (jeton) => etat.jetons.delete(jeton),
    fermer: () => new Promise((ok) => serveur.close(ok)),
  };
}

module.exports = { serveurComptes, identicon, HOTES };
