// Signing in to GitHub and GitLab, for a project's versions (versions.js) and for sharing it, so that
// people who have never used git needn't make a token:
// - the OAuth 2.0 device flow (RFC 8628): Nuée asks the server for a short code and shows it; the
//   person opens the server's page, types the code there and approves Nuée; meanwhile Nuée asks
//   every few seconds whether it was approved, then receives a token. Only the application's public
//   client id is needed (comptes_ids.js; a self-hosted GitLab's, from the settings), no secret.
//   GitHub: github.com/login/device/code, then /login/oauth/access_token. GitLab (gitlab.com, or a
//   self-hosted GitLab of 17.3 or later whose administrator registered Nuée): /oauth/authorize_device,
//   then /oauth/token; its tokens last two hours and are renewed with their refresh token;
// - this computer's git sign-in, `git credential fill` for the server, when git is on the PATH (Git
//   Credential Manager signs in through the browser), never waiting on a terminal prompt;
// - a token given by hand, the advanced way (a self-hosted GitLab where Nuée isn't registered).
// Once signed in: the account (user name, name, picture), a repository created (private, named
// after the project), a colleague invited (a GitHub collaborator; a GitLab project member as
// Developer), signing out.
// A token goes into the versions layer's store through its jeton(hote, jeton) (encrypted by the
// system; versions.js reads it back for git), a refresh token into a store of the same kind, what is
// known of each account (never a secret) into the settings. A token, a refresh token or a device
// code is never logged, never put in an address, never handed back, and blanked from every message.
// main.js gives net.fetch (the system's proxy and certificates), the stores, the settings and
// shell.openExternal; tests/essai_comptes.js gives stand-ins.
const fs = require('fs');
const path = require('path');
const { spawn, execFile, execFileSync } = require('child_process');
const IDS = require('./comptes_ids');

// An error meant for the user, its message in their language; `code` says which, for the interface
class Refus extends Error {
  constructor(message, code) {
    super(message);
    if (code) this.code = code;
  }
}

const fournisseur = (hote) => (hote === 'github.com' ? 'github' : 'gitlab');

// Where each server answers, and what Nuée asks for: GitHub's `repo` (private repositories, their
// collaborators); GitLab's `api` (creating a project, adding a member), `read_user` (the account)
// and `write_repository` (sending versions over HTTPS)
function adresses(hote) {
  return fournisseur(hote) === 'github'
    ? { code: 'https://github.com/login/device/code', jeton: 'https://github.com/login/oauth/access_token', api: 'https://api.github.com', portee: 'repo' }
    : { code: `https://${hote}/oauth/authorize_device`, jeton: `https://${hote}/oauth/token`, api: `https://${hote}/api/v4`, portee: 'api read_user write_repository' };
}

// A client id as GitHub (Ov23li…, Iv1.…) and GitLab (64 hex digits) give them; a placeholder such
// as "<to fill>" isn't one
const idValide = (id) => typeof id === 'string' && /^[\w.-]{6,200}$/.test(id);

// git on the PATH, or null. macOS's /usr/bin/git counts only once the command line tools are
// installed: run without them, it offers to install them.
function trouverGit() {
  const exe = process.platform === 'win32' ? 'git.exe' : 'git';
  for (const d of String(process.env.PATH || '').split(path.delimiter).filter(Boolean)) {
    const c = path.join(d.replace(/^"(.*)"$/, '$1'), exe);
    try { if (!fs.statSync(c).isFile()) continue; } catch { continue; }
    if (process.platform === 'darwin' && c === '/usr/bin/git' && !outilsMac()) continue;
    return c;
  }
  return null;
}
let outilsMacPresents;
function outilsMac() {
  if (outilsMacPresents === undefined) {
    try {
      const d = execFileSync('xcode-select', ['-p'], { encoding: 'utf8', timeout: 5000, stdio: ['ignore', 'pipe', 'ignore'] }).trim();
      outilsMacPresents = !!d && fs.existsSync(path.join(d, 'usr', 'bin', 'git'));
    } catch { outilsMacPresents = false; }
  }
  return outilsMacPresents;
}

// A process and the ones it started (Git Credential Manager's window, under git) stopped
function arreterArbre(p) {
  if (p.exitCode !== null || p.signalCode !== null) return;
  if (process.platform === 'win32') execFile('taskkill', ['/pid', String(p.pid), '/T', '/F'], { windowsHide: true }, () => {});
  else { try { process.kill(-p.pid, 'SIGTERM'); } catch { p.kill('SIGTERM'); } }
}

// A repository's name on GitHub and its path on GitLab, from the project's name: letters without
// their accents, digits, ".", "_" and "-" (a run of anything else, or of those, becomes one "-"),
// neither starting nor ending with one, 100 characters at most
function nomDepot(nom) {
  const s = String(nom == null ? '' : nom).replace(/\.nuee$/i, '').normalize('NFD').replace(/[̀-ͯ]/g, '')
    .replace(/œ/g, 'oe').replace(/Œ/g, 'OE').replace(/æ/g, 'ae').replace(/Æ/g, 'AE').replace(/ß/g, 'ss')
    .replace(/[^A-Za-z0-9._-]+/g, '-').replace(/[._-]{2,}/g, '-').replace(/^[._-]+/, '').slice(0, 100).replace(/[._-]+$/, '');
  if (!s) return 'nuee';
  return /\.(git|atom)$/i.test(s) ? `${s}-nuee` : s;
}
// A GitLab project's name: letters, digits, emoji, "_", ".", "+", "-" and spaces, starting with a
// letter, a digit, an emoji or "_"
function titreGitlab(nom) {
  return String(nom == null ? '' : nom).replace(/\.nuee$/i, '').replace(/[^\p{L}\p{N}\p{Extended_Pictographic}_.+\- ]+/gu, ' ')
    .replace(/\s+/g, ' ').trim().replace(/^[^\p{L}\p{N}\p{Extended_Pictographic}_]+/u, '').slice(0, 250).trim();
}

// The functions of window.bureau.comptes (preload.js, main.js), and `preparer`, which main.js calls
// once the application is ready.
//   fetch: (url, init) → Response (main.js: net.fetch)
//   jeton: (hote, jeton) → stores the token for the server ('' forgets it): the versions layer's
//   jetons: the versions layer's store (magasinJetons), read here: lire, present, connus, seulementSession
//   renouvellements: a store of the same kind for refresh tokens: lire, ecrire, connus
//   reglages: { lire: () → { comptes, serveurs }, ecrire(objet) }, kept with the settings
//   ouvrirNavigateur: (url) → the page opened in the browser (main.js: shell.openExternal)
//   langue: () → 'fr' | 'en', the interface's
//   ids: the client ids (comptes_ids.js); maintenant, pause, delaiGit, delaiGitDiscret: for the checks
function creerComptes({ fetch, jeton: ecrireJeton, jetons, renouvellements, reglages, ouvrirNavigateur, langue = () => 'en', ids = IDS,
  maintenant = Date.now, pause = (ms) => new Promise((ok) => setTimeout(ok, ms)), delaiGit = 180000, delaiGitDiscret = 20000 }) {
  const t = (fr, en) => (langue() === 'fr' ? fr : en);
  const refus = (fr, en, code) => new Refus(t(fr, en), code);
  const service = (h) => (h === 'github.com' ? 'GitHub' : h === 'gitlab.com' ? 'GitLab' : h);

  // ------------------------------------------------------------------ what is kept

  const lus = () => { const r = reglages.lire(); return r && typeof r === 'object' ? r : {}; };
  const propre = (o, cle) => (o && typeof o === 'object' && Object.hasOwn(o, cle) ? o[cle] : null);
  // What is known of the account signed in on each server: { source, login, nom, expire (the
  // token's end, ms), id (the client id that renews it) }
  const note = (h) => { const n = propre(lus().comptes, h); return n && typeof n === 'object' ? n : null; };
  function noter(h, valeur) {
    const r = lus();
    const comptes = { ...(r.comptes && typeof r.comptes === 'object' ? r.comptes : {}) };
    if (valeur) comptes[h] = valeur; else delete comptes[h];
    reglages.ecrire({ ...r, comptes });
  }

  // The client id for a server: Nuée's own (comptes_ids.js) for github.com and gitlab.com, the
  // settings' for a self-hosted GitLab; null when there is none yet
  function idClient(h) {
    const id = Object.hasOwn(ids, h) ? ids[h] : propre(lus().serveurs, h);
    return idValide(id) ? id : null;
  }

  // Secrets seen in this run but maybe not stored (a device code, a token being checked): blanked
  // from messages too (the last hundred)
  const vus = new Set();
  const retenir = (s) => { if (s) { vus.add(String(s)); if (vus.size > 100) vus.delete(vus.values().next().value); } return s; };
  async function sansSecret(message) {
    let m = String(message);
    const secrets = [...vus, ...await jetons.connus().catch(() => []), ...await renouvellements.connus().catch(() => [])].filter((s) => s && s.length >= 4);
    for (const j of secrets) {
      const formes = [j, encodeURIComponent(j), Buffer.from(j).toString('base64'),
        ...['x-access-token', 'oauth2'].map((u) => Buffer.from(`${u}:${j}`).toString('base64'))];
      for (const f of formes) m = m.split(f).join('•••');
    }
    return m;
  }

  function exigerHote(hote) {
    const h = String(hote == null ? '' : hote).trim().toLowerCase();
    if (!/^[a-z0-9]([a-z0-9.-]*[a-z0-9])?(:\d{1,5})?$/.test(h)) throw refus('Donnez le nom du serveur, comme github.com.', 'Give the server’s name, such as github.com.', 'serveur');
    return h;
  }

  // ------------------------------------------------------------------ HTTP

  // A request: { statut, json (null when the answer isn't JSON), entetes }. No cookie; with a token,
  // no redirect followed (it would carry the token elsewhere). A network error or 30 s without an
  // answer: a refusal saying so.
  async function demander(url, { methode = 'GET', entetes = {}, corps, suivre = false } = {}) {
    let r;
    try {
      r = await fetch(url, { method: methode, headers: entetes, body: corps, credentials: 'omit', redirect: suivre ? 'follow' : 'error', signal: AbortSignal.timeout(30000) });
    } catch (e) {
      const h = new URL(url).host;
      if (!suivre && /redirect/i.test(`${e && e.message} ${e && e.cause && e.cause.message}`)) {
        throw refus(`${h} renvoie ailleurs : Nuée ne suit pas une redirection avec la connexion.`, `${h} sends elsewhere: Nuée doesn’t follow a redirect with the sign-in.`, 'redirection');
      }
      const quoi = (/net::ERR_[A-Z_0-9]+/.exec(String(e && e.message)) || [])[0] || (e && e.cause && e.cause.code)
        || (e && e.name === 'TimeoutError' ? t('pas de réponse en 30 s', 'no answer in 30 s') : 'fetch failed');
      throw refus(`Impossible de joindre ${h} (${quoi}). Vérifiez la connexion, ou le proxy.`, `Couldn’t reach ${h} (${quoi}). Check the connection, or the proxy.`, 'reseau');
    }
    const texte = await r.text().catch(() => '');
    let json = null;
    try { json = texte ? JSON.parse(texte) : null; } catch { /* an HTML page */ }
    return { statut: r.status, json, entetes: r.headers };
  }

  // The OAuth endpoints: a form, the answer in JSON (GitHub answers a form otherwise)
  const oauth = (url, champs) => demander(url, { methode: 'POST', corps: new URLSearchParams(champs).toString(),
    entetes: { Accept: 'application/json', 'Content-Type': 'application/x-www-form-urlencoded' } });

  // The server's REST API with the token
  function api(h, jeton, methode, chemin, corps) {
    const gh = fournisseur(h) === 'github';
    return demander(`${adresses(h).api}${chemin}`, { methode, corps: corps ? JSON.stringify(corps) : undefined,
      entetes: { Accept: gh ? 'application/vnd.github+json' : 'application/json', Authorization: `Bearer ${jeton}`,
        ...(gh ? { 'X-GitHub-Api-Version': '2022-11-28' } : {}), ...(corps ? { 'Content-Type': 'application/json' } : {}) } });
  }

  // What the server said, in words (`r`: demander's answer; `json.message` may be GitLab's object)
  function detail(r) {
    const j = r.json && typeof r.json === 'object' ? r.json : {};
    const m = j.error_description || j.message || j.error || '';
    const erreurs = Array.isArray(j.errors) ? j.errors.map((x) => (x && (x.message || x.code)) || '').filter(Boolean) : [];
    return [typeof m === 'string' ? m : JSON.stringify(m), ...erreurs].filter(Boolean).join(' — ').slice(0, 300);
  }
  const repondu = (h, r) => {
    const d = detail(r);
    return refus(`${h} a répondu : ${r.statut}${d ? ` ${d}` : ''}`, `${h} answered: ${r.statut}${d ? ` ${d}` : ''}`);
  };

  // The device flow's refusals: Nuée's application not set up there, or the server without it
  function refusOauth(h, r, etape) {
    const e = r.json && typeof r.json.error === 'string' ? r.json.error : '';
    if (e === 'device_flow_disabled') {
      return refus('La connexion par code n’est pas activée pour l’application Nuée sur GitHub (son réglage « Enable Device Flow »).',
        'Signing in with a code isn’t enabled for Nuée’s application on GitHub (its “Enable Device Flow” setting).', 'non_regle');
    }
    if (['incorrect_client_credentials', 'invalid_client', 'unauthorized_client'].includes(e) || (etape === 'code' && r.statut === 401)
      || (etape === 'code' && fournisseur(h) === 'github' && r.statut === 404)) {
      return fournisseur(h) === 'github' || h === 'gitlab.com'
        ? refus(`${h} ne reconnaît pas l’application Nuée (son identifiant, dans comptes_ids.js).`, `${h} doesn’t recognise Nuée’s application (its id, in comptes_ids.js).`, 'non_regle')
        : refus(`${h} ne reconnaît pas l’identifiant d’application donné, ou l’application est confidentielle : son administrateur doit enregistrer Nuée comme application non confidentielle.`,
          `${h} doesn’t recognise the application ID given, or the application is confidential: its administrator must register Nuée as a non-confidential application.`, 'non_regle');
    }
    if (e === 'invalid_scope') {
      return refus(`L’application Nuée sur ${h} n’a pas les droits que Nuée demande (${adresses(h).portee}).`,
        `Nuée’s application on ${h} lacks the scopes Nuée asks for (${adresses(h).portee}).`, 'non_regle');
    }
    if (etape === 'code' && fournisseur(h) === 'gitlab' && (r.statut === 404 || !r.json)) {
      return refus(`${h} ne propose pas la connexion par code (il faut GitLab 17.3 ou plus récent) : utilisez un jeton.`,
        `${h} doesn’t offer signing in with a code (it takes GitLab 17.3 or later): use a token instead.`, 'non_disponible');
    }
    return repondu(h, r);
  }

  // The REST API's refusals, for an action: 'creer' (a repository) or 'inviter'
  function refusApi(h, r, action) {
    const N = service(h);
    const j = r.json && typeof r.json === 'object' ? r.json : {};
    if (r.statut === 401) return refus(`${N} a refusé la connexion : connectez-vous à nouveau.`, `${N} refused the sign-in: sign in again.`, 'refuse');
    if (r.statut === 429 || (r.statut === 403 && r.entetes && r.entetes.get('x-ratelimit-remaining') === '0')) {
      return refus(`${N} limite le nombre de demandes : réessayez dans quelques minutes.`, `${N} limits the number of requests: try again in a few minutes.`, 'limite');
    }
    if (r.statut === 403 && action === 'creer') {
      return refus(`Cette connexion ne permet pas de créer un dépôt sur ${N} (le jeton n’en a pas le droit, ou le compte a atteint sa limite de projets).`,
        `This sign-in may not create repositories on ${N} (the token lacks the right, or the account has reached its project limit).`, 'droits');
    }
    if (r.statut === 403 && j.error === 'insufficient_scope') {
      return refus(`Cette connexion ne permet pas d’inviter sur ${N} : connectez-vous avec ${N} plutôt.`, `This sign-in may not invite people on ${N}: sign in with ${N} instead.`, 'droits');
    }
    if (r.statut === 403) {
      return refus('Vous ne pouvez pas inviter dans ce dépôt : il faut en être le propriétaire ou l’administrer (Maintainer sur GitLab).',
        'You may not invite people to this repository: it takes its owner or an administrator (a Maintainer on GitLab).', 'droits');
    }
    if (r.statut === 404) return refus(`Aucun dépôt à cette adresse sur ${N}, ou ce compte ne peut pas le voir.`, `No repository at this address on ${N}, or this account can’t see it.`, 'introuvable');
    return repondu(h, r);
  }

  // ------------------------------------------------------------------ the account

  // The account a token opens: { statut: 'ok', compte: { login, nom, avatar (its address) } },
  // { statut: 'refuse' } (401), or { statut: 'limite' } (a token that may use git but not read the
  // account: GitLab's write_repository alone, as Git Credential Manager asks for)
  async function utilisateur(h, jeton) {
    retenir(jeton);
    const r = await api(h, jeton, 'GET', '/user');
    const j = r.json && typeof r.json === 'object' ? r.json : null;
    const login = j && (fournisseur(h) === 'github' ? j.login : j.username);
    if (r.statut === 200 && typeof login === 'string' && login) {
      return { statut: 'ok', compte: { login, nom: typeof j.name === 'string' && j.name.trim() ? j.name.trim() : null, avatar: typeof j.avatar_url === 'string' ? j.avatar_url : null } };
    }
    if (r.statut === 401) return { statut: 'refuse' };
    if (r.statut === 403 && j && j.error === 'insufficient_scope') return { statut: 'limite' };
    throw repondu(h, r);
  }

  // An account's picture as a data: URL (PNG, JPEG, GIF or WebP, 1 MB at most), which the page shows
  // without asking another site (its security policy); null when there is none. Never sent a token.
  const avatars = new Map();  // server → { url, donnees }
  async function avatar(h, url) {
    let u;
    try { u = new URL(url); } catch { return null; }
    if (u.protocol !== 'https:') return null;
    if (/(^|\.)githubusercontent\.com$/.test(u.hostname) && !u.searchParams.has('s')) u.searchParams.set('s', '96');
    const memo = avatars.get(h);
    if (memo && memo.url === u.href) return memo.donnees;
    try {
      const r = await fetch(u.href, { credentials: 'omit', redirect: 'follow', signal: AbortSignal.timeout(15000) });
      const type = String(r.headers.get('content-type') || '').split(';')[0].trim().toLowerCase();
      if (!r.ok || !['image/png', 'image/jpeg', 'image/gif', 'image/webp'].includes(type)) return null;
      const octets = Buffer.from(await r.arrayBuffer());
      if (!octets.length || octets.length > 1024 * 1024) return null;
      const donnees = `data:${type};base64,${octets.toString('base64')}`;
      avatars.set(h, { url: u.href, donnees });
      return donnees;
    } catch { return null; }
  }

  // What compte() and the sign-ins give the interface: never a token
  async function compteDe(h, u, source) {
    return { connecte: true, login: u.login, nom: u.nom, avatar: u.avatar ? await avatar(h, u.avatar) : null, source,
      ...(await jetons.seulementSession() ? { session: true } : {}) };
  }
  const limite = (login, source) => ({ connecte: true, login: login || null, nom: null, avatar: null, source, limite: true });
  const absent = () => ({ connecte: false, login: null, nom: null, avatar: null, source: null });

  // The token kept for a server, renewed first when it ends within five minutes and a refresh token
  // is kept (a GitLab sign-in lasts two hours); null when none is kept
  async function jetonFrais(h, { force = false } = {}) {
    const j = await jetons.lire(h);
    if (!j) return null;
    const n = note(h);
    if (n && n.source === 'connexion' && n.expire && (force || maintenant() > n.expire - 5 * 60000)) return (await renouveler(h)) || j;
    return j;
  }

  // A refresh token exchanged for a new token (and a new refresh token, GitLab's way); null when it
  // can't be (offline: tried again later; refused: the sign-in is over, its refresh token forgotten)
  const renouvelant = new Map();
  function renouveler(h) {
    if (renouvelant.has(h)) return renouvelant.get(h);
    const p = (async () => {
      const n = note(h);
      const rafraichir = await renouvellements.lire(h);
      const id = (n && n.id) || idClient(h);
      if (!n || !rafraichir || !id) return null;
      let r;
      try { r = await oauth(adresses(h).jeton, { grant_type: 'refresh_token', refresh_token: rafraichir, client_id: id }); } catch { return null; }
      const j = r.json || {};
      if (!j.access_token) {
        if (r.statut >= 400 && r.statut < 500) await renouvellements.ecrire(h, '');
        return null;
      }
      const nouveau = retenir(String(j.access_token));
      await ecrireJeton(h, nouveau);
      if (j.refresh_token) await renouvellements.ecrire(h, retenir(String(j.refresh_token)));
      noter(h, { ...n, expire: Number(j.expires_in) > 0 ? maintenant() + 1000 * Number(j.expires_in) : null });
      planifier(h);
      return nouveau;
    })();
    renouvelant.set(h, p);
    return p.finally(() => renouvelant.delete(h));
  }

  // While Nuée runs, a token that expires is renewed five minutes before, so that sending and
  // receiving versions (which read it from the store) always find a valid one
  const minuteries = new Map();
  function planifier(h) {
    clearTimeout(minuteries.get(h));
    minuteries.delete(h);
    const n = note(h);
    if (!n || n.source !== 'connexion' || !n.expire) return;
    const dans = Math.max(1000, n.expire - 5 * 60000 - maintenant());
    if (dans > 2 ** 31 - 1) return;
    const m = setTimeout(() => { minuteries.delete(h); jetonFrais(h).catch(() => {}); }, dans);
    if (m.unref) m.unref();
    minuteries.set(h, m);
  }

  // A token kept, and what is known of its account
  async function garder(h, jeton, { source, login = null, nom = null, rafraichir = null, expire = null, id = null }) {
    await ecrireJeton(h, jeton);
    await renouvellements.ecrire(h, rafraichir ? retenir(String(rafraichir)) : '');
    noter(h, { source, login, nom, expire: rafraichir && expire ? expire : null, ...(rafraichir ? { id } : {}) });
    avatars.delete(h);
    planifier(h);
  }

  async function exigerJeton(h) {
    const j = await jetonFrais(h);
    if (!j) throw refus(`Connectez-vous d’abord à ${service(h)}.`, `Sign in to ${service(h)} first.`, 'connexion');
    return j;
  }

  // ------------------------------------------------------------------ signing in with a code

  const enCours = new Map();  // server → the sign-in under way
  const gitEnCours = new Map();  // server → stops its `git credential fill`

  function annuler(h) {
    const s = enCours.get(h);
    if (s) { s.annule = true; s.reveiller(); enCours.delete(h); }
    const g = gitEnCours.get(h);
    if (g) g();
  }

  // The server's page for typing the code: https, on the server itself (null otherwise)
  function pageDe(h, adresse) {
    try {
      const u = new URL(String(adresse));
      return u.protocol === 'https:' && u.host.toLowerCase() === h ? u.href : null;
    } catch { return null; }
  }

  function pasRegle(h) {
    if (Object.hasOwn(ids, h)) {
      return refus(`La connexion à ${service(h)} n’est pas encore prête dans cette copie de Nuée : utilisez un jeton.`,
        `Signing in with ${service(h)} isn’t set up in this copy of Nuée yet: use a token instead.`, 'non_regle');
    }
    return refus(`Nuée n’est pas enregistré sur ${h} : demandez à son administrateur de l’enregistrer (il vous donnera un identifiant d’application), ou utilisez un jeton.`,
      `Nuée isn’t registered on ${h}: ask its administrator to register it (they will give you an application ID), or use a token instead.`, 'non_regle');
  }

  async function commencerConnexion(hote) {
    const h = exigerHote(hote);
    const id = idClient(h);
    if (!id) throw pasRegle(h);
    annuler(h);  // (one sign-in at a time for a server: a new code replaces the old)
    const r = await oauth(adresses(h).code, { client_id: id, scope: adresses(h).portee });
    const j = r.json || {};
    if (r.statut !== 200 || typeof j.device_code !== 'string' || typeof j.user_code !== 'string' || j.error) throw refusOauth(h, r, 'code');
    retenir(j.device_code);
    const adresse = pageDe(h, j.verification_uri);
    if (!adresse) throw refus(`La page de connexion donnée par ${h} n’est pas une page https de ${h}.`, `The sign-in page ${h} gave isn’t an https page of ${h}.`);
    let reveiller;
    const s = { appareil: j.device_code, code: j.user_code, adresse, page: pageDe(h, j.verification_uri_complete) || adresse, id,
      intervalle: 1000 * Math.max(1, Number(j.interval) || 5), fin: maintenant() + 1000 * (Number(j.expires_in) > 0 ? Number(j.expires_in) : 900),
      annule: false, reveil: new Promise((ok) => { reveiller = ok; }) };
    s.reveiller = reveiller;
    enCours.set(h, s);
    return { code: s.code, adresse: s.adresse, expire: s.fin };
  }

  // The page of the sign-in under way opened in the browser (GitLab's with the code filled in)
  async function ouvrirPage(hote) {
    const h = exigerHote(hote);
    const s = enCours.get(h);
    if (!s) throw refus(`Aucune connexion n’est en cours pour ${h} : recommencez.`, `No sign-in is under way for ${h}: start again.`, 'aucune');
    if (!/^https:\/\//.test(s.page)) throw refus('Nuée n’ouvre que des pages https.', 'Nuée opens https pages only.');
    await ouvrirNavigateur(s.page);
  }

  // Until the code is approved (→ { ok: true, compte }), refused, expired or the sign-in cancelled
  // (→ { ok: false, raison: 'refusee' | 'expiree' | 'annulee' }); the server asked no faster than it
  // says (5 s, then 5 s more at each slow_down). Asked twice, the same wait.
  function attendreConnexion(hote) {
    const h = exigerHote(hote);
    const s = enCours.get(h);
    if (!s) return Promise.reject(refus(`Aucune connexion n’est en cours pour ${h} : recommencez.`, `No sign-in is under way for ${h}: start again.`, 'aucune'));
    if (!s.attente) s.attente = sonder(h, s).finally(() => { if (enCours.get(h) === s) enCours.delete(h); });
    return s.attente;
  }

  async function sonder(h, s) {
    let echecsReseau = 0;
    for (;;) {
      await Promise.race([pause(s.intervalle), s.reveil]);
      if (s.annule) return { ok: false, raison: 'annulee' };
      if (maintenant() >= s.fin) return { ok: false, raison: 'expiree' };
      let r;
      try {
        r = await oauth(adresses(h).jeton, { client_id: s.id, device_code: s.appareil, grant_type: 'urn:ietf:params:oauth:grant-type:device_code' });
        echecsReseau = 0;
      } catch (e) {
        // (the network may come back: three failures in a row end the wait)
        echecsReseau += 1;
        if (e.code !== 'reseau' || echecsReseau >= 3) throw e;
        continue;
      }
      if (s.annule) return { ok: false, raison: 'annulee' };
      const j = r.json || {};
      if (typeof j.access_token === 'string' && j.access_token) return { ok: true, compte: await connexionAccordee(h, j, s.id) };
      const erreur = typeof j.error === 'string' ? j.error : '';
      if (erreur === 'authorization_pending') continue;
      if (erreur === 'slow_down') { s.intervalle = Math.max(s.intervalle + 5000, 1000 * (Number(j.interval) || 0)); continue; }
      if (erreur === 'access_denied') return { ok: false, raison: 'refusee' };
      if (erreur === 'expired_token') return { ok: false, raison: 'expiree' };
      throw refusOauth(h, r, 'jeton');
    }
  }

  // The token received: its account read with it, then kept
  async function connexionAccordee(h, j, id) {
    const jeton = retenir(String(j.access_token));
    if (j.refresh_token) retenir(String(j.refresh_token));
    const u = await utilisateur(h, jeton);
    if (u.statut === 'refuse') throw refus(`${h} refuse le jeton qu’il vient de donner : recommencez.`, `${h} refuses the token it has just given: start again.`, 'refuse');
    const c = u.statut === 'ok' ? u.compte : { login: null, nom: null };
    await garder(h, jeton, { source: 'connexion', login: c.login, nom: c.nom, rafraichir: j.refresh_token || null,
      expire: Number(j.expires_in) > 0 ? maintenant() + 1000 * Number(j.expires_in) : null, id });
    return u.statut === 'ok' ? compteDe(h, u.compte, 'connexion') : limite(null, 'connexion');
  }

  async function annulerConnexion(hote) {
    annuler(exigerHote(hote));
  }

  // ------------------------------------------------------------------ this computer's git

  // What this computer's git gives for the server (`git credential fill`: its credential helpers;
  // Git Credential Manager signing in through the browser when `interactif`): { jeton, utilisateur },
  // or { raison: 'absent' | 'aucun' | 'delai' | 'annulee' }. Never a terminal prompt (git's, or an
  // askpass program's), and stopped after `delaiGit`. Git's own words are never shown.
  function remplirGit(h, { interactif }) {
    const programme = trouverGit();
    if (!programme) return Promise.resolve({ raison: 'absent' });
    return new Promise((ok) => {
      const env = Object.fromEntries(Object.entries(process.env).filter(([k]) => !/^(GIT_ASKPASS|SSH_ASKPASS|GIT_TERMINAL_PROMPT|GCM_INTERACTIVE)$/i.test(k)));
      env.GIT_TERMINAL_PROMPT = '0';
      if (!interactif) env.GCM_INTERACTIVE = 'never';
      let p;
      try {
        p = spawn(programme, ['credential', 'fill'], { env, windowsHide: true, stdio: ['pipe', 'pipe', 'ignore'], detached: process.platform !== 'win32' });
      } catch { ok({ raison: 'absent' }); return; }
      let sortie = '';
      let fini = false;
      const finir = (r) => {
        if (fini) return;
        fini = true;
        clearTimeout(minuterie);
        if (gitEnCours.get(h) === arreter) gitEnCours.delete(h);
        ok(r);
      };
      const arreter = () => { arreterArbre(p); finir({ raison: 'annulee' }); };
      const minuterie = setTimeout(() => { arreterArbre(p); finir({ raison: 'delai' }); }, interactif ? delaiGit : delaiGitDiscret);
      gitEnCours.set(h, arreter);
      p.stdout.on('data', (d) => { sortie += d; if (sortie.length > 100000) { arreterArbre(p); finir({ raison: 'aucun' }); } });
      p.on('error', () => finir({ raison: 'absent' }));
      p.on('close', (code) => {
        const champs = {};
        for (const ligne of sortie.split(/\r?\n/)) {
          const i = ligne.indexOf('=');
          if (i > 0) champs[ligne.slice(0, i)] = ligne.slice(i + 1);
        }
        if (code === 0 && champs.password) finir({ jeton: retenir(champs.password), utilisateur: champs.username || '' });
        else finir({ raison: 'aucun' });
      });
      p.stdin.on('error', () => {});
      p.stdin.end(`protocol=https\nhost=${h}\n\n`);
    });
  }
  // git's user name for the account, unless it is the name a token goes with
  const nomGit = (u) => (u && !/^(oauth2|x-access-token|x-oauth-basic|personalaccesstoken|token|git)$/i.test(u) ? u : null);

  async function compteGit(hote) {
    const h = exigerHote(hote);
    const g = await remplirGit(h, { interactif: true });
    if (g.raison) return { ok: false, raison: g.raison };
    const u = await utilisateur(h, g.jeton);
    if (u.statut === 'refuse') return { ok: false, raison: 'refuse' };
    const c = u.statut === 'ok' ? u.compte : { login: nomGit(g.utilisateur), nom: null };
    await garder(h, g.jeton, { source: 'git', login: c.login, nom: c.nom });
    return { ok: true, compte: u.statut === 'ok' ? await compteDe(h, u.compte, 'git') : limite(c.login, 'git') };
  }

  // ------------------------------------------------------------------ a token given by hand

  async function utiliserJeton(hote, valeur) {
    const h = exigerHote(hote);
    const j = retenir(String(valeur == null ? '' : valeur).trim());
    if (!j || /\s/.test(j) || j.length > 1000) throw refus('Un jeton est un seul mot, tel que GitHub ou GitLab l’a donné.', 'A token is one word, as GitHub or GitLab gave it.', 'jeton');
    const u = await utilisateur(h, j);
    if (u.statut === 'refuse') throw refus(`${service(h)} refuse ce jeton : il est peut-être faux ou expiré.`, `${service(h)} refuses this token: it may be wrong or expired.`, 'refuse');
    const c = u.statut === 'ok' ? u.compte : { login: null, nom: null };
    await garder(h, j, { source: 'jeton', login: c.login, nom: c.nom });
    return u.statut === 'ok' ? compteDe(h, u.compte, 'jeton') : limite(null, 'jeton');
  }

  // ------------------------------------------------------------------ the account, the server

  // Who is signed in on the server. A token kept but refused (expired, revoked): connecte false,
  // `refuse`; the server not answering: what was known, `horsLigne`. An expiring token is renewed
  // first; one from git that is refused is asked of git again, quietly.
  async function compte(hote) {
    const h = exigerHote(hote);
    const n = note(h);
    if (!(await jetons.present(h))) {
      // (forgotten elsewhere, as by Versions' "Forget the token": so is the rest)
      if (n) { noter(h, null); await renouvellements.ecrire(h, ''); planifier(h); }
      return absent();
    }
    const source = n && ['connexion', 'git', 'jeton'].includes(n.source) ? n.source : 'jeton';
    let jeton = await jetonFrais(h);
    let u;
    try {
      u = await utilisateur(h, jeton);
      if (u.statut === 'refuse' && source === 'connexion' && n && n.expire) {
        const nouveau = await jetonFrais(h, { force: true });
        if (nouveau && nouveau !== jeton) { jeton = nouveau; u = await utilisateur(h, jeton); }
      }
      if (u.statut === 'refuse' && source === 'git') {
        const g = await remplirGit(h, { interactif: false });
        if (g.jeton && g.jeton !== jeton) {
          const v = await utilisateur(h, g.jeton);
          if (v.statut !== 'refuse') {
            u = v;
            await garder(h, g.jeton, { source: 'git', login: v.statut === 'ok' ? v.compte.login : nomGit(g.utilisateur), nom: v.statut === 'ok' ? v.compte.nom : null });
          }
        }
      }
    } catch (e) {
      if (e.code !== 'reseau') throw e;
      return { connecte: true, login: (n && n.login) || null, nom: (n && n.nom) || null, avatar: null, source, horsLigne: true };
    }
    if (u.statut === 'refuse') return { ...absent(), login: (n && n.login) || null, nom: (n && n.nom) || null, source, refuse: true };
    if (u.statut === 'limite') return limite((n && n.login) || null, source);
    const apres = note(h) || {};
    if (!n || apres.login !== u.compte.login || apres.nom !== u.compte.nom) noter(h, { ...apres, source, login: u.compte.login, nom: u.compte.nom });
    planifier(h);
    return compteDe(h, u.compte, source);
  }

  // What the sign-in dialog offers for a server: { hote, fournisseur, connexion (Nuée's application
  // is set up there), reglable (its application ID may be given: a self-hosted GitLab), git (git is
  // on the PATH) }
  async function serveur(hote) {
    const h = exigerHote(hote);
    return { hote: h, fournisseur: fournisseur(h), connexion: !!idClient(h), reglable: !Object.hasOwn(ids, h), git: !!trouverGit() };
  }

  // A self-hosted GitLab's application ID ('' forgets it), kept with the settings
  async function reglerServeur(hote, id) {
    const h = exigerHote(hote);
    if (Object.hasOwn(ids, h)) throw refus(`L’application Nuée de ${h} est fixée dans Nuée.`, `Nuée’s application on ${h} is built into Nuée.`, 'serveur');
    const v = String(id == null ? '' : id).trim();
    if (v && !idValide(v)) {
      throw refus('Un identifiant d’application est un seul mot, tel que GitLab le montre (Application ID).', 'An application ID is one word, as GitLab shows it (Application ID).', 'id');
    }
    const r = lus();
    const serveurs = { ...(r.serveurs && typeof r.serveurs === 'object' ? r.serveurs : {}) };
    if (v) serveurs[h] = v; else delete serveurs[h];
    reglages.ecrire({ ...r, serveurs });
    return serveur(h);
  }

  // ------------------------------------------------------------------ repositories, colleagues

  // A repository of the account's own, named after the project: { url (for versions.distant), page, nom };
  // a name taken refused with the code 'existe'
  async function creerDepot(hote, nom, options = {}) {
    const h = exigerHote(hote);
    const jeton = await exigerJeton(h);
    const chemin = nomDepot(nom);
    const prive = !(options && options.prive === false);
    const pris = () => refus(`Un dépôt nommé « ${chemin} » existe déjà sur votre compte ${service(h)} : choisissez un autre nom.`,
      `A repository named “${chemin}” already exists on your ${service(h)} account: choose another name.`, 'existe');
    if (fournisseur(h) === 'github') {
      const r = await api(h, jeton, 'POST', '/user/repos', { name: chemin, private: prive, auto_init: false });
      if (r.statut === 201 && r.json && typeof r.json.clone_url === 'string') return { url: r.json.clone_url, page: r.json.html_url || null, nom: r.json.name || chemin };
      if (r.statut === 422 && r.json && Array.isArray(r.json.errors) && r.json.errors.some((e) => e && e.field === 'name')) throw pris();
      throw refusApi(h, r, 'creer');
    }
    const r = await api(h, jeton, 'POST', '/projects', { name: titreGitlab(nom) || chemin, path: chemin, visibility: prive ? 'private' : 'public', initialize_with_readme: false });
    if (r.statut === 201 && r.json && typeof r.json.http_url_to_repo === 'string') return { url: r.json.http_url_to_repo, page: r.json.web_url || null, nom: r.json.path || chemin };
    if (r.statut === 400 && r.json && r.json.message && typeof r.json.message === 'object'
      && Object.values(r.json.message).some((v) => [].concat(v).some((x) => /taken|already/i.test(String(x))))) throw pris();
    throw refusApi(h, r, 'creer');
  }

  // The repository of an address on the server: its path's parts (owner and name on GitHub;
  // groups and name on GitLab), without .git
  function partiesDepot(h, url) {
    let u = null;
    try { u = new URL(String(url == null ? '' : url).trim()); } catch { /* below */ }
    let parties = u && u.protocol === 'https:' && u.host.toLowerCase() === h ? u.pathname.split('/').filter(Boolean) : [];
    const tiret = parties.indexOf('-');
    if (tiret >= 0) parties = parties.slice(0, tiret);
    if (fournisseur(h) === 'github') parties = parties.slice(0, 2);
    if (parties.length < 2) throw refus(`Donnez l’adresse du dépôt sur ${service(h)} (https://${h}/…).`, `Give the repository’s address on ${service(h)} (https://${h}/…).`, 'adresse');
    parties[parties.length - 1] = parties[parties.length - 1].replace(/\.git$/i, '');
    try { return parties.map(decodeURIComponent); } catch { return parties; }
  }

  // A colleague given access to the repository: on GitHub invited as a collaborator (they accept it:
  // 'invite'), on GitLab made a member as Developer ('ajoute'); 'deja' when they already were.
  // → { resultat, login, nom }
  async function inviter(hote, url, utilisateurInvite) {
    const h = exigerHote(hote);
    const parties = partiesDepot(h, url);
    const login = String(utilisateurInvite == null ? '' : utilisateurInvite).trim().replace(/^@/, '');
    if (!/^[A-Za-z0-9_.][A-Za-z0-9_.-]{0,254}$/.test(login)) {
      throw refus('Donnez le nom d’utilisateur de votre collègue, tel qu’il est dans l’adresse de son profil.', 'Give your colleague’s user name, as it is in their profile’s address.', 'utilisateur');
    }
    const jeton = await exigerJeton(h);
    const n = note(h);
    if (n && n.login && n.login.toLowerCase() === login.toLowerCase()) throw refus('C’est vous : donnez le nom d’un collègue.', 'That’s you: give a colleague’s name.', 'soi');
    const inconnu = () => refus(`Personne ne s’appelle « ${login} » sur ${service(h)}.`, `No one is called “${login}” on ${service(h)}.`, 'inconnu');
    if (fournisseur(h) === 'github') {
      const u = await api(h, jeton, 'GET', `/users/${encodeURIComponent(login)}`);
      if (u.statut === 404) throw inconnu();
      if (u.statut !== 200 || !u.json || typeof u.json.login !== 'string') throw refusApi(h, u, 'inviter');
      if (u.json.type === 'Organization') throw refus(`« ${login} » est une organisation sur GitHub, pas une personne.`, `“${login}” is an organization on GitHub, not a person.`, 'organisation');
      const qui = { login: u.json.login, nom: typeof u.json.name === 'string' && u.json.name ? u.json.name : null };
      const r = await api(h, jeton, 'PUT', `/repos/${parties.map(encodeURIComponent).join('/')}/collaborators/${encodeURIComponent(qui.login)}`, { permission: 'push' });
      if (r.statut === 201) return { resultat: 'invite', ...qui };
      if (r.statut === 204) return { resultat: 'deja', ...qui };
      throw refusApi(h, r, 'inviter');
    }
    const l = await api(h, jeton, 'GET', `/users?username=${encodeURIComponent(login)}`);
    if (l.statut !== 200 || !Array.isArray(l.json)) throw refusApi(h, l, 'inviter');
    const u = l.json.find((x) => x && typeof x.username === 'string' && x.username.toLowerCase() === login.toLowerCase());
    if (!u) throw inconnu();
    const qui = { login: u.username, nom: typeof u.name === 'string' && u.name ? u.name : null };
    const r = await api(h, jeton, 'POST', `/projects/${encodeURIComponent(parties.join('/'))}/members`, { user_id: u.id, access_level: 30 });
    if (r.statut === 201) return { resultat: 'ajoute', ...qui };
    if (r.statut === 409) return { resultat: 'deja', ...qui };
    throw refusApi(h, r, 'inviter');
  }

  // Signing out: Nuée forgets the token, its refresh token and the account (this computer's git keeps
  // its own sign-in; the token itself is revoked from the account's settings on the server)
  async function deconnecter(hote) {
    const h = exigerHote(hote);
    annuler(h);
    await ecrireJeton(h, '');
    await renouvellements.ecrire(h, '');
    noter(h, null);
    avatars.delete(h);
    planifier(h);
  }

  // At start-up: the renewals of the sign-ins that expire (an expired one renewed now)
  function preparer() {
    const comptes = lus().comptes;
    for (const h of Object.keys(comptes && typeof comptes === 'object' ? comptes : {})) planifier(h);
  }

  // Every error a message for the user, without any secret
  const commande = (f) => async (...args) => {
    try { return await f(...args); } catch (e) {
      const r = e instanceof Refus ? e : new Refus(String((e && e.message) || e));
      throw new Refus(await sansSecret(r.message), r.code);
    }
  };
  return {
    serveur: commande(serveur), reglerServeur: commande(reglerServeur), commencerConnexion: commande(commencerConnexion),
    ouvrirPage: commande(ouvrirPage), attendreConnexion: commande(attendreConnexion), annulerConnexion: commande(annulerConnexion),
    compteGit: commande(compteGit), utiliserJeton: commande(utiliserJeton), compte: commande(compte), creerDepot: commande(creerDepot),
    inviter: commande(inviter), deconnecter: commande(deconnecter), preparer,
  };
}

module.exports = { creerComptes, nomDepot, titreGitlab, trouverGit };
