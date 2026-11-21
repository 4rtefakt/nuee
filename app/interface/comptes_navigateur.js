// Signing in to GitHub and GitLab in the browser version, and what sharing a project needs there:
// - nuee.io's relay (vitrine/src/relais.js), for what a page can't ask github.com and gitlab.com
//   itself (git's smart HTTP, the device flow's two requests: they send no CORS headers), at the
//   page's own origin, /relais, as isomorphic-git's corsProxy addresses it:
//   https://nuee.io/relais/<host>/<path> for https://<host>/<path>. Their APIs (api.github.com,
//   gitlab.com/api/v4) and the accounts' pictures answer the page itself (its security policy:
//   construire_navigateur.js). Only github.com and gitlab.com: a self-hosted GitLab is the desktop
//   application's;
// - the tokens, in this browser's storage for the site (IndexedDB, database nuee-jetons: one store
//   for the tokens, one for GitLab's refresh tokens), for this visit only when it keeps nothing;
//   never in an address or a log; signing out removes them;
// - the desktop's app/comptes.js itself (an ES module of the site, app/comptes.js:
//   construire_navigateur.js), with them: the functions of window.bureau.comptes (preload.js) for
//   connexion.js and versions.js. What is known of each account (never a secret) is kept in
//   localStorage['nuee-comptes'].
import { t, langueActuelle } from './i18n.js';
import { ONGLET } from './secours_navigateur.js';

export const HOTES_RELAIS = ['github.com', 'gitlab.com'];
// The relay: the page's own origin's (nuee.io: https://nuee.io/relais), or the one the site's
// navigateur.js, or a check, gives (window.NUEE_RELAIS); never one an address of the page gives
export const relais = () => String(window.NUEE_RELAIS || new URL('/relais', location.origin).href).replace(/\/+$/, '');

// An address as the page asks it: github.com and gitlab.com through the relay, but GitLab's API
export function adresseRelayee(url) {
  let u;
  try { u = new URL(url); } catch { return url; }
  if (u.protocol !== 'https:' || u.port || !HOTES_RELAIS.includes(u.hostname)) return url;
  if (u.hostname === 'gitlab.com' && /^\/api\/v4(\/|$)/.test(u.pathname)) return url;
  return `${relais()}/${u.hostname}${u.pathname}${u.search}`;
}

// What the page asks itself (its security policy: construire_navigateur.js): GitHub's and GitLab's
// APIs, and the accounts' pictures on GitHub's and Gravatar's servers. A picture elsewhere (GitLab's
// own uploads, which send neither CORS nor CORP) isn't asked for: the account has none here.
const DIRECTES = /^https:\/\/(?:api\.github\.com|avatars\.githubusercontent\.com|secure\.gravatar\.com)\/|^https:\/\/gitlab\.com\/api\/v4\//;
async function fetchCompte(url, init) {
  const adresse = adresseRelayee(url);
  if (adresse === url && !DIRECTES.test(String(url))) throw new TypeError('fetch failed');
  return fetch(adresse, init);
}

// A refusal for a server the relay doesn't reach, in words
export const refusHote = (hote) => Object.assign(new Error(t('versions_relais_hote', { hote })), { code: 'bureau' });
export function exigerHoteRelais(hote) {
  const h = String(hote == null ? '' : hote).trim().toLowerCase();
  if (!HOTES_RELAIS.includes(h)) throw refusHote(h || '?');
  return h;
}

// isomorphic-git's requests (versions.js: clientHttp), through the relay: the relay's own refusals
// said in words (a body over 100 MB, too many requests); the network's failure as Node's fetch says
// it, which versions.js puts in words; the answer's body read as isomorphic-git reads one
async function* morceaux(flux) {
  const lecteur = flux.getReader();
  try {
    for (;;) {
      const { done, value } = await lecteur.read();
      if (done) return;
      yield value;
    }
  } finally { lecteur.releaseLock(); }
}
export async function fetchGit(url, init) {
  const adresse = adresseRelayee(url);
  let r;
  try { r = await fetch(adresse, init); } catch { throw new TypeError('fetch failed'); }
  if (adresse !== url && r.status === 413) throw new Error(t('versions_relais_trop_gros'));
  if (adresse !== url && r.status === 429) throw new Error(t('versions_relais_limite'));
  return { url: r.url, status: r.status, statusText: r.statusText, headers: r.headers, body: r.body ? morceaux(r.body) : null };
}

// ---- the tokens, in IndexedDB (versions.js's magasinJetons, in the browser)

let base = null;
const ouvrirBase = () => (base ||= new Promise((ok, ko) => {
  const r = indexedDB.open('nuee-jetons', 1);
  r.onupgradeneeded = () => { r.result.createObjectStore('jetons'); r.result.createObjectStore('renouvellements'); };
  r.onsuccess = () => ok(r.result);
  r.onerror = () => ko(r.error);
}).catch(() => null));
function magasinJetons(nom) {
  const visite = new Map();  // (a browser that keeps nothing: this visit only)
  async function faire(mode, f) {
    const db = await ouvrirBase();
    return new Promise((ok, ko) => {
      const tx = db.transaction(nom, mode);
      const q = f(tx.objectStore(nom));
      tx.oncomplete = () => ok(q.result);
      tx.onerror = () => ko(tx.error);
      tx.onabort = () => ko(tx.error);
    });
  }
  const lire = async (h) => {
    if (!(await ouvrirBase())) return visite.get(h) || null;
    const v = await faire('readonly', (s) => s.get(h));
    return typeof v === 'string' && v ? v : null;
  };
  return {
    seulementSession: async () => !(await ouvrirBase()),
    lire,
    present: async (h) => !!(await lire(h)),
    connus: async () => (await ouvrirBase() ? [].concat(await faire('readonly', (s) => s.getAll())) : [...visite.values()]).filter((x) => typeof x === 'string' && x),
    async ecrire(h, jeton) {
      if (!(await ouvrirBase())) { if (jeton) visite.set(h, String(jeton)); else visite.delete(h); return; }
      await faire('readwrite', (s) => (jeton ? s.put(String(jeton), h) : s.delete(h)));
    },
  };
}
export const jetonsNavigateur = magasinJetons('jetons');
const renouvellements = magasinJetons('renouvellements');

// ---- app/comptes.js in the page

const REGLAGES = 'nuee-comptes';
let reglagesVisite = null;
const reglages = {
  lire: () => {
    if (reglagesVisite) return reglagesVisite;
    try { return JSON.parse(localStorage.getItem(REGLAGES) || '{}') || {}; } catch { return {}; }
  },
  ecrire: (r) => { try { localStorage.setItem(REGLAGES, JSON.stringify(r)); reglagesVisite = null; } catch { reglagesVisite = r; } },
};
// (whether an account is known here: its renewal is then planned at the first use of History)
export const comptesNotes = () => Object.keys((reglages.lire() || {}).comptes || {}).length > 0;

// (the page has no git to ask, nor programs to start: comptes.js's trouverGit finds none)
const PROCESSUS = { platform: 'browser', env: {}, pid: ONGLET };
let noyau = null;
function charger() {
  noyau ||= (async () => {
    const [{ Buffer }, { default: chargerComptes }, { default: chargerIds }] = await Promise.all([
      import('./vendor/git.js'), import('./app/comptes.js'), import('./app/comptes_ids.js')]);
    globalThis.Buffer ||= Buffer;
    const rien = () => { throw new Error('comptes.js: not in the page'); };
    const ids = chargerIds(rien, PROCESSUS, Buffer);
    const modules = { './comptes_ids': ids, fs: { statSync: rien }, path: { delimiter: ':', join: (...p) => p.join('/') },
      child_process: { spawn: rien, execFile: rien, execFileSync: rien } };
    const { creerComptes } = chargerComptes((n) => {
      if (!Object.hasOwn(modules, n)) throw new Error(`comptes.js: ${n} isn't in the page`);
      return modules[n];
    }, PROCESSUS, Buffer);
    const c = creerComptes({
      fetch: fetchCompte,
      jeton: (h, j) => jetonsNavigateur.ecrire(h, j),
      jetons: jetonsNavigateur,
      renouvellements,
      reglages,
      ouvrirNavigateur: (url) => { window.open(url, '_blank', 'noopener,noreferrer'); },
      langue: () => langueActuelle(),
    });
    // (a GitLab sign-in's token renewed before it ends, while the tab is open; one ended renewed now)
    c.preparer();
    return c;
  })();
  noyau.catch(() => { noyau = null; });
  return noyau;
}
export const preparerComptes = () => charger().then(() => {}, () => {});

// The functions of window.bureau.comptes: github.com and gitlab.com alone (a self-hosted GitLab's
// sign-in, and the computer's git, are the desktop application's)
const sur = (nom) => async (hote, ...args) => {
  const h = exigerHoteRelais(hote);
  return (await charger())[nom](h, ...args);
};
export const comptesNavigateur = Object.fromEntries(['serveur', 'commencerConnexion', 'ouvrirPage', 'attendreConnexion', 'annulerConnexion',
  'compteGit', 'utiliserJeton', 'compte', 'creerDepot', 'inviter', 'deconnecter'].map((n) => [n, sur(n)]));
comptesNavigateur.reglerServeur = async (hote) => { throw refusHote(hote); };
