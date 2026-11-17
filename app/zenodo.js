// A reproducibility package (interface/paquet.js, moteur/R/paquet.R) deposited on Zenodo as a
// draft, from the main process (main.js: IPC zenodo-*, creerZenodo), with a token: the one Zenodo
// gives Nuée when the user signs in with Zenodo (below), or their personal access token (on Zenodo:
// Account › Applications › Personal access tokens, scope deposit:write), through its deposit API
// (developers.zenodo.org), in this order:
//   1. the draft created with its DOI reserved (prereserve_doi): Zenodo gives back the DOI it will
//      register when the draft is published (creerBrouillon);
//   2. the package written by the engine with that DOI, which its README, CITATION.cff and
//      codemeta.json name (the interface asks for it between the two);
//   3. the package's .zip put in the draft's bucket (its MD5 checked against the one Zenodo
//      computes), then the draft's description filled in from the package's dialog (title, authors
//      with their ORCID, description, keywords, licence), with Nuée as the software that made it, by
//      its DOI, the reservation kept (remplirBrouillon).
// Never published: nothing here calls the publish action. The user reviews the draft on Zenodo's
// site and publishes it there, its DOI then registered. Every request goes to `base` (zenodo.org,
// sandbox.zenodo.org, or a stand-in for the checks), and an address Zenodo gives elsewhere is
// refused: the token is only ever sent there, in the Authorization header.
// Signing in with Zenodo (OAuth 2.0's authorization code, RFC 6749 § 4.1), so that nobody needs to
// make a token: Nuée listens on 127.0.0.1, at the first of PORTS_RETOUR free, and opens Zenodo's page
// in the browser, where the user signs in and authorizes Nuée for deposit:write alone (uploading,
// never publishing); Zenodo sends the browser back to Nuée with a code, exchanged for a token, kept
// as a pasted one is. Zenodo sends it back only to an address registered with Nuée's application,
// exactly as written (docs/developpement.md, "Signing in to Zenodo"). Nuée's applications are public
// clients: their client id alone (IDS), no secret. Zenodo's tokens last 60 days and aren't renewed
// (it renews only confidential clients'); a new sign-in to the same account, on this computer or
// another, replaces the one before, which Zenodo then refuses (401): Nuée forgets a token refused so.
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const path = require('path');
const { idValide } = require('./comptes');

const DOI_NUEE = '10.5281/zenodo.24263014';
// Zenodo and its test server, whose deposits don't count (it has accounts and tokens of its own)
const HOTES = ['zenodo.org', 'sandbox.zenodo.org'];
// The client id of Nuée's application on each host (public: no secret). A placeholder ("<to fill>")
// says signing in isn't set up for that host in this copy of Nuée: a token can still be pasted.
const IDS = { 'zenodo.org': '<to fill>', 'sandbox.zenodo.org': '<to fill>' };
// Where Zenodo sends the browser back: http://127.0.0.1:<port>/zenodo, for each of these ports (the
// second when another program has the first), every one registered with the applications
const PORTS_RETOUR = [47864, 47865];
const CHEMIN_RETOUR = '/zenodo';
const PORTEE = 'deposit:write';
const DELAI_CONNEXION = 10 * 60000;
// the licences the package offers (moteur/R/paquet.R: LICENCES_PAQUET), by their SPDX id, and Zenodo's
const LICENCES = { 'CC-BY-4.0': 'cc-by-4.0', 'CC0-1.0': 'cc0-1.0', 'CC-BY-SA-4.0': 'cc-by-sa-4.0', 'CC-BY-NC-4.0': 'cc-by-nc-4.0' };
// a DOI as the engine takes it (paquet.R: doi_paquet)
const UN_DOI = /^10\.[0-9]{4,9}\/[-._;()/:A-Za-z0-9]+$/;

// code: jeton (401: the token refused), droits (403: not the scopes), refus (400: what Zenodo didn't
// take, `detail`), taille (413), http (another status), reseau, adresse (an address elsewhere),
// reponse (an answer without what is needed), doi (no DOI reserved for the draft, or another one
// than the package names), empreinte (the file received isn't the one sent).
// `page`: the draft's page, once the draft exists (it is then to be finished, or deleted, on Zenodo).
class ErreurZenodo extends Error {
  constructor(code, message, extra = {}) {
    super(message);
    this.code = code;
    Object.assign(this, extra);
  }
}

const echapper = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

// The draft's metadata in the deposit API's words, from the package's (the dialog's answers, as the
// engine keeps them: titre, auteurs [{ prenom, nom, orcid }], description, mots_cles, licence), its
// reserved DOI kept (prereserve_doi). `langue`: the package's (its README's).
function metadonneesZenodo(meta, { langue = 'en' } = {}) {
  const creators = [].concat(meta.auteurs || []).map((a) => ({
    name: a.prenom ? `${a.nom}, ${a.prenom}` : String(a.nom || ''),
    ...(a.orcid ? { orcid: a.orcid } : {}),
  }));
  // (paragraphs as the README writes them, in Zenodo's HTML)
  const paragraphes = String(meta.description || '').split(/\n\s*\n/).map((x) => x.trim()).filter(Boolean);
  const mots = [].concat(meta.mots_cles || []).filter(Boolean);
  return {
    upload_type: 'dataset',
    title: String(meta.titre || ''),
    creators,
    description: (paragraphes.length ? paragraphes : [String(meta.titre || '')]).map((x) => `<p>${echapper(x).replace(/\n/g, '<br>')}</p>`).join(''),
    access_right: 'open',
    license: LICENCES[meta.licence] || LICENCES['CC-BY-4.0'],
    ...(mots.length ? { keywords: mots } : {}),
    language: langue === 'fr' ? 'fra' : 'eng',
    related_identifiers: [{ identifier: DOI_NUEE, relation: 'isCompiledBy', resource_type: 'software', scheme: 'doi' }],
    prereserve_doi: true,
  };
}

// An answer that isn't 2xx, as an error (Zenodo's JSON: { status, message, errors: [{ field, message }] }).
function erreurReponse(statut, j) {
  const message = [j && j.message, ...[].concat((j && j.errors) || []).map((e) => [e.field, [].concat(e.messages || e.message || [])].filter(Boolean).join(': '))]
    .filter(Boolean).join(' — ') || `HTTP ${statut}`;
  const code = statut === 401 ? 'jeton' : statut === 403 ? 'droits' : statut === 400 ? 'refus' : statut === 413 ? 'taille' : 'http';
  return new ErreurZenodo(code, message, { statut });
}

// A request to Zenodo's API at `base`'s origin, with the token: `adresse` a path, or an address
// Zenodo gave, refused when it is on another server; JSON back. `page`: carried by its errors.
// `fetch`: the main process gives Electron's (the system's proxy).
function client({ base, jeton, fetch = globalThis.fetch }) {
  const origine = new URL(base).origin;
  const ici = (adresse, page) => {
    const u = new URL(adresse, origine);
    if (u.origin !== origine) throw new ErreurZenodo('adresse', `Zenodo gave an address elsewhere: ${u.origin}`, { page });
    return u;
  };
  const appel = async (adresse, { method = 'GET', corps, type = 'application/json', page = null } = {}) => {
    const u = ici(adresse, page);
    let r;
    try {
      r = await fetch(u.href, { method, body: corps, redirect: 'error',
        headers: { Authorization: `Bearer ${jeton}`, Accept: 'application/json', ...(corps !== undefined ? { 'Content-Type': type } : {}) } });
    } catch (e) {
      throw new ErreurZenodo('reseau', String((e && e.cause && e.cause.message) || (e && e.message) || e), { page });
    }
    const texte = await r.text();
    let j = null;
    try { j = texte ? JSON.parse(texte) : null; } catch { j = null; }
    if (!r.ok) throw Object.assign(erreurReponse(r.status, j), { page });
    return j || {};
  };
  return { origine, ici, appel };
}

// The DOI a deposition's answer says is reserved for it (metadata.prereserve_doi.doi), or null.
const doiReserve = (j) => {
  const doi = j && j.metadata && j.metadata.prereserve_doi && j.metadata.prereserve_doi.doi;
  return typeof doi === 'string' && UN_DOI.test(doi) ? doi : null;
};

// 1. A new draft at `base` (https://zenodo.org…), empty but for its DOI, reserved. Returns
// { id, doi, page (the draft's page), bucket (where its files go) }. A draft Zenodo gives without a
// DOI (the reservation refused) is an error `doi`, with its page: the draft exists, to be deleted.
async function creerBrouillon({ base, jeton, fetch = globalThis.fetch }) {
  const { origine, ici, appel } = client({ base, jeton, fetch });
  const b = await appel('/api/deposit/depositions', { method: 'POST', corps: JSON.stringify({ metadata: { prereserve_doi: true } }) });
  const id = b.id;
  if (id === undefined || id === null) throw new ErreurZenodo('reponse', 'Zenodo gave no draft');
  const html = b.links && b.links.html;
  const page = html && new URL(html, origine).origin === origine ? new URL(html, origine).href : `${origine}/uploads/${encodeURIComponent(id)}`;
  const doi = doiReserve(b);
  if (!doi) throw new ErreurZenodo('doi', 'Zenodo reserved no DOI for the draft', { page });
  const bucket = b.links && b.links.bucket;
  if (!bucket) throw new ErreurZenodo('reponse', 'Zenodo gave no bucket for the files', { page });
  return { id, doi, page, bucket: ici(bucket, page).href };
}

// 3. The draft `brouillon` (creerBrouillon's) given `fichier`, the package's .zip, in its bucket, then
// the description `metadonnees` (metadonneesZenodo). The answer must still give the draft the DOI
// the package names. Returns { id, doi, page, fichier, taille }.
async function remplirBrouillon({ base, jeton, brouillon, fichier, metadonnees, fetch = globalThis.fetch }) {
  const { appel } = client({ base, jeton, fetch });
  const { id, doi, page, bucket } = brouillon;
  const octets = await fs.promises.readFile(fichier);
  const nom = path.basename(fichier);
  const f = await appel(`${String(bucket).replace(/\/+$/, '')}/${encodeURIComponent(nom)}`, { method: 'PUT', corps: octets, type: 'application/octet-stream', page });
  const md5 = crypto.createHash('md5').update(octets).digest('hex');
  if (f.checksum && String(f.checksum).toLowerCase() !== `md5:${md5}`) {
    throw new ErreurZenodo('empreinte', `Zenodo received another file (${f.checksum}, md5:${md5} sent)`, { page });
  }
  const r = await appel(`/api/deposit/depositions/${encodeURIComponent(id)}`, { method: 'PUT', corps: JSON.stringify({ metadata: metadonnees }), page });
  const garde = doiReserve(r);
  if (garde && garde !== doi) throw new ErreurZenodo('doi', `Zenodo gave the draft the DOI ${garde}, the package names ${doi}`, { page });
  return { id, doi, page, fichier: nom, taille: octets.length };
}

// A server listening on 127.0.0.1, at the first of `ports` free, or null
async function ecouterRetour(ports) {
  for (const port of ports) {
    const serveur = http.createServer();
    const libre = await new Promise((ok) => {
      serveur.once('error', () => ok(false));
      serveur.listen(port, '127.0.0.1', () => ok(true));
    });
    if (libre) return serveur;
  }
  return null;
}

// The code Zenodo sent back exchanged for a token, at its /oauth/token: the client id, no secret (a
// public client), the same return address. The token, or an ErreurZenodo (oauth: Zenodo refused).
async function echangerCode({ base, id, code, retour, fetch = globalThis.fetch }) {
  let r;
  try {
    r = await fetch(`${new URL(base).origin}/oauth/token`, { method: 'POST', redirect: 'error',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded', Accept: 'application/json' },
      body: new URLSearchParams({ grant_type: 'authorization_code', code, redirect_uri: retour, client_id: id }).toString() });
  } catch (e) {
    throw new ErreurZenodo('reseau', String((e && e.cause && e.cause.message) || (e && e.message) || e));
  }
  const texte = await r.text();
  let j = null;
  try { j = texte ? JSON.parse(texte) : null; } catch { j = null; }
  const jeton = j && typeof j.access_token === 'string' ? j.access_token : '';
  if (!r.ok || !/^[\x21-\x7e]{8,512}$/.test(jeton)) {
    throw new ErreurZenodo('oauth', [j && j.error, j && j.error_description].filter((x) => typeof x === 'string' && x).join(': ') || `HTTP ${r.status}`);
  }
  return jeton;
}

// The page the browser shows once Zenodo sent it back to Nuée, in the interface's language
function pageRetour(cle, langue, detail = '') {
  const textes = langue === 'fr'
    ? { connecte: 'Nuée est connecté à Zenodo. Vous pouvez fermer cet onglet et revenir à Nuée.',
      refusee: 'Nuée n’est pas connecté : son autorisation a été refusée sur Zenodo. Vous pouvez fermer cet onglet.',
      echec: `Nuée n’a pas pu se connecter à Zenodo : ${detail}. Revenez à Nuée pour recommencer.`,
      autre: 'Ce n’est pas la connexion que Nuée attend : recommencez-la depuis Nuée.' }
    : { connecte: 'Nuée is signed in to Zenodo. You can close this tab and go back to Nuée.',
      refusee: 'Nuée isn’t signed in: its authorization was declined on Zenodo. You can close this tab.',
      echec: `Nuée couldn’t sign in to Zenodo: ${detail}. Go back to Nuée to start again.`,
      autre: 'This isn’t the sign-in Nuée is waiting for: start it again from Nuée.' };
  return `<!doctype html><html lang="${langue === 'fr' ? 'fr' : 'en'}"><head><meta charset="utf-8"><title>Nuée</title>`
    + '<meta name="viewport" content="width=device-width, initial-scale=1"></head>'
    + `<body style="font: 16px/1.5 system-ui, sans-serif; max-width: 36em; margin: 4em auto; padding: 0 1em"><p>${echapper(textes[cle])}</p></body></html>`;
}

// A file inside a folder (its real path: no link nor `..` gets out of it)
function dansDossier(fichier, dossier) {
  try {
    const rel = path.relative(fs.realpathSync(dossier), fs.realpathSync(fichier));
    return !!rel && !rel.startsWith('..') && !path.isAbsolute(rel);
  } catch { return false; }
}

// What main.js gives the interface (preload.js: bureau.zenodo, IPC zenodo-<name>). The token of each
// host (zenodo.org, sandbox.zenodo.org), the sign-in's or the one pasted, is kept under the host's
// name in the store of the versions and the sign-ins (versions.js: magasinJetons, the same one
// main.js gives them): encrypted by the system, or until Nuée closes where it can't encrypt. It never
// goes back to the interface, which only learns whether one is kept. The drafts this run created
// are kept here (their bucket among them): the interface names one by its id. Each function answers
// { erreur: code, detail, page } rather than throwing: `demande` for what is refused (a host other
// than Zenodo's, a draft this run didn't create, a package that isn't a .zip of the engine's session
// folder), `jeton_invalide`, `sans_jeton`, `non_regle` (signing in not set up for the host), `port`
// (no port of PORTS_RETOUR free), and the codes of ErreurZenodo, the token taken out of their detail.
//   jetons: magasinJetons(...); fetch: Electron's (main.js: net.fetch, the system's proxy);
//   base(hote): the address of a host's API (main.js: NUEE_ZENODO, a stand-in for the checks);
//   dossier(): the engine's session folder (R's temporary folder of this run), the only one a
//   package is deposited from; ouvrir(page): the draft's page opened in the browser;
//   ouvrirNavigateur(adresse): Zenodo's page to sign in, opened in the browser; idClient(hote): the
//   client id of Nuée's application there (IDS; main.js: a stand-in's); ports, delaiConnexion: for
//   the checks.
function creerZenodo({ jetons, fetch, base = (hote) => `https://${hote}`, dossier, ouvrir = () => {}, ouvrirNavigateur = () => {},
  idClient = (hote) => IDS[hote], ports = PORTS_RETOUR, delaiConnexion = DELAI_CONNEXION }) {
  const hote = (h) => (HOTES.includes(h) ? h : null);
  const brouillons = new Map();
  const connexions = new Map();  // (each host's last sign-in, under way or ended with its answer)
  const echec = (e, jeton) => ({ erreur: e.code || 'reseau', detail: String((e && e.message) || e).split(jeton).join('…'), page: (e && e.page) || null });
  // (a token Zenodo refuses, 401, is forgotten: expired, or replaced by a sign-in elsewhere)
  const refuse = async (h, e) => { if (e && e.code === 'jeton') await jetons.ecrire(h, ''); };
  // A sign-in ended with `r` (attendreConnexion's answer), its server closed
  const finir = (s, r) => {
    if (s.fini) return;
    s.fini = true;
    clearTimeout(s.minuterie);
    s.serveur.close();
    s.resoudre(r);
  };
  // Zenodo's answer, the browser sent back to Nuée: the state given to Zenodo, then its code exchanged
  // for the token, kept, or its refusal; the browser shown what came of it. Another address, or
  // another state (an old tab, another sign-in's), changes nothing: this sign-in goes on waiting.
  // (each answer closes its connection: a browser keeping it would send the next sign-in's answer to
  // this one's server, ended, which still answers on the connections it has)
  async function retour(h, s, req, res) {
    const u = new URL(req.url, s.retour);
    const page = (statut, cle, detail) => {
      res.writeHead(statut, { 'Content-Type': 'text/html; charset=utf-8', 'Cache-Control': 'no-store', Connection: 'close' });
      res.end(pageRetour(cle, s.langue, detail));
    };
    if (req.method !== 'GET' || u.pathname !== CHEMIN_RETOUR) { res.writeHead(404, { Connection: 'close' }); res.end(); return; }
    if (u.searchParams.get('state') !== s.etat || s.recu || s.fini) { page(400, 'autre'); return; }
    s.recu = true;
    const code = u.searchParams.get('code');
    const erreur = u.searchParams.get('error');
    let r;
    if (erreur === 'access_denied') r = { ok: false, raison: 'refusee' };
    else if (erreur || !code) r = { erreur: 'oauth', detail: [erreur, u.searchParams.get('error_description')].filter(Boolean).join(': ') || 'no code' };
    else {
      try {
        const jeton = await echangerCode({ base: base(h), id: s.id, code, retour: s.retour, fetch });
        // (cancelled meanwhile: nothing kept)
        if (s.fini) r = { ok: false, raison: 'annulee' };
        else {
          await jetons.ecrire(h, jeton);
          r = { ok: true, persistant: !(await jetons.seulementSession()) };
        }
      } catch (e) {
        r = { erreur: e.code || 'reseau', detail: String((e && e.message) || e).split(code).join('…') };
      }
    }
    page(200, r.ok ? 'connecte' : r.raison === 'refusee' ? 'refusee' : r.raison === 'annulee' ? 'autre' : 'echec', r.detail);
    finir(s, r);
  }
  return {
    // whether a token is kept for the host, whether one kept now outlives this session, and
    // whether this copy of Nuée can sign in there (its application's client id)
    async etat(h) {
      if (!hote(h)) return { erreur: 'demande' };
      return { jeton: await jetons.present(h), persistant: !(await jetons.seulementSession()), connexion: idValide(idClient(h)) };
    },
    // Signing in with Zenodo: Nuée listening for its answer, its page opened in the browser;
    // { adresse }, that page (to open it again). One sign-in at a time for a host: a new one ends
    // the one before. `langue`: the interface's, for the page the browser shows at the end.
    async connexion(h, langue = 'en') {
      if (!hote(h)) return { erreur: 'demande' };
      const id = idClient(h);
      if (!idValide(id)) return { erreur: 'non_regle' };
      if (connexions.has(h)) finir(connexions.get(h), { ok: false, raison: 'annulee' });
      const serveur = await ecouterRetour(ports);
      if (!serveur) return { erreur: 'port', detail: ports.join(', ') };
      const s = { serveur, id, langue: langue === 'fr' ? 'fr' : 'en', etat: crypto.randomBytes(24).toString('hex'),
        retour: `http://127.0.0.1:${serveur.address().port}${CHEMIN_RETOUR}` };
      s.resultat = new Promise((ok) => { s.resoudre = ok; });
      s.minuterie = setTimeout(() => finir(s, { ok: false, raison: 'expiree' }), delaiConnexion);
      serveur.on('request', (req, res) => { retour(h, s, req, res).catch(() => { if (!res.headersSent) { res.writeHead(500); res.end(); } }); });
      connexions.set(h, s);
      const adresse = `${new URL(base(h)).origin}/oauth/authorize?${new URLSearchParams({ response_type: 'code', client_id: id, redirect_uri: s.retour, scope: PORTEE, state: s.etat })}`;
      try { await ouvrirNavigateur(adresse); } catch { /* (the interface offers to open it again) */ }
      return { adresse };
    },
    // Until Zenodo's answer to the host's last sign-in (the answer itself once it came): { ok: true,
    // persistant } once the token is kept, { ok: false, raison: 'refusee' (Nuée not authorized) |
    // 'expiree' (no answer in ten minutes) | 'annulee' }, or { erreur, detail } (oauth: Zenodo
    // didn't give a token for the code; reseau). Asked twice, the same wait.
    async attendreConnexion(h) {
      const s = hote(h) && connexions.get(h);
      return s ? s.resultat : { erreur: 'aucune' };
    },
    async annulerConnexion(h) {
      if (hote(h) && connexions.has(h)) finir(connexions.get(h), { ok: false, raison: 'annulee' });
      return {};
    },
    // the token the user pasted, kept for the host
    async jeton(h, valeur) {
      if (!hote(h)) return { erreur: 'demande' };
      const j = String(valeur || '').trim();
      if (!/^[\x21-\x7e]{8,512}$/.test(j)) return { erreur: 'jeton_invalide' };
      await jetons.ecrire(h, j);
      return { persistant: !(await jetons.seulementSession()) };
    },
    async oublier(h) {
      if (!hote(h)) return { erreur: 'demande' };
      await jetons.ecrire(h, '');
      return {};
    },
    // 1. a new draft, its DOI reserved: { id, doi, page }
    async brouillon(h) {
      if (!hote(h)) return { erreur: 'demande' };
      const jeton = await jetons.lire(h);
      if (!jeton) return { erreur: 'sans_jeton' };
      try {
        const b = await creerBrouillon({ base: base(h), jeton, fetch });
        brouillons.set(`${h}|${b.id}`, b);
        return { id: b.id, doi: b.doi, page: b.page };
      } catch (e) {
        await refuse(h, e);
        return echec(e, jeton);
      }
    },
    // 3. that draft given the package (the .zip the engine wrote in its session folder, with the
    // draft's DOI) and its description; its page opens. Tried again after a failure, the same draft.
    async deposer(h, id, chemin, meta) {
      const fichier = String(chemin || '');
      const b = brouillons.get(`${h}|${id}`);
      if (!hote(h) || !b || !/\.zip$/i.test(fichier) || !dansDossier(fichier, dossier())) return { erreur: 'demande' };
      const jeton = await jetons.lire(h);
      if (!jeton) return { erreur: 'sans_jeton', page: b.page };
      try {
        const r = await remplirBrouillon({ base: base(h), jeton, brouillon: b, fichier, fetch, metadonnees: metadonneesZenodo(meta || {}, { langue: meta && meta.langue }) });
        brouillons.delete(`${h}|${id}`);
        ouvrir(r.page);
        return { id: r.id, doi: r.doi, page: r.page };
      } catch (e) {
        await refuse(h, e);
        return echec(e, jeton);
      }
    },
  };
}

module.exports = { creerZenodo, creerBrouillon, remplirBrouillon, metadonneesZenodo, ErreurZenodo, DOI_NUEE, HOTES, IDS, PORTS_RETOUR };
