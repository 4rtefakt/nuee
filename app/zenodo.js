// A reproducibility package (interface/paquet.js, moteur/R/paquet.R) deposited on Zenodo as a
// draft, from the main process (main.js: IPC zenodo-*, creerZenodo), with the user's personal
// access token (on Zenodo: Account › Applications › Personal access tokens, scopes deposit:write and
// deposit:actions), through its deposit API (developers.zenodo.org), in this order:
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
const crypto = require('crypto');
const fs = require('fs');
const path = require('path');

const DOI_NUEE = '10.5281/zenodo.24263014';
// Zenodo and its test server, whose deposits don't count (it has accounts and tokens of its own)
const HOTES = ['zenodo.org', 'sandbox.zenodo.org'];
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

// A file inside a folder (its real path: no link nor `..` gets out of it)
function dansDossier(fichier, dossier) {
  try {
    const rel = path.relative(fs.realpathSync(dossier), fs.realpathSync(fichier));
    return !!rel && !rel.startsWith('..') && !path.isAbsolute(rel);
  } catch { return false; }
}

// What main.js gives the interface (preload.js: bureau.zenodo, IPC zenodo-<name>). The personal
// access token of each host (zenodo.org, sandbox.zenodo.org) is kept under the host's name in the
// store of the versions and the sign-ins (versions.js: magasinJetons, the same one main.js gives
// them): encrypted by the system, or until Nuée closes where it can't encrypt. It never goes back to
// the interface, which only learns whether one is kept. The drafts this run created are kept here
// (their bucket among them): the interface names one by its id. Each function answers
// { erreur: code, detail, page } rather than throwing: `demande` for what is refused (a host other
// than Zenodo's, a draft this run didn't create, a package that isn't a .zip of the engine's session
// folder), `jeton_invalide`, `sans_jeton`, and the codes of ErreurZenodo, the token taken out of
// their detail.
//   jetons: magasinJetons(...); fetch: Electron's (main.js: net.fetch, the system's proxy);
//   base(hote): the address of a host's API (main.js: NUEE_ZENODO, a stand-in for the checks);
//   dossier(): the engine's session folder (R's temporary folder of this run), the only one a
//   package is deposited from; ouvrir(page): the draft's page opened in the browser.
function creerZenodo({ jetons, fetch, base = (hote) => `https://${hote}`, dossier, ouvrir = () => {} }) {
  const hote = (h) => (HOTES.includes(h) ? h : null);
  const brouillons = new Map();
  const echec = (e, jeton) => ({ erreur: e.code || 'reseau', detail: String((e && e.message) || e).split(jeton).join('…'), page: (e && e.page) || null });
  return {
    // whether a token is kept for the host, and whether one kept now outlives this session
    async etat(h) {
      if (!hote(h)) return { erreur: 'demande' };
      return { jeton: await jetons.present(h), persistant: !(await jetons.seulementSession()) };
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
        return echec(e, jeton);
      }
    },
  };
}

module.exports = { creerZenodo, creerBrouillon, remplirBrouillon, metadonneesZenodo, ErreurZenodo, DOI_NUEE, HOTES };
