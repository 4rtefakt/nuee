// A reproducibility package (interface/paquet.js, moteur/R/paquet.R) deposited on Zenodo as a
// draft, from the main process (main.js: IPC zenodo-deposer), with the user's personal access token
// (on Zenodo: Account › Applications › Personal access tokens, scopes deposit:write and
// deposit:actions), through its deposit API (developers.zenodo.org): the draft created, the
// package's .zip put in the draft's bucket (its MD5 checked against the one Zenodo computes), then
// the draft's description filled in from the package's dialog (title, authors with their ORCID,
// description, keywords, licence), with Nuée as the software that made it, by its DOI.
// Never published: nothing here calls the publish action. The user reviews the draft on Zenodo's
// site and publishes it there. Every request goes to `base` (zenodo.org, sandbox.zenodo.org, or a
// stand-in for the checks), and an address Zenodo gives elsewhere is refused: the token is only ever
// sent there, in the Authorization header.
const crypto = require('crypto');
const fs = require('fs');
const path = require('path');

const DOI_NUEE = '10.5281/zenodo.24263014';
// the licences the package offers (moteur/R/paquet.R: LICENCES_PAQUET), by their SPDX id, and Zenodo's
const LICENCES = { 'CC-BY-4.0': 'cc-by-4.0', 'CC0-1.0': 'cc0-1.0', 'CC-BY-SA-4.0': 'cc-by-sa-4.0', 'CC-BY-NC-4.0': 'cc-by-nc-4.0' };

// code: jeton (401: the token refused), droits (403: not the scopes), refus (400: what Zenodo didn't
// take, `detail`), taille (413), http (another status), reseau, adresse (an address elsewhere),
// reponse (an answer without what is needed), empreinte (the file received isn't the one sent).
// `page`: the draft's page, once the draft exists (it is then to be finished on Zenodo).
class ErreurZenodo extends Error {
  constructor(code, message, extra = {}) {
    super(message);
    this.code = code;
    Object.assign(this, extra);
  }
}

const echapper = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

// The draft's metadata in the deposit API's words, from the package's (the dialog's answers, as the
// engine keeps them: titre, auteurs [{ prenom, nom, orcid }], description, mots_cles, licence).
// `langue`: the package's (its README's).
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
  };
}

// An answer that isn't 2xx, as an error (Zenodo's JSON: { status, message, errors: [{ field, message }] }).
function erreurReponse(statut, j) {
  const message = [j && j.message, ...[].concat((j && j.errors) || []).map((e) => [e.field, [].concat(e.messages || e.message || [])].filter(Boolean).join(': '))]
    .filter(Boolean).join(' — ') || `HTTP ${statut}`;
  const code = statut === 401 ? 'jeton' : statut === 403 ? 'droits' : statut === 400 ? 'refus' : statut === 413 ? 'taille' : 'http';
  return new ErreurZenodo(code, message, { statut });
}

// Deposits `fichier` (the package's .zip) as a new draft at `base` (https://zenodo.org…), with the
// token `jeton` and the draft's metadata `metadonnees` (metadonneesZenodo). `fetch`: the main
// process gives Electron's (the system's proxy). Returns { id, page (the draft's page), fichier, taille }.
async function deposerBrouillon({ base, jeton, fichier, metadonnees, fetch = globalThis.fetch }) {
  const origine = new URL(base).origin;
  let page = null;
  const appel = async (adresse, { method = 'GET', corps, type = 'application/json' } = {}) => {
    const u = new URL(adresse, origine);
    if (u.origin !== origine) throw new ErreurZenodo('adresse', `Zenodo gave an address elsewhere: ${u.origin}`, { page });
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
  // 1. the draft, empty; its file then its description follow
  const brouillon = await appel('/api/deposit/depositions', { method: 'POST', corps: '{}' });
  const id = brouillon.id;
  if (id === undefined || id === null) throw new ErreurZenodo('reponse', 'Zenodo gave no draft');
  const html = brouillon.links && brouillon.links.html;
  page = html && new URL(html, origine).origin === origine ? new URL(html, origine).href : `${origine}/uploads/${encodeURIComponent(id)}`;
  // 2. the package, in the draft's bucket
  const bucket = brouillon.links && brouillon.links.bucket;
  if (!bucket) throw new ErreurZenodo('reponse', 'Zenodo gave no bucket for the files', { page });
  const octets = await fs.promises.readFile(fichier);
  const nom = path.basename(fichier);
  const f = await appel(`${String(bucket).replace(/\/+$/, '')}/${encodeURIComponent(nom)}`, { method: 'PUT', corps: octets, type: 'application/octet-stream' });
  const md5 = crypto.createHash('md5').update(octets).digest('hex');
  if (f.checksum && String(f.checksum).toLowerCase() !== `md5:${md5}`) {
    throw new ErreurZenodo('empreinte', `Zenodo received another file (${f.checksum}, md5:${md5} sent)`, { page });
  }
  // 3. its description
  await appel(`/api/deposit/depositions/${encodeURIComponent(id)}`, { method: 'PUT', corps: JSON.stringify({ metadata: metadonnees }) });
  return { id, page, fichier: nom, taille: octets.length };
}

module.exports = { deposerBrouillon, metadonneesZenodo, ErreurZenodo, DOI_NUEE };
