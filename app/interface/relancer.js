// A reproducibility package opened again and rerun: File › Open a reproducibility package…
// (commandes.js: paquet_ouvrir), File › Open on such a .zip and one dropped on the window (the
// engine's answer to ouvrir says so: app.js, siPaquet), and in the browser version its link,
// ?doi=<doi> or ?record=<id>, the package fetched from Zenodo (ouvrirLien). The engine unzips it in
// its session folder, checks every file against MANIFEST.sha256 and opens its project as a new
// project without a file (moteur/R/paquet.R, serveur.R: paquet_ouvrir): nothing is written into the
// package. Then every model is fitted again (its views as the package's session had them, app.js:
// restaurerSession; the others without), with the number of components of its deposited fit, and
// its key results compared with those the package deposited (results/key_results.json) by
// comparer_resultats, in the What changed panel (changements.js: afficherChangements with `paquet`):
// how many give the deposited results, what changed in those that differ, those that can't be fitted
// again here (said as such, not counted as a difference), what the manifest found, which Nuée and R
// made the deposit and which reran it.
//   ouvrirPaquet(app, chemin, { nom, lien })   the package at `chemin` (the engine's), rerun; its comparison
//   choisirPaquet(app)                          File › Open a reproducibility package…
//   ouvrirLien(app, { doi, record })            the browser version's link
//   recordZenodo({ doi, record }, recuperer)     the Zenodo record a DOI or an id names (fetch by default)
//   fichiersRecord(record)                      its files, { key, size }
// The link asks nothing of any site but zenodo.org (its API, which answers any page:
// Access-Control-Allow-Origin *): a version's DOI, 10.5281/zenodo.<id>, is the record <id>; a concept
// DOI's id is its parent's, which Zenodo answers with its latest version (a redirect to it); any other
// DOI is searched for among every version of every record (Zenodo's search, its fields doi and
// conceptdoi).
import { t, ajouterTextes, deuxPoints } from './i18n.js';
import { el, dialogue } from './ui.js';
import { demander, deposer, bureau } from './api.js';
import { VERSION } from './nouveautes.js';
import { afficherChangements, bilanPaquet, phraseBilan, phraseManifeste } from './changements.js';
import { progression } from './etapes.js';

const ZENODO = 'https://zenodo.org';

export async function ouvrirPaquet(app, chemin, { nom = null, lien = null } = {}) {
  app.statut(t('relance_ouverture', { nom: nom || String(chemin).split(/[\\/]/).pop() }));
  // (a .zip that isn't a package: refused as such, which the link tries the next one for, not an error)
  const r = await app.occupe(() => demander('paquet_ouvrir', { chemin, ...(nom ? { nom } : {}), version: VERSION })
    .catch((e) => { if (e.reponse && e.reponse.pas_un_paquet) return { refus: e }; throw e; }));
  if (r.refus) { app.statut(r.refus.message, true); throw r.refus; }
  await app.projetOuvert(r.projet);
  const notes = [].concat(r.note || []);
  const pid = r.projet.id;
  const paquet = { titre: r.titre, doi: r.doi, fichier: r.fichier, manifeste: r.manifeste, depose: r.depose, ici: r.ici, paquets: r.paquets_differents, lien };
  if (!r.depose || !Array.isArray(r.depose.modeles)) {
    // (a package of Nuée 1.1, or one whose results file is gone: nothing to compare with)
    dialogue({ titre: t('relance_titre', { titre: r.titre }), contenu: el('div', {}, el('p', {}, t('relance_sans_resultats')), el('p', { class: 'note' }, phraseManifeste(r.manifeste))),
      boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
    app.statut(t('relance_ouvert_sans_resultats', { titre: r.titre }), notes.length > 0);
    return null;
  }
  const pr = progression(t('relance_titre', { titre: r.titre }));
  let apres;
  try { apres = await relancerModeles(app, pid, r.depose, pr); } finally { pr.fermer(); }
  const c = await afficherChangements(app, { titre: r.titre, pid, avant: { modeles: r.depose.modeles }, apres, paquet });
  const b = bilanPaquet(c);
  app.statut([phraseBilan(b), b.nonRelances.length ? t('relance_non_relances', { noms: b.nonRelances.map((m) => m.nom).join(', ') }) : '',
    r.manifeste && r.manifeste.intact ? '' : t('relance_manifeste_statut'), ...notes].filter(Boolean).join(' '),
  b.differents.length > 0 || !(r.manifeste && r.manifeste.intact));
  return c;
}

// Every model of the project fitted again, as Update the data fits them again (etapes.js): the
// fits its session just made (app.js: restaurerSession) when they have the number of components of
// the deposited fit, else a fit without views (app.calculerDiscret) with that number; then its key
// results. A model that can't be fitted here keeps why (the browser version without a package, an
// R-code model whose code waits for Run it); Stop, of the dialog or of the status bar: the models left
// say so. Resolves to { modeles: [{ nom, numero, type, entrees | erreur }] }, as resumerProjet
// (changements.js).
async function relancerModeles(app, pid, depose, pr) {
  const deposes = new Map([].concat(depose.modeles || []).map((m) => [m.nom, m]));
  const definitions = [].concat(app.projet(pid).modeles || []).filter((d) => d.numero !== null && d.numero !== undefined);
  const modeles = [];
  for (const [k, d] of definitions.entries()) {
    const m = { nom: d.nom, numero: d.numero, type: d.type };
    if (pr.arrete()) { modeles.push({ ...m, entrees: null, erreur: t('relance_arretee') }); continue; }
    pr.montrer(t('relance_p_modele', { n: k + 1, total: definitions.length }), k, definitions.length, { note: `${d.nom} · ${d.type}` });
    const x = deposes.get(d.nom);
    const A = x && Number(x.A) > 0 ? Number(x.A) : undefined;
    try {
      const deja = app.modelesDe(pid).filter((f) => f.numero === d.numero && (!A || f.A === A)).pop();
      const id = deja ? deja.id : await app.calculerDiscret(pid, d.numero, A);
      const r = await demander('resultats_cles', { modele: id });
      modeles.push({ ...m, entrees: [].concat(r.entrees || []) });
    } catch (e) {
      modeles.push({ ...m, entrees: null, erreur: e.reponse && e.reponse.arrete ? t('relance_arretee') : e.message });
    }
  }
  return { modeles };
}

// File › Open a reproducibility package…: a .zip chosen (the desktop's dialog, or the page's).
export async function choisirPaquet(app) {
  try {
    if (bureau) {
      const chemin = await bureau.choisirFichier([{ name: t('relance_format'), extensions: ['zip'] }]);
      if (chemin) await ouvrirPaquet(app, chemin);
      return;
    }
    const f = await new Promise((resolve) => {
      const i = el('input', { type: 'file', accept: '.zip', onchange: () => resolve(i.files[0]) });
      i.click();
    });
    if (f) await ouvrirPaquet(app, await deposer(f), { nom: f.name });
  } catch (e) {
    app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true);
  }
}

// ------------------------------------------------------------ the link (browser version)

// A DOI as written in a link or a citation (its https://doi.org/ address, doi:...), or null when it
// isn't one: 10.<registrant>/<suffix>.
export function lireDoi(x) {
  const d = String(x || '').trim().replace(/^(https?:\/\/(dx\.)?doi\.org\/|doi:)/i, '');
  return /^10\.\d{4,9}\/\S+$/.test(d) ? d : null;
}
const memeDoi = (a, b) => !!a && !!b && String(a).toLowerCase() === String(b).toLowerCase();

// A refusal of the link, said as it is (genre: doi, inconnu, reseau, http, sans_paquet).
const refus = (genre, message) => Object.assign(new Error(message), { genre });

// The Zenodo record that `record` (its id) or `doi` names: the version a version's DOI names, the
// latest version of a concept DOI. `recuperer`: fetch, or a stand-in (the checks). Rejects with a
// refusal (`genre`) said in the interface's language.
export async function recordZenodo({ doi = null, record = null } = {}, recuperer = (url, o) => fetch(url, o)) {
  const lire = async (url) => {
    let r;
    try { r = await recuperer(url, { headers: { Accept: 'application/json' } }); } catch { throw refus('reseau', t('relance_zenodo_reseau')); }
    if (r.status === 404 || r.status === 410) return null;
    if (!r.ok) throw refus('http', t('relance_zenodo_http', { statut: r.status }));
    try { return await r.json(); } catch { throw refus('http', t('relance_zenodo_http', { statut: r.status })); }
  };
  if (record !== null && record !== undefined && String(record).trim() !== '') {
    const id = String(record).trim();
    if (!/^\d+$/.test(id)) throw refus('doi', t('relance_record_invalide', { record: id }));
    const r = await lire(`${ZENODO}/api/records/${id}`);
    if (!r) throw refus('inconnu', t('relance_record_inconnu', { record: id }));
    return r;
  }
  const d = lireDoi(doi);
  if (!d) throw refus('doi', t('relance_doi_invalide', { doi: String(doi || '') }));
  // (Zenodo's own DOI: its record, a version's; a concept's id is its parent's, which Zenodo redirects to
  // its latest version, and fetch follows)
  const zenodo = /^10\.5281\/zenodo\.(\d+)$/i.exec(d);
  if (zenodo) {
    const r = await lire(`${ZENODO}/api/records/${zenodo[1]}`).catch((e) => { if (e.genre === 'reseau') return null; throw e; });
    if (r && (memeDoi(r.doi, d) || memeDoi(r.conceptdoi, d))) return r;
  }
  // (another DOI, which a record of Zenodo may carry, or a concept DOI whose redirect didn't reach the
  // page: every version searched, the latest first; no network: said here)
  const q = encodeURIComponent(`doi:"${d}" OR conceptdoi:"${d}"`);
  const s = await lire(`${ZENODO}/api/records?q=${q}&allversions=true&sort=version&size=25`);
  const hits = [].concat((s && s.hits && s.hits.hits) || []);
  const r = hits.find((h) => memeDoi(h.doi, d)) || hits.find((h) => memeDoi(h.conceptdoi, d));
  if (!r) throw refus('inconnu', t('relance_zenodo_inconnu', { doi: d }));
  return r;
}

// A record's files, { key, size }: Zenodo's JSON gives them as an array ({ key, size, checksum, links }),
// InvenioRDM's as { entries: { <key>: { key, size } } }.
export function fichiersRecord(r) {
  const f = r && r.files;
  const liste = Array.isArray(f) ? f : f && f.entries ? Object.values(f.entries) : [];
  return liste.map((x) => ({ key: String(x.key || x.filename || ''), size: Number(x.size ?? x.filesize) || 0 })).filter((x) => x.key);
}

// The link's package, from Zenodo: the record found, each of its .zip files downloaded from zenodo.org
// until one is a package (the engine says which isn't), then opened and rerun. What keeps it from
// opening is said plainly, in a dialog: not a DOI, a DOI or a record Zenodo doesn't know, a record
// without a package, no network, an error of Zenodo.
export async function ouvrirLien(app, { doi = null, record = null }, recuperer = (url, o) => fetch(url, o)) {
  const quoi = record ? t('relance_lien_record', { record }) : t('relance_lien_doi', { doi });
  // (busy meanwhile, as any request; a refusal is no error of the page: it is said below)
  const travail = async (f) => {
    const x = await app.occupe(() => f().catch((e) => { if (e.genre) return { refus: e }; throw e; }));
    if (x && x.refus) throw x.refus;
    return x;
  };
  try {
    app.statut(t('relance_zenodo_recherche', { quoi }));
    const r = await travail(() => recordZenodo({ doi, record }, recuperer));
    const titre = (r.metadata && r.metadata.title) || r.title || '';
    const zips = fichiersRecord(r).filter((f) => /\.zip$/i.test(f.key));
    for (const f of zips) {
      app.statut(t('relance_zenodo_telechargement', { nom: f.key, taille: (f.size / 1e6).toFixed(1) }));
      const octets = await travail(async () => {
        let x;
        try { x = await recuperer(`${ZENODO}/api/records/${encodeURIComponent(r.id)}/files/${encodeURIComponent(f.key)}/content`); } catch { throw refus('reseau', t('relance_zenodo_reseau')); }
        if (!x.ok) throw refus('http', t('relance_zenodo_fichier', { nom: f.key, statut: x.status }));
        return new Uint8Array(await x.arrayBuffer());
      });
      const chemin = await deposer(new File([octets], f.key, { type: 'application/zip' }));
      const lien = { record: r.id, doi: r.doi || '', concept: memeDoi(r.conceptdoi, lireDoi(doi)) && !memeDoi(r.doi, lireDoi(doi)) ? r.conceptdoi : null, titre };
      try {
        return await ouvrirPaquet(app, chemin, { nom: f.key, lien });
      } catch (e) {
        if (!(e.reponse && e.reponse.pas_un_paquet)) throw e;
      }
    }
    throw refus('sans_paquet', t('relance_zenodo_sans_paquet', { titre: titre || r.id, doi: r.doi || '' }));
  } catch (e) {
    app.statut(e.message, true);
    dialogue({ titre: t('relance_lien_titre', { quoi }), contenu: el('p', { class: e.genre ? '' : 'erreur' }, e.message),
      boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
    return null;
  }
}

ajouterTextes({
  paquet_ouvrir: 'Ouvrir un paquet de reproductibilité…', relance_format: 'Paquet de reproductibilité (.zip)',
  relance_ouverture: 'Paquet : {nom}, ses fichiers vérifiés…', relance_titre: 'Relancer le paquet « {titre} »',
  relance_p_modele: 'Ajustement de chaque modèle de nouveau ({n} sur {total})…',
  relance_arretee: 'Arrêter a été pressé avant lui.',
  relance_sans_resultats: 'Ce paquet n’a pas de résultats déposés (resultats/resultats_cles.json) : Nuée 1.1 n’en écrivait pas. Son projet est ouvert, ses modèles tels qu’il les avait ; rien n’est comparé.',
  relance_ouvert_sans_resultats: 'Paquet « {titre} » ouvert, sans résultats déposés à comparer.',
  relance_manifeste_statut: 'Ses fichiers ne sont plus ceux de son manifeste : voir le panneau.',
  relance_lien_doi: 'le DOI {doi}', relance_lien_record: 'l’enregistrement {record}', relance_lien_titre: 'Ouvrir {quoi}',
  relance_zenodo_recherche: 'Zenodo : {quoi}…', relance_zenodo_telechargement: 'Zenodo : {nom} ({taille} Mo)…',
  relance_doi_invalide: '« {doi} » n’est pas un DOI : un DOI commence par 10. et a une barre oblique, comme 10.5281/zenodo.123.',
  relance_record_invalide: '« {record} » n’est pas un numéro d’enregistrement de Zenodo.',
  relance_zenodo_inconnu: 'Zenodo ne connaît pas le DOI {doi}.', relance_record_inconnu: 'Zenodo n’a pas d’enregistrement {record}.',
  relance_zenodo_reseau: 'Nuée n’a pas pu joindre Zenodo (zenodo.org) : vérifiez la connexion à Internet, puis rechargez la page.',
  relance_zenodo_http: 'Zenodo a répondu par une erreur (HTTP {statut}) : réessayez plus tard.',
  relance_zenodo_fichier: 'Zenodo n’a pas donné le fichier {nom} (HTTP {statut}).',
  relance_zenodo_sans_paquet: 'L’enregistrement « {titre} » de Zenodo ({doi}) n’a pas de paquet de reproductibilité : aucun de ses fichiers n’est un .zip écrit par Nuée.',
}, {
  paquet_ouvrir: 'Open a reproducibility package…', relance_format: 'Reproducibility package (.zip)',
  relance_ouverture: 'Package: {nom}, its files checked…', relance_titre: 'Rerun the package “{titre}”',
  relance_p_modele: 'Fitting each model again ({n} of {total})…',
  relance_arretee: 'Stop was pressed before it.',
  relance_sans_resultats: 'This package has no deposited results (results/key_results.json): Nuée 1.1 didn’t write them. Its project is open, its models as it had them; nothing is compared.',
  relance_ouvert_sans_resultats: 'Package “{titre}” opened, without deposited results to compare with.',
  relance_manifeste_statut: 'Its files are no longer those of its manifest: see the panel.',
  relance_lien_doi: 'the DOI {doi}', relance_lien_record: 'record {record}', relance_lien_titre: 'Open {quoi}',
  relance_zenodo_recherche: 'Zenodo: {quoi}…', relance_zenodo_telechargement: 'Zenodo: {nom} ({taille} MB)…',
  relance_doi_invalide: '“{doi}” isn’t a DOI: a DOI starts with 10. and has a slash, as 10.5281/zenodo.123.',
  relance_record_invalide: '“{record}” isn’t a Zenodo record number.',
  relance_zenodo_inconnu: 'Zenodo doesn’t know the DOI {doi}.', relance_record_inconnu: 'Zenodo has no record {record}.',
  relance_zenodo_reseau: 'Nuée couldn’t reach Zenodo (zenodo.org): check the connection to the internet, then reload the page.',
  relance_zenodo_http: 'Zenodo answered with an error (HTTP {statut}): try again later.',
  relance_zenodo_fichier: 'Zenodo didn’t give the file {nom} (HTTP {statut}).',
  relance_zenodo_sans_paquet: 'The Zenodo record “{titre}” ({doi}) has no reproducibility package: none of its files is a .zip Nuée wrote.',
});
