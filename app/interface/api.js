// Requests to the R engine: the local R server (desktop app, development), or, in the browser
// version (index.html sets window.NUEE_MOTEUR = 'webr'), R itself running in the page through
// webR (moteur_webr.js). Every number shown in the interface comes from there.
// X-Langue: the language of the messages the engine sends back (moteur/R/langue.R).
import { langueActuelle } from './i18n.js';

// The requests the user waits for (not those made in the background), by number, with their name
// and when each began: after a moment the status bar offers to stop the computation (app.js, Stop)
// and says where it is (progression.js). A view that waits puts its waiting text (.infos.attente)
// just before asking: that text is given the request's number (data-demande), for progression.js.
// A change is told to the page ('requetes').
const enAttente = new Map();
let numeroRequete = 0;
export const debutAttente = () => (enAttente.size ? Math.min(...[...enAttente.values()].map((x) => x.debut)) : null);
export const demandesEnAttente = () => [...enAttente.entries()].map(([n, x]) => ({ n, ...x }));
const attente = (n, oui, nom) => {
  if (oui) {
    enAttente.set(n, { debut: Date.now(), nom });
    const texte = [...document.querySelectorAll('.infos.attente:not([data-demande])')].pop();
    if (texte) texte.dataset.demande = String(n);
  } else enAttente.delete(n);
  window.dispatchEvent(new CustomEvent('requetes'));
};

const WEBR = typeof window !== 'undefined' && window.NUEE_MOTEUR === 'webr';
export const MOTEUR_WEBR = WEBR;
let moteur = null;
// (the engine in the page, started at the first request: R, its packages, the engine's files)
export const moteurWebR = () => (moteur ||= import('./moteur_webr.js').then((m) => m.demarrer()));

// Each request answered: an event 'nuee-demande' { nom, corps } (the browser version copies the
// projects a document or a figure changed: app.js), and, when the page has an array
// window.nueeDemandes, the request with its time (checks and measures: tests/essai_navigateur.mjs).
function noter(nom, corps, debut, ok) {
  if (Array.isArray(window.nueeDemandes)) window.nueeDemandes.push({ nom, corps, ms: performance.now() - debut, ok });
  if (ok) window.dispatchEvent(new CustomEvent('nuee-demande', { detail: { nom, corps } }));
}

// silencieux: a request made in the background (the console's log), which doesn't report a
// stopped engine: only what the user asks for does. garder: a request that must reach the engine
// even if the window closes meanwhile (a document's last words; the browser keeps those of 64 kB
// at most).
export async function demander(nom, corps = {}, { silencieux = false, garder = false } = {}) {
  const n = silencieux ? 0 : (numeroRequete += 1);
  if (n) attente(n, true, nom);
  try { return await envoyer(nom, corps, silencieux, garder); } finally { if (n) attente(n, false); }
}

async function envoyer(nom, corps, silencieux, garder) {
  const debut = performance.now();
  if (WEBR) {
    let r;
    try {
      r = await (await moteurWebR()).demander(nom, corps, langueActuelle());
    } catch (e) {  // R itself has stopped (out of memory...)
      if (!silencieux) window.dispatchEvent(new CustomEvent('moteur-injoignable'));
      throw e;
    }
    noter(nom, corps, debut, r.statut === 200);
    if (r.statut !== 200) throw Object.assign(new Error(r.json.erreur || String(r.statut)), { reponse: r.json });
    return r.json;
  }
  let r;
  try {
    const body = JSON.stringify(corps);
    r = await fetch('/api/' + nom, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json', 'X-Langue': langueActuelle() },
      body,
      keepalive: garder && body.length < 60000,
    });
  } catch (e) {  // no answer at all: the R engine has stopped (app.js offers to restart it)
    if (!silencieux) window.dispatchEvent(new CustomEvent('moteur-injoignable'));
    throw e;
  }
  const j = await r.json();
  noter(nom, corps, debut, r.ok);
  // the whole answer goes with the error (a file changed since it was read: { conflit, date })
  if (!r.ok) throw Object.assign(new Error(j.erreur || r.statusText), { reponse: j });
  return j;
}

// A file chosen in the page, opened as a project.
export async function televerser(fichier) {
  if (WEBR) return demander('ouvrir', { chemin: await (await moteurWebR()).deposer(fichier) });
  const r = await fetch('/api/televerser?nom=' + encodeURIComponent(fichier.name), { method: 'POST', body: fichier, headers: { 'X-Langue': langueActuelle() } });
  const j = await r.json();
  if (!r.ok) throw new Error(j.erreur || r.statusText);
  return j;
}

// A file chosen in the page, given to the engine (a table to predict, spectra...): its path there.
export async function deposer(fichier) {
  if (WEBR) return (await moteurWebR()).deposer(fichier);
  return (await (await fetch('/api/deposer?nom=' + encodeURIComponent(fichier.name), { method: 'POST', body: fichier })).json()).chemin;
}

// Files chosen together, given to the engine in one folder (a raster and its .tfw): their paths.
export async function deposerEnsemble(fichiers) {
  if (WEBR) return (await moteurWebR()).deposerEnsemble(fichiers);
  return Promise.all(fichiers.map((f) => deposer(f)));
}

// A file the engine wrote (a project saved, an export), downloaded.
export function telechargerFichier(chemin) {
  if (WEBR) moteurWebR().then((m) => m.telecharger(chemin));
  else window.location.href = '/api/fichier?chemin=' + encodeURIComponent(chemin);
}

// A file the engine wrote, its bytes (a reproducible document's scripts, put in a .zip).
export async function lireFichier(chemin) {
  if (WEBR) return new Uint8Array(await (await moteurWebR()).lireFichier(chemin));
  return new Uint8Array(await (await fetch(`/api/fichier?chemin=${encodeURIComponent(chemin)}`)).arrayBuffer());
}

// Desktop app only (see preload.js): native file dialogs.
export const bureau = window.bureau || null;
