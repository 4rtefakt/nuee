// Requests to the R engine: the local R server (desktop app, development), or, in the browser
// version (index.html sets window.NUEE_MOTEUR = 'webr'), R itself running in the page through
// webR (moteur_webr.js). Every number shown in the interface comes from there.
// X-Langue: the language of the messages the engine sends back (moteur/R/langue.R).
import { langueActuelle } from './i18n.js';

const WEBR = typeof window !== 'undefined' && window.NUEE_MOTEUR === 'webr';
let moteur = null;
// (the engine in the page, started at the first request: R, its packages, the engine's files)
const moteurWebR = () => (moteur ||= import('./moteur_webr.js').then((m) => m.demarrer()));

// silencieux: a request made in the background (the console's log), which doesn't report a
// stopped engine: only what the user asks for does. garder: a request that must reach the engine
// even if the window closes meanwhile (a document's last words; the browser keeps those of 64 kB
// at most).
export async function demander(nom, corps = {}, { silencieux = false, garder = false } = {}) {
  if (WEBR) {
    let r;
    try {
      r = await (await moteurWebR()).demander(nom, corps, langueActuelle());
    } catch (e) {  // R itself has stopped (out of memory...)
      if (!silencieux) window.dispatchEvent(new CustomEvent('moteur-injoignable'));
      throw e;
    }
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

// A file the engine wrote (a project saved, an export), downloaded.
export function telechargerFichier(chemin) {
  if (WEBR) moteurWebR().then((m) => m.telecharger(chemin));
  else window.location.href = '/api/fichier?chemin=' + encodeURIComponent(chemin);
}

// Desktop app only (see preload.js): native file dialogs.
export const bureau = window.bureau || null;
