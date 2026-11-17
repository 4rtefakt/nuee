// Requests to the local R server. Every number shown in the interface comes from there.
// X-Langue: the language of the messages the engine sends back (moteur/R/langue.R).
import { langueActuelle } from './i18n.js';

// The requests the user waits for (not those made in the background), by number, and when each
// began: after a moment the status bar offers to stop the computation (app.js, Stop). A change
// is told to the page ('requetes').
const enAttente = new Map();
let numeroRequete = 0;
export const debutAttente = () => (enAttente.size ? Math.min(...enAttente.values()) : null);
const attente = (n, oui) => {
  if (oui) enAttente.set(n, Date.now()); else enAttente.delete(n);
  window.dispatchEvent(new CustomEvent('requetes'));
};

// silencieux: a request made in the background (the console's log), which doesn't report a
// stopped engine: only what the user asks for does. garder: a request that must reach the engine
// even if the window closes meanwhile (a document's last words; the browser keeps those of 64 kB
// at most).
export async function demander(nom, corps = {}, { silencieux = false, garder = false } = {}) {
  const n = silencieux ? 0 : (numeroRequete += 1);
  if (n) attente(n, true);
  try { return await envoyer(nom, corps, silencieux, garder); } finally { if (n) attente(n, false); }
}

async function envoyer(nom, corps, silencieux, garder) {
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

export async function televerser(fichier) {
  const r = await fetch('/api/televerser?nom=' + encodeURIComponent(fichier.name), { method: 'POST', body: fichier, headers: { 'X-Langue': langueActuelle() } });
  const j = await r.json();
  if (!r.ok) throw new Error(j.erreur || r.statusText);
  return j;
}

// Desktop app only (see preload.js): native file dialogs.
export const bureau = window.bureau || null;
