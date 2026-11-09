// Requests to the local R server. Every number shown in the interface comes from there.
// X-Langue: the language of the messages the engine sends back (moteur/R/langue.R).
import { langueActuelle } from './i18n.js';

// silencieux: a request made in the background (the console's log), which doesn't report a
// stopped engine: only what the user asks for does.
export async function demander(nom, corps = {}, { silencieux = false } = {}) {
  let r;
  try {
    r = await fetch('/api/' + nom, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json', 'X-Langue': langueActuelle() },
      body: JSON.stringify(corps),
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
