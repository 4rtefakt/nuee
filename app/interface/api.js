// Requests to the local R server. Every number shown in the interface comes from there.

export async function demander(nom, corps = {}) {
  let r;
  try {
    r = await fetch('/api/' + nom, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(corps),
    });
  } catch (e) {  // no answer at all: the R engine has stopped (app.js offers to restart it)
    window.dispatchEvent(new CustomEvent('moteur-injoignable'));
    throw e;
  }
  const j = await r.json();
  if (!r.ok) throw new Error(j.erreur || r.statusText);
  return j;
}

export async function televerser(fichier) {
  const r = await fetch('/api/televerser?nom=' + encodeURIComponent(fichier.name), { method: 'POST', body: fichier });
  const j = await r.json();
  if (!r.ok) throw new Error(j.erreur || r.statusText);
  return j;
}

// Desktop app only (see preload.js): native file dialogs.
export const bureau = window.bureau || null;
