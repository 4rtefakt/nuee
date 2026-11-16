// Recovery copies in the browser version: each open project's copy kept in the browser's
// storage (its origin private file system, OPFS, else IndexedDB), and offered again at the next
// visit, the way the desktop app keeps them in its recovery folder (app.js: sauvegarderSecours,
// proposerRecuperation; main.js). The same calls as the desktop's (preload.js: secoursDossier,
// secoursIndex, secoursListe), and two of its own: garder (a copy R wrote into webR's file
// system goes to the browser's storage) and rendre (back into webR's, to be opened).
// Each tab writes its own copies and its own index, under its name, and holds a lock (Web Locks)
// while it is open. The copies of a tab without its lock are those of a closed tab: the next tab
// to start offers them, holding that lock until they are recovered or set aside, so that two tabs
// never offer the same ones; closing the question without answering keeps them for next time.
import { moteurWebR } from './api.js';

const DOSSIER = 'nuee-secours';
const DANS_R = '/secours';
export const ONGLET = `${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 7)}`;
const verrou = (onglet) => `nuee-onglet-${onglet}`;
const nomIndex = (onglet) => `index-${onglet}.json`;
const ongletDe = (nom) => (/^index-(.+)\.json$/.exec(nom) || [])[1];
const nomCopie = (fichier) => fichier.split('/').pop();

// The tab's lock, as long as it is open (the browser version's main page only)
const PAGE = typeof window !== 'undefined' && window.NUEE_MOTEUR === 'webr' && !new URLSearchParams(window.location.search).get('vue');
if (PAGE && navigator.locks) navigator.locks.request(verrou(ONGLET), () => new Promise(() => {})).catch(() => {});

// ---- the storage: OPFS, else IndexedDB (a Safari or Firefox that lacks createWritable, a private window...)
let stockage = null;
async function opfs() {
  const racine = await navigator.storage.getDirectory();
  const d = await racine.getDirectoryHandle(DOSSIER, { create: true });
  const essai = await d.getFileHandle('.essai', { create: true });
  if (typeof essai.createWritable !== 'function') throw new Error('createWritable');
  return {
    type: 'OPFS',
    async ecrire(nom, octets) {
      const w = await (await d.getFileHandle(nom, { create: true })).createWritable();
      await w.write(octets);
      await w.close();  // (the file takes its new content only now: a tab closed before keeps the last one)
    },
    async lire(nom) { return new Uint8Array(await (await (await d.getFileHandle(nom)).getFile()).arrayBuffer()); },
    async supprimer(nom) { try { await d.removeEntry(nom); } catch { /* not there */ } },
    async noms() { const n = []; for await (const k of d.keys()) if (k !== '.essai') n.push(k); return n; },
  };
}
function indexedDb() {
  return new Promise((ok, ko) => {
    const r = indexedDB.open(DOSSIER, 1);
    r.onupgradeneeded = () => r.result.createObjectStore('fichiers');
    r.onerror = () => ko(r.error);
    r.onsuccess = () => {
      const db = r.result;
      const faire = (mode, f) => new Promise((ok2, ko2) => {
        const tx = db.transaction('fichiers', mode);
        const req = f(tx.objectStore('fichiers'));
        tx.oncomplete = () => ok2(req && req.result);
        tx.onerror = () => ko2(tx.error);
        tx.onabort = () => ko2(tx.error);
      });
      ok({
        type: 'IndexedDB',
        ecrire: (nom, octets) => faire('readwrite', (s) => s.put(octets, nom)),
        lire: async (nom) => { const v = await faire('readonly', (s) => s.get(nom)); if (!v) throw new Error(`${nom} : absent`); return new Uint8Array(v); },
        supprimer: (nom) => faire('readwrite', (s) => s.delete(nom)),
        noms: async () => [].concat(await faire('readonly', (s) => s.getAllKeys())).map(String),
      });
    };
  });
}
function lieu() {
  stockage ||= opfs().catch(() => indexedDb()).then((s) => {
    // (kept by the browser even when the disk fills up, when it allows it; Firefox asks)
    if (navigator.storage && navigator.storage.persist) navigator.storage.persisted().then((p) => p || navigator.storage.persist()).catch(() => {});
    return s;
  });
  return stockage;
}
export const typeStockage = async () => (await lieu()).type;

const texte = (o) => new TextEncoder().encode(JSON.stringify(o));
const lireJson = async (s, nom) => JSON.parse(new TextDecoder().decode(await s.lire(nom)));

// the closed tabs whose copies this one offers (it holds their locks): their index's entries
const pris = new Map();
async function prendre(onglet) {
  if (pris.has(onglet)) return true;
  if (!navigator.locks) return true;
  return new Promise((ok) => {
    navigator.locks.request(verrou(onglet), { ifAvailable: true }, (l) => {
      ok(!!l);
      if (l) return new Promise((liberer) => pris.set(onglet, liberer));
      return null;
    }).catch(() => ok(false));
  });
}

export const secoursNavigateur = {
  // where R writes a copy: a folder of webR's file system
  async secoursDossier() {
    const m = await moteurWebR();
    await m.creerDossier(DANS_R);
    return DANS_R;
  },
  // a copy R just wrote: into the browser's storage (webR's own goes)
  async garder(fichier) {
    const m = await moteurWebR();
    const octets = await m.lireFichier(fichier);
    await (await lieu()).ecrire(nomCopie(fichier), octets);
    await m.supprimerFichier(fichier);
  },
  // this tab's copies: those listed stay, the others of this tab go
  async secoursIndex(entrees) {
    const s = await lieu();
    const gardes = new Set(entrees.map((e) => nomCopie(e.fichier)));
    await s.ecrire(nomIndex(ONGLET), texte(entrees));
    for (const n of await s.noms()) if (n.startsWith(`${ONGLET}-`) && !gardes.has(n)) await s.supprimer(n);
  },
  // the copies of closed tabs, each with its tab (`onglet`); an empty index goes, and so do the
  // copies of a closed tab without an index (closed between the two)
  async secoursListe() {
    const s = await lieu();
    const noms = await s.noms();
    const entrees = [];
    for (const n of noms) {
      const onglet = ongletDe(n);
      if (!onglet || onglet === ONGLET || !(await prendre(onglet))) continue;
      let liste = [];
      try { liste = [].concat(await lireJson(s, n)); } catch { liste = []; }
      if (!liste.length) { await secoursNavigateur.oublier(onglet); continue; }
      entrees.push(...liste.map((e) => ({ ...e, onglet })));
    }
    const avecIndex = new Set(noms.map(ongletDe).filter(Boolean));
    const ouverts = navigator.locks ? new Set(((await navigator.locks.query()).held || []).map((l) => l.name)) : null;
    for (const n of noms) {
      const onglet = n.split('-').slice(0, 2).join('-');
      if (ouverts && !ongletDe(n) && onglet !== ONGLET && !avecIndex.has(onglet) && !ouverts.has(verrou(onglet))) await s.supprimer(n);
    }
    return entrees;
  },
  // a copy, back into webR's file system where `ouvrir` reads it; then gone from there
  async rendre(e) {
    const m = await moteurWebR();
    await m.creerDossier(DANS_R);
    await m.ecrireFichier(e.fichier, await (await lieu()).lire(nomCopie(e.fichier)));
  },
  async effacerDansR(e) { await (await moteurWebR()).supprimerFichier(e.fichier); },
  // the closed tabs' copies recovered or set aside: gone, but those in `garder` (unreadable, for
  // another try: their tab's index keeps only them); their locks are let go
  async oublier(onglet, garder = []) {
    const s = await lieu();
    let liste = [];
    try { liste = [].concat(await lireJson(s, nomIndex(onglet))); } catch { liste = []; }
    const restent = new Set(garder.map((e) => nomCopie(e.fichier)));
    for (const e of liste) if (!restent.has(nomCopie(e.fichier))) await s.supprimer(nomCopie(e.fichier));
    if (restent.size) await s.ecrire(nomIndex(onglet), texte(liste.filter((e) => restent.has(nomCopie(e.fichier)))));
    else await s.supprimer(nomIndex(onglet));
    if (!restent.size && pris.has(onglet)) { pris.get(onglet)(); pris.delete(onglet); }
  },
};
