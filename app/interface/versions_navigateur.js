// History and sharing in the browser version: the desktop's app/versions.js itself runs in the page
// (an ES module of the site, app/versions.js: construire_navigateur.js), on the browser's storage as
// its file system (fichiers_navigateur.js: OPFS, else IndexedDB), with isomorphic-git's browser
// build (vendor/git.js: copier-vendor.js). interface/versions.js gets the functions of
// window.bureau.versions (preload.js). Sharing on GitHub or GitLab goes through nuee.io's relay with
// the tokens kept in this browser (comptes_navigateur.js: fetchGit, jetonsNavigateur), github.com
// and gitlab.com alone; the background exchanges as on the desktop, and when the tab comes back to
// the front (auPremierPlan).
// A project has no path in the page. Each one saved in this browser has its place, as a project on a
// computer has its folder: /projets/<key>/<name>.nuee, its file, and its history beside it in
// /projets/<key>/.nuee-versions/<name>.nuee.git; the engine's copy of that file at the same path in
// webR's file system (moteur_webr.js), where Save writes it before downloading it (app.js:
// lieuEnregistrement; Save as, a new place, the history copied there). After each save the file goes
// to the browser's storage, before its version is made (apresEnregistrement); a version restored or
// received comes back to the engine's copy, and so do a conflict's files, which the engine merges,
// while its merge goes to the storage. A project file opened in the page (/depots/<n>/<name>.nuee:
// moteur_webr.js, deposer) takes the place whose file, or one of whose versions, it is, its history
// found again; else a place of its own. A project opened from GitHub or GitLab gets a new place. A
// project recovered after a reload opens at its place again, kept with its recovery copy (app.js:
// origine).
import { langueActuelle } from './i18n.js';
import { moteurWebR } from './api.js';
import { ONGLET } from './secours_navigateur.js';
import { systemeFichiers, stockagePersistant } from './fichiers_navigateur.js';
import { fetchGit, jetonsNavigateur, exigerHoteRelais, comptesNotes, preparerComptes } from './comptes_navigateur.js';
import { nomFichier } from './versions_texte.js';

const RACINE = '/projets';
const TEMPORAIRE = '/temporaire';
const REGLAGES = 'nuee-versions-reglages';

// Node's path as versions.js uses it here: absolute paths with "/", those it is given and those it
// makes from them
const normaliser = (p) => {
  const r = [];
  for (const s of String(p).split('/')) { if (s === '..') r.pop(); else if (s && s !== '.') r.push(s); }
  return `/${r.join('/')}`;
};
const chemins = {
  sep: '/',
  isAbsolute: (p) => String(p).startsWith('/'),
  join: (...p) => normaliser(p.join('/')),
  resolve: (...p) => normaliser(p.reduce((a, x) => (String(x).startsWith('/') ? String(x) : `${a}/${x}`), '/')),
  dirname: (p) => normaliser(`${p}/..`),
  basename: (p) => normaliser(p).split('/').pop(),
};

// The settings (keep versions, the author), as main.js keeps them with the application's: in the
// browser, for this site; for this visit only when it keeps nothing
let reglagesVisite = null;
const reglages = {
  lire: () => {
    if (reglagesVisite) return reglagesVisite;
    try { return JSON.parse(localStorage.getItem(REGLAGES) || '{}') || {}; } catch { return {}; }
  },
  ecrire: (r) => { try { localStorage.setItem(REGLAGES, JSON.stringify(r)); reglagesVisite = null; } catch { reglagesVisite = r; } },
};

const existe = (fs, chemin) => fs.promises.stat(chemin).then(() => true, () => false);
async function effacer(fs, dossier) {
  for (const e of await fs.promises.readdir(dossier, { withFileTypes: true }).catch(() => [])) {
    const p = `${dossier}/${e.name}`;
    if (e.isDirectory()) await effacer(fs, p); else await fs.promises.unlink(p).catch(() => {});
  }
  await fs.promises.rmdir(dossier).catch(() => {});
}
const nouvelleCle = () => `${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 7)}`;
const nomSur = (nom) => String(nom).replace(/[\\/\u0000-\u001f]+/g, '_');
// (an object a history holds, by its oid: loose, as versions.js writes them)
const objet = (fichier, oid) => `${chemins.dirname(fichier)}/.nuee-versions/${chemins.basename(fichier)}.git/objects/${oid.slice(0, 2)}/${oid.slice(2)}`;
// (a file of the browser's storage into webR's file system, at the same path: the engine reads it there)
const versR = async ({ fs, moteur }, fichier, vers = fichier) => moteur.ecrireFichier(vers, await fs.promises.readFile(fichier));

// What the background told (versions.js: evenement), to interface/versions.js, under each path the
// engine has for that place: a version received in the engine's copy first, a conflict's files in
// webR's file system
const ecouteurs = new Set();
const resolus = new Map();  // the engine's path of a project file → its place, once known
const sousChemins = (ici) => new Set([ici, ...[...resolus].filter(([, v]) => v === ici).map(([k]) => k)]);
let noyau = null;
async function diffuser(e) {
  const ici = e && e.chemin;
  const c = await noyau;
  if (c && e.type === 'recu') for (const chemin of sousChemins(ici)) await versR(c, ici, chemin).catch(() => {});
  if (c && e.type === 'conflit') await fichiersConflit(c, e.fichiers).catch(() => {});
  for (const chemin of sousChemins(ici)) for (const f of ecouteurs) { try { f({ ...e, chemin }); } catch { /* the next one */ } }
}
// A conflict's three files (versions.js: detailsConflit, in the storage's temporary folder) into
// webR's file system, where the engine's merge reads them
async function fichiersConflit(c, fichiers) {
  const f = fichiers || {};
  for (const k of ['base', 'mien', 'leur']) if (typeof f[k] === 'string' && f[k]) await versR(c, f[k]);
  const un = ['base', 'mien', 'leur'].map((k) => f[k]).find((x) => typeof x === 'string' && x);
  if (un) await effacer(c.fs, chemins.dirname(un));
}

// versions.js, isomorphic-git and the storage, loaded at the first use
function charger() {
  noyau ||= (async () => {
    const [{ git, Buffer, Sha1 }, { default: chargerVersions }, fs, moteur] = await Promise.all([
      import('./vendor/git.js'), import('./app/versions.js'), systemeFichiers(), moteurWebR()]);
    // (isomorphic-git reads Buffer from the page)
    globalThis.Buffer ||= Buffer;
    const modules = {
      fs, path: chemins, 'isomorphic-git': git,
      // (the lock of a history names no computer here: another tab holds it)
      os: { hostname: () => '', userInfo: () => ({ username: 'Nuée' }) },
      crypto: {
        randomUUID: () => crypto.randomUUID(),
        createHash: (algorithme) => { if (algorithme !== 'sha1') throw new Error(`versions.js: ${algorithme} isn't in the page`); return new Sha1(); },
      },
      child_process: { execFile: () => {} },
    };
    const requis = (nom) => {
      if (!Object.hasOwn(modules, nom)) throw new Error(`versions.js: ${nom} isn't in the page`);
      return modules[nom];
    };
    const { creerVersions } = chargerVersions(requis, { platform: 'browser', env: {}, pid: ONGLET }, Buffer);
    await fs.promises.mkdir(TEMPORAIRE, { recursive: true });
    const v = creerVersions({
      fetch: fetchGit,
      jetons: jetonsNavigateur,
      langue: () => langueActuelle(),
      reglages,
      temporaire: () => TEMPORAIRE,
      evenement: (e) => { diffuser(e).catch(() => {}); },
      utilisateur: () => 'Nuée',
    });
    // (the tab back in front: what the shared projects' repositories have, every five minutes at
    // most, as when a window of the desktop application comes to the front)
    const devant = () => { if (document.visibilityState === 'visible') v.auPremierPlan(); };
    document.addEventListener('visibilitychange', devant);
    window.addEventListener('focus', devant);
    // (an account signed in here: its token renewed before it ends, GitLab's)
    if (comptesNotes()) preparerComptes();
    return { git, fs, moteur, v };
  })();
  noyau.catch(() => { noyau = null; });
  return noyau;
}

// The place that has a project file, by its content (`oid`, git's): the one whose file it is, else
// one of whose versions it is; the most recently written first. { fichier, pareil } or null
async function chercher({ git, fs }, oid, taille) {
  let meilleur = null;
  for (const cle of await fs.promises.readdir(RACINE).catch(() => [])) {
    const dossier = `${RACINE}/${cle}`;
    const nom = (await fs.promises.readdir(dossier).catch(() => [])).find((n) => /\.nuee$/i.test(n));
    if (!nom) continue;
    const fichier = `${dossier}/${nom}`;
    const st = await fs.promises.stat(fichier).catch(() => null);
    if (!st) continue;
    const pareil = st.size === taille && (await git.hashBlob({ object: await fs.promises.readFile(fichier) })).oid === oid;
    if (!pareil && !(await existe(fs, objet(fichier, oid)))) continue;
    if (!meilleur || pareil > meilleur.pareil || (pareil === meilleur.pareil && st.mtimeMs > meilleur.date)) meilleur = { fichier, pareil, date: st.mtimeMs };
  }
  return meilleur;
}

// A project file's place (`chemin`: the engine's path of it)
async function placer(chemin) {
  const c = await charger();
  const { git, fs, moteur, v } = c;
  if (chemin.startsWith(`${RACINE}/`)) {
    // (saved in this tab, or recovered after a reload: its own place, the engine's copy put back)
    if (!(await moteur.existeFichier(chemin)) && (await existe(fs, chemin))) await versR(c, chemin);
    return chemin;
  }
  // (nothing else but a file chosen in the page has a place: versions.js says it isn't there)
  if (!chemin.startsWith('/depots/')) return chemin;
  // a project file opened in the page
  const octets = await moteur.lireFichier(chemin);
  const { oid } = await git.hashBlob({ object: octets });
  const trouve = await chercher(c, oid, octets.length);
  if (trouve && trouve.pareil) return trouve.fichier;
  if (trouve) {
    // (its file holds another state, downloaded since: kept in a version first unless one holds it,
    // then this file in its place)
    const ici = await fs.promises.readFile(trouve.fichier);
    if (!(await existe(fs, objet(trouve.fichier, (await git.hashBlob({ object: ici })).oid)))) await v.marquer(trouve.fichier, '').catch(() => {});
    await fs.promises.writeFile(trouve.fichier, octets);
    return trouve.fichier;
  }
  const nouveau = `${RACINE}/${nouvelleCle()}/${nomSur(chemins.basename(chemin))}`;
  await fs.promises.mkdir(chemins.dirname(nouveau), { recursive: true });
  await fs.promises.writeFile(nouveau, octets);
  return nouveau;
}
const lieux = new Map();  // the engine's path of a project file → the promise of its place
function lieu(chemin) {
  // (not a project file: versions.js refuses it, in the user's words)
  if (typeof chemin !== 'string' || !/\.nuee$/i.test(chemin)) return Promise.resolve(chemin);
  if (!lieux.has(chemin)) {
    const p = placer(chemin).then((ici) => { resolus.set(chemin, ici); return ici; });
    lieux.set(chemin, p);
    p.catch(() => lieux.delete(chemin));
  }
  return lieux.get(chemin);
}

// One function at a time for a place, in the order they were asked (a save's file in the storage
// before the version that follows it)
const files = new Map();
function enFile(cle, f) {
  const r = (files.get(cle) || Promise.resolve()).then(f);
  const suite = r.catch(() => {});
  files.set(cle, suite);
  suite.then(() => { if (files.get(cle) === suite) files.delete(cle); });
  return r;
}
// A function of versions.js on a project's place; what it needs before and after
const sur = (nom, { avant = null, apres = null } = {}) => async (chemin, ...args) => {
  const c = await charger();
  const ici = await lieu(chemin);
  return enFile(ici, async () => {
    if (avant) await avant(c, chemin, ici);
    const r = await c.v[nom](ici, ...args);
    return apres ? apres(c, chemin, ici, r) : r;
  });
};
// (an exchange's answer: a version received in the engine's copy; a conflict's files in webR's)
const apresEchange = async (c, chemin, ici, r) => {
  if (r && r.resultat === 'recu') await versR(c, ici, chemin);
  if (r && r.fichiers) await fichiersConflit(c, r.fichiers);
  return r;
};
// (a repository's address: github.com's and gitlab.com's alone, those nuee.io's relay reaches; what
// isn't an https address is versions.js's to refuse, in its words)
function exigerDepot(url) {
  let u = null;
  try { u = new URL(String(url == null ? '' : url).trim()); } catch { return; }
  if (u.protocol === 'https:') exigerHoteRelais(u.hostname);
}

const apresEnregistrementSur = sur('apresEnregistrement', {
  // (the file the engine has just written at the project's place, into the browser's storage)
  avant: async ({ fs, moteur }, chemin, ici) => {
    if (chemin !== ici) return;
    const octets = await moteur.lireFichier(chemin);
    await fs.promises.mkdir(chemins.dirname(ici), { recursive: true });
    await fs.promises.writeFile(ici, octets);
  },
});
const partagerSur = sur('partager');
const choisirSur = sur('choisir', {
  // (theirs, or the merge: the project's file changed, the engine's copy too)
  apres: async (c, chemin, ici, r) => { await versR(c, ici, chemin); return r; },
});
export const versionsNavigateur = {
  // (where the versions are: this browser's storage, and whether it keeps it for good)
  etat: sur('etat', { apres: async ({ fs }, chemin, ici, r) => ({ ...r, stockage: fs.type, persistant: await stockagePersistant() }) }),
  ouvert: sur('ouvert'),
  // (after Save as, a new place: the former place's history copied there, as versions.js does it)
  apresEnregistrement: (chemin, options = {}) => {
    const a = options.ancien;
    const ancien = typeof a === 'string' ? (a.startsWith(`${RACINE}/`) ? a : resolus.get(a)) : null;
    return apresEnregistrementSur(chemin, { ...(ancien ? { ancien } : {}), ...(options.auto === false ? { auto: false } : {}) });
  },
  aLaFermeture: sur('aLaFermeture'),
  marquer: sur('marquer'),
  liste: sur('liste'),
  nommer: sur('nommer'),
  retirerNom: sur('retirerNom'),
  // (the version read into webR's file system, where the engine opens it: the same path)
  lire: sur('lire', {
    apres: async (c, chemin, ici, r) => {
      try { await versR(c, r.fichier); } finally { await effacer(c.fs, chemins.dirname(r.fichier)); }
      return r;
    },
  }),
  // (the version restored into the engine's copy of the file, which the project is read again from)
  restaurer: sur('restaurer', { apres: async (c, chemin, ici, r) => { await versR(c, ici, chemin); return r; } }),
  activer: sur('activer'),
  reglages: async () => (await charger()).v.reglages(),
  regler: async (r) => (await charger()).v.regler(r || {}),
  surVersions: (f) => { ecouteurs.add(f); },

  // sharing, with github.com and gitlab.com
  partager: async (chemin, url) => { exigerDepot(url); return partagerSur(chemin, url); },
  jeton: async (hote, valeur) => (await charger()).v.jeton(exigerHoteRelais(hote), valeur),
  verifierPartage: sur('verifierPartage'),
  envoyer: sur('envoyer', { apres: apresEchange }),
  recevoir: sur('recevoir', { apres: apresEchange }),
  // (a merge: the file the engine wrote, from webR's file system into the storage's temporary folder)
  async choisir(chemin, cote, fichier = null, description = null) {
    const c = await charger();
    if (cote !== 'fusion' || typeof fichier !== 'string') return choisirSur(chemin, cote, fichier, description);
    const dossier = await c.fs.promises.mkdtemp(`${TEMPORAIRE}/fusion-`);
    const ici = `${dossier}/${nomSur(chemins.basename(fichier))}`;
    try {
      await c.fs.promises.writeFile(ici, await c.moteur.lireFichier(fichier));
      return await choisirSur(chemin, cote, ici, description);
    } finally { await effacer(c.fs, dossier); }
  },
  arreterPartage: sur('arreterPartage'),
  listerDistant: async (url) => { exigerDepot(url); return (await charger()).v.listerDistant(url); },
  // (a new place, whatever folder is given: the project's file then in the engine's copy, which opens)
  async ouvrirDistant(url, cheminDistant) {
    exigerDepot(url);
    const c = await charger();
    const dossier = `${RACINE}/${nouvelleCle()}`;
    await c.fs.promises.mkdir(dossier, { recursive: true });
    let r;
    try { r = await c.v.ouvrirDistant(url, cheminDistant, dossier); } catch (e) { await effacer(c.fs, dossier); throw e; }
    await versR(c, r.chemin);
    return r;
  },
};

// Where Save writes a project in webR's file system (app.js: enregistrer), its folder made: its
// place (one opened from a file: the place found or made for it); a new place for a project without
// one, for Save as (`sous`: its history follows, apresEnregistrement) and for an old version opened
// read-only, saved as a project of its own
export async function lieuEnregistrement(p, { sous = false } = {}) {
  const f = p.chemin_enregistrement;
  const ici = !sous && !p.lecture_seule && typeof f === 'string' && /\.nuee$/i.test(f) ? await lieu(f)
    : `${RACINE}/${nouvelleCle()}/${nomSur(`${nomFichier(p.lecture_seule ? p.nom : String(p.nom || '').replace(/\.[^.]+$/, ''))}.nuee`)}`;
  await (await moteurWebR()).creerDossier(chemins.dirname(ici));
  return ici;
}

// A project's place, when it has one and it is known (the recovery copies: app.js)
export function lieuConnu(p) {
  const f = p && !p.lecture_seule ? p.chemin_enregistrement : null;
  if (typeof f !== 'string' || !/\.nuee$/i.test(f)) return null;
  return f.startsWith(`${RACINE}/`) ? f : resolus.get(f) || null;
}
