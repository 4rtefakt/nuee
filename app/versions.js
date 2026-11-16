// Versions of a project, kept in the background with git without git to install (isomorphic-git,
// MIT), and shared with GitHub or GitLab over HTTPS with a personal access token.
// - Each project file <folder>/<name>.nuee has its history in a bare repository,
//   <folder>/.nuee-versions/<name>.nuee.git. That folder is hidden and holds a .gitignore of "*",
//   so a git repository of the user's around it sees nothing of it, and is never touched. In the
//   history's tree the project is the path its config records (nuee.chemin, <name>.nuee at the
//   root by default); every other path belongs to the repository it is shared with, and is carried
//   over as it is. The project file is the only file written in the user's folder (a version
//   restored or received), as the engine saves it: a temporary file, then renamed over it.
// - Versions are made automatically (on by default): when a project is saved and its last version
//   is more than an hour old, when it is closed or Nuée quits, before a data update (marquer); an
//   automatic one says what changed (decrireChangements). Named versions are annotated tags. The
//   automatic versions never sent anywhere are thinned out, once a day at most (eclaircir).
// - Shared: receiving brings their branch in path by path (integrer), a conflict for the user
//   when both sides changed the project; sending receives first. Both also run in the background,
//   after each version, when a project opens and when Nuée comes back to the front, and tell the
//   interface what happened (evenement).
// - A token is never logged, never put in an address, and never handed back once stored.
// - A repository is written by one command at a time in this process, and by one Nuée at a time
//   across computers: a lock file beside it, stale after two minutes.
// Each function takes the project file's absolute path unless said; its errors are thrown with a
// message in the user's language (`langue`). main.js gives net.fetch, safeStorage, the settings and
// the windows; tests/essai_versions.js gives others.
const crypto = require('crypto');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFile } = require('child_process');
const git = require('isomorphic-git');

const DOSSIER = '.nuee-versions';
const MINUTE = 60e3;
const HEURE = 60 * MINUTE;
const JOUR = 24 * HEURE;
// (the last line of the message of an automatic version, and of a choice between two versions)
const AUTO = 'Nuee: auto';
const CHOIX = 'Nuee: choix';
const insensible = process.platform === 'win32' || process.platform === 'darwin';

const pause = (ms) => new Promise((ok) => setTimeout(ok, ms));
const existe = (p) => fs.promises.stat(p).then(() => true, () => false);

// An error meant for the user, its message in their language
class Refus extends Error {}

// A version's name as a tag's (refs/tags/<name>). Git's names and Windows's file names refuse
// spaces, control characters, ~ ^ : ? * [ \ < > " |, "..", "@{", a "." or "-" at either end and
// ".lock"; "/" would make folders. Each run of them becomes "-". The name as typed is the tag's
// message, which the list shows (tags()).
function nomRef(nom) {
  let r = String(nom).normalize('NFC').replace(/[\u0000-\u0020\u007f~^:?*[\\<>"|/]+/g, '-')
    .replace(/\.{2,}/g, '.').replace(/@\{/g, '@-').replace(/-{2,}/g, '-').slice(0, 100)
    .replace(/^[-.]+|[-.]+$/g, '');
  if (/\.lock$/i.test(r)) r = `${r.slice(0, -5)}-lock`;
  if (r === '@') return '';
  // (CON, NUL, COM1… aren't file names on Windows)
  return /^(con|prn|aux|nul|com\d|lpt\d)(\.|$)/i.test(r) ? `_${r}` : r;
}

// A repository's address as pasted (from its page, or its Code or Clone button): https only,
// without a user name or token; the query and fragment dropped, a GitHub page cut to its owner and
// repository, a GitLab page at "/-/"; ending in .git, which GitHub and GitLab both answer (GitLab
// redirects the address without it, and a POST redirected comes back a GET). { url, hote }, or
// { refus: 'adresse' | 'identifiants' }.
function adresseDistante(texte) {
  let u;
  try { u = new URL(String(texte == null ? '' : texte).trim()); } catch { return { refus: 'adresse' }; }
  if (u.protocol !== 'https:' || !u.hostname) return { refus: 'adresse' };
  if (u.username || u.password) return { refus: 'identifiants' };
  let parties = u.pathname.split('/').filter(Boolean);
  const tiret = parties.indexOf('-');
  if (tiret >= 0) parties = parties.slice(0, tiret);
  if (u.hostname === 'github.com') parties = parties.slice(0, 2);
  if (!parties.length || (u.hostname === 'github.com' && parties.length < 2)) return { refus: 'adresse' };
  const chemin = parties.join('/');
  return { url: `https://${u.host}/${/\.git$/i.test(chemin) ? chemin : `${chemin}.git`}`, hote: u.host };
}

// The server of a repository's address, if Nuée can reach it (https), else null (an SSH address
// set with git, for instance)
function hoteDe(url) {
  try { const u = new URL(url); return u.protocol === 'https:' ? u.host : null; } catch { return null; }
}

// isomorphic-git's HTTP client (its `http` option) over a fetch: in main.js, Electron's net.fetch,
// Chromium's network stack, which goes through the system's proxy (PAC, WPAD) and trusts its
// certificates (an institute's own authority), where Node's own client would do neither. No
// cookie is sent, and nothing is logged: the headers hold the token.
function clientHttp(fetch) {
  return {
    async request({ url, method = 'GET', headers = {}, body }) {
      let corps;
      if (body) {
        const morceaux = [];
        for await (const m of body) morceaux.push(Buffer.from(m));
        corps = Buffer.concat(morceaux);
      }
      const r = await fetch(url, { method, headers, body: corps, credentials: 'omit' });
      const entetes = {};
      r.headers.forEach((valeur, nom) => { entetes[nom] = valeur; });
      return { url: r.url, method, statusCode: r.status, statusMessage: r.statusText, headers: entetes, body: r.body || [] };
    },
  };
}

// The file system as isomorphic-git needs it for a repository. It joins paths with "/" and keeps
// a drive letter, but loses the "\\" of a UNC path: \\server\share\folder becomes
// /server/share/folder, which Windows would look for on the current drive. On a network share,
// its paths get their "\\" back.
function fsPour(racine) {
  if (process.platform !== 'win32' || !/^[\\/]{2}[^\\/?.]/.test(racine)) return fs;
  const reel = (p) => (typeof p === 'string' && /^\/[^/]/.test(p) ? `\\\\${p.slice(1).replace(/\//g, '\\')}` : p);
  const f = fs.promises;
  return {
    promises: {
      readFile: (p, ...a) => f.readFile(reel(p), ...a),
      writeFile: (p, ...a) => f.writeFile(reel(p), ...a),
      mkdir: (p, ...a) => f.mkdir(reel(p), ...a),
      rmdir: (p, ...a) => f.rmdir(reel(p), ...a),
      unlink: (p, ...a) => f.unlink(reel(p), ...a),
      stat: (p, ...a) => f.stat(reel(p), ...a),
      lstat: (p, ...a) => f.lstat(reel(p), ...a),
      readdir: (p, ...a) => f.readdir(reel(p), ...a),
      readlink: (p, ...a) => f.readlink(reel(p), ...a),
      symlink: (cible, p, ...a) => f.symlink(cible, reel(p), ...a),
      rm: (p, ...a) => f.rm(reel(p), ...a),
    },
  };
}

// The tokens, one per server (github.com, gitlab.com, an institute's GitLab), encrypted by the
// system with Electron's safeStorage (DPAPI on Windows, the Keychain on macOS, the keyring on
// Linux; its asynchronous functions, the others being deprecated) in a file of the user's
// settings (`fichier()`). Where it can't encrypt (Linux without a keyring, where Chromium would
// keep them in clear: basic_text), they are kept in memory until Nuée closes, and nothing is
// written.
function magasinJetons({ fichier, safeStorage }) {
  const session = new Map();
  const chiffrable = async () => {
    try {
      return await safeStorage.isAsyncEncryptionAvailable()
        && !(process.platform === 'linux' && ['basic_text', 'unknown'].includes(safeStorage.getSelectedStorageBackend()));
    } catch { return false; }
  };
  const lus = () => { try { return JSON.parse(fs.readFileSync(fichier(), 'utf8')) || {}; } catch { return {}; } };
  const garder = (tous) => {
    fs.mkdirSync(path.dirname(fichier()), { recursive: true });
    fs.writeFileSync(fichier(), JSON.stringify(tous, null, 1), { mode: 0o600 });
  };
  const chiffre = (hote) => { const t = lus(); return Object.hasOwn(t, hote) && typeof t[hote] === 'string' ? t[hote] : null; };
  // (encrypted again when the system says its key has changed)
  async function dechiffrer(hote, b64) {
    try {
      const { shouldReEncrypt, result } = await safeStorage.decryptStringAsync(Buffer.from(b64, 'base64'));
      if (shouldReEncrypt && result) garder({ ...lus(), [hote]: (await safeStorage.encryptStringAsync(result)).toString('base64') });
      return result || null;
    } catch { return null; }
  }
  // (the system's encryptor is asked for only when a token is there: started, it may ask for the
  // Keychain on macOS, again after each update of an application signed ad hoc)
  return {
    seulementSession: async () => !(await chiffrable()),
    present: async (hote) => session.has(hote) || (chiffre(hote) !== null && await chiffrable()),
    lire: async (hote) => session.get(hote) || (chiffre(hote) && await chiffrable() ? dechiffrer(hote, chiffre(hote)) : null),
    // (every token known, to blank one from any message)
    connus: async () => {
      const chiffres = Object.entries(lus()).filter(([, x]) => typeof x === 'string');
      return [...session.values(), ...(chiffres.length && await chiffrable() ? await Promise.all(chiffres.map(([h, x]) => dechiffrer(h, x))) : [])].filter(Boolean);
    },
    async ecrire(hote, jeton) {
      session.delete(hote);
      const tous = lus();
      const avait = Object.hasOwn(tous, hote);
      delete tous[hote];
      const chiffrer = jeton && await chiffrable();
      if (jeton && !chiffrer) session.set(hote, jeton);
      else if (jeton) tous[hote] = (await safeStorage.encryptStringAsync(jeton)).toString('base64');
      if (avait || chiffrer) garder(tous);
    },
  };
}

// What changed from one version of a project to the next, in a few words read from the two files
// (their JSON; `avant` null: the first version, undefined: unreadable), in French or English: the
// data's rows and variables (or their values), the models by name, the documents and figures by
// title, the steps, the data's source file.
function decrireChangements(avant, apres, fr = false) {
  const t = (f, e) => (fr ? f : e);
  if (avant === null) return t('Première version', 'First version');
  if (!avant || !apres || typeof avant !== 'object' || typeof apres !== 'object') return t('Projet modifié', 'Project changed');
  const n = (x) => (Array.isArray(x) ? x.length : 0);
  const pareil = (a, b) => JSON.stringify(a === undefined ? null : a) === JSON.stringify(b === undefined ? null : b);
  const ecart = (a, b) => (a === b ? '' : ` (${a > b ? '+' : '−'}${Math.abs(a - b)})`);
  const q = (s) => (fr ? `« ${s} »` : `“${s}”`);
  const enLigne = (noms) => (noms.length > 3 ? `${noms.slice(0, 3).join(', ')} ${t(`et ${noms.length - 3} autres`, `and ${noms.length - 3} more`)}` : noms.join(', '));
  const parties = [];
  const [lignes, lignesAvant, vars, varsAvant] = [n(apres.observations), n(avant.observations), n(apres.variables), n(avant.variables)];
  if (lignes !== lignesAvant || vars !== varsAvant) {
    const m = [];
    if (lignes !== lignesAvant) m.push(`${lignes} ${lignes === 1 ? t('ligne', 'row') : t('lignes', 'rows')}${ecart(lignes, lignesAvant)}`);
    if (vars !== varsAvant) m.push(`${vars} variable${vars === 1 ? '' : 's'}${ecart(vars, varsAvant)}`);
    parties.push(`${t('données :', 'data:')} ${m.join(', ')}`);
  } else if (!pareil(avant.datasets, apres.datasets) || !pareil(avant.observations, apres.observations) || !pareil(avant.variables, apres.variables)) {
    parties.push(t('données modifiées', 'data changed'));
  }
  // (what a list keeps by a key: models by name, documents and figures by id, said by their title;
  // in French the participle agrees with the thing and its number)
  const dire = (a, b, cle, titre, quoi) => {
    const avantPar = new Map((Array.isArray(a) ? a : []).map((x) => [String(x && x[cle]), x]));
    const apresPar = new Map((Array.isArray(b) ? b : []).map((x) => [String(x && x[cle]), x]));
    const nom = (x, k) => (titre ? q((x && x[titre]) || k) : k);
    const groupes = [
      ['added', 'ajouté', [...apresPar.keys()].filter((k) => !avantPar.has(k)).map((k) => nom(apresPar.get(k), k))],
      ['removed', 'retiré', [...avantPar.keys()].filter((k) => !apresPar.has(k)).map((k) => nom(avantPar.get(k), k))],
      ['changed', 'modifié', [...apresPar.keys()].filter((k) => avantPar.has(k) && !pareil(avantPar.get(k), apresPar.get(k))).map((k) => nom(apresPar.get(k), k))],
    ];
    for (const [en, participe, noms] of groupes) {
      if (!noms.length) continue;
      const accord = `${participe}${quoi && quoi.feminin ? 'e' : ''}${noms.length > 1 ? 's' : ''}`;
      parties.push(`${quoi ? `${noms.length > 1 ? quoi.pluriel : quoi.singulier} ` : ''}${enLigne(noms)} ${fr ? accord : en}`);
    }
  };
  dire(avant.models, apres.models, 'name', null, null);
  dire(avant.documents, apres.documents, 'id', 'titre', { singulier: 'document', pluriel: 'documents' });
  dire(avant.figures, apres.figures, 'id', 'titre', { singulier: 'figure', pluriel: 'figures', feminin: true });
  const etapes = n(apres.etapes) - n(avant.etapes);
  const derniere = n(apres.etapes) ? apres.etapes[apres.etapes.length - 1] : null;
  if (etapes === 1 && derniere && typeof derniere.etiquette === 'string' && derniere.etiquette) {
    parties.push(t(`étape ${q(derniere.etiquette)} ajoutée`, `step ${q(derniere.etiquette)} added`));
  } else if (etapes > 0) {
    parties.push(t(`${etapes} étapes ajoutées`, `${etapes} steps added`));
  } else if (etapes < 0) {
    parties.push(t(`${-etapes} étape${etapes < -1 ? 's' : ''} retirée${etapes < -1 ? 's' : ''}`, `${-etapes} step${etapes < -1 ? 's' : ''} removed`));
  }
  if (!pareil(avant.source_donnees, apres.source_donnees)) {
    const s = apres.source_donnees;
    const fichier = s && typeof s === 'object' ? String(s.relatif || s.chemin || '').split(/[\\/]/).pop() : '';
    parties.push(fichier ? t(`source des données : ${fichier}`, `data source: ${fichier}`) : t('source des données retirée', 'data source removed'));
  }
  if (!parties.length) return t('Projet modifié', 'Project changed');
  const texte = parties.join(fr ? ' ; ' : '; ');
  return texte.charAt(0).toUpperCase() + texte.slice(1);
}

// The message of a commit as a version shows it: its description, and whether it was made
// automatically or is a choice between two versions (its last line)
function marque(message) {
  const lignes = String(message || '').replace(/\r/g, '').trim().split('\n');
  return {
    description: lignes.filter((l) => l !== AUTO && l !== CHOIX).join('\n').trim(),
    auto: lignes.includes(AUTO),
    choix: lignes.includes(CHOIX),
  };
}

// A folder copied, then removed, file by file: Node's cp and rm on Windows leave read-only files
// in some versions (git's objects) and paths that aren't ASCII in others
async function copierDossier(de, vers) {
  await fs.promises.mkdir(vers, { recursive: true });
  for (const e of await fs.promises.readdir(de, { withFileTypes: true })) {
    if (e.isDirectory()) await copierDossier(path.join(de, e.name), path.join(vers, e.name));
    else await fs.promises.copyFile(path.join(de, e.name), path.join(vers, e.name));
  }
}
async function effacerDossier(d) {
  let entrees = [];
  try { entrees = await fs.promises.readdir(d, { withFileTypes: true }); } catch { return; }
  for (const e of entrees) {
    const p = path.join(d, e.name);
    if (e.isDirectory()) await effacerDossier(p);
    else { await fs.promises.chmod(p, 0o666).catch(() => {}); await fs.promises.unlink(p); }
  }
  await fs.promises.rmdir(d);
}

// The functions of window.bureau.versions (preload.js, main.js).
//   fetch: (url, init) → Response (main.js: net.fetch)
//   jetons: magasinJetons(...)
//   langue: () → 'fr' | 'en', the interface's
//   reglages: { lire: () → { auto, auteur }, ecrire(r) }, kept with the application's settings
//   temporaire: () → the folder where the versions read (lire) and the remote's listings go
//   evenement: (e) → told to the windows
//   maintenant, utilisateur: the clock and the system's user name (the checks give their own)
// The returned object also has, not enumerable (main.js leaves them out of the page's reach):
// auPremierPlan() (a window came to the front), fermerTout() (Nuée quits), enAttente() (the
// background exchanges started, ended).
function creerVersions({ fetch, jetons, langue = () => 'en', reglages: stockes, temporaire, evenement = () => {},
  maintenant = () => Date.now(), utilisateur = () => { try { return os.userInfo().username; } catch { return 'Nuée'; } } }) {
  const http = clientHttp(fetch);
  const t = (fr, en) => (langue() === 'fr' ? fr : en);
  const refus = (fr, en) => new Refus(t(fr, en));
  const cle = (p) => (insensible ? path.resolve(p).toLowerCase() : path.resolve(p));

  // Objects read once: an oid names the same content in every repository
  const commits = new Map();
  const arbres = new Map();
  const ancetresMemo = new Map();
  const listes = new Map();
  const empreintes = new Map();
  const tailles = new Map();
  const garder = (m, k, valeur, max = 50000) => { if (m.size >= max) m.clear(); m.set(k, valeur); return valeur; };
  // The projects open in a window, with or without changes not saved (ouvert); the last error of
  // each one's background exchange
  const ouverts = new Map();
  const erreurs = new Map();

  // ------------------------------------------------------------------ settings

  function reglagesActuels() {
    const r = (stockes && stockes.lire()) || {};
    const a = r.auteur && typeof r.auteur.nom === 'string' && r.auteur.nom ? r.auteur : null;
    return { auto: r.auto !== false, auteur: a ? { nom: a.nom, email: String(a.email || '') } : { nom: utilisateur(), email: '' } };
  }
  function auteurGit() {
    const { auteur } = reglagesActuels();
    return { name: auteur.nom, email: auteur.email, timestamp: Math.floor(maintenant() / 1000), timezoneOffset: new Date(maintenant()).getTimezoneOffset() };
  }

  // ------------------------------------------------------------------ the project and its history

  // The project file: its path, folder and name (as the disk writes it: the history's name is made
  // from it, whatever case it was typed in)
  async function projet(chemin) {
    if (typeof chemin !== 'string' || !path.isAbsolute(chemin) || !/\.nuee$/i.test(chemin)) {
      throw refus('Les versions se gardent pour un fichier de projet (.nuee), donné par son chemin complet.',
        'Versions are kept for a project file (.nuee), given by its full path.');
    }
    const c = path.resolve(chemin);
    let st = null;
    try { st = await fs.promises.stat(c); } catch { /* below */ }
    if (!st || !st.isFile()) throw refus(`Le fichier du projet n’est plus là : ${c}`, `The project file isn’t there any more: ${c}`);
    const dossier = path.dirname(c);
    let nom = path.basename(c);
    try {
      const noms = await fs.promises.readdir(dossier);
      if (!noms.includes(nom)) nom = noms.find((x) => x.normalize('NFC').toLowerCase() === nom.normalize('NFC').toLowerCase()) || nom;
    } catch { /* as given */ }
    return { chemin: path.join(dossier, nom), dossier, nom };
  }

  // Where a project's history is: the hidden folder, the bare repository, its lock
  function lieux(p) {
    const versions = path.join(p.dossier, DOSSIER);
    const gitdir = path.join(versions, `${p.nom}.git`);
    return { ...p, versions, gitdir, verrou: path.join(versions, `${p.nom}.lock`), fs: fsPour(gitdir) };
  }
  const g = (d) => ({ fs: d.fs, gitdir: d.gitdir });

  async function depot(chemin, { retrouver = false } = {}) {
    const d = lieux(await projet(chemin));
    d.existe = await existe(path.join(d.gitdir, 'HEAD'));
    if (!d.existe && retrouver) await retrouverRenomme(d);
    if (d.existe) await lireDepot(d);
    return d;
  }

  // The hidden folder beside the project: git, for a repository of the user's around it, sees
  // nothing of it; hidden as git hides .git on Windows
  async function preparerDossier(d) {
    await fs.promises.mkdir(d.versions, { recursive: true });
    const ignorer = path.join(d.versions, '.gitignore');
    if (await existe(ignorer)) return;
    await fs.promises.writeFile(ignorer, '*\n');
    if (process.platform === 'win32') execFile('attrib', ['+h', d.versions], { windowsHide: true }, () => {});
  }

  async function creerDepot(d) {
    await preparerDossier(d);
    await git.init({ ...g(d), bare: true, defaultBranch: 'main' });
    if (!(await config(d, 'nuee.chemin'))) await git.setConfig({ ...g(d), path: 'nuee.chemin', value: d.nom });
    d.existe = true;
    await lireDepot(d);
  }

  const config = async (d, cheminConfig) => {
    const v = await git.getConfig({ ...g(d), path: cheminConfig });
    return v === undefined || v === null ? null : String(v);
  };
  const ecrireConfig = (d, cheminConfig, value) => git.setConfig({ ...g(d), path: cheminConfig, value });

  async function lireDepot(d) {
    d.branche = (await git.currentBranch(g(d))) || 'main';
    const suit = await config(d, `branch.${d.branche}.merge`);
    d.brancheDistante = suit ? suit.replace(/^refs\/heads\//, '') : d.branche;
    d.cheminProjet = (await config(d, 'nuee.chemin')) || d.nom;
    // (each change of path: the last commit where the project was at the former one, and that path)
    d.anciens = ((await git.getConfigAll({ ...g(d), path: 'nuee.ancien' })) || []).map(String)
      .map((x) => /^([0-9a-f]{40}) (.+)$/.exec(x)).filter(Boolean).map((m) => ({ jusqua: m[1], chemin: m[2] }));
    const actif = await config(d, 'nuee.actif');
    d.reglage = actif === null ? null : actif === 'true';
    d.tete = await resoudre(d, `refs/heads/${d.branche}`);
    d.arbreTete = d.tete ? (await lireCommit(d, d.tete)).tree : null;
    d.blobTete = await blobDe(d, d.tete);
    d.url = await config(d, 'remote.origin.url');
    d.hote = d.url ? hoteDe(d.url) : null;
  }

  // Versions kept for this project: its own switch, else the setting
  const actifPour = (d) => (d.existe && d.reglage !== null ? d.reglage : reglagesActuels().auto);

  const resoudre = (d, ref) => (ref ? git.resolveRef({ ...g(d), ref }).catch((e) => {
    if (e.code === 'NotFoundError') return null;
    throw e;
  }) : Promise.resolve(null));

  async function lireCommit(d, oid) {
    if (commits.has(oid)) return commits.get(oid);
    const { commit } = await git.readCommit({ ...g(d), oid });
    return garder(commits, oid, { tree: commit.tree, parents: commit.parent, auteur: commit.author, committer: commit.committer, message: commit.message });
  }

  async function lireArbre(d, oid) {
    if (arbres.has(oid)) return arbres.get(oid);
    return garder(arbres, oid, (await git.readTree({ ...g(d), oid })).tree);
  }

  // The entry at `chemin` ("a/b.nuee") in a tree (null: none)
  async function entreeA(d, arbre, chemin) {
    let entree = null;
    let oid = arbre;
    for (const s of chemin.split('/')) {
      if (!oid) return null;
      entree = (await lireArbre(d, oid)).find((x) => x.path === s) || null;
      if (!entree) return null;
      oid = entree.type === 'tree' ? entree.oid : null;
    }
    return entree;
  }
  async function blobA(d, arbre, chemin) {
    const e = arbre ? await entreeA(d, arbre, chemin) : null;
    return e && e.type === 'blob' ? e.oid : null;
  }
  // The project's path in a commit: the former one for this side's commits up to a change of path
  // (when it was shared where that path held another file), oldest change first; else its path.
  // Their commits are never before such a change: a former path in their tree is someone else's.
  async function cheminDe(d, oid) {
    for (const a of d.anciens) if ((await ancetres(d, a.jusqua)).has(oid)) return a.chemin;
    return d.cheminProjet;
  }
  // The project's blob in a commit (null: none, or not there)
  async function blobDe(d, oid) {
    if (!oid) return null;
    return blobA(d, (await lireCommit(d, oid)).tree, await cheminDe(d, oid));
  }

  // A tree written: `arbre` (null: empty) with the entry at `chemin` set to `blob` (null: removed),
  // every other entry as it was; null when nothing is left in it
  async function arbreAvec(d, arbre, chemin, blob) {
    const [premier, ...reste] = chemin.split('/');
    const entrees = arbre ? [...(await lireArbre(d, arbre))] : [];
    const i = entrees.findIndex((e) => e.path === premier);
    const avant = i >= 0 ? entrees.splice(i, 1)[0] : null;
    if (!reste.length) {
      if (blob) entrees.push({ mode: avant && avant.type === 'blob' && /^100(644|755)$/.test(avant.mode) ? avant.mode : '100644', path: premier, oid: blob, type: 'blob' });
    } else if (avant && avant.type !== 'tree') {
      if (blob) {
        throw refus(`Dans le dépôt, ${avant.path} est un fichier, là où doit être le dossier du projet : utilisez git directement.`,
          `In the repository, ${avant.path} is a file where the project’s folder must be: use git directly.`);
      }
      entrees.push(avant);
    } else {
      const sous = await arbreAvec(d, avant ? avant.oid : null, reste.join('/'), blob);
      if (sous) entrees.push({ mode: '040000', path: premier, oid: sous, type: 'tree' });
    }
    return entrees.length ? git.writeTree({ ...g(d), tree: entrees }) : null;
  }
  // `arbre` with the project's blob at its path: their tree, or this side's own (deCeCote), where
  // the project moves from the path it had in the last commit
  async function arbreProjet(d, arbre, blob, { deCeCote = false } = {}) {
    let a = arbre;
    if (deCeCote && d.tete) {
      const ici = await cheminDe(d, d.tete);
      if (ici !== d.cheminProjet && a && await entreeA(d, a, ici)) a = await arbreAvec(d, a, ici, null);
    }
    return arbreAvec(d, a, d.cheminProjet, blob);
  }

  // Every commit reachable from `oid` (a missing parent, in a shallow history, ends its line)
  async function ancetres(d, oid) {
    if (!oid) return new Set();
    if (ancetresMemo.has(oid)) return ancetresMemo.get(oid);
    const vus = new Set();
    const pile = [oid];
    while (pile.length) {
      const o = pile.pop();
      if (vus.has(o)) continue;
      let c;
      try { c = await lireCommit(d, o); } catch (e) { if (e.code === 'NotFoundError') continue; throw e; }
      vus.add(o);
      pile.push(...c.parents);
    }
    return garder(ancetresMemo, oid, vus, 40);
  }

  // The versions reachable from `tete`: the commits where the project is and differs from their
  // first parent's, those with no parent, and the choices between two versions; newest first
  async function versionsDe(d, tete) {
    if (!tete) return [];
    const k = `${d.gitdir}\n${d.cheminProjet}\n${JSON.stringify(d.anciens)}\n${tete}`;
    if (listes.has(k)) return listes.get(k);
    const res = [];
    const parentsDe = new Map();
    const vus = new Set([tete]);
    const pile = [tete];
    while (pile.length) {
      const oid = pile.pop();
      const c = await lireCommit(d, oid);
      let premier = null;
      const lus = [];
      for (const [i, p] of c.parents.entries()) {
        try { await lireCommit(d, p); } catch (e) { if (e.code !== 'NotFoundError') throw e; continue; }
        if (i === 0) premier = p;
        lus.push(p);
        if (!vus.has(p)) { vus.add(p); pile.push(p); }
      }
      parentsDe.set(oid, lus);
      const blob = await blobDe(d, oid);
      const m = marque(c.message);
      if (blob && (!c.parents.length || m.choix || !premier || (await blobDe(d, premier)) !== blob)) {
        res.push({ oid, description: m.description, auto: m.auto, auteur: { nom: c.auteur.name, email: c.auteur.email },
          date: new Date(c.auteur.timestamp * 1000).toISOString(), temps: c.auteur.timestamp });
      }
    }
    // (by date; between versions of the same second, a version before those it comes from: its
    // generation, one more than its parents' highest)
    const generation = new Map();
    for (const depart of parentsDe.keys()) {
      const reste = [[depart, false]];
      while (reste.length) {
        const [o, apres] = reste.pop();
        if (generation.has(o)) continue;
        const ps = parentsDe.get(o) || [];
        if (apres) { generation.set(o, 1 + Math.max(0, ...ps.map((p) => generation.get(p) || 0))); continue; }
        reste.push([o, true]);
        for (const p of ps) if (!generation.has(p)) reste.push([p, false]);
      }
    }
    res.sort((a, b) => b.temps - a.temps || generation.get(b.oid) - generation.get(a.oid));
    return garder(listes, k, res, 200);
  }

  // The commits already on a remote: those under the remote's branches, and what was sent to a
  // remote forgotten since (refs/nuee/envoye/)
  async function envoyes(d) {
    const res = new Set();
    for (const base of ['refs/remotes/origin', 'refs/nuee/envoye']) {
      let noms = [];
      try { noms = await git.listRefs({ ...g(d), filepath: base }); } catch { /* none */ }
      for (const n of noms) for (const o of await ancetres(d, await resoudre(d, `${base}/${n}`))) res.add(o);
    }
    return res;
  }

  // The project's names: each tag, its tag object (oid), its commit and the name it shows (the
  // first line of its message when the tag is that line made a ref, a name given in Nuée; else
  // the tag's own name)
  async function tags(d) {
    const res = new Map();
    for (const nom of await git.listTags(g(d))) {
      const oid = await resoudre(d, `refs/tags/${nom}`);
      if (!oid) continue;
      let commit = oid;
      let affiche = nom;
      try {
        const { tag } = await git.readTag({ ...g(d), oid });
        commit = tag.object;
        const ligne = tag.message.split('\n')[0].trim();
        if (ligne && nomRef(ligne) === nom.normalize('NFC')) affiche = ligne;
      } catch { /* a tag without an object of its own: the commit's */ }
      res.set(nom, { oid, commit, affiche });
    }
    return res;
  }

  // The versions as the interface gets them
  async function enVersions(d, vs) {
    const noms = new Map();
    for (const { commit, affiche } of (await tags(d)).values()) noms.set(commit, [...(noms.get(commit) || []), affiche].sort());
    const partis = await envoyes(d);
    return vs.map((v) => ({ oid: v.oid, date: v.date, auteur: v.auteur, description: v.description, auto: v.auto, noms: noms.get(v.oid) || [], envoyee: partis.has(v.oid) }));
  }

  // The commits the thinning made again: an oid the interface was given before still names its
  // version (eclaircir)
  const remplaces = new Map();
  const actuel = (oid) => { let o = oid; for (let i = 0; remplaces.has(o) && i < 100; i += 1) o = remplaces.get(o); return o; };

  // A version of this history, or of theirs not brought in yet (avecLeurs)
  async function versionConnue(d, ancien, { avecLeurs = false } = {}) {
    const oid = typeof ancien === 'string' ? actuel(ancien) : ancien;
    const chercherDans = async (tete) => (typeof oid === 'string' ? (await versionsDe(d, tete)).find((x) => x.oid === oid) : null);
    const v = (await chercherDans(d.tete)) || (avecLeurs && d.url ? await chercherDans(await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`)) : null);
    if (!v) throw refus('Cette version n’est pas dans l’historique du projet.', 'This version isn’t in the project’s history.');
    return v;
  }

  // The project file's blob, computed again (fraiche) or kept while its size and date stay the
  // same (for etat, asked often, of files that can be large)
  async function empreinte(chemin, fraiche = false) {
    const st = await fs.promises.stat(chemin);
    const m = empreintes.get(chemin);
    if (!fraiche && m && m.taille === st.size && m.date === st.mtimeMs && m.ino === st.ino) return m.oid;
    const { oid } = await git.hashBlob({ object: await fs.promises.readFile(chemin) });
    garder(empreintes, chemin, { taille: st.size, date: st.mtimeMs, ino: st.ino, oid }, 200);
    return oid;
  }

  const lireBlob = async (d, oid) => Buffer.from((await git.readBlob({ ...g(d), oid })).blob);
  const lireJson = (octets) => { try { return JSON.parse(Buffer.from(octets).toString('utf8')); } catch { return undefined; } };

  // The size of a history on disk, counted again after a write or ten minutes
  async function taille(d) {
    const m = tailles.get(d.gitdir);
    if (m && maintenant() - m.quand < 10 * MINUTE) return m.octets;
    let octets = 0;
    const compter = async (p) => {
      for (const e of await fs.promises.readdir(p, { withFileTypes: true })) {
        const f = path.join(p, e.name);
        if (e.isDirectory()) await compter(f); else octets += (await fs.promises.stat(f)).size;
      }
    };
    try { await compter(d.gitdir); } catch { /* counted so far */ }
    tailles.set(d.gitdir, { octets, quand: maintenant() });
    return octets;
  }

  // A project renamed outside Nuée: a history in the folder whose file is gone, and whose last
  // version is this file as it is, is this project's (its repository takes this name)
  async function retrouverRenomme(d) {
    let noms = [];
    try { noms = await fs.promises.readdir(d.versions); } catch { return; }
    const candidats = noms.filter((n) => /\.nuee\.git$/i.test(n) && n.toLowerCase() !== `${d.nom}.git`.toLowerCase());
    if (!candidats.length) return;
    const blob = await empreinte(d.chemin, true);
    for (const n of candidats) {
      if (await existe(path.join(d.dossier, n.slice(0, -4)))) continue;
      const autre = lieux({ dossier: d.dossier, nom: n.slice(0, -4), chemin: path.join(d.dossier, n.slice(0, -4)) });
      if (!(await existe(path.join(autre.gitdir, 'HEAD')))) continue;
      try { await lireDepot(autre); } catch { continue; }
      if (autre.blobTete !== blob) continue;
      // (in its queue and under its lock: Windows can't rename a folder while a file in it is open;
      // again for five seconds, a file just written being held a moment by an antivirus)
      await enFile(fileDe(autre.chemin), () => avecVerrou(autre, async () => {
        for (let i = 0; ; i += 1) {
          try { await fs.promises.rename(autre.gitdir, d.gitdir); return; } catch (e) { if (i >= 24 || !['EPERM', 'EBUSY', 'EACCES'].includes(e.code)) throw e; await pause(200); }
        }
      }));
      d.existe = true;
      return;
    }
  }

  // ------------------------------------------------------------------ writing

  // One command at a time per history in this process (a queue per repository), and one Nuée at a
  // time (the lock file: host, process, time; touched every 30 s while held, taken over after two
  // minutes without)
  const files = new Map();
  function enFile(k, f) {
    const r = (files.get(k) || Promise.resolve()).then(f);
    const suite = r.then(() => {}, () => {});
    files.set(k, suite);
    suite.then(() => { if (files.get(k) === suite) files.delete(k); });
    return r;
  }
  const fileDe = (chemin) => cle(path.join(path.dirname(path.resolve(chemin)), DOSSIER, `${path.basename(chemin)}.git`));

  async function avecVerrou(d, f) {
    const moi = JSON.stringify({ hote: os.hostname(), processus: process.pid, date: new Date().toISOString(), n: crypto.randomUUID() });
    for (let i = 0; ; i += 1) {
      try { await fs.promises.writeFile(d.verrou, moi, { flag: 'wx' }); break; } catch (e) {
        if (e.code !== 'EEXIST') throw e;
        let age = 0;
        try { age = Date.now() - (await fs.promises.stat(d.verrou)).mtimeMs; } catch { continue; }
        if (age > 2 * MINUTE) { await fs.promises.unlink(d.verrou).catch(() => {}); continue; }
        if (i >= 100) {
          let qui = '';
          try { qui = JSON.parse(await fs.promises.readFile(d.verrou, 'utf8')).hote || ''; } catch { /* unknown */ }
          throw refus(`L’historique du projet est en train d’être écrit par un autre Nuée${qui ? ` (${qui})` : ''} : réessayez dans un moment.`,
            `The project’s history is being written by another Nuée${qui ? ` (${qui})` : ''}: try again in a moment.`);
        }
        await pause(200);
      }
    }
    const battement = setInterval(() => { fs.promises.utimes(d.verrou, new Date(), new Date()).catch(() => {}); }, 30e3);
    try {
      return await f();
    } finally {
      clearInterval(battement);
      try { if ((await fs.promises.readFile(d.verrou, 'utf8')) === moi) await fs.promises.unlink(d.verrou); } catch { /* gone */ }
    }
  }

  // A command that writes a project's history: in its queue, its history read again under the
  // lock; made first when `creer` says so (a function of the history not there yet)
  function ecrire(chemin, f, { creer = false } = {}) {
    return enFile(fileDe(chemin), async () => {
      const d = await depot(chemin, { retrouver: true });
      if (!d.existe && (typeof creer === 'function' ? creer(d) : creer)) await creerDepot(d);
      if (!d.existe) return f(d);
      return avecVerrou(d, async () => { await lireDepot(d); return f(d); });
    });
  }

  // The project file replaced as the engine saves it (moteur/R/nouveau.R: ecrire_sans_risque):
  // written beside it, then renamed over it, again for five seconds while another program holds it
  async function remplacerFichier(chemin, contenu) {
    const temporaire = path.join(path.dirname(chemin), `.${path.basename(chemin)}.${process.pid}.tmp`);
    await fs.promises.writeFile(temporaire, contenu);
    try {
      for (let i = 0; ; i += 1) {
        try { await fs.promises.rename(temporaire, chemin); return; } catch (e) { if (i >= 24) throw e; await pause(200); }
      }
    } finally {
      // (unlink: Node's rm leaves a path that isn't ASCII in some versions, on Windows)
      await fs.promises.unlink(temporaire).catch(() => {});
    }
  }
  const ecrireProjet = async (d, blob) => remplacerFichier(d.chemin, await lireBlob(d, blob));

  // A commit on the branch: the project's `blob` in the branch's tree (or `arbre`), its parents
  async function commettre(d, { blob, description, marqueFin = null, arbre = null, parents = null }) {
    const tree = arbre || await arbreProjet(d, d.arbreTete, blob, { deCeCote: true });
    const auteur = auteurGit();
    const message = `${description.trim()}\n${marqueFin ? `\n${marqueFin}\n` : ''}`;
    const oid = await git.writeCommit({ ...g(d), commit: { message, tree, parent: parents || (d.tete ? [d.tete] : []), author: auteur, committer: auteur } });
    await git.writeRef({ ...g(d), ref: `refs/heads/${d.branche}`, value: oid, force: true });
    Object.assign(d, { tete: oid, arbreTete: tree, blobTete: blob });
    tailles.delete(d.gitdir);
    return oid;
  }

  // A version of the project file as it is, when it differs from the last one; an automatic one
  // says what changed. Its oid, or null.
  async function versionDuFichier(d, { description = null, auto = true } = {}) {
    const contenu = await fs.promises.readFile(d.chemin);
    const { oid } = await git.hashBlob({ object: contenu });
    if (oid === d.blobTete) return null;
    const avant = d.blobTete ? lireJson(await lireBlob(d, d.blobTete)) : null;
    const texte = description || decrireChangements(avant, lireJson(contenu), langue() === 'fr');
    const blob = await git.writeBlob({ ...g(d), blob: contenu });
    return commettre(d, { blob, description: texte, marqueFin: auto ? AUTO : null });
  }

  const versionDeTete = async (d) => (d.tete ? (await enVersions(d, (await versionsDe(d, d.tete)).slice(0, 1)))[0] || null : null);

  // ------------------------------------------------------------------ thinning

  // The automatic versions never sent anywhere, older than 30 days, thinned to one a day (the last
  // of each), older than a year to one a month; named versions, the others and anything sent are
  // kept. Once a day at most. The commits after the first dropped one are made again (the same
  // trees, messages and dates), the names follow, and the objects nothing reaches any more go.
  async function eclaircir(d) {
    const fait = await config(d, 'nuee.eclairci');
    if (fait && maintenant() - Date.parse(fait) < JOUR) return 0;
    await ecrireConfig(d, 'nuee.eclairci', new Date(maintenant()).toISOString());
    const partis = await envoyes(d);
    const noms = await tags(d);
    const nommes = new Set([...noms.values()].map((x) => x.commit));
    const ligne = [];
    for (let o = d.tete; o && !partis.has(o);) {
      let c;
      try { c = await lireCommit(d, o); } catch { break; }
      ligne.push({ oid: o, c });
      o = c.parents[0] || null;
    }
    const gardes = new Set();
    const groupes = new Set();
    for (const { oid, c } of ligne) {
      const quand = c.committer.timestamp * 1000;
      const age = maintenant() - quand;
      if (!marque(c.message).auto || nommes.has(oid) || age < 30 * JOUR) { gardes.add(oid); continue; }
      const j = new Date(quand);
      const groupe = age > 365 * JOUR ? `m${j.getFullYear()}-${j.getMonth()}` : `j${j.getFullYear()}-${j.getMonth()}-${j.getDate()}`;
      if (!groupes.has(groupe)) { groupes.add(groupe); gardes.add(oid); }
    }
    const retires = ligne.length - gardes.size;
    // (what an earlier pass, or one cut short, left: removed now that it is an hour old)
    if (!retires) { await nettoyerObjets(d); tailles.delete(d.gitdir); return 0; }
    const nouveaux = new Map();
    let parent = ligne[ligne.length - 1].c.parents[0] || null;
    for (let i = ligne.length - 1; i >= 0; i -= 1) {
      const { oid, c } = ligne[i];
      if (!gardes.has(oid)) continue;
      const parents = [parent, ...c.parents.slice(1)].filter(Boolean);
      const memes = parents.length === c.parents.length && parents.every((p, k) => p === c.parents[k]);
      const nouveau = memes ? oid : await git.writeCommit({ ...g(d), commit: { message: c.message, tree: c.tree, parent: parents, author: c.auteur, committer: c.committer } });
      nouveaux.set(oid, nouveau);
      if (nouveau !== oid) remplaces.set(oid, nouveau);
      parent = nouveau;
    }
    await git.writeRef({ ...g(d), ref: `refs/heads/${d.branche}`, value: parent, force: true });
    for (const [nom, x] of noms) {
      const nouveau = nouveaux.get(x.commit);
      if (!nouveau || nouveau === x.commit) continue;
      if (x.oid === x.commit) { await git.writeRef({ ...g(d), ref: `refs/tags/${nom}`, value: nouveau, force: true }); continue; }
      const { tag } = await git.readTag({ ...g(d), oid: x.oid });
      await git.annotatedTag({ ...g(d), ref: nom, object: nouveau, message: tag.message, tagger: tag.tagger, force: true });
    }
    await lireDepot(d);
    await nettoyerObjets(d);
    tailles.delete(d.gitdir);
    return retires;
  }

  // The loose objects nothing reaches any more (from any ref), an hour old at least, removed
  async function nettoyerObjets(d) {
    const atteints = new Set();
    const pile = [];
    let refs = [];
    try { refs = (await git.listRefs({ ...g(d), filepath: 'refs' })).map((r) => `refs/${r}`); } catch { /* none */ }
    for (const r of ['HEAD', ...refs]) { const o = await resoudre(d, r); if (o) pile.push([o, null]); }
    while (pile.length) {
      const [o, type] = pile.pop();
      if (atteints.has(o)) continue;
      atteints.add(o);
      if (type === 'blob') continue;
      let x;
      try { x = await git.readObject({ ...g(d), oid: o, format: 'parsed' }); } catch { continue; }
      if (x.type === 'commit') pile.push([x.object.tree, 'tree'], ...x.object.parent.map((p) => [p, 'commit']));
      else if (x.type === 'tree') for (const e of x.object) { if (e.type !== 'commit') pile.push([e.oid, e.type]); }
      else if (x.type === 'tag') pile.push([x.object.object, null]);
    }
    const objets = path.join(d.gitdir, 'objects');
    for (const deux of await fs.promises.readdir(objets).catch(() => [])) {
      if (!/^[0-9a-f]{2}$/.test(deux)) continue;
      for (const reste of await fs.promises.readdir(path.join(objets, deux)).catch(() => [])) {
        if (atteints.has(deux + reste)) continue;
        const f = path.join(objets, deux, reste);
        const st = await fs.promises.stat(f).catch(() => null);
        if (st && maintenant() - st.mtimeMs > HEURE) await fs.promises.unlink(f).catch(() => {});
      }
    }
  }

  // ------------------------------------------------------------------ the remote

  // The token for the remote's server, given only when it asks (401): a public repository is read
  // without it. GitHub and GitLab take any user name with a token; these are their own.
  function auth(d) {
    return {
      onAuth: async () => {
        const jeton = await jetons.lire(d.hote);
        d.jetonDonne = !!jeton;
        return jeton ? { username: d.hote === 'github.com' ? 'x-access-token' : 'oauth2', password: jeton } : undefined;
      },
    };
  }

  // An error of git or of the network as the user reads it (`messages`: what the server said
  // while refusing versions)
  function traduire(e, d, messages = []) {
    if (e instanceof Refus) return e;
    const hote = d && d.hote;
    const code = (e && e.code) || '';
    if (code === 'HttpError') {
      const s = e.data && e.data.statusCode;
      if (s === 401 && d && d.jetonDonne) {
        return refus(`${hote} a refusé le jeton : il est peut-être faux ou expiré. Donnez-en un nouveau.`,
          `${hote} refused the token: it may be wrong or expired. Give a new one.`);
      }
      if (s === 401) {
        return refus(`${hote} demande un jeton : donnez-en un qui permet de lire et d’écrire dans ce dépôt.`,
          `${hote} asks for a token: give one that may read and write to this repository.`);
      }
      if (s === 403) {
        return refus(`Le jeton ne permet pas d’écrire dans ce dépôt sur ${hote} : donnez-en un qui le permet.`,
          `The token may not write to this repository on ${hote}: give one that may.`);
      }
      if (s === 404) return refus(`Aucun dépôt à cette adresse sur ${hote}, ou le jeton ne permet pas de le voir.`, `No repository at this address on ${hote}, or the token can’t see it.`);
      return refus(`${hote} a répondu : ${s} ${(e.data && e.data.statusMessage) || ''}`.trim(), `${hote} answered: ${s} ${(e.data && e.data.statusMessage) || ''}`.trim());
    }
    if (code === 'PushRejectedError') {
      return refus('Le dépôt a des versions que ce projet n’a pas encore : recevez-les d’abord.', 'The repository has versions this project doesn’t have yet: receive them first.');
    }
    if (code === 'GitPushError') {
      const dit = messages.join('').split(/\r?\n/).map((l) => l.trim()).filter((l) => /error|refus|reject|denied|declined|GH\d{3}/i.test(l));
      const details = (dit.length ? dit.slice(0, 3).join(' ') : e.message).trim();
      return refus(`${hote} a refusé les versions : ${details}`, `${hote} refused the versions: ${details}`);
    }
    if (['SmartHttpError', 'ParseError', 'UnknownTransportError', 'EmptyServerResponseError'].includes(code)) {
      return refus(`Cette adresse n’est pas celle d’un dépôt : ${hote} ne répond pas comme un serveur git.`,
        `This address isn’t a repository’s: ${hote} doesn’t answer as a git server does.`);
    }
    // the network: Chromium's net::ERR_…, or Node's (its fetch's cause)
    const reseau = (/net::ERR_[A-Z_0-9]+/.exec(String(e && e.message)) || [])[0] || (e && e.cause && e.cause.code)
      || (e instanceof TypeError && /fetch failed/.test(e.message) ? 'fetch failed' : null);
    if (reseau && hote) {
      return refus(`Impossible de joindre ${hote} (${reseau}). Vérifiez la connexion, ou le proxy.`, `Couldn’t reach ${hote} (${reseau}). Check the connection, or the proxy.`);
    }
    return new Refus(String((e && e.message) || e));
  }

  // (a known token blanked from a message, in any form a request could have carried it)
  async function sansJeton(message) {
    let m = String(message);
    for (const j of await jetons.connus()) {
      const formes = [j, encodeURIComponent(j), Buffer.from(j).toString('base64'),
        ...['x-access-token', 'oauth2'].map((u) => Buffer.from(`${u}:${j}`).toString('base64'))];
      for (const f of formes) m = m.split(f).join('•••');
    }
    return m;
  }

  const reseau = async (d, f) => { try { return await f(); } catch (e) { throw traduire(e, d); } };

  function exigerPartage(d) {
    if (!d.url) throw refus('Ce projet n’est pas partagé avec GitHub ou GitLab.', 'This project isn’t shared with GitHub or GitLab.');
    if (!d.hote) {
      throw refus(`Nuée envoie et reçoit par HTTPS seulement, et le dépôt de ce projet est à ${d.url}.`,
        `Nuée sends and receives over HTTPS only, and this project’s repository is at ${d.url}.`);
    }
    if (d.reglage === false) throw refus('Les versions de ce projet sont désactivées : activez-les d’abord.', 'Versions are off for this project: turn them on first.');
  }

  // Their branch fetched (refs/remotes/origin/<branch>, the commits it needs): its last commit,
  // null when the remote hasn't this branch (an empty repository)
  async function chercher(d) {
    try {
      const r = await git.fetch({ ...g(d), http, remote: 'origin', ref: d.branche, remoteRef: `refs/heads/${d.brancheDistante}`, singleBranch: true, tags: false, ...auth(d) });
      if (!r.fetchHead) return null;
    } catch (e) {
      if (e.code === 'NotFoundError') return null;
      throw traduire(e, d);
    }
    tailles.delete(d.gitdir);
    return resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
  }

  // What to do with the project's path when their branch comes in, against the version both sides
  // last had (none when the two histories have nothing in common): 'mien' (only this side changed
  // it, or both made it the same, or they removed it, which Nuée never does), 'leur' (only they
  // changed it), 'conflit' (both did)
  async function decider(d, leur) {
    const arbreLeur = (await lireCommit(d, leur)).tree;
    const blobLeur = await blobDe(d, leur);
    let bases = [];
    if (d.tete) try { bases = await git.findMergeBase({ ...g(d), oids: [d.tete, leur] }); } catch { bases = []; }
    const blobBase = bases.length ? await blobDe(d, bases[0]) : null;
    const res = { arbreLeur, blobLeur, blobBase, quoi: 'mien' };
    if (!blobLeur || blobLeur === d.blobTete || blobLeur === blobBase) return res;
    res.quoi = !d.blobTete || d.blobTete === blobBase ? 'leur' : 'conflit';
    return res;
  }

  // A conflict as the interface gets it: the last version of each side, and the three versions of
  // the project (the one both last had, mine, theirs) as temporary files for the engine's merge
  // (fusionner); base null, and sansBase, when the two histories have no version of it in common.
  // Remembered, so that a merge made from them is refused once either side has moved.
  const conflitsDonnes = new Map();
  async function detailsConflit(d, leur) {
    const dec = await decider(d, leur);
    const dossier = await fs.promises.mkdtemp(path.join(temporaire(), 'fusion-'));
    const copie = async (nom, blob) => {
      if (!blob) return null;
      const f = path.join(dossier, `${nom}.nuee`);
      await fs.promises.writeFile(f, await lireBlob(d, blob));
      return f;
    };
    conflitsDonnes.set(cle(d.chemin), `${d.tete}:${leur}`);
    return {
      mien: (await enVersions(d, (await versionsDe(d, d.tete)).slice(0, 1)))[0] || null,
      leur: (await enVersions(d, (await versionsDe(d, leur)).slice(0, 1)))[0] || null,
      fichiers: { base: await copie('base', dec.blobBase), mien: await copie('mien', d.blobTete), leur: await copie('leur', dec.blobLeur), sansBase: !dec.blobBase },
    };
  }

  const enAttenteIci = (d) => { const o = ouverts.get(cle(d.chemin)); return !!(o && o.modifie); };

  // Their branch brought into this history, path by path: every path other than the project's is
  // theirs; the project's, what decider says. A fast-forward when this history is in theirs, else
  // a merge commit whose two parents are both histories. Nothing changes on a conflict, nor when
  // their project would be written over an open project's changes not saved ('attente'), nor, for
  // the background (ouvertSansAccord false), over a project open in a window at all ('pret'): the
  // window first makes sure it holds nothing unsaved, in any of its windows, then gets it (recevoir).
  // { resultat: 'a_jour' | 'recu' | 'conflit' | 'attente' | 'pret', version (theirs, when the file
  //   took it or would), aEnvoyer (a merge made here) }
  async function integrer(d, leur, { ouvertSansAccord = true } = {}) {
    if (!leur || leur === d.tete || (d.tete && (await ancetres(d, d.tete)).has(leur))) return { resultat: 'a_jour' };
    const dec = await decider(d, leur);
    if (dec.quoi === 'conflit') return { resultat: 'conflit' };
    const blob = dec.quoi === 'leur' ? dec.blobLeur : d.blobTete;
    const change = blob !== d.blobTete;
    if (change && enAttenteIci(d)) return { resultat: 'attente' };
    if (change && !ouvertSansAccord && ouverts.has(cle(d.chemin))) {
      return { resultat: 'pret', version: (await enVersions(d, (await versionsDe(d, leur)).slice(0, 1)))[0] };
    }
    // (the file first: if it can't be written, the history stays as it was)
    if (change) await ecrireProjet(d, blob);
    const avance = !d.tete || (await ancetres(d, leur)).has(d.tete);
    let aEnvoyer = false;
    if (avance && dec.blobLeur) {
      await git.writeRef({ ...g(d), ref: `refs/heads/${d.branche}`, value: leur, force: true });
      Object.assign(d, { tete: leur, arbreTete: dec.arbreLeur, blobTete: dec.blobLeur });
    } else {
      const c = await lireCommit(d, leur);
      const tree = await arbreProjet(d, dec.arbreLeur, blob);
      await commettre(d, { blob, arbre: tree, parents: d.tete ? [d.tete, leur] : [leur],
        description: change ? t(`Version de ${c.auteur.name} reçue`, `Received the version of ${c.auteur.name}`) : t('Fichiers du dépôt reçus', 'Files of the repository received') });
      aEnvoyer = true;
    }
    tailles.delete(d.gitdir);
    return { resultat: change ? 'recu' : 'a_jour', version: change ? (await enVersions(d, (await versionsDe(d, leur)).slice(0, 1)))[0] : undefined, aEnvoyer };
  }

  // The remote's names: name → { oid: its tag, commit: what it names }
  async function nomsDistants(d) {
    const refs = await reseau(d, () => git.listServerRefs({ http, url: d.url, prefix: 'refs/tags/', peelTags: true, protocolVersion: 1, ...auth(d) }));
    return new Map(refs.filter((r) => !r.ref.endsWith('^{}')).map((r) => [r.ref.slice('refs/tags/'.length), { oid: r.oid, commit: r.peeled || r.oid }]));
  }

  // The names both sides had at the last exchange, each with its tag, kept with the remote's
  // address in the repository (nuee-noms.json): one gone since from one side, and the same tag on
  // the other, was removed there
  const fichierBase = (d) => path.join(d.gitdir, 'nuee-noms.json');
  async function baseNoms(d) {
    try {
      const b = JSON.parse(await fs.promises.readFile(fichierBase(d), 'utf8'));
      if (b.url === d.url && b.noms && typeof b.noms === 'object') return new Map(Object.entries(b.noms));
    } catch { /* no exchange yet */ }
    return new Map();
  }

  // The names put in step with the remote's, from what each side did since the last exchange: a
  // name added on one side is added on the other, one removed is removed on the other (unless the
  // other gave it since to another version: that one stays). A remote's name comes here only for
  // a version of this history (a release tag of other files stays there), its tag fetched alone.
  // Receiving changes this side only; sending, the remote's too. A name on both sides with
  // different tags is left as it is.
  async function accorderNoms(d, { envoi }) {
    const distants = await nomsDistants(d);
    const base = await baseNoms(d);
    const ici = await tags(d);
    const notre = await ancetres(d, d.tete);
    const garde = new Map();
    const pousser = (nom, supprimer) => reseau(d, () => git.push({ ...g(d), http, remote: 'origin', ref: `refs/tags/${nom}`,
      remoteRef: `refs/tags/${nom}`, delete: supprimer, ...auth(d) }));
    for (const nom of new Set([...ici.keys(), ...distants.keys(), ...base.keys()])) {
      const local = ici.has(nom) ? ici.get(nom).oid : null;
      const la = distants.has(nom) ? distants.get(nom).oid : null;
      const connu = base.get(nom) || null;
      if (local && la) garde.set(nom, la);
      else if (local) {
        if (local === connu) await git.deleteTag({ ...g(d), ref: nom });  // removed on the remote
        else if (envoi) { await pousser(nom, false); garde.set(nom, local); }
      } else if (la) {
        if (la === connu && envoi) {
          // removed here: removed there (a deletion pushes a local ref, put back for the time of it)
          await git.writeRef({ ...g(d), ref: `refs/tags/${nom}`, value: la, force: true });
          try { await pousser(nom, true); } finally { await git.deleteRef({ ...g(d), ref: `refs/tags/${nom}` }); }
        } else if (la === connu) {
          garde.set(nom, la);  // (removed here, not sent yet)
        } else if (notre.has(distants.get(nom).commit)) {
          await reseau(d, () => git.fetch({ ...g(d), http, remote: 'origin', ref: d.branche, remoteRef: `refs/tags/${nom}`, singleBranch: true, tags: false, ...auth(d) }));
          await git.writeRef({ ...g(d), ref: `refs/tags/${nom}`, value: la, force: true });
          garde.set(nom, la);
        }
      }
    }
    await fs.promises.writeFile(fichierBase(d), JSON.stringify({ url: d.url, noms: Object.fromEntries([...garde].sort()) }));
  }

  // An exchange with the remote (under the history's lock): the file made a version first if it
  // changed, their branch fetched and brought in (integrer; ouvertSansAccord: as there); when
  // sending, and nothing waits for the user, this history pushed (again once if the remote moved
  // meanwhile); then the names.
  // { resultat: 'a_jour' | 'recu' | 'conflit' | 'attente' | 'pret' | 'envoye', version?, aEnvoyer? }
  async function echanger(d, { envoi, ouvertSansAccord = true }) {
    exigerPartage(d);
    if (actifPour(d)) await versionDuFichier(d);
    for (let essai = 0; ; essai += 1) {
      const leur = await chercher(d);
      const r = await integrer(d, leur, { ouvertSansAccord });
      if (['conflit', 'attente', 'pret'].includes(r.resultat) || !envoi) {
        await accorderNoms(d, { envoi: false });
        await ecrireConfig(d, 'nuee.echange', new Date(maintenant()).toISOString());
        return r;
      }
      const surLeServeur = await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
      if (d.tete && d.tete !== surLeServeur) {
        const messages = [];
        try {
          await git.push({ ...g(d), http, remote: 'origin', ref: d.branche, remoteRef: `refs/heads/${d.brancheDistante}`,
            onMessage: (m) => { messages.push(m); }, ...auth(d) });
        } catch (e) {
          if (e.code === 'PushRejectedError' && essai === 0) continue;
          throw traduire(e, d, messages);
        }
      }
      await accorderNoms(d, { envoi: true });
      await ecrireConfig(d, 'nuee.echange', new Date(maintenant()).toISOString());
      return { ...r, resultat: r.resultat === 'recu' ? 'recu' : 'envoye' };
    }
  }

  // ------------------------------------------------------------------ the background

  // Exchanges started by Nuée itself (after a version, when a project opens or Nuée comes back to
  // the front): one at a time per project and kind, told to the interface; a network error never
  // interrupts, it is kept for etat and tried again at the next occasion
  const enCours = new Set();
  const taches = new Set();
  function arrierePlan(chemin, envoi) {
    const k = `${cle(chemin)}\n${envoi}`;
    if (enCours.has(k)) return;
    enCours.add(k);
    const tache = (async () => {
      try {
        const r = await ecrire(chemin, async (d) => {
          if (!d.existe || !d.url || !d.hote || d.reglage === false) return null;
          if (envoi) evenement({ type: 'envoi', chemin: d.chemin });
          try {
            // (a project open in a window: its file written once the window agrees, 'pret')
            const res = await echanger(d, { envoi, ouvertSansAccord: false });
            erreurs.delete(cle(d.chemin));
            // (a conflict told once, with its files, until either side moves)
            if (res.resultat === 'conflit') {
              const leur = await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
              if (conflitsDonnes.get(cle(d.chemin)) === `${d.tete}:${leur}`) return null;
              Object.assign(res, await detailsConflit(d, leur));
            }
            return { ...res, chemin: d.chemin };
          } catch (e) {
            const message = await sansJeton(traduire(e, d).message);
            erreurs.set(cle(d.chemin), message);
            evenement({ type: 'erreur', chemin: d.chemin, message });
            return null;
          }
        });
        if (!r) return;
        if (r.resultat === 'recu') evenement({ type: 'recu', chemin: r.chemin, version: r.version });
        if (r.resultat === 'pret') evenement({ type: 'pret', chemin: r.chemin, version: r.version });
        if (r.resultat === 'conflit') evenement({ type: 'conflit', chemin: r.chemin, mien: r.mien, leur: r.leur, fichiers: r.fichiers });
        if (r.resultat === 'attente') evenement({ type: 'attente', chemin: r.chemin });
        if (r.resultat === 'envoye') evenement({ type: 'envoye', chemin: r.chemin });
        if (!envoi && r.aEnvoyer) arrierePlan(r.chemin, true);
      } catch { /* the project gone, or its history locked: next time */ } finally {
        enCours.delete(k);
      }
    })();
    taches.add(tache);
    tache.finally(() => taches.delete(tache));
  }

  // After a version: sent in the background when shared; thinned once a day
  function apres(chemin, { envoi = true } = {}) {
    if (envoi) arrierePlan(chemin, true);
    const k = `${cle(chemin)}\neclaircir`;
    if (enCours.has(k)) return;
    enCours.add(k);
    // (thinned: the interface's list of versions is told to be read again)
    const tache = ecrire(chemin, async (d) => { if (d.existe && d.tete && await eclaircir(d)) evenement({ type: 'eclairci', chemin: d.chemin }); })
      .catch(() => 0).finally(() => enCours.delete(k));
    taches.add(tache);
    tache.finally(() => taches.delete(tache));
  }

  // ------------------------------------------------------------------ the functions

  async function etat(chemin) {
    const d = await depot(chemin);
    const res = { actif: actifPour(d), desactive: d.existe && d.reglage === false, dossier: d.versions, taille: 0, versions: 0, derniere: null, partage: null, conflit: null };
    if (!d.existe) return res;
    res.taille = await taille(d);
    const vs = await versionsDe(d, d.tete);
    res.versions = vs.length;
    if (vs.length) res.derniere = (await enVersions(d, vs.slice(0, 1)))[0];
    if (!d.url) return res;
    const leur = await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
    const nous = await ancetres(d, d.tete);
    const eux = await ancetres(d, leur);
    res.partage = {
      url: d.url, hote: d.hote, chemin: d.cheminProjet,
      jeton: !!d.hote && await jetons.present(d.hote), jetonSession: !!d.hote && await jetons.seulementSession(),
      aEnvoyer: [...nous].filter((o) => !eux.has(o)).length, aRecevoir: [...eux].filter((o) => !nous.has(o)).length,
      dernierEchange: await config(d, 'nuee.echange'), erreur: erreurs.get(cle(d.chemin)) || null,
    };
    if (leur && d.tete && !nous.has(leur) && !eux.has(d.tete) && (await decider(d, leur)).quoi === 'conflit') {
      const [mien, leurs] = await Promise.all([versionsDe(d, d.tete), versionsDe(d, leur)]);
      res.conflit = { mien: (await enVersions(d, mien.slice(0, 1)))[0] || null, leur: (await enVersions(d, leurs.slice(0, 1)))[0] || null };
    }
    return res;
  }

  // The interface says a project is open in a window, and whether it has changes not saved: its
  // history found again if it was renamed, then, the first time, what the remote has, in the
  // background
  async function ouvert(chemin, { modifie = false } = {}) {
    const k = cle(chemin);
    const deja = ouverts.get(k);
    ouverts.set(k, { chemin: path.resolve(chemin), modifie: !!modifie, recu: deja ? deja.recu : maintenant() });
    if (!deja) {
      await ecrire(chemin, async () => null);
      arrierePlan(chemin, false);
    }
    return {};
  }

  async function apresEnregistrement(chemin, { ancien = null } = {}) {
    if (ancien && cle(ancien) !== cle(chemin)) {
      const avant = ouverts.get(cle(ancien));
      ouverts.delete(cle(ancien));
      if (avant) ouverts.set(cle(chemin), { ...avant, chemin: path.resolve(chemin) });
      await copierHistoire(ancien, chemin);
    }
    const o = ouverts.get(cle(chemin));
    if (o) o.modifie = false;
    const v = await ecrire(chemin, async (d) => {
      if (!actifPour(d)) return null;
      const derniere = d.tete ? (await lireCommit(d, d.tete)).committer.timestamp * 1000 : null;
      if (derniere !== null && maintenant() - derniere <= HEURE) return null;
      return (await versionDuFichier(d)) ? versionDeTete(d) : null;
    }, { creer: () => reglagesActuels().auto });
    apres(chemin, { envoi: true });
    return { version: v };
  }

  async function aLaFermeture(chemin) {
    ouverts.delete(cle(chemin));
    const v = await ecrire(chemin, async (d) => (actifPour(d) && (await versionDuFichier(d)) ? versionDeTete(d) : null), { creer: () => reglagesActuels().auto });
    apres(chemin, { envoi: !!v });
    return { version: v };
  }

  async function marquer(chemin, description) {
    const texte = String(description == null ? '' : description).trim();
    const r = await ecrire(chemin, async (d) => {
      if (!actifPour(d)) return { oid: null };
      const oid = await versionDuFichier(d, { description: texte || null, auto: !texte });
      return { oid: oid || d.tete };
    }, { creer: () => reglagesActuels().auto });
    apres(chemin);
    return r;
  }

  async function liste(chemin) {
    const d = await depot(chemin);
    return d.existe ? enVersions(d, await versionsDe(d, d.tete)) : [];
  }

  async function nommer(chemin, oid, nom) {
    const r = await ecrire(chemin, async (d) => {
      if (!d.existe) throw refus('Ce projet n’a pas encore de versions.', 'This project has no versions yet.');
      const v = await versionConnue(d, oid);
      const texte = String(nom == null ? '' : nom).normalize('NFC').trim().replace(/\s+/g, ' ');
      const ref = nomRef(texte);
      if (!ref) throw refus('Donnez un nom à la version (des lettres ou des chiffres).', 'Give the version a name (letters or digits).');
      // (the same name whatever its case: Windows and macOS keep tags as files)
      for (const [k, x] of await tags(d)) {
        if (k.toLowerCase() === ref.toLowerCase() || x.affiche.toLowerCase() === texte.toLowerCase()) {
          throw refus(`Le nom « ${texte} » est déjà donné à une version.`, `The name “${texte}” is already given to a version.`);
        }
      }
      const a = auteurGit();
      await git.annotatedTag({ ...g(d), ref, object: v.oid, message: texte, tagger: a });
      return { nom: texte };
    });
    apres(chemin);
    return r;
  }

  async function retirerNom(chemin, nom) {
    await ecrire(chemin, async (d) => {
      const texte = String(nom == null ? '' : nom).normalize('NFC').trim();
      const trouve = d.existe ? [...(await tags(d))].find(([k, x]) => x.affiche === texte || k === texte) : null;
      if (!trouve) throw refus(`Aucune version ne s’appelle « ${texte} ».`, `No version is named “${texte}”.`);
      await git.deleteTag({ ...g(d), ref: trouve[0] });
    });
    apres(chemin);
    return {};
  }

  async function lire(chemin, oid) {
    const d = await depot(chemin);
    if (!d.existe) throw refus('Ce projet n’a pas encore de versions.', 'This project has no versions yet.');
    const v = await versionConnue(d, oid, { avecLeurs: true });
    const contenu = await lireBlob(d, await blobDe(d, v.oid));
    const dossier = await fs.promises.mkdtemp(path.join(temporaire(), 'version-'));
    const fichier = path.join(dossier, d.nom);
    await fs.promises.writeFile(fichier, contenu);
    return { fichier };
  }

  async function restaurer(chemin, oid) {
    const r = await ecrire(chemin, async (d) => {
      if (!d.existe) throw refus('Ce projet n’a pas encore de versions.', 'This project has no versions yet.');
      const v = await versionConnue(d, oid, { avecLeurs: true });
      // (what the file has that no version has, kept first)
      await versionDuFichier(d);
      const blob = await blobDe(d, v.oid);
      if (blob === d.blobTete) throw refus('Cette version est le projet tel qu’il est.', 'This version is the project as it is now.');
      await ecrireProjet(d, blob);
      const date = new Date(v.temps * 1000).toLocaleString(t('fr-FR', 'en-GB'), { dateStyle: 'long', timeStyle: 'short' });
      return { oid: await commettre(d, { blob, description: t(`Version du ${date} restaurée`, `Restored the version of ${date}`) }) };
    });
    apres(chemin);
    return r;
  }

  async function activer(chemin, oui) {
    await ecrire(chemin, async (d) => {
      await ecrireConfig(d, 'nuee.actif', oui ? 'true' : 'false');
      d.reglage = !!oui;
      if (oui && !d.tete) await versionDuFichier(d);
    }, { creer: true });
    if (oui) apres(chemin);
    return { actif: !!oui };
  }

  function reglages() {
    return reglagesActuels();
  }

  function regler({ auto, auteur } = {}) {
    const r = { ...((stockes && stockes.lire()) || {}) };
    if (auto !== undefined) r.auto = !!auto;
    if (auteur !== undefined) {
      const nom = String((auteur && auteur.nom) || '').trim().replace(/\s+/g, ' ');
      const email = String((auteur && auteur.email) || '').trim();
      if (!nom || nom.length > 200 || /[<>]/.test(nom) || email.length > 200 || !/^[^\s@<>]+@[^\s@<>]+$/.test(email)) {
        throw refus('Donnez un nom, et une adresse e-mail comme nom@exemple.org.', 'Give a name, and an email address such as name@example.org.');
      }
      r.auteur = { nom, email };
    }
    stockes.ecrire(r);
    return reglagesActuels();
  }

  // Shared with a repository: its address (normalised), its default branch, and the project's path
  // there: where it already is when both histories have versions in common; else <name>.nuee at
  // the root, or <name>/<name>.nuee (then <name> (2)/…) when that path holds another file
  async function partager(chemin, url) {
    const a = adresseDistante(url);
    if (a.refus === 'identifiants') {
      throw refus('Donnez l’adresse sans nom d’utilisateur ni jeton : le jeton se donne à part.', 'Give the address without a user name or token: the token is given apart.');
    }
    if (a.refus) {
      throw refus('Donnez l’adresse du dépôt, qui commence par https:// (sur GitHub ou GitLab, son bouton Code ou Cloner).',
        'Give the repository’s address, starting with https:// (on GitHub or GitLab, its Code or Clone button).');
    }
    const r = await ecrire(chemin, async (d) => {
      if (d.reglage === false) throw refus('Les versions de ce projet sont désactivées : activez-les d’abord.', 'Versions are off for this project: turn them on first.');
      await versionDuFichier(d);
      const avant = d.url;
      const remettre = async () => {
        if (avant) await ecrireConfig(d, 'remote.origin.url', avant); else await git.deleteRemote({ ...g(d), remote: 'origin' }).catch(() => {});
      };
      if (avant !== a.url) {
        if (avant) await ecrireConfig(d, 'remote.origin.url', a.url); else await git.addRemote({ ...g(d), remote: 'origin', url: a.url });
        for (const n of await git.listRefs({ ...g(d), filepath: 'refs/remotes/origin' }).catch(() => [])) await git.deleteRef({ ...g(d), ref: `refs/remotes/origin/${n}` });
      }
      Object.assign(d, { url: a.url, hote: a.hote });
      let leur = null;
      let branche = null;
      try {
        const f = await git.fetch({ ...g(d), http, remote: 'origin', ref: d.branche, remoteRef: 'HEAD', singleBranch: true, tags: false, ...auth(d) });
        if (f.fetchHead) { leur = f.fetchHead; branche = String(f.defaultBranch || '').replace(/^refs\/heads\//, '') || d.branche; }
      } catch (e) {
        if (e.code !== 'NotFoundError') { await remettre(); throw traduire(e, d); }
      }
      d.brancheDistante = branche || d.branche;
      await ecrireConfig(d, `branch.${d.branche}.remote`, 'origin');
      await ecrireConfig(d, `branch.${d.branche}.merge`, `refs/heads/${d.brancheDistante}`);
      if (leur) await git.writeRef({ ...g(d), ref: `refs/remotes/origin/${d.brancheDistante}`, value: leur, force: true });
      let cible = d.nom;
      if (leur) {
        const nous = await ancetres(d, d.tete);
        const eux = await ancetres(d, leur);
        if ([...nous].some((o) => eux.has(o))) cible = d.cheminProjet;
        else {
          const arbre = (await lireCommit(d, leur)).tree;
          const base = d.nom.replace(/\.nuee$/i, '');
          const essais = [d.nom, ...Array.from({ length: 9 }, (_, i) => `${base}${i ? ` (${i + 1})` : ''}/${d.nom}`)];
          cible = null;
          for (const c of essais) if (!(await entreeA(d, arbre, c))) { cible = c; break; }
          if (!cible) throw refus('Le dépôt a déjà des fichiers à chacune des places de ce projet : choisissez un autre dépôt.', 'The repository already has files at each of this project’s places: choose another repository.');
        }
      }
      if (cible !== d.cheminProjet) {
        if (d.tete) {
          await git.setConfig({ ...g(d), path: 'nuee.ancien', value: `${d.tete} ${await cheminDe(d, d.tete)}`, append: true });
          d.anciens.push({ jusqua: d.tete, chemin: await cheminDe(d, d.tete) });
        }
        await ecrireConfig(d, 'nuee.chemin', cible);
        d.cheminProjet = cible;
      }
      erreurs.delete(cle(d.chemin));
      return { url: d.url, hote: d.hote, chemin: d.cheminProjet };
    }, { creer: true });
    arrierePlan(chemin, true);
    return r;
  }

  async function jeton(hote, valeur) {
    const h = String(hote == null ? '' : hote).trim().toLowerCase();
    if (!/^[a-z0-9]([a-z0-9.-]*[a-z0-9])?(:\d{1,5})?$/.test(h)) throw refus('Donnez le nom du serveur, comme github.com.', 'Give the server’s name, such as github.com.');
    const j = String(valeur == null ? '' : valeur).trim();
    if (/\s/.test(j) || j.length > 1000) throw refus('Un jeton est un seul mot, tel que GitHub ou GitLab l’a donné.', 'A token is one word, as GitHub or GitLab gave it.');
    await jetons.ecrire(h, j);
    return { jeton: !!j, jetonSession: await jetons.seulementSession() };
  }

  async function verifierPartage(chemin) {
    const d = await depot(chemin);
    try {
      if (!d.existe) exigerPartage({ url: null });
      exigerPartage(d);
      // (the push side, which needs the token's write access)
      await reseau(d, () => git.getRemoteInfo2({ http, url: d.url, forPush: true, protocolVersion: 1, ...auth(d) }));
      return { ok: true, message: t(`Le dépôt sur ${d.hote} répond, et le jeton permet d’y envoyer des versions.`, `The repository on ${d.hote} answers, and the token may send versions to it.`) };
    } catch (e) {
      return { ok: false, message: await sansJeton(traduire(e, d).message) };
    }
  }

  async function envoyer(chemin) {
    const r = await ecrire(chemin, async (d) => {
      if (!d.existe) exigerPartage({ url: null });
      const res = await echanger(d, { envoi: true });
      erreurs.delete(cle(d.chemin));
      return res;
    });
    return { resultat: r.resultat, ...(r.version ? { version: r.version } : {}) };
  }

  async function recevoir(chemin) {
    const r = await ecrire(chemin, async (d) => {
      if (!d.existe) exigerPartage({ url: null });
      const res = await echanger(d, { envoi: false });
      erreurs.delete(cle(d.chemin));
      if (res.resultat === 'conflit') Object.assign(res, await detailsConflit(d, await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`)));
      return res;
    });
    if (r.aEnvoyer) arrierePlan(chemin, true);
    const { aEnvoyer, ...res } = r;
    return res;
  }

  // A conflict resolved: my version, theirs, or a merge of both made by the engine (`fichier`, from
  // the files detailsConflit gave); a merge commit whose two parents are both histories, then sent
  async function choisir(chemin, cote, fichier = null, description = null) {
    let fusion = null;
    if (cote === 'fusion') {
      if (typeof fichier !== 'string' || !path.isAbsolute(fichier)) throw refus('Le projet fusionné n’est pas là.', 'The merged project isn’t there.');
      try { fusion = await fs.promises.readFile(fichier); } catch { throw refus('Le projet fusionné n’est pas là.', 'The merged project isn’t there.'); }
      const j = lireJson(fusion);
      if (!j || typeof j !== 'object' || Array.isArray(j)) throw refus('Le projet fusionné ne se lit pas.', 'The merged project can’t be read.');
    }
    const r = await ecrire(chemin, async (d) => {
      if (!d.existe) exigerPartage({ url: null });
      exigerPartage(d);
      const nouvelle = await versionDuFichier(d);
      const leur = await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
      const diverge = leur && d.tete && !(await ancetres(d, d.tete)).has(leur) && !(await ancetres(d, leur)).has(d.tete);
      const dec = diverge ? await decider(d, leur) : null;
      if (!['mien', 'leur', 'fusion'].includes(cote) || !dec || dec.quoi !== 'conflit') {
        throw refus('Il n’y a rien à choisir : recevez d’abord les versions.', 'There is nothing to choose: receive the versions first.');
      }
      // (a merge made from versions that are no longer the last ones would lose what came since)
      if (fusion && (nouvelle || conflitsDonnes.get(cle(d.chemin)) !== `${d.tete}:${leur}`)) {
        throw refus('Le projet a changé depuis que la fusion a été préparée : recevez les versions et fusionnez de nouveau.',
          'The project changed since the merge was prepared: receive the versions and merge again.');
      }
      if (cote !== 'mien' && enAttenteIci(d)) {
        throw refus('Le projet a des changements qui ne sont pas enregistrés : enregistrez-les d’abord, ils seraient perdus.',
          'The project has changes not saved: save them first, they would be lost.');
      }
      const blob = cote === 'mien' ? d.blobTete : cote === 'leur' ? dec.blobLeur : await git.writeBlob({ ...g(d), blob: fusion });
      if (cote !== 'mien') await ecrireProjet(d, blob);
      const tree = await arbreProjet(d, dec.arbreLeur, blob);
      const texte = String(description == null ? '' : description).trim() || (cote === 'mien'
        ? t('Ma version gardée plutôt que celle reçue', 'Kept my version rather than the one received')
        : cote === 'leur' ? t('Version reçue prise plutôt que la mienne', 'Took the version received rather than mine')
          : t('Les deux versions réunies', 'Both versions merged'));
      conflitsDonnes.delete(cle(d.chemin));
      return { oid: await commettre(d, { blob, description: texte, marqueFin: CHOIX, arbre: tree, parents: [d.tete, leur] }) };
    });
    arrierePlan(chemin, true);
    return r;
  }

  async function arreterPartage(chemin) {
    await ecrire(chemin, async (d) => {
      if (!d.existe || !d.url) return;
      // (what was sent stays known as sent: never thinned)
      const leur = await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
      if (leur) await git.writeRef({ ...g(d), ref: `refs/nuee/envoye/${crypto.createHash('sha1').update(d.url).digest('hex').slice(0, 12)}`, value: leur, force: true });
      for (const n of await git.listRefs({ ...g(d), filepath: 'refs/remotes/origin' }).catch(() => [])) await git.deleteRef({ ...g(d), ref: `refs/remotes/origin/${n}` });
      await git.deleteRemote({ ...g(d), remote: 'origin' });
      await ecrireConfig(d, `branch.${d.branche}.remote`, undefined);
      await ecrireConfig(d, `branch.${d.branche}.merge`, undefined);
      await fs.promises.unlink(fichierBase(d)).catch(() => {});
      erreurs.delete(cle(d.chemin));
    });
    return {};
  }

  // A remote repository's projects: its default branch fetched alone (only its last commit) into a
  // temporary repository, the .nuee files of its tree. { fichiers, jeton }: jeton, when it asks for
  // a token and has none.
  async function listerDistant(url) {
    const a = adresseDistante(url);
    if (a.refus) throw refus('Donnez l’adresse du dépôt, qui commence par https://.', 'Give the repository’s address, starting with https://.');
    const dossier = await fs.promises.mkdtemp(path.join(temporaire(), 'distant-'));
    const d = { gitdir: path.join(dossier, 'depot.git'), url: a.url, hote: a.hote };
    d.fs = fsPour(d.gitdir);
    try {
      await git.init({ ...g(d), bare: true });
      await git.addRemote({ ...g(d), remote: 'origin', url: a.url });
      let f;
      try {
        f = await git.fetch({ ...g(d), http, remote: 'origin', remoteRef: 'HEAD', singleBranch: true, depth: 1, tags: false, ...auth(d) });
      } catch (e) {
        if (e.code === 'HttpError' && e.data && e.data.statusCode === 401 && !d.jetonDonne) return { fichiers: [], jeton: true };
        throw traduire(e, d);
      }
      const fichiers = [];
      const parcourir = async (arbre, prefixe) => {
        for (const e of await lireArbre(d, arbre)) {
          if (fichiers.length >= 1000) return;
          if (e.type === 'tree') await parcourir(e.oid, `${prefixe}${e.path}/`);
          else if (e.type === 'blob' && /\.nuee$/i.test(e.path)) fichiers.push(`${prefixe}${e.path}`);
        }
      };
      if (f.fetchHead) await parcourir((await lireCommit(d, f.fetchHead)).tree, '');
      return { fichiers: fichiers.sort(), jeton: false };
    } finally {
      await effacerDossier(dossier).catch(() => {});
    }
  }

  // A project opened from a remote repository: <dossier>/<its name>, and its history, shared with
  // that repository at that path. { chemin }
  async function ouvrirDistant(url, cheminDistant, dossier) {
    const a = adresseDistante(url);
    if (a.refus) throw refus('Donnez l’adresse du dépôt, qui commence par https://.', 'Give the repository’s address, starting with https://.');
    const dans = String(cheminDistant == null ? '' : cheminDistant).replace(/^\/+/, '');
    if (!/\.nuee$/i.test(dans) || dans.split('/').some((s) => !s || s === '.' || s === '..')) throw refus('Choisissez un projet (.nuee) du dépôt.', 'Choose a project (.nuee) of the repository.');
    if (typeof dossier !== 'string' || !path.isAbsolute(dossier) || !(await fs.promises.stat(dossier).then((s) => s.isDirectory(), () => false))) {
      throw refus('Choisissez un dossier où mettre le projet.', 'Choose a folder to put the project in.');
    }
    const nom = dans.split('/').pop();
    const chemin = path.join(path.resolve(dossier), nom);
    const d = lieux({ dossier: path.resolve(dossier), nom, chemin });
    if (await existe(chemin) || await existe(d.gitdir)) {
      throw refus(`Ce dossier a déjà un projet ${nom} : choisissez-en un autre.`, `This folder already has a project ${nom}: choose another one.`);
    }
    return enFile(fileDe(chemin), async () => {
      let fait = false;
      try {
        await creerDepot(d);
        return await avecVerrou(d, async () => {
          await git.addRemote({ ...g(d), remote: 'origin', url: a.url });
          await ecrireConfig(d, 'nuee.chemin', dans);
          Object.assign(d, { url: a.url, hote: a.hote, cheminProjet: dans, anciens: [] });
          let f;
          try {
            f = await git.fetch({ ...g(d), http, remote: 'origin', ref: 'main', remoteRef: 'HEAD', singleBranch: true, tags: false, ...auth(d) });
          } catch (e) { throw traduire(e, d); }
          if (!f.fetchHead) throw refus('Ce dépôt est vide.', 'This repository is empty.');
          const branche = String(f.defaultBranch || '').replace(/^refs\/heads\//, '') || 'main';
          await git.writeRef({ ...g(d), ref: `refs/heads/${branche}`, value: f.fetchHead, force: true });
          await git.writeRef({ ...g(d), ref: `refs/remotes/origin/${branche}`, value: f.fetchHead, force: true });
          if (branche !== 'main') await git.writeRef({ ...g(d), ref: 'HEAD', value: `refs/heads/${branche}`, symbolic: true, force: true });
          await ecrireConfig(d, `branch.${branche}.remote`, 'origin');
          await ecrireConfig(d, `branch.${branche}.merge`, `refs/heads/${branche}`);
          await lireDepot(d);
          if (!d.blobTete) throw refus(`Le dépôt n’a pas de projet ${dans}.`, `The repository has no project ${dans}.`);
          await ecrireProjet(d, d.blobTete);
          fait = true;
          await accorderNoms(d, { envoi: false }).catch(() => {});
          await ecrireConfig(d, 'nuee.echange', new Date(maintenant()).toISOString());
          return { chemin };
        });
      } finally {
        if (!fait) await effacerDossier(d.gitdir).catch(() => {});
      }
    });
  }

  // After Save as: the new file takes the old one's history (unless it has one), not shared: a
  // copy is another project, what its versions sent stays known as sent
  async function copierHistoire(ancien, nouveau) {
    if (typeof ancien !== 'string' || !path.isAbsolute(ancien)) throw refus('Les versions se gardent pour un fichier de projet (.nuee), donné par son chemin complet.', 'Versions are kept for a project file (.nuee), given by its full path.');
    const source = lieux({ dossier: path.dirname(path.resolve(ancien)), nom: path.basename(ancien), chemin: path.resolve(ancien) });
    if (!(await existe(path.join(source.gitdir, 'HEAD')))) return { copie: false };
    return ecrire(nouveau, async (d) => {
      if (d.existe) return { copie: false };
      await preparerDossier(d);
      await enFile(fileDe(ancien), () => avecVerrou(source, () => copierDossier(source.gitdir, d.gitdir)));
      d.existe = true;
      await lireDepot(d);
      return avecVerrou(d, async () => {
        if (d.url) {
          const leur = await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
          if (leur) await git.writeRef({ ...g(d), ref: `refs/nuee/envoye/${crypto.createHash('sha1').update(d.url).digest('hex').slice(0, 12)}`, value: leur, force: true });
          for (const n of await git.listRefs({ ...g(d), filepath: 'refs/remotes/origin' }).catch(() => [])) await git.deleteRef({ ...g(d), ref: `refs/remotes/origin/${n}` });
          await git.deleteRemote({ ...g(d), remote: 'origin' });
          await ecrireConfig(d, `branch.${d.branche}.remote`, undefined);
          await ecrireConfig(d, `branch.${d.branche}.merge`, undefined);
          await fs.promises.unlink(fichierBase(d)).catch(() => {});
        }
        return { copie: true };
      });
    });
  }

  // ------------------------------------------------------------------ for main.js and the checks

  function auPremierPlan() {
    for (const o of ouverts.values()) {
      if (maintenant() - o.recu < 5 * MINUTE) continue;
      o.recu = maintenant();
      arrierePlan(o.chemin, false);
    }
  }
  const fermerTout = () => Promise.all([...ouverts.values()].map((o) => aLaFermeture(o.chemin).catch(() => null)));
  async function enAttente() {
    while (taches.size) await Promise.all([...taches]);
  }

  // Every function one at a time per history (ecrire), and every error a message for the user,
  // without a token
  const protege = (f) => async (...args) => {
    try { return await f(...args); } catch (e) { throw new Refus(await sansJeton(traduire(e, null).message)); }
  };
  const api = {};
  for (const [nom, f] of Object.entries({ etat, ouvert, apresEnregistrement, aLaFermeture, marquer, liste, nommer, retirerNom, lire, restaurer,
    activer, reglages, regler, partager, jeton, verifierPartage, envoyer, recevoir, choisir, arreterPartage, listerDistant, ouvrirDistant, copierHistoire })) {
    api[nom] = protege(f);
  }
  Object.defineProperties(api, {
    auPremierPlan: { value: auPremierPlan }, fermerTout: { value: fermerTout }, enAttente: { value: enAttente },
  });
  return api;
}

module.exports = { creerVersions, magasinJetons, clientHttp, adresseDistante, nomRef, decrireChangements };
