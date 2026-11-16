// Versions of a project, kept with git without git to install (isomorphic-git, MIT), and sent to or
// received from GitHub or GitLab over HTTPS with a personal access token.
// - The repository is the one holding the project file's folder: the user's own when the folder
//   (or a parent) is one, else one made in that folder, branch main.
// - A version is a commit of the project file alone, made when asked: its tree is the branch's
//   with that file's entry replaced. Nothing else of the folder is ever added, committed, written
//   or deleted, nor staged (the index's entry for the project file is brought to each version, so
//   that git, for whoever uses it there, has nothing to commit). The working tree changes only
//   when the project file is restored or received.
// - Several projects may share a folder, and so a repository: each one's list shows the commits
//   that changed its file. Named versions are annotated tags.
// - A token is never logged, never put in an address, and never handed back once stored.
// Each function takes the project file's absolute path; its errors are thrown with a message in
// the user's language (`langue`). main.js gives net.fetch, safeStorage and the settings;
// tests/essai_versions.js gives others.
const fs = require('fs');
const path = require('path');
const { execFile } = require('child_process');
const git = require('isomorphic-git');

const pause = (ms) => new Promise((ok) => setTimeout(ok, ms));

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
  return {
    seulementSession: async () => !(await chiffrable()),
    present: async (hote) => session.has(hote) || ((await chiffrable()) && chiffre(hote) !== null),
    lire: async (hote) => session.get(hote) || ((await chiffrable()) && chiffre(hote) ? dechiffrer(hote, chiffre(hote)) : null),
    // (every token known, to blank one from any message)
    connus: async () => [...session.values(), ...((await chiffrable())
      ? await Promise.all(Object.entries(lus()).filter(([, x]) => typeof x === 'string').map(([h, x]) => dechiffrer(h, x))) : [])].filter(Boolean),
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

// The functions of window.bureau.versions (preload.js, main.js).
//   fetch: (url, init) → Response (main.js: net.fetch)
//   jetons: magasinJetons(...)
//   langue: () → 'fr' | 'en', the interface's
//   auteur: { lire: () → { nom, email } | null, ecrire({ nom, email }) }, kept with the settings
//   temporaire: () → the folder where lire() writes the versions it reads
function creerVersions({ fetch, jetons, langue = () => 'en', auteur: reglageAuteur, temporaire }) {
  const http = clientHttp(fetch);
  const t = (fr, en) => (langue() === 'fr' ? fr : en);
  const refus = (fr, en) => new Refus(t(fr, en));
  const pasActive = () => refus('Les versions ne sont pas activées pour ce projet.', 'Versions aren’t turned on for this project.');
  const modifie = () => refus('Le projet a des changements qui ne sont pas enregistrés comme version : enregistrez d’abord une version.',
    'The project has changes not saved as a version: save a version first.');

  // Objects read once: an oid names the same content in every repository
  const commits = new Map();
  const arbres = new Map();
  const ancetresMemo = new Map();
  const listes = new Map();
  const empreintes = new Map();
  const garder = (m, cle, valeur, max = 50000) => { if (m.size >= max) m.clear(); m.set(cle, valeur); return valeur; };

  // ------------------------------------------------------------------ the project and its repository

  // The repository holding a folder: the nearest one, from the folder up to its drive or share,
  // with a .git (a folder, or the file of a submodule). Not git.findRoot: on Windows it climbs from
  // C: to ".", this process's working folder, and from a share's root to "\" and "/".
  async function racineDepot(dossier) {
    for (let d = dossier; ; d = path.dirname(d)) {
      try { await fs.promises.lstat(path.join(d, '.git')); return d; } catch { /* not here */ }
      if (path.dirname(d) === d) return null;
    }
  }

  // `chemin` with the names below `base` written as on the disk: a tree's names must be the same
  // whatever case the path was typed in (Windows and macOS don't mind it)
  async function casse(base, chemin) {
    let r = base;
    for (const nom of path.relative(base, chemin).split(path.sep)) {
      let noms = [];
      try { noms = await fs.promises.readdir(r); } catch { /* unreadable: as given */ }
      const pareil = (n) => n.normalize('NFC').toLowerCase() === nom.normalize('NFC').toLowerCase();
      r = path.join(r, noms.includes(nom) ? nom : noms.find(pareil) || nom);
    }
    return r;
  }

  // The project file: its path, its folder, the repository holding it (racine, null: none) and its
  // path there (fichier, with "/")
  async function projet(chemin) {
    if (typeof chemin !== 'string' || !path.isAbsolute(chemin) || !/\.nuee$/i.test(chemin)) {
      throw refus('Les versions se gardent pour un fichier de projet (.nuee), donné par son chemin complet.',
        'Versions are kept for a project file (.nuee), given by its full path.');
    }
    let c = path.resolve(chemin);
    let st = null;
    try { st = await fs.promises.stat(c); } catch { /* below */ }
    if (!st || !st.isFile()) throw refus(`Le fichier du projet n’est plus là : ${c}`, `The project file isn’t there any more: ${c}`);
    const racine = await racineDepot(path.dirname(c));
    c = await casse(racine || path.dirname(c), c);
    const fichier = racine ? path.relative(racine, c).split(path.sep).join('/') : path.basename(c);
    return { chemin: c, dossier: path.dirname(c), racine, fichier, segments: fichier.split('/') };
  }

  const resoudre = (d, ref) => git.resolveRef({ fs: d.fs, dir: d.dir, ref }).catch((e) => {
    if (e.code === 'NotFoundError') return null;
    throw e;
  });

  // The repository, for a command: its branch (refused when none is checked out), the branch's last
  // commit (tete), its tree and the project file's blob there (refused when there is none: suivi),
  // and its remote (refused when there is none: distant)
  async function ouvrir(chemin, { suivi = true, distant = false } = {}) {
    const p = await projet(chemin);
    if (!p.racine) throw pasActive();
    const d = { ...p, dir: p.racine, fs: fsPour(p.racine) };
    d.branche = await git.currentBranch({ fs: d.fs, dir: d.dir });
    if (!d.branche) {
      throw refus('Le dépôt du projet n’est sur aucune branche (un état passé y est extrait) : revenez à une branche avec git.',
        'The project’s repository isn’t on a branch (a past state is checked out): go back to a branch with git.');
    }
    // (the remote's branch it follows, if git set one; else the same name)
    const suit = await git.getConfig({ fs: d.fs, dir: d.dir, path: `branch.${d.branche}.merge` });
    d.brancheDistante = typeof suit === 'string' ? suit.replace(/^refs\/heads\//, '') : d.branche;
    d.tete = await resoudre(d, `refs/heads/${d.branche}`);
    d.arbreTete = d.tete ? (await lireCommit(d, d.tete)).tree : null;
    d.blobTete = d.arbreTete ? await blobDans(d, d.arbreTete) : null;
    if (suivi && !d.blobTete) throw pasActive();
    d.url = await git.getConfig({ fs: d.fs, dir: d.dir, path: 'remote.origin.url' });
    d.hote = d.url ? hoteDe(d.url) : null;
    if (distant) exigerDistant(d);
    return d;
  }

  function exigerDistant(d) {
    if (!d.url) throw refus('Aucun dépôt sur GitHub ou GitLab n’est indiqué pour ce projet.', 'No repository on GitHub or GitLab is set for this project.');
    if (!d.hote) {
      throw refus(`Nuée envoie et reçoit par HTTPS seulement, et le dépôt de ce projet est à ${d.url}.`,
        `Nuée sends and receives over HTTPS only, and this project’s repository is at ${d.url}.`);
    }
  }

  async function lireCommit(d, oid) {
    if (commits.has(oid)) return commits.get(oid);
    const { commit } = await git.readCommit({ fs: d.fs, dir: d.dir, oid });
    return garder(commits, oid, { tree: commit.tree, parents: commit.parent, auteur: commit.author, message: commit.message });
  }

  async function lireArbre(d, oid) {
    if (arbres.has(oid)) return arbres.get(oid);
    return garder(arbres, oid, (await git.readTree({ fs: d.fs, dir: d.dir, oid })).tree);
  }

  // The project file's blob in a tree (null: not there)
  async function blobDans(d, arbre, segments = d.segments) {
    let oid = arbre;
    for (let i = 0; i < segments.length; i += 1) {
      const e = (await lireArbre(d, oid)).find((x) => x.path === segments[i]);
      if (!e || e.type !== (i < segments.length - 1 ? 'tree' : 'blob')) return null;
      oid = e.oid;
    }
    return oid;
  }

  // A tree written: `arbre` (null: empty) with the project file's entry set to `blob`, every
  // other entry as it was
  async function arbreAvec(d, arbre, blob, segments = d.segments) {
    const entrees = arbre ? [...(await lireArbre(d, arbre))] : [];
    const i = entrees.findIndex((e) => e.path === segments[0]);
    const avant = i >= 0 ? entrees.splice(i, 1)[0] : null;
    if (segments.length === 1) {
      const mode = avant && avant.type === 'blob' && /^100(644|755)$/.test(avant.mode) ? avant.mode : '100644';
      entrees.push({ mode, path: segments[0], oid: blob, type: 'blob' });
    } else {
      // (a file where the project's folder is would go: not replaced)
      if (avant && avant.type !== 'tree') {
        throw refus(`Dans le dépôt, ${avant.path} est un fichier, là où est le dossier du projet : utilisez git directement.`,
          `In the repository, ${avant.path} is a file where the project’s folder is: use git directly.`);
      }
      entrees.push({ mode: '040000', path: segments[0], oid: await arbreAvec(d, avant && avant.oid, blob, segments.slice(1)), type: 'tree' });
    }
    return git.writeTree({ fs: d.fs, dir: d.dir, tree: entrees });
  }

  // The paths that differ between two trees, the project file aside (five at most, for a message)
  async function differences(d, a, b, segments = d.segments, prefixe = '') {
    if (a === b) return [];
    const ea = a ? await lireArbre(d, a) : [];
    const eb = b ? await lireArbre(d, b) : [];
    const res = [];
    for (const nom of new Set([...ea, ...eb].map((e) => e.path))) {
      const ici = !!segments && segments[0] === nom;
      if (ici && segments.length === 1) continue;
      const x = ea.find((e) => e.path === nom);
      const y = eb.find((e) => e.path === nom);
      if (x && y && x.oid === y.oid && x.mode === y.mode) continue;
      if ((!x || x.type === 'tree') && (!y || y.type === 'tree')) {
        res.push(...await differences(d, x ? x.oid : null, y ? y.oid : null, ici ? segments.slice(1) : null, `${prefixe}${nom}/`));
      } else res.push(`${prefixe}${nom}`);
      if (res.length >= 5) break;
    }
    return res.slice(0, 5);
  }

  const autresFichiers = (autres) => refus(
    `Le dépôt a d’autres fichiers que le projet, et ils diffèrent de part et d’autre (${autres.join(', ')}) : utilisez git directement pour les réunir.`,
    `The repository holds other files than the project, and they differ between the two sides (${autres.join(', ')}): use git directly to bring them together.`);

  // Every commit reachable from `oid` (a missing parent, in a shallow clone, ends its line)
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
    return garder(ancetresMemo, oid, vus, 20);
  }

  // The versions of the project file reachable from `tete`: the commits where the file is and
  // differs from one of their parents' (from either for a merge, the choice of one side included),
  // or that have no parent; newest first.
  async function versionsDe(d, tete) {
    if (!tete) return [];
    const cle = `${d.dir}\n${d.fichier}\n${tete}`;
    if (listes.has(cle)) return listes.get(cle);
    const res = [];
    const vus = new Set([tete]);
    const pile = [tete];
    while (pile.length) {
      const oid = pile.pop();
      const c = await lireCommit(d, oid);
      const parents = [];
      for (const p of c.parents) {
        try { parents.push(await lireCommit(d, p)); } catch (e) { if (e.code !== 'NotFoundError') throw e; continue; }
        if (!vus.has(p)) { vus.add(p); pile.push(p); }
      }
      const blob = await blobDans(d, c.tree);
      if (blob && (!parents.length || (await Promise.all(parents.map((p) => blobDans(d, p.tree)))).some((b) => b !== blob))) {
        res.push({ oid, description: c.message.trim(), auteur: { nom: c.auteur.name, email: c.auteur.email },
          date: new Date(c.auteur.timestamp * 1000).toISOString(), temps: c.auteur.timestamp });
      }
    }
    // (by date, the walk's order between versions of the same second: from the newest)
    res.sort((a, b) => b.temps - a.temps);
    return garder(listes, cle, res, 200);
  }

  async function versionConnue(d, oid) {
    const v = typeof oid === 'string' && (await versionsDe(d, d.tete)).find((x) => x.oid === oid);
    if (!v) throw refus('Cette version n’est pas dans l’historique du projet.', 'This version isn’t in the project’s history.');
    return v;
  }

  // The project's names: each tag, its tag object (oid), its commit and the name it shows (the
  // first line of its message when the tag is that line made a ref, a name given in Nuée; else
  // the tag's own name)
  async function tags(d) {
    const res = new Map();
    for (const nom of await git.listTags({ fs: d.fs, dir: d.dir })) {
      const oid = await resoudre(d, `refs/tags/${nom}`);
      if (!oid) continue;
      let commit = oid;
      let affiche = nom;
      try {
        const { tag } = await git.readTag({ fs: d.fs, dir: d.dir, oid });
        commit = tag.object;
        const ligne = tag.message.split('\n')[0].trim();
        if (ligne && nomRef(ligne) === nom.normalize('NFC')) affiche = ligne;
      } catch { /* a tag without an object of its own: the commit's */ }
      res.set(nom, { oid, commit, affiche });
    }
    return res;
  }

  async function nomsParCommit(d) {
    const res = new Map();
    for (const { commit, affiche } of (await tags(d)).values()) res.set(commit, [...(res.get(commit) || []), affiche].sort());
    return res;
  }

  const sortie = (noms) => (v) => ({ oid: v.oid, description: v.description, auteur: v.auteur, date: v.date, noms: noms.get(v.oid) || [] });

  // The working file's blob, computed again (fraiche) or kept while the file's size and date stay
  // the same (for etat, asked often, of files that can be large)
  async function empreinte(c, fraiche = false) {
    const st = await fs.promises.stat(c);
    const m = empreintes.get(c);
    if (!fraiche && m && m.taille === st.size && m.date === st.mtimeMs && m.ino === st.ino) return m.oid;
    const { oid } = await git.hashBlob({ object: await fs.promises.readFile(c) });
    garder(empreintes, c, { taille: st.size, date: st.mtimeMs, ino: st.ino, oid }, 200);
    return oid;
  }

  function auteurExige() {
    const a = reglageAuteur.lire();
    if (!a || !a.nom || !a.email) {
      throw refus('Donnez d’abord votre nom et votre adresse e-mail : ils sont écrits avec chaque version.',
        'Give your name and email first: they are written with each version.');
    }
    return { name: a.nom, email: a.email };
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

  // The index's entry for the project file brought to the file as it is (and so to its version)
  const indexer = (d) => git.updateIndex({ fs: d.fs, dir: d.dir, filepath: d.fichier, add: true });

  // A version of `contenu` (the working file): its blob, the branch's tree with it, a commit on the
  // branch, then the index's entry
  async function sauver(d, message, auteur, contenu) {
    const blob = await git.writeBlob({ fs: d.fs, dir: d.dir, blob: contenu });
    const tree = await arbreAvec(d, d.arbreTete, blob);
    const oid = await git.commit({ fs: d.fs, dir: d.dir, ref: `refs/heads/${d.branche}`, message, author: auteur, tree, parent: d.tete ? [d.tete] : [] });
    await indexer(d);
    return oid;
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
    if (code === 'UnmergedPathsError') return refus('Le dépôt est au milieu d’une fusion : terminez-la d’abord avec git.', 'The repository is in the middle of a merge: finish it with git first.');
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

  // The remote's names: name → its tag (refs/tags/)
  async function nomsDistants(d) {
    const refs = await reseau(d, () => git.listServerRefs({ http, url: d.url, prefix: 'refs/tags/', protocolVersion: 1, ...auth(d) }));
    return new Map(refs.filter((r) => !r.ref.endsWith('^{}')).map((r) => [r.ref.slice('refs/tags/'.length), r.oid]));
  }

  // The names both sides had at the last exchange, each with its tag, kept with the remote's
  // address in the repository (.git/nuee-noms.json): one gone since from one side, and the same
  // tag on the other, was removed there
  async function fichierBase(d) {
    const g = path.join(d.racine, '.git');
    if ((await fs.promises.stat(g)).isDirectory()) return path.join(g, 'nuee-noms.json');
    const m = /^gitdir:\s*(.+)$/m.exec(await fs.promises.readFile(g, 'utf8'));  // (a submodule)
    return path.join(m ? path.resolve(d.racine, m[1].trim()) : g, 'nuee-noms.json');
  }
  async function baseNoms(d) {
    try {
      const b = JSON.parse(await fs.promises.readFile(await fichierBase(d), 'utf8'));
      if (b.url === d.url && b.noms && typeof b.noms === 'object') return new Map(Object.entries(b.noms));
    } catch { /* no exchange yet */ }
    return new Map();
  }

  // The names put in step with the remote's, from what each side did since the last exchange: a
  // name added on one side is added on the other, one removed is removed on the other (unless the
  // other gave it since to another version: that one stays). `avant`: this side's names before the
  // exchange (a receive's fetch brings the remote's new ones); `distants`: the remote's, name → tag.
  // Receiving changes this side only; sending, the remote's too (its new names wait for the next
  // receive, which brings their tags). A name on both sides with different tags is left as it is.
  async function accorderNoms(d, avant, distants, { envoi }) {
    const base = await baseNoms(d);
    const ici = await tags(d);
    const garde = new Map();
    const pousser = (nom, supprimer) => reseau(d, () => git.push({ fs: d.fs, http, dir: d.dir, remote: 'origin', ref: `refs/tags/${nom}`,
      remoteRef: `refs/tags/${nom}`, delete: supprimer, ...auth(d) }));
    for (const nom of new Set([...avant.keys(), ...distants.keys(), ...base.keys()])) {
      const local = avant.has(nom) ? avant.get(nom).oid : null;
      const la = distants.get(nom) || null;
      const connu = base.get(nom) || null;
      if (local && la) garde.set(nom, la);
      else if (local) {
        if (local === connu) await git.deleteTag({ fs: d.fs, dir: d.dir, ref: nom });  // removed on the remote
        else if (envoi) { await pousser(nom, false); garde.set(nom, local); }
      } else if (la) {
        if (la === connu && envoi) {
          // removed here: removed there (a deletion pushes a local ref, put back for the time of it)
          await git.writeRef({ fs: d.fs, dir: d.dir, ref: `refs/tags/${nom}`, value: la, force: true });
          try { await pousser(nom, true); } finally { await git.deleteRef({ fs: d.fs, dir: d.dir, ref: `refs/tags/${nom}` }); }
        } else if (la === connu) {
          garde.set(nom, la);  // (removed here, not sent yet: the fetch brought it back)
          if (ici.has(nom)) await git.deleteTag({ fs: d.fs, dir: d.dir, ref: nom });
        } else if (ici.has(nom)) garde.set(nom, la);  // new there, fetched
      }
    }
    await fs.promises.writeFile(await fichierBase(d), JSON.stringify({ url: d.url, noms: Object.fromEntries([...garde].sort()) }));
  }

  // The branch sent, then the names
  async function envoyerDepot(d) {
    const messages = [];
    try {
      await git.push({ fs: d.fs, http, dir: d.dir, remote: 'origin', ref: d.branche, remoteRef: `refs/heads/${d.brancheDistante}`,
        onMessage: (m) => { messages.push(m); }, ...auth(d) });
    } catch (e) {
      throw traduire(e, d, messages);
    }
    const noms = await tags(d);
    await accorderNoms(d, noms, await nomsDistants(d), { envoi: true });
  }

  // ------------------------------------------------------------------ the functions

  async function etat(chemin) {
    const p = await projet(chemin);
    const a = reglageAuteur.lire();
    const res = { depot: !!p.racine, suivi: false, racine: p.racine, modifie: false, versions: 0, distant: null, jeton: false,
      jetonSession: await jetons.seulementSession(), avance: 0, retard: 0, auteur: a && a.nom ? { nom: a.nom, email: a.email } : null };
    if (!p.racine) return res;
    const d = { ...p, dir: p.racine, fs: fsPour(p.racine) };
    const branche = await git.currentBranch({ fs: d.fs, dir: d.dir });
    const tete = await resoudre(d, branche ? `refs/heads/${branche}` : 'HEAD');
    const blob = tete ? await blobDans(d, (await lireCommit(d, tete)).tree) : null;
    if (blob) {
      res.suivi = true;
      res.modifie = (await empreinte(d.chemin)) !== blob;
      res.versions = (await versionsDe(d, tete)).length;
    }
    const url = await git.getConfig({ fs: d.fs, dir: d.dir, path: 'remote.origin.url' });
    if (!url) return res;
    const hote = hoteDe(url);
    res.distant = { url, hote };
    res.jeton = !!hote && await jetons.present(hote);
    if (tete && branche) {
      const suit = await git.getConfig({ fs: d.fs, dir: d.dir, path: `branch.${branche}.merge` });
      const leur = await resoudre(d, `refs/remotes/origin/${typeof suit === 'string' ? suit.replace(/^refs\/heads\//, '') : branche}`);
      const nous = await ancetres(d, tete);
      const eux = await ancetres(d, leur);
      res.avance = [...nous].filter((o) => !eux.has(o)).length;
      res.retard = [...eux].filter((o) => !nous.has(o)).length;
    }
    return res;
  }

  async function activer(chemin) {
    const auteur = auteurExige();
    const p = await projet(chemin);
    if (!p.racine) {
      await git.init({ fs: fsPour(p.dossier), dir: p.dossier, defaultBranch: 'main' });
      // (hidden on Windows, as git makes it: a folder the user has no use for, and must not delete)
      if (process.platform === 'win32') execFile('attrib', ['+h', path.join(p.dossier, '.git')], { windowsHide: true }, () => {});
    }
    const d = await ouvrir(chemin, { suivi: false });
    if (d.blobTete) return { oid: d.tete };
    return { oid: await sauver(d, t('Première version', 'First version'), auteur, await fs.promises.readFile(d.chemin)) };
  }

  async function enregistrerVersion(chemin, description) {
    const message = String(description == null ? '' : description).trim();
    if (!message) throw refus('Décrivez cette version en quelques mots.', 'Describe this version in a few words.');
    const auteur = auteurExige();
    const d = await ouvrir(chemin);
    const contenu = await fs.promises.readFile(d.chemin);
    if ((await git.hashBlob({ object: contenu })).oid === d.blobTete) {
      throw refus('Le projet n’a pas changé depuis sa dernière version.', 'The project hasn’t changed since its last version.');
    }
    return { oid: await sauver(d, message, auteur, contenu) };
  }

  async function liste(chemin) {
    const p = await projet(chemin);
    if (!p.racine) return [];
    const d = { ...p, dir: p.racine, fs: fsPour(p.racine) };
    const branche = await git.currentBranch({ fs: d.fs, dir: d.dir });
    const tete = await resoudre(d, branche ? `refs/heads/${branche}` : 'HEAD');
    return (await versionsDe(d, tete)).map(sortie(await nomsParCommit(d)));
  }

  async function nommer(chemin, oid, nom) {
    const auteur = auteurExige();
    const d = await ouvrir(chemin);
    await versionConnue(d, oid);
    const texte = String(nom == null ? '' : nom).normalize('NFC').trim().replace(/\s+/g, ' ');
    const ref = nomRef(texte);
    if (!ref) throw refus('Donnez un nom à la version (des lettres ou des chiffres).', 'Give the version a name (letters or digits).');
    // (the same name whatever its case: Windows and macOS keep tags as files)
    for (const [cle, x] of await tags(d)) {
      if (cle.toLowerCase() === ref.toLowerCase() || x.affiche.toLowerCase() === texte.toLowerCase()) {
        throw refus(`Le nom « ${texte} » est déjà donné à une version.`, `The name “${texte}” is already given to a version.`);
      }
    }
    await git.annotatedTag({ fs: d.fs, dir: d.dir, ref, object: oid, message: texte, tagger: auteur });
    return { nom: texte };
  }

  async function retirerNom(chemin, nom) {
    const d = await ouvrir(chemin);
    const texte = String(nom == null ? '' : nom).normalize('NFC').trim();
    const trouve = [...(await tags(d))].find(([cle, x]) => x.affiche === texte || cle === texte);
    if (!trouve) throw refus(`Aucune version ne s’appelle « ${texte} ».`, `No version is named “${texte}”.`);
    await git.deleteTag({ fs: d.fs, dir: d.dir, ref: trouve[0] });
  }

  async function lire(chemin, oid) {
    const d = await ouvrir(chemin);
    await versionConnue(d, oid);
    const { blob } = await git.readBlob({ fs: d.fs, dir: d.dir, oid: await blobDans(d, (await lireCommit(d, oid)).tree) });
    const dossier = await fs.promises.mkdtemp(path.join(temporaire(), 'version-'));
    const fichier = path.join(dossier, path.basename(d.chemin));
    await fs.promises.writeFile(fichier, blob);
    return { fichier };
  }

  async function restaurer(chemin, oid) {
    const auteur = auteurExige();
    const d = await ouvrir(chemin);
    const v = await versionConnue(d, oid);
    if ((await empreinte(d.chemin, true)) !== d.blobTete) throw modifie();
    const blobV = await blobDans(d, (await lireCommit(d, oid)).tree);
    if (blobV === d.blobTete) throw refus('Cette version est le projet tel qu’il est.', 'This version is the project as it is now.');
    const { blob } = await git.readBlob({ fs: d.fs, dir: d.dir, oid: blobV });
    await remplacerFichier(d.chemin, blob);
    const date = new Date(v.temps * 1000).toLocaleString(t('fr-FR', 'en-GB'), { dateStyle: 'long', timeStyle: 'short' });
    return { oid: await sauver(d, t(`Version du ${date} restaurée`, `Restored the version of ${date}`), auteur, blob) };
  }

  function auteur(a) {
    const nom = String((a && a.nom) || '').trim().replace(/\s+/g, ' ');
    const email = String((a && a.email) || '').trim();
    if (!nom || nom.length > 200 || /[<>]/.test(nom) || email.length > 200 || !/^[^\s@<>]+@[^\s@<>]+$/.test(email)) {
      throw refus('Donnez un nom, et une adresse e-mail comme nom@exemple.org.', 'Give a name, and an email address such as name@example.org.');
    }
    reglageAuteur.ecrire({ nom, email });
    return { nom, email };
  }

  async function distant(chemin, url) {
    const d = await ouvrir(chemin, { suivi: false });
    const a = adresseDistante(url);
    if (a.refus === 'identifiants') {
      throw refus('Donnez l’adresse sans nom d’utilisateur ni jeton : le jeton se donne à part.', 'Give the address without a user name or token: the token is given apart.');
    }
    if (a.refus) {
      throw refus('Donnez l’adresse du dépôt, qui commence par https:// (sur GitHub ou GitLab, son bouton Code ou Cloner).',
        'Give the repository’s address, starting with https:// (on GitHub or GitLab, its Code or Clone button).');
    }
    if (d.url === a.url) return { url: a.url, hote: a.hote };
    if (d.url) await git.setConfig({ fs: d.fs, dir: d.dir, path: 'remote.origin.url', value: a.url });
    else await git.addRemote({ fs: d.fs, dir: d.dir, remote: 'origin', url: a.url });
    // another repository: what was known of the old one goes (its branches, fetched again from the
    // new one; the names it had, kept with its address)
    for (const r of await git.listRefs({ fs: d.fs, dir: d.dir, filepath: 'refs/remotes/origin' })) {
      await git.deleteRef({ fs: d.fs, dir: d.dir, ref: `refs/remotes/origin/${r}` });
    }
    return { url: a.url, hote: a.hote };
  }

  async function jeton(hote, valeur) {
    const h = String(hote == null ? '' : hote).trim().toLowerCase();
    if (!/^[a-z0-9]([a-z0-9.-]*[a-z0-9])?(:\d{1,5})?$/.test(h)) throw refus('Donnez le nom du serveur, comme github.com.', 'Give the server’s name, such as github.com.');
    const j = String(valeur == null ? '' : valeur).trim();
    if (/\s/.test(j) || j.length > 1000) throw refus('Un jeton est un seul mot, tel que GitHub ou GitLab l’a donné.', 'A token is one word, as GitHub or GitLab gave it.');
    await jetons.ecrire(h, j);
    return { jeton: !!j, jetonSession: await jetons.seulementSession() };
  }

  async function verifierDistant(chemin) {
    const d = await ouvrir(chemin, { suivi: false });
    try {
      exigerDistant(d);
      // (the push side, which needs the token's write access)
      await reseau(d, () => git.getRemoteInfo2({ http, url: d.url, forPush: true, protocolVersion: 1, ...auth(d) }));
      return { ok: true, message: t(`Le dépôt sur ${d.hote} répond, et le jeton permet d’y envoyer des versions.`, `The repository on ${d.hote} answers, and the token may send versions to it.`) };
    } catch (e) {
      return { ok: false, message: await sansJeton(traduire(e, d).message) };
    }
  }

  async function envoyer(chemin) {
    const d = await ouvrir(chemin, { distant: true });
    await envoyerDepot(d);
    return { oid: d.tete };
  }

  async function recevoir(chemin) {
    const d = await ouvrir(chemin, { distant: true });
    if ((await empreinte(d.chemin, true)) !== d.blobTete) throw modifie();
    const avant = await tags(d);
    let rien = false;
    try {
      await git.fetch({ fs: d.fs, http, dir: d.dir, remote: 'origin', ref: d.branche, remoteRef: d.brancheDistante, singleBranch: false, tags: true, ...auth(d) });
    } catch (e) {
      if (e.code !== 'NotFoundError') throw traduire(e, d);
      rien = true;  // (the remote hasn't this branch)
    }
    await accorderNoms(d, avant, await nomsDistants(d), { envoi: false });
    const leur = rien ? null : await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
    const nous = await ancetres(d, d.tete);
    if (!leur || nous.has(leur)) return { resultat: 'a_jour' };
    const arbreLeur = (await lireCommit(d, leur)).tree;
    const autres = await differences(d, d.arbreTete, arbreLeur);
    if (autres.length) throw autresFichiers(autres);
    const blobLeur = await blobDans(d, arbreLeur);
    if (!blobLeur) throw refus(`Les versions reçues n’ont pas ${d.fichier} : utilisez git directement.`, `The versions received don’t hold ${d.fichier}: use git directly.`);
    const eux = await ancetres(d, leur);
    if (eux.has(d.tete)) {
      if (blobLeur !== d.blobTete) await remplacerFichier(d.chemin, (await git.readBlob({ fs: d.fs, dir: d.dir, oid: blobLeur })).blob);
      await git.writeRef({ fs: d.fs, dir: d.dir, ref: `refs/heads/${d.branche}`, value: leur, force: true });
      await indexer(d);
      return { resultat: 'avance' };
    }
    const noms = await nomsParCommit(d);
    return {
      resultat: 'diverge',
      mien: (await versionsDe(d, d.tete)).filter((v) => !eux.has(v.oid)).map(sortie(noms)),
      leur: (await versionsDe(d, leur)).filter((v) => !nous.has(v.oid)).map(sortie(noms)),
    };
  }

  async function choisir(chemin, cote) {
    const auteur = auteurExige();
    const d = await ouvrir(chemin, { distant: true });
    if ((await empreinte(d.chemin, true)) !== d.blobTete) throw modifie();
    const leur = await resoudre(d, `refs/remotes/origin/${d.brancheDistante}`);
    if (!['mien', 'leur'].includes(cote) || !leur || (await ancetres(d, d.tete)).has(leur) || (await ancetres(d, leur)).has(d.tete)) {
      throw refus('Il n’y a rien à choisir : recevez d’abord les versions.', 'There is nothing to choose: receive the versions first.');
    }
    const arbreLeur = (await lireCommit(d, leur)).tree;
    const autres = await differences(d, d.arbreTete, arbreLeur);
    if (autres.length) throw autresFichiers(autres);
    const blobLeur = await blobDans(d, arbreLeur);
    if (!blobLeur) throw refus(`Les versions reçues n’ont pas ${d.fichier} : utilisez git directement.`, `The versions received don’t hold ${d.fichier}: use git directly.`);
    // A merge of the two histories whose tree is the side chosen (theirs: written to the file before
    // the branch moves to it)
    const tree = cote === 'mien' ? d.arbreTete : arbreLeur;
    const message = cote === 'mien' ? t('Ma version gardée plutôt que celle reçue', 'Kept my version rather than the one received')
      : t('Version reçue prise plutôt que la mienne', 'Took the version received rather than mine');
    const oid = await git.commit({ fs: d.fs, dir: d.dir, ref: `refs/heads/${d.branche}`, message, author: auteur, tree, parent: [d.tete, leur], noUpdateBranch: cote === 'leur' });
    if (cote === 'leur') {
      if (blobLeur !== d.blobTete) await remplacerFichier(d.chemin, (await git.readBlob({ fs: d.fs, dir: d.dir, oid: blobLeur })).blob);
      await git.writeRef({ fs: d.fs, dir: d.dir, ref: `refs/heads/${d.branche}`, value: oid, force: true });
      await indexer(d);
    }
    Object.assign(d, { tete: oid, arbreTete: tree });
    try {
      await envoyerDepot(d);
    } catch (e) {
      const m = traduire(e, d).message;
      throw refus(`Votre choix est enregistré, mais l’envoi a échoué : ${m}`, `Your choice is saved, but sending failed: ${m}`);
    }
    return { oid };
  }

  // One command at a time (they read and write the same repositories), and every error a message
  // for the user, without a token
  let file = Promise.resolve();
  const commande = (f, { attendre = true } = {}) => async (...args) => {
    const faire = async () => {
      try { return await f(...args); } catch (e) { throw new Refus(await sansJeton(traduire(e, null).message)); }
    };
    if (!attendre) return faire();
    const r = file.then(faire);
    file = r.then(() => {}, () => {});
    return r;
  };
  return {
    etat: commande(etat), activer: commande(activer), enregistrerVersion: commande(enregistrerVersion), liste: commande(liste),
    nommer: commande(nommer), retirerNom: commande(retirerNom), lire: commande(lire), restaurer: commande(restaurer),
    auteur: commande(auteur, { attendre: false }), distant: commande(distant), jeton: commande(jeton, { attendre: false }),
    verifierDistant: commande(verifierDistant), envoyer: commande(envoyer), recevoir: commande(recevoir), choisir: commande(choisir),
  };
}

module.exports = { creerVersions, magasinJetons, clientHttp, adresseDistante, nomRef };
