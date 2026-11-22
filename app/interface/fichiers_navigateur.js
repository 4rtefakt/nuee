// The browser's storage as a file system, for History in the browser version (versions_navigateur.js):
// the promise-based functions isomorphic-git takes (readFile, writeFile, unlink, readdir, mkdir,
// rmdir, stat, lstat, readlink, symlink, chmod) and those app/versions.js uses besides (rename,
// mkdtemp, copyFile, utimes), on absolute paths made with "/". In the origin private file system
// (OPFS), else in IndexedDB, as the recovery copies (secours_navigateur.js): a browser without OPFS,
// or whose OPFS can't write a file from the page (no createWritable: Safari). Its errors have Node's
// codes (ENOENT, EEXIST, ENOTDIR, ENOTEMPTY, EISDIR), which both read. A file written is whole or as
// it was: OPFS replaces it when its writer closes, IndexedDB in one transaction.
const NOM = 'nuee-versions';

const erreur = (code, chemin) => Object.assign(new Error(`${code}: ${chemin}`), { code });
// (the parts of an absolute path: "/a/b" → ["a", "b"]; the root, [])
const parties = (chemin) => String(chemin).split('/').filter((s) => s && s !== '.');
const texte = (options) => ['utf8', 'utf-8'].includes(String((typeof options === 'string' ? options : options && options.encoding) || '').toLowerCase());
// (a copy of its own: a Buffer can be a view of a larger memory, which IndexedDB would keep whole)
const octets = (donnees) => (typeof donnees === 'string' ? new TextEncoder().encode(donnees) : new Uint8Array(donnees));
const lu = (o, options) => (texte(options) ? new TextDecoder().decode(o) : o);
const stats = (dossier, taille, date) => ({
  size: taille, mtimeMs: date, ctimeMs: date, mode: dossier ? 0o40755 : 0o100644, ino: 0, dev: 0, uid: 0, gid: 0,
  isFile: () => !dossier, isDirectory: () => dossier, isSymbolicLink: () => false,
});
const entree = (nom, dossier) => ({ name: nom, isFile: () => !dossier, isDirectory: () => dossier, isSymbolicLink: () => false });
const hasard = () => Math.random().toString(36).slice(2, 8).padEnd(6, '0');

// What both stores give: the functions on the paths (a, b: the parts of a path)
function systeme(type, s) {
  const p = {
    readFile: async (chemin, options) => lu(await s.lire(parties(chemin), chemin), options),
    // (flag "wx": only if there is none, as the lock of a history is taken)
    writeFile: async (chemin, donnees, options) => s.ecrire(parties(chemin), octets(donnees), { exclusif: !!(options && typeof options === 'object' && /x/.test(options.flag || '')), chemin }),
    unlink: (chemin) => s.supprimer(parties(chemin), false, chemin),
    rmdir: (chemin) => s.supprimer(parties(chemin), true, chemin),
    mkdir: async (chemin, options) => {
      const a = parties(chemin);
      if (!(options && options.recursive)) return s.creerDossier(a, chemin);
      for (let i = 1; i <= a.length; i += 1) {
        try { await s.creerDossier(a.slice(0, i), chemin); } catch (e) { if (e.code !== 'EEXIST' || !(await s.etat(a.slice(0, i), chemin)).isDirectory()) throw e; }
      }
      return undefined;
    },
    readdir: async (chemin, options) => {
      const l = await s.lister(parties(chemin), chemin);
      return options && options.withFileTypes ? l.map(([nom, dossier]) => entree(nom, dossier)) : l.map(([nom]) => nom);
    },
    stat: (chemin) => s.etat(parties(chemin), chemin),
    lstat: (chemin) => s.etat(parties(chemin), chemin),
    // (no symbolic links here, nor in a history's repository)
    readlink: async (chemin) => { await s.etat(parties(chemin), chemin); throw erreur('EINVAL', chemin); },
    symlink: async (_, chemin) => { throw erreur('ENOSYS', chemin); },
    chmod: async (chemin) => { await s.etat(parties(chemin), chemin); },
    // (the time a file was last written, now: what a history's lock is kept fresh by)
    utimes: (chemin) => s.toucher(parties(chemin), chemin),
    rename: async (de, vers) => {
      const [a, b] = [parties(de), parties(vers)];
      if (a.join('/') === b.join('/')) { await s.etat(a, de); return; }
      await s.deplacer(a, b, de);
    },
    copyFile: async (de, vers) => s.ecrire(parties(vers), await s.lire(parties(de), de), { chemin: vers }),
    mkdtemp: async (prefixe) => {
      for (;;) {
        const chemin = `${prefixe}${hasard()}`;
        try { await s.creerDossier(parties(chemin), chemin); return chemin; } catch (e) { if (e.code !== 'EEXIST') throw e; }
      }
    },
  };
  // (isomorphic-git takes `promises` when it is an own enumerable property, as Node's fs has it)
  return { type, promises: p };
}

// ---- OPFS: a folder of the origin's private file system, its folders and files as they are

async function opfs() {
  const racine = await (await navigator.storage.getDirectory()).getDirectoryHandle(NOM, { create: true });
  const essai = await racine.getFileHandle('.essai', { create: true });
  if (typeof essai.createWritable !== 'function') throw new Error('createWritable');
  await racine.removeEntry('.essai');
  // (the folders already found, by path; forgotten with what is removed or moved here: nothing else
  // removes a folder of this storage)
  const dossiers = new Map([['', racine]]);
  const oublier = (a) => { const k = a.join('/'); for (const x of [...dossiers.keys()]) if (x === k || x.startsWith(`${k}/`)) dossiers.delete(x); };
  // (a DOMException's own `code` is a number: Node's are words)
  const traduire = (e, chemin, dansDossier = false) => {
    if (e && typeof e.code === 'string') return e;
    if (e && e.name === 'NotFoundError') return erreur('ENOENT', chemin);
    if (e && e.name === 'TypeMismatchError') return erreur(dansDossier ? 'ENOTDIR' : 'EISDIR', chemin);
    if (e && e.name === 'InvalidModificationError') return erreur('ENOTEMPTY', chemin);
    return e;
  };
  async function dossier(a, chemin) {
    let h = racine;
    for (let i = 1; i <= a.length; i += 1) {
      const k = a.slice(0, i).join('/');
      if (!dossiers.has(k)) {
        try { dossiers.set(k, await h.getDirectoryHandle(a[i - 1])); } catch (e) { throw traduire(e, chemin, true); }
      }
      h = dossiers.get(k);
    }
    return h;
  }
  // (what is at a path: { fichier } or { dossier }; the root is a folder)
  async function trouver(a, chemin) {
    if (!a.length) return { dossier: racine };
    const parent = await dossier(a.slice(0, -1), chemin);
    const nom = a[a.length - 1];
    try { return { parent, nom, fichier: await parent.getFileHandle(nom) }; } catch (e) {
      if (e.name !== 'TypeMismatchError') throw traduire(e, chemin);
    }
    return { parent, nom, dossier: await dossier(a, chemin) };
  }
  const ecrireDans = async (h, o, chemin) => {
    try { const w = await h.createWritable(); await w.write(o); await w.close(); } catch (e) { throw traduire(e, chemin); }
  };
  const fichier = async (h, chemin) => { try { return await h.getFile(); } catch (e) { throw traduire(e, chemin); } };
  async function ecrire(a, o, { exclusif = false, chemin }) {
    if (!a.length) throw erreur('EISDIR', chemin);
    const parent = await dossier(a.slice(0, -1), chemin);
    const nom = a[a.length - 1];
    const faire = async () => {
      let h;
      try { h = await parent.getFileHandle(nom); } catch (e) {
        if (e.name !== 'NotFoundError') throw traduire(e, chemin);
        h = await parent.getFileHandle(nom, { create: true });
        return ecrireDans(h, o, chemin);
      }
      if (exclusif) throw erreur('EEXIST', chemin);
      return ecrireDans(h, o, chemin);
    };
    // (only if there is none: one tab at a time looks and writes, as a lock's file is taken)
    return exclusif && navigator.locks ? navigator.locks.request(`${NOM}-exclusif`, faire) : faire();
  }
  const s = {
    async lire(a, chemin) {
      const x = await trouver(a, chemin);
      if (!x.fichier) throw erreur('EISDIR', chemin);
      return new Uint8Array(await (await fichier(x.fichier, chemin)).arrayBuffer());
    },
    ecrire,
    async supprimer(a, estDossier, chemin) {
      const x = await trouver(a, chemin);
      if (!a.length) throw erreur('EPERM', chemin);
      if (estDossier && !x.dossier) throw erreur('ENOTDIR', chemin);
      if (!estDossier && !x.fichier) throw erreur('EISDIR', chemin);
      try { await x.parent.removeEntry(x.nom); } catch (e) { throw traduire(e, chemin); }
      if (estDossier) oublier(a);
    },
    async creerDossier(a, chemin) {
      if (!a.length) throw erreur('EEXIST', chemin);
      const parent = await dossier(a.slice(0, -1), chemin);
      // (a folder or a file already there: EEXIST)
      try { await parent.getDirectoryHandle(a[a.length - 1]); } catch (e) {
        if (e.name === 'NotFoundError') { await parent.getDirectoryHandle(a[a.length - 1], { create: true }); return; }
        if (e.name !== 'TypeMismatchError') throw traduire(e, chemin);
      }
      throw erreur('EEXIST', chemin);
    },
    async lister(a, chemin) {
      const x = await trouver(a, chemin);
      if (!x.dossier) throw erreur('ENOTDIR', chemin);
      const l = [];
      for await (const [nom, h] of x.dossier.entries()) l.push([nom, h.kind === 'directory']);
      return l;
    },
    async etat(a, chemin) {
      const x = await trouver(a, chemin);
      if (x.dossier) return stats(true, 0, 0);
      const f = await fichier(x.fichier, chemin);
      return stats(false, f.size, f.lastModified);
    },
    async toucher(a, chemin) {
      const x = await trouver(a, chemin);
      if (x.fichier) await ecrireDans(x.fichier, new Uint8Array(await (await fichier(x.fichier, chemin)).arrayBuffer()), chemin);
    },
    // (a file read, written there, then removed here; a folder, everything in it so)
    async deplacer(de, vers, chemin) {
      const x = await trouver(de, chemin);
      if (x.fichier) {
        await ecrire(vers, await s.lire(de, chemin), { chemin });
        await x.parent.removeEntry(x.nom);
        return;
      }
      const copier = async (a, b) => {
        await s.creerDossier(b, chemin);
        for (const [nom, estDossier] of await s.lister(a, chemin)) {
          if (estDossier) await copier([...a, nom], [...b, nom]);
          else await ecrire([...b, nom], await s.lire([...a, nom], chemin), { chemin });
        }
      };
      await copier(de, vers);
      await x.parent.removeEntry(x.nom, { recursive: true });
      oublier(de);
    },
  };
  return systeme('OPFS', s);
}

// ---- IndexedDB: an entry per path ({ dossier, parent, taille, date }, the parent indexed for a
// folder's list), the files' bytes in a store of their own

function indexedDb() {
  return new Promise((ok, ko) => {
    const r = indexedDB.open(NOM, 1);
    r.onupgradeneeded = () => {
      r.result.createObjectStore('entrees').createIndex('parent', 'parent');
      r.result.createObjectStore('contenus');
    };
    r.onerror = () => ko(r.error);
    r.onsuccess = () => ok(r.result);
  }).then((db) => {
    const requete = (q) => new Promise((ok, ko) => { q.onsuccess = () => ok(q.result); q.onerror = () => ko(q.error); });
    // (one transaction: what it reads and writes, together; an error leaves everything as it was)
    async function faire(mode, f) {
      const tx = db.transaction(['entrees', 'contenus'], mode);
      const fin = new Promise((ok, ko) => { tx.oncomplete = () => ok(); tx.onerror = () => ko(tx.error); tx.onabort = () => ko(tx.error || new Error('abort')); });
      let r;
      try { r = await f(tx.objectStore('entrees'), tx.objectStore('contenus')); } catch (e) {
        try { tx.abort(); } catch { /* already over */ }
        await fin.catch(() => {});
        throw e;
      }
      await fin;
      return r;
    }
    const cle = (a) => `/${a.join('/')}`;
    const racine = { dossier: true, taille: 0, date: 0 };
    const lireEntree = async (e, a) => (a.length ? (await requete(e.get(cle(a)))) || null : racine);
    const exigerDossier = async (e, a, chemin) => {
      const x = await lireEntree(e, a);
      if (!x) throw erreur('ENOENT', chemin);
      if (!x.dossier) throw erreur('ENOTDIR', chemin);
    };
    const ecrire = (a, o, { exclusif = false, chemin }) => faire('readwrite', async (e, c) => {
      if (!a.length) throw erreur('EISDIR', chemin);
      await exigerDossier(e, a.slice(0, -1), chemin);
      const x = await lireEntree(e, a);
      if (x && x.dossier) throw erreur('EISDIR', chemin);
      if (x && exclusif) throw erreur('EEXIST', chemin);
      await requete(e.put({ dossier: false, parent: cle(a.slice(0, -1)), taille: o.length, date: Date.now() }, cle(a)));
      await requete(c.put(o, cle(a)));
    });
    const s = {
      lire: (a, chemin) => faire('readonly', async (e, c) => {
        const x = await lireEntree(e, a);
        if (!x) throw erreur('ENOENT', chemin);
        if (x.dossier) throw erreur('EISDIR', chemin);
        return new Uint8Array(await requete(c.get(cle(a))));
      }),
      ecrire,
      supprimer: (a, estDossier, chemin) => faire('readwrite', async (e, c) => {
        if (!a.length) throw erreur('EPERM', chemin);
        const x = await lireEntree(e, a);
        if (!x) throw erreur('ENOENT', chemin);
        if (estDossier && !x.dossier) throw erreur('ENOTDIR', chemin);
        if (!estDossier && x.dossier) throw erreur('EISDIR', chemin);
        if (estDossier && (await requete(e.index('parent').count(cle(a))))) throw erreur('ENOTEMPTY', chemin);
        await requete(e.delete(cle(a)));
        if (!estDossier) await requete(c.delete(cle(a)));
      }),
      creerDossier: (a, chemin) => faire('readwrite', async (e) => {
        if (!a.length || await lireEntree(e, a)) throw erreur('EEXIST', chemin);
        await exigerDossier(e, a.slice(0, -1), chemin);
        await requete(e.put({ dossier: true, parent: cle(a.slice(0, -1)), taille: 0, date: Date.now() }, cle(a)));
      }),
      lister: (a, chemin) => faire('readonly', async (e) => {
        const x = await lireEntree(e, a);
        if (!x) throw erreur('ENOENT', chemin);
        if (!x.dossier) throw erreur('ENOTDIR', chemin);
        const prefixe = a.length ? `${cle(a)}/` : '/';
        const [cles, valeurs] = await Promise.all([requete(e.index('parent').getAllKeys(cle(a))), requete(e.index('parent').getAll(cle(a)))]);
        return cles.map((k, i) => [String(k).slice(prefixe.length), valeurs[i].dossier]);
      }),
      etat: (a, chemin) => faire('readonly', async (e) => {
        const x = await lireEntree(e, a);
        if (!x) throw erreur('ENOENT', chemin);
        return stats(x.dossier, x.taille, x.date);
      }),
      toucher: (a, chemin) => faire('readwrite', async (e) => {
        const x = await lireEntree(e, a);
        if (!x) throw erreur('ENOENT', chemin);
        if (a.length) await requete(e.put({ ...x, date: Date.now() }, cle(a)));
      }),
      // (the entry and everything under it, under the new path, at once)
      deplacer: (de, vers, chemin) => faire('readwrite', async (e, c) => {
        const x = await lireEntree(e, de);
        if (!x || !de.length) throw erreur('ENOENT', chemin);
        await exigerDossier(e, vers.slice(0, -1), chemin);
        const la = await lireEntree(e, vers);
        if (la && la.dossier) throw erreur('EISDIR', chemin);
        const [ancien, nouveau] = [cle(de), cle(vers)];
        const sous = x.dossier ? await requete(e.getAllKeys(IDBKeyRange.bound(`${ancien}/`, `${ancien}/￿`))) : [];
        for (const k of [ancien, ...sous.map(String)]) {
          const v = k === ancien ? x : await requete(e.get(k));
          const n = `${nouveau}${k.slice(ancien.length)}`;
          await requete(e.put({ ...v, parent: n.replace(/\/[^/]*$/, '') || '/' }, n));
          await requete(e.delete(k));
          if (!v.dossier) {
            await requete(c.put(await requete(c.get(k)), n));
            await requete(c.delete(k));
          }
        }
      }),
    };
    return systeme('IndexedDB', s);
  });
}

// The file system (memoized): OPFS, else IndexedDB. The browser is asked to keep it even when the
// disk fills up, when it allows it (Firefox asks; Chrome decides by itself).
let promesse = null;
export function systemeFichiers() {
  promesse ||= opfs().catch(() => indexedDb()).then((fs) => {
    if (navigator.storage && navigator.storage.persist) navigator.storage.persisted().then((p) => p || navigator.storage.persist()).catch(() => {});
    return fs;
  });
  return promesse;
}

// Whether the browser keeps this storage for good (null: it doesn't say)
export async function stockagePersistant() {
  try { return navigator.storage && navigator.storage.persisted ? await navigator.storage.persisted() : null; } catch { return null; }
}
