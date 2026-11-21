// The R engine in the page, for the browser version (index.html sets window.NUEE_MOTEUR =
// 'webr'; api.js loads this module at the first request). webR, R compiled to WebAssembly, runs
// the engine's own files (moteur/), and each request goes to traiter_demande() as the desktop
// app's local server sends it there (moteur/serveur.R): the same code, the same numbers.
// Files the user opens are written into webR's file system; files the engine writes (a project
// saved, an export) are downloaded from it. Nothing leaves the page.
import { langueActuelle } from './i18n.js';
import { PAQUETS_WEBR, SANS_WEBR } from './paquets_webr.js';
import { magasin } from './secours_navigateur.js';
import { VERSION } from './nouveautes.js';

const TEXTES = {
  fr: { r: 'Démarrage de R dans le navigateur…', fichiers: 'Fichiers du moteur…', paquet: 'Paquets R : {p} ({i}/{n})', moteur: 'Chargement du moteur…',
    note: 'Nuée calcule dans cet onglet : vos données ne le quittent pas. La première visite charge R et ses paquets ; les suivantes sont plus rapides.',
    echec: 'R n’a pas pu démarrer dans ce navigateur : {e}' },
  en: { r: 'Starting R in the browser…', fichiers: 'Engine files…', paquet: 'R packages: {p} ({i}/{n})', moteur: 'Loading the engine…',
    note: 'Nuée computes in this tab: your data never leaves it. The first visit loads R and its packages; the next ones are quicker.',
    echec: 'R couldn’t start in this browser: {e}' },
};
const texte = (cle, v = {}) => (TEXTES[langueActuelle()] || TEXTES.fr)[cle].replace(/\{(\w+)\}/g, (_, k) => v[k]);

// A veil over the interface while R starts, with what it is doing.
function voile() {
  const fond = document.createElement('div');
  fond.className = 'voile-webr';
  fond.style.cssText = 'position:fixed;inset:0;z-index:100000;display:flex;align-items:center;justify-content:center;'
    + 'background:color-mix(in srgb, Canvas 88%, transparent);color:CanvasText;font:15px/1.5 system-ui,sans-serif;backdrop-filter:blur(2px)';
  const boite = document.createElement('div');
  boite.style.cssText = 'max-width:420px;padding:24px 28px;border-radius:10px;background:Canvas;box-shadow:0 8px 30px rgba(0,0,0,.18);border:1px solid color-mix(in srgb, CanvasText 15%, transparent)';
  const titre = document.createElement('div');
  titre.style.cssText = 'font-weight:600;margin-bottom:10px';
  const barre = document.createElement('div');
  barre.style.cssText = 'height:6px;border-radius:3px;background:color-mix(in srgb, CanvasText 12%, transparent);overflow:hidden';
  const rempli = document.createElement('div');
  rempli.style.cssText = 'height:100%;width:0;background:#2b6cb0;transition:width .3s';
  barre.append(rempli);
  const note = document.createElement('p');
  note.style.cssText = 'margin:12px 0 0;font-size:13px;opacity:.75';
  note.textContent = texte('note');
  boite.append(titre, barre, note);
  fond.append(boite);
  document.body.append(fond);
  return {
    dire(t, part) { titre.textContent = t; if (part !== undefined) rempli.style.width = `${Math.round(part * 100)}%`; },
    retirer() { fond.remove(); },
  };
}

// webR's file system: folders made along the way
async function creerDossiers(webR, chemin) {
  const parties = chemin.split('/').filter(Boolean);
  for (let i = 1; i <= parties.length; i += 1) {
    try { await webR.FS.mkdir(`/${parties.slice(0, i).join('/')}`); } catch { /* already there */ }
  }
}
async function ecrireFichier(webR, chemin, octets) {
  await creerDossiers(webR, chemin.split('/').slice(0, -1).join('/'));
  await webR.FS.writeFile(chemin, octets);
}

// The plugins installed in the page (File › Plugins…: vues_greffons.js) are in R's folder
// NUEE_GREFFONS (moteur/R/greffons.R), kept in the browser's storage after each install or removal
// and put back at each visit (secours_navigateur.js: magasin, a file for each of theirs); the R code
// and the plugins approved here (vues_code.js: approuver) are given back to the engine at each visit,
// as the desktop app keeps them in its settings (main.js: reglages.codeApprouve, the latest 10,000).
const GREFFONS = '/greffons';
const MAGASIN_GREFFONS = 'nuee-greffons';
const APPROUVES = 'nuee-code-approuve';
const approuves = () => {
  try { return [].concat(JSON.parse(localStorage.getItem(APPROUVES) || '[]')).filter((x) => /^[0-9a-f]{64}$/.test(x)); } catch { return []; }
};
async function rendreGreffons(webR) {
  const s = await magasin(MAGASIN_GREFFONS);
  for (const nom of await s.noms()) await ecrireFichier(webR, `${GREFFONS}/${decodeURIComponent(nom)}`, await s.lire(nom));
}
async function garderGreffons(webR) {
  const s = await magasin(MAGASIN_GREFFONS);
  const fichiers = [].concat(await webR.evalRRaw(`list.files("${GREFFONS}", recursive = TRUE)`, 'string[]'));
  const gardes = new Set(fichiers.map((f) => encodeURIComponent(f)));
  for (const f of fichiers) await s.ecrire(encodeURIComponent(f), await webR.FS.readFile(`${GREFFONS}/${f}`));
  for (const nom of await s.noms()) if (!gardes.has(nom)) await s.supprimer(nom);
}

// The packages to install: PAQUETS_WEBR, less those of ?sans=a,b (to see the browser version
// without them: their features say they need the desktop application)
const sansPaquets = () => (new URLSearchParams(location.search).get('sans') || '').split(',').map((x) => x.trim()).filter(Boolean);

// Where they come from: the site's own copy of webR's repository when it has one (navigateur.js:
// window.NUEE_MIROIR_WEBR, written by miroir_webr.mjs, as on nuee.io), so that a first visit asks
// nothing of another site; webR's repository for what that copy lacks, such as an R-code model's own
// package (navigateur.R: installer_paquets_navigateur reads the repositories from webr_pkg_repos).
// { depots: the repositories in that order, miroir: the packages of the site's copy }
const DEPOT_WEBR = 'https://repo.r-wasm.org';
async function depotsPaquets(webR) {
  const sans = { depots: [DEPOT_WEBR], miroir: new Set() };
  if (!window.NUEE_MIROIR_WEBR) return sans;
  const miroir = new URL(window.NUEE_MIROIR_WEBR, location.href).href.replace(/\/$/, '');
  const index = await fetch(`${miroir}/bin/emscripten/contrib/${webR.versionR.replace(/\.\d+$/, '')}/PACKAGES`).catch(() => null);
  if (!index || !index.ok) return sans;
  return { depots: [miroir, DEPOT_WEBR], miroir: new Set([...(await index.text()).matchAll(/^Package:\s*(\S+)/gm)].map((m) => m[1])) };
}

// One webR with the engine. The main one answers the interface's requests: every package, and
// the request layer (serveur.R). The others (`aide`) take pieces of long requests: the engine
// alone, no package (what they compute needs none: TRAVAUX in serveur.R).
let fichiersMoteur = null;
async function nouveauR(progres = () => {}, aide = false) {
  const { WebR } = await import('./webr/webr.mjs');
  const webR = new WebR({ baseUrl: new URL('./webr/', location.href).href, interactive: false });
  progres(texte('r'), 0.02);
  await webR.init();
  // what R prints outside of a request (warnings...) goes to the console, not into a queue
  (async () => { for (;;) { const m = await webR.read(); if (m.type === 'stderr') console.warn('[R]', m.data); if (m.type === 'closed') break; } })();
  progres(texte('fichiers'), 0.08);
  fichiersMoteur ||= (async () => {
    const liste = await (await fetch('./moteur-fichiers.json')).json();
    return Promise.all(liste.map(async (f) => [f, new Uint8Array(await (await fetch(`./${encodeURI(f)}`)).arrayBuffer())]));
  })();
  for (const [f, octets] of await fichiersMoteur) await ecrireFichier(webR, `/nuee/${f}`, octets);
  if (aide) {
    await webR.evalRVoid('setwd("/nuee"); source("moteur/charger.R", encoding = "UTF-8")');
    return { webR, absents: [] };
  }
  // each package on its own: one webR can't install leaves only its features out; from the site's
  // copy alone when it has the package (what it depends on is there too)
  const sans = sansPaquets();
  const absents = [...Object.keys(SANS_WEBR)];
  const paquets = PAQUETS_WEBR.filter((p) => !sans.includes(p));
  absents.push(...PAQUETS_WEBR.filter((p) => sans.includes(p)));
  const { depots, miroir } = await depotsPaquets(webR);
  if (miroir.size) await webR.evalRVoid('options(webr_pkg_repos = depots)', { env: { depots } });
  for (const [i, p] of paquets.entries()) {
    progres(texte('paquet', { p, i: i + 1, n: paquets.length }), 0.1 + 0.85 * (i / paquets.length));
    try {
      await webR.installPackages([p], { quiet: true, repos: miroir.has(p) ? depots.slice(0, 1) : depots });
    } catch (e) {
      console.warn(`Nuée : le paquet R ${p} n'a pas pu être installé`, e);
      absents.push(p);
    }
  }
  progres(texte('moteur'), 0.97);
  try { await rendreGreffons(webR); } catch (e) { console.warn('Nuée : les greffons installés n’ont pas pu être lus', e); }
  await creerDossiers(webR, GREFFONS);
  // (navigateur.R: what needs a package left out says it needs the desktop application; a model's
  // packages webR can't load are never installed). The engine's request layer, serveur.R, read up
  // to its HTTP server, as the engine's checks read it (moteur/tests/verifier_etapes.R): each
  // request goes to traiter_demande() from here.
  await webR.evalRVoid(`options(nuee.navigateur = TRUE, nuee.paquets_absents = absents[nzchar(absents)], nuee.paquets_sans_webr = sans_webr)
    Sys.setenv(NUEE_GREFFONS = greffons, NUEE_VERSION = version)
    setwd("/nuee")
    for (e in parse("moteur/serveur.R", encoding = "UTF-8", keep.source = FALSE)) {
      if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("appli"))) break
      eval(e, globalenv())
    }
    rm(e)`, { env: { absents: absents.length ? absents : [''], sans_webr: Object.keys(SANS_WEBR), greffons: GREFFONS, version: VERSION } });
  const empreintes = approuves();
  if (empreintes.length) await webR.evalRVoid('approuver_codes(empreintes)', { env: { empreintes } });
  return { webR, absents };
}

// The requests that can be shared out (TRAVAUX in serveur.R), from how many pieces; and how many
// other R: one per core left, 4 at most (each holds its own copy of the data), or ?aides=N.
const PARTAGEABLES = { permutations: (corps) => corps.n ?? 20 };
const SEUIL_PARTAGE = 8;
const nAides = () => {
  const v = new URLSearchParams(location.search).get('aides');
  if (v !== null && v !== '') return Math.max(0, Math.min(15, Number(v) || 0));
  return Math.max(0, Math.min(4, (navigator.hardwareConcurrency || 2) - 1));
};

// a request in the R `r`, as the local server answers it: { statut, json }
async function traiter(r, fonction, env) {
  const [statut, json] = await r.evalRRaw(`local({ r <- ${fonction}; c(as.character(r$status), r$body) })`, 'string[]', { env });
  return { statut: Number(statut), json: JSON.parse(json) };
}

export async function demarrer() {
  const v = voile();
  let principal;
  try {
    principal = await nouveauR((t, part) => v.dire(t, part));
  } catch (e) {
    v.dire(texte('echec', { e: e.message }));
    throw e;
  }
  v.retirer();
  const { webR, absents } = principal;
  // the other R, started once the interface is up (null: none, or they failed to start)
  let aides = null;
  if (nAides() > 0) {
    aides = new Promise((ok) => setTimeout(ok, 2000))
      .then(() => Promise.all(Array.from({ length: nAides() }, () => nouveauR(() => {}, true).then((x) => x.webR))))
      .catch((e) => { console.warn('Nuée : calcul partagé indisponible', e); return null; });
  }
  // a long request shared out: prepared here, its pieces in the other R and here, put together here
  async function partager(nom, corps, langue, autres) {
    const env = { nom, corps: JSON.stringify(corps ?? {}), langue };
    const prep = await traiter(webR, 'travail_preparer(nom, charToRaw(enc2utf8(corps)), langue)', env);
    if (prep.statut !== 200) return prep;
    const { fichier, n } = prep.json;
    const octets = await webR.FS.readFile(fichier);
    const tous = [webR, ...autres].slice(0, n);
    const morceaux = tous.map((r, i) => [Math.floor((i * n) / tous.length) + 1, Math.floor(((i + 1) * n) / tous.length)]);
    // (Stop interrupts each R: its piece comes back "arrete", and so does the request, nothing
    // changed; an interruption from before, passed first: navigateur.R, passer_interruption)
    const morceau = (debut, fin, f, s) => `local({ passer_interruption(); tryCatch({ travail_morceau("${f}", ${debut}, ${fin}, "${s}"); "fait" }, interrupt = function(c) "arrete") })`;
    partage = tous.filter((r) => r !== webR);
    arretPartage = false;
    const sorties = await Promise.all(tous.map(async (r, i) => {
      const [debut, fin] = morceaux[i];
      const ici = `/tmp/morceau-${i + 1}.rds`;
      if (r === webR) return (await webR.evalRString(morceau(debut, fin, fichier, ici))) === 'fait' ? ici : null;
      await r.FS.writeFile('/tmp/travail.rds', octets);
      if ((await r.evalRString(morceau(debut, fin, '/tmp/travail.rds', '/tmp/morceau.rds'))) !== 'fait') return null;
      await webR.FS.writeFile(ici, await r.FS.readFile('/tmp/morceau.rds'));
      return ici;
    })).finally(() => { partage = null; });
    // (also when Stop came between two of its steps, no R computing then)
    if (arretPartage || sorties.includes(null)) return { statut: 400, json: { erreur: langue === 'en' ? 'Computation stopped.' : 'Calcul arrêté.', arrete: true } };
    return traiter(webR, 'travail_finir(nom, charToRaw(enc2utf8(corps)), langue, fichier, sorties)', { ...env, fichier, sorties });
  }
  let depot = 0;
  let file = Promise.resolve();  // (one shared request at a time: the other R hold one task each)
  let enCours = 0;  // (requests R is computing or about to: Stop interrupts it only then)
  let partage = null;  // (the other R of the shared request under way)
  let arretPartage = false;  // (and whether Stop came meanwhile)
  return {
    // a request, as the local server answers it: { statut, json }
    async demander(nom, corps, langue) {
      const autres = PARTAGEABLES[nom] && PARTAGEABLES[nom](corps ?? {}) >= SEUIL_PARTAGE && aides ? await aides : null;
      enCours += 1;
      let r;
      try {
        if (autres && autres.length) {
          const p = file.then(() => partager(nom, corps, langue, autres));
          file = p.catch(() => {});
          return await p;
        }
        r = await traiter(webR, 'traiter_demande(nom, charToRaw(enc2utf8(corps)), langue)', { nom, corps: JSON.stringify(corps ?? {}), langue });
      } finally { enCours -= 1; }
      // (a plugin installed or removed: the plugins' folder kept in the browser's storage)
      if (r.statut === 200 && (nom === 'greffon_installer' || nom === 'greffon_retirer')) {
        await garderGreffons(webR).catch((e) => console.warn('Nuée : les greffons installés n’ont pas pu être gardés', e));
      }
      return r;
    },
    // Stop (app.js: arreterCalcul; etapes.js): R interrupted where it computes, and the other R of a
    // shared request (navigateur.R: calcul_arretable); nothing when no request is under way
    arreter() {
      if (!enCours) return;
      webR.interrupt();
      if (partage) arretPartage = true;
      for (const r of partage || []) r.interrupt();
    },
    // the packages the browser version doesn't have (SANS_WEBR, ?sans=, failed installs)
    absents: () => absents,
    // a plugin's packages installed from webR's repository (navigateur.R:
    // installer_paquets_navigateur): those still missing
    async installerPaquets(paquets) {
      return [].concat(await webR.evalRRaw('installer_paquets_navigateur(paquets)', 'string[]', { env: { paquets: paquets.length ? paquets : [''] } }));
    },
    // R code or plugins approved in the page: remembered for the next visits (the latest 10,000)
    garderApprobations(empreintes) {
      const liste = [...new Set([...approuves(), ...empreintes])].slice(-10000);
      try { localStorage.setItem(APPROUVES, JSON.stringify(liste)); } catch { /* only for this visit */ }
    },
    // (for measuring: the main R, the others, a shareable request with k of them, 0: alone)
    r: () => webR,
    aides: () => aides,
    async enParallele(nom, corps, langue, k) {
      const autres = k > 0 ? ((await aides) || []).slice(0, k) : [];
      if (!autres.length) return traiter(webR, 'traiter_demande(nom, charToRaw(enc2utf8(corps)), langue)', { nom, corps: JSON.stringify(corps ?? {}), langue });
      return partager(nom, corps, langue, autres);
    },
    // a file chosen in the page, written into webR's file system: its path there; several, in
    // one folder
    async deposer(fichier) {
      return (await this.deposerEnsemble([fichier]))[0];
    },
    async deposerEnsemble(fichiers) {
      depot += 1;
      return Promise.all(fichiers.map(async (fichier) => {
        const chemin = `/depots/${depot}/${fichier.name.replace(/[/\\]/g, '_')}`;
        await ecrireFichier(webR, chemin, new Uint8Array(await fichier.arrayBuffer()));
        return chemin;
      }));
    },
    // webR's files: those the engine wrote (an export, a recovery copy), and back (secours_navigateur.js,
    // versions_navigateur.js)
    lireFichier: (chemin) => webR.FS.readFile(chemin),
    existeFichier: async (chemin) => (await webR.FS.analyzePath(chemin)).exists === true,
    ecrireFichier: (chemin, octets) => ecrireFichier(webR, chemin, octets),
    supprimerFichier: (chemin) => webR.FS.unlink(chemin).catch(() => {}),
    creerDossier: (chemin) => creerDossiers(webR, chemin),
    // a file the engine wrote, downloaded under its name
    async telecharger(chemin) {
      const octets = await webR.FS.readFile(chemin);
      const a = document.createElement('a');
      a.href = URL.createObjectURL(new Blob([octets]));
      a.download = chemin.split('/').pop();
      document.body.append(a);
      a.click();
      a.remove();
      setTimeout(() => URL.revokeObjectURL(a.href), 5000);
    },
  };
}
