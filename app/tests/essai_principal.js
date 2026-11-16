// Checks main.js, the desktop app's main process, without Electron and without a window: main.js runs
// in a context of its own (vm) with a stand-in for Electron (its app, windows, dialogs, IPC and
// session, recorded; nothing is shown) and for R (Node under an Rscript's name, running a stand-in
// moteur/serveur.R: ready at once), then its functions are driven directly:
// - the engine restarted after R ended from a signal (R raises its SIGSEGV again, the OOM killer
//   sends SIGKILL; Node's kill() on Windows): no exit code, its signal only, and Restart doesn't wait
//   for an end that already came;
// - a plugin's packages installed into the engine's R as the engine runs it (RHOME, R_LIBS), another
//   R with its own environment only;
// - the settings written beside, then put in their place: a write cut short (a full disk) leaves them
//   as they were; a window's size and place written once it stops moving, and when it closes;
// - another R that is never ready stopped after its time (shortened here), Nuée's own taking over and
//   saying why; Nuée's own R, as slow, never stopped.
//   node tests/essai_principal.js [main.js]
// (a main.js given: that one checked instead, another version's, the app's modules still these)
const childProcess = require('child_process');
const EventEmitter = require('events');
const fs = require('fs');
const os = require('os');
const path = require('path');
const vm = require('vm');
const { createRequire } = require('module');

const APP = path.join(__dirname, '..');
const MAIN = path.resolve(process.argv[2] || path.join(APP, 'main.js'));
const windows = process.platform === 'win32';

let echecs = 0;
function verifier(ok, nom, detail) {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${!ok && detail !== undefined ? ` : ${typeof detail === 'string' ? detail : JSON.stringify(detail)}` : ''}`);
  if (!ok) echecs += 1;
  return ok;
}
// a promise, or its failure after `ms` (a wait that never ends fails the check instead of the run)
const auPlus = (p, ms, quoi) => {
  let minuterie;
  return Promise.race([p, new Promise((_, ko) => { minuterie = setTimeout(() => ko(new Error(`${quoi}: still waiting after ${ms / 1000} s`)), ms); })])
    .finally(() => clearTimeout(minuterie));
};
const pause = (ms) => new Promise((ok) => setTimeout(ok, ms));
const egal = (a, b) => JSON.stringify(a) === JSON.stringify(b);

// ------------------------------------------------------------ stand-ins

// Electron as main.js uses it; E records what it did (dialogs asked, windows, IPC handlers, quits)
function electronFactice(dossier) {
  const E = { dialogues: [], reponses: [], erreurs: [], quitte: 0, gestionnaires: new Map(), fenetres: [] };
  const app = Object.assign(new EventEmitter(), {
    isPackaged: false,
    commandLine: { hasSwitch: (s) => s === 'user-data-dir' },
    getPath: (n) => ({ userData: path.join(dossier, 'profil'), appData: dossier, temp: path.join(dossier, 'temp') })[n],
    setPath() {},
    getVersion: () => '1.1.0',
    getLocale: () => 'en-US',
    requestSingleInstanceLock: () => true,
    whenReady: () => new Promise(() => {}),  // (demarrer() isn't run: each check calls what it needs)
    quit: () => { E.quitte += 1; },
    addRecentDocument() {},
  });
  const ipcMain = Object.assign(new EventEmitter(), { handle: (canal, f) => E.gestionnaires.set(canal, f) });
  class BrowserWindow extends EventEmitter {
    constructor(o = {}) {
      super();
      this.url = '';
      this.cachee = false;
      this.detruite = false;
      this.bornes = { x: o.x ?? 10, y: o.y ?? 10, width: o.width, height: o.height };
      this.webContents = Object.assign(new EventEmitter(), { getURL: () => this.url, send: (canal) => this.recu(canal), isDestroyed: () => this.detruite,
        setWindowOpenHandler() {} });
      E.fenetres.push(this);
    }

    static getAllWindows() { return E.fenetres.filter((f) => !f.detruite); }
    static fromWebContents(w) { return E.fenetres.find((f) => f.webContents === w) || null; }
    static getFocusedWindow() { return null; }
    loadURL(u) { this.url = u; }
    hide() { this.cachee = true; }
    show() { this.cachee = false; }
    isDestroyed() { return this.detruite; }
    getNormalBounds() { return { ...this.bornes }; }
    isMaximized() { return false; }
    maximize() {}
    // what the window's page answers (preload.js): 'vider' at once; 'enregistrer-tout' saved
    recu(canal) {
      if (canal === 'vider') setImmediate(() => ipcMain.emit('vide', { sender: this.webContents }));
      if (canal === 'enregistrer-tout') setImmediate(() => ipcMain.emit('enregistrement-fini', { sender: this.webContents }, true));
    }
  }
  const electron = {
    app, ipcMain, BrowserWindow,
    Menu: { setApplicationMenu() {}, buildFromTemplate: () => ({ popup() {} }) },
    clipboard: { readText: () => '' },
    dialog: {
      showMessageBoxSync: (_, o) => { E.dialogues.push(o.message); return E.reponses.length ? E.reponses.shift() : o.cancelId; },
      showErrorBox: (_, contenu) => { E.erreurs.push(contenu); },
    },
    net: { fetch: async () => { throw new Error('no network in this check'); } },
    safeStorage: {},
    session: { defaultSession: { clearCache: async () => {}, getSpellCheckerLanguages: () => [], availableSpellCheckerLanguages: [], setSpellCheckerLanguages() {},
      cookies: { set: async () => {} } } },
    shell: { openExternal() {}, openPath: async () => '' },
    screen: { getAllDisplays: () => [{ bounds: { x: 0, y: 0, width: 1920, height: 1080 } }] },
  };
  return { electron, E };
}

// The versions and the sign-ins (versions.js, comptes.js), which these checks leave aside
const versionsFactices = {
  magasinJetons: () => ({}),
  creerVersions: () => ({ fermerTout: async () => {}, auPremierPlan() {}, jeton: async () => null }),
};
const comptesFactices = { creerComptes: () => ({ preparer() {}, annulerConnexion: async () => {} }) };

// main.js run in a context of its own: `dossier/app` as its folder, so that the engine it starts is
// `dossier/moteur/serveur.R`, the stand-in below. `modules`: others in place of the app's; `remplacer`:
// [text, replacement] of main.js (a time limit shortened), each found once or the check stops there
function chargerMain(dossier, modules = {}, remplacer = []) {
  const { electron, E } = electronFactice(dossier);
  const requireApp = createRequire(path.join(APP, 'main.js'));
  const tous = { electron, './zotero': { demanderZotero: async () => null }, './versions': versionsFactices, './comptes': comptesFactices, ...modules };
  const ctx = vm.createContext({
    require: (n) => (n in tous ? tous[n] : requireApp(n)), process, console, Buffer, URL, setTimeout, clearTimeout, setInterval, clearInterval,
    setImmediate, clearImmediate, queueMicrotask, structuredClone, __dirname: path.join(dossier, 'app'), __filename: MAIN, module: { exports: {} }, exports: {},
  });
  let source = fs.readFileSync(MAIN, 'utf8');
  for (const [avant, apres] of remplacer) {
    if (source.split(avant).length !== 2) throw new Error(`main.js doesn't have this once: ${avant}`);
    source = source.replace(avant, () => apres);
  }
  vm.runInContext(source, ctx, { filename: MAIN });
  // main.js's variables (let, const) are read and set by code run in its context; a value goes in through __valeur
  const evaluer = (code, valeur) => { ctx.__valeur = valeur; return vm.runInContext(code, ctx); };
  return { ctx, E, electron, evaluer };
}
// A request from a window of the interface to main.js (ipc(): IPC handlers, only for the interface's pages)
const ADRESSE = 'http://127.0.0.1:9/';
function demandeur(m) {
  m.evaluer(`adresse = ${JSON.stringify(ADRESSE)}; ipc()`);
  const page = Object.assign(new EventEmitter(), { isDestroyed: () => false, send() {} });
  return (canal, ...args) => m.E.gestionnaires.get(canal)({ senderFrame: { url: ADRESSE }, sender: page }, ...args);
}

// The R engine's stand-in: moteur/serveur.R, run by Node started under an Rscript's name; ready at
// once, or never: an Rscript of a folder named "lent" (Windows), or given FAUX_R=lent (its script)
const SERVEUR_FACTICE = String.raw`if (process.env.FAUX_R !== 'lent' && !/[\\/]lent[\\/]/i.test(process.execPath)) console.log('PRET');
setInterval(() => {}, 1000);
`;
// an "Rscript" in `dossier` that is Node (Windows: a hard link to it, or a copy; elsewhere a script)
function rscriptFactice(dossier) {
  fs.mkdirSync(dossier, { recursive: true });
  if (!windows) {
    const f = path.join(dossier, 'Rscript');
    const lent = path.basename(dossier) === 'lent' ? 'FAUX_R=lent ' : '';
    fs.writeFileSync(f, `#!/bin/sh\n${lent}exec '${process.execPath}' "$@"\n`, { mode: 0o755 });
    return f;
  }
  const f = path.join(dossier, 'Rscript.exe');
  try { fs.linkSync(process.execPath, f); } catch { fs.copyFileSync(process.execPath, f); }
  return f;
}

// ------------------------------------------------------------ the checks

// R ended from a signal: Restart starts a new one instead of waiting for an end that already came
async function redemarrageApresUnSignal(dossier) {
  console.log('Redémarrer le moteur après un arrêt par un signal');
  const m = chargerMain(dossier);
  m.ctx.trouverRscript = () => rscriptFactice(path.join(dossier, 'R-nuee'));
  const tue = childProcess.spawn(process.execPath, ['-e', 'setInterval(() => {}, 1000)'], { stdio: 'ignore' });
  await new Promise((ok) => tue.once('spawn', ok));
  const fin = new Promise((ok) => tue.once('exit', ok));
  tue.kill(windows ? 'SIGTERM' : 'SIGKILL');
  await fin;
  verifier(tue.exitCode === null && !!tue.signalCode, `un processus arrêté par un signal : pas de code de sortie, son signal (${tue.signalCode})`, [tue.exitCode, tue.signalCode]);
  m.evaluer('serveur = __valeur', tue);
  try {
    await auPlus(m.evaluer('redemarrerR()'), 20000, 'Restart');
    const nouveau = m.evaluer('serveur');
    verifier(nouveau !== tue && nouveau.exitCode === null && nouveau.signalCode === null, 'Redémarrer : un nouveau moteur, qui tourne');
  } catch (e) {
    verifier(false, 'Redémarrer : un nouveau moteur, qui tourne', e.message);
  } finally {
    // (never the process that ended: its number may be another program's by now)
    if (m.evaluer('serveur') !== tue) m.evaluer('arreterR()');
  }
}

// The packages a plugin needs, installed into the R the engine runs on, run as the engine runs it
async function paquetsDUnGreffon(dossier) {
  console.log('Les paquets d’un greffon, dans le R du moteur');
  const appels = [];
  const m = chargerMain(dossier, { './r-installes': { ...require('../r-installes'),
    installer: async (chemin, paquets, o = {}) => { appels.push({ chemin, paquets, env: o.env || {} }); return { ok: true }; } } });
  const demander = demandeur(m);
  m.electron.app.isPackaged = true;  // (an installed copy: Nuée's own R with its packages)
  m.evaluer("rUtilise = { type: 'nuee', rscript: rscriptMac, chemin: null }");
  await demander('r-installer-greffon', ['unmarked', 'pas un paquet']);
  const a = appels.shift();
  verifier(!!a && a.chemin === path.join(dossier, 'R', 'bin', 'Rscript') && egal(a.paquets, ['unmarked']) && a.env.RHOME === path.join(dossier, 'R')
    && a.env.R_LIBS === (windows || process.platform === 'darwin' ? path.join(dossier, 'R', 'library') : process.env.R_LIBS) && a.env.PATH === process.env.PATH,
  'le R de Nuée (celui de Nuée.app sur macOS) : RHOME et R_LIBS, comme le moteur, avec l’environnement de l’ordinateur', a && { ...a, env: { RHOME: a.env.RHOME, R_LIBS: a.env.R_LIBS } });
  const autre = path.join(dossier, 'R-4.5.2', 'bin', 'Rscript');
  m.evaluer("rUtilise = { type: 'autre', rscript: __valeur, chemin: __valeur }", autre);
  await demander('r-installer-greffon', ['unmarked']);
  const b = appels.shift();
  verifier(!!b && b.chemin === autre && b.env.RHOME === process.env.RHOME && b.env.R_LIBS === process.env.R_LIBS, 'un autre R : l’environnement de l’ordinateur seul',
    b && { ...b, env: { RHOME: b.env.RHOME, R_LIBS: b.env.R_LIBS } });
}

// The settings written in one step; a window's moves written once it stops
async function reglagesEcrits(dossier) {
  console.log('Les réglages : écrits d’un coup, la fenêtre une fois arrêtée');
  const profil = path.join(dossier, 'profil');
  fs.mkdirSync(profil, { recursive: true });
  const fichier = path.join(profil, 'reglages.json');
  // (a full disk: half of what is written goes, then the error; the writes of the settings counted)
  let panne = false;
  let ecritures = 0;
  const espion = { ...fs,
    writeFileSync: (f, d, o) => {
      if (panne && String(f).startsWith(profil)) {
        fs.writeFileSync(f, String(d).slice(0, Math.floor(String(d).length / 2)));
        throw Object.assign(new Error('ENOSPC: no space left on device, write'), { code: 'ENOSPC' });
      }
      if (path.basename(String(f)) === 'reglages.json') ecritures += 1;
      return fs.writeFileSync(f, d, o);
    },
    renameSync: (a, b) => { if (path.basename(String(b)) === 'reglages.json') ecritures += 1; return fs.renameSync(a, b); },
  };
  const m = chargerMain(dossier, { fs: espion });
  m.evaluer("reglages = { ...reglages, port: 47863, rscript: '/opt/R/4.5.2/bin/Rscript', codeApprouve: ['a'.repeat(64)] }; ecrireReglages()");
  panne = true;
  m.evaluer('reglages.port = 50000; ecrireReglages()');
  panne = false;
  let relu = null;
  try { relu = JSON.parse(fs.readFileSync(fichier, 'utf8')); } catch { /* cut */ }
  verifier(!!relu && relu.port === 47863 && relu.rscript === '/opt/R/4.5.2/bin/Rscript' && egal(fs.readdirSync(profil), ['reglages.json']),
    'une écriture interrompue (disque plein) laisse les réglages tels qu’ils étaient, et rien à côté', relu || fs.readdirSync(profil));

  ecritures = 0;
  const fenetre = m.evaluer('fenetrePrincipale()');
  for (let i = 1; i <= 100; i += 1) { fenetre.bornes.x = 10 + i; fenetre.emit('move'); }
  const pendant = ecritures;
  await pause(1500);
  const garde = JSON.parse(fs.readFileSync(fichier, 'utf8')).fenetre || {};
  verifier(pendant === 0 && ecritures === 1 && garde.x === 110 && garde.width === 1400, 'cent pas d’un déplacement : les réglages écrits une fois, quand il s’arrête',
    { pendant, ecritures, garde });
  fenetre.bornes.width = 1234;
  fenetre.emit('resize');
  fenetre.emit('close');
  const fermee = JSON.parse(fs.readFileSync(fichier, 'utf8')).fenetre || {};
  verifier(ecritures === 2 && fermee.width === 1234, 'fermée juste après un changement de taille : celle-ci gardée tout de suite', { ecritures, fermee });
  await pause(1300);
  verifier(ecritures === 2, 'et rien de plus ensuite', ecritures);
}

// The processes main.js starts, recorded
function espionProcessus() {
  const enfants = [];
  return { enfants, module: { ...childProcess, spawn: (...a) => { const c = childProcess.spawn(...a); enfants.push(c); return c; } } };
}
const fini = (p) => p.exitCode !== null || p.signalCode !== null;
const DELAI_COURT = [['const DELAI_AUTRE_R = 90000;', 'const DELAI_AUTRE_R = 1500;']];

// Another R never ready: stopped after its time, Nuée's own R takes over; Nuée's own, never stopped
async function autreRJamaisPret(dossier) {
  console.log('Un autre R jamais prêt');
  const p1 = espionProcessus();
  const m = chargerMain(dossier, { child_process: p1.module }, DELAI_COURT);
  m.ctx.trouverRscript = () => rscriptFactice(path.join(dossier, 'R-nuee'));
  m.evaluer('reglages.rscript = __valeur', rscriptFactice(path.join(dossier, 'R-4.5.2', 'lent')));
  const debut = Date.now();
  try {
    await auPlus(m.evaluer('demarrerMoteur()'), 30000, 'the start');
    const u = m.evaluer('rUtilise');
    const dit = m.evaluer('rRepliASignaler');
    verifier(u.type === 'nuee' && u.repli && u.repli.motif === 'delai' && m.evaluer('rEnMarche()'),
      `pas prêt au bout de 1,5 s : le R de Nuée prend le relais (${((Date.now() - debut) / 1000).toFixed(1)} s)`, u);
    verifier(!!dit && dit.motif === 'delai' && dit.secondes === 1.5 && dit.type === 'autre', 'la fenêtre le dira : pas prêt au bout de 1,5 s', dit);
    verifier(p1.enfants.length === 2 && fini(p1.enfants[0]) && !fini(p1.enfants[1]), 'cet autre R arrêté, celui de Nuée en marche', p1.enfants.map(fini));
  } catch (e) {
    verifier(false, 'pas prêt au bout de 1,5 s : le R de Nuée prend le relais', e.message);
  } finally {
    m.evaluer('arreterR()');
  }
  // Nuée's own R, as slow: it has the time it needs
  const p2 = espionProcessus();
  const m2 = chargerMain(dossier, { child_process: p2.module }, DELAI_COURT);
  m2.ctx.trouverRscript = () => rscriptFactice(path.join(dossier, 'R-nuee', 'lent'));
  m2.evaluer('demarrerMoteur()').catch(() => {});
  await pause(4000);
  verifier(p2.enfants.length === 1 && !fini(p2.enfants[0]) && m2.evaluer('rUtilise') === null, 'le R de Nuée, aussi lent : pas arrêté au bout de 4 s', p2.enfants.map(fini));
  m2.evaluer('arreterR()');
}

async function principal() {
  const dossier = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-principal-'));
  try {
    fs.mkdirSync(path.join(dossier, 'moteur'), { recursive: true });
    fs.mkdirSync(path.join(dossier, 'temp'), { recursive: true });
    fs.writeFileSync(path.join(dossier, 'moteur', 'serveur.R'), SERVEUR_FACTICE);
    await redemarrageApresUnSignal(dossier);
    await paquetsDUnGreffon(dossier);
    await reglagesEcrits(dossier);
    await autreRJamaisPret(dossier);
  } catch (e) {
    verifier(false, 'erreur', e.stack || e.message);
  } finally {
    await pause(300);  // (the stand-in Rs stopped, their files let go: Windows)
    try { fs.rmSync(dossier, { recursive: true, force: true, maxRetries: 5 }); } catch (e) { console.log(`(${dossier} reste : ${e.message})`); }
  }
  console.log(echecs ? `${echecs} vérification(s) en échec` : 'Tout est bon');
  process.exit(echecs ? 1 : 0);
}

principal();
