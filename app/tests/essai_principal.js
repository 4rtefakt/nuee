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
//   saying why; Nuée's own R, as slow, never stopped;
// - the last session's files given to the window without a look at them, then looked at when they
//   reopen, without holding the main process: a network share that doesn't answer looked at once,
//   given up after its time (shortened here), its files said out of reach;
// - closing Nuée with unsaved projects ("Save and close"), asked twice (a second ×, the update): one
//   question; the windows hidden while the projects' last versions go, then Nuée quits once, what the
//   update runs just before; Cancel closes nothing, and the next request asks again;
// - the organisation's policies: R code off when they say so and when they couldn't be read, then
//   given so to the engine at start-up, read before it starts; NUEE_GREFFONS_ORGANISATION not taken
//   by an installed copy on Windows; Help › Diagnostic information says whether they were read.
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
  process.env.NUEE_ESSAI_ENVIRONNEMENT = 'celui de l’ordinateur';  // (one of the computer's variables)
  await demander('r-installer-greffon', ['unmarked', 'pas un paquet']);
  const a = appels.shift();
  verifier(!!a && a.chemin === path.join(dossier, 'R', 'bin', 'Rscript') && egal(a.paquets, ['unmarked']) && a.env.RHOME === path.join(dossier, 'R')
    && a.env.R_LIBS === (windows || process.platform === 'darwin' ? path.join(dossier, 'R', 'library') : process.env.R_LIBS)
    && a.env.NUEE_ESSAI_ENVIRONNEMENT === process.env.NUEE_ESSAI_ENVIRONNEMENT,
  'le R de Nuée (celui de Nuée.app sur macOS) : RHOME et R_LIBS, comme le moteur, avec l’environnement de l’ordinateur', a && { ...a, env: { RHOME: a.env.RHOME, R_LIBS: a.env.R_LIBS } });
  delete process.env.NUEE_ESSAI_ENVIRONNEMENT;
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

// The processes main.js starts, recorded, and what they were given (options: their environment)
function espionProcessus() {
  const enfants = [];
  const options = [];
  return { enfants, options, module: { ...childProcess, spawn: (...a) => { const c = childProcess.spawn(...a); enfants.push(c); options.push(a[2] || {}); return c; } } };
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

// The last session's files: given without a look; looked at when they reopen, a share that doesn't
// answer once, and given up after its time
async function derniereSeance(dossier) {
  console.log('La dernière séance : ses fichiers regardés sans attendre');
  const partage = windows ? '\\\\serveur-absent\\partage' : '/serveur-absent/partage';
  const surLePartage = (f) => String(f).toLowerCase().startsWith(partage);
  const regards = { synchrones: 0, partage: 0, enMeme: 0, auPlus: 0 };
  const espion = { ...fs,
    existsSync: (f) => { if (surLePartage(f)) regards.synchrones += 1; return fs.existsSync(f); },
    statSync: (f, o) => { if (surLePartage(f)) regards.synchrones += 1; return fs.statSync(f, o); },
    promises: { ...fs.promises,
      // (a share out of reach: a look that never ends)
      stat: (f, o) => {
        if (!surLePartage(f)) return fs.promises.stat(f, o);
        regards.partage += 1;
        regards.enMeme += 1;
        regards.auPlus = Math.max(regards.auPlus, regards.enMeme);
        return new Promise(() => {});
      } } };
  const m = chargerMain(dossier, { fs: espion }, [['const DELAI_PRESENCE = 5000;', 'const DELAI_PRESENCE = 500;']]);
  const ici = path.join(dossier, 'seance');
  fs.mkdirSync(ici, { recursive: true });
  const present = path.join(ici, 'comptages.nuee');
  const autre = path.join(ici, 'autre.nuee');
  for (const f of [present, autre]) fs.writeFileSync(f, '{}');
  const fichiers = [present, path.join(ici, 'transects.nuee'), ...[1, 2, 3].map((i) => path.join(partage, `projet${i}.nuee`)), autre];
  m.evaluer('seancePrecedente = __valeur', fichiers);
  const demander = demandeur(m);
  try {
    const liste = await demander('seance-precedente');
    verifier(egal(liste.map((x) => x.chemin), fichiers) && regards.synchrones === 0 && regards.partage === 0, 'donnés à la fenêtre sans regarder les fichiers', regards);
    const debut = Date.now();
    const presents = await auPlus(Promise.resolve(demander('seance-presents', fichiers)), 10000, 'the look');
    verifier(egal(presents, [true, false, null, null, null, true]) && Date.now() - debut < 3000,
      `regardés en ${Date.now() - debut} ms : présent, supprimé, trois sur un partage qui ne répond pas (null), présent`, presents);
    verifier(regards.partage === 1 && regards.synchrones === 0, 'un seul regard vers ce partage, et sans attendre', regards);
    verifier(egal(await demander('seance-precedente'), []), 'donnés une fois');
  } catch (e) {
    verifier(false, 'regardés sans attendre', e.message);
  }
}

// The organisation's policies as main.js reads them (r-installes.js: lirePolitiques): R code off when
// they say so, and when they couldn't be read; NUEE_GREFFONS_ORGANISATION not taken by an installed
// copy on Windows; Help › Diagnostic information says whether they were read
async function lesPolitiques(dossier) {
  console.log('Les politiques : lues, pas lues');
  if (!process.getSystemVersion) process.getSystemVersion = () => os.release();  // (Electron's, for the diagnostic)
  const variable = path.join(dossier, 'greffons de n’importe qui');
  const avant = process.env.NUEE_GREFFONS_ORGANISATION;
  process.env.NUEE_GREFFONS_ORGANISATION = variable;
  try {
    const p = espionProcessus();
    const m = chargerMain(dossier, { child_process: p.module, './r-installes': { ...require('../r-installes'), lirePolitiques: async () => m.lues } });
    m.ctx.trouverRscript = () => rscriptFactice(path.join(dossier, 'R-nuee'));
    m.electron.app.isPackaged = true;  // (an installed copy)
    const demander = demandeur(m);
    const sansCode = process.env.NUEE_SANS_CODE === '1';
    const etat = async (lues) => {
      m.evaluer('politiques = __valeur; greffonsOrganisation = undefined', lues);
      return { code: m.evaluer('codeInterdit()'), greffons: m.evaluer('dossierGreffonsOrganisation()'), diagnostic: (await demander('diagnostic')).politiques };
    };
    let e = await etat({ valeurs: {}, lues: true, par: 'reg' });
    verifier(e.code === sansCode && e.greffons === (windows ? null : variable) && (windows ? egal(e.diagnostic, { lues: true, par: 'reg', valeurs: {} }) : e.diagnostic === null),
      'lues, aucune : le code R permis ; NUEE_GREFFONS_ORGANISATION ignorée par une copie installée sous Windows, prise ailleurs', e);
    e = await etat({ valeurs: { DisableCustomCode: 1, PluginsFolder: '\\\\serveur\\greffons', RHome: 'D:\\R' }, lues: true, par: 'powershell' });
    verifier(e.code === true && e.greffons === (windows ? '\\\\serveur\\greffons' : variable)
      && (!windows || egal(e.diagnostic, { lues: true, par: 'powershell', valeurs: { DisableCustomCode: 1, RHome: 'set', PluginsFolder: 'set' } })),
    'DisableCustomCode : le code R retiré ; PluginsFolder, le dossier de l’organisation ; le diagnostic les dit, sans les chemins', e);
    e = await etat({ valeurs: {}, lues: false, par: null });
    verifier(e.code === true && (!windows || egal(e.diagnostic, { lues: false, par: null, valeurs: {} })), 'pas lues (reg.exe et PowerShell refusés) : le code R retiré quand même, et le diagnostic le dit', e);
    // what the engine is given at start-up: the policies read first (demarrer); here, that they
    // couldn't be. (The stand-in R, given R's --no-init-file as an installed copy gives it, stops at once.)
    m.lues = { valeurs: {}, lues: false, par: null };
    m.evaluer('politiques = { valeurs: {}, lues: true, par: "reg" }; greffonsOrganisation = undefined');
    m.evaluer('demarrer()').catch(() => {});
    for (let i = 0; i < 100 && !p.options.length; i += 1) await pause(50);
    const env = (p.options[0] || {}).env || {};
    verifier(m.evaluer('politiques.lues') === false && env.NUEE_SANS_CODE === '1' && (windows ? !('NUEE_GREFFONS_ORGANISATION' in env) : env.NUEE_GREFFONS_ORGANISATION === variable),
      'au démarrage, lues d’abord : le moteur reçoit NUEE_SANS_CODE (et sous Windows pas la variable)', { NUEE_SANS_CODE: env.NUEE_SANS_CODE, NUEE_GREFFONS_ORGANISATION: env.NUEE_GREFFONS_ORGANISATION });
    for (let i = 0; i < 100 && p.enfants.length && !fini(p.enfants[0]); i += 1) await pause(50);
  } catch (e) {
    verifier(false, 'les politiques', e.stack || e.message);
  } finally {
    if (avant === undefined) delete process.env.NUEE_GREFFONS_ORGANISATION; else process.env.NUEE_GREFFONS_ORGANISATION = avant;
  }
}

// Closing with unsaved projects, asked twice: one question; nothing to change while the versions go
async function fermerUneFois(dossier) {
  console.log('Fermer Nuée : une question pour deux demandes, les fenêtres cachées ensuite');
  // (the engine's answer: a project with unsaved changes)
  const moteur = require('http').createServer((req, res) => {
    req.resume();
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify(req.url === '/api/projets_modifies' ? { noms: ['vergers'] } : {}));
  });
  await new Promise((ok) => moteur.listen(0, '127.0.0.1', ok));
  const port = moteur.address().port;
  const adresse = `http://127.0.0.1:${port}/`;
  // (the last versions sent when this check says)
  let envoyees = 0;
  let finEnvoi = null;
  const versions = { ...versionsFactices,
    creerVersions: () => ({ fermerTout: () => { envoyees += 1; return new Promise((ok) => { finEnvoi = ok; }); }, auPremierPlan() {}, jeton: async () => null }) };
  const charger = () => {
    const m = chargerMain(dossier, { './versions': versions });
    m.evaluer(`adresse = ${JSON.stringify(adresse)}; port = ${port}`);
    const principaleF = new m.electron.BrowserWindow({});
    principaleF.loadURL(adresse);
    const vue = new m.electron.BrowserWindow({});
    vue.loadURL(`${adresse}?vue=scores&modele=M1`);
    return { m, principaleF, vue };
  };
  const attendre = async (f, quoi) => { for (let i = 0; i < 100 && !f(); i += 1) await pause(50); if (!f()) throw new Error(`waited for ${quoi}`); };
  try {
    const { m, principaleF, vue } = charger();
    const ordre = [];
    m.electron.app.quit = () => { ordre.push('quitter'); m.E.quitte += 1; };
    m.E.reponses.push(0);  // Save and close
    const a = m.evaluer('fermerNuee(__valeur)', principaleF);
    // (asked again while the first waits for the engine: a second ×, then Update with its installer)
    const b = m.evaluer('fermerNuee(__valeur)', principaleF);
    const c = m.evaluer('fermerNuee(__valeur.f, __valeur.apres)', { f: principaleF, apres: () => ordre.push('installateur') });
    await attendre(() => envoyees > 0, 'the last versions');
    verifier(m.E.dialogues.length === 1 && envoyees === 1, 'une seule question pour trois demandes', m.E.dialogues);
    verifier(principaleF.cachee && vue.cachee && m.E.quitte === 0, 'les fenêtres cachées pendant que les dernières versions partent, avant de quitter',
      { cachees: [principaleF.cachee, vue.cachee], quitte: m.E.quitte });
    finEnvoi();
    const r = await auPlus(Promise.all([a, b, c]), 5000, 'the close');
    verifier(egal(r, [true, true, true]) && egal(ordre, ['installateur', 'quitter']), 'puis Nuée quitte, une fois, l’installateur de la mise à jour lancé juste avant',
      { r, ordre });
  } catch (e) {
    verifier(false, 'une seule question, les fenêtres cachées', e.message);
  }
  try {
    const { m, principaleF } = charger();
    m.E.reponses.push(2);  // Cancel
    const r = await auPlus(m.evaluer('fermerNuee(__valeur)', principaleF), 5000, 'the close');
    verifier(r === false && !principaleF.cachee && m.E.quitte === 0 && envoyees === 1, 'Annuler : rien ne se ferme', { r, cachee: principaleF.cachee });
    m.E.reponses.push(1);  // Close without saving
    const fin = m.evaluer('fermerNuee(__valeur)', principaleF);
    await attendre(() => envoyees === 2, 'the last versions');
    finEnvoi();
    verifier((await auPlus(fin, 5000, 'the close')) === true && m.E.dialogues.length === 2 && m.E.quitte === 1, 'redemandé ensuite : la question de nouveau, puis Nuée quitte',
      m.E.dialogues);
  } catch (e) {
    verifier(false, 'Annuler, puis redemandé', e.message);
  }
  moteur.close();
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
    await derniereSeance(dossier);
    await fermerUneFois(dossier);
    await lesPolitiques(dossier);
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
