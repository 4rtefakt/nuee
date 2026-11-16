// Desktop application: starts the R server (moteur/serveur.R) once and shows its interface
// in as many windows as wanted. Windows remember their size; recent files are kept.
const { app, BrowserWindow, Menu, clipboard, dialog, ipcMain, net: reseau, safeStorage, session, shell, screen } = require('electron');
const { spawn, execFileSync } = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const net = require('net');
const path = require('path');
const { fileURLToPath } = require('url');
const { demanderZotero } = require('./zotero');
const { creerVersions, magasinJetons } = require('./versions');
const { creerComptes } = require('./comptes');
const rInstalles = require('./r-installes');

// Packaged: R, the engine and the .usp converter sit in resources/. In development they
// are found in the repository and the per-user R installation.
const racine = app.isPackaged ? process.resourcesPath : path.join(__dirname, '..');
const interfaceDir = path.join(__dirname, 'interface');
const fichierReglages = () => path.join(app.getPath('userData'), 'reglages.json');
const TITRE = 'Nuée';

// Settings, interface preferences and recovery copies in %APPDATA%\Nuee (~/.config/Nuee on
// Linux, ~/Library/Application Support/Nuee on macOS), or the folder given with
// --user-data-dir. Up to version 0.4 the application was
// called "Analyse multivariée": its settings are taken over once.
const profilDonne = app.commandLine.hasSwitch('user-data-dir');
if (!profilDonne) app.setPath('userData', path.join(app.getPath('appData'), 'Nuee'));
const ancienDossier = path.join(app.getPath('appData'), 'analyse-multivariee');
if (!profilDonne && !fs.existsSync(app.getPath('userData')) && fs.existsSync(ancienDossier)) {
  for (const f of ['reglages.json', 'Local Storage', 'secours']) {
    try { fs.cpSync(path.join(ancienDossier, f), path.join(app.getPath('userData'), f), { recursive: true }); } catch { /* not there */ }
  }
}

let reglages = { recents: [], fenetre: null };
let serveur = null;
let journal = '';
let adresse = null;
let port = null;
let quitter = false;
let arretVoulu = false;  // R stopped on purpose (restart), not a crash
// The R server answers only requests carrying this token (moteur/serveur.R): web pages open in
// a browser and other users of the computer reach 127.0.0.1 too. Drawn at each start, given to
// R in its environment and to the windows in a cookie no other site can send.
const jeton = crypto.randomBytes(32).toString('hex');
const cookieR = () => `nuee_${port}`;
// The R code of a project (an R-code model, moteur/R/code.R) runs only once this user has written or
// approved it on this computer: its fingerprint among those kept in the settings (codeApprouve),
// which R is given at each start, and after each approval, with this secret only this process and R
// know (the windows, which have the token, can't approve code themselves). The engine refuses any
// other code, whatever a window asks: opening a file never runs code from it (docs/deploiement.md).
const secretCode = crypto.randomBytes(32).toString('hex');
const APPROUVES_MAX = 10000;  // (the latest approvals: an older code is asked about again)
// Plugins (moteur/R/greffons.R): R code with a manifest, which becomes a model type. This user's are
// installed in the settings' folder (greffons), where the engine writes them (installing one approves
// it, as for R code: approuver-code); an organisation's, offered to everyone without asking, are in a
// folder set by a policy (REG_SZ PluginsFolder under HKLM or HKCU\SOFTWARE\Policies\Nuee, read as RHome
// is: r-installes.js) or, on any system, by the variable NUEE_GREFFONS_ORGANISATION; with
// DisableCustomCode = 1 only those run. The engine is given both folders and Nuée's version (a plugin
// says the oldest Nuée it works with). docs/deploiement.md
const dossierGreffons = () => path.join(app.getPath('userData'), 'greffons');
let greffonsOrganisation;
const dossierGreffonsOrganisation = () => {
  if (greffonsOrganisation === undefined) {
    greffonsOrganisation = (process.platform === 'win32' ? rInstalles.politiqueTexte('PluginsFolder') : null) || process.env.NUEE_GREFFONS_ORGANISATION || null;
  }
  return greffonsOrganisation;
};
let langue = 'en';  // the interface's (English until it says), for the dialogs shown from here
// Recovery copies of the projects not saved yet, written regularly by the interface; they
// are deleted when the application closes normally.
const dossierSecours = () => path.join(app.getPath('userData'), 'secours');
const indexSecours = () => path.join(dossierSecours(), 'index.json');
// A fixed port when free: the interface keeps its settings (theme, language, plot format...)
// per address, so they come back from one launch to the next.
const PORT_HABITUEL = 47863;

function lireReglages() {
  try { reglages = { ...reglages, ...JSON.parse(fs.readFileSync(fichierReglages(), 'utf8')) }; } catch { /* first start */ }
}
// Written beside, then put in its place in one step: a stop meanwhile (a crash, a full disk, the
// computer switched off) leaves the settings as they were, never a cut file, which would read as a
// first start (the R chosen, the approvals, the GitLab application IDs, the session, the port lost).
function ecrireReglages() {
  const fichier = fichierReglages();
  const temporaire = `${fichier}.${process.pid}.tmp`;
  try {
    fs.writeFileSync(temporaire, JSON.stringify(reglages, null, 1));
    fs.renameSync(temporaire, fichier);
  } catch {
    try { fs.rmSync(temporaire, { force: true }); } catch { /* read-only profile */ }
  }
}

// Windows: bin\x64\Rscript.exe is R itself; bin\Rscript.exe only starts it, and stopping that
// one would leave R running. macOS: CRAN's R, brought along in Nuée.app (r-macos.js). Linux: R
// is not brought along, the system's R is used (the .deb depends on it); found on the PATH, or
// where R's installers put it when a launch from the desktop menu gives a short PATH.
const rscriptMac = path.join(racine, 'R', 'bin', 'Rscript');
function trouverRscript() {
  const candidats = process.platform === 'win32' ? [
    process.env.RSCRIPT,
    path.join(racine, 'R', 'bin', 'x64', 'Rscript.exe'),
    path.join(racine, 'R', 'bin', 'Rscript.exe'),
    path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'x64', 'Rscript.exe'),
    path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe'),
  ] : [
    process.env.RSCRIPT,
    ...(process.platform === 'darwin' ? [rscriptMac] : []),
    ...(process.env.PATH || '').split(path.delimiter).filter(Boolean).map((d) => path.join(d, 'Rscript')),
    '/usr/local/bin/Rscript', '/usr/bin/Rscript',
    ...versionsOptR().map((v) => path.join('/opt/R', v, 'bin', 'Rscript')),
  ];
  return candidats.filter(Boolean).find((c) => fs.existsSync(c)) || 'Rscript';
}

// The R the engine runs on: RSCRIPT above all (developers); then the organisation's, a policy
// (RHome under HKLM or HKCU\SOFTWARE\Policies\Nuee: an R's folder or its Rscript, as politique());
// then the one chosen in File › Settings… (reglages.rscript); Nuée's own otherwise (on Linux, the
// computer's: trouverRscript). { type: 'nuee', 'organisation', 'autre' or 'rscript', rscript (null:
// none where it was set), chemin (as it was set) }. r-installes.js finds and checks the others.
const rPropre = () => ({ type: 'nuee', rscript: trouverRscript(), chemin: null });
const rOrganisation = () => (process.platform === 'win32' ? rInstalles.politiqueTexte('RHome') : null);
function rVoulu() {
  if (process.env.RSCRIPT) return { type: 'rscript', rscript: trouverRscript(), chemin: process.env.RSCRIPT };
  const organisation = rOrganisation();
  const chemin = organisation || reglages.rscript;
  if (!chemin) return rPropre();
  return { type: organisation ? 'organisation' : 'autre', rscript: rInstalles.rscriptDe(chemin), chemin };
}
let rUtilise = null;        // the R the engine runs on now (rVoulu's, with `repli` when Nuée's own took over)
let rRepliASignaler = null;  // that fallback, until a window tells it (once: 'r-repli')
// The projects of the last session: the files of the saved projects open when Nuée last closed, as
// the main windows tell them while they change (reglages.seance; interface/app.js: noterSeance),
// read at start-up before this session writes its own; reopened at start when File › Settings… says so
// (reglages.rouvrirSeance), or from the start page's Reopen last session. Given once, to the first
// main window that asks: a second window, or the page loaded again, doesn't reopen them twice.
let seancePrecedente = [];
// Whether files are there (the last session's, when they reopen), without holding the main process:
// true, false, or null when their folder didn't answer in time (a network share out of reach, which
// the system waits for far longer). One look at a time per share, drive or mount, and none more at one
// that didn't answer: a look at a share out of reach holds one of the few threads Node reads files
// with until the system gives up.
const DELAI_PRESENCE = 5000;
const racineDe = (c) => ((/^(?:\\\\|\/\/)[^\\/]+[\\/][^\\/]+/.exec(c) || /^[A-Za-z]:/.exec(c) || /^\/[^/]+(?:\/[^/]+)?/.exec(c) || [''])[0]).toLowerCase();
async function fichiersPresents(chemins) {
  const presents = chemins.map(() => null);
  const parRacine = new Map();
  chemins.forEach((c, i) => parRacine.set(racineDe(c), [...(parRacine.get(racineDe(c)) || []), i]));
  await Promise.all([...parRacine.values()].map(async (indices) => {
    for (const i of indices) {
      let minuterie;
      presents[i] = await Promise.race([fs.promises.stat(chemins[i]).then((s) => s.isFile(), () => false),
        new Promise((ok) => { minuterie = setTimeout(ok, DELAI_PRESENCE, null); })]);
      clearTimeout(minuterie);
      if (presents[i] === null) return;
    }
  }));
  return presents;
}

// Quarto, if the computer has it: on the PATH, where its installer puts it, or the one RStudio
// brings along; null otherwise (documents.js then compiles by itself).
function trouverQuarto() {
  const exe = process.platform === 'win32' ? 'quarto.exe' : 'quarto';
  const programmes = process.env.ProgramFiles || 'C:\\Program Files';
  const local = process.env.LOCALAPPDATA || '';
  const candidats = [
    process.env.QUARTO_PATH,
    ...(process.env.PATH || '').split(path.delimiter).filter(Boolean).map((d) => path.join(d, exe)),
    ...(process.platform === 'win32' ? [
      path.join(local, 'Programs', 'Quarto', 'bin', exe), path.join(programmes, 'Quarto', 'bin', exe),
      path.join(programmes, 'RStudio', 'resources', 'app', 'bin', 'quarto', 'bin', exe),
      path.join(local, 'Programs', 'RStudio', 'resources', 'app', 'bin', 'quarto', 'bin', exe),
    ] : ['/usr/local/bin/quarto', '/opt/quarto/bin/quarto', '/usr/lib/rstudio/resources/app/bin/quarto/bin/quarto',
      '/Applications/quarto/bin/quarto', '/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto']),
  ];
  return candidats.filter(Boolean).find((c) => { try { return fs.statSync(c).isFile(); } catch { return false; } }) || null;
}
// R from Posit's packages: /opt/R/<version>, the most recent first
function versionsOptR() {
  try { return fs.readdirSync('/opt/R').sort((a, b) => b.localeCompare(a, undefined, { numeric: true })); } catch { return []; }
}

// Shown when R does not start on Linux: R and the packages the engine loads are installed apart.
const AIDE_R_LINUX = {
  fr: '\n\nSous Linux, Nuée utilise le R de l’ordinateur, avec les paquets httpuv, jsonlite, '
    + 'openxlsx, haven, readxl et foreign. Sous Debian ou Ubuntu :\n  sudo apt install r-base-core r-cran-httpuv '
    + 'r-cran-jsonlite r-cran-openxlsx r-cran-haven r-cran-readxl r-cran-foreign\nAilleurs, dans R : install.packages(c("httpuv", '
    + '"jsonlite", "openxlsx", "haven", "readxl", "foreign")). Un autre R : lancer Nuée avec RSCRIPT=/chemin/vers/Rscript.',
  en: '\n\nOn Linux, Nuée uses the computer’s R, with the packages httpuv, jsonlite, '
    + 'openxlsx, haven, readxl and foreign. On Debian or Ubuntu:\n  sudo apt install r-base-core r-cran-httpuv '
    + 'r-cran-jsonlite r-cran-openxlsx r-cran-haven r-cran-readxl r-cran-foreign\nElsewhere, in R: install.packages(c("httpuv", '
    + '"jsonlite", "openxlsx", "haven", "readxl", "foreign")). Another R: start Nuée with RSCRIPT=/path/to/Rscript.',
};

function portLibre(souhaite = 0) {
  return new Promise((resolve, reject) => {
    const s = net.createServer();
    s.once('error', (e) => (souhaite ? portLibre(0).then(resolve, reject) : reject(e)));
    s.listen(souhaite, '127.0.0.1', () => { const { port: p } = s.address(); s.close(() => resolve(p)); });
  });
}

// R's temporary files (pasted tables, converted .usp projects, files exported for a browser,
// console plots) go in a folder of this run, deleted when Nuée closes: R, stopped by taskkill,
// can't delete its own. The folders of runs that ended without closing go at the next start.
// On Linux, /tmp is shared by every user of the computer: the folder names the user too, and
// only this user's are ever cleaned.
const prefixeR = process.platform === 'win32' ? 'nuee-r-' : `nuee-r-${process.getuid()}-`;
const temporaireDuLancement = path.join(app.getPath('temp'), `${prefixeR}${process.pid}`);
function nettoyerTemporairesR() {
  let noms = [];
  try { noms = fs.readdirSync(app.getPath('temp')).filter((n) => n.startsWith(prefixeR) && /^\d+(-\w+)?$/.test(n.slice(prefixeR.length))); } catch { /* no temporary folder */ }
  for (const n of noms) {
    const pid = Number(n.slice(prefixeR.length).split('-')[0]);
    let vivant = pid === process.pid;
    if (!vivant) try { process.kill(pid, 0); vivant = true; } catch (e) { vivant = e.code === 'EPERM'; }  // (EPERM: running, someone else's)
    if (!vivant) supprimer(path.join(app.getPath('temp'), n));
  }
}

// A folder removed with all it holds (R's, where R and the R console write what they like). On
// Windows fs.rmSync gives up at a read-only file or folder (EPERM), and Node 24.12's left a path
// that isn't ASCII in place without an error: what is still there is made writable (by this user
// only) and removed one by one. (A file still held by a program stays: next time.)
function supprimer(chemin) {
  try { fs.rmSync(chemin, { recursive: true, force: true }); } catch { /* one by one below */ }
  const un = (c) => {
    const s = fs.lstatSync(c);
    if (!s.isSymbolicLink()) fs.chmodSync(c, s.isDirectory() ? 0o700 : 0o600);
    if (s.isDirectory()) { for (const n of fs.readdirSync(c)) un(path.join(c, n)); fs.rmdirSync(c); } else fs.unlinkSync(c);
  };
  try { if (fs.existsSync(chemin)) un(chemin); } catch { /* still held: next time */ }
}

// The folder made for this run, readable by this user only. On Linux, someone else could
// have made one of that name beforehand in the shared /tmp, to read or swap R's files: then
// another, with a name nobody can foresee.
let temporaireR = temporaireDuLancement;
function dossierTemporaireR() {
  try { fs.mkdirSync(temporaireR, { mode: 0o700 }); } catch (e) { if (e.code !== 'EEXIST') throw e; }
  if (process.platform === 'win32') return;
  const s = fs.lstatSync(temporaireR);
  if (!s.isDirectory() || s.uid !== process.getuid() || (s.mode & 0o077)) {
    temporaireR = fs.mkdtempSync(`${temporaireDuLancement}-`);
  }
}

// Stop, in the status bar (interface/app.js): R, one process, reads no request while it computes,
// so the stop is a file of this run's R folder (the one given to the R running), which its long
// loops look at between two rounds and which it removes at the next request (moteur/R/arret.R). A
// new R starts without it.
let fichierArret = null;
function arreterCalcul() {
  if (!fichierArret) return false;
  fs.writeFileSync(fichierArret, String(Date.now()));
  return true;
}

// What the R `r` (rVoulu's) runs with besides the computer's environment, as the engine and the
// installation of a plugin's packages into it: Nuée's own R (an installed copy, on Windows and macOS)
// finds the packages that come with Nuée before those of the user's own R library, which stays
// available to the R console (library(vegan)…); the Rscript of Nuée.app starts the R of RHOME, or else
// the one CRAN's installer puts in /Library/Frameworks, if the Mac has it. Another R loads its own
// libraries only: Nuée's packages are built for Nuée's R, and may not load in another version.
function environnementR(r) {
  const env = {};
  if (r.type === 'nuee' && app.isPackaged && process.platform !== 'linux') env.R_LIBS = path.join(racine, 'R', 'library');
  if (r.rscript === rscriptMac) env.RHOME = path.join(racine, 'R');
  return env;
}

// `r`: rVoulu's. repli: Nuée's own R takes over if this one stops before it is ready
// (demarrerMoteur), so that stop isn't reported to the windows. delai (ms, 0: none): an R not ready by
// then is stopped (`depasse` in the error).
function demarrerR(port, r, repli = false, delai = 0) {
  return new Promise((resolve, reject) => {
    dossierTemporaireR();
    fichierArret = path.join(temporaireR, 'arreter');
    try { fs.rmSync(fichierArret, { force: true }); } catch { /* none */ }
    const env = { ...process.env, NUEE_JETON: jeton, NUEE_CODE_SECRET: secretCode, NUEE_ARRET: fichierArret, TMPDIR: temporaireR, TMP: temporaireR, TEMP: temporaireR,
      NUEE_GREFFONS: dossierGreffons(), NUEE_VERSION: app.getVersion() };
    // (R-code models turned off for everyone by the organisation: the engine refuses them too)
    if (codeInterdit()) env.NUEE_SANS_CODE = '1'; else delete env.NUEE_SANS_CODE;
    if (dossierGreffonsOrganisation()) env.NUEE_GREFFONS_ORGANISATION = dossierGreffonsOrganisation(); else delete env.NUEE_GREFFONS_ORGANISATION;
    const conv = path.join(racine, 'outils', process.platform === 'win32' ? 'export_project.exe' : 'export_project');
    if (fs.existsSync(conv)) env.CONVERTISSEUR_USP = conv;
    // An installed copy runs the same engine whoever uses it: the researcher's own R setup stays
    // out (their .Rprofile could attach packages that hide the engine's functions), and the
    // packages that come with Nuée are found first (environnementR). Another R keeps its .Renviron.
    const optionsR = app.isPackaged ? ['--no-init-file'] : [];
    Object.assign(env, environnementR(r));
    let pret = false;
    let depasse = false;
    serveur = spawn(r.rscript, [...optionsR, path.join(racine, 'moteur', 'serveur.R'), String(port), interfaceDir], { env, windowsHide: true });
    const ceR = serveur;  // (the one the time limit stops: never one started after it)
    const minuterie = delai ? setTimeout(() => { if (!pret && serveur === ceR) { depasse = true; arreterR(); } }, delai) : null;
    serveur.stdout.on('data', (d) => { journal += d; if (!pret && journal.includes('PRET')) { pret = true; clearTimeout(minuterie); resolve(); } });
    serveur.stderr.on('data', (d) => { journal += d; });
    serveur.on('exit', (code) => {
      clearTimeout(minuterie);
      reject(Object.assign(new Error(`R s'est arrêté (code ${code}).\n${journal.slice(-2000)}`), { fin: journal.slice(-1500), depasse }));
      // once running, an unexpected stop is reported to the windows, which offer a restart
      if (!quitter && !arretVoulu && (pret || !repli)) BrowserWindow.getAllWindows().forEach((f) => f.webContents.send('moteur-arrete', journal.slice(-1500)));
    });
    serveur.on('error', (e) => { clearTimeout(minuterie); reject(e); });
  });
}

// The engine started on the R wanted (rVoulu). When that is another R than Nuée's own and it is
// missing, stops before it is ready, or isn't ready in time, Nuée's own starts instead, and the user
// is told once: by the first window that asks ('r-repli'), and not at the next starts while the same
// R fails (reglages.rSignale; forgotten once it starts, or when the settings change).
// In time: another R's first start reads every file of the engine, a minute and more on a slow
// computer or from a network share; one that is never ready (a share that doesn't answer, an
// Rprofile.site that waits) is stopped then. Nuée's own R has no such limit.
const DELAI_AUTRE_R = 90000;
async function demarrerMoteur() {
  const voulu = rVoulu();
  if (voulu.type !== 'autre' && voulu.type !== 'organisation') {
    await demarrerR(port, voulu);
    rUtilise = voulu;
    return;
  }
  let motif = 'absent';
  let sortie = null;
  if (voulu.rscript) {
    try {
      await demarrerR(port, voulu, true, DELAI_AUTRE_R);
      rUtilise = voulu;
      if (reglages.rSignale) { delete reglages.rSignale; ecrireReglages(); }
      return;
    } catch (e) {
      // (not ready in time: said so, with what it said meanwhile)
      motif = e.depasse ? 'delai' : (e.fin || e.message);
      if (e.depasse) sortie = e.fin || null;
    }
  }
  journal = '';
  const propre = rPropre();
  await demarrerR(port, propre);
  rUtilise = { ...propre, repli: { ...voulu, motif } };
  if (reglages.rSignale !== voulu.chemin) {
    rRepliASignaler = { ...decrireR(voulu), motif, sortie, secondes: DELAI_AUTRE_R / 1000, plateforme: process.platform };
    reglages.rSignale = voulu.chemin;
    ecrireReglages();
  }
}

// An R as Settings shows it, with its folder: its Rscript's, else as it was set (a folder, or an
// Rscript that isn't there)
const decrireR = (r) => (r ? { type: r.type, rscript: r.rscript, chemin: r.chemin,
  dossier: r.rscript || /rscript(\.exe)?$/i.test(r.chemin || '') ? rInstalles.dossierDe(r.rscript || r.chemin) : r.chemin } : null);
// What Settings shows (interface/reglages.js): the R in use and why, the one chosen, the
// organisation's, Nuée's own
function etatR() {
  const organisation = rOrganisation();
  const chemin = (c) => ({ rscript: rInstalles.rscriptDe(c), chemin: c });
  return {
    plateforme: process.platform,
    utilise: rUtilise && { ...decrireR(rUtilise), repli: rUtilise.repli ? { ...decrireR(rUtilise.repli), motif: rUtilise.repli.motif } : null },
    choisi: reglages.rscript ? decrireR({ type: 'autre', ...chemin(reglages.rscript) }) : null,
    organisation: organisation ? decrireR({ type: 'organisation', ...chemin(organisation) }) : null,
    rscript: process.env.RSCRIPT || null,
    propre: decrireR(rPropre()),
  };
}

// R still running. (R that ended from a signal has no exit code, only its signalCode: R raises its
// SIGSEGV again once it has said so, the system's OOM killer sends SIGKILL; on Windows, a process
// Node stopped with kill() ends the same way)
const rEnMarche = () => !!serveur && serveur.exitCode === null && serveur.signalCode === null;

// R and anything it started (on Linux and macOS, Rscript becomes R itself: stopping it is enough)
function arreterR() {
  if (!rEnMarche()) return;
  if (process.platform !== 'win32') { serveur.kill(); return; }
  try { execFileSync('taskkill', ['/pid', String(serveur.pid), '/T', '/F'], { stdio: 'ignore', windowsHide: true }); } catch { serveur.kill(); }
}

async function redemarrerR() {
  arretVoulu = true;
  if (rEnMarche()) {
    await new Promise((resolve) => { serveur.once('exit', resolve); arreterR(); });
  }
  arretVoulu = false;
  journal = '';
  await demarrerMoteur();
  await envoyerApprobations();  // (before the windows reopen their projects: their code approved runs)
}

// The approved fingerprints told to R (secretCode, see above): true once R has them.
function envoyerApprobations() {
  return new Promise((resolve) => {
    if (!adresse) { resolve(false); return; }
    const req = http.request(`${adresse}api/code_approuve`, { method: 'POST', timeout: 10000,
      headers: { 'Content-Type': 'application/json', Cookie: `${cookieR()}=${jeton}`, 'X-Nuee-Code': secretCode } }, (res) => {
      res.resume();
      res.on('end', () => resolve(res.statusCode === 200));
    });
    req.on('timeout', () => req.destroy());
    req.on('error', () => resolve(false));
    req.end(JSON.stringify({ empreintes: [].concat(reglages.codeApprouve || []) }));
  });
}
// Fingerprints the engine computed (hexadecimal SHA-256, whatever the R: an approval holds when
// File › Settings… changes the R), approved by this user: added to the settings, the most recent
// last, then told to R.
function approuverCode(empreintes) {
  const valides = [].concat(empreintes || []).filter((e) => typeof e === 'string' && /^[0-9a-f]{64}$/.test(e));
  const deja = [].concat(reglages.codeApprouve || []).filter((e) => !valides.includes(e));
  reglages.codeApprouve = [...deja, ...new Set(valides)].slice(-APPROUVES_MAX);
  ecrireReglages();
  return envoyerApprobations();
}

// "Save and close": the window saves each modified project (asking where for new ones);
// false if the user cancelled a save, or if the window's page has crashed (it can't save).
function demanderEnregistrement(fenetre) {
  return new Promise((resolve) => {
    const finir = (ok) => {
      ipcMain.removeListener('enregistrement-fini', fini);
      fenetre.webContents.removeListener('render-process-gone', perdue);
      resolve(ok);
    };
    const fini = (e, ok) => { if (e.sender === fenetre.webContents) finir(!!ok); };
    const perdue = () => finir(false);
    ipcMain.on('enregistrement-fini', fini);
    fenetre.webContents.once('render-process-gone', perdue);
    fenetre.webContents.send('enregistrer-tout');
  });
}

// Before R is asked what is unsaved: each window's page sends what it still holds (a document's
// text typed less than a second ago, a figure's last change: they are sent after a pause), and
// answers; two seconds at most for one (a page that has crashed doesn't).
function viderFenetres() {
  return Promise.all(BrowserWindow.getAllWindows().filter((f) => deLInterface(f.webContents.getURL())).map((f) => new Promise((ok) => {
    const finir = () => { clearTimeout(attente); ipcMain.removeListener('vide', fini); ok(); };
    const fini = (e) => { if (e.sender === f.webContents) finir(); };
    const attente = setTimeout(finir, 2000);
    ipcMain.on('vide', fini);
    f.webContents.send('vider');
  })));
}

function effacerSecours() {
  try { fs.rmSync(dossierSecours(), { recursive: true, force: true }); } catch { /* nothing to delete */ }
}
// Closing normally: the copies of this launch go (their projects were saved or left), those of
// an earlier stop that were neither recovered nor set aside stay, offered at the next start
// (closing that question without answering keeps them). They are the ones the index listed
// when Nuée started, or that the interface marks as waiting (a project not reopened after a
// restart of the R engine).
let secoursDuDemarrage = new Set();
function copiesEnAttente() {
  try { return JSON.parse(fs.readFileSync(indexSecours(), 'utf8')); } catch { return []; }
}
function fermerSecours() {
  const gardes = copiesEnAttente().filter((e) => e && e.fichier && (e.attente || secoursDuDemarrage.has(path.basename(e.fichier))));
  if (!gardes.length) { effacerSecours(); return; }
  try {
    const noms = new Set(gardes.map((e) => path.basename(e.fichier)));
    for (const f of fs.readdirSync(dossierSecours())) if (f !== 'index.json' && !noms.has(f)) fs.rmSync(path.join(dossierSecours(), f), { force: true });
    fs.writeFileSync(indexSecours(), JSON.stringify(gardes, null, 1));
  } catch { /* left as it is: offered again next time */ }
}
function secoursPresents() {
  try { return JSON.parse(fs.readFileSync(indexSecours(), 'utf8')).length > 0; } catch { return false; }
}

// The names of the projects with unsaved changes, asked to the R server; null when it doesn't
// answer within five seconds (stopped, or busy with a long computation): nothing is known then.
function projetsModifies() {
  return new Promise((resolve) => {
    const req = http.request(`${adresse}api/projets_modifies`, { method: 'POST', timeout: 5000,
      headers: { 'Content-Type': 'application/json', Cookie: `${cookieR()}=${jeton}` } }, (res) => {
      let corps = '';
      res.on('data', (d) => { corps += d; });
      res.on('end', () => {
        try { resolve(res.statusCode === 200 ? [].concat(JSON.parse(corps).noms || []) : null); } catch { resolve(null); }
      });
    });
    req.on('timeout', () => req.destroy());
    req.on('error', () => resolve(null));
    req.end('{}');
  });
}

function options(largeur, hauteur) {
  return {
    width: largeur, height: hauteur, minWidth: 640, minHeight: 420, title: TITRE, autoHideMenuBar: true, show: false,
    backgroundColor: '#eef0f3',
    // Windows takes the icon from Nuee.exe; Linux needs it given
    ...(process.platform === 'linux' ? { icon: path.join(__dirname, 'icone', 'nuee.png') } : {}),
    webPreferences: { preload: path.join(__dirname, 'preload.js'), contextIsolation: true, sandbox: true },
  };
}

// The windows show the interface and nothing else: what the preload gives them (writing files,
// opening them) must never reach a page from elsewhere.
const deLInterface = (url) => !!adresse && String(url || '').startsWith(adresse);
app.on('web-contents-created', (_, contenu) => {
  contenu.on('will-navigate', (e, url) => { if (!deLInterface(url)) e.preventDefault(); });
  contenu.on('will-attach-webview', (e) => e.preventDefault());
  // Right-click in text being written (a document, a field): the spelling suggestions, then
  // cut, copy, paste. (The interface's own menus, on plots and lists, stop the event first.)
  contenu.on('context-menu', (_, p) => {
    if (!p.isEditable) return;
    const fr = langue === 'fr';
    const items = [];
    if (p.misspelledWord) {
      for (const s of (p.dictionarySuggestions || []).slice(0, 6)) items.push({ label: s, click: () => contenu.replaceMisspelling(s) });
      if (!items.length) items.push({ label: fr ? 'Aucune suggestion' : 'No suggestions', enabled: false });
      items.push({ label: fr ? 'Ajouter au dictionnaire' : 'Add to dictionary', click: () => contenu.session.addWordToSpellCheckerDictionary(p.misspelledWord) },
        { type: 'separator' });
    }
    const f = p.editFlags || {};
    items.push({ label: fr ? 'Couper' : 'Cut', role: 'cut', enabled: !!f.canCut }, { label: fr ? 'Copier' : 'Copy', role: 'copy', enabled: !!f.canCopy },
      { label: fr ? 'Coller' : 'Paste', role: 'paste', enabled: !!f.canPaste }, { type: 'separator' },
      { label: fr ? 'Tout sélectionner' : 'Select all', role: 'selectAll', enabled: !!f.canSelectAll });
    Menu.buildFromTemplate(items).popup({ window: BrowserWindow.fromWebContents(contenu) || undefined });
  });
});

const cachees = new WeakSet();  // windows that stay hidden (a report printed to PDF)
function preparer(fenetre) {
  fenetre.once('ready-to-show', () => { if (!cachees.has(fenetre)) fenetre.show(); });
  // Windows opened by the interface (a second main window, a detached view) stay in the
  // app; web links go to the default browser; anything else (files, other programs) is refused.
  // (A main window outlives the one it was opened from; a detached view goes with it. Electron
  // closes a window with its opener: closing the first main window closed the others, and Nuée
  // with them, without asking about unsaved projects.)
  fenetre.webContents.setWindowOpenHandler(({ url }) => {
    if (deLInterface(url)) {
      const vue = new URL(url).searchParams.get('vue');
      return { action: 'allow', outlivesOpener: !vue, overrideBrowserWindowOptions: vue ? options(1000, 720) : options(1300, 850) };
    }
    if (/^(https?|mailto):/i.test(url)) shell.openExternal(url);
    return { action: 'deny' };
  });
  // A detached view, or one main window of several, just closes; the last main window closes
  // Nuée (its detached views with it), once unsaved projects are dealt with.
  fenetre.on('close', async (e) => {
    if (quitter || !principale(fenetre) || BrowserWindow.getAllWindows().filter(principale).length > 1) return;
    e.preventDefault();
    await fermerNuee(fenetre);
  });
}

// A main window of the interface (not a detached view, not the hidden one printing a report):
// those can save the projects.
function principale(f) {
  try { return deLInterface(f.webContents.getURL()) && !new URL(f.webContents.getURL()).searchParams.get('vue'); } catch { return false; }
}

// Closes Nuée, once unsaved projects are saved or left (accordFermeture); the recovery copies go
// with it, unless R didn't answer (they then hold the latest state known, offered next time). One
// close at a time: asked again meanwhile (a second ×, File › Quit, ⌘Q, Update), it waits for the
// first one's answer instead of asking again. Once the user agreed, the windows are hidden while the
// projects' last versions go (fifteen seconds at most): nothing can be changed there meanwhile, then
// lost. `apres`: what runs just before Nuée quits, if it does (the update's installer).
let fermeture = null;
const avantDeQuitter = [];
function fermerNuee(fenetre, apres = null) {
  if (apres) avantDeQuitter.push(apres);
  if (!fermeture) {
    fermeture = (async () => {
      const accord = await accordFermeture(fenetre);
      if (!accord) { avantDeQuitter.length = 0; return false; }
      for (const f of BrowserWindow.getAllWindows()) if (!f.isDestroyed()) f.hide();
      await versionsAvantDeFermer();
      quitter = true;
      if (accord !== 'secours') fermerSecours();
      for (const f of avantDeQuitter.splice(0)) f();
      app.quit();
      return true;
    })().finally(() => { fermeture = null; });
  }
  return fermeture;
}

// Before closing (or installing an update): projects with unsaved changes are saved, or left,
// as the user says, in the interface's language. false if they cancelled; 'secours' when R
// doesn't answer and there are recovery copies: close, but keep them. When R doesn't answer
// (a long computation, or stopped), what is unsaved isn't known: the user is asked either way.
async function accordFermeture(fenetre) {
  await viderFenetres();
  const noms = await projetsModifies();
  const en = langue === 'en';
  if (noms === null) {
    const secours = secoursPresents();
    const r = dialog.showMessageBoxSync(fenetre, {
      type: 'warning', defaultId: secours ? 0 : 1, cancelId: 1, title: TITRE, buttons: en ? ['Close', 'Cancel'] : ['Fermer', 'Annuler'],
      message: en ? 'The R engine isn’t answering.' : 'Le moteur R ne répond pas.',
      detail: secours
        ? (en ? 'Projects with unsaved changes will be offered at the next start, as they were in their last recovery copy (two minutes old at most).'
          : 'Les projets non enregistrés seront proposés au prochain lancement, tels qu’ils étaient dans leur dernière copie de secours (deux minutes au plus).')
        : (en ? 'It may still be computing. Changes not saved yet would be lost: Cancel, wait for the computation to end, then save.'
          : 'Il calcule peut-être encore. Les changements pas encore enregistrés seraient perdus : Annuler, attendre la fin du calcul, puis enregistrer.'),
    });
    if (r !== 0) return false;
    return secours ? 'secours' : true;
  }
  if (!noms.length) return true;
  const r = dialog.showMessageBoxSync(fenetre, {
    type: 'warning', defaultId: 0, cancelId: 2, title: TITRE, detail: noms.join('\n'),
    buttons: en ? ['Save and close', 'Close without saving', 'Cancel'] : ['Enregistrer et fermer', 'Fermer sans enregistrer', 'Annuler'],
    message: en ? 'Some projects have unsaved changes.' : 'Des projets ont des changements non enregistrés.',
  });
  if (r === 2) return false;
  return r === 1 || demanderEnregistrement(fenetre);
}
app.on('browser-window-created', (_, fenetre) => preparer(fenetre));

function fenetrePrincipale() {
  const f = reglages.fenetre;
  const visible = f && screen.getAllDisplays().some((d) => f.x >= d.bounds.x - 50 && f.x < d.bounds.x + d.bounds.width && f.y >= d.bounds.y - 50 && f.y < d.bounds.y + d.bounds.height);
  const fenetre = new BrowserWindow({ ...options(1400, 900), ...(visible ? { x: f.x, y: f.y, width: f.width, height: f.height } : {}) });
  if (visible && f.maximized) fenetre.maximize();
  // (its size and place kept once it stops moving, and when it closes: not the whole settings file
  // written again at each step of a move)
  let attente = null;
  const garder = () => {
    clearTimeout(attente);
    attente = null;
    if (fenetre.isDestroyed()) return;
    reglages.fenetre = { ...fenetre.getNormalBounds(), maximized: fenetre.isMaximized() };
    ecrireReglages();
  };
  const plusTard = () => { clearTimeout(attente); attente = setTimeout(garder, 1000); };
  fenetre.on('resize', plusTard);
  fenetre.on('move', plusTard);
  fenetre.on('close', () => { if (attente) garder(); });
  const fichier = fichierDonne(process.argv) || fichierMac;
  fichierMac = null;
  fenetre.loadURL(fichier ? `${adresse}?ouvrir=${encodeURIComponent(fichier)}` : adresse);
  return fenetre;
}

// The main window brought forward, with a file to open in it (null: none). The file waits in
// aOuvrir until a main window's page takes it (preload.js: surOuvrirFichier): it may come while
// Nuée is still starting, before its window, or the window's page, is there to hear about it.
const aOuvrir = [];
function montrerFenetre(fichier) {
  if (fichier) aOuvrir.push(fichier);
  const f = BrowserWindow.getAllWindows().find(principale) || BrowserWindow.getAllWindows()[0];
  if (!f) return;
  if (f.isMinimized()) f.restore();
  f.focus();
  if (fichier) f.webContents.send('ouvrir-fichier');
}

// macOS gives the file to open (a .nuee double-clicked in the Finder, dropped on the Dock icon)
// by an event, not on the command line, at start-up too: then kept for the main window.
let fichierMac = null;
app.on('open-file', (e, chemin) => {
  e.preventDefault();
  if (BrowserWindow.getAllWindows().length) montrerFenetre(chemin);
  else fichierMac = chemin;
});

// macOS: the menu bar at the top of the screen, besides the one the interface draws. Without
// its Edit menu, ⌘C, ⌘V, ⌘X, ⌘A and ⌘Z do nothing in text fields (the interface leaves them to
// the system there, and takes them elsewhere: the menu only gets the keys the page didn't use).
// Quit asks about unsaved projects, as File › Quit does; Settings… opens File › Settings… in the
// main window in front. In the interface's language.
function menuMac(l) {
  const fr = l === 'fr';
  const quitter = () => { const f = BrowserWindow.getAllWindows().find(principale); if (f) fermerNuee(f); else app.quit(); };
  const reglagesMac = () => {
    const devant = BrowserWindow.getFocusedWindow();
    const f = devant && principale(devant) ? devant : BrowserWindow.getAllWindows().find(principale);
    if (f) { f.focus(); f.webContents.send('ouvrir-reglages'); }
  };
  return Menu.buildFromTemplate([
    { label: TITRE, submenu: [
      { role: 'about', label: fr ? `À propos de ${TITRE}` : `About ${TITRE}` }, { type: 'separator' },
      { label: fr ? 'Réglages…' : 'Settings…', accelerator: 'Cmd+,', click: reglagesMac }, { type: 'separator' },
      { role: 'services', label: 'Services' }, { type: 'separator' },
      { role: 'hide', label: fr ? `Masquer ${TITRE}` : `Hide ${TITRE}` }, { role: 'hideOthers', label: fr ? 'Masquer les autres' : 'Hide Others' },
      { role: 'unhide', label: fr ? 'Tout afficher' : 'Show All' }, { type: 'separator' },
      { label: fr ? `Quitter ${TITRE}` : `Quit ${TITRE}`, accelerator: 'Cmd+Q', click: quitter },
    ] },
    { label: fr ? 'Édition' : 'Edit', submenu: [
      { role: 'undo', label: fr ? 'Annuler' : 'Undo' }, { role: 'redo', label: fr ? 'Rétablir' : 'Redo' }, { type: 'separator' },
      { role: 'cut', label: fr ? 'Couper' : 'Cut' }, { role: 'copy', label: fr ? 'Copier' : 'Copy' }, { role: 'paste', label: fr ? 'Coller' : 'Paste' },
      { role: 'selectAll', label: fr ? 'Tout sélectionner' : 'Select All' },
    ] },
    { label: fr ? 'Fenêtre' : 'Window', role: 'window', submenu: [
      { role: 'minimize', label: fr ? 'Placer dans le Dock' : 'Minimize' }, { role: 'zoom', label: 'Zoom' }, { type: 'separator' },
      { role: 'front', label: fr ? 'Tout ramener au premier plan' : 'Bring All to Front' },
    ] },
  ]);
}

// The file the system asks Nuée to open (a .nuee project double-clicked, "Open with"): the
// first argument that is an existing file, a relative path taken from the folder Nuée was
// started in (`dossier`: a second start's own, not this one's). A Linux desktop gives it as a
// file:// URL (%U).
function fichierDonne(argv, dossier = process.cwd()) {
  const chemin = (a) => { if (!/^file:\/\//i.test(a)) return path.resolve(dossier, a); try { return fileURLToPath(a); } catch { return null; } };
  return argv.slice(1).filter((a) => !a.startsWith('-')).map(chemin).find((a) => a && fs.existsSync(a) && fs.statSync(a).isFile()) || null;
}

// Versions of projects kept in the background with git (versions.js): GitHub and GitLab reached
// through Chromium's network stack, so that the system's proxy and certificates apply; the tokens
// encrypted by the system (safeStorage) in jetons.json beside the settings, its own settings (keep
// versions, the author) with the settings; the versions read written in this run's temporary
// folder, deleted when Nuée closes; what the background exchanges did told to every window.
const jetons = magasinJetons({ fichier: () => path.join(app.getPath('userData'), 'jetons.json'), safeStorage });
const versions = creerVersions({
  fetch: (url, init) => reseau.fetch(url, init),
  jetons,
  langue: () => langue,
  reglages: { lire: () => reglages.versions || {}, ecrire: (r) => { reglages.versions = r; ecrireReglages(); } },
  temporaire: () => { dossierTemporaireR(); return temporaireR; },
  evenement: (e) => { for (const f of BrowserWindow.getAllWindows()) if (!f.isDestroyed()) f.webContents.send('versions', e); },
});
// (a window back to the front: the open projects' remotes asked again, every five minutes at most)
app.on('browser-window-focus', () => versions.auPremierPlan());
// The open projects' last versions, before Nuée closes (fifteen seconds at most: their sending
// waits for the next start)
const versionsAvantDeFermer = () => Promise.race([versions.fermerTout(), new Promise((ok) => setTimeout(ok, 15000))]);
// Signing in to GitHub and GitLab (comptes.js): the token kept through the versions layer, the
// refresh tokens of the sign-ins that expire (GitLab's, after two hours) encrypted the same way
// beside it, what is known of each account (no secret) and the self-hosted GitLabs' application IDs
// with the settings; the server's sign-in page opened in the browser.
const comptes = creerComptes({
  fetch: (url, init) => reseau.fetch(url, init),
  jeton: versions.jeton,
  jetons,
  renouvellements: magasinJetons({ fichier: () => path.join(app.getPath('userData'), 'jetons-renouvellement.json'), safeStorage }),
  reglages: { lire: () => reglages.comptes || {}, ecrire: (c) => { reglages.comptes = c; ecrireReglages(); } },
  ouvrirNavigateur: (url) => shell.openExternal(url),
  langue: () => langue,
});

function ipc() {
  const parent = (e) => BrowserWindow.fromWebContents(e.sender);
  // only the interface's own pages may ask (see deLInterface)
  const recevoir = (canal, f) => ipcMain.handle(canal, (e, ...args) => {
    if (!deLInterface(e.senderFrame && e.senderFrame.url)) throw new Error(`${canal}: refusé`);
    return f(e, ...args);
  });
  // the interface gives the filters (formats.js), in its language
  recevoir('choisir-fichier', async (e, filtres) => {
    const r = await dialog.showOpenDialog(parent(e), { properties: ['openFile'], filters: filtres || [{ name: '*', extensions: ['*'] }] });
    return r.canceled ? null : r.filePaths[0];
  });
  // several files at once (spectra, one per file)
  recevoir('choisir-fichiers', async (e, filtres) => {
    const r = await dialog.showOpenDialog(parent(e), { properties: ['openFile', 'multiSelections'], filters: filtres || [{ name: '*', extensions: ['*'] }] });
    return r.canceled ? [] : r.filePaths;
  });
  // a folder (where a project opened from GitHub or GitLab goes: interface/versions.js)
  recevoir('choisir-dossier', async (e) => {
    const r = await dialog.showOpenDialog(parent(e), { properties: ['openDirectory', 'createDirectory'] });
    return r.canceled ? null : r.filePaths[0];
  });
  recevoir('lire-presse-papiers', () => clipboard.readText());
  recevoir('enregistrer-sous', async (e, nom, filtres) => {
    const r = await dialog.showSaveDialog(parent(e), { defaultPath: nom, filters: filtres });
    return r.canceled ? null : r.filePath;
  });
  recevoir('recents', () => reglages.recents);
  // (the last session's projects, once: their files, whether each is still there asked only when they
  // reopen, fichiersPresents; this session's, as told)
  recevoir('seance-precedente', () => {
    const liste = seancePrecedente;
    seancePrecedente = [];
    return liste.map((chemin) => ({ chemin }));
  });
  recevoir('seance-presents', (_, chemins) => fichiersPresents([].concat(chemins || []).filter((c) => typeof c === 'string').slice(0, 50)));
  recevoir('seance-noter', (_, chemins) => {
    const liste = [...new Set([].concat(chemins || []).filter((c) => typeof c === 'string' && c))].slice(0, 50);
    if (JSON.stringify(liste) === JSON.stringify(reglages.seance || [])) return;
    reglages.seance = liste;
    ecrireReglages();
  });
  recevoir('seance-rouvrir', (_, oui) => {
    if (oui !== undefined) { reglages.rouvrirSeance = !!oui; ecrireReglages(); }
    return reglages.rouvrirSeance === true;
  });
  recevoir('ajouter-recent', (_, chemin) => {
    reglages.recents = [chemin, ...reglages.recents.filter((r) => r !== chemin)].slice(0, 10);
    ecrireReglages();
    app.addRecentDocument(chemin);
    return reglages.recents;
  });
  recevoir('fenetres', (e) => BrowserWindow.getAllWindows()
    .map((f) => ({ id: f.id, titre: f.getTitle(), actuelle: f.webContents === e.sender, principale: principale(f) })));
  // (id: a window's, null: a main window, 'moi': the one asking, back after Zotero's picker)
  recevoir('focus-fenetre', (e, id) => {
    const f = id === 'moi' ? BrowserWindow.fromWebContents(e.sender)
      : id === null ? BrowserWindow.getAllWindows().find((x) => !new URL(x.webContents.getURL()).searchParams.get('vue')) : BrowserWindow.fromId(id);
    if (f) { if (f.isMinimized()) f.restore(); f.focus(); }
  });
  recevoir('zoom', (e, d) => {
    const w = e.sender;
    w.setZoomLevel(d === 0 ? 0 : w.getZoomLevel() + d * 0.5);
  });
  // Fichier › Quitter: asked once, from a main window, whichever window it came from
  recevoir('quitter', (e) => fermerNuee(BrowserWindow.getAllWindows().find(principale) || parent(e)));
  // Reports: a file written as given (Word), or an HTML page printed to PDF in a hidden window.
  // (its folder made if needed: a document's figures beside its .qmd)
  recevoir('ecrire-fichier', async (_, chemin, octets) => {
    await fs.promises.mkdir(path.dirname(chemin), { recursive: true });
    await fs.promises.writeFile(chemin, Buffer.from(octets));
  });
  // A document compiled by Quarto, when the computer has it (documents.js): its .qmd rendered to
  // Word, HTML or PDF (through Typst, which Quarto brings: no LaTeX needed). Returns the file
  // written, or the end of what Quarto said.
  recevoir('quarto-disponible', () => !!trouverQuarto());
  // Zotero on this computer, for a document's citations (zotero.js: its local API and Better
  // BibTeX's picker, read only). The picker waits for the user's choice: up to 15 minutes.
  recevoir('zotero', (_, chemin, delai) => demanderZotero(chemin, { delai: Math.min(Number(delai) || 8000, 15 * 60000) }));
  // (options.R: the bin folder of an R that has knitr and rmarkdown, for the R chunks of a
  // reproducible .qmd: the engine's, as QUARTO_R, unless the computer sets its own; on macOS
  // with its R folder as RHOME, without which the Rscript of Nuée.app starts another R: demarrerR)
  recevoir('quarto', (_, chemin, format, options = {}) => new Promise((ok) => {
    const quarto = trouverQuarto();
    if (!quarto) return ok({ erreur: 'quarto_absent' });
    if (!['docx', 'html', 'typst'].includes(format) || !/\.qmd$/i.test(String(chemin))) return ok({ erreur: 'demande' });
    const dossier = path.dirname(chemin);
    const sortie = path.join(dossier, path.basename(chemin).replace(/\.qmd$/i, format === 'typst' ? '.pdf' : `.${format}`));
    const R = options && typeof options.R === 'string' && !process.env.QUARTO_R && fs.existsSync(options.R) ? options.R : null;
    let journal = '';
    const env = R ? { ...process.env, QUARTO_R: R, ...(process.platform === 'darwin' ? { RHOME: path.dirname(R) } : {}) } : process.env;
    const q = spawn(quarto, ['render', path.basename(chemin), '--to', format], { cwd: dossier, windowsHide: true, env });
    q.stdout.on('data', (d) => { journal += d; });
    q.stderr.on('data', (d) => { journal += d; });
    q.on('error', (e) => ok({ erreur: e.message }));
    q.on('close', (code) => ok(code === 0 && fs.existsSync(sortie) ? { sortie } : { erreur: journal.slice(-3000) || `quarto: ${code}` }));
  }));
  // (format: a figure's page, { largeur, hauteur } in mm (figures.js), printed at that size
  // without margins, its first page only; otherwise A4. Its size is taken from the page's own
  // @page rule: Chromium puts a page on a grid of 1/300 inch, and from pageSize in inches it came
  // out up to 0.3 mm too large, from @page within 0.1 mm)
  recevoir('imprimer-pdf', async (_, html, chemin, format) => {
    const page = path.join(app.getPath('temp'), `rapport-${process.pid}-${Date.now()}.html`);
    await fs.promises.writeFile(page, html, 'utf8');
    const f = new BrowserWindow({ show: false, webPreferences: { offscreen: false, javascript: false } });
    cachees.add(f);
    const mm = (x) => Number(x) >= 10 && Number(x) <= 1000;
    const options = format && mm(format.largeur) && mm(format.hauteur)
      ? { pageSize: { width: format.largeur / 25.4, height: format.hauteur / 25.4 }, preferCSSPageSize: true, printBackground: true,
        margins: { top: 0, bottom: 0, left: 0, right: 0 }, pageRanges: '1' }
      : { pageSize: 'A4', printBackground: true, margins: { marginType: 'default' } };
    try {
      await f.loadFile(page);
      const pdf = await f.webContents.printToPDF(options);
      await fs.promises.writeFile(chemin, pdf);
    } finally {
      f.destroy();
      fs.promises.unlink(page).catch(() => {});
    }
  });
  recevoir('ouvrir-chemin', (_, chemin) => shell.openPath(chemin));
  // a project's versions (versions.js): { valeur }, or { erreur } with its message in the
  // interface's language, which preload.js throws again in the page (an error thrown here would
  // reach it inside Electron's own words)
  for (const [nom, f] of Object.entries(versions)) {
    recevoir(`versions-${nom}`, async (_, ...args) => {
      try { return { valeur: await f(...args) }; } catch (e) { return { erreur: e.message }; }
    });
  }
  // signing in to GitHub and GitLab (comptes.js), the same way, with the refusal's code; a sign-in
  // waited for by a window that closes meanwhile ends with it
  for (const [nom, f] of Object.entries(comptes)) {
    if (nom === 'preparer') continue;
    recevoir(`comptes-${nom}`, async (e, ...args) => {
      const fin = () => { comptes.annulerConnexion(args[0]).catch(() => {}); };
      const attente = nom === 'attendreConnexion' || nom === 'compteGit';
      if (attente) e.sender.once('destroyed', fin);
      try { return { valeur: await f(...args) }; } catch (err) { return { erreur: err.message, code: err.code || null }; } finally {
        if (attente && !e.sender.isDestroyed()) e.sender.removeListener('destroyed', fin);
      }
    });
  }
  recevoir('fichiers-a-ouvrir', () => aOuvrir.splice(0));  // (see montrerFenetre)
  // recovery copies: their folder, and the index of what they hold (files not in it are deleted)
  // (once Nuée is closing, nothing is written there any more: a copy or an index arriving after
  // the folder was deleted would bring it back, offering at the next start what was saved)
  recevoir('secours-dossier', () => { if (!quitter) fs.mkdirSync(dossierSecours(), { recursive: true }); return dossierSecours(); });
  recevoir('secours-liste', () => { try { return JSON.parse(fs.readFileSync(indexSecours(), 'utf8')); } catch { return []; } });
  recevoir('secours-index', (_, entrees) => {
    if (quitter) return;
    fs.mkdirSync(dossierSecours(), { recursive: true });
    const gardes = new Set(entrees.map((e) => path.basename(e.fichier)));
    for (const f of fs.readdirSync(dossierSecours())) if (f !== 'index.json' && !gardes.has(f)) fs.rmSync(path.join(dossierSecours(), f), { force: true });
    fs.writeFileSync(indexSecours(), JSON.stringify(entrees, null, 1));
  });
  recevoir('redemarrer-moteur', async () => {
    try { await redemarrerR(); return null; } catch (e) { return e.message; }
  });
  recevoir('arreter-calcul', () => arreterCalcul());
  // File › Settings…, the R the engine runs on (interface/reglages.js, r-installes.js): what is set
  // and in use, the Rs of this computer, one of them checked, its missing packages installed (one
  // installation at a time; what R says meanwhile sent to the window that asked), the choice kept
  // (null: Nuée's own; nothing changes while the organisation or RSCRIPT sets it), and a fallback to
  // Nuée's own R told once
  recevoir('r-etat', () => etatR());
  recevoir('r-trouver', () => rInstalles.trouver());
  recevoir('r-verifier', (_, chemin) => rInstalles.verifier(chemin));
  let installation = null;
  recevoir('r-installer', (e, chemin, paquets) => {
    if (!installation) {
      const fenetre = e.sender;
      installation = rInstalles.installer(chemin, paquets, { surTexte: (s) => { if (!fenetre.isDestroyed()) fenetre.send('r-installation', s); } })
        .finally(() => { installation = null; });
    }
    return installation;
  });
  // the packages a plugin needs (File › Plugins…, interface/vues_greffons.js), installed into the user
  // library of the R the engine runs on, as Settings installs the engine's own (one installation at a
  // time), that R run as the engine runs it (environnementR: on macOS, Nuée.app's Rscript starts
  // another R without it); names that aren't an R package's are left out
  recevoir('r-installer-greffon', (e, paquets) => {
    if (!installation) {
      const fenetre = e.sender;
      const noms = [...new Set([].concat(paquets || []).filter((x) => typeof x === 'string' && /^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$/.test(x)))];
      const r = rUtilise || rPropre();
      installation = rInstalles.installer(r.rscript, noms,
        { env: { ...process.env, ...environnementR(r) }, autres: noms, surTexte: (s) => { if (!fenetre.isDestroyed()) fenetre.send('r-installation', s); } })
        .finally(() => { installation = null; });
    }
    return installation;
  });
  recevoir('r-choisir', (_, chemin) => {
    const rscript = chemin ? rInstalles.rscriptDe(chemin) : null;
    if (!rOrganisation() && !process.env.RSCRIPT && (rscript || !chemin)) {
      if (rscript) reglages.rscript = rscript; else delete reglages.rscript;
      delete reglages.rSignale;
      ecrireReglages();
    }
    return etatR();
  });
  recevoir('r-repli', () => { const r = rRepliASignaler; rRepliASignaler = null; return r; });
  // R code approved by this user (interface/vues_code.js: typed and fitted, or read and run from the
  // bar over a project whose code was written elsewhere): true once the engine knows it; false when
  // the policy turns R-code models off
  recevoir('approuver-code', (_, empreintes) => (codeInterdit() ? false : approuverCode(empreintes)));
  recevoir('nouvelle-version', (_, maintenant) => nouvelleVersion(!!maintenant).catch((e) => ({ erreur: e.message })));
  // for Aide › Informations de diagnostic (no paths, nothing personal): which R, not where
  recevoir('diagnostic', () => ({ systeme: `${process.platform} ${process.getSystemVersion()} ${process.arch}`,
    electron: process.versions.electron, chrome: process.versions.chrome, installe: app.isPackaged,
    r: rUtilise ? { type: rUtilise.type, repli: rUtilise.repli ? rUtilise.repli.type : null } : null }));
  // the check at start-up, on or off (null: not offered, see verificationPossible)
  recevoir('maj-auto', (_, oui) => {
    if (!verificationPossible()) return null;
    if (oui !== undefined) { reglages.majAuto = !!oui; ecrireReglages(); }
    return reglages.majAuto !== false;
  });
  recevoir('langue', (_, l) => {
    langue = l === 'en' ? 'en' : 'fr';
    if (process.platform === 'darwin') Menu.setApplicationMenu(menuMac(langue));
  });
  // the full user guide, in the interface's language (text files next to the engine)
  recevoir('ouvrir-guide', () => shell.openPath(path.join(racine, langue === 'en' ? 'USER-GUIDE.txt' : 'LISEZ-MOI-APPLICATION.txt')));
  // One-click update: the new installer is downloaded (progress sent to the window) and
  // checked; then, once unsaved projects are dealt with, Nuée closes, the installer runs and
  // Nuée starts again. { erreur } when it can't: aucune, reseau, empreinte, annulee. One at a
  // time: Update chosen again meanwhile (its link stays in the status bar) gets the same answer.
  // (A second download emptying the same folder failed, said as a network error, and took the
  // first one's file away while it was being written: the first had no installer left to run.)
  const mettreAJour = async (e) => {
    const v = await (versionTrouvee || nouvelleVersion());
    if (!v || !v.installateur) return { erreur: 'aucune' };
    const fenetre = parent(e);
    let fichier;
    try {
      fichier = await telechargerInstallateur(v.installateur, (f) => { if (!fenetre.isDestroyed()) fenetre.webContents.send('maj-progression', f); });
    } catch (err) {
      return { erreur: err.code === 'empreinte' ? 'empreinte' : 'reseau', detail: err.message };
    }
    const ferme = await fermerNuee(fenetre, () => { arreterR(); installerPuisRelancer(fichier); });
    return ferme ? null : { erreur: 'annulee' };
  };
  let miseAJour = null;
  recevoir('mettre-a-jour', (e) => miseAJour || (miseAJour = mettreAJour(e).finally(() => { miseAJour = null; })));
}

// An installed copy (Setup.exe) hears about new versions: at start-up, one request to the
// GitHub page of the latest release (nothing else is sent), turned off in Aide. Not for the
// network-share copy, which the share updates, nor when running from the sources.
// NUEE_SOURCE_MAJ: another address answering like GitHub, to test the update
// (docs/developpement.md); the installer may then come from anywhere.
const DERNIERE_VERSION = process.env.NUEE_SOURCE_MAJ || 'https://api.github.com/repos/4rtefakt/nuee/releases/latest';
const sourceSure = (url) => !!process.env.NUEE_SOURCE_MAJ || String(url).startsWith('https://github.com/4rtefakt/nuee/releases/download/');
// An organisation that deploys and updates Nuée itself turns the check and the one-click update
// off for everyone (Group Policy, Intune…): DWORD DisableUpdateCheck = 1 under
// HKLM\SOFTWARE\Policies\Nuee (the computer, which wins) or HKCU\SOFTWARE\Policies\Nuee. (The text
// RHome there, the R every user's engine runs on, is read by r-installes.js: rVoulu.)
// docs/deploiement.md
function politique(nom) {
  if (process.platform !== 'win32') return null;
  for (const cle of ['HKLM\\SOFTWARE\\Policies\\Nuee', 'HKCU\\SOFTWARE\\Policies\\Nuee']) {
    try {
      const m = new RegExp(`${nom}\\s+REG_DWORD\\s+0x([0-9a-f]+)`, 'i')
        .exec(execFileSync('reg', ['query', cle, '/v', nom], { encoding: 'utf8', windowsHide: true, stdio: ['ignore', 'pipe', 'ignore'] }));
      if (m) return parseInt(m[1], 16);
    } catch { /* not set there */ }
  }
  return null;
}
// The same policy key turns R-code models off (DWORD DisableCustomCode = 1), as does the environment
// variable NUEE_SANS_CODE=1 (any system): they aren't offered, and the engine refuses them
// (NUEE_SANS_CODE, demarrerR). Read once per start.
let codeRefuse;
const codeInterdit = () => {
  if (codeRefuse === undefined) codeRefuse = process.env.NUEE_SANS_CODE === '1' || politique('DisableCustomCode') === 1;
  return codeRefuse;
};
let majInterdite;
const verificationPossible = () => {
  if (!app.isPackaged || process.argv.includes('--lanceur-nas')) return false;
  if (majInterdite === undefined) majInterdite = politique('DisableUpdateCheck') === 1;
  return !majInterdite;
};
let versionTrouvee;  // checked once per start, for every window, and again from Aide › Chercher une mise à jour
function chercherVersion() {
  return reseau.fetch(DERNIERE_VERSION, { headers: { Accept: 'application/vnd.github+json' } })
    .then((r) => { if (!r.ok) throw new Error(`HTTP ${r.status}`); return r.json(); })
    .then((j) => {
      const v = String(j.tag_name || '').replace(/^v/, '');
      if (!v || !plusRecente(v, app.getVersion())) return null;
      // its Windows installer, with the SHA-256 GitHub computed for it, for the one-click
      // update (elsewhere, the link opens the release page)
      const a = process.platform === 'win32' && (j.assets || []).find((x) => /^Nuee-Setup-[\d.]+\.exe$/.test(x.name) && /^sha256:[0-9a-f]{64}$/.test(x.digest || '') && sourceSure(x.browser_download_url));
      return { version: v, page: j.html_url,
        installateur: a ? { nom: a.name, url: a.browser_download_url, taille: a.size, empreinte: a.digest.slice(7) } : null };
    });
}
// maintenant: asked for in Aide, whatever the start-up setting; then a network error reaches
// the window ({ erreur }) instead of passing for "no new version".
function nouvelleVersion(maintenant = false) {
  if (!verificationPossible() || (reglages.majAuto === false && !maintenant)) return Promise.resolve(null);
  if (maintenant) {
    const v = chercherVersion();
    versionTrouvee = v.catch(() => null);
    return v;
  }
  if (versionTrouvee === undefined) versionTrouvee = chercherVersion().catch(() => null);
  return versionTrouvee;
}

// The installer, downloaded to the temporary folder; its SHA-256 must be the one GitHub gives.
async function telechargerInstallateur(a, progression) {
  const dossier = path.join(app.getPath('temp'), 'nuee-mise-a-jour');
  fs.rmSync(dossier, { recursive: true, force: true });
  fs.mkdirSync(dossier, { recursive: true });
  const fichier = path.join(dossier, a.nom);
  const r = await reseau.fetch(a.url);
  if (!r.ok) throw new Error(`HTTP ${r.status}`);
  const empreinte = crypto.createHash('sha256');
  const sortie = fs.createWriteStream(fichier);
  const lecteur = r.body.getReader();
  let recu = 0;
  let pourcent = -1;
  try {
    for (;;) {
      const { done, value } = await lecteur.read();
      if (done) break;
      empreinte.update(value);
      recu += value.length;
      if (!sortie.write(value)) await new Promise((ok) => sortie.once('drain', ok));
      if (Math.floor(100 * recu / a.taille) !== pourcent) { pourcent = Math.floor(100 * recu / a.taille); progression(Math.min(1, recu / a.taille)); }
    }
  } finally {
    await new Promise((ok) => sortie.end(ok));
  }
  if (empreinte.digest('hex') !== a.empreinte) {
    fs.rmSync(dossier, { recursive: true, force: true });
    throw Object.assign(new Error('SHA-256'), { code: 'empreinte' });
  }
  return fichier;
}

// Once Nuée has closed, the installer runs silently, in the same mode (for all users when it
// was installed so, which Windows asks to allow: installation.js; for this user otherwise) and folder;
// then Nuée starts again, with the same profile. --updated tells the installer it is an update:
// it then keeps the Start menu and desktop shortcuts and the application's taskbar identity, so a
// pin on the taskbar survives. Without it, an installer that lets one choose the folder
// (allowToChangeInstallationDirectory) has the old version's uninstaller delete the shortcuts and
// makes new ones, and Windows drops the pin with them. cmd.exe waits two seconds (Nuée has closed) by
// running this executable as Node, a program without a console (ping, a console program run by
// a cmd.exe that has none, opened a black window), then for the installer; the paths reach it
// through environment variables. A go-between starts it and ends at once (this executable run
// as Node), so it isn't a child of Nuée: the installer stops Nuée and everything Nuée started,
// and would stop it too.
function installerPuisRelancer(fichier) {
  const dossier = path.dirname(process.execPath);
  const pourTous = require('./installation').pourTousLesUtilisateurs(dossier);
  const relance = process.argv.slice(1).filter((x) => /^--(user-data-dir|remote-debugging-port)=/.test(x)).map((x) => `"${x}"`).join(' ');
  const commande = `start "" /wait "%NUEE_EXE%" -e "setTimeout(Boolean, 2000)" & set "ELECTRON_RUN_AS_NODE="`
    + ` & start "" /wait "%NUEE_INSTALLATEUR%" /S --updated ${pourTous ? '/allusers' : '/currentuser'}`
    + (/^[^&|<>^%"]+$/.test(dossier) ? ' /D=%NUEE_DOSSIER%' : '') + ` & start "" "%NUEE_EXE%"${relance ? ' %NUEE_RELANCE%' : ''}`;
  // (cmd.exe keeps ELECTRON_RUN_AS_NODE for the wait, then drops it: the installer, then Nuée)
  const relais = `require('child_process').spawn('cmd.exe', ['/d', '/s', '/c', '"' + process.env.NUEE_COMMANDE + '"'], { detached: true, stdio: 'ignore', windowsHide: true, windowsVerbatimArguments: true }).unref();`;
  spawn(process.execPath, ['-e', relais], {
    detached: true, stdio: 'ignore', windowsHide: true,
    env: { ...process.env, ELECTRON_RUN_AS_NODE: '1', NUEE_COMMANDE: commande, NUEE_INSTALLATEUR: fichier, NUEE_DOSSIER: dossier, NUEE_EXE: process.execPath,
      ...(relance ? { NUEE_RELANCE: relance } : {}) },
  }).unref();
}
function plusRecente(a, b) {
  const x = a.split('.').map(Number);
  const y = b.split('.').map(Number);
  for (let i = 0; i < 3; i += 1) if ((x[i] || 0) !== (y[i] || 0)) return (x[i] || 0) > (y[i] || 0);
  return false;
}

// Started by the network-share launcher (Lancer Nuee.cmd passes --lanceur-nas): the copy on
// this PC is listed in Settings › Apps like an installed program, with its uninstaller
// (resources\desinstaller.cmd). The Setup.exe installer lists itself.
const CLE_PROGRAMMES = 'HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Nuee';
function enregistrerProgramme() {
  if (process.platform !== 'win32' || !app.isPackaged || !process.argv.includes('--lanceur-nas')) return;
  const reg = (args) => execFileSync('reg', args, { encoding: 'utf8', windowsHide: true, stdio: ['ignore', 'pipe', 'ignore'] });
  try {
    if (new RegExp(`DisplayVersion\\s+REG_SZ\\s+${app.getVersion().replace(/\./g, '\\.')}\\s*$`, 'm').test(reg(['query', CLE_PROGRAMMES, '/v', 'DisplayVersion']))) return;
  } catch { /* not listed yet */ }
  const dossier = path.dirname(process.execPath);
  let octets = 0;
  const mesurer = (d) => { for (const e of fs.readdirSync(d, { withFileTypes: true })) { if (e.isDirectory()) mesurer(path.join(d, e.name)); else octets += fs.statSync(path.join(d, e.name)).size; } };
  try { mesurer(dossier); } catch { /* size left out */ }
  const valeurs = [
    ['DisplayName', 'REG_SZ', TITRE], ['DisplayVersion', 'REG_SZ', app.getVersion()], ['Publisher', 'REG_SZ', '4rtefakt'],
    ['DisplayIcon', 'REG_SZ', process.execPath], ['InstallLocation', 'REG_SZ', dossier],
    ['UninstallString', 'REG_SZ', `"${path.join(process.resourcesPath, 'desinstaller.cmd')}"`],
    ['NoModify', 'REG_DWORD', '1'], ['NoRepair', 'REG_DWORD', '1'], ['EstimatedSize', 'REG_DWORD', String(Math.round(octets / 1024))],
  ];
  try {
    for (const [nom, type, valeur] of valeurs) reg(['add', CLE_PROGRAMMES, '/v', nom, '/t', type, '/d', valeur, '/f']);
  } catch { /* registry closed to this user: the application works without it */ }
}

async function demarrer() {
  lireReglages();
  seancePrecedente = [].concat(reglages.seance || []).filter((c) => typeof c === 'string' && c);
  // A new version (an update): Chromium's HTTP cache may still hold the interface of the version
  // before, which the engine served up to 0.9.5 without Cache-Control; Chromium kept it fresh for
  // a tenth of its age and showed it again without asking, the new Nuée with the old interface.
  // Forgotten once, before any window; since 0.9.6 each load asks the engine (moteur/serveur.R).
  if (reglages.version !== app.getVersion()) {
    await session.defaultSession.clearCache().catch(() => {});
    reglages.version = app.getVersion();
    ecrireReglages();
  }
  // (the recovery copies of an earlier stop, before this launch writes any: fermerSecours)
  secoursDuDemarrage = new Set(copiesEnAttente().map((e) => path.basename(String((e && e.fichier) || ''))));
  enregistrerProgramme();
  nettoyerTemporairesR();
  // the installer of the last one-click update, once it has run
  if (verificationPossible()) fs.rm(path.join(app.getPath('temp'), 'nuee-mise-a-jour'), { recursive: true, force: true }, () => {});
  // the interface draws its own menu bar (macOS keeps one of its own: menuMac, in the system's
  // language until the interface gives its own)
  Menu.setApplicationMenu(process.platform === 'darwin' ? menuMac(app.getLocale().startsWith('fr') ? 'fr' : 'en') : null);
  // Spelling in French and in English, the languages of documents, besides the system's: alone,
  // the system's would mark every word of a document in the other one. (Their dictionaries
  // come the first time from Chromium's servers, as the system's does; macOS uses its own.)
  const s = session.defaultSession;
  const langues = [...new Set([...s.getSpellCheckerLanguages(), 'fr', 'en-US'])].filter((l) => s.availableSpellCheckerLanguages.includes(l));
  if (langues.length) s.setSpellCheckerLanguages(langues);
  ipc();
  comptes.preparer();  // (a GitLab sign-in's token renewed before it expires, while Nuée runs)
  // the same port as last time when free: on a computer shared by several people (remote
  // desktop), whoever doesn't get the usual one keeps their own, and their settings with it
  port = await portLibre(reglages.port || PORT_HABITUEL);
  if (port !== reglages.port) { reglages.port = port; ecrireReglages(); }
  try {
    await demarrerMoteur();
  } catch (err) {
    const l = app.getLocale().startsWith('fr') ? 'fr' : 'en';  // before the interface: the system's language
    dialog.showErrorBox(TITRE, `${l === 'fr' ? 'Le moteur R n’a pas pu démarrer.' : 'The R engine couldn’t start.'}\n\n${err.message}${process.platform === 'linux' ? AIDE_R_LINUX[l] : ''}`);
    app.quit();
    return;
  }
  adresse = `http://127.0.0.1:${port}/`;
  // a session cookie (not kept on disk), for this address only, unreadable by the page's scripts
  await session.defaultSession.cookies.set({ url: adresse, name: cookieR(), value: jeton, httpOnly: true, sameSite: 'strict' });
  await envoyerApprobations();  // (before the window opens a project: its approved code runs)
  fenetrePrincipale();
}

// Nuée started again (a .nuee double-clicked while it runs): that start hands its file to this one
// and ends, without starting R nor writing the settings (it wrote another port there, and the
// interface's own settings, kept by address, seemed lost at the next start)
const premiere = app.requestSingleInstanceLock();
if (!premiere) app.quit();
app.on('second-instance', (_, argv, dossier) => montrerFenetre(fichierDonne(argv, dossier)));  // its file opens in the main window
if (premiere) app.whenReady().then(demarrer);
app.on('window-all-closed', () => app.quit());
app.on('quit', () => {
  arreterR();
  supprimer(temporaireR);  // (a file still held: next start)
});
