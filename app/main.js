// Desktop application: starts the R server (moteur/serveur.R) once and shows its interface
// in as many windows as wanted. Windows remember their size; recent files are kept.
const { app, BrowserWindow, Menu, clipboard, dialog, ipcMain, net: reseau, session, shell, screen } = require('electron');
const { spawn, execFileSync } = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const net = require('net');
const path = require('path');

// Packaged: R, the engine and the .usp converter sit in resources/. In development they
// are found in the repository and the per-user R installation.
const racine = app.isPackaged ? process.resourcesPath : path.join(__dirname, '..');
const interfaceDir = path.join(__dirname, 'interface');
const fichierReglages = () => path.join(app.getPath('userData'), 'reglages.json');
const TITRE = 'Nuée';

// Settings, interface preferences and recovery copies in %APPDATA%\Nuee (~/.config/Nuee on
// Linux), or the folder given with --user-data-dir. Up to version 0.4 the application was
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
let langue = 'fr';  // the interface's, for the dialogs shown from here
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
function ecrireReglages() {
  try { fs.writeFileSync(fichierReglages(), JSON.stringify(reglages, null, 1)); } catch { /* read-only profile */ }
}

// Windows: bin\x64\Rscript.exe is R itself; bin\Rscript.exe only starts it, and stopping that
// one would leave R running. Linux: R is not brought along, the system's R is used (the .deb
// depends on it); found on the PATH, or where R's installers put it when a launch from the
// desktop menu gives a short PATH.
function trouverRscript() {
  const candidats = process.platform === 'win32' ? [
    process.env.RSCRIPT,
    path.join(racine, 'R', 'bin', 'x64', 'Rscript.exe'),
    path.join(racine, 'R', 'bin', 'Rscript.exe'),
    path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'x64', 'Rscript.exe'),
    path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe'),
  ] : [
    process.env.RSCRIPT,
    ...(process.env.PATH || '').split(path.delimiter).filter(Boolean).map((d) => path.join(d, 'Rscript')),
    '/usr/local/bin/Rscript', '/usr/bin/Rscript',
    ...versionsOptR().map((v) => path.join('/opt/R', v, 'bin', 'Rscript')),
  ];
  return candidats.filter(Boolean).find((c) => fs.existsSync(c)) || 'Rscript';
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
const temporaireR = path.join(app.getPath('temp'), `nuee-r-${process.pid}`);
function nettoyerTemporairesR() {
  let noms = [];
  try { noms = fs.readdirSync(app.getPath('temp')).filter((n) => /^nuee-r-\d+$/.test(n)); } catch { /* no temporary folder */ }
  for (const n of noms) {
    const pid = Number(n.slice(7));
    let vivant = pid === process.pid;
    if (!vivant) try { process.kill(pid, 0); vivant = true; } catch { vivant = false; }
    if (!vivant) fs.rmSync(path.join(app.getPath('temp'), n), { recursive: true, force: true });
  }
}

function demarrerR(port) {
  return new Promise((resolve, reject) => {
    fs.mkdirSync(temporaireR, { recursive: true });
    const env = { ...process.env, NUEE_JETON: jeton, TMPDIR: temporaireR, TMP: temporaireR, TEMP: temporaireR };
    const conv = path.join(racine, 'outils', 'export_project.exe');
    if (fs.existsSync(conv)) env.CONVERTISSEUR_USP = conv;
    // An installed copy runs the same engine whoever uses it: the researcher's own R setup stays
    // out (their .Rprofile could attach packages that hide the engine's functions), and the
    // packages that come with Nuée are found before those of their own R library, which stays
    // available to the R console (library(vegan)…).
    const optionsR = app.isPackaged ? ['--no-init-file'] : [];
    if (app.isPackaged && process.platform === 'win32') env.R_LIBS = path.join(racine, 'R', 'library');
    serveur = spawn(trouverRscript(), [...optionsR, path.join(racine, 'moteur', 'serveur.R'), String(port), interfaceDir], { env, windowsHide: true });
    serveur.stdout.on('data', (d) => { journal += d; if (journal.includes('PRET')) resolve(); });
    serveur.stderr.on('data', (d) => { journal += d; });
    serveur.on('exit', (code) => {
      reject(new Error(`R s'est arrêté (code ${code}).\n${journal.slice(-2000)}`));
      // once running, an unexpected stop is reported to the windows, which offer a restart
      if (!quitter && !arretVoulu) BrowserWindow.getAllWindows().forEach((f) => f.webContents.send('moteur-arrete', journal.slice(-1500)));
    });
    serveur.on('error', reject);
  });
}

// R and anything it started (on Linux, Rscript becomes R itself: stopping it is enough)
function arreterR() {
  if (!serveur || serveur.exitCode !== null) return;
  if (process.platform !== 'win32') { serveur.kill(); return; }
  try { execFileSync('taskkill', ['/pid', String(serveur.pid), '/T', '/F'], { stdio: 'ignore', windowsHide: true }); } catch { serveur.kill(); }
}

async function redemarrerR() {
  arretVoulu = true;
  if (serveur && serveur.exitCode === null) {
    await new Promise((resolve) => { serveur.once('exit', resolve); arreterR(); });
  }
  arretVoulu = false;
  journal = '';
  await demarrerR(port);
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

function effacerSecours() {
  try { fs.rmSync(dossierSecours(), { recursive: true, force: true }); } catch { /* nothing to delete */ }
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
});

const cachees = new WeakSet();  // windows that stay hidden (a report printed to PDF)
function preparer(fenetre) {
  fenetre.once('ready-to-show', () => { if (!cachees.has(fenetre)) fenetre.show(); });
  // Windows opened by the interface (a second main window, a detached view) stay in the
  // app; web links go to the default browser; anything else (files, other programs) is refused.
  fenetre.webContents.setWindowOpenHandler(({ url }) => {
    if (deLInterface(url)) {
      const vue = new URL(url).searchParams.get('vue');
      return { action: 'allow', overrideBrowserWindowOptions: vue ? options(1000, 720) : options(1300, 850) };
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
// with it, unless R didn't answer (they then hold the latest state known, offered next time).
async function fermerNuee(fenetre) {
  const accord = await accordFermeture(fenetre);
  if (!accord) return false;
  quitter = true;
  if (accord !== 'secours') effacerSecours();
  app.quit();
  return true;
}

// Before closing (or installing an update): projects with unsaved changes are saved, or left,
// as the user says, in the interface's language. false if they cancelled; 'secours' when R
// doesn't answer and there are recovery copies: close, but keep them.
async function accordFermeture(fenetre) {
  const noms = await projetsModifies();
  const en = langue === 'en';
  if (noms === null) {
    if (!secoursPresents()) return true;
    const r = dialog.showMessageBoxSync(fenetre, {
      type: 'warning', defaultId: 0, cancelId: 1, title: TITRE, buttons: en ? ['Close', 'Cancel'] : ['Fermer', 'Annuler'],
      message: en ? 'The R engine isn’t answering.' : 'Le moteur R ne répond pas.',
      detail: en ? 'Projects with unsaved changes will be offered at the next start, as they were in their last recovery copy (two minutes old at most).'
        : 'Les projets non enregistrés seront proposés au prochain lancement, tels qu’ils étaient dans leur dernière copie de secours (deux minutes au plus).',
    });
    return r === 0 ? 'secours' : false;
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
  const garder = () => {
    if (fenetre.isDestroyed()) return;
    reglages.fenetre = { ...fenetre.getNormalBounds(), maximized: fenetre.isMaximized() };
    ecrireReglages();
  };
  fenetre.on('resize', garder);
  fenetre.on('move', garder);
  const fichier = fichierDonne(process.argv);
  fenetre.loadURL(fichier ? `${adresse}?ouvrir=${encodeURIComponent(fichier)}` : adresse);
  return fenetre;
}

// The file Windows asks Nuée to open (a .nuee project double-clicked, "Open with"): the first
// argument that is an existing file.
function fichierDonne(argv) {
  return argv.slice(1).find((a) => !a.startsWith('-') && fs.existsSync(a) && fs.statSync(a).isFile()) || null;
}

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
  recevoir('lire-presse-papiers', () => clipboard.readText());
  recevoir('enregistrer-sous', async (e, nom, filtres) => {
    const r = await dialog.showSaveDialog(parent(e), { defaultPath: nom, filters: filtres });
    return r.canceled ? null : r.filePath;
  });
  recevoir('recents', () => reglages.recents);
  recevoir('ajouter-recent', (_, chemin) => {
    reglages.recents = [chemin, ...reglages.recents.filter((r) => r !== chemin)].slice(0, 10);
    ecrireReglages();
    app.addRecentDocument(chemin);
    return reglages.recents;
  });
  recevoir('fenetres', (e) => BrowserWindow.getAllWindows()
    .map((f) => ({ id: f.id, titre: f.getTitle(), actuelle: f.webContents === e.sender, principale: principale(f) })));
  recevoir('focus-fenetre', (e, id) => {
    const f = id === null ? BrowserWindow.getAllWindows().find((x) => !new URL(x.webContents.getURL()).searchParams.get('vue')) : BrowserWindow.fromId(id);
    if (f) { if (f.isMinimized()) f.restore(); f.focus(); }
  });
  recevoir('zoom', (e, d) => {
    const w = e.sender;
    w.setZoomLevel(d === 0 ? 0 : w.getZoomLevel() + d * 0.5);
  });
  // Fichier › Quitter: asked once, from a main window, whichever window it came from
  recevoir('quitter', (e) => fermerNuee(BrowserWindow.getAllWindows().find(principale) || parent(e)));
  // Reports: a file written as given (Word), or an HTML page printed to PDF in a hidden window.
  recevoir('ecrire-fichier', (_, chemin, octets) => fs.promises.writeFile(chemin, Buffer.from(octets)));
  recevoir('imprimer-pdf', async (_, html, chemin) => {
    const page = path.join(app.getPath('temp'), `rapport-${process.pid}-${Date.now()}.html`);
    await fs.promises.writeFile(page, html, 'utf8');
    const f = new BrowserWindow({ show: false, webPreferences: { offscreen: false, javascript: false } });
    cachees.add(f);
    try {
      await f.loadFile(page);
      const pdf = await f.webContents.printToPDF({ pageSize: 'A4', printBackground: true, margins: { marginType: 'default' } });
      await fs.promises.writeFile(chemin, pdf);
    } finally {
      f.destroy();
      fs.promises.unlink(page).catch(() => {});
    }
  });
  recevoir('ouvrir-chemin', (_, chemin) => shell.openPath(chemin));
  // recovery copies: their folder, and the index of what they hold (files not in it are deleted)
  recevoir('secours-dossier', () => { fs.mkdirSync(dossierSecours(), { recursive: true }); return dossierSecours(); });
  recevoir('secours-liste', () => { try { return JSON.parse(fs.readFileSync(indexSecours(), 'utf8')); } catch { return []; } });
  recevoir('secours-index', (_, entrees) => {
    fs.mkdirSync(dossierSecours(), { recursive: true });
    const gardes = new Set(entrees.map((e) => path.basename(e.fichier)));
    for (const f of fs.readdirSync(dossierSecours())) if (f !== 'index.json' && !gardes.has(f)) fs.rmSync(path.join(dossierSecours(), f), { force: true });
    fs.writeFileSync(indexSecours(), JSON.stringify(entrees, null, 1));
  });
  recevoir('redemarrer-moteur', async () => {
    try { await redemarrerR(); return null; } catch (e) { return e.message; }
  });
  recevoir('nouvelle-version', (_, maintenant) => nouvelleVersion(!!maintenant).catch((e) => ({ erreur: e.message })));
  // for Aide › Informations de diagnostic (no paths, nothing personal)
  recevoir('diagnostic', () => ({ systeme: `${process.platform} ${process.getSystemVersion()} ${process.arch}`,
    electron: process.versions.electron, chrome: process.versions.chrome, installe: app.isPackaged }));
  // the check at start-up, on or off (null: not offered, see verificationPossible)
  recevoir('maj-auto', (_, oui) => {
    if (!verificationPossible()) return null;
    if (oui !== undefined) { reglages.majAuto = !!oui; ecrireReglages(); }
    return reglages.majAuto !== false;
  });
  recevoir('langue', (_, l) => { langue = l === 'en' ? 'en' : 'fr'; });
  // the full user guide, in the interface's language (text files next to the engine)
  recevoir('ouvrir-guide', () => shell.openPath(path.join(racine, langue === 'en' ? 'USER-GUIDE.txt' : 'LISEZ-MOI-APPLICATION.txt')));
  // One-click update: the new installer is downloaded (progress sent to the window) and
  // checked; then, once unsaved projects are dealt with, Nuée closes, the installer runs and
  // Nuée starts again. { erreur } when it can't: aucune, reseau, empreinte, annulee.
  recevoir('mettre-a-jour', async (e) => {
    const v = await (versionTrouvee || nouvelleVersion());
    if (!v || !v.installateur) return { erreur: 'aucune' };
    const fenetre = parent(e);
    let fichier;
    try {
      fichier = await telechargerInstallateur(v.installateur, (f) => { if (!fenetre.isDestroyed()) fenetre.webContents.send('maj-progression', f); });
    } catch (err) {
      return { erreur: err.code === 'empreinte' ? 'empreinte' : 'reseau', detail: err.message };
    }
    const accord = await accordFermeture(fenetre);
    if (!accord) return { erreur: 'annulee' };
    quitter = true;
    if (accord !== 'secours') effacerSecours();
    arreterR();
    installerPuisRelancer(fichier);
    app.quit();
    return null;
  });
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
// HKLM\SOFTWARE\Policies\Nuee (the computer, which wins) or HKCU\SOFTWARE\Policies\Nuee.
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

// Once Nuée has closed, the installer runs silently, in the same mode (for all users when
// Nuée is in Program Files, which Windows asks to allow; for this user otherwise) and folder;
// then Nuée starts again, with the same profile. cmd.exe waits two seconds (Nuée has closed),
// then for the installer; the paths reach it through environment variables. A go-between
// starts it and ends at once (this executable run as Node), so it isn't a child of Nuée: the
// installer stops Nuée and everything Nuée started, and would stop it too.
function installerPuisRelancer(fichier) {
  const dossier = path.dirname(process.execPath);
  const programmes = [process.env.ProgramFiles, process.env.ProgramW6432, process.env['ProgramFiles(x86)']].filter(Boolean).map((p) => p.toLowerCase() + path.sep);
  const pourTous = programmes.some((p) => (dossier.toLowerCase() + path.sep).startsWith(p));
  const relance = process.argv.slice(1).filter((x) => /^--(user-data-dir|remote-debugging-port)=/.test(x)).map((x) => `"${x}"`).join(' ');
  const commande = `ping -n 3 127.0.0.1 > nul & start "" /wait "%NUEE_INSTALLATEUR%" /S ${pourTous ? '/allusers' : '/currentuser'}`
    + (/^[^&|<>^%"]+$/.test(dossier) ? ' /D=%NUEE_DOSSIER%' : '') + ` & start "" "%NUEE_EXE%"${relance ? ' %NUEE_RELANCE%' : ''}`;
  const relais = `const env = { ...process.env }; delete env.ELECTRON_RUN_AS_NODE;
    require('child_process').spawn('cmd.exe', ['/d', '/s', '/c', '"' + env.NUEE_COMMANDE + '"'], { env, detached: true, stdio: 'ignore', windowsHide: true, windowsVerbatimArguments: true }).unref();`;
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
  enregistrerProgramme();
  nettoyerTemporairesR();
  // the installer of the last one-click update, once it has run
  if (verificationPossible()) fs.rm(path.join(app.getPath('temp'), 'nuee-mise-a-jour'), { recursive: true, force: true }, () => {});
  Menu.setApplicationMenu(null);  // the interface draws its own menu bar
  ipc();
  // the same port as last time when free: on a computer shared by several people (remote
  // desktop), whoever doesn't get the usual one keeps their own, and their settings with it
  port = await portLibre(reglages.port || PORT_HABITUEL);
  if (port !== reglages.port) { reglages.port = port; ecrireReglages(); }
  try {
    await demarrerR(port);
  } catch (err) {
    const l = app.getLocale().startsWith('fr') ? 'fr' : 'en';  // before the interface: the system's language
    dialog.showErrorBox(TITRE, `${l === 'fr' ? 'Le moteur R n’a pas pu démarrer.' : 'The R engine couldn’t start.'}\n\n${err.message}${process.platform === 'linux' ? AIDE_R_LINUX[l] : ''}`);
    app.quit();
    return;
  }
  adresse = `http://127.0.0.1:${port}/`;
  // a session cookie (not kept on disk), for this address only, unreadable by the page's scripts
  await session.defaultSession.cookies.set({ url: adresse, name: cookieR(), value: jeton, httpOnly: true, sameSite: 'strict' });
  fenetrePrincipale();
}

if (!app.requestSingleInstanceLock()) app.quit();
app.on('second-instance', (_, argv) => {  // Nuée started again: its file opens in the main window
  const f = BrowserWindow.getAllWindows().find((x) => !new URL(x.webContents.getURL()).searchParams.get('vue')) || BrowserWindow.getAllWindows()[0];
  if (!f) return;
  if (f.isMinimized()) f.restore();
  f.focus();
  const fichier = fichierDonne(argv);
  if (fichier) f.webContents.send('ouvrir-fichier', fichier);
});
app.whenReady().then(demarrer);
app.on('window-all-closed', () => app.quit());
app.on('quit', () => {
  arreterR();
  try { fs.rmSync(temporaireR, { recursive: true, force: true }); } catch { /* a file still held: next start */ }
});
