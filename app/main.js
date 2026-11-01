// Desktop application: starts the R server (moteur/serveur.R) once and shows its interface
// in as many windows as wanted. Windows remember their size; recent files are kept.
const { app, BrowserWindow, Menu, clipboard, dialog, ipcMain, net: reseau, shell, screen } = require('electron');
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

// Settings, interface preferences and recovery copies in %APPDATA%\Nuee (or the folder given
// with --user-data-dir). Up to version 0.4 the application was called "Analyse multivariée":
// its settings are taken over once.
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

// bin\x64\Rscript.exe is R itself; bin\Rscript.exe only starts it, and stopping that one
// would leave R running.
function trouverRscript() {
  const candidats = [
    process.env.RSCRIPT,
    path.join(racine, 'R', 'bin', 'x64', 'Rscript.exe'),
    path.join(racine, 'R', 'bin', 'Rscript.exe'),
    path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'x64', 'Rscript.exe'),
    path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe'),
  ].filter(Boolean);
  return candidats.find((c) => fs.existsSync(c)) || 'Rscript';
}

function portLibre(souhaite = 0) {
  return new Promise((resolve, reject) => {
    const s = net.createServer();
    s.once('error', (e) => (souhaite ? portLibre(0).then(resolve, reject) : reject(e)));
    s.listen(souhaite, '127.0.0.1', () => { const { port: p } = s.address(); s.close(() => resolve(p)); });
  });
}

function demarrerR(port) {
  return new Promise((resolve, reject) => {
    const env = { ...process.env };
    const conv = path.join(racine, 'outils', 'export_project.exe');
    if (fs.existsSync(conv)) env.CONVERTISSEUR_USP = conv;
    serveur = spawn(trouverRscript(), [path.join(racine, 'moteur', 'serveur.R'), String(port), interfaceDir], { env, windowsHide: true });
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

// R and anything it started
function arreterR() {
  if (!serveur || serveur.exitCode !== null) return;
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
// false if the user cancelled a save.
function demanderEnregistrement(fenetre) {
  return new Promise((resolve) => {
    const fini = (e, ok) => {
      if (e.sender !== fenetre.webContents) return;
      ipcMain.removeListener('enregistrement-fini', fini);
      resolve(!!ok);
    };
    ipcMain.on('enregistrement-fini', fini);
    fenetre.webContents.send('enregistrer-tout');
  });
}

function effacerSecours() {
  try { fs.rmSync(dossierSecours(), { recursive: true, force: true }); } catch { /* nothing to delete */ }
}

// Projects with models not saved yet (asked to the R server).
function projetsModifies() {
  return new Promise((resolve) => {
    const req = http.request(`${adresse}api/etat`, { method: 'POST', headers: { 'Content-Type': 'application/json' } }, (res) => {
      let corps = '';
      res.on('data', (d) => { corps += d; });
      res.on('end', () => {
        try { resolve(JSON.parse(corps).projets.filter((p) => p.modifie).map((p) => p.nom)); } catch { resolve([]); }
      });
    });
    req.on('error', () => resolve([]));
    req.end('{}');
  });
}

function options(largeur, hauteur) {
  return {
    width: largeur, height: hauteur, minWidth: 640, minHeight: 420, title: TITRE, autoHideMenuBar: true, show: false,
    backgroundColor: '#eef0f3',
    webPreferences: { preload: path.join(__dirname, 'preload.js'), contextIsolation: true },
  };
}

function preparer(fenetre) {
  fenetre.once('ready-to-show', () => fenetre.show());
  // Windows opened by the interface (a second main window, a detached view) stay in the
  // app; anything else goes to the default browser.
  fenetre.webContents.setWindowOpenHandler(({ url }) => {
    if (url.startsWith(adresse)) {
      const vue = new URL(url).searchParams.get('vue');
      return { action: 'allow', overrideBrowserWindowOptions: vue ? options(1000, 720) : options(1300, 850) };
    }
    shell.openExternal(url);
    return { action: 'deny' };
  });
  fenetre.on('close', async (e) => {
    if (quitter || BrowserWindow.getAllWindows().length > 1) return;
    e.preventDefault();
    if (!(await accordFermeture(fenetre))) return;
    quitter = true;
    effacerSecours();
    app.quit();
  });
}

// Before closing (or installing an update): projects with unsaved changes are saved, or left,
// as the user says, in the interface's language; false if they cancelled.
async function accordFermeture(fenetre) {
  const noms = await projetsModifies();
  if (!noms.length) return true;
  const en = langue === 'en';
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
  fenetre.loadURL(adresse);
  return fenetre;
}

function ipc() {
  const parent = (e) => BrowserWindow.fromWebContents(e.sender);
  // the interface gives the filters (formats.js), in its language
  ipcMain.handle('choisir-fichier', async (e, filtres) => {
    const r = await dialog.showOpenDialog(parent(e), { properties: ['openFile'], filters: filtres || [{ name: '*', extensions: ['*'] }] });
    return r.canceled ? null : r.filePaths[0];
  });
  // several files at once (spectra, one per file)
  ipcMain.handle('choisir-fichiers', async (e, filtres) => {
    const r = await dialog.showOpenDialog(parent(e), { properties: ['openFile', 'multiSelections'], filters: filtres || [{ name: '*', extensions: ['*'] }] });
    return r.canceled ? [] : r.filePaths;
  });
  ipcMain.handle('lire-presse-papiers', () => clipboard.readText());
  ipcMain.handle('enregistrer-sous', async (e, nom, filtres) => {
    const r = await dialog.showSaveDialog(parent(e), { defaultPath: nom, filters: filtres });
    return r.canceled ? null : r.filePath;
  });
  ipcMain.handle('recents', () => reglages.recents);
  ipcMain.handle('ajouter-recent', (_, chemin) => {
    reglages.recents = [chemin, ...reglages.recents.filter((r) => r !== chemin)].slice(0, 10);
    ecrireReglages();
    app.addRecentDocument(chemin);
    return reglages.recents;
  });
  ipcMain.handle('fenetres', (e) => BrowserWindow.getAllWindows()
    .map((f) => ({ id: f.id, titre: f.getTitle(), actuelle: f.webContents === e.sender })));
  ipcMain.handle('focus-fenetre', (e, id) => {
    const f = id === null ? BrowserWindow.getAllWindows().find((x) => !new URL(x.webContents.getURL()).searchParams.get('vue')) : BrowserWindow.fromId(id);
    if (f) { if (f.isMinimized()) f.restore(); f.focus(); }
  });
  ipcMain.handle('zoom', (e, d) => {
    const w = e.sender;
    w.setZoomLevel(d === 0 ? 0 : w.getZoomLevel() + d * 0.5);
  });
  ipcMain.handle('quitter', () => { BrowserWindow.getAllWindows().forEach((f) => f.close()); });
  // Reports: a file written as given (Word), or an HTML page printed to PDF in a hidden window.
  ipcMain.handle('ecrire-fichier', (_, chemin, octets) => fs.promises.writeFile(chemin, Buffer.from(octets)));
  ipcMain.handle('imprimer-pdf', async (_, html, chemin) => {
    const page = path.join(app.getPath('temp'), `rapport-${process.pid}-${Date.now()}.html`);
    await fs.promises.writeFile(page, html, 'utf8');
    const f = new BrowserWindow({ show: false, webPreferences: { offscreen: false } });
    try {
      await f.loadFile(page);
      const pdf = await f.webContents.printToPDF({ pageSize: 'A4', printBackground: true, margins: { marginType: 'default' } });
      await fs.promises.writeFile(chemin, pdf);
    } finally {
      f.destroy();
      fs.promises.unlink(page).catch(() => {});
    }
  });
  ipcMain.handle('ouvrir-chemin', (_, chemin) => shell.openPath(chemin));
  // recovery copies: their folder, and the index of what they hold (files not in it are deleted)
  ipcMain.handle('secours-dossier', () => { fs.mkdirSync(dossierSecours(), { recursive: true }); return dossierSecours(); });
  ipcMain.handle('secours-liste', () => { try { return JSON.parse(fs.readFileSync(indexSecours(), 'utf8')); } catch { return []; } });
  ipcMain.handle('secours-index', (_, entrees) => {
    fs.mkdirSync(dossierSecours(), { recursive: true });
    const gardes = new Set(entrees.map((e) => path.basename(e.fichier)));
    for (const f of fs.readdirSync(dossierSecours())) if (f !== 'index.json' && !gardes.has(f)) fs.rmSync(path.join(dossierSecours(), f), { force: true });
    fs.writeFileSync(indexSecours(), JSON.stringify(entrees, null, 1));
  });
  ipcMain.handle('redemarrer-moteur', async () => {
    try { await redemarrerR(); return null; } catch (e) { return e.message; }
  });
  ipcMain.handle('nouvelle-version', () => nouvelleVersion());
  // the check at start-up, on or off (null: not offered, see verificationPossible)
  ipcMain.handle('maj-auto', (_, oui) => {
    if (!verificationPossible()) return null;
    if (oui !== undefined) { reglages.majAuto = !!oui; ecrireReglages(); }
    return reglages.majAuto !== false;
  });
  ipcMain.handle('langue', (_, l) => { langue = l === 'en' ? 'en' : 'fr'; });
  // the full user guide, in the interface's language (text files next to the engine)
  ipcMain.handle('ouvrir-guide', () => shell.openPath(path.join(racine, langue === 'en' ? 'USER-GUIDE.txt' : 'LISEZ-MOI-APPLICATION.txt')));
  // One-click update: the new installer is downloaded (progress sent to the window) and
  // checked; then, once unsaved projects are dealt with, Nuée closes, the installer runs and
  // Nuée starts again. { erreur } when it can't: aucune, reseau, empreinte, annulee.
  ipcMain.handle('mettre-a-jour', async (e) => {
    const v = await nouvelleVersion();
    if (!v || !v.installateur) return { erreur: 'aucune' };
    const fenetre = parent(e);
    let fichier;
    try {
      fichier = await telechargerInstallateur(v.installateur, (f) => { if (!fenetre.isDestroyed()) fenetre.webContents.send('maj-progression', f); });
    } catch (err) {
      return { erreur: err.code === 'empreinte' ? 'empreinte' : 'reseau', detail: err.message };
    }
    if (!(await accordFermeture(fenetre))) return { erreur: 'annulee' };
    quitter = true;
    effacerSecours();
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
const verificationPossible = () => app.isPackaged && !process.argv.includes('--lanceur-nas');
let versionTrouvee;  // checked once per start, for every window
function nouvelleVersion() {
  if (!verificationPossible() || reglages.majAuto === false) return Promise.resolve(null);
  if (versionTrouvee === undefined) {
    versionTrouvee = reseau.fetch(DERNIERE_VERSION, { headers: { Accept: 'application/vnd.github+json' } })
      .then((r) => (r.ok ? r.json() : null))
      .then((j) => {
        const v = j && String(j.tag_name || '').replace(/^v/, '');
        if (!v || !plusRecente(v, app.getVersion())) return null;
        // its Windows installer, with the SHA-256 GitHub computed for it, for the one-click
        // update (elsewhere, the link opens the release page)
        const a = process.platform === 'win32' && (j.assets || []).find((x) => /^Nuee-Setup-[\d.]+\.exe$/.test(x.name) && /^sha256:[0-9a-f]{64}$/.test(x.digest || '') && sourceSure(x.browser_download_url));
        return { version: v, page: j.html_url,
          installateur: a ? { nom: a.name, url: a.browser_download_url, taille: a.size, empreinte: a.digest.slice(7) } : null };
      })
      .catch(() => null);
  }
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
  if (!app.isPackaged || !process.argv.includes('--lanceur-nas')) return;
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
  // the installer of the last one-click update, once it has run
  if (verificationPossible()) fs.rm(path.join(app.getPath('temp'), 'nuee-mise-a-jour'), { recursive: true, force: true }, () => {});
  Menu.setApplicationMenu(null);  // the interface draws its own menu bar
  ipc();
  port = await portLibre(PORT_HABITUEL);
  try {
    await demarrerR(port);
  } catch (err) {
    dialog.showErrorBox(TITRE, `Le moteur R n'a pas pu démarrer.\n\n${err.message}`);
    app.quit();
    return;
  }
  adresse = `http://127.0.0.1:${port}/`;
  fenetrePrincipale();
}

if (!app.requestSingleInstanceLock()) app.quit();
app.on('second-instance', () => {
  const f = BrowserWindow.getAllWindows()[0];
  if (f) { if (f.isMinimized()) f.restore(); f.focus(); }
});
app.whenReady().then(demarrer);
app.on('window-all-closed', () => app.quit());
app.on('quit', () => arreterR());
