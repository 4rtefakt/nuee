// Desktop application: starts the R server (moteur/serveur.R) once and shows its interface
// in as many windows as wanted. Windows remember their size; recent files are kept.
const { app, BrowserWindow, Menu, clipboard, dialog, ipcMain, net: reseau, shell, screen } = require('electron');
const { spawn, execFileSync } = require('child_process');
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
    const noms = await projetsModifies();
    if (noms.length) {
      const r = dialog.showMessageBoxSync(fenetre, {
        type: 'warning', buttons: ['Enregistrer et fermer', 'Fermer sans enregistrer', 'Annuler'], defaultId: 0, cancelId: 2, title: TITRE,
        message: 'Des projets ont des changements non enregistrés.', detail: noms.join('\n'),
      });
      if (r === 2) return;
      if (r === 0 && !(await demanderEnregistrement(fenetre))) return;
    }
    quitter = true;
    effacerSecours();
    app.quit();
  });
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
}

// An installed copy (Setup.exe) hears about new versions: at start-up, one request to the
// GitHub page of the latest release (nothing else is sent), turned off in Aide. Not for the
// network-share copy, which the share updates, nor when running from the sources.
const DERNIERE_VERSION = 'https://api.github.com/repos/4rtefakt/nuee/releases/latest';
const verificationPossible = () => app.isPackaged && !process.argv.includes('--lanceur-nas');
let versionTrouvee;  // checked once per start, for every window
function nouvelleVersion() {
  if (!verificationPossible() || reglages.majAuto === false) return Promise.resolve(null);
  if (versionTrouvee === undefined) {
    versionTrouvee = reseau.fetch(DERNIERE_VERSION, { headers: { Accept: 'application/vnd.github+json' } })
      .then((r) => (r.ok ? r.json() : null))
      .then((j) => {
        const v = j && String(j.tag_name || '').replace(/^v/, '');
        return v && plusRecente(v, app.getVersion()) ? { version: v, page: j.html_url } : null;
      })
      .catch(() => null);
  }
  return versionTrouvee;
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
