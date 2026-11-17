// The driver of the workflow tests (tests/essai_parcours.js): it starts the built program with a
// profile of its own, then uses its window over the Chrome DevTools Protocol as a person would:
// real clicks and keys on the menus, dialogs, plots and editors, then checks what the window shows
// against what the engine answers. Every wait is for a condition (an element, an answer, a status),
// never a fixed time, so a slow machine only makes the run longer.
//
// The program's main process is reached through its Node inspector (--inspect), where the
// system's file dialogs are answered (the next path a scenario gives, or "cancelled"), and where
// files are opened, links followed and recent documents added only on paper: a run never opens
// Word or a browser, nor touches the system's list of recent files.
//
// A scenario (parcours/<number>_<name>.js) exports executer(n, verifier, contexte); `n`, which
// demarrer() returns, drives the window:
//   n.menu('m_fichier', 'ouvrir')        a menu item, by the keys of its labels (i18n.js)
//   n.cliquer(expression)                a click at the middle of an element (an expression in the page)
//   n.ecrire(expression, texte)          a field's text replaced, typed
//   n.choisir(expression, valeur)        an option of a <select> chosen
//   n.bouton('ajuster')                  a button of the dialog in front, by its label's key
//   n.attendre(expression)               until the expression is true in the page (60 s at most)
//   n.fichierAOuvrir(chemin)             the answer of the next Open dialog (n.fichierAEnregistrer: Save)
//   n.nouveauModele({ type, y, sans })   Model › New model filled in, then fitted
//   n.api('etat')                        a request to the engine, as the interface makes it
//   n.fenetre('vue=scores')              a view's own window, by a piece of its address
//   n.fenetre((url) => …)                another main window (Window › New window), its own `n`
// and the helpers below (graphe, plotlyPret, cliquerPoint, modele, lireClasseur, ...).
const { spawn, execFileSync } = require('child_process');
const fs = require('fs');
const net = require('net');
const path = require('path');
const zlib = require('zlib');

const windows = process.platform === 'win32';
const attente = (ms) => new Promise((r) => setTimeout(r, ms));  // (between two looks at a condition only)
const ATTENTE = Number(process.env.NUEE_ATTENTE || 60000);    // the longest wait for one condition, in ms
// a DevTools command the program doesn't answer within this time fails, naming it, instead of
// holding the run until the CI machine gives up (a start on GitHub's arm64 Mac once waited 85 min)
const REPONSE_MAX = Math.max(3 * ATTENTE, 180000);

// Every program a run started is stopped with the run, however it ends: a failure, an exception,
// Ctrl+C, the CI cancelling it. (A run killed outright runs nothing more: the program's main process
// then sees it gone and stops itself, GARDIEN.) No window outlives its run.
const lances = new Set();
let accroche = false;
function arreterAvecLeRun(arreter) {
  lances.add(arreter);
  if (accroche) return;
  accroche = true;
  process.on('exit', () => { for (const a of lances) a(); });
  for (const [signal, code] of [['SIGINT', 130], ['SIGTERM', 143], ['SIGHUP', 129], ['SIGBREAK', 149]]) {
    process.on(signal, () => process.exit(code));  // (the 'exit' above stops them)
  }
}

// What the program's main process runs once the run reaches it: every second, whether the run is
// still there; once it isn't, the program stops, with all it started (R).
const GARDIEN = (pid) => `(() => {
  if (globalThis.__gardien) return true;
  const { execFile } = (process.mainModule ? process.mainModule.require.bind(process.mainModule) : require)('child_process');
  globalThis.__gardien = setInterval(() => {
    try { process.kill(${pid}, 0); return; } catch (e) { if (e.code === 'EPERM') return; }
    clearInterval(globalThis.__gardien);
    if (process.platform === 'win32') execFile('taskkill', ['/pid', String(process.pid), '/T', '/F'], () => process.exit(1));
    else try { process.kill(-process.pid, 'SIGKILL'); } catch { process.exit(1); }
  }, 1000);
  globalThis.__gardien.unref();
  return true;
})()`;

function portLibre() {
  return new Promise((ok, ko) => {
    const s = net.createServer();
    s.once('error', ko);
    s.listen(0, '127.0.0.1', () => { const { port } = s.address(); s.close(() => ok(port)); });
  });
}

// A Chrome DevTools Protocol session over a WebSocket: commands, and the events a test waits for.
async function session(url) {
  const ws = new WebSocket(url);
  await new Promise((ok, ko) => {
    const delai = setTimeout(() => { ws.close(); ko(new Error(`no DevTools connection within ${REPONSE_MAX / 1000} s: ${url}`)); }, REPONSE_MAX);
    ws.onopen = () => { clearTimeout(delai); ok(); };
    ws.onerror = () => { clearTimeout(delai); ko(new Error(`no DevTools connection: ${url}`)); };
  });
  let n = 0;
  const reponses = new Map();
  const ecouteurs = new Set();
  ws.onmessage = (m) => {
    const j = JSON.parse(m.data);
    if (j.id && reponses.has(j.id)) { reponses.get(j.id)(j); reponses.delete(j.id); return; }
    for (const f of ecouteurs) f(j);
  };
  ws.onclose = () => { for (const f of reponses.values()) f({ error: { message: 'connection closed' } }); reponses.clear(); };
  return {
    envoyer: (method, params = {}) => new Promise((ok, ko) => {
      if (ws.readyState !== 1) { ko(new Error(`${method}: connection closed`)); return; }
      n += 1;
      const id = n;
      const delai = setTimeout(() => { reponses.delete(id); ko(new Error(`${method}: no answer within ${REPONSE_MAX / 1000} s`)); }, REPONSE_MAX);
      reponses.set(id, (j) => { clearTimeout(delai); if (j.error) ko(new Error(`${method}: ${j.error.message}`)); else ok(j.result); });
      ws.send(JSON.stringify({ id, method, params }));
    }),
    ecouter: (f) => { ecouteurs.add(f); return () => ecouteurs.delete(f); },
    fermer: () => ws.close(),
  };
}

// What the main process does instead of the system's dialogs, opening files and following links
// (installed once its inspector answers; main.js holds the same `dialog`, `shell` and `app`).
const SUBSTITUTS = `(() => {
  // (the inspector's own require, until main.js is the main module)
  const { dialog, shell, app } = (process.mainModule ? process.mainModule.require.bind(process.mainModule) : require)('electron');
  const E = globalThis.__essai = globalThis.__essai || { ouvrir: [], enregistrer: [], messages: [], appels: [], ouverts: [], liens: [], recents: [] };
  const sans = (a, b) => (b === undefined ? a : b);  // (the window is optional)
  dialog.showOpenDialog = async (a, b) => {
    const o = sans(a, b) || {};
    const r = E.ouvrir.shift();
    E.appels.push({ dialogue: 'ouvrir', plusieurs: (o.properties || []).includes('multiSelections'), reponse: r === undefined ? null : r });
    return r === undefined || r === null ? { canceled: true, filePaths: [] } : { canceled: false, filePaths: [].concat(r) };
  };
  // an error box (the R engine couldn't start...) written to the output instead of shown: it
  // would wait for a click that never comes, and the run would only time out, saying nothing
  dialog.showErrorBox = (titre, contenu) => { E.appels.push({ dialogue: 'erreur', titre, contenu }); console.error('[error box] ' + titre + ': ' + contenu); };
  dialog.showSaveDialog = async (a, b) => {
    const o = sans(a, b) || {};
    const r = E.enregistrer.shift();
    E.appels.push({ dialogue: 'enregistrer', nom: o.defaultPath || '', reponse: r === undefined ? null : r });
    return r === undefined || r === null ? { canceled: true } : { canceled: false, filePath: r };
  };
  // a question (unsaved projects, the engine not answering): the answer queued, else Cancel
  const message = (a, b) => {
    const o = sans(a, b) || {};
    const r = E.messages.shift();
    E.appels.push({ dialogue: 'message', message: o.message || '', reponse: r === undefined ? null : r });
    return r === undefined ? (o.cancelId ?? (o.buttons || []).length - 1) : r;
  };
  dialog.showMessageBoxSync = message;
  dialog.showMessageBox = async (a, b) => ({ response: message(a, b), checkboxChecked: false });
  shell.openPath = async (chemin) => { E.ouverts.push(chemin); return ''; };
  shell.openExternal = async (url) => { E.liens.push(url); };
  app.addRecentDocument = (chemin) => { E.recents.push(chemin); };
  return true;
})()`;

// NUEE_HORS_ECRAN=1, a run on a computer someone is using: the program's windows kept off the screen
// and out of the taskbar, shown without ever taking the focus, and still drawn (Chromium's occlusion
// tracking off, demarrer; their pages never throttled as hidden ones).
const HORS_ECRAN = process.env.NUEE_HORS_ECRAN === '1';
const SANS_ECRAN = `(() => {
  const { app, BrowserWindow } = (process.mainModule ? process.mainModule.require.bind(process.mainModule) : require)('electron');
  const cacher = (w) => { w.setPosition(-32000, -32000); w.setSkipTaskbar(true); w.webContents.setBackgroundThrottling(false); };
  BrowserWindow.prototype.show = function show() { cacher(this); this.showInactive(); };
  BrowserWindow.prototype.focus = function focus() {};
  for (const w of BrowserWindow.getAllWindows()) cacher(w);
  if (!globalThis.__horsEcran) { globalThis.__horsEcran = true; app.on('browser-window-created', (_, w) => cacher(w)); }
  return true;
})()`;

// What each page of the window gets before its own scripts: the requests to the engine counted
// (those under way, those refused), and the security policy's violations.
const ESPION = `(() => {
  const E = window.__essai = { enCours: 0, refus: [], requetes: [], violations: [] };
  document.addEventListener('securitypolicyviolation', (e) => E.violations.push(e.violatedDirective));
  // the last mouse event the page got: where, and on what, and how many moves so far (cliquer)
  E.mouvements = 0;
  for (const type of ['mousemove', 'mousedown']) {
    window.addEventListener(type, (e) => { E.souris = { type, x: e.clientX, y: e.clientY, cible: e.target }; if (type === 'mousemove') E.mouvements += 1; }, true);
  }
  const f = window.fetch;
  window.fetch = async (...a) => {
    const url = String((a[0] && a[0].url) || a[0]);
    const nom = (/\\/api\\/([^?]+)/.exec(url) || [])[1];
    // (the R console reads its log every 1.5 s: not counted)
    const compte = !!nom && nom !== 'journal';
    if (compte) E.enCours += 1;
    try {
      const r = await f(...a);
      if (!r.ok) E.refus.push(url.replace(/^.*\\/api\\//, '') + ' ' + r.status);
      if (compte) E.requetes.push({ nom, corps: a[1] && typeof a[1].body === 'string' ? a[1].body : null, statut: r.status });
      return r;
    } finally { if (compte) E.enCours -= 1; }
  };
})()`;

// Helpers kept in the page: texts by their key in the interface's language, elements by their text.
const OUTILS = `(async () => {
  if (window.__n) return true;
  const { t, langueActuelle } = await import('/i18n.js');
  const norm = (s) => String(s ?? '').replace(/\\s+/g, ' ').trim();
  const visible = (e) => !!e && e.isConnected && e.getClientRects().length > 0 && getComputedStyle(e).visibility !== 'hidden';
  window.__n = {
    t, langueActuelle, norm, visible,
    tous: (sel, dans = document) => [...dans.querySelectorAll(sel)].filter(visible),
    texte: (sel, texte, dans = document) => __n.tous(sel, dans).find((e) => norm(e.textContent) === norm(texte)),
    debut: (sel, texte, dans = document) => __n.tous(sel, dans).find((e) => norm(e.textContent).startsWith(norm(texte))),
    contient: (sel, texte, dans = document) => __n.tous(sel, dans).find((e) => norm(e.textContent).includes(norm(texte))),
    // the dialog in front, and its box
    dialogue: () => [...document.querySelectorAll('.fond-dialogue')].pop() || null,
    boite: () => { const f = __n.dialogue(); return f ? f.querySelector('.dialogue, .palette') : null; },
    titreDialogue: () => { const b = __n.boite(); const h = b && b.querySelector('.tete-dialogue h2'); return h ? norm(h.textContent) : ''; },
    // a Plotly plot by the start of its title, drawn (its data and axes there)
    graphe: (debut) => __n.tous('.js-plotly-plot').find((g) => g._fullLayout && g.data && norm(((g.layout || {}).title || {}).text).startsWith(norm(debut))) || null,
    points: (g) => (g && g.data ? g.data.reduce((s, tr) => s + (Array.isArray(tr.x) ? tr.x.length : Array.isArray(tr.y) ? tr.y.length : Array.isArray(tr.z) ? tr.z.length : 0), 0) : 0),
    // the tab of a panel, by its title
    // (with several projects open, a model's tab begins with its project: "skylark_counts · M1 · TRIM")
    onglet: (titre) => __n.tous('.dv-tab').find((e) => norm(e.textContent) === norm(titre))
      || __n.tous('.dv-tab').find((e) => norm(e.textContent).endsWith(' · ' + norm(titre))) || null,
    // the status bar's message
    statut: () => norm((document.querySelector('#statut .moteur') || {}).textContent),
    occupe: () => document.body.classList.contains('occupe') || window.__essai.enCours > 0,
    api: async (nom, q = {}) => {
      const r = await fetch('/api/' + nom, { method: 'POST', headers: { 'Content-Type': 'application/json', 'X-Langue': langueActuelle() }, body: JSON.stringify(q) });
      const j = await r.json();
      if (!r.ok) throw new Error(nom + ': ' + (j.erreur || r.status));
      return j;
    },
  };
  return true;
})()`;

const TOUCHES = { Enter: 13, Escape: 27, F1: 112 };

// A zip archive's files (an .xlsx, a .docx), read without anything else: name -> Buffer.
function lireZip(octets) {
  const b = Buffer.from(octets);
  let fin = b.length - 22;
  while (fin >= 0 && b.readUInt32LE(fin) !== 0x06054b50) fin -= 1;
  if (fin < 0) throw new Error('not a zip archive');
  const n = b.readUInt16LE(fin + 10);
  let k = b.readUInt32LE(fin + 16);
  const fichiers = new Map();
  for (let i = 0; i < n; i += 1) {
    const methode = b.readUInt16LE(k + 10);
    const taille = b.readUInt32LE(k + 20);
    const lnom = b.readUInt16LE(k + 28);
    const lextra = b.readUInt16LE(k + 30);
    const lcomm = b.readUInt16LE(k + 32);
    const local = b.readUInt32LE(k + 42);
    const nom = b.toString('utf8', k + 46, k + 46 + lnom);
    const debut = local + 30 + b.readUInt16LE(local + 26) + b.readUInt16LE(local + 28);
    const brut = b.subarray(debut, debut + taille);
    fichiers.set(nom, methode === 8 ? zlib.inflateRawSync(brut) : Buffer.from(brut));
    k += 46 + lnom + lextra + lcomm;
  }
  return fichiers;
}

// The sheets of an .xlsx, read back: [{ nom, lignes: [[cell, ...], ...] }], a cell a number, a
// text or null (enough for the files the engine writes: shared or inline strings, numbers).
function lireClasseur(chemin) {
  const z = lireZip(fs.readFileSync(chemin));
  const texte = (nom) => (z.has(nom) ? z.get(nom).toString('utf8') : '');
  const xml = (s) => s.replace(/<[^>]+>/g, '').replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&quot;/g, '"').replace(/&apos;/g, "'").replace(/&amp;/g, '&');
  const partagees = [...texte('xl/sharedStrings.xml').matchAll(/<si>([\s\S]*?)<\/si>/g)].map((m) => xml(m[1]));
  const liens = new Map([...texte('xl/_rels/workbook.xml.rels').matchAll(/<Relationship\b[^>]*>/g)]
    .map((m) => [/Id="([^"]+)"/.exec(m[0])[1], /Target="([^"]+)"/.exec(m[0])[1].replace(/^\/?(xl\/)?/, 'xl/')]));
  const colonne = (lettres) => [...lettres].reduce((s, c) => s * 26 + c.charCodeAt(0) - 64, 0) - 1;
  return [...texte('xl/workbook.xml').matchAll(/<sheet\b[^>]*>/g)].map((m) => {
    const nom = xml(/name="([^"]*)"/.exec(m[0])[1]);
    const lignes = [...texte(liens.get(/r:id="([^"]+)"/.exec(m[0])[1])).matchAll(/<row\b[^>]*>([\s\S]*?)<\/row>/g)].map((r) => {
      const ligne = [];
      for (const c of r[1].matchAll(/<c\b([^>]*?)(?:\/>|>([\s\S]*?)<\/c>)/g)) {
        const j = colonne(/r="([A-Z]+)\d+"/.exec(c[1])[1]);
        const type = (/t="(\w+)"/.exec(c[1]) || [])[1];
        const v = (/<v>([\s\S]*?)<\/v>/.exec(c[2] || '') || [])[1];
        ligne[j] = type === 's' ? partagees[Number(v)] : type === 'inlineStr' ? xml(c[2] || '') : type === 'str' ? xml(v ?? '') : v === undefined ? null : Number(v);
      }
      return Array.from(ligne, (x) => (x === undefined ? null : x));
    });
    return { nom, lignes };
  });
}

// An .xlsx of made-up sheets, written with the interface's own zip writer (stored, no compression):
// [[name, [[cells of the first row], ...]], ...]; a cell is a number, a text, or null.
async function ecrireClasseur(chemin, feuilles, dossierTemporaire) {
  const copie = path.join(dossierTemporaire, 'zip.mjs');  // (an ES module: imported from a copy named so)
  if (!fs.existsSync(copie)) fs.copyFileSync(path.join(__dirname, '..', '..', 'interface', 'zip.js'), copie);
  const { zip } = await import(require('url').pathToFileURL(copie));
  const o = (s) => new TextEncoder().encode(s);
  const x = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  const col = (j) => { let s = ''; for (let k = j + 1; k > 0; k = Math.floor((k - 1) / 26)) s = String.fromCharCode(65 + ((k - 1) % 26)) + s; return s; };
  const entete = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>';
  const ns = 'http://schemas.openxmlformats.org';
  const feuille = (lignes) => `${entete}<worksheet xmlns="${ns}/spreadsheetml/2006/main"><sheetData>${lignes.map((l, i) => `<row r="${i + 1}">${l.map((v, j) => {
    const ref = `${col(j)}${i + 1}`;
    if (v === null || v === undefined || v === '') return '';
    return typeof v === 'number' ? `<c r="${ref}"><v>${v}</v></c>` : `<c r="${ref}" t="inlineStr"><is><t>${x(v)}</t></is></c>`;
  }).join('')}</row>`).join('')}</sheetData></worksheet>`;
  const fichiers = [
    ['[Content_Types].xml', o(`${entete}<Types xmlns="${ns}/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>`
      + '<Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'
      + `${feuilles.map((_, i) => `<Override PartName="/xl/worksheets/sheet${i + 1}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>`).join('')}</Types>`)],
    ['_rels/.rels', o(`${entete}<Relationships xmlns="${ns}/package/2006/relationships"><Relationship Id="rId1" Type="${ns}/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>`)],
    ['xl/workbook.xml', o(`${entete}<workbook xmlns="${ns}/spreadsheetml/2006/main" xmlns:r="${ns}/officeDocument/2006/relationships"><sheets>`
      + `${feuilles.map(([nom], i) => `<sheet name="${x(nom)}" sheetId="${i + 1}" r:id="rId${i + 1}"/>`).join('')}</sheets></workbook>`)],
    ['xl/_rels/workbook.xml.rels', o(`${entete}<Relationships xmlns="${ns}/package/2006/relationships">`
      + `${feuilles.map((_, i) => `<Relationship Id="rId${i + 1}" Type="${ns}/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet${i + 1}.xml"/>`).join('')}</Relationships>`)],
    ...feuilles.map(([, lignes], i) => [`xl/worksheets/sheet${i + 1}.xml`, o(feuille(lignes))]),
  ];
  fs.writeFileSync(chemin, zip(fichiers));
  return chemin;
}

// Starts the program and connects to it. `dossier`: the run's temporary folder (the profile
// goes in it); `options`: the program's own options (--no-sandbox). Returns the object scenarios use.
async function demarrer({ programme, options = [], dossier, profil = path.join(dossier, 'profil'), premierLancement = true, journal = () => {} }) {
  const env = windows ? { ...process.env }
    // (as from the desktop menu or the Finder: no LANG, a short PATH; TMPDIR: macOS gives each user their own)
    : { ...Object.fromEntries(['HOME', 'DISPLAY', 'XAUTHORITY', 'WAYLAND_DISPLAY', 'XDG_RUNTIME_DIR', 'DBUS_SESSION_BUS_ADDRESS', 'TMPDIR']
      .filter((k) => process.env[k]).map((k) => [k, process.env[k]])), PATH: '/usr/bin:/bin' };
  // (no question to GitHub about a newer version: a closed local port answers instead)
  env.NUEE_SOURCE_MAJ = 'http://127.0.0.1:9/';
  // (a new profile: its engine on a port of its own, not on the one an installed Nuée takes when
  // it is free, whose interface settings are kept by that address: main.js, PORT_HABITUEL)
  const fichierReglages = path.join(profil, 'reglages.json');
  if (!fs.existsSync(fichierReglages)) {
    fs.mkdirSync(profil, { recursive: true });
    fs.writeFileSync(fichierReglages, JSON.stringify({ port: await portLibre() }));
  }
  const portPage = await portLibre();
  const portNode = await portLibre();
  const debut = Date.now();
  const horsEcran = HORS_ECRAN ? ['--disable-features=CalculateNativeWinOcclusion', '--disable-backgrounding-occluded-windows', '--disable-renderer-backgrounding'] : [];
  const proc = spawn(programme, [`--user-data-dir=${profil}`, `--remote-debugging-port=${portPage}`, `--inspect=${portNode}`, ...horsEcran, ...options],
    { env, stdio: ['ignore', 'pipe', 'pipe'], detached: !windows });
  let sortie = '';
  proc.stdout.on('data', (d) => { sortie += d; });
  proc.stderr.on('data', (d) => { sortie += d; });
  const arreter = () => {
    if (proc.exitCode !== null) return;
    try {
      if (windows) execFileSync('taskkill', ['/pid', String(proc.pid), '/T', '/F'], { stdio: 'ignore' });
      else process.kill(-proc.pid, 'SIGKILL');
    } catch { /* already gone */ }
  };
  arreterAvecLeRun(arreter);
  proc.on('exit', () => lances.delete(arreter));
  const vivant = () => proc.exitCode === null;
  const jusqua = async (f, ms, quoi) => {
    const fin = Date.now() + ms;
    for (;;) {
      const v = await f();
      if (v) return v;
      if (!vivant()) throw new Error(`the program stopped (code ${proc.exitCode}) while waiting for ${quoi}:\n${sortie.slice(-2000)}`);
      if (Date.now() > fin) throw new Error(`waited ${ms / 1000} s for ${quoi}`);
      await attente(200);
    }
  };
  // (a port that takes the connection and never answers counts as nothing yet: the waits stay theirs)
  const lister = async (port) => { try { return await (await fetch(`http://127.0.0.1:${port}/json/list`, { signal: AbortSignal.timeout(5000) })).json(); } catch { return null; } };

  try {
    // the main process first: its dialogs answered before the window can ask anything
    const cibleNode = await jusqua(async () => ((await lister(portNode)) || [])[0], 30000, 'the main process’s inspector');
    const principal = await session(cibleNode.webSocketDebuggerUrl);
    const evaluerPrincipal = async (expression) => {
      const r = await principal.envoyer('Runtime.evaluate', { expression, includeCommandLineAPI: true, returnByValue: true, awaitPromise: true });
      if (r.exceptionDetails) throw new Error(r.exceptionDetails.exception?.description || r.exceptionDetails.text);
      return r.result.value;
    };
    await jusqua(() => evaluerPrincipal(SUBSTITUTS).catch(() => false), 30000, 'the main process’s modules');
    await evaluerPrincipal(GARDIEN(process.pid));
    if (HORS_ECRAN) await evaluerPrincipal(SANS_ECRAN);

    // then the window (R starts first: up to two minutes)
    const cible = await jusqua(async () => ((await lister(portPage)) || []).find((c) => c.type === 'page' && /^http:\/\/127\.0\.0\.1:\d+\/(\?.*)?$/.test(c.url)),
      120000, 'the window');
    const erreurs = [];
    // a window's page made ready to be driven: its errors recorded in `erreurs`
    const brancher = async (c) => {
      const page = await session(c.webSocketDebuggerUrl);
      page.ecouter((j) => {
        if (j.method === 'Runtime.exceptionThrown') erreurs.push(j.params.exceptionDetails.exception?.description || j.params.exceptionDetails.text);
        if (j.method === 'Runtime.consoleAPICalled' && j.params.type === 'error') erreurs.push(j.params.args.map((a) => a.value ?? a.description ?? '').join(' '));
        // a question of the page (window.confirm: closing a project with unsaved changes...) that no
        // scenario expects: answered Cancel at once, so that nothing waits for it, and counted as an error
        if (j.method === 'Page.javascriptDialogOpening') {
          erreurs.push(`unexpected ${j.params.type}: ${j.params.message}`);
          page.envoyer('Page.handleJavaScriptDialog', { accept: false }).catch(() => {});
        }
      });
      await page.envoyer('Page.enable');
      await page.envoyer('Runtime.enable');
      // (without a window manager, as under Xvfb, the window may never get the focus the keys need)
      await page.envoyer('Emulation.setFocusEmulationEnabled', { enabled: true });
      // the window's page at the size of the window Nuée opens, whatever the screen (GitHub's
      // Windows machines have 1024 × 768): the same layout everywhere
      await page.envoyer('Emulation.setDeviceMetricsOverride', { width: 1400, height: 900, deviceScaleFactor: 1, mobile: false });
      await page.envoyer('Page.addScriptToEvaluateOnNewDocument', { source: ESPION });
      // the page was already loading: once more now, then the interface's own load
      await page.envoyer('Runtime.evaluate', { expression: `window.__essai || ${ESPION}` });
      return page;
    };
    const page = await brancher(cible);
    const pret = "document.querySelectorAll('.menu-haut').length > 3 && !!document.querySelector('#dock .arbre, #dock .accueil-explorateur')";

    const n = await piloter({ page, principal, evaluerPrincipal, erreurs, proc, vivant, arreter, sortie: () => sortie, dossier, programme, portPage, lister });
    await n.attendre(pret, 120000);
    await n.outils();
    // Another main window of the program (Window › New window), the first page whose address
    // `filtre` accepts, driven the same way once it shows the interface (its errors count with this
    // window's); detacher() lets it go, the program going on.
    const fenetrePrincipale = async (filtre) => {
      const c = await jusqua(async () => ((await lister(portPage)) || []).find((x) => x.type === 'page' && x.id !== cible.id && filtre(x.url)), ATTENTE, 'the other window');
      const autre = await brancher(c);
      const m = await piloter({ page: autre, principal, evaluerPrincipal, erreurs, proc, vivant, arreter, sortie: () => sortie, dossier, programme, portPage, lister });
      m.detacher = () => autre.fermer();
      await m.attendre(pret, 120000);
      await m.evaluer(`!!(window.__essai || ${ESPION})`);  // (in case its page was still the one before when it came)
      await m.outils();
      return m;
    };
    // (one n.fenetre: a test on the address finds another main window, a piece of it a view's window)
    const fenetreVue = n.fenetre;
    n.fenetre = (x, ...reste) => (typeof x === 'function' ? fenetrePrincipale(x, ...reste) : fenetreVue(x, ...reste));
    // a first start opens on the start page alone (What's new comes once after an update): once
    // the start has noted the version seen, no dialog is open
    if (premierLancement) {
      await n.attendre("localStorage.getItem('version_vue') !== null", 30000);
      const titre = await n.evaluer('__n.dialogue() ? __n.titreDialogue() || "(untitled)" : ""');
      if (titre) throw new Error(`a dialog opened on the first start: ${titre}`);
    }
    journal(`started in ${((Date.now() - debut) / 1000).toFixed(1)} s`);
    return n;
  } catch (e) {
    arreter();
    throw e;
  }
}

// The object scenarios use (see the top of this file).
async function piloter({ page, principal, evaluerPrincipal, erreurs, proc, vivant, arreter, sortie, dossier, programme, portPage, lister }) {
  const envoyer = page.envoyer;
  // Another window of the program (a view moved to a window of its own, ?vue=…), by a piece of its
  // address: its page with evaluer, attendre, a click (the pointer's events at an element's middle),
  // keys and typing, as the main window's; its page errors count with the main window's. Its
  // address as it is now: adresse().
  const fenetre = async (motif, ms = ATTENTE) => {
    const fin = Date.now() + ms;
    let cible = null;
    while (!cible) {
      cible = ((await lister(portPage)) || []).find((c) => c.type === 'page' && c.url.includes(motif)) || null;
      if (cible) break;
      if (!vivant()) throw new Error(`the program stopped while waiting for a window with ${motif}`);
      if (Date.now() > fin) throw new Error(`no window whose address has ${motif} within ${ms / 1000} s`);
      await attente(200);
    }
    const s = await session(cible.webSocketDebuggerUrl);
    s.ecouter((j) => {
      if (j.method === 'Runtime.exceptionThrown') erreurs.push(`(${motif}) ${j.params.exceptionDetails.exception?.description || j.params.exceptionDetails.text}`);
      if (j.method === 'Runtime.consoleAPICalled' && j.params.type === 'error') erreurs.push(`(${motif}) ${j.params.args.map((a) => a.value ?? a.description ?? '').join(' ')}`);
    });
    await s.envoyer('Runtime.enable');
    await s.envoyer('Emulation.setFocusEmulationEnabled', { enabled: true });
    const f = {
      async evaluer(expression) {
        const r = await s.envoyer('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true });
        if (r.exceptionDetails) throw new Error(`${r.exceptionDetails.exception?.description || r.exceptionDetails.text}\n  in (${motif}): ${expression.slice(0, 300)}`);
        return r.result.value;
      },
      async attendre(expression, delai = ATTENTE) {
        const limite = Date.now() + delai;
        let derniere = null;
        for (;;) {
          let v;
          try { v = await f.evaluer(expression); derniere = null; } catch (e) { v = false; derniere = e.message; }
          if (v) return v;
          if (!vivant()) throw new Error(`the program stopped while waiting (${motif}) for: ${expression.slice(0, 200)}`);
          if (Date.now() > limite) throw new Error(`waited ${delai / 1000} s (${motif}) for: ${expression.slice(0, 300)}${derniere ? `\n  (${derniere.split('\n')[0]})` : ''}`);
          await attente(100);
        }
      },
      adresse: () => f.evaluer('location.href'),
      async cliquer(expression, clics = 1) {
        const { x, y } = await f.attendre(`(() => { const e = ${expression}; if (!e || !e.getClientRects().length) return null; e.scrollIntoView({ block: 'nearest' });
          const r = e.getBoundingClientRect(); return { x: r.left + r.width / 2, y: r.top + r.height / 2 }; })()`);
        await s.envoyer('Input.dispatchMouseEvent', { type: 'mouseMoved', x, y });
        for (let k = 1; k <= clics; k += 1) {
          await s.envoyer('Input.dispatchMouseEvent', { type: 'mousePressed', x, y, button: 'left', buttons: 1, clickCount: k });
          await s.envoyer('Input.dispatchMouseEvent', { type: 'mouseReleased', x, y, button: 'left', buttons: 0, clickCount: k });
        }
      },
      taper: (texte) => s.envoyer('Input.insertText', { text: texte }),
      // a key with Ctrl (Ctrl+Z: 'z', 'KeyZ', 90)
      async ctrl(key, code, vk) {
        for (const type of ['rawKeyDown', 'keyUp']) await s.envoyer('Input.dispatchKeyEvent', { type, key, code, windowsVirtualKeyCode: vk, nativeVirtualKeyCode: vk, modifiers: 2 });
      },
      fermer: () => s.fermer(),
      // the window closed once the driver no longer listens to it (the requests its closing cuts, a
      // document's last words sent with keepalive among them, are no errors of the page), and gone
      // before the run goes on (a click meanwhile that closed another window lost its connection)
      async fermerFenetre() {
        await f.evaluer('setTimeout(() => window.close(), 50), true').catch(() => {});
        s.fermer();
        const fin = Date.now() + 5000;
        while (Date.now() < fin && ((await lister(portPage)) || []).some((c) => c.id === cible.id)) await attente(100);
      },
    };
    await f.attendre("document.readyState === 'complete'");
    await f.evaluer(`window.__essai || ${ESPION}`);
    await f.evaluer(OUTILS);
    return f;
  };
  const n = {
    fenetre,
    dossier, programme, proc, erreurs, sortie, arreter, vivant,
    async evaluer(expression) {
      const r = await envoyer('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true });
      if (r.exceptionDetails) throw new Error(`${r.exceptionDetails.exception?.description || r.exceptionDetails.text}\n  in: ${expression.slice(0, 300)}`);
      return r.result.value;
    },
    // until `expression` gives something true (returned); its errors count as false meanwhile
    // (NUEE_TRACE=1: the waits longer than a second, and how long)
    async attendre(expression, ms = ATTENTE) {
      const debut = Date.now();
      const fin = debut + ms;
      let derniere = null;
      for (;;) {
        let v;
        try { v = await n.evaluer(expression); derniere = null; } catch (e) { v = false; derniere = e.message; }
        if (v) {
          if (process.env.NUEE_TRACE && Date.now() - debut > 1000) console.log(`    ${((Date.now() - debut) / 1000).toFixed(1)} s: ${expression.replace(/\s+/g, ' ').slice(0, 110)}`);
          return v;
        }
        if (!vivant()) throw new Error(`the program stopped (code ${proc.exitCode}) while waiting for: ${expression.slice(0, 200)}`);
        if (Date.now() > fin) throw new Error(`waited ${ms / 1000} s for: ${expression.slice(0, 300)}${derniere ? `\n  (${derniere.split('\n')[0]})` : ''}`);
        await attente(100);
      }
    },
    outils: () => n.evaluer(OUTILS),
    t: (cle, valeurs) => n.evaluer(`__n.t(${JSON.stringify(cle)}${valeurs ? `, ${JSON.stringify(valeurs)}` : ''})`),

    // ---- the mouse and the keyboard
    // the middle of the element `expression` gives, once it is shown and nothing covers it
    // (an item of a menu: once the menu is in its place. On GitHub's Windows and macOS machines a
    // menu that opens is laid out at the window's top left corner, 0, 0 (where ui.js's placer()
    // first puts it), for some tens of milliseconds before it is under its title: an item measured
    // then was pressed where the menu bar, or the item above, was once the menu had moved.)
    async centre(expression, ms = ATTENTE) {
      return n.attendre(`(() => { const e = ${expression}; if (!e || !__n.visible(e)) return null;
        const m = e.closest && e.closest('.menu');
        if (m) { const q = m.getBoundingClientRect(); if (q.left < 1 && q.top < 1) return null; }
        e.scrollIntoView({ block: 'nearest', inline: 'nearest' });
        const r = e.getBoundingClientRect(); if (r.width < 1 || r.height < 1) return null;
        const x = r.left + r.width / 2, y = r.top + r.height / 2;
        const sous = document.elementFromPoint(x, y);
        return sous && (sous === e || e.contains(sous) || sous.contains(e)) ? { x, y } : null; })()`, ms);
    },
    // A click at the middle of the element `expression` gives: the pointer brought over it, and
    // pressed once the page says it is over that very element (its own record of the pointer,
    // ESPION); an element that moved meanwhile is measured again, and a press the page got on
    // another element stops the run there, saying on what (rather than a wait for what it
    // didn't do).
    async cliquer(expression, clics = 1, ms = ATTENTE) {
      const lu = `(() => { const s = window.__essai.souris; const e = ${expression}; if (!s) return null;
        const c = s.cible; const d = c && c.closest ? c.closest('[class], [id]') || c : c;
        return { type: s.type, x: s.x, y: s.y, dessus: !!e && !!c && (c === e || e.contains(c)),
          sur: d && d.tagName ? d.tagName.toLowerCase() + (d.id ? '#' + d.id : '') + (d.className && typeof d.className === 'string' ? '.' + d.className.trim().split(/\\s+/).join('.') : '')
            + ' “' + __n.norm(d.textContent).slice(0, 40) + '”' : String(c) }; })()`;
      let vu = null;
      for (let essai = 1; essai <= 6; essai += 1) {
        const c = await n.centre(expression, ms);
        const { x, y } = c;
        // (from a pixel away: a pointer already there would give the page no move to record; and
        // the page may get moves at its next frame only)
        const avant = await n.evaluer('window.__essai.mouvements');
        await envoyer('Input.dispatchMouseEvent', { type: 'mouseMoved', x: x - 1, y });
        await envoyer('Input.dispatchMouseEvent', { type: 'mouseMoved', x, y });
        await n.attendre(`window.__essai.mouvements > ${avant}`, 3000).catch(() => {});
        vu = await n.evaluer(lu);
        if (!vu || !vu.dessus) {
          console.log(`    (the pointer at ${Math.round(c.x)}, ${Math.round(c.y)} was over ${vu ? vu.sur : 'nothing the page saw'}, not over ${expression.replace(/\s+/g, ' ').slice(0, 90)}: measured again)`);
          await attente(150);
          continue;
        }
        for (let k = 1; k <= clics; k += 1) {
          await envoyer('Input.dispatchMouseEvent', { type: 'mousePressed', x, y, button: 'left', buttons: 1, clickCount: k });
          if (k === 1) vu = await n.evaluer(lu);
          await envoyer('Input.dispatchMouseEvent', { type: 'mouseReleased', x, y, button: 'left', buttons: 0, clickCount: k });
          if (k === 1 && !(vu && vu.type === 'mousedown' && vu.dessus)) {
            throw new Error(`pressed at ${Math.round(x)}, ${Math.round(y)} for ${expression.slice(0, 200)}, the page got it at ${vu ? `${Math.round(vu.x)}, ${Math.round(vu.y)} on ${vu.sur}` : 'no press'}`);
          }
        }
        return;
      }
      throw new Error(`the pointer never came over ${expression.slice(0, 200)}: the page had it at ${vu ? `${Math.round(vu.x)}, ${Math.round(vu.y)} over ${vu.sur}` : 'no move'}`);
    },
    double: (expression) => n.cliquer(expression, 2),
    taper: (texte) => envoyer('Input.insertText', { text: texte }),
    // a key: Enter, Escape (TOUCHES)
    async touche(key) {
      const vk = TOUCHES[key];
      for (const type of ['rawKeyDown', 'keyUp']) await envoyer('Input.dispatchKeyEvent', { type, key, code: key, windowsVirtualKeyCode: vk, nativeVirtualKeyCode: vk });
    },
    // a text field's text replaced by `texte`, typed (its input events), then left (its change)
    async ecrire(expression, texte, { quitter = true } = {}) {
      await n.cliquer(expression);
      await n.evaluer(`(() => { const e = ${expression}; e.focus(); if (e.select) e.select(); return true; })()`);
      // (typing replaces the selected text; the Delete key needs an editing command on macOS)
      if (texte) await n.taper(texte); else await n.evaluer("document.execCommand('delete')");
      if (quitter) await n.evaluer(`(() => { const e = ${expression}; if (e && e === document.activeElement) e.blur(); return true; })()`);
    },
    // an option of a <select>, as choosing it in its list does (input and change)
    async choisir(expression, valeur) {
      await n.centre(expression);
      const ok = await n.evaluer(`(() => { const s = ${expression}; const o = [...s.options].find((x) => x.value === ${JSON.stringify(String(valeur))});
        if (!o || o.disabled) return false; s.focus(); s.value = o.value;
        s.dispatchEvent(new Event('input', { bubbles: true })); s.dispatchEvent(new Event('change', { bubbles: true })); return true; })()`);
      if (!ok) throw new Error(`no option ${valeur} in: ${expression.slice(0, 200)}`);
    },
    // ... by the option's text
    async choisirTexte(expression, texte) {
      const v = await n.attendre(`(() => { const s = ${expression}; const o = s && [...s.options].find((x) => __n.norm(x.textContent) === ${JSON.stringify(texte)}); return o ? o.value : null; })()`);
      await n.choisir(expression, v);
    },
    async coche(expression, oui = true) {
      if ((await n.evaluer(`!!(${expression}).checked`)) !== oui) await n.cliquer(expression);
      await n.attendre(`(${expression}).checked === ${oui}`);
    },

    // ---- the interface
    // a menu of the menu bar, then one of its items (keys of their labels; an item whose label
    // goes on, "Undo “…”", by its start); waits until the item can be chosen
    async menu(cleMenu, cleItem) {
      const item = `(() => { const x = __n.t(${JSON.stringify(cleItem)}); const items = __n.tous('.menu:not(.sous-menu) > .item');
        const l = (e) => __n.norm((e.querySelector('.libelle') || e).textContent);
        return items.find((e) => l(e) === x) || items.find((e) => l(e).startsWith(x.replace(/…$/, ''))); })()`;
      const fin = Date.now() + ATTENTE;
      for (let rate = 0; ;) {
        try {
          await n.fermerMenus();
          await n.cliquer(`__n.texte('.menu-haut', __n.t(${JSON.stringify(cleMenu)}))`);
          const etat = await n.attendre(`(() => { const e = ${item}; return e ? (e.classList.contains('desactive') ? 'inactif' : 'pret') : null; })()`, 10000);
          if (etat === 'pret') { await n.cliquer(item, 1, 5000); return; }
          if (Date.now() > fin) throw new Error(`menu item still disabled: ${cleMenu} › ${cleItem}`);
          await attente(250);  // (not ready yet: the menu is opened again)
        } catch (e) {
          // (the menu gone before its item was pressed: opened again; a press that landed elsewhere
          // is not undone, and stops here)
          rate += 1;
          if (rate >= 3 || /^(pressed at|menu item still disabled)/.test(e.message)) throw e;
          console.log(`    (${cleMenu} › ${cleItem}: ${e.message.split('\n')[0].slice(0, 160)}; the menu opened again)`);
        }
      }
    },
    // ... and an item of one of its submenus (Help › Example projects › Apples), by their keys
    async sousMenu(cleMenu, cleItem, cleSous) {
      await n.menu(cleMenu, cleItem);
      const item = `(() => { const x = __n.t(${JSON.stringify(cleSous)}); return __n.tous('.menu.sous-menu > .item').find((e) => __n.norm((e.querySelector('.libelle') || e).textContent) === x); })()`;
      await n.attendre(`!!${item}`, 10000);
      await n.cliquer(item);
    },
    // (a press outside the menus closes them, ui.js)
    fermerMenus: () => n.evaluer("document.querySelectorAll('.menu').length ? (document.body.dispatchEvent(new MouseEvent('mousedown', { bubbles: true })), true) : true"),
    // a button of the dialog in front, by its label's key
    bouton: (cle) => n.cliquer(`(() => { const b = __n.boite(); return b && __n.tous('button', b).find((x) => __n.norm(x.textContent) === __n.norm(__n.t(${JSON.stringify(cle)})) && !x.disabled); })()`),
    // until the dialog in front is the one whose title starts with its key's text
    dialogue: (cle) => n.attendre(`__n.titreDialogue().startsWith(__n.t(${JSON.stringify(cle)}).replace(/…$/, ''))`),
    // the engine idle: nothing under way in the window
    libre: () => n.attendre('!__n.occupe()'),
    // a line of the explorer: the project, a model's definition or its fit, by its bold name
    ligneProjet: (nom) => `__n.tous('.arbre .noeud.n1').find((e) => __n.norm((e.querySelector('.libelle b') || {}).textContent) === ${JSON.stringify(nom)})`,
    ligneModele: (nom, calcule = true) => `__n.tous('.arbre .noeud.n3').find((e) => e.classList.contains('calcule') === ${calcule}
      && __n.norm((e.querySelector('.libelle b') || {}).textContent) === ${JSON.stringify(nom)})`,
    ligne: (texte, niveau) => `__n.tous('.arbre .noeud${niveau ? `.n${niveau}` : ''}').find((e) => __n.norm((e.querySelector('.libelle') || e).textContent).startsWith(${JSON.stringify(texte)}))`,
    // a Plotly plot of the window whose title starts with `debut`, drawn with `points` values at least
    graphe: (debut) => `__n.graphe(${JSON.stringify(debut)})`,
    async plotlyPret(debut, points = 1) {
      return n.attendre(`(() => { const g = __n.graphe(${JSON.stringify(debut)}); return g && g.querySelector('.main-svg') && __n.points(g) >= ${points} ? __n.points(g) : 0; })()`);
    },
    // a click on a point of a plot: its trace whose customdata holds `valeur` (an observation's
    // uid), the mouse brought over it until Plotly's hover finds it, then pressed
    async cliquerPoint(debut, valeur) {
      const g = n.graphe(debut);
      const pos = await n.attendre(`(() => { const g = ${g}; if (!g || !g._fullLayout) return null;
        for (const tr of g._fullData) {
          const k = [].concat(tr.customdata || []).findIndex((c) => c === ${JSON.stringify(valeur)});
          if (k < 0 || !tr.visible) continue;
          const xa = g._fullLayout[(tr.xaxis || 'x').replace('x', 'xaxis')], ya = g._fullLayout[(tr.yaxis || 'y').replace('y', 'yaxis')];
          const r = g.getBoundingClientRect();
          return { x: r.left + xa._offset + xa.l2p(xa.d2l(tr.x[k])), y: r.top + ya._offset + ya.l2p(ya.d2l(tr.y[k])) };
        } return null; })()`);
      await envoyer('Input.dispatchMouseEvent', { type: 'mouseMoved', x: pos.x - 3, y: pos.y - 3 });
      await envoyer('Input.dispatchMouseEvent', { type: 'mouseMoved', x: pos.x, y: pos.y });
      await n.attendre(`(() => { const g = ${g}; return !!(g && g._hoverdata && g._hoverdata.some((p) => p.customdata === ${JSON.stringify(valeur)})); })()`, 10000);
      await envoyer('Input.dispatchMouseEvent', { type: 'mousePressed', x: pos.x, y: pos.y, button: 'left', buttons: 1, clickCount: 1 });
      await envoyer('Input.dispatchMouseEvent', { type: 'mouseReleased', x: pos.x, y: pos.y, button: 'left', buttons: 0, clickCount: 1 });
      await envoyer('Input.dispatchMouseEvent', { type: 'mouseMoved', x: 2, y: 2 });  // (the hover label leaves the plot)
    },

    // Model › New model, filled in as a person does: on its General tab the type and, when given,
    // the cross-validation groups (an option's text: a variable's name); on its Variables tab
    // every variable X, then `y` as Y and `sans` without a role; then Fit, until the model is in
    // the explorer. Returns the fit as the engine has it.
    async nouveauModele({ type, y = [], sans = [], vc = null }) {
      const boite = '__n.boite()';
      await n.menu('m_modele', 'nouveau_modele');
      await n.dialogue('nouveau_modele');
      const onglet = (cle) => n.cliquer(`__n.tous('.onglets button', ${boite}).find((b) => __n.norm(b.textContent) === __n.t('${cle}'))`);
      await onglet('general');
      await n.choisir(`__n.tous('select', ${boite}).find((s) => [...s.options].some((o) => o.value === 'PCA'))`, type);
      if (vc) await n.choisirTexte(`__n.tous('select', ${boite}).find((s) => [...s.options].some((o) => o.value === 'ordre'))`, vc);
      await onglet('variables');
      await n.choisir(`__n.tous('.onglet-vars .outils select', ${boite})[0]`, 'x');
      const role = (nom) => `__n.tous('.onglet-vars tbody tr', ${boite}).find((r) => __n.norm(r.cells[0].textContent) === ${JSON.stringify(nom)}).cells[2].querySelector('select')`;
      for (const v of y) await n.choisir(role(v), 'y');
      for (const v of sans) await n.choisir(role(v), '');
      const avant = (await n.etat()).modeles.map((m) => m.id);
      await n.bouton('ajuster');
      await n.attendre('!__n.dialogue()', 180000);
      const id = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles).find((x) => !${JSON.stringify(avant)}.includes(x.id)); return m ? m.id : null; })`);
      const m = await n.api('modele', { modele: id });
      await n.attendre(n.ligneModele(m.nom, true));
      return m;
    },

    // ---- the engine
    api: (nom, q = {}) => n.evaluer(`__n.api(${JSON.stringify(nom)}, ${JSON.stringify(q)})`),
    async etat() {
      const e = await n.api('etat');
      return { projets: [].concat(e.projets || []), modeles: [].concat(e.modeles || []) };
    },
    // the latest fit of the model `nom` of the project `pid` (the engine's answer)
    async modele(nom, pid = null) {
      const e = await n.etat();
      const m = e.modeles.filter((x) => x.nom === nom && (!pid || x.projet === pid)).pop();
      return m ? n.api('modele', { modele: m.id }) : null;
    },
    // how many requests the window has sent (those after it: window.__essai.requetes.slice(k))
    nombreRequetes: () => n.evaluer('window.__essai.requetes.length'),

    // ---- the system's dialogs, answered by the main process
    fichierAOuvrir: (chemin) => evaluerPrincipal(`(__essai.ouvrir.push(${JSON.stringify(chemin)}), true)`),
    fichierAEnregistrer: (chemin) => evaluerPrincipal(`(__essai.enregistrer.push(${JSON.stringify(chemin)}), true)`),
    reponseMessage: (i) => evaluerPrincipal(`(__essai.messages.push(${Number(i)}), true)`),
    principal: () => evaluerPrincipal('JSON.parse(JSON.stringify(__essai))'),
    // every dialog asked for was answered as the scenario planned
    async dialoguesRepondus() {
      const e = await n.principal();
      return { restent: e.ouvrir.length + e.enregistrer.length + e.messages.length, sansReponse: e.appels.filter((a) => a.reponse === null && a.dialogue !== 'message') };
    },

    // ---- files
    lireZip,
    lireClasseur,
    ecrireClasseur: (chemin, feuilles) => ecrireClasseur(chemin, feuilles, dossier),
    async capture(fichier) {
      const r = await envoyer('Page.captureScreenshot', { format: 'png' });
      fs.mkdirSync(path.dirname(fichier), { recursive: true });
      fs.writeFileSync(fichier, Buffer.from(r.data, 'base64'));
    },

    // ---- between scenarios: no dialog, no project; the window loaded again, as new
    async remettreAZero() {
      await n.evaluer("document.querySelectorAll('.menu').forEach((m) => m.remove()); true").catch(() => {});
      for (let i = 0; i < 5 && (await n.evaluer('!!__n.dialogue()').catch(() => false)); i += 1) await n.touche('Escape');
      await n.attendre('!__n.occupe()').catch(() => {});
      const e = await n.etat();
      for (const p of e.projets) await n.api('fermer_projet', { projet: p.id });
      // (their recovery copies too: the reload would offer them as an earlier stop's)
      await n.evaluer('window.bureau.secoursIndex([])');
      // (and the main process's answers and records: a scenario stopped before its dialog would
      // hand its file to the next scenario's)
      await evaluerPrincipal('(Object.values(__essai).forEach((a) => { if (Array.isArray(a)) a.length = 0; }), true)');
      let fin;
      const charge = new Promise((ok, ko) => {
        const ecoute = page.ecouter((j) => { if (j.method === 'Page.loadEventFired') { ecoute(); ok(); } });
        fin = setTimeout(() => { ecoute(); ko(new Error(`the window didn't load again within ${ATTENTE / 1000} s`)); }, ATTENTE);
      });
      await envoyer('Page.reload', { ignoreCache: false });
      await charge.finally(() => clearTimeout(fin));
      await n.attendre("document.querySelectorAll('.menu-haut').length > 3 && !!document.querySelector('#dock .accueil-explorateur')");
      await n.outils();
      await n.attendre('!__n.occupe()');
    },

    // Quits as File › Quit does (answering "Close without saving" if asked), then waits for the
    // program to end.
    async quitter() {
      if (!vivant()) return false;
      await n.reponseMessage(1);
      // (the inspector let go first: Node waits for its debugger to leave before it ends)
      principal.fermer();
      n.evaluer('window.bureau.quitter()').catch(() => {});  // (no answer: the page goes with the program)
      const debut = Date.now();
      while (vivant() && Date.now() < debut + 30000) await attente(200);
      if (process.env.NUEE_TRACE) console.log(`    quit in ${((Date.now() - debut) / 1000).toFixed(1)} s`);
      page.fermer();
      return !vivant();
    },
    fermer() { page.fermer(); principal.fermer(); arreter(); },
  };
  return n;
}

module.exports = { demarrer };
