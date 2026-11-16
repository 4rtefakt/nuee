// Accessibility check of the interface: axe-core (a devDependency) on its main screens, in the light
// and dark themes, then the keyboard alone: the menu bar from Alt, a dialog keeping the focus and giving
// it back, the explorer's tree, a table's grid, the plots' text. In Electron (the app's own Chromium,
// from node_modules) on an engine started here, with a temporary profile, its window hidden.
//   node tests/essai_accessibilite.js [Rscript]
// Needs Node 22 or later (fetch, WebSocket); on Linux, a display (xvfb-run, as essai_appli.js).
// Exits with 1 when a check fails. This file is also the Electron side: Electron runs it as its main.
const fs = require('fs');
const path = require('path');

if (process.versions.electron) {
  // ---- in Electron: the window, on the page given, nothing else (no menu taking keys such as Alt or Ctrl+W)
  const { app, BrowserWindow, Menu } = require('electron');
  const arg = (k) => (process.argv.find((a) => a.startsWith(`--${k}=`)) || '').slice(k.length + 3);
  app.setPath('userData', arg('profil'));
  Menu.setApplicationMenu(null);
  app.whenReady().then(() => {
    const w = new BrowserWindow({ width: 1400, height: 900, show: false, paintWhenInitiallyHidden: true,
      webPreferences: { contextIsolation: true, sandbox: true, backgroundThrottling: false } });
    w.loadURL(arg('adresse'));
  });
  app.on('window-all-closed', () => app.quit());
  return;
}

const { spawn, execFileSync } = require('child_process');
const net = require('net');
const os = require('os');

const depot = path.join(__dirname, '..', '..');
const interfaceDir = path.join(depot, 'app', 'interface');
// (as essai_api.js: bin\x64\Rscript.exe is R itself)
const lanceur = process.argv[2] || process.env.RSCRIPT
  || (process.platform === 'win32' ? path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe') : 'Rscript');
const rscript = fs.existsSync(path.join(path.dirname(lanceur), 'x64', 'Rscript.exe')) ? path.join(path.dirname(lanceur), 'x64', 'Rscript.exe') : lanceur;
const AXE = fs.readFileSync(require.resolve('axe-core/axe.min.js'), 'utf8');
const dossier = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-accessibilite-'));
const pause = (ms) => new Promise((r) => setTimeout(r, ms));
const echecs = [];

function portLibre() {
  return new Promise((ok) => { const s = net.createServer(); s.listen(0, '127.0.0.1', () => { const { port } = s.address(); s.close(() => ok(port)); }); });
}
function tuer(proc) {
  if (!proc || proc.exitCode !== null) return;
  try {
    if (process.platform === 'win32') execFileSync('taskkill', ['/pid', String(proc.pid), '/T', '/F'], { stdio: 'ignore' });
    else proc.kill('SIGKILL');
  } catch { /* already gone */ }
}
async function etape(nom, f) {
  try {
    await f();
    console.log(`  ok      ${nom}`);
  } catch (e) {
    echecs.push(nom);
    console.log(`  ÉCHEC   ${nom} : ${e.message}`);
  }
}
const verifier = (condition, message) => { if (!condition) throw new Error(message); };

// The page over the Chrome DevTools Protocol: evaluate, wait for, keys.
async function brancher(cible) {
  const ws = new WebSocket(cible.webSocketDebuggerUrl);
  await new Promise((ok, ko) => { ws.onopen = ok; ws.onerror = ko; });
  let n = 0;
  const attente = new Map();
  const erreurs = [];
  ws.onmessage = (m) => {
    const j = JSON.parse(m.data);
    if (j.id && attente.has(j.id)) { attente.get(j.id)(j); attente.delete(j.id); return; }
    if (j.method === 'Runtime.exceptionThrown') erreurs.push(j.params.exceptionDetails.exception?.description || j.params.exceptionDetails.text);
  };
  const envoyer = (method, params = {}) => new Promise((ok) => { n += 1; attente.set(n, ok); ws.send(JSON.stringify({ id: n, method, params })); });
  await envoyer('Runtime.enable');
  await envoyer('Emulation.setFocusEmulationEnabled', { enabled: true });  // (a hidden window has the keyboard's focus)
  const VK = { Enter: 13, Escape: 27, Tab: 9, ArrowLeft: 37, ArrowUp: 38, ArrowRight: 39, ArrowDown: 40, Home: 36, End: 35, F1: 112, F6: 117, F10: 121, Alt: 18 };
  const p = {
    erreurs,
    async evaluer(expression) {
      const r = await envoyer('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true });
      if (r.result.exceptionDetails) throw new Error(r.result.exceptionDetails.exception?.description || r.result.exceptionDetails.text);
      return r.result.result.value;
    },
    async attendre(expression, ms = 60000) {
      const fin = Date.now() + ms;
      for (;;) {
        let v;
        try { v = await p.evaluer(expression); } catch { v = false; }
        if (v) return v;
        if (Date.now() > fin) throw new Error(`waited ${ms / 1000} s for: ${expression}`);
        await pause(200);
      }
    },
    // a key, with modifiers: 'Alt', 'Alt+h', 'Control+n', 'Shift+Tab'
    async touche(combo) {
      const parts = combo.split('+');
      const key = parts.pop();
      const mods = (parts.includes('Alt') ? 1 : 0) | (parts.includes('Control') ? 2 : 0) | (parts.includes('Shift') ? 8 : 0);
      const lettre = key.length === 1;
      const base = { key, code: lettre ? `Key${key.toUpperCase()}` : key === 'Alt' ? 'AltLeft' : key, windowsVirtualKeyCode: lettre ? key.toUpperCase().charCodeAt(0) : VK[key] };
      await envoyer('Input.dispatchKeyEvent', { type: lettre && !mods ? 'keyDown' : 'rawKeyDown', ...base, modifiers: key === 'Alt' ? 1 : mods, ...(lettre && !mods ? { text: key } : {}) });
      await envoyer('Input.dispatchKeyEvent', { type: 'keyUp', ...base, modifiers: key === 'Alt' ? 0 : mods });
      await pause(120);
    },
    // what has the keyboard: { role, nom, dans } (dans: menu bar, menu, dialog, tree, grid)
    focus: () => p.evaluer(`(() => { const a = document.activeElement; if (!a || a === document.body) return { role: 'body' };
      const dans = a.closest('.menu') ? 'menu' : a.closest('#menus') ? 'barre' : a.closest('.fond-dialogue') ? 'dialogue' : a.closest('[role="tree"]') ? 'arbre' : a.closest('[role="grid"]') ? 'grille' : '';
      return { role: a.getAttribute('role') || a.tagName.toLowerCase(), nom: (a.getAttribute('aria-label') || a.textContent || '').trim().slice(0, 60), dans }; })()`),
    async axe(nom) {
      await p.evaluer(AXE);
      const v = await p.evaluer(`axe.run(document, { resultTypes: ['violations'] }).then((r) => r.violations.map((v) => ({ id: v.id, tags: v.tags, n: v.nodes.length,
        cible: v.nodes.slice(0, 3).map((x) => x.target.join(' ')).join(' | ') })))`);
      const wcag = v.filter((x) => x.tags.some((t) => /^wcag\d/.test(t)));
      const autres = v.filter((x) => !wcag.includes(x));
      if (autres.length) console.log(`          (${nom}, best practice: ${autres.map((x) => `${x.id}×${x.n}`).join(', ')})`);
      verifier(!wcag.length, wcag.map((x) => `${x.id}×${x.n} [${x.cible}]`).join(' ; '));
    },
    fermer: () => ws.close(),
  };
  return p;
}

(async () => {
  console.log('Accessibilité de l’interface (axe-core, clavier)…');
  const port = await portLibre();
  const jeton = require('crypto').randomBytes(32).toString('hex');
  const serveur = spawn(rscript, [path.join(depot, 'moteur', 'serveur.R'), String(port), interfaceDir], { windowsHide: true, env: { ...process.env, NUEE_JETON: jeton } });
  let sortieR = '';
  await new Promise((ok, ko) => {
    serveur.stdout.on('data', (d) => { sortieR += d; if (sortieR.includes('PRET')) ok(); });
    serveur.stderr.on('data', (d) => { sortieR += d; });
    serveur.on('exit', (code) => ko(new Error(`R stopped (code ${code}): ${sortieR.slice(-800)}`)));
  });
  const portCDP = await portLibre();
  const profil = path.join(dossier, 'profil');
  const electron = spawn(require('electron'), [__filename, `--adresse=http://127.0.0.1:${port}/api/connexion?jeton=${jeton}`, `--profil=${profil}`,
    `--user-data-dir=${profil}`, `--remote-debugging-port=${portCDP}`, ...(process.platform === 'linux' && process.getuid && process.getuid() === 0 ? ['--no-sandbox'] : [])],
  { stdio: 'ignore', windowsHide: true });
  let p;
  try {
    let cible;
    for (let i = 0; i < 120 && !cible && electron.exitCode === null; i += 1) {
      await pause(500);
      try { cible = (await (await fetch(`http://127.0.0.1:${portCDP}/json/list`)).json()).find((c) => c.type === 'page' && c.url.startsWith(`http://127.0.0.1:${port}/`)); } catch { /* not yet */ }
    }
    if (!cible) throw new Error(`no Electron window (exit code ${electron.exitCode})`);
    p = await brancher(cible);
    await p.attendre("document.querySelectorAll('#menus .menu-haut').length > 3");
    const calme = () => p.attendre("!document.body.classList.contains('occupe') && !document.querySelector('.moteur.occupe')", 120000).then(() => pause(600));
    // a menu's command from the keyboard: Alt and its letter, then down to the item named so
    const commande = async (lettre, libelle) => {
      await p.touche(`Alt+${lettre}`);
      for (let i = 0; i < 60; i += 1) {
        if ((await p.evaluer(`(document.activeElement.querySelector('.libelle') || {}).textContent || ''`)).startsWith(libelle)) break;
        await p.touche('ArrowDown');
      }
      await p.touche('Enter');
      await calme();
    };
    const theme = (sombre) => p.evaluer(`(document.documentElement.dataset.theme === 'sombre') === ${sombre} || document.querySelector('#statut button[aria-pressed]').click()`);

    // ---- axe-core on the main screens, light then dark
    await p.attendre("!!document.querySelector('.fond-dialogue')", 10000).catch(() => {});  // What's new, at a first start
    if (await p.evaluer("!!document.querySelector('.fond-dialogue')")) {
      await etape('axe : Quoi de neuf (dialogue)', () => p.axe('quoi de neuf'));
      await etape('le dialogue a le clavier, Échap le ferme', async () => {
        verifier((await p.focus()).dans === 'dialogue', JSON.stringify(await p.focus()));
        verifier(await p.evaluer("document.getElementById('appli').inert"), 'the window behind is not inert');
        await p.touche('Escape');
        verifier(await p.evaluer("!document.querySelector('.fond-dialogue') && !document.getElementById('appli').inert"), 'still open');
      });
    }
    await etape('axe : accueil (clair)', () => p.axe('accueil'));
    await etape('barre de menus : Alt, les flèches, Échap', async () => {
      await p.touche('Alt');
      let f = await p.focus();
      verifier(f.dans === 'barre' && f.nom === 'File', `Alt: ${JSON.stringify(f)}`);
      await p.touche('ArrowRight');
      verifier((await p.focus()).nom === 'Edit', `→: ${JSON.stringify(await p.focus())}`);
      await p.touche('ArrowDown');
      f = await p.focus();
      verifier(f.dans === 'menu' && /^menuitem/.test(f.role), `↓: ${JSON.stringify(f)}`);
      verifier(await p.evaluer("document.querySelector('#menus [aria-expanded=\"true\"]').textContent === 'Edit'"), 'aria-expanded');
      await p.touche('Escape');
      verifier((await p.focus()).dans === 'barre', 'Escape: back to the menu bar');
      await p.touche('Escape');
      verifier(!(await p.focus()).dans, 'Escape: out of the menu bar');
    });
    await etape('Aide › Projets d’exemple › Pommes au clavier (Alt+H, Entrée ouvre le sous-menu)', async () => {
      // (Entrée sur Projets d’exemple ouvre son sous-menu, le clavier sur son premier élément, les pommes)
      await commande('h', 'Example projects');
      verifier((await p.evaluer("(document.activeElement.querySelector('.libelle') || {}).textContent || ''")).startsWith('Apples'), 'the submenu has the keyboard');
      await p.touche('Enter');
      await p.attendre("document.querySelectorAll('.js-plotly-plot').length >= 2", 60000);
      await calme();
    });
    await etape('axe : modèle, ses vues (clair)', () => p.axe('modele'));
    // F1 : le guide dans un panneau à côté du modèle, le clavier dans sa recherche ; Ctrl+W le ferme
    await etape('F1 : le guide à côté du modèle (axe, le clavier, Ctrl+W)', async () => {
      await p.touche('F1');
      await p.attendre("!!document.querySelector('.aide-corps h3') && document.activeElement === document.querySelector('.aide-recherche')", 30000);
      await p.axe('guide');
      await p.touche('Control+w');
      await p.attendre("!document.querySelector('.aide-panneau')", 5000);
    });
    await etape('chaque graphique a son texte', async () => {
      const g = await p.evaluer("[...document.querySelectorAll('.js-plotly-plot')].map((d) => [d.getAttribute('role'), d.getAttribute('aria-label') || '', d.tabIndex])");
      verifier(g.length >= 2, `${g.length} plots`);
      for (const [role, nom, tab] of g) verifier(role === 'figure' && nom.length > 10 && tab === 0, JSON.stringify([role, nom, tab]));
    });
    await etape('l’explorateur : un arbre au clavier', async () => {
      await p.evaluer("document.querySelector('[role=\"tree\"] [tabindex=\"0\"]').focus()");
      await p.touche('Home');
      let f = await p.focus();
      verifier(f.dans === 'arbre' && f.role === 'treeitem', JSON.stringify(f));
      const ouvert = () => p.evaluer("document.activeElement.getAttribute('aria-expanded')");
      verifier(await ouvert() === 'true', 'the project unfolded');
      await p.touche('ArrowLeft');
      verifier(await ouvert() === 'false', '← folds it');
      await p.touche('ArrowRight');
      verifier(await ouvert() === 'true', '→ unfolds it');
      await p.touche('ArrowDown');
      f = await p.focus();
      verifier(f.dans === 'arbre' && await p.evaluer("document.activeElement.getAttribute('aria-level')") === '2', `↓: ${JSON.stringify(f)}`);
    });
    await etape('un dialogue garde le clavier et le rend (Ctrl+N)', async () => {
      const avant = await p.focus();
      await p.touche('Control+n');
      await p.attendre("!!document.querySelector('.fond-dialogue')", 10000);
      await pause(500);
      await etape('axe : Nouveau modèle (clair)', () => p.axe('nouveau modele'));
      for (let i = 0; i < 40; i += 1) {
        await p.touche(i % 7 === 6 ? 'Shift+Tab' : 'Tab');
        verifier((await p.focus()).dans === 'dialogue', `Tab ${i + 1}: ${JSON.stringify(await p.focus())}`);
      }
      await p.touche('Escape');
      const apres = await p.focus();
      verifier(apres.dans === avant.dans && apres.nom === avant.nom, `back to ${JSON.stringify(avant)}, not ${JSON.stringify(apres)}`);
    });
    await etape('Graphiques › Données : une grille au clavier', async () => {
      await commande('p', 'Data');
      await p.attendre("!!document.querySelector('table[role=\"grid\"] [tabindex=\"0\"]')", 30000);
      await p.evaluer("document.querySelector('table[role=\"grid\"] [tabindex=\"0\"]').focus()");
      await p.touche('ArrowDown');
      await p.touche('ArrowRight');
      const c = await p.evaluer("(() => { const a = document.activeElement; return a.matches('td, th') ? [a.parentElement.rowIndex, a.cellIndex] : null; })()");
      verifier(c && c[0] === 2 && c[1] === 1, JSON.stringify(c));
    });
    await etape('axe : données (clair)', () => p.axe('donnees'));
    await etape('axe : document (clair)', async () => {
      await commande('f', 'New document');
      await p.attendre("!!document.querySelector('.fond-dialogue .principal')", 10000);
      await p.axe('dialogue document');
      await p.evaluer("document.querySelector('.fond-dialogue .principal').click()");
      // (!!: an element of the editor carries CodeMirror's own objects, which can't be returned by value)
      await p.attendre("!!document.querySelector('.cm-content')", 30000);
      await calme();
      await p.axe('document');
    });
    await theme(true);
    await pause(800);
    await etape('axe : document, données, modèle (sombre)', async () => {
      await p.axe('document sombre');
      await commande('p', 'Scores');
      await p.axe('modele sombre');
      await p.touche('Control+n');
      await p.attendre("!!document.querySelector('.fond-dialogue')", 10000);
      await pause(500);
      await p.axe('nouveau modele sombre');
      await p.touche('Escape');
    });
    await etape('pas d’erreur dans la page', () => verifier(!p.erreurs.length, p.erreurs.slice(0, 3).join(' ; ')));
  } catch (e) {
    echecs.push(e.message);
    console.log(`  ÉCHEC   ${e.message}`);
  } finally {
    if (p) p.fermer();
    tuer(electron);
    tuer(serveur);
    await pause(500);
    try { fs.rmSync(dossier, { recursive: true, force: true }); } catch { /* Electron may still hold a file a moment */ }
  }
  console.log(echecs.length ? `\n${echecs.length} échec(s).` : '\nTout est accessible sur ces écrans.');
  process.exit(echecs.length ? 1 : 0);
})();
