// Starts the built program and uses it over the Chrome DevTools Protocol, as a person would:
// the R engine starts (on Linux, the computer's R), the update is offered by a local stand-in for
// GitHub (never the real one), and chosen twice while it downloads, then refused by its SHA-256
// (Windows), the example project opens with its plots,
// a PLS model's views, the R console shows each fit's printout, a plot is saved as an image,
// Export to R offers all the session's models only with those a session script holds, a
// .nuee project given on the command line (what a double-click does) opens in the running
// window, a second main window stays when the first is closed, then Nuée quits from it, R stops
// and its temporary folder goes (on Windows, in a temporary
// folder whose path isn't ASCII, with a read-only file in it; an earlier run's goes at the start).
//   node tests/essai_appli.js <program> [screenshot.png] [program options...]
// On Linux the program starts as from the desktop menu: no LANG and a short PATH (/usr/bin:/bin),
// which once left the engine unable to read its own files. On a server, run it under xvfb-run.
// macOS, as from the Finder (no LANG either): <program> is Nuée.app/Contents/MacOS/Nuée, and the
// engine must run on the R in Nuée.app; a .nuee given to the Finder (open) opens in the window.
// Needs Node 22 or later (fetch, WebSocket). Exits with 1 when a check fails.
const { spawn, spawnSync, execFileSync } = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const net = require('net');
const os = require('os');
const path = require('path');

const [programme, capture, ...optionsProgramme] = process.argv.slice(2);
if (!programme) {
  console.error('node tests/essai_appli.js <program> [screenshot.png] [program options...]');
  process.exit(2);
}
const windows = process.platform === 'win32';
const pause = (ms) => new Promise((r) => setTimeout(r, ms));
const dossier = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-essai-'));
const profil = path.join(dossier, 'profil');
// (Windows: the program's temporary folder in a path that isn't ASCII, as a user named Clément has)
const temp = path.join(dossier, 'Temp de Clément');
if (windows) fs.mkdirSync(temp);

const mac = process.platform === 'darwin';
// (TMPDIR: macOS gives each user their own)
const garder = ['HOME', 'DISPLAY', 'XAUTHORITY', 'WAYLAND_DISPLAY', 'XDG_RUNTIME_DIR', 'DBUS_SESSION_BUS_ADDRESS', 'APPIMAGE_EXTRACT_AND_RUN', 'TMPDIR'];
const env = windows ? { ...process.env, TEMP: temp, TMP: temp }
  : { ...Object.fromEntries(garder.filter((k) => process.env[k]).map((k) => [k, process.env[k]])), PATH: '/usr/bin:/bin' };

// A stand-in for GitHub's latest release (main.js: NUEE_SOURCE_MAJ), so that the program never asks
// the real one: a 9.9.9 whose "installer" is random bytes under a SHA-256 they don't have, sent
// slowly. Nothing could install it.
const installateurFactice = crypto.randomBytes(1 << 20);
let telechargements = 0;
const serveurMaj = http.createServer(async (req, res) => {
  const ici = `http://127.0.0.1:${serveurMaj.address().port}`;
  if (req.url === '/latest') {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ tag_name: 'v9.9.9', html_url: `${ici}/page`, assets: [{ name: 'Nuee-Setup-9.9.9.exe', size: installateurFactice.length,
      browser_download_url: `${ici}/Nuee-Setup-9.9.9.exe`, digest: `sha256:${'0'.repeat(64)}` }] }));
  } else if (req.url === '/Nuee-Setup-9.9.9.exe') {
    telechargements += 1;
    res.writeHead(200, { 'Content-Type': 'application/octet-stream', 'Content-Length': installateurFactice.length });
    for (let i = 0; i < 20; i += 1) {
      res.write(installateurFactice.subarray(i * installateurFactice.length / 20, (i + 1) * installateurFactice.length / 20));
      await pause(100);
    }
    res.end();
  } else {
    res.writeHead(404);
    res.end();
  }
});

function portLibre() {
  return new Promise((ok, ko) => {
    const s = net.createServer();
    s.once('error', ko);
    s.listen(0, '127.0.0.1', () => { const { port } = s.address(); s.close(() => ok(port)); });
  });
}

// (detached on Linux: its own process group, so that a failed run can stop all of it)
const lancer = (port, args = [], cwd = undefined) => spawn(programme, [`--user-data-dir=${profil}`, `--remote-debugging-port=${port}`, ...optionsProgramme, ...args],
  { env, cwd, stdio: ['ignore', 'pipe', 'pipe'], detached: !windows });
function tuer(proc) {
  if (proc.exitCode !== null) return;
  try {
    if (windows) execFileSync('taskkill', ['/pid', String(proc.pid), '/T', '/F'], { stdio: 'ignore' });
    else process.kill(-proc.pid, 'SIGKILL');
  } catch { /* already gone */ }
}

// A process still running: on Linux, a zombie has stopped (in a container, the first process
// may not reap the R that Nuée left when it quit)
function enVie(pid) {
  if (process.platform === 'linux') {
    try { const s = fs.readFileSync(`/proc/${pid}/stat`, 'utf8'); return s[s.lastIndexOf(')') + 2] !== 'Z'; } catch { return false; }
  }
  try { process.kill(pid, 0); return true; } catch { return false; }
}

async function pages(port) {
  try { return (await (await fetch(`http://127.0.0.1:${port}/json/list`)).json()).filter((c) => c.type === 'page'); } catch { return []; }
}

// The page of the main window: evaluate, wait for, keys, text, screenshot.
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
    if (j.method === 'Runtime.consoleAPICalled' && j.params.type === 'error') erreurs.push(j.params.args.map((a) => a.value ?? a.description ?? '').join(' '));
  };
  const envoyer = (method, params = {}) => new Promise((ok) => { n += 1; attente.set(n, ok); ws.send(JSON.stringify({ id: n, method, params })); });
  await envoyer('Runtime.enable');
  // (without a window manager, as under Xvfb, the window may never get the focus the keys need)
  await envoyer('Emulation.setFocusEmulationEnabled', { enabled: true });
  const p = {
    erreurs,
    async evaluer(expression) {
      const r = await envoyer('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true });
      if (r.result.exceptionDetails) throw new Error(r.result.exceptionDetails.exception?.description || r.result.exceptionDetails.text);
      return r.result.result.value;
    },
    async attendre(expression, ms = 30000) {
      const fin = Date.now() + ms;
      for (;;) {
        let v;
        try { v = await p.evaluer(expression); } catch { v = false; }
        if (v) return v;
        if (Date.now() > fin) throw new Error(`waited ${ms / 1000} s for: ${expression}`);
        await pause(250);
      }
    },
    async touche(key, ctrl = false) {
      const vk = key.length === 1 ? key.toUpperCase().charCodeAt(0) : { Enter: 13 }[key];
      const code = key.length === 1 ? `Key${key.toUpperCase()}` : key;
      for (const type of ['rawKeyDown', 'keyUp']) await envoyer('Input.dispatchKeyEvent', { type, key, code, windowsVirtualKeyCode: vk, modifiers: ctrl ? 2 : 0 });
    },
    taper: (text) => envoyer('Input.insertText', { text }),
    api: (nom, q = {}) => p.evaluer(`fetch('/api/${nom}', { method: 'POST', body: ${JSON.stringify(JSON.stringify(q))} }).then((r) => r.json())`),
    async capture(fichier) {
      const r = await envoyer('Page.captureScreenshot', { format: 'png' });
      fs.writeFileSync(fichier, Buffer.from(r.result.data, 'base64'));
    },
    fermer: () => ws.close(),
  };
  return p;
}

(async () => {
  const res = {};
  const echecs = [];
  const verifier = (nom, ok, valeur) => { res[nom] = valeur === undefined ? ok : valeur; if (!ok) echecs.push(nom); };
  // an earlier stop left a recovery copy: the question about it is closed without an answer
  // (commande() removes the dialogs), so a normal quit must keep it for the next start
  const secours = path.join(profil, 'secours');
  const copieAncienne = path.join(secours, 'arret-precedent-P1.json');
  fs.mkdirSync(secours, { recursive: true });
  fs.writeFileSync(copieAncienne, '{}');
  fs.writeFileSync(path.join(secours, 'index.json'), JSON.stringify([{ fichier: copieAncienne, projet: 'P1', nom: 'arret precedent', date: new Date().toISOString() }]));
  // (Windows) R's folder of an earlier run whose process has ended, with a read-only file and
  // folder in it: the start removes it (main.js: nettoyerTemporairesR, supprimer)
  const ancienR = windows ? path.join(temp, `nuee-r-${spawnSync(process.execPath, ['-e', '']).pid}`) : null;
  if (ancienR) {
    fs.mkdirSync(path.join(ancienR, 'Rtmp', 'figé'), { recursive: true });
    fs.writeFileSync(path.join(ancienR, 'Rtmp', 'lecture seule.txt'), 'x');
    fs.chmodSync(path.join(ancienR, 'Rtmp', 'lecture seule.txt'), 0o444);
    fs.chmodSync(path.join(ancienR, 'Rtmp', 'figé'), 0o555);
  }
  // a project given to a second start while Nuée is still starting (its R folder made, no window
  // yet): it waits for the window's page, and opens there (main.js: montrerFenetre, aOuvrir)
  const auDemarrage = path.join(dossier, 'vergers.json');
  fs.copyFileSync(path.join(__dirname, '..', '..', 'exemple', 'vergers.json'), auDemarrage);
  const dossiersR = () => fs.readdirSync(windows ? temp : os.tmpdir()).filter((n) => n.startsWith('nuee-r-'));
  const avantR = new Set(dossiersR());
  await new Promise((ok) => serveurMaj.listen(0, '127.0.0.1', ok));
  env.NUEE_SOURCE_MAJ = `http://127.0.0.1:${serveurMaj.address().port}/latest`;
  const port = await portLibre();
  const proc = lancer(port);
  let sortie = '';
  proc.stdout.on('data', (d) => { sortie += d; });
  proc.stderr.on('data', (d) => { sortie += d; });
  let p;
  try {
    for (let i = 0; i < 600 && !dossiersR().some((n) => !avantR.has(n)) && proc.exitCode === null; i += 1) await pause(50);
    const secondAuDemarrage = lancer(await portLibre(), [auDemarrage]);
    let cible;
    for (let i = 0; i < 240 && !cible && proc.exitCode === null; i += 1) {  // R starts first: up to two minutes
      await pause(500);
      cible = (await pages(port)).find((c) => /^http:\/\/127\.0\.0\.1:\d+\//.test(c.url));
    }
    if (!cible) throw new Error(`no window (exit code ${proc.exitCode})`);
    p = await brancher(cible);
    await p.attendre("document.querySelectorAll('.menu-haut').length > 3", 60000);
    const ouvertAuDemarrage = await p.attendre("document.querySelector('.arbre').textContent.includes('vergers')", 30000).catch(() => false);
    verifier('fichierDonnePendantLeDemarrage', !!ouvertAuDemarrage && secondAuDemarrage.exitCode !== null, `${!!ouvertAuDemarrage} ${secondAuDemarrage.exitCode}`);
    tuer(secondAuDemarrage);
    for (const x of [].concat((await p.api('etat')).projets)) await p.api('fermer_projet', { projet: x.id });  // (the example project, below, is then the only one)
    await p.evaluer("new BroadcastChannel('nuee').postMessage({ type: 'modeles' })");
    await p.attendre("!(document.querySelector('.arbre') || { textContent: '' }).textContent.includes('vergers')", 20000);
    // the update, for an installed program (Help offers it; not from the sources), from the stand-in:
    // the version it offers, then (Windows) Update chosen twice while the installer downloads, as its
    // link stays in the status bar: one download, refused by its SHA-256, the same answer for both,
    // nothing left (main.js: mettre-a-jour; elsewhere the link opens the release page)
    if ((await p.evaluer('window.bureau.majAuto()')) !== null) {
      const v = await p.evaluer('window.bureau.nouvelleVersion(true)');
      const maj = windows ? await p.evaluer(`(async () => { const a = window.bureau.mettreAJour();
        await new Promise((r) => setTimeout(r, 500)); return Promise.all([a, window.bureau.mettreAJour()]); })()`) : [];
      verifier('miseAJour', !!v && v.version === '9.9.9' && (!windows || (!!v.installateur && telechargements === 1
        && maj.every((r) => r && r.erreur === 'empreinte') && !fs.existsSync(path.join(temp, 'nuee-mise-a-jour')))), { v, maj, telechargements });
    }
    // what goes wrong without a page error: a policy violation, a request the engine refuses
    await p.evaluer(`window.__violations = []; document.addEventListener('securitypolicyviolation', (e) => window.__violations.push(e.violatedDirective));
      window.__refus = []; const f = window.fetch;
      window.fetch = async (...a) => { const r = await f(...a); if (!r.ok) window.__refus.push(String(a[0]) + ' ' + r.status); return r; }`);
    const v = await p.api('version');
    res.R = `${v.R} (${v.systeme})`;
    const sorties = (r) => [].concat(r.journal || []).filter((e) => e.type === 'sortie').map((e) => e.texte).join('').trim();
    const pidR = Number(sorties(await p.api('console', { code: 'cat(Sys.getpid())' })));
    // the folder of this run (by its long name: R on Windows gives the short one of a path with spaces)
    let temporaire = sorties(await p.api('console', { code: 'cat(dirname(tempdir()))' }));
    try { temporaire = fs.realpathSync.native(temporaire); } catch { /* checked below */ }
    verifier('ancienDossierRSupprime', !ancienR || !fs.existsSync(ancienR), ancienR);
    // (a read-only file in it, as a file copied there from a read-only folder: quitting removes it too)
    await p.api('console', { code: 'f <- file.path(tempdir(), "lecture seule.txt"); writeLines("x", f); Sys.chmod(f, "444")' });
    res.localeR = sorties(await p.api('console', { code: 'cat(Sys.getlocale("LC_CTYPE"))' }));
    verifier('accentsLus', sorties(await p.api('console', { code: 'cat(nchar("Modèle"))' })) === '6');
    if (mac) {  // (not one CRAN's installer put in /Library/Frameworks)
      // (the é of Nuée.app may be one character or two, e and its accent: compared as one)
      const rHome = sorties(await p.api('console', { code: 'cat(normalizePath(R.home()), normalizePath(.libPaths()[1]), sep = "\\n")' })).normalize('NFC').split('\n');
      const r = fs.realpathSync(path.join(path.dirname(programme), '..', 'Resources', 'R')).normalize('NFC');
      verifier('RDeNueeApp', rHome[0] === r && rHome[1] === path.join(r, 'library'), rHome);
    }

    // the command palette (Ctrl+K), as a person would use it
    const commande = async (texte) => {
      await p.evaluer("document.querySelectorAll('.fond-dialogue').forEach((f) => f.remove()); document.activeElement && document.activeElement.blur()");
      for (let essai = 1; ; essai += 1) {
        await p.touche('k', true);
        try { await p.attendre("!!document.querySelector('.palette-saisie')", 5000); break; } catch (e) { if (essai === 3) throw e; }
      }
      await p.taper(texte);
      await pause(250);
      const trouve = await p.evaluer("(document.querySelector('.palette-item.actuel .palette-libelle') || {}).textContent || ''");
      await p.touche('Enter');
      return trouve;
    };
    const graphiques = () => p.evaluer("document.querySelectorAll('.js-plotly-plot').length");
    const doubleClic = (texte) => p.evaluer(`(() => { const x = [...document.querySelectorAll('.arbre *')].find((e) => e.children.length === 0 && e.textContent.trim() === ${JSON.stringify(texte)});
      if (!x) throw new Error('not in the tree: ${texte}'); x.dispatchEvent(new MouseEvent('dblclick', { bubbles: true })); })()`);

    // (each command by its name in the interface's language: English on a new profile)
    const libelle = (cle) => p.evaluer(`import('/i18n.js').then((m) => m.t(${JSON.stringify(cle)}))`);
    res.langue = await p.evaluer("import('/i18n.js').then((m) => m.langueActuelle())");
    res.exemple = await commande(await libelle('projet_exemple'));
    await p.attendre("document.querySelectorAll('.js-plotly-plot').length >= 2", 60000);
    for (const m of ['M2', 'M3', 'M4']) { await doubleClic(m); await pause(3000); }
    await doubleClic('M2');
    await pause(2000);
    res.vues = [];
    for (const vue of ['vip', 'coefficients', 'obspred', 'dmodx']) {
      res.vues.push(await commande(await libelle(vue)));
      await pause(2000);
    }
    verifier('vues', res.vues.every(Boolean) && (await graphiques()) >= 2, `${res.vues.join(', ')}: ${await graphiques()} plots`);
    const image = await p.evaluer(`(async () => { const g = document.querySelector('.js-plotly-plot');
      const url = await Plotly.toImage(g, { format: 'png', width: 400, height: 300 }); return (await (await fetch(url)).blob()).size; })()`);
    verifier('imageDuGraphique', image > 1000, image);

    // the R console: each fit with what R prints of it
    await p.touche('j', true);
    await p.attendre("document.querySelectorAll('.journal-r .r-resultat').length >= 3", 20000).catch(() => {});
    const console_ = await p.evaluer("[...document.querySelectorAll('.journal-r .r-resultat')].map((x) => x.textContent)");
    verifier('consoleAjustements', console_.length >= 3 && console_.every((t) => /Mod(èle|el) \S+ \(/.test(t)), console_.length);

    // Export to R's "all the session's models": only those a session script holds (PCA, PLS, OPLS and
    // regressions); another analysis is exported alone (serveur.R: script_demande), so its own dialog
    // doesn't offer it. A k-means of the example project fitted here, the window told as another
    // window would tell it.
    const exemple = [].concat((await p.api('etat')).projets)[0];  // (the only project open)
    const variables = [].concat((await p.api('projet', { projet: exemple.id })).variables).filter((v) => !v.qualitative).slice(0, 3);
    await p.api('ajuster', { projet: exemple.id, type: 'KMEANS', x: variables.map((v) => v.uid), echelle: 'UV', options: { k: 3 }, nom: 'K1' });
    await p.evaluer("new BroadcastChannel('nuee').postMessage({ type: 'modeles' })");
    await p.attendre("[...document.querySelectorAll('.arbre *')].some((e) => e.children.length === 0 && e.textContent.trim() === 'K1')", 20000);
    const dialogueScript = async (modele) => {
      await doubleClic(modele);
      await pause(2000);
      await commande(await libelle('script_r'));
      await p.attendre("!!document.querySelector('.options-script')", 10000);
      return p.evaluer("({ seance: document.querySelectorAll('.options-script input[name=\"script-portee\"]').length, liste: [...document.querySelectorAll('.options-script .doux')].map((x) => x.textContent).join(' | ') })");
    };
    const dk = await dialogueScript('K1');
    const dm = await dialogueScript('M2');
    verifier('scriptSeanceSansAutreAnalyse', dk.seance === 0 && dm.seance === 2 && !/\bK1\b/.test(dm.liste), { k1: dk, m2: dm });
    // (the project, which K1 joined, saved: quitting below asks nothing)
    await p.evaluer("document.querySelectorAll('.fond-dialogue').forEach((f) => f.remove())");
    await p.api('enregistrer', { projet: exemple.id, chemin: path.join(dossier, 'exemple.nuee') });

    // a document in English, the interface in French: the table it cites is in English, words and
    // numbers, as its values are (documents.js, i18n.js: dansLangue); the references of the Methods
    // text it cites are in its list of references (bibliographie.js: joindreReferences)
    await p.evaluer("import('/i18n.js').then((m) => m.changerLangue('fr'))");
    await p.api('document_enregistrer', { projet: exemple.id, titre: 'Document EN', langue: 'en', texte: 'Q2 = {{M2.Q2}}.\n\n{{M2.table.resume | Summary}}\n\n{{M2.methodes}}\n' });
    await p.evaluer("new BroadcastChannel('nuee').postMessage({ type: 'modeles' })");  // (as another window says it: the explorer lists it)
    await p.attendre("document.querySelector('.arbre').textContent.includes('Document EN')", 20000);
    await doubleClic('Document EN');
    const tableDocument = await p.attendre("(() => { const a = document.querySelector('.apercu-document table'); return a && [...a.querySelectorAll('th, td')].map((x) => x.textContent); })()", 60000)
      .catch(() => null);
    verifier('documentDansSaLangue', !!tableDocument && tableDocument.includes('Eigenvalue') && tableDocument.some((x) => /^0\.\d{3}$/.test(x)) && !tableDocument.some((x) => /\d,\d/.test(x)),
      tableDocument && tableDocument.slice(0, 16));
    const listeDocument = await p.attendre("(() => { const l = [...document.querySelectorAll('.apercu-document .bibliographie p')].map((x) => x.textContent); return l.length && l; })()", 30000)
      .catch(() => null);
    verifier('documentReferencesDesMethodes', !!listeDocument && listeDocument.some((x) => x.startsWith('Wold, S., Sjöström, M., & Eriksson, L. (2001). PLS-regression'))
      && listeDocument.some((x) => x.startsWith('R Core Team (')), listeDocument);
    await p.evaluer("import('/i18n.js').then((m) => m.changerLangue('en'))");
    await p.api('enregistrer', { projet: exemple.id, chemin: path.join(dossier, 'exemple.nuee') });  // (Quit then has nothing to ask)

    // a .nuee project given to a second start (a double-click): it opens in this window
    const nom = 'essai-ligne-de-commande';
    const colle = await p.api('coller', { texte: 'nom\ta\tb\no1\t1\t2\no2\t3\t5\no3\t4\t4\n', nom });
    const fichier = path.join(dossier, `${nom}.nuee`);
    await p.api('enregistrer', { projet: colle.id, chemin: fichier });
    await p.api('fermer_projet', { projet: colle.id });
    const portReglages = () => { try { return JSON.parse(fs.readFileSync(path.join(profil, 'reglages.json'), 'utf8')).port; } catch { return null; } };
    const portAvant = portReglages();
    const second = lancer(await portLibre(), [fichier]);
    await new Promise((ok) => { second.on('exit', ok); setTimeout(ok, 60000); });
    const ouvert = await p.attendre(`document.querySelector('.arbre').textContent.includes(${JSON.stringify(nom)})`, 20000).catch(() => false);
    verifier('fichierOuvertParUnSecondLancement', !!ouvert && second.exitCode !== null);
    // (that start only handed its file over: no R of its own, the settings' port left as it was)
    const rDuSecond = fs.readdirSync(windows ? temp : os.tmpdir()).filter((n) => new RegExp(`^nuee-r-(\\d+-)?${second.pid}(-|$)`).test(n));
    verifier('secondLancementSansMoteur', !rDuSecond.length && portReglages() === portAvant, { rDuSecond, portAvant, portApres: portReglages() });
    tuer(second);
    // the same by its name alone, from its folder, as a command line in that folder gives it (main.js:
    // fichierDonne, from the second start's working folder)
    for (const x of [].concat((await p.api('etat')).projets).filter((y) => y.nom.includes(nom))) await p.api('fermer_projet', { projet: x.id });
    await p.evaluer("new BroadcastChannel('nuee').postMessage({ type: 'modeles' })");
    await p.attendre(`!document.querySelector('.arbre').textContent.includes(${JSON.stringify(nom)})`, 20000);
    const relatif = lancer(await portLibre(), [path.basename(fichier)], dossier);
    await new Promise((ok) => { relatif.on('exit', ok); setTimeout(ok, 60000); });
    verifier('fichierRelatifOuvertParUnSecondLancement', !!(await p.attendre(`document.querySelector('.arbre').textContent.includes(${JSON.stringify(nom)})`, 20000).catch(() => false)));
    tuer(relatif);
    // macOS: a .nuee double-clicked in the Finder comes as an event (open-file), not as an argument
    if (mac) {
      const nomFinder = 'essai-finder';
      const c = await p.api('coller', { texte: 'nom\ta\tb\no1\t1\t2\no2\t3\t5\no3\t4\t4\n', nom: nomFinder });
      const f = path.join(dossier, `${nomFinder}.nuee`);
      await p.api('enregistrer', { projet: c.id, chemin: f });
      await p.api('fermer_projet', { projet: c.id });
      execFileSync('open', ['-a', path.resolve(programme, '..', '..', '..'), f]);
      verifier('fichierOuvertParLeFinder', !!(await p.attendre(`document.querySelector('.arbre').textContent.includes(${JSON.stringify(nomFinder)})`, 20000).catch(() => false)));
    }

    verifier('sansViolationCSP', (await p.evaluer('window.__violations')).length === 0, await p.evaluer('window.__violations'));
    verifier('sansRefusDuMoteur', (await p.evaluer('window.__refus')).length === 0, await p.evaluer('window.__refus'));
    if (capture) await p.capture(capture);

    // Window › New window: a second main window, which outlives the first (main.js: outlivesOpener).
    // Closing the first one leaves Nuée running in the second, from which it quits below.
    await commande(await libelle('nouvelle_fenetre'));
    let cible2;
    for (let i = 0; i < 60 && !cible2; i += 1) { await pause(500); cible2 = (await pages(port)).find((c) => /[?&]fenetre=/.test(c.url)); }
    if (!cible2) throw new Error('no second main window');
    const p2 = await brancher(cible2);
    await p2.attendre("document.querySelectorAll('.menu-haut').length > 3", 60000);
    const erreurs1 = p.erreurs;
    await p.evaluer('setTimeout(() => window.close(), 100); true').catch(() => {});
    await pause(3000);
    p.fermer();
    p = p2;
    verifier('secondeFenetreGardee', proc.exitCode === null && (await p.evaluer("!!document.querySelector('.arbre')").catch(() => false)),
      `${proc.exitCode} ${(await pages(port)).map((c) => c.url).join(' ')}`);
    // and the recovery copies: the second window writes them now, every two minutes (app.js:
    // sauvegarderSecours), keeping the earlier stop's that waits (checked after quitting)
    const nomCopie = 'essai-copie-de-secours';
    const pc = await p.api('coller', { texte: 'nom\ta\tb\no1\t1\t2\no2\t3\t5\n', nom: nomCopie });
    await p.evaluer("new BroadcastChannel('nuee').postMessage({ type: 'modeles' })");
    const copie = () => { try { return JSON.parse(fs.readFileSync(path.join(secours, 'index.json'), 'utf8')).find((e) => e.nom === nomCopie && fs.existsSync(e.fichier)); } catch { return undefined; } };
    for (let i = 0; i < 150 && !copie() && proc.exitCode === null; i += 1) await pause(1000);
    verifier('copieDeSecoursDeLaSecondeFenetre', !!copie(), fs.existsSync(path.join(secours, 'index.json')) ? fs.readFileSync(path.join(secours, 'index.json'), 'utf8') : 'no index');
    await p.api('fermer_projet', { projet: pc.id });  // (quitting then has nothing to ask)

    // quitting stops R and removes its temporary folder
    verifier('dossierTemporaireR', /nuee-r-[\d-]+$/.test(temporaire) && fs.existsSync(temporaire), temporaire);
    await Promise.race([p.evaluer('window.bureau.quitter()').catch(() => {}), pause(3000)]);  // the page goes with the app
    for (let i = 0; i < 60 && proc.exitCode === null; i += 1) await pause(250);
    verifier('quitte', proc.exitCode !== null, proc.exitCode);
    await pause(1000);
    verifier('RArrete', pidR > 0 && !enVie(pidR), pidR);
    verifier('dossierTemporaireRSupprime', !fs.existsSync(temporaire));
    let index = [];
    try { index = JSON.parse(fs.readFileSync(path.join(secours, 'index.json'), 'utf8')); } catch { index = []; }
    verifier('copieDUnArretPrecedentGardee', fs.existsSync(copieAncienne) && index.length === 1 && path.basename(index[0].fichier) === path.basename(copieAncienne),
      `${fs.existsSync(copieAncienne)} / ${JSON.stringify(index)}`);
    verifier('sansErreurDansLaPage', !erreurs1.length && !p.erreurs.length, [...erreurs1, ...p.erreurs].slice(0, 10));
  } catch (e) {
    verifier('erreur', false, e.message);
    if (p && capture) try { await p.capture(capture.replace(/\.png$/i, '') + '-erreur.png'); } catch { /* no page */ }
  } finally {
    if (p) p.fermer();
    tuer(proc);
    serveurMaj.close();
    if (echecs.length) res.sortieProgramme = sortie.slice(-3000);
    console.log(JSON.stringify(res, null, 1));
    console.log(echecs.length ? `Échecs : ${echecs.join(', ')}` : 'Toutes les vérifications sont passées.');
    // (fs.rmSync of Node 24.12 leaves a path that isn't ASCII in place without an error)
    await fs.promises.rm(dossier, { recursive: true, force: true }).catch(() => {});
    process.exit(echecs.length ? 1 : 0);
  }
})();
