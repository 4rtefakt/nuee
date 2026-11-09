// Starts the built program and uses it over the Chrome DevTools Protocol, as a person would:
// the R engine starts (on Linux, the computer's R), the example project opens with its plots,
// a PLS model's views, the R console shows each fit's printout, a plot is saved as an image, a
// .nuee project given on the command line (what a double-click does) opens in the running
// window, then Nuée quits, R stops and its temporary folder goes.
//   node tests/essai_appli.js <program> [screenshot.png] [program options...]
// On Linux the program starts as from the desktop menu: no LANG and a short PATH (/usr/bin:/bin),
// which once left the engine unable to read its own files. On a server, run it under xvfb-run.
// Needs Node 22 or later (fetch, WebSocket). Exits with 1 when a check fails.
const { spawn, execFileSync } = require('child_process');
const fs = require('fs');
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

const garder = ['HOME', 'DISPLAY', 'XAUTHORITY', 'WAYLAND_DISPLAY', 'XDG_RUNTIME_DIR', 'DBUS_SESSION_BUS_ADDRESS', 'APPIMAGE_EXTRACT_AND_RUN'];
const env = windows ? process.env
  : { ...Object.fromEntries(garder.filter((k) => process.env[k]).map((k) => [k, process.env[k]])), PATH: '/usr/bin:/bin' };

function portLibre() {
  return new Promise((ok, ko) => {
    const s = net.createServer();
    s.once('error', ko);
    s.listen(0, '127.0.0.1', () => { const { port } = s.address(); s.close(() => ok(port)); });
  });
}

// (detached on Linux: its own process group, so that a failed run can stop all of it)
const lancer = (port, args = []) => spawn(programme, [`--user-data-dir=${profil}`, `--remote-debugging-port=${port}`, ...optionsProgramme, ...args],
  { env, stdio: ['ignore', 'pipe', 'pipe'], detached: !windows });
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
  if (!windows) {
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
  const port = await portLibre();
  const proc = lancer(port);
  let sortie = '';
  proc.stdout.on('data', (d) => { sortie += d; });
  proc.stderr.on('data', (d) => { sortie += d; });
  let p;
  try {
    let cible;
    for (let i = 0; i < 240 && !cible && proc.exitCode === null; i += 1) {  // R starts first: up to two minutes
      await pause(500);
      cible = (await pages(port)).find((c) => /^http:\/\/127\.0\.0\.1:\d+\//.test(c.url));
    }
    if (!cible) throw new Error(`no window (exit code ${proc.exitCode})`);
    p = await brancher(cible);
    await p.attendre("document.querySelectorAll('.menu-haut').length > 3", 60000);
    // what goes wrong without a page error: a policy violation, a request the engine refuses
    await p.evaluer(`window.__violations = []; document.addEventListener('securitypolicyviolation', (e) => window.__violations.push(e.violatedDirective));
      window.__refus = []; const f = window.fetch;
      window.fetch = async (...a) => { const r = await f(...a); if (!r.ok) window.__refus.push(String(a[0]) + ' ' + r.status); return r; }`);
    const v = await p.api('version');
    res.R = `${v.R} (${v.systeme})`;
    const sorties = (r) => [].concat(r.journal || []).filter((e) => e.type === 'sortie').map((e) => e.texte).join('').trim();
    const pidR = Number(sorties(await p.api('console', { code: 'cat(Sys.getpid())' })));
    const temporaire = sorties(await p.api('console', { code: 'cat(dirname(tempdir()))' }));  // the folder of this run
    res.localeR = sorties(await p.api('console', { code: 'cat(Sys.getlocale("LC_CTYPE"))' }));
    verifier('accentsLus', sorties(await p.api('console', { code: 'cat(nchar("Modèle"))' })) === '6');

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

    res.exemple = await commande('exemple');
    await p.attendre("document.querySelectorAll('.js-plotly-plot').length >= 2", 60000);
    for (const m of ['M2', 'M3', 'M4']) { await doubleClic(m); await pause(3000); }
    await doubleClic('M2');
    await pause(2000);
    res.vues = [];
    for (const vue of ['VIP', 'Coefficients', 'Observé', 'DModX']) {
      res.vues.push(await commande(vue));
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

    // a .nuee project given to a second start (a double-click): it opens in this window
    const nom = 'essai-ligne-de-commande';
    const colle = await p.api('coller', { texte: 'nom\ta\tb\no1\t1\t2\no2\t3\t5\no3\t4\t4\n', nom });
    const fichier = path.join(dossier, `${nom}.nuee`);
    await p.api('enregistrer', { projet: colle.id, chemin: fichier });
    await p.api('fermer_projet', { projet: colle.id });
    const second = lancer(await portLibre(), [fichier]);
    await new Promise((ok) => { second.on('exit', ok); setTimeout(ok, 60000); });
    const ouvert = await p.attendre(`document.querySelector('.arbre').textContent.includes(${JSON.stringify(nom)})`, 20000).catch(() => false);
    verifier('fichierOuvertParUnSecondLancement', !!ouvert && second.exitCode !== null);
    tuer(second);

    verifier('sansViolationCSP', (await p.evaluer('window.__violations')).length === 0, await p.evaluer('window.__violations'));
    verifier('sansRefusDuMoteur', (await p.evaluer('window.__refus')).length === 0, await p.evaluer('window.__refus'));
    if (capture) await p.capture(capture);

    // quitting stops R and removes its temporary folder
    verifier('dossierTemporaireR', /nuee-r-\d+$/.test(temporaire) && fs.existsSync(temporaire), temporaire);
    await Promise.race([p.evaluer('window.bureau.quitter()').catch(() => {}), pause(3000)]);  // the page goes with the app
    for (let i = 0; i < 60 && proc.exitCode === null; i += 1) await pause(250);
    verifier('quitte', proc.exitCode !== null, proc.exitCode);
    await pause(1000);
    verifier('RArrete', pidR > 0 && !enVie(pidR), pidR);
    verifier('dossierTemporaireRSupprime', !fs.existsSync(temporaire));
    verifier('sansErreurDansLaPage', p.erreurs.length === 0, p.erreurs.slice(0, 10));
  } catch (e) {
    verifier('erreur', false, e.message);
    if (p && capture) try { await p.capture(capture.replace(/\.png$/i, '') + '-erreur.png'); } catch { /* no page */ }
  } finally {
    if (p) p.fermer();
    tuer(proc);
    if (echecs.length) res.sortieProgramme = sortie.slice(-3000);
    console.log(JSON.stringify(res, null, 1));
    console.log(echecs.length ? `Échecs : ${echecs.join(', ')}` : 'Toutes les vérifications sont passées.');
    fs.rmSync(dossier, { recursive: true, force: true });
    process.exit(echecs.length ? 1 : 0);
  }
})();
