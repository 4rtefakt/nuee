// The tutorials' screenshots (docs/site/images/<langue>/), taken from the browser version of Nuée:
// the R engine (moteur/serveur.R) serves the interface, a headless Chrome (or Edge) shows it, and
// this script uses it over the Chrome DevTools Protocol as a person would, once in French with the
// French data, once in English with the English data, each in the light theme (images/<langue>/)
// and the dark one (images/<langue>/sombre/, shown by the site's dark theme). Each tutorial
// starts a fresh engine.
//   node docs/site/captures.mjs [baci] [pls] [suivi] [--langue fr|en] [--theme clair|sombre]
// Needs Node 22 or later, R with the engine's packages (RSCRIPT=/path/to/Rscript, otherwise R
// 4.6.2's default place on Windows, or Rscript on the PATH) and Chrome or Edge (CHROME=...).
// With Python and Pillow, the images are then reduced to 256 colours (a third of their size).
// The engine listens on 127.0.0.1:8793 and Chrome on 127.0.0.1:9333 (PORT_MOTEUR, PORT_CHROME).
import { spawn, execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const ICI = path.dirname(fileURLToPath(import.meta.url));
const RACINE = path.resolve(ICI, '..', '..');
const DONNEES = path.join(ICI, 'donnees');
const windows = process.platform === 'win32';
const RSCRIPT = process.env.RSCRIPT || (windows ? path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe') : 'Rscript');
const CHROME = process.env.CHROME || [
  'C:/Program Files/Google/Chrome/Application/chrome.exe', 'C:/Program Files (x86)/Google/Chrome/Application/chrome.exe',
  'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe', '/usr/bin/google-chrome', '/usr/bin/chromium', '/usr/bin/chromium-browser',
].find((f) => fs.existsSync(f));
const PORT_MOTEUR = Number(process.env.PORT_MOTEUR || 8793);
const PORT_CHROME = Number(process.env.PORT_CHROME || 9333);
const pause = (ms) => new Promise((r) => setTimeout(r, ms));

const args = process.argv.slice(2);
const iLangue = args.indexOf('--langue');
const LANGUES = iLangue >= 0 ? [args[iLangue + 1]] : ['fr', 'en'];
const iTheme = args.indexOf('--theme');
const THEMES = iTheme >= 0 ? [args[iTheme + 1]] : ['clair', 'sombre'];
const choisis = args.filter((a, i) => !a.startsWith('--') && (iLangue < 0 || i !== iLangue + 1) && (iTheme < 0 || i !== iTheme + 1));
let theme = 'clair';  // the interface's theme for the tutorial being captured

// ------------------------------------------------------------ the engine and the browser

function demarrerMoteur() {
  return new Promise((ok, ko) => {
    const proc = spawn(RSCRIPT, ['moteur/serveur.R', String(PORT_MOTEUR), 'app/interface'], { cwd: RACINE, stdio: ['ignore', 'pipe', 'pipe'] });
    let sortie = '';
    const delai = setTimeout(() => ko(new Error(`the engine didn't start:\n${sortie}`)), 90000);
    const lire = (d) => {
      sortie += d;
      const m = sortie.match(/(http:\/\/127\.0\.0\.1:\d+\/api\/connexion\?jeton=\w+)/);
      if (m) { clearTimeout(delai); ok({ proc, url: m[1], base: `http://127.0.0.1:${PORT_MOTEUR}` }); }
    };
    proc.stdout.on('data', lire);
    proc.stderr.on('data', lire);
    proc.on('exit', (c) => { clearTimeout(delai); ko(new Error(`the engine stopped (${c}):\n${sortie}`)); });
  });
}

function arreter(proc) {
  if (!proc || proc.exitCode !== null) return;
  try {
    if (windows) execFileSync('taskkill', ['/pid', String(proc.pid), '/T', '/F'], { stdio: 'ignore' });
    else proc.kill('SIGKILL');
  } catch { /* already gone */ }
}

async function demarrerChrome() {
  if (!CHROME) throw new Error('no Chrome or Edge found: set CHROME=/path/to/chrome');
  const profil = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-captures-'));
  const proc = spawn(CHROME, ['--headless=new', `--remote-debugging-port=${PORT_CHROME}`, `--user-data-dir=${profil}`, '--no-first-run',
    '--no-default-browser-check', '--hide-scrollbars', '--force-device-scale-factor=1', '--window-size=1440,900', 'about:blank'], { stdio: 'ignore' });
  for (let i = 0; i < 150; i += 1) {
    try { await fetch(`http://127.0.0.1:${PORT_CHROME}/json/version`); return { proc, profil }; } catch { await pause(200); }
  }
  throw new Error('Chrome did not answer');
}

// A tab, and what this script does in it.
async function onglet(largeur, hauteur) {
  const cible = await (await fetch(`http://127.0.0.1:${PORT_CHROME}/json/new?about:blank`, { method: 'PUT' })).json();
  const ws = new WebSocket(cible.webSocketDebuggerUrl);
  await new Promise((ok, ko) => { ws.onopen = ok; ws.onerror = ko; });
  let n = 0;
  const attente = new Map();
  const ecouteurs = [];
  ws.onmessage = (m) => {
    const j = JSON.parse(m.data);
    if (j.id && attente.has(j.id)) { attente.get(j.id)(j); attente.delete(j.id); return; }
    if (j.method === 'Runtime.exceptionThrown') console.log('   (page error)', j.params.exceptionDetails.exception?.description || j.params.exceptionDetails.text);
    for (const f of ecouteurs) f(j);
  };
  const envoyer = (method, params = {}) => new Promise((ok, ko) => {
    n += 1;
    attente.set(n, (j) => (j.error ? ko(new Error(`${method}: ${j.error.message}`)) : ok(j.result)));
    ws.send(JSON.stringify({ id: n, method, params }));
  });
  const evenement = (nom) => new Promise((ok) => { const f = (j) => { if (j.method === nom) { ecouteurs.splice(ecouteurs.indexOf(f), 1); ok(j.params); } }; ecouteurs.push(f); });
  await envoyer('Page.enable');
  await envoyer('Runtime.enable');
  await envoyer('Emulation.setDeviceMetricsOverride', { width: largeur, height: hauteur, deviceScaleFactor: 1, mobile: false });
  await envoyer('Emulation.setFocusEmulationEnabled', { enabled: true });
  const p = {
    envoyer,
    evenement,
    async evaluer(expression) {
      const r = await envoyer('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true });
      if (r.exceptionDetails) throw new Error(`${r.exceptionDetails.exception?.description || r.exceptionDetails.text}\n  in: ${expression.slice(0, 300)}`);
      return r.result.value;
    },
    async attendre(expression, ms = 60000) {
      const fin = Date.now() + ms;
      for (;;) {
        let v;
        try { v = await p.evaluer(expression); } catch { v = false; }
        if (v) return v;
        if (Date.now() > fin) throw new Error(`waited ${ms / 1000} s for: ${expression}`);
        await pause(250);
      }
    },
    async aller(url) {
      const charge = evenement('Page.loadEventFired');
      await envoyer('Page.navigate', { url });
      await charge;
      // the page's helpers: texts by their key in the interface's language, elements by their text
      await p.attendre("document.readyState === 'complete'");
      if (url.startsWith('http://127.0.0.1')) await p.evaluer(OUTILS);
    },
    taille: (l, h) => envoyer('Emulation.setDeviceMetricsOverride', { width: l, height: h, deviceScaleFactor: 1, mobile: false }),
    // a real click, at the middle of the element the expression gives
    async cliquer(expression, double = false) {
      const c = await p.attendre(`(() => { const e = ${expression}; return e ? __n.centre(e) : null; })()`, 30000);
      await envoyer('Input.dispatchMouseEvent', { type: 'mouseMoved', x: c.x, y: c.y });
      for (const clickCount of double ? [1, 2] : [1]) {
        await envoyer('Input.dispatchMouseEvent', { type: 'mousePressed', x: c.x, y: c.y, button: 'left', clickCount });
        await envoyer('Input.dispatchMouseEvent', { type: 'mouseReleased', x: c.x, y: c.y, button: 'left', clickCount });
      }
      await pause(150);
    },
    taper: (text) => envoyer('Input.insertText', { text }),
    async touche(key, modifiers = 0) {
      const codes = { Enter: 13, Escape: 27, Tab: 9 };
      const vk = key.length === 1 ? key.toUpperCase().charCodeAt(0) : codes[key];
      const code = key.length === 1 ? `Key${key.toUpperCase()}` : key;
      for (const type of ['rawKeyDown', 'keyUp']) await envoyer('Input.dispatchKeyEvent', { type, key, code, windowsVirtualKeyCode: vk, modifiers });
    },
    // the rectangle of an element (an expression), cut to the viewport
    async capture(fichier, expression, marges = {}) {
      // (no field with the keyboard, no row under the mouse)
      await p.evaluer('document.activeElement && document.activeElement.blur && document.activeElement.blur(); true');
      await envoyer('Input.dispatchMouseEvent', { type: 'mouseMoved', x: 1, y: 1 });
      await pause(200);
      const r = await p.attendre(`(() => { const e = ${expression}; return e ? __n.${marges.contenu ? 'contenu' : 'rect'}(e) : null; })()`, 30000);
      const vue = await p.evaluer('({ l: innerWidth, h: innerHeight })');
      const x = Math.max(0, Math.floor(r.x - (marges.gauche || 0)));
      const y = Math.max(0, Math.floor(r.y - (marges.haut || 0)));
      const clip = { x, y, width: Math.min(vue.l, Math.ceil(r.x + r.width + (marges.droite || 0))) - x,
        height: Math.min(vue.h, Math.ceil(r.y + r.height + (marges.bas || 0))) - y, scale: 1 };
      if (marges.hauteur) clip.height = Math.min(clip.height, marges.hauteur);
      const res = await envoyer('Page.captureScreenshot', { format: 'png', clip });
      fs.mkdirSync(path.dirname(fichier), { recursive: true });
      fs.writeFileSync(fichier, Buffer.from(res.data, 'base64'));
      console.log(`   ${path.relative(ICI, fichier)} ${clip.width}×${clip.height}`);
    },
    async fermer() {
      ws.close();
      await fetch(`http://127.0.0.1:${PORT_CHROME}/json/close/${cible.id}`).catch(() => {});
    },
  };
  return p;
}

// Helpers put in each page of the interface.
const OUTILS = `(async () => {
  const { t } = await import('/i18n.js');
  const norm = (s) => String(s).replace(/\\s+/g, ' ').trim();
  const visible = (e) => !!e && e.getClientRects().length > 0 && getComputedStyle(e).visibility !== 'hidden';
  window.__n = {
    t, norm, visible,
    tous: (sel, dans = document) => [...dans.querySelectorAll(sel)].filter(visible),
    texte: (sel, texte, dans = document) => __n.tous(sel, dans).find((e) => norm(e.textContent) === norm(texte)),
    debut: (sel, texte, dans = document) => __n.tous(sel, dans).find((e) => norm(e.textContent).startsWith(norm(texte))),
    centre(e) { e.scrollIntoView({ block: 'nearest', inline: 'nearest' }); const r = e.getBoundingClientRect(); return { x: r.left + r.width / 2, y: r.top + r.height / 2 }; },
    rect(e) { const r = e.getBoundingClientRect(); return { x: r.left, y: r.top, width: r.width, height: r.height }; },
    // an element cut where its content ends (a summary shorter than its panel)
    contenu(e) {
      const r = e.getBoundingClientRect();
      const bas = Math.max(...[...e.querySelectorAll('*')].filter((x) => visible(x) && !x.children.length).map((x) => x.getBoundingClientRect().bottom));
      return { x: r.left, y: r.top, width: r.width, height: Math.min(r.height, bas - r.top + 10) };
    },
    dialogue: () => [...document.querySelectorAll('.fond-dialogue')].pop(),
    boite: () => { const f = __n.dialogue(); return f && (f.querySelector('.dialogue') || f.firstElementChild); },
    // a select by one of its options' values, then a value chosen in it
    select: (valeur, dans = document) => __n.tous('select', dans).find((s) => [...s.options].some((o) => o.value === String(valeur))),
    choisir(s, valeur) { s.value = String(valeur); s.dispatchEvent(new Event('change', { bubbles: true })); return true; },
    saisir(e, valeur) { e.focus(); e.value = valeur; e.dispatchEvent(new Event('input', { bubbles: true })); e.dispatchEvent(new Event('change', { bubbles: true })); return true; },
    // the API, as the interface asks it
    api: async (nom, q = {}) => (await fetch('/api/' + nom, { method: 'POST', headers: { 'Content-Type': 'application/json', 'X-Langue': localStorage.getItem('langue') || 'fr' }, body: JSON.stringify(q) })).json(),
  };
  return true;
})()`;

// ------------------------------------------------------------ steps shared by the tutorials

// The interface in the language and theme wanted, the What's new of a first start closed.
async function demarrer(moteur, langue, fichier) {
  const p = await onglet(1280, 900);
  await p.aller(moteur.url);
  await p.evaluer(`localStorage.setItem('langue', '${langue}'); localStorage.setItem('theme', '${theme}'); true`);
  await p.aller(`${moteur.base}/?ouvrir=${encodeURIComponent(fichier)}`);
  await p.attendre(`!!document.querySelector('.arbre') && document.querySelector('.arbre').textContent.includes(${JSON.stringify(path.basename(fichier))})`);
  await pause(1500);
  await fermerDialogues(p);
  return p;
}

async function fermerDialogues(p) {
  await p.evaluer("document.querySelectorAll('.fond-dialogue').forEach((f) => f.remove()); true");
}

async function menu(p, cleMenu, cleItem) {
  await fermerDialogues(p);
  await p.cliquer(`__n.texte('.menu-haut', __n.t('${cleMenu}'))`);
  await p.attendre("!!document.querySelector('.menu .item')");
  await p.cliquer(`__n.debut('.menu .item', __n.t('${cleItem}'))`);
}

// a command by its name, from the command palette (Ctrl+K): for the items at the end of a long menu
async function commande(p, cle) {
  await fermerDialogues(p);
  await p.evaluer('document.activeElement && document.activeElement.blur && document.activeElement.blur(); true');
  await p.touche('k', 2);
  await p.attendre("!!document.querySelector('.palette-saisie')", 10000);
  await p.taper(await p.evaluer(`__n.t('${cle}')`));
  await pause(400);
  await p.touche('Enter');
  await pause(300);
}

// the project or model of the explorer whose name starts with this, chosen (a model: the last
// such line, its fit under "Fitted models" rather than its definition)
async function choisirDansExplorateur(p, debut) {
  await p.cliquer(`__n.tous('.arbre .noeud').filter((e) => __n.norm((e.querySelector('.libelle') || e).textContent).startsWith(${JSON.stringify(debut)})).pop()`);
  await pause(400);
}

async function etat(p) {
  return p.evaluer("__n.api('etat').then((e) => ({ projets: e.projets.map((x) => ({ id: x.id, nom: x.nom, variables: x.variables.map((v) => ({ uid: v.uid, nom: v.nom })) })), modeles: e.modeles.map((m) => ({ id: m.id, nom: m.nom, type: m.type, projet: m.projet })) }))");
}

// a view on its own (the interface's detached view), the model's last fit; `loc`: its settings
async function vueSeule(moteur, vue, { modele, projet, loc, largeur = 900, hauteur = 560, attente = "document.querySelectorAll('.js-plotly-plot .main-svg, table').length > 0", avant } = {}) {
  const v = await onglet(largeur, hauteur);
  const q = new URLSearchParams({ vue, ...(modele ? { modele } : {}), ...(projet ? { projet } : {}), ...(loc ? { loc: JSON.stringify(loc) } : {}) });
  await v.aller(`${moteur.base}/?${q}`);
  await v.attendre(attente, 180000);
  await pause(1200);
  if (avant) { await avant(v); await pause(1200); }
  return v;
}

// what is under the detached view's title bar
const CORPS_SEUL = "document.querySelector('#seul > .panneau')";

async function captureVue(moteur, fichier, vue, options) {
  const v = await vueSeule(moteur, vue, options);
  await v.capture(fichier, CORPS_SEUL, { contenu: !!options.contenu });
  await v.fermer();
}

// New model on its General tab
async function nouveauModele(p, type) {
  await menu(p, 'm_modele', 'nouveau_modele');
  await p.attendre("!!document.querySelector('.fond-dialogue .onglets')");
  await p.cliquer(`__n.texte('.fond-dialogue .onglets button', __n.t('general'))`);
  await p.attendre(`!!__n.select('${type}')`);
}

// With NUEE_TEXTES=<folder>: each model's Methods and Results texts (and the BACI model's script of
// Export to R) written there too, to quote them in the tutorials.
const TEXTES = process.env.NUEE_TEXTES || '';
const VERSION = JSON.parse(fs.readFileSync(path.join(RACINE, 'app', 'package.json'), 'utf8')).version;
async function garderTextes(p, nom, modele, langue, script = false) {
  if (!TEXTES) return;
  fs.mkdirSync(TEXTES, { recursive: true });
  const r = await p.evaluer(`Promise.all([__n.api('methodes', { modele: '${modele}', langue: '${langue}', version: '${VERSION}' }),
    __n.api('resultats_texte', { modele: '${modele}', langue: '${langue}' })])`);
  fs.writeFileSync(path.join(TEXTES, `${nom}-${langue}.json`), JSON.stringify(r, null, 1));
  if (script) {
    await p.evaluer(`__n.api('script_r', { modele: '${modele}', chemin: ${JSON.stringify(path.join(TEXTES, `${nom}-${langue}.R`))}, format: 'R', donnees: 'script', version: '${VERSION}', langue: '${langue}' })`);
  }
}

// ------------------------------------------------------------ BACI

const BACI = {
  fr: { fichier: 'baci_truite.csv', y: 'densite', groupe: 'groupe', periode: 'periode', formulePeriode: 'ifelse(annee >= 2016, "après", "avant")',
    modele: 'BACI densite', facteurs: 'BACI facteurs', formule: 'densite ~ periode * groupe + (1 | station)', temps: 'annee', paireImpact: 0 },
  en: { fichier: 'baci_trout.csv', y: 'density', groupe: 'group', periode: 'period', formulePeriode: 'ifelse(year >= 2016, "after", "before")',
    modele: 'BACI density', facteurs: 'BACI factors', formule: 'density ~ period * group + (1 | station)', temps: 'year', paireImpact: 1 },
};

async function tutorielBaci(langue, images) {
  const L = BACI[langue];
  const moteur = await demarrerMoteur();
  let p;
  try {
    p = await demarrer(moteur, langue, path.join(DONNEES, L.fichier));
    // Which analysis? › before-after with controls
    await menu(p, 'm_modele', 'quelle_analyse');
    await p.cliquer("document.querySelector('[data-cle=\"qa-question:baci\"]')");
    await p.attendre("!!document.querySelector('[data-cle=\"qa-y\"]')");
    await p.evaluer(`(() => { const s = document.querySelector('[data-cle="qa-y"]'); return __n.choisir(s, [...s.options].find((o) => o.textContent === ${JSON.stringify(L.y)}).value); })()`);
    await p.attendre("!!document.querySelector('.qa-carte .qa-ouvrir button')");
    await pause(600);
    await p.capture(path.join(images, 'baci-choix.png'), '__n.boite()');
    await p.cliquer("document.querySelector('.qa-carte .qa-ouvrir button')");
    // the BACI view: the impact and the controls, the event
    await p.attendre(`__n.tous('.unites label').length >= 12`);
    for (const s of ['A1', 'A2', 'A3']) await p.cliquer(`__n.tous('.unites')[0].querySelectorAll('label')[${['A1', 'A2', 'A3'].indexOf(s)}]`);
    await p.cliquer(`__n.texte('.unites button', __n.t('baci_tous_autres'))`);
    await p.evaluer(`__n.saisir(__n.tous('label.champ').find((l) => l.textContent.includes(__n.t('baci_evenement'))).querySelector('input'), '2016')`);
    await p.attendre("__n.tous('.js-plotly-plot').length > 0 && !document.querySelector('.infos.attente')");
    await pause(800);
    // the mixed model, in one click
    await p.cliquer(`__n.tous('button').find((b) => __n.norm(b.textContent) === __n.t('baci_modele') && !b.disabled)`);
    await p.attendre(`__n.tous('.arbre .noeud').some((e) => e.textContent.includes(${JSON.stringify(L.modele)}))`, 120000);
    await pause(2000);
    let e = await etat(p);
    const projet = e.projets[0];
    const uid = (nom) => projet.variables.find((v) => v.nom === nom).uid;
    const baci = e.modeles.find((m) => m.nom === L.modele);
    await garderTextes(p, 'baci', baci.id, langue, true);
    const locBaci = { mode: 'baci', baci: { y: uid(L.y), impact: ['A1', 'A2', 'A3'], temoins: ['T1', 'T2', 'T3'], evenement: '2016' } };
    await captureVue(moteur, path.join(images, 'baci-ecarts.png'), 'tests', { projet: projet.id, loc: locBaci, largeur: 1000, hauteur: 900, contenu: true,
      attente: "document.querySelectorAll('.js-plotly-plot .main-svg').length > 0 && !document.querySelector('.infos.attente')" });
    await captureVue(moteur, path.join(images, 'baci-resume.png'), 'resume', { modele: baci.id, largeur: 520, hauteur: 860, contenu: true, attente: "document.querySelectorAll('table').length > 0" });
    await captureVue(moteur, path.join(images, 'baci-coefficients.png'), 'coefficients', { modele: baci.id, largeur: 900, hauteur: 520 });
    // the same model with two qualitative variables, for the adjusted means
    await menu(p, 'm_edition', 'nouvelle_variable');
    await p.attendre("!!__n.boite() && __n.boite().querySelectorAll('input, textarea').length >= 2");
    await p.evaluer(`(() => { const b = __n.boite(); __n.saisir(b.querySelector('input'), ${JSON.stringify(L.periode)}); __n.saisir(b.querySelector('textarea'), ${JSON.stringify(L.formulePeriode)}); return true; })()`);
    await pause(400);
    await p.capture(path.join(images, 'baci-nouvelle-variable.png'), '__n.boite()');
    await p.cliquer(`__n.texte('.fond-dialogue button', __n.t('ajouter'))`);
    await p.attendre("!document.querySelector('.fond-dialogue')");
    await pause(800);
    await nouveauModele(p, 'LMM');
    await p.evaluer("__n.choisir(__n.select('LMM'), 'LMM')");
    await pause(500);
    await p.evaluer(`(() => { const z = __n.boite().querySelector('textarea'); z.focus(); z.select(); return true; })()`);
    await p.taper(L.formule);
    await p.cliquer(`__n.tous('.fond-dialogue button').find((b) => __n.norm(b.textContent) === __n.t('appliquer') && !b.disabled)`);
    await pause(600);
    await p.cliquer(`__n.tous('.fond-dialogue label.champ').find((l) => l.textContent.includes('CAR1')).querySelector('input')`);
    await pause(400);
    await p.evaluer(`(() => { const s = __n.tous('.fond-dialogue select').find((x) => [...x.options].some((o) => o.textContent === ${JSON.stringify(L.temps)}) && [...x.options][0].value === ''); return __n.choisir(s, [...s.options].find((o) => o.textContent === ${JSON.stringify(L.temps)}).value); })()`);
    await p.evaluer(`(() => { const i = __n.tous('.fond-dialogue input[type=text], .fond-dialogue input:not([type])').find((x) => /^M\\d+$/.test(x.value)); return __n.saisir(i, ${JSON.stringify(L.facteurs)}); })()`);
    await pause(500);
    await p.capture(path.join(images, 'baci-modele-facteurs.png'), '__n.boite()');
    await p.cliquer(`__n.texte('.fond-dialogue button', __n.t('ajuster'))`);
    await p.attendre(`__n.tous('.arbre .noeud').some((e) => e.textContent.includes(${JSON.stringify(L.facteurs)}))`, 120000);
    await pause(2000);
    e = await etat(p);
    const facteurs = e.modeles.find((m) => m.nom === L.facteurs);
    await garderTextes(p, 'baci-facteurs', facteurs.id, langue);
    const p2 = e.projets[0];
    const uid2 = (nom) => p2.variables.find((v) => v.nom === nom).uid;
    await captureVue(moteur, path.join(images, 'baci-moyennes.png'), 'moyennes', { modele: facteurs.id, largeur: 1100, hauteur: 680, contenu: true,
      loc: { reglages: { facteur: uid2(L.periode), dans: uid2(L.groupe), pente: null, ajustement: 'tukey' }, paire: L.paireImpact } });
    // the Methods text and Export to R, for the one-click model
    await choisirDansExplorateur(p, L.modele);
    await menu(p, 'm_modele', 'texte_methodes');
    await p.attendre("!!document.querySelector('.texte-methodes p')");
    await pause(600);
    await p.capture(path.join(images, 'baci-methodes.png'), '__n.boite()');
    await menu(p, 'm_fichier', 'script_r');
    await p.attendre("!!__n.boite() && __n.boite().querySelectorAll('input[type=radio]').length > 0");
    await pause(400);
    await p.capture(path.join(images, 'baci-export-r.png'), '__n.boite()');
    await fermerDialogues(p);
  } finally {
    if (p) await p.fermer();
    arreter(moteur.proc);
  }
}

// ------------------------------------------------------------ PLS on spectra

const PLS = {
  fr: { fichier: 'sols_spectres.csv', nouveaux: 'sols_nouveaux.csv', y: 'cos', qualitative: 'occupation' },
  en: { fichier: 'soils_spectra.csv', nouveaux: 'soils_new.csv', y: 'soc', qualitative: 'land_use' },
};

async function tutorielPls(langue, images) {
  const L = PLS[langue];
  const moteur = await demarrerMoteur();
  let p;
  try {
    p = await demarrer(moteur, langue, path.join(DONNEES, L.fichier));
    await nouveauModele(p, 'PLS');
    // centred only, by default; then the roles: every variable X, the carbon Y, the land use left out
    await p.evaluer("__n.choisir(__n.tous('.fond-dialogue select').find((s) => s.options[0].value === 'UV' && [...s.options].some((o) => o.value === 'Ctr')), 'Ctr')");
    await pause(300);
    await p.cliquer(`__n.texte('.fond-dialogue .onglets button', __n.t('variables'))`);
    await p.attendre("!!__n.select('aucun')");
    await p.evaluer("__n.choisir(__n.select('aucun'), 'x')");
    await pause(300);
    const ligne = (nom) => `__n.tous('.fond-dialogue tr').find((r) => r.querySelector('td') && __n.norm(r.querySelector('td').textContent) === ${JSON.stringify(nom)})`;
    await p.evaluer(`__n.choisir(${ligne(L.y)}.querySelector('select'), 'y')`);
    await p.evaluer(`__n.choisir(${ligne(L.qualitative)}.querySelector('select'), '')`);
    await pause(400);
    await p.capture(path.join(images, 'pls-variables.png'), '__n.boite()');
    await p.cliquer(`__n.texte('.fond-dialogue .onglets button', __n.t('general'))`);
    await p.attendre("!!__n.select('snv')");
    await p.evaluer("__n.choisir(__n.tous('.fond-dialogue select').filter((s) => [...s.options].some((o) => o.value === 'snv'))[0], 'snv')");
    await pause(300);
    await p.evaluer("__n.choisir(__n.tous('.fond-dialogue select').filter((s) => [...s.options].some((o) => o.value === 'snv'))[1], 'detrend')");
    await pause(500);
    await p.capture(path.join(images, 'pls-nouveau-modele.png'), '__n.boite()');
    await p.cliquer(`__n.texte('.fond-dialogue button', __n.t('ajuster'))`);
    await p.attendre("__n.tous('.arbre .noeud').some((e) => / A=\\d/.test(e.textContent))", 180000);
    await pause(2500);
    // the whole window, for the home page
    await p.capture(path.join(images, 'accueil-fenetre.png'), 'document.body');
    const e = await etat(p);
    const m = e.modeles.find((x) => x.type === 'PLS');
    await garderTextes(p, 'pls', m.id, langue, true);
    const proj = e.projets[0];
    const uidQual = proj.variables.find((v) => v.nom === L.qualitative).uid;
    await captureVue(moteur, path.join(images, 'pls-spectres-bruts.png'), 'profils', { modele: m.id, loc: { quoi: 'spectres', bruts: true }, largeur: 900, hauteur: 480 });
    await captureVue(moteur, path.join(images, 'pls-spectres-pretraites.png'), 'profils', { modele: m.id, loc: { quoi: 'spectres', bruts: false }, largeur: 900, hauteur: 480 });
    await captureVue(moteur, path.join(images, 'pls-resume.png'), 'resume', { modele: m.id, largeur: 560, hauteur: 700, contenu: true, attente: "document.querySelectorAll('table').length > 0" });
    await captureVue(moteur, path.join(images, 'pls-vip.png'), 'profils', { modele: m.id, loc: { quoi: 'vip' }, largeur: 900, hauteur: 480 });
    await captureVue(moteur, path.join(images, 'pls-observe-predit.png'), 'obspred', { modele: m.id, loc: { couleur: `v${uidQual}` }, largeur: 760, hauteur: 560 });
    await captureVue(moteur, path.join(images, 'pls-permutations.png'), 'permutations', { modele: m.id, loc: { n: 100 }, largeur: 760, hauteur: 560 });
    // twenty new samples, predicted from a file (the file chooser answered by this script)
    await choisirDansExplorateur(p, m.nom);
    await p.envoyer('Page.setInterceptFileChooserDialog', { enabled: true });
    const choix = p.evenement('Page.fileChooserOpened');
    await menu(p, 'm_prediction', 'predire_fichier');
    const c = await choix;
    await p.envoyer('DOM.setFileInputFiles', { files: [path.join(DONNEES, L.nouveaux)], backendNodeId: c.backendNodeId });
    await p.attendre("__n.tous('.dv-tab').some((t) => t.textContent.includes(__n.t('predictions')))", 120000);
    await pause(2000);
    // that view's group of tabs, maximized, then cut
    await p.taille(1100, 760);
    await pause(800);
    const vue = "__n.tous('.panneau').find((x) => x.closest('.dv-content-container') && x.querySelector('table') && x.textContent.includes('N001'))";
    await p.attendre(`!!(${vue})`);
    await p.cliquer(`(${vue}).closest('.dv-groupview').querySelector('[title="' + __n.t('agrandir') + '"]')`);
    await pause(1500);
    // the end of the table, where the last sample is
    await p.evaluer(`[...(${vue}).querySelectorAll('*')].filter((x) => x.scrollHeight > x.clientHeight + 4 && getComputedStyle(x).overflowY !== 'visible').forEach((x) => { x.scrollTop = x.scrollHeight; }); true`);
    await pause(600);
    await p.capture(path.join(images, 'pls-prediction.png'), `(${vue}).closest('.dv-groupview')`);
  } finally {
    if (p) await p.fermer();
    arreter(moteur.proc);
  }
}

// ------------------------------------------------------------ monitoring: TRIM, distance sampling, a figure

const SUIVI = {
  fr: { comptages: 'comptages_alouette.csv', transects: 'transects_alouette.csv', y: 'alouettes' },
  en: { comptages: 'skylark_counts.csv', transects: 'skylark_transects.csv', y: 'skylarks' },
};

async function tutorielSuivi(langue, images) {
  const L = SUIVI[langue];
  const moteur = await demarrerMoteur();
  let p;
  try {
    p = await demarrer(moteur, langue, path.join(DONNEES, L.comptages));
    // Which analysis? › a trend › a count
    await menu(p, 'm_modele', 'quelle_analyse');
    await p.cliquer("document.querySelector('[data-cle=\"qa-question:tendance\"]')");
    await p.cliquer("document.querySelector('[data-cle=\"qa-reponse:comptage\"]')");
    await p.attendre("!!document.querySelector('[data-cle=\"qa-y\"]')");
    await p.evaluer(`(() => { const s = document.querySelector('[data-cle="qa-y"]'); return __n.choisir(s, [...s.options].find((o) => o.textContent === ${JSON.stringify(L.y)}).value); })()`);
    await p.attendre("!!document.querySelector('.qa-carte .qa-ouvrir button')");
    await pause(600);
    await p.capture(path.join(images, 'suivi-choix-trim.png'), '__n.boite()');
    await p.cliquer("document.querySelector('.qa-carte .qa-ouvrir button')");
    await p.attendre("!!document.querySelector('.fond-dialogue .onglets')");
    await p.cliquer(`__n.texte('.fond-dialogue .onglets button', __n.t('general'))`);
    await p.attendre("!!__n.select('TRIM')");
    await pause(500);
    await p.capture(path.join(images, 'suivi-trim-modele.png'), '__n.boite()');
    await p.cliquer(`__n.texte('.fond-dialogue button', __n.t('ajuster'))`);
    await p.attendre("__n.tous('.arbre .noeud').some((e) => e.textContent.includes('TRIM'))", 180000);
    await pause(2500);
    let e = await etat(p);
    const trim = e.modeles.find((x) => x.type === 'TRIM');
    await garderTextes(p, 'trim', trim.id, langue);
    await captureVue(moteur, path.join(images, 'suivi-trim-resume.png'), 'resume', { modele: trim.id, largeur: 560, hauteur: 800, contenu: true, attente: "document.querySelectorAll('table').length > 0" });
    await captureVue(moteur, path.join(images, 'suivi-trim-indices.png'), 'trim_indices', { modele: trim.id, loc: { modele: true, bande: true, droite: true }, largeur: 900, hauteur: 680 });

    // a figure for the report, the indices and the yearly totals side by side (the composer offers
    // the plots open in the window: those the fit opened)
    await commande(p, 'figure_composer');
    await p.attendre("!!document.querySelector('.cadre-figure')", 30000);
    await pause(1500);
    const ajout = "__n.tous('select').find((s) => s.options.length > 1 && s.options[0].textContent === __n.t('figure_vues_ouvertes'))";
    for (const titre of ['trim_indices', 'trim_totaux']) {
      const deja = await p.evaluer(`__n.tous('.ligne-panneau .nom-panneau').some((x) => x.textContent.includes(__n.t('${titre}')))`);
      if (deja) continue;
      await p.attendre(`!!(${ajout})`);
      await p.evaluer(`(() => { const s = ${ajout}; const o = [...s.options].find((x) => x.value && x.textContent.includes(__n.t('${titre}'))); return __n.choisir(s, o.value); })()`);
      await pause(2500);
    }
    // a height given: 75 mm
    const hauteur = "__n.tous('.cadre-figure label.champ').find((l) => l.title === __n.t('figure_hauteur_aide'))";
    await p.cliquer(`(${hauteur}).querySelector('input')`);
    await pause(500);
    await p.evaluer(`__n.saisir((${hauteur}).parentElement.querySelector('input[type=number]'), '75')`);
    await p.taille(1440, 900);
    await pause(3000);
    await p.capture(path.join(images, 'suivi-figure.png'), "document.querySelector('.cadre-figure')", { hauteur: 640 });
    await p.taille(1280, 900);

    // distance sampling: the transects of 2024
    await p.aller(`${moteur.base}/?ouvrir=${encodeURIComponent(path.join(DONNEES, L.transects))}`);
    await p.attendre(`document.querySelector('.arbre').textContent.includes(${JSON.stringify(L.transects)})`);
    await pause(1500);
    await fermerDialogues(p);
    await choisirDansExplorateur(p, L.transects);
    await menu(p, 'm_modele', 'quelle_analyse');
    await p.cliquer("document.querySelector('[data-cle=\"qa-question:densite\"]')");
    await p.attendre("!!document.querySelector('.qa-carte .qa-ouvrir button')");
    await pause(600);
    await p.capture(path.join(images, 'suivi-choix-distance.png'), '__n.boite()');
    await p.cliquer("document.querySelector('.qa-carte .qa-ouvrir button')");
    await p.attendre("!!document.querySelector('.fond-dialogue .onglets')");
    await p.cliquer(`__n.texte('.fond-dialogue .onglets button', __n.t('general'))`);
    await p.attendre("!!__n.select('pourcentage')");
    await p.evaluer("__n.choisir(__n.select('pourcentage'), 'pourcentage')");
    await pause(500);
    await p.capture(path.join(images, 'suivi-distance-modele.png'), '__n.boite()');
    await p.cliquer(`__n.texte('.fond-dialogue button', __n.t('ajuster'))`);
    await p.attendre("__n.tous('.arbre .noeud').some((e) => e.textContent.includes('DISTANCE'))", 180000);
    await pause(2500);
    e = await etat(p);
    const distance = e.modeles.find((x) => x.type === 'DISTANCE');
    await garderTextes(p, 'distance', distance.id, langue);
    await captureVue(moteur, path.join(images, 'suivi-distance-resume.png'), 'resume', { modele: distance.id, largeur: 560, hauteur: 520, contenu: true, attente: "document.querySelectorAll('table').length > 0" });
    await captureVue(moteur, path.join(images, 'suivi-distance-detection.png'), 'distance_detection', { modele: distance.id, largeur: 860, hauteur: 680 });

  } finally {
    if (p) await p.fermer();
    arreter(moteur.proc);
  }
}

// ------------------------------------------------------------ run

const TUTORIELS = { baci: tutorielBaci, pls: tutorielPls, suivi: tutorielSuivi };

function compresser(dossier) {
  const script = [
    'import sys, os', 'from PIL import Image',
    'for f in sys.argv[1:]:',
    '    im = Image.open(f).convert("RGB")',
    '    q = im.quantize(colors=256, method=Image.Quantize.MEDIANCUT, dither=Image.Dither.NONE)',
    '    q.save(f, optimize=True)',
  ].join('\n');
  const fichiers = fs.readdirSync(dossier).filter((f) => f.endsWith('.png')).map((f) => path.join(dossier, f));
  for (const py of ['python', 'python3']) {
    try { execFileSync(py, ['-c', script, ...fichiers], { stdio: 'inherit' }); return true; } catch { /* the next one */ }
  }
  console.log('   (no Python with Pillow: the images keep their size)');
  return false;
}

(async () => {
  const chrome = await demarrerChrome();
  try {
    for (const langue of LANGUES) {
      for (theme of THEMES) {
        const images = path.join(ICI, 'images', langue, ...(theme === 'sombre' ? ['sombre'] : []));
        for (const [nom, f] of Object.entries(TUTORIELS)) {
          if (choisis.length && !choisis.includes(nom)) continue;
          console.log(`${nom} (${langue}, ${theme})`);
          await f(langue, images);
        }
        compresser(images);
      }
    }
  } finally {
    arreter(chrome.proc);
    await pause(500);
    fs.rmSync(chrome.profil, { recursive: true, force: true, maxRetries: 5 });
  }
})().catch((e) => { console.error(e); process.exit(1); });
