// The browser version used end to end in a headless Chromium (Playwright), as a person would: the
// site built (construire_navigateur.js) and served on this computer with the headers webR needs
// (servir_navigateur.js); R starts in the page; the example project opens; a PLS, a mixed model
// and a PCoA are fitted and their views drawn; the data exported as CSV (a download); random
// forests, whose package the browser version lacks, say they need the desktop app while the rest
// goes on; the recovery copies: a reload, then a closed tab, offer the projects again, with their
// models and their unsaved changes, and a second tab open meanwhile is offered nothing, and
// without OPFS's createWritable they go to IndexedDB. Then 1.1 in a tab of its own: History of a
// table never saved says so (Share leads there); a table opened (File › Open), a data step (New
// variable), Data steps, Update the data with next year's file chosen in the page, What changed; a
// project saved, opened, saved again, downloaded each time; an R-code model, its package (Kendall)
// installed from webR's repository, and one needing ranger refused; a plugin installed from its
// file and fitted; the help panel's guide; Stop, R in the page interrupted (the shared permutation
// test, a PCoA's permutations in one R), the next request as usual; reloaded, the plugin and the
// approvals still there. Then 1.2's link of a reproducibility package, ?doi=, against a stand-in
// for Zenodo's API (Playwright's routes; zenodo.org never resolves in this browser): the package
// written in the page, with a document, its main one, shown first from its record without R (the
// reader's page, lecture.js), then rerun, every model giving its deposited results; Open the whole
// project, its document in front; at a phone's size, the reader's page as wide as the screen, then
// the project one view at a time, the explorer behind its button, one click opening a view in its
// place (etroit.js); then the package chosen in the page (File › Open, File › Open a
// reproducibility package…); a concept DOI, a
// record, an unknown DOI, another one, not a DOI, no network; the link of an unknown DOI and of a
// record without a package said plainly; nothing asked of another site. Then 1.2's History in the
// page, in a tab of its own: the example saved (downloaded, kept at its place in the browser's
// storage, its first version), Save a version…, Name…, Compare, Open, Restore…; a table saved, its
// history apart; reloaded, the projects recovered at their places with their histories; the first
// download opened again under another name, its history found by its content; Save as…, a new
// place with the history; without OPFS's createWritable, the versions in IndexedDB. Then 1.2's
// sharing through nuee.io's relay, in a tab of its own: the relay's own handler for the page's
// /relais/, against stand-ins (GitHub's device flow, git's http-backend; GitHub's API as routes):
// signed in from Share, the project sent, a colleague's change received with Get now, a change
// sent with Send now, the project opened from the repository at a new place, signed out, the token
// gone; a push too large for the relay said in words; nothing reaches github.com or gitlab.com, no
// token in an address. Also the site as served: the page's headers (webR's two, no security policy
// but its own), where R's packages came from (the site's copy of webR's repository when it has
// one, miroir_webr.mjs: then nothing from another site until R is ready, and Kendall alone from
// repo.r-wasm.org), and, under a path, /app redirected to /app/. Last, ?langue=fr: a new profile
// in French, until a language is chosen. The time of each request goes to <folder>/demandes.json.
//   node tests/essai_navigateur.mjs [--site=<built site>] [--adresse=<url>] [--sortie=<folder>] [--visible] [--navigateur=firefox]
// --navigateur=firefox: the same in Playwright's Firefox (cli.js install firefox), which, unlike
// Chromium, sends nothing to an element a click took out of the page.
// --adresse: the site as another server serves it, neither built nor served here (nuee.io's
// browser version: `wrangler dev` in vitrine/, then --adresse=http://127.0.0.1:8787/app/).
// Chromium comes from node_modules/.cache/ms-playwright (PLAYWRIGHT_BROWSERS_PATH):
//   PLAYWRIGHT_BROWSERS_PATH=node_modules/.cache/ms-playwright node node_modules/playwright-core/cli.js install chromium-headless-shell
// The first start downloads R's packages from the site's copy, else from repo.r-wasm.org. Exits with 1 when a check fails.
import { execFileSync, spawn } from 'node:child_process';
import fs from 'node:fs';
import http from 'node:http';
import { createRequire } from 'node:module';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const ICI = path.dirname(fileURLToPath(import.meta.url));
const APP = path.join(ICI, '..');
const require = createRequire(import.meta.url);
const { servir } = require('../servir_navigateur.js');
process.env.PLAYWRIGHT_BROWSERS_PATH ||= path.join(APP, 'node_modules', '.cache', 'ms-playwright');
const { chromium, firefox } = await import('playwright-core');

const options = Object.fromEntries(process.argv.slice(2).filter((a) => a.startsWith('--')).map((a) => {
  const [k, ...v] = a.slice(2).split('=');
  return [k, v.join('=') || true];
}));
const sortie = typeof options.sortie === 'string' ? path.resolve(options.sortie) : fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-navigateur-'));
fs.mkdirSync(sortie, { recursive: true });
let serveur = null;
let adresse = typeof options.adresse === 'string' ? new URL(options.adresse).href : null;
if (!adresse) {
  let site = typeof options.site === 'string' ? path.resolve(options.site) : null;
  if (!site) {
    site = path.join(sortie, 'site');
    execFileSync(process.execPath, [path.join(APP, 'construire_navigateur.js'), site], { stdio: 'inherit' });
  }
  serveur = await servir(site, 0);
  adresse = `http://127.0.0.1:${serveur.address().port}/`;
}
const profil = path.join(sortie, 'profil');
fs.rmSync(profil, { recursive: true, force: true });

const res = {};
const echecs = [];
const verifier = (nom, ok, valeur) => {
  res[nom] = valeur === undefined ? ok : valeur;
  console.log(`  ${ok ? 'ok    ' : 'ÉCHEC '} ${nom}${valeur === undefined ? '' : ` : ${typeof valeur === 'string' ? valeur : JSON.stringify(valeur)}`}`);
  if (!ok) echecs.push(nom);
};
const erreurs = [];
// (zenodo.org never resolves in this browser: its API is a stand-in, Playwright's routes below, and a
// request that would get past them, a redirect's, fails rather than reach Zenodo; nor do github.com,
// gitlab.com, their APIs and the accounts' pictures: sharing's stand-ins, below too. Chromium's
// resolver rules; Firefox's local domains, 127.0.0.1, where nothing answers on their ports, take no
// wildcard: their hosts named)
const HOTES_COUPES = ['zenodo.org', '*.zenodo.org', 'github.com', '*.github.com', 'gitlab.com', '*.gitlab.com', '*.githubusercontent.com', 'gravatar.com', '*.gravatar.com'];
const HOTES_COUPES_FIREFOX = ['zenodo.org', 'www.zenodo.org', 'sandbox.zenodo.org', 'github.com', 'api.github.com', 'codeload.github.com', 'gitlab.com', 'www.gitlab.com',
  'avatars.githubusercontent.com', 'raw.githubusercontent.com', 'gravatar.com', 'www.gravatar.com', 'secure.gravatar.com'];
const lancer = (dossier, reglages) => (options.navigateur === 'firefox'
  ? firefox.launchPersistentContext(dossier, { ...reglages, firefoxUserPrefs: { 'network.dns.localDomains': HOTES_COUPES_FIREFOX.join(',') } })
  : chromium.launchPersistentContext(dossier, { ...reglages, args: [`--host-resolver-rules=${HOTES_COUPES.map((h) => `MAP ${h} ~NOTFOUND`).join(', ')}`] }));
const contexte = await lancer(profil, { headless: !options.visible, acceptDownloads: true, viewport: { width: 1440, height: 900 } });
// every request of the pages and of their workers (webR's, which installs R's packages)
const requetes = [];
contexte.on('request', (r) => requetes.push(r.url()));
const relaisEnCours = [];  // (the relay's requests in the sharing part, answered or not)
const ailleurs = () => requetes.filter((u) => /^https?:/.test(u) && new URL(u).origin !== new URL(adresse).origin);

// a page of the site: its errors noted (but the stand-in for Zenodo's planned refusals, a 404, no
// network), every request timed (api.js: window.nueeDemandes)
// (Zenodo's planned refusals, a 404, no network: Chromium's message is located at Zenodo's address,
// Firefox's names it in its text)
const zenodoAttendu = (m) => (m.location().url || '').startsWith('https://zenodo.org/') || /https:\/\/zenodo\.org\//.test(m.text());
// a page's console error noted, where it was written; Firefox's text of an Error is "Error" alone:
// its message asked of the page
async function noterConsole(m) {
  if (m.type() !== 'error' || zenodoAttendu(m)) return;
  let texte = m.text();
  if (texte.trim() === 'Error') {
    texte = (await Promise.all(m.args().map((a) => a.evaluate((x) => (x instanceof Error ? `${x.name}: ${x.message}` : String(x))).catch(() => '?')))).join(' ');
  }
  erreurs.push(`console : ${texte} (${m.location().url || '?'}:${m.location().lineNumber})`);
}
async function ouvrirPage(url = adresse) {
  const page = await contexte.newPage();
  page.on('pageerror', (e) => erreurs.push(`page : ${e.message}`));
  page.on('console', noterConsole);
  page.on('dialog', (d) => d.accept());  // (leaving the page while a copy is written: leave)
  await page.addInitScript(() => {
    window.nueeDemandes = [];
    window.__violations = [];
    document.addEventListener('securitypolicyviolation', (e) => window.__violations.push(`${e.violatedDirective} ${e.blockedURI}`));
  });
  await page.goto(url);
  await page.waitForFunction(() => document.querySelectorAll('.menu-haut').length > 3, null, { timeout: 60000 });
  return page;
}
const libelle = (page, cle) => page.evaluate((c) => import('./i18n.js').then((m) => m.t(c)), cle);
// the command palette (Ctrl+K), as a person uses it
async function commande(page, cle) {
  await page.evaluate(() => { document.querySelectorAll('.fond-dialogue').forEach((f) => f.remove()); if (document.activeElement) document.activeElement.blur(); });
  await page.keyboard.press('Control+K');
  await page.waitForSelector('.palette-saisie', { timeout: 5000 });
  await page.keyboard.type(await libelle(page, cle));
  await page.waitForTimeout(250);
  const trouve = await page.evaluate(() => (document.querySelector('.palette-item.actuel .palette-libelle') || {}).textContent || '');
  await page.keyboard.press('Enter');
  return trouve;
}
const graphiques = (page) => page.evaluate(() => document.querySelectorAll('.js-plotly-plot').length);
// the plots on screen (the page in front), with something drawn: their titles
const visibles = (page) => page.evaluate(() => [...document.querySelectorAll('.js-plotly-plot')]
  .filter((g) => g.offsetParent !== null && g.data && g.data.length)
  .map((g) => (g.layout && g.layout.title && (g.layout.title.text || g.layout.title)) || (g.closest('[data-vue]') || {}).dataset?.vue || '?'));
// the model in front: its type, name and the views of its page
const modeleActif = (page) => page.evaluate(async () => {
  const { app } = await import('./app.js');
  const id = app.modeleActif();
  const m = id && app.modelesDe(app.projetActif()).find((x) => x.id === id);
  return m ? { id, type: m.type, nom: m.nom } : null;
});
// a node of the tree double-clicked as a mouse does it (two clicks, then dblclick: explorateur.js,
// marquerNoeud), found by its words
async function doubleClic(page, texte) {
  const cle = await page.evaluate((x) => {
    const n = [...document.querySelectorAll('.arbre *')].find((e) => e.children.length === 0 && e.textContent.trim() === x);
    if (!n) throw new Error(`not in the tree: ${x}`);
    return n.closest('[role="treeitem"]').dataset.cle;
  }, texte);
  await page.dblclick(`.arbre [role="treeitem"][data-cle="${cle}"] .libelle`);
}
// Waiting for an async condition of the page: Playwright's waitForFunction takes the Promise of an
// async function as a truthy value at once, page.evaluate awaits it
async function attendre(page, f, arg, ms = 60000) {
  const fin = Date.now() + ms;
  for (;;) {
    let v;
    try { v = await page.evaluate(f, arg); } catch { v = false; }
    if (v) return v;
    if (Date.now() > fin) throw new Error(`attendu ${ms / 1000} s : ${String(f).slice(0, 120)}`);
    await page.waitForTimeout(500);
  }
}
const copiesAJour = (page) => attendre(page, () => import('./app.js').then((m) => m.app.copiesAJour()));
// the requests answered so far, and waiting for one more of a name
const demandes = (page) => page.evaluate(() => window.nueeDemandes.map((d) => ({ nom: d.nom, ms: Math.round(d.ms), ok: d.ok })));
async function attendreDemande(page, nom, avant, ms = 120000) {
  await page.waitForFunction(([n, a]) => window.nueeDemandes.filter((d) => d.nom === n).length > a, [nom, avant], { timeout: ms });
  return (await page.evaluate((n) => window.nueeDemandes.filter((d) => d.nom === n).pop(), nom));
}
const combien = (page, nom) => page.evaluate((n) => window.nueeDemandes.filter((d) => d.nom === n).length, nom);
// New model, filled in as Which analysis? fills it (dialogues.js: dialogueModele), then Fit
async function nouveauModele(page, reglages) {
  const avant = await combien(page, 'ajuster');
  await page.evaluate(async (r) => {
    const { app } = await import('./app.js');
    const { dialogueModele } = await import('./dialogues.js');
    const p = app.projet(app.projetActif());
    const u = (nom) => p.variables.find((v) => v.nom === nom).uid;
    // (an R-code model has no X or Y; a plugin's inputs are given by their variables' names)
    const q = { ...r, x: (r.x || []).map(u), y: (r.y || []).map(u), ...(r.aleatoires ? { aleatoires: r.aleatoires.map((v) => ({ by: 'variable', variable: u(v) })) } : {}),
      ...(r.entrees ? { options: { ...r.options, entrees: Object.fromEntries(Object.entries(r.entrees).map(([k, v]) => [k, v.map(u)])) } } : {}) };
    dialogueModele(app, p, q);
  }, reglages);
  await page.click('.fond-dialogue .pied-dialogue button.principal');
  const d = await attendreDemande(page, 'ajuster', avant).catch(() => null);
  const erreur = await page.evaluate(() => (document.querySelector('.fond-dialogue .erreur') || {}).textContent || '');
  return { d, erreur };
}
// the browser's recovery copies (secours_navigateur.js): the indexes, each with its entries
// (an index that can't be read is 'illisible', the others still read: a tab closed during its
// first write leaves its index empty, which the next tab takes for no copies; one removed while
// the folder is read isn't there)
const copies = (page) => page.evaluate(async () => {
  let d;
  try { d = await (await navigator.storage.getDirectory()).getDirectoryHandle('nuee-secours'); } catch { return {}; }
  const r = {};
  for await (const [nom, h] of d.entries()) {
    if (!nom.startsWith('index-')) continue;
    try { r[nom] = JSON.parse(await (await h.getFile()).text()); } catch (e) { if (e.name !== 'NotFoundError') r[nom] = 'illisible'; }
  }
  return r;
});
const ongletDe = (page) => page.evaluate(() => import('./secours_navigateur.js').then((m) => m.ONGLET));

let page;
try {
  // ---- R starts in the page
  const debut = Date.now();
  page = await ouvrirPage();
  verifier('isolation (SharedArrayBuffer)', await page.evaluate(() => crossOriginIsolated));
  await page.waitForFunction(() => window.nueeDemandes.length > 0 && !document.querySelector('.voile-webr'), null, { timeout: 600000 });
  res.demarrage_s = (Date.now() - debut) / 1000;
  const dehors = ailleurs();
  const absents = await page.evaluate(() => import('./api.js').then(async (a) => (await a.moteurWebR()).absents()));
  verifier('R démarré dans la page', true, `${res.demarrage_s.toFixed(1)} s ; paquets absents : ${absents.join(', ')}`);

  // ---- the site as served: the page's headers; where R's packages came from (the site's copy of
  // webR's repository when it has one: then nothing from another site until R was ready); under a
  // path, that path without its last slash redirected to it, the query kept
  const entetes = (await fetch(adresse)).headers;
  verifier('les en-têtes de la page : COOP et COEP, pas d’autre politique de sécurité que la sienne',
    entetes.get('cross-origin-opener-policy') === 'same-origin' && entetes.get('cross-origin-embedder-policy') === 'require-corp' && !entetes.has('content-security-policy'),
    ['cross-origin-opener-policy', 'cross-origin-embedder-policy', 'content-security-policy'].map((h) => `${h}: ${entetes.get(h)}`).join(' ; '));
  const miroir = await page.evaluate(() => window.NUEE_MIROIR_WEBR || null);
  const duSite = requetes.filter((u) => u.startsWith(adresse) && /\/bin\/emscripten\/contrib\/[\d.]+\/[^/]+\.tgz$/.test(u)).length;
  res.paquets = { miroir, du_site: duSite, ailleurs: dehors };
  verifier(miroir ? 'les paquets R depuis le site (sa copie du dépôt de webR), rien d’un autre site avant que R soit prêt' : 'les paquets R depuis repo.r-wasm.org (le site n’a pas de copie du dépôt de webR)',
    miroir ? duSite > 0 && dehors.length === 0 : dehors.length > 0 && dehors.every((u) => u.startsWith('https://repo.r-wasm.org/')),
    `${duSite} paquets du site, ${dehors.length} requêtes ailleurs${dehors.length ? ` (${[...new Set(dehors.map((u) => new URL(u).origin))].join(', ')})` : ''}`);
  if (new URL(adresse).pathname !== '/') {
    const sansBarre = `${adresse.replace(/\/$/, '')}?aides=0`;
    const r = await fetch(sansBarre, { redirect: 'manual' });
    const vers = r.headers.get('location') && new URL(r.headers.get('location'), adresse).href;
    verifier(`${new URL(sansBarre).pathname} redirigée vers ${new URL(adresse).pathname}`, r.status >= 301 && r.status <= 308 && vers === `${adresse}?aides=0`, `${r.status} ${vers}`);
  }
  const version = await page.evaluate(() => import('./api.js').then((a) => a.demander('version')));
  res.R = version.R;

  // ---- the example project, a PLS and its views
  res.exemple = await commande(page, 'projet_exemple');
  await page.waitForFunction(() => document.querySelectorAll('.js-plotly-plot').length >= 2, null, { timeout: 120000 });
  verifier('projet d’exemple ouvert, ses graphiques', true, `${res.exemple} : ${await graphiques(page)} graphiques`);
  // The explorer with the mouse: a click chooses a node and keeps its element in the page (the tree
  // isn't drawn again), so that a double click's dblclick reaches the page and opens its view.
  const clicArbre = await page.evaluate(() => {
    window.__dblclicks = [];
    // (as it goes down to the node: the node's handler stops it there)
    document.addEventListener('dblclick', (e) => { const n = e.target.closest && e.target.closest('[role="treeitem"]'); if (n) window.__dblclicks.push(n.dataset.cle); }, true);
    window.__noeudClique = document.querySelector('.arbre [role="treeitem"][data-cle$="/statistiques"]');
    return window.__noeudClique.dataset.cle;
  });
  await page.click(`.arbre [role="treeitem"][data-cle="${clicArbre}"] .libelle`);
  const apresClic = await page.evaluate(() => ({ garde: window.__noeudClique.isConnected, choisi: window.__noeudClique.classList.contains('actif') && window.__noeudClique.getAttribute('aria-selected') === 'true',
    autres: document.querySelectorAll('.arbre [role="treeitem"].actif').length }));
  const statsAvant = await page.evaluate(() => [...document.querySelectorAll('.dv-tab')].filter((x) => /Variable statistics/.test(x.textContent)).length);
  await page.dblclick(`.arbre [role="treeitem"][data-cle="${clicArbre}"] .libelle`);
  const statsOuvertes = await page.waitForFunction((n) => [...document.querySelectorAll('.dv-tab')].filter((x) => /Variable statistics/.test(x.textContent)).length > n, statsAvant, { timeout: 30000 })
    .then(() => true, () => false);
  const dblclicks = await page.evaluate(() => window.__dblclicks);
  verifier('l’explorateur à la souris : un clic choisit le nœud sans le redessiner, un double-clic ouvre sa vue (Statistiques des variables)',
    apresClic.garde && apresClic.choisi && apresClic.autres === 1 && statsOuvertes && dblclicks.includes(clicArbre),
    `${JSON.stringify(apresClic)} ; ouverte : ${statsOuvertes} ; dblclick reçu par la page : ${dblclicks.join(', ') || 'aucun'}`);
  const nAvant = await combien(page, 'ajuster');
  await doubleClic(page, 'M2');
  const pls = await attendreDemande(page, 'ajuster', nAvant);
  await page.waitForTimeout(1500);
  res.vues_pls = [];
  for (const v of ['vip', 'coefficients', 'obspred']) {
    res.vues_pls.push(await commande(page, v));
    await page.waitForTimeout(1500);
  }
  verifier('PLS M2 ajustée, ses vues', pls.ok && res.vues_pls.every(Boolean) && (await graphiques(page)) >= 3,
    `${Math.round(pls.ms)} ms ; ${res.vues_pls.join(', ')} ; ${await graphiques(page)} graphiques`);

  // ---- a mixed model and a PCoA, through New model: each one's page in front, its plots drawn
  const lmm = await nouveauModele(page, { type: 'LMM', x: ['sugar', 'firmness', 'acidity'], y: ['taste_score'], echelle: 'none', aleatoires: ['orchard'] });
  await page.waitForTimeout(3000);
  const mLmm = await modeleActif(page);
  const vLmm = await visibles(page);
  verifier('modèle mixte ajusté (New model), ses vues', !!lmm.d && lmm.d.ok && !lmm.erreur && mLmm && mLmm.type === 'LMM' && vLmm.length >= 2,
    lmm.erreur || `${lmm.d && Math.round(lmm.d.ms)} ms ; ${mLmm && mLmm.nom} ; ${vLmm.join(', ')}`);
  const chimie = ['weight', 'diameter', 'firmness', 'sugar', 'acidity', 'starch', 'dry_matter', 'polyphenols', 'colour_a', 'colour_b', 'juice', 'calcium', 'nitrogen', 'storage'];
  const pcoa = await nouveauModele(page, { type: 'PCOA', x: ['variety'], y: chimie, echelle: 'none', options: { distance: 'gower', permutations: 99, graine: 1 } });
  await page.waitForTimeout(3000);
  const mPcoa = await modeleActif(page);
  const vPcoa = await visibles(page);
  verifier('PCoA ajustée (New model), ses vues', !!pcoa.d && pcoa.d.ok && !pcoa.erreur && mPcoa && mPcoa.type === 'PCOA' && vPcoa.length >= 2,
    pcoa.erreur || `${pcoa.d && Math.round(pcoa.d.ms)} ms ; ${mPcoa && mPcoa.nom} ; ${vPcoa.join(', ')}`);
  if (options.capture !== false) await page.screenshot({ path: path.join(sortie, 'pcoa.png') });
  // (no part of a view left out written as the text "null" or "undefined", the mixed model's
  // Coefficients among them: replaceChildren writes what isn't a node as text)
  const vides = await page.evaluate(() => {
    const w = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
    const r = [];
    for (let x = w.nextNode(); x; x = w.nextNode()) {
      const t = x.textContent.trim();
      const p = x.parentElement;
      if ((t === 'null' || t === 'undefined') && p && !p.closest('td, .cm-editor, .console, textarea, pre, code')) r.push(`${t} : ${(p.closest('.dv-content-container') || p).textContent.trim().slice(0, 80)}`);
    }
    return r;
  });
  verifier('aucun « null » ni « undefined » écrit dans les vues ouvertes (les Coefficients du modèle mixte compris)', vides.length === 0, vides.join(' | ') || 'aucun');

  // ---- the data as CSV: a download
  const telechargement = page.waitForEvent('download', { timeout: 60000 });
  await commande(page, 'exporter_donnees');
  await page.waitForSelector('input[name="format-export"][value="csv"]', { timeout: 10000 });
  await page.check('input[name="format-export"][value="csv"]');
  await page.click('.fond-dialogue .pied-dialogue button.principal');
  const t = await telechargement;
  const csv = path.join(sortie, t.suggestedFilename());
  await t.saveAs(csv);
  const lignes = fs.readFileSync(csv, 'utf8').replace(/^﻿/, '').trim().split(/\r?\n/);
  verifier('export CSV téléchargé', lignes.length === 121 && /taste_score/.test(lignes[0]) && /variety/.test(lignes[0]), `${t.suggestedFilename()} : ${lignes.length} lignes, ${lignes[0].slice(0, 80)}…`);

  // ---- a package the browser version lacks: random forests say so, the rest goes on
  const foret = await nouveauModele(page, { type: 'FOREST', x: ['sugar', 'firmness', 'acidity'], y: ['taste_score'], echelle: 'none' });
  const bureau = await libelle(page, 'titre');  // (Nuée)
  verifier('forêt aléatoire : « il faut l’application de bureau »', /desktop application|application de bureau/.test(foret.erreur) && /ranger/.test(foret.erreur), foret.erreur || bureau);
  await page.evaluate(() => document.querySelectorAll('.fond-dialogue').forEach((f) => f.remove()));
  const nPls = await combien(page, 'ajuster');
  await doubleClic(page, 'M3');
  const plsda = await attendreDemande(page, 'ajuster', nPls);
  verifier('après le refus, une PLS-DA s’ajuste', plsda.ok, `${Math.round(plsda.ms)} ms`);

  // ---- the recovery copies: written (the last one after the PLS-DA, 3 s later), then offered after a reload
  const onglet1 = await ongletDe(page);
  const depuis = await page.evaluate(() => new Date().toISOString());
  await page.waitForTimeout(1000);  // (the PLS-DA kept, its project's summary asked again: its copy to come)
  await attendre(page, async ([o, t]) => {
    try {
      const { app } = await import('./app.js');
      const d = await (await navigator.storage.getDirectory()).getDirectoryHandle('nuee-secours');
      const e = JSON.parse(await (await (await d.getFileHandle(`index-${o}.json`)).getFile()).text());
      return app.copiesAJour() && e.length === 1 && e[0].modifie && e[0].date > t;
    } catch { return false; }
  }, [onglet1, depuis]);
  const avantRechargement = await copies(page);
  verifier('copie de secours écrite (OPFS)', (avantRechargement[`index-${onglet1}.json`] || []).length === 1,
    `${await page.evaluate(() => import('./secours_navigateur.js').then((m) => m.typeStockage()))} : ${JSON.stringify(avantRechargement[`index-${onglet1}.json`])}`);
  const modelesAvant = await page.evaluate(async () => { const { app } = await import('./app.js'); return app.modelesDe(app.projetActif()).map((m) => m.nom); });
  const definitionsAvant = await page.evaluate(async () => { const { app } = await import('./app.js'); return (app.projet(app.projetActif()).modeles || []).length; });
  await page.reload();
  await page.waitForFunction(() => document.querySelectorAll('.menu-haut').length > 3, null, { timeout: 60000 });
  const titreRecuperation = await libelle(page, 'recuperation');
  await page.waitForFunction((tt) => [...document.querySelectorAll('.fond-dialogue h2')].some((h) => h.textContent === tt), titreRecuperation, { timeout: 120000 });
  const proposes = await page.evaluate(() => [...document.querySelectorAll('.fond-dialogue li')].map((l) => l.textContent));
  await page.click('.fond-dialogue .pied-dialogue button.principal');
  await page.waitForFunction(() => window.nueeDemandes.some((d) => d.nom === 'ouvrir' && d.ok), null, { timeout: 300000 });
  await attendre(page, async (n) => { const { app } = await import('./app.js'); const pid = app.projetActif(); return pid && app.modelesDe(pid).length >= n; },
    modelesAvant.length, 300000).catch(() => {});
  await page.waitForTimeout(3000);
  const repris = await page.evaluate(async () => {
    const { app } = await import('./app.js');
    const p = app.projet(app.projetActif());
    return p ? { nom: p.nom, modifie: p.modifie, modeles: app.modelesDe(p.id).map((m) => m.nom), definitions: (p.modeles || []).length } : null;
  });
  verifier('rechargé : le projet proposé, repris avec ses modèles et ses changements non enregistrés',
    proposes.length === 1 && !!repris && repris.modifie === true && repris.definitions === definitionsAvant
      && JSON.stringify([...repris.modeles].sort()) === JSON.stringify([...modelesAvant].sort()),
    `proposé : ${proposes.join(' | ')} ; repris : ${JSON.stringify(repris)} ; avant : ${definitionsAvant} définitions, ${modelesAvant.join(', ')}`);

  // ---- a second tab meanwhile: offered nothing (the first one is open)
  const onglet2 = await ongletDe(page);
  await attendre(page, async (o) => {
    try {
      const d = await (await navigator.storage.getDirectory()).getDirectoryHandle('nuee-secours');
      return JSON.parse(await (await (await d.getFileHandle(`index-${o}.json`)).getFile()).text()).length === 1;
    } catch { return false; }
  }, onglet2);
  await copiesAJour(page);
  const autre = await ouvrirPage();
  await autre.waitForTimeout(5000);
  const dialogueAutre = await autre.evaluate((tt) => [...document.querySelectorAll('.fond-dialogue h2')].some((h) => h.textContent === tt), titreRecuperation);
  verifier('un second onglet ouvert en même temps : rien ne lui est proposé', !dialogueAutre);
  await autre.close();
  const apresReprise = await copies(page);
  verifier('les copies de l’onglet rechargé parties une fois reprises, celles du nouvel onglet là',
    !apresReprise[`index-${onglet1}.json`] && (apresReprise[`index-${onglet2}.json`] || []).length === 1, Object.keys(apresReprise).join(', '));
  if (options.capture !== false) await page.screenshot({ path: path.join(sortie, 'navigateur.png') });

  // ---- the tab closed: the next one offers the project; set aside, the copies go
  verifier('sans violation de la politique de sécurité', (await page.evaluate(() => window.__violations)).length === 0, await page.evaluate(() => window.__violations));
  res.demandes = await demandes(page);
  await copiesAJour(page);
  await page.close({ runBeforeUnload: true });
  page = await ouvrirPage();
  await page.waitForFunction((tt) => [...document.querySelectorAll('.fond-dialogue h2')].some((h) => h.textContent === tt), titreRecuperation, { timeout: 120000 });
  const proposes2 = await page.evaluate(() => [...document.querySelectorAll('.fond-dialogue li')].map((l) => l.textContent));
  await page.click('.fond-dialogue .pied-dialogue button:not(.principal)');
  await page.waitForTimeout(2000);
  const apresIgnorer = await copies(page);
  const onglet3 = await ongletDe(page);
  verifier('onglet fermé : le projet proposé au suivant ; ignoré, ses copies parties', proposes2.length === 1 && !apresIgnorer[`index-${onglet2}.json`]
    && Object.keys(apresIgnorer).every((k) => k === `index-${onglet3}.json`), `${proposes2.join(' | ')} ; restent : ${Object.keys(apresIgnorer).join(', ') || 'aucune'}`);
  await page.close({ runBeforeUnload: true });

  // ---- a browser whose OPFS can't write (no createWritable): the copies in IndexedDB
  const sansOpfs = await contexte.newPage();
  sansOpfs.on('pageerror', (e) => erreurs.push(`page : ${e.message}`));
  sansOpfs.on('dialog', (d) => d.accept());
  await sansOpfs.addInitScript(() => { window.nueeDemandes = []; if (window.FileSystemFileHandle) delete FileSystemFileHandle.prototype.createWritable; });
  await sansOpfs.goto(adresse);
  await sansOpfs.waitForFunction(() => document.querySelectorAll('.menu-haut').length > 3, null, { timeout: 60000 });
  page = sansOpfs;
  await commande(page, 'projet_exemple');
  await page.waitForFunction(() => window.nueeDemandes.some((d) => d.nom === 'exemple' && d.ok), null, { timeout: 300000 });
  const lmm2 = await nouveauModele(page, { type: 'LMM', x: ['sugar'], y: ['taste_score'], echelle: 'none', aleatoires: ['orchard'] });
  const depuis2 = await page.evaluate(() => new Date().toISOString());
  const indexIdb = () => page.evaluate(() => new Promise((ok) => {
    const r = indexedDB.open('nuee-secours', 1);
    r.onerror = () => ok(null);
    r.onsuccess = () => {
      try {
        const req = r.result.transaction('fichiers', 'readonly').objectStore('fichiers').getAll();
        req.onsuccess = () => {
          const textes = req.result.map((v) => new TextDecoder().decode(v));
          ok(textes.filter((x) => x.startsWith('[')).map((x) => JSON.parse(x)).flat());
        };
        req.onerror = () => ok(null);
      } catch { ok(null); }
    };
  }));
  await attendre(page, () => import('./secours_navigateur.js').then((m) => m.typeStockage()).then((t) => t === 'IndexedDB'), null, 30000);
  for (let i = 0; i < 60; i += 1) { const e = await indexIdb(); if (e && e.length === 1 && e[0].date > depuis2) break; await page.waitForTimeout(500); }
  await copiesAJour(page);
  const entreesIdb = await indexIdb();
  await page.reload();
  await page.waitForFunction((tt) => [...document.querySelectorAll('.fond-dialogue h2')].some((h) => h.textContent === tt), titreRecuperation, { timeout: 120000 });
  await page.click('.fond-dialogue .pied-dialogue button.principal');
  await page.waitForFunction(() => window.nueeDemandes.some((d) => d.nom === 'ouvrir' && d.ok), null, { timeout: 300000 });
  await page.waitForTimeout(3000);
  const reprisIdb = await page.evaluate(async () => {
    const { app } = await import('./app.js');
    const p = app.projet(app.projetActif());
    return p ? { nom: p.nom, modifie: p.modifie, definitions: (p.modeles || []).length } : null;
  });
  verifier('sans OPFS : copie dans IndexedDB, reprise après un rechargement', !!lmm2.d && lmm2.d.ok && (entreesIdb || []).length === 1 && !!reprisIdb && reprisIdb.modifie === true && reprisIdb.definitions === 5,
    `${JSON.stringify(entreesIdb)} ; repris : ${JSON.stringify(reprisIdb)}`);
  await sansOpfs.close({ runBeforeUnload: true });

  // ---- 1.1 in the page, in a tab of its own: what needs the desktop application says so; a table
  // opened, a data step, Data steps, Update the data with next year's file chosen in the page, What
  // changed; a project saved, opened, saved again (downloaded each time); an R-code model approved,
  // its package installed from webR's repository, and one that webR can't have; a plugin installed
  // from its file; then, the tab reloaded, the plugin and the approvals still there
  page = await ouvrirPage();
  await page.waitForFunction(() => window.nueeDemandes.length > 0 && !document.querySelector('.voile-webr'), null, { timeout: 600000 });
  const pidActif = () => page.evaluate(() => import('./app.js').then((m) => m.app.projetActif()));
  const projetActif = () => page.evaluate(async () => {
    const { app } = await import('./app.js');
    const p = app.projet(app.projetActif());
    return p ? { id: p.id, nom: p.nom, n: p.observations.uid.length, variables: p.variables.map((v) => v.nom), modeles: app.modelesDe(p.id).map((m) => m.nom) } : null;
  });
  const cliquerTexte = (selecteur, texte) => page.evaluate(([s, x]) => {
    const b = [...document.querySelectorAll(s)].find((e) => e.textContent.includes(x));
    if (!b) throw new Error(`no ${s} with ${x}`);
    b.click();
  }, [selecteur, texte]);
  // (File › Open, the file chosen in the page)
  async function ouvrirFichier(f) {
    const n = await combien(page, 'ouvrir');
    const choix = page.waitForEvent('filechooser', { timeout: 15000 });
    await commande(page, 'ouvrir');
    await (await choix).setFiles(f);
    const d = await attendreDemande(page, 'ouvrir', n);
    await page.waitForTimeout(1500);
    return d;
  }
  // the table exported above, and next year's: ten more rows
  const lignesCsv = fs.readFileSync(csv, 'utf8').replace(/^\uFEFF/, '').trim().split(/\r?\n/);
  const csv2027 = path.join(sortie, 'orchards_2027.csv');
  fs.writeFileSync(csv2027, [...lignesCsv, ...lignesCsv.slice(1, 11).map((l) => l.replace(/^"P/, '"N'))].join('\n'));
  const ouvert = await ouvrirFichier(csv);
  const tableau = await projetActif();
  verifier('un tableau ouvert dans la page (File › Open)', !!ouvert && ouvert.ok && !!tableau && tableau.n === 120, JSON.stringify(tableau && { nom: tableau.nom, n: tableau.n }));

  // Share on a table never saved: History (the way to Share for a project without a file, as on the
  // desktop) says it was never saved here, with Save (History and sharing in the page: 1.2, below)
  await commande(page, 'versions_partager');
  const jamaisSauve = await libelle(page, 'versions_jamais_enregistre_navigateur');
  const historiqueTableau = await attendre(page, (x) => [...document.querySelectorAll('.versions')].some((v) => v.offsetParent !== null && v.textContent.includes(x)), jamaisSauve, 30000).catch(() => false);
  verifier('Partager un tableau jamais enregistré : l’Historique dit qu’il ne l’a jamais été ici, avec Enregistrer', historiqueTableau);

  // a model, then a data step: a new variable (its dialog)
  const lmCsv = await nouveauModele(page, { type: 'LM', x: ['sugar'], y: ['taste_score'], echelle: 'none' });
  await page.waitForTimeout(1000);
  const nVar = await combien(page, 'variable_calculee');
  await commande(page, 'nouvelle_variable');
  await page.waitForSelector('.dialogue-variable textarea.formule', { timeout: 10000 });
  await page.fill('.dialogue-variable .grille-champs input', 'sugar_acid');
  await page.fill('.dialogue-variable textarea.formule', 'sugar / acidity');
  await page.click('.fond-dialogue .pied-dialogue button.principal');
  const variable = await attendreDemande(page, 'variable_calculee', nVar);
  await page.waitForTimeout(1000);
  await commande(page, 'etapes_donnees');
  await page.waitForFunction(() => { const c = document.querySelector('.contenu-etapes'); return c && !c.querySelector('.attente'); }, null, { timeout: 30000 });
  const texteEtapes = await page.evaluate(() => document.querySelector('.contenu-etapes').textContent);
  verifier('une étape (Nouvelle variable), Données › Étapes : la source et l’étape', !!lmCsv.d && lmCsv.d.ok && variable.ok && /orchards\.csv/.test(texteEtapes) && /sugar_acid/.test(texteEtapes),
    texteEtapes.replace(/\s+/g, ' ').slice(0, 300));
  await page.evaluate(() => document.querySelectorAll('.fond-dialogue').forEach((f) => f.remove()));

  // Update the data: next year's file chosen in the page, its preview, then the update, What changed
  const nMaj = await combien(page, 'mettre_a_jour');
  await commande(page, 'etapes_mettre_a_jour');
  await page.waitForSelector('.dialogue-maj .choisir-fichier-maj', { timeout: 20000 });
  const choixMaj = page.waitForEvent('filechooser', { timeout: 15000 });
  await page.click('.dialogue-maj .choisir-fichier-maj');
  await (await choixMaj).setFiles(csv2027);
  // (read as it is waited for: the preview is drawn again meanwhile)
  const apercuMaj = await (await page.waitForFunction(() => { const r = document.querySelector('.dialogue-maj .resume-maj'); return r ? r.textContent : null; }, null, { timeout: 60000 })).jsonValue();
  await page.waitForFunction(() => { const b = document.querySelector('.fond-dialogue .pied-dialogue button.principal'); return b && !b.disabled; }, null, { timeout: 30000 });
  await page.click('.fond-dialogue .pied-dialogue button.principal');
  await page.waitForFunction(([a]) => window.nueeDemandes.filter((d) => d.nom === 'mettre_a_jour' && !(d.corps && d.corps.apercu)).length > 0 && window.nueeDemandes.filter((d) => d.nom === 'mettre_a_jour').length > a, [nMaj], { timeout: 120000 });
  await page.waitForFunction(() => document.querySelector('.dv-default-tab[data-panneau^="changements:"]'), null, { timeout: 120000 }).catch(() => {});
  await page.waitForTimeout(1500);
  const apresMaj = await projetActif();
  const changements = await page.evaluate(() => [...document.querySelectorAll('.dv-default-tab[data-panneau^="changements:"]')].map((e) => e.textContent));
  const majOk = await page.evaluate(() => window.nueeDemandes.filter((d) => d.nom === 'mettre_a_jour' && !(d.corps && d.corps.apercu)).pop());
  verifier('Mettre à jour les données (le fichier choisi dans la page) : 130 observations, l’étape rejouée, Ce qui a changé',
    !!majOk && majOk.ok && !!apresMaj && apresMaj.n === 130 && apresMaj.variables.includes('sugar_acid') && changements.length > 0,
    `aperçu : ${apercuMaj.slice(0, 160)} ; après : ${JSON.stringify(apresMaj && { n: apresMaj.n, modeles: apresMaj.modeles })} ; ${changements.join(', ')}`);

  // Save downloads the project; opened again in the page, Save downloads it again (never into R's
  // file system of the tab, where an opened file is put)
  const telechargementNuee = page.waitForEvent('download', { timeout: 60000 });
  await commande(page, 'enregistrer');
  const tn = await telechargementNuee;
  const nuee = path.join(sortie, tn.suggestedFilename());
  await tn.saveAs(nuee);
  await ouvrirFichier(nuee);
  const rouvert = await projetActif();
  const telechargementNuee2 = page.waitForEvent('download', { timeout: 30000 }).catch(() => null);
  await commande(page, 'enregistrer');
  const tn2 = await telechargementNuee2;
  verifier('un projet enregistré, rouvert dans la page, enregistré de nouveau : téléchargé chaque fois', !!tn2 && /\.nuee$/.test(tn.suggestedFilename()) && !!rouvert && rouvert.n === 130,
    `${tn.suggestedFilename()} ; rouvert : ${rouvert && rouvert.nom} ; ${tn2 ? tn2.suggestedFilename() : 'pas de second téléchargement'}`);

  // File › Export a reproducibility package: written in R's files of the page, then downloaded (the
  // page stays: no address of the local server)
  const telechargementPaquet = page.waitForEvent('download', { timeout: 180000 }).catch(() => null);
  await commande(page, 'paquet');
  await page.waitForSelector('.dialogue-paquet .paquet-auteurs tbody tr input', { timeout: 15000 });
  const champsAuteur = page.locator('.dialogue-paquet .paquet-auteurs tbody tr').first().locator('input');
  await champsAuteur.nth(0).fill('Alice');
  await champsAuteur.nth(1).fill('Exemple');
  await page.click('.fond-dialogue .pied-dialogue button.principal');
  const tp = await telechargementPaquet;
  const zip = tp ? path.join(sortie, tp.suggestedFilename()) : null;
  if (tp) await tp.saveAs(zip);
  const octetsZip = zip ? fs.readFileSync(zip) : Buffer.alloc(0);
  const dansZip = (nom) => octetsZip.includes(Buffer.from(nom));
  const toujoursLa = await projetActif().catch(() => null);
  verifier('un paquet de reproductibilité exporté dans la page : téléchargé (.zip), la page restée',
    !!tp && /\.zip$/.test(tp.suggestedFilename()) && octetsZip.subarray(0, 2).toString() === 'PK'
      && ['README.md', 'CITATION.cff', 'MANIFEST.sha256', '.nuee', 'results/key_results.json'].every(dansZip) && !!toujoursLa && toujoursLa.n === 130,
    tp ? `${tp.suggestedFilename()} : ${octetsZip.length} octets` : 'pas de téléchargement');

  // R code: approved when fitted, its package (Kendall), which the site's copy doesn't have, installed
  // from webR's repository; ranger refused
  const avantKendall = requetes.length;
  const codeR = 'k <- Kendall::Kendall(donnees$sugar, donnees$taste_score)\nfit <- lm(taste_score ~ sugar + acidity, data = donnees)\nlist(modele = fit, valeurs = c(tau = unname(k$tau)))';
  const rc = await nouveauModele(page, { type: 'CODE', options: { code: codeR } });
  await page.waitForTimeout(1500);
  const mCode = await modeleActif(page);
  const kendall = requetes.slice(avantKendall).filter((u) => /\/Kendall_[^/]+\.tgz$/.test(u));
  verifier('un modèle de code R approuvé, son paquet (Kendall) installé depuis le dépôt de webR', !!rc.d && rc.d.ok && !rc.erreur && !!mCode && mCode.type === 'CODE'
    && kendall.length > 0 && kendall.every((u) => u.startsWith('https://repo.r-wasm.org/')), rc.erreur || `${rc.d && Math.round(rc.d.ms)} ms ; ${mCode && mCode.nom} ; ${kendall.join(', ')}`);
  const rr = await nouveauModele(page, { type: 'CODE', options: { code: 'library(ranger)\nlm(taste_score ~ sugar, data = donnees)' } });
  verifier('un code R qui demande ranger : « il faut l’application de bureau »', /desktop application is needed|application de bureau/.test(rr.erreur) && /ranger/.test(rr.erreur), rr.erreur);
  await page.evaluate(() => document.querySelectorAll('.fond-dialogue').forEach((f) => f.remove()));

  // a plugin installed from its file (File › Plugins… › Install from file…), then fitted
  const nInst = await combien(page, 'greffon_installer');
  await commande(page, 'greffons');
  await page.waitForSelector('.dialogue-greffons', { timeout: 30000 });
  const choixGreffon = page.waitForEvent('filechooser', { timeout: 15000 });
  await cliquerTexte('.dialogue-greffons button', await libelle(page, 'greffon_installer_fichier'));
  await (await choixGreffon).setFiles(path.join(APP, '..', 'docs', 'site', 'greffons', 'mann-kendall.nueeplugin'));
  await page.waitForSelector('.installation-greffon', { timeout: 30000 });
  await page.evaluate(() => document.querySelector('.installation-greffon').closest('.fond-dialogue').querySelector('.pied-dialogue button.principal').click());
  const installe = await attendreDemande(page, 'greffon_installer', nInst, 60000).catch(() => null);
  await page.waitForTimeout(1500);
  await page.evaluate(() => document.querySelectorAll('.fond-dialogue').forEach((f) => f.remove()));
  const mk = await nouveauModele(page, { type: 'PLUGIN:mann-kendall', options: { greffon: { id: 'mann-kendall' }, options: {} }, entrees: { series: ['taste_score'] } });
  await page.waitForTimeout(1500);
  const mGreffon = await modeleActif(page);
  verifier('un greffon installé depuis son fichier dans la page, puis ajusté', !!installe && installe.ok && !!mk.d && mk.d.ok && !mk.erreur && !!mGreffon && mGreffon.type === 'PLUGIN',
    mk.erreur || `${mGreffon && mGreffon.nom}`);

  // reloaded: the project offered and recovered, its R-code model still approved; the plugin still installed
  await copiesAJour(page);
  await page.reload();
  await page.waitForFunction((tt) => [...document.querySelectorAll('.fond-dialogue h2')].some((h) => h.textContent === tt), titreRecuperation, { timeout: 120000 });
  await page.click('.fond-dialogue .pied-dialogue button.principal');
  await page.waitForFunction(() => window.nueeDemandes.filter((d) => d.nom === 'ouvrir' && d.ok).length >= 1, null, { timeout: 300000 });
  await page.waitForTimeout(4000);
  const apresRechargement = await page.evaluate(async () => {
    const { app } = await import('./app.js');
    const { demander } = await import('./api.js');
    const g = await demander('greffons');
    const codes = [];
    for (const p of app.projets()) codes.push(...[].concat((await demander('code_modeles', { projet: p.id })).modeles || []));
    return { greffons: [].concat(g.liste || []).map((x) => ({ id: x.id, permis: x.permis })), codes: codes.map((c) => ({ nom: c.nom, approuve: c.approuve })) };
  });
  // Stop: R in the page interrupted, in the permutation test shared with the other R of the page
  // (999) and in a PCoA's permutations in one R (99,999); each answers "stopped", the project as it
  // was, and the next request as usual
  const ex = await page.evaluate(async () => {
    const a = await import('./api.js');
    const p = await a.demander('exemple', { nom: 'pommes' });
    const m = await a.demander('ajuster', { projet: p.id, numero: 2 });
    return { pid: p.id, m2: m.id, definitions: [].concat(p.modeles || []).length };
  });
  // Where the computation is, in the page as on the desktop: R says its request and its loops'
  // rounds on the tab's channel (navigateur.R: progres_tache, progres_navigateur). The permutation
  // test shared with the other R (99, its preparation first): this R's piece says its rounds on the
  // whole 99, rising.
  const progresPartage = await page.evaluate(async ({ m2 }) => {
    const a = await import('./api.js');
    const { CANAL_PROGRES } = await import('./progression.js');
    const messages = [];
    const canal = new BroadcastChannel(CANAL_PROGRES);
    canal.onmessage = ({ data }) => messages.push(data);
    const t0 = performance.now();
    await a.demander('permutations', { modele: m2, n: 99 });
    canal.close();
    const etapes = messages.filter((m) => m.nom === 'permutations' && m.etapes.length && m.etapes[0].quoi === 'permutations').map((m) => m.etapes[0]);
    return { s: (performance.now() - t0) / 1000, debuts: messages.filter((m) => m.nom === 'permutations' && !m.etapes.length).length,
      etapes: etapes.map((e) => `${e.quoi} ${e.fait}/${e.total}`), monte: etapes.every((e, i) => !i || e.fait >= etapes[i - 1].fait) };
  }, ex);
  verifier('où en est le calcul, dit par le R de la page : sa demande, puis les tours du test de permutation partagé sur ses 99',
    progresPartage.debuts >= 1 && progresPartage.etapes.length >= 1 && progresPartage.monte
      && progresPartage.etapes.every((e) => /^permutations \d+\/99$/.test(e) && Number(e.split(' ')[1].split('/')[0]) <= 99),
    `${progresPartage.s.toFixed(1)} s ; ${progresPartage.debuts} début(s) ; ${progresPartage.etapes.join(', ')}`);
  // (before Stop, the status bar, as the desktop's: the step, a bar that fills, the time left)
  const barreStatut = () => page.waitForFunction(() => {
    const b = document.querySelector('.progres-statut .progres');
    const texte = b ? b.querySelector('.progres-texte').textContent : '';
    return b && b.hasAttribute('aria-valuenow') && /Permutation [\d,]+ of 99,999/.test(texte) && /left/.test(texte)
      ? { texte, valeur: Number(b.getAttribute('aria-valuenow')) } : null;
  }, null, { timeout: 45000, polling: 100 }).then((h) => h.jsonValue(), () => null);
  const arreter = async (f, arg, barre = false) => {
    const fini = page.evaluate(f, arg);
    await page.waitForSelector('#arret-calcul:not([hidden]) button:not([disabled])', { timeout: 60000 });
    const vue = barre ? await barreStatut() : null;
    await page.click('#arret-calcul button');
    return { ...(await fini), barre: vue };
  };
  const arretPermutations = await arreter(async ({ m2 }) => {
    const a = await import('./api.js');
    const t0 = performance.now();
    try { await a.demander('permutations', { modele: m2, n: 999 }); return { fini: true, s: (performance.now() - t0) / 1000 }; } catch (e) {
      return { arrete: !!(e.reponse && e.reponse.arrete), message: e.message, s: (performance.now() - t0) / 1000 };
    }
  }, ex);
  const arretPcoa = await arreter(async ({ pid }) => {
    const a = await import('./api.js');
    const p = await a.demander('projet', { projet: pid });
    const u = (n) => p.variables.find((v) => v.nom === n).uid;
    const y = ['weight', 'diameter', 'firmness', 'sugar', 'acidity', 'starch', 'dry_matter', 'polyphenols', 'colour_a', 'colour_b', 'juice', 'calcium', 'nitrogen', 'storage'];
    const t0 = performance.now();
    try {
      await a.demander('ajuster', { projet: pid, type: 'PCOA', x: [u('variety')], y: y.map(u), echelle: 'none', options: { distance: 'gower', permutations: 99999, graine: 1 }, nom: 'PCoA' });
      return { fini: true, s: (performance.now() - t0) / 1000 };
    } catch (e) { return { arrete: !!(e.reponse && e.reponse.arrete), message: e.message, s: (performance.now() - t0) / 1000 }; }
  }, ex, true);
  verifier('la barre d’état dans la page, comme dans l’application de bureau : l’étape d’une PCoA, la barre qui avance, le temps restant',
    !!arretPcoa.barre && arretPcoa.barre.valeur >= 0, arretPcoa.barre ? `${arretPcoa.barre.texte} (${arretPcoa.barre.valeur} %)` : 'rien');
  const apresArret = await page.evaluate(async ({ pid }) => {
    const a = await import('./api.js');
    const p = await a.demander('projet', { projet: pid });
    const m = await a.demander('ajuster', { projet: pid, numero: 1 });
    return { definitions: [].concat(p.modeles || []).length, pca: !!m.id };
  }, ex);
  verifier('Arrêter : le test de permutation partagé et une PCoA arrêtés, rien de changé, la demande suivante comme d’habitude',
    arretPermutations.arrete && arretPcoa.arrete && apresArret.definitions === ex.definitions && apresArret.pca,
    `${JSON.stringify(arretPermutations)} ; ${JSON.stringify(arretPcoa)} ; ${JSON.stringify(apresArret)}`);

  await commande(page, 'guide');
  const aide = await attendre(page, () => { const c = document.querySelector('.aide-corps'); return c && c.textContent.length > 500 ? c.textContent.length : 0; }, null, 30000).catch(() => 0);
  verifier('le guide dans le panneau d’aide (F1)', aide > 500, `${aide} caractères`);
  verifier('rechargé : le greffon encore installé et approuvé, le code R encore approuvé',
    apresRechargement.greffons.some((x) => x.id === 'mann-kendall' && x.permis) && apresRechargement.codes.length >= 1 && apresRechargement.codes.some((c) => c.approuve),
    JSON.stringify(apresRechargement));

  // ?langue=fr (nuee.io's French page links there): a new profile, no language kept yet, gets the
  // interface in French; a choice made since (View › Language) wins over the address at the next visit
  fs.rmSync(path.join(sortie, 'profil-fr'), { recursive: true, force: true });
  const contexteFr = await lancer(path.join(sortie, 'profil-fr'), { headless: !options.visible });
  try {
    const p = await contexteFr.newPage();
    p.on('dialog', (d) => d.accept());
    const langueEtMenus = async () => {
      await p.waitForFunction(() => document.querySelectorAll('.menu-haut').length > 3, null, { timeout: 60000 });
      return p.evaluate(() => import('./i18n.js').then((m) => `${m.langueActuelle()} : ${[...document.querySelectorAll('.menu-haut')].slice(0, 3).map((b) => b.textContent).join(', ')}`));
    };
    await p.goto(`${adresse}?langue=fr`);
    const neuf = await langueEtMenus();
    await p.evaluate(() => import('./app.js').then((m) => m.app.changerLangue('en')));
    await p.goto(`${adresse}?langue=fr`);
    const apresChoix = await langueEtMenus();
    verifier('?langue=fr : un profil neuf en français ; un choix fait ensuite (Affichage › Langue) l’emporte sur l’adresse', /^fr : Fichier/.test(neuf) && /^en : File/.test(apresChoix), `${neuf} | ${apresChoix}`);
  } finally {
    await contexteFr.close().catch(() => {});
  }

  // ---- 1.2, a reproducibility package's link: https://nuee.io/app/?doi=<doi>, against a stand-in for
  // Zenodo's API (Playwright's routes; zenodo.org itself never resolves here). The package written in
  // the page (the example's four models, a mixed model and a PCoA, a document citing M1, its main one,
  // with the DOI of record 7000002) is the record's .zip, beside a PDF; its link opens it in a tab of
  // its own: the document first, without R (lecture.js), then Rerun the models: each model gives its
  // deposited results. A concept DOI gives the record of its latest version (Zenodo
  // redirects /api/records/<concept> to it, which fetch follows: the stand-in answers that record), a
  // record by its id; a DOI Zenodo doesn't know, a record without a package and no network are said.
  const paquet = await page.evaluate(async () => {
    const { app } = await import('./app.js');
    const a = await import('./api.js');
    const p = await a.demander('exemple', { nom: 'pommes' });
    await app.projetOuvert(p);
    const u = (nom) => p.variables.find((v) => v.nom === nom).uid;
    for (const d of p.modeles) await app.calculerDiscret(p.id, d.numero);
    await app.calculerDiscret(p.id, (await a.demander('ajuster', { projet: p.id, type: 'LMM', x: [u('sugar'), u('firmness')], y: [u('taste_score')], echelle: 'none', nom: 'R1',
      aleatoires: [{ by: 'variable', variable: u('orchard') }] })).numero);
    await app.calculerDiscret(p.id, (await a.demander('ajuster', { projet: p.id, type: 'PCOA', x: [u('variety')], y: ['sugar', 'acidity', 'firmness', 'starch'].map(u), echelle: 'none',
      options: { distance: 'gower', permutations: 99, graine: 1 }, nom: 'P1' })).numero);
    const ids = app.modelesSeance(p.id).map((m) => m.id);
    // (the document, as the package's dialog writes it: documents.js, documentPourPaquet; its page
    // filled in, the main one)
    const texte = '---\ntitle: What the apples say\n---\n\nThe PCA’s Q² is {{M1.Q2}}.\n\n{{M1.scores | Scores of the 120 apples}}\n\n{{M1.table.resume | The components}}\n';
    const rd = await a.demander('document_enregistrer', { projet: p.id, titre: 'What the apples say', langue: 'en', texte });
    app.majProjet(rd.projet);
    const doc = [].concat(rd.projet.documents)[0];
    const d = await (await import('./documents.js')).documentPourPaquet(app, p.id, doc.id, 'What the apples say');
    const r = await a.demander('paquet', { projet: p.id, meta: { titre: 'Orchards rerun', auteurs: [{ prenom: 'Alice', nom: 'Exemple', orcid: '' }], licence: 'CC-BY-4.0', principal: doc.id },
      modeles: ids, documents: JSON.stringify([{ ...d, principal: true }]), session: app.sessionProjet(p.id), doi: '10.5281/zenodo.7000002', version: 'essai', langue: 'en' });
    const o = await a.lireFichier(r.chemin);
    let s = '';
    for (let i = 0; i < o.length; i += 8192) s += String.fromCharCode(...o.subarray(i, i + 8192));
    return { nom: r.chemin.split('/').pop(), octets: btoa(s), n: ids.length };
  });
  const octetsPaquet = Buffer.from(paquet.octets, 'base64');
  const recues = [];
  let horsLigne = false;
  const enregistrement = (id, fichiers) => ({ id, recid: String(id), doi: `10.5281/zenodo.${id}`, conceptdoi: '10.5281/zenodo.7000000', conceptrecid: '7000000',
    title: 'Orchards rerun', metadata: { title: 'Orchards rerun' }, links: { self: `https://zenodo.org/api/records/${id}`, files: `https://zenodo.org/api/records/${id}/files` },
    files: fichiers.map(([key, o, taille]) => ({ id: `f-${key}`, key, size: taille ?? o.length, checksum: 'md5:0', links: { self: `https://zenodo.org/api/records/${id}/files/${encodeURIComponent(key)}/content` } })) });
  const fichiers7000002 = [['notes.pdf', Buffer.from('%PDF-1.4')], [paquet.nom, octetsPaquet]];
  await contexte.route('https://zenodo.org/**', (route) => {
    const u = new URL(route.request().url());
    recues.push(`${route.request().method()} ${u.pathname}${u.search}`);
    if (horsLigne) return route.abort('internetdisconnected');
    const json = (statut, x) => route.fulfill({ status: statut, headers: { 'Access-Control-Allow-Origin': '*', 'Content-Type': 'application/json' }, body: JSON.stringify(x) });
    if (u.pathname === '/api/records/7000002' || u.pathname === '/api/records/7000000') return json(200, enregistrement(7000002, fichiers7000002));
    if (u.pathname === '/api/records/7000005') return json(200, enregistrement(7000005, [['notes.pdf', Buffer.from('%PDF-1.4')]]));
    // (a package of 612 MB, by the size Zenodo gives: its content is never asked for)
    if (u.pathname === '/api/records/7000007') return json(200, enregistrement(7000007, [['notes.pdf', Buffer.from('%PDF-1.4')], ['Huge package.zip', Buffer.alloc(0), 612345678]]));
    if (u.pathname === `/api/records/7000002/files/${encodeURIComponent(paquet.nom)}/content`) {
      return route.fulfill({ status: 200, headers: { 'Access-Control-Allow-Origin': '*', 'Content-Type': 'application/octet-stream' }, body: octetsPaquet });
    }
    if (u.pathname === '/api/records') return json(200, { hits: { hits: [], total: 0 }, links: {} });
    return json(404, { status: 404, message: 'The persistent identifier does not exist.' });
  });
  const autresSites = [];
  // (`taille`: a phone's screen; the page's own requests in p.requetes)
  const ouvrirLien = async (requete, taille = null) => {
    const p = await contexte.newPage();
    p.requetes = [];
    p.on('pageerror', (e) => erreurs.push(`page : ${e.message}`));
    p.on('console', noterConsole);
    p.on('request', (r) => {
      p.requetes.push(r.url());
      const h = new URL(r.url()).hostname;
      if (![new URL(adresse).hostname, 'zenodo.org', 'repo.r-wasm.org'].includes(h) && !/^(data|blob):/.test(r.url())) autresSites.push(r.url());
    });
    await p.addInitScript(() => { window.nueeDemandes = []; window.__violations = []; document.addEventListener('securitypolicyviolation', (e) => window.__violations.push(`${e.violatedDirective} ${e.blockedURI}`)); });
    if (taille) await p.setViewportSize(taille);
    await p.goto(`${adresse}?${requete}`);
    return p;
  };
  // the reader's page (lecture.js): its document in its frame (sandboxed: no script, nothing of the
  // page), what the page shows of it
  const pageLecteur = (p) => attendre(p, () => {
    const f = document.querySelector('#lecture iframe.document-lecture');
    if (!f || !f.srcdoc) return null;
    return { titre: document.querySelector('.titre-lecture h1').textContent, sandbox: f.getAttribute('sandbox'), largeur: Math.round(f.getBoundingClientRect().width),
      fenetre: innerWidth, defile: document.documentElement.scrollWidth > innerWidth + 1, demandes: window.nueeDemandes.length, violations: window.__violations.length,
      document: /<h1 class="titre-document">What the apples say<\/h1>/.test(f.srcdoc), q2: /Q² is <span class="ref"[^>]*>0[.,]\d+<\/span>/.test(f.srcdoc),
      figure: /<figure class="ref-figure"[^>]*><img src="data:image\/png;base64,/.test(f.srcdoc), tableau: /<div class="tableau-defile"><table>/.test(f.srcdoc),
      boutons: [...document.querySelectorAll('.actions-lecture button')].map((b) => b.textContent) };
  }, null, 120000).catch((e) => ({ erreur: e.message }));
  // (R in the page: webR's files, R's packages)
  const avecR = (p) => p.requetes.some((u) => /\/webr\/|repo\.r-wasm\.org/.test(u));
  const debutLien = Date.now();
  const lien = await ouvrirLien('doi=10.5281/zenodo.7000002');
  const lu = await pageLecteur(lien);
  const sansR = !avecR(lien);
  verifier('le lien ?doi= : le document principal du paquet montré d’abord, rempli (Q², figure, tableau), dans un cadre sans script, sans R (ni ses fichiers ni une demande), Ouvrir tout le projet et Relancer les modèles offerts',
    lu.document && lu.q2 && lu.figure && lu.tableau && lu.sandbox === 'allow-popups allow-popups-to-escape-sandbox' && !lu.demandes && sansR && !lu.violations && (lu.boutons || []).length === 2,
    { ...lu, sansR });
  if (options.capture !== false) await lien.screenshot({ path: path.join(sortie, 'lecture.png') });
  await lien.click('.relancer-lecture');
  const rapport = await attendre(lien, async (n) => {
    const c = [...document.querySelectorAll('.changements')].find((x) => x.querySelector('.paquet-relance .bilan-relance'));
    if (!c) return null;
    const { phraseBilan } = await import('./changements.js');
    const { app } = await import('./app.js');
    const p = app.projet(app.projetActif());
    return { bilan: c.querySelector('.bilan-relance').textContent, attendu: phraseBilan({ relances: n, identiques: n, differents: [] }),
      intact: !c.querySelector('.manifeste-relance').classList.contains('attention'), texte: c.innerText, projet: p && p.nom, chemin: p && p.chemin };
  }, paquet.n, 900000).catch((e) => ({ erreur: e.message }));
  res.lien_s = (Date.now() - debutLien) / 1000;
  const demandesZenodo = [...recues];
  verifier(`le lien ?doi= : le paquet de l’enregistrement (stand-in de Zenodo), ouvert et relancé, ses ${paquet.n} modèles redonnent les résultats déposés`,
    rapport.bilan === rapport.attendu && rapport.intact && rapport.projet === 'Orchards rerun.nuee' && !rapport.chemin && /record 7000002|enregistrement 7000002/.test(rapport.texte)
      && JSON.stringify(demandesZenodo) === JSON.stringify(['GET /api/records/7000002', `GET /api/records/7000002/files/${encodeURIComponent(paquet.nom)}/content`]),
    `${rapport.erreur || rapport.bilan} (${res.lien_s.toFixed(1)} s) ; ${demandesZenodo.join(' | ')}`);
  if (options.capture !== false) await lien.screenshot({ path: path.join(sortie, 'lien.png') });
  // Open the whole project: the project as its authors left it, its main document in front (with
  // its references live), nothing compared
  const entier = await ouvrirLien('doi=10.5281/zenodo.7000002');
  await pageLecteur(entier);
  await entier.click('.ouvrir-lecture');
  // (the document's view on screen, its preview resolved; the tab in front of its own group)
  const documentDevant = (p) => attendre(p, async () => {
    const { app } = await import('./app.js');
    const projet = app.projet(app.projetActif());
    const apercu = document.querySelector('.document .apercu-document');
    if (!projet || !apercu || !apercu.offsetWidth || !/Q² is 0[.,]\d/.test(apercu.textContent)) return null;
    const groupe = apercu.closest('.dv-groupview');
    const onglet = groupe && groupe.querySelector('.dv-tab.dv-active-tab');
    return { projet: projet.nom, onglet: onglet ? onglet.textContent.trim() : '', comparaison: !!document.querySelector('.changements'), lecture: !!document.querySelector('#lecture') };
  }, null, 600000).catch((e) => ({ erreur: e.message }));
  const projetEntier = await documentDevant(entier);
  verifier('Ouvrir tout le projet : le projet du paquet, son document principal devant, ses références vivantes, rien de comparé',
    projetEntier.projet === 'Orchards rerun.nuee' && /What the apples say/.test(projetEntier.onglet) && !projetEntier.comparaison && !projetEntier.lecture, projetEntier);
  await entier.close();
  // a phone's screen (390 × 844): the reader's page as wide as it, nothing beside; then the whole
  // project one view at a time (etroit.js), the explorer behind its button, one click opening a
  // view in its place
  const TELEPHONE = { width: 390, height: 844 };
  const tel = await ouvrirLien('doi=10.5281/zenodo.7000002', TELEPHONE);
  const luTel = await pageLecteur(tel);
  verifier('sur un téléphone, la page du lecteur à la largeur de l’écran, son document dans toute la largeur, rien qui défile de côté',
    luTel.document && luTel.largeur === luTel.fenetre && luTel.fenetre === TELEPHONE.width && !luTel.defile, luTel);
  if (options.capture !== false) await tel.screenshot({ path: path.join(sortie, 'lecture-telephone.png') });
  await tel.click('.ouvrir-lecture');
  // (the groups of the main dock on screen, the tab in front of each)
  const groupes = (p) => p.evaluate(() => ({
    etroit: document.body.classList.contains('etroit'), defile: document.documentElement.scrollWidth > innerWidth + 1,
    bouton: !!document.querySelector('#bouton-explorateur') && document.querySelector('#bouton-explorateur').offsetWidth > 0,
    outils: document.querySelector('#barre-outils').offsetHeight,
    vus: [...document.querySelectorAll('#dock .dv-groupview')].filter((g) => !g.closest('.page-modele') && g.offsetWidth > 0 && g.offsetHeight > 0)
      .map((g) => ({ largeur: Math.round(g.offsetWidth), onglet: ((g.querySelector('.dv-tab.dv-active-tab') || {}).textContent || '').trim() })) }));
  const unSeul = (x, motif) => x.etroit && !x.defile && x.bouton && !x.outils && x.vus.length === 1 && x.vus[0].largeur >= TELEPHONE.width - 2 && motif.test(x.vus[0].onglet);
  const docTel = await documentDevant(tel);
  const devantDoc = docTel.erreur ? docTel : await groupes(tel);
  // (the recovery copies of the tabs closed above, which a tab starting offers: ignored here)
  await tel.waitForTimeout(1000);
  const ignorer = await libelle(tel, 'ignorer');
  await tel.evaluate((x) => { const b = [...document.querySelectorAll('.fond-dialogue button')].find((y) => y.textContent.trim() === x); if (b) b.click(); }, ignorer);
  await tel.click('#bouton-explorateur');
  await tel.waitForTimeout(500);
  const devantExplorateur = await groupes(tel);
  // one click on M1's Loadings, under the fitted models: its view in the explorer's place
  const cleChargements = await tel.evaluate(() => { const l = [...document.querySelectorAll('.arbre [role="treeitem"]')].find((x) => /\/m\/[^/]+\/loadings$/.test(x.dataset.cle || '')); return l ? l.dataset.cle : null; });
  if (cleChargements) await tel.click(`.arbre [role="treeitem"][data-cle="${cleChargements}"] .libelle`);
  const devantVue = await attendre(tel, () => { const x = document.querySelector('.page-modele .dv-tab.dv-active-tab'); return x && /Loadings/.test(x.textContent) && document.querySelector('.page-modele .js-plotly-plot'); }, null, 300000)
    .then(() => groupes(tel), (e) => ({ erreur: e.message }));
  verifier('sur un téléphone, le projet une vue à la fois : son document dans tout l’écran, l’explorateur derrière son bouton, la barre d’outils cachée ; un clic sur une vue de l’explorateur l’ouvre à sa place',
    unSeul(devantDoc, /What the apples say/) && unSeul(devantExplorateur, /explorer/i) && !!cleChargements && unSeul(devantVue, /M1/), { devantDoc, devantExplorateur, cleChargements, devantVue });
  if (options.capture !== false) await tel.screenshot({ path: path.join(sortie, 'telephone.png') });
  await tel.close();
  const resolutions = await lien.evaluate(async () => {
    const m = await import('./relancer.js');
    const id = async (x) => { try { return (await m.recordZenodo(x)).id; } catch (e) { return `${e.genre} : ${e.message}`; } };
    return { concept: await id({ doi: 'https://doi.org/10.5281/zenodo.7000000' }), record: await id({ record: '7000002' }), inconnu: await id({ doi: '10.5281/zenodo.9999999' }),
      autre: await id({ doi: '10.5061/dryad.x1' }), pasDoi: await id({ doi: 'zenodo.12' }) };
  });
  horsLigne = true;
  resolutions.reseau = await lien.evaluate(() => import('./relancer.js').then((m) => m.recordZenodo({ doi: '10.5281/zenodo.7000002' }).then((r) => r.id, (e) => `${e.genre} : ${e.message}`)));
  horsLigne = false;
  verifier('un DOI de concept donne sa dernière version, un enregistrement son id ; un DOI que Zenodo ne connaît pas, un autre qui n’est pas un DOI, pas de réseau : dits',
    resolutions.concept === 7000002 && resolutions.record === 7000002 && /^inconnu : .*10\.5281\/zenodo\.9999999/.test(resolutions.inconnu)
      && /^inconnu : .*10\.5061\/dryad\.x1/.test(resolutions.autre) && /^doi : /.test(resolutions.pasDoi) && /^reseau : .*zenodo\.org/.test(resolutions.reseau), resolutions);
  // the same package chosen in the page: File › Open on its .zip, then File › Open a reproducibility
  // package…, each opened again as a new project and rerun (the 2nd and 3rd comparisons of the tab)
  const zipLien = path.join(sortie, paquet.nom);
  fs.writeFileSync(zipLien, octetsPaquet);
  const rouvrir = async (ouvrir, k) => {
    const choix = lien.waitForEvent('filechooser', { timeout: 15000 });
    await ouvrir();
    await (await choix).setFiles(zipLien);
    return attendre(lien, async ([n, kk]) => {
      const c = document.querySelector(`[data-comparaison^="C${kk}-"] .changements`);
      const b = c && c.querySelector('.paquet-relance .bilan-relance');
      if (!b) return null;
      const { phraseBilan } = await import('./changements.js');
      const { app } = await import('./app.js');
      return { bilan: b.textContent, attendu: phraseBilan({ relances: n, identiques: n, differents: [] }), projets: app.projets().length };
    }, [paquet.n, k], 600000).catch((e) => ({ erreur: e.message }));
  };
  const parOuvrir = await rouvrir(() => lien.evaluate(() => import('./app.js').then((m) => { m.app.ouvrir(); })), 2);
  const parCommande = await rouvrir(() => commande(lien, 'paquet_ouvrir'), 3);
  verifier('le même paquet choisi dans la page, par Fichier › Ouvrir puis par Fichier › Ouvrir un paquet de reproductibilité… : chaque fois un nouveau projet, relancé',
    parOuvrir.bilan === parOuvrir.attendu && parCommande.bilan === parCommande.attendu && parOuvrir.projets === 2 && parCommande.projets === 3,
    `${parOuvrir.erreur || parOuvrir.bilan} | ${parCommande.erreur || parCommande.bilan}`);
  const dialogueLien = async (requete) => {
    const p = await ouvrirLien(requete);
    // (the link's dialog, not one a start may offer: the recovery copies of a tab closed before)
    const texte = await attendre(p, () => { const d = [...document.querySelectorAll('.fond-dialogue .corps-dialogue')].find((x) => /Zenodo/.test(x.textContent)); return d ? d.textContent : null; },
      null, 300000).catch(() => '');
    const projets = await p.evaluate(() => import('./app.js').then((m) => m.app.projets().length));
    await p.close();
    return { texte, projets };
  };
  const inconnu = await dialogueLien('doi=10.5281/zenodo.9999999');
  const attenduInconnu = await libelle(page, 'relance_zenodo_inconnu');
  const sansPaquet = await dialogueLien('record=7000005');
  const gros = await dialogueLien('record=7000007');
  const attenduGros = (await libelle(page, 'relance_zenodo_trop_gros')).replace('{nom}', 'Huge package.zip').replace('{taille}', '612.3').replace('{max}', '500');
  verifier('le lien d’un DOI que Zenodo ne connaît pas, celui d’un enregistrement sans paquet : dits dans la page, rien d’ouvert',
    inconnu.texte === attenduInconnu.replace('{doi}', '10.5281/zenodo.9999999') && !inconnu.projets && /7000005/.test(sansPaquet.texte) && /\.zip/.test(sansPaquet.texte) && !sansPaquet.projets,
    `${inconnu.texte} | ${sansPaquet.texte}`);
  verifier('le paquet d’un enregistrement plus gros que 500 Mo (612 Mo, la taille que Zenodo donne) : refusé avant d’être téléchargé, sa taille dite, l’application de bureau proposée',
    [attenduGros, attenduGros.replace('612.3', '612,3')].includes(gros.texte) && !gros.projets && !recues.some((x) => x.includes('/api/records/7000007/files/')),
    gros.texte);
  verifier('le lien ne demande rien qu’à zenodo.org (et R ses paquets, à repo.r-wasm.org), sans violation de la politique de sécurité',
    !autresSites.length && !(await lien.evaluate(() => window.__violations)).length, autresSites.slice(0, 5));
  await lien.close();

  // ---- 1.2, History in the page, in a tab of its own (versions_navigateur.js): the example saved
  // (downloaded and kept at its place in the browser's storage, its first version), Save a version…,
  // Name…, Compare, Open, Restore…, from History's own buttons; a table saved, its history apart;
  // Share still the desktop application's; reloaded, the projects recovered at their places with their
  // histories; the first download opened again, its history found by its content; Save as…, a place
  // of its own with the history; in a browser without OPFS's createWritable, the versions in IndexedDB
  const hp = await ouvrirPage();
  await hp.waitForFunction(() => window.nueeDemandes.length > 0 && !document.querySelector('.voile-webr'), null, { timeout: 600000 });
  // (the copies of the link's tabs, closed: set aside)
  const ignorerCopies = async (p) => {
    const offertes = await attendre(p, (tt) => [...document.querySelectorAll('.fond-dialogue h2')].some((h) => h.textContent === tt), titreRecuperation, 5000).catch(() => false);
    if (offertes) await p.click('.fond-dialogue .pied-dialogue button:not(.principal)');
  };
  await ignorerCopies(hp);
  // a project's place and History's versions, as History asks them (the project in front, by its
  // id, or at a place: the project read again after Restore has another id)
  const historique = (p, { id = null, chemin = null } = {}) => p.evaluate(async ([pid, lieu]) => {
    const { app } = await import('./app.js');
    const { coucheVersions } = await import('./versions.js');
    const q = lieu ? app.projets().find((x) => !x.lecture_seule && x.chemin_enregistrement === lieu) : app.projet(pid || app.projetActif());
    if (!q) return null;
    const c = q.chemin_enregistrement;
    const l = c && /\.nuee$/i.test(c) ? await coucheVersions().liste(c) : [];
    return { id: q.id, nom: q.nom, chemin: c, modifie: q.modifie, modeles: (q.modeles || []).length, versions: l.map((v) => ({ oid: v.oid, description: v.description, auto: v.auto, noms: v.noms })) };
  }, [id, chemin]);
  // History's panel in front, drawn: its header, its text
  const panneau = (p) => attendre(p, () => {
    const v = [...document.querySelectorAll('.versions')].find((x) => x.offsetParent !== null);
    return v && !v.querySelector('.attente') ? { tete: (v.querySelector('.tete-versions') || {}).textContent || '', texte: v.textContent } : null;
  }, null, 60000);
  // (History waiting for ever: what it shows, and the relay's requests still without an answer)
  // (asked again every 3 s while it waits: a model's page just fitted comes to the front when its
  // views are drawn, later than History in a slower browser, as it does for a person)
  const montrerHistorique = async (p, pid) => {
    const montrer = () => p.evaluate((id) => import('./app.js').then(({ app }) => app.versions(id)), pid);
    await montrer();
    const encore = setInterval(() => { montrer().catch(() => {}); }, 3000);
    return panneau(p).finally(() => clearInterval(encore)).catch(async (e) => {
      const montre = await p.evaluate(() => [...document.querySelectorAll('.versions')].map((v) => `${v.offsetParent !== null ? 'visible' : 'caché'} : ${v.textContent.slice(0, 120)}`)).catch(() => []);
      throw new Error(`${e.message.split(/\r?\n/)[0]} ; Historique : ${montre.join(' | ') || 'aucun'} ; relais sans réponse : ${relaisEnCours.filter((x) => !x.fini).map((x) => x.quoi).join(', ') || 'aucun'}`);
    });
  };
  // a button of History (its words), on a version (by its rank, -1 the oldest) or at the top; waited
  // for 15 s (the panel is drawn again once the project's history is known: a button greyed at first)
  const boutonHistorique = (p, cle, rang = null) => p.evaluate(async ([c, k]) => {
    const { t } = await import('./i18n.js');
    for (let i = 0; ; i += 1) {
      const v = [...document.querySelectorAll('.versions')].find((x) => x.offsetParent !== null);
      const lignes = v ? [...v.querySelectorAll('.liste-versions .version')] : [];
      const ou = !v ? null : k === null ? v : k < 0 ? lignes[lignes.length + k] : lignes[k];
      const b = ou && [...ou.querySelectorAll('button')].find((x) => x.textContent.trim() === t(c) && !x.disabled);
      if (b) { b.click(); return; }
      if (i >= 75) throw new Error(`no ${c} in History`);
      await new Promise((ok) => setTimeout(ok, 200));
    }
  }, [cle, rang]);
  const lignesHistorique = (p, n) => attendre(p, (k) => [...document.querySelectorAll('.versions')].some((v) => v.offsetParent !== null && v.querySelectorAll('.liste-versions .version').length === k), n, 60000);
  const enregistrerPar = async (p, f) => {
    const t = p.waitForEvent('download', { timeout: 60000 });
    await f();
    return t;
  };

  // the example: History says it was never saved here; Save, from History, downloads it and keeps it
  // at its place, with its first version
  await commande(hp, 'projet_exemple');
  await hp.waitForFunction(() => window.nueeDemandes.some((d) => d.nom === 'exemple' && d.ok), null, { timeout: 300000 });
  await hp.waitForTimeout(2000);
  await commande(hp, 'versions');
  const jamaisIci = await panneau(hp);
  const dl1 = await enregistrerPar(hp, () => boutonHistorique(hp, 'versions_enregistrer_projet'));
  const fichier1 = path.join(sortie, `historique-1-${dl1.suggestedFilename()}`);
  await dl1.saveAs(fichier1);
  await lignesHistorique(hp, 1);
  const h1 = await historique(hp);
  const tete1 = (await panneau(hp)).tete;
  const ici = (await libelle(hp, 'versions_tete_navigateur')).split('(')[0].trim();
  verifier('Historique dans la page : jamais enregistré ici, il le dit ; Enregistrer le télécharge et le garde à sa place dans le navigateur, sa première version, la place qu’elle prend',
    jamaisIci.texte.includes(jamaisSauve) && /\.nuee$/.test(dl1.suggestedFilename()) && /^\/projets\/[^/]+\/[^/]+\.nuee$/.test(h1.chemin) && !h1.modifie
      && h1.versions.length === 1 && h1.versions[0].auto && tete1.includes(ici) && /\d+(\.\d+)? kB/.test(tete1),
    `${dl1.suggestedFilename()} ; ${h1.chemin} ; ${h1.versions.map((v) => v.description).join(', ')} ; ${tete1.trim()}`);

  // a model added, then Save a version…: the project saved first (downloaded), the version with its words
  await nouveauModele(hp, { type: 'LM', x: ['sugar'], y: ['taste_score'], echelle: 'none' });
  await hp.waitForTimeout(1500);
  await montrerHistorique(hp, h1.id);
  const dl2 = enregistrerPar(hp, async () => {
    await boutonHistorique(hp, 'versions_maintenant');
    await hp.waitForSelector('.dialogue-version input', { timeout: 10000 });
    await hp.fill('.dialogue-version input', 'Sugar model added');
    await hp.click('.fond-dialogue .pied-dialogue button.principal');
  });
  await dl2;
  await lignesHistorique(hp, 2);
  const h2 = await historique(hp, { id: h1.id });
  verifier('Enregistrer une version… : le projet enregistré d’abord (téléchargé), la version avec ses mots',
    h2.versions.length === 2 && h2.versions[0].description === 'Sugar model added' && !h2.versions[0].auto && h2.modeles === h1.modeles + 1, h2.versions.map((v) => v.description).join(' | '));

  // Name… the first version
  await boutonHistorique(hp, 'versions_nommer', -1);
  await hp.waitForSelector('.dialogue-version input', { timeout: 10000 });
  await hp.fill('.dialogue-version input', 'submitted');
  await hp.click('.fond-dialogue .pied-dialogue button.principal');
  await attendre(hp, () => [...document.querySelectorAll('.versions .nom-version')].some((b) => b.textContent.includes('submitted')), null, 30000);
  const h3 = await historique(hp, { id: h1.id });
  verifier('Nommer… : le nom sur la première version', JSON.stringify(h3.versions.map((v) => v.noms)) === JSON.stringify([[], ['submitted']]), JSON.stringify(h3.versions.map((v) => v.noms)));

  // Compare the first version with the project now: What changed, the model added after
  await boutonHistorique(hp, 'versions_comparer', -1);
  const apresSeul = await libelle(hp, 'changements_apres_seul');
  const comparaison = await attendre(hp, (x) => {
    const c = [...document.querySelectorAll('.changements')].find((e) => e.offsetParent !== null);
    return c && c.textContent.includes(x) ? c.textContent.replace(/\s+/g, ' ') : null;
  }, apresSeul, 300000).catch(() => '');
  verifier('Comparer : la première version et le projet d’aujourd’hui, Ce qui a changé (le modèle ajouté)', /M5/.test(comparaison.slice(comparaison.indexOf(apresSeul))), comparaison.slice(0, 240));

  // Open the first version: beside the project, read-only, its models
  await montrerHistorique(hp, h1.id);
  await boutonHistorique(hp, 'versions_ouvrir', -1);
  const lecture = await attendre(hp, (oid) => import('./app.js').then(({ app }) => {
    const p = app.projets().find((x) => x.lecture_seule && x.lecture_seule.oid === oid);
    return p ? { nom: p.nom, modeles: (p.modeles || []).length } : null;
  }), h3.versions[1].oid, 120000).catch(() => null);
  verifier('Ouvrir : la première version à côté du projet, en lecture seule', !!lecture && lecture.modeles === h1.modeles, JSON.stringify(lecture));

  // Restore… the first version: asked, then the project becomes it again, a version saying so
  await montrerHistorique(hp, h1.id);
  await boutonHistorique(hp, 'versions_restaurer', -1);
  await hp.waitForSelector('.fond-dialogue .pied-dialogue button.principal', { timeout: 10000 });
  await hp.click('.fond-dialogue .pied-dialogue button.principal');
  const restauree = (await libelle(hp, 'versions_restauree')).split('{date}')[0];
  await attendre(hp, (r) => document.getElementById('statut').textContent.includes(r), restauree, 300000).catch(() => {});
  // (the project read again from the version: back at its place with its three versions)
  let h4 = null;
  for (let i = 0; i < 60 && !(h4 && h4.versions.length === 3 && h4.modeles === h1.modeles); i += 1) {
    await hp.waitForTimeout(1000);
    h4 = await historique(hp, { chemin: h1.chemin }).catch(() => null);
  }
  verifier('Restaurer… : le projet redevient la première version (ses modèles), une version le dit',
    !!h4 && h4.versions.length === 3 && /^Restored the version of/.test(h4.versions[0].description) && h4.modeles === h1.modeles, h4 && `${h4.modeles} modèles ; ${h4.versions.map((v) => v.description).join(' | ')}`);

  // a table opened, saved: its own place and history (a project of the same name), the first's unchanged
  const nOuvrir = await combien(hp, 'ouvrir');
  const choixCsv = hp.waitForEvent('filechooser', { timeout: 15000 });
  await commande(hp, 'ouvrir');
  await (await choixCsv).setFiles(csv);
  await attendreDemande(hp, 'ouvrir', nOuvrir);
  await hp.waitForTimeout(1500);
  // (the table's Data view in front, as a person clicks its tab: the restored project, read again
  // meanwhile, may come to the front after the table in a slower run, and Save saves the one in front)
  await attendre(hp, () => import('./app.js').then(({ app }) => {
    const table = app.projets().filter((x) => /\.csv$/i.test(x.nom) && !x.chemin_enregistrement).pop();
    if (!table) return false;
    if (app.projetActif() !== table.id) app.ouvrirVue('donnees', null, null, table.id);
    return app.projetActif() === table.id;
  }), null, 30000).catch(() => {});
  await enregistrerPar(hp, () => commande(hp, 'enregistrer'));
  const deuxieme = await attendre(hp, () => import('./app.js').then(async ({ app }) => {
    const { coucheVersions } = await import('./versions.js');
    const p = app.projet(app.projetActif());
    const l = p && p.chemin_enregistrement ? await coucheVersions().liste(p.chemin_enregistrement) : [];
    return l.length === 1 ? { chemin: p.chemin_enregistrement, versions: l.length } : null;
  }), null, 60000).catch(() => null);
  const h5 = await historique(hp, { chemin: h1.chemin });
  verifier('un second projet : sa place et son historique à lui, ceux du premier inchangés',
    !!deuxieme && deuxieme.chemin.split('/')[2] !== h1.chemin.split('/')[2] && h5.versions.length === 3, `${deuxieme && deuxieme.chemin} ; ${h1.chemin} : ${h5.versions.length} versions`);

  // reloaded: the projects recovered at their places, with their histories and names
  await copiesAJour(hp);
  await hp.waitForTimeout(1000);
  await hp.reload();
  await hp.waitForFunction((tt) => [...document.querySelectorAll('.fond-dialogue h2')].some((h) => h.textContent === tt), titreRecuperation, { timeout: 120000 });
  await hp.click('.fond-dialogue .pied-dialogue button.principal');
  const reprisAvecHistorique = await attendre(hp, () => import('./app.js').then(async ({ app }) => {
    const { coucheVersions } = await import('./versions.js');
    const ps = app.projets().filter((p) => p.chemin_enregistrement);
    if (ps.length < 2) return null;
    const r = [];
    for (const p of ps) r.push({ chemin: p.chemin_enregistrement, versions: (await coucheVersions().liste(p.chemin_enregistrement)).map((v) => v.noms.join(',') || v.description) });
    return r;
  }), null, 300000).catch(() => []);
  const premierRepris = reprisAvecHistorique.find((x) => x.chemin === h1.chemin);
  verifier('rechargé : les projets repris à leur place, leurs historiques là (le nom aussi)',
    !!premierRepris && premierRepris.versions.length === 3 && premierRepris.versions.includes('submitted') && !!deuxieme && reprisAvecHistorique.some((x) => x.chemin === deuxieme.chemin && x.versions.length === 1),
    JSON.stringify(reprisAvecHistorique));

  // the first download opened again, under another name: its history found by its content
  const nOuvrir2 = await combien(hp, 'ouvrir');
  const choixNuee = hp.waitForEvent('filechooser', { timeout: 15000 });
  await commande(hp, 'ouvrir');
  await (await choixNuee).setFiles(fichier1);
  await attendreDemande(hp, 'ouvrir', nOuvrir2);
  await hp.waitForTimeout(1500);
  const rouvertIci = await historique(hp);
  verifier('son premier téléchargement rouvert, sous un autre nom : son historique retrouvé par son contenu',
    /^\/depots\//.test(rouvertIci.chemin) && rouvertIci.versions.length === 3 && rouvertIci.versions.some((v) => v.noms.includes('submitted')), `${rouvertIci.chemin} : ${rouvertIci.versions.length} versions`);

  // Save as…: a project of its own at a new place, its history with it, the former place's kept
  await enregistrerPar(hp, () => commande(hp, 'enregistrer_sous'));
  const copie = await attendre(hp, (id) => import('./app.js').then(async ({ app }) => {
    const { coucheVersions } = await import('./versions.js');
    const p = app.projet(id);
    if (!p || !/^\/projets\//.test(p.chemin_enregistrement)) return null;
    const l = await coucheVersions().liste(p.chemin_enregistrement);
    return l.length >= 3 ? { chemin: p.chemin_enregistrement, versions: l.length } : null;
  }), rouvertIci.id, 60000).catch(() => null);
  const h6 = await historique(hp, { chemin: h1.chemin });
  verifier('Enregistrer sous… : une nouvelle place, l’historique avec lui, celui d’avant resté',
    !!copie && copie.chemin !== h1.chemin && copie.versions === 3 && h6.versions.length === 3, `${JSON.stringify(copie)} ; ${h1.chemin} : ${h6.versions.length}`);
  if (options.capture !== false) await hp.screenshot({ path: path.join(sortie, 'historique.png') });
  verifier('l’Historique sans violation de la politique de sécurité', (await hp.evaluate(() => window.__violations)).length === 0, await hp.evaluate(() => window.__violations));
  await copiesAJour(hp);
  await hp.close({ runBeforeUnload: true });

  // a browser whose OPFS can't write (no createWritable): the versions in IndexedDB, there after a reload
  const hi = await contexte.newPage();
  hi.on('pageerror', (e) => erreurs.push(`page : ${e.message}`));
  hi.on('dialog', (d) => d.accept());
  await hi.addInitScript(() => { window.nueeDemandes = []; if (window.FileSystemFileHandle) delete FileSystemFileHandle.prototype.createWritable; });
  await hi.goto(adresse);
  await hi.waitForFunction(() => document.querySelectorAll('.menu-haut').length > 3, null, { timeout: 60000 });
  await ignorerCopies(hi);
  await commande(hi, 'projet_exemple');
  await hi.waitForFunction(() => window.nueeDemandes.some((d) => d.nom === 'exemple' && d.ok), null, { timeout: 300000 });
  await hi.waitForTimeout(2000);
  await enregistrerPar(hi, () => commande(hi, 'enregistrer'));
  const dansIdb = await attendre(hi, () => import('./app.js').then(async ({ app }) => {
    const { coucheVersions } = await import('./versions.js');
    const { systemeFichiers } = await import('./fichiers_navigateur.js');
    const p = app.projet(app.projetActif());
    const l = p && p.chemin_enregistrement ? await coucheVersions().liste(p.chemin_enregistrement) : [];
    return l.length === 1 ? { chemin: p.chemin_enregistrement, stockage: (await systemeFichiers()).type } : null;
  }), null, 60000).catch(() => null);
  const entreesVersionsIdb = await hi.evaluate(() => new Promise((ok) => {
    const r = indexedDB.open('nuee-versions');
    r.onerror = () => ok(-1);
    r.onsuccess = () => {
      try { const q = r.result.transaction('entrees', 'readonly').objectStore('entrees').count(); q.onsuccess = () => ok(q.result); q.onerror = () => ok(-1); } catch { ok(-1); }
    };
  }));
  await copiesAJour(hi);
  await hi.waitForTimeout(1000);
  await hi.reload();
  await hi.waitForFunction((tt) => [...document.querySelectorAll('.fond-dialogue h2')].some((h) => h.textContent === tt), titreRecuperation, { timeout: 120000 });
  await hi.click('.fond-dialogue .pied-dialogue button.principal');
  const reprisVersionsIdb = dansIdb ? await attendre(hi, (c) => import('./app.js').then(async ({ app }) => {
    const { coucheVersions } = await import('./versions.js');
    const p = app.projets().find((x) => x.chemin_enregistrement === c);
    return p ? (await coucheVersions().liste(c)).length : null;
  }), dansIdb.chemin, 300000).catch(() => null) : null;
  verifier('sans OPFS : les versions dans IndexedDB, là après un rechargement',
    !!dansIdb && dansIdb.stockage === 'IndexedDB' && entreesVersionsIdb > 5 && reprisVersionsIdb === 1, `${JSON.stringify(dansIdb)} ; ${entreesVersionsIdb} entrées ; repris : ${reprisVersionsIdb}`);
  await copiesAJour(hi);
  await hi.close({ runBeforeUnload: true });

  // ---- 1.2, sharing in the page through nuee.io's relay, in a tab of its own: the relay's own
  // handler (vitrine/src/relais.js) answers the page's /relais/ (Playwright's routes), against
  // stand-ins on this computer (RELAIS_ESSAI): GitHub's device flow, and git (the system git's
  // http-backend, every repository private); GitHub's API and the account's picture answer through
  // routes too. Signed in from Share (the device flow, its code), a repository made and the project
  // sent; a colleague's change received with Get now; a change sent with Send now; the project opened
  // from the repository into a new place, its versions with it; signed out, the token gone. Nothing
  // reaches github.com or gitlab.com (their names never resolve here), no token in an address.
  let gitSysteme = true;
  try { execFileSync('git', ['--version'], { stdio: 'ignore' }); } catch { gitSysteme = false; }
  if (!gitSysteme) console.log('  (le partage dans la page : sauté, sans git sur cet ordinateur, dont le http-backend fait GitHub)');
  else {
    const racinePartage = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-partage-'));
    const configVide = path.join(racinePartage, 'gitconfig');
    fs.writeFileSync(configVide, '');
    const envGit = { ...process.env, GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: configVide, GIT_TERMINAL_PROMPT: '0' };
    const gitSys = (args, cwd = racinePartage) => execFileSync('git', args, { cwd, env: envGit, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
    const DEPOTS = path.join(racinePartage, 'depots');
    const JETON = `gho_essai${Math.random().toString(36).slice(2, 14)}`;
    let sondes = 0;
    // the stand-in for github.com, as the relay sends it /github.com/<path>: the device flow (pending
    // once, then the token), git's smart HTTP for the repositories made through the API (a token needed)
    const banc = http.createServer((req, res) => {
      const u = new URL(req.url, 'http://banc');
      const [, hote = '', chemin = ''] = /^\/([^/]+)\/(.*)$/.exec(u.pathname) || [];
      if (req.method === 'POST' && hote === 'github.com' && ['login/device/code', 'login/oauth/access_token'].includes(chemin)) {
        req.resume();
        req.on('end', () => {
          res.writeHead(200, { 'Content-Type': 'application/json' });
          if (chemin === 'login/device/code') {
            res.end(JSON.stringify({ device_code: 'appareil-essai', user_code: 'WDJB-MJHT', verification_uri: 'https://github.com/login/device', expires_in: 899, interval: 1 }));
            return;
          }
          sondes += 1;
          res.end(JSON.stringify(sondes === 1 ? { error: 'authorization_pending' } : { access_token: JETON, token_type: 'bearer', scope: 'repo' }));
        });
        return;
      }
      const a = /^Basic (.+)$/i.exec(req.headers.authorization || '');
      const motDePasse = a ? Buffer.from(a[1], 'base64').toString('utf8').replace(/^[^:]*:/, '') : null;
      const depot = /^(.+\.git)\/(?:info\/refs|git-upload-pack|git-receive-pack)$/.exec(chemin);
      if (hote !== 'github.com' || !depot) { req.resume(); res.writeHead(404); res.end('Not Found\n'); return; }
      if (motDePasse !== JETON) { req.resume(); res.writeHead(401, { 'WWW-Authenticate': 'Basic realm="github.com"' }); res.end('Authentication required\n'); return; }
      if (!fs.existsSync(path.join(DEPOTS, hote, depot[1]))) { req.resume(); res.writeHead(404); res.end('Repository not found.\n'); return; }
      const env = { ...envGit, GIT_PROJECT_ROOT: path.join(DEPOTS, hote), GIT_HTTP_EXPORT_ALL: '1', PATH_INFO: `/${chemin}`, REQUEST_METHOD: req.method,
        QUERY_STRING: u.search.replace(/^\?/, ''), CONTENT_TYPE: req.headers['content-type'] || '', REMOTE_USER: 'essai', REMOTE_ADDR: '127.0.0.1' };
      if (req.headers['content-length']) env.CONTENT_LENGTH = req.headers['content-length'];
      if (req.headers['git-protocol']) env.GIT_PROTOCOL = req.headers['git-protocol'];
      const cgi = spawn('git', ['http-backend'], { env, windowsHide: true });
      req.pipe(cgi.stdin);
      let tete = Buffer.alloc(0);
      let commence = false;
      cgi.stdout.on('data', (morceau) => {
        if (commence) { res.write(morceau); return; }
        tete = Buffer.concat([tete, morceau]);
        const fin = tete.indexOf('\r\n\r\n');
        if (fin < 0) return;
        let statut = 200;
        const entetes = {};
        for (const ligne of tete.subarray(0, fin).toString('latin1').split('\r\n')) {
          const i = ligne.indexOf(':');
          if (/^status$/i.test(ligne.slice(0, i).trim())) statut = parseInt(ligne.slice(i + 1), 10);
          else entetes[ligne.slice(0, i).trim()] = ligne.slice(i + 1).trim();
        }
        res.writeHead(statut, entetes);
        commence = true;
        res.write(tete.subarray(fin + 4));
      });
      cgi.on('close', () => { if (!commence) res.writeHead(500); res.end(); });
    });
    await new Promise((ok) => banc.listen(0, '127.0.0.1', ok));
    try {
      // the relay at the page's own origin, as on nuee.io, its handler here
      const { relayer } = await import(pathToFileURL(path.join(APP, '..', 'vitrine', 'src', 'relais.js')).href);
      const RELAIS_ESSAI = `http://127.0.0.1:${banc.address().port}`;
      const origine = new URL(adresse).origin;
      await contexte.route(`${origine}/relais/**`, async (route) => {
        const q = route.request();
        const relais = { quoi: `${q.method()} ${new URL(q.url()).pathname.replace(/^\/relais\//, '')}`, fini: false };
        relaisEnCours.push(relais);
        const url = new URL(q.url());
        // (a body over 100 MB: Cloudflare's 413, before the Worker runs; trop-gros.git stands for it)
        if (q.method() === 'POST' && /\/trop-gros\.git\//.test(url.pathname)) return route.fulfill({ status: 413, headers: { 'Content-Type': 'text/html' }, body: '<html>413</html>' });
        const h = new Headers();
        for (const [k, v] of Object.entries(await q.allHeaders())) if (!['host', 'connection', 'content-length'].includes(k)) h.set(k, v);
        const r = await relayer(new Request(url, { method: q.method(), headers: h, body: q.method() === 'POST' ? q.postDataBuffer() : undefined, duplex: 'half' }), { RELAIS_ESSAI }, url);
        const e = {};
        r.headers.forEach((v, k) => { e[k] = v; });
        await route.fulfill({ status: r.status, headers: e, body: Buffer.from(await r.arrayBuffer()) });
        relais.fini = true;
      });
      // GitHub's API (the account, a repository made: a bare one of the stand-in), and the picture
      const cors = { 'Access-Control-Allow-Origin': '*', 'Access-Control-Allow-Headers': 'Authorization, Content-Type, Accept, X-GitHub-Api-Version', 'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE' };
      await contexte.route('https://api.github.com/**', (route) => {
        const q = route.request();
        const u = new URL(q.url());
        if (q.method() === 'OPTIONS') return route.fulfill({ status: 204, headers: cors });
        const json = (statut, o) => route.fulfill({ status: statut, headers: { ...cors, 'Content-Type': 'application/json' }, body: JSON.stringify(o) });
        if ((q.headers().authorization || '') !== `Bearer ${JETON}`) return json(401, { message: 'Bad credentials' });
        if (q.method() === 'GET' && u.pathname === '/user') return json(200, { login: 'alice-exemple', name: 'Alice Exemple', avatar_url: 'https://avatars.githubusercontent.com/u/1?v=4' });
        if (q.method() === 'POST' && u.pathname === '/user/repos') {
          const { name } = JSON.parse(q.postData() || '{}');
          const d = path.join(DEPOTS, 'github.com', 'alice-exemple', `${name}.git`);
          fs.mkdirSync(d, { recursive: true });
          gitSys(['init', '-q', '--bare', d]);
          gitSys(['symbolic-ref', 'HEAD', 'refs/heads/main'], d);
          gitSys(['config', 'http.receivepack', 'true'], d);
          return json(201, { name, clone_url: `https://github.com/alice-exemple/${name}.git`, html_url: `https://github.com/alice-exemple/${name}` });
        }
        return json(404, { message: 'Not Found' });
      });
      await contexte.route('https://avatars.githubusercontent.com/**', (route) => route.fulfill({ status: 200,
        headers: { 'Access-Control-Allow-Origin': '*', 'Cross-Origin-Resource-Policy': 'cross-origin', 'Content-Type': 'image/png' },
        body: Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=', 'base64') }));
      const nu = path.join(DEPOTS, 'github.com', 'alice-exemple', 'orchards.git');
      const versionsDuDepot = () => { try { return gitSys(['--git-dir', nu, 'log', '--format=%an|%s', 'main']).split('\n').filter(Boolean); } catch { return []; } };
      const attendreDepot = async (n) => {
        for (let i = 0; i < 120 && versionsDuDepot().length < n; i += 1) await new Promise((ok) => setTimeout(ok, 500));
        return versionsDuDepot();
      };
      const debutPartage = requetes.length;
      // (its errors noted, but the relay's planned 401s, isomorphic-git asking first without the token
      // and giving it when the server asks, and the 413 below, which Chrome writes to the console)
      const sp = await contexte.newPage();
      sp.on('pageerror', (e) => erreurs.push(`page : ${e.message}`));
      sp.on('console', (m) => {
        if (m.type() === 'error' && !(/status of (401|413)/.test(m.text()) && (m.location().url || '').startsWith(`${origine}/relais/`))) erreurs.push(`console : ${m.text()}`);
      });
      sp.on('dialog', (d) => d.accept());
      await sp.addInitScript(() => {
        window.nueeDemandes = [];
        window.__violations = [];
        document.addEventListener('securitypolicyviolation', (e) => window.__violations.push(`${e.violatedDirective} ${e.blockedURI}`));
      });
      await sp.goto(adresse);
      await sp.waitForFunction(() => document.querySelectorAll('.menu-haut').length > 3, null, { timeout: 60000 });
      await sp.waitForFunction(() => window.nueeDemandes.length > 0 && !document.querySelector('.voile-webr'), null, { timeout: 600000 });
      await ignorerCopies(sp);
      await commande(sp, 'projet_exemple');
      await sp.waitForFunction(() => window.nueeDemandes.some((d) => d.nom === 'exemple' && d.ok), null, { timeout: 300000 });
      await sp.waitForTimeout(2000);
      await enregistrerPar(sp, () => commande(sp, 'enregistrer'));
      const premier = await attendre(sp, () => import('./app.js').then(({ app }) => {
        const p = app.projet(app.projetActif());
        return p && /^\/projets\//.test(p.chemin_enregistrement || '') ? { id: p.id, chemin: p.chemin_enregistrement } : null;
      }), null, 60000);
      const projetA = (p, c) => p.evaluate((x) => import('./app.js').then(({ app }) => {
        const q = app.projets().find((y) => !y.lecture_seule && y.chemin_enregistrement === x);
        return q ? { id: q.id, documents: [].concat(q.documents || []).map((d) => d.titre), modeles: (q.modeles || []).length, modifie: q.modifie } : null;
      }), c);

      // Share, from History: the author asked; GitHub; signed in with the device flow (the code, the
      // relay's note in the dialog); a private repository made, the project sent through the relay
      await montrerHistorique(sp, premier.id);
      await boutonHistorique(sp, 'versions_partager');
      await sp.waitForSelector('.dialogue-partage input[type="email"]', { timeout: 15000 });
      const autreGitlab = await sp.evaluate(() => (document.querySelector('.dialogue-partage .autre-gitlab') || {}).textContent || '');
      await sp.fill('.dialogue-partage input[autocomplete="name"]', 'Alice Exemple');
      await sp.fill('.dialogue-partage input[type="email"]', 'alice@example.org');
      await sp.evaluate(() => [...document.querySelectorAll('.dialogue-partage .services-connexion button')].find((b) => b.textContent === 'GitHub').click());
      await sp.waitForSelector('.dialogue-compte .code-appareil code', { timeout: 30000 });
      const codeAppareil = await sp.evaluate(() => document.querySelector('.dialogue-compte .code-appareil code').textContent);
      const noteRelais = await sp.evaluate(() => (document.querySelector('.dialogue-compte .relais-connexion') || {}).textContent || '');
      await sp.waitForSelector('.dialogue-compte .ok-compte', { timeout: 60000 });
      await sp.evaluate(() => document.querySelector('.dialogue-compte').closest('.fond-dialogue').querySelector('.pied-dialogue button.principal').click());
      const creerPartager = await libelle(sp, 'versions_creer_partager');
      await attendre(sp, (x) => [...document.querySelectorAll('.dialogue-partage button.principal')].some((b) => b.textContent.trim() === x), creerPartager, 30000);
      await sp.evaluate((x) => [...document.querySelectorAll('.dialogue-partage button.principal')].find((b) => b.textContent.trim() === x).click(), creerPartager);
      await sp.waitForSelector('.dialogue-partage .ok-compte', { timeout: 60000 });
      const partageFait = await sp.evaluate(() => document.querySelector('.dialogue-partage .ok-compte').textContent);
      await sp.evaluate(() => document.querySelector('.dialogue-partage').closest('.fond-dialogue').querySelector('.pied-dialogue button.principal').click());
      const envoyees = await attendreDepot(1);
      const relaisAttendu = await libelle(sp, 'connexion_relais');
      verifier('Partager dans la page : connecté à GitHub par son code (le relais, le jeton gardé dans ce navigateur : dit), un dépôt privé fait, le projet envoyé par le relais de nuee.io',
        codeAppareil === 'WDJB-MJHT' && noteRelais === relaisAttendu && /desktop application|application de bureau/.test(autreGitlab) && /alice-exemple\/orchards/.test(partageFait)
          && envoyees.length === 1 && /\|First version$/.test(envoyees[0]),
        `${codeAppareil} ; ${partageFait} ; ${envoyees.join(' / ')} ; ${autreGitlab}`);

      // a colleague's change (a document), sent to the repository with git; then Get now
      const copie = path.join(racinePartage, 'collegue');
      gitSys(['clone', '-q', nu, copie]);
      const fichierCopie = path.join(copie, fs.readdirSync(copie).find((x) => x.endsWith('.nuee')));
      const j = JSON.parse(fs.readFileSync(fichierCopie, 'utf8'));
      j.documents = [...(j.documents || []), { id: 'D9', titre: 'Colleague notes', langue: 'en', texte: 'Seen by the colleague.' }];
      fs.writeFileSync(fichierCopie, `${JSON.stringify(j)}\n`);
      gitSys(['-c', 'user.name=Bob Exemple', '-c', 'user.email=bob@example.org', 'commit', '-q', '-am', 'Colleague notes added'], copie);
      gitSys(['push', '-q', 'origin', 'main'], copie);
      await montrerHistorique(sp, (await projetA(sp, premier.chemin)).id);
      await boutonHistorique(sp, 'versions_recevoir_maintenant');
      const recu = await attendre(sp, (c) => import('./app.js').then(({ app }) => {
        const q = app.projets().find((y) => !y.lecture_seule && y.chemin_enregistrement === c);
        return q && [].concat(q.documents || []).some((d) => d.titre === 'Colleague notes') ? { id: q.id, modifie: q.modifie } : null;
      }), premier.chemin, 120000).catch(() => null);
      verifier('Recevoir maintenant : la version d’un collègue reçue par le relais, le projet relu avec elle', !!recu && !recu.modifie, JSON.stringify(recu));

      // a change here, then Send now: saved first (downloaded), its version sent
      if (recu) await sp.evaluate((id) => import('./app.js').then(({ app }) => app.montrerProjet(id)), recu.id);
      await nouveauModele(sp, { type: 'LM', x: ['sugar'], y: ['taste_score'], echelle: 'none' });
      await sp.waitForTimeout(1000);
      const ici = await projetA(sp, premier.chemin);
      await montrerHistorique(sp, ici.id);
      await enregistrerPar(sp, () => boutonHistorique(sp, 'versions_envoyer_maintenant'));
      const apresEnvoi = await attendreDepot(3);
      verifier('Envoyer maintenant : le projet enregistré d’abord (téléchargé), sa version envoyée par le relais',
        apresEnvoi.length === 3 && /^Bob Exemple\|Colleague notes added$/.test(apresEnvoi[1]) && /^Alice Exemple\|/.test(apresEnvoi[0]), apresEnvoi.join(' / '));

      // Open from GitHub or GitLab: the repository's projects, then the project at a new place of the
      // browser, its versions with it (no folder asked)
      await commande(sp, 'versions_ouvrir_distant');
      await sp.waitForSelector('.dialogue-distant input[type="url"]', { timeout: 15000 });
      const dossierDemande = await sp.evaluate(async () => { const { t } = await import('./i18n.js'); return [...document.querySelectorAll('.dialogue-distant label')].some((l) => l.textContent === t('versions_distant_dossier')); });
      await sp.fill('.dialogue-distant input[type="url"]', 'https://github.com/alice-exemple/orchards');
      await sp.evaluate(async () => { const { t } = await import('./i18n.js'); [...document.querySelectorAll('.dialogue-distant button')].find((b) => b.textContent.trim() === t('versions_distant_chercher')).click(); });
      await sp.waitForSelector('.dialogue-distant .projets-distants input[type="radio"]', { timeout: 60000 });
      const nOuvrirDistant = await combien(sp, 'ouvrir');
      await sp.evaluate(() => document.querySelector('.dialogue-distant').closest('.fond-dialogue').querySelector('.pied-dialogue button.principal').click());
      await attendreDemande(sp, 'ouvrir', nOuvrirDistant, 120000);
      const distant = await attendre(sp, (c) => import('./app.js').then(async ({ app }) => {
        const { coucheVersions } = await import('./versions.js');
        const q = app.projets().find((y) => !y.lecture_seule && /^\/projets\//.test(y.chemin_enregistrement || '') && y.chemin_enregistrement !== c && y.chemin_enregistrement.split('/')[2] !== c.split('/')[2]);
        if (!q) return null;
        const l = await coucheVersions().liste(q.chemin_enregistrement);
        const e = await coucheVersions().etat(q.chemin_enregistrement);
        return { chemin: q.chemin_enregistrement, documents: [].concat(q.documents || []).map((d) => d.titre), versions: l.map((v) => `${v.auteur.nom}|${v.description}`), partage: e.partage && e.partage.url };
      }), premier.chemin, 120000).catch((e) => ({ erreur: e.message }));
      verifier('Ouvrir depuis GitHub ou GitLab dans la page : le projet du dépôt à une nouvelle place, ses versions avec lui, partagé avec ce dépôt (aucun dossier demandé)',
        !dossierDemande && !!distant.chemin && distant.versions.length === 3 && distant.versions.some((v) => v === 'Bob Exemple|Colleague notes added') && distant.documents.includes('Colleague notes')
          && distant.partage === 'https://github.com/alice-exemple/orchards.git', JSON.stringify(distant));

      // signed out, from History: the token no longer in this browser's storage
      await montrerHistorique(sp, (await projetA(sp, premier.chemin)).id);
      const deconnecter = await libelle(sp, 'comptes_deconnecter');
      await attendre(sp, (x) => [...document.querySelectorAll('.versions .compte-partage button')].some((b) => b.textContent.trim() === x), deconnecter, 30000);
      await sp.evaluate((x) => [...document.querySelectorAll('.versions .compte-partage button')].find((b) => b.textContent.trim() === x).click(), deconnecter);
      await sp.waitForSelector('.fond-dialogue .pied-dialogue button.principal', { timeout: 10000 });
      await sp.click('.fond-dialogue .pied-dialogue button.principal');
      // (History in front again: the project opened from the repository may have brought its views since)
      await sp.waitForTimeout(1000);
      await montrerHistorique(sp, (await projetA(sp, premier.chemin)).id);
      const jetonsGardes = async () => sp.evaluate(() => new Promise((ok) => {
        const r = indexedDB.open('nuee-jetons');
        r.onerror = () => ok(-1);
        r.onsuccess = () => {
          try { const q = r.result.transaction('jetons', 'readonly').objectStore('jetons').count(); q.onsuccess = () => ok(q.result); q.onerror = () => ok(-1); } catch { ok(-1); }
        };
      }));
      const connecter = await libelle(sp, 'comptes_connecter');
      const apresDeconnexion = await attendre(sp, (x) => [...document.querySelectorAll('.versions .compte-partage button')].some((b) => b.textContent.trim() === x), connecter, 30000).catch(() => false);
      const gardes = await jetonsGardes();
      const adresses = requetes.slice(debutPartage);
      const directs = adresses.filter((u) => /^https?:\/\/([^/]*\.)?(github\.com|gitlab\.com|githubusercontent\.com|gravatar\.com)\//.test(u) && !/^https:\/\/(api\.github\.com|avatars\.githubusercontent\.com)\//.test(u));
      const parRelais = adresses.filter((u) => u.startsWith(`${origine}/relais/github.com/`)).length;
      verifier('Se déconnecter : le jeton n’est plus dans ce navigateur ; rien n’a joint github.com ni gitlab.com que par le relais, aucun jeton dans une adresse',
        apresDeconnexion && gardes === 0 && !directs.length && !adresses.some((u) => u.includes(JETON)) && parRelais > 0,
        `${apresDeconnexion ? 'Se connecter… proposé' : 'pas de Se connecter…'} ; ${gardes} jeton(s) gardé(s) ; ${parRelais} demandes par le relais ; ${directs.slice(0, 3).join(', ') || 'rien en direct'}`);
      // a push the relay refuses for its size (413): said in words
      const tropGros = await sp.evaluate(() => import('./comptes_navigateur.js').then((m) => m.fetchGit('https://github.com/alice-exemple/trop-gros.git/git-receive-pack',
        { method: 'POST', body: new Uint8Array(8) })).then(() => null, (e) => e.message));
      verifier('des versions de plus de 100 Mo, que le relais refuse (413) : dit en mots', tropGros === (await libelle(sp, 'versions_relais_trop_gros')), tropGros);
      if (options.capture !== false) await sp.screenshot({ path: path.join(sortie, 'partage.png') });
      verifier('le partage sans violation de la politique de sécurité', (await sp.evaluate(() => window.__violations)).length === 0, await sp.evaluate(() => window.__violations));
      await copiesAJour(sp);
      await sp.close({ runBeforeUnload: true });
    } finally {
      banc.close();
    }
  }

  // (but the refusal of the random forest, which the interface writes to the console as any refusal)
  const autres = erreurs.filter((e) => !/desktop application is needed|application de bureau/.test(e));
  verifier('sans erreur dans la page', autres.length === 0, autres.slice(0, 10));
} catch (e) {
  verifier('erreur', false, e.stack || e.message);
  if (page) try { await page.screenshot({ path: path.join(sortie, 'navigateur-erreur.png') }); } catch { /* no page */ }
  // (every tab open, the one that failed among them, and what the pages said wrong so far)
  for (const [i, x] of contexte.pages().entries()) try { await x.screenshot({ path: path.join(sortie, `navigateur-erreur-${i + 1}.png`) }); } catch { /* closed */ }
  if (erreurs.length) console.log(`  Erreurs des pages : ${erreurs.slice(-20).join(' | ')}`);
} finally {
  fs.writeFileSync(path.join(sortie, 'demandes.json'), JSON.stringify(res, null, 1));
  await contexte.close().catch(() => {});
  if (serveur) serveur.close();
  console.log(`\n${echecs.length ? `Échecs : ${echecs.join(', ')}` : 'Toutes les vérifications sont passées.'} (${sortie})`);
  process.exit(echecs.length ? 1 : 0);
}
