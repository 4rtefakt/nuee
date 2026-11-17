// The browser version used end to end in a headless Chromium (Playwright), as a person would:
// the site built (construire_navigateur.js) and served on this computer with the headers webR
// needs (servir_navigateur.js); R starts in the page; the example project opens; a PLS, a mixed
// model and a PCoA are fitted and their views drawn; the data exported as CSV (a download);
// random forests, whose package the browser version lacks, say they need the desktop app while
// the rest goes on; the recovery copies: a reload, then a closed tab, offer the projects again,
// with their models and their unsaved changes, and a second tab open meanwhile is offered
// nothing, and without OPFS's createWritable they go to IndexedDB. Then 1.1 in a tab of its own:
// History and Share say they need the desktop app; a table opened (File › Open), a data step (New
// variable), Data steps, Update the data with next year's file chosen in the page, What changed;
// a project saved, opened, saved again, downloaded each time; an R-code model, its package
// (Kendall) installed from webR's repository, and one needing ranger refused; a plugin installed
// from its file and fitted; the help panel's guide; reloaded, the plugin and the approvals still
// there. The time of each request goes to <folder>/demandes.json.
//   node tests/essai_navigateur.mjs [--site=<built site>] [--sortie=<folder>] [--visible]
// Chromium comes from node_modules/.cache/ms-playwright (PLAYWRIGHT_BROWSERS_PATH):
//   PLAYWRIGHT_BROWSERS_PATH=node_modules/.cache/ms-playwright node node_modules/playwright-core/cli.js install chromium-headless-shell
// The first start downloads R's packages from repo.r-wasm.org. Exits with 1 when a check fails.
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import { createRequire } from 'node:module';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const ICI = path.dirname(fileURLToPath(import.meta.url));
const APP = path.join(ICI, '..');
const require = createRequire(import.meta.url);
const { servir } = require('../servir_navigateur.js');
process.env.PLAYWRIGHT_BROWSERS_PATH ||= path.join(APP, 'node_modules', '.cache', 'ms-playwright');
const { chromium } = await import('playwright-core');

const options = Object.fromEntries(process.argv.slice(2).filter((a) => a.startsWith('--')).map((a) => {
  const [k, ...v] = a.slice(2).split('=');
  return [k, v.join('=') || true];
}));
const sortie = typeof options.sortie === 'string' ? path.resolve(options.sortie) : fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-navigateur-'));
fs.mkdirSync(sortie, { recursive: true });
let site = typeof options.site === 'string' ? path.resolve(options.site) : null;
if (!site) {
  site = path.join(sortie, 'site');
  execFileSync(process.execPath, [path.join(APP, 'construire_navigateur.js'), site], { stdio: 'inherit' });
}
const serveur = await servir(site, 0);
const adresse = `http://127.0.0.1:${serveur.address().port}/`;
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
const contexte = await chromium.launchPersistentContext(profil, { headless: !options.visible, acceptDownloads: true, viewport: { width: 1440, height: 900 } });

// a page of the site: its errors noted, every request timed (api.js: window.nueeDemandes)
async function ouvrirPage(url = adresse) {
  const page = await contexte.newPage();
  page.on('pageerror', (e) => erreurs.push(`page : ${e.message}`));
  page.on('console', (m) => { if (m.type() === 'error') erreurs.push(`console : ${m.text()}`); });
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
const doubleClic = (page, texte) => page.evaluate((x) => {
  const n = [...document.querySelectorAll('.arbre *')].find((e) => e.children.length === 0 && e.textContent.trim() === x);
  if (!n) throw new Error(`not in the tree: ${x}`);
  n.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
}, texte);
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
const copies = (page) => page.evaluate(async () => {
  const lire = async () => {
    try {
      const d = await (await navigator.storage.getDirectory()).getDirectoryHandle('nuee-secours');
      const r = {};
      for await (const [nom, h] of d.entries()) if (nom.startsWith('index-')) r[nom] = JSON.parse(await (await h.getFile()).text());
      return r;
    } catch { return {}; }
  };
  return lire();
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
  const absents = await page.evaluate(() => import('./api.js').then(async (a) => (await a.moteurWebR()).absents()));
  verifier('R démarré dans la page', true, `${res.demarrage_s.toFixed(1)} s ; paquets absents : ${absents.join(', ')}`);
  const version = await page.evaluate(() => import('./api.js').then((a) => a.demander('version')));
  res.R = version.R;

  // ---- the example project, a PLS and its views
  res.exemple = await commande(page, 'projet_exemple');
  await page.waitForFunction(() => document.querySelectorAll('.js-plotly-plot').length >= 2, null, { timeout: 120000 });
  verifier('projet d’exemple ouvert, ses graphiques', true, `${res.exemple} : ${await graphiques(page)} graphiques`);
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
  const statutTexte = () => page.evaluate(() => document.getElementById('statut').textContent);
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

  // what only the desktop application does: History, Share, said in the status bar
  const desktop = [];
  for (const cle of ['versions', 'versions_partager']) {
    await commande(page, cle);
    await page.waitForTimeout(400);
    desktop.push(await statutTexte());
  }
  verifier('Historique… et Partager : « il faut l’application de bureau »', desktop.every((s) => /desktop application is needed|application de bureau/.test(s)), desktop.join(' | '));

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
  await page.waitForFunction(() => document.querySelector('.dialogue-maj .resume-maj'), null, { timeout: 60000 });
  const apercuMaj = await page.evaluate(() => document.querySelector('.dialogue-maj .resume-maj').textContent);
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

  // R code: approved when fitted, its package (Kendall) installed from webR's repository; ranger refused
  const codeR = 'k <- Kendall::Kendall(donnees$sugar, donnees$taste_score)\nfit <- lm(taste_score ~ sugar + acidity, data = donnees)\nlist(modele = fit, valeurs = c(tau = unname(k$tau)))';
  const rc = await nouveauModele(page, { type: 'CODE', options: { code: codeR } });
  await page.waitForTimeout(1500);
  const mCode = await modeleActif(page);
  verifier('un modèle de code R approuvé, son paquet (Kendall) installé depuis le dépôt de webR', !!rc.d && rc.d.ok && !rc.erreur && !!mCode && mCode.type === 'CODE',
    rc.erreur || `${rc.d && Math.round(rc.d.ms)} ms ; ${mCode && mCode.nom}`);
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
  await commande(page, 'guide');
  const aide = await attendre(page, () => { const c = document.querySelector('.aide-corps'); return c && c.textContent.length > 500 ? c.textContent.length : 0; }, null, 30000).catch(() => 0);
  verifier('le guide dans le panneau d’aide (F1)', aide > 500, `${aide} caractères`);
  verifier('rechargé : le greffon encore installé et approuvé, le code R encore approuvé',
    apresRechargement.greffons.some((x) => x.id === 'mann-kendall' && x.permis) && apresRechargement.codes.length >= 1 && apresRechargement.codes.some((c) => c.approuve),
    JSON.stringify(apresRechargement));

  // (but the refusal of the random forest, which the interface writes to the console as any refusal)
  const autres = erreurs.filter((e) => !/desktop application is needed|application de bureau/.test(e));
  verifier('sans erreur dans la page', autres.length === 0, autres.slice(0, 10));
} catch (e) {
  verifier('erreur', false, e.stack || e.message);
  if (page) try { await page.screenshot({ path: path.join(sortie, 'navigateur-erreur.png') }); } catch { /* no page */ }
} finally {
  fs.writeFileSync(path.join(sortie, 'demandes.json'), JSON.stringify(res, null, 1));
  await contexte.close().catch(() => {});
  serveur.close();
  console.log(`\n${echecs.length ? `Échecs : ${echecs.join(', ')}` : 'Toutes les vérifications sont passées.'} (${sortie})`);
  process.exit(echecs.length ? 1 : 0);
}
