// The browser version's speed against native R on this computer, with the same requests (not a
// check): the example project opened, its four models fitted (PCA, PLS, PLS-DA, OPLS-DA), a mixed
// model and a PCoA, and the PLS's permutation test (100 permutations); in a headless Chromium
// (R in the page, webR; the permutation test shared with the other R of the page, as the
// interface shares it, then in one R: ?aides=0) and through the desktop's local R server
// (moteur/serveur.R). Each request as the interface sends it, timed from the page or from Node;
// a first round to load what each R loads once, then the median of the rounds after it. Also
// the start of R in the page: a first visit (R's packages downloaded) and the next one.
//   node tests/banc_navigateur.mjs [--site=<built site>] [--tours=3] [--rscript=<Rscript>] [--sortie=<folder>]
import { execFileSync, spawn } from 'node:child_process';
import crypto from 'node:crypto';
import fs from 'node:fs';
import { createRequire } from 'node:module';
import net from 'node:net';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const ICI = path.dirname(fileURLToPath(import.meta.url));
const APP = path.join(ICI, '..');
const DEPOT = path.join(APP, '..');
const require = createRequire(import.meta.url);
const { servir } = require('../servir_navigateur.js');
process.env.PLAYWRIGHT_BROWSERS_PATH ||= path.join(APP, 'node_modules', '.cache', 'ms-playwright');
const { chromium } = await import('playwright-core');

const options = Object.fromEntries(process.argv.slice(2).filter((a) => a.startsWith('--')).map((a) => {
  const [k, ...v] = a.slice(2).split('=');
  return [k, v.join('=') || true];
}));
const TOURS = Number(options.tours) || 3;
const sortie = typeof options.sortie === 'string' ? path.resolve(options.sortie) : fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-banc-'));
fs.mkdirSync(sortie, { recursive: true });
const lanceur = typeof options.rscript === 'string' ? options.rscript
  : process.platform === 'win32' ? path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe') : 'Rscript';
const rscript = fs.existsSync(path.join(path.dirname(lanceur), 'x64', 'Rscript.exe')) ? path.join(path.dirname(lanceur), 'x64', 'Rscript.exe') : lanceur;
let site = typeof options.site === 'string' ? path.resolve(options.site) : null;
if (!site) {
  site = path.join(sortie, 'site');
  execFileSync(process.execPath, [path.join(APP, 'construire_navigateur.js'), site], { stdio: 'inherit' });
}

// The requests, in order, each { nom, q } from what came before; `demander(nom, q)` -> answer
async function sequence(demander, horloge) {
  const temps = [];
  const fois = async (cle, nom, q) => {
    const t0 = horloge();
    const r = await demander(nom, q);
    temps.push([cle, horloge() - t0]);
    return r;
  };
  const p = await fois('ouvrir l’exemple', 'exemple', {});
  const u = (nom) => p.variables.find((v) => v.nom === nom).uid;
  const m = [];
  for (const [i, n] of [[1, 'ACP M1'], [2, 'PLS M2'], [3, 'PLS-DA M3'], [4, 'OPLS-DA M4']].entries()) m[i] = await fois(`ajuster ${n[1]}`, 'ajuster', { projet: p.id, numero: n[0] });
  await fois('ajuster un modèle mixte', 'ajuster', { projet: p.id, type: 'LMM', x: ['sugar', 'firmness', 'acidity'].map(u), y: [u('taste_score')], echelle: 'none',
    aleatoires: [{ by: 'variable', variable: u('orchard') }], nom: 'LMM' });
  const chimie = ['weight', 'diameter', 'firmness', 'sugar', 'acidity', 'starch', 'dry_matter', 'polyphenols', 'colour_a', 'colour_b', 'juice', 'calcium', 'nitrogen', 'storage'];
  await fois('ajuster une PCoA (Gower, 99 permutations)', 'ajuster', { projet: p.id, type: 'PCOA', x: [u('variety')], y: chimie.map(u), echelle: 'none',
    options: { distance: 'gower', permutations: 99, graine: 1 }, nom: 'PCoA' });
  await fois('test de permutation de M2 (100)', 'permutations', { modele: m[1].id, n: 100 });
  return temps;
}
const mediane = (x) => { const s = [...x].sort((a, b) => a - b); return s.length % 2 ? s[(s.length - 1) / 2] : (s[s.length / 2 - 1] + s[s.length / 2]) / 2; };
function resumer(tours) {
  const r = {};
  for (const t of tours.slice(1)) for (const [cle, ms] of t) (r[cle] ||= []).push(ms);
  return Object.fromEntries(Object.entries(r).map(([k, v]) => [k, { mediane: mediane(v), tours: v.map(Math.round), premier: Math.round(tours[0].find((x) => x[0] === k)[1]) }]));
}

// ---- native R, through the desktop's local server
function portLibre() {
  return new Promise((ok) => { const s = net.createServer(); s.listen(0, '127.0.0.1', () => { const { port } = s.address(); s.close(() => ok(port)); }); });
}
async function natif() {
  const port = await portLibre();
  const jeton = crypto.randomBytes(32).toString('hex');
  const r = spawn(rscript, [path.join(DEPOT, 'moteur', 'serveur.R'), String(port), path.join(APP, 'interface')], { windowsHide: true, env: { ...process.env, NUEE_JETON: jeton } });
  let sortieR = '';
  await new Promise((ok, ko) => {
    r.stdout.on('data', (d) => { sortieR += d; if (sortieR.includes('PRET')) ok(); });
    r.stderr.on('data', (d) => { sortieR += d; });
    r.on('exit', (code) => ko(new Error(`R : ${code} ${sortieR.slice(-500)}`)));
  });
  const demander = async (nom, q) => {
    const rep = await fetch(`http://127.0.0.1:${port}/api/${nom}`, { method: 'POST', body: JSON.stringify(q), headers: { Cookie: `nuee_${port}=${jeton}`, 'X-Langue': 'en', 'Content-Type': 'application/json' } });
    const j = await rep.json();
    if (!rep.ok) throw new Error(`${nom} : ${j.erreur}`);
    return j;
  };
  const version = await demander('version', {});
  const tours = [];
  for (let i = 0; i <= TOURS; i += 1) tours.push(await sequence(demander, () => performance.now()));
  r.kill();
  return { R: version.R, temps: resumer(tours) };
}

// ---- R in the page
async function navigateur(adresse, profil, aides) {
  const contexte = await chromium.launchPersistentContext(profil, { headless: true });
  const page = await contexte.newPage();
  const t0 = Date.now();
  await page.goto(`${adresse}${aides === null ? '' : `?aides=${aides}`}`);
  await page.waitForFunction(() => document.querySelectorAll('.menu-haut').length > 3, null, { timeout: 60000 });
  const demarrage = await page.evaluate(async () => {
    const debut = performance.now();
    const a = await import('./api.js');
    await a.demander('version');
    return performance.now() - debut;
  });
  const total = (Date.now() - t0) / 1000;
  // (the other R of the page, started 2 s after the first: ready before the permutation test)
  await page.evaluate(async () => { const a = await import('./api.js'); await (await a.moteurWebR()).aides(); });
  const r = await page.evaluate(async ([seq, n]) => {
    const a = await import('./api.js');
    const sequence = new Function(`return (${seq})`)();
    const tours = [];
    for (let i = 0; i <= n; i += 1) tours.push(await sequence((nom, q) => a.demander(nom, q), () => performance.now()));
    const v = await a.demander('version');
    return { tours, R: v.R, coeurs: navigator.hardwareConcurrency, aides: ((await (await a.moteurWebR()).aides()) || []).length, ua: navigator.userAgent };
  }, [sequence.toString(), TOURS]);
  await contexte.close();
  return { demarrage_s: demarrage / 1000, page_s: total, R: r.R, coeurs: r.coeurs, aides: r.aides, navigateur: r.ua.match(/(HeadlessChrome|Chrome)\/[\d.]+/)?.[0], temps: resumer(r.tours) };
}

const serveur = await servir(site, 0);
const adresse = `http://127.0.0.1:${serveur.address().port}/`;
const profil = path.join(sortie, 'profil');
fs.rmSync(profil, { recursive: true, force: true });
const resultats = {};
resultats.natif = await natif();
console.log(`R natif (${resultats.natif.R}) : fait`);
// (the first visit, an empty profile, then the next one, the packages in the browser's cache;
// --inverser: one R first)
const ordre = options.inverser ? [['navigateur_un_R', 0], ['navigateur', null]] : [['navigateur', null], ['navigateur_un_R', 0]];
for (const [cle, aides] of ordre) {
  resultats[cle] = await navigateur(adresse, profil, aides);
  console.log(`Navigateur, ${resultats[cle].aides} autres R : fait (démarrage ${resultats[cle].demarrage_s.toFixed(1)} s)`);
}
serveur.close();
fs.writeFileSync(path.join(sortie, 'banc.json'), JSON.stringify(resultats, null, 1));

const s = (ms) => (ms / 1000).toFixed(2);
console.log(`\nMédiane de ${TOURS} tours (secondes) | R natif | navigateur (${resultats.navigateur.aides} autres R) | rapport | navigateur (un R) | rapport`);
for (const cle of Object.keys(resultats.natif.temps)) {
  const n = resultats.natif.temps[cle].mediane;
  const b = resultats.navigateur.temps[cle].mediane;
  const u = resultats.navigateur_un_R.temps[cle].mediane;
  console.log(`${cle} | ${s(n)} | ${s(b)} | ${(b / n).toFixed(1)}× | ${s(u)} | ${(u / n).toFixed(1)}×`);
}
const [premiere, suivante] = ordre.map(([cle]) => resultats[cle].demarrage_s.toFixed(1));
console.log(`R dans la page prêt : ${premiere} s à la première visite, ${suivante} s à la suivante ; ${sortie}`);
