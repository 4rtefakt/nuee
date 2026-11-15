// The picture a link to nuee.io shows when it is shared (src/assets/og.png, 1200 × 630): the name,
// a line, and the example's scores plot from donnees.json, drawn with the site's own styles and
// photographed by a headless Chrome or Edge (CHROME=… to say which).
//   node vitrine/image.mjs        (after a change to the look or to the data; then build again)

import { readFileSync, writeFileSync, existsSync, mkdtempSync, rmSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { tmpdir } from 'node:os';
import path from 'node:path';

const ICI = path.dirname(fileURLToPath(import.meta.url));
const ASSETS = path.join(ICI, 'src', 'assets');
const acp = JSON.parse(readFileSync(path.join(ICI, 'src', 'donnees.json'), 'utf8')).formes.acp;
const navigateur = [process.env.CHROME, 'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe', 'C:/Program Files/Google/Chrome/Application/chrome.exe',
  '/usr/bin/google-chrome', '/usr/bin/chromium', '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'].find((c) => c && existsSync(c));
if (!navigateur) throw new Error('No Chrome or Edge found: set CHROME to its path');

// the plot, 560 × 440: the axes through the middle, the ellipse, the apples in their varieties' colours
const L = 560;
const H = 440;
const COULEURS = ['#2f6fb5', '#d1495b', '#3b9a5b', '#e09f3e'];
const X = (x) => (x * L).toFixed(1);
const Y = (y) => ((1 - y) * H).toFixed(1);
const graphique = `<svg viewBox="0 0 ${L} ${H}" width="${L}" height="${H}">
  <line x1="0" y1="${H / 2}" x2="${L}" y2="${H / 2}"/><line x1="${L / 2}" y1="0" x2="${L / 2}" y2="${H}"/>
  <polygon points="${acp.deco.ellipse.map(([x, y]) => `${X(x)},${Y(y)}`).join(' ')}" stroke-dasharray="5 5"/>
  ${acp.points.map(([x, y, g]) => `<circle cx="${X(x)}" cy="${Y(y)}" r="5.2" fill="${COULEURS[g]}" stroke="none"/>`).join('')}
</svg>`;

const dossier = mkdtempSync(path.join(tmpdir(), 'nuee-og-'));
const page = path.join(dossier, 'og.html');
writeFileSync(page, `<!doctype html><meta charset="utf-8">
<link rel="stylesheet" href="${pathToFileURL(path.join(ASSETS, 'site.css')).href}">
<style>
  html, body { width: 1200px; height: 630px; overflow: hidden; }
  body { display: grid; grid-template-columns: 540px 1fr; align-items: center; padding: 0 0 0 76px; }
  h1 { font: 460 232px/.8 var(--serif); font-variation-settings: "SOFT" 100, "WONK" 1; letter-spacing: -.05em; margin: 0 0 34px -10px; color: var(--bleu); }
  .devise { font: 400 39px/1.12 var(--serif); font-variation-settings: "SOFT" 60, "WONK" 1; letter-spacing: -.02em; margin: 0 0 30px; }
  .devise em { color: var(--bleu); font-weight: 340; }
  .liste { font: 400 17px/1.5 var(--mono); color: var(--encre-douce); margin: 0; }
  svg { justify-self: center; overflow: visible; }
  svg line, svg polygon { fill: none; stroke: var(--encre); stroke-width: 1.3; }
  svg polygon { stroke: var(--encre-douce); }
</style>
<div>
  <h1>Nuée</h1>
  <p class="devise">Statistics you can read,<br><em>computed in R.</em></p>
  <p class="liste">PCA · PLS · mixed models · communities<br>monitoring · maps — nuee.io</p>
</div>
${graphique}
`);
const sortie = path.join(ASSETS, 'og.png');
execFileSync(navigateur, ['--headless=new', '--disable-gpu', '--hide-scrollbars', '--force-device-scale-factor=1', '--window-size=1200,630',
  '--virtual-time-budget=4000', `--screenshot=${sortie}`, pathToFileURL(page).href], { stdio: 'ignore' });
rmSync(dossier, { recursive: true, force: true });
console.log(`${path.relative(process.cwd(), sortie)} written`);
