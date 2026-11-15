// Checks the links of the documentation website (docs/site): every link and image of every page
// leads to a file of the site, and every #anchor to an element of that page; the pages are all
// reachable from the home page. With --externes, the links to other sites are fetched too.
//   node docs/site/verifier_liens.js [--externes]
// Exits with 1 when a link is broken.
'use strict';
const fs = require('fs');
const path = require('path');

const SITE = __dirname;
const externes = process.argv.includes('--externes');
const IGNORES = new Set(['sources', 'node_modules']);

function fichiers(dossier, res = []) {
  for (const e of fs.readdirSync(dossier, { withFileTypes: true })) {
    if (IGNORES.has(e.name)) continue;
    const f = path.join(dossier, e.name);
    if (e.isDirectory()) fichiers(f, res);
    else if (e.name.endsWith('.html')) res.push(f);
  }
  return res;
}

const pages = fichiers(SITE);
const ids = new Map();      // page -> its ids
const liens = new Map();    // page -> its links
for (const p of pages) {
  const html = fs.readFileSync(p, 'utf8');
  ids.set(p, new Set([...html.matchAll(/\sid="([^"]+)"/g)].map((m) => m[1])));
  liens.set(p, [...html.matchAll(/\s(href|src)="([^"]*)"/g)].map((m) => m[2].replace(/&amp;/g, '&')));
}

const casses = [];
const dehors = new Map();   // external link -> the pages that have it
const atteintes = new Set();
for (const p of pages) {
  for (const l of liens.get(p)) {
    if (/^(https?:)?\/\//.test(l)) { if (!dehors.has(l)) dehors.set(l, []); dehors.get(l).push(p); continue; }
    if (/^(mailto|data|javascript):/.test(l)) continue;
    const [chemin, ancre] = l.split('#');
    let cible = chemin ? path.resolve(path.dirname(p), decodeURIComponent(chemin.split('?')[0])) : p;
    if (chemin && (chemin.endsWith('/') || (fs.existsSync(cible) && fs.statSync(cible).isDirectory()))) cible = path.join(cible, 'index.html');
    if (!cible.startsWith(SITE)) { casses.push(`${path.relative(SITE, p)}: ${l} (outside the site)`); continue; }
    if (!fs.existsSync(cible)) { casses.push(`${path.relative(SITE, p)}: ${l} (no such file)`); continue; }
    if (cible.endsWith('.html')) atteintes.add(cible);
    if (ancre && cible.endsWith('.html') && !(ids.get(cible) || new Set()).has(ancre)) casses.push(`${path.relative(SITE, p)}: ${l} (no #${ancre} there)`);
  }
}

// the pages a reader can reach, from the root
const accessibles = new Set([path.join(SITE, 'index.html')]);
const aVoir = [...accessibles];
while (aVoir.length) {
  const p = aVoir.pop();
  for (const l of liens.get(p) || []) {
    if (/^(https?:)?\/\/|^(mailto|data|javascript):/.test(l)) continue;
    const chemin = l.split('#')[0];
    if (!chemin) continue;
    let c = path.resolve(path.dirname(p), decodeURIComponent(chemin));
    if (chemin.endsWith('/')) c = path.join(c, 'index.html');
    if (c.endsWith('.html') && !accessibles.has(c) && fs.existsSync(c)) { accessibles.add(c); aVoir.push(c); }
  }
}
const orphelines = pages.filter((p) => !accessibles.has(p));

(async () => {
  if (externes) {
    for (const [l, ou] of dehors) {
      let etat;
      try {
        const r = await fetch(l, { method: 'GET', redirect: 'follow', headers: { 'User-Agent': 'link-checker' } });
        etat = r.status;
      } catch (e) { etat = e.message; }
      const ok = typeof etat === 'number' && etat < 400;
      console.log(`${ok ? '  ok    ' : '  ÉCHEC '} ${etat}  ${l}${ok ? '' : `  (${ou.length} page(s), e.g. ${path.relative(SITE, ou[0])})`}`);
      if (!ok) casses.push(`external: ${l} (${etat})`);
    }
  }
  const n = [...liens.values()].reduce((s, x) => s + x.length, 0);
  console.log(`${pages.length} pages, ${n} links (${dehors.size} distinct external ones${externes ? ', fetched' : ', not fetched: --externes'})`);
  for (const o of orphelines) console.log(`  not reachable from index.html: ${path.relative(SITE, o)}`);
  for (const c of casses) console.log(`  BROKEN ${c}`);
  if (casses.length || orphelines.length) process.exit(1);
  console.log('Every link works.');
})();
