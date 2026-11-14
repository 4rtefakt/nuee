// Checks zotero.js (Zotero on this computer, for documents' citations) against a stand-in for
// Zotero's server that behaves as Zotero's does: it drops what comes from a browser (a "Mozilla/"
// user agent, an Origin header), answers its local API with BibTeX and Better BibTeX's picker.
//   node tests/essai_zotero.js
const http = require('http');
const path = require('path');
const { demanderZotero } = require('../zotero');

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};

const BIB = '@article{wold_pls-regression_2001,\n  title = {{PLS}-regression},\n  author = {Wold, Svante and Sj{\\"o}str{\\"o}m, Michael},\n  year = {2001},\n  journal = {Chemom. Intell. Lab.},\n  file = {C:\\\\Users\\\\x\\\\a.pdf}\n}\n';
// (what Better BibTeX's picker gives, then its export of those references)
const CHOIX = [{ citationKey: 'broSurvie2019', locator: '12', label: 'page', prefix: 'voir', suffix: '', suppressAuthor: false, item: { itemKey: 'QMRNMBF9', libraryID: 1 } }];
const CSL = [{ id: 'broSurvie2019', type: 'article-journal', title: 'Survie de la perdrix grise (<i>Perdix perdix</i>)', author: [{ family: 'Bro', given: 'Elisabeth' }],
  issued: { 'date-parts': [['2019']] } }];
const recues = [];
const serveur = http.createServer((req, res) => {
  recues.push({ url: req.url, ua: req.headers['user-agent'] || '', origin: req.headers.origin, version: req.headers['zotero-api-version'] });
  // (Zotero: "Preventing request from browser")
  if ((req.headers['user-agent'] || '').startsWith('Mozilla/') || 'origin' in req.headers) { req.socket.destroy(); return; }
  const u = new URL(req.url, 'http://x');
  if (u.pathname === '/api/users/0/items/top' && u.searchParams.get('format') === 'bibtex') {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end(/wold|pls/i.test(u.searchParams.get('q') || '') ? BIB : '');
  } else if (u.pathname === '/api/users/0/items/top' && u.searchParams.get('format') === 'keys') {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('ABCD1234');
  } else if (u.pathname === '/better-bibtex/cayw' && u.searchParams.get('probe')) {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end('ready');
  } else if (u.pathname === '/better-bibtex/cayw' && u.searchParams.get('format') === 'json') {
    setTimeout(() => { res.writeHead(200, { 'Content-Type': 'application/json' }); res.end(JSON.stringify(CHOIX)); }, 300);  // (the user picks)
  } else if (u.pathname === '/better-bibtex/export/item' && u.searchParams.get('translator') === 'Better CSL JSON') {
    const cles = (u.searchParams.get('citationKeys') || '').split(',');
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end(JSON.stringify(CSL.filter((x) => cles.includes(x.id) && u.searchParams.get('libraryID') === '1')));
  } else if (u.pathname === '/api/users/0/collections') {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify([{ key: 'ABCD1234', data: { name: 'Perdrix', parentCollection: false }, meta: { numItems: 1 } }]));
  } else if (u.pathname === '/api/users/0/collections/ABCD1234/items/top') {
    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end(BIB);
  } else if (u.pathname === '/api/users/0/items' && u.searchParams.get('lent')) {
    /* never answers */
  } else {
    res.writeHead(404);
    res.end('No endpoint found');
  }
});

(async () => {
  await new Promise((r) => serveur.listen(0, '127.0.0.1', r));
  const port = serveur.address().port;
  const z = (chemin, o = {}) => demanderZotero(chemin, { port, ...o });

  const r = await z('/api/users/0/items/top?q=wold&qmode=titleCreatorYear&itemType=-attachment&itemType=-note&format=bibtex&limit=25');
  verifier('recherche dans la bibliothèque (API locale), en BibTeX', r.statut === 200 && r.texte.includes('@article{wold_pls-regression_2001'), JSON.stringify(r).slice(0, 200));
  const d = recues[recues.length - 1];
  verifier('rien de ce qui ferait refuser la demande (navigateur), la version de l’API', !d.ua.startsWith('Mozilla/') && d.origin === undefined && d.version === '3', JSON.stringify(d));
  const { lireBibtex } = await import(path.join(__dirname, '..', 'interface', 'bibliographie.js').replace(/\\/g, '/').replace(/^([A-Za-z]):/, 'file:///$1:'));
  const e = lireBibtex(r.texte).entrees[0];
  verifier('ce que Zotero envoie se lit', e && e.cle === 'wold_pls-regression_2001' && e.auteurs[1].nom === 'Sjöström', JSON.stringify(e && e.auteurs));
  const choix = await z('/better-bibtex/cayw?format=json&minimize=true', { delai: 5000 });
  const exporte = await z(`/better-bibtex/export/item?citationKeys=${encodeURIComponent('broSurvie2019')}&translator=${encodeURIComponent('Better CSL JSON')}&libraryID=1`);
  verifier('Better BibTeX : présent, son sélecteur, puis l’export de ce qui y est choisi', (await z('/better-bibtex/cayw?probe=1')).texte === 'ready'
    && JSON.parse(choix.texte)[0].citationKey === 'broSurvie2019' && JSON.parse(exporte.texte)[0].id === 'broSurvie2019', JSON.stringify([choix, exporte]).slice(0, 300));
  const coll = await z('/api/users/0/collections?format=json&limit=1000');
  const dans = await z('/api/users/0/collections/ABCD1234/items/top?itemType=-attachment&itemType=-note&format=bibtex&limit=300');
  verifier('les collections, et les références d’une collection', coll.statut === 200 && JSON.parse(coll.texte)[0].key === 'ABCD1234' && dans.texte === BIB, JSON.stringify([coll, dans]).slice(0, 200));
  const refus = await Promise.all(['/api/users/0/items/ABCD/children', '/api/users/1/items?q=x', '/connector/saveItems', '/api/users/0/items/top?q=x#a', 'http://autre/api/users/0/items?q=x',
    '/api/users/0/collections/abc/items/top?format=bibtex', '/api/users/0/collections/ABCD1234/items?format=bibtex', '/api/users/0/collections/ABCD1234?format=json',
    '/better-bibtex/json-rpc', '/better-bibtex/export/collection?/1/x.bib', '/better-bibtex/export/item#x'].map((c) => z(c)));
  verifier('les autres adresses sont refusées sans rien demander', refus.every((x) => x.erreur === 'demande'), JSON.stringify(refus));
  const t0 = Date.now();
  const lent = await demanderZotero('/api/users/0/items?lent=1', { port, delai: 300 });
  verifier('une demande sans réponse s’arrête au délai', lent.erreur === 'delai' && Date.now() - t0 < 2000, JSON.stringify(lent));
  serveur.close();
  await new Promise((r) => setTimeout(r, 100));
  const absent = await demanderZotero('/connector/ping', { port, delai: 2000 });
  verifier('Zotero absent : « absent »', absent.erreur === 'absent', JSON.stringify(absent));

  console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
  process.exit(echecs ? 1 : 0);
})();
