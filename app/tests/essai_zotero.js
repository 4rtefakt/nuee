// Checks zotero.js (Zotero on this computer, for documents' citations) against a stand-in for
// Zotero's server that behaves as Zotero's does: it drops what comes from a browser (a "Mozilla/"
// user agent, an Origin header), answers its local API with BibTeX and Better BibTeX's picker; then
// against Zotero's real answers for group libraries, replayed (tests/donnees/zotero: Zotero 10.0.8
// alone and with Better BibTeX 9.0.77, with My Library, a group the user can edit and one read only).
//   node tests/essai_zotero.js
const http = require('http');
const fs = require('fs');
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

  // ---------------------------------------------------------------- group libraries, real answers
  console.log('Bibliothèques de groupe (réponses enregistrées de Zotero)');
  const { lireReponseZotero, lireCslJson } = await import(path.join(__dirname, '..', 'interface', 'bibliographie.js').replace(/\\/g, '/').replace(/^([A-Za-z]):/, 'file:///$1:'));
  // (each folder's index: the address asked, its status, its type; `api_fermee`: what any /api/
  // address answers with the local API off)
  const rejouer = (dossier, { fermee = false } = {}) => {
    const index = JSON.parse(fs.readFileSync(path.join(__dirname, 'donnees', 'zotero', dossier, 'index.json'), 'utf8'));
    const lire = (f) => ({ ...index[f], texte: fs.readFileSync(path.join(__dirname, 'donnees', 'zotero', dossier, f), 'utf8') });
    const par = new Map(Object.keys(index).filter((f) => f !== 'api_fermee.txt' && f !== 'cayw_annule.txt').map((f) => [index[f].chemin, f]));
    const s = http.createServer((req, res) => {
      if ((req.headers['user-agent'] || '').startsWith('Mozilla/') || 'origin' in req.headers) { req.socket.destroy(); return; }
      const f = fermee && req.url.startsWith('/api/') ? 'api_fermee.txt' : par.get(req.url);
      if (!f) { res.writeHead(404, { 'Content-Type': 'text/plain' }); res.end('No endpoint found'); return; }
      const r = lire(f);
      res.writeHead(r.statut, { 'Content-Type': r.type });
      res.end(r.texte);
    });
    return new Promise((ok) => s.listen(0, '127.0.0.1', () => ok({ s, port: s.address().port, index, lire })));
  };
  // what documents.js asks (etatZotero, chercherZotero, collectionsZotero)
  const SANS = 'itemType=-attachment&itemType=-note';
  const recherche = (b, q) => `/api/${b}/items/top?q=${q}&qmode=titleCreatorYear&${SANS}&format=json&include=bibtex&limit=25`;
  const clesEtLieux = async (port, b, q) => lireReponseZotero((await demanderZotero(recherche(b, q), { port })).texte).map((e) => `${e.cle} ${e.zotero}`);
  for (const [dossier, bbt] of [['better-bibtex', true], ['simple', false]]) {
    const z = await rejouer(dossier);
    const p = z.port;
    const groupes = await demanderZotero('/api/users/0/groups?format=json', { port: p });
    const l = JSON.parse(groupes.texte);
    verifier(`${dossier} : les groupes listés (pas Ma bibliothèque), leur id et leur nom`, groupes.statut === 200
      && JSON.stringify(l.map((g) => [g.id, g.data.name])) === JSON.stringify([[6011001, 'Équipe Camargue'], [6011002, 'Réseau Perdrix']]), groupes.texte.slice(0, 200));
    const moi = await clesEtLieux(p, 'users/0', 'bechet');
    const camargue = await clesEtLieux(p, 'groups/6011001', 'bechet');
    const cle = bbt ? 'bechetPopulationDynamicsGreater2019' : 'bechet_population_2019';
    verifier(`${dossier} : la même clé dans Ma bibliothèque et dans le groupe, pour deux œuvres, que leur lieu distingue`,
      moi.length === 1 && moi[0].startsWith(`${cle} users/0/items/`) && camargue.some((x) => x.startsWith(`${cle} groups/6011001/items/`)) && camargue.length === 2, JSON.stringify([moi, camargue]));
    const perdrix = await clesEtLieux(p, 'groups/6011002', 'perdrix');
    verifier(`${dossier} : le groupe en lecture seule (${bbt ? 'les clés de Better BibTeX' : 'les clés de Zotero'})`,
      JSON.stringify(perdrix.map((x) => x.split(' ')[0]).sort()) === JSON.stringify(bbt ? ['broSurvieHivernalePerdrix2019', 'reitzSuiviPopulationsPerdrix2012'] : ['bro_survie_2019', 'reitz_suivi_2012'])
      && perdrix.every((x) => / groups\/6011002\/items\/[A-Z0-9]{8}$/.test(x)), JSON.stringify(perdrix));
    const colls = JSON.parse((await demanderZotero('/api/groups/6011001/collections?format=json&limit=1000', { port: p })).texte);
    const flamants = colls.find((c) => c.data.name === 'Flamants');
    const repro = colls.find((c) => c.data.name === 'Reproduction');
    const dans = await demanderZotero(`/api/groups/6011001/collections/${flamants.key}/items/top?${SANS}&format=json&include=bibtex&limit=300`, { port: p });
    verifier(`${dossier} : les collections d’un groupe, une sous-collection, les références de l’une`, repro.data.parentCollection === flamants.key
      && JSON.stringify(lireReponseZotero(dans.texte).map((e) => e.titre).sort()) === JSON.stringify(['Population dynamics of greater flamingos in Mediterranean wetlands', 'The greater flamingo']),
    dans.texte.slice(0, 200));
    const inconnu = await demanderZotero(recherche('groups/999', 'x'), { port: p });
    verifier(`${dossier} : un groupe inconnu, 404`, inconnu.statut === 404 && lireReponseZotero(inconnu.texte).length === 0, JSON.stringify(inconnu));
    if (bbt) {
      // the picker: each reference with its item's URI and Zotero's libraryID (2, 3: not the group's
      // id), its export asked by that libraryID; a dialog closed without a pick gives nothing
      const choix = JSON.parse(z.lire('cayw_camargue.json').texte);
      verifier('Better BibTeX : son sélecteur dans un groupe (clé, URI de l’élément, libraryID)', JSON.stringify(choix.map((c) => [c.citationKey, c.uri.replace(/^.*\/groups\//, 'groups/'), c.item.libraryID]))
        === JSON.stringify([['bechetPopulationDynamicsGreater2019', 'groups/6011001/items/79PWGJ9R', 2], ['johnsonGreaterFlamingo2007', 'groups/6011001/items/BXYBNU2K', 2]]), JSON.stringify(choix).slice(0, 300));
      const parLib = async (b, cles) => lireCslJson((await demanderZotero(`/better-bibtex/export/item?citationKeys=${encodeURIComponent(cles)}&translator=${encodeURIComponent('Better CSL JSON')}&libraryID=${b}`, { port: p })).texte).entrees;
      const deux = await parLib(2, 'bechetPopulationDynamicsGreater2019,johnsonGreaterFlamingo2007');
      const un = await parLib(1, 'bechetPopulationDynamicsGreater2019');
      verifier('Better BibTeX : la même clé exportée de deux bibliothèques, deux œuvres', deux.length === 2 && un.length === 1
        && deux[0].titre !== un[0].titre && deux[0].cle === un[0].cle, JSON.stringify([deux.map((e) => e.titre), un.map((e) => e.titre)]));
      const perdrixChoisie = JSON.parse(z.lire('cayw_perdrix.json').texte)[0];
      verifier('Better BibTeX : dans le groupe en lecture seule, sa clé et libraryID 3', perdrixChoisie.citationKey === 'broSurvieHivernalePerdrix2019' && perdrixChoisie.item.libraryID === 3
        && (await parLib(3, 'broSurvieHivernalePerdrix2019')).length === 1, JSON.stringify(perdrixChoisie).slice(0, 200));
      const inconnue = await demanderZotero(`/better-bibtex/export/item?citationKeys=x&translator=${encodeURIComponent('Better CSL JSON')}&libraryID=9`, { port: p });
      verifier('Better BibTeX : une bibliothèque inconnue, 400 ; le dialogue fermé sans choix, rien', inconnue.statut === 400 && z.lire('cayw_annule.txt').texte === ''
        && z.index['cayw_annule.txt'].statut === 200, JSON.stringify(inconnue));
    }
    z.s.close();
  }
  const ferme = await rejouer('simple', { fermee: true });
  const r403 = await Promise.all(['/api/users/0/groups?format=json', '/api/users/0/items/top?limit=1&format=keys'].map((c) => demanderZotero(c, { port: ferme.port })));
  verifier('l’API locale fermée : 403 partout', r403.every((x) => x.statut === 403 && x.texte === 'Local API is not enabled'), JSON.stringify(r403));
  ferme.s.close();
  // the addresses documents.js asks, for My Library and a group, pass; others don't
  const demandes = ['/api/users/0/groups?format=json', recherche('groups/6011001', 'bechet'), `/api/groups/6011001/collections/IYEWRT45/items/top?${SANS}&format=json&include=bibtex&limit=300`,
    '/api/groups/6011001/collections?format=json&limit=1000', recherche('users/0', 'wold')];
  const refusees = ['/api/groups/abc/items?q=x', '/api/groups/6011001/items/79PWGJ9R?format=json', '/api/groups/6011001?format=json', '/api/users/0/groups/6011001?format=json',
    '/api/groups/6011001/items/79PWGJ9R/children?format=json', '/api/groups/6011001/collections/IYEWRT45?format=json', '/api/groups/6011001/searches?format=json',
    '/api/groups//items/top?q=x', '/api/groups/6011001/items/top/../../../users/1/items?q=x'];
  const { PERMIS } = require('../zotero');
  verifier('les adresses des groupes que Nuée demande passent', demandes.every((c) => PERMIS.test(c)), JSON.stringify(demandes.filter((c) => !PERMIS.test(c))));
  verifier('les autres adresses de groupe sont refusées', refusees.every((c) => !PERMIS.test(c)), JSON.stringify(refusees.filter((c) => PERMIS.test(c))));

  console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
  process.exit(echecs ? 1 : 0);
})();
