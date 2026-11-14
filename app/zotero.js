// Zotero, when it runs on this computer: what documents.js asks of it, through the main process
// (main.js: IPC "zotero"). Zotero answers on 127.0.0.1:23119 and refuses what comes from a browser
// (a "Mozilla/" user agent, an Origin header): Node's own client sends neither.
//   - its local API (Zotero 7 and later: Settings › Advanced › "Allow other applications on this
//     computer to communicate with Zotero"), read only: the library's items (/api/users/0/items...),
//     its collections and theirs (/api/users/0/collections, .../collections/<key>/items/top);
//   - Better BibTeX's citation picker (/better-bibtex/cayw), when that plugin is installed, and its
//     export of the references picked (/better-bibtex/export/item).
// Only these addresses, and only reading: nothing here can change the library.
const http = require('http');

const PERMIS = /^\/(api\/users\/0\/items(\/top)?\?[^#\s]*|api\/users\/0\/collections(\/[A-Z0-9]{8}\/items\/top)?\?[^#\s]*|better-bibtex\/(cayw|export\/item)\?[^#\s]*|connector\/ping)$/;

// GET `chemin` from Zotero: { statut, texte } or { erreur: 'absent' (Zotero isn't running),
// 'delai', 'demande' (an address not allowed), or the system's message }.
function demanderZotero(chemin, { port = 23119, delai = 8000 } = {}) {
  return new Promise((ok) => {
    if (!PERMIS.test(String(chemin))) { ok({ erreur: 'demande' }); return; }
    let fini = false;
    const finir = (r) => { if (!fini) { fini = true; ok(r); } };
    const req = http.get({ host: '127.0.0.1', port, path: chemin, headers: { 'Zotero-API-Version': '3', Accept: '*/*' } }, (res) => {
      let corps = '';
      res.setEncoding('utf8');
      res.on('data', (d) => {
        corps += d;
        if (corps.length > 30e6) { req.destroy(); finir({ erreur: 'trop' }); }
      });
      res.on('end', () => finir({ statut: res.statusCode, texte: corps }));
      res.on('error', (e) => finir({ erreur: e.message }));
    });
    req.setTimeout(delai, () => { req.destroy(); finir({ erreur: 'delai' }); });
    req.on('error', (e) => finir({ erreur: e.code === 'ECONNREFUSED' ? 'absent' : e.message }));
  });
}

module.exports = { demanderZotero, PERMIS };
