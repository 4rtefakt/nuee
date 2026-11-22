// The reproducibility package: the example project (Help › Example projects › Apples, its PCA M1
// fitted; the package fits the others), a document that cites M1's Q2 and a reference kept in it
// (BibTeX); File › Export a reproducibility package…, its dialog filled in, the .zip written where
// the system's dialog says and read back: the project (format 3, its data from data/orchards.csv), the
// R script with the licence notice, the document as a reproducible Quarto file with its .bib and as
// an HTML page filled in, the main one (the dialog's Shown first: the project's only document),
// which the package's results name for its link (lecture.js) and its README to read first; every
// file's SHA-256. Then File › Deposit on Zenodo… against a stand-in for Zenodo's deposit API and its
// OAuth server on 127.0.0.1 (the main process's NUEE_ZENODO; nothing reaches zenodo.org): Sign in with
// Zenodo, its page "opened in the browser" (the main process's shell.openExternal, pilote.js) and
// fetched here, Zenodo's redirect followed back to Nuée, the token it gives for the code kept
// encrypted in the profile's jetons.json, the draft created with its DOI reserved, the package
// written with that DOI, then deposited in it with its description; never published. Its last dialog
// shows the link that opens the package in the browser version and its QR code, which jsQR reads back
// (as drawn in the window, saved as PNG, and the package's qr-code.svg): the README's link. Then the
// package deposited, File › Open on its .zip, and the package exported first, File › Open a
// reproducibility package…: each opened again as a new project, its files those of its manifest,
// every model fitted again giving its deposited results. With `captures`, a screenshot of the deposit
// dialog offering Sign in with Zenodo, and of its last one with the link and its QR code.
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const path = require('path');
const jsQR = require('jsqr');

const JETON = 'zx9TOKENfake0123456789abcdefABCDEF0123456789abcdefABCDEF0123';
const DOI = '10.5072/zenodo.4242';

// Zenodo's deposit API as essai_zenodo.js stands in for it: a DOI reserved only when the draft's
// creation asks for one; every request recorded, the file's bytes kept. And its OAuth server, the
// user signed in there: /oauth/authorize authorizes Nuée at once, the browser sent back with a code;
// /oauth/token gives JETON for it (its requests in `oauth`).
function zenodo() {
  const s = { recues: [], oauth: [], codes: new Map(), fichiers: new Map(), creation: null, metadonnees: null };
  s.serveur = http.createServer((req, res) => {
    const morceaux = [];
    req.on('data', (d) => morceaux.push(d));
    req.on('end', () => {
      const corps = Buffer.concat(morceaux);
      const json = (statut, x) => { res.writeHead(statut, { 'Content-Type': 'application/json' }); res.end(JSON.stringify(x)); };
      const u = new URL(req.url, 'http://x');
      if (u.pathname.startsWith('/oauth/')) {
        const champs = Object.fromEntries(new URLSearchParams(corps.toString('utf8')));
        const q = Object.fromEntries(u.searchParams);
        s.oauth.push({ method: req.method, chemin: u.pathname, q, champs });
        if (req.method === 'GET' && u.pathname === '/oauth/authorize') {
          const code = crypto.randomBytes(12).toString('hex');
          s.codes.set(code, q);
          res.writeHead(302, { Location: `${q.redirect_uri}?code=${code}&state=${encodeURIComponent(q.state)}` });
          res.end();
        } else if (req.method === 'POST' && u.pathname === '/oauth/token' && s.codes.has(champs.code)
          && champs.client_id === s.codes.get(champs.code).client_id && champs.redirect_uri === s.codes.get(champs.code).redirect_uri) {
          json(200, { access_token: JETON, token_type: 'Bearer', expires_in: 5184000, scope: s.codes.get(champs.code).scope, user: { id: '4242' } });
        } else {
          json(400, { error: 'invalid_grant' });
        }
        return;
      }
      s.recues.push({ method: req.method, url: req.url, auth: req.headers.authorization || '' });
      if (req.headers.authorization !== `Bearer ${JETON}`) { json(401, { status: 401, message: 'Unauthorized.' }); return; }
      const base = `http://127.0.0.1:${s.serveur.address().port}`;
      if (req.method === 'POST' && req.url === '/api/deposit/depositions') {
        s.creation = JSON.parse(corps.toString('utf8') || '{}');
        const demande = !!(s.creation.metadata && s.creation.metadata.prereserve_doi === true);
        json(201, { id: 4242, metadata: demande ? { prereserve_doi: { doi: DOI, recid: 4242 } } : {},
          links: { html: `${base}/deposit/4242`, bucket: `${base}/api/files/b0c1d2e3` } });
      } else if (req.method === 'PUT' && req.url.startsWith('/api/files/b0c1d2e3/')) {
        s.fichiers.set(decodeURIComponent(req.url.split('/').pop()), corps);
        json(201, { checksum: `md5:${crypto.createHash('md5').update(corps).digest('hex')}` });
      } else if (req.method === 'PUT' && req.url === '/api/deposit/depositions/4242') {
        s.metadonnees = JSON.parse(corps.toString('utf8')).metadata;
        json(200, { id: 4242, metadata: { ...s.metadonnees, prereserve_doi: { doi: DOI, recid: 4242 } } });
      } else {
        json(404, { status: 404, message: 'Not found.' });
      }
    });
  });
  return new Promise((ok) => s.serveur.listen(0, '127.0.0.1', () => ok(s)));
}

// A QR code drawn in the window (an image's address: the expression `adresse`), its pixels read here
// by jsQR: its text, or null.
async function lireQR(n, adresse) {
  const p = await n.evaluer(`(async () => {
    const img = new Image();
    img.src = ${adresse};
    await img.decode();
    const c = document.createElement('canvas');
    c.width = img.naturalWidth;
    c.height = img.naturalHeight;
    const ctx = c.getContext('2d');
    ctx.fillStyle = '#fff';
    ctx.fillRect(0, 0, c.width, c.height);
    ctx.drawImage(img, 0, 0);
    const d = ctx.getImageData(0, 0, c.width, c.height).data;
    let s = '';
    for (let i = 0; i < d.length; i += 8192) s += String.fromCharCode(...d.subarray(i, i + 8192));
    return { largeur: c.width, hauteur: c.height, pixels: btoa(s) };
  })()`);
  const lu = jsQR(new Uint8ClampedArray(Buffer.from(p.pixels, 'base64')), p.largeur, p.hauteur);
  return lu ? lu.data : null;
}

// The report of the k-th comparison made in the window (changements.js: C<k>-…), a package rerun
// (its section paquet-relance), once compared: its sentence, what it says of the manifest, the
// models it shows, and the sentence of 4 models that all give their deposited results, in the
// window's language.
async function rapportRelance(n, k) {
  return n.attendre(`(async () => {
    const p = document.querySelector('[data-comparaison^="C${k}-"]');
    const c = p && p.querySelector('.changements');
    if (!c || !c.querySelector('.paquet-relance')) return null;
    const { phraseBilan } = await import('./changements.js');
    return { bilan: __n.norm(c.querySelector('.bilan-relance').textContent), manifeste: __n.norm(c.querySelector('.manifeste-relance').textContent),
      intact: !c.querySelector('.manifeste-relance').classList.contains('attention'), modeles: [...c.querySelectorAll('.modele-change h3')].map((h) => __n.norm(h.textContent)),
      attendu: phraseBilan({ relances: 4, identiques: 4, differents: [] }), doi: __n.norm((c.querySelector('.paquet-relance h3 .compte-change') || {}).textContent || '') };
  })()`, 300000);
}

// every file of the package's folder has the SHA-256 its manifest gives
const manifesteJuste = (zip, dossier) => {
  const lignes = zip.get(`${dossier}/MANIFEST.sha256`).toString('utf8').trim().split('\n');
  const faux = lignes.filter((l) => { const f = zip.get(`${dossier}/${l.slice(66)}`); return !f || crypto.createHash('sha256').update(f).digest('hex') !== l.slice(0, 64); });
  return lignes.length === [...zip.keys()].filter((k) => !k.endsWith('/')).length - 1 && !faux.length;
};

module.exports = {
  async executer(n, verifier, { dossier, captures }) {
    const capture = async (nom) => { if (captures) await n.capture(`${captures}-paquet-${nom}.png`); };
    await n.sousMenu('m_aide', 'projet_exemple', 'exemple_pommes');
    await n.attendre(n.ligneModele('M1', true), 120000);
    const pid = (await n.etat()).projets[0].id;

    // ---- a document that cites M1's Q2, and a reference kept in it
    await n.menu('m_fichier', 'nouveau_document');
    await n.dialogue('nouveau_document');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'Package test');
    await n.bouton('creer_document');
    await n.attendre('!__n.dialogue() && !!document.querySelector(".document .cm-content")');
    await n.cliquer("document.querySelector('.document .cm-content')");
    await n.taper('The PCA predicts Q2 = {{M1.Q2}} [@wold2001].');
    await n.touche('Escape');  // (a completion list, if one is open)
    const doc = await n.attendre(`__n.api('projet', { projet: ${JSON.stringify(pid)} }).then((p) => [].concat(p.documents || [])[0] || null)`);
    await n.api('document_enregistrer', { projet: pid, id: doc.id, bib: '@article{wold2001,\n  title = {PLS-regression},\n  journal = {Chemometrics and Intelligent Laboratory Systems},\n  year = {2001}\n}\n' });

    // ---- File › Export a reproducibility package…
    const titrePaquet = "__n.titreDialogue().startsWith(__n.t('paquet_titre').split('{')[0].trim())";
    const zip = path.join(dossier, 'Apples package.zip');
    await n.fichierAEnregistrer(zip);
    await n.menu('m_fichier', 'paquet');
    await n.attendre(titrePaquet);
    await n.ecrire("__n.boite().querySelector('.paquet-titre')", 'Apples');
    await n.ecrire("__n.boite().querySelectorAll('.paquet-auteurs input')[0]", 'Alice');
    await n.ecrire("__n.boite().querySelectorAll('.paquet-auteurs input')[1]", 'Exemple');
    await n.ecrire("__n.boite().querySelector('.dialogue-paquet textarea')", 'Made-up apples, for the workflow test.');
    // (Shown first: the project's only document, then "No document: the whole project")
    const montre = await n.evaluer("(() => { const s = __n.boite().querySelector('.paquet-principal'); return s ? [s.value, [...s.options].map((o) => o.textContent)] : null; })()");
    await n.bouton('choisir_emplacement');
    const ecrit = await n.attendre("__n.statut().startsWith(__n.t('paquet_ecrit').split('{')[0].trim()) && __n.statut()", 300000);
    // (the remarks on what the package couldn't do, if any: the README says them too)
    if (await n.evaluer("__n.titreDialogue().startsWith(__n.t('paquet_remarques_titre'))")) await n.touche('Escape');
    verifier('paquetEcrit', fs.existsSync(zip) && ecrit.includes('Apples package.zip'), ecrit);
    const z = n.lireZip(fs.readFileSync(zip));
    const lire = (f) => (z.has(`Apples package/${f}`) ? z.get(`Apples package/${f}`).toString('utf8') : '');
    const nuee = JSON.parse(lire('orchards.nuee') || '{}');
    verifier('paquetProjetFormat3', nuee.nuee && nuee.nuee.format === 3 && nuee.source_donnees && nuee.source_donnees.relatif === 'data/orchards.csv'
      && nuee.source_donnees.chemin === 'data/orchards.csv' && !(nuee.etapes || []).length && nuee.paquet && nuee.paquet.titre === 'Apples'
      && !lire('orchards.nuee').includes(path.dirname(dossier).replace(/\\/g, '/')) && !lire('orchards.nuee').includes(path.dirname(dossier).replace(/\\/g, '\\\\')),
    { format: nuee.nuee && nuee.nuee.format, source: nuee.source_donnees });
    const script = lire('analysis.R');
    verifier('paquetScript', (script.match(/LICENSE-EXCEPTION\.md/g) || []).length === 1 && (script.match(/please cite Nuée/g) || []).length === 1
      && /^M4 <- list\(|^M2 <- list\(/m.test(script), script.slice(0, 200));
    const qmd = lire('documents/Package test.qmd');
    const bib = lire('documents/Package test.bib');
    verifier('paquetDocument', qmd.includes('Package test.bib') && /```\{r[^}]*\}/.test(qmd) && /`r [^`]+`/.test(qmd) && qmd.includes('@wold2001')
      && bib.includes('@article{wold2001') && z.has('Apples package/documents/Package test_R/nuee.R'), qmd.slice(0, 300));
    const page = lire('documents/Package test.html');
    const cles = JSON.parse(lire('results/key_results.json') || '{}');
    verifier('paquetDocumentPage', montre && montre[0] === doc.id && montre[1].length === 2 && montre[1][0] === 'Package test'
      && page.startsWith('<!doctype html>') && page.includes('<meta name="viewport" content="width=device-width, initial-scale=1">')
      && /Q2 = <span class="ref"[^>]*>0[.,]\d+<\/span>/.test(page) && cles.document_principal === 'documents/Package test.html'
      && lire('README.md').includes('- **Read first:** `documents/Package test.html`, “Package test”, which opens in any browser')
      && nuee.paquet && nuee.paquet.principal === doc.id, { montre, principal: cles.document_principal, page: page.slice(0, 160) });
    verifier('paquetManifeste', manifesteJuste(z, 'Apples package'));
    verifier('paquetReadme', lire('README.md').startsWith('# Apples') && !lire('README.md').includes('nuee.io/app'), lire('README.md').slice(0, 120));

    // ---- File › Deposit on Zenodo…, against the stand-in
    const s = await zenodo();
    const base = `http://127.0.0.1:${s.serveur.address().port}`;
    await n.variablePrincipal('NUEE_ZENODO', base);
    try {
      await n.menu('m_fichier', 'paquet_zenodo');
      await n.attendre(titrePaquet);
      await n.bouton('paquet_zenodo');
      await n.dialogue('zenodo_titre');
      // Sign in with Zenodo (a token still possible, under "Or use a personal access token"): Zenodo's
      // page opened in the browser, here a fetch following its redirect back to Nuée
      // (the dialog draws them once it knows the sign-in state: a moment after its title, on macOS)
      const offert = await n.attendre("!!__n.boite().querySelector('button.zenodo-connexion') && !!__n.boite().querySelector('details.zenodo-par-jeton .zenodo-champ')", 10000)
        .then(() => true, () => false);
      await capture('1-connexion');
      await n.cliquer("__n.boite().querySelector('button.zenodo-connexion')");
      await n.attendre("!!__n.boite().querySelector('.zenodo-attente')");
      const page = (await n.principal()).liens.filter((l) => l.startsWith(`${base}/oauth/authorize?`)).pop() || '';
      const q = page ? Object.fromEntries(new URL(page).searchParams) : {};
      const navigateur = page ? await fetch(page) : null;
      const vu = navigateur ? await navigateur.text() : '';
      await n.attendre(`__n.norm(__n.boite().textContent).includes(__n.norm(__n.t('zenodo_jeton_garde').replace('{hote}', 'zenodo.org')))`);
      const echange = s.oauth.find((x) => x.chemin === '/oauth/token') || { champs: {} };
      verifier('zenodoConnexion', offert && q.response_type === 'code' && q.client_id === 'nuee-essai' && q.scope === 'deposit:write'
        && /^http:\/\/127\.0\.0\.1:4786[45]\/zenodo$/.test(q.redirect_uri || '') && /^[0-9a-f]{48}$/.test(q.state || '') && navigateur && navigateur.status === 200
        && vu.includes('Nuée is signed in to Zenodo') && echange.champs.grant_type === 'authorization_code' && echange.champs.client_id === 'nuee-essai'
        && !Object.keys(echange.champs).some((k) => /secret/.test(k)), { offert, q, statut: navigateur && navigateur.status, vu: vu.slice(0, 200), echange });
      await n.bouton('zenodo_creer');
      const fait = await n.attendre("__n.titreDialogue() === __n.t('zenodo_fait_titre') && __n.norm(__n.boite().textContent)", 300000);
      await n.attendre("(() => { const i = __n.boite().querySelector('img.qr-lien'); return !!i && i.complete && i.naturalWidth > 0; })()");
      await capture('2-qr');
      const ordre = s.recues.map((x) => `${x.method} ${x.url.startsWith('/api/files/b0c1d2e3/') ? '/api/files/b0c1d2e3/…' : x.url}`).join(' | ');
      verifier('zenodoOrdre', ordre === 'POST /api/deposit/depositions | PUT /api/files/b0c1d2e3/… | PUT /api/deposit/depositions/4242'
        && s.creation && s.creation.metadata && s.creation.metadata.prereserve_doi === true && s.recues.every((x) => x.auth === `Bearer ${JETON}`), ordre);
      const depose = [...s.fichiers.values()][0];
      const zz = depose ? n.lireZip(depose) : new Map();
      const readme = [...zz.entries()].filter(([k]) => k.endsWith('/README.md')).map(([, v]) => v.toString('utf8'))[0] || '';
      verifier('zenodoPaquetAvecSonDoi', readme.includes(`https://nuee.io/app/?doi=${DOI}`) && readme.includes(`[${DOI}](https://doi.org/${DOI})`)
        && [...zz.keys()].some((k) => k.endsWith('/CITATION.cff') && zz.get(k).toString('utf8').includes(`\ndoi: "${DOI}"\n`)), readme.slice(0, 300));
      verifier('zenodoDescription', s.metadonnees && s.metadonnees.title === 'Apples' && s.metadonnees.prereserve_doi === true
        && s.metadonnees.creators[0].name === 'Exemple, Alice', s.metadonnees);
      verifier('zenodoFait', fait.includes(DOI) && fait.includes(`${base}/deposit/4242`), fait);
      // the link and its QR code: drawn in the dialog, saved as PNG (Save the QR code…), and in the
      // package (qr-code.svg), each read back by jsQR as the README's link
      const lien = `https://nuee.io/app/?doi=${DOI}`;
      const qrDialogue = await lireQR(n, "__n.boite().querySelector('img.qr-lien').src");
      const png = path.join(dossier, 'Apples QR.png');
      await n.fichierAEnregistrer(png);
      await n.cliquer("__n.boite().querySelector('button.enregistrer-qr')");
      await n.attendre(`__n.statut().startsWith(__n.t('zenodo_qr_enregistre').split('{')[0].trim())`);
      const qrPng = fs.existsSync(png) ? await lireQR(n, JSON.stringify(`data:image/png;base64,${fs.readFileSync(png).toString('base64')}`)) : null;
      const svg = [...zz.entries()].filter(([k]) => k.endsWith('/qr-code.svg')).map(([, v]) => v.toString('utf8'))[0] || '';
      const qrPaquet = svg ? await lireQR(n, JSON.stringify(`data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg)}`)) : null;
      verifier('zenodoLienQR', fait.includes(lien) && readme.includes(`[${lien}](${lien})`) && qrDialogue === lien && qrPng === lien && qrPaquet === lien
        && fs.readFileSync(png).subarray(1, 4).toString() === 'PNG', { qrDialogue, qrPng, qrPaquet });
      fs.writeFileSync(path.join(dossier, 'Apples deposited.zip'), depose || Buffer.alloc(0));
      // the token in the profile, encrypted, under zenodo.org: the store of the versions and the sign-ins
      const jetons = path.join(path.dirname(dossier), 'profil', 'jetons.json');
      const texte = fs.existsSync(jetons) ? fs.readFileSync(jetons, 'utf8') : '';
      verifier('zenodoJetonGarde', !texte.includes(JETON) && (!texte || Object.keys(JSON.parse(texte)).includes('zenodo.org')), texte.slice(0, 80));
      await n.touche('Escape');
    } finally {
      // (an address that answers nothing, rather than none: a deposit still under way after a
      // failure must not go on to zenodo.org)
      await n.variablePrincipal('NUEE_ZENODO', 'http://127.0.0.1:9');
      s.serveur.closeAllConnections();
      await new Promise((ok) => s.serveur.close(ok));
    }

    // ---- the package deposited, File › Open on its .zip; the one exported first, File › Open a
    // reproducibility package…: each opened again as a new project, its files those of its manifest,
    // every model fitted again giving its deposited results
    const avant = (await n.etat()).projets.length;
    const ouvrirPaquet = async (fichier, commande, k) => {
      await n.fichierAOuvrir(fichier);
      await n.menu('m_fichier', commande);
      const r = await rapportRelance(n, k);
      const p = (await n.etat()).projets;
      return { ...r, projets: p.map((x) => x.nom), nouveau: p.length === avant + k };
    };
    const rouvert = await ouvrirPaquet(path.join(dossier, 'Apples deposited.zip'), 'ouvrir', 1);
    verifier('paquetRouvertOuvrir', rouvert.bilan === rouvert.attendu && rouvert.intact && rouvert.doi === `DOI ${DOI}` && rouvert.nouveau
      && rouvert.projets.includes('Apples.nuee') && ['M1', 'M2', 'M3', 'M4'].every((m) => rouvert.modeles.some((h) => h.startsWith(m))), rouvert);
    const exporte = await ouvrirPaquet(zip, 'paquet_ouvrir', 2);
    verifier('paquetRouvertCommande', exporte.bilan === exporte.attendu && exporte.intact && !exporte.doi && exporte.nouveau && exporte.projets.includes('Apples package.nuee'), exporte);
    // (nothing written into the package: the .zip as it was written)
    verifier('paquetIntact', n.lireZip(fs.readFileSync(zip)).size === z.size && manifesteJuste(n.lireZip(fs.readFileSync(zip)), 'Apples package'));

    // ---- last-digit gaps: a copy of the package exported first, M2's deposited Q2 moved by 1e-9
    // relative (its manifest made again for it), opened again: M2 gives the deposited results to 6
    // significant digits, its largest relative gap said, the other three identical
    const { zip: ecrireZip } = await import(require('url').pathToFileURL(path.join(__dirname, '..', '..', 'interface', 'zip.js')).href);
    const fichiers = new Map(n.lireZip(fs.readFileSync(zip)));
    const cleResultats = 'Apples package/results/key_results.json';
    const deposes = JSON.parse(fichiers.get(cleResultats).toString('utf8'));
    const q2 = deposes.modeles.find((m) => m.nom === 'M2').entrees.find((e) => e.cle === 'Q2');
    q2.valeur *= 1 + 1e-9;
    fichiers.set(cleResultats, Buffer.from(JSON.stringify(deposes, null, 2)));
    const hache = (o) => crypto.createHash('sha256').update(o).digest('hex');
    fichiers.set('Apples package/MANIFEST.sha256', Buffer.from(fichiers.get('Apples package/MANIFEST.sha256').toString('utf8')
      .replace(/^[0-9a-f]{64}(  results\/key_results\.json)$/m, (_, reste) => hache(fichiers.get(cleResultats)) + reste)));
    const ecarts = path.join(dossier, 'Apples gaps.zip');
    fs.writeFileSync(ecarts, ecrireZip([...fichiers.entries()].map(([k, v]) => [k, new Uint8Array(v)])));
    const r3 = await ouvrirPaquet(ecarts, 'paquet_ouvrir', 3);
    const m2 = await n.evaluer(`(async () => {
      const c = document.querySelector('[data-comparaison^="C3-"] .changements');
      const s = [...c.querySelectorAll('.modele-change.six-chiffres')];
      const { phraseBilan } = await import('./changements.js');
      return { six: s.map((x) => __n.norm(x.querySelector('h3').textContent)), texte: s.map((x) => __n.norm(x.querySelector('.verdict-six').textContent)).join(' | '),
        attendu: phraseBilan({ relances: 4, identiques: 3, sixChiffres: 1, differents: [] }) };
    })()`);
    verifier('paquetSixChiffres', r3.intact && r3.bilan === m2.attendu && m2.six.length === 1 && m2.six[0].startsWith('M2') && /1[.,]0e-9/.test(m2.texte), { bilan: r3.bilan, ...m2 });
  },
};
