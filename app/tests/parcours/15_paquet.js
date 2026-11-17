// The reproducibility package: the example project (Help › Example projects › Apples, its PCA M1
// fitted; the package fits the others), a document that cites M1's Q2 and a reference kept in it
// (BibTeX); File › Export a reproducibility package…, its dialog filled in, the .zip written where
// the system's dialog says and read back: the project (format 3, its data from data/orchards.csv), the
// R script with the licence notice, the document as a reproducible Quarto file with its .bib, every
// file's SHA-256. Then File › Deposit on Zenodo… against a stand-in for Zenodo's deposit API on
// 127.0.0.1 (the main process's NUEE_ZENODO; nothing reaches zenodo.org): the token pasted once and
// kept encrypted in the profile's jetons.json, the draft created with its DOI reserved, the package
// written with that DOI, then deposited in it with its description; never published.
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const path = require('path');

const JETON = 'zx9TOKENfake0123456789abcdefABCDEF0123456789abcdefABCDEF0123';
const DOI = '10.5072/zenodo.4242';

// Zenodo's deposit API as essai_zenodo.js stands in for it: a DOI reserved only when the draft's
// creation asks for one; every request recorded, the file's bytes kept
function zenodo() {
  const s = { recues: [], fichiers: new Map(), creation: null, metadonnees: null };
  s.serveur = http.createServer((req, res) => {
    const morceaux = [];
    req.on('data', (d) => morceaux.push(d));
    req.on('end', () => {
      const corps = Buffer.concat(morceaux);
      s.recues.push({ method: req.method, url: req.url, auth: req.headers.authorization || '' });
      const json = (statut, x) => { res.writeHead(statut, { 'Content-Type': 'application/json' }); res.end(JSON.stringify(x)); };
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

// every file of the package's folder has the SHA-256 its manifest gives
const manifesteJuste = (zip, dossier) => {
  const lignes = zip.get(`${dossier}/MANIFEST.sha256`).toString('utf8').trim().split('\n');
  const faux = lignes.filter((l) => { const f = zip.get(`${dossier}/${l.slice(66)}`); return !f || crypto.createHash('sha256').update(f).digest('hex') !== l.slice(0, 64); });
  return lignes.length === [...zip.keys()].filter((k) => !k.endsWith('/')).length - 1 && !faux.length;
};

module.exports = {
  async executer(n, verifier, { dossier }) {
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
      await n.ecrire("__n.boite().querySelector('.zenodo-champ')", JETON);
      await n.bouton('zenodo_creer');
      const fait = await n.attendre("__n.titreDialogue() === __n.t('zenodo_fait_titre') && __n.norm(__n.boite().textContent)", 300000);
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
      // the token in the profile, encrypted, under zenodo.org: the store of the versions and the sign-ins
      const jetons = path.join(path.dirname(dossier), 'profil', 'jetons.json');
      const texte = fs.existsSync(jetons) ? fs.readFileSync(jetons, 'utf8') : '';
      verifier('zenodoJetonGarde', !texte.includes(JETON) && (!texte || Object.keys(JSON.parse(texte)).includes('zenodo.org')), texte.slice(0, 80));
      await n.touche('Escape');
    } finally {
      await n.variablePrincipal('NUEE_ZENODO', undefined);
      s.serveur.closeAllConnections();
      await new Promise((ok) => s.serveur.close(ok));
    }
  },
};
