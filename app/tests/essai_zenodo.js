// Checks zenodo.js (a reproducibility package deposited on Zenodo as a draft) against a stand-in for
// Zenodo's deposit API on 127.0.0.1, which answers as developers.zenodo.org describes it: the draft
// created with its DOI reserved (prereserve_doi), then, once the package is written with that DOI,
// the file put in its bucket (with its MD5) and the description checked, the DOI still reserved; a
// reservation refused, and the other ways it goes wrong. Then creerZenodo, what main.js gives the
// interface: the token of each host kept in the store of the versions and the sign-ins
// (versions.js: magasinJetons, with a stand-in for Electron's safeStorage), never given back, the
// drafts in that order, and the deposit of a .zip of the engine's session folder only. Nothing here
// reaches zenodo.org or sandbox.zenodo.org.
//   node tests/essai_zenodo.js
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const os = require('os');
const path = require('path');
const { creerZenodo, creerBrouillon, remplirBrouillon, metadonneesZenodo, DOI_NUEE } = require('../zenodo');
const { magasinJetons } = require('../versions');

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const JETON = 'zx9TOKENfake0123456789abcdefABCDEF0123456789abcdefABCDEF0123';
const DOI = '10.5072/zenodo.4242';  // (the prefix of sandbox.zenodo.org's DOIs)
const dossier = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-zenodo-'));

// The stand-in: every request recorded (method, path, Authorization, body); `mode` makes it answer
// otherwise (no DOI reserved, a bucket elsewhere, another MD5, a description refused, another DOI
// at the end). A draft gets a DOI only when its creation asks for one, as Zenodo does.
function zenodo() {
  const s = { recues: [], fichiers: new Map(), creation: null, metadonnees: null, mode: '', autre: null };
  s.serveur = http.createServer((req, res) => {
    const morceaux = [];
    req.on('data', (d) => morceaux.push(d));
    req.on('end', () => {
      const corps = Buffer.concat(morceaux);
      s.recues.push({ method: req.method, url: req.url, auth: req.headers.authorization || '', type: req.headers['content-type'] || '', taille: corps.length });
      const json = (statut, x) => { res.writeHead(statut, { 'Content-Type': 'application/json' }); res.end(JSON.stringify(x)); };
      if (req.headers.authorization !== `Bearer ${JETON}`) {
        json(401, { status: 401, message: 'The server could not verify that you are authorized to access the URL requested.' });
        return;
      }
      const base = `http://127.0.0.1:${s.serveur.address().port}`;
      const reserve = { doi: DOI, recid: 4242 };
      if (req.method === 'POST' && req.url === '/api/deposit/depositions') {
        s.creation = JSON.parse(corps.toString('utf8') || '{}');
        const demande = !!(s.creation.metadata && s.creation.metadata.prereserve_doi === true);
        json(201, { id: 4242, state: 'unsubmitted', submitted: false, metadata: demande && s.mode !== 'sans_doi' ? { prereserve_doi: reserve } : {},
          links: { self: `${base}/api/deposit/depositions/4242`, html: `${base}/deposit/4242`, publish: `${base}/api/deposit/depositions/4242/actions/publish`,
            bucket: s.mode === 'ailleurs' ? `${s.autre}/api/files/b0c1d2e3` : `${base}/api/files/b0c1d2e3-f4a5-4b6c-8d7e-9f0a1b2c3d4e` } });
      } else if (req.method === 'PUT' && req.url.startsWith('/api/files/b0c1d2e3-f4a5-4b6c-8d7e-9f0a1b2c3d4e/')) {
        const nom = decodeURIComponent(req.url.split('/').pop());
        s.fichiers.set(nom, corps);
        const md5 = s.mode === 'empreinte' ? '0'.repeat(32) : crypto.createHash('md5').update(corps).digest('hex');
        json(201, { key: nom, size: corps.length, mimetype: 'application/zip', checksum: `md5:${md5}` });
      } else if (req.method === 'PUT' && req.url === '/api/deposit/depositions/4242') {
        const m = JSON.parse(corps.toString('utf8')).metadata;
        if (s.mode === 'refus' || !m.description || !m.creators || !m.creators.length) {
          json(400, { status: 400, message: 'Validation error.', errors: [{ field: 'metadata.creators', messages: ['Missing data for required field.'] }] });
          return;
        }
        s.metadonnees = m;
        const garde = s.mode === 'autre_doi' ? { doi: '10.5072/zenodo.4343', recid: 4343 } : reserve;
        json(200, { id: 4242, metadata: { ...m, prereserve_doi: m.prereserve_doi === true ? garde : undefined }, state: 'unsubmitted', submitted: false });
      } else {
        json(404, { status: 404, message: 'The requested URL was not found on the server.' });
      }
    });
  });
  return new Promise((ok) => s.serveur.listen(0, '127.0.0.1', () => ok(s)));
}

// The package as the engine writes it once the DOI is known (moteur/R/paquet.R): here, bytes with
// the DOI among them, in a .zip of `ici`
const paquetAvec = (ici, doi) => {
  const f = path.join(ici, 'Apples package.zip');
  fs.writeFileSync(f, Buffer.concat([crypto.randomBytes(100000), Buffer.from(`https://nuee.io/app/?doi=${doi}`), crypto.randomBytes(100000)]));
  return f;
};
const requetes = (s) => s.recues.map((x) => `${x.method} ${x.url}`).join(' | ');

(async () => {
  const META = { titre: 'Apples in <orchards> & co', description: 'Made-up data: 120 apples.\n\nA second paragraph,\non two lines.',
    auteurs: [{ prenom: 'Alice', nom: 'Exemple', orcid: '0000-0002-1825-0097' }, { prenom: '', nom: 'Tour du Valat', orcid: '' }],
    mots_cles: ['apples', 'PCA'], licence: 'CC0-1.0' };
  const m = metadonneesZenodo(META, { langue: 'fr' });
  verifier('the description: title, creators (Family, Given; ORCID), HTML paragraphs escaped, keywords, licence, dataset, open, its DOI still reserved',
    m.title === META.titre && m.creators.length === 2 && m.creators[0].name === 'Exemple, Alice' && m.creators[0].orcid === '0000-0002-1825-0097'
      && m.creators[1].name === 'Tour du Valat' && !('orcid' in m.creators[1])
      && m.description === '<p>Made-up data: 120 apples.</p><p>A second paragraph,<br>on two lines.</p>'
      && m.keywords.join() === 'apples,PCA' && m.license === 'cc0-1.0' && m.upload_type === 'dataset' && m.access_right === 'open' && m.language === 'fra'
      && m.prereserve_doi === true,
    JSON.stringify(m));
  verifier('Nuée as the software that made it, by its DOI', m.related_identifiers.length === 1 && m.related_identifiers[0].identifier === DOI_NUEE
    && m.related_identifiers[0].relation === 'isCompiledBy' && m.related_identifiers[0].resource_type === 'software' && DOI_NUEE === '10.5281/zenodo.24263014');
  verifier('the licences by their Zenodo ids, CC BY 4.0 when none is given; no keywords when there are none',
    ['CC-BY-4.0', 'CC-BY-SA-4.0', 'CC-BY-NC-4.0', undefined].map((l) => metadonneesZenodo({ ...META, licence: l }).license).join() === 'cc-by-4.0,cc-by-sa-4.0,cc-by-nc-4.0,cc-by-4.0'
      && !('keywords' in metadonneesZenodo({ ...META, mots_cles: [] })) && metadonneesZenodo(META).language === 'eng');

  const s = await zenodo();
  const base = `http://127.0.0.1:${s.serveur.address().port}`;

  // 1. the draft, its DOI reserved: one request, before the package exists
  const b = await creerBrouillon({ base, jeton: JETON });
  verifier('first, the draft created with its DOI reserved (prereserve_doi), nothing else sent yet', b.id === 4242 && b.doi === DOI && b.page === `${base}/deposit/4242`
    && b.bucket === `${base}/api/files/b0c1d2e3-f4a5-4b6c-8d7e-9f0a1b2c3d4e` && JSON.stringify(s.creation) === '{"metadata":{"prereserve_doi":true}}'
    && requetes(s) === 'POST /api/deposit/depositions', `${JSON.stringify(b)} ${requetes(s)}`);
  // 2. the package written with that DOI; 3. in the draft's bucket, then its description
  const fichier = paquetAvec(dossier, b.doi);
  const octets = fs.readFileSync(fichier);
  const r = await remplirBrouillon({ base, jeton: JETON, brouillon: b, fichier, metadonnees: m });
  verifier('then the package written with that DOI in the draft’s bucket (the same bytes, its MD5 checked), then its description, the DOI kept',
    r.id === 4242 && r.doi === DOI && r.fichier === 'Apples package.zip' && r.taille === octets.length
      && requetes(s) === 'POST /api/deposit/depositions | PUT /api/files/b0c1d2e3-f4a5-4b6c-8d7e-9f0a1b2c3d4e/Apples%20package.zip | PUT /api/deposit/depositions/4242'
      && Buffer.compare(s.fichiers.get('Apples package.zip'), octets) === 0 && s.fichiers.get('Apples package.zip').includes(`?doi=${DOI}`)
      && JSON.stringify(s.metadonnees) === JSON.stringify(m), requetes(s));
  verifier('its page, on Zenodo itself, is given back', r.page === `${base}/deposit/4242`, r.page);
  verifier('never published: no request to the publish action', !s.recues.some((x) => /publish/.test(x.url)));
  verifier('the token only in the Authorization header, never in an address', s.recues.every((x) => x.auth === `Bearer ${JETON}` && !x.url.includes(JETON)));
  verifier('the reservation and the description sent as JSON, the file as a file', s.recues[0].type === 'application/json' && s.recues[1].type === 'application/octet-stream'
    && s.recues[2].type === 'application/json');

  // a reservation refused: the draft comes without a DOI; its page given (to delete it), nothing more sent
  s.recues.length = 0;
  s.mode = 'sans_doi';
  let e = await creerBrouillon({ base, jeton: JETON }).catch((x) => x);
  verifier('a DOI not reserved: code doi, the draft’s page given, and no package nor description sent', e.code === 'doi' && e.page === `${base}/deposit/4242`
    && requetes(s) === 'POST /api/deposit/depositions', `${e.code} ${e.message} ${requetes(s)}`);
  // the description given back with another DOI than the package names
  s.mode = 'autre_doi';
  e = await remplirBrouillon({ base, jeton: JETON, brouillon: b, fichier, metadonnees: m }).catch((x) => x);
  verifier('the draft given another DOI than the package’s at the end: code doi, its page', e.code === 'doi' && e.page === b.page && /10\.5072\/zenodo\.4343/.test(e.message),
    `${e.code} ${e.message}`);

  // a token Zenodo refuses: no draft, the error says so, without the token
  s.mode = '';
  s.recues.length = 0;
  e = await creerBrouillon({ base, jeton: 'faux-jeton-qui-ne-marche-pas' }).catch((x) => x);
  verifier('a token refused (401): code jeton, no draft, nothing sent after', e.code === 'jeton' && e.statut === 401 && !e.page && s.recues.length === 1 && !String(e.message).includes('faux-jeton'), `${e.code} ${e.message}`);

  // a description refused: the draft exists, and its page is given to finish it there
  s.mode = 'refus';
  e = await remplirBrouillon({ base, jeton: JETON, brouillon: b, fichier, metadonnees: m }).catch((x) => x);
  verifier('a description refused (400): code refus, Zenodo’s words, the draft’s page to finish it', e.code === 'refus' && /Validation error/.test(e.message)
    && /metadata\.creators: Missing data/.test(e.message) && e.page === `${base}/deposit/4242`, `${e.code} ${e.message} ${e.page}`);

  // the file received isn't the one sent
  s.mode = 'empreinte';
  s.recues.length = 0;
  e = await remplirBrouillon({ base, jeton: JETON, brouillon: b, fichier, metadonnees: m }).catch((x) => x);
  verifier('another MD5 than the file’s: code empreinte, and its description isn’t sent', e.code === 'empreinte' && e.page === b.page
    && requetes(s) === 'PUT /api/files/b0c1d2e3-f4a5-4b6c-8d7e-9f0a1b2c3d4e/Apples%20package.zip', `${e.code} ${e.message} ${requetes(s)}`);

  // an address elsewhere in Zenodo's answer: refused, the token never goes there, and before any package is written
  const autre = await zenodo();
  s.autre = `http://127.0.0.1:${autre.serveur.address().port}`;
  s.mode = 'ailleurs';
  e = await creerBrouillon({ base, jeton: JETON }).catch((x) => x);
  verifier('a bucket on another server: refused (code adresse) once the draft is created, that server never reached', e.code === 'adresse' && e.page && autre.recues.length === 0,
    `${e.code} ${e.message} ${autre.recues.length}`);

  // nothing answers
  const ferme = await zenodo();
  const portFerme = ferme.serveur.address().port;
  await new Promise((ok) => ferme.serveur.close(ok));
  e = await creerBrouillon({ base: `http://127.0.0.1:${portFerme}`, jeton: JETON }).catch((x) => x);
  verifier('Zenodo out of reach: code reseau', e.code === 'reseau', `${e.code} ${e.message}`);

  // ---- creerZenodo: the token of each host kept in the store of the versions and the sign-ins
  // (versions.js: magasinJetons, the same file as their github.com's), with a stand-in for Electron's
  // safeStorage; never given back to the interface
  const coffre = (disponible = true) => ({
    isAsyncEncryptionAvailable: async () => disponible,
    getSelectedStorageBackend: () => 'gnome_libsecret',
    // (a stand-in for the system's encryption: reversible, and nothing like the token)
    encryptStringAsync: async (x) => Buffer.from(`chiffre:${Buffer.from(x, 'utf8').toString('hex').split('').reverse().join('')}`, 'utf8'),
    decryptStringAsync: async (c) => {
      const x = c.toString('utf8');
      if (!x.startsWith('chiffre:')) throw new Error('not ours');
      return { shouldReEncrypt: false, result: Buffer.from(x.slice(8).split('').reverse().join(''), 'hex').toString('utf8') };
    },
  });
  const f = path.join(dossier, 'jetons.json');
  const magasin = (disponible = true, fichierJetons = f) => magasinJetons({ fichier: () => fichierJetons, safeStorage: coffre(disponible) });
  const GITHUB = 'ghp_jetonDeGitHubPourLesVersions0123456789';
  await magasin().ecrire('github.com', GITHUB);
  const session = path.join(dossier, 'seance');
  fs.mkdirSync(session);
  const ouverts = [];
  const z = creerZenodo({ jetons: magasin(), base: () => base, dossier: () => session, ouvrir: (page) => ouverts.push(page) });
  verifier('no token kept yet, and one kept would outlive the session', JSON.stringify(await z.etat('zenodo.org')) === '{"jeton":false,"persistant":true}');
  const garde = await z.jeton('zenodo.org', `  ${JETON}\n`);
  const texte = fs.readFileSync(f, 'utf8');
  verifier('kept under zenodo.org, encrypted, in the same file as github.com’s, which stays', garde.persistant === true && !texte.includes(JETON)
    && Object.keys(JSON.parse(texte)).sort().join() === 'github.com,zenodo.org' && await magasin().lire('github.com') === GITHUB, texte);
  const etat = await z.etat('zenodo.org');
  verifier('the interface learns that a token is kept, never the token; the next session reads it; one host at a time',
    JSON.stringify(etat) === '{"jeton":true,"persistant":true}' && await magasin().lire('zenodo.org') === JETON
      && (await creerZenodo({ jetons: magasin(), dossier: () => session }).etat('zenodo.org')).jeton === true && (await z.etat('sandbox.zenodo.org')).jeton === false);
  verifier('another host, or what isn’t a token, refused: nothing kept',
    (await z.jeton('github.com', JETON)).erreur === 'demande' && (await z.etat('evil.example')).erreur === 'demande' && (await z.oublier('github.com')).erreur === 'demande'
      && (await z.jeton('sandbox.zenodo.org', 'deux mots')).erreur === 'jeton_invalide' && (await z.jeton('sandbox.zenodo.org', 'court')).erreur === 'jeton_invalide'
      && await magasin().lire('github.com') === GITHUB && !(await magasin().present('sandbox.zenodo.org')));
  // the deposit, in the interface's order: the draft and its DOI, the package written with it in the
  // engine's session folder, then the package in that draft
  s.mode = '';
  s.recues.length = 0;
  const d1 = await z.brouillon('zenodo.org');
  const zip = paquetAvec(session, d1.doi);
  const apres1 = requetes(s);
  const d = await z.deposer('zenodo.org', d1.id, zip, { ...META, langue: 'fr' });
  verifier('the draft and its DOI first ({ id, doi, page }), the package then deposited in it with the host’s token; its page opened',
    d1.id === 4242 && d1.doi === DOI && d1.page === `${base}/deposit/4242` && !('bucket' in d1) && apres1 === 'POST /api/deposit/depositions'
      && d.id === 4242 && d.doi === DOI && d.page === d1.page && ouverts.join() === d.page && s.recues.length === 3 && s.recues.every((x) => x.auth === `Bearer ${JETON}`)
      && s.metadonnees.language === 'fra' && s.fichiers.get('Apples package.zip').includes(`?doi=${DOI}`), `${JSON.stringify([d1, d])} ${requetes(s)}`);
  const pasZip = path.join(session, 'Apples.txt');
  fs.writeFileSync(pasZip, 'x');
  const d2 = await z.brouillon('zenodo.org');
  verifier('a draft this run didn’t create, a package outside the engine’s session folder or not a .zip, another host: refused',
    (await z.deposer('zenodo.org', 9999, zip, META)).erreur === 'demande' && (await z.deposer('sandbox.zenodo.org', d2.id, zip, META)).erreur === 'demande'
      && (await z.deposer('zenodo.org', d2.id, fichier, META)).erreur === 'demande' && (await z.deposer('zenodo.org', d2.id, pasZip, META)).erreur === 'demande'
      && (await z.deposer('evil.example', d2.id, zip, META)).erreur === 'demande' && (await z.brouillon('evil.example')).erreur === 'demande');
  verifier('without a token for the host: sans_jeton, no draft', (await z.brouillon('sandbox.zenodo.org')).erreur === 'sans_jeton');
  // a failure in the draft (its description refused): tried again, the same draft, not a new one
  s.mode = 'refus';
  const echoue = await z.deposer('zenodo.org', d2.id, zip, META);
  s.mode = '';
  s.recues.length = 0;
  const repris = await z.deposer('zenodo.org', d2.id, zip, META);
  verifier('a deposit that failed, tried again: the same draft, no new one', echoue.erreur === 'refus' && echoue.page === d2.page && repris.id === d2.id && !s.recues.some((x) => x.method === 'POST'),
    `${JSON.stringify([echoue, repris])} ${requetes(s)}`);
  s.mode = 'sans_doi';
  const sansDoi = await z.brouillon('zenodo.org');
  s.mode = '';
  verifier('a DOI not reserved: code doi with the draft’s page, and that draft can’t be given a package', sansDoi.erreur === 'doi' && sansDoi.page === `${base}/deposit/4242`
    && (await z.deposer('zenodo.org', 4242, zip, META)).erreur === 'demande', JSON.stringify(sansDoi));
  const refuse = await creerZenodo({ jetons: { lire: async () => 'faux-jeton-qui-ne-marche-pas', present: async () => true, seulementSession: async () => false },
    base: () => base, dossier: () => session }).brouillon('zenodo.org');
  verifier('a token refused: its code, and its detail without the token', refuse.erreur === 'jeton' && !JSON.stringify(refuse).includes('faux-jeton'), JSON.stringify(refuse));
  await z.oublier('zenodo.org');
  verifier('forgotten: gone from the file, github.com’s kept', !(await z.etat('zenodo.org')).jeton && !('zenodo.org' in JSON.parse(fs.readFileSync(f, 'utf8')))
    && await magasin().lire('github.com') === GITHUB);
  // (a system that can't encrypt: for this session only, nothing written)
  const f2 = path.join(dossier, 'jetons-session.json');
  const z2 = creerZenodo({ jetons: magasin(false, f2), dossier: () => session });
  const g2 = await z2.jeton('zenodo.org', JETON);
  verifier('without encryption: kept for the session only, nothing written', g2.persistant === false && (await z2.etat('zenodo.org')).jeton && !fs.existsSync(f2)
    && !(await creerZenodo({ jetons: magasin(false, f2), dossier: () => session }).etat('zenodo.org')).jeton);
  // (fetch keeps its connections open: closed from the server's side, before leaving)
  for (const x of [s, autre]) {
    x.serveur.closeAllConnections();
    await new Promise((ok) => x.serveur.close(ok));
  }

  fs.rmSync(dossier, { recursive: true, force: true });
  console.log(echecs ? `${echecs} vérification(s) en échec` : 'Tout est bon');
  process.exitCode = echecs ? 1 : 0;
})().catch((e) => { console.error(e); process.exitCode = 1; });
