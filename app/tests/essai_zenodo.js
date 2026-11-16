// Checks zenodo.js (a reproducibility package deposited on Zenodo as a draft) against a stand-in for
// Zenodo's deposit API on 127.0.0.1, which answers as developers.zenodo.org describes it: the draft
// created, the file put in its bucket (with its MD5), the description checked; then jetons.js, the
// tokens kept per host with a stand-in for Electron's safeStorage. Nothing here reaches zenodo.org
// or sandbox.zenodo.org.
//   node tests/essai_zenodo.js
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const os = require('os');
const path = require('path');
const { deposerBrouillon, metadonneesZenodo, DOI_NUEE } = require('../zenodo');
const { creerJetons } = require('../jetons');

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const JETON = 'zx9TOKENfake0123456789abcdefABCDEF0123456789abcdefABCDEF0123';
const dossier = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-zenodo-'));

// The stand-in: every request recorded (method, path, Authorization, body); `mode` makes it answer
// otherwise (a bucket elsewhere, another MD5, a description refused).
function zenodo() {
  const s = { recues: [], fichiers: new Map(), metadonnees: null, mode: '', autre: null };
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
      if (req.method === 'POST' && req.url === '/api/deposit/depositions') {
        json(201, { id: 4242, state: 'unsubmitted', submitted: false, metadata: { prereserve_doi: { doi: '10.5072/zenodo.4242', recid: 4242 } },
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
        json(200, { id: 4242, metadata: m, state: 'unsubmitted', submitted: false });
      } else {
        json(404, { status: 404, message: 'The requested URL was not found on the server.' });
      }
    });
  });
  return new Promise((ok) => s.serveur.listen(0, '127.0.0.1', () => ok(s)));
}

(async () => {
  const META = { titre: 'Apples in <orchards> & co', description: 'Made-up data: 120 apples.\n\nA second paragraph,\non two lines.',
    auteurs: [{ prenom: 'Alice', nom: 'Exemple', orcid: '0000-0002-1825-0097' }, { prenom: '', nom: 'Tour du Valat', orcid: '' }],
    mots_cles: ['apples', 'PCA'], licence: 'CC0-1.0' };
  const m = metadonneesZenodo(META, { langue: 'fr' });
  verifier('the description: title, creators (Family, Given; ORCID), HTML paragraphs escaped, keywords, licence, dataset, open',
    m.title === META.titre && m.creators.length === 2 && m.creators[0].name === 'Exemple, Alice' && m.creators[0].orcid === '0000-0002-1825-0097'
      && m.creators[1].name === 'Tour du Valat' && !('orcid' in m.creators[1])
      && m.description === '<p>Made-up data: 120 apples.</p><p>A second paragraph,<br>on two lines.</p>'
      && m.keywords.join() === 'apples,PCA' && m.license === 'cc0-1.0' && m.upload_type === 'dataset' && m.access_right === 'open' && m.language === 'fra',
    JSON.stringify(m));
  verifier('Nuée as the software that made it, by its DOI', m.related_identifiers.length === 1 && m.related_identifiers[0].identifier === DOI_NUEE
    && m.related_identifiers[0].relation === 'isCompiledBy' && m.related_identifiers[0].resource_type === 'software' && DOI_NUEE === '10.5281/zenodo.24263014');
  verifier('the licences by their Zenodo ids, CC BY 4.0 when none is given; no keywords when there are none',
    ['CC-BY-4.0', 'CC-BY-SA-4.0', 'CC-BY-NC-4.0', undefined].map((l) => metadonneesZenodo({ ...META, licence: l }).license).join() === 'cc-by-4.0,cc-by-sa-4.0,cc-by-nc-4.0,cc-by-4.0'
      && !('keywords' in metadonneesZenodo({ ...META, mots_cles: [] })) && metadonneesZenodo(META).language === 'eng');

  const fichier = path.join(dossier, 'Apples package.zip');
  const octets = crypto.randomBytes(200000);
  fs.writeFileSync(fichier, octets);
  const s = await zenodo();
  const base = `http://127.0.0.1:${s.serveur.address().port}`;

  const r = await deposerBrouillon({ base, jeton: JETON, fichier, metadonnees: m });
  verifier('a draft: created, its package in its bucket (the same bytes, its MD5 checked), then its description', r.id === 4242 && r.fichier === 'Apples package.zip'
    && s.recues.map((x) => `${x.method} ${x.url}`).join(' | ') === 'POST /api/deposit/depositions | PUT /api/files/b0c1d2e3-f4a5-4b6c-8d7e-9f0a1b2c3d4e/Apples%20package.zip | PUT /api/deposit/depositions/4242'
    && Buffer.compare(s.fichiers.get('Apples package.zip'), octets) === 0 && JSON.stringify(s.metadonnees) === JSON.stringify(m), s.recues.map((x) => `${x.method} ${x.url}`).join(' | '));
  verifier('its page, on Zenodo itself, is given back', r.page === `${base}/deposit/4242`, r.page);
  verifier('never published: no request to the publish action', !s.recues.some((x) => /publish/.test(x.url)));
  verifier('the token only in the Authorization header, never in an address', s.recues.every((x) => x.auth === `Bearer ${JETON}` && !x.url.includes(JETON)));
  verifier('the file sent as a file, the description as JSON', s.recues[1].type === 'application/octet-stream' && s.recues[2].type === 'application/json');

  // a token Zenodo refuses: no draft, the error says so, without the token
  s.recues.length = 0;
  let e = await deposerBrouillon({ base, jeton: 'faux-jeton-qui-ne-marche-pas', fichier, metadonnees: m }).catch((x) => x);
  verifier('a token refused (401): code jeton, no draft, nothing sent after', e.code === 'jeton' && e.statut === 401 && !e.page && s.recues.length === 1 && !String(e.message).includes('faux-jeton'), `${e.code} ${e.message}`);

  // a description refused: the draft exists, and its page is given to finish it there
  s.mode = 'refus';
  e = await deposerBrouillon({ base, jeton: JETON, fichier, metadonnees: m }).catch((x) => x);
  verifier('a description refused (400): code refus, Zenodo’s words, the draft’s page to finish it', e.code === 'refus' && /Validation error/.test(e.message)
    && /metadata\.creators: Missing data/.test(e.message) && e.page === `${base}/deposit/4242`, `${e.code} ${e.message} ${e.page}`);

  // the file received isn't the one sent
  s.mode = 'empreinte';
  e = await deposerBrouillon({ base, jeton: JETON, fichier, metadonnees: m }).catch((x) => x);
  verifier('another MD5 than the file’s: code empreinte, and its description isn’t sent', e.code === 'empreinte' && e.page && s.recues[s.recues.length - 1].method === 'PUT'
    && /\/api\/files\//.test(s.recues[s.recues.length - 1].url), `${e.code} ${e.message}`);

  // an address elsewhere in Zenodo's answer: refused, the token never goes there
  const autre = await zenodo();
  s.autre = `http://127.0.0.1:${autre.serveur.address().port}`;
  s.mode = 'ailleurs';
  e = await deposerBrouillon({ base, jeton: JETON, fichier, metadonnees: m }).catch((x) => x);
  verifier('a bucket on another server: refused (code adresse), that server never reached', e.code === 'adresse' && autre.recues.length === 0, `${e.code} ${e.message} ${autre.recues.length}`);

  // nothing answers
  const ferme = await zenodo();
  const portFerme = ferme.serveur.address().port;
  await new Promise((ok) => ferme.serveur.close(ok));
  e = await deposerBrouillon({ base: `http://127.0.0.1:${portFerme}`, jeton: JETON, fichier, metadonnees: m }).catch((x) => x);
  verifier('Zenodo out of reach: code reseau', e.code === 'reseau', `${e.code} ${e.message}`);
  // (fetch keeps its connections open: closed from the server's side, before leaving)
  for (const x of [s, autre]) {
    x.serveur.closeAllConnections();
    await new Promise((ok) => x.serveur.close(ok));
  }

  // ---- jetons.js: one token per host, encrypted in the profile, or for the session only
  const coffre = (disponible, fond = 'gnome_libsecret') => ({
    isEncryptionAvailable: () => disponible,
    getSelectedStorageBackend: () => fond,
    // (a stand-in for the system's encryption: reversible, and nothing like the token)
    encryptString: (x) => Buffer.from(`chiffre:${Buffer.from(x, 'utf8').toString('hex').split('').reverse().join('')}`, 'utf8'),
    decryptString: (b) => { const t = b.toString('utf8'); if (!t.startsWith('chiffre:')) throw new Error('not ours'); return Buffer.from(t.slice(8).split('').reverse().join(''), 'hex').toString('utf8'); },
  });
  const f = path.join(dossier, 'jetons.json');
  const j1 = creerJetons({ fichier: f, coffre: coffre(true) });
  const garde = j1.garder('zenodo.org', JETON);
  const texte = fs.readFileSync(f, 'utf8');
  verifier('kept encrypted in the profile: the file holds no token in clear', garde === true && j1.persistant() && !texte.includes(JETON) && JSON.parse(texte)['zenodo.org'].length > 0);
  verifier('read back, in this session and the next; one host at a time', j1.lire('zenodo.org') === JETON && creerJetons({ fichier: f, coffre: coffre(true) }).lire('zenodo.org') === JETON
    && j1.lire('sandbox.zenodo.org') === null);
  j1.garder('sandbox.zenodo.org', 'autre-jeton-du-bac-a-sable');
  j1.oublier('zenodo.org');
  verifier('forgotten: gone from the profile, the other host’s kept', j1.lire('zenodo.org') === null && !('zenodo.org' in JSON.parse(fs.readFileSync(f, 'utf8')))
    && creerJetons({ fichier: f, coffre: coffre(true) }).lire('sandbox.zenodo.org') === 'autre-jeton-du-bac-a-sable');
  const f2 = path.join(dossier, 'jetons-session.json');
  for (const [nom, c, plateforme] of [['no encryption', coffre(false), 'win32'], ['Linux without a keyring (basic_text)', coffre(true, 'basic_text'), 'linux']]) {
    const j2 = creerJetons({ fichier: f2, coffre: c, plateforme });
    const g = j2.garder('zenodo.org', JETON);
    verifier(`${nom}: kept for the session only, nothing written`, g === false && !j2.persistant() && j2.lire('zenodo.org') === JETON && !fs.existsSync(f2)
      && creerJetons({ fichier: f2, coffre: c, plateforme }).lire('zenodo.org') === null);
  }
  fs.writeFileSync(f2, '{ pas du JSON');
  const j3 = creerJetons({ fichier: f2, coffre: coffre(true) });
  verifier('a damaged file: no token, and the next one kept over it', j3.lire('zenodo.org') === null && j3.garder('zenodo.org', JETON) && j3.lire('zenodo.org') === JETON);

  fs.rmSync(dossier, { recursive: true, force: true });
  console.log(echecs ? `${echecs} vérification(s) en échec` : 'Tout est bon');
  process.exitCode = echecs ? 1 : 0;
})().catch((e) => { console.error(e); process.exitCode = 1; });
