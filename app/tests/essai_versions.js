// Checks versions.js (projects' versions kept in the background with git, shared with GitHub or
// GitLab) without Electron, on files in the system's temporary folder, a clock of its own:
// - automatic versions and their rules (an hour since the last one, closing, before an update),
//   what they say changed, the settings and a project switched off; names, reading and restoring;
//   a project renamed outside Nuée or saved as another, the lock between two Nuée, the thinning of
//   old versions (never what was sent);
// - a git repository of the user's around the folder, left exactly as it was; a UNC path on Windows
//   (\\localhost\C$\…) when that share answers;
// - against a smart-HTTP git server started here (Node's http and the system git's http-backend,
//   private repositories: a token is needed, one may only read): sharing with an empty repository,
//   one GitLab made with a README, one with other files and another project at the same place;
//   opening a project from a repository; receiving each case (a fast-forward, other files merged,
//   a conflict and each way out: mine, theirs, a merge made elsewhere), sending after a merge,
//   changes not saved waiting, names both ways, the background (events, every five minutes at
//   most, a failure told and tried again); skipped with a message when git isn't found;
// - the tokens: encrypted at rest, kept for the session where the system can't encrypt, never in an
//   error, a log, an address, a file in clear, or what the functions return.
//   node tests/essai_versions.js
//   npx electron tests/essai_versions.js    (the same through Chromium's network stack, net.fetch,
//                                            and the system's safeStorage, as main.js)
const { execFileSync, spawn } = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const os = require('os');
const path = require('path');
const electron = process.versions.electron && !process.env.ELECTRON_RUN_AS_NODE ? require('electron') : null;
const { creerVersions, magasinJetons, adresseDistante, nomRef, decrireChangements } = require('../versions');

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${typeof detail === 'string' ? detail : JSON.stringify(detail)}`}`);
  if (!ok) echecs += 1;
};
// (every message thrown, for the tokens' check)
const erreurs = [];
async function refus(f) {
  try { await f(); return null; } catch (e) { erreurs.push(`${e.message}\n${e.stack}`); return e.message; }
}
const journal = [];
for (const k of ['log', 'info', 'warn', 'error', 'debug']) {
  const avant = console[k];
  console[k] = (...a) => { journal.push(a.map(String).join(' ')); avant.apply(console, a); };
}

const racine = fs.mkdtempSync(path.join(os.tmpdir(), 'essai-versions-'));
const egal = (a, b) => (process.platform === 'win32' ? a.toLowerCase() === b.toLowerCase() : a === b);
const ecrire = (f, texte) => { fs.mkdirSync(path.dirname(f), { recursive: true }); fs.writeFileSync(f, texte); };
const contenu = (f) => fs.readFileSync(f, 'utf8');
const MINUTE = 60e3;
const JOUR = 24 * 60 * MINUTE;
// A project as the engine writes it (what the descriptions read)
function projetJson({ lignes = 2, vars = 2, modeles = { M1: 2 }, documents = { D1: ['Report', 'a'] }, figures = {}, etapes = [], source = null } = {}) {
  const obs = Array.from({ length: lignes }, (_, i) => ({ uid: i + 1, ids: `S${i + 1}` }));
  const vs = Array.from({ length: vars }, (_, i) => ({ uid: i + 1, ids: `v${i + 1}` }));
  return `${JSON.stringify({
    format: 2, nom: 'Suivi des hérons', observations: obs, variables: vs,
    datasets: [{ name: 'D', observations: obs.map((o) => o.uid), variables: vs.map((v) => v.uid), values: obs.map((o) => vs.map((v) => o.uid * 10 + v.uid)) }],
    models: Object.entries(modeles).map(([name, A]) => ({ type: 'PLS', name, A })),
    documents: Object.entries(documents).map(([id, [titre, texte]]) => ({ id, titre, texte })),
    figures: Object.entries(figures).map(([id, [titre, c]]) => ({ id, titre, contenu: c })),
    etapes, ...(source ? { source_donnees: source } : {}), nuee: { format: 3 },
  })}\n`;
}

// The system git, without the computer's own settings (a Windows one sets core.autocrlf)
const configVide = path.join(racine, 'gitconfig');
fs.writeFileSync(configVide, '');
const envGit = { ...process.env, GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: configVide, GIT_TERMINAL_PROMPT: '0',
  GIT_AUTHOR_NAME: 'Collègue', GIT_AUTHOR_EMAIL: 'collegue@example.org', GIT_COMMITTER_NAME: 'Collègue', GIT_COMMITTER_EMAIL: 'collegue@example.org' };
const gitSys = (args, cwd) => execFileSync('git', ['-c', 'core.quotepath=false', ...args], { cwd, env: envGit, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
let gitPresent = true;
try { gitSys(['--version'], racine); } catch { gitPresent = false; }
const initSys = (dossier, ...options) => { gitSys(['init', '-q', ...options, dossier], racine); gitSys(['symbolic-ref', 'HEAD', 'refs/heads/main'], dossier); };

// A stand-in for Electron's safeStorage: "encrypted" by a XOR after the key's number, refused at
// will; a key changed since asks for encrypting again; its calls counted
const fauxChiffrement = ({ disponible = true, magasin = 'gnome_libsecret', cle = 1 } = {}) => {
  const f = {
    appels: 0,
    isAsyncEncryptionAvailable: async () => { f.appels += 1; return disponible; },
    getSelectedStorageBackend: () => magasin,
    encryptStringAsync: async (s) => { f.appels += 1; return Buffer.from([cle, ...Buffer.from(s, 'utf8').map((b) => b ^ 0xa5)]); },
    decryptStringAsync: async (b) => { f.appels += 1; return { shouldReEncrypt: b[0] !== cle, result: Buffer.from(b.subarray(1).map((x) => x ^ 0xa5)).toString('utf8') }; },
  };
  return f;
};
// (Node's rmSync, on Windows, leaves read-only files in some versions, git's objects among them,
// and paths that aren't ASCII in others: every file made writable, then removed one by one)
function effacer(d) {
  for (const e of fs.readdirSync(d, { withFileTypes: true })) {
    const p = path.join(d, e.name);
    if (e.isDirectory()) effacer(p);
    else { fs.chmodSync(p, 0o666); fs.unlinkSync(p); }
  }
  fs.rmdirSync(d);
}

const JETON = `ghp_essai${crypto.randomBytes(12).toString('hex')}`;
const JETON_LECTURE = `glpat-lecture${crypto.randomBytes(12).toString('hex')}`;
const FAUX_JETON = `ghp_faux${crypto.randomBytes(12).toString('hex')}`;
const adresses = [];  // every address fetched
let fetchReel = fetch;
// (the module only takes https: the test server's https addresses are fetched over http)
const fetchEssai = (url, init) => {
  adresses.push(String(url));
  return fetchReel(String(url).replace(/^https:\/\/127\.0\.0\.1:/, 'http://127.0.0.1:'), init);
};

// The world's clock, the same for every computer, moved by the checks (never back before now: an
// object an hour old by it is one the thinning may remove)
const horloge = { t: Date.now() };
const avancer = (ms) => { horloge.t += ms; };
const temporaires = path.join(racine, 'temporaires');
fs.mkdirSync(temporaires);
const retours = [];  // what the functions returned, for the tokens' check
// One computer: its user, settings (author, tokens), language and the events it is told
function ordinateur(nom, { langue = 'en', chiffrement = fauxChiffrement() } = {}) {
  let r = {};
  const fichierJetons = path.join(racine, `jetons-${nom}.json`);
  const evenements = [];
  const v = creerVersions({
    fetch: fetchEssai, langue: () => langue, temporaire: () => temporaires, maintenant: () => horloge.t, utilisateur: () => nom,
    jetons: magasinJetons({ fichier: () => fichierJetons, safeStorage: chiffrement }),
    reglages: { lire: () => r, ecrire: (x) => { r = x; } }, evenement: (e) => { evenements.push(e); retours.push(e); },
  });
  const ordi = { nom, fichierJetons, chiffrement, evenements, langue: (l) => { langue = l; }, attendre: () => v.enAttente(), auPremierPlan: () => v.auPremierPlan() };
  for (const [k, f] of Object.entries(v)) ordi[k] = async (...a) => { const x = await f(...a); retours.push(x); return x; };
  return ordi;
}
const types = (o) => o.evenements.map((e) => e.type);

// ---------------------------------------------------------------- a smart-HTTP git server

// The system git's http-backend behind Node's http, as GitHub answers: every repository private
// (401 without a token, Basic authentication), 403 to send with the token that may only read; down
// on demand (panne: connections cut)
function serveurGit(depots) {
  const requetes = [];
  const etat = { panne: false };
  const serveur = http.createServer((req, res) => {
    requetes.push({ url: req.url, authorization: req.headers.authorization || '' });
    if (etat.panne) { req.socket.destroy(); return; }
    const u = new URL(req.url, 'http://x');
    const m = /^Basic (.+)$/i.exec(req.headers.authorization || '');
    const motDePasse = m ? Buffer.from(m[1], 'base64').toString('utf8').replace(/^[^:]*:/, '') : null;
    const droit = motDePasse === JETON ? 'ecrire' : motDePasse === JETON_LECTURE ? 'lire' : null;
    if (!droit) {
      res.writeHead(401, { 'WWW-Authenticate': 'Basic realm="essai"', 'Content-Type': 'text/plain' });
      res.end('Authentication required');
      return;
    }
    if (droit !== 'ecrire' && (u.searchParams.get('service') === 'git-receive-pack' || u.pathname.endsWith('/git-receive-pack'))) {
      res.writeHead(403, { 'Content-Type': 'text/plain' });
      res.end('Write access to repository not granted.');
      return;
    }
    const depot = /^\/([\w.-]+\.git)\//.exec(u.pathname);
    if (!depot || !fs.existsSync(path.join(depots, depot[1]))) {
      res.writeHead(404, { 'Content-Type': 'text/plain' });
      res.end('Repository not found.');
      return;
    }
    const env = { ...envGit, GIT_PROJECT_ROOT: depots, GIT_HTTP_EXPORT_ALL: '1', PATH_INFO: u.pathname, REQUEST_METHOD: req.method,
      QUERY_STRING: u.search.replace(/^\?/, ''), CONTENT_TYPE: req.headers['content-type'] || '', REMOTE_USER: 'essai', REMOTE_ADDR: '127.0.0.1' };
    if (req.headers['content-length']) env.CONTENT_LENGTH = req.headers['content-length'];
    if (req.headers['git-protocol']) env.GIT_PROTOCOL = req.headers['git-protocol'];
    if (req.headers['content-encoding']) env.HTTP_CONTENT_ENCODING = req.headers['content-encoding'];
    const cgi = spawn('git', ['http-backend'], { env, windowsHide: true });
    req.pipe(cgi.stdin);
    let tete = Buffer.alloc(0);
    let commence = false;
    cgi.stdout.on('data', (morceau) => {
      if (commence) { res.write(morceau); return; }
      tete = Buffer.concat([tete, morceau]);
      const fin = tete.indexOf('\r\n\r\n');
      if (fin < 0) return;
      let statut = 200;
      const entetes = {};
      for (const ligne of tete.subarray(0, fin).toString('latin1').split('\r\n')) {
        const i = ligne.indexOf(':');
        if (/^status$/i.test(ligne.slice(0, i).trim())) statut = parseInt(ligne.slice(i + 1), 10);
        else entetes[ligne.slice(0, i).trim()] = ligne.slice(i + 1).trim();
      }
      res.writeHead(statut, entetes);
      commence = true;
      res.write(tete.subarray(fin + 4));
    });
    cgi.on('close', () => { if (!commence) { res.writeHead(500); } res.end(); });
    cgi.on('error', () => { if (!commence) { res.writeHead(500); } res.end(); });
  });
  return new Promise((ok) => serveur.listen(0, '127.0.0.1', () => ok({ serveur, requetes, etat, port: serveur.address().port })));
}

// ---------------------------------------------------------------- checks

async function sansDepot() {
  console.log('Ce qui se calcule sans dépôt');
  const p = (o) => JSON.parse(projetJson(o));
  const avant = p({ lignes: 264, modeles: { M1: 2, M2: 3, M3: 2 }, documents: { D1: ['Report', 'a'] } });
  const apres = p({ lignes: 288, modeles: { M1: 2, M2: 4, M4: 2 }, documents: { D1: ['Report', 'b'] } });
  verifier('ce qui a changé : les lignes, un modèle ajouté, retiré, modifié, un document', decrireChangements(avant, apres)
    === 'Data: 288 rows (+24); M4 added; M3 removed; M2 changed; document “Report” changed', decrireChangements(avant, apres));
  verifier('… en français', decrireChangements(avant, apres, true) === 'Données : 288 lignes (+24) ; M4 ajouté ; M3 retiré ; M2 modifié ; document « Report » modifié',
    decrireChangements(avant, apres, true));
  const f1 = p({ figures: { F1: ['Carte', '{}'], F2: ['Courbe', '{}'] } });
  const f2 = p({ figures: { F1: ['Carte', '{"a":1}'], F2: ['Courbe', '{"b":1}'] }, vars: 4 });
  verifier('des figures (féminin pluriel), des variables', decrireChangements(f1, f2, true) === 'Données : 4 variables (+2) ; figures « Carte », « Courbe » modifiées'
    && decrireChangements(f1, f2) === 'Data: 4 variables (+2); figures “Carte”, “Courbe” changed', [decrireChangements(f1, f2, true), decrireChangements(f1, f2)]);
  const e1 = p({ etapes: [] });
  const e2 = p({ etapes: [{ demande: 'remplacer', etiquette: 'Remplacer -9999 par vide' }], source: { chemin: 'C:/x/releves_2026.csv', sha256: 'b' } });
  const e3 = p({ etapes: [{ etiquette: 'a' }, { etiquette: 'b' }, { etiquette: 'c' }] });
  verifier('une étape (son nom), plusieurs, la source des données', decrireChangements(e1, e2) === 'Step “Remplacer -9999 par vide” added; data source: releves_2026.csv'
    && decrireChangements(e1, e3) === '3 steps added' && decrireChangements(e1, e3, true) === '3 étapes ajoutées', [decrireChangements(e1, e2), decrireChangements(e1, e3)]);
  const m1 = p({ modeles: {} });
  const m2 = p({ modeles: { M1: 1, M2: 1, M3: 1, M4: 1, M5: 1 } });
  const v1 = p({});
  const v2 = JSON.parse(projetJson({}));
  v2.datasets[0].values[0][0] = 99;
  verifier('plus de trois noms, des valeurs changées, rien de dit, la première version, illisible', decrireChangements(m1, m2) === 'M1, M2, M3 and 2 more added'
    && decrireChangements(v1, v2) === 'Data changed' && decrireChangements(v1, { ...v1, coordonnees: {} }) === 'Project changed'
    && decrireChangements(null, v1) === 'First version' && decrireChangements(undefined, v1, true) === 'Projet modifié', [decrireChangements(m1, m2), decrireChangements(v1, v2)]);
  if (gitPresent) {
    const etranges = ['a b', 'x..y', 'fin.', '.cache', 'v@{1}', 'a/b\\c', 'ré:sumé?*[', 'trop.lock', 'CON', 'nul.txt', '  -tiret-  ', 'Ünïcødé ok', 'x'.repeat(300)];
    const invalides = etranges.map(nomRef).filter((n) => { try { gitSys(['check-ref-format', `refs/tags/${n}`], racine); return !n; } catch { return true; } });
    verifier('les noms faits refs : valables pour git (ceux sans lettre ni chiffre, vides : refusés)', invalides.length === 0 && nomRef('<"|>') === '' && nomRef('@') === '', invalides);
  }
  verifier('les adresses : https seulement, sans identifiants, la page d’un dépôt ramenée au dépôt', JSON.stringify([
    'https://github.com/4rtefakt/nuee', 'https://github.com/4rtefakt/nuee/tree/main/app?x=1#l', 'https://GitLab.com/groupe/sous-groupe/projet/-/tree/main',
    'https://gitlab.inrae.fr:8443/eco/suivi.git/', 'http://github.com/a/b', 'git@github.com:a/b.git', 'https://x-access-token:abc@github.com/a/b', 'https://github.com/a', ' '].map(adresseDistante))
    === JSON.stringify([{ url: 'https://github.com/4rtefakt/nuee.git', hote: 'github.com' }, { url: 'https://github.com/4rtefakt/nuee.git', hote: 'github.com' },
      { url: 'https://gitlab.com/groupe/sous-groupe/projet.git', hote: 'gitlab.com' }, { url: 'https://gitlab.inrae.fr:8443/eco/suivi.git', hote: 'gitlab.inrae.fr:8443' },
      { refus: 'adresse' }, { refus: 'adresse' }, { refus: 'identifiants' }, { refus: 'adresse' }, { refus: 'adresse' }]));
  // the tokens' store
  const fichier = path.join(racine, 'jetons-magasin.json');
  const m = magasinJetons({ fichier: () => fichier, safeStorage: fauxChiffrement() });
  await m.ecrire('github.com', JETON);
  verifier('un jeton chiffré dans le fichier des réglages', fs.existsSync(fichier) && !contenu(fichier).includes(JETON) && await m.lire('github.com') === JETON
    && await m.present('github.com') && !(await m.seulementSession()));
  await m.ecrire('gitlab.example.org', JETON_LECTURE);
  await m.ecrire('github.com', '');
  verifier('un jeton vide le retire, les autres restent', !(await m.present('github.com')) && await m.lire('gitlab.example.org') === JETON_LECTURE
    && !Object.keys(JSON.parse(contenu(fichier))).includes('github.com') && !(await m.present('constructor')));
  const nouvelleCle = magasinJetons({ fichier: () => fichier, safeStorage: fauxChiffrement({ cle: 2 }) });
  verifier('la clé du système changée : lu, puis chiffré de nouveau', await nouvelleCle.lire('gitlab.example.org') === JETON_LECTURE
    && Buffer.from(JSON.parse(contenu(fichier))['gitlab.example.org'], 'base64')[0] === 2);
  const fichierS = path.join(racine, 'jetons-session.json');
  const s = magasinJetons({ fichier: () => fichierS, safeStorage: fauxChiffrement({ disponible: false }) });
  await s.ecrire('github.com', JETON);
  verifier('sans chiffrement : gardé pour la session, rien d’écrit, oublié au lancement suivant', await s.seulementSession() && await s.lire('github.com') === JETON
    && !fs.existsSync(fichierS) && !(await magasinJetons({ fichier: () => fichierS, safeStorage: fauxChiffrement({ disponible: false }) }).present('github.com')));
  if (process.platform === 'linux') {
    const b = magasinJetons({ fichier: () => fichierS, safeStorage: fauxChiffrement({ magasin: 'basic_text' }) });
    await b.ecrire('github.com', JETON);
    verifier('Linux sans trousseau (basic_text) : pour la session', await b.seulementSession() && !fs.existsSync(fichierS));
  }
  if (electron) {
    const fichierE = path.join(racine, 'jetons-electron.json');
    const e = magasinJetons({ fichier: () => fichierE, safeStorage: electron.safeStorage });
    await e.ecrire('github.com', JETON);
    const session = await e.seulementSession();
    verifier(`safeStorage du système (${session ? 'session' : 'chiffré'})`, await e.lire('github.com') === JETON && (session || !contenu(fichierE).includes(JETON)));
  }
}

async function automatiques() {
  console.log('Des versions automatiques');
  const dossier = path.join(racine, 'Projets d’été', 'Camargue 2026');
  const projet = path.join(dossier, 'Suivi des hérons.nuee');
  const histoire = path.join(dossier, '.nuee-versions');
  ecrire(projet, projetJson({ lignes: 2 }));
  ecrire(path.join(dossier, 'donnees.csv'), 'site;n\nS1;3\n');
  const alice = ordinateur('alice');
  const e0 = await alice.etat(projet);
  verifier('avant : des versions gardées (le réglage), aucune encore, rien d’écrit', e0.actif && !e0.desactive && e0.versions === 0 && e0.derniere === null
    && e0.dossier === histoire && !fs.existsSync(histoire), e0);
  await alice.ouvert(projet);
  const v1 = (await alice.apresEnregistrement(projet)).version;
  verifier('enregistré : une première version, l’utilisateur du système pour auteur', v1 && v1.description === 'First version' && v1.auto && v1.auteur.nom === 'alice'
    && v1.auteur.email === '' && !v1.envoyee, v1);
  verifier('à côté du projet : un dossier caché, un dépôt nu, un .gitignore de « * » ; rien d’autre', JSON.stringify(fs.readdirSync(dossier).sort())
    === JSON.stringify(['.nuee-versions', 'donnees.csv', 'Suivi des hérons.nuee'].sort()) && JSON.stringify(fs.readdirSync(histoire).sort()) === JSON.stringify(['.gitignore', 'Suivi des hérons.nuee.git'])
    && contenu(path.join(histoire, '.gitignore')) === '*\n' && fs.existsSync(path.join(histoire, 'Suivi des hérons.nuee.git', 'HEAD')), fs.readdirSync(histoire));
  if (process.platform === 'win32') {
    let cache = '';
    for (let i = 0; i < 20 && !/^\s*\S*H/.test(cache); i += 1) { await new Promise((ok) => setTimeout(ok, 100)); cache = execFileSync('attrib', [histoire], { encoding: 'utf8' }); }
    verifier('… caché sous Windows', /^\s*\S*H/.test(cache), cache);
  }
  ecrire(projet, projetJson({ lignes: 26, modeles: { M1: 2, M4: 1 } }));
  avancer(10 * MINUTE);
  verifier('enregistré dix minutes après : pas de version', (await alice.apresEnregistrement(projet)).version === null);
  avancer(51 * MINUTE);
  const v2 = (await alice.apresEnregistrement(projet)).version;
  verifier('une heure après la dernière : une version, ce qui a changé', v2 && v2.description === 'Data: 26 rows (+24); M4 added' && v2.auto, v2);
  ecrire(projet, projetJson({ lignes: 26, modeles: { M1: 3, M4: 1 } }));
  const v3 = (await alice.aLaFermeture(projet)).version;
  verifier('fermé : une version si le fichier a changé, puis plus', v3 && v3.description === 'M1 changed' && (await alice.aLaFermeture(projet)).version === null, v3);
  const avantMaj = await alice.marquer(projet, 'Before updating the data');
  ecrire(projet, projetJson({ lignes: 30, modeles: { M1: 3, M4: 1 } }));
  const maj = await alice.marquer(projet, 'Before updating the data');
  const l = await alice.liste(projet);
  verifier('avant une mise à jour : la dernière version si rien n’a changé, sinon une version qui le dit', avantMaj.oid === v3.oid && maj.oid === l[0].oid
    && l[0].description === 'Before updating the data' && !l[0].auto && l.length === 4, l.map((x) => x.description));
  verifier('la liste : de la plus récente à la plus ancienne, dates ISO', JSON.stringify(l.map((x) => x.description)) === JSON.stringify(['Before updating the data', 'M1 changed', 'Data: 26 rows (+24); M4 added', 'First version'])
    && l.every((x) => !Number.isNaN(Date.parse(x.date)) && /Z$/.test(x.date) && Array.isArray(x.noms)));
  // the settings, a project switched off
  verifier('les réglages : versions gardées, l’utilisateur du système', JSON.stringify(await alice.reglages()) === JSON.stringify({ auto: true, auteur: { nom: 'alice', email: '' } }));
  verifier('un auteur sans adresse valable : refusé', /email address/.test(await refus(() => alice.regler({ auteur: { nom: 'Alice', email: 'pas une adresse' } }))));
  await alice.regler({ auto: false });
  ecrire(projet, projetJson({ lignes: 31 }));
  avancer(61 * MINUTE);
  verifier('le réglage éteint : plus de version automatique', (await alice.apresEnregistrement(projet)).version === null && !(await alice.etat(projet)).actif);
  await alice.regler({ auto: true, auteur: { nom: '  Alice   Martin ', email: 'alice@example.org' } });
  await alice.activer(projet, false);
  const eOff = await alice.etat(projet);
  verifier('le projet éteint : plus de version, les autres lisibles', eOff.desactive && !eOff.actif && (await alice.aLaFermeture(projet)).version === null && eOff.versions === 4, eOff);
  await alice.activer(projet, true);
  const v5 = (await alice.aLaFermeture(projet)).version;
  verifier('rallumé : de nouveau, avec l’auteur donné', v5 && v5.auteur.nom === 'Alice Martin' && v5.auteur.email === 'alice@example.org' && (await alice.etat(projet)).actif, v5);
  // names, reading, restoring
  const ln = await alice.liste(projet);
  await alice.nommer(projet, ln[2].oid, 'Envoyé au comité');
  verifier('un nom déjà donné : refusé (quelle que soit sa casse)', /already given/.test(await refus(() => alice.nommer(projet, ln[0].oid, 'envoyé au COMITÉ'))));
  verifier('un nom vide, une version inconnue : refusés', /Give the version a name/.test(await refus(() => alice.nommer(projet, ln[0].oid, ' ~~ ')))
    && /isn’t in the project’s history/.test(await refus(() => alice.nommer(projet, '0123456789abcdef0123456789abcdef01234567', 'x'))));
  verifier('le nom dans la liste', JSON.stringify((await alice.liste(projet)).map((x) => x.noms)) === JSON.stringify([[], [], ['Envoyé au comité'], [], []]),
    (await alice.liste(projet)).map((x) => x.noms));
  if (gitPresent) {
    const tagsSys = gitSys(['--git-dir', path.join(histoire, 'Suivi des hérons.nuee.git'), 'tag', '-l', '-n1'], racine).trim().replace(/\s+/g, ' ');
    verifier('pour git : une étiquette annotée, son message le nom donné', tagsSys === 'Envoyé-au-comité Envoyé au comité', tagsSys);
  }
  await alice.retirerNom(projet, 'Envoyé au comité');
  verifier('un nom retiré ; un nom inconnu : refusé', (await alice.liste(projet)).every((x) => !x.noms.length) && /No version is named/.test(await refus(() => alice.retirerNom(projet, 'Inconnu'))));
  const lu = await alice.lire(projet, ln[3].oid);
  verifier('lire : la version dans un fichier du même nom, dans un dossier temporaire neuf', path.basename(lu.fichier) === 'Suivi des hérons.nuee'
    && path.dirname(path.dirname(lu.fichier)) === temporaires && contenu(lu.fichier) === projetJson({ lignes: 26, modeles: { M1: 2, M4: 1 } }), lu);
  ecrire(projet, projetJson({ lignes: 40 }));
  const r = await alice.restaurer(projet, ln[3].oid);
  const lr = await alice.liste(projet);
  verifier('restaurer : ce que le fichier avait gardé d’abord, puis le fichier de cette version et une version qui le dit',
    contenu(projet) === projetJson({ lignes: 26, modeles: { M1: 2, M4: 1 } }) && lr[0].oid === r.oid && /^Restored the version of \d{1,2} \w+ \d{4}/.test(lr[0].description)
    && lr[1].auto && lr[1].description.startsWith('Data: 40 rows'), lr.slice(0, 2));
  verifier('restaurer la version actuelle : refusé', /project as it is now/.test(await refus(() => alice.restaurer(projet, ln[3].oid))));
  alice.langue('fr');
  ecrire(projet, projetJson({ lignes: 26, modeles: { M1: 2, M4: 1 }, documents: { D1: ['Report', 'a'], D2: ['Notes', 'n'] } }));
  await alice.marquer(projet, '');
  verifier('… en français', (await alice.liste(projet))[0].description === 'Document « Notes » ajouté'
    && /n’a pas encore de versions|Aucune version/.test(await refus(() => alice.retirerNom(projet, 'x')) || 'Aucune version'), (await alice.liste(projet))[0].description);
  alice.langue('en');
  verifier('un chemin relatif, un autre fichier, un projet absent : refusés', /full path/.test(await refus(() => alice.etat('projet.nuee')))
    && /full path/.test(await refus(() => alice.marquer(path.join(dossier, 'donnees.csv'), ''))) && /isn’t there any more/.test(await refus(() => alice.etat(path.join(dossier, 'absent.nuee')))));
  if (process.platform === 'win32') {
    const autreCasse = path.join(dossier.toUpperCase(), 'SUIVI DES HÉRONS.NUEE');
    verifier('le chemin écrit dans une autre casse : le même projet', (await alice.etat(autreCasse)).versions === (await alice.liste(projet)).length);
  }
  // Save as, a rename outside Nuée
  const copie = path.join(dossier, 'Suivi 2027.nuee');
  fs.copyFileSync(projet, copie);
  await alice.apresEnregistrement(copie, { ancien: projet });
  const nAvant = (await alice.liste(projet)).length;
  verifier('enregistré sous un autre nom : la copie garde le passé', (await alice.liste(copie)).length === nAvant, (await alice.liste(copie)).length);
  ecrire(copie, projetJson({ lignes: 50 }));
  await alice.marquer(copie, '');
  verifier('… puis chacun son histoire', (await alice.liste(copie)).length === nAvant + 1 && (await alice.liste(projet)).length === nAvant);
  const renomme = path.join(dossier, 'Hérons 2027.nuee');
  fs.renameSync(copie, renomme);
  await alice.ouvert(renomme);
  verifier('renommé hors de Nuée : son histoire le suit', (await alice.liste(renomme)).length === nAvant + 1
    && !fs.existsSync(path.join(histoire, 'Suivi 2027.nuee.git')) && fs.existsSync(path.join(histoire, 'Hérons 2027.nuee.git')));
  const orphelin = path.join(dossier, 'Autre.nuee');
  fs.renameSync(renomme, orphelin);
  ecrire(orphelin, projetJson({ lignes: 7 }));
  await alice.ouvert(orphelin);
  verifier('un autre contenu sous un nouveau nom : une histoire neuve', (await alice.liste(orphelin)).length === 0 && fs.existsSync(path.join(histoire, 'Hérons 2027.nuee.git')));
  // the lock of another Nuée: waited for while held, taken over once two minutes old
  const verrou = path.join(histoire, 'Suivi des hérons.nuee.lock');
  fs.writeFileSync(verrou, JSON.stringify({ hote: 'autre-ordinateur', processus: 1, date: new Date().toISOString() }));
  setTimeout(() => fs.unlinkSync(verrou), 1200);
  ecrire(projet, projetJson({ lignes: 60 }));
  const t0 = Date.now();
  await alice.marquer(projet, '');
  verifier('le verrou d’un autre Nuée : attendu', Date.now() - t0 >= 1000 && !fs.existsSync(verrou), Date.now() - t0);
  fs.writeFileSync(verrou, JSON.stringify({ hote: 'autre-ordinateur', processus: 1, date: '2026-01-01T00:00:00Z' }));
  const vieux = new Date(Date.now() - 3 * MINUTE);
  fs.utimesSync(verrou, vieux, vieux);
  ecrire(projet, projetJson({ lignes: 61 }));
  const t1 = Date.now();
  await alice.marquer(projet, '');
  verifier('un verrou de plus de deux minutes : repris', Date.now() - t1 < 1000 && !fs.existsSync(verrou), Date.now() - t1);
  if (gitPresent) {
    gitSys(['--git-dir', path.join(histoire, 'Suivi des hérons.nuee.git'), 'fsck', '--strict', '--no-progress'], racine);
    verifier('pour git : un dépôt nu valable', gitSys(['--git-dir', path.join(histoire, 'Suivi des hérons.nuee.git'), 'rev-parse', '--is-bare-repository'], racine).trim() === 'true');
  }
  await alice.attendre();
  verifier('sans partage ni jeton : le chiffrement du système jamais demandé', alice.chiffrement.appels === 0, alice.chiffrement.appels);
}

// Old automatic versions thinned out, once a day in the background: one a day after 30 days, one a
// month after a year; a named one, one made before an update, and the last 30 days kept. Versions
// made over 400 days by the clock, thinned as they go, then once more.
async function eclaircissement() {
  console.log('Les versions anciennes éclaircies');
  const projet = path.join(racine, 'Long suivi', 'Long suivi.nuee');
  ecrire(projet, projetJson({ lignes: 1 }));
  const eve = ordinateur('eve');
  const maintenant = horloge.t;
  const minuit = new Date(maintenant);
  minuit.setHours(0, 0, 0, 0);
  const faits = [];
  let n = 1;
  let nomme = null;
  for (const j of [400, 399, 370, 200, 100, 45, 20, 5]) {
    for (const h of [9, 13, 17]) {
      horloge.t = minuit.getTime() - j * JOUR + h * 3600e3;
      n += 1;
      ecrire(projet, projetJson({ lignes: n }));
      const explicite = j === 100 && h === 13;
      const { oid } = await eve.marquer(projet, explicite ? 'Before updating the data' : '');
      if (j === 200 && h === 9) { await eve.nommer(projet, oid, 'Protocole 2025'); nomme = { quand: horloge.t, lignes: n }; }
      await eve.attendre();
      faits.push({ quand: horloge.t, lignes: n, auto: !explicite, nomme: j === 200 && h === 9 });
    }
  }
  const avant = await eve.liste(projet);
  const tailleAvant = (await eve.etat(projet)).taille;
  horloge.t = Math.max(maintenant, Date.now()) + 2 * 3600e3;
  // (the expected ones, by the same rules)
  const groupes = new Set();
  const attendus = [...faits].reverse().filter((f) => {
    const age = horloge.t - f.quand;
    if (!f.auto || f.nomme || age < 30 * JOUR) return true;
    const d = new Date(f.quand);
    const k = age > 365 * JOUR ? `m${d.getFullYear()}-${d.getMonth()}` : `j${d.getFullYear()}-${d.getMonth()}-${d.getDate()}`;
    if (groupes.has(k)) return false;
    groupes.add(k);
    return true;
  });
  ecrire(projet, projetJson({ lignes: 1 }));
  await eve.apresEnregistrement(projet);
  await eve.attendre();
  const apres = await eve.liste(projet);
  const dates = (vs) => vs.map((x) => Date.parse(x.date)).filter((t) => t < maintenant);
  verifier('éclairci : une par jour après 30 jours, une par mois après un an ; le nom, l’explicite et les 30 derniers jours gardés',
    JSON.stringify(dates(apres)) === JSON.stringify(attendus.map((f) => Math.floor(f.quand / 1000) * 1000)) && apres.length === attendus.length + 1 && attendus.length < faits.length,
    { avant: avant.length, apres: apres.length, attendus: attendus.length, faits: faits.length });
  const v = apres.find((x) => x.noms.includes('Protocole 2025'));
  verifier('… le nom suit sa version, le contenu des versions gardées intact', v && Date.parse(v.date) === Math.floor(nomme.quand / 1000) * 1000
    && contenu((await eve.lire(projet, v.oid)).fichier) === projetJson({ lignes: nomme.lignes }) && apres.some((x) => x.description === 'Before updating the data'));
  verifier('… et la place qu’elles prenaient rendue', (await eve.etat(projet)).taille < tailleAvant, [(await eve.etat(projet)).taille, tailleAvant]);
  if (gitPresent) gitSys(['--git-dir', path.join(racine, 'Long suivi', '.nuee-versions', 'Long suivi.nuee.git'), 'fsck', '--strict', '--no-progress'], racine);
}

// A repository of the user's around the folder: nothing of it changes
async function depotUtilisateur() {
  console.log('Dans un dépôt git de l’utilisateur');
  const etude = path.join(racine, 'Étude existante');
  ecrire(path.join(etude, 'donnees.csv'), 'site,n\nA,1\n');
  ecrire(path.join(etude, 'scripts', 'analyse.R'), 'x <- read.csv("donnees.csv")\n');
  ecrire(path.join(etude, '.gitignore'), 'sortie/\n');
  initSys(etude);
  ecrire(path.join(etude, 'analyses', 'Modèle final.nuee'), projetJson({ lignes: 4 }));
  gitSys(['add', '.'], etude);
  gitSys(['commit', '-q', '-m', 'Données'], etude);
  ecrire(path.join(etude, 'donnees.csv'), 'site,n\nA,1\nB,2\n');
  gitSys(['add', 'donnees.csv'], etude);
  ecrire(path.join(etude, 'scripts', 'analyse.R'), 'summary(x)\n');
  ecrire(path.join(etude, 'notes.txt'), 'à faire\n');
  ecrire(path.join(etude, 'sortie', 'figure.svg'), '<svg/>');
  const projet = path.join(etude, 'analyses', 'Modèle final.nuee');
  const objets = () => { let k = 0; const c = (p) => { for (const e of fs.readdirSync(p, { withFileTypes: true })) { if (e.isDirectory()) c(path.join(p, e.name)); else k += 1; } }; c(path.join(etude, '.git', 'objects')); return k; };
  const photo = () => JSON.stringify({
    fichiers: ['donnees.csv', 'scripts/analyse.R', '.gitignore', 'notes.txt', 'sortie/figure.svg'].map((f) => `${contenu(path.join(etude, f))}|${fs.statSync(path.join(etude, f)).mtimeMs}`),
    statut: gitSys(['status', '--porcelain', '--untracked-files=all'], etude).split(/\r?\n/).filter((x) => x && !x.includes('.nuee')).sort(),
    index: gitSys(['diff', '--cached'], etude), entrees: gitSys(['ls-files', '-s'], etude), tete: gitSys(['rev-parse', 'HEAD'], etude), refs: gitSys(['for-each-ref'], etude), objets: objets(),
  });
  const avant = photo();
  const bob = ordinateur('bob');
  await bob.apresEnregistrement(projet);
  ecrire(projet, projetJson({ lignes: 6 }));
  await bob.marquer(projet, '');
  await bob.attendre();
  verifier('deux versions, gardées à côté du projet', (await bob.liste(projet)).length === 2 && fs.existsSync(path.join(etude, 'analyses', '.nuee-versions', 'Modèle final.nuee.git', 'HEAD')));
  verifier('le dépôt de l’utilisateur, ses fichiers, son index et ce que git en dit : comme avant', photo() === avant);
  const vu = gitSys(['status', '--porcelain', '--untracked-files=all', '--', 'analyses'], etude).trim();
  verifier('… pour git, le dossier des versions est ignoré (son .gitignore), jamais à ajouter', !gitSys(['status', '--porcelain', '--untracked-files=all'], etude).includes('.nuee-versions')
    && gitSys(['status', '--porcelain', '--ignored'], etude).includes('!! analyses/.nuee-versions/') && /^M "?analyses\/Modèle final\.nuee"?$/.test(vu), JSON.stringify(vu));
  gitSys(['fsck', '--strict', '--no-progress'], etude);
}

// On Windows, a project on a network share (\\server\share\…): isomorphic-git alone would look for
// its repository on the current drive
async function partageReseau() {
  if (process.platform !== 'win32') return;
  const unc = `\\\\localhost\\${racine[0]}$${racine.slice(2)}`;
  if (!/^[A-Za-z]:\\/.test(racine) || !fs.existsSync(unc)) { console.log('  (partage \\\\localhost\\C$ absent : chemin UNC pas vérifié)'); return; }
  console.log('Un projet sur un partage réseau (UNC)');
  const local = path.join(racine, 'Partagé', 'Équipe');
  const projet = path.join(unc, 'Partagé', 'Équipe', 'Héronnière.nuee');
  ecrire(projet, projetJson({ lignes: 2 }));
  const deviné = path.join(`${racine[0]}:\\`, 'localhost');
  const existait = fs.existsSync(deviné);
  const v = ordinateur('carole');
  await v.apresEnregistrement(projet);
  ecrire(projet, projetJson({ lignes: 4 }));
  await v.marquer(projet, '');
  const r = await v.restaurer(projet, (await v.liste(projet))[1].oid);
  const e = await v.etat(projet);
  verifier('UNC : l’histoire dans le dossier partagé, versions, restauration', fs.existsSync(path.join(local, '.nuee-versions', 'Héronnière.nuee.git', 'HEAD')) && e.versions === 3
    && e.dossier.startsWith('\\\\localhost\\') && contenu(path.join(local, 'Héronnière.nuee')) === projetJson({ lignes: 2 }) && !!r.oid, e);
  verifier('UNC : rien d’écrit sur le disque courant', existait || !fs.existsSync(deviné));
  await v.attendre();
  if (gitPresent) gitSys(['--git-dir', path.join(local, '.nuee-versions', 'Héronnière.nuee.git'), 'fsck', '--strict', '--no-progress'], racine);
}

// Sharing, sending, receiving, opening from a repository, the background
async function echanges() {
  console.log('Partager, envoyer, recevoir (un serveur git sur 127.0.0.1)');
  const depots = path.join(racine, 'serveur');
  fs.mkdirSync(depots);
  const nu = (nom) => { const d = path.join(depots, nom); gitSys(['init', '-q', '--bare', d], racine); gitSys(['symbolic-ref', 'HEAD', 'refs/heads/main'], d); return d; };
  const surServeur = (depot, ...args) => gitSys(['--git-dir', depot, ...args], racine).trim();
  const fichierServeur = (depot, chemin) => gitSys(['--git-dir', depot, 'show', `main:${chemin}`], racine);
  // a colleague using git: a commit of some files, sent straight to the server's repository
  const parGit = (depot, fichiers, message) => {
    const c = fs.mkdtempSync(path.join(racine, 'collegue-'));
    gitSys(['clone', '-q', depot, c], racine);
    for (const [f, texte] of Object.entries(fichiers)) ecrire(path.join(c, f), texte);
    gitSys(['add', '-A'], c);
    gitSys(['commit', '-q', '-m', message], c);
    gitSys(['push', '-q', 'origin', 'HEAD:main'], c);
  };
  const { serveur, requetes, etat: serveurEtat, port } = await serveurGit(depots);
  const hote = `127.0.0.1:${port}`;
  const url = (nom) => `https://${hote}/${nom}`;
  try {
    // ---- shared with an empty repository, the token asked for, refused, only reading, right
    const herons = nu('herons.git');
    const dA = path.join(racine, 'Alice');
    const pA = path.join(dA, 'Suivi des hérons.nuee');
    ecrire(pA, projetJson({ lignes: 2 }));
    const alice = ordinateur('alice');
    await alice.regler({ auteur: { nom: 'Alice Martin', email: 'alice@example.org' } });
    await alice.ouvert(pA);
    await alice.apresEnregistrement(pA);
    verifier('une adresse http, une adresse avec un jeton : refusées', /starting with https/.test(await refus(() => alice.partager(pA, `http://${hote}/herons.git`)))
      && /without a user name or token/.test(await refus(() => alice.partager(pA, `https://u:${JETON}@${hote}/herons.git`))));
    verifier('partager sans jeton : le serveur en demande un, rien de gardé', /asks for a token/.test(await refus(() => alice.partager(pA, url('herons'))))
      && (await alice.etat(pA)).partage === null);
    await alice.jeton(hote, FAUX_JETON);
    verifier('un jeton faux : refusé', /refused the token/.test(await refus(() => alice.partager(pA, url('herons')))));
    await alice.jeton(hote, JETON_LECTURE);
    const partage = await alice.partager(pA, url('herons'));
    verifier('partagé (un jeton qui permet de lire) : l’adresse, .git ajouté, le projet à la racine', JSON.stringify(partage)
      === JSON.stringify({ url: url('herons.git'), hote, chemin: 'Suivi des hérons.nuee' }), partage);
    await alice.attendre();
    const erreurEnvoi = alice.evenements.find((e) => e.type === 'erreur');
    verifier('envoyé en arrière-plan : refusé (403), dit par un événement et par etat', erreurEnvoi && /may not write/.test(erreurEnvoi.message)
      && (await alice.etat(pA)).partage.erreur === erreurEnvoi.message, alice.evenements);
    await alice.jeton(` ${hote.toUpperCase()} `, JETON);
    verifier('vérifier le partage : le bon jeton', (await alice.verifierPartage(pA)).ok);
    const envoi = await alice.envoyer(pA);
    const e1 = await alice.etat(pA);
    verifier('envoyé : la version sur le serveur, à la racine ; plus d’erreur', envoi.resultat === 'envoye' && surServeur(herons, 'log', '--format=%s', 'main') === 'First version'
      && fichierServeur(herons, 'Suivi des hérons.nuee') === contenu(pA) && e1.partage.aEnvoyer === 0 && e1.partage.erreur === null && !!e1.partage.dernierEchange
      && (await alice.liste(pA))[0].envoyee, { envoi, e1 });
    // ---- Bob opens it from the repository
    const bob = ordinateur('bob');
    await bob.regler({ auteur: { nom: 'Bob', email: 'bob@example.org' } });
    verifier('les projets d’un dépôt, sans jeton : il en faut un', JSON.stringify(await bob.listerDistant(url('herons'))) === JSON.stringify({ fichiers: [], jeton: true }));
    await bob.jeton(hote, JETON);
    verifier('… avec : ses fichiers .nuee', JSON.stringify(await bob.listerDistant(url('herons'))) === JSON.stringify({ fichiers: ['Suivi des hérons.nuee'], jeton: false }));
    const dB = path.join(racine, 'Bob');
    fs.mkdirSync(dB);
    const { chemin: pB } = await bob.ouvrirDistant(url('herons'), 'Suivi des hérons.nuee', dB);
    const eB = await bob.etat(pB);
    verifier('ouvert depuis le dépôt : le fichier, son histoire, partagé', pB === path.join(dB, 'Suivi des hérons.nuee') && contenu(pB) === contenu(pA)
      && eB.versions === 1 && eB.partage.chemin === 'Suivi des hérons.nuee' && eB.partage.aRecevoir === 0 && (await bob.liste(pB))[0].envoyee, eB);
    verifier('… une seconde fois dans ce dossier : refusé', /already has a project/.test(await refus(() => bob.ouvrirDistant(url('herons'), 'Suivi des hérons.nuee', dB))));
    await bob.ouvert(pB);
    // ---- a fast-forward
    ecrire(pB, projetJson({ lignes: 5 }));
    avancer(61 * MINUTE);
    await bob.apresEnregistrement(pB);
    await bob.attendre();
    verifier('la version de Bob envoyée en arrière-plan (envoi, envoyé)', types(bob).includes('envoi') && types(bob).includes('envoye'), types(bob));
    const recu = await alice.recevoir(pA);
    verifier('Alice reçoit : le fichier de Bob, sa version', recu.resultat === 'recu' && recu.version.auteur.nom === 'Bob' && contenu(pA) === projetJson({ lignes: 5 })
      && (await alice.etat(pA)).partage.aRecevoir === 0, recu);
    // ---- names, both ways
    await alice.nommer(pA, (await alice.liste(pA))[0].oid, 'Soumis à la revue');
    await alice.attendre();
    verifier('un nom envoyé', /^Soumis-à-la-revue\s+Soumis à la revue$/.test(surServeur(herons, 'tag', '-l', '-n1')), surServeur(herons, 'tag', '-l', '-n1'));
    await bob.recevoir(pB);
    verifier('Bob le reçoit', (await bob.liste(pB))[0].noms.includes('Soumis à la revue'));
    await bob.retirerNom(pB, 'Soumis à la revue');
    await bob.attendre();
    await alice.recevoir(pA);
    verifier('Bob le retire : retiré du serveur, puis chez Alice', surServeur(herons, 'tag', '-l') === '' && (await alice.liste(pA)).every((x) => !x.noms.length));
    // ---- other files of the repository: theirs, carried over; the project's: mine
    parGit(herons, { 'README.md': '# Hérons\n', 'scripts/analyse.R': 'x <- 1\n' }, 'README');
    ecrire(pA, projetJson({ lignes: 6 }));
    await alice.marquer(pA, '');
    const envoiFusion = await alice.envoyer(pA);
    const parents = surServeur(herons, 'log', '-1', '--format=%P', 'main').split(' ');
    verifier('d’autres fichiers sur le serveur : réunis, envoyés après', envoiFusion.resultat === 'envoye' && parents.length === 2
      && fichierServeur(herons, 'README.md') === '# Hérons\n' && fichierServeur(herons, 'Suivi des hérons.nuee') === projetJson({ lignes: 6 }), { envoiFusion, parents });
    verifier('… jamais écrits dans le dossier de l’utilisateur', JSON.stringify(fs.readdirSync(dA).sort()) === JSON.stringify(['.nuee-versions', 'Suivi des hérons.nuee']));
    verifier('… et la réunion n’est pas une version de plus', (await alice.liste(pA))[0].description === 'Data: 6 rows (+1)', (await alice.liste(pA)).slice(0, 3));
    verifier('Bob reçoit', (await bob.recevoir(pB)).resultat === 'recu' && contenu(pB) === projetJson({ lignes: 6 }));
    // ---- a conflict, and each way out: theirs, mine, a merge
    const conflit = async (mien, leur) => {
      ecrire(pA, leur); await alice.marquer(pA, ''); await alice.envoyer(pA);
      ecrire(pB, mien); await bob.marquer(pB, '');
      return bob.recevoir(pB);
    };
    const c1 = await conflit(projetJson({ lignes: 7, modeles: { M1: 2, M5: 1 } }), projetJson({ lignes: 6, modeles: { M1: 2, M6: 1 } }));
    verifier('les deux ont changé le projet : un conflit, rien d’écrit', c1.resultat === 'conflit' && contenu(pB) === projetJson({ lignes: 7, modeles: { M1: 2, M5: 1 } })
      && c1.mien.auteur.nom === 'Bob' && c1.leur.auteur.nom === 'Alice Martin' && (await bob.etat(pB)).conflit.leur.oid === c1.leur.oid, c1);
    verifier('… les trois versions en fichiers, pour fusionner', contenu(c1.fichiers.base) === projetJson({ lignes: 6 }) && contenu(c1.fichiers.mien) === projetJson({ lignes: 7, modeles: { M1: 2, M5: 1 } })
      && contenu(c1.fichiers.leur) === projetJson({ lignes: 6, modeles: { M1: 2, M6: 1 } }) && c1.fichiers.sansBase === false, c1.fichiers);
    await bob.choisir(pB, 'leur');
    await bob.attendre();
    const lB = await bob.liste(pB);
    verifier('Bob prend celle d’Alice : son fichier, une fusion des deux histoires, envoyée', contenu(pB) === projetJson({ lignes: 6, modeles: { M1: 2, M6: 1 } })
      && lB[0].description === 'Took the version received rather than mine' && surServeur(herons, 'log', '-1', '--format=%P', 'main').split(' ').length === 2
      && lB.some((x) => x.description === 'M5 added' || x.description.includes('M5')) && (await bob.etat(pB)).conflit === null, lB.slice(0, 3).map((x) => x.description));
    verifier('choisir sans conflit : rien à choisir', /nothing to choose/.test(await refus(() => bob.choisir(pB, 'mien'))));
    await alice.recevoir(pA);
    const c2 = await conflit(projetJson({ lignes: 8 }), projetJson({ lignes: 9 }));
    await bob.choisir(pB, 'mien');
    await bob.attendre();
    verifier('Bob garde la sienne : son fichier, envoyée', c2.resultat === 'conflit' && contenu(pB) === projetJson({ lignes: 8 })
      && (await bob.liste(pB))[0].description === 'Kept my version rather than the one received' && fichierServeur(herons, 'Suivi des hérons.nuee') === projetJson({ lignes: 8 }));
    await alice.recevoir(pA);
    const c3 = await conflit(projetJson({ lignes: 8, modeles: { M1: 2, M7: 1 } }), projetJson({ lignes: 8, modeles: { M1: 2, M8: 1 } }));
    const fusion = path.join(racine, 'fusion.nuee');
    ecrire(fusion, projetJson({ lignes: 8, modeles: { M1: 2, M7: 1, M8: 1 } }));
    ecrire(pB, projetJson({ lignes: 8, modeles: { M1: 2, M7: 2 } }));
    verifier('une fusion préparée, puis le projet changé : refusée', c3.resultat === 'conflit' && /changed since the merge was prepared/.test(await refus(() => bob.choisir(pB, 'fusion', fusion))));
    const c4 = await bob.recevoir(pB);
    verifier('… un fichier fusionné absent, illisible : refusé', /isn’t there/.test(await refus(() => bob.choisir(pB, 'fusion', path.join(racine, 'absent.nuee'))))
      && /can’t be read/.test(await refus(async () => { ecrire(path.join(racine, 'casse.nuee'), '{ pas du json'); return bob.choisir(pB, 'fusion', path.join(racine, 'casse.nuee')); })));
    await bob.choisir(pB, 'fusion', fusion, 'M7 and M8 brought together');
    await bob.attendre();
    verifier('une fusion faite par le moteur : le fichier, une version qui le dit, envoyée', c4.resultat === 'conflit' && contenu(pB) === contenu(fusion)
      && (await bob.liste(pB))[0].description === 'M7 and M8 brought together' && fichierServeur(herons, 'Suivi des hérons.nuee') === contenu(fusion));
    verifier('… Alice la reçoit', (await alice.recevoir(pA)).resultat === 'recu' && contenu(pA) === contenu(fusion));
    // ---- changes not saved here: their version waits
    await bob.ouvert(pB, { modifie: true });
    ecrire(pA, projetJson({ lignes: 10 }));
    await alice.marquer(pA, '');
    await alice.envoyer(pA);
    const attente = await bob.recevoir(pB);
    verifier('des changements non enregistrés ici : leur version attend, rien d’écrit', attente.resultat === 'attente' && contenu(pB) === contenu(fusion), attente);
    bob.evenements.length = 0;
    await bob.apresEnregistrement(pB);
    await bob.attendre();
    verifier('… enregistré (rien de changé) : reçue, en arrière-plan', bob.evenements.some((e) => e.type === 'recu') && contenu(pB) === projetJson({ lignes: 10 })
      && (await bob.recevoir(pB)).resultat === 'a_jour', bob.evenements);
    // ---- the background: after a version, when Nuée comes back to the front (every five minutes
    // at most), a conflict told once, a failure told and tried again
    avancer(6 * MINUTE);
    bob.auPremierPlan();
    await bob.attendre();
    bob.evenements.length = 0;
    ecrire(pA, projetJson({ lignes: 11 }));
    avancer(61 * MINUTE);
    await alice.apresEnregistrement(pA);
    await alice.attendre();
    verifier('la version d’Alice envoyée en arrière-plan', types(alice).slice(-2).join() === 'envoi,envoye', types(alice).slice(-3));
    const avantRetour = bob.evenements.length;
    bob.auPremierPlan();
    await bob.attendre();
    verifier('Bob revenu au premier plan : ce que le dépôt a, reçu, dit', bob.evenements.length > avantRetour && bob.evenements.some((e) => e.type === 'recu'), bob.evenements);
    ecrire(pA, projetJson({ lignes: 11, modeles: { M1: 3 } }));
    await alice.marquer(pA, '');
    await alice.attendre();
    bob.evenements.length = 0;
    avancer(2 * MINUTE);
    bob.auPremierPlan();
    await bob.attendre();
    verifier('… deux minutes après : rien demandé', !bob.evenements.length && contenu(pB) === projetJson({ lignes: 11 }), bob.evenements);
    avancer(6 * MINUTE);
    bob.auPremierPlan();
    await bob.attendre();
    const reçu = bob.evenements.find((e) => e.type === 'recu');
    verifier('… cinq minutes après : reçue en arrière-plan, dite', reçu && egal(reçu.chemin, pB) && reçu.version.auteur.nom === 'Alice Martin'
      && contenu(pB) === projetJson({ lignes: 11, modeles: { M1: 3 } }), bob.evenements);
    ecrire(pA, projetJson({ lignes: 12 }));
    await alice.marquer(pA, '');
    await alice.attendre();
    bob.evenements.length = 0;
    ecrire(pB, projetJson({ lignes: 13 }));
    await bob.marquer(pB, '');
    await bob.attendre();
    const cf = bob.evenements.find((e) => e.type === 'conflit');
    verifier('un conflit en arrière-plan (l’envoi d’une version) : dit, avec ses trois fichiers', cf && contenu(cf.fichiers.leur) === projetJson({ lignes: 12 })
      && contenu(cf.fichiers.mien) === projetJson({ lignes: 13 }) && contenu(pB) === projetJson({ lignes: 13 }) && egal(cf.chemin, pB), bob.evenements);
    bob.evenements.length = 0;
    avancer(6 * MINUTE);
    bob.auPremierPlan();
    await bob.attendre();
    verifier('… une fois seulement', !bob.evenements.some((e) => e.type === 'conflit'), bob.evenements);
    await bob.choisir(pB, 'mien');
    await bob.attendre();
    await alice.recevoir(pA);
    serveurEtat.panne = true;
    ecrire(pA, projetJson({ lignes: 14 }));
    avancer(61 * MINUTE);
    await alice.apresEnregistrement(pA);
    await alice.attendre();
    const panne = alice.evenements.filter((e) => e.type === 'erreur').pop();
    verifier('le serveur en panne : l’erreur dite, gardée par etat, rien d’interrompu', panne && /Couldn’t reach/.test(panne.message) && (await alice.etat(pA)).partage.erreur === panne.message
      && (await alice.etat(pA)).partage.aEnvoyer > 0, panne);
    serveurEtat.panne = false;
    alice.evenements.length = 0;
    await alice.marquer(pA, '');
    await alice.attendre();
    verifier('… revenu : envoyé à la version suivante, l’erreur oubliée', types(alice).includes('envoye') && (await alice.etat(pA)).partage.erreur === null
      && (await alice.etat(pA)).partage.aEnvoyer === 0, types(alice));
    // ---- a repository GitLab made with a README; tokens kept for the session only
    const gitlab = nu('gitlab.git');
    parGit(gitlab, { 'README.md': '# Flamants\n' }, 'Initial commit');
    const pC = path.join(racine, 'Carole', 'Flamants.nuee');
    ecrire(pC, projetJson({ lignes: 3 }));
    const carole = ordinateur('carole', { chiffrement: fauxChiffrement({ disponible: false }) });
    await carole.apresEnregistrement(pC);
    const jC = await carole.jeton(hote, JETON);
    const pc = await carole.partager(pC, url('gitlab.git'));
    await carole.attendre();
    const eC = await carole.etat(pC);
    verifier('sans chiffrement : le jeton pour la session, dit', jC.jetonSession && eC.partage.jeton && eC.partage.jetonSession && !fs.existsSync(carole.fichierJetons));
    verifier('un dépôt créé avec un README : réuni sans version commune, envoyé', pc.chemin === 'Flamants.nuee' && types(carole).includes('envoye')
      && fichierServeur(gitlab, 'README.md') === '# Flamants\n' && fichierServeur(gitlab, 'Flamants.nuee') === projetJson({ lignes: 3 })
      && surServeur(gitlab, 'log', '-1', '--format=%P', 'main').split(' ').length === 2 && eC.versions === 1, { pc, eC, types: types(carole) });
    // ---- a repository with other files and another project at the same place
    const commun = nu('commun.git');
    parGit(commun, { 'Suivi des hérons.nuee': projetJson({ lignes: 77 }), 'scripts/x.R': 'y <- 2\n' }, 'Leur projet');
    const pD = path.join(racine, 'Dan', 'Suivi des hérons.nuee');
    ecrire(pD, projetJson({ lignes: 1 }));
    const dan = ordinateur('dan', { langue: 'fr' });
    await dan.apresEnregistrement(pD);
    ecrire(pD, projetJson({ lignes: 2 }));
    await dan.marquer(pD, '');
    await dan.jeton(hote, JETON);
    const pd = await dan.partager(pD, url('commun.git'));
    await dan.attendre();
    verifier('un autre projet à la même place : le sien dans un dossier, l’autre intact', pd.chemin === 'Suivi des hérons/Suivi des hérons.nuee'
      && fichierServeur(commun, 'Suivi des hérons.nuee') === projetJson({ lignes: 77 }) && fichierServeur(commun, 'Suivi des hérons/Suivi des hérons.nuee') === projetJson({ lignes: 2 })
      && fichierServeur(commun, 'scripts/x.R') === 'y <- 2\n', { pd, types: types(dan) });
    const lD = await dan.liste(pD);
    verifier('… ses versions d’avant toujours là, lisibles', lD.length === 2 && contenu((await dan.lire(pD, lD[1].oid)).fichier) === projetJson({ lignes: 1 }), lD.map((x) => x.description));
    parGit(commun, { 'Suivi des hérons.nuee': projetJson({ lignes: 78 }) }, 'Leur projet changé');
    verifier('… leur projet changé : à jour pour Dan, et gardé tel quel à la réunion suivante', (await dan.recevoir(pD)).resultat === 'a_jour' && contenu(pD) === projetJson({ lignes: 2 }));
    ecrire(pD, projetJson({ lignes: 3 }));
    await dan.marquer(pD, '');
    await dan.envoyer(pD);
    verifier('… envoyé : les deux projets, chacun le sien', fichierServeur(commun, 'Suivi des hérons.nuee') === projetJson({ lignes: 78 })
      && fichierServeur(commun, 'Suivi des hérons/Suivi des hérons.nuee') === projetJson({ lignes: 3 }));
    // ---- what was sent is never thinned
    const pE = path.join(racine, 'Erin', 'Ancien.nuee');
    const erin = ordinateur('erin');
    const depuis = horloge.t;
    for (const [j, h] of [[90, 9], [90, 15], [60, 9], [60, 15]]) {
      horloge.t = depuis - j * JOUR + h * 3600e3;
      ecrire(pE, projetJson({ lignes: j + h }));
      await erin.marquer(pE, '');
      await erin.attendre();
    }
    horloge.t = depuis;
    nu('ancien.git');
    await erin.jeton(hote, JETON);
    await erin.partager(pE, url('ancien.git'));
    await erin.attendre();
    ecrire(pE, projetJson({ lignes: 1 }));
    avancer(2 * JOUR);
    await erin.apresEnregistrement(pE);
    await erin.attendre();
    verifier('ce qui a été envoyé : jamais éclairci', (await erin.liste(pE)).length === 5, (await erin.liste(pE)).length);
    // ---- stop sharing; a repository that isn't there, a server that doesn't answer
    await erin.arreterPartage(pE);
    verifier('le partage arrêté : les versions restent', (await erin.etat(pE)).partage === null && (await erin.liste(pE)).length === 5 && (await erin.liste(pE)).every((x) => x.envoyee));
    const fiona = ordinateur('fiona');
    const pF = path.join(racine, 'Fiona', 'Chouettes.nuee');
    ecrire(pF, projetJson({ lignes: 1 }));
    await fiona.apresEnregistrement(pF);
    await fiona.jeton(hote, JETON);
    verifier('un dépôt absent : 404, dit', /No repository at this address/.test(await refus(() => fiona.partager(pF, url('absent.git')))));
    const ferme = await new Promise((ok) => { const x = http.createServer(); x.listen(0, '127.0.0.1', () => { const p = x.address().port; x.close(() => ok(p)); }); });
    await fiona.jeton(`127.0.0.1:${ferme}`, JETON);
    verifier('un serveur qui ne répond pas : impossible de le joindre', /Couldn’t reach 127\.0\.0\.1:\d+ \(/.test(await refus(() => fiona.partager(pF, `https://127.0.0.1:${ferme}/x.git`))));
    fiona.langue('fr');
    verifier('… en français', /Impossible de joindre/.test(await refus(() => fiona.listerDistant(`https://127.0.0.1:${ferme}/x.git`))));
    // the tokens: sent in the Authorization header only, nowhere else
    const formes = [JETON, JETON_LECTURE, FAUX_JETON].flatMap((x) => [x, encodeURIComponent(x), Buffer.from(x).toString('base64'),
      ...['x-access-token', 'oauth2'].map((u) => Buffer.from(`${u}:${x}`).toString('base64'))]);
    const trouve = (texte) => formes.filter((f) => texte.includes(f));
    verifier('les jetons envoyés au serveur dans l’en-tête Authorization', requetes.some((r) => r.authorization === `Basic ${Buffer.from(`oauth2:${JETON}`).toString('base64')}`));
    verifier('… dans aucune erreur', erreurs.length > 20 && trouve(erreurs.join('\n')).length === 0, trouve(erreurs.join('\n')));
    verifier('… dans aucun journal', trouve(journal.join('\n')).length === 0);
    verifier('… dans aucune adresse', trouve([...adresses, ...requetes.map((r) => r.url)].join('\n')).length === 0);
    verifier('… dans rien de ce que les fonctions rendent, ni les événements', trouve(JSON.stringify(retours)).length === 0);
    const configs = [dA, dB, path.dirname(pC), path.dirname(pD), path.dirname(pE)].flatMap((d) => fs.readdirSync(path.join(d, '.nuee-versions')).filter((n) => n.endsWith('.git'))
      .map((n) => contenu(path.join(d, '.nuee-versions', n, 'config')))).join('\n');
    const reglages = fs.readdirSync(racine).filter((f) => /^jetons-.*\.json$/.test(f)).map((f) => contenu(path.join(racine, f))).join('\n');
    verifier('… ni dans la configuration des dépôts, ni en clair dans les réglages', trouve(configs).length === 0 && trouve(reglages).length === 0 && reglages.length > 0);
  } finally {
    serveur.close();
  }
}

(async () => {
  let fin = 0;
  try {
    if (electron) { await electron.app.whenReady(); fetchReel = electron.net.fetch; }
    // (the checks make repositories: none may be in a parent folder)
    for (let d = racine; ; d = path.dirname(d)) {
      if (fs.existsSync(path.join(d, '.git'))) throw new Error(`${d} is a git repository: the checks would write there`);
      if (path.dirname(d) === d) break;
    }
    console.log(`Vérification des versions (${electron ? 'Electron, net.fetch' : 'Node'})`);
    await sansDepot();
    await automatiques();
    await eclaircissement();
    await partageReseau();
    if (gitPresent) {
      await depotUtilisateur();
      await echanges();
    } else console.log('  (git introuvable : le dépôt de l’utilisateur, le partage et les échanges ne sont pas vérifiés)');
  } catch (e) {
    verifier('erreur', false, e.stack);
  } finally {
    console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
    try { effacer(racine); } catch (e) { console.log(`  (${racine} pas supprimé : ${e.code} ${e.path})`); }
    fin = echecs ? 1 : 0;
    if (electron) electron.app.exit(fin); else process.exit(fin);
  }
})();
