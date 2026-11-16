// Checks versions.js (a project's versions kept with git, sent to and received from GitHub or
// GitLab) without Electron, on files in the system's temporary folder:
// - versions turned on in a fresh folder (a repository made there, branch main), and inside an
//   existing repository with other files (committed, staged, changed, untracked, ignored), which
//   stay as they were, git's own view of them included; two projects in one folder;
// - versions, the list, names, reading and restoring a version, an unchanged project refused;
//   paths with spaces and accents, and on Windows a UNC path (\\localhost\C$\…) when that share
//   answers;
// - sending, receiving, a fast-forward, a divergence and each choice, names exchanged both ways,
//   the refusals (other files differ, changes not saved, nothing to choose), against a smart-HTTP
//   git server started here (Node's http and the system git's http-backend; private repositories:
//   a token is needed, one may only read); skipped with a message when git isn't found;
// - the tokens: encrypted at rest, kept for the session where the system can't encrypt, never in
//   an error, a log, an address, or what the functions return.
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
const { creerVersions, magasinJetons, adresseDistante, nomRef } = require('../versions');

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
const projetJson = (n) => `${JSON.stringify({ nuee: { format: 2 }, nom: 'Suivi des hérons', comptages: Array.from({ length: n }, (_, i) => ({ site: `S${i}`, n: i * 3 })) })}\n`;

// The system git, without the computer's own settings (a Windows one sets core.autocrlf)
const configVide = path.join(racine, 'gitconfig');
fs.writeFileSync(configVide, '');
const envGit = { ...process.env, GIT_CONFIG_NOSYSTEM: '1', GIT_CONFIG_GLOBAL: configVide, GIT_TERMINAL_PROMPT: '0',
  GIT_AUTHOR_NAME: 'Bob Essai', GIT_AUTHOR_EMAIL: 'bob@example.org', GIT_COMMITTER_NAME: 'Bob Essai', GIT_COMMITTER_EMAIL: 'bob@example.org' };
const gitSys = (args, cwd) => execFileSync('git', ['-c', 'core.quotepath=false', ...args], { cwd, env: envGit, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
let gitPresent = true;
try { gitSys(['--version'], racine); } catch { gitPresent = false; }
const initSys = (dossier, ...options) => { gitSys(['init', '-q', ...options, dossier], racine); gitSys(['symbolic-ref', 'HEAD', 'refs/heads/main'], dossier); };

// A stand-in for Electron's safeStorage: "encrypted" by a XOR after the key's number, refused at
// will; a key changed since asks for encrypting again
const fauxChiffrement = ({ disponible = true, magasin = 'gnome_libsecret', cle = 1 } = {}) => ({
  isAsyncEncryptionAvailable: async () => disponible,
  getSelectedStorageBackend: () => magasin,
  encryptStringAsync: async (s) => Buffer.from([cle, ...Buffer.from(s, 'utf8').map((b) => b ^ 0xa5)]),
  decryptStringAsync: async (b) => ({ shouldReEncrypt: b[0] !== cle, result: Buffer.from(b.subarray(1).map((x) => x ^ 0xa5)).toString('utf8') }),
});
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

let nInstances = 0;
const versionsTemporaires = path.join(racine, 'versions-lues');
fs.mkdirSync(versionsTemporaires);
// One computer: its own settings (author, tokens) and language
function ordinateur({ langue = 'en', chiffrement = fauxChiffrement(), auteur = null } = {}) {
  let a = auteur;
  nInstances += 1;
  const fichierJetons = path.join(racine, `jetons-${nInstances}.json`);
  const v = creerVersions({
    fetch: fetchEssai, langue: () => langue, temporaire: () => versionsTemporaires,
    jetons: magasinJetons({ fichier: () => fichierJetons, safeStorage: chiffrement }),
    auteur: { lire: () => a, ecrire: (x) => { a = x; } },
  });
  return Object.assign(v, { fichierJetons, langue: (l) => { langue = l; } });
}

// ---------------------------------------------------------------- a smart-HTTP git server

// The system git's http-backend behind Node's http, as GitHub answers: every repository private
// (401 without a token, Basic authentication), 403 to send with the token that may only read
function serveurGit(depots) {
  const requetes = [];
  const serveur = http.createServer((req, res) => {
    requetes.push({ url: req.url, authorization: req.headers.authorization || '' });
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
  return new Promise((ok) => serveur.listen(0, '127.0.0.1', () => ok({ serveur, requetes, port: serveur.address().port })));
}

// ---------------------------------------------------------------- checks

async function local() {
  console.log('Un projet dans un dossier sans dépôt');
  const dossier = path.join(racine, 'Projets d’été', 'Camargue 2026');
  const projet = path.join(dossier, 'Suivi des hérons.nuee');
  ecrire(projet, projetJson(2));
  ecrire(path.join(dossier, 'donnees.csv'), 'site;n\nS1;3\n');
  const alice = ordinateur();

  const e0 = await alice.etat(projet);
  verifier('état avant : pas de dépôt, pas de versions', !e0.depot && !e0.suivi && e0.versions === 0 && e0.racine === null && e0.auteur === null, e0);
  verifier('activer sans auteur : refusé', /name and email/.test(await refus(() => alice.activer(projet))));
  verifier('un auteur sans adresse valable : refusé', /email address/.test(await refus(() => alice.auteur({ nom: 'Alice', email: 'pas une adresse' }))));
  await alice.auteur({ nom: '  Alice   Martin ', email: 'alice@example.org' });
  const a1 = await alice.activer(projet);
  const e1 = await alice.etat(projet);
  verifier('activé : un dépôt dans le dossier, branche main, une première version', e1.depot && e1.suivi && e1.versions === 1 && !e1.modifie
    && egal(e1.racine, dossier) && /^[0-9a-f]{40}$/.test(a1.oid) && fs.readFileSync(path.join(dossier, '.git', 'HEAD'), 'utf8').trim() === 'ref: refs/heads/main', e1);
  verifier('l’auteur gardé tel que donné, sans espaces de trop', JSON.stringify(e1.auteur) === JSON.stringify({ nom: 'Alice Martin', email: 'alice@example.org' }), e1.auteur);
  verifier('activer de nouveau : rien de plus', (await alice.activer(projet)).oid === a1.oid && (await alice.liste(projet)).length === 1);
  verifier('rien de changé : refusé', /hasn’t changed/.test(await refus(() => alice.enregistrerVersion(projet, 'Rien'))));
  alice.langue('fr');
  verifier('… en français', /n’a pas changé depuis sa dernière version/.test(await refus(() => alice.enregistrerVersion(projet, 'Rien'))));
  alice.langue('en');

  ecrire(projet, projetJson(3));
  verifier('le fichier changé : modifié', (await alice.etat(projet)).modifie === true);
  verifier('une version sans description : refusée', /Describe this version/.test(await refus(() => alice.enregistrerVersion(projet, '  '))));
  const v2 = await alice.enregistrerVersion(projet, 'Ajout des comptages de mai');
  ecrire(projet, projetJson(5));
  const v3 = await alice.enregistrerVersion(projet, 'Juin');
  const l = await alice.liste(projet);
  verifier('la liste : les versions, la plus récente d’abord, avec auteur et date', JSON.stringify(l.map((x) => x.description)) === JSON.stringify(['Juin', 'Ajout des comptages de mai', 'First version'])
    && l[0].oid === v3.oid && l[1].oid === v2.oid && l[2].oid === a1.oid && l.every((x) => x.auteur.nom === 'Alice Martin' && x.auteur.email === 'alice@example.org'
      && !Number.isNaN(Date.parse(x.date)) && /Z$/.test(x.date) && Array.isArray(x.noms)), l);
  const e2 = await alice.etat(projet);
  verifier('état : trois versions, rien de modifié', e2.versions === 3 && !e2.modifie && e2.distant === null && e2.avance === 0 && e2.retard === 0, e2);

  // names
  await alice.nommer(projet, v2.oid, 'Envoyé au comité');
  const n2 = await alice.nommer(projet, a1.oid, ' v1.0: final?  ');
  verifier('un nom déjà donné : refusé (quelle que soit sa casse)', /already given/.test(await refus(() => alice.nommer(projet, v3.oid, 'envoyé au COMITÉ'))));
  verifier('un nom vide : refusé', /Give the version a name/.test(await refus(() => alice.nommer(projet, v3.oid, ' ~~ '))));
  verifier('nommer une version inconnue : refusé', /isn’t in the project’s history/.test(await refus(() => alice.nommer(projet, '0123456789abcdef0123456789abcdef01234567', 'x'))));
  const ln = await alice.liste(projet);
  verifier('les noms dans la liste, tels que donnés', JSON.stringify(ln.map((x) => x.noms)) === JSON.stringify([[], ['Envoyé au comité'], ['v1.0: final?']]) && n2.nom === 'v1.0: final?', ln.map((x) => x.noms));
  if (gitPresent) {
    const tagsSys = gitSys(['tag', '-l', '-n1'], dossier).trim().split(/\r?\n/).map((x) => x.replace(/\s+/g, ' '));
    verifier('pour git : des étiquettes annotées, leur message le nom donné', JSON.stringify(tagsSys) === JSON.stringify(['Envoyé-au-comité Envoyé au comité', 'v1.0-final v1.0: final?']), tagsSys);
    const etranges = ['a b', 'x..y', 'fin.', '.cache', 'v@{1}', 'a/b\\c', 'ré:sumé?*[', 'trop.lock', 'CON', 'nul.txt', '  -tiret-  ', 'Ünïcødé ok', 'x'.repeat(300)];
    const invalides = etranges.map(nomRef).filter((n) => { try { gitSys(['check-ref-format', `refs/tags/${n}`], racine); return !n; } catch { return true; } });
    verifier('les noms faits refs : valables pour git (ceux sans lettre ni chiffre, vides : refusés)', invalides.length === 0 && nomRef('<"|>') === '' && nomRef('@') === '', invalides);
  }
  await alice.retirerNom(projet, 'v1.0: final?');
  verifier('un nom retiré', JSON.stringify((await alice.liste(projet)).map((x) => x.noms)) === JSON.stringify([[], ['Envoyé au comité'], []]));
  verifier('retirer un nom inconnu : refusé', /No version is named/.test(await refus(() => alice.retirerNom(projet, 'Inconnu'))));

  // reading and restoring
  const lu = await alice.lire(projet, v2.oid);
  verifier('lire : la version dans un fichier du même nom, dans un dossier temporaire neuf', path.basename(lu.fichier) === 'Suivi des hérons.nuee'
    && path.dirname(path.dirname(lu.fichier)) === versionsTemporaires && fs.readFileSync(lu.fichier, 'utf8') === projetJson(3), lu);
  const lu2 = await alice.lire(projet, v2.oid);
  verifier('lire deux fois : deux dossiers', path.dirname(lu2.fichier) !== path.dirname(lu.fichier));
  ecrire(projet, projetJson(9));
  verifier('restaurer avec des changements non enregistrés : refusé', /save a version first/.test(await refus(() => alice.restaurer(projet, v2.oid))));
  verifier('… le fichier laissé tel quel', fs.readFileSync(projet, 'utf8') === projetJson(9));
  await alice.enregistrerVersion(projet, 'Septembre');
  const r = await alice.restaurer(projet, v2.oid);
  const lr = await alice.liste(projet);
  verifier('restaurer : le fichier de cette version, et une version qui le dit', fs.readFileSync(projet, 'utf8') === projetJson(3) && lr[0].oid === r.oid
    && /^Restored the version of \d{1,2} \w+ \d{4}/.test(lr[0].description) && lr.length === 5 && !(await alice.etat(projet)).modifie, lr[0]);
  verifier('restaurer la version actuelle : refusé', /project as it is now/.test(await refus(() => alice.restaurer(projet, v2.oid))));
  alice.langue('fr');
  await alice.restaurer(projet, a1.oid);
  verifier('… en français', /^Version du \d{1,2} \S+ \d{4}.* restaurée$/.test((await alice.liste(projet))[0].description), (await alice.liste(projet))[0].description);
  alice.langue('en');
  verifier('un chemin relatif, un autre fichier : refusés', /full path/.test(await refus(() => alice.etat('projet.nuee')))
    && /full path/.test(await refus(() => alice.activer(path.join(dossier, 'donnees.csv')))));
  verifier('un projet absent : refusé', /isn’t there any more/.test(await refus(() => alice.etat(path.join(dossier, 'absent.nuee')))));
  // the path typed in another case: the same file in the repository (Windows, macOS)
  if (process.platform === 'win32') {
    const autreCasse = path.join(dossier.toUpperCase(), 'SUIVI DES HÉRONS.NUEE');
    verifier('le chemin écrit dans une autre casse : le même projet', (await alice.etat(autreCasse)).versions === 6 && (await alice.liste(autreCasse)).length === 6);
  }
  if (gitPresent) {
    gitSys(['fsck', '--strict', '--no-progress'], dossier);
    const statut = gitSys(['status', '--porcelain', '--untracked-files=all'], dossier).trim();
    const sujets = gitSys(['log', '--format=%s'], dossier).trim().split(/\r?\n/);
    verifier('pour git : un dépôt valable, le projet à jour dans l’index, l’autre fichier hors du dépôt', statut === '?? donnees.csv' && sujets.length === 6
      && sujets[1].startsWith('Restored the version of') && sujets.at(-1) === 'First version', { statut, sujets });
  }
  return { alice, projet, dossier };
}

// A repository of the user's own, with files of every kind: Nuée adds the project file alone
async function depotExistant() {
  console.log('Dans un dépôt existant, avec d’autres fichiers');
  const etude = path.join(racine, 'Étude existante');
  ecrire(path.join(etude, 'donnees.csv'), 'site,n\nA,1\n');
  ecrire(path.join(etude, 'scripts', 'analyse.R'), 'x <- read.csv("donnees.csv")\n');
  ecrire(path.join(etude, '.gitignore'), '*.nuee\nsortie/\n');
  initSys(etude);
  gitSys(['add', '.'], etude);
  gitSys(['commit', '-q', '-m', 'Données'], etude);
  ecrire(path.join(etude, 'donnees.csv'), 'site,n\nA,1\nB,2\n');
  gitSys(['add', 'donnees.csv'], etude);                                  // staged
  ecrire(path.join(etude, 'scripts', 'analyse.R'), 'x <- read.csv("donnees.csv")\nsummary(x)\n');  // changed, not staged
  ecrire(path.join(etude, 'notes.txt'), 'à faire\n');                    // untracked
  ecrire(path.join(etude, 'sortie', 'figure.svg'), '<svg/>');            // ignored
  const projet = path.join(etude, 'analyses', 'Modèle final.nuee');      // ignored too (*.nuee)
  ecrire(projet, projetJson(4));
  const autres = ['donnees.csv', 'scripts/analyse.R', '.gitignore', 'notes.txt', 'sortie/figure.svg'];
  const photo = () => {
    const fichiers = Object.fromEntries(autres.map((f) => [f, `${fs.readFileSync(path.join(etude, f), 'utf8')}|${fs.statSync(path.join(etude, f)).mtimeMs}`]));
    const statut = gitSys(['status', '--porcelain', '--ignored', '--untracked-files=all'], etude).split(/\r?\n/).filter((x) => x && !x.includes('.nuee')).sort();
    return JSON.stringify({ fichiers, statut, index: gitSys(['diff', '--cached'], etude), autresDansLIndex: gitSys(['ls-files', '-s'], etude).split(/\r?\n/).filter((x) => !x.includes('.nuee')) });
  };
  const avant = photo();
  const bob = ordinateur({ auteur: { nom: 'Bob', email: 'bob@example.org' } });
  await bob.activer(projet);
  const e = await bob.etat(projet);
  const changes = (de, a) => gitSys(['diff', '--name-only', de, a], etude).trim().split(/\r?\n/);
  verifier('activé : le dépôt de l’utilisateur, sa racine', e.suivi && e.versions === 1 && egal(e.racine, etude), e);
  verifier('la première version ne change que le projet', JSON.stringify(changes('HEAD~1', 'HEAD')) === JSON.stringify(['analyses/Modèle final.nuee']));
  verifier('les autres fichiers, l’index et ce que git en dit : comme avant', photo() === avant);
  verifier('le projet, ignoré par .gitignore, suivi et à jour pour git', gitSys(['status', '--porcelain', '--', 'analyses'], etude).trim() === '');
  ecrire(projet, projetJson(6));
  await bob.enregistrerVersion(projet, 'Modèle refait');
  verifier('une version : le projet seul, le reste comme avant', JSON.stringify(changes('HEAD~1', 'HEAD')) === JSON.stringify(['analyses/Modèle final.nuee']) && photo() === avant
    && gitSys(['show', 'HEAD~2:donnees.csv'], etude) === 'site,n\nA,1\n' && gitSys(['show', 'HEAD:donnees.csv'], etude) === 'site,n\nA,1\n');
  // a second project in the same folder: each list has its own versions
  const autre = path.join(etude, 'analyses', 'Autre analyse.nuee');
  ecrire(autre, projetJson(1));
  await bob.activer(autre);
  verifier('deux projets dans un dossier : chacun ses versions', (await bob.liste(autre)).length === 1 && (await bob.liste(projet)).length === 2
    && (await bob.etat(projet)).versions === 2 && photo() === avant);
  // a past state checked out: no branch to save on
  gitSys(['checkout', '-q', '--detach'], etude);
  ecrire(projet, projetJson(7));
  verifier('hors d’une branche : refusé', /isn’t on a branch/.test(await refus(() => bob.enregistrerVersion(projet, 'x'))));
  gitSys(['checkout', '-q', 'main'], etude);
  await bob.enregistrerVersion(projet, 'Après le retour sur main');
  verifier('revenu sur la branche : enregistré, le reste comme avant', (await bob.liste(projet)).length === 3 && photo() === avant);
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
  ecrire(projet, projetJson(2));
  const deviné = path.join(`${racine[0]}:\\`, 'localhost');
  const existait = fs.existsSync(deviné);
  const v = ordinateur({ auteur: { nom: 'Carole', email: 'carole@example.org' } });
  await v.activer(projet);
  ecrire(projet, projetJson(4));
  const v2 = await v.enregistrerVersion(projet, 'Sur le partage');
  const r = await v.restaurer(projet, (await v.liste(projet))[1].oid);
  const e = await v.etat(projet);
  verifier('UNC : le dépôt dans le dossier partagé, versions, liste, restauration', fs.existsSync(path.join(local, '.git', 'HEAD')) && e.versions === 3
    && e.racine.startsWith('\\\\localhost\\') && fs.readFileSync(path.join(local, 'Héronnière.nuee'), 'utf8') === projetJson(2) && !!v2.oid && !!r.oid, e);
  verifier('UNC : rien d’écrit sur le disque courant', existait || !fs.existsSync(deviné));
  if (gitPresent) gitSys(['fsck', '--strict', '--no-progress'], local);
}

// Two people sending and receiving through a server, then the refusals; every token kept out of
// what they could be seen in
async function echanges() {
  console.log('Envoyer et recevoir (un serveur git sur 127.0.0.1)');
  const depots = path.join(racine, 'serveur');
  fs.mkdirSync(depots);
  const nu = (nom) => { const d = path.join(depots, nom); gitSys(['init', '-q', '--bare', d], racine); gitSys(['symbolic-ref', 'HEAD', 'refs/heads/main'], d); return d; };
  const surServeur = (depot, ...args) => gitSys(['--git-dir', depot, ...args], racine).trim();
  const { serveur, requetes, port } = await serveurGit(depots);
  const hote = `127.0.0.1:${port}`;
  const url = (nom) => `https://${hote}/${nom}`;
  const retours = [];
  const garde = (x) => { retours.push(x); return x; };
  const contenu = (f) => fs.readFileSync(f, 'utf8');
  const descriptions = async (v, p) => (await v.liste(p)).map((x) => x.description);
  try {
    const herons = nu('herons.git');
    const dA = path.join(racine, 'Alice', 'Hérons 2026');
    const pA = path.join(dA, 'Suivi des hérons.nuee');
    ecrire(pA, projetJson(2));
    const alice = ordinateur({ auteur: { nom: 'Alice', email: 'alice@example.org' } });
    await alice.activer(pA);
    ecrire(pA, projetJson(3));
    const mai = await alice.enregistrerVersion(pA, 'Mai');
    await alice.nommer(pA, mai.oid, 'Soumis à la revue');

    // the address, then the token
    verifier('envoyer sans dépôt indiqué : refusé', /No repository on GitHub or GitLab/.test(await refus(() => alice.envoyer(pA))));
    verifier('une adresse http : refusée', /starting with https/.test(await refus(() => alice.distant(pA, `http://${hote}/herons.git`))));
    verifier('une adresse avec un jeton : refusée', /without a user name or token/.test(await refus(() => alice.distant(pA, `https://u:${JETON}@${hote}/herons.git`))));
    const adresse = await alice.distant(pA, url('herons'));
    verifier('l’adresse gardée, .git ajouté', adresse.url === url('herons.git') && adresse.hote === hote, adresse);
    const e0 = garde(await alice.etat(pA));
    verifier('état : le dépôt, pas de jeton, deux versions à envoyer', JSON.stringify(e0.distant) === JSON.stringify(adresse) && !e0.jeton && !e0.jetonSession && e0.avance === 2 && e0.retard === 0, e0);
    const sans = garde(await alice.verifierDistant(pA));
    verifier('vérifier sans jeton : le serveur en demande un', !sans.ok && /asks for a token/.test(sans.message), sans);
    verifier('envoyer sans jeton : refusé', /asks for a token/.test(await refus(() => alice.envoyer(pA))));
    verifier('un jeton de deux mots : refusé', /one word/.test(await refus(() => alice.jeton(hote, `${JETON} ${JETON}`))));
    garde(await alice.jeton(hote, FAUX_JETON));
    const faux = garde(await alice.verifierDistant(pA));
    verifier('un jeton faux : refusé', !faux.ok && /refused the token/.test(faux.message), faux);
    garde(await alice.jeton(hote, JETON_LECTURE));
    const lecture = garde(await alice.verifierDistant(pA));
    verifier('un jeton qui ne permet que de lire : 403', !lecture.ok && /may not write/.test(lecture.message), lecture);
    verifier('… envoyer refusé de même', /may not write/.test(await refus(() => alice.envoyer(pA))));
    const j = garde(await alice.jeton(` ${hote.toUpperCase()} `, JETON));
    const bon = garde(await alice.verifierDistant(pA));
    verifier('le bon jeton : vérifié', bon.ok && /answers/.test(bon.message) && j.jeton && !j.jetonSession, bon);

    // sending, receiving
    garde(await alice.envoyer(pA));
    verifier('envoyé : les versions et le nom sur le serveur', surServeur(herons, 'log', '--format=%s', 'main') === 'Mai\nFirst version'
      && /^Soumis-à-la-revue\s+Soumis à la revue$/.test(surServeur(herons, 'tag', '-l', '-n1')), surServeur(herons, 'tag', '-l', '-n1'));
    const e1 = garde(await alice.etat(pA));
    verifier('état : rien à envoyer ni à recevoir, un jeton', e1.avance === 0 && e1.retard === 0 && e1.jeton, e1);
    verifier('recevoir sans rien de neuf : à jour', garde(await alice.recevoir(pA)).resultat === 'a_jour');
    const dB = path.join(racine, 'Bob', 'Hérons');
    gitSys(['clone', '-q', herons, dB], racine);
    const pB = path.join(dB, 'Suivi des hérons.nuee');
    const bob = ordinateur({ auteur: { nom: 'Bob', email: 'bob@example.org' } });
    await bob.distant(pB, url('herons.git'));
    await bob.jeton(hote, JETON);
    verifier('Bob (un clone fait avec git) reçoit : à jour, les versions d’Alice et son nom', garde(await bob.recevoir(pB)).resultat === 'a_jour'
      && JSON.stringify((await bob.liste(pB)).map((v) => [v.description, v.noms])) === JSON.stringify([['Mai', ['Soumis à la revue']], ['First version', []]]));
    ecrire(pB, projetJson(4));
    await bob.enregistrerVersion(pB, 'Juin (Bob)');
    await bob.envoyer(pB);
    verifier('état d’Alice : son retard compté sur ce qu’elle a reçu (rien encore)', (await alice.etat(pA)).retard === 0);
    const ff = garde(await alice.recevoir(pA));
    verifier('Alice reçoit : avancée, le fichier de Bob', ff.resultat === 'avance' && contenu(pA) === projetJson(4) && (await descriptions(alice, pA))[0] === 'Juin (Bob)'
      && !(await alice.etat(pA)).modifie && gitSys(['status', '--porcelain'], dA).trim() === '', ff);

    // both sides with new versions
    ecrire(pA, projetJson(5));
    verifier('recevoir avec des changements non enregistrés : refusé', /save a version first/.test(await refus(() => alice.recevoir(pA))));
    await alice.enregistrerVersion(pA, 'Juillet (Alice)');
    ecrire(pB, projetJson(6));
    await bob.enregistrerVersion(pB, 'Juillet (Bob)');
    await bob.envoyer(pB);
    verifier('Alice envoie : refusé, à recevoir d’abord', /receive them first/.test(await refus(() => alice.envoyer(pA))));
    verifier('choisir avant de recevoir : rien à choisir', /nothing to choose/.test(await refus(() => alice.choisir(pA, 'leur'))));
    const div = garde(await alice.recevoir(pA));
    verifier('Alice reçoit : divergence, les versions de chacun depuis la dernière commune', div.resultat === 'diverge'
      && JSON.stringify(div.mien.map((v) => v.description)) === '["Juillet (Alice)"]' && JSON.stringify(div.leur.map((v) => v.description)) === '["Juillet (Bob)"]'
      && div.leur[0].auteur.nom === 'Bob' && contenu(pA) === projetJson(5) && (await alice.etat(pA)).retard === 1 && (await alice.etat(pA)).avance === 1, div);
    verifier('choisir autre chose que mien ou leur : refusé', /nothing to choose/.test(await refus(() => alice.choisir(pA, 'les deux'))));
    const leur = garde(await alice.choisir(pA, 'leur'));
    const parents = surServeur(herons, 'log', '-1', '--format=%P', 'main').split(' ');
    const lA = await descriptions(alice, pA);
    verifier('Alice prend la version de Bob : son fichier, une fusion des deux histoires, envoyée', contenu(pA) === projetJson(6) && parents.length === 2
      && surServeur(herons, 'rev-parse', 'main') === leur.oid && lA[0] === 'Took the version received rather than mine' && lA.includes('Juillet (Alice)')
      && lA.includes('Juillet (Bob)') && !(await alice.etat(pA)).modifie && gitSys(['status', '--porcelain'], dA).trim() === '', { parents, lA });
    verifier('choisir de nouveau : rien à choisir', /nothing to choose/.test(await refus(() => alice.choisir(pA, 'mien'))));
    verifier('Bob reçoit la fusion : avancé, son fichier tel quel', garde(await bob.recevoir(pB)).resultat === 'avance' && contenu(pB) === projetJson(6));
    ecrire(pA, projetJson(7));
    await alice.enregistrerVersion(pA, 'Août (Alice)');
    ecrire(pB, projetJson(8));
    await bob.enregistrerVersion(pB, 'Août (Bob)');
    await bob.envoyer(pB);
    verifier('une autre divergence', garde(await alice.recevoir(pA)).resultat === 'diverge');
    await alice.choisir(pA, 'mien');
    verifier('Alice garde sa version : son fichier, envoyée', contenu(pA) === projetJson(7) && (await descriptions(alice, pA))[0] === 'Kept my version rather than the one received'
      && surServeur(herons, 'rev-parse', 'main') === gitSys(['rev-parse', 'HEAD'], dA).trim());
    verifier('Bob reçoit : le fichier d’Alice', garde(await bob.recevoir(pB)).resultat === 'avance' && contenu(pB) === projetJson(7) && !(await bob.etat(pB)).modifie);

    // names, both ways
    await bob.retirerNom(pB, 'Soumis à la revue');
    verifier('un nom retiré, pas encore envoyé : recevoir ne le ramène pas', (await bob.recevoir(pB)).resultat === 'a_jour' && (await bob.liste(pB)).every((v) => !v.noms.length));
    await bob.envoyer(pB);
    verifier('envoyé : le nom retiré du serveur', surServeur(herons, 'tag', '-l') === '');
    await bob.nommer(pB, (await bob.liste(pB))[0].oid, 'Version finale');
    await bob.envoyer(pB);
    await alice.nommer(pA, (await alice.liste(pA))[1].oid, 'Brouillon');
    await alice.recevoir(pA);
    const nomsA = (await alice.liste(pA)).flatMap((v) => v.noms).sort();
    verifier('Alice reçoit : le nom retiré par Bob retiré, le nouveau reçu, le sien (pas envoyé) gardé', JSON.stringify(nomsA) === '["Brouillon","Version finale"]', nomsA);
    await alice.envoyer(pA);
    verifier('Alice envoie : son nom sur le serveur', surServeur(herons, 'tag', '-l').split(/\r?\n/).sort().join() === 'Brouillon,Version-finale', surServeur(herons, 'tag', '-l'));
    await bob.recevoir(pB);
    await bob.retirerNom(pB, 'Brouillon');
    await bob.envoyer(pB);
    await alice.retirerNom(pA, 'Brouillon');
    await alice.nommer(pA, (await alice.liste(pA))[0].oid, 'Brouillon');
    await alice.recevoir(pA);
    verifier('un nom retiré là-bas mais redonné ici, à une autre version : gardé', (await alice.liste(pA))[0].noms.includes('Brouillon'), (await alice.liste(pA)).map((v) => v.noms));
    await alice.envoyer(pA);
    verifier('… et renvoyé', surServeur(herons, 'tag', '-l').split(/\r?\n/).includes('Brouillon') && surServeur(herons, 'rev-parse', 'Brouillon^{}') === gitSys(['rev-parse', 'HEAD'], dA).trim());

    // another file in the repository: not Nuée's to bring together
    ecrire(path.join(dB, 'README.md'), '# Hérons\n');
    gitSys(['add', 'README.md'], dB);
    gitSys(['commit', '-q', '-m', 'README'], dB);
    gitSys(['push', '-q', herons, 'main'], dB);
    const teteA = gitSys(['rev-parse', 'HEAD'], dA).trim();
    const autres = await refus(() => alice.recevoir(pA));
    verifier('un autre fichier reçu (README.md) : refusé, nommé, rien de changé', /other files.*README\.md.*use git directly/.test(autres) && contenu(pA) === projetJson(7)
      && gitSys(['rev-parse', 'HEAD'], dA).trim() === teteA, autres);

    // a repository GitLab made with a README; tokens kept for the session only
    const gitlab = nu('gitlab.git');
    const initial = path.join(racine, 'initial');
    gitSys(['clone', '-q', gitlab, initial], racine);
    ecrire(path.join(initial, 'README.md'), '# Projet\n');
    gitSys(['add', '.'], initial);
    gitSys(['commit', '-q', '-m', 'Initial commit'], initial);
    gitSys(['push', '-q', 'origin', 'HEAD:main'], initial);
    const pC = path.join(racine, 'Carole', 'Flamants.nuee');
    ecrire(pC, projetJson(1));
    const carole = ordinateur({ auteur: { nom: 'Carole', email: 'carole@example.org' }, chiffrement: fauxChiffrement({ disponible: false }) });
    await carole.activer(pC);
    await carole.distant(pC, url('gitlab.git'));
    const jC = garde(await carole.jeton(hote, JETON));
    const eC = garde(await carole.etat(pC));
    verifier('sans chiffrement : le jeton pour la session, dit par etat', jC.jetonSession && eC.jeton && eC.jetonSession && !fs.existsSync(carole.fichierJetons), eC);
    verifier('un dépôt créé avec un README : envoyer refusé, à recevoir d’abord', /receive them first/.test(await refus(() => carole.envoyer(pC))));
    verifier('… recevoir refusé, le README nommé', /README\.md/.test(await refus(() => carole.recevoir(pC))) && contenu(pC) === projetJson(1));

    // the same project started apart (a copy of the file): two histories with nothing in common
    nu('commun.git');
    const pE = path.join(racine, 'Erin', 'Suivi des hérons.nuee');
    const pD = path.join(racine, 'Dan', 'Suivi des hérons.nuee');
    ecrire(pE, projetJson(2));
    ecrire(pD, projetJson(3));
    const erin = ordinateur({ auteur: { nom: 'Erin', email: 'erin@example.org' } });
    const dan = ordinateur({ auteur: { nom: 'Dan', email: 'dan@example.org' }, langue: 'fr' });
    for (const [v, p] of [[erin, pE], [dan, pD]]) { await v.activer(p); await v.distant(p, url('commun.git')); await v.jeton(hote, JETON); }
    await erin.envoyer(pE);
    const sep = garde(await dan.recevoir(pD));
    verifier('deux histoires sans version commune : divergence, toutes les versions de chacun', sep.resultat === 'diverge' && sep.mien.length === 1 && sep.leur.length === 1
      && sep.mien[0].description === 'Première version' && sep.leur[0].auteur.nom === 'Erin', sep);
    await dan.choisir(pD, 'leur');
    verifier('… prendre l’autre : son fichier, envoyé ; Erin reçoit la fusion', contenu(pD) === projetJson(2) && garde(await erin.recevoir(pE)).resultat === 'avance'
      && contenu(pE) === projetJson(2) && (await descriptions(erin, pE))[0] === 'Version reçue prise plutôt que la mienne');

    // a repository that isn't there, a server that doesn't answer
    const pF = path.join(racine, 'Fiona', 'Chouettes.nuee');
    ecrire(pF, projetJson(1));
    const fiona = ordinateur({ auteur: { nom: 'Fiona', email: 'fiona@example.org' } });
    await fiona.activer(pF);
    await fiona.distant(pF, url('absent.git'));
    await fiona.jeton(hote, JETON);
    const absent = garde(await fiona.verifierDistant(pF));
    verifier('un dépôt absent : 404, dit', !absent.ok && /No repository at this address/.test(absent.message), absent);
    const ferme = await new Promise((ok) => { const x = http.createServer(); x.listen(0, '127.0.0.1', () => { const p = x.address().port; x.close(() => ok(p)); }); });
    await fiona.distant(pF, `https://127.0.0.1:${ferme}/x.git`);
    await fiona.jeton(`127.0.0.1:${ferme}`, JETON);
    verifier('un serveur qui ne répond pas : impossible de le joindre', /Couldn’t reach 127\.0\.0\.1:\d+ \(/.test(await refus(() => fiona.envoyer(pF))));
    fiona.langue('fr');
    verifier('… en français', /Impossible de joindre/.test(await refus(() => fiona.recevoir(pF))));

    // the tokens: sent in the Authorization header only, nowhere else
    const formes = [JETON, JETON_LECTURE, FAUX_JETON].flatMap((x) => [x, encodeURIComponent(x), Buffer.from(x).toString('base64'),
      ...['x-access-token', 'oauth2'].map((u) => Buffer.from(`${u}:${x}`).toString('base64'))]);
    const trouve = (texte) => formes.filter((f) => texte.includes(f));
    verifier('les jetons envoyés au serveur dans l’en-tête Authorization', requetes.some((r) => r.authorization === `Basic ${Buffer.from(`oauth2:${JETON}`).toString('base64')}`));
    verifier('… dans aucune erreur', erreurs.length > 20 && trouve(erreurs.join('\n')).length === 0, trouve(erreurs.join('\n')));
    verifier('… dans aucun journal', trouve(journal.join('\n')).length === 0);
    verifier('… dans aucune adresse', trouve([...adresses, ...requetes.map((r) => r.url)].join('\n')).length === 0);
    verifier('… dans rien de ce que les fonctions rendent', trouve(JSON.stringify(retours)).length === 0);
    const configs = [dA, dB, path.dirname(pC), path.dirname(pE), path.dirname(pD), path.dirname(pF)].map((d) => contenu(path.join(d, '.git', 'config'))).join('\n');
    const reglages = fs.readdirSync(racine).filter((f) => /^jetons-.*\.json$/.test(f)).map((f) => contenu(path.join(racine, f))).join('\n');
    verifier('… ni dans la configuration des dépôts, ni en clair dans les réglages', trouve(configs).length === 0 && trouve(reglages).length === 0 && reglages.length > 0);
  } finally {
    serveur.close();
  }
}

async function jetons() {
  console.log('Les jetons');
  const fichier = path.join(racine, 'jetons-magasin.json');
  const m = magasinJetons({ fichier: () => fichier, safeStorage: fauxChiffrement() });
  await m.ecrire('github.com', JETON);
  verifier('chiffré dans le fichier des réglages', fs.existsSync(fichier) && !fs.readFileSync(fichier, 'utf8').includes(JETON) && await m.lire('github.com') === JETON
    && await m.present('github.com') && !(await m.seulementSession()));
  await m.ecrire('gitlab.example.org', JETON_LECTURE);
  await m.ecrire('github.com', '');
  verifier('un jeton vide le retire, les autres restent', !(await m.present('github.com')) && await m.lire('github.com') === null
    && await m.lire('gitlab.example.org') === JETON_LECTURE && !Object.keys(JSON.parse(fs.readFileSync(fichier, 'utf8'))).includes('github.com'));
  verifier('un serveur nommé comme une propriété d’objet : sans jeton', !(await m.present('constructor')) && await m.lire('constructor') === null);
  const nouvelleCle = magasinJetons({ fichier: () => fichier, safeStorage: fauxChiffrement({ cle: 2 }) });
  verifier('la clé du système changée : lu, puis chiffré de nouveau', await nouvelleCle.lire('gitlab.example.org') === JETON_LECTURE
    && Buffer.from(JSON.parse(fs.readFileSync(fichier, 'utf8'))['gitlab.example.org'], 'base64')[0] === 2);
  const fichierS = path.join(racine, 'jetons-session.json');
  const s = magasinJetons({ fichier: () => fichierS, safeStorage: fauxChiffrement({ disponible: false }) });
  await s.ecrire('github.com', JETON);
  verifier('sans chiffrement : gardé pour la session, rien d’écrit', await s.seulementSession() && await s.present('github.com') && await s.lire('github.com') === JETON
    && !fs.existsSync(fichierS));
  const autre = magasinJetons({ fichier: () => fichierS, safeStorage: fauxChiffrement({ disponible: false }) });
  verifier('… et oublié au lancement suivant', !(await autre.present('github.com')));
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
    verifier(`safeStorage du système (${session ? 'session' : 'chiffré'})`, await e.lire('github.com') === JETON && (session || !fs.readFileSync(fichierE, 'utf8').includes(JETON)));
  }
  verifier('les adresses : https seulement, sans identifiants, la page d’un dépôt ramenée au dépôt', JSON.stringify([
    'https://github.com/4rtefakt/nuee', 'https://github.com/4rtefakt/nuee/tree/main/app?x=1#l', 'https://GitLab.com/groupe/sous-groupe/projet/-/tree/main',
    'https://gitlab.inrae.fr:8443/eco/suivi.git/', 'http://github.com/a/b', 'git@github.com:a/b.git', 'https://x-access-token:abc@github.com/a/b', 'https://github.com/a', ' '].map(adresseDistante))
    === JSON.stringify([{ url: 'https://github.com/4rtefakt/nuee.git', hote: 'github.com' }, { url: 'https://github.com/4rtefakt/nuee.git', hote: 'github.com' },
      { url: 'https://gitlab.com/groupe/sous-groupe/projet.git', hote: 'gitlab.com' }, { url: 'https://gitlab.inrae.fr:8443/eco/suivi.git', hote: 'gitlab.inrae.fr:8443' },
      { refus: 'adresse' }, { refus: 'adresse' }, { refus: 'identifiants' }, { refus: 'adresse' }, { refus: 'adresse' }]));
}

(async () => {
  let fin = 0;
  try {
    if (electron) { await electron.app.whenReady(); fetchReel = electron.net.fetch; }
    // (the checks make repositories: none may be in a parent folder, where they would go)
    for (let d = racine; ; d = path.dirname(d)) {
      if (fs.existsSync(path.join(d, '.git'))) throw new Error(`${d} is a git repository: the checks would write there`);
      if (path.dirname(d) === d) break;
    }
    console.log(`Vérification des versions (${electron ? 'Electron, net.fetch' : 'Node'})`);
    await jetons();
    await local();
    if (gitPresent) await depotExistant();
    await partageReseau();
    if (gitPresent) await echanges();
    else console.log('  (git introuvable : le dépôt existant, l’envoi et la réception ne sont pas vérifiés)');
  } catch (e) {
    verifier('erreur', false, e.stack);
  } finally {
    console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
    try { effacer(racine); } catch (e) { console.log(`  (${racine} pas supprimé : ${e.code} ${e.path})`); }
    fin = echecs ? 1 : 0;
    if (electron) electron.app.exit(fin); else process.exit(fin);
  }
})();
