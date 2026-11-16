// Checks comptes.js (signing in to GitHub and GitLab) without Electron, against a stand-in of their
// servers on this computer (tests/serveur_comptes.js): the real ones are never asked anything.
// - the device flow: approved, refused, expired (said by the server, or the code's time over),
//   slow_down (5 s more between two polls each time), cancelled, a new code replacing one under way;
//   GitHub's answers (errors in 200) and GitLab's (errors in 400, the code filled in its page); not
//   set up (a placeholder id), an application the server doesn't know, the device flow not enabled,
//   a GitLab without it, a self-hosted GitLab's id from the settings; the page opened: https, the
//   server's own;
// - the user API: the account and its picture (a data: URL); the server not answering; a token
//   refused or revoked; a token given by hand, or in Versions; a GitLab token renewed with its
//   refresh token before it expires (and at start-up), then refused;
// - a repository created (private, named after the project), a name taken; a colleague invited
//   (GitHub: an invitation, GitLab: a member as Developer), twice, unknown, an organization, oneself;
// - `git credential fill` with a stand-in git first on the PATH: what it is given (the host, no
//   terminal prompt, no askpass), its token checked and kept, nothing, refused, too slow (stopped
//   with what it started), cancelled, git absent, asked again without a window when its token is
//   refused, a GitLab token for git only;
// - signing out;
// - the tokens, refresh tokens and device codes: in no message, return value, log line, address,
//   page opened, setting or file in clear; the token sent to the API only.
//   node tests/essai_comptes.js
//   npx electron tests/essai_comptes.js    (the requests through Chromium's network stack, net.fetch)
const { execFileSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');
const electron = process.versions.electron && !process.env.ELECTRON_RUN_AS_NODE ? require('electron') : null;
const { creerComptes, nomDepot, titreGitlab } = require('../comptes');
const { creerVersions, magasinJetons } = require('../versions');
const { serveurComptes } = require('./serveur_comptes');

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${typeof detail === 'string' ? detail : JSON.stringify(detail)}`}`);
  if (!ok) echecs += 1;
};
// every message thrown, value returned and line logged, for the secrets' check
const messages = [];
const retours = [];
const journal = [];
for (const k of ['log', 'info', 'warn', 'error', 'debug']) {
  const avant = console[k];
  console[k] = (...a) => { journal.push(a.map(String).join(' ')); avant.apply(console, a); };
}
async function refus(f) {
  try { await f(); return null; } catch (e) { return { message: e.message, code: e.code || null }; }
}
const attendreMs = (ms) => new Promise((ok) => setTimeout(ok, ms));
async function jusqua(f, ms = 5000) {
  const fin = Date.now() + ms;
  for (;;) {
    if (await f()) return true;
    if (Date.now() > fin) return false;
    await attendreMs(20);
  }
}
const vivant = (pid) => { try { process.kill(pid, 0); return true; } catch (e) { return e.code === 'EPERM'; } };

const racine = fs.mkdtempSync(path.join(os.tmpdir(), 'essai-comptes-'));

// A stand-in for Electron's safeStorage (as tests/essai_versions.js): "encrypted" by a XOR
const fauxChiffrement = () => ({
  isAsyncEncryptionAvailable: async () => true,
  getSelectedStorageBackend: () => 'gnome_libsecret',
  encryptStringAsync: async (s) => Buffer.from([1, ...Buffer.from(s, 'utf8').map((b) => b ^ 0xa5)]),
  decryptStringAsync: async (b) => ({ shouldReEncrypt: false, result: Buffer.from(b.subarray(1).map((x) => x ^ 0xa5)).toString('utf8') }),
});

let simule;  // the stand-in of GitHub and GitLab
let ids;     // Nuée's client ids there
let fetchReel = fetch;
// the clock the polls are timed by: each wait between two polls is recorded and moves it on at once
let decalage = 0;
const maintenant = () => Date.now() + decalage;
let attentes = [];
const pause = (ms) => { attentes.push(ms); decalage += ms; return attendreMs(1); };
const ouverts = [];  // the pages opened in the browser

// One computer: its stores, settings and language; comptes.js with the versions layer's own jeton()
let nOrdinateurs = 0;
function ordinateur({ idsClients = null, langue = 'en' } = {}) {
  nOrdinateurs += 1;
  const dossier = path.join(racine, `ordinateur-${nOrdinateurs}`);
  fs.mkdirSync(dossier);
  const chiffrement = fauxChiffrement();
  const jetons = magasinJetons({ fichier: () => path.join(dossier, 'jetons.json'), safeStorage: chiffrement });
  const renouvellements = magasinJetons({ fichier: () => path.join(dossier, 'jetons-renouvellement.json'), safeStorage: chiffrement });
  const fetchSimule = simule.fetch((u, i) => fetchReel(u, i));
  const versions = creerVersions({ fetch: fetchSimule, jetons, langue: () => langue, auteur: { lire: () => null, ecrire: () => {} }, temporaire: () => dossier });
  let r = {};
  const c = creerComptes({
    fetch: fetchSimule, jeton: versions.jeton, jetons, renouvellements,
    reglages: { lire: () => r, ecrire: (x) => { r = JSON.parse(JSON.stringify(x)); fs.writeFileSync(path.join(dossier, 'reglages.json'), JSON.stringify(x, null, 1)); } },
    ouvrirNavigateur: async (url) => { ouverts.push(url); },
    langue: () => langue, ids: idsClients || ids, maintenant, pause, delaiGit: 1500, delaiGitDiscret: 1500,
  });
  const espion = Object.fromEntries(Object.entries(c).map(([nom, f]) => [nom, async (...a) => {
    try { const v = await f(...a); retours.push(JSON.stringify(v) ?? ''); return v; } catch (e) { messages.push(`${e.message}\n${e.stack}`); throw e; }
  }]));
  return { ...espion, jetons, renouvellements, dossier, reglages: () => r, jetonVersions: versions.jeton, langue: (l) => { langue = l; } };
}

const requetes = (hote, chemin = null) => simule.etat.requetes.filter((q) => q.hote === hote && (chemin === null || q.chemin.split('?')[0] === chemin));
const formulaire = (q) => Object.fromEntries(new URLSearchParams(q ? q.corps : ''));

// ---------------------------------------------------------------- not set up

async function nonRegle() {
  const c = ordinateur({ idsClients: { 'github.com': '<to fill>', 'gitlab.com': '<to fill>' } });
  const s = await c.serveur('github.com');
  verifier('pas prêt : serveur() le dit', s.connexion === false && s.reglable === false && s.fournisseur === 'github', s);
  const avant = simule.etat.requetes.length;
  const e = await refus(() => c.commencerConnexion('github.com'));
  verifier('pas prêt : GitHub refusé, en le disant', e && e.code === 'non_regle' && /Signing in with GitHub isn’t set up in this copy of Nuée yet/.test(e.message), e);
  const e2 = await refus(() => c.commencerConnexion('gitlab.com'));
  verifier('pas prêt : GitLab de même', e2 && e2.code === 'non_regle' && /Signing in with GitLab isn’t set up/.test(e2.message), e2);
  verifier('pas prêt : rien demandé aux serveurs', simule.etat.requetes.length === avant);
  c.langue('fr');
  const e3 = await refus(() => c.commencerConnexion('github.com'));
  verifier('pas prêt : en français', e3 && /n’est pas encore prête dans cette copie de Nuée/.test(e3.message), e3);
  c.langue('en');
  verifier('un nom de serveur invalide refusé', (await refus(() => c.serveur('https://github.com/')))?.code === 'serveur');

  // a self-hosted GitLab: its application ID from the settings
  const s2 = await c.serveur('gitlab.example.org');
  verifier('GitLab de l’institut sans identifiant : à régler', s2.connexion === false && s2.reglable === true && s2.fournisseur === 'gitlab', s2);
  const e4 = await refus(() => c.commencerConnexion('gitlab.example.org'));
  verifier('… refusé : demander à son administrateur', e4 && e4.code === 'non_regle' && /ask its administrator/.test(e4.message), e4);
  verifier('un identifiant d’application invalide refusé', (await refus(() => c.reglerServeur('gitlab.example.org', 'deux mots')))?.code === 'id');
  verifier('celui de github.com n’est pas réglable', (await refus(() => c.reglerServeur('github.com', 'Ov23liAutreIdentifiant')))?.code === 'serveur');
  const s3 = await c.reglerServeur('gitlab.example.org', simule.etat.ids['gitlab.example.org']);
  verifier('identifiant donné : connexion possible, gardé dans les réglages', s3.connexion === true
    && c.reglages().serveurs['gitlab.example.org'] === simule.etat.ids['gitlab.example.org'], s3);
  const d = await c.commencerConnexion('gitlab.example.org');
  simule.approuver(d.code);
  const r = await c.attendreConnexion('gitlab.example.org');
  verifier('GitLab de l’institut : connecté, par son propre serveur', r.ok && r.compte.login === 'chercheuse' && r.compte.nom === 'Claire Chercheuse'
    && formulaire(requetes('gitlab.example.org', '/oauth/authorize_device').pop()).client_id === simule.etat.ids['gitlab.example.org'], r);
  const s4 = await c.reglerServeur('gitlab.example.org', '');
  verifier('identifiant oublié', s4.connexion === false && !Object.hasOwn(c.reglages().serveurs, 'gitlab.example.org'));
}

// ---------------------------------------------------------------- GitHub's device flow

async function connexionGithub() {
  const c = ordinateur();
  simule.etat.prochain = ['attente', 'attente'];
  attentes = [];
  const d = await c.commencerConnexion('github.com');
  const demande = requetes('github.com', '/login/device/code').pop();
  verifier('GitHub : le code demandé (POST, formulaire, identifiant, portée repo)', demande.methode === 'POST'
    && formulaire(demande).client_id === ids['github.com'] && formulaire(demande).scope === 'repo', formulaire(demande));
  verifier('GitHub : le code, la page, sa fin', /^[0-9A-F]{4}-[0-9A-F]{4}$/.test(d.code) && d.adresse === 'https://github.com/login/device'
    && Math.abs(d.expire - (maintenant() + 900000)) < 5000 && Object.keys(d).sort().join() === 'adresse,code,expire', d);
  await c.ouvrirPage('github.com');
  verifier('la page de GitHub ouverte', ouverts[ouverts.length - 1] === 'https://github.com/login/device', ouverts);
  simule.approuver(d.code);
  const r = await c.attendreConnexion('github.com');
  verifier('GitHub : connecté après deux attentes', r.ok && r.compte.connecte && r.compte.login === 'octocat' && r.compte.nom === 'Mona Octocat' && r.compte.source === 'connexion', r.compte);
  verifier('… son image en data: PNG', /^data:image\/png;base64,/.test(r.compte.avatar) && Buffer.from(r.compte.avatar.split(',')[1], 'base64').subarray(1, 4).toString() === 'PNG');
  verifier('… GitHub demandé toutes les 5 s', JSON.stringify(attentes) === '[5000,5000,5000]', attentes);
  const sondages = requetes('github.com', '/login/oauth/access_token');
  verifier('… chaque fois : device_code, client_id, grant_type (formulaire)', sondages.length === 3 && sondages.every((q) => {
    const f = formulaire(q);
    return f.grant_type === 'urn:ietf:params:oauth:grant-type:device_code' && f.client_id === ids['github.com'] && f.device_code && q.methode === 'POST';
  }));
  const jeton = await c.jetons.lire('github.com');
  verifier('… le jeton gardé par la couche des versions', /^gho_/.test(jeton) && simule.etat.jetons.has(jeton));
  verifier('… le compte noté, sans secret', JSON.stringify(c.reglages().comptes['github.com']) === JSON.stringify({ source: 'connexion', login: 'octocat', nom: 'Mona Octocat', expire: null }),
    c.reglages().comptes);
  const images = requetes('avatars.githubusercontent.com');
  verifier('… l’image demandée sans jeton, en petit', images.length === 1 && !images[0].authorization && /[?&]s=96/.test(images[0].chemin), images);
  verifier('plus de connexion en cours', (await refus(() => c.ouvrirPage('github.com')))?.code === 'aucune'
    && (await refus(() => c.attendreConnexion('github.com')))?.code === 'aucune');

  // slow_down: 5 s more each time (GitHub also gives the new interval)
  simule.etat.prochain = ['lent', 'attente', 'lent', 'ok'];
  attentes = [];
  await c.commencerConnexion('github.com');
  const r2 = await c.attendreConnexion('github.com');
  verifier('slow_down : 10 s, puis 15 s entre deux demandes', r2.ok && JSON.stringify(attentes) === '[5000,10000,10000,15000]', attentes);

  const c2 = ordinateur();
  simule.etat.prochain = ['attente', 'refuse'];
  await c2.commencerConnexion('github.com');
  const r3 = await c2.attendreConnexion('github.com');
  verifier('refusé sur la page de GitHub', !r3.ok && r3.raison === 'refusee' && !(await c2.jetons.present('github.com')), r3);
  simule.etat.prochain = ['attente', 'expire'];
  await c2.commencerConnexion('github.com');
  const r4 = await c2.attendreConnexion('github.com');
  verifier('expiré (dit par GitHub)', !r4.ok && r4.raison === 'expiree', r4);

  // cancelled while waiting; asked twice, the same wait
  const d5 = await c2.commencerConnexion('github.com');
  const attente = c2.attendreConnexion('github.com');
  const memeAttente = c2.attendreConnexion('github.com');
  await jusqua(() => [...simule.etat.codes.values()].some((x) => x.code === d5.code && x.sondages >= 1));
  await c2.annulerConnexion('github.com');
  verifier('annulé', (await attente).raison === 'annulee' && (await memeAttente).raison === 'annulee');
  const d6 = await c2.commencerConnexion('github.com');
  const attente6 = c2.attendreConnexion('github.com');
  const d7 = await c2.commencerConnexion('github.com');
  verifier('un nouveau code remplace celui en cours', (await attente6).raison === 'annulee' && d7.code !== d6.code);
  await c2.annulerConnexion('github.com');
  verifier('rien de gardé sans accord', !(await c2.jetons.present('github.com')) && !c2.reglages().comptes);

  simule.etat.fluxDesactive = true;
  const e = await refus(() => c2.commencerConnexion('github.com'));
  verifier('Device Flow pas activé dans l’application GitHub : dit', e?.code === 'non_regle' && /Enable Device Flow/.test(e.message), e);
  simule.etat.fluxDesactive = false;
  const c3 = ordinateur({ idsClients: { 'github.com': 'Ov23liPasLeBonId00', 'gitlab.com': ids['gitlab.com'] } });
  const e2 = await refus(() => c3.commencerConnexion('github.com'));
  verifier('un identifiant que GitHub ne connaît pas : dit', e2?.code === 'non_regle' && /doesn’t recognise Nuée’s application/.test(e2.message), e2);
  return c;
}

// ---------------------------------------------------------------- GitLab's device flow, its renewal

async function connexionGitlab() {
  const c = ordinateur();
  simule.etat.prochain = ['attente', 'lent'];
  attentes = [];
  const d = await c.commencerConnexion('gitlab.com');
  const q = requetes('gitlab.com', '/oauth/authorize_device').pop();
  verifier('GitLab : le code demandé avec api read_user write_repository', formulaire(q).client_id === ids['gitlab.com'] && formulaire(q).scope === 'api read_user write_repository', formulaire(q));
  verifier('GitLab : le code, sa page', /^[0-9A-F]{8}$/.test(d.code) && d.adresse === 'https://gitlab.com/oauth/device' && Math.abs(d.expire - (maintenant() + 300000)) < 5000, d);
  await c.ouvrirPage('gitlab.com');
  verifier('… la page ouverte le code déjà rempli', ouverts[ouverts.length - 1] === `https://gitlab.com/oauth/device?user_code=${d.code}`, ouverts);
  simule.approuver(d.code);
  const r = await c.attendreConnexion('gitlab.com');
  verifier('GitLab : connecté (ses refus en 400 ; slow_down sans intervalle : 5 s de plus)', r.ok && r.compte.login === 'alice' && r.compte.nom === 'Alice Durand'
    && JSON.stringify(attentes) === '[5000,5000,10000]', { compte: r.compte, attentes });
  verifier('… son image (depuis gitlab.com, sans jeton)', /^data:image\/png;base64,/.test(r.compte.avatar) && requetes('gitlab.com').filter((x) => x.chemin.startsWith('/uploads/')).every((x) => !x.authorization));
  const n = c.reglages().comptes['gitlab.com'];
  verifier('… sa fin notée (deux heures), et l’identifiant qui le renouvelle', n.source === 'connexion' && Math.abs(n.expire - (maintenant() + 7200000)) < 5000 && n.id === ids['gitlab.com'], n);
  const premier = await c.jetons.lire('gitlab.com');
  const rafraichir = await c.renouvellements.lire('gitlab.com');
  verifier('… le jeton de renouvellement gardé à part, chiffré', !!rafraichir && rafraichir !== premier
    && !fs.readFileSync(path.join(c.dossier, 'jetons-renouvellement.json'), 'utf8').includes(rafraichir));

  // two hours later: renewed before the account is asked
  decalage += 7200000;
  const avant = requetes('gitlab.com', '/oauth/token').length;
  const k = await c.compte('gitlab.com');
  const renouvellement = requetes('gitlab.com', '/oauth/token').slice(avant);
  verifier('deux heures plus tard : renouvelé (refresh_token, client_id), puis le compte lu', k.connecte && k.login === 'alice' && renouvellement.length === 1
    && formulaire(renouvellement[0]).grant_type === 'refresh_token' && formulaire(renouvellement[0]).client_id === ids['gitlab.com']
    && formulaire(renouvellement[0]).refresh_token === rafraichir, { k, renouvellement: renouvellement.map(formulaire).map((f) => f.grant_type) });
  const second = await c.jetons.lire('gitlab.com');
  verifier('… un nouveau jeton gardé, l’ancien ne marche plus', second !== premier && simule.etat.jetons.has(second) && !simule.etat.jetons.has(premier));
  verifier('… sa nouvelle fin notée, le nouveau jeton de renouvellement gardé', c.reglages().comptes['gitlab.com'].expire > maintenant() + 7000000
    && (await c.renouvellements.lire('gitlab.com')) !== rafraichir);
  // at start-up (preparer): an expired one renewed at once
  decalage += 7200000;
  await c.preparer();
  verifier('au démarrage, un jeton expiré renouvelé', await jusqua(async () => (await c.jetons.lire('gitlab.com')) !== second, 6000));

  // the refresh token no longer works (revoked on GitLab), the token neither: signed out, said
  const c2 = ordinateur();
  await c2.commencerConnexion('gitlab.com').then((x) => simule.approuver(x.code));
  await c2.attendreConnexion('gitlab.com');
  const j2 = await c2.jetons.lire('gitlab.com');
  simule.etat.renouvellements.delete(await c2.renouvellements.lire('gitlab.com'));
  simule.revoquer(j2);
  decalage += 7200000;
  const k2 = await c2.compte('gitlab.com');
  verifier('renouvellement refusé : plus connecté (refuse), le jeton de renouvellement oublié', !k2.connecte && k2.refuse === true && k2.login === 'alice'
    && !(await c2.renouvellements.present('gitlab.com')), k2);

  // the code's time over (GitLab's last five minutes), GitLab always saying "pending"
  simule.etat.prochain = [];
  attentes = [];
  await c2.commencerConnexion('gitlab.com');
  const r3 = await c2.attendreConnexion('gitlab.com');
  verifier('expiré (le temps du code passé)', r3.raison === 'expiree' && attentes.length === 60, { r3, n: attentes.length });

  // a GitLab without the device flow (before 17.3), an application ID it doesn't know
  const c4 = ordinateur();
  await c4.reglerServeur('gitlab.example.org', simule.etat.ids['gitlab.example.org']);
  simule.etat.ancien.add('gitlab.example.org');
  const e = await refus(() => c4.commencerConnexion('gitlab.example.org'));
  verifier('un GitLab sans connexion par code : dit (17.3), un jeton proposé', e?.code === 'non_disponible' && /GitLab 17\.3 or later\): use a token instead/.test(e.message), e);
  simule.etat.ancien.delete('gitlab.example.org');
  await c4.reglerServeur('gitlab.example.org', 'f'.repeat(64));
  const e2 = await refus(() => c4.commencerConnexion('gitlab.example.org'));
  verifier('un identifiant inconnu, ou une application confidentielle : dit', e2?.code === 'non_regle' && /non-confidential application/.test(e2.message), e2);
  return c;
}

// ---------------------------------------------------------------- the account, a token by hand

async function compteEtJeton(cGithub) {
  const k = await cGithub.compte('github.com');
  verifier('compte() : le compte connecté', k.connecte && k.login === 'octocat' && k.nom === 'Mona Octocat' && k.source === 'connexion' && /^data:image\/png/.test(k.avatar), k);
  simule.etat.coupes.add('api.github.com');
  const k2 = await cGithub.compte('github.com');
  verifier('GitHub injoignable : ce qu’on en savait (horsLigne)', k2.connecte && k2.horsLigne === true && k2.login === 'octocat' && k2.nom === 'Mona Octocat', k2);
  simule.etat.coupes.delete('api.github.com');

  const c = ordinateur();
  verifier('compte() sans connexion', JSON.stringify(await c.compte('github.com')) === JSON.stringify({ connecte: false, login: null, nom: null, avatar: null, source: null }));
  const j = simule.jeton('github.com', 'hubot');
  const k3 = await c.utiliserJeton('github.com', `  ${j}  `);
  verifier('un jeton donné : vérifié, gardé (source jeton)', k3.connecte && k3.login === 'hubot' && k3.source === 'jeton' && (await c.jetons.lire('github.com')) === j, k3);
  verifier('un jeton de deux mots refusé', (await refus(() => c.utiliserJeton('github.com', 'deux mots')))?.code === 'jeton');
  const e = await refus(() => c.utiliserJeton('github.com', 'ghp_faux000000000000000000'));
  verifier('un jeton que GitHub refuse : dit, l’ancien gardé', e?.code === 'refuse' && /refuses this token/.test(e.message) && (await c.jetons.lire('github.com')) === j, e);
  simule.revoquer(j);
  const k4 = await c.compte('github.com');
  verifier('le jeton révoqué : plus connecté (refuse)', !k4.connecte && k4.refuse === true && k4.login === 'hubot', k4);

  // a token given in Versions (the versions layer alone): recognised; forgotten there, forgotten here
  const c5 = ordinateur();
  await c5.jetonVersions('github.com', simule.jeton('github.com', 'alice-martin'));
  const k5 = await c5.compte('github.com');
  verifier('un jeton donné dans Versions : reconnu (source jeton)', k5.connecte && k5.login === 'alice-martin' && k5.source === 'jeton', k5);
  await c5.jetonVersions('github.com', '');
  verifier('… oublié dans Versions : oublié ici aussi', !(await c5.compte('github.com')).connecte && !Object.hasOwn(c5.reglages().comptes, 'github.com'));

  // a GitLab token for git only: signed in, limited
  const c6 = ordinateur();
  const k6 = await c6.utiliserJeton('gitlab.com', simule.jeton('gitlab.com', 'bob', 'write_repository'));
  verifier('un jeton GitLab pour git seulement : connecté, limité', k6.connecte && k6.limite === true && k6.source === 'jeton', k6);
  verifier('… sans créer de dépôt (dit)', (await refus(() => c6.creerDepot('gitlab.com', 'Essai')))?.code === 'droits');

  // a message of the server that carries the token: blanked
  simule.etat.bavard = true;
  const e7 = await refus(() => cGithub.compte('github.com'));
  const jg = await cGithub.jetons.lire('github.com');
  verifier('un message du serveur qui contient le jeton : le jeton effacé', !!e7 && !e7.message.includes(jg) && e7.message.includes('•••'), e7 && e7.message.replace(jg, '<jeton>'));
  simule.etat.bavard = false;
  // the API sending elsewhere: not followed, the token not carried there
  simule.etat.redirige = true;
  const e8 = await refus(() => cGithub.compte('github.com'));
  verifier('une redirection de l’API : pas suivie, le jeton n’y va pas', e8?.code === 'redirection' && requetes('ailleurs').length === 0, e8);
  simule.etat.redirige = false;
}

// ---------------------------------------------------------------- repositories, colleagues

async function depotsEtInvitations(cGithub, cGitlab) {
  const d = await cGithub.creerDepot('github.com', 'Suivi des hérons 2026.nuee');
  const q = requetes('api.github.com', '/user/repos').pop();
  verifier('GitHub : dépôt créé, privé, nommé d’après le projet', d.url === 'https://github.com/octocat/Suivi-des-herons-2026.git' && d.page === 'https://github.com/octocat/Suivi-des-herons-2026'
    && JSON.parse(q.corps).private === true && JSON.parse(q.corps).name === 'Suivi-des-herons-2026' && /^Bearer gho_/.test(q.authorization), d);
  const e = await refus(() => cGithub.creerDepot('github.com', 'Suivi des hérons 2026'));
  verifier('GitHub : le nom déjà pris (existe)', e?.code === 'existe' && /already exists on your GitHub account/.test(e.message), e);
  await cGithub.creerDepot('github.com', 'Données publiques', { prive: false });
  verifier('public quand on le demande', JSON.parse(requetes('api.github.com', '/user/repos').pop().corps).private === false);

  const i = await cGithub.inviter('github.com', d.url, '@hubot');
  const qi = requetes('api.github.com', '/repos/octocat/Suivi-des-herons-2026/collaborators/hubot').pop();
  verifier('GitHub : un collègue invité (push)', i.resultat === 'invite' && i.login === 'hubot' && i.nom === 'Hubot Robot' && qi.methode === 'PUT' && JSON.parse(qi.corps).permission === 'push', i);
  verifier('GitHub : déjà invité', (await cGithub.inviter('github.com', 'https://github.com/octocat/Suivi-des-herons-2026/tree/main', 'HUBOT')).resultat === 'deja');
  verifier('GitHub : un inconnu', (await refus(() => cGithub.inviter('github.com', d.url, 'personne-ici')))?.code === 'inconnu');
  verifier('GitHub : une organisation', (await refus(() => cGithub.inviter('github.com', d.url, 'github')))?.code === 'organisation');
  verifier('GitHub : soi-même', (await refus(() => cGithub.inviter('github.com', d.url, 'Octocat')))?.code === 'soi');
  verifier('GitHub : un nom invalide', (await refus(() => cGithub.inviter('github.com', d.url, 'a b')))?.code === 'utilisateur');
  verifier('GitHub : l’adresse d’un autre serveur', (await refus(() => cGithub.inviter('github.com', 'https://gitlab.com/a/b', 'hubot')))?.code === 'adresse');
  verifier('GitHub : un dépôt qu’on ne gère pas', (await refus(() => cGithub.inviter('github.com', 'https://github.com/quelquun/autre', 'hubot')))?.code === 'introuvable');

  const g = await cGitlab.creerDepot('gitlab.com', 'Suivi des hérons (2026)');
  const corps = JSON.parse(requetes('gitlab.com', '/api/v4/projects').pop().corps);
  verifier('GitLab : projet créé, privé, nommé d’après le projet', g.url === 'https://gitlab.com/alice/Suivi-des-herons-2026.git' && corps.visibility === 'private'
    && corps.path === 'Suivi-des-herons-2026' && corps.name === 'Suivi des hérons 2026', { g, corps });
  verifier('GitLab : le nom déjà pris', (await refus(() => cGitlab.creerDepot('gitlab.com', 'Suivi des hérons (2026)')))?.code === 'existe');
  const m = await cGitlab.inviter('gitlab.com', 'https://gitlab.com/alice/Suivi-des-herons-2026/-/tree/main', 'bob');
  const qm = requetes('gitlab.com', '/api/v4/projects/alice%2FSuivi-des-herons-2026/members').pop();
  verifier('GitLab : un collègue membre, Developer (30)', m.resultat === 'ajoute' && m.login === 'bob' && m.nom === 'Bob Leroy'
    && JSON.parse(qm.corps).access_level === 30 && JSON.parse(qm.corps).user_id === 2, m);
  verifier('GitLab : déjà membre', (await cGitlab.inviter('gitlab.com', g.url, 'bob')).resultat === 'deja');
  verifier('GitLab : un inconnu', (await refus(() => cGitlab.inviter('gitlab.com', g.url, 'zoe')))?.code === 'inconnu');

  const c = ordinateur();
  verifier('sans connexion : se connecter d’abord', (await refus(() => c.creerDepot('github.com', 'x')))?.code === 'connexion'
    && (await refus(() => c.inviter('github.com', d.url, 'hubot')))?.code === 'connexion');
  verifier('les noms de dépôt', JSON.stringify(['Données été 2026.nuee', '..x..', '', 'a.git', 'Œuvre & Cœur', 'a._-b', 'x'.repeat(150)].map(nomDepot))
    === JSON.stringify(['Donnees-ete-2026', 'x', 'nuee', 'a.git-nuee', 'OEuvre-Coeur', 'a-b', 'x'.repeat(100)]));
  verifier('les noms de projet GitLab', JSON.stringify(['Suivi (2026)', '#1 Hérons', '— Été —'].map(titreGitlab)) === JSON.stringify(['Suivi 2026', '1 Hérons', 'Été']));
}

// ---------------------------------------------------------------- this computer's git

// A stand-in for git: `credential fill` answers as FAUX_GIT_SCENARIO says, and each call is written
// to FAUX_GIT_JOURNAL (its arguments, what it read, what it was told about prompts)
const FAUX_GIT = `// A stand-in for git, written by tests/essai_comptes.js
const fs = require('fs');
const args = process.argv.slice(2);
const entree = fs.readFileSync(0, 'utf8');
const env = {};
for (const k of ['GIT_TERMINAL_PROMPT', 'GIT_ASKPASS', 'SSH_ASKPASS', 'GCM_INTERACTIVE']) env[k] = process.env[k] === undefined ? null : process.env[k];
fs.appendFileSync(process.env.FAUX_GIT_JOURNAL, JSON.stringify({ pid: process.pid, args, entree, env }) + '\\n');
const hote = (/^host=(.*)$/m.exec(entree) || [])[1] || '';
const scenario = process.env.FAUX_GIT_SCENARIO;
if (args.join(' ') === 'credential fill' && scenario === 'ok') {
  process.stdout.write('protocol=https\\nhost=' + hote + '\\nusername=' + process.env.FAUX_GIT_UTILISATEUR + '\\npassword=' + process.env.FAUX_GIT_JETON + '\\n');
  process.exit(0);
}
if (scenario === 'lent') Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 20000);
process.stderr.write("fatal: could not read Username for 'https://" + hote + "': terminal prompts disabled\\n");
process.exit(128);
`;
// On Windows a program is looked for as git.exe: a small one, compiled here by the .NET Framework's
// compiler, runs the stand-in with Node and passes its input and output along
const FAUX_GIT_CS = String.raw`using System;
using System.Diagnostics;
using System.Text;
class FauxGit {
  static int Main(string[] args) {
    var p = new ProcessStartInfo(@"__NODE__");
    var ligne = new StringBuilder("\"" + @"__SCRIPT__" + "\"");
    foreach (var a in args) ligne.Append(" \"" + a.Replace("\"", "\\\"") + "\"");
    p.Arguments = ligne.ToString();
    p.UseShellExecute = false;
    p.CreateNoWindow = true;
    p.RedirectStandardInput = true;
    p.RedirectStandardOutput = true;
    p.RedirectStandardError = true;
    p.EnvironmentVariables["ELECTRON_RUN_AS_NODE"] = "1";
    using (var q = Process.Start(p)) {
      var sortie = q.StandardOutput.ReadToEndAsync();
      var erreurs = q.StandardError.ReadToEndAsync();
      q.StandardInput.Write(Console.In.ReadToEnd());
      q.StandardInput.Close();
      q.WaitForExit();
      Console.Out.Write(sortie.Result);
      Console.Error.Write(erreurs.Result);
      return q.ExitCode;
    }
  }
}
`;
function fauxGit(dossier) {
  fs.mkdirSync(dossier, { recursive: true });
  const script = path.join(dossier, 'faux_git.js');
  fs.writeFileSync(script, FAUX_GIT);
  if (process.platform !== 'win32') {
    fs.writeFileSync(path.join(dossier, 'git'), `#!/bin/sh\nELECTRON_RUN_AS_NODE=1 exec '${process.execPath}' '${script}' "$@"\n`, { mode: 0o755 });
    return true;
  }
  const windir = process.env.WINDIR || 'C:\\Windows';
  const csc = ['Framework64', 'Framework'].map((f) => path.join(windir, 'Microsoft.NET', f, 'v4.0.30319', 'csc.exe')).find((f) => fs.existsSync(f));
  if (!csc) return false;
  const source = path.join(dossier, 'faux_git.cs');
  fs.writeFileSync(source, FAUX_GIT_CS.replace('__NODE__', () => process.execPath.replace(/"/g, '""')).replace('__SCRIPT__', () => script.replace(/"/g, '""')));
  execFileSync(csc, ['/nologo', '/target:exe', `/out:${path.join(dossier, 'git.exe')}`, source], { stdio: 'pipe', windowsHide: true });
  return fs.existsSync(path.join(dossier, 'git.exe'));
}

async function gitDeLOrdinateur() {
  const dossierGit = path.join(racine, 'git');
  let pret = false;
  try { pret = fauxGit(dossierGit); } catch (e) { console.log(`  (${e.message.split('\n')[0]})`); }
  if (!pret) { console.log('  (pas de git de remplacement possible ici : git credential fill pas vérifié)'); return; }
  // (only the stand-in on the PATH, never the computer's own git; Windows's System32 for taskkill)
  const systeme = process.platform === 'win32' ? path.join(process.env.SystemRoot || 'C:\\Windows', 'System32') : null;
  const sauve = Object.fromEntries(['PATH', 'GIT_ASKPASS', 'SSH_ASKPASS', 'FAUX_GIT_JOURNAL', 'FAUX_GIT_SCENARIO', 'FAUX_GIT_JETON', 'FAUX_GIT_UTILISATEUR'].map((k) => [k, process.env[k]]));
  const journalGit = path.join(racine, 'git-appels.jsonl');
  const appels = () => (fs.existsSync(journalGit) ? fs.readFileSync(journalGit, 'utf8').split('\n').filter(Boolean).map((l) => JSON.parse(l)) : []);
  Object.assign(process.env, { PATH: [dossierGit, systeme].filter(Boolean).join(path.delimiter), GIT_ASKPASS: path.join(racine, 'askpass'), SSH_ASKPASS: path.join(racine, 'askpass'),
    FAUX_GIT_JOURNAL: journalGit });
  try {
    const c = ordinateur();
    verifier('git trouvé sur le PATH', (await c.serveur('github.com')).git === true);
    const jeton = simule.jeton('github.com', 'octocat');
    Object.assign(process.env, { FAUX_GIT_SCENARIO: 'ok', FAUX_GIT_JETON: jeton, FAUX_GIT_UTILISATEUR: 'octocat' });
    const r = await c.compteGit('github.com');
    const a = appels().pop() || {};
    verifier('git credential fill : connecté (source git), son jeton gardé', r.ok && r.compte.login === 'octocat' && r.compte.source === 'git' && /^data:image\/png/.test(r.compte.avatar)
      && (await c.jetons.lire('github.com')) === jeton, r.compte);
    verifier('… git appelé avec credential fill, l’hôte en entrée', JSON.stringify(a.args) === '["credential","fill"]' && a.entree === 'protocol=https\nhost=github.com\n\n', a);
    verifier('… sans invite de terminal ni programme askpass, Git Credential Manager libre de se montrer',
      a.env && a.env.GIT_TERMINAL_PROMPT === '0' && a.env.GIT_ASKPASS === null && a.env.SSH_ASKPASS === null && a.env.GCM_INTERACTIVE === null, a.env);
    verifier('… le compte noté (source git)', c.reglages().comptes['github.com'].source === 'git');
    // its token refused later: git asked again, without a window
    simule.revoquer(jeton);
    const jeton2 = simule.jeton('github.com', 'octocat');
    process.env.FAUX_GIT_JETON = jeton2;
    const k = await c.compte('github.com');
    verifier('le jeton de git refusé : git redemandé sans fenêtre (GCM_INTERACTIVE=never), le nouveau gardé', k.connecte && k.source === 'git'
      && (appels().pop() || { env: {} }).env.GCM_INTERACTIVE === 'never' && (await c.jetons.lire('github.com')) === jeton2, k);

    const c2 = ordinateur();
    process.env.FAUX_GIT_SCENARIO = 'aucun';
    verifier('git n’a rien pour ce serveur : dit (aucun), rien de gardé', JSON.stringify(await c2.compteGit('github.com')) === '{"ok":false,"raison":"aucun"}'
      && !(await c2.jetons.present('github.com')));
    Object.assign(process.env, { FAUX_GIT_SCENARIO: 'ok', FAUX_GIT_JETON: 'gho_pasunvraijeton0000000' });
    verifier('git donne un jeton que GitHub refuse : dit (refuse), rien de gardé', JSON.stringify(await c2.compteGit('github.com')) === '{"ok":false,"raison":"refuse"}'
      && !(await c2.jetons.present('github.com')));
    process.env.FAUX_GIT_SCENARIO = 'lent';
    const debut = Date.now();
    const r3 = await c2.compteGit('github.com');
    const lent = appels().pop() || {};
    verifier('git qui ne répond pas : arrêté après le délai (delai)', r3.raison === 'delai' && Date.now() - debut < 10000, { r3, ms: Date.now() - debut });
    verifier('… avec ce qu’il a lancé', await jusqua(() => !vivant(lent.pid), 8000), lent.pid);
    const n = appels().length;
    const enCours = c2.compteGit('github.com');
    await jusqua(() => appels().length > n);
    await c2.annulerConnexion('github.com');
    verifier('git annulé', (await enCours).raison === 'annulee');
    Object.assign(process.env, { FAUX_GIT_SCENARIO: 'ok', FAUX_GIT_JETON: simule.jeton('gitlab.com', 'bob', 'write_repository'), FAUX_GIT_UTILISATEUR: 'oauth2' });
    const r4 = await c2.compteGit('gitlab.com');
    verifier('GitLab par git (write_repository seul, l’utilisateur oauth2) : connecté, limité, sans nom', r4.ok && r4.compte.limite === true && r4.compte.login === null
      && r4.compte.source === 'git', r4);
    const sansGit = path.join(racine, 'sans-git');
    fs.mkdirSync(sansGit);
    process.env.PATH = [sansGit, systeme].filter(Boolean).join(path.delimiter);
    verifier('git absent : serveur() le dit, compteGit aussi', (await c2.serveur('github.com')).git === false && (await c2.compteGit('github.com')).raison === 'absent');
  } finally {
    for (const [k, v] of Object.entries(sauve)) { if (v === undefined) delete process.env[k]; else process.env[k] = v; }
  }
}

// ---------------------------------------------------------------- signing out, the secrets

async function deconnexion(cGithub, cGitlab) {
  await cGithub.deconnecter('github.com');
  verifier('déconnecté de GitHub : le jeton et le compte oubliés', !(await cGithub.jetons.present('github.com')) && !Object.hasOwn(cGithub.reglages().comptes, 'github.com')
    && !(await cGithub.compte('github.com')).connecte);
  await cGitlab.deconnecter('gitlab.com');
  verifier('déconnecté de GitLab : le jeton de renouvellement oublié aussi', !(await cGitlab.jetons.present('gitlab.com')) && !(await cGitlab.renouvellements.present('gitlab.com')));
}

async function secretsGardes() {
  const tous = [...new Set(simule.etat.secrets)].filter((s) => s.length >= 8);
  const textes = [...messages, ...retours, ...journal, ...ouverts];
  const fuites = tous.filter((s) => textes.some((x) => x.includes(s)));
  verifier(`aucun des ${tous.length} jetons, jetons de renouvellement et codes d’appareil dans un message, un retour, le journal ou une page ouverte`, !fuites.length, `${fuites.length} trouvé(s)`);
  const adresses = simule.etat.requetes.map((q) => `${q.hote}${q.chemin}`);
  verifier('… ni dans une adresse demandée', !tous.some((s) => adresses.some((a) => a.includes(s) || a.includes(encodeURIComponent(s)))));
  const fichiers = [];
  const lire = (d) => { for (const e of fs.readdirSync(d, { withFileTypes: true })) { const p = path.join(d, e.name); if (e.isDirectory()) lire(p); else if (/\.json$/.test(e.name)) fichiers.push(fs.readFileSync(p, 'utf8')); } };
  lire(racine);
  verifier(`… ni en clair dans un fichier (${fichiers.length} : réglages, jetons chiffrés)`, fichiers.length > 10 && !tous.some((s) => fichiers.some((t) => t.includes(s))));
  verifier('le jeton envoyé à l’API seulement (pas aux pages OAuth ni aux images)', simule.etat.requetes.filter((q) => q.authorization)
    .every((q) => q.hote === 'api.github.com' || q.chemin.startsWith('/api/v4/')));
  verifier('les pages ouvertes : https, sur leur serveur', ouverts.length > 0 && ouverts.every((u) => /^https:\/\/(github\.com|gitlab\.com|gitlab\.example\.org)\//.test(u)), ouverts);
  verifier('aucun autre serveur demandé', simule.etat.horsListe.length === 0, simule.etat.horsListe);
}

(async () => {
  let fin = 0;
  try {
    if (electron) { await electron.app.whenReady(); fetchReel = electron.net.fetch; }
    simule = await serveurComptes();
    ids = { 'github.com': simule.etat.ids['github.com'], 'gitlab.com': simule.etat.ids['gitlab.com'] };
    console.log(`Vérification des comptes GitHub et GitLab (${electron ? 'Electron, net.fetch' : 'Node'})`);
    await nonRegle();
    const cGithub = await connexionGithub();
    const cGitlab = await connexionGitlab();
    await compteEtJeton(cGithub);
    await depotsEtInvitations(cGithub, cGitlab);
    await gitDeLOrdinateur();
    await deconnexion(cGithub, cGitlab);
    await secretsGardes();
  } catch (e) {
    verifier('erreur', false, e.stack);
  } finally {
    console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
    if (simule) await simule.fermer().catch(() => {});
    try { fs.rmSync(racine, { recursive: true, force: true, maxRetries: 5 }); } catch (e) { console.log(`  (${racine} pas supprimé : ${e.code})`); }
    fin = echecs ? 1 : 0;
    if (electron) electron.app.exit(fin); else process.exit(fin);
  }
})();
