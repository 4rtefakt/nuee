// Quick check run before each build (construire.js): the interface files parse, and the R
// server answers the main requests correctly on a small made-up table.
//   node tests/essai_api.js [Rscript]
const { spawn, execFileSync } = require('child_process');
const fs = require('fs');
const net = require('net');
const os = require('os');
const path = require('path');

const depot = path.join(__dirname, '..', '..');
// bin\x64\Rscript.exe is R itself (bin\Rscript.exe only starts it, and would leave it running)
const lanceur = process.argv[2] || process.env.RSCRIPT
  || (process.platform === 'win32' ? path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe') : 'Rscript');
const rscript = fs.existsSync(path.join(path.dirname(lanceur), 'x64', 'Rscript.exe')) ? path.join(path.dirname(lanceur), 'x64', 'Rscript.exe') : lanceur;
const dossier = fs.mkdtempSync(path.join(os.tmpdir(), 'essai-analyse-'));
const echecs = [];

// A table of 60 observations: an identifier, two qualitative variables, six X with some
// structure and a few missing values, one Y.
function tableau() {
  let graine = 7;
  const alea = () => { graine = (graine * 16807) % 2147483647; return graine / 2147483647 - 0.5; };
  const lignes = ['nom,zone,groupe,x1,x2,x3,x4,x5,x6,y'];
  for (let i = 1; i <= 60; i += 1) {
    const z = alea() * 4;
    const o = alea() * 3;
    const x = [z + alea(), z - o + alea(), o + alea(), 2 * z + alea(), o * 0.5 + alea(), alea() * 2];
    if (i % 17 === 0) x[2] = '';
    lignes.push([`o${i}`, ['A', 'B', 'C'][i % 3], z > 0 ? 'haut' : 'bas', ...x.map((v) => (v === '' ? '' : v.toFixed(4))), (z * 1.5 + alea()).toFixed(4)].join(','));
  }
  const f = path.join(dossier, 'essai.csv');
  fs.writeFileSync(f, lignes.join('\n'));
  return f;
}

function portLibre() {
  return new Promise((resolve) => { const s = net.createServer(); s.listen(0, '127.0.0.1', () => { const { port } = s.address(); s.close(() => resolve(port)); }); });
}

function verifier(condition, message) { if (!condition) throw new Error(message); }

async function etape(nom, f) {
  try {
    await f();
    console.log(`  ok      ${nom}`);
  } catch (e) {
    echecs.push(nom);
    console.log(`  ÉCHEC   ${nom} : ${e.message}`);
  }
}

async function principal() {
  console.log('Vérification de l’interface (syntaxe)…');
  const interfaceDir = path.join(depot, 'app', 'interface');
  // (checked as ES modules, through a .mjs copy: a .js that imports or exports passes
  // `node --check` whatever its errors, a declaration made twice included)
  const copies = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-syntaxe-'));
  for (const f of fs.readdirSync(interfaceDir).filter((x) => x.endsWith('.js'))) {
    const copie = path.join(copies, f.replace(/\.js$/, '.mjs'));
    fs.copyFileSync(path.join(interfaceDir, f), copie);
    await etape(`syntaxe ${f}`, () => execFileSync(process.execPath, ['--check', copie], { stdio: 'pipe' }));
  }
  fs.rmSync(copies, { recursive: true, force: true });

  console.log('Vérification du serveur R…');
  const port = await portLibre();
  // the session token, as main.js gives it to R and to its windows
  const jeton = require('crypto').randomBytes(32).toString('hex');
  const cookie = `nuee_${port}=${jeton}`;
  const serveur = spawn(rscript, [path.join(depot, 'moteur', 'serveur.R'), String(port), interfaceDir], { windowsHide: true, env: { ...process.env, NUEE_JETON: jeton } });
  let sortie = '';
  await new Promise((resolve, reject) => {
    serveur.stdout.on('data', (d) => { sortie += d; if (sortie.includes('PRET')) resolve(); });
    serveur.stderr.on('data', (d) => { sortie += d; });
    serveur.on('exit', (code) => reject(new Error(`R s'est arrêté (code ${code}) : ${sortie.slice(-800)}`)));
  });
  const api = async (nom, q = {}, langue = null) => {
    const r = await fetch(`http://127.0.0.1:${port}/api/${nom}`, { method: 'POST', body: JSON.stringify(q), headers: { Cookie: cookie, ...(langue ? { 'X-Langue': langue } : {}) } });
    const j = await r.json();
    if (!r.ok) throw new Error(`${nom} : ${j.erreur}`);
    return j;
  };
  // a raw request, with exactly the headers given (fetch would not send another Host)
  const brute = (chemin, entetes, corps = '') => new Promise((resolve, reject) => {
    const req = require('http').request({ host: '127.0.0.1', port, path: chemin, method: corps ? 'POST' : 'GET', headers: entetes }, (res) => {
      let texte = '';
      res.on('data', (d) => { texte += d; });
      res.on('end', () => resolve({ statut: res.statusCode, entetes: res.headers, texte }));
    });
    req.on('error', reject);
    req.end(corps);
  });
  const uid = {};
  let p; let pca; let pls; let opls; let da;
  try {
    await etape('le moteur refuse ce qui ne vient pas de la fenêtre de Nuée', async () => {
      // what any web page can send to 127.0.0.1 (no preflight), and what another user could
      const code = JSON.stringify({ code: 'writeLines("non", file.path(tempdir(), "intrus.txt"))' });
      for (const entetes of [{ 'Content-Type': 'text/plain', Origin: 'https://exemple.invalid' }, { Cookie: `nuee_${port}=${'0'.repeat(64)}` }, { Cookie: `autre=${jeton}` }]) {
        const r = await brute('/api/console', entetes, code);
        verifier(r.statut === 403, `console sans le jeton : HTTP ${r.statut}`);
      }
      // a site whose name points to 127.0.0.1 (DNS rebinding) is refused even with the cookie
      const r = await brute('/api/etat', { Host: `exemple.invalid:${port}`, Cookie: cookie }, '{}');
      verifier(r.statut === 403, `autre nom d'hôte : HTTP ${r.statut}`);
      const etat = await api('console', { code: 'file.exists(file.path(tempdir(), "intrus.txt"))' });
      verifier(etat.journal.some((e) => e.type === 'sortie' && e.texte.includes('FALSE')), 'le code refusé a été exécuté');
      // the browser's way in (development): the right token sets the cookie, a wrong one doesn't
      verifier((await brute(`/api/connexion?jeton=${'0'.repeat(64)}`, {})).statut === 403, 'connexion avec un faux jeton');
      const c = await brute(`/api/connexion?jeton=${jeton}&ouvrir=a%20b.csv`, {});
      verifier(c.statut === 302 && c.entetes.location === '/?ouvrir=a%20b.csv', `connexion : HTTP ${c.statut} vers ${c.entetes.location}`);
      verifier(/HttpOnly/.test(c.entetes['set-cookie']) && /SameSite=Strict/.test(c.entetes['set-cookie'])
        && String(c.entetes['set-cookie']).startsWith(cookie), `cookie posé : ${c.entetes['set-cookie']}`);
    });
    await etape('ouvrir un tableau', async () => {
      p = await api('ouvrir', { chemin: tableau() });
      for (const v of p.variables) uid[v.nom] = v.uid;
      verifier(p.variables.length === 9 && p.observations.uid.length === 60, `${p.variables.length} variables, ${p.observations.uid.length} observations`);
    });
    const x = ['x1', 'x2', 'x3', 'x4', 'x5', 'x6'].map((n) => uid[n]);
    await etape('ACP, nombre de composantes automatique', async () => {
      pca = await api('ajuster', { projet: p.id, type: 'PCA', x, A: 0, nom: 'ACP' });
      verifier(pca.A >= 1 && pca.observations.t1.length === 60, `A = ${pca.A}`);
      verifier(pca.reglages && pca.reglages.x.length === 6, 'réglages du modèle absents');
      verifier(pca.numero !== null && pca.numero !== undefined, 'numéro du modèle absent');
    });
    await etape('PLS', async () => {
      pls = await api('ajuster', { projet: p.id, type: 'PLS', x, y: [uid.y], A: 2, nom: 'PLS' });
      verifier(pls.composantes.R2Ycum.length === 2, 'R2Y');
      verifier(Array.isArray(pls.noms_y) && pls.noms_y.length === 1, 'noms_y doit être une liste');
      verifier('CoeffCS y' in pls.coefficients && pls.variables.VIP.length === 6, 'coefficients ou VIP');
    });
    await etape('OPLS', async () => {
      opls = await api('ajuster', { projet: p.id, type: 'OPLS', x, y: [uid.y], A: 2, nom: 'OPLS' });
      verifier(opls.type === 'OPLS' && opls.opls === true && opls.A === 2, opls.type);
    });
    await etape('PLS-DA', async () => {
      da = await api('ajuster', { projet: p.id, type: 'PLS-DA', x, classe: uid.groupe, A: 2, nom: 'DA' });
      verifier(da.type === 'PLS-DA' && typeof da.classification.vc.global === 'number', da.type);
    });
    await etape('réajuster sans deux observations', async () => {
      const r = await api('reajuster', { modele: pls.id, exclues: [1, 2], A: 2 });
      verifier(r.N === 58, `N = ${r.N}`);
    });
    await etape('groupes de validation croisée par variable, blocs et modificateur', async () => {
      // as New model sends them: nothing set (vc null, empty objects), then a group per zone
      const sans = await api('ajuster', { projet: p.id, type: 'PLS', x, y: [uid.y], A: 2, nom: 'PLS0', vc: null, blocs: {}, modificateurs: {} });
      verifier(sans.G === 7 && sans.vc_par === '' && !sans.reglages.vc, `sans réglage : G = ${sans.G}, ${sans.vc_par}`);
      const m = await api('ajuster', { projet: p.id, type: 'PLS', x, y: [uid.y], A: 2, nom: 'PLSzone', vc: { by: 'variable', variable: uid.zone },
        blocs: { [uid.x1]: 'a', [uid.x2]: 'a' }, modificateurs: { [uid.x3]: 2 } });
      verifier(m.G === 3 && m.vc_par === 'zone', `G = ${m.G}, par ${m.vc_par}`);
      const r = m.reglages;
      verifier(r.vc && r.vc.by === 'variable' && r.vc.variable === uid.zone && r.blocs[uid.x1] === 'a' && r.modificateurs[uid.x3] === 2,
        `réglages : ${JSON.stringify([r.vc, r.blocs, r.modificateurs])}`);
      const re = await api('reajuster', { modele: m.id, exclues: [3], A: 2 });
      verifier(re.G === 3 && re.vc_par === 'zone' && re.N === 59, `réajusté : G = ${re.G}, N = ${re.N}`);
      let refus = '';
      try { await api('ajuster', { projet: p.id, type: 'PLS', x, y: [uid.y], A: 2, vc: { by: 'id', id: 'site' } }); } catch (e) { refus = e.message; }
      verifier(/site/.test(refus), `identifiant absent : « ${refus} »`);
    });
    await etape('transformations personnalisées et terme à trois facteurs', async () => {
      const m = await api('ajuster', { projet: p.id, type: 'PLS', x: [uid.zone, ...x], y: [uid.y], A: 2, nom: 'PLSperso',
        transformations: { [uid.x1]: 'ln:1:5', [uid.x2]: 'puissance:1:10:0.33', [uid.x4]: 'logit:-20:20' }, croises: [[uid.zone, uid.x1, uid.x2], [uid.x3, uid.x3, uid.x3]] });
      verifier(m.K === 3 + 6 + 3 + 1, `K = ${m.K}`);
      const r = m.reglages;
      verifier(r.transformations[uid.x1] === 'ln:1:5' && r.transformations[uid.x2] === 'puissance:1:10:0.33' && r.transformations[uid.x4] === 'logit:-20:20',
        `réglages : ${JSON.stringify(r.transformations)}`);
      verifier(r.croises.length === 2 && r.croises.every((c) => c.length === 3), `termes : ${JSON.stringify(r.croises)}`);
    });
    await etape('prédire, dendrogramme, permutations, contributions, biplot', async () => {
      const pr = await api('predire', { modele: pls.id });
      verifier(pr.observations.t1.length === 60, 'prédiction');
      verifier((await api('hca', { modele: pca.id })).merge.length === 59, 'dendrogramme');
      verifier((await api('permutations', { modele: pls.id, n: 3 })).points.R2.length === 4, 'permutations');
      verifier((await api('contributions', { modele: pls.id, observations: [3] })).contribution.length === 6, 'contributions');
      const b = await api('biplot', { modele: pls.id });
      verifier(Array.isArray(b.c[0]) && b.t.length === 2, 'biplot');
    });
    await etape('statistiques des variables, vérification des données', async () => {
      verifier((await api('statistiques_variables', { projet: p.id })).nom.length === 9, 'statistiques');
      await api('verifier', { projet: p.id, type: 'PCA', x });
    });
    await etape('nouvelle variable, modification, annuler', async () => {
      // each change of the data, and each undo, is a revision of the project (the views that keep
      // results computed from its data compute them again)
      const avant = (await api('projet', { projet: p.id })).revision;
      let r = await api('variable_calculee', { projet: p.id, nom: 'x1x2', formule: 'x1 * x2' });
      verifier(r.variables.length === 10 && r.annulable && r.revision === avant + 1, `variable calculée (révision ${r.revision})`);
      r = (await api('annuler', { projet: p.id })).projet;
      verifier(r.variables.length === 9 && r.revision === avant + 2, `annuler la variable (révision ${r.revision})`);
      const m = await api('modifier_valeur', { projet: p.id, observation: 1, variable: uid.x1, valeur: '12,5' });
      const d = await api('donnees', { projet: p.id });
      verifier(d.valeurs[d.variables.indexOf('x1')][0] === 12.5 && m.revision === avant + 3, 'valeur modifiée');
      await api('annuler', { projet: p.id });
      // a change that fails isn't one: no revision
      const apres = (await api('projet', { projet: p.id })).revision;
      let refus = '';
      try { await api('variable_calculee', { projet: p.id, nom: 'mauvaise', formule: 'x1 *' }); } catch (err) { refus = err.message; }
      verifier(refus && (await api('projet', { projet: p.id })).revision === apres, `un changement refusé compte une révision (${refus})`);
    });
    await etape('notes, enregistrement avec séance, réouverture', async () => {
      verifier((await api('note_modele', { modele: pls.id, note: 'essai' })).note === 'essai', 'note');
      const f = path.join(dossier, 'projet.nuee');
      await api('enregistrer', { projet: p.id, chemin: f, session: JSON.stringify({ version: 2, modeles: [{ numero: pls.numero, A: 2 }] }) });
      const q = await api('ouvrir', { chemin: f });
      verifier(q.session === true, 'séance enregistrée');
      const m = await api('ajuster', { projet: q.id, numero: pls.numero, A: 2 });
      verifier(m.note === 'essai' && m.reglages.x.length === 6, 'modèle relu');
    });
    await etape('enregistrer sans risque : version, fichier changé par ailleurs, projet reçu', async () => {
      const f = path.join(dossier, 'partage.nuee');
      await api('enregistrer', { projet: p.id, chemin: f, version: '9.9.9' });
      const lu = JSON.parse(fs.readFileSync(f, 'utf8'));
      verifier(lu.nuee && lu.nuee.format === 1 && lu.nuee.version === '9.9.9' && !lu.chemin_enregistrement, `en-tête : ${JSON.stringify(lu.nuee)}`);
      verifier(!fs.readdirSync(dossier).some((x) => x.endsWith('.tmp')), 'fichier temporaire resté');
      // a colleague saves the same file meanwhile: asked before overwriting, then overwritten if wanted
      fs.writeFileSync(f, JSON.stringify({ ...lu, source: 'collègue' }));
      fs.utimesSync(f, new Date(), new Date(Date.now() + 5000));
      const r = await fetch(`http://127.0.0.1:${port}/api/enregistrer`, { method: 'POST', headers: { Cookie: cookie }, body: JSON.stringify({ projet: p.id, chemin: f }) });
      const j = await r.json();
      verifier(r.status === 409 && j.conflit === true && j.date > 0, `HTTP ${r.status} ${JSON.stringify(j)}`);
      verifier(JSON.parse(fs.readFileSync(f, 'utf8')).source === 'collègue', 'écrasé sans demander');
      await api('enregistrer', { projet: p.id, chemin: f, ecraser: true });
      verifier(JSON.parse(fs.readFileSync(f, 'utf8')).source !== 'collègue', 'pas écrasé quand demandé');
      await api('enregistrer', { projet: p.id, chemin: f });  // ours since: no question
      // a project from a newer Nuée is said so; one that names another file to save to doesn't
      fs.writeFileSync(f, JSON.stringify({ ...lu, nuee: { format: 99, version: '7.0' }, chemin_enregistrement: path.join(dossier, 'these.docx'), modifie: true }));
      const q = await api('ouvrir', { chemin: f });
      verifier([].concat(q.note || []).some((n) => n.includes('7.0')), `note : ${q.note}`);
      verifier(q.chemin_enregistrement === f && q.modifie === false, `enregistre dans ${q.chemin_enregistrement}`);
      // a recovery copy of a pasted table (no file): reopened with no file to save to
      const copie = path.join(dossier, 'copie-secours.json');
      await api('sauvegarde_secours', { projet: p.id, chemin: copie });
      const s = await api('ouvrir', { chemin: copie, secours: true, origine: null, nom: 'Tableau collé 1' });
      verifier(s.nom === 'Tableau collé 1' && s.modifie === true && !s.chemin && !s.chemin_enregistrement, `copie rouverte : ${s.chemin_enregistrement}`);
      verifier([].concat((await api('projets_modifies')).noms).includes('Tableau collé 1'), 'projets_modifies');
    });
    await etape('copie de secours reprise : le fichier changé par ailleurs l’est encore', async () => {
      const f = path.join(dossier, 'repris.nuee');
      await api('enregistrer', { projet: p.id, chemin: f });
      // the copy comes with the state of the project's file, which the index keeps (JSON)
      const copie1 = path.join(dossier, 'secours-1.json');
      const c1 = await api('sauvegarde_secours', { projet: p.id, chemin: copie1 });
      verifier(c1.lu && c1.lu.taille > 0 && c1.lu.date > 0, `lu : ${JSON.stringify(c1.lu)}`);
      // the file unchanged since: saved without a question
      const s1 = await api('ouvrir', { chemin: copie1, secours: true, origine: f, nom: 'repris', lu: JSON.parse(JSON.stringify(c1.lu)) });
      verifier(s1.chemin_enregistrement === f, `enregistre dans ${s1.chemin_enregistrement}`);
      await api('enregistrer', { projet: s1.id, chemin: f });
      // changed by a colleague between the stop and the recovery: asked before overwriting
      const copie2 = path.join(dossier, 'secours-2.json');
      const c2 = await api('sauvegarde_secours', { projet: s1.id, chemin: copie2 });
      fs.writeFileSync(f, JSON.stringify({ ...JSON.parse(fs.readFileSync(f, 'utf8')), source: 'collègue' }));
      fs.utimesSync(f, new Date(), new Date(Date.now() + 5000));
      const s2 = await api('ouvrir', { chemin: copie2, secours: true, origine: f, nom: 'repris 2', lu: c2.lu });
      const r = await fetch(`http://127.0.0.1:${port}/api/enregistrer`, { method: 'POST', headers: { Cookie: cookie }, body: JSON.stringify({ projet: s2.id, chemin: f }) });
      verifier(r.status === 409 && JSON.parse(fs.readFileSync(f, 'utf8')).source === 'collègue', `HTTP ${r.status}, écrasé sans demander`);
    });
    await etape('jeu de test : choisi par Kennard–Stone, prédit après l’ajustement', async () => {
      const t = await api('jeu_test', { projet: p.id, x: [uid.x1, uid.x2, uid.x3], methode: 'kennard_stone', part: 0.25 });
      verifier([].concat(t.test).length === 15, `${[].concat(t.test).length} observations en test`);
      const m = await api('ajuster', { projet: p.id, type: 'PLS', x: [uid.x1, uid.x2, uid.x3], y: [uid.y], A: 2, nom: 'Test', test: t.test });
      verifier(m.N === 45 && m.test.n === 15 && [].concat(m.test.statistiques.RMSEP)[0] > 0 && [].concat(m.reglages.test).length === 15, `N ${m.N}, test ${m.test.n}`);
    });
    await etape('messages en anglais (X-Langue)', async () => {
      const essai = (langue) => api('ouvrir', { chemin: path.join(dossier, 'x.zzz') }, langue).then(() => '', (e) => e.message);
      verifier(/Unrecognised file format/.test(await essai('en')) && /Format de fichier non reconnu/.test(await essai(null)), await essai('en'));
    });
    await etape('script R autonome', async () => {
      const r = await api('script_r', { modele: pls.id, chemin: path.join(dossier, 'pls.R'), version: 'essai' });
      verifier(fs.existsSync(r.script) && [].concat(r.chemins).length === 1 && !r.ecart, 'script seul, mêmes résultats');
      const texte = fs.readFileSync(r.script, 'utf8');
      // written for this model alone: the PLS itself, none of the engine's general functions
      verifier(!texte.includes('charger.R') && /^pls <- function/m.test(texte) && /^composante_pls <- function/m.test(texte)
        && !/^(ajuster|preparer|statistiques|composante_acp|opls\w*|snv|msc) <- function/m.test(texte), 'sans Nuée, ce modèle seul');
      const s = await api('script_r', { modele: pls.id, chemin: path.join(dossier, 'pls2.R'), donnees: 'csv', renv: true, rdata: true, rproj: true });
      verifier([].concat(s.chemins).length === 5 && [].concat(s.chemins).every((c) => fs.existsSync(c)), 'CSV, renv.lock, .RData, .Rproj');
      const q = await api('script_r', { modeles: [pca.id, pls.id], chemin: path.join(dossier, 'seance.R'), version: 'essai' });
      const seance = fs.readFileSync(q.script, 'utf8');
      verifier(!q.ecart && /^# Les 2 modèles de la séance/.test(seance) && (seance.match(/^composante_pls <- function/gm) || []).length === 1
        && (seance.match(/^composante_acp <- function/gm) || []).length === 1, 'séance : deux modèles, chaque fonction une fois');
    });
    await etape('informations de diagnostic', async () => {
      const v = await api('version');
      verifier(/^R version/.test(v.R) && v.plateforme && typeof v.paquets === 'object' && v.paquets.jsonlite, JSON.stringify(v).slice(0, 120));
    });
    await etape('console R et journal', async () => {
      const r = await api('console', { code: 'm$A', modele: pls.id });
      verifier(r.journal.some((e) => e.type === 'sortie' && e.texte.includes('[1] 2')), 'console');
      verifier((await api('journal', { depuis: 0 })).entrees.some((e) => e.type === 'appel'), 'journal');
    });
    await etape('complétion (Tab) sans rien exécuter', async () => {
      const c = await api('completer', { texte: 'm$stat', modele: pls.id });
      verifier(c.debut === 2 && [].concat(c.candidats).includes('statistiques'), JSON.stringify(c));
      // an object whose `[[` method has an effect: listing its names must not call it
      await api('console', { code: 'piege <- structure(list(a = list(b = 1, bc = 2)), class = "piege"); appels <- 0; registerS3method("[[", "piege", function(x, i) { appels <<- appels + 1; NULL })' });
      const d = await api('completer', { texte: 'piege$a$b' });
      const n = await api('console', { code: 'cat(appels)' });
      verifier([].concat(d.candidats).join(' ') === 'b bc' && n.journal.some((e) => e.type === 'sortie' && e.texte.trim() === '0'), `${JSON.stringify(d.candidats)}, appels : ${JSON.stringify(n.journal)}`);
      const inconnu = await api('completer', { texte: 'inexistant$a' });  // nothing to offer, no error
      verifier([].concat(inconnu.candidats).length === 0, JSON.stringify(inconnu));
    });
    await etape('projet d’exemple', async () => {
      const e = await api('exemple');
      verifier(e.modeles.length === 4 && e.observations.uid.length === 120 && !e.chemin_enregistrement, `${e.modeles.length} modèles`);
      const m = await api('ajuster', { projet: e.id, numero: e.modeles[0].numero, A: e.modeles[0].A });
      verifier(m.type === 'PCA' && m.N === 120, m.type);
    });
    await etape('tests par variable, texte des méthodes, modèles de classe, OPLS-DA à 4 classes', async () => {
      const e = await api('exemple');
      const v = (nom) => e.variables.find((x) => x.nom === nom).uid;
      const t = await api('tests_variables', { projet: e.id, classe: v('culture'), test: 'welch', correction: 'BH' });
      verifier([].concat(t.tableau.p_ajuste).length === 15 && t.groupes.length === 2, 'tests par variable');
      const k = await api('test_deux_variables', { projet: e.id, x: v('verger'), y: v('culture') });
      verifier(k.type === 'contingence' && k.conseille === 'khi2' && k.effectifs.length === 3 && k.residus[0].length === 2 && /χ²\(2, N = 120\)/.test(k.phrase), 'khi²');
      const g = await api('test_deux_variables', { projet: e.id, x: v('variete'), y: v('sucre') });
      verifier(g.type === 'groupes' && [].concat(g.posthoc.kruskal.p_ajuste).length === 6 && [].concat(g.valeurs.valeur).length === 120, 'une mesure entre groupes');
      const r = await api('correlations_variables', { projet: e.id, methode: 'spearman', correction: 'BH' });
      verifier(r.r.length === 15 && r.r[0].length === 15 && r.p_ajuste[0][0] === null && r.n[0][1] > 0, 'matrice des corrélations');
      // regressions (moteur/R/regression.R): no components, their own results
      const lm = await api('ajuster', { projet: e.id, type: 'LM', x: [v('sucre'), v('fermete')], y: [v('note_gout')], echelle: 'none', nom: 'R1' });
      verifier(lm.regression && lm.type === 'LM' && lm.A === 0 && lm.N === 120 && [].concat(lm.coefficients_regression.terme).length === 3
        && lm.qualite.AIC !== null && [].concat(lm.observations.quantile_normal).length === 120, 'régression linéaire');
      const lmm = await api('ajuster', { projet: e.id, type: 'LMM', x: [v('sucre'), v('fermete')], y: [v('note_gout')], echelle: 'none', nom: 'R2',
        aleatoires: [{ by: 'variable', variable: v('verger') }] });
      verifier(lmm.type === 'LMM' && lmm.vc_par === 'verger' && lmm.G === 3 && lmm.qualite.marginal > 0 && [].concat(lmm.variances.composante).length === 2
        && lm.signature === lmm.signature, 'modèle mixte validé par verger, même ensemble AIC que la régression');
      const ap = await api('apports', { modele: lmm.id });
      verifier([].concat(ap.apports.groupe).length === 2 && [].concat(ap.apports.delta_aic).every((d) => d !== null), 'apport des variables (AIC)');
      const ex = await api('exporter', { modele: lmm.id, chemin: path.join(dossier, 'R2.csv') });
      verifier(ex.chemins.length === 4 && ex.chemins.every((c) => fs.existsSync(c)), 'exporter les résultats d’une régression (un fichier par tableau)');
      const sc = await api('script_r', { modele: lmm.id, version: 'essai', langue: 'fr', chemin: path.join(dossier, 'R2.R') });
      verifier(fs.existsSync(sc.script) && !sc.ecart && fs.readFileSync(sc.script, 'utf8').includes('lme('), 'exporter un modèle mixte vers R : le script redonne ses résultats');
      const tm = await api('methodes', { modele: lmm.id, langue: 'en', version: 'essai' });
      verifier([].concat(tm.paragraphes).some((x) => /linear mixed model/.test(x)) && [].concat(tm.references).some((x) => /nlme/.test(x)), 'texte des méthodes d’un modèle mixte');
      const glm = await api('ajuster', { projet: e.id, type: 'GLM', x: [v('sucre')], y: [v('stockage')], echelle: 'none', nom: 'R3', famille: 'poisson' });
      verifier(glm.type === 'GLM' && glm.famille === 'poisson' && glm.lien === 'log' && glm.qualite.deviance_expliquee !== undefined
        && glm.signature !== lm.signature, 'GLM de Poisson (un autre ensemble AIC)');
      const gam = await api('ajuster', { projet: e.id, type: 'GAM', x: [v('sucre'), v('fermete')], y: [v('note_gout')], echelle: 'none', nom: 'R4',
        famille: 'gaussienne', lisses: [v('sucre')] });
      verifier(gam.type === 'GAM' && [].concat(gam.courbes.terme).length === 1, 'GAM, une courbe');
      const ef = await api('effets', { modele: gam.id });
      verifier([].concat(ef.effets).length === 2 && ef.effets[0].courbe === true && [].concat(ef.effets[0].x).length === 80, 'courbes d’effet');
      const se = await api('seuils', { modele: gam.id, terme: 'sucre' });
      verifier([].concat(se.x).length === 200 && [].concat(se.pente).length === 199 && se.edf > 0 && se.tirages === 2000, 'seuils d’une courbe (pente et bandes simultanées)');
      const cle = 'note_gout prédit';
      const pr = await api('predire', { modele: gam.id });
      verifier(pr.observations[cle].length === 120 && pr.observations[cle].every((x) => x !== null) && pr.observations.dans_modele.every(Boolean)
        && pr.observations.hors_domaine.every((x) => x === false), 'prédire les observations avec un GAM');
      const fp = path.join(dossier, 'a_predire.csv');
      fs.writeFileSync(fp, 'nom,sucre,fermete\nA,12,7\nB,40,7\n');
      const pf = await api('predire_fichier', { modele: lm.id, chemin: fp });
      verifier(pf.observations[cle].length === 2 && pf.observations.hors_domaine[0] === false && pf.observations.hors_domaine[1] === true,
        'prédire un fichier avec une régression, une extrapolation signalée');
      const ep = await api('exporter_prediction', { modele: lm.id, chemin: path.join(dossier, 'pred_lm.csv') });
      verifier(fs.readFileSync(ep.chemin, 'utf8').includes('note_gout prédit'), 'exporter la prédiction d’une régression');
      const fo = await api('formule', { projet: e.id, type: 'GAM', x: [v('sucre'), v('fermete')], y: [v('note_gout')], lisses: [v('sucre')],
        croises: [], transformations: {}, aleatoires: [], decalage: null });
      verifier(fo.formule === 'note_gout ~ s(sucre) + fermete', `formule R des réglages : ${fo.formule}`);
      const lu = await api('lire_formule', { projet: e.id, texte: 'note_gout ~ sucre * fermete + (1 | verger)', type: 'LM' });
      verifier(lu.type === 'LMM' && lu.x.length === 2 && lu.croises.length === 1 && lu.aleatoires[0].by === 'variable', 'formule lue en réglages');
      let refusF = '';
      try { await api('lire_formule', { projet: e.id, texte: 'note_gout ~ sucre + system("calc")', type: 'LM' }); } catch (err) { refusF = err.message; }
      verifier(/system/.test(refusF), 'une formule qui porte du code, refusée');
      const im = await api('imputer', { projet: e.id, variables: [v('sucre'), v('fermete'), v('acidite'), v('polyphenols')], k: 2 });
      verifier(im.imputation.valeurs === 11 && im.variables.find((x) => x.nom === 'polyphenols (imputée)').manquantes === 0 && im.variables.find((x) => x.nom === 'polyphenols').manquantes === 11
        && !im.variables.some((x) => x.nom === 'sucre (imputée)'),
        'imputation par ACP : une nouvelle variable pour celle qui avait des valeurs manquantes, moins de valeurs manquantes');
      let refusReg = '';
      try { await api('hca', { modele: lmm.id }); } catch (err) { refusReg = err.message; }
      verifier(/régression/.test(refusReg), 'une demande qui suppose des composantes, refusée pour une régression');
      const m = await api('ajuster', { projet: e.id, numero: e.modeles[2].numero, A: e.modeles[2].A });
      const txt = await api('methodes', { modele: m.id, langue: 'en', version: 'essai' });
      verifier([].concat(txt.paragraphes).some((p) => /PLS-DA/.test(p)) && [].concat(txt.references).length >= 3, 'méthodes');
      const x = e.variables.filter((y) => !y.qualitative).map((y) => y.uid);
      const c = await api('modeles_classe', { projet: e.id, x, classe: v('variete'), nom: 'MC', echelle: 'UV', A: 0 });
      verifier([].concat(c.modeles).length === 4 && c.modeles[0].groupe === 'MC', 'modèles de classe');
      const a = await api('appartenance', { modele: c.modeles[0].id });
      verifier([].concat(a.classes).length === 4 && [].concat(a.observations.nom).length === 120, 'appartenance');
      const o = await api('ajuster', { projet: e.id, type: 'OPLS-DA', x, classe: v('variete'), echelle: 'UV', A: 0 });
      verifier(o.Ap === 3 && o.A >= 3, `OPLS-DA : Ap = ${o.Ap}`);
    });
    await etape('test apparié : deux secteurs sur leurs années communes', async () => {
      // 3 secteurs suivis de 2001 à 2010, une ligne par secteur et par année, dans le désordre ;
      // S1 commence en 2002, S2 n'a pas de densité en 2006
      let g = 3;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = [];
      for (const s of ['S1', 'S2', 'S3']) {
        for (let an = 2001; an <= 2010; an += 1) {
          if (s !== 'S1' || an > 2001) lignes.push([`${s}_${an}`, s, an, s === 'S2' && an === 2006 ? '' : (2 + (s === 'S2' ? 0.5 : 0) + alea()).toFixed(3)]);
        }
      }
      lignes.sort((a, b) => (a[2] % 3) - (b[2] % 3) || a[0].localeCompare(b[0]));
      const q = await api('coller', { texte: ['nom\tsecteur\tannee\tdensite', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Secteurs' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const n = await api('niveaux', { projet: q.id, variable: v.secteur });
      verifier([].concat(n.niveaux).join() === 'S1,S2,S3', `niveaux : ${JSON.stringify(n.niveaux)}`);
      const r = await api('test_deux_variables', { projet: q.id, x: v.densite, y: v.secteur, appariement: v.annee, niveaux: ['S1', 'S2'] });
      // the paired t of the S2 − S1 differences over the years both have, computed here
      const val = (s, an) => { const l = lignes.find((x) => x[1] === s && x[2] === an); return l && l[3] !== '' ? Number(l[3]) : null; };
      const d = [];
      for (let an = 2001; an <= 2010; an += 1) if (val('S1', an) !== null && val('S2', an) !== null) d.push(val('S2', an) - val('S1', an));
      const m = d.reduce((s, x) => s + x, 0) / d.length;
      const et = Math.sqrt(d.reduce((s, x) => s + (x - m) ** 2, 0) / (d.length - 1));
      verifier(r.type === 'apparie' && r.n === 8 && d.length === 8 && Math.abs(r.tests.statistique[0] - m / (et / Math.sqrt(8))) < 1e-8 && r.tests.ddl1[0] === 7
        && [].concat(r.tests.test).join() === 't_apparie,wilcoxon' && [].concat(r.sans_partenaire.a).join() === '2006'
        && [].concat(r.sans_partenaire.b).join() === '2001' && r.manquantes === 1 && /\(n = 8 paires, appariées par annee\)\.$/.test(r.phrase), `test apparié : ${r.phrase}`);
      const defaut = await api('test_deux_variables', { projet: q.id, x: v.secteur, y: v.densite, appariement: v.annee });
      verifier(defaut.a === 'S1' && defaut.b === 'S2' && defaut.tests.p[0] === r.tests.p[0], 'A et B : les deux premiers niveaux par défaut');
      // three sectors: every pair of them, on its own shared years, Holm-corrected
      const tp = r.toutes;
      verifier([].concat(tp.a).join() === 'S1,S1,S2' && [].concat(tp.b).join() === 'S2,S3,S3' && [].concat(tp.n).join() === '8,9,9'
        && tp.p_t[0] === r.tests.p[0] && tp.p_t_holm.every((x, i) => x >= tp.p_t[i]), `toutes les paires : ${JSON.stringify(tp)}`);
      // within each site: only the pairs of sectors of the same site (S1 and S2 in A, S3 alone in B), Holm over those
      const ls = await api('coller', { texte: ['nom\tsecteur\tsite\tannee\tdensite', ...lignes.map((l) => [l[0], l[1], l[1] === 'S3' ? 'B' : 'A', l[2], l[3]].join('\t'))].join('\r\n'),
        nom: 'Secteurs et sites' });
      const vs = Object.fromEntries(ls.variables.map((x) => [x.nom, x.uid]));
      const ts = (await api('test_deux_variables', { projet: ls.id, x: vs.densite, y: vs.secteur, appariement: vs.annee, dans: vs.site })).toutes;
      verifier([].concat(ts.groupe).join() === 'A' && [].concat(ts.a).join() === 'S1' && [].concat(ts.b).join() === 'S2'
        && [].concat(ts.p_t)[0] === r.tests.p[0] && [].concat(ts.p_t_holm)[0] === [].concat(ts.p_t)[0], `paires dans chaque site : ${JSON.stringify(ts)}`);
      // a pair A–B that can't be tested (S3 has two years): its error apart, the table of all pairs still there, with why
      const deux = await api('coller', { texte: ['nom\tsecteur\tannee\tdensite', ...[2001, 2002, 2003, 2004].flatMap((an, i) => [`a${an}\tS1\t${an}\t${2 + i / 10}`,
        `b${an}\tS2\t${an}\t${3 + (i % 3) / 10}`]), 'c2001\tS3\t2001\t4', 'c2002\tS3\t2002\t5'].join('\r\n'), nom: 'Deux années' });
      const dv = Object.fromEntries(deux.variables.map((x) => [x.nom, x.uid]));
      const ab = await api('test_deux_variables', { projet: deux.id, x: dv.densite, y: dv.secteur, appariement: dv.annee, niveaux: ['S1', 'S3'] });
      verifier(/au moins trois paires/.test(ab.erreur) && !ab.tests && !ab.phrase && [].concat(ab.toutes.non_teste).join('|') === '|2 valeur(s) de annee en commun, il en faut 3|2 valeur(s) de annee en commun, il en faut 3',
        `paire A–B non testée : ${JSON.stringify(ab)}`);
      // two measurements of the same rows (before, after): paired by the observation
      const large = await api('coller', { texte: ['nom\tavant\tapres', 'a\t5,1\t5,6', 'b\t4,2\t4,9', 'c\t6,3\t6,2', 'd\t5,0\t', 'e\t3,9\t4,8', 'f\t4,4\t5,0'].join('\r\n'), nom: 'Avant-après' });
      const lv = Object.fromEntries(large.variables.map((x) => [x.nom, x.uid]));
      const w = await api('test_deux_variables', { projet: large.id, x: lv.avant, y: lv.apres, appariement: 'observation' });
      const dl = [5.6 - 5.1, 4.9 - 4.2, 6.2 - 6.3, 4.8 - 3.9, 5.0 - 4.4];
      const ml = dl.reduce((s, x) => s + x, 0) / 5;
      const sl = Math.sqrt(dl.reduce((s, x) => s + (x - ml) ** 2, 0) / 4);
      verifier(w.type === 'apparie' && w.ligne === true && w.n === 5 && w.manquantes === 1 && Math.abs(w.tests.statistique[0] - ml / (sl / Math.sqrt(5))) < 1e-8
        && [].concat(w.paires.cle).join() === 'a,b,c,e,f' && /une par observation\)\.$/.test(w.phrase), `en large : ${w.phrase}`);
      let refus = '';
      try { await api('test_deux_variables', { projet: q.id, x: v.densite, y: v.secteur, appariement: v.annee, niveaux: ['S1', 'S1'] }); } catch (err) { refus = err.message; }
      verifier(/deux niveaux différents/.test(refus), `deux fois le même niveau, refusé : « ${refus} »`);
    });
    await etape('séries : une variable au fil du temps, par secteur et par site (moteur/R/series.R)', async () => {
      // S1 dans le site A, S2 et S3 dans B, de 2001 à 2006, dans le désordre : S2 n'a pas 2004,
      // S3 n'a pas de densité en 2002 ; densité = année - 2000, + 10 pour S2, + 20 pour S3
      const lignes = [];
      for (const [s, site, plus] of [['S1', 'A', 0], ['S2', 'B', 10], ['S3', 'B', 20]]) {
        for (let an = 2001; an <= 2006; an += 1) {
          if (s !== 'S2' || an !== 2004) lignes.push([`${s}_${an}`, s, site, an, s === 'S3' && an === 2002 ? '' : String(an - 2000 + plus)]);
        }
      }
      lignes.reverse();
      const q = await api('coller', { texte: ['nom\tsecteur\tsite\tannee\tdensite', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Séries' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const r = await api('series', { projet: q.id, variable: v.densite, temps: v.annee, lignes: v.secteur, panneaux: v.site });
      const col = (x, k) => [].concat(x[k]);
      // the observations with a time and a value, by panel, line and time; S2's 2003 and 2005 are
      // two steps apart: the interface breaks its line there
      verifier(col(r.points, 'temps').join() === '2001,2002,2003,2004,2005,2006,2001,2002,2003,2005,2006,2001,2003,2004,2005,2006'
        && col(r.points, 'ligne').join() === 'S1,S1,S1,S1,S1,S1,S2,S2,S2,S2,S2,S3,S3,S3,S3,S3' && col(r.points, 'panneau').join() === 'A,A,A,A,A,A,B,B,B,B,B,B,B,B,B,B'
        && r.pas === 1 && r.sans_temps_valeur === 1 && r.sans_groupe === 0 && r.doubles.n === 0
        && [].concat(r.niveaux_lignes).join() === 'S1,S2,S3' && [].concat(r.niveaux_panneaux).join() === 'A,B', `points : ${JSON.stringify(r.points)}`);
      const i = col(r.points, 'nom').indexOf('S2_2005');
      verifier(col(r.points, 'valeur')[i] === 15 && col(r.points, 'uid')[i] === lignes.findIndex((l) => l[0] === 'S2_2005') + 1, 'nom, uid et valeur d’un point');
      // the mean of each panel's lines, year by year, over the sectors that have a value that year
      verifier(col(r.moyennes, 'panneau').join() === 'A,A,A,A,A,A,B,B,B,B,B,B' && col(r.moyennes, 'valeur').join() === '1,2,3,4,5,6,16,12,18,24,20,21'
        && col(r.moyennes, 'n').join() === '1,1,1,1,1,1,2,1,2,1,2,2', `moyennes : ${JSON.stringify(r.moyennes)}`);
      // lines by site, without panels: two sectors of B the same year are counted as duplicates
      const d = await api('series', { projet: q.id, variable: v.densite, temps: v.annee, lignes: v.site });
      verifier(d.doubles.n === 4 && d.doubles.ligne === 'B' && d.doubles.temps === 2001 && d.panneaux === null, `doublons : ${JSON.stringify(d.doubles)}`);
      // the same project with the site, the sector and the year as observation IDs (the year as text)
      const f = path.join(dossier, 'series.json');
      fs.writeFileSync(f, JSON.stringify({ format: 'usp-json/1', source: 'series.json', observation_id_names: ['Observation', 'site', 'secteur', 'an'],
        variable_id_names: 'Variable', observations: lignes.map((l, k) => ({ uid: k + 1, ids: [l[0], l[2], l[1], String(l[3])] })),
        variables: [{ uid: 1, ids: 'densite', formula: '' }],
        datasets: [{ name: 'series', observations: lignes.map((_, k) => k + 1), variables: [1], values: lignes.map((l) => [l[4] === '' ? null : Number(l[4])]) }],
        models: [], prediction_sets: [], dendrograms: [] }));
      const pj = await api('ouvrir', { chemin: f });
      const rang = Object.fromEntries([].concat(pj.identifiants).map((x) => [x.nom, `id:${x.rang}`]));
      const ri = await api('series', { projet: pj.id, variable: 1, temps: rang.an, lignes: rang.secteur, panneaux: rang.site });
      verifier(ri.temps.nom === 'an' && col(ri.points, 'nom').join() === col(r.points, 'nom').join() && col(ri.points, 'valeur').join() === col(r.points, 'valeur').join()
        && col(ri.points, 'temps').join() === col(r.points, 'temps').join(), `par identifiants : ${JSON.stringify(ri.points)}`);
      let refus = '';
      try { await api('series', { projet: pj.id, variable: 1, temps: rang.secteur }); } catch (err) { refus = err.message; }
      verifier(/numérique : « S3 » \(secteur\)/.test(refus), `un temps qui n’est pas un nombre, refusé : « ${refus} »`);
    });
    await etape('autocorrélation des résidus, rapports de vraisemblance entre modèles emboîtés', async () => {
      // 6 sites suivis 10 ans (2006 manque), un bruit autocorrélé dans chaque site (moteur/R/regression_diagnostics.R)
      let g = 11;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647 - 0.5; };
      const lignes = [];
      for (const s of ['A', 'B', 'C', 'D', 'E', 'F']) {
        const effet = alea();
        let bruit = 0;
        for (const an of [2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011]) {
          bruit = 0.7 * bruit + alea();
          const x = 2 * alea();
          lignes.push([`${s}${an}`, s, an, x.toFixed(4), (2 * alea()).toFixed(4), (1 + 0.5 * x + effet + bruit).toFixed(4)]);
        }
      }
      const q = await api('coller', { texte: ['nom\tsite\tannee\tx\tz\ty', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Sites' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const mixte = (nom, x, correlation) => api('ajuster', { projet: q.id, type: 'LMM', x, y: [v.y], echelle: 'none', nom,
        aleatoires: [{ by: 'variable', variable: v.site }], ...(correlation ? { correlation: { type: 'CAR1', temps: { by: 'variable', variable: v.annee } } } : {}) });
      const c1 = await mixte('C1', [v.x, v.z], true);
      const a = await api('autocorrelation_residus', { modele: c1.id });
      verifier(a.car1 === true && a.temps === 'annee' && a.groupe === 'site' && a.groupes === 6 && [].concat(a.decalage).length === 10
        && a.series.length === 2 && a.series[1].residus === 'normalises' && a.series[0].acf[0] === 1 && [].concat(a.bande).length === 10,
      `CAR1 : ses résidus et ses résidus normalisés : ${JSON.stringify(a).slice(0, 300)}`);
      const m2 = await mixte('M2', [v.x, v.z]);
      const m1 = await mixte('M1', [v.x]);
      const b = await api('autocorrelation_residus', { modele: m2.id, temps: { by: 'variable', variable: v.annee }, groupe: { by: 'effets' } });
      verifier(b.car1 === false && b.series.length === 1 && b.series[0].residus === 'pearson' && b.groupe === 'site' && b.n === 60,
        `sans CAR1, le temps et les groupes choisis : ${JSON.stringify(b).slice(0, 300)}`);
      const b2 = await api('autocorrelation_residus', { modele: m2.id, temps: { by: 'variable', variable: v.annee }, groupe: [{ by: 'variable', variable: v.site }] });
      verifier(b2.groupe === 'site' && b2.groupes === 6 && JSON.stringify(b2.series[0].acf) === JSON.stringify(b.series[0].acf),
        `les groupes d'une variable (une liste de groupements emboîtés) : ${JSON.stringify(b2).slice(0, 300)}`);
      let refusT = '';
      try { await api('autocorrelation_residus', { modele: m2.id, temps: { by: 'variable', variable: v.annee }, groupe: null }); } catch (err) { refusT = err.message; }
      verifier(/même temps/.test(refusT), `les années de tous les sites en une série, refusé : « ${refusT} »`);
      const r = await api('rapports_vraisemblance', { modeles: [m2.id, m1.id, c1.id, q.id] });
      const khi2 = 2 * (m2.qualite.logLik - m1.qualite.logLik);
      verifier(r.total === 1 && [].concat(r.paires.grand)[0] === m2.id && [].concat(r.paires.petit)[0] === m1.id && [].concat(r.paires.ddl)[0] === 1
        && Math.abs([].concat(r.paires.khi2)[0] - Math.max(0, khi2)) < 1e-8 && [].concat(r.paires.approche)[0] === false,
      `x + z contre x (pas le modèle avec CAR1) : ${JSON.stringify(r)}`);
    });
    await etape('paquets de lecture des formats', async () => {
      const r = await api('console', { code: 'all(vapply(c("haven", "readxl", "foreign", "zip"), requireNamespace, TRUE, quietly = TRUE))' });
      verifier(r.journal.some((e) => e.type === 'sortie' && e.texte.includes('[1] TRUE')), 'haven, readxl, foreign ou zip absent');
    });
    await etape('coller un tableau', async () => {
      const r = await api('coller', { texte: 'nom\tx\ty\r\na\t1,5\t2\r\nb\t3\t4,25\r\nc\t5\t1\r\n', nom: 'Collé' });
      verifier(r.nom === 'Collé' && r.modifie && r.variables.length === 2 && r.observations.nom[2] === 'c', `${r.variables.length} variables`);
    });
    await etape('importer des spectres (texte et JCAMP-DX), avec un tableau d’informations', async () => {
      const onde = Array.from({ length: 30 }, (_, k) => 1000 + 4 * k);
      const chemins = [];
      for (let i = 1; i <= 8; i += 1) {
        // two sources of variation (the band's height, the baseline's slope): models with 2 components
        const y = onde.map((x) => (0.2 + 0.05 * i) * Math.exp(-(((x - 1060) / 20) ** 2)) + 0.01 * i + (1 + (i % 3)) * 1e-4 * x);
        const f = path.join(dossier, `s${i}.${i % 2 ? 'csv' : 'jdx'}`);
        fs.writeFileSync(f, i % 2 ? `nm;abs\n${onde.map((x, k) => `${x};${String(y[k]).replace('.', ',')}`).join('\n')}\n`
          : ['##TITLE= s', '##JCAMP-DX= 4.24', '##XFACTOR= 1', '##YFACTOR= 1', '##FIRSTX= 1000', '##LASTX= 1116', '##NPOINTS= 30', '##XYDATA= (X++(Y..Y))',
            ...y.map((v, k) => `${onde[k]} ${v}`), '##END='].join('\n'));
        chemins.push(f);
      }
      const infos = path.join(dossier, 'infos.csv');
      fs.writeFileSync(infos, `echantillon,teneur\n${chemins.slice(0, 7).map((c, i) => `${path.basename(c)},${i + 1}`).join('\n')}\n`);
      const r = await api('importer_spectres', { chemins, infos, nom: 'Spectres' });
      verifier(r.observations.uid.length === 8 && r.variables.length === 31 && r.variables[0].nom === 'teneur' && r.variables[1].nom === '1000', `${r.variables.length} variables`);
      verifier([].concat(r.note).join() === '1 spectre(s) sans informations', [].concat(r.note).join());
      const x = r.variables.slice(1).filter((v) => +v.nom < 1040 || +v.nom > 1080).map((v) => v.uid);
      const m = await api('ajuster', { projet: r.id, type: 'PLS', x, y: [r.variables[0].uid], A: 1, nom: 'S', echelle: 'Ctr',
        spectres: [{ type: 'ligne_base' }, { type: 'detrend' }, { type: 'sg', derivee: 1, fenetre: 5, degre: 2 }] });
      const s = await api('spectres', { modele: m.id });
      verifier(s.variables.length === x.length && s.pretraites.length === 8 && s.pretraites[0].every(Number.isFinite), `${s.variables.length} variables, ${s.pretraites.length} spectres`);
      const m2 = await api('ajuster', { projet: r.id, type: 'PLS', x, y: [r.variables[0].uid], A: 2, nom: 'S2', echelle: 'Ctr',
        spectres: [{ type: 'sg', derivee: 1, fenetre: 5, degre: 2 }] });
      const i = await api('ipls', { modele: m2.id, n: 4, A: 2 });
      // (the whole spectrum is preprocessed first: even an interval smaller than the derivative's window fits)
      const ajustes = i.intervalles.filter((z) => z.A >= 1 && z.rmsecv.length === 2);
      verifier(i.intervalles.length === 4 && i.intervalles.flatMap((z) => z.termes).length === x.length && ajustes.length === 4
        && i.moyen.length === x.length && i.complet.rmsecv.length === 2, `iPLS : ${ajustes.length} intervalles ajustés sur ${i.intervalles.length}`);
    });
    await etape('exporter les données dans chaque format, puis les rouvrir', async () => {
      const formats = ['xlsx', 'csv', 'tsv', 'rds', 'RData', 'sav', 'dta', 'xpt', 'omv', 'dbf', 'arff'];
      // R files are opened only with R 4.4 or newer (moteur/R/formats.R: fichier_r_lisible)
      const v = /R version (\d+)\.(\d+)/.exec((await api('version')).R || '');
      const rRecent = !v || +v[1] > 4 || (+v[1] === 4 && +v[2] >= 4);
      for (const ext of formats) {
        const f = path.join(dossier, `donnees.${ext}`);
        await api('exporter_donnees', { projet: p.id, chemin: f });
        if (!rRecent && ['rds', 'RData'].includes(ext)) {
          let refus = '';
          try { await api('ouvrir', { chemin: f }); } catch (e) { refus = e.message; }
          verifier(/R 4\.4/.test(refus), `.${ext} avec R ${v[1]}.${v[2]} : « ${refus} »`);
          continue;
        }
        const q = await api('ouvrir', { chemin: f });
        verifier(q.observations.uid.length === 60 && q.variables.length === 9, `.${ext} : ${q.observations.uid.length} observations, ${q.variables.length} variables`);
        verifier(q.observations.nom[0] === 'o1', `.${ext} : noms des observations`);
      }
    });
    await etape('exporter les résultats (un fichier par tableau en SPSS) et la prédiction', async () => {
      const r = await api('exporter', { modele: pls.id, chemin: path.join(dossier, 'PLS.sav') });
      verifier(r.chemins.length === 5 && r.chemins.every((c) => fs.existsSync(c)), `${r.chemins.length} fichiers`);
      await api('predire', { modele: pls.id });
      const e = await api('exporter_prediction', { modele: pls.id, chemin: path.join(dossier, 'pred.csv') });
      verifier(fs.readFileSync(e.chemin, 'utf8').startsWith('"Observation"'), 'prédiction en CSV');
    });
    await etape('un fichier à plusieurs tableaux demande lequel ouvrir', async () => {
      const f = path.join(dossier, 'deux.xlsx');
      await api('console', { code: `openxlsx::write.xlsx(list(a = data.frame(x = 1:3, y = 3:1), b = data.frame(z = 1:4, w = 4:1)), ${JSON.stringify(f.replace(/\\/g, '/'))})` });
      const c = await api('ouvrir', { chemin: f });
      verifier(Array.isArray(c.choisir) && c.choisir.join() === 'a,b' && !c.id, 'liste des feuilles');
      const q = await api('ouvrir', { chemin: f, table: 'b' });
      verifier(q.observations.uid.length === 4 && q.table === 'b' && q.nom === 'deux (b).xlsx', q.nom);
    });
    await etape('RDA (Hellinger, partielle, permutée par site) et analyse des correspondances', async () => {
      // 30 relevés de 6 espèces le long de deux gradients (température, sable), 5 sites
      let g = 11;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\tsite\ttemp\tsable\tsp1\tsp2\tsp3\tsp4\tsp5\tsp6'];
      for (let i = 1; i <= 30; i += 1) {
        const temp = 8 + 10 * alea();
        const sable = 100 * alea();
        const sp = [temp, 20 - temp, sable / 10, 10 - sable / 10, (temp * sable) / 100, 3].map((v) => Math.round(v * (0.5 + alea())));
        lignes.push([`r${i}`, `S${1 + (i % 5)}`, temp.toFixed(2), sable.toFixed(1), ...sp].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Relevés' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const especes = ['sp1', 'sp2', 'sp3', 'sp4', 'sp5', 'sp6'].map((n) => v[n]);
      const rda = await api('ajuster', { projet: q.id, type: 'RDA', x: [v.temp], y: especes, echelle: 'none', nom: 'RDA',
        options: { transformation: 'hellinger', permutations: 99, graine: 2, z: [v.sable], strates: { by: 'variable', variable: v.site } } });
      verifier(rda.analyse === true && rda.prediction === false && rda.A === 0 && rda.N === 30 && /^R2aj /.test(rda.chiffres)
        && rda.ordination['2'].sites.RDA1.length === 30 && [].concat(rda.vues_defaut).join() === 'ordination,valeurs_propres', `RDA : ${rda.chiffres}, N = ${rda.N}`);
      verifier(rda.reglages.options.transformation === 'hellinger' && [].concat(rda.reglages.options.z).join() === String(v.sable) && rda.strates === 'site',
        `réglages : ${JSON.stringify(rda.reglages.options)}`);
      const tests = await api('rda_tests', { modele: rda.id });
      verifier([].concat(tests.axes.axe).length === 2 && [].concat(tests.termes.terme).length === 2 && typeof [].concat(tests.axes.p)[0] === 'number', 'tests par axe et par terme');
      const re = await api('reajuster', { modele: rda.id, exclues: [1] });
      verifier(re.type === 'RDA' && re.N === 29 && re.A === 0, `réajustée : N = ${re.N}, A = ${re.A}`);
      const s = await api('script_r', { modele: rda.id, chemin: path.join(dossier, 'rda.R'), version: 'essai' });
      verifier(fs.existsSync(s.script) && !s.ecart && !/vegan::/.test(fs.readFileSync(s.script, 'utf8').replace(/#.*$/gm, '')), `script R : ${s.ecart || 'R seul'}`);
      const e = await api('exporter', { modele: rda.id, chemin: path.join(dossier, 'rda.xlsx') });
      verifier(fs.existsSync(e.chemin), 'exporter les résultats');
      const mt = await api('methodes', { modele: rda.id, langue: 'en' });
      verifier(mt.paragraphes.some((x) => /redundancy analysis/.test(x) && /Hellinger/.test(x)) && mt.references.some((x) => /Legendre/.test(x)), 'texte des méthodes');
      let refus = '';
      try { await api('hca', { modele: rda.id }); } catch (err) { refus = err.message; }
      verifier(/autre analyse/.test(refus), `dendrogramme refusé : « ${refus} »`);
      const ca = await api('ajuster', { projet: q.id, type: 'CA', x: especes, echelle: 'none', nom: 'CA' });
      verifier(ca.type === 'CA' && [].concat(ca.contributions.colonnes.nom).length === 6 && [].concat(ca.valeurs_propres.axe).length === 5
        && Object.keys(ca.ordination).join() === '1,2,3', `CA : ${ca.chiffres}`);
      let refusCA = '';
      try { await api('ajuster', { projet: q.id, type: 'CA', x: [v.temp, v.site], echelle: 'none' }); } catch (err) { refusCA = err.message; }
      verifier(/quantitatives/.test(refusCA), `CA d'une variable qualitative refusée : « ${refusCA} »`);
    });
    await etape('arbres de décision, LDA et k-moyennes (moteur/R/arbres.R, discrimination.R, kmoyennes.R)', async () => {
      // 12 sites suivis 6 ans : une densité qui baisse au-delà de 12 jours de gel, des X manquants, une classe
      let g = 5;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\tsite\tgel\thaie\tpluie\tsol\tdensite\tniveau'];
      for (let i = 0; i < 72; i += 1) {
        const gel = Math.round(30 * alea());
        const haie = alea();
        const densite = 2 + (gel > 12 ? -1.2 : 0) + (haie > 0.5 ? 0.6 : 0) + (alea() - 0.5);
        lignes.push([`r${i + 1}`, `S${1 + Math.floor(i / 6)}`, i % 11 === 0 ? '' : gel, haie.toFixed(3), (200 * alea()).toFixed(1), ['argile', 'limon'][i % 2],
          densite.toFixed(3), densite > 1.5 ? 'haute' : 'basse'].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Sites' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const xs = [v.gel, v.haie, v.pluie, v.sol];
      const vc = { by: 'variable', variable: v.site };
      const arbre = await api('ajuster', { projet: q.id, type: 'TREE', x: xs, y: [v.densite], vc, echelle: 'none', nom: 'Arbre', options: { minbucket: 8 } });
      verifier(arbre.analyse === true && arbre.prediction === true && arbre.A === 0 && arbre.N === 72 && arbre.G === 12 && /^R2 .* feuilles$/.test(arbre.chiffres)
        && [].concat(arbre.vues_defaut).join() === 'arbre,obspred' && arbre.arbre.id[0] === 1 && arbre.observations['densite ajusté'].length === 72
        && typeof arbre.comparaison.Q2 === 'number' && arbre.reglages.options.minbucket === 8, `arbre : ${arbre.chiffres}, N = ${arbre.N}, G = ${arbre.G}`);
      const st = await api('arbre_stabilite', { modele: arbre.id });
      verifier(st.tirages === 200 && [].concat(st.stabilite.variable).length >= 1 && Math.abs([].concat(st.stabilite.frequence).reduce((a, b) => a + b, 0) - 100) < 1e-6,
        `stabilité : ${JSON.stringify(st.stabilite)}`);
      const classement = await api('ajuster', { projet: q.id, type: 'TREE', x: xs, classe: v.niveau, vc, echelle: 'none', nom: 'Arbre classes', options: { minbucket: 8 } });
      verifier(classement.classement === true && [].concat(classement.vues_defaut).join() === 'arbre,classement' && classement.classification.vc.table.length === 2
        && classement.reglages.classe === v.niveau, `arbre de classement : ${classement.chiffres}`);
      const lda = await api('ajuster', { projet: q.id, type: 'LDA', x: [v.gel, v.haie, v.pluie], classe: v.niveau, vc, echelle: 'none', nom: 'LDA', options: { priors: 'egales' } });
      verifier(lda.type === 'LDA' && [].concat(lda.axes).join() === 'LD1' && lda.N === 65 && lda.observations.LD1.length === 65 && lda.priors.prior[0] === 0.5
        && [].concat(lda.vues_defaut).join() === 'lda_scores,classement', `LDA : ${lda.chiffres}, N = ${lda.N}`);
      const km = await api('ajuster', { projet: q.id, type: 'KMEANS', x: [v.gel, v.haie, v.pluie], echelle: 'UV', nom: 'Groupes', options: { k: 3 } });
      verifier(km.k === 3 && km.N === 65 && [].concat(km.tailles.n).reduce((a, b) => a + b, 0) === 65 && typeof km.silhouette === 'number'
        && km.observations.CP2.length === 65, `k-moyennes : ${km.chiffres}`);
      const ch = await api('kmeans_choix', { modele: km.id });
      verifier([].concat(ch.choix.k).join() === '2,3,4,5,6,7,8,9,10', `choix de k : ${[].concat(ch.choix.k).join()}`);
      const pv = await api('kmeans_variable', { projet: q.id, modele: km.id, nom: 'groupes km' });
      const nv = pv.variables.find((x) => x.nom === 'groupes km');
      verifier(nv && nv.qualitative && [].concat(nv.categories).join() === '1,2,3' && pv.annulable, 'groupes en variable du projet');
      const an = (await api('annuler', { projet: q.id })).projet;
      verifier(!an.variables.some((x) => x.nom === 'groupes km'), 'annuler la variable des groupes');
      const pr = await api('predire', { modele: arbre.id });
      verifier(pr.observations['densite prédit'].length === 72 && pr.observations.feuille.length === 72, 'prédire avec l’arbre');
      const prl = await api('predire', { modele: lda.id });
      verifier(prl.observations['classe prédite'].length === 72 && prl.observations['probabilité haute'].length === 72, 'prédire avec la LDA');
      const re = await api('reajuster', { modele: arbre.id, exclues: [1, 2] });
      verifier(re.type === 'TREE' && re.N === 70 && re.A === 0, `arbre réajusté : N = ${re.N}`);
      for (const [m, f] of [[arbre, 'arbre.R'], [lda, 'lda.R'], [km, 'km.Rmd']]) {
        const s = await api('script_r', { modele: m.id, chemin: path.join(dossier, f), version: 'essai' });
        verifier(fs.existsSync(s.script) && !s.ecart, `script ${f} : ${s.ecart || 'sans écart'}`);
      }
      const e = await api('exporter', { modele: classement.id, chemin: path.join(dossier, 'arbre.xlsx') });
      verifier(fs.existsSync(e.chemin), 'exporter les résultats de l’arbre');
      const mt = await api('methodes', { modele: arbre.id, langue: 'en' });
      verifier(mt.paragraphes.some((x) => /rpart/.test(x)) && mt.paragraphes.some((x) => /1-SE rule/.test(x)) && mt.references.some((x) => /Breiman/.test(x)),
        `texte des méthodes de l’arbre : ${mt.paragraphes.join(' ').slice(0, 300)}`);
      let refus = '';
      try { await api('ajuster', { projet: q.id, type: 'TREE', x: xs, y: [v.densite], classe: v.niveau, vc, echelle: 'none' }); } catch (err) { refus = err.message; }
      verifier(/pas les deux/.test(refus), `arbre à deux réponses refusé : « ${refus} »`);
      let refusHCA = '';
      try { await api('hca', { modele: km.id }); } catch (err) { refusHCA = err.message; }
      verifier(/autre analyse/.test(refusHCA), `dendrogramme refusé : « ${refusHCA} »`);
    });
    await etape('retirer un modèle et sa définition', async () => {
      const avant = (await api('projet', { projet: p.id })).modeles.length;
      const r = await api('supprimer_modele', { modele: da.id, definition: true });
      verifier(r.projet.modeles.length === avant - 1, 'définition retirée');
    });
  } finally {
    try { execFileSync('taskkill', ['/pid', String(serveur.pid), '/T', '/F'], { stdio: 'ignore' }); } catch { serveur.kill(); }
    fs.rmSync(dossier, { recursive: true, force: true });
  }
  if (echecs.length) {
    console.log(`\n${echecs.length} vérification(s) en échec.`);
    process.exit(1);
  }
  console.log('\nToutes les vérifications sont passées.');
  process.exit(0);
}

principal().catch((e) => { console.error(e); process.exit(1); });
