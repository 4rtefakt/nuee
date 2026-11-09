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
  for (const f of fs.readdirSync(interfaceDir).filter((x) => x.endsWith('.js'))) {
    await etape(`syntaxe ${f}`, () => execFileSync(process.execPath, ['--check', path.join(interfaceDir, f)], { stdio: 'pipe' }));
  }

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
      let r = await api('variable_calculee', { projet: p.id, nom: 'x1x2', formule: 'x1 * x2' });
      verifier(r.variables.length === 10 && r.annulable, 'variable calculée');
      r = (await api('annuler', { projet: p.id })).projet;
      verifier(r.variables.length === 9, 'annuler la variable');
      await api('modifier_valeur', { projet: p.id, observation: 1, variable: uid.x1, valeur: '12,5' });
      const d = await api('donnees', { projet: p.id });
      verifier(d.valeurs[d.variables.indexOf('x1')][0] === 12.5, 'valeur modifiée');
      await api('annuler', { projet: p.id });
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
      for (const ext of formats) {
        const f = path.join(dossier, `donnees.${ext}`);
        await api('exporter_donnees', { projet: p.id, chemin: f });
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
