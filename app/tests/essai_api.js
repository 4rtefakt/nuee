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
  const serveur = spawn(rscript, [path.join(depot, 'moteur', 'serveur.R'), String(port), interfaceDir], { windowsHide: true });
  let sortie = '';
  await new Promise((resolve, reject) => {
    serveur.stdout.on('data', (d) => { sortie += d; if (sortie.includes('PRET')) resolve(); });
    serveur.stderr.on('data', (d) => { sortie += d; });
    serveur.on('exit', (code) => reject(new Error(`R s'est arrêté (code ${code}) : ${sortie.slice(-800)}`)));
  });
  const api = async (nom, q = {}) => {
    const r = await fetch(`http://127.0.0.1:${port}/api/${nom}`, { method: 'POST', body: JSON.stringify(q) });
    const j = await r.json();
    if (!r.ok) throw new Error(`${nom} : ${j.erreur}`);
    return j;
  };
  const uid = {};
  let p; let pca; let pls; let opls; let da;
  try {
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
      const f = path.join(dossier, 'projet.json');
      await api('enregistrer', { projet: p.id, chemin: f, session: JSON.stringify({ version: 2, modeles: [{ numero: pls.numero, A: 2 }] }) });
      const q = await api('ouvrir', { chemin: f });
      verifier(q.session === true, 'séance enregistrée');
      const m = await api('ajuster', { projet: q.id, numero: pls.numero, A: 2 });
      verifier(m.note === 'essai' && m.reglages.x.length === 6, 'modèle relu');
    });
    await etape('script R autonome', async () => {
      const r = await api('script_r', { modele: pls.id, chemin: path.join(dossier, 'pls.R'), version: 'essai' });
      verifier(fs.existsSync(r.script) && [].concat(r.chemins).length === 1 && !r.ecart, 'script seul, mêmes résultats');
      const texte = fs.readFileSync(r.script, 'utf8');
      verifier(!texte.includes('charger.R') && texte.includes('ajuster <- function'), 'sans Nuée');
      const s = await api('script_r', { modele: pls.id, chemin: path.join(dossier, 'pls2.R'), donnees: 'csv', renv: true, rdata: true, rproj: true });
      verifier([].concat(s.chemins).length === 5 && [].concat(s.chemins).every((c) => fs.existsSync(c)), 'CSV, renv.lock, .RData, .Rproj');
    });
    await etape('console R et journal', async () => {
      const r = await api('console', { code: 'm$A', modele: pls.id });
      verifier(r.journal.some((e) => e.type === 'sortie' && e.texte.includes('[1] 2')), 'console');
      verifier((await api('journal', { depuis: 0 })).entrees.some((e) => e.type === 'appel'), 'journal');
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
        const y = onde.map((x) => (0.2 + 0.05 * i) * Math.exp(-(((x - 1060) / 20) ** 2)) + 0.01 * i + 1e-4 * x);
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
