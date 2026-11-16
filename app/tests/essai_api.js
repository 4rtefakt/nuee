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
  // the documents' references and their rendering (Word, Quarto), without the page; Which analysis?'s rules;
  // the figures' layout and their PNG and TIFF files; the interface's numbers in each language; an
  // update of the data read for its dialogs
  for (const f of ['essai_references.mjs', 'essai_documents.mjs', 'essai_bibliographie.mjs', 'essai_zotero.js', 'essai_choix.mjs', 'essai_figures.mjs', 'essai_nombres.mjs',
    'essai_etapes.mjs']) {
    await etape(f, () => execFileSync(process.execPath, ['--no-warnings', path.join(__dirname, f)], { stdio: 'pipe' }));
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
    await etape("l'interface est relue à chaque chargement : Cache-Control no-cache, 304 quand rien n'a changé", async () => {
      for (const chemin of ['/', '/nouveautes.js']) {
        const r = await brute(chemin, {});
        verifier(r.statut === 200 && r.entetes['cache-control'] === 'no-cache' && !!r.entetes['last-modified'],
          `${chemin} : HTTP ${r.statut}, Cache-Control ${r.entetes['cache-control']}, Last-Modified ${r.entetes['last-modified']}`);
        const r2 = await brute(chemin, { 'If-Modified-Since': r.entetes['last-modified'] });
        verifier(r2.statut === 304, `${chemin} revalidé : HTTP ${r2.statut}`);
      }
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
      const s = await api('statistiques_variables', { projet: p.id });
      verifier(s.nom.length === 9, 'statistiques');
      // (whole numbers and the share of zeros, for Which analysis?: none here, nothing for a qualitative variable)
      verifier(s.entiers.every((e, i) => (s.qualitative[i] ? e === null : e === false)) && s.zeros.every((z, i) => (s.qualitative[i] ? z === null : z === 0)),
        `statistiques : entiers ${JSON.stringify(s.entiers)}, zéros ${JSON.stringify(s.zeros)}`);
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
      // nor a step back for Ctrl+Z (both changes above were undone: nothing is left to undo)
      const apresRefus = await api('projet', { projet: p.id });
      verifier(refus && apresRefus.revision === apres && !apresRefus.annulable,
        `un changement refusé ne compte ni révision ni retour en arrière (${refus} ; ${apresRefus.annulable})`);
    });
    await etape('indices de diversité en Nouvelle variable (richesse, shannon, simpson, pielou)', async () => {
      // three surveys of four species, the site before them: an ordinary one (10, 5, 0, 1), an empty
      // one, a missing value; the columns by a range in the table's order, a list, or both
      const lignes = [['r1', 'A', 10, 5, 0, 1], ['r2', 'A', 0, 0, 0, 0], ['r3', 'B', 3, '', 2, 1]];
      const q = await api('coller', { texte: ['nom\tsite\tAblette\tBrochet\tGardon\tVandoise', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Poissons' });
      await api('variable_calculee', { projet: q.id, nom: 'H', formule: 'shannon(Ablette:Vandoise)' });
      await api('variable_calculee', { projet: q.id, nom: 'S', formule: 'richesse(Ablette, Brochet, Gardon, Vandoise)' });
      const r = await api('variable_calculee', { projet: q.id, nom: 'D', formule: 'simpson(Ablette:Brochet, Gardon, Vandoise)' });
      const d = await api('donnees', { projet: q.id });
      const col = (n) => d.valeurs[d.variables.indexOf(n)];
      const H1 = -[10, 5, 1].map((x) => x / 16).reduce((s, x) => s + x * Math.log(x), 0);
      verifier(Math.abs(col('H')[0] - H1) < 1e-12 && col('H')[1] === 0 && col('H')[2] === null && col('S').join() === '3,0,' && Math.abs(col('D')[0] - 130 / 256) < 1e-12
        && col('D')[1] === 0 && col('D')[2] === null && r.annulable, `indices : ${JSON.stringify([col('H'), col('S'), col('D')])}`);
      const a = (await api('annuler', { projet: q.id })).projet;
      let refus = '';
      try { await api('variable_calculee', { projet: q.id, nom: 'J', formule: 'pielou(Ablette, Brochet - 6)' }); } catch (err) { refus = err.message; }
      verifier(/: pielou\(\) : Brochet - 6 a une valeur négative \(-1\)/.test(refus) && a.variables.length === 7, `refusé : ${refus} ; annuler : ${a.variables.length} variables`);
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
    await etape('documents : écrire, relire, annuler sans y toucher, enregistrer et rouvrir, supprimer', async () => {
      // (documents.js: Markdown kept in the project and saved with it; Ctrl+Z leaves it alone)
      let r = await api('document_enregistrer', { projet: p.id, titre: 'Article', langue: 'fr', texte: 'Q² = {{M2.Q2}}' });
      const id = r.id;
      verifier(/^D\d+$/.test(id) && r.projet.modifie && r.projet.documents.some((d) => d.id === id && d.titre === 'Article'), `créé : ${JSON.stringify(r.projet.documents)}`);
      const bib = '@article{wold2001, author = {Wold, S.}, title = {PLS}, year = 2001}';
      await api('document_enregistrer', { projet: p.id, id, texte: 'Q² = {{M2.Q2}} ; N = {{M2.N}}', langue: 'en', bib, style: 'numerique' });
      let d = await api('document', { projet: p.id, id });
      verifier(d.texte === 'Q² = {{M2.Q2}} ; N = {{M2.N}}' && d.titre === 'Article' && d.langue === 'en' && d.bib === bib && d.style === 'numerique', `relu : ${JSON.stringify(d)}`);
      await api('variable_calculee', { projet: p.id, nom: 'x1x3', formule: 'x1 * x3' });
      await api('annuler', { projet: p.id });
      d = await api('document', { projet: p.id, id });
      verifier(d.texte.endsWith('{{M2.N}}'), 'annuler laisse le document tel qu’il est');
      const f = path.join(dossier, 'documents.nuee');
      await api('enregistrer', { projet: p.id, chemin: f });
      const q = await api('ouvrir', { chemin: f });
      d = await api('document', { projet: q.id, id });
      verifier(d.texte.endsWith('{{M2.N}}') && d.langue === 'en' && d.bib === bib && q.documents.length === 1, 'relu du fichier');
      // a bibliography file beside the project, named relative to it (as Quarto reads it), or not
      fs.writeFileSync(path.join(dossier, 'refs.bib'), '﻿@book{eriksson2006, author = {Eriksson, L.}, title = {Multi- and megavariate}, year = 2006}\n');
      const b1 = await api('bibliographie_fichier', { projet: q.id, chemin: 'refs.bib' });
      const b2 = await api('bibliographie_fichier', { projet: q.id, chemin: path.join(dossier, 'refs.bib') });
      verifier(b1.texte.startsWith('@book{eriksson2006') && b2.texte === b1.texte && /\/refs\.bib$/.test(b1.chemin), `bibliographie : ${JSON.stringify(b1).slice(0, 120)}`);
      const refusB = await Promise.all([path.join(dossier, 'documents.nuee'), 'absent.bib'].map((c) => api('bibliographie_fichier', { projet: q.id, chemin: c }).then(() => '', (e) => e.message)));
      verifier(refusB.every(Boolean), `refusés : ${JSON.stringify(refusB)}`);
      r = await api('document_supprimer', { projet: q.id, id });
      let refus = '';
      try { await api('document', { projet: q.id, id }); } catch (e) { refus = e.message; }
      verifier(!r.projet.documents.length && refus, `supprimé (${refus})`);
      await api('fermer_projet', { projet: q.id });
    });
    await etape('figures : créer, relire, annuler sans y toucher, enregistrer et rouvrir, renommer, supprimer ; un projet d’avant les figures', async () => {
      // (figures.js: the project's plots composed for a journal, their settings as JSON text the
      // interface alone reads, kept in the project; Ctrl+Z leaves them alone)
      const contenu = JSON.stringify({ version: 1, largeur: 174, hauteur: 0, lignes: 1, colonnes: 2, police: 8, ligne: 1,
        panneaux: [{ vue: 'scores', numero: pls.numero, nom: 'PLS', loc: { a: 1, b: 2, couleur: 'm@2|t1' }, zoom: { x: [-2, 2], y: null } }, { vue: 'carte', numero: null, loc: {} }] });
      let r = await api('figure_enregistrer', { projet: p.id, titre: 'Figure 1', contenu });
      const id = r.id;
      verifier(/^F\d+$/.test(id) && r.projet.modifie && r.projet.figures.some((f) => f.id === id && f.titre === 'Figure 1'), `créée : ${JSON.stringify(r.projet.figures)}`);
      // (one panel: an array of one, which must stay one; accents)
      const un = JSON.stringify({ panneaux: [{ vue: 'effets', numero: 3, loc: { terme: 'année' } }], lettres: { style: '(a)', gras: false } });
      const id2 = (await api('figure_enregistrer', { projet: p.id, titre: 'Fig. 2 — données', contenu: un })).id;
      let f = await api('figure', { projet: p.id, id });
      verifier(f.contenu === contenu && f.titre === 'Figure 1' && id2 !== id, `relue : ${JSON.stringify(f).slice(0, 200)}`);
      await api('variable_calculee', { projet: p.id, nom: 'x2x3', formule: 'x2 * x3' });
      const a = (await api('annuler', { projet: p.id })).projet;
      verifier(a.figures.length === 2 && (await api('figure', { projet: p.id, id })).contenu === contenu, 'annuler laisse les figures telles quelles');
      const fichier = path.join(dossier, 'figures.nuee');
      await api('enregistrer', { projet: p.id, chemin: fichier });
      const lu = JSON.parse(fs.readFileSync(fichier, 'utf8'));
      verifier(Array.isArray(lu.figures) && lu.figures.length === 2 && lu.figures.every((x) => typeof x.contenu === 'string'), `dans le fichier : ${JSON.stringify(lu.figures).slice(0, 200)}`);
      const q = await api('ouvrir', { chemin: fichier });
      f = await api('figure', { projet: q.id, id });
      const f2 = await api('figure', { projet: q.id, id: id2 });
      verifier(q.figures.length === 2 && f.contenu === contenu && f2.contenu === un && f2.titre === 'Fig. 2 — données'
        && JSON.parse(f2.contenu).panneaux.length === 1, `relues du fichier : ${JSON.stringify(q.figures)}`);
      // renamed: the title only, its content as it was; then deleted
      r = await api('figure_enregistrer', { projet: q.id, id, titre: 'Figure 1 bis' });
      f = await api('figure', { projet: q.id, id });
      verifier(f.titre === 'Figure 1 bis' && f.contenu === contenu && r.projet.figures.find((x) => x.id === id).titre === 'Figure 1 bis', 'renommée');
      r = await api('figure_supprimer', { projet: q.id, id });
      let refus = '';
      try { await api('figure', { projet: q.id, id }); } catch (e) { refus = e.message; }
      verifier(r.projet.figures.length === 1 && refus, `supprimée (${refus})`);
      await api('fermer_projet', { projet: q.id });
      // a project saved before figures existed opens, without any
      delete lu.figures;
      const ancien = path.join(dossier, 'sans-figures.nuee');
      fs.writeFileSync(ancien, JSON.stringify(lu));
      const v = await api('ouvrir', { chemin: ancien });
      verifier(Array.isArray(v.figures) && v.figures.length === 0, `projet sans figures : ${JSON.stringify(v.figures)}`);
      const n = await api('figure_enregistrer', { projet: v.id, titre: 'Nouvelle' });
      verifier(n.id === 'F1' && (await api('figure', { projet: v.id, id: 'F1' })).contenu === '{}', `première figure : ${n.id}`);
      await api('fermer_projet', { projet: v.id });
    });
    await etape('enregistrer sans risque : version, fichier changé par ailleurs, projet reçu', async () => {
      const f = path.join(dossier, 'partage.nuee');
      await api('enregistrer', { projet: p.id, chemin: f, version: '9.9.9' });
      const lu = JSON.parse(fs.readFileSync(f, 'utf8'));
      verifier(lu.nuee && lu.nuee.format === 3 && lu.nuee.version === '9.9.9' && !lu.chemin_enregistrement, `en-tête : ${JSON.stringify(lu.nuee)}`);
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
    await etape('une condition (onglet Observations) : le compte, les refus, le modèle, ses réajustements, l’enregistrement', async () => {
      const condition = 'zone != "A" & x1 > 0';
      const t = await api('donnees', { projet: p.id });
      const zone = t.valeurs[t.variables.indexOf('zone')];
      const x1 = t.valeurs[t.variables.indexOf('x1')];
      const vraie = t.uid_observations.map((u, i) => zone[i] !== 'A' && x1[i] !== null && x1[i] > 0);
      const parmi = (exclues) => t.uid_observations.filter((u, i) => vraie[i] && !exclues.includes(u));
      // the count under the field: the ticked observations, without the test set, where it is TRUE
      const c = await api('condition_observations', { projet: p.id, condition, exclues: [1, 2], test: [3] });
      verifier(c.n === parmi([1, 2, 3]).length && c.total === 57, `compte : ${JSON.stringify(c)}, attendu ${parmi([1, 2, 3]).length} sur 57`);
      const vide = await api('condition_observations', { projet: p.id, condition: ' ', exclues: [], test: [] });
      verifier(vide.n === 60 && vide.total === 60, `sans condition : ${JSON.stringify(vide)}`);
      // refused, saying why, and never run
      for (const [cond, motif] of [['zones == "A"', /Variable introuvable : zones/], ['file.create("intrus.txt")', /file\.create\(\) » n'est pas permis/],
        ['c(TRUE, FALSE)', /une valeur par observation \(60\) ; elle en donne 2/], ['x1', /TRUE ou FALSE/], ['x1 >', /Condition illisible/]]) {
        let refus = '';
        try { await api('condition_observations', { projet: p.id, condition: cond }); } catch (e) { refus = e.message; }
        verifier(motif.test(refus), `${cond} : « ${refus} »`);
      }
      let anglais = '';
      try { await api('condition_observations', { projet: p.id, condition: 'system("dir") == 0' }, 'en'); } catch (e) { anglais = e.message; }
      verifier(/isn't allowed in a condition/.test(anglais), `en anglais : « ${anglais} »`);
      // the model takes the ticked observations where it is TRUE; its settings keep the ticks and the condition
      const m = await api('ajuster', { projet: p.id, type: 'PLS', x, y: [uid.y], A: 2, nom: 'PLScond', exclues: [1, 2], condition });
      const attendues = parmi([1, 2]);
      verifier(m.N === attendues.length && JSON.stringify(m.observations.uid) === JSON.stringify(attendues), `N ${m.N}, attendu ${attendues.length}`);
      verifier(m.reglages.condition === condition && JSON.stringify([].concat(m.reglages.exclues)) === '[1,2]', `réglages : ${JSON.stringify([m.reglages.condition, m.reglages.exclues])}`);
      // refits keep it: a component more, the VIP variables, the selection excluded (ticks too)
      const r1 = await api('reajuster', { modele: m.id, A: 3 });
      const r2 = await api('reajuster', { modele: m.id, garder: ['x1', 'x2', 'x4'] });
      const sel = m.observations.uid.slice(0, 2);
      const r3 = await api('reajuster', { modele: m.id, exclues: sel, A: 2 });
      verifier(r1.N === m.N && r2.N === m.N && r3.N === m.N - 2 && [r1, r2, r3].every((r) => r.reglages.condition === condition)
        && sel.every((u) => [].concat(r3.reglages.exclues).includes(u)), `réajustés : N ${r1.N}, ${r2.N}, ${r3.N}`);
      // a regression's AIC selection, then Fit this model: the same observations
      const lm = await api('ajuster', { projet: p.id, type: 'LM', x: [uid.x1, uid.x2], y: [uid.y], echelle: 'none', nom: 'LMcond', condition });
      await api('selection_aic', { modele: lm.id });
      const choisi = await api('reajuster', { modele: lm.id, groupes_aic: ['x2'] });
      verifier(choisi.N === lm.N && choisi.reglages.condition === condition, `Ajuster ce modèle : N ${choisi.N} (${lm.N})`);
      // the data changed: refitted, the condition takes the new values; undone, back as it was
      const i = t.uid_observations.findIndex((u, k) => zone[k] !== 'A' && x1[k] !== null && x1[k] <= 0 && ![1, 2].includes(u));
      await api('modifier_valeur', { projet: p.id, observation: t.uid_observations[i], variable: uid.x1, valeur: '5' });
      const apres = await api('ajuster', { projet: p.id, numero: m.numero });
      await api('annuler', { projet: p.id });
      const revenu = await api('ajuster', { projet: p.id, numero: m.numero });
      verifier(apres.N === m.N + 1 && revenu.N === m.N, `après le changement : N ${apres.N}, annulé : ${revenu.N}`);
      // saved with the project and its session: reopened, refitted with the condition
      const f = path.join(dossier, 'condition.nuee');
      await api('enregistrer', { projet: p.id, chemin: f, session: JSON.stringify({ version: 2, modeles: [{ numero: m.numero, A: 2 }] }) });
      const lu = JSON.parse(fs.readFileSync(f, 'utf8')).models.find((d) => d.name === 'PLScond');
      verifier(lu.condition === condition && [].concat(lu.cochees).length === 58 && [].concat(lu.observations).length === m.N, 'définition enregistrée');
      const q = await api('ouvrir', { chemin: f });
      const relu = await api('ajuster', { projet: q.id, numero: m.numero, A: 2 });
      verifier(relu.N === m.N && relu.reglages.condition === condition && JSON.stringify(relu.observations.uid) === JSON.stringify(attendues), `relu : N ${relu.N}`);
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
      // in English, the same apples in English (orchards.json); in French, vergers.json
      const en = await api('exemple', {}, 'en');
      const noms = (p) => p.variables.map((x) => x.nom);
      verifier(en.modeles.length === 4 && en.observations.uid.length === 120 && noms(en).includes('sugar') && noms(en).includes('variety')
        && !noms(en).includes('sucre') && noms(e).includes('sucre'), `exemple en anglais : ${noms(en).join(', ')}`);
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
      // the AIC selection (regression.R: selection_aic), then "Fit this model" (reajuster with groupes_aic)
      const sa = await api('selection_aic', { modele: lmm.id });
      const poids = [].concat(sa.modeles.poids);
      verifier(sa.methode === 'toutes' && [].concat(sa.groupes).length === 2 && poids.length === 4 && Math.abs(poids.reduce((a, b) => a + b, 0) - 1) < 1e-9
        && [].concat(sa.importance.importance).length === 2 && sa.meilleur === 1 && sa.parcimonieux >= 1 && [].concat(sa.contient).length === 2,
      `sélection par AIC : les 4 combinaisons, leurs poids, l’importance des groupes (${JSON.stringify(sa.modeles)})`);
      const meilleur = [].concat(sa.groupes).filter((_, j) => [].concat(sa.contient[j])[0] === 1);
      const choisi = await api('reajuster', { modele: lmm.id, groupes_aic: meilleur });
      verifier(/^R2 \(\d+\)$/.test(choisi.nom) && choisi.type === 'LMM' && choisi.K === meilleur.length && choisi.N === lmm.N
        && Math.abs(choisi.qualite.AICc - [].concat(sa.modeles.AICc)[0]) < 1e-6, `Ajuster ce modèle : ${choisi.nom}, K = ${choisi.K}, AICc ${choisi.qualite.AICc}`);
      const nul = await api('reajuster', { modele: lmm.id, groupes_aic: [] });
      const ligneNul = [].concat(sa.modeles.modele).indexOf('1');
      verifier(nul.K === 0 && Math.abs(nul.qualite.AICc - [].concat(sa.modeles.AICc)[ligneNul]) < 1e-6, 'Ajuster ce modèle : le modèle nul');
      const tsel = await api('methodes', { modele: lmm.id, langue: 'en', version: 'essai' });
      verifier([].concat(tsel.paragraphes).some((x) => /AICc/.test(x) && /combinations/.test(x)) && [].concat(tsel.references).some((x) => /Burnham/.test(x)),
        'texte des méthodes : la sélection par AIC, une fois faite');
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
      // the randomized quantile residuals of counts (moteur/R/regression_diagnostics.R), with their normal quantiles: the same when refitted
      const rq = glm.residus_quantiles;
      const glm2 = await api('reajuster', { modele: glm.id });
      verifier(rq && [].concat(rq.residu).length === glm.N && [].concat(rq.residu).every(Number.isFinite) && [].concat(rq.quantile_normal).length === glm.N
        && JSON.stringify(glm2.residus_quantiles) === JSON.stringify(rq) && lm.residus_quantiles === null && gam.residus_quantiles === null,
      `résidus quantiles d'un GLM de Poisson, les mêmes réajusté ; ni pour une régression linéaire ni pour la loi normale : ${JSON.stringify(rq).slice(0, 200)}`);
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
      // a cross term with a qualitative variable (moteur/R/regression.R): its products by category, one
      // line per category in Effects, a main effect leaving with it in the contributions; without the
      // main effect of the number, one slope per category, and the formula that says so
      const cq = await api('ajuster', { projet: e.id, type: 'LM', x: [v('sucre'), v('variete')], y: [v('note_gout')], echelle: 'none', nom: 'RQ1',
        croises: [[v('sucre'), v('variete')]] });
      const efq = (await api('effets', { modele: cq.id })).effets.find((x) => x.terme === 'sucre');
      const apq = await api('apports', { modele: cq.id });
      verifier([].concat(cq.coefficients_regression.terme).filter((x) => /^sucre\*variete\(/.test(x)).length === 3 && [].concat(cq.termes_retires).length === 2
        && efq.par === 'variete' && [].concat(efq.niveaux).length === 4 && efq.niveaux.every((n) => Number.isFinite(n.pente) && [].concat(n.x).length === 80)
        && [].concat(apq.apports.avec)[0] === 'sucre*variete',
      `terme croisé avec une variable qualitative : coefficients, une droite par catégorie, l’apport d’un effet principal avec son terme croisé (${JSON.stringify(apq.apports).slice(0, 200)})`);
      const luq = await api('lire_formule', { projet: e.id, texte: 'note_gout ~ variete + sucre:variete', type: 'LM' });
      const cq2 = await api('ajuster', { projet: e.id, type: 'LM', x: luq.x, y: luq.y, echelle: 'none', nom: 'RQ2', croises: luq.croises });
      const foq = await api('formule', { projet: e.id, ...cq2.reglages, type: 'LM' });
      verifier([].concat(luq.x).length === 1 && [].concat(cq2.coefficients_regression.terme).filter((x) => /^sucre\*variete\(/.test(x)).length === 4
        && [].concat(cq2.reglages.x).length === 1 && foq.formule === 'note_gout ~ variete + sucre:variete',
      `sans l’effet principal du sucre : une pente par variété, et la formule réécrite (${foq.formule})`);
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
      // before the pairs, Friedman's test on the 8 years where the three sectors have a value (2001
      // and 2006 left out), computed here: the ranks within each year (ties at their mean rank)
      const complets = [];
      for (let an = 2001; an <= 2010; an += 1) if (['S1', 'S2', 'S3'].every((s) => val(s, an) !== null)) complets.push(['S1', 'S2', 'S3'].map((s) => val(s, an)));
      const sommes = [0, 0, 0];
      let liens = 0;
      for (const b of complets) {
        b.forEach((x, j) => {
          const egaux = b.filter((y) => y === x).length;
          sommes[j] += b.filter((y) => y < x).length + (egaux + 1) / 2;
          if (j === b.indexOf(x)) liens += egaux ** 3 - egaux;
        });
      }
      const nb = complets.length;
      const khi2 = (12 * sommes.reduce((s, R) => s + (R - (nb * 4) / 2) ** 2, 0)) / (nb * 3 * 4 - liens / 2);
      // (a table of one test: each column a list of one value)
      const f = Object.fromEntries(Object.entries(r.friedman).map(([cle, x]) => [cle, [].concat(x)[0]]));
      verifier(nb === 8 && f.n === 8 && f.k === 3 && f.laissees === 2 && f.ddl === 2 && Math.abs(f.statistique - khi2) < 1e-9 && Math.abs(f.W - khi2 / (8 * 2)) < 1e-12
        && /^Test de Friedman : χ²\(2\) = .* \(n = 8 valeurs de annee où les 3 niveaux ont tous une valeur ; 2 laissées de côté, un niveau sans valeur\)\.$/.test(f.phrase),
      `Friedman avant les paires : ${JSON.stringify(r.friedman)}`);
      // within each site: only the pairs of sectors of the same site (S1 and S2 in A, S3 alone in B), Holm over those
      const ls = await api('coller', { texte: ['nom\tsecteur\tsite\tannee\tdensite', ...lignes.map((l) => [l[0], l[1], l[1] === 'S3' ? 'B' : 'A', l[2], l[3]].join('\t'))].join('\r\n'),
        nom: 'Secteurs et sites' });
      const vs = Object.fromEntries(ls.variables.map((x) => [x.nom, x.uid]));
      const rs = await api('test_deux_variables', { projet: ls.id, x: vs.densite, y: vs.secteur, appariement: vs.annee, dans: vs.site });
      const ts = rs.toutes;
      verifier([].concat(ts.groupe).join() === 'A' && [].concat(ts.a).join() === 'S1' && [].concat(ts.b).join() === 'S2'
        && [].concat(ts.p_t)[0] === r.tests.p[0] && [].concat(ts.p_t_holm)[0] === [].concat(ts.p_t)[0], `paires dans chaque site : ${JSON.stringify(ts)}`);
      // (one Friedman test per site: two sectors in A, one in B, so neither has one, and why)
      verifier([].concat(rs.friedman.groupe).join() === 'A,B' && [].concat(rs.friedman.phrase).every((x) => x === null)
        && [].concat(rs.friedman.non_teste).join('|') === '2 niveau(x), il en faut 3|1 niveau(x), il en faut 3', `Friedman dans chaque site : ${JSON.stringify(rs.friedman)}`);
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
        && r.sans_temps_valeur === 1 && r.sans_groupe === 0 && r.doubles.n === 0
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
      verifier(/: Le temps \(secteur\) doit être fait de nombres .* : « S3 » n'est ni un nombre ni une date$/.test(refus), `un temps qui n’est pas un nombre, refusé : « ${refus} »`);
    });
    await etape('séries sur des dates, et leurs tendances (Mann–Kendall, pente de Sen)', async () => {
      // two sectors counted on a spring day of 2011 to 2018 (the same day for both, a different
      // one each year), the date written day first: a qualitative variable that can be the time;
      // S2 has 3 years only
      const jours = ['14/05/2011', '02/05/2012', '20/05/2013', '09/05/2014', '27/04/2015', '18/05/2016', '05/05/2017', '22/05/2018'];
      const s1 = [3, 5, 5, 4, 7, 7, 9, 8];
      const lignes = jours.map((j, k) => [`a${k}`, 'S1', j, s1[k]]).concat(jours.slice(0, 3).map((j, k) => [`b${k}`, 'S2', j, 10 - k]));
      const q = await api('coller', { texte: ['nom\tsecteur\tdate\tdensite', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Dates' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x]));
      verifier(v.date.qualitative && v.date.temps === true && v.secteur.temps === false, `une variable de dates, qui peut être le temps : ${JSON.stringify(v.date)}`);
      // the decimal year computed here: the year + (day of the year − 1) / the days in the year
      const decimale = (s) => { const [j, m, a] = s.split('/').map(Number); const d0 = Date.UTC(a, 0, 1); return a + (Date.UTC(a, m - 1, j) - d0) / (Date.UTC(a + 1, 0, 1) - d0); };
      const r = await api('series', { projet: q.id, variable: v.densite.uid, temps: v.date.uid, lignes: v.secteur.uid, tendances: true });
      const col = (x, k) => [].concat(x[k]);
      const attendus = jours.map(decimale).concat(jours.slice(0, 3).map(decimale));
      verifier(r.dates === true && col(r.points, 'temps').every((x, i) => Math.abs(x - attendus[i]) < 1e-9), `les dates en années décimales : ${JSON.stringify(r.points.temps)}`);
      // S1: S and its variance with the ties (two 5s, two 7s), p by the normal approximation with the continuity correction
      let S = 0;
      for (let i = 0; i < 8; i += 1) for (let j = i + 1; j < 8; j += 1) S += Math.sign(s1[j] - s1[i]);
      const varS = (8 * 7 * 21 - 2 * (2 * 1 * 9)) / 18;
      const z = (Math.abs(S) - 1) / Math.sqrt(varS);
      const erfc = (x) => { const t = 1 / (1 + 0.3275911 * x); return t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429)))) * Math.exp(-x * x); };
      const p = erfc(z / Math.SQRT2);  // (two-sided: 2 × (1 − Φ(z)))
      const tr = r.tendances;
      verifier(col(tr, 'ligne').join() === 'S1,S2' && col(tr, 'S')[0] === S && Math.abs(col(tr, 'var_S')[0] - varS) < 1e-9 && Math.abs(col(tr, 'p')[0] - p) < 1e-6
        && col(tr, 'n').join() === '8,3' && col(tr, 'p')[1] === null && /3 temps/.test(col(tr, 'non_teste')[1]) && col(tr, 'p_ajuste')[0] === col(tr, 'p')[0]
        && Math.abs(col(tr, 'debut')[0] - attendus[0]) < 1e-9 && col(tr, 'pente')[0] > 0, `tendances : ${JSON.stringify(tr)}`);
      const h = await api('series', { projet: q.id, variable: v.densite.uid, temps: v.date.uid, lignes: v.secteur.uid, tendances: true, correction: 'holm' });
      const sans = await api('series', { projet: q.id, variable: v.densite.uid, temps: v.date.uid });
      verifier(col(h.tendances, 'p_ajuste')[0] === col(tr, 'p')[0] && sans.tendances === undefined && sans.dates === true, 'Holm sur une seule ligne testée ; sans tendances, pas de tableau');
      verifier(col(tr, 'test').join() === 'mann_kendall,mann_kendall', `le test de chaque ligne : ${JSON.stringify(tr.test)}`);
      // Hamed and Rao's correction: 8 years, no autocorrelation of the ranks beyond ±1.96/√8, so n/n* = 1 and the same p
      const base = { projet: q.id, variable: v.densite.uid, temps: v.date.uid, lignes: v.secteur.uid, tendances: true };
      const hr = await api('series', { ...base, autocorrelation: true });
      verifier(col(hr.tendances, 'test').join() === 'hamed_rao,hamed_rao' && col(hr.tendances, 'n_n_etoile')[0] === 1 && col(hr.tendances, 'p')[0] === col(tr, 'p')[0],
        `Hamed et Rao : ${JSON.stringify(hr.tendances)}`);
      // the seasonal test by the month of the dates: S1 seen in May 7 years (April 2015 once: that season
      // doesn't count), S over those years and its variance with the two 5s; S2, 3 years: no test
      const mai = s1.filter((_, k) => k !== 4);
      let Sm = 0;
      for (let i = 0; i < mai.length; i += 1) for (let j = i + 1; j < mai.length; j += 1) Sm += Math.sign(mai[j] - mai[i]);
      const varSm = (7 * 6 * 19 - 2 * 1 * 9) / 18;
      const sa = await api('series', { ...base, saisons: 'mois' });
      const ts = sa.tendances;
      verifier(col(ts, 'test').join() === 'saisonnier,saisonnier' && col(ts, 'S')[0] === Sm && Math.abs(col(ts, 'var_S')[0] - varSm) < 1e-9
        && Math.abs(col(ts, 'p')[0] - erfc((Math.abs(Sm) - 1) / Math.sqrt(varSm) / Math.SQRT2)) < 1e-6 && col(ts, 'saisons').join() === '1,1'
        && col(ts, 'p')[1] === null && /^trop peu d'années par saison : 3 valeur/.test(col(ts, 'non_teste')[1]) && sa.saisons.uid === 'mois'
        && sa.sans_saison === 0 && col(sa.points, 'saison').join() === '5,5,5,5,4,5,5,5,5,5,5', `Kendall saisonnier par mois : ${JSON.stringify(ts)}`);
      // the R console's log: the trends' call on the series' points, which gives the same S when run
      const appel = (await api('journal', { depuis: 0 })).entrees.filter((e) => e.type === 'appel' && e.texte.startsWith('tendances_series(')).pop();
      const refait = appel ? await api('console', { code: `cat(${appel.texte}$S[1])` }) : { journal: [] };
      verifier(appel && /saisons = "mois"\)\$points, "BH"\)$/.test(appel.texte) && refait.journal.some((e) => e.type === 'sortie' && e.texte.trim() === String(Sm)),
        `journal : ${appel && appel.texte}`);
      // options refused: the plot stays, the trends say why (a quantitative variable for the seasons;
      // the correction with the seasonal test is never sent by the interface, and refused by R)
      const ref = await api('series', { ...base, saisons: v.densite.uid });
      const ref2 = await api('series', { ...base, saisons: 'mois', autocorrelation: true });
      verifier(ref.tendances === undefined && /^Les saisons se font par une variable qualitative/.test(ref.tendances_erreur) && col(ref.points, 'valeur').length === 11
        && /^Le test saisonnier tient les saisons pour indépendantes/.test(ref2.tendances_erreur), `options refusées : ${ref.tendances_erreur} / ${ref2.tendances_erreur}`);
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
          lignes.push([`${s}${an}`, s, an, x.toFixed(4), (2 * alea()).toFixed(4), (1 + 0.5 * x + effet + bruit).toFixed(4), an < 2006 ? 'p1' : 'p2']);
        }
      }
      const q = await api('coller', { texte: ['nom\tsite\tannee\tx\tz\ty\tperiode', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Sites' });
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
      const b3 = await api('autocorrelation_residus', { modele: m2.id, temps: { by: 'variable', variable: v.annee }, groupe: [{ by: 'effets' }, { by: 'variable', variable: v.periode }] });
      verifier(b3.groupe === 'site / periode' && b3.groupes === 12 && b3.n === 60,
        `les groupes des effets aléatoires, puis un niveau plus fin : ${JSON.stringify(b3).slice(0, 300)}`);
      let refusT = '';
      try { await api('autocorrelation_residus', { modele: m2.id, temps: { by: 'variable', variable: v.annee }, groupe: null }); } catch (err) { refusT = err.message; }
      verifier(/même temps/.test(refusT), `les années de tous les sites en une série, refusé : « ${refusT} »`);
      const r = await api('rapports_vraisemblance', { modeles: [m2.id, m1.id, c1.id, q.id] });
      const khi2 = 2 * (m2.qualite.logLik - m1.qualite.logLik);
      verifier(r.total === 1 && [].concat(r.paires.grand)[0] === m2.id && [].concat(r.paires.petit)[0] === m1.id && [].concat(r.paires.ddl)[0] === 1
        && Math.abs([].concat(r.paires.khi2)[0] - Math.max(0, khi2)) < 1e-8 && [].concat(r.paires.approche)[0] === false,
      `x + z contre x (pas le modèle avec CAR1) : ${JSON.stringify(r)}`);
      // Fit with CAR1 (reajuster with car1): M2 again with CAR1 along the year in each site, which is C1
      const annee = { by: 'variable', variable: v.annee };
      const car = await api('reajuster', { modele: m2.id, car1: { temps: annee, groupe: [{ by: 'effets' }] } });
      verifier(/^M2 \(\d+\)$/.test(car.nom) && car.type === 'LMM' && car.app === true && car.temps === 'annee' && !car.dans && car.signature === m2.signature
        && Math.abs(car.phi - c1.phi) < 1e-8 && Math.abs(car.qualite.AIC - c1.qualite.AIC) < 1e-8,
      `Ajuster avec CAR1 : ${car.nom}, φ ${car.phi} (C1 : ${c1.phi}), AIC ${car.qualite.AIC} (C1 : ${c1.qualite.AIC})`);
      const fin = await api('reajuster', { modele: m2.id, car1: { temps: annee, groupe: [{ by: 'effets' }, { by: 'variable', variable: v.periode }] } });
      verifier(fin.dans === 'periode' && fin.phi !== null && fin.signature === m2.signature, `Ajuster avec CAR1 dans chaque période de chaque site : ${fin.dans}`);
      let refusC = '';
      try { await api('reajuster', { modele: m2.id, car1: { temps: annee, groupe: [{ by: 'variable', variable: v.site }] } }); } catch (err) { refusC = err.message; }
      verifier(/choisissez ses effets aléatoires/.test(refusC), `d'autres groupes que ceux des effets aléatoires, refusé : « ${refusC} »`);
    });
    await etape('pentes aléatoires : (1 + annee | site), sa formule, ses refus, CAR1 et la sélection par AIC', async () => {
      // 8 sites followed 11 years, each with its own trend around the mean one (moteur/R/regression.R: pentes)
      let g = 3;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647 - 0.5; };
      const lignes = [];
      for (const s of ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']) {
        const effet = 2 * alea();
        const pente = 0.4 * alea();
        for (let an = 2001; an <= 2011; an += 1) {
          const x = 2 * alea();
          lignes.push([`${s}${an}`, s, an, x.toFixed(4), (1 + 0.5 * x + 0.1 * (an - 2006) + effet + pente * (an - 2006) + alea()).toFixed(4)]);
        }
      }
      const q = await api('coller', { texte: ['nom\tsite\tannee\tx\ty', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Pentes' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const aleatoires = [{ by: 'variable', variable: v.site, pentes: [v.annee] }];
      const reglages = { projet: q.id, type: 'LMM', x: [v.x, v.annee], y: [v.y], echelle: 'none', echelles: { [v.annee]: 'Ctr' }, aleatoires };
      const p1 = await api('ajuster', { ...reglages, nom: 'P1' });
      const vp = p1.variances;
      const parts = [].concat(p1.parts_variance.part);
      verifier([].concat(vp.terme).join('|') === '(Intercept)|annee|' && [].concat(vp.composante).join('|') === 'site|site|résiduelle'
        && Math.abs([].concat(vp.correlation)[1]) < 1 && [].concat(p1.pentes[0]).join() === 'annee' && parts.length === 2
        && Math.abs(parts[0] + parts[1] - 1) < 1e-9 && [].concat(p1.reglages.aleatoires[0].pentes).map(Number).join() === String(v.annee)
        && p1.qualite.conditionnel > p1.qualite.marginal, `pente aléatoire de l'année par site : ${JSON.stringify({ vp, parts: p1.parts_variance, pentes: p1.pentes })}`);
      const fo = await api('formule', { ...reglages, croises: [], transformations: {}, decalage: null });
      const lu = await api('lire_formule', { projet: q.id, texte: 'y ~ x + annee + (annee | site)', type: 'LM' });
      verifier(fo.formule === 'y ~ x + annee + (1 + annee | site)' && lu.type === 'LMM' && [].concat(lu.aleatoires[0].pentes).map(Number).join() === String(v.annee),
        `formule de la pente : ${fo.formule} ; relue : ${JSON.stringify(lu.aleatoires)}`);
      let refusF = '';
      try { await api('lire_formule', { projet: q.id, texte: 'y ~ x + annee + (1 | site) + (0 + annee | site)', type: 'LM' }); } catch (err) { refusF = err.message; }
      let refusX = '';
      try { await api('ajuster', { ...reglages, x: [v.x], nom: 'P2' }); } catch (err) { refusX = err.message; }
      verifier(/indépendante de l'ordonnée à l'origine/.test(refusF) && /« annee » a une pente aléatoire/.test(refusX),
        `une pente non corrélée, une pente hors des X : refusées (« ${refusF} » ; « ${refusX} »)`);
      // the year left in its units (≈ 2006): the fit is signalled as singular, with the advice to centre it
      const nc = await api('ajuster', { ...reglages, echelles: {}, nom: 'P3' });
      verifier(nc.singularites && [].concat(nc.singularites.terme)[0] === 'annee' && [].concat(nc.singularites.loin)[0] === true,
        `l'année non centrée : ajustement singulier signalé (${JSON.stringify(nc.singularites)})`);
      const car = await api('reajuster', { modele: p1.id, car1: { temps: { by: 'variable', variable: v.annee }, groupe: [{ by: 'effets' }] } });
      const sel = await api('selection_aic', { modele: p1.id });
      verifier(car.phi !== null && [].concat(car.pentes[0]).join() === 'annee' && [].concat(car.variances.terme).includes('annee')
        && sel.K === 1 && [].concat(sel.gardes).join() === 'annee' && [].concat(sel.groupes).join() === 'x',
        `Ajuster avec CAR1 garde la pente (φ ${car.phi}) ; la sélection garde l'année : ${JSON.stringify({ gardes: sel.gardes, groupes: sel.groupes })}`);
    });
    await etape('catégories que le modèle n’a pas vues ; une courbe par catégorie, s(annee, by = secteur)', async () => {
      // 4 sites followed 12 years, 3 sectors each, each sector its own shape; secteur_b: site A's S3 renamed
      // S4 (a category of one site); secteur_site: sectors proper to each site (moteur/R/regression.R:
      // niveaux_inconnus, par_courbes)
      let g = 11;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647 - 0.5; };
      const forme = { S1: (a) => Math.sin((a - 2000) / 2.5), S2: (a) => -0.15 * (a - 2006), S3: (a) => 0.02 * (a - 2006) ** 2 - 0.5 };
      const lignes = [];
      for (const s of ['A', 'B', 'C', 'D']) for (const sec of ['S1', 'S2', 'S3']) for (let an = 2001; an <= 2012; an += 1) {
        lignes.push([`${s}${sec}${an}`, s, sec, s === 'A' && sec === 'S3' ? 'S4' : sec, `${s}${sec}`, an, (3 + forme[sec](an) + 0.6 * alea()).toFixed(4)]);
      }
      const q = await api('coller', { texte: ['nom\tsite\tsecteur\tsecteur_b\tsecteur_site\tannee\ty', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Courbes' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const parSite = { by: 'variable', variable: v.site };
      const siteA = q.observations.uid.filter((_, i) => q.observations.nom[i].startsWith('A'));
      // cross-validation by site: S4 (site A only) left without a prediction, out of Q2; sectors proper to each site: none predicted
      const c1 = await api('ajuster', { projet: q.id, type: 'LM', x: [v.annee, v.secteur_b], y: [v.y], echelle: 'none', nom: 'K1', vc: parSite });
      const c2 = await api('ajuster', { projet: q.id, type: 'LM', x: [v.annee, v.secteur_site], y: [v.y], echelle: 'none', nom: 'K2', vc: parSite });
      verifier(c1.qualite.sans_categorie_vc === 12 && [].concat(c1.qualite.facteurs_vc).join() === 'secteur_b' && Number.isFinite(c1.qualite.Q2)
        && c2.qualite.sans_categorie_vc === 144 && c2.qualite.Q2 === null && [].concat(c2.qualite.facteurs_vc).join() === 'secteur_site',
      `validation croisée par site : ${c1.qualite.sans_categorie_vc} lignes de S4 sans prédiction ; des secteurs propres à chaque site, Q2 ${c2.qualite.Q2}`);
      // the test set (site A): its S4 rows, a category the model hasn't seen, without a prediction, counted; Predict marks them
      const c3 = await api('ajuster', { projet: q.id, type: 'LM', x: [v.annee, v.secteur_b], y: [v.y], echelle: 'none', nom: 'K3', test: siteA });
      const pr3 = await api('predire', { modele: c3.id });
      const marquees = pr3.observations.uid.filter((_, i) => pr3.observations.categorie_inconnue[i]);
      verifier(c3.test.n === 36 && c3.test.inconnues === 12 && [].concat(c3.test.facteurs_inconnus).join() === 'secteur_b' && [].concat(c3.test.statistiques.n)[0] === 24
        && marquees.length === 12 && marquees.every((u) => siteA.includes(u)) && pr3.observations['y prédit'].filter((x) => x === null).length === 12,
      `jeu de test : ${c3.test.inconnues} sans prédiction sur ${c3.test.n} ; prédire : ${marquees.length} marquées`);
      // a curve per sector: its rows in the curves table, one curve per sector in Effects, the thresholds of the sector chosen
      const reglages = { projet: q.id, type: 'GAM', x: [v.annee, v.secteur], y: [v.y], echelle: 'none', famille: 'gaussienne', lisses: [v.annee],
        par_courbes: { [v.annee]: v.secteur } };
      const gc = await api('ajuster', { ...reglages, nom: 'K4' });
      const efc = (await api('effets', { modele: gc.id })).effets.find((x) => x.terme === 'annee');
      const se2 = await api('seuils', { modele: gc.id, terme: 'annee', categorie: 'S2' });
      const se0 = await api('seuils', { modele: gc.id, terme: 'annee' });
      verifier([].concat(gc.courbes.niveau).join() === 'S1,S2,S3' && [].concat(gc.courbes.par).every((x) => x === 'secteur') && [].concat(gc.courbes.edf).every(Number.isFinite)
        && Number(gc.reglages.par_courbes[v.annee]) === v.secteur && efc.par_categorie === true && efc.courbe === true && [].concat(efc.niveaux).length === 3
        && se2.categorie === 'S2' && se2.par === 'secteur' && [].concat(se2.x).length === 200 && se0.categorie === 'S1',
      `une courbe par secteur : ${JSON.stringify(gc.courbes).slice(0, 300)}`);
      const fo = await api('formule', { ...gc.reglages, projet: q.id, type: 'GAM' });
      const lu = await api('lire_formule', { projet: q.id, texte: 'y ~ secteur + s(annee, by = secteur, k = 5)', type: 'LM' });
      let refusX = '';
      try { await api('ajuster', { ...reglages, x: [v.annee], nom: 'K5' }); } catch (err) { refusX = err.message; }
      verifier(fo.formule === 'y ~ s(annee, by = secteur) + secteur' && lu.type === 'GAM' && Number(lu.par_courbes[v.annee]) === v.secteur && Number(lu.k_courbes[v.annee]) === 5
        && /doit être aussi une variable X/.test(refusX), `formule ${fo.formule} ; relue ${JSON.stringify(lu.par_courbes)} ; sans le secteur en X : « ${refusX} »`);
      // fitted without S2: its rows predicted by nothing, marked; the export to R redoes the model
      const sansS2 = q.observations.uid.filter((_, i) => q.observations.nom[i].includes('S2'));
      const gc6 = await api('ajuster', { ...reglages, nom: 'K6', exclues: sansS2 });
      const pr6 = await api('predire', { modele: gc6.id });
      const sc = await api('script_r', { modele: gc.id, version: 'essai', langue: 'fr', chemin: path.join(dossier, 'K4.R') });
      verifier(pr6.observations.uid.filter((_, i) => pr6.observations.categorie_inconnue[i]).length === 48 && !sc.ecart && fs.readFileSync(sc.script, 'utf8').includes('by = '),
        `sans S2 : ses lignes marquées ; le script R sans écart (${sc.ecart || ''})`);
    });
    await etape('coefficients côte à côte ; puissance pour détecter une tendance', async () => {
      // 6 sites of 2 sectors counted from 2001 to 2010, a decline and an autocorrelated noise in each sector
      let g = 17;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647 - 0.5; };
      const lignes = [];
      for (const s of ['A', 'B', 'C', 'D', 'E', 'F']) {
        const effet = alea();
        for (const sec of ['a', 'b']) {
          let bruit = 0;
          for (let an = 2001; an <= 2010; an += 1) {
            bruit = 0.4 * bruit + 0.3 * alea();
            lignes.push([`${s}${sec}${an}`, s, sec, an, (2 * alea()).toFixed(3), (3 - 0.03 * (an - 2000) + effet + bruit).toFixed(4)]);
          }
        }
      }
      const q = await api('coller', { texte: ['nom\tsite\tsecteur\tannee\tz\ty', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Suivi' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const lm = await api('ajuster', { projet: q.id, type: 'LM', x: [v.annee, v.z], y: [v.y], echelle: 'none', nom: 'P1' });
      const lmm = await api('ajuster', { projet: q.id, type: 'LMM', x: [v.annee], y: [v.y], echelle: 'none', nom: 'P2',
        aleatoires: [{ by: 'variable', variable: v.site }], correlation: { type: 'CAR1', temps: { by: 'variable', variable: v.annee }, dans: { by: 'variable', variable: v.secteur } } });
      // (moteur/R/coefficients_modeles.R) the rows of both tables, a cell per model, empty where it lacks the term
      const c = await api('coefficients_modeles', { modeles: [lm.id, lmm.id], erreur: 'ic' });
      const ligne = (x) => [].concat(c.termes).indexOf(x);
      verifier([].concat(c.modeles).join() === 'P1,P2' && [].concat(c.termes).join() === '(Intercept),annee,z' && c.cellules.length === 3 && c.cellules[0].length === 2
        && c.cellules[ligne('z')][1] === '' && c.p[ligne('z')][1] === null && Math.abs(c.p[ligne('annee')][0] - lm.coefficients_regression.p[1]) < 1e-12
        && [].concat(c.statistiques).includes('φ (CAR1)') && c.copie.lignes.length === 3 + [].concat(c.statistiques).length && c.copie.entetes.length === 3
        && /^\* p ≤ 0,05/.test([].concat(c.copie.notes)[0]), `coefficients côte à côte : ${JSON.stringify(c).slice(0, 400)}`);
      const un = await api('coefficients_modeles', { modeles: [lmm.id], erreur: 'et' }, 'en');
      verifier(un.cellules.length === 2 && un.cellules[1].length === 1 && /^-?\d+\.\d+ \(\d+\.\d+\)$/.test(un.cellules[1][0]) && un.entete === 'Estimate (SE)',
        `un seul modèle, en anglais, avec l’erreur-type : ${JSON.stringify(un.cellules)}`);
      const ex = await api('exporter_coefficients', { modeles: [lm.id, lmm.id], erreur: 'ic', chemin: path.join(dossier, 'cote.csv') });
      verifier(ex.chemins.length === 2 && ex.chemins.every((f) => fs.existsSync(f)) && fs.readFileSync(ex.chemins[0], 'utf8').includes('P2'), 'exporter les coefficients côte à côte');
      // (moteur/R/puissance.R) the defaults: the time named like one, a slope (Y untransformed), the data's 6 sites among the designs
      const pu = await api('puissance_tendance', { modele: lmm.id });
      const tab = pu.tableau;
      verifier(pu.reglages.terme === 'annee' && pu.reglages.unite === 'pente' && [].concat(pu.reglages.unites).includes(6) && pu.plan.unites === 6 && pu.plan.annees === 10
        && pu.plan.dans === 2 && tab.annees.length === tab.puissance.length && tab.puissance.every((x) => x >= 0 && x <= 1) && /^Avec 6 unités de « site »/.test(pu.phrase)
        && pu.estimation.par_ajustement > 0, `puissance par défaut : ${JSON.stringify(pu).slice(0, 400)}`);
      const pr = await api('puissance_tendance', { modele: lmm.id, terme: 'annee', unite: 'pente', tendance: -0.03, alpha: 0.05, annees: [5, 12], unites: [4, 10],
        simulation: { annees: 8, unites: 6, n: 10 }, simuler: true });
      const s = pr.simulation;
      verifier(pr.tableau.annees.length === 8 * 3 && [].concat(pr.reglages.unites).join() === '4,6,10' && s.n === 10 && [].concat(s.p).length === 10
        && s.puissance >= 0 && s.puissance <= 1 && s.analytique > 0, `réglages et simulation : ${JSON.stringify(s).slice(0, 300)}`);
      const garde = await api('puissance_tendance', { modele: lmm.id });
      const tm = await api('methodes', { modele: lmm.id, langue: 'en', version: 'essai' });
      verifier(garde.simulation && JSON.stringify(garde.simulation.p) === JSON.stringify(s.p) && garde.reglages.tendance === -0.03
        && [].concat(tm.paragraphes).some((x) => /The power to detect a decline of 0.03 per year/.test(x)) && [].concat(tm.references).some((x) => /^Diggle/.test(x))
        && [].concat(tm.references).some((x) => /^Johnson/.test(x)), 'gardée avec le modèle ; dans le texte des méthodes');
      const sc = await api('script_r', { modele: lmm.id, version: 'essai', langue: 'fr', chemin: path.join(dossier, 'P2.R') });
      verifier(!sc.ecart && fs.readFileSync(sc.script, 'utf8').includes('simuler_puissance('), 'exporter vers R : le script refait la puissance et sa simulation');
      // kept in the project file (serveur.R: garder_avec_definition): saved, opened again and refitted,
      // the settings and the simulation come back, in the Methods text before the view asks for them,
      // and asking again with the same settings leaves the project unchanged
      const fp = path.join(dossier, 'puissance.nuee');
      await api('enregistrer', { projet: q.id, chemin: fp });
      const q2 = await api('ouvrir', { chemin: fp });
      const lmm2 = await api('ajuster', { projet: q2.id, numero: q2.modeles.find((x) => x.nom === 'P2').numero });
      const tm2 = await api('methodes', { modele: lmm2.id, langue: 'en', version: 'essai' });
      const garde2 = await api('puissance_tendance', { modele: lmm2.id, ...pr.reglages });
      const proche = (a, b) => [].concat(a).length === [].concat(b).length && [].concat(a).every((x, i) => Math.abs(x - [].concat(b)[i]) < 1e-12);
      const modifie2 = (await api('projets_modifies', {})).noms.includes(q2.nom);
      verifier(JSON.stringify(tm2.paragraphes) === JSON.stringify(tm.paragraphes) && garde2.reglages.tendance === -0.03 && [].concat(garde2.reglages.unites).join() === '4,6,10'
        && garde2.simulation && proche(garde2.simulation.p, s.p) && !modifie2,
      `enregistrée avec le projet, rouverte : ${JSON.stringify({ reglages: garde2.reglages, simulation: !!garde2.simulation, modifie: modifie2 })}`);
      const glm = await api('ajuster', { projet: q.id, type: 'GLM', x: [v.annee], y: [v.y], echelle: 'none', nom: 'P3', famille: 'gamma' });
      let refus = '';
      try { await api('puissance_tendance', { modele: glm.id }); } catch (err) { refus = err.message; }
      verifier(/GLM, GAM/.test(refus), `un GLM, refusé : « ${refus} »`);
    });
    await etape('moyennes ajustées (comme emmeans) : les moyennes, les paires, les pentes par catégorie, gardées avec le modèle', async () => {
      // 4 sites of 3 sectors counted from 2001 to 2008, a slope per sector, a habitat per site
      let g = 23;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647 - 0.5; };
      const pentes = { S1: 0.1, S2: 0.3, S3: -0.1 };
      const lignes = [];
      for (const s of ['A', 'B', 'C', 'D']) {
        for (const sec of ['S1', 'S2', 'S3']) {
          for (let an = 2001; an <= 2008; an += 1) lignes.push([`${s}${sec}${an}`, s, sec, s < 'C' ? 'H1' : 'H2', an, (3 + pentes[sec] * (an - 2000) + 0.3 * alea()).toFixed(4)]);
        }
      }
      const q = await api('coller', { texte: ['nom\tsite\tsecteur\thabitat\tannee\ty', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Moyennes' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const lmm = await api('ajuster', { projet: q.id, type: 'LMM', x: [v.annee, v.secteur, v.habitat], y: [v.y], echelle: 'none', nom: 'MA1',
        croises: [[v.annee, v.secteur]], aleatoires: [{ by: 'variable', variable: v.site }] });
      const lm0 = await api('ajuster', { projet: q.id, type: 'LM', x: [v.annee], y: [v.y], echelle: 'none', nom: 'MA0' });
      // (moteur/R/moyennes.R) by default: the first qualitative variable, its 3 means, their 3 pairs (Tukey), a sentence each
      const r = await api('moyennes', { modele: lmm.id });
      const col = (t, k) => [].concat(t[k]);
      const ddlMoy = col(r.moyennes, 'ddl');
      const ddlPaires = col(r.paires, 'ddl');
      verifier([].concat(lmm.qualitatives).join() === 'secteur,habitat' && [].concat(lm0.qualitatives).length === 0 && String(r.reglages.facteur) === String(v.secteur)
        && col(r.moyennes, 'niveau').join() === 'S1,S2,S3' && col(r.paires, 'niveau_1').join() === 'S1,S1,S2' && col(r.paires, 'p').every((p) => p >= 0 && p <= 1)
        && ddlMoy.every((d) => d === ddlMoy[0]) && ddlPaires.every((d) => d > ddlMoy[0]) && r.ddl.methode === 'confinement' && [].concat(r.phrases).length === 3
        && /^Moyennes ajustées de « y » par « secteur » \(modèle linéaire mixte\) : S1 − S2 = /.test(r.phrases[0]) && [].concat(r.grille.moyennees).join() === 'habitat'
        && r.options.length === 2 && [].concat(r.options[0].pentes).map((o) => o.nom).join() === 'annee' && [].concat(r.options[1].dans).length === 0,
      `moyennes par défaut : ${JSON.stringify({ reglages: r.reglages, moyennes: r.moyennes, ddl: r.ddl }).slice(0, 500)}`);
      // the slope of the year in each sector, Holm, in English; then kept with the model, in its Methods text
      const p = await api('moyennes', { modele: lmm.id, facteur: v.secteur, dans: null, pente: v.annee, ajustement: 'holm', echelle: 'reponse' }, 'en');
      const est = col(p.moyennes, 'estimation');
      const garde = await api('moyennes', { modele: lmm.id });
      const tm = await api('methodes', { modele: lmm.id, langue: 'en', version: 'essai' });
      verifier(p.mode === 'pentes' && est.every((x, i) => Math.abs(x - [0.1, 0.3, -0.1][i]) < 0.05) && col(p.paires, 'famille').every((k) => k === 3)
        && /^Slopes of “annee” by “secteur” \(linear mixed model\): S1 − S2 = .*\(Holm, 3 pairs\)/.test(p.phrases[0])
        && garde.mode === 'pentes' && garde.reglages.ajustement === 'holm'
        && [].concat(tm.paragraphes).some((x) => /The slope of “annee” in each category of “secteur”/.test(x)) && [].concat(tm.references).some((x) => /^Searle/.test(x)),
      `pentes par secteur, gardées, dans le texte des méthodes : ${JSON.stringify(est)} ${p.phrases[0]}`);
      let refusDans = '';
      try { await api('moyennes', { modele: lmm.id, facteur: v.secteur, dans: v.habitat }); } catch (err) { refusDans = err.message; }
      let refusSans = '';
      try { await api('moyennes', { modele: lm0.id }); } catch (err) { refusSans = err.message; }
      verifier(/n'est croisée avec « secteur » dans aucun terme/.test(refusDans) && /pas de variable qualitative/.test(refusSans),
        `refus : « ${refusDans} » ; « ${refusSans} »`);
      // kept in the project file: the slopes by sector come back once it is opened again and the model refitted
      const fm = path.join(dossier, 'moyennes.nuee');
      await api('enregistrer', { projet: q.id, chemin: fm });
      const q2 = await api('ouvrir', { chemin: fm });
      const ma = await api('ajuster', { projet: q2.id, numero: q2.modeles.find((x) => x.nom === 'MA1').numero });
      const tm2 = await api('methodes', { modele: ma.id, langue: 'en', version: 'essai' });
      const garde2 = await api('moyennes', { modele: ma.id }, 'en');
      verifier(JSON.stringify(tm2.paragraphes) === JSON.stringify(tm.paragraphes) && garde2.mode === 'pentes' && garde2.reglages.ajustement === 'holm'
        && col(garde2.moyennes, 'estimation').every((x, i) => Math.abs(x - est[i]) < 1e-12) && !(await api('projets_modifies', {})).noms.includes(q2.nom),
      `enregistrées avec le projet, rouvertes : ${JSON.stringify(garde2.reglages)}`);
    });
    await etape('Poisson à excès de zéros (ziP) et loi bêta (betar) : loi, lien, paramètres, résidus quantiles, moyennes, côte à côte, méthodes, refus', async () => {
      // 90 surveys of three habitats with an effort: counts with extra zeros, proportions between 0 and 1
      let g = 41;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = [];
      for (let i = 1; i <= 90; i += 1) {
        const habitat = ['bois', 'haie', 'prairie'][i % 3];
        const x = alea();
        const effort = 1 + 2 * alea();
        const mu = effort * Math.exp(0.3 + 0.8 * x + (habitat === 'bois' ? 0.5 : 0));
        const compte = alea() < 0.35 ? 0 : Math.max(1, Math.round(mu + (alea() - 0.5) * 2 * Math.sqrt(mu)));
        const m = 1 / (1 + Math.exp(-(-0.3 + 1.2 * x + (habitat === 'haie' ? -0.4 : 0))));
        const prop = Math.min(0.97, Math.max(0.03, m + (alea() - 0.5) * 0.3));
        lignes.push([`r${i}`, habitat, x.toFixed(3), effort.toFixed(2), compte, prop.toFixed(4)].join('\t'));
      }
      const q = await api('coller', { texte: ['nom\thabitat\tx\teffort\tcompte\tprop', ...lignes].join('\r\n'), nom: 'Familles' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const zip = await api('ajuster', { projet: q.id, type: 'GLM', x: [v.x, v.habitat], y: [v.compte], echelle: 'none', nom: 'ZIP', famille: 'poisson_zi', decalage: v.effort });
      const beta = await api('ajuster', { projet: q.id, type: 'GAM', x: [v.x, v.habitat], y: [v.prop], echelle: 'none', nom: 'BETA', famille: 'beta', lisses: [v.x] });
      const pl = zip.parametre_loi;
      verifier(zip.famille === 'poisson_zi' && zip.lien === 'log' && [].concat(pl.nom).join() === 'theta1,theta2' && [].concat(pl.valeur).length === 2
        && zip.qualite.r2_ajuste === undefined && zip.qualite.deviance_expliquee > 0 && [].concat(zip.residus_quantiles.residu).length === zip.N
        && beta.famille === 'beta' && beta.lien === 'logit' && beta.parametre_loi.nom === 'phi' && beta.parametre_loi.valeur > 0
        && [].concat(beta.residus_quantiles.residu).length === beta.N && [].concat(beta.courbes.terme).join() === 'x',
      `ziP et bêta : ${JSON.stringify({ zip: [zip.famille, zip.lien, pl], beta: [beta.famille, beta.lien, beta.parametre_loi] })}`);
      // (moteur/R/moyennes.R) expected counts and their ratios; mean proportions and odds ratios
      const mz = await api('moyennes', { modele: zip.id });
      const mb = await api('moyennes', { modele: beta.id });
      verifier(mz.echelle === 'reponse' && mz.famille === 'poisson_zi' && [].concat(mz.moyennes.estimation).every((x) => x > 0) && mz.ddl.methode === 'asymptotique'
        && /loi de Poisson à excès de zéros ; échelle de la réponse\) : bois \/ haie = /.test(mz.phrases[0])
        && mb.echelle === 'reponse' && [].concat(mb.moyennes.estimation).every((x) => x > 0 && x < 1) && /rapport des cotes bois \/ haie/.test(mb.phrases[0]),
      `moyennes ajustées : ${mz.phrases[0]} / ${mb.phrases[0]}`);
      const c = await api('coefficients_modeles', { modeles: [zip.id, beta.id], erreur: 'ic' });
      const st = [].concat(c.statistiques);
      verifier(st.includes('θ₁ (Poisson à excès de zéros)') && st.includes('θ₂ (Poisson à excès de zéros)') && st.includes('φ (bêta)'), `côte à côte : ${st.join(' | ')}`);
      const tm = await api('methodes', { modele: zip.id, langue: 'en', version: 'essai' });
      verifier([].concat(tm.paragraphes).some((x) => /zero-inflated Poisson distribution, log link/.test(x)) && [].concat(tm.references).some((x) => /^Wood, S\. N\., Pya, N\./.test(x)),
        `texte des méthodes du ziP, en anglais : ${[].concat(tm.paragraphes)[1]}`);
      let refus = '';
      try { await api('ajuster', { projet: q.id, type: 'GLM', x: [v.x], y: [v.compte], echelle: 'none', famille: 'beta' }); } catch (err) { refus = err.message; }
      verifier(/strictement comprises entre 0 et 1/.test(refus), `bêta sur des comptages : ${refus}`);
    });
    await etape('BACI : l’écart impact − témoins après contre avant ; deux classes appariées, variable par variable', async () => {
      // 4 sectors followed from 2001 to 2010, one row per sector and year, in no order; S1 (the
      // impact) rises from 2006; S3 has no row in 2002; S4 is neither impact nor control
      let g = 5;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = [];
      for (const s of ['S1', 'S2', 'S3', 'S4']) {
        for (let an = 2001; an <= 2010; an += 1) {
          if (s !== 'S3' || an !== 2002) lignes.push([`${s}_${an}`, s, an, (3 + (s === 'S1' && an >= 2006 ? 1 : 0) + alea()).toFixed(3), (2 + alea()).toFixed(3)]);
        }
      }
      lignes.sort((a, b) => (a[2] % 4) - (b[2] % 4) || a[0].localeCompare(b[0]));
      const q = await api('coller', { texte: ['nom\tsecteur\tannee\tdensite\tnnicad', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'BACI' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const base = { projet: q.id, y: v.densite, temps: v.annee, unite: v.secteur, impact: ['S1'], temoins: ['S2', 'S3'] };
      const r = await api('baci', { ...base, evenement: 2006 });
      // d_t computed here: S1 − the mean of S2 and S3 (S2 alone in 2002), and Welch's t after against before
      const val = (s, an) => { const l = lignes.find((x) => x[1] === s && x[2] === an); return l ? Number(l[3]) : null; };
      const moy = (x) => x.reduce((a, b) => a + b, 0) / x.length;
      const vari = (x) => { const m = moy(x); return x.reduce((a, b) => a + (b - m) ** 2, 0) / (x.length - 1); };
      const d = [];
      for (let an = 2001; an <= 2010; an += 1) d.push([an, val('S1', an) - moy([val('S2', an), val('S3', an)].filter((x) => x !== null))]);
      const av = d.filter(([an]) => an < 2006).map(([, x]) => x);
      const ap = d.filter(([an]) => an >= 2006).map(([, x]) => x);
      const tw = (moy(ap) - moy(av)) / Math.sqrt(vari(ap) / ap.length + vari(av) / av.length);
      const ecarts = [].concat(r.serie.ecart);
      verifier(r.type === 'baci' && ecarts.length === 10 && ecarts.every((x, i) => Math.abs(x - d[i][1]) < 1e-9) && [].concat(r.serie.n_temoins)[1] === 1
        && Math.abs(r.tests.statistique[0] - tw) < 1e-8 && [].concat(r.tests.test).join() === 'welch,student,mann_whitney' && r.test === 'welch'
        && Math.abs(r.difference[0] - (moy(ap) - moy(av))) < 1e-9 && /\(n = 5 avant et 5 après ; après : annee ≥ 2006\)\.$/.test(r.phrase), `BACI : ${r.phrase}`);
      const seule = await api('baci', base);
      verifier(!seule.tests && !seule.phrase && [].concat(seule.serie.ecart).length === 10, 'BACI sans événement : les écarts seuls');
      const en = await api('baci', { ...base, evenement: 2004, fin: 2008, test: 'mann_whitney' }, 'en');
      verifier(en.hors === 2 && /^Mann–Whitney test \(BACI\), densite gap impact \(S1\) − control \(S2, S3\)/.test(en.phrase)
        && /\(n = 3 before and 5 after; after: 2004 ≤ annee ≤ 2008\)\.$/.test(en.phrase), `BACI en anglais, avec une fin : ${en.phrase}`);
      let refus = '';
      try { await api('baci', { ...base, evenement: 2002 }); } catch (err) { refus = err.message; }
      verifier(/au moins deux valeurs de annee dans chaque période/.test(refus), `une seule année avant, refusé : « ${refus} »`);
      // between classes: S1 and S2 paired by the year, each variable on its shared years (the paired t by default)
      const tv = await api('tests_variables', { projet: q.id, classe: v.secteur, groupes: ['S1', 'S2'], appariement: v.annee, correction: 'BH' });
      const dp = [];
      for (let an = 2001; an <= 2010; an += 1) dp.push(val('S2', an) - val('S1', an));
      const k = [].concat(tv.tableau.nom).indexOf('densite');
      verifier(tv.test === 't_apparie' && tv.cle.nom === 'annee' && [].concat(tv.tableau.nom).sort().join() === 'densite,nnicad' && [].concat(tv.tableau.n_paires)[k] === 10
        && Math.abs([].concat(tv.tableau.statistique)[k] - moy(dp) / Math.sqrt(vari(dp) / 10)) < 1e-8 && Math.abs([].concat(tv.tableau.d)[k] - moy(dp) / Math.sqrt(vari(dp))) < 1e-9,
        `deux classes appariées : ${JSON.stringify(tv.tableau)}`);
      // Fit the mixed model (comparaisons.R: modele_baci): its two 0/1 variables, added to the
      // project (Ctrl+Z removes them), and the model y ~ period × impact + (1 | sector) in it
      const nVariables = q.variables.length;
      const bm = await api('baci_modele', { ...base, evenement: 2006, car1: false });
      const dn = await api('donnees', { projet: q.id });
      const colonne = (nom) => dn.valeurs[dn.variables.indexOf(nom)];
      const periode = colonne('période BACI (2006)');
      const impact = colonne('impact BACI (S1 contre S2, S3)');
      const pb = await api('projet', { projet: q.id });
      verifier(bm.modele.type === 'LMM' && bm.modele.nom === 'BACI densite' && [].concat(bm.nouvelles).join('|') === 'période BACI (2006)|impact BACI (S1 contre S2, S3)'
        && pb.variables.length === nVariables + 2 && /^variables période BACI/.test(pb.annulable) && pb.modeles.some((m) => m.nom === 'BACI densite' && m.type === 'LMM')
        && lignes.every((l, i) => periode[i] === (l[2] >= 2006 ? 1 : 0) && impact[i] === ({ S1: 1, S2: 0, S3: 0 }[l[1]] ?? null))
        && bm.modele.N === lignes.filter((l) => l[1] !== 'S4').length && bm.modele.vc_par === 'secteur',
      `modèle mixte BACI : ${bm.modele.nom}, ${JSON.stringify(bm.nouvelles)}, N = ${bm.modele.N}`);
      // the BACI view then gives its effect, the period × impact coefficient, as a sentence
      const avec = await api('baci', { ...base, evenement: 2006 });
      const ef = [].concat(avec.modeles);
      const co = bm.modele.coefficients_regression;
      const kc = [].concat(co.terme).indexOf('période BACI (2006)*impact BACI (S1 contre S2, S3)');
      verifier(ef.length === 1 && ef[0].id === bm.modele.id && ef[0].car1 === false && kc === 3 && ef[0].estimation === co.estimation[kc] && ef[0].p === co.p[kc]
        && ef[0].bas === co.bas[kc] && /^Modèle linéaire mixte \(BACI\), avec une ordonnée à l'origine aléatoire par secteur, qui tient compte des unités ; densite, impact \(S1\) contre témoins \(S2, S3\) : effet BACI/.test(ef[0].phrase)
        && avec.temps_unites === 9, `effet BACI : ${JSON.stringify(ef)}`);
      // with CAR1, another model; the same settings again, the same model refitted (its name, its number)
      const bc = await api('baci_modele', { ...base, evenement: 2006, car1: true });
      const bs = await api('baci_modele', { ...base, evenement: 2006, car1: false });
      const en2 = await api('baci', { ...base, evenement: 2006 }, 'en');
      const ef2 = [].concat(en2.modeles);
      verifier(bc.modele.nom === 'BACI densite (2)' && !bc.nouvelles.length && /CAR1/.test(bc.modele.titre) && bs.modele.nom === 'BACI densite' && bs.modele.numero === bm.modele.numero
        && ef2.length === 3 && ef2.map((x) => x.car1).join() === 'false,true,false' && ef2[2].id === bs.modele.id
        && /^Linear mixed model \(BACI\), with a random intercept per secteur and a CAR1 autocorrelation along annee \(φ = /.test(ef2[1].phrase),
      `CAR1, puis les mêmes réglages : ${bc.modele.nom}, ${bs.modele.nom} ; ${JSON.stringify(ef2.map((x) => x.phrase))}`);
      const sansVar = (await api('annuler', { projet: q.id })).projet;
      verifier(sansVar.variables.length === nVariables && !sansVar.modeles.some((m) => /^BACI/.test(m.nom)), 'annuler : les deux variables et le modèle quittent le projet');
      await api('variable_calculee', { projet: q.id, nom: 'période BACI (2006)', formule: 'ifelse(annee >= 2007, 1, 0)' });
      const libre = await api('baci_modele', { ...base, evenement: 2006 });
      verifier([].concat(libre.nouvelles).join() === 'période BACI (2006) (2),impact BACI (S1 contre S2, S3)'
        && (await api('projet', { projet: q.id })).variables.length === nVariables + 3, `un nom pris par une autre variable : le nom libre suivant (${[].concat(libre.nouvelles).join(', ')})`);
    });
    await etape('paquets de lecture des formats', async () => {
      const r = await api('console', { code: 'all(vapply(c("haven", "readxl", "foreign", "zip"), requireNamespace, TRUE, quietly = TRUE))' });
      verifier(r.journal.some((e) => e.type === 'sortie' && e.texte.includes('[1] TRUE')), 'haven, readxl, foreign ou zip absent');
    });
    await etape('coller un tableau', async () => {
      const r = await api('coller', { texte: 'nom\tx\ty\r\na\t1,5\t2\r\nb\t3\t4,25\r\nc\t5\t1\r\n', nom: 'Collé' });
      verifier(r.nom === 'Collé' && r.modifie && r.variables.length === 2 && r.observations.nom[2] === 'c', `${r.variables.length} variables`);
    });
    await etape('ajouter des colonnes d’un tableau par une clé (une, puis deux), annuler ; résumer par groupe', async () => {
      // deux placettes par site et par année, de 2019 à 2021, sur les sites S1 à S3 ; densité = année − 2018 + placette
      const lignes = [];
      for (const s of ['S1', 'S2', 'S3']) for (const an of [2019, 2020, 2021]) for (const pl of [1, 2]) lignes.push([`${s}-${an}-${pl}`, s, an, an - 2018 + pl]);
      const q = await api('coller', { texte: ['nom\tsite\tannee\tdensite', ...lignes.map((l) => l.join('\t'))].join('\r\n'), nom: 'Placettes' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      // a table of sites in a file (S4 matches no row, S3 has no row): its columns and the key proposed by name
      const f = path.join(dossier, 'sites.csv');
      fs.writeFileSync(f, 'Site;altitude;sol\nS2;200;argile\nS1;100;sable\nS4;400;limon\n');
      const t = await api('table_a_joindre', { projet: q.id, chemin: f });
      verifier(t.n === 3 && t.colonnes.nom.join() === 'Site,altitude,sol' && t.colonnes.nombres.join() === 'false,true,false'
        && t.cles.length === 1 && t.cles[0].projet === v.site && t.cles[0].table === 'Site', `tableau lu : ${JSON.stringify(t)}`);
      const avant = q.revision;
      const r = await api('ajouter_colonnes', { projet: q.id, chemin: t.chemin, nom: t.nom, cles_projet: [v.site], cles_table: ['Site'], colonnes: ['altitude', 'sol'] });
      const d = await api('donnees', { projet: q.id });
      const alt = { S1: 100, S2: 200 };
      verifier(r.variables.length === 5 && r.revision === avant + 1 && r.annulable === 'colonnes de sites.csv'
        && d.valeurs[d.variables.indexOf('altitude')].every((x, i) => x === (alt[lignes[i][1]] ?? null))
        && r.jointure.trouvees === 12 && r.jointure.sans_correspondance === 6 && r.jointure.cles_seules === 1
        && [].concat(r.jointure.phrases)[2] === '6 observation(s) sans correspondance, aux valeurs manquantes : S3-2019-1, S3-2019-2, S3-2020-1, S3-2020-2, S3-2021-1 et 1 autre(s).',
      `colonnes ajoutées : ${JSON.stringify(r.jointure)}`);
      // pasted weather by site and year: two keys, proposed; a name already taken gets " (2)"
      const c = await api('table_a_joindre', { projet: q.id, texte: 'site\tannee\tpluie\taltitude\r\nS1\t2019\t600\t1\r\nS1\t2020\t700\t2\r\nS2\t2019\t650\t3\r\n' });
      verifier(c.colle && c.cles.map((x) => `${x.projet}=${x.table}`).join() === `${v.site}=site,${v.annee}=annee` && c.colonnes.nom_ajoute.join() === 'site (2),annee (2),pluie,altitude (2)',
        `cellules collées : ${JSON.stringify(c)}`);
      const r2 = await api('ajouter_colonnes', { projet: q.id, chemin: c.chemin, nom: c.nom, colle: true, cles_projet: [v.site, v.annee], cles_table: ['site', 'annee'], colonnes: ['pluie', 'altitude'] });
      const d2 = await api('donnees', { projet: q.id });
      const pluie = { 'S1 2019': 600, 'S1 2020': 700, 'S2 2019': 650 };
      verifier([].concat(r2.jointure.noms).join() === 'pluie,altitude (2)' && d2.valeurs[d2.variables.indexOf('pluie')].every((x, i) => x === (pluie[`${lignes[i][1]} ${lignes[i][2]}`] ?? null)),
        `deux clés : ${JSON.stringify(r2.jointure)}`);
      let refus = '';
      try { await api('ajouter_colonnes', { projet: q.id, chemin: c.chemin, cles_projet: [v.site], cles_table: ['site'], colonnes: ['pluie'] }); } catch (e) { refus = e.message; }
      verifier(/La clé « S1 » revient 2 fois dans le tableau/.test(refus) && (await api('projet', { projet: q.id })).revision === avant + 2, `une clé qui revient : « ${refus} »`);
      const j = (await api('journal', { depuis: 0 })).entrees.filter((e) => e.type === 'appel').map((e) => e.texte).join('\n');
      verifier(j.includes(`${q.id} <- ajouter_colonnes_table(${q.id}, tableau, cles_projet = c(${v.site}, ${v.annee}), cles_table = c("site", "annee"), colonnes = c("pluie", "altitude"))`)
        && j.includes('k <- match(paste(trimws(d$site), d$annee), paste(trimws(tableau$site), tableau$annee))') && j.includes('textes = c("site", "annee"))  # cellules collées'),
      'journal : ajouter_colonnes_table et match()');
      // Ctrl+Z, twice: the columns leave the project
      await api('annuler', { projet: q.id });
      const u = (await api('annuler', { projet: q.id })).projet;
      verifier(u.variables.map((x) => x.nom).join() === 'site,annee,densite' && !u.annulable, `annuler : ${u.variables.map((x) => x.nom).join()}`);
      // summarize by site: the mean and n of each site, a new project beside the first
      const s = await api('resumer_groupes', { projet: q.id, groupes: [v.site], variables: [v.densite], statistiques: ['moyenne', 'n'] });
      const ds = await api('donnees', { projet: s.id });
      const moyenne = (site) => { const x = lignes.filter((l) => l[1] === site).map((l) => l[3]); return x.reduce((a, b) => a + b, 0) / x.length; };
      verifier(s.id !== q.id && s.nom === 'Placettes par site' && s.modifie && ds.observations.join() === 'S1,S2,S3'
        && ds.variables.join() === 'site,densite (moyenne),densite (n)' && ds.valeurs[1].every((x, i) => Math.abs(x - moyenne(['S1', 'S2', 'S3'][i])) < 1e-12)
        && ds.valeurs[2].every((x) => x === 6) && s.resume.groupes === 3, `résumé : ${JSON.stringify(ds)}`);
      const js = (await api('journal', { depuis: 0 })).entrees.filter((e) => e.type === 'appel').map((e) => e.texte).join('\n');
      verifier(js.includes(`${s.id} <- resumer_par_groupe(${q.id}, ${v.site}, variables = ${v.densite}, statistiques = c("moyenne", "n"))`)
        && js.includes('aggregate(d["densite"], d["site"], function(x) c(moyenne = mean(x, na.rm = TRUE), n = sum(!is.na(x))))'), 'journal : resumer_par_groupe et aggregate()');
    });
    await etape('atelier des données : chercher et remplacer, catégories, unités et descriptions, chacun annulé par Ctrl+Z', async () => {
      // relevés de terrain : -9999 pour « pas de relevé », un habitat mal orthographié, une teneur « abs »
      const lignes = ['nom\thabitat\taltitude\tdensite\tteneur'];
      const habitats = ['forêt', 'prairie', 'foret', 'lande', 'prairie', 'forêt', 'lande', 'prairie', 'forêt', 'prairie', 'lande', 'forêt'];
      habitats.forEach((h, i) => lignes.push([`R${i + 1}`, h, 100 + 50 * i, i === 4 ? -9999 : (h.startsWith('for') ? 10 : h === 'prairie' ? 6 : 3) + 0.01 * (100 + 50 * i) + (i % 3) * 0.3,
        i % 4 === 1 ? 'abs' : `${i},5`].join('\t')));
      let q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Relevés' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const valeurs = async (nom) => { const d = await api('donnees', { projet: q.id }); return d.valeurs[d.variables.indexOf(nom)]; };
      // the preview changes nothing; the replacement, one undo step
      const a = await api('remplacer', { projet: q.id, apercu: true, variables: [v.densite], chercher: '-9999', remplacer: '' });
      verifier(a.remplacement.cellules === 1 && (await valeurs('densite'))[4] === -9999 && !(await api('projet', { projet: q.id })).annulable,
        `aperçu : ${JSON.stringify(a.remplacement)}`);
      q = await api('remplacer', { projet: q.id, variables: [v.densite], chercher: '-9999', remplacer: '' });
      verifier(q.remplacement.cellules === 1 && (await valeurs('densite'))[4] === null && q.annulable === 'remplacement de -9999', `remplacé : ${q.annulable}`);
      // a text column becomes numbers: "abs" by 0, its commas read
      q = await api('remplacer', { projet: q.id, chercher: 'abs', remplacer: '0' });
      const teneur = q.variables.find((x) => x.nom === 'teneur');
      verifier(!teneur.qualitative && (await valeurs('teneur'))[1] === 0 && (await valeurs('teneur'))[2] === 2.5 && [].concat(q.remplacement.phrases).some((x) => /devient quantitative/.test(x)),
        `teneur : ${JSON.stringify(teneur)}`);
      let refus = '';
      try { await api('remplacer', { projet: q.id, chercher: 'introuvable', remplacer: 'x' }); } catch (e) { refus = e.message; }
      verifier(refus === 'remplacer : Aucune cellule à remplacer.', `rien à remplacer : « ${refus} »`);
      // a regression before the categories change: the last category observed is its reference
      const m = await api('ajuster', { projet: q.id, type: 'LM', x: [v.habitat, v.altitude], y: [v.densite], echelle: 'none', nom: 'LMhab' });
      const termes = (r) => [].concat(r.coefficients_regression.terme);
      verifier(!termes(m).includes('habitat(prairie)') && termes(m).includes('habitat(foret)'), `référence avant : ${termes(m).join()}`);
      // the categories: foret merged into forêt, the order changed, lande the reference (last)
      q = await api('categories', { projet: q.id, variable: v.habitat, noms: ['forêt', 'forêt', 'lande', 'prairie'], ordre: ['prairie', 'forêt', 'lande'] });
      const hab = q.variables.find((x) => x.nom === 'habitat');
      verifier([].concat(hab.categories).join() === 'prairie,forêt,lande' && q.categories.reference === 'lande' && q.annulable === 'catégories de habitat'
        && (await valeurs('habitat'))[2] === 'forêt', `catégories : ${JSON.stringify(q.categories)}`);
      // the model fitted before keeps its results, but can't be refitted nor predict until fitted again (a name changed)
      verifier(termes(await api('modele', { modele: m.id })).join() === termes(m).join(), 'le modèle garde ses résultats');
      refus = '';
      try { await api('reajuster', { modele: m.id, exclues: [1] }); } catch (e) { refus = e.message; }
      verifier(/ont changé depuis l'ajustement de LMhab/.test(refus), `réajuster un modèle aux catégories changées : « ${refus} »`);
      // its definition in the project followed: fitted again, lande is the reference
      const def = q.modeles.find((x) => x.nom === 'LMhab');
      const m2 = await api('ajuster', { projet: q.id, numero: def.numero });
      verifier(!termes(m2).includes('habitat(lande)') && termes(m2).includes('habitat(prairie)') && termes(m2).includes('habitat(forêt)'), `référence après : ${termes(m2).join()}`);
      // a new order only: the model of the session is mapped to it, and refits
      q = await api('categories', { projet: q.id, variable: v.habitat, noms: ['prairie', 'forêt', 'lande'], ordre: ['forêt', 'prairie', 'lande'] });
      const m3 = await api('reajuster', { modele: m2.id, exclues: [2] });
      verifier(m3.N === m2.N - 1 && termes(m3).includes('habitat(forêt)'), `réajusté après un nouvel ordre : N = ${m3.N}`);
      // a model whose condition names categories (not a term of it): a renamed category, then Ctrl+Z,
      // and it is refitted from its own definition each time (serveur.R: suivre_conditions_seance)
      const mc = await api('ajuster', { projet: q.id, type: 'LM', x: [v.altitude], y: [v.densite], echelle: 'none', nom: 'LMcond', condition: 'habitat %in% c("prairie", "lande")' });
      q = await api('categories', { projet: q.id, variable: v.habitat, noms: ['forêt', 'pré', 'lande'], ordre: ['forêt', 'pré', 'lande'] });
      const mc2 = await api('reajuster', { modele: mc.id, exclues: [] });
      await api('annuler', { projet: q.id });
      const mc3 = await api('reajuster', { modele: mc.id, exclues: [] });
      verifier(mc.N === 6 && mc2.N === 6 && mc3.N === 6, `une condition sur des catégories renommées puis rendues : N = ${mc.N}, ${mc2.N}, ${mc3.N}`);
      // units and descriptions, kept in the summary, in the Methods text, saved and read again
      q = await api('decrire_variables', { projet: q.id, descriptions: [{ variable: v.densite, unite: 'ind./ha', description: 'Densité des adultes' }, { variable: v.altitude, unite: 'm' }] });
      const dens = q.variables.find((x) => x.nom === 'densite');
      verifier(dens.unite === 'ind./ha' && dens.description === 'Densité des adultes' && q.variables.find((x) => x.nom === 'altitude').unite === 'm'
        && q.annulable === 'unités et descriptions', `unités : ${JSON.stringify(dens)}`);
      const meth = await api('methodes', { modele: m3.id, langue: 'en' });
      verifier(meth.paragraphes[0].includes('the response is “densite” (ind./ha)'), meth.paragraphes[0]);
      const res = await api('resultats_texte', { modele: m3.id, langue: 'fr' });
      verifier(res.paragraphes.some((x) => /« altitude » : β = [-0-9,]+ par m/.test(x)), [].concat(res.paragraphes).join(' '));
      const f = path.join(dossier, 'releves.nuee');
      await api('enregistrer', { projet: q.id, chemin: f });
      const relu = await api('ouvrir', { chemin: f });
      verifier(relu.variables.find((x) => x.nom === 'densite').unite === 'ind./ha', 'unités relues du .nuee');
      // the export: a workbook with a "Variables" sheet, opened again with its units
      const x = path.join(dossier, 'releves.xlsx');
      await api('exporter_donnees', { projet: q.id, chemin: x });
      const rx = await api('ouvrir', { chemin: x });
      verifier(!rx.choisir && rx.variables.find((y) => y.nom === 'densite').unite === 'ind./ha' && rx.variables.find((y) => y.nom === 'altitude').unite === 'm',
        `classeur relu : ${JSON.stringify(rx.choisir || rx.variables)}`);
      const j = (await api('journal', { depuis: 0 })).entrees.filter((e) => e.type === 'appel').map((e) => e.texte).join('\n');
      verifier(j.includes(`${q.id} <- remplacer_valeurs(${q.id}, variables = ${v.densite}, chercher = "-9999", remplacer = "")`)
        && j.includes(`${q.id} <- changer_categories(${q.id}, ${v.habitat}, c("forêt", "forêt", "lande", "prairie"), ordre = c("prairie", "forêt", "lande"))`)
        && j.includes(`${q.id} <- decrire_variables(${q.id}, list(list(variable = ${v.densite}, unite = "ind./ha", description = "Densité des adultes"), list(variable = ${v.altitude}, unite = "m")))`),
      'journal : remplacer_valeurs, changer_categories, decrire_variables');
      // Ctrl+Z: the units, the order, the merge, the replacements leave, one by one
      let u = (await api('annuler', { projet: q.id })).projet;
      verifier(!u.variables.find((x) => x.nom === 'densite').unite, 'annuler les unités');
      await api('annuler', { projet: q.id });
      u = (await api('annuler', { projet: q.id })).projet;
      verifier([].concat(u.variables.find((x) => x.nom === 'habitat').categories).join() === 'foret,forêt,lande,prairie', 'annuler les catégories');
      await api('annuler', { projet: q.id });
      u = (await api('annuler', { projet: q.id })).projet;
      verifier((await valeurs('densite'))[4] === -9999 && u.variables.find((x) => x.nom === 'teneur').qualitative, 'annuler les remplacements');
      // a block of cells pasted from a spreadsheet (two rows, two columns; null: a cell left), then undone
      const d0 = await api('donnees', { projet: q.id });
      const c = await api('coller_cellules', { projet: q.id, observations: d0.uid_observations.slice(0, 2), variables: [v.altitude, v.habitat],
        valeurs: [['1,5', 'marais'], ['', null]] });
      verifier(c.collage.cellules === 3 && (await valeurs('altitude'))[0] === 1.5 && (await valeurs('altitude'))[1] === null
        && (await valeurs('habitat'))[0] === 'marais' && (await valeurs('habitat'))[1] === habitats[1] && c.annulable === 'cellules collées', `collé : ${JSON.stringify(c.collage)}`);
      await api('annuler', { projet: q.id });
      verifier((await valeurs('altitude'))[0] === 100 && (await valeurs('habitat'))[0] === habitats[0], 'annuler le collage');
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
    await etape('KoboToolbox et ODK Central : l’export reconnu à l’ouverture, son aperçu, une répétition importée avec des colonnes de l’envoi, tel quel, refait au redémarrage (moteur/R/kobo.R)', async () => {
      const r = (x) => JSON.stringify(x.replace(/\\/g, '/'));
      const xlsx = path.join(dossier, 'releve kobo.xlsx');
      const form = path.join(dossier, 'formulaire.xlsx');
      // a Kobo workbook (groups in the headers, a geopoint, a select_multiple with its 0/1 columns, a repeat sheet) and its XLSForm in two languages
      await api('console', { code: [
        'envois <- data.frame(start = c("2024-05-03T07:58:00.000+02:00", "2024-05-04T08:05:00.000+02:00", "2024-05-05T09:00:00.000+02:00"),',
        '  "group_site/site" = c("S1", "S2", "S3"), "group_site/habitat" = c("foret", "prairie", "foret"), "group_site/gps" = c("43.61 3.87 50 5", "43.65 3.9 60 4", NA),',
        '  "_group_site/gps_latitude" = c(43.61, 43.65, NA), "_group_site/gps_longitude" = c(3.87, 3.9, NA), meteo = c("soleil vent", "pluie", NA),',
        '  "meteo/soleil" = c(1, 0, NA), "meteo/pluie" = c(0, 1, NA), "meteo/vent" = c(1, 0, NA), "_id" = 1:3, "_uuid" = c("u1", "u2", "u3"),',
        '  "_submission_time" = c("2024-05-03T06:45:12", "2024-05-03T06:45:12", "2024-05-05T08:00:00"), "__version__" = "v1", "_index" = 1:3, check.names = FALSE)',
        'obs <- data.frame("observation/espece" = c("turmer", "parmaj", "turmer", "erirub"), "observation/effectif" = c(2, 1, 3, 1), "_index" = 1:4,',
        '  "_parent_table_name" = "Relevé", "_parent_index" = c(1, 1, 2, 3), check.names = FALSE)',
        `openxlsx::write.xlsx(list("Relevé" = envois, observation = obs), ${r(xlsx)})`,
        'survey <- data.frame(type = c("start", "begin_group", "text", "select_one habitat", "geopoint", "end_group", "select_multiple meteo", "begin_repeat", "select_one espece", "integer", "end_repeat"),',
        '  name = c("start", "group_site", "site", "habitat", "gps", NA, "meteo", "observation", "espece", "effectif", NA),',
        '  "label::Français (fr)" = c(NA, "Site", "Site", "Habitat", "Position", NA, "Météo", "Observation", "Espèce", "Effectif", NA),',
        '  "label::English (en)" = c(NA, "Site", "Site", "Habitat", "Location", NA, "Weather", "Observation", "Species", "Count", NA), check.names = FALSE)',
        'choices <- data.frame(list_name = c("habitat", "habitat", "meteo", "meteo", "meteo", "espece", "espece", "espece"),',
        '  name = c("prairie", "foret", "soleil", "pluie", "vent", "turmer", "parmaj", "erirub"),',
        '  "label::Français (fr)" = c("Prairie", "Forêt", "Soleil", "Pluie", "Vent", "Merle noir", "Mésange charbonnière", "Rougegorge familier"),',
        '  "label::English (en)" = c("Grassland", "Forest", "Sun", "Rain", "Wind", "Common blackbird", "Great tit", "European robin"), check.names = FALSE)',
        `openxlsx::write.xlsx(list(survey = survey, choices = choices, settings = data.frame(form_title = "Relevé des oiseaux")), ${r(form)})`].join('\n') });
      // opened: the export is recognised, the preview of its import comes back instead of a project
      const o = await api('ouvrir', { chemin: xlsx });
      verifier(o.kobo && !o.id && o.kobo.outil === 'kobo' && o.kobo.tables.map((x) => x.nom).join() === 'Relevé,observation' && o.kobo.table === 'Relevé'
        && o.kobo.phrases.includes('Métadonnées retirées (6) : start, _id, _uuid, _submission_time, __version__, _index.'), `aperçu à l’ouverture : ${JSON.stringify(o).slice(0, 400)}`);
      const a = await api('kobo_apercu', { chemin: xlsx, table: 'observation', garder: ['envoi'], formulaire: form });
      verifier(a.lignes === 4 && a.parents.length === 1 && a.parents[0].table === 'Relevé' && a.parents[0].colonnes.includes('submission_time')
        && a.formulaire.langue === 'Français (fr)' && a.coordonnees && a.metadonnees.find((m) => m.role === 'envoi').garde, `aperçu de la répétition : ${JSON.stringify(a).slice(0, 600)}`);
      const p = await api('kobo_importer', { chemin: xlsx, nom: 'releve kobo.xlsx', table: 'observation', garder: ['envoi'],
        parents: { Relevé: ['habitat', 'gps_longitude', 'gps_latitude', 'submission_time'] }, formulaire: form });
      const noms = p.variables.map((v) => v.nom);
      verifier(p.id && p.nom === 'releve kobo (observation).xlsx' && p.table === 'observation' && noms.join() === 'espece,effectif,habitat,gps_longitude,gps_latitude,submission_time'
        && p.coordonnees && p.coordonnees.nom_x === 'gps_longitude' && p.coordonnees.systeme === 4326 && p.collecte.phrases.length > 3
        && p.variables.find((v) => v.nom === 'espece').categories.join() === 'Merle noir,Mésange charbonnière,Rougegorge familier'
        && p.variables.find((v) => v.nom === 'habitat').categories.join() === 'Prairie,Forêt', `import : ${JSON.stringify(p).slice(0, 600)}`);
      const d = await api('donnees', { projet: p.id });
      verifier(d.valeurs[d.variables.indexOf('gps_latitude')].join() === '43.61,43.61,43.65,' && d.valeurs[d.variables.indexOf('effectif')].join() === '2,1,3,1',
        `valeurs jointes : ${JSON.stringify(d.valeurs)}`);
      // as it is (the sheets to choose from), and the import redone from its settings (the engine restarted)
      const brut = await api('ouvrir', { chemin: xlsx, brut: true });
      verifier(Array.isArray(brut.choisir) && brut.choisir.join() === 'Relevé,observation', `tel quel : ${JSON.stringify(brut).slice(0, 200)}`);
      const relance = await api('ouvrir', { chemin: xlsx, table: p.table, kobo: p.kobo });
      verifier(relance.id !== p.id && relance.variables.map((v) => v.nom).join() === noms.join() && relance.nom === p.nom, `refait : ${JSON.stringify(relance.kobo)}`);
      const j = (await api('journal', { depuis: 0 })).entrees.filter((e) => e.type === 'appel').map((e) => e.texte).join('\n');
      verifier(j.includes(`${p.id} <- importer_collecte(`) && j.includes('table = "observation", garder = "envoi"'), 'journal : importer_collecte');
      const en = await api('kobo_apercu', { chemin: xlsx }, 'en');
      verifier(en.phrases[0] === 'KoboToolbox export (Excel workbook, 2 tables): the submissions table “Relevé”, 3 row(s).', `en anglais : ${en.phrases[0]}`);
      let refus = '';
      try { await api('ouvrir', { chemin: tableau(), collecte: true }); } catch (e) { refus = e.message; }
      verifier(/essai\.csv n'est pas un export de KoboToolbox ni d'ODK Central/.test(refus), `un tableau ordinaire importé : « ${refus} »`);
      // an ODK Central zip (one CSV per table, PARENT_KEY), sent by the browser as File › Open does
      const odk = path.join(dossier, 'odk');
      fs.mkdirSync(odk);
      fs.writeFileSync(path.join(odk, 'releve.csv'), ['SubmissionDate,group_site-site,group_site-gps-Latitude,group_site-gps-Longitude,meteo,meta-instanceID,KEY,SubmitterName,ReviewState',
        '2024-05-03T06:45:12.000Z,S1,43.61,3.87,soleil vent,uuid:a,uuid:a,Anne,approved', '2024-05-04T07:00:00.000Z,S2,43.65,3.9,pluie,uuid:b,uuid:b,Paul,'].join('\n'));
      fs.writeFileSync(path.join(odk, 'releve-observation.csv'), ['espece,effectif,PARENT_KEY,KEY', 'turmer,2,uuid:a,uuid:a/observation[1]',
        'parmaj,1,uuid:a,uuid:a/observation[2]', 'erirub,1,uuid:b,uuid:b/observation[1]'].join('\n'));
      const zipf = path.join(dossier, 'releve.zip');
      await api('console', { code: `zip::zip(${r(zipf)}, files = c("releve.csv", "releve-observation.csv"), root = ${r(odk)})` });
      const envoi = await fetch(`http://127.0.0.1:${port}/api/televerser?nom=releve.zip`, { method: 'POST', body: fs.readFileSync(zipf), headers: { Cookie: cookie } });
      const z = await envoi.json();
      verifier(envoi.ok && z.kobo && z.kobo.outil === 'odk' && z.kobo.tables.map((x) => x.nom).join() === 'releve,observation', `archive envoyée : ${JSON.stringify(z).slice(0, 300)}`);
      const pz = await api('kobo_importer', { chemin: z.chemin, nom: z.nom, table: 'observation', formulaire: form });
      verifier(pz.variables.map((v) => v.nom).join() === 'espece,effectif,site,gps_longitude,gps_latitude,meteo_soleil,meteo_pluie,meteo_vent'
        && pz.coordonnees && pz.coordonnees.nom_y === 'gps_latitude' && pz.nom === 'releve (observation).zip', `répétition d’ODK : ${pz.variables.map((v) => v.nom).join()}`);
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
    await etape('CCA (partielle, permutée par site) : échelles 1 à 3, tests par axe, séquentiels et par terme, script, méthodes', async () => {
      // 30 relevés de 6 espèces le long de la température et du sable, 5 sites
      let g = 13;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\tsite\ttemp\tsable\tsp1\tsp2\tsp3\tsp4\tsp5\tsp6'];
      for (let i = 1; i <= 30; i += 1) {
        const temp = 8 + 10 * alea();
        const sable = 100 * alea();
        const sp = [temp, 20 - temp, sable / 10, 10 - sable / 10, (temp * sable) / 100, 3].map((x) => Math.round(x * (0.5 + alea())) + (x > 0 ? 1 : 0));
        lignes.push([`r${i}`, `S${1 + (i % 5)}`, temp.toFixed(2), sable.toFixed(1), ...sp].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Relevés CCA' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const especes = ['sp1', 'sp2', 'sp3', 'sp4', 'sp5', 'sp6'].map((n) => v[n]);
      const cca = await api('ajuster', { projet: q.id, type: 'CCA', x: [v.temp], y: especes, echelle: 'none', nom: 'CCA',
        options: { permutations: 99, graine: 2, z: [v.sable], strates: { by: 'variable', variable: v.site } } });
      verifier(cca.type === 'CCA' && cca.analyse === true && cca.prediction === false && cca.N === 30 && Object.keys(cca.ordination).join() === '1,2,3'
        && cca.ordination['2'].sites.CCA1.length === 30 && /^R2aj /.test(cca.chiffres) && cca.strates === 'site' && [].concat(cca.covariables).join() === 'sable',
      `CCA : ${cca.chiffres}, N = ${cca.N}`);
      const tests = await api('rda_tests', { modele: cca.id });
      verifier([].concat(tests.axes.axe).length === 2 && [].concat(tests.sequentiels.terme).join() === 'temp,Résidus' && [].concat(tests.termes.terme).length === 2
        && typeof [].concat(tests.sequentiels.p)[0] === 'number', `tests : ${JSON.stringify(tests.sequentiels)}`);
      const s = await api('script_r', { modele: cca.id, chemin: path.join(dossier, 'cca.R'), version: 'essai' });
      verifier(fs.existsSync(s.script) && !s.ecart && !/vegan::/.test(fs.readFileSync(s.script, 'utf8').replace(/#.*$/gm, '')), `script R : ${s.ecart || 'R seul'}`);
      const mt = await api('methodes', { modele: cca.id, langue: 'en' });
      verifier(mt.paragraphes.some((x) => /canonical correspondence analysis/.test(x)) && mt.references.some((x) => /ter Braak, C\. J\. F\. \(1986\)/.test(x)), 'texte des méthodes');
      const e = await api('exporter', { modele: cca.id, chemin: path.join(dossier, 'cca.xlsx') });
      verifier(fs.existsSync(e.chemin), 'exporter les résultats');
      let refus = '';
      try { await api('rda_partition', { modele: cca.id }); } catch (err) { refus = err.message; }
      verifier(/pas pour une CCA/.test(refus), `partition refusée : « ${refus} »`);
    });
    await etape('espèces indicatrices (IndVal.g, combinaisons de groupes, permutations par site, p corrigées)', async () => {
      // 30 relevés en trois milieux (5 sites) : sp1 du bois, sp2 de la lande, sp3 du pré, sp4 du bois et de la lande, sp5 partout
      let g = 17;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const milieux = ['bois', 'lande', 'pré'];
      const lignes = ['nom\tmilieu\tsite\tsp1\tsp2\tsp3\tsp4\tsp5'];
      for (let i = 0; i < 30; i += 1) {
        const k = i % 3;
        const sp = [k === 0 ? 5 : 0, k === 1 ? 4 : 0, k === 2 ? 3 : 0, k < 2 ? 2 : 0, 2].map((x) => Math.round(x * (0.5 + alea())));
        lignes.push([`r${i + 1}`, milieux[k], `S${1 + (i % 5)}`, ...sp].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Milieux' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const especes = ['sp1', 'sp2', 'sp3', 'sp4', 'sp5'].map((n) => v[n]);
      const iv = await api('ajuster', { projet: q.id, type: 'INDVAL', x: [], y: especes, classe: v.milieu, echelle: 'none', nom: 'IndVal',
        options: { permutations: 199, graine: 4, strates: { by: 'variable', variable: v.site } } });
      const groupe = Object.fromEntries([].concat(iv.especes.espece).map((e, i) => [e, [].concat(iv.especes.groupe)[i]]));
      verifier(iv.type === 'INDVAL' && iv.analyse === true && iv.N === 30 && [].concat(iv.combinaisons).length === 7 && iv.strates === 'site'
        && [].concat(iv.vues_defaut).join() === 'indicatrices,indval_graphe' && groupe.sp1 === 'bois' && groupe.sp4 === 'bois+lande'
        && typeof [].concat(iv.especes.p_ajuste)[0] === 'number' && iv.retenues.corrigees >= 3 && iv.reglages.classe === v.milieu, `IndVal : ${iv.chiffres}, ${JSON.stringify(groupe)}`);
      const mt = await api('methodes', { modele: iv.id, langue: 'en' });
      verifier(mt.paragraphes.some((x) => /IndVal\.g/.test(x)) && mt.references.some((x) => /Dufrêne, M\./.test(x)), 'texte des méthodes');
      const e = await api('exporter', { modele: iv.id, chemin: path.join(dossier, 'indval.xlsx') });
      verifier(fs.existsSync(e.chemin), 'exporter les résultats');
      const sc = await api('script_r', { modele: iv.id, chemin: path.join(dossier, 'indval.R'), version: 'essai', langue: 'en' });
      verifier(fs.existsSync(sc.script) && !sc.ecart && /indval_test <- function/.test(fs.readFileSync(sc.script, 'utf8')), `script indval.R : ${sc.ecart || 'sans écart'}`);
      let refus = '';
      try { await api('ajuster', { projet: q.id, type: 'INDVAL', x: [v.sp1], y: [v.sp2], classe: v.milieu, echelle: 'none' }); } catch (err) { refus = err.message; }
      verifier(/variable X/.test(refus), `des X refusés : « ${refus} »`);
    });
    await etape('raréfaction et accumulation des espèces, par groupe ; des effectifs non entiers (moteur/R/rarefaction.R)', async () => {
      // 20 relevés de 6 espèces (des effectifs) dans deux milieux ; une valeur manquante
      let g = 23;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\tmilieu\tsp1\tsp2\tsp3\tsp4\tsp5\tsp6'];
      for (let i = 0; i < 20; i += 1) {
        const sp = [8, 5, 3, 2, 1, 1].map((x) => String(Math.round(x * 2 * alea()) + (x === 8 ? 1 : 0)));
        if (i === 4) sp[2] = '';
        lignes.push([`r${i + 1}`, i < 10 ? 'haie' : 'champ', ...sp].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Effectifs' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const especes = ['sp1', 'sp2', 'sp3', 'sp4', 'sp5', 'sp6'].map((n) => v[n]);
      const r = await api('rarefaction', { projet: q.id, especes, groupe: v.milieu });
      const col = (df, k) => [].concat(df[k]);
      const N = col(r.observations, 'N');
      verifier(r.n === 19 && r.manquantes === 1 && r.entiers === true && col(r, 'niveaux').length === 2 && col(r.estimateurs, 'groupe').length === 2
        && r.taille === Math.min(...N) && col(r.observations, 'Sm').every((x) => typeof x === 'number') && col(r.courbes, 'uid').length > 19,
      `raréfaction : ${JSON.stringify({ n: r.n, manquantes: r.manquantes, taille: r.taille, entiers: r.entiers })}`);
      // the last point of each accumulation curve: the species seen in its group
      const acc = col(r.accumulation, 'richesse');
      const fins = col(r.accumulation, 'sites').map((s, i) => [s, i]).filter(([s], i, a) => i === a.length - 1 || a[i + 1][0] === 1).map(([, i]) => acc[i]);
      verifier(fins.length === 2 && fins.every((x, k) => Math.abs(x - col(r.estimateurs, 'S')[k]) < 1e-9), `accumulation : ${JSON.stringify(fins)}`);
      const q2 = await api('coller', { texte: ['nom\ta\tb', 'x\t1,5\t2', 'y\t0\t3', 'z\t2\t1'].join('\r\n'), nom: 'Couverts' });
      const v2 = Object.fromEntries(q2.variables.map((x) => [x.nom, x.uid]));
      const r2 = await api('rarefaction', { projet: q2.id, especes: [v2.a, v2.b] });
      verifier(r2.entiers === false && !r2.courbes && col(r2.observations, 'Sm').every((x) => x === null) && col(r2.estimateurs, 'jack1').length === 1,
        `non entiers : ${JSON.stringify(r2.observations)}`);
      let refus = '';
      try { await api('rarefaction', { projet: q.id, especes: [v.sp1] }); } catch (err) { refus = err.message; }
      verifier(/deux espèces/.test(refus), `une seule espèce refusée : « ${refus} »`);
    });
    await etape('PCoA, NMDS, PERMANOVA et test de Mantel (moteur/R/communautes.R)', async () => {
      // 30 relevés de 6 espèces le long d'un gradient de température, trois habitats, 5 sites
      let g = 13;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\tsite\thabitat\ttemp\tsp1\tsp2\tsp3\tsp4\tsp5\tsp6'];
      for (let i = 1; i <= 30; i += 1) {
        const temp = 8 + 10 * alea();
        const h = ['lande', 'prairie', 'bois'][i % 3];
        const sp = [temp, 20 - temp, h === 'bois' ? 8 : 2, h === 'lande' ? 6 : 1, temp / 2, 4].map((v) => Math.round(v * (0.5 + alea())));
        lignes.push([`r${i}`, `S${1 + (i % 5)}`, h, temp.toFixed(2), ...sp].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Communautés' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const especes = ['sp1', 'sp2', 'sp3', 'sp4', 'sp5', 'sp6'].map((n) => v[n]);
      const pcoa = await api('ajuster', { projet: q.id, type: 'PCOA', x: [v.temp, v.habitat], y: especes, echelle: 'none', nom: 'PCoA',
        options: { distance: 'bray', transformation: 'racine2', permutations: 99, graine: 2 } });
      verifier(pcoa.analyse === true && pcoa.prediction === false && pcoa.A === 0 && pcoa.N === 30 && /PCoA1/.test(pcoa.chiffres) && pcoa.observations.PCoA1.length === 30
        && [].concat(pcoa.vues_defaut).join() === 'ordination_dist,valeurs_propres_pcoa' && [].concat(pcoa.ajustement.vecteurs.terme).join() === 'temp'
        && [].concat(pcoa.ajustement.centroides.terme).length === 3 && [].concat(pcoa.especes.terme).length === 6 && pcoa.reglages.options.transformation === 'racine2',
      `PCoA : ${pcoa.chiffres}, N = ${pcoa.N}`);
      const seule = await api('ajuster', { projet: q.id, type: 'PCOA', x: [], y: especes, echelle: 'none', nom: 'PCoA seule', options: { correction: 'lingoes' } });
      verifier(!seule.ajustement && seule.distance.correction === 'lingoes' && ![].concat(seule.valeurs_propres.negative).some(Boolean), 'PCoA sans X, corrigée par Lingoes');
      const nmds = await api('ajuster', { projet: q.id, type: 'NMDS', x: [v.temp], y: especes, echelle: 'none', nom: 'NMDS', options: { distance: 'bray', k: 2, departs: 5, permutations: 99 } });
      verifier(nmds.type === 'NMDS' && nmds.stress > 0 && nmds.stress < 0.3 && [].concat(nmds.stress_departs.stress).length === 6 && nmds.shepard.dissimilarite.length === 435
        && nmds.observations.NMDS2.length === 30 && [].concat(nmds.vues_defaut).join() === 'ordination_dist,shepard', `NMDS : ${nmds.chiffres}`);
      const perma = await api('ajuster', { projet: q.id, type: 'PERMANOVA', x: [v.temp, v.habitat], y: especes, classe: v.site, echelle: 'none', nom: 'PERMANOVA',
        options: { distance: 'bray', par: 'marges', permutations: 99, graine: 3 } });
      verifier(perma.type === 'PERMANOVA' && [].concat(perma.table.terme).join() === 'temp,habitat,Résidus,Total' && perma.dispersions.length === 2
        && perma.dispersions.map((d) => d.variable).join() === 'habitat,site' && typeof perma.modele.p === 'number'
        && [].concat(perma.vues_defaut).join() === 'permanova,dispersion' && perma.reglages.classe === v.site, `PERMANOVA : ${perma.chiffres}`);
      const mantel = await api('ajuster', { projet: q.id, type: 'MANTEL', x: [v.temp], y: especes, echelle: 'UV', nom: 'Mantel',
        options: { distance: 'bray', methode: 'spearman', permutations: 99, graine: 4, z: [v.habitat], strates: { by: 'variable', variable: v.site } } });
      verifier(mantel.type === 'MANTEL' && mantel.test.r > 0 && typeof mantel.test.r_partiel === 'number' && [].concat(mantel.correlogramme.table.classe).length >= 3
        && mantel.paires.y.length === 435 && mantel.reglages.options.methode === 'spearman' && [].concat(mantel.reglages.options.z).join() === String(v.habitat)
        && mantel.strates === 'site', `Mantel : ${mantel.chiffres}`);
      const re = await api('reajuster', { modele: perma.id, exclues: [1, 2] });
      verifier(re.type === 'PERMANOVA' && re.N === 28 && re.A === 0, `PERMANOVA réajustée : N = ${re.N}`);
      for (const [m, f] of [[pcoa, 'pcoa.xlsx'], [nmds, 'nmds.xlsx'], [perma, 'permanova.xlsx'], [mantel, 'mantel.xlsx']]) {
        const e = await api('exporter', { modele: m.id, chemin: path.join(dossier, f) });
        verifier(fs.existsSync(e.chemin), `exporter ${f}`);
      }
      for (const [m, f, langue] of [[pcoa, 'pcoa.R', 'fr'], [nmds, 'nmds.Rmd', 'en'], [perma, 'permanova.R', 'en'], [mantel, 'mantel.qmd', 'fr']]) {
        const s = await api('script_r', { modele: m.id, chemin: path.join(dossier, f), version: 'essai', langue });
        verifier(fs.existsSync(s.script) && !s.ecart, `script ${f} : ${s.ecart || 'sans écart'}`);
      }
      const mt = await api('methodes', { modele: perma.id, langue: 'en' });
      verifier(mt.paragraphes.some((x) => /permutational multivariate analysis of variance \(PERMANOVA/.test(x)) && mt.paragraphes.some((x) => /adonis2/.test(x))
        && mt.paragraphes.some((x) => /betadisper/.test(x))
        && mt.references.some((x) => /Anderson, M\. J\. \(2001\)/.test(x)), 'texte des méthodes de la PERMANOVA');
      let refus = '';
      try { await api('ajuster', { projet: q.id, type: 'PERMANOVA', x: [], y: especes, echelle: 'none' }); } catch (err) { refus = err.message; }
      verifier(/variables X/.test(refus), `PERMANOVA sans X refusée : « ${refus} »`);
      let refusEn = '';
      try { await api('ajuster', { projet: q.id, type: 'PCOA', x: [], y: [v.temp, v.habitat], echelle: 'none' }, 'en'); } catch (err) { refusEn = err.message; }
      verifier(/quantitative variables only/.test(refusEn), `Y qualitative refusée en anglais : « ${refusEn} »`);
      let refusHCA = '';
      try { await api('hca', { modele: nmds.id }); } catch (err) { refusHCA = err.message; }
      verifier(/autre analyse/.test(refusHCA), `dendrogramme refusé : « ${refusHCA} »`);
    });
    await etape('document Quarto reproductible : l’export vers R des modèles cités à côté, chaque valeur en R, rendu par Quarto quand il est là', async () => {
      // a PLS, a GLM and a PERMANOVA of 40 made-up relevés, cited by a document resolved as the
      // interface resolves it (references.js, rendu_document.js), then written as a reproducible .qmd
      let g = 21;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\thabitat\ttemp\tph\tsp1\tsp2\tsp3\tsp4\toiseaux\tbiomasse'];
      for (let i = 1; i <= 40; i += 1) {
        const temp = 8 + 10 * alea();
        const h = ['bois', 'lande', 'prairie'][i % 3];
        const sp = [temp, 20 - temp, h === 'bois' ? 8 : 2, 4].map((x) => Math.round(x * (0.5 + alea())));
        lignes.push([`r${i}`, h, temp.toFixed(2), (6 + alea()).toFixed(2), ...sp, Math.round((temp / 3) * (0.5 + alea())), (2 + 0.3 * temp + alea()).toFixed(3)].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Reproductible' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const modeles = {
        M1: await api('ajuster', { projet: q.id, type: 'PLS', x: [v.temp, v.ph, v.sp1, v.sp2], y: [v.biomasse], A: 2, nom: 'M1' }),
        M2: await api('ajuster', { projet: q.id, type: 'GLM', x: [v.temp, v.habitat], y: [v.oiseaux], echelle: 'none', nom: 'M2', famille: 'poisson' }),
        M3: await api('ajuster', { projet: q.id, type: 'PERMANOVA', x: [v.habitat, v.temp], y: [v.sp1, v.sp2, v.sp3, v.sp4], echelle: 'none', nom: 'M3',
          options: { distance: 'bray', permutations: 99, graine: 3 } }),
      };
      verifier(![].concat((await api('export_r_possible', { projet: q.id, noms: Object.keys(modeles) })).sans_export).length, 'les trois s’exportent vers R');
      const interfaceUrl = (f) => require('url').pathToFileURL(path.join(interfaceDir, f)).href;
      const { formeReference, texteForme, separerCible } = await import(interfaceUrl('references.js'));
      const { analyser, pluginReferences, versQmd, referencesReproductibles } = await import(interfaceUrl('rendu_document.js'));
      const md = require('markdown-it')({ html: false, linkify: true }).use(pluginReferences);
      const terme = [].concat(modeles.M2.coefficients_regression.terme)[1];
      const valeurs = ['M1.Q2', 'M1.R2Y:%', 'M1.N', 'M1.vip.temp', 'M1.coef.temp', 'M1.RMSEcv', 'M1.cvanova.p', 'M1.gain', 'M2.AIC', `M2.coef.${terme}`, `M2.p.${terme}`,
        'M2.deviance_expliquee', 'M2.Q2', 'M3.N', 'M3.K'];
      const texte = `---\ntitle: "Essai"\n---\n\n${valeurs.map((c, i) => `- v${i + 1}: {{${c}}}`).join('\n')}\n\n{{M1.scores #fig-scores | Scores}}\n\n`
        + '{{M2.table.coefficients #tbl-coef | Coefficients}}\n\n{{M3.permanova #fig-permanova | PERMANOVA}}\n\n{{M1.biplot | Biplot}}\n\n{{M1.methodes}}\n';
      const analyse = analyser(md, texte);
      const PNG = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==';
      let figures = 0;
      const resolus = analyse.refs.map((tok) => {
        const ref = tok.meta.ref;
        const c = separerCible(ref.cible, Object.keys(modeles));
        const m = modeles[c.modele];
        const base = { modele: m.id, chemin: c.chemin, legende: ref.legende, etiquette: ref.etiquette };
        if (['scores', 'permanova', 'biplot'].includes(c.chemin[0])) {
          figures += 1;
          return { ...base, genre: 'figure', image: PNG, largeur: 900, hauteur: 580, numero: figures, nom: c.chemin[0], vue: c.chemin[0], variante: {} };
        }
        if (c.chemin[0] === 'table') return { ...base, genre: 'tableau', blocs: [{ entetes: ['x'], lignes: [['1']] }], numero: 1, nom: 'Coefficients', cle: c.chemin[1] };
        if (c.chemin[0] === 'methodes') return { ...base, genre: 'texte', paragraphes: ['Les méthodes.'] };
        const f = formeReference(m, c.chemin, ref);
        return { ...base, genre: 'valeur', texte: texteForme(f, 'fr'), forme: f };
      });
      verifier(resolus.every((x) => !x.erreur), 'références résolues');
      const ici = path.join(dossier, 'reproductible');
      fs.mkdirSync(ici);
      const rep = await api('document_reproductible', { projet: q.id, langue: 'fr', version: 'essai', nom: 'Essai', dossier: ici, ...referencesReproductibles(resolus) });
      const refs = [].concat(rep.references);
      const enDur = refs.map((x, i) => (x && x.raison ? `${analyse.refs[i].meta.ref.cible} (${x.raison})` : null)).filter(Boolean);
      verifier(refs.slice(0, valeurs.length).every((x) => x && x.code) && enDur.length === 2 && /biplot/.test(enDur[0]) && /methodes/.test(enDur[1]),
        `${valeurs.length} valeurs en R ; en dur : ${enDur.join(', ')}`);
      verifier(['nuee.R', 'M1.R', 'M1 - donnees.csv', 'M2.R', 'M3.R'].every((f) => fs.existsSync(path.join(ici, 'Essai_R', f))) && [].concat(rep.relatifs).length === 7,
        `à côté : ${[].concat(rep.relatifs).join(', ')}`);
      const qmd = versQmd(texte, analyse, resolus, { fichierFigure: (x) => `Essai_figures/figure-${x.numero}.png`, langue: 'fr', titre: 'Essai', reproductible: rep });
      verifier(/^```\{r\}\n#\| label: nuee-modeles/m.test(qmd.texte) && qmd.texte.includes('- v1: `r nuee_nombre(M1$composantes$Q2cum[2])`')
        && qmd.texte.includes('#| label: fig-scores') && qmd.texte.includes('#| label: tbl-coef') && qmd.images.length === 1, 'le .qmd : son premier bloc, ses valeurs en R, ses blocs');
      fs.writeFileSync(path.join(ici, 'Essai.qmd'), qmd.texte);
      fs.mkdirSync(path.join(ici, 'Essai_figures'));
      fs.writeFileSync(path.join(ici, 'Essai_figures', 'figure-3.png'), Buffer.from(PNG.split(',')[1], 'base64'));
      // rendered by Quarto when the computer has it (with knitr and rmarkdown for R): the numbers of the page are Nuée's
      const quarto = [process.env.QUARTO_PATH, ...(process.env.PATH || '').split(path.delimiter).map((d) => path.join(d, process.platform === 'win32' ? 'quarto.exe' : 'quarto')),
        path.join(process.env.LOCALAPPDATA || '', 'Programs', 'Quarto', 'bin', 'quarto.exe'), path.join(process.env.ProgramFiles || 'C:\\Program Files', 'Quarto', 'bin', 'quarto.exe'),
        '/usr/local/bin/quarto', '/opt/quarto/bin/quarto'].filter(Boolean).find((f) => { try { return fs.statSync(f).isFile(); } catch { return false; } });
      if (!quarto || !rep.R.knitr || !rep.R.rmarkdown) {
        console.log(`          (${quarto ? 'knitr ou rmarkdown absent' : 'Quarto absent'} : le .qmd n’est pas rendu)`);
        return;
      }
      execFileSync(quarto, ['render', 'Essai.qmd', '--to', 'html'], { cwd: ici, env: { ...process.env, QUARTO_R: rep.R.bin }, stdio: 'pipe' });
      const page = fs.readFileSync(path.join(ici, 'Essai.html'), 'utf8');
      const rendus = Object.fromEntries([...page.matchAll(/<li>v(\d+): ([^<]*)<\/li>/g)].map((x) => [x[1], x[2].replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&')]));
      const differents = resolus.slice(0, valeurs.length).map((x, i) => (rendus[i + 1] === x.texte ? null : `${valeurs[i]} : ${rendus[i + 1]} au lieu de ${x.texte}`)).filter(Boolean);
      verifier(!differents.length && /id="fig-scores"/.test(page) && /id="tbl-coef"/.test(page) && /id="fig-permanova"/.test(page),
        `rendu par Quarto : ${differents.join(', ') || 'les nombres de Nuée'}`);
      console.log(`          (rendu par Quarto : ${valeurs.length} nombres, ${resolus.slice(0, 3).map((x) => x.texte).join(' ; ')}…)`);
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
    await etape('données spatiales : coordonnées, carte, autocorrélation, lissage spatial, GeoPackage (moteur/R/spatial.R, geopackage.R)', async () => {
      // 40 placettes en Lambert-93 autour de la Tour du Valat, une valeur qui varie dans l'espace
      let g = 5;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['placette\tX_L93\tY_L93\tx\tv'];
      for (let i = 1; i <= 40; i += 1) {
        const X = 820000 + 25000 * alea();
        const Y = 6260000 + 25000 * alea();
        const x = alea();
        lignes.push([`p${i}`, X.toFixed(1), Y.toFixed(1), x.toFixed(3), (Math.sin(X / 4000) + Math.cos(Y / 5000) + x + 0.2 * alea()).toFixed(4)].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Placettes' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const c0 = await api('coordonnees', { projet: q.id });
      verifier(c0.coordonnees === null && c0.proposition.x === v.X_L93 && c0.proposition.y === v.Y_L93 && c0.proposition.systeme === 2154,
        `coordonnées proposées : ${JSON.stringify(c0.proposition)}`);
      const c1 = await api('coordonnees', { projet: q.id, regler: true, x: v.X_L93, y: v.Y_L93, systeme: '2154' });
      verifier(c1.coordonnees.systeme === 2154 && c1.projet.coordonnees.nom_x === 'X_L93' && c1.projet.annulable, 'coordonnées réglées, annulables');
      const carte = await api('carte', { projet: q.id });
      verifier(carte.uid.length === 40 && carte.lon.every((l) => l > 4.4 && l < 4.9) && carte.lat.every((l) => l > 43.3 && l < 43.8), 'carte : longitudes et latitudes');
      const a = await api('autocorrelation_spatiale', { projet: q.id, variable: v.v, permutations: 99 });
      verifier(a.n === 40 && a.moran.I > 0 && a.moran.p_permutations <= 0.05 && a.correlogramme.I.length === 10 && a.variogramme.gamma.length > 3,
        `I de Moran de v : ${a.moran.I}`);
      const sansLissage = await api('ajuster', { projet: q.id, type: 'GAM', x: [v.x], y: [v.v], echelle: 'none', famille: 'gaussienne', nom: 'G0' });
      const gam = await api('ajuster', { projet: q.id, type: 'GAM', x: [v.x], y: [v.v], echelle: 'none', famille: 'gaussienne', nom: 'G1',
        lisse_spatial: { variables: [v.X_L93, v.Y_L93] } });
      verifier([].concat(gam.courbes.terme).includes('s(X_L93, Y_L93)') && gam.reglages.lisse_spatial && gam.qualite.AIC < sansLissage.qualite.AIC
        && gam.observations['s(X_L93, Y_L93)'].length === 40, `lissage spatial : AIC ${gam.qualite.AIC} contre ${sansLissage.qualite.AIC}`);
      const r = await api('autocorrelation_spatiale', { modele: gam.id, permutations: 0 });
      verifier(r.moran.I < a.moran.I, `les résidus du lissage spatial : I = ${r.moran.I}`);
      const f = await api('formule', { projet: q.id, type: 'GAM', x: [v.x], y: [v.v], lisse_spatial: { variables: [v.X_L93, v.Y_L93], k: 20 } });
      verifier(f.formule === 'v ~ x + s(X_L93, Y_L93, k = 20)', `formule : ${f.formule}`);
      const gpkg = path.join(dossier, 'placettes.gpkg');
      await api('exporter_donnees', { projet: q.id, chemin: gpkg });
      const resultats = path.join(dossier, 'resultats.gpkg');
      await api('exporter', { modele: gam.id, chemin: resultats });
      const o = await api('ouvrir', { chemin: gpkg });
      verifier(o.coordonnees && o.coordonnees.systeme === 2154 && o.observations.uid.length === 40, 'GeoPackage écrit, rouvert avec ses coordonnées');
      const choix = await api('ouvrir', { chemin: resultats });
      verifier(choix.choisir && [].concat(choix.choisir).includes('G1'), `GeoPackage de résultats : ${[].concat(choix.choisir || []).join(', ')}`);
      const pr = await api('par_reponse', { modele: sansLissage.id, reponses: [v.v, v.Y_L93], correction: 'holm' });
      verifier([].concat(pr.resume.reponse).length === 2 && [].concat(pr.tableau.terme).includes('x') && pr.correction === 'holm' && /Holm/.test(pr.methodes),
        `un modèle par réponse : ${[].concat(pr.resume.reponse).join(', ')}`);
      const rt = await api('resultats_texte', { modele: gam.id, langue: 'en' });
      verifier([].concat(rt.paragraphes).some((x) => /The model of “v” uses 40 observations/.test(x)) && [].concat(rt.paragraphes).some((x) => /s\(X_L93, Y_L93\), edf =/.test(x)),
        `texte des résultats : ${[].concat(rt.paragraphes).join(' ').slice(0, 160)}`);
      const t = await api('projet_texte', { projet: q.id, version: 'essai' });
      verifier(/"coordonnees"/.test(t.texte) && /"paquets"/.test(t.texte), 'projet en texte, pour un rapport de problème');
    });
    await etape('valeurs de rasters aux observations : GeoTIFF écrit octet par octet, grille ASCII, refus, annuler, enregistrer (moteur/R/raster.R)', async () => {
      // un GeoTIFF petit-boutiste, réels de 32 bits en une bande de lignes, en Lambert-93 : 50 × 40 cellules de 500 m depuis
      // (815000, 6290000), de valeur 10 + i + j / 2 (ligne i, colonne j), une cellule sans valeur (-9999)
      const geotiff = (chemin, { epsg = 2154, compression = 1 } = {}) => {
        const W = 50; const H = 40;
        const image = Buffer.alloc(W * H * 4);
        for (let i = 0; i < H; i += 1) for (let j = 0; j < W; j += 1) image.writeFloatLE(i === 5 && j === 6 ? -9999 : 10 + i + j / 2, (i * W + j) * 4);
        const court = (...v) => { const b = Buffer.alloc(2 * v.length); v.forEach((x, k) => b.writeUInt16LE(x, 2 * k)); return b; };
        const long = (...v) => { const b = Buffer.alloc(4 * v.length); v.forEach((x, k) => b.writeUInt32LE(x, 4 * k)); return b; };
        const reel = (...v) => { const b = Buffer.alloc(8 * v.length); v.forEach((x, k) => b.writeDoubleLE(x, 8 * k)); return b; };
        const entrees = [[256, 4, 1, long(W)], [257, 4, 1, long(H)], [258, 3, 1, court(32)], [259, 3, 1, court(compression)], [262, 3, 1, court(1)],
          [273, 4, 1, long(8)], [277, 3, 1, court(1)], [278, 4, 1, long(H)], [279, 4, 1, long(image.length)], [284, 3, 1, court(1)], [339, 3, 1, court(3)],
          [33550, 12, 3, reel(500, 500, 0)], [33922, 12, 6, reel(0, 0, 0, 815000, 6290000, 0)],
          [34735, 3, 16, court(1, 1, 0, 3, 1024, 0, 1, 1, 1025, 0, 1, 1, 3072, 0, 1, epsg)], [42113, 2, 6, Buffer.from('-9999\0', 'latin1')]];
        // (les valeurs qui ne tiennent pas dans leur entrée, après l'image ; puis le répertoire)
        let position = 8 + image.length;
        const hors = [];
        for (const e of entrees) if (e[3].length > 4) { e.push(position); hors.push(e[3]); position += e[3].length; }
        const repertoire = Buffer.alloc(2 + 12 * entrees.length + 4);
        repertoire.writeUInt16LE(entrees.length, 0);
        entrees.forEach(([etiquette, type, n, valeur, ou], k) => {
          const o = 2 + 12 * k;
          repertoire.writeUInt16LE(etiquette, o); repertoire.writeUInt16LE(type, o + 2); repertoire.writeUInt32LE(n, o + 4);
          if (ou !== undefined) repertoire.writeUInt32LE(ou, o + 8); else valeur.copy(repertoire, o + 8);
        });
        const tete = Buffer.from([0x49, 0x49, 42, 0, 0, 0, 0, 0]);
        tete.writeUInt32LE(position, 4);
        fs.writeFileSync(chemin, Buffer.concat([tete, image, ...hors, repertoire]));
      };
      const tif = path.join(dossier, 'mnt.tif');
      geotiff(tif);
      const jpeg = path.join(dossier, 'photo.tif');
      geotiff(jpeg, { compression: 7 });
      const nord = path.join(dossier, 'nord.tif');
      geotiff(nord, { epsg: 27561 });
      // une grille ASCII d'ESRI de 10 × 8 cellules de 2 km, ses classes, et son .prj (Lambert-93, sans code)
      const asc = path.join(dossier, 'occupation.asc');
      const lignesAsc = ['ncols 10', 'nrows 8', 'xllcorner 815000', 'yllcorner 6270000', 'cellsize 2500', 'NODATA_value -1'];
      for (let i = 0; i < 8; i += 1) lignesAsc.push(Array.from({ length: 10 }, (_, j) => (i + j) % 4).join(' '));
      fs.writeFileSync(asc, lignesAsc.join('\n'));
      fs.writeFileSync(path.join(dossier, 'occupation.prj'), 'PROJCS["RGF_1993_Lambert_93",GEOGCS["GCS_RGF_1993",DATUM["D_RGF_1993",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic"],PARAMETER["False_Easting",700000.0],PARAMETER["False_Northing",6600000.0],PARAMETER["Central_Meridian",3.0],PARAMETER["Standard_Parallel_1",49.0],PARAMETER["Standard_Parallel_2",44.0],PARAMETER["Latitude_Of_Origin",46.5],UNIT["Meter",1.0]]');
      // des placettes : au centre de la cellule (3, 4), au coin de quatre cellules, sur la cellule sans valeur, hors du raster, sans coordonnées
      const x0 = 815000; const y0 = 6290000;
      const placettes = [['centre', x0 + 4.5 * 500, y0 - 3.5 * 500], ['coin', x0 + 10 * 500, y0 - 10 * 500], ['sans_valeur', x0 + 6.5 * 500, y0 - 5.5 * 500],
        ['dehors', x0 - 1000, y0 - 1000], ['sans_coordonnees', '', y0 - 2000], ['loin', x0 + 20.25 * 500, y0 - 30.75 * 500]];
      const q = await api('coller', { texte: ['placette\tX\tY\tz', ...placettes.map((l, k) => [...l, k].join('\t'))].join('\r\n'), nom: 'Placettes (raster)' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      let sans = '';
      try { await api('raster_infos', { projet: q.id, chemins: [tif] }); } catch (e) { sans = e.message; }
      verifier(/n'a pas de coordonnées/.test(sans), `sans coordonnées : « ${sans} »`);
      await api('coordonnees', { projet: q.id, regler: true, x: v.X, y: v.Y, systeme: '2154' });
      const infos = [].concat((await api('raster_infos', { projet: q.id, chemins: [tif, asc, jpeg, nord] })).fichiers);
      const [i1, i2, i3, i4] = infos;
      verifier(i1.systeme.code === 2154 && i1.largeur === 50 && i1.hauteur === 40 && i1.type === 'float32' && i1.resolution === '500 m' && i1.methode === 'bilineaire'
        && i1.nodata === -9999 && i1.dedans === 4 && i1.observations === 5 && [].concat(i1.noms).join() === 'mnt' && i1.taille === fs.statSync(tif).size,
      `le GeoTIFF : ${JSON.stringify(i1)}`);
      verifier(i2.format === 'ASCII' && i2.systeme.code === 2154 && i2.methode === 'cellule' && i2.resolution === '2500 m', `la grille ASCII : ${JSON.stringify(i2)}`);
      verifier(/la compression JPEG ne se lit pas ici/.test(i3.erreur) && /EPSG:27561/.test(i4.erreur), `refus : « ${i3.erreur} » ; « ${i4.erreur} »`);
      const en = [].concat((await api('raster_infos', { projet: q.id, chemins: [jpeg] }, 'en')).fichiers)[0];
      verifier(/JPEG compression isn't read here/.test(en.erreur), `en anglais : « ${en.erreur} »`);
      const r = await api('ajouter_raster', { projet: q.id, fichiers: [
        { chemin: tif, methode: 'cellule', noms: ['altitude'] }, { chemin: tif, methode: 'bilineaire', noms: ['altitude_bil'] },
        { chemin: tif, methode: 'moyenne', rayon: 600, noms: ['altitude_600m'] }, { chemin: asc, methode: 'majorite', rayon: 3000, noms: ['occupation'] }] });
      const d = await api('donnees', { projet: q.id });
      const col = (nom) => d.valeurs[d.variables.indexOf(nom)];
      const f = (i, j) => 10 + i + j / 2;
      // (le coin de quatre cellules : la cellule en bas à droite, l'interpolation à mi-chemin ; dans 600 m du centre, la cellule et ses quatre voisines)
      verifier(JSON.stringify(col('altitude')) === JSON.stringify([f(3, 4), f(10, 10), null, null, null, f(30, 20)])
        && Math.abs(col('altitude_bil')[1] - f(9.5, 9.5)) < 1e-9 && col('altitude_bil')[0] === f(3, 4) && col('altitude_bil')[3] === null
        && Math.abs(col('altitude_600m')[0] - f(3, 4)) < 1e-9 && r.annulable === 'valeurs de mnt.tif, occupation.asc',
      `valeurs : ${JSON.stringify({ cellule: col('altitude'), bilineaire: col('altitude_bil'), cercle: col('altitude_600m') })}`);
      verifier([].concat(r.raster.phrases)[0] === '« altitude » : 3 observation(s) sur 6 ont une valeur, manquante pour 1 hors du raster, 1 sur une cellule sans valeur, 1 sans coordonnées.'
        && col('occupation').filter((x) => x !== null).length === 5, `phrases : ${[].concat(r.raster.phrases).join(' | ')}`);
      const j = (await api('journal', { depuis: 0 })).entrees.filter((e) => e.type === 'appel').map((e) => e.texte).join('\n');
      verifier(j.includes(`${q.id} <- ajouter_valeurs_rasters(${q.id}, list(list(chemin = `) && j.includes('methode = "moyenne", rayon = 600'), 'journal : ajouter_valeurs_rasters');
      // le texte des Méthodes d'un modèle qui utilise ces variables dit d'où elles viennent
      const lm = await api('ajuster', { projet: q.id, type: 'LM', x: [r.variables.find((x) => x.nom === 'altitude_bil').uid], y: [v.z], echelle: 'none', nom: 'R1' });
      const mt = await api('methodes', { modele: lm.id, langue: 'fr' });
      const me = await api('methodes', { modele: lm.id, langue: 'en' });
      verifier(mt.paragraphes[0].includes('Lues dans des rasters à la position de chaque observation : « altitude_bil » de mnt.tif (GeoTIFF, cellules de 500 m, RGF93 v1 / Lambert-93 ; par interpolation bilinéaire')
        && me.paragraphes[0].includes('“altitude_bil” from mnt.tif (GeoTIFF, 500 m cells, RGF93 v1 / Lambert-93; by bilinear interpolation'), `méthodes : ${mt.paragraphes[0]}`);
      // enregistré et rouvert : des variables comme les autres ; puis Ctrl+Z les retire
      const fichier = path.join(dossier, 'placettes_raster.nuee');
      await api('enregistrer', { projet: q.id, chemin: fichier });
      const o = await api('ouvrir', { chemin: fichier });
      verifier(['altitude', 'altitude_bil', 'altitude_600m', 'occupation'].every((n) => o.variables.some((x) => x.nom === n)), 'enregistré et rouvert');
      const u = (await api('annuler', { projet: q.id })).projet;
      verifier(u.variables.map((x) => x.nom).join() === 'X,Y,z' && u.annulable === 'coordonnées', `annuler : ${u.variables.map((x) => x.nom).join()}`);
      let refus = '';
      try { await api('ajouter_raster', { projet: q.id, fichiers: [{ chemin: nord, noms: ['x'] }] }); } catch (e) { refus = e.message; }
      verifier(/nord.tif : le système de coordonnées du fichier, EPSG:27561/.test(refus) && !(await api('projet', { projet: q.id })).variables.some((x) => x.nom === 'x'),
        `un système inconnu refusé, rien d'ajouté : « ${refus} »`);
    });
    await etape('TRIM : indices et tendances de comptages d’un tableau site × année (moteur/R/trim.R)', async () => {
      // 8 sites comptés de 2015 à 2021, des comptages manquants (vides) et des lignes absentes, un habitat par site
      let g = 3;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\tsite\tannee\tcompte\thabitat'];
      for (let s = 1; s <= 8; s += 1) {
        const niveau = 5 + 20 * alea();
        for (let a = 2015; a <= 2021; a += 1) {
          if (s === 3 && a === 2018) continue;  // (une ligne absente : un comptage manquant)
          const attendu = niveau * Math.exp(-0.06 * (a - 2015)) * (0.7 + 0.6 * alea());
          lignes.push([`c${s}-${a}`, `S${s}`, a, alea() < 0.2 ? '' : Math.round(attendu), s <= 4 ? 'bocage' : 'marais'].join('\t'));
        }
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Comptages' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const options = { site: { by: 'variable', variable: v.site }, temps: { by: 'variable', variable: v.annee }, modele: 3, surdispersion: true, correlation: true };
      const def = { projet: q.id, type: 'TRIM', x: [v.habitat], y: [v.compte], echelle: 'none', nom: 'TRIM', options };
      const verif = await api('verifier', def);
      verifier(![].concat(verif.variables.uid || []).length, `vérification avant l’ajustement : ${JSON.stringify(verif.variables)}`);
      const tr = await api('ajuster', def);
      verifier(tr.analyse === true && tr.prediction === false && tr.type === 'TRIM' && tr.A === 0 && tr.N === 55 && tr.I === 8 && tr.J === 7
        && [].concat(tr.vues_defaut).join() === 'trim_indices,trim_totaux' && [].concat(tr.indices.impute).length === 7 && [].concat(tr.pentes.quoi).join() === 'imputee,modele'
        && /^pente /.test(tr.chiffres) && tr.comptes.length === 8 && tr.comptes[0].length === 7 && tr.observations.compte.length === 55 && typeof tr.sigma2 === 'number'
        && [].concat(tr.tests.test).includes('covariable') && tr.reglages.options.modele === 3, `TRIM : ${tr.chiffres}, N = ${tr.N}`);
      const m2 = await api('ajuster', { ...def, x: [], nom: 'TRIM 2', options: { ...options, modele: 2, changements: [2015, 2018], correlation: false } });
      verifier(m2.modele_trim === 2 && [].concat(m2.changements).join() === '2015,2018' && [].concat(m2.tests.test).join() === 'changement,changement' && m2.rho === null,
        `TRIM modèle 2 : ${m2.chiffres}`);
      const re = await api('reajuster', { modele: tr.id, exclues: [1] });
      verifier(re.type === 'TRIM' && re.N === 54, `réajusté : N = ${re.N}`);
      const mt = await api('methodes', { modele: tr.id, langue: 'en' });
      verifier(mt.paragraphes.some((x) => /TRIM model 3/.test(x)) && mt.paragraphes.some((x) => /overall slope/.test(x)) && mt.references.some((x) => /TRIM 3 manual/.test(x)), 'texte des méthodes');
      const e = await api('exporter', { modele: tr.id, chemin: path.join(dossier, 'trim.xlsx') });
      verifier(fs.existsSync(e.chemin), 'exporter les résultats');
      let refus = '';
      try { await api('ajuster', { ...def, x: [], options: { ...options, site: { by: 'variable', variable: v.habitat } } }); } catch (err) { refus = err.message; }
      verifier(/Deux lignes pour le même site et le même temps/.test(refus), `deux lignes pour un site et un temps refusées : « ${refus} »`);
      let refusHCA = '';
      try { await api('hca', { modele: tr.id }); } catch (err) { refusHCA = err.message; }
      verifier(/autre analyse/.test(refusHCA), `dendrogramme refusé : « ${refusHCA} »`);
    });
    await etape('échantillonnage par distances : densité et effectif de transects linéaires (moteur/R/distance.R)', async () => {
      // 2 strates, 14 lignes : des oiseaux détectés selon une demi-normale (σ = 30 m), une ligne par détection, les lignes sans détection gardées
      let g = 11;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\tstrate\taire\ttransect\tlongueur\tdistance\ttaille'];
      let k = 0;
      for (const [strate, aire, n] of [['bocage', 12, 8], ['marais', 7, 6]]) {
        for (let j = 1; j <= n; j += 1) {
          const longueur = (1 + 2 * alea()).toFixed(2);
          let vus = 0;
          for (let a = 0; a < 40 * longueur; a += 1) {
            const x = 100 * alea();
            if (alea() < Math.exp(-(x * x) / (2 * 30 * 30))) {
              k += 1;
              vus += 1;
              lignes.push([`d${k}`, strate, aire, `${strate}-${j}`, longueur, x.toFixed(1), 1 + Math.floor(3 * alea())].join('\t'));
            }
          }
          if (!vus) { k += 1; lignes.push([`d${k}`, strate, aire, `${strate}-${j}`, longueur, '', ''].join('\t')); }
        }
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Transects' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const options = { transect: 'ligne', echantillon: { by: 'variable', variable: v.transect }, effort: v.longueur, strate: { by: 'variable', variable: v.strate }, aire: v.aire,
        troncature: { type: 'pourcentage', valeur: 5 }, cle: 'hn', serie: 'cos', ajustements: 'aic', monotonie: 'stricte', unites: { distance: 'm', effort: 'km', aire: 'km2' } };
      const def = { projet: q.id, type: 'DISTANCE', x: [], y: [v.distance], echelle: 'none', nom: 'Distance', options };
      const verif = await api('verifier', def);
      verifier(![].concat(verif.variables.uid || []).length && ![].concat(verif.observations.uid || []).length, `vérification avant l’ajustement : ${JSON.stringify(verif.variables)}`);
      const dm = await api('ajuster', def);
      const dn = dm.densite;
      verifier(dm.analyse === true && dm.prediction === false && dm.type === 'DISTANCE' && dm.A === 0 && dm.N === k && dm.n > 50
        && [].concat(dm.vues_defaut).join() === 'distance_detection,distance_densite' && / · D [0-9.,]+ \/km² \(CV /.test(dm.chiffres)
        && [].concat(dn.strate).length === 3 && [].concat(dn.strate)[2] === null && [].concat(dn.D).every((x) => x > 0) && [].concat(dn.N).every((x) => x > 0)
        && [].concat(dm.candidats.modele)[0] === 'hn' && dm.courbe.x.length === 101 && dm.adequation.qq.uid.length === dm.n && typeof dm.adequation.cvm.p === 'number'
        && dm.troncature.pourcentage === 5 && dm.au_dela > 0 && dm.effectif.quoi === 'ESW' && dm.reglages.options.cle === 'hn', `distance : ${dm.chiffres}, n = ${dm.n}`);
      const groupes = await api('ajuster', { ...def, nom: 'Distance 2', options: { ...options, taille: v.taille, cle: 'hr', serie: 'cos', ajustements: 1, troncature: { type: 'distance', valeur: 90 } } });
      verifier(groupes.groupes === true && groupes.modele_detection === 'hr + cos(2)' && [].concat(groupes.densite_groupes.D).length === 3 && groupes.troncature.w === 90,
        `groupes, hasard et un terme : ${groupes.chiffres}`);
      const points = await api('ajuster', { ...def, nom: 'Distance 3', options: { ...options, transect: 'point', effort: null, strate: null, aire: null, unites: { distance: 'm', effort: 'm', aire: 'ha' } } });
      verifier(points.transect === 'point' && points.effectif.quoi === 'EDR' && points.densite_seule === true && [].concat(points.densite.D).length === 1 && / \/ha /.test(points.chiffres),
        `points, la densité seule : ${points.chiffres}`);
      const classes = await api('ajuster', { ...def, nom: 'Distance 4', options: { ...options, intervalles: [0, 15, 30, 50, 75, 100] } });
      verifier([].concat(classes.intervalles).join() === '0,15,30,50,75,100' && classes.adequation.cvm === null && classes.adequation.qq === null
        && classes.histogramme.compte.length === 5 && classes.troncature.w === 100, `distances par intervalles : ${classes.chiffres}`);
      // des covariables de la fonction de détection (les variables Z : options.z) : la strate, qualitative, et la taille des groupes
      const cov = await api('ajuster', { ...def, nom: 'Distance 5', options: { ...options, taille: v.taille, z: [v.strate, v.taille] } });
      const niveaux = [].concat(cov.courbes_niveaux);
      verifier(cov.formule === 'strate + taille' && cov.p_moyenne === true && cov.modele_detection === 'hn ~ strate + taille' && [].concat(cov.candidats.covariables).join('|') === '|strate + taille'
        && [].concat(cov.termes_covariables).join() === 'strate(marais),taille' && [].concat(cov.covariables.reference)[0] === 'bocage' && niveaux.length === 1
        && [].concat(niveaux[0].niveaux).join() === 'bocage,marais' && niveaux[0].g.length === 2 && niveaux[0].g[0].length === 101 && [].concat(cov.observations.p).filter((x) => x !== null).length === cov.n
        && [].concat(cov.reglages.options.z).length === 2, `covariables : ${cov.chiffres}`);
      const sc = await api('script_r', { modele: cov.id, chemin: path.join(dossier, 'distance_covariables.R'), version: 'essai', langue: 'en' });
      verifier(fs.existsSync(sc.script) && !sc.ecart && /covariate_matrix\(covariates\)/.test(fs.readFileSync(sc.script, 'utf8')), `script à covariables : ${sc.ecart || 'sans écart'}`);
      const re = await api('reajuster', { modele: dm.id, exclues: [1] });
      verifier(re.type === 'DISTANCE' && re.N === k - 1, `réajusté : N = ${re.N}`);
      const mt = await api('methodes', { modele: dm.id, langue: 'en' });
      verifier(mt.paragraphes.some((x) => /distance sampling/.test(x)) && mt.paragraphes.some((x) => /Satterthwaite/.test(x)) && mt.references.some((x) => /^Buckland/.test(x))
        && mt.references.some((x) => /^Fewster/.test(x)), 'texte des méthodes');
      const e = await api('exporter', { modele: dm.id, chemin: path.join(dossier, 'distance.xlsx') });
      verifier(fs.existsSync(e.chemin), 'exporter les résultats');
      let refus = '';
      try { await api('ajuster', { ...def, options: { ...options, effort: null } }); } catch (err) { refus = err.message; }
      verifier(/longueur de la ligne/.test(refus), `des lignes sans effort refusées : « ${refus} »`);
      for (const [m, f, langue, motif] of [[dm, 'distance.R', 'en', /choose_detection <- function/], [groupes, 'distance_groupes.R', 'fr', /densite_groupes <- densite_echantillonnage\(/]]) {
        const s = await api('script_r', { modele: m.id, chemin: path.join(dossier, f), version: 'essai', langue });
        verifier(fs.existsSync(s.script) && !s.ecart && motif.test(fs.readFileSync(s.script, 'utf8')), `script ${f} : ${s.ecart || 'sans écart'}`);
      }
      // (the compile dialog of a reproducible Quarto document lists the models without an export to R:
      // distance sampling exports, so none here)
      const sansExport = [].concat((await api('export_r_possible', { projet: q.id, noms: ['Distance'] })).sans_export);
      verifier(sansExport.length === 0, `sans export vers R : ${JSON.stringify(sansExport)}`);
    });
    await etape('forêts aléatoires et arbres boostés (moteur/R/forets.R, boosting.R)', async () => {
      // 12 sites suivis 6 ans : une densité qui baisse au-delà de 12 jours de gel, des X manquants, une classe, une présence
      let g = 7;
      const alea = () => { g = (g * 16807) % 2147483647; return g / 2147483647; };
      const lignes = ['nom\tsite\tgel\thaie\tpluie\tsol\tdensite\tniveau\tpresence'];
      for (let i = 0; i < 72; i += 1) {
        const gel = Math.round(30 * alea());
        const haie = alea();
        const densite = 2 + (gel > 12 ? -1.2 : 0) + (haie > 0.5 ? 0.6 : 0) + (alea() - 0.5);
        lignes.push([`r${i + 1}`, `S${1 + Math.floor(i / 6)}`, i % 11 === 0 ? '' : gel, haie.toFixed(3), (200 * alea()).toFixed(1), ['argile', 'limon'][i % 2],
          densite.toFixed(3), densite > 1.5 ? 'haute' : 'basse', densite > 1.8 ? 1 : 0].join('\t'));
      }
      const q = await api('coller', { texte: lignes.join('\r\n'), nom: 'Sites (forêts)' });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      const xs = [v.gel, v.haie, v.pluie, v.sol];
      const vc = { by: 'variable', variable: v.site };
      const foret = await api('ajuster', { projet: q.id, type: 'FOREST', x: xs, y: [v.densite], vc, echelle: 'none', nom: 'Forêt', options: { arbres: 200 } });
      verifier(foret.analyse === true && foret.prediction === true && foret.type === 'FOREST' && foret.A === 0 && foret.N === 65 && foret.G === 12
        && /^R2 OOB .* arbres$/.test(foret.chiffres) && [].concat(foret.vues_defaut).join() === 'ens_importance,ens_obspred'
        && foret.observations['densite prédit (OOB)'].length === 65 && [].concat(foret.importance.variable).length === 4 && foret.courbe.arbres.length === 200
        && foret.exclues_manquantes === 7 && foret.reglages.options.arbres === 200 && typeof foret.comparaison.Q2 === 'number', `forêt : ${foret.chiffres}, N = ${foret.N}`);
      const dp = await api('dependance_partielle', { modele: foret.id, variables: ['gel'] });
      const d0 = [].concat(dp.dependance)[0];
      verifier(d0.variable === 'gel' && d0.x.length === 25 && d0.y.length === 25 && d0.tapis.length > 1, `dépendance partielle : ${d0.x.length} points`);
      const fc = await api('ajuster', { projet: q.id, type: 'FOREST', x: xs, classe: v.niveau, vc, echelle: 'none', nom: 'Forêt classes', options: { arbres: 200 } });
      verifier(fc.classement === true && [].concat(fc.vues_defaut).join() === 'ens_importance,ens_classement' && fc.classification.oob.table.length === 2
        && fc.observations['probabilité haute'].length === 65 && fc.reglages.classe === v.niveau, `forêt de classement : ${fc.chiffres}`);
      const bo = await api('ajuster', { projet: q.id, type: 'BOOST', x: xs, y: [v.densite], vc, echelle: 'none', nom: 'Boosting',
        options: { loi: 'gaussienne', arbres_max: 120, taux: 0.1, profondeur: 2, min_feuille: 5 } });
      verifier(bo.type === 'BOOST' && bo.N === 65 && bo.meilleur >= 1 && bo.meilleur <= 120 && bo.courbe.validation.length === 120
        && Math.abs([].concat(bo.influence.pourcentage).reduce((a, b) => a + b, 0) - 100) < 1e-6 && [].concat(bo.vues_defaut).join() === 'ens_importance,ens_erreur'
        && /arbres$/.test(bo.chiffres) && bo.reglages.options.profondeur === 2, `arbres boostés : ${bo.chiffres}`);
      const bb = await api('ajuster', { projet: q.id, type: 'BOOST', x: xs, y: [v.presence], vc, echelle: 'none', nom: 'Boosting présence',
        options: { loi: 'binomiale', arbres_max: 80, taux: 0.1, profondeur: 1, min_feuille: 5 } });
      verifier(bb.loi === 'binomiale' && typeof bb.qualite.auc_vc === 'number' && bb.classification.vc.table.length === 2 && /^AUC/.test(bb.chiffres), `présence : ${bb.chiffres}`);
      const db = await api('dependance_partielle', { modele: bb.id });
      verifier([].concat(db.dependance).length === 4 && [].concat(db.dependance).every((d) => d.reponse.every((p) => p > 0 && p < 1)), 'dépendance partielle : 4 variables, des probabilités');
      const pr = await api('predire', { modele: foret.id });
      verifier(pr.observations['densite prédit'].length === 72 && pr.observations['densite prédit'].filter((x) => x === null).length === 7, 'prédire avec la forêt');
      const pb = await api('predire', { modele: bb.id });
      verifier(pb.observations['presence prédit'].length === 72, 'prédire avec les arbres boostés');
      const re = await api('reajuster', { modele: foret.id, exclues: [2, 3] });
      verifier(re.type === 'FOREST' && re.N === 63 && re.A === 0, `forêt réajustée : N = ${re.N}`);
      const e = await api('exporter', { modele: bo.id, chemin: path.join(dossier, 'boosting.xlsx') });
      verifier(fs.existsSync(e.chemin), 'exporter les résultats des arbres boostés');
      const mf = await api('methodes', { modele: foret.id, langue: 'en' });
      verifier(mf.paragraphes.some((x) => /ranger/.test(x) && /out-of-bag|bootstrap|replacement/.test(x)) && mf.references.some((x) => /^Breiman, L\. \(2001\)/.test(x)),
        `texte des méthodes de la forêt : ${mf.paragraphes.join(' ').slice(0, 200)}`);
      const mb = await api('methodes', { modele: bo.id, langue: 'fr' });
      verifier(mb.paragraphes.some((x) => /gbm/.test(x)) && mb.references.some((x) => /^Elith/.test(x)) && mb.references.some((x) => /^Friedman, J\. H\. \(2002\)/.test(x)),
        'texte des méthodes des arbres boostés');
      // the partial dependence asked for is kept in the project file (serveur.R: garder_avec_definition):
      // opened again and refitted, the forest and the boosted trees have it, in their Methods text too
      const fd = path.join(dossier, 'forets.nuee');
      await api('enregistrer', { projet: q.id, chemin: fd });
      const q2 = await api('ouvrir', { chemin: fd });
      const numero = (nom) => q2.modeles.find((x) => x.nom === nom).numero;
      const foret2 = await api('ajuster', { projet: q2.id, numero: numero('Forêt') });
      const bb2 = await api('ajuster', { projet: q2.id, numero: numero('Boosting présence') });
      const mf2 = await api('methodes', { modele: foret2.id, langue: 'en' });
      verifier([].concat(foret2.dependance).map((x) => x.variable).join() === 'gel' && JSON.stringify([].concat(foret2.dependance)[0].y) === JSON.stringify(d0.y)
        && [].concat(bb2.dependance).length === 4 && mf2.paragraphes.some((x) => /partial dependence/.test(x)),
      `dépendance partielle enregistrée avec le projet, rouverte : ${[].concat(foret2.dependance || []).map((x) => x.variable).join()} ; ${[].concat(bb2.dependance || []).length} pour les arbres boostés`);
      let refus = '';
      try { await api('ajuster', { projet: q.id, type: 'BOOST', x: xs, classe: v.niveau, vc, echelle: 'none' }); } catch (err) { refus = err.message; }
      verifier(/pas une classe/.test(refus), `arbres boostés d'une classe refusés : « ${refus} »`);
    });
    await etape('résultats clés d’un modèle, comparés avant et après un réajustement (moteur/R/resultats_cles.R)', async () => {
      const { offreModele, formeReference } = await import(require('url').pathToFileURL(path.join(interfaceDir, 'references.js')).href);
      const lm = await api('ajuster', { projet: p.id, type: 'LM', x: [uid.x1, uid.x2, uid.zone], y: [uid.y], echelle: 'none', nom: 'LMcles' });
      // (another analysis: references.js reads it like a model of components, N, K, A, G, RMSEE...)
      const arbre = await api('ajuster', { projet: p.id, type: 'TREE', x: [uid.x1, uid.x2, uid.x4], y: [uid.y], echelle: 'none', nom: 'Arbrecles', options: { minbucket: 8 } });
      const lda = await api('ajuster', { projet: p.id, type: 'LDA', x: [uid.x1, uid.x2, uid.x4], classe: uid.groupe, echelle: 'UV', nom: 'LDAcles' });
      // every value a document can cite (references.js offers it) is an entry, under its reference or
      // an alias, with the number the document writes
      for (const m of [pca, pls, opls, da, lm, arbre, lda]) {
        const r = await api('resultats_cles', { modele: m.id });
        verifier(r.modele === m.id && Array.isArray(r.entrees) && r.entrees.length > 0, `${m.nom} : aucune entrée`);
        const parCle = new Map();
        for (const e of r.entrees) for (const c of [e.cle].concat(e.alias || [])) parCle.set(c, e);
        const terme = m.regression ? [].concat(m.coefficients_regression.terme)[1] : null;
        const chemins = offreModele(m).filter((o) => o.type === 'valeur').map((o) => o.chemin)
          .concat(m.regression ? [`est.${terme}`, `p.${terme}`, `ic.${terme}`] : m.analyse ? [] : ['R2X.1', 'Q2.1']);
        verifier(chemins.length >= (m.analyse ? 4 : 8), `${m.nom} : ${chemins.length} références offertes seulement`);
        for (const chemin of chemins) {
          const e = parCle.get(chemin);
          verifier(e, `${m.nom} : {{${m.nom}.${chemin}}} sans entrée`);
          const f = formeReference(m, chemin.split('.'), { format: '' });
          const attendu = f.forme === 'coef' ? { p: f.p, ic: f.bas }[f.quoi] ?? f.est : f.forme === 'coef_cs' ? f.est : f.x;
          const quoi = chemin.split('.');
          const obtenu = quoi[0] === 'p' || (quoi[0] === 'cvanova' && quoi[1] === 'p') ? e.p : quoi[0] === 'ic' ? e.ic[0] : e.valeur;
          verifier(Math.abs(obtenu - attendu) <= 1e-9 * Math.max(1, Math.abs(attendu)), `${m.nom}.${chemin} : ${obtenu} au lieu de ${attendu}`);
        }
      }
      // the summary before and after a refit without five observations, sent back as received
      const avant = await api('resultats_cles', { modele: pls.id }, 'en');
      const re = await api('reajuster', { modele: pls.id, exclues: [3, 4, 5, 6, 7], A: 2 });
      const apres = await api('resultats_cles', { modele: re.id }, 'en');
      verifier(avant.entrees.find((e) => e.cle === 'Q2').libelle === 'Cumulative Q²', 'libellés dans la langue de la demande');
      const c = await api('comparer_resultats', { avant, apres, langue: 'en' });
      const n = c.entrees.find((x) => x.cle === 'N');
      verifier(n && n.avant === 60 && n.apres === 55 && n.change && n.phrase === 'The number of observations goes from 60 to 55.', `N comparé : ${JSON.stringify(n)}`);
      verifier(c.entrees.every((x) => ['change', 'signe', 'seuil', 'ic', 'nouveau', 'disparu', 'categorie'].every((k) => typeof x[k] === 'boolean') && typeof x.importance === 'number'),
        'chaque entrée comparée a ses drapeaux et son importance');
      verifier(c.resume.entrees === c.entrees.length && c.resume.changees > 0 && c.phrases.length === c.resume.phrases
        && c.phrases.every((f) => typeof f.texte === 'string' && Array.isArray(f.cles)), `résumé de la comparaison : ${JSON.stringify(c.resume)}`);
      const q2 = c.entrees.find((x) => x.cle === 'Q2');
      verifier(q2 && Math.abs(q2.ecart - (q2.apres - q2.avant)) < 1e-12, `écart du Q² : ${JSON.stringify(q2)}`);
      const meme = await api('comparer_resultats', { avant, apres: avant });
      verifier(meme.resume.changees === 0 && meme.phrases.length === 0, `un résumé comparé à lui-même : ${JSON.stringify(meme.resume)}`);
      // in French (the request's language, without langue): the Results text's decimal comma
      const fr = await api('comparer_resultats', { avant: await api('resultats_cles', { modele: lm.id }), apres: await api('resultats_cles', { modele: (await api('reajuster', { modele: lm.id, exclues: [8, 9, 10, 11] })).id }) });
      const nfr = fr.entrees.find((x) => x.cle === 'N');
      verifier(nfr.phrase === 'Le nombre d\'observations passe de 60 à 56.' && fr.phrases.every((f) => !/\d\.\d/.test(f.texte)), `phrases en français : ${fr.phrases.map((f) => f.texte).join(' | ')}`);
    });
    await etape('rapports vivants : la source, les étapes, l’aperçu et la mise à jour, une étape retirée, Ctrl+Z (moteur/R/etapes.R)', async () => {
      // six relevés, un nom par ligne ; puis l'année suivante, deux de plus et les colonnes rangées autrement
      const releves = path.join(dossier, 'releves.csv');
      const lignes = (n) => Array.from({ length: n }, (_, i) => [`r${i + 1}`, ['A', 'B'][i % 2], (i * 1.5 + 2).toFixed(1), i + 10]);
      fs.writeFileSync(releves, ['nom,zone,x,y', ...lignes(6).map((l) => l.join(','))].join('\n'));
      const q = await api('ouvrir', { chemin: releves });
      const v = Object.fromEntries(q.variables.map((x) => [x.nom, x.uid]));
      await api('modifier_valeur', { projet: q.id, observation: q.observations.uid[1], variable: v.x, valeur: '40' });
      await api('variable_calculee', { projet: q.id, nom: 'xy', formule: 'x * y' });
      const m = await api('ajuster', { projet: q.id, type: 'LM', x: [v.x], y: [v.y], nom: 'L' });
      let e = await api('etapes', { projet: q.id });
      verifier(e.source && e.source.existe && e.source.modifie === false && e.source.lecteur === 'tableau' && e.source.chemin.endsWith('releves.csv')
        && e.etapes.length === 2 && e.etapes[0].demande === 'modifier_valeur' && e.etapes[1].etiquette === 'variable xy' && e.conformes === true,
      `etapes : ${JSON.stringify(e)}`);
      fs.writeFileSync(releves, ['y,nom,x,zone', ...lignes(8).map((l) => [l[3], l[0], l[2], l[1]].join(','))].join('\n'));
      e = await api('etapes', { projet: q.id });
      verifier(e.source.modifie === true && e.source.actuel.taille !== e.source.taille, `le fichier changé : ${JSON.stringify(e.source)}`);
      const a = await api('mettre_a_jour', { projet: q.id, apercu: true });
      verifier(a.apercu === true && a.observations.ajoutees === 2 && a.observations.retirees === 0 && [].concat(a.observations.premieres_ajoutees).join() === 'r7,r8'
        && a.etapes.length === 2 && a.etapes.every((x) => x.statut === 'ok') && !a.projet && a.modeles.variables_absentes.length === 0,
      `aperçu : ${JSON.stringify(a)}`);
      verifier((await api('projet', { projet: q.id })).observations.uid.length === 6, 'l’aperçu ne change rien');
      const u = await api('mettre_a_jour', { projet: q.id });
      const d = await api('donnees', { projet: q.id });
      const col = (n) => d.valeurs[d.variables.indexOf(n)];
      verifier(u.projet.observations.uid.length === 8 && u.projet.annulable === 'mise à jour des données' && col('x')[1] === 40 && col('xy')[7] === (7 * 1.5 + 2) * 17,
        `mis à jour : ${u.projet.observations.uid.length} observations, x ${JSON.stringify(col('x'))}`);
      const refait = await api('ajuster', { projet: q.id, numero: m.numero });
      verifier(refait.N === 8, `le modèle réajusté : N = ${refait.N}`);
      const r = await api('retirer_etape', { projet: q.id, n: 1 });
      verifier(r.projet.annulable === "retrait de l'étape 1" && (await api('etapes', { projet: q.id })).etapes.length === 1
        && (await api('donnees', { projet: q.id })).valeurs[d.variables.indexOf('x')][1] === 3.5, `étape retirée : ${r.projet.annulable}`);
      await api('annuler', { projet: q.id });
      verifier((await api('etapes', { projet: q.id })).etapes.length === 2, 'Ctrl+Z rend l’étape');
      const f = path.join(dossier, 'vivant.nuee');
      await api('enregistrer', { projet: q.id, chemin: f });
      const lu = JSON.parse(fs.readFileSync(f, 'utf8'));
      const o = await api('ouvrir', { chemin: f });
      const eo = await api('etapes', { projet: o.id });
      verifier(lu.source_donnees.relatif === 'releves.csv' && lu.etapes.length === 2 && typeof lu.etapes[0].parametres === 'string'
        && eo.etapes.length === 2 && eo.conformes === true && eo.source.trouve.endsWith('releves.csv'), `enregistré et rouvert : ${JSON.stringify(eo)}`);
      let refus = '';
      try { await api('retirer_etape', { projet: o.id, n: 9 }); } catch (err) { refus = err.message; }
      verifier(/Étape inconnue/.test(refus), `étape inconnue : « ${refus} »`);
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
