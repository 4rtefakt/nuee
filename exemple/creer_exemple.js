// Builds the example project (Help › Example project): 120 apples, entirely made up by this
// script from a seeded random generator. Writes orchards.csv (the table, as one would import
// it) and orchards.json (the project with four models, made through the R server like in the
// application), in English; with --langue fr, vergers.csv and vergers.json, the same apples in
// French (the French interface opens them, and the engine's checks use them).
//   node exemple/creer_exemple.js [Rscript] [--langue fr]
//
// How the values are made: each apple has a variety (four), a growing method (organic or
// conventional), an orchard, and three hidden factors: ripeness, size and days in storage.
// Each measure is a mix of the variety's typical value and these factors, plus noise; the
// tasting score follows sugar, acidity and firmness. So the PCA separates the varieties,
// the PLS predicts the score, and the OPLS-DA finds what tells organic apples apart
// (polyphenols, nitrogen, calcium).
const { spawn, execFileSync } = require('child_process');
const fs = require('fs');
const net = require('net');
const path = require('path');

const ici = __dirname;
const args = process.argv.slice(2);
const iLangue = args.indexOf('--langue');
const LANGUE = iLangue >= 0 ? args.splice(iLangue, 2)[1] : 'en';
// the same apples in each language: the file, its columns, the growing methods, the models' titles and notes
const TEXTES = {
  en: { fichier: 'orchards', bio: 'organic', conventionnel: 'conventional',
    colonnes: ['fruit', 'variety', 'farming', 'orchard', 'weight', 'diameter', 'firmness', 'sugar', 'acidity', 'starch',
      'dry_matter', 'polyphenols', 'colour_a', 'colour_b', 'juice', 'calcium', 'nitrogen', 'storage', 'taste_score'],
    titres: ['Overview', 'Taste score', 'Varieties', 'Organic or conventional'],
    notes: ['Made-up data. Colour the scores by “variety”: the four varieties separate on t1 and t2.',
      'What makes the score: sugar, firmness, acidity (see the VIP and the coefficients).', null,
      'S-plot: the variables that best separate the two farming methods are at the ends.'] },
  fr: { fichier: 'vergers', bio: 'bio', conventionnel: 'conventionnel',
    colonnes: ['fruit', 'variete', 'culture', 'verger', 'poids', 'diametre', 'fermete', 'sucre', 'acidite', 'amidon',
      'mat_seche', 'polyphenols', 'couleur_a', 'couleur_b', 'jus', 'calcium', 'azote', 'stockage', 'note_gout'],
    titres: ['Vue d’ensemble', 'Note de dégustation', 'Variétés', 'Bio ou conventionnel'],
    notes: ['Données inventées. Colorer les scores par « variete » : les quatre variétés se séparent sur t1 et t2.',
      'Ce qui fait la note : sucre, fermeté, acidité (voir les VIP et les coefficients).', null,
      'Graphique S : les variables qui séparent le mieux les deux cultures sont aux extrémités.'] },
};
const T = TEXTES[LANGUE];
if (!T) throw new Error(`unknown language: ${LANGUE}`);
const lanceur = args[0] || process.env.RSCRIPT
  || (process.platform === 'win32' ? path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe') : 'Rscript');
const rscript = fs.existsSync(path.join(path.dirname(lanceur), 'x64', 'Rscript.exe')) ? path.join(path.dirname(lanceur), 'x64', 'Rscript.exe') : lanceur;

// ---------------------------------------------------------------- the made-up table

let graine = 20261101;
const uniforme = () => { graine = (graine * 16807) % 2147483647; return graine / 2147483647; };
const normale = () => Math.sqrt(-2 * Math.log(uniforme())) * Math.cos(2 * Math.PI * uniforme());
const arrondi = (x, n) => Number(x.toFixed(n));

const VARIETES = {
  //          poids fermeté sucre acidité mat. sèche polyphénols a*  b*  amidon jus
  Gala:     [145, 7.2, 14.2, 3.8, 15.2, 850, 20, 24, 1.0, 64],
  Golden:   [180, 5.6, 14.6, 4.8, 14.6, 650, -4, 40, 1.5, 66],
  Granny:   [200, 8.9, 10.5, 9.8, 13.8, 1100, -20, 30, -1.2, 61],
  Reinette: [160, 7.0, 13.0, 7.8, 17.6, 1600, 5, 33, 0.0, 58],
};

function tableau() {
  const lignes = [T.colonnes];
  let n = 0;
  for (const [variete, [poids0, ferm0, sucre0, acid0, ms0, poly0, a0, b0, amidon0, jus0]] of Object.entries(VARIETES)) {
    for (let i = 0; i < 30; i += 1) {
      n += 1;
      const bio = i % 2 === 0;
      const o = bio ? 0.5 : -0.5;              // organic
      const m = normale();                     // ripeness
      const s = normale();                     // size
      const stockage = Math.floor(uniforme() * 121);
      const poids = poids0 + 12 * s - 8 * o + 6 * normale();
      const diametre = 45 + 0.12 * poids + 1.2 * normale();
      const fermete = ferm0 - 0.3 * m - 0.006 * stockage + 0.3 * normale();
      const sucre = sucre0 + 0.5 * m + 0.35 * normale();
      const acidite = Math.max(1.5, acid0 - 0.3 * m - 0.005 * stockage + 0.3 * normale());
      const amidon = Math.min(10, Math.max(1, 5 + amidon0 + 1.0 * m + 0.5 * normale()));
      const matSeche = ms0 + 0.2 * m - 0.25 * s + 0.3 * normale();
      const poly = poly0 * Math.exp(0.32 * o + 0.07 * normale());
      const couleurA = a0 + 1.5 * m + 1.6 * normale();
      const couleurB = b0 + 1.6 * normale();
      const jus = jus0 + 1.0 * s - 0.5 * (matSeche - 15) + 1.0 * normale();
      const calcium = 46 + 9 * o - 3 * s + 3.5 * normale();
      const azote = 520 - 110 * o + 25 * s + 28 * normale();
      const note = Math.min(10, Math.max(0, 5.2 + 0.7 * (sucre - 13) + 0.45 * (fermete - 7)
        - 0.07 * (acidite - 6) ** 2 + 0.0008 * (poly - 1000) + 0.3 * normale()));
      const ligne = [`P${String(n).padStart(3, '0')}`, variete, bio ? T.bio : T.conventionnel, 'ABC'[Math.floor(uniforme() * 3)],
        arrondi(poids, 0), arrondi(diametre, 1), arrondi(fermete, 2), arrondi(sucre, 1), arrondi(acidite, 2), arrondi(amidon, 1),
        arrondi(matSeche, 1), arrondi(poly, 0), arrondi(couleurA, 1), arrondi(couleurB, 1), arrondi(jus, 1), arrondi(calcium, 1),
        arrondi(azote, 0), stockage, arrondi(note, 1)];
      // a few measures missing, as in real data
      if (uniforme() < 0.04) ligne[11] = '';
      if (uniforme() < 0.03) ligne[15] = '';
      lignes.push(ligne);
    }
  }
  return lignes.map((l) => l.join(',')).join('\r\n') + '\r\n';
}

// ---------------------------------------------------------------- the project

function portLibre() {
  return new Promise((resolve) => { const s = net.createServer(); s.listen(0, '127.0.0.1', () => { const { port } = s.address(); s.close(() => resolve(port)); }); });
}

async function principal() {
  const csv = path.join(ici, `${T.fichier}.csv`);
  fs.writeFileSync(csv, tableau());

  const port = await portLibre();
  const jeton = require('crypto').randomBytes(32).toString('hex');  // as the application does (main.js)
  const serveur = spawn(rscript, [path.join(ici, '..', 'moteur', 'serveur.R'), String(port), path.join(ici, '..', 'app', 'interface')],
    { windowsHide: true, env: { ...process.env, NUEE_JETON: jeton } });
  await new Promise((resolve, reject) => {
    let sortie = '';
    serveur.stdout.on('data', (d) => { sortie += d; if (sortie.includes('PRET')) resolve(); });
    serveur.stderr.on('data', (d) => { sortie += d; });
    serveur.on('exit', (code) => reject(new Error(`R s'est arrêté (code ${code}) : ${sortie.slice(-800)}`)));
  });
  const api = async (nom, q = {}) => {
    const r = await fetch(`http://127.0.0.1:${port}/api/${nom}`, { method: 'POST', body: JSON.stringify(q), headers: { Cookie: `nuee_${port}=${jeton}`, 'X-Langue': LANGUE } });
    const j = await r.json();
    if (!r.ok) throw new Error(`${nom} : ${j.erreur}`);
    return j;
  };
  try {
    const p = await api('ouvrir', { chemin: csv });
    const uid = Object.fromEntries(p.variables.map((v) => [v.nom, v.uid]));
    const c = T.colonnes;  // fruit, variety, farming, orchard, the 14 measures, the tasting score
    const x = c.slice(4, 18).map((n) => uid[n]);
    const modeles = [
      { type: 'PCA', x, A: 0, nom: 'M1', titre: T.titres[0], note: T.notes[0] },
      { type: 'PLS', x, y: [uid[c[18]]], A: 0, nom: 'M2', titre: T.titres[1], note: T.notes[1] },
      { type: 'PLS-DA', x, classe: uid[c[1]], A: 3, nom: 'M3', titre: T.titres[2] },
      { type: 'OPLS-DA', x, classe: uid[c[2]], A: 2, nom: 'M4', titre: T.titres[3], note: T.notes[3] },
    ];
    for (const { note, ...q } of modeles) {
      const m = await api('ajuster', { projet: p.id, ...q });
      if (note) await api('note_modele', { modele: m.id, note });
      console.log(`${q.nom} ${m.type} A=${m.A} R2X=${m.composantes.R2Xcum[m.A - 1].toFixed(2)} Q2=${m.composantes.Q2cum[m.A - 1].toFixed(2)}`);
    }
    await api('enregistrer', { projet: p.id, chemin: path.join(ici, `${T.fichier}.json`) });
    console.log('Written:', path.join(ici, `${T.fichier}.json`));
  } finally {
    try { execFileSync('taskkill', ['/pid', String(serveur.pid), '/T', '/F'], { stdio: 'ignore' }); } catch { serveur.kill(); }
  }
  process.exit(0);
}

principal().catch((e) => { console.error(e); process.exit(1); });
