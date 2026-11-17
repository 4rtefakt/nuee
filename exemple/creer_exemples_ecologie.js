// Builds the ecology example projects (Help › Example projects): the skylark counts and line
// transects of the monitoring tutorial, and the trout densities of the BACI tutorial, from the
// tutorials' simulated data (docs/site/donnees, made by suivi.R and baci.R with fixed seeds: no
// bird nor fish was counted). Each project is made through the R server as the application makes
// it, its models fitted as the tutorials fit them, then saved with a session: the models fitted
// and the views opened when it opens, as a project saved by the application has them. In English
// (skylark_counts.json, skylark_transects.json, baci_trout.json) and in French
// (comptages_alouette.json, transects_alouette.json, baci_truite.json), next to this script.
//   node exemple/creer_exemples_ecologie.js [Rscript]
//
// The projects keep their data, not where they were read from: the file each was opened from (its
// source and data steps, moteur/R/etapes.R) would name a folder of this computer.
const { spawn, execFileSync } = require('child_process');
const fs = require('fs');
const net = require('net');
const path = require('path');

const ici = __dirname;
const donnees = path.join(ici, '..', 'docs', 'site', 'donnees');
const lanceur = process.argv[2] || process.env.RSCRIPT
  || (process.platform === 'win32' ? path.join(process.env.LOCALAPPDATA || '', 'Programs', 'R', 'R-4.6.2', 'bin', 'Rscript.exe') : 'Rscript');
const rscript = fs.existsSync(path.join(path.dirname(lanceur), 'x64', 'Rscript.exe')) ? path.join(path.dirname(lanceur), 'x64', 'Rscript.exe') : lanceur;

// each project in each language: its table, its columns, the models' titles and notes
const TEXTES = {
  en: {
    comptages: { fichier: 'skylark_counts', table: 'skylark_counts.csv', site: 'site', habitat: 'habitat', annee: 'year', y: 'skylarks',
      titres: ['Trend (TRIM)', 'Trend, negative binomial GLMM'],
      notes: ['Simulated data (the monitoring tutorial’s, made by its script suivi.R): a decline of 3.5% a year in croplands and 2% in grasslands, 40 sites from 2008 to 2024, some counts missing. TRIM imputes them; its overall slope is in the Summary, the yearly indices in Indices.',
        'The same counts as a negative binomial mixed model: a random intercept per site, the year and the habitat as X. The year’s coefficient is the trend on the log scale: exp(β) is the yearly factor.'] },
    transects: { fichier: 'skylark_transects', table: 'skylark_transects.csv', distance: 'distance', echantillon: 'Sample.Label', effort: 'Effort', aire: 'Area', taille: 'size',
      titre: 'Density (distance sampling)',
      note: 'Simulated data (the monitoring tutorial’s, made by its script suivi.R): 24 line transects over 120 km² in 2024, a density of 25 groups per km². The farthest 5% of the distances are left out; the half-normal key was kept by AIC.' },
    baci: { fichier: 'baci_trout', table: 'baci_trout.csv', y: 'density', annee: 'year', station: 'station',
      note: 'Simulated data (the BACI tutorial’s, made by its script baci.R): a mill weir removed in the winter of 2015-2016, three impact stations upstream (A1 to A3), three controls on a neighbouring stream (T1 to T3), +6 juveniles per 100 m² at the restored stations from 2016 on. The BACI effect is the period × impact cross term.' },
  },
  fr: {
    comptages: { fichier: 'comptages_alouette', table: 'comptages_alouette.csv', site: 'site', habitat: 'habitat', annee: 'annee', y: 'alouettes',
      titres: ['Tendance (TRIM)', 'Tendance, GLMM binomial négatif'],
      notes: ['Données simulées (celles du tutoriel de suivi, faites par son script suivi.R) : un déclin de 3,5 % par an dans les cultures et de 2 % dans les prairies, 40 sites de 2008 à 2024, des comptages manquants. TRIM les impute ; sa pente globale est dans le Résumé, les indices de chaque année dans Indices.',
        'Les mêmes comptages en modèle mixte binomial négatif : une constante aléatoire par site, l’année et l’habitat en X. Le coefficient de l’année est la tendance sur l’échelle log : exp(β) est le facteur annuel.'] },
    transects: { fichier: 'transects_alouette', table: 'transects_alouette.csv', distance: 'distance', echantillon: 'transect', effort: 'longueur', aire: 'aire', taille: 'taille',
      titre: 'Densité (échantillonnage par distances)',
      note: 'Données simulées (celles du tutoriel de suivi, faites par son script suivi.R) : 24 transects linéaires sur 120 km² en 2024, une densité de 25 groupes par km². Les 5 % des distances les plus grandes sont laissés de côté ; l’AIC a gardé la clé demi-normale.' },
    baci: { fichier: 'baci_truite', table: 'baci_truite.csv', y: 'densite', annee: 'annee', station: 'station',
      note: 'Données simulées (celles du tutoriel BACI, faites par son script baci.R) : le seuil d’un moulin arasé pendant l’hiver 2015-2016, trois stations d’impact en amont (A1 à A3), trois témoins sur un affluent voisin (T1 à T3), +6 juvéniles pour 100 m² aux stations restaurées à partir de 2016. L’effet BACI est le terme croisé période × impact.' },
  },
};

function portLibre() {
  return new Promise((resolve) => { const s = net.createServer(); s.listen(0, '127.0.0.1', () => { const { port } = s.address(); s.close(() => resolve(port)); }); });
}

// A session as the application saves it (interface/app.js: collecterSession): the models by their
// number in the project, then the views to open, a model's written '@<number>'; no layout, so
// that each model's page opens as for a new model (restaurerSession).
const session = (modeles, vues) => JSON.stringify({ version: 2, modeles: modeles.map((m) => ({ numero: m.numero, A: m.A })), vues,
  disposition: null, selection: {}, variables: {}, choixVIP: {}, groupesHCA: {} });

async function principal() {
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
  try {
    for (const langue of ['en', 'fr']) {
      const api = async (nom, q = {}) => {
        const r = await fetch(`http://127.0.0.1:${port}/api/${nom}`, { method: 'POST', body: JSON.stringify(q), headers: { Cookie: `nuee_${port}=${jeton}`, 'X-Langue': langue } });
        const j = await r.json();
        if (!r.ok) throw new Error(`${nom} : ${j.erreur}`);
        return j;
      };
      const ouvrir = async (table) => {
        const p = await api('ouvrir', { chemin: path.join(donnees, table) });
        return { p, uid: Object.fromEntries(p.variables.map((v) => [v.nom, v.uid])) };
      };
      const enregistrer = async (p, fichier, s) => {
        const chemin = path.join(ici, `${fichier}.json`);
        await api('enregistrer', { projet: p.id, chemin, session: s });
        // (its data only: where they were read from names a folder of this computer)
        const j = JSON.parse(fs.readFileSync(chemin, 'utf8'));
        for (const k of ['source_donnees', 'etapes', 'empreinte_donnees']) delete j[k];
        fs.writeFileSync(chemin, JSON.stringify(j));
        console.log('Written:', chemin);
      };
      const T = TEXTES[langue];

      // the counts: TRIM, as Which analysis? fills it in (model 3, overdispersion and serial
      // correlation), then the negative binomial mixed model it offers beside it
      {
        const c = T.comptages;
        const { p, uid } = await ouvrir(c.table);
        const trim = await api('ajuster', { projet: p.id, type: 'TRIM', x: [], y: [uid[c.y]], echelle: 'none', nom: 'M1', titre: c.titres[0],
          options: { site: { by: 'variable', variable: uid[c.site] }, temps: { by: 'variable', variable: uid[c.annee] }, modele: 3, changements: [], pas_a_pas: false,
            surdispersion: true, correlation: true, poids: null, base: null } });
        await api('note_modele', { modele: trim.id, note: c.notes[0] });
        const glmm = await api('ajuster', { projet: p.id, type: 'GLM', x: [uid[c.annee], uid[c.habitat]], y: [uid[c.y]], echelle: 'none', nom: 'M2', titre: c.titres[1],
          famille: 'binomiale_negative', aleatoires: [{ by: 'variable', variable: uid[c.site] }] });
        await api('note_modele', { modele: glmm.id, note: c.notes[1] });
        console.log(`${c.fichier}: ${trim.nom} ${trim.type} N = ${trim.N}, ${trim.chiffres}; ${glmm.nom} ${glmm.type} N = ${glmm.N}, AIC ${glmm.qualite.AIC.toFixed(1)}`);
        await enregistrer(p, c.fichier, session([trim, glmm], [{ vue: 'resume', modele: `@${glmm.numero}`, loc: {} },
          { vue: 'trim_indices', modele: `@${trim.numero}`, loc: { droite: true } }]));
      }
      // the line transects: distance sampling, its columns as their names propose them (one region:
      // the first column, the same for every row, names the observations), the farthest 5% of the
      // distances left out, the half-normal key and cosine terms chosen by AIC
      {
        const c = T.transects;
        const { p, uid } = await ouvrir(c.table);
        const d = await api('ajuster', { projet: p.id, type: 'DISTANCE', x: [], y: [uid[c.distance]], echelle: 'none', nom: 'M1', titre: c.titre,
          options: { transect: 'ligne', echantillon: { by: 'variable', variable: uid[c.echantillon] }, strate: null, effort: uid[c.effort],
            aire: uid[c.aire], taille: uid[c.taille], z: [], troncature: { type: 'pourcentage', valeur: 5 }, gauche: 0, cle: 'hn', serie: 'cos', ajustements: 'aic',
            echelle_ajustements: 'largeur', monotonie: 'stricte', unites: { distance: 'm', effort: 'km', aire: 'km2' }, facteur: null, intervalles: null } });
        await api('note_modele', { modele: d.id, note: c.note });
        console.log(`${c.fichier}: ${d.nom} ${d.type} N = ${d.N}, ${d.chiffres}`);
        await enregistrer(p, c.fichier, session([d], [{ vue: 'resume', modele: `@${d.numero}`, loc: {} }]));
      }
      // the trout: Statistical tests in their BACI mode, A1 to A3 the impact, the others the
      // controls, the event in 2016, then Fit the mixed model (CAR1 in time): its two 0/1 variables
      // join the project; the BACI view kept in the session, in front
      {
        const c = T.baci;
        const { p, uid } = await ouvrir(c.table);
        const baci = { y: uid[c.y], temps: uid[c.annee], unite: uid[c.station], impact: ['A1', 'A2', 'A3'], temoins: ['T1', 'T2', 'T3'], test: 'welch', evenement: '2016', fin: null };
        const r = await api('baci_modele', { projet: p.id, ...baci, car1: true });
        const m = r.modele;
        await api('note_modele', { modele: m.id, note: c.note });
        console.log(`${c.fichier}: ${m.nom} ${m.type} N = ${m.N}, AIC ${m.qualite.AIC.toFixed(1)}`);
        await enregistrer(p, c.fichier, session([m], [{ vue: 'resume', modele: `@${m.numero}`, loc: {} },
          { vue: 'tests', modele: null, loc: { mode: 'baci', baci } }]));
      }
    }
  } finally {
    try { execFileSync('taskkill', ['/pid', String(serveur.pid), '/T', '/F'], { stdio: 'ignore' }); } catch { serveur.kill(); }
  }
  process.exit(0);
}

principal().catch((e) => { console.error(e); process.exit(1); });
