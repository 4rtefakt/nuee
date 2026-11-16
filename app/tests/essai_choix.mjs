// Checks interface/choix_analyse.js (Model › Which analysis?), without the application: the
// facts read from a project and its variable statistics, the answers proposed, the analysis each
// set of answers gives, and the settings New model or a view opens with.
//   node tests/essai_choix.mjs
import { QUESTIONS, BUTS, REPONSES, FORMULAIRE, formulaire, CATALOGUE, REGLES, NOTES, TEXTES_CHOIX, faitsVariables, genre, beaucoupDeZeros,
  proposer, choisirReponse, contexte, recommander, construire, notesPour } from '../interface/choix_analyse.js';

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);

// A project summary as the engine sends it (moteur/serveur.R: resume_projet) and its variable
// statistics (moteur/R/diagnostic.R: statistiques_variables), from a short description: each
// variable [name, categories or null, { min, max, moyenne, ecart, entiers, zeros, asymetrie, manquantes, suggestion }].
function projet(n, variables, autres = {}) {
  const vars = variables.map(([nom, cats], i) => ({ uid: i + 1, nom, qualitative: !!cats, categories: cats || [], manquantes: 0, temps: false }));
  const col = (k, d = null) => variables.map(([, cats, s]) => (cats ? d : (s || {})[k] ?? d));
  const stats = { uid: vars.map((v) => v.uid), nom: vars.map((v) => v.nom), qualitative: vars.map((v) => v.qualitative), categories: vars.map((v) => v.categories.length),
    N: vars.map(() => n), manquantes: col('manquantes', 0), min: col('min'), max: col('max'), moyenne: col('moyenne'), ecart_type: col('ecart'),
    test_asymetrie: col('asymetrie', 0), suggestion: col('suggestion'), entiers: col('entiers'), zeros: col('zeros') };
  return { p: { id: 'P1', nom: 'essai', observations: { uid: Array.from({ length: n }, (_, i) => i + 1) }, variables: vars, identifiants: [], coordonnees: null, ...autres }, stats };
}
const uid = (p, nom) => p.variables.find((v) => v.nom === nom).uid;
// the answers proposed, then the question and the response chosen, as the dialog does
function repondre(p, F, changements, infos = null) {
  const R = proposer(p, F, infos);
  const { y, ...reste } = changements;
  Object.assign(R, reste);
  if (y !== undefined) choisirReponse(R, F, uid(p, y));
  return R;
}
const conseil = (p, F, R) => recommander(contexte(R, F, p));

console.log('La table elle-même');
{
  const ids = Object.keys(CATALOGUE);
  const inconnues = REGLES.flatMap((r) => [r.choix, ...r.autres]).filter((id) => !CATALOGUE[id]);
  egal('chaque règle ne nomme que des analyses du catalogue', inconnues, []);
  const sansTexte = ids.flatMap((id) => ['qa_nom_', 'qa_pourquoi_', 'qa_conditions_'].flatMap((k) => ['fr', 'en'].filter((l) => !TEXTES_CHOIX[l][k + id]).map((l) => `${l} ${k}${id}`)));
  egal('chaque analyse a son nom, son pourquoi et ses conditions en français et en anglais', sansTexte, []);
  const fr = Object.keys(TEXTES_CHOIX.fr);
  const en = Object.keys(TEXTES_CHOIX.en);
  egal('les mêmes clés dans les deux langues', [fr.filter((k) => !en.includes(k)), en.filter((k) => !fr.includes(k))], [[], []]);
  const cles = [...QUESTIONS.flatMap((q) => [`qa_q_${q}`, `qa_ex_${q}`]), ...BUTS.map((b) => `qa_but_${b}`), ...REPONSES.flatMap((r) => [`qa_r_${r}`, `qa_rex_${r}`]), ...NOTES.map((n) => n.cle)];
  egal('les questions, les réponses et les notes ont leurs textes', cles.filter((k) => TEXTES_CHOIX.fr[k] === undefined || TEXTES_CHOIX.en[k] === undefined), []);
  egal('chaque question (et chaque but des espèces) a son formulaire', [...QUESTIONS.filter((q) => q !== 'especes'), ...BUTS.map((b) => `especes/${b}`)].filter((k) => !FORMULAIRE[k]), []);
  // every kind of response a question offers leads to an analysis
  const sansRegle = [];
  for (const q of QUESTIONS) for (const but of q === 'especes' ? BUTS : [null]) {
    for (const reponse of formulaire(q, but).reponses) {
      const c = { question: q, but, reponse };
      if (!recommander(c)) sansRegle.push(`${q}${but ? `/${but}` : ''} ${reponse}`);
    }
  }
  egal('chaque question et chaque réponse qu’elle propose mènent à une analyse', sansRegle, []);
  const texte = JSON.stringify(TEXTES_CHOIX) + JSON.stringify(REGLES);
  verifier('aucun autre logiciel nommé', !/simca/i.test(texte));
}

console.log('Ce qu’on lit des variables');
{
  egal('genre : classe, présence, proportion, comptage, mesure', [
    genre({ qualitative: true }), genre({ min: 0, max: 1, entiers: true }), genre({ min: 0.02, max: 0.9, entiers: false }),
    genre({ min: 0, max: 87, entiers: true }), genre({ min: -2.1, max: 5.3, entiers: false }), genre({ min: null, max: null })],
  ['classe', 'presence', 'proportion', 'comptage', 'mesure', 'mesure']);
  verifier('beaucoup de zéros : 45 % pour une moyenne de 3 (une loi de Poisson en donnerait 5 %)', beaucoupDeZeros({ zeros: 45, moyenne: 3 }));
  verifier('pas beaucoup de zéros : 40 % pour une moyenne de 0,8 (une loi de Poisson en donnerait 45 %)', !beaucoupDeZeros({ zeros: 40, moyenne: 0.8 }));
  // R sends the columns of a table of one variable as scalars
  const F = faitsVariables({ variables: [{ uid: 4, nom: 'x', qualitative: false, categories: [] }] }, { uid: 4, nom: 'x', min: 0, max: 3, entiers: true, zeros: 10, moyenne: 1.2 });
  egal('une seule variable : ses statistiques en scalaires', [F.get(4).min, F.get(4).max, F.get(4).entiers], [0, 3, true]);
  const G = faitsVariables({ variables: [{ uid: 1, nom: 'y', qualitative: false, categories: [] }] }, null);
  egal('sans statistiques : une mesure', genre(G.get(1)), 'mesure');
  // whole numbers that are a measurement rounded to its unit, not counts: by the unit or the name
  egal('des entiers en g ou en °C, une moyenne, une biomasse : des mesures ; en ind., une abondance : des comptages', [
    genre({ min: 0, max: 640, entiers: true, unite: 'g' }), genre({ min: 0, max: 30, entiers: true, unite: '°C' }),
    genre({ min: 0, max: 12, entiers: true, nom: 'nids_moyenne' }), genre({ min: 0, max: 900, entiers: true, nom: 'biomass' }),
    genre({ min: 0, max: 87, entiers: true, unite: 'ind.' }), genre({ min: 0, max: 87, entiers: true, nom: 'abondance' })],
  ['mesure', 'mesure', 'mesure', 'mesure', 'comptage', 'comptage']);
}

console.log('Des entiers : des comptages, ou une mesure arrondie à son unité');
{
  const { p, stats } = projet(40, [['traitement', ['A', 'B']], ['pente', null, { min: 1.5, max: 30, moyenne: 12, ecart: 6, entiers: false, zeros: 0 }],
    ['y', null, { min: 0, max: 640, moyenne: 210, ecart: 120, entiers: true, zeros: 3 }],
    ['masse', null, { min: 3, max: 640, moyenne: 210, ecart: 120, entiers: true, zeros: 0 }],
    ['z', null, { min: 3, max: 640, moyenne: 210, ecart: 120, entiers: true, zeros: 0 }]]);
  p.variables.find((v) => v.nom === 'z').unite = 'g/m²';
  const F = faitsVariables(p, stats);
  egal('en g/m² (Unités et descriptions) ou nommée « masse » : une mesure', [genre(F.get(uid(p, 'z'))), genre(F.get(uid(p, 'masse')))], ['mesure', 'mesure']);
  const Z = repondre(p, F, { question: 'effet', y: 'z' });
  egal('une mesure en g/m² → le modèle linéaire, pas un GLM de comptages', [Z.reponse, conseil(p, F, Z).choix], ['mesure', 'normal']);
  const Y = repondre(p, F, { question: 'effet', y: 'y' });
  const r = conseil(p, F, Y);
  egal('des entiers sans unité ni nom qui le dise : des comptages, le modèle linéaire proposé aussi, et la note qui le dit',
    [Y.reponse, r.choix, r.autres.includes('normal'), notesPour(r.choix, contexte(Y, F, p)).includes('qa_note_comptage_incertain')], ['comptage', 'glm_nb', true, true]);
}

// Counts with many zeros, sites followed over the years
const comptages = projet(96, [
  ['site', ['S1', 'S2', 'S3', 'S4', 'S5', 'S6']],
  ['annee', null, { min: 2015, max: 2022, moyenne: 2018.5, ecart: 2.3, entiers: true, zeros: 0 }],
  ['habitat', ['dune', 'prairie', 'roselière']],
  ['profondeur', null, { min: 0.2, max: 3.5, moyenne: 1.4, ecart: 0.8, entiers: false, zeros: 0 }],
  ['temperature', null, { min: 8.1, max: 19.6, moyenne: 14, ecart: 2.5, entiers: false, zeros: 0 }],
  ['abondance', null, { min: 0, max: 87, moyenne: 3.1, ecart: 9.4, entiers: true, zeros: 45, asymetrie: 9 }],
]);
console.log('Des comptages avec beaucoup de zéros, des sites suivis plusieurs années');
{
  const { p, stats } = comptages;
  const F = faitsVariables(p, stats);
  const R0 = proposer(p, F);
  egal('proposés : le site, le temps, les X sans le site ni le temps', [R0.site, R0.temps, R0.repetes, R0.x], [`v:${uid(p, 'site')}`, `v:${uid(p, 'annee')}`, true,
    ['habitat', 'profondeur', 'temperature', 'abondance'].map((n) => uid(p, n))]);
  egal('pas d’espèces dans un tableau de mesures', R0.especes, []);
  const R = repondre(p, F, { question: 'effet', y: 'abondance' });
  egal('la réponse : un comptage, beaucoup de zéros', [R.reponse, R.zeros, R.x.includes(uid(p, 'abondance'))], ['comptage', true, false]);
  verifier('une abondance : des comptages sans doute, sans le modèle linéaire en plus', !conseil(p, F, R).autres.includes('normal'));
  const r = conseil(p, F, R);
  egal('effet sur un comptage à zéros → GLM binomial négatif, la Poisson à excès de zéros d’abord parmi les autres', [r.choix, r.autres[0]], ['glm_nb', 'glm_zip']);
  const m = construire(r.choix, R, F, p);
  const site = { by: 'variable', variable: uid(p, 'site') };
  egal('Nouveau modèle : GLM, binomiale négative, Y, X, effet aléatoire et validation croisée par site', m.reglages,
    { type: 'GLM', y: [uid(p, 'abondance')], x: ['habitat', 'profondeur', 'temperature'].map((n) => uid(p, n)), famille: 'binomiale_negative', aleatoires: [site], vc: site });
  egal('rien à compléter', m.manque, []);
  verifier('la note de l’effet aléatoire par site', notesPour('glm_nb', contexte(R, F, p)).includes('qa_note_site'));
  const sans = construire('glm_nb', { ...R, repetes: false }, F, p);
  verifier('sans sites répétés : ni effet aléatoire ni validation par site', !sans.reglages.aleatoires && !sans.reglages.vc);
  const zip = construire('glm_zip', R, F, p);
  egal('l’autre : Poisson à excès de zéros', [zip.reglages.type, zip.reglages.famille], ['GLM', 'poisson_zi']);

  const T = repondre(p, F, { question: 'tendance', y: 'abondance' });
  const rt = conseil(p, F, T);
  egal('tendance de comptages, deux placettes par site et par année → GLM binomial négatif, TRIM après un résumé par site et année',
    [rt.choix, rt.autres.includes('trim'), notesPour('trim', contexte(T, F, p)).includes('qa_note_trim_resumer')], ['glm_nb', true, true]);
  egal('TRIM : Y, le site et le temps', construire('trim', T, F, p).reglages,
    { type: 'TRIM', y: [uid(p, 'abondance')], options: { site, temps: { by: 'variable', variable: uid(p, 'annee') } } });
  // one count per site and year
  const parAn = projet(48, comptages.p.variables.map((v) => [v.nom, v.qualitative ? v.categories : null,
    Object.fromEntries(['min', 'max', 'moyenne', 'ecart', 'entiers', 'zeros'].map((k) => [k, comptages.stats[k === 'ecart' ? 'ecart_type' : k][v.uid - 1]]))]));
  const Fp = faitsVariables(parAn.p, parAn.stats);
  const Tp = repondre(parAn.p, Fp, { question: 'tendance', y: 'abondance' });
  egal('une ligne par site et par année → TRIM, sans la note', [conseil(parAn.p, Fp, Tp).choix, notesPour('trim', contexte(Tp, Fp, parAn.p))], ['trim', []]);
  const glmT = construire('glm_nb', T, F, p);
  egal('l’autre, un GLM : l’année en X, l’effet aléatoire par site', [glmT.reglages.x, glmT.reglages.aleatoires], [[uid(p, 'annee')], [site]]);
  const serie = construire('series', T, F, p);
  egal('Séries : variable, temps, une ligne par site, tendances', serie.loc, { variable: uid(p, 'abondance'), temps: uid(p, 'annee'), lignes: uid(p, 'site'), tendances: true });

  const G = repondre(p, F, { question: 'groupes', y: 'abondance', repetes: false });
  const rg = conseil(p, F, G);
  egal('comparer des groupes → GLM binomial négatif sur l’habitat', [rg.choix, construire(rg.choix, G, F, p).reglages.x], ['glm_nb', [uid(p, 'habitat')]]);
  verifier('puis les moyennes ajustées', notesPour('glm_nb', contexte(G, F, p)).includes('qa_note_moyennes'));
  egal('le test des deux variables : l’habitat et l’abondance', construire('tests_deux', G, F, p).loc, { mode: 'deux', deux: { x: uid(p, 'habitat'), y: uid(p, 'abondance') } });

  const E = repondre(p, F, { question: 'espace', y: 'abondance' });
  const re = conseil(p, F, E);
  egal('où, sans coordonnées → la carte, colorée par la réponse', [re.choix, construire('carte', E, F, p).loc], ['carte', { couleur: `v${uid(p, 'abondance')}` }]);
  egal('l’autocorrélation spatiale demande les coordonnées', construire('autocorrelation', E, F, p).manque, ['coordonnees']);
}

// A species table: habitats, a depth, eight species counted
const especes = projet(40, [
  ['habitat', ['herbier', 'sable', 'roche']],
  ['profondeur', null, { min: 1, max: 12, moyenne: 5, ecart: 3, entiers: false, zeros: 0 }],
  ...['Ablette', 'Brochet', 'Gardon', 'Perche', 'Rotengle', 'Sandre', 'Tanche', 'Vandoise'].map((n, i) => [n, null, { min: 0, max: 20 + i, moyenne: 3, ecart: 4, entiers: true, zeros: 30 + i }]),
]);
console.log('Un tableau d’espèces');
{
  const { p, stats } = especes;
  const F = faitsVariables(p, stats);
  const sp = ['Ablette', 'Brochet', 'Gardon', 'Perche', 'Rotengle', 'Sandre', 'Tanche', 'Vandoise'].map((n) => uid(p, n));
  const R0 = proposer(p, F);
  egal('proposées : les huit espèces, et en X l’habitat et la profondeur', [R0.especes, R0.x], [sp, [uid(p, 'habitat'), uid(p, 'profondeur')]]);
  egal('proposés : les groupes, l’habitat', R0.groupe, uid(p, 'habitat'));
  const C = repondre(p, F, { question: 'especes', but: 'composition', reponse: 'especes', x: [uid(p, 'profondeur')] });
  const rc = conseil(p, F, C);
  egal('composition selon une variable quantitative → RDA', rc.choix, 'rda');
  egal('RDA : les espèces en Y, la profondeur en X, Hellinger', construire('rda', C, F, p).reglages,
    { type: 'RDA', y: sp, x: [uid(p, 'profondeur')], options: { transformation: 'hellinger' } });
  const Q = repondre(p, F, { question: 'especes', but: 'composition', reponse: 'especes', x: [uid(p, 'habitat')] });
  egal('composition entre habitats → PERMANOVA, l’habitat en X', [conseil(p, F, Q).choix, construire('permanova', Q, F, p).reglages.x], ['permanova', [uid(p, 'habitat')]]);
  const V = repondre(p, F, { question: 'especes', but: 'composition', reponse: 'especes', x: [] });
  egal('sans X → NMDS', conseil(p, F, V).choix, 'nmds');
  const I = repondre(p, F, { question: 'especes', but: 'indicatrices', reponse: 'especes' });
  egal('espèces indicatrices des habitats → IndVal, l’habitat en classe', [conseil(p, F, I).choix, construire('indval', I, F, p).reglages],
    ['indval', { type: 'INDVAL', y: sp, classe: uid(p, 'habitat') }]);
  const M = repondre(p, F, { question: 'especes', but: 'par_espece', reponse: 'especes' });
  const pm = construire(conseil(p, F, M).choix, M, F, p);
  egal('un modèle par espèce : la première espèce en Y, binomiale négative, les autres ensuite', [pm.ouvre, pm.reglages.type, pm.reglages.famille, pm.reglages.y, pm.especes],
    ['par_reponse', 'GLM', 'binomiale_negative', [sp[0]], sp]);
  const D = repondre(p, F, { question: 'diversite', reponse: 'especes' });
  const rd = construire(conseil(p, F, D).choix, D, F, p);
  egal('diversité → raréfaction des comptages entiers, par habitat', [rd.vue, rd.loc], ['rarefaction', { especes: sp, mode: 'rarefaction', groupe: String(uid(p, 'habitat')) }]);
  egal('l’autre : la richesse en nouvelle variable, une plage dans l’ordre du tableau', construire('indices', D, F, p, undefined, { langue: 'fr' }).variable,
    { nom: 'richesse', formule: 'richesse(Ablette:Vandoise)' });
  egal('explorer un tableau d’espèces → NMDS', conseil(p, F, repondre(p, F, { question: 'explorer', reponse: 'especes' })).choix, 'nmds');
  const G = repondre(p, F, { question: 'groupes', reponse: 'especes' });
  egal('comparer les habitats sur les espèces → PERMANOVA', [conseil(p, F, G).choix, construire('permanova', G, F, p).reglages.x], ['permanova', [uid(p, 'habitat')]]);
}

// Measurements with sites: a length, a skewed biomass, a density with zeros, a cover, a presence, a state
const mesures = projet(60, [
  ['site', ['A', 'B', 'C', 'D', 'E']],
  ['annee', null, { min: 2010, max: 2021, moyenne: 2015.5, ecart: 3.5, entiers: true, zeros: 0 }],
  ['traitement', ['fauche', 'pâturage']],
  ['longueur', null, { min: 12.5, max: 30.2, moyenne: 21, ecart: 3.9, entiers: false, zeros: 0, asymetrie: 0.4 }],
  ['biomasse', null, { min: 0.3, max: 48, moyenne: 6, ecart: 7.5, entiers: false, zeros: 0, asymetrie: 5.1, suggestion: 'log10' }],
  ['densite', null, { min: 0, max: 14.2, moyenne: 2.2, ecart: 3, entiers: false, zeros: 28, asymetrie: 4 }],
  ['couverture', null, { min: 0.04, max: 0.93, moyenne: 0.4, ecart: 0.2, entiers: false, zeros: 0 }],
  ['presence', null, { min: 0, max: 1, moyenne: 0.4, ecart: 0.5, entiers: true, zeros: 60 }],
  ['etat', ['bon', 'mauvais']],
]);
console.log('Des mesures sur des sites');
{
  const { p, stats } = mesures;
  const F = faitsVariables(p, stats);
  const site = { by: 'variable', variable: uid(p, 'site') };
  const L = repondre(p, F, { question: 'effet', y: 'longueur' });
  const rl = conseil(p, F, L);
  const ml = construire(rl.choix, L, F, p);
  egal('une mesure, des sites répétés → modèle linéaire mixte, effet aléatoire et validation par site', [rl.choix, ml.reglages.type, ml.reglages.aleatoires, ml.reglages.vc, ml.reglages.famille],
    ['normal', 'LMM', [site], site, undefined]);
  egal('mesure positive asymétrique → GLM gamma', conseil(p, F, repondre(p, F, { question: 'effet', y: 'biomasse' })).choix, 'glm_gamma');
  egal('le log de la réponse, celui que proposent les statistiques', construire('lm_log', repondre(p, F, { question: 'effet', y: 'biomasse' }), F, p).reglages.transformations,
    { [uid(p, 'biomasse')]: 'log10' });
  egal('mesure avec des zéros → GLM de Tweedie', conseil(p, F, repondre(p, F, { question: 'effet', y: 'densite' })).choix, 'glm_tweedie');
  const Cv = repondre(p, F, { question: 'effet', y: 'couverture' });
  egal('proportion → régression bêta, sans note des bornes', [Cv.reponse, conseil(p, F, Cv).choix, notesPour('glm_beta', contexte(Cv, F, p)).includes('qa_note_bornes01')],
    ['proportion', 'glm_beta', false]);
  const Pr = repondre(p, F, { question: 'effet', y: 'presence' });
  egal('présence-absence → régression logistique', [Pr.reponse, conseil(p, F, Pr).choix, construire('glm_binomial', Pr, F, p).reglages.famille], ['presence', 'glm_binomial', 'binomiale']);
  const Et = repondre(p, F, { question: 'predire', y: 'etat' });
  const rf = construire(conseil(p, F, Et).choix, Et, F, p);
  egal('prédire une classe → forêt aléatoire, la classe en classe, validation par site', [rf.reglages.type, rf.reglages.classe, rf.reglages.y, rf.reglages.vc], ['FOREST', uid(p, 'etat'), undefined, site]);
  const Gr = repondre(p, F, { question: 'groupes', y: 'longueur', groupe: uid(p, 'traitement') });
  const rgr = conseil(p, F, Gr);
  egal('comparer deux traitements sur des sites suivis plusieurs années (des lignes par site et traitement) → modèle mixte, sans test apparié',
    [rgr.choix, construire('normal', Gr, F, p).reglages.x, rgr.autres.includes('tests_apparies')], ['normal', [uid(p, 'traitement')], false]);
  // ten sites, each counted once before and once after
  const avant = projet(20, [['site', Array.from({ length: 10 }, (_, i) => `S${i + 1}`)], ['periode', ['avant', 'après']],
    ['hauteur', null, { min: 3, max: 9, moyenne: 6, ecart: 1.5, entiers: false, zeros: 0 }]]);
  const Fa = faitsVariables(avant.p, avant.stats);
  const Pa = repondre(avant.p, Fa, { question: 'groupes', y: 'hauteur' });
  egal('chaque site une fois avant et une fois après → test apparié par site', [Pa.groupe, conseil(avant.p, Fa, Pa).choix, construire('tests_apparies', Pa, Fa, avant.p).loc],
    [2, 'tests_apparies', { mode: 'deux', deux: { x: 2, y: 3, apparier: 1 } }]);
  const Tl = repondre(p, F, { question: 'tendance', y: 'longueur' });
  const mt = construire(conseil(p, F, Tl).choix, Tl, F, p);
  egal('tendance d’une mesure sur des sites → LMM, l’année en X', [mt.reglages.type, mt.reglages.x], ['LMM', [uid(p, 'annee')]]);
  egal('puis l’autocorrélation des résidus et la puissance', notesPour('normal', contexte(Tl, F, p)).filter((k) => ['qa_note_annees', 'qa_note_puissance'].includes(k)), ['qa_note_annees', 'qa_note_puissance']);
  egal('une seule série → Séries, ses tendances', conseil(p, F, { ...Tl, repetes: false }).choix, 'series');
  const B = repondre(p, F, { question: 'baci', y: 'longueur' });
  egal('BACI → la vue BACI, réponse, temps, unité', [conseil(p, F, B).choix, construire('baci', B, F, p).loc],
    ['baci', { mode: 'baci', baci: { y: uid(p, 'longueur'), temps: uid(p, 'annee'), unite: uid(p, 'site') } }]);
  const Ex = repondre(p, F, { question: 'explorer', reponse: 'mesure' });
  const acp = construire(conseil(p, F, Ex).choix, Ex, F, p);
  egal('explorer des mesures → ACP, les variables quantitatives, composantes automatiques', [acp.reglages.type, acp.reglages.auto, acp.reglages.x],
    ['PCA', true, ['longueur', 'biomasse', 'densite', 'couverture', 'presence'].map((n) => uid(p, n))]);
  const Bord = faitsVariables(p, { ...stats, min: stats.min.map((v, i) => (stats.nom[i] === 'couverture' ? 0 : v)) });
  verifier('une proportion qui touche 0 : la note des bornes', notesPour('glm_beta', contexte(repondre(p, Bord, { question: 'effet', y: 'couverture' }), Bord, p)).includes('qa_note_bornes01'));
  egal('un BACI coché sous « tendance » → BACI', conseil(p, F, repondre(p, F, { question: 'tendance', y: 'longueur', baci: true })).choix, 'baci');
}

console.log('Des spectres et des coordonnées');
{
  const { p, stats } = projet(30, [['teneur', null, { min: 1, max: 9, moyenne: 5, ecart: 2, entiers: false, zeros: 0 }],
    ...Array.from({ length: 40 }, (_, i) => [`l${1000 + 2 * i}`, null, { min: 0.1, max: 0.9, moyenne: 0.4, ecart: 0.1, entiers: false, zeros: 0 }])]);
  const F = faitsVariables(p, stats);
  const R = repondre(p, F, { question: 'predire', y: 'teneur' });
  egal('prédire une mesure avec 40 X corrélées → PLS, composantes automatiques', [conseil(p, F, R).choix, construire('pls', R, F, p).reglages.auto], ['pls', true]);
  const geo = projet(50, [['x', null, { min: 600000, max: 700000, moyenne: 650000, ecart: 2e4, entiers: false, zeros: 0 }],
    ['y', null, { min: 6.2e6, max: 6.3e6, moyenne: 6.25e6, ecart: 2e4, entiers: false, zeros: 0 }],
    ['hauteur', null, { min: 1, max: 30, moyenne: 12, ecart: 6, entiers: false, zeros: 0 }]], { coordonnees: { x: 1, y: 2, systeme: 2154, nom_x: 'x', nom_y: 'y' } });
  const Fg = faitsVariables(geo.p, geo.stats);
  const infos = { coordonnees: geo.p.coordonnees, proposition: null };
  egal('les coordonnées ne sont pas proposées en X', proposer(geo.p, Fg, infos).x, [3]);
  egal('celles que les noms proposent non plus', proposer({ ...geo.p, coordonnees: null }, Fg, { coordonnees: null, proposition: { x: 1, y: 2, systeme: 2154 } }).x, [3]);
  const Rg = repondre(geo.p, Fg, { question: 'espace', y: 'hauteur' }, infos);
  const gs = construire('gam_spatial', Rg, Fg, geo.p);
  egal('le lissage spatial, sur le plan en Lambert-93', [gs.reglages.type, gs.reglages.lisse_spatial, gs.manque], ['GAM', { variables: [1, 2], base: 'tp' }, []]);
  egal('l’autocorrélation de la hauteur', construire('autocorrelation', Rg, Fg, geo.p).loc, { source: 'v3' });
}

// Distance sampling's flat files, one row per detection (Distance's column names, or French ones)
const lignes = projet(120, [
  ['Region.Label', ['Nord', 'Sud']], ['Area', null, { min: 450, max: 630, moyenne: 540, ecart: 90, entiers: true, zeros: 0 }],
  ['Sample.Label', Array.from({ length: 12 }, (_, i) => `L${i + 1}`)], ['Effort', null, { min: 1.2, max: 3.5, moyenne: 2.4, ecart: 0.6, entiers: false, zeros: 0 }],
  ['distance', null, { min: 0, max: 148, moyenne: 41, ecart: 30, entiers: false, zeros: 1, manquantes: 3 }], ['size', null, { min: 1, max: 6, moyenne: 1.8, ecart: 1, entiers: true, zeros: 0 }],
]);
const lignesSimples = projet(80, [['transect', ['T1', 'T2', 'T3', 'T4', 'T5', 'T6']], ['longueur', null, { min: 2, max: 4, moyenne: 3, ecart: 0.5, entiers: false, zeros: 0 }],
  ['distance', null, { min: 0, max: 60, moyenne: 18, ecart: 12, entiers: true, zeros: 4 }]]);
const points = projet(90, [['point', Array.from({ length: 30 }, (_, i) => `P${i + 1}`)], ['distance', null, { min: 0, max: 45, moyenne: 15, ecart: 9, entiers: false, zeros: 2 }]]);
const pointsStrates = projet(144, [['Region.Label', ['Est', 'Ouest']], ['Area', null, { min: 0, max: 0, moyenne: 0, ecart: 0, entiers: true, zeros: 100 }],
  ['Sample.Label', null, { min: 1, max: 30, moyenne: 15, ecart: 8, entiers: true, zeros: 0 }], ['Effort', null, { min: 1, max: 2, moyenne: 1.2, ecart: 0.4, entiers: true, zeros: 0 }],
  ['distance', null, { min: 0.5, max: 34, moyenne: 12, ecart: 7, entiers: false, zeros: 0 }]]);
console.log('L’échantillonnage par distance');
{
  const sansNote = ['qa_note_distance_sans_aire', 'qa_note_distance_un_passage'];
  const notesDistance = (p, R, F) => notesPour('distance', contexte(R, F, p)).filter((k) => sansNote.includes(k));
  {
    const { p, stats } = lignes;
    const F = faitsVariables(p, stats);
    const R = repondre(p, F, { question: 'densite', y: 'distance' });
    const r = conseil(p, F, R);
    egal('une densité, des distances → l’échantillonnage par distance, les distances d’abord à regarder', [R.reponse, R.transect, r.choix, r.autres], ['distances', 'ligne', 'distance', ['statistiques']]);
    const b = construire('distance', R, F, p);
    egal('des lignes en strates : les colonnes de Distance (Sample.Label, Effort, Region.Label, Area, size)', b.reglages,
      { type: 'DISTANCE', y: [uid(p, 'distance')], options: { transect: 'ligne', echantillon: { by: 'variable', variable: uid(p, 'Sample.Label') }, effort: uid(p, 'Effort'),
        strate: { by: 'variable', variable: uid(p, 'Region.Label') }, aire: uid(p, 'Area'), taille: uid(p, 'size') } });
    egal('à compléter : les unités seules ; aucune note', [b.manque, notesDistance(p, R, F)], [['unites_lignes'], []]);
    egal('les distances ne sont pas proposées comme espèces, ni l’aire', proposer(p, F).especes, []);
  }
  {
    const { p, stats } = lignesSimples;
    const F = faitsVariables(p, stats);
    const R = repondre(p, F, { question: 'densite', y: 'distance' });
    const b = construire('distance', R, F, p);
    egal('des lignes sans strate, des noms français (transect, longueur) ; des distances entières restent des distances', [R.reponse, b.reglages.options],
      ['distances', { transect: 'ligne', echantillon: { by: 'variable', variable: uid(p, 'transect') }, effort: uid(p, 'longueur'), strate: null, aire: null, taille: null }]);
    egal('sans aire : la densité seule', notesDistance(p, R, F), ['qa_note_distance_sans_aire']);
  }
  {
    const { p, stats } = points;
    const F = faitsVariables(p, stats);
    const R = repondre(p, F, { question: 'densite', y: 'distance' });
    const b = construire('distance', R, F, p);
    egal('des points (la colonne « point ») sans passages : une visite chacun, pas d’effort à compléter', [R.transect, b.reglages.options.transect, b.reglages.options.effort, b.manque, notesDistance(p, R, F)],
      ['point', 'point', null, ['unites_points'], ['qa_note_distance_sans_aire', 'qa_note_distance_un_passage']]);
  }
  {
    const { p, stats } = pointsStrates;
    const F = faitsVariables(p, stats);
    const R = repondre(p, F, { question: 'densite', y: 'distance', transect: 'point' });
    const b = construire('distance', R, F, p);
    egal('des points en strates, leurs aires à 0 (PTExercise) : des numéros de points, la densité seule', [b.reglages.options, notesDistance(p, R, F)],
      [{ transect: 'point', echantillon: { by: 'variable', variable: uid(p, 'Sample.Label') }, effort: uid(p, 'Effort'), strate: { by: 'variable', variable: uid(p, 'Region.Label') },
        aire: uid(p, 'Area'), taille: null }, ['qa_note_distance_sans_aire']]);
    const lignesAuLieu = construire('distance', { ...R, transect: 'ligne' }, F, p);
    egal('les mêmes en lignes : l’effort est leur longueur', [lignesAuLieu.reglages.options.transect, lignesAuLieu.manque], ['ligne', ['unites_lignes']]);
  }
  {
    // a transect given as an observation ID, and a table without any transect column
    const { p, stats } = projet(40, [['Effort', null, { min: 1, max: 2, moyenne: 1.5, ecart: 0.3, entiers: false, zeros: 0 }],
      ['distance', null, { min: 0, max: 30, moyenne: 9, ecart: 6, entiers: false, zeros: 0 }]], { identifiants: [{ rang: 2, nom: 'Sample.Label' }] });
    const F = faitsVariables(p, stats);
    const R = repondre(p, F, { question: 'densite', y: 'distance' });
    egal('le transect, un identifiant des observations', construire('distance', R, F, p).reglages.options.echantillon, { by: 'id', id: 'Sample.Label' });
    const sans = { ...p, identifiants: [] };
    const b = construire('distance', repondre(sans, F, { question: 'densite', y: 'distance' }), F, sans);
    egal('sans colonne de transect : à compléter, et New model devine lui-même les autres', [b.manque, Object.keys(b.reglages.options)], [['echantillon', 'unites_lignes'], ['transect']]);
    const vide = construire('distance', repondre(sans, F, { question: 'densite' }), F, sans);
    egal('sans distances choisies : à compléter', vide.manque[0], 'distances');
  }
}

console.log('Toutes les questions sur cinq tableaux');
{
  const erreurs = [];
  const clesManquantes = new Set();
  let n = 0;
  for (const { p, stats } of [comptages, especes, mesures, lignes, points]) {
    const F = faitsVariables(p, stats);
    for (const question of QUESTIONS) for (const but of question === 'especes' ? BUTS : [null]) {
      for (const reponse of formulaire(question, but).reponses) {
        for (const plan of [{}, { repetes: false, annees: false, baci: false, coordonnees: false }]) {
          // the first variable of the kind, as the response
          const R = proposer(p, F);
          Object.assign(R, { question, but: but || 'composition', ...plan });
          const v = p.variables.find((x) => (reponse === 'classe' ? x.qualitative : !x.qualitative && genre(F.get(x.uid)) === reponse));
          if (v && reponse !== 'especes') choisirReponse(R, F, v.uid);
          R.reponse = reponse;
          const c = contexte(R, F, p);
          const r = recommander(c);
          if (!r) { erreurs.push(`${p.nom} ${question} ${reponse} : aucune analyse`); continue; }
          for (const id of [r.choix, ...r.autres]) {
            try {
              const b = construire(id, R, F, p, c, { langue: 'en' });
              n += 1;
              for (const k of b.manque) if (!TEXTES_CHOIX.fr[`qa_manque_${k}`] || !TEXTES_CHOIX.en[`qa_manque_${k}`]) clesManquantes.add(k);
              for (const x of b.rempli) if (!TEXTES_CHOIX.fr[x.cle] || (x.texte && !x.texte.startsWith('famille_') && !TEXTES_CHOIX.fr[x.texte])) clesManquantes.add(x.cle);
              for (const k of notesPour(id, c)) if (!TEXTES_CHOIX.fr[k]) clesManquantes.add(k);
              if (b.ouvre === 'modele' && !b.reglages.type) erreurs.push(`${id} sans type`);
              if (b.ouvre === 'vue' && !b.vue) erreurs.push(`${id} sans vue`);
            } catch (e) { erreurs.push(`${question} ${reponse} ${id} : ${e.message}`); }
          }
        }
      }
    }
  }
  egal(`chaque analyse se construit (${n} essais), sans erreur`, erreurs, []);
  egal('chaque « à compléter », « rempli » et note a son texte', [...clesManquantes], []);
}

console.log(echecs ? `\n${echecs} vérification(s) en échec` : '\nTout est bon.');
process.exit(echecs ? 1 : 0);
