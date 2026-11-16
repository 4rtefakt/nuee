// Which analysis? (Model › Which analysis?): the decision logic, apart from its dialog
// (quelle_analyse.js) so that it reads, extends and is tested on its own (app/tests/essai_choix.mjs,
// run with node: no page, no engine). Nothing here draws or asks anything.
//   - what Nuée knows of each variable (faitsVariables): its type and categories from the project,
//     and from the engine's variable statistics (moteur/R/diagnostic.R, statistiques_variables) its
//     bounds, whole numbers, share of zeros, skewness and missing values;
//   - the answers proposed from them (proposer, genre, beaucoupDeZeros, xProposees);
//   - REGLES, the table of rules: a question, a kind of response and the design give the analysis
//     recommended and its alternatives, the first rule that matches winning (recommander);
//   - CATALOGUE: what each analysis opens, New model (its type and distribution) or a view, and
//     construire(), the settings it opens with (roles, random effects, cross-validation groups...);
//   - NOTES: sentences added to an analysis when the answers call for them;
//   - TEXTES_CHOIX: the strings, French and English (quelle_analyse.js adds them: ajouterTextes).
// To add an analysis: its entry in CATALOGUE (and its case in construire if it opens New model
// with settings of its own), its name, why and conditions in TEXTES_CHOIX (qa_nom_<id>,
// qa_pourquoi_<id>, qa_conditions_<id>), and the rules that recommend it in REGLES.

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)

// ------------------------------------------------------------ the questions and their fields

export const QUESTIONS = ['groupes', 'effet', 'especes', 'tendance', 'densite', 'espace', 'explorer', 'predire', 'diversite', 'baci'];
// what the species are for, under "Many species at once"
export const BUTS = ['composition', 'par_espece', 'indicatrices'];
// the kinds of response (a measurement, a count, 0/1, a proportion, a qualitative variable, species
// columns, the distances of the animals detected from a line or a point)
export const REPONSES = ['mesure', 'comptage', 'presence', 'proportion', 'classe', 'especes', 'distances'];

// What each question asks (a sub-goal of the species: 'especes/<but>'):
//   reponses: the kinds of response offered ('especes' alone: the species columns are the response;
//     a single other kind: the response's variable, of that kind);
//   facultative: the response may stay unchosen; plusieurs: several variables rather than one (explore);
//   champs: 'x' (the explanatory variables), 'groupe' (a qualitative variable: the groups), and the
//     design: 'repetes' (the same sites measured several times: the site), 'annees' (the time),
//     'baci' (the unit), 'coordonnees', 'transects' (lines or points); a name ending in '!' is
//     required, without a box to tick.
export const FORMULAIRE = {
  densite: { reponses: ['distances'], champs: ['transects!'] },
  groupes: { reponses: ['mesure', 'comptage', 'presence', 'proportion', 'classe', 'especes'], champs: ['groupe', 'repetes', 'baci'] },
  effet: { reponses: ['mesure', 'comptage', 'presence', 'proportion', 'classe', 'especes'], champs: ['x', 'repetes', 'annees', 'baci', 'coordonnees'] },
  'especes/composition': { reponses: ['especes'], champs: ['x', 'repetes', 'coordonnees'] },
  'especes/par_espece': { reponses: ['especes'], champs: ['x', 'repetes', 'annees'] },
  'especes/indicatrices': { reponses: ['especes'], champs: ['groupe!', 'repetes'] },
  tendance: { reponses: ['mesure', 'comptage', 'presence', 'proportion', 'especes'], champs: ['annees!', 'repetes', 'baci'] },
  espace: { reponses: ['mesure', 'comptage', 'presence', 'proportion', 'classe', 'especes'], facultative: true, champs: ['coordonnees!'] },
  explorer: { reponses: ['mesure', 'especes'], plusieurs: true, champs: [] },
  predire: { reponses: ['mesure', 'comptage', 'presence', 'proportion', 'classe'], champs: ['x', 'repetes'] },
  diversite: { reponses: ['especes'], champs: ['groupe'] },
  baci: { reponses: ['mesure', 'comptage', 'proportion'], champs: ['annees!', 'baci!'] },
};
export const formulaire = (question, but) => (question === 'especes' ? FORMULAIRE[`especes/${but || 'composition'}`] : FORMULAIRE[question]) || null;

// ------------------------------------------------------------ what Nuée knows of the variables

// Each variable's facts, by its number: from the project's summary (p.variables) and, when the
// engine gave them, from its statistics (s: the statistiques_variables request, column by column).
export function faitsVariables(p, s) {
  const uids = s ? liste(s.uid).map(Number) : [];
  const index = new Map(uids.map((u, i) => [u, i]));
  const col = (k, i) => (s && i !== undefined && s[k] !== undefined ? (liste(s[k])[i] ?? null) : null);
  return new Map(liste(p.variables).map((v) => {
    const i = index.get(v.uid);
    return [v.uid, {
      uid: v.uid, nom: v.nom, qualitative: !!v.qualitative, ncat: liste(v.categories).length, dates: !!v.temps, unite: String(v.unite || '').trim(),
      N: col('N', i), manquantes: col('manquantes', i), min: col('min', i), max: col('max', i), moyenne: col('moyenne', i),
      ecart: col('ecart_type', i), asymetrie: col('test_asymetrie', i), suggestion: col('suggestion', i),
      entiers: col('entiers', i), zeros: col('zeros', i),
    }];
  }));
}

// Whole numbers that are a measurement rounded to its unit, not counts: the unit of a measurement
// (Edit › Units and descriptions: g, kg, m, cm, mm, ha, %, °C..., or anything per something, a
// density), or a name that says a measurement (a mean, a ratio, a rate, a biomass, a length...).
// A count's unit (ind., nests) or name (abundance, n_birds) says counts; neither: counts, in doubt.
const UNITE_COMPTAGE = /^(ind\.?|individus?|individuals?|nids?|nests?|oiseaux|birds?|couples?|pairs?|n|nb|nombre|number|counts?|comptages?|effectifs?)$/i;
const MOTIF_MESURE = /(^|[_ .-])(moy|mean|average|avg|ratio|rapport|taux|rate|indice|index|densit|biomass|masse|mass|poids|weight|longueur|length|hauteur|height|diam[eè]tre|diameter|dbh|surface|area|recouvrement|couverture|cover|pourcent|percent|pct|temp[ée]rature|altitude|elevation|profondeur|depth)/i;
const MOTIF_COMPTAGE = /(^|[_ .-])(n|nb|nombre|number|counts?|comptages?|effectifs?|abondances?|abundances?|individus|individuals|nids|nests|oiseaux|birds|captures|catch)([_ .-]|$)/i;
const uniteComptage = (f) => !!f.unite && UNITE_COMPTAGE.test(f.unite);
export const mesureArrondie = (f) => !!f && !uniteComptage(f) && (!!f.unite || MOTIF_MESURE.test(f.nom || ''));
// whole numbers 0 or more read as counts with nothing to say so: a measurement rounded is possible
export const comptageIncertain = (f) => !!f && f.entiers === true && !mesureArrondie(f) && !uniteComptage(f) && !MOTIF_COMPTAGE.test(f.nom || '');

// The kind of a response, from its facts: a class (qualitative), a presence-absence (whole numbers
// from 0 to 1), a proportion (between 0 and 1), a count (whole numbers, 0 or more, unless their unit
// or name says a measurement), else a measurement.
export function genre(f) {
  if (!f) return null;
  if (f.qualitative) return 'classe';
  if (f.min === null || f.max === null) return 'mesure';
  if (f.entiers === true && f.min >= 0 && f.max <= 1) return 'presence';
  if (f.min >= 0 && f.max <= 1) return 'proportion';
  if (f.entiers === true && f.min >= 0) return mesureArrondie(f) ? 'mesure' : 'comptage';
  return 'mesure';
}
// The share of zeros a Poisson distribution of the same mean gives, in %.
export const zerosPoisson = (f) => (f && f.moyenne !== null && f.moyenne >= 0 ? 100 * Math.exp(-f.moyenne) : null);
// Many zeros: a quarter of the values at least, and 15 points more than a Poisson of the same mean gives.
export const beaucoupDeZeros = (f) => !!f && f.zeros !== null && zerosPoisson(f) !== null && f.zeros >= 25 && f.zeros >= zerosPoisson(f) + 15;
// Counts that vary more than a Poisson allows: a variance beyond 1.5 times the mean (before any X).
export const surdispersion = (f) => !!f && f.ecart !== null && f.moyenne > 0 && f.ecart * f.ecart > 1.5 * f.moyenne;
// A strictly positive measurement skewed to the right (the skewness test beyond 2, as Variable statistics marks it).
export const asymetrique = (f) => !!f && f.min !== null && f.min > 0 && f.asymetrie !== null && f.asymetrie > 2;
// A measurement with zeros, not a count (a density, a biomass): 5 % of zeros at least.
export const avecZeros = (f) => !!f && f.entiers !== true && f.min === 0 && f.zeros !== null && f.zeros >= 5;
// A proportion that reaches 0 or 1, which the beta distribution refuses (moteur/R/regression.R: verifier_reponse).
export const bornes01 = (f) => !!f && f.min !== null && (f.min <= 0 || f.max >= 1);
// A whole number variable with fewer than five values possible (0/1, a score of 1 to 4): no curve.
const peuDeValeurs = (f) => !!f && f.entiers === true && f.min !== null && f.max - f.min < 4;

// The names that point to a role (the TRIM, Series and BACI views guess theirs the same way).
const MOTIF_SITE = /^(site|station|parcelle|placette|plot|quadrat|transect|carr[ée]|route|point|lieu|localit|lac|lake|[ée]tang|pond|mare|rivi[eè]re|river)/i;
const MOTIF_SECTEUR = /^(secteur|sector|unit)/i;
const MOTIF_TEMPS = /^(ann[ée]e|year|an$|date|time|temps|jour|day|mois|month|campagne|survey)/i;
const MOTIF_GROUPE = /^(habitat|traitement|treatment|groupe|group|type|milieu|zone|p[ée]riode|period|saison|season|gestion|management|statut|status|substrat|substrate|facies|faci[eè]s|classe|class)/i;
const MOTIF_BACI = /^(impact|t[ée]moins?|contr[ôo]les?|controls?|r[ée]f[ée]rences?)$/i;

const nObs = (p) => liste(p.observations && p.observations.uid).length;
// a column picked: 'v:<number>' a variable, 'id:<rank>' an observation ID
const colonneVariable = (u) => `v:${u}`;
export const uidDe = (col) => (typeof col === 'string' && col.startsWith('v:') ? Number(col.slice(2)) : null);
// as New model takes it (random effects, cross-validation groups, TRIM's site and time)
export function specDialogue(col, p) {
  if (!col) return null;
  if (col.startsWith('v:')) return { by: 'variable', variable: Number(col.slice(2)) };
  const i = liste(p.identifiants).find((x) => String(x.rang) === col.slice(3));
  return i ? { by: 'id', id: i.nom } : null;
}
// as the project's views take it (tests, Series, BACI): a variable's number, or 'id:<rank>'
export const valeurVue = (col) => (!col ? null : col.startsWith('v:') ? Number(col.slice(2)) : col);
// its name
export function nomColonne(col, p) {
  if (!col) return '';
  const u = uidDe(col);
  if (u !== null) return (liste(p.variables).find((v) => v.uid === u) || {}).nom || '';
  return (liste(p.identifiants).find((x) => String(x.rang) === col.slice(3)) || {}).nom || '';
}

// A year: whole numbers between 1900 and 2100.
const annees = (f) => !!f && f.entiers === true && f.min >= 1900 && f.max <= 2100 && f.max > f.min;
function trouverTemps(p, F) {
  const id = liste(p.identifiants).find((i) => MOTIF_TEMPS.test(i.nom));
  if (id) return `id:${id.rang}`;
  const vars = liste(p.variables);
  const v = vars.find((x) => MOTIF_TEMPS.test(x.nom) && (!x.qualitative || x.temps))
    || vars.find((x) => !x.qualitative && annees(F.get(x.uid))) || vars.find((x) => x.qualitative && x.temps);
  return v ? colonneVariable(v.uid) : null;
}
// a site: a qualitative variable named like one whose categories repeat (two observations per site
// at least, on average), else an ID named like one (a .usp file's IDs may repeat)
function trouverSite(p, motif = MOTIF_SITE) {
  const n = nObs(p);
  const v = liste(p.variables).find((x) => x.qualitative && motif.test(x.nom) && liste(x.categories).length >= 2 && liste(x.categories).length <= n / 2);
  if (v) return colonneVariable(v.uid);
  const id = liste(p.identifiants).find((i) => motif.test(i.nom));
  return id ? `id:${id.rang}` : null;
}
// the groups: a qualitative variable named like them, else the one with the fewest categories (two at least)
function trouverGroupe(p, exclus) {
  const n = nObs(p);
  const qualitatives = liste(p.variables).filter((v) => v.qualitative && !exclus.has(v.uid)
    && liste(v.categories).length >= 2 && liste(v.categories).length <= Math.max(2, Math.min(30, n / 3)));
  const v = qualitatives.find((x) => MOTIF_GROUPE.test(x.nom))
    || [...qualitatives].sort((a, b) => liste(a.categories).length - liste(b.categories).length)[0];
  return v ? v.uid : null;
}
// a BACI design: a qualitative variable with an impact or control category, or named after one
const baciProbable = (p) => liste(p.variables).some((v) => v.qualitative
  && (/baci|impact/i.test(v.nom) || liste(v.categories).some((c) => MOTIF_BACI.test(String(c).trim()))));

// Distance sampling's flat file, one row per detection (Distance's Sample.Label, Effort,
// Region.Label, Area, size): its columns by the names New model's own fields guess from
// (vues_distance.js: MOTIFS and deviner, the same patterns).
const MOTIFS_DISTANCE = {
  distance: /^(distance|dist|perp)/i, echantillon: /^(sample|transect|ligne|line|point|station|parcours|route|itin)/i,
  effort: /^(effort|longueur|length|passages?|visites?|visits?)/i, strate: /^(region|r[ée]gion|strat|zone|secteur)/i,
  aire: /^(area|aire|surface|superficie)/i, taille: /^(size|taille|cluster|groupe|group)/i,
};
const MOTIF_POINTS = /point|station|[ée]coute|poste/i;
// a column by its name: an ID or any variable ('id:<rank>', 'v:<number>'), or a quantitative
// variable only (its number); null: none
function colonneParNom(p, motif, quantitative = false) {
  if (!quantitative) {
    const id = liste(p.identifiants).find((i) => motif.test(i.nom));
    if (id) return `id:${id.rang}`;
  }
  const v = liste(p.variables).find((x) => motif.test(x.nom) && (!quantitative || !x.qualitative));
  return v ? (quantitative ? v.uid : colonneVariable(v.uid)) : null;
}
// The transect (the line or the point), the stratum, the effort, the area and the group size the
// names show.
export function colonnesDistance(p) {
  return {
    echantillon: colonneParNom(p, MOTIFS_DISTANCE.echantillon), strate: colonneParNom(p, MOTIFS_DISTANCE.strate),
    effort: colonneParNom(p, MOTIFS_DISTANCE.effort, true), aire: colonneParNom(p, MOTIFS_DISTANCE.aire, true),
    taille: colonneParNom(p, /^(size|taille)$/i, true) ?? colonneParNom(p, MOTIFS_DISTANCE.taille, true),
  };
}
// the distances: a quantitative variable named like them, or null
export const distanceProposee = (p) => colonneParNom(p, MOTIFS_DISTANCE.distance, true);

// A species column: numbers 0 or more with at least one zero (an absence), not a time nor a coordinate.
function especeProbable(v, f, autres) {
  return !v.qualitative && !autres.has(v.uid) && !MOTIF_TEMPS.test(v.nom) && !!f && f.min !== null && f.min >= 0 && f.zeros !== null && f.zeros > 0
    && !annees(f);
}

// The X proposed: the variables with no other role, without those that can't explain anything (a
// code per observation, constant, more than half missing).
export function xProposees(R, F, p, coordonnees = []) {
  const n = nObs(p);
  const exclus = new Set([R.y, ...liste(R.especes), uidDe(R.site), uidDe(R.temps), uidDe(R.unite), ...liste(coordonnees)]
    .filter((u) => u !== null && u !== undefined));
  return liste(p.variables).filter((v) => {
    const f = F.get(v.uid);
    if (exclus.has(v.uid)) return false;
    if (f && f.manquantes !== null && f.manquantes > 50) return false;
    if (v.qualitative) return liste(v.categories).length >= 2 && liste(v.categories).length < Math.max(3, n / 2);
    return !(f && f.min !== null && f.min === f.max);
  }).map((v) => v.uid);
}

// The answers proposed when the dialog opens: no question (that is the user's), the design and
// the variables guessed from the data. coordonnees: the engine's `coordonnees` request (the
// project's, and those its variables' names propose), or null.
export function proposer(p, F, coordonnees = null) {
  const co = (coordonnees && coordonnees.coordonnees) || p.coordonnees || null;
  const proposees = co || (coordonnees && coordonnees.proposition) || null;
  const coords = proposees ? [proposees.x, proposees.y].map(Number) : [];
  const temps = trouverTemps(p, F);
  const site = trouverSite(p) || trouverSite(p, MOTIF_SECTEUR);
  const unite = trouverSite(p, MOTIF_SECTEUR) || site;
  const autres = new Set([uidDe(temps), ...coords].filter((u) => u !== null));
  const especes = liste(p.variables).filter((v) => especeProbable(v, F.get(v.uid), autres)).map((v) => v.uid);
  const R = {
    question: null, but: 'composition', reponse: null, zeros: false, y: null,
    // (four columns at least: a few counts with zeros in a table of measurements aren't a community)
    especes: especes.length >= 4 ? especes : [], x: [], variables: [],
    groupe: trouverGroupe(p, new Set([uidDe(site), uidDe(unite)])), site, temps, unite,
    repetes: !!site, annees: !!temps, baci: baciProbable(p), coordonnees: !!proposees,
    // (distance sampling: points when the transect's column is named like them, lines otherwise)
    transect: MOTIF_POINTS.test(nomColonne(colonnesDistance(p).echantillon, p)) ? 'point' : 'ligne',
  };
  R.x = xProposees(R, F, p, coords);
  // (explore: the quantitative variables proposed as X, the time and the coordinates aside)
  R.variables = R.x.filter((u) => !F.get(u).qualitative);
  return R;
}

// The response chosen: its kind (the only one its question offers, or the one its values say) and
// many zeros, from its facts. p (the project): the groups, when the response is the qualitative
// variable they were, become another one (or none, to choose), never the response itself.
export function choisirReponse(R, F, uid, p = null) {
  const f = uid === null || uid === undefined ? null : F.get(uid);
  R.y = f ? uid : null;
  if (f) {
    const seule = formulaire(R.question, R.but);
    R.reponse = seule && seule.reponses.length === 1 ? seule.reponses[0] : genre(f);
    R.zeros = R.reponse === 'comptage' && beaucoupDeZeros(f);
  }
  R.x = liste(R.x).filter((u) => u !== R.y);
  R.variables = liste(R.variables).filter((u) => u !== R.y);
  if (p && R.y !== null && R.groupe === R.y) R.groupe = trouverGroupe(p, new Set([R.y, uidDe(R.site), uidDe(R.unite)]));
  return R;
}

// The variables the response list offers (their numbers, in the dialog's order): the quantitative
// ones for a question with numbers as its response, then the qualitative ones for a class; not
// the time, the site or the unit of a design box ticked (or required): they are the design.
export function reponsesPossibles(R, F, p) {
  const f = formulaire(R.question, R.but);
  if (!f) return [];
  const coche = (k) => f.champs.includes(`${k}!`) || (f.champs.includes(k) && !!R[k]);
  const plan = new Set([coche('annees') && uidDe(R.temps), coche('repetes') && uidDe(R.site), coche('baci') && uidDe(R.unite)].filter((u) => typeof u === 'number'));
  const numeriques = f.reponses.some((k) => ['mesure', 'comptage', 'presence', 'proportion', 'distances'].includes(k));
  const vars = liste(p.variables).filter((v) => !plan.has(v.uid));
  return [...(numeriques ? vars.filter((v) => !v.qualitative) : []), ...(f.reponses.includes('classe') ? vars.filter((v) => v.qualitative) : [])].map((v) => v.uid);
}

// The response once the question or the design is chosen: none when the list no longer offers it
// (the time, once it is the trend's time), and the only variable left when one alone fits (the
// counts of a trend, the year being its time). Not for a response that may stay unchosen, several
// variables or the species.
export function proposerReponse(R, F, p) {
  const f = formulaire(R.question, R.but);
  if (!f || f.facultative || f.plusieurs || R.reponse === 'especes' || f.reponses.every((k) => k === 'especes')) return R;
  const possibles = reponsesPossibles(R, F, p);
  if (R.y !== null && R.y !== undefined && !possibles.includes(R.y)) R.y = null;
  if ((R.y === null || R.y === undefined) && possibles.length === 1) choisirReponse(R, F, possibles[0], p);
  return R;
}

// ------------------------------------------------------------ the rules

// What the rules read: the answers, and what the data say of the response, the X and the species.
// A design box counts only where the question shows it (FORMULAIRE), ticked or required ('!').
export function contexte(R, F, p) {
  const f = R.y !== null && R.y !== undefined ? F.get(R.y) : null;
  const xs = liste(R.x).map((u) => F.get(u)).filter(Boolean);
  const n = nObs(p);
  const classe = R.reponse === 'classe' && f ? f : R.groupe !== null && R.groupe !== undefined ? F.get(R.groupe) : null;
  const especes = liste(R.especes).map((u) => F.get(u)).filter(Boolean);
  const temps = uidDe(R.temps) !== null ? F.get(uidDe(R.temps)) : null;
  const site = uidDe(R.site) !== null ? F.get(uidDe(R.site)) : null;
  const groupe = R.groupe !== null && R.groupe !== undefined ? F.get(R.groupe) : null;
  const champs = (formulaire(R.question, R.but) || { champs: [] }).champs;
  const coche = (k) => champs.includes(`${k}!`) || (champs.includes(k) && !!R[k]);
  return {
    question: R.question, but: R.question === 'especes' ? R.but || 'composition' : null, reponse: R.reponse,
    zeros: R.reponse === 'comptage' && !!R.zeros,
    // (whole numbers taken for counts with nothing to say so: a measurement rounded to its unit too)
    comptageIncertain: R.reponse === 'comptage' && comptageIncertain(f),
    surdispersion: R.reponse === 'comptage' && surdispersion(f),
    asymetrique: R.reponse === 'mesure' && asymetrique(f),
    avecZeros: R.reponse === 'mesure' && avecZeros(f),
    bornes01: R.reponse === 'proportion' && bornes01(f),
    repetes: coche('repetes') && !!R.site,
    // (each site once in each group at most: no more observations than sites × groups, which a
    // paired test needs; a site followed several years in each group isn't paired by the site alone)
    unParSite: coche('repetes') && !!site && !!groupe && site.qualitative && groupe.qualitative && n > 0 && n <= site.ncat * groupe.ncat,
    // (one row per site and year at most, as TRIM needs: no more observations than sites × years
    // spanned; null when it can't be told, a time made of dates or an ID)
    unParSiteTemps: !site || !site.qualitative || !temps || temps.qualitative || temps.entiers !== true || temps.min === null ? null
      : n <= site.ncat * (temps.max - temps.min + 1),
    annees: coche('annees') && !!R.temps,
    // (a time that isn't a quantitative variable can't be an X)
    tempsNonQuantitatif: coche('annees') && !!R.temps && !(temps && !temps.qualitative),
    baci: coche('baci'),
    coordonnees: coche('coordonnees') && !!p.coordonnees,
    xQuantitatives: xs.some((x) => !x.qualitative),
    xQualitatives: xs.length > 0 && xs.every((x) => x.qualitative),
    beaucoupDeX: xs.length >= 20 || (n > 0 && xs.length >= n / 2),
    deuxClasses: !!classe && classe.qualitative && classe.ncat === 2,
    especesEntieres: especes.length ? especes.every((e) => e.entiers === true) : null,
    // distance sampling: no area for the strata (or areas all 0), the density only; points without a
    // column of visits, one visit each
    distanceSansAire: R.question === 'densite' && (() => { const a = colonnesDistance(p).aire; return a === null || (!!F.get(a) && F.get(a).max === 0); })(),
    pointsSansEffort: R.question === 'densite' && R.transect === 'point' && colonnesDistance(p).effort === null,
  };
}

// The table of rules, read from the top: the first whose question, sub-goal (but), kind of
// response and conditions (si: other values of the context above) all match gives the analysis
// recommended (choix) and its alternatives (autres), ids of CATALOGUE. A value may be a list of
// those accepted; a condition left out accepts anything.
export const REGLES = [
  // A before-after-control-impact design, whatever the question that leads to it
  { question: ['groupes', 'effet', 'tendance'], si: { baci: true }, choix: 'baci', autres: ['series'] },
  { question: 'baci', choix: 'baci', autres: ['series'] },

  // Compare groups or periods (the groups: a qualitative variable)
  { question: 'groupes', reponse: 'especes', choix: 'permanova', autres: ['indval', 'nmds', 'tests_classes'] },
  { question: 'groupes', reponse: 'classe', choix: 'tests_deux', autres: [] },
  { question: 'groupes', reponse: 'mesure', si: { repetes: true, unParSite: true }, choix: 'tests_apparies', autres: ['normal', 'tests_deux'] },
  { question: 'groupes', reponse: 'mesure', si: { repetes: true }, choix: 'normal', autres: ['tests_deux'] },
  { question: 'groupes', reponse: 'mesure', si: { asymetrique: true }, choix: 'tests_deux', autres: ['glm_gamma', 'lm_log', 'tests_classes'] },
  { question: 'groupes', reponse: 'mesure', choix: 'tests_deux', autres: ['normal', 'tests_classes'] },
  { question: 'groupes', reponse: 'comptage', si: { zeros: true }, choix: 'glm_nb', autres: ['glm_zip', 'tests_deux'] },
  { question: 'groupes', reponse: 'comptage', si: { surdispersion: true }, choix: 'glm_nb', autres: ['glm_poisson', 'tests_deux'] },
  { question: 'groupes', reponse: 'comptage', choix: 'glm_poisson', autres: ['glm_nb', 'tests_deux'] },
  { question: 'groupes', reponse: 'presence', choix: 'glm_binomial', autres: ['tests_deux'] },
  { question: 'groupes', reponse: 'proportion', choix: 'glm_beta', autres: ['tests_deux'] },

  // The effect of variables on one response
  { question: 'effet', reponse: 'especes', choix: 'rda', autres: ['cca', 'par_reponse', 'permanova'] },
  { question: 'effet', reponse: 'classe', choix: 'arbre', autres: ['lda', 'foret', 'plsda'] },
  { question: 'effet', reponse: 'mesure', si: { avecZeros: true }, choix: 'glm_tweedie', autres: ['lm_log', 'normal', 'gam'] },
  // (a skewed response, judged before any X: the X may make it so, as a biomass falling with the
  // elevation; the linear model first, its residuals checked after the fit, the gamma GLM and the
  // log offered, never recommended on the raw skew alone)
  { question: 'effet', reponse: 'mesure', si: { coordonnees: true }, choix: 'normal', autres: ['gam_spatial', 'gam', 'foret'] },
  { question: 'effet', reponse: 'mesure', si: { asymetrique: true }, choix: 'normal', autres: ['glm_gamma', 'lm_log', 'gam'] },
  { question: 'effet', reponse: 'mesure', choix: 'normal', autres: ['gam', 'foret', 'arbre'] },
  { question: 'effet', reponse: 'comptage', si: { zeros: true }, choix: 'glm_nb', autres: ['glm_zip', 'glm_poisson', 'gam'] },
  { question: 'effet', reponse: 'comptage', si: { surdispersion: true }, choix: 'glm_nb', autres: ['glm_poisson', 'gam', 'boost'] },
  { question: 'effet', reponse: 'comptage', choix: 'glm_poisson', autres: ['glm_nb', 'gam'] },
  { question: 'effet', reponse: 'presence', choix: 'glm_binomial', autres: ['gam', 'boost', 'arbre'] },
  { question: 'effet', reponse: 'proportion', choix: 'glm_beta', autres: ['gam', 'foret'] },

  // Many species at once
  { question: 'especes', but: 'indicatrices', choix: 'indval', autres: ['permanova', 'tests_classes'] },
  { question: 'especes', but: 'par_espece', choix: 'par_reponse', autres: ['rda', 'permanova'] },
  { question: 'especes', but: 'composition', si: { xQuantitatives: true }, choix: 'rda', autres: ['cca', 'nmds', 'permanova'] },
  { question: 'especes', but: 'composition', si: { xQualitatives: true }, choix: 'permanova', autres: ['nmds', 'indval', 'rda'] },
  { question: 'especes', but: 'composition', si: { coordonnees: true }, choix: 'nmds', autres: ['mantel', 'pcoa', 'ca'] },
  { question: 'especes', but: 'composition', choix: 'nmds', autres: ['pcoa', 'ca'] },

  // A trend over time, a monitoring index
  { question: 'tendance', reponse: 'especes', choix: 'par_reponse', autres: ['rda', 'trim'] },
  { question: 'tendance', reponse: 'comptage', si: { repetes: true, unParSiteTemps: [true, null] }, choix: 'trim', autres: ['glm_nb', 'series', 'gam'] },
  { question: 'tendance', reponse: 'comptage', si: { zeros: true }, choix: 'glm_nb', autres: ['glm_zip', 'series', 'trim'] },
  { question: 'tendance', reponse: 'comptage', si: { surdispersion: true }, choix: 'glm_nb', autres: ['series', 'trim', 'gam'] },
  { question: 'tendance', reponse: 'comptage', choix: 'glm_poisson', autres: ['glm_nb', 'series', 'trim'] },
  { question: 'tendance', reponse: 'mesure', si: { repetes: true }, choix: 'normal', autres: ['series', 'gam'] },
  { question: 'tendance', reponse: 'mesure', choix: 'series', autres: ['normal', 'gam'] },
  { question: 'tendance', reponse: 'presence', choix: 'glm_binomial', autres: ['gam'] },
  { question: 'tendance', reponse: 'proportion', choix: 'glm_beta', autres: ['gam', 'series'] },

  // Where: maps, spatial autocorrelation
  { question: 'espace', reponse: 'especes', si: { coordonnees: true }, choix: 'mantel', autres: ['nmds', 'carte'] },
  { question: 'espace', reponse: 'especes', choix: 'carte', autres: ['mantel', 'nmds'] },
  { question: 'espace', choix: 'carte', autres: ['autocorrelation', 'gam_spatial'] },

  // Explore or summarise a table
  { question: 'explorer', reponse: 'especes', choix: 'nmds', autres: ['ca', 'pcoa', 'rarefaction'] },
  { question: 'explorer', choix: 'acp', autres: ['statistiques', 'correlations', 'kmeans'] },

  // Predict or classify
  { question: 'predire', reponse: 'classe', si: { beaucoupDeX: true }, choix: 'plsda', autres: ['oplsda', 'foret', 'classes', 'lda'] },
  { question: 'predire', reponse: 'classe', choix: 'foret', autres: ['lda', 'arbre', 'plsda', 'oplsda'] },
  { question: 'predire', reponse: 'mesure', si: { beaucoupDeX: true }, choix: 'pls', autres: ['opls', 'foret', 'boost'] },
  { question: 'predire', reponse: 'mesure', choix: 'foret', autres: ['boost', 'normal', 'arbre', 'pls'] },
  { question: 'predire', reponse: 'comptage', choix: 'boost', autres: ['foret', 'glm_nb'] },
  { question: 'predire', reponse: 'presence', choix: 'boost', autres: ['foret', 'glm_binomial', 'gam'] },
  { question: 'predire', reponse: 'proportion', choix: 'foret', autres: ['glm_beta', 'boost'] },

  // Diversity and species richness
  { question: 'diversite', choix: 'rarefaction', autres: ['indices'] },

  // A density or an abundance from the distances of the animals detected (distance sampling); the
  // distances' histogram first, for their shape and the truncation
  { question: 'densite', choix: 'distance', autres: ['statistiques'] },
];

const vaut = (attendu, valeur) => (Array.isArray(attendu) ? attendu.includes(valeur) : attendu === valeur);
const valeurContexte = (c, k) => (c[k] === undefined ? false : c[k]);
// the first rule that matches the context, or null (no question yet, or no kind of response)
export function regle(c) {
  return REGLES.find((r) => vaut(r.question, c.question) && (r.but === undefined || vaut(r.but, c.but))
    && (r.reponse === undefined || vaut(r.reponse, c.reponse))
    && Object.entries(r.si || {}).every(([k, v]) => vaut(v, valeurContexte(c, k)))) || null;
}
// The analysis recommended and its alternatives (those the context allows: CATALOGUE[id].si).
// Counts in doubt (comptageIncertain) are offered the linear model too, the other family.
export function recommander(c) {
  const r = regle(c);
  if (!r) return null;
  const possible = (id) => Object.entries(CATALOGUE[id].si || {}).every(([k, v]) => vaut(v, valeurContexte(c, k)));
  const autres = r.autres.concat(c.comptageIncertain && !r.autres.includes('normal') ? ['normal'] : []);
  return { choix: r.choix, autres: autres.filter((id) => id !== r.choix && possible(id)), regle: r };
}

// ------------------------------------------------------------ the analyses

// What each analysis opens: 'modele' New model (type: its type; famille: the distribution of a
// regression, 'reponse' from the kind of response, 'especes' from the species), 'par_reponse' New
// model for the first species then Model › One model per response, 'vue' a view of the project
// (vue: its name in VUES), 'variable' Edit › New variable with its formula. si: what the context
// must say for it to be offered as an alternative.
export const CATALOGUE = {
  // regressions (moteur/R/regression.R): LM, or LMM with the site's random effect; GLM and GAM by mgcv
  normal: { ouvre: 'modele', type: 'LM', famille: 'gaussienne' },
  lm_log: { ouvre: 'modele', type: 'LM', famille: 'gaussienne', log: true },
  glm_poisson: { ouvre: 'modele', type: 'GLM', famille: 'poisson' },
  glm_nb: { ouvre: 'modele', type: 'GLM', famille: 'binomiale_negative' },
  glm_zip: { ouvre: 'modele', type: 'GLM', famille: 'poisson_zi' },
  glm_binomial: { ouvre: 'modele', type: 'GLM', famille: 'binomiale' },
  glm_beta: { ouvre: 'modele', type: 'GLM', famille: 'beta' },
  glm_gamma: { ouvre: 'modele', type: 'GLM', famille: 'gamma' },
  glm_tweedie: { ouvre: 'modele', type: 'GLM', famille: 'tweedie' },
  gam: { ouvre: 'modele', type: 'GAM', famille: 'reponse', courbes: true },
  gam_spatial: { ouvre: 'modele', type: 'GAM', famille: 'reponse', spatial: true },
  par_reponse: { ouvre: 'par_reponse', type: 'GLM', famille: 'especes' },
  // components (their number chosen by cross-validation)
  acp: { ouvre: 'modele', type: 'PCA' },
  pls: { ouvre: 'modele', type: 'PLS' },
  opls: { ouvre: 'modele', type: 'OPLS' },
  plsda: { ouvre: 'modele', type: 'PLS-DA' },
  oplsda: { ouvre: 'modele', type: 'OPLS-DA', si: { deuxClasses: true } },
  classes: { ouvre: 'modele', type: 'CLASSES' },
  // the other analyses (dialogues.js: ANALYSES, registered by the vues_*.js modules)
  arbre: { ouvre: 'modele', type: 'TREE' },
  lda: { ouvre: 'modele', type: 'LDA' },
  kmeans: { ouvre: 'modele', type: 'KMEANS' },
  foret: { ouvre: 'modele', type: 'FOREST' },
  boost: { ouvre: 'modele', type: 'BOOST' },
  rda: { ouvre: 'modele', type: 'RDA' },
  cca: { ouvre: 'modele', type: 'CCA' },
  ca: { ouvre: 'modele', type: 'CA' },
  pcoa: { ouvre: 'modele', type: 'PCOA' },
  nmds: { ouvre: 'modele', type: 'NMDS' },
  permanova: { ouvre: 'modele', type: 'PERMANOVA' },
  mantel: { ouvre: 'modele', type: 'MANTEL' },
  indval: { ouvre: 'modele', type: 'INDVAL' },
  trim: { ouvre: 'modele', type: 'TRIM' },
  distance: { ouvre: 'modele', type: 'DISTANCE' },
  // the project's views (app.ouvrirVue)
  tests_deux: { ouvre: 'vue', vue: 'tests' },
  tests_apparies: { ouvre: 'vue', vue: 'tests', si: { unParSite: true } },
  tests_classes: { ouvre: 'vue', vue: 'tests' },
  correlations: { ouvre: 'vue', vue: 'tests' },
  baci: { ouvre: 'vue', vue: 'tests' },
  series: { ouvre: 'vue', vue: 'series' },
  carte: { ouvre: 'vue', vue: 'carte' },
  autocorrelation: { ouvre: 'vue', vue: 'autocorrelation_spatiale' },
  rarefaction: { ouvre: 'vue', vue: 'rarefaction' },
  statistiques: { ouvre: 'vue', vue: 'statistiques' },
  // Edit › New variable, its formula written
  indices: { ouvre: 'variable' },
};
const REGRESSIONS = Object.keys(CATALOGUE).filter((k) => ['LM', 'GLM', 'GAM'].includes(CATALOGUE[k].type));
const COMPOSANTES = ['acp', 'pls', 'opls', 'plsda', 'oplsda', 'classes'];

// Sentences added to an analysis (pour: its ids) when the context says so (si, as in REGLES).
export const NOTES = [
  { si: { comptageIncertain: true }, pour: ['glm_poisson', 'glm_nb', 'glm_zip', 'trim', 'boost', 'gam'], cle: 'qa_note_comptage_incertain' },
  { si: { asymetrique: true, question: 'effet' }, pour: ['normal', 'gam', 'gam_spatial'], cle: 'qa_note_asymetrie_residus' },
  { si: { repetes: true }, pour: REGRESSIONS, cle: 'qa_note_site' },
  { si: { repetes: true }, pour: ['pls', 'opls', 'plsda', 'oplsda', 'arbre', 'lda', 'foret', 'boost'], cle: 'qa_note_vc_site' },
  { si: { repetes: true }, pour: ['rda', 'cca', 'permanova', 'indval'], cle: 'qa_note_strates' },
  { si: { question: 'groupes' }, pour: REGRESSIONS, cle: 'qa_note_moyennes' },
  // (CAR1 needs the normal distribution: moteur/R/regression.R)
  { si: { annees: true }, pour: ['normal', 'lm_log'], cle: 'qa_note_annees' },
  { si: { question: 'tendance' }, pour: ['normal'], cle: 'qa_note_puissance' },
  { si: { tempsNonQuantitatif: true, question: 'tendance' }, pour: REGRESSIONS, cle: 'qa_note_temps_x' },
  { si: { annees: true, tempsNonQuantitatif: false, question: 'tendance' }, pour: REGRESSIONS, cle: 'qa_note_temps_centre' },
  { si: { bornes01: true }, pour: ['glm_beta', 'gam', 'gam_spatial'], cle: 'qa_note_bornes01' },
  { si: { coordonnees: true, question: 'effet' }, pour: REGRESSIONS, cle: 'qa_note_espace_residus' },
  { si: { especesEntieres: false }, pour: ['rarefaction'], cle: 'qa_note_especes_non_entieres' },
  { si: { unParSiteTemps: false }, pour: ['trim'], cle: 'qa_note_trim_resumer' },
  { si: { distanceSansAire: true }, pour: ['distance'], cle: 'qa_note_distance_sans_aire' },
  { si: { pointsSansEffort: true }, pour: ['distance'], cle: 'qa_note_distance_un_passage' },
];
export const notesPour = (id, c) => NOTES.filter((n) => n.pour.includes(id) && Object.entries(n.si).every(([k, v]) => vaut(v, valeurContexte(c, k)))).map((n) => n.cle);

// The distribution of a regression whose family follows the response (a GAM) or the species (one
// model per species): counts → negative binomial (Poisson without overdispersion nor many zeros),
// 0/1 → binomial, proportions → beta, a measurement with zeros → Tweedie, otherwise normal.
export function familleDe(c, especes = []) {
  if (c.question === 'especes' || c.reponse === 'especes') {
    if (especes.length && especes.every((f) => genre(f) === 'presence')) return 'binomiale';
    if (especes.length && especes.every((f) => f.entiers === true)) return 'binomiale_negative';
    return 'tweedie';
  }
  if (c.reponse === 'comptage') return c.zeros || c.surdispersion ? 'binomiale_negative' : 'poisson';
  if (c.reponse === 'presence') return 'binomiale';
  if (c.reponse === 'proportion') return 'beta';
  if (c.avecZeros) return 'tweedie';
  // (a skewed response stays normal: its residuals, not its raw values, say whether gamma is needed)
  return 'gaussienne';
}
// the log of the response: the one Variable statistics proposes (log10(x + C2)), else log10, or
// log10(x + 1) when it has zeros
const transfoLog = (f) => (f && f.suggestion ? f.suggestion : f && f.min !== null && f.min <= 0 ? 'log10:1' : 'log10');
const SYSTEMES_DEGRES = [4326, 4258, 4171];  // (as New model: longitudes and latitudes, a smooth on the sphere)

// A name as an R formula writes it (New variable): between backticks unless it is a plain name.
const nomR = (n) => (/^[\p{L}.][\p{L}\p{N}._]*$/u.test(n) ? n : `\`${n}\``);

// What analysis `id` opens, and with what: { id, ouvre, reglages (New model's settings, as
// dialogueModele takes them) | vue and loc (a view's settings) | variable ({ nom, formule }),
// especes (one model per response: the responses to tick), manque (what is left to choose, keys
// qa_manque_<k>), rempli (what is filled in: { cle, noms } or { cle, texte: a key }) }.
// options.langue: the language of a formula written (New variable's function names).
export function construire(id, R, F, p, c = contexte(R, F, p), options = {}) {
  const e = CATALOGUE[id];
  const nom = (u) => (F.get(u) || {}).nom || String(u);
  const noms = (us) => liste(us).map(nom);
  const manque = [];
  const rempli = [];
  const res = { id, ouvre: e.ouvre, manque, rempli };
  const y = R.y !== null && R.y !== undefined && F.get(R.y) ? R.y : null;
  const qualY = y !== null && F.get(y).qualitative;
  const especes = liste(R.especes).filter((u) => F.get(u));
  const groupe = R.groupe !== null && R.groupe !== undefined && F.get(R.groupe) ? R.groupe : null;
  const site = c.repetes ? specDialogue(R.site, p) : null;
  const tempsUid = uidDe(R.temps) !== null && F.get(uidDe(R.temps)) && !F.get(uidDe(R.temps)).qualitative ? uidDe(R.temps) : null;
  const x = liste(R.x).filter((u) => F.get(u) && u !== y && !especes.includes(u));
  // (the groups rather than the X: those compared, or those species characterize)
  const parGroupe = c.question === 'groupes' || c.but === 'indicatrices';
  const ajouter = (cle, valeurs) => {
    const vs = liste(valeurs).filter((v) => v !== '' && v !== null && v !== undefined);
    if (vs.length) rempli.push({ cle, noms: vs });
  };

  if (e.ouvre === 'modele' || e.ouvre === 'par_reponse') {
    const r = { type: e.type };
    res.reglages = r;
    const vc = () => { if (site) { r.vc = site; ajouter('qa_rempli_vc', [nomColonne(R.site, p)]); } };
    if (REGRESSIONS.includes(id)) {
      const reponse = id === 'par_reponse' ? especes[0] ?? null : y;
      if (reponse === null) manque.push(id === 'par_reponse' ? 'especes' : 'y');
      else if (F.get(reponse).qualitative) manque.push('y_quantitative');
      r.y = reponse === null ? [] : [reponse];
      // X: the groups compared, the time of a trend, or the explanatory variables
      r.x = c.question === 'groupes' ? (groupe !== null ? [groupe] : []) : c.question === 'tendance' ? (tempsUid !== null ? [tempsUid] : [])
        : c.question === 'espace' ? [] : x;
      if (c.question === 'groupes' && groupe === null) manque.push('groupe');
      if (c.question === 'tendance' && !R.temps) manque.push('temps');
      if (r.type === 'LM' && site) r.type = 'LMM';
      if (r.type !== 'LM' && r.type !== 'LMM') r.famille = e.famille === 'reponse' ? familleDe(c) : e.famille === 'especes' ? familleDe(c, especes.map((u) => F.get(u))) : e.famille;
      ajouter('qa_rempli_y', noms(r.y));
      ajouter('qa_rempli_x', noms(r.x));
      // (the time of a trend centred on its mean, its own scaling, the others in their units: crossed
      // with a factor, each category's coefficient is then its difference at the mean year, not in
      // year 0, far from the data)
      if (c.question === 'tendance' && tempsUid !== null && r.x.includes(tempsUid)) {
        Object.assign(r, { echelle: 'none', echelles: { [tempsUid]: 'Ctr' } });
        ajouter('qa_rempli_centree', noms([tempsUid]));
      }
      if (site) { r.aleatoires = [site]; ajouter('qa_rempli_aleatoire', [nomColonne(R.site, p)]); }
      vc();
      if (r.famille) rempli.push({ cle: 'qa_rempli_loi', texte: `famille_${r.famille}` });
      if (e.courbes) {
        r.lisses = r.x.filter((u) => !F.get(u).qualitative && !peuDeValeurs(F.get(u)));
        ajouter('qa_rempli_courbes', noms(r.lisses));
      }
      if (e.log && reponse !== null) {
        r.transformations = { [reponse]: transfoLog(F.get(reponse)) };
        rempli.push({ cle: 'qa_rempli_transformation', noms: [r.transformations[reponse].replace(/^log10:(.*)$/, 'log10(x + $1)')] });
      }
      if (e.spatial) {
        if (p.coordonnees) {
          r.lisse_spatial = { variables: [Number(p.coordonnees.x), Number(p.coordonnees.y)], base: SYSTEMES_DEGRES.includes(Number(p.coordonnees.systeme)) ? 'sos' : 'tp' };
          ajouter('qa_rempli_spatial', noms(r.lisse_spatial.variables));
        } else manque.push('coordonnees');
      }
      if (id === 'par_reponse') {
        res.especes = especes;
        if (especes.length < 2) manque.push('especes');
        ajouter('qa_rempli_especes', [especesTexte(especes, nom)]);
      }
      return res;
    }
    if (COMPOSANTES.includes(id)) {
      r.auto = true;  // (the number of components by cross-validation)
      if (id === 'acp') {
        r.x = liste(R.variables).filter((u) => F.get(u));
        if (r.x.length < 2) manque.push('variables');
        ajouter('qa_rempli_x', [especesTexte(r.x, nom)]);
      } else {
        r.x = x;
        if (!x.length) manque.push('x');
        const classe = ['plsda', 'oplsda', 'classes'].includes(id);
        if (classe) {
          if (y !== null && qualY) r.classe = y; else manque.push('classe');
          ajouter('qa_rempli_classe', noms(r.classe ?? []));
        } else {
          r.y = y !== null && !qualY ? [y] : [];
          if (!r.y.length) manque.push('y');
          ajouter('qa_rempli_y', noms(r.y));
        }
        ajouter('qa_rempli_x', [especesTexte(x, nom)]);
        vc();
      }
      rempli.push({ cle: 'qa_rempli_composantes', texte: 'qa_auto' });
      return res;
    }
    const speciesY = () => {
      r.y = especes;
      if (especes.length < 2) manque.push('especes');
      ajouter('qa_rempli_especes', [especesTexte(especes, nom)]);
    };
    switch (id) {
      case 'arbre': case 'foret': case 'boost': case 'lda': {
        r.x = x;
        if (!x.length) manque.push('x');
        if (y === null) manque.push(id === 'lda' ? 'classe' : 'y');
        else if (qualY && id !== 'boost') r.classe = y;
        else if (id === 'lda') manque.push('classe');
        else if (qualY) manque.push('y_quantitative');
        else r.y = [y];
        if (id === 'boost') {
          r.options = { loi: c.reponse === 'presence' ? 'binomiale' : c.reponse === 'comptage' ? 'poisson' : 'gaussienne' };
          rempli.push({ cle: 'qa_rempli_loi', texte: `famille_${r.options.loi}` });
        }
        ajouter(r.classe !== undefined ? 'qa_rempli_classe' : 'qa_rempli_y', noms(r.classe !== undefined ? [r.classe] : r.y || []));
        ajouter('qa_rempli_x', [especesTexte(x, nom)]);
        vc();
        break;
      }
      case 'kmeans':
        r.x = liste(R.variables).filter((u) => F.get(u));
        if (r.x.length < 2) manque.push('variables');
        ajouter('qa_rempli_x', [especesTexte(r.x, nom)]);
        break;
      case 'rda': case 'cca':
        speciesY();
        r.x = x;
        if (!x.length) manque.push('x');
        ajouter('qa_rempli_x', noms(x));
        if (id === 'rda') {
          r.options = { transformation: 'hellinger' };  // (abundances: Legendre and Gallagher 2001)
          rempli.push({ cle: 'qa_rempli_transformation', texte: 'qa_hellinger' });
        }
        break;
      case 'ca':
        r.x = especes;
        if (especes.length < 2) manque.push('especes');
        ajouter('qa_rempli_especes', [especesTexte(especes, nom)]);
        break;
      case 'pcoa': case 'nmds':
        speciesY();
        r.x = parGroupe ? (groupe !== null ? [groupe] : []) : x;  // (fitted onto the ordination afterwards: they can be left out)
        ajouter('qa_rempli_x_ajustees', noms(r.x));
        break;
      case 'permanova':
        speciesY();
        r.x = parGroupe ? (groupe !== null ? [groupe] : []) : x;
        if (!r.x.length) manque.push(parGroupe ? 'groupe' : 'x');
        ajouter('qa_rempli_x', noms(r.x));
        break;
      case 'mantel':
        speciesY();
        if (p.coordonnees) {
          r.options = { distance_x: 'geographique' };  // (the geographic distance between the observations)
          rempli.push({ cle: 'qa_rempli_distance_x', texte: 'qa_distance_geographique' });
        } else {
          r.x = x;
          if (!x.length) manque.push('coordonnees');
          ajouter('qa_rempli_x', noms(x));
        }
        break;
      case 'indval':
        speciesY();
        if (groupe !== null) r.classe = groupe; else manque.push('groupe');
        ajouter('qa_rempli_groupes', noms(groupe !== null ? [groupe] : []));
        break;
      case 'trim': {
        // (the site and the time even without the box ticked: TRIM needs them; empty, its fields guess them)
        if (y === null) manque.push('y'); else if (qualY) manque.push('y_quantitative'); else r.y = [y];
        r.options = {};
        const s = specDialogue(R.site, p);
        const tp = specDialogue(R.temps, p);
        if (s) r.options.site = s; else manque.push('site');
        if (tp) r.options.temps = tp; else manque.push('temps');
        ajouter('qa_rempli_y', noms(r.y || []));
        ajouter('qa_rempli_site', [nomColonne(R.site, p)].filter(Boolean));
        ajouter('qa_rempli_temps', [nomColonne(R.temps, p)].filter(Boolean));
        break;
      }
      case 'distance': {
        // the distances as Y, lines or points, and the columns of the flat file their names show
        // (vues_distance.js). With the transect given, New model guesses none of them itself: the
        // others go with it, found or not; without it, its fields guess them all, as here.
        if (y === null) manque.push('distances'); else if (qualY) manque.push('y_quantitative'); else r.y = [y];
        const points = R.transect === 'point';
        const k = colonnesDistance(p);
        r.options = { transect: points ? 'point' : 'ligne' };
        if (k.echantillon) Object.assign(r.options, { echantillon: specDialogue(k.echantillon, p), effort: k.effort, strate: specDialogue(k.strate, p), aire: k.aire, taille: k.taille });
        else manque.push('echantillon');
        if (!points && k.effort === null) manque.push('effort');
        manque.push(points ? 'unites_points' : 'unites_lignes');  // (what no column says: m, km and km² until changed)
        ajouter('qa_rempli_y', noms(r.y || []));
        rempli.push({ cle: 'qa_rempli_plan', texte: points ? 'qa_transects_point' : 'qa_transects_ligne' });
        ajouter('qa_rempli_transect', [nomColonne(k.echantillon, p)]);
        ajouter('qa_rempli_effort', k.effort !== null ? noms([k.effort]) : []);
        ajouter('qa_rempli_strate', [nomColonne(k.strate, p)]);
        ajouter('qa_rempli_aire', k.aire !== null ? noms([k.aire]) : []);
        ajouter('qa_rempli_taille', k.taille !== null ? noms([k.taille]) : []);
        break;
      }
      default:
        break;
    }
    return res;
  }

  if (e.ouvre === 'vue') {
    res.vue = e.vue;
    const loc = {};
    res.loc = loc;
    switch (id) {
      case 'tests_deux': case 'tests_apparies':
        loc.mode = 'deux';
        loc.deux = {};
        if (groupe !== null) loc.deux.x = groupe; else manque.push('groupe');
        if (y !== null) loc.deux.y = y; else manque.push('y');
        ajouter('qa_rempli_groupes', noms(groupe !== null ? [groupe] : []));
        ajouter('qa_rempli_y', noms(y !== null ? [y] : []));
        if (id === 'tests_apparies') {
          if (R.site) { loc.deux.apparier = valeurVue(R.site); ajouter('qa_rempli_apparier', [nomColonne(R.site, p)]); } else manque.push('site');
        }
        break;
      case 'tests_classes':
        loc.mode = 'classes';
        if (groupe !== null) {
          loc.classe = groupe;
          loc.comparaison = F.get(groupe).ncat === 2 ? 'deux' : 'toutes';
          ajouter('qa_rempli_groupes', noms([groupe]));
        } else manque.push('groupe');
        break;
      case 'correlations':
        loc.mode = 'correlations';
        break;
      case 'baci': {
        loc.mode = 'baci';
        loc.baci = {};
        const unite = R.unite || R.site;
        if (y !== null && !qualY) loc.baci.y = y; else manque.push('y');
        if (R.temps) loc.baci.temps = valeurVue(R.temps); else manque.push('temps');
        if (unite) loc.baci.unite = valeurVue(unite); else manque.push('unite');
        ajouter('qa_rempli_y', noms(loc.baci.y !== undefined ? [y] : []));
        ajouter('qa_rempli_temps', [nomColonne(R.temps, p)].filter(Boolean));
        ajouter('qa_rempli_unite', [nomColonne(unite, p)].filter(Boolean));
        break;
      }
      case 'series':
        if (y !== null && !qualY) loc.variable = y; else manque.push('y');
        if (R.temps) loc.temps = valeurVue(R.temps); else manque.push('temps');
        if (c.repetes) loc.lignes = valeurVue(R.site);
        loc.tendances = true;
        ajouter('qa_rempli_y', noms(loc.variable !== undefined ? [y] : []));
        ajouter('qa_rempli_temps', [nomColonne(R.temps, p)].filter(Boolean));
        ajouter('qa_rempli_lignes', c.repetes ? [nomColonne(R.site, p)] : []);
        rempli.push({ cle: 'qa_rempli_tendances', texte: 'qa_oui' });
        break;
      case 'carte':
        if (y !== null) { loc.couleur = `v${y}`; ajouter('qa_rempli_couleur', noms([y])); }
        break;
      case 'autocorrelation':
        if (y !== null && !qualY) { loc.source = `v${y}`; ajouter('qa_rempli_y', noms([y])); } else manque.push('y');
        if (!p.coordonnees) manque.push('coordonnees');
        break;
      case 'rarefaction':
        loc.especes = especes;
        if (especes.length < 2) manque.push('especes');
        loc.mode = c.especesEntieres === false ? 'accumulation' : 'rarefaction';
        if (groupe !== null) loc.groupe = String(groupe);
        ajouter('qa_rempli_especes', [especesTexte(especes, nom)]);
        ajouter('qa_rempli_groupes', noms(groupe !== null ? [groupe] : []));
        break;
      default:
        break;
    }
    return res;
  }

  if (e.ouvre === 'variable') {
    // the richness of each survey over the species: a range in the table's order when they
    // follow each other there (richesse(a:b)), otherwise listed (variables.R: indice_diversite)
    if (especes.length < 2) manque.push('especes');
    const ordre = liste(p.variables).map((v) => v.uid);
    const rangs = especes.map((u) => ordre.indexOf(u)).sort((a, b) => a - b);
    const suite = rangs.length > 1 && rangs.every((k, i) => !i || k === rangs[i - 1] + 1);
    const fonction = options.langue === 'en' ? 'richness' : 'richesse';
    const nomsR = (us) => us.map((u) => nomR(nom(u)));
    const args = suite ? `${nomsR([ordre[rangs[0]]])[0]}:${nomsR([ordre[rangs[rangs.length - 1]]])[0]}` : nomsR(especes).join(', ');
    res.variable = { nom: fonction, formule: especes.length ? `${fonction}(${args})` : '' };
    rempli.push({ cle: 'qa_rempli_formule', noms: [res.variable.formule] });
    return res;
  }
  return res;
}
// a list of species or variables in short: the first three and how many in all
function especesTexte(us, nom) {
  const n = us.length;
  if (!n) return '';
  return n <= 4 ? us.map(nom).join(', ') : `${us.slice(0, 3).map(nom).join(', ')}… (${n})`;
}

// ------------------------------------------------------------ the strings

export const TEXTES_CHOIX = {
  fr: {
    quelle_analyse: 'Quelle analyse ?…',
    qa_titre_question: 'Votre question', qa_titre_reponse: 'La réponse', qa_titre_plan: 'Le dispositif', qa_titre_variables: 'Les variables',
    qa_q_groupes: 'Comparer des groupes ou des périodes', qa_ex_groupes: 'des habitats, des traitements, avant et après',
    qa_q_effet: 'L’effet de variables sur une réponse', qa_ex_effet: 'la densité selon la profondeur et l’habitat',
    qa_q_especes: 'Beaucoup d’espèces à la fois', qa_ex_especes: 'la composition des communautés, un modèle par espèce, les espèces indicatrices',
    qa_q_tendance: 'Une tendance dans le temps, un indice de suivi', qa_ex_tendance: 'des comptages année après année',
    qa_q_densite: 'Une densité ou un effectif (échantillonnage par distance)', qa_ex_densite: 'des animaux vus depuis des lignes ou des points, à une distance notée',
    qa_q_espace: 'Où : cartes, autocorrélation spatiale', qa_ex_espace: 'où les valeurs sont fortes, si des points proches se ressemblent',
    qa_q_explorer: 'Explorer ou résumer un tableau', qa_ex_explorer: 'quelles variables vont ensemble, quelles observations se ressemblent',
    qa_q_predire: 'Prédire ou classer', qa_ex_predire: 'une valeur ou une classe pour de nouvelles observations',
    qa_q_diversite: 'Diversité et richesse en espèces', qa_ex_diversite: 'combien d’espèces, comparées à effort égal',
    qa_q_baci: 'Avant/après avec des témoins (BACI)', qa_ex_baci: 'l’effet d’une gestion sur des unités, comparées à des unités témoins',
    qa_but: 'Pour les espèces', qa_but_composition: 'comment la composition change (avec l’environnement, entre groupes)',
    qa_but_par_espece: 'un modèle par espèce', qa_but_indicatrices: 'les espèces qui caractérisent chaque groupe',
    qa_r_mesure: 'Une mesure', qa_rex_mesure: 'longueur, biomasse, concentration',
    qa_r_comptage: 'Un comptage', qa_rex_comptage: 'des individus, des nids',
    qa_r_presence: 'Une présence-absence (0 ou 1)', qa_rex_presence: 'une espèce vue ou non',
    qa_r_proportion: 'Une proportion entre 0 et 1', qa_rex_proportion: 'recouvrement, survie',
    qa_r_classe: 'Une classe', qa_rex_classe: 'un habitat, un état, une espèce',
    qa_r_especes: 'Un tableau d’espèces', qa_rex_especes: 'une colonne par espèce',
    qa_r_distances: 'Des distances aux animaux détectés', qa_rex_distances: 'une ligne par détection',
    qa_zeros: 'avec beaucoup de zéros', qa_zeros_aide: 'Plus de zéros qu’une loi de Poisson de même moyenne n’en donne',
    qa_reponse_variable: 'Réponse', qa_variables_explorer: 'Variables du tableau', qa_choisir: '— choisir —',
    qa_reponse_facultative: '— aucune —',
    qa_faits_bornes: 'de {min} à {max}', qa_faits_entiers: 'des entiers', qa_faits_decimales: 'pas tous entiers',
    qa_faits_mesure_arrondie: 'une mesure arrondie à son unité, d’après son unité ou son nom',
    qa_faits_zeros: '{p} % de zéros', qa_faits_poisson: 'une loi de Poisson de même moyenne en donnerait {q} %',
    qa_faits_manquantes: '{p} % de valeurs manquantes', qa_faits_categories: '{n} catégories', qa_faits_categorie_une: '1 catégorie', qa_faits_asymetrie: 'asymétrique (quelques très grandes valeurs)',
    qa_faits_inconnus: 'les statistiques des variables n’ont pas pu être lues', qa_faits_propose: 'Proposé d’après les données : {faits}.',
    qa_especes_proposees: 'Proposées : les colonnes de nombres positifs ou nuls qui ont des zéros.', qa_n_cochees: '{n} sur {total}',
    qa_d_repetes: 'Les mêmes sites ou placettes mesurés plusieurs fois', qa_d_repetes_aide: 'Un effet aléatoire par site, la validation croisée par site',
    qa_d_annees: 'Des années, ou des dates', qa_d_baci: 'Un dispositif BACI : des unités d’impact et des témoins, avant et après',
    qa_d_coordonnees: 'Des coordonnées spatiales',
    qa_d_transects: 'Des lignes ou des points', qa_transects_ligne: 'Des lignes (transects linéaires)', qa_transects_point: 'Des points (points d’écoute)',
    qa_site: 'Site', qa_temps: 'Temps', qa_unite: 'Unité', qa_groupe: 'Groupes', qa_x: 'Variables explicatives (X)', qa_especes: 'Espèces',
    qa_coordonnees_projet: 'celles du projet : {x}, {y}', qa_coordonnees_proposees: 'proposées : {x}, {y}, à choisir dans Graphiques › Carte',
    qa_coordonnees_aucune: 'aucune : à choisir dans Graphiques › Carte',
    qa_recommandee: 'Analyse conseillée', qa_autres: 'Autres possibilités', qa_pourquoi: 'Pourquoi', qa_conditions: 'Conditions', qa_rempli: 'Rempli',
    qa_a_completer: 'À compléter : {liste}.', qa_avec_site: '{nom}, avec un effet aléatoire par {site}',
    qa_choisir_question: 'Choisissez votre question : l’analyse conseillée s’affiche ici, avec pourquoi, ses conditions et un bouton qui ouvre la bonne fenêtre, déjà remplie.',
    qa_choisir_reponse: 'Choisissez la réponse, ou son type.',
    qa_conseil: 'Un conseil, pas une règle : les conditions se vérifient après l’ajustement (Résumé › Conditions du modèle).',
    qa_ouvrir_modele: 'Ouvrir Nouveau modèle, déjà rempli', qa_ouvrir_par_reponse: 'Ouvrir Nouveau modèle, puis Un modèle par réponse',
    qa_ouvrir_vue: 'Ouvrir {vue}', qa_ouvrir_variable: 'Ouvrir Nouvelle variable, déjà remplie', qa_ouvrir_court: 'Ouvrir',
    qa_manque_y: 'la réponse', qa_manque_y_quantitative: 'une réponse quantitative', qa_manque_x: 'les variables explicatives (X)', qa_manque_groupe: 'les groupes',
    qa_manque_classe: 'la classe (une réponse qualitative)', qa_manque_especes: 'les espèces (deux au moins)', qa_manque_site: 'le site', qa_manque_temps: 'le temps',
    qa_manque_unite: 'l’unité', qa_manque_variables: 'les variables (deux au moins)', qa_manque_coordonnees: 'les coordonnées du projet (Graphiques › Carte)',
    qa_manque_distances: 'les distances (en Y)', qa_manque_echantillon: 'le transect ou le point de chaque ligne', qa_manque_effort: 'l’effort de chaque transect (sa longueur)',
    qa_manque_unites_lignes: 'les unités des distances, des longueurs et des aires (m, km et km² sinon)', qa_manque_unites_points: 'les unités des distances et des aires (m et km² sinon)',
    qa_rempli_y: 'Y', qa_rempli_classe: 'Classe', qa_rempli_x: 'X', qa_rempli_x_ajustees: 'X ajustées sur le plan', qa_rempli_especes: 'Espèces',
    qa_rempli_aleatoire: 'Effet aléatoire', qa_rempli_vc: 'Groupes de validation croisée', qa_rempli_loi: 'Loi', qa_rempli_courbes: 'Courbes',
    qa_rempli_transformation: 'Transformation', qa_rempli_spatial: 'Lissage spatial', qa_rempli_site: 'Site', qa_rempli_temps: 'Temps',
    qa_rempli_centree: 'Centrée sur sa moyenne',
    qa_rempli_groupes: 'Groupes', qa_rempli_apparier: 'Apparié par', qa_rempli_unite: 'Unité', qa_rempli_lignes: 'Lignes', qa_rempli_tendances: 'Tendances',
    qa_rempli_couleur: 'Couleur', qa_rempli_formule: 'Formule', qa_rempli_distance_x: 'Distance des X', qa_rempli_composantes: 'Composantes',
    qa_rempli_plan: 'Plan', qa_rempli_transect: 'Transect ou point', qa_rempli_effort: 'Effort', qa_rempli_strate: 'Strate', qa_rempli_aire: 'Aire',
    qa_rempli_taille: 'Taille des groupes',
    qa_auto: 'automatiques', qa_oui: 'oui', qa_hellinger: 'Hellinger', qa_distance_geographique: 'géographique',

    qa_nom_normal: 'Régression linéaire', qa_nom_lm_log: 'Régression linéaire sur le logarithme de la réponse',
    qa_nom_lmm: 'Modèle linéaire mixte', qa_nom_lmm_log: 'Modèle linéaire mixte sur le logarithme de la réponse',
    qa_nom_glm_poisson: 'GLM de Poisson', qa_nom_glm_nb: 'GLM binomial négatif', qa_nom_glm_zip: 'GLM de Poisson à excès de zéros',
    qa_nom_glm_binomial: 'Régression logistique (GLM binomial)', qa_nom_glm_beta: 'Régression bêta', qa_nom_glm_gamma: 'GLM gamma',
    qa_nom_glm_tweedie: 'GLM de Tweedie', qa_nom_gam: 'Modèle additif (GAM), avec des courbes', qa_nom_gam_spatial: 'Modèle additif avec un lissage spatial',
    qa_nom_par_reponse: 'Un modèle par espèce', qa_nom_acp: 'ACP (analyse en composantes principales)', qa_nom_pls: 'Régression PLS', qa_nom_opls: 'OPLS',
    qa_nom_plsda: 'PLS-DA (PLS discriminante)', qa_nom_oplsda: 'OPLS-DA', qa_nom_classes: 'Modèles de classe (une ACP par classe)',
    qa_nom_arbre: 'Arbre de décision', qa_nom_lda: 'Analyse discriminante linéaire (LDA)', qa_nom_kmeans: 'k-moyennes', qa_nom_foret: 'Forêt aléatoire',
    qa_nom_boost: 'Arbres boostés (BRT)', qa_nom_rda: 'RDA (analyse de redondance)', qa_nom_cca: 'CCA (analyse canonique des correspondances)',
    qa_nom_ca: 'Analyse des correspondances (CA)', qa_nom_pcoa: 'PCoA (analyse en coordonnées principales)',
    qa_nom_nmds: 'NMDS (positionnement multidimensionnel non métrique)', qa_nom_permanova: 'PERMANOVA (analyse de variance par permutation)',
    qa_nom_mantel: 'Test de Mantel', qa_nom_indval: 'Espèces indicatrices (IndVal)', qa_nom_trim: 'TRIM : indices et tendance de comptages',
    qa_nom_tests_deux: 'Test entre deux variables', qa_nom_tests_apparies: 'Test apparié par site', qa_nom_tests_classes: 'Tests entre classes, variable par variable',
    qa_nom_correlations: 'Matrice des corrélations', qa_nom_baci: 'Avant/après avec témoins (BACI)', qa_nom_series: 'Séries et tendances (Mann–Kendall, pente de Sen)',
    qa_nom_carte: 'Carte', qa_nom_autocorrelation: 'Autocorrélation spatiale (I de Moran)', qa_nom_rarefaction: 'Raréfaction et accumulation des espèces',
    qa_nom_statistiques: 'Statistiques des variables', qa_nom_indices: 'Indices de diversité (nouvelle variable)',
    qa_nom_distance: 'Échantillonnage par distance (densité, effectif)',

    qa_pourquoi_normal: 'Une mesure continue dont les écarts au modèle sont à peu près normaux : la régression linéaire donne l’effet de chaque variable, dans ses unités.',
    qa_conditions_normal: 'Des résidus à peu près normaux, de dispersion constante : le Résumé les vérifie (Conditions du modèle).',
    qa_pourquoi_lm_log: 'Une mesure positive et asymétrique : son logarithme se rapproche d’une loi normale, et les effets se lisent en rapports.',
    qa_conditions_lm_log: 'Avec des zéros, log10(y + C) : la constante que propose Statistiques des variables.',
    qa_pourquoi_glm_poisson: 'Des comptages (des entiers positifs ou nuls) : la loi de Poisson, avec un lien log ; les effets se lisent en rapports.',
    qa_conditions_glm_poisson: 'Une variance égale à la moyenne : le Résumé signale une surdispersion, la binomiale négative la prend alors en compte.',
    qa_pourquoi_glm_nb: 'Des comptages plus dispersés qu’une loi de Poisson ne le permet (une variance bien plus grande que la moyenne, beaucoup de zéros) : la binomiale négative estime cette dispersion.',
    qa_conditions_glm_nb: 'Des entiers positifs ou nuls. Beaucoup de zéros ne veut pas toujours dire excès de zéros (Warton 2005) : comparez avec la Poisson à excès de zéros par l’AIC (Prédiction › Comparer les modèles).',
    qa_pourquoi_glm_zip: 'Des comptages avec plus de zéros qu’une loi de Poisson n’en donne, quand un zéro peut venir d’une vraie absence : la Poisson à excès de zéros.',
    qa_conditions_glm_zip: 'Au moins un zéro ; comparez avec la binomiale négative par l’AIC ; le Résumé compare les zéros observés à ceux attendus.',
    qa_pourquoi_glm_binomial: 'Une présence-absence (0 ou 1) : la régression logistique donne la probabilité de présence ; les effets se lisent en rapports de cotes.',
    qa_conditions_glm_binomial: 'Une réponse 0 ou 1 seulement, et assez de présences et d’absences (une dizaine de chaque par variable explicative).',
    qa_pourquoi_glm_beta: 'Une proportion continue entre 0 et 1 (un recouvrement, un taux) : la loi bêta, avec un lien logit.',
    qa_conditions_glm_beta: 'Des valeurs strictement entre 0 et 1.',
    qa_pourquoi_glm_gamma: 'Une mesure strictement positive et asymétrique : la loi gamma, avec un lien log, garde l’échelle d’origine.',
    qa_conditions_glm_gamma: 'Aucune valeur nulle ou négative ; une dispersion qui croît avec la moyenne.',
    qa_pourquoi_glm_tweedie: 'Une mesure positive avec des zéros (une densité, une biomasse) : la loi de Tweedie modélise à la fois les zéros et les valeurs positives.',
    qa_conditions_glm_tweedie: 'Des valeurs positives ou nulles ; sa puissance p est estimée (entre 1 et 2).',
    qa_pourquoi_gam: 'Quand l’effet d’une variable peut être courbe (un optimum, un seuil) : le modèle additif ajuste une courbe à chaque variable quantitative.',
    qa_conditions_gam: 'Assez d’observations pour chaque courbe (une vingtaine au moins) ; décochez la courbe d’une variable à l’effet droit.',
    qa_pourquoi_gam_spatial: 'Une réponse structurée dans l’espace : le modèle additif ajoute une surface lisse des coordonnées, qui prend ce que les variables n’expliquent pas.',
    qa_conditions_gam_spatial: 'Les coordonnées du projet (Graphiques › Carte) et assez de lieux différents.',
    qa_pourquoi_par_reponse: 'Le même modèle pour chaque espèce, les valeurs p corrigées sur les espèces : un modèle par réponse.',
    qa_conditions_par_reponse: 'Ajustez le modèle de la première espèce : la fenêtre Un modèle par réponse s’ouvre ensuite, les autres espèces cochées.',
    qa_pourquoi_acp: 'Résumer de nombreuses variables quantitatives en quelques axes : quelles variables vont ensemble, quelles observations se ressemblent.',
    qa_conditions_acp: 'Des variables mises à l’échelle (UV) ; Graphiques › Dendrogramme regroupe ensuite les observations.',
    qa_pourquoi_pls: 'Prédire une réponse à partir de nombreuses variables corrélées (des spectres, plus de variables que d’observations) : la PLS.',
    qa_conditions_pls: 'Le nombre de composantes est choisi par validation croisée ; regardez le Q2 et les VIP.',
    qa_pourquoi_opls: 'Comme la PLS, en séparant ce qui prédit la réponse de ce qui varie sans elle.',
    qa_conditions_opls: 'Une composante prédictive par Y ; la même prédiction que la PLS.',
    qa_pourquoi_plsda: 'Classer des observations à partir de nombreuses variables corrélées : la PLS discriminante.',
    qa_conditions_plsda: 'La validation croisée donne la part de bien classés.',
    qa_pourquoi_oplsda: 'Deux classes et de nombreuses variables : l’OPLS-DA sépare ce qui les distingue du reste.',
    qa_conditions_oplsda: 'Surtout pour deux classes ; le S-plot montre les variables qui les séparent.',
    qa_pourquoi_classes: 'Savoir si une observation ressemble à une classe connue : une ACP par classe, et le graphique de Coomans.',
    qa_conditions_classes: 'Assez d’observations dans chaque classe (une dizaine au moins).',
    qa_pourquoi_arbre: 'Des règles faciles à lire (« si la profondeur dépasse 3 m… ») et des seuils dans les unités des variables : l’arbre de décision.',
    qa_conditions_arbre: 'Assez d’observations (une centaine) ; un arbre est instable : la vue Stabilité de la première coupure le dit.',
    qa_pourquoi_lda: 'Séparer des classes connues par des combinaisons linéaires des variables : l’analyse discriminante linéaire.',
    qa_conditions_lda: 'Des variables à peu près normales et de dispersions semblables dans chaque classe.',
    qa_pourquoi_kmeans: 'Former des groupes d’observations qui se ressemblent, sans classe connue d’avance : les k-moyennes.',
    qa_conditions_kmeans: 'Le nombre de groupes k se choisit (la vue Choix de k aide) ; des variables mises à l’échelle.',
    qa_pourquoi_foret: 'Prédire au mieux, avec des effets non linéaires et des interactions sans les écrire : la forêt aléatoire.',
    qa_conditions_foret: 'Les observations avec une X manquante sont laissées de côté ; elle donne l’importance des variables, pas des coefficients.',
    qa_pourquoi_boost: 'Prédire une présence, un comptage ou une mesure avec des arbres ajoutés un à un (Elith et al. 2008) : les arbres boostés.',
    qa_conditions_boost: 'Le nombre d’arbres est choisi par validation croisée ; quelques minutes sur un grand tableau.',
    qa_pourquoi_rda: 'Combien de la composition des espèces les variables explicatives expliquent, et comment : la RDA, après la transformation de Hellinger (Legendre et Gallagher 2001).',
    qa_conditions_rda: 'Des espèces qui répondent à peu près linéairement, le long de gradients courts ; sinon la CCA. Tests par permutations.',
    qa_pourquoi_cca: 'Des espèces qui ont un optimum le long de gradients longs : l’analyse canonique des correspondances.',
    qa_conditions_cca: 'Des abondances positives ou nulles ; les espèces rares y pèsent beaucoup.',
    qa_pourquoi_ca: 'Les grandes structures d’un tableau d’espèces, abondances ou présences, sans variable explicative.',
    qa_conditions_ca: 'Des valeurs positives ou nulles ; les espèces rares y pèsent beaucoup.',
    qa_pourquoi_pcoa: 'Placer les observations d’après une distance entre elles (Bray-Curtis pour des abondances).',
    qa_conditions_pcoa: 'Une distance qui convient aux données ; les X sont ajustées sur le plan, après coup.',
    qa_pourquoi_nmds: 'Montrer la ressemblance des communautés en deux dimensions, d’après les rangs de leurs distances (Bray-Curtis) : le NMDS.',
    qa_conditions_nmds: 'Un stress au-dessous de 0,2 ; les X sont ajustées sur le plan, après coup.',
    qa_pourquoi_permanova: 'Tester si la composition diffère entre groupes ou selon des variables, sur une distance (Bray-Curtis) : la PERMANOVA.',
    qa_conditions_permanova: 'Des dispersions semblables entre les groupes : la vue Dispersion les teste.',
    qa_pourquoi_mantel: 'Savoir si des communautés proches dans l’espace se ressemblent plus : le test de Mantel entre la distance des espèces et la distance géographique.',
    qa_conditions_mantel: 'Les coordonnées du projet ; le corrélogramme le détaille par classe de distance.',
    qa_pourquoi_indval: 'Les espèces qui caractérisent chaque groupe (des habitats, des types de sites) : la valeur indicatrice, spécificité et fidélité.',
    qa_conditions_indval: 'Des groupes connus d’avance (une variable qualitative) ; les p corrigées sur les espèces.',
    qa_pourquoi_trim: 'Des comptages par site et par année, avec des comptages manquants : TRIM donne les indices annuels et la tendance, comme pour le suivi des oiseaux en Europe.',
    qa_conditions_trim: 'Une ligne par site et par année, des comptages entiers ; les comptages manquants sont imputés.',
    qa_pourquoi_tests_deux: 'Comparer deux variables avec le test que leurs types demandent, ses conditions vérifiées (t, ANOVA, Kruskal–Wallis, khi-deux…).',
    qa_conditions_tests_deux: 'Des observations indépendantes ; le résultat vient en une phrase prête pour un rapport.',
    qa_pourquoi_tests_apparies: 'Les mêmes sites dans chaque groupe : un test apparié les compare site par site (t apparié, Wilcoxon ; Friedman pour trois groupes ou plus).',
    qa_conditions_tests_apparies: 'Chaque site une fois par groupe ; les sites sans partenaire sont laissés de côté, et comptés.',
    qa_pourquoi_tests_classes: 'Chaque variable quantitative comparée entre les classes, les p corrigées sur les variables.',
    qa_conditions_tests_classes: 'Des observations indépendantes.',
    qa_pourquoi_correlations: 'Les corrélations de toutes les variables quantitatives deux à deux.',
    qa_conditions_correlations: 'Pearson pour des liens linéaires, Spearman sinon ; les p corrigées.',
    qa_pourquoi_baci: 'Une gestion changée sur des unités (l’impact) comparée à des unités témoins, avant et après : l’écart impact − témoins, avant contre après.',
    qa_conditions_baci: 'Cochez les unités d’impact et les témoins, donnez le temps de l’évènement ; « Ajuster le modèle mixte » tient compte des unités et de l’autocorrélation.',
    qa_pourquoi_series: 'Une variable au fil du temps : les courbes, et la tendance de chaque série (test de Mann–Kendall, pente de Sen).',
    qa_conditions_series: 'Au moins 4 temps par série ; des années qui se ressemblent (autocorrélation) rendent p trop petit.',
    qa_pourquoi_carte: 'Voir où sont les valeurs fortes et faibles : les observations sur une carte, colorées par la réponse.',
    qa_conditions_carte: 'Les coordonnées du projet : deux variables et leur système, choisis la première fois.',
    qa_pourquoi_autocorrelation: 'Savoir si des observations proches se ressemblent : l’I de Moran, le corrélogramme et le semi-variogramme.',
    qa_conditions_autocorrelation: 'Les coordonnées du projet ; pour les résidus d’un modèle, ouvrez-la depuis ce modèle.',
    qa_pourquoi_rarefaction: 'Comparer la richesse à effort égal : la raréfaction et l’accumulation des espèces, avec Chao1, ACE et les jackknifes.',
    qa_conditions_rarefaction: 'Des comptages entiers d’individus pour la raréfaction ; sinon l’accumulation seule.',
    qa_pourquoi_statistiques: 'Un premier regard : valeurs manquantes, minimum, maximum, asymétrie et la transformation conseillée de chaque variable.',
    qa_conditions_statistiques: 'Aucune.',
    qa_pourquoi_indices: 'Un indice par relevé (richesse, Shannon, Simpson, Pielou), en nouvelle variable, à comparer ensuite entre groupes ou à modéliser.',
    qa_conditions_indices: 'Des colonnes d’espèces, des nombres positifs ou nuls ; la richesse dépend de l’effort : voir la raréfaction.',
    qa_pourquoi_distance: 'On ne voit pas tous les animaux, et moins ceux qui sont loin : la fonction de détection, ajustée aux distances, donne la probabilité de détection, puis la densité et l’effectif de chaque strate (Buckland et al. 2001), comme le paquet Distance.',
    qa_conditions_distance: 'Une ligne par détection, sa distance en Y, et chaque transect sans détection gardé, sa distance vide ; tous les animaux sur la ligne ou le point vus ; une soixantaine de détections au moins. Ce qui change la détection (l’observateur, le vent, la taille des groupes) peut en être une covariable (Nouveau modèle › Covariables).',

    qa_note_asymetrie_residus: 'La réponse est asymétrique, mais ses X peuvent la rendre ainsi : c’est sur les résidus qu’on en juge, après l’ajustement (Résumé › Conditions du modèle). S’ils restent asymétriques, choisissez la loi gamma ou le log de la réponse, parmi les autres possibilités.',
    qa_note_comptage_incertain: 'Des entiers sans unité, lus comme des comptages : si ce sont des mesures arrondies à leur unité (des grammes, des centimètres), choisissez plutôt Une mesure, ou le modèle linéaire parmi les autres possibilités ; Édition › Unités et descriptions donne son unité à la variable.',
    qa_note_site: 'Les observations d’un même site se ressemblent : l’effet aléatoire par site en tient compte, et la validation croisée prédit chaque site sans lui.',
    qa_note_vc_site: 'La validation croisée par site juge le modèle sur des sites qu’il n’a jamais vus.',
    qa_note_strates: 'Les observations d’un même site ne sont pas indépendantes : des permutations dans les sites (onglet Général) si les X varient dans chaque site, sinon résumez d’abord par site (Édition › Résumer par groupe).',
    qa_note_moyennes: 'Puis Graphiques › Moyennes ajustées compare les groupes deux à deux (Tukey).',
    qa_note_annees: 'Des années qui se suivent se ressemblent : après l’ajustement, Graphiques › Autocorrélation des résidus, puis au besoin Ajuster avec CAR1.',
    qa_note_puissance: 'Graphiques › Puissance pour une tendance dit ensuite combien d’années il faut pour la détecter.',
    qa_note_temps_centre: 'Le temps est centré sur sa moyenne (son échelle Ctr) : la pente ne change pas, et si vous croisez le temps avec une variable qualitative (la tendance diffère-t-elle d’un habitat à l’autre ?), le coefficient de chaque catégorie est son écart à l’année moyenne, non en l’an 0, loin des données.',
    qa_note_temps_x: 'Le temps choisi n’est pas une variable quantitative : pour l’avoir en X, faites-en une (Édition › Nouvelle variable).',
    qa_note_bornes01: 'La réponse touche 0 ou 1, que la loi bêta refuse : ramenez-la d’abord dans l’intervalle, en le disant (Édition › Nouvelle variable : (y * (n - 1) + 0.5) / n pour n observations ; Smithson et Verkuilen 2006).',
    qa_note_espace_residus: 'Avec des coordonnées : après l’ajustement, Graphiques › Autocorrélation spatiale de ses résidus.',
    qa_note_especes_non_entieres: 'Les espèces ne sont pas toutes des comptages entiers : la raréfaction ne s’applique pas, l’accumulation oui.',
    qa_note_trim_resumer: 'Le tableau a plusieurs lignes par site et par année (des placettes, des passages) : TRIM n’en prend qu’une ; additionnez-les d’abord (Édition › Résumer par groupe, la somme par site et année).',
    qa_note_distance_sans_aire: 'Sans aire des strates (pas de colonne, ou des aires nulles), la densité seule, sans effectif.',
    qa_note_distance_un_passage: 'Sans colonne de passages, chaque point compte une visite.',
  },
  en: {
    quelle_analyse: 'Which analysis?…',
    qa_titre_question: 'Your question', qa_titre_reponse: 'The response', qa_titre_plan: 'The design', qa_titre_variables: 'The variables',
    qa_q_groupes: 'Compare groups or periods', qa_ex_groupes: 'habitats, treatments, before and after',
    qa_q_effet: 'The effect of variables on one response', qa_ex_effet: 'density against depth and habitat',
    qa_q_especes: 'Many species at once', qa_ex_especes: 'community composition, one model per species, indicator species',
    qa_q_tendance: 'A trend over time, a monitoring index', qa_ex_tendance: 'counts year after year',
    qa_q_densite: 'A density or an abundance (distance sampling)', qa_ex_densite: 'animals seen from lines or points, at a recorded distance',
    qa_q_espace: 'Where: maps, spatial autocorrelation', qa_ex_espace: 'where values are high, whether nearby points resemble each other',
    qa_q_explorer: 'Explore or summarise a table', qa_ex_explorer: 'which variables go together, which observations resemble each other',
    qa_q_predire: 'Predict or classify', qa_ex_predire: 'a value or a class for new observations',
    qa_q_diversite: 'Diversity and species richness', qa_ex_diversite: 'how many species, compared on an equal footing',
    qa_q_baci: 'Before-after with controls (BACI)', qa_ex_baci: 'the effect of a management on some units, compared with control units',
    qa_but: 'For the species', qa_but_composition: 'how the composition changes (with the environment, between groups)',
    qa_but_par_espece: 'one model per species', qa_but_indicatrices: 'the species that characterize each group',
    qa_r_mesure: 'A measurement', qa_rex_mesure: 'length, biomass, concentration',
    qa_r_comptage: 'A count', qa_rex_comptage: 'individuals, nests',
    qa_r_presence: 'A presence-absence (0 or 1)', qa_rex_presence: 'a species seen or not',
    qa_r_proportion: 'A proportion between 0 and 1', qa_rex_proportion: 'cover, survival',
    qa_r_classe: 'A class', qa_rex_classe: 'a habitat, a state, a species',
    qa_r_especes: 'A table of species', qa_rex_especes: 'one column per species',
    qa_r_distances: 'Distances to detected animals', qa_rex_distances: 'one row per detection',
    qa_zeros: 'with many zeros', qa_zeros_aide: 'More zeros than a Poisson distribution of the same mean gives',
    qa_reponse_variable: 'Response', qa_variables_explorer: 'Variables of the table', qa_choisir: '— choose —',
    qa_reponse_facultative: '— none —',
    qa_faits_bornes: 'from {min} to {max}', qa_faits_entiers: 'whole numbers', qa_faits_decimales: 'not all whole numbers',
    qa_faits_mesure_arrondie: 'a measurement rounded to its unit, by its unit or its name',
    qa_faits_zeros: '{p}% zeros', qa_faits_poisson: 'a Poisson distribution of the same mean would give {q}%',
    qa_faits_manquantes: '{p}% missing values', qa_faits_categories: '{n} categories', qa_faits_categorie_une: '1 category', qa_faits_asymetrie: 'skewed (a few very large values)',
    qa_faits_inconnus: 'the variable statistics could not be read', qa_faits_propose: 'Proposed from the data: {faits}.',
    qa_especes_proposees: 'Proposed: the columns of numbers zero or more that have zeros.', qa_n_cochees: '{n} of {total}',
    qa_d_repetes: 'The same sites or plots measured several times', qa_d_repetes_aide: 'A random effect by site, cross-validation by site',
    qa_d_annees: 'Years, or dates', qa_d_baci: 'A BACI design: impact and control units, before and after',
    qa_d_coordonnees: 'Spatial coordinates',
    qa_d_transects: 'Lines or points', qa_transects_ligne: 'Lines (line transects)', qa_transects_point: 'Points (point transects)',
    qa_site: 'Site', qa_temps: 'Time', qa_unite: 'Unit', qa_groupe: 'Groups', qa_x: 'Explanatory variables (X)', qa_especes: 'Species',
    qa_coordonnees_projet: 'the project’s: {x}, {y}', qa_coordonnees_proposees: 'proposed: {x}, {y}, to choose in Plots › Map',
    qa_coordonnees_aucune: 'none: to choose in Plots › Map',
    qa_recommandee: 'Recommended analysis', qa_autres: 'Alternatives', qa_pourquoi: 'Why', qa_conditions: 'Conditions', qa_rempli: 'Filled in',
    qa_a_completer: 'To complete: {liste}.', qa_avec_site: '{nom}, with a random effect by {site}',
    qa_choisir_question: 'Choose your question: the recommended analysis shows here, with why, its conditions and a button that opens the right window, already filled in.',
    qa_choisir_reponse: 'Choose the response, or its kind.',
    qa_conseil: 'Advice, not a rule: the conditions are checked after the fit (Summary › Model conditions).',
    qa_ouvrir_modele: 'Open New model, already filled in', qa_ouvrir_par_reponse: 'Open New model, then One model per response',
    qa_ouvrir_vue: 'Open {vue}', qa_ouvrir_variable: 'Open New variable, already filled in', qa_ouvrir_court: 'Open',
    qa_manque_y: 'the response', qa_manque_y_quantitative: 'a quantitative response', qa_manque_x: 'the explanatory variables (X)', qa_manque_groupe: 'the groups',
    qa_manque_classe: 'the class (a qualitative response)', qa_manque_especes: 'the species (two at least)', qa_manque_site: 'the site', qa_manque_temps: 'the time',
    qa_manque_unite: 'the unit', qa_manque_variables: 'the variables (two at least)', qa_manque_coordonnees: 'the project’s coordinates (Plots › Map)',
    qa_manque_distances: 'the distances (as Y)', qa_manque_echantillon: 'the transect or point of each row', qa_manque_effort: 'each transect’s effort (its length)',
    qa_manque_unites_lignes: 'the units of the distances, lengths and areas (m, km and km² otherwise)', qa_manque_unites_points: 'the units of the distances and areas (m and km² otherwise)',
    qa_rempli_y: 'Y', qa_rempli_classe: 'Class', qa_rempli_x: 'X', qa_rempli_x_ajustees: 'X fitted onto the plane', qa_rempli_especes: 'Species',
    qa_rempli_aleatoire: 'Random effect', qa_rempli_vc: 'Cross-validation groups', qa_rempli_loi: 'Distribution', qa_rempli_courbes: 'Curves',
    qa_rempli_transformation: 'Transformation', qa_rempli_spatial: 'Spatial smooth', qa_rempli_site: 'Site', qa_rempli_temps: 'Time',
    qa_rempli_centree: 'Centred on its mean',
    qa_rempli_groupes: 'Groups', qa_rempli_apparier: 'Paired by', qa_rempli_unite: 'Unit', qa_rempli_lignes: 'Lines', qa_rempli_tendances: 'Trends',
    qa_rempli_couleur: 'Colour', qa_rempli_formule: 'Formula', qa_rempli_distance_x: 'Distance of the X', qa_rempli_composantes: 'Components',
    qa_rempli_plan: 'Design', qa_rempli_transect: 'Transect or point', qa_rempli_effort: 'Effort', qa_rempli_strate: 'Stratum', qa_rempli_aire: 'Area',
    qa_rempli_taille: 'Group size',
    qa_auto: 'automatic', qa_oui: 'yes', qa_hellinger: 'Hellinger', qa_distance_geographique: 'geographic',

    qa_nom_normal: 'Linear regression', qa_nom_lm_log: 'Linear regression on the log of the response',
    qa_nom_lmm: 'Linear mixed model', qa_nom_lmm_log: 'Linear mixed model on the log of the response',
    qa_nom_glm_poisson: 'Poisson GLM', qa_nom_glm_nb: 'Negative binomial GLM', qa_nom_glm_zip: 'Zero-inflated Poisson GLM',
    qa_nom_glm_binomial: 'Logistic regression (binomial GLM)', qa_nom_glm_beta: 'Beta regression', qa_nom_glm_gamma: 'Gamma GLM',
    qa_nom_glm_tweedie: 'Tweedie GLM', qa_nom_gam: 'Additive model (GAM), with curves', qa_nom_gam_spatial: 'Additive model with a spatial smooth',
    qa_nom_par_reponse: 'One model per species', qa_nom_acp: 'PCA (principal component analysis)', qa_nom_pls: 'PLS regression', qa_nom_opls: 'OPLS',
    qa_nom_plsda: 'PLS-DA (discriminant PLS)', qa_nom_oplsda: 'OPLS-DA', qa_nom_classes: 'Class models (one PCA per class)',
    qa_nom_arbre: 'Decision tree', qa_nom_lda: 'Linear discriminant analysis (LDA)', qa_nom_kmeans: 'k-means', qa_nom_foret: 'Random forest',
    qa_nom_boost: 'Boosted regression trees (BRT)', qa_nom_rda: 'RDA (redundancy analysis)', qa_nom_cca: 'CCA (canonical correspondence analysis)',
    qa_nom_ca: 'Correspondence analysis (CA)', qa_nom_pcoa: 'PCoA (principal coordinates analysis)',
    qa_nom_nmds: 'NMDS (non-metric multidimensional scaling)', qa_nom_permanova: 'PERMANOVA (permutational analysis of variance)',
    qa_nom_mantel: 'Mantel test', qa_nom_indval: 'Indicator species (IndVal)', qa_nom_trim: 'TRIM: indices and trend of counts',
    qa_nom_tests_deux: 'Test between two variables', qa_nom_tests_apparies: 'Test paired by site', qa_nom_tests_classes: 'Tests between classes, variable by variable',
    qa_nom_correlations: 'Correlation matrix', qa_nom_baci: 'Before/after with controls (BACI)', qa_nom_series: 'Series and trends (Mann–Kendall, Sen’s slope)',
    qa_nom_carte: 'Map', qa_nom_autocorrelation: 'Spatial autocorrelation (Moran’s I)', qa_nom_rarefaction: 'Rarefaction and species accumulation',
    qa_nom_statistiques: 'Variable statistics', qa_nom_indices: 'Diversity indices (new variable)',
    qa_nom_distance: 'Distance sampling (density, abundance)',

    qa_pourquoi_normal: 'A continuous measurement whose departures from the model are roughly normal: linear regression gives each variable’s effect, in its units.',
    qa_conditions_normal: 'Residuals roughly normal with a constant spread: the Summary checks them (Model conditions).',
    qa_pourquoi_lm_log: 'A positive, skewed measurement: its logarithm comes closer to a normal distribution, and the effects read as ratios.',
    qa_conditions_lm_log: 'With zeros, log10(y + C): the constant Variable statistics proposes.',
    qa_pourquoi_glm_poisson: 'Counts (whole numbers, zero or more): the Poisson distribution, with a log link; the effects read as ratios.',
    qa_conditions_glm_poisson: 'A variance equal to the mean: the Summary flags overdispersion, which the negative binomial then accounts for.',
    qa_pourquoi_glm_nb: 'Counts more spread out than a Poisson distribution allows (a variance well above the mean, many zeros): the negative binomial estimates that spread.',
    qa_conditions_glm_nb: 'Whole numbers, zero or more. Many zeros don’t always mean zero inflation (Warton 2005): compare with the zero-inflated Poisson by AIC (Predict › Compare models).',
    qa_pourquoi_glm_zip: 'Counts with more zeros than a Poisson distribution gives, when a zero can come from a true absence: the zero-inflated Poisson.',
    qa_conditions_glm_zip: 'At least one zero; compare with the negative binomial by AIC; the Summary compares the zeros observed with those expected.',
    qa_pourquoi_glm_binomial: 'A presence-absence (0 or 1): logistic regression gives the probability of presence; the effects read as odds ratios.',
    qa_conditions_glm_binomial: 'A 0 or 1 response only, and enough presences and absences (about ten of each per explanatory variable).',
    qa_pourquoi_glm_beta: 'A continuous proportion between 0 and 1 (a cover, a rate): the beta distribution, with a logit link.',
    qa_conditions_glm_beta: 'Values strictly between 0 and 1.',
    qa_pourquoi_glm_gamma: 'A strictly positive, skewed measurement: the gamma distribution, with a log link, keeps the original scale.',
    qa_conditions_glm_gamma: 'No zero or negative value; a spread that grows with the mean.',
    qa_pourquoi_glm_tweedie: 'A positive measurement with zeros (a density, a biomass): the Tweedie distribution models both the zeros and the positive values.',
    qa_conditions_glm_tweedie: 'Values zero or more; its power p is estimated (between 1 and 2).',
    qa_pourquoi_gam: 'When a variable’s effect may be curved (an optimum, a threshold): the additive model fits a curve to each quantitative variable.',
    qa_conditions_gam: 'Enough observations for each curve (twenty at least); untick the curve of a variable whose effect is straight.',
    qa_pourquoi_gam_spatial: 'A response structured in space: the additive model adds a smooth surface of the coordinates, which takes what the variables don’t explain.',
    qa_conditions_gam_spatial: 'The project’s coordinates (Plots › Map) and enough different places.',
    qa_pourquoi_par_reponse: 'The same model for each species, the p-values corrected over the species: one model per response.',
    qa_conditions_par_reponse: 'Fit the model of the first species: the One model per response window then opens with the other species ticked.',
    qa_pourquoi_acp: 'Summarise many quantitative variables in a few axes: which variables go together, which observations resemble each other.',
    qa_conditions_acp: 'Variables scaled (UV); Plots › Dendrogram then groups the observations.',
    qa_pourquoi_pls: 'Predict a response from many correlated variables (spectra, more variables than observations): PLS.',
    qa_conditions_pls: 'The number of components is chosen by cross-validation; look at Q2 and the VIPs.',
    qa_pourquoi_opls: 'Like PLS, separating what predicts the response from what varies without it.',
    qa_conditions_opls: 'One predictive component per Y; the same prediction as PLS.',
    qa_pourquoi_plsda: 'Classify observations from many correlated variables: discriminant PLS.',
    qa_conditions_plsda: 'Cross-validation gives the share correctly classified.',
    qa_pourquoi_oplsda: 'Two classes and many variables: OPLS-DA separates what tells them apart from the rest.',
    qa_conditions_oplsda: 'Mostly for two classes; the S-plot shows the variables that separate them.',
    qa_pourquoi_classes: 'Tell whether an observation resembles a known class: one PCA per class, and the Coomans plot.',
    qa_conditions_classes: 'Enough observations in each class (ten at least).',
    qa_pourquoi_arbre: 'Rules that are easy to read (“if depth is above 3 m…”) and thresholds in the variables’ units: the decision tree.',
    qa_conditions_arbre: 'Enough observations (a hundred); a tree is unstable: the First-split stability view says how much.',
    qa_pourquoi_lda: 'Separate known classes by linear combinations of the variables: linear discriminant analysis.',
    qa_conditions_lda: 'Variables roughly normal with similar spreads in each class.',
    qa_pourquoi_kmeans: 'Form groups of observations that resemble each other, with no class known beforehand: k-means.',
    qa_conditions_kmeans: 'The number of groups k is chosen (the Choice of k view helps); variables scaled.',
    qa_pourquoi_foret: 'Predict as well as possible, with non-linear effects and interactions, without writing them: the random forest.',
    qa_conditions_foret: 'Observations with a missing X are left out; it gives the variables’ importance, not coefficients.',
    qa_pourquoi_boost: 'Predict a presence, a count or a measurement with trees added one by one (Elith et al. 2008): boosted trees.',
    qa_conditions_boost: 'The number of trees is chosen by cross-validation; a few minutes on a large table.',
    qa_pourquoi_rda: 'How much of the species composition the explanatory variables explain, and how: RDA, after the Hellinger transformation (Legendre and Gallagher 2001).',
    qa_conditions_rda: 'Species that respond roughly linearly, along short gradients; otherwise CCA. Tests by permutations.',
    qa_pourquoi_cca: 'Species that have an optimum along long gradients: canonical correspondence analysis.',
    qa_conditions_cca: 'Abundances zero or more; rare species weigh a lot.',
    qa_pourquoi_ca: 'The main structures of a table of species, abundances or presences, without explanatory variables.',
    qa_conditions_ca: 'Values zero or more; rare species weigh a lot.',
    qa_pourquoi_pcoa: 'Place the observations from a distance between them (Bray-Curtis for abundances).',
    qa_conditions_pcoa: 'A distance that suits the data; the X are fitted onto the plane afterwards.',
    qa_pourquoi_nmds: 'Show how communities resemble each other in two dimensions, from the ranks of their distances (Bray-Curtis): NMDS.',
    qa_conditions_nmds: 'A stress below 0.2; the X are fitted onto the plane afterwards.',
    qa_pourquoi_permanova: 'Test whether the composition differs between groups or with variables, on a distance (Bray-Curtis): PERMANOVA.',
    qa_conditions_permanova: 'Similar spreads between the groups: the Dispersion view tests them.',
    qa_pourquoi_mantel: 'Tell whether communities close in space resemble each other more: the Mantel test between the species distance and the geographic distance.',
    qa_conditions_mantel: 'The project’s coordinates; the correlogram details it by distance class.',
    qa_pourquoi_indval: 'The species that characterize each group (habitats, types of sites): the indicator value, specificity and fidelity.',
    qa_conditions_indval: 'Groups known beforehand (a qualitative variable); p corrected over the species.',
    qa_pourquoi_trim: 'Counts by site and year, with missing counts: TRIM gives the yearly indices and the trend, as for bird monitoring in Europe.',
    qa_conditions_trim: 'One row per site and year, whole counts; the missing counts are imputed.',
    qa_pourquoi_tests_deux: 'Compare two variables with the test their types call for, its conditions checked (t, ANOVA, Kruskal–Wallis, chi-square…).',
    qa_conditions_tests_deux: 'Independent observations; the result comes as a sentence ready for a report.',
    qa_pourquoi_tests_apparies: 'The same sites in each group: a paired test compares them site by site (paired t, Wilcoxon; Friedman for three groups or more).',
    qa_conditions_tests_apparies: 'Each site once per group; the sites without a partner are left out, and counted.',
    qa_pourquoi_tests_classes: 'Each quantitative variable compared between the classes, the p corrected over the variables.',
    qa_conditions_tests_classes: 'Independent observations.',
    qa_pourquoi_correlations: 'The correlations of all the quantitative variables, two by two.',
    qa_conditions_correlations: 'Pearson for linear relations, Spearman otherwise; p corrected.',
    qa_pourquoi_baci: 'A management changed on some units (the impact) compared with control units, before and after: the impact − control gap, after against before.',
    qa_conditions_baci: 'Tick the impact and control units, give the time of the event; “Fit the mixed model” accounts for the units and the autocorrelation.',
    qa_pourquoi_series: 'A variable over time: the curves, and the trend of each series (Mann–Kendall test, Sen’s slope).',
    qa_conditions_series: 'At least 4 times per series; years that resemble each other (autocorrelation) make p too small.',
    qa_pourquoi_carte: 'See where values are high and low: the observations on a map, coloured by the response.',
    qa_conditions_carte: 'The project’s coordinates: two variables and their system, chosen the first time.',
    qa_pourquoi_autocorrelation: 'Tell whether nearby observations resemble each other: Moran’s I, the correlogram and the semivariogram.',
    qa_conditions_autocorrelation: 'The project’s coordinates; for a model’s residuals, open it from that model.',
    qa_pourquoi_rarefaction: 'Compare richness on an equal footing: rarefaction and species accumulation, with Chao1, ACE and the jackknifes.',
    qa_conditions_rarefaction: 'Whole counts of individuals for rarefaction; otherwise accumulation alone.',
    qa_pourquoi_statistiques: 'A first look: missing values, minimum, maximum, skewness and the transformation advised for each variable.',
    qa_conditions_statistiques: 'None.',
    qa_pourquoi_indices: 'One index per survey (richness, Shannon, Simpson, Pielou), as a new variable, to compare between groups or model afterwards.',
    qa_conditions_indices: 'Species columns, numbers zero or more; richness depends on the effort: see rarefaction.',
    qa_pourquoi_distance: 'Not every animal is seen, and fewer far away: the detection function, fitted to the distances, gives the probability of detection, then the density and abundance of each stratum (Buckland et al. 2001), as the Distance package.',
    qa_conditions_distance: 'One row per detection, its distance as Y, and each transect without detection kept, its distance empty; every animal on the line or point seen; about sixty detections at least. What changes detection (the observer, the wind, the group size) can be a covariate of it (New model › Covariates).',

    qa_note_asymetrie_residus: 'The response is skewed, but its X may make it so: its residuals tell, after the fit (Summary › Model conditions). If they stay skewed, choose the gamma distribution or the log of the response, among the alternatives.',
    qa_note_comptage_incertain: 'Whole numbers without a unit, read as counts: if they are measurements rounded to their unit (grams, centimetres), choose A measurement instead, or the linear model among the alternatives; Edit › Units and descriptions gives the variable its unit.',
    qa_note_site: 'Observations of the same site resemble each other: the random effect by site accounts for it, and cross-validation predicts each site without it.',
    qa_note_vc_site: 'Cross-validation by site judges the model on sites it has never seen.',
    qa_note_strates: 'Observations of the same site aren’t independent: permutations within sites (General tab) if the X vary within each site; otherwise summarise by site first (Edit › Summarize by group).',
    qa_note_moyennes: 'Then Plots › Adjusted means compares the groups two by two (Tukey).',
    qa_note_annees: 'Successive years resemble each other: after the fit, Plots › Residual autocorrelation, then Fit with CAR1 if needed.',
    qa_note_puissance: 'Plots › Trend power then says how many years it takes to detect it.',
    qa_note_temps_centre: 'The time is centred on its mean (its scaling Ctr): the slope doesn’t change, and if you cross the time with a qualitative variable (does the trend differ from one habitat to another?), each category’s coefficient is its difference at the mean year, not in year 0, far from the data.',
    qa_note_temps_x: 'The time chosen isn’t a quantitative variable: to have it as an X, make one (Edit › New variable).',
    qa_note_bornes01: 'The response reaches 0 or 1, which the beta distribution refuses: bring it inside the interval first, saying so (Edit › New variable: (y * (n - 1) + 0.5) / n for n observations; Smithson and Verkuilen 2006).',
    qa_note_espace_residus: 'With coordinates: after the fit, Plots › Spatial autocorrelation of its residuals.',
    qa_note_especes_non_entieres: 'The species aren’t all whole counts: rarefaction doesn’t apply, accumulation does.',
    qa_note_trim_resumer: 'The table has several rows per site and year (plots, visits): TRIM takes one; add them up first (Edit › Summarize by group, the sum by site and year).',
    qa_note_distance_sans_aire: 'Without the strata’s areas (no column, or areas of 0), the density only, without abundance.',
    qa_note_distance_un_passage: 'Without a column of visits, each point counts one visit.',
  },
};
