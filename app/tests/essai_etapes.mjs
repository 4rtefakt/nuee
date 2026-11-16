// Checks interface/etapes_texte.js (what the engine says of a project's source and steps, for the
// dialogs of etapes.js), without the application: a source's state, an update's report read in
// sections (its preview), the models fitted again and the summary after an update.
//   node tests/essai_etapes.mjs
import { liste, noms, dateLisible, tailleLisible, etatSource, genreEtape, lireRapport, modelesAReajuster, resumeApres, phraseAutour, valeursChangees,
  descriptionVersion } from '../interface/etapes_texte.js';
import { changerLangue } from '../interface/i18n.js';

// (i18n.js puts the language on the page: none here)
globalThis.document ??= { documentElement: {} };

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);
const textes = (lu, cle) => (lu.sections.find((s) => s.cle === cle) || { lignes: [] }).lignes.map((l) => l.texte);
const classes = (lu, cle) => (lu.sections.find((s) => s.cle === cle) || { lignes: [] }).lignes.map((l) => l.classe);

changerLangue('en');

console.log('Small pieces');
egal('a value R sends alone, or nothing, as a list', [liste(3), liste(null), liste([1, 2])], [[3], [], [1, 2]]);
egal('names: the first ones, then how many in all', noms(['a', 'b', 'c', 'd'], 2), 'a, b… (4 in all)');
egal('sizes', [tailleLisible(512), tailleLisible(12345), tailleLisible(2.5e6), tailleLisible(null)], ['512 bytes', '12 kB', '2.5 MB', '']);
verifier('a date of the engine, read in the computer’s time', /2026/.test(dateLisible('2026-11-16T14:05:00')) && /14:05/.test(dateLisible('2026-11-16T14:05:00')));
egal('no date, or one that can’t be read', [dateLisible(''), dateLisible('pas une date')], ['', 'pas une date']);
egal('a step’s kind, and an unknown one as it is', [genreEtape('remplacer'), genreEtape('nouvelle_demande')], ['find and replace', 'nouvelle_demande']);

console.log('The source’s state');
const source = { chemin: 'C:/etude/donnees/releves.csv', relatif: 'donnees/releves.csv', trouve: 'C:/etude/donnees/releves.csv', existe: true,
  taille: 4096, date: '2026-11-16T09:12:00', sha256: 'ab', lu: '2026-11-16T09:15:00', modifie: false, actuel: { taille: 4096, date: '2026-11-16T09:12:00' },
  table: null, lecteur: 'tableau' };
egal('none', etatSource(null).etat, 'aucune');
egal('as it was read, where it was', [etatSource(source).etat, etatSource(source).ailleurs], ['lue', false]);
const changee = { ...source, modifie: true, actuel: { taille: 4200, date: '2026-11-16T15:00:00' } };
egal('changed since it was read', etatSource(changee).etat, 'changee');
verifier('the file as it is now names its notice: changed again, another key', etatSource(changee).cle !== etatSource({ ...changee, actuel: { taille: 4300, date: '2026-11-16T16:00:00' } }).cle);
egal('not found (modifie is null then)', etatSource({ ...source, existe: false, trouve: null, modifie: null, actuel: undefined }).etat, 'introuvable');
verifier('found beside the project, moved with its data', etatSource({ ...source, trouve: 'D:/copie/etude/donnees/releves.csv' }).ailleurs);
verifier('the same path written with other slashes or capitals is the same file', !etatSource({ ...source, trouve: 'c:\\etude\\donnees\\RELEVES.csv' }).ailleurs);

console.log('An update’s report: next year’s file');
// (as the engine sends it: moteur/R/etapes.R, aligner_projet and mise_a_jour; a vector of one
// value comes as that value, I() keeps the others lists)
const rapport = {
  observations: { ajoutees: 5, retirees: 1, premieres_ajoutees: ['S1-2026', 'S2-2026', 'S3-2026', 'S4-2026', 'S5-2026'], premieres_retirees: ['S2-2019'],
    cellules: 40, cellules_par_variable: [{ variable: 'y', cellules: 1 }, { variable: '$C1.t1', cellules: 39 }], par_position: false, repetees: 0 },
  variables: { nouvelles: ['profondeur'], absentes: [], recalculees: [], categories: [{ variable: 'habitat', nouvelles: ['dune'] }, { variable: 'site', nouvelles: ['S5'] }],
    types: [] },
  identifiants: { nouveaux: [], absents: [] },
  modeles: { variables_absentes: [], observations_choisies: [{ nom: 'Sans', numero: 2, observations: 38, admises: 40, nouvelles: 5 }], sans_nouvelles: [],
    references_changees: [{ nom: 'Tous', numero: 1, variable: 'habitat', avant: 'prairie', apres: 'lande' }],
    nouvelles_categories: [{ nom: 'C1', numero: 5, variable: 'habitat', categories: ['dune'] }] },
  etapes: [{ n: 1, demande: 'modifier_valeur', etiquette: 'modification de x1', statut: 'ok', raison: '', notes: [] },
    { n: 2, demande: 'remplacer', etiquette: 'remplacement', statut: 'ok', raison: '', notes: [] }],
  fichier: { chemin: 'C:/etude/donnees/releves 2026.csv', table: null, lecteur: 'tableau' }, notes: [],
};
const lu = lireRapport(rapport, 40);
egal('the sections that have something to say, in order', lu.sections.map((s) => s.cle), ['nouveau', 'absent', 'change', 'etapes', 'modeles']);
egal('what is new: observations, variables, categories', textes(lu, 'nouveau'),
  ['5 observation(s): S1-2026, S2-2026, S3-2026, S4-2026, S5-2026', '1 variable(s): profondeur', 'Categories of “habitat”: dune', 'Categories of “site”: S5']);
egal('what is missing, as a warning', [textes(lu, 'absent'), classes(lu, 'absent')], [['1 observation(s) no longer in the file, which leave every model: S2-2019'], ['attention']]);
egal('the cells changed, the variable with the most first', textes(lu, 'change'), ['40 cell(s) of the observations already there: $C1.t1 (39), y (1)']);
egal('every step replayed', textes(lu, 'etapes'), ['All 2 steps replayed.']);
egal('a reference category that changes warns, a category gained informs', [textes(lu, 'modeles'), classes(lu, 'modeles')],
  [['Tous: its reference category of “habitat”, “prairie”, has no observations left; “lande” takes its place', 'C1 takes the new categories of “habitat”: “dune”'], ['attention', '']]);
egal('one line: the observations before and after, what came and went', lu.resume, '40 → 44 observations · 5 new, 1 gone · 40 cell(s) changed · variables: 1 new, 0 gone');
egal('the models asked whether they take the new rows: those they fit, could fit, and would take', lu.choix.map((c) => [c.nom, c.numero, c.nouvelles, c.texte]),
  [['Sans', 2, 5, 'Sans · fits 38 of 40 rows · takes 5 new row(s)']]);
verifier('something changes', !lu.rien);
const avant = lireRapport({ ...rapport, modeles: { ...rapport.modeles, references_changees: undefined, nouvelles_categories: undefined } }, 40);
egal('an engine without the fields on categories: no line for them', textes(avant, 'modeles'), []);

console.log('An update’s report: a file without a column, steps that no longer apply');
const manque = {
  observations: { ajoutees: 0, retirees: 0, premieres_ajoutees: [], premieres_retirees: [], cellules: 0, cellules_par_variable: [], par_position: true, repetees: 2 },
  variables: { nouvelles: [], absentes: ['x2', 'x12'], recalculees: ['xz'], categories: [], types: [{ variable: 'note', type: 'quantitative', perdues: 3 }, { variable: 'lot', type: 'qualitative', perdues: 0 }] },
  identifiants: { nouveaux: ['parcelle'], absents: ['secteur'] },
  modeles: { variables_absentes: [{ nom: 'C1', numero: 5, variables: ['x2'] }], observations_choisies: [], sans_nouvelles: ['M3'] },
  etapes: [{ n: 1, demande: 'modifier_valeur', etiquette: 'modification de x1', statut: 'sautee', raison: '1 observation(s) missing from the data: S2-2016', notes: [] },
    { n: 2, demande: 'variable_calculee', etiquette: 'variable x12', statut: 'erreur', raison: 'object x2 not found', notes: [] },
    { n: 3, demande: 'coller_cellules', etiquette: 'pasted cells', statut: 'ok', raison: '', notes: ['1 observation(s) missing from the data: S9'] }],
  fichier: { chemin: 'C:/etude/donnees/sans x2.csv', lecteur: 'tableau' }, notes: '2 cell(s) read as missing',
};
const lu2 = lireRapport(manque, 40);
egal('the variables gone, an identifier gone', textes(lu2, 'absent'),
  ['2 variable(s) no longer in the file, which leave the project: x2, x12', 'Identifier columns no longer in the file: secteur']);
egal('computed again from its formula; types given back, values lost said', textes(lu2, 'change'),
  ['Computed again from their formula: xz', '“note” holds text in the file: read as numbers, as in the project; 3 value(s) that aren’t numbers become missing',
    '“lot” holds numbers in the file: kept qualitative, as in the project']);
egal('the steps: how many, then each skipped or failed with its reason, a note', [textes(lu2, 'etapes'), classes(lu2, 'etapes')],
  [['1 of 3 steps replayed.', 'Skipped — 1. modification de x1: 1 observation(s) missing from the data: S2-2016', 'Failed — 2. variable x12: object x2 not found',
    '3. pasted cells: 1 observation(s) missing from the data: S9'], ['', 'attention', 'erreur', '']]);
egal('a model that can’t be fitted again, and one without the new observations', [textes(lu2, 'modeles'), classes(lu2, 'modeles')],
  [['C1 can’t be fitted again: x2 missing', 'Without the new observations: M3'], ['erreur', '']]);
egal('how the file was read: by row numbers, names that repeat, the reader’s notes', textes(lu2, 'lecture'),
  ['No column names the observations: they are matched by their row number, and a row added or removed in the middle shifts all those after it.',
    '2 observation(s) share their name with another: those of the same name are matched in their order.', '2 cell(s) read as missing']);
egal('the models that can’t be fitted again', lu2.bloques, [{ nom: 'C1', numero: 5, variables: ['x2'] }]);
egal('one line, without the counts of observations when none is given', lireRapport(manque).resume, 'variables: 0 new, 2 gone · 2 step(s) not replayed');

console.log('The same file read again');
const meme = { observations: { ajoutees: 0, retirees: 0, premieres_ajoutees: [], premieres_retirees: [], cellules: 0, cellules_par_variable: [], par_position: false, repetees: 0 },
  variables: { nouvelles: [], absentes: [], recalculees: [], categories: [], types: [] }, identifiants: { nouveaux: [], absents: [] },
  modeles: { variables_absentes: [], observations_choisies: [], sans_nouvelles: [] },
  etapes: [{ n: 1, demande: 'remplacer', etiquette: 'remplacement', statut: 'ok', raison: '', notes: [] }], fichier: { chemin: 'C:/etude/releves.csv' }, notes: [] };
const lu3 = lireRapport(meme, 40);
egal('nothing changes: said, and only the steps listed', [lu3.rien, lu3.resume, lu3.sections.map((s) => s.cle)], [true, 'Nothing changes: the file gives the same data.', ['etapes']]);

console.log('The models fitted again');
const projet = { modeles: [{ numero: 3, nom: 'M3', type: 'PLS', ajuste: true }, { numero: 1, nom: 'Tous', type: 'LM', ajuste: true },
  { numero: 5, nom: 'C1', type: 'PCA', ajuste: true }, { numero: 7, nom: 'Vieux', type: 'PLS', ajuste: false }, { numero: null, nom: 'Sans numéro', type: 'PCA', ajuste: true }] };
const modeles = modelesAReajuster(projet, manque);
egal('those that can be fitted, by their numbers, the blocked one with what it misses', modeles.map((m) => [m.numero, m.nom, m.bloque]),
  [[1, 'Tous', null], [3, 'M3', null], [5, 'C1', ['x2']]]);
const apres = { modeles: [{ nom: 'Tous', numero: 1, type: 'LM', entrees: [{ cle: 'R2', valeur: 0.5 }] }, { nom: 'M3', numero: 3, type: 'PLS', entrees: [{ cle: 'Q2', valeur: 0.4 }] },
  { nom: 'C1', numero: 5, type: 'PCA', entrees: [{ cle: 'R2X', valeur: 0.7 }] }] };
const marque = resumeApres(apres, modeles, new Map([[1, { id: 'M9' }], [3, { erreur: 'Singular fit' }]]));
egal('after the update: a model fitted again as it is, one that failed and one blocked with their reasons', marque.modeles.map((m) => [m.nom, !!m.entrees, m.erreur || '']),
  [['Tous', true, ''], ['M3', false, 'Singular fit'], ['C1', false, 'variables missing from the data: x2']]);
const arrete = resumeApres(apres, modeles.slice(0, 2), new Map([[1, { id: 'M9' }]]));
egal('a model left when the update was stopped', arrete.modeles.map((m) => m.erreur || ''), ['', 'the update was stopped before its turn', '']);
verifier('the summary given is left as it was', apres.modeles.every((m) => m.entrees && !m.erreur));

console.log('The values cited in documents');
const doc = ['---', 'title: Suivi', '---', '', '# Results', 'The trend is **{{M1.coef.annee}}** per year. The model explains {{M1.R2:%}} of',
  'the variance (Q² = {{M1.Q2}}).', '', '- Sites: {{M2.N}} in all', '- Last one: none', '', '{{M1.scores | Scores. Of the sites}}'].join('\n');
const position = (brut, depuis = 0) => { const k = doc.indexOf(brut, depuis); return { debut: k, fin: k + brut.length, brut }; };
const valeursAvant = [{ ...position('{{M1.coef.annee}}'), modele: 'M1', texte: '0.12 [0.05, 0.19], p = 0.002' },
  { ...position('{{M1.R2:%}}'), modele: 'M1', texte: '41%' }, { ...position('{{M1.Q2}}'), modele: 'M1', texte: '0.35' }, { ...position('{{M2.N}}'), modele: 'M2', texte: '40' }];
const k = (brut) => valeursAvant.find((v) => v.brut === brut);
egal('the sentence of a reference: its words before and after, in its sentence only, a heading above left out',
  phraseAutour(doc, k('{{M1.coef.annee}}').debut, k('{{M1.coef.annee}}').fin, valeursAvant), { avant: 'The trend is ', apres: ' per year.' });
egal('over two lines, the other references as their values', phraseAutour(doc, k('{{M1.Q2}}').debut, k('{{M1.Q2}}').fin, valeursAvant),
  { avant: 'The model explains 41% of the variance (Q² = ', apres: ').' });
egal('a list item, its mark left out, the next item not taken', phraseAutour(doc, k('{{M2.N}}').debut, k('{{M2.N}}').fin, valeursAvant), { avant: 'Sites: ', apres: ' in all' });
const long = `${'word '.repeat(60)}{{M1.Q2}} ${'more '.repeat(60)}end.`;
const ph = phraseAutour(long, long.indexOf('{{'), long.indexOf('}}') + 2, [], 40);
verifier('a long sentence cut on both sides at a word', ph.avant.startsWith('…') && ph.avant.length <= 42 && ph.apres.endsWith('…') && ph.apres.length <= 42 && !/\s…$/.test(ph.apres),
  JSON.stringify(ph));
const valeursApres = valeursAvant.map((v) => (v.brut === '{{M1.Q2}}' ? { ...v, texte: '0.31' } : v.brut === '{{M1.R2:%}}' ? null : v)).filter(Boolean);
const changes = valeursChangees([{ id: 'D1', titre: 'Report', texte: doc, valeurs: valeursAvant }], [{ id: 'D1', titre: 'Report', texte: doc, valeurs: valeursApres }]);
egal('the values that changed, one the reference no longer gives, in the order of the text', changes.map((d) => [d.id, d.valeurs.map((v) => [v.brut, v.avant, v.apres])]),
  [['D1', [['{{M1.R2:%}}', '41%', ''], ['{{M1.Q2}}', '0.35', '0.31']]]]);
egal('with their sentences, a reference without a value written as it is', changes[0].valeurs[1].phrase, { avant: 'The model explains {{M1.R2:%}} of the variance (Q² = ', apres: ').' });
egal('a model not fitted again: its values are said not updated', valeursChangees([{ id: 'D1', texte: doc, valeurs: valeursAvant }],
  [{ id: 'D1', texte: doc, valeurs: valeursAvant }], ['M2']).map((d) => d.valeurs.map((v) => [v.brut, v.perime])), [[['{{M2.N}}', true]]]);
egal('a document whose values didn’t change is left out', valeursChangees([{ id: 'D1', texte: doc, valeurs: valeursAvant }], [{ id: 'D1', texte: doc, valeurs: valeursAvant }]), []);

console.log('The version before');
verifier('its description, with the date', /^Before the data update of 16 November 2026/.test(descriptionVersion(new Date(2026, 10, 16, 14, 5))));

console.log('En français');
changerLangue('fr');
const fr = lireRapport(rapport, 40);
egal('une ligne', fr.resume, '40 → 44 observations · 5 nouvelle(s), 1 partie(s) · 40 cellule(s) changée(s) · variables : 1 nouvelle(s), 0 partie(s)');
egal('ce qui est nouveau', textes(fr, 'nouveau')[2], 'Catégories de « habitat » : dune');
egal('les tailles, la virgule décimale', tailleLisible(2.5e6), '2,5 Mo');
verifier('la version d’avant', /^Avant la mise à jour des données du 16 novembre 2026/.test(descriptionVersion(new Date(2026, 10, 16, 14, 5))));
changerLangue('en');

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
