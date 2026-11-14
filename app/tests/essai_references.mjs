// Checks interface/references.js (the references of a document to a project's results), without
// the application: reading {{...}}, the values in each language, the completion, the numbering.
//   node tests/essai_references.mjs
import { lireReferences, lireReference, separerCible, nombre, pourcentage, valeurP, valeurReference, offreModele, genreChemin, numeroter }
  from '../interface/references.js';

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);

console.log('Lire les références');
const texte = 'Le Q² est de {{M1.Q2}} ({{M1.Q2:3}}).\n\n{{M1.scores #fig-scores | Scores des sociétés}}\n\nVoir {{ref:fig-scores}} et {{ M2 (1).R2Y:% }}.';
const refs = lireReferences(texte);
egal('quatre références et leurs cibles', refs.map((r) => r.cible), ['M1.Q2', 'M1.Q2', 'M1.scores', 'ref:fig-scores', 'M2 (1).R2Y']);
egal('format, étiquette, légende', [refs[1].format, refs[2].etiquette, refs[2].legende, refs[4].format], ['3', 'fig-scores', 'Scores des sociétés', '%']);
egal('positions dans le texte', texte.slice(refs[0].debut, refs[0].fin), '{{M1.Q2}}');
egal('une référence vide est ignorée', lireReferences('a {{ }} b'), []);
egal('format %1', lireReference('M1.R2Y:%1').format, '%1');
egal('le nom de modèle le plus long gagne (noms à espaces et points)', separerCible('M2 (1).Q2', ['M2', 'M2 (1)']), { modele: 'M2 (1)', chemin: ['Q2'] });
egal('chemin à plusieurs parties', separerCible('M5.coef.annee:secteur', ['M5']), { modele: 'M5', chemin: ['coef', 'annee:secteur'] });
egal('modèle inconnu', separerCible('M9.Q2', ['M1']), null);

console.log('Nombres dans la langue du document');
egal('français : virgule, vrai signe moins', nombre(-0.1234, { langue: 'fr' }), '−0,12');
egal('anglais : point', nombre(0.6478, { decimales: 3, langue: 'en' }), '0.648');
egal('pas de signe moins devant un zéro arrondi', nombre(-0.001, { langue: 'fr' }), '0,00');
egal('chiffres significatifs', nombre(0.0012345, { significatifs: 3, langue: 'en' }), '0.00123');
egal('séparateur de milliers au-delà de 10 000 seulement', [nombre(1196, { decimales: 0 }), nombre(12345, { decimales: 0, langue: 'en' })], ['1196', '12,345']);
egal('pourcentage, espace fine en français', [pourcentage(0.657, { langue: 'fr' }), pourcentage(0.657, { decimales: 1, langue: 'en' })], ['66 %', '65.7%']);
egal('valeurs de p', [valeurP(0.0004, { langue: 'fr' }), valeurP(0.0321, { langue: 'en' })], ['p < 0,001', 'p = 0.032']);

console.log('Valeurs d’un modèle à composantes');
const pls = {
  type: 'PLS', A: 2, N: 120, K: 6, G: 3, noms_y: ['note'],
  composantes: { R2Xcum: [0.649, 0.838], R2Ycum: [0.619, 0.657], Q2cum: [0.60, 0.648] },
  q2_vide: -0.008, gain: 0.6507, RMSEE: { note: 0.5076 }, RMSEcv: { note: 0.508 },
  cv_anova: { note: { p: [null, 3.3e-25, null], F: [null, 52.9, null] } },
  variables: { terme: ['sucre', 'acidite'], VIP: [1.96, 0.71] },
  coefficients: { terme: ['sucre', 'acidite', 'Constante'], 'CoeffCS note': [0.35, -0.12, null], 'CoeffCS note IC': [0.1, 0.2, null] },
};
const v = (cible, langue = 'fr') => { const r = lireReference(cible); return valeurReference(pls, separerCible(r.cible, ['M1']).chemin, r, langue); };
egal('Q2 cumulé, 2 décimales par défaut', v('M1.Q2'), { texte: '0,65' });
egal('Q2 d’une composante', v('M1.Q2.1:3'), { texte: '0,600' });
egal('R2Y en pourcentage', v('M1.R2Y:%'), { texte: '66 %' });
egal('N, entier', v('M1.N'), { texte: '120' });
egal('gain et modèle vide', [v('M1.gain').texte, v('M1.Q2_vide:3').texte], ['0,65', '−0,008']);
egal('p de la CV-ANOVA', v('M1.cvanova.p', 'en'), { texte: 'p < 0.001' });
egal('VIP d’une variable', v('M1.vip.sucre'), { texte: '1,96' });
egal('coefficient et son intervalle, aux décimales de l’estimation', v('M1.coef.acidite'), { texte: '−0,120 [−0,320 ; 0,080]' });
egal('coefficient, 2 décimales demandées', v('M1.coef.acidite:2', 'en'), { texte: '−0.12 [−0.32, 0.08]' });
egal('variable inconnue', v('M1.vip.azote'), { erreur: 'variable_inconnue' });
egal('valeur inconnue', v('M1.truc'), { erreur: 'valeur_inconnue' });
egal('pas de Q2 honnête sans modèle hiérarchique', v('M1.Q2_honnete'), { erreur: 'valeur_inconnue' });

console.log('Valeurs d’une régression');
const lm = {
  regression: true, type: 'LM', N: 60, K: 3, noms_y: ['densite'],
  qualite: { r2: 0.42, r2_ajuste: 0.40, Q2: 0.37, AIC: 123.456, AICc: 124.1, BIC: 130.2 },
  coefficients_regression: { terme: ['(Intercept)', 'annee'], estimation: [3.2, -0.1234], bas: [2.8, -0.2011], haut: [3.6, -0.0457], p: [1e-9, 0.0031] },
};
const r = (cible, langue = 'fr') => { const x = lireReference(cible); return valeurReference(lm, separerCible(x.cible, ['M5']).chemin, x, langue); };
egal('coefficient complet', r('M5.coef.annee'), { texte: '−0,123 [−0,201 ; −0,046], p = 0,003' });
egal('estimation, intervalle, p seuls', [r('M5.est.annee:2').texte, r('M5.ic.annee', 'en').texte, r('M5.p.annee').texte], ['−0,12', '[−0.201, −0.046]', 'p = 0,003']);
egal('R2 ajusté, AIC à une décimale', [r('M5.R2_ajuste').texte, r('M5.AIC').texte], ['0,40', '123,5']);
egal('terme inconnu', r('M5.coef.altitude'), { erreur: 'terme_inconnu' });

console.log('Complétion, genres, numérotation');
const offre = offreModele(pls, { vues: ['scores', 'vip'], tableaux: ['composantes'] });
const chemins = offre.map((o) => o.chemin);
verifier('la complétion propose valeurs, VIP et coefficients par variable, tableaux, graphiques, méthodes',
  ['Q2', 'R2Y', 'gain', 'cvanova.p', 'vip.sucre', 'coef.acidite', 'table.composantes', 'scores', 'methodes'].every((c) => chemins.includes(c)) && !chemins.includes('Q2_honnete'),
  chemins.join(', '));
egal('chaque valeur proposée avec ce qu’elle donne', offre.find((o) => o.chemin === 'Q2').detail, '0,65');
egal('genres', [genreChemin(['table', 'composantes']), genreChemin(['methodes']), genreChemin(['scores'], ['scores']), genreChemin(['vip', 'sucre'], ['vip']), genreChemin(['vip'], ['vip'])],
  ['tableau', 'texte', 'graphique', 'valeur', 'graphique']);
const n = numeroter([{ etiquette: 'a' }, {}, { etiquette: 't1' }, { etiquette: 'b' }], (x) => ({ a: 'figure', t1: 'tableau', b: 'figure' }[x.etiquette] || null));
egal('figures et tableaux numérotés à part, étiquettes', [[...n.numeros.entries()], [...n.etiquettes.entries()]],
  [[[0, 1], [2, 1], [3, 2]], [['a', { genre: 'figure', numero: 1 }], ['t1', { genre: 'tableau', numero: 1 }], ['b', { genre: 'figure', numero: 2 }]]]);

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
