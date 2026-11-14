// Checks interface/bibliographie.js (the references a document cites), without the application:
// BibTeX and CSL-JSON read, LaTeX decoded, names, citations in Pandoc's syntax, the author–year and
// numbered styles in French and English, the list of references, BibTeX written back.
//   node tests/essai_bibliographie.mjs
import { decoderLatex, lireBibtex, lireNoms, lireCslJson, lireBibliographie, versBibtex, lireGroupeCitations, formaterCitations,
  segments, texteSeul, resume, correspond, DEBUT_ITALIQUE, FIN_ITALIQUE } from '../interface/bibliographie.js';

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);

console.log('LaTeX');
egal('accents, lettres, tirets, accolades', decoderLatex(String.raw`{\'E}cologie {\o}kologi \"uber \c{c}a, Gr\"{u}n, na\"{\i}ve, \v{S}koda, \'{\i}, pp. 10--20 --- {France}`),
  'Écologie økologi über ça, Grün, naïve, Škoda, í, pp. 10–20 — France');
egal('italique gardé (noms d’espèces)', decoderLatex(String.raw`Survie de \textit{Perdix perdix} et {\it Alectoris rufa}`),
  `Survie de ${DEBUT_ITALIQUE}Perdix perdix${FIN_ITALIQUE} et ${DEBUT_ITALIQUE}Alectoris rufa${FIN_ITALIQUE}`);
egal('symboles échappés', decoderLatex(String.raw`R\&D, 50\% de \$, a\_b`), 'R&D, 50% de $, a_b');
egal('l’espace dans l’argument d’une commande (Better BibTeX), biblatex', texteSeul(segments(decoderLatex(String.raw`({{{\emph{Perdix}}}}{\emph{ perdix}}) et \mkbibemph{Alectoris}`))),
  '(Perdix perdix) et Alectoris');
egal('en segments, le titre d’un livre en italique, ses noms latins droits', segments(`Biologie de ${DEBUT_ITALIQUE}Perdix${FIN_ITALIQUE}`, true),
  [{ texte: 'Biologie de ', italique: true }, { texte: 'Perdix', italique: false }]);

console.log('BibTeX');
const bib = String.raw`
Commentaire libre avec une adresse a@b.fr

@string{chemo = "Chemometrics and Intelligent Laboratory Systems"}
@comment{ rien {ici} }
@Article{wold2001,
  author = {Wold, Svante and Sj{\"o}str{\"o}m, Michael and Eriksson, Lennart},
  title = {{PLS}-regression: a basic tool of chemometrics},
  journal = chemo,
  year = 2001, month = feb,
  volume = {58}, number = {2}, pages = {109--130},
  doi = {https://doi.org/10.1016/S0169-7439(01)00155-1},
  file = {C:\Users\moi\Zotero\wold.pdf}
}
@book{eriksson2006,
  author = "Eriksson, L. and Johansson, E. and Kettaneh-Wold, N. and Trygg, J. and Wikstr{\"o}m, C. and Wold, S.",
  title = "Multi- and megavariate data analysis",
  edition = {2}, publisher = {Umetrics}, address = {Ume{\aa}}, year = {2006}
}
@incollection{cassel1999, author={Cassel, C. and Hackl, P. and Westlund, A.}, title={Robustness of partial least-squares method},
  booktitle={Handbook of methods}, editor={Smith, John and Doe, Jane}, pages={1-10}, publisher={Springer}, year={1999}}
@phdthesis{poncin2003, author = {Poncin, Anne}, title = {Les perdrix de plaine}, school = {Universit{\'e} de Lille}, year = 2003}
@misc{rcore, author = {{R Core Team}}, title = {R: A Language and Environment for Statistical Computing}, year = {2026},
  publisher = {R Foundation for Statistical Computing}, url = {https://www.R-project.org/}}
@article{casse, author = {Untel}, title = {Sans fin
@article{wold2001b, author = {Wold, Svante and Trygg, Johan and Berglund, Anders}, title = {Another paper}, journal = {J. Chemometrics}, year = {2001}}
`;
const r = lireBibtex(bib);
const par = new Map(r.entrees.map((e) => [e.cle, e]));
egal('entrées lues, @string et @comment compris', r.entrees.map((e) => e.cle), ['wold2001', 'eriksson2006', 'cassel1999', 'poncin2003', 'rcore', 'wold2001b']);
verifier('une entrée cassée est signalée et les suivantes lues', r.erreurs.length === 1 && /@article/.test(r.erreurs[0]), JSON.stringify(r.erreurs));
const w = par.get('wold2001');
egal('champs décodés, macro, DOI sans son adresse', [w.titre, w.revue, w.annee, w.pages, w.doi], ['PLS-regression: a basic tool of chemometrics',
  'Chemometrics and Intelligent Laboratory Systems', '2001', '109–130', '10.1016/S0169-7439(01)00155-1']);
egal('noms', w.auteurs.map((n) => [n.nom, n.prenom]), [['Wold', 'Svante'], ['Sjöström', 'Michael'], ['Eriksson', 'Lennart']]);
egal('un auteur collectif', par.get('rcore').auteurs, [{ litteral: 'R Core Team' }]);
egal('particules, suffixes, « and others »', lireNoms(String.raw`Ludwig van Beethoven and van Gogh, Vincent and Smith, Jr, John and others`),
  [{ nom: 'Beethoven', prenom: 'Ludwig', particule: 'van', suffixe: '' }, { nom: 'Gogh', prenom: 'Vincent', particule: 'van', suffixe: '' },
    { nom: 'Smith', prenom: 'John', particule: '', suffixe: 'Jr' }, { autres: true }]);
const reecrit = versBibtex(w);
verifier('réécrit sans le chemin du fichier, relu pareil', !/file\s*=/.test(reecrit) && lireBibtex(reecrit).entrees[0].titre === w.titre, reecrit);

console.log('CSL-JSON');
const csl = JSON.stringify([{ id: 'http://zotero.org/x', 'citation-key': 'legendre2012', type: 'book', title: 'Numerical ecology',
  author: [{ family: 'Legendre', given: 'Pierre' }, { family: 'Legendre', given: 'Louis' }], issued: { 'date-parts': [[2012]] },
  publisher: 'Elsevier', edition: '3' }, { id: 'zuur', type: 'chapter', title: 'Mixed effects', author: [{ family: 'Zuur', given: 'Alain F.' }],
  editor: [{ family: 'Doe', given: 'J.' }], 'container-title': 'Mixed models in ecology', page: '101-142', issued: { 'date-parts': [[2009, 3]] }, publisher: 'Springer' }]);
const c = lireBibliographie(csl, 'refs.json');
egal('lu par son extension, clés, types, années', c.entrees.map((e) => [e.cle, e.type, e.annee]), [['legendre2012', 'book', '2012'], ['zuur', 'incollection', '2009']]);
const relu = lireBibtex(versBibtex(c.entrees[0])).entrees[0];
egal('écrit en BibTeX et relu', [relu.cle, relu.type, relu.titre, relu.auteurs.length, relu.edition, relu.editeur], ['legendre2012', 'book', 'Numerical ecology', 2, '3', 'Elsevier']);
const riche = lireCslJson(JSON.stringify([{ id: 'bro2019', type: 'article-journal', title: 'Survie de la perdrix (<i>Perdix perdix</i>) en <span class="nocase">France</span>',
  'container-title': 'Faune sauvage' }])).entrees[0];
const ita = `Survie de la perdrix (${DEBUT_ITALIQUE}Perdix perdix${FIN_ITALIQUE}) en France`;
egal('texte riche de CSL-JSON : italique, balises retirées ; écrit en BibTeX (\\textit) et relu', [riche.titre, /\\textit\{Perdix perdix\}/.test(versBibtex(riche)),
  lireBibtex(versBibtex(riche)).entrees[0].titre], [ita, true, ita]);
const enFrancais = lireBibtex('@article{a, title = {Les oiseaux en {France}}, language = {french}}\n@article{b, title = {T}, language = {french}, langid = {french}}').entrees;
const deCsl = lireCslJson(JSON.stringify([{ id: 'c', type: 'book', title: 'T', language: 'fr' }])).entrees[0];
egal('la langue en langid (que Pandoc lit), une fois', [versBibtex(enFrancais[0]).match(/langid/g), versBibtex(enFrancais[1]).match(/langid/g), /langid = \{fr\}/.test(versBibtex(deCsl)),
  versBibtex(lireBibtex(versBibtex(enFrancais[0])).entrees[0]) === versBibtex(enFrancais[0])], [['langid'], ['langid'], true, true]);

console.log('Citations');
egal('groupe : préfixe, suffixe, sans auteur', lireGroupeCitations('voir @wold2001, p. 33; -@eriksson2006'),
  [{ cle: 'wold2001', prefixe: 'voir', suffixe: ', p. 33', sansAuteur: false }, { cle: 'eriksson2006', prefixe: '', suffixe: '', sansAuteur: true }]);
egal('une adresse électronique n’est pas une citation', lireGroupeCitations('écrire à a@b.fr'), null);
egal('clé entre accolades, ponctuation finale laissée', [lireGroupeCitations('@{une clé}')[0].cle, lireGroupeCitations('@doe99.')[0].cle], ['une clé', 'doe99']);

const entrees = new Map([...r.entrees, ...c.entrees].map((e) => [e.cle, e]));
const groupes = [
  { cites: [{ cle: 'wold2001', prefixe: '', suffixe: '', sansAuteur: false }] },
  { cites: [{ cle: 'eriksson2006', prefixe: 'voir', suffixe: 'p. 33', sansAuteur: false }, { cle: 'legendre2012', prefixe: '', suffixe: '', sansAuteur: false }] },
  { narratif: true, cites: [{ cle: 'wold2001b', prefixe: '', suffixe: '', sansAuteur: false }] },
  { cites: [{ cle: 'rcore', prefixe: '', suffixe: '', sansAuteur: true }] },
  { cites: [{ cle: 'inconnue', prefixe: '', suffixe: '', sansAuteur: false }] },
  { cites: [{ cle: 'cassel1999' }, { cle: 'poncin2003' }, { cle: 'zuur' }].map((x) => ({ prefixe: '', suffixe: '', sansAuteur: false, ...x })) },
];
const fr = formaterCitations(groupes, entrees, { style: 'auteur-annee', langue: 'fr' });
egal('auteur-année en français (2001a, 2001b ; et ; et al.)', fr.textes, ['(Wold et al., 2001b)', '(voir Eriksson et al., 2006, p. 33; Legendre et Legendre, 2012)',
  'Wold et al. (2001a)', '(2026)', '@inconnue', '(Cassel et al., 1999; Poncin, 2003; Zuur, 2009)']);
egal('les clés introuvables', fr.inconnues, ['inconnue']);
const liste = fr.liste.map((x) => texteSeul(x.segments));
egal('la liste, par auteur puis année', fr.liste.map((x) => x.cle), ['cassel1999', 'eriksson2006', 'legendre2012', 'poncin2003', 'rcore', 'wold2001b', 'wold2001', 'zuur']);
egal('un article', liste[6], 'Wold, S., Sjöström, M. et Eriksson, L. (2001b). PLS-regression: a basic tool of chemometrics. Chemometrics and Intelligent Laboratory Systems, 58(2), 109–130. https://doi.org/10.1016/S0169-7439(01)00155-1');
egal('un livre, son édition, sa revue en italique', [liste[1], fr.liste[1].segments.filter((x) => x.italique).map((x) => x.texte)],
  ['Eriksson, L., Johansson, E., Kettaneh-Wold, N., Trygg, J., Wikström, C. et Wold, S. (2006). Multi- and megavariate data analysis (2e éd.). Umetrics.', ['Multi- and megavariate data analysis']]);
egal('un chapitre (en français : Dans, dir.)', liste[0], 'Cassel, C., Hackl, P. et Westlund, A. (1999). Robustness of partial least-squares method. Dans J. Smith et J. Doe (dir.), Handbook of methods (p. 1–10). Springer.');
egal('une thèse, un auteur collectif avec son adresse', [liste[3], liste[4]], ['Poncin, A. (2003). Les perdrix de plaine [Thèse de doctorat]. Université de Lille.',
  'R Core Team (2026). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing. https://www.R-project.org/']);
const en = formaterCitations(groupes.slice(0, 3), entrees, { style: 'auteur-annee', langue: 'en' });
egal('en anglais (&, and, pp., Ed.)', [en.textes[1], texteSeul(formaterCitations([groupes[5]], entrees, { langue: 'en' }).liste[0].segments)],
  ['(voir Eriksson et al., 2006, p. 33; Legendre & Legendre, 2012)', 'Cassel, C., Hackl, P., & Westlund, A. (1999). Robustness of partial least-squares method. In J. Smith & J. Doe (Eds.), Handbook of methods (pp. 1–10). Springer.']);
const num = formaterCitations(groupes, entrees, { style: 'numerique', langue: 'fr' });
egal('numéroté : dans l’ordre des citations, plages', num.textes, ['[1]', '[voir 2, p. 33; 3]', 'Wold et al. [4]', '[5]', '[@inconnue]', '[6–8]']);
egal('numéroté : la liste dans le même ordre', num.liste.map((x) => [x.numero, x.cle]), [[1, 'wold2001'], [2, 'eriksson2006'], [3, 'legendre2012'], [4, 'wold2001b'], [5, 'rcore'], [6, 'cassel1999'], [7, 'poncin2003'], [8, 'zuur']]);

console.log('Recherche');
verifier('une recherche sur l’auteur et l’année, sans accents', correspond(par.get('wold2001'), 'sjostrom 2001') && !correspond(par.get('wold2001'), 'legendre'));
egal('le résumé d’une entrée', resume(par.get('wold2001')), 'Wold et al. 2001 · PLS-regression: a basic tool of chemometrics');

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
