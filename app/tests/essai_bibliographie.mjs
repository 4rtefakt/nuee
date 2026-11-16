// Checks interface/bibliographie.js (the references a document cites), without the application:
// BibTeX and CSL-JSON read, LaTeX decoded, names, citations in Pandoc's syntax, the author–year and
// numbered styles in French and English, the list of references, BibTeX written back; references
// from Zotero's libraries (Zotero's real answers, tests/donnees/zotero), where they are kept and the
// same key in two libraries.
//   node tests/essai_bibliographie.mjs
import fs from 'fs';
import { decoderLatex, lireBibtex, lireNoms, lireCslJson, lireBibliographie, versBibtex, lireGroupeCitations, formaterCitations,
  segments, texteSeul, resume, correspond, DEBUT_ITALIQUE, FIN_ITALIQUE, elementZotero, lireReponseZotero, memeOeuvre, placerReference, SANS_ZOTERO,
  lireReferenceTexte, memeReference, joindreReferences } from '../interface/bibliographie.js';

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

console.log('Bibliothèques de Zotero');
const zot = (dossier, f) => fs.readFileSync(new URL(`./donnees/zotero/${dossier}/${f}`, import.meta.url), 'utf8');
egal('le lieu d’un élément : Ma bibliothèque (locale ou synchronisée), un groupe, tel que gardé ; rien sinon', ['http://zotero.org/users/local/ahrmAr1M/items/YP7DZ63D',
  'http://zotero.org/users/12345/items/ABCD1234', 'http://zotero.org/groups/6011001/items/79PWGJ9R', 'groups/6011001/items/79PWGJ9R', 'users/0/items/YP7DZ63D',
  'http://zotero.org/groups/x/items/79PWGJ9R', 'groups/6011001/items/abc', '', undefined].map(elementZotero),
['users/0/items/YP7DZ63D', 'users/0/items/ABCD1234', 'groups/6011001/items/79PWGJ9R', 'groups/6011001/items/79PWGJ9R', 'users/0/items/YP7DZ63D', '', '', '', '']);
// (Zotero 10.0.8 with Better BibTeX: format=json&include=bibtex, each item's library, key and BibTeX)
const moiB = lireReponseZotero(zot('better-bibtex', 'recherche_users_0_bechet.json'));
const camB = lireReponseZotero(zot('better-bibtex', 'recherche_groupe_6011001_bechet.json'));
const camW = lireReponseZotero(zot('better-bibtex', 'recherche_groupe_6011001_wold.json'))[0];
const perB = lireReponseZotero(zot('better-bibtex', 'recherche_groupe_6011002_perdrix.json'));
const perS = lireReponseZotero(zot('simple', 'recherche_groupe_6011002_perdrix.json'));
const bechetMoi = moiB[0];
const bechetGroupe = camB.find((e) => e.type === 'article');
egal('lu de la réponse de Zotero : la clé, le lieu, les champs', [bechetMoi.cle, bechetMoi.zotero, bechetGroupe.cle, bechetGroupe.zotero, bechetGroupe.doi, bechetGroupe.auteurs[1].nom],
  ['bechetPopulationDynamicsGreater2019', 'users/0/items/YP7DZ63D', 'bechetPopulationDynamicsGreater2019', 'groups/6011001/items/79PWGJ9R', '10.1111/jav.01234', 'Germain']);
egal('une réponse illisible, ou une erreur : rien', [lireReponseZotero('Not found'), lireReponseZotero(''), lireReponseZotero('[{"key":"ABCD1234"}]')], [[], [], []]);
const woldMoi = lireBibtex(String.raw`@article{woldPLSregressionBasicTool2001, title = {{PLS}-regression: a basic tool of chemometrics}, author = {Wold, Svante and Sj{\"o}str{\"o}m, Michael}, year = {2001}}`).entrees[0];
verifier('la même œuvre : copiée d’une bibliothèque à l’autre (titre, année, premier auteur ; DOI quand les deux en ont)', memeOeuvre(woldMoi, camW)
  && !memeOeuvre(bechetMoi, bechetGroupe) && !memeOeuvre({ ...camW, doi: '10.1/autre' }, camW) && memeOeuvre({ ...bechetMoi, doi: 'X' }, { ...bechetGroupe, doi: 'x' }));
verifier('les clés d’un groupe en lecture seule : celles de Better BibTeX, ou de Zotero sans lui, pour les mêmes œuvres', perB.map((e) => e.cle).join() === 'broSurvieHivernalePerdrix2019,reitzSuiviPopulationsPerdrix2012'
  && perS.map((e) => e.cle).join() === 'bro_survie_2019,reitz_suivi_2012' && memeOeuvre(perB[0], perS[0]), JSON.stringify([perB.map((e) => e.cle), perS.map((e) => e.cle)]));

// where a reference from Zotero goes among those of a document (kept: no `origine`; a file's: its path)
const doc = (...l) => new Map(l.map((e) => [e.cle, e]));
const garde = (e) => lireBibtex(versBibtex(e)).entrees[0];
const d1 = doc({ ...garde(bechetMoi), origine: null });
egal('une clé libre : prise', placerReference(camB.find((e) => e.type === 'techreport'), d1), { cle: 'bechetSuiviColoniesFlamants2021', nouvelle: true });
const p1 = placerReference(bechetGroupe, d1);
egal('la même clé dans deux bibliothèques, deux œuvres : la seconde prend une lettre', p1, { cle: 'bechetPopulationDynamicsGreater2019a', nouvelle: true, renommee: 'bechetPopulationDynamicsGreater2019' });
const d2 = doc(...d1.values(), { ...garde({ ...bechetGroupe, cle: p1.cle }), origine: null });
egal('choisie de nouveau, elle retrouve sa clé (le même élément de Zotero)', placerReference(bechetGroupe, d2),
  { cle: 'bechetPopulationDynamicsGreater2019a', remplace: true, autre: 'bechetPopulationDynamicsGreater2019' });
egal('Ma bibliothèque de nouveau : sa clé, remise à jour', placerReference({ ...bechetMoi, titre: `${bechetMoi.titre} !` }, d2),
  { cle: 'bechetPopulationDynamicsGreater2019', remplace: true, autre: null });
const d3 = doc(...d2.values(), { ...garde({ ...bechetGroupe, cle: 'bechetPopulationDynamicsGreater2019b', zotero: 'groups/1/items/AAAAAAAA', titre: 'Encore une autre' }), origine: null });
egal('une troisième œuvre : la lettre suivante', placerReference({ ...bechetGroupe, zotero: 'groups/1/items/BBBBBBBB', titre: 'Et une autre', doi: '' }, d3).cle, 'bechetPopulationDynamicsGreater2019c');
egal('la même œuvre copiée dans un groupe : la clé du document, sans la remplacer', placerReference(camW, doc({ ...garde({ ...woldMoi, zotero: 'users/0/items/QCQEELND' }), origine: null })),
  { cle: 'woldPLSregressionBasicTool2001', remplace: false, autre: null });
egal('un élément gardé sous la clé de Zotero, choisi dans Better BibTeX : la clé du document', placerReference(perB[0], doc({ ...garde({ ...perS[0], zotero: perB[0].zotero }), origine: null })),
  { cle: 'bro_survie_2019', remplace: true, autre: 'broSurvieHivernalePerdrix2019' });
egal('la clé d’une autre œuvre d’un fichier lié : une lettre ; la même œuvre d’un fichier : sa clé, le fichier gardé', [placerReference(bechetGroupe, doc({ ...bechetMoi, zotero: '', origine: 'refs.bib' })),
  placerReference(bechetMoi, doc({ ...bechetMoi, zotero: '', origine: 'refs.bib' }))],
[{ cle: 'bechetPopulationDynamicsGreater2019a', nouvelle: true, renommee: 'bechetPopulationDynamicsGreater2019' }, { cle: 'bechetPopulationDynamicsGreater2019', remplace: false, autre: null }]);
const ecrit = versBibtex({ ...bechetGroupe, cle: p1.cle });
egal('gardée avec son lieu dans Zotero, relue pareil ; sans lui dans le .bib de Quarto', [/zotero = \{groups\/6011001\/items\/79PWGJ9R\}/.test(ecrit), lireBibtex(ecrit).entrees[0].zotero,
  versBibtex(lireBibtex(ecrit).entrees[0]) === ecrit, /zotero/.test(versBibtex(lireBibtex(ecrit).entrees[0], SANS_ZOTERO)), /zotero/.test(versBibtex({ ...lireCslJson(zot('better-bibtex', 'export_camargue_2.json')).entrees[0], zotero: 'groups/6011001/items/79PWGJ9R' }, SANS_ZOTERO))],
[true, 'groups/6011001/items/79PWGJ9R', true, false, false]);
const deuxBechet = formaterCitations([{ cites: [{ cle: 'bechetPopulationDynamicsGreater2019', prefixe: '', suffixe: '', sansAuteur: false }, { cle: p1.cle, prefixe: '', suffixe: '', sansAuteur: false }] }],
  new Map([...d2.entries()]), { langue: 'en' });
egal('les deux citées, dans leur ordre : deux références', [deuxBechet.textes[0], deuxBechet.liste.map((x) => x.cle), deuxBechet.inconnues],
  ['(Béchet & Johnson, 2019; Béchet & Germain, 2019)', ['bechetPopulationDynamicsGreater2019a', 'bechetPopulationDynamicsGreater2019'], []]);

// The references of a Methods text (moteur/R/methodes.R…: APA texts) join the document's list: read
// as entries, written again in the document's style as the engine writes them, the same work once
// (the document's entry for it: by DOI, else by title and year), listed though not cited by key.
console.log('Les références d’un texte des méthodes');
const METHODES = ['Wold, S., Sjöström, M., & Eriksson, L. (2001). PLS-regression: a basic tool of chemometrics. Chemometrics and Intelligent Laboratory Systems, 58(2), 109–130. https://doi.org/10.1016/S0169-7439(01)00155-1',
  'Wood, S. N. (2017). Generalized additive models: An introduction with R (2nd ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781315370279',
  'R Core Team (2026). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/',
  'Boutin, C. (2026). Nuée (version 1.1.0) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.24263014',
  'Pinheiro, J., Bates, D., & R Core Team (2026). nlme: Linear and nonlinear mixed effects models (R package version 3.1-168). https://CRAN.R-project.org/package=nlme',
  'van den Berg, R. A., Hoefsloot, H. C. J., Westerhuis, J. A., Smilde, A. K., & van der Werf, M. J. (2006). Centering, scaling, and transformations: improving the biological information content of metabolomics data. BMC Genomics, 7, 142. https://doi.org/10.1186/1471-2164-7-142',
  'Oksanen, J., Simpson, G. L., Blanchet, F. G., Kindt, R., Legendre, P., Minchin, P. R., et al. (2025). vegan: Community ecology package (R package). https://CRAN.R-project.org/package=vegan',
  'Stewart-Oaten, A., Murdoch, W. W., & Parker, K. R. (1986). Environmental impact assessment: "Pseudoreplication" in time? Ecology, 67(4), 929–940. https://doi.org/10.2307/1939815',
  'Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. Biometrika, 73(1), 13–22. https://doi.org/10.1093/biomet/73.1.13',
  'Kruskal, J. B. (1964a). Multidimensional scaling by optimizing goodness of fit to a nonmetric hypothesis. Psychometrika, 29(1), 1–27. https://doi.org/10.1007/BF02289565',
  'Kruskal, J. B. (1964b). Nonmetric multidimensional scaling: A numerical method. Psychometrika, 29(2), 115–129. https://doi.org/10.1007/BF02289694',
  'ter Braak, C. J. F. (1994). Canonical community ordination. Part I: Basic theory and linear methods. Écoscience, 1(2), 127–140.',
  'Tukey, J. W. (1953). The problem of multiple comparisons. Unpublished manuscript, Princeton University. Reprinted in H. I. Braun (Ed.), The collected works of John W. Tukey: Vol. VIII. Multiple comparisons: 1948–1983 (pp. 1–300). Chapman & Hall, 1994.'];
const wold = lireReferenceTexte(METHODES[0]);
egal('un article lu', [wold.type, wold.auteurs.map((n) => `${n.nom}, ${n.prenom}`), wold.annee, wold.titre, wold.revue, wold.volume, wold.numero, wold.pages, wold.doi],
  ['article', ['Wold, S.', 'Sjöström, M.', 'Eriksson, L.'], '2001', 'PLS-regression: a basic tool of chemometrics', 'Chemometrics and Intelligent Laboratory Systems', '58', '2', '109–130', '10.1016/S0169-7439(01)00155-1']);
const lus = METHODES.map(lireReferenceTexte);
egal('un livre et son édition, un logiciel, un auteur collectif, une particule, et al.', [lus[1].type, lus[1].edition, lus[1].editeur, lus[2].auteurs, lus[3].type, lus[4].auteurs[2],
  lus[5].auteurs[0], lus[6].auteurs[6]], ['book', '2', 'Chapman and Hall/CRC', [{ litteral: 'R Core Team' }], 'misc', { litteral: 'R Core Team' },
  { nom: 'Berg', prenom: 'R. A.', particule: 'van den', suffixe: '' }, { autres: true }]);
const enListe = (textes, options = {}) => {
  const j = joindreReferences(options.entrees || new Map(), textes);
  const c = formaterCitations(options.groupes || [], j.entrees, { style: 'auteur-annee', langue: 'en', ajoutees: j.ajoutees, ...options });
  return { j, c, liste: c.liste.map((x) => texteSeul(x.segments)) };
};
const tous = enListe(METHODES);
egal('écrites dans le style du document, comme le moteur les écrit (sauf l’ordre)', [...tous.liste].sort(), [...METHODES].sort());
verifier('les deux Kruskal de 1964 gardent leurs lettres, la liste dans l’ordre des auteurs (van den Berg à B, comme le document trie ses noms)',
  tous.liste.indexOf(METHODES[9]) >= 0 && tous.liste.indexOf(METHODES[9]) + 1 === tous.liste.indexOf(METHODES[10]) && /^van den Berg/.test(tous.liste[0]) && /^Boutin/.test(tous.liste[1]) && /^Wood/.test(tous.liste[tous.liste.length - 1]), tous.liste.join(' | '));
egal('leurs clés, et non citées par le texte', [tous.j.ajoutees.slice(0, 4), tous.c.nonCitees.length], [['nuee:wold2001', 'nuee:wood2017', 'nuee:rcoreteam2026', 'nuee:boutin2026'], METHODES.length]);
egal('en BibTeX', versBibtex({ ...lus[0], cle: 'nuee:wold2001' }),
  '@article{nuee:wold2001,\n  author = {Wold, S. and Sjöström, M. and Eriksson, L.},\n  title = {PLS-regression: a basic tool of chemometrics},\n  journal = {Chemometrics and Intelligent Laboratory Systems},\n  year = {2001},\n  volume = {58},\n  number = {2},\n  pages = {109--130},\n  doi = {10.1016/S0169-7439(01)00155-1}\n}');
// the document's own references: Wood 2017 from its .bib (another title's case, its DOI), R from
// Zotero without a DOI (the same title and year), Wold 2001 cited by the text: once each
const bibDoc = lireBibtex(`@book{wood2017, author = {Wood, Simon N.}, title = {Generalized Additive Models: An Introduction with {R}}, edition = {2}, publisher = {Chapman and Hall/CRC}, year = {2017}, doi = {10.1201/9781315370279}}
@manual{rcore, author = {{R Core Team}}, title = {{R}: A Language and Environment for Statistical Computing}, organization = {R Foundation for Statistical Computing}, year = {2026}}
@article{wold2001, author = {Wold, Svante and Sjöström, Michael and Eriksson, Lennart}, title = {{PLS}-regression: a basic tool of chemometrics}, journal = {Chemometrics and Intelligent Laboratory Systems}, year = {2001}}
@book{wood2006, author = {Wood, Simon N.}, title = {Generalized Additive Models: An Introduction with {R}}, publisher = {Chapman and Hall/CRC}, year = {2006}}`).entrees;
const avecDoc = enListe(METHODES.slice(0, 4), { entrees: new Map(bibDoc.map((e) => [e.cle, e])), groupes: [{ cites: [{ cle: 'wold2001', prefixe: '', suffixe: '', sansAuteur: false }], narratif: false }] });
egal('la même œuvre une fois : celle du document (par le DOI, ou par le titre et l’année), une autre édition à part', [avecDoc.c.liste.map((x) => x.cle).sort(), avecDoc.c.nonCitees],
  [['nuee:boutin2026', 'rcore', 'wold2001', 'wood2017'], ['wood2017', 'rcore', 'nuee:boutin2026']]);
egal('le texte cite toujours la sienne', avecDoc.c.textes, ['(Wold et al., 2001)']);
verifier('memeReference : le DOI d’abord, puis le titre et l’année, le début d’un titre de trois mots au moins', memeReference(lus[1], { titre: 'Autre', annee: '2017', doi: '10.1201/9781315370279' })
  && !memeReference(lus[1], { titre: lus[1].titre, annee: '2017', doi: '10.1/autre' }) && memeReference(lus[11], { titre: 'Canonical community ordination. Part I: basic theory and linear methods', annee: '1994' })
  && !memeReference(lus[11], { titre: 'Canonical community ordination', annee: '1995' }) && !memeReference({ titre: 'Random forests', annee: '2001' }, { titre: 'Random forests in ecology', annee: '2001' }));
const numerique = enListe(METHODES.slice(0, 2), { style: 'numerique', groupes: [{ cites: [{ cle: 'x', prefixe: '', suffixe: '', sansAuteur: false }], narratif: false }],
  entrees: new Map([['x', lireBibtex('@article{x, author = {Zed, A.}, title = {A cited work}, journal = {J}, year = {2020}}').entrees[0]]]) });
egal('numérotées après celles que le texte cite', [numerique.c.textes, numerique.c.liste.map((x) => [x.cle, x.numero])], [['[1]'], [['x', 1], ['nuee:wold2001', 2], ['nuee:wood2017', 3]]]);

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
