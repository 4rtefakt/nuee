// Checks interface/rendu_document.js (a document turned into its preview, a Word file or a Quarto
// file), without the application: the references markdown-it finds (not in code, not escaped, not
// Quarto's {{< >}}), those alone on their line, the preview, the .docx's parts and the .qmd's text.
//   node tests/essai_documents.mjs
import fs from 'fs';
import os from 'os';
import path from 'path';
import { spawnSync } from 'child_process';
import markdownit from 'markdown-it';
import { pluginReferences, analyser, versHTML, versDocx, versQmd, lireEntete, referencesDansTexte, fixerEntete, listeEntete, referencesReproductibles,
  nomFichierQuarto, sansMarkdown, pageHTML } from '../interface/rendu_document.js';
import { lireBibtex, formaterCitations, joindreReferences, versBibtex, SANS_ZOTERO } from '../interface/bibliographie.js';

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);
const contient = (nom, texte, morceaux) => {
  const absents = morceaux.filter((m) => !texte.includes(m));
  verifier(nom, !absents.length, `absent : ${absents.join(' | ')}`);
};

const md = markdownit({ html: false, linkify: true }).use(pluginReferences);
const texte = `---
title: "Essai"
author: "A. Auteur"
---

# Résultats

Le Q² est de {{M1.Q2}} et le R²Y de {{M1.R2Y:%}}.

{{M1.scores #fig-scores | Scores}}

Voir {{ref:fig-scores}} et {{ref:tbl-res}}.

{{M1.table.resume #tbl-res | Résumé}}

- un point {{M1.N}}
- {{M1.A}}

\`{{M1.Q2}}\` dans du code, \\{{M1.Q2}} échappé, {{< pagebreak >}} dans une phrase.

{{< pagebreak >}}

{{M1.inconnu}}

> {{M1.R2Y |
> sur deux lignes}}

| a | b |
|---|--:|
| x | {{M1.Q2}} |
`;
// (a 1×1 PNG)
const PNG = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==';
const resolus = [
  { genre: 'valeur', texte: '0,65' },
  { genre: 'valeur', texte: '71 %' },
  { genre: 'figure', image: PNG, largeur: 900, hauteur: 580, numero: 1, legende: 'Scores', etiquette: 'fig-scores', nom: 'Scores' },
  { genre: 'renvoi', cible: 'fig-scores', texte: 'Figure 1' },
  { genre: 'renvoi', cible: 'tbl-res', texte: 'Tableau 1' },
  { genre: 'tableau', blocs: [{ texte: 'PLS · A = 2' }, { entetes: ['A', 'R2Y', 'Q2'], lignes: [['1', '0,50', '0,40'], ['2', '0,71', '0,65']] }], numero: 1, legende: 'Résumé', etiquette: 'tbl-res', nom: 'Résumé' },
  { genre: 'valeur', texte: '40' },
  { genre: 'valeur', texte: '2' },
  { erreur: 'M1.inconnu : ce modèle n’a pas cette valeur' },
  { genre: 'valeur', texte: '0,71' },
  { genre: 'valeur', texte: '0,65' },
];
const libelles = { figure: 'Figure', tableau: 'Tableau', seul: 'seul sur sa ligne' };

console.log('Lire le document');
const a = analyser(md, texte);
egal('l’en-tête', a.entete, { title: 'Essai', author: 'A. Auteur' });
egal('les références lues (ni dans le code, ni échappées, ni {{< >}})', a.refs.map((r) => r.meta.ref.cible),
  ['M1.Q2', 'M1.R2Y', 'M1.scores', 'ref:fig-scores', 'ref:tbl-res', 'M1.table.resume', 'M1.N', 'M1.A', 'M1.inconnu', 'M1.R2Y', 'M1.Q2']);
egal('seules sur leur ligne', a.refs.map((r) => !!r.meta.bloc), [false, false, true, false, false, true, false, true, true, true, false]);
egal('en-tête à part', lireEntete('---\ntitle: x\n---\nCorps').corps, 'Corps');

console.log('Aperçu');
const html = versHTML(md, a, resolus, libelles);
contient('valeurs, figure, renvois, tableau', html, ['<span class="ref" title="{{M1.Q2}}">0,65</span>', '<figure class="ref-figure" id="fig-scores"',
  '<b>Figure 1.</b> Scores', '>Figure 1</span> et <span', '<b>Tableau 1.</b> Résumé', '<td class="num">0,65</td>']);
verifier('une figure prend la place de son paragraphe', !/<p>\s*<figure/.test(html) && !/<p>\s*<div class="ref-tableau"/.test(html), html.slice(0, 300));
contient('le code et l’échappé restent tels quels', html, ['<code>{{M1.Q2}}</code>', '{{M1.Q2}} échappé', '{{&lt; pagebreak &gt;}} dans une phrase']);
contient('saut de page, erreur avec sa raison', html, ['<p class="saut-page"></p>', 'class="ref erreur"', '<small>— M1.inconnu']);
contient('dans une liste et dans un tableau', html, ['<li><span class="ref" title="{{M1.A}}">2</span></li>', '<td style="text-align:right"><span class="ref" title="{{M1.Q2}}">0,65</span></td>']);
contient('le titre du document', html, ['<h1 class="titre-document">Essai</h1>', 'A. Auteur']);
// the HTML page (Compile › HTML, and each document's page in a reproducibility package, which its link
// shows first): as wide as a phone's screen, a wide table scrolling on its own
const page = pageHTML(html, 'Essai', 'fr');
contient('la page HTML : à la largeur de l’écran d’un téléphone, ses tableaux défilant de côté', page,
  ['<meta name="viewport" content="width=device-width, initial-scale=1">', '<div class="tableau-defile"><table>', '.tableau-defile { overflow-x: auto; }']);
// a figure's source: the engine's PNG, or the preview's blob: copy of it (documents.js, urlApercu); anything else dropped
const figure = (image) => versHTML(md, analyser(md, '{{M1.scores}}\n'), [{ genre: 'figure', image, numero: 1 }], libelles);
const blob = 'blob:http://127.0.0.1:52817/0b6f2d9e-6c1a-4b0e-9a55-3f1d2e4c5b6a';
contient('la copie de l’aperçu (blob:) gardée', figure(blob), [`<img src="${blob}"`]);
contient('une autre source laissée de côté', figure('https://exemple.invalid/x.png') + figure('javascript:alert(1)'), ['<img src=""', '<img src=""']);
// a plot in a sentence is refused (documents.js gives it this error); the text alone stays a paragraph
const b = analyser(md, 'Texte {{M1.scores}} suite.\n\n{{M1.Q2}}\n');
const html2 = versHTML(md, b, [{ erreur: 'seul' }, { genre: 'valeur', texte: '0,65' }], libelles);
contient('une valeur seule sur sa ligne reste un paragraphe', html2, ['<p><span class="ref" title="{{M1.Q2}}">0,65</span></p>']);

console.log('Word (.docx)');
const octets = versDocx(a, resolus, { langue: 'fr', libelles });
const fichiers = {};
for (let k = 0; k + 30 <= octets.length;) {
  const v = new DataView(octets.buffer, octets.byteOffset + k);
  if (v.getUint32(0, true) !== 0x04034b50) break;
  const taille = v.getUint32(18, true);
  const n = v.getUint16(26, true);
  const nom = new TextDecoder().decode(octets.subarray(k + 30, k + 30 + n));
  fichiers[nom] = octets.subarray(k + 30 + n, k + 30 + n + taille);
  k += 30 + n + taille;
}
egal('ses parties', Object.keys(fichiers).sort(), ['[Content_Types].xml', '_rels/.rels', 'word/_rels/document.xml.rels', 'word/document.xml', 'word/media/figure1.png', 'word/numbering.xml', 'word/styles.xml']);
const doc = new TextDecoder().decode(fichiers['word/document.xml']);
contient('valeurs, légendes, saut de page, erreur en rouge, liste numérotée', doc,
  ['>0,65<', '>Figure 1.<', '>Tableau 1.<', '<w:br w:type="page"/>', '<w:color w:val="B00020"/>', '{{M1.inconnu}}', '<w:numId w:val="1"/>', '<w:pStyle w:val="Title"/>', 'r:embed="rIdImage1"']);
// every tag closed in the right order (a Word file with one wrong tag doesn't open)
const xmlBienForme = (x) => {
  const pile = [];
  for (const m of x.replace(/<\?xml[^>]*\?>/, '').matchAll(/<(\/?)([\w:]+)[^>]*?(\/?)>/g)) {
    if (m[3]) continue;
    if (!m[1]) pile.push(m[2]);
    else if (pile.pop() !== m[2]) return false;
  }
  return !pile.length;
};
verifier('XML bien formé (document, styles, numérotation, liens)', ['word/document.xml', 'word/styles.xml', 'word/numbering.xml', 'word/_rels/document.xml.rels', '[Content_Types].xml']
  .every((f) => xmlBienForme(new TextDecoder().decode(fichiers[f]))));

console.log('Quarto (.qmd)');
egal('les références retrouvées dans le texte, dans l’ordre', referencesDansTexte(lireEntete(texte).corps, a).map((r) => r.i), [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
const q = versQmd(texte, a, resolus, { fichierFigure: (r) => `essai_figures/figure-${r.numero}.png`, langue: 'fr', titre: 'Essai' });
contient('valeurs, figure et tableau étiquetés, renvois Quarto', q.texte, ['Le Q² est de 0,65 et le R²Y de 71 %.', '![Scores](<essai_figures/figure-1.png>){#fig-scores}',
  'Voir @fig-scores et @tbl-res.', '| A | R2Y | Q2 |', ': Résumé {#tbl-res}', '| x | 0,65 |', '- un point 40', '> 0,71']);
contient('le code, l’échappé, les {{< >}} et l’introuvable restent tels quels', q.texte, ['`{{M1.Q2}}`', '\\{{M1.Q2}}', '{{< pagebreak >}}', '{{M1.inconnu}}']);
egal('les introuvables', q.erreurs, ['{{M1.inconnu}}']);
contient('la langue ajoutée à l’en-tête', q.texte, ['---\ntitle: "Essai"\nauthor: "A. Auteur"\nlang: fr\n---']);
// one reference markdown-it doesn't read (in a link's address) doesn't shift the others
const lien = '[lien]({{M1.Q2}}) puis {{M1.N}}.';
egal('une référence dans l’adresse d’un lien ne décale pas les autres', referencesDansTexte(lien, analyser(md, lien)).map((r) => r.brut), ['{{M1.N}}']);

console.log('Quarto reproductible (.qmd)');
// (what documents.js resolves, with each reference's model and path; what the engine answers:
// moteur/R/document_reproductible.R)
const texteR = '---\ntitle: "Essai"\n---\n\nQ² = {{M1.Q2}}, N = {{M1.N}}, AIC {{M2.AIC}}.\n\n{{M1.scores #fig-scores | Scores}}\n\n{{M1.biplot | Biplot}}\n\n'
  + '{{M1.table.resume #tbl-res | Résumé}}\n\n{{M1.methodes}}\n\n- dans une liste : {{M1.scores | Encore}}\n\n| a |\n|---|\n| {{M1.Q2}} |\n';
const aR = analyser(md, texteR);
const resR = [
  { genre: 'valeur', texte: '0,65', modele: 'm1', chemin: ['Q2'], forme: { forme: 'nombre', x: 0.6512, decimales: 2, significatifs: null } },
  { genre: 'valeur', texte: '40', modele: 'm1', chemin: ['N'], forme: { forme: 'nombre', x: 40, decimales: 0 } },
  { genre: 'valeur', texte: '171,0', modele: 'm2', chemin: ['AIC'], forme: { forme: 'nombre', x: 171.01, decimales: 1 } },
  { genre: 'figure', image: PNG, largeur: 900, hauteur: 580, numero: 1, legende: 'Scores', etiquette: 'fig-scores', nom: 'Scores', modele: 'm1', chemin: ['scores'], vue: 'scores', variante: {} },
  { genre: 'figure', image: PNG, largeur: 900, hauteur: 580, numero: 2, legende: 'Biplot', etiquette: '', nom: 'Biplot', modele: 'm1', chemin: ['biplot'], vue: 'biplot', variante: {} },
  { genre: 'tableau', blocs: [{ entetes: ['A', 'Q2'], lignes: [['1', '0,40']] }], numero: 1, legende: 'Résumé', etiquette: 'tbl-res', nom: 'Résumé', modele: 'm1', chemin: ['table', 'resume'], cle: 'resume' },
  { genre: 'texte', paragraphes: ['Une PLS a été ajustée.'], modele: 'm1', chemin: ['methodes'] },
  { genre: 'figure', image: PNG, largeur: 900, hauteur: 580, numero: 3, legende: 'Encore', etiquette: '', nom: 'Scores', modele: 'm1', chemin: ['scores'], vue: 'scores', variante: {} },
  { genre: 'valeur', texte: '0,65', modele: 'm1', chemin: ['Q2'], forme: { forme: 'nombre', x: 0.6512, decimales: 2 } },
];
const envoi = referencesReproductibles(resR);
const refsEnvoyees = JSON.parse(envoi.references);
egal('ce que le moteur reçoit : les modèles cités, et chaque référence sans ses nombres', [envoi.modeles, refsEnvoyees[0], refsEnvoyees[3].vue, refsEnvoyees[5].cle, refsEnvoyees[6].genre],
  [['m1', 'm2'], { modele: 'm1', chemin: ['Q2'], genre: 'valeur', forme: { forme: 'nombre', decimales: 2, significatifs: null }, texte: '0,65' }, 'scores', 'resume', 'texte']);
const scores = ['nuee_dans(M1, {', '  plot(m$T[, 1], m$T[, 2])', '})'];
const rep = {
  installation: ['```{r}', '#| label: nuee-modeles', '#| include: false', 'source("Essai_R/nuee.R", encoding = "UTF-8")', 'M1 <- nuee_refaire("Essai_R/M1.R")', '```'],
  references: [{ code: 'nuee_nombre(M1$composantes$Q2cum[2])' }, { code: 'nuee_nombre(M1$N, 0)' }, { raison: 'pas d’export vers R' },
    { lignes: scores, largeur: 6, hauteur: 5 }, { raison: 'le script ne dessine pas ce graphique' }, { lignes: ['nuee_dans(M1, knitr::kable(composantes, digits = 3))'] },
    { raison: 'le texte des méthodes -- tel quel' }, { lignes: scores, largeur: 6, hauteur: 5 }, { code: 'nuee_nombre(M1$composantes$Q2cum[2])' }],
};
const qr = versQmd(texteR, aR, resR, { fichierFigure: (r) => `Essai_figures/figure-${r.numero}.png`, langue: 'fr', titre: 'Essai', reproductible: rep });
contient('le bloc du début après l’en-tête, les valeurs en R en ligne, le reste en dur', qr.texte, ['lang: fr\n---\n\n```{r}\n#| label: nuee-modeles\n#| include: false\nsource("Essai_R/nuee.R"',
  'Q² = `r nuee_nombre(M1$composantes$Q2cum[2])`, N = `r nuee_nombre(M1$N, 0)`, AIC 171,0.', '| `r nuee_nombre(M1$composantes$Q2cum[2])` |']);
contient('un graphique : un bloc R étiqueté, sa légende, sa taille', qr.texte, ['```{r}\n#| label: fig-scores\n#| fig-cap: "Scores"\n#| fig-width: 6\n#| fig-height: 5\n#| echo: false\nnuee_dans(M1, {\n  plot(m$T[, 1], m$T[, 2])\n})\n```']);
contient('un tableau : knitr::kable dans un bloc étiqueté', qr.texte, ['```{r}\n#| label: tbl-res\n#| tbl-cap: "Résumé"\n#| echo: false\nnuee_dans(M1, knitr::kable(composantes, digits = 3))\n```']);
contient('ce qui reste en dur : l’image, le texte, chacun après un commentaire', qr.texte, ['<!-- Nuée : le script ne dessine pas ce graphique -->\n\n![Biplot](<Essai_figures/figure-2.png>){#fig-nuee-2}',
  '<!-- Nuée : le texte des méthodes — tel quel -->\n\nUne PLS a été ajustée.']);
contient('un bloc dans une liste : l’image, pas de bloc R', qr.texte, ['- dans une liste : ![Encore](<Essai_figures/figure-3.png>){#fig-nuee-3}']);
egal('les figures écrites en images', qr.images.map((x) => x.numero), [2, 3]);
egal('sans « reproductible », le .qmd d’avant', versQmd(texteR, aR, resR, { fichierFigure: (r) => `f/${r.numero}.png`, langue: 'fr' }).texte.includes('```{r}'), false);

console.log('Citations');
const bib = new Map(lireBibtex(String.raw`
@article{wold2001, author = {Wold, Svante and Sj{\"o}str{\"o}m, Michael and Eriksson, Lennart}, title = {PLS-regression}, journal = {Chemom. Intell. Lab.}, year = 2001, volume = 58, pages = {109--130}, doi = {10.1016/S0169-7439(01)00155-1}}
@book{legendre2012, author = {Legendre, P. and Legendre, L.}, title = {Numerical ecology}, publisher = {Elsevier}, year = 2012}
`).entrees.map((e) => [e.cle, e]));
const docCite = `---
title: Essai
bibliography: [refs.bib, autre.bib]
csl: ecology.csl
---

# Méthodes

La PLS [voir @wold2001, p. 3; @legendre2012] et @legendre2012 [chap. 9], pas a@b.fr ni \\@wold2001, ni [un lien](https://x.org) ni [@wold2001](https://y.org), ni @inconnue.

- @wold2001
`;
const ac = analyser(md, docCite);
egal('les citations lues (pas l’adresse, l’échappée, le lien)', ac.citations.map((t) => [t.meta.narratif, t.meta.cites.map((c) => c.cle).join('+'), t.meta.cites[0].suffixe]),
  [[false, 'wold2001+legendre2012', ', p. 3'], [true, 'legendre2012', 'chap. 9'], [true, 'inconnue', ''], [true, 'wold2001', '']]);
egal('en-tête : une liste en ligne', listeEntete(ac.entete.bibliography), ['refs.bib', 'autre.bib']);
const ct = formaterCitations(ac.citations.map((t) => t.meta), bib, { langue: 'fr' });
const hc = versHTML(md, ac, [], { ...libelles, references: 'Références' }, ct);
contient('aperçu : citations, inconnue en rouge, titre et liste ajoutés à la fin', hc, ['>(voir Wold et al., 2001, p. 3; Legendre et Legendre, 2012)</span>',
  '>Legendre et Legendre (2012, chap. 9)</span>', 'class="cite erreur"', '<li><span class="cite"', '<h1>Références</h1><div class="bibliographie">',
  '<i>Chemom. Intell. Lab.</i>', '<a href="https://doi.org/10.1016/S0169-7439(01)00155-1">', '<a href="https://y.org">', 'a@b.fr']);
const avecPlace = analyser(md, '# Résultats\n\nComme @wold2001.\n\n{{bibliographie}}\n\n# Annexe\n\nTexte.\n');
const hp = versHTML(md, avecPlace, [], libelles, formaterCitations(avecPlace.citations.map((t) => t.meta), bib, { langue: 'en', style: 'numerique' }));
verifier('la liste où {{bibliographie}} la met, numérotée', /Comme <span class="cite"[^>]*>Wold et al\. \[1\]<\/span>[\s\S]*<div class="bibliographie"><p class="reference" id="ref-wold2001"><span class="numero">\[1\]<\/span>[\s\S]*<h1>Annexe/.test(hp)
  && !/<p><div/.test(hp), hp.slice(0, 600));
const sousTitre = analyser(md, '# Intro\n\n[@wold2001]\n\n# Références\n');
verifier('sous un dernier titre « Références », pas de titre en plus', !/<h1>Références<\/h1><div/.test(versHTML(md, sousTitre, [], { ...libelles, references: 'Références' },
  formaterCitations(sousTitre.citations.map((t) => t.meta), bib, {}))));
const dc = new TextDecoder().decode(Object.entries((() => {
  const o = versDocx(ac, [], { langue: 'fr', libelles: { ...libelles, references: 'Références' }, citations: ct });
  const f = {};
  for (let k = 0; k + 30 <= o.length;) {
    const v = new DataView(o.buffer, o.byteOffset + k);
    if (v.getUint32(0, true) !== 0x04034b50) break;
    const taille = v.getUint32(18, true);
    const n = v.getUint16(26, true);
    f[new TextDecoder().decode(o.subarray(k + 30, k + 30 + n))] = o.subarray(k + 30 + n, k + 30 + n + taille);
    k += 30 + n + taille;
  }
  return f;
})()).find(([n]) => n === 'word/document.xml')[1]);
contient('Word : citations, liste en style Bibliographie, revue en italique, DOI en lien', dc, ['(voir Wold et al., 2001, p. 3; Legendre et Legendre, 2012)',
  '<w:pStyle w:val="Bibliographie"/>', '<w:i/></w:rPr><w:t xml:space="preserve">Chemom. Intell. Lab.', '<w:hyperlink r:id="rIdLien', '>Références<']);
const qc = versQmd(docCite, ac, [], { fichierFigure: () => '', langue: 'fr', titre: 'Essai', bibliographie: 'essai.bib', csl: 'ecology.csl', libelles: { references: 'Références' } });
contient('Quarto : citations laissées, en-tête refait, titre de la liste', qc.texte, ['[voir @wold2001, p. 3; @legendre2012]', 'bibliography: essai.bib', 'csl: ecology.csl', 'lang: fr', '# Références {.unnumbered}']);
const avecCiteproc = (t) => versQmd(t, analyser(md, t), [], { fichierFigure: () => '', bibliographie: 'essai.bib' }).texte.match(/^citeproc:.*$/gm);
egal('Quarto : citeproc pour tous les formats (le PDF par Typst aussi), sans changer celui du document', [avecCiteproc('Texte [@wold2001].\n'),
  avecCiteproc('---\nciteproc: false\n---\nTexte [@wold2001].\n')], [['citeproc: true'], ['citeproc: false']]);
verifier('Quarto : l’ancienne liste de fichiers remplacée', !/refs\.bib|autre\.bib/.test(qc.texte), qc.texte.slice(0, 200));
egal('{{bibliographie}} devient la place de Quarto', versQmd('Texte [@wold2001].\n\n{{bibliographie}}\n', analyser(md, 'Texte [@wold2001].\n\n{{bibliographie}}\n'), [], { fichierFigure: () => '' }).texte.split('\n').slice(-3), ['::: {#refs}', ':::', '']);
egal('en-tête : une clé ajoutée, remplacée, ou une liste', [fixerEntete('---\ntitle: x\n---\n', 'csl', 'a b.csl'), fixerEntete('---\nbibliography:\n  - a.bib\n  - b.bib\ntitle: x\n---\n', 'bibliography', ['c.bib'])],
  ['---\ntitle: x\ncsl: "a b.csl"\n---\n', '---\nbibliography:\n  - c.bib\ntitle: x\n---\n']);

// The references of a Methods text cited join the document's list (documents.js: citer; the
// references cited from the .bib once): one list, the text's {{M1.methodes.references}} showing
// nothing where it stands; in Quarto, in the .bib with nocite, under the list's heading.
console.log('Les références du texte des méthodes dans la liste du document');
const docMethodes = '# Méthodes\n\nLa PLS [@wold2001].\n\n{{M1.methodes}}\n\n# Logiciels\n\n{{M1.methodes.references}}\n';
const am = analyser(md, docMethodes);
const refsMethodes = ['Wold, S., Sjöström, M., & Eriksson, L. (2001). PLS-regression: a basic tool of chemometrics. Chemometrics and Intelligent Laboratory Systems, 58(2), 109–130. https://doi.org/10.1016/S0169-7439(01)00155-1',
  'R Core Team (2026). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/'];
const resM = [{ genre: 'texte', paragraphes: ['La PLS (Wold et al., 2001) a été faite avec R (R Core Team, 2026).'], references: refsMethodes },
  { genre: 'texte', paragraphes: [], references: refsMethodes }];
const jm = joindreReferences(bib, refsMethodes);
const cm = formaterCitations(am.citations.map((t) => t.meta), jm.entrees, { langue: 'fr', ajoutees: jm.ajoutees });
const hm = versHTML(md, am, resM, { ...libelles, references: 'Références' }, cm);
egal('une seule liste : wold2001 du .bib, R du texte des méthodes', [(hm.match(/class="bibliographie"/g) || []).length, cm.liste.map((x) => x.cle), cm.nonCitees], [1, ['rcoreteam2026'.replace(/^/, 'nuee:'), 'wold2001'], ['nuee:rcoreteam2026']]);
verifier('{{M1.methodes.references}} ne montre rien là où il est', /<h1>Logiciels<\/h1>\s*<div class="ref-texte"[^>]*><\/div>\s*<h1>Références<\/h1><div class="bibliographie">/.test(hm), hm.slice(-900));
const qm = versQmd(docMethodes, am, resM, { fichierFigure: () => '', langue: 'fr', titre: 'Essai', bibliographie: 'essai.bib', libelles: { references: 'Références' }, nonCitees: cm.nonCitees });
contient('Quarto : nocite pour celles que le texte ne cite pas, la liste sous son titre', qm.texte, ['nocite: "@nuee:rcoreteam2026"', 'bibliography: essai.bib', '# Références {.unnumbered}']);
verifier('Quarto : rien à la place de {{M1.methodes.references}}', !/methodes\.references|Nuée :/.test(qm.texte) && /# Logiciels\n\n# Références/.test(qm.texte), qm.texte);
const seulesMethodes = analyser(md, '{{M1.methodes}}\n');
const qs = versQmd('{{M1.methodes}}\n', seulesMethodes, [resM[0]], { fichierFigure: () => '', bibliographie: 'essai.bib', libelles: { references: 'References' }, nonCitees: jm.ajoutees });
contient('sans citation dans le texte, la liste vient quand même', qs.texte, ['nocite: "@wold2001, @nuee:rcoreteam2026"', '# References {.unnumbered}']);
egal('le nocite du document gardé, les leurs ajoutés', versQmd('---\nnocite: "@x"\n---\n{{M1.methodes}}\n', analyser(md, '---\nnocite: "@x"\n---\n{{M1.methodes}}\n'), [resM[0]],
  { fichierFigure: () => '', bibliographie: 'essai.bib', nonCitees: ['nuee:r'] }).texte.match(/^nocite:.*$/gm), ['nocite: "@x, @nuee:r"']);

// Odd titles and file names for Quarto: the names of the files beside a .qmd escaped in its header
// (bibliography: and csl: are read as Markdown), its figures' folder without # nor % (their
// address is a URL), a title from the title field written as it is (Markdown's signs escaped), a
// template's title a valid YAML string, read back by the preview; then rendered by Quarto when it
// is installed.
console.log('Titres et noms de fichiers pour Quarto');
egal('le dossier des figures', ['rapport #2', 'taux 50%', 'Résultats été (v2)', ''].map(nomFichierQuarto), ['rapport -2', 'taux 50-', 'Résultats été (v2)', 'document']);
egal('les noms de fichiers de l’en-tête : laissés quand rien n’y est du Markdown, sinon échappés', ['essai.bib', 'Résultats été (v2).bib', "d'Alembert & fils.bib", 'a--b.bib', 'x_y.bib', '1. Bilan.bib']
  .map(sansMarkdown), ['essai.bib', 'Résultats été (v2).bib', "d\\'Alembert \\& fils\\.bib", 'a\\-\\-b\\.bib', 'x\\_y\\.bib', '1\\. Bilan\\.bib']);
const titre = 'Site @Nord *2* A\\B';
const qt = versQmd('Texte.\n', analyser(md, 'Texte.\n'), [], { fichierFigure: () => '', titre });
egal('un titre du champ Titre : ses signes de Markdown échappés, en YAML', qt.texte.split('\n')[1], 'title: "Site \\\\@Nord \\\\*2\\\\* A\\\\\\\\B"');
egal('l’en-tête relu : les échappements d’une chaîne YAML entre guillemets', [lireEntete('---\ntitle: "Le \\"bon\\" site A\\\\B"\n---\n').entete.title, lireEntete("---\ntitle: 'a'\n---\n").entete.title],
  ['Le "bon" site A\\B', 'a']);
const quarto = [process.env.QUARTO_PATH, ...(process.env.PATH || '').split(path.delimiter).map((d) => path.join(d, process.platform === 'win32' ? 'quarto.exe' : 'quarto')),
  path.join(process.env.LOCALAPPDATA || '', 'Programs', 'Quarto', 'bin', 'quarto.exe'), '/usr/local/bin/quarto', '/opt/quarto/bin/quarto', '/Applications/quarto/bin/quarto']
  .filter(Boolean).find((f) => { try { return fs.statSync(f).isFile(); } catch { return false; } });
if (!quarto) console.log('  (Quarto absent : pas de rendu)');
else {
  const ici = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-quarto-'));
  const nom = 'Suivi de l\'été #2 -- relevés';
  const texteQ = 'Le suivi [@wold2001].\n\n{{M1.scores | Scores}}\n\n{{M1.methodes}}\n';
  const aq = analyser(md, texteQ);
  const jq = joindreReferences(bib, refsMethodes.slice(1));
  const cq = formaterCitations(aq.citations.map((t) => t.meta), jq.entrees, { langue: 'fr', ajoutees: jq.ajoutees });
  const png = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==', 'base64');
  const figures = `${nomFichierQuarto(nom)}_figures`;
  const qq = versQmd(texteQ, aq, [{ genre: 'figure', image: '', largeur: 900, hauteur: 580, numero: 1, legende: 'Scores', etiquette: '' },
    { genre: 'texte', paragraphes: ['Avec R (R Core Team, 2026).'], references: refsMethodes.slice(1) }],
  { fichierFigure: (x) => `${figures}/figure-${x.numero}.png`, langue: 'fr', titre: 'Suivi @Nord', bibliographie: `${nom}.bib`, libelles: { references: 'Références' }, nonCitees: cq.nonCitees });
  fs.writeFileSync(path.join(ici, `${nom}.bib`), cq.liste.map((x) => versBibtex(jq.entrees.get(x.cle), SANS_ZOTERO)).join('\n\n'));
  fs.mkdirSync(path.join(ici, figures));
  fs.writeFileSync(path.join(ici, figures, 'figure-1.png'), png);
  fs.writeFileSync(path.join(ici, `${nom}.qmd`), qq.texte);
  // (as main.js runs it: from the .qmd's folder, by its file name)
  const r = spawnSync(quarto, ['render', `${nom}.qmd`, '--to', 'html'], { cwd: ici, encoding: 'utf8', windowsHide: true });
  const html = fs.existsSync(path.join(ici, `${nom}.html`)) ? fs.readFileSync(path.join(ici, `${nom}.html`), 'utf8') : '';
  const src = (/<img src="([^"]*)"/.exec(html) || [])[1];
  const titreRendu = (/<h1 class="title">([^<]*)<\/h1>/.exec(html) || [])[1];
  verifier('rendu par Quarto : un nom à apostrophe, dièse et tirets, ses références, sa figure, son titre tel qu’écrit',
    r.status === 0 && /Wold/.test(html) && /R Core Team/.test(html) && !!src && decodeURI(src) === `${figures}/figure-1.png` && titreRendu === 'Suivi @Nord'
      && !/WARN/.test(`${r.stdout}${r.stderr}`), `${r.status} ${src} ${titreRendu} ${`${r.stderr}`.split('\n').filter((l) => /WARN|ERROR/.test(l)).slice(0, 3).join(' | ')}`);
  fs.rmSync(ici, { recursive: true, force: true });
}

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
