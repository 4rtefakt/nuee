// Checks interface/rendu_document.js (a document turned into its preview, a Word file or a Quarto
// file), without the application: the references markdown-it finds (not in code, not escaped, not
// Quarto's {{< >}}), those alone on their line, the preview, the .docx's parts and the .qmd's text.
//   node tests/essai_documents.mjs
import markdownit from 'markdown-it';
import { pluginReferences, analyser, versHTML, versDocx, versQmd, lireEntete, referencesDansTexte } from '../interface/rendu_document.js';

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

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
