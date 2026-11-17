// Checks interface/aide_texte.js (Help), without the application: the documentation site's
// addresses in each language, the sentence to cite Nuée, and the full user guide as the help panel
// reads it (USER-GUIDE.txt, LISEZ-MOI-APPLICATION.txt): its sections, the section of each kind of
// model and view, in both guides, the paragraph of a view, its text in blocks, links and a search.
//   node tests/essai_aide.mjs
import fs from 'fs';
import { adresseGuide, adresseTutoriels, adresseTutoriel, citation, DOI, SITE, DEPOT, PROBLEMES, sectionsGuide, largeurGuide, blocsSection, alias,
  morceaux, motsRecherche, occurrences, chercher, sectionPour, blocCible, SECTIONS } from '../interface/aide_texte.js';
import { NOUVEAUTES, VERSION } from '../interface/nouveautes.js';

const depot = new URL('../../', import.meta.url);
const lire = (f) => fs.readFileSync(new URL(f, depot), 'utf8');

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);

console.log('Addresses and citation');
egal('the documentation site in each language (docs/site/construire.js: PAGES)',
  [adresseGuide('en'), adresseGuide('fr'), adresseTutoriels('en'), adresseTutoriels('fr')],
  ['https://4rtefakt.github.io/nuee/en/guide/', 'https://4rtefakt.github.io/nuee/fr/guide/', 'https://4rtefakt.github.io/nuee/en/tutorials/', 'https://4rtefakt.github.io/nuee/fr/tutoriels/']);
egal('a tutorial’s page', [adresseTutoriel('suivi', 'en'), adresseTutoriel('suivi', 'fr'), adresseTutoriel('baci', 'fr')],
  ['https://4rtefakt.github.io/nuee/en/tutorials/trends.html', 'https://4rtefakt.github.io/nuee/fr/tutoriels/tendances.html', 'https://4rtefakt.github.io/nuee/fr/tutoriels/baci.html']);
egal('the websites', [SITE, DEPOT, PROBLEMES], ['https://nuee.io', 'https://github.com/4rtefakt/nuee', 'https://github.com/4rtefakt/nuee/issues/new/choose']);
egal('README’s sentence, for the version running and its year', citation('1.1.0', '2026-11-16'),
  'Boutin, C. (2026). Nuée (version 1.1.0) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.24263014');
// (About writes the sentence README and README.fr give, for this version)
const date = (NOUVEAUTES.find((n) => n.version === VERSION) || {}).date;
for (const f of ['README.md', 'README.fr.md']) {
  const phrase = (lire(f).match(/^> (Boutin, C\. .*)$/m) || [])[1];
  egal(`About cites Nuée as ${f} does`, citation(VERSION, date), (phrase || '').trim());
}
verifier('the concept DOI of CITATION.cff', new RegExp(`^doi: "${DOI.replace(/\./g, '\\.')}"`, 'm').test(lire('CITATION.cff')));
verifier('without a date, this year', citation('9.9.9', null).includes(`(${new Date().getFullYear()})`));

console.log('The guide');
const guides = { en: sectionsGuide(lire('USER-GUIDE.txt')), fr: sectionsGuide(lire('LISEZ-MOI-APPLICATION.txt')) };
verifier('both guides have the same number of sections', guides.en.length === guides.fr.length && guides.en.length > 50, `${guides.en.length} / ${guides.fr.length}`);
const absents = Object.entries(SECTIONS).filter(([, [f, e]]) => {
  const i = guides.en.findIndex((s) => s.titre === e);
  return i < 0 || !guides.fr[i] || guides.fr[i].titre !== f;
});
verifier('every section the help panel opens at is a section of both guides, at the same place', !absents.length, absents.map(([k]) => k).join(', '));
const numero = (l, titre) => guides[l].findIndex((s) => s.titre === titre);
egal('the section of a model type, a view, nothing in front',
  [sectionPour({ type: 'TRIM', langue: 'en' }, guides.en), sectionPour({ type: 'TRIM', langue: 'fr' }, guides.fr), sectionPour({ vue: 'selection_aic', type: 'GLM', langue: 'en' }, guides.en),
    sectionPour({ vue: 'resume', type: 'DISTANCE', langue: 'fr' }, guides.fr), sectionPour({ langue: 'en' }, guides.en), sectionPour({ vue: 'carte', langue: 'fr' }, guides.fr)],
  [numero('en', 'TRIM: indices and trends of counts'), numero('en', 'TRIM: indices and trends of counts'), numero('en', 'Linear, generalized and mixed models'),
    numero('en', 'Distance sampling: density and abundance'), numero('en', 'Opening'), numero('en', 'Maps and spatial data')]);

// a section's paragraphs and list items, as the panel shows them (their texts, in order)
const textes = (liste) => liste.flatMap((b) => (b.type === 'p' ? b.textes : b.type === 'ul' || b.type === 'ol' ? b.elements.flatMap(textes)
  : b.type === 'groupe' || b.type === 'retrait' ? textes(b.blocs) : b.type === 'dl' ? b.elements.map((e) => e.def) : [b.texte]));
const section = (l, titre) => guides[l][numero(l, titre)];
const blocsDe = (l, titre) => blocsSection(section(l, titre).lignes, largeurGuide(guides[l]));
const paragrapheDe = (l, titre, contexte) => { const x = textes(blocsDe(l, titre)); const k = blocCible(x, contexte); return k < 0 ? null : x[k]; };
const aic = paragrapheDe('en', 'Linear, generalized and mixed models', { libelle: 'AIC selection', chemin: 'Plots › AIC selection' });
verifier('a view’s paragraph: the one that starts with its menu and its name', !!aic && aic.startsWith('Plots › AIC selection (also under each regression'), aic && aic.slice(0, 60));
const aicFr = paragrapheDe('fr', 'Modèles linéaires, généralisés et mixtes', { libelle: 'Sélection par AIC', chemin: 'Graphiques › Sélection par AIC' });
verifier('… in French', !!aicFr && aicFr.startsWith('Graphiques › Sélection par AIC (aussi sous chaque régression'), aicFr && aicFr.slice(0, 60));
const comparer = paragrapheDe('en', 'Linear, generalized and mixed models', { libelle: 'Compare models', chemin: 'Predict › Compare models' });
verifier('… whatever the menu’s name in the guide', !!comparer && /^Prediction › Compare models\b/.test(comparer) && comparer.includes('Q2 and RMSEcv'), comparer && comparer.slice(0, 60));
const indices = paragrapheDe('en', 'TRIM: indices and trends of counts', { libelle: 'Indices', chemin: null });
verifier('… a view of its own analysis, by its name', !!indices && /\bIndices \(the imputed indices/.test(indices), indices && indices.slice(0, 80));
verifier('… none for a name the section doesn’t have', blocCible(['Plots › Scores: …'], { libelle: 'Loadings', chemin: 'Plots › Loadings' }) === -1);

const menus = blocsDe('en', 'Menus and toolbar');
verifier('a table of shortcuts stays preformatted', menus.some((b) => b.type === 'pre' && b.texte.includes('Ctrl+O open') && b.texte.includes('\n')), JSON.stringify(menus.map((b) => b.type)));
const ouvrir = blocsDe('en', 'Opening');
verifier('an indented list, its items’ lines joined', ouvrir.some((b) => ['retrait', 'ul', 'groupe'].includes(b.type)), JSON.stringify(ouvrir.map((b) => b.type)));
const trim = textes(blocsDe('en', 'TRIM: indices and trends of counts'));
verifier('a paragraph’s lines joined into sentences', trim.some((x) => x.includes('the standard of European bird monitoring), with missing counts: the model imputes them.')), trim[0] && trim[0].slice(0, 80));

const renvois = guides.en.flatMap((s, i) => alias(s.titre).map((a) => [a, i])).sort((a, b) => b[0].length - a[0].length);
const m = morceaux('Groups: see Cross-validation, or https://example.org/a.html.', { renvois, mots: motsRecherche('validation') });
egal('a paragraph’s pieces: its lead-in, a section it refers to, a web address, the words searched for',
  m.map((x) => [x.genre, x.texte, x.marque, x.section ?? x.url ?? null]),
  [['entree', 'Groups:', false, null], ['texte', ' see ', false, null], ['renvoi', 'Cross-', false, numero('en', 'Cross-validation')], ['renvoi', 'validation', true, numero('en', 'Cross-validation')],
    ['texte', ', or ', false, null], ['url', 'https://example.org/a.html', false, 'https://example.org/a.html'], ['texte', '.', false, null]]);
egal('a search without accents nor capitals finds the text as written', occurrences('Le modèle Résumé, résumé.', motsRecherche('RESUME')), [[10, 16], [18, 24]]);
const t1 = chercher(guides.en, 'trim');
verifier('a search: the section with the words in its title first', t1.length > 1 && t1[0].section === numero('en', 'TRIM: indices and trends of counts') && /TRIM/i.test(t1[0].extrait), t1.slice(0, 3).map((r) => guides.en[r.section].titre));
const t2 = chercher(guides.fr, 'validation croisée');
verifier('… in French, every word in the section', t2.length > 0 && t2[0].section === numero('fr', 'Validation croisée'), t2.slice(0, 3).map((r) => guides.fr[r.section].titre));
egal('… nothing for a word the guide hasn’t, nor for one letter', [chercher(guides.en, 'zzqx').length, chercher(guides.en, 'a').length], [0, 0]);

if (echecs) { console.log(`${echecs} échec(s)`); process.exit(1); }
console.log('Tout est bon.');
