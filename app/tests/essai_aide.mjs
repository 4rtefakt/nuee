// Checks interface/aide_texte.js (Help), without the application: the documentation site's
// addresses in each language, the sentence to cite Nuée.
//   node tests/essai_aide.mjs
import fs from 'fs';
import { adresseGuide, adresseTutoriels, adresseTutoriel, citation, DOI, SITE, DEPOT, PROBLEMES } from '../interface/aide_texte.js';
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

if (echecs) { console.log(`${echecs} échec(s)`); process.exit(1); }
console.log('Tout est bon.');
