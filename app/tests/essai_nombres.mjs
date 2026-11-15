// Checks interface/nombres.js (the interface's numbers for the reader), without the application:
// a decimal comma in French, the point in English exactly as before (Number.toFixed), a number's
// text changed once only, and the separator between two numbers.
//   node tests/essai_nombres.mjs
import { fmt, decimal, entreNombres } from '../interface/nombres.js';
import { changerLangue, langueActuelle } from '../interface/i18n.js';

// (i18n.js puts the language on the page: none here)
globalThis.document ??= { documentElement: {} };

let echecs = 0;
const verifier = (nom, ok, detail = '') => {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${ok || !detail ? '' : ` — ${detail}`}`);
  if (!ok) echecs += 1;
};
const egal = (nom, obtenu, attendu) => verifier(nom, JSON.stringify(obtenu) === JSON.stringify(attendu), `${JSON.stringify(obtenu)} au lieu de ${JSON.stringify(attendu)}`);

// the values of the sweeps: small, large, negative, round, from a fixed seed
let graine = 7;
const hasard = () => { graine = (graine * 16807) % 2147483647; return graine / 2147483647; };
const valeurs = [0, -0, 1, -1, 0.5, 0.05, 0.004, 120, 1e-7, 123456.789, -98765.4321, 1e15, 2.675]
  .concat(Array.from({ length: 300 }, () => (hasard() - 0.5) * 10 ** Math.floor(hasard() * 12 - 6)));

console.log('En anglais : comme avant');
changerLangue('en');
egal('la langue', langueActuelle(), 'en');
verifier('fmt(v, d) = Number(v).toFixed(d), pour 313 valeurs et 0 à 6 décimales',
  valeurs.every((v) => [0, 1, 2, 3, 4, 5, 6].every((d) => fmt(v, d) === Number(v).toFixed(d))));
egal('trois décimales par défaut', fmt(0.9591234), '0.959');
egal('valeur manquante : vide', [fmt(null), fmt(undefined), fmt(NaN)], ['', '', '']);
egal('decimal : un texte inchangé', [decimal('1.5e-7'), decimal(0.25), decimal('p = 0.05.')], ['1.5e-7', '0.25', 'p = 0.05.']);
egal('entre deux nombres', entreNombres(), ', ');

console.log('En français : la virgule décimale');
changerLangue('fr');
egal('la langue', langueActuelle(), 'fr');
egal('fmt', [fmt(0.9591234), fmt(-1.5, 1), fmt(2, 3), fmt(120, 0), fmt(12345.678, 2), fmt(-0.0001, 2)],
  ['0,959', '-1,5', '2,000', '120', '12345,68', '-0,00']);
verifier('fmt(v, d) = Number(v).toFixed(d), sa virgule à la place du point, pour 313 valeurs et 0 à 6 décimales',
  valeurs.every((v) => [0, 1, 2, 3, 4, 5, 6].every((d) => fmt(v, d) === Number(v).toFixed(d).replace('.', ','))));
verifier('relu par le moteur (la virgule pour le point) : le même nombre qu’en anglais',
  valeurs.every((v) => [0, 2, 4].every((d) => Number(fmt(v, d).replace(',', '.')) === Number(Number(v).toFixed(d)))));
egal('valeur manquante : vide', [fmt(null), fmt(undefined), fmt(NaN)], ['', '', '']);
egal('les grands nombres sans séparateur de milliers, comme les textes du moteur', fmt(1234567.5, 1), '1234567,5');
egal('l’écriture scientifique', [decimal((1.5e-7).toExponential(1)), decimal('1e-16'), decimal(Number(1e21).toFixed(2))], ['1,5e-7', '1e-16', '1e+21']);
egal('decimal prend un nombre ou son texte', [decimal(0.25), decimal('0.25'), decimal(-3), decimal(+(0.000123456).toPrecision(4))], ['0,25', '0,25', '-3', '0,0001235']);
egal('seul un point entre deux chiffres change : un texte français reste tel quel', [decimal('0,25'), decimal(fmt(0.25, 2)), decimal('p = 0.05.'), decimal('M1.Q2')],
  ['0,25', '0,25', 'p = 0,05.', 'M1.Q2']);
egal('plusieurs nombres dans un texte', decimal('12.46 – 34.09'), '12,46 – 34,09');
egal('entre deux nombres : un point-virgule', `${fmt(0.123)}${entreNombres()}${fmt(-0.456)}`, '0,123 ; -0,456');

console.log('De nouveau en anglais');
changerLangue('en');
egal('le point revient', [fmt(0.9591234), decimal('1.5'), entreNombres()], ['0.959', '1.5', ', ']);

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
