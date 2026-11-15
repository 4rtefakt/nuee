// Checks r-macos.js: the names R's programs and libraries give the libraries they load, once R is
// brought along in Nuée.app, and the folder bin/R finds for itself. (The build then uses R from
// the application: tests/essai_appli.js.)
//   node tests/essai_r_macos.js
const { nouveauNom, scriptR } = require('../r-macos');

let echecs = 0;
function verifier(ok, nom) {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}`);
  if (!ok) echecs += 1;
}
const leve = (f) => { try { f(); return null; } catch (e) { return e.message; } };

const R = '/Applications/Nuée.app/Contents/Resources/R';
const cran = (f) => `/Library/Frameworks/R.framework/Versions/4.6/Resources/lib/${f}`;
verifier(nouveauNom(`${R}/bin/exec/R`, cran('libR.dylib'), R) === '@loader_path/../../lib/libR.dylib', 'le programme R : libR depuis bin/exec');
verifier(nouveauNom(`${R}/lib/libR.dylib`, cran('libgfortran.5.dylib'), R) === '@loader_path/libgfortran.5.dylib', 'une bibliothèque de R : sa voisine');
verifier(nouveauNom(`${R}/library/httpuv/libs/httpuv.so`, cran('libR.dylib'), R) === '@loader_path/../../../lib/libR.dylib', 'un paquet : libR trois dossiers plus haut');
verifier(nouveauNom(`${R}/lib/libR.dylib`, '/Library/Frameworks/R.framework/Resources/lib/libRblas.dylib', R) === '@loader_path/libRblas.dylib',
  'le chemin par Resources, sans Versions/…');
verifier(nouveauNom(`${R}/modules/lapack.so`, '/Library/Frameworks/R.framework/Versions/4.6-arm64/Resources/lib/libRlapack.dylib', R)
  === '@loader_path/../lib/libRlapack.dylib', 'le dossier de version des R jusqu’à 4.5 (4.5-arm64)');
verifier(['/usr/lib/libSystem.B.dylib', '/System/Library/Frameworks/CoreFoundation.framework/Versions/A/CoreFoundation', '/opt/X11/lib/libX11.6.dylib',
  '@loader_path/../../lib/libR.dylib'].every((d) => nouveauNom(`${R}/bin/exec/R`, d, R) === null), 'le système, XQuartz et un nom déjà changé restent');
verifier(/library\/haven\/libs\/haven\.so charge \/opt\/R\/arm64\/lib\/libz\.dylib/.test(leve(() => nouveauNom(`${R}/library/haven/libs/haven.so`, '/opt/R/arm64/lib/libz.dylib', R))),
  'une bibliothèque absente d’un autre Mac arrête la construction, en la nommant');
verifier(!!leve(() => nouveauNom(`${R}/bin/exec/R`, '/opt/homebrew/lib/libintl.8.dylib', R)), 'Homebrew aussi');

const avant = ['#!/bin/sh', 'R_HOME_DIR="/Library/Frameworks/R.framework/Resources"', 'if test "${R_HOME_DIR}" = "/Library/Frameworks/lib/R"; then', 'fi',
  'R_HOME="${R_HOME_DIR}"', 'R_SHARE_DIR="/Library/Frameworks/R.framework/Resources/share"', 'R_DOC_DIR="/Library/Frameworks/R.framework/Resources/doc"'].join('\n');
const apres = scriptR(avant);
verifier(apres.includes('R_HOME_DIR="$(cd "$(dirname "$0")/.." && pwd)"'), 'bin/R : son dossier trouvé depuis son propre chemin');
verifier(apres.includes('R_SHARE_DIR="${R_HOME}/share"') && apres.includes('R_DOC_DIR="${R_HOME}/doc"') && !apres.includes('R.framework'),
  'bin/R : share et doc dans ce dossier');
verifier(scriptR(apres) === apres, 'bin/R : changé une fois (une deuxième construction ne le change plus)');
verifier(!!leve(() => scriptR('#!/bin/sh\nR_HOME_DIR=/usr/lib/R\n')), 'bin/R : un script d’une autre forme arrête la construction');

console.log(echecs ? `${echecs} échec(s)` : 'Toutes les vérifications sont passées.');
process.exit(echecs ? 1 : 0);
