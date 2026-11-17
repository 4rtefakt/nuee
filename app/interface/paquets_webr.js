// The R packages the engine loads, for the browser version: webR has R's base packages only, and
// moteur_webr.js installs these from webR's repository (repo.r-wasm.org, built for webR 0.6.1 and
// R 4.6), or from the site's copy of it (miroir_webr.mjs, as on nuee.io), with what they depend on. The desktop's list (construire.js: paquetsR), less httpuv
// (no server in the page) and those of SANS_WEBR. tests/essai_moteur_webr.mjs installs them too,
// to run the engine's checks in webR. One webR can't install leaves only its features out: the
// engine says they need the desktop application (moteur/R/navigateur.R).
export const PAQUETS_WEBR = [
  'jsonlite',  // the requests and the project files
  'nlme', 'mgcv', 'MASS', 'rpart', 'cluster',  // models (R's recommended packages: not in webR)
  'foreign', 'openxlsx', 'readxl', 'haven', 'zip',  // dBase and Weka, Excel, SPSS, Stata and SAS, jamovi
];

// The engine's packages left out of the browser version, and why. Never installed: a package
// whose library webR can't load leaves webR unable to load any library after it (Emscripten
// keeps its missing symbol), the models' too.
export const SANS_WEBR = {
  RSQLite: 'GeoPackage files: its webR build (3.53.3) needs OpenSSL (X509_free), which webR 0.6.1 lacks',
  ranger: 'random forests: its webR build (0.18.0) loads, then stops at its first forest, "thread constructor failed: Not supported" (webR has no threads)',
};
