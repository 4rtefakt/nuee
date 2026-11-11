// Checks installation.js: how the updater tells an installation for all users from one for this
// user (Windows). The decision with stand-in registries, then reg.exe itself.
//   node tests/essai_installation.js
const { pourTousLesUtilisateurs, registreContient } = require('../installation');

let echecs = 0;
function verifier(ok, nom) {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}`);
  if (!ok) echecs += 1;
}

const env = { ProgramFiles: 'C:\\Program Files', ProgramW6432: 'C:\\Program Files', 'ProgramFiles(x86)': 'C:\\Program Files (x86)' };
// a registry holding one uninstall entry, under `racine`, for the folder `dossier`
const registre = (racine, dossier) => {
  const vus = [];
  const contient = (r, motif) => { vus.push(motif); return r === racine && `${dossier}\\Uninstall Nuée.exe`.toLowerCase().includes(motif.toLowerCase()); };
  return { contient, vus };
};

let r = registre('HKLM', 'D:\\Logiciels\\Nuée');
verifier(pourTousLesUtilisateurs('D:\\Logiciels\\Nuée', { contient: r.contient, env }) === true,
  'installé pour tous hors de Program Files (son entrée sous HKLM) : pour tous');
verifier(r.vus.every((m) => m === 'D:\\Logiciels\\Nuée\\'), 'le dossier cherché finit par « \\ » (pas un dossier au nom qui commence pareil)');
r = registre('HKLM', 'D:\\Logiciels\\Nuée');
verifier(pourTousLesUtilisateurs('D:\\Logiciels\\Nuée2', { contient: r.contient, env }) === false, 'Nuée2 ne prend pas l’entrée de Nuée');
r = registre('HKCU', 'C:\\Users\\a\\AppData\\Local\\Programs\\Nuée');
verifier(pourTousLesUtilisateurs('C:\\Users\\a\\AppData\\Local\\Programs\\Nuée', { contient: r.contient, env }) === false,
  'installé pour cet utilisateur (son entrée sous HKCU) : pour lui');
r = registre('HKCU', 'C:\\Program Files\\Nuée');
verifier(pourTousLesUtilisateurs('C:\\Program Files\\Nuée\\', { contient: r.contient, env }) === false,
  'l’entrée l’emporte sur le dossier (et une barre finale ne compte pas)');
const aucun = { contient: () => false, env };
verifier(pourTousLesUtilisateurs('C:\\Program Files\\Nuée', aucun) === true && pourTousLesUtilisateurs('c:\\program files (x86)\\Nuée', aucun) === true
  && pourTousLesUtilisateurs('C:\\Users\\a\\AppData\\Local\\Nuee', aucun) === false && pourTousLesUtilisateurs('C:\\Program Files Nuée', aucun) === false,
'sans entrée (une copie dépaquetée) : Program Files dit pour tous, comme avant');

if (process.platform === 'win32') {
  verifier(registreContient('HKCU', 'Z:\\Sûrement\\absent de ce registre\\') === false, 'reg.exe : un dossier absent n’est pas trouvé');
  verifier(registreContient('HKLM', ':\\') === true, 'reg.exe : une entrée qui le contient est trouvée');
}

console.log(echecs ? `${echecs} vérification(s) en échec` : 'Tout est bon');
process.exit(echecs ? 1 : 0);
