// Windows: is Nuée installed for every user of the computer, or for this user only? The updater
// runs the next installer the same way (main.js: installerPuisRelancer). The installer (NSIS)
// lists Nuée in Settings › Apps under HKLM when it installs for all users, under HKCU when it
// installs for one user, and that entry names the folder (its uninstaller, its icon). The folder
// alone can't tell: an administrator may install for all users outside Program Files. Without an
// entry (a copy unpacked by hand), the folder decides, as before: Program Files is for all users.
const { execFileSync } = require('child_process');

const DESINSTALLATION = 'Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall';

// Does a value of an uninstall entry under `racine` (HKLM, HKCU) hold `texte`? Both registry
// views: a 32-bit installer writes to the 32-bit one.
function registreContient(racine, texte) {
  return ['/reg:64', '/reg:32'].some((vue) => {
    try {
      execFileSync('reg', ['query', `${racine}\\${DESINSTALLATION}`, '/s', '/f', texte, '/d', vue], { stdio: 'ignore', windowsHide: true, timeout: 15000 });
      return true;  // (exit code 0: at least one match)
    } catch {
      return false;
    }
  });
}

// `dossier`: Nuée's folder. The folder followed by "\" finds this installation's entry and not
// that of a folder whose name starts the same way (Nuée2).
function pourTousLesUtilisateurs(dossier, { contient = registreContient, env = process.env } = {}) {
  const motif = `${dossier.replace(/[\\/]+$/, '')}\\`;
  if (contient('HKLM', motif)) return true;
  if (contient('HKCU', motif)) return false;
  const programmes = [env.ProgramFiles, env.ProgramW6432, env['ProgramFiles(x86)']].filter(Boolean)
    .map((p) => `${p.toLowerCase().replace(/[\\/]+$/, '')}\\`);
  return programmes.some((p) => motif.toLowerCase().startsWith(p));
}

module.exports = { pourTousLesUtilisateurs, registreContient };
