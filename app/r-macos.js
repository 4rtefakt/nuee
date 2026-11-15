// macOS: CRAN's R, brought along in Nuée.app (construire.js), made to run from wherever the
// application is. CRAN builds R to sit in /Library/Frameworks/R.framework: every program and
// library of R, and of the packages built for it, names the libraries it loads by their path
// there, and bin/R names its own folder there. Here they name them from where they are
// (@loader_path/…), and bin/R finds its folder from its own path. The libraries keep their own
// names (/Library/…/libR.dylib): a package the researcher installs from the R console, built
// for CRAN's R, asks for that name, and the system gives it the libR already loaded.
// Each file changed is signed again (ad hoc): an Apple silicon Mac doesn't run a program, or
// load a library, whose signature no longer matches.
//   node r-macos.js <R folder>      (the R folder of an R expanded from CRAN's .pkg, for example)
const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const CADRE = /^\/Library\/Frameworks\/R\.framework\/(?:Versions\/[^/]+\/)?Resources\/(.+)$/;
// What may be loaded from outside: the system, and XQuartz (/opt/X11), which only the X11 and
// cairo graphics devices load, as with CRAN's R installed (the engine draws with Quartz)
const SYSTEME = /^(\/usr\/lib\/|\/System\/Library\/|\/opt\/X11\/|@loader_path\/)/;

// The name `dependance` that `fichier` (in the R folder `dossierR`) gives a library, as it must
// be once R is moved: '@loader_path/…' for a library of R itself, null for one of the system.
// Throws for a library that would be missing on another Mac (/opt/R/…, Homebrew's…).
function nouveauNom(fichier, dependance, dossierR) {
  const m = CADRE.exec(dependance);
  if (m) return `@loader_path/${path.posix.relative(path.posix.dirname(fichier), path.posix.join(dossierR, m[1]))}`;
  if (SYSTEME.test(dependance)) return null;
  throw new Error(`${path.posix.relative(dossierR, fichier)} charge ${dependance}, absent d'un autre Mac`);
}

// bin/R, the script Rscript starts: its folder found from its own path.
function scriptR(texte) {
  const ligne = 'R_HOME_DIR="/Library/Frameworks/R.framework/Resources"';
  const nouvelle = 'R_HOME_DIR="$(cd "$(dirname "$0")/.." && pwd)"';
  if (!texte.includes(ligne) && !texte.includes(nouvelle)) throw new Error('bin/R : R_HOME_DIR introuvable');
  return texte.replace(ligne, nouvelle).replaceAll('"/Library/Frameworks/R.framework/Resources/', '"${R_HOME}/');
}

// A program or a library (Mach-O, or several in one file), by its first bytes
function machO(fichier) {
  const octets = Buffer.alloc(8);
  const fd = fs.openSync(fichier, 'r');
  try { if (fs.readSync(fd, octets, 0, 8, 0) < 8) return false; } finally { fs.closeSync(fd); }
  const magique = octets.readUInt32BE(0);
  if (magique === 0xcafebabe) return octets.readUInt32BE(4) < 20;  // (not a Java class, whose version comes there)
  return [0xfeedface, 0xfeedfacf, 0xcefaedfe, 0xcffaedfe].includes(magique);
}

// the libraries a file loads (otool -l: its load commands, not its own name)
function dependances(fichier) {
  const sortie = execFileSync('otool', ['-l', fichier], { encoding: 'utf8', maxBuffer: 64e6 });
  const noms = [];
  let charge = false;
  for (const l of sortie.split('\n')) {
    const cmd = /^\s*cmd (\S+)/.exec(l);
    if (cmd) charge = ['LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB', 'LC_LAZY_LOAD_DYLIB', 'LC_LOAD_UPWARD_DYLIB'].includes(cmd[1]);
    const nom = charge && /^\s*name (.+) \(offset \d+\)$/.exec(l);
    if (nom) noms.push(nom[1]);
  }
  return [...new Set(noms)];
}

function fichiers(dossier) {
  return fs.readdirSync(dossier, { withFileTypes: true }).flatMap((e) => {
    const f = path.join(dossier, e.name);
    return e.isDirectory() ? fichiers(f) : e.isFile() ? [f] : [];
  });
}

function rendreDeplacable(dossierR) {
  dossierR = path.resolve(dossierR);
  const script = path.join(dossierR, 'bin', 'R');
  fs.writeFileSync(script, scriptR(fs.readFileSync(script, 'utf8')));
  let changes = 0;
  for (const f of fichiers(dossierR).filter(machO)) {
    const noms = dependances(f).map((d) => [d, nouveauNom(f, d, dossierR)]).filter(([, n]) => n);
    if (!noms.length) continue;
    execFileSync('install_name_tool', [...noms.flatMap(([a, n]) => ['-change', a, n]), f], { stdio: 'pipe' });
    execFileSync('codesign', ['--force', '--sign', '-', f], { stdio: 'pipe' });
    changes += 1;
  }
  return changes;
}

if (require.main === module) console.log(`${rendreDeplacable(process.argv[2])} fichiers de R changés`);
module.exports = { rendreDeplacable, nouveauNom, scriptR };
