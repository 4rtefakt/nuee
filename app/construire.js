// Builds the program for the system it runs on.
// Windows: Electron + a portable R with its packages + the R engine + the .usp converter, in
// one folder that runs without installing anything (Nuee/, for the NAS), then the installer
// installateur/Nuee-Setup-<version>.exe made from that folder.
//   node construire.js <output folder> [R folder]
// The converter is compiled with PyInstaller when outils/export_project.exe is missing or was
// compiled from other sources than convertisseur/*.py (outils/export_project.sources: their hash).
// macOS: Nuée.app (Electron + CRAN's R with its packages, made to run from the application by
// r-macos.js + the R engine + the converter, compiled as on Windows) in Nuee/, signed ad hoc,
// then installateur/Nuee-<version>-<arch>.dmg. The R folder is CRAN's, installed from its .pkg.
//   node construire.js <output folder> [R folder]     (PYTHON=<python> to compile the converter)
// Linux: Electron + the R engine + the converter as a Python script, in Nuee/ (./nuee starts
// it), then installateur/ with a .deb, an AppImage and a .tar.gz made from that folder. R is
// not brought along: an R build is tied to the libraries of its distribution, so the system's
// R is used (the .deb depends on it and on the packages the engine loads).
//   node construire.js <output folder>        (RSCRIPT=<Rscript> for another R than the PATH's)
// tests/essai_api.js runs first (interface syntax, main requests of the R server): a failure
// stops the build (SANS_ESSAI=1 skips it). SANS_INSTALLATEUR=1 skips the installer.
//   node construire.js --installateur <packaged application> <output folder>: the installer alone.
const { packager } = require('@electron/packager');
const builder = require('electron-builder');
const { execFileSync } = require('child_process');
const crypto = require('crypto');
const fs = require('fs');
const path = require('path');

const depot = path.join(__dirname, '..');
const sortie = path.resolve(process.argv[2] || path.join(depot, 'dist'));
const windows = process.platform === 'win32';
const mac = process.platform === 'darwin';
const dossierR = windows ? process.argv[3] || path.join(process.env.LOCALAPPDATA, 'Programs', 'R', 'R-4.6.2')
  : mac ? process.argv[3] || '/Library/Frameworks/R.framework/Resources' : null;
const rscript = windows ? path.join(dossierR, 'bin', 'Rscript.exe') : mac ? path.join(dossierR, 'bin', 'Rscript') : process.env.RSCRIPT || 'Rscript';
// (that Rscript starts the R of RHOME: the one given, wherever it is)
if (mac) process.env.RHOME = dossierR;
const arch = windows || process.arch !== 'arm64' ? 'x64' : 'arm64';

// Parts of R the program never uses (manuals, tests, C headers for compiling packages, example
// files, Tcl/Tk, the uninstaller, message translations other than French and English; on
// macOS, debug symbols and the framework's own copies of R and Rscript, outside bin/).
const inutile = (rel) => /^(doc|tests|Tcl)([\\/]|$)|^unins/i.test(rel)
  || /^library[\\/][^\\/]+[\\/](doc|html|tests|include)([\\/]|$)/i.test(rel)
  || /^library[\\/](readr|readxl|vroom)[\\/](extdata|bench|words)([\\/]|$)/i.test(rel)
  || /^library[\\/]tcltk([\\/]|$)/i.test(rel)
  || /^library[\\/]translations[\\/](?!(fr|en|en_GB|en@quot|Meta|DESCRIPTION)([\\/]|$))[^\\/]+([\\/]|$)/i.test(rel)
  || /\.dSYM([\\/]|$)/.test(rel) || /^(R|Rscript)$/.test(rel);

// R packages to bring: base R, the recommended packages (for the R console; foreign reads dBase
// and Weka files) and what the engine loads with its dependencies (haven: SPSS, Stata, SAS;
// readxl: .xls; RSQLite: GeoPackage; ranger: random forests). Others installed in the local R
// (testthat...) stay out, and so do those only needed to compile a package (LinkingTo: cpp11,
// progress). One of the engine's packages missing from R's own library stops the build: the
// program would be shipped without it.
function paquetsR() {
  const moteur = 'c("httpuv", "jsonlite", "openxlsx", "haven", "readxl", "RSQLite", "ranger")';
  const code = 'ip <- installed.packages(lib.loc = .Library); '
    + `manque <- setdiff(${moteur}, rownames(ip)); `
    + 'if (length(manque)) stop("not in R\'s own library (install.packages(..., lib = .Library)): ", paste(manque, collapse = ", "), call. = FALSE); '
    + 'std <- rownames(ip)[ip[, "Priority"] %in% c("base", "recommended")]; '
    + `dep <- unlist(tools::package_dependencies(${moteur}, db = ip, recursive = TRUE, which = c("Depends", "Imports"))); `
    + `writeLines(union(std, c(${moteur}, "translations", dep)))`;
  return new Set(execFileSync(rscript, ['-e', code], { encoding: 'utf8' }).split(/\r?\n/).filter(Boolean));
}

function copier(src, dest, filtre = () => true, base = src) {
  fs.mkdirSync(dest, { recursive: true });
  for (const n of fs.readdirSync(src, { withFileTypes: true })) {
    const s = path.join(src, n.name);
    const rel = path.relative(base, s);
    if (!filtre(rel)) continue;
    const d = path.join(dest, n.name);
    // (macOS: R's framework links a few files and folders; what they point to is copied, and a
    // link to nothing, outside an R installed by its .pkg, left out)
    if (n.isSymbolicLink() && !fs.existsSync(s)) continue;
    if (n.isDirectory() || (n.isSymbolicLink() && fs.statSync(s).isDirectory())) {
      fs.mkdirSync(d, { recursive: true });
      copier(s, d, filtre, base);
    } else {
      fs.copyFileSync(s, d);
    }
  }
}

async function construire() {
  execFileSync(process.execPath, [path.join(__dirname, 'copier-vendor.js')], { stdio: 'inherit' });
  await require('./changelog').ecrireChangelog();  // CHANGELOG.md follows interface/nouveautes.js
  if (!process.env.SANS_ESSAI) {
    execFileSync(process.execPath, [path.join(__dirname, 'tests', 'essai_api.js'), rscript], { stdio: 'inherit' });
  }
  // icone/nuee.png stays: the windows' icon on Linux
  const [dossier] = await packager({
    dir: __dirname, out: sortie, overwrite: true, platform: windows ? 'win32' : mac ? 'darwin' : 'linux', arch,
    // (macOS: Nuée.app, so that the menu bar and the Dock say Nuée)
    name: mac ? 'Nuée' : 'Nuee', executableName: windows ? 'Nuee' : mac ? 'Nuée' : 'nuee', asar: false,
    icon: path.join(__dirname, 'icone', mac ? 'nuee.icns' : 'nuee.ico'),
    win32metadata: { ProductName: 'Nuée', FileDescription: 'Nuée', CompanyName: '4rtefakt', InternalName: 'Nuee', OriginalFilename: 'Nuee.exe' },
    // macOS: .nuee projects open in Nuée when double-clicked in the Finder (main.js: open-file), with
    // its icon. CRAN's packages for R 4.6 on Apple silicon need macOS 14 (Electron 45: 13).
    ...(mac ? { appBundleId: 'io.github.4rtefakt.nuee', appCopyright: '4rtefakt', extendInfo: { CFBundleDocumentTypes: [
      { CFBundleTypeName: 'Projet Nuée', CFBundleTypeExtensions: ['nuee'], CFBundleTypeRole: 'Editor', LSHandlerRank: 'Owner', CFBundleTypeIconFile: 'electron.icns' }],
    ...(arch === 'arm64' ? { LSMinimumSystemVersion: '14.0' } : {}) } } : {}),
    ignore: [/^\/node_modules/, /^\/tests/, /^\/construire\.js$/, /^\/copier-vendor\.js$/, /^\/r-macos\.js$/, /^\/package-lock\.json$/, /\.cmd$/, /^\/icone\/(?!nuee\.png$)/],
  });
  const appli = mac ? path.join(dossier, 'Nuée.app') : null;
  const ressources = mac ? path.join(appli, 'Contents', 'Resources') : path.join(dossier, 'resources');
  // Chromium's interface languages: the application is in French and English (on macOS, a
  // folder per language in Electron's framework, and the application's own)
  if (mac) {
    const cadre = path.join(appli, 'Contents', 'Frameworks', 'Electron Framework.framework', 'Resources');
    for (const d of [cadre, ressources]) {
      for (const f of fs.readdirSync(d)) if (/\.lproj$/.test(f) && !['fr.lproj', 'en.lproj'].includes(f)) fs.rmSync(path.join(d, f), { recursive: true });
    }
  } else {
    for (const f of fs.readdirSync(path.join(dossier, 'locales'))) {
      if (!['fr.pak', 'en-US.pak'].includes(f)) fs.rmSync(path.join(dossier, 'locales', f));
    }
  }

  if (windows || mac) {
    console.log('R portable…');
    const paquets = paquetsR();
    const paquetUtile = (rel) => { const m = /^library[\\/]([^\\/]+)/.exec(rel); return !m || paquets.has(m[1]); };
    copier(dossierR, path.join(ressources, 'R'), (rel) => !inutile(rel) && paquetUtile(rel));
    if (mac) console.log(`R déplaçable : ${require('./r-macos').rendreDeplacable(path.join(ressources, 'R'))} fichiers changés`);
  }

  console.log('moteur…');
  copier(path.join(depot, 'moteur'), path.join(ressources, 'moteur'));
  // the example project (made-up data) and its table
  copier(path.join(depot, 'exemple'), path.join(ressources, 'exemple'), (rel) => /^(orchards|vergers)\.(json|csv)$/.test(rel));
  // the full user guide, opened from Aide › Guide d'utilisation
  for (const f of ['LISEZ-MOI-APPLICATION.txt', 'USER-GUIDE.txt']) fs.copyFileSync(path.join(depot, f), path.join(ressources, f));

  if (!windows && !mac) {
    // the converter needs only Python's standard library: python3 runs it (moteur/serveur.R)
    copier(path.join(depot, 'convertisseur'), path.join(ressources, 'convertisseur'), (rel) => /^\w+\.py$/.test(rel));
    await construireLinux(dossier);
    return;
  }

  // (compiled again when its sources change: an old converter would read the files otherwise
  // than the engine computes, as the coefficients with missing values did between 0.9.8 and 0.9.9)
  // (macOS has no Python of its own: compiled there too)
  const exe = path.join(depot, 'outils', windows ? 'export_project.exe' : 'export_project');
  const empreinte = path.join(depot, 'outils', 'export_project.sources');
  const h = crypto.createHash('sha256');
  for (const f of fs.readdirSync(path.join(depot, 'convertisseur')).filter((x) => /\.py$/.test(x)).sort()) {
    h.update(f).update(fs.readFileSync(path.join(depot, 'convertisseur', f)));
  }
  const sources = h.digest('hex');
  const aJour = fs.existsSync(exe) && fs.existsSync(empreinte) && fs.readFileSync(empreinte, 'utf8').trim() === sources;
  if (!aJour) {
    console.log('convertisseur .usp (PyInstaller)…');
    const tmp = path.join(sortie, 'pyinstaller');
    execFileSync(process.env.PYTHON || (windows ? 'python' : 'python3'), ['-m', 'PyInstaller', '--onefile', '--noconfirm', '--clean', '--name', 'export_project',
      '--distpath', path.join(depot, 'outils'), '--workpath', tmp, '--specpath', tmp,
      '--exclude-module', 'openpyxl', '--exclude-module', 'matplotlib', '--exclude-module', 'numpy',
      '--exclude-module', 'tkinter', path.join(depot, 'convertisseur', 'export_project.py')], { stdio: 'inherit', cwd: depot });
    fs.rmSync(tmp, { recursive: true, force: true });
    fs.writeFileSync(empreinte, `${sources}\n`);
  }
  fs.mkdirSync(path.join(ressources, 'outils'), { recursive: true });
  fs.copyFileSync(exe, path.join(ressources, 'outils', path.basename(exe)));
  if (mac) {
    await construireMac(dossier);
    return;
  }
  // uninstaller of the copy the NAS launcher makes (Settings › Apps runs it)
  const crlf = (f) => fs.readFileSync(path.join(__dirname, f), 'utf8').replace(/\r?\n/g, '\r\n');
  fs.writeFileSync(path.join(ressources, 'desinstaller.cmd'), crlf('desinstaller.cmd'));

  // What goes on the NAS: the launcher (Electron cannot start from a network share, so it
  // copies the application to the PC and starts it there), the user guide, application/.
  const final = path.join(sortie, 'Nuee');
  fs.rmSync(final, { recursive: true, force: true });
  fs.mkdirSync(final);
  fs.renameSync(dossier, path.join(final, 'application'));
  fs.writeFileSync(path.join(final, 'Lancer Nuee.cmd'), crlf('Lancer Nuee.cmd'));
  fs.copyFileSync(path.join(depot, 'LISEZ-MOI-APPLICATION.txt'), path.join(final, 'LISEZ-MOI.txt'));
  fs.copyFileSync(path.join(depot, 'USER-GUIDE.txt'), path.join(final, 'USER-GUIDE.txt'));
  console.log('Prêt :', final);

  if (process.env.SANS_INSTALLATEUR) return;
  await construireInstallateur(path.join(final, 'application'), path.join(sortie, 'installateur'));
}

// The installer for everyone else, from the packaged application `application` into `dossier`: for
// this user (no administrator rights needed) or for all users, Start menu and desktop shortcuts,
// listed in Settings › Apps with its uninstaller. installateur.nsh: an installation over Nuée's
// folder keeps its shortcuts and taskbar pin. (Alone: node construire.js --installateur
// <application> <output folder>, as tests/essai_mise_a_jour.js does with a stand-in application.)
async function construireInstallateur(application, dossier) {
  console.log('installateur…');
  const fichiers = await builder.build({
    targets: builder.Platform.WINDOWS.createTarget('nsis', builder.Arch.x64),
    prepackaged: application,
    projectDir: __dirname,
    config: {
      appId: 'io.github.4rtefakt.nuee', productName: 'Nuée', executableName: 'Nuee', copyright: '4rtefakt', compression: 'maximum',
      directories: { output: dossier },
      win: { icon: path.join(__dirname, 'icone', 'nuee.ico') },
      // .nuee projects open in Nuée when double-clicked, with its icon
      fileAssociations: [{ ext: 'nuee', name: 'Nuee.Projet', description: 'Nuée project', icon: path.join(__dirname, 'icone', 'nuee.ico') }],
      nsis: {
        oneClick: false, perMachine: false, allowToChangeInstallationDirectory: true,
        createDesktopShortcut: true, createStartMenuShortcut: true, shortcutName: 'Nuée', uninstallDisplayName: 'Nuée',
        installerIcon: path.join(__dirname, 'icone', 'nuee.ico'), uninstallerIcon: path.join(__dirname, 'icone', 'nuee.ico'),
        installerSidebar: path.join(__dirname, 'icone', 'installateur.bmp'), uninstallerSidebar: path.join(__dirname, 'icone', 'installateur.bmp'),
        // (Windows's language when it is one of them, English otherwise: the first one)
        installerLanguages: ['en_US', 'fr_FR'], language: '1033', artifactName: 'Nuee-Setup-${version}.exe',
        include: path.join(__dirname, 'installateur.nsh'),
      },
    },
  });
  console.log('Installateur :', fichiers.filter((f) => f.endsWith('.exe')).join(', '));
}

// macOS: Nuee/ with Nuée.app and the user guide, then a .dmg: Nuée.app beside a link to
// Applications, to drag it there. Without an Apple Developer ID, the application is signed ad
// hoc: an Apple silicon Mac runs nothing unsigned, and the packager changed Electron's files,
// and their signatures with them. macOS then asks the first time to allow it (README.md).
async function construireMac(dossier) {
  const final = path.join(sortie, 'Nuee');
  fs.rmSync(final, { recursive: true, force: true });
  fs.renameSync(dossier, final);
  const appli = path.join(final, 'Nuée.app');
  fs.copyFileSync(path.join(depot, 'LISEZ-MOI-APPLICATION.txt'), path.join(final, 'LISEZ-MOI.txt'));
  fs.copyFileSync(path.join(depot, 'USER-GUIDE.txt'), path.join(final, 'USER-GUIDE.txt'));
  execFileSync('codesign', ['--force', '--deep', '--sign', '-', appli], { stdio: 'inherit' });
  execFileSync('codesign', ['--verify', '--deep', '--strict', appli], { stdio: 'inherit' });
  console.log('Prêt :', final);

  if (process.env.SANS_INSTALLATEUR) return;
  console.log('image disque…');
  const fichiers = await builder.build({
    targets: builder.Platform.MAC.createTarget('dmg', builder.Arch[arch]),
    prepackaged: appli,
    projectDir: __dirname,
    config: {
      appId: 'io.github.4rtefakt.nuee', productName: 'Nuée', copyright: '4rtefakt',
      directories: { output: path.join(sortie, 'installateur') },
      mac: { icon: path.join(__dirname, 'icone', 'nuee.icns'), identity: null },
      dmg: { title: 'Nuée', artifactName: 'Nuee-${version}-${arch}.${ext}' },
    },
  });
  console.log('Image disque :', fichiers.filter((f) => f.endsWith('.dmg')).join(', '));
}

// Linux: the folder Nuee/ (./nuee starts it), then a .deb (Debian, Ubuntu…: R and its packages
// come with it), an AppImage and a .tar.gz (any distribution, R installed apart).
async function construireLinux(dossier) {
  const final = path.join(sortie, 'Nuee');
  fs.rmSync(final, { recursive: true, force: true });
  fs.renameSync(dossier, final);
  fs.copyFileSync(path.join(depot, 'LISEZ-MOI-APPLICATION.txt'), path.join(final, 'LISEZ-MOI.txt'));
  fs.copyFileSync(path.join(depot, 'USER-GUIDE.txt'), path.join(final, 'USER-GUIDE.txt'));
  // The packager builds in a folder made by mkdtemp (mode 700): installed as is, /opt/Nuée
  // would open for root alone. Everything readable by all, folders and programs runnable.
  execFileSync('chmod', ['-R', 'a+rX', final]);
  console.log('Prêt :', final);

  if (process.env.SANS_INSTALLATEUR) return;
  console.log('paquets Linux…');
  const fichiers = await builder.build({
    targets: builder.Platform.LINUX.createTarget(['deb', 'AppImage', 'tar.gz'], builder.Arch[arch]),
    prepackaged: final,
    projectDir: __dirname,
    config: {
      appId: 'io.github.4rtefakt.nuee', productName: 'Nuée', executableName: 'nuee', copyright: '4rtefakt',
      directories: { output: path.join(sortie, 'installateur') },
      linux: {
        icon: path.join(__dirname, 'icone', 'nuee.png'), category: 'Science', maintainer: '4rtefakt <4rtefakt@kore.sh>',
        synopsis: 'PCA, PLS, PLS-DA and OPLS computed in plain R', syncDesktopName: true,
        artifactName: 'Nuee-${version}-${arch}.${ext}',
      },
      // .nuee projects open in Nuée (MIME type in the menu entry and the .deb)
      fileAssociations: [{ ext: 'nuee', name: 'Nuee.Projet', description: 'Nuée project', mimeType: 'application/x-nuee' }],
      // electron-builder's own list (it is replaced, not added to) with libgbm1 and libasound2,
      // which Electron needs too (a minimal Ubuntu 22.04 lacks libgbm1), then R and the engine's
      // packages (foreign, for dBase and Weka files, nlme and mgcv, for mixed, generalized and
      // additive models, MASS, for a curve's thresholds and the LDA, rpart, for decision trees, and
      // cluster, for k-means silhouettes, are among R's recommended packages, which Debian keeps
      // out of r-base-core; RSQLite reads and writes GeoPackage files and ranger grows random forests;
      // the others are for the R console, as on Windows)
      deb: {
        packageCategory: 'science',
        depends: ['libgtk-3-0', 'libnotify4', 'libnss3', 'libxss1', 'libxtst6', 'xdg-utils', 'libatspi2.0-0', 'libuuid1', 'libsecret-1-0',
          'libgbm1', 'libasound2',
          'r-base-core', 'r-cran-httpuv', 'r-cran-jsonlite', 'r-cran-openxlsx', 'r-cran-haven', 'r-cran-readxl', 'r-cran-foreign', 'r-cran-nlme', 'r-cran-mgcv',
          'r-cran-mass', 'r-cran-rpart', 'r-cran-cluster', 'r-cran-rsqlite', 'r-cran-ranger'],
        recommends: ['python3', 'r-recommended'],
      },
    },
  });
  console.log('Paquets :', fichiers.filter((f) => /\.(deb|AppImage|tar\.gz)$/.test(f)).join(', '));
}

(process.argv[2] === '--installateur' ? construireInstallateur(path.resolve(process.argv[3]), path.resolve(process.argv[4])) : construire())
  .catch((e) => { console.error(e); process.exit(1); });
