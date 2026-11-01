// Builds the program for the system it runs on.
// Windows: Electron + a portable R with its packages + the R engine + the .usp converter, in
// one folder that runs without installing anything (Nuee/, for the NAS), then the installer
// installateur/Nuee-Setup-<version>.exe made from that folder.
//   node construire.js <output folder> [R folder]
// The converter is compiled with PyInstaller if outils/export_project.exe is missing.
// Linux: Electron + the R engine + the converter as a Python script, in Nuee/ (./nuee starts
// it), then installateur/ with a .deb, an AppImage and a .tar.gz made from that folder. R is
// not brought along: an R build is tied to the libraries of its distribution, so the system's
// R is used (the .deb depends on it and on the packages the engine loads).
//   node construire.js <output folder>        (RSCRIPT=<Rscript> for another R than the PATH's)
// tests/essai_api.js runs first (interface syntax, main requests of the R server): a failure
// stops the build (SANS_ESSAI=1 skips it). SANS_INSTALLATEUR=1 skips the installer.
const { packager } = require('@electron/packager');
const builder = require('electron-builder');
const { execFileSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const depot = path.join(__dirname, '..');
const sortie = path.resolve(process.argv[2] || path.join(depot, 'dist'));
const windows = process.platform === 'win32';
const dossierR = windows ? process.argv[3] || path.join(process.env.LOCALAPPDATA, 'Programs', 'R', 'R-4.6.2') : null;
const rscript = windows ? path.join(dossierR, 'bin', 'Rscript.exe') : process.env.RSCRIPT || 'Rscript';
const arch = windows || process.arch !== 'arm64' ? 'x64' : 'arm64';

// Parts of R the program never uses (manuals, tests, C headers for compiling packages, example
// files, Tcl/Tk, the uninstaller, message translations other than French and English).
const inutile = (rel) => /^(doc|tests|Tcl)([\\/]|$)|^unins/i.test(rel)
  || /^library[\\/][^\\/]+[\\/](doc|html|tests|include)([\\/]|$)/i.test(rel)
  || /^library[\\/](readr|readxl|vroom)[\\/](extdata|bench|words)([\\/]|$)/i.test(rel)
  || /^library[\\/]tcltk([\\/]|$)/i.test(rel)
  || /^library[\\/]translations[\\/](?!(fr|en|en_GB|en@quot|Meta|DESCRIPTION)([\\/]|$))[^\\/]+([\\/]|$)/i.test(rel);

// R packages to bring: base R, the recommended packages (for the R console; foreign reads dBase
// and Weka files) and what the engine loads with its dependencies (haven: SPSS, Stata, SAS;
// readxl: .xls). Others installed in the local R (testthat...) stay out, and so do those only
// needed to compile a package (LinkingTo: cpp11, progress).
function paquetsR() {
  const code = 'ip <- installed.packages(lib.loc = .Library); '
    + 'std <- rownames(ip)[ip[, "Priority"] %in% c("base", "recommended")]; '
    + 'dep <- unlist(tools::package_dependencies(c("httpuv", "jsonlite", "openxlsx", "haven", "readxl"), db = ip, recursive = TRUE, which = c("Depends", "Imports"))); '
    + 'writeLines(union(std, c("httpuv", "jsonlite", "openxlsx", "haven", "readxl", "translations", dep)))';
  return new Set(execFileSync(rscript, ['-e', code], { encoding: 'utf8' }).split(/\r?\n/).filter(Boolean));
}

function copier(src, dest, filtre = () => true, base = src) {
  fs.mkdirSync(dest, { recursive: true });
  for (const n of fs.readdirSync(src, { withFileTypes: true })) {
    const s = path.join(src, n.name);
    const rel = path.relative(base, s);
    if (!filtre(rel)) continue;
    const d = path.join(dest, n.name);
    if (n.isDirectory()) {
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
    dir: __dirname, out: sortie, overwrite: true, platform: windows ? 'win32' : 'linux', arch,
    name: 'Nuee', executableName: windows ? 'Nuee' : 'nuee', asar: false, icon: path.join(__dirname, 'icone', 'nuee.ico'),
    win32metadata: { ProductName: 'Nuée', FileDescription: 'Nuée', CompanyName: '4rtefakt', InternalName: 'Nuee', OriginalFilename: 'Nuee.exe' },
    ignore: [/^\/node_modules/, /^\/tests/, /^\/construire\.js$/, /^\/copier-vendor\.js$/, /^\/package-lock\.json$/, /\.cmd$/, /^\/icone\/(?!nuee\.png$)/],
  });
  const ressources = path.join(dossier, 'resources');
  // Chromium's interface languages: the application is in French and English
  for (const f of fs.readdirSync(path.join(dossier, 'locales'))) {
    if (!['fr.pak', 'en-US.pak'].includes(f)) fs.rmSync(path.join(dossier, 'locales', f));
  }

  if (windows) {
    console.log('R portable…');
    const paquets = paquetsR();
    const paquetUtile = (rel) => { const m = /^library[\\/]([^\\/]+)/.exec(rel); return !m || paquets.has(m[1]); };
    copier(dossierR, path.join(ressources, 'R'), (rel) => !inutile(rel) && paquetUtile(rel));
  }

  console.log('moteur…');
  copier(path.join(depot, 'moteur'), path.join(ressources, 'moteur'));
  // the example project (made-up data) and its table
  copier(path.join(depot, 'exemple'), path.join(ressources, 'exemple'), (rel) => /^vergers\.(json|csv)$/.test(rel));
  // the full user guide, opened from Aide › Guide d'utilisation
  for (const f of ['LISEZ-MOI-APPLICATION.txt', 'USER-GUIDE.txt']) fs.copyFileSync(path.join(depot, f), path.join(ressources, f));

  if (!windows) {
    // the converter needs only Python's standard library: python3 runs it (moteur/serveur.R)
    copier(path.join(depot, 'convertisseur'), path.join(ressources, 'convertisseur'), (rel) => /^\w+\.py$/.test(rel));
    await construireLinux(dossier);
    return;
  }

  const exe = path.join(depot, 'outils', 'export_project.exe');
  if (!fs.existsSync(exe)) {
    console.log('convertisseur .usp (PyInstaller)…');
    const tmp = path.join(sortie, 'pyinstaller');
    execFileSync('python', ['-m', 'PyInstaller', '--onefile', '--noconfirm', '--clean', '--name', 'export_project',
      '--distpath', path.join(depot, 'outils'), '--workpath', tmp, '--specpath', tmp,
      '--exclude-module', 'openpyxl', '--exclude-module', 'matplotlib', '--exclude-module', 'numpy',
      '--exclude-module', 'tkinter', path.join(depot, 'convertisseur', 'export_project.py')], { stdio: 'inherit', cwd: depot });
    fs.rmSync(tmp, { recursive: true, force: true });
  }
  fs.mkdirSync(path.join(ressources, 'outils'), { recursive: true });
  fs.copyFileSync(exe, path.join(ressources, 'outils', 'export_project.exe'));
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

  // The installer for everyone else: for this user (no administrator rights needed) or for
  // all users, Start menu and desktop shortcuts, listed in Settings › Apps with its uninstaller.
  if (process.env.SANS_INSTALLATEUR) return;
  console.log('installateur…');
  const fichiers = await builder.build({
    targets: builder.Platform.WINDOWS.createTarget('nsis', builder.Arch.x64),
    prepackaged: path.join(final, 'application'),
    projectDir: __dirname,
    config: {
      appId: 'io.github.4rtefakt.nuee', productName: 'Nuée', executableName: 'Nuee', copyright: '4rtefakt', compression: 'maximum',
      directories: { output: path.join(sortie, 'installateur') },
      win: { icon: path.join(__dirname, 'icone', 'nuee.ico') },
      // .nuee projects open in Nuée when double-clicked, with its icon
      fileAssociations: [{ ext: 'nuee', name: 'Nuee.Projet', description: 'Projet Nuée', icon: path.join(__dirname, 'icone', 'nuee.ico') }],
      nsis: {
        oneClick: false, perMachine: false, allowToChangeInstallationDirectory: true,
        createDesktopShortcut: true, createStartMenuShortcut: true, shortcutName: 'Nuée', uninstallDisplayName: 'Nuée',
        installerIcon: path.join(__dirname, 'icone', 'nuee.ico'), uninstallerIcon: path.join(__dirname, 'icone', 'nuee.ico'),
        installerSidebar: path.join(__dirname, 'icone', 'installateur.bmp'), uninstallerSidebar: path.join(__dirname, 'icone', 'installateur.bmp'),
        installerLanguages: ['fr_FR', 'en_US'], language: '1036', artifactName: 'Nuee-Setup-${version}.exe',
      },
    },
  });
  console.log('Installateur :', fichiers.filter((f) => f.endsWith('.exe')).join(', '));
}

// Linux: the folder Nuee/ (./nuee starts it), then a .deb (Debian, Ubuntu…: R and its packages
// come with it), an AppImage and a .tar.gz (any distribution, R installed apart).
async function construireLinux(dossier) {
  const final = path.join(sortie, 'Nuee');
  fs.rmSync(final, { recursive: true, force: true });
  fs.renameSync(dossier, final);
  fs.copyFileSync(path.join(depot, 'LISEZ-MOI-APPLICATION.txt'), path.join(final, 'LISEZ-MOI.txt'));
  fs.copyFileSync(path.join(depot, 'USER-GUIDE.txt'), path.join(final, 'USER-GUIDE.txt'));
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
      fileAssociations: [{ ext: 'nuee', name: 'Nuee.Projet', description: 'Projet Nuée', mimeType: 'application/x-nuee' }],
      // electron-builder's own list (it is replaced, not added to), then R and the engine's packages
      deb: {
        packageCategory: 'science',
        depends: ['libgtk-3-0', 'libnotify4', 'libnss3', 'libxss1', 'libxtst6', 'xdg-utils', 'libatspi2.0-0', 'libuuid1', 'libsecret-1-0',
          'r-base-core', 'r-cran-httpuv', 'r-cran-jsonlite', 'r-cran-openxlsx', 'r-cran-haven', 'r-cran-readxl'],
        recommends: ['python3'],
      },
    },
  });
  console.log('Paquets :', fichiers.filter((f) => /\.(deb|AppImage|tar\.gz)$/.test(f)).join(', '));
}

construire().catch((e) => { console.error(e); process.exit(1); });
