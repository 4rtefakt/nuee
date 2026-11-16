// Checks r-installes.js, the Rs of this computer the engine can run on instead of Nuée's own (File ›
// Settings…, main.js): finding them (a stand-in registry, folders and links, for Windows, macOS and
// Linux), the registry's export and the RHome policy read, then the check of an R and the
// installation of its missing packages with stand-in Rscripts: one too old, one missing packages
// (installed one after the other), one that says nothing R would, one that never answers.
// Given an R (its folder), the same for real: found, checked, its missing packages installed into a
// temporary user library and checked again. Given the program too (Nuée built, or Electron and the
// app's folder): that R chosen in File › Settings… with a project open and a PCA not saved yet, the
// engine restarted on it and the project back with its PCA, the engine's libraries, a model fitted
// through its API, Help › Diagnostic information; the next start on the setting kept; then an R
// that isn't there any more: Nuée's own R takes over, and says so once.
//   node tests/essai_r_installe.js [<R folder> [--neuf] [<program> [program options...]]]
// --neuf: that R was just installed: the check must find the engine's packages missing, R's
// recommended ones there. Needs Node 22 or later (the program: fetch, WebSocket).
const { execFileSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');
const R = require('../r-installes');

let echecs = 0;
function verifier(ok, nom, detail) {
  console.log(`${ok ? '  ok      ' : '  ÉCHEC   '}${nom}${!ok && detail !== undefined ? ` : ${typeof detail === 'string' ? detail : JSON.stringify(detail)}` : ''}`);
  if (!ok) echecs += 1;
  return ok;
}
const egal = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const windows = process.platform === 'win32';

// ------------------------------------------------------------ stand-in Rscripts

// What an Rscript of a stand-in R does: the version its folder's name says (R-4.4.1), and what
// faux.json in that folder says: { manquants (the packages that don't load), installables (those an
// installation brings), muet (it answers as R never would), lent (it never answers), arret (the
// engine stops before it is ready) }. An installation writes faux.json back.
const FAUX_RSCRIPT = String.raw`
const fs = require('fs');
const path = require('path');
let dossier = path.dirname(process.env.FAUX_R_EXE);
while (!/^R-\d/.test(path.basename(dossier)) && path.dirname(dossier) !== dossier) dossier = path.dirname(dossier);
const fiche = path.join(dossier, 'faux.json');
const f = JSON.parse(fs.readFileSync(fiche, 'utf8'));
const version = path.basename(dossier).slice(2);
const code = process.argv[process.argv.indexOf('-e') + 1] || '';
const bibliotheque = f.bibliotheque || path.join(dossier, 'bibliotheque-utilisateur');
const ecrire = (s) => process.stdout.write(s + '\n');
if (f.lent) Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 60000);
if (f.muet) { process.stderr.write('Fatal error: unable to open the base package\n'); process.exit(2); }
if (process.argv.some((a) => /serveur\.R$/.test(a))) {
  process.stderr.write("Error in loadNamespace(x) : there is no package called 'httpuv'\nExecution halted\n");
  process.exit(1);
}
const install = /install\.packages\((c\([^)]*\)|"[^"]*")/.exec(code);
if (install) {
  const demandes = [...install[1].matchAll(/"([^"]+)"/g)].map((m) => m[1]);
  ecrire('NUEE_BIBLIOTHEQUE=' + bibliotheque);
  for (const p of demandes) {
    ecrire("trying URL 'https://cloud.r-project.org/bin/windows/contrib/" + version.split('.').slice(0, 2).join('.') + '/' + p + "_1.0.zip'");
    Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 30);
    if ((f.installables || []).includes(p)) {
      f.manquants = (f.manquants || []).filter((x) => x !== p);
      ecrire('package ‘' + p + '’ successfully unpacked and MD5 sums checked');
    } else process.stderr.write('Warning: package ‘' + p + '’ is not available for this version of R\n');
  }
  fs.writeFileSync(fiche, JSON.stringify(f));
  ecrire('NUEE_MANQUANTS ' + demandes.filter((p) => (f.manquants || []).includes(p)).join(' ') + ' ');
} else {
  ecrire('NUEE_R ' + version + ' ');
  ecrire('NUEE_BIBLIOTHEQUE=' + bibliotheque);
  ecrire('NUEE_MANQUANTS ' + (f.manquants || []).join(' ') + ' ');
}
`;
// On Windows a program is started as Rscript.exe: a small one, compiled by the .NET Framework's
// compiler, runs the stand-in with Node, its outputs passed on line by line as they come
const FAUX_RSCRIPT_CS = String.raw`using System;
using System.Diagnostics;
using System.IO;
using System.Text;
class FauxRscript {
  static string Citer(string a) {
    var s = new StringBuilder("\"");
    int barres = 0;
    foreach (char c in a) {
      if (c == '\\') { barres++; continue; }
      if (c == '"') { s.Append('\\', barres * 2 + 1); s.Append('"'); barres = 0; continue; }
      s.Append('\\', barres); barres = 0; s.Append(c);
    }
    s.Append('\\', barres * 2);
    return s.Append('"').ToString();
  }
  static readonly object verrou = new object();
  static DataReceivedEventHandler Vers(Stream flux) {
    return (s, e) => {
      if (e.Data == null) return;
      var b = Encoding.UTF8.GetBytes(e.Data + "\n");
      lock (verrou) { flux.Write(b, 0, b.Length); flux.Flush(); }
    };
  }
  static int Main(string[] args) {
    var p = new ProcessStartInfo(@"__NODE__");
    var ligne = new StringBuilder(Citer(@"__SCRIPT__"));
    foreach (var a in args) ligne.Append(" ").Append(Citer(a));
    p.Arguments = ligne.ToString();
    p.UseShellExecute = false;
    p.CreateNoWindow = true;
    p.RedirectStandardOutput = true;
    p.RedirectStandardError = true;
    p.StandardOutputEncoding = Encoding.UTF8;
    p.StandardErrorEncoding = Encoding.UTF8;
    p.EnvironmentVariables["ELECTRON_RUN_AS_NODE"] = "1";
    p.EnvironmentVariables["FAUX_R_EXE"] = System.Reflection.Assembly.GetEntryAssembly().Location;
    using (var q = new Process()) {
      q.StartInfo = p;
      q.OutputDataReceived += Vers(Console.OpenStandardOutput());
      q.ErrorDataReceived += Vers(Console.OpenStandardError());
      q.Start();
      q.BeginOutputReadLine();
      q.BeginErrorReadLine();
      q.WaitForExit();
      return q.ExitCode;
    }
  }
}
`;
let rscriptCompile = null;
// A stand-in R in `dossier` (named R-<version>), laid out as the platform's installers lay it out;
// returns its folder, or null when no stand-in can be made here (Windows without csc.exe)
function fauxR(dossier, faux = {}) {
  const rscript = windows ? path.join(dossier, 'bin', 'x64', 'Rscript.exe') : path.join(dossier, 'bin', 'Rscript');
  fs.mkdirSync(path.dirname(rscript), { recursive: true });
  fs.writeFileSync(path.join(dossier, 'faux.json'), JSON.stringify(faux));
  const script = path.join(path.dirname(dossier), 'faux_rscript.js');
  if (!fs.existsSync(script)) fs.writeFileSync(script, FAUX_RSCRIPT);
  if (!windows) {
    fs.writeFileSync(rscript, `#!/bin/sh\nFAUX_R_EXE="$0" ELECTRON_RUN_AS_NODE=1 exec '${process.execPath}' '${script}' "$@"\n`, { mode: 0o755 });
    return dossier;
  }
  if (!rscriptCompile) {
    const windir = process.env.WINDIR || 'C:\\Windows';
    const csc = ['Framework64', 'Framework'].map((f) => path.join(windir, 'Microsoft.NET', f, 'v4.0.30319', 'csc.exe')).find((f) => fs.existsSync(f));
    if (!csc) return null;
    const source = path.join(path.dirname(dossier), 'faux_rscript.cs');
    fs.writeFileSync(source, FAUX_RSCRIPT_CS.replace('__NODE__', () => process.execPath.replace(/"/g, '""')).replace('__SCRIPT__', () => script.replace(/"/g, '""')));
    rscriptCompile = path.join(path.dirname(dossier), 'Rscript.exe');
    execFileSync(csc, ['/nologo', '/target:exe', `/out:${rscriptCompile}`, source], { stdio: 'pipe', windowsHide: true });
  }
  fs.copyFileSync(rscriptCompile, rscript);
  return dossier;
}

// ------------------------------------------------------------ with fakes only

function sansProgramme() {
  console.log('Versions, chemins, registre');
  verifier(R.comparerVersions('4.2.3', '4.3.0') < 0 && R.comparerVersions('4.10.0', '4.9.9') > 0 && R.comparerVersions('4.5', '4.5.0') === 0,
    'les versions se comparent nombre par nombre');

  // Windows: R's folder, its bin or bin\x64 folder, its Rscript (bin\x64's, which is R itself)
  const fichiers = new Set(['C:\\Program Files\\R\\R-4.5.2\\bin\\x64\\Rscript.exe', 'C:\\Program Files\\R\\R-4.5.2\\bin\\Rscript.exe']);
  const existeW = (f) => fichiers.has(path.win32.normalize(f));
  const de = (c) => R.rscriptDe(c, { plateforme: 'win32', existe: existeW });
  const x64 = 'C:\\Program Files\\R\\R-4.5.2\\bin\\x64\\Rscript.exe';
  verifier(['C:\\Program Files\\R\\R-4.5.2', 'C:\\Program Files\\R\\R-4.5.2\\', '"C:\\Program Files\\R\\R-4.5.2"', 'C:\\Program Files\\R\\R-4.5.2\\bin',
    'C:\\Program Files\\R\\R-4.5.2\\bin\\x64', 'C:\\Program Files\\R\\R-4.5.2\\bin\\Rscript.exe', 'C:\\Program Files\\R\\R-4.5.2\\bin\\x64\\Rscript.exe'].every((c) => de(c) === x64),
  'Windows : le dossier de R, bin, bin\\x64 ou Rscript.exe donnent bin\\x64\\Rscript.exe');
  verifier(de('C:\\Program Files\\R') === null && de('C:\\Program Files\\R\\R-4.5.2\\bin\\x64\\R.exe') === null && de('') === null,
    'Windows : un dossier sans R, un autre programme : rien');
  verifier(R.dossierDe(x64, 'win32') === 'C:\\Program Files\\R\\R-4.5.2' && R.dossierDe('C:\\R\\bin\\Rscript.exe', 'win32') === 'C:\\R',
    'Windows : le dossier montré est celui de R');
  const unix = (liste) => (f) => liste.includes(f);
  verifier(R.rscriptDe('/Library/Frameworks/R.framework', { plateforme: 'darwin', existe: unix(['/Library/Frameworks/R.framework/Resources/bin/Rscript']) })
    === '/Library/Frameworks/R.framework/Resources/bin/Rscript'
    && R.rscriptDe('/opt/R/4.5.1', { plateforme: 'linux', existe: unix(['/opt/R/4.5.1/bin/Rscript']) }) === '/opt/R/4.5.1/bin/Rscript'
    && R.rscriptDe('/usr/lib/R/bin', { plateforme: 'linux', existe: unix(['/usr/lib/R/bin/Rscript']) }) === '/usr/lib/R/bin/Rscript',
  'macOS et Linux : R.framework, /opt/R/<version>, un dossier bin');

  // reg export: UTF-16 whatever the code page (an accent in a user's folder), strings with their
  // escapes, REG_EXPAND_SZ in hexadecimal over two lines, a DWORD
  const utf16 = (s) => [...Buffer.from(`${s}\0`, 'utf16le')].map((b) => b.toString(16).padStart(2, '0')).join(',');
  const etendu = utf16('%ProgramFiles%\\R\\R-4.5.2');
  const export1 = ['\uFEFFWindows Registry Editor Version 5.00', '',
    '[HKEY_LOCAL_MACHINE\\SOFTWARE\\R-core]', '',
    '[HKEY_LOCAL_MACHINE\\SOFTWARE\\R-core\\R]', '"Current Version"="4.5.2"', '"InstallPath"="C:\\\\Program Files\\\\R\\\\R-4.5.2"', '',
    '[HKEY_LOCAL_MACHINE\\SOFTWARE\\R-core\\R\\4.1.0]', '"InstallPath"="C:\\\\Program Files\\\\R\\\\R-4.1.0"', '',
    '[HKEY_LOCAL_MACHINE\\SOFTWARE\\R-core\\R\\4.5.2]', '"InstallPath"="C:\\\\Program Files\\\\R\\\\R-4.5.2"', '',
    '[HKEY_LOCAL_MACHINE\\SOFTWARE\\R-core\\R64\\4.5.2]', '"InstallPath"="C:\\\\Program Files\\\\R\\\\R-4.5.2"', '"Nom \\"cité\\""="a\\\\b"', ''].join('\r\n');
  const cles = R.lireExportRegistre(export1);
  verifier(cles['HKEY_LOCAL_MACHINE\\SOFTWARE\\R-core\\R'].InstallPath === 'C:\\Program Files\\R\\R-4.5.2'
    && cles['HKEY_LOCAL_MACHINE\\SOFTWARE\\R-core\\R64\\4.5.2']['Nom "cité"'] === 'a\\b', 'reg export : les textes et leurs échappements');
  verifier(egal(R.rsDuRegistre(cles), [{ version: null, dossier: 'C:\\Program Files\\R\\R-4.5.2' }, { version: '4.1.0', dossier: 'C:\\Program Files\\R\\R-4.1.0' },
    { version: '4.5.2', dossier: 'C:\\Program Files\\R\\R-4.5.2' }, { version: '4.5.2', dossier: 'C:\\Program Files\\R\\R-4.5.2' }]),
  'reg export : les R de R-core\\R et R64, une clé par version (le R courant sans la sienne)', R.rsDuRegistre(cles));
  const politique = (hklm, hkcu) => (cle) => (cle.startsWith('HKEY_LOCAL_MACHINE') ? hklm : hkcu);
  const exportPolitique = (lignes) => ['\uFEFFWindows Registry Editor Version 5.00', '', '[HKEY_LOCAL_MACHINE\\SOFTWARE\\Policies\\Nuee]', ...lignes, ''].join('\r\n');
  const exportPolitiqueU = (lignes) => exportPolitique(lignes).replace('HKEY_LOCAL_MACHINE', 'HKEY_CURRENT_USER');
  const env = { ProgramFiles: 'C:\\Program Files' };
  verifier(R.politiqueTexte('RHome', { exporter: politique(exportPolitique(['"DisableUpdateCheck"=dword:00000001', '"RHome"="D:\\\\Logiciels\\\\R\\\\R-4.5.2"']),
    exportPolitiqueU(['"RHome"="C:\\\\Autre"'])), env }) === 'D:\\Logiciels\\R\\R-4.5.2', 'RHome : celui de l’ordinateur l’emporte');
  const coupure = etendu.indexOf(',', 50) + 1;  // (reg export cuts its lines after a comma)
  verifier(R.politiqueTexte('RHome', { exporter: politique('', exportPolitiqueU([`"RHome"=hex(2):${etendu.slice(0, coupure)}\\`, `  ${etendu.slice(coupure)}`])), env })
    === 'C:\\Program Files\\R\\R-4.5.2', 'RHome : celui de l’utilisateur, en REG_EXPAND_SZ sur deux lignes, %ProgramFiles% remplacé');
  verifier(R.politiqueTexte('RHome', { exporter: politique(exportPolitique(['"RHome"=""', '"DisableUpdateCheck"=dword:00000001']), ''), env }) === null
    && R.politiqueTexte('RHome', { exporter: () => '', env }) === null, 'RHome : vide ou absent, pas de politique');
  if (windows) {
    // (a key every Windows has, its names in the system's language: accents on a French one)
    const cle = 'HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\UTC';
    const utc = R.lireExportRegistre(R.exporterRegistre(cle))[cle] || {};
    verifier(typeof utc.Std === 'string' && utc.Std.length > 3 && !utc.Std.includes('�'), `reg.exe : une clé exportée et relue (${utc.Std})`, utc);
    verifier(R.exporterRegistre('HKEY_CURRENT_USER\\SOFTWARE\\Sûrement\\absente de ce registre') === '', 'reg.exe : une clé absente, rien');
  }

  console.log('Les R trouvés');
  // Windows: the registry's (one removed since, its folder gone; one of a user named Clément), the
  // folders' (Program Files' written in another case; R-devel isn't a version)
  const clement = 'C:\\Users\\Clément\\AppData\\Local';
  const presents = new Set(['C:\\Program Files\\R\\R-4.5.2', 'C:\\Program Files\\R\\R-4.2.3', `${clement}\\Programs\\R\\R-4.4.1`, `${clement}\\Programs\\R\\R-devel`]
    .flatMap((d) => [`${d}\\bin\\x64\\Rscript.exe`, `${d}\\bin\\Rscript.exe`]).map((f) => f.toLowerCase()));
  const hkcu = ['\uFEFFWindows Registry Editor Version 5.00', '', '[HKEY_CURRENT_USER\\SOFTWARE\\R-core\\R\\4.4.1]',
    `"InstallPath"="${clement.replace(/\\/g, '\\\\')}\\\\Programs\\\\R\\\\R-4.4.1"`, ''].join('\r\n');
  const trouves = R.trouver({ plateforme: 'win32', env: { ProgramFiles: 'C:\\PROGRAM FILES', LOCALAPPDATA: clement },
    exporter: (cle) => (cle.startsWith('HKEY_LOCAL_MACHINE') ? export1 : hkcu),
    existe: (f) => presents.has(path.win32.normalize(f).toLowerCase()),
    lister: (d) => ({ 'c:\\program files\\r': ['R-4.5.2', 'R-4.2.3', 'Rtools'], [`${clement}\\programs\\r`.toLowerCase()]: ['R-4.4.1', 'R-devel'] })[d.toLowerCase()] || [] });
  verifier(egal(trouves.map((x) => [x.version, x.dossier, x.sources]), [['4.5.2', 'C:\\Program Files\\R\\R-4.5.2', ['registre', 'dossier']],
    ['4.4.1', `${clement}\\Programs\\R\\R-4.4.1`, ['registre', 'dossier']], ['4.2.3', 'C:\\PROGRAM FILES\\R\\R-4.2.3', ['dossier']]]),
  'Windows : registre et dossiers, une fois chacun, le plus récent d’abord (un R désinstallé et R-devel laissés)', trouves);
  // macOS: CRAN's (the version its Current link names), Homebrew's; CRAN's link in /usr/local/bin once
  const mac = R.trouver({ plateforme: 'darwin', existe: unix(['/Library/Frameworks/R.framework/Resources/bin/Rscript', '/opt/homebrew/bin/Rscript', '/usr/local/bin/Rscript']),
    reel: (f) => ({ '/Library/Frameworks/R.framework/Resources': '/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources',
      '/opt/homebrew/bin/Rscript': '/opt/homebrew/Cellar/r/4.5.1/bin/Rscript',
      '/usr/local/bin/Rscript': '/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/bin/Rscript' })[f] || null });
  verifier(egal(mac.map((x) => [x.version, x.rscript]), [['4.5.1', '/opt/homebrew/bin/Rscript'], ['4.5', '/Library/Frameworks/R.framework/Resources/bin/Rscript']]),
    'macOS : le R du CRAN (sa version Current) et celui de Homebrew', mac);
  const linux = R.trouver({ plateforme: 'linux', existe: unix(['/opt/R/4.5.1/bin/Rscript', '/opt/R/4.4.3/bin/Rscript']), lister: (d) => (d === '/opt/R' ? ['4.4.3', '4.5.1', 'LISEZMOI'] : []) });
  verifier(egal(linux.map((x) => [x.version, x.dossier]), [['4.5.1', '/opt/R/4.5.1'], ['4.4.3', '/opt/R/4.4.3']]), 'Linux : les R de /opt/R', linux);
  verifier(R.ligneInstallation(['ranger']) === 'install.packages("ranger", repos = "https://cloud.r-project.org")'
    && R.ligneInstallation(['httpuv', 'RSQLite']) === 'install.packages(c("httpuv", "RSQLite"), repos = "https://cloud.r-project.org")',
  'la ligne à lancer dans ce R');
}

async function avecFauxR(racine) {
  console.log('Vérifier et installer, avec des Rscript de remplacement');
  const faux = (version, f) => fauxR(path.join(racine, `R-${version}`), f);
  const vieux = faux('4.2.3', {});
  if (!vieux) { console.log('  (pas de Rscript de remplacement possible ici : csc.exe absent)'); return; }
  const v1 = await R.verifier(vieux);
  verifier(!v1.ok && v1.ancien && v1.version === '4.2.3' && !v1.erreur, 'un R 4.2 : trop ancien', v1);
  const r43 = faux('4.3.0', {});
  const v2 = await R.verifier(r43);
  verifier(v2.ok && v2.rds && v2.sha256 && !v2.manquants.length, 'un R 4.3 complet : utilisable, sans .rds et avec des empreintes MD5', v2);
  const r44 = faux('4.4.1', { manquants: ['ranger', 'RSQLite'], installables: ['ranger'] });
  const v3 = await R.verifier(path.join(r44, 'bin'));
  verifier(!v3.ok && egal(v3.manquants, ['ranger', 'RSQLite']) && !v3.rds && v3.sha256 && v3.dossier === r44
    && v3.ligne === 'install.packages(c("ranger", "RSQLite"), repos = "https://cloud.r-project.org")'
    && v3.bibliotheque === path.join(r44, 'bibliotheque-utilisateur'), 'un R 4.4 sans ranger ni RSQLite : les paquets manquants, la ligne qui les installe', v3);
  const morceaux = [];
  const i1 = await R.installer(r44, v3.manquants, { surTexte: (s) => morceaux.push(s) });
  verifier(!i1.ok && egal(i1.manquants, ['RSQLite']) && i1.ligne === 'install.packages("RSQLite", repos = "https://cloud.r-project.org")'
    && /not available/.test(i1.fin), 'l’installation : ranger installé, RSQLite pas (et la ligne pour lui seul)', i1);
  verifier(morceaux.length >= 2 && /ranger_1\.0\.zip/.test(morceaux.join('')), 'l’installation : ce que dit R, au fur et à mesure', morceaux.length);
  verifier(egal((await R.verifier(r44)).manquants, ['RSQLite']), 'vérifié de nouveau : RSQLite manque encore');
  fs.writeFileSync(path.join(r44, 'faux.json'), JSON.stringify({ manquants: ['RSQLite'], installables: ['RSQLite'] }));
  const i2 = await R.installer(r44, ['RSQLite', 'pas-un-paquet-du-moteur']);
  const v4 = await R.verifier(r44);
  verifier(i2.ok && !i2.ligne && v4.ok, 'installé ensuite : ce R est utilisable (un paquet que le moteur ne demande pas n’est pas installé)', [i2, v4]);
  const muet = await R.verifier(faux('4.5.0', { muet: true }));
  verifier(!muet.ok && /unable to open the base package/.test(muet.erreur), 'un R qui ne répond pas comme R : ce qu’il a dit', muet);
  const debut = Date.now();
  const lent = await R.verifier(faux('4.6.0', { lent: true }), { delai: 1500 });
  verifier(!lent.ok && lent.erreur === 'delai' && Date.now() - debut < 15000, 'un R qui ne répond pas : arrêté après le délai', [lent, Date.now() - debut]);
  verifier((await R.verifier(path.join(racine, 'nulle-part'))).erreur === 'introuvable', 'un dossier sans R : introuvable');
}

// ------------------------------------------------------------ for real

async function avecVraiR(dossierR, neuf, racine) {
  console.log(`Le R de ${dossierR}`);
  const rscript = R.rscriptDe(dossierR);
  if (!verifier(!!rscript, 'son Rscript', dossierR)) return null;
  const trouve = R.trouver().find((x) => R.memeChemin(x.rscript, rscript));
  verifier(!!trouve, 'trouvé parmi les R de l’ordinateur', R.trouver());
  if (trouve) console.log(`    (${trouve.sources.join(' et ')}, R ${trouve.version})`);
  // (its packages go into a temporary user library, for this run only)
  process.env.R_LIBS_USER = path.join(racine, 'bibliotheque');
  const v = await R.verifier(dossierR);
  if (!verifier(!v.erreur && !v.ancien && !!v.version, `vérifié : R ${v.version}`, v)) return null;
  verifier(v.bibliotheque && R.memeChemin(v.bibliotheque, process.env.R_LIBS_USER), 'sa bibliothèque d’utilisateur, celle de R_LIBS_USER', v.bibliotheque);
  if (neuf) {
    verifier(egal([...v.manquants].sort(), [...R.PAQUETS_MOTEUR].sort()), 'un R qui vient d’être installé : il lui manque les paquets du moteur, pas les recommandés', v.manquants);
  }
  if (v.manquants.length) {
    const debut = Date.now();
    let morceaux = 0;
    const i = await R.installer(dossierR, v.manquants, { surTexte: () => { morceaux += 1; } });
    verifier(i.ok && fs.existsSync(process.env.R_LIBS_USER), `installés dans une bibliothèque faite pour eux (${Math.round((Date.now() - debut) / 1000)} s)`, i);
    verifier(morceaux > 2, 'ce que dit R pendant l’installation, au fur et à mesure', morceaux);
    const apres = await R.verifier(dossierR);
    verifier(apres.ok, 'vérifié de nouveau : ce R est utilisable', apres);
    return apres.ok ? apres : null;
  }
  console.log('    (aucun paquet ne manque : rien à installer)');
  return v.ok ? v : null;
}

// The program started on a profile whose settings (reglages.json) are `reglages`
async function lancer(programme, options, racine, nom, reglages, premierLancement = true) {
  const { demarrer } = require('./parcours/pilote');
  const profil = path.join(racine, nom);
  fs.mkdirSync(profil, { recursive: true });
  if (reglages) fs.writeFileSync(path.join(profil, 'reglages.json'), JSON.stringify(reglages));
  return demarrer({ programme, options, dossier: racine, profil, premierLancement, journal: (t) => console.log(`    ${t}`) });
}

// The engine on that R, as its API and the console say (its version, its libraries)
async function moteurSurCeR(n, v) {
  const version = await n.api('version');
  verifier(version.R.includes(`R version ${v.version} `), `le moteur tourne sur ce R : ${version.R}`, version.R);
  const etat = await n.evaluer('window.bureau.r.etat()');
  verifier(etat.utilise.type === 'autre' && R.memeChemin(etat.utilise.rscript, v.rscript) && !etat.utilise.repli, 'Réglages : cet autre R, utilisé', etat.utilise);
  const libs = [].concat((await n.api('console', { code: 'cat(.libPaths(), sep = "\\n")' })).journal || []).filter((e) => e.type === 'sortie')
    .map((e) => [].concat(e.texte).join('\n')).join('\n');
  // (R writes them in full, with /: the temporary folder may come with a short name, RUNNER~1)
  const forme = (c) => c.toLowerCase().replace(/\\/g, '/');
  const utilisateur = fs.existsSync(process.env.R_LIBS_USER) ? forme(fs.realpathSync.native(process.env.R_LIBS_USER)) : null;
  verifier(!!libs && !/resources\/R\/library/i.test(forme(libs)) && (!utilisateur || forme(libs).includes(utilisateur)),
    'ses bibliothèques : celle de l’utilisateur, pas celle de Nuée', libs);
}

async function avecProgramme(programme, options, v, racine) {
  console.log(`Nuée : Fichier › Réglages…, ce R, un projet ouvert (${programme})`);
  const csv = path.join(__dirname, '..', '..', 'exemple', 'orchards.csv');
  let n = await lancer(programme, options, racine, 'profil-autre-r', null);
  try {
    verifier((await n.evaluer('window.bureau.r.etat()')).utilise.type === 'nuee', 'au départ, le R de Nuée');
    // a project with a change not saved: a PCA fitted
    await n.fichierAOuvrir(csv);
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('orchards.csv'));
    await n.nouveauModele({ type: 'PCA' });
    await n.menu('m_fichier', 'reglages');
    await n.dialogue('reglages_titre');
    await n.cliquer(`[...__n.boite().querySelectorAll('.entree-r input')].find((i) => i.value.toLowerCase() === ${JSON.stringify(v.rscript.toLowerCase())})`);
    await n.attendre("__n.boite().querySelector('.resultat-r .etat-r.ok')", 120000);
    await n.bouton('r_utiliser');
    await n.dialogue('r_redemarrer_titre');
    await n.bouton('redemarrer');
    await n.attendre("window.bureau.r.etat().then((e) => e.utilise && e.utilise.type === 'autre')", 120000);
    const rouvert = await n.attendre(`__n.api('etat').then((e) => { const p = [].concat(e.projets).find((x) => x.nom === 'orchards.csv');
      return !!p && [].concat(e.modeles).some((m) => m.projet === p.id && m.type === 'PCA'); })`, 120000).catch(() => false);
    verifier(rouvert, 'le moteur redémarré sur ce R : le projet revient, avec son ACP pas encore enregistrée');
    await moteurSurCeR(n, v);
    const p = await n.api('ouvrir', { chemin: csv });
    const x = [].concat(p.variables).filter((x) => !x.qualitative).map((x) => x.uid);
    const acp = await n.api('ajuster', { projet: p.id, type: 'PCA', x, A: 2, nom: 'ACP' });
    verifier(acp.A === 2 && [].concat(acp.observations.t1).length === [].concat(p.observations.uid).length, `une ACP ajustée par son API (${x.length} variables)`, acp.A);
    await n.menu('m_aide', 'diagnostic');
    await n.dialogue('diagnostic');
    const diag = await n.evaluer("__n.norm(__n.boite().querySelector('pre.diagnostic').textContent)");
    verifier(diag.includes('R used: another R of this computer (File › Settings)') && diag.includes(`R version ${v.version}`), 'Aide › Informations de diagnostic dit quel R', diag);
    verifier(await n.quitter(), 'Nuée quitte');
  } finally { n.fermer(); }
  console.log('Le démarrage suivant, sur le réglage gardé');
  n = await lancer(programme, options, racine, 'profil-autre-r', null, false);
  try {
    await moteurSurCeR(n, v);
    verifier(await n.quitter(), 'Nuée quitte');
  } finally { n.fermer(); }

  console.log('Un R choisi qui n’est plus là');
  const absent = path.join(racine, 'R-absent', 'bin', windows ? 'x64\\Rscript.exe' : 'Rscript');
  n = await lancer(programme, options, racine, 'profil-absent', { rscript: absent }, false);
  try {
    await n.dialogue('r_repli_titre');
    const texte = await n.evaluer('__n.norm(__n.boite().textContent)');
    verifier(texte.includes(path.join(racine, 'R-absent')), 'le message dit lequel', texte);
    const etat = await n.evaluer('window.bureau.r.etat()');
    verifier(etat.utilise.type === 'nuee' && etat.utilise.repli && etat.utilise.repli.motif === 'absent', 'le R de Nuée a pris le relais', etat.utilise);
    verifier(!!(await n.api('version')).R, 'et le moteur répond');
    verifier(await n.quitter(), 'Nuée quitte');
  } finally { n.fermer(); }
  n = await lancer(programme, options, racine, 'profil-absent', null, false);
  try {
    await n.libre();
    const encore = await n.evaluer('window.bureau.r.repli()');
    const titre = await n.evaluer('__n.dialogue() ? __n.titreDialogue() : ""');
    verifier(encore === null && !titre, 'au démarrage suivant, rien n’est redit', [encore, titre]);
    verifier(await n.quitter(), 'Nuée quitte');
  } finally { n.fermer(); }
}

async function principal() {
  const args = process.argv.slice(2);
  const neuf = args.includes('--neuf');
  const [dossierR, programme, ...options] = args.filter((a) => a !== '--neuf');
  const racine = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-r-installe-'));
  try {
    sansProgramme();
    await avecFauxR(path.join(racine, 'faux'));
    if (dossierR) {
      const v = await avecVraiR(path.resolve(dossierR), neuf, racine);
      if (v && programme) await avecProgramme(/[\\/]/.test(programme) ? path.resolve(programme) : programme, options, v, racine);
      else if (programme) verifier(false, 'Nuée sur ce R', 'ce R n’est pas utilisable');
    }
  } catch (e) {
    verifier(false, 'erreur', e.stack || e.message);
  } finally {
    try { fs.rmSync(racine, { recursive: true, force: true, maxRetries: 5 }); } catch (e) { console.log(`(${racine} reste : ${e.message})`); }
  }
  console.log(echecs ? `${echecs} vérification(s) en échec` : 'Tout est bon');
  process.exit(echecs ? 1 : 0);
}

if (require.main === module) principal();
module.exports = { fauxR };
