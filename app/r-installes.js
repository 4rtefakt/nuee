// The Rs installed on this computer, which the engine can run on instead of Nuée's own (File ›
// Settings…, main.js): found where their installers put them, checked by running them once (their
// version and the packages the engine loads), and the packages missing installed into that R's
// user library. Where they are looked for:
// - Windows: the registry, HKLM and HKCU, SOFTWARE\R-core\R and \R64, a key per version with its
//   InstallPath; then %ProgramFiles%\R\R-* (an installation for every user) and
//   %LOCALAPPDATA%\Programs\R\R-* (for one user);
// - macOS: CRAN's R in /Library/Frameworks/R.framework (its bin/R starts the version the
//   framework's Current link names, whichever of its Versions is given: that one only), and
//   Homebrew's (/opt/homebrew, /usr/local);
// - Linux: Posit's builds in /opt/R/<version> (the computer's own R, on the PATH, is the default).
const { execFile, execFileSync, spawn } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

// The packages the engine loads beyond R's own (construire.js: paquetsR), then the recommended
// packages it uses (mixed and additive models, LDA and thresholds, trees, silhouettes, dBase files)
const PAQUETS_MOTEUR = ['httpuv', 'jsonlite', 'openxlsx', 'haven', 'readxl', 'RSQLite', 'ranger'];
const PAQUETS_RECOMMANDES = ['mgcv', 'nlme', 'MASS', 'rpart', 'cluster', 'foreign'];
const PAQUETS = [...PAQUETS_MOTEUR, ...PAQUETS_RECOMMANDES];
const CRAN = 'https://cloud.r-project.org';
// The oldest R checked (Ubuntu 24.04's R 4.3, on GitHub). Older ones run without .rds and .RData
// files (R 4.4: moteur/R/formats.R) and with MD5 fingerprints of data files (R 4.5: tools::sha256sum,
// moteur/R/etapes.R): the engine already allows for both.
const VERSION_MINIMALE = '4.3.0';
const DELAI_VERIFICATION = 60000;

const numeros = (v) => String(v || '').split(/[.-]/).map((x) => Number.parseInt(x, 10) || 0);
// < 0 when version a is older than b
function comparerVersions(a, b) {
  const x = numeros(a);
  const y = numeros(b);
  for (let i = 0; i < Math.max(x.length, y.length); i += 1) if ((x[i] || 0) !== (y[i] || 0)) return (x[i] || 0) - (y[i] || 0);
  return 0;
}

const fichierExiste = (f) => { try { return fs.statSync(f).isFile(); } catch { return false; } };
const lireDossier = (d) => { try { return fs.readdirSync(d); } catch { return []; } };
const cheminReel = (f) => { try { return fs.realpathSync(f); } catch { return null; } };
const surWindows = (plateforme) => plateforme === 'win32';
const chemins = (plateforme) => (surWindows(plateforme) ? path.win32 : path.posix);
// one R, whatever the case of its path on Windows
const memeChemin = (a, b, plateforme = process.platform) => (surWindows(plateforme)
  ? path.win32.normalize(a).toLowerCase() === path.win32.normalize(b).toLowerCase() : path.posix.normalize(a) === path.posix.normalize(b));

// The Rscript of an R given by its folder (R's own, its bin or bin\x64 folder) or by its Rscript;
// null when there is none. (Windows: bin\x64\Rscript.exe is R itself; bin\Rscript.exe only starts
// it, and stopping that one would leave R running: main.js, arreterR)
function rscriptDe(chemin, { plateforme = process.platform, existe = fichierExiste } = {}) {
  const c = String(chemin || '').trim().replace(/^"(.*)"$/, '$1');
  if (!c) return null;
  const p = chemins(plateforme);
  const nom = surWindows(plateforme) ? 'Rscript.exe' : 'Rscript';
  const egal = (a, b) => (surWindows(plateforme) ? a.toLowerCase() === b.toLowerCase() : a === b);
  if (egal(p.basename(c), nom)) {
    const x64 = p.join(p.dirname(c), 'x64', nom);
    if (surWindows(plateforme) && egal(p.basename(p.dirname(c)), 'bin') && existe(x64)) return x64;
    return existe(c) ? c : null;
  }
  const essais = surWindows(plateforme) ? [['bin', 'x64'], ['x64'], [], ['bin']]
    // (R's folder or a prefix, the folder of Rscript, an R.framework or one of its Versions,
    // /opt/R/<version>/lib/R's prefix)
    : [['bin'], [], ['Resources', 'bin'], ['lib', 'R', 'bin']];
  return essais.map((x) => p.join(c, ...x, nom)).find(existe) || null;
}

// The folder an Rscript belongs to, to show (R's own folder when it is laid out as usual)
function dossierDe(rscript, plateforme = process.platform) {
  const p = chemins(plateforme);
  const d = p.dirname(rscript);
  if (surWindows(plateforme) && /^x64$/i.test(p.basename(d)) && /^bin$/i.test(p.basename(p.dirname(d)))) return p.dirname(p.dirname(d));
  return /^bin$/i.test(p.basename(d)) ? p.dirname(d) : d;
}

// ------------------------------------------------------------ the registry (Windows)

// A key of the registry and those under it, as `reg export` writes them (UTF-16, whatever the
// system's code page: `reg query` writes paths in it, and an accent of a user's folder came out
// changed): { key: { name: value } }, a value a text (REG_SZ; REG_EXPAND_SZ with its %VARIABLES%
// replaced) or a number (REG_DWORD); other types are left out.
function lireExportRegistre(texte, env = process.env) {
  const cles = {};
  let cle = null;
  const texteDe = (s) => s.replace(/\\(.)/g, '$1');
  const variables = (s) => s.replace(/%([^%]+)%/g, (m, n) => env[n] ?? m);
  for (const l of String(texte || '').replace(/^﻿/, '').replace(/\\\r?\n\s*/g, '').split(/\r?\n/)) {
    const k = /^\[(.+)\]\s*$/.exec(l);
    if (k) { cle = k[1]; cles[cle] = {}; continue; }
    const v = /^"((?:[^"\\]|\\.)*)"=(.*)$/.exec(l);
    if (!cle || !v) continue;
    const brut = v[2].trim();
    let valeur;
    if (/^"(?:[^"\\]|\\.)*"$/.test(brut)) valeur = texteDe(brut.slice(1, -1));
    else if (/^hex\(2\):/i.test(brut)) {
      const octets = brut.slice(7).split(',').map((x) => x.trim()).filter(Boolean).map((x) => Number.parseInt(x, 16));
      valeur = variables(Buffer.from(octets).toString('utf16le').replace(/\0+$/, ''));
    } else if (/^dword:[0-9a-f]+$/i.test(brut)) valeur = Number.parseInt(brut.slice(6), 16);
    else continue;
    cles[cle][texteDe(v[1])] = valeur;
  }
  return cles;
}

// `reg export` of a key into a temporary file, read back; '' when the key isn't there
let nExports = 0;
function exporterRegistre(cle) {
  nExports += 1;
  const fichier = path.join(os.tmpdir(), `nuee-registre-${process.pid}-${nExports}.reg`);
  try {
    execFileSync('reg', ['export', cle, fichier, '/y'], { stdio: 'ignore', windowsHide: true, timeout: 15000 });
    return fs.readFileSync(fichier, 'utf16le');
  } catch {
    return '';
  } finally {
    fs.rmSync(fichier, { force: true });
  }
}

// The Rs the registry lists: [{ version, dossier }] (R-core\R and R64 hold the current one too,
// without its version)
function rsDuRegistre(cles) {
  const res = [];
  for (const [cle, valeurs] of Object.entries(cles)) {
    const m = /\\R-core\\R(?:64)?(?:\\([^\\]+))?$/i.exec(cle);
    if (m && typeof valeurs.InstallPath === 'string' && valeurs.InstallPath) res.push({ version: m[1] || null, dossier: valeurs.InstallPath });
  }
  return res;
}

// ------------------------------------------------------------ the policies (Windows)

// Nuée's policies (Group Policy, Intune…): the values of HKLM\SOFTWARE\Policies\Nuee (the computer's,
// which win) and HKCU\SOFTWARE\Policies\Nuee, in the registry's 64-bit view, read once per start by
// main.js: { valeurs: { name: value } (a REG_DWORD's number, a REG_SZ's text, a REG_EXPAND_SZ's with
// its %variables% replaced; an empty text is no value), lues (both keys read, there or not), par
// ('reg' or 'powershell': what read them) }. reg.exe first, the system's: `reg query` lists the keys of
// Policies (its answer tells a key that isn't there from one it can't read), `reg export` writes
// Nuée's in UTF-16 (a folder's accents kept). It doesn't run under "Prevent access to registry editing
// tools", nor where AppLocker or WDAC keeps it from users: then PowerShell's registry provider. lues
// false when neither could: main.js then turns R-code models off rather than ignore a policy it
// couldn't read. `cle`: another key than Nuée's (the checks: tests/essai_r_installe.js).
const NOMS_POLITIQUES = ['DisableUpdateCheck', 'DisableCustomCode', 'RHome', 'PluginsFolder'];
const RACINES = ['HKEY_LOCAL_MACHINE', 'HKEY_CURRENT_USER'];
const programmeSysteme = (env, ...p) => path.win32.join(env.SystemRoot || env.windir || 'C:\\Windows', 'System32', ...p);

// a program run to its end: its standard output, or null when it couldn't start or ended in error
function sortieDe(programme, args) {
  return new Promise((ok) => {
    execFile(programme, args, { encoding: 'utf8', windowsHide: true, timeout: 30000, maxBuffer: 16 * 1024 * 1024 }, (e, sortie) => ok(e ? null : String(sortie)));
  });
}

// One root's key read by reg.exe: its values ({} when it isn't there), null when reg.exe can't tell
async function politiquesParReg(racine, cle, { executer, env, temporaire }) {
  const reg = programmeSysteme(env, 'reg.exe');
  const chemin = `${racine}\\${cle}`;
  const liste = await executer(reg, ['query', path.win32.dirname(chemin), '/reg:64']);
  if (liste === null) return null;
  if (!liste.split(/\r?\n/).some((l) => l.trim().toLowerCase() === chemin.toLowerCase())) return {};
  const fichier = path.join(temporaire, `nuee-politiques-${process.pid}-${RACINES.indexOf(racine)}.reg`);
  try {
    if (await executer(reg, ['export', chemin, fichier, '/y', '/reg:64']) === null) return null;
    const lues = Object.entries(lireExportRegistre(fs.readFileSync(fichier, 'utf16le'), env)).find(([k]) => k.toLowerCase() === chemin.toLowerCase());
    return lues ? lues[1] : {};
  } catch {
    return null;
  } finally {
    fs.rmSync(fichier, { force: true });
  }
}

// Both keys read by PowerShell's registry provider: [HKLM's values, HKCU's] ({} when a key isn't
// there), null when PowerShell can't read them. What it found goes to a file, in UTF-8 (its own output
// follows the console's code page); cmdlets only, which the Constrained Language mode of WDAC and
// AppLocker allows. (A REG_EXPAND_SZ comes with its %variables% replaced.)
async function politiquesParPowerShell(cle, { executer, env, temporaire }) {
  const fichier = path.join(temporaire, `nuee-politiques-${process.pid}.json`);
  const cite = (s) => `'${s.replace(/'/g, "''")}'`;
  const code = ["$ErrorActionPreference = 'Stop'", '$r = @{}',
    "foreach ($h in 'HKLM', 'HKCU') {",
    `  $k = $h + ':\\' + ${cite(cle)}`,
    '  if (Test-Path -LiteralPath $k) { $r[$h] = Get-ItemProperty -LiteralPath $k | Select-Object -Property * -ExcludeProperty PSPath, PSParentPath, PSChildName, PSDrive, PSProvider } else { $r[$h] = @{} }',
    '}',
    `ConvertTo-Json -InputObject $r -Compress | Set-Content -LiteralPath ${cite(fichier)} -Encoding UTF8`].join('\n');
  try {
    if (await executer(programmeSysteme(env, 'WindowsPowerShell', 'v1.0', 'powershell.exe'), ['-NoProfile', '-NonInteractive', '-Command', code]) === null) return null;
    const lu = JSON.parse(fs.readFileSync(fichier, 'utf8').replace(/^\uFEFF/, ''));
    return [lu.HKLM, lu.HKCU].map((v) => (v && typeof v === 'object' && !Array.isArray(v) ? v : {}));
  } catch {
    return null;
  } finally {
    fs.rmSync(fichier, { force: true });
  }
}

async function lirePolitiques({ plateforme = process.platform, cle = 'SOFTWARE\\Policies\\Nuee', executer = sortieDe, env = process.env, temporaire = os.tmpdir() } = {}) {
  if (!surWindows(plateforme)) return { valeurs: {}, lues: true, par: null };
  const o = { executer, env, temporaire };
  let cles = [];
  for (const racine of RACINES) cles.push(await politiquesParReg(racine, cle, o));
  let par = 'reg';
  if (cles.includes(null)) {
    const ps = await politiquesParPowerShell(cle, o);
    if (ps) { cles = ps; par = 'powershell'; }
  }
  // (HKCU's, then HKLM's over them; a policy's name in any case)
  const valeurs = {};
  for (const c of [...cles].reverse()) {
    for (const [nom, v] of Object.entries(c || {})) {
      const n = NOMS_POLITIQUES.find((x) => x.toLowerCase() === nom.toLowerCase()) || nom;
      if (typeof v === 'number' && Number.isFinite(v)) valeurs[n] = v;
      else if (typeof v === 'string' && v.trim()) valeurs[n] = v.trim();
    }
  }
  const lues = !cles.includes(null);
  return { valeurs, lues, par: lues ? par : null };
}

// ------------------------------------------------------------ the Rs found

// [{ rscript, dossier, version, sources }], the most recent first; version: from the registry or
// the folder's name (null when neither says it); sources: where it was found ('registre', 'dossier')
function trouver({ plateforme = process.platform, env = process.env, exporter = exporterRegistre, existe = fichierExiste, lister = lireDossier,
  reel = cheminReel } = {}) {
  const p = chemins(plateforme);
  const trouves = [];
  const ajouter = (dossier, version, source, rscript = rscriptDe(dossier, { plateforme, existe })) => {
    if (!rscript) return;
    const deja = trouves.find((t) => memeChemin(t.rscript, rscript, plateforme));
    if (deja) {
      if (!deja.version && version) deja.version = version;
      if (!deja.sources.includes(source)) deja.sources.push(source);
      return;
    }
    trouves.push({ rscript, dossier: dossierDe(rscript, plateforme), version: version || null, sources: [source] });
  };
  if (surWindows(plateforme)) {
    for (const racine of ['HKEY_LOCAL_MACHINE', 'HKEY_CURRENT_USER']) {
      for (const r of rsDuRegistre(lireExportRegistre(exporter(`${racine}\\SOFTWARE\\R-core`), env))) ajouter(r.dossier, r.version, 'registre');
    }
    for (const base of [env.ProgramFiles && p.join(env.ProgramFiles, 'R'), env.LOCALAPPDATA && p.join(env.LOCALAPPDATA, 'Programs', 'R')].filter(Boolean)) {
      for (const n of lister(base)) if (/^R-\d/.test(n)) ajouter(p.join(base, n), n.slice(2), 'dossier');
    }
    // (the registry's current R, without its version, gets it from its folder's name)
    for (const t of trouves) if (!t.version) t.version = (/[\\/]R-(\d[\w.-]*)$/.exec(t.dossier) || [])[1] || null;
  } else if (plateforme === 'darwin') {
    const cadre = '/Library/Frameworks/R.framework/Resources';
    ajouter(cadre, (/\/Versions\/(\d+\.\d+)/.exec(reel(cadre) || '') || [])[1], 'dossier');
    for (const rscript of ['/opt/homebrew/bin/Rscript', '/usr/local/bin/Rscript']) {
      const vrai = existe(rscript) ? reel(rscript) : null;
      // (CRAN's installer links /usr/local/bin/Rscript to its framework, already there)
      if (!vrai || /\/R\.framework\//.test(vrai)) continue;
      ajouter(p.dirname(p.dirname(rscript)), (/\/Cellar\/r\/(\d[\d.]*)/.exec(vrai) || [])[1], 'dossier', rscript);
    }
  } else {
    for (const v of lister('/opt/R')) ajouter(p.join('/opt/R', v), /^\d/.test(v) ? v : null, 'dossier');
  }
  return trouves.sort((a, b) => comparerVersions(b.version, a.version));
}

// ------------------------------------------------------------ running an R

// A program run to its end: { code, sortie (its standard output), journal (its two outputs, in
// order), depasse (stopped after `delai` ms), erreur (it couldn't start) }. surTexte: each piece
// of its outputs as it comes.
function lancer(programme, args, { env = process.env, delai = 0, surTexte = null } = {}) {
  return new Promise((ok) => {
    let sortie = '';
    let journal = '';
    let depasse = false;
    let fini = false;
    let minuterie = null;
    let p;
    const finir = (x) => {
      if (fini) return;
      fini = true;
      clearTimeout(minuterie);
      ok({ code: null, erreur: null, ...x, sortie, journal, depasse });
    };
    try { p = spawn(programme, args, { env, windowsHide: true }); } catch (e) { finir({ erreur: e.message }); return; }
    // (Windows: Rscript.exe starts R, which taskkill stops with it)
    if (delai) minuterie = setTimeout(() => { depasse = true; arreter(p); }, delai);
    for (const [flux, deSortie] of [[p.stdout, true], [p.stderr, false]]) {
      flux.setEncoding('utf8');
      flux.on('data', (s) => {
        if (deSortie) sortie += s;
        journal += s;
        if (surTexte) surTexte(s);
      });
    }
    p.on('error', (e) => finir({ erreur: e.message }));
    p.on('close', (code) => finir({ code }));
  });
}
// (ended from a signal, a process has no exit code, only its signalCode: main.js, rEnMarche)
function arreter(p) {
  if (!p || p.exitCode !== null || p.signalCode !== null) return;
  if (process.platform === 'win32') {
    try { execFileSync('taskkill', ['/pid', String(p.pid), '/T', '/F'], { stdio: 'ignore', windowsHide: true }); return; } catch { /* then kill */ }
  }
  p.kill('SIGKILL');
}

const listeR = (paquets) => (paquets.length === 1 ? JSON.stringify(paquets[0]) : `c(${paquets.map((x) => JSON.stringify(x)).join(', ')})`);
// The line to run in that R (its console, R or RStudio) to install the packages: R asks there to
// use a personal library, and makes it.
const ligneInstallation = (paquets) => `install.packages(${listeR(paquets)}, repos = "${CRAN}")`;
// The packages of `paquets` that don't load, after the line NUEE_MANQUANTS; and the user library,
// after NUEE_BIBLIOTHEQUE= (R_LIBS_USER's first folder, where install.packages offers to put them)
const codeManquants = (paquets) => [
  `p <- ${listeR(paquets)}`,
  'ok <- vapply(p, function(x) suppressWarnings(suppressMessages(requireNamespace(x, quietly = TRUE))), NA)',
  'cat("NUEE_MANQUANTS", p[!ok], "\\n")',
];
const CODE_BIBLIOTHEQUE = 'lib <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep))[1L]';

function lireSortie(sortie) {
  const m = /NUEE_MANQUANTS([^\r\n]*)/.exec(sortie);
  const b = (/NUEE_BIBLIOTHEQUE=([^\r\n]*)/.exec(sortie) || [])[1];
  return { version: (/NUEE_R\s+(\d[\d.]*)/.exec(sortie) || [])[1] || null, manquants: m ? m[1].trim().split(/\s+/).filter(Boolean) : null,
    bibliotheque: b && b.trim() && b.trim() !== 'NA' ? b.trim() : null };
}

// What an R is worth to the engine, by running it once as the engine will (--no-init-file: its
// .Rprofile stays out; its .Renviron applies): { rscript, dossier, version, ok, ancien (older than
// 4.3: not usable), rds (older than 4.4), sha256 (older than 4.5), manquants (the packages that
// don't load), bibliotheque (the user library), ligne (the line installing the missing ones),
// erreur: 'introuvable' (no Rscript there), 'delai' (no answer within a minute), or the end of what
// R said }. `chemin`: its folder or its Rscript.
async function verifier(chemin, { plateforme = process.platform, env = process.env, delai = DELAI_VERIFICATION } = {}) {
  const rscript = rscriptDe(chemin, { plateforme });
  if (!rscript) return { rscript: null, dossier: chemin, ok: false, erreur: 'introuvable', manquants: [] };
  const code = ['cat("NUEE_R", as.character(getRversion()), "\\n")', CODE_BIBLIOTHEQUE, 'cat("NUEE_BIBLIOTHEQUE=", lib, "\\n", sep = "")',
    ...codeManquants(PAQUETS)].join('; ');
  const r = await lancer(rscript, ['--no-init-file', '-e', code], { env, delai });
  const lu = lireSortie(r.sortie);
  const base = { rscript, dossier: dossierDe(rscript, plateforme), version: lu.version, bibliotheque: lu.bibliotheque && chemins(plateforme).normalize(lu.bibliotheque) };
  if (!lu.version || !lu.manquants) {
    return { ...base, ok: false, manquants: [], erreur: r.depasse ? 'delai' : (r.erreur || r.journal.trim().split(/\r?\n/).slice(-12).join('\n') || `code ${r.code}`) };
  }
  const ancien = comparerVersions(lu.version, VERSION_MINIMALE) < 0;
  return { ...base, ok: !ancien && !lu.manquants.length, ancien, rds: comparerVersions(lu.version, '4.4.0') < 0, sha256: comparerVersions(lu.version, '4.5.0') < 0,
    manquants: lu.manquants, ligne: lu.manquants.length ? ligneInstallation(lu.manquants) : null, erreur: null };
}

// The packages installed into that R's user library (made if needed), from CRAN; surTexte: what R
// says meanwhile; autres: names allowed besides the engine's (a plugin's packages, main.js), each an R
// package's name. { ok, manquants (those that still don't load), bibliotheque, ligne (to run in that
// R by hand), fin (the end of what R said) }
async function installer(chemin, paquets, { plateforme = process.platform, env = process.env, surTexte = null, autres = [] } = {}) {
  const rscript = rscriptDe(chemin, { plateforme });
  const permis = [].concat(autres || []).filter((x) => typeof x === 'string' && /^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$/.test(x));
  const liste = [...new Set([].concat(paquets || []).filter((x) => PAQUETS.includes(x) || permis.includes(x)))];
  if (!rscript || !liste.length) return { ok: false, manquants: liste, ligne: liste.length ? ligneInstallation(liste) : null, fin: '' };
  const code = [CODE_BIBLIOTHEQUE, 'if (is.na(lib) || !nzchar(lib)) stop("R_LIBS_USER is empty")', 'dir.create(lib, recursive = TRUE, showWarnings = FALSE)',
    '.libPaths(c(lib, .libPaths()))', 'cat("NUEE_BIBLIOTHEQUE=", lib, "\\n", sep = "")', `install.packages(${listeR(liste)}, lib = lib, repos = "${CRAN}")`,
    ...codeManquants(liste)].join('; ');
  const r = await lancer(rscript, ['--no-init-file', '-e', code], { env, surTexte });
  const lu = lireSortie(r.sortie);
  const manquants = lu.manquants || liste;
  return { ok: !!lu.manquants && !manquants.length, manquants, bibliotheque: lu.bibliotheque && chemins(plateforme).normalize(lu.bibliotheque),
    ligne: manquants.length ? ligneInstallation(manquants) : null, fin: r.journal.trim().split(/\r?\n/).slice(-15).join('\n') };
}

module.exports = {
  PAQUETS, PAQUETS_MOTEUR, PAQUETS_RECOMMANDES, VERSION_MINIMALE, comparerVersions, rscriptDe, dossierDe, memeChemin,
  lireExportRegistre, exporterRegistre, rsDuRegistre, NOMS_POLITIQUES, lirePolitiques, trouver, lancer, arreter, ligneInstallation, verifier, installer,
};
