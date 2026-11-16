// Rscript with webR's R (R compiled to WebAssembly, the R of the browser version:
// interface/moteur_webr.js), run by Node: an R file run as Rscript runs it, so that the engine's
// checks (moteur/tests/verifier_*.R) run in that R. essai_moteur_webr.mjs runs them all.
//   node tests/webr_rscript.mjs [--vanilla] <file.R> [arguments...]
//   node tests/webr_rscript.mjs -e <expression> [-e <expression>...]
// - The file is read by R's console, line by line, as Rscript reads it: each expression at the
//   top level, in the global environment (no frame around it: a script sourced from it finds
//   its own file in sys.frame(1)$ofile), its value printed when visible, an error ending it
//   ("Execution halted"). webR's console reads more than one line only when R is interactive:
//   interactive() says FALSE, as under Rscript, and options(error) halts as Rscript does.
//   (R's console takes lines of 4,096 bytes at most.)
// - R's working folder is the folder Node starts in, which R sees at /depot; its packages, the
//   library essai_moteur_webr.mjs installs (node_modules/.cache/nuee-webr), at /paquets, first in
//   .libPaths().
// - R's session folder (tempdir()) is a folder of this computer's temporary folder, so that
//   another R started by system2(file.path(R.home("bin"), "Rscript"), ...) sees the files this
//   one wrote. webR can't start a program: that R is this script again, in a Node process of its
//   own, started from webR's worker (spawnSync), with the same folders at the same places
//   (NUEE_WEBR_MONTAGES) and the same working folder (NUEE_WEBR_DOSSIER).
// - quit(status = n) ends it with that status; an error, with 1.
import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const ICI = path.dirname(fileURLToPath(import.meta.url));
const SOI = fileURLToPath(import.meta.url);
const BIBLIOTHEQUE = path.join(ICI, '..', 'node_modules', '.cache', 'nuee-webr', 'paquets');
const CROCHET = `--import=${pathToFileURL(path.join(ICI, 'webr_chemins.mjs')).href}`;

// webR's worker inherits this process's options: started again with the hook if it isn't there
if (!process.execArgv.some((a) => a.includes('webr_chemins.mjs'))) {
  const r = spawnSync(process.execPath, [CROCHET, ...process.execArgv, SOI, ...process.argv.slice(2)], { stdio: 'inherit' });
  process.exit(r.status ?? 1);
}

// the command line, as Rscript reads it: its options, then the file and its arguments
const argv = process.argv.slice(2);
const expressions = [];
while (argv.length && argv[0].startsWith('-') && argv[0] !== '--args') {
  const a = argv.shift();
  if (a === '-e') expressions.push(argv.shift());
  // (--vanilla, --no-echo, --default-packages...: webR's R starts without a profile, an
  // environment file or a saved workspace)
}
if (argv[0] === '--args') argv.shift();
const fichier = expressions.length ? null : argv.shift();
if (!fichier && !expressions.length) {
  console.error('node tests/webr_rscript.mjs [--vanilla] <file.R> [arguments...]');
  process.exit(2);
}
const argumentsR = argv;

const { WebR } = await import(pathToFileURL(path.join(ICI, '..', 'node_modules', 'webr', 'dist', 'webr.mjs')).href);
const webR = new WebR({ interactive: true });  // (its console reads line after line only so: see above)
await webR.init();

// what R prints, from the file on (not webR's banner); the end: the status R gives
// ("\x01nuee-statut n", from quit() or the file's end) or an error that halts it
const MARQUE = '\u0001nuee-statut ';
let affiche = false;
let finir = null;
const fin = new Promise((resolve) => { finir = resolve; });
const lecture = (async () => {
  for (;;) {
    let m;
    try { m = await webR.read(); } catch { finir(1); return; }
    if (m.type === 'closed') { finir(1); return; }
    if (m.type !== 'stdout' && m.type !== 'stderr') continue;
    // (after what a line printed without its end: a package's escape codes, say)
    const k = m.type === 'stdout' ? m.data.indexOf(MARQUE) : -1;
    if (k >= 0) { finir(Number(m.data.slice(k + MARQUE.length)) || 0); continue; }
    if (!affiche) continue;
    // (without a terminal's escape codes: cli shows the cursor again after each top-level call when R is interactive)
    (m.type === 'stdout' ? process.stdout : process.stderr).write(`${m.data.replace(/\x1b\[[0-9;?]*[A-Za-z]/g, '')}\n`);
    if (m.type === 'stderr' && m.data === 'Execution halted') finir(1);
  }
})();

async function creerDossiers(chemin) {
  const parties = chemin.split('/').filter(Boolean);
  for (let i = 1; i <= parties.length; i += 1) {
    try { await webR.FS.mkdir(`/${parties.slice(0, i).join('/')}`); } catch { /* already there */ }
  }
}

// the folders of this computer R sees, [this computer's, R's]: the folder Node starts in and the
// packages, or those of the R that started this one
const heritees = process.env.NUEE_WEBR_MONTAGES ? JSON.parse(process.env.NUEE_WEBR_MONTAGES) : null;
const montages = heritees || [[process.cwd(), '/depot'], [BIBLIOTHEQUE, '/paquets']];
fs.mkdirSync(BIBLIOTHEQUE, { recursive: true });
for (const [hote, r] of montages) {
  await creerDossiers(r);
  await webR.FS.mount('NODEFS', { root: hote }, r);
}
// R's session folder, on this computer's disk (removed at the end, as R removes it)
const tmpR = await webR.evalRString('tempdir()');
const tmpHote = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-webr-'));
await webR.FS.mount('NODEFS', { root: tmpHote }, tmpR);
montages.push([tmpHote, tmpR]);
const dossier = process.env.NUEE_WEBR_DOSSIER || '/depot';
// the file, as R sees it: relative to its working folder, or a file of this computer (under the
// folder Node starts in, or in a folder mounted at /script), or already one of R's
let fichierR = fichier;
if (fichier && !heritees && path.isAbsolute(fichier) && fs.existsSync(fichier)) {
  const rel = path.relative(process.cwd(), fichier);
  if (!rel.startsWith('..') && !path.isAbsolute(rel)) fichierR = `/depot/${rel.split(path.sep).join('/')}`;
  else {
    await creerDossiers('/script');
    await webR.FS.mount('NODEFS', { root: path.dirname(fichier) }, '/script');
    fichierR = `/script/${path.basename(fichier)}`;
  }
}

// Rscript in R: this script again, in a process of its own; the shell's words of `texte`
// (system2 quotes its arguments with shQuote), the working folder `dossier`. In webR's worker,
// where R waits for the answer: "status \x01 stdout \x01 stderr".
const rscriptJs = `globalThis.nueeRscript = (texte, dossier) => {
  const mots = [];
  let mot = null;
  for (let i = 0; i < texte.length; i += 1) {
    const c = texte[i];
    if (/\\s/.test(c)) { if (mot !== null) { mots.push(mot); mot = null; } continue; }
    mot = mot ?? '';
    if (c === "'") { const j = texte.indexOf("'", i + 1); mot += texte.slice(i + 1, j < 0 ? texte.length : j); i = j < 0 ? texte.length : j; continue; }
    if (c === '"') {
      for (i += 1; i < texte.length && texte[i] !== '"'; i += 1) mot += texte[i] === '\\\\' && i + 1 < texte.length ? texte[++i] : texte[i];
      continue;
    }
    mot += c === '\\\\' && i + 1 < texte.length ? texte[++i] : c;
  }
  if (mot !== null) mots.push(mot);
  const r = process.getBuiltinModule('child_process').spawnSync(${JSON.stringify(process.execPath)}, [${JSON.stringify(CROCHET)}, ${JSON.stringify(SOI)}, ...mots],
    { encoding: 'utf8', maxBuffer: 1 << 28, env: { ...process.env, NUEE_WEBR_MONTAGES: ${JSON.stringify(JSON.stringify(montages))}, NUEE_WEBR_DOSSIER: dossier } });
  return String(r.status ?? 1) + '\\u0001' + (r.stdout || '') + '\\u0001' + (r.stderr || (r.error ? String(r.error) : ''));
};
0;`;

// commandArgs() and interactive() as under Rscript, quit() that says its status, an error that
// halts R, and system2() that starts this script for Rscript; in an environment of the search
// path, so that the global one stays empty
const appel = ['/usr/lib/R/bin/exec/R', '--no-echo', '--no-restore', ...(fichier ? [`--file=${fichier}`] : expressions.flatMap((x) => ['-e', x]))];
const prelude = `local({
  liste <- function(x) if (nzchar(x)) strsplit(x, "\\037", fixed = TRUE)[[1]] else character(0)  # (webR passes no empty array)
  arguments_r <- liste(arguments_r)
  appel <- liste(appel)
  webr::eval_js(rscript_js)
  .libPaths(c("/paquets", .libPaths()))
  setwd(dossier)
  e <- attach(NULL, pos = 2L, name = "nuee:webr")
  e$commandArgs <- function(trailingOnly = FALSE) if (trailingOnly) arguments_r else c(appel, if (length(arguments_r)) c("--args", arguments_r))
  e$interactive <- function() FALSE
  # (and for the packages that ask rlang or cli: no cursor, spinner or colour codes, as under Rscript)
  options(rlang_interactive = FALSE, cli.dynamic = FALSE, cli.hide_cursor = FALSE, cli.num_colors = 1L, cli.hyperlink = FALSE, crayon.enabled = FALSE)
  e$quit <- e$q <- function(save = "default", status = 0, runLast = TRUE) {
    cat("\\001nuee-statut ", as.integer(status), "\\n", sep = "")
    base::quit(save = "no", status = status, runLast = runLast)
  }
  options(error = function() {
    cat("Execution halted\\n", file = base::stderr())
    cat("\\001nuee-statut 1\\n")
    base::quit(save = "no", status = 1L)
  })
  e$system2 <- function(command, args = character(), stdout = "", stderr = "", stdin = "", input = NULL, env = character(), wait = TRUE, ...) {
    if (!grepl("^Rscript(\\\\.exe)?$", basename(command))) {
      return(base::system2(command, args, stdout = stdout, stderr = stderr, stdin = stdin, input = input, env = env, wait = wait, ...))
    }
    r <- strsplit(webr::eval_js(sprintf("nueeRscript(%s, %s)", encodeString(paste(args, collapse = " "), quote = '"'),
                                        encodeString(getwd(), quote = '"'))), "\\001", fixed = TRUE)[[1]]
    length(r) <- 3
    r[is.na(r)] <- ""
    statut <- as.integer(r[1])
    lignes <- function(x) if (nzchar(x)) strsplit(sub("\\n$", "", x), "\\r?\\n")[[1]] else character(0)
    if (isTRUE(stdout) || isTRUE(stderr)) {
      res <- c(if (isTRUE(stdout)) lignes(r[2]), if (isTRUE(stderr)) lignes(r[3]))
      if (statut != 0) attr(res, "status") <- statut
      return(res)
    }
    if (identical(stdout, "")) cat(r[2]) else if (is.character(stdout) && nzchar(stdout)) writeLines(lignes(r[2]), stdout)
    if (identical(stderr, "")) cat(r[3], file = base::stderr()) else if (is.character(stderr) && nzchar(stderr)) writeLines(lignes(r[3]), stderr)
    invisible(statut)
  }
})`;
const US = '\u001f';  // (R splits these strings at it)
await webR.evalRVoid(prelude, { env: { rscript_js: rscriptJs, dossier, arguments_r: argumentsR.join(US), appel: appel.join(US) } });

// the file's text (or the expressions), then the status of a file read to its end
let texte;
try {
  texte = fichierR ? new TextDecoder().decode(await webR.FS.readFile(fichierR.startsWith('/') ? fichierR : `${dossier}/${fichierR}`)) : expressions.join('\n');
} catch (e) {
  process.stderr.write(`Fatal error: cannot open file '${fichier}': No such file or directory\n`);
  process.exit(2);
}
affiche = true;
// (line by line: webR gives R's console one input at a time, cut at 4,096 bytes)
for (const ligne of `${texte.replace(/\r\n/g, '\n').replace(/\n$/, '')}\ncat("\\001nuee-statut 0\\n")`.split('\n')) webR.writeConsole(`${ligne}\n`);
const statut = await fin;
try { webR.close(); } catch { /* already stopped */ }
await Promise.race([lecture, new Promise((r) => setTimeout(r, 500))]);
fs.rmSync(tmpHote, { recursive: true, force: true });
process.exit(statut);
