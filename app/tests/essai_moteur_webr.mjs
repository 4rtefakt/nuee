// The engine's checks (moteur/tests/verifier_*.R) in webR's R, the R of the browser version,
// each run as Rscript runs it (webr_rscript.mjs), from the repository's root; or, with
// --rscript=<Rscript>, in that R, for comparison.
//   node app/tests/essai_moteur_webr.mjs [verifier_x.R ...] [--rscript=<Rscript>] [--paralleles=N] [--sortie=<folder>]
// First the R packages of the browser version (interface/paquets_webr.js) are installed from
// webR's repository into node_modules/.cache/nuee-webr, once. Each check's output goes to
// <folder>/<check>.txt (by default a new folder of the computer's temporary folder); the summary
// gives its status, its lines "ok" and "ÉCHEC", and how long it took.
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const ICI = path.dirname(fileURLToPath(import.meta.url));
// (the interface's module, read as a module: Node would read a .js of this folder as CommonJS first)
const { PAQUETS_WEBR, SANS_WEBR } = await import(`data:text/javascript,${encodeURIComponent(fs.readFileSync(path.join(ICI, '..', 'interface', 'paquets_webr.js'), 'utf8'))}`);
const DEPOT = path.join(ICI, '..', '..');
const EMULATEUR = path.join(ICI, 'webr_rscript.mjs');
const BIBLIOTHEQUE = path.join(ICI, '..', 'node_modules', '.cache', 'nuee-webr', 'paquets');  // (webr_rscript.mjs's)

const options = Object.fromEntries(process.argv.slice(2).filter((a) => a.startsWith('--')).map((a) => {
  const [k, ...v] = a.slice(2).split('=');
  return [k, v.join('=') || true];
}));
const demandees = process.argv.slice(2).filter((a) => !a.startsWith('--'));
const rscript = typeof options.rscript === 'string' ? options.rscript : null;
const paralleles = Math.max(1, Number(options.paralleles) || Math.min(6, Math.max(1, Math.floor(os.cpus().length / 2))));
const sortie = typeof options.sortie === 'string' ? path.resolve(options.sortie)
  : fs.mkdtempSync(path.join(os.tmpdir(), `nuee-verifications-${rscript ? 'natif' : 'webr'}-`));
fs.mkdirSync(sortie, { recursive: true });
const MINUTES = Number(options.minutes) || 90;

// verifier_references.R refits the models of a folder of exported .usp projects, which the
// repository doesn't have (GitHub's checks leave it out too)
const SANS_DONNEES = ['verifier_references.R'];
const toutes = fs.readdirSync(path.join(DEPOT, 'moteur', 'tests')).filter((f) => /^verifier_.*\.R$/.test(f)).sort();
const liste = (demandees.length ? demandees.map((f) => path.basename(f)) : toutes).filter((f) => !SANS_DONNEES.includes(f));

function lancer(commande, args, journal) {
  return new Promise((resolve) => {
    const debut = Date.now();
    const flux = fs.createWriteStream(journal);
    const p = spawn(commande, args, { cwd: DEPOT, windowsHide: true });
    p.stdout.pipe(flux, { end: false });
    p.stderr.pipe(flux, { end: false });
    const minuterie = setTimeout(() => { flux.write(`\n[arrêté après ${MINUTES} min]\n`); p.kill(); }, MINUTES * 60000);
    p.on('close', (code) => {
      clearTimeout(minuterie);
      flux.end(() => resolve({ code, secondes: (Date.now() - debut) / 1000 }));
    });
  });
}

// the packages, once (webr::install installs what they depend on); those the browser version
// leaves out are not there either (paquets_webr.js: SANS_WEBR)
if (!rscript) {
  for (const p of Object.keys(SANS_WEBR)) fs.rmSync(path.join(BIBLIOTHEQUE, p), { recursive: true, force: true });
  const code = `p <- c(${PAQUETS_WEBR.map((x) => JSON.stringify(x)).join(', ')}); `
    + 'manque <- p[!vapply(p, requireNamespace, TRUE, quietly = TRUE)]; '
    + 'if (length(manque)) webr::install(manque, lib = "/paquets", mount = FALSE); '
    + 'absents <- p[!vapply(p, requireNamespace, TRUE, quietly = TRUE)]; '
    + 'if (length(absents)) stop("not installed: ", paste(absents, collapse = ", ")); '
    + 'cat(sprintf("%s %s", p, vapply(p, function(x) as.character(utils::packageVersion(x)), "")), sep = "\\n")';
  const journal = path.join(sortie, 'paquets.txt');
  const r = await lancer(process.execPath, [EMULATEUR, '-e', code], journal);
  if (r.code !== 0) {
    console.error(`Paquets de webR : échec (${journal})\n${fs.readFileSync(journal, 'utf8').slice(-2000)}`);
    process.exit(1);
  }
  console.log(`Paquets de webR (${r.secondes.toFixed(0)} s) : ${fs.readFileSync(journal, 'utf8').trim().split(/\r?\n/).join(', ')}`);
}

console.log(`${liste.length} vérifications ${rscript ? `avec ${rscript}` : 'dans webR'}, ${paralleles} à la fois ; sorties dans ${sortie}`);
const resultats = {};
const file = [...liste];
async function suivant() {
  for (let f = file.shift(); f; f = file.shift()) {
    const journal = path.join(sortie, f.replace(/\.R$/, '.txt'));
    const fichier = path.posix.join('moteur', 'tests', f);
    const r = rscript ? await lancer(rscript, [fichier], journal) : await lancer(process.execPath, [EMULATEUR, fichier], journal);
    const texte = fs.readFileSync(journal, 'utf8');
    const echecs = texte.split(/\r?\n/).filter((l) => /^\s*ÉCHEC\b/.test(l));
    resultats[f] = { ...r, ok: (texte.match(/^\s*ok\b/gm) || []).length, echecs, fin: texte.trim().split(/\r?\n/).slice(-3).join(' / ') };
    console.log(`  ${r.code === 0 ? 'ok    ' : 'ÉCHEC '} ${f.padEnd(32)} ${String(resultats[f].ok).padStart(4)} ok, ${String(echecs.length).padStart(3)} échecs, ${r.secondes.toFixed(0).padStart(5)} s${r.code === 0 ? '' : `  (code ${r.code}) ${resultats[f].fin.slice(0, 160)}`}`);
  }
}
await Promise.all(Array.from({ length: paralleles }, suivant));
fs.writeFileSync(path.join(sortie, 'resultats.json'), JSON.stringify(resultats, null, 1));
const rates = liste.filter((f) => resultats[f].code !== 0);
console.log(`\n${liste.length - rates.length} sur ${liste.length} passent${rates.length ? ` ; en échec : ${rates.join(', ')}` : ''}. Laissée de côté : ${SANS_DONNEES.join(', ')} (ses projets .usp exportés ne sont pas dans le dépôt).`);
process.exit(rates.length ? 1 : 0);
