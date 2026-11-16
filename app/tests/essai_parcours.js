// The workflows people actually do, in the built program, through its window: each scenario of
// tests/parcours/ (<number>_<name>.js, run in their numbers' order) opens files, fits models,
// opens their views, writes documents and saves, with real clicks and keys (tests/parcours/
// pilote.js), and checks what the window shows against what the engine answers. One program
// runs them all, with a temporary profile; between two scenarios its projects are closed and its
// window loaded again. Never a fixed wait: each step waits for what it needs.
//   node tests/essai_parcours.js <program> [screenshots] [program options...] [--parcours=donnees,documents]
// <screenshots>: the start of the screenshots' names (one per scenario, <screenshots>-<name>.png,
// and -erreur.png when it fails). On a server, run it under xvfb-run, as tests/essai_appli.js.
// Needs Node 22 or later (fetch, WebSocket). Exits with 1 when a check fails.
const fs = require('fs');
const os = require('os');
const path = require('path');
const { demarrer } = require('./parcours/pilote');

const args = process.argv.slice(2);
const choisis = args.filter((a) => a.startsWith('--parcours=')).flatMap((a) => a.slice(11).split(',')).filter(Boolean);
const reste = args.filter((a) => !a.startsWith('--parcours='));
const programme = reste[0];
const captures = reste[1] && !reste[1].startsWith('--') ? reste[1].replace(/\.png$/i, '') : null;
const options = reste.slice(captures ? 2 : 1);
if (!programme) {
  console.error('node tests/essai_parcours.js <program> [screenshots] [program options...] [--parcours=name,name]');
  process.exit(2);
}

const depot = path.join(__dirname, '..', '..');
const DOSSIER_PARCOURS = path.join(__dirname, 'parcours');
const SCENARIOS = fs.readdirSync(DOSSIER_PARCOURS).map((f) => /^(\d+)_([\w-]+)\.js$/.exec(f)).filter(Boolean)
  .sort((a, b) => Number(a[1]) - Number(b[1])).map((m) => ({ fichier: m[0], nom: m[2] }))
  .filter((s) => !choisis.length || choisis.includes(s.nom));
if (!SCENARIOS.length) { console.error(`no scenario named ${choisis.join(', ')} in tests/parcours`); process.exit(2); }

const secondes = (debut) => Math.round((Date.now() - debut) / 100) / 10;

(async () => {
  const debutRun = Date.now();
  const dossier = fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-parcours-'));
  const profil = path.join(dossier, 'profil');
  const resultats = {};
  let echec = false;
  let n = null;
  const journal = (texte) => console.log(`  ${texte}`);
  // the program, started with the run's profile (again: `premierLancement` false, What's new seen)
  const lancer = (premierLancement) => demarrer({ programme, options, dossier, profil, premierLancement, journal });
  // before a scenario, the window as new; a program that stopped or doesn't answer any more
  // (the scenario before failed) is started again, so that the others still run
  const pret = async () => {
    if (n.vivant()) {
      try { await n.remettreAZero(); return; } catch (e) { journal(`(the window didn’t come back: ${e.message.split('\n')[0]})`); }
    }
    n.fermer();
    journal('(Nuée started again)');
    n = await lancer(false);
    await n.remettreAZero();  // (what an abrupt stop leaves: its recovery copies, offered at the start)
  };
  try {
    console.log(`Workflows in ${programme}`);
    n = await lancer(true);
    for (const [i, s] of SCENARIOS.entries()) {
      const debut = Date.now();
      const r = { verifications: {}, echecs: [] };
      resultats[s.nom] = r;
      const verifier = (nom, ok, valeur) => {
        r.verifications[nom] = valeur === undefined ? !!ok : valeur;
        if (!ok) r.echecs.push(nom);
        return !!ok;
      };
      const ici = path.join(dossier, s.nom);
      fs.mkdirSync(ici, { recursive: true });
      // what went wrong without failing a step: a page error, a request the engine refused, a
      // security policy violation, a system dialog not answered as the scenario planned (gathered
      // from each program the scenario ran: it may start the program again)
      const bilan = { erreurs: [], refus: [], violations: [], dialogues: [] };
      let depuis = n.erreurs.length;
      const relever = async () => {
        bilan.erreurs.push(...n.erreurs.slice(depuis));
        depuis = n.erreurs.length;
        if (!n.vivant()) return;
        const espion = await n.evaluer('window.__essai').catch(() => null);
        if (espion) { bilan.refus.push(...espion.refus); bilan.violations.push(...espion.violations); }
        const d = await n.dialoguesRepondus().catch(() => null);
        if (d && (d.restent || d.sansReponse.length)) bilan.dialogues.push(d);
      };
      // what a scenario may use besides `n`: the repository's files, its own folder, and a new
      // start of the program on the same profile (the `n` it returns replaces the old one)
      const contexte = {
        depot, dossier: ici, captures,
        donnee: (...p) => path.join(depot, ...p),
        redemarrer: async () => {
          await relever();
          if (!(await n.quitter())) throw new Error('the program did not quit');
          n = await lancer(false);
          depuis = 0;
          return n;
        },
      };
      try {
        if (i > 0) await pret();
        depuis = n.erreurs.length;
        await require(path.join(DOSSIER_PARCOURS, s.fichier)).executer(n, verifier, contexte);
      } catch (e) {
        verifier('erreur', false, e.message);
      }
      await relever();
      verifier('sansRefusDuMoteur', bilan.refus.length === 0, bilan.refus);
      verifier('sansViolationCSP', bilan.violations.length === 0, bilan.violations);
      verifier('dialoguesRepondus', bilan.dialogues.length === 0, bilan.dialogues);
      verifier('sansErreurDansLaPage', bilan.erreurs.length === 0, bilan.erreurs.slice(0, 10));
      if (captures && n.vivant()) {
        await n.capture(`${captures}-${s.nom}${r.echecs.length ? '-erreur' : ''}.png`).catch(() => {});
      }
      r.secondes = secondes(debut);
      if (r.echecs.length) echec = true;
      console.log(`${r.echecs.length ? 'FAILED ' : 'ok     '} ${s.nom} (${r.secondes} s)${r.echecs.length ? `: ${r.echecs.join(', ')}` : ''}`);
    }
    resultats.quitte = await n.quitter();
    if (!resultats.quitte) echec = true;
  } catch (e) {
    resultats.erreur = e.message;
    echec = true;
  } finally {
    if (n) n.fermer();
    if (echec && n) resultats.sortieProgramme = n.sortie().slice(-3000);
    resultats.secondes = secondes(debutRun);
    console.log(JSON.stringify(resultats, null, 1));
    console.log(echec ? `Failures: ${Object.entries(resultats).filter(([, r]) => r && r.echecs && r.echecs.length).map(([k, r]) => `${k} (${r.echecs.join(', ')})`).join('; ') || resultats.erreur || 'the program did not quit'}`
      : `Every check passed (${resultats.secondes} s).`);
    try { fs.rmSync(dossier, { recursive: true, force: true, maxRetries: 10 }); } catch (e) { console.log(`(${dossier} left: ${e.message})`); }
    process.exit(echec ? 1 : 0);
  }
})();
