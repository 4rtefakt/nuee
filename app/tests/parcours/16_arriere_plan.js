// Computing in the background (moteur/arriere_plan.R): the apples' table opened and a PCA fitted, then
// an R-code model whose code waits (it never looks at Stop) fitted from New model. The dialog sets
// itself aside after a moment and the window stays usable while the engine computes: the pointer is
// no progress one, the project's Variable statistics open with the engine's numbers, and the
// computation is still under way; the status bar's bar goes across, with the time since it began.
// Stop stops it all the same, a second and a half later: no model added, the dialog gone. Then code
// that fails after a moment: the dialog comes back with its error. Last, 999 permutations of a PLS:
// the status bar and the view's skeleton say the permutation under way and the time left, with the
// same bar; Stop. With screenshots for review: <screenshots>-arriere_plan-*.png.

module.exports = {
  async executer(n, verifier, { donnee, captures }) {
    const capture = async (nom) => { if (captures) await n.capture(`${captures}-arriere_plan-${nom}.png`); };
    await n.fichierAOuvrir(donnee('exemple', 'orchards.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('orchards.csv'));
    const acp = await n.nouveauModele({ type: 'PCA', sans: ['variety', 'farming', 'orchard'] });
    await n.libre();

    // New model › R code, its code written, Fit
    const boite = '__n.boite()';
    const codeR = async (code) => {
      await n.menu('m_modele', 'nouveau_modele');
      await n.dialogue('nouveau_modele');
      await n.cliquer(`__n.tous('.onglets button', ${boite}).find((b) => __n.norm(b.textContent) === __n.t('general'))`);
      await n.choisir(`__n.tous('select', ${boite}).find((s) => [...s.options].some((o) => o.value === 'PCA'))`, 'CODE');
      await n.attendre(`!!${boite}.querySelector('.editeur-code .cm-content')`);
      await n.cliquer(`${boite}.querySelector('.editeur-code .cm-content')`);
      await n.taper(code);
      await n.bouton('ajuster');
    };
    const modeles = async () => (await n.etat()).modeles.length;

    // ---- a computation that never looks at Stop: the dialog aside, the window free
    const avant = await modeles();
    const debut = Date.now();
    await codeR('Sys.sleep(60)\nlm(taste_score ~ sugar, data = donnees)');
    await n.attendre('!__n.dialogue()', 10000);
    const aCote = (Date.now() - debut) / 1000;
    // (slow CI machines: the fit waits 60 s, the dialog sets itself aside after 1.5 s)
    verifier('dialogueDeCote', aCote < 30 && await modeles() === avant && await n.evaluer("!!document.getElementById('arret-calcul') && !document.getElementById('arret-calcul').hidden"), `${aCote} s`);
    const curseur = await n.evaluer("getComputedStyle(document.querySelector('.arbre') || document.body).cursor");
    verifier('pasDeCurseurOccupe', curseur !== 'progress', curseur);
    // (code that doesn't count its rounds: a bar going across, and the time since it began)
    const sansTours = await n.attendre("(() => { const b = document.querySelector('#statut .progres.indetermine'); return b && /\\d:\\d\\d/.test(b.textContent) ? b.textContent : null; })()", 10000);
    verifier('barreSansTours', /\d:\d\d/.test(sansTours), sansTours);
    // the project's Variable statistics, which the engine computes, open meanwhile
    await n.menu('m_graphiques', 'statistiques');
    const lignes = await n.attendre("(() => { const t = [...document.querySelectorAll('.dv-content-container table.tableau')].find((x) => x.tBodies[0] && x.tBodies[0].rows.length >= 15); return t ? t.tBodies[0].rows.length : null; })()", 20000);
    const pendant = (Date.now() - debut) / 1000;
    verifier('statistiquesPendantLeCalcul', lignes >= 15 && pendant < 55 && await modeles() === avant, `${lignes} rows after ${pendant} s`);
    await capture('1-pendant');
    // Stop: the computation's R stopped, nothing added
    const arret = Date.now();
    await n.cliquer("document.querySelector('#arret-calcul button')");
    await n.attendre(`__n.statut() === __n.t('calcul_arrete') || __n.norm((document.querySelector('#statut') || {}).textContent).includes(__n.norm(__n.t('calcul_arrete')))`, 20000);
    const duree = (Date.now() - arret) / 1000;
    await n.libre();
    verifier('arreteEnLeTuant', duree < 15 && await modeles() === avant && !(await n.evaluer('!!__n.dialogue()')), `${duree} s`);
    // the engine answers at once: the PCA's model, then a new fit
    const m = await n.api('modele', { modele: acp.id });
    verifier('apresArret', m && m.id === acp.id, m && m.id);

    // ---- code that fails after a moment: the dialog back, its error in it
    await codeR('Sys.sleep(3)\nstop("on purpose")');
    await n.attendre('!__n.dialogue()', 10000);
    const retour = await n.attendre(`(() => { const b = __n.boite(); const e = b && [...b.querySelectorAll('p.erreur')].find((x) => /on purpose/.test(x.textContent)); return e ? __n.norm(e.textContent) : null; })()`, 60000);
    verifier('dialogueRevientAvecErreur', /on purpose/.test(retour) && await modeles() === avant, retour);
    await capture('2-erreur');
    await n.touche('Escape');
    await n.attendre('!__n.dialogue()');

    // ---- a computation that counts its rounds: 999 permutations of a PLS, where they are and the
    // time left, in the status bar and in the view's skeleton
    await n.nouveauModele({ type: 'PLS', y: ['taste_score'], sans: ['variety', 'farming', 'orchard'] });
    await n.libre();
    await n.menu('m_graphiques', 'permutations');
    await n.attendre("__n.tous('input[type=number]').some((i) => i.max === '999')", 120000);
    await n.libre();
    await n.ecrire("__n.tous('input[type=number]').find((i) => i.max === '999')", '999');
    const progres = await n.attendre(`(() => {
      const s = document.querySelector('#statut .progres[aria-valuenow]');
      const v = document.querySelector('.dv-content-container .infos.attente .progres[aria-valuenow]');
      if (!s || !v || Number(s.getAttribute('aria-valuenow')) < 1 || !s.textContent.includes(__n.t('progres_permutations', { fait: '', total: '999' }).split(' ')[0])
        || !/ (s|min) |seconds/.test(s.textContent)) return null;
      return { statut: s.textContent, valeur: Number(s.getAttribute('aria-valuenow')), vue: v.textContent };
    })()`, 60000);
    verifier('barreDesPermutations', progres.valeur >= 1 && progres.valeur < 100 && / (s|min) |seconds/.test(progres.statut) && progres.vue === progres.statut, progres);
    await capture('3-progres');
    await n.cliquer("document.querySelector('#arret-calcul button')");
    await n.attendre(`__n.tous('.dv-content-container p.infos').some((e) => __n.norm(e.textContent) === __n.t('calcul_arrete'))`, 30000);
    await n.libre();
    // (the fits and the permutations stopped and the code that fails, refused by the engine on
    // purpose, the code's error written to the page's console by occupe(): not errors of the run)
    await n.evaluer("(window.__essai.refus = window.__essai.refus.filter((r) => !['ajuster 400', 'permutations 400'].includes(r)), true)");
    for (let i = n.erreurs.length - 1; i >= 0; i -= 1) if (/on purpose/.test(n.erreurs[i])) n.erreurs.splice(i, 1);
  },
};
