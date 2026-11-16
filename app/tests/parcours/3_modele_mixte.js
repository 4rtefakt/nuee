// A mixed model, as the BACI tutorial makes it: the trout densities (docs/site/donnees, made up)
// opened, Model › Which analysis? › before and after with controls, the impact stations ticked,
// the controls, the event year typed, then Fit the mixed model. Its Summary shows the engine's
// numbers: the same request (as the window sent it) asked of the engine again, each figure and
// variance compared as the Summary writes them; its coefficients, effects and residuals drawn.
const proches = (a, b, tol = 1e-9) => a.length === b.length && a.every((x, i) => (x === null && b[i] === null) || Math.abs(x - b[i]) <= tol * Math.max(1, Math.abs(b[i])));

module.exports = {
  async executer(n, verifier, { donnee }) {
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'baci_trout.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('baci_trout.csv'));

    // ---- Which analysis? › before and after, with controls: the response, then its view
    await n.menu('m_modele', 'quelle_analyse');
    await n.cliquer(`document.querySelector('[data-cle="qa-question:baci"]')`);
    await n.choisirTexte(`document.querySelector('[data-cle="qa-y"]')`, 'density');
    await n.cliquer("document.querySelector('.qa-carte .qa-ouvrir button')");
    await n.attendre('!__n.dialogue()');
    // the stations: A1 to A3 the impact, the others the controls; the event in 2016
    await n.attendre("__n.tous('.unites').length === 2 && __n.tous('.unites')[0].querySelectorAll('label').length === 6");
    for (const s of ['A1', 'A2', 'A3']) {
      await n.coche(`[...__n.tous('.unites')[0].querySelectorAll('label')].find((l) => __n.norm(l.textContent) === '${s}').querySelector('input')`);
    }
    await n.cliquer(`__n.tous('.unites button').find((b) => __n.norm(b.textContent) === __n.t('baci_tous_autres'))`);
    await n.attendre(`[...__n.tous('.unites')[1].querySelectorAll('input')].filter((i) => i.checked).length === 3`);
    await n.ecrire(`__n.tous('label.champ').find((l) => __n.norm((l.querySelector('span') || {}).textContent) === __n.t('baci_evenement')).querySelector('input')`, '2016');
    // the test of the gaps, then the mixed model
    const phrase = await n.attendre("(() => { const b = __n.tous('.note b')[0]; return b && !document.querySelector('.infos.attente') ? __n.norm(b.textContent) : null; })()");
    verifier('testBaci', /2016|after|before/i.test(phrase), phrase);
    await n.plotlyPret('density', 2);
    const depuis = await n.nombreRequetes();
    await n.cliquer(`__n.tous('button').find((b) => __n.norm(b.textContent) === __n.t('baci_modele') && !b.disabled)`);
    const demande = await n.attendre(`window.__essai.requetes.slice(${depuis}).find((r) => r.nom === 'baci_modele' && r.statut === 200) || null`, 180000);
    const e = await n.etat();
    const lmm = e.modeles.find((m) => m.type === 'LMM');
    verifier('modeleMixte', !!lmm && /density/.test(lmm.nom), lmm && `${lmm.nom} (${lmm.type})`);
    await n.attendre(n.ligneModele(lmm.nom, true));

    // ---- the Summary against the engine's answer to the same request
    const r = await n.api('baci_modele', JSON.parse(demande.corps));
    const x = r.modele;
    const q = x.qualite;
    const y = [].concat(x.noms_y)[0];
    const cles = await n.evaluer("({ termes: __n.t('nb_termes'), vc: __n.t('groupes_vc'), r2m: __n.t('r2_marginal'), r2c: __n.t('r2_conditionnel'), parametres: __n.t('parametres') })");
    const chiffre = (v, d) => (typeof v === 'number' ? v.toFixed(d) : v ?? '—');
    const attendus = {
      N: chiffre(x.N, 0), [cles.termes]: chiffre(x.K, 0), [cles.vc]: x.vc_par ? `${x.G} (${x.vc_par})` : `${x.G}`,
      [cles.r2m]: chiffre(q.marginal, 3), [cles.r2c]: chiffre(q.conditionnel, 3), Q2: chiffre(q.Q2, 3),
      [`RMSEE ${y}`]: chiffre(x.RMSEE[y], 4), [`RMSEcv ${y}`]: chiffre(q.RMSEcv, 4),
      AIC: chiffre(q.AIC, 1), AICc: chiffre(q.AICc, 1), BIC: chiffre(q.BIC, 1), logLik: chiffre(q.logLik, 2), [cles.parametres]: chiffre(q.parametres, 0),
    };
    const montres = await n.attendre(`(() => { const r = __n.tous('.resume').find((x) => __n.norm((x.querySelector('.fiche-titre b') || {}).textContent) === ${JSON.stringify(lmm.nom)});
      if (!r || !r.querySelector('table.tableau')) return null;
      return { chiffres: Object.fromEntries([...r.querySelectorAll('.fiche-chiffres > span')].map((s) => [__n.norm((s.querySelector('small') || {}).textContent), __n.norm((s.querySelector('b') || {}).textContent)])),
        variances: [...r.querySelector('table.tableau').tBodies[0].rows].map((l) => [...l.cells].map((c) => __n.norm(c.textContent))) }; })()`);
    const differents = Object.entries(attendus).filter(([k, v]) => montres.chiffres[k] !== v).map(([k, v]) => `${k}: ${montres.chiffres[k]} ≠ ${v}`);
    verifier('resumeCommeLeMoteur', differents.length === 0 && Object.keys(attendus).length === 13, differents.length ? differents : montres.chiffres);
    const v = x.variances;
    const variances = [].concat(v.composante).map((c, i) => [String(c), Number([].concat(v.variance)[i]).toFixed(4), Math.sqrt([].concat(v.variance)[i]).toFixed(4)]);
    verifier('variancesCommeLeMoteur', variances.length >= 2 && variances.every((l, i) => l.every((c, j) => (montres.variances[i] || [])[j] === c)),
      { montrees: montres.variances, moteur: variances });
    // (the 84 rows less the two without a density)
    const garde = await n.api('modele', { modele: lmm.id });
    verifier('memeAjustement', x.nom === lmm.nom && x.N === 82 && garde.N === 82 && Math.abs(q.AIC - garde.qualite.AIC) < 1e-9, `${x.nom} N=${x.N} AIC=${q.AIC}`);
    await n.api('supprimer_modele', { modele: x.id });  // (the engine's second fit leaves; the project keeps its model)

    // ---- its views: the coefficients (the engine's estimates), the effects, the residuals
    const coefficients = `${lmm.nom} · ${await n.t('coefficients')}`;
    await n.plotlyPret(coefficients, 1);
    const estimations = [].concat(x.coefficients_regression.estimation);
    const traces = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(coefficients)}); return g.data.flatMap((tr) => Array.isArray(tr.x) ? [...tr.x] : []); })()`);
    verifier('coefficients', traces.length > 0 && traces.every((t) => estimations.some((s) => Math.abs(s - t) < 1e-9)), traces);
    const residus = `${lmm.nom} · ${await n.t('residus_ajustes')}`;
    await n.plotlyPret(residus, x.N);
    const ry = await n.evaluer(`[...__n.graphe(${JSON.stringify(residus)}).data[0].y]`);
    verifier('residus', proches(ry, [].concat(x.observations.residu)), ry.length);
    await n.menu('m_graphiques', 'effets');
    const ne = await n.plotlyPret(`${lmm.nom} · ${await n.t('effets')}`, 2);
    verifier('effets', ne >= 2, ne);
  },
};
