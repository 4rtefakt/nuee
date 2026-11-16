// PCA and PLS: the example's table opened, a PCA of its 15 measures (cross-validated in 7 groups)
// and a PLS of the taste score (cross-validated by orchard) made with Model › New model; their
// Summary, Scores, Loadings, VIP and Coefficients views drawn with the engine's numbers; two
// observations picked on the PLS scores and left out with Exclude selection and refit; the refit's
// results exported to Excel with File › Export model results, and the workbook read back.
const path = require('path');

const proches = (a, b, tol = 1e-9) => a.length === b.length && a.every((x, i) => (x === null && b[i] === null) || Math.abs(x - b[i]) <= tol * Math.max(1, Math.abs(b[i])));
const tries = (x) => [...x].map(Number).sort((a, b) => a - b);

module.exports = {
  async executer(n, verifier, { donnee, dossier }) {
    await n.fichierAOuvrir(donnee('exemple', 'orchards.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('orchards.csv'));
    const qualitatives = ['variety', 'farming', 'orchard'];

    // the Summary of model `m`: its figures (N, K, A...) and its components' table, as shown
    const resume = (m) => n.attendre(`(() => { const r = __n.tous('.resume').find((x) => __n.norm((x.querySelector('.fiche-titre b') || {}).textContent) === ${JSON.stringify(m.nom)});
      const t = r && r.querySelector('table.tableau'); if (!t || !t.tBodies[0].rows.length) return null;
      const chiffres = Object.fromEntries([...r.querySelectorAll('.fiche-chiffres > span')].map((s) => [__n.norm((s.querySelector('small') || {}).textContent), __n.norm((s.querySelector('b') || {}).textContent)]));
      const entetes = [...t.tHead.rows[0].cells].map((c) => __n.norm(c.textContent));
      return { chiffres, entetes, lignes: [...t.tBodies[0].rows].map((l) => [...l.cells].map((c) => __n.norm(c.textContent))) }; })()`);
    // a column of the components' table, as the engine's numbers are shown (3 decimals)
    const colonne = (r, titre) => r.lignes.map((l) => l[r.entetes.indexOf(titre)]);
    const montre = (x) => [].concat(x).map((v) => (v === null || v === undefined ? '' : Number(v).toFixed(3)));  // (R sends one value alone)

    // ---- a PCA of the 15 measures, cross-validated in 7 groups (the default)
    const acp = await n.nouveauModele({ type: 'PCA', sans: qualitatives });
    verifier('acpAjustee', acp.type === 'PCA' && acp.N === 120 && acp.K === 15 && acp.A >= 2 && acp.G === 7, `${acp.nom}: A=${acp.A} N=${acp.N} K=${acp.K} G=${acp.G}`);
    const ra = await resume(acp);
    verifier('acpResume', ra.chiffres.N === '120' && ra.chiffres.A === String(acp.A) && ra.lignes.length === acp.A
      && JSON.stringify(colonne(ra, 'Q2(cum)')) === JSON.stringify(montre(acp.composantes.Q2cum))
      && JSON.stringify(colonne(ra, 'R2X(cum)')) === JSON.stringify(montre(acp.composantes.R2Xcum)), `${JSON.stringify(ra.chiffres)} Q2(cum) ${colonne(ra, 'Q2(cum)')}`);
    // its page opens on the scores and the loadings: the engine's t1 and t2, and a point per variable
    const scores = await n.t('scores');
    const loadings = await n.t('loadings');
    await n.plotlyPret(`${acp.nom} · ${scores}`, 120);
    const xy = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(`${acp.nom} · ${scores}`)}); return { x: [...g.data[0].x], y: [...g.data[0].y] }; })()`);
    verifier('acpScores', proches(xy.x, acp.observations.t1) && proches(xy.y, acp.observations.t2), xy.x.slice(0, 2));
    const nl = await n.plotlyPret(`${acp.nom} · ${loadings}`, 15);
    verifier('acpLoadings', nl >= 15, nl);

    // ---- a PLS of the taste score, cross-validated by orchard (one orchard left out at a time)
    const pls = await n.nouveauModele({ type: 'PLS', y: ['taste_score'], sans: qualitatives, vc: 'orchard' });
    verifier('plsAjustee', pls.type === 'PLS' && pls.N === 120 && pls.K === 14 && [].concat(pls.noms_y).join() === 'taste_score' && pls.vc_par === 'orchard' && pls.G === 3,
      `${pls.nom}: A=${pls.A} N=${pls.N} K=${pls.K} G=${pls.G} by ${pls.vc_par}`);
    const rp = await resume(pls);
    verifier('plsResume', rp.chiffres.N === '120' && JSON.stringify(colonne(rp, 'Q2(cum)')) === JSON.stringify(montre(pls.composantes.Q2cum))
      && JSON.stringify(colonne(rp, 'R2Y(cum)')) === JSON.stringify(montre(pls.composantes.R2Ycum)), `${JSON.stringify(rp.chiffres)} Q2(cum) ${colonne(rp, 'Q2(cum)')}`);
    // Plots › VIP and Plots › Coefficients: a bar per X, the VIP as the engine computed them
    await n.menu('m_graphiques', 'vip');
    await n.plotlyPret(`${pls.nom} · VIP`, 14);
    const vip = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(`${pls.nom} · VIP`)}); return g.data.flatMap((tr) => tr.type === 'bar' ? [...tr.y] : []); })()`);
    verifier('plsVIP', proches(tries(vip), tries([].concat(pls.variables.VIP).filter((v) => v !== null))), vip.length);
    await n.menu('m_graphiques', 'coefficients');
    const nc = await n.plotlyPret(`${pls.nom} · ${await n.t('coefficients')}`, 14);
    verifier('plsCoefficients', nc >= 14, nc);

    // ---- the observations with the lowest and the highest t1, clicked on the scores, left out, refitted
    await n.menu('m_graphiques', 'scores');
    await n.plotlyPret(`${pls.nom} · ${scores}`, 120);
    const t1 = [].concat(pls.observations.t1);
    const u = [].concat(pls.observations.uid);
    const extremes = [u[t1.indexOf(Math.min(...t1))], u[t1.indexOf(Math.max(...t1))]];
    for (const x of extremes) await n.cliquerPoint(`${pls.nom} · ${scores}`, x);
    const choisies = await n.attendre(`(() => { const s = document.querySelector('#statut .sel'); return s && /^2\\b/.test(__n.norm(s.textContent)) ? __n.norm(s.textContent) : null; })()`);
    verifier('selectionSurLesScores', !!choisies, choisies);
    const avant = (await n.etat()).modeles.map((m) => m.id);
    await n.menu('m_edition', 'exclure_reajuster');
    const id = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles).find((x) => !${JSON.stringify(avant)}.includes(x.id)); return m ? m.id : null; })`);
    const refit = await n.api('modele', { modele: id });
    const sans = [].concat(refit.observations.uid);
    // (a model of its own, named after the one refitted: "M2 (1)")
    verifier('exclusEtReajuste', refit.nom.startsWith(`${pls.nom} (`) && refit.type === 'PLS' && refit.N === 118 && extremes.every((x) => !sans.includes(x)), `${refit.nom} N=${refit.N}`);
    const rr = await n.attendre(`(() => { const r = __n.tous('.resume').find((x) => __n.norm((x.querySelector('.fiche-titre b') || {}).textContent) === ${JSON.stringify(refit.nom)}
      && [...x.querySelectorAll('.fiche-chiffres > span')].some((s) => __n.norm((s.querySelector('small') || {}).textContent) === 'N' && __n.norm((s.querySelector('b') || {}).textContent) === '118'));
      return r ? __n.norm(r.querySelector('table.tableau').textContent) : null; })()`);
    verifier('resumeReajuste', rr.includes(Number([].concat(refit.composantes.Q2cum)[refit.A - 1]).toFixed(3)), rr.slice(0, 120));
    await n.plotlyPret(`${refit.nom} · ${scores}`, 118);
    verifier('scoresReajustes', (await n.evaluer(`__n.points(__n.graphe(${JSON.stringify(`${refit.nom} · ${scores}`)}))`)) === 118);

    // ---- File › Export model results: Excel, where the system's dialog says
    const fichier = path.join(dossier, 'pls-results.xlsx');
    await n.fichierAEnregistrer(fichier);
    await n.menu('m_fichier', 'exporter');
    await n.attendre(`__n.boite() && !!__n.boite().querySelector('input[name=format-export][value=xlsx]')`);
    await n.cliquer("__n.boite().querySelector('input[name=format-export][value=xlsx]')");
    await n.bouton('choisir_emplacement');
    const exporte = await n.attendre(`__n.statut().startsWith(__n.t('exporte')) && __n.statut()`);
    verifier('exporte', exporte.includes('pls-results.xlsx'), exporte);
    const feuilles = n.lireClasseur(fichier);
    const obs = feuilles.find((f) => f.lignes[0] && f.lignes[0].includes('t1'));
    const k = obs ? obs.lignes[0].indexOf('t1') : -1;
    const lus = obs ? obs.lignes.slice(1).map((l) => l[k]) : [];
    verifier('classeurExporte', !!obs && obs.lignes.length === 119 && proches(lus, [].concat(refit.observations.t1), 1e-6),
      feuilles.map((f) => `${f.nom} (${f.lignes.length - 1})`).join(', '));
  },
};
