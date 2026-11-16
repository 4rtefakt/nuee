// Monitoring: the skylark counts of the monitoring tutorial (docs/site/donnees, made up), Model ›
// Which analysis? › a trend, which proposes the skylarks as the response (the year being the time)
// and fills New model in with TRIM; fitted, its indices view drawn with the engine's indices, and a
// document citing its overall slope, its change in % and its class. Then the negative binomial
// GLMM, Which analysis?'s alternative, with its year centred, made year * habitat in the R formula:
// its Effects on the response scale (the predicted skylarks by year in each habitat) and its
// Coefficients as exp(β), drawn with the engine's numbers; and a GAM of curves only, which opens
// on its Effects. Then a table with coordinates (the made-up bird-count sites of
// moteur/tests/donnees/collecte): Plots › Map proposes its longitude and latitude, which are
// applied, and its sites drawn where they are.
const proches = (a, b, tol = 1e-9) => a.length === b.length && a.every((x, i) => (x === null && b[i] === null) || Math.abs(x - b[i]) <= tol * Math.max(1, Math.abs(b[i])));
const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));

module.exports = {
  async executer(n, verifier, { donnee, captures }) {
    const capture = async (nom) => { if (captures) await n.capture(`${captures}-suivi-${nom}.png`); };
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'skylark_counts.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('skylark_counts.csv'));

    // ---- Which analysis? › a trend: the skylarks proposed (the year is the time, the site the
    // design), counts; New model, with TRIM chosen
    await n.menu('m_modele', 'quelle_analyse');
    await n.cliquer(`document.querySelector('[data-cle="qa-question:tendance"]')`);
    const proposee = await n.attendre(`(() => { const s = document.querySelector('[data-cle="qa-y"]'); return s && s.value ? { nom: __n.norm(s.options[s.selectedIndex].textContent),
      options: [...s.options].filter((o) => o.value).map((o) => __n.norm(o.textContent)), comptage: !!document.querySelector('[data-cle="qa-reponse:comptage"]:checked') } : null; })()`);
    verifier('reponseProposee', proposee.nom === 'skylarks' && proposee.comptage && !proposee.options.includes('year'), proposee);
    await capture('1-reponse-proposee');
    await n.cliquer(`document.querySelector('[data-cle="qa-reponse:comptage"]')`);
    await n.choisirTexte(`document.querySelector('[data-cle="qa-y"]')`, 'skylarks');
    await n.cliquer("document.querySelector('.qa-carte .qa-ouvrir button')");
    await n.dialogue('nouveau_modele');
    await n.cliquer(`__n.tous('.onglets button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('general'))`);
    const type = await n.attendre(`(() => { const s = __n.tous('select', __n.boite()).find((x) => [...x.options].some((o) => o.value === 'TRIM')); return s && s.value; })()`);
    verifier('trimPropose', type === 'TRIM', type);
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 180000);
    const m = await n.attendre("__n.api('etat').then((e) => [].concat(e.modeles).find((x) => x.type === 'TRIM') || null)");
    const trim = await n.api('modele', { modele: m.id });
    const temps = liste(trim.indices.temps);
    verifier('trimAjuste', temps.length === 17 && temps[0] === 2008 && temps[16] === 2024, `${trim.nom}: ${temps[0]}–${temps[temps.length - 1]}`);

    // ---- its Indices, from the explorer: the engine's imputed indices, 1 in the base year
    await n.attendre(n.ligneModele(trim.nom, true));
    await n.double(n.ligne(await n.t('trim_indices'), 4));
    const titre = `${trim.nom} · ${await n.t('trim_indices')}`;
    await n.plotlyPret(titre, 17);
    const imputes = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(titre)}); const tr = g.data.find((x) => x.name === __n.t('trim_indices_imputes')); return { x: [...tr.x], y: [...tr.y] }; })()`);
    const base = temps.indexOf(trim.base);
    verifier('indices', proches(imputes.y, liste(trim.indices.impute)) && proches(imputes.x, temps) && Math.abs(imputes.y[base] - 1) < 1e-9,
      imputes.y.slice(0, 4).map((v) => +v.toFixed(3)));
    // and their table under the plot, as the engine's indices with 4 decimals
    const table = await n.attendre(`(() => { const t = __n.tous('.dv-content-container table.tableau').find((x) => x.closest('.panneau') && x.closest('.panneau').querySelector('.js-plotly-plot') === __n.graphe(${JSON.stringify(titre)}));
      return t && t.tBodies[0].rows.length ? [...t.tBodies[0].rows].map((r) => __n.norm(r.cells[1].textContent)) : null; })()`);
    verifier('tableIndices', JSON.stringify(table) === JSON.stringify(liste(trim.indices.impute).map((v) => v.toFixed(4))), table.slice(0, 4));

    // ---- a document citing TRIM's key numbers (references.js: referenceCle), offered as {{ is typed
    await n.menu('m_fichier', 'nouveau_document');
    await n.dialogue('nouveau_document');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'Skylark report');
    await n.bouton('creer_document');
    await n.attendre('!__n.dialogue() && !!document.querySelector(".document .cm-content")');
    await n.cliquer("document.querySelector('.document .cm-content')");
    await n.taper(`The skylarks changed by {{${trim.nom}.pente`);
    const offertes = await n.attendre(`(() => { const l = document.querySelector('.cm-tooltip-autocomplete');
      return l && l.textContent.includes('pente.pourcent') ? [...l.querySelectorAll('li')].map((x) => __n.norm(x.textContent)) : null; })()`);
    verifier('documentTrimOffert', ['pente.ic', 'pente.pourcent', 'pente.classe'].every((c) => offertes.some((x) => x.includes(c))), offertes.slice(0, 8));
    await capture('2-document-trim-offert');
    await n.touche('Escape');
    await n.taper(`.pourcent}} a year ({{${trim.nom}.pente.classe}}): an overall slope of {{${trim.nom}.pente}} {{${trim.nom}.pente.ic}}.`);
    await n.touche('Escape');
    const ip = liste(trim.pentes.quoi).indexOf('imputee');
    const p4 = (x) => `${x < 0 ? '−' : ''}${Math.abs(x).toFixed(4)}`;
    const [pm, pb, ph] = ['multiplicative', 'bas', 'haut'].map((k) => liste(trim.pentes[k])[ip]);
    const classes = { forte_hausse: 'strong increase', hausse_moderee: 'moderate increase', stable: 'stable', incertaine: 'uncertain', declin_modere: 'moderate decline', fort_declin: 'steep decline' };
    const attendu = `The skylarks changed by ${pm < 1 ? '−' : ''}${Math.abs(100 * (pm - 1)).toFixed(1)}% a year (${classes[liste(trim.pentes.classe)[ip]]}): an overall slope of ${p4(pm)} [${p4(pb)}, ${p4(ph)}].`;
    const ecrit = await n.attendre(`(() => { const a = document.querySelector('.document .apercu-document');
      return a && !document.querySelector('.document .etat-document .erreur') && /an overall slope of [0-9]/.test(a.textContent) ? __n.norm(a.textContent) : null; })()`);
    verifier('documentTrim', ecrit.includes(attendu), { ecrit: ecrit.slice(0, 160), attendu });
    await capture('3-document-trim');

    // ---- Which analysis? again (its answers kept): the negative binomial GLMM, its year centred;
    // year * habitat in the R formula, then fitted
    await n.menu('m_modele', 'quelle_analyse');
    await n.cliquer(`__n.tous('.qa-autres details').find((d) => __n.norm(d.querySelector('.qa-nom').textContent).startsWith(__n.t('qa_nom_glm_nb'))).querySelector('summary button')`);
    await n.dialogue('nouveau_modele');
    const echelleAnnee = await n.attendre(`(() => { const r = __n.tous('tr', __n.boite()).find((x) => x.cells.length > 3 && __n.norm(x.cells[0].textContent) === 'year');
      return r && r.cells[3].querySelector('select') ? r.cells[3].querySelector('select').value : null; })()`);
    verifier('anneeCentree', echelleAnnee === 'Ctr', echelleAnnee);
    await n.cliquer(`__n.tous('.onglets button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('general'))`);
    await n.ecrire("__n.boite().querySelector('.formule-r textarea')", 'skylarks ~ year * habitat + (1 | site)');
    await n.cliquer(`__n.tous('.formule-r button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('appliquer'))`);
    await n.attendre(`__n.tous('.formule-r button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('appliquer')).disabled`);
    const avant = (await n.etat()).modeles.length;
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 240000);
    const glmm = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles); return m.length > ${avant} ? m[m.length - 1] : null; })`);
    const g = await n.api('modele', { modele: glmm.id });
    const co = (terme) => liste(g.coefficients_regression.estimation)[liste(g.coefficients_regression.terme).indexOf(terme)];
    const pa = (await n.etat()).projets.find((x) => x.nom === 'skylark_counts.csv');
    const uYear = pa.variables.find((v) => v.nom === 'year').uid;
    verifier('glmmCentre', g.type === 'GLM' && g.famille === 'binomiale_negative' && g.reglages.echelles[uYear] === 'Ctr' && Math.abs(co('habitat(cropland)')) < 5,
      { echelles: g.reglages.echelles, habitat: co('habitat(cropland)') });

    // ---- its Effects on the response scale: the skylarks the model predicts by year in each habitat
    await n.attendre(n.ligneModele(g.nom, true));
    await n.menu('m_graphiques', 'effets');
    await n.choisir(`__n.tous('.outils select').find((s) => [...s.options].some((o) => o.value === 'reponse'))`, 'reponse');
    const titreP = `${g.nom} · ${await n.t('valeurs_predites')} · year × habitat`;
    await n.plotlyPret(titreP, 80);
    const predites = await n.api('effets', { modele: g.id, echelle: 'reponse' });
    const lignes = liste(predites.effets[0].niveaux);
    const traces = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(titreP)}); return g.data.filter((t) => t.mode === 'lines' && !t.fill).map((t) => ({ nom: t.name, y: [...t.y] })); })()`);
    verifier('effetsReponse', traces.length === 2 && lignes.every((l) => { const t = traces.find((x) => x.nom === l.niveau); return t && proches(t.y, liste(l.effet)); }),
      traces.map((t) => [t.nom, +t.y[0].toFixed(3)]));
    const noteP = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(titreP)}); const s = g.closest('.panneau').querySelector('.outils .infos'); return s ? __n.norm(s.textContent) : ''; })()`);
    verifier('effetsReponseNote', noteP.includes(await n.t('fixe_aleatoires', { noms: 'site' })), noteP);
    await capture('4-effets-reponse');

    // ---- its Coefficients as exp(β) on a log axis through 1
    await n.menu('m_graphiques', 'coefficients');
    const titreC = `${g.nom} · ${await n.t('coefficients')}`;
    await n.plotlyPret(titreC, 3);
    await n.coche(`__n.graphe(${JSON.stringify(titreC)}).closest('.panneau').querySelector('.outils input[type=checkbox]')`);
    const xs = await n.attendre(`(() => { const g = __n.graphe(${JSON.stringify(titreC)}); return g && g._fullLayout.xaxis.type === 'log' ? [...g.data[0].x] : null; })()`);
    const termes = liste(g.coefficients_regression.terme).filter((x) => x !== '(Intercept)');
    verifier('coefficientsExp', proches(xs, termes.map((t) => Math.exp(co(t)))), xs);
    await capture('5-coefficients-exp');

    // ---- the same model with the year in its units, as made by hand (Model › Edit model, scaling
    // none): Model conditions say why habitat(cropland) is so large, and fit it again centred
    await n.menu('m_modele', 'modifier_modele');
    await n.dialogue('modifier_modele');
    await n.choisir(`(() => { const r = __n.tous('tr', __n.boite()).find((x) => x.cells.length > 3 && __n.norm(x.cells[0].textContent) === 'year'); return r.cells[3].querySelector('select'); })()`, 'none');
    const avantBrut = (await n.etat()).modeles.length;
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 240000);
    const brut = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles); return m.length > ${avantBrut} ? m[m.length - 1] : null; })`);
    const bouton = await n.t('verif_centrer', { nom: 'year' });
    const condition = await n.attendre(`(() => { const b = __n.tous('.verifications .verification.info button').find((x) => __n.norm(x.textContent) === ${JSON.stringify(bouton)});
      return b ? __n.norm(b.closest('.verification').textContent) : null; })()`, 120000);
    verifier('conditionCentrer', /habitat/.test(condition) && /2008/.test(condition) && /20(15|16|17)/.test(condition), condition.slice(0, 200));
    await n.cliquer(`(() => { const b = __n.tous('.verifications .verification.info button').find((x) => __n.norm(x.textContent) === ${JSON.stringify(bouton)}); b.scrollIntoView(); return b; })()`);
    await capture('7-condition-centrer');
    const centre = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles); return m.length > ${avantBrut + 1} ? m[m.length - 1] : null; })`, 240000);
    const gc = await n.api('modele', { modele: centre.id });
    const coc = (m, terme) => liste(m.coefficients_regression.estimation)[liste(m.coefficients_regression.terme).indexOf(terme)];
    const gb = await n.api('modele', { modele: brut.id });
    verifier('refitCentre', gc.reglages.echelles[uYear] === 'Ctr' && Math.abs(coc(gb, 'habitat(cropland)')) > 20 && Math.abs(coc(gc, 'habitat(cropland)') - co('habitat(cropland)')) < 1e-4,
      { brut: coc(gb, 'habitat(cropland)'), centre: coc(gc, 'habitat(cropland)'), nom: gc.nom });

    // ---- a GAM of curves only, skylarks ~ s(year) + (1 | site): its page opens on its Effects
    await n.menu('m_modele', 'nouveau_modele');
    await n.dialogue('nouveau_modele');
    await n.cliquer(`__n.tous('.onglets button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('general'))`);
    await n.choisir(`__n.tous('select', __n.boite()).find((x) => [...x.options].some((o) => o.value === 'GAM'))`, 'GAM');
    await n.ecrire("__n.boite().querySelector('.formule-r textarea')", 'skylarks ~ s(year) + (1 | site)');
    await n.cliquer(`__n.tous('.formule-r button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('appliquer'))`);
    await n.attendre(`__n.tous('.formule-r button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('appliquer')).disabled`);
    await n.choisir(`__n.tous('select', __n.boite()).find((x) => [...x.options].some((o) => o.value === 'binomiale_negative'))`, 'binomiale_negative');
    const avantGam = (await n.etat()).modeles.length;
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 240000);
    const gam = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles); return m.length > ${avantGam} ? m[m.length - 1] : null; })`);
    const titreE = `${gam.nom} · ${await n.t('effets')} · year`;
    await n.plotlyPret(titreE, 80);
    const vues = await n.evaluer(`__n.tous('.js-plotly-plot').map((x) => __n.norm(((x.layout || {}).title || {}).text || '')).filter((x) => x.startsWith(${JSON.stringify(`${gam.nom} · `)}))`);
    const titreCoef = `${gam.nom} · ${await n.t('coefficients')}`;
    verifier('gamCourbes', gam.type === 'GAM' && vues.some((v) => v.startsWith(titreE)) && !vues.some((v) => v.startsWith(titreCoef)), vues);
    await capture('6-gam-courbes');

    // ---- a table with coordinates, and its map
    await n.fichierAOuvrir(donnee('moteur', 'tests', 'donnees', 'collecte', 'envois.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('envois.csv'));
    await n.cliquer(`(${n.ligneProjet('envois.csv')}).querySelector('.libelle')`);
    await n.menu('m_graphiques', 'carte');
    // the coordinates proposed from the variables' names: longitude, latitude, WGS 84
    const proposees = await n.attendre(`(() => { const s = __n.tous('.carte-coordonnees select'); if (s.length < 3) return null;
      return s.slice(0, 3).map((x) => __n.norm(x.options[x.selectedIndex].textContent)); })()`);
    verifier('coordonneesProposees', proposees[0] === 'longitude' && proposees[1] === 'latitude' && /4326/.test(proposees[2]), proposees);
    await n.cliquer(`__n.tous('.carte-coordonnees button').find((b) => __n.norm(b.textContent) === __n.t('carte_appliquer'))`);
    const carte = `envois.csv · ${await n.t('carte')}`;
    await n.plotlyPret(carte, 1);  // (drawn once the coordinates are the project's)
    const p = (await n.etat()).projets.find((x) => x.nom === 'envois.csv');
    const valeurs = async (nom) => liste((await n.api('valeurs', { projet: p.id, variable: p.variables.find((v) => v.nom === nom).uid, observations: p.observations.uid })).valeurs);
    // (the visits with a position: one of the 20 has none, and the map says so)
    const [lon, lat] = [await valeurs('longitude'), await valeurs('latitude')];
    const places = lon.map((_, i) => i).filter((i) => lon[i] !== null && lat[i] !== null);
    await n.plotlyPret(carte, places.length);
    const xy = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(carte)}); return { x: g.data.flatMap((t) => [...t.x]), y: g.data.flatMap((t) => [...t.y]) }; })()`);
    const tri = (x) => [...x].sort((a, b) => a - b);
    verifier('carte', places.length === 19 && proches(tri(xy.x), tri(places.map((i) => lon[i]))) && proches(tri(xy.y), tri(places.map((i) => lat[i]))), `${xy.x.length} visits placed`);
    const note = await n.attendre(`(() => { const g = __n.graphe(${JSON.stringify(carte)}); const p = g && g.closest('.panneau').querySelector('p.note'); return p ? __n.norm(p.textContent) : null; })()`);
    verifier('carteSansCoordonnees', note === await n.t('carte_sans_coordonnees', { n: 1 }), note);
    verifier('coordonneesDuProjet', !!p.coordonnees && String(p.coordonnees.systeme) === '4326', p.coordonnees);
  },
};
