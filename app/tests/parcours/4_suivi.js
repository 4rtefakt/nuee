// Monitoring: the skylark counts of the monitoring tutorial (docs/site/donnees, made up), Model ›
// Which analysis? › a trend › of a count, which fills New model in with TRIM; fitted, its indices
// view drawn with the engine's indices. Then a table with coordinates (the made-up bird-count
// sites of moteur/tests/donnees/collecte): Plots › Map proposes its longitude and latitude, which
// are applied, and its sites drawn where they are.
const proches = (a, b, tol = 1e-9) => a.length === b.length && a.every((x, i) => (x === null && b[i] === null) || Math.abs(x - b[i]) <= tol * Math.max(1, Math.abs(b[i])));
const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));

module.exports = {
  async executer(n, verifier, { donnee }) {
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'skylark_counts.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('skylark_counts.csv'));

    // ---- Which analysis? › a trend › of a count: New model, with TRIM chosen
    await n.menu('m_modele', 'quelle_analyse');
    await n.cliquer(`document.querySelector('[data-cle="qa-question:tendance"]')`);
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
