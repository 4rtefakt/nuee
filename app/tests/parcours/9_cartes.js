// Maps and spatial models, as the maps tutorial makes them: the meadows (docs/site/donnees, made
// up) opened; Plots › Map proposes X_L93 and Y_L93 in Lambert-93, applied: the 90 meadows placed
// at their coordinates and coloured by their biomass; Edit › Add raster values describes
// elevation.tif (its cells, band and system, the 90 meadows inside, bilinear) and adds its
// elevation; Model › Which analysis? › the effect of variables on one response recommends the
// linear regression (M1), fitted: its slope, its Summary as the engine's numbers, its residuals
// said autocorrelated in space; Plots › Spatial autocorrelation of M1's residuals: Moran's I and
// the correlogram as the engine answers the same request; then the alternative, an additive model
// with a spatial smooth (M2), set up and fitted: its slope, Summary and conditions, and its smooth
// on the map. Each figure also against the one the tutorial quotes.
const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));
const proches = (a, b, tol = 1e-9) => a.length === b.length && a.every((x, i) => Math.abs(x - b[i]) <= tol * Math.max(1, Math.abs(b[i])));
const tri = (x) => [...x].sort((a, b) => a - b);
const etendue = (x) => [Math.min(...x), Math.max(...x)];

module.exports = {
  async executer(n, verifier, { donnee }) {
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'meadows.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('meadows.csv'));
    let p = (await n.etat()).projets.find((x) => x.nom === 'meadows.csv');
    verifier('ouvert', p.observations.uid.length === 90 && p.variables.map((v) => v.nom).join() === 'X_L93,Y_L93,biomass',
      `${p.observations.uid.length} observations, ${p.variables.map((v) => v.nom).join(', ')}`);
    const valeurs = async (nom) => liste((await n.api('valeurs', { projet: p.id, variable: p.variables.find((v) => v.nom === nom).uid, observations: p.observations.uid })).valeurs);

    // ---- Plots › Map: X_L93 and Y_L93 proposed, in Lambert-93, used; coloured by the biomass
    await n.cliquer(`(${n.ligneProjet('meadows.csv')}).querySelector('.libelle')`);
    await n.menu('m_graphiques', 'carte');
    const proposees = await n.attendre(`(() => { const s = __n.tous('.carte-coordonnees select'); if (s.length < 3) return null;
      return s.slice(0, 3).map((x) => __n.norm(x.options[x.selectedIndex].textContent)); })()`);
    verifier('coordonneesProposees', proposees[0] === 'X_L93' && proposees[1] === 'Y_L93' && /Lambert-93 \(EPSG:2154\)/.test(proposees[2]), proposees);
    await n.cliquer(`__n.tous('.carte-coordonnees button').find((b) => __n.norm(b.textContent) === __n.t('carte_appliquer'))`);
    const carte = `meadows.csv · ${await n.t('carte')}`;
    await n.plotlyPret(carte, 90);
    const xy = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(carte)}); return { x: g.data.flatMap((t) => [...t.x]), y: g.data.flatMap((t) => [...t.y]) }; })()`);
    verifier('carte', proches(tri(xy.x), tri(await valeurs('X_L93'))) && proches(tri(xy.y), tri(await valeurs('Y_L93'))), `${xy.x.length} meadows placed`);
    const couleur = `__n.tous('label.champ').find((l) => __n.norm((l.querySelector('span') || {}).textContent) === __n.t('couleur')
      && l.closest('.panneau') === __n.graphe(${JSON.stringify(carte)}).closest('.panneau')).querySelector('select')`;
    const couleurs = async (titre) => n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(titre)});
      return { uid: g.data.flatMap((t) => Array.isArray(t.marker.color) ? [...t.customdata] : []), v: g.data.flatMap((t) => Array.isArray(t.marker.color) ? [...t.marker.color] : []) }; })()`);
    await n.choisirTexte(couleur, 'biomass');
    await n.plotlyPret(`${carte} · biomass`, 90);
    const biomasse = await couleurs(`${carte} · biomass`);
    const attendue = await valeurs('biomass');
    const [b0, b1] = etendue(biomasse.v);
    verifier('carteBiomasse', proches(biomasse.v, biomasse.uid.map((u) => attendue[p.observations.uid.indexOf(u)])) && b0 === 431.2 && b1 === 653.4,
      `${biomasse.v.length} colours, ${b0} to ${b1} g/m²`);

    // ---- Edit › Add raster values: elevation.tif described before adding, then added
    await n.menu('m_edition', 'raster_valeurs');
    await n.dialogue('raster_valeurs');
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'elevation.tif'));
    await n.cliquer(`__n.tous('button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('raster_choisir'))`);
    const fiche = await n.attendre(`(() => { const f = __n.boite().querySelector('.raster-fichier'); if (!f) return null;
      return { infos: [...f.querySelectorAll('p.infos, p.erreur')].map((x) => __n.norm(x.textContent)), methode: f.querySelector('.grille-champs select').value,
        nom: f.querySelector('.grille-champs input:not([type="number"])').value }; })()`);
    const textes = await n.evaluer(`[__n.t('raster_cellules', { l: '240', h: '180', r: '50 m' }), __n.t('raster_bande', { type: __n.t('raster_type_int16') }),
      __n.t('raster_dedans_tous', { n: 90, total: 90 })]`);
    verifier('rasterDecrit', fiche.infos[0].includes(textes[0]) && fiche.infos[0].includes(textes[1]) && fiche.infos[0].includes('Lambert-93 (EPSG:2154)')
      && fiche.infos[1] === textes[2] && fiche.methode === 'bilineaire' && fiche.nom === 'elevation', fiche);
    await n.bouton('ajouter');
    await n.attendre(`__n.titreDialogue().startsWith(__n.t('raster_fait'))`);
    const ajout = await n.evaluer(`__n.norm(__n.boite().querySelector('p').textContent)`);
    await n.bouton('fermer');
    await n.attendre('!__n.dialogue()');
    p = (await n.etat()).projets.find((x) => x.nom === 'meadows.csv');
    const altitudes = await valeurs('elevation');
    const [a0, a1] = etendue(altitudes);
    verifier('rasterAjoute', /elevation.*\b90\b.*\b90\b/.test(ajout) && altitudes.length === 90 && altitudes.every((v) => typeof v === 'number')
      && a0.toFixed(4) === '1138.4800' && a1.toFixed(4) === '1334.2024', `${ajout} ${a0.toFixed(4)} to ${a1.toFixed(4)} m`);

    // ---- Model › Which analysis? › the effect of variables on one response: biomass
    await n.menu('m_modele', 'quelle_analyse');
    await n.cliquer(`document.querySelector('[data-cle="qa-question:effet"]')`);
    await n.choisirTexte(`document.querySelector('[data-cle="qa-y"]')`, 'biomass');
    const choix = await n.attendre(`(() => { const c = document.querySelector('.qa-carte h4'); if (!c) return null;
      return { recommandee: __n.norm(c.textContent), autres: [...document.querySelectorAll('.qa-autres .qa-nom')].map((x) => __n.norm(x.textContent)),
        rempli: __n.norm((document.querySelector('.qa-carte .qa-rempli') || {}).textContent) }; })()`);
    const noms = await n.evaluer("[__n.t('qa_nom_normal'), __n.t('qa_nom_gam_spatial')]");
    verifier('quelleAnalyse', choix.recommandee === noms[0] && choix.autres.includes(noms[1]) && /biomass/.test(choix.rempli) && /elevation/.test(choix.rempli), choix);
    const ajuster = async () => {
      const avant = (await n.etat()).modeles.map((m) => m.id);
      await n.bouton('ajuster');
      await n.attendre('!__n.dialogue()', 180000);
      const id = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles).find((x) => !${JSON.stringify(avant)}.includes(x.id)); return m ? m.id : null; })`);
      const m = await n.api('modele', { modele: id });
      await n.attendre(n.ligneModele(m.nom, true));
      return m;
    };
    await n.cliquer("document.querySelector('.qa-carte .qa-ouvrir button')");
    await n.dialogue('nouveau_modele');
    const m1 = await ajuster();
    // the Summary's figures and its conditions checked in words
    const resume = (nom) => n.attendre(`(() => { const r = __n.tous('.resume').find((x) => __n.norm((x.querySelector('.fiche-titre b') || {}).textContent) === ${JSON.stringify(nom)});
      if (!r || !r.querySelector('.verifications .verification')) return null;
      return { chiffres: Object.fromEntries([...r.querySelectorAll('.fiche-chiffres > span')].map((s) => [__n.norm((s.querySelector('small') || {}).textContent), __n.norm((s.querySelector('b') || {}).textContent)])),
        conditions: [...r.querySelectorAll('.verifications .verification')].map((v) => ({ statut: [...v.classList].find((c) => c !== 'verification'), texte: __n.norm(v.textContent) })) }; })()`);
    const ajuste = await n.t('r2_ajuste');
    const pente = (m) => { const c = m.coefficients_regression; const k = liste(c.terme).indexOf('elevation');
      return [liste(c.estimation)[k], liste(c.bas)[k], liste(c.haut)[k]].map((v) => v.toFixed(4)).join(' '); };
    const spatiale = (r) => r.conditions.find((c) => /Moran/.test(c.texte)) || {};
    const r1 = await resume(m1.nom);
    const q1 = m1.qualite;
    verifier('m1', m1.type === 'LM' && pente(m1) === '-0.8056 -1.0520 -0.5591' && liste(m1.coefficients_regression.p)[1].toPrecision(2) === '4.8e-9', `${m1.nom} (${m1.type}): ${pente(m1)}`);
    verifier('resumeM1', r1.chiffres.R2 === q1.r2.toFixed(3) && r1.chiffres[ajuste] === q1.r2_ajuste.toFixed(3) && r1.chiffres.Q2 === q1.Q2.toFixed(3) && r1.chiffres.AIC === q1.AIC.toFixed(1)
      && r1.chiffres.R2 === '0.324' && r1.chiffres[ajuste] === '0.316' && r1.chiffres.Q2 === '0.311' && r1.chiffres.AIC === '920.5', r1.chiffres);
    verifier('conditionsM1', spatiale(r1).statut === 'attention' && /Moran's I = 0\.312, p < 0\.001/.test(spatiale(r1).texte) && /p = 0\.15\b/.test(r1.conditions[0].texte), r1.conditions.map((c) => c.statut));

    // ---- Plots › Spatial autocorrelation, M1 chosen: Moran's I of its residuals, as the engine answers
    await n.cliquer(`(${n.ligneModele(m1.nom, true)}).querySelector('.libelle')`);
    const depuis = await n.nombreRequetes();
    await n.menu('m_graphiques', 'autocorrelation_spatiale');
    const lignes = await n.attendre(`(() => { const r = __n.tous('.resume-autocorrelation')[0]; return r ? [...r.querySelectorAll('p')].map((x) => __n.norm(x.textContent)) : null; })()`);
    const demande = await n.attendre(`window.__essai.requetes.slice(${depuis}).find((r) => r.nom === 'autocorrelation_spatiale' && r.statut === 200) || null`);
    const q = JSON.parse(demande.corps);
    const ac = await n.api('autocorrelation_spatiale', q);
    const mo = ac.moran;
    const nombres = (s) => (s.match(/-?\d+(\.\d+)?(e-?\d+)?/g) || []);
    verifier('moranDesResidus', q.modele === m1.id && q.residus === 'standardises' && q.voisinage.k === 4 && q.permutations === 999, demande.corps);
    verifier('moran', JSON.stringify(nombres(lignes[0])) === JSON.stringify([mo.I.toFixed(4), mo.esperance.toFixed(4), mo.z.toFixed(2), '7.0e-7'])
      && mo.p.toPrecision(2) === '7.0e-7' && mo.I.toFixed(4) === '0.3120' && mo.esperance.toFixed(4) === '-0.0112' && mo.z.toFixed(2) === '4.83', lignes[0]);
    verifier('moranPermutations', lignes[1].endsWith('p = 0.0010.') && mo.p_permutations === 0.001, lignes[1]);
    verifier('moranModele', JSON.stringify(nombres(lignes[2])) === JSON.stringify([mo.modele.I.toFixed(4), mo.modele.esperance.toFixed(4), mo.modele.z.toFixed(2), '3.8e-7'])
      && mo.modele.I.toFixed(4) === '0.3112' && mo.modele.p.toPrecision(2) === '3.8e-7', lignes[2]);
    verifier('voisins', ac.voisinage.voisins === 4.8 && lignes[3].includes('4.8'), ac.voisinage);
    const correlogramme = `${await n.t('autocorrelation_correlogramme')} · `;
    await n.plotlyPret(correlogramme, 2);
    const iCorr = await n.evaluer(`[...__n.graphe(${JSON.stringify(correlogramme)}).data[0].y]`);
    const cg = ac.correlogramme;
    verifier('correlogramme', proches(iCorr, liste(cg.I)) && liste(cg.I).slice(0, 3).map((v) => v.toFixed(2)).join() === '0.41,0.34,0.18'
      && liste(cg.a).slice(0, 3).map((v) => (v / 1000).toFixed(2)).join() === '0.69,1.38,2.07', iCorr.slice(0, 3).map((v) => +v.toFixed(2)));

    // ---- the alternative: an additive model with a spatial smooth, set up by Which analysis?
    await n.menu('m_modele', 'quelle_analyse');
    const gam = `[...document.querySelectorAll('.qa-autres details')].find((d) => __n.norm(d.querySelector('.qa-nom').textContent) === __n.t('qa_nom_gam_spatial'))`;
    await n.cliquer(`(${gam}).querySelector('summary button')`);
    await n.dialogue('nouveau_modele');
    await n.cliquer(`__n.tous('.onglets button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('general'))`);
    const reglages = await n.attendre(`(() => { const b = __n.boite(); const z = b.querySelector('.formule-r textarea.formule'); if (!z || !z.value) return null;
      const type = __n.tous('select', b).find((s) => [...s.options].some((o) => o.value === 'PCA'));
      const l = __n.tous('label', b).find((x) => __n.norm(x.textContent) === __n.t('lissage_spatial'));
      const ligne = l && l.nextElementSibling;
      return { type: type && type.value, formule: z.value, spatial: ligne ? { coche: ligne.querySelector('input[type=checkbox]').checked, texte: __n.norm(ligne.textContent) } : null }; })()`);
    verifier('gamPropose', reglages.type === 'GAM' && reglages.spatial.coche && reglages.spatial.texte === 's(X_L93, Y_L93)'
      && reglages.formule === 'biomass ~ elevation + s(X_L93, Y_L93)', reglages);
    const m2 = await ajuster();
    const r2 = await resume(m2.nom);
    const q2 = m2.qualite;
    const lisse = m2.courbes;
    verifier('m2', m2.type === 'GAM' && pente(m2) === '-0.8476 -1.1101 -0.5850' && liste(lisse.edf)[0].toFixed(2) === '8.88' && liste(lisse.p)[0].toFixed(4) === '0.0011'
      && liste(lisse.concurvite)[0].toFixed(2) === '0.04', `${m2.nom} (${m2.type}): ${pente(m2)}, edf ${liste(lisse.edf)[0]}`);
    verifier('resumeM2', r2.chiffres[ajuste] === q2.r2_ajuste.toFixed(3) && r2.chiffres.Q2 === q2.Q2.toFixed(3) && r2.chiffres.AIC === q2.AIC.toFixed(1)
      && r2.chiffres[ajuste] === '0.526' && r2.chiffres.Q2 === '0.459' && r2.chiffres.AIC === '902.2' && Math.round(q1.AIC - q2.AIC) === 18, r2.chiffres);
    verifier('conditionsM2', spatiale(r2).statut === 'ok' && /Moran's I = -0\.036, p = 0\.64/.test(spatiale(r2).texte) && r2.conditions.every((c) => c.statut === 'ok'),
      spatiale(r2).texte);

    // ---- the smooth on the map: Colour M2 · s(X_L93, Y_L93), from −50.5 to +25.4 g/m²
    await n.cliquer(`(${n.ligneProjet('meadows.csv')}).querySelector('.libelle')`);
    await n.menu('m_graphiques', 'carte');
    const surface = `${m2.nom} · s(X_L93, Y_L93)`;
    await n.choisirTexte(couleur, surface);
    const colonne = (await n.evaluer(`(${couleur}).value`)).split('|')[1];
    await n.plotlyPret(`${carte} · ${surface}`, 90);
    const lissage = await couleurs(`${carte} · ${surface}`);
    const index = new Map(liste(m2.observations.uid).map((u, i) => [u, i]));
    const [l0, l1] = etendue(lissage.v);
    verifier('carteLissage', proches(lissage.v, lissage.uid.map((u) => liste(m2.observations[colonne])[index.get(u)])) && l0.toFixed(1) === '-50.5' && l1.toFixed(1) === '25.4',
      `${colonne}: ${l0.toFixed(1)} to ${l1.toFixed(1)} g/m²`);
  },
};
