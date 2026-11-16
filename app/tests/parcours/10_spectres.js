// A PLS on near-infrared spectra, as the PLS tutorial makes it: the soils' spectra (docs/site/donnees,
// made up) opened, Model › New model filled in as it says (PLS, centring only, SNV then detrend,
// components by cross-validation in 7 groups; Role… X for all, then soc Y and land_use without a
// role), fitted. Its Summary shows the engine's figures and the tutorial's (3 components, R2X 0.987,
// R2Y 0.806, Q2 0.78, RMSEE 4.4, RMSEcv 4.6, gain 0.78, CV-ANOVA p < 0.001); Plots › Profiles
// (spectra): the spectra as the engine preprocessed them, before preprocessing (the file's values),
// then the VIP (the engine's, highest at 2320, 2330, 2310 and 2340 nm, as the Results text says);
// Observed vs predicted coloured by land_use (the engine's fitted carbon); the permutation test with
// 100 permutations (its intercepts, R2 0.02 and Q2 −0.13); Predict › Predict a file: the 20 new
// soils, each predicted carbon the engine's, N020 alone outside the model's domain, about 45 g/kg;
// Model › Text for the Methods section: the sentences the tutorial quotes.
const fs = require('fs');

const proches = (a, b, tol = 1e-9) => a.length === b.length && a.every((x, i) => (x === null && b[i] === null) || Math.abs(x - b[i]) <= tol * Math.max(1, Math.abs(b[i])));
const fmt = (v, d) => Number(v).toFixed(d);

module.exports = {
  async executer(n, verifier, { donnee }) {
    const fichier = donnee('docs', 'site', 'donnees', 'soils_spectra.csv');
    await n.fichierAOuvrir(fichier);
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('soils_spectra.csv'));
    const projet = (await n.etat()).projets[0];
    verifier('donnees', projet.observations.uid.length === 120 && projet.variables.length === 143,
      `${projet.observations.uid.length} obs. × ${projet.variables.length} variables`);

    // ---- Model › New model: General tab (PLS, centring only, SNV then detrend, automatic components)
    const boite = '__n.boite()';
    const select = (valeur, k = 0) => `__n.tous('select', ${boite}).filter((s) => [...s.options].some((o) => o.value === ${JSON.stringify(valeur)}))[${k}]`;
    const onglet = (cle) => n.cliquer(`__n.tous('.onglets button', ${boite}).find((b) => __n.norm(b.textContent) === __n.t('${cle}'))`);
    await n.menu('m_modele', 'nouveau_modele');
    await n.dialogue('nouveau_modele');
    await onglet('general');
    await n.choisir(select('PCA'), 'PLS');
    await n.choisir(select('Ctr'), 'Ctr');
    await n.choisir(select('snv', 0), 'snv');
    await n.choisir(select('snv', 1), 'detrend');
    await n.coche(`[...__n.boite().querySelectorAll('label.champ')].find((l) => __n.norm(l.textContent) === __n.t('auto')).querySelector('input')`);
    const general = await n.evaluer(`({ vc: ${select('ordre')}.value, G: ${select('ordre')}.parentElement.querySelector('input[type=number]').value,
      etapes: __n.tous('select', ${boite}).filter((s) => [...s.options].some((o) => o.value === 'snv')).map((s) => s.value) })`);
    verifier('ongletGeneral', general.vc === 'ordre' && general.G === '7' && general.etapes.join(',') === 'snv,detrend,', general);
    // ---- the Variables tab: Role… X for all, then soc Y and land_use without a role
    await onglet('variables');
    await n.choisir(`__n.tous('.onglet-vars .outils select', ${boite})[0]`, 'x');
    const role = (nom) => `__n.tous('.onglet-vars tbody tr', ${boite}).find((r) => __n.norm(r.cells[0].textContent) === ${JSON.stringify(nom)}).cells[2].querySelector('select')`;
    await n.choisir(role('soc'), 'y');
    await n.choisir(role('land_use'), '');
    const roles = await n.t('resume_dialogue', { x: 141, y: 1, n: 120 });
    const resume = await n.attendre(`__n.boite().textContent.includes(${JSON.stringify(roles)}) && ${JSON.stringify(roles)}`);
    verifier('roles', !!resume, resume);
    const avant = (await n.etat()).modeles.map((m) => m.id);
    const envoi = await n.nombreRequetes();
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 180000);
    const id = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles).find((x) => !${JSON.stringify(avant)}.includes(x.id)); return m ? m.id : null; })`);
    const m = await n.api('modele', { modele: id });
    await n.attendre(n.ligneModele(m.nom, true));
    // (the fit as the window asked for it: centring only, SNV then detrend, components by cross-validation)
    const q = JSON.parse((await n.evaluer(`window.__essai.requetes.slice(${envoi}).filter((r) => r.nom !== 'verifier' && r.statut === 200 && /"spectres"/.test(r.corps || '')).pop()`)).corps);
    const spectres = [].concat(m.reglages.spectres).map((e) => e.type).join(',');
    verifier('plsAjustee', m.type === 'PLS' && m.N === 120 && m.K === 141 && spectres === 'snv,detrend' && q.echelle === 'Ctr' && q.A === 0 && q.G === 7 && q.vc === null,
      `${m.nom} ${m.type} N=${m.N} K=${m.K} ${spectres}, asked: ${q.echelle} A=${q.A} G=${q.G}`);

    // ---- the Summary: the engine's figures, and the tutorial's
    const c = m.composantes;
    const dernier = (k) => [].concat(c[k]).slice(-1)[0];
    const cles = await n.evaluer("({ vc: __n.t('groupes_vc'), validation: __n.t('validation_vc'), gain: __n.t('gain_vide'), spectres: __n.t('spectres') })");
    const p = [].concat(m.cv_anova.soc.p)[1];
    const attendus = {
      A: String(m.A), N: String(m.N), K: String(m.K), [cles.vc]: m.vc_par ? `${m.G} (${m.vc_par})` : `${m.G}`,
      [cles.validation]: await n.t('validation_' + m.validation), [cles.gain]: fmt(m.gain, 3),
      [cles.spectres]: `${await n.t('etape_snv')} → ${await n.t('etape_detrend')}`,
      'RMSEE soc': fmt(m.RMSEE.soc, 4), 'RMSEcv soc': fmt(m.RMSEcv.soc, 4), 'CV-ANOVA p soc': p < 0.001 ? Number(p).toExponential(1) : fmt(p, 3),
    };
    const montres = await n.attendre(`(() => { const r = __n.tous('.resume').find((x) => __n.norm((x.querySelector('.fiche-titre b') || {}).textContent) === ${JSON.stringify(m.nom)});
      if (!r || !r.querySelector('table.tableau')) return null;
      return { chiffres: Object.fromEntries([...r.querySelectorAll('.fiche-chiffres > span')].map((s) => [__n.norm((s.querySelector('small') || {}).textContent), __n.norm((s.querySelector('b') || {}).textContent)])),
        composantes: [...r.querySelector('table.tableau').tBodies[0].rows].map((l) => [...l.cells].map((x) => __n.norm(x.textContent))) }; })()`);
    const differents = Object.entries(attendus).filter(([k, v]) => montres.chiffres[k] !== v).map(([k, v]) => `${k}: ${montres.chiffres[k]} ≠ ${v}`);
    verifier('resumeCommeLeMoteur', differents.length === 0, differents.length ? differents : montres.chiffres);
    const lignes = [].concat(c.composante).map((a, i) => [String(a), ...['R2X', 'R2Xcum', 'R2Y', 'R2Ycum', 'Q2', 'Q2cum', 'valeur_propre'].map((k) => fmt([].concat(c[k])[i], 3))]);
    verifier('composantesCommeLeMoteur', lignes.length === 3 && lignes.every((l, i) => l.join('|') === (montres.composantes[i] || []).join('|')),
      { montrees: montres.composantes, moteur: lignes });
    // the tutorial: 3 components, 98.7% of X, 80.6% of Y, Q2 0.78, RMSEcv 4.6 and RMSEE 4.4 g/kg, gain 0.78, p < 0.001
    const tutoriel = { A: m.A === 3, R2X: fmt(100 * dernier('R2Xcum'), 1) === '98.7', R2Y: fmt(100 * dernier('R2Ycum'), 1) === '80.6',
      Q2: fmt(dernier('Q2cum'), 2) === '0.78', RMSEcv: fmt(m.RMSEcv.soc, 1) === '4.6', RMSEE: fmt(m.RMSEE.soc, 1) === '4.4',
      gain: fmt(m.gain, 2) === '0.78', p: p < 0.001 };
    verifier('commeLeTutoriel', Object.values(tutoriel).every(Boolean),
      { ...tutoriel, valeurs: [m.A, dernier('R2Xcum'), dernier('R2Ycum'), dernier('Q2cum'), m.RMSEcv.soc, m.RMSEE.soc, m.gain, p] });

    // ---- Plots › Profiles (spectra), Show: Spectra; the spectra as the engine preprocessed them
    await n.menu('m_graphiques', 'profils');
    const montrer = `__n.tous('select').find((s) => [...s.options].some((o) => o.value === 'vip') && [...s.options].some((o) => o.value === 'spectres'))`;
    await n.choisir(montrer, 'spectres');
    const pretraites = `${m.nom} · ${await n.t('profil_spectres')} (${attendus[cles.spectres]})`;
    await n.plotlyPret(pretraites, 120 * 141);
    // (the engine sends them to 6 significant digits; detrending leaves each spectrum a mean of 0)
    const moteur = await n.api('spectres', { modele: id, quoi: 'pretraites' });
    const dessin = (titre) => n.evaluer(`(() => { const tr = __n.graphe(${JSON.stringify(titre)}).data[0]; const s = {};
      tr.y.forEach((y, k) => { if (y !== null) (s[tr.text[k]] = s[tr.text[k]] || []).push(y); }); return s; })()`);
    const traits = await dessin(pretraites);
    const noms = [].concat(moteur.uid).map((u) => m.observations.nom[m.observations.uid.indexOf(u)]);
    const moyennes = [].concat(moteur.pretraites).map((s) => s.reduce((a, b) => a + b, 0) / s.length);
    verifier('spectresPretraites', Object.keys(traits).length === 120 && noms.every((nom, i) => proches(traits[nom], moteur.pretraites[i]))
      && moyennes.every((x) => Math.abs(x) < 1e-5), `${Object.keys(traits).length} spectra, means ${Math.max(...moyennes.map(Math.abs)).toExponential(1)}`);
    // ... ticked Before preprocessing: the file's values
    await n.coche(`[...document.querySelectorAll('label.champ')].find((l) => __n.visible(l) && __n.norm(l.textContent) === __n.t('spectres_bruts')).querySelector('input')`);
    const bruts = `${m.nom} · ${await n.t('profil_spectres')} (${(await n.t('spectres_bruts')).toLowerCase()})`;
    await n.plotlyPret(bruts, 120 * 141);
    const lus = await dessin(bruts);
    const csv = fs.readFileSync(fichier, 'utf8').split(/\r?\n/).filter(Boolean).map((l) => l.split(',').map((x) => x.replace(/"/g, '')));
    const iSpectre = csv[0].indexOf('1100');
    const differentes = csv.slice(1).filter((l) => !proches(lus[l[0]] || [], l.slice(iSpectre).map(Number), 0)).map((l) => l[0]);
    verifier('spectresBruts', Object.keys(lus).length === 120 && differentes.length === 0, differentes.length ? differentes : `${Object.keys(lus).length} spectra as in the file`);

    // ---- Show: VIP, the engine's; the highest at the bands of organic matter
    await n.choisir(montrer, 'vip');
    const vip = `${m.nom} · VIP`;
    await n.plotlyPret(vip, 141);
    const vy = await n.evaluer(`[...__n.graphe(${JSON.stringify(vip)}).data[0].y]`);
    verifier('vipCommeLeMoteur', proches(vy, [].concat(m.variables.VIP)), vy.length);
    const termes = [].concat(m.variables.terme);
    const premiers = [].concat(m.variables.VIP).map((v, i) => [termes[i], v]).sort((a, b) => b[1] - a[1]).slice(0, 8)
      .map(([t, v]) => `${String(t).replace(/`/g, '')} (${fmt(v, 1)})`);
    verifier('vipCommeLeTutoriel', premiers.join(', ') === '2320 (3.5), 2330 (3.4), 2310 (3.3), 2340 (3.2), 2300 (2.8), 2350 (2.6), 2290 (2.1), 1720 (2.1)', premiers);

    // ---- Plots › Observed vs predicted, coloured by land_use: the engine's fitted carbon, grasslands
    // above croplands, the soils beyond 40 g/kg a little underestimated
    await n.menu('m_graphiques', 'obspred');
    const obspred = `${m.nom} · soc`;
    await n.plotlyPret(obspred, 120);
    const usage = projet.variables.find((v) => v.nom === 'land_use').uid;
    await n.choisir(`__n.graphe(${JSON.stringify(obspred)}).parentElement.querySelector('.outils select option[value="v${usage}"]').parentElement`, `v${usage}`);
    const nuage = await n.attendre(`(() => { const g = __n.graphe(${JSON.stringify(obspred)}); if (!g || __n.points(g) < 120) return null;
      const tr = g.data.filter((x) => ['grassland', 'cropland'].includes(x.name)); return tr.length === 2 ? tr.map((x) => ({ nom: x.name, x: [...x.x], y: [...x.y] })) : null; })()`);
    const observe = [].concat(m.observations['soc observé']);
    const ajuste = [].concat(m.observations['soc ajusté']);
    const memes = nuage.every((tr) => tr.x.every((x, k) => observe.some((o, i) => o === x && Math.abs(ajuste[i] - tr.y[k]) < 1e-9)));
    const moyenne = (v) => v.reduce((a, b) => a + b, 0) / v.length;
    const [prairies, cultures] = ['grassland', 'cropland'].map((nom) => moyenne(nuage.find((tr) => tr.nom === nom).y));
    const riches = observe.map((o, i) => o - ajuste[i]).filter((_, i) => observe[i] > 40);
    verifier('observePredit', memes && nuage.reduce((s, tr) => s + tr.x.length, 0) === 120 && prairies > cultures && riches.length > 0 && moyenne(riches) > 0,
      { grassland: prairies, cropland: cultures, auDela40: riches.length, ecartMoyen: riches.length ? moyenne(riches) : null });

    // ---- Plots › Permutation test, 100 permutations: the intercepts the tutorial quotes, R2 0.02 and Q2 −0.13
    await n.menu('m_graphiques', 'permutations');
    const nombre = `[...document.querySelectorAll('.outils label.champ')].find((l) => __n.visible(l) && __n.norm((l.querySelector('span') || {}).textContent) === __n.t('nb_permutations')).querySelector('input')`;
    await n.ecrire(nombre, '100');
    const permutations = `${m.nom} · ${await n.t('permutations')} · soc · 100`;
    await n.plotlyPret(permutations, 2 * 101);
    // (the real model's point, at a correlation of 1: the engine's R2Y(cum) and Q2(cum); the permutations are seeded)
    const test = await n.evaluer(`(() => { const g = __n.graphe(${JSON.stringify(permutations)});
      return { ordonnees: __n.norm(g.parentElement.querySelector('.outils .infos').textContent),
        origine: g.data.filter((tr) => tr.mode === 'markers' && tr.x.length === 1).map((tr) => [tr.x[0], tr.y[0]]) }; })()`);
    const [r2, q2] = (/R2 = (-?[\d.]+) · Q2 = (-?[\d.]+)/.exec(test.ordonnees) || []).slice(1).map(Number);
    verifier('permutations', proches(test.origine.flat(), [1, dernier('R2Ycum'), 1, dernier('Q2cum')]) && fmt(r2, 2) === '0.02' && fmt(q2, 2) === '-0.13',
      test);

    // ---- Predict › Predict a file: the 20 new soils
    const nouveaux = donnee('docs', 'site', 'donnees', 'soils_new.csv');
    await n.fichierAOuvrir(nouveaux);
    const depuis = await n.nombreRequetes();
    await n.menu('m_prediction', 'predire_fichier');
    const demande = await n.attendre(`window.__essai.requetes.slice(${depuis}).find((r) => r.nom === 'predire_fichier' && r.statut === 200) || null`);
    const titre = `${m.nom} · ${await n.t('predictions')} · soils_new.csv`;
    await n.plotlyPret(titre, 120 + 20);
    const r = await n.api('predire_fichier', JSON.parse(demande.corps));
    const o = r.observations;
    const colonnes = Object.keys(o);
    const cle = colonnes.find((k) => /soc/.test(k) && !/obs/i.test(k));
    const table = await n.attendre(`(() => { const t = __n.tous('.bas table.tableau').pop(); if (!t || t.tBodies[0].rows.length < 20) return null;
      const tetes = [...t.tHead.rows[0].cells].map((x) => __n.norm(x.textContent));
      return { tetes, lignes: [...t.tBodies[0].rows].map((l) => [...l.cells].map((x) => __n.norm(x.textContent))) }; })()`);
    verifier('predictions', o.nom.length === 20 && table.lignes.length === 20, { obs: o.nom.length, lignes: table.lignes.length, colonnes });
    const k = table.tetes.findIndex((x) => x.includes('soc'));
    const predits = table.lignes.map((l) => [l[0], l[k]]);
    verifier('predictionsCommeLeMoteur', k > 0 && predits.every(([nom, v]) => v === fmt(o[cle][o.nom.indexOf(nom)], 4)), { colonne: table.tetes[k], predits: predits.slice(0, 3) });
    const hors = o.nom.filter((_, i) => o.hors_domaine[i]);
    const signales = table.lignes.filter((l) => l.includes('⚠')).map((l) => l[0]);
    const n020 = o[cle][o.nom.indexOf('N020')];
    verifier('horsDomaine', hors.join(',') === 'N020' && signales.join(',') === 'N020' && Math.round(n020) === 45, { hors, signales, N020: n020 });

    // ---- Model › Text for the Methods section: the sentences the tutorial quotes, and its Results line
    await n.menu('m_modele', 'texte_methodes');
    await n.attendre("__n.titreDialogue().startsWith(__n.t('texte_methodes_titre').split('{nom}')[0]) && !!__n.boite().querySelector('.texte-methodes p')");
    const texte = await n.evaluer("__n.norm(__n.boite().querySelector('.texte-methodes').textContent)");
    const citations = [
      'Each spectrum (the 141 quantitative X variables, in table order) was first corrected by SNV (Barnes et al., 1989) and detrending (degree-2 polynomial removed; Barnes et al., 1989). The X variables were mean-centred only.',
      'We fitted a PLS regression (partial least squares; Wold et al., 2001) with the NIPALS algorithm, with 3 components.',
      'Cross-validation used 7 groups (observation i in group i modulo 7); each group was predicted by a model fitted without it, with centring recomputed on the other observations and the scaling of the full model.',
      'The most important variables (VIP > 1): 2320 (3.5), 2330 (3.4), 2310 (3.3), 2340 (3.2), 2300 (2.8), 2350 (2.6), 2290 (2.1), 1720 (2.1)',
    ];
    const absentes = citations.filter((x) => !texte.includes(x));
    verifier('texteMethodes', absentes.length === 0, absentes.length ? { absentes, texte: texte.slice(0, 2000) } : `${texte.length} characters`);
    await n.touche('Escape');
    await n.attendre('!__n.dialogue()');
  },
};
