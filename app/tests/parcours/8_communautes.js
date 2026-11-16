// Community ecology, as the communities tutorial makes it: the ground beetles of three habitats
// (docs/site/donnees, made up) opened; Model › Which analysis? › Many species at once proposes the
// 26 species and habitat, and recommends the PERMANOVA, with the NMDS and the indicator species
// beside it. Plots › Rarefaction, the species' range taken, by habitat: each trap's richness at 15
// individuals (the smallest catch), its Chao1 and ACE, as the engine answers the same request and
// as the tutorial quotes them. The NMDS opened from Which analysis?, square root then Wisconsin,
// fitted: its Summary's stress and starts, habitat fitted onto it. The PERMANOVA the same way: its
// Summary's R2, pseudo-F and p, its dispersion test. The indicator species: its view's table the
// engine's, the species the tutorial names. Then, the PERMANOVA chosen in the explorer, Model ›
// Text for the Methods section: the sentences the tutorial quotes.
const num = (s) => (s === '' || s === '—' ? null : Number(String(s).replace(/−/g, '-')));
const arrondi = (v, d) => (v === null || v === undefined ? null : Number(Number(v).toFixed(d)));

module.exports = {
  async executer(n, verifier, { donnee }) {
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'ground_beetles.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('ground_beetles.csv'));
    const projet = (await n.etat()).projets[0];
    // (36 traps; the first column names them, the 27 variables: habitat and the 26 species)
    verifier('donnees', projet.observations.uid.length === 36 && projet.variables.length === 27,
      `${projet.observations.uid.length} obs. × ${projet.variables.length} variables`);
    const especes = projet.variables.filter((v) => !v.qualitative);
    const habitat = projet.variables.find((v) => v.nom === 'habitat');

    // ---- Model › Which analysis? › Many species at once: the species and habitat proposed
    const quelleAnalyse = async () => {
      await n.menu('m_modele', 'quelle_analyse');
      await n.cliquer(`document.querySelector('[data-cle="qa-question:especes"]')`);
      return n.attendre(`(() => { const cases = (c) => [...(__n.boite().querySelector('.qa-cases[data-defile="' + c + '"]') || { querySelectorAll: () => [] }).querySelectorAll('label')]
          .filter((l) => l.querySelector('input').checked).map((l) => __n.norm(l.childNodes[1].textContent));
        const carte = __n.boite().querySelector('.qa-carte h4');
        if (!carte) return null;
        return { y: cases('qa-especes'), x: cases('qa-x'), recommandee: __n.norm(carte.textContent),
          autres: [...__n.boite().querySelectorAll('.qa-autres .qa-nom')].map((e) => __n.norm(e.textContent)) }; })()`);
    };
    const qa = await quelleAnalyse();
    const noms = await n.evaluer("({ permanova: __n.t('qa_nom_permanova'), nmds: __n.t('qa_nom_nmds'), indval: __n.t('qa_nom_indval') })");
    verifier('especesProposees', qa.y.length === 26 && qa.y.join('|') === especes.map((v) => v.nom).join('|') && qa.x.join('|') === 'habitat',
      { y: qa.y.length, x: qa.x });
    verifier('permanovaRecommandee', qa.recommandee === noms.permanova && qa.autres.includes(noms.nmds) && qa.autres.includes(noms.indval), qa);
    // (closed for now: richness comes first)
    await n.touche('Escape');
    await n.attendre('!__n.dialogue()');

    // ---- Plots › Rarefaction: the species from Abax parallelepipedus to Trechus quadristriatus, by habitat
    await n.menu('m_graphiques', 'rarefaction');
    const plage = "__n.tous('.outils label.champ').find((l) => __n.norm((l.querySelector('span') || {}).textContent) === __n.t('rar_de'))";
    await n.choisirTexte(`${plage}.querySelectorAll('select')[0]`, 'Abax parallelepipedus');
    await n.choisirTexte(`${plage}.querySelectorAll('select')[1]`, 'Trechus quadristriatus');
    await n.cliquer(`${plage}.querySelector('button')`);
    const depuis = await n.nombreRequetes();
    await n.choisirTexte("__n.tous('.outils label.champ').find((l) => __n.norm((l.querySelector('span') || {}).textContent) === __n.t('rar_groupes')).querySelector('select')", 'habitat');
    const demande = await n.attendre(`window.__essai.requetes.slice(${depuis}).find((r) => r.nom === 'rarefaction' && r.statut === 200 && /"groupe"/.test(r.corps || '')) || null`);
    const table = await n.attendre(`(() => { const t = __n.tous('table.tableau').find((x) => [...x.tHead.rows[0].cells].some((c) => __n.norm(c.textContent) === __n.t('rar_s_rarefiee', { m: 15 })));
      if (!t || t.tBodies[0].rows.length < 36 || __n.norm(t.tHead.rows[0].cells[1].textContent) !== 'habitat') return null;
      return [...t.tBodies[0].rows].map((l) => [...l.cells].map((c) => __n.norm(c.textContent))); })()`);
    const q = JSON.parse(demande.corps);
    const r = await n.api('rarefaction', q);
    const obs = [].concat(r.observations.uid).map((_, i) => Object.fromEntries(Object.keys(r.observations).map((k) => [k, [].concat(r.observations[k])[i]])));
    // the table: each trap's catch, richness at 15, Chao1 and ACE, the engine's (to two decimals)
    const colonnes = ['N', 'S', 'Sm', 'se', 'chao1', 'se_chao1', 'ace', 'se_ace'];
    const ecarts = table.filter((l) => {
      const o = obs.find((x) => x.nom === l[0]);
      return !o || l[1] !== o.groupe || colonnes.some((k, j) => num(l[j + 2]) !== arrondi(o[k], 2));
    }).map((l) => l[0]);
    verifier('rarefactionCommeLeMoteur', q.especes.length === 26 && String(q.groupe) === String(habitat.uid) && table.length === 36 && ecarts.length === 0,
      ecarts.length ? ecarts : `${table.length} traps, ${q.especes.length} species, at ${r.taille}`);
    // the tutorial: the dashed line at 15, P14's catch; at 15, hedgerows 8.02 to 10.57 species,
    // woodland 3.21 to 8.92, fields 5.60 to 9.13 (29 to 110 beetles); P06: Chao1 16 ± 10.07, no ACE
    const etendue = (g, k, d) => { const v = obs.filter((o) => o.groupe === g).map((o) => o[k]); return `${Math.min(...v).toFixed(d)}–${Math.max(...v).toFixed(d)}`; };
    const p06 = obs.find((o) => o.nom === 'P06');
    const p14 = obs.find((o) => o.nom === 'P14');
    const courbes = `${projet.nom} · ${await n.t('rar_mode_rarefaction')}`;
    await n.plotlyPret(courbes, 36);
    const tirets = await n.evaluer(`[].concat(__n.graphe(${JSON.stringify(courbes)}).layout.shapes || []).map((s) => s.x0)`);
    const tutoriel = { taille: r.taille === 15 && p14.N === 15 && tirets.includes(15), hedgerow: etendue('hedgerow', 'Sm', 2) === '8.02–10.57',
      woodland: etendue('woodland', 'Sm', 2) === '3.21–8.92', field: etendue('field', 'Sm', 2) === '5.60–9.13', prises: etendue('field', 'N', 0) === '29–110',
      p06: p06.N === 34 && arrondi(p06.chao1, 2) === 16 && arrondi(p06.se_chao1, 2) === 10.07 && (p06.ace === null || p06.ace === undefined) };
    verifier('rarefactionCommeLeTutoriel', Object.values(tutoriel).every(Boolean),
      { ...tutoriel, valeurs: [r.taille, tirets, etendue('hedgerow', 'Sm', 2), etendue('woodland', 'Sm', 2), etendue('field', 'Sm', 2), etendue('field', 'N', 0), p06] });

    // ---- a model opened from Which analysis? (the NMDS, then the PERMANOVA), square root then
    // Wisconsin on its General tab, fitted
    const ajuster = async (nom, recommandee, transformation = true) => {
      await quelleAnalyse();
      if (recommandee) await n.cliquer("__n.boite().querySelector('.qa-carte .qa-ouvrir button')");
      else await n.cliquer(`[...__n.boite().querySelectorAll('.qa-autres summary')].find((s) => __n.norm(s.querySelector('.qa-nom').textContent) === ${JSON.stringify(nom)}).querySelector('button')`);
      await n.dialogue('nouveau_modele');
      if (transformation) {
        await n.cliquer("__n.tous('.onglets button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('general'))");
        await n.choisir("__n.tous('select', __n.boite()).find((s) => [...s.options].some((o) => o.value === 'racine_wisconsin'))", 'racine_wisconsin');
      }
      const avant = (await n.etat()).modeles.map((m) => m.id);
      await n.bouton('ajuster');
      await n.attendre('!__n.dialogue()', 180000);
      const id = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles).find((x) => !${JSON.stringify(avant)}.includes(x.id)); return m ? m.id : null; })`);
      const m = await n.api('modele', { modele: id });
      await n.attendre(n.ligneModele(m.nom, true));
      return m;
    };
    // its Summary's figures, as they are shown, once its fit is there
    const resume = (nom) => n.attendre(`(() => { const r = __n.tous('.resume').find((x) => __n.norm((x.querySelector('.fiche-titre b') || {}).textContent) === ${JSON.stringify(nom)});
      if (!r) return null;
      return { chiffres: Object.fromEntries([...r.querySelectorAll('.fiche-chiffres > span')].map((s) => [__n.norm((s.querySelector('small') || {}).textContent), __n.norm((s.querySelector('b') || {}).textContent)])),
        notes: [...r.querySelectorAll('p.note')].map((p) => __n.norm(p.textContent)) }; })()`);

    // ---- the NMDS: stress 0.1347, found again by another start; habitat fitted, R² 0.8062, p 0.001
    const nmds = await ajuster(noms.nmds, false);
    const vn = await resume(nmds.nom);
    const cles = await n.evaluer("({ departs: __n.t('com_departs'), retrouvee: __n.t('com_retrouvee_mot'), f: __n.t('ord_pseudo_f') })");
    verifier('nmdsAjuste', nmds.type === 'NMDS' && nmds.N === 36 && nmds.K === 26 && nmds.reglages && JSON.stringify(nmds.reglages).includes('racine_wisconsin'),
      `${nmds.nom} ${nmds.type} N=${nmds.N} K=${nmds.K}`);
    verifier('resumeNmds', vn.chiffres.stress === Number(nmds.stress).toFixed(4) && vn.chiffres.k === String(nmds.k) && vn.chiffres[cles.departs] === String(nmds.departs)
      && vn.chiffres[cles.retrouvee] === String(nmds.retrouvee), vn.chiffres);
    const fh = nmds.ajustement && nmds.ajustement.facteurs;
    const r2h = fh ? [].concat(fh.r2)[[].concat(fh.variable).indexOf('habitat')] : null;
    const ph = fh ? [].concat(fh.p)[[].concat(fh.variable).indexOf('habitat')] : null;
    // (Apple silicon's arithmetic ends this NMDS a little elsewhere: 0.1346 and 0.8060, the same to
    // three decimals)
    verifier('nmdsCommeLeTutoriel', Math.abs(nmds.stress - 0.1347) < 5e-4 && nmds.k === 2 && nmds.departs === 20 && nmds.retrouvee === 1
      && Math.abs(r2h - 0.8062) < 5e-4 && arrondi(ph, 4) === 0.001 && (1 - nmds.stress ** 2).toFixed(3) === '0.982',
      { stress: nmds.stress, k: nmds.k, departs: nmds.departs, retrouvee: nmds.retrouvee, habitat: [r2h, ph] });

    // ---- the PERMANOVA: R² 0.512, pseudo-F 17.344, p 0.001; the dispersions don't differ (F 1.862, p 0.198)
    const perm = await ajuster(noms.permanova, true);
    const vp = await resume(perm.nom);
    const mo = perm.modele;
    const d = [].concat(perm.dispersions)[0] || {};
    verifier('permanovaAjustee', perm.type === 'PERMANOVA' && perm.N === 36 && JSON.stringify(perm.reglages || {}).includes('racine_wisconsin'), `${perm.nom} ${perm.type} N=${perm.N}`);
    verifier('resumePermanova', num(vp.chiffres.R2) === arrondi(mo.R2, 3) && vp.chiffres[cles.f] === Number(mo.F).toFixed(3) && Math.abs(num(vp.chiffres.p) - mo.p) < 1e-9
      && vp.notes.some((x) => x.includes(Number(d.F).toFixed(3))), vp);
    const moyennes = Object.fromEntries([].concat(d.groupes.groupe).map((g, i) => [g, Number([].concat(d.groupes.moyenne)[i]).toFixed(4)]));
    verifier('permanovaCommeLeTutoriel', Number(mo.R2).toFixed(3) === '0.512' && Number(mo.F).toFixed(3) === '17.344' && Number(mo.p).toFixed(4) === '0.0010'
      && d.variable === 'habitat' && Number(d.F).toFixed(3) === '1.862' && Number(d.p_permutation).toFixed(4) === '0.1980'
      && moyennes.field === '0.2939' && moyennes.hedgerow === '0.3473' && moyennes.woodland === '0.3410',
      { R2: mo.R2, F: mo.F, p: mo.p, dispersion: [d.variable, d.F, d.p_permutation], moyennes });

    // ---- the indicator species: the Indicator species view's table the engine's; Abax parallelus
    // woodland's (A 0.833, B 0.917), Leistus spinibarbis the hedgerows' (A 1, B 0.583), Pterostichus
    // melanarius field+hedgerow's (A 0.971, B 0.917), Loricera pilicornis everywhere, untested; 25
    // species with a corrected p ≤ 0.05
    const iv = await ajuster(noms.indval, false, false);
    const lignesIv = [].concat(iv.especes.espece).map((e, i) => Object.fromEntries(Object.keys(iv.especes).map((k) => [k, [].concat(iv.especes[k])[i]])));
    const tableIv = await n.attendre(`(() => { const t = __n.tous('table.tableau').find((x) => __n.norm(x.tHead.rows[0].cells[0].textContent) === __n.t('iv_espece'));
      if (!t || t.tBodies[0].rows.length < 26) return null;
      return [...t.tBodies[0].rows].map((l) => [...l.cells].map((c) => __n.norm(c.textContent))); })()`);
    const ecartsIv = tableIv.filter((l) => {
      const o = lignesIv.find((x) => x.espece === l[0]);
      return !o || ['A', 'B', 'indval'].some((k, j) => num(l[j + 2]) !== arrondi(o[k], 3));
    }).map((l) => l[0]);
    verifier('indicatricesCommeLeMoteur', iv.type === 'INDVAL' && tableIv.length === 26 && ecartsIv.length === 0, ecartsIv.length ? { ecartsIv, tableIv: tableIv.slice(0, 3) } : `${tableIv.length} species`);
    const sp = (nom) => lignesIv.find((x) => x.espece === nom) || {};
    const ab = (nom) => `${sp(nom).groupe} ${arrondi(sp(nom).A, 3)} ${arrondi(sp(nom).B, 3)}`;
    const significatives = lignesIv.filter((x) => x.p_ajuste !== null && x.p_ajuste <= 0.05).length;
    verifier('indicatricesCommeLeTutoriel', ab('Abax parallelus') === 'woodland 0.833 0.917' && ab('Leistus spinibarbis') === 'hedgerow 1 0.583'
      && ab('Pterostichus melanarius') === 'field+hedgerow 0.971 0.917' && sp('Abax parallelepipedus').groupe === 'hedgerow+woodland' && sp('Harpalus affinis').groupe === 'field'
      && (sp('Loricera pilicornis').p === null || sp('Loricera pilicornis').p === undefined) && significatives === 25,
      { especes: ['Abax parallelus', 'Leistus spinibarbis', 'Pterostichus melanarius', 'Abax parallelepipedus', 'Harpalus affinis', 'Loricera pilicornis'].map((x) => [x, sp(x).groupe, sp(x).A, sp(x).B, sp(x).p]), significatives });

    // ---- Model › Text for the Methods section (the PERMANOVA, chosen in the explorer): the sentences the tutorial quotes
    await n.cliquer(n.ligneModele(perm.nom, true));
    await n.menu('m_modele', 'texte_methodes');
    await n.attendre(`__n.titreDialogue() === __n.t('texte_methodes_titre', { nom: ${JSON.stringify(perm.nom)} }) && !!__n.boite().querySelector('.texte-methodes p')`);
    const texte = await n.evaluer("__n.norm(__n.boite().querySelector('.texte-methodes').textContent)");
    const citations = [
      'The values were first square-root transformed then given Wisconsin double standardization (each variable divided by its maximum, then each observation by its total). The distance between observations is the Bray–Curtis dissimilarity (Bray & Curtis, 1957).',
      'habitat: R² = 0.512, pseudo-F = 17.34, p = 0.001.',
      'habitat: F = 1.86, p = 0.198 by permutation.',
    ];
    const absentes = citations.filter((x) => !texte.includes(x));
    verifier('texteMethodes', absentes.length === 0, absentes.length ? { absentes, texte: texte.slice(0, 2500) } : `${texte.length} characters`);
    await n.touche('Escape');
    await n.attendre('!__n.dialogue()');
  },
};
