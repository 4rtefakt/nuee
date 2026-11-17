// The data workbench from a column's header, Redo, and Stop: in the example's table as a CSV file, a
// PLS fitted, then by a right-click on a header of the Data view a variable the PLS uses renamed (its
// definition follows, and the PLS is fitted again under the new name), a code in numbers made with
// New variable treated as categories, then deleted, and a variable the PLS uses refused deletion;
// Edit › Undo twice and Redo (Ctrl+Y) named and made; then 999 permutations of the PLS stopped from
// the status bar: Stop appears, the view says the computation stopped, nothing changed. The engine's
// answers are checked after each step. With `captures`, a screenshot of each new thing.
module.exports = {
  async executer(n, verifier, { donnee, captures }) {
    const capture = async (nom) => { if (captures) await n.capture(`${captures}-colonnes-${nom}.png`); };
    const variable = (p, nom) => p.variables.find((v) => v.nom === nom);
    const projet = async () => (await n.etat()).projets.find((x) => x.nom === 'orchards.csv');

    // ---- the CSV, its Data view, a PLS of the taste on every other variable
    await n.fichierAOuvrir(donnee('exemple', 'orchards.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('orchards.csv'));
    await n.attendre("__n.tous('.dv-content-container .tableau thead th').length > 5");
    const pls = await n.nouveauModele({ type: 'PLS', y: ['taste_score'] });
    verifier('plsAjuste', pls && [].concat(pls.variables.terme).includes('sugar'), pls && [].concat(pls.variables.terme).slice(0, 6));
    // (the Data view in front again: its tab)
    await n.cliquer(`__n.onglet(${JSON.stringify(`orchards.csv · ${await n.t('donnees')}`)})`);
    const entete = (nom) => `__n.tous('.dv-content-container .tableau thead th').find((c) => __n.norm(c.textContent).startsWith(${JSON.stringify(nom)}))`;
    // a right-click on a header (the menu of the Data view's columns)
    const clicDroit = async (nom) => {
      await n.fermerMenus();
      const c = await n.centre(entete(nom));
      await n.evaluer(`(() => { const e = ${entete(nom)}; e.dispatchEvent(new MouseEvent('contextmenu', { bubbles: true, cancelable: true, clientX: ${c.x}, clientY: ${c.y} })); return true; })()`);
      await n.attendre("__n.tous('.menu.contextuel .item').length > 0");
    };
    // (a dialog whose title names the variable, "Rename “sugar” — orchards.csv")
    const dialogueDe = (cle, nom) => n.attendre(`__n.titreDialogue().startsWith(__n.t(${JSON.stringify(cle)}, { nom: __n.langueActuelle() === 'fr' ? '« ${nom} »' : '“${nom}”' }))`);
    const item = (cle, nom) => `(() => { const x = __n.t(${JSON.stringify(cle)}, { nom: __n.langueActuelle() === 'fr' ? '« ${nom} »' : '“${nom}”' });
      return __n.tous('.menu.contextuel .item').find((e) => __n.norm(e.querySelector('.libelle').textContent) === __n.norm(x)); })()`;

    // ---- Rename…: the preview says what follows; the PLS fitted again under the new name
    await clicDroit('sugar');
    verifier('menuColonne', !!(await n.attendre(`!!${item('atelier_renommer_de', 'sugar')} && !!${item('atelier_en_categories', 'sugar')} && !!${item('atelier_supprimer_de', 'sugar')}`)));
    await capture('1-menu');
    await n.cliquer(item('atelier_renommer_de', 'sugar'));
    await dialogueDe('atelier_renommer_titre', 'sugar');
    await n.ecrire("__n.boite().querySelector('input.nouveau-nom')", 'sugar (°Brix)', { quitter: false });
    const apercu = await n.attendre(`(() => { const p = __n.boite().querySelector('.apercu-remplacer .resume-remplacer'); return p && /Brix/.test(p.textContent) ? __n.norm(__n.boite().querySelector('.apercu-remplacer').textContent) : null; })()`);
    const refaire = await n.attendre("(() => { const c = __n.boite().querySelector('.grille-champs input[type=checkbox]'); return c ? c.checked : null; })()");
    verifier('apercuRenommer', /M1|PLS/.test(apercu) && refaire === true, apercu);
    await capture('2-renommer');
    await n.bouton('atelier_renommer');
    await n.attendre('!__n.dialogue()');
    await n.libre();
    let p = await projet();
    const refait = await n.modele(pls.nom, p.id);
    verifier('renommee', !!variable(p, 'sugar (°Brix)') && !variable(p, 'sugar') && p.annulable === 'sugar renamed sugar (°Brix)'.replace('renamed', (await n.evaluer('__n.langueActuelle()')) === 'fr' ? 'renommée' : 'renamed'),
      p.annulable);
    verifier('plsAjusteSousLeNouveauNom', refait && refait.id !== pls.id && [].concat(refait.variables.terme).includes('sugar (°Brix)'), refait && refait.id);
    verifier('enteteRenommee', !!(await n.attendre(`!!${entete('sugar (°Brix)')}`)));

    // ---- a code in numbers (New variable), treated as categories, then deleted
    await n.menu('m_edition', 'nouvelle_variable');
    await n.dialogue('nouvelle_variable');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'lot');
    await n.cliquer("__n.boite().querySelector('textarea.formule')");
    await n.taper('c(3, 1, 2)[(seq_along(weight) - 1) %% 3 + 1]');  // (rep() isn't allowed in a formula)
    await n.bouton('ajouter');
    await n.attendre('!__n.dialogue()');
    await n.attendre(`!!${entete('lot')}`);
    await clicDroit('lot');
    await n.cliquer(item('atelier_en_categories', 'lot'));
    await n.attendre(`__n.api('projet', { projet: ${JSON.stringify(p.id)} }).then((x) => x.variables.some((v) => v.nom === 'lot' && v.qualitative))`);
    await n.libre();
    p = await projet();
    verifier('enCategories', [].concat(variable(p, 'lot').categories).join() === '1,2,3' && /lot/.test(p.annulable), [].concat(variable(p, 'lot').categories));
    await clicDroit('lot');
    await n.cliquer(item('atelier_supprimer_de', 'lot'));
    await dialogueDe('atelier_supprimer_titre', 'lot');
    await n.bouton('atelier_supprimer');
    await n.attendre('!__n.dialogue()');
    await n.attendre(`__n.api('projet', { projet: ${JSON.stringify(p.id)} }).then((x) => !x.variables.some((v) => v.nom === 'lot'))`);
    await n.libre();
    // a variable the PLS uses: refused, saying why
    await clicDroit('acidity');
    await n.cliquer(item('atelier_supprimer_de', 'acidity'));
    await dialogueDe('atelier_supprimer_titre', 'acidity');
    const refus = await n.evaluer("__n.norm(__n.boite().querySelector('.corps-dialogue').textContent)");
    verifier('suppressionRefusee', /M1/.test(refus) && /acidity/.test(refus), refus);
    await capture('3-refus');
    await n.bouton('fermer');
    await n.attendre('!__n.dialogue()');

    // ---- Edit › Undo twice (the deletion, the categories), then Redo by Ctrl+Y
    const annuler = async () => {
      const avant = (await projet()).revision;
      await n.menu('m_edition', 'annuler_action');
      await n.attendre(`__n.api('projet', { projet: ${JSON.stringify(p.id)} }).then((x) => x.revision !== ${JSON.stringify(avant)})`);
      await n.libre();
      return projet();
    };
    p = await annuler();
    verifier('annuleSuppression', variable(p, 'lot') && variable(p, 'lot').qualitative, p.variables.map((v) => v.nom).slice(-2));
    p = await annuler();
    verifier('annuleCategories', variable(p, 'lot') && !variable(p, 'lot').qualitative && /lot/.test(p.retablissable || ''), p.retablissable);
    // (Edit › Redo names what it redoes)
    await n.fermerMenus();
    await n.cliquer(`__n.texte('.menu-haut', __n.t('m_edition'))`);
    const libelle = await n.attendre("(() => { const e = __n.tous('.menu .item .libelle').find((x) => __n.norm(x.textContent).startsWith(__n.t('retablir_action'))); return e ? __n.norm(e.textContent) : null; })()");
    verifier('retablirNomme', /lot/.test(libelle), libelle);
    await capture('4-retablir');
    await n.fermerMenus();
    const avantY = (await projet()).revision;
    await n.evaluer("(document.activeElement && document.activeElement.blur && document.activeElement.blur(), document.dispatchEvent(new KeyboardEvent('keydown', { key: 'y', code: 'KeyY', ctrlKey: true, bubbles: true, cancelable: true })), true)");
    await n.attendre(`__n.api('projet', { projet: ${JSON.stringify(p.id)} }).then((x) => x.revision !== ${JSON.stringify(avantY)})`);
    await n.libre();
    p = await projet();
    verifier('retabliCtrlY', variable(p, 'lot') && variable(p, 'lot').qualitative && /lot/.test(p.annulable), p.annulable);

    // ---- Stop: 999 permutations of the PLS, stopped from the status bar
    const modeles = (await n.etat()).modeles.length;
    await n.menu('m_graphiques', 'permutations');
    await n.attendre("__n.tous('input[type=number]').some((i) => i.max === '999')", 120000);
    await n.libre();
    await n.ecrire("__n.tous('input[type=number]').find((i) => i.max === '999')", '999');
    await n.attendre("(() => { const a = document.getElementById('arret-calcul'); return !!a && !a.hidden; })()", 30000);
    const statutCalcul = await n.evaluer('__n.statut()');
    verifier('arretVisible', statutCalcul === (await n.t('calcul')), statutCalcul);
    await capture('5-arreter');
    await n.cliquer("document.querySelector('#arret-calcul button')");
    await n.attendre(`__n.tous('.dv-content-container p.infos').some((e) => __n.norm(e.textContent) === __n.t('calcul_arrete'))`, 30000);
    await n.attendre("(() => { const a = document.getElementById('arret-calcul'); return !a || a.hidden; })()");
    const apres = await n.etat();
    verifier('arreteRienNaChange', apres.modeles.length === modeles, apres.modeles.length);
    verifier('apresArretLeMoteurRepond', (await n.api('permutations', { modele: refait.id, n: 3 })).points.R2.length === 4);
    // (the stopped request answered 400 with `arrete`, and the deletion refused by design: not
    // refusals of the engine for the run's check)
    await n.evaluer("(window.__essai.refus = window.__essai.refus.filter((r) => !/^(permutations|supprimer_variable) /.test(r)), true)");
  },
};
