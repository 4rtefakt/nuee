// The window around the models, with two projects open: New model's types by family; the
// toolbar and the Plots menu of a regression (a PCA's toolbar as it was).
module.exports = {
  async executer(n, verifier, { donnee }) {
    await n.menu('m_aide', 'projet_exemple');
    await n.attendre(n.ligneModele('M1', true));
    await n.libre();

    // ---- New model's Type list: its families in their order, each type once, R code under Other
    await n.menu('m_modele', 'nouveau_modele');
    await n.dialogue('nouveau_modele');
    await n.cliquer("__n.tous('.onglets button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('general'))");
    const familles = await n.attendre(`(() => { const s = __n.tous('select', __n.boite()).find((x) => [...x.options].some((o) => o.value === 'PCA'));
      return s && [...s.querySelectorAll('optgroup')].map((g) => ({ libelle: g.label, types: [...g.querySelectorAll('option')].map((o) => o.value) })); })()`);
    const attendus = await n.evaluer("['multivariees', 'regressions', 'communautes', 'classement', 'suivi', 'autres'].map((k) => __n.t('types_' + k))");
    const tous = familles.flatMap((f) => f.types);
    verifier('typesParFamille', JSON.stringify(familles.map((f) => f.libelle)) === JSON.stringify(attendus)
      && familles[0].types.join() === 'PCA,PLS,PLS-DA,OPLS,OPLS-DA,CLASSES' && familles[1].types.join() === 'LM,LMM,GLM,GAM'
      && familles[4].types.join() === 'TRIM,DISTANCE' && familles[5].types.includes('CODE') && new Set(tous).size === tous.length && tous.length >= 26,
    familles.map((f) => `${f.libelle}: ${f.types.join(' ')}`));
    await n.bouton('annuler');
    await n.attendre('!__n.dialogue()');

    // ---- the toolbar: a PCA's, as it always was
    const barre = () => n.evaluer("[...document.querySelectorAll('#barre-outils .groupe-outils')].map((g) => [...g.querySelectorAll('.outil')].map((b) => b.dataset.commande).join(' ')).join(' | ')");
    const pca = 'ouvrir enregistrer | nouveau_modele modifier_modele autoajuster plus_comp moins_comp | resume scores loadings dmodx t2 contributions vip coefficients obspred dendrogramme'
      + ' | permutations classification predire comparaison exporter';
    verifier('barrePCA', (await barre()) === pca, await barre());

    // ---- a linear regression of the taste score: its own views in the toolbar and first in Plots
    const lm = await n.nouveauModele({ type: 'LM', y: ['taste_score'], sans: ['variety', 'farming', 'orchard'] });
    await n.attendre(`__n.tous('#barre-outils .outil').some((b) => b.dataset.commande === 'effets')`);
    const b = await barre();
    verifier('barreRegression', b.startsWith('ouvrir enregistrer | nouveau_modele modifier_modele | resume coefficients effets') && b.endsWith('| predire comparaison exporter')
      && !b.includes('scores') && !b.includes('autoajuster'), b);
    await n.cliquer(`__n.texte('.menu-haut', __n.t('m_graphiques'))`);
    const menu = await n.attendre(`(() => { const m = __n.tous('.menu:not(.sous-menu)')[0]; if (!m) return null;
      return [...m.querySelectorAll(':scope > .item')].map((e) => ({ libelle: __n.norm((e.querySelector('.libelle') || e).textContent), actif: !e.classList.contains('desactive'), sous: e.classList.contains('parent') })); })()`);
    const libelles = await n.evaluer("({ effets: __n.t('effets'), scores: __n.t('scores'), autres: __n.t('autres_graphiques'), resume: __n.t('resume') })");
    verifier('menuGraphiquesRegression', menu[0].libelle === libelles.resume && menu.some((x) => x.libelle === libelles.effets && x.actif)
      && !menu.some((x) => x.libelle === libelles.scores) && menu.some((x) => x.libelle === libelles.autres && x.sous), menu.map((x) => x.libelle));
    await n.fermerMenus();
    verifier('regressionAjustee', lm.type === 'LM', lm.nom);
  },
};
