// The window around the models, with two projects open: New model's types by family; the
// toolbar and the Plots menu of a regression (a PCA's toolbar as it was); Find a command's
// analyses; two projects' M1 told apart in the tabs, the files' names and Compare models.
const path = require('path');

module.exports = {
  async executer(n, verifier, { donnee, dossier }) {
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

    // ---- Find a command knows the analyses by their names: "trim", then Enter, opens New model on TRIM
    await n.menu('m_affichage', 'chercher_commande');
    await n.attendre("!!document.querySelector('.palette-saisie')");
    await n.taper('trim');
    const premiere = await n.attendre("(() => { const x = document.querySelector('.palette-item'); return x ? __n.norm(x.querySelector('.palette-libelle').textContent) : null; })()");
    verifier('paletteTrim', premiere === await n.evaluer("__n.t('palette_nouveau_type', { type: __n.t('type_trim') })"), premiere);
    await n.touche('Enter');
    await n.dialogue('nouveau_modele');
    const type = await n.attendre("(() => { const s = __n.tous('select', __n.boite()).find((x) => [...x.options].some((o) => o.value === 'PCA')); return s && s.value; })()");
    verifier('paletteNouveauModele', type === 'TRIM', type);
    await n.bouton('annuler');
    await n.attendre('!__n.dialogue()');

    // ---- a second project with an M1 of its own: the tabs, the files and Compare models tell them apart
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'skylark_counts.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('skylark_counts.csv'));
    const tendance = await n.nouveauModele({ type: 'LM', y: ['skylarks'], sans: ['site', 'habitat'] });
    // the explorer follows the work: the example's fitted models folded, the new model's line in view
    const ids = Object.fromEntries((await n.etat()).projets.map((p) => [p.nom, p.id]));
    const arbre = await n.attendre(`(() => { const c = document.querySelector('.panneau.explorateur').getBoundingClientRect();
      const ligne = document.querySelector('[data-cle="${ids['skylark_counts.csv']}/m/${tendance.id}"]'); if (!ligne) return null;
      const r = ligne.getBoundingClientRect();
      const plies = [...document.querySelectorAll('.arbre .noeud.n3.calcule')].filter((l) => l.dataset.cle.startsWith('${ids['orchards.json']}/m/')).map((l) => l.getAttribute('aria-expanded'));
      return { visible: r.top >= c.top - 1 && r.bottom <= c.bottom + 1, plies }; })()`);
    verifier('explorateurSuitLeTravail', arbre.visible && arbre.plies.length === 2 && arbre.plies.every((x) => x === 'false'), arbre);
    const onglets = await n.attendre(`(() => { const o = __n.tous('.dv-tab').map((e) => __n.norm(e.textContent));
      return o.includes('orchards · M1 · PCA') && o.includes('skylark_counts · M1 · LM') && o.includes('orchards · M5 · LM') ? o : null; })()`);
    verifier('ongletsAvecProjet', tendance.nom === 'M1' && !!onglets, onglets);
    // File › Export model results proposes "skylark_counts - M1 LM.xlsx"
    await n.fichierAEnregistrer(path.join(dossier, 'resultats.xlsx'));
    await n.menu('m_fichier', 'exporter');
    await n.attendre("__n.titreDialogue().startsWith(__n.t('exporter_resultats_titre').split('{')[0])");
    await n.cliquer("__n.boite().querySelector('.formats-export input[value=\"xlsx\"]')");
    await n.bouton('choisir_emplacement');
    await n.attendre("__n.statut().startsWith(__n.t('exporte'))");
    const enregistrement = (await n.principal()).appels.filter((a) => a.dialogue === 'enregistrer').pop();
    verifier('nomDeFichierAvecProjet', !!enregistrement && enregistrement.nom === 'skylark_counts - M1 LM.xlsx', enregistrement && enregistrement.nom);
    // Predict › Compare models: one project's models
    await n.menu('m_prediction', 'comparaison');
    const filtre = "__n.tous('.outils label.champ').find((l) => __n.norm((l.querySelector('span') || {}).textContent) === __n.t('projets'))";
    await n.attendre(filtre);
    const skylarks = (await n.etat()).projets.find((p) => p.nom === 'skylark_counts.csv').id;
    await n.choisir(`${filtre}.querySelector('select')`, skylarks);
    const lignes = await n.attendre(`(() => { const t = ${filtre}.closest('.panneau').querySelector('table.tableau');
      const l = t ? [...t.tBodies[0].rows].map((r) => __n.norm(r.cells[0].textContent)) : []; return l.length === 1 ? l : null; })()`);
    verifier('comparerUnProjet', lignes[0] === 'M1', lignes);

    // ---- tabs closed in bulk: a tab's menu › Close all tabs of its project, then Window › Close all views
    const onglet = (titre) => `__n.onglet(${JSON.stringify(titre)})`;
    await n.evaluer(`(() => { const o = ${onglet('skylark_counts · M1 · LM')}; const r = o.getBoundingClientRect();
      o.dispatchEvent(new MouseEvent('contextmenu', { bubbles: true, cancelable: true, clientX: r.left + 10, clientY: r.top + 5 })); return true; })()`);
    const fermerProjet = await n.evaluer("__n.t('fermer_onglets_projet', { nom: 'skylark_counts.csv' })");
    await n.cliquer(`__n.tous('.menu .item').find((e) => __n.norm(e.textContent) === ${JSON.stringify(fermerProjet)})`);
    const restent = await n.attendre(`(() => { const o = __n.tous('.dv-tab').map((e) => __n.norm(e.textContent));
      return o.some((x) => x.startsWith('skylark_counts')) ? null : o; })()`);
    verifier('fermerOngletsDuProjet', restent.includes('orchards · M1 · PCA') && restent.includes(await n.t('comparaison')), restent);
    await n.menu('m_fenetre', 'fermer_vues');
    const accueil = await n.attendre("(() => { const o = __n.tous('.dv-tab').map((e) => __n.norm(e.textContent)); return document.querySelector('.accueil') ? o : null; })()");
    verifier('fermerToutesLesVues', accueil.length === 2 && accueil.includes(await n.t('explorateur')), accueil);
  },
};
