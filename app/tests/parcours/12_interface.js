// The window around the models, on the example and then a second project: the Data view's
// decimals as in the data; Format plot › Spread the labels on the PCA's loadings; New model's
// types by family; the toolbar and the Plots menu of a regression (a PCA's toolbar as it was);
// Find a command's analyses; the explorer following the work; the two projects' M1 told apart in
// the tabs, the files' names and Compare models; tabs closed in bulk; the figure composer offering
// the plots of models none of whose views is open.
const path = require('path');

module.exports = {
  async executer(n, verifier, { donnee, dossier }) {
    await n.menu('m_aide', 'projet_exemple');
    await n.attendre(n.ligneModele('M1', true));
    await n.libre();

    // ---- the Data view: each column's decimals as in its values (weight 133, diameter 62.1, firmness 6.63)
    await n.menu('m_graphiques', 'donnees');
    const cellules = await n.attendre(`(() => { const t = __n.tous('table.tableau').find((x) => [...x.tHead.rows[0].cells].some((c) => __n.norm(c.childNodes[0] && c.childNodes[0].textContent) === 'firmness'));
      if (!t || !t.tBodies[0].rows.length) return null;
      const k = (nom) => [...t.tHead.rows[0].cells].findIndex((c) => __n.norm(c.childNodes[0] && c.childNodes[0].textContent) === nom);
      const l = t.tBodies[0].rows[0];
      return ['weight', 'diameter', 'firmness'].map((nom) => __n.norm(l.cells[k(nom)].textContent)); })()`);
    verifier('donneesDecimales', JSON.stringify(cellules) === JSON.stringify(['133', '62.1', '6.63']), cellules);

    // ---- Format plot › Spread the labels: the PCA's loadings, weight and diameter apart, no label on another
    const vueLoadings = `M1 · ${await n.t('loadings')}`;
    const repousser = async (oui) => {
      await n.cliquer(`__n.graphe(${JSON.stringify(vueLoadings)})`);
      await n.menu('m_edition', 'mise_en_forme');
      await n.dialogue('mise_en_forme');
      await n.coche("__n.tous('label', __n.boite()).find((l) => __n.norm(l.textContent) === __n.t('repousser_etiquettes')).nextElementSibling", oui);
      await n.bouton('appliquer');
      await n.attendre('!__n.dialogue()');
    };
    await n.cliquer("__n.onglet('M1 · PCA')");
    await n.plotlyPret(vueLoadings, 14);
    await repousser(true);
    const etiquettes = await n.attendre(`(() => { const g = __n.graphe(${JSON.stringify(vueLoadings)}); if (!g || !g.layout.annotations) return null;
      const noms = g.layout.annotations.filter((a) => String(a.name || '').startsWith('etiquette_repoussee')).map((a) => a.text);
      const boites = [...g.querySelectorAll('.annotation-text')].map((t) => ({ texte: t.textContent, r: t.getBoundingClientRect() })).filter((b) => noms.includes(b.texte));
      if (noms.length < 14 || boites.length !== noms.length) return null;
      const chevauchent = [];
      for (let i = 0; i < boites.length; i += 1) for (let j = i + 1; j < boites.length; j += 1) {
        const a = boites[i].r, b = boites[j].r;
        if (Math.min(a.right, b.right) - Math.max(a.left, b.left) > 0.5 && Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top) > 0.5) chevauchent.push(boites[i].texte + '/' + boites[j].texte);
      }
      return { n: noms.length, chevauchent, textes: g.data.some((tr) => String(tr.mode).includes('text')) }; })()`);
    verifier('etiquettesEcartees', etiquettes.n >= 14 && etiquettes.chevauchent.length === 0 && !etiquettes.textes, etiquettes);
    await repousser(false);
    await n.attendre(`(() => { const g = __n.graphe(${JSON.stringify(vueLoadings)}); return g && !(g.layout.annotations || []).some((a) => String(a.name || '').startsWith('etiquette_repoussee'))
      && g.data.some((tr) => String(tr.mode).includes('text')); })()`);

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

    // ---- the figure composer offers every plot of the project's fitted models, none of them open now
    await n.cliquer(n.ligneProjet('orchards.json'));
    await n.menu('m_graphiques', 'figure_composer');
    const ajout = "document.querySelector('select.ajout-figure')";
    await n.attendre(`${ajout} && !${ajout}.disabled`);
    await n.evaluer(`${ajout}.dispatchEvent(new Event('focus')), true`);  // (its list is made when it is opened)
    const groupes = await n.attendre(`(() => { const g = [...${ajout}.querySelectorAll('optgroup')].map((x) => ({ libelle: x.label, vues: [...x.querySelectorAll('option')].map((o) => o.value) }));
      return g.length ? g : null; })()`);
    const m1 = groupes.find((g) => g.libelle === 'orchards · M1 · PCA');
    const chargements = m1 && m1.vues.find((v) => /^m:[^:]+:loadings$/.test(v));
    verifier('figureToutesLesVues', !!chargements && groupes.some((g) => g.libelle === 'orchards · M5 · LM' && g.vues.some((v) => v.endsWith(':effets'))),
      groupes.map((g) => `${g.libelle}: ${g.vues.length}`));
    await n.choisir(ajout, chargements);
    // (its preview drawn again with the panel: the summary under it counts one)
    const apercu = await n.attendre(`(() => { const f = document.querySelector('.feuille-figure'); const i = f && f.querySelector('img');
      const resume = __n.norm((document.querySelector('.resume-figure') || {}).textContent);
      return f && !f.classList.contains('attente') && i && i.src.startsWith('blob:') && / 1 /.test(resume) && document.querySelectorAll('.ligne-panneau').length === 1
        ? { resume, erreurs: [...document.querySelectorAll('.erreurs-figure .erreur')].map((e) => __n.norm(e.textContent)), panneau: __n.norm(document.querySelector('.ligne-panneau .nom-panneau').textContent) } : null; })()`);
    verifier('figureVueNonOuverte', apercu.erreurs.length === 0 && apercu.panneau.endsWith(await n.t('loadings')), apercu);
  },
};
