// Import and data: the example's table as a CSV file and a made-up Excel workbook of two sheets
// opened with File › Open (the workbook asks which sheet); in the CSV, a variety renamed with
// Find and replace, two orchards merged with Categories, a computed variable added with New
// variable, then each change undone with Edit › Undo, which names it. The Data view and the
// engine's values are checked after each step.
const path = require('path');

module.exports = {
  async executer(n, verifier, { donnee, dossier }) {
    // ---- a CSV file: File › Open
    await n.fichierAOuvrir(donnee('exemple', 'orchards.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('orchards.csv'));
    const e1 = await n.etat();
    const csv = e1.projets.find((p) => p.nom === 'orchards.csv');
    const variable = (p, nom) => p.variables.find((v) => v.nom === nom);
    verifier('csvOuvert', csv && csv.observations.uid.length === 120 && csv.variables.length === 18 && csv.observations.nom[0] === 'P001',
      csv && `${csv.observations.uid.length} obs. × ${csv.variables.length} variables, first ${csv.observations.nom[0]}`);
    verifier('csvQualitatives', ['variety', 'farming', 'orchard'].every((v) => variable(csv, v) && variable(csv, v).qualitative)
      && !variable(csv, 'sugar').qualitative, csv.variables.filter((v) => v.qualitative).map((v) => v.nom));
    // the Data view: the first row as the file has it
    await n.double(n.ligne(await n.t('donnees'), 2));
    const premiere = await n.attendre(`(() => { const r = document.querySelector('.dv-content-container .tableau tbody tr');
      return r ? [...r.cells].map((c) => __n.norm(c.textContent)) : null; })()`);
    const lu = premiere.slice(0, 9).map((x) => (/^-?[\d.]+$/.test(x) ? Number(x) : x));
    verifier('vueDonnees', JSON.stringify(lu) === JSON.stringify(['P001', 'Gala', 'organic', 'A', 133, 62.1, 6.63, 13.9, 2.88]), premiere.join(' '));

    // ---- an Excel workbook of two sheets: the second one chosen
    const classeur = await n.ecrireClasseur(path.join(dossier, 'plots.xlsx'), [
      ['notes', [['Made-up data for a test of Nuée'], ['No real plot was measured.']]],
      ['plots', [['plot', 'treatment', 'yield', 'height'], ['p1', 'control', 4.2, 31], ['p2', 'mulch', 5.1, 35], ['p3', 'control', 3.9, null],
        ['p4', 'mulch', 5.6, 38], ['p5', 'control', 4.4, 30], ['p6', 'mulch', 5.3, 36]]]]);
    await n.fichierAOuvrir(classeur);
    await n.menu('m_fichier', 'ouvrir');
    await n.dialogue('choisir_table');
    const feuilles = await n.evaluer("[...__n.boite().querySelectorAll('.liste-tables option')].map((o) => o.value)");
    verifier('feuillesProposees', feuilles.join(',') === 'notes,plots', feuilles);
    await n.choisir("__n.boite().querySelector('.liste-tables')", 'plots');
    await n.bouton('ouvrir_table');
    await n.attendre("!__n.dialogue() && __n.tous('.arbre .noeud.n1').length === 2");
    await n.libre();
    const e2 = await n.etat();
    const xl = e2.projets.find((p) => p.id !== csv.id);
    const valeurs = async (p, nom) => [].concat((await n.api('valeurs', { projet: p.id, variable: variable(p, nom).uid, observations: p.observations.uid })).valeurs);
    verifier('excelOuvert', xl && xl.observations.uid.length === 6 && xl.variables.length === 3 && xl.observations.nom.join(',') === 'p1,p2,p3,p4,p5,p6',
      xl && `${xl.nom}: ${xl.observations.nom.join(',')} × ${xl.variables.map((v) => v.nom).join(',')}`);
    const rendement = await valeurs(xl, 'yield');
    const hauteur = await valeurs(xl, 'height');
    verifier('excelValeurs', variable(xl, 'treatment').qualitative && rendement.join(',') === '4.2,5.1,3.9,5.6,4.4,5.3' && hauteur[2] === null,
      `${rendement.join(',')} / ${hauteur.join(',')}`);

    // ---- back to the CSV's project: Edit › Find and replace, the whole cell "Granny"
    await n.cliquer(`(${n.ligneProjet('orchards.csv')}).querySelector('.libelle')`);
    await n.menu('m_edition', 'atelier_remplacer');
    await n.dialogue('atelier_remplacer');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'Granny', { quitter: false });
    await n.ecrire("__n.boite().querySelectorAll('.grille-champs > input')[0]", 'Granny Smith', { quitter: false });
    // the preview says what will change before anything does
    const apercu = await n.attendre("(() => { const p = __n.boite().querySelector('.apercu-remplacer .resume-remplacer'); return p ? __n.norm(p.textContent) : null; })()");
    verifier('apercuRemplacement', /\b30\b/.test(apercu), apercu);
    await n.bouton('atelier_bouton_remplacer');
    await n.attendre('!__n.dialogue()');
    const remplace = await n.attendre(`__n.statut() === __n.t('atelier_remplace', { n: 30 }) && __n.statut()`);
    verifier('remplace', !!remplace, remplace);
    let p = (await n.etat()).projets.find((x) => x.id === csv.id);
    verifier('varietes', [].concat(variable(p, 'variety').categories).join(',') === 'Gala,Golden,Granny Smith,Reinette', [].concat(variable(p, 'variety').categories));
    // (the Data view is drawn again from the new data)
    verifier('vueDonneesRemplacee', !!(await n.attendre("__n.tous('.dv-content-container .tableau tbody td').some((c) => __n.norm(c.textContent) === 'Granny Smith')")));

    // ---- Edit › Categories: orchards B and C ticked and merged
    await n.menu('m_edition', 'atelier_categories');
    await n.dialogue('atelier_categories');
    await n.choisirTexte("__n.boite().querySelector('.grille-champs select')", 'orchard');
    await n.attendre("__n.boite().querySelectorAll('.ligne-categorie').length === 3 && [...__n.boite().querySelectorAll('.nom-categorie')].map((i) => i.value).join() === 'A,B,C'");
    const comptes = await n.evaluer("[...__n.boite().querySelectorAll('.ligne-categorie .effectif')].map((e) => parseInt(e.textContent, 10))");
    for (const nom of ['B', 'C']) await n.coche(`[...__n.boite().querySelectorAll('.ligne-categorie')].find((l) => l.querySelector('.nom-categorie').value === '${nom}').querySelector('input[type=checkbox]')`);
    await n.cliquer(`__n.tous('button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.t('atelier_fusionner'))`);
    await n.attendre("__n.boite().querySelectorAll('.ligne-categorie').length === 2");
    const garde = comptes[1] >= comptes[2] ? 'B' : 'C';
    await n.bouton('appliquer');
    await n.attendre('!__n.dialogue()');
    await n.libre();
    p = (await n.etat()).projets.find((x) => x.id === csv.id);
    const vergers = await valeurs(p, 'orchard');
    verifier('categoriesFusionnees', [].concat(variable(p, 'orchard').categories).join(',') === `A,${garde}`
      && vergers.filter((x) => x === garde).length === comptes[1] + comptes[2], `${[].concat(variable(p, 'orchard').categories)} (${comptes})`);

    // ---- Edit › New variable: sugar / acidity
    await n.menu('m_edition', 'nouvelle_variable');
    await n.dialogue('nouvelle_variable');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'sugar_acidity');
    await n.cliquer("__n.boite().querySelector('textarea.formule')");
    await n.taper('sugar / acidity');
    await n.bouton('ajouter');
    await n.attendre('!__n.dialogue()');
    const ajoutee = await n.attendre(`__n.statut() === __n.t('variable_ajoutee', { nom: 'sugar_acidity' }) && __n.statut()`);
    verifier('variableAjoutee', !!ajoutee, ajoutee);
    p = (await n.etat()).projets.find((x) => x.id === csv.id);
    const rapport = variable(p, 'sugar_acidity') ? await valeurs(p, 'sugar_acidity') : [];
    const sucre = await valeurs(p, 'sugar');
    const acidite = await valeurs(p, 'acidity');
    verifier('variableCalculee', rapport.length === 120 && rapport.every((x, i) => Math.abs(x - sucre[i] / acidite[i]) < 1e-9), rapport.slice(0, 3));
    verifier('vueDonneesNouvelleColonne', !!(await n.attendre("__n.tous('.dv-content-container .tableau thead th').some((c) => __n.norm(c.textContent).startsWith('sugar_acidity'))")));

    // ---- Edit › Undo, which names what it undoes: the variable, then the merge, then the replacement
    const annuler = async () => {
      const avant = (await n.etat()).projets.find((x) => x.id === csv.id).revision;
      await n.menu('m_edition', 'annuler_action');
      await n.attendre(`__n.api('projet', { projet: ${JSON.stringify(csv.id)} }).then((x) => x.revision !== ${JSON.stringify(avant)})`);
      await n.libre();
      return (await n.etat()).projets.find((x) => x.id === csv.id);
    };
    const libelleAnnuler = async () => {
      await n.fermerMenus();
      await n.cliquer(`__n.texte('.menu-haut', __n.t('m_edition'))`);
      const l = await n.attendre("(() => { const e = __n.tous('.menu .item .libelle').find((x) => __n.norm(x.textContent).startsWith(__n.t('annuler_action'))); return e ? __n.norm(e.textContent) : null; })()");
      await n.fermerMenus();
      return l;
    };
    // ("Undo “variable sugar_acidity”", in the interface's quotes)
    const nomme = await libelleAnnuler();
    const [ouvre, ferme] = (await n.evaluer('__n.langueActuelle()')) === 'fr' ? ['« ', ' »'] : ['“', '”'];
    verifier('annulerNomme', nomme.startsWith(`${await n.t('annuler_action')} ${ouvre}`) && nomme.endsWith(ferme) && nomme.includes('sugar_acidity'), nomme);
    p = await annuler();
    verifier('annuleVariable', !variable(p, 'sugar_acidity') && [].concat(variable(p, 'orchard').categories).length === 2, p.variables.map((v) => v.nom).slice(-2));
    p = await annuler();
    verifier('annuleFusion', [].concat(variable(p, 'orchard').categories).join(',') === 'A,B,C', [].concat(variable(p, 'orchard').categories));
    p = await annuler();
    verifier('annuleRemplacement', [].concat(variable(p, 'variety').categories).join(',') === 'Gala,Golden,Granny,Reinette', [].concat(variable(p, 'variety').categories));
    verifier('vueDonneesAnnulee', !!(await n.attendre("__n.tous('.dv-content-container .tableau tbody td').some((c) => __n.norm(c.textContent) === 'Granny') && !__n.tous('.dv-content-container .tableau thead th').some((c) => __n.norm(c.textContent).startsWith('sugar_acidity'))")));
  },
};
