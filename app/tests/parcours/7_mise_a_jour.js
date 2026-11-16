// Updating the data, a living report: the example's table written to a CSV file and opened (the
// file is the table's source), a PLS of the taste score fitted with Model › New model, a document
// that cites its Q2, the project saved (History keeps its first version). Then next season's ten
// fruits added to the file: Edit › Update the data… previews what it brings, Update fits the PLS
// again on 130 fruits and opens What changed (the data's report, the PLS's key results compared,
// the document's cited Q2); File › History… › Save a version… keeps the updated project as one
// more version; Edit › Undo brings the 120 fruits back and fits the PLS again, as it was.
const fs = require('fs');
const path = require('path');

const q2 = (m) => [].concat(m.composantes.Q2cum).slice(-1)[0];

module.exports = {
  async executer(n, verifier, { donnee, dossier }) {
    // ---- the table: a CSV file of this folder, opened with File › Open
    const lignes = fs.readFileSync(donnee('exemple', 'orchards.csv'), 'utf8').split(/\r?\n/).filter(Boolean);
    const csv = path.join(dossier, 'orchards.csv');
    fs.writeFileSync(csv, `${lignes.join('\n')}\n`);
    await n.fichierAOuvrir(csv);
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('orchards.csv'));
    const pid = (await n.etat()).projets[0].id;

    // ---- a PLS of the taste score, and a document that cites its Q2
    const pls = await n.nouveauModele({ type: 'PLS', y: ['taste_score'], sans: ['variety', 'farming', 'orchard'] });
    verifier('plsAjustee', pls.N === 120, `${pls.nom}: N=${pls.N}`);
    await n.menu('m_fichier', 'nouveau_document');
    await n.dialogue('nouveau_document');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'Season report');
    await n.bouton('creer_document');
    await n.attendre('!__n.dialogue() && !!document.querySelector(".document .cm-content")');
    await n.cliquer("document.querySelector('.document .cm-content')");
    await n.taper(`The PLS predicts the taste score with Q2 = {{${pls.nom}.Q2}}.`);
    await n.touche('Escape');
    const cite = await n.attendre(`(() => { const a = document.querySelector('.document .apercu-document');
      return a && !document.querySelector('.document .etat-document .erreur') && /Q2 = \\d/.test(a.textContent) ? __n.norm(a.textContent) : null; })()`);
    verifier('documentEcrit', cite.includes('Q2 = '), cite);

    // ---- File › Save project
    const fichier = path.join(dossier, 'orchards.nuee');
    await n.fichierAEnregistrer(fichier);
    await n.menu('m_fichier', 'enregistrer');
    await n.attendre(`__n.statut().startsWith(__n.t('enregistre')) && __n.statut()`);
    verifier('enregistre', fs.existsSync(fichier));

    // ---- next season: ten more fruits in the file (the first ten measured again, with their taste
    // scores the other way round, so that the PLS's Q2 moves)
    const entetes = lignes[0].split(',');
    const [iNom, iNote] = [entetes.indexOf('fruit'), entetes.indexOf('taste_score')];
    const nouvelles = lignes.slice(1, 11).map((l, k) => {
      const c = l.split(',');
      c[iNom] = `P${121 + k}`;
      c[iNote] = (10 - Number(c[iNote])).toFixed(1);
      return c.join(',');
    });
    fs.writeFileSync(csv, `${[...lignes, ...nouvelles].join('\n')}\n`);

    // ---- Edit › Update the data…: the file read again and previewed before anything changes
    await n.menu('m_edition', 'etapes_mettre_a_jour');
    await n.dialogue('etapes_maj_titre');
    const apercu = await n.attendre("(() => { const r = __n.boite().querySelector('.apercu-maj .resume-maj'); return r ? __n.norm(r.textContent) : null; })()", 120000);
    verifier('apercuMiseAJour', /\b10\b/.test(apercu), apercu);
    await n.attendre("(() => { const b = __n.boite().querySelector('.pied-dialogue button.principal'); return !!b && !b.disabled; })()");
    await n.bouton('etapes_maj_bouton');
    const fini = await n.attendre(`__n.statut() === __n.t('etapes_p_fini', { n: 1 }) && __n.statut()`, 240000);
    verifier('miseAJourFaite', !!fini, fini);
    const p1 = (await n.etat()).projets.find((x) => x.id === pid);
    const plsApres = await n.modele(pls.nom);
    verifier('plsAjusteeDeNouveau', p1.observations.uid.length === 130 && plsApres.N === 130 && q2(plsApres) !== q2(pls),
      `${p1.observations.uid.length} fruits, ${pls.nom}: N=${plsApres.N}, Q2 ${q2(pls)} → ${q2(plsApres)}`);

    // ---- What changed: the data's report, the PLS compared, the document's cited value
    const changements = await n.attendre(`(() => { const c = __n.tous('.changements').find((x) => x.querySelector('h2') && !x.querySelector('.attente'));
      if (!c) return null;
      const texte = (s) => { const e = c.querySelector(s); return e ? __n.norm(e.textContent) : ''; };
      return { donnees: texte('.donnees-change .resume-maj'), documents: texte('.documents-change'),
        modeles: [...c.querySelectorAll('section.modele-change')].filter((s) => !s.matches('.donnees-change, .documents-change, .seul-cote'))
          .map((s) => __n.norm(s.querySelector('h3').textContent)) }; })()`, 120000);
    verifier('changementsDonnees', /\b10\b/.test(changements.donnees), changements.donnees);
    verifier('changementsModele', changements.modeles.length === 1 && changements.modeles[0].startsWith(pls.nom), changements.modeles);
    verifier('changementsDocument', changements.documents.includes('Season report') && changements.documents.includes('Q2'), changements.documents);

    // ---- File › History…: the first save's version (and one kept before the update, when anything
    // changed since); Save a version… keeps the updated project as one more
    await n.menu('m_fichier', 'versions');
    const gardees = await n.attendre("(() => { const v = __n.tous('.liste-versions .version').length; return v ? v : null; })()");
    await n.cliquer("__n.tous('.versions .outils button').find((b) => __n.norm(b.textContent) === __n.norm(__n.t('versions_maintenant')))");
    await n.attendre("__n.titreDialogue().startsWith(__n.t('versions_description_titre').split('{nom}')[0])");
    const description = 'With next season’s fruits';
    await n.ecrire("__n.boite().querySelector('.dialogue-version input')", description, { quitter: false });
    await n.bouton('versions_bouton_enregistrer');
    const gardee = await n.attendre(`__n.statut() === __n.t('versions_enregistree', { description: ${JSON.stringify(description)} }) && __n.statut()`);
    verifier('versionGardee', !!gardee, gardee);
    const versions = await n.attendre(`(() => { const v = __n.tous('.liste-versions .version'); return v.length === ${gardees + 1} ? v.map((x) => __n.norm(x.querySelector('.version-tete').textContent)) : null; })()`);
    verifier('versionDecrite', gardees <= 2 && versions[0].includes(description), versions);

    // ---- Edit › Undo: the update undone, the PLS fitted again on the 120 fruits, as it was
    await n.menu('m_edition', 'annuler_action');
    const debut = (await n.t('etapes_annulation_fini')).split('{quoi}')[0];
    const annule = await n.attendre(`__n.statut().startsWith(${JSON.stringify(debut)}) && !__n.dialogue() && __n.statut()`, 240000);
    const p2 = (await n.etat()).projets.find((x) => x.id === pid);
    const plsRetour = await n.modele(pls.nom);
    verifier('miseAJourAnnulee', p2.observations.uid.length === 120 && plsRetour.N === 120 && Math.abs(q2(plsRetour) - q2(pls)) < 1e-9,
      `${annule} ${p2.observations.uid.length} fruits, ${pls.nom}: N=${plsRetour.N}, Q2 ${q2(plsRetour)}`);
  },
};
