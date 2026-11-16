// Reopen last session: the skylark example's two projects (Help › Example projects › Skylark
// monitoring) saved with File › Save project, where the system's dialog says; Nuée quit and started
// again: the start page offers Reopen last session, whose click opens both again with their models.
// File › Settings… › At start-up ticked (taken at once, Cancel or not); one of the files deleted, Nuée
// started again: the other reopens by itself, the deleted one named in the status bar, not an error.
// (The box unticked at the end, for the scenarios after.)
const fs = require('fs');
const path = require('path');

module.exports = {
  async executer(n, verifier, { dossier, redemarrer }) {
    await n.sousMenu('m_aide', 'projet_exemple', 'exemple_alouettes');
    await n.attendre(`__n.api('etat').then((e) => [].concat(e.modeles).length === 3)`, 240000);
    await n.libre();
    // ---- each project saved, its page in front (the menus act on the project in front)
    const fichiers = { 'M1 · TRIM': path.join(dossier, 'comptages.nuee'), 'M1 · DISTANCE': path.join(dossier, 'transects.nuee') };
    for (const [onglet, fichier] of Object.entries(fichiers)) {
      await n.cliquer(`__n.onglet(${JSON.stringify(onglet)})`);
      await n.fichierAEnregistrer(fichier);
      await n.menu('m_fichier', 'enregistrer');
      await n.attendre(`__n.statut().startsWith(__n.t('enregistre')) && __n.statut().includes(${JSON.stringify(path.basename(fichier))})`);
    }
    verifier('enregistres', Object.values(fichiers).every((f) => fs.existsSync(f)));

    // ---- Nuée started again: the start page's Reopen last session
    let m = await redemarrer();
    const bouton = `__n.tous('.accueil-actions button').find((b) => __n.norm(b.textContent) === __n.t('seance_bouton'))`;
    const titre = await m.attendre(`(() => { const b = ${bouton}; return b ? b.title : null; })()`);
    verifier('boutonSeance', Object.values(fichiers).every((f) => titre.includes(f)), titre);
    await m.cliquer(bouton);
    const e = await m.attendre(`__n.api('etat').then((e) => [].concat(e.modeles).length === 3 && [].concat(e.projets).length === 2 ? e : null)`, 240000);
    const rouverte = await m.attendre(`__n.statut() === __n.t('seance_rouverte', { n: 2 }) && __n.statut()`);
    verifier('seanceRouverte', !!rouverte && [].concat(e.projets).map((p) => p.chemin_enregistrement).sort().join('|') === Object.values(fichiers).sort().join('|'), rouverte);
    verifier('boutonParti', await m.evaluer(`!(${bouton})`));

    // ---- File › Settings… › At start-up: ticked, taken at once
    await m.menu('m_fichier', 'reglages');
    await m.dialogue('reglages_titre');
    const coche = `[...__n.boite().querySelectorAll('label.seance-champ')].map((l) => l.querySelector('input'))[0]`;
    await m.coche(coche, true);
    await m.attendre('window.bureau.seance.rouvrir()');
    await m.bouton('annuler');
    await m.attendre('!__n.dialogue()');
    verifier('reglageGarde', await m.evaluer('window.bureau.seance.rouvrir()'));

    // ---- one file deleted, Nuée started again: the other reopens by itself, the deleted one named
    fs.unlinkSync(fichiers['M1 · DISTANCE']);
    m = await redemarrer();
    const statut = await m.attendre(`__n.statut().startsWith(__n.t('seance_rouverte_un')) && __n.statut()`, 240000);
    const p = await m.attendre(`__n.api('etat').then((e) => [].concat(e.projets).length === 1 && [].concat(e.modeles).length === 2 ? [].concat(e.projets)[0] : null)`, 240000);
    verifier('rouverteAuDemarrage', p.chemin_enregistrement === fichiers['M1 · TRIM'], p.chemin_enregistrement);
    verifier('absentNomme', statut.includes(await m.t('seance_absents', { noms: 'transects.nuee' })) && !(await m.evaluer("document.querySelector('#statut .moteur').classList.contains('erreur')")), statut);
    await m.evaluer('window.bureau.seance.rouvrir(false)');
  },
};
