// A view in a window of its own after Help › Restart the R engine (app.js: redemarrerMoteur): the new
// engine numbers its projects and fits from the start, so a PLS's scores moved to their own window
// follow the PLS to its fit in the new engine (its address, its plot), instead of keeping an id the new
// engine doesn't have (or gives to another model), and Move back opens them in the main window. A
// project opened and closed first makes the old ids differ from the new ones; the project is saved, so
// that the restart reopens it from its file with its PLS.
const fs = require('fs');
const path = require('path');

module.exports = {
  async executer(n, verifier, { donnee, dossier }) {
    const lignes = fs.readFileSync(donnee('exemple', 'orchards.csv'), 'utf8');
    const autre = path.join(dossier, 'other orchards.csv');
    const csv = path.join(dossier, 'orchards.csv');
    fs.writeFileSync(autre, lignes);
    fs.writeFileSync(csv, lignes);
    // ---- a project opened, then closed (its ids taken), then the one kept, a PLS fitted on it
    for (const f of [autre, csv]) {
      await n.fichierAOuvrir(f);
      await n.menu('m_fichier', 'ouvrir');
      await n.attendre(n.ligneProjet(path.basename(f)));
    }
    await n.cliquer(`(${n.ligneProjet(path.basename(autre))}).querySelector('.libelle')`);
    await n.menu('m_fichier', 'fermer_projet');
    await n.attendre(`!${n.ligneProjet(path.basename(autre))}`);
    const pls = await n.nouveauModele({ type: 'PLS', y: ['taste_score'], sans: ['variety', 'farming', 'orchard'] });
    // (saved: the restart reopens it from its file, with its PLS)
    await n.fichierAEnregistrer(path.join(dossier, 'orchards.nuee'));
    await n.menu('m_fichier', 'enregistrer');
    await n.attendre(`__n.statut().startsWith(__n.t('enregistre')) && !(window.__n && __n.occupe())`);
    const scores = await n.t('scores');
    await n.cliquer(`__n.onglet(${JSON.stringify(`${pls.nom} · PLS`)})`);
    const onglet = `__n.onglet(${JSON.stringify(scores)})`;
    await n.cliquer(onglet);
    await n.cliquer(`${onglet}.closest('.dv-groupview').querySelector('.actions-groupe button[title="' + __n.t('detacher') + '"]')`);
    const f = await n.fenetre('vue=scores');
    await f.attendre(`location.search.includes(${JSON.stringify(`modele=${pls.id}`)}) && !!document.querySelector('#seul .js-plotly-plot')`);

    // ---- Help › Restart the R engine: the project reopens from its file, its PLS fitted again
    await n.menu('m_aide', 'redemarrer_moteur');
    await n.dialogue('redemarrer_moteur');
    await n.bouton('redemarrer');
    await n.attendre(`__n.statut() === __n.t('moteur_redemarre', { n: 1 })`, 240000);
    const nouveau = await n.attendre(`__n.api('etat').then((e) => [].concat(e.modeles).find((m) => m.nom === ${JSON.stringify(pls.nom)} && m.type === 'PLS') || null)`, 120000);
    verifier('autresNumeros', nouveau.id !== pls.id, `${pls.id} → ${nouveau.id}`);
    // ---- the scores' own window: its address and plot those of the PLS's new fit
    const suit = await f.attendre(`location.search.includes(${JSON.stringify(`modele=${nouveau.id}`)}) && !document.querySelector('#seul .note')
      && (() => { const g = document.querySelector('#seul .js-plotly-plot'); return !!(g && g.data && __n.points(g) >= 120); })() && document.title`, 60000).catch((e) => e.message);
    verifier('vueDetacheeSuitLeRedemarrage', typeof suit === 'string' && suit.includes(pls.nom), suit);
    // ---- Move back: the scores of the new fit in the main window
    await f.cliquerQuiFerme("[...document.querySelectorAll('#tete-seul button')].pop()");
    const rattache = await n.attendre(`(() => { const o = ${onglet}; return !!o; })()`).catch(() => false);
    verifier('rattacheeApresRedemarrage', !!rattache);
  },
};
