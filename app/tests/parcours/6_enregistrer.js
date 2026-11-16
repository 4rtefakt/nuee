// Save and reopen: the example project (Help › Example projects › Apples) with its PLS M2 fitted
// too, a document that cites {{M2.Q2}} and shows the VIP, a figure made of the PLS's scores (Plots ›
// Compose a figure);
// File › Save project, File › Close project, Nuée quit and started again, File › Open: the models
// are fitted again with the same numbers, the document and the figure are back as they were.
const fs = require('fs');
const path = require('path');

module.exports = {
  async executer(n, verifier, { dossier, redemarrer }) {
    await n.sousMenu('m_aide', 'projet_exemple', 'exemple_pommes');
    await n.attendre(n.ligneModele('M1', true), 120000);
    // M2, the taste score's PLS, fitted from its line under the project's models
    await n.double(`(${n.ligneModele('M2', false)}).querySelector('.libelle')`);
    await n.attendre(n.ligneModele('M2', true), 120000);
    const scores = await n.t('scores');
    await n.plotlyPret(`M2 · ${scores}`, 120);
    const avant = { M1: await n.modele('M1'), M2: await n.modele('M2') };

    // ---- a document
    await n.menu('m_fichier', 'nouveau_document');
    await n.dialogue('nouveau_document');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'Taste');
    await n.bouton('creer_document');
    await n.attendre('!__n.dialogue() && !!document.querySelector(".document .cm-content")');
    await n.cliquer("document.querySelector('.document .cm-content')");
    await n.taper('The PLS predicts the taste score with Q2 = {{M2.Q2}}.');
    await n.touche('Escape');
    await n.touche('Enter');
    await n.touche('Enter');
    await n.taper('{{M2.vip | What makes the score}}');
    await n.touche('Escape');
    const apercu = () => n.attendre(`(() => { const a = document.querySelector('.document .apercu-document'); const i = a && a.querySelector('img');
      return a && i && i.complete && i.naturalWidth > 0 && !document.querySelector('.document .etat-document .erreur') ? __n.norm(a.textContent) : null; })()`);
    const apercuAvant = await apercu();
    verifier('documentEcrit', apercuAvant.includes('Q2 = ') && apercuAvant.includes('What makes the score'), apercuAvant);

    // ---- a figure: the PLS's scores in front, then Plots › Compose a figure
    await n.cliquer(`__n.onglet('M2 · PLS')`);
    await n.cliquer(`__n.onglet(${JSON.stringify(scores)})`);
    await n.plotlyPret(`M2 · ${scores}`, 120);
    await n.menu('m_graphiques', 'figure_composer');
    const figure = () => n.attendre(`(() => { const i = document.querySelector('.cadre-figure .feuille-figure img'); return i && i.complete && i.naturalWidth > 0 ? i.naturalWidth : 0; })()`, 120000);
    verifier('figureComposee', (await figure()) > 0);
    await n.libre();

    // ---- File › Save project: where the system's dialog says
    const fichier = path.join(dossier, 'orchards-saved.nuee');
    await n.fichierAEnregistrer(fichier);
    await n.menu('m_fichier', 'enregistrer');
    const enregistre = await n.attendre(`__n.statut().startsWith(__n.t('enregistre')) && __n.statut()`);
    verifier('enregistre', fs.existsSync(fichier) && enregistre.includes('orchards-saved.nuee'), enregistre);
    const projet = (await n.etat()).projets[0];
    const docs = [].concat(projet.documents || []);
    const figs = [].concat(projet.figures || []);
    const texteAvant = (await n.api('document', { projet: projet.id, id: docs[0].id })).texte;
    const figureAvant = (await n.api('figure', { projet: projet.id, id: figs[0].id })).contenu;
    verifier('rienANoterAvant', !projet.modifie && docs.length === 1 && figs.length === 1, `modified ${projet.modifie}, ${docs.length} document(s), ${figs.length} figure(s)`);

    // ---- File › Close project (saved: nothing asked), then Nuée quit and started again
    await n.menu('m_fichier', 'fermer_projet');
    await n.attendre("!!document.querySelector('#dock .accueil-explorateur')");
    verifier('ferme', (await n.etat()).projets.length === 0);
    const sans = (await n.principal()).appels.filter((a) => a.dialogue === 'message');
    verifier('rienDemandeEnFermant', sans.length === 0, sans);
    const m = await redemarrer();

    // ---- File › Open: the project, its models fitted again, its document and its figure
    await m.fichierAOuvrir(fichier);
    await m.menu('m_fichier', 'ouvrir');
    await m.attendre(m.ligneModele('M2', true), 120000);
    await m.attendre(m.ligneModele('M1', true), 120000);
    const restaure = await m.attendre(`__n.statut() === __n.t('session_restauree', { n: 2 }) && __n.statut()`);
    verifier('sessionRestauree', !!restaure, restaure);
    const apres = { M1: await m.modele('M1'), M2: await m.modele('M2') };
    const memes = (a, b, cles) => cles.every((k) => JSON.stringify([].concat(a.composantes[k])) === JSON.stringify([].concat(b.composantes[k])));
    verifier('memesNombres', ['M1', 'M2'].every((k) => apres[k].A === avant[k].A && apres[k].N === avant[k].N && memes(apres[k], avant[k], ['R2Xcum', 'Q2cum'])
      && (k === 'M1' || memes(apres[k], avant[k], ['R2Ycum']))), `M2 Q2(cum) ${[].concat(apres.M2.composantes.Q2cum).slice(-1)} / ${[].concat(avant.M2.composantes.Q2cum).slice(-1)}`);
    // the window laid out as it was: the two models' pages, the document and the figure, in their tabs
    const onglets = await m.attendre(`(() => { const o = ['M1 · PCA', 'M2 · PLS', 'Taste', ${JSON.stringify(figs[0].titre)}].map((x) => !!__n.onglet(x)); return o.every(Boolean) ? o : null; })()`);
    verifier('ongletsRevenus', !!onglets);
    // the page of M2: its scores, with the engine's numbers
    await m.cliquer(`__n.onglet('M2 · PLS')`);
    await m.cliquer(`__n.onglet(${JSON.stringify(scores)})`);
    await m.plotlyPret(`M2 · ${scores}`, 120);
    const t1 = await m.evaluer(`[...__n.graphe(${JSON.stringify(`M2 · ${scores}`)}).data[0].x]`);
    verifier('scoresRestaures', JSON.stringify(t1) === JSON.stringify([].concat(avant.M2.observations.t1)), t1.length);
    const p2 = (await m.etat()).projets[0];
    const d2 = [].concat(p2.documents || []);
    const f2 = [].concat(p2.figures || []);
    verifier('documentRevenu', d2.length === 1 && d2[0].titre === 'Taste' && (await m.api('document', { projet: p2.id, id: d2[0].id })).texte === texteAvant, d2.map((d) => d.titre));
    verifier('figureRevenue', f2.length === 1 && f2[0].titre === figs[0].titre && (await m.api('figure', { projet: p2.id, id: f2[0].id })).contenu === figureAvant, f2.map((f) => f.titre));
    // both open from the explorer and show what they showed
    await m.double(m.ligne('Taste', 3));
    const apercuApres = await m.attendre(`(() => { const a = document.querySelector('.document .apercu-document'); const i = a && a.querySelector('img');
      return a && i && i.complete && i.naturalWidth > 0 && !document.querySelector('.document .etat-document .erreur') ? __n.norm(a.textContent) : null; })()`);
    verifier('memeApercu', apercuApres === apercuAvant, apercuApres);
    await m.double(m.ligne(figs[0].titre, 3));
    const largeur = await m.attendre(`(() => { const i = document.querySelector('.cadre-figure .feuille-figure img'); return i && i.complete && i.naturalWidth > 0 ? i.naturalWidth : 0; })()`, 120000);
    verifier('figureDessinee', largeur > 0, largeur);
  },
};
