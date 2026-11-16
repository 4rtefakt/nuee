// A project file changed by a colleague while its document is open here (a shared folder): a line
// typed here, a paragraph added there and saved; File › Save meets the file changed, Merge both;
// the open document's editor then shows both (versions.js: the documents the merge changed read
// again), and what is typed next keeps the colleague's paragraph.
const fs = require('fs');
const path = require('path');

const BASE = 'Line one.\n\nLine two.\n\nLine three.';
const ICI = '\n\nMy addition.';
const LEUR = '\n\nBob’s conclusion.';
const ENSUITE = ' Then more.';

module.exports = {
  async executer(n, verifier, { donnee, dossier }) {
    const csv = path.join(dossier, 'orchards.csv');
    fs.copyFileSync(donnee('exemple', 'orchards.csv'), csv);
    await n.fichierAOuvrir(csv);
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('orchards.csv'));
    const pid = (await n.etat()).projets[0].id;
    await n.menu('m_fichier', 'nouveau_document');
    await n.dialogue('nouveau_document');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'Report');
    await n.bouton('creer_document');
    await n.attendre('!__n.dialogue() && !!document.querySelector(".document .cm-content")');
    const editeur = "window.CM.EditorView.findFromDOM(document.querySelector('.document .cm-editor'))";
    const texteEditeur = `${editeur}.state.doc.toString()`;
    await n.cliquer("document.querySelector('.document .cm-content')");
    await n.evaluer(`(() => { const v = ${editeur}; v.dispatch({ changes: { from: 0, to: v.state.doc.length, insert: ${JSON.stringify(BASE)} } }); return true; })()`);
    const doc = await n.attendre(`__n.api('projet', { projet: ${JSON.stringify(pid)} }).then((p) => { const d = [].concat(p.documents || [])[0];
      return d ? __n.api('document', { projet: ${JSON.stringify(pid)}, id: d.id }).then((x) => ((x.texte || '') === ${JSON.stringify(BASE)} ? d.id : null)) : null; })`);
    const fichier = path.join(dossier, 'orchards.nuee');
    await n.fichierAEnregistrer(fichier);
    await n.menu('m_fichier', 'enregistrer');
    await n.attendre(`__n.statut().startsWith(__n.t('enregistre'))`);

    // ---- a colleague opens the same file, adds a paragraph at the end, saves it
    const bob = await n.api('ouvrir', { chemin: fichier });
    await n.api('document_enregistrer', { projet: bob.id, id: doc, titre: 'Report', texte: `${BASE}${LEUR}` });
    await n.api('enregistrer', { projet: bob.id, chemin: fichier, auteur: 'Bob' });
    await n.api('fermer_projet', { projet: bob.id });

    // ---- a line typed here meanwhile, after the first one; then Save: the file changed, Merge both
    await n.evaluer(`(() => { const v = ${editeur}; const k = v.state.doc.toString().indexOf('Line one.') + 'Line one.'.length;
      v.dispatch({ changes: { from: k, insert: ${JSON.stringify(ICI)} } }); return true; })()`);
    await n.menu('m_fichier', 'enregistrer');
    await n.attendre("__n.titreDialogue().startsWith(__n.t('versions_dossier_titre').split('{nom}')[0])", 60000);
    await n.cliquer("__n.boite().querySelector('.pied-dialogue button.principal')");
    await n.attendre(`!__n.dialogue() && __n.statut().startsWith(__n.t('enregistre'))`, 120000);
    const fusionne = await n.attendre(`(() => { const t = ${texteEditeur}; return t.includes('My addition.') && t.includes('Bob’s conclusion.') ? t : null; })()`, 20000)
      .catch(() => n.evaluer(texteEditeur));
    verifier('editeurOuvertMontreLaFusion', typeof fusionne === 'string' && fusionne.includes('My addition.') && fusionne.includes('Bob’s conclusion.'), fusionne);
    // (typed next: the colleague's paragraph stays)
    await n.evaluer(`(() => { const v = ${editeur}; v.focus(); v.dispatch({ changes: { from: v.state.doc.length, insert: ${JSON.stringify(ENSUITE)} } }); return true; })()`);
    const garde = await n.attendre(`__n.api('document', { projet: ${JSON.stringify(pid)}, id: ${JSON.stringify(doc)} }).then((x) => ((x.texte || '').includes('Then more.') ? x.texte : null))`)
      .catch(() => null);
    verifier('frappeSuivanteGardeLaFusion', !!garde && garde.includes('Bob’s conclusion.') && garde.includes('My addition.'), garde);
    // (the one refusal expected: Save meeting the file changed, 409, which opened the merge)
    const refus = await n.evaluer('window.__essai.refus.slice()');
    verifier('unSeulRefusAttendu', refus.filter((r) => r === 'enregistrer 409').length === 1, refus);
    await n.evaluer("(window.__essai.refus = window.__essai.refus.filter((r) => r !== 'enregistrer 409'), true)");
  },
};
