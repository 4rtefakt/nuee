// Documents: the example project (Help › Example project, its PCA M1 fitted), File › New document,
// a sentence typed in its editor that cites {{M1.Q2}} and a line that shows the PCA's scores (a
// dialog opened and closed meanwhile gives the keyboard back to the text where it was); the
// preview gives the engine's Q2 and the plot; then Compile › HTML, Word and PDF, each written where
// the system's dialog says and read back. All three are written by Nuée itself (Electron prints the
// PDF): only Compile › Quarto needs other software, Quarto, and isn't run here.
const fs = require('fs');
const path = require('path');

// a value as the document writes it, in English: 2 decimals, the minus sign (references.js)
const enAnglais = (x, d = 2) => `${x < 0 && Math.abs(x).toFixed(d) !== (0).toFixed(d) ? '−' : ''}${Math.abs(x).toLocaleString('en-GB', { minimumFractionDigits: d, maximumFractionDigits: d })}`;
const texte = (html) => html.replace(/<[^>]+>/g, ' ').replace(/&nbsp;|&#160;/g, ' ').replace(/\s+/g, ' ');

module.exports = {
  async executer(n, verifier, { dossier }) {
    await n.menu('m_aide', 'projet_exemple');
    await n.attendre(n.ligneModele('M1', true), 120000);
    const m1 = await n.modele('M1');
    const q2 = enAnglais([].concat(m1.composantes.Q2cum)[m1.A - 1]);

    // ---- File › New document: a title, blank
    await n.menu('m_fichier', 'nouveau_document');
    await n.dialogue('nouveau_document');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'Workflow test');
    await n.bouton('creer_document');
    await n.attendre('!__n.dialogue() && !!document.querySelector(".document .cm-content")');
    // typed in the editor: a value in a sentence, and a plot on its own line
    await n.cliquer("document.querySelector('.document .cm-content')");
    await n.taper('The PCA predicts Q2 = {{M1.Q2}} of the variation.');
    await n.touche('Escape');  // (a completion list, if one is open)
    await n.touche('Enter');
    await n.touche('Enter');
    await n.taper('{{M1.scores | The scores of the PCA}}');
    await n.touche('Escape');
    // a dialog from the menu bar, closed with Esc: the keyboard comes back to the text where it was,
    // so the next key goes on from the end (a key typed and taken back)
    await n.menu('m_edition', 'atelier_remplacer');
    await n.dialogue('atelier_remplacer');
    await n.touche('Escape');
    await n.attendre('!__n.dialogue()');
    await n.taper('§');
    const fin = await n.evaluer("__n.norm(document.querySelector('.document .cm-content').textContent)");
    verifier('curseurGardeApresDialogue', fin.endsWith('The scores of the PCA}}§'), fin.slice(-60));
    await n.touche('Backspace');
    // the preview: the engine's Q2, the plot drawn, two references and none in error
    const apercu = await n.attendre(`(() => { const a = document.querySelector('.document .apercu-document'); const i = a && a.querySelector('img');
      return a && i && i.complete && i.naturalWidth > 0 && !document.querySelector('.document .etat-document .erreur') ? __n.norm(a.textContent) : null; })()`);
    verifier('apercuValeur', apercu.includes(`The PCA predicts Q2 = ${q2} of the variation.`), apercu.slice(0, 120));
    verifier('apercuFigure', /Figure 1/.test(apercu) && apercu.includes('The scores of the PCA'), apercu.slice(-80));
    const etat = await n.evaluer("__n.norm(document.querySelector('.document .etat-document').textContent)");
    verifier('deuxReferences', etat.includes(await n.t('document_refs', { n: 2 })), etat);

    // ---- Compile › HTML, Word, PDF
    const compiler = async (libelle, fichier) => {
      await n.fichierAEnregistrer(fichier);
      await n.cliquer(`__n.tous('.document .barre-document button').find((b) => __n.norm(b.textContent) === __n.t('document_compiler'))`);
      await n.cliquer(`__n.tous('.menu .item').find((e) => __n.norm(e.querySelector('.libelle').textContent) === ${JSON.stringify(libelle)})`);
      const s = await n.attendre(`__n.statut().startsWith(__n.t('document_ecrit')) && __n.statut().includes(${JSON.stringify(path.basename(fichier))}) && __n.statut()`, 120000);
      return fs.existsSync(fichier) ? s : null;
    };
    const html = path.join(dossier, 'workflow.html');
    verifier('html', !!(await compiler('HTML…', html)));
    const page = fs.readFileSync(html, 'utf8');
    verifier('htmlContenu', texte(page).includes(`Q2 = ${q2}`) && /<img[^>]+src="data:image\/png;base64,/.test(page) && /Figure 1/.test(texte(page)), texte(page).slice(0, 160));

    const docx = path.join(dossier, 'workflow.docx');
    verifier('word', !!(await compiler('Word (.docx)…', docx)));
    const z = n.lireZip(fs.readFileSync(docx));
    const corps = texte(z.has('word/document.xml') ? z.get('word/document.xml').toString('utf8') : '');
    verifier('wordContenu', corps.includes(`Q2 = ${q2}`) && [...z.keys()].some((k) => /^word\/media\/.+\.png$/.test(k)) && /Figure 1/.test(corps), [...z.keys()].filter((k) => k.startsWith('word/')).join(', '));

    const pdf = path.join(dossier, 'workflow.pdf');
    verifier('pdf', !!(await compiler('PDF…', pdf)));
    const octets = fs.readFileSync(pdf);
    verifier('pdfContenu', octets.subarray(0, 5).toString() === '%PDF-' && octets.length > 10000 && /\/Subtype\s*\/Image/.test(octets.toString('latin1')), `${octets.length} bytes`);

    // each file was handed to the system to open (main.js: shell.openPath), and opened by nobody here
    const ouverts = (await n.principal()).ouverts;
    verifier('ouvertsApresCompilation', [html, docx, pdf].every((f) => ouverts.includes(f)), ouverts);
  },
};
