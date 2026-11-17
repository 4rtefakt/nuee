// Several windows on one project (app.js, versions.js): a PLS's scores moved to a window of their own
// follow the PLS when Edit › Update the data fits it again, and go back to the main window; Undo
// pressed in a view's own window fits the PLS again there and in the main window; text typed in a
// document moved to its own window counts as unsaved for History › Restore, which asks first, and
// that window follows the project read again from its file, its next words going to it; History in
// a window of its own restores through the main window, which reads the project again, no project
// left behind in the engine.
const fs = require('fs');
const path = require('path');

const MODELE = 'M1';
const TEXTE = 'First text.';
const TAPE = ' Typed in its own window.';
const APRES = ' After the restore.';

module.exports = {
  async executer(n, verifier, { donnee, dossier }) {
    // ---- the table, the PLS, a document, the project saved
    const lignes = fs.readFileSync(donnee('exemple', 'orchards.csv'), 'utf8').split(/\r?\n/).filter(Boolean);
    const csv = path.join(dossier, 'orchards.csv');
    fs.writeFileSync(csv, `${lignes.join('\n')}\n`);
    await n.fichierAOuvrir(csv);
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('orchards.csv'));
    const pid = (await n.etat()).projets[0].id;
    const pls = await n.nouveauModele({ type: 'PLS', y: ['taste_score'], sans: ['variety', 'farming', 'orchard'] });
    await n.menu('m_fichier', 'nouveau_document');
    await n.dialogue('nouveau_document');
    await n.ecrire("__n.boite().querySelector('.grille-champs input')", 'Season report');
    await n.bouton('creer_document');
    await n.attendre('!__n.dialogue() && !!document.querySelector(".document .cm-content")');
    await n.cliquer("document.querySelector('.document .cm-content')");
    await n.taper(TEXTE);
    await n.touche('Escape');
    const texteDocument = (projet) => `__n.api('projet', { projet: ${JSON.stringify(projet)} }).then((p) => { const d = [].concat(p.documents || [])[0];
      return d ? __n.api('document', { projet: ${JSON.stringify(projet)}, id: d.id }).then((x) => x.texte || '') : null; })`;
    await n.attendre(`${texteDocument(pid)}.then((t) => t !== null && t.includes(${JSON.stringify(TEXTE)}))`);
    const fichier = path.join(dossier, 'orchards.nuee');
    await n.fichierAEnregistrer(fichier);
    await n.menu('m_fichier', 'enregistrer');
    await n.attendre(`__n.statut().startsWith(__n.t('enregistre'))`);
    await n.attendre(`window.bureau.versions.liste(${JSON.stringify(fichier)}).then((v) => v.length >= 1)`);

    // A view moved to a window of its own: its tab in front, then its group's Move to a new window
    const detacher = async (titre, motif) => {
      const onglet = `__n.onglet(${JSON.stringify(titre)})`;
      await n.cliquer(onglet);
      await n.cliquer(`${onglet}.closest('.dv-groupview').querySelector('.actions-groupe button[title="' + __n.t('detacher') + '"]')`);
      return n.fenetre(motif);
    };
    const dernierAjustement = `__n.api('etat').then((e) => { const x = [].concat(e.modeles).filter((m) => m.nom === ${JSON.stringify(MODELE)}); return x.length === 1 ? x[0] : null; })`;
    const pagesDuModele = `__n.tous('.dv-tab').filter((e) => __n.norm(e.textContent) === ${JSON.stringify(`${MODELE} · PLS`)}).length`;

    // ---- the scores in their own window, then Update the data: they follow the PLS's new fit
    const scores = await n.t('scores');
    // (the PLS's page in front, its scores in it)
    await n.cliquer(`__n.onglet(${JSON.stringify(`${MODELE} · PLS`)})`);
    const f = await detacher(scores, 'vue=scores');
    await f.attendre(`location.search.includes(${JSON.stringify(`modele=${pls.id}`)}) && !!document.querySelector('#seul .js-plotly-plot')`);
    // (a second main window, the PLS's page open in it too: Window › New window, a double-click on the fit)
    await n.menu('m_fenetre', 'nouvelle_fenetre');
    const w2 = await n.fenetre('fenetre=');
    await w2.attendre(`!!${n.ligneModele(MODELE, true)}`);
    await w2.cliquer(`(${n.ligneModele(MODELE, true)}).querySelector('.libelle')`, 2);
    await w2.attendre(`${pagesDuModele} === 1`);
    const entetes = lignes[0].split(',');
    const [iNom, iNote] = [entetes.indexOf('fruit'), entetes.indexOf('taste_score')];
    const nouvelles = lignes.slice(1, 11).map((l, k) => {
      const c = l.split(',');
      c[iNom] = `P${121 + k}`;
      c[iNote] = (10 - Number(c[iNote])).toFixed(1);
      return c.join(',');
    });
    fs.writeFileSync(csv, `${[...lignes, ...nouvelles].join('\n')}\n`);
    await n.menu('m_edition', 'etapes_mettre_a_jour');
    await n.dialogue('etapes_maj_titre');
    await n.attendre("(() => { const b = __n.boite().querySelector('.pied-dialogue button.principal'); return !!b && !b.disabled; })()", 120000);
    await n.bouton('etapes_maj_bouton');
    await n.attendre(`__n.statut() === __n.t('etapes_p_fini', { n: 1 })`, 240000);
    const apres = await n.attendre(`${dernierAjustement}.then((m) => (m && m.N === 130 ? m : null))`);
    const suit = await f.attendre(`location.search.includes(${JSON.stringify(`modele=${apres.id}`)}) && !document.querySelector('#seul .note')
      && (() => { const g = document.querySelector('#seul .js-plotly-plot'); return !!(g && g.data); })() && document.title`).catch((e) => e.message);
    verifier('vueDetacheeSuitLeNouvelAjustement', typeof suit === 'string' && suit.includes(MODELE), suit);
    verifier('unePageDuModele', (await n.evaluer(pagesDuModele)) === 1, await n.evaluer(pagesDuModele));
    // (the second main window: its page of the PLS follows the new fit, its scores with the 130 fruits)
    const pageW2 = await w2.attendre(`${pagesDuModele} === 1 && __n.tous('.js-plotly-plot').some((g) => __n.points(g) >= 130)`).catch(() => false);
    verifier('secondeFenetreGardeLaPage', !!pageW2, await w2.evaluer(pagesDuModele).catch(() => null));
    await w2.evaluer('setTimeout(() => window.close(), 100), true').catch(() => {});
    w2.fermer();

    // ---- Move back to the main window: the scores of the new fit there
    await f.cliquer("[...document.querySelectorAll('#tete-seul button')].pop()");
    f.fermer();
    const rattache = await n.attendre(`!!__n.onglet(${JSON.stringify(scores)})`).catch(() => false);
    verifier('rattacheeAuNouvelAjustement', !!rattache);

    // ---- Undo pressed in a view's own window: the data before, the PLS fitted again, here and there
    await n.cliquer(`__n.onglet(${JSON.stringify(`${MODELE} · PLS`)})`);
    const g = await detacher(scores, `modele=${apres.id}`);
    await g.attendre(`location.search.includes(${JSON.stringify(`modele=${apres.id}`)}) && !!document.querySelector('#seul .js-plotly-plot')`);
    await g.ctrl('z', 'KeyZ', 90);
    const retour = await n.attendre(`${dernierAjustement}.then((m) => (m && m.N === 120 ? m : null))`, 240000).catch(() => null);
    verifier('annulerDansSaFenetreReajuste', !!retour && retour.id !== apres.id, retour && `${retour.id}: N=${retour.N}`);
    if (retour) {
      const ici = await g.attendre(`location.search.includes(${JSON.stringify(`modele=${retour.id}`)}) && !__n.dialogue()`).catch((e) => e.message);
      verifier('vueDetacheeSuitLAnnulation', ici === true, ici);
      const pages = await n.attendre(`(() => { const k = ${pagesDuModele}; return k === 1 ? k : null; })()`).catch(() => n.evaluer(pagesDuModele));
      verifier('pageDuModeleApresAnnulation', pages === 1, pages);
    }
    await g.evaluer('setTimeout(() => window.close(), 100), true').catch(() => {});
    g.fermer();

    // ---- the project saved; a document moved to its own window, written there; History › Restore
    await n.menu('m_fichier', 'enregistrer');
    await n.attendre(`__n.statut().startsWith(__n.t('enregistre')) && !(window.__n && __n.occupe())`);
    const fd = await detacher('Season report', 'vue=document');
    await fd.attendre("!!document.querySelector('#seul .document .cm-content')");
    await fd.evaluer(`(() => { const v = window.CM.EditorView.findFromDOM(document.querySelector('#seul .cm-editor')); v.focus();
      v.dispatch({ selection: { anchor: v.state.doc.length } }); return true; })()`);
    await fd.taper(TAPE);
    // (at once: what that window still holds is for History to send first)
    await n.menu('m_fichier', 'versions');
    await n.attendre("__n.tous('.liste-versions .version').length >= 2");
    const restaurerDernier = `[...__n.tous('.liste-versions .version')].pop().querySelector('.version-actions button:last-child')`;
    await n.cliquer(restaurerDernier);
    await n.attendre("__n.titreDialogue().startsWith(__n.t('versions_restaurer_titre').split('{date}')[0])");
    await n.bouton('versions_bouton_restaurer');
    const demande = await n.attendre("__n.titreDialogue() === __n.t('versions_avant_titre')", 30000).catch(() => false);
    verifier('restaurerDemandeAvantDePerdre', !!demande);
    if (!demande) return;
    await n.bouton('versions_enregistrer_restaurer');
    const restauree = (await n.t('versions_restauree')).split('{date}')[0];
    await n.attendre(`__n.statut().startsWith(${JSON.stringify(restauree)})`, 240000);
    const e2 = await n.etat();
    const pid2 = e2.projets.length === 1 ? e2.projets[0].id : null;
    verifier('unSeulProjetApresRestauration', !!pid2 && pid2 !== pid, e2.projets.map((p) => p.id));
    // (what was typed there was saved first: the history has it)
    const versions = await n.evaluer(`window.bureau.versions.liste(${JSON.stringify(fichier)})`);
    let garde = false;
    for (const v of versions) {
      const lu = await n.evaluer(`window.bureau.versions.lire(${JSON.stringify(fichier)}, ${JSON.stringify(v.oid)})`).catch(() => null);
      const j = lu && lu.fichier && fs.existsSync(lu.fichier) ? JSON.parse(fs.readFileSync(lu.fichier, 'utf8')) : null;
      if (j && [].concat(j.documents || []).some((d) => String(d.texte || d.text || '').includes(TAPE.trim()))) { garde = true; break; }
    }
    verifier('texteDeSaFenetreGardeDansLHistoire', garde, versions.map((v) => v.description));
    // (the document's window follows the project to its new id, shows its text as restored, and
    // what is typed there next goes to it)
    const lie = await fd.attendre(`location.search.includes(${JSON.stringify(`projet=${pid2}`)}) && __n.norm(document.querySelector('#seul .cm-content').textContent)`)
      .catch((e) => e.message);
    verifier('documentDetacheSuitLeProjet', typeof lie === 'string' && lie.includes(TEXTE) && !lie.includes(TAPE.trim()), lie);
    await fd.evaluer(`(() => { const v = window.CM.EditorView.findFromDOM(document.querySelector('#seul .cm-editor')); v.focus();
      v.dispatch({ selection: { anchor: v.state.doc.length } }); return true; })()`);
    await fd.taper(APRES);
    const ecrit = await n.attendre(`${texteDocument(pid2)}.then((t) => t !== null && t.includes(${JSON.stringify(APRES.trim())}) ? t : null)`).catch(() => null);
    verifier('documentDetacheEcritDansLeProjet', !!ecrit, ecrit);

    // ---- History in a window of its own: Restore, the changes let go, done by the main window
    const titreHistorique = `${e2.projets[0].nom} · ${await n.t('versions_vue')}`;
    await n.attendre(`!!__n.onglet(${JSON.stringify(titreHistorique)})`);
    const fh = await detacher(titreHistorique, 'vue=versions');
    // (the version Restore kept of the file before, with what was typed in the document's window)
    const avecTape = `__n.tous('.liste-versions .version').find((v) => __n.norm(v.querySelector('.version-tete').textContent).includes('Season report'))`;
    await fh.attendre(`!!${avecTape}`);
    await fh.cliquer(`${avecTape}.querySelector('.version-actions button:last-child')`);
    await fh.attendre("__n.titreDialogue().startsWith(__n.t('versions_restaurer_titre').split('{date}')[0])");
    const boutonFh = (cle) => `__n.tous('button', __n.boite()).find((b) => __n.norm(b.textContent) === __n.norm(__n.t(${JSON.stringify(cle)})))`;
    await fh.cliquer(boutonFh('versions_bouton_restaurer'));
    const demandeFh = await fh.attendre("__n.titreDialogue() === __n.t('versions_avant_titre')", 30000).catch(() => false);
    verifier('historiqueDetacheDemandeAvant', !!demandeFh);
    if (!demandeFh) return;
    await fh.cliquer(boutonFh('versions_sans_garder'));
    const pid3 = await n.attendre(`__n.api('etat').then((e) => { const p = [].concat(e.projets); return p.length === 1 && p[0].id !== ${JSON.stringify(pid2)} ? p[0].id : null; })`, 240000)
      .catch(() => null);
    verifier('historiqueDetacheRestaureParLaFenetrePrincipale', !!pid3, (await n.etat()).projets.map((p) => p.id));
    if (pid3) {
      const explorateur = await n.attendre(`!!${n.ligneProjet('orchards.nuee')} && !__n.occupe()`).catch(() => false);
      verifier('projetRelu', !!explorateur);
      const liees = await Promise.all([fh, fd].map((x) => x.attendre(`location.search.includes(${JSON.stringify(`projet=${pid3}`)})`).catch(() => false)));
      verifier('fenetresSuiventLeProjetRelu', liees.every(Boolean), liees);
      const texte = await n.evaluer(texteDocument(pid3));
      verifier('versionRestaureeChangementsLaisses', typeof texte === 'string' && texte.includes(TAPE.trim()) && !texte.includes(APRES.trim()), texte);
    }
    // (each window closed a moment after the driver stops listening to it: the requests its closing
    // cuts, a document's last words sent with keepalive among them, are no errors of the page)
    for (const x of [fh, fd]) {
      await x.evaluer('setTimeout(() => window.close(), 100), true').catch(() => {});
      x.fermer();
    }
  },
};
