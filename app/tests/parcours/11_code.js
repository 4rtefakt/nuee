// Your own model in R code: the trout densities (docs/site/donnees, made up) opened, Model › New model
// › type "R code", a mixed model of nlme typed in the code editor, Fit; its coefficients, its summary
// and its residuals as the engine has them, the plot its code drew; its code edited, fitted again in its
// place. Then the project saved, its code changed in a copy as a colleague would, the copy opened: its
// model not fitted, the bar over the project, Show the code (the colleague's code as it is), Run it:
// fitted, the approval kept in the settings. With screenshots for review: <screenshots>-code-*.png.
const fs = require('fs');
const path = require('path');

const proches = (a, b, tol = 1e-9) => a.length === b.length && a.every((x, i) => Math.abs(x - b[i]) <= tol * Math.max(1, Math.abs(b[i])));
const CODE = [
  'library(nlme)',
  '# the densities of each station: a trend per group, a random intercept per station',
  'fit <- lme(density ~ year * group, random = ~ 1 | station, data = donnees,',
  '           method = "ML", na.action = na.omit)',
  'plot(fit)',
  'fit',
].join('\n');

module.exports = {
  async executer(n, verifier, { donnee, dossier, captures }) {
    // (a screenshot once every plot has taken its panel's size: Plotly follows a panel that changed
    // size a moment after it, panneaux.js)
    const capture = async (nom) => {
      if (!captures) return;
      await n.attendre("[...document.querySelectorAll('.js-plotly-plot')].every((g) => !g._fullLayout || !g.offsetWidth || (Math.abs(g._fullLayout.width - g.offsetWidth) < 2 && Math.abs(g._fullLayout.height - g.offsetHeight) < 2))", 10000).catch(() => {});
      await n.capture(`${captures}-code-${nom}.png`);
    };
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'baci_trout.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('baci_trout.csv'));

    // ---- New model › R code: the editor, its help, the code typed
    const boite = '__n.boite()';
    await n.menu('m_modele', 'nouveau_modele');
    await n.dialogue('nouveau_modele');
    await n.cliquer(`__n.tous('.onglets button', ${boite}).find((b) => __n.norm(b.textContent) === __n.t('general'))`);
    await n.choisir(`__n.tous('select', ${boite}).find((s) => [...s.options].some((o) => o.value === 'PCA'))`, 'CODE');
    const onglets = await n.attendre(`(() => { const b = __n.tous('.onglets button', ${boite}).map((x) => __n.norm(x.textContent)); return b.length ? b : null; })()`);
    verifier('ongletsDuCode', onglets.length === 2 && !onglets.includes(await n.t('variables')), onglets);
    await n.attendre(`!!${boite}.querySelector('.editeur-code .cm-content')`);
    await n.cliquer(`${boite}.querySelector('.editeur-code .cm-content')`);
    await n.taper(CODE);
    const ecrit = await n.evaluer(`window.CM.EditorView.findFromDOM(${boite}.querySelector('.editeur-code .cm-editor')).state.doc.toString()`);
    verifier('codeEcrit', ecrit === CODE, ecrit);
    const aide = await n.evaluer(`__n.norm((${boite}.querySelector('.aide-code') || {}).textContent)`);
    verifier('aideDuCode', aide.includes('donnees') && aide.includes('valeurs'), aide.slice(0, 120));
    await capture('1-editeur');
    const avant = (await n.etat()).modeles.map((m) => m.id);
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 180000);
    const id = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles).find((x) => !${JSON.stringify(avant)}.includes(x.id)); return m ? m.id : null; })`);
    const m = await n.api('modele', { modele: id });
    await n.attendre(n.ligneModele(m.nom, true));
    const termes = [].concat(m.coefficients.terme);
    verifier('modeleDuCode', m.type === 'CODE' && [].concat(m.classe)[0] === 'lme' && m.N === 82 && termes.includes('year') && termes.includes('year:groupimpact')
      && [].concat(m.images).length === 1 && [].concat(m.paquets.nom).includes('nlme'), `${m.chiffres}; ${termes.join(', ')}`);

    // ---- its views: the coefficients (the engine's estimates), the summary, the residuals
    const coefficients = `${m.nom} · ${await n.t('coefficients')}`;
    await n.plotlyPret(coefficients, 1);
    const traces = await n.evaluer(`[...__n.graphe(${JSON.stringify(coefficients)}).data[0].x]`);
    // (the plot without the intercept, the table with it)
    const estimations = [].concat(m.coefficients.estimation).filter((_, i) => termes[i] !== '(Intercept)');
    verifier('coefficientsCommeLeMoteur', proches(traces, estimations), traces);
    const residus = `${m.nom} · ${await n.t('residus_ajustes')}`;
    const points = await n.plotlyPret(residus, 82);
    verifier('residus', points === 82, points);
    const resume = await n.attendre(`(() => { const r = __n.tous('.resume').find((x) => __n.norm((x.querySelector('.fiche-titre b') || {}).textContent) === ${JSON.stringify(m.nom)});
      return r && r.querySelector('pre.code-sortie') ? __n.norm(r.textContent) : null; })()`);
    verifier('resumeDuCode', resume.includes('lme') && resume.includes('Linear mixed-effects model') && resume.includes('nlme'), resume.slice(0, 160));
    await capture('2-resultats');

    // ---- its code edited (the summary's Edit the code): fitted again in its place, under its name
    await n.cliquer(`__n.tous('button').find((b) => __n.norm(b.textContent) === __n.t('code_modifier'))`);
    await n.attendre(`!!${boite} && !!${boite}.querySelector('.editeur-code .cm-content')`);
    // (its type stays: the fit takes the place of an R-code model's)
    const typeFige = await n.evaluer(`__n.tous('select', ${boite}).find((s) => [...s.options].some((o) => o.value === 'PCA')).disabled`);
    verifier('typeFigeEnModifiant', typeFige === true, typeFige);
    const CODE2 = CODE.replace('year * group', 'year + group');
    await n.evaluer(`(() => { const v = window.CM.EditorView.findFromDOM(${boite}.querySelector('.editeur-code .cm-editor'));
      v.dispatch({ changes: { from: 0, to: v.state.doc.length, insert: ${JSON.stringify(CODE2)} } }); return true; })()`);
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 180000);
    const m2 = await n.attendre(`__n.api('etat').then((e) => { const x = [].concat(e.modeles).filter((y) => y.nom === ${JSON.stringify(m.nom)} && y.projet === ${JSON.stringify(m.projet)});
      return x.length === 1 && x[0].id !== ${JSON.stringify(id)} ? x[0] : null; })`);
    verifier('codeModifieReajuste', m2.code === CODE2 && ![].concat(m2.coefficients.terme).includes('year:groupimpact'), [].concat(m2.coefficients.terme).join(', '));
    // (its number kept: its figures, documents and session know it by it; one definition of it)
    const definitions = [].concat((await n.api('projet', { projet: m.projet })).modeles).filter((d) => d.nom === m.nom);
    verifier('numeroGarde', m2.numero === m.numero && definitions.length === 1 && definitions[0].numero === m.numero, { avant: m.numero, apres: m2.numero, definitions: definitions.length });
    await n.plotlyPret(coefficients, 1);
    // ---- Edit the code from its summary in a window of its own: the dialog in the main window (app.js:
    // demanderPrincipale), its fit followed in that window (ajustement_remplace), still one fit
    const ongletResume = `__n.onglet(__n.t('resume'))`;
    await n.cliquer(ongletResume);
    await n.cliquer(`${ongletResume}.closest('.dv-groupview').querySelector('.actions-groupe button[title="' + __n.t('detacher') + '"]')`);
    const fr = await n.fenetre('vue=resume');
    await fr.cliquer(`__n.tous('button').find((b) => __n.norm(b.textContent) === __n.t('code_modifier'))`);
    await n.attendre(`!!${boite} && !!${boite}.querySelector('.editeur-code .cm-content')`);
    const CODE2b = `${CODE2}\n# edited from a window of its own`;
    await n.evaluer(`(() => { const v = window.CM.EditorView.findFromDOM(${boite}.querySelector('.editeur-code .cm-editor'));
      v.dispatch({ changes: { from: 0, to: v.state.doc.length, insert: ${JSON.stringify(CODE2b)} } }); return true; })()`);
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 180000);
    const m3 = await n.attendre(`__n.api('etat').then((e) => { const x = [].concat(e.modeles).filter((y) => y.nom === ${JSON.stringify(m.nom)} && y.projet === ${JSON.stringify(m.projet)});
      return x.length === 1 && x[0].code === ${JSON.stringify(CODE2b)} ? x[0] : null; })`);
    const suivi = await fr.attendre(`location.search.includes(${JSON.stringify(`modele=${m3.id}`)}) && __n.tous('.resume').some((x) => __n.norm(x.textContent).includes('lme'))`).catch((e) => e.message);
    verifier('modifierLeCodeDepuisSaFenetre', suivi === true && m3.numero === m.numero, suivi);
    await fr.evaluer('setTimeout(() => window.close(), 100), true').catch(() => {});
    fr.fermer();
    await n.plotlyPret(coefficients, 1);

    // ---- saved, then changed by a colleague in a copy
    const fichier = path.join(dossier, 'trout.nuee');
    await n.fichierAEnregistrer(fichier);
    await n.menu('m_fichier', 'enregistrer');
    await n.attendre(`__n.statut().startsWith(__n.t('enregistre'))`);
    const j = JSON.parse(fs.readFileSync(fichier, 'utf8'));
    const CODE3 = `${CODE2}\n# a colleague's change: the same model, fitted by REML\nupdate(fit, method = "REML")`;
    for (const d of j.models) if (d.type === 'CODE') d.options.code = CODE3;
    const copie = path.join(dossier, 'trout-colleague.nuee');
    fs.writeFileSync(copie, JSON.stringify(j));
    await n.fichierAOuvrir(copie);
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('trout-colleague.nuee'));
    const barre = await n.attendre("(() => { const b = document.querySelector('#barres-code .barre-code'); return b ? __n.norm(b.textContent) : null; })()");
    const e = await n.etat();
    const pc = e.projets.find((p) => p.nom === 'trout-colleague.nuee');
    verifier('barreDuCodeVenuDAilleurs', barre.includes(m.nom) && barre.includes(await n.t('code_executer')) && !e.modeles.some((x) => x.projet === pc.id), barre);
    await capture('3-barre');

    // ---- Show the code: the colleague's code, as it is; then Run it
    await n.cliquer(`__n.tous('#barres-code button').find((b) => __n.norm(b.textContent) === __n.t('code_montrer'))`);
    await n.dialogue('code_montrer');
    const montre = await n.attendre(`(() => { const p = ${boite}.querySelector('pre.code-r'); return p ? [...p.querySelectorAll('.ligne-code')].map((l) => l.textContent).join('\\n') : null; })()`);
    verifier('codeMontreTelQuel', montre === CODE3, montre);
    // (the keyboard on the code, not on Run it: Enter runs nothing unread)
    const focus = await n.evaluer(`(() => { const a = document.activeElement; return a === ${boite}.querySelector('.corps-dialogue') ? 'code' : a && a.matches('button.principal') ? 'executer' : (a && a.tagName) || ''; })()`);
    verifier('clavierSurLeCode', focus === 'code', focus);
    await capture('4-montrer');
    await n.bouton('code_executer');
    const fait = await n.attendre(`__n.api('etat').then((e) => [].concat(e.modeles).find((x) => x.projet === ${JSON.stringify(pc.id)} && x.type === 'CODE') || null)`, 180000);
    await n.attendre("!document.querySelector('#barres-code .barre-code')");
    verifier('executeApresApprobation', fait.code === CODE3 && [].concat(fait.classe)[0] === 'lme', fait.chiffres);
    const reglages = JSON.parse(fs.readFileSync(path.join(path.dirname(dossier), 'profil', 'reglages.json'), 'utf8'));
    verifier('approbationsGardees', [].concat(reglages.codeApprouve || []).length >= 3, [].concat(reglages.codeApprouve || []).length);
    await n.plotlyPret(`${fait.nom} · ${await n.t('coefficients')}`, 1);
    // the plot its code drew (lattice's plot of an lme), in its view (the explorer's line under the model)
    await n.double(`(${n.ligne(await n.t('code_graphiques'), 4)}).querySelector('.libelle')`);
    const image = await n.attendre("(() => { const i = __n.tous('.image-code img')[0]; return i && i.complete && i.naturalWidth > 0 ? i.naturalWidth : 0; })()");
    verifier('graphiqueDuCode', image === 900, image);
    await capture('5-graphique');
  },
};
