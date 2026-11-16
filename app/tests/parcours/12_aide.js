// Help: Help › Example projects › Skylark monitoring opens the monitoring tutorial's two projects
// (simulated data), their models fitted from their sessions, TRIM's page in front with its Indices
// and their overall slope line, and the tutorial linked in the status bar; the window's title names
// the project in front, and follows the transects' page. F1 on TRIM's Indices opens the guide in a
// panel beside them, not a dialog, at TRIM's section and its paragraph on Indices; a search finds
// distance sampling's section, its words marked, while the menus still act on the project in front;
// F1 again goes back to what is in front. About gives the citation (copied in the page only, not to
// the computer's clipboard) and its links; Help › Tutorials and the example's tutorial open the
// documentation site (in the main process's record: no browser opens). Example projects › A weir
// removed opens the BACI tutorial's trout, its BACI view in front with the mixed model's effect.
module.exports = {
  async executer(n, verifier) {
    const norm = (s) => String(s || '').replace(/\s+/g, ' ').trim();
    const liens = async () => (await n.principal()).liens;
    // (a link given to the main process, which opens it in the browser: recorded there, pilote.js)
    const attendreLien = async (url) => { for (let i = 0; i < 100 && !(await liens()).includes(url); i += 1) await new Promise((r) => setTimeout(r, 100)); return (await liens()).includes(url); };
    const titreFenetre = () => n.evaluer('document.title');
    const aide = (f) => n.evaluer(`import('/aide_texte.js').then((a) => (${f})(a))`);

    // ---- Help › Example projects › Skylark monitoring: two projects, three models fitted
    await n.sousMenu('m_aide', 'projet_exemple', 'exemple_alouettes');
    const e = await n.attendre(`__n.api('etat').then((e) => [].concat(e.modeles).length === 3 && [].concat(e.projets).length === 2 ? e : null)`, 240000);
    const projets = [].concat(e.projets);
    const comptages = projets.find((p) => p.nom === 'skylark_counts.json');
    const transects = projets.find((p) => p.nom === 'skylark_transects.json');
    const types = [].concat(e.modeles).map((m) => m.type).sort();
    verifier('exemplesOuverts', !!comptages && !!transects && JSON.stringify(types) === JSON.stringify(['DISTANCE', 'GLM', 'TRIM'])
      && projets.every((p) => !p.chemin_enregistrement), `${projets.map((p) => p.nom).join(', ')}: ${types.join(', ')}`);
    const indices = `M1 · ${await n.t('trim_indices')}`;
    await n.plotlyPret(indices, 17);
    const droite = await n.evaluer(`__n.graphe(${JSON.stringify(indices)}).data.some((x) => x.name === __n.t('trim_droite'))`);
    verifier('indicesEtDroite', droite);
    await n.libre();
    verifier('titreDuProjetDevant', (await titreFenetre()) === 'Nuée — skylark_counts.json', await titreFenetre());

    // its tutorial, in the status bar, opened in the browser
    const tutoriel = await n.t('exemple_tutoriel', { titre: await n.t('tutoriel_suivi') });
    await n.cliquer(`__n.tous('#statut .lien-tutoriel a').find((a) => __n.norm(a.textContent) === ${JSON.stringify(tutoriel)})`);
    const adresse = await aide('(a) => a.adresseTutoriel("suivi", "en")');
    verifier('lienDuTutoriel', await attendreLien(adresse), (await liens()).join(', '));

    // ---- the window's title follows the project in front
    await n.cliquer("__n.onglet('M1 · DISTANCE')");
    await n.attendre(`document.title === 'Nuée — skylark_transects.json'`);
    verifier('titreSuit', (await titreFenetre()) === 'Nuée — skylark_transects.json', await titreFenetre());
    await n.cliquer("__n.onglet('M1 · TRIM')");
    await n.cliquer(`__n.onglet(${JSON.stringify(await n.t('trim_indices'))})`);
    await n.attendre(`document.title === 'Nuée — skylark_counts.json'`);

    // ---- F1 on TRIM's Indices: the guide beside them, at TRIM's section and its paragraph on Indices
    await n.touche('F1');
    const section = await n.attendre(`(() => { const h = document.querySelector('.aide-corps h3'); return h && __n.visible(h) ? __n.norm(h.textContent) : null; })()`);
    verifier('aideALaSection', section === 'TRIM: indices and trends of counts', section);
    const cible = await n.evaluer(`__n.norm((document.querySelector('.aide-corps .aide-cible') || {}).textContent)`);
    verifier('aideAuParagraphe', /\bIndices \(the imputed indices/.test(cible), cible.slice(0, 100));
    verifier('aideSansDialogue', !(await n.evaluer('!!__n.dialogue()')) && (await n.evaluer("!!document.querySelector('.dv-tab [data-panneau=\"aide:\"]') || !!document.querySelector('.aide-panneau')")));

    // a search: distance sampling's section first, its words marked; the menus still act on the counts
    await n.ecrire("document.querySelector('.aide-recherche')", 'distance sampling', { quitter: false });
    const premier = await n.attendre(`(() => { const a = document.querySelector('.aide-resultats li > a'); return a ? __n.norm(a.textContent) : null; })()`);
    verifier('rechercheTrouve', premier === 'Distance sampling: density and abundance', premier);
    await n.cliquer("document.querySelector('.aide-resultats li > a')");
    const marques = await n.attendre(`(() => { const h = document.querySelector('.aide-corps h3'); return h && __n.norm(h.textContent).startsWith('Distance sampling') ? document.querySelectorAll('.aide-corps mark').length : 0; })()`);
    verifier('motsMarques', marques > 3, marques);
    // (the keyboard in the guide's search field: the menus act on the view beside it, the counts')
    await n.evaluer("document.querySelector('.aide-recherche').focus(); true");
    verifier('menusSurLeProjetDevant', (await titreFenetre()) === 'Nuée — skylark_counts.json' && (await n.evaluer("document.activeElement === document.querySelector('.aide-recherche')")),
      await titreFenetre());
    // F1 again (the keyboard in the search field): what is in front, TRIM
    await n.touche('F1');
    const retour = await n.attendre(`(() => { const h = document.querySelector('.aide-corps h3'); return h && __n.norm(h.textContent).startsWith('TRIM') ? __n.norm(h.textContent) : null; })()`);
    verifier('f1DeNouveau', !!retour && (await n.evaluer("document.querySelector('.aide-recherche').value")) === '', retour);

    // ---- About: the citation, copied in the page only, and its links; Help › Tutorials
    await n.menu('m_aide', 'apropos');
    await n.dialogue('apropos');
    const attendue = await aide("async (a) => { const nv = await import('/nouveautes.js'); return a.citation(nv.VERSION, nv.NOUVEAUTES.find((x) => x.version === nv.VERSION).date); }");
    const ecrite = await n.attendre("(() => { const p = document.querySelector('.citation-nuee'); return p ? __n.norm(p.textContent) : null; })()");
    verifier('citation', ecrite === norm(attendue) && /10\.5281\/zenodo\.24263014/.test(ecrite), ecrite);
    await n.evaluer('navigator.clipboard.writeText = async (x) => { window.__copie = x; }; true');
    await n.bouton('apropos_copier');
    await n.attendre("window.__copie !== undefined");
    verifier('citationCopiee', (await n.evaluer('window.__copie')) === attendue);
    const probleme = await n.t('signaler_probleme');
    await n.cliquer(`__n.tous('.apropos a').find((a) => __n.norm(a.textContent) === ${JSON.stringify(probleme)})`);
    await n.cliquer(`__n.tous('.apropos a').find((a) => __n.norm(a.textContent) === 'nuee.io')`);
    verifier('liensAPropos', (await attendreLien('https://github.com/4rtefakt/nuee/issues/new/choose')) && (await attendreLien('https://nuee.io/')), (await liens()).join(', '));
    await n.bouton('fermer');
    await n.attendre('!__n.dialogue()');
    await n.menu('m_aide', 'tutoriels');
    verifier('tutoriels', await attendreLien('https://4rtefakt.github.io/nuee/en/tutorials/'), (await liens()).join(', '));

    // ---- Example projects › A weir removed: the BACI view in front, with the mixed model's effect
    await n.sousMenu('m_aide', 'projet_exemple', 'exemple_baci');
    await n.attendre(`__n.api('etat').then((e) => [].concat(e.modeles).some((m) => m.type === 'LMM'))`, 240000);
    const phrases = await n.attendre(`(() => { const b = __n.tous('.note b'); return b.length >= 2 && !document.querySelector('.infos.attente') ? b.map((x) => __n.norm(x.textContent)) : null; })()`, 120000);
    verifier('baciEtSonModele', /BACI effect \(period × impact\) = 6\.91 \[95% CI 3\.46, 10\.36\]/.test(phrases.join(' ')), phrases.join(' | ').slice(0, 300));
    verifier('titreBaci', (await titreFenetre()) === 'Nuée — baci_trout.json', await titreFenetre());
    const tutorielBaci = await n.t('exemple_tutoriel', { titre: await n.t('tutoriel_baci') });
    verifier('tutorielBaci', await n.evaluer(`__n.tous('#statut .lien-tutoriel a').some((a) => __n.norm(a.textContent) === ${JSON.stringify(tutorielBaci)})`));
  },
};
