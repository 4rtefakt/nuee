// Plugins: the two example plugins installed from their .nueeplugin files (docs/site/greffons) through
// File › Plugins…, Install from file… (the install dialog shows the plugin's card, the trust it asks
// for and its code); the occupancy example's data (docs/site/donnees) opened, Model › Plugins › Site
// occupancy…: the dialog its manifest describes, the four visits ticked as detection histories, forest
// for occupancy and noise for detection, Fit; the fit as the engine has it (the Coefficients view's
// points, the plugin's card in the Summary, its values and tables). Then the project saved, the plugin
// removed (File › Plugins… › Remove), and a copy of the project whose plugin a colleague changed
// (version 1.0.1) opened as someone else's: its model not fitted, the bar naming the plugin, its
// version and its author; Show (the colleague's code as it is), Install and run: installed from the
// project, fitted with it. With screenshots for review: <screenshots>-greffon-*.png.
const fs = require('fs');
const path = require('path');

const proches = (a, b, tol = 1e-9) => a.length === b.length && a.every((x, i) => Math.abs(x - b[i]) <= tol * Math.max(1, Math.abs(b[i])));
const NOM = 'Site occupancy (single season)';

module.exports = {
  async executer(n, verifier, { donnee, dossier, captures }) {
    const capture = async (nom) => {
      if (!captures) return;
      await n.attendre("[...document.querySelectorAll('.js-plotly-plot')].every((g) => !g._fullLayout || !g.offsetWidth || (Math.abs(g._fullLayout.width - g.offsetWidth) < 2 && Math.abs(g._fullLayout.height - g.offsetHeight) < 2))", 10000).catch(() => {});
      await n.capture(`${captures}-greffon-${nom}.png`);
    };
    const boite = '__n.boite()';
    const bouton = (cle) => `__n.tous('button', ${boite}).find((b) => __n.norm(b.textContent) === __n.norm(__n.t(${JSON.stringify(cle)})) && !b.disabled)`;

    // ---- File › Plugins…, Install from file…: each example read, shown with its code, installed
    await n.menu('m_fichier', 'greffons');
    await n.dialogue('greffons_titre');
    const installer = async (fichier, nom) => {
      await n.fichierAOuvrir(donnee('docs', 'site', 'greffons', fichier));
      await n.cliquer(bouton('greffon_installer_fichier'));
      await n.attendre(`__n.titreDialogue() === __n.norm(__n.t('greffon_installer_titre', { nom: ${JSON.stringify(nom)}, version: '1.0.0' }))`);
      const code = await n.attendre(`(() => { const p = ${boite}.querySelector('pre.code-r'); return p ? [...${boite}.querySelectorAll('pre.code-r')].map((x) => x.textContent).join('\\n') : null; })()`);
      const confiance = await n.evaluer(`__n.norm((${boite}.querySelector('.confiance-greffon') || {}).textContent)`);
      return { code, confiance };
    };
    const occ = await installer('occupancy.nueeplugin', NOM);
    verifier('dialogueInstallation', occ.code.includes('occupancy_fit <- function(donnees, entrees, options)') && occ.code.includes('"id": "occupancy"')
      && occ.confiance === await n.evaluer("__n.norm(__n.t('greffon_confiance'))"), occ.confiance.slice(0, 80));
    await capture('2-installer');
    await n.cliquer(bouton('greffon_installer'));
    await n.attendre(`__n.titreDialogue() === __n.norm(__n.t('greffons_titre')) && __n.tous('.liste-greffons .carte-greffon').length === 1`);
    await installer('mann-kendall.nueeplugin', 'Mann–Kendall trend test and Sen’s slope');
    await n.cliquer(bouton('greffon_installer'));
    const cartes = await n.attendre(`(() => { const c = __n.tous('.liste-greffons .carte-greffon b').map((b) => __n.norm(b.textContent)); return c.length === 2 ? c : null; })()`);
    const e = await n.api('greffons');
    verifier('deuxGreffonsInstalles', cartes.includes(NOM) && [].concat(e.liste).every((g) => g.etat === 'ok' && g.permis === true && g.source === 'utilisateur'), cartes);
    await capture('1-greffons');
    await n.bouton('fermer');
    await n.attendre('!__n.dialogue()');

    // ---- the occupancy data, Model › Plugins › Site occupancy…: the dialog its manifest describes
    await n.fichierAOuvrir(donnee('docs', 'site', 'donnees', 'woodpecker_occupancy.csv'));
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('woodpecker_occupancy.csv'));
    await n.fermerMenus();
    await n.cliquer(`__n.texte('.menu-haut', __n.t('m_modele'))`);
    await n.cliquer(`__n.tous('.menu:not(.sous-menu) > .item').find((e) => __n.norm((e.querySelector('.libelle') || e).textContent) === __n.norm(__n.t('greffons_menu')))`);
    await n.cliquer(`__n.tous('.menu.sous-menu > .item').find((e) => __n.norm((e.querySelector('.libelle') || e).textContent) === ${JSON.stringify(`${NOM}…`)})`);
    await n.attendre(`__n.titreDialogue().startsWith(${JSON.stringify(NOM)})`);
    const champs = await n.evaluer(`__n.tous('.grille-champs > label', ${boite}).map((l) => __n.norm(l.textContent))`);
    verifier('dialogueDuManifeste', champs.includes('Detection histories (one column per visit)') && champs.includes('Site covariates for occupancy (psi)')
      && champs.includes('Optimizer (optim)') && !(await n.evaluer(`__n.tous('.onglets button', ${boite}).some((b) => __n.norm(b.textContent) === __n.t('variables'))`)), champs);
    // (the variables of an input: its box of ticks, the k-th input's)
    const coche = (k, nom) => n.coche(`[...__n.tous('.champ-greffon .liste-variables', ${boite})[${k}].querySelectorAll('label')].find((l) => __n.norm(l.textContent) === ${JSON.stringify(nom)}).querySelector('input')`);
    for (const v of ['visit1', 'visit2', 'visit3', 'visit4']) await coche(0, v);
    await coche(1, 'forest');
    await coche(2, 'noise');
    const compte = await n.evaluer(`__n.norm(__n.tous('.champ-greffon', ${boite})[0].querySelector('.infos').textContent)`);
    verifier('variablesCochees', compte.startsWith(await n.t('greffon_n_choisies', { n: 4 })), compte);
    await capture('3-dialogue');
    const avant = (await n.etat()).modeles.map((m) => m.id);
    await n.bouton('ajuster');
    await n.attendre('!__n.dialogue()', 180000);
    const id = await n.attendre(`__n.api('etat').then((e) => { const m = [].concat(e.modeles).find((x) => !${JSON.stringify(avant)}.includes(x.id)); return m ? m.id : null; })`);
    const m = await n.api('modele', { modele: id });
    await n.attendre(n.ligneModele(m.nom, true));
    const termes = [].concat(m.coefficients.terme);
    verifier('modeleDuGreffon', m.type === 'PLUGIN' && m.N === 80 && termes.join() === 'psi.(Intercept),psi.forest,p.(Intercept),p.noise' && m.greffon.version === '1.0.0'
      && [].concat(m.valeurs.nom).includes('psi'), `${m.chiffres}; ${termes.join(', ')}`);

    // ---- its views: the coefficients (the engine's estimates), the summary with the plugin's card, the values
    const coefficients = `${m.nom} · ${await n.t('coefficients')}`;
    await n.plotlyPret(coefficients, 1);
    const traces = await n.evaluer(`[...__n.graphe(${JSON.stringify(coefficients)}).data[0].x]`);
    const estimations = [].concat(m.coefficients.estimation).filter((_, i) => !/\(Intercept\)$/.test(termes[i]));
    verifier('coefficientsCommeLeMoteur', proches(traces, estimations), traces);
    const carte = await n.attendre(`(() => { const c = __n.tous('.carte-greffon.modele')[0]; return c ? __n.norm(c.textContent) : null; })()`);
    verifier('carteDuGreffon', carte.includes(NOM) && carte.includes('Clément Boutin') && carte.includes('visit1, visit2, visit3, visit4') && carte.includes('forest'), carte.slice(0, 160));
    await capture('4-resultats');

    // ---- saved; the plugin removed; a copy whose plugin is a colleague's version opened as someone else's
    const fichier = path.join(dossier, 'occupancy.nuee');
    await n.fichierAEnregistrer(fichier);
    await n.menu('m_fichier', 'enregistrer');
    await n.attendre(`__n.statut().startsWith(__n.t('enregistre'))`);
    await n.menu('m_fichier', 'greffons');
    await n.dialogue('greffons_titre');
    await n.cliquer(`(() => { const c = __n.tous('.liste-greffons .carte-greffon').find((x) => __n.norm((x.querySelector('b') || {}).textContent) === ${JSON.stringify(NOM)});
      return c && __n.tous('button', c).find((b) => __n.norm(b.textContent) === __n.t('greffon_retirer')); })()`);
    await n.attendre(`__n.titreDialogue() === __n.norm(__n.t('greffon_retirer_titre', { nom: ${JSON.stringify(NOM)} }))`);
    await n.cliquer(bouton('greffon_retirer'));
    await n.attendre(`__n.titreDialogue() === __n.norm(__n.t('greffons_titre')) && __n.tous('.liste-greffons .carte-greffon').length === 1`);
    await n.bouton('fermer');
    await n.attendre('!__n.dialogue()');
    const j = JSON.parse(fs.readFileSync(fichier, 'utf8'));
    const CODE_COLLEGUE = '# a colleague\'s change: version 1.0.1\n';
    for (const d of j.models) {
      if (d.type !== 'PLUGIN') continue;
      for (const f of d.options.greffon.fichiers) {
        if (f.chemin === 'plugin.json') f.texte = f.texte.replace('"version": "1.0.0"', '"version": "1.0.1"');
        else f.texte = CODE_COLLEGUE + f.texte;
      }
    }
    const codeCollegue = j.models.find((d) => d.type === 'PLUGIN').options.greffon.fichiers.find((f) => f.chemin === 'occupancy.R').texte;
    const copie = path.join(dossier, 'occupancy-colleague.nuee');
    fs.writeFileSync(copie, JSON.stringify(j));
    await n.fichierAOuvrir(copie);
    await n.menu('m_fichier', 'ouvrir');
    await n.attendre(n.ligneProjet('occupancy-colleague.nuee'));
    const barre = await n.attendre("(() => { const b = document.querySelector('#barres-greffons .barre-greffon'); return b ? __n.norm(b.textContent) : null; })()");
    const et = await n.etat();
    const pc = et.projets.find((p) => p.nom === 'occupancy-colleague.nuee');
    verifier('barreDuGreffonVenuDAilleurs', barre.includes(NOM) && barre.includes('1.0.1') && barre.includes('Clément Boutin') && barre.includes(await n.t('greffon_installer_executer'))
      && !et.modeles.some((x) => x.projet === pc.id), barre);
    await capture('6-barre');

    // ---- Show: the colleague's code as it is; then Install and run
    await n.cliquer(`__n.tous('#barres-greffons button').find((b) => __n.norm(b.textContent) === __n.t('greffon_montrer'))`);
    await n.attendre(`__n.titreDialogue().startsWith(__n.t('greffon_montrer'))`);
    const montre = await n.attendre(`(() => { const s = __n.tous('.montrer-code section', ${boite}).find((x) => __n.norm((x.querySelector('h4') || {}).textContent) === 'occupancy.R');
      return s ? [...s.querySelectorAll('.ligne-code')].map((l) => l.textContent).join('\\n') : null; })()`);
    // (a blank line shows as a space: vues_code.js, codeNumerote)
    verifier('codeMontreTelQuel', montre === codeCollegue.replace(/\n$/, '').split('\n').map((l) => l || ' ').join('\n'), montre.slice(0, 80));
    await capture('5-montrer');
    await n.cliquer(bouton('greffon_installer_executer'));
    const fait = await n.attendre(`__n.api('etat').then((e) => [].concat(e.modeles).find((x) => x.projet === ${JSON.stringify(pc.id)} && x.type === 'PLUGIN') || null)`, 180000);
    await n.attendre("!document.querySelector('#barres-greffons .barre-greffon')");
    const apres = await n.api('greffons');
    verifier('installeEtExecute', fait.greffon.version === '1.0.1' && proches([].concat(fait.coefficients.estimation), [].concat(m.coefficients.estimation), 1e-10)
      && [].concat(apres.liste).some((g) => g.id === 'occupancy' && g.version === '1.0.1' && g.permis === true), fait.chiffres);
    const reglages = JSON.parse(fs.readFileSync(path.join(path.dirname(dossier), 'profil', 'reglages.json'), 'utf8'));
    verifier('approbationsGardees', [].concat(reglages.codeApprouve || []).includes(fait.greffon.empreinte), [].concat(reglages.codeApprouve || []).length);
  },
};
