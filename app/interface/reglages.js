// File › Settings… (on macOS also Nuée › Settings…): the R Nuée computes with, its own (the
// default, and the one recommended; on Linux, the computer's) or another R installed on this
// computer, through window.bureau.r (main.js, r-installes.js). The Rs found are listed, with
// Browse… for any other; the one chosen is checked by running it once (its version, the packages
// the engine loads), and its missing packages are installed into its user library, or the line
// installing them copied, to run in that R. A new choice restarts the engine as Help › Restart the
// R engine does, recovery copies made just before so that open projects reopen with their changes,
// or waits for the next start. The R an organisation sets (a policy) shows and can't be changed
// here; RSCRIPT, for developers, overrides both.
// Also: once, the message that the R chosen couldn't start and Nuée's own took over (main.js:
// demarrerMoteur), with the R in use for About.
import { t, ajouterTextes } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { bureau } from './api.js';

const rDuBureau = () => (bureau && bureau.r) || null;
// An element's content replaced, what is absent left out
const remplir = (parent, ...enfants) => parent.replaceChildren(...enfants.flat().filter((x) => x !== null && x !== undefined && x !== false));
const nomR = (version) => (version ? `R ${version}` : 'R');
const plusAncien = (v, min) => {
  const a = String(v).split('.').map(Number);
  const b = String(min).split('.').map(Number);
  for (let i = 0; i < 3; i += 1) if ((a[i] || 0) !== (b[i] || 0)) return (a[i] || 0) < (b[i] || 0);
  return false;
};
const VERSION_MINIMALE = '4.3.0';  // (r-installes.js)

export function installerReglages(app) {
  app.reglages = () => dialogueReglages(app);
  app.signalerRepliR = () => signalerRepli(app);
  // the R in use, in a few words, for About (null in a browser)
  app.texteR = async () => {
    const r = rDuBureau();
    const e = r ? await r.etat().catch(() => null) : null;
    const u = e && e.utilise;
    if (!u) return null;
    const propre = t(e.plateforme === 'linux' ? 'r_systeme' : 'r_propre');
    if (u.repli) return `${propre} (${t('r_repli_court', { dossier: u.repli.dossier || u.repli.chemin })})`;
    return u.type === 'nuee' ? propre : u.type === 'rscript' ? `RSCRIPT=${u.chemin}` : u.dossier;
  };
  if (bureau && bureau.surReglages) bureau.surReglages(() => app.reglages());
}

async function dialogueReglages(app) {
  const r = rDuBureau();
  if (!r) return;
  let etat;
  let trouves;
  try { [etat, trouves] = await Promise.all([r.etat(), r.trouver()]); } catch (e) { app.statut(e.message, true); return; }
  let versionEnService = null;  // (the engine's answer: it may be busy, so the dialog doesn't wait for it)
  const linux = etat.plateforme === 'linux';
  const meme = (a, b) => !!a && !!b && (etat.plateforme === 'win32' ? a.toLowerCase() === b.toLowerCase() : a === b);
  const utilise = etat.utilise || { type: 'nuee' };
  const autreEnService = utilise.type !== 'nuee' ? utilise.rscript : null;
  const impose = !!etat.organisation || !!etat.rscript;
  const propre = t(linux ? 'r_systeme' : 'r_propre');

  // Nuée's own, the Rs found, then those set (or browsed) that weren't found; one set but no longer
  // there shows so
  const entrees = [{ cle: 'nuee' }, ...trouves.map((x) => ({ cle: x.rscript, ...x }))];
  const ajouter = (x, version = null) => {
    if (!x) return null;
    const deja = x.rscript && entrees.find((e) => meme(e.cle, x.rscript));
    if (deja) return deja;
    const e = { cle: x.rscript || `absent:${x.chemin}`, rscript: x.rscript, dossier: x.dossier || x.chemin, version };
    entrees.push(e);
    return e;
  };
  const enService = ajouter(autreEnService ? utilise : null);
  const choisi = ajouter(etat.organisation || etat.choisi);
  let choix = choisi && (choisi.rscript || impose) ? choisi.cle : 'nuee';

  const verifications = new Map();  // Rscript -> what its check said
  let numero = 0;      // the check drawn: one that comes back for another choice is set aside
  let enCours = false;  // an installation
  const groupe = `r-nuee-${Date.now()}`;
  const liste = el('div', { class: 'liste-r', role: 'radiogroup', 'aria-label': t('r_titre') });
  const resultat = el('div', { class: 'resultat-r', 'aria-live': 'polite' });
  const message = el('p', { class: 'note', 'aria-live': 'polite' });
  const parcourir = el('button', { onclick: () => parcourirR() }, t('r_parcourir'));

  const versionDe = (e) => (verifications.get(e.cle) || {}).version || e.version;
  function etiquettes(e) {
    const propreR = e.cle === 'nuee';
    const v = versionDe(e);
    const enServiceIci = propreR ? utilise.type === 'nuee' : meme(e.rscript, autreEnService);
    const regle = propreR ? !etat.organisation && !etat.choisi : !!choisi && e === choisi;
    return [enServiceIci ? [t('r_en_service'), ''] : null,
      !enServiceIci && regle && !etat.rscript && (propreR || e.rscript) ? [t('r_au_prochain'), ''] : null,
      e === choisi && etat.organisation ? [t('r_organisation_etiquette'), ''] : null,
      !propreR && !e.rscript ? [t('r_absent'), 'alerte'] : null,
      !propreR && v && plusAncien(v, VERSION_MINIMALE) ? [t('r_trop_ancien'), 'alerte'] : null].filter(Boolean);
  }
  function dessinerListe() {
    const focus = liste.contains(document.activeElement);
    remplir(liste, entrees.map((e) => {
      const propreR = e.cle === 'nuee';
      const v = versionDe(e);
      const inutilisable = !propreR && (!e.rscript || (v && plusAncien(v, VERSION_MINIMALE)));
      const detail = propreR
        ? [linux ? etat.propre && etat.propre.rscript : t('r_recommande'), utilise.type === 'nuee' && versionEnService ? `R ${versionEnService}` : null].filter(Boolean).join(' · ')
        : e.dossier;
      const desactive = impose || enCours || inutilisable;
      return el('label', { class: `entree-r${desactive ? ' desactivee' : ''}` },
        el('input', { type: 'radio', name: groupe, value: e.cle, checked: choix === e.cle, disabled: desactive, onchange: () => { choix = e.cle; montrerResultat(); } }),
        el('span', { class: 'nom-r' }, el('b', {}, propreR ? propre : nomR(v)), detail ? el('span', { class: 'doux' }, detail) : null),
        etiquettes(e).map(([texte, genre]) => el('span', { class: `etiquette-r ${genre}` }, texte)));
    }));
    parcourir.disabled = enCours;
    if (focus) { const c = liste.querySelector('input:checked') || liste.querySelector('input:not([disabled])'); if (c) c.focus(); }
  }

  const etatLigne = (genre, texte) => el('p', { class: `etat-r ${genre}` }, icone(genre === 'ok' ? 'circle-check' : 'circle-alert'), el('span', {}, texte));
  const attente = (texte) => el('p', { class: 'attente-connexion' }, el('span', { class: 'point' }), texte);
  // the line installing the packages, to copy into that R
  const aCopier = (ligne) => el('div', { class: 'ligne-r' }, el('code', {}, ligne), el('button', { onclick: () => {
    navigator.clipboard.writeText(ligne).then(() => { message.textContent = t('r_copie'); }, () => {});
  } }, icone('copy'), t('r_copier')));
  function rendreVerification(e, v) {
    const nom = nomR(v.version || e.version);
    if (v.erreur === 'introuvable') return [etatLigne('alerte', t('r_absent_texte'))];
    if (v.erreur === 'delai') return [etatLigne('alerte', t('r_delai', { r: nom }))];
    if (v.erreur) return [etatLigne('alerte', t('r_erreur', { r: nom })), el('pre', { class: 'sortie-r' }, v.erreur)];
    if (v.ancien) return [etatLigne('alerte', t('r_trop_ancien_texte', { r: nom, min: VERSION_MINIMALE.replace(/\.0$/, '') }))];
    const notes = [v.rds ? t('r_note_rds') : null, v.sha256 ? t('r_note_sha256') : null].filter(Boolean).map((x) => el('p', { class: 'note' }, x));
    if (!v.manquants.length) return [etatLigne('ok', t('r_pret', { r: nom })), ...notes];
    return [etatLigne('alerte', t('r_manquants', { r: nom, paquets: v.manquants.join(', ') })),
      el('div', { class: 'ligne-r' }, el('button', { onclick: () => installer(e, v) }, t('r_installer')),
        el('span', { class: 'doux' }, t('r_installer_ou', { bibliotheque: v.bibliotheque || t('r_bibliotheque_utilisateur') }))),
      el('p', {}, t('r_ligne')), aCopier(v.ligne), ...notes];
  }

  // the one chosen, checked (once per dialog: again after an installation)
  async function montrerResultat() {
    const n = ++numero;
    message.textContent = '';
    const e = entrees.find((x) => x.cle === choix);
    majBouton();
    if (!e || e.cle === 'nuee') { remplir(resultat); return; }
    if (!e.rscript) { remplir(resultat, etatLigne('alerte', t('r_absent_texte'))); return; }
    let v = verifications.get(e.cle);
    if (!v) {
      remplir(resultat, attente(t('r_verification', { r: nomR(e.version) })));
      v = await r.verifier(e.rscript).catch((err) => ({ ok: false, erreur: err.message, manquants: [] }));
      verifications.set(e.cle, v);
      dessinerListe();
      if (n !== numero) return;
    }
    remplir(resultat, rendreVerification(e, v));
    majBouton();
  }

  async function installer(e, v) {
    enCours = true;
    dessinerListe();
    majBouton();
    const sortie = el('pre', { class: 'sortie-r' });
    remplir(resultat, attente(t('r_installation', { paquets: v.manquants.join(', '), bibliotheque: v.bibliotheque || t('r_bibliotheque_utilisateur') })), sortie);
    let fin;
    try {
      fin = await r.installer(e.rscript, v.manquants, (texte) => {
        sortie.textContent = (sortie.textContent + texte).slice(-20000);
        sortie.scrollTop = sortie.scrollHeight;
      });
    } catch (err) {
      fin = { ok: false, manquants: v.manquants, ligne: v.ligne, fin: err.message };
    }
    // (checked again: what loads now)
    const apres = await r.verifier(e.rscript).catch(() => null);
    enCours = false;
    if (apres) verifications.set(e.cle, apres);
    dessinerListe();
    if (choix !== e.cle) { majBouton(); return; }
    if (apres && !apres.erreur && !apres.manquants.length) {
      remplir(resultat, rendreVerification(e, apres));
    } else {
      const manquants = (apres && !apres.erreur ? apres.manquants : fin.manquants) || v.manquants;
      remplir(resultat, etatLigne('alerte', t('r_installation_echec', { paquets: manquants.join(', ') })), aCopier((apres && apres.ligne) || fin.ligne || v.ligne),
        fin.fin ? el('pre', { class: 'sortie-r' }, fin.fin) : null,
        el('div', { class: 'ligne-r' }, el('button', { onclick: () => { verifications.delete(e.cle); montrerResultat(); } }, t('r_reverifier'))));
    }
    majBouton();
  }

  // Browse…: an R's folder (or its bin folder), checked; then in the list, chosen
  async function parcourirR() {
    const dossier = await bureau.choisirDossier();
    if (!dossier) return;
    message.textContent = '';
    remplir(resultat, attente(t('r_verification', { r: 'R' })));
    const v = await r.verifier(dossier).catch((err) => ({ ok: false, erreur: err.message, manquants: [] }));
    if (!v.rscript) { remplir(resultat); message.textContent = t('r_parcourir_rien', { dossier }); return; }
    const e = ajouter({ rscript: v.rscript, dossier: v.dossier }, v.version);
    verifications.set(e.cle, v);
    choix = e.cle;
    dessinerListe();
    montrerResultat();
  }

  const d = dialogue({ titre: t('reglages_titre'), contenu: el('div', { class: 'dialogue-r' },
    el('h3', {}, t('r_titre')),
    el('p', {}, t(linux ? 'r_texte_linux' : 'r_texte')),
    etat.organisation ? el('p', { class: 'note' }, t('r_organisation')) : null,
    etat.rscript ? el('p', { class: 'note' }, t('r_rscript', { chemin: etat.rscript })) : null,
    utilise.repli ? el('p', { class: 'note' }, t('r_repli_en_service', { propre: t(linux ? 'r_systeme_phrase' : 'r_propre_phrase'),
      dossier: utilise.repli.dossier || utilise.repli.chemin })) : null,
    liste, impose ? null : el('div', { class: 'ligne-r' }, parcourir), resultat, message),
  // (set elsewhere: nothing to choose, only to look at, or to install the packages it lacks)
  boutons: impose ? [{ libelle: t('fermer'), principal: true, action: () => true }]
    : [{ libelle: t('annuler'), action: () => true }, { libelle: t('r_utiliser'), principal: true, action: () => { appliquer(); return true; } }] });
  const bouton = d.boite.querySelector('.pied-dialogue button.principal');
  // (usable: Nuée's own, or an R whose check passed; nothing to change while it is set elsewhere)
  function majBouton() {
    const e = entrees.find((x) => x.cle === choix);
    const v = e && verifications.get(e.cle);
    bouton.disabled = !impose && (enCours || !e || (e.cle !== 'nuee' && !(v && v.ok)));
  }

  // the choice kept; then the engine restarted on it (asked first when projects are open)
  async function appliquer() {
    if (impose) return;
    const e = entrees.find((x) => x.cle === choix);
    const propreR = e.cle === 'nuee';
    const dejaRegle = propreR ? !etat.choisi : !!etat.choisi && meme(e.rscript, etat.choisi.rscript);
    if (!dejaRegle) {
      try { await r.choisir(propreR ? null : e.rscript); } catch (err) { app.statut(err.message, true); return; }
    }
    if (propreR ? utilise.type === 'nuee' : meme(e.rscript, autreEnService)) return;  // (already the engine's)
    const nom = propreR ? propre : nomR(versionDe(e));
    if (!app.projets().length) { app.redemarrerMoteur({ copies: true }); return; }
    dialogue({ titre: t('r_redemarrer_titre'), contenu: el('p', {}, t('r_redemarrer_texte', { r: nom })),
      boutons: [{ libelle: t('r_plus_tard'), action: () => { app.statut(t('r_au_prochain_demarrage', { r: nom })); return true; } },
        { libelle: t('redemarrer'), principal: true, action: () => { app.redemarrerMoteur({ copies: true }); return true; } }] });
  }

  dessinerListe();
  montrerResultat();
  app.version().then((v) => {
    versionEnService = (/(\d+\.\d+\.\d+)/.exec(v.R || '') || [])[1] || null;
    if (enService && !enService.version) enService.version = versionEnService;
    dessinerListe();
  }).catch(() => {});
}

// The R chosen (or the organisation's) couldn't start, and Nuée's own took over: said once, by
// the first window that asks (main.js)
async function signalerRepli(app) {
  const r = rDuBureau();
  const repli = r ? await r.repli().catch(() => null) : null;
  if (!repli) return;
  const propre = t(repli.plateforme === 'linux' ? 'r_systeme_phrase' : 'r_propre_phrase');
  const dossier = repli.dossier || repli.chemin;
  dialogue({ titre: t('r_repli_titre'), contenu: el('div', { class: 'dialogue-r' },
    el('p', {}, t(repli.type === 'organisation' ? 'r_repli_organisation' : 'r_repli_texte', { dossier, propre })),
    repli.motif === 'absent' ? el('p', {}, t('r_absent_texte')) : [el('p', {}, t('r_repli_arret')), el('pre', { class: 'sortie-r' }, String(repli.motif || '').trim())]),
  boutons: [{ libelle: t('reglages'), action: () => { app.reglages(); return true; } }, { libelle: t('fermer'), principal: true, action: () => true }] });
}

ajouterTextes({
  reglages: 'Réglages…', reglages_titre: 'Réglages', r_titre: 'Le R utilisé par Nuée',
  r_texte: 'Nuée calcule avec son propre R, qui vient avec elle, avec tous les paquets qu’il lui faut. Elle peut utiliser à la place un R installé sur cet ordinateur : ses paquets et son .Renviron s’appliquent, pas votre .Rprofile, pour qu’il ne change pas les résultats.',
  r_texte_linux: 'Nuée calcule avec le R de l’ordinateur, trouvé dans le PATH. Elle peut utiliser à la place un autre R installé dessus : ses paquets et son .Renviron s’appliquent, pas votre .Rprofile, pour qu’il ne change pas les résultats.',
  r_propre: 'Le R de Nuée', r_systeme: 'Le R de l’ordinateur', r_recommande: 'recommandé', r_propre_phrase: 'son propre R', r_systeme_phrase: 'le R de l’ordinateur',
  r_en_service: 'utilisé', r_au_prochain: 'au prochain démarrage', r_organisation_etiquette: 'choisi par votre organisation', r_absent: 'introuvable', r_trop_ancien: 'trop ancien',
  r_parcourir: 'Parcourir…', r_parcourir_rien: 'Pas de R dans {dossier} : choisissez le dossier d’un R (celui qui contient son dossier bin), ou celui de son Rscript.',
  r_verification: 'Vérification de {r}…', r_pret: '{r} a tous les paquets dont le moteur a besoin.',
  r_manquants: 'Il manque à {r} des paquets dont le moteur a besoin : {paquets}.',
  r_installer: 'Les installer', r_installer_ou: 'dans {bibliotheque}, depuis le CRAN (quelques minutes)', r_bibliotheque_utilisateur: 'la bibliothèque de l’utilisateur de ce R',
  r_ligne: 'Ou lancez cette ligne dans ce R (R ou RStudio) :', r_copier: 'Copier', r_copie: 'Ligne copiée.',
  r_installation: 'Installation de {paquets} dans {bibliotheque}, depuis le CRAN…',
  r_installation_echec: 'Pas installés : {paquets}. Lancez cette ligne dans ce R (R ou RStudio), puis vérifiez de nouveau :', r_reverifier: 'Vérifier de nouveau',
  r_trop_ancien_texte: '{r} est trop ancien : Nuée demande R {min} ou plus récent.',
  r_note_rds: 'Avec un R antérieur à 4.4, les fichiers .rds et .RData ne s’ouvrent pas (en lire un pourrait exécuter du code) : enregistrez le tableau en CSV ou en Excel.',
  r_note_sha256: 'Avec un R antérieur à 4.5, les empreintes des fichiers de données (Édition › Étapes des données) sont des MD5 et non des SHA-256.',
  r_absent_texte: 'Il n’y a plus de R à cet endroit.', r_delai: '{r} n’a pas répondu en une minute.', r_erreur: '{r} n’a pas répondu comme R le fait :',
  r_organisation: 'Votre organisation choisit le R qu’utilise Nuée sur cet ordinateur.',
  r_rscript: 'RSCRIPT est défini ({chemin}) : c’est lui qui décide du R qu’utilise Nuée, quel que soit ce réglage.',
  r_repli_en_service: 'Nuée calcule en ce moment avec {propre}, car {dossier} n’a pas pu démarrer.', r_repli_court: '{dossier} n’a pas pu démarrer',
  r_utiliser: 'Utiliser ce R', r_redemarrer_titre: 'Redémarrer le moteur R ?',
  r_redemarrer_texte: 'Nuée calcule avec {r} une fois son moteur R redémarré. Les projets ouverts se rouvrent avec leurs modèles, leurs vues et leurs changements.',
  r_plus_tard: 'Au prochain démarrage', r_au_prochain_demarrage: 'Nuée utilisera {r} à partir de son prochain démarrage.',
  r_repli_titre: 'Le R choisi n’a pas pu démarrer',
  r_repli_texte: 'Le R choisi dans Fichier › Réglages… ({dossier}) n’a pas pu démarrer : Nuée calcule avec {propre}. Réglages… permet d’en choisir un autre.',
  r_repli_organisation: 'Le R que votre organisation choisit pour Nuée ({dossier}) n’a pas pu démarrer : Nuée calcule avec {propre}. Votre service informatique peut en choisir un autre.',
  r_repli_arret: 'Il s’est arrêté avant d’être prêt, en disant :',
}, {
  reglages: 'Settings…', reglages_titre: 'Settings', r_titre: 'R used by Nuée',
  r_texte: 'Nuée computes with its own R, which comes with it, with every package it needs. It can use an R installed on this computer instead: that R’s packages and .Renviron apply, not your .Rprofile, so that it can’t change the results.',
  r_texte_linux: 'Nuée computes with the computer’s R, found on the PATH. It can use another R installed on it instead: that R’s packages and .Renviron apply, not your .Rprofile, so that it can’t change the results.',
  r_propre: 'Nuée’s own R', r_systeme: 'The computer’s R', r_recommande: 'recommended', r_propre_phrase: 'its own R', r_systeme_phrase: 'the computer’s R',
  r_en_service: 'in use', r_au_prochain: 'from the next start', r_organisation_etiquette: 'set by your organisation', r_absent: 'not found', r_trop_ancien: 'too old',
  r_parcourir: 'Browse…', r_parcourir_rien: 'No R in {dossier}: choose the folder of an R (the one holding its bin folder), or that of its Rscript.',
  r_verification: 'Checking {r}…', r_pret: '{r} has every package the engine needs.',
  r_manquants: '{r} lacks packages the engine needs: {paquets}.',
  r_installer: 'Install them', r_installer_ou: 'into {bibliotheque}, from CRAN (a few minutes)', r_bibliotheque_utilisateur: 'that R’s user library',
  r_ligne: 'Or run this line in that R (R or RStudio):', r_copier: 'Copy', r_copie: 'Line copied.',
  r_installation: 'Installing {paquets} into {bibliotheque}, from CRAN…',
  r_installation_echec: 'Not installed: {paquets}. Run this line in that R (R or RStudio), then check again:', r_reverifier: 'Check again',
  r_trop_ancien_texte: '{r} is too old: Nuée needs R {min} or newer.',
  r_note_rds: 'With an R older than 4.4, .rds and .RData files don’t open (reading one could run code): save the table as CSV or Excel.',
  r_note_sha256: 'With an R older than 4.5, data files are fingerprinted with MD5 rather than SHA-256 (Edit › Data steps).',
  r_absent_texte: 'There is no R there any more.', r_delai: '{r} didn’t answer within a minute.', r_erreur: '{r} didn’t answer as R does:',
  r_organisation: 'Your organisation sets the R Nuée uses on this computer.',
  r_rscript: 'RSCRIPT is set ({chemin}): it decides which R Nuée uses, whatever this setting.',
  r_repli_en_service: 'Nuée computes with {propre} for now, as {dossier} couldn’t start.', r_repli_court: '{dossier} couldn’t start',
  r_utiliser: 'Use this R', r_redemarrer_titre: 'Restart the R engine?',
  r_redemarrer_texte: 'Nuée computes with {r} once its R engine restarts. Open projects reopen with their models, views and changes.',
  r_plus_tard: 'At the next start', r_au_prochain_demarrage: 'Nuée will use {r} from its next start.',
  r_repli_titre: 'The R chosen couldn’t start',
  r_repli_texte: 'The R chosen in File › Settings… ({dossier}) couldn’t start, so Nuée computes with {propre}. Settings… lets you choose another one.',
  r_repli_organisation: 'The R your organisation sets for Nuée ({dossier}) couldn’t start, so Nuée computes with {propre}. Your IT department can set another one.',
  r_repli_arret: 'It stopped before it was ready, saying:',
});
