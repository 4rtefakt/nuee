// What changed: the key results of a project's models on two sides (an old version and the project
// now, File › Versions: versions.js), compared by the engine (moteur/R/resultats_cles.R: requests
// resultats_cles and comparer_resultats) and shown in a panel: the models with notable changes
// first, their sentences, a table of the results that changed, then the models of one side only.
//   resumerProjet(app, pid, { surProgression })      → { modeles: [{ nom, numero, type, entrees }] }
//   afficherChangements(app, { titre, pid, avant, apres, donnees, documents, paquet })   the panel, and what it shows
//   bilanPaquet(c)                                   a package's rerun counted: rerun, reproduced, differing, not rerun
// Both summaries and their comparison are asked in the interface's language: an entry's label comes
// in the language of its resultats_cles request. After an update of the data (etapes.js), the panel
// also shows what the update did to the data (`donnees`) and the values cited in the project's
// documents that changed (`documents`). A reproducibility package rerun (relancer.js: `paquet`, what
// the engine said of it) compares its deposited results with the models fitted again: the panel
// says first how many give the deposited results, which Nuée and R made the deposit and which reran
// it, and what its manifest found, then each model as for two versions, the deposited side before.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, entreNombres } from './ui.js';
import { demander } from './api.js';
import { VUES } from './panneaux.js';
import { valeurResultat, texteP, ecartResultat, drapeauxEntree, apparierModeles, ordonnerComparaisons, entreesChangees } from './versions_texte.js';

// The comparisons made in this window, by id (C1, C2…): a panel shows one, and finds it again when
// it comes back from a window of its own. Not kept with the session: they are made again.
const COMPARAISONS = new Map();
const GARDEES = 12;
let compteur = 0;
// (a panel moved to its own window asks the window that made its comparison for it)
const canal = typeof BroadcastChannel === 'function' ? new BroadcastChannel('nuee-changements') : null;
const demandees = new Set();

// The table of a model's changed results shows its first rows; the others on request (thousands
// of loadings for spectra).
const LIGNES = 60;

// Every saved model of the project, its key results: from its latest fit in the session, or fitted
// quietly (app.calculerDiscret: no views open). A project the interface doesn't show (an old
// version opened in the engine only) has none in the session: its models are fitted with the
// number of components its saved session had. A model that can't be fitted is kept with its error.
// arrete(): true once Stop was pressed (etapes.js, its progress dialog): no more models; Stop
// pressed in the status bar during a fit, the request's refusal, goes on to the caller.
export async function resumerProjet(app, pid, { surProgression, arrete = null } = {}) {
  const montre = !!app.projet(pid);
  const p = app.projet(pid) || await demander('projet', { projet: pid });
  const definitions = [].concat(p.modeles || []).filter((d) => d.ajuste);
  const A = new Map();
  if (!montre && p.session) {
    try {
      for (const m of [].concat(JSON.parse((await demander('session', { projet: pid })).session).modeles || [])) A.set(m.numero, m.A);
    } catch { /* the definitions' own */ }
  }
  const numero = (id) => Number(String(id).replace(/\D/g, '')) || 0;
  const modeles = [];
  for (const [k, d] of definitions.entries()) {
    if (arrete && arrete()) break;
    if (surProgression) surProgression({ fait: k, total: definitions.length, nom: d.nom });
    try {
      const calcules = montre ? app.modelesDe(pid).filter((m) => m.numero === d.numero).sort((a, b) => numero(b.id) - numero(a.id)) : [];
      const id = calcules.length ? calcules[0].id : await app.calculerDiscret(pid, d.numero, A.get(d.numero));
      const r = await demander('resultats_cles', { modele: id });
      modeles.push({ nom: d.nom, numero: d.numero, type: d.type, entrees: [].concat(r.entrees || []) });
    } catch (e) {
      if (arrete && e.reponse && e.reponse.arrete) throw e;
      modeles.push({ nom: d.nom, numero: d.numero, type: d.type, entrees: null, erreur: e.message });
    }
  }
  if (surProgression) surProgression({ fait: definitions.length, total: definitions.length });
  return { modeles };
}

// The comparison of two summaries (resumerProjet), model by model, shown in a panel of the project
// `pid`, titled `titre`; it opens at once and fills in as the engine answers. Resolves to what it
// shows: { id, titre, pid, modeles: [{ nom, numero, type, comparaison | erreur }], avantSeul, apresSeul,
// donnees, documents, paquet }. After an update of the data (etapes.js): `donnees`, its report as
// etapes_texte.js reads it ({ resume, rien, sections, fichier }); `documents`, the documents whose cited
// values changed ([{ id, titre, valeurs: [{ debut, fin, brut, avant, apres, phrase, perime }] }]). A
// package rerun (relancer.js): `paquet`, { titre, doi, fichier, manifeste, depose, ici, paquets, lien };
// a model then not fitted again here, or without deposited results, keeps why (`relance`).
export async function afficherChangements(app, { titre, pid, avant, apres, donnees = null, documents = null, paquet = null }) {
  const id = `C${++compteur}-${Date.now().toString(36)}`;
  const c = { id, titre, pid, enCours: true, modeles: [], avantSeul: [], apresSeul: [], donnees, documents, paquet };
  COMPARAISONS.set(id, c);
  while (COMPARAISONS.size > GARDEES) COMPARAISONS.delete(COMPARAISONS.keys().next().value);
  app.ouvrirVue('changements', null, { comparaison: id }, pid);
  const { deux, avantSeul, apresSeul } = apparierModeles(avant, apres);
  Object.assign(c, { avantSeul, apresSeul });
  const langue = langueActuelle();
  for (const [a, b] of deux) {
    const m = { nom: b.nom, numero: b.numero, type: b.type };
    // (a package: a model not fitted again here, the browser without its package, its code not run yet;
    // or one the package couldn't give results for: neither is a difference)
    if (paquet && (!a.entrees || !b.entrees)) {
      m.relance = b.entrees ? 'sans_depot' : 'non_relance';
      m.erreur = t(`relance_${m.relance}`, { nom: b.nom, message: (b.entrees ? a.erreur : b.erreur) || '' });
    } else if (!a.entrees || !b.entrees) m.erreur = t('changements_erreur', { nom: b.nom, message: a.erreur || b.erreur });
    else {
      try {
        m.comparaison = await demander('comparer_resultats', { avant: { entrees: a.entrees }, apres: { entrees: b.entrees }, langue });
      } catch (e) { m.erreur = t('changements_non_compare', { nom: b.nom, message: e.message }); }
    }
    c.modeles.push(m);
  }
  c.enCours = false;
  redessiner(id);
  return c;
}

export function installerChangements(app) {
  VUES.changements = {
    rendre: (conteneur, ctx, params, loc) => rendreChangements(app, conteneur, loc),
    titre: (_, p, loc = {}) => {
      const c = COMPARAISONS.get(loc.comparaison);
      return c ? titreComparaison(c) : t('changements');
    },
    projet: true,
    suffixe: (loc) => loc.comparaison || '',
    // (app.js: collecterSession leaves it out of the session, which can't make it again)
    passager: true,
  };
  if (canal) {
    canal.onmessage = ({ data }) => {
      const c = COMPARAISONS.get(data.id);
      if (data.type === 'demande' && c && !c.enCours) canal.postMessage({ type: 'comparaison', id: data.id, c });
      else if (data.type === 'comparaison' && demandees.has(data.id) && !c) {
        COMPARAISONS.set(data.id, data.c);
        redessiner(data.id);
      }
    };
  }
}

// The panels of this window that show comparison `id`, drawn again.
function redessiner(id) {
  for (const p of document.querySelectorAll('[data-comparaison]')) if (p.dataset.comparaison === id && p._redessiner) p._redessiner();
}

function rendreChangements(app, conteneur, loc) {
  conteneur.dataset.comparaison = loc.comparaison || '';
  conteneur._redessiner = () => rendreChangements(app, conteneur, loc);
  const c = COMPARAISONS.get(loc.comparaison);
  if (!c) {
    // (in a window of its own: the one that made it may still have it)
    if (canal && loc.comparaison && !demandees.has(loc.comparaison)) {
      demandees.add(loc.comparaison);
      canal.postMessage({ type: 'demande', id: loc.comparaison });
      setTimeout(() => redessiner(loc.comparaison), 1500);
      conteneur.replaceChildren(el('div', { class: 'changements' }, el('p', { class: 'infos attente' }, t('changements_calcul'))));
      return;
    }
    conteneur.replaceChildren(el('div', { class: 'changements' }, el('p', { class: 'note attente' }, t('changements_pas_la'))));
    return;
  }
  if (c.enCours) {
    conteneur.replaceChildren(el('div', { class: 'changements' }, el('h2', {}, titreComparaison(c)), el('p', { class: 'infos attente' }, t('changements_calcul')),
      el('div', { class: 'defile' }, sectionsMiseAJour(app, c))));
    return;
  }
  loc.ouverts = loc.ouverts || {};
  const tries = ordonnerComparaisons(c.modeles);
  const notables = tries.filter((m) => m.comparaison && [].concat(m.comparaison.phrases || []).length).length;
  const compares = tries.filter((m) => m.comparaison).length;
  // (a package: the deposited side and the side rerun, rather than before and after)
  const cotes = c.paquet ? ['relance_depose', 'relance_relance'] : ['changements_avant', 'changements_apres'];
  const corps = el('div', { class: 'defile' },
    c.paquet ? sectionPaquet(c) : sectionsMiseAJour(app, c),
    !c.modeles.length && !c.avantSeul.length && !c.apresSeul.length ? el('p', { class: 'note' }, t('changements_aucun_modele')) : null,
    tries.map((m) => sectionModele(m, loc, () => rendreChangements(app, conteneur, loc), c.paquet ? { cotes, inchange: 'relance_identique' } : { cotes })),
    c.avantSeul.length || c.apresSeul.length ? el('section', { class: 'modele-change seul-cote' },
      c.avantSeul.length ? el('p', {}, el('b', {}, t(c.paquet ? 'relance_depose_seul' : 'changements_avant_seul')), ' ', listeNoms(c.avantSeul)) : null,
      c.apresSeul.length ? el('p', {}, el('b', {}, t(c.paquet ? 'relance_relance_seul' : 'changements_apres_seul')), ' ', listeNoms(c.apresSeul)) : null) : null);
  conteneur.replaceChildren(el('div', { class: 'changements' },
    el('h2', {}, titreComparaison(c)),
    el('p', { class: 'infos' }, compares && !c.paquet ? t('changements_resume', { n: compares, notables }) : '',
      compares && (!c.paquet || bilanPaquet(c).differents.length) ? el('span', { class: 'aide-drapeaux', title: t('changements_d_aide') }, icone('circle-help')) : null),
    corps));
}

const titreComparaison = (c) => (c.paquet ? t('relance_titre_panneau', { titre: c.titre }) : t('changements_titre', { titre: c.titre }));

// A package's rerun counted (afficherChangements with `paquet`): the models rerun, those whose every
// deposited value comes back (within comparer_resultats's tolerance: an entry only the rerun has is
// no difference, nothing was deposited for it), those that differ, those not fitted again here, those
// without deposited results.
export function bilanPaquet(c) {
  const b = { relances: 0, identiques: 0, differents: [], nonRelances: [], sansDepot: [] };
  for (const m of c.modeles) {
    if (m.comparaison) {
      b.relances += 1;
      if ([].concat(m.comparaison.entrees || []).some((e) => e.change && !e.nouveau)) b.differents.push(m); else b.identiques += 1;
    } else if (m.relance === 'sans_depot') b.sansDepot.push(m);
    else b.nonRelances.push(m);
  }
  return b;
}

// The package's sentence: "4 models rerun: 4 give the deposited results, 0 differ." (French counts 0
// and 1 in the singular, English 1 only)
export function phraseBilan(b) {
  const un = (n) => (langueActuelle() === 'fr' ? n <= 1 : n === 1);
  const n = (cle, x) => t(`${cle}_${un(x) ? 'un' : 'n'}`, { n: x });
  return t('relance_bilan', { relances: n('relance_n_relances', b.relances), identiques: n('relance_n_identiques', b.identiques), differents: n('relance_n_differents', b.differents.length) });
}

// What a package's manifest found (paquet_ouvrir's manifeste), in a sentence: its files the same,
// or those missing, changed or added; or no manifest to check them by.
export function phraseManifeste(m = {}) {
  if (!m.present) return t('relance_sans_manifeste');
  if (m.intact) return t('relance_manifeste_intact', { n: m.fichiers });
  const liste = (cle, l) => ([].concat(l || []).length ? t(cle, { fichiers: [].concat(l).join(', ') }) : null);
  const ecarts = [liste('relance_manifeste_changes', m.changes), liste('relance_manifeste_manquants', m.manquants), liste('relance_manifeste_ajoutes', m.ajoutes),
    m.illisibles ? t('relance_manifeste_illisibles', { n: m.illisibles }) : null].filter(Boolean);
  return `${t('relance_manifeste_differe')} ${ecarts.join(langueActuelle() === 'fr' ? ' ; ' : '; ')}.`;
}

// What the engine said of the package: the sentence, the models not fitted again here or without
// deposited results, which Nuée and R made the deposit and which reran it (and the R packages in
// another version here), what its manifest found, where it came from.
function sectionPaquet(c) {
  const p = c.paquet;
  const b = bilanPaquet(c);
  const noms = (l) => l.map((m) => m.nom).join(', ');
  const d = p.depose || {};
  const ici = p.ici || {};
  const v = (x) => t('relance_version', { nuee: x.nuee || '?', R: x.R || '?', plateforme: x.plateforme || '?' });
  const m = p.manifeste || {};
  const paquets = [].concat(p.paquets || []);
  return el('section', { class: 'modele-change paquet-relance' },
    el('h3', {}, icone('package-open'), t('relance_section'), p.doi ? el('span', { class: 'doux compte-change' }, `DOI ${p.doi}`) : null),
    el('p', { class: `bilan-relance${b.differents.length ? ' attention' : ''}` }, phraseBilan(b)),
    b.nonRelances.length ? el('p', { class: 'attention' }, t('relance_non_relances', { noms: noms(b.nonRelances) })) : null,
    b.sansDepot.length ? el('p', { class: 'doux' }, t('relance_sans_depots', { noms: noms(b.sansDepot) })) : null,
    el('p', { class: 'doux versions-relance' }, t('relance_versions', { depot: v(d), date: d.date || '?', ici: v(ici) })),
    paquets.length ? el('p', { class: 'doux' }, t('relance_paquets', { liste: paquets.slice(0, 8).map((x) => `${x.paquet} ${x.depose} → ${x.ici}`).join(', ') + (paquets.length > 8 ? '…' : '') })) : null,
    el('p', { class: `manifeste-relance${m.intact ? '' : ' attention'}` }, phraseManifeste(m)),
    p.lien ? el('p', { class: 'doux' }, p.lien.concept ? t('relance_lien_concept', { record: p.lien.record, doi: p.lien.doi, concept: p.lien.concept })
      : t('relance_lien', { record: p.lien.record, doi: p.lien.doi || '' })) : null);
}

const listeNoms = (modeles) => modeles.map((m, i) => [i ? ', ' : '', m.nom, m.type ? el('span', { class: 'doux' }, ` (${m.type})`) : null]);

// An update's report in sections, as etapes_texte.js reads it (lireRapport): each a heading and its
// lines. Also the preview of Update the data (etapes.js).
const ICONES_SECTIONS = { nouveau: 'circle-plus', absent: 'circle-minus', change: 'pencil', etapes: 'list', modeles: 'box', lecture: 'file-input' };
export function sectionsRapport(lu) {
  return el('div', { class: 'sections-maj' }, [].concat(lu.sections || []).map((s) => el('section', { class: `section-maj ${s.cle}` },
    el('h4', {}, icone(ICONES_SECTIONS[s.cle] || 'info'), s.titre),
    el('ul', {}, [].concat(s.lignes || []).map((l) => el('li', { class: l.classe || '' }, l.texte))))));
}

// After an update of the data, before the models: what it did to the data, then the values cited
// in the documents that changed, each with its sentence; a click opens the document there.
function sectionsMiseAJour(app, c) {
  const d = c.donnees;
  const docs = [].concat(c.documents || []);
  return [
    d ? el('section', { class: 'modele-change donnees-change' }, el('h3', {}, icone('database'), t('changements_donnees')),
      d.fichier ? el('p', { class: 'doux fichier-lu' }, t('changements_fichier', { chemin: d.fichier })) : null,
      el('p', { class: `resume-maj${d.rien ? ' rien' : ''}` }, d.resume), sectionsRapport(d)) : null,
    docs.length ? el('section', { class: 'modele-change documents-change' },
      el('h3', {}, icone('file-text'), t('changements_documents'), el('span', { class: 'doux compte-change' },
        t('changements_documents_compte', { n: docs.reduce((n, x) => n + x.valeurs.length, 0) }))),
      docs.map((x) => el('div', { class: 'document-change' }, el('h4', {}, x.titre || t('document_sans_titre')),
        el('ul', { class: 'valeurs-change' }, x.valeurs.map((v) => el('li', {},
          el('button', { class: 'valeur-change', title: t('changements_ouvrir_document'), onclick: () => app.montrerDansDocument(c.pid, x.id, v) },
            el('span', { class: 'avant-apres' }, v.perime ? [el('b', {}, v.avant), ' ', el('span', { class: 'perime' }, t('changements_valeur_perimee', { nom: v.modele }))]
              : [el('s', {}, v.avant || '—'), ' → ', el('b', {}, v.apres || '—')]),
            el('span', { class: 'phrase-valeur' }, entreGuillemets(v.phrase.avant, el('mark', {}, v.perime ? v.avant : v.apres), v.phrase.apres))))))))) : null,
  ];
}
const entreGuillemets = (...x) => (langueActuelle() === 'fr' ? ['« ', ...x, ' »'] : ['“', ...x, '”']);

// A model's part: its sentences, then the table of what changed (shown when a change is notable,
// folded otherwise), or what kept it from being compared. `cotes`: the keys of the two columns'
// names; `inchange`: what to say of a model whose results didn't change.
function sectionModele(m, loc, redessinerPanneau, { cotes, inchange = 'changements_inchange' }) {
  const tete = el('h3', {}, m.nom, m.type ? el('span', { class: 'badge' }, m.type) : null);
  if (!m.comparaison) return el('section', { class: `modele-change erreur-change${m.relance ? ` ${m.relance}` : ''}` }, tete, el('p', { class: m.relance ? 'doux' : 'erreur' }, m.erreur || ''));
  const r = m.comparaison;
  const phrases = [].concat(r.phrases || []);
  const changees = entreesChangees(r);
  const total = (r.resume && r.resume.entrees) || [].concat(r.entrees || []).length;
  if (!changees.length) return el('section', { class: 'modele-change inchange' }, tete, el('p', { class: 'doux' }, t(inchange)));
  tete.append(el('span', { class: 'doux compte-change' }, t('changements_entrees', { n: changees.length, total })));
  const tout = !!loc.ouverts[m.nom];
  const lignes = tout ? changees : changees.slice(0, LIGNES);
  const tableau = el('table', { class: 'tableau compact tableau-changements' },
    el('thead', {}, el('tr', {}, [t('changements_resultat'), t(cotes[0]), t(cotes[1]), t('changements_ecart'), ''].map((h) => el('th', {}, h)))),
    el('tbody', {}, lignes.map(ligneEntree)));
  const details = el('details', { open: phrases.length ? true : undefined },
    el('summary', {}, t('changements_entrees', { n: changees.length, total })),
    el('div', { class: 'tableau-court' }, tableau),
    changees.length > lignes.length ? el('button', { onclick: () => { loc.ouverts[m.nom] = true; redessinerPanneau(); } }, t('changements_tout', { n: changees.length })) : null);
  return el('section', { class: `modele-change${phrases.length ? ' notable' : ''}` }, tete,
    phrases.length ? el('ul', { class: 'phrases-change' }, phrases.map((f) => el('li', {}, f.texte))) : el('p', { class: 'doux' }, t('changements_peu')),
    details);
}

// A group's name (resultats_cles.R: entree_resultat), or the engine's word for a group it adds.
const nomGroupe = (g) => { const x = t(`changements_g_${g}`); return x === `changements_g_${g}` ? g : x; };

// A changed result: what it is (and its group), before, after, the change, its marks.
function ligneEntree(e) {
  const cote = (valeur, ic, p, classe) => el('td', { class: 'num' }, [
    valeur !== null && valeur !== undefined ? valeurResultat(valeur, e.forme) : '',
    classe ? el('span', { class: 'categorie-change' }, ` ${classe}`) : null,
    ic && [].concat(ic).length === 2 ? el('span', { class: 'doux' }, ` [${valeurResultat(ic[0], e.forme)}${entreNombres()}${valeurResultat(ic[1], e.forme)}]`) : null,
    p !== null && p !== undefined ? el('span', { class: 'doux' }, ` ${texteP(p)}`) : null]);
  return el('tr', { class: e.phrase ? 'notable' : '' },
    el('td', {}, e.libelle || e.cle, e.groupe ? el('span', { class: 'doux' }, ` · ${nomGroupe(e.groupe)}`) : null),
    cote(e.avant, e.ic_avant, e.p_avant, e.classe_texte_avant || e.classe_avant),
    cote(e.apres, e.ic_apres, e.p_apres, e.classe_texte_apres || e.classe_apres),
    el('td', { class: 'num' }, ecartResultat(e)),
    el('td', {}, drapeauxEntree(e).map((d) => el('span', { class: `badge drapeau-change ${d}` }, t(`changements_d_${d}`)))));
}

// (the parts an update of the data adds: sectionsMiseAJour)
ajouterTextes({
  changements_donnees: 'Les données', changements_fichier: 'Lues dans {chemin}',
  changements_documents: 'Les valeurs citées dans les documents', changements_documents_compte: '{n} changée(s)',
  changements_ouvrir_document: 'Ouvrir le document à cet endroit',
  changements_valeur_perimee: 'pas mise à jour : {nom} n’a pas pu être ajusté à nouveau',
  // (a package rerun: relancer.js)
  relance_titre_panneau: 'Paquet relancé : {titre}', relance_section: 'Le paquet de reproductibilité',
  relance_bilan: '{relances} : {identiques}, {differents}.',
  relance_n_relances_un: '{n} modèle relancé', relance_n_relances_n: '{n} modèles relancés',
  relance_n_identiques_un: '{n} redonne les résultats déposés', relance_n_identiques_n: '{n} redonnent les résultats déposés',
  relance_n_differents_un: '{n} diffère', relance_n_differents_n: '{n} diffèrent',
  relance_non_relances: 'Pas ajustés de nouveau ici : {noms}.', relance_sans_depots: 'Sans résultats déposés : {noms}.',
  relance_non_relance: '{nom} n’a pas pu être ajusté de nouveau ici : {message}', relance_sans_depot: 'Le paquet n’a pas de résultats de {nom} : {message}',
  relance_identique: 'Il redonne les résultats déposés.', relance_depose: 'Déposé', relance_relance: 'Relancé',
  relance_depose_seul: 'Déposés, absents du projet :', relance_relance_seul: 'Dans le projet, sans résultats déposés :',
  relance_version: 'Nuée {nuee} et R {R} ({plateforme})',
  relance_versions: 'Déposé avec {depot}, le {date} ; relancé ici avec {ici}.',
  relance_paquets: 'Paquets R d’une autre version ici : {liste}.',
  relance_manifeste_intact: 'Ses {n} fichiers sont ceux de son manifeste (MANIFEST.sha256 : les mêmes empreintes SHA-256).',
  relance_manifeste_differe: 'Ses fichiers ne sont plus ceux de son manifeste (MANIFEST.sha256) :',
  relance_manifeste_changes: 'changés : {fichiers}', relance_manifeste_manquants: 'manquants : {fichiers}', relance_manifeste_ajoutes: 'ajoutés : {fichiers}',
  relance_manifeste_illisibles: '{n} ligne(s) du manifeste illisible(s)',
  relance_sans_manifeste: 'Il n’a pas de manifeste (MANIFEST.sha256) : ses fichiers ne peuvent pas être vérifiés.',
  relance_lien: 'Venu de Zenodo : l’enregistrement {record} ({doi}).',
  relance_lien_concept: 'Venu de Zenodo : la dernière version de {concept}, l’enregistrement {record} ({doi}).',
}, {
  changements_donnees: 'The data', changements_fichier: 'Read from {chemin}',
  changements_documents: 'Values cited in the documents', changements_documents_compte: '{n} changed',
  changements_ouvrir_document: 'Open the document there',
  changements_valeur_perimee: 'not updated: {nom} couldn’t be fitted again',
  relance_titre_panneau: 'Package rerun: {titre}', relance_section: 'The reproducibility package',
  relance_bilan: '{relances}: {identiques}, {differents}.',
  relance_n_relances_un: '{n} model rerun', relance_n_relances_n: '{n} models rerun',
  relance_n_identiques_un: '{n} gives the deposited results', relance_n_identiques_n: '{n} give the deposited results',
  relance_n_differents_un: '{n} differs', relance_n_differents_n: '{n} differ',
  relance_non_relances: 'Not fitted again here: {noms}.', relance_sans_depots: 'Without deposited results: {noms}.',
  relance_non_relance: '{nom} couldn’t be fitted again here: {message}', relance_sans_depot: 'The package has no results of {nom}: {message}',
  relance_identique: 'It gives the deposited results.', relance_depose: 'Deposited', relance_relance: 'Rerun',
  relance_depose_seul: 'Deposited, not in the project:', relance_relance_seul: 'In the project, without deposited results:',
  relance_version: 'Nuée {nuee} and R {R} ({plateforme})',
  relance_versions: 'Deposited with {depot}, on {date}; rerun here with {ici}.',
  relance_paquets: 'R packages in another version here: {liste}.',
  relance_manifeste_intact: 'Its {n} files are those of its manifest (MANIFEST.sha256: the same SHA-256 hashes).',
  relance_manifeste_differe: 'Its files are no longer those of its manifest (MANIFEST.sha256):',
  relance_manifeste_changes: 'changed: {fichiers}', relance_manifeste_manquants: 'missing: {fichiers}', relance_manifeste_ajoutes: 'added: {fichiers}',
  relance_manifeste_illisibles: '{n} unreadable line(s) in the manifest',
  relance_sans_manifeste: 'It has no manifest (MANIFEST.sha256): its files can’t be checked.',
  relance_lien: 'From Zenodo: record {record} ({doi}).',
  relance_lien_concept: 'From Zenodo: the latest version of {concept}, record {record} ({doi}).',
});
