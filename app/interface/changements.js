// What changed: the key results of a project's models on two sides (an old version and the project
// now, File › Versions: versions.js), compared by the engine (moteur/R/resultats_cles.R: requests
// resultats_cles and comparer_resultats) and shown in a panel: the models with notable changes
// first, their sentences, a table of the results that changed, then the models of one side only.
//   resumerProjet(app, pid, { surProgression })      → { modeles: [{ nom, numero, type, entrees }] }
//   afficherChangements(app, { titre, pid, avant, apres, donnees, documents })   the panel, and what it shows
// Both summaries and their comparison are asked in the interface's language: an entry's label comes
// in the language of its resultats_cles request. After an update of the data (etapes.js), the panel
// also shows what the update did to the data (`donnees`) and the values cited in the project's
// documents that changed (`documents`).
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
export async function resumerProjet(app, pid, { surProgression } = {}) {
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
    if (surProgression) surProgression({ fait: k, total: definitions.length, nom: d.nom });
    try {
      const calcules = montre ? app.modelesDe(pid).filter((m) => m.numero === d.numero).sort((a, b) => numero(b.id) - numero(a.id)) : [];
      const id = calcules.length ? calcules[0].id : await app.calculerDiscret(pid, d.numero, A.get(d.numero));
      const r = await demander('resultats_cles', { modele: id });
      modeles.push({ nom: d.nom, numero: d.numero, type: d.type, entrees: [].concat(r.entrees || []) });
    } catch (e) {
      modeles.push({ nom: d.nom, numero: d.numero, type: d.type, entrees: null, erreur: e.message });
    }
  }
  if (surProgression) surProgression({ fait: definitions.length, total: definitions.length });
  return { modeles };
}

// The comparison of two summaries (resumerProjet), model by model, shown in a panel of the project
// `pid`, titled `titre`; it opens at once and fills in as the engine answers. Resolves to what it
// shows: { id, titre, pid, modeles: [{ nom, numero, type, comparaison | erreur }], avantSeul, apresSeul,
// donnees, documents }. After an update of the data (etapes.js): `donnees`, its report as
// etapes_texte.js reads it ({ resume, rien, sections, fichier }); `documents`, the documents whose cited
// values changed ([{ id, titre, valeurs: [{ debut, fin, brut, avant, apres, phrase, perime }] }]).
export async function afficherChangements(app, { titre, pid, avant, apres, donnees = null, documents = null }) {
  const id = `C${++compteur}-${Date.now().toString(36)}`;
  const c = { id, titre, pid, enCours: true, modeles: [], avantSeul: [], apresSeul: [], donnees, documents };
  COMPARAISONS.set(id, c);
  while (COMPARAISONS.size > GARDEES) COMPARAISONS.delete(COMPARAISONS.keys().next().value);
  app.ouvrirVue('changements', null, { comparaison: id }, pid);
  const { deux, avantSeul, apresSeul } = apparierModeles(avant, apres);
  Object.assign(c, { avantSeul, apresSeul });
  const langue = langueActuelle();
  for (const [a, b] of deux) {
    const m = { nom: b.nom, numero: b.numero, type: b.type };
    if (!a.entrees || !b.entrees) m.erreur = t('changements_erreur', { nom: b.nom, message: a.erreur || b.erreur });
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
      return c ? t('changements_titre', { titre: c.titre }) : t('changements');
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
    conteneur.replaceChildren(el('div', { class: 'changements' }, el('h2', {}, t('changements_titre', { titre: c.titre })), el('p', { class: 'infos attente' }, t('changements_calcul')),
      el('div', { class: 'defile' }, sectionsMiseAJour(app, c))));
    return;
  }
  loc.ouverts = loc.ouverts || {};
  const tries = ordonnerComparaisons(c.modeles);
  const notables = tries.filter((m) => m.comparaison && [].concat(m.comparaison.phrases || []).length).length;
  const compares = tries.filter((m) => m.comparaison).length;
  const corps = el('div', { class: 'defile' },
    sectionsMiseAJour(app, c),
    !c.modeles.length && !c.avantSeul.length && !c.apresSeul.length ? el('p', { class: 'note' }, t('changements_aucun_modele')) : null,
    tries.map((m) => sectionModele(m, loc, () => rendreChangements(app, conteneur, loc))),
    c.avantSeul.length || c.apresSeul.length ? el('section', { class: 'modele-change seul-cote' },
      c.avantSeul.length ? el('p', {}, el('b', {}, t('changements_avant_seul')), ' ', listeNoms(c.avantSeul)) : null,
      c.apresSeul.length ? el('p', {}, el('b', {}, t('changements_apres_seul')), ' ', listeNoms(c.apresSeul)) : null) : null);
  conteneur.replaceChildren(el('div', { class: 'changements' },
    el('h2', {}, t('changements_titre', { titre: c.titre })),
    el('p', { class: 'infos' }, compares ? t('changements_resume', { n: compares, notables }) : '',
      compares ? el('span', { class: 'aide-drapeaux', title: t('changements_d_aide') }, icone('circle-help')) : null),
    corps));
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
// folded otherwise), or what kept it from being compared.
function sectionModele(m, loc, redessinerPanneau) {
  const tete = el('h3', {}, m.nom, m.type ? el('span', { class: 'badge' }, m.type) : null);
  if (!m.comparaison) return el('section', { class: 'modele-change erreur-change' }, tete, el('p', { class: 'erreur' }, m.erreur || ''));
  const r = m.comparaison;
  const phrases = [].concat(r.phrases || []);
  const changees = entreesChangees(r);
  const total = (r.resume && r.resume.entrees) || [].concat(r.entrees || []).length;
  if (!changees.length) return el('section', { class: 'modele-change inchange' }, tete, el('p', { class: 'doux' }, t('changements_inchange')));
  tete.append(el('span', { class: 'doux compte-change' }, t('changements_entrees', { n: changees.length, total })));
  const tout = !!loc.ouverts[m.nom];
  const lignes = tout ? changees : changees.slice(0, LIGNES);
  const tableau = el('table', { class: 'tableau compact tableau-changements' },
    el('thead', {}, el('tr', {}, [t('changements_resultat'), t('changements_avant'), t('changements_apres'), t('changements_ecart'), ''].map((h) => el('th', {}, h)))),
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
}, {
  changements_donnees: 'The data', changements_fichier: 'Read from {chemin}',
  changements_documents: 'Values cited in the documents', changements_documents_compte: '{n} changed',
  changements_ouvrir_document: 'Open the document there',
  changements_valeur_perimee: 'not updated: {nom} couldn’t be fitted again',
});
