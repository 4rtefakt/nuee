// Living reports, the data side, pure: the engine's answers about a project's source and steps
// (moteur/R/etapes.R; serveur.R: etapes, mettre_a_jour, retirer_etape) made into what the dialogs
// of etapes.js show. No page and no engine: tests/essai_etapes.mjs checks them.
//   etatSource(source)                       the source file's state, and the key of its notice
//   lireRapport(rapport, n)                  an update's report, as its preview lists it
//   modelesAReajuster(projet, rapport)       the models to fit again once the data are updated
//   resumeApres(apres, modeles, refaits)     the summary after the update, models not refitted marked
//   descriptionVersion(date)                 the version saved before an update
import { t, langueActuelle } from './i18n.js';
import { fmt } from './nombres.js';

// (R sends a vector of one value as that value, and an empty one as [] or null)
export const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));
const entre = (n) => (langueActuelle() === 'fr' ? `« ${n} »` : `“${n}”`);
const locale = () => (langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB');

// Names in a sentence: the first `max`, then "…" and how many in all.
export function noms(x, max = 8) {
  const l = liste(x).map(String);
  return l.length > max ? `${l.slice(0, max).join(', ')}… (${t('etapes_en_tout', { n: l.length })})` : l.join(', ');
}

// A date the engine writes ("2026-11-16T14:05:00", the computer's time, no zone), or a Date, for
// the reader; '' when there is none, the text itself when it can't be read.
export function dateLisible(texte) {
  if (!texte) return '';
  const d = texte instanceof Date ? texte : new Date(String(texte));
  return Number.isNaN(d.getTime()) ? String(texte) : d.toLocaleString(locale(), { dateStyle: 'medium', timeStyle: 'short' });
}

// A file's size for the reader: bytes, kB or MB (powers of 1000, the system's file dialogs' way).
export function tailleLisible(octets) {
  const n = octets === null || octets === undefined || octets === '' ? NaN : Number(octets);
  if (!Number.isFinite(n)) return '';
  if (n < 1000) return t('etapes_octets', { n });
  return n < 1e6 ? t('etapes_ko', { n: fmt(n / 1000, n < 1e4 ? 1 : 0) }) : t('etapes_mo', { n: fmt(n / 1e6, n < 1e7 ? 1 : 0) });
}

// The last part of a path.
export const nomFichier = (chemin) => String(chemin || '').split(/[\\/]/).pop();

// ------------------------------------------------------------ the source

// Where the project's source file stands (the etapes request's `source`): `etat` "aucune" (a project
// without a file: a pasted table, a summary by group, a .usp project, one from before 1.1),
// "introuvable", "changee" (changed since it was read) or "lue" (as it was read); `ailleurs`: found
// by its path from the project's folder, the folder moved with its data; `cle`: the file as it is
// now, for a notice dismissed until it changes again.
export function etatSource(s) {
  if (!s) return { etat: 'aucune', cle: null };
  if (!s.existe) return { etat: 'introuvable', cle: null };
  const a = s.actuel || {};
  const meme = (x, y) => String(x || '').replace(/\\/g, '/').toLowerCase() === String(y || '').replace(/\\/g, '/').toLowerCase();
  return { etat: s.modifie === true ? 'changee' : 'lue', ailleurs: !!s.trouve && !meme(s.trouve, s.chemin),
    cle: `${s.trouve}|${a.taille}|${a.date}` };
}

// ------------------------------------------------------------ the update's report

// The words of the data steps' requests (ETAPES_DONNEES in etapes.R), and their icons.
export const ICONES_ETAPES = {
  modifier_valeur: 'pencil', variable_calculee: 'calculator', imputer: 'wand-sparkles', classes_valeurs: 'tag', ajouter_colonnes: 'file-spreadsheet',
  ajouter_raster: 'layers', remplacer: 'search', categories: 'tag', coller_cellules: 'clipboard-paste', decrire_variables: 'ruler',
  coordonnees: 'map', classe_selection: 'tag', baci_modele: 'flask-conical', scores_en_variables: 'git-merge', kmeans_variable: 'shapes',
};
export const genreEtape = (demande) => (ICONES_ETAPES[demande] ? t(`etapes_genre_${demande}`) : String(demande || ''));

// An update's report (mettre_a_jour, its preview or once made) read for the reader, in sections:
// what is new, what is missing, what changed, the steps replayed, the models, how the file was read.
// Each section { cle, titre, lignes: [{ texte, classe }] } (classe: '', 'attention' or 'erreur');
// `resume`, one line; `rien`, nothing changes; `choix`, the models to ask whether they take the new
// observations; `bloques`, those that can't be fitted again. `n`: the project's observations now.
export function lireRapport(r, n = null) {
  const o = r.observations || {};
  const v = r.variables || {};
  const ids = r.identifiants || {};
  const m = r.modeles || {};
  const etapes = liste(r.etapes);
  const ligne = (texte, classe = '') => ({ texte, classe });
  const ajoutees = Number(o.ajoutees) || 0;
  const retirees = Number(o.retirees) || 0;
  const cellules = Number(o.cellules) || 0;
  const avecNoms = (cle, nombre, premiers) => t(cle, { n: nombre, noms: liste(premiers).join(', ') + (nombre > liste(premiers).length ? '…' : '') });

  const nouveau = [];
  if (ajoutees) nouveau.push(ligne(avecNoms('etapes_r_obs_nouvelles', ajoutees, o.premieres_ajoutees)));
  if (liste(v.nouvelles).length) nouveau.push(ligne(t('etapes_r_vars_nouvelles', { n: liste(v.nouvelles).length, noms: noms(v.nouvelles, 12) })));
  for (const c of liste(v.categories)) nouveau.push(ligne(t('etapes_r_categories', { variable: entre(c.variable), noms: noms(c.nouvelles, 12) })));
  if (liste(ids.nouveaux).length) nouveau.push(ligne(t('etapes_r_ids_nouveaux', { noms: noms(ids.nouveaux) })));

  const absent = [];
  if (retirees) absent.push(ligne(avecNoms('etapes_r_obs_retirees', retirees, o.premieres_retirees), 'attention'));
  if (liste(v.absentes).length) absent.push(ligne(t('etapes_r_vars_absentes', { n: liste(v.absentes).length, noms: noms(v.absentes, 12) }), 'attention'));
  if (liste(ids.absents).length) absent.push(ligne(t('etapes_r_ids_absents', { noms: noms(ids.absents) }), 'attention'));

  const change = [];
  if (cellules) {
    const parVariable = liste(o.cellules_par_variable).slice().sort((a, b) => b.cellules - a.cellules);
    change.push(ligne(t('etapes_r_cellules', { n: cellules,
      detail: noms(parVariable.map((x) => `${x.variable} (${x.cellules})`), 10) })));
  }
  if (liste(v.recalculees).length) change.push(ligne(t('etapes_r_recalculees', { noms: noms(v.recalculees) })));
  for (const x of liste(v.types)) {
    change.push(ligne(x.type === 'qualitative' ? t('etapes_r_type_qualitative', { variable: entre(x.variable) })
      : t(Number(x.perdues) ? 'etapes_r_type_perdues' : 'etapes_r_type_quantitative', { variable: entre(x.variable), n: x.perdues }), Number(x.perdues) ? 'attention' : ''));
  }

  const faites = etapes.filter((e) => e.statut === 'ok').length;
  const lignesEtapes = [];
  if (etapes.length) {
    lignesEtapes.push(ligne(faites === etapes.length ? t('etapes_r_toutes', { n: etapes.length }) : t('etapes_r_faites', { n: faites, total: etapes.length })));
    for (const e of etapes) {
      const qui = `${e.n}. ${e.etiquette || genreEtape(e.demande)}`;
      if (e.statut === 'sautee') lignesEtapes.push(ligne(t('etapes_r_sautee', { etape: qui, raison: e.raison }), 'attention'));
      else if (e.statut === 'erreur') lignesEtapes.push(ligne(t('etapes_r_erreur', { etape: qui, raison: e.raison }), 'erreur'));
      for (const note of liste(e.notes)) lignesEtapes.push(ligne(t('etapes_r_note', { etape: qui, note })));
    }
  }

  const bloques = liste(m.variables_absentes).map((x) => ({ nom: x.nom, numero: x.numero, variables: liste(x.variables) }));
  const lignesModeles = bloques.map((x) => ligne(t('etapes_r_bloque', { nom: x.nom, variables: noms(x.variables) }), 'erreur'));
  // (a new category goes before a model's reference, the last one, which stays: a model whose
  // reference has no rows left takes another, and says so; a model of components gains a term)
  for (const x of liste(m.references_changees)) {
    lignesModeles.push(ligne(t('etapes_r_reference', { nom: x.nom, variable: entre(x.variable), avant: entre(x.avant), apres: entre(x.apres) }), 'attention'));
  }
  for (const x of liste(m.nouvelles_categories)) {
    lignesModeles.push(ligne(t('etapes_r_categories_modele', { nom: x.nom, variable: entre(x.variable), noms: noms(liste(x.categories).map(entre)) })));
  }
  if (liste(m.sans_nouvelles).length) lignesModeles.push(ligne(t('etapes_r_sans_nouvelles', { noms: noms(m.sans_nouvelles) })));

  const lecture = [];
  if (o.par_position) lecture.push(ligne(t('etapes_r_par_position'), 'attention'));
  if (Number(o.repetees)) lecture.push(ligne(t('etapes_r_repetees', { n: o.repetees })));
  for (const x of liste(r.notes)) lecture.push(ligne(x));

  const sections = [
    { cle: 'nouveau', titre: t('etapes_s_nouveau'), lignes: nouveau },
    { cle: 'absent', titre: t('etapes_s_absent'), lignes: absent },
    { cle: 'change', titre: t('etapes_s_change'), lignes: change },
    { cle: 'etapes', titre: t('etapes_s_etapes'), lignes: lignesEtapes },
    { cle: 'modeles', titre: t('etapes_s_modeles'), lignes: lignesModeles },
    { cle: 'lecture', titre: t('etapes_s_lecture'), lignes: lecture },
  ].filter((s) => s.lignes.length);

  const rien = !ajoutees && !retirees && !cellules && !nouveau.length && !absent.length && !change.length
    && etapes.every((e) => e.statut === 'ok' && !liste(e.notes).length);
  const morceaux = [];
  if (n !== null && n !== undefined) morceaux.push(t('etapes_r_obs_total', { avant: n, apres: n + ajoutees - retirees }));
  if (ajoutees || retirees) morceaux.push(t('etapes_r_obs_bilan', { plus: ajoutees, moins: retirees }));
  if (cellules) morceaux.push(t('etapes_r_cellules_court', { n: cellules }));
  if (liste(v.nouvelles).length || liste(v.absentes).length) morceaux.push(t('etapes_r_vars_bilan', { plus: liste(v.nouvelles).length, moins: liste(v.absentes).length }));
  const ecartees = etapes.length - faites;
  if (ecartees) morceaux.push(t('etapes_r_etapes_bilan', { n: ecartees }));
  return {
    rien, sections, bloques,
    resume: rien ? t('etapes_r_rien') : morceaux.join(' · '),
    // (the rows each one fits, those it could fit, and the file's new rows it would take)
    choix: liste(m.observations_choisies).map((x) => ({ nom: x.nom, numero: x.numero, observations: x.observations, admises: x.admises, nouvelles: x.nouvelles,
      texte: t('etapes_r_choix', { nom: x.nom, n: x.observations, total: x.admises, nouvelles: x.nouvelles }) })),
  };
}

// ------------------------------------------------------------ the models fitted again

// The project's models to fit again once its data are updated (`projet`: its summary then), in
// the order of their numbers: those that can be fitted (`ajuste`, as the explorer has them), each
// { numero, nom, type, bloque } — `bloque`, the variables it misses, when the report says it
// can't be fitted again.
export function modelesAReajuster(projet, rapport = {}) {
  const bloques = liste((rapport.modeles || {}).variables_absentes);
  const bloque = (d) => {
    const b = bloques.find((x) => (x.numero !== null && x.numero !== undefined && Number(x.numero) === Number(d.numero)) || x.nom === d.nom);
    return b ? liste(b.variables) : null;
  };
  return liste(projet && projet.modeles).filter((d) => d.ajuste && d.numero !== null && d.numero !== undefined)
    .sort((a, b) => Number(a.numero) - Number(b.numero))
    .map((d) => ({ numero: d.numero, nom: d.nom, type: d.type, bloque: bloque(d) }));
}

// The project's summary after the update (changements.js: resumerProjet) with each model that
// wasn't fitted again (`refaits`: numero → { id } | { erreur }) given its reason in place of its
// entries: its latest fit there is still that of the data before, and What changed would say it
// didn't change.
export function resumeApres(apres, modeles, refaits) {
  const raisons = new Map();
  for (const m of modeles) {
    const r = refaits.get(m.numero);
    if (r && r.id) continue;
    raisons.set(Number(m.numero), r && r.erreur ? r.erreur
      : m.bloque ? t('etapes_bloque_raison', { variables: noms(m.bloque) }) : t('etapes_non_reajuste'));
  }
  return { ...apres, modeles: liste(apres && apres.modeles).map((x) => (raisons.has(Number(x.numero)) ? { ...x, entrees: null, erreur: raisons.get(Number(x.numero)) } : x)) };
}

// ------------------------------------------------------------ the values cited in documents

// A line that starts a block of its own in Markdown (a heading, a list item, a quote, a table's
// row), and one that ends it (blank, a rule, the YAML header's ---).
const DEBUT_BLOC = /^\s{0,3}(#{1,6}\s|[-*+]\s|\d+[.)]\s|>|\|)/;
const FIN_BLOC = /^\s*$|^\s*(---|\.\.\.|\*\*\*|___)\s*$/;
const MARQUE_BLOC = /^\s{0,3}(#{1,6}\s+|[-*+]\s+|\d+[.)]\s+|>\s*|\|\s*)/;
// (a sentence ends at . ! ? or … before a space, its closing quote or bracket with it)
const FIN_PHRASE = /[.!?…]["»”’)\]]*(?=\s|$)/g;

// The sentence of a document's text around its reference at [debut, fin): { avant, apres }, its
// words before and after the reference, within the paragraph or list item and between sentence
// ends; the other references in it written as `valeurs` gives them ([{ debut, fin, texte }]),
// Markdown's marks left out, each side cut to about `max` characters.
export function phraseAutour(texte, debut, fin, valeurs = [], max = 120) {
  const s = String(texte);
  // (the block: line by line, up to a blank line, a rule, or a line that starts a block)
  const debutLigne = (i) => s.lastIndexOf('\n', i - 1) + 1;
  const finLigne = (i) => { const k = s.indexOf('\n', i); return k < 0 ? s.length : k; };
  let a = debutLigne(debut);
  while (a > 0 && !DEBUT_BLOC.test(s.slice(a, finLigne(a)))) {
    const avant = debutLigne(a - 1);
    const ligne = s.slice(avant, a - 1);
    if (FIN_BLOC.test(ligne) || /^\s{0,3}#/.test(ligne)) break;
    a = avant;
  }
  let b = finLigne(fin);
  while (b < s.length) {
    const ligne = s.slice(b + 1, finLigne(b + 1));
    if (FIN_BLOC.test(ligne) || DEBUT_BLOC.test(ligne)) break;
    b = finLigne(b + 1);
  }
  // (the sentence ends, the references aside: a caption's full stop isn't one)
  const masque = s.slice(a, b).replace(/\{\{[\s\S]*?\}\}/g, (m) => 'x'.repeat(m.length));
  let p = a;
  let q = b;
  for (const m of masque.matchAll(FIN_PHRASE)) {
    const k = a + m.index + m[0].length;
    if (k <= debut) p = k;
    else if (a + m.index >= fin) { q = k; break; }
  }
  // (the other references written as their values, those without one as they are)
  const ecrire = (x, y) => {
    let r = '';
    let i = x;
    for (const v of liste(valeurs).filter((w) => w.debut >= x && w.fin <= y).sort((u, w) => u.debut - w.debut)) {
      r += s.slice(i, v.debut) + v.texte;
      i = v.fin;
    }
    return (r + s.slice(i, y)).replace(/\*\*|__|`|\*/g, '').replace(/\s+/g, ' ');
  };
  let avant = ecrire(p, debut).replace(MARQUE_BLOC, '').trimStart();
  let apres = ecrire(fin, q).trimEnd();
  if (avant.length > max) avant = `…${avant.slice(avant.indexOf(' ', avant.length - max) + 1)}`;
  if (apres.length > max) { const k = apres.lastIndexOf(' ', max); apres = `${apres.slice(0, k > 0 ? k : max)}…`; }
  return { avant, apres };
}

// The values cited in the documents that an update of the data changed: `avant` and `apres`, the
// documents' values before and after it (documents.js: valeursCitees), matched by their place and
// their text; `nonRefaits`, the names of the models not fitted again, whose values are still those
// of the data before (`perime`). [{ id, titre, valeurs: [{ debut, fin, brut, modele, avant, apres,
// perime, phrase }] }] in the order of the text, the documents without one left out.
export function valeursChangees(avant, apres, nonRefaits = []) {
  const vieux = new Set(liste(nonRefaits));
  return liste(apres).map((d) => {
    const a = liste(avant).find((x) => x.id === d.id) || { valeurs: [] };
    const meme = (x, y) => x.debut === y.debut && x.brut === y.brut;
    const valeurs = [];
    for (const v of liste(d.valeurs)) {
      const w = liste(a.valeurs).find((x) => meme(x, v));
      const perime = vieux.has(v.modele);
      if (!perime && w && w.texte === v.texte) continue;
      valeurs.push({ debut: v.debut, fin: v.fin, brut: v.brut, modele: v.modele, avant: w ? w.texte : '', apres: v.texte, perime });
    }
    // (a value before that the reference no longer gives)
    for (const w of liste(a.valeurs)) {
      if (!liste(d.valeurs).some((v) => meme(v, w))) valeurs.push({ debut: w.debut, fin: w.fin, brut: w.brut, modele: w.modele, avant: w.texte, apres: '', perime: false });
    }
    valeurs.sort((x, y) => x.debut - y.debut);
    for (const v of valeurs) v.phrase = phraseAutour(d.texte, v.debut, v.fin, d.valeurs);
    return { id: d.id, titre: d.titre, valeurs };
  }).filter((d) => d.valeurs.length);
}

// ------------------------------------------------------------ the version before

// The description of the version saved before an update (versions.js: enregistrerVersion).
// (its moment, also that of the version before a step is removed: "16 November 2026 at 14:05")
export const dateDescription = (date = new Date()) => date.toLocaleString(locale(), { dateStyle: 'long', timeStyle: 'short' });
export const descriptionVersion = (date = new Date()) => t('etapes_version', { date: dateDescription(date) });
