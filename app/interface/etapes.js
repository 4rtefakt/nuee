// Living reports, the data side (Edit › Data steps…, Edit › Update the data…). A project made from
// a data file keeps that file, its source, and every change made to its data in Nuée as a step
// that can be replayed (moteur/R/etapes.R). Data steps shows the source and the steps; Update the
// data reads the file again, or next year's, replays the steps, and removing a step reads the source
// again without it: each previews what it would do, then replaces the data as one change (Ctrl+Z),
// fits every model again and shows What changed (changements.js), with the values cited in the
// documents that changed, marked in their previews (documents.js). A notice in the status bar says
// when the source file changed since it was read. What the engine's answers say is read by
// etapes_texte.js.
import { t, deuxPoints } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander, bureau, MOTEUR_WEBR, moteurWebR } from './api.js';
import { resumerProjet, afficherChangements, sectionsRapport } from './changements.js';
import { valeursCitees } from './documents.js';
import { liste, noms, dateLisible, tailleLisible, nomFichier, etatSource, ICONES_ETAPES, genreEtape, lireRapport, modelesAReajuster, resumeApres,
  valeursChangees, descriptionVersion, dateDescription } from './etapes_texte.js';

const VUE_SEULE = new URLSearchParams(location.search).get('vue');
// (an element's children made again, those that are null or false left out, as el() does)
const remplir = (x, ...enfants) => x.replaceChildren(...enfants.flat(Infinity).filter((e) => e !== null && e !== undefined && e !== false));

export function installerEtapes(app) {
  Object.assign(app, {
    etapesDonnees: (pid) => dialogueEtapes(app, pid),
    miseAJourDonnees: (pid) => dialogueMiseAJour(app, pid),
    verifierSource: (pid) => verifierSource(app, pid),
    avisDonnees: () => avisDonnees(app),
    apresAnnulation: (pid, r, retabli) => apresAnnulation(app, pid, r, retabli),
  });
  // (a file changed by another program while Nuée was behind it: seen when it comes back)
  if (!VUE_SEULE) window.addEventListener('focus', () => verifierSources(app));
}

// ------------------------------------------------------------ the notice of a changed source

// What the latest etapes request said of each project's source (etatSource), and the notices
// dismissed, by project and by the file as it was then: a file that changes again brings its
// notice back. This window only.
const sources = new Map();
const ignorees = new Set();
let derniereVerification = 0;

function noterSource(app, pid, r) {
  const e = etatSource(r && r.source);
  const avant = sources.get(pid);
  sources.set(pid, e);
  if (!avant || avant.etat !== e.etat || avant.cle !== e.cle) app.majStatut();
  return e;
}

// Only a project made from a file (a_source), not an old version opened read-only (versions.js: its
// source is the project's, said there), and without asking whether its data are still those of its
// steps (conformes: false), which would read them all.
async function verifierSource(app, pid) {
  const p = app.projet(pid);
  if (!p || p.lecture_seule || !p.a_source) return;
  try { noterSource(app, pid, await demander('etapes', { projet: pid, conformes: false }, { silencieux: true })); } catch { /* asked again when the window comes back */ }
}

// The window back in front: each open project's source asked again, at most every ten seconds (a
// file dialog closing brings the window back too).
function verifierSources(app) {
  if (Date.now() - derniereVerification < 10000) return;
  derniereVerification = Date.now();
  for (const p of app.projets()) verifierSource(app, p.id);
}

// The status bar's notice, like that of a new version: for the active project first, then the
// others; its link opens Update the data, its × hides it until the file changes again.
function avisDonnees(app) {
  const actif = app.projetActif();
  const ids = app.projets().map((p) => p.id).sort((a, b) => (b === actif) - (a === actif));
  for (const pid of ids) {
    const e = sources.get(pid);
    if (!e || e.etat !== 'changee' || ignorees.has(`${pid}|${e.cle}`)) continue;
    const p = app.projet(pid);
    return el('span', { class: 'avis-donnees' },
      el('a', { href: '#', title: t('etapes_avis_aide'), onclick: (ev) => { ev.preventDefault(); app.miseAJourDonnees(pid); } },
        icone('refresh-cw'), app.projets().length > 1 ? t('etapes_avis_projet', { nom: p.nom }) : t('etapes_avis')),
      el('button', { class: 'icone-seule', title: t('etapes_avis_ignorer'), 'aria-label': t('etapes_avis_ignorer'),
        onclick: () => { ignorees.add(`${pid}|${e.cle}`); app.majStatut(); } }, icone('x')));
  }
  return null;
}

// ------------------------------------------------------------ pieces of both dialogs

const ETATS = { introuvable: 'etapes_etat_introuvable', changee: 'etapes_etat_changee', lue: 'etapes_etat_lue' };
const badgeEtat = (etat) => (ETATS[etat] ? el('span', { class: `etat-fichier ${etat}` }, t(ETATS[etat])) : null);
const LECTEURS = { archive: 'etapes_lecteur_archive', collecte: 'etapes_lecteur_collecte' };

// A file the project read (its source, its XLSForm form, a step's table or raster): its name, its
// state, where it is, and when it was read.
function ficheFichier(f, { titre = null, grand = false } = {}) {
  const e = etatSource(f);
  const a = f.actuel || {};
  return el('div', { class: `fiche-fichier ${e.etat}${grand ? ' grand' : ''}` },
    el('div', { class: 'ligne' }, icone(f.lecteur === 'collecte' || f.lecteur === 'archive' ? 'smartphone' : 'file-spreadsheet'),
      titre ? el('span', { class: 'doux' }, titre) : null, el('b', {}, nomFichier(f.chemin)), badgeEtat(e.etat)),
    el('p', { class: 'chemin-fichier' }, f.trouve || f.chemin),
    e.ailleurs ? el('p', { class: 'note' }, t('etapes_ailleurs', { chemin: f.chemin })) : null,
    el('p', { class: 'note' }, [f.table ? t('etapes_table', { table: f.table }) : '', LECTEURS[f.lecteur] ? t(LECTEURS[f.lecteur]) : '',
      t('etapes_lu_le', { date: dateLisible(f.lu), taille: tailleLisible(f.taille), modifie: dateLisible(f.date) })].filter(Boolean).join(' · ')),
    e.etat === 'changee' ? el('p', { class: 'note attention' }, t('etapes_maintenant', { taille: tailleLisible(a.taille), modifie: dateLisible(a.date) })) : null);
}

// Data that changed outside the steps (the R console, a version before 1.1), or a project from
// before 1.1: what the engine says of it.
function avisConformes(r) {
  if (r.conformes === false) return el('div', { class: 'alerte-donnees' }, el('p', {}, r.raison));
  return r.conformes === null && r.raison ? el('p', { class: 'note' }, r.raison) : null;
}

const confirmer = (titre, texte, note, bouton) => new Promise((resolve) => {
  let oui = false;
  dialogue({ titre, contenu: el('div', {}, el('p', {}, texte), note ? el('p', { class: 'note' }, note) : null),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: bouton, principal: true, action: () => { oui = true; return true; } }],
    surFermeture: () => resolve(oui) });
});

// ------------------------------------------------------------ Data steps

// The project's source and its steps: each with its label (written by the engine in the reader's
// language), its date and the files it read, and Remove, whose preview comes first (dialogueRetrait).
async function dialogueEtapes(app, pid) {
  const p = app.projet(pid);
  if (!p) { app.statut(t('choisir_projet'), true); return; }
  const contenu = el('div', { class: 'contenu-etapes' }, el('p', { class: 'infos attente' }, t('calcul')));
  let fermee = false;
  const d = dialogue({ titre: `${t('etapes_titre')} — ${p.nom}`, large: true, contenu: el('div', { class: 'dialogue-etapes' }, contenu),
    boutons: [{ libelle: t('fermer'), action: () => true },
      { libelle: t('etapes_mettre_a_jour'), principal: true, action: () => { app.miseAJourDonnees(pid); return true; } }],
    surFermeture: () => { fermee = true; } });
  // (removed, this dialog closes: What changed says what the removal did)
  const retirer = (e) => dialogueRetrait(app, pid, e, () => d.fermer());

  async function charger() {
    let r;
    try { r = await demander('etapes', { projet: pid }); } catch (e) { contenu.replaceChildren(el('p', { class: 'erreur' }, e.message)); return; }
    noterSource(app, pid, r);
    if (fermee) return;
    const etapes = liste(r.etapes);
    remplir(contenu,
      el('h3', {}, t('etapes_source')),
      r.source ? ficheFichier(r.source, { grand: true }) : el('p', { class: 'note' }, t('etapes_sans_source')),
      r.source && r.source.formulaire ? ficheFichier(r.source.formulaire, { titre: t('etapes_formulaire') }) : null,
      avisConformes(r),
      el('h3', {}, t('etapes_liste', { n: etapes.length })),
      etapes.length ? el('div', { class: 'defile-etapes' }, el('table', { class: 'tableau compact liste-etapes' },
        el('thead', {}, el('tr', {}, [el('span', { class: 'lecteur-seul' }, t('etapes_col_n')), t('etapes_col_etape'), t('etapes_col_date'), t('etapes_col_fichiers'),
          el('span', { class: 'lecteur-seul' }, t('retirer'))].map((h) => el('th', {}, h)))),
        el('tbody', {}, etapes.map((e) => el('tr', {},
          el('td', { class: 'num doux' }, String(e.n)),
          // (the kind of step on its icon, for the mouse and screen readers)
          el('td', {}, el('span', { class: 'ligne etape' }, el('span', { class: 'genre-etape', title: genreEtape(e.demande), role: 'img', 'aria-label': genreEtape(e.demande) },
            icone(ICONES_ETAPES[e.demande] || 'pencil')), el('span', {}, e.etiquette || genreEtape(e.demande)))),
          el('td', { class: 'doux' }, dateLisible(e.date)),
          el('td', {}, liste(e.fichiers).map((f) => el('span', { class: 'fichier-etape', title: f.trouve || f.chemin }, nomFichier(f.chemin), ' ', badgeEtat(etatSource(f).etat)))),
          el('td', {}, el('button', { title: t('etapes_retirer_aide'), onclick: () => retirer(e) }, icone('trash-2'), t('retirer')))))))) : el('p', { class: 'note' }, t('etapes_aucune')),
      el('p', { class: 'note' }, t('etapes_note')));
  }
  await charger();
}

// ------------------------------------------------------------ the preview of a change of the data

// Whether the project keeps versions (versions.js): saved to a file whose versions are on.
async function versionsActives(p) {
  if (!bureau || !bureau.versions || !p.chemin_enregistrement) return false;
  try { return (await bureau.versions.etat(p.chemin_enregistrement)).actif === true; } catch { return false; }
}

// "Save a version first" (versions.js), ticked, when the project keeps versions: its zone, and
// whether to save one.
function boiteVersion(p, description) {
  const zone = el('div', { class: 'version-maj' });
  const st = { actif: false, garder: true };
  const lue = versionsActives(p).then((oui) => {
    st.actif = oui;
    if (oui) {
      remplir(zone, el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: true, onchange: (e) => { st.garder = e.target.checked; } }), t('etapes_maj_version')),
        el('p', { class: 'note' }, t('etapes_maj_version_note', { description })));
    }
  });
  // (once History has answered: a button pressed before it did still keeps the version, ticked by default)
  return { zone, voulue: async () => { await lue; return st.actif && st.garder; } };
}

// The preview of a change the engine makes from the project's source (`demande`: mettre_a_jour or
// retirer_etape, with `apercu`: nothing changes), in its own zone: asked again by previsualiser
// (`nouveau`: another file, its tables forgotten); a file of several tables asks which one, in the
// list File › Open shows (app.js: choisirTable); the report in sections, then the models that
// leave out rows asked whether they take the new rows the file brings, ticked. `parametres()`: the
// request's own, or null while there is no file to read; `surChangement()`: when the preview comes
// or goes (the dialog's button); `n`: the project's observations now; `choix`: whether to ask the
// models about the new rows (an update's, not a removal's, which brings none it can make).
function apercuDonnees(pid, { demande, parametres, surChangement, n, choix = true }) {
  const zone = el('div', { class: 'apercu-maj', 'aria-live': 'polite' });
  const st = { tables: null, table: null, rapport: null, lu: null, exclus: new Set(), numero: 0 };
  const cle = (c) => (c.numero !== null && c.numero !== undefined ? c.numero : c.nom);
  const montrer = (...enfants) => {
    // (the list of tables drawn again with the preview: the keyboard stays on it)
    const surListe = !!(document.activeElement && document.activeElement.matches && document.activeElement.matches('.apercu-maj .liste-tables') && zone.contains(document.activeElement));
    remplir(zone, el('h3', {}, t('etapes_maj_apercu')),
      st.tables ? el('div', { class: 'ligne table-maj' }, el('span', {}, t('etapes_maj_table')),
        el('select', { class: 'liste-tables', size: Math.min(8, Math.max(3, st.tables.length)), 'aria-label': t('etapes_maj_table'),
          onchange: (e) => { st.table = e.target.value || null; previsualiser(); } },
        st.tables.map((x) => el('option', { value: x, selected: x === st.table }, x)))) : null,
      ...enfants);
    const tables = surListe && zone.querySelector('.liste-tables');
    if (tables) tables.focus();
  };
  const rapport = () => {
    const lu = st.lu;
    return [
      el('p', { class: `resume-maj${lu.rien ? ' rien' : ''}` }, lu.resume),
      sectionsRapport(lu),
      choix && lu.choix.length ? el('div', { class: 'choix-maj' }, el('h4', {}, icone('circle-plus'), t('etapes_maj_nouvelles_obs')),
        el('p', { class: 'note' }, t('etapes_maj_nouvelles_note')),
        lu.choix.map((c) => el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: !st.exclus.has(cle(c)), onchange: (e) => {
          if (e.target.checked) st.exclus.delete(cle(c)); else st.exclus.add(cle(c));
        } }), c.texte))) : null,
    ];
  };
  async function previsualiser({ nouveau = false } = {}) {
    const k = ++st.numero;
    if (nouveau) Object.assign(st, { tables: null, table: null });
    st.rapport = null;
    surChangement();
    const q = parametres();
    if (!q) { montrer(el('p', { class: 'doux' }, t('etapes_maj_choisir_fichier'))); return; }
    if (st.tables && !st.table) { montrer(el('p', { class: 'doux' }, t('etapes_maj_choisir_table'))); return; }
    montrer(el('p', { class: 'infos attente' }, t('etapes_maj_lecture')));
    let r;
    try {
      r = await demander(demande, { projet: pid, ...q, table: st.table || undefined, apercu: true });
    } catch (e) {
      if (k === st.numero) montrer(el('p', { class: 'erreur' }, e.message));
      return;
    }
    if (k !== st.numero) return;
    if (r.choisir) {  // (a file of several tables, none of them the source's)
      st.tables = liste(r.choisir);
      montrer(el('p', { class: 'doux' }, t('etapes_maj_choisir_table')));
      return;
    }
    Object.assign(st, { rapport: r, lu: lireRapport(r, n), exclus: new Set() });  // (another file: each model asked again, ticked)
    montrer(...rapport());
    surChangement();
  }
  return {
    zone, previsualiser,
    pret: () => !!st.rapport,
    table: () => st.table || undefined,
    sansNouvelles: () => (st.lu ? st.lu.choix.filter((c) => st.exclus.has(cle(c))).map(cle) : []),
  };
}

// ------------------------------------------------------------ Update the data

// The file to read (the source, or another one: next year's), its preview, a version saved first;
// then Update (executer).
async function dialogueMiseAJour(app, pid) {
  const p = app.projet(pid);
  if (!p) { app.statut(t('choisir_projet'), true); return; }
  let info;
  try { info = await app.occupe(() => demander('etapes', { projet: pid })); } catch { return; }
  noterSource(app, pid, info);
  const s = info.source;
  const es = etatSource(s);
  const avecSource = es.etat === 'lue' || es.etat === 'changee';
  const st = { quel: avecSource ? 'source' : 'autre', autre: null, enCours: false };
  const zoneFichier = el('div', { class: 'fichier-maj' });
  const apercu = apercuDonnees(pid, { demande: 'mettre_a_jour', n: p.observations.uid.length, surChangement: () => majBouton(),
    parametres: () => (st.quel === 'autre' ? (st.autre ? { chemin: st.autre } : null) : {}) });
  const version = boiteVersion(p, descriptionVersion());

  const rendreFichier = () => {
    // (drawn again: the keyboard stays where it was, on the same choice or button)
    const a = document.activeElement;
    const avant = a && zoneFichier.contains(a) ? (a.name === 'fichier-maj' ? `radio:${a.value}` : 'choisir') : null;
    // (Another file chosen without a file yet: its preview asks for one, Choose… opens the dialog;
    // the arrow keys go through the choices without opening it)
    const radio = (valeur, libelle, desactive = false) => el('label', { class: 'champ' },
      el('input', { type: 'radio', name: 'fichier-maj', value: valeur, checked: st.quel === valeur, disabled: desactive, onchange: () => {
        st.quel = valeur;
        rendreFichier();
        apercu.previsualiser({ nouveau: true });
      } }), libelle);
    remplir(zoneFichier, el('h3', {}, t('etapes_maj_le_fichier')),
      es.etat === 'aucune' ? el('p', { class: 'note' }, t('etapes_maj_sans_source')) : el('div', { class: 'ligne' },
        radio('source', t('etapes_maj_source', { nom: nomFichier(s.chemin) }), !avecSource), badgeEtat(es.etat),
        el('span', { class: 'chemin-fichier', title: s.trouve || s.chemin }, s.trouve || s.chemin)),
      el('div', { class: 'ligne' },
        es.etat === 'aucune' ? el('span', {}, t('etapes_maj_fichier')) : radio('autre', t('etapes_maj_autre')),
        el('button', { class: 'choisir-fichier-maj', onclick: () => choisirAutre() }, icone('folder-open'), t('etapes_maj_choisir')),
        st.autre ? el('span', { class: 'chemin-fichier', title: st.autre }, st.autre) : null));
    const cible = avant === 'choisir' ? zoneFichier.querySelector('.choisir-fichier-maj')
      : avant ? zoneFichier.querySelector(`input[name="fichier-maj"][value="${avant.slice(6)}"]`) : null;
    if (cible) cible.focus();
  };
  const choisirAutre = async () => {
    const c = await app.choisirTableau();
    if (c) Object.assign(st, { autre: c, quel: 'autre' });
    else if (!st.autre && avecSource) st.quel = 'source';
    rendreFichier();
    apercu.previsualiser({ nouveau: true });
  };

  const lancer = async () => {
    if (!apercu.pret() || st.enCours) return;
    st.enCours = true;
    majBouton();
    const quand = new Date();
    d.fermer();
    await executer(app, pid, { demande: 'mettre_a_jour', textes: LIBELLES.mettre_a_jour, quand, version: (await version.voulue()) ? descriptionVersion(quand) : null,
      parametres: { chemin: st.quel === 'autre' ? st.autre : undefined, table: apercu.table(), sans_nouvelles: apercu.sansNouvelles() } });
  };
  const d = dialogue({ titre: `${t('etapes_maj_titre')} — ${p.nom}`, large: true,
    contenu: el('div', { class: 'dialogue-maj' }, avisConformes(info), zoneFichier, apercu.zone, version.zone, el('p', { class: 'note' }, t('etapes_maj_note'))),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('etapes_maj_bouton'), principal: true, action: () => { lancer(); return false; } }] });
  const bouton = d.boite.querySelector('.pied-dialogue button.principal');
  function majBouton() { if (bouton) bouton.disabled = !apercu.pret() || st.enCours; }
  rendreFichier();
  apercu.previsualiser();
}

// ------------------------------------------------------------ Remove a step

// A step removed, its preview first (retirer_etape with apercu: the source read again, the other
// steps replayed on it, nothing changed yet), then as an update: a version first, the models
// fitted again, What changed (executer). A source changed since it was read can't be read for
// it, the removal would bring its new data in too, which the engine refuses: the dialog says so
// and offers Update the data. `quitterEtapes`: called when it leads on, the removal asked or Update
// the data opened (Data steps closes).
async function dialogueRetrait(app, pid, etape, quitterEtapes = () => {}) {
  const p = app.projet(pid);
  if (!p) return;
  let info;
  try { info = await app.occupe(() => demander('etapes', { projet: pid, conformes: false })); } catch { return; }
  noterSource(app, pid, info);
  const changee = etatSource(info.source).etat === 'changee';
  const st = { enCours: false };
  const apercu = apercuDonnees(pid, { demande: 'retirer_etape', n: p.observations.uid.length, parametres: () => ({ n: etape.n }), surChangement: () => majBouton(),
    choix: false });
  const version = boiteVersion(p, t('etapes_version_retrait', { n: etape.n, date: dateDescription(new Date()) }));
  const lancer = async () => {
    if (!apercu.pret() || st.enCours) return;
    st.enCours = true;
    majBouton();
    const quand = new Date();
    d.fermer();
    quitterEtapes();
    await executer(app, pid, { demande: 'retirer_etape', textes: LIBELLES.retirer_etape, quand, etape: etape.n,
      version: (await version.voulue()) ? t('etapes_version_retrait', { n: etape.n, date: dateDescription(quand) }) : null,
      parametres: { n: etape.n, table: apercu.table() } });
  };
  const d = dialogue({ titre: `${t('etapes_retrait_dialogue', { n: etape.n })} — ${p.nom}`, large: true,
    contenu: el('div', { class: 'dialogue-maj' },
      el('p', { class: 'etape-retiree' }, el('span', { class: 'genre-etape', title: genreEtape(etape.demande), role: 'img', 'aria-label': genreEtape(etape.demande) },
        icone(ICONES_ETAPES[etape.demande] || 'pencil')), el('b', {}, etape.etiquette || genreEtape(etape.demande)), el('span', { class: 'doux' }, ` · ${dateLisible(etape.date)}`)),
      changee ? el('div', { class: 'alerte-donnees' }, el('p', {}, t('etapes_retrait_source_changee'))) : null,
      apercu.zone, changee ? null : version.zone, el('p', { class: 'note' }, t('etapes_retrait_note'))),
    // (the source changed since it was read: update the data first)
    boutons: [{ libelle: t('annuler'), action: () => true }, changee
      ? { libelle: t('etapes_mettre_a_jour'), principal: true, action: () => { quitterEtapes(); app.miseAJourDonnees(pid); return true; } }
      : { libelle: t('etapes_retrait_bouton'), principal: true, action: () => { lancer(); return false; } }] });
  const bouton = d.boite.querySelector('.pied-dialogue button.principal');
  function majBouton() { if (bouton && !changee) bouton.disabled = !apercu.pret() || st.enCours; }
  apercu.previsualiser();
}

// ------------------------------------------------------------ the change made, the models fitted again

// The words of each change of the data made from its source: the progress dialog's title, its
// step that replaces the data, What changed's title, the status bar at the end.
const LIBELLES = {
  mettre_a_jour: { titre: 'etapes_p_titre', donnees: 'etapes_p_donnees', changements: 'etapes_changements', fini: 'etapes_p_fini', finiSauf: 'etapes_p_fini_sauf' },
  retirer_etape: { titre: 'etapes_retrait_titre', donnees: 'etapes_retrait_donnees', changements: 'etapes_retrait_changements', fini: 'etapes_retrait_fini',
    finiSauf: 'etapes_retrait_fini_sauf' },
};

// The progress of an update, in a dialog: what is being done, a bar, and Stop: the model being
// fitted stopped too (moteur/R/arret.R, the stop of the status bar, which this Stop follows), and no
// more models fitted again (before the data change, no update at all). Closing the dialog (Esc, ×;
// not a click beside it) fits no more models either, the one being fitted finishing. (Also a package's
// rerun: relancer.js.)
export function progression(titre) {
  const etape = el('p', { class: 'etape-maj' });
  const barre = el('progress', { class: 'barre-maj' });
  const detail = el('p', { class: 'note' });
  let arret = false;
  // (the status bar's Stop: app.js, arreterCalcul)
  const surArret = () => arreter(false);
  const d = dialogue({ titre, contenu: el('div', { class: 'progression-maj' }, etape, barre, detail), clicDehors: false,
    boutons: [{ libelle: t('etapes_arreter'), action: () => { arreter(); return false; } }],
    surFermeture: () => { arret = true; window.removeEventListener('arret-demande', surArret); } });
  window.addEventListener('arret-demande', surArret);
  const bouton = d.boite.querySelector('.pied-dialogue button');
  function arreter(moteur = true) {
    arret = true;
    bouton.disabled = true;
    bouton.textContent = t('etapes_arret_demande');
    if (moteur && bureau && bureau.arreterCalcul) bureau.arreterCalcul().catch(() => {});
    else if (moteur && MOTEUR_WEBR) moteurWebR().then((m) => m.arreter()).catch(() => {});  // (R in the page interrupted)
  }
  return {
    arrete: () => arret,
    // (total: the bar's length, none for a step that takes as long as it takes)
    montrer(texte, fait = null, total = null, { arretable = true, note = '' } = {}) {
      etape.textContent = texte;
      if (total) { barre.max = total; barre.value = fait; } else barre.removeAttribute('value');
      detail.textContent = note;
      if (!arret) bouton.disabled = !arretable;
    },
    fermer: () => d.fermer(),
  };
}

// The version saved before the change (versions.js), once the project is saved (without the
// automatic version an hour after the last one: this one, with its words), when the layer makes
// one on request (marquer) and the project keeps versions.
async function marquerVersion(app, pid, chemin, description) {
  if (!(await app.enregistrer(pid, false, { versionAuto: false }))) throw new Error(t('etapes_p_non_enregistre'));
  const v = bureau && bureau.versions;
  if (!v || typeof v.marquer !== 'function' || (await v.etat(chemin)).actif !== true) return;
  await v.marquer(chemin, description);
}

// Every model of the project fitted again (app.ajusterDeNouveau) but those blocked (variables
// missing from the data), shown in the progress `pr` until Stop: numero → { id } | { erreur }.
async function reajusterTout(app, pid, pr, modeles) {
  const refaits = new Map();
  for (const [k, m] of modeles.entries()) {
    if (pr.arrete()) break;
    pr.montrer(t('etapes_p_reajuster', { n: k + 1, total: modeles.length }), k, modeles.length, { note: `${m.nom} · ${m.type}` });
    if (m.bloque) continue;
    try { refaits.set(m.numero, { id: await app.ajusterDeNouveau(pid, m.numero) }); } catch (e) {
      refaits.set(m.numero, { erreur: e.message });
      if (e.reponse && e.reponse.arrete) break;  // (Stop, of the status bar or of the dialog: no more models)
    }
  }
  return refaits;
}

// An update of the data, or a step removed (`demande`, its `parametres`, its `textes`): a version
// first if asked; the models' key results and the values the documents cite before; the change made
// by the engine (one Ctrl+Z); every model fitted again but those the report says can't be; the key
// results and cited values after; then What changed, and the values that changed marked in the
// documents. A model not fitted again goes into What changed with its reason (resumeApres), its cited
// values as not updated. A step can't be removed from a source changed meanwhile: Update the data.
async function executer(app, pid, { demande, parametres, textes, quand, version, etape = null }) {
  const p = app.projet(pid);
  const pr = progression(t(textes.titre, { nom: p.nom, etape }));
  let change = false;
  try {
    if (version) {
      pr.montrer(t('etapes_p_version'), null, null, { arretable: false });
      await marquerVersion(app, pid, p.chemin_enregistrement, version);
    }
    const suivre = (cle) => (x) => pr.montrer(t(cle, { n: Math.min(x.fait + 1, x.total), total: x.total }), x.fait, x.total, { note: x.nom || '' });
    // (Stop, of the dialog or of the status bar, here: no update at all, not the models' results in full)
    const avant = await resumerProjet(app, pid, { surProgression: suivre('etapes_p_avant'), arrete: pr.arrete });
    // (the documents' values, a part of What changed that never stops the change: null if unread)
    const citees = () => valeursCitees(app, pid).catch(() => null);
    const citeesAvant = await citees();
    if (pr.arrete()) { pr.fermer(); app.statut(t('etapes_p_arrete_avant')); return; }
    pr.montrer(t(textes.donnees), null, null, { arretable: false });
    const n = app.projet(pid).observations.uid.length;
    const r = await app.changerDonnees(demande, pid, parametres);
    change = true;
    // (a removed step that had renamed a variable: the documents whose references followed its name back)
    if (liste(r.documents).length) app.relireDocuments(pid, liste(r.documents));
    verifierSource(app, pid);
    const modeles = modelesAReajuster(r.projet, r);
    const refaits = await reajusterTout(app, pid, pr, modeles);
    const apres = resumeApres(await resumerProjet(app, pid, { surProgression: suivre('etapes_p_apres') }), modeles, refaits);
    const restes = modeles.filter((m) => !(refaits.get(m.numero) || {}).id);
    const citeesApres = citeesAvant && await citees();
    const documents = citeesApres ? valeursChangees(citeesAvant, citeesApres, restes.map((m) => m.nom)) : [];
    pr.fermer();
    const date = dateLisible(quand);
    app.marquerChangements(pid, documents, date);
    const lu = lireRapport(r, n);
    await afficherChangements(app, { titre: t(textes.changements, { date, etape }), pid, avant, apres, documents,
      donnees: { resume: lu.resume, rien: lu.rien, sections: lu.sections, fichier: (r.fichier || {}).chemin || '' } });
    app.statut(restes.length ? t(textes.finiSauf, { n: modeles.length - restes.length, total: modeles.length, noms: noms(restes.map((m) => m.nom)), etape })
      : t(textes.fini, { n: modeles.length, etape }), restes.length > 0);
  } catch (e) {
    pr.fermer();
    // (Stop, of the status bar, before the data changed: nothing changed, the engine stopped there)
    if (e.reponse && e.reponse.arrete && !change) { app.statut(t('etapes_p_arrete_avant')); return; }
    // (the source changed between the preview and the removal: an update first)
    if (e.reponse && e.reponse.source_modifiee) {
      if (await confirmer(t('etapes_retirer_titre'), e.message, null, t('etapes_mettre_a_jour'))) app.miseAJourDonnees(pid);
      return;
    }
    app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true);
  }
}

// ------------------------------------------------------------ undone

// A change undone (app.js: annulerAction, `r` the engine's answer), or redone (`retabli`:
// retablirAction), in whichever window it was made. One that replaced the data (an update, a step
// removed: r.reajuster) brings the data, the source, the steps and the models' definitions back, but
// the session's fits are those of the other data: every model is fitted again, as the change did.
// Another may leave fits on variables the project no longer has (a column's rename undone after
// its models were fitted again under the new name: r.a_reajuster, their numbers): those are. Its
// progress, Stop, and the status bar says so.
async function apresAnnulation(app, pid, r, retabli = false) {
  if (!app.projet(pid)) return;
  const numeros = new Set(liste(r.a_reajuster).map(Number));
  const modeles = modelesAReajuster(r.projet).filter((m) => r.reajuster || numeros.has(Number(m.numero)));
  if (!modeles.length) return;
  const cle = retabli ? 'etapes_retablissement' : 'etapes_annulation';
  const pr = progression(t(`${cle}_titre`, { quoi: r.etiquette, nom: app.projet(pid).nom }));
  let refaits = new Map();
  try { refaits = await reajusterTout(app, pid, pr, modeles); } finally { pr.fermer(); }
  const restes = modeles.filter((m) => !(refaits.get(m.numero) || {}).id);
  app.statut(restes.length ? t(`${cle}_fini_sauf`, { quoi: r.etiquette, n: modeles.length - restes.length, total: modeles.length, noms: noms(restes.map((m) => m.nom)) })
    : t(`${cle}_fini`, { quoi: r.etiquette, n: modeles.length }), restes.length > 0);
}
