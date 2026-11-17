// Plugins (moteur/R/greffons.R): an R function and its manifest, which become a model type, PLUGIN.
// Here: File › Plugins… (the plugins installed, yours and your organisation's, the packages they need,
// installed into the R Nuée uses, Remove), Install from file… (the manifest and the code read before
// anything runs; installing a plugin approves it on this computer), Model › Plugins › <name>… (New
// model with the dialog its manifest describes: its inputs and options in the General tab, then the
// Observations tab), the views of a plugin's model (an R-code model's, vues_code.js, with the plugin's
// card and its code), and the bar over a project carrying a plugin not approved on this computer: Show,
// then Install and run, Run the project's version, or Use the installed version (docs/deploiement.md:
// opening a file never runs code from it). Imported by app.js.
import { t, ajouterTextes, langueActuelle, deuxPoints } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander, bureau } from './api.js';
import { VUES } from './panneaux.js';
import { ANALYSES, FAMILLES_TYPES, dialogueModele } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';
import { approuver, codeNumerote, rendreResumeCode } from './vues_code.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const estGreffon = (m) => !!m && !!m.analyse && m.type === 'PLUGIN';
// a text of a manifest in the interface's language (its English when it has no French)
const texte = (x) => (!x ? '' : typeof x === 'string' ? x : (langueActuelle() === 'fr' ? x.fr : x.en) || x.en || '');
const nomsAuteurs = (auteurs) => liste(auteurs).map((a) => (typeof a === 'string' ? a : a.nom)).filter(Boolean);
const etEntre = (noms) => (noms.length <= 1 ? noms.join('') : `${noms.slice(0, -1).join(', ')} ${t('greffon_et')} ${noms[noms.length - 1]}`);
const CLE = (id) => `PLUGIN:${id}`;
let appli = null;  // (the application, given by installerGreffons)

// ------------------------------------------------------------ the plugins the engine knows

// What the engine says of the plugins (moteur/R/greffons.R: greffons_interface), kept by app.js at
// each synchronisation (app.greffonsEtat): their folders, the policy, and each plugin.
const etatGreffons = () => (appli && appli.greffonsEtat) || { liste: [] };
const installes = () => liste(etatGreffons().liste);
// those a new model can use: well formed, the organisation's or approved here, not for a newer Nuée
const utilisable = (g) => !!g && g.etat === 'ok' && g.permis === true && !g.trop_recent;
const utilisables = () => installes().filter(utilisable);
const installePour = (id) => installes().find((g) => g.id === id && g.etat === 'ok') || null;

// Each usable plugin as a type of New model (dialogues.js: ANALYSES), PLUGIN:<id>; a model's own
// version, when it isn't the one installed, is PLUGIN:<id>@<fingerprint> (modifierGreffon).
function majAnalyses() {
  for (const k of Object.keys(ANALYSES)) if (k.startsWith('PLUGIN:') && !k.includes('@')) delete ANALYSES[k];
  for (const g of utilisables()) ANALYSES[CLE(g.id)] = analyseGreffon(g);
}

// The engine's list asked again (after an installation, a removal), then the open projects' summaries
// (a plugin they carry may run now).
async function actualiser(app) {
  app.greffonsEtat = await demander('greffons');
  majAnalyses();
  for (const p of app.projets()) {
    try { app.majProjet(await demander('projet', { projet: p.id })); } catch { /* closed meanwhile */ }
  }
  barresGreffons(app);
  app.diffuser('modeles');  // (the other windows' menus and New model too: app.js, synchroniser)
}

// ------------------------------------------------------------ the model's dialog

// The New model entry of a plugin `g` (its manifest's inputs and options, as the engine describes
// them); `modele`: the model being edited when the dialog is made from that model's own version.
// (New model's Type list: the plugins in a family of their own, after Nuée's own, dialogues.js)
if (!FAMILLES_TYPES.some((f) => f.cle === 'greffons')) FAMILLES_TYPES.push({ cle: 'greffons', libelle: () => t('greffons_menu'), types: [] });

function analyseGreffon(g, modele = null) {
  return {
    typeMoteur: 'PLUGIN',
    familleType: 'greffons',
    libelle: () => `${texte(g.nom) || g.id} (${t('greffon_mot')})`,
    note: () => `${texte(g.description)} ${t('greffon_note_version', { version: g.version, auteurs: etEntre(nomsAuteurs(g.auteurs)) })}`,
    roles: [],
    echelle: 'none',
    vc: false,
    xFacultatif: true,
    sansEchelle: true,
    onglets: ['general', 'observations'],  // (its inputs in the General tab, from the manifest)
    offert: () => !modele && utilisable(installePour(g.id)),
    champs: (st, rendre, p) => champsGreffon(g, st, rendre, p, modele),
    valider: (st) => validerGreffon(g, st),
    options: (st) => optionsGreffon(g, st, modele),
    surErreur: (st, e) => surErreurGreffon(e),
  };
}

// The dialog's state for this plugin: its inputs (variables' numbers, in the project's order) and its
// options (their defaults first); another plugin's, or none, start again.
function preparer(g, st) {
  if (!st.options || !st.options.greffon || st.options.greffon.id !== g.id) st.options = { greffon: { id: g.id }, entrees: {}, options: {} };
  const o = st.options;
  if (!o.entrees || Array.isArray(o.entrees)) o.entrees = {};
  if (!o.options || Array.isArray(o.options)) o.options = {};
  for (const e of liste(g.entrees)) o.entrees[e.id] = liste(o.entrees[e.id]).map(Number);
  for (const x of liste(g.options)) if (o.options[x.id] === undefined || o.options[x.id] === null) o.options[x.id] = x.defaut;
  st._filtres ??= {};
}

const accepte = (e, v) => (e.accepte === 'quantitative' ? !v.qualitative : e.accepte === 'qualitative' ? v.qualitative : true);

// An input: one variable (a list, empty allowed when it is optional) or several (boxes to tick, a
// filter when they are many, with Tick or Untick those shown); its help under it.
function champEntree(e, st, p) {
  const candidates = p.variables.filter((v) => accepte(e, v));
  const choisies = () => st.options.entrees[e.id];
  const aide = texte(e.aide) ? el('p', { class: 'note' }, texte(e.aide)) : null;
  if (e.type === 'variable') {
    const valeur = choisies()[0];
    return el('div', { class: 'champ-greffon' }, el('select', { 'aria-label': texte(e.libelle), onchange: (ev) => { st.options.entrees[e.id] = ev.target.value ? [Number(ev.target.value)] : []; } },
      el('option', { value: '', selected: valeur === undefined }, e.facultatif ? t('greffon_aucune') : t('greffon_choisir')),
      candidates.map((v) => el('option', { value: v.uid, selected: v.uid === valeur }, v.nom))), aide);
  }
  const compte = el('span', { class: 'infos' });
  const majCompte = () => {
    const n = choisies().length;
    const bornes = e.max ? t('greffon_entre', { min: e.min, max: e.max }) : t('greffon_au_moins', { min: e.min });
    compte.textContent = `${t('greffon_n_choisies', { n })} (${bornes})`;
    compte.classList.toggle('attention', n < e.min || (!!e.max && n > e.max));
  };
  const cases = el('div', { class: 'liste-variables', role: 'group', 'aria-label': texte(e.libelle) });
  const montrees = () => {
    const f = (st._filtres[e.id] || '').toLowerCase();
    return candidates.filter((v) => !f || v.nom.toLowerCase().includes(f));
  };
  const fixer = (uids) => { const s = new Set(uids); st.options.entrees[e.id] = candidates.filter((v) => s.has(v.uid)).map((v) => v.uid); majCompte(); };
  const dessinerCases = () => {
    const s = new Set(choisies());
    cases.replaceChildren(...montrees().map((v) => el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: s.has(v.uid), onchange: (ev) => {
      fixer(ev.target.checked ? [...choisies(), v.uid] : choisies().filter((u) => u !== v.uid));
    } }), v.nom)));
    if (!cases.children.length) cases.append(el('span', { class: 'doux' }, t('greffon_aucune_variable')));
  };
  dessinerCases();
  majCompte();
  const outils = candidates.length > 12 ? el('div', { class: 'ligne' },
    el('input', { type: 'search', class: 'filtre', placeholder: t('recherche'), value: st._filtres[e.id] || '', 'aria-label': `${texte(e.libelle)} — ${t('recherche')}`,
      oninput: (ev) => { st._filtres[e.id] = ev.target.value; dessinerCases(); } }),
    el('button', { onclick: () => { fixer([...choisies(), ...montrees().map((v) => v.uid)]); dessinerCases(); } }, t('greffon_cocher')),
    el('button', { onclick: () => { const m = new Set(montrees().map((v) => v.uid)); fixer(choisies().filter((u) => !m.has(u))); dessinerCases(); } }, t('greffon_decocher'))) : null;
  return el('div', { class: 'champ-greffon' }, outils, cases, compte, aide);
}

// An option: a number (its bounds; a whole number), a choice, a box to tick, a text; its help under it.
function champOption(o, st) {
  const v = st.options.options[o.id];
  const aide = texte(o.aide) ? el('p', { class: 'note' }, texte(o.aide)) : null;
  const libelle = texte(o.libelle);
  let champ;
  if (o.type === 'number') {
    champ = el('input', { type: 'number', 'aria-label': libelle, value: v ?? '', step: o.entier ? 1 : 'any', style: 'width:9em',
      ...(o.min !== undefined && o.min !== null ? { min: o.min } : {}), ...(o.max !== undefined && o.max !== null ? { max: o.max } : {}),
      oninput: (ev) => { st.options.options[o.id] = ev.target.value === '' ? null : Number(ev.target.value); } });
  } else if (o.type === 'choice') {
    champ = el('select', { 'aria-label': libelle, onchange: (ev) => { st.options.options[o.id] = ev.target.value; } },
      liste(o.valeurs).map((x) => el('option', { value: x.valeur, selected: x.valeur === v }, texte(x.libelle))));
  } else if (o.type === 'checkbox') {
    champ = el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: v === true, onchange: (ev) => { st.options.options[o.id] = ev.target.checked; } }), libelle);
  } else {
    champ = el('input', { 'aria-label': libelle, value: v ?? '', oninput: (ev) => { st.options.options[o.id] = ev.target.value; } });
  }
  return el('div', { class: 'champ-greffon' }, champ, aide);
}

// The General tab's fields: a model's own version said (and the installed one offered) when the dialog
// is made from it, then each input and each option.
function champsGreffon(g, st, rendre, p, modele) {
  preparer(g, st);
  const out = [];
  if (modele) {
    const i = installePour(g.id);
    const autre = i && i.empreinte !== g.empreinte;
    out.push(el('label', {}, t('greffon_version')), el('div', { class: 'champ-greffon' },
      el('p', { class: 'note' }, autre ? t(i.version === g.version ? 'greffon_version_copie' : 'greffon_version_autre', { version: g.version, installee: i.version })
        : t('greffon_version_seule', { version: g.version })),
      autre && utilisable(i) ? el('button', { onclick: () => { st.type = CLE(g.id); rendre(); } }, icone('refresh-cw'), t('greffon_utiliser_installee', { version: i.version })) : null));
  }
  for (const e of liste(g.entrees)) out.push(el('label', {}, texte(e.libelle)), champEntree(e, st, p));
  for (const o of liste(g.options)) out.push(el('label', {}, o.type === 'checkbox' ? '' : texte(o.libelle)), champOption(o, st));
  return out;
}

// What's missing or wrong, in words ('' when nothing): each input's number of variables, each number.
function validerGreffon(g, st) {
  preparer(g, st);
  for (const e of liste(g.entrees)) {
    const n = liste(st.options.entrees[e.id]).length;
    const nom = texte(e.libelle);
    if (n < e.min) return e.type === 'variable' ? t('greffon_entree_requise', { nom }) : t('greffon_entree_min', { nom, min: e.min });
    if (e.max && n > e.max) return t('greffon_entree_max', { nom, max: e.max });
  }
  for (const o of liste(g.options)) {
    if (o.type !== 'number') continue;
    const v = st.options.options[o.id];
    const nom = texte(o.libelle);
    if (typeof v !== 'number' || !Number.isFinite(v)) return t('greffon_option_nombre', { nom });
    if (o.min !== undefined && o.min !== null && v < o.min) return t('greffon_option_min', { nom, min: o.min });
    if (o.max !== undefined && o.max !== null && v > o.max) return t('greffon_option_max', { nom, max: o.max });
    if (o.entier && !Number.isInteger(v)) return t('greffon_option_entier', { nom });
  }
  return '';
}

// The options sent (moteur/R/greffons.R: definir_plugin): the plugin, its version when the dialog is a
// model's own (by its fingerprint: the project has it), its inputs and options.
function optionsGreffon(g, st, modele) {
  preparer(g, st);
  return { greffon: { id: g.id, ...(modele ? { empreinte: g.empreinte } : {}) },
    entrees: Object.fromEntries(liste(g.entrees).map((e) => [e.id, liste(st.options.entrees[e.id])])),
    options: Object.fromEntries(liste(g.options).map((o) => [o.id, st.options.options[o.id]])) };
}

// A fit refused for packages that aren't installed: offered at once (installerPaquets).
function surErreurGreffon(e) {
  const r = e && e.reponse;
  if (r && liste(r.paquets_manquants).length && appli) installerPaquets(appli, liste(r.paquets_manquants));
}

// Model › Plugins › <name>…: New model with its dialog (its type given as a new model's, as Find a
// command does: Auto ticked, the scaling its own; not as a model to edit)
function nouveauModeleGreffon(app, g) {
  const p = app.projet(app.projetActif());
  if (!p) { app.statut(t('choisir_projet'), true); return; }
  dialogueModele(app, p, null, texte(g.nom) || g.id, null, CLE(g.id));
}

// Edit the model: its dialog again, made from the installed plugin when it is the model's own version,
// otherwise from the model's (which the project carries), with the installed one offered.
function modifierGreffon(app, m) {
  const g = m.greffon || {};
  const i = installePour(g.id);
  let type = CLE(g.id);
  if (!(i && utilisable(i) && i.empreinte === g.empreinte)) {
    type = `PLUGIN:${g.id}@${g.empreinte}`;
    ANALYSES[type] = analyseGreffon({ ...g, entrees: liste((g.manifeste || {}).entrees), options: liste((g.manifeste || {}).options) }, m);
  }
  dialogueModele(app, app.projet(m.projet), { ...m.reglages, type }, t('modifier_modele'));
}

// ------------------------------------------------------------ a plugin, as the dialogs show it

const lienOrcid = (a) => (a.orcid ? el('a', { href: `https://orcid.org/${a.orcid}`, target: '_blank', rel: 'noopener', title: `ORCID ${a.orcid}` }, a.nom) : a.nom);
const auteursEnLiens = (auteurs) => {
  const a = liste(auteurs);
  return a.flatMap((x, k) => [k ? (k === a.length - 1 ? ` ${t('greffon_et')} ` : ', ') : '', lienOrcid(typeof x === 'string' ? { nom: x } : x)]);
};

// A plugin's card: its name, version and where it comes from, its authors (their ORCID), licence and
// the Nuée it needs, its description, what is wrong with it, its packages, its citations.
function carteGreffon(g, { boutons = [], paquets = true } = {}) {
  const pq = g.paquets || {};
  const noms = liste(pq.nom);
  const etiquettes = [g.source === 'organisation' ? [t('greffon_organisation'), ''] : g.source === 'utilisateur' ? [t('greffon_vous'), ''] : null,
    g.etat === 'invalide' ? [t('greffon_invalide'), 'alerte'] : null, g.etat === 'double' ? [t('greffon_double'), 'alerte'] : null,
    g.etat === 'remplace' ? [t('greffon_remplace'), 'alerte'] : null, g.source === 'utilisateur' && g.etat === 'ok' && g.permis === false ? [t('greffon_change'), 'alerte'] : null,
    g.trop_recent ? [t('greffon_trop_recent', { nuee: g.nuee }), 'alerte'] : null].filter(Boolean);
  const citations = liste(g.citations).filter((c) => c && c.texte);
  return el('section', { class: 'carte-greffon' + (etiquettes.some((x) => x[1]) ? ' alerte' : '') },
    el('div', { class: 'fiche-titre' }, icone('puzzle'), el('b', {}, texte(g.nom) || g.id || '?'), g.version ? el('span', { class: 'badge' }, g.version) : null,
      etiquettes.map(([x, genre]) => el('span', { class: `etiquette-r ${genre}` }, x))),
    nomsAuteurs(g.auteurs).length || g.licence ? el('p', { class: 'doux' }, nomsAuteurs(g.auteurs).length ? [t('greffon_par'), ' ', auteursEnLiens(g.auteurs)] : null,
      g.licence ? ` · ${t('greffon_licence')}${deuxPoints()}${g.licence}` : '', g.nuee ? ` · ${t('greffon_nuee', { nuee: g.nuee })}` : '') : null,
    texte(g.description) ? el('p', {}, texte(g.description)) : null,
    liste(g.erreurs).length ? el('div', { class: 'alerte-donnees' }, el('b', {}, t('greffon_erreurs')), el('ul', {}, liste(g.erreurs).map((x) => el('li', {}, x)))) : null,
    liste(g.avertissements).length ? el('ul', { class: 'note' }, liste(g.avertissements).map((x) => el('li', {}, x))) : null,
    paquets && noms.length ? el('p', { class: 'paquets-greffon' }, `${t('greffon_paquets')}${deuxPoints()}`, noms.map((n, i) => el('span', { class: `etiquette-r ${liste(pq.present)[i] ? '' : 'alerte'}` },
      liste(pq.present)[i] ? `${n} ${liste(pq.version)[i] || ''}`.trim() : `${n} — ${t('greffon_absent')}`))) : null,
    citations.length ? el('details', { class: 'citations-greffon' }, el('summary', {}, t('greffon_citer', { n: citations.length })),
      citations.map((c) => el('div', { class: 'citation-greffon' }, el('p', {}, c.texte), c.bibtex ? el('button', { onclick: (ev) => {
        navigator.clipboard.writeText(c.bibtex).then(() => { ev.target.textContent = t('greffon_bibtex_copie'); }, () => {});
      } }, icone('copy'), t('greffon_copier_bibtex')) : null))) : null,
    boutons.length ? el('div', { class: 'ligne' }, boutons) : null);
}

// Its files, each with its line numbers.
const codeGreffon = (fichiers) => liste(fichiers).map((f) => el('section', {}, el('h4', {}, f.chemin), codeNumerote(f.texte)));

// ------------------------------------------------------------ File › Plugins…

async function gererGreffons(app) {
  let e;
  try { e = app.greffonsEtat = await demander('greffons'); } catch (err) { app.statut(err.message, true); return; }
  majAnalyses();
  const zone = el('div', { class: 'liste-greffons', 'aria-live': 'polite' });
  const message = el('p', { class: 'note', 'aria-live': 'polite' });
  const dessiner = () => {
    const gs = liste(e.liste);
    zone.replaceChildren(...(gs.length ? gs.map((g) => carteGreffon(g, { boutons: [
      utilisable(g) ? el('button', { onclick: () => { d.fermer(); nouveauModeleGreffon(app, g); }, disabled: !app.projetActif() }, icone('file-plus'), t('greffon_nouveau_modele')) : null,
      liste((g.paquets || {}).present).some((x) => !x) ? el('button', { onclick: () => installerPaquets(app, liste(g.paquets.nom).filter((_, i) => !liste(g.paquets.present)[i]), apres) },
        icone('download'), t('greffon_installer_paquets')) : null,
      g.dossier ? el('button', { onclick: () => montrerInstalle(g) }, icone('file-code'), t('greffon_montrer')) : null,
      g.source === 'utilisateur' ? el('button', { onclick: () => retirer(g) }, icone('trash-2'), t('greffon_retirer')) : null].filter(Boolean) }))
      : [el('p', { class: 'note' }, t('greffons_aucun_installe'))]));
  };
  const apres = async () => { e = app.greffonsEtat = await demander('greffons'); majAnalyses(); dessiner(); await actualiser(app); };
  // (an installed plugin's code: read from its folder by the engine, as Install from file reads it)
  const montrerInstalle = async (g) => {
    try {
      const lu = await demander('greffon_lire', { chemin: g.dossier });
      dialogue({ titre: `${texte(g.nom) || g.id} ${g.version || ''}`, large: true, contenu: el('div', { class: 'montrer-code' }, carteGreffon({ ...lu.greffon, source: g.source, etat: g.etat, permis: g.permis }),
        codeGreffon(lu.greffon.fichiers)), boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
    } catch (err) { message.textContent = err.message; }
  };
  const retirer = (g) => dialogue({ titre: t('greffon_retirer_titre', { nom: texte(g.nom) || g.id }), contenu: el('p', {}, t('greffon_retirer_texte')),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('greffon_retirer'), principal: true, action: () => {
      demander('greffon_retirer', { id: g.id }).then(apres, (err) => { message.textContent = err.message; });
      return true;
    } }] });
  dessiner();
  const installation = e.installation
    ? el('div', { class: 'ligne' }, el('button', { class: 'principal', onclick: () => installerDepuisFichier(app, apres) }, icone('folder-open'), t('greffon_installer_fichier')),
      el('span', { class: 'note' }, t('greffon_installer_note')))
    : el('p', { class: 'note' }, e.interdit ? t('greffons_politique') : t('greffons_bureau'));
  const d = dialogue({ titre: t('greffons_titre'), large: true, contenu: el('div', { class: 'dialogue-greffons' },
    el('p', {}, t('greffons_intro')), e.interdit ? el('p', { class: 'note attention' }, t('greffons_politique')) : null,
    zone, installation, message,
    el('p', { class: 'note' }, [e.dossier ? t('greffons_dossier', { dossier: e.dossier }) : '', e.organisation ? ` ${t('greffons_dossier_organisation', { dossier: e.organisation })}` : ''].join(''))),
  boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
}

// Install from file…: a .nueeplugin (or a plugin's plugin.json), read by the engine without running
// anything, then shown before it is installed.
async function installerDepuisFichier(app, apres) {
  const chemin = bureau ? await bureau.choisirFichier([{ name: t('greffon_filtre'), extensions: ['nueeplugin'] }, { name: 'plugin.json', extensions: ['json'] }]) : null;
  if (!chemin) return;
  let lu;
  try { lu = await demander('greffon_lire', { chemin }); } catch (err) { app.statut(err.message, true); return; }
  dialogueInstallation(app, lu, { chemin }, apres);
}

// The install dialog: the plugin's card, what it replaces, why it can't be installed when it can't,
// the trust it asks for, and its code; Install approves its fingerprint, then the engine writes it.
function dialogueInstallation(app, lu, source, apres = null) {
  const g = lu.greffon;
  const i = lu.installe;
  const remplace = i ? t(i.meme ? 'greffon_deja' : i.ordre > 0 ? 'greffon_remplace_ancienne' : i.ordre < 0 ? 'greffon_remplace_recente' : 'greffon_remplace_copie', { version: i.version }) : '';
  const manquants = liste((g.paquets || {}).nom).filter((_, k) => !liste((g.paquets || {}).present)[k]);
  // (the keyboard on its text and code: Enter doesn't install what wasn't read)
  dialogue({ titre: t('greffon_installer_titre', { nom: texte(g.nom) || g.id || '?', version: g.version || '' }), large: true, focus: 'corps',
    contenu: el('div', { class: 'montrer-code installation-greffon' }, carteGreffon(g),
      remplace ? el('p', { class: 'note' }, remplace) : null,
      !lu.peut ? el('p', { class: 'erreur' }, lu.raison) : null,
      lu.peut ? el('div', { class: 'alerte-donnees confiance-greffon' }, icone('lock'), el('span', {}, t('greffon_confiance'))) : null,
      manquants.length && lu.peut ? el('p', { class: 'note' }, t('greffon_paquets_a_installer', { paquets: manquants.join(', ') })) : null,
      el('h3', {}, t('greffon_code')), codeGreffon(g.fichiers)),
    boutons: lu.peut ? [{ libelle: t('annuler'), action: () => true }, { libelle: t('greffon_installer'), principal: true, action: () => {
      (async () => {
        try {
          await approuver([g.empreinte]);
          await demander('greffon_installer', { ...source, empreinte: g.empreinte });
          app.statut(t('greffon_installe', { nom: texte(g.nom), version: g.version }));
          if (apres) await apres(); else await actualiser(app);
          if (manquants.length) installerPaquets(app, manquants, apres);
        } catch (err) { app.statut(`${t('erreur')}${deuxPoints()}${err.message}`, true); }
      })();
      return true;
    } }] : [{ libelle: t('fermer'), principal: true, action: () => true }] });
}

// A plugin's packages installed into the user library of the R Nuée uses (main.js: r-installer-greffon,
// as File › Settings… installs the engine's), what R says shown meanwhile; in a browser, the line to run.
function installerPaquets(app, paquets, apres = null) {
  const ligne = `install.packages(${paquets.length === 1 ? JSON.stringify(paquets[0]) : `c(${paquets.map((x) => JSON.stringify(x)).join(', ')})`}, repos = "https://cloud.r-project.org")`;
  const sortie = el('pre', { class: 'sortie-r' });
  const etat = el('p', { 'aria-live': 'polite' }, t('greffon_paquets_texte', { paquets: paquets.join(', ') }));
  const r = bureau && bureau.r && bureau.r.installerGreffon;
  const lancer = async () => {
    etat.textContent = t('greffon_paquets_en_cours', { paquets: paquets.join(', ') });
    let fin;
    try { fin = await r(paquets, (s) => { sortie.textContent = (sortie.textContent + s).slice(-20000); sortie.scrollTop = sortie.scrollHeight; }); } catch (err) { fin = { ok: false, fin: err.message }; }
    etat.textContent = fin && fin.ok ? t('greffon_paquets_ok', { paquets: paquets.join(', ') }) : t('greffon_paquets_echec', { paquets: liste(fin && fin.manquants).join(', ') || paquets.join(', ') });
    if (apres) await apres(); else await actualiser(app);
  };
  dialogue({ titre: t('greffon_installer_paquets'), contenu: el('div', { class: 'dialogue-r' }, etat,
    el('div', { class: 'ligne-r' }, el('code', {}, ligne), el('button', { onclick: () => navigator.clipboard.writeText(ligne).catch(() => {}) }, icone('copy'), t('r_copier'))), sortie),
  boutons: r ? [{ libelle: t('fermer'), action: () => true }, { libelle: t('greffon_installer_paquets'), principal: true, action: () => { lancer(); return false; } }]
    : [{ libelle: t('fermer'), principal: true, action: () => true }] });
}

// ------------------------------------------------------------ the views of a plugin's model

// Its card in the summary: the plugin, and what the model chose (its inputs' variables, its options),
// a newer or older version installed here said.
function carteModele(m) {
  const g = m.greffon || {};
  const r = g.reglages || {};
  const valeur = (o) => (o.texte ? texte(o.texte) : typeof o.valeur === 'boolean' ? t(o.valeur ? 'greffon_oui' : 'greffon_non') : o.valeur === '' ? '—' : String(o.valeur));
  const i = g.installe;
  return el('div', { class: 'carte-greffon modele' },
    el('div', { class: 'fiche-titre' }, icone('puzzle'), el('b', {}, texte(g.nom)), el('span', { class: 'badge' }, g.version),
      nomsAuteurs(g.auteurs).length ? el('span', { class: 'doux' }, [t('greffon_par'), ' ', auteursEnLiens(g.auteurs)]) : null),
    el('dl', { class: 'reglages-greffon' },
      liste(r.entrees).flatMap((e) => [el('dt', {}, texte(e.libelle)), el('dd', {}, liste(e.variables).join(', ') || '—')]),
      liste(r.options).flatMap((o) => [el('dt', {}, texte(o.libelle)), el('dd', {}, valeur(o))])),
    i && i.empreinte !== g.empreinte ? el('p', { class: 'note' }, t(i.version === g.version ? 'greffon_version_copie' : 'greffon_version_autre', { version: g.version, installee: i.version })) : null);
}

function rendreResumeGreffon(conteneur, ctx, m) {
  rendreResumeCode(conteneur, ctx, m, { badge: t('greffon_mot'), carte: carteModele(m), boutons: [
    el('button', { onclick: () => appli && appli.modifierModele(m.id) }, icone('square-pen'), t('greffon_modifier')),
    el('button', { onclick: () => ctx.ouvrirVue('greffon_source', m.id) }, icone('puzzle'), t('greffon_le_greffon'))] });
}

// The plugin of the model: its card, its fingerprint and its code, as the project keeps them (the
// engine's: greffon_projet).
function rendreSourceGreffon(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const g = m.greffon || {};
  const zone = el('div', { class: 'defile resume montrer-code' }, carteGreffon(g, { paquets: false }),
    el('p', { class: 'note' }, t('greffon_empreinte', { empreinte: g.empreinte, fonction: g.fonction })));
  conteneur.replaceChildren(zone);
  demander('greffon_projet', { projet: m.projet }).then((r) => {
    const x = liste(r.greffons).find((y) => y.empreinte === g.empreinte);
    if (x && zone.isConnected) zone.append(...codeGreffon(x.fichiers));
  }).catch(() => {});
}

// ------------------------------------------------------------ a project's plugin from elsewhere

// The plugins of each open project that aren't approved on this computer (the engine says so in its
// summary: moteur/serveur.R, resume_modele), one bar each, with Show and what can be done: install it,
// run the project's version, or use the installed one; × hides it until the project changes.
const caches = new Set();
function groupes(p) {
  const par = new Map();
  for (const d of liste(p.modeles)) {
    if (d.type !== 'PLUGIN' || !d.code_r || d.code_r.approuve !== false) continue;
    const g = d.greffon || {};
    const k = g.empreinte || d.code_r.empreinte || '';
    if (!par.has(k)) par.set(k, { ...g, modeles: [] });
    par.get(k).modeles.push(d);
  }
  return [...par.values()];
}
function barresGreffons(app) {
  let zone = document.getElementById('barres-greffons');
  const entete = document.getElementById('entete');
  if (!zone && entete) { zone = el('div', { id: 'barres-greffons' }); entete.append(zone); }
  if (!zone) return;
  const plusieurs = app.projets().length > 1;
  const barres = app.projets().flatMap((p) => groupes(p).map((g) => {
    const cle = `${p.id}|${g.empreinte}|${g.modeles.map((d) => d.numero).join(',')}`;
    if (caches.has(cle)) return null;
    const i = g.installe;
    const valeurs = { projet: p.nom, nom: texte(g.nom) || g.id || '?', version: g.version || '?', auteurs: etEntre(liste(g.auteurs)), modeles: g.modeles.map((d) => d.nom).join(', '),
      installee: i ? i.version : '' };
    const cas = g.valide === false ? 'invalide' : g.interdit ? 'interdit' : !i ? 'absent' : i.version === g.version ? 'copie' : 'autre';
    const texteBarre = `${plusieurs ? `${p.nom}${deuxPoints()}` : ''}${t(`greffon_barre_${cas}`, valeurs)}`;
    return el('div', { class: 'barre-code barre-greffon', role: 'status' }, icone('puzzle'), el('span', {}, texteBarre),
      el('button', { onclick: () => montrerGreffons(app, p.id, g.empreinte) }, icone('file-code'), t('greffon_montrer')),
      actionsBarre(app, p.id, g, cas).map((a, k) => el('button', { class: k === 0 ? 'principal' : '', onclick: a.action }, a.libelle)),
      el('button', { class: 'icone-seule', title: t('greffon_barre_cacher'), 'aria-label': t('greffon_barre_cacher'), onclick: () => { caches.add(cle); barresGreffons(app); } }, icone('x')));
  })).filter(Boolean);
  zone.replaceChildren(...barres);
}

// What the bar and Show offer: install the project's plugin and run it (not installed here); run the
// project's version, or use the installed one (another version, or another copy, installed).
function actionsBarre(app, pid, g, cas) {
  const i = g.installe;
  if (cas === 'absent') return [{ libelle: t('greffon_installer_executer'), action: () => executerGreffon(app, pid, g, 'installer') }];
  if (cas === 'autre' || cas === 'copie') {
    return [{ libelle: t(cas === 'copie' ? 'greffon_executer_copie' : 'greffon_executer_version', { version: g.version }), action: () => executerGreffon(app, pid, g, 'executer') },
      ...(i && (i.source === 'organisation' || installes().some((x) => x.id === g.id && x.permis))
        ? [{ libelle: t('greffon_utiliser_installee', { version: i.version }), action: () => executerGreffon(app, pid, g, 'mettre_a_jour') }] : [])];
  }
  return [];
}

// Show: the project's plugin as it is, its card, what it would do here, and its code.
async function montrerGreffons(app, pid, empreinte = null) {
  let r;
  try { r = await demander('greffon_projet', { projet: pid }); } catch (err) { app.statut(err.message, true); return; }
  const gs = liste(r.greffons).filter((g) => (!empreinte ? !g.permis : g.empreinte === empreinte));
  const p = app.projet(pid) || {};
  if (!gs.length) return;
  const g = gs[0];
  const cas = g.etat === 'invalide' ? 'invalide' : g.interdit ? 'interdit' : !g.installe ? 'absent' : g.installe.version === g.version ? 'copie' : 'autre';
  // (the keyboard on its text and code: Enter doesn't run what wasn't read)
  const d = dialogue({ titre: `${t('greffon_montrer')} — ${p.nom || ''}`, large: true, focus: 'corps',
    contenu: el('div', { class: 'montrer-code' }, el('p', { class: 'note' }, t('greffon_montrer_note')),
      gs.map((x) => [carteGreffon(x), el('p', { class: 'note' }, t('greffon_montrer_modeles', { modeles: liste(x.modeles).map((y) => y.nom).join(', ') })),
        x.installe ? el('p', { class: 'note' }, t(x.installe.version === x.version ? 'greffon_version_copie' : 'greffon_version_autre', { version: x.version, installee: x.installe.version })) : null,
        codeGreffon(x.fichiers)])),
    boutons: [{ libelle: t('fermer'), action: () => true },
      ...actionsBarre(app, pid, { ...g, modeles: liste(g.modeles) }, cas).map((a, k) => ({ libelle: a.libelle, principal: k === 0, action: () => { a.action(); return true; } }))] });
  return d;
}

// Install and run, Run the project's version, Use the installed version; then each of its models
// fitted, their views opened.
async function executerGreffon(app, pid, g, mode) {
  const modeles = liste(g.modeles);
  let numeros = modeles.map((d) => d.numero);
  try {
    if (mode === 'mettre_a_jour') {
      const r = await demander('greffon_mettre_a_jour', { projet: pid, id: g.id });
      numeros = liste(r.numeros);
      for (const x of liste(r.refus)) app.statut(`${x.nom}${deuxPoints()}${x.erreur}`, true);
    } else {
      await approuver([g.empreinte]);
      if (mode === 'installer') await demander('greffon_installer', { projet: pid, numero: numeros[0], empreinte: g.empreinte });
    }
    await actualiser(app);
    for (const n of numeros) await app.calculer(pid, n);
  } catch (err) {
    app.statut(`${t('erreur')}${deuxPoints()}${err.message}`, true);
  }
  try { app.majProjet(await demander('projet', { projet: pid })); } catch { /* the project closed meanwhile */ }
  barresGreffons(app);
}

// ------------------------------------------------------------ registration

const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estGreffon(m) ? rendreResumeGreffon(c, ctx, m) : resume(c, ctx, p, loc);
} };
VUES.greffon_source = { rendre: rendreSourceGreffon, titre: (m) => `${m.nom} · ${t('greffon_le_greffon')}` };
ICONES_VUES.greffon_source = 'puzzle';
// (only the views that have something: a test without coefficients has its values and its tables)
VUES_TYPES.PLUGIN = (m) => ['resume'].concat(liste((m.coefficients || {}).terme).length ? ['code_coefficients'] : [],
  m.ajustees && liste(m.ajustees.ajuste).length ? ['code_diagnostics'] : [], liste(m.images).length ? ['code_graphiques'] : [],
  (m.valeurs && liste(m.valeurs.nom).length) || liste(m.tableaux).length ? ['code_tableaux'] : [], ['greffon_source'], m.prediction ? ['predictions'] : []);
// the report: an R-code model's parts, its summary naming the plugin first
PARTIES_TYPES.PLUGIN = PARTIES_TYPES.CODE.map((x) => {
  if (x.cle === 'resume') return { ...x, tableau: (m) => [{ texte: `${texte((m.greffon || {}).nom)} ${(m.greffon || {}).version || ''} (${t('greffon_mot')}${deuxPoints()}${etEntre(nomsAuteurs((m.greffon || {}).auteurs))})` }, ...x.tableau(m)] };
  if (x.cle === 'code_coefficients') return { ...x, si: (m) => liste((m.coefficients || {}).terme).length > 0 };
  return x;
});

export function installerGreffons(app) {
  appli = app;
  Object.assign(app, {
    gererGreffons: () => gererGreffons(app),
    majGreffons: () => { majAnalyses(); barresGreffons(app); },
    barresGreffons: () => barresGreffons(app),
    modifierGreffon: (m) => modifierGreffon(app, m),
    montrerGreffons: (pid) => montrerGreffons(app, pid),
    executerGreffons: (pid) => { for (const g of groupes(app.projet(pid) || {})) {
      const cas = g.valide === false || g.interdit ? null : !g.installe ? 'installer' : 'executer';
      if (cas) executerGreffon(app, pid, g, cas);
    } },
    // Model › Plugins: each plugin usable here, then File › Plugins…
    menuGreffons: () => [...(utilisables().length ? utilisables().map((g) => ({ libelle: `${texte(g.nom) || g.id}…`, icone: 'puzzle', actif: !!app.projetActif(),
      action: () => nouveauModeleGreffon(app, g) })) : [{ libelle: t('greffons_aucun'), actif: false, action: () => {} }]), '-',
    { libelle: t('greffons'), icone: 'puzzle', action: () => gererGreffons(app) }],
  });
}

ajouterTextes({
  greffons: 'Greffons…', greffons_menu: 'Greffons', greffons_titre: 'Greffons', greffon_mot: 'greffon', greffon_et: 'et', greffon_oui: 'oui', greffon_non: 'non',
  greffons_intro: 'Un greffon ajoute à Nuée un type de modèle écrit en R par quelqu’un d’autre : son dialogue, ses résultats, son texte des méthodes et sa citation, son export vers R. Il se partage en un fichier .nueeplugin ; un projet qui l’utilise le garde, et se refait ailleurs.',
  greffons_aucun: 'Aucun greffon installé', greffons_aucun_installe: 'Aucun greffon n’est installé. Les deux greffons d’exemple (occupation des sites, Mann–Kendall) se téléchargent sur le site de la documentation.',
  greffons_politique: 'Une politique de votre organisation (DisableCustomCode) ne permet que les greffons qu’elle donne : aucun autre ne s’installe ni ne s’exécute.',
  greffons_bureau: 'Les greffons s’installent depuis l’application de bureau.',
  greffons_dossier: 'Les vôtres sont dans {dossier}.', greffons_dossier_organisation: 'Ceux de votre organisation, dans {dossier}.',
  greffon_installer_fichier: 'Installer depuis un fichier…', greffon_installer_note: 'Un .nueeplugin, ou le plugin.json du dossier d’un greffon que vous écrivez.',
  greffon_filtre: 'Greffon de Nuée', greffon_installer: 'Installer', greffon_installer_titre: 'Installer le greffon « {nom} » {version} ?',
  greffon_confiance: 'Un greffon est du code R : il s’exécute avec vos droits, comme du code tapé dans la console R, et peut lire ou changer vos fichiers. Ne l’installez que si vous faites confiance à sa source. L’installer l’approuve sur cet ordinateur : les modèles faits avec lui s’ajustent sans le redemander.',
  greffon_code: 'Son code', greffon_deja: 'Cette version est déjà installée, à l’identique.',
  greffon_remplace_ancienne: 'Il remplace la version {version}, plus ancienne, installée ici.', greffon_remplace_recente: 'Attention : la version {version} installée ici est plus récente ; il la remplace.',
  greffon_remplace_copie: 'Il remplace une autre copie de la version {version}, qui diffère de celle-ci.',
  greffon_installe: 'Greffon « {nom} » {version} installé.', greffon_paquets_a_installer: 'Il demande des paquets que le R qu’utilise Nuée n’a pas : {paquets}. Nuée propose de les installer ensuite.',
  greffon_organisation: 'de votre organisation', greffon_vous: 'installé par vous', greffon_invalide: 'a des erreurs', greffon_double: 'en double',
  greffon_remplace: 'remplacé par celui de votre organisation', greffon_change: 'changé depuis son installation : ne s’exécute pas', greffon_trop_recent: 'demande Nuée {nuee}',
  greffon_par: 'par', greffon_licence: 'licence', greffon_nuee: 'Nuée {nuee} ou plus récent', greffon_erreurs: 'Ses erreurs :', greffon_paquets: 'Paquets R', greffon_absent: 'absent',
  greffon_citer: 'Le citer ({n})', greffon_copier_bibtex: 'Copier le BibTeX', greffon_bibtex_copie: 'BibTeX copié',
  greffon_nouveau_modele: 'Nouveau modèle…', greffon_installer_paquets: 'Installer les paquets manquants', greffon_montrer: 'Montrer', greffon_retirer: 'Retirer',
  greffon_retirer_titre: 'Retirer le greffon « {nom} » ?', greffon_retirer_texte: 'Il quitte vos menus. Les projets qui l’utilisent le gardent : ils se refont toujours avec lui sur cet ordinateur (son approbation reste).',
  greffon_paquets_texte: 'Ce greffon demande des paquets que le R qu’utilise Nuée n’a pas : {paquets}. Nuée peut les installer depuis le CRAN dans la bibliothèque de l’utilisateur de ce R (quelques minutes), ou lancez cette ligne dans ce R :',
  greffon_paquets_en_cours: 'Installation de {paquets} depuis le CRAN…', greffon_paquets_ok: 'Installés : {paquets}. Ajustez de nouveau le modèle.', greffon_paquets_echec: 'Pas installés : {paquets}. Lancez la ligne dans ce R (R ou RStudio).',
  greffon_note_version: '(Version {version}, de {auteurs}.)', greffon_choisir: 'Choisir…', greffon_aucune: '—', greffon_aucune_variable: 'Aucune variable de ce type dans le projet.',
  greffon_n_choisies: '{n} choisie(s)', greffon_entre: 'de {min} à {max}', greffon_au_moins: 'au moins {min}', greffon_cocher: 'Cocher celles-ci', greffon_decocher: 'Décocher celles-ci',
  greffon_entree_requise: '{nom} : choisissez une variable.', greffon_entree_min: '{nom} : choisissez au moins {min} variable(s).', greffon_entree_max: '{nom} : {max} variable(s) au plus.',
  greffon_option_nombre: '{nom} : un nombre.', greffon_option_min: '{nom} : {min} au moins.', greffon_option_max: '{nom} : {max} au plus.', greffon_option_entier: '{nom} : un entier.',
  greffon_version: 'Version du greffon', greffon_version_seule: 'Ce modèle est fait de la version {version} du greffon, que le projet garde ; elle n’est pas installée ici.',
  greffon_version_autre: 'Ce modèle est fait de la version {version} du greffon, que le projet garde ; la version {installee} est installée ici.',
  greffon_version_copie: 'La copie du greffon {version} que garde le projet diffère de celle installée ici.', greffon_utiliser_installee: 'Utiliser la version {version}',
  greffon_modifier: 'Modifier le modèle', greffon_le_greffon: 'Le greffon', greffon_source: 'Le greffon', greffon_empreinte: 'Empreinte SHA-256 de ses fichiers : {empreinte}. Sa fonction : {fonction}(donnees, entrees, options).',
  greffon_barre_absent: 'Ce projet utilise le greffon « {nom} » {version}, de {auteurs} ({modeles}), qui n’est pas installé sur cet ordinateur : ses modèles ne s’ajustent pas tant que vous ne l’avez pas installé.',
  greffon_barre_autre: 'Ce projet utilise la version {version} du greffon « {nom} », de {auteurs} ({modeles}) ; cet ordinateur a la version {installee}. Ses modèles attendent que vous choisissiez.',
  greffon_barre_copie: 'La copie du greffon « {nom} » {version} que garde ce projet ({modeles}) diffère de celle installée ici : ses modèles attendent que vous choisissiez.',
  greffon_barre_interdit: 'Ce projet utilise le greffon « {nom} » {version} ({modeles}) : une politique de votre organisation ne permet que les greffons qu’elle donne.',
  greffon_barre_invalide: 'La copie du greffon « {nom} » que garde ce projet ({modeles}) a des erreurs : ses modèles ne s’ajustent pas.',
  greffon_barre_cacher: 'Cacher (les modèles restent non ajustés)', greffon_installer_executer: 'Installer et exécuter',
  greffon_executer_version: 'Exécuter la version {version}', greffon_executer_copie: 'Exécuter la copie du projet',
  greffon_montrer_note: 'Ce greffon vient du fichier du projet : il a été installé ou écrit ailleurs. L’exécuter le lance avec vos droits, comme du code tapé dans la console R ; ne le faites que si vous lui faites confiance.',
  greffon_montrer_modeles: 'Ses modèles dans ce projet : {modeles}.',
  greffon_attente: 'Le modèle {nom} est fait d’un greffon qui n’est pas approuvé sur cet ordinateur : il attend (Montrer, dans la barre au-dessus du projet).',
  greffon_non_execute: 'greffon venu d’ailleurs, pas exécuté', greffon_non_execute_aide: 'Son greffon n’est pas installé ni approuvé sur cet ordinateur : il ne s’exécute pas tant que vous ne l’avez pas lu et installé. Double-clic : le montrer.',
}, {
  greffons: 'Plugins…', greffons_menu: 'Plugins', greffons_titre: 'Plugins', greffon_mot: 'plugin', greffon_et: 'and', greffon_oui: 'yes', greffon_non: 'no',
  greffons_intro: 'A plugin adds a model type written in R by someone else to Nuée: its dialog, its results, its Methods text and citation, its Export to R. It is shared as a .nueeplugin file; a project that uses it keeps it, and can be redone elsewhere.',
  greffons_aucun: 'No plugin installed', greffons_aucun_installe: 'No plugin is installed. The two example plugins (site occupancy, Mann–Kendall) can be downloaded from the documentation website.',
  greffons_politique: 'Your organisation’s policy (DisableCustomCode) only allows the plugins it provides: no other installs or runs.',
  greffons_bureau: 'Plugins are installed from the desktop application.',
  greffons_dossier: 'Yours are in {dossier}.', greffons_dossier_organisation: 'Your organisation’s, in {dossier}.',
  greffon_installer_fichier: 'Install from file…', greffon_installer_note: 'A .nueeplugin, or the plugin.json of the folder of a plugin you are writing.',
  greffon_filtre: 'Nuée plugin', greffon_installer: 'Install', greffon_installer_titre: 'Install the plugin “{nom}” {version}?',
  greffon_confiance: 'A plugin is R code: it runs with your rights, like code typed in the R console, and can read or change your files. Install it only if you trust where it comes from. Installing it approves it on this computer: models made with it are fitted without asking again.',
  greffon_code: 'Its code', greffon_deja: 'This version is already installed, the same.',
  greffon_remplace_ancienne: 'It replaces version {version}, older, installed here.', greffon_remplace_recente: 'Careful: version {version} installed here is newer; this replaces it.',
  greffon_remplace_copie: 'It replaces another copy of version {version}, which differs from this one.',
  greffon_installe: 'Plugin “{nom}” {version} installed.', greffon_paquets_a_installer: 'It needs packages the R Nuée uses doesn’t have: {paquets}. Nuée offers to install them next.',
  greffon_organisation: 'your organisation’s', greffon_vous: 'installed by you', greffon_invalide: 'has errors', greffon_double: 'duplicate',
  greffon_remplace: 'replaced by your organisation’s', greffon_change: 'changed since it was installed: doesn’t run', greffon_trop_recent: 'needs Nuée {nuee}',
  greffon_par: 'by', greffon_licence: 'licence', greffon_nuee: 'Nuée {nuee} or newer', greffon_erreurs: 'Its errors:', greffon_paquets: 'R packages', greffon_absent: 'missing',
  greffon_citer: 'Citing it ({n})', greffon_copier_bibtex: 'Copy the BibTeX', greffon_bibtex_copie: 'BibTeX copied',
  greffon_nouveau_modele: 'New model…', greffon_installer_paquets: 'Install the missing packages', greffon_montrer: 'Show', greffon_retirer: 'Remove',
  greffon_retirer_titre: 'Remove the plugin “{nom}”?', greffon_retirer_texte: 'It leaves your menus. The projects that use it keep it: they are still redone with it on this computer (its approval stays).',
  greffon_paquets_texte: 'This plugin needs packages the R Nuée uses doesn’t have: {paquets}. Nuée can install them from CRAN into that R’s user library (a few minutes), or run this line in that R:',
  greffon_paquets_en_cours: 'Installing {paquets} from CRAN…', greffon_paquets_ok: 'Installed: {paquets}. Fit the model again.', greffon_paquets_echec: 'Not installed: {paquets}. Run the line in that R (R or RStudio).',
  greffon_note_version: '(Version {version}, by {auteurs}.)', greffon_choisir: 'Choose…', greffon_aucune: '—', greffon_aucune_variable: 'No variable of this kind in the project.',
  greffon_n_choisies: '{n} chosen', greffon_entre: '{min} to {max}', greffon_au_moins: 'at least {min}', greffon_cocher: 'Tick these', greffon_decocher: 'Untick these',
  greffon_entree_requise: '{nom}: choose a variable.', greffon_entree_min: '{nom}: choose at least {min} variable(s).', greffon_entree_max: '{nom}: {max} variable(s) at most.',
  greffon_option_nombre: '{nom}: a number.', greffon_option_min: '{nom}: {min} at least.', greffon_option_max: '{nom}: {max} at most.', greffon_option_entier: '{nom}: a whole number.',
  greffon_version: 'Plugin version', greffon_version_seule: 'This model is made with version {version} of the plugin, which the project keeps; it isn’t installed here.',
  greffon_version_autre: 'This model is made with version {version} of the plugin, which the project keeps; version {installee} is installed here.',
  greffon_version_copie: 'The copy of the plugin {version} the project keeps differs from the one installed here.', greffon_utiliser_installee: 'Use version {version}',
  greffon_modifier: 'Edit the model', greffon_le_greffon: 'The plugin', greffon_source: 'The plugin', greffon_empreinte: 'SHA-256 fingerprint of its files: {empreinte}. Its function: {fonction}(donnees, entrees, options).',
  greffon_barre_absent: 'This project uses the plugin “{nom}” {version}, by {auteurs} ({modeles}), which isn’t installed on this computer: its models aren’t fitted until you install it.',
  greffon_barre_autre: 'This project uses version {version} of the plugin “{nom}”, by {auteurs} ({modeles}); this computer has version {installee}. Its models wait for you to choose.',
  greffon_barre_copie: 'The copy of the plugin “{nom}” {version} this project keeps ({modeles}) differs from the one installed here: its models wait for you to choose.',
  greffon_barre_interdit: 'This project uses the plugin “{nom}” {version} ({modeles}): your organisation’s policy only allows the plugins it provides.',
  greffon_barre_invalide: 'The copy of the plugin “{nom}” this project keeps ({modeles}) has errors: its models aren’t fitted.',
  greffon_barre_cacher: 'Hide (the models stay not fitted)', greffon_installer_executer: 'Install and run',
  greffon_executer_version: 'Run version {version}', greffon_executer_copie: 'Run the project’s copy',
  greffon_montrer_note: 'This plugin comes from the project’s file: it was installed or written elsewhere. Running it starts it with your rights, like code typed in the R console; do it only if you trust it.',
  greffon_montrer_modeles: 'Its models in this project: {modeles}.',
  greffon_attente: 'Model {nom} is made with a plugin that isn’t approved on this computer: it waits (Show, in the bar above the project).',
  greffon_non_execute: 'plugin from elsewhere, not run', greffon_non_execute_aide: 'Its plugin isn’t installed or approved on this computer: it doesn’t run until you have read and installed it. Double-click: show it.',
});
