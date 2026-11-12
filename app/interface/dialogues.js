// Dialogs: model definition (type, variables, observations), help and about.
import { t, langueActuelle } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander } from './api.js';
import { libelleTransfo, lireTransfo, codeTransfo, TYPES_TRANSFO, FORMAT, changerFormat } from './panneaux.js';

const ECHELLES = ['UV', 'Par', 'Ctr', 'none'];
const TRANSFOS = ['', 'racine2', 'racine4', 'log10'];
const PERSO = '~perso';  // "Custom…": opens editeurTransfo

// The transformations offered for a variable: the usual ones, its own if it is another
// (a custom one, or 'autre' from a .usp file), and "Custom…".
const optionsTransfo = (courant) => TRANSFOS.map((k) => [k, t('tr_' + (k || 'aucune'))])
  .concat(courant && !TRANSFOS.includes(courant) ? [[courant, libelleTransfo(courant)]] : [])
  .concat([[PERSO, t('tr_perso')]]);

// Custom transformation: its type and constants, and the formula as it will be applied
// (moteur/R/pretraitement.R, transformer). `choisir(code)` on OK; `apres` once closed.
function editeurTransfo(initial, choisir, apres) {
  const s = { type: 'log10', c1: 1, c2: 1, c3: 0.5, ...(lireTransfo(initial) || {}) };
  const formule = el('b');
  const erreur = el('p', { class: 'erreur' });
  const champs = el('div', { class: 'grille-champs' });
  const valide = () => [s.c1, s.c2].concat(s.type === 'puissance' ? [s.c3] : []).every(Number.isFinite) && (s.type !== 'logit' || s.c1 < s.c2);
  const maj = () => {
    formule.textContent = valide() ? (libelleTransfo(codeTransfo(s)) || t('tr_aucune')) : '—';
    erreur.textContent = '';
  };
  const nombre = (cle) => el('input', { type: 'number', step: 'any', value: s[cle], style: 'width:8em',
    oninput: (e) => { s[cle] = parseFloat(e.target.value); maj(); } });
  const rendreChamps = () => {
    champs.replaceChildren(...[
      el('label', {}, t('type')), el('select', { onchange: (e) => {
        s.type = e.target.value;
        if (s.type === 'logit' && !(s.c1 < s.c2)) Object.assign(s, { c1: 0, c2: 1 });
        rendreChamps();
      } }, TYPES_TRANSFO.map((k) => el('option', { value: k, selected: k === s.type }, t('tr_type_' + k)))),
      el('label', {}, s.type === 'logit' ? t('tr_borne_basse') : 'C1'), nombre('c1'),
      el('label', {}, s.type === 'logit' ? t('tr_borne_haute') : 'C2'), nombre('c2'),
      s.type === 'puissance' ? [el('label', {}, 'C3'), nombre('c3')] : null,
      el('label', {}, t('tr_formule')), formule,
      s.type === 'logit' ? [el('label', {}, ''), el('p', { class: 'note' }, t('tr_logit_note'))] : null,
      el('label', {}, ''), el('p', { class: 'note' }, t('tr_domaine_note'))].flat().filter(Boolean));  // (replaceChildren writes null as text)
    maj();
  };
  rendreChamps();
  dialogue({ titre: t('tr_perso_titre'), contenu: el('div', {}, champs, erreur), surFermeture: apres,
    boutons: [{ libelle: t('annuler'), action: () => true },
      { libelle: 'OK', principal: true, action: () => {
        if (!valide()) { erreur.textContent = t('tr_invalide'); return false; }
        choisir(codeTransfo(s));
        return true;
      } }] });
}
// Other analyses (moteur/R/analyses.R: RDA, CA, LDA, TREE, KMEANS), each registered by its module
// (vues_*.js) under its type:
//   libelle(): its name in the Type list; note(): a note under it;
//   roles: the roles the Variables tab offers, among 'x', 'y', 'classe' (a qualitative variable)
//     and 'z' (a covariate of a partial analysis, sent as options.z);
//   echelle: the default scaling chosen with it; vc: false when cross-validation doesn't apply;
//   spectres: true when spectral preprocessing applies;
//   champs(st, rendre): its own fields in the General tab (label, element pairs), their values
//     kept in st.options; valider(st, cles): '' or what's missing; options(st, cles): the options sent.
export const ANALYSES = {};

// spectral preprocessing steps (moteur/R/spectres.R); sg1 = Savitzky–Golay 1st derivative
const ETAPES = ['', 'snv', 'msc', 'detrend', 'ligne_base', 'sg0', 'sg1', 'sg2'];

// projet: project summary; r: settings of an existing model to start from (m.reglages).
export function dialogueModele(app, projet, r = null, titre = t('nouveau_modele')) {
  const p = projet;
  const st = {
    type: r ? r.type : 'PLS', nom: nomLibre(p), titre: r && r.nom ? `${r.titre ? r.titre + ' — ' : ''}${langueActuelle() === 'fr' ? 'd’après' : 'from'} ${r.nom}` : '',
    A: r ? r.A : 0, auto: !r, G: r ? r.G || 7 : 7, echelle: 'UV', roles: {}, echelles: {}, transfos: {},
    // cross-validation groups: null = in turn (G groups), else { by: 'id', id } | { by: 'variable', variable } | { by: 'groups', … } (moteur/R/validation_croisee.R)
    vc: r && r.vc && r.vc.by ? r.vc : null, blocs: {}, modificateurs: {}, bloc: '',
    exclues: new Set(r ? [].concat(r.exclues || []) : []), onglet: 'variables', filtre: '', filtreObs: '',
    test: new Set(r ? [].concat(r.test || []).map(Number) : []), methodeTest: 'hasard', partTest: 25,
    croises: r && r.croises ? r.croises.map((c) => [].concat(c).map(Number)) : [], a: '', b: '', c: '',
    // each cross term's scaling, block and modifier, by its key "3*5" (moteur/R/nouveau.R: croises_reglages)
    croisesReglages: r && r.croises_reglages && !Array.isArray(r.croises_reglages) ? { ...r.croises_reglages } : {},
    // a mixed model: nested random effects, the widest first, and an autocorrelation in time
    aleatoires: r && r.aleatoires ? [].concat(r.aleatoires) : [], correlation: r && r.correlation ? r.correlation : null,
    // a generalized or additive model: the distribution of Y, the X in curves, the effort (offset)
    famille: r && r.famille ? r.famille : 'gaussienne', lisses: new Set(r && r.lisses ? [].concat(r.lisses).map(Number) : []),
    // each curve's basis dimension (k), by variable; none: Nuée sets it (moteur/R/regression.R: dimension_courbe)
    kCourbes: r && r.k_courbes && !Array.isArray(r.k_courbes) ? Object.fromEntries(Object.entries(r.k_courbes).map(([u, k]) => [u, Number([].concat(k)[0])])) : {},
    decalage: r && r.decalage !== undefined && r.decalage !== null ? Number(r.decalage) : null,
    // a regression as an R formula: written by R from the settings, or edited (not applied yet)
    formule: '', formuleModifiee: false,
    // another analysis's own settings (ANALYSES), as definir_modele() keeps them (d$options)
    options: r && r.options && !Array.isArray(r.options) ? { ...r.options } : {},
  };
  if (r && ['LM', 'LMM', 'GLM', 'GAM'].includes(r.type) && !Object.keys(r.echelles || {}).length) st.echelle = 'none';
  const etapes = r && r.spectres ? [].concat(r.spectres) : [];
  const sg = etapes.find((e) => e.type === 'sg');
  Object.assign(st, { etapes: [0, 1, 2].map((i) => (etapes[i] ? (etapes[i].type === 'sg' ? `sg${etapes[i].derivee || 0}` : etapes[i].type) : '')),
    fenetre: sg ? sg.fenetre : 11, degre: sg ? sg.degre : 2 });
  if (r) {
    [].concat(r.x || []).forEach((u) => { st.roles[u] = 'x'; });
    [].concat(r.y || []).forEach((u) => { st.roles[u] = 'y'; });
    if (r.classe !== null && r.classe !== undefined) st.roles[r.classe] = 'classe';
    [].concat((r.options && r.options.z) || []).forEach((u) => { st.roles[u] = 'z'; });  // (a partial analysis's covariates)
    Object.assign(st.echelles, r.echelles || {});
    Object.assign(st.transfos, r.transformations || {});
    Object.assign(st.blocs, r.blocs || {});
    Object.assign(st.modificateurs, r.modificateurs || {});
  }
  const contenu = el('div', { class: 'dialogue-modele' });
  const erreur = el('p', { class: 'erreur' });
  const resume = el('span', { class: 'infos' });

  const majResume = () => {
    const x = Object.values(st.roles).filter((v) => v === 'x').length;
    const y = Object.values(st.roles).filter((v) => v === 'y' || v === 'classe').length;
    const test = [...st.test].filter((u) => !st.exclues.has(u)).length;
    resume.textContent = t('resume_dialogue', { x, y, n: p.observations.uid.length - st.exclues.size - test }) + (test ? ` · ${t('n_test', { n: test })}` : '');
  };

  const onglets = () => el('div', { class: 'onglets' }, [['general', t('general')], ['variables', t('variables')],
    ['termes', `${t('termes')}${st.croises.length ? ` (${st.croises.length})` : ''}`], ['observations', t('observations')]]
    .map(([k, n]) => el('button', { class: st.onglet === k ? 'actif' : '', onclick: () => { st.onglet = k; rendre(); } }, n)));

  // Cross-validation groups: in turn (G groups), one group per value of an observation ID or of
  // a variable (all the years of a site in one group: the model is judged on sites it has never
  // seen), or the groups stored in the .usp file the model comes from.
  const vcFichier = r && r.vc && r.vc.by === 'groups' ? r.vc : null;
  const groupesVC = () => {
    const valeur = !st.vc ? 'ordre' : st.vc.by === 'id' ? `id:${st.vc.id}` : st.vc.by === 'variable' ? `var:${st.vc.variable}` : 'fichier';
    const ids = [].concat(p.identifiants || []).map((i) => i.nom);
    if (st.vc && st.vc.by === 'id' && !ids.includes(st.vc.id)) ids.unshift(st.vc.id);
    const choisir = (v) => {
      if (v === 'ordre') st.vc = null;
      else if (v === 'fichier') st.vc = vcFichier;
      else if (v.startsWith('id:')) st.vc = { by: 'id', id: v.slice(3) };
      else st.vc = { by: 'variable', variable: Number(v.slice(4)) };
      rendre();
    };
    const option = (v, n) => el('option', { value: v, selected: v === valeur }, n);
    // a wide table (spectra): qualitative variables only, and the one chosen
    const vars = p.variables.length <= 200 ? p.variables
      : p.variables.filter((v) => v.qualitative || (st.vc && st.vc.by === 'variable' && v.uid === st.vc.variable));
    return el('div', { class: 'ligne' },
      el('select', { onchange: (e) => choisir(e.target.value) },
        option('ordre', t('vc_ordre')),
        vcFichier ? option('fichier', t('vc_fichier')) : null,
        ids.length ? el('optgroup', { label: t('vc_par_identifiant') }, ids.map((n) => option(`id:${n}`, n))) : null,
        vars.length ? el('optgroup', { label: t('vc_par_variable') }, vars.map((v) => option(`var:${v.uid}`, v.nom))) : null),
      st.vc ? null : el('input', { type: 'number', min: 2, max: 50, value: st.G, style: 'width:5em', title: t('vc_nombre'),
        oninput: (e) => { st.G = +e.target.value || 7; } }));
  };

  // Regressions (moteur/R/regression.R): one Y, no components or spectra; a mixed model takes
  // nested random effects, from an observation ID or a qualitative variable, and an optional
  // autocorrelation in time within the last one (a numeric ID or a quantitative variable).
  const regression = () => ['LM', 'LMM', 'GLM', 'GAM'].includes(st.type);
  const analyse = () => ANALYSES[st.type] || null;  // another analysis (RDA, LDA...)
  const sansSpectres = () => regression() || (!!analyse() && !analyse().spectres);
  const generalise = () => ['GLM', 'GAM'].includes(st.type);
  const FAMILLES = ['gaussienne', 'poisson', 'binomiale_negative', 'binomiale', 'gamma', 'tweedie'];
  const FAMILLES_LOG = ['poisson', 'binomiale_negative', 'gamma', 'tweedie'];  // (an offset needs a log link)
  const loi = () => [el('label', {}, t('loi')), el('select', { onchange: (e) => { st.famille = e.target.value; rendre(); } },
    FAMILLES.map((k) => el('option', { value: k, selected: k === st.famille }, t('famille_' + k))))]
    .concat(FAMILLES_LOG.includes(st.famille) ? [el('label', {}, t('decalage')), el('select', { title: t('decalage_aide'),
      onchange: (e) => { st.decalage = e.target.value ? Number(e.target.value) : null; } },
    [el('option', { value: '', selected: st.decalage === null }, t('aucun'))].concat(p.variables.filter((v) => !v.qualitative)
      .map((v) => el('option', { value: v.uid, selected: v.uid === st.decalage }, v.nom))))] : []);
  const codeSpec = (s) => (!s ? '' : s.by === 'id' ? `id:${s.id}` : `var:${s.variable}`);
  const lireSpec = (v) => (!v ? null : v.startsWith('id:') ? { by: 'id', id: v.slice(3) } : { by: 'variable', variable: Number(v.slice(4)) });
  const groupements = (temps = false) => [['', '—']]
    .concat([].concat(p.identifiants || []).map((i) => [`id:${i.nom}`, `${i.nom} (${t('identifiant')})`]),
      p.variables.filter((v) => (temps ? !v.qualitative : v.qualitative)).map((v) => [`var:${v.uid}`, v.nom]));
  const aleatoires = () => el('div', { class: 'ligne' }, [0, 1, 2].slice(0, Math.min(3, st.aleatoires.length + 1)).map((i) => el('select', {
    title: t('aleatoire_n', { n: i + 1 }),
    onchange: (e) => {
      const s = lireSpec(e.target.value);
      if (s) st.aleatoires[i] = s; else st.aleatoires.splice(i);  // (the finer ones go with it)
      if (i === 0 && s && !st.vc) st.vc = s;  // validated by the widest one: a site never seen
      rendre();
    } }, groupements().map(([k, n]) => el('option', { value: k, selected: k === codeSpec(st.aleatoires[i]) }, n)))));
  // (a mixed model; a generalized or additive one with the normal distribution and random effects, then fitted by mgcv::gamm)
  const avecCar1 = () => st.type === 'LMM' || (generalise() && st.famille === 'gaussienne' && st.aleatoires.length > 0);
  const autocorrelation = () => el('div', {},
    el('div', { class: 'ligne' },
      el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: !!st.correlation,
        onchange: (e) => { st.correlation = e.target.checked ? { type: 'CAR1', temps: null } : null; rendre(); } }), t('car1')),
      st.correlation ? el('select', { onchange: (e) => { st.correlation.temps = lireSpec(e.target.value); } },
        groupements(true).map(([k, n]) => el('option', { value: k, selected: k === codeSpec(st.correlation.temps) }, n))) : null),
    // within the last random effect's groups, or a finer level (the sector within its site)
    st.correlation ? el('div', { class: 'ligne' }, el('span', { class: 'doux' }, t('car1_dans')),
      el('select', { title: t('car1_dans_aide'), onchange: (e) => {
        const s = lireSpec(e.target.value);
        if (s) st.correlation.dans = s; else delete st.correlation.dans;
      } }, [['', t('car1_dernier')]].concat(groupements().slice(1)).map(([k, n]) => el('option', { value: k, selected: k === codeSpec(st.correlation.dans) }, n)))) : null);

  // The regression as an R formula (moteur/R/formule.R): written by R from the settings; edited
  // and applied (Ctrl+Enter), it sets them: roles, transformations, cross terms, curves, random
  // effects, effort. R reads it without ever running it, and it isn't kept.
  const reglagesFormule = () => {
    const cles = (r2) => Object.keys(st.roles).filter((k) => st.roles[k] === r2).map(Number);
    return { projet: p.id, type: st.type, x: cles('x'), y: cles('y').slice(0, 1), croises: st.croises,
      transformations: Object.fromEntries(Object.entries(st.transfos).filter(([u, v]) => v && v !== 'autre' && ['x', 'y'].includes(st.roles[u]))),
      lisses: st.type === 'GAM' ? [...st.lisses].filter((u) => st.roles[u] === 'x') : [], k_courbes: kCourbesEnvoyes(),
      aleatoires: ['LMM', 'GLM', 'GAM'].includes(st.type) ? st.aleatoires : [],
      decalage: generalise() && FAMILLES_LOG.includes(st.famille) ? st.decalage : null };
  };
  let derniereFormule = 0;
  const formule = () => {
    const zone = el('textarea', { class: 'formule', rows: 2, spellcheck: 'false', 'aria-label': t('formule_r'),
      oninput: (e) => { st.formule = e.target.value; st.formuleModifiee = true; appliquer.disabled = false; defaire.disabled = false; },
      onkeydown: (e) => { if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) { e.preventDefault(); e.stopPropagation(); appliquerFormule(); } } },
    st.formuleModifiee ? st.formule : '');
    const appliquer = el('button', { disabled: !st.formuleModifiee, onclick: () => { appliquerFormule(); } }, t('appliquer'));
    const defaire = el('button', { disabled: !st.formuleModifiee, title: t('formule_defaire_aide'), onclick: () => { st.formuleModifiee = false; rendre(); } },
      t('formule_defaire'));
    if (!st.formuleModifiee) {
      const n = ++derniereFormule;
      demander('formule', reglagesFormule(), { silencieux: true }).then((res) => {
        if (n === derniereFormule && !st.formuleModifiee) { st.formule = res.formule; zone.value = res.formule; }
      }).catch(() => {});
    }
    return el('div', { class: 'formule-r' }, zone, el('div', { class: 'ligne' }, appliquer, defaire, el('span', { class: 'note' }, t('formule_note'))));
  };
  // The formula is the whole model: the X and Y of the others are cleared.
  async function appliquerFormule() {
    erreur.textContent = '';
    let f;
    try {
      f = await demander('lire_formule', { projet: p.id, texte: st.formule, type: st.type, famille: st.famille });
    } catch (e) {
      erreur.textContent = e.message;
      return false;
    }
    for (const k of Object.keys(st.roles)) if (['x', 'y'].includes(st.roles[k])) delete st.roles[k];
    const x = [].concat(f.x || []).map(Number);
    const y = [].concat(f.y || []).map(Number);
    x.forEach((u) => { st.roles[u] = 'x'; });
    y.forEach((u) => { st.roles[u] = 'y'; });
    for (const u of x.concat(y)) delete st.transfos[u];
    Object.assign(st.transfos, f.transformations || {});
    st.croises = [].concat(f.croises || []).map((c) => [].concat(c).map(Number));
    st.lisses = new Set([].concat(f.lisses || []).map(Number));
    st.kCourbes = f.k_courbes && !Array.isArray(f.k_courbes) ? Object.fromEntries(Object.entries(f.k_courbes).map(([u, k]) => [u, Number([].concat(k)[0])])) : {};
    st.aleatoires = [].concat(f.aleatoires || []);
    st.decalage = f.decalage === null || f.decalage === undefined ? null : Number(f.decalage);
    st.type = f.type;
    if (st.aleatoires.length && !st.vc) st.vc = st.aleatoires[0];  // validated by the widest one, as the field does
    st.formuleModifiee = false;
    rendre();
    return true;
  }

  const general = () => el('div', { class: 'grille-champs' },
    el('label', {}, t('type')), el('select', { onchange: (e) => {
      const avant = regression();
      st.type = e.target.value;
      if (!r && regression() !== avant) st.echelle = regression() ? 'none' : 'UV';  // a regression keeps the units
      if (!r && analyse()) st.echelle = analyse().echelle || 'UV';
      rendre();
    } },
      [['PCA', 'ACP / PCA'], ['PLS', 'PLS'], ['PLS-DA', 'PLS-DA'], ['OPLS', 'OPLS'], ['OPLS-DA', 'OPLS-DA'], ['CLASSES', t('modeles_classe')],
        ['LM', t('type_lm')], ['LMM', t('type_lmm')], ['GLM', t('type_glm')], ['GAM', t('type_gam')]]
        .concat(Object.entries(ANALYSES).map(([k, a]) => [k, a.libelle()]))
        .map(([k, n]) => el('option', { value: k, selected: k === st.type }, n))),
    analyse() && analyse().note ? [el('label', {}, ''), el('p', { class: 'note' }, analyse().note())] : null,
    analyse() && analyse().champs ? analyse().champs(st, rendre) : null,
    regression() ? [el('label', {}, ''), el('p', { class: 'note' }, t({ LM: 'lm_note', LMM: 'lmm_note', GLM: 'glm_note', GAM: 'gam_note' }[st.type]))] : null,
    generalise() ? loi() : null,
    ['LMM', 'GLM', 'GAM'].includes(st.type) ? [el('label', {}, t('effets_aleatoires')), aleatoires()] : null,
    avecCar1() ? [el('label', {}, t('autocorrelation')), autocorrelation()] : null,
    avecCar1() && generalise() && st.correlation ? [el('label', {}, ''), el('p', { class: 'note' }, t('car1_gamm_note'))] : null,
    regression() ? [el('label', { title: t('formule_aide') }, t('formule_r')), formule()] : null,
    ['OPLS', 'OPLS-DA'].includes(st.type) ? el('label', {}, '') : null,
    ['OPLS', 'OPLS-DA'].includes(st.type) ? el('p', { class: 'note' }, t('opls_note')) : null,
    st.type === 'CLASSES' ? el('label', {}, '') : null,
    st.type === 'CLASSES' ? el('p', { class: 'note' }, t('classes_note')) : null,
    el('label', {}, t('nom')), el('input', { value: st.nom, oninput: (e) => { st.nom = e.target.value; } }),
    el('label', {}, t('titre_modele')), el('input', { value: st.titre, oninput: (e) => { st.titre = e.target.value; } }),
    regression() || analyse() ? null : [el('label', {}, t('nb_comp')), el('div', { class: 'ligne' },
      el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: st.auto, onchange: (e) => { st.auto = e.target.checked; rendre(); } }), t('auto')),
      el('input', { type: 'number', min: 1, max: 50, value: st.A || 2, disabled: st.auto, style: 'width:5em', oninput: (e) => { st.A = +e.target.value; } }))],
    analyse() && analyse().vc === false ? null : [el('label', {}, t('groupes_vc')), groupesVC()],
    st.vc && !(analyse() && analyse().vc === false) ? [el('label', {}, ''), el('p', { class: 'note' }, t(st.vc.by === 'groups' ? 'vc_fichier_note' : 'vc_note'))] : null,
    el('label', {}, t('echelle_defaut')), el('select', { onchange: (e) => { st.echelle = e.target.value; rendre(); } },
      ECHELLES.map((k) => el('option', { value: k, selected: k === st.echelle }, t('echelle_' + k)))),
    regression() ? null : el('label', {}, t('spectres')), regression() ? null : el('div', { class: 'ligne' },
      st.etapes.map((code, i) => el('select', { title: `${i + 1}`, onchange: (e) => { st.etapes[i] = e.target.value; rendre(); } },
        ETAPES.map((k) => el('option', { value: k, selected: k === code }, k ? `${i + 1}. ${t('etape_' + k)}` : `${i + 1}. —`)))),
      st.etapes.some((k) => k.startsWith('sg')) ? [
        el('label', { class: 'champ' }, t('fenetre'), el('input', { type: 'number', min: 3, step: 2, value: st.fenetre, style: 'width:4.5em',
          oninput: (e) => { st.fenetre = +e.target.value; } })),
        el('label', { class: 'champ' }, t('degre'), el('input', { type: 'number', min: 1, max: 5, value: st.degre, style: 'width:4em',
          oninput: (e) => { st.degre = +e.target.value; } }))] : null),
    st.etapes.some(Boolean) && !regression() ? [el('label', {}, ''), el('p', { class: 'note' }, t('spectres_note'))] : null);

  const avecY = () => ['PLS', 'OPLS', 'LM', 'LMM', 'GLM', 'GAM'].includes(st.type) || (!!analyse() && analyse().roles.includes('y'));
  const avecClasse = () => ['PLS-DA', 'OPLS-DA', 'CLASSES'].includes(st.type) || (!!analyse() && analyse().roles.includes('classe'));
  const avecZ = () => !!analyse() && analyse().roles.includes('z');
  const roles = (v) => [['', t('role_aucun')], ['x', t('role_x')]].concat(avecY() ? [['y', t('role_y')]] : [])
    .concat(avecClasse() && v.qualitative ? [['classe', t('role_classe')]] : []).concat(avecZ() ? [['z', t('role_z')]] : []);

  const variables = () => {
    const f = st.filtre.toLowerCase();
    const vars = p.variables.filter((v) => !f || v.nom.toLowerCase().includes(f));
    const tous = (cle, valeur) => {
      for (const v of vars) {
        if (cle === 'role') {
          if (valeur === 'classe' && !v.qualitative) continue;
          if (valeur === 'y' && !avecY()) continue;
          st.roles[v.uid] = valeur;
        } else if (cle === 'echelle') st.echelles[v.uid] = valeur;
        else if (cle === 'bloc') st.blocs[v.uid] = valeur;
        else if (!v.qualitative) st.transfos[v.uid] = valeur;
      }
      rendre();
    };
    const sel = (opts, val, f2, actif = true) => el('select', { onchange: (e) => f2(e.target.value), disabled: !actif },
      opts.map(([k, n]) => el('option', { value: k, selected: String(k) === String(val ?? '') }, n)));
    const filtre = el('input', { type: 'search', placeholder: t('recherche'), value: st.filtre, class: 'filtre',
      oninput: (e) => { st.filtre = e.target.value; rendre(); const i = contenu.querySelector('.filtre'); i.focus(); i.setSelectionRange(i.value.length, i.value.length); } });
    return el('div', { class: 'onglet-vars' },
      el('div', { class: 'outils' }, filtre, el('span', { class: 'infos' }, t('pour_filtrees')),
        sel([['-', t('role') + '…']].concat(roles({ qualitative: true }).map(([k, n]) => [k || 'aucun', n])), '-', (v) => tous('role', v === 'aucun' ? '' : v)),
        sel([['-', t('echelle') + '…']].concat(ECHELLES.map((k) => [k, t('echelle_' + k)])), '-', (v) => tous('echelle', v)),
        el('input', { placeholder: t('bloc') + '…', title: `${t('bloc_aide')} ${t('bloc_entree')}`, size: 9, value: st.bloc,
          oninput: (e) => { st.bloc = e.target.value; },
          onkeydown: (e) => { if (e.key === 'Enter') { e.preventDefault(); e.stopPropagation(); tous('bloc', st.bloc.trim()); } } }),
        sel([['-', t('transformation') + '…']].concat(TRANSFOS.map((k) => [k || 'aucune', t('tr_' + (k || 'aucune'))]), [[PERSO, t('tr_perso')]]), '-',
          (v) => (v === PERSO ? editeurTransfo('', (c) => tous('transfo', c), rendre) : tous('transfo', v === 'aucune' ? '' : v))),
        el('button', { title: t('transfos_suggerees_aide'), onclick: () => suggerer(vars) }, icone('wand-sparkles'), t('transfos_suggerees')),
        st.suggerees !== undefined ? el('span', { class: 'infos' }, t('transfos_appliquees', { n: st.suggerees })) : null),
      el('div', { class: 'defile' }, el('table', { class: 'tableau' },
        el('thead', {}, el('tr', {}, el('th', {}, t('variables')), el('th', {}, ''), el('th', {}, t('role')), el('th', {}, t('echelle')),
          el('th', { title: regression() ? t('bloc_aide_regression') : t('bloc_aide') }, t('bloc')),
          regression() ? null : el('th', { title: t('modificateur_aide') }, t('modificateur')), el('th', {}, t('transformation')),
          st.type === 'GAM' ? el('th', { title: t('courbe_aide') }, t('courbe')) : null)),
        el('tbody', {}, vars.map((v) => el('tr', { class: st.roles[v.uid] ? 'role-' + st.roles[v.uid] : '' },
          el('td', {}, v.nom),
          el('td', { class: 'doux' }, v.qualitative ? `${v.categories.length} ${t('categories')}` : ''),
          el('td', {}, sel(roles(v), st.roles[v.uid] || '', (x) => { st.roles[v.uid] = x; rendre(); })),
          el('td', {}, sel(ECHELLES.map((k) => [k, t('echelle_' + k)]), st.echelles[v.uid] || st.echelle, (x) => { st.echelles[v.uid] = x; })),
          // block scaling and modifier (moteur/R/pretraitement.R: facteurs_ponderation)
          el('td', {}, el('input', { value: st.blocs[v.uid] || '', placeholder: '—', size: 7, title: t('bloc_aide'),
            oninput: (e) => { st.blocs[v.uid] = e.target.value.trim(); } })),
          // (shown to 5 significant digits; the exact value stays unless it is edited)
          regression() ? null : el('td', {}, el('input', { type: 'number', step: 'any', min: 0, value: st.modificateurs[v.uid] !== undefined ? +Number(st.modificateurs[v.uid]).toPrecision(5) : '',
            placeholder: '1', style: 'width:7em',
            title: t('modificateur_aide'),
            oninput: (e) => { const x = parseFloat(e.target.value); if (Number.isFinite(x) && x !== 1) st.modificateurs[v.uid] = x; else delete st.modificateurs[v.uid]; } })),
          el('td', {}, sel(optionsTransfo(st.transfos[v.uid]), st.transfos[v.uid] || '',
            (x) => (x === PERSO ? editeurTransfo(st.transfos[v.uid], (c) => { st.transfos[v.uid] = c; }, rendre) : (st.transfos[v.uid] = x)),
            !v.qualitative)),
          st.type === 'GAM' ? el('td', {}, v.qualitative ? '' : courbe(v)) : null))))));
  };

  // The proposed transformation, worked out in R: log10(x + C2) for the variables
  // shown whose skewness and Min/Max call for it.
  async function suggerer(vars) {
    const s = await app.statistiquesVariables(p.id);
    let n = 0;
    for (const v of vars) {
      const i = s.uid.indexOf(v.uid);
      if (i >= 0 && s.suggestion[i] && !v.qualitative) { st.transfos[v.uid] = s.suggestion[i]; n += 1; }
    }
    st.suggerees = n;
    rendre();
  }

  const observations = () => {
    const f = st.filtreObs.toLowerCase();
    const o = p.observations;
    const idx = o.uid.map((_, i) => i).filter((i) => !f || String(o.nom[i]).toLowerCase().includes(f));
    const modele = app.modeleActif() && app.modele(app.modeleActif());
    const filtre = el('input', { type: 'search', placeholder: t('recherche'), value: st.filtreObs, class: 'filtre',
      oninput: (e) => { st.filtreObs = e.target.value; rendre(); const i = contenu.querySelector('.filtre'); i.focus(); i.setSelectionRange(i.value.length, i.value.length); } });
    return el('div', { class: 'onglet-obs' },
      el('div', { class: 'outils' }, filtre,
        el('button', { onclick: () => { idx.forEach((i) => st.exclues.delete(o.uid[i])); rendre(); } }, t('tout_inclure')),
        el('button', { onclick: () => { idx.forEach((i) => { const u = o.uid[i]; if (st.exclues.has(u)) st.exclues.delete(u); else st.exclues.add(u); }); rendre(); } }, t('inverser')),
        modele && modele.projet === p.id ? el('button', { disabled: !app.selection(modele.id).length,
          onclick: () => { app.selection(modele.id).forEach((u) => st.exclues.add(u)); rendre(); } }, icone('eye-off'), t('exclure_selection')) : null,
        el('span', { class: 'infos' }, `${o.uid.length - st.exclues.size} / ${o.uid.length} ${t('incluses')}`)),
      // test set: observations left out of the model, then predicted by it (moteur/R/jeu_test.R)
      el('div', { class: 'outils' }, el('span', { title: t('jeu_test_aide') }, t('jeu_test')),
        el('select', { onchange: (e) => { st.methodeTest = e.target.value; } },
          [['hasard', t('test_hasard')], ['kennard_stone', t('test_kennard_stone')]].map(([k, n]) => el('option', { value: k, selected: k === st.methodeTest }, n))),
        el('input', { type: 'number', min: 5, max: 50, value: st.partTest, style: 'width:4.5em', oninput: (e) => { st.partTest = +e.target.value || 25; } }), '%',
        el('button', { onclick: async () => {
          const cles = (r2) => Object.keys(st.roles).filter((k) => st.roles[k] === r2).map(Number);
          if (!cles('x').length) { erreur.textContent = t('x_requis'); return; }
          try {
            const res = await app.jeuTest({ projet: p.id, x: cles('x'), exclues: [...st.exclues], methode: st.methodeTest, part: st.partTest / 100,
              classe: avecClasse() ? cles('classe')[0] ?? null : null });
            st.test = new Set([].concat(res.test || []).map(Number));
            rendre();
          } catch (e) { erreur.textContent = e.message; }
        } }, t('choisir_test')),
        modele && modele.projet === p.id ? el('button', { disabled: !app.selection(modele.id).length,
          onclick: () => { app.selection(modele.id).forEach((u) => st.test.add(u)); rendre(); } }, t('selection_en_test')) : null,
        st.test.size ? el('button', { onclick: () => { st.test.clear(); rendre(); } }, t('vider_test')) : null),
      el('div', { class: 'defile' }, el('table', { class: 'tableau' },
        el('thead', {}, el('tr', {}, el('th', {}, t('inclure')), el('th', { title: t('jeu_test_aide') }, t('test')), el('th', {}, t('observations')))),
        el('tbody', {}, idx.slice(0, 3000).map((i) => el('tr', { class: st.exclues.has(o.uid[i]) ? 'exclue' : st.test.has(o.uid[i]) ? 'en-test' : '' },
          el('td', {}, el('input', { type: 'checkbox', checked: !st.exclues.has(o.uid[i]),
            onchange: (e) => { if (e.target.checked) st.exclues.delete(o.uid[i]); else st.exclues.add(o.uid[i]); rendre(); } })),
          el('td', {}, el('input', { type: 'checkbox', checked: st.test.has(o.uid[i]), disabled: st.exclues.has(o.uid[i]),
            onchange: (e) => { if (e.target.checked) st.test.add(o.uid[i]); else st.test.delete(o.uid[i]); majResume(); e.target.closest('tr').classList.toggle('en-test', e.target.checked); } })),
          el('td', {}, o.nom[i])))))));
  };

  // Cross terms (A × B, A × B × C), squares and cubes: products of X variables (every factor
  // must stay in X; a qualitative variable once at most, by each of its categories).
  const termes = () => {
    const nom = (u) => (p.variables.find((v) => v.uid === u) || { nom: `#${u}` }).nom;
    const qual = (u) => !!(p.variables.find((v) => v.uid === u) || {}).qualitative;
    const enX = p.variables.filter((v) => st.roles[v.uid] === 'x');
    const quantX = enX.filter((v) => !v.qualitative).map((v) => v.uid);
    const cle = (fs) => [...fs].sort((x, y) => x - y).join(',');
    const existe = (fs) => st.croises.some((c) => cle(c) === cle(fs));
    const qualDouble = (fs) => fs.some((u, i) => qual(u) && fs.indexOf(u) !== i);
    const ajouter = (...fs) => { if (!existe(fs) && !qualDouble(fs)) st.croises.push(fs); };
    const options = [['', '—']].concat(enX.map((v) => [v.uid, v.nom]));
    const sel = (c) => el('select', { onchange: (e) => { st[c] = e.target.value; } },
      options.map(([k, n]) => el('option', { value: k, selected: String(k) === String(st[c] ?? '') }, n)));
    return el('div', { class: 'onglet-termes' },
      el('div', { class: 'outils' }, sel('a'), '×', sel('b'), el('span', { title: t('facteur_facultatif') }, '×'), sel('c'),
        el('button', { onclick: () => {
          if (!st.a || !st.b) return;
          const fs = [st.a, st.b].concat(st.c ? [st.c] : []).map(Number);
          if (qualDouble(fs)) { erreur.textContent = t('carre_qualitatif'); return; }
          ajouter(...fs);
          rendre();
        } }, icone('plus'), t('ajouter')),
        el('span', { class: 'espace' }),
        el('button', { disabled: !quantX.length, onclick: () => { quantX.forEach((u) => ajouter(u, u)); rendre(); } }, t('tous_carres')),
        el('button', { disabled: quantX.length < 2, title: `${quantX.length * (quantX.length - 1) / 2}`,
          onclick: () => { quantX.forEach((u, i) => quantX.slice(i + 1).forEach((w) => ajouter(u, w))); rendre(); } }, t('toutes_interactions')),
        el('button', { disabled: !st.croises.length, onclick: () => { st.croises = []; st.croisesReglages = {}; rendre(); } }, t('tout_retirer'))),
      el('p', { class: 'note' }, t('termes_note')),
      el('div', { class: 'defile' }, st.croises.length ? el('table', { class: 'tableau' },
        el('thead', {}, el('tr', {}, el('th', {}, t('termes')), el('th', {}, ''), el('th', {}, t('echelle')),
          el('th', { title: regression() ? t('bloc_aide_regression') : t('bloc_aide') }, t('bloc')),
          regression() ? null : el('th', { title: t('modificateur_aide') }, t('modificateur')), el('th', {}, ''))),
        el('tbody', {}, st.croises.map((fs, i) => {
          const horsX = [...new Set(fs)].filter((u) => st.roles[u] !== 'x');
          const k = fs.join('*');
          const reglage = (cle, valeur) => {
            const r2 = { ...(st.croisesReglages[k] || {}) };
            if (valeur === null || valeur === '' || valeur === undefined) delete r2[cle]; else r2[cle] = valeur;
            if (Object.keys(r2).length) st.croisesReglages[k] = r2; else delete st.croisesReglages[k];
          };
          const r2 = st.croisesReglages[k] || {};
          return el('tr', {}, el('td', {}, fs.map(nom).join(' × ')),
            el('td', { class: 'erreur' }, horsX.length ? `${t('facteur_non_x')} : ${horsX.map(nom).join(', ')}` : ''),
            el('td', {}, el('select', { onchange: (e) => reglage('echelle', e.target.value) },
              ECHELLES.map((e2) => el('option', { value: e2, selected: e2 === (r2.echelle || st.echelle) }, t('echelle_' + e2))))),
            el('td', {}, el('input', { value: r2.bloc || '', placeholder: '—', size: 7, title: t('bloc_aide'),
              oninput: (e) => reglage('bloc', e.target.value.trim()) })),
            regression() ? null : el('td', {}, el('input', { type: 'number', step: 'any', min: 0, placeholder: '1', title: t('modificateur_aide'),
              value: r2.modificateur !== undefined ? +Number(r2.modificateur).toPrecision(5) : '', style: 'width:5em',
              oninput: (e) => { const x2 = parseFloat(e.target.value); reglage('modificateur', Number.isFinite(x2) && x2 !== 1 ? x2 : null); } })),
            el('td', { class: 'num' }, el('button', { class: 'icone-seule', onclick: () => { st.croises.splice(i, 1); delete st.croisesReglages[k]; rendre(); } }, icone('x-circle'))));
        }))) : el('p', { class: 'infos' }, t('aucun_terme'))));
  };

  function rendre() {
    contenu.replaceChildren(onglets(), el('div', { class: 'onglet' },
      st.onglet === 'general' ? general() : st.onglet === 'variables' ? variables() : st.onglet === 'termes' ? termes() : observations()));
    majResume();
    erreur.textContent = '';
  }

  // a curve's box, and its basis dimension k (empty: Nuée sets it)
  function courbe(v) {
    const k = el('input', { type: 'number', min: 3, max: 100, step: 1, placeholder: 'k', title: t('k_courbe_aide'), style: 'width:4.2em',
      disabled: !st.lisses.has(v.uid), value: st.kCourbes[v.uid] ?? '',
      oninput: (e) => { const x = parseInt(e.target.value, 10); if (x >= 3 && x <= 100) st.kCourbes[v.uid] = x; else delete st.kCourbes[v.uid]; } });
    return el('span', { class: 'ligne' }, el('input', { type: 'checkbox', checked: st.lisses.has(v.uid), title: t('courbe_aide'),
      onchange: (e) => { if (e.target.checked) st.lisses.add(v.uid); else st.lisses.delete(v.uid); k.disabled = !e.target.checked; } }), k);
  }
  // the k of the curves the model has
  function kCourbesEnvoyes() {
    if (st.type !== 'GAM') return {};
    return Object.fromEntries(Object.entries(st.kCourbes).filter(([u]) => st.lisses.has(Number(u)) && st.roles[u] === 'x'));
  }

  function valider() {
    if (regression() && st.formuleModifiee) {  // an edited formula is applied first
      appliquerFormule().then((ok) => { if (ok) valider(); });
      return false;
    }
    const cles = (r2) => Object.keys(st.roles).filter((k) => st.roles[k] === r2).map(Number);
    const x = cles('x');
    const y = cles('y');
    const classe = cles('classe')[0];
    if (!x.length && !regression()) { erreur.textContent = t('x_requis'); return false; }  // (a regression without X: the null model)
    if (regression() && y.length !== 1) { erreur.textContent = t('regression_y_requis'); return false; }
    if (st.type === 'LMM' && !st.aleatoires.length) { erreur.textContent = t('lmm_aleatoire_requis'); return false; }
    if (avecCar1() && st.correlation && !st.correlation.temps) { erreur.textContent = t('car1_temps_requis'); return false; }
    if (!analyse() && avecY() && !y.length) { erreur.textContent = t('pls_y_requis'); return false; }
    if (!analyse() && avecClasse() && classe === undefined) { erreur.textContent = t('plsda_classe_requise'); return false; }
    const manque = analyse() && analyse().valider ? analyse().valider(st, cles) : '';  // (another analysis's own checks)
    if (manque) { erreur.textContent = manque; return false; }
    const horsX = [...new Set(st.croises.flat().filter((u) => st.roles[u] !== 'x'))];
    if (horsX.length) {
      erreur.textContent = t('croises_x_requis', { noms: horsX.map((u) => (p.variables.find((v) => v.uid === u) || {}).nom).join(', ') });
      return false;
    }
    const echelles = Object.fromEntries(Object.entries(st.echelles).filter(([, v]) => v && v !== st.echelle));
    const transformations = Object.fromEntries(Object.entries(st.transfos).filter(([, v]) => v && v !== 'autre'));
    if (st.etapes.some((k) => k.startsWith('sg')) && (st.fenetre < 3 || st.fenetre % 2 === 0 || st.degre >= st.fenetre
      || st.etapes.some((k) => k.startsWith('sg') && +k[2] > st.degre))) {
      erreur.textContent = t('sg_invalide');
      return false;
    }
    const spectres = st.etapes.filter(Boolean).map((k) => (k.startsWith('sg') ? { type: 'sg', derivee: +k[2], fenetre: st.fenetre, degre: st.degre } : { type: k }));
    const blocs = Object.fromEntries(Object.entries(st.blocs).filter(([, b]) => b));
    const modificateurs = Object.fromEntries(Object.entries(st.modificateurs).filter(([, m]) => Number.isFinite(m) && m !== 1));
    // the cross terms' own settings: a scaling other than the default, a block, a modifier
    const croisesReglages = Object.fromEntries(st.croises.map((fs) => {
      const r2 = st.croisesReglages[fs.join('*')] || {};
      const garde = { ...(r2.echelle && r2.echelle !== st.echelle ? { echelle: r2.echelle } : {}), ...(r2.bloc ? { bloc: r2.bloc } : {}),
        ...(!regression() && Number.isFinite(r2.modificateur) && r2.modificateur !== 1 ? { modificateur: r2.modificateur } : {}) };
      return [fs.join('*'), garde];
    }).filter(([, r2]) => Object.keys(r2).length));
    const q = { projet: p.id, type: st.type, x, y: avecY() ? y : [], classe: avecClasse() ? classe : null,
      exclues: [...st.exclues], test: [...st.test].filter((u) => !st.exclues.has(u)), echelle: st.echelle, echelles, transformations, croises: st.croises,
      spectres: sansSpectres() ? [] : spectres, A: st.auto || regression() || analyse() ? 0 : st.A, G: st.G, vc: st.vc, blocs, modificateurs: regression() ? {} : modificateurs,
      aleatoires: ['LMM', 'GLM', 'GAM'].includes(st.type) ? st.aleatoires : [], correlation: avecCar1() ? st.correlation : null,
      famille: generalise() ? st.famille : null, lisses: st.type === 'GAM' ? [...st.lisses].filter((u) => st.roles[u] === 'x') : [], k_courbes: kCourbesEnvoyes(),
      decalage: generalise() && FAMILLES_LOG.includes(st.famille) ? st.decalage : null, croises_reglages: croisesReglages, nom: st.nom, titre: st.titre,
      options: analyse() && analyse().options ? analyse().options(st, cles) : undefined };
    if (!enCours) verifierPuisAjuster(q);
    return false;  // the dialog closes once the model is fitted
  }

  // The fit runs with the dialog open: it closes once the model is there; an error ("Component 3
  // isn't possible…") shows in it, with every setting kept. A second click on Fit does nothing.
  let enCours = false;
  async function ajusterPuisFermer(q) {
    enCours = true;
    erreur.textContent = '';
    alerte.replaceChildren(el('p', { class: 'infos' }, t('calcul')));
    try {
      await app.ajusterDefinition(q);
      d.fermer();
    } catch (e) {
      alerte.replaceChildren();
      erreur.textContent = e.message;
    } finally {
      enCours = false;
    }
  }

  // Checks before fitting: variables or observations with more than 50 % missing
  // values, constant variables. Listed with the choice to exclude them or to fit anyway.
  async function verifierPuisAjuster(q) {
    erreur.textContent = '';
    alerte.replaceChildren(el('p', { class: 'infos' }, t('calcul')));
    let v;
    enCours = true;
    try {
      v = await app.verifier(q);
    } catch (e) {
      alerte.replaceChildren();
      erreur.textContent = e.message;
      return;
    } finally {
      enCours = false;
    }
    const vars = v.variables && v.variables.uid ? v.variables.uid.map((u, i) => ({ uid: u, nom: v.variables.nom[i], manque: v.variables.manquantes[i], constante: v.variables.constante[i] })) : [];
    const obs = v.observations && v.observations.uid ? v.observations.uid.map((u, i) => ({ uid: u, nom: v.observations.nom[i], manque: v.observations.manquantes[i] })) : [];
    if (!vars.length && !obs.length) {
      await ajusterPuisFermer(q);
      return;
    }
    const pct = (x) => t('manquantes_pct', { p: Math.round(100 * x) });
    alerte.replaceChildren(el('div', { class: 'alerte-donnees' },
      el('b', {}, t('verif_titre')), el('p', { class: 'note' }, t('verif_note')),
      vars.length ? el('p', {}, t('verif_variables'), ' ', vars.map((x) => `${x.nom} (${x.constante ? t('constante') : pct(x.manque)})`).join(', ')) : null,
      obs.length ? el('p', {}, t('verif_observations'), ' ', obs.slice(0, 30).map((x) => `${x.nom} (${pct(x.manque)})`).join(', '), obs.length > 30 ? '…' : '') : null,
      el('div', { class: 'barre-alerte' },
        el('button', { class: 'principal', onclick: () => {
          vars.forEach((x) => { st.roles[x.uid] = ''; });
          st.croises = st.croises.filter((c) => !c.some((u) => vars.some((x) => x.uid === u)));
          obs.forEach((x) => st.exclues.add(x.uid));
          alerte.replaceChildren();
          rendre();
          valider();
        } }, t('verif_exclure')),
        el('button', { onclick: () => { if (!enCours) ajusterPuisFermer(q); } }, t('verif_garder')))));
  }

  rendre();
  const alerte = el('div');
  const d = dialogue({ titre: `${titre} — ${p.nom}`, contenu: el('div', {}, contenu, alerte, erreur), large: true,
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('ajuster'), principal: true, action: valider }] });
  d.boite.querySelector('.pied-dialogue').prepend(resume);
}

function nomLibre(p) {
  const pris = new Set(p.modeles.map((d) => d.nom));
  let k = p.modeles.length + 1;
  while (pris.has(`M${k}`)) k += 1;
  return `M${k}`;
}

// ------------------------------------------------------------ help

// where the engine sits in the installed application
const MOTEUR = /Win/.test(navigator.platform) ? 'resources\\moteur\\R' : 'resources/moteur/R';
const AIDE = {
  fr: {
    guide: [
      ['Ouvrir', 'Fichier › Ouvrir (Ctrl+O) : un tableau, une ligne par observation (Excel .xlsx ou .xls, texte .csv ou .txt, R .rds ou .RData, SPSS .sav, Stata .dta, SAS .sas7bdat ou .xpt, jamovi .omv, dBase .dbf, Weka .arff), un projet enregistré par l’application (.nuee, ou .json avant la version 0.9.1) ou un projet .usp. Pour un classeur à plusieurs feuilles ou un .RData à plusieurs tableaux, l’application demande lequel ouvrir.'],
      ['Coller un tableau', 'Fichier › Coller un tableau (Ctrl+V) : des cellules copiées dans Excel, LibreOffice, Google Sheets ou jamovi deviennent un projet, à enregistrer. La première ligne donne les noms des variables, la première colonne ceux des observations si c’est du texte.'],
      ['Projet d’exemple', 'Aide › Projet d’exemple : 120 pommes inventées et quatre modèles prêts (ACP, PLS, PLS-DA, OPLS-DA), pour essayer l’application.'],
      ['Explorateur', 'À gauche : projets, données, modèles importés d’un fichier .usp, modèles calculés et leurs vues. Double-clic pour ouvrir, clic droit pour les actions.'],
      ['Modèles importés', 'Double-cliquer un modèle importé le recalcule en R. Le résumé marque chaque valeur : = identique à la valeur enregistrée dans le fichier, ≈ proche, ≠ différente.'],
      ['Nouveau modèle', 'Modèle › Nouveau modèle (Ctrl+N) : type (ACP, PLS, PLS-DA, OPLS), rôle, échelle, bloc, modificateur et transformation de chaque variable (racine, log10, ou personnalisée : log10, ln, −log10, exp, logit, puissance ou linéaire, avec ses constantes), termes croisés (A × B, A × B × C, carrés et cubes), observations incluses, nombre de composantes (automatique par défaut), groupes de validation croisée (à tour de rôle, ou un groupe par valeur d’un identifiant ou d’une variable : toutes les années d’un site ensemble).'],
      ['Sélection', 'Cadre ou lasso dans un graphique, clic sur un point ou une ligne de liste : la sélection apparaît dans toutes les vues et toutes les fenêtres du modèle. Échap l’efface ; Ctrl+Suppr exclut les observations et réajuste.'],
      ['Variables en évidence', 'Un clic sur une variable (barre des VIP, des coefficients ou des loadings, point des loadings, du biplot ou du S-plot, ligne d’un profil ou de la liste des variables) la met en évidence sur tous les graphiques du modèle ; un cadre en choisit plusieurs dans les nuages de points. Échap les efface.'],
      ['Fenêtres', 'Un onglet par modèle : sa page montre résumé, scores et loadings, ses autres vues s’y ajoutent en onglets. Les vues se déplacent par leur onglet (glisser-déposer), se divisent et s’empilent ; glisser l’onglet d’un modèle à côté d’un autre pour les comparer. Clic droit sur un onglet : détacher dans une fenêtre, agrandir, fermer. Fenêtre › Nouvelle fenêtre ouvre une deuxième fenêtre complète.'],
      ['Graphiques', 'Molette : zoom ; clic droit : copier l’image, l’enregistrer en PNG ou SVG, voir les contributions de la sélection, en faire une classe. Listes Couleur et Étiquettes : par classe, groupe du dendrogramme, identifiant ou n’importe quelle variable.'],
      ['Tableaux et commandes', 'Ctrl+C copie le tableau de la vue, à coller dans Excel (avec du texte sélectionné : le texte). Ctrl+K : rechercher une commande par son nom.'],
      ['Prédire un fichier', 'Prédiction › Prédire un fichier : un tableau (CSV, Excel, R, SPSS…) dont les colonnes portent les noms des variables du modèle. Les observations hors du domaine du modèle sont signalées ; le résultat s’exporte dans tous les formats d’export.'],
      ['Modèles hiérarchiques', 'Modèle › Scores en variables : les scores d’un modèle deviennent des variables ($M1.t1…) utilisables comme X d’un autre modèle. Après avoir modifié le modèle de base, refaire cette étape puis réajuster le modèle supérieur.'],
      ['Exporter', 'Fichier › Exporter les données : le tableau du projet ; Fichier › Exporter les résultats du modèle (Ctrl+E) : scores, T2, DModX, loadings, VIP, coefficients… Formats : Excel, CSV, texte, R (.rds, .RData), SPSS, Stata, SAS, jamovi, dBase, Weka. Les qualitatives restent qualitatives ; pour SPSS, Stata et SAS, les noms de variables sont rendus valides et le nom d’origine devient l’étiquette.'],
      ['Exporter vers R', 'Fichier › Exporter vers R : un script, ou un rapport R Markdown ou Quarto, qui refait le modèle sans Nuée, avec R seul (sans paquet). Le calcul y est écrit pour ce modèle seul et commenté : mise à l’échelle, NIPALS, validation croisée, statistiques, graphiques ; les données sont dans le script ou dans un CSV à côté. Le rapport ajoute le texte des méthodes, des tableaux, des graphiques et les références ; il se compile dans RStudio (Knit) ou avec Quarto. En plus, au choix : renv.lock, données et modèle en .RData, projet RStudio. Toute la séance peut aussi aller dans un seul script .R.'],
      ['Texte des méthodes', 'Modèle › Texte pour la section Méthodes : un paragraphe écrit d’après le modèle (données, prétraitement, méthode, validation croisée, R², Q², logiciel), en français ou en anglais, avec ses références. À relire et compléter avant de le publier.'],
      ['Rapport', 'Fichier › Rapport du modèle : un PDF ou un document Word avec les parties choisies ; les graphiques y sont mis en forme comme dans leurs panneaux.'],
      ['Console R', 'Affichage › Console R (Ctrl+J) : le code R de chaque action, et une invite R dans la session de l’application (m = modèle affiché, p = son projet). recharger_moteur() relit les fichiers du moteur après une correction.'],
      ['Séance', 'Ctrl+S enregistre aussi la séance : modèles calculés, panneaux, réglages des graphiques et sélections reviennent à la réouverture du projet.'],
      ['Sécurité', 'En fermant : « Enregistrer et fermer ». Copie de secours toutes les deux minutes, proposée après un arrêt imprévu. Ctrl+Z annule une modification des données. Aide › Redémarrer le moteur R si un calcul ne finit pas.'],
      ['Statistiques des variables', 'Graphiques › Statistiques des variables : statistiques du jeu de travail, transformations proposées en rouge, histogrammes et carte des valeurs manquantes.'],
      ['Modèles linéaires, généralisés et mixtes', 'Nouveau modèle › type « Régression linéaire » ou « Modèle linéaire mixte » : une variable Y quantitative, les X de l’onglet Variables ; sans X, le modèle nul. Un modèle mixte (nlme) ajoute des effets aléatoires emboîtés (le site, puis le secteur dans le site : un identifiant ou une variable qualitative) et, au choix, une autocorrélation dans le temps (CAR1). Coefficients en REML, AIC en ML. Type « Modèle linéaire généralisé (GLM) » ou « Modèle additif (GAM) » (mgcv) : la loi de Y (Poisson ou binomiale négative pour des comptages, binomiale pour la présence-absence, gamma pour des valeurs positives, Tweedie pour des densités avec des zéros), au choix des effets aléatoires et un effort en décalage ; dans un GAM, les X cochés dans la colonne Courbe entrent par une courbe (un effet qui monte puis descend, un seuil). Vues : Résumé (R2, R2 marginal et conditionnel ou déviance expliquée ; Q2 ; AIC ; variances), Coefficients, Effets, Observé / prédit, Résidus, Apport des blocs (AIC : chaque bloc retiré, le modèle réajusté). Onglet Général › Formule R : le modèle écrit comme dans R, y ~ a * b + log10(c) + s(d) + offset(log(effort)) + (1 | site/secteur) ; Appliquer (Ctrl+Entrée) règle les onglets, et R la lit sans jamais l’exécuter. Prédiction › Prédire : Y prédit par les effets fixes, ⚠ une extrapolation. Fichier › Exporter, Exporter vers R (lm, nlme, mgcv) et Rapport du modèle, comme pour les autres modèles. Prédiction › Comparer les modèles : AIC, ΔAIC et poids d’Akaike entre les régressions de la même réponse sur les mêmes observations, quand leurs vraisemblances se comparent.'],
      ['Tests statistiques', 'Graphiques › Tests statistiques, en trois modes. Entre classes, variable par variable : chaque variable quantitative comparée entre deux classes (t de Welch ou de Student, Mann–Whitney ; graphique volcan) ou entre toutes (ANOVA, Kruskal–Wallis), valeurs p corrigées (Benjamini–Hochberg ou Bonferroni). Deux variables : le test qui convient à leurs types, ses conditions vérifiées (khi² ou Fisher ; t de Welch, ANOVA ou Kruskal–Wallis avec les comparaisons deux à deux ; Pearson ou Spearman), et son résultat en une phrase ; un identifiant (site, année…) peut faire les groupes. Matrice des corrélations : toutes les paires de variables quantitatives, valeurs p corrigées ; un clic sur une paire la teste.'],
      ['Modèles de classe', 'Nouveau modèle › type Modèles de classe : une ACP par catégorie de la variable de classe. Graphiques › Graphique de Coomans : la distance (DModX) de chaque observation à deux de ces modèles ; sous la limite d’un modèle, l’observation appartient à sa classe (à une, plusieurs ou aucune).'],
      ['Données', 'Double-clic sur une case de la vue Données pour la modifier ; Édition › Nouvelle variable : formule R ou classes d’après les valeurs.'],
      ['OPLS', 'Des composantes prédictives (une par variable Y, ou une de moins que de classes pour l’OPLS-DA) et des composantes orthogonales. Graphique S (S-plot) dans Graphiques.'],
      ['Spectres', 'Fichier › Importer des spectres : des fichiers JCAMP-DX, SPC ou texte, un spectre par ligne du projet, avec un tableau d’informations (teneurs, classes) joint par le nom des fichiers. Nouveau modèle › Général › Prétraitement des spectres : SNV, MSC, detrend, ligne de base, lissage ou dérivées de Savitzky–Golay, en une à trois étapes, appliqués à chaque observation avant la mise à l’échelle (variables X dans l’ordre du tableau). Graphiques › Profils (spectres) : loadings, coefficients, VIP ou les spectres eux-mêmes en lignes, la longueur d’onde lue dans le nom des variables ; « Choisir des zones » : faire glisser pour marquer des zones, puis un nouveau modèle sans elles ou avec elles seules. Graphiques › iPLS : chaque intervalle du spectre ajusté seul ; en vert, ceux qui prédisent mieux que le spectre entier.'],
      ['Mises à jour', 'La version installée cherche au démarrage une nouvelle version sur GitHub et la signale dans la barre d’état ; sous Windows, un clic la télécharge, la vérifie, l’installe et relance Nuée (Aide › Chercher une nouvelle version au démarrage pour l’arrêter). Aide › Chercher une mise à jour : la même recherche, à tout moment.'],
      ['Historique du fichier', 'Sous un projet .usp : le journal d’audit enregistré dans le fichier (dates, versions, changements de chaque modèle).'],
      ['Signaler un problème', 'Aide › Informations de diagnostic : les versions à coller dans une issue sur github.com/4rtefakt/nuee, en français ou en anglais.'],
      ['Code', `Tous les calculs sont en R, commentés en français, dans ${MOTEUR}.`],
    ],
    differences: [
      ['Identique aux valeurs enregistrées', 'Vérifié sur 944 modèles de 133 projets .usp : mise à l’échelle (pondération par blocs et modificateurs compris), groupes de validation croisée, R2X, R2Y, scores, loadings, valeurs propres, T2 et sa limite, DModX (normal et pondéré) et DCrit, VIP, coefficients, RMSEE.'],
      ['Validation croisée', 'Chaque groupe est prédit par un modèle ajusté sans lui, centrage et mise à l’échelle recalculés sur les autres observations. Q2, RMSEcv, CV-ANOVA et classement en validation croisée s’écartent donc des valeurs enregistrées, d’autant plus que les observations sont peu nombreuses (PLS, écart médian du Q2 : 0,003 au-delà de 200 observations, 0,02 de 50 à 200, 0,1 à 0,3 en dessous) ; le Q2 de Nuée est le plus souvent plus bas. Avec des groupes par identifiant (un site, un secteur), l’écart est plus grand : le modèle ajusté sans un site ne connaît pas non plus son niveau moyen.'],
      ['Signe des composantes', 'Arbitraire pour les nouveaux modèles ; les modèles importés sont orientés comme la solution enregistrée.'],
      ['Dendrogrammes', 'Méthode de Ward exacte : les fusions du haut de l’arbre peuvent différer de celles enregistrées.'],
      ['Intervalles de confiance', 'Jack-knife sur les sous-modèles de la validation croisée, chacun dans sa propre mise à l’échelle : ils diffèrent des valeurs enregistrées.'],
      ['Ajustement automatique', 'Règles publiées R1, R2, N3, N4, sur le Q2 de cette validation croisée : le nombre de composantes peut différer ; pour l’ACP, la limite exacte n’est pas publiée.'],
      ['Seuil VIP', 'Plus grande marche des VIP triés autour de 0,8 : méthode de l’application.'],
    ],
  },
  en: {
    guide: [
      ['Open', 'File › Open (Ctrl+O): a table, one row per observation (Excel .xlsx or .xls, text .csv or .txt, R .rds or .RData, SPSS .sav, Stata .dta, SAS .sas7bdat or .xpt, jamovi .omv, dBase .dbf, Weka .arff), a project saved by this application (.nuee, or .json before version 0.9.1) or a .usp project. For a workbook with several sheets or an .RData file with several tables, the application asks which one to open.'],
      ['Paste a table', 'File › Paste a table (Ctrl+V): cells copied from Excel, LibreOffice, Google Sheets or jamovi become a project, to save. The first row gives the variable names, the first column the observation names when it is text.'],
      ['Example project', 'Help › Example project: 120 made-up apples and four ready models (PCA, PLS, PLS-DA, OPLS-DA), to try the application.'],
      ['Explorer', 'On the left: projects, data, models imported from a .usp file, fitted models and their views. Double-click to open, right-click for actions.'],
      ['Imported models', 'Double-clicking an imported model recomputes it in R. The summary marks each value: = same as the value stored in the file, ≈ close, ≠ different.'],
      ['New model', 'Model › New model (Ctrl+N): type (PCA, PLS, PLS-DA, OPLS), each variable’s role, scaling, block, modifier and transform (root, log10, or custom: log10, ln, −log10, exp, logit, power or linear, with its constants), cross terms (A × B, A × B × C, squares and cubes), included observations, number of components (automatic by default), cross-validation groups (in turn, or one group per value of an ID or of a variable: all the years of a site together).'],
      ['Selection', 'Box or lasso in a plot, click a point or a list row: the selection shows in every view and window of the model. Esc clears it; Ctrl+Delete excludes the observations and refits.'],
      ['Highlighted variables', 'Clicking a variable (a bar of the VIP, coefficients or loadings, a point of the loadings, biplot or S-plot, a line of a profile or of the variable list) highlights it on all the model’s graphs; a box picks several in the scatter plots. Esc clears them.'],
      ['Windows', 'One tab per model: its page shows the summary, scores and loadings, and its other views join it as tabs. Drag views by their tab to move, split and stack them; drag a model’s tab beside another to compare them. Right-click a tab to move it to a new window, maximize or close it. Window › New window opens a second full window.'],
      ['Plots', 'Wheel: zoom; right-click: copy the image, save it as PNG or SVG, see the contributions of the selection, make it a class. Colour and Labels lists: by class, dendrogram group, ID or any variable.'],
      ['Tables and commands', 'Ctrl+C copies the view’s table, to paste into Excel (with text selected: the text). Ctrl+K: find a command by its name.'],
      ['Predict a file', 'Predict › Predict a file: a table (CSV, Excel, R, SPSS…) whose columns carry the model’s variable names. Observations outside the model’s domain are flagged; results export to every export format.'],
      ['Hierarchical models', 'Model › Scores as variables: a model’s scores become variables ($M1.t1…) usable as X of another model. After changing the base model, do it again and refit the upper model.'],
      ['Export', 'File › Export data: the project’s table; File › Export model results (Ctrl+E): scores, T2, DModX, loadings, VIP, coefficients… Formats: Excel, CSV, text, R (.rds, .RData), SPSS, Stata, SAS, jamovi, dBase, Weka. Qualitative variables stay qualitative; for SPSS, Stata and SAS, variable names are made valid and the original name becomes the label.'],
      ['Export to R', 'File › Export to R: a script, or an R Markdown or Quarto report, that refits the model without Nuée, in plain R (no package). The computation is written for this model alone, with comments: scaling, NIPALS, cross-validation, statistics, plots; the data are in the script or in a CSV next to it. The report adds the methods text, tables, plots and references; it compiles in RStudio (Knit) or with Quarto. Optionally: renv.lock, data and model as .RData, an RStudio project. The whole session can also go into one .R script.'],
      ['Methods text', 'Model › Text for the Methods section: a paragraph written from the model (data, preprocessing, method, cross-validation, R², Q², software), in French or English, with its references. Read it and complete it before publishing.'],
      ['Report', 'File › Model report: a PDF or Word document with the chosen parts; plots look as in their panels.'],
      ['R console', 'View › R console (Ctrl+J): the R code of each action, and an R prompt in the application’s session (m = model shown, p = its project). recharger_moteur() reloads the engine files after a correction.'],
      ['Session', 'Ctrl+S also saves the session: fitted models, panels, plot settings and selections come back when the project is reopened.'],
      ['Safety', 'When closing: “Save and close”. A recovery copy every two minutes, offered after an unexpected stop. Ctrl+Z undoes a data change. Help › Restart the R engine if a calculation never ends.'],
      ['Variable statistics', 'Plots › Variable statistics: workset statistics, proposed transformations in red, histograms and a missing-value map.'],
      ['Linear, generalized and mixed models', 'New model › type “Linear regression” or “Linear mixed model”: one quantitative Y, the X of the Variables tab; without X, the null model. A mixed model (nlme) adds nested random effects (the site, then the sector within the site: an ID or a qualitative variable) and, optionally, an autocorrelation in time (CAR1). Coefficients by REML, AIC by ML. Type “Generalized linear model (GLM)” or “Additive model (GAM)” (mgcv): the distribution of Y (Poisson or negative binomial for counts, binomial for presence-absence, gamma for positive values, Tweedie for densities with zeros), optionally random effects and an effort as an offset; in a GAM, the X ticked in the Curve column enter through a curve (an effect that rises then falls, a threshold). Views: Summary (R2, marginal and conditional R2 or deviance explained; Q2; AIC; variances), Coefficients, Effects, Observed / predicted, Residuals, Block contributions (AIC: each block removed, the model refitted). General tab › R formula: the model written as in R, y ~ a * b + log10(c) + s(d) + offset(log(effort)) + (1 | site/sector); Apply (Ctrl+Enter) sets the tabs, and R reads it without ever running it. Prediction › Predict: Y predicted by the fixed effects, ⚠ an extrapolation. File › Export, Export to R (lm, nlme, mgcv) and Model report, as for the other models. Prediction › Compare models: AIC, ΔAIC and Akaike weights between the regressions of the same response on the same observations, when their likelihoods compare.'],
      ['Statistical tests', 'Plots › Statistical tests, in three modes. Between classes, variable by variable: each quantitative variable compared between two classes (Welch’s or Student’s t, Mann–Whitney; volcano plot) or across all of them (ANOVA, Kruskal–Wallis), p-values corrected (Benjamini–Hochberg or Bonferroni). Two variables: the test their types call for, its conditions checked (chi-square or Fisher; Welch’s t, ANOVA or Kruskal–Wallis with pairwise comparisons; Pearson or Spearman), and its result as a sentence; an ID (site, year…) can make the groups. Correlation matrix: every pair of quantitative variables, p-values corrected; a click on a pair tests it.'],
      ['Class models', 'New model › type Class models: one PCA per category of the class variable. Plots › Coomans plot: each observation’s distance (DModX) to two of these models; below a model’s limit, the observation belongs to its class (to one, several or none).'],
      ['Data', 'Double-click a cell of the Data view to edit it; Edit › New variable: an R formula or classes from values.'],
      ['OPLS', 'Predictive components (one per Y variable, or one fewer than the classes for OPLS-DA) and orthogonal ones. S-plot in Plots.'],
      ['Spectra', 'File › Import spectra: JCAMP-DX, SPC or text files, one spectrum per row of the project, with a table of information (contents, classes) joined by file name. New model › General › Spectral preprocessing: SNV, MSC, detrend, baseline, Savitzky–Golay smoothing or derivatives, in one to three steps, applied to each observation before scaling (X variables in the order of the table). Plots › Profiles (spectra): loadings, coefficients, VIP or the spectra themselves as lines, the wavelength read from the variable names; “Choose ranges”: drag to mark ranges, then a new model without them or with them only. Plots › iPLS: each interval of the spectrum fitted on its own; in green, those that predict better than the whole spectrum.'],
      ['Updates', 'The installed version looks for a new version on GitHub at start-up and shows it in the status bar; on Windows, one click downloads it, checks it, installs it and restarts Nuée (Help › Look for a new version at start-up to stop it). Help › Check for updates: the same check, at any time.'],
      ['File history', 'Under a .usp project: the audit trail stored in the file (dates, versions, changes to each model).'],
      ['Report a problem', 'Help › Diagnostic information: the versions to paste into an issue on github.com/4rtefakt/nuee, in English or French.'],
      ['Code', `All computations are in R, commented in French, in ${MOTEUR}.`],
    ],
    differences: [
      ['Same as the stored values', 'Checked on 944 models from 133 .usp projects: scaling (block scaling and modifiers included), cross-validation groups, R2X, R2Y, scores, loadings, eigenvalues, T2 and its limit, DModX (plain and weighted) and DCrit, VIP, coefficients, RMSEE.'],
      ['Cross-validation', 'Each group is predicted by a model fitted without it, with centring and scaling recomputed on the other observations. Q2, RMSEcv, CV-ANOVA and the cross-validated classification therefore differ from the stored values, more so with few observations (PLS, median Q2 difference: 0.003 above 200 observations, 0.02 from 50 to 200, 0.1 to 0.3 below); Nuée’s Q2 is usually lower. With groups by an ID (a site, a sector), the difference is larger: the model fitted without a site doesn’t know its mean level either.'],
      ['Component signs', 'Arbitrary for new models; imported models are oriented like their stored solution.'],
      ['Dendrograms', 'Exact Ward linkage: the top merges can differ from the stored ones.'],
      ['Confidence intervals', 'Jack-knife over the cross-validation sub-models, each in its own scaling: they differ from the stored ones.'],
      ['Autofit', 'Published rules R1, R2, N3, N4, on the Q2 of this cross-validation: the number of components can differ; for PCA, the exact limit is not published.'],
      ['VIP cut-off', 'Largest step in the sorted VIPs around 0.8: the application’s method.'],
    ],
  },
};

export async function dialogueAide(app, sujet) {
  if (sujet === 'apropos') {
    const v = await app.version();
    dialogue({ titre: t('apropos'), contenu: el('div', { class: 'apropos' },
      el('div', { class: 'apropos-tete' }, el('img', { class: 'logo', src: 'nuee.svg', alt: '' }),
        el('div', {}, el('h3', {}, t('titre')), el('p', { class: 'doux' }, t('sous_titre')))),
      el('dl', {}, el('dt', {}, t('version')), el('dd', {}, v.application),
        el('dt', {}, t('moteur')), el('dd', {}, v.R),
        el('dt', {}, t('interface')), el('dd', {}, `Electron ${v.electron || '—'} · dockview · Plotly`),
        el('dt', {}, t('licence')), el('dd', {}, 'GPL-3.0 · github.com/4rtefakt/nuee'))),
    boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
    return;
  }
  const textes = AIDE[langueActuelle()] || AIDE.fr;
  dialogue({ titre: t(sujet), large: true, contenu: el('div', { class: 'aide' },
    textes[sujet].map(([h, p]) => el('section', {}, el('h4', {}, h), el('p', {}, p)))),
  boutons: (sujet === 'guide' ? [{ libelle: t('guide_complet'), action: () => { app.ouvrirGuide(); return false; } }] : [])
    .concat([{ libelle: t('fermer'), principal: true, action: () => true }]) });
}

const POLICES = ['Segoe UI', 'Arial', 'Calibri', 'Cambria', 'Times New Roman', 'Georgia', 'Verdana', 'Courier New'];

// Plot formatting: this plot's title and axis titles; for every plot the font, text and
// point sizes, palette, grid and legend (kept between sessions); the image exported at a
// chosen size.
export function dialogueFormat(app, entree, div) {
  const l = entree.loc;
  const f = { ...FORMAT };
  const titres = { titre: l.titre, titreX: l.titreX, titreY: l.titreY };
  const champ = (libelle, controle) => [el('label', {}, libelle), controle];
  const texte = (cle, defaut) => el('input', { value: titres[cle] || '', placeholder: defaut || '', style: 'width:24em',
    oninput: (e) => { titres[cle] = e.target.value; } });
  const nombre = (cle, min, max, pas) => el('input', { type: 'number', min, max, step: pas, value: f[cle], style: 'width:6em',
    oninput: (e) => { f[cle] = +e.target.value || f[cle]; } });
  const liste = (cle, options) => el('select', { onchange: (e) => { f[cle] = e.target.value; } },
    options.map(([k, n]) => el('option', { value: k, selected: k === f[cle] }, n)));
  const coche = (cle) => el('input', { type: 'checkbox', checked: f[cle], onchange: (e) => { f[cle] = e.target.checked; } });
  const lay = div.layout || {};
  const contenu = el('div', { class: 'dialogue-format' },
    el('h3', {}, t('ce_graphique')),
    el('div', { class: 'grille-champs' },
      ...champ(t('titre_graphe'), texte('titre', lay.title && lay.title.text)),
      ...champ(t('titre_axe_x'), texte('titreX', lay.xaxis && lay.xaxis.title && lay.xaxis.title.text)),
      ...champ(t('titre_axe_y'), texte('titreY', lay.yaxis && lay.yaxis.title && lay.yaxis.title.text))),
    el('p', { class: 'note' }, t('titres_vides')),
    el('h3', {}, t('tous_graphiques')),
    el('div', { class: 'grille-champs' },
      ...champ(t('police'), liste('police', POLICES.map((x) => [x, x]))),
      ...champ(t('taille_texte'), nombre('taille', 7, 28, 1)),
      ...champ(t('taille_points'), nombre('points', 0.3, 4, 0.1)),
      ...champ(t('palette'), liste('palette', [['defaut', t('palette_defaut')], ['daltonien', t('palette_daltonien')], ['gris', t('palette_gris')]])),
      ...champ(t('grille'), coche('grille')),
      ...champ(t('legende'), coche('legende'))),
    el('h3', {}, t('export_image')),
    el('div', { class: 'grille-champs' },
      ...champ(t('taille_image'), el('div', { class: 'ligne' }, nombre('largeur', 200, 10000, 10), ' × ', nombre('hauteur', 200, 10000, 10), ' px')),
      ...champ(t('echelle_image'), nombre('echelle', 1, 6, 1)),
      ...champ(t('format_image'), liste('format', [['png', 'PNG'], ['svg', 'SVG'], ['jpeg', 'JPEG']]))));
  const appliquer = () => {
    for (const [k, v] of Object.entries(titres)) if (v && v.trim()) l[k] = v.trim(); else delete l[k];
    changerFormat(f);
    app.redessiner();
  };
  const exporter = () => {
    const g = entree.graphe && entree.graphe.isConnected ? entree.graphe : div;
    const nom = ((g.layout && g.layout.title && g.layout.title.text) || 'graphique').replace(/[^\p{L}\p{N} _.-]+/gu, '_');
    window.Plotly.downloadImage(g, { format: f.format, width: f.largeur, height: f.hauteur, scale: f.format === 'svg' ? 1 : f.echelle, filename: nom });
  };
  dialogue({ titre: t('mise_en_forme'), contenu, boutons: [
    { libelle: t('annuler'), action: () => true },
    { libelle: t('exporter_image'), action: () => { appliquer(); setTimeout(exporter, 400); return true; } },
    { libelle: t('appliquer'), principal: true, action: () => { appliquer(); return true; } }] });
}

// New variable: an R formula on the project's variables, or classes cut from the values of a
// quantitative variable (given bounds, or classes of equal size). Computed in R
// (moteur/R/variables.R).
export function dialogueVariable(app, p, mode = 'formule') {
  const quantitatives = p.variables.filter((v) => !v.qualitative);
  const st = { mode, nom: '', formule: '', variable: quantitatives.length ? quantitatives[0].uid : null, parBornes: true, bornes: '', n: 3,
    // imputation by PCA (moteur/R/variables.R: imputer_acp): every quantitative variable takes part, as in a PCA of them
    imputees: new Set(quantitatives.map((v) => v.uid)), k: Math.max(1, Math.min(5, quantitatives.length - 1)), bornes2: true, remplacer: false };
  const contenu = el('div', { class: 'dialogue-variable' });
  const erreur = el('p', { class: 'erreur' });
  const nomR = (n) => (/^[\p{L}.][\p{L}\p{N}._]*$/u.test(n) ? n : `\`${n}\``);
  const rendre = () => {
    const zone = el('textarea', { rows: 3, class: 'formule', oninput: (e) => { st.formule = e.target.value; } }, st.formule);
    const inserer = el('select', { onchange: (e) => {
      if (!e.target.value) return;
      const debut = zone.selectionStart ?? zone.value.length;
      zone.setRangeText(nomR(e.target.value), debut, zone.selectionEnd ?? debut, 'end');
      st.formule = zone.value;
      e.target.value = '';
      zone.focus();
    } }, el('option', { value: '' }, t('inserer_variable')), p.variables.map((v) => el('option', { value: v.nom }, v.nom)));
    const formule = () => [
      el('label', {}, t('formule')), el('div', {}, zone, el('div', { class: 'ligne' }, inserer)),
      el('label', {}, ''), el('p', { class: 'note' }, t('formule_aide'))];
    const classes = () => [
      el('label', {}, t('variable_source')), el('select', { onchange: (e) => { st.variable = +e.target.value; } },
        quantitatives.map((v) => el('option', { value: v.uid, selected: v.uid === st.variable }, v.nom))),
      el('label', {}, ''), el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'decoupage', checked: st.parBornes, onchange: () => { st.parBornes = true; rendre(); } }), t('par_bornes')),
      el('label', {}, t('bornes')), el('input', { value: st.bornes, placeholder: '10 ; 20', disabled: !st.parBornes, oninput: (e) => { st.bornes = e.target.value; } }),
      el('label', {}, ''), el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'decoupage', checked: !st.parBornes, onchange: () => { st.parBornes = false; rendre(); } }), t('par_effectifs')),
      el('label', {}, t('nb_classes')), el('input', { type: 'number', min: 2, max: 20, value: st.n, style: 'width:5em', disabled: st.parBornes, oninput: (e) => { st.n = +e.target.value; } })];
    const imputation = () => {
      const manquent = quantitatives.filter((v) => v.manquantes > 0).length;
      return [
        el('label', {}, t('variables')), el('div', {},
          el('div', { class: 'ligne' }, el('button', { onclick: () => { st.imputees = new Set(quantitatives.map((v) => v.uid)); rendre(); } }, t('tout_cocher')),
            el('button', { onclick: () => { st.imputees = new Set(); rendre(); } }, t('aucune_cocher')),
            el('span', { class: 'infos' }, t('imputation_compte', { n: st.imputees.size, m: manquent }))),
          el('div', { class: 'liste-imputation' }, quantitatives.map((v) => el('label', { class: 'champ' },
            el('input', { type: 'checkbox', checked: st.imputees.has(v.uid), onchange: (e) => { if (e.target.checked) st.imputees.add(v.uid); else st.imputees.delete(v.uid); rendre(); } }),
            v.nom, v.manquantes ? el('span', { class: 'doux' }, ` · ${t('manquantes_n', { n: v.manquantes })}`) : '')))),
        el('label', {}, t('imputation_k')), el('input', { type: 'number', min: 1, max: 50, value: st.k, style: 'width:5em', oninput: (e) => { st.k = +e.target.value || 5; } }),
        el('label', {}, ''), el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: st.bornes2, onchange: (e) => { st.bornes2 = e.target.checked; } }), t('imputation_bornes')),
        el('label', {}, ''), el('div', {},
          el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'imputer', checked: !st.remplacer, onchange: () => { st.remplacer = false; } }), t('imputation_nouvelles')),
          el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'imputer', checked: st.remplacer, onchange: () => { st.remplacer = true; } }), t('imputation_remplacer'))),
        el('label', {}, ''), el('p', { class: 'note' }, t('imputation_note'))];
    };
    contenu.replaceChildren(
      el('div', { class: 'onglets' }, [['formule', t('formule')], ['classes', t('classes_valeurs')], ['imputation', t('imputation')]].map(([k, n]) =>
        el('button', { class: st.mode === k ? 'actif' : '', onclick: () => { st.mode = k; rendre(); } }, n))),
      el('div', { class: 'grille-champs' }, st.mode === 'imputation' ? null : [el('label', {}, t('nom')), el('input', { value: st.nom, oninput: (e) => { st.nom = e.target.value; } })],
        ...(st.mode === 'formule' ? formule() : st.mode === 'classes' ? classes() : imputation())));
  };
  async function valider() {
    erreur.textContent = '';
    let q;
    if (st.mode === 'imputation') q = { imputation: true, variables: [...st.imputees], k: st.k, bornes: st.bornes2, remplacer: st.remplacer };
    else if (st.mode === 'formule') q = { nom: st.nom, formule: st.formule };
    else if (st.parBornes) q = { nom: st.nom, variable: st.variable, bornes: st.bornes.split(/[;\s]+/).filter(Boolean).map((x) => Number(x.replace(',', '.'))) };
    else q = { nom: st.nom, variable: st.variable, n: st.n };
    try {
      await app.ajouterVariable(p.id, q);
      d.fermer();
    } catch (e) {
      erreur.textContent = e.message;
    }
  }
  rendre();
  const d = dialogue({ titre: `${t('nouvelle_variable')} — ${p.nom}`, contenu: el('div', {}, contenu, erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('ajouter'), principal: true, action: () => { valider(); return false; } }] });
}
