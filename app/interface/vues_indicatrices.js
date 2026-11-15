// Indicator species (moteur/R/indicatrices.R, type "INDVAL"): the New model fields, the views (the
// table of species, a plot of their indicator values or of one species' A and B by group, the
// summary), the report parts and the strings. A species clicked in the table or the plot is
// highlighted in both (the model's chosen variables, as for the loadings). Imported by app.js.
import { t, ajouterTextes } from './i18n.js';
import { el, fmt } from './ui.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, tableau, notesModele, valeurP, ficheCondition } from './panneaux.js';
import { ANALYSES } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const SEUIL = 0.05;
const GRIS = '#9aa7b8';
const estIndval = (m) => !!m && !!m.analyse && m.type === 'INDVAL';
const pValeur = (v) => (v === null || v === undefined ? '—' : valeurP(v));
const chiffre = (k, v, d = 3) => el('span', {}, el('small', {}, k), ' ', el('b', {}, typeof v === 'number' ? fmt(v, d) : v ?? '—'));
const colonne = (df, k) => liste((df || {})[k]);
// each species' row of the table, as an object
const especes = (m) => colonne(m.especes, 'espece').map((nom, i) => Object.fromEntries(Object.keys(m.especes).map((k) => [k, colonne(m.especes, k)[i]])));
const significative = (e) => e.p_ajuste !== null && e.p_ajuste !== undefined && e.p_ajuste <= SEUIL;
// a colour per single group, grey for a combination of groups
const couleurGroupe = (m, groupe) => {
  const k = colonne(m.groupes, 'groupe').indexOf(groupe);
  return k < 0 ? GRIS : PALETTE[k % PALETTE.length];
};
const choisir = (ctx, m, nom) => {
  const s = new Set(ctx.variablesChoisies(m.id));
  if (s.has(nom)) s.delete(nom); else s.add(nom);
  ctx.choisirVariables(m.id, [...s]);
};

// ------------------------------------------------------------ the table of species

function rendreIndicatrices(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const toutes = especes(m);
  const lignes = loc.significatives ? toutes.filter(significative) : toutes;
  const choisies = new Set(ctx.variablesChoisies(m.id));
  const groupes = colonne(m.groupes, 'groupe');
  loc.table ??= {};
  loc.table.tri ??= { cle: 'p', sens: 1 };
  const note = el('p', { class: 'note' }, noteIndicatrices(m));
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(note, bas);
  const case_ = el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: !!loc.significatives,
    onchange: (e) => { loc.significatives = e.target.checked; ctx.rafraichir(); } }), t('iv_significatives'));
  const alerte = (cle) => (i) => lignes[i][cle] !== null && lignes[i][cle] !== undefined && lignes[i][cle] <= SEUIL;
  tableau(bas, ctx, loc.table, {
    n: lignes.length, nom: 'Indicator species', avant: [case_],
    colonnes: [{ cle: 'espece', titre: t('iv_espece') },
      { cle: 'groupe', titre: t('iv_groupe'), couleur: (i) => couleurGroupe(m, lignes[i].groupe) },
      { cle: 'A', titre: 'A', num: true, decimales: 3 }, { cle: 'B', titre: 'B', num: true, decimales: 3 },
      { cle: 'indval', titre: m.options.indice === 'IndVal' ? 'IndVal' : 'IndVal.g', num: true, decimales: 3 },
      { cle: 'p', titre: 'p', num: true, format: valeurP, alerte: alerte('p') },
      { cle: 'p_ajuste', titre: `${t('p_ajuste')} (${m.options.correction === 'holm' ? 'Holm' : 'BH'})`, num: true, format: valeurP, alerte: alerte('p_ajuste') },
      { cle: 'presences', titre: t('iv_presences'), num: true, decimales: 0 }, { cle: 'abondance', titre: t('iv_abondance'), num: true, decimales: 2 }]
      .concat(groupes.map((g) => ({ cle: `s.${g}`, titre: g, num: true, decimales: 0 }))),
    valeur: (i, k) => lignes[i][k],
    choisie: (i) => choisies.has(lignes[i].espece),
    cliquer: (i) => choisir(ctx, m, lignes[i].espece),
  });
}

function noteIndicatrices(m) {
  const o = m.options || {};
  const r = m.retenues || {};
  const n = liste(m.groupes.groupe).length;
  const combinaisons = m.ordre > 1 ? t('iv_note_combinaisons', { ordre: m.ordre >= n ? n : m.ordre }) : t('iv_note_seuls');
  const test = o.permutations ? t('iv_note_test', { n: o.permutations, brutes: r.brutes, corrigees: r.corrigees, correction: o.correction === 'holm' ? 'Holm' : 'Benjamini–Hochberg' })
    : t('iv_note_sans_test');
  return `${t(o.indice === 'IndVal' ? 'iv_note_indval' : 'iv_note_indval_g')} ${combinaisons} ${test}${m.strates ? ` ${t('ord_permutations_dans', { g: m.strates })}` : ''}`;
}

// ------------------------------------------------------------ the plot

// Each species' indicator value for its group (bars sorted, coloured by the group, faded when not
// significant after correction), or one species' A, B and indicator value in each single group.
function rendreGrapheIndval(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const toutes = especes(m);
  const choisies = new Set(ctx.variablesChoisies(m.id));
  loc.mode ??= 'indval';
  const noms = toutes.map((e) => e.espece);
  if (!noms.includes(loc.espece)) loc.espece = noms.find((x) => choisies.has(x)) || [...toutes].sort((a, b) => b.indval - a.indval)[0].espece;
  const graphe = el('div', { class: 'graphe' });
  conteneur.replaceChildren(outils(
    choix('', [['indval', t('iv_mode_indval')], ['ab', t('iv_mode_ab')]], loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    loc.mode === 'ab' ? choix(t('iv_espece'), noms.map((x) => [x, x]), loc.espece, (v) => { loc.espece = v; ctx.rafraichir(); }) : null,
    el('span', { class: 'infos' }, t(loc.mode === 'ab' ? 'iv_graphe_ab_note' : 'iv_graphe_note'))), graphe);
  const c = couleursTheme(ctx);
  if (loc.mode === 'ab') {
    const groupes = colonne(m.groupes, 'groupe');
    const i = noms.indexOf(loc.espece);
    const val = (df) => groupes.map((g) => colonne(df, g)[i]);
    const barre = (nom, y, couleur) => ({ type: 'bar', name: nom, x: groupes, y, marker: { color: couleur }, hovertemplate: `<b>%{x}</b><br>${nom} %{y:.3f}<extra></extra>` });
    tracer(ctx, graphe, [barre(t('iv_specificite'), val(m.specificite), PALETTE[0]), barre(t('iv_fidelite'), val(m.fidelite), PALETTE[1]),
      barre(m.options.indice === 'IndVal' ? 'IndVal' : 'IndVal.g', val(m.indval), c.texte)],
    { title: { text: `${m.nom} · ${loc.espece}` }, barmode: 'group', dragmode: false, yaxis: { range: [0, 1.05], title: { text: t('iv_valeur') } } });
    return;
  }
  const ordre = [...toutes].sort((a, b) => a.indval - b.indval).slice(-60);  // (the 60 largest, the largest on top)
  const traces = [{ type: 'bar', orientation: 'h', x: ordre.map((e) => e.indval), y: ordre.map((e) => e.espece), customdata: ordre.map((e) => e.espece),
    text: ordre.map((e) => e.groupe), meta: 'variables',
    marker: { color: ordre.map((e) => couleurGroupe(m, e.groupe)), opacity: ordre.map((e) => (significative(e) ? 1 : 0.35)),
      line: { width: ordre.map((e) => (choisies.has(e.espece) ? 2.5 : 0)), color: c.texte } },
    hovertemplate: '<b>%{y}</b> · %{text}<br>%{x:.3f}<extra></extra>' }];
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${m.options.indice === 'IndVal' ? 'IndVal' : 'IndVal.g'}` }, dragmode: false, showlegend: false,
    xaxis: { range: [0, 1.02], title: { text: t('iv_valeur') } }, yaxis: { automargin: true, type: 'category' },
    margin: { l: 90 } });
  graphe.on('plotly_click', (ev) => { const pt = ev.points && ev.points[0]; if (pt && pt.customdata) choisir(ctx, m, pt.customdata); });
}

// ------------------------------------------------------------ summary

function rendreResumeIndval(conteneur, ctx, m) {
  const o = m.options || {};
  const r = m.retenues || {};
  const fiche = el('div', { class: 'fiche' },
    el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, 'IndVal'), m.titre ? el('span', { class: 'doux' }, m.titre) : null),
    el('div', { class: 'fiche-chiffres' }, chiffre('N', m.N, 0), chiffre(t('iv_especes'), m.K, 0), chiffre(t('iv_groupes'), liste(m.groupes.groupe).length, 0),
      chiffre(t('iv_combinaisons'), liste(m.combinaisons).length, 0), m.exclues_manquantes ? chiffre(t('obs_incompletes'), m.exclues_manquantes, 0) : null),
    el('div', { class: 'fiche-chiffres' }, chiffre(t('iv_indice'), o.indice === 'IndVal' ? 'IndVal' : 'IndVal.g'), chiffre(t('nb_permutations'), o.permutations || 0, 0),
      o.permutations ? chiffre(t('iv_retenues_p'), r.brutes, 0) : null, o.permutations ? chiffre(t('iv_retenues_p_ajuste'), r.corrigees, 0) : null));
  fiche.append(...[ficheCondition(m)].filter(Boolean));
  const groupes = el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, el('th', {}, m.classe_nom), el('th', { class: 'num' }, 'n'), el('th', { class: 'num' }, t('iv_indicatrices_groupe')))),
    el('tbody', {}, colonne(m.groupes, 'groupe').map((g, k) => el('tr', {}, el('td', {}, g), el('td', { class: 'num' }, String(colonne(m.groupes, 'n')[k])),
      el('td', { class: 'num' }, String(especes(m).filter((e) => e.groupe === g && significative(e)).length))))));
  const notes = notesModele(conteneur, ctx, m);
  const textes = [noteIndicatrices(m), liste(m.absentes).length ? t('iv_absentes', { noms: liste(m.absentes).join(', ') }) : ''].filter(Boolean);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, notes.champ, groupes, textes.map((x) => el('p', { class: 'note' }, x))));
  notes.retrouver();
}

// ------------------------------------------------------------ the model dialog

const nombre = (o, cle, defaut, attrs) => el('input', { type: 'number', value: o[cle] ?? defaut, style: 'width:6em', ...attrs,
  oninput: (e) => { o[cle] = e.target.value === '' ? null : Number(e.target.value); } });
function champsIndval(st, rendre, p) {
  const o = st.options;
  const ids = [].concat(p.identifiants || []).map((i) => i.nom);
  const qualitatives = p.variables.filter((v) => v.qualitative);
  const strate = !o.strates ? '' : o.strates.by === 'id' ? `id:${o.strates.id}` : `var:${o.strates.variable}`;
  const option = (v, n) => el('option', { value: v, selected: v === strate }, n);
  const combinaisons = o.ordre === 1 ? 'seuls' : o.ordre ? 'ordre' : 'toutes';
  return [
    el('label', {}, t('iv_indice')), el('select', { onchange: (e) => { o.indice = e.target.value; } },
      [['IndVal.g', t('iv_indval_g')], ['IndVal', t('iv_indval')]].map(([k, n]) => el('option', { value: k, selected: k === (o.indice || 'IndVal.g') }, n))),
    el('label', {}, t('iv_combinaisons')), el('div', { class: 'ligne' }, el('select', { onchange: (e) => {
      o.ordre = e.target.value === 'seuls' ? 1 : e.target.value === 'ordre' ? 2 : null;
      rendre();
    } }, [['toutes', t('iv_toutes')], ['seuls', t('iv_seuls')], ['ordre', t('iv_jusqua')]].map(([k, n]) => el('option', { value: k, selected: k === combinaisons }, n))),
    combinaisons === 'ordre' ? nombre(o, 'ordre', 2, { min: 2, max: 12, step: 1 }) : null),
    el('label', {}, t('iv_donnees')), el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: !!o.presence,
      onchange: (e) => { o.presence = e.target.checked; } }), t('iv_presence')),
    el('label', {}, t('nb_permutations')), el('div', { class: 'ligne' }, nombre(o, 'permutations', 999, { min: 0, max: 99999, step: 1 }),
      el('label', { class: 'champ' }, t('ord_graine'), nombre(o, 'graine', 1, { step: 1 }))),
    el('label', {}, t('ord_strates')), el('select', { onchange: (e) => {
      const v = e.target.value;
      o.strates = !v ? null : v.startsWith('id:') ? { by: 'id', id: v.slice(3) } : { by: 'variable', variable: Number(v.slice(4)) };
    } }, option('', t('ord_strates_aucune')),
    ids.length ? el('optgroup', { label: t('vc_par_identifiant') }, ids.map((n) => option(`id:${n}`, n))) : null,
    qualitatives.length ? el('optgroup', { label: t('vc_par_variable') }, qualitatives.map((v) => option(`var:${v.uid}`, v.nom))) : null),
    el('label', {}, t('correction')), el('select', { onchange: (e) => { o.correction = e.target.value; } },
      [['BH', t('correction_bh')], ['holm', 'Holm']].map(([k, n]) => el('option', { value: k, selected: k === (o.correction || 'BH') }, n))),
    el('label', {}, ''), el('p', { class: 'note' }, t('iv_reglages_note')),
  ];
}
ANALYSES.INDVAL = {
  libelle: () => t('type_indval'),
  note: () => t('iv_note'),
  roles: ['y', 'classe'],
  echelle: 'none',
  vc: false,
  champs: champsIndval,
  valider: (st, cles, p) => {
    if (cles('x').length) return t('iv_sans_x');
    if (!cles('y').length) return t('iv_y_requis');
    if (cles('classe').length !== 1) return t('iv_classe_requise');
    const q = cles('y').filter((u) => (p.variables.find((v) => v.uid === u) || {}).qualitative);
    if (q.length) return t('ord_quantitatives', { noms: q.map((u) => (p.variables.find((v) => v.uid === u) || {}).nom).join(', ') });
    const o = st.options;
    const entier = (v, a, b) => v === undefined || v === null || (Number.isInteger(v) && v >= a && v <= b);
    if (!entier(o.permutations, 0, 99999)) return t('ord_permutations_invalides');
    if (!entier(o.ordre, 1, 12)) return t('iv_ordre_invalide');
    return '';
  },
  options: (st) => {
    const o = st.options;
    return { indice: o.indice === 'IndVal' ? 'IndVal' : 'IndVal.g', ordre: Number.isInteger(o.ordre) ? o.ordre : null, presence: !!o.presence,
      permutations: Number.isFinite(o.permutations) ? o.permutations : 999, graine: Number.isFinite(o.graine) ? Math.round(o.graine) : 1,
      strates: o.strates || null, correction: o.correction === 'holm' ? 'holm' : 'BH' };
  },
};

// ------------------------------------------------------------ the report

const blocsResume = (m) => {
  const o = m.options || {};
  const r = m.retenues || {};
  return [{ texte: `${t('type_indval')} · N = ${m.N} · ${t('iv_especes')} ${m.K} · ${t('iv_groupes')} ${liste(m.groupes.groupe).length}` },
    { entetes: [t('iv_indice'), t('iv_combinaisons'), t('nb_permutations'), t('iv_retenues_p'), t('iv_retenues_p_ajuste')],
      lignes: [[o.indice === 'IndVal' ? 'IndVal' : 'IndVal.g', String(liste(m.combinaisons).length), String(o.permutations || 0),
        o.permutations ? String(r.brutes) : '—', o.permutations ? String(r.corrigees) : '—']] },
    { texte: noteIndicatrices(m) }];
};
const blocsEspeces = (m) => {
  const lignes = especes(m).filter((e) => !m.options.permutations || significative(e)).sort((a, b) => (a.p_ajuste ?? 2) - (b.p_ajuste ?? 2) || b.indval - a.indval);
  return [{ texte: m.options.permutations ? t('iv_rapport_significatives', { n: lignes.length }) : t('iv_note_sans_test') },
    { entetes: [t('iv_espece'), t('iv_groupe'), 'A', 'B', 'IndVal', 'p', t('p_ajuste')],
      lignes: lignes.map((e) => [e.espece, e.groupe, fmt(e.A, 3), fmt(e.B, 3), fmt(e.indval, 3), pValeur(e.p), pValeur(e.p_ajuste)]) }];
};
PARTIES_TYPES.INDVAL = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'indicatrices', defaut: true, tableau: blocsEspeces },
  { cle: 'indval_graphe', vue: 'indval_graphe', defaut: true },
];

// ------------------------------------------------------------ registration

const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estIndval(m) ? rendreResumeIndval(c, ctx, m) : resume(c, ctx, p, loc);
} };
VUES.indicatrices = { rendre: rendreIndicatrices, titre: (m) => `${m.nom} · ${t('indicatrices')}` };
VUES.indval_graphe = { rendre: rendreGrapheIndval, titre: (m) => `${m.nom} · ${t('indval_graphe')}` };
Object.assign(ICONES_VUES, { indicatrices: 'list-checks', indval_graphe: 'chart-bar' });
VUES_TYPES.INDVAL = ['resume', 'indicatrices', 'indval_graphe', 'liste_obs'];

ajouterTextes({
  type_indval: 'Espèces indicatrices (IndVal)', indicatrices: 'Espèces indicatrices', indval_graphe: 'Valeurs indicatrices',
  iv_note: 'Y : les espèces, des abondances ou des présences (0/1) ; Classe : la variable qualitative des groupes d’observations (des milieux, des types de sites). La valeur indicatrice de chaque espèce pour chaque groupe, ou chaque combinaison de groupes, comme le paquet indicspecies la calcule (multipatt), et son test par permutation. Les observations sans groupe ou où une valeur manque sont laissées de côté.',
  iv_indice: 'Indice', iv_indval_g: 'IndVal.g (groupes égalisés, De Cáceres et Legendre 2009)', iv_indval: 'IndVal (Dufrêne et Legendre 1997)',
  iv_combinaisons: 'Combinaisons de groupes', iv_toutes: 'toutes (jusqu’à tous les groupes)', iv_seuls: 'les groupes seuls', iv_jusqua: 'jusqu’à l’ordre',
  iv_donnees: 'Données', iv_presence: 'présences seules (0/1)',
  iv_reglages_note: 'Le test permute les groupes des observations (999 fois par défaut ; 0 : aucun test), à l’intérieur des strates choisies s’il y en a ; la graine rend les permutations reproductibles. Les valeurs p sont corrigées sur les espèces.',
  iv_sans_x: 'Les espèces indicatrices ne prennent pas de variable X : les espèces en Y, les groupes en Classe.',
  iv_y_requis: 'Les espèces indicatrices demandent des espèces en Y.', iv_classe_requise: 'Les espèces indicatrices demandent une variable de groupes (rôle Classe).',
  iv_ordre_invalide: 'L’ordre des combinaisons est un entier de 1 à 12.',
  iv_espece: 'Espèce', iv_groupe: 'Groupe', iv_presences: 'Présences', iv_abondance: 'Abondance', iv_especes: 'Espèces', iv_groupes: 'Groupes',
  iv_significatives: 'p corrigée ≤ 0,05 seulement', iv_valeur: 'Valeur indicatrice', iv_specificite: 'A (spécificité)', iv_fidelite: 'B (fidélité)',
  iv_mode_indval: 'Valeur de chaque espèce', iv_mode_ab: 'A et B d’une espèce par groupe',
  iv_graphe_note: 'Couleur : le groupe de l’espèce (gris : une combinaison) ; pâle : p corrigée > 0,05. Un clic met l’espèce en évidence.',
  iv_graphe_ab_note: 'A : la part de l’abondance de l’espèce dans le groupe ; B : la part des observations du groupe où elle est.',
  iv_note_indval_g: 'IndVal.g : la racine de A × B, A comparant les abondances moyennes des groupes (De Cáceres et Legendre 2009), B la part des observations du groupe où l’espèce est.',
  iv_note_indval: 'IndVal : la racine de A × B, A comparant les abondances totales des groupes (Dufrêne et Legendre 1997), B la part des observations du groupe où l’espèce est.',
  iv_note_combinaisons: 'Chaque espèce va au groupe, ou à la combinaison de 2 à {ordre} groupes, de sa plus grande valeur (une espèce rattachée à tous les groupes n’est pas testée).',
  iv_note_seuls: 'Chaque espèce va au groupe de sa plus grande valeur.',
  iv_note_test: '{n} permutations : {brutes} espèces ont p ≤ 0,05, {corrigees} après la correction de {correction}.',
  iv_note_sans_test: 'Aucune permutation n’a été demandée : pas de valeur p.',
  iv_retenues_p: 'p ≤ 0,05', iv_retenues_p_ajuste: 'p corrigée ≤ 0,05', iv_indicatrices_groupe: 'Indicatrices (p corrigée ≤ 0,05)',
  iv_absentes: 'Absentes de toutes les observations, laissées de côté : {noms}.',
  iv_rapport_significatives: '{n} espèces indicatrices (p corrigée ≤ 0,05).',
}, {
  type_indval: 'Indicator species (IndVal)', indicatrices: 'Indicator species', indval_graphe: 'Indicator values',
  iv_note: 'Y: the species, abundances or presences (0/1); Class: the qualitative variable of the groups of observations (habitats, site types). The indicator value of each species for each group, or each combination of groups, as the indicspecies package computes it (multipatt), and its permutation test. Observations without a group or where a value is missing are left out.',
  iv_indice: 'Index', iv_indval_g: 'IndVal.g (group-equalized, De Cáceres and Legendre 2009)', iv_indval: 'IndVal (Dufrêne and Legendre 1997)',
  iv_combinaisons: 'Combinations of groups', iv_toutes: 'all (up to all the groups)', iv_seuls: 'single groups', iv_jusqua: 'up to order',
  iv_donnees: 'Data', iv_presence: 'presences only (0/1)',
  iv_reglages_note: 'The test permutes the groups of the observations (999 times by default; 0: no test), within the strata chosen if any; the seed makes the permutations reproducible. The p-values are corrected over the species.',
  iv_sans_x: 'Indicator species take no X variable: the species as Y, the groups as Class.',
  iv_y_requis: 'Indicator species need species as Y.', iv_classe_requise: 'Indicator species need a grouping variable (Class role).',
  iv_ordre_invalide: 'The order of the combinations is a whole number from 1 to 12.',
  iv_espece: 'Species', iv_groupe: 'Group', iv_presences: 'Presences', iv_abondance: 'Abundance', iv_especes: 'Species', iv_groupes: 'Groups',
  iv_significatives: 'corrected p ≤ 0.05 only', iv_valeur: 'Indicator value', iv_specificite: 'A (specificity)', iv_fidelite: 'B (fidelity)',
  iv_mode_indval: 'Value of each species', iv_mode_ab: 'A and B of a species by group',
  iv_graphe_note: 'Colour: the species’ group (grey: a combination); pale: corrected p > 0.05. A click highlights the species.',
  iv_graphe_ab_note: 'A: the share of the species’ abundance in the group; B: the share of the group’s observations where it is.',
  iv_note_indval_g: 'IndVal.g: the square root of A × B, A comparing the mean abundances of the groups (De Cáceres and Legendre 2009), B the share of the group’s observations where the species is.',
  iv_note_indval: 'IndVal: the square root of A × B, A comparing the total abundances of the groups (Dufrêne and Legendre 1997), B the share of the group’s observations where the species is.',
  iv_note_combinaisons: 'Each species goes to the group, or the combination of 2 to {ordre} groups, of its largest value (a species assigned to all the groups isn’t tested).',
  iv_note_seuls: 'Each species goes to the group of its largest value.',
  iv_note_test: '{n} permutations: {brutes} species have p ≤ 0.05, {corrigees} after the {correction} correction.',
  iv_note_sans_test: 'No permutations were asked for: no p-value.',
  iv_retenues_p: 'p ≤ 0.05', iv_retenues_p_ajuste: 'corrected p ≤ 0.05', iv_indicatrices_groupe: 'Indicators (corrected p ≤ 0.05)',
  iv_absentes: 'Absent from every observation, left out: {noms}.',
  iv_rapport_significatives: '{n} indicator species (corrected p ≤ 0.05).',
});
