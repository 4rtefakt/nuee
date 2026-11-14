// References to a project's results in a document (documents.js): {{M1.Q2}}, {{M1.scores | caption}}...
// Pure functions, without the page: reading the references of a text, the values they give (in
// the document's language), and what each kind of model offers (for the completion).
//
// A reference is written {{target[:format] [#label] [| caption]}}:
//   - target: a model of the project by its name (M1, or "M2 (1)": names can hold spaces), a dot,
//     then what to take from it: a value (Q2, R2Y, N, gain, vip.sucre, coef.annee...), a table
//     (table.composantes), a plot (scores, loadings, vip...) or the Methods text (methodes);
//     or ref:<label>, the number of a figure or table labelled #<label> ("Figure 2");
//   - format: a number of decimals (:3), or a percentage (:% or :%1);
//   - #label: a name for a figure or table, for {{ref:label}} elsewhere;
//   - caption: the caption of a figure or table (after |).
// A plot, a table or the Methods text is a block: alone on its line (a paragraph of its own).

const MOTIF = /\{\{([\s\S]*?)\}\}/g;

// The references of `texte`, in order: { debut, fin, brut, cible, format, etiquette, legende }.
export function lireReferences(texte) {
  const refs = [];
  for (const r of String(texte).matchAll(MOTIF)) {
    const ref = lireReference(r[1]);
    if (ref) refs.push({ debut: r.index, fin: r.index + r[0].length, brut: r[0], ...ref });
  }
  return refs;
}

// The parts of what is inside {{ }} (null if it is empty, or a Quarto shortcode: {{< pagebreak >}},
// {{% ... %}}, left to Quarto).
export function lireReference(interieur) {
  let s = String(interieur).trim();
  if (!s || /^[<%]/.test(s)) return null;
  let legende = '';
  const barre = s.indexOf('|');
  if (barre >= 0) {
    legende = s.slice(barre + 1).trim();
    s = s.slice(0, barre).trim();
  }
  let etiquette = '';
  const diese = /\s#([\p{L}\p{N}_-]+)\s*$/u.exec(s) || /^#([\p{L}\p{N}_-]+)$/u.exec(s);
  if (diese) {
    etiquette = diese[1];
    s = s.slice(0, diese.index).trim();
  }
  let format = '';
  const f = /:(%\d?|\d)$/.exec(s);
  if (f) {
    format = f[1];
    s = s.slice(0, f.index).trim();
  }
  return { cible: s, format, etiquette, legende };
}

// The model a target names and the path after it: the longest model name followed by a dot
// (names hold spaces, brackets, even dots); null when none fits.
export function separerCible(cible, noms) {
  const tries = [...noms].sort((a, b) => b.length - a.length);
  for (const nom of tries) {
    if (cible.startsWith(nom + '.') && cible.length > nom.length + 1) return { modele: nom, chemin: cible.slice(nom.length + 1).split('.') };
  }
  return null;
}

// ------------------------------------------------------------ numbers in the document's language

const MOINS = '−';
const ESPACE_FINE = ' ';

export function nombre(v, { decimales = 2, langue = 'fr', significatifs = null } = {}) {
  if (v === null || v === undefined || Number.isNaN(Number(v))) return '—';
  const x = Number(v);
  if (!Number.isFinite(x)) return x > 0 ? '∞' : `${MOINS}∞`;
  let d = decimales;
  if (significatifs) {  // as many decimals as `significatifs` significant digits need (at most 6)
    const ordre = x === 0 ? 0 : Math.floor(Math.log10(Math.abs(x)));
    d = Math.max(0, Math.min(6, significatifs - 1 - ordre));
  }
  const texte = Math.abs(x).toLocaleString(langue === 'fr' ? 'fr-FR' : 'en-GB', {
    minimumFractionDigits: d, maximumFractionDigits: d, useGrouping: Math.abs(x) >= 10000,
  }).replace(/\s/g, ESPACE_FINE);
  return (x < 0 && Number(texte.replace(/[^\d]/g, '')) !== 0 ? MOINS : '') + texte;
}

export function pourcentage(v, { decimales = 0, langue = 'fr' } = {}) {
  if (v === null || v === undefined) return '—';
  return `${nombre(100 * v, { decimales, langue })}${langue === 'fr' ? ESPACE_FINE : ''}%`;
}

// A p-value as it is written in a text: "p < 0,001", "p = 0,032".
export function valeurP(p, { langue = 'fr', decimales = 3 } = {}) {
  if (p === null || p === undefined || Number.isNaN(Number(p))) return 'p = —';
  const seuil = 10 ** -decimales;
  return Number(p) < seuil ? `p < ${nombre(seuil, { decimales, langue })}` : `p = ${nombre(p, { decimales, langue })}`;
}

// The decimals that give `significatifs` significant digits to x (at most 6).
const decimalesSignificatives = (x, significatifs = 3) => {
  const ordre = !Number.isFinite(x) || x === 0 ? 0 : Math.floor(Math.log10(Math.abs(x)));
  return Math.max(0, Math.min(6, significatifs - 1 - ordre));
};

// An estimate with its interval, all with the estimate's decimals (3 significant digits unless
// the reference asks for decimals): "−0,123 [−0,201 ; −0,046]".
function avecIntervalle(est, bas, haut, ref, langue) {
  const d = /^\d$/.test(ref.format) ? Number(ref.format) : decimalesSignificatives(est);
  const n = (x) => nombre(x, { decimales: d, langue });
  return { n, ic: Number.isFinite(bas) && Number.isFinite(haut) ? `[${n(bas)}${langue === 'fr' ? ' ; ' : ', '}${n(haut)}]` : '' };
}

// A value with the format of its reference (:3 decimals, :% or :%1), else its own defaults.
function enTexte(v, ref, langue, defaut = {}) {
  if (/^%/.test(ref.format)) return pourcentage(v, { decimales: Number(ref.format.slice(1) || 0), langue });
  if (/^\d$/.test(ref.format)) return nombre(v, { decimales: Number(ref.format), langue });
  if (defaut.pourcentage) return pourcentage(v, { decimales: defaut.decimales ?? 0, langue });
  if (defaut.entier) return nombre(v, { decimales: 0, langue });
  return nombre(v, { decimales: defaut.decimales ?? 2, significatifs: defaut.significatifs ?? null, langue });
}

// ------------------------------------------------------------ what a model offers

const colonne = (x) => [].concat(x ?? []);
const dernier = (x) => { const c = colonne(x); return c.length ? c[c.length - 1] : null; };
const aLaComposante = (x, a) => (a ? colonne(x)[Number(a) - 1] ?? null : dernier(x));
const premierY = (m) => colonne(m.noms_y)[0];

// The values of a model of components (PCA, PLS, PLS-DA, OPLS), in the order the completion offers
// them: name -> function(m, rest of the path) giving the number; `si`: only for the models it applies to.
const VALEURS_COMPOSANTES = {
  R2X: { f: (m, [a]) => aLaComposante(m.composantes && m.composantes.R2Xcum, a) },
  R2Y: { f: (m, [a]) => aLaComposante(m.composantes && m.composantes.R2Ycum, a), si: (m) => m.type !== 'PCA' },
  Q2: { f: (m, [a]) => aLaComposante(m.composantes && m.composantes.Q2cum, a) },
  Q2_vide: { f: (m) => m.q2_vide, si: (m) => Number.isFinite(m.q2_vide) },
  gain: { f: (m) => m.gain, si: (m) => Number.isFinite(m.gain) },
  Q2_honnete: { f: (m) => dernier(m.hierarchique && m.hierarchique.q2cum), si: (m) => !!(m.hierarchique && m.hierarchique.q2cum) },
  gain_honnete: { f: (m) => m.hierarchique && m.hierarchique.gain, si: (m) => !!(m.hierarchique && Number.isFinite(m.hierarchique.gain)) },
  RMSEE: { f: (m, [y]) => (m.RMSEE || {})[y || premierY(m)], significatifs: 3, si: (m) => !!m.RMSEE },
  RMSEcv: { f: (m, [y]) => (m.RMSEcv || {})[y || premierY(m)], significatifs: 3, si: (m) => !!m.RMSEcv },
  bien_classes: { f: (m) => m.classification && m.classification.vc.global, pourcentage: true, decimales: 1, si: (m) => !!m.classification },
  A: { f: (m) => m.A, entier: true },
  N: { f: (m) => m.N, entier: true },
  K: { f: (m) => m.K, entier: true },
  G: { f: (m) => m.G, entier: true },
};

// The values of a regression (lm, nlme, mgcv).
const VALEURS_REGRESSION = {
  N: { f: (m) => m.N, entier: true },
  K: { f: (m) => m.K, entier: true },
  R2: { f: (m) => (m.qualite || {}).r2, si: (m) => (m.qualite || {}).r2 !== undefined },
  R2_ajuste: { f: (m) => (m.qualite || {}).r2_ajuste, si: (m) => (m.qualite || {}).r2_ajuste !== undefined },
  R2_marginal: { f: (m) => (m.qualite || {}).marginal, si: (m) => (m.qualite || {}).marginal !== undefined },
  R2_conditionnel: { f: (m) => (m.qualite || {}).conditionnel, si: (m) => (m.qualite || {}).conditionnel !== undefined },
  deviance_expliquee: { f: (m) => (m.qualite || {}).deviance_expliquee, pourcentage: true, decimales: 1, si: (m) => (m.qualite || {}).deviance_expliquee !== undefined },
  Q2: { f: (m) => (m.qualite || {}).Q2 },
  RMSEcv: { f: (m) => (m.qualite || {}).RMSEcv, significatifs: 3 },
  AIC: { f: (m) => (m.qualite || {}).AIC, decimales: 1 },
  AICc: { f: (m) => (m.qualite || {}).AICc, decimales: 1 },
  BIC: { f: (m) => (m.qualite || {}).BIC, decimales: 1 },
};

// The terms a regression has coefficients for, and its columns.
const coefficientsRegression = (m) => m.coefficients_regression || {};
const termesRegression = (m) => colonne(coefficientsRegression(m).terme);
const termesComposantes = (m) => colonne(m.variables && m.variables.terme);

// The value a reference gives, as text: { texte } or { erreur }. `m`: the model's results (as
// the interface has them); `chemin`: what follows the model's name.
export function valeurReference(m, chemin, ref, langue = 'fr') {
  const [nom, ...reste] = chemin;
  const err = (e) => ({ erreur: e });
  if (m.regression) {
    if (['coef', 'est', 'p', 'ic'].includes(nom)) return coefficientRegression(m, nom, reste.join('.'), ref, langue);
    const v = VALEURS_REGRESSION[nom];
    if (!v || (v.si && !v.si(m))) return err('valeur_inconnue');
    return { texte: enTexte(v.f(m, reste), ref, langue, v) };
  }
  if (nom === 'vip' && reste.length) {
    const i = termesComposantes(m).indexOf(reste.join('.'));
    if (i < 0 || !m.variables.VIP) return err('variable_inconnue');
    return { texte: enTexte(colonne(m.variables.VIP)[i], ref, langue, { decimales: 2 }) };
  }
  if (nom === 'coef' && reste.length) return coefficientComposantes(m, reste.join('.'), ref, langue);
  if (nom === 'cvanova' && ['p', 'F'].includes(reste[0])) {
    const a = (m.cv_anova || {})[reste[1] || premierY(m)];
    if (!a) return err('valeur_inconnue');
    return reste[0] === 'p' ? { texte: valeurP(colonne(a.p)[1], { langue, decimales: /^\d$/.test(ref.format) ? Number(ref.format) : 3 }) }
      : { texte: enTexte(colonne(a.F)[1], ref, langue, { decimales: 2 }) };
  }
  const v = VALEURS_COMPOSANTES[nom];
  if (!v || (v.si && !v.si(m))) return err('valeur_inconnue');
  const x = v.f(m, reste);
  if (x === null || x === undefined) return err('valeur_absente');
  return { texte: enTexte(x, ref, langue, v) };
}

// A PLS coefficient (centred and scaled, of the first Y) with its 95 % interval: "−0,35 [−0,56 ; −0,14]".
function coefficientComposantes(m, terme, ref, langue) {
  const c = m.coefficients;
  if (!c) return { erreur: 'valeur_inconnue' };
  const i = colonne(c.terme).indexOf(terme);
  const y = premierY(m);
  if (i < 0) return { erreur: 'variable_inconnue' };
  const est = colonne(c[`CoeffCS ${y}`])[i];
  if (!Number.isFinite(est)) return { erreur: 'valeur_absente' };  // (the constant, centred and scaled: none)
  const ic = colonne(c[`CoeffCS ${y} IC`])[i];
  const { n, ic: texteIC } = avecIntervalle(est, est - ic, est + ic, ref, langue);
  return { texte: texteIC ? `${n(est)} ${texteIC}` : n(est) };
}

// A regression coefficient: coef (estimate, interval and p), est, ic or p alone.
function coefficientRegression(m, quoi, terme, ref, langue) {
  const c = coefficientsRegression(m);
  const i = termesRegression(m).indexOf(terme);
  if (i < 0) return { erreur: 'terme_inconnu' };
  const est = colonne(c.estimation)[i];
  const { n, ic } = avecIntervalle(est, colonne(c.bas)[i], colonne(c.haut)[i], ref, langue);
  const p = valeurP(colonne(c.p)[i], { langue });
  if (quoi === 'est') return { texte: n(est) };
  if (quoi === 'ic') return { texte: ic };
  if (quoi === 'p') return { texte: p };
  return { texte: `${n(est)} ${ic}, ${p}` };
}

// What a model offers, for the completion: [{ chemin, type: 'valeur' | 'tableau' | 'graphique' |
// 'texte', detail }] (detail: the value it gives now, or the name of the plot). `vues`: the plots
// its panels can draw (panneaux.js VUES that apply to it), `tableaux`: the tables (TABLEAUX).
export function offreModele(m, { vues = [], tableaux = [], langue = 'fr' } = {}) {
  const offre = [];
  const valeur = (chemin) => {
    const r = valeurReference(m, chemin.split('.'), { format: '' }, langue);
    if (!r.erreur) offre.push({ chemin, type: 'valeur', detail: r.texte });
  };
  if (m.regression) {
    Object.keys(VALEURS_REGRESSION).forEach(valeur);
    for (const terme of termesRegression(m)) valeur(`coef.${terme}`);
  } else {
    Object.keys(VALEURS_COMPOSANTES).forEach(valeur);
    if (m.cv_anova && Object.keys(m.cv_anova).length) valeur('cvanova.p');
    if (m.variables && m.variables.VIP) termesComposantes(m).forEach((terme) => valeur(`vip.${terme}`));
    if (m.coefficients) colonne(m.coefficients.terme).forEach((terme) => valeur(`coef.${terme}`));
  }
  for (const t of tableaux) offre.push({ chemin: `table.${t}`, type: 'tableau', detail: t });
  for (const v of vues) offre.push({ chemin: v, type: 'graphique', detail: v });
  offre.push({ chemin: 'methodes', type: 'texte', detail: '' });
  return offre;
}

// The kind of thing a path names, before resolving it: 'tableau', 'texte', 'graphique' (one of
// `vues`) or 'valeur'.
export function genreChemin(chemin, vues = []) {
  if (chemin[0] === 'table') return 'tableau';
  if (chemin[0] === 'methodes' && chemin.length === 1) return 'texte';
  if (chemin.length === 1 && vues.includes(chemin[0])) return 'graphique';
  return 'valeur';
}

// Numbers for the figures and tables of a document, in their order, and the label of each:
// { numeros: Map(index of the reference -> number), etiquettes: Map(label -> { genre, numero }) }.
// `genre(ref)` says whether a reference is a 'figure', a 'tableau' or neither (null).
export function numeroter(refs, genre) {
  const compte = { figure: 0, tableau: 0 };
  const numeros = new Map();
  const etiquettes = new Map();
  refs.forEach((ref, i) => {
    const g = genre(ref);
    if (g !== 'figure' && g !== 'tableau') return;
    compte[g] += 1;
    numeros.set(i, compte[g]);
    if (ref.etiquette) etiquettes.set(ref.etiquette, { genre: g, numero: compte[g] });
  });
  return { numeros, etiquettes };
}
