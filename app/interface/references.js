// References to a project's results in a document (documents.js): {{M1.Q2}}, {{M1.scores | caption}}...
// Pure functions, without the page: reading the references of a text, the values they give (in
// the document's language), and what each kind of model offers (for the completion).
//
// A reference is written {{target[:format] [#label] [| caption]}}:
//   - target: a model of the project by its name (M1, or "M2 (1)": names can hold spaces), a dot,
//     then what to take from it: a value (Q2, R2Y, N, gain, vip.sucre, coef.annee...), a table
//     (table.composantes), a plot (scores, loadings, vip...), the Methods text (methodes) or the
//     results text (resultats, moteur/R/resultats_texte.R); another analysis (TRIM, distance
//     sampling, RDA, PERMANOVA, NMDS...), its key results by the keys What changed compares
//     (pente, pente.ic, pente.pourcent, D, D.cv, R2_ajuste, test.modele.p, stress...);
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
// `decimales` is given): "−0,123 [−0,201 ; −0,046]".
function avecIntervalle(est, bas, haut, decimales, langue) {
  const d = Number.isInteger(decimales) ? decimales : decimalesSignificatives(est);
  const n = (x) => nombre(x, { decimales: d, langue });
  return { n, ic: Number.isFinite(bas) && Number.isFinite(haut) ? `[${n(bas)}${langue === 'fr' ? ' ; ' : ', '}${n(haut)}]` : '' };
}
// (the decimals a reference asks for, :3; null otherwise)
const decimalesDe = (ref) => (/^\d$/.test(ref.format) ? Number(ref.format) : null);

// How a value is written, with the format of its reference (:3 decimals, :% or :%1), else its own defaults.
function formeValeur(x, ref, defaut = {}) {
  if (/^%/.test(ref.format)) return { forme: 'pourcentage', x, decimales: Number(ref.format.slice(1) || 0) };
  if (/^\d$/.test(ref.format)) return { forme: 'nombre', x, decimales: Number(ref.format) };
  if (defaut.pourcentage) return { forme: 'pourcentage', x, decimales: defaut.decimales ?? 0 };
  if (defaut.entier) return { forme: 'nombre', x, decimales: 0 };
  return { forme: 'nombre', x, decimales: defaut.decimales ?? 2, significatifs: defaut.significatifs ?? null };
}

// A value as text (formeReference says how), in the document's language.
export function texteForme(f, langue = 'fr') {
  switch (f.forme) {
    case 'pourcentage': return pourcentage(f.x, { decimales: f.decimales, langue });
    case 'p': return valeurP(f.x, { langue, decimales: f.decimales });
    case 'coef': {
      const { n, ic } = avecIntervalle(f.est, f.bas, f.haut, f.decimales, langue);
      const p = valeurP(f.p, { langue });
      return f.quoi === 'est' ? n(f.est) : f.quoi === 'ic' ? ic : f.quoi === 'p' ? p : `${n(f.est)} ${ic}, ${p}`;
    }
    case 'coef_cs': {
      const { n, ic } = avecIntervalle(f.est, f.est - f.ic, f.est + f.ic, f.decimales, langue);
      return ic ? `${n(f.est)} ${ic}` : n(f.est);
    }
    // (an interval alone, in numbers or percentages: "[0,962 ; 0,978]", "[−3,8 % ; −2,2 %]")
    case 'ic': {
      const n = (x) => (f.pourcentage ? pourcentage(x, { decimales: f.decimales, langue }) : nombre(x, { decimales: f.decimales, langue }));
      return `[${n(f.bas)}${langue === 'fr' ? ' ; ' : ', '}${n(f.haut)}]`;
    }
    // (a text in the document's language: TRIM's class of a slope)
    case 'texte': return (f.textes || {})[langue] ?? f.texte ?? '';
    default: return nombre(f.x, { decimales: f.decimales, significatifs: f.significatifs, langue });
  }
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

// The values of an R-code model (moteur/R/code.R): those the engine could take from the model, its
// coefficients (coef, est, ic, p and se, by term) and the values its code gives (valeurs.<name>).
const statistiqueCode = (k) => (m) => (m.statistiques || {})[k];
const VALEURS_CODE = {
  N: { f: (m) => m.N, entier: true },
  K: { f: (m) => m.K, entier: true },
  logLik: { f: statistiqueCode('logLik'), decimales: 2, si: (m) => statistiqueCode('logLik')(m) !== undefined },
  AIC: { f: statistiqueCode('AIC'), decimales: 1, si: (m) => statistiqueCode('AIC')(m) !== undefined },
  BIC: { f: statistiqueCode('BIC'), decimales: 1, si: (m) => statistiqueCode('BIC')(m) !== undefined },
};
const valeursCode = (m) => m.valeurs || {};
function referenceCode(m, nom, reste, ref) {
  const c = m.coefficients || {};
  const terme = reste.join('.');
  if (['coef', 'est', 'p', 'ic', 'se'].includes(nom)) {
    const i = colonne(c.terme).indexOf(terme);
    if (i < 0) return { erreur: 'terme_inconnu' };
    if (nom === 'se') return formeValeur(colonne(c.erreur_type)[i], ref, { significatifs: 3 });
    return { forme: 'coef', quoi: nom, est: colonne(c.estimation)[i], bas: colonne(c.bas)[i], haut: colonne(c.haut)[i], p: colonne(c.p)[i], decimales: decimalesDe(ref) };
  }
  if (nom === 'valeurs') {
    const i = colonne(valeursCode(m).nom).indexOf(terme);
    if (i < 0) return { erreur: 'valeur_inconnue' };
    return formeValeur(colonne(valeursCode(m).valeur)[i], ref, { significatifs: 3 });
  }
  const v = VALEURS_CODE[nom];
  if (!v || (v.si && !v.si(m))) return { erreur: 'valeur_inconnue' };
  return formeValeur(v.f(m, reste), ref, v);
}

// Another analysis's key results (moteur/R/resultats_cles.R: resume_resultats, m.cles), the numbers
// What changed compares, under the same keys: {{M1.pente}} an entry's value; .ic, .bas, .haut its 95 %
// interval; .p its p-value; a multiplicative value (a TRIM slope, an index: limite 1) .pourcent its
// change in % (.pourcent.ic...); TRIM's overall slope .classe its class in words; distance sampling's
// density or abundance .cv their coefficient of variation (m.densite). A value: an integer, a
// percentage (1 decimal), a multiplicative value (4 decimals, as TRIM writes them), otherwise 3
// significant digits; the decimals of a reference (:3) or a percentage (:%) win. Null: not a key.
const CLASSES_TRIM = {
  fr: { forte_hausse: 'forte augmentation', hausse_moderee: 'augmentation modérée', stable: 'stable', incertaine: 'incertaine',
    declin_modere: 'déclin modéré', fort_declin: 'fort déclin', inconnue: 'inconnue' },
  en: { forte_hausse: 'strong increase', hausse_moderee: 'moderate increase', stable: 'stable', incertaine: 'uncertain',
    declin_modere: 'moderate decline', fort_declin: 'steep decline', inconnue: 'unknown' },
};
const PARTIES = ['ic', 'bas', 'haut', 'p', 'classe', 'cv'];
const entreesCles = (m) => colonne(m.cles);
function referenceCle(m, chemin, ref) {
  const cles = entreesCles(m);
  if (!cles.length) return null;
  const trouver = (k) => cles.find((e) => e.cle === k && e.valeur !== undefined && e.valeur !== null);
  // the longest key first (keys hold dots: indice.2010, D.Nord, permanova.habitat), then its part
  let e = trouver(chemin.join('.'));
  let partie = '';
  let pourcent = false;
  if (!e && PARTIES.includes(chemin[chemin.length - 1])) {
    partie = chemin[chemin.length - 1];
    e = trouver(chemin.slice(0, -1).join('.'));
    if (!e && chemin[chemin.length - 2] === 'pourcent') { pourcent = true; e = trouver(chemin.slice(0, -2).join('.')); }
  }
  if (!e && chemin[chemin.length - 1] === 'pourcent') { pourcent = true; e = trouver(chemin.slice(0, -1).join('.')); }
  if (!e) return null;
  const v = Number(e.valeur);
  const ic = colonne(e.ic);
  const fixe = decimalesDe(ref);
  if (pourcent) {
    if (Number(e.limite) !== 1 || (partie && !['ic', 'bas', 'haut'].includes(partie))) return { erreur: 'valeur_inconnue' };
    const d = fixe ?? 1;
    if (!partie) return { forme: 'pourcentage', x: v - 1, decimales: d };
    if (ic.length !== 2) return { erreur: 'valeur_absente' };
    if (partie === 'ic') return { forme: 'ic', bas: ic[0] - 1, haut: ic[1] - 1, decimales: d, pourcentage: true };
    return { forme: 'pourcentage', x: (partie === 'bas' ? ic[0] : ic[1]) - 1, decimales: d };
  }
  // (the decimals of the value, those of its interval too)
  const decimales = fixe ?? (e.forme === 'entier' ? 0 : e.forme === 'pourcentage' ? 1 : Number(e.limite) === 1 ? 4 : decimalesSignificatives(v));
  const enPourcentage = e.forme === 'pourcentage';
  const valeur = (x) => (enPourcentage ? { forme: 'pourcentage', x, decimales } : { forme: 'nombre', x, decimales });
  switch (partie) {
    case '': return /^%/.test(ref.format) ? formeValeur(v, ref) : valeur(v);
    case 'p': return e.p === undefined || e.p === null ? { erreur: 'valeur_absente' } : { forme: 'p', x: e.p, decimales: fixe ?? 3 };
    case 'ic': return ic.length === 2 ? { forme: 'ic', bas: ic[0], haut: ic[1], decimales, pourcentage: enPourcentage } : { erreur: 'valeur_absente' };
    case 'bas': case 'haut': return ic.length === 2 ? valeur(ic[partie === 'bas' ? 0 : 1]) : { erreur: 'valeur_absente' };
    case 'classe': {
      if (e.cle !== 'pente' || !e.classe) return { erreur: 'valeur_inconnue' };
      return { forme: 'texte', textes: { fr: CLASSES_TRIM.fr[e.classe] ?? e.classe, en: CLASSES_TRIM.en[e.classe] ?? e.classe } };
    }
    case 'cv': {
      // (distance sampling: the CV of the density or abundance, of the total or of a stratum)
      const d = m.densite || {};
      const strate = e.cle.startsWith('D.') ? e.cle.slice(2) : null;
      if (!(e.cle === 'D' || e.cle === 'abondance' || strate)) return { erreur: 'valeur_inconnue' };
      const i = strate ? colonne(d.strate).indexOf(strate) : colonne(d.cv).length - 1;
      const cv = colonne(d.cv)[i];
      return cv === undefined || cv === null ? { erreur: 'valeur_absente' } : { forme: 'pourcentage', x: cv, decimales: fixe ?? 1 };
    }
    default: return null;
  }
}
// What another analysis offers from its key results: each value, its interval and p-value when it
// has them, a multiplicative value's change in %, TRIM's class, distance sampling's CV; not A, nor a
// K of 0 (distance sampling has no X term).
function offreCles(m, valeur) {
  for (const e of entreesCles(m)) {
    if (e.valeur === undefined || e.valeur === null || e.cle === 'A' || (e.cle === 'K' && Number(e.valeur) === 0)) continue;
    valeur(e.cle);
    if (colonne(e.ic).length === 2) valeur(`${e.cle}.ic`);
    if (e.p !== undefined && e.p !== null) valeur(`${e.cle}.p`);
    if (Number(e.limite) === 1) valeur(`${e.cle}.pourcent`);
    if (e.cle === 'pente' && e.classe) valeur('pente.classe');
    if (m.type === 'DISTANCE' && (e.cle === 'D' || e.cle === 'abondance' || e.cle.startsWith('D.'))) valeur(`${e.cle}.cv`);
  }
}

// The terms a regression has coefficients for, and its columns.
const coefficientsRegression = (m) => m.coefficients_regression || {};
const termesRegression = (m) => colonne(coefficientsRegression(m).terme);
const termesComposantes = (m) => colonne(m.variables && m.variables.terme);

// The value a reference gives, as text: { texte } or { erreur }. `m`: the model's results (as
// the interface has them); `chemin`: what follows the model's name.
export function valeurReference(m, chemin, ref, langue = 'fr') {
  const f = formeReference(m, chemin, ref);
  return f.erreur ? f : { texte: texteForme(f, langue) };
}

// What a reference gives before it is written (texteForme writes it): a number { forme: 'nombre',
// x, decimales, significatifs }, a percentage { forme: 'pourcentage', x, decimales }, a p-value
// { forme: 'p', x, decimales }, a regression's coefficient { forme: 'coef', quoi (coef, est, ic or p),
// est, bas, haut, p, decimales }, a PLS coefficient with its interval { forme: 'coef_cs', est, ic,
// decimales }, an interval alone { forme: 'ic', bas, haut, decimales, pourcentage }, a text in each
// language { forme: 'texte', textes }; or { erreur }. A reproducible Quarto file writes the same in R
// (documents.js; moteur/R/document_reproductible.R).
export function formeReference(m, chemin, ref) {
  const [nom, ...reste] = chemin;
  const err = (e) => ({ erreur: e });
  if (m.code_r) return referenceCode(m, nom, reste, ref);
  if (m.regression) {
    if (['coef', 'est', 'p', 'ic'].includes(nom)) return coefficientRegression(m, nom, reste.join('.'), ref);
    const v = VALEURS_REGRESSION[nom];
    if (!v || (v.si && !v.si(m))) return err('valeur_inconnue');
    return formeValeur(v.f(m, reste), ref, v);
  }
  // another analysis: its key results first (then, as before, what it shares with a model of components)
  if (m.analyse && m.cles) {
    const f = referenceCle(m, chemin, ref);
    if (f) return f;
  }
  if (nom === 'vip' && reste.length) {
    const i = termesComposantes(m).indexOf(reste.join('.'));
    if (i < 0 || !m.variables.VIP) return err('variable_inconnue');
    return formeValeur(colonne(m.variables.VIP)[i], ref, { decimales: 2 });
  }
  if (nom === 'coef' && reste.length) return coefficientComposantes(m, reste.join('.'), ref);
  if (nom === 'cvanova' && ['p', 'F'].includes(reste[0])) {
    const a = (m.cv_anova || {})[reste[1] || premierY(m)];
    if (!a) return err('valeur_inconnue');
    return reste[0] === 'p' ? { forme: 'p', x: colonne(a.p)[1], decimales: decimalesDe(ref) ?? 3 } : formeValeur(colonne(a.F)[1], ref, { decimales: 2 });
  }
  const v = VALEURS_COMPOSANTES[nom];
  if (!v || (v.si && !v.si(m))) return err('valeur_inconnue');
  const x = v.f(m, reste);
  if (x === null || x === undefined) return err('valeur_absente');
  return formeValeur(x, ref, v);
}

// A PLS coefficient (centred and scaled, of the first Y) with its 95 % interval: "−0,35 [−0,56 ; −0,14]".
function coefficientComposantes(m, terme, ref) {
  const c = m.coefficients;
  if (!c) return { erreur: 'valeur_inconnue' };
  const i = colonne(c.terme).indexOf(terme);
  const y = premierY(m);
  if (i < 0) return { erreur: 'variable_inconnue' };
  const est = colonne(c[`CoeffCS ${y}`])[i];
  if (!Number.isFinite(est)) return { erreur: 'valeur_absente' };  // (the constant, centred and scaled: none)
  return { forme: 'coef_cs', est, ic: colonne(c[`CoeffCS ${y} IC`])[i], decimales: decimalesDe(ref) };
}

// A regression coefficient: coef (estimate, interval and p), est, ic or p alone.
function coefficientRegression(m, quoi, terme, ref) {
  const c = coefficientsRegression(m);
  const i = termesRegression(m).indexOf(terme);
  if (i < 0) return { erreur: 'terme_inconnu' };
  return { forme: 'coef', quoi, est: colonne(c.estimation)[i], bas: colonne(c.bas)[i], haut: colonne(c.haut)[i], p: colonne(c.p)[i], decimales: decimalesDe(ref) };
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
  if (m.code_r) {
    Object.keys(VALEURS_CODE).forEach(valeur);
    for (const terme of colonne((m.coefficients || {}).terme)) valeur(`coef.${terme}`);
    for (const nom of colonne(valeursCode(m).nom)) valeur(`valeurs.${nom}`);
  } else if (m.regression) {
    Object.keys(VALEURS_REGRESSION).forEach(valeur);
    for (const terme of termesRegression(m)) valeur(`coef.${terme}`);
  } else if (m.analyse && m.cles) {
    offreCles(m, valeur);
  } else {
    Object.keys(VALEURS_COMPOSANTES).forEach(valeur);
    if (m.cv_anova && Object.keys(m.cv_anova).length) valeur('cvanova.p');
    if (m.variables && m.variables.VIP) termesComposantes(m).forEach((terme) => valeur(`vip.${terme}`));
    if (m.coefficients) colonne(m.coefficients.terme).forEach((terme) => valeur(`coef.${terme}`));
  }
  for (const t of tableaux) offre.push({ chemin: `table.${t}`, type: 'tableau', detail: t });
  for (const v of vues) offre.push({ chemin: v, type: 'graphique', detail: v });
  offre.push({ chemin: 'methodes', type: 'texte', detail: '' });
  // (the Methods text's references alone, in the document's list of references: documents.js)
  offre.push({ chemin: 'methodes.references', type: 'texte', detail: '' });
  offre.push({ chemin: 'resultats', type: 'texte', detail: 'resultats' });
  return offre;
}

// The kind of thing a path names, before resolving it: 'tableau', 'texte', 'graphique' (one of
// `vues`) or 'valeur'.
export function genreChemin(chemin, vues = []) {
  if (chemin[0] === 'table') return 'tableau';
  if ((chemin[0] === 'methodes' || chemin[0] === 'resultats') && chemin.length === 1) return 'texte';
  if (chemin[0] === 'methodes' && chemin[1] === 'references' && chemin.length === 2) return 'texte';
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
