// R-code models, the user's own (moteur/R/code.R, type CODE): the code editor in New model
// (CodeMirror with R highlighting, a help with an example), the code approved before it is fitted,
// the views of a fitted model (its summary and statistics, the coefficients, the residual plots, the
// code's own plots, its values and tables, the code itself), its report parts, and the bar over a
// project whose code was written elsewhere: Show the code, Run it (docs/deploiement.md: opening a file
// never runs code from it; the engine refuses code nobody approved on this computer). Imported by app.js.
import { t, ajouterTextes, deuxPoints, separateurIntervalle } from './i18n.js';
import { el, fmt, decimal, icone, dialogue } from './ui.js';
import { demander, bureau, MOTEUR_WEBR, moteurWebR } from './api.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, tableau, notesModele, valeurP, ficheCondition, etendue } from './panneaux.js';
import { ANALYSES } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const estCode = (m) => !!m && !!m.analyse && m.type === 'CODE';
const nombre = (v, d = 3) => (v === null || v === undefined || !Number.isFinite(v) ? '—' : fmt(v, d));
const chiffre = (k, v) => el('span', {}, el('small', {}, k), ' ', el('b', {}, v ?? '—'));
let appli = null;  // (the application, given by installerCode)

// ------------------------------------------------------------ approving code

// Code approved by this user on this computer (typed and fitted, or read and run): its fingerprint,
// computed by the engine, kept by the desktop app (main.js: approuver-code), which tells the engine;
// in a browser (an engine started by hand), told to the engine directly, for its session; in the
// browser version, also kept by the browser for the next visits (moteur_webr.js). (A plugin is
// approved the same way: vues_greffons.js.)
export async function approuver(empreintes) {
  if (!empreintes.length) return;
  if (bureau && bureau.approuverCode) {
    if (!(await bureau.approuverCode(empreintes))) throw new Error(t('code_approbation_echec'));
  } else {
    await demander('code_approuve', { empreintes });
    if (MOTEUR_WEBR) (await moteurWebR()).garderApprobations(empreintes);
  }
}
async function approuverCodeTape(q) {
  const { empreinte } = await demander('empreinte_code', { code: (q.options || {}).code || '' });
  await approuver([empreinte]);
}

// ------------------------------------------------------------ the editor

// R in the editor (a CodeMirror stream language): comments, strings (over several lines too),
// numbers, keywords, constants, backquoted names, operators.
const MOTS_R = new Set(['if', 'else', 'repeat', 'while', 'function', 'for', 'in', 'next', 'break', 'return']);
const CONSTANTES_R = new Set(['TRUE', 'FALSE', 'NULL', 'NA', 'NA_integer_', 'NA_real_', 'NA_character_', 'NA_complex_', 'Inf', 'NaN']);
function chaineR(stream, etat) {
  let echappe = false;
  for (let c = stream.next(); c !== undefined && c !== null; c = stream.next()) {
    if (c === etat.chaine && !echappe) { etat.chaine = null; break; }
    echappe = !echappe && c === '\\';
  }
  return 'string';
}
let langage = null;
function langageR(CM) {
  if (langage || !CM.StreamLanguage) return langage;
  langage = CM.StreamLanguage.define({
    name: 'r',
    startState: () => ({ chaine: null }),
    token(stream, etat) {
      if (etat.chaine) return chaineR(stream, etat);
      if (stream.eatSpace()) return null;
      if (stream.match('#')) { stream.skipToEnd(); return 'comment'; }
      const c = stream.peek();
      if (c === '"' || c === "'") { stream.next(); etat.chaine = c; return chaineR(stream, etat); }
      if (c === '`') { stream.next(); if (stream.skipTo('`')) stream.next(); else stream.skipToEnd(); return 'variableName'; }
      if (stream.match(/^(0[xX][0-9a-fA-F]+|(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?)[iL]?/)) return 'number';
      if (stream.match(/^[A-Za-z.][\w.]*/)) {
        const w = stream.current();
        return MOTS_R.has(w) ? 'keyword' : CONSTANTES_R.has(w) ? 'atom' : 'variableName';
      }
      if (stream.match(/^(<<-|<-|->>|->|\|>|%[^%\s]*%|::?:?|[-+*/^=<>!&|~$@?])/)) return 'operator';
      stream.next();
      return null;
    },
  });
  return langage;
}
let styleR = null;
const styleCode = (CM) => styleR || (styleR = CM.HighlightStyle.define([
  { tag: CM.tags.comment, color: 'var(--texte-doux)', fontStyle: 'italic' },
  { tag: CM.tags.keyword, color: 'var(--accent)', fontWeight: '600' },
  { tag: CM.tags.atom, color: 'var(--attention)' },
  { tag: CM.tags.number, color: 'var(--attention)' },
  { tag: CM.tags.string, color: 'var(--ok)' },
  { tag: CM.tags.operator, color: 'var(--accent)' },
]));

// CodeMirror, from the documents' file (copier-vendor.js: vendor/editeur.min.js), loaded once.
let chargement = null;
function chargerEditeur() {
  if (window.CM) return Promise.resolve(window.CM);
  if (!chargement) {
    chargement = new Promise((ok, ko) => {
      const s = el('script', { src: 'vendor/editeur.min.js' });
      s.onload = () => ok(window.CM);
      s.onerror = () => { chargement = null; ko(new Error(t('document_editeur_absent'))); };
      document.head.append(s);
    });
  }
  return chargement;
}

// The editor of the dialog's code (st.options.code), CodeMirror, or a plain text field until it has
// loaded (or if it can't); Ctrl+Enter fits. st._editeur: the CodeMirror view, to put the cursor on the
// line of an error.
function editeurCode(st) {
  const boite = el('div', { class: 'editeur-code' });
  const changer = (texte) => { st.options.code = texte; };
  const zone = el('textarea', { class: 'formule', rows: 14, spellcheck: 'false', 'aria-label': t('code_r'), oninput: (e) => changer(e.target.value) }, st.options.code || '');
  boite.append(zone);
  chargerEditeur().then((CM) => {
    if (!boite.isConnected && !document.body.contains(zone)) return;
    const ajuster = () => {
      const b = boite.closest('.dialogue');
      const bouton = b && [...b.querySelectorAll('.pied-dialogue button.principal')].pop();
      if (bouton) bouton.click();
      return true;
    };
    const r = langageR(CM);
    const vue = new CM.EditorView({
      parent: boite,
      state: CM.EditorState.create({
        doc: zone.value,
        extensions: [CM.lineNumbers ? CM.lineNumbers() : [], CM.history(), CM.drawSelection(), CM.highlightActiveLine(), CM.indentOnInput(), CM.closeBrackets(),
          CM.bracketMatching ? CM.bracketMatching() : [], r ? [r, CM.syntaxHighlighting(styleCode(CM))] : [],
          CM.placeholder(t('code_placeholder')),
          CM.keymap.of([{ key: 'Mod-Enter', run: ajuster }, ...CM.closeBracketsKeymap, ...CM.defaultKeymap, ...CM.historyKeymap]),
          CM.EditorView.contentAttributes.of({ 'aria-label': t('code_r'), spellcheck: 'false' }),
          CM.EditorView.updateListener.of((u) => { if (u.docChanged) changer(u.state.doc.toString()); })],
      }),
    });
    zone.remove();
    // (the dialog drawn again, a new editor each time: the one before, out of the page, let go)
    if (st._editeur && st._editeur !== vue && !st._editeur.dom.isConnected) st._editeur.destroy();
    st._editeur = vue;
  }).catch(() => { /* the plain field stays */ });
  return boite;
}

// The cursor on the line where the code stopped (the engine's error says it: code.R, erreur_code).
function surErreurCode(st, e) {
  const ligne = e && e.reponse && Number(e.reponse.ligne);
  const vue = st._editeur;
  if (!vue || !ligne || ligne < 1 || ligne > vue.state.doc.lines) return;
  const l = vue.state.doc.line(ligne);
  vue.dispatch({ selection: { anchor: l.from, head: l.to }, scrollIntoView: true });
  vue.focus();
}

// A name as R code writes it (between backquotes when it isn't a syntactic name).
const nomR = (n) => (/^([A-Za-z]|[.][A-Za-z._])[\w.]*$/.test(n) || /^[.]$/.test(n) ? n : `\`${String(n).replace(/`/g, '\\`')}\``);
// The example of the help, with the project's own variables: a linear model of the first
// quantitative variable on the next one.
function exemple(p) {
  const q = p.variables.filter((v) => !v.qualitative);
  const y = q[0] ? nomR(q[0].nom) : 'y';
  const x = q[1] ? nomR(q[1].nom) : 'x';
  return `${t('code_exemple_commentaire')}\nfit <- lm(${y} ~ ${x}, data = donnees)\nfit\n`;
}

// The General tab's fields: the editor, the help (what `donnees` holds, the last value, the list form,
// an example), the columns of `donnees`.
function champsCode(st, rendre, p) {
  const colonnes = p.variables.map((v) => `${v.nom}${v.qualitative ? ` (${t('code_facteur')})` : ''}`)
    .concat(liste(p.identifiants).map((i) => `${i.nom} (${t('identifiant')})`));
  const aide = el('details', { class: 'aide-code', open: !(st.options.code || '').trim() },
    el('summary', {}, t('code_aide_titre')),
    el('p', {}, t('code_aide_donnees')),
    el('p', {}, t('code_aide_valeur')),
    el('pre', { class: 'code-sortie' }, t('code_aide_liste')),
    el('p', {}, t('code_aide_sortie')),
    el('p', { class: 'note' }, t('code_aide_colonnes', { n: colonnes.length }), ' ', colonnes.slice(0, 40).join(', '), colonnes.length > 40 ? '…' : ''),
    el('button', { onclick: () => { st.options.code = exemple(p); rendre(); } }, icone('file-code'), t('code_inserer_exemple')));
  return [el('label', {}, t('code_r')), el('div', { class: 'champ-code' }, editeurCode(st), aide)];
}

ANALYSES.CODE = {
  libelle: () => t('type_code'),
  note: () => t('code_note'),
  roles: [],
  echelle: 'none',
  vc: false,
  xFacultatif: true,
  onglets: ['general', 'observations'],  // (no Variables or Terms tab: the code says what it uses)
  sansEchelle: true,
  offert: () => !(appli && appli.codeInterdit),
  champs: champsCode,
  valider: (st) => (String(st.options.code || '').trim() ? '' : t('code_requis')),
  options: (st) => ({ code: String(st.options.code || '') }),
  avantAjuster: approuverCodeTape,
  surErreur: surErreurCode,
};

// ------------------------------------------------------------ the summary

const texteBloc = (lignes, classe = 'code-sortie') => el('pre', { class: classe }, liste(lignes).join('\n'));
const paquetsTexte = (m) => {
  const p = m.paquets || {};
  return liste(p.nom).map((n, i) => `${n} ${liste(p.version)[i] ?? ''}`.trim()).join(', ');
};
const nObservations = (m) => (m.n_donnees && m.N !== m.n_donnees ? `${m.N} / ${m.n_donnees}` : String(m.N));

// (a plugin's model, vues_greffons.js: its own badge, its card after the figures, its own buttons)
export function rendreResumeCode(conteneur, ctx, m, { badge = t('code_badge'), carte = null, boutons = null } = {}) {
  const s = m.statistiques || {};
  const titre = el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, badge), m.titre ? el('span', { class: 'doux' }, m.titre) : null);
  const fiche = el('div', { class: 'fiche' }, titre,
    el('div', { class: 'fiche-chiffres' }, chiffre(t('code_classe'), liste(m.classe)[0] || '—'), chiffre('N', nObservations(m)),
      s.logLik !== undefined ? chiffre('logLik', `${nombre(s.logLik, 2)}${s.ddl !== undefined ? ` (${s.ddl} ${t('ddl')})` : ''}`) : null,
      s.AIC !== undefined ? chiffre('AIC', nombre(s.AIC, 1)) : null, s.BIC !== undefined ? chiffre('BIC', nombre(s.BIC, 1)) : null),
    el('div', { class: 'fiche-chiffres' }, chiffre('R', m.R), paquetsTexte(m) ? chiffre(t('code_paquets'), paquetsTexte(m)) : null),
    ficheCondition(m),
    m.n_donnees && m.N !== m.n_donnees ? el('p', { class: 'note' }, t('code_observations_laissees', { n: m.n_donnees - m.N, total: m.n_donnees })) : null);
  const c = m.comparaison || {};
  const notes = notesModele(conteneur, ctx, m);
  const avert = liste(m.avertissements);
  const messages = liste(m.messages);
  const sortie = liste(m.sortie);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, carte,
    el('div', { class: 'ligne' }, boutons || [el('button', { onclick: () => modifierCode(m) }, icone('square-pen'), t('code_modifier')),
      el('button', { onclick: () => ctx.ouvrirVue('code_source', m.id) }, icone('file-code'), t('code_source'))]),
    notes.champ,
    avert.length ? el('div', { class: 'alerte-donnees' }, el('b', {}, t('code_avertissements', { n: avert.length })), el('ul', {}, avert.slice(0, 50).map((a) => el('li', {}, a)))) : null,
    liste(m.autres).length ? el('p', { class: 'note attention' }, t('code_autres', { noms: liste(m.autres).join(', ') })) : null,
    el('p', { class: 'note' }, c.signature ? t('code_comparaison_oui') : `${t('code_comparaison_non')}${deuxPoints()}${c.raison || '—'}`),
    liste(m.resume).length ? [el('h4', {}, t('code_resume_texte')), texteBloc(m.resume)] : el('p', { class: 'note' }, t('code_sans_modele')),
    sortie.length ? el('details', {}, el('summary', {}, t('code_sortie', { n: sortie.length })), texteBloc(sortie)) : null,
    messages.length ? el('details', {}, el('summary', {}, t('code_messages', { n: messages.length })), texteBloc(messages)) : null));
  notes.retrouver();
}

// ------------------------------------------------------------ the coefficients

const lignesCoefficients = (m) => {
  const c = m.coefficients || {};
  return liste(c.terme).map((terme, i) => ({ terme, estimation: liste(c.estimation)[i], erreur_type: liste(c.erreur_type)[i], statistique: liste(c.statistique)[i],
    ddl: liste(c.ddl)[i], p: liste(c.p)[i], bas: liste(c.bas)[i], haut: liste(c.haut)[i] }));
};

// Each coefficient with its 95 % interval (the t distribution at its degrees of freedom when its test
// is a t, otherwise the normal: code.R, coefficients_matrice), then the table. The plot leaves the
// intercepts out (on a scale of their own, they would flatten the others), the table has them.
const constante = (terme) => /(^|\.)\(Intercept\)$/.test(terme);
function rendreCoefficientsCode(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const lignes = lignesCoefficients(m);
  if (!lignes.length) { conteneur.replaceChildren(el('p', { class: 'note' }, t('code_sans_coefficients'))); return; }
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(el('p', { class: 'note' }, t('code_coefficients_note')), graphe, bas);
  const c = couleursTheme(ctx);
  const sans = lignes.filter((l) => !constante(l.terme));
  const montres = (sans.length ? sans : lignes).slice(0, 60);
  const signif = (l) => l.p !== null && l.p !== undefined && l.p <= 0.05;
  const g4 = (v) => (v === null || v === undefined || !Number.isFinite(v) ? '—' : decimal(Number(v).toPrecision(4)));
  // (as a regression's: p ≤ 0.05 filled and red, the others open, not by colour alone; the first at the top)
  tracer(ctx, graphe, [{ type: 'scatter', mode: 'markers', x: montres.map((l) => l.estimation), y: montres.map((l) => l.terme),
    error_x: { type: 'data', symmetric: false, array: montres.map((l) => (l.haut ?? l.estimation) - l.estimation), arrayminus: montres.map((l) => l.estimation - (l.bas ?? l.estimation)),
      color: c.doux, thickness: 1.2, width: 4 },
    marker: { size: 8, color: montres.map((l) => (signif(l) ? PALETTE[1] : PALETTE[0])), symbol: montres.map((l) => (signif(l) ? 'circle' : 'circle-open')) },
    customdata: montres.map((l) => [g4(l.bas), g4(l.haut), valeurP(l.p) || '—']),
    hovertemplate: `<b>%{y}</b><br>%{x:.4g} [%{customdata[0]}${separateurIntervalle()}%{customdata[1]}]<br>p = %{customdata[2]}<extra></extra>` }],
  { title: { text: `${m.nom} · ${t('coefficients')}` }, showlegend: false, dragmode: 'zoom',
    xaxis: { title: { text: t('code_estimation') } }, yaxis: { autorange: 'reversed' },
    shapes: [{ type: 'line', yref: 'paper', y0: 0, y1: 1, x0: 0, x1: 0, line: { color: c.doux, width: 1, dash: 'dot' } }] });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: lignes.length, nom: 'Coefficients',
    colonnes: [{ cle: 'terme', titre: t('terme') }, { cle: 'estimation', titre: t('code_estimation'), num: true, decimales: 4 },
      { cle: 'erreur_type', titre: t('code_erreur_type'), num: true, decimales: 4 }, { cle: 'statistique', titre: t('code_statistique'), num: true, decimales: 3 },
      { cle: 'ddl', titre: t('ddl'), num: true, decimales: 1 }, { cle: 'p', titre: 'p', num: true, decimales: 4 },
      { cle: 'bas', titre: t('code_ic_bas'), num: true, decimales: 4 }, { cle: 'haut', titre: t('code_ic_haut'), num: true, decimales: 4 }],
    valeur: (i, k) => lignes[i][k],
  });
}

// ------------------------------------------------------------ the residuals

// The residuals against the fitted values, and their normal QQ plot (its line through the quartiles):
// both side by side, or one. A point is an observation when Nuée knows which (a click selects it).
function rendreDiagnosticsCode(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const a = m.ajustees;
  if (!a || !liste(a.ajuste).length) { conteneur.replaceChildren(el('p', { class: 'note' }, t('code_sans_residus'))); return; }
  if (!['deux', 'ajustes', 'qq'].includes(loc.mode)) loc.mode = 'deux';
  const x = liste(a.ajuste);
  const r = liste(a.residu);
  const uid = liste(a.uid);
  const noms = liste(a.nom);
  const g1 = el('div', { class: 'graphe' });
  const g2 = el('div', { class: 'graphe' });
  const graphes = loc.mode === 'deux' ? [g1, g2] : [loc.mode === 'qq' ? g2 : g1];
  // (without the observations' uids, said: code.R, lignes_ajustement_code)
  const note = uid.length ? t('code_residus_note') : `${t('code_residus_note')} ${t('code_residus_sans_lien')}`;
  conteneur.replaceChildren(outils(choix('', [['deux', t('code_deux_graphes')], ['ajustes', t('residus_ajustes')], ['qq', t('residus_qq')]], loc.mode,
    (v) => { loc.mode = v; ctx.rafraichir(); }), el('span', { class: 'infos' }, note)), el('div', { class: 'deux-graphes' }, graphes));
  const c = couleursTheme(ctx);
  const sel = new Set(ctx.selection(m.id));
  const couleurs = uid.length ? uid.map((u) => (sel.has(u) ? '#e4572e' : PALETTE[0])) : PALETTE[0];
  if (graphes.includes(g1)) {
    tracer(ctx, g1, [{ type: x.length > 3000 ? 'scattergl' : 'scatter', mode: 'markers', x, y: r, customdata: uid.length ? uid : undefined, text: noms.length ? noms : undefined,
      marker: { size: 6, color: couleurs, opacity: 0.8 }, hovertemplate: `${noms.length ? '<b>%{text}</b><br>' : ''}%{x:.4g} · %{y:.4g}<extra></extra>` }],
    { title: { text: `${m.nom} · ${t('residus_ajustes')}` }, showlegend: false, xaxis: { title: { text: t('valeurs_ajustees') } }, yaxis: { title: { text: t('code_residus') } },
      shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: 0, y1: 0, line: { color: c.doux, width: 1, dash: 'dot' } }] });
    if (uid.length) brancherSelection(g1, ctx, m.id);
  }
  if (graphes.includes(g2) && m.qq) {
    const q = m.qq;
    const tx = liste(q.theorique);
    const lim = etendue(tx);
    tracer(ctx, g2, [{ type: 'scatter', mode: 'markers', x: tx, y: liste(q.residu), marker: { size: 6, color: PALETTE[0], opacity: 0.8 }, hovertemplate: '%{x:.3f} · %{y:.4g}<extra></extra>' },
      { type: 'scatter', mode: 'lines', x: lim, y: lim.map((v) => q.ordonnee + q.pente * v), line: { color: c.doux, width: 1, dash: 'dot' }, hoverinfo: 'skip' }],
    { title: { text: `${m.nom} · ${t('residus_qq')}` }, showlegend: false, xaxis: { title: { text: t('quantiles_normaux') } }, yaxis: { title: { text: t('code_residus') } } });
  }
}

// ------------------------------------------------------------ the code's own plots, values, tables, code

function rendreGraphiquesCode(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const images = liste(m.images);
  conteneur.replaceChildren(el('div', { class: 'defile' }, images.length
    ? images.map((src, i) => el('figure', { class: 'image-code' }, el('img', { src, alt: t('code_graphique', { n: i + 1 }) }), el('figcaption', { class: 'note' }, t('code_graphique', { n: i + 1 }))))
    : el('p', { class: 'note' }, t('code_sans_graphique'))));
}

function rendreTableauxCode(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const v = m.valeurs;
  const tabs = liste(m.tableaux);
  const zone = el('div', { class: 'defile resume' });
  conteneur.replaceChildren(zone);
  loc.tables ??= {};
  if (v && liste(v.nom).length) {
    const noms = liste(v.nom);
    const vals = liste(v.valeur);
    zone.append(el('h4', {}, t('code_valeurs')), el('p', { class: 'note' }, t('code_valeurs_note', { modele: m.nom, nom: noms[0] })),
      el('table', { class: 'tableau compact' }, el('thead', {}, el('tr', {}, el('th', {}, t('nom')), el('th', { class: 'num' }, t('code_valeur')))),
        el('tbody', {}, noms.map((n, i) => el('tr', {}, el('td', {}, n), el('td', { class: 'num' }, nombre(vals[i], 4)))))));
  }
  for (const [k, x] of tabs.entries()) {
    const cols = liste(x.colonnes);
    const vals = liste(x.valeurs).map(liste);
    const bas = el('div', { class: 'bas tableau-code' });
    // (a table shown whole: no note, rather than append writing null as the text "null")
    zone.append(...[el('h4', {}, x.nom), x.n > vals[0]?.length ? el('p', { class: 'note' }, t('code_tableau_tronque', { n: vals[0].length, total: x.n })) : null, bas].filter(Boolean));
    loc.tables[k] ??= {};
    tableau(bas, ctx, loc.tables[k], {
      n: vals.length ? vals[0].length : 0, nom: x.nom,
      colonnes: cols.map((cn, j) => ({ cle: String(j), titre: cn, num: typeof vals[j].find((y) => y !== null) === 'number', decimales: 4 })),
      valeur: (i, j) => vals[Number(j)][i],
    });
  }
  if (!zone.children.length) zone.append(el('p', { class: 'note' }, t('code_sans_tableau')));
}

function rendreSourceCode(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  conteneur.replaceChildren(el('div', { class: 'defile resume' },
    el('div', { class: 'ligne' }, el('button', { onclick: () => modifierCode(m) }, icone('square-pen'), t('code_modifier')),
      el('span', { class: 'infos' }, `R ${m.R}${paquetsTexte(m) ? ` · ${paquetsTexte(m)}` : ''}`)),
    codeNumerote(m.code)));
}

// A code shown with its line numbers (read only).
export const codeNumerote = (code) => el('pre', { class: 'code-r' }, String(code || '').replace(/\n$/, '').split('\n').map((l) => el('span', { class: 'ligne-code' }, l || ' ')));

// Edit the code: New model with this model's settings and code; fitted, it takes the place of this
// fit (its page and views), under the same name (app.js: modifierModele, ajusterDefinition's remplace).
function modifierCode(m) {
  if (appli) appli.modifierModele(m.id);
}

// ------------------------------------------------------------ the report

const blocsResume = (m) => {
  const s = m.statistiques || {};
  return [{ texte: [`${t('code_classe')} ${liste(m.classe)[0] || '—'}`, `N ${nObservations(m)}`, s.logLik !== undefined ? `logLik ${nombre(s.logLik, 2)} (${s.ddl} ${t('ddl')})` : '',
    s.AIC !== undefined ? `AIC ${nombre(s.AIC, 1)}` : '', s.BIC !== undefined ? `BIC ${nombre(s.BIC, 1)}` : ''].filter(Boolean).join(' · ') },
  { texte: `R ${m.R}${paquetsTexte(m) ? ` · ${paquetsTexte(m)}` : ''}` }];
};
const blocsCoefficients = (m) => {
  const l = lignesCoefficients(m);
  return l.length ? [{ entetes: [t('terme'), t('code_estimation'), t('code_erreur_type'), t('code_statistique'), 'p', t('ic_95')],
    lignes: l.map((x) => [x.terme, nombre(x.estimation, 4), nombre(x.erreur_type, 4), nombre(x.statistique, 3), x.p === null || x.p === undefined ? '—' : valeurP(x.p),
      x.bas === null || x.bas === undefined ? '—' : `${fmt(x.bas, 4)} – ${fmt(x.haut, 4)}`]) }] : [{ texte: t('code_sans_coefficients') }];
};
const blocsTableaux = (m) => {
  const b = [];
  if (m.valeurs && liste(m.valeurs.nom).length) b.push({ entetes: [t('nom'), t('code_valeur')], lignes: liste(m.valeurs.nom).map((n, i) => [n, nombre(liste(m.valeurs.valeur)[i], 4)]) });
  for (const x of liste(m.tableaux)) {
    const vals = liste(x.valeurs).map(liste);
    b.push({ texte: x.nom }, { entetes: liste(x.colonnes), lignes: (vals[0] || []).slice(0, 200).map((_, i) => vals.map((c) => (typeof c[i] === 'number' ? fmt(c[i], 4) : c[i] ?? ''))) });
  }
  return b;
};
PARTIES_TYPES.CODE = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'code_coefficients', vue: 'code_coefficients', defaut: true, tableau: blocsCoefficients },
  { cle: 'code_diagnostics', vue: 'code_diagnostics', defaut: true, si: (m) => !!(m.ajustees && liste(m.ajustees.ajuste).length),
    variantes: async () => [{ mode: 'ajustes' }, { mode: 'qq' }] },
  { cle: 'code_tableaux', defaut: true, si: (m) => !!((m.valeurs && liste(m.valeurs.nom).length) || liste(m.tableaux).length), tableau: blocsTableaux },
];

// ------------------------------------------------------------ registration

const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estCode(m) ? rendreResumeCode(c, ctx, m) : resume(c, ctx, p, loc);
} };
Object.assign(VUES, {
  code_coefficients: { rendre: rendreCoefficientsCode, titre: (m) => `${m.nom} · ${t('coefficients')}` },
  code_diagnostics: { rendre: rendreDiagnosticsCode, titre: (m) => `${m.nom} · ${t('code_diagnostics')}` },
  code_graphiques: { rendre: rendreGraphiquesCode, titre: (m) => `${m.nom} · ${t('code_graphiques')}` },
  code_tableaux: { rendre: rendreTableauxCode, titre: (m) => `${m.nom} · ${t('code_tableaux')}` },
  code_source: { rendre: rendreSourceCode, titre: (m) => `${m.nom} · ${t('code_source')}` },
});
Object.assign(ICONES_VUES, { code_coefficients: 'chart-bar', code_diagnostics: 'chart-scatter', code_graphiques: 'image-down', code_tableaux: 'table-2', code_source: 'file-code' });
VUES_TYPES.CODE = (m) => ['resume', 'code_coefficients', 'code_diagnostics'].concat(liste(m.images).length ? ['code_graphiques'] : [],
  (m.valeurs && liste(m.valeurs.nom).length) || liste(m.tableaux).length ? ['code_tableaux'] : [], ['code_source'], m.prediction ? ['predictions'] : []);

// ------------------------------------------------------------ code written elsewhere

// The R-code models of each open project whose code isn't approved on this computer (the engine says
// so in each project's summary: serveur.R, resume_modele), and the bars that say it, with Show the
// code and Run it; × hides a project's bar until its code changes.
const caches = new Set();
const enAttente = (p) => liste(p.modeles).filter((d) => d.type === 'CODE' && d.code_r && d.code_r.approuve === false);
function barresCode(app) {
  let zone = document.getElementById('barres-code');
  const entete = document.getElementById('entete');
  if (!zone && entete) { zone = el('div', { id: 'barres-code' }); entete.append(zone); }
  if (!zone) return;
  const barres = app.projets().map((p) => {
    const d = enAttente(p);
    const cle = `${p.id}|${d.map((x) => x.code_r.empreinte).join(',')}`;
    if (!d.length || caches.has(cle)) return null;
    return el('div', { class: 'barre-code', role: 'status' }, icone('lock'),
      el('span', {}, t(app.projets().length > 1 ? 'code_barre_projet' : 'code_barre', { nom: p.nom, modeles: d.map((x) => x.nom).join(', ') })),
      el('button', { onclick: () => montrerCode(app, p.id) }, icone('file-code'), t('code_montrer')),
      el('button', { class: 'principal', onclick: () => executerCode(app, p.id) }, t('code_executer')),
      el('button', { class: 'icone-seule', title: t('code_barre_cacher'), 'aria-label': t('code_barre_cacher'), onclick: () => { caches.add(cle); barresCode(app); } }, icone('x')));
  }).filter(Boolean);
  zone.replaceChildren(...barres);
}

// The code of the project's models, as it is, before running it.
async function montrerCode(app, pid) {
  const r = await demander('code_modeles', { projet: pid });
  const modeles = liste(r.modeles).filter((x) => !x.approuve);
  // (the keyboard on the code, not on Run it: a second Enter doesn't run what wasn't read)
  dialogue({ titre: `${t('code_montrer')} — ${(app.projet(pid) || {}).nom || ''}`, large: true, focus: 'corps',
    contenu: el('div', { class: 'montrer-code' }, el('p', { class: 'note' }, t('code_montrer_note')),
      modeles.map((x) => el('section', {}, el('h4', {}, x.nom, x.titre ? el('span', { class: 'doux' }, ` — ${x.titre}`) : null), codeNumerote(x.code)))),
    boutons: [{ libelle: t('fermer'), action: () => true }, { libelle: t('code_executer'), principal: true, action: () => { executerCode(app, pid); return true; } }] });
}

// Run it: the code of the project's models approved, then each one fitted, its views opened.
async function executerCode(app, pid) {
  const p = app.projet(pid);
  if (!p) return;
  const d = enAttente(p);
  try {
    await approuver(d.map((x) => x.code_r.empreinte));
    for (const x of d) await app.calculer(pid, x.numero);
  } catch (e) {
    app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true);
  }
  try { app.majProjet(await demander('projet', { projet: pid })); } catch { /* the project closed meanwhile */ }
  barresCode(app);
}

export function installerCode(app) {
  appli = app;
  Object.assign(app, { barresCode: () => barresCode(app), montrerCode: (pid) => montrerCode(app, pid), executerCode: (pid) => executerCode(app, pid) });
}

ajouterTextes({
  type_code: 'Code R (votre propre modèle)', code_badge: 'R',
  code_note: 'Votre propre modèle, écrit en R : tout ce que R sait ajuster (lm, glm, nls, mgcv, nlme, glmmTMB, unmarked, brms…), avec les paquets installés pour le R que Nuée utilise. Nuée lui donne les données et en tire le résumé, les coefficients, la vraisemblance, l’AIC, les résidus, et l’exporte vers R tel quel.',
  code_r: 'Code R', code_requis: 'Écrivez le code R du modèle : sa dernière valeur est le modèle ajusté.',
  code_placeholder: 'fit <- lm(y ~ x, data = donnees)\nfit',
  code_aide_titre: 'Comment écrire le code',
  code_aide_donnees: '`donnees` : un data.frame des observations du modèle (celles de l’onglet Observations), une colonne par variable du projet sous son nom (les variables qualitatives en facteurs, leurs catégories dans l’ordre du projet), puis les identifiants des observations, en texte. Le code s’exécute dans un environnement à lui ; library() et paquet:: prennent les paquets installés pour ce R.',
  code_aide_valeur: 'Sa dernière valeur est le modèle ajusté (n’importe quel objet R) : Nuée en tire print(summary()), les coefficients (coef(summary()), sinon coef() et vcov()), nobs(), logLik(), AIC(), BIC(), fitted() et residuals(), et predict() quand il marche. Ou une liste dont Nuée reconnaît ces éléments :',
  code_aide_liste: 'list(modele = fit,\n     valeurs = c(densite = 3.2, pente = -0.4),   # des nombres nommés : {{M1.valeurs.densite}} dans un document\n     tableaux = list(effectifs = d))              # des data.frame nommés, montrés et exportés',
  code_aide_sortie: 'Ce que le code affiche ou dessine est gardé (une valeur visible, comme dans la console : un graphique ggplot ou lattice aussi), avec ses messages et ses avertissements ; une erreur donne le message de R et sa ligne. Ctrl+Entrée ajuste.',
  code_aide_colonnes: 'Les {n} colonnes de donnees :', code_facteur: 'facteur',
  code_inserer_exemple: 'Mettre l’exemple', code_exemple_commentaire: '# donnees : les observations du modèle, une colonne par variable',
  code_approbation_echec: 'Le code n’a pas pu être approuvé : le moteur R ne répond pas.',
  code_classe: 'Classe', code_paquets: 'Paquets', code_modifier: 'Modifier le code', code_source: 'Le code',
  code_observations_laissees: 'L’ajustement a laissé de côté {n} des {total} observations que Nuée lui a données (des valeurs manquantes, le plus souvent).',
  code_avertissements: 'Avertissements de R ({n})', code_autres: 'Éléments de la liste que Nuée ne reconnaît pas (seuls modele, valeurs et tableaux le sont) : {noms}.',
  code_comparaison_oui: 'Comparé par l’AIC (Prédiction › Comparer les modèles) aux régressions et aux modèles de code de la même réponse sur les mêmes observations.',
  code_comparaison_non: 'Pas comparé par l’AIC aux autres modèles',
  code_resume_texte: 'summary()', code_sans_modele: 'La liste du code n’a pas de modele : seulement ses valeurs et ses tableaux.',
  code_sortie: 'Ce que le code a affiché ({n} lignes)', code_messages: 'Messages ({n})',
  code_sans_coefficients: 'Ce modèle ne donne pas de coefficients (ni coef(summary()), ni coef()).',
  code_coefficients: 'Coefficients', code_n_observations: '{n} observations',
  code_coefficients_note: 'coef(summary()) du modèle quand il a une colonne d’erreurs-types, sinon coef() et vcov() ; intervalle à 95 % (loi t à ses ddl quand son test est un t, sinon normale) ; plein et en rouge, p ≤ 0,05 ; l’ordonnée à l’origine dans le tableau seulement.',
  code_estimation: 'Estimation', code_erreur_type: 'Erreur-type', code_statistique: 'Statistique', code_ic_bas: 'IC 95 % bas', code_ic_haut: 'IC 95 % haut',
  code_diagnostics: 'Résidus', code_deux_graphes: 'Les deux', code_residus: 'Résidus', code_residus_note: 'residuals() contre fitted() du modèle, et leur droite de Henry.',
  code_sans_residus: 'Ce modèle ne donne pas de valeurs ajustées et de résidus (fitted(), residuals()).',
  code_residus_sans_lien: 'Nuée ne sait pas à quelle observation revient chaque point (ni sélection ni exclusion) : les noms de fitted() ne sont pas les numéros des lignes de donnees, ou le code a ajusté le modèle sur d’autres données.',
  code_graphiques: 'Graphiques du code', code_graphique: 'Graphique {n}', code_sans_graphique: 'Le code n’a dessiné aucun graphique.',
  code_tableaux: 'Valeurs et tableaux', code_valeurs: 'Valeurs', code_valeur: 'Valeur',
  code_valeurs_note: 'Un document les cite : {{{modele}.valeurs.{nom}}}.', code_tableau_tronque: 'Les {n} premières lignes sur {total} (l’export les a toutes).',
  code_sans_tableau: 'Le code ne donne ni valeurs ni tableaux (la forme liste : list(modele = fit, valeurs = …, tableaux = …)).',
  code_barre: 'Ce projet a du code R écrit par quelqu’un d’autre ({modeles}) : il ne s’exécute pas tant que vous ne l’avez pas lu et approuvé.',
  code_barre_projet: '{nom} a du code R écrit par quelqu’un d’autre ({modeles}) : il ne s’exécute pas tant que vous ne l’avez pas lu et approuvé.',
  code_montrer: 'Montrer le code', code_executer: 'Exécuter', code_barre_cacher: 'Cacher (les modèles restent non ajustés)',
  code_montrer_note: 'Ce code vient du fichier du projet : il a été écrit ou changé ailleurs. Exécuter le lance avec vos droits, comme du code tapé dans la console R ; ne l’exécutez que si vous lui faites confiance.',
  comparaison_hors_aic: '{nom}, hors des ensembles AIC : {raison}',
  code_attente: 'Le code R de {nom} vient d’ailleurs : il attend d’être lu et approuvé (Montrer le code, Exécuter, dans la barre au-dessus du projet).',
  code_non_execute: 'code venu d’ailleurs, pas exécuté', code_non_execute_aide: 'Son code R a été écrit ou changé ailleurs : il ne s’exécute pas tant que vous ne l’avez pas lu et approuvé. Double-clic : le montrer.',
}, {
  type_code: 'R code (your own model)', code_badge: 'R',
  code_note: 'Your own model, written in R: anything R can fit (lm, glm, nls, mgcv, nlme, glmmTMB, unmarked, brms…), with the packages installed for the R Nuée uses. Nuée gives it the data and takes from it the summary, the coefficients, the likelihood, the AIC, the residuals, and exports it to R as it is.',
  code_r: 'R code', code_requis: 'Write the model’s R code: its last value is the fitted model.',
  code_placeholder: 'fit <- lm(y ~ x, data = donnees)\nfit',
  code_aide_titre: 'How to write the code',
  code_aide_donnees: '`donnees`: a data.frame of the model’s observations (those of the Observations tab), one column per variable of the project under its name (qualitative variables as factors, their categories in the project’s order), then the observations’ IDs, as text. The code runs in an environment of its own; library() and package:: take the packages installed for this R.',
  code_aide_valeur: 'Its last value is the fitted model (any R object): Nuée takes print(summary()), the coefficients (coef(summary()), else coef() and vcov()), nobs(), logLik(), AIC(), BIC(), fitted() and residuals(), and predict() when it works. Or a list whose elements Nuée recognises:',
  code_aide_liste: 'list(modele = fit,\n     valeurs = c(density = 3.2, slope = -0.4),   # named numbers: {{M1.valeurs.density}} in a document\n     tableaux = list(counts = d))                 # named data.frames, shown and exported',
  code_aide_sortie: 'What the code prints or draws is kept (a visible value, as in the console: a ggplot or lattice plot too), with its messages and warnings; an error gives R’s message and its line. Ctrl+Enter fits.',
  code_aide_colonnes: 'The {n} columns of donnees:', code_facteur: 'factor',
  code_inserer_exemple: 'Insert the example', code_exemple_commentaire: '# donnees: the model’s observations, one column per variable',
  code_approbation_echec: 'The code couldn’t be approved: the R engine doesn’t answer.',
  code_classe: 'Class', code_paquets: 'Packages', code_modifier: 'Edit the code', code_source: 'The code',
  code_observations_laissees: 'The fit left out {n} of the {total} observations Nuée gave it (missing values, most often).',
  code_avertissements: 'R warnings ({n})', code_autres: 'Elements of the list Nuée doesn’t recognise (only modele, valeurs and tableaux): {noms}.',
  code_comparaison_oui: 'Compared by AIC (Prediction › Compare models) with the regressions and code models of the same response on the same observations.',
  code_comparaison_non: 'Not compared by AIC with other models',
  code_resume_texte: 'summary()', code_sans_modele: 'The code’s list has no modele: only its values and tables.',
  code_sortie: 'What the code printed ({n} lines)', code_messages: 'Messages ({n})',
  code_sans_coefficients: 'This model gives no coefficients (neither coef(summary()) nor coef()).',
  code_coefficients: 'Coefficients', code_n_observations: '{n} observations',
  code_coefficients_note: 'The model’s coef(summary()) when it has a column of standard errors, else coef() and vcov(); 95% interval (t at its df when its test is a t, else normal); filled and red, p ≤ 0.05; the intercept in the table only.',
  code_estimation: 'Estimate', code_erreur_type: 'Standard error', code_statistique: 'Statistic', code_ic_bas: '95% CI low', code_ic_haut: '95% CI high',
  code_diagnostics: 'Residuals', code_deux_graphes: 'Both', code_residus: 'Residuals', code_residus_note: 'The model’s residuals() against fitted(), and their normal QQ plot.',
  code_sans_residus: 'This model gives no fitted values and residuals (fitted(), residuals()).',
  code_residus_sans_lien: 'Nuée doesn’t know which observation each point belongs to (no selection, no exclusion): the names of fitted() aren’t the row numbers of donnees, or the code fitted the model on other data.',
  code_graphiques: 'Plots of the code', code_graphique: 'Plot {n}', code_sans_graphique: 'The code drew no plot.',
  code_tableaux: 'Values and tables', code_valeurs: 'Values', code_valeur: 'Value',
  code_valeurs_note: 'A document cites them: {{{modele}.valeurs.{nom}}}.', code_tableau_tronque: 'The first {n} rows of {total} (the export has them all).',
  code_sans_tableau: 'The code gives no values or tables (the list form: list(modele = fit, valeurs = …, tableaux = …)).',
  code_barre: 'This project has R code written by someone else ({modeles}): it doesn’t run until you have read and approved it.',
  code_barre_projet: '{nom} has R code written by someone else ({modeles}): it doesn’t run until you have read and approved it.',
  code_montrer: 'Show the code', code_executer: 'Run it', code_barre_cacher: 'Hide (the models stay not fitted)',
  code_montrer_note: 'This code comes from the project’s file: it was written or changed elsewhere. Run it starts it with your rights, like code typed in the R console; run it only if you trust it.',
  comparaison_hors_aic: '{nom}, outside the AIC sets: {raison}',
  code_attente: 'The R code of {nom} comes from elsewhere: it waits to be read and approved (Show the code, Run it, in the bar above the project).',
  code_non_execute: 'code from elsewhere, not run', code_non_execute_aide: 'Its R code was written or changed elsewhere: it doesn’t run until you have read and approved it. Double-click: show it.',
});
