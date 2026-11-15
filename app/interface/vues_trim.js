// TRIM, trends and indices of monitoring counts (moteur/R/trim.R): its settings in the model
// dialog, its views (the summary with the overall slope and its class, the indices with their
// band, the time totals, the parameters and tests, each site's counts against the model's), its
// report parts and its strings. Imported by app.js.
import { t, ajouterTextes } from './i18n.js';
import { el, fmt } from './ui.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, tableau, notesModele, valeurP, ficheCondition } from './panneaux.js';
import { ANALYSES } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const estTrim = (m) => !!m && !!m.analyse && m.type === 'TRIM';
const Z95 = 1.959964;
const chiffre = (k, v, d = 3) => el('span', {}, el('small', {}, k), ' ', el('b', {}, typeof v === 'number' ? fmt(v, d) : v ?? '—'));
const pValeur = (v) => (v === null || v === undefined ? '—' : valeurP(v));
const nombre = (v, d = 3) => (v === null || v === undefined || !Number.isFinite(v) ? '—' : fmt(v, d));
const temps = (v) => (v === null || v === undefined ? '' : String(+Number(v).toPrecision(12)));
const caseACocher = (libelle, valeur, changer, titre) => el('label', { class: 'champ', title: titre || '' },
  el('input', { type: 'checkbox', checked: valeur, onchange: (e) => changer(e.target.checked) }), libelle);
// the rows of a data.frame sent by R (columns), as arrays of values
const lignesDe = (df, cles) => { const n = liste((df || {})[cles[0]]).length; return Array.from({ length: n }, (_, i) => cles.map((k) => liste(df[k])[i])); };
function tableCompacte(entetes, lignes, alerte) {
  return el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, entetes.map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
    el('tbody', {}, lignes.map((l, i) => el('tr', {}, l.map((v, j) => el('td', { class: (j ? 'num' : '') + (alerte && alerte(i, j) ? ' alerte' : '') }, v))))));
}
const translucide = (couleur, a) => `rgba(${[1, 3, 5].map((i) => parseInt(couleur.slice(i, i + 2), 16)).join(', ')}, ${a})`;
// the class of an overall slope in words, and its significance (p < 0.01 or p < 0.05)
const classe = (pente) => `${t('trim_classe_' + pente.classe)}${pente.niveau ? ` (p < ${fmt(pente.niveau, 2)})` : ''}`;
const pentes = (m) => lignesDe(m.pentes, ['quoi', 'multiplicative', 'se_multiplicative', 'bas', 'haut', 'p', 'classe', 'niveau'])
  .map((l) => ({ quoi: l[0], mult: l[1], se: l[2], bas: l[3], haut: l[4], p: l[5], classe: l[6], niveau: l[7] }));
const libelleModele = (m) => t('trim_modele_' + m.modele_trim);
const changementsTexte = (m) => liste(m.changements).map(temps).join(', ');
// the model's notes: what was left out, added or removed, and the warnings of rtrim
function notesTrim(m) {
  const n = [];
  if (liste(m.sites_vides).length) n.push(t('trim_sites_vides', { n: liste(m.sites_vides).length, noms: liste(m.sites_vides).slice(0, 12).join(', ') + (liste(m.sites_vides).length > 12 ? '…' : '') }));
  if (liste(m.temps_ajoutes).length) n.push(t('trim_temps_ajoutes', { temps: liste(m.temps_ajoutes).map(temps).join(', ') }));
  if (liste(m.changements_retires).length) n.push(t('trim_changements_retires', { temps: liste(m.changements_retires).map(temps).join(', ') }));
  if (liste(m.covariables_retirees).length) n.push(t('trim_covariables_retirees', { noms: liste(m.covariables_retirees).join(', ') }));
  if (m.sans_site_temps) n.push(t('trim_sans_site_temps', { n: m.sans_site_temps }));
  if (m.correlation && m.rho !== null && m.rho < 0.2) n.push(t(m.rho < 0 ? 'trim_rho_negatif' : 'trim_rho_faible', { rho: fmt(m.rho, 3) }));
  if (m.surdispersion && m.sigma2 !== null && m.sigma2 < 1) n.push(t('trim_sigma2_faible', { s: fmt(m.sigma2, 2) }));
  return n;
}

// ------------------------------------------------------------ summary

function rendreResumeTrim(conteneur, ctx, m) {
  const [pi, pm] = pentes(m);
  const titre = el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, 'TRIM'), m.titre ? el('span', { class: 'doux' }, m.titre) : null);
  const tp = liste(m.temps);
  const fiche = el('div', { class: 'fiche' }, titre,
    el('div', { class: 'fiche-chiffres' }, chiffre(t('trim_modele'), libelleModele(m)), chiffre(t('trim_n_sites'), m.I, 0),
      chiffre(t('trim_temps'), `${temps(tp[0])}–${temps(tp[tp.length - 1])} (${m.J})`), chiffre(t('trim_comptages'), `${m.O} / ${m.I * m.J}`)),
    el('div', { class: 'fiche-chiffres' }, chiffre(t('trim_methode'), m.methode === 'GEE' ? 'GEE' : t('trim_ml')),
      m.surdispersion ? chiffre('σ²', m.sigma2, 3) : null, m.correlation ? chiffre('ρ', m.rho, 3) : null,
      m.modele_trim === 2 ? chiffre(t('trim_changements'), changementsTexte(m) || '—') : null,
      liste(m.covariables).length ? chiffre(t('trim_covariables'), liste(m.covariables).join(', ')) : null,
      m.noms && m.noms.poids ? chiffre(t('trim_poids'), m.noms.poids) : null),
    ficheCondition(m));
  const phrase = (p, cle) => (p.mult === null || p.mult === undefined ? t('trim_pente_inconnue')
    : t(cle, { m: fmt(p.mult, 4), se: fmt(p.se, 4), bas: fmt(p.bas, 4), haut: fmt(p.haut, 4), pct: `${p.mult >= 1 ? '+' : '−'}${fmt(Math.abs(100 * (p.mult - 1)), 1)}`, classe: classe(p) }));
  const pente = el('div', { class: 'fiche' },
    el('div', { class: 'fiche-titre' }, el('b', {}, t('trim_pente_globale')), el('span', { class: 'badge' }, classe(pi))),
    el('p', {}, phrase(pi, 'trim_phrase_pente')),
    el('p', { class: 'note' }, phrase(pm, 'trim_phrase_pente_modele')));
  const a = m.ajustement;
  const notes = notesModele(conteneur, ctx, m);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche, pente, notes.champ,
    tableCompacte(['', t('trim_valeur'), t('ddl'), 'p'], [[t('trim_chi2'), fmt(a.chi2, 2), String(a.ddl), pValeur(a.p_chi2)],
      [t('trim_lr'), fmt(a.LR, 2), String(a.ddl), pValeur(a.p_LR)], ['AIC', fmt(a.AIC, 2), '', '']]),
    el('p', { class: 'note' }, t(m.methode === 'GEE' ? 'trim_ajustement_gee' : 'trim_ajustement_ml')),
    notesTrim(m).map((x) => el('p', { class: 'note' }, x)),
    el('p', { class: 'note' }, t('trim_resume_note'))));
  notes.retrouver();
}

// ------------------------------------------------------------ the indices

// The imputed indices (the observed counts, and the model's for the missing ones) with their 95 %
// band (± 1.96 standard errors), the model's indices, the base time marked, and the line of the
// overall slope through the log of the imputed totals.
function rendreIndices(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.modele ??= true;
  loc.bande ??= true;
  loc.droite ??= false;
  const ix = m.indices;
  const x = liste(ix.temps);
  const imp = liste(ix.impute);
  const se = liste(ix.se_impute);
  const mod = liste(ix.modele);
  const sem = liste(ix.se_modele);
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(outils(caseACocher(t('trim_indices_modele'), loc.modele, (v) => { loc.modele = v; ctx.rafraichir(); }),
    caseACocher(t('trim_bande'), loc.bande, (v) => { loc.bande = v; ctx.rafraichir(); }, t('trim_bande_aide')),
    caseACocher(t('trim_droite'), loc.droite, (v) => { loc.droite = v; ctx.rafraichir(); }, t('trim_droite_aide'))), graphe, bas);
  const c = couleursTheme(ctx);
  const traces = [];
  const bande = (y, s, couleur, nom) => ({ type: 'scatter', mode: 'lines', x: x.concat([...x].reverse()), y: y.map((v, i) => v - Z95 * s[i]).concat(y.map((v, i) => v + Z95 * s[i]).reverse()),
    fill: 'toself', fillcolor: translucide(couleur, 0.16), line: { width: 0 }, hoverinfo: 'skip', name: nom, showlegend: false });
  if (loc.bande) traces.push(bande(imp, se, PALETTE[0], t('trim_bande')));
  if (loc.modele && loc.bande) traces.push(bande(mod, sem, PALETTE[1], t('trim_bande')));
  traces.push({ type: 'scatter', mode: 'lines+markers', name: t('trim_indices_imputes'), x, y: imp, customdata: se, line: { color: PALETTE[0], width: 2 }, marker: { size: 6, color: PALETTE[0] },
    hovertemplate: `<b>%{x}</b><br>${t('trim_indice')} %{y:.3f} ± %{customdata:.3f}<extra>${t('trim_indices_imputes')}</extra>` });
  if (loc.modele) {
    traces.push({ type: 'scatter', mode: 'lines+markers', name: t('trim_indices_modele'), x, y: mod, customdata: sem, line: { color: PALETTE[1], width: 1.5, dash: 'dash' }, marker: { size: 5, color: PALETTE[1] },
      hovertemplate: `<b>%{x}</b><br>${t('trim_indice')} %{y:.3f} ± %{customdata:.3f}<extra>${t('trim_indices_modele')}</extra>` });
  }
  if (loc.droite) {
    // (least squares through ln(imputed totals) against the time's rank, divided by the base's total)
    const tt = liste(m.totaux.impute);
    if (tt.every((v) => v > 0)) {
      const j = tt.map((_, i) => i + 1);
      const mj = j.reduce((a, b) => a + b, 0) / j.length;
      const y = tt.map(Math.log);
      const my = y.reduce((a, b) => a + b, 0) / y.length;
      const b1 = j.reduce((s, v, i) => s + (v - mj) * (y[i] - my), 0) / j.reduce((s, v) => s + (v - mj) ** 2, 0);
      const b0 = my - b1 * mj;
      const base = tt[x.findIndex((v) => v === m.base)] || tt[0];
      traces.push({ type: 'scatter', mode: 'lines', name: t('trim_droite'), x, y: j.map((v) => Math.exp(b0 + b1 * v) / base), line: { color: c.doux, width: 1.5, dash: 'dot' }, hoverinfo: 'skip' });
    }
  }
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${t('trim_indices')} (${t('trim_base')} ${temps(m.base)})` }, dragmode: 'zoom',
    xaxis: { title: { text: m.noms ? m.noms.temps : t('trim_temps') } }, yaxis: { title: { text: t('trim_indice') }, rangemode: 'tozero' },
    shapes: [{ type: 'line', xref: 'paper', x0: 0, x1: 1, y0: 1, y1: 1, line: { color: c.doux, width: 1, dash: 'dot' } },
      { type: 'line', xref: 'x', yref: 'paper', x0: m.base, x1: m.base, y0: 0, y1: 1, line: { color: c.doux, width: 1, dash: 'dot' } }],
    annotations: [{ x: m.base, y: 1, xref: 'x', yref: 'paper', text: t('trim_base'), showarrow: false, yanchor: 'bottom', font: { size: 10, color: c.doux } }] });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: x.length, nom: 'Indices',
    colonnes: [{ cle: 'temps', titre: m.noms ? m.noms.temps : t('trim_temps') }, { cle: 'impute', titre: t('trim_indices_imputes'), num: true, decimales: 4 },
      { cle: 'se_impute', titre: t('trim_se'), num: true, decimales: 4 }, { cle: 'modele', titre: t('trim_indices_modele'), num: true, decimales: 4 },
      { cle: 'se_modele', titre: t('trim_se'), num: true, decimales: 4 }],
    valeur: (i, k) => (k === 'temps' ? temps(x[i]) : liste(ix[k])[i]),
  });
}

// ------------------------------------------------------------ the time totals

function rendreTotaux(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.bande ??= true;
  const tt = m.totaux;
  const x = liste(tt.temps);
  const imp = liste(tt.impute);
  const se = liste(tt.se_impute);
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(outils(caseACocher(t('trim_bande'), loc.bande, (v) => { loc.bande = v; ctx.rafraichir(); }, t('trim_bande_aide'))),
    el('p', { class: 'note' }, t('trim_totaux_note')), graphe, bas);
  const traces = [{ type: 'bar', name: t('trim_totaux_observes'), x, y: liste(tt.observe), marker: { color: translucide(PALETTE[2], 0.55) },
    hovertemplate: `<b>%{x}</b><br>%{y:.1f}<extra>${t('trim_totaux_observes')}</extra>` }];
  if (loc.bande) {
    traces.push({ type: 'scatter', mode: 'lines', x: x.concat([...x].reverse()), y: imp.map((v, i) => v - Z95 * se[i]).concat(imp.map((v, i) => v + Z95 * se[i]).reverse()),
      fill: 'toself', fillcolor: translucide(PALETTE[0], 0.16), line: { width: 0 }, hoverinfo: 'skip', name: t('trim_bande'), showlegend: false });
  }
  traces.push({ type: 'scatter', mode: 'lines+markers', name: t('trim_totaux_imputes'), x, y: imp, customdata: se, line: { color: PALETTE[0], width: 2 }, marker: { size: 6, color: PALETTE[0] },
    hovertemplate: `<b>%{x}</b><br>%{y:.1f} ± %{customdata:.1f}<extra>${t('trim_totaux_imputes')}</extra>` });
  traces.push({ type: 'scatter', mode: 'lines', name: t('trim_totaux_modele'), x, y: liste(tt.modele), customdata: liste(tt.se_modele), line: { color: PALETTE[1], width: 1.5, dash: 'dash' },
    hovertemplate: `<b>%{x}</b><br>%{y:.1f} ± %{customdata:.1f}<extra>${t('trim_totaux_modele')}</extra>` });
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${t('trim_totaux')}` }, dragmode: 'zoom', barmode: 'overlay',
    xaxis: { title: { text: m.noms ? m.noms.temps : t('trim_temps') } }, yaxis: { title: { text: t('trim_total') }, rangemode: 'tozero' } });
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: x.length, nom: 'Totals',
    colonnes: [{ cle: 'temps', titre: m.noms ? m.noms.temps : t('trim_temps') }, { cle: 'observe', titre: t('trim_totaux_observes'), num: true, decimales: 1 },
      { cle: 'impute', titre: t('trim_totaux_imputes'), num: true, decimales: 1 }, { cle: 'se_impute', titre: t('trim_se'), num: true, decimales: 1 },
      { cle: 'modele', titre: t('trim_totaux_modele'), num: true, decimales: 1 }, { cle: 'se_modele', titre: t('trim_se'), num: true, decimales: 1 }],
    valeur: (i, k) => (k === 'temps' ? temps(x[i]) : liste(tt[k])[i]),
  });
}

// ------------------------------------------------------------ parameters and tests

const lignesPentes = (m) => pentes(m).map((p) => [t(p.quoi === 'imputee' ? 'trim_pente_imputee' : 'trim_pente_modele'), nombre(p.mult, 4), nombre(p.se, 4),
  p.bas === null ? '—' : `${fmt(p.bas, 4)} – ${fmt(p.haut, 4)}`, pValeur(p.p), classe(p)]);
const entetesPentes = () => ['', t('trim_multiplicative'), t('trim_se'), t('ic_95'), 'p', t('trim_classe')];
const libelleBloc = (cv, cat) => (cv === null || cv === undefined ? t('trim_reference') : `${cv} = ${cat}`);
function lignesParametres(m) {
  const pr = m.parametres;
  if (!pr) return null;
  const cles = m.modele_trim === 2 ? ['covariable', 'categorie', 'de', 'a', 'additif', 'se_additif', 'multiplicatif', 'se_multiplicatif']
    : ['covariable', 'categorie', 'temps', 'additif', 'se_additif', 'multiplicatif', 'se_multiplicatif'];
  return lignesDe(pr, cles).map((l) => (m.modele_trim === 2
    ? [libelleBloc(l[0], l[1]), `${temps(l[2])} – ${temps(l[3])}`, ...l.slice(4).map((v) => fmt(v, 4))]
    : [libelleBloc(l[0], l[1]), temps(l[2]), ...l.slice(3).map((v) => fmt(v, 4))]));
}
const entetesParametres = (m) => ['', m.modele_trim === 2 ? t('trim_morceau') : t('trim_temps'), t('trim_additif'), t('trim_se'), t('trim_multiplicatif'), t('trim_se')];
const libelleTest = (l) => (l[0] === 'pente' ? t('trim_test_pente') : l[0] === 'changement' ? t('trim_test_changement', { temps: l[1] })
  : l[0] === 'deviations' ? t('trim_test_ecarts') : t('trim_test_covariable', { nom: l[1] }));
const lignesTests = (m) => lignesDe(m.tests, ['test', 'quoi', 'W', 'ddl', 'p']).map((l) => [libelleTest(l), fmt(l[2], 3), String(l[3]), pValeur(l[4])]);

function rendreParametresTrim(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const alerteP = (rows, k) => (i, j) => j === k && rows[i] && rows[i][k] !== '—' && Number(liste(m.tests.p)[i]) <= 0.05;
  const tests = lignesTests(m);
  const etapes = m.pas_a_pas && m.pas_a_pas.etapes ? lignesDe(m.pas_a_pas.etapes, ['etape', 'action', 'temps', 'test', 'p']) : [];
  const ec = m.ecarts;
  conteneur.replaceChildren(el('div', { class: 'defile resume' },
    el('h4', {}, t('trim_pentes')), tableCompacte(entetesPentes(), lignesPentes(m)), el('p', { class: 'note' }, t('trim_pentes_note')),
    el('h4', {}, t('trim_parametres')), lignesParametres(m) ? tableCompacte(entetesParametres(m), lignesParametres(m)) : el('p', { class: 'note' }, t('trim_sans_parametres')),
    el('p', { class: 'note' }, t(m.modele_trim === 2 ? 'trim_parametres_note_2' : 'trim_parametres_note_3')),
    ec ? [el('h4', {}, t('trim_ecarts')), el('p', { class: 'note' }, t('trim_ecarts_note', { b: fmt(Math.exp(ec.pente), 4), se: fmt(Math.exp(ec.pente) * ec.se_pente, 4) })),
      tableCompacte([t('trim_temps'), t('trim_additif'), t('trim_se'), t('trim_multiplicatif'), t('trim_se')],
        lignesDe(ec.ecarts, ['temps', 'additif', 'se_additif', 'multiplicatif', 'se_multiplicatif']).map((l) => [temps(l[0]), ...l.slice(1).map((v) => fmt(v, 4))]))] : null,
    el('h4', {}, t('trim_tests')), tests.length ? tableCompacte([t('terme'), 'W', t('ddl'), 'p'], tests, alerteP(tests, 3)) : el('p', { class: 'note' }, t('trim_sans_tests')),
    el('p', { class: 'note' }, t('trim_tests_note')),
    etapes.length ? [el('h4', {}, t('trim_pas_a_pas')), el('p', { class: 'note' }, t('trim_pas_a_pas_note', { depart: liste(m.pas_a_pas.depart).map(temps).join(', '), gardes: liste(m.pas_a_pas.gardes).map(temps).join(', ') || '—' })),
      tableCompacte(['', t('trim_action'), t('trim_temps'), t('trim_test'), 'p'], etapes.map((l) => [String(l[0]), t('trim_action_' + l[1]), temps(l[2]), l[3] === 'wald' ? 'Wald' : t('trim_score'), pValeur(l[4])]))] : null));
}

// ------------------------------------------------------------ counts by site

// One site's counts (observed, and those the model imputes), against the model's expected counts;
// or every observed count against its expected count (log scales), a click selecting its row.
function rendreSitesTrim(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const sites = liste(m.sites.site);
  loc.mode ??= 'site';
  if (!sites.includes(loc.site)) loc.site = sites[0];
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(outils(
    choix('', [['site', t('trim_un_site')], ['tous', t('trim_tous_sites')]], loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    loc.mode === 'site' ? choix(t('trim_site'), sites.map((s) => [s, s]), loc.site, (v) => { loc.site = v; ctx.rafraichir(); }) : null), graphe, bas);
  const x = liste(m.temps);
  const nomTemps = m.noms ? m.noms.temps : t('trim_temps');
  if (loc.mode === 'site') {
    const i = sites.indexOf(loc.site);
    const f = liste(m.comptes[i]);
    const mu = liste(m.ajustes[i]);
    const vus = x.map((_, j) => j).filter((j) => f[j] !== null);
    const manquants = x.map((_, j) => j).filter((j) => f[j] === null);
    tracer(ctx, graphe, [
      { type: 'scatter', mode: 'lines', name: t('trim_attendus'), x, y: mu, line: { color: PALETTE[1], width: 1.5 }, hovertemplate: `<b>%{x}</b><br>%{y:.2f}<extra>${t('trim_attendus')}</extra>` },
      { type: 'scatter', mode: 'markers', name: t('trim_observes'), x: vus.map((j) => x[j]), y: vus.map((j) => f[j]), marker: { size: 8, color: PALETTE[0] },
        hovertemplate: `<b>%{x}</b><br>%{y}<extra>${t('trim_observes')}</extra>` },
      { type: 'scatter', mode: 'markers', name: t('trim_imputes'), x: manquants.map((j) => x[j]), y: manquants.map((j) => mu[j]),
        marker: { size: 8, symbol: 'circle-open', color: PALETTE[1], line: { width: 1.5 } }, hovertemplate: `<b>%{x}</b><br>%{y:.2f}<extra>${t('trim_imputes')}</extra>` }],
    { title: { text: `${m.nom} · ${t('trim_site')} ${loc.site}` }, dragmode: 'zoom', xaxis: { title: { text: nomTemps } }, yaxis: { title: { text: t('trim_comptage') }, rangemode: 'tozero' } });
  } else {
    const o = m.observations;
    const idx = liste(o.uid).map((_, k) => k).filter((k) => liste(o.compte)[k] !== null);
    const c = couleursTheme(ctx);
    const plus = (v) => v + 0.5;  // (log scales: a zero count shows)
    const yo = idx.map((k) => plus(liste(o.compte)[k]));
    const xo = idx.map((k) => plus(liste(o.ajuste)[k]));
    const lim = [Math.min(...xo, ...yo), Math.max(...xo, ...yo)];
    const sel = new Set(ctx.selection(m.id));
    tracer(ctx, graphe, [
      { type: 'scattergl', mode: 'markers', name: t('trim_observes'), x: xo, y: yo, customdata: idx.map((k) => liste(o.uid)[k]),
        text: idx.map((k) => `${liste(o.site)[k]} · ${temps(liste(o.temps)[k])}`),
        marker: { size: 6, color: idx.map((k) => (sel.has(liste(o.uid)[k]) ? '#e4572e' : PALETTE[0])), opacity: 0.7 },
        hovertemplate: '<b>%{text}</b><br>%{y:.0f} / %{x:.2f}<extra></extra>' },
      { type: 'scatter', mode: 'lines', x: lim, y: lim, line: { color: c.doux, width: 1, dash: 'dot' }, hoverinfo: 'skip', showlegend: false }],
    { title: { text: `${m.nom} · ${t('trim_observes_attendus')}` }, showlegend: false,
      xaxis: { type: 'log', title: { text: `${t('trim_attendus')} + 0,5` } }, yaxis: { type: 'log', title: { text: `${t('trim_observes')} + 0,5` } } });
    brancherSelection(graphe, ctx, m.id);
  }
  loc.table ??= {};
  const s = m.sites;
  tableau(bas, ctx, loc.table, {
    n: sites.length, nom: 'Sites', choisie: (i) => loc.mode === 'site' && sites[i] === loc.site, cliquer: (i) => { loc.site = sites[i]; loc.mode = 'site'; ctx.rafraichir(); },
    colonnes: [{ cle: 'site', titre: t('trim_site') }, { cle: 'observes', titre: t('trim_n_observes'), num: true, decimales: 0 }, { cle: 'positifs', titre: t('trim_n_positifs'), num: true, decimales: 0 },
      { cle: 'total', titre: t('trim_total_observe'), num: true, decimales: 0 }, { cle: 'total_ajuste', titre: t('trim_total_attendu'), num: true, decimales: 1 }],
    valeur: (i, k) => liste(s[k])[i],
  });
}

// ------------------------------------------------------------ the model dialog

const specCode = (s) => (!s ? '' : s.by === 'id' ? `id:${s.id}` : `var:${s.variable}`);
const specLue = (v) => (!v ? null : v.startsWith('id:') ? { by: 'id', id: v.slice(3) } : { by: 'variable', variable: Number(v.slice(4)) });
// the column a name points to, when the dialog opens for a new model (a site, a year)
function deviner(p, motif, quantitative) {
  const id = [].concat(p.identifiants || []).find((i) => motif.test(i.nom));
  if (id) return { by: 'id', id: id.nom };
  const v = p.variables.find((x) => motif.test(x.nom) && (quantitative === undefined || !x.qualitative === quantitative));
  return v ? { by: 'variable', variable: v.uid } : null;
}
const MOTIF_SITE = /^(site|station|point|transect|carr[ée]|route|parcelle|plot)/i;
const MOTIF_TEMPS = /^(ann[ée]e|year|an$|time|temps|saison|season)/i;
const lireTemps = (texte) => String(texte || '').split(/[\s,;]+/).filter(Boolean).map((x) => Number(x.replace(',', '.')));

function champsTrim(st, rendre, p) {
  const o = st.options;
  if (o.site === undefined) o.site = deviner(p, MOTIF_SITE);
  if (o.temps === undefined) o.temps = deviner(p, MOTIF_TEMPS, true) || deviner(p, MOTIF_TEMPS);
  if (o.changements_texte === undefined) {
    const cp = o.changements;
    o.tous = cp === 'tous' || (Array.isArray(cp) && cp[0] === 'tous');
    o.changements_texte = o.tous ? '' : liste(cp).map(temps).join(' ');
  }
  const ids = [].concat(p.identifiants || []).map((i) => i.nom);
  const options = (valeur, filtre, aucun) => [el('option', { value: '', selected: !valeur }, aucun || '—')]
    .concat(ids.length ? [el('optgroup', { label: t('vc_par_identifiant') }, ids.map((n) => el('option', { value: `id:${n}`, selected: `id:${n}` === valeur }, n)))] : [])
    .concat([el('optgroup', { label: t('vc_par_variable') }, p.variables.filter(filtre).map((v) => el('option', { value: `var:${v.uid}`, selected: `var:${v.uid}` === valeur }, v.nom)))]);
  const modele = Number(o.modele ?? 3);
  const oui = (k) => o[k] === undefined || o[k] === null || !!o[k];
  return [
    el('label', {}, t('trim_site')), el('select', { onchange: (e) => { o.site = specLue(e.target.value); } }, options(specCode(o.site), () => true)),
    el('label', {}, t('trim_temps')), el('select', { onchange: (e) => { o.temps = specLue(e.target.value); } }, options(specCode(o.temps), () => true)),
    el('label', {}, t('trim_modele')), el('select', { onchange: (e) => { o.modele = Number(e.target.value); rendre(); } },
      [3, 2, 1].map((k) => el('option', { value: k, selected: k === modele }, t('trim_modele_long_' + k)))),
    modele === 2 ? [el('label', {}, t('trim_changements')), el('div', { class: 'ligne' },
      el('input', { value: o.changements_texte, placeholder: t('trim_changements_exemple'), disabled: !!o.tous, style: 'width:14em', title: t('trim_changements_aide'),
        oninput: (e) => { o.changements_texte = e.target.value; } }),
      caseACocher(t('trim_tous_temps'), !!o.tous, (v) => { o.tous = v; rendre(); }),
      caseACocher(t('trim_pas_a_pas'), !!o.pas_a_pas, (v) => { o.pas_a_pas = v; }, t('trim_pas_a_pas_aide')))] : null,
    el('label', {}, t('trim_estimation')), el('div', { class: 'ligne' },
      caseACocher(t('trim_surdispersion'), oui('surdispersion'), (v) => { o.surdispersion = v; }, t('trim_surdispersion_aide')),
      caseACocher(t('trim_correlation'), oui('correlation'), (v) => { o.correlation = v; }, t('trim_correlation_aide'))),
    el('label', {}, t('trim_poids')), el('select', { title: t('trim_poids_aide'), onchange: (e) => { o.poids = e.target.value ? Number(e.target.value) : null; } },
      [el('option', { value: '', selected: !o.poids }, t('aucun'))].concat(p.variables.filter((v) => !v.qualitative).map((v) => el('option', { value: v.uid, selected: v.uid === o.poids }, v.nom)))),
    el('label', {}, t('trim_base')), el('input', { type: 'number', step: 'any', value: o.base ?? '', placeholder: t('trim_base_premier'), style: 'width:8em',
      oninput: (e) => { o.base = e.target.value === '' ? null : Number(e.target.value); } }),
    el('label', {}, ''), el('p', { class: 'note' }, t('trim_reglages_note')),
  ];
}
ANALYSES.TRIM = {
  libelle: () => t('type_trim'),
  note: () => t('trim_note'),
  roles: ['x', 'y'],
  echelle: 'none',
  vc: false,
  champs: champsTrim,
  valider: (st, cles, p) => {
    const o = st.options;
    const y = cles('y');
    const variable = (u) => p.variables.find((v) => v.uid === u) || {};
    if (y.length !== 1) return t('trim_y_requis');
    if (variable(y[0]).qualitative) return t('trim_y_quantitative', { nom: variable(y[0]).nom });
    const q = cles('x').filter((u) => !variable(u).qualitative);
    if (q.length) return t('trim_x_qualitatives', { noms: q.map((u) => variable(u).nom).join(', ') });
    if (st.croises.length) return t('trim_croises');
    if (!o.site || !o.temps) return t('trim_site_temps_requis');
    if (specCode(o.site) === specCode(o.temps)) return t('trim_site_temps_differents');
    if (Number(o.modele ?? 3) === 2 && !o.tous && lireTemps(o.changements_texte).some((v) => !Number.isFinite(v))) return t('trim_changements_invalides');
    if (o.base !== null && o.base !== undefined && !Number.isFinite(o.base)) return t('trim_base_invalide');
    return '';
  },
  options: (st) => {
    const o = st.options;
    const modele = Number(o.modele ?? 3);
    const oui = (k) => o[k] === undefined || o[k] === null || !!o[k];
    const cp = lireTemps(o.changements_texte);
    return { site: o.site, temps: o.temps, modele, changements: modele !== 2 ? [] : o.tous ? 'tous' : cp, pas_a_pas: modele === 2 && !!o.pas_a_pas,
      surdispersion: oui('surdispersion'), correlation: oui('correlation'), poids: o.poids || null, base: Number.isFinite(o.base) ? o.base : null };
  },
};

// ------------------------------------------------------------ the report

const blocsResume = (m) => {
  const [pi] = pentes(m);
  const tp = liste(m.temps);
  return [{ texte: `TRIM · ${libelleModele(m)} · ${t('trim_n_sites')} ${m.I} · ${t('trim_temps')} ${temps(tp[0])}–${temps(tp[tp.length - 1])} · ${t('trim_comptages')} ${m.O} / ${m.I * m.J}`
    + (m.surdispersion ? ` · σ² ${fmt(m.sigma2, 3)}` : '') + (m.correlation ? ` · ρ ${fmt(m.rho, 3)}` : '') },
  { texte: `${t('trim_pente_globale')} : ×${nombre(pi.mult, 4)} (${t('trim_se')} ${nombre(pi.se, 4)}) · ${classe(pi)}` },
  { entetes: ['', t('trim_valeur'), t('ddl'), 'p'], lignes: [[t('trim_chi2'), fmt(m.ajustement.chi2, 2), String(m.ajustement.ddl), pValeur(m.ajustement.p_chi2)],
    [t('trim_lr'), fmt(m.ajustement.LR, 2), String(m.ajustement.ddl), pValeur(m.ajustement.p_LR)], ['AIC', fmt(m.ajustement.AIC, 2), '', '']] },
  ...notesTrim(m).map((x) => ({ texte: x }))];
};
const blocsIndices = (m) => [{ entetes: [t('trim_temps'), t('trim_indices_imputes'), t('trim_se'), t('trim_indices_modele'), t('trim_se')],
  lignes: lignesDe(m.indices, ['temps', 'impute', 'se_impute', 'modele', 'se_modele']).map((l) => [temps(l[0]), ...l.slice(1).map((v) => fmt(v, 4))]) }];
const blocsTotaux = (m) => [{ entetes: [t('trim_temps'), t('trim_totaux_observes'), t('trim_totaux_imputes'), t('trim_se'), t('trim_totaux_modele'), t('trim_se')],
  lignes: lignesDe(m.totaux, ['temps', 'observe', 'impute', 'se_impute', 'modele', 'se_modele']).map((l) => [temps(l[0]), ...l.slice(1).map((v) => fmt(v, 1))]) }];
const blocsPentes = (m) => [{ entetes: entetesPentes(), lignes: lignesPentes(m) }, { texte: t('trim_pentes_note') }];
const blocsParametres = (m) => (lignesParametres(m) ? [{ entetes: entetesParametres(m), lignes: lignesParametres(m) }] : [{ texte: t('trim_sans_parametres') }]);
const blocsTests = (m) => (m.tests && liste(m.tests.test).length ? [{ entetes: [t('terme'), 'W', t('ddl'), 'p'], lignes: lignesTests(m) }, { texte: t('trim_tests_note') }] : [{ texte: t('trim_sans_tests') }]);
PARTIES_TYPES.TRIM = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'trim_indices', vue: 'trim_indices', defaut: true, tableau: blocsIndices },
  { cle: 'trim_totaux', vue: 'trim_totaux', defaut: true, tableau: blocsTotaux },
  { cle: 'trim_pentes', defaut: true, tableau: blocsPentes },
  { cle: 'trim_parametres', defaut: true, tableau: blocsParametres },
  { cle: 'trim_tests', defaut: true, tableau: blocsTests },
  { cle: 'trim_sites', vue: 'trim_sites' },
];

// ------------------------------------------------------------ registration

const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estTrim(m) ? rendreResumeTrim(c, ctx, m) : resume(c, ctx, p, loc);
} };
Object.assign(VUES, {
  trim_indices: { rendre: rendreIndices, titre: (m) => `${m.nom} · ${t('trim_indices')}` },
  trim_totaux: { rendre: rendreTotaux, titre: (m) => `${m.nom} · ${t('trim_totaux')}` },
  trim_tableaux: { rendre: rendreParametresTrim, titre: (m) => `${m.nom} · ${t('trim_tableaux')}` },
  trim_sites: { rendre: rendreSitesTrim, titre: (m) => `${m.nom} · ${t('trim_sites')}` },
});
Object.assign(ICONES_VUES, { trim_indices: 'trending-up', trim_totaux: 'chart-column', trim_tableaux: 'sigma', trim_sites: 'chart-scatter' });
VUES_TYPES.TRIM = ['resume', 'trim_indices', 'trim_totaux', 'trim_tableaux', 'trim_sites', 'liste_obs'];

ajouterTextes({
  type_trim: 'TRIM (indices et tendances de comptages)',
  trim_note: 'Des comptages d’animaux suivis sur des sites année après année, avec des comptages manquants (le tableau long : une ligne par site et par année). Y : les comptages ; X : des covariables qualitatives, facultatives (l’habitat d’un site), qui changent les effets du temps. Le site et le temps (l’année) se choisissent ci-dessous. Comme TRIM 3 et le paquet rtrim : les comptages manquants sont imputés par le modèle.',
  trim_site: 'Site', trim_temps: 'Temps (année)', trim_modele: 'Modèle', trim_changements: 'Points de changement', trim_covariables: 'Covariables', trim_poids: 'Poids',
  trim_modele_1: '1 (sans effet du temps)', trim_modele_2: '2 (tendance linéaire)', trim_modele_3: '3 (un effet par temps)',
  trim_modele_long_1: 'Modèle 1 : sans effet du temps', trim_modele_long_2: 'Modèle 2 : tendance linéaire (points de changement)', trim_modele_long_3: 'Modèle 3 : un effet par temps',
  trim_changements_exemple: '1990 2000 (le premier temps par défaut)', trim_changements_aide: 'Les temps où la pente change, séparés par des espaces ; le premier temps seul : une seule pente sur toute la période. Avant le premier point, aucune pente.',
  trim_tous_temps: 'à chaque temps', trim_pas_a_pas: 'pas à pas', trim_pas_a_pas_aide: 'Les points de changement choisis pas à pas, comme rtrim : un point sort quand le test de Wald de son changement de pente dépasse p = 0,2, revient quand son test du score passe sous p = 0,15.',
  trim_estimation: 'Estimation', trim_surdispersion: 'surdispersion (σ²)', trim_correlation: 'corrélation sérielle (ρ)',
  trim_surdispersion_aide: 'var(f) = σ² μ : des comptages plus dispersés qu’une loi de Poisson (des oiseaux en groupes). Recommandé par le manuel de TRIM.',
  trim_correlation_aide: 'Une corrélation ρ entre les comptages de deux années successives d’un même site (ρ^k à k ans d’écart). Recommandé par le manuel de TRIM.',
  trim_poids_aide: 'Une variable de poids (des sites sur-représentés ou sous-représentés) : le modèle des comptages pondérés, comme TRIM.',
  trim_base: 'Temps de base', trim_base_premier: 'le premier',
  trim_reglages_note: 'Les erreurs-types tiennent compte de σ² et de ρ (équations d’estimation généralisées) ; sans eux, le maximum de vraisemblance de Poisson. Un temps sans aucune ligne est ajouté, ses comptages imputés ; un site sans aucun comptage positif est laissé de côté.',
  trim_y_requis: 'TRIM demande une variable Y : les comptages.', trim_y_quantitative: 'Les comptages (Y) sont une variable quantitative : {nom} ne l’est pas.',
  trim_x_qualitatives: 'Les covariables (X) de TRIM sont des variables qualitatives : {noms}.', trim_croises: 'TRIM ne prend pas de terme croisé.',
  trim_site_temps_requis: 'Choisissez le site et le temps (l’année) de chaque comptage.', trim_site_temps_differents: 'Le site et le temps doivent être deux colonnes différentes.',
  trim_changements_invalides: 'Les points de changement sont des temps (des nombres), séparés par des espaces.', trim_base_invalide: 'Le temps de base est un nombre.',
  trim_n_sites: 'Sites', trim_comptages: 'Comptages observés', trim_methode: 'Estimation', trim_ml: 'max. de vraisemblance',
  trim_pente_globale: 'Pente globale (totaux imputés)', trim_pente_imputee: 'Totaux imputés', trim_pente_modele: 'Totaux du modèle',
  trim_phrase_pente: 'Les totaux imputés changent de ×{m} par pas de temps ({pct} %), erreur-type {se}, intervalle à 95 % {bas} – {haut} : {classe}.',
  trim_phrase_pente_modele: 'Ceux du modèle : ×{m} ({pct} %), intervalle à 95 % {bas} – {haut} ({classe}).',
  trim_pente_inconnue: 'Pas de pente globale (moins de trois temps, ou un total nul).',
  trim_classe: 'Classe', trim_classe_forte_hausse: 'forte augmentation', trim_classe_hausse_moderee: 'augmentation modérée', trim_classe_stable: 'stable',
  trim_classe_incertaine: 'incertaine', trim_classe_declin_modere: 'déclin modéré', trim_classe_fort_declin: 'fort déclin', trim_classe_inconnue: 'inconnue',
  trim_valeur: 'Valeur', trim_chi2: 'khi² de Pearson', trim_lr: 'Rapport de vraisemblance',
  trim_ajustement_gee: 'Ces tests d’ajustement supposent des comptages de Poisson indépendants : avec σ² ou ρ estimés, ils ne valent plus (l’AIC non plus) ; les tests de Wald en tiennent compte.',
  trim_ajustement_ml: 'Un p faible : le modèle ne décrit pas bien les comptages (souvent de la surdispersion). L’AIC (LR − 2 ddl) compare des modèles des mêmes données.',
  trim_resume_note: 'La classification de TRIM : l’intervalle de confiance à 95 % de la pente multiplicative comparé à 1 et à 0,95 / 1,05 (forte augmentation : au-dessus de 1,05 ; augmentation modérée : au-dessus de 1 ; stable : dans ]0,95 ; 1,05[ en contenant 1 ; fort déclin : sous 0,95 ; déclin modéré : sous 1 ; incertaine sinon) ; p < 0,01 quand l’intervalle à 99 % la donne aussi.',
  trim_sites_vides: '{n} site(s) sans aucun comptage positif, laissé(s) de côté : {noms}.', trim_temps_ajoutes: 'Temps sans aucune ligne, ajoutés (comptages imputés) : {temps}.',
  trim_changements_retires: 'Points de changement retirés, sans comptage positif après eux : {temps}.', trim_covariables_retirees: 'Covariable(s) d’une seule catégorie, laissée(s) de côté : {noms}.',
  trim_sans_site_temps: '{n} ligne(s) sans site ou sans temps, laissée(s) de côté.',
  trim_rho_faible: 'La corrélation sérielle est très faible (ρ = {rho}) : le modèle peut s’en passer.', trim_rho_negatif: 'La corrélation sérielle estimée est négative (ρ = {rho}), ce que rtrim refuse : le modèle peut s’en passer.',
  trim_sigma2_faible: 'σ² = {s} < 1 : pas de surdispersion ; le modèle peut s’en passer.',
  trim_indices: 'Indices', trim_indice: 'Indice', trim_indices_imputes: 'Indices imputés', trim_indices_modele: 'Indices du modèle',
  trim_bande: 'bande à 95 %', trim_bande_aide: '± 1,96 erreur-type (méthode delta).', trim_droite: 'droite de la pente globale', trim_droite_aide: 'La droite des moindres carrés à travers le logarithme des totaux imputés.',
  trim_se: 'Erreur-type', trim_totaux: 'Totaux de chaque temps', trim_total: 'Total', trim_totaux_observes: 'Observés', trim_totaux_imputes: 'Imputés', trim_totaux_modele: 'Du modèle',
  trim_totaux_note: 'Observés : la somme des comptages faits (elle dépend des sites comptés cette année-là) ; imputés : les comptages observés et, pour les manquants, ceux du modèle ; du modèle : les comptages attendus de tous les sites.',
  trim_pentes: 'Pentes globales', trim_multiplicative: 'Multiplicative', trim_pentes_note: 'La pente de la droite des moindres carrés à travers le logarithme des totaux, par pas de temps ; son erreur-type d’après la covariance des totaux ; p et l’intervalle à 95 % par la loi t à J − 2 degrés de liberté, comme TRIM.',
  trim_parametres: 'Paramètres du temps', trim_tableaux: 'Pentes, paramètres et tests', trim_reference: 'référence', trim_morceau: 'Morceau', trim_additif: 'Additif', trim_multiplicatif: 'Multiplicatif',
  trim_parametres_note_2: 'La pente de chaque morceau entre deux points de changement, sur l’échelle log (additive) et en facteur d’un pas à l’autre (multiplicative) ; une ligne « covariable = catégorie » : l’écart de la pente de cette catégorie à celle de la référence.',
  trim_parametres_note_3: 'L’effet de chaque temps par rapport au premier, sur l’échelle log (additif) et en facteur (multiplicatif) ; une ligne « covariable = catégorie » : l’écart de cette catégorie à la référence.',
  trim_sans_parametres: 'Le modèle 1 n’a pas de paramètre du temps.',
  trim_ecarts: 'Droite et écarts à la droite', trim_ecarts_note: 'Le modèle 3 en une droite (pente multiplicative ×{b}, erreur-type {se}) et l’écart de chaque temps à elle.',
  trim_tests: 'Tests de Wald', trim_test_pente: 'Pente', trim_test_changement: 'Changement de pente en {temps}', trim_test_ecarts: 'Écarts à la tendance linéaire', trim_test_covariable: 'Effet de {nom}',
  trim_tests_note: 'p ≤ 0,05 en rouge. Les tests de Wald tiennent compte de σ² et de ρ. Un changement de pente au premier point : la pente elle-même ; avec des covariables, ceux de toutes les catégories ensemble.',
  trim_sans_tests: 'Aucun test pour ce modèle.',
  trim_pas_a_pas_note: 'Depuis {depart} ; gardés : {gardes}.', trim_action: 'Action', trim_action_retire: 'retiré', trim_action_remis: 'remis', trim_test: 'Test', trim_score: 'score',
  trim_sites: 'Comptages par site', trim_un_site: 'Un site', trim_tous_sites: 'Tous les comptages', trim_attendus: 'Attendus (modèle)', trim_observes: 'Observés',
  trim_imputes: 'Imputés', trim_comptage: 'Comptage', trim_observes_attendus: 'Comptages observés et attendus', trim_n_observes: 'Comptages', trim_n_positifs: 'Positifs',
  trim_total_observe: 'Total observé', trim_total_attendu: 'Total attendu',
}, {
  type_trim: 'TRIM (indices and trends of counts)',
  trim_note: 'Counts of animals surveyed on sites year after year, with missing counts (the long table: one row per site and year). Y: the counts; X: optional qualitative covariates (a site’s habitat) that change the time effects. The site and the time (the year) are chosen below. As TRIM 3 and the rtrim package: the missing counts are imputed by the model.',
  trim_site: 'Site', trim_temps: 'Time (year)', trim_modele: 'Model', trim_changements: 'Changepoints', trim_covariables: 'Covariates', trim_poids: 'Weights',
  trim_modele_1: '1 (no time effect)', trim_modele_2: '2 (linear trend)', trim_modele_3: '3 (an effect per time)',
  trim_modele_long_1: 'Model 1: no time effect', trim_modele_long_2: 'Model 2: linear trend (changepoints)', trim_modele_long_3: 'Model 3: an effect per time',
  trim_changements_exemple: '1990 2000 (the first time by default)', trim_changements_aide: 'The times where the slope changes, separated by spaces; the first time alone: a single slope over the whole period. Before the first point, no slope.',
  trim_tous_temps: 'at every time', trim_pas_a_pas: 'stepwise', trim_pas_a_pas_aide: 'The changepoints chosen stepwise, as rtrim does: a point leaves when the Wald test of its change in slope exceeds p = 0.2, comes back when its score test falls below p = 0.15.',
  trim_estimation: 'Estimation', trim_surdispersion: 'overdispersion (σ²)', trim_correlation: 'serial correlation (ρ)',
  trim_surdispersion_aide: 'var(f) = σ² μ: counts more dispersed than a Poisson distribution (birds in flocks). Recommended by the TRIM manual.',
  trim_correlation_aide: 'A correlation ρ between the counts of two successive years of a site (ρ^k k years apart). Recommended by the TRIM manual.',
  trim_poids_aide: 'A weight variable (over- or under-represented sites): the model of the weighted counts, as TRIM does.',
  trim_base: 'Base time', trim_base_premier: 'the first',
  trim_reglages_note: 'The standard errors allow for σ² and ρ (generalized estimating equations); without them, Poisson maximum likelihood. A time without any row is added, its counts imputed; a site without any positive count is left out.',
  trim_y_requis: 'TRIM needs a Y variable: the counts.', trim_y_quantitative: 'The counts (Y) are a quantitative variable: {nom} isn’t one.',
  trim_x_qualitatives: 'TRIM’s covariates (X) are qualitative variables: {noms}.', trim_croises: 'TRIM takes no cross term.',
  trim_site_temps_requis: 'Choose the site and the time (the year) of each count.', trim_site_temps_differents: 'The site and the time must be two different columns.',
  trim_changements_invalides: 'The changepoints are times (numbers), separated by spaces.', trim_base_invalide: 'The base time is a number.',
  trim_n_sites: 'Sites', trim_comptages: 'Observed counts', trim_methode: 'Estimation', trim_ml: 'max. likelihood',
  trim_pente_globale: 'Overall slope (imputed totals)', trim_pente_imputee: 'Imputed totals', trim_pente_modele: 'Model totals',
  trim_phrase_pente: 'The imputed totals change by ×{m} per time step ({pct}%), standard error {se}, 95% interval {bas} – {haut}: {classe}.',
  trim_phrase_pente_modele: 'The model’s: ×{m} ({pct}%), 95% interval {bas} – {haut} ({classe}).',
  trim_pente_inconnue: 'No overall slope (fewer than three times, or a total of zero).',
  trim_classe: 'Class', trim_classe_forte_hausse: 'strong increase', trim_classe_hausse_moderee: 'moderate increase', trim_classe_stable: 'stable',
  trim_classe_incertaine: 'uncertain', trim_classe_declin_modere: 'moderate decline', trim_classe_fort_declin: 'steep decline', trim_classe_inconnue: 'unknown',
  trim_valeur: 'Value', trim_chi2: 'Pearson chi-square', trim_lr: 'Likelihood ratio',
  trim_ajustement_gee: 'These goodness-of-fit tests assume independent Poisson counts: with σ² or ρ estimated they no longer hold (nor does the AIC); the Wald tests allow for them.',
  trim_ajustement_ml: 'A small p: the model doesn’t describe the counts well (often overdispersion). The AIC (LR − 2 df) compares models of the same data.',
  trim_resume_note: 'TRIM’s classification: the 95% confidence interval of the multiplicative slope compared with 1 and with 0.95 / 1.05 (strong increase: above 1.05; moderate increase: above 1; stable: within (0.95, 1.05) and containing 1; steep decline: below 0.95; moderate decline: below 1; uncertain otherwise); p < 0.01 when the 99% interval gives it too.',
  trim_sites_vides: '{n} site(s) without any positive count, left out: {noms}.', trim_temps_ajoutes: 'Times without any row, added (counts imputed): {temps}.',
  trim_changements_retires: 'Changepoints removed, without a positive count after them: {temps}.', trim_covariables_retirees: 'Covariate(s) of a single category, left out: {noms}.',
  trim_sans_site_temps: '{n} row(s) without a site or a time, left out.',
  trim_rho_faible: 'The serial correlation is very low (ρ = {rho}): the model can do without it.', trim_rho_negatif: 'The estimated serial correlation is negative (ρ = {rho}), which rtrim refuses: the model can do without it.',
  trim_sigma2_faible: 'σ² = {s} < 1: no overdispersion; the model can do without it.',
  trim_indices: 'Indices', trim_indice: 'Index', trim_indices_imputes: 'Imputed indices', trim_indices_modele: 'Model indices',
  trim_bande: '95% band', trim_bande_aide: '± 1.96 standard errors (delta method).', trim_droite: 'overall slope line', trim_droite_aide: 'The least-squares line through the logarithm of the imputed totals.',
  trim_se: 'Standard error', trim_totaux: 'Time totals', trim_total: 'Total', trim_totaux_observes: 'Observed', trim_totaux_imputes: 'Imputed', trim_totaux_modele: 'Model',
  trim_totaux_note: 'Observed: the sum of the counts made (it depends on the sites counted that year); imputed: the observed counts and, for the missing ones, the model’s; model: the expected counts of every site.',
  trim_pentes: 'Overall slopes', trim_multiplicative: 'Multiplicative', trim_pentes_note: 'The slope of the least-squares line through the logarithm of the totals, per time step; its standard error from the totals’ covariance; p and the 95% interval by the t distribution with J − 2 degrees of freedom, as TRIM does.',
  trim_parametres: 'Time parameters', trim_tableaux: 'Slopes, parameters and tests', trim_reference: 'reference', trim_morceau: 'Piece', trim_additif: 'Additive', trim_multiplicatif: 'Multiplicative',
  trim_parametres_note_2: 'The slope of each piece between two changepoints, on the log scale (additive) and as a factor from one step to the next (multiplicative); a “covariate = category” row: how the slope of that category differs from the reference’s.',
  trim_parametres_note_3: 'The effect of each time relative to the first, on the log scale (additive) and as a factor (multiplicative); a “covariate = category” row: how that category differs from the reference.',
  trim_sans_parametres: 'Model 1 has no time parameter.',
  trim_ecarts: 'Line and deviations from it', trim_ecarts_note: 'Model 3 as a line (multiplicative slope ×{b}, standard error {se}) and each time’s deviation from it.',
  trim_tests: 'Wald tests', trim_test_pente: 'Slope', trim_test_changement: 'Change in slope at {temps}', trim_test_ecarts: 'Deviations from the linear trend', trim_test_covariable: 'Effect of {nom}',
  trim_tests_note: 'p ≤ 0.05 in red. The Wald tests allow for σ² and ρ. A change in slope at the first point: the slope itself; with covariates, those of every category together.',
  trim_sans_tests: 'No test for this model.',
  trim_pas_a_pas_note: 'From {depart}; kept: {gardes}.', trim_action: 'Action', trim_action_retire: 'removed', trim_action_remis: 'put back', trim_test: 'Test', trim_score: 'score',
  trim_sites: 'Counts by site', trim_un_site: 'One site', trim_tous_sites: 'All counts', trim_attendus: 'Expected (model)', trim_observes: 'Observed',
  trim_imputes: 'Imputed', trim_comptage: 'Count', trim_observes_attendus: 'Observed and expected counts', trim_n_observes: 'Counts', trim_n_positifs: 'Positive',
  trim_total_observe: 'Observed total', trim_total_attendu: 'Expected total',
});
