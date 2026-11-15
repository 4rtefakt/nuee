// Distance sampling, density and abundance from line and point transects (moteur/R/distance.R): its
// settings in the model dialog, its views (the summary with the model chosen, the AIC of the
// candidates, p, ESW or EDR and the goodness of fit; the detection function over the histogram of
// distances, and for points the pdf of the distances; the QQ plot; the density and abundance by
// stratum), its report parts and its strings. Imported by app.js.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, fmt } from './ui.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, tableau, notesModele, valeurP, ficheCondition } from './panneaux.js';
import { ANALYSES } from './dialogues.js';
import { VUES_TYPES } from './explorateur.js';
import { ICONES_VUES } from './commandes.js';
import { PARTIES_TYPES } from './rapport.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const estDistance = (m) => !!m && !!m.analyse && m.type === 'DISTANCE';
const fini = (v) => v !== null && v !== undefined && Number.isFinite(v);
// a value with k significant digits, a large one as a whole number
const sig = (v, k = 4) => (!fini(v) ? '—' : Math.abs(v) >= 10 ** k ? String(Math.round(v)) : String(+Number(v).toPrecision(k)));
const pct = (v) => (!fini(v) ? '—' : `${fmt(100 * v, 1)}${langueActuelle() === 'fr' ? ' %' : '%'}`);
const pValeur = (v) => (!fini(v) ? '—' : valeurP(v));
const chiffre = (k, v) => el('span', {}, el('small', {}, k), ' ', el('b', {}, v ?? '—'));
const caseACocher = (libelle, valeur, changer, titre) => el('label', { class: 'champ', title: titre || '' },
  el('input', { type: 'checkbox', checked: valeur, onchange: (e) => changer(e.target.checked) }), libelle);
// the rows of a data.frame sent by R (columns), as arrays of values
const lignesDe = (df, cles) => { const n = liste((df || {})[cles[0]]).length; return Array.from({ length: n }, (_, i) => cles.map((k) => liste(df[k])[i])); };
function tableCompacte(entetes, lignes, marque) {
  return el('table', { class: 'tableau compact' },
    el('thead', {}, el('tr', {}, entetes.map((h, j) => el('th', { class: j ? 'num' : '' }, h)))),
    el('tbody', {}, lignes.map((l, i) => el('tr', { class: marque && marque(i) ? 'choisie' : '' }, l.map((v, j) => el('td', { class: j ? 'num' : '' }, v))))));
}
const translucide = (couleur, a) => `rgba(${[1, 3, 5].map((i) => parseInt(couleur.slice(i, i + 2), 16)).join(', ')}, ${a})`;
const point = (m) => m.transect === 'point';
const uDist = (m) => (m.unites && m.unites.distance) || '';
const uAire = (m) => (m.unites && m.unites.aire) || '';
const parAire = (m) => (uAire(m) ? ` /${uAire(m)}` : '');
const parAireCourt = (m) => (uAire(m) ? `/${uAire(m)}` : t('distance_par_unite'));
const etiquette = (texte) => el('span', { class: 'doux', style: 'white-space:nowrap' }, texte);
const avecUnite = (v, u) => (u ? `${v} ${u}` : v);
const etiquetteStrate = (s) => (s === null || s === undefined ? t('distance_total') : s === '' ? t('distance_region') : s);
const lignesDensite = (dn) => lignesDe(dn, ['strate', 'aire', 'couverte', 'effort', 'k', 'n', 'rencontre', 'se_rencontre', 'cv_rencontre', 'D', 'se_D', 'cv', 'bas_D', 'haut_D', 'ddl',
  'N', 'se_N', 'bas_N', 'haut_N', 'taille_moyenne', 'part_rencontre', 'part_detection', 'part_groupes']).map((l) => ({
  strate: l[0], aire: l[1], couverte: l[2], effort: l[3], k: l[4], n: l[5], er: l[6], se_er: l[7], cv_er: l[8], D: l[9], se_D: l[10], cv: l[11], bas_D: l[12],
  haut_D: l[13], ddl: l[14], N: l[15], se_N: l[16], bas_N: l[17], haut_N: l[18], taille: l[19], p_er: l[20], p_det: l[21], p_gr: l[22] }));
const effectifLibelle = (m) => t(point(m) ? 'distance_edr' : 'distance_esw');
const libelleCle = (c) => t('distance_cle_' + c);
const libelleSerie = (s) => t('distance_serie_' + (s || 'aucune'));
// the model's notes: rows left out, constraints met, a stratum of one transect
function notesDistance(m) {
  const n = [];
  const tr = m.troncature || {};
  if (m.au_dela) n.push(t('distance_au_dela', { n: m.au_dela, w: avecUnite(sig(tr.w), uDist(m)) }));
  if (m.en_deca) n.push(t('distance_en_deca', { n: m.en_deca, L: avecUnite(sig(tr.L), uDist(m)) }));
  if (m.contraint) n.push(t('distance_contraint'));
  if (lignesDensite(m.densite).some((l) => l.strate !== null && l.k === 1)) n.push(t('distance_un_transect'));
  if (m.densite_seule) n.push(t('distance_densite_seule'));
  if (m.unites && m.unites.donne) n.push(t('distance_facteur_donne', { f: sig(m.unites.facteur, 6) }));
  return n;
}
const phraseSelection = (m) => t('distance_selection_' + (m.selection || 'aic'), { serie: libelleSerie(m.serie_demandee) });

// ------------------------------------------------------------ summary

const lignesCandidats = (m) => lignesDe(m.candidats, ['modele', 'parametres', 'logL', 'AIC', 'delta_AIC', 'converge', 'choisi'])
  .map((l) => [l[0], String(l[1]), fini(l[2]) ? fmt(l[2], 3) : '—', fini(l[3]) ? fmt(l[3], 3) : '—', fini(l[4]) ? fmt(l[4], 3) : '—',
    l[6] ? '✓' : l[5] ? '' : t('distance_non_converge')]);
const entetesCandidats = () => [t('distance_modele'), t('distance_n_parametres'), 'logL', 'AIC', 'ΔAIC', t('distance_retenu')];
const lignesParametres = (m) => (m.parametres ? lignesDe(m.parametres, ['parametre', 'estimation', 'se']).map((l) => [l[0], fmt(l[1], 4), fmt(l[2], 4)]) : []);
const lignesAdequation = (m) => {
  const a = m.adequation;
  return [a.cvm ? [t('distance_cvm'), fmt(a.cvm.W, 4), '', pValeur(a.cvm.p)] : null,
    [t('distance_khi2', { k: liste(a.khi2.classes.de).length }), fmt(a.khi2.chi2, 3), fini(a.khi2.ddl) ? String(a.khi2.ddl) : '—', pValeur(a.khi2.p)]].filter(Boolean);
};
// distances recorded in intervals (their bounds)
const parIntervalles = (m) => liste(m.intervalles).length > 0;
const texteTroncature = (m) => {
  const tr = m.troncature || {};
  if (parIntervalles(m)) return t('distance_n_intervalles', { k: liste(m.intervalles).length - 1, de: sig(tr.L), a: avecUnite(sig(tr.w), uDist(m)) });
  return `${avecUnite(sig(tr.w), uDist(m))}${fini(tr.pourcentage) ? ` (${tr.pourcentage} %)` : ''}${tr.L > 0 ? ` · ${t('distance_gauche')} ${sig(tr.L)}` : ''}`;
};
const phraseDetection = (m) => t('distance_phrase_p', { p: fmt(m.p, 3), se: fmt(m.se_p, 3), cv: pct(m.cv_p), quoi: effectifLibelle(m),
  e: avecUnite(sig(m.effectif.valeur), uDist(m)), se_e: avecUnite(sig(m.effectif.se, 3), uDist(m)) });
function phraseTotal(m) {
  const dn = lignesDensite(m.densite);
  const tot = dn[dn.length - 1];
  const d = t('distance_phrase_D', { D: sig(tot.D), u: parAire(m), cv: pct(tot.cv), bas: sig(tot.bas_D), haut: sig(tot.haut_D), ddl: fmt(tot.ddl, 1) });
  const n = m.densite_seule ? d : `${d} ${t('distance_phrase_N', { N: sig(tot.N), bas: sig(tot.bas_N), haut: sig(tot.haut_N) })}`;
  if (!m.groupes || !m.densite_groupes) return n;
  const g = lignesDensite(m.densite_groupes);
  const tg = g[g.length - 1];
  return `${n} ${t('distance_phrase_groupes', { D: sig(tg.D), u: parAire(m), cv: pct(tg.cv), s: fmt(tot.D / tg.D, 2) })}`;
}

function rendreResumeDistance(conteneur, ctx, m) {
  const titre = el('div', { class: 'fiche-titre' }, el('b', {}, m.nom), el('span', { class: 'badge' }, t('distance_badge')), m.titre ? el('span', { class: 'doux' }, m.titre) : null);
  const strates = liste(m.strates).length;
  const fiche = el('div', { class: 'fiche' }, titre,
    el('div', { class: 'fiche-chiffres' }, chiffre(t('distance_plan'), t(point(m) ? 'distance_points' : 'distance_lignes')), chiffre(t('distance_detections'), String(m.n)),
      chiffre(t('distance_transects'), String(liste(m.transects.transect).length)), m.avec_strates ? chiffre(t('distance_strates'), String(strates)) : null,
      chiffre(t(parIntervalles(m) ? 'distance_intervalles' : 'distance_troncature'), texteTroncature(m))),
    el('div', { class: 'fiche-chiffres' }, chiffre(t('distance_modele'), m.modele_detection), chiffre('p', `${fmt(m.p, 3)} ± ${fmt(m.se_p, 3)}`),
      chiffre(effectifLibelle(m), avecUnite(`${sig(m.effectif.valeur)} ± ${sig(m.effectif.se, 3)}`, uDist(m))), chiffre('AIC', fmt(m.AIC, 2)),
      liste(m.ordres).length ? chiffre(t('distance_monotonie'), t('distance_monotonie_' + m.monotonie)) : null),
    ficheCondition(m));
  const notes = notesModele(conteneur, ctx, m);
  conteneur.replaceChildren(el('div', { class: 'defile resume' }, fiche,
    el('div', { class: 'fiche' }, el('div', { class: 'fiche-titre' }, el('b', {}, t('distance_densite_totale'))), el('p', {}, phraseTotal(m)), el('p', {}, phraseDetection(m))),
    notes.champ,
    el('h4', {}, t('distance_candidats')), tableCompacte(entetesCandidats(), lignesCandidats(m), (i) => !!liste(m.candidats.choisi)[i]),
    el('p', { class: 'note' }, phraseSelection(m)),
    lignesParametres(m).length ? [el('h4', {}, t('distance_parametres')), tableCompacte(['', t('estimation'), t('erreur_type')], lignesParametres(m)),
      el('p', { class: 'note' }, [m.cle === 'hr' ? t('distance_parametre_b') : null, m.cle !== 'unif' ? t('distance_parametre_sigma') : null,
        liste(m.ordres).length ? t('distance_parametre_a') : null, t('distance_parametres_note')].filter(Boolean).join(' '))] : null,
    el('h4', {}, t('distance_adequation')), tableCompacte(['', t('distance_statistique'), t('ddl'), 'p'], lignesAdequation(m)),
    el('p', { class: 'note' }, t(parIntervalles(m) ? 'distance_adequation_note_intervalles' : 'distance_adequation_note')),
    notesDistance(m).map((x) => el('p', { class: 'note' }, x))));
  notes.retrouver();
}

// ------------------------------------------------------------ the detection function

// The histogram of the distances scaled as mrds's plot.ds does (each bar, its count over the count
// the bin would have if every animal were detected: it estimates g there), under the fitted detection
// function; for points, the pdf of the distances, f(r) = 2r g(r) / (w² p), over their density.
function rendreDetection(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.courbe ??= 'g';
  loc.points ??= true;
  const pdf = point(m) && loc.courbe === 'f';
  const points = loc.points && !pdf && !parIntervalles(m);  // (distances in intervals: no exact distance to show)
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(outils(
    point(m) ? choix('', [['g', t('distance_g')], ['f', t('distance_pdf')]], loc.courbe, (v) => { loc.courbe = v; ctx.rafraichir(); }) : null,
    pdf || parIntervalles(m) ? null : caseACocher(t('distance_points_obs'), loc.points, (v) => { loc.points = v; ctx.rafraichir(); }, t('distance_points_obs_aide'))),
  el('p', { class: 'note' }, t(pdf ? 'distance_pdf_note' : point(m) ? 'distance_g_note_points' : 'distance_g_note')), graphe, bas);
  const h = m.histogramme;
  const de = liste(h.de);
  const a = liste(h.a);
  const barres = liste(pdf ? h.f : h.g);
  const c = m.courbe;
  const courbe = liste(pdf ? c.f : c.g);
  const traces = [{ type: 'bar', name: t('distance_histogramme'), x: de.map((v, i) => (v + a[i]) / 2), y: barres, width: de.map((v, i) => a[i] - v),
    customdata: liste(h.compte), marker: { color: translucide(PALETTE[0], 0.3), line: { color: PALETTE[0], width: 1 } },
    hovertemplate: `%{x:.3g}<br>%{y:.3f} (n = %{customdata})<extra>${t('distance_histogramme')}</extra>` },
  { type: 'scatter', mode: 'lines', name: pdf ? t('distance_pdf') : t('distance_g'), x: liste(c.x), y: courbe, line: { color: PALETTE[1], width: 2.5 },
    hovertemplate: `%{x:.3g}<br>%{y:.4f}<extra>${m.modele_detection}</extra>` }];
  if (points) {
    const sel = new Set(ctx.selection(m.id));
    const uids = liste(m.uid_distances);
    traces.push({ type: 'scatter', mode: 'markers', name: t('distance_observees'), x: liste(m.distances), y: liste(m.g_distances), customdata: uids,
      marker: { size: 6, color: uids.map((u) => (sel.has(u) ? '#e4572e' : PALETTE[1])), opacity: 0.6 },
      hovertemplate: `%{x}<br>g = %{y:.3f}<extra>${t('distance_observees')}</extra>` });
  }
  const ymax = Math.max(1.05, ...barres.map((v) => v * 1.05), ...courbe.map((v) => v * 1.05));
  tracer(ctx, graphe, traces, { title: { text: `${m.nom} · ${m.modele_detection}` }, barmode: 'overlay', bargap: 0, dragmode: 'zoom',
    xaxis: { title: { text: avecUnite(m.noms ? m.noms.y : t('distance_distance'), uDist(m) ? `(${uDist(m)})` : '') }, range: [Math.min(0, de[0] || 0), m.troncature.w] },
    yaxis: { title: { text: pdf ? t('distance_densite_proba') : t('distance_proba') }, range: [0, pdf ? undefined : ymax], rangemode: 'tozero' } });
  if (points) brancherSelection(graphe, ctx, m.id);
  const k = m.adequation.khi2.classes;
  const obs = liste(k.observe);
  const att = liste(k.attendu);
  loc.table ??= {};
  tableau(bas, ctx, loc.table, {
    n: obs.length, nom: 'Classes',
    colonnes: [{ cle: 'de', titre: t('distance_de'), num: true, format: (v) => sig(v) }, { cle: 'a', titre: t('distance_a'), num: true, format: (v) => sig(v) },
      { cle: 'observe', titre: t('distance_observe'), num: true, decimales: 0 }, { cle: 'attendu', titre: t('distance_attendu'), num: true, decimales: 2 },
      { cle: 'contribution', titre: t('distance_contribution'), num: true, decimales: 3 }],
    valeur: (i, cle) => (cle === 'contribution' ? (obs[i] - att[i]) ** 2 / att[i] : liste(k[cle])[i]),
  });
}

// ------------------------------------------------------------ the QQ plot

function rendreQQ(conteneur, ctx, p) {
  const m = ctx.modele(p.modele);
  const qq = m.adequation.qq;
  const uids = liste(qq.uid);
  const sel = new Set(ctx.selection(m.id));
  const graphe = el('div', { class: 'graphe' });
  const c = couleursTheme(ctx);
  conteneur.replaceChildren(el('p', { class: 'note' }, t('distance_qq_note', { W: fmt(m.adequation.cvm.W, 4), p: pValeur(m.adequation.cvm.p) })), graphe);
  tracer(ctx, graphe, [
    { type: 'scatter', mode: 'lines', x: [0, 1], y: [0, 1], line: { color: c.doux, width: 1, dash: 'dot' }, hoverinfo: 'skip', showlegend: false },
    { type: 'scatter', mode: 'markers', name: t('distance_observees'), x: liste(qq.empirique), y: liste(qq.ajustee), customdata: uids, text: liste(qq.distance).map((d) => sig(d)),
      marker: { size: 6, color: uids.map((u) => (sel.has(u) ? '#e4572e' : PALETTE[0])), opacity: 0.75 },
      hovertemplate: `${t('distance_distance')} %{text}<br>${t('distance_empirique')} %{x:.3f}<br>${t('distance_ajustee')} %{y:.3f}<extra></extra>` }],
  { title: { text: `${m.nom} · ${t('distance_qq')}` }, showlegend: false,
    xaxis: { title: { text: t('distance_empirique') }, range: [0, 1], constrain: 'domain' }, yaxis: { title: { text: t('distance_ajustee') }, range: [0, 1], scaleanchor: 'x' } });
  brancherSelection(graphe, ctx, m.id);
}

// ------------------------------------------------------------ density and abundance

function rendreDensite(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  loc.quoi ??= 'individus';
  const groupes = m.groupes && m.densite_groupes && loc.quoi === 'groupes';
  const lignes = lignesDensite(groupes ? m.densite_groupes : m.densite);
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas' });
  conteneur.replaceChildren(outils(m.groupes ? choix('', [['individus', t('distance_individus')], ['groupes', t('distance_groupes')]], loc.quoi, (v) => { loc.quoi = v; ctx.rafraichir(); }) : null),
    el('p', { class: 'note' }, t(m.densite_seule ? 'distance_densite_note_seule' : 'distance_densite_note', { u: parAireCourt(m) })), graphe, bas);
  const noms = lignes.map((l) => etiquetteStrate(l.strate));
  const couleurs = lignes.map((l) => (l.strate === null ? PALETTE[1] : PALETTE[0]));
  tracer(ctx, graphe, [{ type: 'scatter', mode: 'markers', x: noms, y: lignes.map((l) => l.D), marker: { size: 10, color: couleurs },
    error_y: { type: 'data', symmetric: false, array: lignes.map((l) => l.haut_D - l.D), arrayminus: lignes.map((l) => l.D - l.bas_D), color: c0(ctx), thickness: 1.5, width: 6 },
    customdata: lignes.map((l) => [sig(l.bas_D), sig(l.haut_D), pct(l.cv)]),
    hovertemplate: `<b>%{x}</b><br>D %{y:.4g}${parAire(m)}<br>${t('ic_95')} %{customdata[0]} – %{customdata[1]}<br>CV %{customdata[2]}<extra></extra>` }],
  { title: { text: `${m.nom} · ${t(groupes ? 'distance_densite_groupes' : 'distance_densite')}` }, showlegend: false, dragmode: 'zoom',
    xaxis: { type: 'category', title: { text: m.noms && m.noms.strate ? m.noms.strate : '' } },
    yaxis: { title: { text: `D${parAire(m) ? ` (${parAire(m).trim()})` : ''}` }, rangemode: 'tozero' } });
  loc.table ??= {};
  const colonnes = [{ cle: 'strate', titre: m.noms && m.noms.strate ? m.noms.strate : t('distance_strate') },
    m.densite_seule ? null : { cle: 'aire', titre: avecUnite(t('distance_aire'), uAire(m) ? `(${uAire(m)})` : ''), num: true, format: (v) => sig(v, 6) },
    { cle: 'couverte', titre: t('distance_aire_couverte'), num: true, format: (v) => sig(v, 5) },
    { cle: 'effort', titre: point(m) ? t('distance_passages') : avecUnite(t('distance_effort'), m.unites.effort ? `(${m.unites.effort})` : ''), num: true, format: (v) => sig(v, 6) },
    { cle: 'k', titre: 'k', num: true, decimales: 0 }, { cle: 'n', titre: 'n', num: true, decimales: 0 },
    { cle: 'er', titre: t('distance_rencontre'), num: true, format: (v) => sig(v) }, { cle: 'cv_er', titre: 'CV', num: true, format: pct },
    { cle: 'D', titre: `D${parAire(m)}`, num: true, format: (v) => sig(v) }, { cle: 'se_D', titre: t('erreur_type'), num: true, format: (v) => sig(v, 3) },
    { cle: 'cv', titre: 'CV', num: true, format: pct }, { cle: 'ic_D', titre: t('ic_95') }, { cle: 'ddl', titre: t('ddl'), num: true, decimales: 1 },
    m.densite_seule ? null : { cle: 'N', titre: 'N', num: true, format: (v) => sig(v) }, m.densite_seule ? null : { cle: 'ic_N', titre: `N ${t('ic_95')}` },
    m.groupes && !groupes ? { cle: 'taille', titre: t('distance_taille_moyenne'), num: true, decimales: 2 } : null,
    { cle: 'p_er', titre: t('distance_part_rencontre'), num: true, decimales: 1 }, { cle: 'p_det', titre: t('distance_part_detection'), num: true, decimales: 1 },
    m.groupes && !groupes ? { cle: 'p_gr', titre: t('distance_part_groupes'), num: true, decimales: 1 } : null].filter(Boolean);
  tableau(bas, ctx, loc.table, {
    n: lignes.length, nom: groupes ? 'Groups' : 'Density', colonnes,
    valeur: (i, cle) => {
      const l = lignes[i];
      if (cle === 'strate') return etiquetteStrate(l.strate);
      if (cle === 'ic_D') return `${sig(l.bas_D)} – ${sig(l.haut_D)}`;
      if (cle === 'ic_N') return `${sig(l.bas_N)} – ${sig(l.haut_N)}`;
      return l[cle];
    },
  });
}
const c0 = (ctx) => couleursTheme(ctx).texte;

// ------------------------------------------------------------ the model dialog

const specCode = (s) => (!s ? '' : s.by === 'id' ? `id:${[].concat(s.id)[0]}` : `var:${[].concat(s.variable)[0]}`);
const specLue = (v) => (!v ? null : v.startsWith('id:') ? { by: 'id', id: v.slice(3) } : { by: 'variable', variable: Number(v.slice(4)) });
// the column a name points to, when the dialog opens for a new model (Distance's flat file: Sample.Label, Effort…)
function deviner(p, motif, quantitative) {
  if (quantitative !== true) {
    const id = [].concat(p.identifiants || []).find((i) => motif.test(i.nom));
    if (id) return { by: 'id', id: id.nom };
  }
  const v = p.variables.find((x) => motif.test(x.nom) && (quantitative === undefined || !x.qualitative === quantitative));
  return v ? (quantitative === true ? v.uid : { by: 'variable', variable: v.uid }) : null;
}
const MOTIFS = {
  distance: /^(distance|dist|perp)/i, echantillon: /^(sample|transect|ligne|line|point|station|parcours|route|itin)/i,
  effort: /^(effort|longueur|length|passages?|visites?|visits?)/i, strate: /^(region|r[ée]gion|strat|zone|secteur)/i,
  aire: /^(area|aire|surface|superficie)/i, taille: /^(size|taille|cluster|groupe|group)/i,
};
const UNITES_LONGUEUR = ['m', 'km', 'ft', 'yd', 'mi', 'nmi'];
const UNITES_AIRE = ['m2', 'ha', 'km2', 'acre', 'mi2', 'nmi2'];
const SYMBOLES = { m2: 'm²', km2: 'km²', mi2: 'mi²', nmi2: 'nmi²' };
const nombreLu = (v) => { const x = Number(String(v ?? '').replace(',', '.')); return String(v ?? '').trim() === '' ? null : x; };
// the bounds of the intervals, separated by spaces or semicolons (a comma: a decimal mark)
const lireBornes = (texte) => String(texte || '').split(/[\s;]+/).filter(Boolean).map((x) => Number(x.replace(',', '.')));
const premier = (x) => (Array.isArray(x) ? x[0] : x);

function champsDistance(st, rendre, p) {
  const o = st.options;
  if (o.echantillon === undefined) {  // (a new model: the columns their names suggest)
    o.echantillon = deviner(p, MOTIFS.echantillon);
    o.effort = deviner(p, MOTIFS.effort, true);
    o.strate = deviner(p, MOTIFS.strate);
    o.aire = deviner(p, MOTIFS.aire, true);
    o.taille = deviner(p, /^(size|taille)$/i, true) ?? deviner(p, MOTIFS.taille, true);
    if (!Object.values(st.roles).includes('y')) {
      const v = p.variables.find((x) => MOTIFS.distance.test(x.nom) && !x.qualitative);
      if (v) st.roles[v.uid] = 'y';
    }
  }
  o.transect = premier(o.transect) || 'ligne';
  o.troncature = o.troncature && !Array.isArray(o.troncature) ? { type: premier(o.troncature.type) || 'aucune', valeur: premier(o.troncature.valeur) ?? null } : { type: 'aucune', valeur: null };
  o.unites = o.unites && !Array.isArray(o.unites) ? { distance: premier(o.unites.distance) || 'm', effort: premier(o.unites.effort) || 'km', aire: premier(o.unites.aire) || 'km2' }
    : { distance: 'm', effort: 'km', aire: 'km2' };
  const lignes = o.transect !== 'point';
  const ids = [].concat(p.identifiants || []).map((i) => i.nom);
  const options = (valeur, filtre, aucun) => [el('option', { value: '', selected: !valeur }, aucun || '—')]
    .concat(ids.length ? [el('optgroup', { label: t('vc_par_identifiant') }, ids.map((n) => el('option', { value: `id:${n}`, selected: `id:${n}` === valeur }, n)))] : [])
    .concat([el('optgroup', { label: t('vc_par_variable') }, p.variables.filter(filtre).map((v) => el('option', { value: `var:${v.uid}`, selected: `var:${v.uid}` === valeur }, v.nom)))]);
  const quantitatives = (valeur, changer, aucun, titre) => el('select', { title: titre || '', onchange: (e) => changer(e.target.value ? Number(e.target.value) : null) },
    [el('option', { value: '', selected: !valeur }, aucun)].concat(p.variables.filter((v) => !v.qualitative).map((v) => el('option', { value: v.uid, selected: v.uid === Number(premier(valeur)) }, v.nom))));
  const choisir = (valeurs, valeur, changer, libelle, titre) => el('select', { title: titre || '', onchange: (e) => changer(e.target.value) },
    valeurs.map((k) => el('option', { value: k, selected: String(k) === String(valeur) }, libelle(k))));
  const tr = o.troncature;
  const aj = premier(o.ajustements) ?? 'aic';
  if (o.intervalles_texte === undefined) o.intervalles_texte = liste(o.intervalles).map((x) => sig(x, 12)).join(' ');
  return [
    el('label', {}, t('distance_plan')), choisir(['ligne', 'point'], o.transect, (v) => { o.transect = v; rendre(); }, (k) => t(k === 'point' ? 'distance_points' : 'distance_lignes')),
    el('label', {}, t('distance_echantillon')), el('select', { title: t('distance_echantillon_aide'), onchange: (e) => { o.echantillon = specLue(e.target.value); } }, options(specCode(o.echantillon), () => true)),
    el('label', {}, lignes ? t('distance_effort_lignes') : t('distance_effort_points')),
    quantitatives(o.effort, (v) => { o.effort = v; }, lignes ? '—' : t('distance_un_passage'), t(lignes ? 'distance_effort_aide' : 'distance_passages_aide')),
    el('label', {}, t('distance_strate')), el('select', { title: t('distance_strate_aide'), onchange: (e) => { o.strate = specLue(e.target.value); } }, options(specCode(o.strate), () => true, t('distance_une_region'))),
    el('label', {}, t('distance_aire')), quantitatives(o.aire, (v) => { o.aire = v; }, t('distance_densite_seule_court'), t('distance_aire_aide')),
    el('label', {}, t('distance_taille')), quantitatives(o.taille, (v) => { o.taille = v; }, t('distance_sans_groupes'), t('distance_taille_aide')),
    el('label', {}, t('distance_troncature')), el('div', { class: 'ligne' },
      choisir(['aucune', 'distance', 'pourcentage'], tr.type, (v) => { tr.type = v; if (v === 'pourcentage' && !Number.isFinite(tr.valeur)) tr.valeur = 5; rendre(); }, (k) => t('distance_troncature_' + k), t('distance_troncature_aide')),
      tr.type === 'aucune' ? null : el('input', { type: 'number', step: 'any', min: 0, value: tr.valeur ?? '', style: 'width:7em', placeholder: tr.type === 'pourcentage' ? '5' : '',
        oninput: (e) => { tr.valeur = nombreLu(e.target.value); } }), tr.type === 'pourcentage' ? '%' : null,
      etiquette(t('distance_gauche')), el('input', { type: 'number', step: 'any', min: 0, value: premier(o.gauche) ?? '', placeholder: '0', style: 'width:6em',
        title: t('distance_gauche_aide'), oninput: (e) => { o.gauche = nombreLu(e.target.value); } })),
    el('label', {}, t('distance_intervalles')), el('input', { value: o.intervalles_texte, placeholder: t('distance_intervalles_exemple'), style: 'width:18em',
      title: t('distance_intervalles_aide'), oninput: (e) => { o.intervalles_texte = e.target.value; } }),
    el('label', {}, t('distance_cle')), choisir(['hn', 'hr', 'unif'], premier(o.cle) || 'hn', (v) => { o.cle = v; rendre(); }, libelleCle, t('distance_cle_aide')),
    el('label', {}, t('distance_ajustements')), el('div', { class: 'ligne' },
      choisir(['cos', 'herm', 'poly', 'aucune'], premier(o.serie) || 'cos', (v) => { o.serie = v; rendre(); }, libelleSerie, t('distance_serie_aide')),
      (premier(o.serie) || 'cos') === 'aucune' ? null : [
        choisir(['aic', 0, 1, 2, 3, 4, 5], aj, (v) => { o.ajustements = v === 'aic' ? 'aic' : Number(v); }, (k) => (k === 'aic' ? t('distance_par_aic') : t('distance_n_termes', { n: k })), t('distance_ajustements_aide')),
        (premier(o.cle) || 'hn') === 'unif' ? null : choisir(['largeur', 'echelle'], premier(o.echelle_ajustements) || 'largeur', (v) => { o.echelle_ajustements = v; },
          (k) => t('distance_echelle_' + k), t('distance_echelle_aide'))]),
    el('label', {}, t('distance_monotonie')), choisir(['stricte', 'faible', 'aucune'], premier(o.monotonie) || 'stricte', (v) => { o.monotonie = v; }, (k) => t('distance_monotonie_' + k), t('distance_monotonie_aide')),
    el('label', {}, t('distance_unites')), el('div', { class: 'ligne', title: t('distance_unites_aide') },
      etiquette(t('distance_unite_distance')), choisir(UNITES_LONGUEUR, o.unites.distance, (v) => { o.unites.distance = v; }, (k) => k),
      lignes ? [etiquette(t('distance_unite_effort')), choisir(UNITES_LONGUEUR, o.unites.effort, (v) => { o.unites.effort = v; }, (k) => k)] : null,
      etiquette(t('distance_unite_aire')), choisir(UNITES_AIRE, o.unites.aire, (v) => { o.unites.aire = v; }, (k) => SYMBOLES[k] || k),
      etiquette(t('distance_ou_facteur')), el('input', { type: 'number', step: 'any', min: 0, value: premier(o.facteur) ?? '', style: 'width:7em', title: t('distance_facteur_aide'),
        oninput: (e) => { o.facteur = nombreLu(e.target.value); } })),
    el('label', {}, ''), el('p', { class: 'note' }, t('distance_reglages_note')),
  ];
}
ANALYSES.DISTANCE = {
  libelle: () => t('type_distance'),
  note: () => t('distance_note'),
  roles: ['y'],
  echelle: 'none',
  vc: false,
  champs: champsDistance,
  valider: (st, cles, p) => {
    const o = st.options;
    const y = cles('y');
    const variable = (u) => p.variables.find((v) => v.uid === u) || {};
    if (y.length !== 1) return t('distance_y_requis');
    if (variable(y[0]).qualitative) return t('distance_y_quantitative', { nom: variable(y[0]).nom });
    if (!o.echantillon) return t('distance_echantillon_requis');
    if (o.strate && specCode(o.strate) === specCode(o.echantillon)) return t('distance_echantillon_strate');
    if (o.transect !== 'point' && !o.effort) return t('distance_effort_requis');
    const tr = o.troncature || {};
    if (tr.type === 'distance' && !(Number.isFinite(tr.valeur) && tr.valeur > 0)) return t('distance_troncature_invalide');
    if (tr.type === 'pourcentage' && !(Number.isFinite(tr.valeur) && tr.valeur >= 0 && tr.valeur < 100)) return t('distance_pourcentage_invalide');
    const g = premier(o.gauche);
    if (g !== null && g !== undefined && !(Number.isFinite(g) && g >= 0)) return t('distance_gauche_invalide');
    const f = premier(o.facteur);
    if (f !== null && f !== undefined && !(Number.isFinite(f) && f > 0)) return t('distance_facteur_invalide');
    const b = lireBornes(o.intervalles_texte);
    if (b.length && (b.length < 3 || b.some((x, i) => !Number.isFinite(x) || x < 0 || (i && x <= b[i - 1])))) return t('distance_intervalles_invalides');
    return '';
  },
  options: (st) => {
    const o = st.options;
    const tr = o.troncature || { type: 'aucune' };
    const f = premier(o.facteur);
    return { transect: o.transect || 'ligne', echantillon: o.echantillon, strate: o.strate || null, effort: o.effort || null, aire: o.aire || null, taille: o.taille || null,
      troncature: tr.type === 'aucune' ? { type: 'aucune' } : { type: tr.type, valeur: tr.valeur }, gauche: Number.isFinite(premier(o.gauche)) ? premier(o.gauche) : 0,
      cle: premier(o.cle) || 'hn', serie: premier(o.serie) || 'cos', ajustements: premier(o.ajustements) ?? 'aic', echelle_ajustements: premier(o.echelle_ajustements) || 'largeur',
      monotonie: premier(o.monotonie) || 'stricte', unites: o.unites, facteur: Number.isFinite(f) && f > 0 ? f : null,
      intervalles: lireBornes(o.intervalles_texte).length ? lireBornes(o.intervalles_texte) : null };
  },
};

// ------------------------------------------------------------ the report

const blocsResume = (m) => [
  { texte: `${t('distance_badge')} · ${t(point(m) ? 'distance_points' : 'distance_lignes')} · ${t('distance_detections')} ${m.n} · ${t('distance_troncature')} ${avecUnite(sig(m.troncature.w), uDist(m))} · ${m.modele_detection} · AIC ${fmt(m.AIC, 2)}` },
  { texte: phraseDetection(m) }, { texte: phraseTotal(m) },
  { entetes: entetesCandidats(), lignes: lignesCandidats(m) }, { texte: phraseSelection(m) },
  { entetes: ['', t('distance_statistique'), t('ddl'), 'p'], lignes: lignesAdequation(m) },
  ...notesDistance(m).map((x) => ({ texte: x }))];
const blocsDetection = (m) => {
  const k = m.adequation.khi2.classes;
  return [{ entetes: [t('distance_de'), t('distance_a'), t('distance_observe'), t('distance_attendu')],
    lignes: lignesDe(k, ['de', 'a', 'observe', 'attendu']).map((l) => [sig(l[0]), sig(l[1]), String(l[2]), fmt(l[3], 2)]) }];
};
const blocsDensite = (m) => [{ entetes: [t('distance_strate'), 'k', 'n', t('distance_rencontre'), `D${parAire(m)}`, 'CV', t('ic_95'), t('ddl')].concat(m.densite_seule ? [] : ['N', `N ${t('ic_95')}`]),
  lignes: lignesDensite(m.densite).map((l) => [etiquetteStrate(l.strate), String(l.k), String(l.n), sig(l.er), sig(l.D), pct(l.cv), `${sig(l.bas_D)} – ${sig(l.haut_D)}`, fmt(l.ddl, 1)]
    .concat(m.densite_seule ? [] : [sig(l.N), `${sig(l.bas_N)} – ${sig(l.haut_N)}`])) }, { texte: t(m.densite_seule ? 'distance_densite_note_seule' : 'distance_densite_note', { u: parAireCourt(m) }) }];
PARTIES_TYPES.DISTANCE = [
  { cle: 'resume', defaut: true, tableau: blocsResume },
  { cle: 'distance_detection', vue: 'distance_detection', defaut: true, tableau: blocsDetection },
  { cle: 'distance_qq', vue: 'distance_qq', defaut: true, si: (m) => !parIntervalles(m),
    tableau: (m) => [{ texte: t('distance_qq_note', { W: fmt(m.adequation.cvm.W, 4), p: pValeur(m.adequation.cvm.p) }) }] },
  { cle: 'distance_densite', vue: 'distance_densite', defaut: true, tableau: blocsDensite },
];

// ------------------------------------------------------------ registration

const resume = VUES.resume.rendre;
VUES.resume = { ...VUES.resume, rendre: (c, ctx, p, loc) => {
  const m = ctx.modele(p.modele);
  return estDistance(m) ? rendreResumeDistance(c, ctx, m) : resume(c, ctx, p, loc);
} };
Object.assign(VUES, {
  distance_detection: { rendre: rendreDetection, titre: (m) => `${m.nom} · ${t('distance_detection')}` },
  distance_qq: { rendre: rendreQQ, titre: (m) => `${m.nom} · ${t('distance_qq')}` },
  distance_densite: { rendre: rendreDensite, titre: (m) => `${m.nom} · ${t('distance_densite')}` },
});
Object.assign(ICONES_VUES, { distance_detection: 'chart-column', distance_qq: 'chart-scatter', distance_densite: 'table-2' });
VUES_TYPES.DISTANCE = (m) => ['resume', 'distance_detection'].concat(parIntervalles(m) ? [] : ['distance_qq'], ['distance_densite', 'liste_obs']);

ajouterTextes({
  type_distance: 'Échantillonnage par distances (densité, effectif)',
  distance_note: 'Des distances d’animaux détectés depuis des lignes (transects linéaires) ou des points (points d’écoute), comme le paquet Distance (ds et dht2). Une ligne par détection : sa distance (Y), son transect, l’effort de ce transect ; un transect sans détection reste une ligne, sa distance vide. Facultatifs : la strate, son aire, la taille du groupe.',
  distance_badge: 'Distance', distance_plan: 'Transects', distance_lignes: 'linéaires (lignes)', distance_points: 'points d’écoute (points)',
  distance_echantillon: 'Transect', distance_echantillon_aide: 'Le transect (la ligne ou le point) de chaque ligne du tableau : un identifiant ou une variable.',
  distance_effort_lignes: 'Effort (longueur)', distance_effort_points: 'Effort (passages)', distance_un_passage: '— (un passage par point)',
  distance_effort_aide: 'La longueur de chaque ligne, la même sur toutes les lignes du tableau d’un transect.',
  distance_passages_aide: 'Le nombre de passages à chaque point (vide : un passage chacun).',
  distance_strate: 'Strate', distance_strate_aide: 'La strate (la région) de chaque transect, facultative : la densité de chaque strate, puis le total.', distance_une_region: '— (une seule région)',
  distance_aire: 'Aire de la strate', distance_aire_aide: 'L’aire de la strate (la même sur toutes ses lignes) : sans elle, la densité seulement, pas l’effectif.',
  distance_densite_seule_court: '— (la densité seulement)', distance_taille: 'Taille des groupes', distance_sans_groupes: '— (des individus)',
  distance_taille_aide: 'Le nombre d’individus de chaque détection : la densité des groupes et celle des individus.',
  distance_troncature: 'Troncature', distance_troncature_aucune: 'la plus grande distance', distance_troncature_distance: 'à une distance', distance_troncature_pourcentage: 'des plus grandes distances',
  distance_troncature_aide: 'Les détections au-delà de la troncature w sortent du modèle (leur transect reste) : tronquer les 5 à 10 % des plus grandes distances est l’usage (Buckland et al., 2001).',
  distance_gauche: 'à gauche', distance_gauche_aide: 'Une troncature à gauche (facultative) : les distances plus petites sortent du modèle.',
  distance_intervalles: 'Intervalles', distance_intervalles_exemple: 'distances exactes (ou : 0 10 25 50 100)',
  distance_intervalles_aide: 'Des distances relevées par classes : les bornes des intervalles, croissantes, séparées par des espaces (comme ds(cutpoints = )). Chaque distance du tableau (la vraie, ou le milieu de sa classe) compte dans son intervalle ; la première borne fait la troncature à gauche, la dernière w, à la place de la troncature ci-dessus.',
  distance_intervalles_invalides: 'Les bornes des intervalles sont au moins trois distances croissantes, positives ou nulles, séparées par des espaces.',
  distance_n_intervalles: '{k} intervalles de {de} à {a}',
  distance_cle: 'Clé', distance_cle_hn: 'demi-normale', distance_cle_hr: 'hasard (hazard-rate)', distance_cle_unif: 'uniforme',
  distance_cle_aide: 'La forme de base de la fonction de détection g(x) : demi-normale exp(−x²/2σ²), hasard 1 − exp(−(x/σ)^−b), uniforme 1 (les termes d’ajustement la façonnent).',
  distance_ajustements: 'Termes d’ajustement', distance_serie_cos: 'cosinus', distance_serie_herm: 'polynômes d’Hermite', distance_serie_poly: 'polynômes simples', distance_serie_aucune: 'aucun',
  distance_serie_aide: 'Les termes qui ajustent la clé : cosinus (le défaut de Distance), polynômes d’Hermite ou polynômes simples.',
  distance_par_aic: 'nombre choisi par l’AIC', distance_n_termes: '{n} terme(s)',
  distance_ajustements_aide: 'Choisi par l’AIC comme ds() : la clé seule, puis un terme de plus tant que l’AIC baisse (5 au plus) ; ou un nombre fixé.',
  distance_echelle_largeur: 'à l’échelle de w', distance_echelle_echelle: 'à l’échelle de σ', distance_echelle_aide: 'Les distances des termes d’ajustement divisées par la troncature w (le défaut de ds) ou par σ de la clé.',
  distance_monotonie: 'Monotonie', distance_monotonie_stricte: 'stricte', distance_monotonie_faible: 'faible', distance_monotonie_aucune: 'aucune',
  distance_monotonie_aide: 'La contrainte de mrds sur une fonction de détection à termes d’ajustement, en 10 distances de 0 à w : stricte, g décroît (le défaut de ds) ; faible, g reste sous g(0) ; toujours g ≥ 0.',
  distance_unites: 'Unités', distance_unite_distance: 'distances', distance_unite_effort: 'effort', distance_unite_aire: 'aires', distance_ou_facteur: 'ou facteur',
  distance_unites_aide: 'Les unités des distances, de l’effort (des lignes) et des aires : la densité est donnée par unité d’aire.',
  distance_facteur_aide: 'Un facteur de conversion donné tel quel (convert_units de Distance), à la place des unités.',
  distance_reglages_note: 'La variance de p vient de la méthode delta, celle du taux de rencontre de l’estimateur R2 (lignes) ou P2 (points) de Fewster et al. (2009) ; l’intervalle de confiance est log-normal, avec les degrés de liberté de Satterthwaite, comme dht2.',
  distance_y_requis: 'Choisissez une variable Y : les distances.', distance_y_quantitative: 'Les distances (Y) sont une variable quantitative : {nom} ne l’est pas.',
  distance_echantillon_requis: 'Choisissez le transect de chaque ligne.', distance_echantillon_strate: 'Le transect et la strate doivent être deux colonnes différentes.',
  distance_effort_requis: 'Des transects linéaires demandent l’effort : la longueur de chaque ligne.', distance_troncature_invalide: 'La troncature est une distance positive.',
  distance_pourcentage_invalide: 'Le pourcentage des plus grandes distances tronquées va de 0 à 100 (exclu).', distance_gauche_invalide: 'La troncature à gauche est une distance positive ou nulle.',
  distance_facteur_invalide: 'Le facteur de conversion est un nombre positif.',
  distance_detections: 'Détections', distance_transects: 'Transects', distance_strates: 'Strates', distance_modele: 'Modèle', distance_esw: 'ESW', distance_edr: 'EDR',
  distance_densite_totale: 'Densité', distance_candidats: 'Fonctions de détection ajustées', distance_n_parametres: 'Paramètres', distance_retenu: 'Retenu', distance_non_converge: 'ne converge pas',
  distance_selection_aic: 'Comme ds() : la clé seule, puis un terme {serie} de plus tant que l’AIC baisse ; le premier qui le fait monter arrête la recherche.',
  distance_selection_max: 'Comme ds() : la clé seule, puis un terme {serie} de plus tant que l’AIC baisse, jusqu’au maximum de 5 termes.',
  distance_selection_convergence: 'Comme ds() : la clé seule, puis un terme {serie} de plus tant que l’AIC baisse ; un modèle qui ne converge pas a arrêté la recherche.',
  distance_selection_parametres: 'Comme ds() : la clé seule, puis un terme {serie} de plus tant que l’AIC baisse ; la recherche s’arrête quand les distances distinctes ne suffisent plus.',
  distance_selection_cle: 'La clé seule, sans terme d’ajustement.', distance_selection_fixe: 'Le nombre de termes d’ajustement donné.', distance_selection_echec: 'La clé seule ne converge pas.',
  distance_parametres: 'Paramètres de la fonction de détection', distance_parametre_b: 'ln_b : le logarithme de la forme b du hasard.',
  distance_parametre_sigma: 'ln_sigma : le logarithme de l’échelle σ de la clé.', distance_parametre_a: 'Puis chaque terme d’ajustement (série_ordre).',
  distance_parametres_note: 'Erreurs-types d’après la somme des produits des scores des distances, comme mrds.',
  distance_adequation: 'Adéquation', distance_statistique: 'Statistique', distance_cvm: 'Cramér–von Mises (W)', distance_khi2: 'khi² ({k} classes)',
  distance_adequation_note_intervalles: 'Un p faible : la fonction de détection ne décrit pas bien les distances. Le khi² compare les comptes de chaque intervalle à ceux qu’attend le modèle (des distances par intervalles : ni Cramér–von Mises ni diagramme quantile-quantile, comme ddf.gof).',
  distance_adequation_note: 'Un p faible : la fonction de détection ne décrit pas bien les distances. Cramér–von Mises compare la répartition ajustée des distances à leur répartition observée (le diagramme quantile-quantile) ; le khi², les comptes des classes égales de l’histogramme à ceux qu’attend le modèle.',
  distance_phrase_p: 'Probabilité moyenne de détection dans la troncature : p = {p} (erreur-type {se}, CV {cv}) ; {quoi} = {e} (erreur-type {se_e}).',
  distance_phrase_D: 'D = {D}{u} (CV {cv} ; IC à 95 % {bas} – {haut} ; {ddl} ddl).', distance_phrase_N: 'Effectif N = {N} (IC à 95 % {bas} – {haut}).',
  distance_phrase_groupes: 'Ces valeurs comptent les individus ; les groupes : D = {D}{u} (CV {cv}), de taille moyenne {s}.',
  distance_au_dela: '{n} détection(s) au-delà de la troncature ({w}), laissée(s) de côté (leur transect reste).', distance_en_deca: '{n} détection(s) en deçà de la troncature à gauche ({L}), laissée(s) de côté.',
  distance_contraint: 'La contrainte de monotonie agit : sans elle, la fonction de détection monterait ou deviendrait négative en l’un des 10 points.',
  distance_un_transect: 'Une strate d’un seul transect n’a pas de variance du taux de rencontre (comme dht2).',
  distance_densite_seule: 'Sans aire de strate : la densité seulement (le total est la moyenne des strates pondérée par leurs aires couvertes, comme dht2).',
  distance_facteur_donne: 'Les distances converties par le facteur {f} : les densités sont par unité d’aire de ce facteur.',
  distance_detection: 'Fonction de détection', distance_g: 'Fonction de détection g', distance_pdf: 'Densité des distances f', distance_histogramme: 'Histogramme des distances',
  distance_points_obs: 'distances observées', distance_points_obs_aide: 'Chaque détection sur la courbe, à sa distance ; un clic la choisit.',
  distance_g_note: 'Chaque barre : le compte de la classe sur celui qu’elle aurait si tous les animaux étaient détectés (comme plot.ds de mrds) ; la courbe, la fonction de détection ajustée, 1 en 0.',
  distance_g_note_points: 'Chaque barre : le compte de la classe (un anneau) sur celui qu’elle aurait si tous les animaux étaient détectés (comme plot.ds de mrds) ; la courbe, la fonction de détection ajustée, 1 en 0.',
  distance_pdf_note: 'Des points : la densité des distances f(r) = 2r g(r) / (w² p), qui monte d’abord avec la surface des anneaux, sur l’histogramme des distances (en densité).',
  distance_proba: 'Probabilité de détection', distance_densite_proba: 'Densité de probabilité', distance_distance: 'Distance', distance_observees: 'Détections',
  distance_de: 'De', distance_a: 'À', distance_observe: 'Observées', distance_attendu: 'Attendues', distance_contribution: '(O − A)² / A',
  distance_qq: 'Diagramme quantile-quantile', distance_qq_note: 'La répartition ajustée de chaque distance contre sa répartition empirique (comme qqplot.ddf de mrds) : des points sur la diagonale, une fonction de détection qui décrit bien les distances. Cramér–von Mises : W = {W}, p = {p}.',
  distance_empirique: 'Répartition empirique', distance_ajustee: 'Répartition ajustée',
  distance_densite: 'Densité et effectif', distance_densite_groupes: 'Densité des groupes', distance_individus: 'Individus', distance_groupes: 'Groupes',
  distance_densite_note: 'D par strate et au total ({u}), avec son intervalle de confiance à 95 % (log-normal, ddl de Satterthwaite) ; N = D × aire ; la part de la variance due au taux de rencontre, à la détection et à la taille des groupes.',
  distance_densite_note_seule: 'D par strate et au total ({u}), avec son intervalle de confiance à 95 % (log-normal, ddl de Satterthwaite) ; sans aire de strate, pas d’effectif. La part de la variance due au taux de rencontre, à la détection et à la taille des groupes.',
  distance_par_unite: 'par unité d’aire', distance_total: 'Total', distance_region: 'Région',
  distance_aire_couverte: 'Aire couverte', distance_effort: 'Effort', distance_passages: 'Passages', distance_rencontre: 'Taux de rencontre', distance_taille_moyenne: 'Taille moyenne',
  distance_part_rencontre: '% var. rencontre', distance_part_detection: '% var. détection', distance_part_groupes: '% var. groupes',
}, {
  type_distance: 'Distance sampling (density, abundance)',
  distance_note: 'Distances of animals detected from lines (line transects) or points (point transects), as the Distance package does (ds and dht2). One row per detection: its distance (Y), its transect, that transect’s effort; a transect without detections stays a row, its distance empty. Optional: the stratum, its area, the group size.',
  distance_badge: 'Distance', distance_plan: 'Transects', distance_lignes: 'line transects', distance_points: 'point transects',
  distance_echantillon: 'Transect', distance_echantillon_aide: 'The transect (the line or the point) of each row of the table: an ID or a variable.',
  distance_effort_lignes: 'Effort (length)', distance_effort_points: 'Effort (visits)', distance_un_passage: '— (one visit per point)',
  distance_effort_aide: 'The length of each line, the same on every row of a transect.',
  distance_passages_aide: 'The number of visits to each point (empty: one visit each).',
  distance_strate: 'Stratum', distance_strate_aide: 'The stratum (the region) of each transect, optional: the density of each stratum, then the total.', distance_une_region: '— (a single region)',
  distance_aire: 'Stratum area', distance_aire_aide: 'The area of the stratum (the same on all its rows): without it, the density only, no abundance.',
  distance_densite_seule_court: '— (density only)', distance_taille: 'Group size', distance_sans_groupes: '— (individuals)',
  distance_taille_aide: 'The number of individuals of each detection: the density of groups and that of individuals.',
  distance_troncature: 'Truncation', distance_troncature_aucune: 'the largest distance', distance_troncature_distance: 'at a distance', distance_troncature_pourcentage: 'of the largest distances',
  distance_troncature_aide: 'Detections beyond the truncation w leave the model (their transect stays): truncating the 5 to 10% largest distances is usual (Buckland et al., 2001).',
  distance_gauche: 'left', distance_gauche_aide: 'An optional left truncation: smaller distances leave the model.',
  distance_intervalles: 'Intervals', distance_intervalles_exemple: 'exact distances (or: 0 10 25 50 100)',
  distance_intervalles_aide: 'Distances recorded in classes: the bounds of the intervals, increasing, separated by spaces (as ds(cutpoints = )). Each distance of the table (the true one, or its class’s midpoint) counts in its interval; the first bound is the left truncation, the last w, instead of the truncation above.',
  distance_intervalles_invalides: 'The bounds of the intervals are at least three increasing distances, positive or zero, separated by spaces.',
  distance_n_intervalles: '{k} intervals from {de} to {a}',
  distance_cle: 'Key', distance_cle_hn: 'half-normal', distance_cle_hr: 'hazard-rate', distance_cle_unif: 'uniform',
  distance_cle_aide: 'The basic shape of the detection function g(x): half-normal exp(−x²/2σ²), hazard-rate 1 − exp(−(x/σ)^−b), uniform 1 (the adjustment terms shape it).',
  distance_ajustements: 'Adjustment terms', distance_serie_cos: 'cosine', distance_serie_herm: 'Hermite polynomial', distance_serie_poly: 'simple polynomial', distance_serie_aucune: 'none',
  distance_serie_aide: 'The terms that adjust the key: cosine (Distance’s default), Hermite polynomials or simple polynomials.',
  distance_par_aic: 'number chosen by AIC', distance_n_termes: '{n} term(s)',
  distance_ajustements_aide: 'Chosen by AIC as ds() does: the key alone, then one more term while the AIC falls (5 at most); or a fixed number.',
  distance_echelle_largeur: 'scaled by w', distance_echelle_echelle: 'scaled by σ', distance_echelle_aide: 'The distances of the adjustment terms divided by the truncation w (ds’s default) or by the key’s σ.',
  distance_monotonie: 'Monotonicity', distance_monotonie_stricte: 'strict', distance_monotonie_faible: 'weak', distance_monotonie_aucune: 'none',
  distance_monotonie_aide: 'mrds’s constraint on a detection function with adjustment terms, at 10 distances from 0 to w: strict, g decreases (ds’s default); weak, g stays below g(0); always g ≥ 0.',
  distance_unites: 'Units', distance_unite_distance: 'distances', distance_unite_effort: 'effort', distance_unite_aire: 'areas', distance_ou_facteur: 'or factor',
  distance_unites_aide: 'The units of the distances, of the effort (lines) and of the areas: the density is given per unit of area.',
  distance_facteur_aide: 'A conversion factor given as it is (Distance’s convert_units), instead of the units.',
  distance_reglages_note: 'The variance of p comes from the delta method, that of the encounter rate from the R2 (lines) or P2 (points) estimator of Fewster et al. (2009); the confidence interval is log-normal, with Satterthwaite degrees of freedom, as dht2.',
  distance_y_requis: 'Choose a Y variable: the distances.', distance_y_quantitative: 'The distances (Y) are a quantitative variable: {nom} isn’t one.',
  distance_echantillon_requis: 'Choose the transect of each row.', distance_echantillon_strate: 'The transect and the stratum must be two different columns.',
  distance_effort_requis: 'Line transects need the effort: the length of each line.', distance_troncature_invalide: 'The truncation is a positive distance.',
  distance_pourcentage_invalide: 'The percentage of the largest distances truncated goes from 0 to 100 (excluded).', distance_gauche_invalide: 'The left truncation is a distance, positive or zero.',
  distance_facteur_invalide: 'The conversion factor is a positive number.',
  distance_detections: 'Detections', distance_transects: 'Transects', distance_strates: 'Strata', distance_modele: 'Model', distance_esw: 'ESW', distance_edr: 'EDR',
  distance_densite_totale: 'Density', distance_candidats: 'Detection functions fitted', distance_n_parametres: 'Parameters', distance_retenu: 'Retained', distance_non_converge: 'doesn’t converge',
  distance_selection_aic: 'As ds(): the key alone, then one more {serie} term while the AIC falls; the first that raises it stops the search.',
  distance_selection_max: 'As ds(): the key alone, then one more {serie} term while the AIC falls, up to the maximum of 5 terms.',
  distance_selection_convergence: 'As ds(): the key alone, then one more {serie} term while the AIC falls; a model that doesn’t converge stopped the search.',
  distance_selection_parametres: 'As ds(): the key alone, then one more {serie} term while the AIC falls; the search stops when the distinct distances no longer suffice.',
  distance_selection_cle: 'The key alone, without adjustment terms.', distance_selection_fixe: 'The number of adjustment terms given.', distance_selection_echec: 'The key alone doesn’t converge.',
  distance_parametres: 'Parameters of the detection function', distance_parametre_b: 'ln_b: the logarithm of the hazard-rate’s shape b.',
  distance_parametre_sigma: 'ln_sigma: the logarithm of the key’s scale σ.', distance_parametre_a: 'Then each adjustment term (series_order).',
  distance_parametres_note: 'Standard errors from the outer product of the distances’ scores, as mrds.',
  distance_adequation: 'Goodness of fit', distance_statistique: 'Statistic', distance_cvm: 'Cramér–von Mises (W)', distance_khi2: 'chi-square ({k} classes)',
  distance_adequation_note_intervalles: 'A small p: the detection function doesn’t describe the distances well. The chi-square compares the counts of each interval with those the model expects (distances in intervals: neither Cramér–von Mises nor QQ plot, as ddf.gof).',
  distance_adequation_note: 'A small p: the detection function doesn’t describe the distances well. Cramér–von Mises compares the fitted distribution of the distances with their observed one (the QQ plot); the chi-square, the counts of the histogram’s equal classes with those the model expects.',
  distance_phrase_p: 'Average probability of detection within the truncation: p = {p} (standard error {se}, CV {cv}); {quoi} = {e} (standard error {se_e}).',
  distance_phrase_D: 'D = {D}{u} (CV {cv}; 95% CI {bas} – {haut}; {ddl} df).', distance_phrase_N: 'Abundance N = {N} (95% CI {bas} – {haut}).',
  distance_phrase_groupes: 'These values count individuals; groups: D = {D}{u} (CV {cv}), of mean size {s}.',
  distance_au_dela: '{n} detection(s) beyond the truncation ({w}), left out (their transect stays).', distance_en_deca: '{n} detection(s) below the left truncation ({L}), left out.',
  distance_contraint: 'The monotonicity constraint is active: without it, the detection function would rise or turn negative at one of the 10 points.',
  distance_un_transect: 'A stratum with a single transect has no encounter rate variance (as dht2).',
  distance_densite_seule: 'Without stratum areas: the density only (the total is the strata’s mean weighted by their covered areas, as dht2).',
  distance_facteur_donne: 'The distances converted by the factor {f}: the densities are per unit of area of that factor.',
  distance_detection: 'Detection function', distance_g: 'Detection function g', distance_pdf: 'Density of distances f', distance_histogramme: 'Histogram of distances',
  distance_points_obs: 'observed distances', distance_points_obs_aide: 'Each detection on the curve, at its distance; a click selects it.',
  distance_g_note: 'Each bar: the count of the class over the count it would have if every animal were detected (as mrds’s plot.ds); the curve, the fitted detection function, 1 at 0.',
  distance_g_note_points: 'Each bar: the count of the class (a ring) over the count it would have if every animal were detected (as mrds’s plot.ds); the curve, the fitted detection function, 1 at 0.',
  distance_pdf_note: 'Points: the density of the distances f(r) = 2r g(r) / (w² p), which first rises with the area of the rings, over the histogram of the distances (as a density).',
  distance_proba: 'Detection probability', distance_densite_proba: 'Probability density', distance_distance: 'Distance', distance_observees: 'Detections',
  distance_de: 'From', distance_a: 'To', distance_observe: 'Observed', distance_attendu: 'Expected', distance_contribution: '(O − E)² / E',
  distance_qq: 'QQ plot', distance_qq_note: 'The fitted distribution of each distance against its empirical distribution (as mrds’s qqplot.ddf): points on the diagonal, a detection function that describes the distances well. Cramér–von Mises: W = {W}, p = {p}.',
  distance_empirique: 'Empirical distribution', distance_ajustee: 'Fitted distribution',
  distance_densite: 'Density and abundance', distance_densite_groupes: 'Density of groups', distance_individus: 'Individuals', distance_groupes: 'Groups',
  distance_densite_note: 'D by stratum and overall ({u}), with its 95% confidence interval (log-normal, Satterthwaite df); N = D × area; the share of the variance due to the encounter rate, to detection and to group size.',
  distance_densite_note_seule: 'D by stratum and overall ({u}), with its 95% confidence interval (log-normal, Satterthwaite df); without stratum areas, no abundance. The share of the variance due to the encounter rate, to detection and to group size.',
  distance_par_unite: 'per unit of area', distance_total: 'Total', distance_region: 'Region',
  distance_aire_couverte: 'Covered area', distance_effort: 'Effort', distance_passages: 'Visits', distance_rencontre: 'Encounter rate', distance_taille_moyenne: 'Mean size',
  distance_part_rencontre: '% var. encounter', distance_part_detection: '% var. detection', distance_part_groupes: '% var. groups',
});
