// Model report (Fichier › Rapport du modèle): the parts chosen in the dialog, with the plots as
// they look in their panels (colours, labels, titles), as a PDF (printed by the desktop app,
// or by the browser) or a Word document (.docx, written here).
import { t, langueActuelle } from './i18n.js';
import { el, fmt, dialogue } from './ui.js';
import { VUES, resumeSeuils, notePhi, notesTermesRetires, noteSansCategorie, notesPentes, lignesSelectionAIC, groupesSelectionAIC, nombreParametres, notesPuissance, resultatSimulation, libelleUnites,
  notesMoyennes, titreMoyenne, titrePaire, libellePaire } from './panneaux.js';
import { demander } from './api.js';

const LARGEUR = 900;   // plot images, in pixels (drawn twice as fine)
const HAUTEUR = 580;

// The parts a report can hold, in their order. `vue`: a plot drawn by that view;
// `tableau`: a table built from the model's results.
const PARTIES = [
  { cle: 'resume', defaut: true, tableau: tableauComposantes },
  { cle: 'scores', vue: 'scores', defaut: true },
  { cle: 'loadings', vue: 'loadings', defaut: true },
  { cle: 'biplot', vue: 'biplot', min: 2 },
  { cle: 'splot', vue: 'splot', opls: true, defaut: true },
  { cle: 'dmodx', vue: 'dmodx', defaut: true },
  { cle: 't2', vue: 't2' },
  { cle: 'vip', vue: 'vip', pls: true, defaut: true },
  { cle: 'coefficients', vue: 'coefficients', pls: true, defaut: true },
  { cle: 'obspred', vue: 'obspred', pls: true, defaut: true },
  { cle: 'cvanova', pls: true, defaut: true, tableau: tableauCVANOVA },
  { cle: 'permutations', vue: 'permutations', pls: true },
  { cle: 'classification', vue: 'classification', plsda: true, defaut: true, tableau: tableauClassification },
  { cle: 'liste_vars', tableau: tableauVariables },
];

// Another analysis's parts (RDA, LDA...), by its type: registered by its module (vues_*.js).
export const PARTIES_TYPES = {};

// A regression's parts (moteur/R/regression.R): its summary and coefficients as tables, its plots
// as in their panels. `variantes`: one plot per setting of the view (each term's effect; the
// residuals against the fitted values, then on a normal Q-Q plot, and so the quantile residuals of
// a model of counts, presence-absence or positive values). `si`: only for the models it accepts
// (the thresholds: an additive model's curves; the trend power: a linear or linear mixed model; the
// adjusted means: a model with a qualitative variable).
const avecCourbes = (m) => [].concat((m.courbes && m.courbes.terme) || []).length > 0;
const PARTIES_REGRESSION = [
  { cle: 'resume', defaut: true, tableau: tableauRegression },
  { cle: 'coefficients', vue: 'coefficients', defaut: true, tableau: tableauCoefficientsRegression },
  { cle: 'effets', vue: 'effets', defaut: true, tableau: tableauPentes,
    variantes: async (m) => [].concat((await demander('effets', { modele: m.id })).effets || []).map((e) => ({ terme: e.terme, seuils: false })) },
  { cle: 'seuils', vue: 'effets', defaut: true, si: avecCourbes, tableau: resumesSeuils,
    variantes: async (m) => (await variantesSeuils(m)).map((v) => ({ terme: v.terme, seuils: true, ...(v.categorie ? { categorie: v.categorie } : {}) })) },
  { cle: 'moyennes', vue: 'moyennes', si: (m) => [].concat(m.qualitatives || []).length > 0, tableau: tableauMoyennes },
  { cle: 'obspred', vue: 'obspred', defaut: true },
  { cle: 'residus', vue: 'residus', defaut: true, tableau: (m) => (m.residus_quantiles ? [{ texte: t('residus_note_quantiles') }] : []),
    variantes: async (m) => (m.residus_quantiles ? ['standard', 'quantiles'] : ['standard']).flatMap((residus) => [{ mode: 'ajustes', residus }, { mode: 'qq', residus }]) },
  { cle: 'acf', vue: 'acf' },  // (the time and groups of its panel, else CAR1's or those it guesses)
  { cle: 'apports', vue: 'apports' },
  { cle: 'selection_aic', vue: 'selection_aic', tableau: tableauSelectionAIC },
  { cle: 'puissance', vue: 'puissance', si: (m) => ['LM', 'LMM'].includes(m.type), tableau: tableauPuissance },
];

export function dialogueRapport(app, id) {
  const m = app.modele(id);
  if (!m) return;
  const pls = m.type !== 'PCA';
  const parties = PARTIES_TYPES[m.type] ? PARTIES_TYPES[m.type].filter((x) => !x.si || x.si(m)) : m.regression ? PARTIES_REGRESSION.filter((x) => !x.si || x.si(m))
    : PARTIES.filter((x) => (!x.pls || pls) && (!x.plsda || /-DA$/.test(m.type)) && (!x.min || m.A >= x.min) && (!x.opls || m.opls));
  const choix = new Set(parties.filter((x) => x.defaut).map((x) => x.cle));
  const st = { titre: `${m.nom}${m.titre ? ' — ' + m.titre : ''}`, commentaire: '', format: 'pdf' };
  const contenu = el('div', { class: 'dialogue-rapport' },
    el('div', { class: 'grille-champs' },
      el('label', {}, t('titre_graphe')), el('input', { value: st.titre, style: 'width:28em', oninput: (e) => { st.titre = e.target.value; } }),
      el('label', {}, t('commentaire')), el('textarea', { rows: 3, style: 'width:28em', oninput: (e) => { st.commentaire = e.target.value; } }),
      el('label', {}, t('format_image')), el('div', { class: 'ligne' }, [['pdf', 'PDF'], ['docx', 'Word (.docx)']].map(([k, n]) =>
        el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'format-rapport', checked: k === st.format, onchange: () => { st.format = k; } }), n)))),
    el('h3', {}, t('parties')),
    el('div', { class: 'parties-rapport' }, parties.map((x) => el('label', { class: 'champ' },
      el('input', { type: 'checkbox', checked: choix.has(x.cle), onchange: (e) => { if (e.target.checked) choix.add(x.cle); else choix.delete(x.cle); } }),
      t(x.cle)))),
    el('p', { class: 'note' }, t('rapport_note')));
  dialogue({ titre: `${t('rapport')} — ${m.nom}`, contenu, boutons: [
    { libelle: t('annuler'), action: () => true },
    { libelle: t('creer_rapport'), principal: true, action: () => {
      if (!choix.size) return false;
      creerRapport(app, m, parties.filter((x) => choix.has(x.cle)), st);
      return true;
    } }] });
}

async function creerRapport(app, m, parties, st) {
  const nomFichier = (st.titre || m.nom).replace(/[^\p{L}\p{N} _.()-]+/gu, '_');
  let chemin = null;
  if (app.bureau) {
    chemin = await app.bureau.enregistrerSous(`${nomFichier}.${st.format}`,
      [st.format === 'pdf' ? { name: 'PDF', extensions: ['pdf'] } : { name: 'Word', extensions: ['docx'] }]);
    if (!chemin) return;
  }
  await app.occupe(async () => {
    const blocs = await contenuRapport(app, m, parties, st);
    if (st.format === 'pdf') {
      const html = pageHTML(blocs, st.titre);
      if (app.bureau) await app.bureau.imprimerPDF(html, chemin);
      else imprimerNavigateur(html);
    } else {
      const octets = docx(blocs);
      if (app.bureau) await app.bureau.ecrireFichier(chemin, octets);
      else telechargerOctets(octets, `${nomFichier}.docx`);
    }
  });
  if (chemin) {
    app.statut(`${t('rapport_ecrit')} ${chemin}`);
    app.bureau.ouvrirChemin(chemin);
  }
}

// ------------------------------------------------------------ content

// The report as a list of blocks: { titre, niveau } headings, { texte } paragraphs,
// { entetes, lignes } tables, { image (PNG data URL), largeur, hauteur } plots.
async function contenuRapport(app, m, parties, st) {
  const p = app.projet(m.projet) || {};
  const date = new Date().toLocaleString(langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB', { dateStyle: 'long', timeStyle: 'short' });
  const blocs = [{ titre: st.titre || m.nom, niveau: 0 },
    { texte: `${t('projet_rapport')} : ${p.nom || ''} · ${t('genere_le')} ${date}` }];
  // the condition that chose the model's observations (moteur/R/condition.R)
  if (m.reglages && m.reglages.condition) blocs.push({ texte: t('condition_modele', { condition: m.reglages.condition }) });
  if (st.commentaire.trim()) st.commentaire.trim().split(/\n+/).forEach((x) => blocs.push({ texte: x }));
  if ((m.note || '').trim()) {
    blocs.push({ titre: t('notes'), niveau: 1 });
    m.note.trim().split(/\n+/).forEach((x) => blocs.push({ texte: x }));
  }
  for (const x of parties) {
    blocs.push({ titre: t(x.cle), niveau: 1 });
    if (x.tableau) blocs.push(...(await x.tableau(m)));  // (a table can ask the engine first: thresholds, tests computed on demand)
    if (x.vue) {
      for (const variante of x.variantes ? await x.variantes(m) : [{}]) {
        const image = await imageVue(app, x.vue, m, variante);
        blocs.push(image ? { image, largeur: LARGEUR, hauteur: HAUTEUR } : { texte: t('rapport_graphique_absent') });
      }
    }
  }
  return blocs;
}

// A view drawn off screen with the settings of its open panel, if any (and `variante`'s), then
// turned into a PNG.
async function imageVue(app, vue, m, variante = {}) {
  const hote = el('div', { class: 'panneau', style: `position:fixed;left:-30000px;top:0;width:${LARGEUR}px;height:${HAUTEUR + 60}px` });
  document.body.append(hote);
  let graphe = null;
  const loc = { ...(app.locVue(vue, m.id) || {}), ...variante };
  try {
    await VUES[vue].rendre(hote, app.contexteHorsEcran(loc, (div) => { graphe = div; }), { modele: m.id, projet: m.projet }, loc);
    if (!graphe) return null;
    return await window.Plotly.toImage(graphe, { format: 'png', width: LARGEUR, height: HAUTEUR, scale: 2 });
  } catch (e) {
    console.warn(`rapport : ${vue}`, e);  // the part is left out of the report
    return null;
  } finally {
    if (graphe) window.Plotly.purge(graphe);
    hote.remove();
  }
}

const nombre = (v, d = 4) => (v === null || v === undefined ? '' : typeof v === 'number' ? fmt(v, d) : String(v));
const valeurP = (x) => (x === null || x === undefined ? '' : x === 0 ? '< 1e-16' : x < 0.001 ? Number(x).toExponential(2) : fmt(x, 4));

function tableauComposantes(m) {
  const c = m.composantes;
  const pls = m.type !== 'PCA';
  const cols = [['R2X', 'R2X'], ['R2Xcum', 'R2X(cum)']].concat(pls ? [['R2Y', 'R2Y'], ['R2Ycum', 'R2Y(cum)']] : [])
    .concat([['Q2', 'Q2'], ['Q2cum', 'Q2(cum)'], ['valeur_propre', t('valeur_propre')]]);
  const infos = [`${m.type} · A = ${m.A} · N = ${m.N} · K = ${m.K} · ${t('groupes_vc')} ${m.G}${m.vc_par ? ` (${m.vc_par})` : ''}`
    + (m.validation ? ` · ${t('validation_vc')} : ${t('validation_' + m.validation)}` : '')];
  if (Number.isFinite(m.gain) && Number.isFinite(m.q2_vide)) infos.push(`${t('gain_vide')} : ${fmt(m.gain, 3)} (${t('q2_vide')} ${fmt(m.q2_vide, 3)})`);
  if (m.hierarchique && m.hierarchique.q2cum) {
    const h = m.hierarchique;
    infos.push(`${t('q2_honnete')} : ${fmt([].concat(h.q2cum).pop(), 3)}` + (Number.isFinite(h.gain) ? ` (${t('gain_honnete')} ${fmt(h.gain, 3)})` : ''));
  }
  if (pls && m.noms_y.length) infos.push(`Y : ${[].concat(m.noms_y).join(', ')}`);
  if (pls && m.RMSEE) infos.push(Object.keys(m.RMSEE).map((y) => `${y} : RMSEE ${fmt(m.RMSEE[y], 4)}, RMSEcv ${fmt(m.RMSEcv[y], 4)}`).join(' · '));
  if (m.classification) infos.push(`${t('bien_classes_vc')} : ${fmt(100 * m.classification.vc.global, 1)} %`);
  return [...infos.map((texte) => ({ texte })),
    { entetes: [t('comp'), ...cols.map(([, n]) => n)], lignes: c.composante.map((a, i) => [String(a), ...cols.map(([k]) => nombre(c[k][i], 3))]) }];
}

// A regression's summary: its kind, distribution and fit statistics, then its random effects,
// curves and notes, as in its Summary panel.
function tableauRegression(m) {
  const q = m.qualite || {};
  const y = m.noms_y[0];
  const infos = [`${t('type_' + m.type.toLowerCase())} · N = ${m.N} · ${t('nb_termes')} ${m.K} · ${t('groupes_vc')} ${m.G}${m.vc_par ? ` (${m.vc_par})` : ''}`];
  if (['GLM', 'GAM'].includes(m.type) && m.famille) {
    infos.push([`${t('loi')} : ${t('famille_' + m.famille)}`, `${t('lien')} : ${m.lien}`,
      m.parametre_loi ? `${m.parametre_loi.nom === 'theta' ? 'θ' : 'p'} = ${fmt(m.parametre_loi.valeur, 3)}` : '',
      m.decalage ? `${t('decalage')} : log(${m.decalage})` : ''].filter(Boolean).join(' · '));
  }
  const stats = (q.marginal !== undefined ? [[t('r2_marginal'), q.marginal], [t('r2_conditionnel'), q.conditionnel]]
    : q.deviance_expliquee !== undefined ? [[t('r2_ajuste'), q.r2_ajuste], [t('deviance_expliquee'), q.deviance_expliquee === null ? null : `${fmt(100 * q.deviance_expliquee, 1)} %`]]
      : [['R2', q.r2], [t('r2_ajuste'), q.r2_ajuste]])
    .concat([['Q2', q.Q2], ['RMSEE', m.RMSEE && m.RMSEE[y]], ['RMSEcv', q.RMSEcv], ['AIC', q.AIC], ['AICc', q.AICc], ['BIC', q.BIC], ['logLik', q.logLik], [t('parametres'), q.parametres]]);
  const blocs = [...infos.map((texte) => ({ texte })), { entetes: stats.map(([n]) => n), lignes: [stats.map(([, v]) => nombre(v, 3))] }];
  const v = m.variances;
  if (v) {
    const variances = [].concat(v.variance);
    // (with random slopes: a row per term of each level, and each slope's correlation with the intercept)
    const pentes = v.terme !== undefined;
    const correlations = pentes && [].concat(v.correlation).some((r) => r !== null && r !== undefined);
    blocs.push({ entetes: [t('effet_aleatoire')].concat(pentes ? [t('terme')] : [], [t('variance'), t('ecart_type')], correlations ? [t('correlation_ordonnee')] : []),
      lignes: [].concat(v.composante).map((c, i) => [c].concat(pentes ? [[].concat(v.terme)[i]] : [], [nombre(variances[i]), nombre(Math.sqrt(variances[i]))],
        correlations ? [nombre([].concat(v.correlation)[i], 3)] : [])) });
  }
  const cb = m.courbes;
  if (cb) {
    const cv = cb.concurvite !== undefined;
    const categories = cb.niveau !== undefined;  // (a curve per category: a row per category)
    blocs.push({ entetes: [t('courbe')].concat(categories ? [t('categorie')] : [], ['edf', t('ddl'), t('statistique'), 'p'], cv ? [t('concurvite')] : []),
      lignes: [].concat(cb.terme).map((n, i) => [n].concat(categories ? [[].concat(cb.niveau)[i] === null ? '' : `${[].concat(cb.par)[i]} ${[].concat(cb.niveau)[i]}`] : [],
        [nombre([].concat(cb.edf)[i], 2), nombre([].concat(cb.ref_ddl)[i], 2), nombre([].concat(cb.statistique)[i], 2), valeurP([].concat(cb.p)[i])],
        cv ? [nombre([].concat(cb.concurvite)[i], 2)] : [])) });
  }
  return blocs.concat([...notesPentes(m), notePhi(m),
    m.dispersion > 1.5 ? t('surdispersion', { d: fmt(m.dispersion, 1) }) : '',
    ...notesTermesRetires(m), noteSansCategorie(m)].filter(Boolean).map((texte) => ({ texte })));
}

// The slope per category of each term crossed with categories, as under its plot in the Effects
// view (regression.R: effets_par_niveau).
async function tableauPentes(m) {
  const effets = [].concat((await demander('effets', { modele: m.id })).effets || []).filter((e) => e.niveaux && !e.courbe);
  return effets.map((e) => ({ entetes: [e.par, t('pente_niveau', { terme: e.terme }), t('ic_bas'), t('ic_haut')],
    lignes: [].concat(e.niveaux).map((n) => [n.niveau, nombre(n.pente), nombre(n.pente_bas), nombre(n.pente_haut)]) }));
}

// The thresholds a regression has: each curve's, and each category's for a curve with one per
// category (a curve per category, or a curve crossed with categories: regression.R, courbes_seuils),
// as the Effects view lists its curves.
async function variantesSeuils(m) {
  const effets = [].concat((await demander('effets', { modele: m.id })).effets || []).filter((e) => e.courbe);
  return effets.flatMap((e) => (e.niveaux ? [].concat(e.niveaux).map((n) => ({ terme: e.terme, categorie: n.niveau, par: e.par })) : [{ terme: e.terme }]));
}

// Each curve's thresholds in words, as above its plot in the Effects view (regression.R:
// seuils_courbe, computed once per curve and category and kept by the engine).
async function resumesSeuils(m) {
  const blocs = [];
  for (const v of await variantesSeuils(m)) {
    const s = await demander('seuils', { modele: m.id, terme: v.terme, ...(v.categorie ? { categorie: v.categorie } : {}) });
    blocs.push({ texte: `${v.terme}${v.categorie ? ` (${v.par} ${v.categorie})` : ''} — ${resumeSeuils(s)}` });
  }
  return blocs;
}

// The AIC selection, as in its panel (regression.R: selection_aic, computed once and kept by the
// engine): the suggested models in words, then the table of the best models.
async function tableauSelectionAIC(m) {
  const s = await demander('selection_aic', { modele: m.id });
  // (the groups that carry a random slope, in every model)
  const gardes = [].concat(s.gardes ?? []);
  const noteGardes = gardes.length ? t('selection_aic_gardes', { groupes: gardes.join(', ') }) : '';
  if (!s.K) return [{ texte: [t('selection_aic_vide'), noteGardes].filter(Boolean).join(' ') }];
  const col = (k) => [].concat(s.modeles[k]);
  const groupes = [].concat(s.groupes);
  const nom = (i) => groupesSelectionAIC(s, i).join(' + ') || (gardes.length ? t('aic_modele_gardes', { groupes: gardes.join(' + ') }) : t('aic_modele_nul'));
  const pa = s.parcimonieux - 1;
  const retires = s.chemin ? [].concat(s.chemin.retire).slice(1) : [];
  return [
    { texte: [t('selection_aic_note', { n: s.N }), noteGardes, s.methode === 'toutes' ? t('selection_aic_toutes', { n: col('AICc').length })
      : t('selection_aic_descendante', { k: s.K, limite: s.limite, n: col('AICc').length, retires: retires.length ? retires.join(', ') : t('aic_aucun_retire') })].filter(Boolean).join(' ') },
    { texte: `${t('selection_aic_meilleur')} ${nom(0)} (k ${nombreParametres(col('k')[0])}, AICc ${nombre(col('AICc')[0], 2)}, ${t('poids_akaike')} ${nombre(col('poids')[0], 2)})` },
    { texte: pa > 0 ? `${t('selection_aic_parcimonieux')} ${nom(pa)} (k ${nombreParametres(col('k')[pa])}, ΔAICc ${nombre(col('delta')[pa], 2)})` : t('selection_aic_aussi') },
    { entetes: [t('aic_rang'), ...groupes, 'k', 'logLik', 'AICc', 'ΔAICc', t('poids_akaike'), t('aic_poids_cumule')],
      lignes: lignesSelectionAIC(s).map((i) => [String(i + 1), ...groupes.map((_, j) => ([].concat(s.contient[j])[i] === 1 ? '●' : '')), nombreParametres(col('k')[i]),
        nombre(col('logLik')[i], 2), nombre(col('AICc')[i], 2), nombre(col('delta')[i], 2), nombre(col('poids')[i], 3), nombre(col('poids_cumule')[i], 3)]) }];
}

// The trend power, as in its panel (moteur/R/puissance.R: computed and kept by the engine, the last
// settings of its view, otherwise the defaults): the sentence, what it took from the model, the
// simulation's result if it was run, and the power for each number of years and of units.
async function tableauPuissance(m) {
  let r;
  try { r = await demander('puissance_tendance', { modele: m.id }); } catch (e) { return [{ texte: e.message }]; }
  const tab = r.tableau;
  const col = (k) => [].concat(tab[k]);
  const puissance = new Map(col('puissance').map((p, i) => [`${col('annees')[i]}:${col('unites')[i]}`, p]));
  const unites = [].concat(r.reglages.unites);
  return [{ texte: r.phrase }, { texte: notesPuissance(r) }].concat(r.simulation ? [{ texte: resultatSimulation(r, r.simulation) }] : [],
    [{ entetes: [t('puissance_annees'), ...unites.map((u) => libelleUnites(r, u))],
      lignes: [...new Set(col('annees'))].map((a) => [String(a), ...unites.map((u) => nombre(puissance.get(`${a}:${u}`), 2))]) }]);
}

// The adjusted means, as in their panel (moteur/R/moyennes.R: computed and kept by the engine, the
// last settings of its view, otherwise the defaults): what was computed and how, the sentence of the
// first pair, the means (or slopes) and the pairs.
async function tableauMoyennes(m) {
  let r;
  try { r = await demander('moyennes', { modele: m.id }); } catch (e) { return [{ texte: e.message }]; }
  const cm = (k) => [].concat(r.moyennes[k] ?? []);
  const cp = (k) => [].concat(r.paires[k] ?? []);
  const ddl = (x) => (x === null || x === undefined ? '∞' : nombre(x, Number.isInteger(x) ? 0 : 1));
  const phrases = [].concat(r.phrases || []);
  return [{ texte: notesMoyennes(r) }].concat(phrases.length ? [{ texte: phrases[0] }] : [],
    [{ entetes: [r.facteur.nom].concat(r.dans ? [r.dans.nom] : [], [titreMoyenne(r), t('erreur_type'), t('ddl'), t('ic_bas'), t('ic_haut')]),
      lignes: cm('niveau').map((n, i) => [n].concat(r.dans ? [cm('groupe')[i]] : [], [nombre(cm('estimation')[i]), nombre(cm('erreur_type')[i]), ddl(cm('ddl')[i]),
        nombre(cm('bas')[i]), nombre(cm('haut')[i])])) }],
    cp('estimation').length ? [{ entetes: (r.dans ? [r.dans.nom] : []).concat([t('moyennes_paire'), titrePaire(r), t('erreur_type'), t('ddl'),
      cp('ddl').every((x) => x === null) ? 'z' : 't', r.reglages.ajustement === 'aucun' ? 'p' : t('p_ajuste'), t('ic_bas'), t('ic_haut')]),
    lignes: cp('estimation').map((_, i) => (r.dans ? [cp('groupe')[i]] : []).concat([libellePaire(r, i), nombre(cp('estimation')[i]), nombre(cp('erreur_type')[i]),
      ddl(cp('ddl')[i]), nombre(cp('t')[i], 3), valeurP(cp('p')[i]), nombre(cp('bas')[i]), nombre(cp('haut')[i])])) }] : []);
}

// A regression's coefficients, as in its Coefficients panel (exp() of them with a log or logit link).
function tableauCoefficientsRegression(m) {
  const c = m.coefficients_regression;
  const col = (k) => [].concat(c[k]);
  const exp = ['log', 'logit'].includes(m.lien);
  const est = col('estimation');
  const [bas, haut] = [col('bas'), col('haut')];
  return [{ entetes: [t('terme'), t('estimation'), t('erreur_type'), t('ddl'), 't', 'p', t('ic_bas'), t('ic_haut')]
    .concat(exp ? [t(m.lien === 'logit' ? 'rapport_cotes' : 'effet_multiplicatif'), t('ic_bas'), t('ic_haut')] : []),
  lignes: col('terme').map((n, i) => [n, nombre(est[i]), nombre(col('erreur_type')[i]), nombre(col('ddl')[i], 0), nombre(col('t')[i], 3), valeurP(col('p')[i]),
    nombre(bas[i]), nombre(haut[i])].concat(exp ? [nombre(Math.exp(est[i])), nombre(Math.exp(bas[i])), nombre(Math.exp(haut[i]))] : [])) }];
}

function tableauCVANOVA(m) {
  return Object.entries(m.cv_anova || {}).flatMap(([y, a]) => [
    { texte: `CV-ANOVA · ${y}` },
    { entetes: ['', 'SS', 'DF', 'MS', 'F', 'p', 'SD'], lignes: a.source.map((_, i) => [t('anova_' + i), nombre(a.SS[i], 3), String(a.DF[i]),
      nombre(a.MS[i]), a.F[i] === null ? '' : nombre(a.F[i], 3), valeurP(a.p[i]), nombre(a.SD[i])]) },
    { texte: a.p[1] !== null && a.p[1] < 0.05 ? t('anova_significatif') : t('anova_non_significatif') }]);
}

function tableauClassification(m) {
  const r = m.classification.vc;
  const classes = [].concat(r.classes);
  const somme = (v) => v.reduce((a, b) => a + b, 0);
  return [{ texte: `${t('validation_croisee')} · ${t('bien_classes')} : ${fmt(100 * r.global, 1)} %` },
    { entetes: [`${t('observee')} \\ ${t('predite')}`, ...classes, t('total'), t('bien_classes')],
      lignes: classes.map((k, i) => [k, ...r.table[i].map(String), String(somme(r.table[i])), `${fmt(100 * [].concat(r.correct)[i], 1)} %`]) }];
}

function tableauVariables(m) {
  const v = m.variables;
  const pls = m.type !== 'PCA';
  const charge = pls ? 'w*' : 'p';
  const cols = (pls && v.VIP ? [['VIP', 'VIP']] : []).concat(Array.from({ length: Math.min(m.A, 3) }, (_, a) => [`${charge}${a + 1}`, `${charge}[${a + 1}]`]));
  const c = m.coefficients;
  const y = pls && c ? m.noms_y[0] : null;
  return [{ entetes: [t('variables'), ...cols.map(([, n]) => n), ...(y ? [`Coeff CS ${y}`] : [])],
    lignes: v.terme.map((nom, i) => [nom, ...cols.map(([k]) => nombre(v[k] && v[k][i], 3)),
      ...(y ? [nombre(c[`CoeffCS ${y}`][c.terme.indexOf(nom)], 4)] : [])]) }];
}

// ------------------------------------------------------------ PDF (an HTML page, printed)

const echapper = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

function pageHTML(blocs, titre) {
  const corps = blocs.map((b) => {
    if (b.titre !== undefined) return b.niveau === 0 ? `<h1>${echapper(b.titre)}</h1>` : `<h2>${echapper(b.titre)}</h2>`;
    if (b.texte !== undefined) return `<p>${echapper(b.texte)}</p>`;
    if (b.entetes) {
      return `<table><thead><tr>${b.entetes.map((h) => `<th>${echapper(h)}</th>`).join('')}</tr></thead><tbody>${
        b.lignes.map((l) => `<tr>${l.map((v, k) => `<td${k ? ' class="num"' : ''}>${echapper(v)}</td>`).join('')}</tr>`).join('')}</tbody></table>`;
    }
    return `<figure><img src="${b.image}"></figure>`;
  }).join('\n');
  return `<!doctype html><html lang="${langueActuelle()}"><head><meta charset="utf-8"><title>${echapper(titre)}</title><style>
@page { size: A4; margin: 15mm; }
body { font-family: 'Segoe UI', Calibri, Arial, sans-serif; font-size: 10pt; color: #222; }
h1 { font-size: 17pt; margin: 0 0 4pt; } h2 { font-size: 12.5pt; margin: 16pt 0 6pt; border-bottom: 1px solid #ccc; padding-bottom: 2pt; break-after: avoid; }
p { margin: 3pt 0; } table { border-collapse: collapse; font-size: 8.5pt; margin: 6pt 0; break-inside: auto; }
th, td { border: 1px solid #bbb; padding: 2pt 5pt; text-align: left; } th { background: #eef2f7; } td.num { text-align: right; }
figure { margin: 6pt 0; break-inside: avoid; } img { width: 100%; }
</style></head><body>${corps}</body></html>`;
}

function imprimerNavigateur(html) {
  const f = window.open('', '_blank');
  f.document.write(html);
  f.document.close();
  f.onload = () => f.print();
}

function telechargerOctets(octets, nom) {
  const a = el('a', { href: URL.createObjectURL(new Blob([octets])), download: nom });
  a.click();
  setTimeout(() => URL.revokeObjectURL(a.href), 2000);
}

// ------------------------------------------------------------ Word (.docx)

const xml = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
const W = 'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"';

function docx(blocs) {
  const images = [];
  const run = (texte, gras = false) => `<w:r>${gras ? '<w:rPr><w:b/></w:rPr>' : ''}<w:t xml:space="preserve">${xml(texte)}</w:t></w:r>`;
  const para = (texte, style, droite = false, gras = false) => `<w:p>${style || droite ? `<w:pPr>${style ? `<w:pStyle w:val="${style}"/>` : ''}${droite ? '<w:jc w:val="right"/>' : ''}</w:pPr>` : ''}${run(texte, gras)}</w:p>`;
  const bord = (n) => `<w:${n} w:val="single" w:sz="4" w:space="0" w:color="AAAAAA"/>`;
  const corps = blocs.map((b) => {
    if (b.titre !== undefined) return para(b.titre, b.niveau === 0 ? 'Title' : 'Heading1');
    if (b.texte !== undefined) return para(b.texte);
    if (b.entetes) {
      const cellule = (v, k, entete) => `<w:tc><w:tcPr><w:tcW w:w="0" w:type="auto"/>${entete ? '<w:shd w:val="clear" w:color="auto" w:fill="EEF2F7"/>' : ''}</w:tcPr>${para(v, 'Tableau', k > 0 && !entete, entete)}</w:tc>`;
      return `<w:tbl><w:tblPr><w:tblW w:w="0" w:type="auto"/><w:tblBorders>${['top', 'left', 'bottom', 'right', 'insideH', 'insideV'].map(bord).join('')}</w:tblBorders>`
        + '<w:tblCellMar><w:left w:w="70" w:type="dxa"/><w:right w:w="70" w:type="dxa"/></w:tblCellMar></w:tblPr>'
        + `<w:tblGrid>${b.entetes.map(() => '<w:gridCol/>').join('')}</w:tblGrid>`
        + `<w:tr><w:trPr><w:tblHeader/></w:trPr>${b.entetes.map((h, k) => cellule(h, k, true)).join('')}</w:tr>`
        + b.lignes.map((l) => `<w:tr>${l.map((v, k) => cellule(v, k, false)).join('')}</w:tr>`).join('')
        + '</w:tbl><w:p/>';
    }
    images.push(b.image);
    const n = images.length;
    const cx = 6120000;  // 17 cm, the text width of an A4 page with 2 cm margins
    const cy = Math.round(cx * b.hauteur / b.largeur);
    return `<w:p><w:pPr><w:jc w:val="center"/></w:pPr><w:r><w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0">`
      + `<wp:extent cx="${cx}" cy="${cy}"/><wp:docPr id="${n}" name="Image ${n}"/>`
      + '<wp:cNvGraphicFramePr><a:graphicFrameLocks noChangeAspect="1"/></wp:cNvGraphicFramePr>'
      + '<a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic>'
      + `<pic:nvPicPr><pic:cNvPr id="${n}" name="image${n}.png"/><pic:cNvPicPr/></pic:nvPicPr>`
      + `<pic:blipFill><a:blip r:embed="rIdImage${n}"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>`
      + `<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="${cx}" cy="${cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>`
      + '</pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p>';
  }).join('');
  const document = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:document ${W} xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"><w:body>${corps}`
    + '<w:sectPr><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1134" w:right="1134" w:bottom="1134" w:left="1134" w:header="567" w:footer="567" w:gutter="0"/></w:sectPr></w:body></w:document>';
  const style = (id, nom, rpr, ppr = '', base = 'Normal') => `<w:style w:type="paragraph" w:styleId="${id}"><w:name w:val="${nom}"/>${base ? `<w:basedOn w:val="${base}"/>` : ''}<w:qFormat/>${ppr ? `<w:pPr>${ppr}</w:pPr>` : ''}<w:rPr>${rpr}</w:rPr></w:style>`;
  const styles = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<w:styles ${W}><w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:cs="Calibri"/><w:sz w:val="21"/><w:lang w:val="${langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB'}"/></w:rPr></w:rPrDefault>`
    + '<w:pPrDefault><w:pPr><w:spacing w:after="60"/></w:pPr></w:pPrDefault></w:docDefaults>'
    + style('Normal', 'Normal', '', '', null)
    + style('Title', 'Title', '<w:b/><w:sz w:val="34"/>', '<w:spacing w:after="120"/>')
    + style('Heading1', 'heading 1', '<w:b/><w:color w:val="2F4F7F"/><w:sz w:val="25"/>', '<w:keepNext/><w:spacing w:before="280" w:after="100"/><w:outlineLvl w:val="0"/>')
    + style('Tableau', 'Tableau', '<w:sz w:val="16"/>', '<w:spacing w:after="0"/>')
    + '</w:styles>';
  const relations = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
    + '<Relationship Id="rIdStyles" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>'
    + images.map((_, i) => `<Relationship Id="rIdImage${i + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/image${i + 1}.png"/>`).join('')
    + '</Relationships>';
  const types = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
    + '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/>'
    + '<Default Extension="png" ContentType="image/png"/>'
    + '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>'
    + '<Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/></Types>';
  const racine = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
    + '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/></Relationships>';
  const texte = (s) => new TextEncoder().encode(s);
  const png = (url) => Uint8Array.from(atob(url.split(',')[1]), (c) => c.charCodeAt(0));
  return zip([['[Content_Types].xml', texte(types)], ['_rels/.rels', texte(racine)], ['word/document.xml', texte(document)],
    ['word/styles.xml', texte(styles)], ['word/_rels/document.xml.rels', texte(relations)],
    ...images.map((url, i) => [`word/media/image${i + 1}.png`, png(url)])]);
}

// A zip archive with its files stored as they are (no compression), enough for a .docx.
const TABLE_CRC = Array.from({ length: 256 }, (_, n) => {
  let c = n;
  for (let k = 0; k < 8; k += 1) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1;
  return c >>> 0;
});
function crc32(octets) {
  let c = 0xffffffff;
  for (const o of octets) c = TABLE_CRC[(c ^ o) & 0xff] ^ (c >>> 8);
  return (c ^ 0xffffffff) >>> 0;
}

function zip(fichiers) {
  const morceaux = [];
  const centre = [];
  let decalage = 0;
  const maintenant = new Date();
  const heure = (maintenant.getHours() << 11) | (maintenant.getMinutes() << 5) | (maintenant.getSeconds() >> 1);
  const jour = ((maintenant.getFullYear() - 1980) << 9) | ((maintenant.getMonth() + 1) << 5) | maintenant.getDate();
  for (const [nom, octets] of fichiers) {
    const n = new TextEncoder().encode(nom);
    const crc = crc32(octets);
    const local = new DataView(new ArrayBuffer(30));
    [[0, 0x04034b50, 4], [4, 20, 2], [6, 0x0800, 2], [8, 0, 2], [10, heure, 2], [12, jour, 2], [14, crc, 4],
      [18, octets.length, 4], [22, octets.length, 4], [26, n.length, 2], [28, 0, 2]]
      .forEach(([k, v, l]) => (l === 4 ? local.setUint32(k, v, true) : local.setUint16(k, v, true)));
    const c = new DataView(new ArrayBuffer(46));
    [[0, 0x02014b50, 4], [4, 20, 2], [6, 20, 2], [8, 0x0800, 2], [10, 0, 2], [12, heure, 2], [14, jour, 2], [16, crc, 4],
      [20, octets.length, 4], [24, octets.length, 4], [28, n.length, 2], [30, 0, 2], [32, 0, 2], [34, 0, 2], [36, 0, 2], [38, 0, 4], [42, decalage, 4]]
      .forEach(([k, v, l]) => (l === 4 ? c.setUint32(k, v, true) : c.setUint16(k, v, true)));
    morceaux.push(new Uint8Array(local.buffer), n, octets);
    centre.push(new Uint8Array(c.buffer), n);
    decalage += 30 + n.length + octets.length;
  }
  const tailleCentre = centre.reduce((s, x) => s + x.length, 0);
  const fin = new DataView(new ArrayBuffer(22));
  [[0, 0x06054b50, 4], [4, 0, 2], [6, 0, 2], [8, fichiers.length, 2], [10, fichiers.length, 2], [12, tailleCentre, 4], [16, decalage, 4], [20, 0, 2]]
    .forEach(([k, v, l]) => (l === 4 ? fin.setUint32(k, v, true) : fin.setUint16(k, v, true)));
  const tout = [...morceaux, ...centre, new Uint8Array(fin.buffer)];
  const sortie = new Uint8Array(tout.reduce((s, x) => s + x.length, 0));
  let k = 0;
  for (const x of tout) { sortie.set(x, k); k += x.length; }
  return sortie;
}
