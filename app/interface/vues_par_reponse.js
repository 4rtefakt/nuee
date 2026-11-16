// One model per response (Model › One model per response, moteur/R/par_reponse.R): the same
// regression fitted to each of several responses (one species after another). The view shows,
// for a chosen term: each response's coefficient with its 95% interval as a forest plot (sorted,
// in colour where the p-value corrected over the responses is at most 0.05) and in a table; the
// term's effect in small multiples (one small plot per response, its band, the same x axis); and
// each model's quality (N, AIC, R², Q²) with the responses that couldn't be fitted.
// Imported by app.js.
import { t, ajouterTextes } from './i18n.js';
import { el, fmt } from './ui.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, tableau, valeurP } from './panneaux.js';
import { ICONES_VUES } from './commandes.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));
const GRIS = '#9aa7b8';
const PETITS_MAX = 36;  // small multiples up to this many responses

async function rendreParReponse(conteneur, ctx, p, loc) {
  const m = ctx.modele(p.modele);
  const cache = (loc.cache ??= {});
  const cle = JSON.stringify([liste(loc.reponses), loc.correction || 'BH']);
  if (!liste(loc.reponses).length) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('par_reponse_vide'))); return; }
  if (cache.cle !== cle || !cache.res) {
    conteneur.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
    try { cache.res = await ctx.demander('par_reponse', { modele: m.id, reponses: liste(loc.reponses), correction: loc.correction || 'BH' }); cache.erreur = null; } catch (e) { cache.res = null; cache.erreur = e.message; }
    cache.cle = cle;
  }
  if (!cache.res) { conteneur.replaceChildren(el('p', { class: 'erreur' }, `${t('erreur')} : ${cache.erreur}`)); return; }
  const r = cache.res;
  const tab = r.tableau || {};
  const col = (k) => liste(tab[k]);
  const termes = [...new Set(col('terme'))];
  const resume = r.resume || {};
  const erreurs = liste(resume.erreur).map((e, i) => [liste(resume.reponse)[i], e]).filter(([, e]) => e);
  if (!termes.length) {
    conteneur.replaceChildren(el('p', { class: 'infos' }, t('par_reponse_sans_terme')), ...erreurs.map(([n, e]) => el('p', { class: 'erreur' }, `${n} : ${e}`)));
    return;
  }
  if (!termes.includes(loc.terme)) loc.terme = termes[0];
  const lignes = col('terme').map((x, i) => i).filter((i) => col('terme')[i] === loc.terme);
  const courbe = lignes.every((i) => col('estimation')[i] === null);
  const barre = outils(choix(t('par_reponse_terme'), termes.map((x) => [x, x]), loc.terme, (v) => { loc.terme = v; ctx.rafraichir(); }),
    el('span', { class: 'infos' }, t('par_reponse_resume', { n: liste(resume.reponse).length - erreurs.length, total: liste(resume.reponse).length,
      correction: { BH: t('correction_bh'), holm: 'Holm', bonferroni: 'Bonferroni' }[r.correction] || r.correction })));
  const graphes = el('div', { style: { display: 'flex', gap: '8px', flex: '1 1 auto', minHeight: '260px' } });
  const g1 = el('div', { class: 'graphe', style: { flex: '1 1 45%', minWidth: '0' } });
  const g2 = el('div', { class: 'graphe', style: { flex: '1 1 55%', minWidth: '0' } });
  graphes.append(g1, g2);
  const bas = el('div', { class: 'bas', style: { flex: '0 1 auto', maxHeight: '40%' } });
  const notes = [r.methodes, ...erreurs.map(([n, e]) => t('par_reponse_echec', { nom: n, erreur: e }))].filter(Boolean);
  conteneur.replaceChildren(barre, graphes, el('div', {}, notes.map((x) => el('p', { class: 'note' }, x))), bas);
  const c = couleursTheme(ctx);
  // the forest plot: each response's estimate and interval (a curve: its edf), sorted
  const ordre = [...lignes].sort((a, b) => (courbe ? col('edf')[a] - col('edf')[b] : col('estimation')[a] - col('estimation')[b]));
  const sig = (i) => col('p_corrige')[i] !== null && col('p_corrige')[i] <= 0.05;
  const y = ordre.map((i) => col('reponse')[i]);
  if (courbe) {
    // (a corrected p ≤ 0.05: its colour, and a star, not the colour alone)
    tracer(ctx, g1, [{ type: 'bar', orientation: 'h', x: ordre.map((i) => col('edf')[i]), y, marker: { color: ordre.map((i) => (sig(i) ? PALETTE[1] : PALETTE[0])) },
      text: ordre.map((i) => (sig(i) ? '*' : '')), textposition: 'outside', cliponaxis: false,
      customdata: ordre.map((i) => [valeurP(col('p')[i]), valeurP(col('p_corrige')[i])]),
      hovertemplate: '<b>%{y}</b><br>edf = %{x:.2f}<br>p = %{customdata[0]} · ' + t('par_reponse_p_corrige') + ' = %{customdata[1]}<extra></extra>' }],
    { title: { text: `${m.nom} · ${loc.terme} · edf` }, dragmode: 'zoom', showlegend: false, margin: { l: 110, r: 18, t: 34, b: 48 },
      xaxis: { title: { text: 'edf' }, rangemode: 'tozero' }, yaxis: { automargin: true } });
  } else {
    tracer(ctx, g1, [{ type: 'scatter', mode: 'markers', x: ordre.map((i) => col('estimation')[i]), y,
      error_x: { type: 'data', symmetric: false, array: ordre.map((i) => col('haut')[i] - col('estimation')[i]), arrayminus: ordre.map((i) => col('estimation')[i] - col('bas')[i]),
        color: c.doux, thickness: 1.2, width: 3 },
      marker: { size: 8, color: ordre.map((i) => (sig(i) ? PALETTE[1] : PALETTE[0])), symbol: ordre.map((i) => (sig(i) ? 'circle' : 'circle-open')) },
      customdata: ordre.map((i) => [fmt(col('bas')[i], 3), fmt(col('haut')[i], 3), valeurP(col('p')[i]), valeurP(col('p_corrige')[i])]),
      hovertemplate: '<b>%{y}</b><br>%{x:.4f} [%{customdata[0]} ; %{customdata[1]}]<br>p = %{customdata[2]} · ' + t('par_reponse_p_corrige') + ' = %{customdata[3]}<extra></extra>' }],
    { title: { text: `${m.nom} · ${loc.terme}` }, dragmode: 'zoom', showlegend: false, margin: { l: 110, r: 18, t: 34, b: 48 },
      shapes: [{ type: 'line', xref: 'x', yref: 'paper', x0: 0, x1: 0, y0: 0, y1: 1, line: { color: c.doux, dash: 'dash', width: 1 } }],
      xaxis: { title: { text: t('par_reponse_coefficient') }, zeroline: false }, yaxis: { automargin: true } });
  }
  // the small multiples: the term's effect (its curve or its line) for each response, in a grid
  const effets = liste(r.effets).map((e) => ({ reponse: e.reponse, effet: liste(e.effets).find((x) => x && x.terme === loc.terme && x.x !== undefined) }))
    .filter((e) => e.effet).slice(0, PETITS_MAX);
  if (!effets.length) {
    g2.replaceChildren(el('p', { class: 'infos' }, t('par_reponse_sans_effet')));
  } else {
    const n = effets.length;
    const C = Math.ceil(Math.sqrt(n));
    const R = Math.ceil(n / C);
    const gx = 0.03;
    const gy = Math.min(0.08, 0.3 / R);
    const lay = { title: { text: `${t('par_reponse_effets')} · ${loc.terme}` }, showlegend: false, dragmode: 'zoom', margin: { l: 40, r: 8, t: 40, b: 30 }, annotations: [] };
    const traces = [];
    effets.forEach((e, k) => {
      const s = k ? String(k + 1) : '';
      const rang = Math.floor(k / C);
      const colonne = k % C;
      const dx = [colonne / C + (colonne ? gx / 2 : 0), (colonne + 1) / C - (colonne < C - 1 ? gx / 2 : 0)];
      const dy = [1 - (rang + 1) / R + (rang < R - 1 ? gy / 2 : 0), 1 - rang / R - (rang ? gy / 2 : 0)];
      lay[`xaxis${s}`] = { domain: dx, anchor: `y${s}`, showticklabels: rang === R - 1 || k + C >= n, ...(s ? { matches: 'x' } : {}), gridcolor: c.grille, zeroline: false, tickfont: { size: 9 } };
      lay[`yaxis${s}`] = { domain: dy, anchor: `x${s}`, gridcolor: c.grille, zerolinecolor: c.doux, tickfont: { size: 9 } };
      lay.annotations.push({ text: e.reponse, xref: 'paper', yref: 'paper', x: dx[0], y: dy[1], xanchor: 'left', yanchor: 'bottom', showarrow: false, font: { size: 10 } });
      const X = liste(e.effet.x);
      const ligne = liste(r.tableau.reponse).findIndex((nom, i) => nom === e.reponse && liste(r.tableau.terme)[i] === loc.terme);
      const couleur = ligne >= 0 && sig(ligne) ? PALETTE[1] : PALETTE[0];
      traces.push({ type: 'scatter', mode: 'lines', x: [...X, ...[...X].reverse()], y: [...liste(e.effet.haut), ...[...liste(e.effet.bas)].reverse()], fill: 'toself',
        fillcolor: 'rgba(120,140,170,0.18)', line: { width: 0 }, hoverinfo: 'skip', xaxis: `x${s}`, yaxis: `y${s}` },
      { type: 'scatter', mode: 'lines', x: X, y: liste(e.effet.effet), line: { color: couleur, width: 1.8 }, xaxis: `x${s}`, yaxis: `y${s}`,
        hovertemplate: `${e.reponse}<br>%{x:.3~g} → %{y:.3f}<extra></extra>` });
    });
    tracer(ctx, g2, traces, lay);
  }
  // the table: the chosen term for every response, then the models' quality
  tableau(bas, ctx, (loc.table ??= {}), {
    n: lignes.length, nom: `${m.nom} · ${loc.terme}`,
    colonnes: [{ cle: 'reponse', titre: t('par_reponse_reponse') },
      ...(courbe ? [{ cle: 'edf', titre: 'edf', num: true, decimales: 2 }] : [{ cle: 'estimation', titre: t('par_reponse_coefficient'), num: true, decimales: 4 },
        { cle: 'bas', titre: t('ic_bas'), num: true, decimales: 4 }, { cle: 'haut', titre: t('ic_haut'), num: true, decimales: 4 }]),
      { cle: 'p', titre: 'p', num: true, format: valeurP },
      { cle: 'p_corrige', titre: t('par_reponse_p_corrige'), num: true, format: valeurP, alerte: (i) => sig(lignes[i]) },
      { cle: 'N', titre: 'N', num: true, decimales: 0 }, { cle: 'AIC', titre: 'AIC', num: true, decimales: 1 },
      { cle: 'R2', titre: 'R²', num: true, decimales: 3 }, { cle: 'Q2', titre: 'Q²', num: true, decimales: 3 }],
    valeur: (i, k) => {
      const j = lignes[i];
      if (['N', 'AIC', 'R2', 'Q2'].includes(k)) { const q = liste(resume.reponse).indexOf(col('reponse')[j]); return q >= 0 ? liste(resume[k])[q] ?? null : null; }
      return col(k)[j] ?? null;
    },
  });
}

VUES.par_reponse = { rendre: rendreParReponse, titre: (m) => `${m.nom} · ${t('par_reponse')}` };
ICONES_VUES.par_reponse = 'layers';

ajouterTextes({
  par_reponse: 'Un modèle par réponse', par_reponse_titre: 'Un modèle par réponse, d’après {nom}',
  par_reponse_aide: 'Le même modèle (ses termes, ses effets aléatoires, sa loi, ses courbes, ses observations, la transformation de sa réponse) ajusté à chaque réponse cochée, une espèce après l’autre. Pour chaque terme, les valeurs p sont corrigées sur les réponses.',
  par_reponse_garder: 'Garder aussi chaque modèle dans le projet (ses propres vues)',
  par_reponse_vide: 'Aucune réponse choisie.', par_reponse_sans_terme: 'Aucun terme à comparer : chaque modèle n’a que son ordonnée à l’origine.',
  par_reponse_terme: 'Terme', par_reponse_resume: '{n} modèles ajustés sur {total} ; p corrigées par {correction}.',
  par_reponse_coefficient: 'Coefficient', par_reponse_p_corrige: 'p corrigée', par_reponse_reponse: 'Réponse', par_reponse_effets: 'Effet par réponse',
  par_reponse_sans_effet: 'Ce terme n’a pas de courbe d’effet à montrer (une catégorie : voir son coefficient à gauche).',
  par_reponse_echec: '{nom} : pas de modèle ({erreur}).',
}, {
  par_reponse: 'One model per response', par_reponse_titre: 'One model per response, from {nom}',
  par_reponse_aide: 'The same model (its terms, random effects, distribution, curves, observations, the transformation of its response) fitted to each response ticked, one species after another. For each term, the p-values are corrected over the responses.',
  par_reponse_garder: 'Also keep each model in the project (its own views)',
  par_reponse_vide: 'No response chosen.', par_reponse_sans_terme: 'No term to compare: each model has only its intercept.',
  par_reponse_terme: 'Term', par_reponse_resume: '{n} models fitted out of {total}; p corrected by {correction}.',
  par_reponse_coefficient: 'Coefficient', par_reponse_p_corrige: 'corrected p', par_reponse_reponse: 'Response', par_reponse_effets: 'Effect by response',
  par_reponse_sans_effet: 'This term has no effect curve to show (a category: see its coefficient on the left).',
  par_reponse_echec: '{nom}: no model ({erreur}).',
});
