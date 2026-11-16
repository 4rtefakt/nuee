// Rarefaction and species accumulation (Plots › Rarefaction, a project's view beside Series;
// moteur/R/rarefaction.R, the `rarefaction` request). The species are columns of the table chosen
// in the view, as a range in the table's order (from one column to another, both included, as the
// diversity indices of New variable read them) or ticked one by one. Two modes: each observation's
// rarefaction curve with its richness at a common size and its Chao1 and ACE (integer counts
// only: R says when the data aren't), or the exact accumulation of species over the observations
// (±2 SD, as vegan draws it) with Chao2, the jackknifes and the bootstrap, one curve per group
// of a qualitative variable or ID if one is chosen. A click or a box selects observations, kept
// under the project's id as in Series. Imported by app.js.
import { t, ajouterTextes, langueActuelle, deuxPoints } from './i18n.js';
import { el, fmt } from './ui.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, tableau } from './panneaux.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const GRIS = '#9aa7b8';
const rgba = (hex, a) => { const n = parseInt(hex.slice(1), 16); return `rgba(${(n >> 16) & 255},${(n >> 8) & 255},${n & 255},${a})`; };
const ligneDe = (df, i) => Object.fromEntries(Object.keys(df || {}).map((k) => [k, liste(df[k])[i]]));
const lignes = (df) => liste((df || {})[Object.keys(df || {})[0]]).map((_, i) => ligneDe(df, i));

// The species chooser: a range (from, to) in the table's order, and the list of quantitative
// variables to tick; loc.especes holds the uids chosen.
function choixEspeces(ctx, proj, loc) {
  const quantitatives = proj.variables.filter((v) => !v.qualitative);
  const choisies = new Set(liste(loc.especes));
  const options = quantitatives.map((v) => el('option', { value: v.uid }, v.nom));
  const de = el('select', {}, options.map((o) => o.cloneNode(true)));
  const a = el('select', {}, options.map((o) => o.cloneNode(true)));
  if (choisies.size) {
    const rangs = quantitatives.map((v, i) => (choisies.has(v.uid) ? i : -1)).filter((i) => i >= 0);
    de.value = String(quantitatives[rangs[0]].uid);
    a.value = String(quantitatives[rangs[rangs.length - 1]].uid);
  } else if (quantitatives.length) a.value = String(quantitatives[quantitatives.length - 1].uid);
  const plage = () => {
    const i = quantitatives.findIndex((v) => String(v.uid) === de.value);
    const j = quantitatives.findIndex((v) => String(v.uid) === a.value);
    loc.especes = quantitatives.slice(Math.min(i, j), Math.max(i, j) + 1).map((v) => v.uid);
    ctx.rafraichir();
  };
  const cases = el('div', { class: 'defile', style: { maxHeight: '9em', display: loc.liste ? 'block' : 'none', columns: '12em' } },
    quantitatives.map((v) => el('label', { class: 'champ', style: { display: 'block' } }, el('input', { type: 'checkbox', checked: choisies.has(v.uid),
      onchange: (e) => {
        const s = new Set(liste(loc.especes));
        if (e.target.checked) s.add(v.uid); else s.delete(v.uid);
        loc.especes = quantitatives.filter((w) => s.has(w.uid)).map((w) => w.uid);  // (in the table's order)
        ctx.rafraichir();
      } }), v.nom)));
  return [el('label', { class: 'champ', title: t('rar_plage_aide') }, el('span', {}, t('rar_de')), de, el('span', {}, t('rar_a')), a,
    el('button', { onclick: plage }, t('rar_appliquer'))),
  el('button', { onclick: () => { loc.liste = !loc.liste; ctx.rafraichir(); } }, `${t('rar_liste')} (${choisies.size})`), cases];
}

async function rendreRarefaction(conteneur, ctx, p, loc) {
  const proj = ctx.projet(p.projet);
  const ids = liste(proj.identifiants).map((i) => ({ valeur: `id:${i.rang}`, libelle: `${i.nom} (${t('identifiant')})` }));
  const groupes = [...proj.variables.filter((v) => v.qualitative).map((v) => ({ valeur: String(v.uid), libelle: v.nom })), ...ids];
  loc.mode ??= 'rarefaction';
  if (loc.groupe && !groupes.some((g) => g.valeur === loc.groupe)) loc.groupe = '';
  // (opened without species, from the Plots menu: the range the chooser shows, from the table's first
  // quantitative variable to its last, computed at once rather than waiting for "Take the range")
  if (loc.especes === undefined) loc.especes = proj.variables.filter((v) => !v.qualitative).map((v) => v.uid);
  // (species chosen that are no longer quantitative variables of the project, columns added then
  // undone by Ctrl+Z: left out, rather than a chooser with none of them to show)
  loc.especes = liste(loc.especes).map(Number).filter((u) => proj.variables.some((v) => v.uid === u && !v.qualitative));
  const [plage, bouton, cases] = choixEspeces(ctx, proj, loc);
  const barre = outils(plage, bouton,
    choix('', [['rarefaction', t('rar_mode_rarefaction')], ['accumulation', t('rar_mode_accumulation')]], loc.mode, (v) => { loc.mode = v; ctx.rafraichir(); }),
    choix(t('rar_groupes'), [['', t('aucun')], ...groupes.map((g) => [g.valeur, g.libelle])], loc.groupe || '', (v) => { loc.groupe = v; ctx.rafraichir(); }),
    loc.mode === 'rarefaction' ? el('label', { class: 'champ', title: t('rar_taille_aide') }, el('span', {}, t('rar_taille')),
      el('input', { type: 'number', min: 1, step: 1, value: loc.taille ?? '', placeholder: t('rar_taille_min'), style: 'width:9.5em',
        onchange: (e) => { loc.taille = e.target.value === '' ? null : Number(e.target.value); ctx.rafraichir(); } })) : null);
  if (liste(loc.especes).length < 2) {
    conteneur.replaceChildren(barre, cases, el('p', { class: 'infos' }, t('rar_choisir')));
    return;
  }
  const cache = (loc.cache ??= {});  // (the results: loc.cache isn't saved with the session)
  const q = { projet: p.projet, especes: liste(loc.especes), ...(loc.groupe ? { groupe: loc.groupe } : {}), ...(loc.taille ? { taille: loc.taille } : {}) };
  const cle = JSON.stringify([q, langueActuelle()]);
  if (cache.cle !== cle) {
    conteneur.replaceChildren(barre, cases, el('p', { class: 'infos attente' }, t('calcul')));
    try { cache.res = await ctx.demander('rarefaction', q); cache.erreur = null; } catch (e) { cache.res = null; cache.erreur = e.message; }
    cache.cle = cle;
  }
  if (!cache.res) { conteneur.replaceChildren(barre, cases, el('p', { class: 'erreur' }, `${t('erreur')}${deuxPoints()}${cache.erreur}`)); return; }
  const r = cache.res;
  const niveaux = liste(r.niveaux);
  const couleur = (g) => (niveaux.length ? PALETTE[Math.max(0, niveaux.indexOf(g)) % PALETTE.length] : PALETTE[0]);
  const obs = lignes(r.observations);
  const sel = new Set(ctx.selection(p.projet));
  const notes = [t('rar_note', { n: r.n, s: liste(r.especes).length }),
    r.manquantes ? t('rar_manquantes', { n: r.manquantes }) : '', r.sans_groupe ? t('rar_sans_groupe', { n: r.sans_groupe }) : '',
    r.vides ? t('rar_vides', { n: r.vides }) : '', r.entiers ? '' : t('rar_non_entiers'), sel.size ? `${sel.size} ${t('selectionnees')}` : ''].filter(Boolean).join(' ');
  const graphe = el('div', { class: 'graphe' });
  const bas = el('div', { class: 'bas', style: { flex: '0 1 auto', maxHeight: '45%' } });
  const c = couleursTheme(ctx);
  if (loc.mode === 'accumulation') {
    const acc = lignes(r.accumulation);
    const parties = niveaux.length ? niveaux : [null];
    const traces = parties.flatMap((g) => {
      const a = acc.filter((x) => (g === null ? true : x.groupe === g));
      const col = couleur(g);
      const nom = g === null ? t('rar_toutes') : g;
      return [{ type: 'scatter', mode: 'lines', x: a.map((x) => x.sites), y: a.map((x) => x.richesse + 2 * x.et), line: { width: 0 }, hoverinfo: 'skip', showlegend: false, legendgroup: nom },
        { type: 'scatter', mode: 'lines', x: a.map((x) => x.sites), y: a.map((x) => Math.max(0, x.richesse - 2 * x.et)), fill: 'tonexty', fillcolor: rgba(col, 0.18),
          line: { width: 0 }, hoverinfo: 'skip', showlegend: false, legendgroup: nom },
        { type: 'scatter', mode: 'lines+markers', name: nom, legendgroup: nom, x: a.map((x) => x.sites), y: a.map((x) => x.richesse), customdata: a.map((x) => x.et),
          line: { color: col, width: 2 }, marker: { size: 4, color: col },
          hovertemplate: `<b>${nom}</b><br>%{x} ${t('observations')}<br>${t('rar_richesse')} %{y:.2f} ± %{customdata:.2f}<extra></extra>` }];
    });
    conteneur.replaceChildren(barre, cases, el('p', { class: 'note' }, `${notes} ${t('rar_accumulation_note')}`), graphe, bas);
    tracer(ctx, graphe, traces, { title: { text: `${proj.nom || ''} · ${t('rar_mode_accumulation')}` }, showlegend: parties.length > 1, dragmode: 'zoom',
      xaxis: { title: { text: t('rar_nombre_obs') }, rangemode: 'tozero' }, yaxis: { title: { text: t('rar_richesse') }, rangemode: 'tozero' } });
    const est = lignes(r.estimateurs);
    tableau(bas, ctx, (loc.tableEstimateurs ??= {}), {
      n: est.length, nom: t('rar_estimateurs'),
      colonnes: [...(niveaux.length ? [{ cle: 'groupe', titre: r.groupe.nom, couleur: (i) => couleur(est[i].groupe) }] : []),
        { cle: 'n', titre: 'n', num: true, decimales: 0 }, { cle: 'S', titre: t('rar_s_obs'), num: true, decimales: 0 },
        { cle: 'chao', titre: 'Chao2', num: true, decimales: 2 }, { cle: 'se_chao', titre: 'σ', num: true, decimales: 2 },
        { cle: 'jack1', titre: 'Jack 1', num: true, decimales: 2 }, { cle: 'se_jack1', titre: 'σ', num: true, decimales: 2 },
        { cle: 'jack2', titre: 'Jack 2', num: true, decimales: 2 }, { cle: 'boot', titre: 'Bootstrap', num: true, decimales: 2 }, { cle: 'se_boot', titre: 'σ', num: true, decimales: 2 }],
      valeur: (i, k) => est[i][k],
    });
    return;
  }
  // the rarefaction curves: a line per observation, its points the sizes R computed
  const courbes = lignes(r.courbes);
  const parUid = new Map();
  for (const x of courbes) { if (!parUid.has(x.uid)) parUid.set(x.uid, []); parUid.get(x.uid).push(x); }
  const groupeDe = new Map(obs.map((o) => [o.uid, o.groupe]));
  const nomDe = new Map(obs.map((o) => [o.uid, o.nom]));
  const vus = new Set();
  const traces = [...parUid.entries()].map(([uid, pts]) => {
    const g = groupeDe.get(uid) ?? null;
    const nom = niveaux.length ? g : t('observations');
    const premier = !vus.has(nom);
    vus.add(nom);
    const choisie = sel.has(uid);
    // (legendrank: the groups in the legend in their order, as the accumulation's, not in that of
    // their first observation in the table)
    return { type: 'scatter', mode: 'lines', name: nom, legendgroup: nom, showlegend: niveaux.length > 0 && premier,
      ...(niveaux.length ? { legendrank: 1000 + niveaux.indexOf(g) } : {}),
      x: pts.map((x) => x.taille), y: pts.map((x) => x.S), customdata: pts.map(() => uid), text: pts.map((x) => x.se),
      line: { color: niveaux.length ? couleur(g) : PALETTE[0], width: choisie ? 3 : 1.3 }, opacity: sel.size && !choisie ? 0.35 : 0.9,
      hovertemplate: `<b>${nomDe.get(uid)}</b><br>${t('rar_individus')} %{x}<br>${t('rar_richesse')} %{y:.2f} ± %{text:.2f}<extra>${niveaux.length ? g : ''}</extra>` };
  });
  const shapes = r.entiers && r.taille ? [{ type: 'line', xref: 'x', yref: 'paper', x0: r.taille, x1: r.taille, y0: 0, y1: 1, line: { dash: 'dash', color: c.doux, width: 1 } }] : [];
  conteneur.replaceChildren(barre, cases, el('p', { class: 'note' }, `${notes}${r.entiers ? ` ${t('rar_rarefaction_note', { m: r.taille, points: r.pas === 1 ? t('rar_point_chacun') : t('rar_point_tous', { pas: r.pas }) })}` : ''}${r.petites ? ` ${t('rar_petites', { n: r.petites, m: r.taille })}` : ''}`),
    graphe, bas);
  if (r.entiers) {
    tracer(ctx, graphe, traces, { title: { text: `${proj.nom || ''} · ${t('rar_mode_rarefaction')}` }, shapes, dragmode: 'select',
      xaxis: { title: { text: t('rar_individus') }, rangemode: 'tozero' }, yaxis: { title: { text: t('rar_richesse') }, rangemode: 'tozero' } });
    brancherSelection(graphe, ctx, p.projet);
  } else graphe.replaceChildren(el('p', { class: 'infos' }, t('rar_non_entiers')));
  tableau(bas, ctx, (loc.tableObs ??= {}), {
    n: obs.length, nom: t('rar_mode_rarefaction'), uid: (i) => obs[i].uid, idModele: p.projet,
    colonnes: [{ cle: 'nom', titre: t('observations') }, ...(niveaux.length ? [{ cle: 'groupe', titre: r.groupe.nom, couleur: (i) => couleur(obs[i].groupe) }] : []),
      { cle: 'N', titre: t('rar_individus'), num: true, decimales: 0 }, { cle: 'S', titre: t('rar_s_obs'), num: true, decimales: 0 },
      { cle: 'Sm', titre: t('rar_s_rarefiee', { m: r.taille }), num: true, decimales: 2 }, { cle: 'se', titre: 'σ', num: true, decimales: 2 },
      { cle: 'chao1', titre: 'Chao1', num: true, decimales: 2 }, { cle: 'se_chao1', titre: 'σ', num: true, decimales: 2 },
      { cle: 'ace', titre: 'ACE', num: true, decimales: 2 }, { cle: 'se_ace', titre: 'σ', num: true, decimales: 2 }],
    valeur: (i, k) => obs[i][k],
  });
}

VUES.rarefaction = { rendre: rendreRarefaction, titre: (_, p) => `${p.nom || ''} · ${t('rarefaction')}`, projet: true, selection: true };

ajouterTextes({
  rarefaction: 'Raréfaction et accumulation des espèces',
  rar_de: 'Espèces de', rar_a: 'à', rar_appliquer: 'Prendre la plage', rar_liste: 'Liste',
  rar_plage_aide: 'Les colonnes d’espèces (des effectifs), de l’une à l’autre dans l’ordre du tableau, les deux comprises ; ou cochées une à une dans la liste.',
  rar_choisir: 'Choisir au moins deux colonnes d’espèces (des effectifs : une ligne par observation), par une plage ou dans la liste.',
  rar_mode_rarefaction: 'Raréfaction (par observation)', rar_mode_accumulation: 'Accumulation (sur les observations)', rar_groupes: 'Par groupe',
  rar_taille: 'Taille commune', rar_point_chacun: 'un point par individu', rar_point_tous: 'un point tous les {pas} individus', rar_taille_min: 'la plus petite', rar_taille_aide: 'La richesse de chaque observation raréfiée à ce nombre d’individus (par défaut, celui de la plus petite observation).',
  rar_note: '{n} observations, {s} espèces.', rar_manquantes: '{n} laissées de côté : une espèce y manque.', rar_sans_groupe: '{n} sans groupe, laissées de côté.',
  rar_vides: '{n} sans aucun individu (sans courbe de raréfaction ; elles comptent dans l’accumulation).',
  rar_non_entiers: 'Ces données ne sont pas des effectifs entiers : la raréfaction, Chao1 et ACE demandent des comptages d’individus. L’accumulation et les estimateurs fondés sur les présences (Chao2, jackknife, bootstrap) restent possibles.',
  rar_rarefaction_note: 'La richesse attendue dans m individus tirés sans remise (Hurlbert 1971), ±σ (Heck et al. 1975), comme vegan::rarefy ; en pointillés, la taille commune ({m}) ; {points}. Chao1 (corrigé du biais) et ACE estiment la richesse de chaque observation (vegan::estimateR).',
  rar_petites: '{n} observations ont moins de {m} individus : sans richesse raréfiée.',
  rar_accumulation_note: 'La richesse attendue dans k observations tirées parmi toutes (méthode exacte, Ugland et al. 2003, comme vegan::specaccum), ±2 écarts-types. Sous le graphique, les estimateurs de la richesse totale d’après les présences (vegan::specpool).',
  rar_toutes: 'toutes', rar_nombre_obs: 'Nombre d’observations', rar_richesse: 'Richesse', rar_individus: 'Individus', rar_s_obs: 'S observée',
  rar_s_rarefiee: 'S à {m}', rar_estimateurs: 'Estimateurs',
}, {
  rarefaction: 'Rarefaction and species accumulation',
  rar_de: 'Species from', rar_a: 'to', rar_appliquer: 'Take the range', rar_liste: 'List',
  rar_plage_aide: 'The species columns (counts), from one to another in the table’s order, both included; or ticked one by one in the list.',
  rar_choisir: 'Choose at least two species columns (counts: one row per observation), by a range or in the list.',
  rar_mode_rarefaction: 'Rarefaction (by observation)', rar_mode_accumulation: 'Accumulation (over the observations)', rar_groupes: 'By group',
  rar_taille: 'Common size', rar_point_chacun: 'a point for each individual', rar_point_tous: 'a point every {pas} individuals', rar_taille_min: 'the smallest', rar_taille_aide: 'Each observation’s richness rarefied to this number of individuals (by default, that of the smallest observation).',
  rar_note: '{n} observations, {s} species.', rar_manquantes: '{n} left out: a species is missing.', rar_sans_groupe: '{n} without a group, left out.',
  rar_vides: '{n} without any individual (no rarefaction curve; they count in the accumulation).',
  rar_non_entiers: 'These data aren’t integer counts: rarefaction, Chao1 and ACE need counts of individuals. The accumulation and the incidence-based estimators (Chao2, jackknife, bootstrap) remain possible.',
  rar_rarefaction_note: 'The richness expected in m individuals drawn without replacement (Hurlbert 1971), ±σ (Heck et al. 1975), as vegan::rarefy; dashed, the common size ({m}); {points}. Chao1 (bias-corrected) and ACE estimate each observation’s richness (vegan::estimateR).',
  rar_petites: '{n} observations have fewer than {m} individuals: no rarefied richness.',
  rar_accumulation_note: 'The richness expected in k observations drawn among all (exact method, Ugland et al. 2003, as vegan::specaccum), ±2 standard deviations. Under the plot, the estimators of the total richness from the presences (vegan::specpool).',
  rar_toutes: 'all', rar_nombre_obs: 'Number of observations', rar_richesse: 'Richness', rar_individus: 'Individuals', rar_s_obs: 'Observed S',
  rar_s_rarefiee: 'S at {m}', rar_estimateurs: 'Estimators',
});
