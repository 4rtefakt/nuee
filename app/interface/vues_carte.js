// Maps (Plots › Map) and spatial autocorrelation (Plots › Spatial autocorrelation), from the
// project's coordinates (moteur/R/spatial.R): two of its variables and their system, chosen in
// the map view and saved with the project. The map shows the observations on a plane with equal
// scales, or on tiles fetched from OpenStreetMap or IGN when the user turns a background on (the
// tiles tell those servers which area is shown, so it is never on by default), coloured and sized
// by a variable or a column of a fitted model's results (scores, residuals, clusters...), the
// selection shared with the project's models. The autocorrelation view gives Moran's I of a
// variable or of a regression's residuals, its correlogram and the semivariogram.
// Imported by app.js.
import { t, ajouterTextes, libelleResultat } from './i18n.js';
import { el, fmt, decimal, entreNombres } from './ui.js';
import { VUES, PALETTE, choix, outils, couleursTheme, tracer, brancherSelection, tableau, valeurP, uniteAxe, etendue } from './panneaux.js';
import { ICONES_VUES } from './commandes.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const SELECTION = '#e4572e';
const GRIS = '#9aa7b8';
const caseACocher = (libelle, valeur, changer, titre) => el('label', { class: 'champ', title: titre || '' },
  el('input', { type: 'checkbox', checked: valeur, onchange: (e) => changer(e.target.checked) }), libelle);

// The tile backgrounds (MapLibre styles); index.html's Content-Security-Policy allows these hosts only.
const WMTS_IGN = (couche, format) => `https://data.geopf.fr/wmts?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=${couche}&STYLE=normal`
  + `&TILEMATRIXSET=PM&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}&FORMAT=${format}`;
const raster = (tuiles, attribution) => ({ version: 8, sources: { fond: { type: 'raster', tiles: [tuiles], tileSize: 256, attribution } },
  layers: [{ id: 'fond', type: 'raster', source: 'fond' }] });
const FONDS = {
  osm: () => 'open-street-map',  // (Plotly's own style, with OpenStreetMap's attribution)
  ign_plan: () => raster(WMTS_IGN('GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2', 'image/png'), '© IGN – Plan IGN'),
  ign_photo: () => raster(WMTS_IGN('ORTHOIMAGERY.ORTHOPHOTOS', 'image/jpeg'), '© IGN – Orthophotographies'),
};

// What the project's coordinates are, the proposal and the systems (R: coordonnees), kept until
// the project's data change (app.js: oublierCalculs forgets loc.cache).
async function infosCoordonnees(ctx, pid, cache) {
  if (!cache.coordonnees) cache.coordonnees = await ctx.demander('coordonnees', { projet: pid });
  return cache.coordonnees;
}

// The coordinates' form: x, y and their system, proposed from the variables' names the first
// time; applied (R: coordonnees with regler, undone by Ctrl+Z) and the project's summary read again.
function formulaireCoordonnees(ctx, pid, infos, loc, apres) {
  const proj = ctx.projet(pid);
  const quantitatives = proj.variables.filter((v) => !v.qualitative);
  const actuelles = infos.coordonnees || infos.proposition || {};
  loc.cx ??= actuelles.x ?? (quantitatives[0] || {}).uid;
  loc.cy ??= actuelles.y ?? (quantitatives[1] || quantitatives[0] || {}).uid;
  loc.csys ??= String(actuelles.systeme ?? 4326);
  const options = quantitatives.map((v) => [v.uid, v.nom]);
  const systemes = liste(infos.systemes).map((s) => [String(s.code), `${s.nom} (EPSG:${s.code})`]).concat([['plan', t('carte_plan')]]);
  if (!systemes.some(([c]) => c === loc.csys)) systemes.unshift([loc.csys, `EPSG:${loc.csys}`]);
  const erreur = el('span', { class: 'erreur' });
  const appliquer = el('button', { class: 'principal', onclick: async () => {
    erreur.textContent = '';
    try {
      await ctx.occupe(() => ctx.demander('coordonnees', { projet: pid, regler: true, x: Number(loc.cx), y: Number(loc.cy), systeme: loc.csys }));
      await ctx.apresConsole([pid]);
      apres();
    } catch (e) { erreur.textContent = e.message; }
  } }, t('carte_appliquer'));
  const autre = el('input', { type: 'text', placeholder: 'EPSG', style: 'width:6em', title: t('carte_autre_code'),
    onchange: (e) => { const c = e.target.value.replace(/^EPSG:/i, '').trim(); if (c) { loc.csys = c; ctx.rafraichir(); } } });
  return el('div', { class: 'carte-coordonnees' }, outils(
    choix(t('carte_x'), options, loc.cx, (v) => { loc.cx = Number(v); }),
    choix(t('carte_y'), options, loc.cy, (v) => { loc.cy = Number(v); }),
    choix(t('carte_systeme'), systemes, loc.csys, (v) => { loc.csys = v; }), autre, appliquer, erreur),
  el('p', { class: 'note' }, t('carte_coordonnees_note')));
}

// The values of a colour or size source for the observations `uids`: a variable of the project
// ('v<uid>', fetched from R once per data revision) or a column of a fitted model's results
// ('m<id>|<column>', matched by uid). null until they arrive.
function valeursSource(ctx, pid, source, uids, cache) {
  if (!source) return null;
  if (source.startsWith('m')) {
    const [id, col] = source.slice(1).split('|');
    const m = ctx.modele(id);
    if (!m || !m.observations || !m.observations[col]) return uids.map(() => null);
    const index = new Map(liste(m.observations.uid).map((u, i) => [u, i]));
    const v = liste(m.observations[col]);
    return uids.map((u) => (index.has(u) ? v[index.get(u)] ?? null : null));
  }
  const cle = `${source}:${uids.length}`;
  cache.valeurs ??= {};
  if (cache.valeurs[cle] !== undefined) return cache.valeurs[cle];
  cache.valeurs[cle] = null;
  ctx.valeurs(pid, Number(source.slice(1)), uids).then((r) => { cache.valeurs[cle] = liste(r.valeurs); ctx.rafraichir(); });
  return null;
}

// The sources offered: the project's variables, then each fitted model's numeric (or class)
// columns, under its name.
function sources(ctx, pid, quantitativesSeules = false) {
  const proj = ctx.projet(pid);
  const vars = proj.variables.filter((v) => !quantitativesSeules || !v.qualitative).map((v) => [`v${v.uid}`, v.nom]);
  const modeles = ctx.tousModeles().filter((m) => m.projet === pid && m.observations);
  const cols = modeles.flatMap((m) => Object.keys(m.observations).filter((c) => !['uid', 'nom'].includes(c)
    && liste(m.observations[c]).some((x) => (quantitativesSeules ? typeof x === 'number' : x !== null)))
    .map((c) => [`m${m.id}|${c}`, `${m.nom} · ${libelleResultat(c)}`]));
  return [...vars, ...cols];
}

// ------------------------------------------------------------ the map

async function rendreCarte(conteneur, ctx, p, loc) {
  const pid = p.projet;
  const cache = (loc.cache ??= {});
  let infos;
  try { infos = await infosCoordonnees(ctx, pid, cache); } catch (e) { conteneur.replaceChildren(el('p', { class: 'erreur' }, e.message)); return; }
  const changer = () => { loc.cache = {}; loc.modifier = false; ctx.rafraichir(); };
  if (!infos.coordonnees || loc.modifier) {
    conteneur.replaceChildren(...[el('h3', {}, t('carte_titre_coordonnees')), formulaireCoordonnees(ctx, pid, infos, loc, changer),
      infos.coordonnees ? el('button', { onclick: () => { loc.modifier = false; ctx.rafraichir(); } }, t('annuler')) : null].filter(Boolean));
    return;
  }
  if (!cache.points) {
    try { cache.points = await ctx.demander('carte', { projet: pid }); } catch (e) { conteneur.replaceChildren(el('p', { class: 'erreur' }, e.message)); return; }
  }
  const r = cache.points;
  const U = liste(r.uid);
  const N = liste(r.nom);
  const X = liste(r.x);
  const Y = liste(r.y);
  const LON = liste(r.lon);
  const LAT = liste(r.lat);
  const avec = U.map((_, i) => i).filter((i) => X[i] !== null && Y[i] !== null);
  const geo = r.systeme.type !== 'plan';
  if (!geo || !FONDS[loc.fond]) loc.fond = '';
  const opts = sources(ctx, pid);
  if (loc.couleur && !opts.some(([v]) => v === loc.couleur)) loc.couleur = '';
  if (loc.taille && !sources(ctx, pid, true).some(([v]) => v === loc.taille)) loc.taille = '';
  const uids = avec.map((i) => U[i]);
  const couleurs = valeursSource(ctx, pid, loc.couleur, uids, cache);
  const tailles = valeursSource(ctx, pid, loc.taille, uids, cache);
  const co = r.coordonnees;
  const barre = outils(
    el('span', { class: 'infos', title: r.systeme.nom }, t('carte_resume', { x: co.nom_x, y: co.nom_y, systeme: r.systeme.nom })),
    el('button', { onclick: () => { loc.modifier = true; ctx.rafraichir(); } }, t('carte_changer')),
    choix(t('couleur'), [['', t('aucune')], ...opts], loc.couleur || '', (v) => { loc.couleur = v; ctx.rafraichir(); }),
    choix(t('carte_taille'), [['', t('aucune')], ...sources(ctx, pid, true)], loc.taille || '', (v) => { loc.taille = v; ctx.rafraichir(); }),
    caseACocher(t('etiquettes'), !!loc.etiquettes, (v) => { loc.etiquettes = v; ctx.rafraichir(); }),
    geo ? el('span', { title: t('carte_fond_aide') }, choix(t('carte_fond'), [['', t('carte_fond_aucun')], ['osm', 'OpenStreetMap'], ['ign_plan', t('carte_ign_plan')],
      ['ign_photo', t('carte_ign_photo')]], loc.fond, (v) => { loc.fond = v; ctx.rafraichir(); })) : null,
    el('button', { title: t('autocorrelation_spatiale_aide'), onclick: () => ctx.ouvrirVue('autocorrelation_spatiale', null,
      loc.couleur && loc.couleur.startsWith('v') ? { source: loc.couleur } : null, pid) }, t('autocorrelation_spatiale')),
    // (raster.js: an elevation, a land cover... at each point, as new variables of the project)
    el('button', { title: t('raster_carte_aide'), onclick: () => ctx.valeursRaster(pid) }, t('raster_valeurs')));
  const graphe = el('div', { class: 'graphe' });
  const sans = U.length - avec.length;
  const sel = new Set(ctx.selection(pid));
  const note = el('p', { class: 'note' }, [sans ? t('carte_sans_coordonnees', { n: sans }) : '', loc.fond ? t('carte_fond_note') : '',
    sel.size ? `${sel.size} ${t('selectionnees')}` : ''].filter(Boolean).join(' '));
  conteneur.replaceChildren(barre, graphe, note);
  if (!avec.length) { graphe.replaceChildren(el('p', { class: 'infos' }, t('carte_vide'))); return; }

  // positions: longitude and latitude on a background; otherwise the coordinates as they are
  const px = loc.fond ? avec.map((i) => LON[i]) : avec.map((i) => X[i]);
  const py = loc.fond ? avec.map((i) => LAT[i]) : avec.map((i) => Y[i]);
  const nomCouleur = (opts.find(([v]) => v === loc.couleur) || [])[1] || '';
  const numerique = couleurs && couleurs.some((v) => typeof v === 'number') && couleurs.every((v) => v === null || typeof v === 'number');
  // sizes: 6 to 22 px by the square root of the values (their minimum at 6); none: 8
  let taillesPx = avec.map(() => 8);
  if (tailles && tailles.some((v) => typeof v === 'number')) {
    const ok = tailles.filter((v) => typeof v === 'number' && Number.isFinite(v));
    const [min, max] = etendue(ok);
    taillesPx = tailles.map((v) => (typeof v === 'number' && max > min ? 6 + 16 * Math.sqrt((v - min) / (max - min)) : typeof v === 'number' ? 10 : 4));
  }
  const type = loc.fond ? 'scattermap' : 'scattergl';
  const pos = (idx) => (loc.fond ? { lon: idx.map((k) => px[k]), lat: idx.map((k) => py[k]) } : { x: idx.map((k) => px[k]), y: idx.map((k) => py[k]) });
  const coordTexte = (k) => (loc.fond ? `${fmt(px[k], 5)}${entreNombres()}${fmt(py[k], 5)}`
    : `${co.nom_x} = ${decimal(+Number(X[avec[k]]).toPrecision(8))}${entreNombres()}${co.nom_y} = ${decimal(+Number(Y[avec[k]]).toPrecision(8))}`);
  const valeurTexte = (k) => (couleurs && couleurs[k] !== null && couleurs[k] !== undefined ? `<br>${nomCouleur} = ${typeof couleurs[k] === 'number' ? decimal(+couleurs[k].toPrecision(4)) : couleurs[k]}` : '');
  const trace = (idx, nom, couleur, extra = {}) => ({
    type, mode: loc.etiquettes && !loc.fond ? 'markers+text' : 'markers', name: nom, ...pos(idx),
    customdata: idx.map((k) => uids[k]), hovertext: idx.map((k) => `<b>${N[avec[k]]}</b><br>${coordTexte(k)}${valeurTexte(k)}`),
    hovertemplate: '%{hovertext}<extra></extra>', text: loc.etiquettes ? idx.map((k) => N[avec[k]]) : undefined, textposition: 'top center',
    textfont: { size: 10 },
    marker: { size: idx.map((k) => taillesPx[k]), color: couleur, opacity: sel.size ? idx.map((k) => (sel.has(uids[k]) ? 1 : 0.35)) : 0.85, ...extra },
  });
  const tous = avec.map((_, k) => k);
  const traces = [];
  if (!couleurs) {
    traces.push(trace(tous, t('observations'), PALETTE[0]));
  } else if (numerique) {
    const connus = tous.filter((k) => couleurs[k] !== null);
    const inconnus = tous.filter((k) => couleurs[k] === null);
    traces.push(trace(connus, nomCouleur, connus.map((k) => couleurs[k]), { colorscale: 'Viridis', showscale: true,
      colorbar: { title: { text: `${nomCouleur}${uniteAxe(ctx, pid, nomCouleur)}`, side: 'right' }, thickness: 12 } }));
    if (inconnus.length) traces.push(trace(inconnus, t('carte_manquante'), GRIS));
  } else {
    const groupes = new Map();
    tous.forEach((k) => { const c = couleurs[k] === null ? '—' : String(couleurs[k]); if (!groupes.has(c)) groupes.set(c, []); groupes.get(c).push(k); });
    [...groupes.entries()].sort((a, b) => a[0].localeCompare(b[0], undefined, { numeric: true }))
      .forEach(([nom, idx], j) => traces.push(trace(idx, nom, nom === '—' ? GRIS : PALETTE[j % PALETTE.length])));
  }
  // on a background, a white halo under each point, so that dark colours show on aerial photographs
  if (loc.fond) traces.unshift({ type, mode: 'markers', ...pos(tous), hoverinfo: 'skip', showlegend: false, name: '',
    marker: { size: tous.map((k) => taillesPx[k] + 4), color: '#ffffff', opacity: 0.9 } });
  // the selection on top, in its colour
  const choisis = tous.filter((k) => sel.has(uids[k]));
  if (choisis.length) traces.push({ ...trace(choisis, t('selection'), SELECTION), showlegend: false, hoverinfo: 'skip', hovertemplate: undefined,
    marker: { size: choisis.map((k) => taillesPx[k] + 3), color: SELECTION, opacity: 1 } });
  const c = couleursTheme(ctx);
  const titre = `${ctx.projet(pid).nom} · ${t('carte')}${nomCouleur ? ` · ${nomCouleur}` : ''}`;
  if (loc.fond) {
    // centre and zoom from the extent of the points (MapLibre's Web Mercator: 512 px per world at zoom 0)
    const lons = px.filter(Number.isFinite);
    const lats = py.filter(Number.isFinite);
    const merc = (l) => Math.log(Math.tan(Math.PI / 4 + (l * Math.PI) / 360));
    // (the plot's area: the panel less the colour bar and the margins)
    const largeur = Math.max(250, (graphe.clientWidth || conteneur.clientWidth || 600) - (numerique ? 110 : 30));
    const hauteur = Math.max(200, (graphe.clientHeight || conteneur.clientHeight - 90 || 400) - 50);
    // (etendue: thousands of points, which Math.min(...) would pass one by one as arguments)
    const [lon0, lon1] = etendue(lons);
    const [lat0, lat1] = etendue(lats);
    const dl = Math.max(1e-4, lon1 - lon0);
    const dm = Math.max(1e-6, merc(lat1) - merc(lat0));
    const zoom = Math.max(0, Math.min(18, Math.log2(Math.min((largeur * 360) / (512 * dl), (hauteur * 2 * Math.PI) / (512 * dm))) - 0.15));
    const centre = { lon: (lon1 + lon0) / 2,
      lat: (360 / Math.PI) * Math.atan(Math.exp((merc(lat1) + merc(lat0)) / 2)) - 90 };
    tracer(ctx, graphe, traces, { title: { text: titre }, margin: { l: 8, r: 8, t: 34, b: 8 }, dragmode: 'pan',
      map: { style: FONDS[loc.fond](), center: loc.vueCarte?.fond === loc.fond ? loc.vueCarte.center : centre,
        zoom: loc.vueCarte?.fond === loc.fond ? loc.vueCarte.zoom : zoom },
      legend: { orientation: 'v', x: 1, xanchor: 'right', y: 1, bgcolor: c.fond } });
    graphe.on('plotly_relayout', (e) => {  // (the user's pan and zoom, kept while the view is redrawn)
      if (e['map.center'] || e['map.zoom'] !== undefined) loc.vueCarte = { fond: loc.fond, center: e['map.center'] || graphe.layout.map.center, zoom: e['map.zoom'] ?? graphe.layout.map.zoom };
    });
  } else {
    // the same scale on both axes; with longitudes and latitudes, a degree of longitude is
    // shorter by the cosine of the latitude
    const lonlat = r.systeme.type === 'lonlat';
    const moyenne = py.reduce((s, v) => s + v, 0) / py.length;
    tracer(ctx, graphe, traces, { title: { text: titre },
      xaxis: { title: { text: co.nom_x }, zeroline: false, constrain: 'domain' },
      yaxis: { title: { text: co.nom_y }, zeroline: false, scaleanchor: 'x', scaleratio: lonlat ? 1 / Math.cos((moyenne * Math.PI) / 180) : 1, constrain: 'domain' } });
  }
  brancherSelection(graphe, ctx, pid);
}

// ------------------------------------------------------------ spatial autocorrelation

// Moran's I (randomisation and permutations; for a linear model, the test that accounts for the
// fit), its correlogram and the semivariogram, of a variable ('v<uid>') or of a regression's
// residuals ('r:<id>', or its quantile residuals 'q:<id>'), with a neighbourhood: k nearest
// neighbours, a distance band or inverse distances.
async function rendreAutocorrelationSpatiale(conteneur, ctx, p, loc) {
  const pid = p.projet;
  const cache = (loc.cache ??= {});
  let infos;
  try { infos = await infosCoordonnees(ctx, pid, cache); } catch (e) { conteneur.replaceChildren(el('p', { class: 'erreur' }, e.message)); return; }
  if (!infos.coordonnees) {
    conteneur.replaceChildren(el('p', { class: 'infos' }, t('autocorrelation_sans_coordonnees')),
      el('button', { onclick: () => ctx.ouvrirVue('carte', null, null, pid) }, t('carte')));
    return;
  }
  const proj = ctx.projet(pid);
  const regressions = ctx.tousModeles().filter((m) => m.projet === pid && m.regression);
  // (the coordinates themselves left out: they vary in space by definition)
  const co = infos.coordonnees;
  const options = [...regressions.flatMap((m) => [[`r:${m.id}`, `${m.nom} · ${t('residus')}`]].concat(m.residus_quantiles ? [[`q:${m.id}`, `${m.nom} · ${t('autocorrelation_residus_quantiles')}`]] : [])),
    ...proj.variables.filter((v) => !v.qualitative && v.uid !== co.x && v.uid !== co.y).map((v) => [`v${v.uid}`, v.nom])];
  if (!options.length) { conteneur.replaceChildren(el('p', { class: 'infos' }, t('series_sans_mesure'))); return; }
  if (!loc.source || !options.some(([v]) => v === loc.source)) loc.source = p.modele && options.some(([v]) => v === `r:${p.modele}`) ? `r:${p.modele}` : options[0][0];
  loc.methode ??= 'voisins';
  loc.k ??= 4;
  loc.standardiser ??= true;
  loc.permutations ??= 999;
  const q = { projet: pid, voisinage: { methode: loc.methode, k: Number(loc.k), standardiser: !!loc.standardiser,
    ...(loc.methode !== 'voisins' && Number(loc.portee) > 0 ? { portee: Number(loc.portee) * 1000 } : {}) },
  permutations: Number(loc.permutations), par_lieu: !!loc.parLieu };
  if (loc.source.startsWith('v')) q.variable = Number(loc.source.slice(1));
  else { q.modele = loc.source.slice(2); q.residus = loc.source.startsWith('q') ? 'quantiles' : 'standardises'; }
  const barre = outils(
    choix(t('autocorrelation_valeurs'), options, loc.source, (v) => { loc.source = v; ctx.rafraichir(); }),
    choix(t('autocorrelation_voisinage'), [['voisins', t('autocorrelation_voisins')], ['bande', t('autocorrelation_bande')], ['inverse', t('autocorrelation_inverse')]],
      loc.methode, (v) => { loc.methode = v; ctx.rafraichir(); }),
    loc.methode === 'voisins' ? el('label', { class: 'champ' }, el('span', {}, 'k'), el('input', { type: 'number', min: 1, max: 50, value: loc.k, style: 'width:4em',
      onchange: (e) => { loc.k = Math.max(1, Math.round(Number(e.target.value) || 4)); ctx.rafraichir(); } }))
      : el('label', { class: 'champ', title: t('autocorrelation_portee_aide') }, el('span', {}, t('autocorrelation_portee')),
        el('input', { type: 'number', min: 0, step: 'any', value: loc.portee ?? '', placeholder: t('carte_auto'), style: 'width:6em',
          onchange: (e) => { loc.portee = e.target.value; ctx.rafraichir(); } })),
    caseACocher(t('autocorrelation_standardiser'), !!loc.standardiser, (v) => { loc.standardiser = v; ctx.rafraichir(); }, t('autocorrelation_standardiser_aide')),
    choix(t('autocorrelation_n_permutations'), [[0, '0'], [99, '99'], [999, '999'], [9999, '9 999']], loc.permutations, (v) => { loc.permutations = Number(v); ctx.rafraichir(); }),
    caseACocher(t('autocorrelation_par_lieu'), !!loc.parLieu, (v) => { loc.parLieu = v; ctx.rafraichir(); }, t('autocorrelation_par_lieu_aide')));
  const cle = JSON.stringify(q);
  if (cache.cle !== cle) {
    conteneur.replaceChildren(barre, el('p', { class: 'infos attente' }, t('calcul')));
    try { cache.res = await ctx.demander('autocorrelation_spatiale', q); cache.erreur = null; } catch (e) { cache.res = null; cache.erreur = e.message; }
    cache.cle = cle;
  }
  if (!cache.res) { conteneur.replaceChildren(barre, el('p', { class: 'erreur' }, `${t('erreur')} : ${cache.erreur}`)); return; }
  const r = cache.res;
  const mo = r.moran;
  const km = r.distance_max > 5000;
  const unite = km ? 'km' : 'm';
  const dist = (d) => (d === null || d === undefined ? '—' : km ? fmt(d / 1000, 2) : fmt(d, 0));
  const v = r.voisinage;
  const ligne = (titre, x) => el('p', {}, el('b', {}, titre), ' ', t('autocorrelation_ligne', { I: fmt(x.I, 4), E: fmt(x.esperance, 4), z: fmt(x.z, 2), p: valeurP(x.p) }));
  const resume = el('div', { class: 'resume-autocorrelation' },
    ligne(t('autocorrelation_moran', { nom: r.nom }), mo),
    mo.p_permutations !== undefined && mo.p_permutations !== null ? el('p', {}, t('autocorrelation_permutations', { n: mo.permutations, p: valeurP(mo.p_permutations) })) : null,
    mo.modele ? ligne(t('autocorrelation_modele'), mo.modele) : null,
    el('p', { class: 'note' }, [t(v.methode === 'inverse' && (v.portee === null || v.portee === undefined) ? 'autocorrelation_methode_inverse_tout' : `autocorrelation_methode_${v.methode}`,
      { k: v.k, portee: dist(v.portee), unite }),
      t('autocorrelation_note_voisins', { n: r.n, moyenne: fmt(v.voisins, 1) }),
      v.sans_voisin ? t('autocorrelation_sans_voisin', { n: v.sans_voisin }) : '',
      r.laissees ? t('autocorrelation_laissees', { n: r.laissees }) : '',
      r.lieux ? t('autocorrelation_lieux', { n: r.n, obs: r.observations }) : r.nulles ? t('autocorrelation_nulles', { n: r.nulles }) : '',
      t('autocorrelation_note_p')].filter(Boolean).join(' ')));
  const graphes = el('div', { class: 'graphes-cote', style: { display: 'flex', gap: '8px', flex: '1 1 auto', minHeight: '240px' } });
  const g1 = el('div', { class: 'graphe', style: { flex: '1 1 50%', minWidth: '0' } });
  const g2 = el('div', { class: 'graphe', style: { flex: '1 1 50%', minWidth: '0' } });
  graphes.append(g1, g2);
  const bas = el('div', { class: 'bas', style: { flex: '0 1 auto', maxHeight: '35%' } });
  conteneur.replaceChildren(barre, resume, graphes, bas);
  const c = couleursTheme(ctx);
  const cg = r.correlogramme;
  const D = liste(cg.distance);
  const I = liste(cg.I);
  const PH = liste(cg.p_holm);
  const E = liste(cg.esperance);
  tracer(ctx, g1, [
    { type: 'scatter', mode: 'lines+markers', name: 'I', x: D.map((d) => (km ? d / 1000 : d)), y: I, line: { color: c.doux, width: 1 },
      marker: { size: 9, color: PH.map((x) => (x !== null && x <= 0.05 ? PALETTE[1] : PALETTE[0])) },
      customdata: liste(cg.paires).map((n, i) => [n, valeurP(liste(cg.p)[i]), valeurP(PH[i])]),
      hovertemplate: `%{x:.3~f} ${unite}<br>I = %{y:.4f}<br>${t('carte_paires')} : %{customdata[0]}<br>p = %{customdata[1]} (Holm : %{customdata[2]})<extra></extra>` },
    { type: 'scatter', mode: 'lines', name: t('autocorrelation_esperance'), x: D.map((d) => (km ? d / 1000 : d)), y: E,
      line: { color: c.doux, dash: 'dash', width: 1 }, hoverinfo: 'skip' }],
  { title: { text: `${t('autocorrelation_correlogramme')} · ${r.nom}` }, dragmode: 'zoom', showlegend: false,
    xaxis: { title: { text: `${t('carte_distance')} (${unite})` } }, yaxis: { title: { text: t('autocorrelation_i') } } });
  const vg = r.variogramme;
  const variance = liste(r.valeurs).reduce((s, x, _, a) => s + (x - a.reduce((u, y) => u + y, 0) / a.length) ** 2, 0) / Math.max(1, liste(r.valeurs).length - 1);
  const VD = liste(vg.distance).map((d) => (km ? d / 1000 : d));
  const NP = liste(vg.paires);
  const maxNP = Math.max(1, ...NP);
  tracer(ctx, g2, [
    { type: 'scatter', mode: 'markers', name: 'γ', x: VD, y: liste(vg.gamma), customdata: NP,
      marker: { color: PALETTE[0], size: NP.map((n) => 5 + 10 * Math.sqrt(n / maxNP)) },
      hovertemplate: `%{x:.3~f} ${unite}<br>γ = %{y:.4~g}<br>${t('carte_paires')} : %{customdata}<extra></extra>` },
    { type: 'scatter', mode: 'lines', name: t('autocorrelation_variance'), x: [0, Math.max(...VD, 0)], y: [variance, variance],
      line: { color: c.doux, dash: 'dash', width: 1 }, hoverinfo: 'skip' }],
  { title: { text: `${t('autocorrelation_variogramme')} · ${r.nom}` }, dragmode: 'zoom', showlegend: false,
    xaxis: { title: { text: `${t('carte_distance')} (${unite})` }, rangemode: 'tozero' }, yaxis: { title: { text: 'γ' }, rangemode: 'tozero' } });
  const col = (k) => liste(cg[k]);
  tableau(bas, ctx, (loc.table ??= {}), {
    n: col('classe').length, nom: t('autocorrelation_correlogramme'),
    colonnes: [{ cle: 'classe', titre: t('carte_classe'), num: true, decimales: 0 },
      { cle: 'de', titre: `${t('carte_de')} (${unite})`, num: true, format: dist }, { cle: 'a', titre: `${t('carte_a')} (${unite})`, num: true, format: dist },
      { cle: 'paires', titre: t('carte_paires'), num: true, decimales: 0 }, { cle: 'I', titre: 'I', num: true, decimales: 4 },
      { cle: 'z', titre: 'z', num: true, decimales: 2 }, { cle: 'p', titre: 'p', num: true, format: valeurP },
      { cle: 'p_holm', titre: 'p (Holm)', num: true, format: valeurP, alerte: (i) => col('p_holm')[i] !== null && col('p_holm')[i] <= 0.05 }],
    valeur: (i, k) => col(k)[i] ?? null,
  });
}

VUES.carte = { rendre: rendreCarte, titre: (_, p) => `${p.nom || ''} · ${t('carte')}`, projet: true, selection: true };
VUES.autocorrelation_spatiale = { rendre: rendreAutocorrelationSpatiale, projet: true,
  titre: (m, p) => `${(m && m.nom) || p.nom || ''} · ${t('autocorrelation_spatiale')}` };
ICONES_VUES.carte = 'map';
ICONES_VUES.autocorrelation_spatiale = 'radar';

ajouterTextes({
  carte: 'Carte', carte_titre_coordonnees: 'Coordonnées des observations', carte_x: 'X (est ou longitude)', carte_y: 'Y (nord ou latitude)',
  carte_systeme: 'Système', carte_plan: 'Coordonnées planes en mètres, autre système', carte_appliquer: 'Utiliser ces coordonnées',
  carte_autre_code: 'Un autre code EPSG : une zone UTM (326xx au nord, 327xx au sud), 4258, 4171, 9794…',
  carte_coordonnees_note: 'Deux variables du projet donnent la position de chaque observation, dans leur système de coordonnées : des longitudes et latitudes en degrés (WGS 84), du Lambert-93, une zone UTM… Elles sont enregistrées avec le projet. Un système plan non listé garde des distances justes, sans fond de carte.',
  carte_resume: '{x}, {y} · {systeme}', carte_changer: 'Changer…', carte_taille: 'Taille', carte_fond: 'Fond',
  carte_fond_aucun: 'Aucun (hors ligne)', carte_ign_plan: 'IGN – plan', carte_ign_photo: 'IGN – photographies aériennes',
  carte_fond_aide: 'Un fond de carte se télécharge en ligne : OpenStreetMap (tile.openstreetmap.org) ou l’IGN (data.geopf.fr, en France) reçoivent alors la zone affichée. Sans fond, rien ne quitte l’ordinateur.',
  carte_fond_note: 'Fond de carte en ligne : son serveur reçoit la zone affichée.',
  carte_sans_coordonnees: '{n} observation(s) sans coordonnées ne sont pas placées.', carte_vide: 'Aucune observation n’a de coordonnées.',
  autocorrelation_spatiale: 'Autocorrélation spatiale',
  autocorrelation_spatiale_aide: 'L’I de Moran d’une variable ou des résidus d’une régression, son corrélogramme et le semi-variogramme',
  autocorrelation_sans_coordonnees: 'Ce projet n’a pas encore de coordonnées : choisissez-les dans la carte.',
  autocorrelation_valeurs: 'Valeurs', autocorrelation_residus_quantiles: 'résidus quantiles', autocorrelation_voisinage: 'Voisinage',
  autocorrelation_voisins: 'k plus proches voisins', autocorrelation_bande: 'Distance maximale', autocorrelation_inverse: 'Inverse de la distance',
  autocorrelation_portee: 'Portée (km)', autocorrelation_portee_aide: 'Vide : la plus petite distance qui donne un voisin à chaque observation (pour l’inverse de la distance : toutes les paires).',
  autocorrelation_standardiser: 'Lignes de somme 1', autocorrelation_standardiser_aide: 'Les poids de chaque observation ramenés à une somme de 1 (style « W » de spdep) ; sinon les poids tels quels (« B »).',
  autocorrelation_par_lieu: 'Moyenne par lieu', autocorrelation_par_lieu_aide: 'Les observations au même endroit (une placette suivie plusieurs années) remplacées par la moyenne de leurs valeurs : sinon elles sont les plus proches voisines les unes des autres.',
  autocorrelation_moran: 'I de Moran de {nom} :', autocorrelation_modele: 'Test des résidus qui tient compte de l’ajustement (modèle linéaire) :',
  autocorrelation_ligne: 'I = {I}, attendu {E} sans autocorrélation, z = {z}, p = {p}.',
  autocorrelation_permutations: 'Par {n} permutations : p = {p}.',
  autocorrelation_note_voisins: '{n} observations, {moyenne} voisins en moyenne.',
  autocorrelation_methode_voisins: 'Voisinage : les {k} plus proches voisines de chaque observation, rendues réciproques.',
  autocorrelation_methode_bande: 'Voisinage : les observations à moins de {portee} {unite}.',
  autocorrelation_methode_inverse: 'Poids : l’inverse de la distance, jusqu’à {portee} {unite}.',
  autocorrelation_methode_inverse_tout: 'Poids : l’inverse de la distance, entre toutes les paires.',
  autocorrelation_n_permutations: 'Permutations', autocorrelation_i: 'I de Moran',
  autocorrelation_sans_voisin: '{n} observation(s) sans voisin.', autocorrelation_laissees: '{n} observation(s) sans coordonnées ou sans valeur laissées de côté.',
  autocorrelation_lieux: '{n} lieux pour {obs} observations (moyenne par lieu).',
  autocorrelation_nulles: '{n} paires d’observations au même endroit : la même placette plusieurs années ? « Moyenne par lieu » les regroupe.',
  autocorrelation_note_p: 'p unilatérale (autocorrélation positive), comme spdep ; dans le corrélogramme, bilatérale et corrigée par Holm sur les classes.',
  autocorrelation_esperance: 'Sans autocorrélation', autocorrelation_correlogramme: 'Corrélogramme', autocorrelation_variogramme: 'Semi-variogramme',
  autocorrelation_variance: 'Variance', carte_paires: 'paires', carte_distance: 'Distance', carte_classe: 'Classe', carte_de: 'De', carte_a: 'À',
  carte_auto: 'auto', carte_manquante: 'Valeur manquante',
}, {
  carte: 'Map', carte_titre_coordonnees: 'Coordinates of the observations', carte_x: 'X (east or longitude)', carte_y: 'Y (north or latitude)',
  carte_systeme: 'System', carte_plan: 'Plane coordinates in metres, another system', carte_appliquer: 'Use these coordinates',
  carte_autre_code: 'Another EPSG code: a UTM zone (326xx north, 327xx south), 4258, 4171, 9794…',
  carte_coordonnees_note: 'Two of the project’s variables give each observation’s position, in their coordinate system: longitudes and latitudes in degrees (WGS 84), Lambert-93, a UTM zone… They are saved with the project. A plane system not listed keeps the distances right, without a background map.',
  carte_resume: '{x}, {y} · {systeme}', carte_changer: 'Change…', carte_taille: 'Size', carte_fond: 'Background',
  carte_fond_aucun: 'None (offline)', carte_ign_plan: 'IGN – map', carte_ign_photo: 'IGN – aerial photographs',
  carte_fond_aide: 'A background map is downloaded: OpenStreetMap (tile.openstreetmap.org) or IGN (data.geopf.fr, in France) then receive the area shown. Without one, nothing leaves the computer.',
  carte_fond_note: 'Online background map: its server receives the area shown.',
  carte_sans_coordonnees: '{n} observation(s) without coordinates are not placed.', carte_vide: 'No observation has coordinates.',
  autocorrelation_spatiale: 'Spatial autocorrelation',
  autocorrelation_spatiale_aide: 'Moran’s I of a variable or of a regression’s residuals, its correlogram and the semivariogram',
  autocorrelation_sans_coordonnees: 'This project has no coordinates yet: choose them in the map.',
  autocorrelation_valeurs: 'Values', autocorrelation_residus_quantiles: 'quantile residuals', autocorrelation_voisinage: 'Neighbourhood',
  autocorrelation_voisins: 'k nearest neighbours', autocorrelation_bande: 'Maximum distance', autocorrelation_inverse: 'Inverse distance',
  autocorrelation_portee: 'Range (km)', autocorrelation_portee_aide: 'Empty: the smallest distance that gives each observation a neighbour (for inverse distance: every pair).',
  autocorrelation_standardiser: 'Rows summing to 1', autocorrelation_standardiser_aide: 'Each observation’s weights scaled to sum to 1 (spdep’s “W” style); otherwise the weights as they are (“B”).',
  autocorrelation_par_lieu: 'Mean by place', autocorrelation_par_lieu_aide: 'Observations at the same place (a plot surveyed several years) replaced by the mean of their values: otherwise they are each other’s nearest neighbours.',
  autocorrelation_moran: 'Moran’s I of {nom}:', autocorrelation_modele: 'Test of the residuals that accounts for the fit (linear model):',
  autocorrelation_ligne: 'I = {I}, {E} expected without autocorrelation, z = {z}, p = {p}.',
  autocorrelation_permutations: 'By {n} permutations: p = {p}.',
  autocorrelation_note_voisins: '{n} observations, {moyenne} neighbours on average.',
  autocorrelation_methode_voisins: 'Neighbourhood: the {k} nearest neighbours of each observation, made mutual.',
  autocorrelation_methode_bande: 'Neighbourhood: the observations within {portee} {unite}.',
  autocorrelation_methode_inverse: 'Weights: the inverse distance, up to {portee} {unite}.',
  autocorrelation_methode_inverse_tout: 'Weights: the inverse distance, between every pair.',
  autocorrelation_n_permutations: 'Permutations', autocorrelation_i: 'Moran’s I',
  autocorrelation_sans_voisin: '{n} observation(s) without a neighbour.', autocorrelation_laissees: '{n} observation(s) without coordinates or without a value left out.',
  autocorrelation_lieux: '{n} places for {obs} observations (mean by place).',
  autocorrelation_nulles: '{n} pairs of observations at the same place: the same plot over several years? “Mean by place” groups them.',
  autocorrelation_note_p: 'One-sided p (positive autocorrelation), as spdep; in the correlogram, two-sided and corrected by Holm over the classes.',
  autocorrelation_esperance: 'Without autocorrelation', autocorrelation_correlogramme: 'Correlogram', autocorrelation_variogramme: 'Semivariogram',
  autocorrelation_variance: 'Variance', carte_paires: 'pairs', carte_distance: 'Distance', carte_classe: 'Class', carte_de: 'From', carte_a: 'To',
  carte_auto: 'auto', carte_manquante: 'Missing value',
});
