// Figures for publication (Plots › Compose a figure…, or a plot's menu › Add to a figure): plots
// of the project's models and of the project itself (scores, loadings, effects, a map, series…)
// side by side in a grid, each with its letter, at a journal's width in millimetres, their text
// in points and their lines as wide as wanted, then written as a PDF (vectors: printed by the
// desktop app at the figure's size; in a browser, through its print window), an SVG, or a PNG or
// TIFF at 300 or 600 dpi with that resolution written in the file. Kept in the project
// (p$figures: moteur/serveur.R, figure*), so saved with it and exported again once the models are
// refitted: a panel names its model by its number in the project (its latest fit; computed, its
// views not opened, when it has none) and keeps its view's settings. Each plot is drawn off
// screen by its view, as for a report (rapport.js), in the light theme and without the
// selection, then adapted to its cell (composition.js), its SVG nested in the figure's; PNG and
// TIFF are that SVG drawn on a canvas, written by images.js. A map on tiles becomes a plane plot
// whose points are vectors over a picture of its tiles, fetched again at the export's resolution.
import { t, ajouterTextes, langueActuelle, deuxPoints } from './i18n.js';
import { el, icone, dialogue, decimal } from './ui.js';
import { demander } from './api.js';
import { VUES, FORMAT } from './panneaux.js';
import { partiesModele } from './rapport.js';
import { replacerEtiquettes } from './etiquettes.js';
import { vuesDe } from './explorateur.js';
import { PX_PAR_PT, LARGEURS, STYLES_LETTRES, COINS, GRILLE_MAX, completer, grilleProposee, disposer, enPixels, lettre, placeLettre, adapterFigure,
  composerSVG, pagePDF, pixelsImage, carteEnPlan, cadrer, tuilesPour, adresseTuile, placeTuile } from './composition.js';
import { ecrirePNG, ecrireTIFF } from './images.js';

let appli = null;
const ouvertes = new Set();  // the figures open in a panel of this window (their state)
// (a figure changed in another window: its panel here shows it again; app.js keeps its own channel)
const canal = new BroadcastChannel('nuee');
const cleFigure = (pid, id) => `figure:${pid}~${id}`;
const ouverte = (pid, id) => [...ouvertes].find((s) => s.pid === pid && s.id === id);
const EXTENSIONS = { pdf: 'pdf', svg: 'svg', png: 'png', tiff: 'tif' };

export function installerFigures(app) {
  appli = app;
  VUES.figure = {
    rendre: (conteneur, ctx, params, loc) => rendreFigure(app, conteneur, params, loc),
    titre: (_, p, loc = {}) => {
      const s = ouverte(p.id, loc.figure);
      const f = [].concat(p.figures || []).find((x) => x.id === loc.figure);
      return (s ? s.titre : f && f.titre) || t('figure_sans_titre');
    },
    // (global: in a window of its own, it gets every model of the project)
    projet: true, global: true,
    suffixe: (loc) => loc.figure || '',
    fermer: (conteneur) => {
      const s = conteneur.etatFigure;
      conteneur.etatFigure = null;
      if (s) s.fermer();
    },
  };
  Object.assign(app, {
    composerFigure: (pid) => {
      const p = app.projet(pid || app.projetActif());
      if (!p) { app.statut(t('choisir_projet'), true); return; }
      const actif = app.panneauActif();
      nouvelleFigure(app, p.id, app.vuesGraphiques().filter((e) => actif && e.id === actif.id && sonProjet(app, e) === p.id))
        .catch((e) => app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true));
    },
    ouvrirFigure: (pid, id) => app.ouvrirVue('figure', null, { figure: id }, pid),
    menuFigure: (pid, id) => menuFigure(app, pid, id),
    // a plot's menu › Add to a figure: the project's figures, or a new one
    sousMenuFigure: (entree) => {
      const pid = sonProjet(app, entree);
      const p = app.projet(pid);
      if (!p || !entree.graphe) return [{ libelle: t('figure_nouvelle'), actif: false, action: () => {} }];
      const ajout = (id) => () => ajouterA(app, pid, id, entree).catch((e) => app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true));
      const figures = [].concat(p.figures || []);
      return figures.map((f) => ({ libelle: f.titre || t('figure_sans_titre'), icone: 'layout-grid', action: ajout(f.id) }))
        .concat(figures.length ? ['-'] : [], [{ libelle: t('figure_nouvelle'), icone: 'plus',
          action: () => nouvelleFigure(app, pid, [entree]).catch((e) => app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true)) }]);
    },
    // (before a project is saved or closed: what was just changed in its figures goes in first)
    viderFigures: (pid) => Promise.all([...ouvertes].filter((s) => !pid || s.pid === pid).map((s) => s.enregistrer())),
    // (a project read again from its file, app.js: projet_recharge: its figures open here follow it
    // to its new id, a change made meanwhile sent there, else the figure shown as it is there)
    reattacherFigures: async (ancien, nouveau) => {
      for (const s of [...ouvertes].filter((x) => x.pid === ancien)) {
        s.pid = nouveau;
        if (s.modifie) await s.enregistrer(); else await s.recharger();
      }
    },
  });
  // (app.js's channel says when models change, in this window too: a model refitted, its figures
  // drawn again with its new fit)
  canal.addEventListener('message', ({ data: m }) => {
    if (m && m.type === 'figure') for (const s of ouvertes) if (s.pid === m.projet && s.id === m.id && !s.modifie) s.recharger();
    if (m && m.type === 'modeles') for (const s of ouvertes) s.rafraichir();
  });
}

const sonProjet = (app, e) => e.params.projet || ((app.modele(e.params.modele) || {}).projet) || null;

// ------------------------------------------------------------ the panels of a figure

// What a figure keeps of an open view: the view, its model by its number in the project (its
// runtime id when it has none, good for this session only), the view's settings as a session saves
// them (app.js: locEnregistrable), the model ids in them by their numbers too ('m@3|t1': the map
// coloured by a column of M3's results), the zoom of its plot, its aspect on screen.
function panneauDeVue(app, e) {
  const m = e.params.modele ? app.modele(e.params.modele) : null;
  const pid = sonProjet(app, e);
  // (its size as Plotly last drew it: a view in a tab behind another isn't in the page)
  const fl = (e.graphe && e.graphe._fullLayout) || {};
  const pn = { vue: e.vue, loc: versCodes(app, pid, app.locEnregistrable(e.loc || {})), lignes: 1, colonnes: 1,
    aspect: fl.width > 50 && fl.height > 50 ? Math.round((fl.height / fl.width) * 100) / 100 : 0.7 };
  if (m) {
    if (m.numero !== null && m.numero !== undefined) pn.numero = m.numero; else pn.modele = m.id;
    pn.nom = m.nom;
  }
  if (e.graphe && e.graphe._zoom) pn.zoom = JSON.parse(JSON.stringify(e.graphe._zoom));
  return pn;
}

// (plots a model's report leaves out that a figure can take: spectra's profiles, the dendrogram,
// class models' Coomans plot)
const PLUS_QUE_LE_RAPPORT = ['profils', 'dendrogramme', 'coomans'];

// A model's view that isn't open, as a panel: its usual settings, the shape of a plot on screen.
function panneauSansVue(m, vue) {
  const pn = { vue, loc: {}, lignes: 1, colonnes: 1, aspect: 0.7, nom: m.nom };
  if (m.numero !== null && m.numero !== undefined) pn.numero = m.numero; else pn.modele = m.id;
  return pn;
}

// The project's fitted models: the latest fit of each of its models, in their numbers' order, then
// the fits without a number (a model of this session only).
function modelesAjustes(app, pid) {
  const parNumero = new Map();
  const sans = [];
  for (const m of app.modelesDe(pid)) {
    if (m.numero === null || m.numero === undefined) sans.push(m);
    else if (!parNumero.has(m.numero) || numeroId(m.id) > numeroId(parNumero.get(m.numero).id)) parNumero.set(m.numero, m);
  }
  return [...parNumero.values()].sort((a, b) => a.numero - b.numero).concat(sans);
}

// (only a view's own settings name models: the map's colour and size, 'm<id>|<column>', the
// spatial autocorrelation's values, 'r:<id>' and 'q:<id>')
const SOURCE_ID = /^(m|[rq]:)(M\d+)(\|.*)?$/;
const SOURCE_CODE = /^(m|[rq]:)@(\d+)(\|.*)?$/;
function versCodes(app, pid, loc) {
  const out = { ...loc };
  for (const [k, v] of Object.entries(out)) {
    const r = typeof v === 'string' && SOURCE_ID.exec(v);
    const m = r && app.modele(r[2]);
    if (m && m.projet === pid && m.numero !== null && m.numero !== undefined) out[k] = `${r[1]}@${m.numero}${r[3] || ''}`;
  }
  return out;
}
async function depuisCodes(app, pid, loc, calculs) {
  for (const [k, v] of Object.entries(loc)) {
    const r = typeof v === 'string' && SOURCE_CODE.exec(v);
    if (!r) continue;
    const m = await modeleNumero(app, pid, Number(r[2]), calculs).catch(() => null);
    loc[k] = m ? `${r[1]}${m.id}${r[3] || ''}` : '';
  }
  return loc;
}

// The latest fit of the project's model number `numero`; when it has none, computed now, its
// views not opened (as a document does: documents.js); null when the project no longer has it.
const numeroId = (id) => Number(String(id).replace(/\D/g, '')) || 0;
function dernierAjustement(app, pid, numero) {
  const calcules = app.modelesDe(pid).filter((m) => m.numero === numero);
  return calcules.length ? calcules.sort((a, b) => numeroId(b.id) - numeroId(a.id))[0] : null;
}
async function modeleNumero(app, pid, numero, calculs) {
  const m = dernierAjustement(app, pid, numero);
  if (m) return m;
  const d = (app.projet(pid).modeles || []).find((x) => x.numero === numero);
  if (!d) return null;
  if (!d.ajuste) throw new Error(t('figure_modele_non_ajuste', { nom: d.nom }));
  if (!calculs.has(numero)) calculs.set(numero, app.calculerDiscret(pid, numero).catch((e) => { calculs.delete(numero); throw e; }));
  return app.modele(await calculs.get(numero));
}
async function modeleDuPanneau(app, pid, pn, calculs) {
  if (pn.numero !== null && pn.numero !== undefined) {
    const m = await modeleNumero(app, pid, pn.numero, calculs);
    if (!m) throw new Error(t('figure_modele_absent', { nom: pn.nom || `#${pn.numero}` }));
    return m;
  }
  if (pn.modele) {
    const m = app.modele(pn.modele);
    if (!m) throw new Error(t('figure_modele_absent', { nom: pn.nom || pn.modele }));
    return m;
  }
  return null;
}

// A panel's name in the figure's list: its model and view, as the explorer names them.
function libellePanneau(app, pid, pn) {
  const v = VUES[pn.vue];
  const m = pn.numero !== undefined && pn.numero !== null ? dernierAjustement(app, pid, pn.numero) : pn.modele ? app.modele(pn.modele) : null;
  try { if (v && (m || v.projet)) return v.titre(m, app.projet(pid) || {}, pn.loc || {}); } catch { /* a title that needs more */ }
  return `${pn.nom ? `${pn.nom} · ` : ''}${t(pn.vue)}`;
}

// A view drawn off screen with a panel's settings, as Plotly holds it (data, layout), with the
// text size it was drawn at and its plot's area (for a map's zoom). The view may ask the engine
// for more after drawing (a colour by a variable) and ask to be drawn again: what it asked is
// awaited, then it is drawn again, up to six times. Each plot it made is let finish before it is
// read and taken down (Plotly goes on drawing after it returns). In the light theme, without the
// selection.
async function dessinerHorsEcran(app, vue, params, loc) {
  const hote = el('div', { class: 'panneau', style: 'position:fixed;left:-30000px;top:0;width:900px;height:640px' });
  document.body.append(hote);
  let graphe = null;
  let encore = false;
  const enCours = new Set();
  const suivre = (p) => { enCours.add(p); const fin = () => enCours.delete(p); p.then(fin, fin); return p; };
  const dessines = new Map();  // each plot made -> drawn
  const surGraphe = (div) => {
    graphe = div;
    if (!dessines.has(div)) {
      dessines.set(div, new Promise((ok) => {
        try { div.once('plotly_afterplot', ok); } catch { ok(); }
        setTimeout(ok, 15000);
      }));
    }
  };
  const base = app.contexteHorsEcran(loc, surGraphe);
  const ctx = { ...base, selection: () => [], variablesChoisies: () => [], rafraichir: () => { encore = true; },
    demander: (...a) => suivre(base.demander(...a)), valeurs: (...a) => suivre(base.valeurs(...a)) };
  try {
    for (let tour = 0; tour < 6; tour += 1) {
      encore = false;
      await VUES[vue].rendre(hote, ctx, params, loc);
      while (enCours.size) {
        await Promise.allSettled([...enCours]);
        await new Promise((ok) => { setTimeout(ok, 0); });
      }
      if (!encore) break;
    }
    if (!graphe || !graphe.data) {
      const raison = hote.querySelector('.erreur, .infos');
      return { erreur: (raison && raison.textContent.trim()) || t('figure_pas_de_graphique') };
    }
    await dessines.get(graphe);
    if (graphe._etiquettes) await graphe._etiquettes;  // (its labels spread, drawn again: panneaux.js, tracer)
    const copie = (x) => { try { return structuredClone(x); } catch { return JSON.parse(JSON.stringify(x)); } };
    const zone = graphe._fullLayout && graphe._fullLayout._size;
    return { data: copie(graphe.data), layout: copie(graphe.layout), taille: FORMAT.taille, source: zone ? { largeur: zone.w, hauteur: zone.h } : null };
  } finally {
    hote.remove();
    for (const [div, fini] of dessines) fini.then(() => { try { window.Plotly.purge(div); } catch { /* already gone */ } });
  }
}

// A panel's plot as its view draws it now, kept while nothing it depends on changes (its model's
// fit, the project's data, its settings, Plots › Formatting, the language).
async function baseDuPanneau(app, pid, pn, caches) {
  const v = VUES[pn.vue];
  if (!v || pn.vue === 'figure') throw new Error(t('figure_vue_inconnue'));
  const m = await modeleDuPanneau(app, pid, pn, caches.calculs);
  if (!m && !v.projet && !v.global) throw new Error(t('figure_modele_absent', { nom: pn.nom || '?' }));
  const loc = await depuisCodes(app, pid, JSON.parse(JSON.stringify(pn.loc || {})), caches.calculs);
  const cle = JSON.stringify([pn.vue, m && m.id, pid, (app.projet(pid) || {}).revision, loc, FORMAT, langueActuelle()]);
  if (caches.bases.has(cle)) return caches.bases.get(cle);
  const r = await dessinerHorsEcran(app, pn.vue, { modele: m ? m.id : null, projet: pid }, loc);
  caches.bases.set(cle, r);
  while (caches.bases.size > 40) caches.bases.delete(caches.bases.keys().next().value);
  return r;
}

// ------------------------------------------------------------ a map's tiles

// The pixels per px of a map's background: a PNG's or a TIFF's, 300 dpi in a PDF or an SVG (a
// picture under their vectors), the screen's in the preview.
const densiteFond = (pour, dpi) => (pour === 'png' || pour === 'tiff' ? dpi / 96 : pour === 'apercu' ? Math.min(2, Math.max(1, window.devicePixelRatio || 1)) : 300 / 96);

// The plot area (px) of a plot drawn at its size: what its margins and colour bar leave.
async function zoneDuGraphique(fig) {
  const div = el('div', { style: `position:fixed;left:-30000px;top:0;width:${fig.layout.width}px;height:${fig.layout.height}px` });
  document.body.append(div);
  try {
    await window.Plotly.newPlot(div, structuredClone(fig.data), structuredClone(fig.layout), { staticPlot: true });
    return { largeur: div._fullLayout._size.w, hauteur: div._fullLayout._size.h };
  } finally {
    window.Plotly.purge(div);
    div.remove();
  }
}

// A map's tiles (composition.js: tuilesPour), fetched four at a time and drawn into a picture of
// cadre.largeur × cadre.hauteur pixels spanning cadre.x and cadre.y; a tile not fetched within a
// minute stays white. The picture as a data: URL (JPEG for photographs), and the tiles missing.
async function imageDesTuiles(fond, tuiles, cadre) {
  const toile = document.createElement('canvas');
  toile.width = cadre.largeur;
  toile.height = cadre.hauteur;
  const g = toile.getContext('2d');
  g.fillStyle = '#ffffff';
  g.fillRect(0, 0, toile.width, toile.height);
  g.imageSmoothingQuality = 'high';
  const file = [];
  for (let c = tuiles.colonnes[0]; c <= tuiles.colonnes[1]; c += 1) for (let l = tuiles.lignes[0]; l <= tuiles.lignes[1]; l += 1) file.push([c, l]);
  const arret = new AbortController();
  const minute = setTimeout(() => arret.abort(), 60000);
  let manquantes = 0;
  const suivantes = async () => {
    for (let k = file.shift(); k; k = file.shift()) {
      try {
        const r = await fetch(adresseTuile(fond, tuiles.zoom, ...k), { signal: arret.signal });
        if (!r.ok) throw new Error(String(r.status));
        const image = await createImageBitmap(await r.blob());
        const p = placeTuile(tuiles.zoom, ...k, cadre);
        g.drawImage(image, p.x, p.y, p.largeur, p.hauteur);
        image.close();
      } catch {
        manquantes += 1;
      }
    }
  };
  try { await Promise.all([suivantes(), suivantes(), suivantes(), suivantes()]); } finally { clearTimeout(minute); }
  const blob = await new Promise((ok) => { toile.toBlob(ok, fond.photo ? 'image/jpeg' : 'image/png', 0.92); });
  const url = await new Promise((ok, ko) => { const f = new FileReader(); f.onload = () => ok(f.result); f.onerror = ko; f.readAsDataURL(blob); });
  return { url, manquantes };
}

// A plane map (composition.js: carteEnPlan) adapted to its cell, put on its tiles: its plot area
// measured, its axes made to show the view's area there, its tiles drawn at `densite` pixels per
// px into a picture under its points. What `notes` should say: a background less sharp than
// asked (`voulu`, dpi; not in the preview), tiles missing.
async function surSesTuiles(fig, plan, densite, nom, voulu) {
  const zone = await zoneDuGraphique(fig);
  const { x, y } = cadrer(plan.vue, zone.largeur, zone.hauteur);
  fig.layout.xaxis.range = x;
  fig.layout.yaxis.range = y;
  const tuiles = tuilesPour({ x, y, largeur: zone.largeur, densite, taille: plan.fond.taille });
  const im = await imageDesTuiles(plan.fond, tuiles, { x, y, largeur: Math.round(zone.largeur * densite), hauteur: Math.round(zone.hauteur * densite) });
  if (im.manquantes === tuiles.nombre) throw new Error(t('figure_fond_absent'));
  fig.layout.images = [...(fig.layout.images || []), { source: im.url, xref: 'paper', yref: 'paper', x: 0, y: 1, sizex: 1, sizey: 1,
    xanchor: 'left', yanchor: 'top', sizing: 'stretch', layer: 'below' }];
  const notes = [];
  if (voulu && tuiles.densite < densite * 0.999) {
    notes.push(t(tuiles.limite === 'serveur' ? 'figure_fond_serveur' : 'figure_fond_limite', { lettre: nom, dpi: Math.round(96 * tuiles.densite), voulu, zoom: tuiles.zoom }));
  }
  if (im.manquantes) notes.push(t('figure_fond_manquantes', { lettre: nom, n: im.manquantes, total: tuiles.nombre }));
  return notes;
}

// ------------------------------------------------------------ the figure drawn

const avecDelai = (promesse, ms) => Promise.race([promesse,
  new Promise((_, ko) => { setTimeout(() => ko(new Error(t('figure_delai', { s: Math.round(ms / 1000) }))), ms); })]);

// Every panel drawn at its cell's size (composition.js), its letter, the panels that couldn't be
// drawn and why, what else to say (`notes`: a map's background). `pour`: 'apercu', 'pdf', 'svg',
// 'png' or 'tiff' (a picture's WebGL parts and maps' backgrounds at its resolution).
async function composer(app, pid, spec, caches, pour = 'svg', dpi = 300) {
  const d = disposer({ largeur: spec.largeur, hauteur: spec.hauteur, lignes: spec.lignes, colonnes: spec.colonnes, panneaux: spec.panneaux });
  const L = spec.lettres;
  const tailleLettre = L.taille * PX_PAR_PT;
  const police = `${FORMAT.police}, Arial, sans-serif`;
  const panneaux = [];
  const lettres = [];
  const erreurs = [];
  const notes = [];
  const image = pour === 'png' || pour === 'tiff';
  for (let i = 0; i < spec.panneaux.length; i += 1) {
    const pn = spec.panneaux[i];
    const texte = lettre(i, L.style);
    const nomLettre = texte || String(i + 1);
    const c = d.cellules[i];
    if (!c) { erreurs.push({ lettre: nomLettre, message: t('figure_hors_grille') }); continue; }
    const r = enPixels(c);
    const pl = texte ? { ...placeLettre(r, L.coin, tailleLettre, texte), coin: L.coin } : null;
    let base;
    try { base = await baseDuPanneau(app, pid, pn, caches); } catch (e) { base = { erreur: e.message }; }
    if (base.erreur) {
      erreurs.push({ lettre: nomLettre, message: base.erreur });
      panneaux.push({ ...r, svg: null, vide: `${nomLettre} · ${base.erreur}` });
    } else {
      // (a map on tiles: a plane plot over them; its view's area, not a zoom of its axes)
      const plan = carteEnPlan(base.data, base.layout, base.source);
      let fig = adapterFigure(plan ? plan.data : base.data, plan ? plan.layout : base.layout, { largeur: r.largeur, hauteur: r.hauteur,
        police: spec.police * PX_PAR_PT, base: base.taille, ligne: spec.ligne * PX_PAR_PT, titres: spec.titres, legende: spec.legendes, lettre: pl,
        zoom: plan ? null : pn.zoom, source: base.source });
      try {
        // (labels spread in its view: placed again in its cell, at the figure's text size: etiquettes.js)
        if (!plan) fig = await replacerEtiquettes(fig);
        if (plan) notes.push(...(await surSesTuiles(fig, plan, densiteFond(pour, dpi), nomLettre, pour === 'apercu' ? 0 : image ? dpi : 300)));
        // (a plot drawn with WebGL waits for frames the browser doesn't draw in a hidden window:
        // a panel that takes too long is left out, rather than the figure never coming)
        const svg = await avecDelai(window.Plotly.toImage({ data: fig.data, layout: fig.layout, config: { plotGlPixelRatio: image ? Math.min(8, Math.ceil(dpi / 96)) : 4 } },
          { format: 'svg', width: r.largeur, height: r.hauteur, imageDataOnly: true }), 45000);
        panneaux.push({ ...r, svg });
      } catch (e) {
        erreurs.push({ lettre: nomLettre, message: e.message });
        panneaux.push({ ...r, svg: null, vide: `${nomLettre} · ${e.message}` });
      }
    }
    if (pl) lettres.push({ texte, x: pl.x, y: pl.y, ancre: pl.ancre, taille: tailleLettre, gras: L.gras, police });
  }
  return { largeur: spec.largeur, hauteur: d.hauteur, panneaux, lettres, erreurs, notes };
}

// The SVG drawn on a white canvas of `pixels`, its RGBA pixels.
async function pixelsDe(svg, pixels) {
  const url = URL.createObjectURL(new Blob([svg], { type: 'image/svg+xml' }));
  try {
    const img = new Image();
    await new Promise((ok, ko) => { img.onload = ok; img.onerror = () => ko(new Error(t('figure_image_impossible'))); img.src = url; });
    const toile = document.createElement('canvas');
    toile.width = pixels.largeur;
    toile.height = pixels.hauteur;
    const c = toile.getContext('2d', { willReadFrequently: true });
    c.fillStyle = '#ffffff';
    c.fillRect(0, 0, pixels.largeur, pixels.hauteur);
    c.drawImage(img, 0, 0, pixels.largeur, pixels.hauteur);
    return c.getImageData(0, 0, pixels.largeur, pixels.hauteur).data;
  } finally {
    URL.revokeObjectURL(url);
  }
}

// The file of a figure composed (composer()): its bytes (SVG, PNG, TIFF), or the page a PDF is
// printed from, with its size in mm.
async function fichierDe(c, format, { dpi = 300, compression = 'lzw', titre = '' } = {}) {
  if (format === 'pdf') return { html: pagePDF(composerSVG(c), c.largeur, c.hauteur, titre, langueActuelle()), largeur: c.largeur, hauteur: c.hauteur };
  if (format === 'svg') return { octets: new TextEncoder().encode(`<?xml version="1.0" encoding="UTF-8"?>\n${composerSVG(c)}\n`), type: 'image/svg+xml' };
  const pixels = pixelsImage(c.largeur, c.hauteur, dpi);
  const rgba = await pixelsDe(composerSVG({ ...c, pixels }), pixels);
  const octets = format === 'png' ? await ecrirePNG({ ...pixels, rgba, dpi }) : ecrireTIFF({ ...pixels, rgba, dpi, compression });
  return { octets, type: format === 'png' ? 'image/png' : 'image/tiff', pixels };
}

// A figure of the project as a file, without its panel (the explorer's Export, the checks):
// { octets | html, largeur, hauteur, erreurs, notes }.
export async function fichierFigure(pid, id, format = null, options = {}) {
  const f = await demander('figure', { projet: pid, id });
  const spec = completer(lireContenu(f.contenu));
  const fmt = format || spec.format;
  const c = await composer(appli, pid, spec, { bases: new Map(), calculs: new Map() }, fmt, options.dpi || spec.dpi);
  return { ...(await fichierDe(c, fmt, { dpi: options.dpi || spec.dpi, compression: options.compression || spec.compression, titre: f.titre })),
    largeur: c.largeur, hauteur: c.hauteur, erreurs: c.erreurs, notes: c.notes };
}

const nomFichier = (titre) => (titre || t('figure_nom')).replace(/[^\p{L}\p{N} _.()-]+/gu, '_').trim() || 'figure';

// Export: the figure drawn, the panels that couldn't be asked about, then where to write it (the
// desktop app) and written; in a browser, downloaded, or the PDF printed from its print window.
async function exporter(app, pid, spec, titre, caches) {
  const format = spec.format;
  if (!spec.panneaux.length) { app.statut(t('figure_aucun_panneau'), true); return; }
  let c;
  try { c = await app.occupe(() => composer(app, pid, spec, caches, format, spec.dpi)); } catch { return; }
  if (c.erreurs.length && !window.confirm(t('figure_exporter_quand_meme', { n: c.erreurs.length, liste: c.erreurs.map((e) => `${e.lettre} (${e.message})`).join(', ') }))) return;
  const nom = `${nomFichier(titre)}.${EXTENSIONS[format]}`;
  let chemin = null;
  if (app.bureau) {
    chemin = await app.bureau.enregistrerSous(nom, [{ name: format.toUpperCase(), extensions: format === 'tiff' ? ['tif', 'tiff'] : [EXTENSIONS[format]] }]);
    if (!chemin) return;
  }
  try {
    await app.occupe(async () => {
      const r = await fichierDe(c, format, { dpi: spec.dpi, compression: spec.compression, titre });
      if (r.html) {
        if (app.bureau) await app.bureau.imprimerPDF(r.html, chemin, { largeur: r.largeur, hauteur: r.hauteur });
        else imprimer(r.html);
      } else if (app.bureau) await app.bureau.ecrireFichier(chemin, r.octets);
      else telecharger(r.octets, nom, r.type);
    });
  } catch { return; }
  // (a map's background less sharp than asked, tiles missing: said after the file is written)
  const notes = c.notes.map((n) => ` · ${n}`).join('');
  if (chemin) {
    app.statut(`${t('figure_ecrite')} ${chemin}${notes}`);
    app.bureau.ouvrirChemin(chemin);
  } else if (notes) app.statut(notes.slice(3));
}

function telecharger(octets, nom, type) {
  const a = el('a', { href: URL.createObjectURL(new Blob([octets], { type })), download: nom });
  a.click();
  setTimeout(() => URL.revokeObjectURL(a.href), 4000);
}
function imprimer(html) {
  const f = window.open('', '_blank');
  f.document.write(html);
  f.document.close();
  f.onload = () => f.print();
}

// ------------------------------------------------------------ the panel

function lireContenu(texte) {
  try { return completer(JSON.parse(texte || '{}')); } catch { return completer({}); }
}

async function rendreFigure(app, conteneur, params, loc) {
  const pid = params.projet;
  const p = app.projet(pid);
  if (!p) return;
  const figures = [].concat(p.figures || []);
  const existe = (id) => figures.some((f) => f.id === id);
  let s = conteneur.etatFigure;
  if (s && !existe(s.id) && !s.modifie) {  // (deleted, in another window)
    s.fermer();
    conteneur.etatFigure = s = null;
  }
  if (!loc.figure && figures[0]) loc.figure = figures[0].id;
  const id = loc.figure;
  if (!id || !existe(id)) {
    conteneur.replaceChildren(el('div', { class: 'vide-document' }, el('p', { class: 'note' }, t(id ? 'figure_supprimee' : 'figure_aucune')),
      el('button', { class: 'principal', onclick: () => app.composerFigure(pid) }, icone('layout-grid'), ` ${t('figure_composer')}`)));
    return;
  }
  if (s && s.id === id) { s.rafraichir(); return; }
  if (conteneur.chargementFigure === id) return;
  conteneur.chargementFigure = id;
  try {
    if (s) { s.enregistrer(); s.fermer(); conteneur.etatFigure = null; }
    conteneur.replaceChildren(el('p', { class: 'note' }, t('figure_chargement')));
    const f = await demander('figure', { projet: pid, id });
    conteneur.etatFigure = creerComposeur(app, conteneur, pid, f);
    app.titrePanneau(cleFigure(pid, id), f.titre || t('figure_sans_titre'));
  } catch (e) {
    conteneur.replaceChildren(el('p', { class: 'erreur' }, `${t('erreur')}${deuxPoints()}${e.message}`));
  } finally {
    conteneur.chargementFigure = null;
  }
}

// A figure's panel: its settings on the left, its preview on the right, drawn again a moment
// after each change; each change kept in the project a moment later (or when the panel closes,
// or before the project is saved).
function creerComposeur(app, conteneur, pid, f) {
  const s = {
    pid, id: f.id, titre: f.titre || '', spec: lireContenu(f.contenu), envoye: f.contenu, titreEnvoye: f.titre || '',
    modifie: false, ferme: false, attente: null, attenteApercu: null, generation: 0, signature: null, url: null, langueUI: null,
    caches: { bases: new Map(), calculs: new Map() },
  };
  ouvertes.add(s);
  const reglages = el('div', { class: 'reglages-figure' });
  const image = el('img', { alt: '' });
  const feuille = el('div', { class: 'feuille-figure' }, image);
  const resume = el('p', { class: 'note resume-figure' });
  const erreurs = el('div', { class: 'erreurs-figure' });
  const apercu = el('div', { class: 'apercu-figure' }, feuille, resume, erreurs);
  conteneur.replaceChildren(el('div', { class: 'cadre-figure' }, el('div', { class: 'figure-compo' }, reglages, apercu)));
  const zones = {};

  const changer = () => {
    s.modifie = true;
    s.programmer();
    s.apercu();
    majPanneaux();
    majHauteur();
  };
  // a panel that no longer fits: the grid grows (up to GRILLE_MAX)
  const agrandir = () => {
    const sp = s.spec;
    for (let n = 0; n < 2 * GRILLE_MAX; n += 1) {
      const d = disposer({ largeur: sp.largeur, hauteur: sp.hauteur, lignes: sp.lignes, colonnes: sp.colonnes, panneaux: sp.panneaux });
      if (d.cellules.every(Boolean)) return;
      const cases = sp.panneaux.reduce((k, p) => k + p.lignes * p.colonnes, 0);
      const g = grilleProposee(cases, sp.largeur);
      if (g.lignes * g.colonnes > sp.lignes * sp.colonnes && g.colonnes >= sp.colonnes && g.lignes >= sp.lignes) { sp.lignes = g.lignes; sp.colonnes = g.colonnes; }
      else if (sp.colonnes < 3 && sp.colonnes <= sp.lignes && sp.colonnes < GRILLE_MAX) sp.colonnes += 1;
      else if (sp.lignes < GRILLE_MAX) sp.lignes += 1;
      else return;
    }
  };
  const ajouterPanneau = (pn) => {
    s.spec.panneaux.push(pn);
    agrandir();
    construire();
    changer();
  };
  s.ajouter = (entree) => ajouterPanneau(panneauDeVue(app, entree));
  s.ajouterVue = (m, vue) => ajouterPanneau(panneauSansVue(m, vue));

  // ---- the settings
  const nombre = (valeur, min, max, pas, f, titre = '') => el('input', { type: 'number', min, max, step: pas, value: valeur, title: titre,
    onchange: (e) => { const v = Number(e.target.value); if (Number.isFinite(v) && v >= min && v <= max) f(v); else e.target.value = valeur; } });
  const coche = (valeur, libelle, f, titre = '') => el('label', { class: 'champ', title: titre },
    el('input', { type: 'checkbox', checked: valeur, onchange: (e) => f(e.target.checked) }), libelle);
  const liste = (options, valeur, f, titre = '') => el('select', { title: titre, onchange: (e) => f(e.target.value) },
    options.map(([v, n]) => el('option', { value: v, selected: String(v) === String(valeur) }, n)));
  const radio = (nom, options, valeur, f) => el('div', { class: 'ligne choix-figure' }, options.map(([v, n]) => el('label', { class: 'champ' },
    el('input', { type: 'radio', name: `${nom}-${s.pid}-${s.id}`, checked: String(v) === String(valeur), onchange: () => f(v) }), n)));
  const titreDeLargeur = (x) => t(x.colonnes === 1 ? 'figure_colonne_1' : x.colonnes === 2 ? 'figure_colonne_2' : 'figure_colonne_15', { mm: x.mm });

  function majHauteur() {
    if (!zones.hauteurAuto) return;
    const d = disposer({ ...s.spec, hauteur: 0 });
    zones.hauteurAuto.textContent = t('figure_hauteur_auto', { mm: d.hauteur });
  }

  // The plots that can go in, by model ("M1 · PCA"; "orchards · M1 · PCA" with several projects open):
  // every plot view of the project's fitted models (the latest fit of each), those a report or a
  // document draws (rapport.js: partiesModele), drawn off screen with their usual settings, an open
  // one with its own; then the project's plots open in this window (the map, series...).
  function majAjout() {
    const ouvertesIci = app.vuesGraphiques().filter((e) => sonProjet(app, e) === s.pid);
    const ouverte = (m, vue) => ouvertesIci.find((e) => e.params.modele === m.id && e.vue === vue);
    const option = (valeur, libelle) => el('option', { value: valeur }, libelle);
    const groupes = modelesAjustes(app, s.pid).map((m) => {
      const vues = [...new Set(partiesModele(m).filter((x) => x.vue).map((x) => x.vue))]
        .concat(vuesDe(m).filter((v) => PLUS_QUE_LE_RAPPORT.includes(v)), ouvertesIci.filter((e) => e.params.modele === m.id).map((e) => e.vue))
        .filter((v, i, l) => VUES[v] && l.indexOf(v) === i);
      return vues.length ? el('optgroup', { label: `${app.plusieursProjets() ? `${app.nomProjetCourt(s.pid)} · ` : ''}${m.nom} · ${m.type}` },
        vues.map((v) => (ouverte(m, v) ? option(`o:${ouverte(m, v).id}`, `${t(v)} (${t('figure_ouverte')})`) : option(`m:${m.id}:${v}`, t(v))))) : null;
    }).filter(Boolean);
    const duProjet = ouvertesIci.filter((e) => !e.params.modele);
    if (duProjet.length) {
      groupes.push(el('optgroup', { label: (app.projet(s.pid) || {}).nom || '' }, duProjet.map((e) => {
        let titre = e.vue;
        try { titre = VUES[e.vue].titre(null, app.projet(s.pid) || {}, e.loc || {}); } catch { /* its key */ }
        return option(`o:${e.id}`, `${titre} (${t('figure_ouverte')})`);
      })));
    }
    zones.ajout.replaceChildren(option('', t(groupes.length ? 'figure_vues_ouvertes' : 'figure_aucune_vue_courte')), ...groupes);
    zones.ajout.disabled = !groupes.length;
    zones.noteAjout.style.display = groupes.length ? 'none' : '';
  }

  function majPanneaux() {
    if (!zones.panneaux) return;
    const sp = s.spec;
    const d = disposer(sp);
    const ouvertesIci = app.vuesGraphiques();
    const n = sp.panneaux.length;
    zones.panneaux.replaceChildren(...(n ? sp.panneaux.map((pn, i) => {
      const vue = ouvertesIci.find((e) => e.vue === pn.vue && sonProjet(app, e) === s.pid
        && (pn.numero !== undefined && pn.numero !== null ? (app.modele(e.params.modele) || {}).numero === pn.numero : pn.modele ? e.params.modele === pn.modele : !e.params.modele));
      const deplacer = (k) => {
        const [x] = sp.panneaux.splice(i, 1);
        sp.panneaux.splice(i + k, 0, x);
        changer();
      };
      return el('div', { class: `ligne-panneau${d.cellules[i] ? '' : ' hors-grille'}`, title: d.cellules[i] ? '' : t('figure_hors_grille') },
        el('b', { class: 'lettre-panneau' }, lettre(i, sp.lettres.style) || String(i + 1)),
        el('span', { class: 'nom-panneau', title: libellePanneau(app, s.pid, pn) }, libellePanneau(app, s.pid, pn)),
        liste([['1x1', t('figure_une_case')], ['1x2', t('figure_deux_colonnes')], ['2x1', t('figure_deux_lignes')]], `${pn.lignes}x${pn.colonnes}`, (v) => {
          [pn.lignes, pn.colonnes] = v.split('x').map(Number);
          agrandir();
          construireTaille();
          changer();
        }, t('figure_etendue_aide')),
        el('button', { class: 'icone-seule', title: t('figure_monter'), disabled: i === 0, onclick: () => deplacer(-1) }, icone('arrow-up')),
        el('button', { class: 'icone-seule', title: t('figure_descendre'), disabled: i === n - 1, onclick: () => deplacer(1) }, icone('arrow-down')),
        vue ? el('button', { class: 'icone-seule', title: t('figure_reprendre'), onclick: () => {
          const nouveau = panneauDeVue(app, vue);
          sp.panneaux[i] = { ...nouveau, lignes: pn.lignes, colonnes: pn.colonnes };
          changer();
        } }, icone('refresh-cw')) : null,
        el('button', { class: 'icone-seule', title: t('figure_retirer'), onclick: () => { sp.panneaux.splice(i, 1); changer(); } }, icone('x')));
    }) : [el('p', { class: 'note' }, t('figure_aucun_panneau'))]));
  }

  function construireTaille() {
    const sp = s.spec;
    const prereglee = LARGEURS.some((x) => x.mm === sp.largeur);
    zones.taille.replaceChildren(
      el('label', {}, t('figure_largeur')),
      el('div', { class: 'ligne' },
        liste(LARGEURS.map((x) => [x.mm, titreDeLargeur(x)]).concat([['autre', t('figure_autre_largeur')]]), prereglee ? sp.largeur : 'autre', (v) => {
          if (v !== 'autre') { sp.largeur = Number(v); construireTaille(); changer(); } else { zones.largeurLibre.style.display = ''; zones.largeurLibre.focus(); }
        }),
        zones.largeurLibre = nombre(sp.largeur, 20, 400, 1, (v) => { sp.largeur = v; construireTaille(); changer(); }), ' mm'),
      el('label', {}, t('figure_hauteur')),
      el('div', { class: 'ligne' },
        coche(!sp.hauteur, zones.hauteurAuto = el('span', {}), (oui) => {
          sp.hauteur = oui ? 0 : disposer({ ...sp, hauteur: 0 }).hauteur;
          construireTaille();
          changer();
        }, t('figure_hauteur_aide')),
        sp.hauteur ? [nombre(sp.hauteur, 10, 400, 1, (v) => { sp.hauteur = v; changer(); }), ' mm'] : null),
      el('label', {}, t('figure_grille')),
      el('div', { class: 'ligne' },
        nombre(sp.lignes, 1, GRILLE_MAX, 1, (v) => { sp.lignes = v; changer(); }, t('figure_lignes')), '×',
        nombre(sp.colonnes, 1, GRILLE_MAX, 1, (v) => { sp.colonnes = v; changer(); }, t('figure_colonnes')),
        liste([['', '…']].concat(['1x2', '2x1', '2x2', '1x3', '3x1', '2x3', '3x2', '3x3'].map((g) => [g, g.replace('x', ' × ')])), '', (v) => {
          if (!v) return;
          [sp.lignes, sp.colonnes] = v.split('x').map(Number);
          construireTaille();
          changer();
        }, t('figure_grilles'))));
    if (prereglee) zones.largeurLibre.style.display = 'none';
    majHauteur();
  }

  function construire() {
    const sp = s.spec;
    s.langueUI = langueActuelle();
    zones.titre = el('input', { class: 'titre-figure', value: s.titre, placeholder: t('figure_sans_titre'), oninput: (e) => {
      s.titre = e.target.value;
      app.titrePanneau(cleFigure(s.pid, s.id), s.titre || t('figure_sans_titre'));
      s.modifie = true;
      s.programmer();
    } });
    zones.panneaux = el('div', { class: 'panneaux-figure' });
    zones.ajout = el('select', { class: 'ajout-figure', 'aria-label': t('figure_vues_ouvertes'), onmousedown: majAjout, onfocus: majAjout, onchange: (e) => {
      const v = e.target.value;
      e.target.value = '';
      // (an open view: with its settings; a model's view not open: drawn with its usual ones)
      const entree = v.startsWith('o:') && app.vuesGraphiques().find((x) => x.id === v.slice(2));
      const [, id, vue] = /^m:([^:]+):(.+)$/.exec(v) || [];
      if (entree) s.ajouter(entree);
      else if (id && app.modele(id)) s.ajouterVue(app.modele(id), vue);
    } });
    zones.noteAjout = el('p', { class: 'note' }, t('figure_aucune_vue'));
    zones.taille = el('div', { class: 'grille-champs' });
    const L = sp.lettres;
    const raster = () => ['png', 'tiff'].includes(sp.format);
    zones.raster = el('div', { class: 'grille-champs', style: { display: raster() ? '' : 'none' } },
      el('label', {}, t('figure_resolution')), radio('dpi', [[300, t('figure_dpi', { n: 300 })], [600, t('figure_dpi', { n: 600 })]], sp.dpi, (v) => { sp.dpi = Number(v); changer(); }),
      zones.compression = el('label', {}, t('figure_compression')),
      zones.choixCompression = liste([['lzw', 'LZW'], ['aucune', t('figure_sans_compression')]], sp.compression, (v) => { sp.compression = v; changer(); }, t('figure_compression_aide')));
    const majFormat = () => {
      zones.raster.style.display = raster() ? '' : 'none';
      zones.compression.style.display = sp.format === 'tiff' ? '' : 'none';
      zones.choixCompression.style.display = sp.format === 'tiff' ? '' : 'none';
      zones.notePDF.style.display = sp.format === 'pdf' && !app.bureau ? '' : 'none';
      zones.pixels.textContent = raster() ? t('figure_pixels', { ...pixelsImage(sp.largeur, disposer(sp).hauteur, sp.dpi), dpi: sp.dpi }) : '';
    };
    s.majFormat = majFormat;
    zones.notePDF = el('p', { class: 'note' }, t('figure_navigateur_pdf'));
    zones.pixels = el('p', { class: 'note' });
    reglages.replaceChildren(
      el('div', { class: 'ligne' }, icone('layout-grid'), zones.titre),
      el('h3', {}, t('figure_panneaux')), zones.panneaux,
      el('div', { class: 'ligne' }, zones.ajout), zones.noteAjout,
      el('h3', {}, t('figure_taille')), zones.taille,
      el('h3', {}, t('figure_texte')),
      el('div', { class: 'grille-champs' },
        el('label', { title: t('figure_police_aide') }, t('figure_police')), el('div', { class: 'ligne' }, nombre(sp.police, 4, 24, 0.5, (v) => { sp.police = v; changer(); }, t('figure_police_aide')), ' pt'),
        el('label', { title: t('figure_ligne_aide') }, t('figure_ligne')), el('div', { class: 'ligne' }, nombre(sp.ligne, 0.1, 4, 0.05, (v) => { sp.ligne = v; changer(); }, t('figure_ligne_aide')), ' pt'),
        el('label', {}, t('figure_lettres')),
        el('div', { class: 'ligne lettres-figure' },
          liste(STYLES_LETTRES.map((x) => [x, x ? ['A', 'B', 'C'].map((_, i) => lettre(i, x)).join(', ') : t('figure_lettres_aucune')]), L.style, (v) => { L.style = v; changer(); }),
          coche(L.gras, t('figure_gras'), (v) => { L.gras = v; changer(); })),
        el('label', {}, ''),
        el('div', { class: 'ligne' },
          liste(COINS.map((x) => [x, t(`figure_coin_${x}`)]), L.coin, (v) => { L.coin = v; changer(); }),
          nombre(L.taille, 4, 36, 0.5, (v) => { L.taille = v; changer(); }, t('figure_taille_lettres')), ' pt'),
        el('label', {}, ''),
        el('div', { class: 'ligne' }, coche(sp.titres, t('figure_titres'), (v) => { sp.titres = v; changer(); }, t('figure_titres_aide')),
          coche(sp.legendes, t('figure_legendes'), (v) => { sp.legendes = v; changer(); }))),
      el('h3', {}, t('figure_export')),
      el('div', { class: 'grille-champs' },
        el('label', {}, t('figure_format')),
        radio('format', [['pdf', t('figure_pdf')], ['svg', t('figure_svg')], ['png', 'PNG'], ['tiff', 'TIFF']], sp.format, (v) => { sp.format = v; majFormat(); s.modifie = true; s.programmer(); })),
      zones.raster, zones.pixels, zones.notePDF,
      el('div', { class: 'ligne' }, el('button', { class: 'principal', onclick: () => s.exporter() }, icone('image-down'), ` ${t('figure_exporter')}`)),
      el('p', { class: 'note' }, t('figure_note')));
    construireTaille();
    majPanneaux();
    majAjout();
    majFormat();
  }

  // ---- the preview, drawn again a moment after the last change
  const signature = () => JSON.stringify([s.spec, s.spec.panneaux.map((pn) => (pn.numero !== undefined && pn.numero !== null ? (dernierAjustement(app, s.pid, pn.numero) || {}).id : pn.modele)),
    (app.projet(s.pid) || {}).revision, FORMAT, langueActuelle()]);
  const dessiner = async () => {
    const generation = ++s.generation;
    s.signature = signature();
    feuille.classList.add('attente');
    let c;
    try {
      c = await composer(app, s.pid, s.spec, s.caches, 'apercu');
    } catch (e) {
      if (generation === s.generation) erreurs.replaceChildren(el('p', { class: 'erreur' }, `${t('erreur')}${deuxPoints()}${e.message}`));
      return;
    } finally {
      if (generation === s.generation) feuille.classList.remove('attente');
    }
    if (generation !== s.generation || s.ferme) return;
    const url = URL.createObjectURL(new Blob([composerSVG(c)], { type: 'image/svg+xml' }));
    image.src = url;
    // (for screen readers: the figure's title, then each panel by its letter)
    image.alt = [s.titre || t('figure_sans_titre'), ...[...reglages.querySelectorAll('.ligne-panneau')]
      .map((l) => [l.querySelector('.lettre-panneau'), l.querySelector('.nom-panneau')].map((x) => (x ? x.textContent.trim() : '')).filter(Boolean).join(' '))].join(' — ');
    if (s.url) URL.revokeObjectURL(s.url);
    s.url = url;
    image.style.width = `${(c.largeur * 96) / 25.4}px`;
    resume.textContent = t('figure_resume', { largeur: decimal(c.largeur), hauteur: decimal(c.hauteur), n: s.spec.panneaux.length, police: decimal(s.spec.police) });
    erreurs.replaceChildren(...c.erreurs.map((e) => el('p', { class: 'erreur' }, `${e.lettre}${deuxPoints()}${e.message}`)), ...c.notes.map((n) => el('p', { class: 'note' }, n)));
    if (s.majFormat) s.majFormat();
  };
  s.apercu = () => {
    clearTimeout(s.attenteApercu);
    s.attenteApercu = setTimeout(() => { if (!s.ferme) dessiner(); }, 250);
  };

  // ---- kept in the project
  s.enregistrer = async () => {
    clearTimeout(s.attente);
    s.attente = null;
    if (!s.modifie) return;
    s.modifie = false;
    const contenu = JSON.stringify(s.spec);
    const q = { projet: s.pid, id: s.id };
    if (contenu !== s.envoye) q.contenu = contenu;
    if (s.titre !== s.titreEnvoye) q.titre = s.titre;
    if (!q.contenu && q.titre === undefined) return;
    try {
      const r = await demander('figure_enregistrer', q, { silencieux: true, garder: true });
      if (q.contenu) s.envoye = contenu;
      if (q.titre !== undefined) { s.titreEnvoye = q.titre; app.diffuser('modeles'); }
      app.majProjet(r.projet);
      canal.postMessage({ type: 'figure', projet: s.pid, id: s.id });
    } catch {
      s.modifie = true;  // (tried again at the next change, or when the panel closes)
    }
  };
  s.programmer = () => {
    clearTimeout(s.attente);
    s.attente = setTimeout(() => s.enregistrer(), 600);
  };
  s.exporter = async () => {
    await s.enregistrer();
    await exporter(app, s.pid, s.spec, s.titre, s.caches);
  };
  s.nommer = (titre) => {
    s.titre = titre;
    if (zones.titre) zones.titre.value = titre;
    app.titrePanneau(cleFigure(s.pid, s.id), titre || t('figure_sans_titre'));
    s.modifie = true;
    s.enregistrer();
  };
  // called again by its panel (the language, the models, the data changed): what changed shown
  s.rafraichir = () => {
    if (s.langueUI !== langueActuelle()) construire(); else { majPanneaux(); majAjout(); }
    if (s.signature !== signature()) s.apercu();
  };
  s.recharger = async () => {
    try {
      const x = await demander('figure', { projet: s.pid, id: s.id }, { silencieux: true });
      if (s.modifie || s.ferme) return;
      s.spec = lireContenu(x.contenu);
      s.envoye = x.contenu;
      s.titre = s.titreEnvoye = x.titre || '';
      construire();
      s.apercu();
    } catch { /* deleted meanwhile: its panel says so when drawn again */ }
  };
  s.fermer = () => {
    if (s.ferme) return;
    s.enregistrer();
    s.ferme = true;
    clearTimeout(s.attenteApercu);
    if (s.url) URL.revokeObjectURL(s.url);
    ouvertes.delete(s);
  };
  construire();
  dessiner();
  return s;
}

// ------------------------------------------------------------ new, add, rename, duplicate, delete

// A new figure of the project, holding the views given (open plot views), its panel opened.
async function nouvelleFigure(app, pid, entrees = []) {
  const p = app.projet(pid);
  const titres = new Set([].concat(p.figures || []).map((f) => f.titre));
  let n = [].concat(p.figures || []).length + 1;
  while (titres.has(`${t('figure_nom')} ${n}`)) n += 1;
  const spec = completer({});
  spec.panneaux = entrees.filter((e) => e.graphe).map((e) => panneauDeVue(app, e));
  spec.largeur = spec.panneaux.length === 1 ? 85 : 174;
  if (spec.panneaux.length) Object.assign(spec, grilleProposee(spec.panneaux.length, spec.largeur));
  const r = await demander('figure_enregistrer', { projet: pid, titre: `${t('figure_nom')} ${n}`, contenu: JSON.stringify(spec) });
  app.majProjet(r.projet);
  app.diffuser('modeles');  // (the other windows' explorers)
  app.ouvrirFigure(pid, r.id);
  return r.id;
}

// A plot added to a figure: in its panel if open in this window, otherwise to what the project
// keeps of it; its panel then comes to the front.
async function ajouterA(app, pid, id, entree) {
  const s = ouverte(pid, id);
  if (s) s.ajouter(entree);
  else {
    const f = await demander('figure', { projet: pid, id });
    const spec = lireContenu(f.contenu);
    spec.panneaux.push(panneauDeVue(app, entree));
    for (let k = 0; k < 2 * GRILLE_MAX && !disposer(spec).cellules.every(Boolean); k += 1) {
      const g = grilleProposee(spec.panneaux.reduce((x, p) => x + p.lignes * p.colonnes, 0), spec.largeur);
      spec.lignes = Math.max(spec.lignes, g.lignes);
      spec.colonnes = Math.max(spec.colonnes, g.colonnes);
      if (!disposer(spec).cellules.every(Boolean)) spec.lignes = Math.min(GRILLE_MAX, spec.lignes + 1);
    }
    const r = await demander('figure_enregistrer', { projet: pid, id, contenu: JSON.stringify(spec) });
    app.majProjet(r.projet);
    canal.postMessage({ type: 'figure', projet: pid, id });
  }
  app.ouvrirFigure(pid, id);
  const p = app.projet(pid);
  const titre = ([].concat(p.figures || []).find((x) => x.id === id) || {}).titre || t('figure_sans_titre');
  app.statut(t('figure_ajoutee', { figure: titre }));
}

function menuFigure(app, pid, id) {
  const erreur = (e) => app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true);
  return [
    { libelle: t('figure_ouvrir'), icone: 'layout-grid', action: () => app.ouvrirFigure(pid, id) },
    { libelle: t('figure_exporter'), icone: 'image-down', action: async () => {
      const s = ouverte(pid, id);
      if (s) { s.exporter(); return; }
      try {
        const f = await demander('figure', { projet: pid, id });
        await exporter(app, pid, lireContenu(f.contenu), f.titre, { bases: new Map(), calculs: new Map() });
      } catch (e) { erreur(e); }
    } },
    { libelle: t('figure_renommer'), icone: 'pencil', action: () => renommer(app, pid, id) },
    { libelle: t('figure_dupliquer'), icone: 'copy', action: () => dupliquer(app, pid, id).catch(erreur) },
    '-',
    { libelle: t('figure_supprimer'), icone: 'trash-2', action: () => supprimer(app, pid, id).catch(erreur) },
  ];
}

function renommer(app, pid, id) {
  const f = [].concat(app.projet(pid).figures || []).find((x) => x.id === id);
  if (!f) return;
  let titre = f.titre || '';
  dialogue({ titre: t('figure_renommer').replace(/…$/, ''), contenu: el('input', { value: titre, style: 'width:26em', oninput: (e) => { titre = e.target.value; } }), boutons: [
    { libelle: t('annuler'), action: () => true },
    { libelle: t('ok'), principal: true, action: () => {
      const s = ouverte(pid, id);
      if (s) s.nommer(titre);
      else demander('figure_enregistrer', { projet: pid, id, titre }).then((r) => { app.majProjet(r.projet); app.diffuser('modeles'); });
      return true;
    } }] });
}

async function dupliquer(app, pid, id) {
  const s = ouverte(pid, id);
  if (s) await s.enregistrer();
  const f = await demander('figure', { projet: pid, id });
  const r = await demander('figure_enregistrer', { projet: pid, titre: `${f.titre || t('figure_sans_titre')} (${t('figure_copie')})`, contenu: f.contenu });
  app.majProjet(r.projet);
  app.diffuser('modeles');
  app.ouvrirFigure(pid, r.id);
}

async function supprimer(app, pid, id) {
  const f = [].concat(app.projet(pid).figures || []).find((x) => x.id === id);
  if (!f || !window.confirm(t('figure_confirmer_supprimer', { titre: f.titre || t('figure_sans_titre') }))) return;
  const s = ouverte(pid, id);
  if (s) s.modifie = false;  // (what it holds is going: nothing to keep when its panel closes)
  app.fermerVue(cleFigure(pid, id));
  const r = await demander('figure_supprimer', { projet: pid, id });
  app.majProjet(r.projet);
  app.diffuser('modeles');
}

// ------------------------------------------------------------ strings

ajouterTextes({
  figure_composer: 'Composer une figure…', figure_ajouter: 'Ajouter à une figure', figure_nouvelle: 'Nouvelle figure', figure_nom: 'Figure',
  figure_figures: 'Figures', figure_sans_titre: 'Figure sans titre', figure_chargement: 'Ouverture de la figure…',
  figure_aucune: 'Ce projet n’a pas encore de figure.', figure_supprimee: 'Cette figure a été supprimée.',
  figure_panneaux: 'Panneaux', figure_aucun_panneau: 'Aucun panneau encore.', figure_vues_ouvertes: 'Ajouter un graphique…', figure_ouverte: 'ouverte',
  figure_aucune_vue_courte: 'Aucun graphique à ajouter',
  figure_aucune_vue: 'Ajustez un modèle du projet, ou ouvrez un de ses graphiques (menu Graphiques), puis choisissez-le ici ; ou clic droit sur un graphique › Ajouter à une figure.',
  figure_une_case: '1 case', figure_deux_colonnes: '2 colonnes', figure_deux_lignes: '2 lignes', figure_etendue_aide: 'La place du panneau dans la grille',
  figure_monter: 'Plus tôt dans l’ordre', figure_descendre: 'Plus tard dans l’ordre',
  figure_reprendre: 'Reprendre les réglages de sa vue ouverte (axes, couleurs, étiquettes, zoom)', figure_retirer: 'Retirer de la figure',
  figure_hors_grille: 'pas de place dans la grille : agrandissez-la, ou changez l’ordre des panneaux', figure_taille: 'Taille', figure_largeur: 'Largeur',
  figure_colonne_1: '{mm} mm — une colonne', figure_colonne_15: '{mm} mm — une colonne et demie', figure_colonne_2: '{mm} mm — deux colonnes',
  figure_autre_largeur: 'Autre largeur', figure_hauteur: 'Hauteur', figure_hauteur_auto: 'd’après les panneaux ({mm} mm)',
  figure_hauteur_aide: 'Les lignes de la grille aussi hautes que le demandent les graphiques tels qu’ils sont à l’écran ; décochez pour donner la hauteur',
  figure_grille: 'Grille', figure_lignes: 'Lignes', figure_colonnes: 'Colonnes', figure_grilles: 'Grilles courantes (lignes × colonnes)',
  figure_texte: 'Texte et traits', figure_police: 'Texte', figure_police_aide: 'La taille des graduations, des titres des axes et des légendes une fois imprimée (de 6 à 8 pt pour la plupart des revues)',
  figure_ligne: 'Traits', figure_ligne_aide: 'La largeur d’un trait de courbe une fois imprimé ; les axes, la grille et les formes à la moitié',
  figure_lettres: 'Lettres', figure_lettres_aucune: 'aucune', figure_gras: 'gras', figure_taille_lettres: 'Taille des lettres',
  figure_coin_hg: 'en haut à gauche', figure_coin_hd: 'en haut à droite', figure_coin_bg: 'en bas à gauche', figure_coin_bd: 'en bas à droite',
  figure_titres: 'Titres des graphiques', figure_titres_aide: 'Le titre de chaque graphique au-dessus de lui (la lettre et la légende de la figure suffisent souvent)',
  figure_legendes: 'Légendes', figure_export: 'Export', figure_format: 'Format', figure_pdf: 'PDF (vectoriel)', figure_svg: 'SVG (vectoriel)',
  figure_resolution: 'Résolution', figure_dpi: '{n} points par pouce', figure_compression: 'Compression',
  figure_compression_aide: 'LZW : sans perte, celle que les revues demandent ; aucune : un fichier bien plus gros', figure_sans_compression: 'aucune',
  figure_pixels: '{largeur} × {hauteur} pixels à {dpi} points par pouce.', figure_exporter: 'Exporter…',
  figure_navigateur_pdf: 'Dans le navigateur, le PDF passe par sa fenêtre d’impression : choisissez « Enregistrer au format PDF » et aucune marge. L’application de bureau l’écrit directement, à la taille de la figure.',
  figure_note: 'Chaque graphique est dessiné comme dans sa vue au moment où il a été ajouté (axes, couleurs, étiquettes, titres de Mise en forme, zoom), ou avec ses réglages habituels s’il n’était pas ouvert, dans le thème clair et sans la sélection, avec le dernier ajustement de son modèle : la figure suit les modèles réajustés. Elle est gardée dans le projet.',
  figure_resume: '{largeur} × {hauteur} mm · {n} panneau(x) · texte {police} pt', figure_ecrite: 'Figure écrite :',
  figure_exporter_quand_meme: '{n} panneau(x) n’ont pas pu être dessinés : {liste}. Exporter quand même, une case vide à leur place ?',
  figure_modele_absent: 'le modèle {nom} n’est plus dans le projet', figure_modele_non_ajuste: '{nom} n’a pas été ajusté dans le fichier d’origine',
  figure_vue_inconnue: 'vue inconnue', figure_pas_de_graphique: 'pas de graphique à dessiner', figure_image_impossible: 'L’image de la figure n’a pas pu être dessinée.',
  figure_delai: 'pas dessiné en {s} s (un fond de carte qui ne se charge pas ?)',
  figure_fond_absent: 'les tuiles de son fond de carte n’ont pas pu être téléchargées (hors ligne ?)',
  figure_fond_limite: 'Carte {lettre} : fond à environ {dpi} points par pouce et non {voulu}, pour demander moins de tuiles à son serveur',
  figure_fond_serveur: 'Carte {lettre} : fond à environ {dpi} points par pouce et non {voulu} : les tuiles ne sont pas demandées au-delà du niveau de zoom {zoom}',
  figure_fond_manquantes: 'Carte {lettre} : {n} des {total} tuiles du fond n’ont pas pu être téléchargées',
  figure_ouvrir: 'Ouvrir', figure_renommer: 'Renommer…', figure_dupliquer: 'Dupliquer', figure_supprimer: 'Supprimer la figure…', figure_copie: 'copie',
  figure_confirmer_supprimer: 'Supprimer la figure « {titre} » ? (Elle disparaît du projet à son prochain enregistrement.)',
  figure_ajoutee: 'Graphique ajouté à « {figure} ».',
}, {
  figure_composer: 'Compose a figure…', figure_ajouter: 'Add to a figure', figure_nouvelle: 'New figure', figure_nom: 'Figure',
  figure_figures: 'Figures', figure_sans_titre: 'Untitled figure', figure_chargement: 'Opening the figure…',
  figure_aucune: 'This project has no figure yet.', figure_supprimee: 'This figure was deleted.',
  figure_panneaux: 'Panels', figure_aucun_panneau: 'No panel yet.', figure_vues_ouvertes: 'Add a plot…', figure_ouverte: 'open',
  figure_aucune_vue_courte: 'No plot to add',
  figure_aucune_vue: 'Fit a model of the project, or open one of its plots (Plots menu), then choose it here; or right-click a plot › Add to a figure.',
  figure_une_case: '1 cell', figure_deux_colonnes: '2 columns', figure_deux_lignes: '2 rows', figure_etendue_aide: 'The panel’s room in the grid',
  figure_monter: 'Earlier in the order', figure_descendre: 'Later in the order',
  figure_reprendre: 'Take the settings of its open view again (axes, colours, labels, zoom)', figure_retirer: 'Remove from the figure',
  figure_hors_grille: 'no room in the grid: make it bigger, or change the panels’ order', figure_taille: 'Size', figure_largeur: 'Width',
  figure_colonne_1: '{mm} mm — one column', figure_colonne_15: '{mm} mm — one and a half columns', figure_colonne_2: '{mm} mm — two columns',
  figure_autre_largeur: 'Other width', figure_hauteur: 'Height', figure_hauteur_auto: 'from the panels ({mm} mm)',
  figure_hauteur_aide: 'The grid’s rows as high as the plots need, as they are on screen; untick to give the height',
  figure_grille: 'Grid', figure_lignes: 'Rows', figure_colonnes: 'Columns', figure_grilles: 'Common grids (rows × columns)',
  figure_texte: 'Text and lines', figure_police: 'Text', figure_police_aide: 'The size of the tick labels, axis titles and legends once printed (6 to 8 pt for most journals)',
  figure_ligne: 'Lines', figure_ligne_aide: 'The width of a curve’s line once printed; axes, grid and shapes half of it',
  figure_lettres: 'Letters', figure_lettres_aucune: 'none', figure_gras: 'bold', figure_taille_lettres: 'Letter size',
  figure_coin_hg: 'top left', figure_coin_hd: 'top right', figure_coin_bg: 'bottom left', figure_coin_bd: 'bottom right',
  figure_titres: 'Plot titles', figure_titres_aide: 'Each plot’s title above it (the letter and the figure’s caption are often enough)',
  figure_legendes: 'Legends', figure_export: 'Export', figure_format: 'Format', figure_pdf: 'PDF (vector)', figure_svg: 'SVG (vector)',
  figure_resolution: 'Resolution', figure_dpi: '{n} dpi', figure_compression: 'Compression',
  figure_compression_aide: 'LZW: lossless, the one journals ask for; none: a much bigger file', figure_sans_compression: 'none',
  figure_pixels: '{largeur} × {hauteur} pixels at {dpi} dpi.', figure_exporter: 'Export…',
  figure_navigateur_pdf: 'In a browser, the PDF goes through its print window: choose “Save as PDF” and no margins. The desktop application writes it directly, at the figure’s size.',
  figure_note: 'Each plot is drawn as in its view when it was added (axes, colours, labels, Format titles, zoom), or with its usual settings if it wasn’t open, in the light theme and without the selection, with its model’s latest fit: the figure follows refitted models. It is kept in the project.',
  figure_resume: '{largeur} × {hauteur} mm · {n} panel(s) · text {police} pt', figure_ecrite: 'Figure written:',
  figure_exporter_quand_meme: '{n} panel(s) couldn’t be drawn: {liste}. Export anyway, with an empty cell in their place?',
  figure_modele_absent: 'model {nom} is no longer in the project', figure_modele_non_ajuste: '{nom} wasn’t fitted in the original file',
  figure_vue_inconnue: 'unknown view', figure_pas_de_graphique: 'no plot to draw', figure_image_impossible: 'The figure’s picture couldn’t be drawn.',
  figure_delai: 'not drawn within {s} s (a map background that doesn’t load?)',
  figure_fond_absent: 'its map background’s tiles couldn’t be fetched (offline?)',
  figure_fond_limite: 'Map {lettre}: background at about {dpi} dpi rather than {voulu}, to ask its server for fewer tiles',
  figure_fond_serveur: 'Map {lettre}: background at about {dpi} dpi rather than {voulu}: tiles are not asked for beyond zoom level {zoom}',
  figure_fond_manquantes: 'Map {lettre}: {n} of the background’s {total} tiles couldn’t be fetched',
  figure_ouvrir: 'Open', figure_renommer: 'Rename…', figure_dupliquer: 'Duplicate', figure_supprimer: 'Delete the figure…', figure_copie: 'copy',
  figure_confirmer_supprimer: 'Delete the figure “{titre}”? (It leaves the project when the project is next saved.)',
  figure_ajoutee: 'Plot added to “{figure}”.',
});
