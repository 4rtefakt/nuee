// Builds nuee.io into dist/: the page in English (/) and in French (/fr/), their 404 pages, the
// assets, and what Cloudflare reads beside them (_headers, robots.txt, sitemap.xml).
//   node vitrine/construire.mjs        (wrangler runs it before `dev` and `deploy`)
// It needs Node 22 or later and nothing else. It reads:
//   - src/gabarit.html, the page, where {{key}} is a text of src/textes.mjs or a block made here;
//   - src/donnees.json, the figures and the quoted texts, computed by donnees.R;
//   - the application itself: the rules and texts of Which analysis?
//     (app/interface/choix_analyse.js) and the way a document writes the values it cites
//     (app/interface/references.js), so that the page says what the application says;
//   - the screenshots of the documentation site (docs/site/images);
//   - src/assets/carte.webp, the map under the tour's last figure, made by carte.py.

import { readFileSync, writeFileSync, mkdirSync, cpSync, rmSync, readdirSync, statSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { fileURLToPath, pathToFileURL } from 'node:url';
import path from 'node:path';
import * as textes from './src/textes.mjs';

const ICI = path.dirname(fileURLToPath(import.meta.url));
const DEPOT = path.resolve(ICI, '..');
const SRC = path.join(ICI, 'src');
const DIST = path.join(ICI, 'dist');
const lire = (...p) => readFileSync(path.join(...p), 'utf8');
const ecrire = (chemin, texte) => { mkdirSync(path.dirname(chemin), { recursive: true }); writeFileSync(chemin, texte); };
const importer = (...p) => import(pathToFileURL(path.join(DEPOT, ...p)).href);
const echapper = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

const D = JSON.parse(lire(SRC, 'donnees.json'));
const choix = await importer('app', 'interface', 'choix_analyse.js');
const { nombre, pourcentage, valeurP, lireReferences, separerCible, valeurReference } = await importer('app', 'interface', 'references.js');

// ------------------------------------------------------------ the two languages have the same keys
for (const cle of new Set([...Object.keys(textes.en), ...Object.keys(textes.fr)])) {
  if (!(cle in textes.en) || !(cle in textes.fr)) throw new Error(`textes.mjs: "${cle}" is missing in ${cle in textes.en ? 'fr' : 'en'}`);
}

// ------------------------------------------------------------ R code, coloured
// A line of R as HTML: comments, strings, keywords, numbers, the assignment arrow and the names of
// the functions called.
const MOTIF_R = /(#.*$)|("(?:[^"\\]|\\.)*")|\b(function|for|if|else|break|in|while|return|TRUE|FALSE|NULL)\b|((?<![\w.])\d+(?:\.\d+)?(?:e-?\d+)?)|(<-)|([A-Za-z_.][\w.]*)(?=\()/g;
function colorerR(ligne) {
  let html = '';
  let fin = 0;
  for (const m of ligne.matchAll(MOTIF_R)) {
    const classe = m[1] ? 'rc' : m[2] ? 'rs' : m[3] ? 'rk' : m[4] ? 'rn' : m[5] ? 'ra' : 'rf';
    html += `${echapper(ligne.slice(fin, m.index))}<span class="${classe}">${echapper(m[0])}</span>`;
    fin = m.index + m[0].length;
  }
  return html + echapper(ligne.slice(fin));
}

// ------------------------------------------------------------ the blocks of a page

// The numbers of a Methods paragraph that come from the model, marked (not the years of the
// references, a version number, nor the 1 of the gain's formula).
const marquerNombres = (texte) => echapper(texte).replace(/\d+\.\d+\.\d+|\b(?:19|20)\d\d\b|(?<=\()1(?= [−-] Q)|(-?\d+(?:[.,]\d+)?(?:[\s ]?%)?)/g,
  (tout, n) => (n ? `<span class="valeur">${n}</span>` : tout));

// The six steps of the tour: the family of analyses, its figure's caption with the colours' keys,
// and its pages of the documentation.
const ETAPES = [
  { forme: 'acp', cles: (t) => [[3, t.js_cle_cultures], [2, t.js_cle_prairies]], liens: ['docs_acp'] },
  { forme: 'spectres', cles: (t) => [[0, t.tour2_g1], [1, t.tour2_g2]], liens: ['docs_tuto_pls', 'docs_acp'] },
  { forme: 'baci', cles: (t) => [[0, t.tour3_g1], [1, t.tour3_g2]], liens: ['docs_tuto_baci', 'docs_regressions'] },
  { forme: 'communautes', cles: (t) => [[3, t.js_cle_cultures], [2, t.js_cle_prairies]], liens: ['docs_communautes'] },
  { forme: 'distance', cles: () => [], liens: ['docs_tuto_suivi', 'docs_suivis'] },
  { forme: 'carte', cles: (t) => [[1, t.tour6_g1], [3, ''], [5, ''], [0, t.tour6_g2]], liens: ['docs_tuto_suivi', 'docs_cartes'] },
];
function blocEtapes(t) {
  return ETAPES.map((e, i) => {
    const k = i + 1;
    const cles = e.cles(t).map(([c, nom]) => `<span class="cle"><i class="c${c}"></i>${echapper(nom)}</span>`).join('');
    const liens = e.liens.map((l) => `<a href="${t[l]}">${l.startsWith('docs_tuto') ? t.tour_tutoriel : t.tour_guide}</a>`).join('');
    return `        <li class="etape" data-forme="${e.forme}" id="tour-${e.forme}">
          <p class="etape-nom"><span>${String(k).padStart(2, '0')}</span>${t[`tour${k}_nom`]}</p>
          <h3>${t[`tour${k}_titre`]}</h3>
          <p>${t[`tour${k}_p`]}</p>
          <p class="legende"><span><span class="fig">Fig. ${k + 2}</span> ${t[`tour${k}_fig`]}</span>${cles ? ` <span class="cles">${cles}</span>` : ''}</p>
          <p class="etape-liens">${liens}</p>
        </li>`;
  }).join('\n');
}

// The R console: the command (typed by site.js when it comes into view) and its output.
const blocConsole = () => `<span class="invite">&gt; </span><span class="commande" id="commande">${colorerR(D.console.commande)}</span>\n`
  + `<span class="sortie" id="sortie">${D.console.sortie.map(echapper).join('\n')}</span>`;

// The script of Export to R: its header, then its function for the overall slope, with the line numbers.
function blocScript(s) {
  // (each line is a block of its own: no line break between them, the <pre> would show it too)
  const lignes = (ls, debut) => ls.map((l, i) => `<span class="ligne"><span class="num">${debut + i}</span>${colorerR(l) || ' '}</span>`).join('');
  return `${lignes(s.entete, 1)}<span class="ligne coupe"><span class="num">⋮</span></span>${lignes(s.coeur, s.de)}`;
}

// The Methods text in both languages (the switch of the sheet shows one), the page's own first.
function blocMethodes(t) {
  return [t.langue, t.autre_langue].map((l, i) => {
    const x = D.texte[l];
    return `          <div class="feuille-texte" data-langue="${l}" lang="${l}"${i ? ' hidden' : ''}>
            ${x.methodes.map((p) => `<p>${marquerNombres(p)}</p>`).join('\n            ')}
            <h4>${textes[l].pub_references}</h4>
            <ul>${x.references.map((r) => `<li>${echapper(r)}</li>`).join('')}</ul>
          </div>`;
  }).join('\n');
}

// A document's line as it is written, and as it is read: its references to the model named BACI
// (the skylark plots' mixed model) are read and written by the application's own functions
// (references.js), from the model's coefficients as the interface has them.
function blocDocument(t) {
  const modele = { regression: true, coefficients_regression: { ...D.document, terme: D.document.terme.map((x) => (x === 'after*plots' ? t.pub_doc_terme : x)) } };
  const source = t.pub_doc_texte;
  let ecrit = '';
  let lu = '';
  let fin = 0;
  for (const r of lireReferences(source)) {
    const cible = separerCible(r.cible, ['BACI']);
    const v = cible ? valeurReference(modele, cible.chemin, r, t.langue) : { erreur: 'modele_inconnu' };
    if (v.erreur) throw new Error(`textes.mjs: pub_doc_texte (${t.langue}), ${r.brut}: ${v.erreur}`);
    ecrit += `${echapper(source.slice(fin, r.debut))}<b>${echapper(r.brut)}</b>`;
    lu += `${echapper(source.slice(fin, r.debut))}<b>${v.texte}</b>`;
    fin = r.fin;
  }
  const citation = '[@stewartoaten1986]';
  if (!source.includes(citation)) throw new Error(`textes.mjs: pub_doc_texte (${t.langue}) should cite ${citation}`);
  ecrit = (ecrit + echapper(source.slice(fin))).replace(citation, `<b>${citation}</b>`);
  lu = (lu + echapper(source.slice(fin))).replace(citation, t.pub_doc_citation);
  return `          <div class="doc-demo">
            <p><span class="etiquette">${t.pub_doc_source}</span><code>${ecrit}</code></p>
            <p><span class="etiquette">${t.pub_doc_rendu}</span><span>${lu}</span></p>
          </div>`;
}

// The files Nuée opens and writes, as two bands that scroll (twice the list, for the loop).
const FORMATS = {
  entree: [['.xlsx', 'Excel'], ['.xls', 'Excel'], ['.csv', ''], ['.tsv', ''], ['.rds', 'R'], ['.RData', 'R'], ['.sav', 'SPSS'], ['.por', 'SPSS'], ['.dta', 'Stata'],
    ['.sas7bdat', 'SAS'], ['.xpt', 'SAS'], ['.omv', 'jamovi'], ['.gpkg', 'QGIS'], ['.tif', 'GeoTIFF'], ['.asc', 'ESRI'], ['.jdx', 'JCAMP-DX'], ['.spc', 'Galactic, Thermo'],
    ['.dbf', 'dBase'], ['.arff', 'Weka'], ['.bib', 'BibTeX'], ['KoboToolbox', ''], ['ODK Central', ''], ['Zotero', ''], ['Ctrl+V', '']],
  sortie: [['.xlsx', 'Excel'], ['.csv', ''], ['.rds', 'R'], ['.RData', 'R'], ['.sav', 'SPSS'], ['.dta', 'Stata'], ['.xpt', 'SAS'], ['.omv', 'jamovi'], ['.gpkg', 'QGIS'],
    ['.dbf', 'dBase'], ['.arff', 'Weka'], ['.R', ''], ['.Rmd', 'R Markdown'], ['.qmd', 'Quarto'], ['renv.lock', ''], ['.docx', 'Word'], ['.pdf', ''], ['.html', ''],
    ['.svg', ''], ['.png', ''], ['.tiff', '600 dpi'], ['.nuee', '']],
};
function blocFormats(liste) {
  const ul = (cache) => `<ul${cache ? ' aria-hidden="true"' : ''}>${liste.map(([f, de]) => `<li><b>${f}</b>${de ? `<span>${de}</span>` : ''}</li>`).join('')}</ul>`;
  return ul(false) + ul(true);
}

// What Nuée is checked against: the packages, what is compared, and how close.
const COCHE = '<svg viewBox="0 0 16 16" width="16" height="16" aria-hidden="true"><path d="M2.5 8.5l3.5 4 7.5-9" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>';
const REGISTRE = ['vegan · indicspecies', 'nlme · mgcv · emmeans', 'rtrim', 'Distance', 'terra · PROJ · spdep · gstat', 'ranger · gbm', null];
const blocRegistre = (t) => REGISTRE.map((paquets, i) => `        <tr><th scope="row">${paquets || t.conf_usp}</th><td>${t[`conf_l${i + 1}`]}</td><td>${t[`conf_v${i + 1}`]}</td><td class="coche">${COCHE}</td></tr>`).join('\n');

// Which analysis?: the application's rules and texts, for site.js to run them (choix_analyse.js).
function donneesChoix(langue) {
  const T = choix.TEXTES_CHOIX[langue];
  const ids = [...new Set(choix.REGLES.flatMap((r) => [r.choix, ...r.autres]))];
  const de = (prefixe) => Object.fromEntries(ids.map((id) => [id, T[`${prefixe}_${id}`]]));
  return {
    questions: choix.QUESTIONS.map((q) => [q, T[`qa_q_${q}`], T[`qa_ex_${q}`]]),
    buts: choix.BUTS.map((b) => [b, T[`qa_but_${b}`]]),
    reponses: Object.fromEntries(choix.REPONSES.map((r) => [r, [T[`qa_r_${r}`], T[`qa_rex_${r}`]]])),
    formulaire: choix.FORMULAIRE,
    regles: choix.REGLES,
    si: Object.fromEntries(ids.filter((id) => choix.CATALOGUE[id].si).map((id) => [id, choix.CATALOGUE[id].si])),
    regressions: ids.filter((id) => ['LM', 'GLM', 'GAM'].includes(choix.CATALOGUE[id].type)),
    notes: choix.NOTES.map((n) => ({ si: n.si, pour: n.pour, t: T[n.cle] })),
    noms: { ...de('qa_nom'), lmm: T.qa_nom_lmm, lmm_log: T.qa_nom_lmm_log }, pourquoi: de('qa_pourquoi'), conditions: de('qa_conditions'),
    t: { but: T.qa_but, recommandee: T.qa_recommandee, autres: T.qa_autres, pourquoi: T.qa_pourquoi, conditions: T.qa_conditions, conseil: T.qa_conseil,
      repetes: T.qa_d_repetes, annees: T.qa_d_annees, baci: T.qa_d_baci, coordonnees: T.qa_d_coordonnees, zeros: T.qa_zeros,
      aucune: T.qa_reponse_facultative, avec_site: T.qa_avec_site },
  };
}

// ------------------------------------------------------------ a page

const taillePng = (chemin) => { const b = readFileSync(chemin); return { largeur: b.readUInt32BE(16), hauteur: b.readUInt32BE(20) }; };

function textesDe(langue) {
  const t = { ...textes[langue] };
  const T = choix.TEXTES_CHOIX[langue];
  const F = D.formes;
  const n = (x, decimales = 0) => nombre(x, { decimales, langue });
  const pc = (x) => pourcentage(x, { langue });
  const s = D.texte[langue].script;
  // what the texts' {names} stand for
  const valeurs = {
    acp_n: n(F.acp.n), acp_k: n(F.acp.k), acp_r1: pc(F.acp.r2x[0]), acp_r2: pc(F.acp.r2x[1]), acp_dehors: F.acp.dehors,
    sp_n: n(F.spectres.n), sp_k: n(F.spectres.k), sp_montee: n(F.spectres.montee),
    baci_fermes: n(F.baci.fermes), baci_avant: n(F.baci.avant, 1), baci_apres: n(F.baci.apres, 1),
    baci_effet: n(F.baci.effet, 1), baci_bas: n(F.baci.bas, 1), baci_haut: n(F.baci.haut, 1),
    com_n: n(F.communautes.n), com_sp: n(F.communautes.especes), com_stress: n(F.communautes.stress, 3),
    com_r2: pc(F.communautes.r2), com_p: valeurP(F.communautes.p, { langue }),
    dist_n: n(F.distance.n), dist_p: pc(F.distance.p), dist_w: n(F.distance.w), carte_n: n(F.carte.n), carte_baisse: n(F.carte.baisse),
    // the skylarks' counts: TRIM's slope as a change a year ("−3.0%"), and in round numbers ("3%")
    cpt_n: n(F.comptages.n), cpt_sites: n(F.comptages.sites), cpt_annees: n(F.comptages.annees),
    cpt_pente: pourcentage(F.comptages.pente - 1, { decimales: 1, langue }), cpt_baisse: pc(1 - F.comptages.pente),
    lignes_moteur: n(D.chiffres.lignes_moteur), fichiers_moteur: n(D.chiffres.fichiers_moteur), verifications: n(D.chiffres.verifications),
    script_n: n(s.n), de: s.de, a: s.a, n: s.n,
  };
  for (const [cle, texte] of Object.entries(t)) {
    if (typeof texte === 'string') t[cle] = texte.replace(/\{(\w+)\}/g, (tout, nom) => (nom in valeurs ? valeurs[nom] : tout));
  }
  return { ...t, qa_titre_question: T.qa_titre_question, qa_titre_reponse: T.qa_titre_reponse, qa_titre_plan: T.qa_titre_plan };
}

function page(langue, v) {
  const t = textesDe(langue);
  const figure = taillePng(path.join(DEPOT, 'docs', 'site', 'images', langue, 'suivi-figure.png'));
  // what site.js needs: the shapes of the swarm, the rules of Which analysis?, a few texts
  const formes = Object.fromEntries(Object.entries(D.formes).map(([id, f]) => [id, { points: f.points, lignes: f.lignes, part_lignes: f.part_lignes, petit: f.petit, deco: f.deco }]));
  const donnees = {
    langue, formes, choix: donneesChoix(langue),
    t: Object.fromEntries(Object.entries(t).filter(([k]) => k.startsWith('js_')).map(([k, x]) => [k.slice(3), x])
      .concat([['axe_spectres', t.tour2_axe], ['evenement', t.tour3_evenement], ['axe_distance', t.tour5_axe],
        ['telecharger_windows', t.telecharger_windows], ['telecharger_mac', t.telecharger_mac], ['telecharger_linux', t.telecharger_linux],
        ['telecharger_ici', t.telecharger_ici]])),
  };
  const blocs = {
    bloc_etapes: blocEtapes(t), bloc_console: blocConsole(), bloc_script: blocScript(D.texte[langue].script), bloc_methodes: blocMethodes(t),
    bloc_document: blocDocument(t), bloc_formats_entree: blocFormats(FORMATS.entree), bloc_formats_sortie: blocFormats(FORMATS.sortie),
    bloc_registre: blocRegistre(t), figure_largeur: figure.largeur, figure_hauteur: figure.hauteur, v,
    donnees: JSON.stringify(donnees).replace(/</g, '\\u003c'),
  };
  // (one pass: what a block or a text holds is not read again, a document's {{BACI.est...}} stays as it is)
  return lire(SRC, 'gabarit.html').replace(/\{\{(\w+)\}\}/g, (tout, cle) => {
    const x = cle in blocs ? blocs[cle] : t[cle];
    if (x === undefined) throw new Error(`gabarit.html: {{${cle}}} is neither a text nor a block`);
    return x;
  }).replace(/(\/assets\/[\w./-]+\.(?:css|js|png|svg|webp))(\?v=\w+)?(?=")/g, `$1?v=${v}`);
}

function page404(langue, v) {
  const t = textes[langue];
  return `<!doctype html>
<html lang="${langue}">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>404 — Nuée</title>
<meta name="robots" content="noindex">
<link rel="icon" href="/assets/nuee.svg?v=${v}" type="image/svg+xml">
<link rel="stylesheet" href="/assets/site.css?v=${v}">
</head>
<body class="perdu">
<main>
  <p class="entree"><span class="entree-mot">404</span></p>
  <h1>${t.e404_titre}</h1>
  <p>${t.e404_p}</p>
  <p><a class="bouton" href="${t.url_accueil}">${t.e404_retour}</a></p>
</main>
</body>
</html>
`;
}

// ------------------------------------------------------------ dist/

rmSync(DIST, { recursive: true, force: true, maxRetries: 8, retryDelay: 150 });  // (Windows may still hold a file for a moment)
cpSync(path.join(SRC, 'assets'), path.join(DIST, 'assets'), { recursive: true });
for (const langue of ['en', 'fr']) {
  const images = path.join(DEPOT, 'docs', 'site', 'images', langue);
  const captures = path.join(DIST, 'assets', 'captures', langue);
  mkdirSync(captures, { recursive: true });
  cpSync(path.join(images, 'suivi-fenetre.png'), path.join(captures, 'fenetre.png'));
  cpSync(path.join(images, 'sombre', 'suivi-fenetre.png'), path.join(captures, 'fenetre-sombre.png'));
  cpSync(path.join(images, 'suivi-figure.png'), path.join(captures, 'figure.png'));
}

// the logo, and the icons browsers and phones ask for by name: the application's own
cpSync(path.join(DEPOT, 'app', 'interface', 'nuee.svg'), path.join(DIST, 'assets', 'nuee.svg'));
cpSync(path.join(DEPOT, 'app', 'icone', 'nuee.ico'), path.join(DIST, 'favicon.ico'));
cpSync(path.join(DEPOT, 'app', 'icone', 'nuee.png'), path.join(DIST, 'apple-touch-icon.png'));

// one stamp for every asset's address: a changed file changes them all, and each can be kept a year
const fichiers = (dossier) => readdirSync(dossier).flatMap((f) => { const c = path.join(dossier, f); return statSync(c).isDirectory() ? fichiers(c) : [c]; });
const empreinte = createHash('sha1');
for (const f of fichiers(path.join(DIST, 'assets')).sort()) empreinte.update(readFileSync(f));
const v = empreinte.digest('hex').slice(0, 8);

ecrire(path.join(DIST, 'index.html'), page('en', v));
ecrire(path.join(DIST, 'fr', 'index.html'), page('fr', v));
ecrire(path.join(DIST, '404.html'), page404('en', v));
ecrire(path.join(DIST, 'fr', '404.html'), page404('fr', v));
ecrire(path.join(DIST, '_headers'), `/assets/*
  Cache-Control: public, max-age=31536000, immutable

/*
  X-Content-Type-Options: nosniff
  X-Frame-Options: DENY
  Referrer-Policy: strict-origin-when-cross-origin
  Permissions-Policy: geolocation=(), camera=(), microphone=()
  Content-Security-Policy: default-src 'self'; img-src 'self' data:; style-src 'self'; script-src 'self'; font-src 'self'; connect-src 'self'; base-uri 'self'; form-action 'none'; frame-ancestors 'none'
`);
ecrire(path.join(DIST, 'robots.txt'), 'User-agent: *\nAllow: /\n\nSitemap: https://nuee.io/sitemap.xml\n');
const alternees = '<xhtml:link rel="alternate" hreflang="en" href="https://nuee.io/"/><xhtml:link rel="alternate" hreflang="fr" href="https://nuee.io/fr/"/>';
ecrire(path.join(DIST, 'sitemap.xml'), `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9" xmlns:xhtml="http://www.w3.org/1999/xhtml">
  <url><loc>https://nuee.io/</loc>${alternees}</url>
  <url><loc>https://nuee.io/fr/</loc>${alternees}</url>
</urlset>
`);
console.log(`dist/ built, assets ${v}`);
