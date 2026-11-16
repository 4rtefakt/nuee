// Builds the documentation website, in French and in English, into this folder (docs/site):
//   - the user guide, split into pages, from USER-GUIDE.txt and LISEZ-MOI-APPLICATION.txt, the
//     guides the application installs with itself (they stay the reference: edit them, not the pages);
//   - What's new, from CHANGELOG.md (English) and app/interface/nouveautes.js (French), the list
//     Help › What's new shows;
//   - the home page, the installation and the tutorials, from the Markdown files of sources/fr and
//     sources/en;
//   - the download links, from the version in app/package.json;
//   - the example plugins of exemple/plugins, as .nueeplugin files in greffons/ (greffons()).
//   node docs/site/construire.js        (markdown-it comes from app/node_modules: npm install in app/)
// The pages are plain HTML and CSS (assets/), with a small script for the light or dark theme.
// In the Markdown sources, `page:<id>` links to another page of the same language (page:guide.regressions,
// page:tutoriels.baci#methodes...) and `@/` starts a path from the site's root (@/images/fr/baci-choix.png):
// both become relative links, so the site works from any folder of any server.
'use strict';
const fs = require('fs');
const os = require('os');
const path = require('path');
const { pathToFileURL } = require('url');

const SITE = __dirname;
const RACINE = path.resolve(SITE, '..', '..');
const MarkdownIt = require(path.join(RACINE, 'app', 'node_modules', 'markdown-it'));
const md = new MarkdownIt({ html: true, typographer: false });
const VERSION = JSON.parse(fs.readFileSync(path.join(RACINE, 'app', 'package.json'), 'utf8')).version;
const DEPOT = 'https://github.com/4rtefakt/nuee';
const PUBLICATION = `${DEPOT}/releases/download/v${VERSION}`;

// ------------------------------------------------------------ the pages and their names

const TEXTES = {
  fr: {
    langue: 'Français', autre: 'en', nomAutre: 'English', titreSite: 'Nuée',
    accueil: 'Accueil', installation: 'Installation', guide: 'Guide', tutoriels: 'Tutoriels', nouveautes: 'Quoi de neuf',
    sauter: 'Aller au contenu', theme: 'Thème clair ou sombre', surCettePage: 'Sur cette page', pagesGuide: 'Le guide',
    precedent: 'Précédent', suivant: 'Suivant', pied: `Nuée ${VERSION} · logiciel libre (GPL-3.0) · <a href="${DEPOT}">code source</a> · <a href="${DEPOT}/issues">signaler un problème</a>`,
    sourceGuide: (f) => `Cette page reprend le guide installé avec Nuée (<a href="${DEPOT}/blob/main/${f}">${f}</a>, aussi dans Aide › Guide d’utilisation).`,
    versionDu: 'Version', menuGuide: 'Pages du guide',
  },
  en: {
    langue: 'English', autre: 'fr', nomAutre: 'Français', titreSite: 'Nuée',
    accueil: 'Home', installation: 'Install', guide: 'Guide', tutoriels: 'Tutorials', nouveautes: 'What’s new',
    sauter: 'Skip to content', theme: 'Light or dark theme', surCettePage: 'On this page', pagesGuide: 'The guide',
    precedent: 'Previous', suivant: 'Next', pied: `Nuée ${VERSION} · free software (GPL-3.0) · <a href="${DEPOT}">source code</a> · <a href="${DEPOT}/issues">report a problem</a>`,
    sourceGuide: (f) => `This page is part of the guide installed with Nuée (<a href="${DEPOT}/blob/main/${f}">${f}</a>, also in Help › User guide).`,
    versionDu: 'Version', menuGuide: 'Guide pages',
  },
};

// The guide's pages: the sections of the guide each takes, by their English title (the French
// guide has the same sections in the same order). A section the guide gains must be given a page.
const PAGES_GUIDE = [
  { id: 'demarrer', slug: { fr: 'demarrer', en: 'getting-started' }, titre: { fr: 'Démarrer', en: 'Getting started' },
    resume: { fr: 'Installer, ouvrir un tableau, enregistrer et retrouver son travail, le R utilisé par Nuée, l’explorateur, les menus, les fenêtres.',
      en: 'Installing, opening a table, saving and getting your work back, the R Nuée uses, the explorer, the menus, the windows.' },
    sections: ['Getting started', 'Opening', 'Saving and getting your work back', 'If the R engine stops', 'R used by Nuée', 'The explorer (on the left)', 'Menus and toolbar', 'Windows'] },
  { id: 'donnees', slug: { fr: 'donnees', en: 'data' }, titre: { fr: 'Les données', en: 'The data' },
    resume: { fr: 'Les données de terrain de KoboToolbox ou d’ODK, modifier les données, créer des variables, nettoyer, joindre un autre tableau, les étapes des données et leur mise à jour, résumer par groupe, les dates.',
      en: 'Field data from KoboToolbox or ODK, editing the data, making variables, cleaning, joining another table, data steps and updating the data, summarizing by group, dates.' },
    sections: ['Field data from KoboToolbox or ODK', 'Editing the data, making variables', 'Cleaning the data: find and replace, categories, units', 'Adding columns from another table',
      'Data steps and updating the data', 'Summarizing by group', 'Variable statistics', 'Dates as time'] },
  { id: 'modeles', slug: { fr: 'modeles', en: 'models' }, titre: { fr: 'Choisir et construire un modèle', en: 'Choosing and building a model' },
    resume: { fr: 'Quelle analyse ?, Nouveau modèle, les observations, la validation croisée, le jeu de test, prédire.',
      en: 'Which analysis?, New model, the observations, cross-validation, the test set, predicting.' },
    sections: ['Which analysis? (Model › Which analysis?)', 'New model (Model › New model)', 'Observations chosen by a condition', 'Cross-validation',
      'Test set', 'Predicting', 'Hierarchical models'] },
  { id: 'graphiques', slug: { fr: 'graphiques', en: 'plots' }, titre: { fr: 'Graphiques et figures', en: 'Plots and figures' },
    resume: { fr: 'Couleurs et étiquettes, mise en forme, contributions, figures pour publication.',
      en: 'Colours and labels, formatting, contributions, figures for publication.' },
    sections: ['Plots: colours and labels', 'Other plots', 'Formatting plots (for a paper, a report)', 'Contributions', 'Figures for publication'] },
  { id: 'acp-pls', slug: { fr: 'acp-pls', en: 'pca-pls' }, titre: { fr: 'ACP, PLS, OPLS et spectres', en: 'PCA, PLS, OPLS and spectra' },
    resume: { fr: 'OPLS, VIP, validation des PLS, modèles de classe, spectres et leur prétraitement.',
      en: 'OPLS, VIP, validating a PLS, class models, spectra and their preprocessing.' },
    sections: ['OPLS and OPLS-DA', 'VIP', 'Validation (PLS and PLS-DA)', 'Class models and the Coomans plot', 'Spectra'] },
  { id: 'regressions', slug: { fr: 'regressions', en: 'regressions' }, titre: { fr: 'Modèles linéaires, généralisés et mixtes', en: 'Linear, generalized and mixed models' },
    resume: { fr: 'Régressions, modèles mixtes, GLM et GAM, conditions du modèle, moyennes ajustées, sélection par AIC, un modèle par réponse.',
      en: 'Regressions, mixed models, GLMs and GAMs, model conditions, adjusted means, AIC selection, one model per response.' },
    sections: ['Linear, generalized and mixed models', 'One model per response'] },
  { id: 'tests', slug: { fr: 'tests', en: 'tests' }, titre: { fr: 'Tests statistiques', en: 'Statistical tests' },
    resume: { fr: 'Entre classes, entre deux variables, tests appariés, matrice des corrélations, avant/après (BACI).',
      en: 'Between classes, between two variables, paired tests, correlation matrix, before/after (BACI).' },
    sections: ['Statistical tests'] },
  { id: 'communautes', slug: { fr: 'communautes', en: 'communities' }, titre: { fr: 'Écologie des communautés', en: 'Community ecology' },
    resume: { fr: 'RDA, CA, CCA, espèces indicatrices, raréfaction, PCoA, NMDS, PERMANOVA, test de Mantel.',
      en: 'RDA, CA, CCA, indicator species, rarefaction, PCoA, NMDS, PERMANOVA, Mantel test.' },
    sections: ['Redundancy analysis and correspondence analysis', 'Canonical correspondence analysis (CCA)', 'Indicator species',
      'Rarefaction and species accumulation', 'PCoA, NMDS, PERMANOVA and Mantel test'] },
  { id: 'suivis', slug: { fr: 'suivis', en: 'monitoring' }, titre: { fr: 'Suivis : séries, TRIM, distances', en: 'Monitoring: series, TRIM, distance sampling' },
    resume: { fr: 'Séries et tendances, indices et tendances de comptages (TRIM), densité et effectif par échantillonnage par distances.',
      en: 'Series and trends, indices and trends of counts (TRIM), density and abundance by distance sampling.' },
    sections: ['Series', 'TRIM: indices and trends of counts', 'Distance sampling: density and abundance'] },
  { id: 'cartes', slug: { fr: 'cartes', en: 'maps' }, titre: { fr: 'Cartes et données spatiales', en: 'Maps and spatial data' },
    resume: { fr: 'Coordonnées et carte, GeoPackage et QGIS, valeurs de rasters, autocorrélation spatiale.',
      en: 'Coordinates and the map, GeoPackage and QGIS, raster values, spatial autocorrelation.' },
    sections: ['Maps and spatial data'] },
  { id: 'arbres', slug: { fr: 'arbres', en: 'trees' }, titre: { fr: 'Arbres, forêts, discriminante, k-moyennes', en: 'Trees, forests, discriminant analysis, k-means' },
    resume: { fr: 'Arbres de décision, analyse discriminante linéaire, k-moyennes, forêts aléatoires, arbres boostés.',
      en: 'Decision trees, linear discriminant analysis, k-means, random forests, boosted trees.' },
    sections: ['Decision trees, linear discriminant analysis and k-means', 'Random forests and boosted trees'] },
  { id: 'code-r', slug: { fr: 'code-r', en: 'r-code' }, titre: { fr: 'Votre propre modèle en code R', en: 'Your own model in R code' },
    resume: { fr: 'Un modèle écrit en R par vous (lm, glm, nls, mgcv, nlme, glmmTMB…) : ses données, ses résultats, ses textes, son export, et le code venu d’ailleurs.',
      en: 'A model you write in R (lm, glm, nls, mgcv, nlme, glmmTMB…): its data, its results, its texts, its export, and code from elsewhere.' },
    sections: ['Your own model in R code'] },
  { id: 'greffons', slug: { fr: 'greffons', en: 'plugins' }, titre: { fr: 'Greffons', en: 'Plugins' },
    resume: { fr: 'Installer un greffon, l’utiliser, le partager avec un projet, la confiance qu’il demande ; ceux de votre organisation.',
      en: 'Installing a plugin, using it, sharing it with a project, the trust it asks for; your organisation’s plugins.' },
    sections: ['Plugins'] },
  { id: 'ecrire-greffon', slug: { fr: 'ecrire-un-greffon', en: 'writing-a-plugin' }, titre: { fr: 'Écrire un greffon', en: 'Writing a plugin' },
    resume: { fr: 'Le manifeste, la fonction, ce qu’elle reçoit et ce qu’elle rend, l’essayer dans Nuée, l’empaqueter.',
      en: 'The manifest, the function, what it receives and what it returns, testing it in Nuée, packaging it.' },
    sections: ['Writing a plugin'] },
  { id: 'resultats', slug: { fr: 'resultats', en: 'results' }, titre: { fr: 'Résultats, documents et R', en: 'Results, documents and R' },
    resume: { fr: 'Texte des méthodes, rapport, documents avec citations, exports vers d’autres logiciels, export vers R, console R.',
      en: 'Methods text, report, documents with citations, exports to other software, Export to R, R console.' },
    sections: ['Methods text', 'Model report', 'Documents (articles, reports)', 'Exporting to other software', 'Export to R (script, R Markdown, Quarto)',
      'R console', 'The code'] },
  { id: 'versions', slug: { fr: 'versions', en: 'versions' }, titre: { fr: 'Versions et travail à plusieurs', en: 'Versions and working together' },
    resume: { fr: 'L’historique des versions d’un projet, Ce qui a changé, le partage sur GitHub ou GitLab et la connexion, travailler à plusieurs sur un projet.',
      en: 'A project’s history of versions, What changed, sharing on GitHub or GitLab and signing in, working together on a project.' },
    sections: ['History and versions (File › History)', 'What changed', 'Sharing on GitHub or GitLab', 'Signing in to GitHub or GitLab', 'Working together'] },
  { id: 'usp', slug: { fr: 'projets-usp', en: 'usp-projects' }, titre: { fr: 'Projets .usp', en: '.usp projects' },
    resume: { fr: 'L’historique du fichier, ce qui est identique aux valeurs enregistrées et ce qui diffère.',
      en: 'The file history, what matches the stored values and what differs.' },
    sections: ['File history', 'What matches the stored values (checked on 944 models from 133 .usp projects)', 'What differs'] },
];

// Every page: its place in each language, its title and how it is made.
const PAGES = [
  { id: 'accueil', chemin: { fr: 'fr/index.html', en: 'en/index.html' }, source: { fr: 'accueil.md', en: 'home.md' }, nav: 'accueil' },
  { id: 'installation', chemin: { fr: 'fr/installation.html', en: 'en/installation.html' }, source: { fr: 'installation.md', en: 'installation.md' }, nav: 'installation' },
  { id: 'guide', chemin: { fr: 'fr/guide/index.html', en: 'en/guide/index.html' }, nav: 'guide' },
  ...PAGES_GUIDE.map((g) => ({ id: `guide.${g.id}`, chemin: { fr: `fr/guide/${g.slug.fr}.html`, en: `en/guide/${g.slug.en}.html` }, nav: 'guide', guide: g })),
  { id: 'tutoriels', chemin: { fr: 'fr/tutoriels/index.html', en: 'en/tutorials/index.html' }, source: { fr: 'tutoriels.md', en: 'tutorials.md' }, nav: 'tutoriels' },
  { id: 'tutoriels.baci', chemin: { fr: 'fr/tutoriels/baci.html', en: 'en/tutorials/baci.html' }, source: { fr: 'tutoriel-baci.md', en: 'tutorial-baci.md' }, nav: 'tutoriels' },
  { id: 'tutoriels.pls', chemin: { fr: 'fr/tutoriels/pls-spectres.html', en: 'en/tutorials/pls-spectra.html' }, source: { fr: 'tutoriel-pls.md', en: 'tutorial-pls.md' }, nav: 'tutoriels' },
  { id: 'tutoriels.suivi', chemin: { fr: 'fr/tutoriels/tendances.html', en: 'en/tutorials/trends.html' }, source: { fr: 'tutoriel-suivi.md', en: 'tutorial-trends.md' }, nav: 'tutoriels' },
  { id: 'tutoriels.communautes', chemin: { fr: 'fr/tutoriels/communautes.html', en: 'en/tutorials/communities.html' }, source: { fr: 'tutoriel-communautes.md', en: 'tutorial-communities.md' }, nav: 'tutoriels' },
  { id: 'tutoriels.cartes', chemin: { fr: 'fr/tutoriels/cartes.html', en: 'en/tutorials/maps.html' }, source: { fr: 'tutoriel-cartes.md', en: 'tutorial-maps.md' }, nav: 'tutoriels' },
  { id: 'tutoriels.rapport', chemin: { fr: 'fr/tutoriels/rapport.html', en: 'en/tutorials/report.html' }, source: { fr: 'tutoriel-rapport.md', en: 'tutorial-report.md' }, nav: 'tutoriels' },
  { id: 'nouveautes', chemin: { fr: 'fr/nouveautes.html', en: 'en/whats-new.html' }, nav: 'nouveautes' },
];
const PAGE = Object.fromEntries(PAGES.map((p) => [p.id, p]));
const NAV = ['accueil', 'installation', 'guide', 'tutoriels', 'nouveautes'];

// ------------------------------------------------------------ small helpers

const echapper = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
const attribut = (s) => echapper(s).replace(/"/g, '&quot;');
const slug = (s) => String(s).normalize('NFD').replace(/\p{Diacritic}/gu, '').toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '').slice(0, 60);
// a relative link from one page to a file of the site (both paths from the site's root)
const relatif = (de, vers) => {
  const r = path.posix.relative(path.posix.dirname(de), vers);
  return r === '' ? path.posix.basename(vers) : r;
};
// The site names no other software in the asides that compare an option with one: a guide's
// "(like X)" or "(comme X)" after an option's name is left out of the pages.
const sansComparaison = (s) => s.replace(/\s*\((?:like|comme) [A-Z]{2,}\)/g, '');

// the size of a PNG image (its header), for <img width height>
function taillePng(fichier) {
  const b = fs.readFileSync(fichier);
  if (b.toString('ascii', 1, 4) !== 'PNG') return null;
  return { largeur: b.readUInt32BE(16), hauteur: b.readUInt32BE(20) };
}

// ------------------------------------------------------------ the guide: plain text to HTML

// The guide's sections: a title underlined with dashes, then its lines.
function sectionsGuide(fichier) {
  const lignes = fs.readFileSync(fichier, 'utf8').replace(/\r\n?/g, '\n').split('\n');
  const sections = [];
  let s = null;
  for (let i = 0; i < lignes.length; i += 1) {
    const l = lignes[i];
    const sous = lignes[i + 1];
    if (sous !== undefined && /^[-=]{3,}$/.test(sous) && l.trim() && !/^\s/.test(l) && Math.abs([...sous].length - [...l].length) <= 3) {
      if (sous[0] === '=') { i += 1; continue; }  // the guide's own title
      s = { titre: sansComparaison(l.trim()), lignes: [] };
      sections.push(s);
      i += 1;
      continue;
    }
    if (s) s.lignes.push(sansComparaison(l));
  }
  for (const x of sections) while (x.lignes.length && !x.lignes[x.lignes.length - 1].trim()) x.lignes.pop();
  return sections;
}

// The width the guide is wrapped at: what nearly all its lines stay within.
function largeurGuide(sections) {
  const n = sections.flatMap((s) => s.lignes).filter((l) => l.trim()).map((l) => [...l].length).sort((a, b) => a - b);
  return n[Math.floor(n.length * 0.97)];
}

const retrait = (l) => l.match(/^ */)[0].length;
const vide = (l) => !l.t.trim();

// Lines, each with its text and its length as written (the paragraph rule below needs it), into
// blocks: paragraphs, lists, definitions, preformatted text, indented text.
function blocs(lignes) {
  const res = [];
  let para = null;
  const fin = () => { if (para) res.push({ type: 'p', lignes: para }); para = null; };
  let i = 0;
  while (i < lignes.length) {
    const l = lignes[i];
    if (vide(l)) { fin(); i += 1; continue; }
    const r = retrait(l.t);
    if (r === 0) {
      const puce = /^- /.test(l.t);
      const numero = l.t.match(/^(\d+)\. /);
      if (puce || numero) {
        fin();
        const decalage = puce ? 2 : numero[0].length;
        const element = [{ t: l.t.slice(decalage), n: l.n }];
        i += 1;
        while (i < lignes.length && !vide(lignes[i]) && retrait(lignes[i].t) >= decalage) {
          element.push({ t: lignes[i].t.slice(decalage), n: lignes[i].n });
          i += 1;
        }
        const type = puce ? 'ul' : 'ol';
        const dernier = res[res.length - 1];
        if (dernier && dernier.type === type) dernier.elements.push(blocs(element));
        else res.push({ type, elements: [blocs(element)] });
        continue;
      }
      (para ??= []).push(l);
      i += 1;
      continue;
    }
    // lines more indented than the text around them
    fin();
    const groupe = [];
    while (i < lignes.length && !vide(lignes[i]) && retrait(lignes[i].t) > 0) { groupe.push(lignes[i]); i += 1; }
    res.push(blocIndente(groupe));
  }
  fin();
  return res;
}

function blocIndente(groupe) {
  const k = Math.min(...groupe.map((l) => retrait(l.t)));
  const sous = groupe.map((l) => ({ t: l.t.slice(k), n: l.n }));
  if (/^(- |\d+\. )/.test(sous[0].t)) return { type: 'groupe', blocs: blocs(sous) };
  const aligne = sous.some((l) => /\S {2,}\S/.test(l.t.trim()));
  const code = sous.every((l) => / <- |^\S+ ~ |^\w+\(/.test(l.t.trim()));
  if (code) return { type: 'pre', texte: sous.map((l) => l.t).join('\n') };
  if (aligne) return definitions(sous) || { type: 'pre', texte: sous.map((l) => l.t).join('\n') };
  return { type: 'retrait', blocs: blocs(sous) };
}

// "term   definition" lines, a definition going on in more indented lines: a list of definitions,
// or null when the lines are a table (several columns) rather than terms
function definitions(sous) {
  const elements = [];
  let suites = 0;
  for (const l of sous) {
    if (retrait(l.t) > 0) {
      if (!elements.length) return null;
      elements[elements.length - 1].def.push(l.t.trim());
      suites += 1;
      continue;
    }
    const m = l.t.match(/^(\S(?:.*?\S)?) {2,}(\S.*)$/) || l.t.match(/^([^:\s][^:]{0,29}):\s+(\S.*)$/);
    if (!m || [...m[1]].length > 30 || / {2,}/.test(m[2])) return null;
    elements.push({ terme: m[1], def: [m[2]] });
  }
  if (!suites && !elements.every((e) => /[;.]$/.test(e.def[e.def.length - 1]))) return null;
  return { type: 'dl', elements };
}

// A paragraph's lines: a line that ends a sentence, when the next line starts with a capital and
// its first word would have fitted on the line, ends a paragraph (the guide starts a new one on a
// new line, without a blank line).
function paragraphes(lignes, largeur) {
  const res = [];
  let courant = '';
  lignes.forEach((l, k) => {
    const texte = l.t.trim();
    courant = !courant ? texte : /\p{L}[–-]$/u.test(courant) ? courant + texte : `${courant} ${texte}`;
    const suivante = lignes[k + 1];
    if (!suivante) return;
    const mot = suivante.t.trim().split(/\s+/)[0];
    if (/[.:;!?]["»”)]?$/.test(l.t.trimEnd()) && /^["«“]?\s?\p{Lu}/u.test(mot) && l.n + 1 + [...mot].length <= largeur) { res.push(courant); courant = ''; }
  });
  if (courant) res.push(courant);
  return res;
}

// a paragraph's text: escaped, its web addresses and references to other sections linked, its
// lead-in in bold
function texteGuide(texte, ctx, avecEntree = true) {
  let h = echapper(texte);
  const liens = [];
  h = h.replace(/https?:\/\/[^\s<>"]*[^\s<>".,;:!?)»]/g, (u) => {
    liens.push(`<a href="${attribut(u.replace(/&amp;/g, '&'))}">${u}</a>`);
    return `\u0000${liens.length - 1}\u0000`;
  });
  for (const [alias, url] of ctx.renvois) {
    const motif = new RegExp(`(\\b(?:see|See|voir|Voir) +)(["«“] ?)?(${alias.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')})(?![\\p{L}\\d])`, 'gu');
    h = h.replace(motif, (_, avant, guillemet = '', titre) => {
      liens.push(`<a href="${attribut(url)}">${titre}</a>`);
      return `${avant}${guillemet}\u0000${liens.length - 1}\u0000`;
    });
  }
  h = h.replace(/\u0000(\d+)\u0000/g, (_, k) => liens[k]);
  if (avecEntree) {
    // "Random slopes: under each…", "Plots › Series (…): …": the lead-in, when it is a name
    const m = h.match(/^([^<:]{1,60}?)( ?:)( |$)/);
    if (m) {
      const entree = m[1];
      const mots = entree.trim().split(/\s+/).length;
      const ouvertes = (entree.match(/\(/g) || []).length === (entree.match(/\)/g) || []).length;
      if (ouvertes && !/[.;]\s/.test(entree) && (entree.includes('›') || mots <= 6)) h = `<strong>${entree}${m[2]}</strong>${h.slice(entree.length + m[2].length)}`;
    }
  }
  return h;
}

function rendreBlocs(liste, ctx, dansListe = false) {
  return liste.map((b) => {
    if (b.type === 'p') {
      const ps = paragraphes(b.lignes, ctx.largeur);
      if (dansListe && liste.length === 1 && ps.length === 1) return texteGuide(ps[0], ctx);
      return ps.map((p) => `<p>${texteGuide(p, ctx)}</p>`).join('\n');
    }
    if (b.type === 'ul' || b.type === 'ol') return `<${b.type}>\n${b.elements.map((e) => `<li>${rendreBlocs(e, ctx, true)}</li>`).join('\n')}\n</${b.type}>`;
    if (b.type === 'groupe') return rendreBlocs(b.blocs, ctx, dansListe);
    if (b.type === 'retrait') return `<div class="retrait">\n${rendreBlocs(b.blocs, ctx)}\n</div>`;
    if (b.type === 'pre') return `<pre>${echapper(b.texte)}</pre>`;
    if (b.type === 'dl') return `<dl>\n${b.elements.map((e) => `<dt>${echapper(e.terme)}</dt><dd>${texteGuide(e.def.join(' '), ctx, false)}</dd>`).join('\n')}\n</dl>`;
    throw new Error(`unknown block ${b.type}`);
  }).join('\n');
}

// the names a section is referred to by: its title, without what follows "(" or ":"
function alias(titre) {
  const a = new Set([titre, titre.replace(/ \(.*\)$/, ''), titre.replace(/ ?:.*$/, '')]);
  return [...a].filter((x) => x.length >= 4);
}

const titreCourt = (titre) => titre.replace(/ \(.*\)$/, '');

function preparerGuide() {
  const guides = {
    en: { fichier: 'USER-GUIDE.txt', sections: sectionsGuide(path.join(RACINE, 'USER-GUIDE.txt')) },
    fr: { fichier: 'LISEZ-MOI-APPLICATION.txt', sections: sectionsGuide(path.join(RACINE, 'LISEZ-MOI-APPLICATION.txt')) },
  };
  if (guides.en.sections.length !== guides.fr.sections.length) {
    throw new Error(`USER-GUIDE.txt has ${guides.en.sections.length} sections, LISEZ-MOI-APPLICATION.txt ${guides.fr.sections.length}: the two guides must have the same sections, in the same order`);
  }
  // each section's page, by its English title; the French one at the same place goes with it
  const page = {};
  for (const g of PAGES_GUIDE) for (const t of g.sections) page[t] = g;
  const manquantes = guides.en.sections.map((s) => s.titre).filter((t) => !page[t]);
  if (manquantes.length) throw new Error(`sections of the guide without a page (docs/site/construire.js, PAGES_GUIDE): ${manquantes.join('; ')}`);
  const absentes = PAGES_GUIDE.flatMap((g) => g.sections).filter((t) => !guides.en.sections.some((s) => s.titre === t));
  if (absentes.length) throw new Error(`PAGES_GUIDE names sections the guide no longer has: ${absentes.join('; ')}`);
  for (const langue of ['fr', 'en']) {
    const g = guides[langue];
    g.largeur = largeurGuide(g.sections);
    g.sections.forEach((s, k) => {
      s.page = page[guides.en.sections[k].titre];
      s.ancre = slug(titreCourt(s.titre));
      s.cleEn = guides.en.sections[k].titre;
    });
    const ancres = new Set();
    for (const s of g.sections) { let a = s.ancre; let n = 2; while (ancres.has(a)) a = `${s.ancre}-${n++}`; s.ancre = a; ancres.add(a); }
  }
  return guides;
}

// ------------------------------------------------------------ What's new

async function nouveautes() {
  // English: CHANGELOG.md, as GitHub shows it
  const versionsEn = [];
  let v = null;
  for (const l of fs.readFileSync(path.join(RACINE, 'CHANGELOG.md'), 'utf8').replace(/\r\n?/g, '\n').split('\n')) {
    const t = l.match(/^## (\S+) — (\d{4}-\d{2}-\d{2})/);
    if (t) { v = { version: t[1], date: t[2], elements: [] }; versionsEn.push(v); continue; }
    if (v && l.startsWith('- ')) v.elements.push(md.renderInline(sansComparaison(l.slice(2))));
    else if (v && l.trim() && v.elements.length) v.elements[v.elements.length - 1] += ` ${md.renderInline(sansComparaison(l.trim()))}`;
  }
  // French: the list the application shows (app/interface/nouveautes.js, an ES module)
  const copie = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-site-')), 'nouveautes.mjs');
  fs.copyFileSync(path.join(RACINE, 'app', 'interface', 'nouveautes.js'), copie);
  const { NOUVEAUTES } = await import(pathToFileURL(copie).href);
  fs.rmSync(path.dirname(copie), { recursive: true, force: true });
  const versionsFr = NOUVEAUTES.map((x) => ({ version: x.version, date: x.date, elements: x.fr.map((e) => echapper(sansComparaison(e))) }));
  return { fr: versionsFr, en: versionsEn };
}

// ------------------------------------------------------------ the example plugins

// Each example plugin of exemple/plugins as a .nueeplugin, the file Nuée installs (File › Plugins…):
// its folder's files in a zip archive, written by the interface's own zip writer (app/interface/zip.js)
// with a fixed date, so that an unchanged plugin gives the same bytes at each build.
async function greffons() {
  const source = path.join(RACINE, 'exemple', 'plugins');
  const copie = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'nuee-site-')), 'zip.mjs');
  fs.copyFileSync(path.join(RACINE, 'app', 'interface', 'zip.js'), copie);
  const { zip } = await import(pathToFileURL(copie).href);
  fs.rmSync(path.dirname(copie), { recursive: true, force: true });
  const ecrits = [];
  fs.mkdirSync(path.join(SITE, 'greffons'), { recursive: true });
  for (const id of fs.readdirSync(source).filter((d) => fs.existsSync(path.join(source, d, 'plugin.json'))).sort()) {
    const lister = (d) => fs.readdirSync(path.join(source, id, d), { withFileTypes: true })
      .flatMap((e) => (e.isDirectory() ? lister(path.posix.join(d, e.name)) : [path.posix.join(d, e.name)]));
    // (their text with LF line ends, whatever the checkout: the same bytes on every system)
    const fichiers = lister('').sort().map((f) => [f, new TextEncoder().encode(fs.readFileSync(path.join(source, id, f), 'utf8').replace(/\r\n?/g, '\n'))]);
    const nom = path.posix.join('greffons', `${id}.nueeplugin`);
    fs.writeFileSync(path.join(SITE, nom), zip(fichiers, new Date(2026, 10, 16, 12, 0, 0)));
    ecrits.push(nom);
  }
  return ecrits;
}

const dateLongue = (iso, langue) => new Intl.DateTimeFormat(langue === 'fr' ? 'fr-FR' : 'en-GB', { day: 'numeric', month: 'long', year: 'numeric', timeZone: 'UTC' })
  .format(new Date(`${iso}T00:00:00Z`));

// ------------------------------------------------------------ Markdown pages

// `page:` and `@/` links made relative; {{version}} and the download links filled in; a lone image with a
// title becomes a figure with its caption (and its size, read from the file)
function rendreMarkdown(texte, langue, chemin) {
  const remplace = texte
    .replace(/\{\{version\}\}/g, VERSION)
    .replace(/\{\{telechargement:(\w+)\}\}/g, (_, quoi) => {
      const fichiers = { windows: `Nuee-Setup-${VERSION}.exe`, macarm64: `Nuee-${VERSION}-arm64.dmg`, macx64: `Nuee-${VERSION}-x64.dmg`, deb: `Nuee-${VERSION}-amd64.deb`, appimage: `Nuee-${VERSION}-x86_64.AppImage`, targz: `Nuee-${VERSION}-x64.tar.gz` };
      if (quoi === 'publication') return `${DEPOT}/releases/tag/v${VERSION}`;
      if (quoi === 'toutes') return `${DEPOT}/releases`;
      if (!fichiers[quoi]) throw new Error(`unknown download: ${quoi}`);
      return `${PUBLICATION}/${fichiers[quoi]}`;
    })
    .replace(/\{\{fichier:(\w+)\}\}/g, (_, quoi) => {
      const fichiers = { windows: `Nuee-Setup-${VERSION}.exe`, macarm64: `Nuee-${VERSION}-arm64.dmg`, macx64: `Nuee-${VERSION}-x64.dmg`, deb: `Nuee-${VERSION}-amd64.deb`, appimage: `Nuee-${VERSION}-x86_64.AppImage`, targz: `Nuee-${VERSION}-x64.tar.gz` };
      return fichiers[quoi];
    });
  let h = md.render(remplace);
  h = h.replace(/(href|src)="(page:|@\/)([^"]*)"/g, (_, attr, sorte, cible) => `${attr}="${attribut(lien(sorte, cible, langue, chemin))}"`);
  // a screenshot and, when there is one (images/<langue>/sombre/), its dark version, which the
  // site's dark theme shows instead (site.css)
  h = h.replace(/<p><img src="([^"]+)" alt="([^"]*)" title="([^"]*)"><\/p>/g, (_, src, alt, titre) => {
    const fichier = (s) => path.join(SITE, path.posix.normalize(path.posix.join(path.posix.dirname(chemin), s)));
    const image = (s, classe) => {
      const t = taillePng(fichier(s));
      const taille = t ? ` width="${t.largeur}" height="${t.hauteur}"` : '';
      return `<a${classe ? ` class="${classe}"` : ''} href="${s}"><img src="${s}" alt="${alt}"${taille} loading="lazy"></a>`;
    };
    const sombre = path.posix.join(path.posix.dirname(src), 'sombre', path.posix.basename(src));
    const images = fs.existsSync(fichier(sombre)) ? image(src, 'clair-seul') + image(sombre, 'sombre-seul') : image(src);
    return `<figure>${images}<figcaption>${titre}</figcaption></figure>`;
  });
  return h;
}

function lien(sorte, cible, langue, depuis) {
  if (sorte === '@/') {
    const [fichier] = cible.split('#');
    if (!fs.existsSync(path.join(SITE, fichier))) throw new Error(`${depuis}: no such file in the site: @/${cible}`);
    return relatif(depuis, cible);
  }
  const [id, ancre] = cible.split('#');
  const p = PAGE[id];
  if (!p) throw new Error(`${depuis}: no such page: page:${cible}`);
  return relatif(depuis, p.chemin[langue]) + (ancre ? `#${ancre}` : '');
}

// The headings of a page (h2), for its "On this page" list; each given an anchor.
function ancrer(html) {
  const vus = new Set();
  const titres = [];
  const h = html.replace(/<h2>(.*?)<\/h2>/g, (_, contenu) => {
    let a = slug(contenu.replace(/<[^>]+>/g, ''));
    let n = 2;
    const base = a;
    while (vus.has(a)) a = `${base}-${n++}`;
    vus.add(a);
    titres.push({ ancre: a, titre: contenu });
    return `<h2 id="${a}">${contenu}</h2>`;
  });
  return { html: h, titres };
}

// ------------------------------------------------------------ the page around the content

function gabarit({ langue, page, titre, description, contenu, cote = '' }) {
  const T = TEXTES[langue];
  const chemin = page.chemin[langue];
  const vers = (c) => relatif(chemin, c);
  const autre = T.autre;
  const nav = NAV.map((n) => {
    const actuelle = page.nav === n;
    return `<a href="${vers(PAGE[n].chemin[langue])}"${actuelle ? ' aria-current="page"' : ''}>${T[n]}</a>`;
  }).join('\n      ');
  return `<!doctype html>
<html lang="${langue}">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>${titre === T.titreSite ? 'Nuée' : `${echapper(titre)} · Nuée`}</title>
<meta name="description" content="${attribut(description)}">
<meta name="color-scheme" content="light dark">
<link rel="icon" href="${vers('assets/nuee.svg')}" type="image/svg+xml">
<link rel="stylesheet" href="${vers('assets/site.css')}">
<link rel="alternate" hreflang="${langue}" href="${vers(chemin)}">
<link rel="alternate" hreflang="${autre}" href="${vers(page.chemin[autre])}">
<script src="${vers('assets/site.js')}"></script>
</head>
<body>
<a class="sauter" href="#contenu">${T.sauter}</a>
<header class="entete">
  <div class="entete-interieur">
    <a class="marque" href="${vers(PAGE.accueil.chemin[langue])}"><img src="${vers('assets/nuee.svg')}" alt="" width="28" height="28"><span>Nuée</span></a>
    <nav class="principale" aria-label="${langue === 'fr' ? 'Pages du site' : 'Site pages'}">
      ${nav}
    </nav>
    <div class="outils">
      <a class="langue" href="${vers(page.chemin[autre])}" hreflang="${autre}" lang="${autre}">${T.nomAutre}</a>
      <button class="bouton-theme" type="button" title="${attribut(T.theme)}" aria-label="${attribut(T.theme)}" hidden><svg viewBox="0 0 24 24" width="18" height="18" aria-hidden="true"><path d="M12 3a9 9 0 1 0 9 9c0-.5 0-.9-.1-1.3A6 6 0 0 1 12.3 3.1 9 9 0 0 0 12 3Z" fill="currentColor"/></svg></button>
    </div>
  </div>
</header>
<div class="corps${cote ? ' avec-cote' : ''}">
${cote}<main id="contenu">
${contenu}
</main>
</div>
<footer class="pied"><p>${T.pied}</p></footer>
</body>
</html>
`;
}

// the guide's pages, beside the page (on a phone: at its top, folded)
function coteGuide(langue, actuelle) {
  const T = TEXTES[langue];
  const chemin = actuelle.chemin[langue];
  const liens = [`<li><a href="${relatif(chemin, PAGE.guide.chemin[langue])}"${actuelle.id === 'guide' ? ' aria-current="page"' : ''}>${T.pagesGuide}</a></li>`]
    .concat(PAGES_GUIDE.map((g) => {
      const p = PAGE[`guide.${g.id}`];
      return `<li><a href="${relatif(chemin, p.chemin[langue])}"${p.id === actuelle.id ? ' aria-current="page"' : ''}>${echapper(g.titre[langue])}</a></li>`;
    }));
  return `<aside class="cote" aria-label="${attribut(T.menuGuide)}">
<details open><summary>${T.menuGuide}</summary>
<ul>
${liens.join('\n')}
</ul>
</details>
</aside>
`;
}

const surCettePage = (langue, titres) => (titres.length > 2 ? `<nav class="sommaire" aria-label="${attribut(TEXTES[langue].surCettePage)}">
<p>${TEXTES[langue].surCettePage}</p>
<ul>
${titres.map((t) => `<li><a href="#${t.ancre}">${t.titre.replace(/<[^>]+>/g, '')}</a></li>`).join('\n')}
</ul>
</nav>
` : '');

// ------------------------------------------------------------ build

async function construire() {
  const guides = preparerGuide();
  const versions = await nouveautes();
  const ecrits = [];
  const ecrire = (chemin, html) => {
    const f = path.join(SITE, chemin);
    fs.mkdirSync(path.dirname(f), { recursive: true });
    fs.writeFileSync(f, html);
    ecrits.push(chemin);
  };

  for (const langue of ['fr', 'en']) {
    const T = TEXTES[langue];
    const g = guides[langue];

    // Markdown pages
    for (const page of PAGES.filter((p) => p.source)) {
      const chemin = page.chemin[langue];
      const source = fs.readFileSync(path.join(SITE, 'sources', langue, page.source[langue]), 'utf8');
      const titre = (source.match(/^# (.+)$/m) || [])[1] || T.titreSite;
      const description = (source.match(/^<!-- description: (.+) -->$/m) || [])[1] || '';
      const { html, titres } = ancrer(rendreMarkdown(source, langue, chemin));
      const sommaire = page.id.startsWith('tutoriels.') ? surCettePage(langue, titres) : '';
      // (the list of the page's sections after its title and its lead paragraph)
      const contenu = sommaire ? html.replace(/(<\/h1>\n(?:<p[^>]*>.*?<\/p>\n)?)/s, `$1${sommaire}`) : html;
      ecrire(chemin, gabarit({ langue, page, titre: titre.replace(/<[^>]+>/g, ''), description, contenu }));
    }

    // the guide: its index, then a page per group of sections
    const indexGuide = PAGE.guide;
    const cheminIndex = indexGuide.chemin[langue];
    const listeGuide = PAGES_GUIDE.map((pg) => {
      const p = PAGE[`guide.${pg.id}`];
      const sections = g.sections.filter((s) => s.page === pg).sort((a, b) => pg.sections.indexOf(a.cleEn) - pg.sections.indexOf(b.cleEn));
      return `<li><a href="${relatif(cheminIndex, p.chemin[langue])}"><strong>${echapper(pg.titre[langue])}</strong></a> — ${echapper(pg.resume[langue])}
<ul class="sections">${sections.map((s) => `<li><a href="${relatif(cheminIndex, p.chemin[langue])}#${s.ancre}">${echapper(titreCourt(s.titre))}</a></li>`).join('')}</ul></li>`;
    }).join('\n');
    const introGuide = langue === 'fr'
      ? `<h1>Guide d’utilisation</h1>
<p>Le guide complet de Nuée, en pages. C’est le guide installé avec l’application (Aide › Guide d’utilisation › Guide complet), écrit pour la version ${VERSION}. Pour commencer, les <a href="${relatif(cheminIndex, PAGE.tutoriels.chemin.fr)}">tutoriels</a> suivent six études du début à la fin.</p>`
      : `<h1>User guide</h1>
<p>Nuée’s full guide, in pages. It is the guide installed with the application (Help › User guide › Full guide), written for version ${VERSION}. To start with, the <a href="${relatif(cheminIndex, PAGE.tutoriels.chemin.en)}">tutorials</a> follow six studies from start to finish.</p>`;
    ecrire(cheminIndex, gabarit({ langue, page: indexGuide, titre: langue === 'fr' ? 'Guide d’utilisation' : 'User guide',
      description: langue === 'fr' ? 'Le guide d’utilisation de Nuée, en pages.' : 'Nuée’s user guide, in pages.',
      contenu: `${introGuide}\n<ul class="liste-guide">\n${listeGuide}\n</ul>\n<p class="note">${T.sourceGuide(g.fichier)}</p>`,
      cote: coteGuide(langue, indexGuide) }));

    PAGES_GUIDE.forEach((pg, k) => {
      const page = PAGE[`guide.${pg.id}`];
      const chemin = page.chemin[langue];
      const sections = g.sections.filter((s) => s.page === pg).sort((a, b) => pg.sections.indexOf(a.cleEn) - pg.sections.indexOf(b.cleEn));
      // a reference to a section of the guide: on this page, its anchor; elsewhere, the other page's
      const renvois = [];
      for (const s of g.sections) for (const a of alias(s.titre)) renvois.push([a, (s.page === pg ? '' : relatif(chemin, PAGE[`guide.${s.page.id}`].chemin[langue])) + `#${s.ancre}`]);
      renvois.sort((a, b) => b[0].length - a[0].length);
      const ctx = { largeur: g.largeur, renvois };
      // (a page of a single section of its own name: no second heading)
      const seule = sections.length === 1 && titreCourt(sections[0].titre) === pg.titre[langue];
      const corps = sections.map((s) => `<section id="${s.ancre}">\n${seule ? '' : `<h2>${echapper(s.titre)}</h2>\n`}${rendreBlocs(blocs(s.lignes.map((l) => ({ t: l, n: [...l].length }))), ctx)}\n</section>`).join('\n');
      const sommaire = surCettePage(langue, sections.map((s) => ({ ancre: s.ancre, titre: echapper(titreCourt(s.titre)) })));
      const avant = PAGES_GUIDE[k - 1];
      const apres = PAGES_GUIDE[k + 1];
      const suite = `<nav class="suite" aria-label="${langue === 'fr' ? 'Pages voisines' : 'Neighbouring pages'}">${avant ? `<a rel="prev" href="${relatif(chemin, PAGE[`guide.${avant.id}`].chemin[langue])}">← ${echapper(avant.titre[langue])}</a>` : '<span></span>'}${apres ? `<a rel="next" href="${relatif(chemin, PAGE[`guide.${apres.id}`].chemin[langue])}">${echapper(apres.titre[langue])} →</a>` : ''}</nav>`;
      ecrire(chemin, gabarit({ langue, page, titre: pg.titre[langue], description: pg.resume[langue],
        contenu: `<p class="fil"><a href="${relatif(chemin, PAGE.guide.chemin[langue])}">${langue === 'fr' ? 'Guide' : 'Guide'}</a></p>\n<h1>${echapper(pg.titre[langue])}</h1>\n${sommaire}<div class="texte-guide">\n${corps}\n</div>\n<p class="note">${T.sourceGuide(g.fichier)}</p>\n${suite}`,
        cote: coteGuide(langue, page) }));
    });

    // What's new
    const page = PAGE.nouveautes;
    const vs = versions[langue];
    const intro = langue === 'fr'
      ? `<h1>Quoi de neuf</h1>\n<p>Ce qui a changé à chaque version : la même liste que dans Aide › Quoi de neuf. Les versions se téléchargent sur la page des <a href="${DEPOT}/releases">publications</a>.</p>`
      : `<h1>What’s new</h1>\n<p>What changed in each version: the same list as Help › What’s new in the application (<a href="${DEPOT}/blob/main/CHANGELOG.md">CHANGELOG.md</a>). Every version can be downloaded from the <a href="${DEPOT}/releases">Releases</a> page.</p>`;
    const versionsHtml = vs.map((x) => `<section id="v${x.version.replace(/\./g, '-')}">\n<h2>${T.versionDu} ${x.version} <span class="date">${dateLongue(x.date, langue)}</span></h2>\n<ul>\n${x.elements.map((e) => `<li>${e}</li>`).join('\n')}\n</ul>\n</section>`).join('\n');
    const liste = `<nav class="sommaire versions" aria-label="${langue === 'fr' ? 'Versions' : 'Versions'}"><p>${langue === 'fr' ? 'Versions' : 'Versions'}</p><ul>${vs.map((x) => `<li><a href="#v${x.version.replace(/\./g, '-')}">${x.version}</a></li>`).join('')}</ul></nav>`;
    ecrire(page.chemin[langue], gabarit({ langue, page, titre: T.nouveautes,
      description: langue === 'fr' ? 'Ce qui a changé dans chaque version de Nuée.' : 'What changed in each version of Nuée.',
      contenu: `${intro}\n${liste}\n<div class="nouveautes">\n${versionsHtml}\n</div>` }));
  }

  // the site's root: the French pages, or the English ones for a browser that prefers English
  fs.writeFileSync(path.join(SITE, 'index.html'), `<!doctype html>
<html lang="fr">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Nuée</title>
<link rel="icon" href="assets/nuee.svg" type="image/svg+xml">
<link rel="alternate" hreflang="fr" href="fr/index.html">
<link rel="alternate" hreflang="en" href="en/index.html">
<script>
  (function () {
    var l = (navigator.languages && navigator.languages.length ? navigator.languages : [navigator.language || 'fr']).map(function (x) { return String(x).slice(0, 2).toLowerCase(); });
    var fr = l.indexOf('fr'), en = l.indexOf('en');
    location.replace(fr === -1 && en !== -1 ? 'en/index.html' : 'fr/index.html');
  })();
</script>
<noscript><meta http-equiv="refresh" content="0; url=fr/index.html"></noscript>
<style>body { font: 16px/1.5 system-ui, sans-serif; margin: 2rem; }</style>
</head>
<body>
<p><a href="fr/index.html" hreflang="fr">Nuée en français</a> · <a href="en/index.html" hreflang="en" lang="en">Nuée in English</a></p>
</body>
</html>
`);
  ecrits.push('index.html');
  ecrits.push(...await greffons());
  return ecrits;
}

construire().then((ecrits) => console.log(`${ecrits.length} pages written in ${path.relative(RACINE, SITE)}`))
  .catch((e) => { console.error(e.message); process.exit(1); });
