// What Help shows, without the page (checked in Node: tests/essai_aide.mjs): the addresses of
// Nuée's websites, the sentence to cite it, and the full user guide read for the help panel
// (aide.js): its sections, their text in paragraphs and lists as the documentation site makes
// them (docs/site/construire.js), a search through all of them, and the section of what is in
// front.

// ------------------------------------------------------------ addresses

export const SITE = 'https://nuee.io';
export const DEPOT = 'https://github.com/4rtefakt/nuee';
export const PROBLEMES = `${DEPOT}/issues/new/choose`;
export const DOI = '10.5281/zenodo.24263014';  // (Zenodo's concept DOI: every version)
// The documentation site (GitHub Pages: .github/workflows/site.yml), its pages in each language as
// docs/site/construire.js names them
const DOCUMENTATION = 'https://4rtefakt.github.io/nuee';
const fr = (langue) => langue === 'fr';
export const adresseGuide = (langue) => `${DOCUMENTATION}/${fr(langue) ? 'fr' : 'en'}/guide/`;
export const adresseTutoriels = (langue) => `${DOCUMENTATION}/${fr(langue) ? 'fr/tutoriels' : 'en/tutorials'}/`;
const TUTORIELS = { suivi: { fr: 'tendances', en: 'trends' }, baci: { fr: 'baci', en: 'baci' } };
export const adresseTutoriel = (cle, langue) => `${adresseTutoriels(langue)}${TUTORIELS[cle][fr(langue) ? 'fr' : 'en']}.html`;

// How to cite Nuée: the sentence README gives, for the version running (its year, the version's
// own; the Methods text writes the same reference: moteur/R/methodes.R)
export function citation(version, date) {
  const annee = /^\d{4}/.test(String(date || '')) ? String(date).slice(0, 4) : String(new Date().getFullYear());
  return `Boutin, C. (${annee}). Nuée (version ${version}) [Computer software]. Zenodo. https://doi.org/${DOI}`;
}

// ------------------------------------------------------------ the guide's sections

// A title underlined with dashes, then its lines, as the site reads them (sectionsGuide); the
// guide's own title (underlined with =) is left out. Both guides have the same sections in the same
// order (the site's build refuses otherwise): a section's place is the same in each language.
export function sectionsGuide(texte) {
  const lignes = String(texte || '').replace(/\r\n?/g, '\n').split('\n');
  const sections = [];
  let s = null;
  for (let i = 0; i < lignes.length; i += 1) {
    const l = lignes[i];
    const sous = lignes[i + 1];
    if (sous !== undefined && /^[-=]{3,}$/.test(sous) && l.trim() && !/^\s/.test(l) && Math.abs([...sous].length - [...l].length) <= 3) {
      i += 1;
      if (sous[0] === '=') { s = null; continue; }
      s = { titre: l.trim(), lignes: [] };
      sections.push(s);
      continue;
    }
    if (s) s.lignes.push(l);
  }
  for (const x of sections) {
    while (x.lignes.length && !x.lignes[0].trim()) x.lignes.shift();
    while (x.lignes.length && !x.lignes[x.lignes.length - 1].trim()) x.lignes.pop();
  }
  return sections;
}

// The width the guide is wrapped at: what nearly all its lines stay within (the paragraph rule).
export function largeurGuide(sections) {
  const n = sections.flatMap((s) => s.lignes).filter((l) => l.trim()).map((l) => [...l].length).sort((a, b) => a - b);
  return n.length ? n[Math.floor(n.length * 0.97)] : 96;
}

// ------------------------------------------------------------ a section's text in blocks

const retrait = (t) => t.match(/^ */)[0].length;
const vide = (l) => !l.t.trim();

// A section's lines in blocks, as the site makes them: paragraphs ({ type: 'p', textes }), lists
// ('ul', 'ol': their elements, each its own blocks), definitions ('dl'), preformatted text ('pre'),
// indented text ('retrait'). largeur: the guide's (largeurGuide).
export function blocsSection(lignes, largeur) {
  return finir(blocs(lignes.map((t) => ({ t, n: [...t].length }))), largeur);
}

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

// "term   definition" lines, a definition going on in more indented lines; null when the lines are
// a table (several columns) rather than terms
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
  return { type: 'dl', elements: elements.map((e) => ({ terme: e.terme, def: e.def.join(' ') })) };
}

// A paragraph's lines: a line that ends a sentence, when the next line starts with a capital and its
// first word would have fitted on the line, ends a paragraph (the guide starts a new one on a new
// line, without a blank line).
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

// the paragraphs' lines joined into their texts, everywhere in the blocks
function finir(liste, largeur) {
  return liste.map((b) => {
    if (b.type === 'p') return { type: 'p', textes: paragraphes(b.lignes, largeur) };
    if (b.type === 'ul' || b.type === 'ol') return { type: b.type, elements: b.elements.map((e) => finir(e, largeur)) };
    if (b.type === 'groupe' || b.type === 'retrait') return { type: b.type, blocs: finir(b.blocs, largeur) };
    return b;
  });
}

// ------------------------------------------------------------ a paragraph's pieces

// A section is referred to by its title, or by its title without what follows "(" or ":"
export function alias(titre) {
  const a = new Set([titre, titre.replace(/ \(.*\)$/, ''), titre.replace(/ ?:.*$/, '')]);
  return [...a].filter((x) => x.length >= 4);
}

// "Résumé" and "resume" alike, for a search: each character of `texte` lowered and without its
// accents, and where it came from in the text (a match is then found in the text as written)
export const simple = (s) => String(s).normalize('NFD').replace(/\p{Diacritic}/gu, '').toLowerCase();
function indexer(texte) {
  let s = '';
  const origine = [];
  let i = 0;
  for (const c of texte) {
    const x = simple(c);
    for (let k = 0; k < x.length; k += 1) origine.push(i);
    s += x;
    i += c.length;
  }
  origine.push(i);
  return { s, origine };
}
// the words of a search, as compared (two letters at least)
export const motsRecherche = (requete) => [...new Set(simple(requete).split(/[\s,;]+/).filter((m) => m.length >= 2))];

// Where the words are in a text, as written: [[début, fin], ...], in order, without overlaps
export function occurrences(texte, mots) {
  if (!mots.length) return [];
  const { s, origine } = indexer(texte);
  const r = [];
  for (const m of mots) {
    for (let k = s.indexOf(m); k >= 0; k = s.indexOf(m, k + m.length)) r.push([origine[k], origine[k + m.length]]);
  }
  r.sort((a, b) => a[0] - b[0] || b[1] - a[1]);
  const res = [];
  for (const x of r) {
    const d = res[res.length - 1];
    if (d && x[0] <= d[1]) d[1] = Math.max(d[1], x[1]); else res.push([...x]);
  }
  return res;
}

// A paragraph in pieces, to draw: { texte, genre: 'texte' (plain), 'entree' (its lead-in, in bold),
// 'url' (a web address), 'renvoi' (a reference to another section: `section`, its number), marque
// (a word searched for) }. renvois: [[alias, section number], ...], the longest first; mots: those
// searched for (motsRecherche); entree: the lead-in is looked for ("Plots › Series (…):").
export function morceaux(texte, { renvois = [], mots = [], entree = true } = {}) {
  const speciaux = [];  // [début, fin, genre, cible]
  const libre = (a, b) => speciaux.every(([x, y]) => b <= x || a >= y);
  for (const m of texte.matchAll(/https?:\/\/[^\s<>"]*[^\s<>".,;:!?)»]/g)) speciaux.push([m.index, m.index + m[0].length, 'url', m[0]]);
  for (const [nom, cible] of renvois) {
    const motif = new RegExp(`(\\b(?:see|See|voir|Voir) +)(["«“] ?)?(${nom.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')})(?![\\p{L}\\d])`, 'gu');
    for (const m of texte.matchAll(motif)) {
      const debut = m.index + m[1].length + (m[2] || '').length;
      const fin = debut + m[3].length;
      if (libre(debut, fin)) speciaux.push([debut, fin, 'renvoi', cible]);
    }
  }
  speciaux.sort((a, b) => a[0] - b[0]);
  // "Random slopes: under each…", "Plots › Series (…): …": the lead-in, when it is a name
  let finEntree = 0;
  const m = entree ? texte.match(/^([^:]{1,60}?)( ?:)( |$)/) : null;
  if (m) {
    const e = m[1];
    const mots_ = e.trim().split(/\s+/).length;
    const parentheses = (e.match(/\(/g) || []).length === (e.match(/\)/g) || []).length;
    if (parentheses && !/[.;]\s/.test(e) && (e.includes('›') || mots_ <= 6) && libre(0, e.length + m[2].length)) finEntree = e.length + m[2].length;
  }
  const pieces = [];
  let k = 0;
  const ajouter = (a, b, genre, cible) => { if (b > a) pieces.push({ debut: a, fin: b, genre, cible }); };
  if (finEntree) { ajouter(0, finEntree, 'entree'); k = finEntree; }
  for (const [a, b, genre, cible] of speciaux) {
    if (a < k) continue;
    ajouter(k, a, 'texte');
    ajouter(a, b, genre, cible);
    k = b;
  }
  ajouter(k, texte.length, 'texte');
  // the words searched for, marked inside the pieces
  const marques = occurrences(texte, mots);
  const res = [];
  for (const p of pieces) {
    let a = p.debut;
    for (const [x, y] of marques) {
      if (y <= a || x >= p.fin) continue;
      if (x > a) res.push({ ...piece(texte, p, a, x), marque: false });
      res.push({ ...piece(texte, p, Math.max(a, x), Math.min(p.fin, y)), marque: true });
      a = Math.min(p.fin, y);
    }
    if (a < p.fin) res.push({ ...piece(texte, p, a, p.fin), marque: false });
  }
  return res;
}
const piece = (texte, p, a, b) => ({ texte: texte.slice(a, b), genre: p.genre, ...(p.genre === 'url' ? { url: p.cible } : p.genre === 'renvoi' ? { section: p.cible } : {}) });

// ------------------------------------------------------------ searching the guide

// The sections that hold every word of the search, those with them in their title first, then
// those that have them most: [{ section (number), nombre (how many times), extrait (the text around
// the first of them) }]
export function chercher(sections, requete) {
  const mots = motsRecherche(requete);
  if (!mots.length) return [];
  const res = [];
  sections.forEach((s, i) => {
    const titre = simple(s.titre);
    const texte = s.lignes.map((l) => l.trim()).join(' ');
    const corps = simple(texte);
    if (!mots.every((m) => titre.includes(m) || corps.includes(m))) return;
    const nombre = mots.reduce((n, m) => n + corps.split(m).length - 1, 0);
    const dansTitre = mots.filter((m) => titre.includes(m)).length;
    const [premiere] = occurrences(texte, mots);
    const extrait = premiere ? extraitAutour(texte, premiere) : texte.slice(0, 140);
    res.push({ section: i, nombre, dansTitre, extrait });
  });
  res.sort((a, b) => b.dansTitre - a.dansTitre || b.nombre - a.nombre || a.section - b.section);
  return res.map(({ dansTitre, ...r }) => r);
}

function extraitAutour(texte, [a, b]) {
  let debut = Math.max(0, a - 60);
  let fin = Math.min(texte.length, b + 90);
  if (debut > 0) debut = texte.indexOf(' ', debut) + 1 || debut;
  if (fin < texte.length) fin = texte.lastIndexOf(' ', fin) > b ? texte.lastIndexOf(' ', fin) : fin;
  return `${debut > 0 ? '…' : ''}${texte.slice(debut, fin)}${fin < texte.length ? '…' : ''}`;
}

// ------------------------------------------------------------ the section of what is in front

// The guide's section for a view (when it has its own) and for each type of model, by its title in
// each language (USER-GUIDE.txt, LISEZ-MOI-APPLICATION.txt; tests/essai_aide.mjs checks that each
// is a section of its guide, at the same place in both).
const S = {
  ouvrir: ['Ouvrir', 'Opening'],
  couleurs: ['Graphiques : couleurs et étiquettes', 'Plots: colours and labels'],
  autres: ['Autres graphiques', 'Other plots'],
  contributions: ['Contributions', 'Contributions'],
  predire: ['Prédire', 'Predicting'],
  spectres: ['Spectres', 'Spectra'],
  console: ['Console R', 'R console'],
  documents: ['Documents (articles, rapports)', 'Documents (articles, reports)'],
  figures: ['Figures pour publication', 'Figures for publication'],
  statistiques: ['Statistiques des variables', 'Variable statistics'],
  series: ['Séries', 'Series'],
  carte: ['Cartes et données spatiales', 'Maps and spatial data'],
  tests: ['Tests statistiques', 'Statistical tests'],
  regressions: ['Modèles linéaires, généralisés et mixtes', 'Linear, generalized and mixed models'],
  classes: ['Modèles de classe et graphique de Coomans', 'Class models and the Coomans plot'],
  rda: ['Analyse de redondance et analyse des correspondances', 'Redundancy analysis and correspondence analysis'],
  cca: ['Analyse canonique des correspondances (CCA)', 'Canonical correspondence analysis (CCA)'],
  indval: ['Espèces indicatrices', 'Indicator species'],
  rarefaction: ['Raréfaction et accumulation des espèces', 'Rarefaction and species accumulation'],
  distances: ['PCoA, NMDS, PERMANOVA et test de Mantel', 'PCoA, NMDS, PERMANOVA and Mantel test'],
  arbres: ['Arbres de décision, analyse discriminante linéaire et k-moyennes', 'Decision trees, linear discriminant analysis and k-means'],
  trim: ['TRIM : indices et tendances de comptages', 'TRIM: indices and trends of counts'],
  distance: ['Échantillonnage par distances : densité et effectif', 'Distance sampling: density and abundance'],
  forets: ['Forêts aléatoires et arbres boostés', 'Random forests and boosted trees'],
  code: ['Votre propre modèle en code R', 'Your own model in R code'],
  donnees: ['Modifier les données, créer des variables', 'Editing the data, making variables'],
  historique_usp: ['Historique du fichier', 'File history'],
  versions: ['Historique et versions (Fichier › Historique)', 'History and versions (File › History)'],
  changements: ['Ce qui a changé', 'What changed'],
  opls: ['OPLS et OPLS-DA', 'OPLS and OPLS-DA'],
  vip: ['VIP', 'VIP'],
  validation: ['Validation (PLS et PLS-DA)', 'Validation (PLS and PLS-DA)'],
};
export const SECTIONS = S;
const TYPES = {
  PCA: S.couleurs, PLS: S.validation, 'PLS-DA': S.validation, OPLS: S.opls, 'OPLS-DA': S.opls, CLASSES: S.classes,
  LM: S.regressions, LMM: S.regressions, GLM: S.regressions, GAM: S.regressions,
  RDA: S.rda, CA: S.rda, CCA: S.cca, INDVAL: S.indval, PCOA: S.distances, NMDS: S.distances, PERMANOVA: S.distances, MANTEL: S.distances,
  TREE: S.arbres, LDA: S.arbres, KMEANS: S.arbres, FOREST: S.forets, BOOST: S.forets, TRIM: S.trim, DISTANCE: S.distance, CODE: S.code,
};
const VUES = {
  vip: S.vip, contributions: S.contributions, biplot: S.autres, scores3d: S.autres, splot: S.autres, permutations: S.validation, cvanova: S.validation,
  coomans: S.classes, predictions: S.predire, ipls: S.spectres, profils: S.spectres, donnees: S.donnees, statistiques: S.statistiques, tests: S.tests,
  series: S.series, carte: S.carte, autocorrelation_spatiale: S.carte, rarefaction: S.rarefaction, document: S.documents, figure: S.figures,
  historique: S.historique_usp, versions: S.versions, changements: S.changements, console: S.console, comparaison: S.regressions,
  acf: S.regressions, selection_aic: S.regressions, puissance: S.regressions, moyennes: S.regressions,
};

// The section for what is in front, and where in it: { section (its number), phrase (the first of
// `phrases` its text has: a view's name with its menu, "Plots › AIC selection", then its name alone),
// or null }. contexte: { vue, type (the active model's), langue }; nothing in front: Opening.
export function sectionPour(contexte, sections, phrases = []) {
  const titres = (contexte.vue && VUES[contexte.vue]) || (contexte.type && TYPES[contexte.type]) || S.ouvrir;
  const titre = titres[fr(contexte.langue) ? 0 : 1];
  let section = sections.findIndex((s) => s.titre === titre);
  if (section < 0) section = 0;
  const texte = (sections[section] ? sections[section].lignes : []).map((l) => l.trim()).join(' ');
  const phrase = phrases.find((p) => p && texte.includes(p)) || null;
  return { section, phrase };
}
