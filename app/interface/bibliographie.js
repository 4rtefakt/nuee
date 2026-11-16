// The references a document cites ([@wold2001], @wold2001...): read from BibTeX (.bib) or CSL-JSON
// (.json) files, from Zotero, or kept in the document itself; then its citations and its list of
// references written in an author–year or numbered style, in the document's language. Pure
// functions, without the page (tests/essai_bibliographie.mjs). The citations follow Pandoc's
// syntax, so that Quarto compiles the same text with any CSL style.

// ------------------------------------------------------------ LaTeX in BibTeX fields

// Accents as combining characters (then composed: \'e -> é), special letters, symbols.
const COMBINANTS = { "'": '́', '`': '̀', '^': '̂', '"': '̈', '~': '̃', '=': '̄', '.': '̇',
  u: '̆', v: '̌', H: '̋', c: '̧', k: '̨', r: '̊', d: '̣', b: '̱' };
const LETTRES = { aa: 'å', AA: 'Å', ae: 'æ', AE: 'Æ', oe: 'œ', OE: 'Œ', o: 'ø', O: 'Ø', ss: 'ß', l: 'ł', L: 'Ł', i: 'ı', j: 'ȷ',
  textendash: '–', textemdash: '—', textquoteright: '’', textquoteleft: '‘', textquotedblleft: '“', textquotedblright: '”',
  textregistered: '®', texttrademark: '™', textdegree: '°', textbackslash: '\\', ldots: '…', dots: '…', textellipsis: '…',
  copyright: '©', textcopyright: '©', textsection: '§', textperiodcentered: '·', textmu: 'µ', textpm: '±', pm: '±', times: '×',
  alpha: 'α', beta: 'β', gamma: 'γ', delta: 'δ', epsilon: 'ε', lambda: 'λ', mu: 'μ', pi: 'π', rho: 'ρ', sigma: 'σ', tau: 'τ',
  chi: 'χ', omega: 'ω', Delta: 'Δ', Sigma: 'Σ', Omega: 'Ω', le: '≤', ge: '≥', neq: '≠', approx: '≈', infty: '∞', degree: '°' };
const ITALIQUES = new Set(['textit', 'emph', 'textsl', 'mathit', 'mkbibemph', 'mkbibitalic']);
const SANS_STYLE = new Set(['textbf', 'textsc', 'textrm', 'textsf', 'texttt', 'textup', 'textmd', 'textnormal', 'mathrm', 'mbox', 'text', 'url', 'NoCaseChange', 'MakeUppercase', 'mkbibbold']);
// Italic in a field's text: between these two characters (segments() makes runs of them).
export const DEBUT_ITALIQUE = '';
export const FIN_ITALIQUE = '';

const COMMANDE = /\\([a-zA-Z]+)\s*/y;

// A BibTeX field's text as plain text (italic marked by DEBUT_ITALIQUE ... FIN_ITALIQUE).
// `argument`: a command's argument, its spaces kept (\emph{ perdix}).
export function decoderLatex(brut, argument = false) {
  let s = String(brut ?? '');
  let sortie = '';
  let i = 0;
  const pile = [];  // for each { open: whether it holds italic (\textit{ or {\it ...})
  const lireArgument = () => {  // after a command: {x} or a single character
    while (s[i] === ' ') i += 1;
    if (s[i] === '{') {
      let profondeur = 0;
      const debut = i;
      for (; i < s.length; i += 1) {
        if (s[i] === '{') profondeur += 1;
        else if (s[i] === '}') { profondeur -= 1; if (!profondeur) { i += 1; break; } }
      }
      return s.slice(debut + 1, i - 1);
    }
    return s[i++] || '';
  };
  while (i < s.length) {
    const c = s[i];
    if (c === '\\') {
      const n = s[i + 1];
      // (an accent over a dotless i or j: over i or j)
      const sansPoint = (x) => x.replace(/[]/g, '').replace('ı', 'i').replace('ȷ', 'j');
      if (n && COMBINANTS[n] && !/[a-zA-Z]/.test(n)) {  // \'e \'{e} \"{o} \^{\i}
        i += 2;
        const lettre = sansPoint(decoderLatex(lireArgument()));
        sortie += (lettre || ' ') + COMBINANTS[n];
        continue;
      }
      COMMANDE.lastIndex = i;
      const m = COMMANDE.exec(s);
      if (m) {
        const nom = m[1];
        i += m[0].length;
        if (COMBINANTS[nom] && nom.length === 1) {  // \c{c} \v{s} \u{a} \H{o} \k{a} \r{a}
          const lettre = sansPoint(decoderLatex(lireArgument()));
          sortie += (lettre || ' ') + COMBINANTS[nom];
        } else if (LETTRES[nom] !== undefined) {
          sortie += LETTRES[nom];
          if (s[i] === '{' && s[i + 1] === '}') i += 2;  // \o{}
        } else if (ITALIQUES.has(nom)) {
          sortie += DEBUT_ITALIQUE + decoderLatex(lireArgument(), true) + FIN_ITALIQUE;
        } else if (nom === 'it' || nom === 'em' || nom === 'itshape' || nom === 'sl') {
          sortie += DEBUT_ITALIQUE;
          if (pile.length) pile[pile.length - 1] = true;
        } else if (SANS_STYLE.has(nom)) {
          sortie += decoderLatex(lireArgument(), true);
        } else if (nom === 'href') {
          lireArgument();
          sortie += decoderLatex(lireArgument(), true);
        }
        // (another command: left out, its argument read as text)
        continue;
      }
      // \& \% \$ \# \_ \{ \} \, \  \-
      if (n === ',' || n === ';' || n === ' ') sortie += n === ' ' ? ' ' : ' ';
      else if (n === '-') { /* a hyphenation point */ } else if (n) sortie += n;
      i += 2;
      continue;
    }
    if (c === '{') { pile.push(false); i += 1; continue; }
    if (c === '}') { if (pile.pop()) sortie += FIN_ITALIQUE; i += 1; continue; }
    if (c === '$') { i += 1; continue; }
    if (c === '~') { sortie += ' '; i += 1; continue; }
    if (c === '-' && s.startsWith('---', i)) { sortie += '—'; i += 3; continue; }
    if (c === '-' && s.startsWith('--', i)) { sortie += '–'; i += 2; continue; }
    if (c === '`' && s.startsWith('``', i)) { sortie += '“'; i += 2; continue; }
    if (c === "'" && s.startsWith("''", i)) { sortie += '”'; i += 2; continue; }
    sortie += c;
    i += 1;
  }
  while (pile.length) if (pile.pop()) sortie += FIN_ITALIQUE;
  if (argument) return sortie;
  return sortie.normalize('NFC').replace(/\s+/g, ' ').replace(//g, '').trim();
}

// ------------------------------------------------------------ BibTeX

const MOIS = { jan: '1', feb: '2', mar: '3', apr: '4', may: '5', jun: '6', jul: '7', aug: '8', sep: '9', oct: '10', nov: '11', dec: '12' };
// (sticky: read where the reader stands, without copying the rest of a large file)
const TYPE = /([a-zA-Z]+)\s*([{(])/y;
const MOT = /[^\s,#}")]+/y;
const NOM_MACRO = /[^\s=]+/y;
const CLE_ENTREE = /[^\s,]*/y;
const NOM_CHAMP = /[^\s=,{}()]+/y;

// The entries of a BibTeX (or BibLaTeX) text: [{ cle, type, champs (raw values, names in lower
// case), brut (the entry's text) }], and what couldn't be read.
export function lireBibtex(texte) {
  const s = String(texte ?? '').replace(/^﻿/, '');
  const macros = { ...MOIS };
  const entrees = [];
  const erreurs = [];
  let i = 0;
  const espaces = () => { while (i < s.length && /\s/.test(s[i])) i += 1; };
  // a value: {...} "..." number macro, joined by #
  const valeur = (fermeture) => {
    const parts = [];
    for (;;) {
      espaces();
      if (s[i] === '{') {
        let profondeur = 0;
        const debut = i;
        for (; i < s.length; i += 1) {
          if (s[i] === '\\') { i += 1; continue; }
          if (s[i] === '{') profondeur += 1;
          else if (s[i] === '}') { profondeur -= 1; if (!profondeur) break; }
        }
        if (i >= s.length) throw new Error('accolade non fermée');
        parts.push(s.slice(debut + 1, i));
        i += 1;
      } else if (s[i] === '"') {
        let profondeur = 0;
        const debut = i + 1;
        for (i += 1; i < s.length; i += 1) {
          if (s[i] === '\\') { i += 1; continue; }
          if (s[i] === '{') profondeur += 1;
          else if (s[i] === '}') profondeur -= 1;
          else if (s[i] === '"' && !profondeur) break;
        }
        if (i >= s.length) throw new Error('guillemet non fermé');
        parts.push(s.slice(debut, i));
        i += 1;
      } else {
        MOT.lastIndex = i;
        const m = MOT.exec(s);
        if (!m) throw new Error('valeur attendue');
        i += m[0].length;
        const nom = m[0];
        parts.push(/^\d+$/.test(nom) ? nom : (macros[nom.toLowerCase()] ?? nom));
      }
      espaces();
      if (s[i] === '#') { i += 1; continue; }
      if (s[i] === ',' || s[i] === fermeture) return parts.join('');
      throw new Error(`« , » attendue après une valeur (caractère ${i})`);
    }
  };
  while (i < s.length) {
    const a = s.indexOf('@', i);
    if (a < 0) break;
    i = a + 1;
    TYPE.lastIndex = i;
    const m = TYPE.exec(s);
    if (!m) continue;
    const type = m[1].toLowerCase();
    const fermeture = m[2] === '{' ? '}' : ')';
    i += m[0].length;
    const debutEntree = a;
    try {
      if (type === 'comment' || type === 'preamble') {
        let profondeur = 1;
        for (; i < s.length && profondeur; i += 1) {
          if (s[i] === m[2]) profondeur += 1;
          else if (s[i] === fermeture) profondeur -= 1;
        }
        continue;
      }
      if (type === 'string') {
        espaces();
        NOM_MACRO.lastIndex = i;
        const n = NOM_MACRO.exec(s);
        if (!n) throw new Error('nom de @string attendu');
        i += n[0].length;
        espaces();
        if (s[i] !== '=') throw new Error('« = » attendu');
        i += 1;
        macros[n[0].toLowerCase()] = valeur(fermeture);
        if (s[i] === ',') i += 1;
        espaces();
        if (s[i] === fermeture) i += 1;
        continue;
      }
      espaces();
      CLE_ENTREE.lastIndex = i;
      const k = CLE_ENTREE.exec(s);
      const cle = k[0];
      i += cle.length;
      espaces();
      const champs = {};
      if (s[i] === ',') i += 1;
      for (;;) {
        espaces();
        if (s[i] === fermeture) { i += 1; break; }
        if (i >= s.length) throw new Error('entrée non fermée');
        NOM_CHAMP.lastIndex = i;
        const n = NOM_CHAMP.exec(s);
        if (!n) throw new Error(`nom de champ attendu (caractère ${i})`);
        i += n[0].length;
        espaces();
        if (s[i] !== '=') throw new Error(`« = » attendu après ${n[0]}`);
        i += 1;
        champs[n[0].toLowerCase()] = valeur(fermeture);
        espaces();
        if (s[i] === ',') i += 1;
      }
      if (!cle) throw new Error('entrée sans clé');
      entrees.push({ cle, type, champs, brut: s.slice(debutEntree, i) });
    } catch (e) {
      erreurs.push(`@${type} (ligne ${s.slice(0, debutEntree).split('\n').length}) : ${e.message}`);
      // on to the next line that starts an entry (an unclosed brace has read on to the end)
      const suite = s.slice(debutEntree + 1).search(/\n\s*@/);
      i = suite < 0 ? s.length : debutEntree + 1 + suite + 1;
    }
  }
  return { entrees: entrees.map(normaliserBibtex), erreurs };
}

// The names of a BibTeX field (author, editor): [{ nom, prenom, particule, suffixe } | { litteral }],
// and { autres: true } for "and others".
export function lireNoms(brut) {
  const noms = decouper(String(brut ?? ''), /\s+and\s+/i);
  return noms.map((n) => n.trim()).filter(Boolean).map((n) => {
    if (n.toLowerCase() === 'others') return { autres: true };
    if (/^\{[\s\S]*\}$/.test(n) && profondeurNulle(n.slice(1, -1))) return { litteral: decoderLatex(n.slice(1, -1)) };
    const parts = decouper(n, /,/).map((x) => x.trim());
    let prenom = '';
    let reste = parts[0];
    let suffixe = '';
    if (parts.length >= 3) { suffixe = parts[1]; prenom = parts.slice(2).join(', '); } else if (parts.length === 2) prenom = parts[1];
    let particule = '';
    let nom = reste;
    const mots = decouper(reste, /\s+/).filter(Boolean);
    if (parts.length === 1) {  // "First von Last"
      const iVon = mots.findIndex((w, k) => k < mots.length - 1 && /^[a-z]/.test(w.replace(/^\{/, '')));
      if (iVon >= 0) {
        let fin = iVon;
        while (fin + 1 < mots.length - 1 && /^[a-z]/.test(mots[fin + 1])) fin += 1;
        prenom = mots.slice(0, iVon).join(' ');
        particule = mots.slice(iVon, fin + 1).join(' ');
        nom = mots.slice(fin + 1).join(' ');
      } else {
        nom = mots[mots.length - 1] || '';
        prenom = mots.slice(0, -1).join(' ');
      }
    } else {  // "von Last, First"
      let k = 0;
      while (k < mots.length - 1 && /^[a-z]/.test(mots[k])) k += 1;
      particule = mots.slice(0, k).join(' ');
      nom = mots.slice(k).join(' ');
    }
    return { nom: decoderLatex(nom), prenom: decoderLatex(prenom), particule: decoderLatex(particule), suffixe: decoderLatex(suffixe) };
  });
}

// `s` split on `motif` outside braces.
function decouper(s, motif) {
  const re = new RegExp(motif.source, `${motif.flags.replace(/[gy]/g, '')}y`);
  const morceaux = [];
  let profondeur = 0;
  let debut = 0;
  for (let i = 0; i < s.length; i += 1) {
    if (s[i] === '{') profondeur += 1;
    else if (s[i] === '}') profondeur -= 1;
    else if (!profondeur) {
      re.lastIndex = i;
      const r = re.exec(s);
      if (r && r[0].length) { morceaux.push(s.slice(debut, i)); i += r[0].length - 1; debut = i + 1; }
    }
  }
  morceaux.push(s.slice(debut));
  return morceaux;
}
const profondeurNulle = (s) => { let p = 0; for (const c of s) { if (c === '{') p += 1; else if (c === '}') { p -= 1; if (p < 0) return false; } } return p === 0; };

const TYPES_THESE = { phdthesis: 'these', mastersthesis: 'memoire', thesis: 'these' };

// A BibTeX entry with its fields as text (the form the styles below read). `zotero`: where a
// reference added from Zotero is there (users/0/items/<key>, groups/<id>/items/<key>), which the
// document keeps with it (versBibtex).
function normaliserBibtex(e) {
  const c = e.champs;
  const t = (n) => (c[n] !== undefined ? decoderLatex(c[n]) : '');
  const date = t('date');
  const annee = (/\d{4}/.exec(t('year')) || /\d{4}/.exec(date) || [''])[0] || t('year');
  return {
    cle: e.cle, type: e.type, source: 'bibtex', brut: e.brut, champsBruts: c, zotero: elementZotero(c.zotero),
    auteurs: c.author ? lireNoms(c.author) : [], editeurs: c.editor ? lireNoms(c.editor) : [],
    annee, titre: t('title'), revue: t('journal') || t('journaltitle'), livre: t('booktitle'),
    volume: t('volume'), numero: t('number') || t('issue'), pages: t('pages'), editeur: t('publisher'), lieu: t('address') || t('location'),
    edition: t('edition'), institution: t('institution') || t('organization'), ecole: t('school') || t('institution'),
    doi: t('doi').replace(/^https?:\/\/(dx\.)?doi\.org\//i, ''), url: t('url'), note: t('note'), publication: t('howpublished'),
    rapport: t('type'), serie: t('series'), consulte: t('urldate'),
    these: TYPES_THESE[e.type] || (e.type === 'thesis' ? 'these' : ''),
  };
}

// ------------------------------------------------------------ CSL-JSON

const TYPES_CSL = { 'article-journal': 'article', article: 'article', 'article-magazine': 'article', 'article-newspaper': 'article', review: 'article',
  book: 'book', chapter: 'incollection', 'entry-encyclopedia': 'incollection', 'entry-dictionary': 'incollection', 'paper-conference': 'inproceedings',
  report: 'report', thesis: 'thesis', webpage: 'online', 'post-weblog': 'online', post: 'online', dataset: 'misc', software: 'software', manuscript: 'unpublished' };

// The entries of a CSL-JSON text (an array of items, as Zotero, Mendeley or Better BibTeX export).
export function lireCslJson(texte) {
  let items;
  try { items = JSON.parse(String(texte ?? '').replace(/^﻿/, '')); } catch (e) { return { entrees: [], erreurs: [`JSON : ${e.message}`] }; }
  if (!Array.isArray(items)) items = items && Array.isArray(items.items) ? items.items : [items];
  // (CSL-JSON's rich text: <i> as italic, the other tags left out, <span class="nocase"> among them)
  const riche = (v) => String(v ?? '').replace(/<(\/?)(?:i|em)>/gi, (_, fin) => (fin ? FIN_ITALIQUE : DEBUT_ITALIQUE))
    .replace(/<\/?(?:b|strong|sup|sub|sc|span)(?:\s[^<>]*)?>/gi, '');
  const noms = (l) => [].concat(l || []).map((n) => (n.literal ? { litteral: n.literal }
    : { nom: n.family || '', prenom: n.given || '', particule: [n['dropping-particle'], n['non-dropping-particle']].filter(Boolean).join(' '), suffixe: n.suffix || '' }));
  const entrees = items.filter((x) => x && (x['citation-key'] || x.id)).map((x) => {
    const type = TYPES_CSL[x.type] || 'misc';
    const d = x.issued || {};
    const annee = String((d['date-parts'] && d['date-parts'][0] && d['date-parts'][0][0]) || (/\d{4}/.exec(d.literal || d.raw || '') || [''])[0] || '');
    const conteneur = riche(x['container-title']);
    return {
      cle: String(x['citation-key'] || x.id), type, source: 'csl', brut: null, csl: x,
      auteurs: noms(x.author), editeurs: noms(x.editor), annee, titre: riche(x.title),
      revue: type === 'article' ? conteneur : '', livre: type === 'article' ? '' : conteneur,
      volume: String(x.volume ?? ''), numero: String(x.issue ?? x.number ?? ''), pages: String(x.page ?? '').replace(/-/g, '–'),
      editeur: x.publisher || '', lieu: x['publisher-place'] || '', edition: String(x.edition ?? ''), institution: x.publisher || '',
      ecole: x.publisher || '', doi: String(x.DOI || '').replace(/^https?:\/\/(dx\.)?doi\.org\//i, ''), url: x.URL || '', note: x.note || '',
      publication: '', rapport: x.genre || '', serie: riche(x['collection-title']), consulte: '',
      these: type === 'thesis' ? (/master/i.test(x.genre || '') ? 'memoire' : 'these') : '',
      langue: String(x.language || ''),
    };
  });
  return { entrees, erreurs: [] };
}

// ------------------------------------------------------------ writing BibTeX

const echapperBibtex = (v) => String(v ?? '').replace(/[&%$#_]/g, (c) => `\\${c}`)
  .split(DEBUT_ITALIQUE).join('\\textit{').split(FIN_ITALIQUE).join('}');
const nomBibtex = (n) => (n.litteral ? `{${n.litteral}}` : [[n.particule, n.nom].filter(Boolean).join(' '), n.suffixe, n.prenom].filter(Boolean).join(', '));

// The fields a document doesn't keep of a reference (a file's path, an abstract...); nor, in the
// .bib written beside a Quarto file, where it is in Zotero.
const SANS = ['file', 'abstract', 'keywords', 'annote', 'annotation', 'mendeley-tags'];
export const SANS_ZOTERO = [...SANS, 'zotero'];

// An entry as BibTeX: its own text when it came from BibTeX (without `sauf`'s fields), else written
// from its fields; where it is in Zotero (`zotero`) as a field of that name. Its language as langid,
// which Pandoc reads (Zotero writes language, which it doesn't): a French title in an English
// document is then left as it is, where a style puts English titles in title case.
export function versBibtex(e, sauf = SANS) {
  const zotero = e.zotero && !sauf.includes('zotero') ? [['zotero', e.zotero]] : [];
  if (e.champsBruts) {
    const champs = Object.entries(e.champsBruts).filter(([k]) => !sauf.includes(k) && k !== 'zotero');
    if (e.champsBruts.language && !e.champsBruts.langid && !e.champsBruts.hyphenation) champs.push(['langid', e.champsBruts.language]);
    champs.push(...zotero);
    return `@${e.type}{${e.cle},\n${champs.map(([k, v]) => `  ${k} = {${v}}`).join(',\n')}\n}`;
  }
  const type = { online: 'misc', software: 'misc', unpublished: 'unpublished', thesis: e.these === 'memoire' ? 'mastersthesis' : 'phdthesis' }[e.type] || e.type;
  const champs = [
    ['author', e.auteurs.filter((n) => !n.autres).map(nomBibtex).join(' and ')], ['editor', e.editeurs.filter((n) => !n.autres).map(nomBibtex).join(' and ')],
    ['title', echapperBibtex(e.titre)], ['journal', echapperBibtex(e.revue)], ['booktitle', echapperBibtex(e.livre)], ['year', e.annee],
    ['volume', e.volume], ['number', e.numero], ['pages', String(e.pages || '').replace(/–/g, '--')], ['publisher', echapperBibtex(e.editeur)],
    ['address', echapperBibtex(e.lieu)], ['edition', e.edition], ['school', e.type === 'thesis' ? echapperBibtex(e.ecole) : ''],
    ['institution', e.type === 'report' ? echapperBibtex(e.institution) : ''], ['type', echapperBibtex(e.rapport)], ['doi', e.doi], ['url', e.url],
    ['note', echapperBibtex(e.note)], ['langid', echapperBibtex(e.langue)], ...zotero,
  ].filter(([, v]) => v);
  return `@${type}{${e.cle},\n${champs.map(([k, v]) => `  ${k} = {${v}}`).join(',\n')}\n}`;
}

// The entries of a text, BibTeX or CSL-JSON (by its look, or its file's extension).
export function lireBibliographie(texte, nomFichier = '') {
  const json = /\.json$/i.test(nomFichier) || /^\s*[[{]/.test(String(texte ?? '').replace(/^﻿/, ''));
  return json ? lireCslJson(texte) : lireBibtex(texte);
}

// ------------------------------------------------------------ Zotero's libraries

// Where an item is in Zotero, from its URI (http://zotero.org/users/local/<id>/items/<key>,
// .../users/<id>/items/<key>, .../groups/<id>/items/<key>) or as kept: users/0/items/<key> for
// My Library, groups/<id>/items/<key> for a group's ('' when it says nowhere).
export function elementZotero(adresse) {
  const m = /(?:^|\/)(?:users\/(?:local\/)?[^/\s]+|groups\/(\d+))\/items\/([A-Z0-9]{8})$/.exec(String(adresse ?? '').trim());
  return !m ? '' : m[1] ? `groups/${m[1]}/items/${m[2]}` : `users/0/items/${m[2]}`;
}

// The references of Zotero's local API answering in JSON with each item's BibTeX (format=json&
// include=bibtex): read from that BibTeX, each with where it is in Zotero (its library and key).
export function lireReponseZotero(texte) {
  let items;
  try { items = JSON.parse(String(texte ?? '')); } catch { return []; }
  return [].concat(items || []).flatMap((x) => {
    const e = x && typeof x.bibtex === 'string' ? lireBibtex(x.bibtex).entrees[0] : null;
    if (!e) return [];
    const b = x.library || {};
    return [{ ...e, zotero: elementZotero(`${b.type === 'group' ? `groups/${b.id}` : 'users/0'}/items/${x.key}`) }];
  });
}

// Whether two entries are the same work: the same DOI when both have one, else the same title,
// year and first author (accents, case and punctuation aside). A reference copied from one Zotero
// library to another is the same work.
export function memeOeuvre(a, b) {
  const doi = (e) => String(e.doi || '').trim().toLowerCase();
  if (doi(a) && doi(b)) return doi(a) === doi(b);
  const titre = (e) => plier(e.titre).replace(/[^\p{L}\p{N}]+/gu, ' ').trim();
  const auteur = (e) => { const n = (e.auteurs || [])[0] || (e.editeurs || [])[0]; return n ? plier(n.litteral || n.nom) : ''; };
  return !!titre(a) && titre(a) === titre(b) && String(a.annee || '') === String(b.annee || '') && auteur(a) === auteur(b);
}

// ------------------------------------------------------------ the references of a Methods text

// A reference as the engine writes those of a Methods text (moteur/R/methodes.R and each analysis's
// texte_methodes_<type>), in APA style: an article "Wold, S., Sjöström, M., & Eriksson, L. (2001).
// Title. Journal, 58(2), 109–130. https://doi.org/…", a book "… (2nd ed.). Springer. https://…",
// software "R Core Team (2026). R: …. R Foundation for Statistical Computing, Vienna, Austria.
// https://www.R-project.org/". Read as an entry (lireBibtex's form), so that it takes its place in
// the document's list, in the document's style; null when it doesn't read as one.
export function lireReferenceTexte(texte) {
  const s = String(texte ?? '').trim();
  const m = /^(.+?)\s*\((\d{4})[a-z]?\)\.\s+([\s\S]+)$/.exec(s);
  if (!m) return null;
  const [, tete, annee, suite] = m;
  // the authors: "Name, I. I." pairs, a group's name alone ("R Core Team"), "et al."
  const morceaux = tete.replace(/,?\s+&\s+/g, ', ').split(/,\s*/).map((x) => x.trim()).filter(Boolean);
  const auteurs = [];
  for (let i = 0; i < morceaux.length; i += 1) {
    const x = morceaux[i];
    if (/^et al\.?$/i.test(x)) { auteurs.push({ autres: true }); continue; }
    if (i + 1 < morceaux.length && /^(\p{Lu}\.(-\p{Lu}\.)?\s*)+$/u.test(morceaux[i + 1])) {
      const mots = x.split(/\s+/);
      let k = 0;
      while (k < mots.length - 1 && /^\p{Ll}/u.test(mots[k])) k += 1;
      auteurs.push({ nom: mots.slice(k).join(' '), prenom: morceaux[i + 1], particule: mots.slice(0, k).join(' '), suffixe: '' });
      i += 1;
    } else auteurs.push({ litteral: x });
  }
  // the title: up to its period (or ? or !) before a space, outside brackets
  let profondeur = 0;
  let fin = suite.length;
  for (let i = 0; i < suite.length; i += 1) {
    if ('([{'.includes(suite[i])) profondeur += 1;
    else if (')]}'.includes(suite[i])) profondeur = Math.max(0, profondeur - 1);
    else if (!profondeur && /[.?!]/.test(suite[i]) && (i + 1 === suite.length || /\s/.test(suite[i + 1]))) { fin = i; break; }
  }
  let titre = suite.slice(0, suite[fin] === '.' ? fin : fin + 1).trim();
  let reste = suite.slice(fin + 1).trim();
  let doi = '';
  let url = '';
  const lien = /(^|\s)(https?:\/\/\S+)$/.exec(reste);
  if (lien) {
    reste = reste.slice(0, lien.index).trim();
    const d = /^https?:\/\/(?:dx\.)?doi\.org\/(.+)$/i.exec(lien[2]);
    if (d) doi = d[1]; else url = lien[2];
  }
  let edition = '';
  const ed = /\s*\((\d+)(?:st|nd|rd|th) ed\.\)$/.exec(titre);
  if (ed) { edition = ed[1]; titre = titre.slice(0, ed.index); }
  const e = { cle: '', type: 'misc', source: 'methodes', texte: s, auteurs, editeurs: [], annee, titre, revue: '', livre: '', volume: '', numero: '', pages: '',
    editeur: '', lieu: '', edition, institution: '', ecole: '', doi, url, note: '', publication: '', rapport: '', serie: '', consulte: '', these: '' };
  // a journal ("Journal, 58(2), 109–130."), else a publisher (a book; software or a manuscript: misc)
  const article = /^([^.]+?),\s*(\d+)(?:\(([^)]+)\))?(?:,\s*([^,]+?))?\.$/.exec(reste);
  if (article) Object.assign(e, { type: 'article', revue: article[1], volume: article[2], numero: article[3] || '', pages: article[4] || '' });
  else if (reste) Object.assign(e, { type: /\[Computer software\]|R package|Unpublished/i.test(`${titre} ${reste}`) ? 'misc' : 'book', editeur: reste.replace(/\.$/, '') });
  return e;
}

// Whether a reference of a Methods text and one of the document are the same work: the same DOI
// when both have one, else the same title and year (accents, case and punctuation aside; one title
// may be the other's beginning, of three words at least: "Canonical community ordination" and its
// ". Part I: …").
export function memeReference(a, b) {
  const doi = (e) => String(e.doi || '').trim().toLowerCase();
  if (doi(a) && doi(b)) return doi(a) === doi(b);
  const titre = (e) => plier(String(e.titre || '').replace(/[]/g, '')).replace(/[^\p{L}\p{N}]+/gu, ' ').trim();
  const [ta, tb] = [titre(a), titre(b)];
  if (!ta || !tb || String(a.annee || '').slice(0, 4) !== String(b.annee || '').slice(0, 4)) return false;
  const [court, long] = ta.length <= tb.length ? [ta, tb] : [tb, ta];
  return court === long || (court.split(' ').length >= 3 && long.startsWith(`${court} `));
}

// The references of the Methods texts a document cites (their texts), joined to the document's own
// (`entrees`, Map key -> entry): each one the document's entry for the same work when it has one
// (memeReference: its key, its form), else read from its text under a key of its own (nuee:wold2001).
// Returns { entrees: the Map with them, ajoutees: their keys, in order, once each }: the list of
// references holds them, cited or not (formaterCitations), one list.
export function joindreReferences(entrees, textes) {
  const toutes = new Map(entrees);
  const ajoutees = [];
  for (const texte of textes) {
    const e = lireReferenceTexte(texte);
    if (!e) continue;
    const meme = [...toutes.values()].find((x) => memeReference(x, e));
    let cle = meme && meme.cle;
    if (!cle) {
      const n = e.auteurs.find((x) => !x.autres);
      const base = `nuee:${plier(n ? n.litteral || n.nom : e.titre).replace(/[^a-z0-9]+/g, '')}${e.annee}`;
      cle = base;
      for (let k = 0; toutes.has(cle); k += 1) cle = `${base}${String.fromCharCode(97 + (k % 26))}${k >= 26 ? Math.floor(k / 26) : ''}`;
      toutes.set(cle, { ...e, cle });
    }
    if (!ajoutees.includes(cle)) ajoutees.push(cle);
  }
  return { entrees: toutes, ajoutees };
}

// Where a reference added from Zotero goes among the document's (`entrees`: Map key -> entry, those
// of its files with their `origine`): { cle: the key it is cited under, nouvelle: added under that
// key, remplace: the one kept there replaced by it (the same Zotero item, up to date), renommee: the
// key it came with, when another work has it already, autre: the key it came with, when the
// document has it under another }. The same Zotero item, or else the same work (a copy in another
// library, Better BibTeX's key and Zotero's own for one item), is cited under the key the document
// has for it; another work whose key is taken gets that key with a letter (bechet2019a, b...).
export function placerReference(e, entrees) {
  const toutes = [...entrees.values()];
  const meme = entrees.has(e.cle) && memeOeuvre(entrees.get(e.cle), e) ? entrees.get(e.cle) : null;
  const x = (e.zotero && toutes.find((y) => y.zotero === e.zotero)) || meme || toutes.find((y) => memeOeuvre(y, e));
  if (x) return { cle: x.cle, remplace: !x.origine && (!x.zotero || x.zotero === e.zotero), autre: x.cle !== e.cle ? e.cle : null };
  if (!entrees.has(e.cle)) return { cle: e.cle, nouvelle: true };
  const lettres = (n) => (n < 26 ? String.fromCharCode(97 + n) : lettres(Math.floor(n / 26) - 1) + String.fromCharCode(97 + (n % 26)));
  let n = 0;
  while (entrees.has(e.cle + lettres(n))) n += 1;
  return { cle: e.cle + lettres(n), nouvelle: true, renommee: e.cle };
}

// ------------------------------------------------------------ citations ([@a; @b], @a)

// A citation key, Pandoc's way: letters, digits and _, with internal punctuation (:.#$%&-+?<>~/),
// or anything between braces (@{a key}).
const CLE = '\\{[^{}]+\\}|[\\p{L}\\p{N}_](?:[\\p{L}\\p{N}_]|[:.#$%&+?<>~/-](?=[\\p{L}\\p{N}_]))*';
export const MOTIF_CLE = new RegExp(`^@(${CLE})`, 'u');
export const cleSansAccolades = (k) => (k.startsWith('{') ? k.slice(1, -1) : k);

// The citations in a bracket's text ("see @a, p. 3; @b"): [{ cle, prefixe, suffixe, sansAuteur }],
// or null when it isn't a citation (every part must cite).
export function lireGroupeCitations(interieur) {
  const parts = String(interieur).split(';');
  const cites = [];
  const motif = new RegExp(`^([\\s\\S]*?)(-?)@(${CLE})([\\s\\S]*)$`, 'u');
  for (const p of parts) {
    const m = motif.exec(p.trim());
    if (!m) return null;
    const prefixe = m[1];
    if (prefixe && !/\s$/.test(prefixe)) return null;  // (an address, a@b)
    cites.push({ cle: cleSansAccolades(m[3]), prefixe: prefixe.trim(), suffixe: m[4].trim(), sansAuteur: m[2] === '-' });
  }
  return cites.length ? cites : null;
}

// ------------------------------------------------------------ styles

const TEXTES = {
  fr: { et: 'et', etParenthese: 'et', etal: 'et al.', sd: 's. d.', dans: 'Dans', dir: 'dir.', p: 'p.', these: 'Thèse de doctorat',
    memoire: 'Mémoire de master', rapport: 'Rapport', introuvable: 'introuvable dans la bibliographie du document', edition: (n) => (/^\d+$/.test(n) ? `${n}${n === '1' ? 're' : 'e'} éd.` : n), consulte: 'consulté le' },
  en: { et: 'and', etParenthese: '&', etal: 'et al.', sd: 'n.d.', dans: 'In', dir: 'Ed.', dirs: 'Eds.', p: 'pp.', these: 'PhD thesis',
    memoire: 'Master’s thesis', rapport: 'Report', introuvable: 'not in the document’s bibliography', edition: (n) => (/^\d+$/.test(n) ? `${n}${({ 1: 'st', 2: 'nd', 3: 'rd' })[n % 100 > 10 && n % 100 < 14 ? 0 : n % 10] || 'th'} ed.` : n), consulte: 'accessed' },
};
export const STYLES = ['auteur-annee', 'numerique'];

const nomFamille = (n) => (n.litteral || [n.particule && /^[a-z]/.test(n.particule) ? '' : n.particule, n.nom].filter(Boolean).join(' '));
const nomTri = (n) => (n.litteral || [n.nom, n.particule].filter(Boolean).join(' '));
const initiales = (prenom) => String(prenom || '').split(/\s+/).filter(Boolean)
  .map((p) => p.split('-').map((x) => (/^\p{L}\.?$/u.test(x) ? x.replace(/\.?$/, '.') : `${[...x][0]}.`)).join('-')).join(' ');

// "Wold", "Wold et Eriksson", "Wold et al.": the authors of a citation.
function auteursCourts(e, langue, parenthese) {
  const t = TEXTES[langue] || TEXTES.fr;
  const noms = (e.auteurs.length ? e.auteurs : e.editeurs).filter((n) => !n.autres);
  const autres = (e.auteurs.length ? e.auteurs : e.editeurs).some((n) => n.autres);
  if (!noms.length) return e.titre ? `« ${e.titre.replace(/[]/g, '')} »`.replace(/^« (.{40}).+ »$/u, '« $1… »') : e.cle;
  if (noms.length >= 3 || autres) return `${nomFamille(noms[0])} ${t.etal}`;
  if (noms.length === 2) return `${nomFamille(noms[0])} ${parenthese ? t.etParenthese : t.et} ${nomFamille(noms[1])}`;
  return nomFamille(noms[0]);
}

// "Wold, S., Sjöström, M. et Eriksson, L." for the list of references.
function listeAuteurs(noms, langue) {
  const t = TEXTES[langue] || TEXTES.fr;
  const autres = noms.some((n) => n.autres);
  const l = noms.filter((n) => !n.autres).map((n) => (n.litteral ? n.litteral
    : `${[n.particule, n.nom].filter(Boolean).join(' ')}${n.prenom ? `, ${initiales(n.prenom)}` : ''}${n.suffixe ? `, ${n.suffixe}` : ''}`));
  if (autres) l.push(t.etal);
  if (l.length <= 1) return l.join('');
  if (autres) return l.join(', ');
  return `${l.slice(0, -1).join(', ')}${langue === 'en' ? ',' : ''} ${langue === 'en' ? '&' : t.et} ${l[l.length - 1]}`;
}
// "S. Wold et L. Eriksson" (the editors of a book, in a chapter's reference).
function listeEditeurs(noms, langue) {
  const t = TEXTES[langue] || TEXTES.fr;
  const l = noms.filter((n) => !n.autres).map((n) => (n.litteral || [initiales(n.prenom), n.particule, n.nom].filter(Boolean).join(' ')));
  if (l.length <= 1) return l.join('');
  return `${l.slice(0, -1).join(', ')} ${langue === 'en' ? '&' : t.et} ${l[l.length - 1]}`;
}

// Text with italic marks (DEBUT_ITALIQUE / FIN_ITALIQUE) as segments; `italique`: the whole text
// in italic (a book's title), its own italic parts then upright.
export function segments(texte, italique = false) {
  const r = [];
  let it = italique;
  for (const morceau of String(texte ?? '').split(/([])/)) {
    if (morceau === DEBUT_ITALIQUE) { it = !italique; continue; }
    if (morceau === FIN_ITALIQUE) { it = italique; continue; }
    if (morceau) r.push({ texte: morceau, italique: it });
  }
  return r;
}
export const texteSeul = (segs) => segs.map((x) => x.texte).join('');
const point = (texte) => (/[.?!…]$/.test(String(texte).replace(/[]/g, '').trim()) ? '' : '.');

// An entry of the list of references, as segments ({ texte, italique, lien }).
function referenceEnSegments(e, langue, annee) {
  const t = TEXTES[langue] || TEXTES.fr;
  const s = [];
  const txt = (x) => { if (x) s.push(...segments(x)); };
  const ita = (x) => { if (x) s.push(...segments(x, true)); };
  const auteurs = e.auteurs.length ? listeAuteurs(e.auteurs, langue) : '';
  const parEditeurs = !auteurs && e.editeurs.length && ['book', 'collection', 'proceedings'].includes(e.type);
  const tete = auteurs || (parEditeurs ? `${listeAuteurs(e.editeurs, langue)} (${e.editeurs.length > 1 ? (t.dirs || t.dir) : t.dir})` : '');
  const titreEnTete = !tete;
  if (tete) txt(`${tete} `);
  else if (e.titre) { (['article', 'incollection', 'inproceedings'].includes(e.type) ? txt : ita)(e.titre); txt(`${point(e.titre)} `); }
  txt(`(${annee}). `);
  const lien = e.doi ? `https://doi.org/${e.doi}` : e.url;
  const pages = String(e.pages || '').replace(/--/g, '–').replace(/(\d)-(\d)/g, '$1–$2');
  const titre = (italique) => { if (titreEnTete || !e.titre) return; (italique ? ita : txt)(e.titre); };
  // (the end of a part: a period, unless what it ends with has one, as "J. Chem. Ecol.")
  const phrase = () => { if (s.length) txt(`${point(texteSeul(s).replace(/\s+$/, ''))} `); };
  const partie = (x) => { if (x) { txt(x); phrase(); } };
  switch (e.type) {
    case 'article':
      titre(false);
      if (e.titre && !titreEnTete) phrase();
      if (e.revue) {
        ita(e.revue);
        if (e.volume) { txt(', '); ita(e.volume); }
        if (e.numero) txt(`(${e.numero})`);
        if (pages) txt(`, ${pages}`);
        phrase();
      }
      break;
    case 'incollection': case 'inbook': case 'inproceedings': case 'conference': {
      titre(false);
      if (e.titre && !titreEnTete) phrase();
      const eds = e.editeurs.length ? `${listeEditeurs(e.editeurs, langue)} (${e.editeurs.length > 1 ? (t.dirs || t.dir) : t.dir}), ` : '';
      if (e.livre) {
        txt(`${t.dans} ${eds}`);
        ita(e.livre);
        if (pages) txt(` (${t.p} ${pages})`);
        phrase();
      } else if (pages) partie(`${t.p} ${pages}`);
      partie(e.editeur);
      break;
    }
    case 'thesis': case 'phdthesis': case 'mastersthesis': {
      titre(true);
      // (Zotero's types in English, "PhD Thesis", "Master's Thesis": in the document's language)
      const genre = /^ph\.?\s?d\.?\s*thesis$|^doctoral\s+thesis$/i.test(e.rapport) ? t.these : /^master'?s?\s*thesis$/i.test(e.rapport) ? t.memoire : e.rapport;
      txt(` [${genre || (e.these === 'memoire' ? t.memoire : t.these)}]`);
      phrase();
      partie(e.ecole);
      break;
    }
    case 'report': case 'techreport':
      titre(true);
      if (e.numero) txt(` (${e.rapport || t.rapport} ${e.numero})`);
      phrase();
      partie(e.institution);
      break;
    case 'book': case 'collection': case 'proceedings': case 'manual':
      titre(true);
      if (e.edition) txt(` (${t.edition(e.edition)})`);
      if (e.titre && !titreEnTete) phrase();
      partie(e.editeur || e.institution);
      break;
    default:
      titre(true);
      if (e.titre && !titreEnTete) phrase();
      partie(e.publication || e.editeur || e.institution);
  }
  if (lien) s.push({ texte: lien, lien });
  // (merged runs, the final space dropped)
  const fusion = [];
  for (const x of s) {
    const d = fusion[fusion.length - 1];
    if (d && !d.lien && !x.lien && d.italique === x.italique) d.texte += x.texte;
    else fusion.push({ ...x });
  }
  if (fusion.length) fusion[fusion.length - 1].texte = fusion[fusion.length - 1].texte.replace(/\s+$/, '');
  return fusion;
}

// The document's citations written in a style: `groupes` [{ cites: [{ cle, prefixe, suffixe,
// sansAuteur }], narratif }] in the document's order, `entrees` Map(key -> entry). Returns
// { textes: [the text of each group], titres: [its references in full, for a tooltip],
// liste: [{ cle, segments, numero }] (the references cited, in the list's order),
// inconnues: [the keys not found], nonCitees: [the keys of `ajoutees` the text doesn't cite] }.
// `ajoutees`: keys listed though the text doesn't cite them (the references of the Methods texts
// it cites: joindreReferences), after those it cites when numbered.
export function formaterCitations(groupes, entrees, { style = 'auteur-annee', langue = 'fr', ajoutees = [] } = {}) {
  const t = TEXTES[langue] || TEXTES.fr;
  const cites = [];  // the keys cited, in order
  for (const g of groupes) for (const c of g.cites) if (entrees.has(c.cle) && !cites.includes(c.cle)) cites.push(c.cle);
  const nonCitees = ajoutees.filter((k, i) => entrees.has(k) && !cites.includes(k) && ajoutees.indexOf(k) === i);
  cites.push(...nonCitees);
  const inconnues = [...new Set(groupes.flatMap((g) => g.cites.map((c) => c.cle)).filter((k) => !entrees.has(k)))];
  // the year, with a letter when two references have the same authors and year (2001a, 2001b)
  const annees = new Map();
  if (style !== 'numerique') {
    const parEtiquette = new Map();
    for (const k of cites) {
      const e = entrees.get(k);
      const etiquette = `${auteursCourts(e, langue, true)}|${e.annee}`;
      if (!parEtiquette.has(etiquette)) parEtiquette.set(etiquette, []);
      parEtiquette.get(etiquette).push(e);
    }
    for (const l of parEtiquette.values()) {
      l.sort((a, b) => a.titre.localeCompare(b.titre, langue));
      l.forEach((e, k) => annees.set(e.cle, `${e.annee || t.sd}${l.length > 1 ? String.fromCharCode(97 + k) : ''}`));
    }
  }
  const annee = (k) => annees.get(k) || entrees.get(k).annee || t.sd;
  const numeros = new Map(cites.map((k, i) => [k, i + 1]));
  const suffixe = (c) => (!c.suffixe ? '' : c.suffixe.startsWith(',') ? c.suffixe : `, ${c.suffixe}`);
  const textes = groupes.map((g) => {
    const connus = g.cites.filter((c) => entrees.has(c.cle));
    const brut = (c) => `@${c.cle}`;
    if (style === 'numerique') {
      if (g.narratif) {
        const c = g.cites[0];
        if (!entrees.has(c.cle)) return brut(c);
        return `${auteursCourts(entrees.get(c.cle), langue, false)} [${numeros.get(c.cle)}${suffixe(c)}]`;
      }
      const simples = g.cites.every((c) => !c.prefixe && !c.suffixe && entrees.has(c.cle));
      if (simples) return `[${plages(g.cites.map((c) => numeros.get(c.cle)))}]`;
      return `[${g.cites.map((c) => (entrees.has(c.cle) ? `${c.prefixe ? `${c.prefixe} ` : ''}${numeros.get(c.cle)}${suffixe(c)}` : brut(c))).join('; ')}]`;
    }
    if (g.narratif) {
      const c = g.cites[0];
      if (!entrees.has(c.cle)) return brut(c);
      return `${auteursCourts(entrees.get(c.cle), langue, false)} (${annee(c.cle)}${suffixe(c)})`;
    }
    if (!connus.length && g.cites.length === 1) return brut(g.cites[0]);
    return `(${g.cites.map((c) => (entrees.has(c.cle)
      ? `${c.prefixe ? `${c.prefixe} ` : ''}${c.sansAuteur ? annee(c.cle) : `${auteursCourts(entrees.get(c.cle), langue, true)}, ${annee(c.cle)}`}${suffixe(c)}`
      : brut(c))).join('; ')})`;
  });
  const liste = cites.map((k) => ({ cle: k, numero: numeros.get(k), segments: referenceEnSegments(entrees.get(k), langue, annee(k)) }));
  if (style !== 'numerique') {
    liste.sort((a, b) => {
      const ea = entrees.get(a.cle);
      const eb = entrees.get(b.cle);
      const na = (ea.auteurs[0] || ea.editeurs[0]);
      const nb = (eb.auteurs[0] || eb.editeurs[0]);
      return (na ? nomTri(na) : ea.titre).localeCompare(nb ? nomTri(nb) : eb.titre, langue, { sensitivity: 'base' })
        || annee(a.cle).localeCompare(annee(b.cle)) || ea.titre.localeCompare(eb.titre, langue);
    });
  }
  const titres = groupes.map((g) => g.cites.map((c) => (entrees.has(c.cle) ? texteSeul(referenceEnSegments(entrees.get(c.cle), langue, annee(c.cle))) : `@${c.cle} : ${t.introuvable}`)).join('\n'));
  return { textes, titres, liste, inconnues, style, nonCitees };
}

// 1, 2, 3, 5 -> "1–3, 5"
function plages(nombres) {
  const n = [...new Set(nombres)].sort((a, b) => a - b);
  const r = [];
  for (let i = 0; i < n.length; i += 1) {
    let j = i;
    while (j + 1 < n.length && n[j + 1] === n[j] + 1) j += 1;
    r.push(j - i >= 2 ? `${n[i]}–${n[j]}` : j > i ? `${n[i]}, ${n[j]}` : `${n[i]}`);
    i = j;
  }
  return r.join(', ');
}

// For the completion and the search: an entry in one line ("Wold et al. 2001 · PLS-regression…").
export function resume(e, langue = 'fr') {
  const titre = String(e.titre || '').replace(/[]/g, '');
  return `${auteursCourts(e, langue, false)} ${e.annee || ''}${titre ? ` · ${titre}` : ''}`.trim();
}

// Whether an entry answers a search: every word in its key, authors, year or title (accents and
// case aside).
const plier = (s) => String(s || '').normalize('NFD').replace(/\p{M}/gu, '').toLowerCase();
export function correspond(e, recherche) {
  const texte = plier([e.cle, e.annee, e.titre, ...e.auteurs.map((n) => n.litteral || `${n.nom} ${n.prenom}`), ...e.editeurs.map((n) => n.nom || n.litteral)].join(' '));
  return plier(recherche).split(/\s+/).filter(Boolean).every((m) => texte.includes(m));
}

// The heading under which the list of references goes, when a document ends with one.
export const TITRE_REFERENCES = /^(r[ée]f[ée]rences( bibliographiques| cit[ée]es)?|bibliograph(ie|y)|literature cited|litt[ée]rature cit[ée]e|works cited|sources)$/i;
