// A document (documents.js) turned into its preview (HTML), a Word file (.docx) or a Quarto file
// (.qmd): its Markdown read by markdown-it, the references to the project's results ({{...}},
// references.js) as tokens of their own. No page here: the references come already resolved,
// `resolus[i]` for the i-th reference of the document, as one of
//   { genre: 'valeur' | 'renvoi', texte }                     a value, or "Figure 2"
//   { genre: 'figure', image (PNG data URL), largeur, hauteur, numero, legende, etiquette }
//   { genre: 'tableau', blocs ([{ texte } | { entetes, lignes }]), numero, legende, etiquette }
//   { genre: 'texte', paragraphes }                           the Methods text
//   { erreur }                                                why it can't be resolved
// The citations ([@wold2001], @wold2001: bibliographie.js) are tokens too (nuee_cite), written as
// `citations` gives them (formaterCitations), with the list of references at the end or where the
// document puts {{bibliographie}}.
import { lireReference, lireReferences } from './references.js';
import { lireGroupeCitations, MOTIF_CLE, cleSansAccolades, TITRE_REFERENCES } from './bibliographie.js';
import { zip } from './zip.js';

const echapper = (s) => String(s ?? '').replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
export const BLOCS = ['figure', 'tableau', 'texte'];

// ------------------------------------------------------------ the YAML header

// A Quarto document may start with a YAML header between --- lines (title, author, date, lang,
// bibliography...). Only "key: value" lines are read, and lists ("key:" then "  - item" lines, or
// "key: [a, b]"); the text goes on after it.
export function lireEntete(texte) {
  const m = /^---[ \t]*\r?\n([\s\S]*?)\r?\n(?:---|\.\.\.)[ \t]*(?:\r?\n|$)/.exec(texte);
  if (!m) return { entete: {}, corps: texte, brut: '' };
  const entete = {};
  const sansGuillemets = (v) => v.trim().replace(/^(["'])(.*)\1$/, '$2');
  let liste = null;
  for (const ligne of m[1].split(/\r?\n/)) {
    const item = /^\s+-\s+(.*)$/.exec(ligne);
    if (item && liste) { liste.push(sansGuillemets(item[1])); continue; }
    const kv = /^([A-Za-z_][\w-]*)\s*:\s*(.*)$/.exec(ligne);
    if (!kv) continue;
    liste = null;
    const v = kv[2].trim();
    if (!v) { liste = []; entete[kv[1]] = liste; } else if (/^\[.*\]$/.test(v)) entete[kv[1]] = v.slice(1, -1).split(',').map(sansGuillemets).filter(Boolean);
    else entete[kv[1]] = sansGuillemets(v);
  }
  return { entete, corps: texte.slice(m[0].length), brut: m[0] };
}

// A header's value as a list (bibliography: a file, or several).
export const listeEntete = (v) => [].concat(v ?? []).map(String).filter(Boolean);

// The header `brut` with `cle` set to `valeur` (a list for several); `seulementSiAbsente`: left as
// it is when the header has it already. Without a header, one with it alone.
export function fixerEntete(brut, cle, valeur, seulementSiAbsente = false) {
  const yaml = (v) => (/^[\w./\\:-]+$/.test(v) && !/^\d/.test(v) ? v : JSON.stringify(v));
  const ligne = Array.isArray(valeur) ? `${cle}:\n${valeur.map((v) => `  - ${yaml(v)}`).join('\n')}` : `${cle}: ${yaml(String(valeur))}`;
  if (!brut) return `---\n${ligne}\n---\n\n`;
  const lignes = brut.replace(/\r\n/g, '\n').split('\n');
  const k = lignes.findIndex((l, i) => i > 0 && new RegExp(`^${cle}\\s*:`).test(l));
  if (k > 0) {
    if (seulementSiAbsente) return brut;
    let fin = k + 1;
    while (fin < lignes.length && /^\s+-\s/.test(lignes[fin])) fin += 1;
    lignes.splice(k, fin - k, ligne);
  } else {
    const fermeture = lignes.findIndex((l, i) => i > 0 && /^(---|\.\.\.)\s*$/.test(l));
    lignes.splice(fermeture, 0, ligne);
  }
  return lignes.join('\n');
}

// ------------------------------------------------------------ markdown-it

// The references as tokens (nuee_ref, meta: { ref, brut, i }); those alone in their paragraph may
// be blocks (meta.bloc, meta.paragraphe: the index of that paragraph's token, meta.cache: whether
// markdown-it hides it, in a tight list): a plot, a table or the Methods text then takes the
// paragraph's place (estBloc). A paragraph of Quarto's {{< pagebreak >}} is a page break.
export function pluginReferences(md) {
  md.inline.ruler.before('emphasis', 'nuee_ref', (state, silencieux) => {
    const s = state.src;
    const p = state.pos;
    if (s.charCodeAt(p) !== 0x7b || s.charCodeAt(p + 1) !== 0x7b) return false;
    const fin = s.indexOf('}}', p + 2);
    if (fin < 0) return false;
    const interieur = s.slice(p + 2, fin);
    if (PLACE_BIBLIOGRAPHIE.test(interieur.trim())) {  // where the list of references goes
      if (!silencieux) state.push('nuee_biblio', '', 0).meta = { brut: s.slice(p, fin + 2) };
      state.pos = fin + 2;
      return true;
    }
    const ref = lireReference(interieur);
    if (!ref) return false;
    if (!silencieux) state.push('nuee_ref', '', 0).meta = { ref, brut: s.slice(p, fin + 2) };
    state.pos = fin + 2;
    return true;
  });
  // [see @a, p. 3; -@b]: before the links (a link is [text](address) or [text][label])
  md.inline.ruler.before('link', 'nuee_cite', (state, silencieux) => {
    const s = state.src;
    const p = state.pos;
    if (s.charCodeAt(p) !== 0x5b) return false;
    let fin = -1;
    for (let k = p + 1; k < state.posMax; k += 1) {
      const c = s.charCodeAt(k);
      if (c === 0x5c) { k += 1; continue; }
      if (c === 0x5b) return false;
      if (c === 0x5d) { fin = k; break; }
    }
    if (fin < 0 || s.charCodeAt(fin + 1) === 0x28 || s.charCodeAt(fin + 1) === 0x5b) return false;
    const cites = lireGroupeCitations(s.slice(p + 1, fin));
    if (!cites) return false;
    if (!silencieux) state.push('nuee_cite', '', 0).meta = { cites, narratif: false, brut: s.slice(p, fin + 1) };
    state.pos = fin + 1;
    return true;
  });
  // @a in a sentence ("Wold et al. (2001)"), its locator after it between brackets (@a [p. 3]);
  // not after a letter (an address, a@b.fr)
  md.inline.ruler.before('emphasis', 'nuee_cite_texte', (state, silencieux) => {
    const s = state.src;
    const p = state.pos;
    // (nor in a link's text: [@a](address) is a link)
    if (s.charCodeAt(p) !== 0x40 || state.linkLevel > 0 || (p > 0 && /[\p{L}\p{N}_]/u.test(s[p - 1]))) return false;
    const m = MOTIF_CLE.exec(s.slice(p, state.posMax));
    if (!m) return false;
    let fin = p + m[0].length;
    const loc = /^ ?\[([^[\]@]*)\](?![([])/.exec(s.slice(fin, state.posMax));
    if (loc) fin += loc[0].length;
    if (!silencieux) {
      state.push('nuee_cite', '', 0).meta = { narratif: true, brut: s.slice(p, fin),
        cites: [{ cle: cleSansAccolades(m[1]), prefixe: '', suffixe: loc ? loc[1].trim() : '', sansAuteur: false }] };
    }
    state.pos = fin;
    return true;
  });
  md.core.ruler.push('nuee_ref_blocs', (state) => {
    const t = state.tokens;
    for (let i = 0; i + 2 < t.length; i += 1) {
      if (t[i].type !== 'paragraph_open' || t[i + 1].type !== 'inline' || t[i + 2].type !== 'paragraph_close') continue;
      if (/^\{\{<\s*pagebreak\s*>\}\}$/.test(t[i + 1].content.trim())) {
        t[i].meta = { saut: true };
        t[i].attrJoin('class', 'saut-page');
        t[i + 1].children = [];
        continue;
      }
      const utiles = (t[i + 1].children || []).filter((c) => !(c.type === 'text' && !c.content.trim()) && c.type !== 'softbreak');
      if (utiles.length === 1 && utiles[0].type === 'nuee_ref') Object.assign(utiles[0].meta, { bloc: true, paragraphe: i, cache: t[i].hidden });
      if (utiles.length === 1 && utiles[0].type === 'nuee_biblio') {  // (the list takes its paragraph's place)
        t[i].meta = { biblio: true };
        t[i].hidden = true;
        t[i + 2].hidden = true;
      }
    }
  });
}

// {{bibliographie}}: where the list of references goes (by default, at the end).
export const PLACE_BIBLIOGRAPHIE = /^(bibliographie|bibliography|r[ée]f[ée]rences)$/i;

// A reference resolved as a plot, a table or the Methods text.
export const estBloc = (r) => !!r && !r.erreur && BLOCS.includes(r.genre);

// The document read: its header, its tokens, its references in order (token.meta.i), its citations
// in order (token.meta.ic: { cites, narratif } for formaterCitations), whether it says where the
// list of references goes ({{bibliographie}}), and whether its last heading is that list's.
export function analyser(md, texte) {
  const { entete, corps } = lireEntete(texte);
  const env = {};
  const tokens = md.parse(corps, env);
  const refs = [];
  const citations = [];
  let biblio = false;
  const parcourir = (liste) => {
    for (const tok of liste) {
      if (tok.type === 'nuee_ref') {
        tok.meta.i = refs.length;
        refs.push(tok);
      } else if (tok.type === 'nuee_cite') {
        tok.meta.ic = citations.length;
        citations.push(tok);
      } else if (tok.type === 'nuee_biblio') biblio = true;
      if (tok.children) parcourir(tok.children);
    }
  };
  parcourir(tokens);
  let dernierTitre = null;
  let niveau = 0;
  tokens.forEach((tok, i) => {
    if (tok.type !== 'heading_open') return;
    dernierTitre = tokens[i + 1].content.trim();
    if (!niveau) niveau = Number(tok.tag.slice(1));
  });
  return { entete, tokens, refs, citations, biblio, env, titreReferences: !!dernierTitre && TITRE_REFERENCES.test(dernierTitre), niveauTitres: niveau || 1 };
}

// The list of references at the end: under the document's last heading when it is one ("#
// Références"), else under a heading of its own.
const titreAjoute = (analyse, citations) => !!citations && citations.liste.length > 0 && !analyse.biblio && !analyse.titreReferences;

const blocReference = (inline) => {
  const enfants = (inline && inline.children) || [];
  const r = enfants.find((c) => c.type === 'nuee_ref');
  return r && r.meta.bloc ? r : null;
};

// ------------------------------------------------------------ HTML (preview, PDF, HTML file)

// `libelles`: { figure, tableau, seul, references } in the document's language (seul: the message of
// a plot, table or text not alone on its line; references: the heading of the list of references).
// `citations`: the citations written (formaterCitations), or null while the references are read.
export function versHTML(md, analyse, resolus, libelles, citations = null) {
  md.renderer.rules.nuee_ref = (tokens, idx) => htmlReference(tokens[idx], resolus[tokens[idx].meta.i], libelles);
  md.renderer.rules.nuee_cite = (tokens, idx) => htmlCitation(tokens[idx], citations);
  md.renderer.rules.nuee_biblio = () => htmlBibliographie(citations);
  for (const tok of analyse.refs) {
    if (!tok.meta.bloc) continue;
    const cache = estBloc(resolus[tok.meta.i]) || tok.meta.cache;
    analyse.tokens[tok.meta.paragraphe].hidden = cache;
    analyse.tokens[tok.meta.paragraphe + 2].hidden = cache;
  }
  const e = analyse.entete;
  const tete = (e.title ? `<h1 class="titre-document">${echapper(e.title)}</h1>` : '')
    + (e.subtitle ? `<p class="sous-titre-document">${echapper(e.subtitle)}</p>` : '')
    + (e.author || e.date ? `<p class="auteur-document">${echapper([].concat(e.author || [], e.date || []).join(' · '))}</p>` : '');
  const n = analyse.niveauTitres;
  const fin = !citations || analyse.biblio || !citations.liste.length ? ''
    : (titreAjoute(analyse, citations) ? `<h${n}>${echapper(libelles.references || 'Références')}</h${n}>` : '') + htmlBibliographie(citations);
  return tete + md.renderer.render(analyse.tokens, md.options, analyse.env) + fin;
}

function htmlCitation(tok, citations) {
  const brut = echapper(tok.meta.brut);
  if (!citations) return `<span class="cite attente">${brut}</span>`;
  const inconnue = tok.meta.cites.some((c) => citations.inconnues.includes(c.cle));
  return `<span class="cite${inconnue ? ' erreur' : ''}" title="${echapper(citations.titres[tok.meta.ic])}">${echapper(citations.textes[tok.meta.ic])}</span>`;
}

const htmlSegment = (x) => (x.lien ? `<a href="${echapper(x.lien)}">${echapper(x.texte)}</a>` : x.italique ? `<i>${echapper(x.texte)}</i>` : echapper(x.texte));
function htmlBibliographie(c) {
  if (!c || !c.liste.length) return '';
  return `<div class="bibliographie">${c.liste.map((r) => `<p class="reference" id="ref-${echapper(r.cle)}">${
    c.style === 'numerique' ? `<span class="numero">[${r.numero}]</span> ` : ''}${r.segments.map(htmlSegment).join('')}</p>`).join('')}</div>`;
}

function htmlReference(tok, r, libelles) {
  const brut = echapper(tok.meta.brut);
  if (!r) return `<span class="ref attente">${brut}</span>`;
  // (alone on its line, the reason shows; in a sentence, on hovering)
  if (r.erreur) return `<span class="ref erreur" title="${echapper(r.erreur)}">${brut}${tok.meta.bloc ? ` <small>— ${echapper(r.erreur)}</small>` : ''}</span>`;
  if (BLOCS.includes(r.genre) && !tok.meta.bloc) return `<span class="ref erreur" title="${echapper(libelles.seul)}">${brut}</span>`;
  const id = r.etiquette ? ` id="${echapper(r.etiquette)}"` : '';
  const legende = (lib) => `<b>${echapper(lib)} ${r.numero}.</b>${r.legende ? ` ${echapper(r.legende)}` : ''}`;
  switch (r.genre) {
    case 'figure':
      return `<figure class="ref-figure"${id} title="${brut}"><img src="${r.image}" alt="${echapper(r.legende || '')}"><figcaption>${legende(libelles.figure)}</figcaption></figure>`;
    case 'tableau':
      return `<div class="ref-tableau"${id} title="${brut}"><p class="legende">${legende(libelles.tableau)}</p>${r.blocs.map(htmlBloc).join('')}</div>`;
    case 'texte':
      return `<div class="ref-texte" title="${brut}">${r.paragraphes.map((p) => `<p>${echapper(p)}</p>`).join('')}</div>`;
    default:
      return `<span class="ref" title="${brut}">${echapper(r.texte)}</span>`;
  }
}

function htmlBloc(b) {
  if (b.texte !== undefined) return `<p class="note">${echapper(b.texte)}</p>`;
  return `<table><thead><tr>${b.entetes.map((h) => `<th>${echapper(h)}</th>`).join('')}</tr></thead><tbody>${
    b.lignes.map((l) => `<tr>${l.map((v, k) => `<td${k ? ' class="num"' : ''}>${echapper(v)}</td>`).join('')}</tr>`).join('')}</tbody></table>`;
}

// A whole HTML page (PDF, or an .html file that holds its images), `corps` from versHTML.
export function pageHTML(corps, titre, langue) {
  return `<!doctype html><html lang="${langue}"><head><meta charset="utf-8"><title>${echapper(titre)}</title><style>
@page { size: A4; margin: 20mm 18mm; }
body { font-family: Calibri, 'Segoe UI', Arial, sans-serif; font-size: 11pt; line-height: 1.45; color: #1a1a1a; max-width: 46em; margin: 2em auto; padding: 0 1em; }
@media print { body { margin: 0; max-width: none; padding: 0; } }
h1, h2, h3, h4 { line-height: 1.2; break-after: avoid; } h1 { font-size: 16pt; } h2 { font-size: 13.5pt; } h3 { font-size: 12pt; } h4 { font-size: 11pt; }
.titre-document { font-size: 20pt; margin-bottom: 4pt; } .sous-titre-document { font-size: 13pt; color: #444; margin-top: 0; } .auteur-document { color: #555; }
table { border-collapse: collapse; font-size: 9.5pt; margin: 6pt 0; } th, td { border: 1px solid #bbb; padding: 2pt 6pt; text-align: left; }
th { background: #eef2f7; } td.num { text-align: right; } figure { margin: 10pt 0; break-inside: avoid; text-align: center; }
figure img { max-width: 100%; } figcaption, .legende { font-size: 9.5pt; color: #333; text-align: left; } .ref-tableau { break-inside: avoid; }
.note { font-size: 9.5pt; color: #444; } blockquote { margin-left: 1.5em; color: #444; font-style: italic; }
code, pre { font-family: Consolas, monospace; font-size: 9.5pt; } pre { background: #f5f5f5; padding: 6pt; white-space: pre-wrap; }
.ref.erreur, .cite.erreur { color: #b00020; } .saut-page { break-after: page; height: 0; margin: 0; }
.bibliographie p { padding-left: 1.6em; text-indent: -1.6em; margin: 0 0 4pt; } .bibliographie a { color: inherit; }
@media screen { .saut-page { border-top: 1px dashed #bbb; margin: 1.5em 0; } }
</style></head><body>${corps}</body></html>`;
}

// ------------------------------------------------------------ Word (.docx)

const xml = (s) => String(s ?? '').replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
const NS = 'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"'
  + ' xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"'
  + ' xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"';

// The document as a Word file: headings, paragraphs (bold, italic, struck, code, links), lists,
// quotes, code, Markdown tables, the references (values in the text, plots and tables numbered
// with their captions, the Methods text), the citations and their list. Returns its bytes.
export function versDocx(analyse, resolus, { langue = 'fr', libelles, citations = null }) {
  const images = [];
  const liens = [];
  const listes = [];  // one numbering per list: { ordonnee, debut }
  const corps = [];

  const run = (texte, f = {}) => {
    const rpr = [f.gras && '<w:b/>', f.italique && '<w:i/>', f.barre && '<w:strike/>',
      f.code && '<w:rFonts w:ascii="Consolas" w:hAnsi="Consolas" w:cs="Consolas"/>', f.lien && '<w:rStyle w:val="Lien"/>',
      f.erreur && '<w:color w:val="B00020"/>'].filter(Boolean).join('');
    return String(texte).split('\n').map((ligne, k) => `${k ? '<w:r><w:br/></w:r>' : ''}<w:r>${rpr ? `<w:rPr>${rpr}</w:rPr>` : ''}<w:t xml:space="preserve">${xml(ligne)}</w:t></w:r>`).join('');
  };
  const para = (contenu, { style, num, centre } = {}) => {
    const ppr = [style && `<w:pStyle w:val="${style}"/>`, num && `<w:numPr><w:ilvl w:val="${num.niveau}"/><w:numId w:val="${num.id}"/></w:numPr>`,
      centre && '<w:jc w:val="center"/>'].filter(Boolean).join('');
    return `<w:p>${ppr ? `<w:pPr>${ppr}</w:pPr>` : ''}${contenu}</w:p>`;
  };
  const enLigne = (enfants) => {
    let f = {};
    let sortie = '';
    for (const c of enfants || []) {
      switch (c.type) {
        case 'text': sortie += run(c.content, f); break;
        case 'softbreak': sortie += run(' ', f); break;
        case 'hardbreak': sortie += '<w:r><w:br/></w:r>'; break;
        case 'strong_open': f = { ...f, gras: true }; break;
        case 'strong_close': f = { ...f, gras: false }; break;
        case 'em_open': f = { ...f, italique: true }; break;
        case 'em_close': f = { ...f, italique: false }; break;
        case 's_open': f = { ...f, barre: true }; break;
        case 's_close': f = { ...f, barre: false }; break;
        case 'code_inline': sortie += run(c.content, { ...f, code: true }); break;
        case 'link_open':
          liens.push(c.attrGet('href') || '');
          sortie += `<w:hyperlink r:id="rIdLien${liens.length}">`;
          f = { ...f, lien: true };
          break;
        case 'link_close': sortie += '</w:hyperlink>'; f = { ...f, lien: false }; break;
        case 'image': sortie += run(c.content || c.attrGet('alt') || '', { ...f, italique: true }); break;
        case 'nuee_ref': {
          const r = resolus[c.meta.i];
          sortie += r && !r.erreur && (r.genre === 'valeur' || r.genre === 'renvoi') ? run(r.texte, f) : run(c.meta.brut, { ...f, erreur: true });
          break;
        }
        case 'nuee_cite': {
          const inconnue = !citations || c.meta.cites.some((x) => citations.inconnues.includes(x.cle));
          sortie += run(citations ? citations.textes[c.meta.ic] : c.meta.brut, { ...f, erreur: inconnue });
          break;
        }
        default: break;  // (HTML in the text: left out)
      }
    }
    return sortie;
  };
  // the list of references: a paragraph each, its title in italic, its DOI a link
  const bibliographie = () => (citations ? citations.liste.map((r) => para(
    (citations.style === 'numerique' ? run(`[${r.numero}] `) : '') + r.segments.map((x) => {
      if (!x.lien) return run(x.texte, { italique: x.italique });
      liens.push(x.lien);
      return `<w:hyperlink r:id="rIdLien${liens.length}">${run(x.texte, { lien: true })}</w:hyperlink>`;
    }).join(''), { style: 'Bibliographie' })) : []);
  const image = (r) => {
    images.push(r.image);
    const n = images.length;
    const cx = 5760000;  // 16 cm
    const cy = Math.round(cx * (r.hauteur || 580) / (r.largeur || 900));
    return para(`<w:r><w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0"><wp:extent cx="${cx}" cy="${cy}"/><wp:docPr id="${n}" name="Figure ${n}"/>`
      + '<wp:cNvGraphicFramePr><a:graphicFrameLocks noChangeAspect="1"/></wp:cNvGraphicFramePr><a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic>'
      + `<pic:nvPicPr><pic:cNvPr id="${n}" name="figure${n}.png"/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill><a:blip r:embed="rIdImage${n}"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>`
      + `<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="${cx}" cy="${cy}"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r>`,
    { centre: true, style: 'Figure' });
  };
  const bord = (n) => `<w:${n} w:val="single" w:sz="4" w:space="0" w:color="AAAAAA"/>`;
  const tableau = (entetes, lignes, alignements = []) => {
    const cellule = (contenu, k, entete) => `<w:tc><w:tcPr><w:tcW w:w="0" w:type="auto"/>${entete ? '<w:shd w:val="clear" w:color="auto" w:fill="EEF2F7"/>' : ''}</w:tcPr>`
      + `<w:p><w:pPr><w:pStyle w:val="Tableau"/>${alignements[k] === 'right' || (!entete && k > 0 && alignements[k] === undefined && alignements.length === 0) ? '<w:jc w:val="right"/>' : alignements[k] === 'center' ? '<w:jc w:val="center"/>' : ''}</w:pPr>${contenu}</w:p></w:tc>`;
    return `<w:tbl><w:tblPr><w:tblW w:w="0" w:type="auto"/><w:tblBorders>${['top', 'left', 'bottom', 'right', 'insideH', 'insideV'].map(bord).join('')}</w:tblBorders>`
      + '<w:tblCellMar><w:left w:w="70" w:type="dxa"/><w:right w:w="70" w:type="dxa"/></w:tblCellMar></w:tblPr>'
      + `<w:tblGrid>${entetes.map(() => '<w:gridCol/>').join('')}</w:tblGrid>`
      + `<w:tr><w:trPr><w:tblHeader/></w:trPr>${entetes.map((h, k) => cellule(h, k, true)).join('')}</w:tr>`
      + lignes.map((l) => `<w:tr>${l.map((v, k) => cellule(v, k, false)).join('')}</w:tr>`).join('') + '</w:tbl>';
  };
  const legende = (lib, r) => para(run(`${lib} ${r.numero}.`, { gras: true }) + (r.legende ? run(` ${r.legende}`) : ''), { style: 'Legende' });
  const blocRef = (r) => {
    if (r.genre === 'figure') corps.push(image(r), legende(libelles.figure, r));
    else if (r.genre === 'tableau') {
      corps.push(legende(libelles.tableau, r));
      for (const b of r.blocs) {
        if (b.texte !== undefined) corps.push(para(run(b.texte), { style: 'Note' }));
        else corps.push(tableau(b.entetes.map((h) => run(h, { gras: true })), b.lignes.map((l) => l.map((v) => run(v)))), '<w:p/>');
      }
    } else r.paragraphes.forEach((p) => corps.push(para(run(p))));
  };

  const e = analyse.entete;
  if (e.title) corps.push(para(run(e.title), { style: 'Title' }));
  if (e.subtitle) corps.push(para(run(e.subtitle), { style: 'Subtitle' }));
  if (e.author || e.date) corps.push(para(run([].concat(e.author || [], e.date || []).join(' · ')), { style: 'Auteur' }));

  const t = analyse.tokens;
  const pile = [];  // the lists open: { id, niveau }
  let item = null;  // the numbering of the first paragraph of a list item
  let citation = 0;
  for (let i = 0; i < t.length; i += 1) {
    const k = t[i];
    switch (k.type) {
      case 'heading_open':
        corps.push(para(enLigne(t[i + 1].children), { style: `Titre${Math.min(4, Number(k.tag.slice(1)))}` }));
        i += 2;
        break;
      case 'paragraph_open': {
        const ref = blocReference(t[i + 1]);
        if (k.meta && k.meta.saut) corps.push('<w:p><w:r><w:br w:type="page"/></w:r></w:p>');
        else if (k.meta && k.meta.biblio) corps.push(...bibliographie());
        else if (ref && estBloc(resolus[ref.meta.i])) blocRef(resolus[ref.meta.i]);
        else corps.push(para(enLigne(t[i + 1].children), { style: citation ? 'Citation' : pile.length ? 'Liste' : null, num: item }));
        item = null;
        i += 2;
        break;
      }
      case 'bullet_list_open':
      case 'ordered_list_open':
        listes.push({ ordonnee: k.type === 'ordered_list_open', debut: Number(k.attrGet('start') || 1) });
        pile.push({ id: listes.length, niveau: Math.min(pile.length, 2) });
        break;
      case 'bullet_list_close':
      case 'ordered_list_close':
        pile.pop();
        break;
      case 'list_item_open': item = { ...pile[pile.length - 1] }; break;
      case 'blockquote_open': citation += 1; break;
      case 'blockquote_close': citation -= 1; break;
      case 'fence':
      case 'code_block':
        k.content.replace(/\n$/, '').split('\n').forEach((l) => corps.push(para(run(l, { code: true }), { style: 'Code' })));
        break;
      case 'hr': corps.push('<w:p><w:pPr><w:pBdr><w:bottom w:val="single" w:sz="6" w:space="1" w:color="AAAAAA"/></w:pBdr></w:pPr></w:p>'); break;
      case 'table_open': {
        const lignes = [];
        let entetes = null;
        const alignements = [];
        let j = i + 1;
        let ligne = null;
        for (; j < t.length && t[j].type !== 'table_close'; j += 1) {
          const x = t[j];
          if (x.type === 'tr_open') ligne = [];
          else if (x.type === 'th_open' || x.type === 'td_open') {
            const style = x.attrGet('style') || '';
            if (x.type === 'th_open') alignements.push((/text-align:(\w+)/.exec(style) || [])[1] || 'left');
          } else if (x.type === 'inline' && ligne) ligne.push(enLigne(x.children));
          else if (x.type === 'tr_close') {
            if (!entetes) entetes = ligne;
            else lignes.push(ligne);
            ligne = null;
          }
        }
        corps.push(tableau(entetes || [], lignes, alignements), '<w:p/>');
        i = j;
        break;
      }
      default: break;
    }
  }
  if (citations && citations.liste.length && !analyse.biblio) {
    if (titreAjoute(analyse, citations)) corps.push(para(run(libelles.references || 'Références'), { style: `Titre${Math.min(4, analyse.niveauTitres)}` }));
    corps.push(...bibliographie());
  }

  const document = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<w:document ${NS}><w:body>${corps.join('')}`
    + '<w:sectPr><w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1134" w:right="1134" w:bottom="1134" w:left="1134" w:header="567" w:footer="567" w:gutter="0"/></w:sectPr></w:body></w:document>';
  return zip([['[Content_Types].xml', texte(typesDocx())], ['_rels/.rels', texte(RACINE)], ['word/document.xml', texte(document)],
    ['word/styles.xml', texte(stylesDocx(langue))], ['word/numbering.xml', texte(numerotationDocx(listes))],
    ['word/_rels/document.xml.rels', texte(relationsDocx(images, liens))],
    ...images.map((url, n) => [`word/media/figure${n + 1}.png`, png(url)])]);
}

const texte = (s) => new TextEncoder().encode(s);
const png = (url) => {
  const b = atob(String(url).split(',')[1] || '');
  const o = new Uint8Array(b.length);
  for (let i = 0; i < b.length; i += 1) o[i] = b.charCodeAt(i);
  return o;
};
const RACINE = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
  + '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/></Relationships>';

function typesDocx() {
  return '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
    + '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/>'
    + '<Default Extension="png" ContentType="image/png"/>'
    + '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>'
    + '<Override PartName="/word/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>'
    + '<Override PartName="/word/numbering.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml"/></Types>';
}

function relationsDocx(images, liens) {
  return '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
    + '<Relationship Id="rIdStyles" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>'
    + '<Relationship Id="rIdNumerotation" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering" Target="numbering.xml"/>'
    + images.map((_, i) => `<Relationship Id="rIdImage${i + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/figure${i + 1}.png"/>`).join('')
    + liens.map((h, i) => `<Relationship Id="rIdLien${i + 1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink" Target="${xml(h)}" TargetMode="External"/>`).join('')
    + '</Relationships>';
}

// One bullet numbering and one decimal numbering (three levels each), and an instance per list
// (each ordered list starting at its own number).
function numerotationDocx(listes) {
  const niveaux = (ordonnee) => [0, 1, 2].map((n) => `<w:lvl w:ilvl="${n}"><w:start w:val="1"/><w:numFmt w:val="${ordonnee ? 'decimal' : 'bullet'}"/>`
    + `<w:lvlText w:val="${ordonnee ? `%${n + 1}.` : ['•', '◦', '▪'][n]}"/><w:lvlJc w:val="left"/><w:pPr><w:ind w:left="${720 + 360 * n}" w:hanging="360"/></w:pPr></w:lvl>`).join('');
  return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<w:numbering ${NS}>`
    + `<w:abstractNum w:abstractNumId="0"><w:multiLevelType w:val="hybridMultilevel"/>${niveaux(false)}</w:abstractNum>`
    + `<w:abstractNum w:abstractNumId="1"><w:multiLevelType w:val="hybridMultilevel"/>${niveaux(true)}</w:abstractNum>`
    + listes.map((l, i) => `<w:num w:numId="${i + 1}"><w:abstractNumId w:val="${l.ordonnee ? 1 : 0}"/>${l.ordonnee ? `<w:lvlOverride w:ilvl="0"><w:startOverride w:val="${l.debut}"/></w:lvlOverride>` : ''}</w:num>`).join('')
    + '</w:numbering>';
}

function stylesDocx(langue) {
  const style = (id, nom, rpr, ppr = '', base = 'Normal', type = 'paragraph') => `<w:style w:type="${type}" w:styleId="${id}"><w:name w:val="${nom}"/>`
    + `${base ? `<w:basedOn w:val="${base}"/>` : ''}<w:qFormat/>${ppr ? `<w:pPr>${ppr}</w:pPr>` : ''}<w:rPr>${rpr}</w:rPr></w:style>`;
  const titre = (n, taille, avant) => style(`Titre${n}`, `heading ${n}`, `<w:b/><w:color w:val="1F3F6F"/><w:sz w:val="${taille}"/>`,
    `<w:keepNext/><w:spacing w:before="${avant}" w:after="100"/><w:outlineLvl w:val="${n - 1}"/>`);
  return `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<w:styles ${NS}><w:docDefaults><w:rPrDefault><w:rPr>`
    + `<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:cs="Calibri"/><w:sz w:val="22"/><w:lang w:val="${langue === 'fr' ? 'fr-FR' : 'en-GB'}"/></w:rPr></w:rPrDefault>`
    + '<w:pPrDefault><w:pPr><w:spacing w:after="120" w:line="276" w:lineRule="auto"/></w:pPr></w:pPrDefault></w:docDefaults>'
    + style('Normal', 'Normal', '', '', null)
    + style('Title', 'Title', '<w:b/><w:sz w:val="40"/>', '<w:spacing w:after="80"/>')
    + style('Subtitle', 'Subtitle', '<w:color w:val="444444"/><w:sz w:val="28"/>', '<w:spacing w:after="80"/>')
    + style('Auteur', 'Auteur', '<w:color w:val="555555"/>', '<w:spacing w:after="240"/>')
    + titre(1, 30, 360) + titre(2, 26, 280) + titre(3, 23, 220) + titre(4, 22, 200)
    + style('Legende', 'caption', '<w:sz w:val="19"/>', '<w:keepLines/><w:spacing w:before="60" w:after="200"/>')
    + style('Figure', 'Figure', '', '<w:keepNext/><w:spacing w:before="120" w:after="0"/>')
    + style('Note', 'Note', '<w:color w:val="444444"/><w:sz w:val="19"/>', '<w:spacing w:after="60"/>')
    + style('Citation', 'Quote', '<w:i/><w:color w:val="444444"/>', '<w:ind w:left="567"/>')
    + style('Liste', 'List Paragraph', '', '<w:spacing w:after="40"/><w:contextualSpacing/>')
    + style('Code', 'Code', '<w:rFonts w:ascii="Consolas" w:hAnsi="Consolas" w:cs="Consolas"/><w:sz w:val="19"/>', '<w:spacing w:after="0"/><w:shd w:val="clear" w:color="auto" w:fill="F3F3F3"/>')
    + style('Tableau', 'Tableau', '<w:sz w:val="18"/>', '<w:spacing w:after="0"/>')
    + style('Bibliographie', 'Bibliography', '', '<w:ind w:left="567" w:hanging="567"/><w:spacing w:after="80"/>')
    + style('Lien', 'Hyperlink', '<w:color w:val="1F5FA8"/><w:u w:val="single"/>', '', null, 'character')
    + '</w:styles>';
}

// ------------------------------------------------------------ Quarto (.qmd)

// The parts of a text in code (fenced blocks, `spans`), where a {{...}} is left as it is.
export function zonesCode(texte) {
  const zones = [];
  const lignes = texte.split('\n');
  let pos = 0;
  let debut = -1;
  let cloture = '';
  for (const l of lignes) {
    const f = /^\s{0,3}(`{3,}|~{3,})/.exec(l);
    if (debut < 0 && f) { debut = pos; cloture = f[1][0].repeat(f[1].length); }
    else if (debut >= 0 && f && f[1].startsWith(cloture)) { zones.push([debut, pos + l.length]); debut = -1; }
    pos += l.length + 1;
  }
  if (debut >= 0) zones.push([debut, texte.length]);
  const dans = (i) => zones.some(([a, b]) => i >= a && i < b);
  for (const m of texte.matchAll(/(`+)[^`][\s\S]*?\1/g)) if (!dans(m.index)) zones.push([m.index, m.index + m[0].length]);
  return zones;
}

// Quarto labels: a figure's starts with fig-, a table's with tbl- (Quarto numbers them and
// writes "Figure 2" for @fig-...).
export const etiquetteQuarto = (genre, etiquette, numero) => {
  const prefixe = genre === 'figure' ? 'fig-' : 'tbl-';
  const e = etiquette || `nuee-${numero}`;
  return e.startsWith(prefixe) ? e : prefixe + e;
};

const tablePipe = (b) => {
  const cellule = (v) => String(v ?? '').replace(/\|/g, '\\|').replace(/\n/g, ' ');
  return [`| ${b.entetes.map(cellule).join(' | ')} |`, `|${b.entetes.map((_, k) => (k ? '---:' : ':---')).join('|')}|`,
    ...b.lignes.map((l) => `| ${l.map(cellule).join(' | ')} |`)].join('\n');
};

// The references of the text that markdown-it read (analyse.refs, those of `resolus`), found again
// in the text: [{ debut, fin, brut, i (index in resolus) }]. Those it didn't read (in code, after a
// backslash, in a link's address) are left out; the two lists are matched in order, by their text
// (whitespace and quote marks aside), and pick up again after a reference only one of them has.
export function referencesDansTexte(corps, analyse) {
  const zones = zonesCode(corps);
  const dans = (i) => zones.some(([a, b]) => i >= a && i < b);
  const norme = (s) => s.replace(/\s*\n[\s>]*/g, ' ').replace(/\s+/g, ' ');
  const lus = analyse.refs.map((tok) => norme(tok.meta.brut));
  const trouvees = [];
  let j = 0;
  for (const ref of lireReferences(corps)) {
    if (j >= lus.length) break;
    if (dans(ref.debut) || corps[ref.debut - 1] === '\\') continue;
    const n = norme(ref.brut);
    const k = lus.slice(j, j + 4).indexOf(n);
    if (k < 0) continue;
    trouvees.push({ ...ref, i: j + k });
    j += k + 1;
  }
  return trouvees;
}

// The text with its references written in for good (values, Quarto figures and tables with their
// labels, @fig-... for "Figure 2"), for Quarto or another Markdown tool. `fichierFigure(r)`: the
// path written for a figure's image. Returns { texte, erreurs: [brut] }.
export function versQmd(texteDocument, analyse, resolus, { fichierFigure, langue = 'fr', titre = '', bibliographie = null, csl = null, libelles = {} }) {
  const { corps, brut } = lireEntete(texteDocument);
  const refs = referencesDansTexte(corps, analyse);
  const etiquettes = new Map();  // a document's label -> the Quarto label of its figure or table
  resolus.forEach((r) => { if (r && (r.genre === 'figure' || r.genre === 'tableau') && r.etiquette) etiquettes.set(r.etiquette, etiquetteQuarto(r.genre, r.etiquette, r.numero)); });
  const erreurs = [];
  let sortie = '';
  let pos = 0;
  refs.forEach((ref) => {
    const r = resolus[ref.i];
    sortie += corps.slice(pos, ref.debut);
    pos = ref.fin;
    if (!r || r.erreur) { erreurs.push(ref.brut); sortie += ref.brut; return; }
    if (r.genre === 'valeur') sortie += r.texte;
    else if (r.genre === 'renvoi') sortie += etiquettes.has(r.cible) ? `@${etiquettes.get(r.cible)}` : r.texte;
    // (Quarto numbers a figure that has a caption: the plot's name when the document gives none)
    // (the path between < >: it may hold spaces, the folder being named after the document)
    else if (r.genre === 'figure') sortie += `![${r.legende || r.nom || ''}](<${fichierFigure(r)}>){#${etiquetteQuarto('figure', r.etiquette, r.numero)}}`;
    else if (r.genre === 'tableau') {
      const parties = r.blocs.map((b) => (b.texte !== undefined ? b.texte : tablePipe(b)));
      const premiere = r.blocs.findIndex((b) => b.entetes);
      if (premiere >= 0) parties.splice(premiere + 1, 0, `: ${r.legende || r.nom || ''} {#${etiquetteQuarto('tableau', r.etiquette, r.numero)}}`);
      sortie += parties.join('\n\n');
    } else sortie += r.paragraphes.join('\n\n');
  });
  sortie += corps.slice(pos);
  // the citations stay as they are, for Quarto (citeproc) with the .bib written beside the .qmd
  // (`bibliographie`) and the style (`csl`); the list goes where {{bibliographie}} stands, else at
  // the end, under a heading of its own unless the document ends with one
  sortie = sortie.replace(/^[ \t]*\{\{\s*(bibliographie|bibliography|r[ée]f[ée]rences)\s*\}\}[ \t]*$/gim, '::: {#refs}\n:::');
  if (analyse.citations.length && !analyse.biblio && !analyse.titreReferences) {
    sortie = `${sortie.replace(/\s+$/, '')}\n\n${'#'.repeat(analyse.niveauTitres)} ${libelles.references || 'Références'} {.unnumbered}\n`;
  }
  let tete = brut || `---\ntitle: ${JSON.stringify(String(titre))}\n---\n\n`;
  tete = fixerEntete(tete, 'lang', langue, true);
  if (bibliographie) tete = fixerEntete(tete, 'bibliography', bibliographie);
  if (csl) tete = fixerEntete(tete, 'csl', csl);
  // (citeproc for every format: Quarto's PDF through Typst would use Typst's own, in another
  // style, with a heading of its own and the LaTeX of the titles left as it is)
  if (bibliographie) tete = fixerEntete(tete, 'citeproc', 'true', true);
  return { texte: tete + sortie, erreurs };
}
