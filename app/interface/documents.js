// The documents of a project: text in Markdown that refers to the project's results ({{M1.Q2}},
// {{M1.scores | caption}}...: references.js), written beside its live preview, then compiled into
// a Word file, a PDF, an HTML page or a Quarto file (rendu_document.js), or by Quarto when the
// computer has it. Kept in the project (p$documents: moteur/serveur.R, document*), so saved with
// it and in its recovery copies; undo leaves them alone.
// The editor (CodeMirror) and the Markdown reader (markdown-it) come in one file, loaded when a
// document is first opened (copier-vendor.js: vendor/editeur.min.js).
// Citations ([@wold2001], bibliographie.js): from the BibTeX or CSL-JSON files the document's header
// names (bibliography:), from Zotero on this computer (zotero.js: My Library or a group library), or
// kept in the document itself (its `bib`: the references added from Zotero, with where they are in
// it, or pasted, and a copy of those cited from files).
import { t, ajouterTextes, langueActuelle, lire, ecrire, deuxPoints } from './i18n.js';
import { el, icone, dialogue, ouvrirMenuSous } from './ui.js';
import { demander, bureau } from './api.js';
import { VUES } from './panneaux.js';
import { partiesModele, imageVue, LARGEUR, HAUTEUR } from './rapport.js';
import { separerCible, formeReference, texteForme, offreModele, numeroter } from './references.js';
import { zip } from './zip.js';
import { pluginReferences, analyser, versHTML, pageHTML, versDocx, versQmd, referencesReproductibles, lireEntete, listeEntete, fixerEntete, BLOCS } from './rendu_document.js';
import { lireBibtex, lireCslJson, lireBibliographie, versBibtex, formaterCitations, resume, correspond, STYLES,
  elementZotero, lireReponseZotero, memeOeuvre, placerReference, SANS_ZOTERO } from './bibliographie.js';
import { VERSION } from './nouveautes.js';

let md = null;              // markdown-it, once loaded
let chargement = null;
const ouverts = new Set();  // the documents open (their editor), to save what is being typed
let dernier = null;         // the document last written in (Insert in the document, from the explorer)
// A document's panel (one per document: app.js, suffixeVue).
const cleDocument = (pid, id) => `document:${pid}~${id}`;

function chargerEditeur() {
  if (window.CM && window.markdownit && md) return Promise.resolve();
  if (!chargement) {
    chargement = new Promise((ok, ko) => {
      if (window.CM && window.markdownit) { ok(); return; }
      const s = document.createElement('script');
      s.src = 'vendor/editeur.min.js';
      s.onload = () => ok();
      s.onerror = () => { chargement = null; ko(new Error(t('document_editeur_absent'))); };
      document.head.append(s);
    });
  }
  return chargement.then(() => {
    if (!md) md = window.markdownit({ html: false, linkify: true, typographer: false }).use(pluginReferences);
  });
}

// The words of the document's language (its figures and tables, its templates).
const MOTS = {
  fr: { figure: 'Figure', tableau: 'Tableau', introduction: 'Introduction', methodes: 'Matériel et méthodes', resultats: 'Résultats',
    discussion: 'Discussion', references: 'Références', resume: 'Résumé' },
  en: { figure: 'Figure', tableau: 'Table', introduction: 'Introduction', methodes: 'Materials and methods', resultats: 'Results',
    discussion: 'Discussion', references: 'References', resume: 'Summary' },
};
const mots = (langue) => MOTS[langue] || MOTS.fr;
const libelles = (langue) => ({ figure: mots(langue).figure, tableau: mots(langue).tableau, seul: t('document_seul'), references: mots(langue).references });

// The Markdown in the editor, in the theme's colours (light or dark).
let styleTexte = null;
function styleMarkdown() {
  const { HighlightStyle, tags: g } = window.CM;
  return styleTexte || (styleTexte = HighlightStyle.define([
    { tag: g.heading1, fontWeight: 'bold', fontSize: '1.3em' },
    { tag: g.heading2, fontWeight: 'bold', fontSize: '1.15em' },
    { tag: [g.heading3, g.heading4, g.heading5, g.heading6], fontWeight: 'bold' },
    { tag: g.emphasis, fontStyle: 'italic' },
    { tag: g.strong, fontWeight: 'bold' },
    { tag: g.strikethrough, textDecoration: 'line-through' },
    { tag: [g.link, g.url], color: 'var(--accent)' },
    { tag: g.monospace, fontFamily: 'Consolas, "Cascadia Mono", monospace', fontSize: '.92em' },
    { tag: [g.processingInstruction, g.contentSeparator, g.meta, g.comment], color: 'var(--texte-doux)' },
    { tag: g.quote, color: 'var(--texte-doux)', fontStyle: 'italic' },
  ]));
}

const REGRESSIONS = ['LM', 'LMM', 'GLM', 'GAM'];
// A project's models in the order of their numbers (fitting one moves it to the end of its list).
const parNumero = (modeles) => [...modeles].sort((a, b) => (Number(a.numero) || 1e9) - (Number(b.numero) || 1e9));
const AUTRES_ANALYSES = ['RDA', 'CA', 'LDA', 'TREE', 'KMEANS', 'CCA', 'INDVAL', 'TRIM', 'PCOA', 'NMDS', 'PERMANOVA', 'MANTEL', 'FOREST', 'BOOST', 'DISTANCE'];

export function installerDocuments(app) {
  VUES.document = {
    rendre: (conteneur, ctx, params, loc) => rendreDocument(app, conteneur, params, loc),
    titre: (_, p, loc = {}) => {
      const s = [...ouverts].find((x) => x.pid === p.id && x.id === loc.doc);
      const d = [].concat(p.documents || []).find((x) => x.id === loc.doc);
      return (s ? s.titre : d && d.titre) || t('document_sans_titre');
    },
    projet: true,
    suffixe: (loc) => loc.doc || '',
    // the panel closed: what was just typed is kept (the request leaves before the editor goes)
    fermer: (conteneur) => {
      const s = conteneur.etatDocument;
      conteneur.etatDocument = null;
      if (s) { s.enregistrer(); s.fermer(); }
    },
  };
  Object.assign(app, {
    ouvrirDocument: (pid, id) => app.ouvrirVue('document', null, { doc: id }, pid),
    nouveauDocument: (pid) => dialogueNouveau(app, pid),
    menuDocument: (pid, id) => menuDocument(app, pid, id),
    insererReference: (pid, cible, bloc) => insererDepuisExplorateur(app, pid, cible, bloc),
    // a model's view a document can show as a figure (the explorer offers to insert it)
    figureDocument: (m, v) => partiesModele(m).some((x) => x.cle === v && x.vue),
    viderDocuments: (pid) => Promise.all([...ouverts].filter((s) => !pid || s.pid === pid).map((s) => s.enregistrer())),
    // a document written in another window: shown again here, unless it is being written here too
    surDocument: (msg) => { for (const s of ouverts) if (s.pid === msg.projet && s.id === msg.id && !s.modifie) s.recharger(); },
  });
  window.addEventListener('beforeunload', () => { for (const s of ouverts) s.enregistrer(true); });
}

// ------------------------------------------------------------ the panel

async function rendreDocument(app, conteneur, params, loc) {
  const pid = params.projet;
  const p = app.projet(pid);
  if (!p) return;
  const docs = [].concat(p.documents || []);
  const existe = (id) => docs.some((d) => d.id === id);
  let s = conteneur.etatDocument;
  if (s && !existe(s.id) && !s.modifie) {  // (deleted, in another window)
    s.fermer();
    conteneur.etatDocument = s = null;
  }
  if (!loc.doc && docs[0]) loc.doc = docs[0].id;  // (a panel without its document: the project's first)
  const id = loc.doc;
  if (!id || !existe(id)) {
    conteneur.replaceChildren(el('div', { class: 'vide-document' }, el('p', { class: 'note' }, t(id ? 'document_supprime' : 'document_aucun')),
      el('button', { class: 'principal', onclick: () => dialogueNouveau(app, pid) }, icone('file-text'), ` ${t('nouveau_document')}`)));
    return;
  }
  if (s && s.id === id) {
    if (s.langueUI !== langueActuelle()) s.construireBarre();
    s.rafraichir();
    return;
  }
  if (conteneur.chargementDocument === id) return;
  const jeton = (conteneur.jetonDocument || 0) + 1;
  conteneur.jetonDocument = jeton;
  conteneur.chargementDocument = id;
  try {
    if (s) { s.enregistrer(); s.fermer(); conteneur.etatDocument = null; }
    conteneur.replaceChildren(el('p', { class: 'note' }, t('document_chargement')));
    await chargerEditeur();
    const d = await demander('document', { projet: pid, id });
    if (conteneur.jetonDocument !== jeton) return;
    conteneur.etatDocument = creerEditeur(app, conteneur, pid, d, loc);
    app.titrePanneau(cleDocument(pid, id), d.titre || t('document_sans_titre'));
  } finally {
    if (conteneur.jetonDocument === jeton) conteneur.chargementDocument = null;
  }
}

function creerEditeur(app, conteneur, pid, d, loc) {
  const CM = window.CM;
  const s = {
    pid, id: d.id, titre: d.titre || '', langue: d.langue || langueActuelle(), conteneur, loc,
    modifie: false, ferme: false, distant: false, attente: null, attenteApercu: null, generation: 0,
    erreurs: new Set(), versionErreurs: 0, quarto: null,
    // what the references gave (plots, tables, Methods texts, models computed for the document)
    images: new Map(), tableaux: new Map(), methodes: new Map(), calculs: new Map(), urls: new Map(),
    // its citations: the references kept in it (BibTeX), their style, the files read, the keys not found
    bib: d.bib || '', style: STYLES.includes(d.style) ? d.style : STYLES[0], bibEnvoye: d.bib || '', styleEnvoye: d.style || STYLES[0],
    biblio: { signature: null, fichiers: [], version: 0, lecture: null, cache: null, date: 0, conflits: new Set(), dernierConflit: '' }, citesInconnues: new Set(),
  };
  ouverts.add(s);
  if (bureau && bureau.quartoDisponible) bureau.quartoDisponible().then((q) => { s.quarto = q; }).catch(() => { s.quarto = false; });

  const cadre = el('div', { class: 'document' });
  const corps = el('div', { class: 'corps-document' + (loc.sansApercu ? ' sans-apercu' : '') });
  s.etatEnregistrement = el('span', { class: 'etat-enregistrement' });
  s.etatTexte = el('span', { class: 'etat-document' });
  // the toolbar (drawn again when the interface changes language)
  s.construireBarre = () => {
    s.langueUI = langueActuelle();
    s.champTitre = el('input', { class: 'titre-document-champ', value: s.titre, placeholder: t('document_sans_titre'), 'aria-label': t('document_titre'),
      oninput: (e) => { s.titre = e.target.value; s.programmer(true); app.titrePanneau(cleDocument(pid, s.id), s.titre || t('document_sans_titre')); } });
    s.choixLangue = el('select', { title: t('document_langue_aide'), 'aria-label': t('document_langue_aide'),
      onchange: (e) => { s.langue = e.target.value; s.vue.contentDOM.setAttribute('lang', s.langue); s.programmer(true); } },
    [['fr', 'Français'], ['en', 'English']].map(([v, n]) => el('option', { value: v, selected: v === s.langue }, n)));
    const bouton = (ic, libelle, aide, action) => el('button', { title: aide, onclick: action }, icone(ic), libelle ? ` ${libelle}` : '');
    const barre = el('div', { class: 'outils barre-document' }, s.champTitre, s.choixLangue,
      bouton('quote', t('document_inserer'), t('document_inserer_aide'), (e) => ouvrirMenuSous(e.currentTarget, menuInserer(app, s))),
      bouton('book-open', t('document_citer'), t('document_citer_aide'), async (e) => {
        const b = e.currentTarget;
        await etatZotero();
        ouvrirMenuSous(b, menuCiter(app, s));
      }),
      bouton('file-output', t('document_compiler'), t('document_compiler_aide'), (e) => ouvrirMenuSous(e.currentTarget, menuCompiler(app, s))),
      bouton('columns-2', '', t('document_apercu_aide'), () => { loc.sansApercu = corps.classList.toggle('sans-apercu'); }),
      bouton('circle-help', '', t('document_aide'), () => dialogueAideDocument()),
      el('span', { class: 'espace' }), s.etatTexte, s.etatEnregistrement);
    if (s.barre) s.barre.replaceWith(barre); else cadre.prepend(barre);
    s.barre = barre;
  };
  const hote = el('div', { class: 'editeur-document' });
  s.apercu = el('div', { class: 'apercu-document', 'aria-live': 'polite' });
  corps.append(hote, s.apercu);
  cadre.append(corps);
  s.construireBarre();
  conteneur.replaceChildren(cadre);

  // the references in the text, and the citations: marked, in red when they can't be resolved
  // (not Quarto's {{< >}}, nor an address a@b.fr)
  const marques = CM.ViewPlugin.fromClass(class {
    constructor(v) { this.version = s.versionErreurs; this.decorations = this.construire(v); }
    update(u) {
      if (u.docChanged || u.viewportChanged || this.version !== s.versionErreurs) {
        this.version = s.versionErreurs;
        this.decorations = this.construire(u.view);
      }
    }
    construire(v) {
      const r = [];
      for (const { from, to } of v.visibleRanges) {
        for (const m of v.state.doc.sliceString(from, to).matchAll(/\{\{(?![<%])[^{}]*?\}\}/g)) {
          r.push(CM.Decoration.mark({ class: s.erreurs.has(m[0]) ? 'cm-ref cm-ref-erreur' : 'cm-ref' }).range(from + m.index, from + m.index + m[0].length));
        }
        for (const m of v.state.doc.sliceString(from, to).matchAll(CITATION_TEXTE)) {
          const debut = from + m.index + m[1].length;
          const cle = m[2].replace(/^-?@\{?|\}$/g, '');
          r.push(CM.Decoration.mark({ class: s.citesInconnues.has(cle) ? 'cm-cite cm-cite-erreur' : 'cm-cite' }).range(debut, debut + m[2].length));
        }
      }
      return CM.Decoration.set(r, true);
    }
  }, { decorations: (x) => x.decorations });

  s.vue = new CM.EditorView({
    parent: hote,
    state: CM.EditorState.create({
      doc: d.texte || '',
      // (the cursor where it was when the document was last open, else at its end)
      selection: { anchor: Math.min(Number.isInteger(loc.curseur) ? loc.curseur : Infinity, (d.texte || '').length) },
      extensions: [
        CM.history(), CM.drawSelection(), CM.highlightActiveLine(), CM.indentOnInput(), CM.EditorView.lineWrapping,
        CM.markdown({ base: CM.markdownLanguage }), CM.syntaxHighlighting(styleMarkdown()),
        CM.closeBrackets(),
        CM.autocompletion({ override: [completion(app, s), citationsDocument(app, s), citationsZotero(app, s)], icons: false, activateOnTyping: true }),
        CM.search({ top: true }), CM.highlightSelectionMatches(),
        CM.keymap.of([...CM.closeBracketsKeymap, ...CM.defaultKeymap, ...CM.searchKeymap, ...CM.historyKeymap, ...CM.completionKeymap, CM.indentWithTab]),
        CM.placeholder(t('document_placeholder')),
        CM.EditorView.contentAttributes.of({ spellcheck: 'true', lang: s.langue }),
        marques,
        CM.EditorView.updateListener.of((u) => {
          if (u.selectionSet) loc.curseur = u.state.selection.main.head;
          if (u.docChanged) { if (s.distant) s.programmerApercu(); else s.programmer(false); }
          if (u.focusChanged && u.view.hasFocus) dernier = s;
        }),
        // the application's shortcuts (undo in the project, delete a model...) stay out of the
        // text, except save (Ctrl+S) and the function keys
        CM.EditorView.domEventHandlers({ keydown: (e) => { if (!((e.ctrlKey || e.metaKey) && /^s$/i.test(e.key)) && !/^F\d+$/.test(e.key)) e.stopPropagation(); return false; } }),
      ],
    }),
  });
  dernier = s;
  s.vue.dispatch({ scrollIntoView: true });
  // links in the preview: web addresses in the browser, the document's own anchors in place
  s.apercu.addEventListener('click', (e) => {
    const a = e.target.closest('a[href]');
    if (!a) return;
    e.preventDefault();
    const h = a.getAttribute('href');
    if (/^(https?|mailto):/i.test(h)) window.open(h, '_blank');
    else if (h.startsWith('#')) { const c = s.apercu.querySelector(`[id="${CSS.escape(h.slice(1))}"]`); if (c) c.scrollIntoView(); }
  });
  // the preview follows the text as it scrolls
  s.vue.scrollDOM.addEventListener('scroll', () => {
    const x = s.vue.scrollDOM;
    s.apercu.scrollTop = (x.scrollTop / Math.max(1, x.scrollHeight - x.clientHeight)) * (s.apercu.scrollHeight - s.apercu.clientHeight);
  });

  s.programmerApercu = () => {
    clearTimeout(s.attenteApercu);
    s.attenteApercu = setTimeout(() => s.rafraichir(), 350);
  };
  // (a change kept: saved soon; `vite`: the title, the language)
  s.garder = (vite) => {
    s.modifie = true;
    s.etatEnregistrement.textContent = t('document_modifie');
    clearTimeout(s.attente);
    s.attente = setTimeout(() => s.enregistrer(), vite ? 100 : 800);
  };
  s.programmer = (vite) => {
    s.garder(vite);
    s.programmerApercu();
  };
  // the text kept in the project (it is saved with it), its references and their style when they
  // changed. `garder`: the window is closing.
  s.enregistrer = async (garder = false) => {
    clearTimeout(s.attente);
    if (!s.modifie || s.ferme) return;
    s.modifie = false;
    const q = { projet: pid, id: s.id, titre: s.titre, langue: s.langue, texte: s.vue.state.doc.toString() };
    if (s.bib !== s.bibEnvoye) q.bib = s.bib;
    if (s.style !== s.styleEnvoye) q.style = s.style;
    try {
      const r = await demander('document_enregistrer', q, { silencieux: true, garder });
      if (q.bib !== undefined) s.bibEnvoye = q.bib;
      if (q.style !== undefined) s.styleEnvoye = q.style;
      if (r.id !== s.id) { s.id = r.id; loc.doc = r.id; }  // (deleted meanwhile, in another window: it comes back)
      const avant = app.projet(pid) || {};
      // (the explorer is drawn again only when what it shows changes: a title, the project now modified)
      if (avant.modifie !== r.projet.modifie || JSON.stringify(avant.documents) !== JSON.stringify(r.projet.documents)) app.majProjet(r.projet);
      app.diffuser('document', { projet: pid, id: r.id });
      if (!s.modifie) s.etatEnregistrement.textContent = '';
    } catch {
      if (s.ferme) return;
      s.modifie = true;
      s.etatEnregistrement.textContent = t('document_non_enregistre');
    }
  };
  // the document as written in another window
  s.recharger = async () => {
    const x = await demander('document', { projet: pid, id: s.id }, { silencieux: true }).catch(() => null);
    if (!x || s.ferme || s.modifie) return;
    if ((x.texte || '') !== s.vue.state.doc.toString()) {
      s.distant = true;
      s.vue.dispatch({ changes: { from: 0, to: s.vue.state.doc.length, insert: x.texte || '' } });
      s.distant = false;
    }
    s.titre = x.titre || '';
    s.champTitre.value = s.titre;
    app.titrePanneau(cleDocument(pid, s.id), s.titre || t('document_sans_titre'));
    if (x.langue && x.langue !== s.langue) { s.langue = x.langue; s.choixLangue.value = x.langue; }
    if ((x.bib || '') !== s.bib) { s.bib = x.bib || ''; s.bibEnvoye = s.bib; s.biblio.cache = null; }
    if (x.style && x.style !== s.style) { s.style = x.style; s.styleEnvoye = x.style; }
    s.rafraichir();
  };
  s.rafraichir = () => mettreAJour(app, s);
  s.fermer = () => {
    s.ferme = true;
    clearTimeout(s.attente);
    clearTimeout(s.attenteApercu);
    ouverts.delete(s);
    if (dernier === s) dernier = null;
    for (const u of s.urls.values()) URL.revokeObjectURL(u);
    s.vue.destroy();
  };
  s.rafraichir();
  return s;
}

// ------------------------------------------------------------ references resolved

// The computed model a document means by `nom`: the last fit of that model of the project; when
// it has none, computed now (without opening its views) if `calculer`, else null.
async function modelePour(app, s, nom, calculer = true) {
  const p = app.projet(s.pid);
  const d = p.modeles.find((x) => x.nom === nom);
  const numero = (id) => Number(String(id).replace(/\D/g, '')) || 0;
  const calcules = app.modelesDe(s.pid).filter((m) => m.nom === nom || (d && m.numero === d.numero));
  if (calcules.length) return calcules.sort((a, b) => numero(b.id) - numero(a.id))[0];
  if (!d || !calculer) return null;
  if (!d.ajuste) throw new Error(t('ref_non_ajuste', { nom }));
  const cle = `${s.pid}|${d.numero}`;
  if (!s.calculs.has(cle)) s.calculs.set(cle, app.calculerDiscret(s.pid, d.numero).catch((e) => { s.calculs.delete(cle); throw e; }));
  return app.modele(await s.calculs.get(cle));
}

const garder = (carte, cle, valeur, max = 80) => {
  carte.set(cle, valeur);
  while (carte.size > max) carte.delete(carte.keys().next().value);
  return valeur;
};

// One reference resolved (rendu_document.js says what it gives) once its model is known. Plots,
// tables and the Methods text take time: unless `attendre`, those not yet at hand give null (the
// preview shows the text first, then them). What a reproducible Quarto file needs to write it in R
// comes too: a value's `forme` (references.js: formeReference), a plot's `vue` and `variante`, a
// table's `cle`.
async function resoudreUne(app, s, m, chemin, ref, attendre) {
  const parties = partiesModele(m);
  const partie = (cle, quoi) => parties.find((x) => x.cle === cle && x[quoi]);
  const erreur = (cle) => ({ erreur: t(cle, { chemin: `${m.nom}.${chemin.join('.')}` }) });
  if (chemin[0] === 'methodes' && chemin.length <= 2) {
    const cle = `${m.id}|${s.langue}`;
    if (!s.methodes.has(cle) && !attendre) return null;
    const r = s.methodes.get(cle) || garder(s.methodes, cle, await demander('methodes', { modele: m.id, langue: s.langue, version: VERSION }));
    return { genre: 'texte', paragraphes: [].concat(chemin[1] === 'references' ? r.references : r.paragraphes).filter(Boolean) };
  }
  if (chemin[0] === 'resultats' && chemin.length === 1) {
    const cle = `${m.id}|${s.langue}|resultats`;
    if (!s.methodes.has(cle) && !attendre) return null;
    const r = s.methodes.get(cle) || garder(s.methodes, cle, await demander('resultats_texte', { modele: m.id, langue: s.langue }));
    return { genre: 'texte', paragraphes: [].concat(r.paragraphes).filter(Boolean) };
  }
  if (chemin[0] === 'table') {
    const x = partie(chemin[1], 'tableau');
    if (!x || chemin.length !== 2) return erreur('ref_tableau_inconnu');
    const cle = `${m.id}|${x.cle}`;
    if (!s.tableaux.has(cle) && !attendre) return null;
    const blocs = s.tableaux.get(cle) || garder(s.tableaux, cle, await x.tableau(m));
    return { genre: 'tableau', blocs: blocs.filter((b) => b.texte !== undefined || b.entetes), nom: t(x.cle), cle: x.cle };
  }
  const x = partie(chemin[0], 'vue');
  if (x && (chemin.length === 1 || x.variantes)) {
    let variante = {};
    if (x.variantes) {
      // {{M5.effets.annee}}: a term's plot; {{M5.residus.qq}}: by the values of its settings
      const v = await x.variantes(m);
      const suite = chemin.slice(1);
      const texte = suite.join('.');
      variante = !suite.length ? v[0] : v.find((y) => y.terme === texte || y.categorie === texte)
        || v.find((y) => suite.every((c) => Object.values(y).map(String).includes(c)));
      if (!variante) return erreur('ref_variante_inconnue');
    }
    const cle = `${m.id}|${x.vue}|${JSON.stringify(variante)}|${JSON.stringify(app.locVue(x.vue, m.id) || {})}`;
    if (!s.images.has(cle) && !attendre) return null;
    const image = s.images.get(cle) || garder(s.images, cle, await imageVue(app, x.vue, m, variante), 40);
    if (!image) return erreur('ref_graphique_absent');
    return { genre: 'figure', image, largeur: LARGEUR, hauteur: HAUTEUR, nom: t(x.cle), vue: x.vue, variante };
  }
  const f = formeReference(m, chemin, ref);
  return f.erreur ? erreur(`ref_${f.erreur}`) : { genre: 'valeur', texte: texteForme(f, s.langue), forme: f };
}

// Every reference of the document resolved, in order (null: not yet, unless `attendre`); then the
// numbers of its figures and tables, and the cross-references to them ({{ref:label}}).
async function resoudre(app, s, analyse, attendre) {
  const p = app.projet(s.pid);
  const noms = p.modeles.map((d) => d.nom).concat(app.modelesDe(s.pid).map((m) => m.nom));
  const resolus = [];
  for (const tok of analyse.refs) {
    const ref = tok.meta.ref;
    if (/^ref:/.test(ref.cible)) { resolus.push({ renvoi: ref.cible.slice(4).trim() }); continue; }
    const c = separerCible(ref.cible, noms);
    if (!c) { resolus.push({ erreur: t('ref_modele_inconnu', { cible: ref.cible }) }); continue; }
    try {
      const m = await modelePour(app, s, c.modele, attendre);
      const r = m ? await resoudreUne(app, s, m, c.chemin, ref, attendre) : null;
      resolus.push(!r || r.erreur ? r : { ...r, legende: ref.legende, etiquette: ref.etiquette, modele: m.id, chemin: c.chemin });
    } catch (e) {
      resolus.push({ erreur: e.message });
    }
  }
  // (a plot, a table or the Methods text in a sentence: it goes alone on its line)
  analyse.refs.forEach((tok, i) => { if (resolus[i] && BLOCS.includes(resolus[i].genre) && !tok.meta.bloc) resolus[i] = { erreur: t('document_seul') }; });
  const { numeros, etiquettes } = numeroter(analyse.refs.map((tok, i) => ({ etiquette: tok.meta.ref.etiquette, r: resolus[i] })),
    (x) => (x.r && (x.r.genre === 'figure' || x.r.genre === 'tableau') ? x.r.genre : null));
  for (const [i, n] of numeros) resolus[i].numero = n;
  resolus.forEach((r, i) => {
    if (!r || r.renvoi === undefined) return;
    const e = etiquettes.get(r.renvoi);
    resolus[i] = e ? { genre: 'renvoi', cible: r.renvoi, texte: `${mots(s.langue)[e.genre]} ${e.numero}` }
      : attendre || resolus.every((x) => x) ? { erreur: t('ref_etiquette_inconnue', { etiquette: r.renvoi }) } : null;
  });
  return resolus;
}

// The preview drawn again from the text (the latest call wins): the values first, then the plots,
// tables and texts as they come.
async function mettreAJour(app, s) {
  if (s.ferme || !md) return;
  const generation = ++s.generation;
  const texte = s.vue.state.doc.toString();
  const analyse = analyser(md, texte);
  const [premiers, citations] = await Promise.all([resoudre(app, s, analyse, false), citer(app, s, analyse).catch(() => null)]);
  let resolus = premiers;
  if (generation !== s.generation || s.ferme) return;
  dessinerApercu(s, analyse, resolus, texte, citations);
  if (resolus.some((r) => !r)) {
    resolus = await resoudre(app, s, analyse, true);
    if (generation !== s.generation || s.ferme) return;
    dessinerApercu(s, analyse, resolus, texte, citations);
  }
}

function dessinerApercu(s, analyse, resolus, texte, citations) {
  const haut = s.apercu.scrollTop;
  // (the plots as blob: addresses, not their whole PNG in the page at every keystroke)
  const pourApercu = resolus.map((r) => (r && r.genre === 'figure' ? { ...r, image: urlApercu(s, r.image) } : r));
  s.apercu.innerHTML = versHTML(md, analyse, pourApercu, libelles(s.langue), citations);
  s.apercu.scrollTop = haut;
  s.erreurs = new Set(analyse.refs.filter((_, i) => resolus[i] && resolus[i].erreur).map((tok) => tok.meta.brut));
  s.citesInconnues = new Set(citations ? citations.inconnues : []);
  s.versionErreurs += 1;
  s.vue.dispatch({});
  const n = analyse.refs.length;
  const erreurs = resolus.filter((r) => r && r.erreur).length + s.citesInconnues.size;
  const nc = analyse.citations.length;
  const motsTexte = texte.replace(/^---[\s\S]*?\n(---|\.\.\.)/, '').replace(/\{\{[^{}]*\}\}/g, ' x ').replace(/[#*_`>|[\]()~-]/g, ' ').split(/\s+/).filter(Boolean).length;
  s.etatTexte.replaceChildren(t('document_mots', { n: motsTexte }), n ? ` · ${t('document_refs', { n })}` : '', nc ? ` · ${t('document_citations', { n: nc })}` : '',
    erreurs ? el('span', { class: 'erreur' }, ` · ${t('document_refs_erreurs', { n: erreurs })}`) : '');
}

function urlApercu(s, image) {
  let u = s.urls.get(image);
  if (!u) {
    const b = atob(String(image).split(',')[1] || '');
    u = URL.createObjectURL(new Blob([Uint8Array.from(b, (c) => c.charCodeAt(0))], { type: 'image/png' }));
    s.urls.set(image, u);
    if (s.urls.size > 60) {
      const [k, v] = s.urls.entries().next().value;
      URL.revokeObjectURL(v);
      s.urls.delete(k);
    }
  }
  return u;
}

// ------------------------------------------------------------ completion

const GENRES_COMPLETION = { valeur: 'variable', tableau: 'type', graphique: 'function', texte: 'text' };
const VALEURS_COURANTES = ['Q2', 'R2X', 'R2Y', 'N', 'A', 'gain', 'Q2_vide', 'RMSEcv'];

// What follows {{ : the models of the project, then what each offers (with its value now), or the
// labels of figures and tables after "ref:".
function completion(app, s) {
  return async (contexte) => {
    const avant = contexte.matchBefore(/\{\{[^{}]*/);
    if (!avant || /^\{\{\s*[<%]/.test(avant.text)) return null;
    const interieur = avant.text.slice(2);
    const debut = avant.from + 2 + (interieur.length - interieur.trimStart().length);
    const tape = interieur.trimStart();
    const CM = window.CM;
    if (/^ref:/.test(tape)) {
      const etiquettes = [...new Set([...s.vue.state.doc.toString().matchAll(/\{\{[^{}]*?\s#([\p{L}\p{N}_-]+)[^{}]*\}\}/gu)].map((m) => m[1]))];
      return { from: debut, options: etiquettes.map((e) => ({ label: `ref:${e}`, type: 'keyword', detail: t('document_renvoi') })), validFor: /^[^{}|#]*$/ };
    }
    if (/[|#]/.test(tape) || /:(%\d?|\d)$/.test(tape)) return null;
    const p = app.projet(s.pid);
    const noms = [...new Set(parNumero(p.modeles).map((d) => d.nom).concat(app.modelesDe(s.pid).map((m) => m.nom)))];
    const nom = noms.filter((n) => tape.startsWith(`${n}.`)).sort((a, b) => b.length - a.length)[0];
    if (!nom) {
      // (a model chosen: its name and a dot, then what it offers)
      const suite = (label) => (vue, _c, from, to) => {
        vue.dispatch({ changes: { from, to, insert: label }, selection: { anchor: from + label.length } });
        CM.startCompletion(vue);
      };
      return { from: debut, validFor: /^[^{}|#:.]*$/, options: [...noms.map((n) => {
        const d = p.modeles.find((x) => x.nom === n);
        return { label: n, type: 'class', detail: d ? [d.type, d.titre].filter(Boolean).join(' · ') : '', apply: suite(`${n}.`) };
      }), { label: 'ref:', type: 'keyword', detail: t('document_renvoi'), apply: suite('ref:') }] };
    }
    const m = await modelePour(app, s, nom, false).catch(() => null);
    let offre;
    if (m) {
      const parties = partiesModele(m);
      offre = offreModele(m, { vues: parties.filter((x) => x.vue).map((x) => x.cle), tableaux: parties.filter((x) => x.tableau).map((x) => x.cle), langue: s.langue })
        .map((o) => ({ ...o, detail: o.type === 'graphique' || o.type === 'tableau' ? t(o.detail) : o.type === 'texte' ? t(o.chemin === 'resultats' ? 'document_resultats' : 'document_methodes') : o.detail }));
    } else {
      offre = [...VALEURS_COURANTES.map((c) => ({ chemin: c, type: 'valeur', detail: t('document_non_calcule') })),
        { chemin: 'methodes', type: 'texte', detail: t('document_methodes') }, { chemin: 'resultats', type: 'texte', detail: t('document_resultats') }];
    }
    // (in the order given: the model's main values first, then by variable, tables, plots;
    // boost keeps it among the options that match as well)
    return { from: debut, validFor: /^[^{}|#]*$/, options: offre.map((o, i) => ({ label: `${nom}.${o.chemin}`, detail: o.detail, type: GENRES_COMPLETION[o.type] || 'variable',
      boost: Math.max(-99, 99 - i),
      // (a plot or a table: its caption follows, after |)
      apply: o.type === 'graphique' || o.type === 'tableau' ? `${nom}.${o.chemin} | ` : undefined })) };
  };
}

// ------------------------------------------------------------ citations

// A citation in the text, for the editor's marks: what comes before it (start of line, space, [ ; ( )
// and the key (-@wold2001, @wold2001, @{a key}).
const CITATION_TEXTE = /(^|[\s[;(])(-?@(?:\{[^{}\n]+\}|[\p{L}\p{N}_](?:[\p{L}\p{N}_]|[:.#$%&+?<>~/-](?=[\p{L}\p{N}_]))*))/gu;
const nomFichier = (chemin) => String(chemin).split(/[\\/]/).pop();

// The references the document can cite: those kept in it (s.bib), then those of the files its
// header names (bibliography: a file or a list, as Quarto reads them), which win (they may have
// changed since). The files are read again when the header changes, or when `forcer`. A file that
// gives the key of a reference kept from Zotero to another work (two libraries' keys alike) is said
// once, in the status bar: the file's is the one cited.
async function bibliotheque(app, s, { forcer = false } = {}) {
  const chemins = listeEntete(lireEntete(s.vue.state.doc.toString()).entete.bibliography);
  const signature = JSON.stringify(chemins);
  const b = s.biblio;
  if (forcer || b.signature !== signature) {
    b.signature = signature;
    b.lecture = Promise.all(chemins.map(async (chemin) => {
      try {
        const f = await demander('bibliographie_fichier', { projet: s.pid, chemin }, { silencieux: true });
        const r = lireBibliographie(f.texte, f.chemin);
        return { chemin, entrees: r.entrees, erreurs: r.erreurs };
      } catch (e) {
        return { chemin, entrees: [], erreurs: [], erreur: e.message };
      }
    })).then((lus) => {
      if (b.signature !== signature) return;
      b.fichiers = lus;
      b.date = Date.now();
      b.version += 1;
    });
  }
  await b.lecture;
  if (b.cache && b.cache.bib === s.bib && b.cache.version === b.version) return b.cache.entrees;
  const entrees = new Map(lireBibtex(s.bib).entrees.map((e) => [e.cle, { ...e, origine: null }]));
  const conflits = [];
  for (const f of b.fichiers) {
    for (const e of f.entrees) {
      const x = entrees.get(e.cle);
      const dit = `${e.cle}|${f.chemin}`;
      if (x && !x.origine && x.zotero && !memeOeuvre(x, e) && !b.conflits.has(dit)) {
        b.conflits.add(dit);
        conflits.push(t('zotero_conflit_fichier', { cle: e.cle, fichier: nomFichier(f.chemin), bibliotheque: nomBibliotheque(x.zotero) }));
      }
      entrees.set(e.cle, { ...e, origine: f.chemin });
    }
  }
  if (conflits.length) {
    b.dernierConflit = conflits.join(' ');
    app.statut(b.dernierConflit, true);
  }
  b.cache = { bib: s.bib, version: b.version, entrees };
  return entrees;
}

// The document's citations written in its style (formaterCitations), with the entries they came
// from; null when it cites nothing. A key not found has the files read again (a reference just
// added to Zotero, which Better BibTeX exports to the .bib), at most every 5 seconds.
async function citer(app, s, analyse) {
  if (!analyse.citations.length) return null;
  const groupes = analyse.citations.map((tok) => tok.meta);
  let entrees = await bibliotheque(app, s);
  let c = formaterCitations(groupes, entrees, { style: s.style, langue: s.langue });
  if (c.inconnues.length && s.biblio.fichiers.length && Date.now() - s.biblio.date > 5000) {
    entrees = await bibliotheque(app, s, { forcer: true });
    c = formaterCitations(groupes, entrees, { style: s.style, langue: s.langue });
  }
  garderCitees(s, c, entrees);
  return { ...c, entrees };
}

// The references cited from files, kept in the document too, as BibTeX (a project sent to someone
// who hasn't the .bib still cites them): added, or brought up to date when their file changed; not
// over another work kept from Zotero under the same key (bibliotheque() says so).
function garderCitees(s, citations, entrees) {
  const stock = new Map(lireBibtex(s.bib).entrees.map((e) => [e.cle, e]));
  let change = false;
  for (const { cle } of citations.liste) {
    const e = entrees.get(cle);
    if (!e || !e.origine) continue;
    const garde = stock.get(cle);
    if (garde && garde.zotero && !memeOeuvre(garde, e)) continue;
    const texte = versBibtex(e);
    if (!stock.has(cle) || versBibtex(stock.get(cle)) !== texte) {
      stock.set(cle, lireBibtex(texte).entrees[0]);
      change = true;
    }
  }
  if (!change) return;
  s.bib = [...stock.values()].map((e) => versBibtex(e)).join('\n\n');
  s.garder();
}

// References pasted in the document: kept in its BibTeX, a key already there replaced.
function ajouterAuDocument(s, liste) {
  const stock = new Map(lireBibtex(s.bib).entrees.map((e) => [e.cle, e]));
  for (const e of liste) stock.set(e.cle, lireBibtex(versBibtex(e)).entrees[0]);
  s.bib = [...stock.values()].map((e) => versBibtex(e)).join('\n\n');
  s.garder();
}

// References added from Zotero, from any of its libraries: kept in the document's BibTeX with where
// they are in Zotero, each placed among the document's references (`entrees`, its files' included)
// by placerReference: the same item or the same work is cited under the key the document has for
// it; another work whose key the document already uses (the same key in two libraries) gets that key
// with a letter. Returns { cles: the key each one is cited under, in order, notes: what to say of
// those cited under another key than Zotero's (dire), alerte: one of them took a letter }.
function ajouterZotero(s, liste, entrees) {
  const stock = new Map(lireBibtex(s.bib).entrees.map((e) => [e.cle, e]));
  const toutes = new Map(entrees);
  const notes = [];
  let alerte = false;
  let change = false;
  const cles = liste.map((e) => {
    const p = placerReference(e, toutes);
    if (p.nouvelle || p.remplace) {
      const x = lireBibtex(versBibtex({ ...e, cle: p.cle })).entrees[0];
      change = change || !stock.has(p.cle) || versBibtex(stock.get(p.cle)) !== versBibtex(x);
      stock.set(p.cle, x);
      toutes.set(p.cle, { ...x, origine: null });
    }
    if (p.renommee) {
      notes.push(t('zotero_renommee', { cle: p.renommee, nouvelle: p.cle, bibliotheque: nomBibliotheque(e.zotero) }));
      alerte = true;
    } else if (p.autre) notes.push(t('zotero_deja_sous', { cle: p.autre, existante: p.cle }));
    return p.cle;
  });
  if (change) {
    s.bib = [...stock.values()].map((e) => versBibtex(e)).join('\n\n');
    s.garder();
  }
  return { cles, notes, alerte };
}

// Text typed at the cursor (never in the YAML header). `citation`: a space before it when it
// would touch the word or the citation before ([@a][@b] reads as a link, not two citations).
function insererTexte(s, texte, citation = false) {
  const v = s.vue;
  let { from, to } = v.state.selection.main;
  const entete = lireEntete(v.state.doc.toString()).brut.length;
  if (from < entete) from = to = entete;
  const avant = from > 0 ? v.state.doc.sliceString(from - 1, from) : '';
  const t = citation && avant && !/[\s([]/.test(avant) ? ` ${texte}` : texte;
  v.dispatch({ changes: { from, to, insert: t }, selection: { anchor: from + t.length } });
  v.focus();
}

// Zotero on this computer (zotero.js), seen again every 20 seconds at most: its local API (Zotero 7
// and later, when "Allow other applications on this computer to communicate with Zotero" is
// ticked), the libraries it has (My Library, users/0, then the group libraries it syncs,
// groups/<id>; `sansGroupes` when it doesn't list them: My Library alone), and Better BibTeX's
// picker. The library searched, or one of its collections, is chosen in Insert a citation and kept
// (`source`: "groups/<id>|<collection key>"); typing @ searches that library.
const zotero = { etat: null, date: 0, recherches: new Map(), source: lire('zotero_source') || '' };
async function etatZotero(forcer = false) {
  if (!bureau || !bureau.zotero) return (zotero.etat = { bureau: false });
  if (!forcer && zotero.etat && Date.now() - zotero.date < 20000) return zotero.etat;
  const [api, bbt, groupes] = await Promise.all([bureau.zotero('/api/users/0/items/top?limit=1&format=keys', 3000), bureau.zotero('/better-bibtex/cayw?probe=1', 3000),
    bureau.zotero('/api/users/0/groups?format=json', 3000)]).catch(() => [{ erreur: 'absent' }, {}, {}]);
  const z = { bureau: true, api: api.statut === 200, bbt: bbt.statut === 200 && /ready/i.test(bbt.texte || ''),
    absent: api.erreur === 'absent', ferme: api.statut === 403, ancien: api.statut === 404, bibliotheques: [{ id: 'users/0' }] };
  if (z.api) {
    let l = null;
    try { l = groupes.statut === 200 ? JSON.parse(groupes.texte) : null; } catch { l = null; }
    if (Array.isArray(l)) {
      z.bibliotheques.push(...l.filter((g) => g && Number.isInteger(g.id)).map((g) => ({ id: `groups/${g.id}`, nom: String((g.data && g.data.name) || g.id) }))
        .sort((a, b) => a.nom.localeCompare(b.nom)));
    } else z.sansGroupes = groupes.statut === 404 ? 'ancien' : 'erreur';
  }
  zotero.etat = z;
  zotero.date = Date.now();
  return z;
}
// The library searched and its collection ('': the whole library): the one chosen, while Zotero
// still has that library, else My Library.
function sourceZotero(z = zotero.etat) {
  const [b, c = ''] = String(zotero.source).split('|');
  return z && z.bibliotheques && z.bibliotheques.some((x) => x.id === b) ? { bibliotheque: b, collection: /^[A-Z0-9]{8}$/.test(c) ? c : '' }
    : { bibliotheque: 'users/0', collection: '' };
}
// A Zotero library's name, from its id or one of its items' (groups/<id>/items/<key>).
function nomBibliotheque(id) {
  const b = (/^(users\/0|groups\/\d+)/.exec(String(id || '')) || [])[1];
  if (!b) return 'Zotero';
  if (b === 'users/0') return t('zotero_ma_bibliotheque');
  const x = zotero.etat && zotero.etat.bibliotheques && zotero.etat.bibliotheques.find((y) => y.id === b);
  return x ? x.nom : t('zotero_groupe', { id: b.slice(7) });
}
function messageZotero(z) {
  if (!z || !z.bureau) return t('zotero_bureau');
  if (z.api) return t(z.bbt ? 'zotero_connecte_bbt' : 'zotero_connecte', { bibliotheque: nomBibliotheque(sourceZotero(z).bibliotheque) });
  if (z.ferme) return t('zotero_ferme');
  if (z.ancien) return t('zotero_ancien');
  return t('zotero_absent');
}
// The notes of ajouterZotero, in the status bar (as a warning when a key took a letter).
const dire = (app, notes, alerte) => { if (notes.length) app.statut(notes.join(' '), alerte); };
// "kept in the document", and from which group when it came from one.
const dansDocument = (e) => (/^groups\//.test(e.zotero || '') ? `${t('citer_dans_document')} · ${nomBibliotheque(e.zotero)}` : t('citer_dans_document'));
// Zotero's references that answer a search (title, authors, year), in one of its libraries or in
// one of their collections (its key: then all of its references when the search is empty): each read
// from its BibTeX, its key the one Zotero exports (Better BibTeX's when it is installed), with where
// it is in Zotero (format=json&include=bibtex: each item's own export, about 4 ms each). Kept a minute.
function chercherZotero(q, { bibliotheque = 'users/0', collection = '' } = {}) {
  const cle = `${bibliotheque}|${collection}|${q.trim().toLowerCase()}`;
  const garde = zotero.recherches.get(cle);
  if (garde && Date.now() - garde.date < 60000) return garde.promesse;
  const base = collection ? `/api/${bibliotheque}/collections/${collection}/items/top?` : `/api/${bibliotheque}/items/top?`;
  const recherche = q.trim() ? `q=${encodeURIComponent(q.trim())}&qmode=titleCreatorYear&` : '';
  const promesse = bureau.zotero(`${base}${recherche}itemType=-attachment&itemType=-note&format=json&include=bibtex&limit=${collection ? 300 : 25}`, 15000)
    .then((r) => (r.statut === 200 ? lireReponseZotero(r.texte) : []), () => []);
  zotero.recherches.set(cle, { date: Date.now(), promesse });
  if (zotero.recherches.size > 100) zotero.recherches.delete(zotero.recherches.keys().next().value);
  return promesse;
}
// A library's collections, with their path ("Thèse › Perdrix") and how many references each holds.
async function collectionsZotero(bibliotheque = 'users/0') {
  const r = await bureau.zotero(`/api/${bibliotheque}/collections?format=json&limit=1000`, 8000);
  let l = [];
  try { l = r.statut === 200 ? JSON.parse(r.texte) : []; } catch { l = []; }
  const par = new Map(l.map((c) => [c.key, c]));
  const chemin = (c) => {
    const noms = [];
    for (let x = c, n = 0; x && n < 12; x = par.get(x.data.parentCollection), n += 1) noms.unshift(x.data.name);
    return noms.join(' › ');
  };
  return l.map((c) => ({ cle: c.key, nom: chemin(c), n: (c.meta && c.meta.numItems) || 0 })).sort((a, b) => a.nom.localeCompare(b.nom));
}
// Better BibTeX's picker, in Zotero: the references chosen there, with the page, prefix and suffix
// given there, cited here and kept in the document. Their entries come from Better BibTeX as
// CSL-JSON, one demand per library (the user's, a group's): titles as typed in Zotero, where its
// BibTeX would have them in title case.
async function choisirDansZotero(app, s) {
  app.statut(t('zotero_attente'));
  const r = await bureau.zotero('/better-bibtex/cayw?format=json&minimize=true', 15 * 60000);
  if (bureau.focusFenetre) bureau.focusFenetre('moi');
  if (r.statut !== 200) { app.statut(r.erreur === 'delai' ? t('zotero_delai') : messageZotero(await etatZotero(true)), true); return; }
  let choix = [];
  try { choix = [].concat(JSON.parse(r.texte || '[]')).filter((c) => c && c.citationKey); } catch { choix = []; }
  app.statut('');
  if (!choix.length) return;
  // (Better BibTeX's keys are unique in a library, not across them: each entry is found again by its
  // library and key, with its item's place in Zotero)
  const bib = (c) => (Number.isInteger(c.item && c.item.libraryID) ? c.item.libraryID : 1);
  const parBibliotheque = new Map();
  for (const c of choix) parBibliotheque.set(bib(c), [...(parBibliotheque.get(bib(c)) || []), c]);
  const trouvees = new Map();
  await Promise.all([...parBibliotheque].map(async ([b, cs]) => {
    const cles = [...new Set(cs.map((c) => c.citationKey))];
    const x = await bureau.zotero(`/better-bibtex/export/item?citationKeys=${encodeURIComponent(cles.join(','))}&translator=${encodeURIComponent('Better CSL JSON')}&libraryID=${b}`, 15000);
    const ou = new Map(cs.map((c) => [c.citationKey, elementZotero(c.uri || (c.item && c.item.uri))]));
    if (x.statut === 200) for (const e of lireCslJson(x.texte).entrees) trouvees.set(`${b}|${e.cle}`, { ...e, zotero: ou.get(e.cle) || '' });
  }));
  const lues = choix.map((c) => trouvees.get(`${bib(c)}|${c.citationKey}`));
  const { cles, notes, alerte } = ajouterZotero(s, lues.filter(Boolean), await bibliotheque(app, s));
  let i = 0;
  const citees = choix.map((c, k) => (lues[k] ? { ...c, citationKey: cles[i++] } : c));
  const manquent = lues.filter((e) => !e).length;
  dire(app, manquent ? [...notes, t('zotero_incomplet', { n: manquent })] : notes, alerte || manquent > 0);
  insererTexte(s, `[${citees.map(citationChoisie).join('; ')}]`, true);
}
// A reference picked in Better BibTeX's picker as Pandoc writes it: "see -@a, p. 3 and after".
function citationChoisie(c) {
  const terme = !c.label || c.label === 'page' ? 'p.' : c.label;
  return [c.prefix, `${c.suppressAuthor ? '-' : ''}@${c.citationKey}${c.locator ? `, ${terme} ${c.locator}` : ''}`, c.suffix]
    .filter(Boolean).join(' ');
}

// What follows @ (after a space or [ ; ( ): the document's references, by key, author, year or
// title; then Zotero's (citationsZotero, after a pause in the typing).
const avantArobase = (contexte) => {
  const avant = contexte.matchBefore(/(?:^|[\s[;(])-?@[^\s[\];,(){}]*/u);
  if (!avant) return null;
  const at = avant.text.lastIndexOf('@');
  return { debut: avant.from + at + 1, q: avant.text.slice(at + 1) };
};
function citationsDocument(app, s) {
  return async (contexte) => {
    const a = avantArobase(contexte);
    if (!a) return null;
    const entrees = await bibliotheque(app, s);
    if (contexte.aborted) return null;
    const q = a.q.toLowerCase();
    const options = [...entrees.values()].filter((e) => !q || e.cle.toLowerCase().startsWith(q) || correspond(e, a.q))
      .slice(0, 80).map((e) => ({ label: e.cle, detail: resume(e, s.langue), type: 'text' }));
    return options.length ? { from: a.debut, options, filter: false } : null;
  };
}
// (in the library chosen in Insert a citation; those the document has already, the same item or the
// same work, are among its own; another work under a key the document uses comes with its letter)
function citationsZotero(app, s) {
  return async (contexte) => {
    const a = avantArobase(contexte);
    if (!a || a.q.length < 3 || !bureau || !bureau.zotero) return null;
    await new Promise((r) => setTimeout(r, 300));
    const z = await etatZotero();
    if (contexte.aborted || !z.api) return null;
    const entrees = await bibliotheque(app, s);
    const { bibliotheque: b } = sourceZotero(z);
    const trouvees = (await chercherZotero(a.q, { bibliotheque: b })).map((e) => ({ e, p: placerReference(e, entrees) })).filter((x) => x.p.nouvelle);
    if (contexte.aborted || !trouvees.length) return null;
    const ou = b === 'users/0' ? 'Zotero' : `Zotero · ${nomBibliotheque(b)}`;
    return { from: a.debut, filter: false, options: trouvees.map(({ e, p }) => ({ label: p.cle, detail: `${resume(e, s.langue)} · ${ou}`, type: 'class',
      apply: (vue, _c, from, to) => {
        const { cles, notes, alerte } = ajouterZotero(s, [e], entrees);
        vue.dispatch({ changes: { from, to, insert: cles[0] }, selection: { anchor: from + cles[0].length } });
        dire(app, notes, alerte);
      } })) };
  };
}

function menuCiter(app, s) {
  const z = zotero.etat;
  const fichiers = s.biblio.fichiers.map((f) => ({ libelle: `${nomFichier(f.chemin)}${deuxPoints()}${f.erreur ? t('citer_fichier_erreur') : t('citer_n', { n: f.entrees.length })}`,
    icone: f.erreur ? 'x-circle' : 'file-text', actif: false, action: () => {} }));
  return [
    { libelle: t('citer_inserer'), icone: 'book-open', action: () => dialogueCiter(app, s) },
    { libelle: t('zotero_choisir'), icone: 'search', actif: !!(z && z.bbt), action: () => choisirDansZotero(app, s) },
    '-',
    { libelle: t('citer_lier'), icone: 'file-plus', action: () => lierFichier(app, s) },
    { libelle: t('citer_coller'), icone: 'clipboard-paste', action: () => dialogueColler(app, s) },
    { libelle: t('citer_references'), icone: 'list', action: () => dialogueReferences(app, s) },
    { libelle: t('citer_actualiser'), icone: 'refresh-cw', action: async () => { zotero.recherches.clear(); await bibliotheque(app, s, { forcer: true }); s.rafraichir(); } },
    '-',
    { libelle: t('citer_style'), sous: STYLES.map((st) => ({ libelle: t(`citer_style_${st}`), coche: s.style === st, action: () => { s.style = st; s.programmer(true); } })) },
    '-',
    ...fichiers,
    { libelle: messageZotero(z), icone: z && z.api ? 'check' : 'info', actif: false, action: () => {} },
  ];
}

// Insert a citation: the document's references and Zotero's that answer a search, in one of its
// libraries (My Library, a group's) or one of their collections (with plain Zotero, what Better
// BibTeX's picker gives); those ticked cited together ([@a; @b]), or in the sentence (@a). The
// library chosen is kept, and typing @ searches it. Without its group libraries (Zotero didn't list
// them), My Library alone, as before groups were listed, and a sentence says so.
async function dialogueCiter(app, s) {
  const choisies = new Map();  // (by line: a key of the document's, or a Zotero item)
  const recherche = el('input', { type: 'search', class: 'recherche-citation', placeholder: t('citer_rechercher'), 'aria-label': t('citer_rechercher') });
  const choixSource = el('select', { class: 'collection-zotero', title: t('zotero_source_aide'), 'aria-label': t('zotero_source_aide') });
  const liste = el('div', { class: 'liste-citations' });
  const etat = el('p', { class: 'note' });
  const narratif = el('input', { type: 'checkbox' });
  let generation = 0;
  const majListe = async () => {
    const g = ++generation;
    const q = recherche.value.trim();
    const [lib, coll = ''] = (choixSource.value || 'users/0|').split('|');
    const entrees = await bibliotheque(app, s);
    const z = await etatZotero();
    // (a collection: all its references, those the document has already among them; else the
    // document's first, then Zotero's it hasn't yet)
    const locales = coll ? [] : [...entrees.values()].filter((e) => !q || correspond(e, q)).sort((a, b) => resume(a).localeCompare(resume(b)));
    const distantes = z.api && (coll || q.length >= 2) ? (await chercherZotero(q, { bibliotheque: lib, collection: coll }))
      .map((e) => ({ e, p: placerReference(e, entrees) })).filter((x) => coll || x.p.nouvelle) : [];
    if (g !== generation) return;
    const ou = lib === 'users/0' ? 'Zotero' : `Zotero · ${nomBibliotheque(lib)}`;
    const ligne = (id, e, source, cle, texte) => el('label', { class: 'ligne-citation' },
      el('input', { type: 'checkbox', checked: choisies.has(id), onchange: (ev) => { if (ev.target.checked) choisies.set(id, { e, source }); else choisies.delete(id); } }),
      el('span', { class: 'resume-citation', title: `@${cle}` }, resume(e, s.langue)),
      el('span', { class: 'source-citation' }, texte));
    liste.replaceChildren(...locales.slice(0, 80).map((e) => ligne(`d|${e.cle}`, e, 'document', e.cle, e.origine ? nomFichier(e.origine) : dansDocument(e))),
      ...distantes.map(({ e, p }) => ligne(`z|${e.zotero || e.cle}`, e, 'zotero', p.cle, p.nouvelle ? ou : t('citer_deja'))));
    if (!liste.childElementCount) liste.append(el('p', { class: 'note' }, q || coll ? t('citer_rien') : t('citer_vide')));
    etat.textContent = messageZotero(z);
  };
  recherche.addEventListener('input', () => { clearTimeout(recherche.attente); recherche.attente = setTimeout(majListe, 250); });
  choixSource.addEventListener('change', () => { zotero.source = choixSource.value; ecrire('zotero_source', zotero.source); majListe(); });
  const z = await etatZotero();
  if (z.api) {
    // each library, with its collections (with groups, under its name); the one chosen last time
    // while Zotero still has it
    const groupes = z.bibliotheques.length > 1;
    const collections = await Promise.all(z.bibliotheques.map((b) => collectionsZotero(b.id)));
    z.bibliotheques.forEach((b, i) => {
      const nom = nomBibliotheque(b.id);
      const options = [el('option', { value: `${b.id}|` }, groupes ? t('zotero_toute', { bibliotheque: nom }) : t('zotero_bibliotheque')),
        ...collections[i].map((c) => el('option', { value: `${b.id}|${c.cle}` }, `${groupes ? `${nom} › ` : ''}${c.nom} (${c.n})`))];
      choixSource.append(...(groupes ? [el('optgroup', { label: nom }, options)] : options));
    });
    const source = sourceZotero(z);
    choixSource.value = `${source.bibliotheque}|${source.collection}`;
    if (!choixSource.value) choixSource.value = `${source.bibliotheque}|`;
  }
  dialogue({ titre: t('citer_inserer').replace(/…$/, ''), large: true,
    contenu: el('div', { class: 'dialogue-citer' }, el('div', { class: 'ligne-recherche-citation' }, recherche, z.api ? choixSource : null),
      z.api && z.sansGroupes ? el('p', { class: 'note' }, t(`zotero_sans_groupes_${z.sansGroupes}`)) : null,
      liste, el('label', { class: 'champ' }, narratif, ` ${t('citer_narratif')}`), etat),
    boutons: [
      ...(z.bbt ? [{ libelle: t('zotero_choisir'), action: () => { choisirDansZotero(app, s); return true; } }] : []),
      { libelle: t('annuler'), action: () => true },
      { libelle: t('citer_bouton'), principal: true, action: () => {
        if (!choisies.size) return false;
        citerChoisies(app, s, [...choisies.values()], narratif.checked);
        return true;
      } }] });
  majListe();
}
// The references ticked in Insert a citation, cited at the cursor: Zotero's kept in the document
// first (ajouterZotero: under the key it gives them), each key once.
async function citerChoisies(app, s, choix, narratif) {
  const zot = choix.filter((x) => x.source === 'zotero').map((x) => x.e);
  const { cles, notes, alerte } = zot.length ? ajouterZotero(s, zot, await bibliotheque(app, s)) : { cles: [], notes: [] };
  let i = 0;
  const citees = [...new Set(choix.map((x) => `@${x.source === 'zotero' ? cles[i++] : x.e.cle}`))];
  insererTexte(s, narratif ? citees.join(', ') : `[${citees.join('; ')}]`, true);
  dire(app, notes, alerte);
}

// A bibliography file named in the header (bibliography:), relative to the project's folder when it
// is in it (Quarto reads it beside the document), its references then cited like the others.
async function lierFichier(app, s) {
  const chemin = bureau ? await bureau.choisirFichier([{ name: 'BibTeX, CSL-JSON', extensions: ['bib', 'bibtex', 'json'] }]) : window.prompt(t('citer_chemin'));
  if (!chemin) return;
  const unix = (x) => String(x).replace(/\\/g, '/');
  const p = app.projet(s.pid) || {};
  const dossier = p.chemin ? unix(p.chemin).replace(/\/[^/]*$/, '') : null;
  const valeur = dossier && unix(chemin).toLowerCase().startsWith(`${dossier.toLowerCase()}/`) ? unix(chemin).slice(dossier.length + 1) : unix(chemin);
  const texte = s.vue.state.doc.toString();
  const { entete, brut } = lireEntete(texte);
  const actuels = listeEntete(entete.bibliography);
  if (!actuels.includes(valeur)) {
    const nouveau = fixerEntete(brut || '', 'bibliography', actuels.length ? [...actuels, valeur] : valeur);
    s.vue.dispatch({ changes: { from: 0, to: brut.length, insert: nouveau } });
  }
  s.biblio.dernierConflit = '';
  await bibliotheque(app, s, { forcer: true });
  const f = s.biblio.fichiers.find((x) => x.chemin === valeur);
  // (with what bibliotheque() said of a key the file gives to another work than one kept from Zotero)
  const conflit = s.biblio.dernierConflit;
  app.statut([f && f.erreur ? `${nomFichier(valeur)}${deuxPoints()}${f.erreur}` : t('citer_lie', { fichier: nomFichier(valeur), n: f ? f.entrees.length : 0 }), conflit].filter(Boolean).join(' — '),
    !!(f && f.erreur) || !!conflit);
  s.rafraichir();
}

// References pasted as BibTeX (or CSL-JSON), kept in the document.
function dialogueColler(app, s) {
  const zone = el('textarea', { class: 'zone-bibtex', rows: 14, spellcheck: 'false', placeholder: '@article{wold2001,\n  author = {Wold, Svante and …},\n  …\n}' });
  dialogue({ titre: t('citer_coller').replace(/…$/, ''), large: true, contenu: el('div', { class: 'dialogue-citer' }, el('p', { class: 'note' }, t('citer_coller_note')), zone),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('citer_ajouter'), principal: true, action: () => {
      const r = lireBibliographie(zone.value);
      if (!r.entrees.length) { app.statut(t('citer_coller_rien'), true); return false; }
      ajouterAuDocument(s, r.entrees);
      app.statut(t('citer_coller_fait', { n: r.entrees.length }) + (r.erreurs.length ? ` ${t('citer_coller_erreurs', { n: r.erreurs.length })}` : ''), r.erreurs.length > 0);
      s.rafraichir();
      return true;
    } }] });
}

// The references the document can cite, where each comes from and whether it cites it; those kept
// in it and no longer cited can be removed.
async function dialogueReferences(app, s) {
  const entrees = await bibliotheque(app, s);
  const citees = new Set(analyser(md, s.vue.state.doc.toString()).citations.flatMap((tok) => tok.meta.cites.map((c) => c.cle)));
  const lignes = [...entrees.values()].sort((a, b) => resume(a).localeCompare(resume(b))).map((e) => el('div', { class: `ligne-reference${citees.has(e.cle) ? ' citee' : ''}` },
    el('code', {}, `@${e.cle}`), el('span', { class: 'resume-citation' }, resume(e, s.langue)),
    el('span', { class: 'source-citation' }, e.origine ? nomFichier(e.origine) : dansDocument(e))));
  const sources = [...s.biblio.fichiers.map((f) => `${f.chemin}${deuxPoints()}${f.erreur || t('citer_n', { n: f.entrees.length })}${f.erreurs.length ? ` (${t('citer_coller_erreurs', { n: f.erreurs.length })})` : ''}`),
    t('citer_n_document', { n: lireBibtex(s.bib).entrees.length })];
  const inutiles = lireBibtex(s.bib).entrees.filter((e) => !citees.has(e.cle));
  dialogue({ titre: t('citer_references').replace(/…$/, ''), large: true,
    contenu: el('div', { class: 'dialogue-citer' }, sources.map((x) => el('p', { class: 'note' }, x)),
      el('div', { class: 'liste-citations' }, lignes.length ? lignes : el('p', { class: 'note' }, t('citer_vide'))), el('p', { class: 'note' }, t('citer_references_note'))),
    boutons: [
      ...(inutiles.length ? [{ libelle: t('citer_retirer', { n: inutiles.length }), action: () => {
        const garder = lireBibtex(s.bib).entrees.filter((e) => citees.has(e.cle));
        s.bib = garder.map((e) => versBibtex(e)).join('\n\n');
        s.programmer(true);
        return true;
      } }] : []),
      { libelle: t('fermer'), principal: true, action: () => true }] });
}

// ------------------------------------------------------------ insert

// A reference typed at the cursor; a block (plot, table, Methods text) on a line of its own.
function inserer(s, cible, bloc) {
  const v = s.vue;
  let { from, to } = v.state.selection.main;
  const entete = lireEntete(v.state.doc.toString()).brut.length;  // (never in the YAML header)
  if (from < entete) from = to = entete;
  const ouvert = cible.endsWith('.') || cible.endsWith(':');
  let texte = `{{${cible}}}`;
  if (bloc) {
    const ligne = v.state.doc.lineAt(from);
    const avant = ligne.text.slice(0, from - ligne.from).trim() ? '\n\n' : '';
    texte = `${avant}${texte}\n\n`;
  }
  const curseur = ouvert ? from + texte.indexOf('}}') : from + texte.length;
  v.dispatch({ changes: { from, to, insert: texte }, selection: { anchor: curseur } });
  v.focus();
  if (ouvert) window.CM.startCompletion(v);
}

// From the explorer: into the document last written in (of that project), else its first,
// opened; a new one when the project has none.
async function insererDepuisExplorateur(app, pid, cible, bloc) {
  if (bloc && !cible.includes('|')) cible = `${cible} | ${t(cible.split('.').pop())}`;  // (a plot: its name as caption)
  let s = dernier && dernier.pid === pid && !dernier.ferme ? dernier : [...ouverts].find((x) => x.pid === pid);
  if (!s) {
    const docs = [].concat((app.projet(pid) || {}).documents || []);
    if (!docs.length) { dialogueNouveau(app, pid, () => insererDepuisExplorateur(app, pid, cible, bloc)); return; }
    app.ouvrirDocument(pid, docs[0].id);
    for (let i = 0; i < 50 && !s; i += 1) {
      await new Promise((r) => setTimeout(r, 100));
      s = [...ouverts].find((x) => x.pid === pid && x.id === docs[0].id);
    }
    if (!s) return;
  } else app.ouvrirDocument(pid, s.id);
  inserer(s, cible, bloc);
}

// (no separator first, last or twice)
const nettoyer = (items) => items.filter((x, i, a) => x !== '-' || (i > 0 && i < a.length - 1 && a[i - 1] !== '-'));

function menuInserer(app, s) {
  const p = app.projet(s.pid);
  const modeles = parNumero(p.modeles.filter((d) => d.ajuste));
  if (!modeles.length) return [{ libelle: t('document_aucun_modele'), actif: false, action: () => {} }];
  const items = modeles.map((d) => {
    const m = app.modelesDe(s.pid).filter((x) => x.numero === d.numero).pop();
    const ref = (chemin, bloc = false) => () => inserer(s, `${d.nom}.${chemin}`, bloc);
    let sous;
    if (m) {
      const parties = partiesModele(m);
      const offre = offreModele(m, { langue: s.langue });
      const valeurs = offre.filter((o) => o.type === 'valeur' && !/^(vip|coef)\./.test(o.chemin));
      const parVariable = (prefixe) => offre.filter((o) => o.chemin.startsWith(`${prefixe}.`));
      sous = nettoyer([...valeurs.map((o) => ({ libelle: `${o.chemin} = ${o.detail}`, action: ref(o.chemin) })),
        ...[['vip', 'VIP'], ['coef', t('coefficients')]].filter(([x]) => parVariable(x).length)
          .map(([x, n]) => ({ libelle: n, sous: parVariable(x).map((o) => ({ libelle: `${o.chemin.slice(x.length + 1)} = ${o.detail}`, action: ref(o.chemin) })) })),
        '-',
        ...parties.filter((x) => x.vue).map((x) => ({ libelle: t(x.cle), icone: 'chart-scatter', action: ref(`${x.cle} | ${t(x.cle)}`, true) })),
        '-',
        ...parties.filter((x) => x.tableau).map((x) => ({ libelle: `${t('document_tableau')} · ${t(x.cle)}`, icone: 'table-2', action: ref(`table.${x.cle} | ${t(x.cle)}`, true) })),
        '-',
        { libelle: t('document_methodes'), icone: 'quote', action: ref('methodes', true) },
        { libelle: t('document_resultats'), icone: 'quote', action: ref('resultats', true) }]);
    } else {
      sous = [...VALEURS_COURANTES.map((c) => ({ libelle: c, action: ref(c) })), '-',
        { libelle: t('document_methodes'), icone: 'quote', action: ref('methodes', true) },
        { libelle: t('document_resultats'), icone: 'quote', action: ref('resultats', true) }];
    }
    return { libelle: `${d.nom}${d.titre ? ` — ${d.titre}` : ''}`, sous };
  });
  return [...items, '-', { libelle: t('document_inserer_renvoi'), action: () => inserer(s, 'ref:', false) }];
}

// ------------------------------------------------------------ compile

function menuCompiler(app, s) {
  const quarto = !!s.quarto;
  return [
    { libelle: 'Word (.docx)…', icone: 'file-text', action: () => compiler(app, s, 'docx') },
    { libelle: 'PDF…', icone: 'file-down', action: () => compiler(app, s, 'pdf') },
    { libelle: 'HTML…', icone: 'file-code', action: () => compiler(app, s, 'html') },
    '-',
    { libelle: t('document_qmd'), icone: 'file-output', action: () => dialogueQuarto(app, s, null) },
    quarto ? { libelle: t('document_quarto'), icone: 'file-output',
      sous: Object.entries(SORTIES_QUARTO).map(([f, n]) => ({ libelle: n, action: () => dialogueQuarto(app, s, f) })) }
      : { libelle: t('document_quarto_absent'), icone: 'file-output', actif: false, action: () => {} },
  ];
}
const SORTIES_QUARTO = { docx: 'Word (.docx)', typst: 'PDF (Typst)', html: 'HTML' };

// The Quarto file's results: written in, or reproducible (R recomputes them from the raw data with
// each cited model's Export to R script, written beside the .qmd). The models cited whose type has
// no export to R yet are said here: their values stay written in. The choice is kept.
async function dialogueQuarto(app, s, quarto) {
  const p = app.projet(s.pid);
  const noms = p.modeles.map((d) => d.nom).concat(app.modelesDe(s.pid).map((m) => m.nom));
  const cites = [...new Set(analyser(md, s.vue.state.doc.toString()).refs.map((tok) => separerCible(tok.meta.ref.cible, noms)).filter(Boolean).map((c) => c.modele))];
  const sans = cites.length ? [].concat((await demander('export_r_possible', { projet: s.pid, noms: cites }).catch(() => ({}))).sans_export || []) : [];
  let reproductible = s.reproductible ?? lire('document_reproductible') === 'oui';
  const choix = (valeur, titre, aide) => el('label', { class: 'champ choix-quarto' },
    el('input', { type: 'radio', name: 'resultats-quarto', checked: reproductible === valeur, onchange: () => { reproductible = valeur; } }),
    el('span', {}, el('span', { class: 'titre-choix' }, titre), el('span', { class: 'note' }, aide)));
  dialogue({ titre: quarto ? `${t('document_quarto')} · ${SORTIES_QUARTO[quarto]}` : t('reproductible_titre'),
    contenu: el('div', { class: 'dialogue-quarto' },
      el('p', {}, t('reproductible_question')),
      choix(false, t('reproductible_non'), t('reproductible_non_aide')),
      choix(true, t('reproductible_oui'), t('reproductible_oui_aide')),
      sans.length ? el('p', { class: 'note avertissement' }, t('reproductible_sans_export', {
        modeles: sans.map((x) => `${x.nom} (${String(x.raison).replace(/\.$/, '').replace(/^\p{Lu}(?!\p{Lu})/u, (c) => c.toLowerCase())})`).join(' ; ') })) : null),
    boutons: [{ libelle: t('annuler'), action: () => true },
      { libelle: bureau ? t('choisir_emplacement') : t('telecharger'), principal: true, action: () => {
        s.reproductible = reproductible;
        ecrire('document_reproductible', reproductible ? 'oui' : 'non');
        compiler(app, s, 'qmd', quarto, reproductible);
        return true;
      } }] });
}

// A code chunk Quarto would run (```{r}, ```{python}..., or inline `r ...`).
const CODE_QUARTO = /^[ \t]*(\x60{3,}|~{3,})[ \t]*\{\s*(r|python|julia|bash|sh|ojs)\b|\x60r /mi;
const FILTRES = { docx: { name: 'Word', extensions: ['docx'] }, pdf: { name: 'PDF', extensions: ['pdf'] }, html: { name: 'HTML', extensions: ['html'] }, qmd: { name: 'Quarto', extensions: ['qmd'] } };
const octets = (texte) => new TextEncoder().encode(texte);

// The document written in `format` (docx, pdf, html, or qmd with its figures beside it), its
// references resolved now; then compiled by Quarto when `quarto` names its output. `reproductible`
// (qmd): R recomputes its results when Quarto renders it, with the cited models' Export to R
// scripts written beside it (in <name>_R); in the browser, all of it in a .zip.
async function compiler(app, s, format, quarto = null, reproductible = false) {
  await s.enregistrer();
  const base = (s.titre || t('document_sans_titre')).replace(/[^\p{L}\p{N} _.()-]+/gu, '_').trim() || 'document';
  const r = await app.occupe(async () => {
    const texte = s.vue.state.doc.toString();
    const analyse = analyser(md, texte);
    return { texte, analyse, resolus: await resoudre(app, s, analyse, true), citations: await citer(app, s, analyse) };
  });
  const erreurs = r.resolus.filter((x) => x.erreur).length + (r.citations ? r.citations.inconnues.length : 0);
  if (erreurs && !window.confirm(t('document_compiler_erreurs', { n: erreurs }))) return;
  // (Quarto runs the code chunks of a .qmd: a project received may hold some)
  if (quarto && CODE_QUARTO.test(r.texte) && !window.confirm(t('document_quarto_code'))) return;
  let chemin = null;
  if (bureau) {
    chemin = await bureau.enregistrerSous(`${base}.${format}`, [FILTRES[format]]);
    if (!chemin) return;
  }
  const lib = libelles(s.langue);
  let ouvrir = chemin;
  let rep = null;
  await app.occupe(async () => {
    if (format === 'docx') {
      const o = versDocx(r.analyse, r.resolus, { langue: s.langue, libelles: lib, citations: r.citations });
      if (bureau) await bureau.ecrireFichier(chemin, o); else telecharger(o, `${base}.docx`);
    } else if (format === 'pdf' || format === 'html') {
      const html = pageHTML(versHTML(md, r.analyse, r.resolus, lib, r.citations), s.titre || base, s.langue);
      if (format === 'pdf') { if (bureau) await bureau.imprimerPDF(html, chemin); else imprimer(html); }
      else if (bureau) await bureau.ecrireFichier(chemin, octets(html)); else telecharger(octets(html), `${base}.html`);
    } else {
      // the .qmd, its figures in a folder beside it (<name>_figures), then Quarto if asked
      const fichier = chemin ? chemin.split(/[\\/]/).pop() : `${base}.qmd`;
      const nom = fichier.replace(/\.qmd$/i, '');
      const dossier = `${nom}_figures`;
      // the references cited, in <name>.bib beside it; the header's CSL style copied there too
      const cites = r.citations ? r.citations.liste.map((x) => r.citations.entrees.get(x.cle)).filter(Boolean) : [];
      const bib = cites.length ? `${nom}.bib` : null;
      const cslEntete = String(lireEntete(r.texte).entete.csl || '');
      let csl = null;
      let texteCsl = null;
      if (cslEntete && !/^https?:/i.test(cslEntete)) {
        try {
          texteCsl = (await demander('bibliographie_fichier', { projet: s.pid, chemin: cslEntete })).texte;
          csl = cslEntete.split(/[\\/]/).pop();
        } catch (e) { app.statut(`${t('document_csl_absent')} ${e.message}`, true); }
      }
      const racine = chemin ? chemin.slice(0, chemin.length - fichier.length) : '';
      // reproducible: the engine writes each cited model's Export to R script and its data beside
      // the .qmd (in the browser, in a folder of its session), and says what the .qmd writes instead
      // of each reference
      if (reproductible) {
        app.statut(t('reproductible_en_cours'));
        rep = await demander('document_reproductible', { projet: s.pid, langue: s.langue, version: VERSION, nom, dossier: bureau ? racine : null,
          ...referencesReproductibles(r.resolus) });
      }
      const q = versQmd(r.texte, r.analyse, r.resolus, { fichierFigure: (x) => `${dossier}/figure-${x.numero}.png`, langue: s.langue, titre: s.titre || base,
        bibliographie: bib, csl, libelles: lib, reproductible: rep });
      const texteBib = bib ? `${cites.map((e) => versBibtex(e, SANS_ZOTERO)).join('\n\n')}\n` : null;
      const png = (x) => { const b = atob(x.image.split(',')[1]); return Uint8Array.from(b, (c) => c.charCodeAt(0)); };
      const figures = q.images.filter((x) => x.numero);
      if (!bureau) {
        if (!rep) { telecharger(octets(q.texte), fichier); return; }
        // (one .zip: the .qmd, its references, the figures R doesn't draw, the scripts and their data)
        const lus = await Promise.all([].concat(rep.fichiers).map(async (f) => new Uint8Array(await (await fetch(`/api/fichier?chemin=${encodeURIComponent(f)}`)).arrayBuffer())));
        telecharger(zip([[fichier, octets(q.texte)], ...(texteBib ? [[bib, octets(texteBib)]] : []), ...(texteCsl !== null ? [[csl, octets(texteCsl)]] : []),
          ...figures.map((x) => [`${dossier}/figure-${x.numero}.png`, png(x)]), ...[].concat(rep.relatifs).map((f, i) => [f, lus[i]])]), `${nom}.zip`);
        const note = noteReproductible(rep);
        if (note) app.statut(note, true);
        return;
      }
      const sep = chemin.includes('\\') ? '\\' : '/';
      if (texteBib) await bureau.ecrireFichier(`${racine}${bib}`, octets(texteBib));
      if (texteCsl !== null) await bureau.ecrireFichier(`${racine}${csl}`, octets(texteCsl));
      for (const x of figures) await bureau.ecrireFichier(`${racine}${dossier}${sep}figure-${x.numero}.png`, png(x));
      await bureau.ecrireFichier(chemin, octets(q.texte));
      if (quarto) {
        app.statut(t('document_quarto_en_cours'));
        // (Quarto runs the R of a reproducible .qmd with the engine's R when it has knitr and rmarkdown, unless QUARTO_R says another)
        const R = rep && rep.R && rep.R.knitr && rep.R.rmarkdown ? rep.R.bin : null;
        const sortie = await bureau.quarto(chemin, quarto, R ? { R } : {});
        if (sortie.erreur) throw new Error(sortie.erreur === 'quarto_absent' ? t('document_quarto_absent') : `Quarto${deuxPoints()}${sortie.erreur}`);
        ouvrir = sortie.sortie;
      }
    }
  });
  if (ouvrir && bureau) {
    const note = rep ? noteReproductible(rep) : '';
    app.statut(`${t('document_ecrit')} ${ouvrir}${note ? ` · ${note}` : ''}`, !!note);
    bureau.ouvrirChemin(ouvrir);
  }
}

// After a reproducible .qmd: how many of its references stay written in, and the models its
// scripts don't redo (its first chunk says why); '' when R recomputes them all.
function noteReproductible(rep) {
  const refs = [].concat(rep.references || []).filter(Boolean);
  const enDur = refs.filter((x) => x.raison).length;
  const sans = [].concat(rep.modeles || []).filter((x) => x.erreur).map((x) => x.nom);
  if (!enDur && !sans.length) return '';
  return t('reproductible_en_dur', { n: enDur, total: refs.length }) + (sans.length ? ` ${t('reproductible_modeles_en_dur', { modeles: sans.join(', ') })}` : '');
}

function telecharger(o, nom) {
  const a = el('a', { href: URL.createObjectURL(new Blob([o])), download: nom });
  a.click();
  setTimeout(() => URL.revokeObjectURL(a.href), 2000);
}
function imprimer(html) {
  const f = window.open('', '_blank');
  f.document.write(html);
  f.document.close();
  f.onload = () => f.print();
}

// ------------------------------------------------------------ new, rename, duplicate, delete

// A new document: blank, an article (IMRaD) or an analysis report, the latter two with the
// references of the models chosen already written in. `apres(id)`: once it exists.
function dialogueNouveau(app, pid, apres = null) {
  const p = app.projet(pid || app.projetActif());
  if (!p) { app.statut(t('choisir_projet'), true); return; }
  const st = { titre: '', langue: langueActuelle(), gabarit: 'vide', modeles: new Set() };
  const ajustes = parNumero(p.modeles.filter((d) => d.ajuste));
  const choixModeles = el('div', { class: 'modeles-gabarit' }, el('p', { class: 'note' }, t('document_modeles_gabarit')),
    ajustes.length ? ajustes.map((d) => el('label', { class: 'champ' },
      el('input', { type: 'checkbox', onchange: (e) => { if (e.target.checked) st.modeles.add(d.nom); else st.modeles.delete(d.nom); } }),
      ` ${d.nom}${d.titre ? ` — ${d.titre}` : ''}`)) : el('p', { class: 'note' }, t('document_aucun_modele')));
  const majModeles = () => { choixModeles.style.display = st.gabarit === 'vide' ? 'none' : ''; };
  const radio = (nom, valeur, libelle, f) => el('label', { class: 'champ' },
    el('input', { type: 'radio', name: `nouveau-${nom}`, checked: st[nom] === valeur, onchange: () => { st[nom] = valeur; if (f) f(); } }), ` ${libelle}`);
  const contenu = el('div', { class: 'dialogue-document' },
    el('div', { class: 'grille-champs' },
      el('label', {}, t('document_titre')), el('input', { style: 'width:26em', placeholder: t('document_sans_titre'), oninput: (e) => { st.titre = e.target.value; } }),
      el('label', {}, t('document_langue')), el('div', { class: 'ligne' }, radio('langue', 'fr', 'Français'), radio('langue', 'en', 'English')),
      el('label', {}, t('document_gabarit')), el('div', { class: 'colonne' }, radio('gabarit', 'vide', t('document_gabarit_vide'), majModeles),
        radio('gabarit', 'article', t('document_gabarit_article'), majModeles), radio('gabarit', 'rapport', t('document_gabarit_rapport'), majModeles))),
    choixModeles,
    el('p', { class: 'note' }, t('document_nouveau_note')));
  majModeles();
  dialogue({ titre: `${t('nouveau_document').replace(/…$/, '')} — ${p.nom}`, contenu, boutons: [
    { libelle: t('annuler'), action: () => true },
    { libelle: t('creer_document'), principal: true, action: () => {
      const choisis = ajustes.filter((d) => st.modeles.has(d.nom));
      creer(app, p.id, { titre: st.titre.trim(), langue: st.langue, texte: gabarit(app, p, st, choisis) })
        .then((id) => { if (id && apres) apres(id); }, (e) => app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true));
      return true;
    } }] });
}

async function creer(app, pid, doc) {
  const r = await demander('document_enregistrer', { projet: pid, ...doc });
  app.majProjet(r.projet);
  app.diffuser('modeles');  // (the other windows' explorers)
  app.ouvrirDocument(pid, r.id);
  return r.id;
}

// The text a template starts with, in the document's language.
function gabarit(app, p, st, choisis) {
  const fr = st.langue === 'fr';
  const w = mots(st.langue);
  const etiquette = (x) => x.replace(/[^\p{L}\p{N}_-]+/gu, '-');
  const calcule = (d) => app.modelesDe(p.id).filter((x) => x.numero === d.numero).pop();
  const phrase = (d) => {
    const c = (x) => `{{${d.nom}.${x}}}`;
    if (REGRESSIONS.includes(d.type)) {
      return fr ? `Le modèle ${d.nom} porte sur ${c('N')} observations ; son pouvoir prédictif en validation croisée est Q² = ${c('Q2')}.`
        : `Model ${d.nom} uses ${c('N')} observations; its cross-validated predictive ability is Q² = ${c('Q2')}.`;
    }
    if (AUTRES_ANALYSES.includes(d.type)) return fr ? `Résultats de l'analyse ${d.nom}.` : `Results of analysis ${d.nom}.`;
    if (d.type === 'PCA') {
      return fr ? `Les ${c('A')} composantes de ${d.nom} expliquent ${c('R2X:%')} de la variance (${c('N')} observations).`
        : `The ${c('A')} components of ${d.nom} explain ${c('R2X:%')} of the variance (${c('N')} observations).`;
    }
    return fr ? `Le modèle ${d.nom} explique ${c('R2Y:%')} de la variance de Y ; son pouvoir prédictif en validation croisée est Q² = ${c('Q2')} (${c('N')} observations).`
      : `Model ${d.nom} explains ${c('R2Y:%')} of the variance of Y; its cross-validated predictive ability is Q² = ${c('Q2')} (${c('N')} observations).`;
  };
  // its plots: those a report holds by default (the report), or the first (the article); a model
  // not computed yet: the plot most models of its kind have
  const PREMIERE = { PCA: 'scores', LM: 'coefficients', LMM: 'coefficients', GLM: 'coefficients', GAM: 'effets', RDA: 'ordination', CA: 'ordination',
    CCA: 'ordination', INDVAL: 'indval_graphe', TRIM: 'trim_indices', DISTANCE: 'distance_detection' };
  const figures = (d, toutes) => {
    const m = calcule(d);
    let liste = m ? partiesModele(m).filter((x) => x.vue && !x.variantes && (!toutes || x.defaut)).map((x) => x.cle) : [];
    if (!toutes) liste = liste.slice(0, 1);
    if (!m) liste = [PREMIERE[d.type] || (AUTRES_ANALYSES.includes(d.type) ? null : 'scores')].filter(Boolean);
    return liste.map((v) => `{{${d.nom}.${v} #fig-${etiquette(d.nom)}-${v} | ${t(v)}}}`);
  };
  const resume = (d) => (!calcule(d) || partiesModele(calcule(d)).some((x) => x.cle === 'resume' && x.tableau) ? [`{{${d.nom}.table.resume | ${w.resume}}}`] : []);
  const entete = `---\ntitle: "${(st.titre || t('document_sans_titre')).replace(/"/g, '\\"')}"\n${st.gabarit === 'article' ? 'author: ""\n' : ''}---\n\n`;
  if (st.gabarit === 'article') {
    return entete + [`# ${w.introduction}`, '', `# ${w.methodes}`, ...choisis.map((d) => `{{${d.nom}.methodes}}`),
      `# ${w.resultats}`, ...choisis.flatMap((d) => [phrase(d), `{{${d.nom}.resultats}}`, ...figures(d, false)]), `# ${w.discussion}`, '', `# ${w.references}`, ''].join('\n\n');
  }
  if (st.gabarit === 'rapport') {
    return entete + choisis.flatMap((d) => [`# ${d.nom}${d.titre ? ` — ${d.titre}` : ''}`, phrase(d), `{{${d.nom}.resultats}}`,
      ...resume(d), ...figures(d, true), `## ${w.methodes}`, `{{${d.nom}.methodes}}`]).join('\n\n') + '\n';
  }
  return entete;
}

function menuDocument(app, pid, id) {
  return [
    { libelle: t('ouvrir_document'), icone: 'file-text', action: () => app.ouvrirDocument(pid, id) },
    { libelle: t('document_renommer'), icone: 'pencil', action: () => renommer(app, pid, id) },
    { libelle: t('document_dupliquer'), icone: 'copy', action: () => dupliquer(app, pid, id).catch((e) => app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true)) },
    '-',
    { libelle: t('supprimer_document'), icone: 'trash-2', action: () => supprimer(app, pid, id).catch((e) => app.statut(`${t('erreur')}${deuxPoints()}${e.message}`, true)) },
  ];
}

const ouvert = (pid, id) => [...ouverts].find((s) => s.pid === pid && s.id === id);

function renommer(app, pid, id) {
  const d = [].concat(app.projet(pid).documents || []).find((x) => x.id === id);
  if (!d) return;
  let titre = d.titre;
  dialogue({ titre: t('document_renommer').replace(/…$/, ''), contenu: el('input', { value: titre, style: 'width:26em', oninput: (e) => { titre = e.target.value; } }), boutons: [
    { libelle: t('annuler'), action: () => true },
    { libelle: t('ok'), principal: true, action: () => {
      const s = ouvert(pid, id);
      if (s) {
        s.titre = titre;
        s.champTitre.value = titre;
        s.programmer(true);
        app.titrePanneau(cleDocument(pid, id), titre || t('document_sans_titre'));
      } else {
        demander('document_enregistrer', { projet: pid, id, titre }).then((r) => { app.majProjet(r.projet); app.diffuser('modeles'); });
      }
      return true;
    } }] });
}

async function dupliquer(app, pid, id) {
  const s = ouvert(pid, id);
  if (s) await s.enregistrer();
  const d = await demander('document', { projet: pid, id });
  await creer(app, pid, { titre: `${d.titre || t('document_sans_titre')} (${t('document_copie')})`, langue: d.langue, texte: d.texte, bib: d.bib || '', style: d.style || STYLES[0] });
}

async function supprimer(app, pid, id) {
  const d = [].concat(app.projet(pid).documents || []).find((x) => x.id === id);
  if (!d || !window.confirm(t('confirmer_supprimer_document', { titre: d.titre || t('document_sans_titre') }))) return;
  const s = ouvert(pid, id);
  if (s) s.modifie = false;  // (what it holds is going: nothing to keep when its panel closes)
  app.fermerVue(cleDocument(pid, id));
  const r = await demander('document_supprimer', { projet: pid, id });
  app.majProjet(r.projet);
  app.diffuser('modeles');
}

// (the help's text: paragraphs, and examples between backticks, on one line or several)
function dialogueAideDocument() {
  const morceaux = [];
  let code = null;
  for (const x of t('document_aide_texte').split('\n')) {
    if (code) {
      code.push(x);
      if (x.endsWith('`')) { morceaux.push(el('pre', {}, code.join('\n').replace(/`/g, ''))); code = null; }
    } else if (x.startsWith('`') && (x.length < 2 || !x.endsWith('`'))) code = [x];
    else morceaux.push(x.startsWith('`') ? el('pre', {}, x.replace(/`/g, '')) : el('p', {}, x));
  }
  dialogue({ titre: t('document_aide'), large: true, contenu: el('div', { class: 'aide-document' }, morceaux),
    boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
}

// ------------------------------------------------------------ strings

ajouterTextes({
  document: 'Document', documents: 'Documents', nouveau_document: 'Nouveau document…', creer_document: 'Créer le document',
  document_sans_titre: 'Sans titre', document_titre: 'Titre', document_langue: 'Langue', document_copie: 'copie', ok: 'OK',
  document_langue_aide: 'La langue du document : ses nombres (0,65 ou 0.65), « Figure », « Tableau » et le texte des méthodes',
  document_gabarit: 'Départ', document_gabarit_vide: 'Document vide', document_gabarit_article: 'Article (introduction, méthodes, résultats, discussion)',
  document_gabarit_rapport: 'Rapport d’analyse (pour chaque modèle : résumé, graphiques, méthodes)',
  document_modeles_gabarit: 'Modèles à y citer :',
  document_nouveau_note: 'Le document est gardé dans le projet et s’enregistre avec lui. Les modèles cochés y sont déjà cités ; les autres se citent en tapant {{.',
  document_aucun: 'Ce projet n’a pas encore de document.', document_supprime: 'Ce document a été supprimé.', document_aucun_modele: 'Aucun modèle ajusté dans ce projet',
  document_chargement: 'Ouverture du document…', document_editeur_absent: 'L’éditeur de documents n’a pas pu être chargé (vendor/editeur.min.js).',
  document_placeholder: 'Écrivez en Markdown. Tapez {{ pour citer un résultat du projet : {{M1.Q2}}, {{M1.scores | légende}}…',
  document_inserer: 'Insérer', document_inserer_aide: 'Insérer une valeur, un graphique, un tableau ou le texte des méthodes d’un modèle du projet',
  document_compiler: 'Compiler', document_compiler_aide: 'Écrire le document en Word, PDF, HTML ou Quarto, les références remplacées par leurs résultats',
  document_apercu_aide: 'Montrer ou cacher l’aperçu', document_aide: 'Écrire un document', document_modifie: 'modifié…',
  document_non_enregistre: 'non gardé : le moteur ne répond pas', document_mots: '{n} mots', document_refs: '{n} références',
  document_refs_erreurs: '{n} introuvable(s)', document_tableau: 'Tableau', document_methodes: 'Texte des méthodes', document_resultats: 'Texte des résultats',
  document_renvoi: 'le numéro d’une figure ou d’un tableau étiqueté (#…)',
  document_inserer_renvoi: 'Renvoi à une figure ou un tableau ({{ref:…}})', document_non_calcule: 'pas encore calculé : il le sera',
  document_seul: 'Un graphique, un tableau ou le texte des méthodes se met seul sur sa ligne.',
  document_qmd: 'Quarto (.qmd)…', document_quarto: 'Compiler avec Quarto', document_quarto_absent: 'Compiler avec Quarto (Quarto n’est pas installé)',
  reproductible_titre: 'Quarto (.qmd)', reproductible_question: 'Les résultats du document :',
  reproductible_non: 'Écrits en dur', reproductible_non_aide: 'Les valeurs, les graphiques et les tableaux tels que Nuée les montre.',
  reproductible_oui: 'Reproductibles',
  reproductible_oui_aide: 'R les recalcule à partir des données brutes quand Quarto compile le document, avec l’export vers R de chaque modèle cité, écrit à côté du .qmd (il faut R et ses paquets knitr et rmarkdown).',
  reproductible_sans_export: 'Sans export vers R, leurs valeurs resteront écrites en dur : {modeles}.',
  reproductible_en_cours: 'Export vers R des modèles cités…',
  reproductible_en_dur: '{n} des {total} références restent écrites en dur (le premier bloc du .qmd dit pourquoi).',
  reproductible_modeles_en_dur: 'Pas refaits par R : {modeles}.',
  document_quarto_en_cours: 'Quarto compile le document…', document_ecrit: 'Document écrit :',
  document_compiler_erreurs: '{n} référence(s) introuvable(s) : elles resteront telles quelles, en rouge. Compiler quand même ?',
  document_quarto_code: 'Ce document contient du code (R, Python…) que Quarto va exécuter sur cet ordinateur. Ne le compilez que si vous savez ce que fait ce code. Compiler avec Quarto ?',
  ouvrir_document: 'Ouvrir', document_renommer: 'Renommer…', document_dupliquer: 'Dupliquer', supprimer_document: 'Supprimer le document…',
  confirmer_supprimer_document: 'Supprimer le document « {titre} » ? (Il disparaît du projet à son prochain enregistrement.)',
  inserer_document: 'Insérer dans le document',
  ref_modele_inconnu: '« {cible} » : aucun modèle de ce nom dans le projet', ref_non_ajuste: '{nom} n’a pas été ajusté dans le fichier d’origine',
  ref_valeur_inconnue: '{chemin} : ce modèle n’a pas cette valeur', ref_variable_inconnue: '{chemin} : pas de variable de ce nom dans ce modèle',
  ref_terme_inconnu: '{chemin} : pas de terme de ce nom dans ce modèle', ref_valeur_absente: '{chemin} : pas de valeur pour ce modèle',
  ref_graphique_absent: '{chemin} : ce graphique n’a pas pu être dessiné', ref_tableau_inconnu: '{chemin} : ce modèle n’a pas ce tableau',
  ref_variante_inconnue: '{chemin} : ce graphique n’existe pas pour ce modèle',
  ref_etiquette_inconnue: 'aucune figure ni aucun tableau n’a l’étiquette #{etiquette}',
  document_citer: 'Citer', document_citer_aide: 'Citer une référence : de la bibliographie du document (fichiers BibTeX ou CSL-JSON) ou de Zotero',
  document_citations: '{n} citations', document_csl_absent: 'Style CSL introuvable :',
  citer_inserer: 'Insérer une citation…', citer_rechercher: 'Rechercher (auteur, année, titre)', citer_bouton: 'Citer',
  citer_narratif: 'Dans la phrase : Wold et al. (2001) plutôt que (Wold et al., 2001)',
  citer_rien: 'Aucune référence ne correspond.',
  citer_vide: 'Aucune référence encore : liez un fichier BibTeX ou CSL-JSON (Citer › Lier un fichier), collez-en, ou cherchez dans Zotero.',
  citer_dans_document: 'gardée dans le document', citer_lier: 'Lier un fichier BibTeX ou CSL-JSON…', citer_chemin: 'Chemin du fichier BibTeX ou CSL-JSON :',
  citer_lie: '{fichier} : {n} références', citer_coller: 'Coller des références BibTeX…',
  citer_coller_note: 'Les références collées (BibTeX ou CSL-JSON, comme les exporte un gestionnaire de références) sont gardées dans le document.',
  citer_ajouter: 'Ajouter', citer_coller_rien: 'Aucune référence lue dans ce texte.', citer_coller_fait: '{n} référence(s) ajoutée(s) au document.',
  citer_coller_erreurs: '{n} illisible(s).', citer_references: 'Références du document…',
  citer_references_note: 'Le document garde une copie des références qu’il cite (et de celles venues de Zotero ou collées) : il les cite encore sans leur fichier, sur un autre ordinateur.',
  citer_retirer: 'Retirer les {n} gardées et non citées', citer_actualiser: 'Relire les fichiers de bibliographie', citer_style: 'Style des citations',
  'citer_style_auteur-annee': 'Auteur-année : (Wold et al., 2001)', citer_style_numerique: 'Numéroté : [1]',
  citer_n: '{n} références', citer_n_document: 'Gardées dans le document : {n}', citer_fichier_erreur: 'illisible ou introuvable',
  zotero_bibliotheque: 'Zotero : toute la bibliothèque', zotero_source_aide: 'Chercher dans une bibliothèque Zotero (la vôtre ou celle d’un groupe), ou parcourir une de ses collections',
  zotero_ma_bibliotheque: 'Ma bibliothèque', zotero_toute: '{bibliotheque} : toute la bibliothèque', zotero_groupe: 'groupe Zotero {id}',
  zotero_sans_groupes_ancien: 'Ce Zotero ne donne pas ses bibliothèques de groupe aux autres applications (Zotero 7 ou plus récent le fait) : seule Ma bibliothèque est proposée.',
  zotero_sans_groupes_erreur: 'Zotero n’a pas répondu pour ses bibliothèques de groupe : seule Ma bibliothèque est proposée.',
  zotero_renommee: '@{cle} est déjà une autre référence dans ce document : celle de {bibliotheque} y est citée sous @{nouvelle}.',
  zotero_deja_sous: '@{cle} est déjà dans le document sous @{existante} : elle y est citée sous cette clé.',
  zotero_conflit_fichier: '@{cle} : {fichier} donne cette clé à une autre référence que celle gardée de {bibliotheque} ; c’est celle du fichier qui est citée.',
  citer_deja: 'déjà dans le document',
  zotero_choisir: 'Choisir dans Zotero (Better BibTeX)…', zotero_attente: 'Choisissez les références dans la fenêtre de Zotero…',
  zotero_delai: 'Zotero n’a rien renvoyé à temps.', zotero_bureau: 'Zotero : dans l’application de bureau',
  zotero_incomplet: 'Zotero n’a pas envoyé {n} des références choisies : elles sont citées, mais introuvables.',
  zotero_connecte: 'Zotero : connecté (recherche dans {bibliotheque})', zotero_connecte_bbt: 'Zotero : connecté, avec Better BibTeX (recherche dans {bibliotheque})',
  zotero_ferme: 'Zotero : cochez Paramètres › Avancé › « Autoriser les autres applications de cet ordinateur à communiquer avec Zotero »',
  zotero_ancien: 'Zotero : la recherche demande Zotero 7 ou plus récent', zotero_absent: 'Zotero ne répond pas (est-il ouvert ?)',
  document_aide_texte: 'Un document s’écrit en Markdown : # Titre, ## Sous-titre, *italique*, **gras**, listes (- ou 1.), tableaux (| a | b |), liens [texte](https://…).\n'
    + 'Les résultats du projet s’y citent entre doubles accolades ; ils sont remplacés par leur valeur dans l’aperçu et à la compilation, et suivent les modèles quand ils changent :\n'
    + '`{{M1.Q2}}  {{M1.R2Y:%}}  {{M1.Q2:3}}  {{M1.N}}  {{M1.gain}}  {{M1.vip.sucre}}  {{M1.coef.sucre}}  {{M5.coef.annee}}  {{M1.cvanova.p}}`\n'
    + 'Un graphique, un tableau ou le texte des méthodes se met seul sur sa ligne, avec une étiquette (#…) et une légende (après |) ; ils sont numérotés dans l’ordre, et {{ref:étiquette}} donne leur numéro :\n'
    + '`{{M1.scores #fig-scores | Scores des observations}}   {{M1.table.resume | R² et Q²}}   {{M1.methodes}}   voir {{ref:fig-scores}}`\n'
    + 'Tapez {{ pour voir les modèles et ce que chacun offre, avec sa valeur. Les graphiques sont dessinés comme dans leur fenêtre quand elle est ouverte. La langue du document règle l’écriture des nombres et le texte des méthodes.\n'
    + 'Les citations s’écrivent comme dans Quarto : [@wold2001], [voir @wold2001, p. 3 ; @eriksson2006], @wold2001 dans la phrase, [-@wold2001] sans l’auteur ; tapez @ pour les références du document et de Zotero. La liste des références vient à la fin, ou là où le document met {{bibliographie}} :\n'
    + '`---\ntitle: Mon article\nbibliography: references.bib\n---\nLa PLS [@wold2001] et la validation croisée (@eriksson2006, p. 92).`\n'
    + 'Citer › Lier un fichier : un fichier BibTeX ou CSL-JSON (exporté de Zotero, Mendeley, JabRef… ; Better BibTeX le tient à jour), nommé dans l’en-tête. Zotero ouvert sur cet ordinateur : ses références se cherchent depuis Nuée (Zotero 7, en cochant « Autoriser les autres applications de cet ordinateur à communiquer avec Zotero »), dans Ma bibliothèque ou la bibliothèque d’un groupe, choisie dans Citer › Insérer une citation. Une référence dont la clé est déjà celle d’une autre du document (la même clé dans deux bibliothèques) y est citée avec une lettre en plus : @bechet2019a.\n'
    + 'Compiler : Word, PDF ou HTML directement ; ou un fichier Quarto (.qmd) avec ses figures et ses références (.bib), et, si Quarto est installé, compilé par lui (style CSL d’une revue : csl: dans l’en-tête, renvois, modèles de revues). {{< pagebreak >}} seul sur sa ligne commence une nouvelle page.\n'
    + 'Quarto reproductible : à côté du .qmd, l’export vers R de chaque modèle cité et ses données ; Quarto les relance, et chaque valeur devient `r …` qui lit la valeur recalculée, chaque graphique et chaque tableau un bloc R. Ce que les scripts n’ont pas (un graphique qu’ils ne dessinent pas, le texte des méthodes et des résultats) reste écrit en dur, et le premier bloc du .qmd le dit.',
}, {
  document: 'Document', documents: 'Documents', nouveau_document: 'New document…', creer_document: 'Create the document',
  document_sans_titre: 'Untitled', document_titre: 'Title', document_langue: 'Language', document_copie: 'copy', ok: 'OK',
  document_langue_aide: 'The document’s language: its numbers (0,65 or 0.65), “Figure”, “Table” and the Methods text',
  document_gabarit: 'Start from', document_gabarit_vide: 'Blank document', document_gabarit_article: 'Article (introduction, methods, results, discussion)',
  document_gabarit_rapport: 'Analysis report (for each model: summary, plots, methods)',
  document_modeles_gabarit: 'Models to cite in it:',
  document_nouveau_note: 'The document is kept in the project and saved with it. The models ticked are cited in it already; the others are cited by typing {{.',
  document_aucun: 'This project has no document yet.', document_supprime: 'This document was deleted.', document_aucun_modele: 'No fitted model in this project',
  document_chargement: 'Opening the document…', document_editeur_absent: 'The document editor couldn’t be loaded (vendor/editeur.min.js).',
  document_placeholder: 'Write in Markdown. Type {{ to cite a result of the project: {{M1.Q2}}, {{M1.scores | caption}}…',
  document_inserer: 'Insert', document_inserer_aide: 'Insert a value, a plot, a table or the Methods text of a model of the project',
  document_compiler: 'Compile', document_compiler_aide: 'Write the document as Word, PDF, HTML or Quarto, its references replaced by their results',
  document_apercu_aide: 'Show or hide the preview', document_aide: 'Writing a document', document_modifie: 'changed…',
  document_non_enregistre: 'not kept: the engine doesn’t answer', document_mots: '{n} words', document_refs: '{n} references',
  document_refs_erreurs: '{n} unresolved', document_tableau: 'Table', document_methodes: 'Methods text', document_resultats: 'Results text',
  document_renvoi: 'the number of a labelled figure or table (#…)',
  document_inserer_renvoi: 'Cross-reference to a figure or table ({{ref:…}})', document_non_calcule: 'not computed yet: it will be',
  document_seul: 'A plot, a table or the Methods text goes alone on its line.',
  document_qmd: 'Quarto (.qmd)…', document_quarto: 'Compile with Quarto', document_quarto_absent: 'Compile with Quarto (Quarto isn’t installed)',
  reproductible_titre: 'Quarto (.qmd)', reproductible_question: 'The document’s results:',
  reproductible_non: 'Written in', reproductible_non_aide: 'The values, plots and tables as Nuée shows them.',
  reproductible_oui: 'Reproducible',
  reproductible_oui_aide: 'R recomputes them from the raw data when Quarto renders the document, with each cited model’s Export to R script, written beside the .qmd (R and its packages knitr and rmarkdown are needed).',
  reproductible_sans_export: 'Without an export to R, their values will stay written in: {modeles}.',
  reproductible_en_cours: 'Exporting the cited models to R…',
  reproductible_en_dur: '{n} of the {total} references stay written in (the .qmd’s first chunk says why).',
  reproductible_modeles_en_dur: 'Not redone by R: {modeles}.',
  document_quarto_en_cours: 'Quarto is compiling the document…', document_ecrit: 'Document written:',
  document_compiler_erreurs: '{n} unresolved reference(s): they will stay as they are, in red. Compile anyway?',
  document_quarto_code: 'This document holds code (R, Python…) that Quarto will run on this computer. Only compile it if you know what that code does. Compile with Quarto?',
  ouvrir_document: 'Open', document_renommer: 'Rename…', document_dupliquer: 'Duplicate', supprimer_document: 'Delete the document…',
  confirmer_supprimer_document: 'Delete the document “{titre}”? (It leaves the project when the project is next saved.)',
  inserer_document: 'Insert in the document',
  ref_modele_inconnu: '“{cible}”: no model of that name in the project', ref_non_ajuste: '{nom} wasn’t fitted in the original file',
  ref_valeur_inconnue: '{chemin}: this model has no such value', ref_variable_inconnue: '{chemin}: no variable of that name in this model',
  ref_terme_inconnu: '{chemin}: no term of that name in this model', ref_valeur_absente: '{chemin}: no value for this model',
  ref_graphique_absent: '{chemin}: this plot couldn’t be drawn', ref_tableau_inconnu: '{chemin}: this model has no such table',
  ref_variante_inconnue: '{chemin}: this plot doesn’t exist for this model',
  ref_etiquette_inconnue: 'no figure or table has the label #{etiquette}',
  document_citer: 'Cite', document_citer_aide: 'Cite a reference: from the document’s bibliography (BibTeX or CSL-JSON files) or from Zotero',
  document_citations: '{n} citations', document_csl_absent: 'CSL style not found:',
  citer_inserer: 'Insert a citation…', citer_rechercher: 'Search (author, year, title)', citer_bouton: 'Cite',
  citer_narratif: 'In the sentence: Wold et al. (2001) rather than (Wold et al., 2001)',
  citer_rien: 'No reference matches.',
  citer_vide: 'No reference yet: link a BibTeX or CSL-JSON file (Cite › Link a file), paste some, or search Zotero.',
  citer_dans_document: 'kept in the document', citer_lier: 'Link a BibTeX or CSL-JSON file…', citer_chemin: 'Path of the BibTeX or CSL-JSON file:',
  citer_lie: '{fichier}: {n} references', citer_coller: 'Paste BibTeX references…',
  citer_coller_note: 'Pasted references (BibTeX or CSL-JSON, as a reference manager exports them) are kept in the document.',
  citer_ajouter: 'Add', citer_coller_rien: 'No reference read in this text.', citer_coller_fait: '{n} reference(s) added to the document.',
  citer_coller_erreurs: '{n} unreadable.', citer_references: 'The document’s references…',
  citer_references_note: 'The document keeps a copy of the references it cites (and of those from Zotero or pasted): it still cites them without their file, on another computer.',
  citer_retirer: 'Remove the {n} kept and not cited', citer_actualiser: 'Read the bibliography files again', citer_style: 'Citation style',
  'citer_style_auteur-annee': 'Author–year: (Wold et al., 2001)', citer_style_numerique: 'Numbered: [1]',
  citer_n: '{n} references', citer_n_document: 'Kept in the document: {n}', citer_fichier_erreur: 'unreadable or missing',
  zotero_bibliotheque: 'Zotero: whole library', zotero_source_aide: 'Search a Zotero library (yours or a group’s), or browse one of its collections',
  zotero_ma_bibliotheque: 'My Library', zotero_toute: '{bibliotheque}: whole library', zotero_groupe: 'Zotero group {id}',
  zotero_sans_groupes_ancien: 'This Zotero doesn’t give its group libraries to other applications (Zotero 7 or later does): only My Library is offered.',
  zotero_sans_groupes_erreur: 'Zotero didn’t answer for its group libraries: only My Library is offered.',
  zotero_renommee: '@{cle} is already another reference in this document: the one from {bibliotheque} is cited as @{nouvelle}.',
  zotero_deja_sous: '@{cle} is already in the document as @{existante}: it is cited under that key.',
  zotero_conflit_fichier: '@{cle}: {fichier} gives this key to another reference than the one kept from {bibliotheque}; the file’s is the one cited.',
  citer_deja: 'already in the document',
  zotero_choisir: 'Pick in Zotero (Better BibTeX)…', zotero_attente: 'Pick the references in Zotero’s window…',
  zotero_delai: 'Zotero sent nothing back in time.', zotero_bureau: 'Zotero: in the desktop application',
  zotero_incomplet: 'Zotero didn’t send {n} of the references picked: they are cited, but not found.',
  zotero_connecte: 'Zotero: connected (searching {bibliotheque})', zotero_connecte_bbt: 'Zotero: connected, with Better BibTeX (searching {bibliotheque})',
  zotero_ferme: 'Zotero: tick Settings › Advanced › “Allow other applications on this computer to communicate with Zotero”',
  zotero_ancien: 'Zotero: searching needs Zotero 7 or later', zotero_absent: 'Zotero doesn’t answer (is it open?)',
  document_aide_texte: 'A document is written in Markdown: # Title, ## Subtitle, *italic*, **bold**, lists (- or 1.), tables (| a | b |), links [text](https://…).\n'
    + 'The project’s results are cited between double braces; they are replaced by their value in the preview and when compiling, and follow the models when they change:\n'
    + '`{{M1.Q2}}  {{M1.R2Y:%}}  {{M1.Q2:3}}  {{M1.N}}  {{M1.gain}}  {{M1.vip.sugar}}  {{M1.coef.sugar}}  {{M5.coef.year}}  {{M1.cvanova.p}}`\n'
    + 'A plot, a table or the Methods text goes alone on its line, with a label (#…) and a caption (after |); they are numbered in order, and {{ref:label}} gives their number:\n'
    + '`{{M1.scores #fig-scores | Scores of the observations}}   {{M1.table.resume | R² and Q²}}   {{M1.methodes}}   see {{ref:fig-scores}}`\n'
    + 'Type {{ to see the models and what each offers, with its value. Plots are drawn as in their window when it is open. The document’s language sets how numbers are written and the Methods text.\n'
    + 'Citations are written as in Quarto: [@wold2001], [see @wold2001, p. 3; @eriksson2006], @wold2001 in the sentence, [-@wold2001] without the author; type @ for the references of the document and of Zotero. The list of references comes at the end, or where the document puts {{bibliographie}}:\n'
    + '`---\ntitle: My paper\nbibliography: references.bib\n---\nPLS [@wold2001] and cross-validation (@eriksson2006, p. 92).`\n'
    + 'Cite › Link a file: a BibTeX or CSL-JSON file (exported from Zotero, Mendeley, JabRef…; Better BibTeX keeps it up to date), named in the header. With Zotero open on this computer, its references are searched from Nuée (Zotero 7, ticking “Allow other applications on this computer to communicate with Zotero”), in My Library or a group’s library, chosen in Cite › Insert a citation. A reference whose key another one of the document already has (the same key in two libraries) is cited with a letter added: @bechet2019a.\n'
    + 'Compile: Word, PDF or HTML directly; or a Quarto file (.qmd) with its figures and references (.bib), compiled by Quarto when it is installed (a journal’s CSL style: csl: in the header, cross-references, journal templates). {{< pagebreak >}} alone on its line starts a new page.\n'
    + 'Reproducible Quarto: beside the .qmd, each cited model’s Export to R script and its data; Quarto runs them again, and each value becomes `r …` reading the recomputed value, each plot and table an R chunk. What the scripts don’t have (a plot they don’t draw, the Methods and Results texts) stays written in, and the .qmd’s first chunk says so.',
});
