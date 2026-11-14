// The documents of a project: text in Markdown that refers to the project's results ({{M1.Q2}},
// {{M1.scores | caption}}...: references.js), written beside its live preview, then compiled into
// a Word file, a PDF, an HTML page or a Quarto file (rendu_document.js), or by Quarto when the
// computer has it. Kept in the project (p$documents: moteur/serveur.R, document*), so saved with
// it and in its recovery copies; undo leaves them alone.
// The editor (CodeMirror) and the Markdown reader (markdown-it) come in one file, loaded when a
// document is first opened (copier-vendor.js: vendor/editeur.min.js).
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, dialogue, ouvrirMenuSous } from './ui.js';
import { demander, bureau } from './api.js';
import { VUES } from './panneaux.js';
import { partiesModele, imageVue, LARGEUR, HAUTEUR } from './rapport.js';
import { separerCible, valeurReference, offreModele, numeroter } from './references.js';
import { pluginReferences, analyser, versHTML, pageHTML, versDocx, versQmd, lireEntete, BLOCS } from './rendu_document.js';
import { VERSION } from './nouveautes.js';

let md = null;              // markdown-it, once loaded
let chargement = null;
const ouverts = new Set();  // the documents open (their editor), to save what is being typed
const affiches = new Map(); // project id -> the document its panel shows
let dernier = null;         // the document last written in (Insert in the document, from the explorer)

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
const libelles = (langue) => ({ figure: mots(langue).figure, tableau: mots(langue).tableau, seul: t('document_seul') });

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
const AUTRES_ANALYSES = ['RDA', 'CA', 'LDA', 'TREE', 'KMEANS'];

export function installerDocuments(app) {
  VUES.document = {
    rendre: (conteneur, ctx, params, loc) => rendreDocument(app, conteneur, params, loc),
    titre: (_, p) => { const s = affiches.get(p.id); return s ? s.titre || t('document_sans_titre') : `${p.nom || ''} · ${t('document')}`; },
    projet: true,
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
  const id = loc.doc && existe(loc.doc) ? loc.doc : s ? s.id : (docs[0] && docs[0].id);
  if (!id) {
    affiches.delete(pid);
    conteneur.replaceChildren(el('div', { class: 'vide-document' }, el('p', { class: 'note' }, t('document_aucun')),
      el('button', { class: 'principal', onclick: () => dialogueNouveau(app, pid) }, icone('file-text'), ` ${t('nouveau_document')}`)));
    return;
  }
  loc.doc = id;
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
    app.titrePanneau(`document:${pid}`, d.titre || t('document_sans_titre'));
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
  };
  affiches.set(pid, s);
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
      oninput: (e) => { s.titre = e.target.value; s.programmer(true); app.titrePanneau(`document:${pid}`, s.titre || t('document_sans_titre')); } });
    s.choixLangue = el('select', { title: t('document_langue_aide'), 'aria-label': t('document_langue_aide'),
      onchange: (e) => { s.langue = e.target.value; s.vue.contentDOM.setAttribute('lang', s.langue); s.programmer(true); } },
    [['fr', 'Français'], ['en', 'English']].map(([v, n]) => el('option', { value: v, selected: v === s.langue }, n)));
    const bouton = (ic, libelle, aide, action) => el('button', { title: aide, onclick: action }, icone(ic), libelle ? ` ${libelle}` : '');
    const barre = el('div', { class: 'outils barre-document' }, s.champTitre, s.choixLangue,
      bouton('quote', t('document_inserer'), t('document_inserer_aide'), (e) => ouvrirMenuSous(e.currentTarget, menuInserer(app, s))),
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

  // the references in the text: marked, in red when they can't be resolved (not Quarto's {{< >}})
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
        CM.closeBrackets(), CM.autocompletion({ override: [completion(app, s)], icons: false, activateOnTyping: true }),
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
  s.programmer = (vite) => {
    s.modifie = true;
    s.etatEnregistrement.textContent = t('document_modifie');
    clearTimeout(s.attente);
    s.attente = setTimeout(() => s.enregistrer(), vite ? 100 : 800);
    s.programmerApercu();
  };
  // the text kept in the project (it is saved with it). `garder`: the window is closing.
  s.enregistrer = async (garder = false) => {
    clearTimeout(s.attente);
    if (!s.modifie || s.ferme) return;
    s.modifie = false;
    try {
      const r = await demander('document_enregistrer', { projet: pid, id: s.id, titre: s.titre, langue: s.langue, texte: s.vue.state.doc.toString() },
        { silencieux: true, garder });
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
    app.titrePanneau(`document:${pid}`, s.titre || t('document_sans_titre'));
    if (x.langue && x.langue !== s.langue) { s.langue = x.langue; s.choixLangue.value = x.langue; }
    s.rafraichir();
  };
  s.rafraichir = () => mettreAJour(app, s);
  s.fermer = () => {
    s.ferme = true;
    clearTimeout(s.attente);
    clearTimeout(s.attenteApercu);
    ouverts.delete(s);
    if (dernier === s) dernier = null;
    if (affiches.get(pid) === s) affiches.delete(pid);
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
// preview shows the text first, then them).
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
  if (chemin[0] === 'table') {
    const x = partie(chemin[1], 'tableau');
    if (!x || chemin.length !== 2) return erreur('ref_tableau_inconnu');
    const cle = `${m.id}|${x.cle}`;
    if (!s.tableaux.has(cle) && !attendre) return null;
    const blocs = s.tableaux.get(cle) || garder(s.tableaux, cle, await x.tableau(m));
    return { genre: 'tableau', blocs: blocs.filter((b) => b.texte !== undefined || b.entetes), nom: t(x.cle) };
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
    return { genre: 'figure', image, largeur: LARGEUR, hauteur: HAUTEUR, nom: t(x.cle) };
  }
  const v = valeurReference(m, chemin, ref, s.langue);
  return v.erreur ? erreur(`ref_${v.erreur}`) : { genre: 'valeur', texte: v.texte };
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
      resolus.push(!r || r.erreur ? r : { ...r, legende: ref.legende, etiquette: ref.etiquette });
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
  let resolus = await resoudre(app, s, analyse, false);
  if (generation !== s.generation || s.ferme) return;
  dessinerApercu(s, analyse, resolus, texte);
  if (resolus.some((r) => !r)) {
    resolus = await resoudre(app, s, analyse, true);
    if (generation !== s.generation || s.ferme) return;
    dessinerApercu(s, analyse, resolus, texte);
  }
}

function dessinerApercu(s, analyse, resolus, texte) {
  const haut = s.apercu.scrollTop;
  // (the plots as blob: addresses, not their whole PNG in the page at every keystroke)
  const pourApercu = resolus.map((r) => (r && r.genre === 'figure' ? { ...r, image: urlApercu(s, r.image) } : r));
  s.apercu.innerHTML = versHTML(md, analyse, pourApercu, libelles(s.langue));
  s.apercu.scrollTop = haut;
  s.erreurs = new Set(analyse.refs.filter((_, i) => resolus[i] && resolus[i].erreur).map((tok) => tok.meta.brut));
  s.versionErreurs += 1;
  s.vue.dispatch({});
  const n = analyse.refs.length;
  const erreurs = resolus.filter((r) => r && r.erreur).length;
  const motsTexte = texte.replace(/^---[\s\S]*?\n(---|\.\.\.)/, '').replace(/\{\{[^{}]*\}\}/g, ' x ').replace(/[#*_`>|[\]()~-]/g, ' ').split(/\s+/).filter(Boolean).length;
  s.etatTexte.replaceChildren(t('document_mots', { n: motsTexte }), n ? ` · ${t('document_refs', { n })}` : '',
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
        .map((o) => ({ ...o, detail: o.type === 'graphique' || o.type === 'tableau' ? t(o.detail) : o.type === 'texte' ? t('document_methodes') : o.detail }));
    } else {
      offre = [...VALEURS_COURANTES.map((c) => ({ chemin: c, type: 'valeur', detail: t('document_non_calcule') })),
        { chemin: 'methodes', type: 'texte', detail: t('document_methodes') }];
    }
    // (in the order given: the model's main values first, then by variable, tables, plots;
    // boost keeps it among the options that match as well)
    return { from: debut, validFor: /^[^{}|#]*$/, options: offre.map((o, i) => ({ label: `${nom}.${o.chemin}`, detail: o.detail, type: GENRES_COMPLETION[o.type] || 'variable',
      boost: Math.max(-99, 99 - i),
      // (a plot or a table: its caption follows, after |)
      apply: o.type === 'graphique' || o.type === 'tableau' ? `${nom}.${o.chemin} | ` : undefined })) };
  };
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
      s = [...ouverts].find((x) => x.pid === pid);
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
        { libelle: t('document_methodes'), icone: 'quote', action: ref('methodes', true) }]);
    } else {
      sous = [...VALEURS_COURANTES.map((c) => ({ libelle: c, action: ref(c) })), '-',
        { libelle: t('document_methodes'), icone: 'quote', action: ref('methodes', true) }];
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
    { libelle: t('document_qmd'), icone: 'file-output', action: () => compiler(app, s, 'qmd') },
    quarto ? { libelle: t('document_quarto'), icone: 'file-output',
      sous: [['docx', 'Word (.docx)'], ['typst', 'PDF (Typst)'], ['html', 'HTML']].map(([f, n]) => ({ libelle: n, action: () => compiler(app, s, 'qmd', f) })) }
      : { libelle: t('document_quarto_absent'), icone: 'file-output', actif: false, action: () => {} },
  ];
}

// A code chunk Quarto would run (```{r}, ```{python}..., or inline `r ...`).
const CODE_QUARTO = /^[ \t]*(\x60{3,}|~{3,})[ \t]*\{\s*(r|python|julia|bash|sh|ojs)\b|\x60r /mi;
const FILTRES = { docx: { name: 'Word', extensions: ['docx'] }, pdf: { name: 'PDF', extensions: ['pdf'] }, html: { name: 'HTML', extensions: ['html'] }, qmd: { name: 'Quarto', extensions: ['qmd'] } };
const octets = (texte) => new TextEncoder().encode(texte);

// The document written in `format` (docx, pdf, html, or qmd with its figures beside it), its
// references resolved now; then compiled by Quarto when `quarto` names its output.
async function compiler(app, s, format, quarto = null) {
  await s.enregistrer();
  const base = (s.titre || t('document_sans_titre')).replace(/[^\p{L}\p{N} _.()-]+/gu, '_').trim() || 'document';
  const r = await app.occupe(async () => {
    const texte = s.vue.state.doc.toString();
    const analyse = analyser(md, texte);
    return { texte, analyse, resolus: await resoudre(app, s, analyse, true) };
  });
  const erreurs = r.resolus.filter((x) => x.erreur).length;
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
  await app.occupe(async () => {
    if (format === 'docx') {
      const o = versDocx(r.analyse, r.resolus, { langue: s.langue, libelles: lib });
      if (bureau) await bureau.ecrireFichier(chemin, o); else telecharger(o, `${base}.docx`);
    } else if (format === 'pdf' || format === 'html') {
      const html = pageHTML(versHTML(md, r.analyse, r.resolus, lib), s.titre || base, s.langue);
      if (format === 'pdf') { if (bureau) await bureau.imprimerPDF(html, chemin); else imprimer(html); }
      else if (bureau) await bureau.ecrireFichier(chemin, octets(html)); else telecharger(octets(html), `${base}.html`);
    } else {
      // the .qmd, its figures in a folder beside it (<name>_figures), then Quarto if asked
      const fichier = chemin ? chemin.split(/[\\/]/).pop() : `${base}.qmd`;
      const nom = fichier.replace(/\.qmd$/i, '');
      const dossier = `${nom}_figures`;
      const q = versQmd(r.texte, r.analyse, r.resolus, { fichierFigure: (x) => `${dossier}/figure-${x.numero}.png`, langue: s.langue, titre: s.titre || base });
      if (!bureau) { telecharger(octets(q.texte), fichier); return; }
      const racine = chemin.slice(0, chemin.length - fichier.length);
      const sep = chemin.includes('\\') ? '\\' : '/';
      for (const x of r.resolus.filter((y) => y.genre === 'figure' && y.numero)) {
        const b = atob(x.image.split(',')[1]);
        await bureau.ecrireFichier(`${racine}${dossier}${sep}figure-${x.numero}.png`, Uint8Array.from(b, (c) => c.charCodeAt(0)));
      }
      await bureau.ecrireFichier(chemin, octets(q.texte));
      if (quarto) {
        app.statut(t('document_quarto_en_cours'));
        const sortie = await bureau.quarto(chemin, quarto);
        if (sortie.erreur) throw new Error(sortie.erreur === 'quarto_absent' ? t('document_quarto_absent') : `Quarto : ${sortie.erreur}`);
        ouvrir = sortie.sortie;
      }
    }
  });
  if (ouvrir && bureau) {
    app.statut(`${t('document_ecrit')} ${ouvrir}`);
    bureau.ouvrirChemin(ouvrir);
  }
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
  dialogue({ titre: t('nouveau_document').replace(/…$/, ''), contenu, boutons: [
    { libelle: t('annuler'), action: () => true },
    { libelle: t('creer_document'), principal: true, action: () => {
      const choisis = ajustes.filter((d) => st.modeles.has(d.nom));
      creer(app, p.id, { titre: st.titre.trim(), langue: st.langue, texte: gabarit(app, p, st, choisis) })
        .then((id) => { if (id && apres) apres(id); }, (e) => app.statut(`${t('erreur')} : ${e.message}`, true));
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
  const PREMIERE = { PCA: 'scores', LM: 'coefficients', LMM: 'coefficients', GLM: 'coefficients', GAM: 'effets', RDA: 'ordination', CA: 'ordination' };
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
      `# ${w.resultats}`, ...choisis.flatMap((d) => [phrase(d), ...figures(d, false)]), `# ${w.discussion}`, '', `# ${w.references}`, ''].join('\n\n');
  }
  if (st.gabarit === 'rapport') {
    return entete + choisis.flatMap((d) => [`# ${d.nom}${d.titre ? ` — ${d.titre}` : ''}`, phrase(d),
      ...resume(d), ...figures(d, true), `## ${w.methodes}`, `{{${d.nom}.methodes}}`]).join('\n\n') + '\n';
  }
  return entete;
}

function menuDocument(app, pid, id) {
  return [
    { libelle: t('ouvrir_document'), icone: 'file-text', action: () => app.ouvrirDocument(pid, id) },
    { libelle: t('document_renommer'), icone: 'pencil', action: () => renommer(app, pid, id) },
    { libelle: t('document_dupliquer'), icone: 'copy', action: () => dupliquer(app, pid, id).catch((e) => app.statut(`${t('erreur')} : ${e.message}`, true)) },
    '-',
    { libelle: t('supprimer_document'), icone: 'trash-2', action: () => supprimer(app, pid, id).catch((e) => app.statut(`${t('erreur')} : ${e.message}`, true)) },
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
        app.titrePanneau(`document:${pid}`, titre || t('document_sans_titre'));
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
  await creer(app, pid, { titre: `${d.titre || t('document_sans_titre')} (${t('document_copie')})`, langue: d.langue, texte: d.texte });
}

async function supprimer(app, pid, id) {
  const d = [].concat(app.projet(pid).documents || []).find((x) => x.id === id);
  if (!d || !window.confirm(t('confirmer_supprimer_document', { titre: d.titre || t('document_sans_titre') }))) return;
  const s = ouvert(pid, id);
  if (s) { s.modifie = false; s.fermer(); s.conteneur.etatDocument = null; }
  const r = await demander('document_supprimer', { projet: pid, id });
  app.majProjet(r.projet);
  app.diffuser('modeles');
  if (s) {
    s.loc.doc = null;
    rendreDocument(app, s.conteneur, { projet: pid }, s.loc);
  }
}

function dialogueAideDocument() {
  const lignes = t('document_aide_texte').split('\n');
  dialogue({ titre: t('document_aide'), large: true, contenu: el('div', { class: 'aide-document' }, lignes.map((x) => (x.startsWith('`') ? el('pre', {}, x.replace(/`/g, '')) : el('p', {}, x)))),
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
  document_aucun: 'Ce projet n’a pas encore de document.', document_aucun_modele: 'Aucun modèle ajusté dans ce projet',
  document_chargement: 'Ouverture du document…', document_editeur_absent: 'L’éditeur de documents n’a pas pu être chargé (vendor/editeur.min.js).',
  document_placeholder: 'Écrivez en Markdown. Tapez {{ pour citer un résultat du projet : {{M1.Q2}}, {{M1.scores | légende}}…',
  document_inserer: 'Insérer', document_inserer_aide: 'Insérer une valeur, un graphique, un tableau ou le texte des méthodes d’un modèle du projet',
  document_compiler: 'Compiler', document_compiler_aide: 'Écrire le document en Word, PDF, HTML ou Quarto, les références remplacées par leurs résultats',
  document_apercu_aide: 'Montrer ou cacher l’aperçu', document_aide: 'Écrire un document', document_modifie: 'modifié…',
  document_non_enregistre: 'non gardé : le moteur ne répond pas', document_mots: '{n} mots', document_refs: '{n} références',
  document_refs_erreurs: '{n} introuvable(s)', document_tableau: 'Tableau', document_methodes: 'Texte des méthodes',
  document_renvoi: 'le numéro d’une figure ou d’un tableau étiqueté (#…)',
  document_inserer_renvoi: 'Renvoi à une figure ou un tableau ({{ref:…}})', document_non_calcule: 'pas encore calculé : il le sera',
  document_seul: 'Un graphique, un tableau ou le texte des méthodes se met seul sur sa ligne.',
  document_qmd: 'Quarto (.qmd), résultats écrits en dur…', document_quarto: 'Compiler avec Quarto', document_quarto_absent: 'Compiler avec Quarto (Quarto n’est pas installé)',
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
  document_aide_texte: 'Un document s’écrit en Markdown : # Titre, ## Sous-titre, *italique*, **gras**, listes (- ou 1.), tableaux (| a | b |), liens [texte](https://…).\n'
    + 'Les résultats du projet s’y citent entre doubles accolades ; ils sont remplacés par leur valeur dans l’aperçu et à la compilation, et suivent les modèles quand ils changent :\n'
    + '`{{M1.Q2}}  {{M1.R2Y:%}}  {{M1.Q2:3}}  {{M1.N}}  {{M1.gain}}  {{M1.vip.sucre}}  {{M1.coef.sucre}}  {{M5.coef.annee}}  {{M1.cvanova.p}}`\n'
    + 'Un graphique, un tableau ou le texte des méthodes se met seul sur sa ligne, avec une étiquette (#…) et une légende (après |) ; ils sont numérotés dans l’ordre, et {{ref:étiquette}} donne leur numéro :\n'
    + '`{{M1.scores #fig-scores | Scores des observations}}   {{M1.table.resume | R² et Q²}}   {{M1.methodes}}   voir {{ref:fig-scores}}`\n'
    + 'Tapez {{ pour voir les modèles et ce que chacun offre, avec sa valeur. Les graphiques sont dessinés comme dans leur fenêtre quand elle est ouverte. La langue du document règle l’écriture des nombres et le texte des méthodes.\n'
    + 'Compiler : Word, PDF ou HTML directement ; ou un fichier Quarto (.qmd) avec ses figures, et, si Quarto est installé, compilé par lui (bibliographie, renvois, modèles de revues). {{< pagebreak >}} seul sur sa ligne commence une nouvelle page.',
}, {
  document: 'Document', documents: 'Documents', nouveau_document: 'New document…', creer_document: 'Create the document',
  document_sans_titre: 'Untitled', document_titre: 'Title', document_langue: 'Language', document_copie: 'copy', ok: 'OK',
  document_langue_aide: 'The document’s language: its numbers (0,65 or 0.65), “Figure”, “Table” and the Methods text',
  document_gabarit: 'Start from', document_gabarit_vide: 'Blank document', document_gabarit_article: 'Article (introduction, methods, results, discussion)',
  document_gabarit_rapport: 'Analysis report (for each model: summary, plots, methods)',
  document_modeles_gabarit: 'Models to cite in it:',
  document_nouveau_note: 'The document is kept in the project and saved with it. The models ticked are cited in it already; the others are cited by typing {{.',
  document_aucun: 'This project has no document yet.', document_aucun_modele: 'No fitted model in this project',
  document_chargement: 'Opening the document…', document_editeur_absent: 'The document editor couldn’t be loaded (vendor/editeur.min.js).',
  document_placeholder: 'Write in Markdown. Type {{ to cite a result of the project: {{M1.Q2}}, {{M1.scores | caption}}…',
  document_inserer: 'Insert', document_inserer_aide: 'Insert a value, a plot, a table or the Methods text of a model of the project',
  document_compiler: 'Compile', document_compiler_aide: 'Write the document as Word, PDF, HTML or Quarto, its references replaced by their results',
  document_apercu_aide: 'Show or hide the preview', document_aide: 'Writing a document', document_modifie: 'changed…',
  document_non_enregistre: 'not kept: the engine doesn’t answer', document_mots: '{n} words', document_refs: '{n} references',
  document_refs_erreurs: '{n} unresolved', document_tableau: 'Table', document_methodes: 'Methods text',
  document_renvoi: 'the number of a labelled figure or table (#…)',
  document_inserer_renvoi: 'Cross-reference to a figure or table ({{ref:…}})', document_non_calcule: 'not computed yet: it will be',
  document_seul: 'A plot, a table or the Methods text goes alone on its line.',
  document_qmd: 'Quarto (.qmd), results written in…', document_quarto: 'Compile with Quarto', document_quarto_absent: 'Compile with Quarto (Quarto isn’t installed)',
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
  document_aide_texte: 'A document is written in Markdown: # Title, ## Subtitle, *italic*, **bold**, lists (- or 1.), tables (| a | b |), links [text](https://…).\n'
    + 'The project’s results are cited between double braces; they are replaced by their value in the preview and when compiling, and follow the models when they change:\n'
    + '`{{M1.Q2}}  {{M1.R2Y:%}}  {{M1.Q2:3}}  {{M1.N}}  {{M1.gain}}  {{M1.vip.sugar}}  {{M1.coef.sugar}}  {{M5.coef.year}}  {{M1.cvanova.p}}`\n'
    + 'A plot, a table or the Methods text goes alone on its line, with a label (#…) and a caption (after |); they are numbered in order, and {{ref:label}} gives their number:\n'
    + '`{{M1.scores #fig-scores | Scores of the observations}}   {{M1.table.resume | R² and Q²}}   {{M1.methodes}}   see {{ref:fig-scores}}`\n'
    + 'Type {{ to see the models and what each offers, with its value. Plots are drawn as in their window when it is open. The document’s language sets how numbers are written and the Methods text.\n'
    + 'Compile: Word, PDF or HTML directly; or a Quarto file (.qmd) with its figures, compiled by Quarto when it is installed (bibliography, cross-references, journal templates). {{< pagebreak >}} alone on its line starts a new page.',
});
