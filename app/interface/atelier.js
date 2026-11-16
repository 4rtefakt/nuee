// The data workbench (Edit menu, the Data view's toolbar and the right-click on a column's
// header): Find and replace, the categories of a qualitative variable, the units and descriptions
// of the variables; from a column's header, a variable renamed, deleted, or treated as categories or
// as numbers. R makes each change (moteur/R/atelier.R), undone by a single Ctrl+Z like the other
// changes of the data; models already fitted keep their results until they are fitted again.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander } from './api.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const entre = (n) => (langueActuelle() === 'fr' ? `« ${n} »` : `“${n}”`);

// The variables to choose among: each with its kind (and its number of categories)
function ligneVariable(v) {
  return [v.nom, el('span', { class: 'doux' }, ` · ${v.qualitative ? t(liste(v.categories).length === 1 ? 'atelier_qualitative_une' : 'atelier_qualitative', { n: liste(v.categories).length }) : t('atelier_quantitative')}`)];
}

// ------------------------------------------------------------ find and replace

// In one variable, several or all: a text or a number, the whole cell or a part of a text, capitals
// counting or not, or the empty cells; R says, as the fields change, how many cells would change
// and what each variable becomes (a text variable whose values all become numbers turns
// quantitative), then makes the change. `uid`: the variable chosen first (from its column's header).
export function dialogueRemplacer(app, p, uid = null) {
  const variables = p.variables;
  const st = { chercher: '', remplacer: '', vides: false, partie: false, casse: false, nombres: true,
    toutes: uid === null || uid === undefined, choisies: new Set(uid === null || uid === undefined ? [] : [uid]) };
  const contenu = el('div', { class: 'dialogue-remplacer' });
  const apercu = el('div', { class: 'apercu-remplacer', 'aria-live': 'polite' });
  const erreur = el('p', { class: 'erreur' });
  const requete = () => ({ variables: st.toutes ? null : [...st.choisies], chercher: st.chercher, remplacer: st.remplacer,
    vides: st.vides, partie: st.partie, casse: st.casse, nombres: st.nombres });
  let minuterie = null;
  let numero = 0;
  const montrer = (r, message) => {
    apercu.replaceChildren(...(r ? liste(r.phrases).map((x, i) => el('p', { class: i ? '' : 'resume-remplacer' }, x)) : [el('p', { class: message ? 'erreur' : 'doux' }, message || t('atelier_apercu_vide'))]));
  };
  const previsualiser = () => {
    clearTimeout(minuterie);
    const k = ++numero;
    if (!st.vides && !(st.partie ? st.chercher : st.chercher.trim())) { montrer(null, ''); return; }
    if (!st.toutes && !st.choisies.size) { montrer(null, t('atelier_aucune_variable')); return; }
    minuterie = setTimeout(async () => {
      try {
        const r = await demander('remplacer', { projet: p.id, apercu: true, ...requete() });
        if (k === numero) montrer(r.remplacement, '');
      } catch (e) { if (k === numero) montrer(null, e.message); }
    }, 250);
  };
  const rendre = () => {
    const cherche = el('input', { value: st.chercher, disabled: st.vides, style: 'width:16em', placeholder: st.vides ? t('atelier_vides') : '',
      oninput: (e) => { st.chercher = e.target.value; previsualiser(); } });
    contenu.replaceChildren(el('div', { class: 'grille-champs' },
      el('label', {}, t('atelier_chercher')), el('div', { class: 'ligne' }, cherche,
        el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: st.vides, onchange: (e) => { st.vides = e.target.checked; rendre(); previsualiser(); } }), t('atelier_vides'))),
      el('label', {}, t('atelier_remplacer_par')), el('input', { value: st.remplacer, style: 'width:16em', placeholder: t('atelier_remplacer_vide'),
        oninput: (e) => { st.remplacer = e.target.value; previsualiser(); } }),
      el('label', {}, t('atelier_dans')), el('div', {},
        el('div', { class: 'ligne' },
          el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'atelier-dans', checked: st.toutes, onchange: () => { st.toutes = true; rendre(); previsualiser(); } }), t('atelier_toutes')),
          el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'atelier-dans', checked: !st.toutes, onchange: () => { st.toutes = false; rendre(); previsualiser(); } }), t('atelier_ces_variables'))),
        st.toutes ? null : el('div', { class: 'liste-imputation' }, variables.map((v) => el('label', { class: 'champ' },
          el('input', { type: 'checkbox', checked: st.choisies.has(v.uid), onchange: (e) => { if (e.target.checked) st.choisies.add(v.uid); else st.choisies.delete(v.uid); previsualiser(); } }),
          ...ligneVariable(v))))),
      el('label', {}, t('atelier_comparer')), el('div', { class: 'ligne' },
        el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'atelier-partie', checked: !st.partie, disabled: st.vides, onchange: () => { st.partie = false; previsualiser(); } }), t('atelier_cellule_entiere')),
        el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'atelier-partie', checked: st.partie, disabled: st.vides, onchange: () => { st.partie = true; previsualiser(); } }), t('atelier_partie')),
        el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: st.casse, disabled: st.vides, onchange: (e) => { st.casse = e.target.checked; previsualiser(); } }), t('atelier_casse'))),
      el('label', {}, ''), el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: st.nombres, onchange: (e) => { st.nombres = e.target.checked; previsualiser(); } }), t('atelier_nombres')),
      el('label', {}, t('atelier_apercu')), apercu,
      el('label', {}, ''), el('p', { class: 'note' }, t('atelier_note_remplacer'))));
    if (!st.vides && document.activeElement === document.body) cherche.focus();
  };
  // (the preview of these very fields first, even typed a moment ago: nothing is replaced unseen;
  // a second click while it runs does nothing: a replacement that contains its search text would
  // otherwise be made twice)
  let enCours = false;
  async function valider() {
    if (enCours) return;
    enCours = true;
    try { await remplacer(); } finally { enCours = false; }
  }
  async function remplacer() {
    erreur.textContent = '';
    clearTimeout(minuterie);
    numero += 1;
    if (!st.toutes && !st.choisies.size) { erreur.textContent = t('atelier_aucune_variable'); return; }
    let r;
    try { r = (await demander('remplacer', { projet: p.id, apercu: true, ...requete() })).remplacement; } catch (e) { montrer(null, e.message); erreur.textContent = e.message; return; }
    montrer(r, '');
    if (!r.cellules) { erreur.textContent = liste(r.phrases).join(' '); return; }
    let res;
    try { res = await app.changerDonnees('remplacer', p.id, requete()); } catch (e) { erreur.textContent = e.message; return; }
    d.fermer();
    app.statut(t('atelier_remplace', { n: res.remplacement.cellules }));
  }
  rendre();
  montrer(null, '');
  const d = dialogue({ titre: `${t('atelier_remplacer').replace('…', '')} — ${p.nom}`, contenu: el('div', {}, contenu, erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('atelier_bouton_remplacer'), principal: true, action: () => { valider(); return false; } }] });
}

// ------------------------------------------------------------ categories

// A qualitative variable's categories: renamed (a name another category has merges them), merged
// (the ticked ones into the most frequent), moved up or down, one set as the reference (moved last: in a
// regression, the last category observed is the one the others are compared with, moteur/R/
// regression.R: candidats_regression). Each category with its number of observations. Applied as
// one change: R gets the new name of each category in its old order, and the new order.
export async function dialogueCategories(app, p, uid = null) {
  const qualitatives = p.variables.filter((v) => v.qualitative);
  if (!qualitatives.length) { app.statut(t('atelier_sans_qualitative'), true); return; }
  const st = { uid: qualitatives.some((v) => v.uid === uid) ? uid : qualitatives[0].uid, cats: [], lignes: [], cochees: new Set(), note: '' };
  const contenu = el('div', { class: 'dialogue-categories' });
  const erreur = el('p', { class: 'erreur' });
  const charger = async () => {
    const v = qualitatives.find((x) => x.uid === st.uid);
    st.cats = liste(v.categories).map(String);
    const comptes = new Map();
    try {
      const r = await demander('valeurs', { projet: p.id, variable: st.uid, observations: p.observations.uid });
      for (const x of liste(r.valeurs)) if (x !== null) comptes.set(String(x), (comptes.get(String(x)) || 0) + 1);
    } catch { /* the counts only */ }
    st.lignes = st.cats.map((c, i) => ({ anciens: [i], nom: c, n: comptes.get(c) || 0 }));
    st.cochees = new Set();
    st.note = '';
    rendre();
  };
  const fusionner = (garde, autres) => {
    for (const l of autres) {
      garde.anciens.push(...l.anciens);
      garde.n += l.n;
    }
    st.lignes = st.lignes.filter((l) => !autres.includes(l));
  };
  const deplacer = (i, j) => {
    if (j < 0 || j >= st.lignes.length) return;
    const [l] = st.lignes.splice(i, 1);
    st.lignes.splice(j, 0, l);
    st.note = '';
    rendre();
  };
  const rendre = () => {
    const n = st.lignes.length;
    const lignes = st.lignes.map((l, i) => el('div', { class: 'ligne ligne-categorie' + (i === n - 1 ? ' reference' : '') },
      el('input', { type: 'checkbox', checked: st.cochees.has(l), title: t('atelier_cocher_fusion'), onchange: (e) => { if (e.target.checked) st.cochees.add(l); else st.cochees.delete(l); rendre(); } }),
      el('input', { value: l.nom, class: 'nom-categorie', 'aria-label': t('atelier_nom_categorie'),
        onchange: (e) => {
          const nom = e.target.value.trim();
          const meme = st.lignes.find((x) => x !== l && x.nom.trim() === nom);
          l.nom = nom;
          // (the name of another category: the two are merged, where the other one is)
          if (meme) { fusionner(meme, [l]); st.note = t('atelier_fusion_nom', { nom: entre(nom) }); }
          rendre();
        } }),
      el('span', { class: 'doux effectif' }, t('atelier_obs', { n: l.n }), l.anciens.length > 1 ? ` · ${t('atelier_fusion_de', { noms: l.anciens.map((k) => st.cats[k]).join(', ') })}` : ''),
      el('button', { class: 'icone-seule', title: t('atelier_monter'), disabled: i === 0, onclick: () => deplacer(i, i - 1) }, '▲'),
      el('button', { class: 'icone-seule', title: t('atelier_descendre'), disabled: i === n - 1, onclick: () => deplacer(i, i + 1) }, '▼'),
      i === n - 1 ? el('span', { class: 'badge', title: t('atelier_reference_aide') }, t('atelier_reference'))
        : el('button', { title: t('atelier_reference_aide'), onclick: () => deplacer(i, n - 1) }, t('atelier_mettre_reference'))));
    const cochees = st.lignes.filter((l) => st.cochees.has(l));
    contenu.replaceChildren(el('div', { class: 'grille-champs' },
      el('label', {}, t('atelier_variable')), el('select', { onchange: (e) => { st.uid = Number(e.target.value); charger(); } },
        qualitatives.map((v) => el('option', { value: v.uid, selected: v.uid === st.uid }, v.nom))),
      el('label', {}, t('atelier_les_categories')), el('div', {},
        el('div', { class: 'liste-categories' }, lignes),
        el('div', { class: 'ligne' }, el('button', { disabled: cochees.length < 2, onclick: () => {
          // (into the one with the most observations, the likely spelling, where it is; its name can then change)
          const garde = cochees.reduce((a, b) => (b.n > a.n ? b : a));
          fusionner(garde, cochees.filter((l) => l !== garde));
          st.cochees = new Set();
          st.note = t('atelier_fusion_cochees', { nom: entre(garde.nom) });
          rendre();
        } }, icone('git-merge'), t('atelier_fusionner')),
        el('button', { onclick: () => charger() }, icone('rotate-ccw'), t('atelier_recommencer'))),
        st.note ? el('p', { class: 'note' }, st.note) : null),
      el('label', {}, ''), el('p', { class: 'note' }, t('atelier_note_categories'))));
  };
  async function valider() {
    erreur.textContent = '';
    const noms = st.cats.map((_, k) => st.lignes.find((l) => l.anciens.includes(k)).nom.trim());
    const ordre = [...new Set(st.lignes.map((l) => l.nom.trim()))];
    if (noms.some((x) => !x)) { erreur.textContent = t('atelier_nom_vide'); return; }
    if (noms.every((x, k) => x === st.cats[k]) && ordre.length === st.cats.length && ordre.every((x, k) => x === st.cats[k])) { d.fermer(); return; }
    let res;
    try { res = await app.changerDonnees('categories', p.id, { variable: st.uid, noms, ordre }); } catch (e) { erreur.textContent = e.message; return; }
    d.fermer();
    app.statut(t('atelier_categories_changees', { nom: entre(res.categories.nom), categories: liste(res.categories.categories).join(', '),
      reference: entre(res.categories.reference) }));
  }
  const d = dialogue({ titre: `${t('atelier_categories').replace('…', '')} — ${p.nom}`, contenu: el('div', {}, contenu, erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('appliquer'), principal: true, action: () => { valider(); return false; } }] });
  contenu.replaceChildren(el('p', { class: 'infos attente' }, t('calcul')));
  await charger();
}

// ------------------------------------------------------------ units and descriptions

// Each variable's unit (shown in the axis titles, the Data view's headers, the Methods and Results
// texts) and its description (the Data view's tooltip, the exported workbook, the R script). The
// changed ones go to R as one change. `uid`: the variable whose fields get the keyboard first.
const LIGNES_MAX = 300;
export function dialogueVariables(app, p, uid = null) {
  const valeurs = new Map(p.variables.map((v) => [v.uid, { unite: v.unite || '', description: v.description || '' }]));
  const st = { filtre: '' };
  const erreur = el('p', { class: 'erreur' });
  // (the filter stays while the rows are drawn again under it)
  const infos = el('span', { class: 'infos' });
  const filtre = p.variables.length > 12 ? el('input', { type: 'search', class: 'filtre', placeholder: t('filtrer_variables'),
    oninput: (e) => { st.filtre = e.target.value; rendre(); } }) : null;
  const zone = el('div', { class: 'defile-unites' });
  const contenu = el('div', { class: 'dialogue-unites' }, filtre ? el('div', { class: 'ligne' }, filtre, infos) : null, zone,
    el('p', { class: 'note' }, t('atelier_note_unites')));
  let premier = null;
  const rendre = () => {
    const f = st.filtre.toLowerCase();
    const vues = p.variables.filter((v) => !f || v.nom.toLowerCase().includes(f) || valeurs.get(v.uid).description.toLowerCase().includes(f)).slice(0, LIGNES_MAX);
    premier = null;
    const corps = el('tbody', {}, vues.map((v) => {
      const x = valeurs.get(v.uid);
      const unite = el('input', { value: x.unite, class: 'unite-variable', placeholder: v.qualitative ? '' : t('atelier_unite_exemple'), 'aria-label': `${t('atelier_unite')} · ${v.nom}`,
        oninput: (e) => { x.unite = e.target.value; } });
      if (v.uid === uid) premier = unite;
      return el('tr', {},
        el('td', {}, ...ligneVariable(v)),
        el('td', {}, unite),
        el('td', {}, el('input', { value: x.description, class: 'description-variable', 'aria-label': `${t('atelier_description')} · ${v.nom}`,
          oninput: (e) => { x.description = e.target.value; } })),
        el('td', {}, v.qualitative ? el('button', { onclick: () => { d.fermer(); app.categoriesVariable(p.id, v.uid); } }, icone('tag'), t('atelier_categories')) : ''));
    }));
    zone.replaceChildren(el('table', { class: 'tableau compact' },
      el('thead', {}, el('tr', {}, [t('variables'), t('atelier_unite'), t('atelier_description'), el('span', { class: 'lecteur-seul' }, t('atelier_categories'))].map((h) => el('th', {}, h)))), corps));
    infos.textContent = vues.length === LIGNES_MAX ? t('atelier_premieres', { n: LIGNES_MAX }) : '';
  };
  async function valider() {
    erreur.textContent = '';
    const descriptions = p.variables.filter((v) => {
      const x = valeurs.get(v.uid);
      return x.unite.trim() !== (v.unite || '') || x.description.trim() !== (v.description || '');
    }).map((v) => ({ variable: v.uid, unite: valeurs.get(v.uid).unite.trim(), description: valeurs.get(v.uid).description.trim() }));
    if (!descriptions.length) { d.fermer(); return; }
    try { await app.changerDonnees('decrire_variables', p.id, { descriptions }); } catch (e) { erreur.textContent = e.message; return; }
    d.fermer();
    app.statut(t('atelier_unites_gardees', { n: descriptions.length }));
  }
  rendre();
  const d = dialogue({ titre: `${t('atelier_variables').replace('…', '')} — ${p.nom}`, large: true, contenu: el('div', {}, contenu, erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('appliquer'), principal: true, action: () => { valider(); return false; } }] });
  if (premier) { premier.focus(); premier.scrollIntoView({ block: 'center' }); }
}

// ------------------------------------------------------------ a variable renamed, deleted, of another type

// What a column's header (and a variable of Variable statistics) offers besides the above: Rename…,
// Treat as categories (or as numbers), Delete…, each one change of the data that Ctrl+Z undoes.
export function actionsVariable(app, p, uid) {
  const v = p && p.variables.find((x) => x.uid === uid);
  if (!v) return [];
  const nom = entre(v.nom);
  return ['-',
    { libelle: t('atelier_renommer_de', { nom }), icone: 'pencil', action: () => dialogueRenommer(app, p, uid) },
    v.qualitative ? { libelle: t('atelier_en_nombres', { nom }), icone: 'hash', action: () => changerType(app, p, uid, 'quantitative') }
      : { libelle: t('atelier_en_categories', { nom }), icone: 'tag', action: () => changerType(app, p, uid, 'qualitative') },
    { libelle: t('atelier_supprimer_de', { nom }), icone: 'trash-2', action: () => dialogueSupprimer(app, p, uid) }];
}

// A variable's new name: R says, as it is typed, what follows it (the models' definitions, the
// formulas of computed variables, the documents' references) and what doesn't (an R-code model's
// code), or why the name is refused. The models whose fit has the old name can be fitted again at
// once, their results then taking the new one (otherwise they keep it until fitted again).
export function dialogueRenommer(app, p, uid) {
  const v = p.variables.find((x) => x.uid === uid);
  if (!v) return;
  const st = { nom: v.nom, refaire: true, ajustes: [] };
  const apercu = el('div', { class: 'apercu-remplacer', 'aria-live': 'polite' });
  const erreur = el('p', { class: 'erreur' });
  const ligneRefaire = el('div', {});
  let minuterie = null;
  let numero = 0;
  const montrer = (info, message) => {
    apercu.replaceChildren(...(info ? liste(info.phrases).map((x, i) => el('p', { class: i ? '' : 'resume-remplacer' }, x))
      : [el('p', { class: message ? 'erreur' : 'doux' }, message || t('atelier_renommer_vide'))]));
    st.ajustes = info ? liste(info.ajustes) : [];
    const noms = st.ajustes.map((n) => (p.modeles.find((m) => m.numero === n) || {}).nom).filter(Boolean);
    ligneRefaire.replaceChildren(noms.length ? el('label', { class: 'champ' },
      el('input', { type: 'checkbox', checked: st.refaire, onchange: (e) => { st.refaire = e.target.checked; } }),
      t('atelier_refaire_ajustes', { modeles: noms.join(', ') })) : '');
  };
  const previsualiser = () => {
    clearTimeout(minuterie);
    const k = ++numero;
    if (!st.nom.trim() || st.nom.trim() === v.nom) { montrer(null, ''); return; }
    minuterie = setTimeout(async () => {
      try {
        const r = await demander('renommer_variable', { projet: p.id, variable: uid, nom: st.nom, apercu: true });
        if (k === numero) montrer(r.renommage, '');
      } catch (e) { if (k === numero) montrer(null, e.message); }
    }, 250);
  };
  const champ = el('input', { value: v.nom, class: 'nouveau-nom', style: 'width:22em', 'aria-label': t('atelier_nouveau_nom'),
    oninput: (e) => { st.nom = e.target.value; previsualiser(); },
    onkeydown: (e) => { if (e.key === 'Enter') { e.preventDefault(); valider(); } } });
  let enCours = false;
  async function valider() {
    if (enCours) return;
    enCours = true;
    try { await renommer(); } finally { enCours = false; }
  }
  async function renommer() {
    erreur.textContent = '';
    clearTimeout(minuterie);
    numero += 1;
    const nom = st.nom.trim();
    if (nom === v.nom) { d.fermer(); return; }
    // (what is being typed in a document goes in first: its references follow the new name)
    if (app.viderDocuments) await app.viderDocuments(p.id);
    let res;
    try { res = await app.changerDonnees('renommer_variable', p.id, { variable: uid, nom }); } catch (e) { erreur.textContent = e.message; return; }
    d.fermer();
    const info = res.renommage;
    app.relireDocuments(p.id, liste(info.documents));
    let refaits = 0;
    if (st.refaire) {
      for (const n of liste(info.ajustes)) {
        try {
          await app.occupe(() => app.ajusterDeNouveau(p.id, n));
          refaits += 1;
        } catch (e) { if (e.reponse && e.reponse.arrete) break; }
      }
    }
    app.statut([...liste(info.phrases), refaits ? t('atelier_reajustes', { n: refaits }) : '', t('atelier_annuler')].filter(Boolean).join(' '));
  }
  const d = dialogue({ titre: `${t('atelier_renommer_titre', { nom: entre(v.nom) })} — ${p.nom}`,
    contenu: el('div', {}, el('div', { class: 'grille-champs' },
      el('label', {}, t('atelier_nouveau_nom')), champ,
      el('label', {}, t('atelier_apercu')), apercu,
      el('label', {}, ''), ligneRefaire,
      el('label', {}, ''), el('p', { class: 'note' }, t('atelier_note_renommer'))), erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('atelier_renommer'), principal: true, action: () => { valider(); return false; } }] });
  montrer(null, '');
  champ.select();
}

// Treat as categories, or as numbers: done at once (one Ctrl+Z), the status bar says what it did
// (its categories; the models whose definitions follow; an R-code model that will see it otherwise),
// or why R refused it (a model that has it as Y, as a class, a curve or a random effect; categories
// that aren't numbers).
async function changerType(app, p, uid, type) {
  let res;
  try { res = await app.changerDonnees('type_variable', p.id, { variable: uid, type }); } catch { return; }  // (occupe said why)
  app.statut(`${liste(res.type.phrases).join(' ')} ${t('atelier_annuler')}`);
}

// Delete: asked first, with what it leaves (an R-code model that names it, a computed variable's
// formula, the coordinates); refused, with the reason, while a model uses it.
async function dialogueSupprimer(app, p, uid) {
  const v = p.variables.find((x) => x.uid === uid);
  if (!v) return;
  const titre = `${t('atelier_supprimer_titre', { nom: entre(v.nom) })} — ${p.nom}`;
  let info;
  try { info = (await demander('supprimer_variable', { projet: p.id, variable: uid, apercu: true })).suppression; } catch (e) {
    dialogue({ titre, contenu: el('p', {}, e.message), boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
    return;
  }
  const supprimer = async () => {
    let res;
    try { res = await app.changerDonnees('supprimer_variable', p.id, { variable: uid }); } catch { return; }
    app.statut(`${liste(res.suppression.phrases).join(' ')} ${t('atelier_annuler')}`);
  };
  dialogue({ titre, contenu: el('div', {}, el('p', {}, t('atelier_supprimer_question', { nom: entre(v.nom) })),
    liste(info.phrases).slice(1).map((x) => el('p', { class: 'note' }, x))),
  boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('atelier_supprimer'), principal: true, action: () => { supprimer(); return true; } }] });
}

ajouterTextes({
  atelier_remplacer: 'Chercher et remplacer…', atelier_categories: 'Catégories…', atelier_variables: 'Unités et descriptions…',
  atelier_quantitative: 'quantitative', atelier_qualitative: 'qualitative, {n} catégories', atelier_qualitative_une: 'qualitative, 1 catégorie',
  atelier_chercher: 'Chercher', atelier_vides: 'les cellules vides', atelier_remplacer_par: 'Remplacer par',
  atelier_remplacer_vide: 'rien : la cellule devient vide', atelier_dans: 'Dans', atelier_toutes: 'toutes les variables',
  atelier_ces_variables: 'ces variables :', atelier_comparer: 'Trouver', atelier_cellule_entiere: 'la cellule entière',
  atelier_partie: 'une partie du texte', atelier_casse: 'les majuscules comptent',
  atelier_nombres: 'une variable de texte dont toutes les valeurs deviennent des nombres devient quantitative',
  atelier_apercu: 'Ce qui changera', atelier_apercu_vide: 'Tapez ce qu’il faut chercher, ou cochez « les cellules vides ».',
  atelier_aucune_variable: 'Choisissez au moins une variable.', atelier_bouton_remplacer: 'Remplacer',
  atelier_note_remplacer: 'Une variable qualitative se compare en texte ; une variable quantitative en nombres (−9999, 3,5), sur la cellule entière, et ne prend qu’un nombre ou rien. Une catégorie remplacée par une autre qui existe s’y fond. Un seul Ctrl+Z annule tout le remplacement ; les modèles déjà ajustés gardent leurs résultats jusqu’à ce que vous les ajustiez à nouveau.',
  atelier_remplace: '{n} cellule(s) remplacée(s). Ctrl+Z annule le remplacement.',
  atelier_sans_qualitative: 'Ce projet n’a pas de variable qualitative.', atelier_variable: 'Variable', atelier_les_categories: 'Catégories',
  atelier_cocher_fusion: 'Cocher pour fusionner', atelier_nom_categorie: 'Nom de la catégorie', atelier_obs: '{n} obs.',
  atelier_fusion_de: 'réunit {noms}', atelier_fusion_nom: 'Les catégories du même nom sont réunies en {nom}.',
  atelier_fusion_cochees: 'Fusionnées en {nom}, la plus nombreuse : changez son nom au besoin.',
  atelier_categories_changees: 'Catégories de {nom} : {categories} ; référence des régressions : {reference}. Ctrl+Z annule.',
  atelier_monter: 'Monter', atelier_descendre: 'Descendre', atelier_reference: 'référence',
  atelier_mettre_reference: 'Référence', atelier_reference_aide: 'Dans une régression, la dernière catégorie observée est la catégorie de référence : le coefficient de chacune des autres est son écart à elle.',
  atelier_fusionner: 'Fusionner les catégories cochées', atelier_recommencer: 'Recommencer',
  atelier_note_categories: 'L’ordre est celui des graphiques et des tableaux. Dans une régression, la dernière catégorie observée est la catégorie de référence (« Référence » la met en dernier) : le coefficient de chacune des autres est son écart à elle. Renommer une catégorie du nom d’une autre les fusionne. Un seul Ctrl+Z annule le tout ; les modèles déjà ajustés gardent leurs résultats jusqu’à ce que vous les ajustiez à nouveau.',
  atelier_nom_vide: 'Une catégorie ne peut pas avoir un nom vide.', atelier_annuler: 'Ctrl+Z annule.',
  atelier_unite: 'Unité', atelier_description: 'Description', atelier_unite_exemple: 'ind./ha, m, °C…',
  atelier_premieres: 'les {n} premières : filtrez pour les autres',
  atelier_note_unites: 'L’unité vient dans les titres des axes (« densité (ind./ha) »), l’en-tête de la vue Données, les textes des Méthodes et des résultats ; la description dans la bulle de l’en-tête. Les deux sont enregistrées dans le projet, gardées par Fichier › Exporter les données en Excel (une feuille « Variables »), GeoPackage, jamovi et R, et écrites en commentaire dans l’export vers R.',
  atelier_unites_gardees: 'Unités et descriptions de {n} variable(s) gardées. Ctrl+Z annule.',
  atelier_entete_aide: 'Clic : trier. Clic droit : unité et description, catégories, chercher et remplacer, renommer, traiter en catégories ou en nombres, supprimer.',
  atelier_vides_n: '{n} vides', atelier_distinctes: '{n} distinctes', atelier_resume: 'Résumé des colonnes',
  atelier_resume_aide: 'Sous chaque nom de colonne : le nombre de cellules vides et de valeurs distinctes.',
  atelier_remplacer_dans: 'Chercher et remplacer dans {nom}…', atelier_categories_de: 'Catégories de {nom}…', atelier_unite_de: 'Unité et description de {nom}…',
  atelier_coller_aide: 'Choisissez d’un clic la case d’où coller, puis Ctrl+V : un bloc de cellules copiées dans un tableur s’y colle.',
  atelier_colle: '{n} cellule(s) collée(s). Ctrl+Z annule.', atelier_coller_hors: 'Les cases au-delà du tableau sont laissées.',
  atelier_aide_cellule: 'Double-clic : modifier. Clic, puis Ctrl+V : coller à partir d’ici des cellules copiées dans un tableur.',
  atelier_renommer_de: 'Renommer {nom}…', atelier_en_categories: 'Traiter {nom} en catégories', atelier_en_nombres: 'Traiter {nom} en nombres',
  atelier_supprimer_de: 'Supprimer {nom}…', atelier_renommer_titre: 'Renommer {nom}', atelier_nouveau_nom: 'Nouveau nom', atelier_renommer: 'Renommer',
  atelier_renommer_vide: 'Tapez le nouveau nom : ce qui le suivra s’écrit ici.',
  atelier_refaire_ajustes: 'Ajuster à nouveau tout de suite ce qui a été ajusté avec l’ancien nom ({modeles}), pour que les résultats prennent le nouveau',
  atelier_note_renommer: 'Les définitions des modèles du projet suivent, leurs conditions aussi, de même que les formules des variables calculées et les références des documents à ses termes ({{M1.coef.site}}). Le code d’un modèle de code R n’est pas réécrit. Les étapes des données rejouent le changement de nom à sa place, après celles qui gardent l’ancien. Un seul Ctrl+Z annule le tout, références comprises.',
  atelier_reajustes: '{n} modèle(s) ajusté(s) à nouveau.',
  atelier_supprimer_titre: 'Supprimer {nom}', atelier_supprimer: 'Supprimer',
  atelier_supprimer_question: 'Supprimer la colonne {nom} et ses valeurs ? Ctrl+Z la rend.',
}, {
  atelier_remplacer: 'Find and replace…', atelier_categories: 'Categories…', atelier_variables: 'Units and descriptions…',
  atelier_quantitative: 'quantitative', atelier_qualitative: 'qualitative, {n} categories', atelier_qualitative_une: 'qualitative, 1 category',
  atelier_chercher: 'Find', atelier_vides: 'empty cells', atelier_remplacer_par: 'Replace with',
  atelier_remplacer_vide: 'nothing: the cell becomes empty', atelier_dans: 'In', atelier_toutes: 'all variables',
  atelier_ces_variables: 'these variables:', atelier_comparer: 'Match', atelier_cellule_entiere: 'the whole cell',
  atelier_partie: 'part of the text', atelier_casse: 'match case',
  atelier_nombres: 'a text variable whose values all become numbers becomes quantitative',
  atelier_apercu: 'What will change', atelier_apercu_vide: 'Type what to find, or tick “empty cells”.',
  atelier_aucune_variable: 'Choose at least one variable.', atelier_bouton_remplacer: 'Replace',
  atelier_note_remplacer: 'A qualitative variable is compared as text; a quantitative one as numbers (−9999, 3.5), on the whole cell, and takes only a number or nothing. A category replaced by another one that exists merges into it. A single Ctrl+Z undoes the whole replacement; models already fitted keep their results until you fit them again.',
  atelier_remplace: '{n} cell(s) replaced. Ctrl+Z undoes the replacement.',
  atelier_sans_qualitative: 'This project has no qualitative variable.', atelier_variable: 'Variable', atelier_les_categories: 'Categories',
  atelier_cocher_fusion: 'Tick to merge', atelier_nom_categorie: 'Name of the category', atelier_obs: '{n} obs.',
  atelier_fusion_de: 'joins {noms}', atelier_fusion_nom: 'Categories with the same name are joined into {nom}.',
  atelier_fusion_cochees: 'Merged into {nom}, the most frequent: change its name if needed.',
  atelier_categories_changees: 'Categories of {nom}: {categories}; reference of the regressions: {reference}. Ctrl+Z undoes it.',
  atelier_monter: 'Move up', atelier_descendre: 'Move down', atelier_reference: 'reference',
  atelier_mettre_reference: 'Reference', atelier_reference_aide: 'In a regression, the last category observed is the reference category: the coefficient of each of the others is its difference from it.',
  atelier_fusionner: 'Merge the ticked categories', atelier_recommencer: 'Start again',
  atelier_note_categories: 'The order is that of the plots and tables. In a regression, the last category observed is the reference category (“Reference” moves it last): the coefficient of each of the others is its difference from it. Renaming a category with another one’s name merges them. A single Ctrl+Z undoes it all; models already fitted keep their results until you fit them again.',
  atelier_nom_vide: 'A category can’t have an empty name.', atelier_annuler: 'Ctrl+Z undoes it.',
  atelier_unite: 'Unit', atelier_description: 'Description', atelier_unite_exemple: 'ind./ha, m, °C…',
  atelier_premieres: 'the first {n}: filter for the others',
  atelier_note_unites: 'The unit goes into the axis titles (“density (ind./ha)”), the Data view’s header, the Methods and Results texts; the description into the header’s tooltip. Both are saved in the project, kept by File › Export data as Excel (a “Variables” sheet), GeoPackage, jamovi and R, and written as comments in the export to R.',
  atelier_unites_gardees: 'Units and descriptions of {n} variable(s) kept. Ctrl+Z undoes it.',
  atelier_entete_aide: 'Click: sort. Right-click: unit and description, categories, find and replace, rename, treat as categories or as numbers, delete.',
  atelier_vides_n: '{n} empty', atelier_distinctes: '{n} distinct', atelier_resume: 'Column summary',
  atelier_resume_aide: 'Under each column’s name: the number of empty cells and of distinct values.',
  atelier_remplacer_dans: 'Find and replace in {nom}…', atelier_categories_de: 'Categories of {nom}…', atelier_unite_de: 'Unit and description of {nom}…',
  atelier_coller_aide: 'Click the cell to paste from, then Ctrl+V: a block of cells copied from a spreadsheet goes there.',
  atelier_colle: '{n} cell(s) pasted. Ctrl+Z undoes it.', atelier_coller_hors: 'Cells beyond the table were left out.',
  atelier_aide_cellule: 'Double-click: edit. Click, then Ctrl+V: paste cells copied from a spreadsheet from here on.',
  atelier_renommer_de: 'Rename {nom}…', atelier_en_categories: 'Treat {nom} as categories', atelier_en_nombres: 'Treat {nom} as numbers',
  atelier_supprimer_de: 'Delete {nom}…', atelier_renommer_titre: 'Rename {nom}', atelier_nouveau_nom: 'New name', atelier_renommer: 'Rename',
  atelier_renommer_vide: 'Type the new name: what will follow it is written here.',
  atelier_refaire_ajustes: 'Fit again now what was fitted with the old name ({modeles}), so that the results take the new one',
  atelier_note_renommer: 'The definitions of the project’s models follow, their conditions too, as do the formulas of computed variables and the documents’ references to its terms ({{M1.coef.site}}). An R-code model’s code isn’t rewritten. The data steps replay the renaming in its place, after those that keep the old name. A single Ctrl+Z undoes it all, references included.',
  atelier_reajustes: '{n} model(s) fitted again.',
  atelier_supprimer_titre: 'Delete {nom}', atelier_supprimer: 'Delete',
  atelier_supprimer_question: 'Delete the column {nom} and its values? Ctrl+Z brings it back.',
});
