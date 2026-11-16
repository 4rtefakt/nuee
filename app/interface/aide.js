// Help: the user guide in a panel beside the work (F1, Help › User guide), Help › Tutorials and
// Help › User guide online (the documentation site, in the interface's language), and About, with
// Nuée's websites, where to report a problem and how to cite it. What doesn't need the page (the
// guide's sections and text, the search, the section of what is in front, the addresses, the
// citation): aide_texte.js.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander } from './api.js';
import { VUES } from './panneaux.js';
import { NOUVEAUTES } from './nouveautes.js';
import { SITE, DEPOT, PROBLEMES, DOI, adresseGuide, adresseTutoriels, adresseTutoriel, citation, sectionsGuide, largeurGuide, blocsSection, alias, morceaux,
  motsRecherche, chercher, sectionPour, blocCible } from './aide_texte.js';

// A web page in the computer's browser (main.js sends http(s) links there; in a browser, a tab).
export const ouvrirLien = (url) => { window.open(url); };
const lien = (url, texte) => el('a', { href: url, title: url, onclick: (e) => { e.preventDefault(); ouvrirLien(url); } }, texte);

export function installerAide(app) {
  app.tutoriels = () => ouvrirLien(adresseTutoriels(langueActuelle()));
  app.guideEnLigne = () => ouvrirLien(adresseGuide(langueActuelle()));
  app.apropos = () => dialogueApropos(app);
  app.noterTutoriel = (pid, cle) => { tutoriels.set(pid, cle); };
  app.lienTutoriel = (pid) => lienTutoriel(app, pid);
  VUES.aide = { rendre: (c, ctx, params, loc) => rendreAide(c, app, loc), titre: () => t('guide'), global: true };
}

// ------------------------------------------------------------ the guide's panel

// The full user guide in each language, read once from the engine (moteur/serveur.R: guide, the
// text files beside it): its sections, the width it is wrapped at, and the names its sections are
// referred to by, the longest first ("see Cross-validation").
const guides = new Map();
function guide(langue) {
  if (!guides.has(langue)) {
    guides.set(langue, demander('guide', { langue }, { silencieux: true }).then((r) => {
      const sections = sectionsGuide(r.texte);
      if (!sections.length) throw new Error(t('aide_vide_guide'));
      const renvois = sections.flatMap((s, i) => alias(s.titre).map((a) => [a, i])).sort((a, b) => b[0].length - a[0].length);
      return { sections, largeur: largeurGuide(sections), renvois };
    }).catch((e) => { guides.delete(langue); throw e; }));
  }
  return guides.get(langue);
}
// (read once the engine is free, so that F1 shows it at once even while R computes)
export const prechargerGuide = () => guide(langueActuelle()).catch(() => {});

// The panel (VUES.aide; app.js opens it at the window's right, ouvrirAide): a search field and the
// contents above, one section of the guide, or the sections a search finds, and below, the section
// before and after, the guide online and as a text file. loc.contexte: what was in front when it
// opened ({ vue, type, langue, libelle, chemin }: aide_texte.js, sectionPour and blocCible). Drawn
// once per language: drawn again (a model refitted, the theme), it stays as it is; another language,
// it shows the same section (both guides have the same, in the same order).
function rendreAide(conteneur, app, loc) {
  const langue = langueActuelle();
  const avant = conteneur._aide;
  if (avant && avant.langue === langue) return;
  const st = { langue, section: avant ? avant.section : null, recherche: avant ? avant.recherche : '', g: null, resultats: false };
  const recherche = el('input', { type: 'search', class: 'aide-recherche', placeholder: t('aide_chercher'), 'aria-label': t('aide_chercher'), value: st.recherche,
    spellcheck: 'false' });
  const sommaire = el('select', { class: 'aide-sommaire', 'aria-label': t('aide_sommaire'), title: t('aide_sommaire') });
  const corps = el('div', { class: 'aide-corps', tabindex: 0, role: 'document', 'aria-label': t('guide') });
  const etat = el('p', { class: 'lecteur-seul', role: 'status' });
  const precedente = el('button', { title: t('aide_precedente') }, t('aide_precedente_court'));
  const suivante = el('button', { title: t('aide_suivante') }, t('aide_suivante_court'));
  const pied = el('div', { class: 'aide-pied' }, precedente, suivante, el('span', { class: 'espace' }),
    el('button', { class: 'icone-seule', title: t('guide_en_ligne'), 'aria-label': t('guide_en_ligne'), onclick: () => app.guideEnLigne() }, icone('external-link')),
    el('button', { class: 'icone-seule', title: t('aide_fichier'), 'aria-label': t('aide_fichier'), onclick: () => app.ouvrirGuide() }, icone('file-text')));
  conteneur.classList.add('aide-conteneur');
  conteneur.replaceChildren(el('div', { class: 'aide-panneau' }, el('div', { class: 'aide-outils' }, recherche, sommaire), corps, pied, etat));
  corps.append(el('p', { class: 'infos' }, t('aide_chargement')));

  // a paragraph, a title or a list item's text: its links (the web, the sections it refers to), its
  // lead-in in bold, the words searched for marked
  const mots = () => (st.resultats ? motsRecherche(st.recherche) : []);
  const texte = (x, entree = true) => morceaux(x, { renvois: st.g.renvois, mots: mots(), entree }).map((m) => {
    const contenu = m.marque ? el('mark', {}, m.texte) : m.texte;
    if (m.genre === 'entree') return el('strong', {}, contenu);
    if (m.genre === 'url') return el('a', { href: m.url, title: m.url, onclick: (e) => { e.preventDefault(); ouvrirLien(m.url); } }, contenu);
    if (m.genre === 'renvoi') return el('a', { href: '#', title: st.g.sections[m.section].titre, onclick: (e) => { e.preventDefault(); montrer(m.section); } }, contenu);
    return contenu;
  });
  const blocs = (liste, dansListe = false) => liste.flatMap((b) => {
    if (b.type === 'p') return dansListe && liste.length === 1 && b.textes.length === 1 ? texte(b.textes[0]) : b.textes.map((x) => el('p', {}, texte(x)));
    if (b.type === 'ul' || b.type === 'ol') return [el(b.type, {}, b.elements.map((e) => el('li', {}, blocs(e, true))))];
    if (b.type === 'groupe') return blocs(b.blocs, dansListe);
    if (b.type === 'retrait') return [el('div', { class: 'retrait' }, blocs(b.blocs))];
    if (b.type === 'pre') return [el('pre', {}, texte(b.texte, false))];
    if (b.type === 'dl') return [el('dl', {}, b.elements.map((e) => [el('dt', {}, texte(e.terme, false)), el('dd', {}, texte(e.def, false))]))];
    return [];
  });

  // one section; cible: the paragraph to bring into view (blocCible's context), else the first word
  // searched for, else its top
  function montrer(i, cible = null) {
    st.section = Math.max(0, Math.min(st.g.sections.length - 1, i));
    const s = st.g.sections[st.section];
    sommaire.value = String(st.section);
    precedente.disabled = st.section === 0;
    suivante.disabled = st.section === st.g.sections.length - 1;
    const retour = st.resultats ? el('p', {}, el('a', { href: '#', onclick: (e) => { e.preventDefault(); resultats(); } }, t('aide_retour'))) : null;
    corps.replaceChildren(...[retour, el('h3', {}, texte(s.titre, false)), ...blocs(blocsSection(s.lignes, st.g.largeur))].filter(Boolean));
    etat.textContent = s.titre;
    const elements = [...corps.querySelectorAll('p, li, dd, pre')];
    const k = cible ? blocCible(elements.map((x) => x.textContent), cible) : -1;
    const vers = k >= 0 ? elements[k] : corps.querySelector('mark');
    if (vers && k >= 0) vers.classList.add('aide-cible');
    corps.scrollTop = vers ? Math.max(0, vers.offsetTop - 8) : 0;
  }
  // the sections the search finds, each with its words in context
  function resultats() {
    st.recherche = recherche.value;
    const r = chercher(st.g.sections, st.recherche);
    st.resultats = motsRecherche(st.recherche).length > 0;
    if (!st.resultats) { montrer(st.section ?? 0); return; }
    const n = r.length;
    etat.textContent = n ? t(n === 1 ? 'aide_un_resultat' : 'aide_resultats', { n }) : t('aide_aucun');
    corps.replaceChildren(el('p', { class: 'doux' }, etat.textContent),
      el('ol', { class: 'aide-resultats' }, r.map((x) => el('li', {},
        el('a', { href: '#', onclick: (e) => { e.preventDefault(); montrer(x.section); } }, texte(st.g.sections[x.section].titre, false)),
        el('p', { class: 'doux' }, texte(x.extrait, false))))));
    corps.scrollTop = 0;
  }
  // what is in front (F1 again, the panel open): its section, the search left
  function aller(contexte) {
    if (!st.g) { loc.contexte = contexte; return; }
    recherche.value = '';
    st.recherche = '';
    st.resultats = false;
    montrer(sectionPour(contexte, st.g.sections), contexte);
  }

  let minuterie = null;
  recherche.addEventListener('input', () => { clearTimeout(minuterie); minuterie = setTimeout(() => { if (st.g) resultats(); }, 150); });
  recherche.addEventListener('keydown', (e) => {
    if (e.key === 'Escape' && recherche.value) { e.preventDefault(); e.stopPropagation(); recherche.value = ''; if (st.g) resultats(); }
    if (e.key === 'Enter' && st.g) {
      e.preventDefault();
      clearTimeout(minuterie);
      resultats();
      const premier = corps.querySelector('.aide-resultats a');
      if (premier) premier.click();
    }
  });
  sommaire.addEventListener('change', () => { recherche.value = ''; st.recherche = ''; st.resultats = false; montrer(Number(sommaire.value)); });
  precedente.addEventListener('click', () => montrer(st.section - 1));
  suivante.addEventListener('click', () => montrer(st.section + 1));
  conteneur._aide = { langue, get section() { return st.section; }, get recherche() { return recherche.value; }, aller, focus: () => recherche.focus() };

  guide(langue).then((g) => {
    if (conteneur._aide.langue !== langue) return;  // (another language meanwhile)
    st.g = g;
    sommaire.replaceChildren(...g.sections.map((s, i) => el('option', { value: String(i) }, s.titre)));
    if (st.recherche && motsRecherche(st.recherche).length) resultats();
    else if (st.section !== null) montrer(st.section);
    else montrer(sectionPour(loc.contexte || { langue }, g.sections), loc.contexte);
    delete loc.contexte;
  }, (e) => {
    corps.replaceChildren(el('p', { class: 'erreur' }, t('aide_illisible', { erreur: e.message })),
      el('p', {}, el('button', { onclick: () => app.ouvrirGuide() }, icone('file-text'), t('aide_fichier'))));
  });
}

// ------------------------------------------------------------ an example's tutorial

// The tutorial an example project follows (app.js: ouvrirExemple), on the documentation site: a link
// in the status bar while the project is in front; its × leaves it out for that project.
const tutoriels = new Map();
function lienTutoriel(app, pid) {
  const cle = tutoriels.get(pid);
  if (!cle || !app.projet(pid)) return null;
  const url = adresseTutoriel(cle, langueActuelle());
  return el('span', { class: 'lien-tutoriel' },
    el('a', { href: url, title: url, onclick: (e) => { e.preventDefault(); ouvrirLien(url); } }, icone('book-open'), t('exemple_tutoriel', { titre: t(`tutoriel_${cle}`) })),
    el('button', { class: 'icone-seule', title: t('exemple_tutoriel_cacher'), 'aria-label': t('exemple_tutoriel_cacher'),
      onclick: () => { tutoriels.delete(pid); app.majStatut(); } }, icone('x')));
}

// ------------------------------------------------------------ About

// The versions, the websites, and the sentence to cite Nuée, with Copy.
async function dialogueApropos(app) {
  const v = await app.version();
  const r = app.texteR ? await app.texteR() : null;  // (which R: File › Settings…, reglages.js)
  const phrase = citation(v.application, (NOUVEAUTES.find((n) => n.version === v.application) || {}).date);
  const copie = el('span', { class: 'doux', role: 'status' });
  const copier = () => navigator.clipboard.writeText(phrase).then(() => { copie.textContent = t('apropos_copiee'); }, () => {});
  dialogue({ titre: t('apropos'), contenu: el('div', { class: 'apropos' },
    el('div', { class: 'apropos-tete' }, el('img', { class: 'logo', src: 'nuee.svg', alt: '' }),
      el('div', {}, el('h3', {}, t('titre')), el('p', { class: 'doux' }, t('sous_titre')))),
    el('dl', {}, el('dt', {}, t('version')), el('dd', {}, v.application),
      el('dt', {}, t('moteur')), el('dd', {}, v.R, r ? el('span', { class: 'doux' }, ` · ${r}`) : null),
      el('dt', {}, t('interface')), el('dd', {}, `Electron ${v.electron || '—'} · dockview · Plotly`),
      el('dt', {}, t('licence')), el('dd', {}, 'GPL-3.0'),
      el('dt', {}, t('apropos_site')), el('dd', {}, lien(SITE, 'nuee.io'), ' · ', lien(adresseGuide(langueActuelle()), t('apropos_documentation'))),
      el('dt', {}, t('apropos_code')), el('dd', {}, lien(DEPOT, 'github.com/4rtefakt/nuee'), ' · ', lien(PROBLEMES, t('signaler_probleme')))),
    el('h4', {}, t('apropos_citer')),
    el('p', { class: 'citation-nuee' }, phrase),
    el('div', { class: 'ligne' }, el('button', { onclick: copier }, icone('copy'), t('apropos_copier')), lien(`https://doi.org/${DOI}`, `DOI ${DOI}`), copie),
    el('p', { class: 'note' }, t('apropos_citer_note'))),
  boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
}

ajouterTextes({
  tutoriels: 'Tutoriels', guide_en_ligne: 'Guide en ligne',
  exemple_pommes: 'Pommes (ACP, PLS)', exemple_alouettes: 'Suivi d’alouettes (TRIM, GLMM, distances)', exemple_baci: 'Un seuil arasé (BACI)',
  exemple_tutoriel: 'Tutoriel : {titre}', exemple_tutoriel_cacher: 'Ne plus montrer le tutoriel de ce projet',
  tutoriel_suivi: 'Suivre une population', tutoriel_baci: 'Avant/après avec témoins (BACI)',
  aide_chercher: 'Chercher dans le guide', aide_sommaire: 'Sommaire', aide_precedente: 'Section précédente', aide_suivante: 'Section suivante',
  aide_precedente_court: '‹ Précédente', aide_suivante_court: 'Suivante ›', aide_fichier: 'Le guide en fichier texte', aide_chargement: 'Lecture du guide…',
  aide_illisible: 'Le guide n’a pas pu être lu ({erreur}).', aide_vide_guide: 'le guide est vide', aide_retour: '‹ Retour aux résultats',
  aide_un_resultat: 'Une section contient ces mots.', aide_resultats: '{n} sections contiennent ces mots.', aide_aucun: 'Aucune section ne contient ces mots.',
  apropos_site: 'Site', apropos_documentation: 'documentation (guide et tutoriels)', apropos_code: 'Code source',
  apropos_citer: 'Citer Nuée', apropos_copier: 'Copier', apropos_copiee: 'Copié.',
  apropos_citer_note: 'Le DOI de Zenodo désigne toutes les versions de Nuée. Modèle › Texte pour la section Méthodes écrit cette référence avec celles des méthodes du modèle.',
}, {
  tutoriels: 'Tutorials', guide_en_ligne: 'User guide online',
  exemple_pommes: 'Apples (PCA, PLS)', exemple_alouettes: 'Skylark monitoring (TRIM, GLMM, distance sampling)', exemple_baci: 'A weir removed (BACI)',
  exemple_tutoriel: 'Tutorial: {titre}', exemple_tutoriel_cacher: 'Don’t show this project’s tutorial',
  tutoriel_suivi: 'Monitoring a population', tutoriel_baci: 'Before-after with controls (BACI)',
  aide_chercher: 'Search the guide', aide_sommaire: 'Contents', aide_precedente: 'Previous section', aide_suivante: 'Next section',
  aide_precedente_court: '‹ Previous', aide_suivante_court: 'Next ›', aide_fichier: 'The guide as a text file', aide_chargement: 'Reading the guide…',
  aide_illisible: 'The guide couldn’t be read ({erreur}).', aide_vide_guide: 'the guide is empty', aide_retour: '‹ Back to the results',
  aide_un_resultat: 'One section holds these words.', aide_resultats: '{n} sections hold these words.', aide_aucun: 'No section holds these words.',
  apropos_site: 'Website', apropos_documentation: 'documentation (guide and tutorials)', apropos_code: 'Source code',
  apropos_citer: 'How to cite', apropos_copier: 'Copy', apropos_copiee: 'Copied.',
  apropos_citer_note: 'Zenodo’s DOI stands for every version of Nuée. Model › Text for the Methods section writes this reference with those of the model’s methods.',
});
