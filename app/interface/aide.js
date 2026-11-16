// Help: Help › Tutorials and Help › User guide online (the documentation site, in the
// interface's language), and About, with Nuée's websites, where to report a problem and how to
// cite it. The addresses and the citation: aide_texte.js.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { NOUVEAUTES } from './nouveautes.js';
import { SITE, DEPOT, PROBLEMES, DOI, adresseGuide, adresseTutoriels, citation } from './aide_texte.js';

// A web page in the computer's browser (main.js sends http(s) links there; in a browser, a tab).
export const ouvrirLien = (url) => { window.open(url); };
const lien = (url, texte) => el('a', { href: url, title: url, onclick: (e) => { e.preventDefault(); ouvrirLien(url); } }, texte);

export function installerAide(app) {
  app.tutoriels = () => ouvrirLien(adresseTutoriels(langueActuelle()));
  app.guideEnLigne = () => ouvrirLien(adresseGuide(langueActuelle()));
  app.apropos = () => dialogueApropos(app);
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
  apropos_site: 'Site', apropos_documentation: 'documentation (guide et tutoriels)', apropos_code: 'Code source',
  apropos_citer: 'Citer Nuée', apropos_copier: 'Copier', apropos_copiee: 'Copié.',
  apropos_citer_note: 'Le DOI de Zenodo désigne toutes les versions de Nuée. Modèle › Texte pour la section Méthodes écrit cette référence avec celles des méthodes du modèle.',
}, {
  tutoriels: 'Tutorials', guide_en_ligne: 'User guide online',
  apropos_site: 'Website', apropos_documentation: 'documentation (guide and tutorials)', apropos_code: 'Source code',
  apropos_citer: 'How to cite', apropos_copier: 'Copy', apropos_copiee: 'Copied.',
  apropos_citer_note: 'Zenodo’s DOI stands for every version of Nuée. Model › Text for the Methods section writes this reference with those of the model’s methods.',
});
