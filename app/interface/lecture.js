// The reader's page of a reproducibility package's link (https://nuee.io/app/?doi=<doi> or
// ?record=<id>, the browser version): the package fetched from Zenodo and read in the page
// (relancer.js: paquetDuLien, lirePaquetZip), then its main document shown at once, as its authors
// wrote it, its numbers, plots and tables already filled in (documents/<name>.html, which the package
// holds: paquet.js, documents.js documentPourPaquet), on a phone's screen as on a computer's. R isn't
// started for it: it starts when the reader asks for the whole project (Open the whole project: the
// project as its authors left it) or for the rerun of its models (Rerun the models: each fitted again
// and compared with the deposited results, relancer.js). A package without such a document (no
// document, its authors chose none, or a package of Nuée before 1.2) opens as the link did before:
// the whole project, rerun. What keeps the link from opening is said in the application, as before.
//
// The document comes from a file anyone may have deposited: it is shown in a frame that runs no
// script and reaches nothing of this page (sandbox without allow-scripts or allow-same-origin), under
// the page's own security policy; its links to the web open in a new tab.
//   demarrerLecture(app, { doi, record }, demarrer)   `demarrer`: what starts the application (app.js)
import { t, ajouterTextes } from './i18n.js';
import { el, icone } from './ui.js';
import { paquetDuLien, ouvrirPaquetDuLien, direRefusLien } from './relancer.js';

// A document's page for the frame: its links to the web opening in a new tab (the frame can't be
// left: the page's policy keeps other sites out of its frames), the others (within the document) as
// they are.
const pourCadre = (html) => String(html).replace(/<a href="(https?:[^"]*)"/gi, '<a target="_blank" rel="noopener noreferrer" href="$1"');

// The authors as Zenodo gives them (creators: { name: "Family, Given" }, or InvenioRDM's
// person_or_org), "Given Family"
const auteurs = (r) => [].concat((r.metadata && r.metadata.creators) || [])
  .map((c) => String(c.name || (c.person_or_org && c.person_or_org.name) || '').trim())
  .filter(Boolean).map((n) => (/^[^,]+,\s*[^,]+$/.test(n) ? n.replace(/^([^,]+),\s*(.+)$/, '$2 $1') : n));

export async function demarrerLecture(app, { doi = null, record = null }, demarrer) {
  const quoi = record ? t('relance_lien_record', { record }) : t('relance_lien_doi', { doi });
  const appli = document.getElementById('appli');
  const attente = el('p', { class: 'attente-lecture', role: 'status' }, t('lecture_attente'));
  const page = el('div', { id: 'lecture', class: 'lecture' }, el('div', { class: 'debut-lecture' }, el('img', { class: 'logo', src: 'nuee.svg', alt: '' }), attente));
  appli.inert = true;
  document.body.append(page);
  const quitter = () => { page.remove(); appli.inert = false; document.title = t('titre'); };
  // (the application started, then the link's package opened in it: the engine's R starts here)
  const ouvrir = async (x, relancer) => {
    quitter();
    await demarrer();
    try {
      await ouvrirPaquetDuLien(app, x, { relancer });
    } catch (e) { direRefusLien(app, quoi, e); }
  };
  let x;
  try {
    x = await paquetDuLien({ doi, record }, undefined, (m) => { attente.textContent = m; });
  } catch (e) {
    quitter();
    await demarrer();
    direRefusLien(app, quoi, e);
    return;
  }
  if (!x.paquet.principal) { await ouvrir(x, true); return; }

  const titre = x.lien.titre || x.f.key.replace(/\.zip$/i, '');
  const noms = auteurs(x.r);
  const cadre = el('iframe', { class: 'document-lecture', sandbox: 'allow-popups allow-popups-to-escape-sandbox', referrerpolicy: 'no-referrer' });
  const montrer = async (chemin) => {
    const html = await x.paquet.lire(chemin);
    cadre.title = chemin.replace(/^documents\//, '').replace(/\.html$/i, '');
    cadre.srcdoc = pourCadre(html || '');
  };
  // (the main document first, then the others)
  const documents = [x.paquet.principal, ...x.paquet.documents.filter((d) => d !== x.paquet.principal)];
  const choix = documents.length > 1
    ? el('select', { class: 'choix-lecture', 'aria-label': t('lecture_document'), onchange: (e) => { montrer(e.target.value); } },
      documents.map((d) => el('option', { value: d, selected: d === x.paquet.principal }, d.replace(/^documents\//, '').replace(/\.html$/i, ''))))
    : null;
  const doiLien = x.lien.doi ? el('a', { href: `https://doi.org/${x.lien.doi}`, target: '_blank', rel: 'noopener' }, `doi:${x.lien.doi}`) : null;
  page.replaceChildren(
    el('header', { class: 'tete-lecture' },
      el('img', { class: 'logo', src: 'nuee.svg', alt: 'Nuée' }),
      el('div', { class: 'titre-lecture' },
        el('h1', { title: titre }, titre),
        noms.length || doiLien ? el('p', { class: 'auteurs-lecture' }, noms.join(', '), noms.length && doiLien ? ' · ' : '', doiLien) : null),
      el('div', { class: 'actions-lecture' },
        choix,
        el('button', { class: 'ouvrir-lecture', title: t('lecture_ouvrir_aide'), onclick: () => ouvrir(x, false) }, icone('folder-open'), t('lecture_ouvrir')),
        el('button', { class: 'principal relancer-lecture', title: t('lecture_relancer_aide'), onclick: () => ouvrir(x, true) }, icone('refresh-cw'), t('lecture_relancer')))),
    cadre);
  document.title = `${titre} — Nuée`;
  await montrer(x.paquet.principal);
}

ajouterTextes({
  lecture_attente: 'Ouverture du lien…',
  lecture_document: 'Document',
  lecture_ouvrir: 'Ouvrir tout le projet', lecture_relancer: 'Relancer les modèles',
  lecture_ouvrir_aide: 'Le projet dans Nuée, tel que ses auteurs l’ont laissé : ses données, ses modèles et ses documents. R démarre dans cette page (environ une demi-minute à une première visite).',
  lecture_relancer_aide: 'Chaque modèle ajusté de nouveau, et comparé aux résultats que ses auteurs ont déposés. R démarre dans cette page (environ une demi-minute à une première visite).',
}, {
  lecture_attente: 'Opening the link…',
  lecture_document: 'Document',
  lecture_ouvrir: 'Open the whole project', lecture_relancer: 'Rerun the models',
  lecture_ouvrir_aide: 'The project in Nuée, as its authors left it: its data, models and documents. R starts in this page (about half a minute at a first visit).',
  lecture_relancer_aide: 'Every model fitted again, and compared with the results its authors deposited. R starts in this page (about half a minute at a first visit).',
});
