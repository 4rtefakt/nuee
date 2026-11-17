// Exports of KoboToolbox and ODK Central (moteur/R/kobo.R). File › Open (and a file dropped on
// the window) answers with the preview of the import when the file is such an export; File ›
// Import from KoboToolbox or ODK asks for the import. The dialog shows what the import will do
// (R's sentences: the metadata dropped, the names changed, the categories, the 0/1 columns, the
// GPS points and the coordinates, the dates), and lets the person choose the table to analyse
// (the submissions, or a repeat group with columns of its parent tables joined to each row), the
// metadata to keep, an XLSForm form and the language of its labels; each change asks R for the
// preview again. "Open as is" keeps the raw table (the sheet or CSV chosen, unchanged).
import { t, ajouterTextes } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { demander, bureau, deposer as deposerFichier } from './api.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)

// The files the import reads, for the file dialogs: exports, and forms.
export const filtresKobo = () => [{ name: t('kobo_format_exports'), extensions: ['xlsx', 'xls', 'csv', 'zip'] }, { name: t('tous_fichiers'), extensions: ['*'] }];
export const acceptKobo = '.xlsx,.xls,.csv,.zip';

// A file chosen in the browser, sent to the engine: its path there (as app.choisirTableau does).
async function deposer(accept) {
  const f = await new Promise((resolve) => {
    const i = el('input', { type: 'file', accept, onchange: () => resolve(i.files[0]) });
    i.click();
  });
  if (!f) return null;
  return deposerFichier(f);
}
export const deposerKobo = () => deposer(acceptKobo);

// The dialog. `res`: what R answered when the file was opened ({ kobo: the preview, chemin, nom }).
// Resolves to the project opened (imported, or the raw table), or null when cancelled.
export function dialogueKobo(app, res) {
  return new Promise((resolve) => {
    let ap = res.kobo;
    // decochees: the parent columns unticked, by table (all are joined by default)
    const st = { table: ap.table, garder: new Set(liste(ap.garder)), decochees: {}, formulaire: null, langue: null };
    let fini = false;
    const finir = (x) => { if (!fini) { fini = true; resolve(x); } };
    const contenu = el('div', { class: 'dialogue-kobo' });
    const erreur = el('p', { class: 'erreur', 'aria-live': 'polite' });
    const requete = () => {
      const q = { chemin: res.chemin, table: st.table, garder: [...st.garder] };
      if (st.formulaire) {
        q.formulaire = st.formulaire;
        if (st.langue !== null) q.langue = st.langue;
      }
      const parents = liste(ap.parents);
      const decochees = (p) => st.decochees[p.table] || new Set();
      if (parents.some((p) => decochees(p).size)) q.parents = Object.fromEntries(parents.map((p) => [p.table, liste(p.colonnes).filter((c) => !decochees(p).has(c))]));
      return q;
    };
    // the preview of these settings, its sentences dimmed meanwhile; `revenir` puts the settings
    // back when R refuses them (a form that isn't one, or doesn't match the export), the reason
    // shown in the dialog
    let numero = 0;
    const actualiser = async (revenir = null) => {
      const k = ++numero;
      erreur.textContent = '';
      const resume = contenu.querySelector('.kobo-resume');
      if (resume) resume.setAttribute('aria-busy', 'true');
      try {
        const r = await demander('kobo_apercu', requete());
        if (k !== numero) return;
        ap = r;
      } catch (e) {
        if (k !== numero) return;
        if (revenir) revenir();
        erreur.textContent = e.message;
      }
      rendre();
    };
    const choisirFormulaire = async () => {
      const chemin = bureau ? await bureau.choisirFichier([{ name: t('kobo_format_formulaire'), extensions: ['xlsx', 'xls'] }, { name: t('tous_fichiers'), extensions: ['*'] }])
        : await deposer('.xlsx,.xls');
      if (!chemin) return;
      const avant = { formulaire: st.formulaire, langue: st.langue, decochees: st.decochees };
      Object.assign(st, { formulaire: chemin, langue: null, decochees: {} });  // (with the form, the columns take its questions' names)
      actualiser(() => Object.assign(st, avant));
    };
    const coche = (checked, ensemble, valeur, ajouter) => { if (checked === ajouter) ensemble.add(valeur); else ensemble.delete(valeur); };
    const rendre = () => {
      const lignes = [];
      lignes.push(el('label', {}, t('kobo_table')), el('div', { class: 'kobo-tables' }, liste(ap.tables).map((x) => el('label', { class: 'champ' },
        el('input', { type: 'radio', name: 'kobo-table', checked: x.nom === st.table, onchange: () => { st.table = x.nom; st.decochees = {}; actualiser(); } }),
        el('span', {}, x.nom),
        el('span', { class: 'doux' }, ` · ${x.principale ? t('kobo_envois', { n: x.lignes }) : t('kobo_repetition', { parent: x.parent, n: x.lignes })}`)))));
      for (const p of liste(ap.parents)) {
        const dec = st.decochees[p.table] || (st.decochees[p.table] = new Set());
        lignes.push(el('label', {}, t('kobo_joindre', { table: p.table })), el('div', {},
          el('div', { class: 'ligne' },
            el('button', { onclick: () => { dec.clear(); actualiser(); } }, t('tout_cocher')),
            el('button', { onclick: () => { liste(p.colonnes).forEach((c) => dec.add(c)); actualiser(); } }, t('aucune_cocher'))),
          el('div', { class: 'liste-imputation' }, liste(p.colonnes).map((c) => el('label', { class: 'champ' },
            el('input', { type: 'checkbox', checked: !dec.has(c), onchange: (e) => { coche(e.target.checked, dec, c, false); actualiser(); } }), c)))));
      }
      if (liste(ap.metadonnees).length || liste(ap.retirees).length) {
        lignes.push(el('label', {}, t('kobo_garder')), el('div', {},
          liste(ap.metadonnees).map((m) => el('label', { class: 'champ' },
            el('input', { type: 'checkbox', checked: st.garder.has(m.role), onchange: (e) => { coche(e.target.checked, st.garder, m.role, true); actualiser(); } }),
            el('span', {}, m.nom), el('span', { class: 'doux' }, ` · ${liste(m.colonnes).join(', ')}`))),
          el('p', { class: 'note' }, liste(ap.retirees).length ? t('kobo_retirees', { noms: liste(ap.retirees).join(', ') }) : t('kobo_garder_note'))));
      }
      const f = ap.formulaire;
      lignes.push(el('label', {}, `${t('kobo_formulaire')} (${t('facultatif')})`), el('div', {},
        el('div', { class: 'ligne' },
          el('button', { onclick: choisirFormulaire }, icone('file-spreadsheet'), f ? t('kobo_autre_formulaire') : t('kobo_choisir_formulaire')),
          f ? el('span', { title: f.titre || '' }, f.fichier) : null,
          f ? el('button', { onclick: () => { Object.assign(st, { formulaire: null, langue: null, decochees: {} }); actualiser(); } }, t('retirer')) : null),
        f && liste(f.langues).length > 1 ? el('div', { class: 'ligne' }, el('span', { class: 'doux' }, t('kobo_langue')),
          el('select', { onchange: (e) => { st.langue = e.target.value; actualiser(); } },
            liste(f.langues).map((l) => el('option', { value: l, selected: l === f.langue }, l || t('kobo_langue_label'))))) : null,
        el('p', { class: 'note' }, t('kobo_formulaire_note'))));
      lignes.push(el('label', {}, t('kobo_resume')), el('ul', { class: 'kobo-resume' }, liste(ap.phrases).map((x) => el('li', {}, x))));
      contenu.replaceChildren(el('p', {}, t('kobo_intro', { outil: ap.nom_outil, nom: ap.fichier })), el('div', { class: 'grille-champs' }, ...lignes));
    };
    // (a second click while it runs does nothing: it would make a second project)
    let enCours = false;
    const importer = async () => {
      if (enCours) return;
      erreur.textContent = '';
      let r;
      enCours = true;
      try { r = await app.occupe(() => demander('kobo_importer', { ...requete(), nom: res.nom })); } catch (e) { erreur.textContent = e.message; enCours = false; return; }
      const c = r.collecte || {};
      app.statut(t('kobo_importe', { n: c.lignes, k: c.colonnes }) + (c.coordonnees ? t('kobo_importe_coordonnees') : ''));
      finir(r);
      d.fermer();
    };
    // the raw table: the sheet or CSV chosen (a CSV file is its one table; the submissions of an
    // archive need no name)
    const telQuel = async () => {
      fini = true;  // (closing the dialog doesn't resolve: the opening does)
      d.fermer();
      const principale = liste(ap.tables).find((x) => x.principale);
      const table = ap.format === 'zip' ? (principale && st.table === principale.nom ? undefined : st.table)
        : ['xlsx', 'xlsm', 'xls'].includes(ap.format) ? st.table : undefined;
      resolve(await app.occupe(() => demander('ouvrir', { chemin: res.chemin, brut: true, table })).catch(() => null));
    };
    rendre();
    const d = dialogue({ titre: `${t('kobo_titre')} — ${res.nom}`, large: true, contenu: el('div', {}, contenu, erreur), surFermeture: () => finir(null),
      boutons: [{ libelle: t('annuler'), action: () => true },
        { libelle: t('kobo_tel_quel'), action: () => { telQuel(); return false; } },
        { libelle: t('kobo_bouton_importer'), principal: true, action: () => { importer(); return false; } }] });
  });
}

ajouterTextes({
  kobo_importer: 'Importer depuis KoboToolbox ou ODK…', kobo_titre: 'Importer depuis KoboToolbox ou ODK',
  kobo_intro: 'Export {outil} : {nom}. Voici ce que l’import en fera ; « Ouvrir tel quel » ouvre la table choisie sans rien y changer.',
  kobo_table: 'Table à analyser', kobo_envois: 'les envois, {n} ligne(s)', kobo_repetition: 'groupe répété dans « {parent} », {n} ligne(s)',
  kobo_joindre: 'Colonnes de « {table} » jointes à chaque ligne', kobo_garder: 'Métadonnées à garder',
  kobo_retirees: 'Retirées : {noms}.', kobo_garder_note: 'Les autres métadonnées (identifiants internes, appareil, version du formulaire…) sont retirées.',
  kobo_formulaire: 'Formulaire XLSForm', kobo_choisir_formulaire: 'Choisir le formulaire…', kobo_autre_formulaire: 'Un autre formulaire…',
  kobo_langue: 'Langue des libellés', kobo_langue_label: 'colonne label',
  kobo_formulaire_note: 'Le formulaire (le classeur aux feuilles survey et choices, téléchargé depuis KoboToolbox ou ODK Central) donne le type des questions, les libellés des choix dans la langue choisie et leur ordre, qui devient celui des catégories, et les colonnes 0/1 des choix multiples ; les colonnes prennent les noms des questions.',
  kobo_resume: 'Ce qui sera fait', kobo_tel_quel: 'Ouvrir tel quel', kobo_bouton_importer: 'Importer',
  kobo_importe: 'Importé : {n} ligne(s), {k} colonne(s).', kobo_importe_coordonnees: ' Coordonnées du projet réglées (WGS 84) : Graphiques › Carte.',
  kobo_format_zip: 'Export d’ODK Central (.zip)', kobo_format_exports: 'Exports de KoboToolbox et d’ODK (.xlsx, .csv, .zip)',
  kobo_format_formulaire: 'Formulaire XLSForm (.xlsx)',
}, {
  kobo_importer: 'Import from KoboToolbox or ODK…', kobo_titre: 'Import from KoboToolbox or ODK',
  kobo_intro: '{outil} export: {nom}. Here is what the import will make of it; “Open as is” opens the chosen table without changing anything.',
  kobo_table: 'Table to analyse', kobo_envois: 'the submissions, {n} row(s)', kobo_repetition: 'repeat group within “{parent}”, {n} row(s)',
  kobo_joindre: 'Columns of “{table}” joined to each row', kobo_garder: 'Metadata to keep',
  kobo_retirees: 'Dropped: {noms}.', kobo_garder_note: 'The other metadata (internal IDs, device, form version…) are dropped.',
  kobo_formulaire: 'XLSForm form', kobo_choisir_formulaire: 'Choose the form…', kobo_autre_formulaire: 'Another form…',
  kobo_langue: 'Language of the labels', kobo_langue_label: 'label column',
  kobo_formulaire_note: 'The form (the workbook with survey and choices sheets, downloaded from KoboToolbox or ODK Central) gives the questions’ types, the choices’ labels in the chosen language and their order, which becomes the categories’ order, and the 0/1 columns of multiple choices; the columns take the questions’ names.',
  kobo_resume: 'What will be done', kobo_tel_quel: 'Open as is', kobo_bouton_importer: 'Import',
  kobo_importe: 'Imported: {n} row(s), {k} column(s).', kobo_importe_coordonnees: ' The project’s coordinates are set (WGS 84): Plots › Map.',
  kobo_format_zip: 'ODK Central export (.zip)', kobo_format_exports: 'KoboToolbox and ODK exports (.xlsx, .csv, .zip)',
  kobo_format_formulaire: 'XLSForm form (.xlsx)',
});
