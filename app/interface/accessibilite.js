// Accessibility helpers of the interface: what the keyboard and screen readers need beyond the
// menus and dialogs (ui.js) and the project tree (explorateur.js). Their strings are added here.
import { t, ajouterTextes } from './i18n.js';
import { el } from './ui.js';

ajouterTextes({
  texte_document: 'Texte du document', afficher_mode: 'Afficher', remarque: 'Remarque', barre_outils: 'Barre d’outils', onglet_suivant: 'Onglet suivant', onglet_precedent: 'Onglet précédent',
  region_suivante: 'Zone suivante', region_precedente: 'Zone précédente',
  dock_ouvert: 'Ouvert : {titre}', dock_ferme: 'Fermé : {titre}', dock_agrandi: 'Agrandi : {titre}', dock_restaure: 'Rétabli : {titre}',
  dock_flottant: 'Flottant : {titre}', dock_ancre: 'Ancré : {titre}', dock_fenetre: 'Dans une nouvelle fenêtre : {titre}',
  dock_fermer: 'Fermer {titre}',
}, {
  texte_document: 'Text of the document', afficher_mode: 'Show', remarque: 'Remark', barre_outils: 'Toolbar', onglet_suivant: 'Next tab', onglet_precedent: 'Previous tab',
  region_suivante: 'Next area', region_precedente: 'Previous area',
  dock_ouvert: '{titre} opened', dock_ferme: '{titre} closed', dock_agrandi: '{titre} maximized', dock_restaure: '{titre} restored',
  dock_flottant: '{titre} floated', dock_ancre: '{titre} docked', dock_fenetre: '{titre} opened in a new window',
  dock_fermer: 'Close {titre}',
});

// ------------------------------------------------------------ names of the fields

// A field a screen reader would announce without a name gets one from what shows it on screen,
// whichever view or dialog drew it: in a table, its column's header and its row's first cell
// ("Role (weight)"); in a grid of fields (.grille-champs), the label beside it (several fields
// there, a radio group: a group named by it); else its tooltip. Watched as the page changes, so
// that every dialog and view is covered, and those still to come.
const CHAMPS = 'input:not([type="hidden"]), select, textarea, [role="textbox"][contenteditable="true"]';
const texte = (x) => (x ? (x.textContent || '').replace(/\s+/g, ' ').trim() : '');
// (a column's name: not the line under it, the Data view's counts, nor the sort's arrow)
const titreColonne = (th) => { const c = th.cloneNode(true); c.querySelectorAll('.sous-titre, [aria-hidden="true"]').forEach((x) => x.remove()); return texte(c); };
// (a label's own words: not the options of the list it holds)
const texteEtiquette = (l) => { const c = l.cloneNode(true); c.querySelectorAll(CHAMPS).forEach((x) => x.remove()); return texte(c); };
const aUnNom = (x) => x.hasAttribute('aria-label') || x.hasAttribute('aria-labelledby') || (x.labels && [...x.labels].some((l) => texteEtiquette(l)));
let nEtiquettes = 0;
const idDe = (x) => x.id || (x.id = `etiquette-${(nEtiquettes += 1)}`);

function nommer(x) {
  if (aUnNom(x)) return;
  const cellule = x.closest('td, th');
  if (cellule && cellule.closest('table')) {
    const table = cellule.closest('table');
    const entete = table.tHead && table.tHead.rows[0] && table.tHead.rows[0].cells[cellule.cellIndex];
    const premiere = cellule.parentElement.cells[0];
    const colonne = entete && entete !== cellule ? titreColonne(entete) : '';
    const ligne = premiere && premiere !== cellule && !premiere.querySelector(CHAMPS) ? texte(premiere) : '';
    const nom = colonne && ligne ? `${colonne} (${ligne})` : colonne || ligne;
    if (nom) { x.setAttribute('aria-label', nom); return; }
  }
  const grille = x.closest('.grille-champs');
  if (grille) {
    let bloc = x;
    while (bloc.parentElement && bloc.parentElement !== grille) bloc = bloc.parentElement;
    const etiquette = bloc.previousElementSibling;
    if (bloc.parentElement === grille && etiquette && etiquette.tagName === 'LABEL' && !etiquette.querySelector(CHAMPS) && texte(etiquette)) {
      if (bloc !== x && bloc.querySelectorAll(CHAMPS).length > 1) {
        if (!bloc.hasAttribute('role')) { bloc.setAttribute('role', 'group'); bloc.setAttribute('aria-labelledby', idDe(etiquette)); }
      } else { x.setAttribute('aria-labelledby', idDe(etiquette)); return; }
    }
  }
  if (x.title) { x.setAttribute('aria-label', x.title); return; }
  // a list whose first choice says what it is for ("Role…"); a view's list of what it shows
  if (x.tagName === 'SELECT' && x.options.length && /…$|\.\.\.$/.test(x.options[0].text)) x.setAttribute('aria-label', x.options[0].text.replace(/…$|\.\.\.$/, ''));
  else if (x.tagName === 'SELECT' && x.closest('.outils')) x.setAttribute('aria-label', t('afficher_mode'));
}

function nommerDans(racine) {
  if (racine.nodeType !== 1) return;
  if (racine.matches(CHAMPS)) nommer(racine);
  for (const x of racine.querySelectorAll(CHAMPS)) nommer(x);
}
new MutationObserver((changements) => {
  for (const c of changements) for (const n of c.addedNodes) nommerDans(n);
}).observe(document.body, { childList: true, subtree: true });
nommerDans(document.body);

// ------------------------------------------------------------ tables

// A table of the views (panneaux.js: tableau) as a grid for the keyboard, one Tab stop: the arrows
// from cell to cell, the headers' row included; Home and End in a row, Ctrl+Home and Ctrl+End to
// the first and last cells, Page up and Page down by ten rows. On a header, Enter or Space sort
// by it; on a cell, Enter or F2 edit it when it can be (aria-readonly false), else Enter chooses
// its row as a click does, and Space chooses it; Shift+F10 or the menu key give the header's or
// the row's menu. `plus()` draws the next slice of a long table's rows (false when none is left).
// loc.cellule: where the keyboard is, so that it stays there when the table is drawn again (a row
// chosen, a sort, a value edited).
export function grilleClavier(table, loc, plus) {
  table.setAttribute('role', 'grid');
  const ou = loc.cellule || { r: table.rows.length > 1 ? 1 : 0, c: 0 };
  const cellule = (r, c) => { const l = table.rows[Math.max(0, Math.min(r, table.rows.length - 1))]; return l && l.cells[Math.max(0, Math.min(c, l.cells.length - 1))]; };
  while (ou.r >= table.rows.length && plus()) { /* the rows down to where the keyboard was */ }
  const depart = cellule(ou.r, ou.c);
  if (!depart) return;
  depart.tabIndex = 0;
  // (drawn again while it had the keyboard, nothing else having taken it since)
  if (ou.focus) requestAnimationFrame(() => { if (depart.isConnected && (document.activeElement === document.body || !document.activeElement)) depart.focus({ preventScroll: false }); });
  let actuelle = depart;
  const aller = (x) => {
    if (!x) return;
    if (actuelle !== x) actuelle.removeAttribute('tabindex');
    x.tabIndex = 0;
    actuelle = x;
    x.focus();
  };
  table.addEventListener('focusin', (e) => {
    const x = e.target.closest('td, th');
    if (!x || e.target !== x) return;
    if (actuelle !== x) { actuelle.removeAttribute('tabindex'); x.tabIndex = 0; actuelle = x; }
    loc.cellule = { r: x.parentElement.rowIndex, c: x.cellIndex, focus: true };
  });
  // (left for something else: not when the table itself is gone, drawn again)
  table.addEventListener('focusout', () => setTimeout(() => {
    if (table.isConnected && !table.contains(document.activeElement) && loc.cellule) loc.cellule.focus = false;
  }));
  table.addEventListener('keydown', (e) => {
    const x = e.target;
    if (!x.matches || !x.matches('td, th') || e.altKey || e.metaKey) return;
    let r = x.parentElement.rowIndex;
    let c = x.cellIndex;
    const k = e.key;
    const derniere = () => { while (plus()) { /* every row */ } return table.rows.length - 1; };
    if (k === 'ArrowDown') r += 1;
    else if (k === 'ArrowUp') r -= 1;
    else if (k === 'ArrowRight') c += 1;
    else if (k === 'ArrowLeft') c -= 1;
    else if (k === 'PageDown') r += 10;
    else if (k === 'PageUp') r -= 10;
    else if (k === 'Home') { c = 0; if (e.ctrlKey) r = 0; }
    else if (k === 'End') { c = Infinity; if (e.ctrlKey) r = derniere(); }
    else if (x.tagName === 'TH' && (k === 'Enter' || k === ' ')) { e.preventDefault(); x.click(); return; }
    else if ((k === 'Enter' || k === 'F2') && x.getAttribute('aria-readonly') === 'false') {
      e.preventDefault();
      x.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
      return;
    } else if (k === 'Enter' || k === ' ') { e.preventDefault(); x.click(); return; }
    else return;
    if (e.ctrlKey && k !== 'Home' && k !== 'End') return;
    e.preventDefault();
    while (r >= table.rows.length && plus()) { /* the next slice */ }
    aller(cellule(r, c));
  });
}

// ------------------------------------------------------------ panels and tabs

// What dockview says in its live region as panels open and close, in the interface's language
// (its own messages are English).
export const messagesDock = () => ({
  panelOpened: (titre) => t('dock_ouvert', { titre }), panelClosed: (titre) => t('dock_ferme', { titre }),
  groupMaximized: (titre) => t('dock_agrandi', { titre }), groupRestored: (titre) => t('dock_restaure', { titre }),
  groupFloated: (titre) => t('dock_flottant', { titre }), groupDocked: (titre) => t('dock_ancre', { titre }),
  groupPoppedOut: (titre) => t('dock_fenetre', { titre }), closeTab: (titre) => t('dock_fermer', { titre }), closeTabPlain: () => t('fermer'),
});

// A panel's tab: its title and, for the mouse, the close cross. dockview's own tab puts a button
// in the tab, which screen readers can't reach (a tab's content is presentational) and which is an
// interactive control inside another; here the cross is left to the mouse, and the keyboard closes
// a tab with Delete on it (dockview) or Ctrl+W. `data-panneau`: the panel's id, for its menu.
export function ongletDock() {
  const DV = window['dockview-core'];
  const titre = el('div', { class: 'dv-default-tab-content' });
  const croix = el('div', { class: 'dv-default-tab-action', 'aria-hidden': 'true' });
  croix.append(DV.createCloseButton());
  const element = el('div', { class: 'dv-default-tab' }, titre, croix);
  const abonnements = [];
  return {
    element,
    init(params) {
      titre.textContent = params.title || '';
      croix.title = t('fermer_panneau');
      element.dataset.panneau = params.api.id;
      abonnements.push(params.api.onDidTitleChange((e) => { titre.textContent = e.title || ''; }));
      croix.addEventListener('pointerdown', (e) => e.preventDefault());
      croix.addEventListener('click', (e) => {
        if (e.defaultPrevented) return;
        e.preventDefault();
        params.api.close();
      });
    },
    dispose() { for (const a of abonnements) a.dispose(); },
  };
}
