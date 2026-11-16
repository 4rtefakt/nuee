// Accessibility helpers of the interface: what the keyboard and screen readers need beyond the
// menus and dialogs (ui.js) and the project tree (explorateur.js). Their strings are added here.
import { t, ajouterTextes } from './i18n.js';
import { el } from './ui.js';

ajouterTextes({
  barre_outils: 'Barre d’outils', onglet_suivant: 'Onglet suivant', onglet_precedent: 'Onglet précédent',
  region_suivante: 'Zone suivante', region_precedente: 'Zone précédente',
  dock_ouvert: 'Ouvert : {titre}', dock_ferme: 'Fermé : {titre}', dock_agrandi: 'Agrandi : {titre}', dock_restaure: 'Rétabli : {titre}',
  dock_flottant: 'Flottant : {titre}', dock_ancre: 'Ancré : {titre}', dock_fenetre: 'Dans une nouvelle fenêtre : {titre}',
  dock_fermer: 'Fermer {titre}',
}, {
  barre_outils: 'Toolbar', onglet_suivant: 'Next tab', onglet_precedent: 'Previous tab',
  region_suivante: 'Next area', region_precedente: 'Previous area',
  dock_ouvert: '{titre} opened', dock_ferme: '{titre} closed', dock_agrandi: '{titre} maximized', dock_restaure: '{titre} restored',
  dock_flottant: '{titre} floated', dock_ancre: '{titre} docked', dock_fenetre: '{titre} opened in a new window',
  dock_fermer: 'Close {titre}',
});

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
