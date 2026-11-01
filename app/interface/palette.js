// Command palette (Ctrl+K): every item of the menus, found by typing part of its name or of
// its menu's ("vip", "export r", "anglais"), run with Enter or a click.
import { t } from './i18n.js';
import { el, icone } from './ui.js';
import { menus } from './commandes.js';

// "Exporter vers R" and "exporter vers r" alike, accents too
const simple = (s) => String(s).normalize('NFD').replace(/\p{Diacritic}/gu, '').toLowerCase();

export function ouvrirPalette(app, C) {
  if (document.querySelector('.fond-dialogue')) return;
  // the menu items as they are now (enabled or not, ticked), under their menu: "Graphiques › VIP"
  const entrees = [];
  const parcourir = (items, chemin) => {
    for (const x of items) {
      if (!x || x === '-') continue;
      if (x.sous) parcourir(x.sous, [...chemin, x.libelle]);
      else entrees.push({ ...x, chemin: chemin.join(' › '), cle: simple(`${chemin.join(' ')} ${x.libelle}`) });
    }
  };
  for (const d of menus(app, C)) parcourir(d.items(), [t(d.cle)]);

  const fond = el('div', { class: 'fond-dialogue palette-fond' });
  const saisie = el('input', { type: 'search', class: 'palette-saisie', placeholder: t('palette_aide') });
  const liste = el('div', { class: 'palette-liste', role: 'listbox' });
  let trouvees = [];
  let rang = 0;
  const fermer = () => fond.remove();
  const lancer = (x) => {
    if (!x || x.actif === false) return;
    fermer();
    x.action();
  };
  const rendre = () => {
    const mots = simple(saisie.value).split(/\s+/).filter(Boolean);
    const debut = (x) => mots.length && simple(x.libelle).startsWith(mots[0]);
    trouvees = entrees.filter((x) => mots.every((m) => x.cle.includes(m)));
    trouvees = [...trouvees.filter(debut), ...trouvees.filter((x) => !debut(x))];  // names that start with the text first
    rang = Math.min(rang, Math.max(0, trouvees.length - 1));
    liste.replaceChildren(...(trouvees.length ? trouvees.map((x, i) => el('div', {
      class: 'palette-item' + (i === rang ? ' actuel' : '') + (x.actif === false ? ' inactif' : ''), role: 'option',
      onmousedown: (e) => e.preventDefault(), onmousemove: () => { if (rang !== i) marquer(i); }, onclick: () => lancer(x) },
    el('span', { class: 'palette-icone' }, x.coche ? icone('check') : x.icone ? icone(x.icone) : null),
    el('span', { class: 'palette-libelle' }, x.libelle), el('span', { class: 'palette-chemin' }, x.chemin),
    el('span', { class: 'raccourci' }, x.raccourci || ''))) : [el('p', { class: 'infos' }, t('palette_rien'))]));
  };
  const marquer = (i) => {  // the item Enter runs
    rang = i;
    liste.querySelectorAll('.palette-item').forEach((d, k) => d.classList.toggle('actuel', k === i));
  };
  saisie.addEventListener('input', () => { rang = 0; rendre(); });
  saisie.addEventListener('keydown', (e) => {
    e.stopPropagation();
    if (e.key === 'Escape') fermer();
    else if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
      e.preventDefault();
      if (!trouvees.length) return;
      marquer((rang + (e.key === 'ArrowDown' ? 1 : -1) + trouvees.length) % trouvees.length);
      liste.querySelector('.actuel').scrollIntoView({ block: 'nearest' });
    } else if (e.key === 'Enter') { e.preventDefault(); lancer(trouvees[rang]); }
  });
  fond.addEventListener('mousedown', (e) => { if (e.target === fond) fermer(); });
  fond.append(el('div', { class: 'palette', role: 'dialog' }, saisie, liste));
  document.body.append(fond);
  rendre();
  saisie.focus();
}
