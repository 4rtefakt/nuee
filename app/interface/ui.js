// Small DOM helpers shared by the interface: elements, icons, context menus, dialogs.
import ICONES from './vendor/icones.js';

export const fmt = (v, d = 3) => (v === null || v === undefined || Number.isNaN(v) ? '' : Number(v).toFixed(d));

export function el(tag, attrs = {}, ...enfants) {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === 'class') e.className = v;
    else if (k === 'style' && typeof v === 'object') Object.assign(e.style, v);
    else if (k.startsWith('on') && typeof v === 'function') e.addEventListener(k.slice(2), v);
    else if (v !== undefined && v !== null && v !== false) e.setAttribute(k, v === true ? '' : v);
  }
  for (const c of enfants.flat(Infinity)) {
    if (c !== null && c !== undefined && c !== false) e.append(c.nodeType ? c : document.createTextNode(String(c)));
  }
  return e;
}

export function icone(nom, classe = '') {
  const s = el('span', { class: 'icone ' + classe, 'aria-hidden': 'true' });
  s.innerHTML = ICONES[nom] || '';
  return s;
}

// ------------------------------------------------------------ menus

let menuOuvert = null;

export function fermerMenus() {
  if (menuOuvert) menuOuvert.remove();
  menuOuvert = null;
  document.querySelectorAll('.menu-haut.ouvert').forEach((b) => b.classList.remove('ouvert'));
}

// items: [{ libelle, raccourci, action, actif, coche, sous: [...] } | '-']
export function construireMenu(items, surFermeture) {
  const m = el('div', { class: 'menu', role: 'menu' });
  for (const it of items) {
    if (it === '-') { m.append(el('div', { class: 'separateur' })); continue; }
    const desactive = it.actif === false;
    const ligne = el('div', { class: 'item' + (desactive ? ' desactive' : '') + (it.sous ? ' parent' : ''), role: 'menuitem', tabindex: -1 },
      el('span', { class: 'coche' }, it.coche ? icone('check') : it.icone ? icone(it.icone) : ''),
      el('span', { class: 'libelle' }, it.libelle),
      el('span', { class: 'raccourci' }, it.sous ? icone('chevron-right') : it.raccourci || ''));
    if (it.sous) {
      ligne.addEventListener('mouseenter', () => {
        m.querySelectorAll(':scope > .sous-menu').forEach((s) => s.remove());
        const s = construireMenu(it.sous, surFermeture);
        s.classList.add('sous-menu');
        m.append(s);
        const r = ligne.getBoundingClientRect();
        placer(s, r.right - 2, r.top - 4, r.left);
      });
    } else {
      ligne.addEventListener('mouseenter', () => m.querySelectorAll(':scope > .sous-menu').forEach((s) => s.remove()));
      ligne.addEventListener('click', (e) => {
        e.stopPropagation();
        if (desactive) return;
        fermerMenus();
        if (surFermeture) surFermeture();
        it.action();
      });
    }
    m.append(ligne);
  }
  return m;
}

function placer(m, x, y, xAlternatif) {
  m.style.left = '0px';
  m.style.top = '0px';
  if (!m.isConnected) document.body.append(m);
  const r = m.getBoundingClientRect();
  const vw = window.innerWidth;
  const vh = window.innerHeight;
  let gx = x;
  if (gx + r.width > vw) gx = xAlternatif !== undefined ? xAlternatif - r.width : vw - r.width - 4;
  m.style.left = Math.max(2, gx) + 'px';
  m.style.top = Math.max(2, Math.min(y, vh - r.height - 4)) + 'px';
}

export function menuContextuel(x, y, items) {
  fermerMenus();
  const m = construireMenu(items);
  m.classList.add('contextuel');
  document.body.append(m);
  placer(m, x, y);
  menuOuvert = m;
}

export function ouvrirMenuSous(bouton, items) {
  fermerMenus();
  const m = construireMenu(items);
  document.body.append(m);
  const r = bouton.getBoundingClientRect();
  placer(m, r.left, r.bottom);
  bouton.classList.add('ouvert');
  menuOuvert = m;
}

document.addEventListener('mousedown', (e) => {
  if (menuOuvert && !e.target.closest('.menu') && !e.target.closest('.menu-haut')) fermerMenus();
});
document.addEventListener('keydown', (e) => { if (e.key === 'Escape') fermerMenus(); }, true);
window.addEventListener('blur', fermerMenus);

// ------------------------------------------------------------ dialogs

export function dialogue({ titre, contenu, boutons = [], large = false }) {
  const fond = el('div', { class: 'fond-dialogue' });
  const fermer = () => { fond.remove(); document.removeEventListener('keydown', touche, true); };
  const touche = (e) => { if (e.key === 'Escape') { e.stopPropagation(); fermer(); } };
  const pied = el('div', { class: 'pied-dialogue' }, boutons.map((b) => el('button', {
    class: b.principal ? 'principal' : '', onclick: () => { if (b.action() !== false) fermer(); } }, b.libelle)));
  const boite = el('div', { class: 'dialogue' + (large ? ' large' : ''), role: 'dialog' },
    el('div', { class: 'tete-dialogue' }, el('h2', {}, titre),
      el('button', { class: 'icone-seule', onclick: fermer, title: 'Esc' }, icone('x'))),
    el('div', { class: 'corps-dialogue' }, contenu), pied);
  fond.append(boite);
  fond.addEventListener('mousedown', (e) => { if (e.target === fond) fermer(); });
  document.addEventListener('keydown', touche, true);
  document.body.append(fond);
  return { fermer, boite };
}
