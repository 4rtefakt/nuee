// Small DOM helpers shared by the interface: numbers, elements, icons, context menus, dialogs.
import ICONES from './vendor/icones.js';
import { t, langueActuelle } from './i18n.js';

export { fmt, decimal, entreNombres } from './nombres.js';

// A comma typed or pasted in a number field of the French interface. Chromium's number fields
// read the decimal mark of the browser's language (the system's, in the desktop app), not the
// interface's: on a system in English they drop the comma, and "0,5" reads 5. There the comma
// goes in as the point they read ("0.5" reads 0.5 in any language). (The value a view gives
// such a field is written with a point, never by fmt: Chromium shows it in its own language.)
const DECIMALE_NAVIGATEUR = (() => {
  try { return new Intl.NumberFormat(navigator.language).formatToParts(0.5).find((p) => p.type === 'decimal').value; } catch { return '.'; }
})();
document.addEventListener('beforeinput', (e) => {
  const champ = e.target;
  if (DECIMALE_NAVIGATEUR === ',' || langueActuelle() !== 'fr' || !(champ instanceof HTMLInputElement) || champ.type !== 'number') return;
  const texte = e.inputType === 'insertText' ? e.data : e.inputType === 'insertFromPaste' && e.dataTransfer ? e.dataTransfer.getData('text/plain') : null;
  if (!texte || !texte.includes(',')) return;
  e.preventDefault();
  document.execCommand('insertText', false, texte.replace(/,/g, '.'));
}, true);

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

// ------------------------------------------------------------ keyboard or pointer

// Whichever was used last: a menu opened from the keyboard starts on its first item, one opened
// by the mouse has none highlighted (as the system's own menus), and a context menu asked from the
// keyboard (Shift+F10, the menu key) opens by the element in focus. The page then has the class
// `clavier`, for the keyboard's own highlights.
let auClavier = false;
const modeClavier = (oui) => {
  if (auClavier === oui) return;
  auClavier = oui;
  document.documentElement.classList.toggle('clavier', oui);
};
document.addEventListener('keydown', () => modeClavier(true), true);
document.addEventListener('pointerdown', () => modeClavier(false), true);
export const dernierAuClavier = () => auClavier;

// "Édition" and "edition" alike: a menu's letter, a typed first letter
const simple = (s) => String(s).normalize('NFD').replace(/\p{Diacritic}/gu, '').toLowerCase();

// ------------------------------------------------------------ menus

let menuOuvert = null;   // the open menu (a submenu is a menu of its own, its parent's `_sous`)
let retour = null;       // where the keyboard goes back when the menus close
let ouvreur = null;      // the button that opened the menu (aria-expanded)
let barre = null;        // the menu bar whose menu is open: { bouton, voisin(sens) }

const ITEMS = ':scope > [role^="menuitem"]';
const premierItem = (m) => m.querySelector(ITEMS) || m;
const retirerMenu = (m) => { if (!m) return; retirerMenu(m._sous); m._sous = null; m.remove(); };

// The text editors (CodeMirror: a document's text) by their content element. The keyboard given
// back to one, after a menu or a dialog, goes through the editor's own focus(), which keeps its
// cursor where it was: the element's focus() alone puts it at the start of the text, where the
// next key would then go.
const EDITEURS = new WeakMap();
export const inscrireEditeur = (vue) => { EDITEURS.set(vue.contentDOM, vue); };
export function rendreFocus(x, options) {
  const vue = EDITEURS.get(x);
  if (vue) vue.focus(); else x.focus(options);
}

// rendre: the keyboard goes back where it was before the menu (not when another menu takes its place)
export function fermerMenus(rendre = true) {
  const dedans = !!(document.activeElement && document.activeElement.closest && document.activeElement.closest('.menu'));
  retirerMenu(menuOuvert);
  menuOuvert = null;
  barre = null;
  if (ouvreur) { ouvreur.classList.remove('ouvert'); ouvreur.setAttribute('aria-expanded', 'false'); }
  ouvreur = null;
  document.querySelectorAll('.menu-haut.ouvert').forEach((b) => b.classList.remove('ouvert'));
  if (rendre) document.querySelectorAll('#menus.clavier').forEach((n) => n.classList.remove('clavier'));  // (the menu bar's letters: until a command runs)
  if (rendre && dedans && retour && retour.isConnected) rendreFocus(retour, { preventScroll: true });
}

// items: [{ libelle, raccourci, action, actif, coche, sous: [...] } | '-']
// The keyboard in a menu: up and down (Home, End, or an item's first letter) from item to item,
// Enter or Space to run one, right to open a submenu (in a menu of the menu bar, else the next
// menu), left or Escape to go back, Tab to leave. (A disabled item can be reached, not run.)
export function construireMenu(items, surFermeture, parent = null) {
  const m = el('div', { class: 'menu', role: 'menu', tabindex: -1 });
  m._parent = parent;
  m._fermerSous = () => {
    if (!m._sous) return;
    retirerMenu(m._sous);
    m._sous = null;
    m.querySelectorAll(':scope > [aria-expanded="true"]').forEach((x) => x.setAttribute('aria-expanded', 'false'));
  };
  for (const it of items) {
    if (it === '-') { m.append(el('div', { class: 'separateur', role: 'separator' })); continue; }
    const desactive = it.actif === false;
    const coche = it.coche !== undefined && !it.sous;
    const ligne = el('div', { class: 'item' + (desactive ? ' desactive' : '') + (it.sous ? ' parent' : ''),
      role: coche ? 'menuitemcheckbox' : 'menuitem', tabindex: -1, 'aria-disabled': desactive ? 'true' : null,
      'aria-checked': coche ? String(!!it.coche) : null, 'aria-haspopup': it.sous ? 'menu' : null, 'aria-expanded': it.sous ? 'false' : null },
      el('span', { class: 'coche' }, it.coche ? icone('check') : it.icone ? icone(it.icone) : ''),
      el('span', { class: 'libelle' }, it.libelle),
      el('span', { class: 'raccourci' }, it.sous ? icone('chevron-right') : it.raccourci || ''));
    // (a submenu goes in the page, beside its menu: a menu holds only items and separators)
    const ouvrirSous = (focaliser) => {
      if (!m._sous || m._sous._parent !== ligne) {
        m._fermerSous();
        const s = construireMenu(it.sous, surFermeture, ligne);
        s.classList.add('sous-menu');
        s.setAttribute('aria-label', it.libelle);
        document.body.append(s);
        const r = ligne.getBoundingClientRect();
        placer(s, r.right - 2, r.top - 4, r.left);
        m._sous = s;
        ligne.setAttribute('aria-expanded', 'true');
      }
      if (focaliser) premierItem(m._sous).focus();
    };
    ligne._activer = (focaliser) => {
      if (it.sous) { ouvrirSous(focaliser); return; }
      if (desactive) return;
      fermerMenus();
      if (surFermeture) surFermeture();
      it.action();
    };
    ligne.addEventListener('mouseenter', () => {
      ligne.focus({ preventScroll: true });  // (the keyboard goes on from the item under the mouse)
      if (it.sous) ouvrirSous(false); else m._fermerSous();
    });
    ligne.addEventListener('click', (e) => { e.stopPropagation(); ligne._activer(auClavier); });
    m.append(ligne);
  }
  m.addEventListener('keydown', (e) => clavierMenu(e, m));
  return m;
}

function clavierMenu(e, m) {
  const lignes = [...m.querySelectorAll(ITEMS)];
  const i = lignes.indexOf(document.activeElement);
  const ligne = lignes[i];
  const aller = (k) => { if (lignes.length) lignes[(k + lignes.length) % lignes.length].focus(); };
  const k = e.key;
  if (k === 'ArrowDown') aller(i + 1);
  else if (k === 'ArrowUp') aller(i < 0 ? -1 : i - 1);
  else if (k === 'Home' || k === 'PageUp') aller(0);
  else if (k === 'End' || k === 'PageDown') aller(-1);
  else if (k === 'Enter' || k === ' ') { if (ligne) ligne._activer(true); }
  else if (k === 'ArrowRight' && ligne && ligne.hasAttribute('aria-haspopup')) ligne._activer(true);
  else if (k === 'ArrowRight' && !m._parent && barre) barre.voisin(1);
  else if ((k === 'ArrowLeft' || k === 'Escape') && m._parent) { m._parent.parentElement._fermerSous(); m._parent.focus(); }
  else if (k === 'ArrowLeft' && barre) barre.voisin(-1);
  else if (k === 'Escape' && barre) { const b = barre.bouton; fermerMenus(false); b.focus(); }
  else if (k === 'Escape' || k === 'Tab') fermerMenus();
  else if (k.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
    const lettre = simple(k);
    const x = [...lignes.slice(i + 1), ...lignes.slice(0, i + 1)].find((y) => simple(y.querySelector('.libelle').textContent).startsWith(lettre));
    if (x) x.focus();
  } else return;
  e.preventDefault();
  e.stopPropagation();
}

// The menu on screen: from the keyboard on its first (or last) item, from the mouse on none.
function montrer(m, focaliser) {
  menuOuvert = m;
  if (focaliser === 'dernier') { const l = m.querySelectorAll(ITEMS); (l[l.length - 1] || m).focus(); } else if (focaliser) premierItem(m).focus();
  else m.focus({ preventScroll: true });
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

// The element in focus, where the keyboard goes back after a menu (not the page itself, nor a menu)
const enFocus = () => {
  const a = document.activeElement;
  return a && a !== document.body && !(a.closest && a.closest('.menu')) ? a : null;
};

// Asked from the keyboard (Shift+F10, the menu key), a context menu opens by the element in focus.
export function menuContextuel(x, y, items) {
  const avant = enFocus();
  fermerMenus(false);
  retour = avant;
  const m = construireMenu(items);
  m.classList.add('contextuel');
  document.body.append(m);
  if (auClavier && avant) {
    const r = avant.getBoundingClientRect();
    x = r.left + Math.min(16, r.width / 2);
    y = r.top + Math.min(r.height, 28);
  }
  placer(m, x, y);
  montrer(m, auClavier);
}

export function ouvrirMenuSous(bouton, items, focaliser = auClavier) {
  const avant = enFocus();
  fermerMenus(false);
  retour = avant;
  const m = construireMenu(items);
  document.body.append(m);
  const r = bouton.getBoundingClientRect();
  placer(m, r.left, r.bottom);
  bouton.classList.add('ouvert');
  bouton.setAttribute('aria-haspopup', 'menu');
  bouton.setAttribute('aria-expanded', 'true');
  ouvreur = bouton;
  montrer(m, focaliser);
  return m;
}

document.addEventListener('mousedown', (e) => {
  if (menuOuvert && !e.target.closest('.menu') && !e.target.closest('.menu-haut')) fermerMenus(false);
});
// (Escape in a menu is the menu's own: it closes that one only)
document.addEventListener('keydown', (e) => { if (e.key === 'Escape' && menuOuvert && !(e.target.closest && e.target.closest('.menu'))) fermerMenus(); }, true);
window.addEventListener('blur', () => fermerMenus(false));

// ------------------------------------------------------------ menu bar

// The window's menu bar (app.js): menus [{ libelle, items() }], items() giving a menu's items (or a
// promise of them) when it opens; `debut`: what comes before them (the logo). From the keyboard:
// Alt alone or F10 goes to the menu bar and back; left and right go from menu to menu; down,
// Enter or Space open one; Escape goes back where the keyboard was. Alt with a menu's underlined
// letter opens it from anywhere (not on macOS, where Option types letters: F10 there).
const MAC = /Mac/.test(navigator.platform);
let barreActive = null;
export function barreMenus(nav, menus, debut = []) {
  const pris = new Set();
  const boutons = menus.map((d, i) => {
    // its letter: the first of its name that a menu before it doesn't have ("Prédiction": P, then "Prédire": r)
    const k = [...d.libelle].findIndex((c) => /\p{L}/u.test(c) && !pris.has(simple(c)));
    if (k >= 0) pris.add(simple(d.libelle[k]));
    const b = el('button', { class: 'menu-haut', role: 'menuitem', 'aria-haspopup': 'menu', 'aria-expanded': 'false', tabindex: i === 0 ? 0 : -1,
      'aria-keyshortcuts': k >= 0 && !MAC ? `Alt+${d.libelle[k].toUpperCase()}` : null },
    el('span', {}, k >= 0 ? [d.libelle.slice(0, k), el('span', { class: 'cle-acces' }, d.libelle[k]), d.libelle.slice(k + 1)] : d.libelle));
    b._cle = k >= 0 ? simple(d.libelle[k]) : null;
    b._menu = d;
    b.addEventListener('mousedown', (e) => {
      e.preventDefault();
      if (b.classList.contains('ouvert')) fermerMenus(false); else barreActive.ouvrir(b, false);
    });
    b.addEventListener('mouseenter', () => { if (document.querySelector('.menu-haut.ouvert') && !b.classList.contains('ouvert')) barreActive.ouvrir(b, false); });
    return b;
  });
  nav.replaceChildren(...debut, ...boutons);
  nav.setAttribute('aria-label', 'Menu');
  if (!nav._barre) installerBarre(nav);
  nav._barre.boutons = boutons;
  barreActive = nav._barre;
}

function installerBarre(nav) {
  const B = { boutons: [], avant: null };
  nav._barre = B;
  // the roving stop of the menu bar: one menu at a time is in the Tab order
  const placerSur = (b) => { for (const x of B.boutons) x.tabIndex = x === b ? 0 : -1; b.focus(); };
  B.ouvrir = async (b, focaliser) => {
    const items = await b._menu.items();
    if (!b.isConnected) return;
    const m = ouvrirMenuSous(b, items, focaliser);
    m.setAttribute('aria-label', b.textContent);
    if (B.avant && B.avant.isConnected) retour = B.avant;  // after a command, back where the keyboard was before the menu bar
    barre = { bouton: b, voisin: (sens) => {
      const i = B.boutons.indexOf(b);
      const x = B.boutons[(i + sens + B.boutons.length) % B.boutons.length];
      for (const y of B.boutons) y.tabIndex = y === x ? 0 : -1;
      B.ouvrir(x, true);
    } };
  };
  B.entrer = () => {
    B.avant = enFocus();
    nav.classList.add('clavier');
    placerSur(B.boutons.find((x) => x.tabIndex === 0) || B.boutons[0]);
  };
  B.sortir = () => {
    fermerMenus(false);
    nav.classList.remove('clavier');
    const a = B.avant;
    B.avant = null;
    if (a && a.isConnected) rendreFocus(a); else if (nav.contains(document.activeElement)) document.activeElement.blur();
  };
  B.basculer = () => { if (nav.contains(document.activeElement) || (barre && menuOuvert)) B.sortir(); else B.entrer(); };
  B.parCle = (touche) => {
    const b = B.boutons.find((x) => x._cle && x._cle === simple(touche));
    if (!b) return false;
    if (!nav.contains(document.activeElement) && !(barre && menuOuvert)) B.avant = enFocus();
    nav.classList.add('clavier');
    for (const y of B.boutons) y.tabIndex = y === b ? 0 : -1;
    B.ouvrir(b, true);
    return true;
  };
  nav.addEventListener('keydown', (e) => {
    const i = B.boutons.indexOf(document.activeElement);
    if (i < 0) return;
    const aller = (k) => placerSur(B.boutons[(k + B.boutons.length) % B.boutons.length]);
    const k = e.key;
    if (k === 'ArrowRight') aller(i + 1);
    else if (k === 'ArrowLeft') aller(i - 1);
    else if (k === 'Home') aller(0);
    else if (k === 'End') aller(-1);
    else if (k === 'ArrowDown' || k === 'Enter' || k === ' ') B.ouvrir(B.boutons[i], true);
    else if (k === 'ArrowUp') B.ouvrir(B.boutons[i], 'dernier');
    else if (k === 'Escape') B.sortir();
    else if (k.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey && B.boutons.some((x) => x._cle === simple(k))) B.parCle(k);
    else return;
    e.preventDefault();
    e.stopPropagation();
  });
  // the access keys shown while the keyboard is in the menu bar or its menus
  document.addEventListener('focusin', (e) => {
    if (nav.contains(e.target) || (e.target.closest && e.target.closest('.menu'))) return;
    nav.classList.remove('clavier');
    if (!menuOuvert) B.avant = null;
  });
}

// Alt alone (pressed and released with no other key) and F10, anywhere but under a dialog
let altSeul = false;
const sousDialogue = () => !!document.querySelector('.fond-dialogue');
window.addEventListener('keydown', (e) => {
  if (e.key === 'Alt') { if (!e.repeat) altSeul = !e.ctrlKey && !e.shiftKey && !e.metaKey; return; }
  altSeul = false;
  if (!barreActive || sousDialogue()) return;
  if (e.key === 'F10' && !e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey) { e.preventDefault(); e.stopPropagation(); barreActive.basculer(); return; }
  if (!MAC && e.altKey && !e.ctrlKey && !e.metaKey && !e.shiftKey && e.key.length === 1 && barreActive.parCle(e.key)) { e.preventDefault(); e.stopPropagation(); }
}, true);
window.addEventListener('keyup', (e) => {
  if (e.key !== 'Alt' || !altSeul) return;
  altSeul = false;
  if (MAC || !barreActive || sousDialogue()) return;
  e.preventDefault();
  barreActive.basculer();
}, true);
window.addEventListener('pointerdown', () => { altSeul = false; }, true);
window.addEventListener('blur', () => { altSeul = false; });

// ------------------------------------------------------------ dialogs

// Behind a modal dialog (or the command palette) the window is inert, out of reach of the keyboard,
// the mouse and screen readers, until it closes: only the dialog on top is live. (Not the menus a
// dialog opens, nor the live regions that speak for the window: #annonce, dockview's.)
const inertes = new Set();
export function majInert() {
  const fonds = document.querySelectorAll('body > .fond-dialogue');
  const dessus = fonds[fonds.length - 1] || null;
  for (const x of inertes) x.inert = false;
  inertes.clear();
  if (!dessus) return;
  for (const x of document.body.children) {
    if (x === dessus || x.inert || x.matches('.menu, script, [aria-live]')) continue;
    x.inert = true;
    inertes.add(x);
  }
}
new MutationObserver(majInert).observe(document.body, { childList: true });

// What Tab goes through in `racine`, in order: a group of radio buttons is one stop (its checked one).
const FOCUSABLES = 'a[href], button, input:not([type="hidden"]), select, textarea, [tabindex], [contenteditable="true"]';
export function focusables(racine) {
  return [...racine.querySelectorAll(FOCUSABLES)].filter((x) => {
    if (x.tabIndex < 0 || x.disabled || x.closest('[inert]') || !x.getClientRects().length) return false;
    if (x.type !== 'radio' || !x.name) return true;
    const groupe = [...racine.querySelectorAll('input[type="radio"]')].filter((r) => r.name === x.name);
    return x === (groupe.find((r) => r.checked) || groupe[0]);
  });
}

// surFermeture: called once the dialog is gone, whichever way (button, Esc, ×, click outside).
// The keyboard starts in the dialog (its first field, or its main button), Tab and Shift+Tab go
// round inside it, and it goes back where it was when the dialog closes (the R console's prompt,
// for one; in a dialog that opened this one, its button).
let nDialogues = 0;
export function dialogue({ titre, contenu, boutons = [], large = false, surFermeture = null }) {
  const avant = document.activeElement;
  const fond = el('div', { class: 'fond-dialogue' });
  let ferme = false;
  const fermer = () => {
    if (ferme) return;
    ferme = true;
    fond.remove();
    majInert();  // (now: what had the keyboard can have it again)
    document.removeEventListener('keydown', touche, true);
    const dessus = [...document.querySelectorAll('.fond-dialogue')].pop();
    if (avant && avant !== document.body && avant.isConnected && (!dessus || dessus.contains(avant))) rendreFocus(avant);
    if (surFermeture) surFermeture();
  };
  // Esc closes the dialog on top only (another may be open under it; a menu over it, or a table
  // cell being edited, takes its own Escape)
  const touche = (e) => {
    if (fond !== [...document.querySelectorAll('.fond-dialogue')].pop()) return;
    if (e.target.closest && e.target.closest('.menu, .edition-cellule')) return;
    if (e.key === 'Escape') {
      e.stopPropagation();
      fermer();
    } else if (e.key === 'Tab') {
      const liste = focusables(boite);
      const i = liste.indexOf(document.activeElement);
      const dedans = boite.contains(document.activeElement);
      if (!liste.length) e.preventDefault();
      else if (e.shiftKey && (i === 0 || !dedans)) { e.preventDefault(); liste[liste.length - 1].focus(); }
      else if (!e.shiftKey && (i === liste.length - 1 || !dedans)) { e.preventDefault(); liste[0].focus(); }
    }
  };
  const pied = el('div', { class: 'pied-dialogue' }, boutons.map((b) => el('button', {
    class: b.principal ? 'principal' : '', onclick: () => { if (b.action() !== false) fermer(); } }, b.libelle)));
  nDialogues += 1;
  const boite = el('div', { class: 'dialogue' + (large ? ' large' : ''), role: 'dialog', 'aria-modal': 'true', 'aria-labelledby': `titre-dialogue-${nDialogues}` },
    el('div', { class: 'tete-dialogue' }, el('h2', { id: `titre-dialogue-${nDialogues}` }, titre),
      el('button', { class: 'icone-seule', onclick: fermer, title: `${t('fermer')} (Esc)`, 'aria-label': t('fermer') }, icone('x'))),
    el('div', { class: 'corps-dialogue' }, contenu), pied);
  fond.append(boite);
  // a click beside it closes a dialog that only shows something; one with fields to fill in (New
  // model, Export to R...) stays, so that a click beside it doesn't lose what was chosen (Esc or ×)
  const aDesChamps = () => boite.querySelector('.corps-dialogue input, .corps-dialogue select, .corps-dialogue textarea');
  fond.addEventListener('mousedown', (e) => { if (e.target === fond && !aDesChamps()) fermer(); });
  document.addEventListener('keydown', touche, true);
  document.body.append(fond);
  const corps = boite.querySelector('.corps-dialogue');
  const champs = focusables(corps);
  if (!champs.length) corps.tabIndex = 0;  // (a text that scrolls: the keyboard scrolls it)
  const premier = champs.find((x) => x.matches('input, select, textarea'))
    || pied.querySelector('button.principal') || pied.querySelector('button') || boite.querySelector('.tete-dialogue button');
  if (premier) premier.focus();
  return { fermer, boite };
}
