// R console (Affichage › Console R, Ctrl+J): the log of the engine calls behind each action
// of the interface, and a prompt that runs R in the application's own R session
// (moteur/console.R). m and p are the model and the project shown; P1, M7... the open
// projects and models, by the names the log gives them.
import { t, lire, ecrire } from './i18n.js';
import { el, icone } from './ui.js';

// the last click anywhere: what the prompt's focus guard compares with the last key typed
let dernierClic = 0;
document.addEventListener('pointerdown', () => { dernierClic = Date.now(); }, true);

// The panel keeps its content between redraws (selections and refits redraw every view).
export function rendreConsole(conteneur, ctx) {
  if (conteneur._console) {
    conteneur._console.entete();
    return;
  }
  const etat = { n: 0, historique: [], position: -1, appels: lire('console_appels') !== '0' };
  try { etat.historique = JSON.parse(lire('historique_r') || '[]'); } catch { etat.historique = []; }
  const journal = el('div', { class: 'journal-r' + (etat.appels ? '' : ' sans-appels') });
  const saisie = el('textarea', { class: 'saisie-r', rows: 1, spellcheck: false, placeholder: t('console_invite') });
  const actif = el('span', { class: 'infos' });
  const appels = el('input', { type: 'checkbox', checked: etat.appels, onchange: (e) => {
    etat.appels = e.target.checked;
    journal.classList.toggle('sans-appels', !etat.appels);
    ecrire('console_appels', etat.appels ? '1' : '0');
  } });
  const libelleAppels = el('span', {});
  const effacer = el('button', { onclick: () => journal.replaceChildren() }, icone('trash-2'));
  const copier = el('button', { onclick: () => navigator.clipboard.writeText([...journal.querySelectorAll('.r-appel, .r-commande')]
    .map((x) => x.dataset.code).join('\n\n')).then(() => ctx.statut(t('journal_copie'))) }, icone('copy'));

  const ajouter = (entrees) => {
    const enBas = journal.scrollHeight - journal.scrollTop - journal.clientHeight < 40;
    for (const e of entrees) {
      etat.n = Math.max(etat.n, e.n);
      if (e.type === 'image') {
        journal.append(el('div', { class: 'r-image' }, el('img', { src: e.texte })));
        continue;
      }
      const texte = e.type === 'commande' ? e.texte.split('\n').map((l, i) => (i ? '+ ' : '> ') + l).join('\n')
        : e.type === 'erreur' ? `${t('erreur')} : ${e.texte}` : e.type === 'avertissement' ? `${t('avertissement')} : ${e.texte}` : e.texte;
      // (resultat: what R prints of a model the interface fitted; hidden with the calls)
      const bloc = el('pre', { class: 'r-' + e.type, title: e.heure }, texte);
      if (e.type === 'appel' || e.type === 'commande') bloc.dataset.code = e.texte;
      journal.append(bloc);
    }
    while (journal.childElementCount > 1500) journal.firstElementChild.remove();
    if (enBas || entrees.some((e) => e.type === 'commande')) journal.scrollTop = journal.scrollHeight;
  };

  // the actions of the interface (and of other windows) come in by polling, from the moment
  // the panel is in the page (not yet when it is first drawn) until it is closed
  let affichee = false;
  const charger = async (force = false) => {
    if (!conteneur.isConnected) {
      if (affichee) clearInterval(minuterie);
      return;
    }
    affichee = true;
    if (!force && (document.hidden || etat.occupe)) return;
    try {
      let r = await ctx.demander('journal', { depuis: etat.n }, { silencieux: true });
      if (r.dernier < etat.n) {  // a new R engine: its log starts again from 1
        etat.n = 0;
        journal.append(el('pre', { class: 'r-aide' }, t('moteur_redemarre_console')));
        r = await ctx.demander('journal', { depuis: 0 }, { silencieux: true });
      }
      ajouter(r.entrees || []);
    } catch { /* server busy or stopped */ }
  };
  const minuterie = setInterval(charger, 1500);

  const lancer = async () => {
    const code = saisie.value.trim();
    if (!code) return;
    etat.historique = [code, ...etat.historique.filter((h) => h !== code)].slice(0, 200);
    ecrire('historique_r', JSON.stringify(etat.historique));
    etat.position = -1;
    saisie.value = '';
    ajuster();
    etat.occupe = true;
    try {
      await charger(true);  // the actions before this command, in order
      const r = await ctx.occupe(() => ctx.demander('console', { code, modele: ctx.modeleActif(), projet: ctx.projetActif() }));
      ajouter(r.journal || []);
      if ([].concat(r.modifies || []).length) await ctx.apresConsole([].concat(r.modifies));
    } catch { /* the error is in the status bar */ } finally {
      etat.occupe = false;
      saisie.focus();
    }
  };
  const ajuster = () => {
    saisie.rows = Math.min(8, saisie.value.split('\n').length);
  };
  saisie.addEventListener('input', ajuster);

  // Tab: the names that can end the word before the cursor (completer_console, moteur/console.R).
  // One is written at once; several are listed above the prompt, after writing what they all
  // begin with: arrows to move, Tab or Enter to choose, Esc to close.
  const liste = el('div', { class: 'completion-r', role: 'listbox', hidden: true });
  let propositions = null;  // { debut, candidats, rang }
  const fermerListe = () => { propositions = null; liste.hidden = true; liste.replaceChildren(); };
  const remplacer = (debut, texte) => {
    saisie.setRangeText(texte, debut, saisie.selectionStart, 'end');
    ajuster();
  };
  const montrer = () => {
    liste.replaceChildren(...propositions.candidats.map((c, i) => el('div', {
      class: 'completion-item' + (i === propositions.rang ? ' actuel' : ''), role: 'option',
      onmousedown: (e) => { e.preventDefault(); choisir(i); } }, c)));
    liste.hidden = false;
    const actuel = liste.querySelector('.actuel');
    if (actuel) actuel.scrollIntoView({ block: 'nearest' });
  };
  const choisir = (i) => {
    const { debut, candidats } = propositions;
    fermerListe();
    remplacer(debut, candidats[i]);
  };
  const completer = async () => {
    const curseur = saisie.selectionStart;
    let r;
    try {
      r = await ctx.demander('completer', { texte: saisie.value, curseur, modele: ctx.modeleActif(), projet: ctx.projetActif() }, { silencieux: true });
    } catch { return; }
    const candidats = [].concat(r.candidats || []);
    if (!candidats.length || saisie.selectionStart !== curseur) return;  // nothing, or typed meanwhile
    if (candidats.length === 1) { remplacer(r.debut, candidats[0]); return; }
    const commun = candidats.reduce((a, c) => { let i = 0; while (i < a.length && a[i] === c[i]) i += 1; return a.slice(0, i); });
    if (commun.length > r.mot.length) remplacer(r.debut, commun);
    propositions = { debut: r.debut, candidats, rang: 0 };
    montrer();
  };

  // The prompt keeps the cursor while one types in it: if something takes it away right after a
  // key, with no click since and no dialog or other field taking it, it comes back.
  let derniereTouche = 0;
  saisie.addEventListener('blur', (e) => {
    fermerListe();
    if (dernierClic > derniereTouche || Date.now() - derniereTouche > 1000) return;
    const vers = e.relatedTarget;
    if (vers && vers.matches && vers.matches('input, textarea, select, [contenteditable]')) return;
    setTimeout(() => {
      if (conteneur.isConnected && document.hasFocus() && document.activeElement !== saisie && !document.querySelector('.fond-dialogue')) saisie.focus();
    });
  });

  saisie.addEventListener('keydown', (e) => {
    derniereTouche = Date.now();
    // the application's shortcuts (Ctrl+S, Ctrl+K...) still work while typing R, except those
    // that edit text
    if (!(e.ctrlKey && /^[a-z]$/i.test(e.key) && !'acvxyzl'.includes(e.key.toLowerCase()))) e.stopPropagation();
    if (propositions) {  // the list of names is open
      if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
        e.preventDefault();
        const n = propositions.candidats.length;
        propositions.rang = (propositions.rang + (e.key === 'ArrowDown' ? 1 : n - 1)) % n;
        montrer();
        return;
      }
      if (e.key === 'Tab' || e.key === 'Enter') { e.preventDefault(); choisir(propositions.rang); return; }
      if (e.key === 'Escape') { e.preventDefault(); fermerListe(); return; }
      fermerListe();
    }
    if (e.key === 'Tab' && !e.shiftKey && !e.ctrlKey && !e.altKey) { e.preventDefault(); completer(); return; }
    if (e.key === 'Enter' && !e.shiftKey) { e.preventDefault(); lancer(); return; }
    const premiere = !saisie.value.slice(0, saisie.selectionStart).includes('\n');
    const derniere = !saisie.value.slice(saisie.selectionEnd).includes('\n');
    if (e.key === 'ArrowUp' && premiere && etat.position < etat.historique.length - 1) {
      e.preventDefault();
      etat.position += 1;
      saisie.value = etat.historique[etat.position];
      ajuster();
    } else if (e.key === 'ArrowDown' && derniere && etat.position >= 0) {
      e.preventDefault();
      etat.position -= 1;
      saisie.value = etat.position >= 0 ? etat.historique[etat.position] : '';
      ajuster();
    } else if (e.key === 'l' && e.ctrlKey) {
      e.preventDefault();
      journal.replaceChildren();
    }
  });

  const entete = () => {
    const m = ctx.modele(ctx.modeleActif());
    const p = ctx.projet(ctx.projetActif());
    actif.textContent = m ? `m = ${m.id} (${m.nom} · ${m.type}) · p = ${m.projet}` : p ? `p = ${p.id} (${p.nom})` : t('console_sans_modele');
    libelleAppels.textContent = t('console_appels');
    effacer.title = t('effacer');
    copier.title = t('copier_journal');
    saisie.placeholder = t('console_invite');
  };
  conteneur._console = { entete };
  conteneur.classList.add('console-r');
  conteneur.replaceChildren(
    el('div', { class: 'outils' }, el('label', { class: 'champ' }, appels, libelleAppels), effacer, copier, el('span', { class: 'espace' }), actif),
    journal,
    el('div', { class: 'invite-r' }, el('span', { class: 'chevron' }, '>'), saisie, liste));
  journal.append(el('pre', { class: 'r-sortie r-aide' }, t('console_aide')));
  entete();
  setTimeout(() => charger(true));  // once the panel is in the page
}
