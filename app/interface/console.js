// R console (Affichage › Console R, Ctrl+J): the log of the engine calls behind each action
// of the interface, and a prompt that runs R in the application's own R session
// (moteur/console.R). m and p are the model and the project shown; P1, M7... the open
// projects and models, by the names the log gives them.
import { t, lire, ecrire } from './i18n.js';
import { el, icone } from './ui.js';

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
      const bloc = el('pre', { class: 'r-' + e.type, title: e.heure }, texte);
      if (e.type === 'appel' || e.type === 'commande') bloc.dataset.code = e.texte;
      journal.append(bloc);
    }
    while (journal.childElementCount > 1500) journal.firstElementChild.remove();
    if (enBas || entrees.some((e) => e.type === 'commande')) journal.scrollTop = journal.scrollHeight;
  };

  // the actions of the interface (and of other windows) come in by polling
  const charger = async (force = false) => {
    if (!conteneur.isConnected) { clearInterval(minuterie); return; }
    if (!force && (document.hidden || etat.occupe)) return;
    try {
      let r = await ctx.demander('journal', { depuis: etat.n });
      if (r.dernier < etat.n) {  // a new R engine: its log starts again from 1
        etat.n = 0;
        journal.append(el('pre', { class: 'r-aide' }, t('moteur_redemarre_console')));
        r = await ctx.demander('journal', { depuis: 0 });
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
  saisie.addEventListener('keydown', (e) => {
    e.stopPropagation();
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
    el('div', { class: 'invite-r' }, el('span', { class: 'chevron' }, '>'), saisie));
  journal.append(el('pre', { class: 'r-sortie r-aide' }, t('console_aide')));
  entete();
  charger(true);
}
