// Where the computation under way is (1.2). While a request the user waits for has been pending a
// second and a half (when Stop shows: app.js), the engine is asked twice a second, one question at a
// time, where its computation in the background is (request progression, moteur/arriere_plan.R): its
// request, how long it has run, what is done when its loops count their rounds (a cross-validation's
// groups, permutations, simulations...), and the requests waiting behind it. The answer draws:
//   - in the status bar, a bar with the step ("Permutation 420 of 999") and the time left, or, for a
//     computation that doesn't count its rounds (a single fit in compiled code), a moving bar with the
//     time since it began (elementStatut, which app.js puts after the engine's dot);
//   - in each view waiting for the engine, the skeleton of what it will show (style.css: .infos.attente)
//     with the same bar when it waits for that computation, "Waiting for the computation before it"
//     when its request is queued behind another one.
// The browser version's single R would hold the question behind the computation: there R says
// itself, on this tab's channel, the request it begins and its loops' rounds (moteur/R/navigateur.R:
// progres_tache, progres_navigateur), which make the same answer; the requests waiting are the
// others the user waits for.
import { demander, demandesEnAttente, MOTEUR_WEBR } from './api.js';
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el } from './ui.js';
import { ONGLET } from './secours_navigateur.js';

const ATTENTE = 1500;  // (as Stop's: app.js, ATTENTE_ARRET)
const INTERVALLE = 500;
let reponse = null;  // the engine's last answer, and when it came (recu)
let enVol = false;
let minuterie = null;
// the time left shown for the computation under way, smoothed (a new estimate moves it by a third)
let reste = null;

const statut = el('span', { class: 'progres-statut' });
export const elementStatut = () => statut;

// The browser version: what R last said (its request's number, name and seconds, its loops' rounds),
// and when; a request ended (moteur_webr.js: finTache) says nothing more, even late
export const CANAL_PROGRES = `nuee-progres-${ONGLET}`;
let ditParR = null;
let finies = new Set();
export function finTache(n) {
  finies.add(n);
  if (finies.size > 2000) finies = new Set([...finies].slice(-1000));
  if (ditParR && ditParR.tache === n) ditParR = null;
}
if (MOTEUR_WEBR && typeof BroadcastChannel === 'function') {
  new BroadcastChannel(CANAL_PROGRES).onmessage = ({ data: m }) => {
    if (!m || finies.has(m.tache)) return;
    ditParR = { ...m, recu: Date.now() };
  };
}
// the same answer as the desktop engine's: the computation R said, the others waiting behind it
function reponseNavigateur() {
  if (!ditParR) return null;
  const autres = demandesEnAttente().map((d) => d.nom);
  const i = autres.indexOf(ditParR.nom);
  if (i >= 0) autres.splice(i, 1);
  return { calcul: { nom: ditParR.nom, secondes: ditParR.secondes, fraction: ditParR.fraction ?? null, etapes: ditParR.etapes || [] }, attente: autres, recu: ditParR.recu };
}

const nombre = (x) => Number(x).toLocaleString(langueActuelle() === 'fr' ? 'fr-FR' : 'en-GB');
const duree = (s) => `${Math.floor(s / 60)}:${String(Math.floor(s % 60)).padStart(2, '0')}`;

// The step of the computation: its outermost loop's round (those inside it come and go too fast to
// read; the bar counts them).
function etape(etapes) {
  const x = [].concat(etapes || []).find((e) => e && e.quoi);
  return x ? t(`progres_${x.quoi}`, { fait: nombre(x.fait), total: nombre(x.total) }) : '';
}

// The time left, smoothed, said roughly: "a few seconds", "about 25 s", "about 3 min".
function tempsRestant(calcul) {
  const f = calcul.fraction;
  if (f === null || f === undefined || f < 0.02 || calcul.secondes < 2) { reste = null; return ''; }
  const estime = calcul.secondes * (1 - f) / f;
  const avant = reste && reste.nom === calcul.nom ? reste : null;
  const valeur = avant ? (avant.valeur - (calcul.secondes - avant.secondes)) * 2 / 3 + estime / 3 : estime;
  reste = { nom: calcul.nom, secondes: calcul.secondes, valeur: Math.max(0, valeur) };
  const r = reste.valeur;
  if (r < 10) return t('progres_quelques_secondes');
  if (r < 60) return t('progres_secondes', { n: Math.max(10, Math.round(r / 5) * 5) });
  if (r < 3600) return t('progres_minutes', { n: Math.max(1, Math.round(r / 60)) });
  return t('progres_heures');
}

// The bar in `parent`, made once and then changed in place (its width glides, a moving bar keeps
// its pace): `fraction` from 0 to 1, or null (it moves); its text beside it.
function poserBarre(parent, fraction, texte) {
  let b = parent.firstElementChild && parent.firstElementChild.classList.contains('progres') ? parent.firstElementChild : null;
  if (!b) {
    b = el('span', { class: 'progres', role: 'progressbar', 'aria-valuemin': '0', 'aria-valuemax': '100' },
      el('span', { class: 'progres-barre' }, el('span', { class: 'progres-fait' })), el('span', { class: 'progres-texte' }));
    parent.replaceChildren(b);
  }
  const determinee = fraction !== null && fraction !== undefined;
  b.classList.toggle('indetermine', !determinee);
  if (determinee) b.setAttribute('aria-valuenow', String(Math.round(fraction * 100)));
  else b.removeAttribute('aria-valuenow');
  b.setAttribute('aria-label', texte);
  b.querySelector('.progres-fait').style.width = determinee ? `${Math.max(2, Math.round(fraction * 100))}%` : '';
  const x = b.querySelector('.progres-texte');
  if (x.textContent !== texte) x.textContent = texte;
}

// What a computation does, by its request, when its loops don't say it (a single fit)
const PAR_DEMANDE = { ajuster: 'progres_ajustement', reajuster: 'progres_ajustement', modeles_classe: 'progres_ajustement', par_reponse: 'progres_ajustement',
  predire: 'progres_prediction', predire_fichier: 'progres_prediction', mettre_a_jour: 'progres_mise_a_jour' };

// What a computation says: its step and the time left; or, without rounds counted, since when.
function texteCalcul(calcul, depuis) {
  if (calcul) {
    const morceaux = [etape(calcul.etapes) || t(PAR_DEMANDE[calcul.nom] || 'calcul'), tempsRestant(calcul)].filter(Boolean);
    if (calcul.fraction === null || calcul.fraction === undefined) morceaux.push(duree(calcul.secondes));
    return morceaux.join(' · ');
  }
  return depuis > 60 ? t('calcul_long') : `${t('calcul')} ${duree(depuis)}`;
}

// The status bar and the waiting views drawn again from the last answer.
function dessiner() {
  const attentes = demandesEnAttente();
  const maintenant = Date.now();
  const plusVieille = attentes.length ? Math.min(...attentes.map((x) => x.debut)) : maintenant;
  const calcul = reponse && reponse.calcul ? { ...reponse.calcul, secondes: reponse.calcul.secondes + (maintenant - reponse.recu) / 1000 } : null;
  const autres = reponse ? [].concat(reponse.attente || []).length : 0;
  // (the computation's text once: the time left is smoothed at each new estimate)
  const leCalcul = calcul ? texteCalcul(calcul, 0) : null;
  const texte = (leCalcul || texteCalcul(null, (maintenant - plusVieille) / 1000)) + (autres ? ` · ${t('progres_en_attente', { n: autres })}` : '');
  poserBarre(statut, calcul ? calcul.fraction : null, texte);
  // each view's waiting text, by the request it waits for (api.js: data-demande)
  const parNumero = new Map(attentes.map((x) => [String(x.n), x]));
  let pris = false;
  for (const p of document.querySelectorAll('.infos.attente[data-demande]')) {
    const d = parNumero.get(p.dataset.demande);
    if (!d || maintenant - d.debut < ATTENTE) continue;
    if (calcul && calcul.nom === d.nom && !pris) {
      pris = true;  // (the oldest view of that request: the others asked the same thing after it)
      poserBarre(p, calcul.fraction, leCalcul);
    } else if (reponse && [].concat(reponse.attente || []).includes(d.nom)) {
      poserBarre(p, null, t('progres_file'));
    } else {
      poserBarre(p, null, texteCalcul(null, (maintenant - d.debut) / 1000));
    }
  }
  window.dispatchEvent(new CustomEvent('progression'));
}

async function demanderProgression() {
  minuterie = null;
  if (!demandesEnAttente().length) return;
  if (MOTEUR_WEBR) reponse = reponseNavigateur();
  else if (!enVol) {
    enVol = true;
    demander('progression', {}, { silencieux: true })
      .then((r) => { reponse = { ...r, recu: Date.now() }; }, () => { reponse = null; })
      .finally(() => { enVol = false; dessiner(); });
  }
  dessiner();
  minuterie = setTimeout(demanderProgression, INTERVALLE);
}

// (the requests waited for have changed: api.js)
window.addEventListener('requetes', () => {
  const attentes = demandesEnAttente();
  if (!attentes.length) {
    clearTimeout(minuterie);
    minuterie = null;
    reponse = null;
    reste = null;
    return;
  }
  if (minuterie) return;
  const debut = Math.min(...attentes.map((x) => x.debut));
  minuterie = setTimeout(demanderProgression, Math.max(0, debut + ATTENTE - Date.now()));
});

ajouterTextes({
  progres_validation: 'Validation croisée : groupe {fait} sur {total}',
  progres_preparation: 'Préparation de la validation croisée : groupe {fait} sur {total}',
  progres_composantes: 'Composante {fait}',
  progres_permutations: 'Permutation {fait} sur {total}',
  progres_simulations: 'Simulation {fait} sur {total}',
  progres_departs: 'Départ {fait} sur {total} du NMDS',
  progres_arbres: 'Arbre {fait} sur {total}',
  progres_tirages: 'Tirage {fait} sur {total}',
  progres_intervalles: 'Intervalle {fait} sur {total}',
  progres_k: 'Valeur de k {fait} sur {total}',
  progres_reponses: 'Réponse {fait} sur {total}',
  progres_etapes: 'Étape des données {fait} sur {total}',
  progres_ajustement: 'Ajustement du modèle…',
  progres_prediction: 'Prédiction…',
  progres_mise_a_jour: 'Mise à jour des données…',
  progres_quelques_secondes: 'encore quelques secondes',
  progres_secondes: 'reste environ {n} s',
  progres_minutes: 'reste environ {n} min',
  progres_heures: 'reste plus d’une heure',
  progres_en_attente: '{n} en attente',
  progres_file: 'En attente du calcul en cours…',
}, {
  progres_validation: 'Cross-validation: group {fait} of {total}',
  progres_preparation: 'Preparing the cross-validation: group {fait} of {total}',
  progres_composantes: 'Component {fait}',
  progres_permutations: 'Permutation {fait} of {total}',
  progres_simulations: 'Simulation {fait} of {total}',
  progres_departs: 'NMDS start {fait} of {total}',
  progres_arbres: 'Tree {fait} of {total}',
  progres_tirages: 'Draw {fait} of {total}',
  progres_intervalles: 'Interval {fait} of {total}',
  progres_k: 'Value of k {fait} of {total}',
  progres_reponses: 'Response {fait} of {total}',
  progres_etapes: 'Data step {fait} of {total}',
  progres_ajustement: 'Fitting the model…',
  progres_prediction: 'Predicting…',
  progres_mise_a_jour: 'Updating the data…',
  progres_quelques_secondes: 'a few seconds left',
  progres_secondes: 'about {n} s left',
  progres_minutes: 'about {n} min left',
  progres_heures: 'over an hour left',
  progres_en_attente: '{n} waiting',
  progres_file: 'Waiting for the computation under way…',
});
