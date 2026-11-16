// Model › Which analysis? (also a button of the start page, and the command palette): a few
// questions in plain words on the left, their answers proposed from what Nuée knows of the
// project's variables (the engine's variable statistics, and the coordinates its names propose);
// on the right, the analysis recommended, why, its main conditions and its alternatives, each with
// a button that opens New model already filled in (type, distribution, roles, random effects,
// cross-validation groups), the right view of the project, or New variable with its formula. The
// decision logic is choix_analyse.js's, a table of rules apart; this module only draws and opens.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, dialogue, decimal } from './ui.js';
import { demander } from './api.js';
import { dialogueModele, dialogueVariable, ANALYSES } from './dialogues.js';
import { QUESTIONS, BUTS, formulaire, TEXTES_CHOIX, faitsVariables, genre, beaucoupDeZeros, zerosPoisson, proposer, choisirReponse,
  contexte, recommander, construire, notesPour, uidDe, nomColonne, distanceProposee } from './choix_analyse.js';

ajouterTextes(TEXTES_CHOIX.fr, TEXTES_CHOIX.en);
ajouterTextes({ qa_site_en_nombres: '{nom} est fait de nombres : en catégories, il peut être le site.' },
  { qa_site_en_nombres: '{nom} is made of numbers: as categories, it can be the site.' });

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));
const entreGuillemets = (n) => (langueActuelle() === 'fr' ? `« ${n} »` : `“${n}”`);
// (the name of a column of sites, plots, stations… that a code in numbers may name, accents removed)
const SITE_EN_NOMBRES = /^(sites?|plots?|placettes?|parcelles?|stations?|transects?|quadrats?|routes?|blocs?|blocks?|lieux?|unites?|units?|pieges?|traps?|mares?|ponds?|etangs?|nichoirs?|colonies?)([ _.-]*(id|code|no|num|numero|number|nr))?$/i;
const deuxPoints = () => (langueActuelle() === 'fr' ? ' : ' : ': ');  // (a space before the colon in French only)
// the answers last given in each project, offered again when the dialog opens again (a model
// dialog cancelled, to try an alternative)
const memoire = new Map();

// The answers kept, against the project as it is now: a variable gone is no longer chosen.
function retrouver(R, p) {
  const uids = new Set(p.variables.map((v) => v.uid));
  const colonne = (c) => (!c ? null : c.startsWith('v:') ? (uids.has(Number(c.slice(2))) ? c : null)
    : liste(p.identifiants).some((i) => `id:${i.rang}` === c) ? c : null);
  return { ...R, y: uids.has(R.y) ? R.y : null, groupe: uids.has(R.groupe) ? R.groupe : null, especes: liste(R.especes).filter((u) => uids.has(u)),
    x: liste(R.x).filter((u) => uids.has(u)), variables: liste(R.variables).filter((u) => uids.has(u)),
    site: colonne(R.site), temps: colonne(R.temps), unite: colonne(R.unite) };
}

// A number as the language writes it (3 significant digits at most), a share in %.
const nombre = (v) => {
  const s = decimal(Number.isInteger(v) ? v : +Number(v).toPrecision(3));
  return langueActuelle() === 'fr' ? s.replace('-', '−') : s;
};
const pourcent = (v) => (v > 0 && v < 1 ? '< 1' : String(Math.round(v)));

// What the data say of a variable, in a few words (under the response).
function faitsTexte(f) {
  if (!f) return '';
  const morceaux = [];
  if (f.qualitative) morceaux.push(t(f.ncat === 1 ? 'qa_faits_categorie_une' : 'qa_faits_categories', { n: f.ncat }));
  else if (f.min === null) return t('qa_faits_inconnus');
  else {
    morceaux.push(t('qa_faits_bornes', { min: nombre(f.min), max: nombre(f.max) }), t(f.entiers ? 'qa_faits_entiers' : 'qa_faits_decimales'));
    // (whole numbers that their unit or name says are a measurement, not counts: choix_analyse.js, genre)
    if (f.entiers && f.min >= 0 && f.max > 1 && genre(f) === 'mesure') morceaux.push(t('qa_faits_mesure_arrondie'));
    if (f.zeros > 0) {
      const poisson = genre(f) === 'comptage' ? zerosPoisson(f) : null;
      morceaux.push(t('qa_faits_zeros', { p: pourcent(f.zeros) }) + (poisson !== null ? ` (${t('qa_faits_poisson', { q: pourcent(poisson) })})` : ''));
    }
    if (f.asymetrie > 2) morceaux.push(t('qa_faits_asymetrie'));
  }
  if (f.manquantes > 0) morceaux.push(t('qa_faits_manquantes', { p: pourcent(f.manquantes) }));
  return t('qa_faits_propose', { faits: morceaux.join(', ') });
}

export async function dialogueQuelleAnalyse(app, projet) {
  const pid = projet.id;
  const p = () => app.projet(pid) || projet;
  // (by id and name: a restarted engine numbers its projects from P1 again)
  const cleMemoire = `${pid}|${projet.nom}`;
  const gauche = el('div', { class: 'qa-gauche' }, el('p', { class: 'infos' }, t('calcul')));
  const droite = el('div', { class: 'qa-droite' });
  let R = null;
  const d = dialogue({ titre: `${t('quelle_analyse').replace('…', '')} — ${p().nom}`, large: true,
    contenu: el('div', {}, el('div', { class: 'quelle-analyse' }, gauche, droite)),
    boutons: [{ libelle: t('fermer'), principal: true, action: () => true }],
    surFermeture: () => { if (R) memoire.set(cleMemoire, R); } });

  // the variables' statistics (whole numbers, zeros, bounds...) and the coordinates (the project's,
  // or those its variables' names propose); without them, the answers are only less well proposed
  const [stats, coordonnees] = await Promise.all([app.statistiquesVariables(pid).catch(() => null),
    demander('coordonnees', { projet: pid }, { silencieux: true }).catch(() => null)]);
  const F = faitsVariables(p(), stats);
  R = memoire.has(cleMemoire) ? retrouver(memoire.get(cleMemoire), p()) : proposer(p(), F, coordonnees);
  const coords = (() => {
    const co = (coordonnees && coordonnees.coordonnees) || null;
    const pr = (coordonnees && coordonnees.proposition) || null;
    return { projet: co, proposees: !co && pr ? pr : null };
  })();

  const vars = () => p().variables;
  const nom = (u) => (F.get(u) || {}).nom || '';
  const quantitatives = () => vars().filter((v) => !v.qualitative);
  const qualitatives = () => vars().filter((v) => v.qualitative);
  const ids = () => liste(p().identifiants).map((i) => [`id:${i.rang}`, `${i.nom} (${t('identifiant')})`]);

  // ---- the form
  const radio = (groupe, valeur, actuel, libelle, aide, changer, titre = '') => el('label', { class: valeur === actuel ? 'actif' : '', title: titre || null },
    el('input', { type: 'radio', name: groupe, checked: valeur === actuel, 'data-cle': `${groupe}:${valeur}`, onchange: () => changer(valeur) }),
    el('span', {}, libelle, aide ? el('span', { class: 'doux' }, ` — ${aide}`) : null));
  const select = (cle, options, valeur, changer, vide = t('qa_choisir')) => el('select', { 'data-cle': cle, onchange: (e) => changer(e.target.value) },
    [el('option', { value: '', selected: valeur === null || valeur === undefined }, vide)].concat(options.map(([k, n]) => el('option', { value: k, selected: String(k) === String(valeur) }, n))));
  // ticked variables: a box per variable, all or none, a filter when there are many
  const cases = (cle, candidates, coches, changer) => {
    const ensemble = new Set(coches);
    const compte = el('span', { class: 'infos' });
    const boite = el('div', { class: 'qa-cases', 'data-defile': cle });
    const filtre = candidates.length > 14 ? el('input', { type: 'search', placeholder: t('recherche'), class: 'qa-filtre', 'data-cle': `${cle}:filtre` }) : null;
    const dessiner = () => {
      const f = filtre ? filtre.value.trim().toLowerCase() : '';
      boite.replaceChildren(...candidates.filter((v) => !f || v.nom.toLowerCase().includes(f)).map((v) => el('label', {},
        el('input', { type: 'checkbox', checked: ensemble.has(v.uid), onchange: (e) => {
          if (e.target.checked) ensemble.add(v.uid); else ensemble.delete(v.uid);
          changer(candidates.filter((w) => ensemble.has(w.uid)).map((w) => w.uid));
          compte.textContent = t('qa_n_cochees', { n: ensemble.size, total: candidates.length });
        } }), v.nom, v.qualitative ? el('span', { class: 'doux' }, ` (${liste(v.categories).length})`) : null)));
      compte.textContent = t('qa_n_cochees', { n: ensemble.size, total: candidates.length });
    };
    const tous = (oui) => {
      const f = filtre ? filtre.value.trim().toLowerCase() : '';
      for (const v of candidates) if (!f || v.nom.toLowerCase().includes(f)) { if (oui) ensemble.add(v.uid); else ensemble.delete(v.uid); }
      changer(candidates.filter((w) => ensemble.has(w.uid)).map((w) => w.uid));
      dessiner();
    };
    if (filtre) filtre.addEventListener('input', dessiner);
    dessiner();
    return el('div', { class: 'qa-liste' }, el('div', { class: 'ligne' }, filtre, el('button', { onclick: () => tous(true) }, t('tout_cocher')),
      el('button', { onclick: () => tous(false) }, t('aucune_cocher')), compte), boite);
  };

  const fiche = () => formulaire(R.question, R.but);
  // a kind of response the question doesn't offer gives way to the response's own, or to none
  const ajusterReponse = () => {
    const f = fiche();
    if (!f || f.reponses.includes(R.reponse)) return;
    const propre = R.y !== null ? genre(F.get(R.y)) : null;
    R.reponse = f.reponses.length === 1 ? f.reponses[0] : f.reponses.includes(propre) ? propre
      : f.plusieurs ? (R.especes.length ? 'especes' : 'mesure') : null;
  };
  const sansEspeces = () => { R.x = R.x.filter((u) => !R.especes.includes(u)); };

  function rendreGauche() {
    // (the scroll of the column and of its lists, and the field with the keyboard, stay where they were)
    const defile = gauche.scrollTop;
    const listes = Object.fromEntries([...gauche.querySelectorAll('[data-defile]')].map((x) => [x.dataset.defile, x.scrollTop]));
    const actif = document.activeElement && gauche.contains(document.activeElement) ? document.activeElement.dataset.cle : null;
    const f = fiche();
    // (once a question is chosen, only its example stays in view: the others as a tooltip)
    const exemple = (q) => (!R.question || R.question === q ? t(`qa_ex_${q}`) : '');
    let n = 0;  // (the sections numbered as they come: a question may have no design to ask)
    const titre = (cle) => el('h3', {}, `${++n}. ${t(cle)}`);
    const sections = [titre('qa_titre_question'),
      el('div', { class: 'qa-choix' }, QUESTIONS.map((q) => radio('qa-question', q, R.question, t(`qa_q_${q}`), exemple(q), (v) => {
        R.question = v;
        // (distance sampling: the column named like the distances, as its New model fields propose it)
        if (v === 'densite' && R.y === null) choisirReponse(R, F, distanceProposee(p()));
        ajusterReponse();
        if (R.reponse === 'especes') sansEspeces();
        maj(true);
      }, exemple(q) ? '' : t(`qa_ex_${q}`))))];
    if (R.question === 'especes') {
      sections.push(el('div', { class: 'qa-choix qa-but' }, el('span', { class: 'doux' }, `${t('qa_but')}${deuxPoints().trimEnd()}`),
        BUTS.map((b) => radio('qa-but', b, R.but, t(`qa_but_${b}`), '', (v) => { R.but = v; maj(true); }))));
    }
    if (f) {
      // the response
      const reponse = [titre('qa_titre_reponse')];
      // (a question with a single kind of response: no kinds to choose; the species, or the response named after its kind)
      const uneSeule = f.reponses.length === 1;
      const seulesEspeces = uneSeule && f.reponses[0] === 'especes';
      if (!seulesEspeces && !(f.plusieurs || R.reponse === 'especes')) {
        // (the variables of the kinds the question offers; the kind then follows the variable chosen)
        const numeriques = f.reponses.some((k) => ['mesure', 'comptage', 'presence', 'proportion', 'distances'].includes(k));
        const options = [...(numeriques ? quantitatives() : []), ...(f.reponses.includes('classe') ? qualitatives() : [])]
          .map((v) => [v.uid, v.qualitative ? `${v.nom} (${liste(v.categories).length})` : v.nom]);
        reponse.push(el('div', { class: 'ligne' }, el('label', {}, uneSeule ? t(`qa_r_${f.reponses[0]}`) : t('qa_reponse_variable')),
          select('qa-y', options, R.y, (v) => { choisirReponse(R, F, v === '' ? null : Number(v), p()); ajusterReponse(); maj(true); },
            f.facultative ? t('qa_reponse_facultative') : t('qa_choisir'))));
      }
      if (!uneSeule) {
        reponse.push(el('div', { class: 'qa-choix qa-genres' }, f.reponses.map((k) => {
          const r = radio('qa-reponse', k, R.reponse, t(`qa_r_${k}`), t(`qa_rex_${k}`), (v) => {
            R.reponse = v;
            const fy = R.y !== null ? F.get(R.y) : null;
            if (fy && (v === 'classe') !== fy.qualitative) R.y = null;  // (a class is qualitative; the others are numbers)
            if (v === 'comptage' && fy && !fy.qualitative) R.zeros = beaucoupDeZeros(fy);
            if (v === 'especes') sansEspeces();
            maj(true);
          });
          // (many zeros, under a count: a box of its own, beside the count's label rather than in it)
          return k === 'comptage' && R.reponse === 'comptage' ? el('div', { class: 'qa-genre' }, r,
            el('label', { class: 'champ qa-zeros', title: t('qa_zeros_aide') },
              el('input', { type: 'checkbox', checked: !!R.zeros, 'data-cle': 'qa-zeros', onchange: (e) => { R.zeros = e.target.checked; maj(false); } }), t('qa_zeros'))) : r;
        })));
      }
      if (R.y !== null && R.reponse !== 'especes' && !f.plusieurs) reponse.push(el('p', { class: 'note' }, faitsTexte(F.get(R.y))));
      if (R.reponse === 'especes' || seulesEspeces) {
        reponse.push(el('div', { class: 'ligne' }, el('label', {}, t('qa_especes'))), el('p', { class: 'note' }, t('qa_especes_proposees')),
          cases('qa-especes', quantitatives(), R.especes, (us) => { R.especes = us; sansEspeces(); maj(false); }));
      } else if (f.plusieurs) {
        reponse.push(el('div', { class: 'ligne' }, el('label', {}, t('qa_variables_explorer'))),
          cases('qa-variables', vars(), R.variables, (us) => { R.variables = us; maj(false); }));
      }
      sections.push(...reponse);

      // the design: a box (or a required field, '!') and its column
      const plan = [];
      const champ = (cle, libelle, contenu) => {
        const requis = f.champs.includes(`${cle}!`);
        if (!requis && !f.champs.includes(cle)) return;
        const coche = requis || !!R[cle];
        plan.push(el('div', { class: 'qa-plan' },
          requis ? el('span', {}, libelle) : el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: coche, 'data-cle': `qa-${cle}`,
            onchange: (e) => { R[cle] = e.target.checked; maj(true); } }), libelle),
          coche && contenu ? contenu() : null));
      };
      const colonnes = (temps) => (temps ? [...quantitatives(), ...qualitatives().filter((v) => v.temps)] : qualitatives()).map((v) => [`v:${v.uid}`, v.nom]).concat(ids());
      // (a site coded 1, 2, 3 is made of numbers, and a site is qualitative: the first such column
      // can be treated as categories from here, one change of the data, and the dialog comes back with it)
      const siteEnNombres = !R.site ? quantitatives().find((v) => SITE_EN_NOMBRES.test(v.nom.normalize('NFD').replace(/[̀-ͯ]/g, '').trim())
        && (F.get(v.uid) || {}).entiers) : null;
      const enCategories = async (v) => {
        try { await app.changerDonnees('type_variable', pid, { variable: v.uid, type: 'qualitative' }); } catch { return; }  // (occupe said why)
        R.site = `v:${v.uid}`;
        d.fermer();
        app.quelleAnalyse(pid);
      };
      champ('repetes', t('qa_d_repetes'), () => el('div', {}, el('div', { class: 'ligne', title: t('qa_d_repetes_aide') }, el('span', { class: 'doux' }, t('qa_site')),
        select('qa-site', colonnes(false), R.site, (v) => { R.site = v || null; R.x = R.x.filter((u) => u !== uidDe(R.site)); maj(false); })),
      siteEnNombres ? el('p', { class: 'note' }, t('qa_site_en_nombres', { nom: entreGuillemets(siteEnNombres.nom) }), ' ',
        el('button', { 'data-cle': 'qa-site-categories', onclick: () => enCategories(siteEnNombres) }, t('atelier_en_categories', { nom: entreGuillemets(siteEnNombres.nom) }))) : null));
      champ('annees', t('qa_d_annees'), () => el('div', { class: 'ligne' }, el('span', { class: 'doux' }, t('qa_temps')),
        select('qa-temps', colonnes(true), R.temps, (v) => { R.temps = v || null; R.x = R.x.filter((u) => u !== uidDe(R.temps)); maj(false); })));
      champ('baci', t('qa_d_baci'), () => el('div', { class: 'ligne' }, el('span', { class: 'doux' }, t('qa_unite')),
        select('qa-unite', colonnes(false), R.unite, (v) => { R.unite = v || null; maj(false); })));
      champ('coordonnees', t('qa_d_coordonnees'), () => el('div', { class: 'ligne' }, el('span', { class: 'doux' }, coords.projet
        ? t('qa_coordonnees_projet', { x: coords.projet.nom_x, y: coords.projet.nom_y })
        : coords.proposees ? t('qa_coordonnees_proposees', { x: nom(Number(coords.proposees.x)), y: nom(Number(coords.proposees.y)) }) : t('qa_coordonnees_aucune'))));
      // (distance sampling: line transects or point transects, which no column tells)
      champ('transects', t('qa_d_transects'), () => el('div', { class: 'ligne' }, el('select', { 'data-cle': 'qa-transects',
        onchange: (e) => { R.transect = e.target.value; maj(false); } },
      ['ligne', 'point'].map((k) => el('option', { value: k, selected: k === (R.transect || 'ligne') }, t(`qa_transects_${k}`))))));
      if (plan.length) sections.push(titre('qa_titre_plan'), ...plan);

      // the variables: the groups, the X
      const variables = [];
      if (f.champs.includes('groupe') || f.champs.includes('groupe!')) {
        variables.push(el('div', { class: 'ligne' }, el('label', {}, t('qa_groupe')),
          select('qa-groupe', qualitatives().filter((v) => v.uid !== R.y).map((v) => [v.uid, `${v.nom} (${liste(v.categories).length})`]), R.groupe,
            (v) => { R.groupe = v === '' ? null : Number(v); maj(false); })));
      }
      if (f.champs.includes('x')) {
        variables.push(el('div', { class: 'ligne' }, el('label', {}, t('qa_x'))),
          cases('qa-x', vars().filter((v) => v.uid !== R.y && !(R.reponse === 'especes' && R.especes.includes(v.uid))), R.x, (us) => { R.x = us; maj(false); }));
      }
      if (variables.length) sections.push(titre('qa_titre_variables'), ...variables);
    }
    gauche.replaceChildren(...sections);
    gauche.scrollTop = defile;
    for (const x of gauche.querySelectorAll('[data-defile]')) if (listes[x.dataset.defile]) x.scrollTop = listes[x.dataset.defile];
    if (actif) { const x = gauche.querySelector(`[data-cle="${CSS.escape(actif)}"]`); if (x) x.focus(); }
  }

  // ---- the recommendation
  // an analysis's name: with the random effect of its site, for a regression that has one
  // (a linear regression with a random effect is New model's linear mixed model)
  const nomAnalyse = (b) => {
    const mixte = b.reglages && b.reglages.type === 'LMM';
    const n = t(mixte ? (b.id === 'lm_log' ? 'qa_nom_lmm_log' : 'qa_nom_lmm') : `qa_nom_${b.id}`);
    return b.reglages && liste(b.reglages.aleatoires).length ? t('qa_avec_site', { nom: n, site: nomColonne(R.site, p()) }) : n;
  };
  const bouton = (b, principal) => el('button', { class: principal ? 'principal' : '', onclick: () => ouvrir(b) },
    icone(b.ouvre === 'vue' ? 'external-link' : b.ouvre === 'variable' ? 'calculator' : 'file-plus'),
    principal ? (b.ouvre === 'modele' ? t('qa_ouvrir_modele') : b.ouvre === 'par_reponse' ? t('qa_ouvrir_par_reponse')
      : b.ouvre === 'variable' ? t('qa_ouvrir_variable') : t('qa_ouvrir_vue', { vue: t(b.vue) })) : t('qa_ouvrir_court'));
  const details = (b, c) => [
    el('p', {}, el('b', {}, `${t('qa_pourquoi')}${deuxPoints()}`), t(`qa_pourquoi_${b.id}`)),
    el('p', {}, el('b', {}, `${t('qa_conditions')}${deuxPoints()}`), t(`qa_conditions_${b.id}`)),
    ...notesPour(b.id, c).map((k) => el('p', { class: 'note' }, t(k))),
    b.rempli.length ? el('p', { class: 'qa-rempli' }, el('b', {}, `${t('qa_rempli')}${deuxPoints()}`),
      b.rempli.map((x) => `${t(x.cle)}${deuxPoints()}${x.noms ? x.noms.join(', ') : t(x.texte)}`).join(' · ')) : null,
    b.manque.length ? el('p', { class: 'attention' }, t('qa_a_completer', { liste: b.manque.map((k) => t(`qa_manque_${k}`)).join(', ') })) : null];
  function rendreDroite() {
    const c = contexte(R, F, p());
    const r = R.question ? recommander(c) : null;
    if (!r) {
      droite.replaceChildren(el('p', { class: 'infos qa-attente' }, t(R.question ? 'qa_choisir_reponse' : 'qa_choisir_question')));
      return;
    }
    const b = construire(r.choix, R, F, p(), c, { langue: langueActuelle() });
    const autres = r.autres.map((id) => construire(id, R, F, p(), c, { langue: langueActuelle() }));
    // (without alternatives, their heading and list are left out: replaceChildren would write a null as "null")
    droite.replaceChildren(...[el('h3', {}, t('qa_recommandee')),
      el('div', { class: 'qa-carte' }, el('h4', {}, nomAnalyse(b)), ...details(b, c), el('div', { class: 'qa-ouvrir' }, bouton(b, true))),
      autres.length ? el('h3', {}, t('qa_autres')) : null,
      autres.length ? el('div', { class: 'qa-autres' }, autres.map((a) => el('details', {},
        el('summary', {}, el('span', { class: 'qa-nom' }, nomAnalyse(a)), el('span', { class: 'espace' }), bouton(a, false)),
        el('div', { class: 'qa-detail' }, ...details(a, c))))) : null,
      el('p', { class: 'note' }, t('qa_conseil'))].filter(Boolean));
  }
  function maj(structure) {
    if (structure) rendreGauche();
    rendreDroite();
  }

  // ---- what each button opens (the dialog closes first: its answers are kept for next time)
  function ouvrir(b) {
    d.fermer();
    const projetActuel = p();
    // (another analysis's own default scaling, as when its type is chosen in New model)
    const reglages = b.reglages && ANALYSES[b.reglages.type] && ANALYSES[b.reglages.type].echelle ? { ...b.reglages, echelle: ANALYSES[b.reglages.type].echelle } : b.reglages;
    if (b.ouvre === 'modele') dialogueModele(app, projetActuel, reglages);
    else if (b.ouvre === 'par_reponse') dialogueModele(app, projetActuel, reglages, t('nouveau_modele'), (id) => app.unModeleParReponse(id, b.especes));
    else if (b.ouvre === 'variable') dialogueVariable(app, projetActuel, 'formule', b.variable);
    else app.ouvrirVue(b.vue, null, b.loc, pid);
  }

  ajusterReponse();
  maj(true);
  // (the keyboard starts on the question: the dialog opened before the form was drawn)
  const premier = gauche.querySelector('input:checked') || gauche.querySelector('input');
  if (premier) premier.focus();
}
