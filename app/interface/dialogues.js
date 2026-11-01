// Dialogs: model definition (type, variables, observations), help and about.
import { t, langueActuelle } from './i18n.js';
import { el, icone, dialogue } from './ui.js';
import { libelleTransfo, FORMAT, changerFormat } from './panneaux.js';

const ECHELLES = ['UV', 'Par', 'Ctr', 'none'];
const TRANSFOS = ['', 'racine2', 'racine4', 'log10'];

// projet: project summary; r: settings of an existing model to start from (m.reglages).
export function dialogueModele(app, projet, r = null, titre = t('nouveau_modele')) {
  const p = projet;
  const st = {
    type: r ? r.type : 'PLS', nom: nomLibre(p), titre: r && r.nom ? `${r.titre ? r.titre + ' — ' : ''}${langueActuelle() === 'fr' ? 'd’après' : 'from'} ${r.nom}` : '',
    A: r ? r.A : 0, auto: !r, G: r ? r.G || 7 : 7, echelle: 'UV', roles: {}, echelles: {}, transfos: {},
    exclues: new Set(r ? [].concat(r.exclues || []) : []), onglet: 'variables', filtre: '', filtreObs: '',
    croises: r && r.croises ? r.croises.map((c) => [].concat(c).map(Number)) : [], a: '', b: '',
  };
  if (r) {
    [].concat(r.x || []).forEach((u) => { st.roles[u] = 'x'; });
    [].concat(r.y || []).forEach((u) => { st.roles[u] = 'y'; });
    if (r.classe !== null && r.classe !== undefined) st.roles[r.classe] = 'classe';
    Object.assign(st.echelles, r.echelles || {});
    Object.assign(st.transfos, r.transformations || {});
  }
  const contenu = el('div', { class: 'dialogue-modele' });
  const erreur = el('p', { class: 'erreur' });
  const resume = el('span', { class: 'infos' });

  const majResume = () => {
    const x = Object.values(st.roles).filter((v) => v === 'x').length;
    const y = Object.values(st.roles).filter((v) => v === 'y' || v === 'classe').length;
    resume.textContent = t('resume_dialogue', { x, y, n: p.observations.uid.length - st.exclues.size });
  };

  const onglets = () => el('div', { class: 'onglets' }, [['general', t('general')], ['variables', t('variables')],
    ['termes', `${t('termes')}${st.croises.length ? ` (${st.croises.length})` : ''}`], ['observations', t('observations')]]
    .map(([k, n]) => el('button', { class: st.onglet === k ? 'actif' : '', onclick: () => { st.onglet = k; rendre(); } }, n)));

  const general = () => el('div', { class: 'grille-champs' },
    el('label', {}, t('type')), el('select', { onchange: (e) => { st.type = e.target.value; rendre(); } },
      [['PCA', 'ACP / PCA'], ['PLS', 'PLS'], ['PLS-DA', 'PLS-DA'], ['OPLS', 'OPLS'], ['OPLS-DA', 'OPLS-DA']]
        .map(([k, n]) => el('option', { value: k, selected: k === st.type }, n))),
    ['OPLS', 'OPLS-DA'].includes(st.type) ? el('label', {}, '') : null,
    ['OPLS', 'OPLS-DA'].includes(st.type) ? el('p', { class: 'note' }, t('opls_note')) : null,
    el('label', {}, t('nom')), el('input', { value: st.nom, oninput: (e) => { st.nom = e.target.value; } }),
    el('label', {}, t('titre_modele')), el('input', { value: st.titre, oninput: (e) => { st.titre = e.target.value; } }),
    el('label', {}, t('nb_comp')), el('div', { class: 'ligne' },
      el('label', { class: 'champ' }, el('input', { type: 'checkbox', checked: st.auto, onchange: (e) => { st.auto = e.target.checked; rendre(); } }), t('auto')),
      el('input', { type: 'number', min: 1, max: 50, value: st.A || 2, disabled: st.auto, style: 'width:5em', oninput: (e) => { st.A = +e.target.value; } })),
    el('label', {}, t('groupes_vc')), el('input', { type: 'number', min: 2, max: 50, value: st.G, style: 'width:5em', oninput: (e) => { st.G = +e.target.value || 7; } }),
    el('label', {}, t('echelle_defaut')), el('select', { onchange: (e) => { st.echelle = e.target.value; rendre(); } },
      ECHELLES.map((k) => el('option', { value: k, selected: k === st.echelle }, t('echelle_' + k)))));

  const avecY = () => ['PLS', 'OPLS'].includes(st.type);
  const avecClasse = () => ['PLS-DA', 'OPLS-DA'].includes(st.type);
  const roles = (v) => [['', t('role_aucun')], ['x', t('role_x')]].concat(avecY() ? [['y', t('role_y')]] : [])
    .concat(avecClasse() && v.qualitative ? [['classe', t('role_classe')]] : []);

  const variables = () => {
    const f = st.filtre.toLowerCase();
    const vars = p.variables.filter((v) => !f || v.nom.toLowerCase().includes(f));
    const tous = (cle, valeur) => {
      for (const v of vars) {
        if (cle === 'role') {
          if (valeur === 'classe' && !v.qualitative) continue;
          if (valeur === 'y' && !avecY()) continue;
          st.roles[v.uid] = valeur;
        } else if (cle === 'echelle') st.echelles[v.uid] = valeur;
        else if (!v.qualitative) st.transfos[v.uid] = valeur;
      }
      rendre();
    };
    const sel = (opts, val, f2, actif = true) => el('select', { onchange: (e) => f2(e.target.value), disabled: !actif },
      opts.map(([k, n]) => el('option', { value: k, selected: String(k) === String(val ?? '') }, n)));
    const filtre = el('input', { type: 'search', placeholder: t('recherche'), value: st.filtre, class: 'filtre',
      oninput: (e) => { st.filtre = e.target.value; rendre(); const i = contenu.querySelector('.filtre'); i.focus(); i.setSelectionRange(i.value.length, i.value.length); } });
    return el('div', { class: 'onglet-vars' },
      el('div', { class: 'outils' }, filtre, el('span', { class: 'infos' }, t('pour_filtrees')),
        sel([['-', t('role') + '…']].concat(roles({ qualitative: true }).map(([k, n]) => [k || 'aucun', n])), '-', (v) => tous('role', v === 'aucun' ? '' : v)),
        sel([['-', t('echelle') + '…']].concat(ECHELLES.map((k) => [k, t('echelle_' + k)])), '-', (v) => tous('echelle', v)),
        sel([['-', t('transformation') + '…']].concat(TRANSFOS.map((k) => [k || 'aucune', t('tr_' + (k || 'aucune'))])), '-', (v) => tous('transfo', v === 'aucune' ? '' : v)),
        el('button', { title: t('transfos_suggerees_aide'), onclick: () => suggerer(vars) }, icone('wand-sparkles'), t('transfos_suggerees')),
        st.suggerees !== undefined ? el('span', { class: 'infos' }, t('transfos_appliquees', { n: st.suggerees })) : null),
      el('div', { class: 'defile' }, el('table', { class: 'tableau' },
        el('thead', {}, el('tr', {}, el('th', {}, t('variables')), el('th', {}, ''), el('th', {}, t('role')), el('th', {}, t('echelle')), el('th', {}, t('transformation')))),
        el('tbody', {}, vars.map((v) => el('tr', { class: st.roles[v.uid] ? 'role-' + st.roles[v.uid] : '' },
          el('td', {}, v.nom),
          el('td', { class: 'doux' }, v.qualitative ? `${v.categories.length} ${t('categories')}` : ''),
          el('td', {}, sel(roles(v), st.roles[v.uid] || '', (x) => { st.roles[v.uid] = x; rendre(); })),
          el('td', {}, sel(ECHELLES.map((k) => [k, t('echelle_' + k)]), st.echelles[v.uid] || st.echelle, (x) => { st.echelles[v.uid] = x; })),
          el('td', {}, sel(TRANSFOS.map((k) => [k, t('tr_' + (k || 'aucune'))])
            .concat(st.transfos[v.uid] === 'autre' ? [['autre', t('tr_autre')]] : [])
            .concat((st.transfos[v.uid] || '').startsWith('log10:') ? [[st.transfos[v.uid], libelleTransfo(st.transfos[v.uid])]] : []),
            st.transfos[v.uid] || '', (x) => { st.transfos[v.uid] = x; }, !v.qualitative))))))));
  };

  // The proposed transformation, worked out in R: log10(x + C2) for the variables
  // shown whose skewness and Min/Max call for it.
  async function suggerer(vars) {
    const s = await app.statistiquesVariables(p.id);
    let n = 0;
    for (const v of vars) {
      const i = s.uid.indexOf(v.uid);
      if (i >= 0 && s.suggestion[i] && !v.qualitative) { st.transfos[v.uid] = s.suggestion[i]; n += 1; }
    }
    st.suggerees = n;
    rendre();
  }

  const observations = () => {
    const f = st.filtreObs.toLowerCase();
    const o = p.observations;
    const idx = o.uid.map((_, i) => i).filter((i) => !f || String(o.nom[i]).toLowerCase().includes(f));
    const modele = app.modeleActif() && app.modele(app.modeleActif());
    const filtre = el('input', { type: 'search', placeholder: t('recherche'), value: st.filtreObs, class: 'filtre',
      oninput: (e) => { st.filtreObs = e.target.value; rendre(); const i = contenu.querySelector('.filtre'); i.focus(); i.setSelectionRange(i.value.length, i.value.length); } });
    return el('div', { class: 'onglet-obs' },
      el('div', { class: 'outils' }, filtre,
        el('button', { onclick: () => { idx.forEach((i) => st.exclues.delete(o.uid[i])); rendre(); } }, t('tout_inclure')),
        el('button', { onclick: () => { idx.forEach((i) => { const u = o.uid[i]; if (st.exclues.has(u)) st.exclues.delete(u); else st.exclues.add(u); }); rendre(); } }, t('inverser')),
        modele && modele.projet === p.id ? el('button', { disabled: !app.selection(modele.id).length,
          onclick: () => { app.selection(modele.id).forEach((u) => st.exclues.add(u)); rendre(); } }, icone('eye-off'), t('exclure_selection')) : null,
        el('span', { class: 'infos' }, `${o.uid.length - st.exclues.size} / ${o.uid.length} ${t('incluses')}`)),
      el('div', { class: 'defile' }, el('table', { class: 'tableau' },
        el('thead', {}, el('tr', {}, el('th', {}, t('inclure')), el('th', {}, t('observations')))),
        el('tbody', {}, idx.slice(0, 3000).map((i) => el('tr', { class: st.exclues.has(o.uid[i]) ? 'exclue' : '' },
          el('td', {}, el('input', { type: 'checkbox', checked: !st.exclues.has(o.uid[i]),
            onchange: (e) => { if (e.target.checked) st.exclues.delete(o.uid[i]); else st.exclues.add(o.uid[i]); majResume(); e.target.closest('tr').classList.toggle('exclue', !e.target.checked); } })),
          el('td', {}, o.nom[i])))))));
  };

  // Cross and square terms: pairs of X variables (both factors must stay in X).
  const termes = () => {
    const nom = (u) => (p.variables.find((v) => v.uid === u) || { nom: `#${u}` }).nom;
    const qual = (u) => !!(p.variables.find((v) => v.uid === u) || {}).qualitative;
    const enX = p.variables.filter((v) => st.roles[v.uid] === 'x');
    const quantX = enX.filter((v) => !v.qualitative).map((v) => v.uid);
    const existe = (a, b) => st.croises.some(([c, d]) => (c === a && d === b) || (c === b && d === a));
    const ajouter = (a, b) => { if (!existe(a, b) && !(a === b && qual(a))) st.croises.push([a, b]); };
    const options = [['', '—']].concat(enX.map((v) => [v.uid, v.nom]));
    const sel = (cle) => el('select', { onchange: (e) => { st[cle] = e.target.value; } },
      options.map(([k, n]) => el('option', { value: k, selected: String(k) === String(st[cle]) }, n)));
    return el('div', { class: 'onglet-termes' },
      el('div', { class: 'outils' }, sel('a'), '×', sel('b'),
        el('button', { onclick: () => {
          const a = Number(st.a);
          const b = Number(st.b);
          if (!st.a || !st.b) return;
          if (a === b && qual(a)) { erreur.textContent = t('carre_qualitatif'); return; }
          ajouter(a, b);
          rendre();
        } }, icone('plus'), t('ajouter')),
        el('span', { class: 'espace' }),
        el('button', { disabled: !quantX.length, onclick: () => { quantX.forEach((u) => ajouter(u, u)); rendre(); } }, t('tous_carres')),
        el('button', { disabled: quantX.length < 2, title: `${quantX.length * (quantX.length - 1) / 2}`,
          onclick: () => { quantX.forEach((u, i) => quantX.slice(i + 1).forEach((w) => ajouter(u, w))); rendre(); } }, t('toutes_interactions')),
        el('button', { disabled: !st.croises.length, onclick: () => { st.croises = []; rendre(); } }, t('tout_retirer'))),
      el('p', { class: 'note' }, t('termes_note')),
      el('div', { class: 'defile' }, st.croises.length ? el('table', { class: 'tableau' }, el('tbody', {}, st.croises.map(([a, b], i) => {
        const horsX = [a, b].filter((u) => st.roles[u] !== 'x');
        return el('tr', {}, el('td', {}, `${nom(a)} × ${nom(b)}`),
          el('td', { class: 'erreur' }, horsX.length ? `${t('facteur_non_x')} : ${horsX.map(nom).join(', ')}` : ''),
          el('td', { class: 'num' }, el('button', { class: 'icone-seule', onclick: () => { st.croises.splice(i, 1); rendre(); } }, icone('x-circle'))));
      }))) : el('p', { class: 'infos' }, t('aucun_terme'))));
  };

  function rendre() {
    contenu.replaceChildren(onglets(), el('div', { class: 'onglet' },
      st.onglet === 'general' ? general() : st.onglet === 'variables' ? variables() : st.onglet === 'termes' ? termes() : observations()));
    majResume();
    erreur.textContent = '';
  }

  function valider() {
    const cles = (r2) => Object.keys(st.roles).filter((k) => st.roles[k] === r2).map(Number);
    const x = cles('x');
    const y = cles('y');
    const classe = cles('classe')[0];
    if (!x.length) { erreur.textContent = t('x_requis'); return false; }
    if (avecY() && !y.length) { erreur.textContent = t('pls_y_requis'); return false; }
    if (st.type === 'OPLS' && y.length > 1) { erreur.textContent = t('opls_un_y'); return false; }
    if (avecClasse() && classe === undefined) { erreur.textContent = t('plsda_classe_requise'); return false; }
    const horsX = [...new Set(st.croises.flat().filter((u) => st.roles[u] !== 'x'))];
    if (horsX.length) {
      erreur.textContent = t('croises_x_requis', { noms: horsX.map((u) => (p.variables.find((v) => v.uid === u) || {}).nom).join(', ') });
      return false;
    }
    const echelles = Object.fromEntries(Object.entries(st.echelles).filter(([, v]) => v && v !== st.echelle));
    const transformations = Object.fromEntries(Object.entries(st.transfos).filter(([, v]) => v && v !== 'autre'));
    const q = { projet: p.id, type: st.type, x, y: avecY() ? y : [], classe: avecClasse() ? classe : null,
      exclues: [...st.exclues], echelle: st.echelle, echelles, transformations, croises: st.croises,
      A: st.auto ? 0 : st.A, G: st.G, nom: st.nom, titre: st.titre };
    verifierPuisAjuster(q);
    return false;  // the dialog closes once the data check is done
  }

  // Checks before fitting: variables or observations with more than 50 % missing
  // values, constant variables. Listed with the choice to exclude them or to fit anyway.
  async function verifierPuisAjuster(q) {
    erreur.textContent = '';
    alerte.replaceChildren(el('p', { class: 'infos' }, t('calcul')));
    let v;
    try {
      v = await app.verifier(q);
    } catch (e) {
      alerte.replaceChildren();
      erreur.textContent = e.message;
      return;
    }
    const vars = v.variables && v.variables.uid ? v.variables.uid.map((u, i) => ({ uid: u, nom: v.variables.nom[i], manque: v.variables.manquantes[i], constante: v.variables.constante[i] })) : [];
    const obs = v.observations && v.observations.uid ? v.observations.uid.map((u, i) => ({ uid: u, nom: v.observations.nom[i], manque: v.observations.manquantes[i] })) : [];
    if (!vars.length && !obs.length) {
      alerte.replaceChildren();
      d.fermer();
      app.ajusterDefinition(q);
      return;
    }
    const pct = (x) => t('manquantes_pct', { p: Math.round(100 * x) });
    alerte.replaceChildren(el('div', { class: 'alerte-donnees' },
      el('b', {}, t('verif_titre')), el('p', { class: 'note' }, t('verif_note')),
      vars.length ? el('p', {}, t('verif_variables'), ' ', vars.map((x) => `${x.nom} (${x.constante ? t('constante') : pct(x.manque)})`).join(', ')) : null,
      obs.length ? el('p', {}, t('verif_observations'), ' ', obs.slice(0, 30).map((x) => `${x.nom} (${pct(x.manque)})`).join(', '), obs.length > 30 ? '…' : '') : null,
      el('div', { class: 'barre-alerte' },
        el('button', { class: 'principal', onclick: () => {
          vars.forEach((x) => { st.roles[x.uid] = ''; });
          st.croises = st.croises.filter((c) => !c.some((u) => vars.some((x) => x.uid === u)));
          obs.forEach((x) => st.exclues.add(x.uid));
          alerte.replaceChildren();
          rendre();
          valider();
        } }, t('verif_exclure')),
        el('button', { onclick: () => { alerte.replaceChildren(); d.fermer(); app.ajusterDefinition(q); } }, t('verif_garder')))));
  }

  rendre();
  const alerte = el('div');
  const d = dialogue({ titre: `${titre} — ${p.nom}`, contenu: el('div', {}, contenu, alerte, erreur), large: true,
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('ajuster'), principal: true, action: valider }] });
  d.boite.querySelector('.pied-dialogue').prepend(resume);
}

function nomLibre(p) {
  const pris = new Set(p.modeles.map((d) => d.nom));
  let k = p.modeles.length + 1;
  while (pris.has(`M${k}`)) k += 1;
  return `M${k}`;
}

// ------------------------------------------------------------ help

const AIDE = {
  fr: {
    guide: [
      ['Ouvrir', 'Fichier › Ouvrir (Ctrl+O) : un projet .usp, un projet enregistré par l’application (.json) ou un tableau (.csv, .xlsx ; une ligne par observation).'],
      ['Projet d’exemple', 'Aide › Projet d’exemple : 120 pommes inventées et quatre modèles prêts (ACP, PLS, PLS-DA, OPLS-DA), pour essayer l’application.'],
      ['Explorateur', 'À gauche : projets, données, modèles importés d’un fichier .usp, modèles calculés et leurs vues. Double-clic pour ouvrir, clic droit pour les actions.'],
      ['Modèles importés', 'Double-cliquer un modèle importé le recalcule en R. Le résumé marque chaque valeur : = identique à la valeur enregistrée dans le fichier, ≈ proche, ≠ différente.'],
      ['Nouveau modèle', 'Modèle › Nouveau modèle (Ctrl+N) : type (ACP, PLS, PLS-DA), rôle, échelle et transformation de chaque variable, observations incluses, nombre de composantes (automatique par défaut).'],
      ['Sélection', 'Cadre ou lasso dans un graphique, clic sur un point ou une ligne de liste : la sélection apparaît dans toutes les vues et toutes les fenêtres du modèle. Échap l’efface ; Ctrl+Suppr exclut les observations et réajuste.'],
      ['Fenêtres', 'Un onglet par modèle : sa page montre résumé, scores et loadings, ses autres vues s’y ajoutent en onglets. Les vues se déplacent par leur onglet (glisser-déposer), se divisent et s’empilent ; glisser l’onglet d’un modèle à côté d’un autre pour les comparer. Clic droit sur un onglet : détacher dans une fenêtre, agrandir, fermer. Fenêtre › Nouvelle fenêtre ouvre une deuxième fenêtre complète.'],
      ['Graphiques', 'Molette : zoom ; clic droit : copier l’image, l’enregistrer en PNG ou SVG, voir les contributions de la sélection, en faire une classe. Listes Couleur et Étiquettes : par classe, groupe du dendrogramme, identifiant ou n’importe quelle variable.'],
      ['Prédire un fichier', 'Prédiction › Prédire un fichier : un tableau CSV ou Excel dont les colonnes portent les noms des variables du modèle. Les observations hors du domaine du modèle sont signalées ; le résultat s’exporte vers Excel.'],
      ['Modèles hiérarchiques', 'Modèle › Scores en variables : les scores d’un modèle deviennent des variables ($M1.t1…) utilisables comme X d’un autre modèle. Après avoir modifié le modèle de base, refaire cette étape puis réajuster le modèle supérieur.'],
      ['Script R', 'Fichier › Exporter en script R : un script commenté qui refait le modèle avec les fonctions du moteur, à relire et modifier dans RStudio.'],
      ['Rapport', 'Fichier › Rapport du modèle : un PDF ou un document Word avec les parties choisies ; les graphiques y sont mis en forme comme dans leurs panneaux.'],
      ['Console R', 'Affichage › Console R (Ctrl+J) : le code R de chaque action, et une invite R dans la session de l’application (m = modèle affiché, p = son projet). recharger_moteur() relit les fichiers du moteur après une correction.'],
      ['Séance', 'Ctrl+S enregistre aussi la séance : modèles calculés, panneaux, réglages des graphiques et sélections reviennent à la réouverture du projet.'],
      ['Sécurité', 'En fermant : « Enregistrer et fermer ». Copie de secours toutes les deux minutes, proposée après un arrêt imprévu. Ctrl+Z annule une modification des données. Aide › Redémarrer le moteur R si un calcul ne finit pas.'],
      ['Statistiques des variables', 'Graphiques › Statistiques des variables : statistiques du jeu de travail, transformations proposées en rouge, histogrammes et carte des valeurs manquantes.'],
      ['Données', 'Double-clic sur une case de la vue Données pour la modifier ; Édition › Nouvelle variable : formule R ou classes d’après les valeurs.'],
      ['OPLS', 'Une composante prédictive et des composantes orthogonales ; un seul Y, ou deux classes (OPLS-DA). Graphique S (S-plot) dans Graphiques.'],
      ['Historique du fichier', 'Sous un projet .usp : le journal d’audit enregistré dans le fichier (dates, versions, changements de chaque modèle).'],
      ['Code', 'Tous les calculs sont en R, commentés en français, dans resources\\moteur\\R.'],
    ],
    differences: [
      ['Identique aux valeurs enregistrées', 'Vérifié sur 944 modèles de 133 projets .usp : mise à l’échelle, R2X, R2Y, Q2 des PLS, scores, loadings, valeurs propres, T2 et sa limite, DModX (normal et pondéré) et DCrit, VIP, coefficients, RMSEE.'],
      ['Q2 des ACP', 'Les valeurs enregistrées viennent d’une étape non publiée ; l’écart typique est de 0,01.'],
      ['Signe des composantes', 'Arbitraire pour les nouveaux modèles ; les modèles importés sont orientés comme la solution enregistrée.'],
      ['Dendrogrammes', 'Méthode de Ward exacte : les fusions du haut de l’arbre peuvent différer de celles enregistrées.'],
      ['Intervalles de confiance', 'Jack-knife sur les tours de validation croisée : proches des valeurs enregistrées, pas identiques.'],
      ['Ajustement automatique', 'Règles publiées R1, R2, N3, N4 ; pour l’ACP, la limite exacte n’est pas publiée.'],
      ['Seuil VIP', 'Plus grande marche des VIP triés autour de 0,8 : méthode de l’application.'],
    ],
  },
  en: {
    guide: [
      ['Open', 'File › Open (Ctrl+O): a .usp project, a project saved by this application (.json), or a table (.csv, .xlsx; one row per observation).'],
      ['Example project', 'Help › Example project: 120 made-up apples and four ready models (PCA, PLS, PLS-DA, OPLS-DA), to try the application.'],
      ['Explorer', 'On the left: projects, data, models imported from a .usp file, fitted models and their views. Double-click to open, right-click for actions.'],
      ['Imported models', 'Double-clicking an imported model recomputes it in R. The summary marks each value: = same as the value stored in the file, ≈ close, ≠ different.'],
      ['New model', 'Model › New model (Ctrl+N): type (PCA, PLS, PLS-DA), each variable’s role, scaling and transform, included observations, number of components (automatic by default).'],
      ['Selection', 'Box or lasso in a plot, click a point or a list row: the selection shows in every view and window of the model. Esc clears it; Ctrl+Delete excludes the observations and refits.'],
      ['Windows', 'One tab per model: its page shows the summary, scores and loadings, and its other views join it as tabs. Drag views by their tab to move, split and stack them; drag a model’s tab beside another to compare them. Right-click a tab to move it to a new window, maximize or close it. Window › New window opens a second full window.'],
      ['Plots', 'Wheel: zoom; right-click: copy the image, save it as PNG or SVG, see the contributions of the selection, make it a class. Colour and Labels lists: by class, dendrogram group, ID or any variable.'],
      ['Predict a file', 'Predict › Predict a file: a CSV or Excel table whose columns carry the model’s variable names. Observations outside the model’s domain are flagged; results export to Excel.'],
      ['Hierarchical models', 'Model › Scores as variables: a model’s scores become variables ($M1.t1…) usable as X of another model. After changing the base model, do it again and refit the upper model.'],
      ['R script', 'File › Export as R script: a commented script that rebuilds the model with the engine’s functions, to read and change in RStudio.'],
      ['Report', 'File › Model report: a PDF or Word document with the chosen parts; plots look as in their panels.'],
      ['R console', 'View › R console (Ctrl+J): the R code of each action, and an R prompt in the application’s session (m = model shown, p = its project). recharger_moteur() reloads the engine files after a correction.'],
      ['Session', 'Ctrl+S also saves the session: fitted models, panels, plot settings and selections come back when the project is reopened.'],
      ['Safety', 'When closing: “Save and close”. A recovery copy every two minutes, offered after an unexpected stop. Ctrl+Z undoes a data change. Help › Restart the R engine if a calculation never ends.'],
      ['Variable statistics', 'Plots › Variable statistics: workset statistics, proposed transformations in red, histograms and a missing-value map.'],
      ['Data', 'Double-click a cell of the Data view to edit it; Edit › New variable: an R formula or classes from values.'],
      ['OPLS', 'One predictive component and orthogonal ones; a single Y, or two classes (OPLS-DA). S-plot in Plots.'],
      ['File history', 'Under a .usp project: the audit trail stored in the file (dates, versions, changes to each model).'],
      ['Code', 'All computations are in R, commented in French, in resources\\moteur\\R.'],
    ],
    differences: [
      ['Same as the stored values', 'Checked on 944 models from 133 .usp projects: scaling, R2X, R2Y, PLS Q2, scores, loadings, eigenvalues, T2 and its limit, DModX (plain and weighted) and DCrit, VIP, coefficients, RMSEE.'],
      ['PCA Q2', 'The stored values come from an unpublished step; typical difference 0.01.'],
      ['Component signs', 'Arbitrary for new models; imported models are oriented like their stored solution.'],
      ['Dendrograms', 'Exact Ward linkage: the top merges can differ from the stored ones.'],
      ['Confidence intervals', 'Jack-knife over the cross-validation rounds: close to the stored ones, not identical.'],
      ['Autofit', 'Published rules R1, R2, N3, N4; for PCA, the exact limit is not published.'],
      ['VIP cut-off', 'Largest step in the sorted VIPs around 0.8: the application’s method.'],
    ],
  },
};

export async function dialogueAide(app, sujet) {
  if (sujet === 'apropos') {
    const v = await app.version();
    dialogue({ titre: t('apropos'), contenu: el('div', { class: 'apropos' },
      el('div', { class: 'apropos-tete' }, el('img', { class: 'logo', src: 'nuee.svg', alt: '' }),
        el('div', {}, el('h3', {}, t('titre')), el('p', { class: 'doux' }, t('sous_titre')))),
      el('dl', {}, el('dt', {}, t('version')), el('dd', {}, v.application),
        el('dt', {}, t('moteur')), el('dd', {}, v.R),
        el('dt', {}, t('interface')), el('dd', {}, `Electron ${v.electron || '—'} · dockview · Plotly`),
        el('dt', {}, t('licence')), el('dd', {}, 'GPL-3.0 · github.com/4rtefakt/nuee'))),
    boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
    return;
  }
  const textes = AIDE[langueActuelle()] || AIDE.fr;
  dialogue({ titre: t(sujet), large: true, contenu: el('div', { class: 'aide' },
    textes[sujet].map(([h, p]) => el('section', {}, el('h4', {}, h), el('p', {}, p)))),
  boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
}

const POLICES = ['Segoe UI', 'Arial', 'Calibri', 'Cambria', 'Times New Roman', 'Georgia', 'Verdana', 'Courier New'];

// Plot formatting: this plot's title and axis titles; for every plot the font, text and
// point sizes, palette, grid and legend (kept between sessions); the image exported at a
// chosen size.
export function dialogueFormat(app, entree, div) {
  const l = entree.loc;
  const f = { ...FORMAT };
  const titres = { titre: l.titre, titreX: l.titreX, titreY: l.titreY };
  const champ = (libelle, controle) => [el('label', {}, libelle), controle];
  const texte = (cle, defaut) => el('input', { value: titres[cle] || '', placeholder: defaut || '', style: 'width:24em',
    oninput: (e) => { titres[cle] = e.target.value; } });
  const nombre = (cle, min, max, pas) => el('input', { type: 'number', min, max, step: pas, value: f[cle], style: 'width:6em',
    oninput: (e) => { f[cle] = +e.target.value || f[cle]; } });
  const liste = (cle, options) => el('select', { onchange: (e) => { f[cle] = e.target.value; } },
    options.map(([k, n]) => el('option', { value: k, selected: k === f[cle] }, n)));
  const coche = (cle) => el('input', { type: 'checkbox', checked: f[cle], onchange: (e) => { f[cle] = e.target.checked; } });
  const lay = div.layout || {};
  const contenu = el('div', { class: 'dialogue-format' },
    el('h3', {}, t('ce_graphique')),
    el('div', { class: 'grille-champs' },
      ...champ(t('titre_graphe'), texte('titre', lay.title && lay.title.text)),
      ...champ(t('titre_axe_x'), texte('titreX', lay.xaxis && lay.xaxis.title && lay.xaxis.title.text)),
      ...champ(t('titre_axe_y'), texte('titreY', lay.yaxis && lay.yaxis.title && lay.yaxis.title.text))),
    el('p', { class: 'note' }, t('titres_vides')),
    el('h3', {}, t('tous_graphiques')),
    el('div', { class: 'grille-champs' },
      ...champ(t('police'), liste('police', POLICES.map((x) => [x, x]))),
      ...champ(t('taille_texte'), nombre('taille', 7, 28, 1)),
      ...champ(t('taille_points'), nombre('points', 0.3, 4, 0.1)),
      ...champ(t('palette'), liste('palette', [['defaut', t('palette_defaut')], ['daltonien', t('palette_daltonien')], ['gris', t('palette_gris')]])),
      ...champ(t('grille'), coche('grille')),
      ...champ(t('legende'), coche('legende'))),
    el('h3', {}, t('export_image')),
    el('div', { class: 'grille-champs' },
      ...champ(t('taille_image'), el('div', { class: 'ligne' }, nombre('largeur', 200, 10000, 10), ' × ', nombre('hauteur', 200, 10000, 10), ' px')),
      ...champ(t('echelle_image'), nombre('echelle', 1, 6, 1)),
      ...champ(t('format_image'), liste('format', [['png', 'PNG'], ['svg', 'SVG'], ['jpeg', 'JPEG']]))));
  const appliquer = () => {
    for (const [k, v] of Object.entries(titres)) if (v && v.trim()) l[k] = v.trim(); else delete l[k];
    changerFormat(f);
    app.redessiner();
  };
  const exporter = () => {
    const g = entree.graphe && entree.graphe.isConnected ? entree.graphe : div;
    const nom = ((g.layout && g.layout.title && g.layout.title.text) || 'graphique').replace(/[^\p{L}\p{N} _.-]+/gu, '_');
    window.Plotly.downloadImage(g, { format: f.format, width: f.largeur, height: f.hauteur, scale: f.format === 'svg' ? 1 : f.echelle, filename: nom });
  };
  dialogue({ titre: t('mise_en_forme'), contenu, boutons: [
    { libelle: t('annuler'), action: () => true },
    { libelle: t('exporter_image'), action: () => { appliquer(); setTimeout(exporter, 400); return true; } },
    { libelle: t('appliquer'), principal: true, action: () => { appliquer(); return true; } }] });
}

// New variable: an R formula on the project's variables, or classes cut from the values of a
// quantitative variable (given bounds, or classes of equal size). Computed in R
// (moteur/R/variables.R).
export function dialogueVariable(app, p, mode = 'formule') {
  const quantitatives = p.variables.filter((v) => !v.qualitative);
  const st = { mode, nom: '', formule: '', variable: quantitatives.length ? quantitatives[0].uid : null, parBornes: true, bornes: '', n: 3 };
  const contenu = el('div', { class: 'dialogue-variable' });
  const erreur = el('p', { class: 'erreur' });
  const nomR = (n) => (/^[\p{L}.][\p{L}\p{N}._]*$/u.test(n) ? n : `\`${n}\``);
  const rendre = () => {
    const zone = el('textarea', { rows: 3, class: 'formule', oninput: (e) => { st.formule = e.target.value; } }, st.formule);
    const inserer = el('select', { onchange: (e) => {
      if (!e.target.value) return;
      const debut = zone.selectionStart ?? zone.value.length;
      zone.setRangeText(nomR(e.target.value), debut, zone.selectionEnd ?? debut, 'end');
      st.formule = zone.value;
      e.target.value = '';
      zone.focus();
    } }, el('option', { value: '' }, t('inserer_variable')), p.variables.map((v) => el('option', { value: v.nom }, v.nom)));
    const formule = () => [
      el('label', {}, t('formule')), el('div', {}, zone, el('div', { class: 'ligne' }, inserer)),
      el('label', {}, ''), el('p', { class: 'note' }, t('formule_aide'))];
    const classes = () => [
      el('label', {}, t('variable_source')), el('select', { onchange: (e) => { st.variable = +e.target.value; } },
        quantitatives.map((v) => el('option', { value: v.uid, selected: v.uid === st.variable }, v.nom))),
      el('label', {}, ''), el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'decoupage', checked: st.parBornes, onchange: () => { st.parBornes = true; rendre(); } }), t('par_bornes')),
      el('label', {}, t('bornes')), el('input', { value: st.bornes, placeholder: '10 ; 20', disabled: !st.parBornes, oninput: (e) => { st.bornes = e.target.value; } }),
      el('label', {}, ''), el('label', { class: 'champ' }, el('input', { type: 'radio', name: 'decoupage', checked: !st.parBornes, onchange: () => { st.parBornes = false; rendre(); } }), t('par_effectifs')),
      el('label', {}, t('nb_classes')), el('input', { type: 'number', min: 2, max: 20, value: st.n, style: 'width:5em', disabled: st.parBornes, oninput: (e) => { st.n = +e.target.value; } })];
    contenu.replaceChildren(
      el('div', { class: 'onglets' }, [['formule', t('formule')], ['classes', t('classes_valeurs')]].map(([k, n]) =>
        el('button', { class: st.mode === k ? 'actif' : '', onclick: () => { st.mode = k; rendre(); } }, n))),
      el('div', { class: 'grille-champs' }, el('label', {}, t('nom')), el('input', { value: st.nom, oninput: (e) => { st.nom = e.target.value; } }),
        ...(st.mode === 'formule' ? formule() : classes())));
  };
  async function valider() {
    erreur.textContent = '';
    let q;
    if (st.mode === 'formule') q = { nom: st.nom, formule: st.formule };
    else if (st.parBornes) q = { nom: st.nom, variable: st.variable, bornes: st.bornes.split(/[;\s]+/).filter(Boolean).map((x) => Number(x.replace(',', '.'))) };
    else q = { nom: st.nom, variable: st.variable, n: st.n };
    try {
      await app.ajouterVariable(p.id, q);
      d.fermer();
    } catch (e) {
      erreur.textContent = e.message;
    }
  }
  rendre();
  const d = dialogue({ titre: `${t('nouvelle_variable')} — ${p.nom}`, contenu: el('div', {}, contenu, erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('ajouter'), principal: true, action: () => { valider(); return false; } }] });
}
