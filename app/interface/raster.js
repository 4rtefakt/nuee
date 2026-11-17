// Edit › Add raster values (and the Map view's button): one or more raster files (GeoTIFF, ESRI
// ASCII grid) read by the engine at each observation's position, from the project's coordinates
// (moteur/R/raster.R). Before adding, each file's size, format, compression, cells, bands, data
// type and coordinate system, and how many observations fall in it; then the method (the cell, a
// bilinear interpolation, or the mean or majority class within a radius) and a name per band.
// Files of the same product side by side (the tiles of a DEM) can make one variable per band.
// The values join the project as ordinary variables, undone by Ctrl+Z. Imported by app.js.
import { t, ajouterTextes, langueActuelle } from './i18n.js';
import { el, icone, dialogue, decimal } from './ui.js';
import { demander, bureau, deposerEnsemble, MOTEUR_WEBR } from './api.js';

const liste = (x) => (x === null || x === undefined ? [] : [].concat(x));  // (R sends one value as a scalar)
const METHODES = ['cellule', 'bilineaire', 'moyenne', 'majorite'];
const AVEC_RAYON = new Set(['moyenne', 'majorite']);

// a size in bytes, in the interface's language
function taille(octets) {
  if (octets === null || octets === undefined) return '';
  const fr = langueActuelle() === 'fr';
  const unites = fr ? ['o', 'Ko', 'Mo', 'Go', 'To'] : ['B', 'KB', 'MB', 'GB', 'TB'];
  let k = 0;
  let v = octets;
  while (v >= 1000 && k < unites.length - 1) { v /= 1000; k += 1; }
  const texte = decimal(k ? v.toFixed(v < 10 ? 1 : 0) : String(v));
  return `${texte} ${unites[k]}`;
}

// what the engine's description of a file says, in one line (its cells counted with the
// thousands of the interface's language, not the system's: "1,250" reads 1.25 in French)
function resume(info) {
  const entier = (n) => n.toLocaleString(langueActuelle() === 'fr' ? 'fr-FR' : undefined);
  const compression = info.compression === 'aucune' ? t('raster_sans_compression')
    : info.compression ? `${info.compression}${info.predicteur > 1 ? ` (${t('raster_predicteur', { n: info.predicteur })})` : ''}` : '';
  const systeme = info.systeme.suppose ? t('raster_systeme_projet', { nom: info.systeme.nom })
    : info.systeme.code !== null && info.systeme.code !== undefined ? `${info.systeme.nom} (EPSG:${info.systeme.code})` : info.systeme.nom;
  return [info.format === 'ASCII' ? t('raster_format_asc') : info.format, compression, info.organisation,
    t('raster_cellules', { l: entier(info.largeur), h: entier(info.hauteur), r: info.resolution }),
    t(info.bandes > 1 ? 'raster_bandes' : 'raster_bande', { n: info.bandes, type: t(`raster_type_${info.type}`) }),
    info.bandes_separees ? t('raster_bandes_separees') : '', systeme,
    info.nodata !== null && info.nodata !== undefined ? t('raster_nodata', { v: decimal(info.nodata) }) : ''].filter(Boolean).join(' · ');
}

// the free name as the engine gives it (variables.R: nom_libre): "name", else "name (2)"...
function nomLibre(nom, pris) {
  let libre = nom;
  for (let k = 2; pris.has(libre); k += 1) libre = `${nom} (${k})`;
  return libre;
}

// two extents that share more than a sliver of area (the same product twice, not tiles side by side)
function chevauchent(a, b) {
  const l = Math.min(a.xmax, b.xmax) - Math.max(a.xmin, b.xmin);
  const h = Math.min(a.ymax, b.ymax) - Math.max(a.ymin, b.ymin);
  if (l <= 0 || h <= 0) return false;
  const aire = (e) => (e.xmax - e.xmin) * (e.ymax - e.ymin);
  return (l * h) / Math.min(aire(a), aire(b)) > 0.01;
}

export function dialogueRaster(app, p) {
  if (!p.coordonnees) {
    const d = dialogue({ titre: t('raster_valeurs').replace('…', ''), contenu: el('div', {}, el('p', {}, t('raster_sans_coordonnees'))),
      boutons: [{ libelle: t('annuler'), action: () => true },
        { libelle: t('carte'), principal: true, action: () => { app.ouvrirVue('carte', null, null, p.id); return true; } }] });
    return d;
  }
  const st = { fichiers: [], tuiles: false };
  const contenu = el('div', { class: 'dialogue-raster' });
  const erreur = el('p', { class: 'erreur' });
  const lisibles = () => st.fichiers.filter((f) => !f.info.erreur);
  // tiles of one raster: two readable files or more of the same bands, type, system and cells, side by side
  const enTuiles = () => {
    const ok = lisibles();
    if (ok.length < 2) return false;
    const a = ok[0].info;
    const memes = ok.every((f) => f.info.bandes === a.bandes && f.info.type === a.type && f.info.systeme.code === a.systeme.code
      && f.info.resolution === a.resolution);
    return memes;
  };
  const proposerTuiles = () => enTuiles() && !lisibles().some((f, i, ok) => ok.slice(0, i).some((g) => chevauchent(f.info.etendue, g.info.etendue)));
  // as tiles, one name per band for the whole raster: the start the files' names share, without
  // the tile's numbers (RGEALTI_FXX_0925_6315… and RGEALTI_FXX_0926_6315… give RGEALTI_FXX);
  // one variable per file again: each file's own names
  const appliquerTuiles = (oui) => {
    const ok = lisibles();
    st.tuiles = oui;
    if (!oui) { for (const f of ok) f.noms = [...liste(f.info.noms)]; return; }
    const communs = liste(ok[0].info.noms).map((n, b) => {
      let debut = n;
      for (const f of ok) { const m = liste(f.info.noms)[b] || ''; while (debut && !m.startsWith(debut)) debut = debut.slice(0, -1); }
      debut = debut.replace(/[\s_.-]*\d*$/, '').replace(/[\s_.-]+$/, '');
      return debut.length >= 2 ? debut : n;
    });
    for (const f of ok) { f.noms = [...communs]; f.methode = ok[0].methode; f.rayon = ok[0].rayon; }
  };
  const ajouterChemins = async (chemins) => {
    erreur.textContent = '';
    const nouveaux = chemins.filter((c) => !st.fichiers.some((f) => f.chemin === c));
    if (!nouveaux.length) return;
    let r;
    try { r = await app.occupe(() => demander('raster_infos', { projet: p.id, chemins: nouveaux })); } catch (e) { erreur.textContent = e.message; return; }
    for (const info of liste(r.fichiers)) {
      st.fichiers.push({ chemin: info.chemin, info, methode: info.methode || 'bilineaire', rayon: '', noms: liste(info.noms) });
    }
    const tuiles = proposerTuiles();  // (the names typed so far stay, unless the files become tiles or stop being tiles)
    if (tuiles || st.tuiles) appliquerTuiles(tuiles);
    rendre();
  };
  const choisir = async () => {
    if (bureau) {
      const chemins = await bureau.choisirFichiers([{ name: t('raster_filtre'), extensions: ['tif', 'tiff', 'asc'] }, { name: t('tous_fichiers'), extensions: ['*'] }]);
      if (chemins && chemins.length) ajouterChemins(chemins);
      return;
    }
    // (the browser version: the files chosen go into R's file system, in the tab)
    if (MOTEUR_WEBR) {
      const fichiers = await new Promise((resolve) => {
        const i = el('input', { type: 'file', multiple: true, accept: '.tif,.tiff,.asc,.tfw,.prj', onchange: () => resolve([...i.files]) });
        i.click();
      });
      // (in one folder: a .tfw or a .prj beside its raster, where the engine looks for it)
      const rasters = (await deposerEnsemble(fichiers)).filter((c) => /\.(tiff?|asc)$/i.test(c));
      if (rasters.length) ajouterChemins(rasters);
      return;
    }
    // (in the browser there is no file chooser that gives a path: the files stay where they are, the engine reads them)
    const texte = window.prompt(t('raster_chemin_invite'));
    if (!texte) return;
    const chemins = texte.split(';').map((c) => c.trim().replace(/^"(.*)"$/, '$1')).filter(Boolean);
    if (chemins.length) ajouterChemins(chemins);
  };
  // the names the variables would take (the same name twice: one variable, made of both files)
  const nomsAjoutes = () => {
    const pris = new Set([...p.variables.map((v) => v.nom), ...liste(p.identifiants).map((i) => i.nom)]);
    const donnes = new Map();
    for (const f of lisibles()) for (const n of f.noms) {
      const nom = n.trim();
      if (nom && !donnes.has(nom)) { const libre = nomLibre(nom, pris); pris.add(libre); donnes.set(nom, libre); }
    }
    return donnes;
  };
  const champMethode = (f, tous) => {
    const rayon = el('input', { type: 'number', min: 1, step: 'any', value: f.rayon, placeholder: 'm', style: 'width:7em', disabled: !AVEC_RAYON.has(f.methode),
      title: t('raster_rayon_aide'), oninput: (e) => { for (const g of tous) g.rayon = e.target.value; } });
    return [el('label', {}, t('raster_methode')), el('div', { class: 'ligne' },
      el('select', { title: t('raster_methode_aide'), onchange: (e) => { for (const g of tous) g.methode = e.target.value; rendre(); } },
        METHODES.map((m) => el('option', { value: m, selected: m === f.methode }, t(`raster_methode_${m}`)))),
      el('label', { class: 'champ', title: t('raster_rayon_aide') }, t('raster_rayon'), rayon, 'm'))];
  };
  const champsNoms = (f, tous, ajoutes) => f.noms.map((n, b) => {
    const libre = ajoutes.get(n.trim());
    return [el('label', {}, f.noms.length > 1 ? t('raster_nom_bande', { n: b + 1 }) : t('raster_nom')), el('div', { class: 'ligne' },
      el('input', { value: n, style: 'width:16em', oninput: (e) => { for (const g of tous) g.noms[b] = e.target.value; majNoms(); } }),
      el('span', { class: 'doux nom-ajoute', 'data-bande': b, 'data-fichier': f.chemin }, libre && libre !== n.trim() ? t('raster_renomme', { nom: libre }) : ''))];
  });
  // (the "added as" notes follow the names typed, without drawing the dialog again)
  const majNoms = () => {
    const ajoutes = nomsAjoutes();
    for (const s of contenu.querySelectorAll('.nom-ajoute')) {
      const f = st.fichiers.find((x) => x.chemin === s.dataset.fichier);
      const n = f ? f.noms[+s.dataset.bande].trim() : '';
      const libre = ajoutes.get(n);
      s.textContent = libre && libre !== n ? t('raster_renomme', { nom: libre }) : '';
    }
  };
  const carte = (f, i, ajoutes, reglages) => {
    const info = f.info;
    const tete = el('div', { class: 'ligne raster-tete' }, icone(info.erreur ? 'x-circle' : 'layers'), el('b', { title: f.chemin }, info.nom),
      el('span', { class: 'doux' }, taille(info.taille)),
      el('button', { class: 'icone-seule', title: t('retirer'), onclick: () => { st.fichiers.splice(i, 1); if (st.tuiles && !enTuiles()) appliquerTuiles(false); rendre(); } }, icone('trash-2')));
    if (info.erreur) {
      // (the engine's message starts with the file's name, already in the card's title)
      const sans = info.erreur.startsWith(`${info.nom} : `) ? info.erreur.slice(info.nom.length + 3) : info.erreur.startsWith(`${info.nom}: `) ? info.erreur.slice(info.nom.length + 2) : info.erreur;
      return el('div', { class: 'raster-fichier refuse' }, tete, el('p', { class: 'erreur' }, sans.charAt(0).toUpperCase() + sans.slice(1)));
    }
    const dedans = info.dedans === info.observations ? t('raster_dedans_tous', { n: info.dedans, total: info.total })
      : t('raster_dedans', { n: info.dedans, obs: info.observations, total: info.total });
    return el('div', { class: 'raster-fichier' }, tete, el('p', { class: 'infos' }, resume(info)),
      el('p', { class: info.dedans ? 'infos' : 'erreur' }, info.dedans || st.tuiles ? dedans : t('raster_aucune_dedans')),
      liste(info.notes).length ? el('p', { class: 'note' }, liste(info.notes).join(' ')) : null,
      reglages ? el('div', { class: 'grille-champs' }, ...champMethode(f, [f]), ...champsNoms(f, [f], ajoutes)) : null);
  };
  const rendre = () => {
    const ajoutes = nomsAjoutes();
    const ok = lisibles();
    const blocs = [el('div', { class: 'ligne' },
      el('button', { onclick: choisir }, icone('folder-open'), t(st.fichiers.length ? 'raster_autres_fichiers' : 'raster_choisir')),
      el('span', { class: 'infos' }, t('raster_coordonnees', { x: p.coordonnees.nom_x, y: p.coordonnees.nom_y }))),
    el('p', { class: 'note' }, t('raster_aide'))];
    if (enTuiles()) {
      blocs.push(el('label', { class: 'champ raster-tuiles', title: t('raster_tuiles_aide') },
        el('input', { type: 'checkbox', checked: !!st.tuiles, onchange: (e) => { appliquerTuiles(e.target.checked); rendre(); } }), t('raster_tuiles', { n: ok.length })));
    }
    if (st.tuiles && ok.length) {
      const total = ok.reduce((s, f) => s + f.info.dedans, 0);
      blocs.push(el('div', { class: 'raster-groupe' }, el('p', { class: 'infos' }, t('raster_tuiles_dedans', { n: total, total: ok[0].info.total })),
        el('div', { class: 'grille-champs' }, ...champMethode(ok[0], ok), ...champsNoms(ok[0], ok, ajoutes))));
    }
    blocs.push(el('div', { class: 'raster-fichiers' }, st.fichiers.map((f, i) => carte(f, i, ajoutes, !st.tuiles))));
    contenu.replaceChildren(...blocs);
  };
  async function valider() {
    erreur.textContent = '';
    const ok = lisibles();
    if (!ok.length) { erreur.textContent = t('raster_aucun_fichier'); return; }
    const sansRayon = ok.find((f) => AVEC_RAYON.has(f.methode) && !(Number(String(f.rayon).replace(',', '.')) > 0));
    if (sansRayon) { erreur.textContent = t('raster_rayon_manquant', { nom: sansRayon.info.nom }); return; }
    const q = { fichiers: ok.map((f) => ({ chemin: f.chemin, methode: f.methode, noms: f.noms.map((n) => n.trim()),
      rayon: AVEC_RAYON.has(f.methode) ? Number(String(f.rayon).replace(',', '.')) : null })) };
    let res;
    try { res = await app.ajouterRaster(p.id, q); } catch (e) { erreur.textContent = e.message; return; }
    d.fermer();
    dialogue({ titre: `${t('raster_fait')} — ${p.nom}`, contenu: el('div', {}, liste(res.raster.phrases).map((x) => el('p', {}, x)), el('p', { class: 'note' }, t('raster_annuler'))),
      boutons: [{ libelle: t('fermer'), principal: true, action: () => true }] });
  }
  rendre();
  const d = dialogue({ titre: `${t('raster_valeurs').replace('…', '')} — ${p.nom}`, large: true, contenu: el('div', {}, contenu, erreur),
    boutons: [{ libelle: t('annuler'), action: () => true }, { libelle: t('ajouter'), principal: true, action: () => { valider(); return false; } }] });
  return d;
}

ajouterTextes({
  raster_valeurs: 'Ajouter des valeurs de rasters…',
  raster_carte_aide: 'Les valeurs d’un raster (GeoTIFF, grille ASCII : altitude, occupation du sol, climat) à la position de chaque observation, ajoutées au projet comme de nouvelles variables',
  raster_sans_coordonnees: 'Ce projet n’a pas encore de coordonnées : choisissez-les dans la carte (deux variables et leur système), puis revenez ici.',
  raster_choisir: 'Choisir des fichiers…', raster_autres_fichiers: 'Ajouter d’autres fichiers…',
  raster_filtre: 'Rasters (GeoTIFF, grille ASCII)',
  raster_chemin_invite: 'Chemin complet du fichier raster (GeoTIFF .tif ou grille ASCII .asc) ; plusieurs : séparés par « ; ». Le moteur le lit là où il est.',
  raster_coordonnees: 'Aux coordonnées du projet : {x}, {y}',
  raster_aide: 'Un GeoTIFF (.tif : un MNT, l’occupation du sol, un climat, exporté de QGIS ou téléchargé) ou une grille ASCII d’ESRI (.asc). Chaque bande devient une variable du projet, sa valeur lue à la position de chaque observation ; seuls les blocs du fichier qui contiennent des observations sont lus.',
  raster_sans_compression: 'sans compression', raster_predicteur: 'prédicteur {n}', raster_format_asc: 'grille ASCII d’ESRI',
  raster_cellules: '{l} × {h} cellules de {r}', raster_bande: '1 bande, {type}', raster_bandes: '{n} bandes, {type}', raster_bandes_separees: 'bandes séparées',
  raster_systeme_projet: 'système non précisé, pris dans celui du projet ({nom})', raster_nodata: 'valeur manquante {v}',
  raster_type_uint8: 'entiers de 0 à 255', raster_type_int8: 'entiers de −128 à 127', raster_type_uint16: 'entiers de 0 à 65 535', raster_type_int16: 'entiers de 16 bits',
  raster_type_uint32: 'entiers positifs de 32 bits', raster_type_int32: 'entiers de 32 bits', raster_type_float32: 'réels (32 bits)', raster_type_float64: 'réels (64 bits)',
  raster_type_texte: 'nombres décimaux', raster_type_texte_entiers: 'nombres entiers',
  raster_dedans: '{n} observation(s) dans le raster, sur {obs} qui ont des coordonnées ({total} en tout).',
  raster_dedans_tous: 'Les {n} observations qui ont des coordonnées sont dans le raster ({total} en tout).',
  raster_aucune_dedans: 'Aucune observation n’est dans ce raster : ses valeurs seraient toutes manquantes. Le système de coordonnées est-il le bon ?',
  raster_methode: 'Valeur', raster_methode_cellule: 'La cellule de l’observation (classes, occupation du sol)',
  raster_methode_bilineaire: 'Interpolation bilinéaire (valeurs continues : altitude, climat)',
  raster_methode_moyenne: 'Moyenne des cellules dans un rayon', raster_methode_majorite: 'Classe majoritaire dans un rayon (classes)',
  raster_methode_aide: 'La cellule qui contient l’observation ; l’interpolation bilinéaire des quatre cellules les plus proches, de leurs centres ; la moyenne, ou la classe la plus fréquente, des cellules dont le centre est à moins du rayon (comme terra::extract).',
  raster_rayon: 'Rayon', raster_rayon_aide: 'En mètres : les cellules dont le centre est à moins de cette distance de l’observation (aucune : sa propre cellule). Pour des longitudes et latitudes aussi, en mètres.',
  raster_nom: 'Nom', raster_nom_bande: 'Nom de la bande {n}', raster_renomme: '→ ajoutée sous le nom « {nom} » (nom déjà pris)',
  raster_tuiles: 'Ces {n} fichiers sont les tuiles d’un même raster : une variable par bande, chaque observation prend la valeur de la tuile qui la contient',
  raster_tuiles_aide: 'Les dalles d’un MNT, côte à côte : les mêmes bandes, le même type, le même système et les mêmes cellules. Décochez pour une variable par fichier.',
  raster_tuiles_dedans: '{n} observation(s) dans ces tuiles ({total} en tout).',
  raster_aucun_fichier: 'Choisissez au moins un fichier lisible.', raster_rayon_manquant: '{nom} : donnez le rayon du cercle, en mètres.',
  raster_fait: 'Valeurs de rasters ajoutées', raster_annuler: 'Édition › Annuler (Ctrl+Z) les retire. Ce sont des variables comme les autres : enregistrées avec le projet, le texte des Méthodes d’un modèle qui les utilise dit d’où elles viennent.',
}, {
  raster_valeurs: 'Add raster values…',
  raster_carte_aide: 'A raster’s values (GeoTIFF, ASCII grid: elevation, land cover, climate) at each observation’s position, added to the project as new variables',
  raster_sans_coordonnees: 'This project has no coordinates yet: choose them in the map (two variables and their system), then come back here.',
  raster_choisir: 'Choose files…', raster_autres_fichiers: 'Add more files…',
  raster_filtre: 'Rasters (GeoTIFF, ASCII grid)',
  raster_chemin_invite: 'Full path of the raster file (GeoTIFF .tif or ASCII grid .asc); several: separated by “;”. The engine reads it where it is.',
  raster_coordonnees: 'At the project’s coordinates: {x}, {y}',
  raster_aide: 'A GeoTIFF (.tif: a DEM, land cover, a climate layer, exported from QGIS or downloaded) or an ESRI ASCII grid (.asc). Each band becomes a variable of the project, its value read at each observation’s position; only the blocks of the file that hold observations are read.',
  raster_sans_compression: 'uncompressed', raster_predicteur: 'predictor {n}', raster_format_asc: 'ESRI ASCII grid',
  raster_cellules: '{l} × {h} cells of {r}', raster_bande: '1 band, {type}', raster_bandes: '{n} bands, {type}', raster_bandes_separees: 'separate bands',
  raster_systeme_projet: 'system not given, taken as the project’s ({nom})', raster_nodata: 'no data {v}',
  raster_type_uint8: 'integers from 0 to 255', raster_type_int8: 'integers from −128 to 127', raster_type_uint16: 'integers from 0 to 65,535', raster_type_int16: '16-bit integers',
  raster_type_uint32: '32-bit positive integers', raster_type_int32: '32-bit integers', raster_type_float32: 'reals (32 bits)', raster_type_float64: 'reals (64 bits)',
  raster_type_texte: 'decimal numbers', raster_type_texte_entiers: 'whole numbers',
  raster_dedans: '{n} observation(s) in the raster, of the {obs} that have coordinates ({total} in all).',
  raster_dedans_tous: 'The {n} observations that have coordinates are in the raster ({total} in all).',
  raster_aucune_dedans: 'No observation is in this raster: its values would all be missing. Is the coordinate system the right one?',
  raster_methode: 'Value', raster_methode_cellule: 'The observation’s cell (classes, land cover)',
  raster_methode_bilineaire: 'Bilinear interpolation (continuous values: elevation, climate)',
  raster_methode_moyenne: 'Mean of the cells within a radius', raster_methode_majorite: 'Majority class within a radius (classes)',
  raster_methode_aide: 'The cell holding the observation; the bilinear interpolation of the four nearest cells, from their centres; the mean, or the most frequent class, of the cells whose centre is within the radius (as terra::extract).',
  raster_rayon: 'Radius', raster_rayon_aide: 'In metres: the cells whose centre is within this distance of the observation (none: its own cell). For longitudes and latitudes too, in metres.',
  raster_nom: 'Name', raster_nom_bande: 'Name of band {n}', raster_renomme: '→ added as “{nom}” (name already taken)',
  raster_tuiles: 'These {n} files are the tiles of one raster: one variable per band, each observation takes the value of the tile that holds it',
  raster_tuiles_aide: 'The tiles of a DEM, side by side: the same bands, type, system and cells. Untick for one variable per file.',
  raster_tuiles_dedans: '{n} observation(s) in these tiles ({total} in all).',
  raster_aucun_fichier: 'Choose at least one readable file.', raster_rayon_manquant: '{nom}: give the circle’s radius, in metres.',
  raster_fait: 'Raster values added', raster_annuler: 'Edit › Undo (Ctrl+Z) removes them. They are variables like the others: saved with the project, and the Methods text of a model that uses them says where they come from.',
});
