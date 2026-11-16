// What the interface can ask of the desktop app (window.bureau); absent in a browser.
const { contextBridge, ipcRenderer, webUtils } = require('electron');

// a project's versions (versions.js, through main.js): an error thrown again here, its message in
// the interface's language
const version = (nom, ...args) => ipcRenderer.invoke(`versions-${nom}`, ...args).then((r) => {
  if ('erreur' in r) throw new Error(r.erreur);
  return r.valeur;
});

contextBridge.exposeInMainWorld('bureau', {
  versions: {
    electron: process.versions.electron,
    // the versions of projects, kept in the background with git and shared with GitHub or GitLab;
    // each given a project file's absolute path unless said
    etat: (chemin) => version('etat', chemin),
    ouvert: (chemin, options) => version('ouvert', chemin, options || {}),
    apresEnregistrement: (chemin, options) => version('apresEnregistrement', chemin, options || {}),
    aLaFermeture: (chemin) => version('aLaFermeture', chemin),
    marquer: (chemin, description) => version('marquer', chemin, description),
    liste: (chemin) => version('liste', chemin),
    nommer: (chemin, oid, nom) => version('nommer', chemin, oid, nom),
    retirerNom: (chemin, nom) => version('retirerNom', chemin, nom),
    lire: (chemin, oid) => version('lire', chemin, oid),
    restaurer: (chemin, oid) => version('restaurer', chemin, oid),
    activer: (chemin, oui) => version('activer', chemin, oui),
    reglages: () => version('reglages'),
    regler: (r) => version('regler', r || {}),
    partager: (chemin, url) => version('partager', chemin, url),
    jeton: (hote, jeton) => version('jeton', hote, jeton),
    verifierPartage: (chemin) => version('verifierPartage', chemin),
    envoyer: (chemin) => version('envoyer', chemin),
    recevoir: (chemin) => version('recevoir', chemin),
    choisir: (chemin, cote, fichier, description) => version('choisir', chemin, cote, fichier || null, description || null),
    arreterPartage: (chemin) => version('arreterPartage', chemin),
    listerDistant: (url) => version('listerDistant', url),
    ouvrirDistant: (url, cheminDistant, dossier) => version('ouvrirDistant', url, cheminDistant, dossier),
    copierHistoire: (ancien, nouveau) => version('copierHistoire', ancien, nouveau),
    // what the background exchanges did: { type: 'recu' | 'conflit' | 'attente' | 'envoi' | 'envoye' | 'erreur', chemin, … }
    surVersions: (f) => ipcRenderer.on('versions', (_, e) => f(e)),
  },
  choisirFichier: (filtres) => ipcRenderer.invoke('choisir-fichier', filtres),
  choisirFichiers: (filtres) => ipcRenderer.invoke('choisir-fichiers', filtres),
  lirePressePapiers: () => ipcRenderer.invoke('lire-presse-papiers'),
  enregistrerSous: (nom, filtres) => ipcRenderer.invoke('enregistrer-sous', nom, filtres),
  recents: () => ipcRenderer.invoke('recents'),
  ajouterRecent: (chemin) => ipcRenderer.invoke('ajouter-recent', chemin),
  fenetres: () => ipcRenderer.invoke('fenetres'),
  focusFenetre: (id) => ipcRenderer.invoke('focus-fenetre', id),
  zoom: (d) => ipcRenderer.invoke('zoom', d),
  quitter: () => ipcRenderer.invoke('quitter'),
  ecrireFichier: (chemin, octets) => ipcRenderer.invoke('ecrire-fichier', chemin, octets),
  // (format: { largeur, hauteur } in mm, a figure's page; otherwise A4)
  imprimerPDF: (html, chemin, format) => ipcRenderer.invoke('imprimer-pdf', html, chemin, format),
  ouvrirChemin: (chemin) => ipcRenderer.invoke('ouvrir-chemin', chemin),
  // a document compiled by Quarto, when the computer has it (documents.js); options.R: the R that
  // runs a reproducible .qmd's chunks
  quartoDisponible: () => ipcRenderer.invoke('quarto-disponible'),
  quarto: (chemin, format, options) => ipcRenderer.invoke('quarto', chemin, format, options || {}),
  // Zotero on this computer, read only (zotero.js)
  zotero: (chemin, delai) => ipcRenderer.invoke('zotero', chemin, delai),
  // closing with "Save and close": the window saves its projects, then answers
  surEnregistrerTout: (f) => ipcRenderer.on('enregistrer-tout', async () => {
    let ok = false;
    try { ok = await f(); } catch { ok = false; }
    ipcRenderer.send('enregistrement-fini', ok);
  }),
  secoursDossier: () => ipcRenderer.invoke('secours-dossier'),
  secoursListe: () => ipcRenderer.invoke('secours-liste'),
  secoursIndex: (entrees) => ipcRenderer.invoke('secours-index', entrees),
  surMoteurArrete: (f) => ipcRenderer.on('moteur-arrete', (_, journal) => f(journal)),
  redemarrerMoteur: () => ipcRenderer.invoke('redemarrer-moteur'),
  nouvelleVersion: (maintenant) => ipcRenderer.invoke('nouvelle-version', maintenant),
  diagnostic: () => ipcRenderer.invoke('diagnostic'),
  majAuto: (oui) => ipcRenderer.invoke('maj-auto', oui),
  mettreAJour: () => ipcRenderer.invoke('mettre-a-jour'),
  surProgressionMaj: (f) => ipcRenderer.on('maj-progression', (_, x) => f(x)),
  langue: (l) => ipcRenderer.invoke('langue', l),
  ouvrirGuide: () => ipcRenderer.invoke('ouvrir-guide'),
  // a file dropped on the window: its path; a file Windows asks Nuée to open (double-click)
  cheminFichier: (f) => webUtils.getPathForFile(f),
  surOuvrirFichier: (f) => ipcRenderer.on('ouvrir-fichier', (_, chemin) => f(chemin)),
});
