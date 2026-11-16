// What the interface can ask of the desktop app (window.bureau); absent in a browser.
const { contextBridge, ipcRenderer, webUtils } = require('electron');

contextBridge.exposeInMainWorld('bureau', {
  versions: { electron: process.versions.electron },
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
  // a file dropped on the window: its path; a file Windows asks Nuée to open (double-click),
  // kept by main.js until asked for: also those that came before the page was ready
  cheminFichier: (f) => webUtils.getPathForFile(f),
  surOuvrirFichier: (f) => {
    const prendre = async () => { for (const chemin of await ipcRenderer.invoke('fichiers-a-ouvrir')) f(chemin); };
    ipcRenderer.on('ouvrir-fichier', prendre);
    prendre();
  },
});
