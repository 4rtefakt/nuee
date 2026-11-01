// What the interface can ask of the desktop app (window.bureau); absent in a browser.
const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('bureau', {
  versions: { electron: process.versions.electron },
  choisirFichier: () => ipcRenderer.invoke('choisir-fichier'),
  enregistrerSous: (nom, filtres) => ipcRenderer.invoke('enregistrer-sous', nom, filtres),
  recents: () => ipcRenderer.invoke('recents'),
  ajouterRecent: (chemin) => ipcRenderer.invoke('ajouter-recent', chemin),
  fenetres: () => ipcRenderer.invoke('fenetres'),
  focusFenetre: (id) => ipcRenderer.invoke('focus-fenetre', id),
  zoom: (d) => ipcRenderer.invoke('zoom', d),
  quitter: () => ipcRenderer.invoke('quitter'),
  ecrireFichier: (chemin, octets) => ipcRenderer.invoke('ecrire-fichier', chemin, octets),
  imprimerPDF: (html, chemin) => ipcRenderer.invoke('imprimer-pdf', html, chemin),
  ouvrirChemin: (chemin) => ipcRenderer.invoke('ouvrir-chemin', chemin),
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
});
