// What changed in each version, shown once after an update (and in Aide › Quoi de neuf).
// The first entry is the current version.
export const NOUVEAUTES = [
  { version: '0.6.0', date: '2026-11-01',
    fr: [
      'Aide › Projet d’exemple : des données inventées (120 pommes) avec quatre modèles prêts à explorer, pour essayer Nuée.',
      'Nouvelle icône, plus lisible en petit ; le logo sur l’écran d’accueil et dans À propos.',
      'Une application plus légère (65 Mo et un millier de fichiers de moins) : elle se copie et s’installe plus vite.',
      'Nuée est maintenant un logiciel libre (licence GPL-3.0) : github.com/4rtefakt/nuee.',
    ],
    en: [
      'Help › Example project: made-up data (120 apples) with four models ready to explore, to try Nuée.',
      'New icon, easier to read at small sizes; the logo on the welcome screen and in About.',
      'A lighter application (65 MB and a thousand files less): it copies and installs faster.',
      'Nuée is now free software (GPL-3.0 licence): github.com/4rtefakt/nuee.',
    ] },
  { version: '0.5.0', date: '2026-11-01',
    fr: [
      'L’application s’appelle maintenant Nuée. Réglages, préférences et projets récents sont repris de la version précédente.',
      'Nuée apparaît dans le menu Démarrer et dans Paramètres › Applications installées, d’où on peut la désinstaller.',
      'Une nouvelle icône ; le raccourci du Bureau s’appelle « Nuée ».',
      'Aide › Résultats et écarts connus : ce que le moteur reproduit à l’identique des fichiers .usp, et les écarts connus.',
    ],
    en: [
      'The application is now called Nuée. Settings, preferences and recent projects carry over from the previous version.',
      'Nuée shows in the Start menu and in Settings › Installed apps, where it can be uninstalled.',
      'A new icon; the desktop shortcut is called “Nuée”.',
      'Help › Results and known differences: what the engine reproduces exactly from .usp files, and the known differences.',
    ] },
  { version: '0.4.0', date: '2026-11-01',
    fr: [
      'En fermant l’application avec des changements non enregistrés : « Enregistrer et fermer ».',
      'Copie de secours automatique des projets non enregistrés : après un arrêt imprévu, l’application propose de les récupérer.',
      'Si le moteur R s’arrête, l’application le signale et propose de le redémarrer (aussi dans Aide › Redémarrer le moteur R).',
      'Édition › Annuler (Ctrl+Z) : revenir sur une modification des données, une nouvelle variable, une classe…',
      'Retirer un modèle demande confirmation ; un modèle fait dans l’application peut aussi quitter le projet.',
      'Notes : chaque modèle a une zone de notes dans son résumé, enregistrée avec le projet et reprise dans le rapport.',
      'Console R : verifier_moteur() recalcule les modèles importés et les compare aux valeurs enregistrées.',
      'Les réglages (thème, langue, mise en forme des graphiques, commandes de la console) sont gardés d’une fois sur l’autre.',
    ],
    en: [
      'Closing with unsaved changes now offers “Save and close”.',
      'Automatic recovery copies of unsaved projects: after an unexpected stop, the application offers to recover them.',
      'If the R engine stops, the application says so and offers to restart it (also in Help › Restart the R engine).',
      'Edit › Undo (Ctrl+Z): reverse a data change, a new variable, a class…',
      'Removing a model asks for confirmation; a model made in the application can also leave the project.',
      'Notes: each model has a notes box in its summary, saved with the project and included in the report.',
      'R console: verifier_moteur() refits the imported models and compares them with the stored values.',
      'Settings (theme, language, plot format, console commands) are kept from one launch to the next.',
    ] },
  { version: '0.3.0', date: '2026-10-31',
    fr: [
      'Une page par modèle : un onglet au centre pour chaque modèle, avec son résumé, ses scores et ses loadings.',
      'Console R (Affichage › Console R, Ctrl+J) : le code R de chaque action, et une invite R.',
      'Le projet enregistré garde la séance : modèles, panneaux, réglages des graphiques, sélections.',
      'Statistiques des variables, transformations proposées, histogrammes, carte des valeurs manquantes.',
      'OPLS et OPLS-DA ; biplot, scores en 3D, S-plot ; DModXPS des prédictions.',
      'Rapport du modèle en PDF ou Word ; mise en forme des graphiques pour un article.',
      'Modifier les données, créer des variables (formule R, classes) ; historique de chaque projet .usp.',
      'Prédire un fichier, modèles hiérarchiques, export en script R, contributions, vérification des données avant d’ajuster.',
    ],
    en: [
      'One page per model: a central tab for each model, with its summary, scores and loadings.',
      'R console (View › R console, Ctrl+J): the R code of each action, and an R prompt.',
      'Saved projects keep the session: models, panels, plot settings, selections.',
      'Variable statistics, proposed transformations, histograms, missing-value map.',
      'OPLS and OPLS-DA; biplot, 3D scores, S-plot; DModXPS for predictions.',
      'Model report as PDF or Word; plot formatting for publication.',
      'Edit data, new variables (R formula, classes); history of each .usp project.',
      'Predict a file, hierarchical models, R script export, contributions, data checks before fitting.',
    ] },
];

export const VERSION = NOUVEAUTES[0].version;
