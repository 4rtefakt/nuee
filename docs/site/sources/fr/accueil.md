<!-- description: Nuée, une application de bureau pour les analyses des écologues : ACP, PLS, écologie des communautés, modèles mixtes, suivis et cartes, calculés en R, sous Windows, macOS et Linux. -->
# Nuée

<p class="chapeau">ACP, PLS, écologie des communautés, modèles linéaires, généralisés et mixtes, suivis de populations et cartes, sous Windows, macOS et Linux, calculés en R.</p>

<div class="boutons">
<a class="bouton" href="{{telechargement:windows}}">Télécharger pour Windows</a>
<a class="bouton second" href="page:installation#macos">macOS</a>
<a class="bouton second" href="page:installation#linux">Linux</a>
<a class="bouton second" href="page:tutoriels">Tutoriels</a>
</div>

Nuée (version {{version}}) est une application de bureau pour analyser des données de terrain et de
laboratoire. Ouvrez un tableau venant d'Excel, de R, de SPSS, de Stata, de SAS, de jamovi ou de
QGIS, ou les relevés de terrain de KoboToolbox ou d'ODK, choisissez vos variables, et obtenez le
modèle avec ses graphiques, ses diagnostics et sa validation. Les résultats repartent vers Excel,
R, SPSS, Stata, SAS, jamovi ou QGIS, avec le paragraphe de votre section Méthodes, ou en script R
qui refait le calcul sans Nuée.

![La fenêtre de Nuée : une PLS sur des spectres, son résumé, ses scores et ses loadings tracés en fonction de la longueur d'onde](@/images/fr/accueil-fenetre.png "Une PLS sur des spectres proche infrarouge (tutoriel 2) : l'explorateur à gauche, le résumé du modèle, ses scores et ses loadings.")

## Pour qui

Pour les écologues de terrain, les gestionnaires d'espaces naturels et les chercheurs en
environnement, statisticiens ou non. Nuée vous aide à choisir :

- **Quelle analyse ?** (menu Modèle) pose votre question en mots simples : comparer des groupes,
  l'effet de variables, beaucoup d'espèces, une tendance, une densité, un avant/après avec
  témoins… Il propose la réponse et le dispositif d'après vos colonnes, conseille une analyse et
  ouvre la fenêtre du modèle déjà remplie.
- Le résumé de chaque régression dit **ses conditions en mots** : la normalité des résidus, une
  dispersion qui change, des observations influentes, une surdispersion, des termes colinéaires,
  avec ce qu'il faut faire.
- **Le texte des méthodes** et **le texte des résultats** sont écrits d'après le modèle, en
  français ou en anglais, avec leurs références.

## Ce que fait Nuée

<ul class="cartes">
<li><h3>Multivarié</h3><p>ACP, PLS, PLS-DA, OPLS, spectres proche infrarouge (SNV, MSC, dérivées), VIP, validation croisée, tests de permutation.</p><p><a href="page:guide.acp-pls">Le guide</a></p></li>
<li><h3>Régressions</h3><p>Modèles linéaires, mixtes (nlme), GLM et GAM (mgcv) pour comptages, présences, proportions, avec autocorrélation dans le temps, moyennes ajustées et sélection par AIC.</p><p><a href="page:guide.regressions">Le guide</a></p></li>
<li><h3>Communautés</h3><p>RDA, CA, CCA, PCoA, NMDS, PERMANOVA, test de Mantel, espèces indicatrices, raréfaction.</p><p><a href="page:guide.communautes">Le guide</a></p></li>
<li><h3>Suivis</h3><p>Tendances de séries, avant/après avec témoins (BACI), indices de comptages (TRIM), densités par échantillonnage par distances.</p><p><a href="page:guide.suivis">Le guide</a></p></li>
<li><h3>Cartes</h3><p>Coordonnées et systèmes (Lambert-93, UTM…), GeoPackage pour QGIS, valeurs de rasters, autocorrélation spatiale.</p><p><a href="page:guide.cartes">Le guide</a></p></li>
<li><h3>Publication</h3><p>Figures composées à la taille d'une revue, rapports PDF et Word, documents avec citations, export vers R, R Markdown et Quarto.</p><p><a href="page:guide.resultats">Le guide</a></p></li>
<li><h3>Rapports vivants</h3><p>Le fichier de données de l'année suivante lu dans le projet, ses changements rejoués, chaque modèle ajusté à nouveau, et les valeurs que citent vos documents mises à jour.</p><p><a href="page:guide.donnees#etapes-des-donnees-et-mise-a-jour">Le guide</a></p></li>
<li><h3>Versions</h3><p>Les versions de chaque projet gardées d'elles-mêmes, nommées, ouvertes, restaurées, et comparées : ce qui a changé dans les résultats de chaque modèle, en phrases.</p><p><a href="page:guide.versions">Le guide</a></p></li>
<li><h3>À plusieurs</h3><p>Un projet partagé sur GitHub ou GitLab, ou sur un dossier réseau, les changements des deux côtés réunis modèle par modèle, passage par passage.</p><p><a href="page:guide.versions#travailler-a-plusieurs">Le guide</a></p></li>
</ul>

Nouveau dans la version 1.1 : les rapports vivants, les versions et le travail à plusieurs, et
toute l'interface au clavier ou avec un lecteur d'écran. Tout ce qui a changé d'autre :
[Quoi de neuf](page:nouveautes).

## Tout en R, sur votre ordinateur

Chaque calcul est écrit en R, commenté, dans le dossier de l'application : on peut lire ce qui a
été calculé, et le modifier. La console R (Affichage › Console R) montre l'appel derrière chaque
action. Les résultats sont ceux des paquets de référence (vegan, nlme, mgcv, emmeans, rtrim,
Distance…), vérifiés à chaque changement.

Vos données restent sur votre ordinateur : Nuée n'envoie rien, sauf une question à GitHub au
démarrage pour savoir s'il existe une nouvelle version (elle se coupe dans le menu Aide), la zone
affichée aux serveurs d'OpenStreetMap ou de l'IGN quand vous choisissez un fond de carte, et les
versions d'un projet au dépôt GitHub ou GitLab où vous le partagez.

## Télécharger

Nuée {{version}} :

- **Windows 10 et 11** : [{{fichier:windows}}]({{telechargement:windows}}), R inclus ;
- **macOS** : [{{fichier:macarm64}}]({{telechargement:macarm64}}) pour Apple silicon (macOS 14 ou plus récent), [{{fichier:macx64}}]({{telechargement:macx64}}) pour Intel (macOS 13 ou plus récent), R inclus ;
- **Debian, Ubuntu et dérivées** : [{{fichier:deb}}]({{telechargement:deb}}) ;
- **autres distributions Linux** : [{{fichier:appimage}}]({{telechargement:appimage}}) ou [{{fichier:targz}}]({{telechargement:targz}}).

Comment installer : voir [Installation](page:installation). Toutes les versions et leurs notes :
la page des [publications sur GitHub]({{telechargement:toutes}}) ; ce qui a changé :
[Quoi de neuf](page:nouveautes).

## Pour commencer

- [Les tutoriels](page:tutoriels) suivent six études du début à la fin : un avant/après avec
  témoins (BACI), une PLS sur des spectres, le suivi d'une population d'oiseaux, les carabes de
  trois milieux (écologie des communautés), les cartes et les modèles spatiaux, et le rapport d'une
  étude.
- Dans l'application, Aide › Projet d'exemple ouvre 120 pommes inventées avec quatre modèles
  prêts.
- [Le guide d'utilisation](page:guide) décrit chaque fenêtre et chaque calcul.

Nuée est un logiciel libre ([GPL-3.0](https://github.com/4rtefakt/nuee/blob/main/LICENSE)).
L'interface est en français et en anglais (Affichage › Langue). Un problème, une question :
[ouvrez un ticket sur GitHub](https://github.com/4rtefakt/nuee/issues), en français ou en anglais ;
Aide › Informations de diagnostic donne les versions à y coller.
