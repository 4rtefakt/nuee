<p align="center"><img src="app/interface/nuee.svg" width="112" alt=""></p>

<h1 align="center">Nuée</h1>

<p align="center"><a href="README.md">English</a> · <b>Français</b></p>

<p align="center">
  <b>ACP, PLS, PLS-DA et OPLS sous Windows et Linux, calculées en R.</b><br>
  Des graphiques interactifs, des modèles côte à côte, et chaque calcul lisible.
</p>

<p align="center">
  <img alt="Windows 10 et 11" src="https://img.shields.io/badge/Windows-10%20%7C%2011-1b4a9c">
  <img alt="Linux" src="https://img.shields.io/badge/Linux-deb%20%7C%20AppImage-1b4a9c">
  <img alt="Interface en français et en anglais" src="https://img.shields.io/badge/interface-FR%20%7C%20EN-2f7dcd">
  <a href="LICENSE"><img alt="GPL-3.0" src="https://img.shields.io/badge/licence-GPL--3.0-1b4a9c"></a>
  <a href="https://github.com/4rtefakt/nuee/actions/workflows/verifications.yml"><img alt="Vérifications" src="https://github.com/4rtefakt/nuee/actions/workflows/verifications.yml/badge.svg"></a>
</p>

![Le projet d'exemple dans Nuée : une ACP avec son résumé, les scores colorés par variété et les loadings](docs/captures/acp.png)

Nuée est une application de bureau pour l'analyse multivariée. Ouvrez un tableau venant
d'Excel, de R, de SPSS, de Stata, de SAS ou de jamovi (ou collez-le), choisissez vos
variables, et obtenez le modèle avec ses scores, ses loadings, ses diagnostics et sa
validation, dans des graphiques où l'on zoome, sélectionne et colore. Renvoyez les données et
les résultats vers chacun de ces logiciels, prenez le paragraphe de votre section Méthodes, ou
exportez un script ou un rapport R Markdown / Quarto qui refait le modèle avec R seul, sans
Nuée. Chaque calcul est écrit en R, commenté, pour vérifier exactement ce qui a été calculé, et
le modifier.

L'interface est en français et en anglais (*Affichage › Langue*) ; les captures la montrent en
français.

## Installer

### Windows

1. Téléchargez `Nuee-Setup-<version>.exe` depuis la page [Releases](https://github.com/4rtefakt/nuee/releases).
2. Lancez-le et choisissez de l'installer pour vous seul (sans droits d'administrateur) ou pour
   tous les utilisateurs de l'ordinateur.
3. Windows peut dire qu'il a protégé votre ordinateur, car l'installateur n'est pas signé :
   cliquez sur *Informations complémentaires*, puis *Exécuter quand même*.

R est inclus : il n'y a rien d'autre à installer. Nuée apparaît dans le menu Démarrer, et se
désinstalle depuis *Paramètres › Applications*. Vos projets restent là où vous les avez
enregistrés.

Au démarrage, Nuée demande à GitHub s'il existe une nouvelle version (rien d'autre n'est
envoyé). S'il y en a une, un clic dans la barre d'état télécharge son programme d'installation,
vérifie son empreinte SHA-256 auprès de GitHub, propose d'enregistrer vos projets, l'installe
et redémarre Nuée. *Aide › Chercher une nouvelle version au démarrage* arrête la recherche.

**Pour les services informatiques :** installation silencieuse pour un utilisateur ou pour tout
l'ordinateur, une stratégie qui coupe la recherche de mises à jour pour tous, ce que Nuée écrit
où et envoie sur le réseau : [docs/deploiement.md](docs/deploiement.md) (en anglais).

### Linux

Sous Linux, Nuée utilise le R installé sur l'ordinateur (une version de R dépend des
bibliothèques de sa distribution : elle ne peut pas être emportée comme sous Windows).

- **Debian, Ubuntu et dérivées :** téléchargez `Nuee-<version>-amd64.deb` depuis la page
  [Releases](https://github.com/4rtefakt/nuee/releases) et installez-le avec
  `sudo apt install ./Nuee-<version>-amd64.deb`. apt installe aussi R et les paquets R dont
  Nuée a besoin. Nuée apparaît dans le menu des applications.
- **Autres distributions :** `Nuee-<version>-x86_64.AppImage` (le rendre exécutable, puis le
  lancer ; il demande FUSE 2, le paquet `libfuse2` dans la plupart des distributions) ou
  `Nuee-<version>-x64.tar.gz` (le décompresser, puis lancer `./nuee`). Installez
  d'abord R depuis votre distribution, puis les paquets, dans R :
  `install.packages(c("httpuv", "jsonlite", "openxlsx", "haven", "readxl", "foreign", "nlme", "mgcv", "MASS", "rpart", "cluster"))`.
  Sous Ubuntu 24.04 et après, préférez le `.deb` : il installe le profil AppArmor dont le bac
  à sable de Chromium a besoin là, et une AppImage doit être lancée avec `--no-sandbox`.

Chaque changement est vérifié sur GitHub avec le `.deb` installé sous Ubuntu 22.04 et 24.04
et Debian 12, avec leur propre R (4.1 à 4.3). Avec un R plus ancien que 4.4, comme ceux-ci, les
fichiers R (`.rds`, `.RData`) ne s'ouvrent pas, car en lire un pourrait exécuter du code
(CVE-2024-27322) : enregistrez le tableau en CSV ou en Excel.

Les projets `.usp` demandent `python3`. Nuée cherche `Rscript` dans le `PATH`, puis dans
`/usr/local/bin`, `/usr/bin` et `/opt/R/<version>/bin` ; lancez-le avec
`RSCRIPT=/chemin/vers/Rscript` pour un autre R. Les réglages sont dans `~/.config/Nuee`. Au
démarrage, Nuée demande à GitHub s'il existe une nouvelle version ; un clic dans la barre
d'état ouvre sa page.

## Premiers pas

- **Essayer l'exemple :** *Aide › Projet d'exemple* ouvre 120 pommes inventées (quatre
  variétés, bio ou conventionnelles, une douzaine de mesures et une note de dégustation) avec
  quatre modèles prêts : une ACP, une PLS de la note, une PLS-DA des variétés et une OPLS-DA du
  mode de culture.
- **Ouvrir vos données :** *Fichier › Ouvrir* (Ctrl+O) lit un tableau, une ligne par
  observation et les noms des observations dans la première colonne (voir [Fichiers](#fichiers)
  pour les formats), ou *Fichier › Coller un tableau* (Ctrl+V) prend des cellules copiées
  dans n'importe quel tableur, ou déposez des fichiers sur la fenêtre. Les colonnes de texte et
  les facteurs deviennent des variables qualitatives, pour colorer ou comme classes.
- **Faire un modèle :** *Modèle › Nouveau modèle* (Ctrl+N) : le type, le rôle de chaque variable
  (X, Y ou écartée), l'échelle et la transformation, les observations retenues. Le nombre de
  composantes est choisi par validation croisée, sauf si vous le fixez.
- **Explorer :** chaque modèle a sa page avec son résumé, ses scores et ses loadings ; ajoutez
  toute autre vue depuis le menu *Graphiques*. Sélectionnez des points avec un cadre ou un
  lasso : la sélection vous suit dans chaque graphique du modèle. Cliquez une variable (une
  barre des VIP, un coefficient, un point des loadings ou du S-plot) pour la mettre en évidence
  dans tous les graphiques du modèle.

Le guide complet est fourni avec l'application (*Aide › Guide d'utilisation › Guide complet*),
en français ([LISEZ-MOI-APPLICATION.txt](LISEZ-MOI-APPLICATION.txt)) et en anglais
([USER-GUIDE.txt](USER-GUIDE.txt)).

## Ce qu'elle fait

**Méthodes :** ACP, PLS, PLS-DA, et OPLS / OPLS-DA (plusieurs Y ou plus de deux classes donnent
plusieurs composantes prédictives), avec les valeurs manquantes traitées par NIPALS et une
validation croisée (7 groupes à tour de rôle, ou un groupe par valeur d'un identifiant ou d'une
variable, un site par exemple) où chaque groupe est prédit par un modèle ajusté sans lui,
centrage et mise à l'échelle compris (R2, Q2, et Q2 par variable). Pondération par blocs et
modificateurs, transformations personnalisées, termes croisés de deux ou trois variables. Modèles de classe : une ACP par
classe, et le graphique de Coomans pour dire à quelle classe appartient une observation, à
plusieurs, ou à aucune.

**Validation :** tests de permutation, CV-ANOVA, tables de classement et courbes ROC de la
PLS-DA, intervalles de confiance par jack-knife. Un jeu de test laissé hors du modèle et de la
validation croisée (choisi à la main, au hasard ou par Kennard–Stone, par classe pour une
PLS-DA), puis prédit : RMSEP, biais, SEP et Q2 externe, ou son classement.

![Un modèle PLS : observé contre prédit, et les VIP avec un seuil placé automatiquement](docs/captures/pls.png)

**Variables :** VIP avec un seuil placé automatiquement (ou déplacé à la main), coefficients,
loadings, biplot, réajustement avec les seules variables retenues. Statistiques des variables
avec les transformations log proposées, histogrammes et carte des valeurs manquantes, que l'ACP
itérative régularisée peut remplir ; nouvelles variables par des formules R, avec les moyennes
et les écarts par groupe. Tests par variable entre deux classes (t de Welch ou de Student,
Mann–Whitney) ou entre toutes (ANOVA, Kruskal–Wallis), avec la correction de
Benjamini–Hochberg et un graphique volcan. Tests entre deux variables, choisis d'après leurs
types, leurs conditions vérifiées : khi² ou test exact de Fisher, test t, ANOVA ou
Kruskal–Wallis avec les comparaisons deux à deux, corrélation de Pearson ou de Spearman ; un
identifiant des observations (site, année...) peut faire les groupes. Tests appariés sur des
données en long (t apparié, rangs signés de Wilcoxon) : deux secteurs comparés sur les années
qu'ils ont tous deux, ou toutes les paires de secteurs avec la correction de Holm ; ou deux
mesures des mêmes lignes (avant, après). Une matrice des corrélations avec les valeurs p
corrigées.

**Modèles linéaires, généralisés et mixtes :** régression linéaire et modèles linéaires mixtes
(nlme), avec des effets aléatoires emboîtés (le site, puis le secteur dans le site) et une
autocorrélation en temps continu (CAR1) ; modèles linéaires généralisés et additifs (mgcv) pour
des comptages (Poisson, binomiale négative), la présence-absence, des valeurs positives et des
densités avec des zéros (Tweedie), avec des effets aléatoires, un effort en décalage et des
courbes lissées pour un effet qui monte puis descend ou un seuil (avec aussi le CAR1, par
mgcv::gamm, pour une réponse normale) ; les coefficients et leurs intervalles, R2 marginal et
conditionnel, les courbes d'effet, les seuils de chaque courbe (là où l'intervalle simultané de
sa pente exclut 0) et sa concurvité, les graphiques des résidus, la variation de l'AIC quand
chaque bloc de variables est retiré, et la comparaison par l'AIC (ΔAIC, poids d'Akaike) des
régressions de la même réponse, à côté du Q2 en validation croisée de tous les modèles. Une
formule R règle le modèle (R la lit sans jamais l'exécuter), et les régressions prédisent de
nouvelles observations et des fichiers, les extrapolations signalées.

**Ordination des communautés :** analyse de redondance (RDA) de réponses comme des abondances
d'espèces (transformées par Hellinger, corde ou log(x + 1)) par des variables explicatives, RDA
partielle avec des covariables, triplots en échelle 1 ou 2, R2 ajusté, tests par permutation
du modèle, de chaque axe et de chaque terme (à l'intérieur des sites au besoin), sélection pas
à pas et partition de la variance ; analyse des correspondances (CA) de tableaux d'effectifs,
avec contributions et cos². Les résultats sont ceux du paquet vegan.

**Arbres, analyse discriminante, groupes :** arbres de décision (rpart) pour une réponse
quantitative ou une classe, sur les valeurs brutes des variables pour que les seuils se lisent
dans leurs unités, élagués par une validation croisée par site et validés en prédisant chaque
site par un arbre refait sans lui, avec la stabilité de la première coupure ; analyse
discriminante linéaire (MASS::lda), classement en validation croisée et probabilités a
posteriori ; k-moyennes avec silhouettes et choix de k.

![Tests par variable sur les pommes de l'exemple : un graphique volcan de Golden contre Gala, et la table des tests](docs/captures/volcan.png)

**Observations :** scores en 2D et en 3D, T2 de Hotelling et DModX avec leurs limites,
graphiques de contributions qui expliquent pourquoi une observation se démarque,
classification hiérarchique sur les scores, classes faites d'une sélection.

![Une OPLS-DA : le S-plot montre les variables qui séparent les deux classes](docs/captures/oplsda.png)

![Modèles de classe : une ACP par variété de pomme ; le graphique de Coomans des distances à deux d'entre elles, la part de chaque variété que chaque modèle accepte, et les distances de chaque pomme](docs/captures/coomans.png)

**Spectres :** ouvrez des fichiers JCAMP-DX, SPC (Galactic, Thermo) ou texte, un spectre par
fichier ou plusieurs, avec un tableau d'informations sur les échantillons (teneurs, classes)
joint par le nom des fichiers. SNV, MSC, detrend, retrait de la ligne de base et lissage ou
dérivées de Savitzky–Golay avant le modèle. Loadings, coefficients, VIP ou les spectres
eux-mêmes en lignes selon la longueur d'onde ; faites glisser dessus pour laisser des zones de
longueurs d'onde hors d'un nouveau modèle, ou pour ne garder qu'elles. L'iPLS (PLS par
intervalles) ajuste chaque zone du spectre seule et montre celles qui prédisent le mieux.

![Une PLS sur des spectres inventés après SNV et une dérivée première : scores colorés par lot, et loadings p en lignes selon la longueur d'onde](docs/captures/spectres.png)

![Des spectres proche infrarouge inventés, importés de 24 fichiers JCAMP-DX avec leurs teneurs en sucre, après SNV, detrend et une dérivée première ; les deux bandes de l'eau sont marquées, pour les laisser hors d'un nouveau modèle](docs/captures/zones.png)

**Prédiction :** appliquez un modèle à de nouvelles observations venant d'un tableau, avec un
signal pour celles hors du domaine du modèle, et exportez les résultats.

**Votre travail :**
- enregistrer un projet (`.nuee`, qui ouvre Nuée d'un double-clic) garde ses modèles, la
  disposition de vos panneaux, les réglages des graphiques et les sélections ;
- une copie de secours toutes les deux minutes, proposée après une panne, et l'annulation des
  modifications des données ;
- des notes sur chaque modèle ;
- la modification des données et de nouvelles variables par des formules R ou des classes de
  valeurs ;
- plusieurs fenêtres, et des vues qui se détachent sur un second écran ;
- des graphiques mis en forme pour une publication (polices, palettes, taille) et exportés en
  PNG ou SVG ;
- Ctrl+C sur un tableau de résultats le copie, prêt à coller dans Excel ;
- *Affichage › Rechercher une commande* (Ctrl+K) : tapez une partie du nom d'une commande au lieu
  de chercher dans les menus ;
- un rapport du modèle en PDF ou Word ;
- le paragraphe d'une section Méthodes, écrit d'après le modèle en français ou en anglais, avec
  ses références (*Modèle › Texte pour la section Méthodes*).

## Fichiers

| | Ouvre | Exporte |
|---|---|---|
| Excel | `.xlsx`, `.xlsm`, `.xls` (feuille au choix) | `.xlsx` (une feuille par tableau) |
| Texte | `.csv`, `.txt`, `.tsv` : séparateur, virgule décimale et encodage reconnus | `.csv`, `.tsv` |
| R | `.rds`, `.RData` (tableau au choix ; facteurs et noms des lignes gardés) | `.rds`, `.RData` |
| SPSS | `.sav`, `.zsav`, `.por` (les étiquettes de valeurs deviennent des catégories) | `.sav` |
| Stata | `.dta` | `.dta` |
| SAS | `.sas7bdat`, `.xpt` | `.xpt` |
| jamovi | `.omv` | `.omv` |
| dBase, Weka | `.dbf`, `.arff` | `.dbf`, `.arff` |
| Spectres | `.jdx`, `.dx`, `.jcm` (JCAMP-DX), `.spc` (Galactic, Thermo), ou texte avec une colonne par spectre ; plusieurs fichiers à la fois avec *Fichier › Importer des spectres* | |
| Presse-papiers | des cellules copiées depuis Excel, LibreOffice, Google Sheets, jamovi… | |

*Fichier › Exporter les données* écrit le tableau du projet ; *Fichier › Exporter les résultats
du modèle* (Ctrl+E) écrit les observations (scores, T2, DModX, valeurs ajustées), les variables
(loadings, VIP), les composantes et les coefficients. Pour SPSS, Stata et SAS, les noms des
variables sont rendus valides et le nom d'origine devient l'étiquette de la variable. Les
fichiers SAS sont au format de transport version 5, que SAS lit avec `libname xport` ; les
fichiers dBase sont en Windows-1252, comme l'attendent LibreOffice et les logiciels de SIG. Les
fichiers exportés ont été ouverts dans jamovi, PSPP (pour SPSS), Weka, LibreOffice et pandas
(pour Stata et SAS). Les lecteurs de spectres ont été vérifiés sur les fichiers d'essai
officiels de JCAMP-DX (toutes les écritures compressées) et sur les fichiers d'exemple SPC
habituels.

## R de bout en bout

Le moteur est du R dans [`moteur/R/`](moteur/R), commenté en français : prétraitement, NIPALS,
validation croisée, statistiques, prédiction. Aucune statistique n'est calculée ailleurs.

- **Console R** (*Affichage › Console R*, Ctrl+J) : le code R derrière chaque action, et votre
  propre code exécuté dans la session de l'application, avec `m` et `p` pour le modèle et le
  projet affichés.
- **Exporter vers R** (*Fichier › Exporter vers R*) : tout modèle devient un script, ou un
  rapport R Markdown ou Quarto, qui le refait sans Nuée, en R seul, sans paquet (une régression :
  `lm`, ou `nlme` et `mgcv`, livrés avec R). Il contient les
  données (ou les lit dans un CSV à côté) et le calcul écrit pour ce modèle seul, commenté : sa
  mise à l'échelle, l'algorithme NIPALS, la validation croisée, puis les statistiques, tableaux
  et graphiques (une PLS fait environ 170 lignes de code). Le rapport ajoute le texte des
  méthodes, des tableaux, des graphiques et les références, et se compile dans RStudio (*Knit*)
  ou avec Quarto. En plus, au choix : un `renv.lock`, les données
  et le modèle ajusté en `.RData`, et un projet RStudio. Toute la séance peut aussi aller dans un
  seul script : les données une fois, puis chaque modèle. Avant d'écrire, Nuée refait les modèles
  du script et vérifie qu'ils donnent les mêmes résultats.
- **Le corriger soi-même :** modifiez un fichier du moteur, tapez `recharger_moteur()` dans la
  console, et réajustez.

![Thème sombre, avec la console R sous une ACP](docs/captures/console.png)

## Ouvrir des projets `.usp`

Nuée ouvre aussi les fichiers de projet `.usp` : leurs données, les modèles qu'ils contiennent
et les valeurs avec lesquelles ces modèles ont été enregistrés. Il recalcule chaque modèle en R
et marque chaque valeur du résumé comme identique, proche ou différente. Sur 944 modèles de 133
projets, R2X, R2Y, les scores, les loadings, T2, DModX, les VIP et les coefficients sont ceux
enregistrés, à quelques modèles avec données manquantes près ; *Aide › Résultats et écarts
connus* liste les écarts connus. Le Q2 est celui de Nuée : sa validation croisée réajuste le
modèle de chaque groupe sans lui, mise à l'échelle comprise, ce que les valeurs enregistrées
ne faisaient pas. Les deux s'accordent pour de grands jeux de données (un écart médian de 0,003
pour les PLS de plus de 200 observations) et s'éloignent avec moins d'observations, où le Q2 de
Nuée est plus bas (de 0,11 en médiane de 20 à 50 observations, de 0,27 jusqu'à 20). Le RMSEcv,
la CV-ANOVA et le classement en validation croisée suivent.

## Signaler un problème

Ouvrez une [issue](https://github.com/4rtefakt/nuee/issues/new/choose), en français ou en anglais.
*Aide › Informations de diagnostic* donne les versions de Nuée, de R et de ses paquets à y
coller (ni chemins ni données).

## Citer Nuée

*Modèle › Texte pour la section Méthodes* cite Nuée et les méthodes pour vous. Sinon :

> 4rtefakt (2026). Nuée (version 0.9.4) [Computer software]. https://github.com/4rtefakt/nuee

Le bouton *Cite this repository* de GitHub la donne dans d'autres formats
([CITATION.cff](CITATION.cff)). Ce qui a changé à chaque version : [CHANGELOG.md](CHANGELOG.md)
(en anglais ; dans l'application, *Aide › Quoi de neuf*).

## Construire depuis les sources

Voir [docs/developpement.md](docs/developpement.md) (en anglais) : lancer depuis les sources,
la construction (installateur et version pour un partage réseau), les vérifications, et la
façon dont les résultats ont été comparés. Les vérifications tournent sur GitHub à chaque envoi
et chaque pull request.

## Licence

Nuée est un logiciel libre sous la [Licence publique générale GNU v3.0](LICENSE) ou
ultérieure.

L'installateur inclut R (GPL-2 | GPL-3) avec ses paquets recommandés et les paquets R httpuv,
jsonlite, openxlsx, haven, readxl et leurs dépendances, Electron (MIT), Plotly.js (MIT),
dockview (MIT) et les icônes Lucide (ISC), chacun sous sa propre licence.
