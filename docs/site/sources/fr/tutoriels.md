<!-- description: Six tutoriels de Nuée : un avant/après avec témoins (BACI), une PLS sur des spectres proche infrarouge, le suivi d'une population d'oiseaux (TRIM et échantillonnage par distances), l'écologie des communautés, les cartes et les modèles spatiaux, et un document de rapport. -->
# Tutoriels

Six études suivies du début à la fin : la question, les données, les fenêtres de Nuée l'une
après l'autre, la lecture des résultats, et le texte des méthodes. Chacune a son tableau à
télécharger.

<ul class="cartes">
<li><h3><a href="page:tutoriels.baci">Avant/après avec témoins (BACI)</a></h3><p>L'arasement d'un seuil a-t-il augmenté la densité de jeunes truites ? Six stations suivies 14 ans : l'écart impact − témoins, le modèle mixte BACI et ses conditions, l'interaction lue correctement, les moyennes ajustées, le texte des méthodes et l'export vers R.</p></li>
<li><h3><a href="page:tutoriels.pls">PLS sur des spectres</a></h3><p>Prédire le carbone organique de sols à partir de leur spectre proche infrarouge : le prétraitement (SNV, detrend), le nombre de composantes par validation croisée, les VIP, l'observé contre le prédit, le test de permutation, puis la prédiction de nouveaux échantillons.</p></li>
<li><h3><a href="page:tutoriels.suivi">Suivre une population</a></h3><p>Une alouette suivie sur 40 sites pendant 17 ans : de Quelle analyse ? aux indices et à la tendance de TRIM et sa classe, puis la densité par transects linéaires (échantillonnage par distances), et une figure pour le rapport.</p></li>
<li><h3><a href="page:tutoriels.communautes">Écologie des communautés</a></h3><p>Les carabes des bois, des haies et des cultures : la raréfaction et l'accumulation des espèces, un NMDS sur Bray-Curtis, une PERMANOVA et son test de dispersion, les espèces indicatrices, et ce qu'il faut écrire dans un article.</p></li>
<li><h3><a href="page:tutoriels.cartes">Cartes et modèles spatiaux</a></h3><p>La biomasse du foin sur un plateau de montagne : des coordonnées en Lambert-93 et la carte, l'altitude lue dans un raster, l'I de Moran des résidus d'un modèle, un GAM avec un lissage spatial, et un GeoPackage pour QGIS.</p></li>
<li><h3><a href="page:tutoriels.rapport">Un document de rapport</a></h3><p>L'étude des prairies mise en forme : des valeurs qui suivent les modèles, les textes des méthodes et des résultats, des citations d'un fichier BibTeX, une figure de deux panneaux à la taille d'une revue, Word et PDF, et un fichier Quarto reproductible.</p></li>
</ul>

## Avant de commencer

- [Installez Nuée](page:installation) : version {{version}} ou plus récente.
- Les données des tutoriels sont **simulées** : elles ont été fabriquées pour l'exercice par un
  script R, avec une graine fixe ; le script est à côté de chaque tableau. Aucun résultat n'est
  à citer comme une observation.
- Les captures d'écran viennent de Nuée {{version}} en français (les pages anglaises montrent
  l'interface en anglais). Elles ont été prises dans la version navigateur de Nuée : la fenêtre
  de l'application de bureau est la même, à quelques boutons près (*Télécharger* y devient
  *Choisir l'emplacement…*).
- L'interface est en français et en anglais (*Affichage › Langue*).

## Les données

| Tutoriel | Tableau | Script qui l'a fabriqué |
|---|---|---|
| BACI | [baci_truite.csv](@/donnees/baci_truite.csv) | [baci.R](@/donnees/baci.R) |
| PLS | [sols_spectres.csv](@/donnees/sols_spectres.csv), [sols_nouveaux.csv](@/donnees/sols_nouveaux.csv) | [spectres.R](@/donnees/spectres.R) |
| Suivi | [comptages_alouette.csv](@/donnees/comptages_alouette.csv), [transects_alouette.csv](@/donnees/transects_alouette.csv) | [suivi.R](@/donnees/suivi.R) |
| Communautés | [carabes.csv](@/donnees/carabes.csv) | [communautes.R](@/donnees/communautes.R) |
| Cartes | [prairies.csv](@/donnees/prairies.csv), [altitude.tif](@/donnees/altitude.tif) | [cartes.R](@/donnees/cartes.R) |
| Rapport | ceux du tutoriel des cartes, et [references.bib](@/donnees/references.bib) (quatre articles et livres publiés) | [cartes.R](@/donnees/cartes.R) |

Les mêmes tableaux existent avec des noms de colonnes en anglais (voir les
[tutoriels en anglais](@/en/tutorials/index.html)). `Rscript baci.R` (et les autres) les réécrit
à l'identique, avec R 3.6 ou plus récent.
