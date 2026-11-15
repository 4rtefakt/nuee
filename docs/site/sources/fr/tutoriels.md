<!-- description: Trois tutoriels de Nuée : un avant/après avec témoins (BACI), une PLS sur des spectres proche infrarouge, et le suivi d'une population d'oiseaux (TRIM et échantillonnage par distances). -->
# Tutoriels

Trois études suivies du début à la fin : la question, les données, les fenêtres de Nuée l'une
après l'autre, la lecture des résultats, et le texte des méthodes. Chacune a son tableau à
télécharger.

<ul class="cartes">
<li><h3><a href="page:tutoriels.baci">Avant/après avec témoins (BACI)</a></h3><p>L'arasement d'un seuil a-t-il augmenté la densité de jeunes truites ? Six stations suivies 14 ans : l'écart impact − témoins, le modèle mixte BACI et ses conditions, l'interaction lue correctement, les moyennes ajustées, le texte des méthodes et l'export vers R.</p></li>
<li><h3><a href="page:tutoriels.pls">PLS sur des spectres</a></h3><p>Prédire le carbone organique de sols à partir de leur spectre proche infrarouge : le prétraitement (SNV, detrend), le nombre de composantes par validation croisée, les VIP, l'observé contre le prédit, le test de permutation, puis la prédiction de nouveaux échantillons.</p></li>
<li><h3><a href="page:tutoriels.suivi">Suivre une population</a></h3><p>Une alouette suivie sur 40 sites pendant 17 ans : de Quelle analyse ? aux indices et à la tendance de TRIM et sa classe, puis la densité par transects linéaires (échantillonnage par distances), et une figure pour le rapport.</p></li>
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

Les mêmes tableaux existent avec des noms de colonnes en anglais (voir les
[tutoriels en anglais](@/en/tutorials/index.html)). `Rscript baci.R` (et les deux autres) les
réécrit à l'identique, avec R 3.6 ou plus récent.
