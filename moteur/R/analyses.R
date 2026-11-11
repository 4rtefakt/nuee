# Les modèles qui ne sont pas faits de composantes NIPALS (ACP, PLS, OPLS) : les régressions
# (regression.R) et les autres analyses, chacune dans son fichier : analyse de redondance (RDA)
# et analyse des correspondances (CA), analyse discriminante linéaire (LDA), arbre de décision
# (TREE), k-moyennes (KMEANS). Leur type est celui de definir_modele() ; leurs réglages propres
# sont dans d$options.
#
# Chaque analyse suit la même convention de noms, pour que le moteur et le serveur la trouvent
# sans liste à tenir : ajuster_<type>(projet, d) l'ajuste (modele.R : ajuster), et
# resultat_<type>(id) donne ce que l'interface en reçoit (serveur.R : resultat_modele), <type>
# en minuscules (ajuster_rda, resultat_kmeans...). Les demandes qui supposent des composantes
# et leurs scores (serveur.R : SANS_REGRESSION) leur sont refusées, comme aux régressions.
AUTRES_ANALYSES <- c("RDA", "CA", "LDA", "TREE", "KMEANS")

est_autre_analyse <- function(d) isTRUE(as.character(d$type %||% "") %in% AUTRES_ANALYSES)

# Un modèle sans composantes NIPALS : une régression ou une autre analyse.
sans_composantes <- function(d) est_regression(d) || est_autre_analyse(d)

ajuster_analyse <- function(projet, d) {
  f <- get0(paste0("ajuster_", tolower(as.character(d$type))), mode = "function")
  if (is.null(f)) stop(sprintf(traduire("Analyse inconnue : %s", "Unknown analysis: %s"), d$type), call. = FALSE)
  f(projet, d)
}
