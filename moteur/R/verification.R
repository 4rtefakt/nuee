# Vérification du moteur : les modèles enregistrés dans un projet .usp sont recalculés et
# leurs R2X, R2Y et Q2 cumulés comparés aux valeurs enregistrées dans le fichier.
# Sert à contrôler le moteur après une correction (console : verifier_moteur()).
#
# Chaque modèle est validé comme son fichier, avec les centres et les poids du modèle complet
# (validation_croisee.R : mode_validation), quelle que soit sa façon de valider dans le projet. Statut d'un modèle,
# d'après le plus grand écart entre ses R2X, R2Y et Q2 cumulés et ceux du fichier : « identique »
# (moins de 0,0001), « proche » (moins de 0,01), « différent » sinon. Avec des données manquantes,
# quelques PLS s'écartent un peu ; le Q2 d'une ACP aussi (0,02 en médiane : le détail de la
# validation des fichiers n'est pas publié), et le sien n'est que dans la colonne Q2.
comparer_references <- function(projet) {
  lignes <- list()
  for (d in projet$models) {
    if (isTRUE(d$app) || !isTRUE(d$fitted) || !isTRUE(d$A > 0) || !is.null(d$error)) next
    ref <- d$reference$components
    if (!length(ref)) next
    val <- function(cle) vapply(ref, function(x) as.numeric(x[[cle]] %||% NA), 0)
    type <- if (isTRUE(d$plsda)) "PLS-DA" else d$type
    d$validation <- "complet"
    m <- tryCatch(ajuster(projet, d), error = function(e) conditionMessage(e))
    if (is.character(m)) {
      lignes[[length(lignes) + 1]] <- data.frame(modele = d$name, type = type, A = d$A, R2X = NA, R2Y = NA, Q2 = NA,
                                                 statut = paste("erreur :", m))
      next
    }
    comp <- resume_composantes(m)
    ecart <- function(a, b) {
      b <- b[seq_along(a)]
      if (all(is.na(b))) NA else max(abs(a - b), na.rm = TRUE)
    }
    r2x <- ecart(comp$R2Xcum, val("R2X(cum)"))
    r2y <- if (!is.null(comp$R2Ycum)) ecart(comp$R2Ycum, val("R2Y(cum)")) else NA
    q2 <- ecart(comp$Q2cum, val("Q2(cum)"))
    pire <- max(c(r2x, r2y, if (!identical(d$type, "PCA")) q2), na.rm = TRUE)
    statut <- if (pire < 1e-4) "identique" else if (pire < 0.01) "proche" else "différent"
    lignes[[length(lignes) + 1]] <- data.frame(modele = d$name, type = type, A = d$A, R2X = signif(r2x, 2),
                                               R2Y = signif(r2y, 2), Q2 = signif(q2, 2), statut = statut)
  }
  if (length(lignes)) do.call(rbind, lignes) else data.frame()
}
