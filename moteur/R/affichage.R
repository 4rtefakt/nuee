# Ce que R affiche d'un modèle ajusté (print), dans la console de l'application (M4, m) et
# dans son journal après chaque ajustement : sa description, le tableau des composantes, ce qui
# concerne chaque Y (erreurs, CV-ANOVA, classement), les limites, puis les variables et les
# observations, les premières lignes seulement quand elles sont nombreuses (spectres) : tout
# est dans m$statistiques.
print.modele_nuee <- function(x, lignes = 25, ...) {
  s <- x$statistiques
  type <- paste0(if (isTRUE(x$opls)) "O" else "", if (isTRUE(x$plsda)) "PLS-DA" else if (identical(x$type, "PCA")) traduire("ACP", "PCA") else x$type)
  ny <- length(x$y$noms %||% character(0))
  cat(sprintf(traduire("Modèle %s (%s)%s\n", "Model %s (%s)%s\n"), x$nom %||% "", type,
              if (nzchar(x$titre %||% "")) paste(" :", x$titre) else ""))
  cat(sprintf(traduire("%d observations, %d termes X%s, %d composante(s)%s, validation croisée en %d groupes\n",
                       "%d observations, %d X terms%s, %d component(s)%s, cross-validation in %d groups\n"),
              x$N, x$K, if (ny) sprintf(traduire(", %d Y", ", %d Y"), ny) else "", x$A,
              if (isTRUE(x$opls)) sprintf(traduire(" (%d prédictive(s), %d orthogonale(s))", " (%d predictive, %d orthogonal)"), x$Ap %||% 1, x$A - (x$Ap %||% 1)) else "",
              x$G %||% 7))

  section <- function(titre) cat("\n", titre, "\n", sep = "")
  tableau <- function(df, n = nrow(df)) {
    print(utils::head(df, n), digits = 4, row.names = FALSE)
    if (nrow(df) > n) cat(sprintf(traduire("… et %d lignes de plus\n", "… and %d more rows\n"), nrow(df) - n))
  }

  section(traduire("Composantes", "Components"))
  tableau(s$composantes)

  if (ny) {
    section("Y")
    y <- data.frame(Y = x$y$noms, RMSEE = unname(s$RMSEE %||% NA), RMSEcv = unname(s$RMSEcv %||% NA), check.names = FALSE)
    if (length(s$cv_anova)) y[["CV-ANOVA p"]] <- vapply(x$y$noms, function(n) as.numeric(s$cv_anova[[n]]$p[2] %||% NA), 0)
    tableau(y)
  }
  if (!is.null(s$classification)) {
    section(traduire("Classement (part bien classée)", "Classification (share correctly classified)"))
    cl <- s$classification
    tableau(data.frame(classe = cl$ajuste$classes, ajuste = unname(cl$ajuste$correct), validation_croisee = unname(cl$vc$correct)))
    cat(sprintf(traduire("Ensemble : %.1f %% (ajusté), %.1f %% (validation croisée)\n", "Overall: %.1f %% (fitted), %.1f %% (cross-validation)\n"),
                100 * cl$ajuste$global, 100 * cl$vc$global))
  }
  if (!is.null(x$test)) {
    section(sprintf(traduire("Jeu de test : %d observations", "Test set: %d observations"), x$test$n %||% NA))
    st <- x$test$statistiques
    if (!is.null(st)) tableau(as.data.frame(st[intersect(c("y", "RMSEP", "biais", "SEP", "Q2ext"), names(st))], check.names = FALSE))
  }

  section(traduire("Limites", "Limits"))
  cat(paste(sprintf("%s %s", names(s$limites), format(unlist(s$limites), digits = 4)), collapse = " · "), "\n")
  if (!is.null(s$seuil_vip)) {
    cat(sprintf(traduire("VIP : seuil %s, %d variable(s) au-dessus : %s\n", "VIP: cut-off %s, %d variable(s) above it: %s\n"),
                format(s$seuil_vip$seuil, digits = 3), length(s$seuil_vip$retenues), paste(s$seuil_vip$retenues, collapse = ", ")))
  }

  section(traduire("Variables (m$statistiques$variables)", "Variables (m$statistiques$variables)"))
  v <- s$variables
  tableau(v[, intersect(c("terme", grep("^p\\d+$", names(v), value = TRUE), "VIP", "R2V_cum", "Q2V_cum"), names(v)), drop = FALSE], lignes)
  section(traduire("Observations (m$statistiques$observations)", "Observations (m$statistiques$observations)"))
  o <- s$observations
  o <- cbind(observation = as.character(x$observations), o[, intersect(c(grep("^t\\d+$", names(o), value = TRUE), "T2", "DModX",
                                                                          grep(" (observé|ajusté)$", names(o), value = TRUE)), names(o)), drop = FALSE])
  tableau(o, min(lignes, 10))
  invisible(x)
}
