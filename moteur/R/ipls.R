# iPLS, PLS par intervalles (Nørgaard et al. 2000) : quelles zones d'un spectre prédisent le
# mieux Y. Le spectre (les variables X quantitatives du modèle `d`, dans leur ordre) est
# prétraité une fois, en entier, comme pour le modèle, puis coupé en `n` intervalles de même
# largeur (à une variable près). Chaque intervalle est une PLS sur ses colonnes prétraitées,
# avec la mise à l'échelle du modèle et ses autres termes X, de 1 à `A` composantes. Une OPLS
# est ajustée en PLS (pour un Y, elle le prédit de la même façon au même nombre de composantes).
# Pour chaque intervalle : ses termes et leurs variables, son RMSEcv pour 1, 2... composantes
# et le nombre qui le rend le plus petit ; de même pour le spectre entier (`complet`, qui
# redonne le modèle).
pls_par_intervalles <- function(projet, d, n = 20, A = d$A) {
  if (identical(d$type, "PCA")) stop(traduire("L'iPLS demande un modèle avec Y (PLS, PLS-DA, OPLS).", "iPLS needs a model with Y (PLS, PLS-DA, OPLS)."))
  spectre <- which(vapply(d$x, function(t) length(t$variables) == 1 && categorie(t) == 0, TRUE))
  if (length(spectre) < 2) stop(traduire("L'iPLS demande des variables X quantitatives (un spectre).", "iPLS needs quantitative X variables (a spectrum)."))
  autres <- setdiff(seq_along(d$x), spectre)
  n <- max(1L, min(as.integer(n), length(spectre)))
  A <- max(1L, as.integer(A))

  # le projet où le spectre des observations du modèle est déjà prétraité (transformation
  # comprise) : ses termes n'ont plus ni transformation ni prétraitement
  s <- spectres_modele(projet, d)
  pp <- projet
  lignes <- match(as.character(d$observations), rownames(pp$donnees))
  for (k in seq_along(spectre)) {
    pp$donnees[lignes, as.character(d$x[[spectre[k]]]$variables[1])] <- s$pretraites[, k]
    d$x[[spectre[k]]]$transform <- NULL
  }
  d$spectres <- NULL
  d$test <- NULL
  d$opls <- FALSE
  d$Ap <- NULL

  # (une zone trop pauvre pour A composantes en reçoit moins ; NA si même une ne s'ajuste pas)
  zone <- function(termes) {
    dz <- d
    dz$x <- d$x[sort(termes)]
    for (a in rev(seq_len(min(A, length(termes))))) {
      dz$A <- a
      m <- tryCatch(ajuster(pp, dz), error = function(e) NULL)
      if (!is.null(m)) return(rmsecv_composantes(m))
    }
    NA_real_
  }
  meilleur <- function(r) if (all(is.na(r))) NA else which.min(r)
  coupes <- split(spectre, cut(seq_along(spectre), n, labels = FALSE))
  noms <- vapply(d$x, function(t) as.character(t$name), "")
  intervalles <- lapply(coupes, function(k) {
    r <- zone(c(k, autres))
    list(termes = I(noms[k]), variables = I(vapply(d$x[k], function(t) as.numeric(t$variables[1]), 0)), rmsecv = I(r), A = meilleur(r))
  })
  complet <- zone(seq_along(d$x))
  list(intervalles = unname(intervalles), complet = list(rmsecv = I(complet), A = meilleur(complet)), A = A,
       spectre = I(noms[spectre]), moyen = I(unname(colMeans(s$pretraites, na.rm = TRUE))), autres = I(noms[autres]))
}

# RMSEcv du modèle `m` pour 1, 2... m$A composantes, en moyenne sur ses Y : la même formule
# que celui du résumé (statistiques.R), qu'il redonne pour m$A composantes.
rmsecv_composantes <- function(m) {
  cumuls <- Reduce(`+`, lapply(m$vc, function(v) v$prediction), accumulate = TRUE)
  vapply(cumuls, function(pred) mean(vapply(seq_len(ncol(m$Y)), function(j)
    sqrt(mean(ifelse(m$MY[, j] == 1, (m$Y[, j] - pred[, j]) / m$y$poids[j], NA)^2, na.rm = TRUE)), 0)), 0)
}
