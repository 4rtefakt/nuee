# Prétraitement des spectres. Chaque observation est un spectre : ses variables X
# quantitatives, dans l'ordre du tableau (par exemple une absorbance par longueur d'onde).
# Chaque spectre est corrigé avant la mise à l'échelle des variables (construire_bloc), par
# des étapes appliquées dans l'ordre donné :
#   list(type = "snv")  SNV (Standard Normal Variate) : le spectre est centré sur sa moyenne
#                       et divisé par son écart-type ;
#   list(type = "msc")  MSC (Multiplicative Scatter Correction) : le spectre x est ajusté sur
#                       le spectre moyen r des observations du modèle, x = a + b r, puis
#                       remplacé par (x - a) / b ;
#   list(type = "sg", derivee = 1, fenetre = 11, degre = 2)  Savitzky–Golay : dans une fenêtre
#                       de `fenetre` variables centrée sur chaque variable, un polynôme de
#                       degré `degre` ajusté par moindres carrés ; la valeur corrigée est sa
#                       valeur (derivee = 0 : lissage) ou sa dérivée au centre, par pas d'une
#                       variable. Aux bords, le polynôme de la première ou de la dernière fenêtre.
# Une valeur manquante reste manquante ; avec Savitzky–Golay, elle rend aussi manquantes les
# valeurs dont la fenêtre la contient. Le prétraitement est fait une fois, sur toutes les
# observations du modèle, avant la validation croisée.

# Les étapes telles que l'interface les envoie (un data.frame, une ligne par étape) en liste.
etapes_spectres <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  if (is.data.frame(x)) x <- lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
  lapply(x, function(e) {
    e <- Filter(function(v) length(v) && !is.na(v[1]), as.list(e))
    e$type <- tolower(as.character(e$type))
    if (!e$type %in% c("snv", "msc", "sg")) stop("Prétraitement des spectres inconnu : ", e$type)
    if (e$type == "sg") {
      e$fenetre <- as.integer(e$fenetre %||% 11)
      e$degre <- as.integer(e$degre %||% 2)
      e$derivee <- as.integer(e$derivee %||% 0)
      if (e$fenetre < 3 || e$fenetre %% 2 == 0) stop("La fenêtre de Savitzky–Golay doit compter un nombre impair de variables, 3 au moins")
      if (e$degre < e$derivee || e$degre >= e$fenetre) stop("Savitzky–Golay : il faut dérivée ≤ degré < fenêtre")
    }
    e
  })
}

# Applique les étapes aux lignes de X. `reference` : les spectres moyens des étapes MSC d'un
# modèle déjà ajusté (pour prédire de nouvelles observations) ; renvoyés sinon.
pretraiter_spectres <- function(X, etapes, reference = NULL) {
  refs <- list()
  for (i in seq_along(etapes)) {
    e <- etapes[[i]]
    X <- switch(e$type,
      snv = snv(X),
      msc = {
        r <- if (length(reference) >= i && !is.null(reference[[i]])) reference[[i]] else colMeans(X, na.rm = TRUE)
        refs[[i]] <- r
        msc(X, r)
      },
      sg = savitzky_golay(X, e$fenetre, e$degre, e$derivee))
  }
  list(X = X, reference = refs)
}

snv <- function(X) {
  moyenne <- rowMeans(X, na.rm = TRUE)
  ecart <- apply(X, 1, stats::sd, na.rm = TRUE)
  ecart[!is.finite(ecart) | ecart == 0] <- NA
  (X - moyenne) / ecart
}

msc <- function(X, r) {
  lignes <- lapply(seq_len(nrow(X)), function(i) {
    x <- X[i, ]
    ok <- !is.na(x) & !is.na(r)
    if (sum(ok) < 2 || stats::var(r[ok]) == 0) return(rep(NA_real_, length(x)))
    b <- stats::cov(x[ok], r[ok]) / stats::var(r[ok])
    a <- mean(x[ok]) - b * mean(r[ok])
    (x - a) / b
  })
  matrix(unlist(lignes), nrow(X), ncol(X), byrow = TRUE, dimnames = dimnames(X))
}

# Coefficients de Savitzky–Golay : la ligne `derivee` de la matrice qui donne les coefficients
# du polynôme à partir des valeurs de la fenêtre, évaluée à la position u (0 = le centre).
coefficients_sg <- function(fenetre, degre, derivee, u = 0) {
  h <- (fenetre - 1) / 2
  V <- outer(-h:h, 0:degre, `^`)
  H <- solve(crossprod(V), t(V))  # (degre + 1) x fenetre
  d <- vapply(0:degre, function(k) if (k < derivee) 0 else factorial(k) / factorial(k - derivee) * u^(k - derivee), 0)
  drop(d %*% H)
}

savitzky_golay <- function(X, fenetre = 11, degre = 2, derivee = 0) {
  K <- ncol(X)
  if (fenetre > K) stop(sprintf("La fenêtre de Savitzky–Golay (%d variables) dépasse le spectre (%d variables X)", fenetre, K))
  h <- (fenetre - 1) / 2
  out <- matrix(NA_real_, nrow(X), K, dimnames = dimnames(X))
  centre <- coefficients_sg(fenetre, degre, derivee)
  for (j in (h + 1):(K - h)) out[, j] <- X[, (j - h):(j + h), drop = FALSE] %*% centre
  for (j in seq_len(h)) {
    out[, j] <- X[, 1:fenetre, drop = FALSE] %*% coefficients_sg(fenetre, degre, derivee, j - h - 1)
    out[, K - h + j] <- X[, (K - fenetre + 1):K, drop = FALSE] %*% coefficients_sg(fenetre, degre, derivee, j)
  }
  out
}

# Les spectres d'un modèle : les variables X quantitatives des observations du modèle, telles
# que lues (après leur transformation éventuelle) et après le prétraitement des spectres.
spectres_modele <- function(projet, definition) {
  quant <- which(vapply(definition$x, function(t) length(t$variables) == 1 && categorie(t) == 0, TRUE))
  obs <- definition$observations
  bruts <- vapply(definition$x[quant], function(t) transformer(valeurs_simples(projet, t, obs), t$transform), numeric(length(obs)))
  bruts <- matrix(bruts, length(obs), length(quant), dimnames = list(as.character(obs), vapply(definition$x[quant], function(t) as.character(t$name), "")))
  pretraites <- if (length(definition$spectres)) pretraiter_spectres(bruts, definition$spectres)$X else bruts
  list(bruts = bruts, pretraites = pretraites)
}
