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
#                       Chaque zone de variables voisines dans le tableau est traitée à part :
#                       une fenêtre ne passe pas par-dessus des variables laissées hors du modèle ;
#   list(type = "detrend", degre = 2)  detrend (Barnes et al., 1989) : un polynôme de degré
#                       `degre` en fonction de l'abscisse (terme constant compris), ajusté par
#                       moindres carrés sur le spectre, en est retiré (souvent après SNV) ;
#   list(type = "ligne_base")  ligne de base : l'enveloppe convexe inférieure du spectre (les
#                       segments qui joignent ses points les plus bas, d'un bout à l'autre) en est
#                       retirée ; le spectre corrigé est positif ou nul.
# L'abscisse d'une variable est le nombre lu dans son nom (« 1450 nm » : 1450) quand chaque
# variable en a un, différent ; sinon sa place dans le tableau.
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
    if (!e$type %in% c("snv", "msc", "sg", "detrend", "ligne_base")) stop(traduire("Prétraitement des spectres inconnu : ", "Unknown spectral preprocessing: "), e$type)
    if (e$type == "detrend") {
      e$degre <- as.integer(e$degre %||% 2)
      if (e$degre < 1) stop(traduire("Detrend : le degré du polynôme doit être 1 au moins", "Detrend: the polynomial's degree must be 1 or more"))
    }
    if (e$type == "sg") {
      e$fenetre <- as.integer(e$fenetre %||% 11)
      e$degre <- as.integer(e$degre %||% 2)
      e$derivee <- as.integer(e$derivee %||% 0)
      if (e$fenetre < 3 || e$fenetre %% 2 == 0) stop(traduire("La fenêtre de Savitzky–Golay doit compter un nombre impair de variables, 3 au moins", "The Savitzky–Golay window must be an odd number of variables, 3 or more"))
      if (e$degre < e$derivee || e$degre >= e$fenetre) stop(traduire("Savitzky–Golay : il faut dérivée ≤ degré < fenêtre", "Savitzky–Golay: derivative ≤ degree < window is needed"))
    }
    e
  })
}

# Applique les étapes aux lignes de X. `reference` : les spectres moyens des étapes MSC d'un
# modèle déjà ajusté (pour prédire de nouvelles observations) ; renvoyés sinon. `positions` :
# la place de chaque colonne dans le tableau du projet (des colonnes voisines y sont voisines).
pretraiter_spectres <- function(X, etapes, reference = NULL, positions = seq_len(ncol(X))) {
  refs <- list()
  x <- abscisses_spectre(colnames(X), positions)
  for (i in seq_along(etapes)) {
    e <- etapes[[i]]
    X <- switch(e$type,
      snv = snv(X),
      msc = {
        r <- if (length(reference) >= i && !is.null(reference[[i]])) reference[[i]] else colMeans(X, na.rm = TRUE)
        refs[[i]] <- r
        msc(X, r)
      },
      sg = {
        zones <- split(seq_len(ncol(X)), cumsum(c(1, diff(positions) != 1)))
        for (z in zones) {
          if (length(zones) > 1 && length(z) < e$fenetre) {
            stop(sprintf(traduire("Savitzky–Golay : la zone de %s à %s n'a que %d variables, moins que la fenêtre (%d)", "Savitzky–Golay: the range from %s to %s has only %d variables, fewer than the window (%d)"),
                         colnames(X)[z[1]], colnames(X)[z[length(z)]], length(z), e$fenetre))
          }
          X[, z] <- savitzky_golay(X[, z, drop = FALSE], e$fenetre, e$degre, e$derivee)
        }
        X
      },
      detrend = detrend(X, x, e$degre),
      ligne_base = ligne_base(X, x))
  }
  list(X = X, reference = refs)
}

# L'abscisse de chaque variable : le nombre de son nom quand toutes en ont un, différent ;
# sinon sa place dans le tableau.
abscisses_spectre <- function(noms, positions) {
  n <- vapply(as.character(noms %||% character(0)), function(s) {
    m <- regmatches(s, regexpr("-?[0-9]+([.,][0-9]+)?", s))
    if (length(m)) as.numeric(sub(",", ".", m, fixed = TRUE)) else NA_real_
  }, 0, USE.NAMES = FALSE)
  if (length(n) == length(positions) && !anyNA(n) && !anyDuplicated(n)) n else positions
}

# Résidus de chaque spectre après l'ajustement d'un polynôme de degré `degre` en x.
detrend <- function(X, x, degre = 2) {
  u <- (x - mean(x)) / stats::sd(x)
  V <- outer(u, 0:degre, `^`)
  complet <- qr(V)
  for (i in seq_len(nrow(X))) {
    ok <- !is.na(X[i, ])
    if (all(ok)) X[i, ] <- qr.resid(complet, X[i, ])
    else if (sum(ok) > degre) X[i, ok] <- qr.resid(qr(V[ok, , drop = FALSE]), X[i, ok])
    else X[i, ] <- NA
  }
  X
}

# La place dans le tableau du projet de la variable de chaque terme.
positions_termes <- function(projet, termes) {
  uids <- vapply(projet$variables, function(v) as.numeric(v$uid), 0)
  vapply(termes, function(t) as.numeric(match(as.numeric(t$variables[1]), uids)), 0)
}

# Chaque spectre moins son enveloppe convexe inférieure, interpolée linéairement.
ligne_base <- function(X, x) {
  o <- order(x)
  for (i in seq_len(nrow(X))) {
    ok <- o[!is.na(X[i, o])]
    if (length(ok) < 2) next
    h <- enveloppe_basse(x[ok], X[i, ok])
    X[i, ok] <- X[i, ok] - stats::approx(x[ok][h], X[i, ok][h], xout = x[ok])$y
  }
  X
}

# Les points de l'enveloppe convexe inférieure de (a, y), a croissant (chaîne monotone
# d'Andrew) : on garde un point tant que le chemin tourne à gauche.
enveloppe_basse <- function(a, y) {
  h <- integer(0)
  for (i in seq_along(a)) {
    while (length(h) >= 2) {
      j <- h[length(h) - 1]
      k <- h[length(h)]
      if ((a[k] - a[j]) * (y[i] - y[j]) - (y[k] - y[j]) * (a[i] - a[j]) > 0) break
      h <- h[-length(h)]
    }
    h <- c(h, i)
  }
  h
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
  if (fenetre > K) stop(sprintf(traduire("La fenêtre de Savitzky–Golay (%d variables) dépasse le spectre (%d variables X)", "The Savitzky–Golay window (%d variables) is wider than the spectrum (%d X variables)"), fenetre, K))
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
  pretraites <- if (length(definition$spectres)) pretraiter_spectres(bruts, definition$spectres, positions = positions_termes(projet, definition$x[quant]))$X else bruts
  list(bruts = bruts, pretraites = pretraites, variables = vapply(definition$x[quant], function(t) as.numeric(t$variables[1]), 0))
}
