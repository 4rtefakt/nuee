# Statistiques dérivées d'un modèle ajusté, avec les formules publiées (T2 de Hotelling,
# DModX, VIP, coefficients...).

statistiques <- function(m, niveau = 0.95) {
  A <- m$A
  N <- m$N
  K <- m$K
  pls <- identical(m$type, "PLS")
  s <- list(composantes = resume_composantes(m))
  # Valeur propre d'une composante : sa part de variance de X multipliée par min(N, K).
  s$composantes$valeur_propre <- s$composantes$R2X * min(N, K)

  # --- Observations : T2 de Hotelling, DModX
  notes <- !is.na(m$T[, 1])
  Nt <- sum(notes)
  var_t <- colSums(m$T[notes, , drop = FALSE]^2) / (Nt - 1)
  obs <- data.frame(row.names = as.character(m$observations))
  for (a in seq_len(A)) obs[[paste0("t", a)]] <- m$T[, a]
  if (pls) for (a in seq_len(A)) obs[[paste0("u", a)]] <- m$U[, a]
  obs$T2 <- rowSums(sweep(m$T^2, 2, var_t, "/"))
  limites <- list()
  for (p in c(0.95, 0.99)) {
    limites[[sprintf("T2Crit(%d%%)", round(100 * p))]] <- A * (Nt - 1) / (Nt - A) * stats::qf(p, A, Nt - A)
  }

  if (K > A && N > A + 1) {
    E <- m$residus_x
    Ki <- rowSums(m$MX)
    ss_i <- rowSums(E^2)
    correction <- sqrt(N / (N - A - 1))  # les observations du modèle ont servi à l'ajuster
    absolu <- sqrt(ss_i / ifelse(Ki > A, Ki - A, NA)) * correction
    absolu[!notes | Ki <= A] <- NA
    s0 <- sqrt(m$ssx[A + 1] / ((N - A - 1) * (K - A)))  # écart-type résiduel du modèle
    obs$DModX <- absolu / s0
    obs$DModX_absolu <- absolu
    # DModX pondéré (par défaut ; les fichiers .usp de version 13 et suivantes) : chaque résidu est multiplié par le
    # pouvoir de modélisation de sa variable, 1 - SV_k / SV0_k.
    pm <- pouvoir_modelisation(m)
    Ew <- sweep(E, 2, pm, "*")
    sw0 <- sqrt(sum(Ew^2) / ((N - A - 1) * (K - A)))
    if (sw0 > 0) {
      obs$DModX_pondere <- sqrt(rowSums(Ew^2) / ifelse(Ki > A, Ki - A, NA)) * correction / sw0
      obs$DModX_pondere[!notes | Ki <= A] <- NA
    }
    limites[["DCrit(5%)"]] <- dcrit(N, K, A)
    s$s0 <- s0
    s$sw0 <- sw0
    s$pouvoir_modelisation <- pm
  }

  # --- Variables
  vars <- data.frame(terme = m$x$noms, bloc = "X", stringsAsFactors = FALSE)
  for (a in seq_len(A)) vars[[paste0("p", a)]] <- m$P[, a]
  vars$R2V_cum <- 1 - m$ssxv[, A + 1] / m$ssxv[, 1]
  if (!pls) {
    vars$Q2V_cum <- 1 - m$press[A, ] / m$ss_vc[1, ]
    ic <- intervalles_acp(m, niveau)
    for (a in seq_len(A)) vars[[paste0("p", a, "_IC")]] <- ic[, a]
  }
  vars$centre <- m$x$centre
  vars$poids <- m$x$poids
  vars$echelle <- m$x$type

  if (pls) {
    cf <- coefficients_pls(m$W, m$P, m$C)
    d <- m$ssy[seq_len(A)] - m$ssy[seq_len(A) + 1]
    vars$VIP <- sqrt(K * rowSums(sweep(m$W^2, 2, d, "*")) / sum(d))
    vars$VIP_IC <- intervalles_vip(m, d, niveau)
    s$seuil_vip <- seuil_vip(stats::setNames(vars$VIP, vars$terme))
    for (a in seq_len(A)) {
      vars[[paste0("w", a)]] <- m$W[, a]
      vars[[paste0("w*", a)]] <- cf$Wetoile[, a]
    }
    ic <- intervalles_pls(m, niveau)
    for (a in seq_len(A)) {
      vars[[paste0("p", a, "_IC")]] <- ic$p[, a]
      vars[[paste0("w*", a, "_IC")]] <- ic$wetoile[, a]
    }
    ny <- m$y$noms
    coef <- data.frame(terme = c(m$x$noms, "Constante"), stringsAsFactors = FALSE)
    ajuste <- matrix(NA_real_, N, length(ny))
    observe <- matrix(NA_real_, N, length(ny))
    s$RMSEE <- s$RMSEcv <- stats::setNames(numeric(length(ny)), ny)
    pred_vc <- m$vc$predictions[[A]]  # Y prédit en validation croisée (validation_croisee.R)
    for (j in seq_along(ny)) {
      bj <- cf$coef_cs[, j]
      brut <- bj * m$x$poids / m$y$poids[j]  # dans les unités des données
      coef[[paste0("CoeffCS ", ny[j])]] <- c(bj, NA)
      coef[[paste0("CoeffCS ", ny[j], " IC")]] <- c(ic$coef_cs[, j], NA)
      coef[[paste0("Coeff ", ny[j])]] <- c(brut, m$y$centre[j] - sum(brut * m$x$centre))
      coef[[paste0("Coeff ", ny[j], " IC")]] <- c(ic$coef_cs[, j] * m$x$poids / m$y$poids[j], NA)
      enleve <- function(z) z / m$y$poids[j] + m$y$centre[j]
      ajuste[, j] <- enleve(as.vector(ifelse(is.na(m$T), 0, m$T) %*% m$C[j, ]))
      ajuste[!notes, j] <- NA
      observe[, j] <- ifelse(m$MY[, j] == 1, enleve(m$Y[, j]), NA)
      r <- observe[, j] - ajuste[, j]
      n <- sum(!is.na(r))
      s$RMSEE[j] <- sqrt(sum(r^2, na.rm = TRUE) / (n - A - 1))
      rcv <- ifelse(m$MY[, j] == 1, (m$Y[, j] - pred_vc[, j]) / m$y$poids[j], NA)
      s$RMSEcv[j] <- sqrt(mean(rcv^2, na.rm = TRUE))
      obs[[paste(ny[j], "observé")]] <- observe[, j]
      obs[[paste(ny[j], "ajusté")]] <- ajuste[, j]
      obs[[paste(ny[j], "résidu")]] <- r
    }
    s$coefficients <- coef
    yv <- data.frame(terme = ny, bloc = "Y", stringsAsFactors = FALSE)
    for (a in seq_len(A)) {
      yv[[paste0("c", a)]] <- m$C[, a]
      yv[[paste0("c", a, "_IC")]] <- ic$c[, a]
    }
    yv$R2V_cum <- 1 - m$ssyv[, A + 1] / m$ssyv[, 1]
    yv$Q2V_cum <- 1 - m$press[A, ] / m$ss_vc[1, ]
    yv$centre <- m$y$centre
    yv$poids <- m$y$poids
    yv$RMSEE <- s$RMSEE
    yv$RMSEcv <- s$RMSEcv
    s$variables_y <- yv
    s$cv_anova <- stats::setNames(cv_anova(m, pred_vc), ny)
    if (m$plsda) {  # classe observée et classe prédite (la plus grande valeur de Y)
      la_plus_grande <- function(M) apply(M, 1, function(v) if (all(is.na(v))) NA else ny[which.max(v)])
      obs$classe <- la_plus_grande(observe)
      obs$classe_predite <- la_plus_grande(ajuste)
      predit_vc <- sweep(sweep(pred_vc, 2, m$y$poids, "/"), 2, m$y$centre, "+")
      predit_vc[!notes, ] <- NA
      s$classification <- classification_plsda(observe, ajuste, predit_vc, ny)
      obs$classe_predite_vc <- s$classification$vc$predite
    }
  }
  s$variables <- vars
  s$observations <- obs
  s$limites <- limites
  s
}

# Seuil de VIP. L'usage retient VIP > 1 comme « les plus importantes » ; la règle
# empirique de Wold place la limite des variables utiles vers 0,8. Triés par ordre
# décroissant, les VIP montrent souvent une marche nette entre les variables utiles et les
# autres : on prend la plus grande chute entre deux VIP consécutifs dont le milieu tombe
# entre 0,5 et 1,2, et le seuil au milieu de cette chute. Sans chute dans cette fenêtre, le
# seuil reste 0,8. (Méthode de l'application.)
seuil_vip <- function(vip, repere = 0.8, fenetre = c(0.5, 1.2)) {
  v <- sort(vip, decreasing = TRUE)
  n <- length(v)
  if (n < 2) return(list(seuil = repere, chute = NA, retenues = names(v)[v > repere], marche = FALSE))
  chutes <- v[-n] - v[-1]
  milieux <- (v[-n] + v[-1]) / 2
  dans <- which(milieux >= fenetre[1] & milieux <= fenetre[2])
  if (!length(dans)) return(list(seuil = repere, chute = NA, retenues = names(v)[v > repere], marche = FALSE))
  i <- dans[which.max(chutes[dans])]
  # une « marche » : la chute est au moins le double de la chute médiane entre VIP voisins
  list(seuil = unname(milieux[i]), chute = unname(chutes[i]), retenues = names(v)[seq_len(i)],
       marche = chutes[i] >= 2 * stats::median(chutes))
}

# Intervalle de confiance des VIP par jack-knife : VIP recalculé avec les poids w de chaque
# sous-modèle de la validation croisée (et la part de Y expliquée par composante du modèle complet).
intervalles_vip <- function(m, d, niveau) {
  G <- length(m$vc$tours)
  vip_g <- sapply(seq_len(G), function(g) {
    W <- matrix(tour(m, g, "w"), ncol = m$A)
    W <- sweep(W, 2, sqrt(colSums(W^2)), "/")
    sqrt(m$K * rowSums(sweep(W^2, 2, d, "*")) / sum(d))
  })
  apply(matrix(vip_g, nrow = m$K), 1, jackknife, niveau = niveau)
}

# W* = W (P'W)^-1 et coefficients centrés-réduits W* C' (une colonne par Y).
coefficients_pls <- function(W, P, C) {
  We <- W %*% solve(crossprod(P, W))
  list(Wetoile = We, coef_cs = We %*% t(C))
}

# Pouvoir de modélisation de chaque variable X : 1 - SV_k / SV0_k, SV étant l'écart-type
# résiduel de la variable après A composantes et SV0 son écart-type initial.
pouvoir_modelisation <- function(m) {
  A <- m$A
  N <- m$N
  sv <- sqrt(m$ssxv[, A + 1] / (N - A - 1))
  sv0 <- sqrt(m$ssxv[, 1] / (N - 1))
  ifelse(sv0 > 0, 1 - sv / sv0, 0)
}

# DModX critique (normalisé) : racine du quantile de F avec
# DF_mod = (N - C - A)(K - A) et DF_obs = (min(K, 100, DF_mod) - A) / (N / (N - C - A)),
# C = 1 si les données sont centrées.
dcrit <- function(N, K, A, C = 1, p = 0.95) {
  dfm <- max((N - C - A) * (K - A), 1)
  m_ <- min(K, 100, dfm)
  corr <- N / max(N - C - A, 1)
  dfo <- (if (K > dfm) m_ + sqrt(K - dfm) - A else m_ - A) / corr
  sqrt(stats::qf(p, max(dfo, 1), dfm))
}

# Demi-largeur d'intervalle de confiance par jack-knife sur les G sous-modèles de la validation
# croisée : t(0.975, G - 1) * racine((G - 1) / G * somme((v_g - moyenne)^2)). Manquante quand un
# sous-modèle n'a pas pu avoir une composante (ses données n'avaient plus de variation).
jackknife <- function(valeurs, niveau = 0.95) {
  G <- length(valeurs)
  if (G < 2 || anyNA(valeurs)) return(NA_real_)
  stats::qt(1 - (1 - niveau) / 2, G - 1) * sqrt((G - 1) / G * sum((valeurs - mean(valeurs))^2))
}

# Les paramètres `quoi` ("w", "p" ou "c") du sous-modèle g de la validation croisée, une colonne
# par composante, chacune orientée comme celle du modèle complet (signe de w en PLS, de p en ACP).
# Ils sont dans les unités du sous-modèle (ses centres et poids).
tour <- function(m, g, quoi) {
  pls <- identical(m$type, "PLS")
  r <- m$vc$tours[[g]]
  sapply(seq_len(m$A), function(a) {
    sens <- if (pls) sign(sum(r$W[, a] * m$W[, a])) else sign(sum(r$P[, a] * m$P[, a]))
    r[[toupper(quoi)]][, a] * if (is.na(sens) || sens == 0) 1 else sens
  })
}

intervalles_acp <- function(m, niveau) {
  G <- length(m$vc$tours)
  Pg <- lapply(seq_len(G), function(g) matrix(tour(m, g, "p"), ncol = m$A))
  sapply(seq_len(m$A), function(a) apply(sapply(Pg, function(P) P[, a]), 1, jackknife, niveau = niveau))
}

intervalles_pls <- function(m, niveau) {
  G <- length(m$vc$tours)
  A <- m$A
  ronds <- lapply(seq_len(G), function(g) {
    W <- matrix(tour(m, g, "w"), ncol = A)
    P <- matrix(tour(m, g, "p"), ncol = A)
    C <- matrix(tour(m, g, "c"), ncol = A)
    cf <- tryCatch(coefficients_pls(W, P, C), error = function(e) NULL)
    list(W = W, P = P, C = C, We = cf$Wetoile, B = cf$coef_cs)
  })
  ok <- all(vapply(ronds, function(r) !is.null(r$B), TRUE))
  jk <- function(f, nr, nc) {
    if (!ok) return(matrix(NA_real_, nr, nc))
    matrix(sapply(seq_len(nr * nc), function(i) jackknife(sapply(ronds, function(r) f(r)[i]), niveau)), nr, nc)
  }
  K <- m$K
  list(p = jk(function(r) r$P, K, A), wetoile = jk(function(r) r$We, K, A),
       c = jk(function(r) matrix(r$C, ncol = A), nrow(m$C), A), coef_cs = jk(function(r) r$B, K, nrow(m$C)))
}

# Biplot : scores et loadings mis à l'échelle des corrélations, tous
# dans le cercle de rayon 1.
#   t(corr) : chaque vecteur de scores divisé par sa norme ;
#   p(corr), et c(corr) pour les Y d'une PLS : corrélation de chaque variable centrée-réduite
#   avec les scores, sur les observations où elle a une valeur.
# La documentation publiée nomme ces vecteurs sans donner leurs formules : ce sont les définitions usuelles
# (Wiklund et al., 2008), qui gardent bien tous les points dans le cercle.
correlations_biplot <- function(m) {
  T <- m$T
  ok <- !is.na(T[, 1])
  A <- ncol(T)
  tc <- sweep(T, 2, sqrt(colSums(T[ok, , drop = FALSE]^2)), "/")
  correlations <- function(X, M) {
    if (is.null(X)) return(NULL)
    matrix(unlist(lapply(seq_len(ncol(X)), function(k) {
      i <- ok & M[, k] == 1
      if (sum(i) < 3 || stats::sd(X[i, k]) == 0) return(rep(NA_real_, A))
      as.numeric(stats::cor(X[i, k], T[i, , drop = FALSE]))
    })), ncol = A, byrow = TRUE)
  }
  list(t = tc, p = correlations(m$X, m$MX), c = correlations(m$Y, m$MY))
}
