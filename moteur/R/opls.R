# OPLS (Trygg et Wold, 2002) : une composante prédictive, qui porte ce
# qui dans X est lié à Y, et des composantes orthogonales, qui décrivent la variation de X
# sans lien avec Y. Les valeurs prédites sont celles d'une PLS au même nombre de composantes :
# l'OPLS sépare seulement ce qui prédit Y du reste, ce qui rend scores et loadings plus
# lisibles (graphique S, scores t[1] / to[1]).
# Ici : une seule composante prédictive, donc un seul Y, ou une OPLS-DA à deux classes.
# Aucun projet .usp disponible n'avait de modèle OPLS : ce calcul suit l'article, sans
# comparaison possible avec des résultats enregistrés.
#
# Le modèle est rangé comme une PLS (voir ajuster_matrices dans modele.R) : composante 1 =
# prédictive, composantes 2... = orthogonales, avec un poids de Y nul (elles n'expliquent pas
# Y). Pour une nouvelle observation, les composantes orthogonales sont retirées d'abord, dans
# leur ordre, puis la prédictive (prediction.R). Les données manquantes sont ignorées dans
# chaque projection, comme dans nipals.R.

# Le calcul pour Ao composantes orthogonales, sur des résidus E (masque ME) et F (masque MF) :
# à chaque étape, la composante PLS de X filtré donne p et w ; la part de p orthogonale à w
# est le poids orthogonal w_o ; ses scores t_o et loadings p_o sont retirés de X. La
# composante prédictive est la composante PLS de X une fois filtré. `predictives[[k + 1]]`
# est la composante prédictive avec k composantes orthogonales (pour la validation croisée).
opls_noyau <- function(E, ME, F, MF, Ao) {
  To <- Po <- Wo <- NULL
  predictives <- list()
  for (k in 0:Ao) {
    comp <- composante_pls(E, ME, F, MF)
    predictives[[k + 1]] <- comp
    if (k == Ao) break
    wo <- comp$p - sum(comp$w * comp$p) * comp$w  # w est de norme 1
    wo <- wo / norme(wo)
    to <- projeter(E, ME, wo)
    po <- projeter(t(E), t(ME), to)
    E <- (E - outer(to, po)) * ME
    To <- cbind(To, to)
    Po <- cbind(Po, po)
    Wo <- cbind(Wo, wo)
  }
  list(pred = predictives[[Ao + 1]], predictives = predictives, To = To, Po = Po, Wo = Wo, E = E)
}

# Y prédit (centré-réduit) pour les lignes E (masque ME) par un noyau, avec ses k premières
# composantes orthogonales.
opls_predire <- function(noyau, E, ME, k = ncol(noyau$Wo) %||% 0) {
  for (j in seq_len(k)) {
    to <- projeter(E, ME, noyau$Wo[, j])
    E <- (E - outer(to, noyau$Po[, j])) * ME
  }
  pred <- noyau$predictives[[k + 1]]
  outer(projeter(E, ME, pred$w), pred$c)
}

# Ajustement et validation croisée, avec le résultat rangé comme celui d'ajuster_matrices.
# Validation croisée : pour 0, 1... Ao composantes orthogonales, le modèle est réajusté sans
# chaque groupe d'observations (avec la mise à l'échelle du modèle complet) et
# prédit les observations retirées ; Q2(cum) = 1 - PRESS / SS(Y). Les tours de validation
# donnent aussi les intervalles de confiance (jack-knife).
ajuster_opls <- function(X, Y, Ao, G) {
  N <- nrow(X$E)
  K <- ncol(X$E)
  M <- ncol(Y$E)
  A <- Ao + 1
  avec_x <- rowSums(X$M) > 0
  groupes <- groupes_validation(N, G)
  n <- opls_noyau(X$E, X$M, Y$E, Y$M, Ao)
  tp <- n$pred$t
  tp[!avec_x] <- NA
  To <- n$To
  if (Ao) To[!avec_x, ] <- NA
  T <- cbind(tp, To)
  P <- cbind(n$pred$p, n$Po)
  W <- cbind(n$pred$w, n$Wo)
  C <- cbind(n$pred$c, matrix(0, M, Ao))
  U <- cbind(n$pred$u, matrix(0, N, Ao))
  dimnames(T) <- dimnames(P) <- dimnames(W) <- dimnames(C) <- dimnames(U) <- NULL

  # résidus : X filtré des composantes orthogonales, moins la composante prédictive
  t0 <- ifelse(is.na(T), 0, T)
  residus_x <- (n$E - outer(t0[, 1], P[, 1])) * X$M
  residus_y <- (Y$E - outer(t0[, 1], C[, 1])) * Y$M
  # part de X expliquée par chaque composante, dans l'ordre affiché (prédictive, orthogonales) :
  # les scores étant orthogonaux entre eux, les parts s'additionnent
  E <- X$E
  ssxv <- matrix(colSums(E^2), K, 1)
  for (a in seq_len(A)) {
    E <- if (a == A) residus_x else (E - outer(t0[, a], P[, a])) * X$M
    ssxv <- cbind(ssxv, colSums(E^2))
  }
  ssyv <- cbind(colSums(Y$E^2), matrix(colSums(residus_y[avec_x, , drop = FALSE]^2), M, A))

  # validation croisée
  press <- matrix(0, A, M)
  prediction <- matrix(0, N, M)
  tours <- list()
  for (g in sort(unique(groupes))) {
    hors <- groupes == g
    dans <- !hors
    nk <- opls_noyau(X$E[dans, , drop = FALSE], X$M[dans, , drop = FALSE], Y$E[dans, , drop = FALSE], Y$M[dans, , drop = FALSE], Ao)
    for (k in 0:Ao) {
      pred <- opls_predire(nk, X$E[hors, , drop = FALSE], X$M[hors, , drop = FALSE], k)
      press[k + 1, ] <- press[k + 1, ] + colSums(((Y$E[hors, , drop = FALSE] - pred) * Y$M[hors, , drop = FALSE])^2)
      if (k == Ao) prediction[hors, ] <- pred
    }
    tours[[length(tours) + 1]] <- nk
  }
  # comme pour la PLS : PRESS de chaque composante rapporté à ce qui restait avant elle
  ss_vc <- rbind(colSums(Y$E^2), press[-A, , drop = FALSE])
  vc <- lapply(seq_len(A), function(a) list(
    press = press[a, ], ss = ss_vc[a, ],
    prediction = if (a == 1) prediction else matrix(0, N, M),
    tours = lapply(tours, function(nk) if (a == 1) nk$pred[c("w", "c", "p")]
                   else list(w = nk$Wo[, a - 1], c = rep(0, M), p = nk$Po[, a - 1]))))
  rapport <- rowSums(press) / rowSums(ss_vc)
  list(T = T, P = P, W = W, C = C, U = U, ssxv = ssxv, ssyv = ssyv, press = press, ss_vc = ss_vc, vc = vc,
       ssx = colSums(ssxv), ssy = colSums(ssyv), q2 = 1 - rapport, q2cum = 1 - cumprod(rapport),
       residus_x = residus_x, residus_y = residus_y, avec_x = avec_x)
}
