# OPLS (Trygg et Wold, 2002) : des composantes prédictives, qui portent ce qui dans X est lié
# à Y, et des composantes orthogonales, qui décrivent la variation de X sans lien avec Y. Les
# valeurs prédites sont celles d'une PLS au même nombre de composantes : l'OPLS sépare
# seulement ce qui prédit Y du reste, ce qui rend scores et loadings plus lisibles (graphique
# S, scores t[1] / to[1]).
# Ap composantes prédictives : une pour un seul Y ou une OPLS-DA à deux classes ; plus pour
# plusieurs Y ou plus de classes (definir_modele : autant que de Y, ou de classes moins une).
# Aucun projet .usp disponible n'avait de modèle OPLS : ce calcul suit l'article, sans
# comparaison possible avec des résultats enregistrés.
#
# Le modèle est rangé comme une PLS (voir ajuster_matrices dans modele.R) : composantes 1 à Ap
# = prédictives, les suivantes = orthogonales, avec un poids de Y nul (elles n'expliquent pas
# Y). Pour une nouvelle observation, les composantes orthogonales sont retirées d'abord, dans
# leur ordre, puis les prédictives (prediction.R). Les données manquantes sont ignorées dans
# chaque projection, comme dans nipals.R.

# Les Ap premières composantes PLS de E (masque ME) pour F (masque MF).
composantes_predictives <- function(E, ME, F, MF, Ap) {
  comps <- list()
  for (a in seq_len(Ap)) {
    comp <- composante_pls(E, ME, F, MF)
    comps[[a]] <- comp
    if (a < Ap) {
      E <- (E - outer(comp$t, comp$p)) * ME
      F <- (F - outer(comp$t, comp$c)) * MF
    }
  }
  comps
}

# Le calcul pour Ao composantes orthogonales, sur des résidus E (masque ME) et F (masque MF) :
# à chaque étape, les composantes PLS de X filtré donnent les poids prédictifs w ; la part du
# premier loading p qui leur est orthogonale est le poids orthogonal w_o ; ses scores t_o et
# loadings p_o sont retirés de X. Les composantes prédictives sont les composantes PLS de X une
# fois filtré. `predictives[[k + 1]]` : les composantes prédictives avec k composantes
# orthogonales (pour la validation croisée).
opls_noyau <- function(E, ME, F, MF, Ao, Ap = 1) {
  To <- Po <- Wo <- NULL
  predictives <- list()
  for (k in 0:Ao) {
    comps <- composantes_predictives(E, ME, F, MF, Ap)
    predictives[[k + 1]] <- comps
    if (k == Ao) break
    p <- comps[[1]]$p
    wo <- p
    for (cp in comps) wo <- wo - sum(cp$w * p) * cp$w  # les w sont de norme 1, orthogonaux entre eux
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
# composantes orthogonales et ses a premières composantes prédictives.
opls_predire <- function(noyau, E, ME, k = ncol(noyau$Wo) %||% 0, a = length(noyau$pred)) {
  for (j in seq_len(k)) {
    to <- projeter(E, ME, noyau$Wo[, j])
    E <- (E - outer(to, noyau$Po[, j])) * ME
  }
  comps <- noyau$predictives[[k + 1]]
  Y <- 0
  for (b in seq_len(a)) {
    tt <- projeter(E, ME, comps[[b]]$w)
    Y <- Y + outer(tt, comps[[b]]$c)
    if (b < a) E <- (E - outer(tt, comps[[b]]$p)) * ME
  }
  Y
}

# Ajustement et validation croisée, avec le résultat rangé comme celui d'ajuster_matrices.
# Validation croisée : le modèle est réajusté sans chaque groupe d'observations (avec la mise
# à l'échelle du modèle complet) et prédit les observations retirées, avec 1, 2... Ap
# composantes prédictives, puis avec les Ap prédictives et 1, 2... Ao orthogonales ;
# Q2(cum) = 1 - PRESS / SS(Y). Les tours de validation donnent aussi les intervalles de
# confiance (jack-knife).
ajuster_opls <- function(X, Y, Ao, G, Ap = 1) {
  N <- nrow(X$E)
  K <- ncol(X$E)
  M <- ncol(Y$E)
  A <- Ap + Ao
  avec_x <- rowSums(X$M) > 0
  groupes <- groupes_validation(N, G)
  n <- opls_noyau(X$E, X$M, Y$E, Y$M, Ao, Ap)
  colonnes <- function(cle) do.call(cbind, lapply(n$pred, `[[`, cle))
  Tp <- colonnes("t")
  Tp[!avec_x, ] <- NA
  To <- n$To
  if (Ao) To[!avec_x, ] <- NA
  T <- cbind(Tp, To)
  P <- cbind(colonnes("p"), n$Po)
  W <- cbind(colonnes("w"), n$Wo)
  C <- cbind(colonnes("c"), matrix(0, M, Ao))
  U <- cbind(colonnes("u"), matrix(0, N, Ao))
  dimnames(T) <- dimnames(P) <- dimnames(W) <- dimnames(C) <- dimnames(U) <- NULL

  # résidus : X filtré des composantes orthogonales, moins les composantes prédictives
  t0 <- ifelse(is.na(T), 0, T)
  residus_x <- n$E
  residus_y <- Y$E
  for (a in seq_len(Ap)) {
    residus_x <- (residus_x - outer(t0[, a], P[, a])) * X$M
    residus_y <- (residus_y - outer(t0[, a], C[, a])) * Y$M
  }
  # part de X expliquée par chaque composante, dans l'ordre affiché (prédictives, orthogonales) :
  # les scores étant orthogonaux entre eux, les parts s'additionnent ; Y n'est expliqué que par
  # les prédictives
  E <- X$E
  F <- Y$E
  ssxv <- matrix(colSums(E^2), K, 1)
  ssyv <- matrix(colSums(F^2), M, 1)
  for (a in seq_len(A)) {
    E <- if (a == A) residus_x else (E - outer(t0[, a], P[, a])) * X$M
    ssxv <- cbind(ssxv, colSums(E^2))
    if (a <= Ap) F <- (F - outer(t0[, a], C[, a])) * Y$M
    ssyv <- cbind(ssyv, colSums(F[avec_x, , drop = FALSE]^2))
  }

  # validation croisée : la ligne r de PRESS correspond à la composante r affichée
  press <- matrix(0, A, M)
  prediction <- matrix(0, N, M)
  tours <- list()
  for (g in sort(unique(groupes))) {
    hors <- groupes == g
    dans <- !hors
    nk <- opls_noyau(X$E[dans, , drop = FALSE], X$M[dans, , drop = FALSE], Y$E[dans, , drop = FALSE], Y$M[dans, , drop = FALSE], Ao, Ap)
    for (r in seq_len(A)) {
      pred <- opls_predire(nk, X$E[hors, , drop = FALSE], X$M[hors, , drop = FALSE], k = max(0, r - Ap), a = min(r, Ap))
      press[r, ] <- press[r, ] + colSums(((Y$E[hors, , drop = FALSE] - pred) * Y$M[hors, , drop = FALSE])^2)
      if (r == A) prediction[hors, ] <- pred
    }
    tours[[length(tours) + 1]] <- nk
  }
  # comme pour la PLS : PRESS de chaque composante rapporté à ce qui restait avant elle
  ss_vc <- rbind(colSums(Y$E^2), press[-A, , drop = FALSE])
  vc <- lapply(seq_len(A), function(a) list(
    press = press[a, ], ss = ss_vc[a, ],
    prediction = if (a == 1) prediction else matrix(0, N, M),
    tours = lapply(tours, function(nk) if (a <= Ap) nk$pred[[a]][c("w", "c", "p")]
                   else list(w = nk$Wo[, a - Ap], c = rep(0, M), p = nk$Po[, a - Ap]))))
  rapport <- rowSums(press) / rowSums(ss_vc)
  list(T = T, P = P, W = W, C = C, U = U, ssxv = ssxv, ssyv = ssyv, press = press, ss_vc = ss_vc, vc = vc,
       ssx = colSums(ssxv), ssy = colSums(ssyv), q2 = 1 - rapport, q2cum = 1 - cumprod(rapport),
       residus_x = residus_x, residus_y = residus_y, avec_x = avec_x)
}
