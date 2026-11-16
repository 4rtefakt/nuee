# OPLS (Trygg et Wold, 2002) : des composantes prédictives, qui portent ce qui dans X est lié
# à Y, et des composantes orthogonales, qui décrivent la variation de X sans lien avec Y. Les
# valeurs prédites sont celles d'une PLS au même nombre de composantes : l'OPLS sépare
# seulement ce qui prédit Y du reste, ce qui rend scores et loadings plus lisibles (graphique
# S, scores t[1] / to[1]).
# Ap composantes prédictives : une pour un seul Y ou une OPLS-DA à deux classes ; plus pour
# plusieurs Y ou plus de classes (definir_modele : autant que de Y, ou de classes moins une).
# Aucun projet .usp disponible n'avait de modèle OPLS : ce calcul suit l'article, sans
# comparaison possible avec des résultats enregistrés (moteur/tests/verifier_opls.R le compare
# à une seconde écriture de l'algorithme de l'article).
#
# Le modèle est rangé comme une PLS (voir ajuster_matrices dans modele.R) : composantes 1 à Ap
# = prédictives, les suivantes = orthogonales, avec un poids de Y nul (elles n'expliquent pas
# Y). Pour une nouvelle observation, les composantes orthogonales sont retirées d'abord, dans
# leur ordre, puis les prédictives (prediction.R). Les données manquantes sont ignorées dans
# chaque projection, comme dans nipals.R.

# Les Ap premières composantes PLS de E (masque ME) pour F (masque MF) ; moins quand il ne reste
# plus de variation à décrire (nipals.R ; `depart` : les sommes des carrés de X et de Y avant la
# 1re composante).
composantes_predictives <- function(E, ME, F, MF, Ap, depart) {
  comps <- list()
  for (a in seq_len(Ap)) {
    comp <- tryCatch(composante_pls(E, ME, F, MF, depart), sans_variation = function(e) NULL)
    if (is.null(comp)) break
    comps[[a]] <- comp
    if (a < Ap) {
      E <- retirer_composante(E, ME, comp$t, comp$p)
      F <- retirer_composante(F, MF, comp$t, comp$c)
    }
  }
  comps
}

# Plusieurs Y : une base orthonormée (colonnes de norme 1) de l'espace des poids w_m = X'y_m / y_m'y_m
# de chaque colonne de Y, par l'ACP de ces poids, arrêtée quand ce qui reste de leur somme des
# carrés est sous 1e-10 (Trygg et Wold, 2002 : OPLS pour plusieurs Y). Avec des classes, les
# colonnes de Y sont liées et l'espace a une dimension de moins que de classes. Une colonne sans
# variation n'y ajoute rien (dans un sous-modèle de la validation croisée : une classe dont toutes
# les observations sont dans le groupe retiré).
base_poids_y <- function(E, ME, F) {
  W <- vapply(seq_len(ncol(F)), function(m) if (sum(F[, m]^2) > 0) projeter_colonnes(E, ME, F[, m]) else numeric(ncol(E)), numeric(ncol(E)))
  s <- svd(W)
  reste <- rev(cumsum(rev(s$d^2))) / sum(s$d^2)  # reste[k] : part des composantes k et suivantes
  s$u[, seq_len(max(1, sum(reste >= 1e-10, na.rm = TRUE))), drop = FALSE]
}

# Le calcul pour Ao composantes orthogonales, sur des résidus E (masque ME) et F (masque MF) :
# à chaque étape, la première composante PLS de X filtré donne le loading p ; sa part
# orthogonale aux poids de Y est le poids orthogonal w_o (un seul Y : orthogonale au poids w de
# cette composante ; plusieurs Y : à l'espace de base_poids_y, calculé sur X avant filtrage),
# si bien que les scores t_o n'ont pas de lien avec Y ; t_o et ses loadings p_o sont retirés de
# X. Les composantes prédictives sont les composantes PLS de X une fois filtré.
# `predictives[[k + 1]]` : les composantes prédictives avec k composantes orthogonales (pour
# la validation croisée).
# Quand les données n'ont plus de variation à décrire (nipals.R ; pour une composante
# orthogonale : plus de variation de X orthogonale à Y), une erreur dit combien de composantes le
# modèle peut avoir ; avec `jusqua`, le noyau s'arrête à la dernière possible (sous-modèles de la
# validation croisée, ajustement automatique).
opls_noyau <- function(E, ME, F, MF, Ao, Ap = 1, depart = c(sum(E^2), sum(F^2)), jusqua = FALSE) {
  To <- Po <- Wo <- NULL
  impossible <- function(a) if (!jusqua) stop(trop_de_composantes(a), call. = FALSE)
  plein <- masque_plein(ME)  # (sans case manquante, les projections se passent du masque : nipals.R)
  base <- if (Ap > 1) base_poids_y(E, ME, F)
  comps <- composantes_predictives(E, ME, F, MF, Ap, depart)
  if (length(comps) < Ap) impossible(length(comps) + 1)
  predictives <- list(comps)
  for (k in seq_len(if (length(comps) == Ap) Ao else 0)) {
    p <- comps[[1]]$p
    wo <- if (Ap > 1) drop(p - base %*% crossprod(base, p)) else p - sum(comps[[1]]$w * p) * comps[[1]]$w
    if (!isTRUE(sum(wo^2) > SEUIL_VARIATION * sum(p^2))) { impossible(Ap + k); break }
    wo <- wo / norme(wo)
    to <- projeter(E, ME, wo, plein)
    po <- projeter_colonnes(E, ME, to, plein)
    filtre <- retirer_composante(E, ME, to, po, plein)
    comps <- composantes_predictives(filtre, ME, F, MF, Ap, depart)
    if (length(comps) < Ap) { impossible(Ap + k); break }
    E <- filtre
    To <- cbind(To, to)
    Po <- cbind(Po, po)
    Wo <- cbind(Wo, wo)
    predictives[[k + 1]] <- comps
  }
  list(pred = predictives[[length(predictives)]], predictives = predictives, To = To, Po = Po, Wo = Wo, E = E)
}

# Y prédit (centré-réduit) pour les lignes E (masque ME) par un noyau, avec ses k premières
# composantes orthogonales et ses a premières composantes prédictives.
opls_predire <- function(noyau, E, ME, k = ncol(noyau$Wo) %||% 0, a = length(noyau$pred)) {
  plein <- masque_plein(ME)
  for (j in seq_len(k)) {
    to <- projeter(E, ME, noyau$Wo[, j], plein)
    E <- retirer_composante(E, ME, to, noyau$Po[, j], plein)
  }
  comps <- noyau$predictives[[k + 1]]
  Y <- 0
  for (b in seq_len(a)) {
    tt <- projeter(E, ME, comps[[b]]$w, plein)
    Y <- Y + outer(tt, comps[[b]]$c)
    if (b < a) E <- retirer_composante(E, ME, tt, comps[[b]]$p, plein)
  }
  Y
}

# Le noyau `n` d'une OPLS (opls_noyau) réduit à ses Ao premières composantes orthogonales (moins s'il
# n'en a pas autant) : ce que rend opls_noyau pour Ao, ses étapes étant les premières de celles de n
# (une composante orthogonale après l'autre, et les prédictives de chacune). E, la matrice filtrée de
# ces composantes, est refait depuis `E0` (masque `ME0`) par les mêmes calculs ; sans E0, pas de E (la
# validation croisée ne s'en sert pas).
noyau_reduit <- function(n, Ao, E0 = NULL, ME0 = NULL) {
  a <- min(Ao, ncol(n$Wo) %||% 0)
  colonnes <- function(M) if (a) M[, seq_len(a), drop = FALSE]
  predictives <- n$predictives[seq_len(a + 1)]
  E <- NULL
  if (!is.null(E0)) {
    plein <- masque_plein(ME0)
    E <- E0
    for (k in seq_len(a)) E <- retirer_composante(E, ME0, n$To[, k], n$Po[, k], plein)
  }
  list(pred = predictives[[length(predictives)]], predictives = predictives, To = colonnes(n$To), Po = colonnes(n$Po), Wo = colonnes(n$Wo), E = E)
}

# Ajustement et validation croisée (`vc` : ses sous-modèles, debut_validation), avec le résultat
# rangé comme celui d'ajuster_matrices. `jusqua` : moins de composantes orthogonales si les
# données n'en permettent pas Ao (ajustement automatique). `etat` : le résultat garde aussi
# (etat_auto) la validation croisée telle que debut_validation l'a faite et les noyaux du modèle et
# de ses sous-modèles, de quoi refaire ce modèle pour moins de composantes orthogonales sans les
# recalculer (`noyaux` : ceux-là ; modele.R, modele_tronque).
ajuster_opls <- function(X, Y, Ao, Ap = 1, vc = NULL, jusqua = FALSE, etat = FALSE, noyaux = NULL) {
  N <- nrow(X$E)
  K <- ncol(X$E)
  M <- ncol(Y$E)
  avec_x <- rowSums(X$M) > 0
  n <- if (is.null(noyaux)) opls_noyau(X$E, X$M, Y$E, Y$M, Ao, Ap, jusqua = jusqua) else noyau_reduit(noyaux$modele, Ao, X$E, X$M)
  if (length(n$pred) < Ap) stop(erreur_composantes(length(n$pred) + 1))
  Ao <- ncol(n$Wo) %||% 0
  A <- Ap + Ao
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
  plein_x <- masque_plein(X$M)
  plein_y <- masque_plein(Y$M)
  for (a in seq_len(Ap)) {
    residus_x <- retirer_composante(residus_x, X$M, t0[, a], P[, a], plein_x)
    residus_y <- retirer_composante(residus_y, Y$M, t0[, a], C[, a], plein_y)
  }
  # part de X expliquée par chaque composante, dans l'ordre affiché (prédictives, orthogonales) :
  # les scores étant orthogonaux entre eux, les parts s'additionnent ; Y n'est expliqué que par
  # les prédictives
  E <- X$E
  F <- Y$E
  ssxv <- matrix(colSums(E^2), K, 1)
  ssyv <- matrix(colSums(F^2), M, 1)
  for (a in seq_len(A)) {
    E <- if (a == A) residus_x else retirer_composante(E, X$M, t0[, a], P[, a], plein_x)
    ssxv <- cbind(ssxv, colSums(E^2))
    if (a <= Ap) F <- retirer_composante(F, Y$M, t0[, a], C[, a], plein_y)
    ssyv <- cbind(ssyv, colSums(F[avec_x, , drop = FALSE]^2))
  }

  v <- if (!is.null(vc)) validation_opls(vc, Ao, Ap, noyaux$tours, garder = etat)
  c(list(T = T, P = P, W = W, C = C, U = U, ssxv = ssxv, ssyv = ssyv), v,
    list(ssx = colSums(ssxv), ssy = colSums(ssyv), residus_x = residus_x, residus_y = residus_y, avec_x = avec_x),
    if (etat) list(etat_auto = list(vc = vc, noyaux = list(modele = n[setdiff(names(n), "E")], tours = attr(v, "noyaux")))))
}

# La validation croisée d'une OPLS (validation_croisee.R) : chaque sous-modèle est une OPLS aux
# mêmes Ap composantes prédictives et Ao orthogonales (moins si les observations qui lui restent
# n'ont plus de variation : il prédit alors avec celles qu'il a). La ligne r du tableau des
# composantes (les prédictives, puis les orthogonales) est la prédiction avec min(r, Ap)
# composantes prédictives après max(0, r - Ap) orthogonales : ses PRESS en sont faits, comme pour
# une PLS. Les paramètres de chaque sous-modèle (rangés comme ceux du modèle) donnent les
# intervalles de confiance (jack-knife). `noyaux` : le noyau de chaque sous-modèle, déjà calculé pour
# au moins Ao composantes orthogonales (noyau_reduit) ; `garder` : les noyaux en attribut "noyaux"
# du résultat (sans leur matrice filtrée E).
validation_opls <- function(vc, Ao, Ap, noyaux = NULL, garder = FALSE) {
  A <- Ap + Ao
  M <- ncol(vc$Z$E)
  vc$press <- matrix(0, A, M)
  vc$predictions <- lapply(seq_len(A), function(r) matrix(NA_real_, nrow(vc$Z$E), M))
  gardes <- list()
  for (i in seq_along(vc$tours)) {
    verifier_arret()
    r <- vc$tours[[i]]
    nk <- if (is.null(noyaux)) opls_noyau(r$X$E, r$X$M, r$Y$E, r$Y$M, Ao, Ap, r$depart, jusqua = TRUE) else noyau_reduit(noyaux[[i]], Ao)
    if (garder) gardes[[i]] <- nk[setdiff(names(nk), "E")]
    ao <- ncol(nk$Wo) %||% 0
    for (l in seq_len(A)) {
      k <- min(max(0, l - Ap), ao)
      a <- min(l, Ap, length(nk$predictives[[k + 1]]))
      pred <- if (a) opls_predire(nk, r$Eh, r$Mh, k, a) else matrix(0, sum(r$hors), M)
      prediction <- vers_modele_complet(pred, r$sous, vc$complet)
      vc$press[l, ] <- vc$press[l, ] + colSums(((vc$Z$E[r$hors, , drop = FALSE] - prediction) * vc$Z$M[r$hors, , drop = FALSE])^2)
      vc$predictions[[l]][r$hors, ] <- prediction
    }
    complet <- ao == Ao && length(nk$pred) == Ap
    for (a in seq_len(A)) {
      j <- a - Ap
      if (a <= Ap && complet) {
        r$W[, a] <- nk$pred[[a]]$w
        r$P[, a] <- nk$pred[[a]]$p
        r$C[, a] <- nk$pred[[a]]$c
      } else if (a > Ap && j <= ao) {
        r$W[, a] <- nk$Wo[, j]
        r$P[, a] <- nk$Po[, j]
        r$C[, a] <- 0
      }
    }
    vc$tours[[i]] <- r
  }
  structure(fin_validation(vc, A), noyaux = if (garder) gardes)
}
