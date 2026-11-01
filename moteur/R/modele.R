# Ajustement d'un modèle ACP ou PLS (la PLS-DA est une PLS dont les Y sont les
# indicatrices 0/1 des classes).
#
# `definition` décrit le modèle : type ("PCA" ou "PLS"), observations retenues,
# termes X et Y avec leur transformation et leur mise à l'échelle, nombre de composantes A,
# nombre de groupes de validation croisée. C'est la forme des modèles d'un projet exporté.

#   orienter : pour un modèle venant d'un fichier .usp, oriente chaque composante comme la
#   solution enregistrée dans le fichier (le signe d'une composante est arbitraire ; ainsi
#   les graphiques ont le même sens que ceux du logiciel qui l'a écrit).
#   arret : function(res, a) appelée après chaque composante ; TRUE arrête le calcul
#   (ajustement automatique). Le modèle a alors moins de A composantes.
ajuster <- function(projet, definition, A = definition$A, G = definition$cv_groups %||% 7,
                    poids = "calcules", orienter = TRUE, arret = NULL) {
  pls <- identical(definition$type, "PLS")
  obs <- definition$observations
  bx <- construire_bloc(projet, definition$x, obs, poids, spectres = definition$spectres)
  by <- if (pls) construire_bloc(projet, definition$y, obs, poids, facteurs = bx$simples)
  # PLS-DA venant d'un fichier .usp : ses classes s'appellent $M1.DA1... ; usp_extract a retrouvé leur
  # nom quand une variable qualitative les définit, par exemple « $M1.DA1 (variete=Gala) »
  noms_y <- definition[["reference"]][["ynames"]]
  if (pls && length(noms_y) == ncol(by$X)) by$noms <- colnames(by$X) <- noms_y
  X <- preparer(bx$X)
  Y <- if (pls) preparer(by$X)
  # OPLS (opls.R) : A = 1 composante prédictive + A - 1 composantes orthogonales
  res <- if (isTRUE(definition$opls)) ajuster_opls(X, Y, A - 1, G) else ajuster_matrices(X, Y, A, G, arret)

  modele <- c(list(
    type = definition$type, plsda = isTRUE(definition$plsda), opls = isTRUE(definition$opls),
    A = ncol(res$T), N = nrow(X$E), K = ncol(X$E), G = G,
    nom = definition$name %||% "", titre = definition$title %||% "",
    observations = obs, x = bx, y = by, X = X$E, MX = X$M, Y = if (pls) Y$E, MY = if (pls) Y$M), res)
  class(modele) <- "modele_nuee"
  if (orienter && !is.null(definition[["solution"]][["T"]])) modele <- orienter_comme(modele, definition[["solution"]][["T"]])
  modele$statistiques <- statistiques(modele)
  modele
}

# Le calcul lui-même, sur des matrices déjà mises à l'échelle (préparées par preparer()) :
# A composantes par NIPALS, chacune avec sa validation croisée. Y absent : ACP.
# Sert aussi au test de permutation, qui réajuste le même X avec un Y permuté.
ajuster_matrices <- function(X, Y, A, G, arret = NULL) {
  pls <- !is.null(Y)
  N <- nrow(X$E)
  K <- ncol(X$E)
  avec_x <- rowSums(X$M) > 0  # sans aucune valeur X, pas de score
  groupes <- groupes_validation(N, G)
  groupes_var <- groupes_validation(K, G)

  E <- X$E
  F <- if (pls) Y$E
  res <- list(T = NULL, P = NULL, W = NULL, C = NULL, U = NULL,
              ssxv = matrix(colSums(E^2), K, 1), ssyv = if (pls) matrix(colSums(F^2), ncol(F), 1),
              press = NULL, ss_vc = NULL, vc = list())
  for (a in seq_len(A)) {
    if (pls) {
      vc <- validation_pls(E, X$M, F, Y$M, groupes)
      comp <- composante_pls(E, X$M, F, Y$M)
    } else {
      vc <- validation_acp(E, X$M, groupes, groupes_var)
      comp <- composante_acp(E, X$M)
    }
    tt <- comp$t
    tt[!avec_x] <- NA
    t0 <- ifelse(is.na(tt), 0, tt)
    E <- (E - outer(t0, comp$p)) * X$M
    res$T <- cbind(res$T, tt)
    res$P <- cbind(res$P, comp$p)
    res$ssxv <- cbind(res$ssxv, colSums(E^2))
    if (pls) {
      F <- (F - outer(t0, comp$c)) * Y$M
      res$W <- cbind(res$W, comp$w)
      res$C <- cbind(res$C, comp$c)
      u <- comp$u
      u[!avec_x] <- NA
      res$U <- cbind(res$U, u)
      res$ssyv <- cbind(res$ssyv, colSums(F[avec_x, , drop = FALSE]^2))
    }
    res$press <- rbind(res$press, vc$press)
    res$ss_vc <- rbind(res$ss_vc, vc$ss)
    res$vc[[a]] <- vc
    if (!is.null(arret) && arret(res, a)) break
  }
  rapport <- rowSums(res$press) / rowSums(res$ss_vc)
  c(res, list(ssx = colSums(res$ssxv), ssy = if (pls) colSums(res$ssyv),
              q2 = 1 - rapport, q2cum = 1 - cumprod(rapport),
              residus_x = E, residus_y = F, avec_x = avec_x))
}

# Change le signe des composantes dont les scores sont opposés à ceux de `reference`
# (une ligne par composante, comme les fichiers .usp les enregistrent).
orienter_comme <- function(m, reference) {
  reference <- if (is.matrix(reference)) reference else matrix(reference, nrow = 1)
  for (a in seq_len(min(m$A, nrow(reference)))) {
    r <- reference[a, ]
    if (length(r) != m$N) next
    if (sum(r * m$T[, a], na.rm = TRUE) < 0) {
      for (k in c("T", "P", "W", "C", "U")) if (!is.null(m[[k]])) m[[k]][, a] <- -m[[k]][, a]
    }
  }
  m
}

# Résumé par composante : R2X, R2X(cum), R2Y, R2Y(cum), Q2, Q2(cum).
resume_composantes <- function(m) {
  a <- seq_len(m$A)
  out <- data.frame(composante = a,
                    R2X = (m$ssx[a] - m$ssx[a + 1]) / m$ssx[1],
                    R2Xcum = 1 - m$ssx[a + 1] / m$ssx[1])
  if (!is.null(m$ssy)) {
    out$R2Y <- (m$ssy[a] - m$ssy[a + 1]) / m$ssy[1]
    out$R2Ycum <- 1 - m$ssy[a + 1] / m$ssy[1]
  }
  out$Q2 <- m$q2
  out$Q2cum <- m$q2cum
  out
}
