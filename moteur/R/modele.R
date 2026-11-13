# Ajustement d'un modèle ACP ou PLS (la PLS-DA est une PLS dont les Y sont les
# indicatrices 0/1 des classes).
#
# `definition` décrit le modèle : type ("PCA" ou "PLS"), observations retenues,
# termes X et Y avec leur transformation et leur mise à l'échelle, nombre de composantes A,
# groupes de validation croisée (cv_groups, et groupes_vc : groupes_definition). C'est la forme des
# modèles d'un projet exporté.

#   orienter : pour un modèle venant d'un fichier .usp, oriente chaque composante comme la
#   solution enregistrée dans le fichier (le signe d'une composante est arbitraire ; ainsi
#   les graphiques ont le même sens que ceux du logiciel qui l'a écrit).
#   arret : function(res, a) appelée après chaque composante ; TRUE arrête le calcul
#   (ajustement automatique). Le modèle a alors moins de A composantes. Une OPLS, ajustée d'un
#   coup, ne l'appelle pas : avec `arret`, elle s'arrête aux composantes orthogonales que ses
#   données permettent, au lieu de le dire par une erreur.
#   projets_vc : pour l'iPLS, le projet de chaque groupe de la validation croisée (validation_croisee.R).
# Une définition à condition prend d'abord les observations où elle est vraie sur les données du
# moment (condition.R : resoudre_observations), quel que soit le type du modèle.
ajuster <- function(projet, definition, A = definition$A, G = definition$cv_groups %||% 7,
                    poids = "calcules", orienter = TRUE, arret = NULL, projets_vc = NULL) {
  definition <- resoudre_observations(projet, definition)
  if (est_regression(definition)) return(ajuster_regression(projet, definition))  # regression.R
  if (est_autre_analyse(definition)) return(ajuster_analyse(projet, definition))  # analyses.R
  modele <- calculer_modele(projet, definition, A, G, poids, arret, projets_vc)
  if (orienter && !is.null(definition[["solution"]][["T"]])) modele <- orienter_comme(modele, definition[["solution"]][["T"]])
  modele$statistiques <- statistiques(modele)
  if (length(definition$test)) modele$test <- evaluer_test(projet, modele, definition)  # jeu_test.R
  modele
}

# Le modèle et sa validation croisée, sans les statistiques qui en découlent (le test de
# permutation, qui en réajuste beaucoup, n'a besoin que de R2 et de Q2).
calculer_modele <- function(projet, definition, A = definition$A, G = definition$cv_groups %||% 7,
                            poids = "calcules", arret = NULL, projets_vc = NULL) {
  pls <- identical(definition$type, "PLS")
  obs <- definition$observations
  vx <- valeurs_bloc(projet, definition$x, obs)
  bx <- construire_bloc(projet, definition$x, obs, poids, spectres = definition$spectres, bruts = vx)
  by <- if (pls) construire_bloc(projet, definition$y, obs, poids, facteurs = bx$simples)
  # PLS-DA venant d'un fichier .usp : ses classes s'appellent $M1.DA1... ; usp_extract a retrouvé leur
  # nom quand une variable qualitative les définit, par exemple « $M1.DA1 (variete=Gala) »
  noms_y <- definition[["reference"]][["ynames"]]
  if (pls && length(noms_y) == ncol(by$X)) by$noms <- colnames(by$X) <- noms_y
  X <- preparer(bx$X)
  Y <- if (pls) preparer(by$X)
  vc <- debut_validation(projet, definition, X, Y, bx, by, A, G, poids, vx, projets_vc)  # validation_croisee.R
  # OPLS (opls.R) : A = Ap composantes prédictives + A - Ap composantes orthogonales
  Ap <- if (isTRUE(definition$opls)) definition$Ap %||% 1
  res <- if (isTRUE(definition$opls)) ajuster_opls(X, Y, max(0, A - Ap), Ap, vc, jusqua = !is.null(arret))
         else ajuster_matrices(X, Y, A, arret, vc)

  modele <- c(list(
    type = definition$type, plsda = isTRUE(definition$plsda), opls = isTRUE(definition$opls), Ap = Ap,
    A = ncol(res$T), N = nrow(X$E), K = ncol(X$E), G = vc$G, vc_par = nom_groupes_vc(projet, definition),
    nom = definition$name %||% "", titre = definition$title %||% "",
    observations = obs, x = bx, y = by, X = X$E, MX = X$M, Y = if (pls) Y$E, MY = if (pls) Y$M), res)
  class(modele) <- "modele_nuee"
  modele
}

# Le calcul lui-même, sur des matrices déjà mises à l'échelle (préparées par preparer()) :
# A composantes par NIPALS. Y absent : ACP. `vc` : les sous-modèles de la validation croisée
# (debut_validation), qui avancent d'une composante avec le modèle (ainsi l'ajustement
# automatique, `arret`, juge chaque composante sur son Q2 sans en calculer de trop).
ajuster_matrices <- function(X, Y, A, arret = NULL, vc = NULL) {
  pls <- !is.null(Y)
  K <- ncol(X$E)
  avec_x <- rowSums(X$M) > 0  # sans aucune valeur X, pas de score
  depart <- c(sum(X$E^2), if (pls) sum(Y$E^2))

  E <- X$E
  F <- if (pls) Y$E
  res <- list(T = NULL, P = NULL, W = NULL, C = NULL, U = NULL,
              ssxv = matrix(colSums(E^2), K, 1), ssyv = if (pls) matrix(colSums(F^2), ncol(F), 1))
  for (a in seq_len(A)) {
    # plus de variation à décrire (nipals.R) : l'ajustement automatique (arret) s'arrête là ;
    # sinon l'erreur dit combien de composantes le modèle peut avoir
    comp <- tryCatch(if (pls) composante_pls(E, X$M, F, Y$M, depart) else composante_acp(E, X$M, depart),
                     sans_variation = function(e) if (!is.null(arret) && a > 1) NULL else stop(trop_de_composantes(a), call. = FALSE))
    if (is.null(comp)) break
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
    if (!is.null(vc)) {  # (PRESS et SS jusqu'à cette composante, pour `arret`)
      vc <- composante_validation(vc, a)
      res$press <- vc$press
      res$ss_vc <- rbind(vc$ss, vc$press[-a, , drop = FALSE], deparse.level = 0)
    }
    if (!is.null(arret) && arret(res, a)) break
  }
  c(res[setdiff(names(res), c("press", "ss_vc"))], if (!is.null(vc)) fin_validation(vc, ncol(res$T)),
    list(ssx = colSums(res$ssxv), ssy = if (pls) colSums(res$ssyv), residus_x = E, residus_y = F, avec_x = avec_x))
}

trop_de_composantes <- function(a) {
  if (a == 1) return(traduire("Aucune composante possible : les données n'ont pas de variation (variables constantes, ou nulles après le prétraitement des spectres).",
                              "No component is possible: the data have no variation (constant variables, or all zero after the spectral preprocessing)."))
  sprintf(traduire("Composante %d impossible : il ne reste plus de variation à décrire après %d composante(s). Ce modèle peut en avoir au plus %d.",
                   "Component %d is not possible: no variation is left to describe after %d component(s). This model can have at most %d."), a, a - 1, a - 1)
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
