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
#   calcule : le modèle et sa validation croisée déjà calculés pour ces A composantes, tels que
#   calculer_modele les rend (l'ajustement automatique : autoajuster_modele, nouveau.R).
# Une définition à condition prend d'abord les observations où elle est vraie sur les données du
# moment (condition.R : resoudre_observations), quel que soit le type du modèle.
ajuster <- function(projet, definition, A = definition$A, G = definition$cv_groups %||% 7,
                    poids = "calcules", orienter = TRUE, arret = NULL, projets_vc = NULL, calcule = NULL) {
  definition <- resoudre_observations(projet, definition)
  if (est_regression(definition)) return(ajuster_regression(projet, definition))  # regression.R
  if (est_autre_analyse(definition)) return(ajuster_analyse(projet, definition))  # analyses.R
  modele <- if (!is.null(calcule)) calcule else calculer_modele(projet, definition, A, G, poids, arret, projets_vc)
  if (orienter && !is.null(definition[["solution"]][["T"]])) modele <- orienter_comme(modele, definition[["solution"]][["T"]])
  modele$statistiques <- statistiques(modele)
  modele$hierarchique <- q2_hierarchique(projet, definition, modele)  # (hierarchique.R : NULL sans score d'un autre modèle)
  if (length(definition$test)) modele$test <- evaluer_test(projet, modele, definition)  # jeu_test.R
  modele
}

# Ce qui, dans le modèle de `definition` et sa validation croisée, ne dépend que de X : les valeurs
# de ses termes X (valeurs_bloc ; des spectres dont chacun se prétraite seul, sans MSC, prétraités ici
# une fois, pour le modèle et pour chaque groupe de la validation croisée, qui en prend ses lignes :
# spectres.R), son bloc X et sa matrice préparée ; avec `tours`, le côté X de chaque sous-modèle de
# sa validation croisée (validation_croisee.R : tours_x_validation ; NULL pour une ACP ou une PLS
# validée avec les centres et poids du modèle complet, qui n'en a pas). Le test de permutation
# (validation.R), dont seul Y change, le calcule une fois pour tous ses modèles (calculer_modele :
# cote_x).
cote_x_modele <- function(projet, definition, G = definition$cv_groups %||% 7, poids = "calcules", tours = TRUE) {
  obs <- definition$observations
  vx <- valeurs_bloc(projet, definition$x, obs)
  par_ligne <- spectres_par_ligne(definition$spectres)
  if (par_ligne) vx <- pretraiter_bloc(projet, definition$x, vx, definition$spectres)$bruts
  bx <- construire_bloc(projet, definition$x, obs, poids, spectres = definition$spectres, bruts = vx, pretraites = par_ligne)
  X <- preparer(bx$X)
  plan <- if (tours) plan_validation(projet, definition, G)
  list(vx = vx, bx = bx, X = X, tours = if (tours && !(plan$complet && !isTRUE(definition$opls))) tours_x_validation(projet, definition, X, bx, vx, poids, plan))
}

# Le modèle et sa validation croisée, sans les statistiques qui en découlent (le test de
# permutation, qui en réajuste beaucoup, n'a besoin que de R2 et de Q2). `etat` : garder de quoi le
# réduire à moins de composantes (ajuster_matrices, modele_tronque ; l'ajustement automatique).
# `cote_x` : le côté X du modèle, déjà calculé sur les mêmes valeurs X (cote_x_modele).
calculer_modele <- function(projet, definition, A = definition$A, G = definition$cv_groups %||% 7,
                            poids = "calcules", arret = NULL, projets_vc = NULL, etat = FALSE, cote_x = NULL) {
  pls <- identical(definition$type, "PLS")
  obs <- definition$observations
  if (is.null(cote_x)) cote_x <- cote_x_modele(projet, definition, G, poids, tours = FALSE)
  vx <- cote_x$vx
  bx <- cote_x$bx
  X <- cote_x$X
  by <- if (pls) construire_bloc(projet, definition$y, obs, poids, facteurs = bx$simples)
  # PLS-DA venant d'un fichier .usp : ses classes s'appellent $M1.DA1... ; usp_extract a retrouvé leur
  # nom quand une variable qualitative les définit, par exemple « $M1.DA1 (variete=Gala) »
  noms_y <- definition[["reference"]][["ynames"]]
  if (pls && length(noms_y) == ncol(by$X)) by$noms <- colnames(by$X) <- noms_y
  Y <- if (pls) preparer(by$X)
  vc <- debut_validation(projet, definition, X, Y, bx, by, A, G, poids, vx, projets_vc, cote_x$tours)  # validation_croisee.R
  # OPLS (opls.R) : A = Ap composantes prédictives + A - Ap composantes orthogonales
  Ap <- if (isTRUE(definition$opls)) definition$Ap %||% 1
  res <- if (isTRUE(definition$opls)) ajuster_opls(X, Y, max(0, A - Ap), Ap, vc, jusqua = !is.null(arret), etat = etat)
         else ajuster_matrices(X, Y, A, arret, vc, etat, version = definition$version_origine)

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
# automatique, `arret`, juge chaque composante sur son Q2 sans en calculer de trop). `etat` : le
# résultat garde aussi (etat_auto) la validation croisée en cours et les résidus d'avant la dernière
# composante, de quoi en faire le modèle d'une composante de moins (modele_tronque). `version` : celle
# du fichier .usp d'où vient le modèle (definition$version_origine), dont il suit alors le NIPALS
# (nipals.R : TOLERANCE_FICHIER) ; NULL pour un modèle de Nuée.
ajuster_matrices <- function(X, Y, A, arret = NULL, vc = NULL, etat = FALSE, version = NULL) {
  pls <- !is.null(Y)
  K <- ncol(X$E)
  avec_x <- rowSums(X$M) > 0  # sans aucune valeur X, pas de score
  depart <- c(sum(X$E^2), if (pls) sum(Y$E^2))
  plein_x <- masque_plein(X$M)  # (sans valeur manquante, retirer une composante se passe du masque : nipals.R)
  plein_y <- pls && masque_plein(Y$M)

  E <- X$E
  F <- if (pls) Y$E
  res <- list(T = NULL, P = NULL, W = NULL, C = NULL, U = NULL,
              ssxv = matrix(colSums(E^2), K, 1), ssyv = if (pls) matrix(colSums(F^2), ncol(F), 1))
  avant <- NULL
  for (a in seq_len(A)) {
    # plus de variation à décrire (nipals.R) : l'ajustement automatique (arret) s'arrête là ;
    # sinon l'erreur dit combien de composantes le modèle peut avoir
    comp <- tryCatch(if (pls) composante_pls(E, X$M, F, Y$M, depart, version) else composante_acp(E, X$M, depart, version),
                     sans_variation = function(e) if (!is.null(arret) && a > 1) NULL else stop(erreur_composantes(a)))
    if (is.null(comp)) break
    tt <- comp$t
    tt[!avec_x] <- NA
    t0 <- ifelse(is.na(tt), 0, tt)
    avant <- list(E = E, F = F)  # (les résidus avant cette composante : la validation avec les centres et poids du modèle complet part d'eux)
    E <- retirer_composante(E, X$M, t0, comp$p, plein_x)
    res$T <- cbind(res$T, tt)
    res$P <- cbind(res$P, comp$p)
    res$ssxv <- cbind(res$ssxv, colSums(E^2))
    if (pls) {
      F <- residus_sans_x(retirer_composante(F, Y$M, t0, comp$c, plein_y), avec_x, version)
      res$W <- cbind(res$W, comp$w)
      res$C <- cbind(res$C, comp$c)
      u <- comp$u
      u[!avec_x] <- NA
      res$U <- cbind(res$U, u)
      res$ssyv <- cbind(res$ssyv, colSums(F[avec_x, , drop = FALSE]^2))
    }
    if (!is.null(vc)) {  # (PRESS et SS jusqu'à cette composante, pour `arret`)
      vc <- composante_validation(vc, a, avant$E, avant$F, comp)
      res$press <- vc$press
      res$ss_vc <- ss_validation(vc, a)
    }
    if (!is.null(arret) && arret(res, a)) break
  }
  c(res[setdiff(names(res), c("press", "ss_vc"))], if (!is.null(vc)) fin_validation(vc, ncol(res$T)),
    list(ssx = colSums(res$ssxv), ssy = if (pls) colSums(res$ssyv), residus_x = E, residus_y = F, avec_x = avec_x),
    if (etat) list(etat_auto = list(vc = vc, precedent = avant)))
}

# Le modèle `m` d'un ajustement automatique (calculer_modele avec etat = TRUE : ses composantes
# jusqu'à celle qui l'a arrêté) réduit à ses A premières composantes : ce que calculer_modele rend
# pour A composantes, sans le refaire. NIPALS calcule les composantes l'une après l'autre et la
# validation croisée avance avec elles : celles d'un modèle de A composantes sont les A premières, aux
# mêmes valeurs (fin_validation les relit pour A) ; ses résidus sont ceux d'après sa dernière
# composante (ceux du modèle, ou ceux d'avant la composante de plus). NULL quand ce n'est pas
# possible (une ACP ou une PLS : A ou A + 1 composantes calculées). Une OPLS calcule toutes ses
# composantes orthogonales d'un coup : le modèle d'autant ou de moins est refait par ajuster_opls
# depuis les noyaux qu'il garde (opls.R : noyau_reduit), sans NIPALS.
modele_tronque <- function(m, A) {
  e <- m$etat_auto
  a <- ncol(m$T)
  entete <- unclass(m)[seq_len(match("T", names(m)) - 1)]
  if (isTRUE(m$opls)) {
    Ap <- m$Ap %||% 1
    if (is.null(e$vc) || is.null(e$noyaux) || !isTRUE(A >= Ap && A <= a)) return(NULL)
    res <- ajuster_opls(list(E = m$X, M = m$MX), list(E = m$Y, M = m$MY), A - Ap, Ap, e$vc, noyaux = e$noyaux)
    entete$A <- ncol(res$T)
    modele <- c(entete, res)
    class(modele) <- "modele_nuee"
    return(modele)
  }
  if (is.null(e$vc) || !isTRUE(A >= 1 && (A == a || A == a - 1))) return(NULL)
  pls <- identical(m$type, "PLS")
  colonnes <- function(M, n) if (!is.null(M)) M[, seq_len(n), drop = FALSE]
  res <- list(T = colonnes(m$T, A), P = colonnes(m$P, A), W = colonnes(m$W, A), C = colonnes(m$C, A), U = colonnes(m$U, A),
              ssxv = colonnes(m$ssxv, A + 1), ssyv = colonnes(m$ssyv, A + 1))
  residus <- if (A == a) list(E = m$residus_x, F = m$residus_y) else e$precedent
  res <- c(res, fin_validation(e$vc, A),
           list(ssx = colSums(res$ssxv), ssy = if (pls) colSums(res$ssyv), residus_x = residus$E, residus_y = residus$F, avec_x = m$avec_x))
  entete$A <- ncol(res$T)
  modele <- c(entete, res)
  class(modele) <- "modele_nuee"
  modele
}

# L'erreur d'un modèle qui ne peut pas avoir sa composante a (classe « trop_de_composantes »,
# que hierarchique.R reconnaît : un modèle de base sans variation dans un tour).
erreur_composantes <- function(a) {
  structure(class = c("trop_de_composantes", "error", "condition"), list(message = trop_de_composantes(a), call = NULL))
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
