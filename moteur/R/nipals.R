# Algorithme NIPALS pour l'ACP et la PLS, avec données manquantes.
#
# Les cases manquantes sont ignorées dans chaque projection : par exemple le score d'une
# observation est  t_i = somme_k(x_ik * p_k) / somme_k(p_k^2)  sur ses seules variables
# présentes. Le facteur de correction  somme(p^2) / somme_présentes(p^2)  est limité à 3.
#
# Convention : E (résidus de X) et F (résidus de Y) ont leurs cases manquantes à 0, et les
# masques ME / MF valent 1 là où la donnée existe, 0 sinon.

PLAFOND_CORRECTION <- 3
TOLERANCE <- 1e-10
ITERATIONS_MAX <- 2000
# Ce qui reste de X (ou de Y) sous cette part de sa somme des carrés de départ n'est fait que
# d'erreurs d'arrondi : une composante de plus ne décrirait qu'elles (une ACP montrerait des
# composantes de R2X 1e-32, une PLS mourrait sur une matrice singulière). Un modèle ne peut donc
# pas avoir plus de composantes que le rang de ses données.
SEUIL_VARIATION <- 1e-12

# Le NIPALS du logiciel des fichiers .usp (ses versions 10, 11 et 13 et après font de même) :
#   - il s'arrête quand le vecteur qui itère (u pour une PLS, t pour une ACP) change de moins de
#     TOLERANCE_FICHIER en relatif, ou après ITERATIONS_FICHIER itérations ;
#   - il part de la première colonne de plus grande somme des carrés (de Y pour une PLS, de X pour
#     une ACP), à EGALITE_DEPART près en relatif : les colonnes de Y mises à l'échelle UV sans case
#     manquante ont toutes N - 1, aux arrondis près, et il prend la première ;
#   - la validation croisée de chaque composante, sur les résidus du modèle complet, part du poids
#     w de cette composante dans le modèle complet (validation_croisee.R : validation_par_composante) ;
#   - la version 10 donne à un terme X sans aucune valeur les w et p du terme d'avant (reporter_vides).
# Vérifié sur les W, P, T, C et U enregistrés : chaque composante (après celles du fichier) est
# une itérée de NIPALS, aux arrondis du fichier près (1e-7), et l'arrêt suit ce seuil sur les 812
# composantes de PLS et les 474 d'ACP du corpus d'essai (copies comprises) où l'itération d'arrêt
# se distingue, sauf quelques-unes dont le critère est à 0,3 % de 1e-5 (le bruit des arrondis) ; les
# tours de validation croisée enregistrés (cvW) sont redonnés de même. Quand NIPALS converge vite,
# ces règles ne changent rien au-delà de 1e-5. Avec des données manquantes, il peut converger très
# lentement, ou ne pas converger (le w de touspassereaux M8, composante 2, est celui de la 200e
# itération), ou avoir plusieurs solutions (données M7, composante 1 : le départ décide de celle
# qu'il trouve). Un modèle venant d'un fichier (sa version d'origine, `version`, notée par le
# convertisseur ; ceux qu'on en dérive la gardent) suit donc ces règles, pour redonner ses
# résultats ; ceux de Nuée gardent les siennes (TOLERANCE, ITERATIONS_MAX, which.max : plus
# précises). Les observations sans aucune valeur X : residus_sans_x.
TOLERANCE_FICHIER <- 1e-5
ITERATIONS_FICHIER <- 200
EGALITE_DEPART <- 1e-9

# La colonne de départ de NIPALS parmi des sommes des carrés `ss` : la plus grande ; pour un modèle
# d'un fichier (version non NULL), la première de celles qui l'égalent à EGALITE_DEPART près.
colonne_depart <- function(ss, version = NULL) {
  if (is.null(version)) which.max(ss) else which(ss >= max(ss) * (1 - EGALITE_DEPART))[1]
}

# Les termes X sans aucune valeur (`vides`, TRUE pour chacun) dans un poids ou un loading `v`
# calculé par projeter_colonnes : Nuée leur donne 0 ; la version 10 du logiciel des fichiers, la
# valeur du terme d'avant (un terme vide en tête garde 0). Son w, normé avec ces valeurs, est un
# peu plus court sur les autres termes : les scores et l'ajustement n'en changent pas, mais W, P,
# les VIP et les coefficients si (resultats_formations M13 : ESPPAT, manquant pour toutes les
# formations du modèle, a les w et p de ANNÉE(98)).
reporter_vides <- function(v, vides) {
  for (k in which(vides)) if (k > 1) v[k] <- v[k - 1]
  v
}

# Les termes X à reporter (reporter_vides) pour un modèle de cette version d'origine : ceux qui n'ont
# aucune valeur dans le masque M (version 10) ; NULL sinon.
termes_vides <- function(M, version) {
  if (!isTRUE(version == 10) || masque_plein(M)) return(NULL)
  vides <- colSums(M) == 0
  if (any(vides)) vides
}

# Projection de chaque ligne de E sur le vecteur v, en ignorant les cases manquantes.
# `plein` : le masque M n'a aucune case manquante (masque_plein). La somme des v^2 des cases
# présentes est alors la même pour chaque ligne, et M %*% v^2 la calcule dans l'ordre de BLAS
# (en double, terme à terme, que sum() ne suit pas : il cumule en précision étendue) : elle est
# calculée une fois, dans ce même ordre (somme_blas), plutôt que par un produit de plus avec M.
projeter <- function(E, M, v, plein = FALSE) {
  v2 <- v^2
  vv <- sum(v2)
  present <- if (plein) somme_blas(v2) else as.vector(M %*% v2)
  correction <- pmin(vv / pmax(present, 1e-300), PLAFOND_CORRECTION)
  as.vector(E %*% v) / vv * correction
}

# La projection de chaque colonne de E sur le vecteur u (une valeur par ligne de E) : ce que donne
# projeter(t(E), t(M), u), sans transposer E ni M (crossprod fait les mêmes produits, dans le même
# ordre).
projeter_colonnes <- function(E, M, u, plein = FALSE) {
  u2 <- u^2
  uu <- sum(u2)
  present <- if (plein) somme_blas(u2) else as.vector(crossprod(M, u2))
  correction <- pmin(uu / pmax(present, 1e-300), PLAFOND_CORRECTION)
  as.vector(crossprod(E, u)) / uu * correction
}

# La somme des éléments de x dans l'ordre d'un produit de matrices de BLAS (en double, du premier
# au dernier) : celle de chaque ligne de M %*% x quand M n'a que des 1.
somme_blas <- function(x) as.vector(crossprod(x, rep(1, length(x))))

# Le masque M (1 là où la donnée existe, 0 sinon) n'a-t-il aucune case manquante ? Alors
# multiplier par M ne change rien, et les projections se passent de lui (projeter).
masque_plein <- function(M) !length(M) || min(M) == 1

norme <- function(v) sqrt(sum(v^2))

# Plus rien à décrire : les résidus de X (ou de Y, pour une PLS) n'ont plus de variation, et une
# composante de plus serait faite de 0 / 0 ou d'erreurs d'arrondi. Erreur de classe
# « sans_variation », qu'ajuster_matrices() complète par le numéro de la composante.
sans_variation <- function() {
  stop(structure(class = c("sans_variation", "error", "condition"),
                 list(message = traduire("Il ne reste plus de variation à décrire dans les données.",
                                         "There is no variation left to describe in the data."), call = NULL)))
}

# Les résidus E ont-ils encore de la variation : une somme des carrés au-dessus de SEUIL_VARIATION
# fois celle de départ (`depart` : celle de la matrice avant la 1re composante) ? Rend cette somme.
verifier_variation <- function(E, depart) {
  s <- sum(E^2)
  if (!isTRUE(s > SEUIL_VARIATION * depart)) sans_variation()
  invisible(s)
}

# Avant chaque produit de matrices, R vérifie qu'elles n'ont ni NaN ni infini (que BLAS ne propage
# pas toujours) : un passage sur toute la matrice, qui coûte à peu près autant que le produit d'une
# matrice par un vecteur. Les résidus d'une composante, dont la somme des carrés est finie
# (`finies`, verifier_variation), n'en ont pas, ni les vecteurs qui en viennent : le temps de ses
# itérations, R appelle BLAS sans vérifier, ce qui fait exactement le même calcul. Seulement quand R
# est à son réglage par défaut (matprod "default", qui appelle alors BLAS) ; rend le réglage à
# remettre ensuite (options(...)).
produits_sans_verification <- function(finies) {
  if (isTRUE(all(finies)) && identical(getOption("matprod"), "default")) options(matprod = "blas") else list()
}

# Une composante d'ACP : scores t et loadings p (de norme 1). `depart` : la somme des carrés de X
# avant la 1re composante (par défaut celle de E : seuls des résidus tous nuls sont refusés).
# `version` : celle du fichier .usp d'où vient le modèle, qui suit alors le NIPALS de son logiciel
# (TOLERANCE_FICHIER ; NULL : celui de Nuée). Le loading rendu est alors celui de la dernière
# itération, dont viennent les scores (le logiciel du fichier l'enregistre ; pour Nuée, la
# projection de E sur ses scores, la même à TOLERANCE près).
composante_acp <- function(E, ME, depart = sum(E^2), version = NULL) {
  ancien <- produits_sans_verification(is.finite(verifier_variation(E, depart)))
  on.exit(options(ancien), add = TRUE)
  # (E transposée une fois : une ACP fait souvent des centaines d'itérations, et le produit d'une
  # matrice transposée par un vecteur est un peu plus rapide que crossprod ; sans case manquante,
  # le masque ne sert pas : projeter)
  plein <- masque_plein(ME)
  Et <- t(E)
  MEt <- if (!plein) t(ME)
  fichier <- !is.null(version)
  vides <- termes_vides(ME, version)
  tt <- E[, colonne_depart(colSums(E^2), version)]
  for (i in seq_len(if (fichier) ITERATIONS_FICHIER else ITERATIONS_MAX)) {
    p <- projeter(Et, MEt, tt, plein)
    if (!is.null(vides)) p <- reporter_vides(p, vides)
    p <- p / norme(p)
    nouveau <- projeter(E, ME, p, plein)
    fini <- norme(nouveau - tt) <= (if (fichier) TOLERANCE_FICHIER else TOLERANCE) * norme(nouveau)
    tt <- nouveau
    if (fini) break
  }
  list(t = tt, p = if (fichier) p else projeter(Et, MEt, tt, plein), iterations = i)
}

# Une composante de PLS : poids w (norme 1), scores t, poids de Y c, scores de Y u, loadings p.
# `depart` : les sommes des carrés de X et de Y avant la 1re composante. `version` : celle du
# fichier .usp d'où vient le modèle, qui suit alors le NIPALS de son logiciel (TOLERANCE_FICHIER ;
# NULL : celui de Nuée). `w0` : un poids de départ (celui du modèle complet, pour un tour de sa
# validation croisée : validation_par_composante), d'où viennent t, c et le premier u ; sinon u
# part d'une colonne de F (colonne_depart).
composante_pls <- function(E, ME, F, MF, depart = c(sum(E^2), sum(F^2)), version = NULL, w0 = NULL) {
  sommes <- c(verifier_variation(E, depart[1]), verifier_variation(F, depart[2]))
  ancien <- produits_sans_verification(is.finite(sommes))
  on.exit(options(ancien), add = TRUE)
  # (quelques itérations seulement : projeter_colonnes plutôt que de transposer E, M, F et MF)
  plein_x <- masque_plein(ME)
  plein_y <- masque_plein(MF)
  fichier <- !is.null(version)
  vides <- termes_vides(ME, version)
  u <- if (!is.null(w0)) projeter(F, MF, projeter_colonnes(F, MF, projeter(E, ME, w0, plein_x), plein_y), plein_y)
       else F[, colonne_depart(colSums(F^2), version)]
  for (i in seq_len(if (fichier) ITERATIONS_FICHIER else ITERATIONS_MAX)) {
    w <- projeter_colonnes(E, ME, u, plein_x)
    if (!is.null(vides)) w <- reporter_vides(w, vides)
    if (!isTRUE(norme(w) > 0)) sans_variation()  # X sans aucun lien avec ce qui reste de Y : 0 / 0
    w <- w / norme(w)
    tt <- projeter(E, ME, w, plein_x)
    cc <- projeter_colonnes(F, MF, tt, plein_y)
    nouveau <- projeter(F, MF, cc, plein_y)
    fini <- norme(nouveau - u) <= (if (fichier) TOLERANCE_FICHIER else TOLERANCE) * norme(nouveau)
    u <- nouveau
    if (fini) break
  }
  p <- projeter_colonnes(E, ME, tt, plein_x)
  list(t = tt, p = if (!is.null(vides)) reporter_vides(p, vides) else p, w = w, c = cc, u = u, iterations = i)
}

# Les résidus F de Y après une composante, pour un modèle d'un fichier .usp (`version` non NULL) :
# son logiciel compte les observations sans aucune valeur X (`avec_x` FALSE) comme parfaitement
# ajustées après la 1re composante (leurs résidus valent 0 ; le R2Y de Nuée les compte ainsi pour
# tout modèle : ajuster_matrices, ssyv), et fait les composantes suivantes sans elles : leur U
# enregistré manque dès la 2e composante, et les w enregistrés ne sont redonnés qu'ainsi
# (oppg_pr_2009, 21 modèles de la version 11), comme le Q2 de la version 13 (analdensitecomplet
# M9). Un modèle de Nuée garde leurs résidus.
residus_sans_x <- function(F, avec_x, version) {
  if (!is.null(version) && !all(avec_x)) F[!avec_x, ] <- 0
  F
}

# Les résidus E (masque M) moins la composante de scores t et de loadings p : (E - t p') * M,
# sans la multiplication quand M n'a que des 1 (`plein`, masque_plein ; elle ne change alors rien).
retirer_composante <- function(E, M, t, p, plein = masque_plein(M)) {
  # (en une expression : R écrit chaque résultat dans la mémoire du précédent, qu'il n'a pas nommé)
  if (plein) E - outer(t, p) else (E - outer(t, p)) * M
}

# Préparation d'une matrice : valeurs manquantes à 0 et masque des valeurs présentes. Sans valeur
# manquante, la matrice telle quelle et un masque de 1 (les mêmes, sans les copies de is.na).
preparer <- function(X) {
  if (!anyNA(X)) return(list(E = X, M = array(1, dim(X), dimnames(X))))
  M <- 1 * !is.na(X)
  X[is.na(X)] <- 0
  list(E = X, M = M)
}
