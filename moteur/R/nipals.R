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
composante_acp <- function(E, ME, depart = sum(E^2)) {
  ancien <- produits_sans_verification(is.finite(verifier_variation(E, depart)))
  on.exit(options(ancien), add = TRUE)
  # (E transposée une fois : une ACP fait souvent des centaines d'itérations, et le produit d'une
  # matrice transposée par un vecteur est un peu plus rapide que crossprod ; sans case manquante,
  # le masque ne sert pas : projeter)
  plein <- masque_plein(ME)
  Et <- t(E)
  MEt <- if (!plein) t(ME)
  tt <- E[, which.max(colSums(E^2))]
  for (i in seq_len(ITERATIONS_MAX)) {
    p <- projeter(Et, MEt, tt, plein)
    p <- p / norme(p)
    nouveau <- projeter(E, ME, p, plein)
    fini <- norme(nouveau - tt) <= TOLERANCE * norme(nouveau)
    tt <- nouveau
    if (fini) break
  }
  list(t = tt, p = projeter(Et, MEt, tt, plein), iterations = i)
}

# Une composante de PLS : poids w (norme 1), scores t, poids de Y c, scores de Y u, loadings p.
# `depart` : les sommes des carrés de X et de Y avant la 1re composante.
composante_pls <- function(E, ME, F, MF, depart = c(sum(E^2), sum(F^2))) {
  sommes <- c(verifier_variation(E, depart[1]), verifier_variation(F, depart[2]))
  ancien <- produits_sans_verification(is.finite(sommes))
  on.exit(options(ancien), add = TRUE)
  # (quelques itérations seulement : projeter_colonnes plutôt que de transposer E, M, F et MF)
  plein_x <- masque_plein(ME)
  plein_y <- masque_plein(MF)
  u <- F[, which.max(colSums(F^2))]
  for (i in seq_len(ITERATIONS_MAX)) {
    w <- projeter_colonnes(E, ME, u, plein_x)
    if (!isTRUE(norme(w) > 0)) sans_variation()  # X sans aucun lien avec ce qui reste de Y : 0 / 0
    w <- w / norme(w)
    tt <- projeter(E, ME, w, plein_x)
    cc <- projeter_colonnes(F, MF, tt, plein_y)
    nouveau <- projeter(F, MF, cc, plein_y)
    fini <- norme(nouveau - u) <= TOLERANCE * norme(nouveau)
    u <- nouveau
    if (fini) break
  }
  list(t = tt, p = projeter_colonnes(E, ME, tt, plein_x), w = w, c = cc, u = u, iterations = i)
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
