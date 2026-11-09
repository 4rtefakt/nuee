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
projeter <- function(E, M, v) {
  vv <- sum(v^2)
  present <- as.vector(M %*% (v^2))
  correction <- pmin(vv / pmax(present, 1e-300), PLAFOND_CORRECTION)
  as.vector(E %*% v) / vv * correction
}

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
# fois celle de départ (`depart` : celle de la matrice avant la 1re composante) ?
verifier_variation <- function(E, depart) if (!isTRUE(sum(E^2) > SEUIL_VARIATION * depart)) sans_variation()

# Une composante d'ACP : scores t et loadings p (de norme 1). `depart` : la somme des carrés de X
# avant la 1re composante (par défaut celle de E : seuls des résidus tous nuls sont refusés).
composante_acp <- function(E, ME, depart = sum(E^2)) {
  verifier_variation(E, depart)
  Et <- t(E)
  MEt <- t(ME)
  tt <- E[, which.max(colSums(E^2))]
  for (i in seq_len(ITERATIONS_MAX)) {
    p <- projeter(Et, MEt, tt)
    p <- p / norme(p)
    nouveau <- projeter(E, ME, p)
    fini <- norme(nouveau - tt) <= TOLERANCE * norme(nouveau)
    tt <- nouveau
    if (fini) break
  }
  list(t = tt, p = projeter(Et, MEt, tt), iterations = i)
}

# Une composante de PLS : poids w (norme 1), scores t, poids de Y c, scores de Y u, loadings p.
# `depart` : les sommes des carrés de X et de Y avant la 1re composante.
composante_pls <- function(E, ME, F, MF, depart = c(sum(E^2), sum(F^2))) {
  verifier_variation(E, depart[1])
  verifier_variation(F, depart[2])
  Et <- t(E)
  MEt <- t(ME)
  Ft <- t(F)
  MFt <- t(MF)
  u <- F[, which.max(colSums(F^2))]
  for (i in seq_len(ITERATIONS_MAX)) {
    w <- projeter(Et, MEt, u)
    if (!isTRUE(norme(w) > 0)) sans_variation()  # X sans aucun lien avec ce qui reste de Y : 0 / 0
    w <- w / norme(w)
    tt <- projeter(E, ME, w)
    cc <- projeter(Ft, MFt, tt)
    nouveau <- projeter(F, MF, cc)
    fini <- norme(nouveau - u) <= TOLERANCE * norme(nouveau)
    u <- nouveau
    if (fini) break
  }
  list(t = tt, p = projeter(Et, MEt, tt), w = w, c = cc, u = u, iterations = i)
}

# Préparation d'une matrice : valeurs manquantes à 0 et masque des valeurs présentes.
preparer <- function(X) {
  M <- 1 * !is.na(X)
  X[is.na(X)] <- 0
  list(E = X, M = M)
}
