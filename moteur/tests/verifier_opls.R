# Vérifie l'OPLS (moteur/R/opls.R) contre une seconde écriture de l'algorithme de l'article
# (Trygg et Wold, 2002, J. Chemometrics 16:119-128, OPLS pour plusieurs Y), en calcul matriciel
# simple, sur des données inventées sans valeur manquante : un Y, deux Y, trois classes.
#   Rscript moteur/tests/verifier_opls.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}

# Une composante PLS (NIPALS) de E pour F.
pls_article <- function(E, F) {
  u <- F[, which.max(colSums(F^2))]
  repeat {
    w <- crossprod(E, u) / sum(u^2)
    w <- w / sqrt(sum(w^2))
    t <- E %*% w
    c <- crossprod(F, t) / sum(t^2)
    u2 <- F %*% c / sum(c^2)
    if (sqrt(sum((u2 - u)^2)) <= 1e-13 * sqrt(sum(u2^2))) break
    u <- u2
  }
  list(w = drop(w), t = drop(t), c = drop(c), p = drop(crossprod(E, t) / sum(t^2)))
}

# L'algorithme de l'article, étape par étape.
opls_article <- function(X, Y, Ao, Ap) {
  W <- sweep(crossprod(X, Y), 2, colSums(Y^2), "/")         # 2-3. un poids w par colonne de Y
  s <- svd(W)                                                 # 4. ACP de W (sans centrage),
  r <- sum(s$d^2 / sum(s$d^2) > 1e-10)                        #    jusqu'à un reste négligeable
  Tw <- s$u[, seq_len(r), drop = FALSE] %*% diag(s$d[seq_len(r)], r)
  E <- X
  To <- NULL
  for (i in seq_len(Ao)) {
    p <- pls_article(E, Y)$p                                  # 5-6. loading p de la composante PLS
    for (j in seq_len(r)) p <- p - sum(Tw[, j] * p) / sum(Tw[, j]^2) * Tw[, j]  # 7. p orthogonal à W
    wo <- p / sqrt(sum(p^2))                                  # 8-9.
    to <- drop(E %*% wo)                                      # 10.
    po <- drop(crossprod(E, to) / sum(to^2))                  # 11.
    E <- E - outer(to, po)                                    # 12. X filtré
    To <- cbind(To, to)
  }
  Tp <- NULL                                                  # 15. PLS de X filtré
  F <- Y
  for (a in seq_len(Ap)) {
    k <- pls_article(E, F)
    E <- E - outer(k$t, k$p)
    F <- F - outer(k$t, k$c)
    Tp <- cbind(Tp, k$t)
  }
  list(Tp = Tp, To = To)
}

# Même résultat au signe près, colonne par colonne.
egaux <- function(A, B) all(vapply(seq_len(ncol(A)), function(j) min(max(abs(A[, j] - B[, j])), max(abs(A[, j] + B[, j]))), 0) < 1e-8 * max(abs(A)))
reduire <- function(M) scale(M)[, , drop = FALSE]

set.seed(20261101)
N <- 40
K <- 15
latents <- matrix(rnorm(N * 4), N, 4)
X <- reduire(latents %*% matrix(rnorm(4 * K), 4, K) + matrix(rnorm(N * K, sd = 0.3), N, K))
Y2 <- reduire(latents[, 1:2] %*% matrix(c(1, 0.5, -0.3, 1), 2, 2) + matrix(rnorm(N * 2, sd = 0.2), N, 2))
classes <- factor(rep(c("a", "b", "c"), length.out = N))
Y3 <- reduire(1 * outer(as.integer(classes), 1:3, `==`))

for (cas in list(list(nom = "un Y", Y = Y2[, 1, drop = FALSE], Ap = 1, Ao = 3),
                 list(nom = "deux Y", Y = Y2, Ap = 2, Ao = 3),
                 list(nom = "trois classes", Y = Y3, Ap = 2, Ao = 2))) {
  cat(cas$nom, "\n")
  m <- ajuster_opls(preparer(X), preparer(cas$Y), cas$Ao, 7, cas$Ap)
  a <- opls_article(X, cas$Y, cas$Ao, cas$Ap)
  To <- m$T[, cas$Ap + seq_len(cas$Ao), drop = FALSE]
  verifier("scores orthogonaux to = article", egaux(To, a$To))
  verifier("scores prédictifs t = article", egaux(m$T[, seq_len(cas$Ap), drop = FALSE], a$Tp))
  verifier("to sans lien avec Y : to'Y = 0", max(abs(crossprod(To, cas$Y))) < 1e-10 * sqrt(sum(To^2) * sum(cas$Y^2)))
  verifier("scores orthogonaux entre eux", max(abs(crossprod(m$T)[upper.tri(diag(ncol(m$T)))])) < 1e-10 * sum(m$T^2))
}

if (echecs) {
  cat(echecs, "vérification(s) en échec\n")
  quit(status = 1)
}
cat("Tout est bon.\n")
