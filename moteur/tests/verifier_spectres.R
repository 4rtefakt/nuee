# Vérifie le prétraitement des spectres (moteur/R/spectres.R) :
#   - les coefficients de Savitzky–Golay publiés (fenêtre de 5, degré 2) ;
#   - un polynôme de degré ≤ 2 est lissé et dérivé exactement, bords compris ;
#   - SNV et MSC ; s'il est installé, les mêmes résultats que le paquet prospectr ;
#   - un modèle PLS sur des spectres inventés : la prédiction des observations du modèle rend
#     leurs scores, le script R refait la même définition, le projet enregistré se relit.
#   Rscript moteur/tests/verifier_spectres.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-10) isTRUE(all.equal(unname(a), unname(b), tolerance = tol, check.attributes = FALSE))

cat("Savitzky–Golay\n")
verifier("lissage 5 points, degré 2 : (-3, 12, 17, 12, -3) / 35", proche(coefficients_sg(5, 2, 0), c(-3, 12, 17, 12, -3) / 35))
verifier("dérivée 5 points, degré 2 : (-2, -1, 0, 1, 2) / 10", proche(coefficients_sg(5, 2, 1), c(-2, -1, 0, 1, 2) / 10))
j <- 1:30
X <- rbind(j^2, 3 - 2 * j, 0.5 * j^2 + j)
verifier("lissage exact d'un polynôme de degré 2", proche(savitzky_golay(X, 7, 2, 0), X))
verifier("dérivée première exacte, bords compris", proche(savitzky_golay(X, 7, 2, 1), rbind(2 * j, rep(-2, 30), j + 1)))
verifier("dérivée seconde exacte", proche(savitzky_golay(X, 5, 2, 2), rbind(rep(2, 30), rep(0, 30), rep(1, 30))))
verifier("fenêtre plus large que le spectre refusée", inherits(try(savitzky_golay(X[, 1:4], 5, 2, 0), silent = TRUE), "try-error"))

cat("SNV, MSC\n")
s <- snv(X)
verifier("SNV : chaque spectre de moyenne 0 et d'écart-type 1", proche(rowMeans(s), rep(0, 3)) && proche(apply(s, 1, sd), rep(1, 3)))
r <- sin(j / 5) + 2
verifier("MSC : a + b r redevient r", proche(msc(rbind(1 + 3 * r, -2 + 0.5 * r), r), rbind(r, r)))

if (requireNamespace("prospectr", quietly = TRUE)) {  # s'il est installé : mêmes résultats que prospectr
  cat("Comparaison avec le paquet prospectr\n")
  set.seed(7)
  Z <- t(vapply(1:20, function(i) runif(1, 0.5, 1.5) * sin(seq(0, 6, length.out = 80) + runif(1)) + runif(1) + rnorm(80, 0, 0.01), numeric(80)))
  verifier("SNV", proche(snv(Z), prospectr::standardNormalVariate(Z), 1e-12))
  verifier("MSC sur le spectre moyen", proche(msc(Z, colMeans(Z)), prospectr::msc(Z, ref_spectrum = colMeans(Z)), 1e-12))
  for (cas in list(c(11, 2, 0), c(11, 2, 1), c(15, 3, 2), c(5, 3, 3))) {
    h <- (cas[1] - 1) / 2
    verifier(sprintf("Savitzky–Golay fenêtre %d, degré %d, dérivée %d (hors bords, que prospectr retire)", cas[1], cas[2], cas[3]),
             proche(savitzky_golay(Z, cas[1], cas[2], cas[3])[, (h + 1):(80 - h)], prospectr::savitzkyGolay(Z, m = cas[3], p = cas[2], w = cas[1]), 1e-12))
  }
}

cat("Modèle sur des spectres inventés\n")
set.seed(3)
n <- 60; K <- 120
onde <- seq(1000, 2190, length.out = K)
bande <- function(c, l) exp(-((onde - c) / l)^2)
y <- runif(n, 5, 15)
spectres <- t(vapply(seq_len(n), function(i) (0.6 + 0.4 * runif(1)) * (y[i] * bande(1450, 40) + 8 * bande(1900, 60) + runif(1, 2, 6) * bande(1700, 120)) +
                       runif(1, -1, 1) + rnorm(K, 0, 0.02), numeric(K)))
df <- data.frame(echantillon = sprintf("E%02d", seq_len(n)), y = round(y, 2), round(spectres, 5), check.names = FALSE)
names(df)[-(1:2)] <- sprintf("%.0f nm", onde)
p <- projet_depuis_tableau(df, "spectres")
uid <- function(nom) as.numeric(names(p$noms_var)[p$noms_var == nom])
x <- vapply(names(df)[-(1:2)], uid, 0)
etapes <- list(list(type = "msc"), list(type = "sg", derivee = 1, fenetre = 11, degre = 2))
d <- definir_modele(p, "PLS", x = rev(x), y = uid("y"), echelle = "Ctr", A = 0, spectres = etapes)
verifier("les variables X sont prises dans l'ordre du tableau", identical(vapply(d$x, function(t) as.numeric(t$variables), 0), unname(x)))
d$A <- autoajuster(p, d)
m <- ajuster(p, d)
brut <- ajuster(p, { d0 <- d; d0$spectres <- NULL; d0 })
verifier(sprintf("le prétraitement améliore la prédiction (Q2 %.3f contre %.3f sans)", m$statistiques$composantes$Q2cum[m$A],
                 brut$statistiques$composantes$Q2cum[brut$A]),
         m$statistiques$composantes$Q2cum[m$A] > brut$statistiques$composantes$Q2cum[brut$A])
pr <- predire(p, m, d, d$observations)
verifier("prédire les observations du modèle rend leurs scores (référence MSC gardée)", proche(as.matrix(pr[paste0("t", seq_len(m$A))]), m$T, 1e-8))
e <- new.env()
assign("p", p, envir = e)
eval(parse(text = code_definition(p, d, "p")), envir = e)
verifier("le code R de la définition refait les mêmes étapes et variables", identical(e$d$spectres, d$spectres) && identical(e$d$x, d$x))
f <- tempfile(fileext = ".json")
d$app <- TRUE
p$models <- list(d)
enregistrer_projet(p, f)
q <- lire_projet(f)
m2 <- ajuster(q, q$models[[1]])
verifier("le modèle enregistré avec le projet se recalcule à l'identique", proche(m2$T, m$T, 1e-12))
s <- spectres_modele(p, d)
verifier("spectres du modèle : bruts et prétraités", all(dim(s$bruts) == c(n, K)) && !proche(s$bruts, s$pretraites))
unlink(f)

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
