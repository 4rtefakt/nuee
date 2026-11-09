# Vérifie la validation croisée (moteur/R/validation_croisee.R) : chaque groupe est prédit par un
# modèle ajusté sans lui, son prétraitement compris.
#   - elle redonne ce que donnent, à la main, un modèle ajusté sur les autres observations
#     (ajuster) et la prédiction des observations du groupe (predire) ;
#   - sur du bruit pur, le Q2 n'est plus gonflé quand les observations sont peu nombreuses
#     (médiane de Q2cum au plus 0, ACP, PLS et OPLS-DA, ajustement automatique ou A fixé) ; en
#     particulier une PLS de 6 observations à 5 composantes n'a plus un Q2 de 1 ;
#     (une OPLS-DA dont une classe est toute dans un groupe, absente de son sous-modèle, comprise) ;
#   - un modèle ne peut pas avoir plus de composantes que le rang de ses données ;
#   - un vrai signal garde un Q2 élevé (les quatre modèles du projet d'exemple) ;
#   - le test de permutation réajuste tout le modèle à partir du projet.
#   Rscript moteur/tests/verifier_validation.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}

p <- lire_projet("exemple/vergers.json")
uid <- function(nom) numeros(p, nom)
complets <- uid(c("poids", "diametre", "fermete", "sucre", "acidite", "amidon", "mat_seche", "couleur_a", "couleur_b", "azote", "stockage"))

cat("La validation croisée redonne : un modèle ajusté sans chaque groupe, qui prédit ce groupe (predire)\n")
# PRESS refait à la main, dans les unités du modèle complet (OPLS : sa dernière ligne, toutes les
# composantes orthogonales retirées comme pour une nouvelle observation)
press_a_la_main <- function(p, d) {
  m <- ajuster(p, d)
  obs <- d$observations
  groupes <- groupes_validation(length(obs), m$G)
  pls <- identical(m$type, "PLS")
  press <- matrix(0, m$A, if (pls) ncol(m$Y) else m$K)
  vers_complet <- function(z, centre, poids) sweep(sweep(z, 2, centre, "-"), 2, poids, "*")
  for (g in sort(unique(groupes))) {
    hors <- groupes == g
    dg <- d
    dg$observations <- obs[!hors]
    mg <- ajuster(p, dg, orienter = FALSE)
    if (pls) {
      for (a in if (isTRUE(m$opls)) m$A else seq_len(m$A)) {
        ma <- mg
        if (!isTRUE(m$opls)) {  # les a premières composantes seulement
          ma$A <- a
          for (k in c("T", "P", "W", "C", "U")) ma[[k]] <- mg[[k]][, seq_len(a), drop = FALSE]
        }
        y <- as.matrix(predire(p, ma, dg, obs[hors])[paste(m$y$noms, "prédit")])
        press[a, ] <- press[a, ] + colSums(((m$Y[hors, , drop = FALSE] - vers_complet(y, m$y$centre, m$y$poids)) * m$MY[hors, , drop = FALSE])^2)
      }
    } else {  # ACP : chaque groupe de termes manquant à son tour
      termes <- groupes_validation(m$K, m$G)
      for (h in sort(unique(termes))) {
        q <- p
        q$donnees[as.character(obs[hors]), as.character(unlist(lapply(d$x[termes == h], `[[`, "variables")))] <- NA
        Tq <- as.matrix(predire(q, mg, dg, obs[hors])[paste0("t", seq_len(m$A))])
        Tq[is.na(Tq)] <- 0
        k <- termes == h
        for (a in seq_len(m$A)) {
          x <- Tq[, seq_len(a), drop = FALSE] %*% t(mg$P[k, seq_len(a), drop = FALSE])
          x <- sweep(sweep(x, 2, ifelse(mg$x$poids[k] > 0, mg$x$poids[k], Inf), "/"), 2, mg$x$centre[k], "+")
          press[a, k] <- press[a, k] + colSums(((m$X[hors, k, drop = FALSE] - vers_complet(x, m$x$centre[k], m$x$poids[k])) * m$MX[hors, k, drop = FALSE])^2)
        }
      }
    }
  }
  lignes <- if (isTRUE(m$opls)) m$A else seq_len(m$A)
  max(abs(press[lignes, ] - m$press[lignes, ])) / max(abs(m$press[lignes, ]))
}
q <- p
q$donnees[c(5, 17, 40), as.character(uid("note_gout"))] <- NA
q$donnees[9, as.character(complets[1:4])] <- NA
q$donnees[c(11, 12), as.character(complets[5])] <- NA
set.seed(3)
onde <- seq(1000, 1600, length.out = 40)
y_s <- runif(30)
df <- data.frame(nom = sprintf("E%02d", 1:30), y = y_s, t(vapply(1:30, function(i) (1 + y_s[i] / 2) * sin(onde / 100 + i / 10) + y_s[i] * exp(-((onde - 1300) / 50)^2) +
                                                              rnorm(40, 0, 0.01), numeric(40))), check.names = FALSE)
names(df)[-(1:2)] <- sprintf("%.0f nm", onde)
s <- projet_depuis_tableau(df, "spectres")
# une classe dont toutes les observations (1 et 8) sont dans le groupe 1 : le sous-modèle sans ce
# groupe n'a pas cette classe (sa colonne de Y y est constante, de poids 0)
set.seed(7)
pk <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:15), classe = c("c", rep(c("a", "b"), 3), "c", rep(c("a", "b"), 3), "a"),
                                       matrix(rnorm(75), 15, 5, dimnames = list(NULL, paste0("V", 1:5))), check.names = FALSE), "classe_groupe")
cas <- list(
  "PLS" = list(p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 3)),
  "PLS à 2 Y, transformations et échelles mêlées" = list(p, definir_modele(p, "PLS", x = complets, y = uid(c("note_gout", "jus")), A = 3,
    transformations = stats::setNames(c("log10", "racine2"), as.character(complets[1:2])), echelle = stats::setNames(c("Par", "Ctr"), as.character(complets[3:4])))),
  "PLS, termes qualitatifs, croisés et carrés" = list(p, definir_modele(p, "PLS", x = c(uid(c("variete", "culture")), complets[1:5]), y = uid("note_gout"), A = 3,
    croises = list(c(uid("variete"), complets[4]), c(complets[4], complets[4])))),
  "PLS-DA" = list(p, definir_modele(p, "PLS-DA", x = complets, classe = uid("variete"), A = 3)),
  "PLS, valeurs manquantes" = list(q, definir_modele(q, "PLS", x = complets, y = uid("note_gout"), A = 3)),
  "PLS, spectres (MSC, dérivée)" = list(s, definir_modele(s, "PLS", x = numeros(s, names(df)[-(1:2)]), y = numeros(s, "y"), A = 2,
    spectres = list(list(type = "msc"), list(type = "sg", derivee = 1, fenetre = 5, degre = 2)))),
  "ACP" = list(p, definir_modele(p, "PCA", x = complets, A = 3)),
  "ACP, valeurs manquantes" = list(q, definir_modele(q, "PCA", x = complets[1:6], A = 2)),
  "OPLS" = list(p, definir_modele(p, "OPLS", x = complets, y = uid("note_gout"), A = 3)),
  "OPLS-DA à 4 classes" = list(p, definir_modele(p, "OPLS-DA", x = complets, classe = uid("variete"), A = 4)),
  "OPLS-DA à 3 classes, dont une toute dans un groupe" = list(pk, definir_modele(pk, "OPLS-DA", x = numeros(pk, paste0("V", 1:5)), classe = numeros(pk, "classe"), A = 3)))
for (k in names(cas)) {
  e <- press_a_la_main(cas[[k]][[1]], cas[[k]][[2]])
  verifier(sprintf("%s : PRESS du moteur = PRESS à la main (écart relatif %.1g)", k, e), e < 1e-10)
}

cat("Bruit pur : le Q2 n'est plus gonflé avec peu d'observations\n")
bruit <- function(N, graine, K = 20) {
  set.seed(graine)
  X <- matrix(rnorm(N * K), N, K, dimnames = list(NULL, paste0("X", seq_len(K))))
  df <- data.frame(nom = sprintf("O%02d", seq_len(N)), y = rnorm(N), classe = sample(rep(c("a", "b"), length.out = N)), X, check.names = FALSE)
  projet_depuis_tableau(df, "bruit")
}
q2_final <- function(pb, type, A) {
  x <- numeros(pb, paste0("X", 1:20))
  d <- switch(type, PCA = definir_modele(pb, "PCA", x = x, A = A), PLS = definir_modele(pb, "PLS", x = x, y = numeros(pb, "y"), A = A),
              "OPLS-DA" = definir_modele(pb, "OPLS-DA", x = x, classe = numeros(pb, "classe"), A = A))
  if (!d$A) d$A <- max(1, autoajuster(pb, d))  # comme l'application
  m <- ajuster(pb, d)
  c(A = m$A, Q2cum = m$q2cum[m$A])
}
tableau <- NULL
for (type in c("PCA", "PLS", "OPLS-DA")) for (N in c(6, 8, 12, 30)) for (A in c(0, 2)) {
  r <- t(vapply(1:20, function(graine) q2_final(bruit(N, 1000 * N + graine), type, A), c(A = 0, Q2cum = 0)))
  tableau <- rbind(tableau, data.frame(type = type, N = N, A = if (A) "A = 2" else "auto", A_median = stats::median(r[, "A"]),
                                       Q2cum_median = round(stats::median(r[, "Q2cum"]), 3), Q2cum_max = round(max(r[, "Q2cum"]), 3)))
}
print(tableau, row.names = FALSE)
for (type in unique(tableau$type)) {
  t <- tableau[tableau$type == type, ]
  verifier(sprintf("%s : médiane de Q2cum au plus 0 pour N = 6, 8, 12, 30, automatique et A = 2 (la plus haute : %.3f)", type, max(t$Q2cum_median)),
           all(t$Q2cum_median <= 0))
}
pb <- bruit(6, 6)
m <- ajuster(pb, definir_modele(pb, "PLS", x = numeros(pb, paste0("X", 1:20)), y = numeros(pb, "y"), A = 5))
verifier(sprintf("PLS de 6 observations sur du bruit, 5 composantes : Q2cum %.3f < 0 (il valait 1 quand les sous-modèles gardaient l'échelle du modèle complet)", m$q2cum[5]),
         m$q2cum[5] < 0)
verifier("ses sous-modèles (5 observations) n'ont que 4 composantes : les intervalles des coefficients sont manquants, le modèle s'ajuste",
         m$A == 5 && all(is.na(m$statistiques$coefficients[["CoeffCS y IC"]][seq_len(m$K)])) && all(is.finite(m$statistiques$coefficients[["CoeffCS y"]][seq_len(m$K)])))

cat("Pas plus de composantes que le rang des données\n")
set.seed(4)
pr <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:30), y = rnorm(30), matrix(rnorm(120), 30, 4, dimnames = list(NULL, paste0("V", 1:4))),
                                       check.names = FALSE), "rang")
x4 <- numeros(pr, paste0("V", 1:4))
message_5 <- trop_de_composantes(5)
for (type in c("PCA", "PLS")) {
  d <- if (type == "PCA") definir_modele(pr, "PCA", x = x4, A = 6) else definir_modele(pr, "PLS", x = x4, y = numeros(pr, "y"), A = 6)
  e <- tryCatch({ ajuster(pr, d); "ajusté" }, error = function(e) conditionMessage(e))
  verifier(sprintf("%s, 4 variables, 6 composantes demandées : « %s »", type, e), identical(e, message_5))
}
d <- definir_modele(pr, "PCA", x = x4, A = 4)
verifier("ACP, 4 variables, 4 composantes : s'ajuste", ajuster(pr, d)$A == 4)
# 5 termes X de rang 4 (V5 = V1) : l'ajustement automatique d'une OPLS-DA en essaie 5 (1 prédictive,
# 4 orthogonales) et s'arrête à ce que les données permettent
set.seed(5)
V <- matrix(rnorm(120), 30, 4, dimnames = list(NULL, paste0("V", 1:4)))
po <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:30), classe = rep(c("a", "b"), 15), V, V5 = V[, 1], check.names = FALSE), "rang")
do <- definir_modele(po, "OPLS-DA", x = numeros(po, paste0("V", 1:5)), classe = numeros(po, "classe"), A = 0)
ao <- autoajuster(po, do)
verifier(sprintf("OPLS-DA automatique, 5 termes X de rang 4 : %d composante(s), 4 au plus", ao), ao >= 1 && ao <= 4)
do$A <- 5
e <- tryCatch({ ajuster(po, do); "ajusté" }, error = function(e) conditionMessage(e))
verifier(sprintf("la même OPLS-DA à 5 composantes demandées : « %s »", e), identical(e, message_5))

cat("Un vrai signal garde un Q2 élevé : les modèles du projet d'exemple\n")
# avant : la validation croisée sur les matrices du modèle complet (version 1.0 jusqu'ici)
avant <- list(M1 = c(0.1716, 0.4035, 0.3481, 0.5278), M2 = c(0.6731, 0.6964, 0.7086, 0.7245), M3 = c(0.3048, 0.5850, 0.8946), M4 = c(0.5924, 0.7417))
for (d in p$models) {
  m <- ajuster(p, d)
  cp <- m$statistiques$composantes
  ancien <- avant[[d$name]]
  cat(sprintf("    %s (%s, A = %d) Q2cum par composante : %s ; avant : %s\n", d$name, paste0(if (m$opls) "O" else "", if (m$plsda) "PLS-DA" else m$type), m$A,
              paste(sprintf("%.3f", cp$Q2cum), collapse = " "), paste(sprintf("%.3f", ancien), collapse = " ")))
  verifier(sprintf("%s : Q2cum %.3f, au moins 0,6 et pas plus de 0,05 sous l'ancien au même nombre de composantes", d$name, cp$Q2cum[m$A]),
           cp$Q2cum[m$A] >= 0.6 && all(cp$Q2cum[seq_along(ancien)] >= ancien - 0.05))
}

cat("Test de permutation : tout le modèle réajusté à partir du projet\n")
d <- definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2)
m <- ajuster(p, d)
r <- permutations(p, m, d, n = 4)
verifier("le premier point est le modèle lui-même (R2, Q2)", isTRUE(all.equal(unname(unlist(r$points[1, c("R2", "Q2")])), c(m$statistiques$composantes$R2Ycum[2], m$q2cum[2]))))
verifier(sprintf("Y permuté : Q2 négatif (ordonnée à l'origine de Q2 %.2f < 0,05, celle de R2 %.2f < 0,4)", r$ordonnee_Q2, r$ordonnee_R2),
         all(r$points$Q2[-1] < 0) && r$ordonnee_Q2 < 0.05 && r$ordonnee_R2 < 0.4)
dd <- definir_modele(p, "PLS-DA", x = complets, classe = uid("variete"), A = 2)
rd <- permutations(p, ajuster(p, dd), dd, n = 2)
verifier("PLS-DA : les classes sont permutées ensemble et le modèle réajusté à chaque fois",
         nrow(rd$points) == 3 && all(rd$points$correlation[-1] < 1))

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
