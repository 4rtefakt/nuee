# Vérifie l'iPLS (moteur/R/ipls.R) sur des spectres inventés où seule une bande porte Y :
# les intervalles couvrent le spectre dans l'ordre, chaque RMSEcv est celui d'une PLS sur les
# colonnes de la zone dans le spectre prétraité en entier, le spectre entier redonne le modèle,
# et le meilleur intervalle est celui de la bande.
#   Rscript moteur/tests/verifier_ipls.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}

set.seed(7)
onde <- seq(1000, 1795, by = 5)  # 160 longueurs d'onde
n <- 40
y <- runif(n, 0, 10)
bande <- exp(-((onde - 1450) / 15)^2)  # une bande vers 1450 nm, dont la hauteur suit y
X <- t(vapply(seq_len(n), function(i) 0.5 + 0.002 * (onde - 1000) * runif(1, 0.8, 1.2) + 0.05 * y[i] * bande +
                rnorm(length(onde), 0, 0.004), numeric(length(onde))))
df <- data.frame(nom = sprintf("E%02d", seq_len(n)), y = y, X, check.names = FALSE)
names(df)[-(1:2)] <- sprintf("%.0f nm", onde)
p <- projet_depuis_tableau(df, "spectres")
x <- numeros(p, names(df)[-(1:2)])

d <- definir_modele(p, "PLS", x = x, y = numeros(p, "y"), A = 3)
r <- pls_par_intervalles(p, d, n = 16)
termes <- unlist(lapply(r$intervalles, function(z) z$termes))
tailles <- vapply(r$intervalles, function(z) length(z$termes), 0)
verifier("16 intervalles qui couvrent le spectre, dans l'ordre, de tailles égales à un près",
         length(r$intervalles) == 16 && identical(termes, names(df)[-(1:2)]) && max(tailles) - min(tailles) <= 1)
meilleur <- which.min(vapply(r$intervalles, function(z) min(z$rmsecv), 0))
dans <- function(z, nm) { a <- as.numeric(sub(" nm", "", z$termes)); min(a) <= nm && nm <= max(a) }
# le centre de la bande est entre deux intervalles (1400-1445 et 1450-1495 nm), presque à égalité
bande_au_bord <- function(z) dans(z, 1445) || dans(z, 1450)
verifier("le meilleur intervalle est un des deux qui se partagent le centre de la bande (1445-1450 nm)", bande_au_bord(r$intervalles[[meilleur]]))
loin <- Filter(function(z) { a <- as.numeric(sub(" nm", "", z$termes)); max(a) < 1400 || min(a) > 1500 }, r$intervalles)
verifier("il prédit bien mieux que les intervalles loin de la bande (hors 1400-1500 nm)",
         length(loin) >= 10 && min(r$intervalles[[meilleur]]$rmsecv) < 0.5 * min(vapply(loin, function(z) min(z$rmsecv), 0)))

# sans prétraitement : chaque RMSEcv est celui d'un modèle sur la zone seule
z <- r$intervalles[[5]]
m5 <- ajuster(p, definir_modele(p, "PLS", x = z$variables, y = numeros(p, "y"), A = 3))
verifier("RMSEcv d'un intervalle = celui du modèle sur cette zone (3 composantes)",
         isTRUE(all.equal(as.numeric(z$rmsecv)[3], unname(m5$statistiques$RMSEcv[1]), tolerance = 1e-12)))
m <- ajuster(p, d)
verifier("spectre entier : le RMSEcv du modèle, pour chaque nombre de composantes",
         isTRUE(all.equal(as.numeric(r$complet$rmsecv)[3], unname(m$statistiques$RMSEcv[1]), tolerance = 1e-12)) &&
           isTRUE(all.equal(as.numeric(r$complet$rmsecv)[2], unname(ajuster(p, modifyList(d, list(A = 2)))$statistiques$RMSEcv[1]), tolerance = 1e-12)))

# avec un prétraitement des spectres : fait une fois sur le spectre entier, puis coupé
etapes <- list(list(type = "snv"), list(type = "sg", derivee = 1, fenetre = 7, degre = 2))
d2 <- definir_modele(p, "PLS", x = x, y = numeros(p, "y"), A = 3, spectres = etapes)
r2 <- pls_par_intervalles(p, d2, n = 10)
s <- spectres_modele(p, d2)
q <- projet_depuis_tableau(data.frame(nom = df$nom, y = y, s$pretraites, check.names = FALSE), "prétraités")
z <- r2$intervalles[[3]]
m3 <- ajuster(q, definir_modele(q, "PLS", x = numeros(q, as.character(z$termes)), y = numeros(q, "y"), A = 3))
verifier("prétraitement (SNV, dérivée) : RMSEcv d'un intervalle = PLS sur ses colonnes du spectre prétraité en entier",
         isTRUE(all.equal(as.numeric(z$rmsecv)[3], unname(m3$statistiques$RMSEcv[1]), tolerance = 1e-12)))
verifier("prétraitement : le spectre entier redonne le RMSEcv du modèle",
         isTRUE(all.equal(as.numeric(r2$complet$rmsecv)[3], unname(ajuster(p, d2)$statistiques$RMSEcv[1]), tolerance = 1e-12)))
meilleur <- which.min(vapply(r2$intervalles, function(z) min(z$rmsecv), 0))
verifier("prétraitement : le meilleur intervalle reste celui de la bande", dans(r2$intervalles[[meilleur]], 1450))
verifier("spectre moyen prétraité, un point par variable", length(r2$moyen) == length(x) && all(is.finite(r2$moyen)))

# SNV et 20 intervalles étroits : la bande garde sa hauteur (le SNV est fait sur le spectre entier)
r3 <- pls_par_intervalles(p, definir_modele(p, "PLS", x = x, y = numeros(p, "y"), A = 3, spectres = list(list(type = "snv"))), n = 20)
b <- vapply(r3$intervalles, function(z) min(z$rmsecv), 0)
verifier(sprintf("SNV, 20 intervalles : celui de la bande est le meilleur (%.3f, médiane %.3f, spectre entier %.3f)",
                 min(b), stats::median(b), min(r3$complet$rmsecv)),
         dans(r3$intervalles[[which.min(b)]], 1450) && min(b) < 0.8 * stats::median(b))  # (le SNV du spectre entier répand un peu la bande partout)

# MSC : le spectre moyen de référence dépend des observations ; chaque sous-modèle de la validation
# croisée prend celui de ses observations, dans l'iPLS comme dans le modèle
d4 <- definir_modele(p, "PLS", x = x, y = numeros(p, "y"), A = 2, spectres = list(list(type = "msc"), list(type = "sg", derivee = 1, fenetre = 7, degre = 2)))
r4 <- pls_par_intervalles(p, d4, n = 8)
verifier("MSC : le spectre entier redonne le RMSEcv du modèle (spectre moyen recalculé à chaque groupe)",
         isTRUE(all.equal(as.numeric(r4$complet$rmsecv), vapply(1:2, function(a) unname(ajuster(p, modifyList(d4, list(A = a)))$statistiques$RMSEcv[1]), 0), tolerance = 1e-12)))

verifier("une ACP est refusée", inherits(try(pls_par_intervalles(p, definir_modele(p, "PCA", x = x, A = 2)), silent = TRUE), "try-error"))

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
