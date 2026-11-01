# Vérifie le prétraitement des spectres (moteur/R/spectres.R) :
#   - les coefficients de Savitzky–Golay publiés (fenêtre de 5, degré 2) ;
#   - un polynôme de degré ≤ 2 est lissé et dérivé exactement, bords compris ;
#   - SNV et MSC ; s'il est installé, les mêmes résultats que le paquet prospectr (detrend et
#     ligne de base aussi) ;
#   - detrend, ligne de base, abscisses lues dans les noms, Savitzky–Golay zone par zone ;
#   - un modèle PLS sur des spectres inventés : la prédiction des observations du modèle rend
#     leurs scores, le script R refait la même définition, le projet enregistré se relit ;
#   - plus de variation à décrire (spectres plats après la ligne de base, données épuisées) :
#     un message clair, et l'ajustement automatique s'arrête sans erreur.
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
  onde_z <- seq(2500, 1000, length.out = 80)  # abscisses décroissantes, comme des nombres d'onde
  for (degre in 1:3) {  # prospectr ajuste le polynôme sans terme constant : il garde la moyenne du spectre
    verifier(sprintf("detrend de degré %d = prospectr::detrend(snv = FALSE), à la moyenne du spectre près", degre),
             proche(detrend(Z, onde_z, degre) + rowMeans(Z), prospectr::detrend(Z, onde_z, p = degre, snv = FALSE), 1e-10))
  }
  verifier("SNV puis detrend = prospectr::detrend()", proche(detrend(snv(Z), onde_z, 2), prospectr::detrend(Z, onde_z, p = 2), 1e-10))
  verifier("ligne de base = prospectr::baseline() (enveloppe convexe)", proche(ligne_base(Z, onde_z), prospectr::baseline(Z, onde_z), 1e-12))
}

cat("Detrend, ligne de base, zones\n")
x <- seq(400, 700, length.out = 31)
P <- rbind(2 + 0.01 * x, 1 - 3e-5 * (x - 550)^2, 5e-6 * x^2 - x / 300)
verifier("detrend de degré 2 : un polynôme de degré 2 disparaît", proche(detrend(P, x, 2), 0 * P, 1e-9))
verifier("detrend : résidus sans lien avec l'abscisse", proche(detrend(P + sin(x / 20), x, 1) %*% cbind(1, x), matrix(0, 3, 2), 1e-8))
creux <- rbind(abs(x - 520) / 100 + sin(x / 7) / 50, (x - 550)^2 / 1e4)
lb <- ligne_base(creux, x)
verifier("ligne de base : spectre corrigé positif ou nul, nul à ses deux bouts", all(lb >= -1e-12) && proche(lb[, c(1, 31)], matrix(0, 2, 2)))
verifier("ligne de base : un spectre convexe devient nul", proche(lb[2, ], rep(0, 31), 1e-12))
manque <- creux
manque[1, 5] <- NA
lbm <- ligne_base(manque, x)
verifier("ligne de base avec une valeur manquante : elle reste manquante", is.na(lbm[1, 5]) && all(lbm[1, -5] >= -1e-12))
verifier("abscisses lues dans les noms (« 1450 nm »), sinon les places", identical(abscisses_spectre(c("1450 nm", "1452,5 nm"), 3:4), c(1450, 1452.5)) &&
         identical(abscisses_spectre(c("a", "b"), 3:4), 3:4) && identical(abscisses_spectre(c("V1", "V1"), 3:4), 3:4))
G <- cbind(X, X)  # deux zones séparées dans le tableau : traitées chacune à part
colnames(G) <- seq_len(60)
g <- pretraiter_spectres(G, list(list(type = "sg", derivee = 1, fenetre = 7, degre = 2)), positions = c(1:30, 41:70))$X
verifier("Savitzky–Golay zone par zone : la fenêtre ne passe pas d'une zone à l'autre", proche(g, cbind(savitzky_golay(X, 7, 2, 1), savitzky_golay(X, 7, 2, 1))))
verifier("une zone plus étroite que la fenêtre est refusée, avec son nom",
         grepl("zone de 27 à 30", tryCatch(pretraiter_spectres(G[, 27:34], list(list(type = "sg", derivee = 0, fenetre = 7, degre = 2)), positions = c(27:30, 41:44)), error = conditionMessage)))
verifier("une seule zone : comme avant", proche(pretraiter_spectres(G, list(list(type = "sg", derivee = 1, fenetre = 7, degre = 2)))$X, savitzky_golay(G, 7, 2, 1)))

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
d3 <- d
d3$spectres <- etapes_spectres(list(list(type = "snv"), list(type = "detrend"), list(type = "ligne_base")))
tm <- texte_methodes(p, d3, ajuster(p, d3), "fr")
verifier("texte des méthodes : detrend (Barnes et al., 1989) et ligne de base",
         any(grepl("detrend \\(polynôme de degré 2", tm$paragraphes)) && any(grepl("ligne de base", tm$paragraphes)) && any(grepl("^Barnes", tm$references)))
unlink(f)

# Plus de variation à décrire : un message clair au lieu d'une erreur interne de R
onde <- 1000 + 4 * (0:29)
plat <- data.frame(nom = paste0("s", 1:8), y = 1:8)
for (k in seq_along(onde)) plat[[as.character(onde[k])]] <- 0.01 * (1:8)  # des spectres plats
pp <- projet_depuis_tableau(plat, "plats")
dp <- definir_modele(pp, "PLS", x = numeros(pp, as.character(onde)), y = numeros(pp, "y"), A = 1, echelle = c(defaut = "Ctr"),
                     spectres = list(list(type = "ligne_base")))
message_de <- function(expr) tryCatch({ expr; "" }, error = function(e) conditionMessage(e))
verifier("spectres nuls après la ligne de base : « aucune composante possible »",
         startsWith(message_de(ajuster(pp, dp)), "Aucune composante possible"))
options(nuee.langue = "en")
verifier("… et en anglais", startsWith(message_de(ajuster(pp, dp)), "No component is possible"))
options(nuee.langue = NULL)
set.seed(4)
epuise <- data.frame(nom = paste0("o", 1:20), x1 = rnorm(20), x2 = 5, x3 = 7)  # une seule variable varie
epuise$y <- epuise$x1 + rnorm(20, 0, 0.1)
pe <- projet_depuis_tableau(epuise, "épuisé")
for (type in c("PCA", "PLS")) {
  de <- definir_modele(pe, type, x = numeros(pe, c("x1", "x2", "x3")), y = if (type == "PLS") numeros(pe, "y"), A = 3, echelle = c(defaut = "Ctr"))
  verifier(sprintf("%s : la 2e composante est impossible, et le message le dit", type),
           startsWith(message_de(ajuster(pe, de)), "Composante 2 impossible") && grepl("au plus 1\\.$", message_de(ajuster(pe, de))))
  # (en ACP, la validation croisée ne prédit pas x1 sans lui : même la 1re composante n'est pas retenue)
  verifier(sprintf("%s : l'ajustement automatique s'arrête sans erreur, à %d composante", type, if (type == "PLS") 1 else 0),
           identical(tryCatch(autoajuster(pe, de, A_max = 3), error = function(e) conditionMessage(e)), if (type == "PLS") 1L else 0))
}

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
