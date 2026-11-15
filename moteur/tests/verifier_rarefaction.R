# Vérifie la raréfaction et l'accumulation des espèces (moteur/R/rarefaction.R) : contre des
# calculs directs (l'espérance et la variance de la richesse raréfiée par l'énumération de tous
# les sous-échantillons, l'accumulation exacte par celle de tous les ensembles d'observations, les
# estimateurs écrits d'après leurs formules, l'écart-type d'ACE d'après un gradient numérique) et,
# quand il est installé, contre vegan (rarefy avec se, les points de rarecurve, specaccum exact,
# estimateR, specpool). Puis la demande de l'interface : groupes, valeurs manquantes, observations
# vides, effectifs non entiers, refus.
#   Rscript moteur/tests/verifier_rarefaction.R
# (NUEE_VEGAN : une bibliothèque où vegan est installé)

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-9) isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = tol, check.attributes = FALSE))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

if (nzchar(Sys.getenv("NUEE_VEGAN"))) .libPaths(c(Sys.getenv("NUEE_VEGAN"), .libPaths()))
avec_vegan <- requireNamespace("vegan", quietly = TRUE)
cat(if (avec_vegan) sprintf("(vegan %s installé : comparaisons avec lui)\n", utils::packageVersion("vegan")) else "(vegan absent : calculs directs seulement)\n")

cat("Calculs directs\n")
# un relevé de 4 espèces (3, 2, 1, 1 individus) : tous les sous-échantillons de m individus
x <- c(a = 3, b = 2, c = 1, d = 1)
individus <- rep(names(x), x)
enumere <- function(m) {
  r <- apply(utils::combn(length(individus), m), 2, function(i) length(unique(individus[i])))
  c(mean(r), sqrt(mean(r^2) - mean(r)^2))
}
attendu <- t(vapply(1:7, enumere, numeric(2)))
verifier("raréfaction (Hurlbert) : la richesse attendue et son écart-type, de 1 à 7 individus = tous les sous-échantillons",
         proche(rarefaction_hurlbert(x, 1:7), attendu))
# 6 observations de 5 espèces en présences : tous les ensembles de k observations
P <- rbind(c(1, 1, 0, 0, 0), c(1, 0, 1, 0, 0), c(0, 1, 1, 1, 0), c(1, 0, 0, 0, 1), c(1, 1, 0, 0, 0), c(0, 0, 0, 1, 0)) > 0
acc <- accumulation_exacte(P)
moyennes <- vapply(1:6, function(k) mean(apply(utils::combn(6, k), 2, function(i) sum(colSums(P[i, , drop = FALSE]) > 0))), 0)
verifier("accumulation exacte : la richesse attendue dans k observations = tous les ensembles de k observations", proche(acc$richesse, moyennes))
f <- colSums(P)
q <- t(sapply(1:6, function(k) choose(6 - f, k) / choose(6, k)))
V <- q * (1 - q)
co <- stats::cor(P)
et <- sqrt(sapply(1:6, function(k) { s <- 0; for (i in 1:5) for (j in 1:5) if (i != j) s <- s + co[i, j] * sqrt(V[k, i] * V[k, j]); sum(V[k, ]) + s }))
verifier("accumulation exacte : son écart-type (variances et covariances des espèces, d'après les corrélations des présences)", proche(acc$et, et))
y <- c(12, 7, 5, 3, 3, 2, 2, 1, 1, 1, 1, 15, 40)
e <- estimateurs_abondance(y)
F1 <- sum(y == 1); F2 <- sum(y == 2)
verifier(sprintf("Chao1 corrigé du biais (%.3f) = S + F1 (F1 - 1) / (2 (F2 + 1))", e[["chao1"]]), proche(e[["chao1"]], length(y) + F1 * (F1 - 1) / (2 * (F2 + 1))))
ace <- function(Fq, S_ab) {  # ACE comme fonction des F1..F10
  i <- 1:10; N <- sum(i * Fq); Sr <- sum(Fq); C <- 1 - Fq[1] / N
  g <- Sr * sum(i * (i - 1) * Fq) / (C * N * (N - 1)) - 1
  S_ab + Sr / C + max(g, 0) * Fq[1] / C
}
Fq <- tabulate(y, 10)
S_ab <- sum(y > 10)
verifier(sprintf("ACE (%.3f) d'après Chao et Lee (1992)", e[["ace"]]), proche(e[["ace"]], ace(Fq, S_ab)))
grad <- vapply(1:10, function(k) { h <- 1e-5; d <- numeric(10); d[k] <- h; (ace(Fq + d, S_ab) - ace(Fq - d, S_ab)) / (2 * h) }, 0)
verifier("écart-type d'ACE : méthode delta avec un gradient numérique de l'ACE", proche(e[["se_ace"]], sqrt(sum(outer(grad, grad) * (diag(Fq) - outer(Fq, Fq) / e[["ace"]]))), 1e-6))
ei <- estimateurs_incidence(P)
q1 <- sum(f == 1); q2 <- sum(f == 2); n <- 6
verifier("Chao2, jackknife 1 et 2, bootstrap sur les présences, d'après leurs formules",
         proche(ei[c("chao", "jack1", "jack2", "boot")], c(5 + (n - 1) / n * q1^2 / (2 * q2), 5 + q1 * (n - 1) / n, 5 + q1 * (2 * n - 3) / n - q2 * (n - 2)^2 / (n * (n - 1)),
                                                           5 + sum((1 - f / n)^n))))

# un projet : 24 relevés (deux milieux) de 12 espèces, effectifs de Poisson ; une valeur manquante, un relevé vide
set.seed(3)
milieu <- rep(c("haie", "champ"), each = 12)
lambda <- outer(ifelse(milieu == "haie", 1.5, 0.6), exp(-(1:12) / 4) * 6)
ab <- matrix(stats::rpois(length(lambda), lambda), 24, 12, dimnames = list(NULL, sprintf("sp%02d", 1:12)))
ab[7, ] <- 0
df <- data.frame(nom = sprintf("r%02d", 1:24), milieu = milieu, ab, stringsAsFactors = FALSE)
df$sp05[20] <- NA
p <- projet_depuis_tableau(df, "rarefaction")
u <- function(...) numeros(p, c(...))
r <- rarefaction_projet(p, u(colnames(ab)), groupe = u("milieu"))
garde <- seq_len(24) != 20
X <- ab[garde, ]
pleines <- rowSums(X) > 0
verifier(sprintf("23 relevés (celui où une valeur manque laissé de côté), dont un vide ; taille commune : la plus petite (%d)", r$taille),
         r$n == 23 && r$manquantes == 1 && r$vides == 1 && r$taille == min(rowSums(X)[pleines]) && isTRUE(r$entiers))
verifier("richesse raréfiée de chaque relevé à la taille commune = rarefaction_hurlbert", proche(r$observations$Sm[pleines],
         vapply(which(pleines), function(i) rarefaction_hurlbert(X[i, ], r$taille)[1, 1], 0)) && is.na(r$observations$Sm[!pleines]))
verifier("une courbe par relevé non vide, jusqu'à son effectif ; son dernier point, sa richesse observée",
         length(unique(r$courbes$uid)) == 22 && all(vapply(split(r$courbes, r$courbes$uid), function(d) utils::tail(d$S, 1) == sum(X[match(d$uid[1], which(garde)), ] > 0), TRUE)))
verifier("accumulation et estimateurs par milieu (le relevé vide compte dans la haie)",
         setequal(unique(r$accumulation$groupe), c("champ", "haie")) && identical(unname(stats::setNames(r$estimateurs$n, r$estimateurs$groupe)[c("haie", "champ")]), c(12, 11)) &&
           proche(r$accumulation$richesse[r$accumulation$groupe == "haie"], accumulation_exacte(X[milieu[garde] == "haie", ] > 0)$richesse))
pd <- p
pd$donnees[3, as.character(u("sp01"))] <- 2.5
rd <- rarefaction_projet(pd, u(colnames(ab)))
verifier("des effectifs non entiers : ni raréfaction ni Chao1 ni ACE, l'accumulation et les estimateurs des présences seulement",
         identical(rd$entiers, FALSE) && is.null(rd$courbes) && all(is.na(rd$observations$Sm)) && all(is.na(rd$observations$chao1)) && nrow(rd$estimateurs) == 1)
verifier("une taille plus grande que des relevés : eux sans richesse raréfiée", {
  r2 <- rarefaction_projet(p, u(colnames(ab)), taille = 20)
  r2$petites == sum(pleines & rowSums(X) < 20) && all(is.na(r2$observations$Sm[rowSums(X) < 20]))
})
neg <- p
neg$donnees[1, as.character(u("sp01"))] <- -1
verifier("refus : une seule espèce, une espèce qualitative, une valeur négative",
         grepl("deux espèces", refus(rarefaction_projet(p, u("sp01")))) && grepl("milieu", refus(rarefaction_projet(p, u("sp01", "milieu")))) &&
           grepl("positifs", refus(rarefaction_projet(neg, u(colnames(ab))))))
verifier("ce que l'interface reçoit se met en JSON", inherits(jsonlite::toJSON(r, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE), "json"))

if (avec_vegan) {
  cat("Comparaison avec vegan\n")
  grDevices::pdf(NULL)  # (rarecurve lit par("col") même quand il ne dessine pas : pas de Rplots.pdf)
  Xp <- X[pleines, ]
  rownames(Xp) <- sprintf("r%02d", which(garde)[pleines])  # (rarecurve(tidy = TRUE) nomme les relevés)
  rv <- vegan::rarefy(Xp, sample = r$taille, se = TRUE)
  verifier("richesse raréfiée et son écart-type = rarefy(se = TRUE)", proche(r$observations$Sm[pleines], rv["S", ]) && proche(r$observations$se[pleines], rv["se", ]))
  rc <- suppressWarnings(vegan::rarecurve(Xp, step = r$pas, tidy = TRUE))
  verifier(sprintf("les points des courbes (pas de %d) = rarecurve", r$pas), proche(r$courbes$S, rc$Species) && proche(r$courbes$taille, rc$Sample))
  ok <- TRUE
  for (k in unique(r$accumulation$groupe)) {
    sa <- vegan::specaccum(X[milieu[garde] == k, ], method = "exact")
    a <- r$accumulation[r$accumulation$groupe == k, ]
    ok <- ok && proche(a$richesse, sa$richness) && proche(a$et, sa$sd)
  }
  verifier("accumulation exacte et son écart-type, par milieu = specaccum(method = \"exact\")", ok)
  er <- vegan::estimateR(Xp)
  o <- r$observations[pleines, ]
  verifier("S, Chao1 corrigé et son écart-type, ACE = estimateR", proche(o$chao1, er["S.chao1", ]) && proche(o$se_chao1, er["se.chao1", ]) &&
             proche(o$ace, er["S.ACE", ]) && proche(o$S, er["S.obs", ]))
  cat(sprintf("    (l'écart-type d'ACE de vegan, d'un autre gradient, s'écarte de celui-ci de %.0f %% en médiane)\n", 100 * stats::median(abs(er["se.ACE", ] / o$se_ace - 1))))
  sp <- vegan::specpool(X, pool = milieu[garde])
  e2 <- r$estimateurs[match(rownames(sp), r$estimateurs$groupe), ]
  verifier("Chao2, jackknife 1 et 2, bootstrap et leurs écarts-types, par milieu = specpool",
           proche(e2$S, sp$Species) && proche(e2$chao, sp$chao) && proche(e2$se_chao, sp$chao.se) && proche(e2$jack1, sp$jack1) &&
             proche(e2$se_jack1, sp$jack1.se) && proche(e2$jack2, sp$jack2) && proche(e2$boot, sp$boot) && proche(e2$se_boot, sp$boot.se) && proche(e2$n, sp$n))
  # sur des données réelles : la forêt de Barro Colorado (vegan::BCI, 50 parcelles, 225 espèces)
  ev <- new.env()
  utils::data("BCI", package = "vegan", envir = ev)
  bci <- as.matrix(ev$BCI)
  pb <- projet_depuis_tableau(data.frame(nom = sprintf("p%02d", 1:50), bci, check.names = FALSE), "BCI")
  rb <- rarefaction_projet(pb, numeros(pb, colnames(bci)))
  rvb <- vegan::rarefy(bci, sample = min(rowSums(bci)), se = TRUE)
  sab <- vegan::specaccum(bci, method = "exact")
  erb <- vegan::estimateR(bci)
  spb <- vegan::specpool(bci)
  verifier("vegan::BCI : raréfaction (et écart-type), accumulation exacte, Chao1, ACE, estimateurs des présences = vegan",
           proche(rb$observations$Sm, rvb["S", ]) && proche(rb$observations$se, rvb["se", ]) && proche(rb$accumulation$richesse, sab$richness) &&
             proche(rb$accumulation$et, sab$sd) && proche(rb$observations$chao1, erb["S.chao1", ]) && proche(rb$observations$ace, erb["S.ACE", ]) &&
             proche(unlist(rb$estimateurs[c("S", "chao", "se_chao", "jack1", "se_jack1", "jack2", "boot", "se_boot")]),
                    unlist(spb[c("Species", "chao", "chao.se", "jack1", "jack1.se", "jack2", "boot", "boot.se")])))
  rcb <- suppressWarnings(vegan::rarecurve(bci, step = rb$pas, tidy = TRUE))
  verifier(sprintf("vegan::BCI : les points des courbes de raréfaction, par pas de %d individus = rarecurve", rb$pas),
           rb$pas > 1 && proche(rb$courbes$S, rcb$Species) && proche(rb$courbes$taille, rcb$Sample))
}

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
