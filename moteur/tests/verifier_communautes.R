# Vérifie les analyses de communautés fondées sur une distance (moteur/R/communautes.R) : PCoA,
# NMDS, PERMANOVA et dispersion multivariée, test de Mantel et corrélogramme. Contre des calculs
# directs (les formules des distances écrites paire par paire, eigen, cmdscale, MASS::isoMDS,
# lm, TukeyHSD, cor) et, quand il est installé, contre le paquet vegan (vegdist, wisconsin,
# wcmdscale, wascores, envfit, procrustes, postMDS, metaMDS, goodness, adonis2, betadisper,
# permutest, TukeyHSD, mantel, mantel.partial, mantel.correlog) : mêmes distances, valeurs propres,
# scores (au signe de chaque axe près), statistiques et, pour les mêmes permutations (passées à
# vegan), mêmes valeurs p. Puis les refus, ce que l'interface reçoit, le texte des méthodes et les
# tableaux exportés.
#   Rscript moteur/tests/verifier_communautes.R
#   (NUEE_VEGAN=<bibliothèque> : une bibliothèque où vegan est installé)

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-8) isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = tol, check.attributes = FALSE))
# deux matrices de scores égales au signe de chaque axe près
au_signe_pres <- function(a, b, tol = 1e-7) {
  a <- unname(as.matrix(a)); b <- unname(as.matrix(b))
  if (!identical(dim(a), dim(b))) return(FALSE)
  for (j in seq_len(ncol(a))) {
    s <- if (sum(a[, j] * b[, j]) < 0) -1 else 1
    if (!isTRUE(all.equal(a[, j], s * b[, j], tolerance = tol))) return(FALSE)
  }
  TRUE
}
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

if (nzchar(Sys.getenv("NUEE_VEGAN"))) .libPaths(c(Sys.getenv("NUEE_VEGAN"), .libPaths()))
avec_vegan <- requireNamespace("vegan", quietly = TRUE)
# (les comparaisons sont celles de vegan 2.7 : adonis2, metaMDS, betadisper et mantel.correlog ont
# changé depuis 2.6 ; avec une version plus ancienne, seuls les calculs directs sont faits)
if (avec_vegan && utils::packageVersion("vegan") < "2.7") {
  cat(sprintf("(vegan %s, avant 2.7 : pas de comparaison avec lui)\n", utils::packageVersion("vegan")))
  avec_vegan <- FALSE
}
cat(if (avec_vegan) sprintf("(vegan %s installé : comparaisons avec lui)\n", utils::packageVersion("vegan")) else "(vegan absent : calculs directs seulement)\n")
# (les permutations de Nuée passées à vegan, avec une description que permute sait afficher)
pv <- function(P) { attr(P, "control") <- permute::how(nperm = nrow(P)); P }

# ---------------------------------------------------------------- les données

# 40 relevés de 12 espèces (effectifs de Poisson le long d'un gradient de température et selon
# l'habitat), leur milieu (température, pH, altitude, habitat), le site (8 sites de 5 relevés).
set.seed(7)
n <- 40
g1 <- stats::runif(n, -2, 2)
habitat <- factor(sample(c("prairie", "foret", "marais"), n, TRUE), levels = c("foret", "marais", "prairie"))
effet <- c(foret = -0.4, marais = 0.5, prairie = 0)
mu <- sapply(1:12, function(j) exp(0.6 + sin(j) * g1 + effet[as.character(habitat)] * ((j %% 3) - 1)))
abondances <- matrix(stats::rpois(n * 12, mu), n, 12, dimnames = list(NULL, sprintf("sp%02d", 1:12)))
abondances[rowSums(abondances) == 0, 1] <- 1
env <- data.frame(temp = g1 * 3 + 12 + stats::rnorm(n, 0, 0.5), ph = stats::rnorm(n, 6.5, 0.3), altitude = 800 - 40 * g1 + stats::rnorm(n, 0, 30),
                  habitat = habitat, site = rep(sprintf("S%d", 1:8), each = 5), stringsAsFactors = FALSE)
df <- data.frame(nom = sprintf("r%02d", 1:n), env, abondances, stringsAsFactors = FALSE)
df$ph[9] <- NA        # une valeur X manquante
df$sp05[23] <- NA     # une valeur Y manquante : ce relevé sort de toutes les analyses
p <- projet_depuis_tableau(df, "communautes")
u <- function(...) numeros(p, c(...))
especes <- colnames(abondances)
gy <- seq_len(n) != 23  # (les relevés dont Y est complet)
Y <- abondances[gy, ]
e <- df[gy, names(env)]  # (le pH manquant compris)
B <- 199
D_ <- function(m) unname(as.matrix(m))

# ---------------------------------------------------------------- les distances

cat("Distances et transformations : les formules paire par paire\n")
par_paire <- function(X, f) {
  k <- nrow(X); D <- matrix(0, k, k)
  for (i in seq_len(k)) for (j in seq_len(k)) D[i, j] <- f(X[i, ], X[j, ])
  D
}
formules <- list(
  bray = function(a, b) sum(abs(a - b)) / sum(a + b),
  jaccard = function(a, b) { x <- sum(abs(a - b)) / sum(a + b); 2 * x / (1 + x) },
  kulczynski = function(a, b) 1 - 0.5 * (sum(pmin(a, b)) / sum(a) + sum(pmin(a, b)) / sum(b)),
  euclidean = function(a, b) sqrt(sum((a - b)^2)),
  manhattan = function(a, b) sum(abs(a - b)),
  canberra = function(a, b) { k <- a != 0 | b != 0; if (!any(k)) 0 else mean(abs(a - b)[k] / (a + b)[k]) },
  hellinger = function(a, b) sqrt(sum((sqrt(a / sum(a)) - sqrt(b / sum(b)))^2)),
  chord = function(a, b) sqrt(sum((a / sqrt(sum(a^2)) - b / sqrt(sum(b^2)))^2)))
etendues <- apply(Y, 2, function(x) diff(range(x)))
formules$gower <- function(a, b) mean((abs(a - b) / etendues)[etendues > 0])
for (methode in DISTANCES_COMMUNAUTES) {
  ok <- proche(distance_communaute(Y, methode), par_paire(Y, formules[[methode]]), 1e-12)
  pa <- (Y > 0) * 1
  if (methode == "gower") etendues <- apply(pa, 2, function(x) diff(range(x)))
  ok_pa <- proche(distance_communaute(Y, methode, binaire = TRUE), par_paire(pa, formules[[methode]]), 1e-12)
  if (methode == "gower") etendues <- apply(Y, 2, function(x) diff(range(x)))
  verifier(sprintf("%s : quantitative et sur les présences-absences", methode), ok && ok_pa)
}
Yc <- cbind(Y, constante = 4)
verifier("Gower : une variable constante ne compte pas", proche(distance_communaute(Yc, "gower"), distance_communaute(Y, "gower"), 1e-12))
Yd <- Y; Yd[2, ] <- Yd[1, ]
verifier("deux observations identiques : distance exactement 0", all(distance_communaute(Yd, "bray")[1:2, 1:2] == 0))
wis <- function(X) { X <- sweep(X, 2, apply(X, 2, max), "/"); X / rowSums(X) }
verifier("transformations : racines, log(x + 1), présences, relatives, Wisconsin (directes)",
         proche(transformer_communaute(Y, "racine2"), sqrt(Y)) && proche(transformer_communaute(Y, "racine4"), Y^(1 / 4)) &&
           proche(transformer_communaute(Y, "log1p"), log(Y + 1)) && proche(transformer_communaute(Y, "presence"), (Y > 0) + 0) &&
           proche(transformer_communaute(Y, "relative"), Y / rowSums(Y)) && proche(transformer_communaute(Y, "wisconsin"), wis(Y)) &&
           proche(transformer_communaute(Y, "racine_wisconsin"), wis(sqrt(Y))))
Yv <- cbind(Y, vide = 0)
verifier("Wisconsin : une espèce sans aucune valeur reste nulle (pas de 0/0)", all(transformer_communaute(Yv, "wisconsin")[, "vide"] == 0) &&
           proche(transformer_communaute(Yv, "wisconsin")[, -13], wis(Y)))
if (avec_vegan) {
  cat("Distances : vegan::vegdist, wisconsin, decostand\n")
  memes <- vapply(DISTANCES_COMMUNAUTES, function(methode) {
    proche(distance_communaute(Y, methode), D_(vegan::vegdist(Y, methode)), 1e-12) &&
      proche(distance_communaute(Y, methode, binaire = TRUE), D_(vegan::vegdist(Y, methode, binary = TRUE)), 1e-12)
  }, TRUE)
  verifier(sprintf("les %d distances, quantitatives et binaires (Sørensen, Jaccard binaire...) = vegdist", length(memes)), all(memes))
  if (!all(memes)) cat("    différentes :", names(memes)[!memes], "\n")
  verifier("Gower avec une variable constante = vegdist (qui la laisse de côté)", proche(distance_communaute(Yc, "gower"), D_(suppressWarnings(vegan::vegdist(Yc, "gower"))), 1e-12))
  verifier("Wisconsin = vegan::wisconsin ; présences = decostand(\"pa\") ; relatives = decostand(\"total\")",
           proche(transformer_communaute(Y, "wisconsin"), vegan::wisconsin(Y)) && proche(transformer_communaute(Y, "racine_wisconsin"), vegan::wisconsin(sqrt(Y))) &&
             proche(transformer_communaute(Y, "presence"), vegan::decostand(Y, "pa")) && proche(transformer_communaute(Y, "relative"), vegan::decostand(Y, "total")))
}

# ---------------------------------------------------------------- PCoA

cat("PCoA : calculs directs (eigen, cmdscale)\n")
dp <- definir_modele(p, "PCOA", x = u("temp", "ph", "habitat"), y = u(especes), echelle = "none", options = list(distance = "bray", permutations = B, graine = 3))
mp <- ajuster(p, dp)
kp <- mp$noyau
Db <- distance_communaute(Y, "bray")
Gb <- -0.5 * (diag(39) - 1 / 39) %*% Db^2 %*% (diag(39) - 1 / 39)
ev <- eigen(Gb, symmetric = TRUE)$values
verifier(sprintf("%d observations (le relevé sans sp05 laissé de côté et compté)", mp$N), mp$N == 39 && identical(mp$exclues, 23))
verifier(sprintf("%d valeurs propres non nulles (%d négatives) = eigen de −½ (I − 11'/n) D² (I − 11'/n)", length(kp$valeurs), sum(kp$valeurs < 0)),
         proche(kp$valeurs, ev[abs(ev) > sqrt(.Machine$double.eps) * ev[1]]) && sum(kp$valeurs < 0) > 0)
cm <- stats::cmdscale(stats::as.dist(Db), k = ncol(kp$points), eig = TRUE)
verifier("axes positifs = cmdscale (au signe près) ; inertie totale = trace = Σd² / n", au_signe_pres(kp$points, cm$points) &&
           proche(kp$totale, sum(Db[lower.tri(Db)]^2) / 39) && proche(kp$totale, sum(kp$valeurs)))
verifier("chaque axe orienté : sa plus grande coordonnée est positive", all(apply(kp$coord, 2, function(v) v[which.max(abs(v))] > 0)))
vpp <- table_valeurs_pcoa(kp)
verifier("parts : de l'inertie totale (somme de toutes les valeurs propres) ; cumul", proche(vpp$part, kp$valeurs / sum(kp$valeurs)) && proche(utils::tail(vpp$cumul, 1), 1))
cl <- corriger_distance(Db, "lingoes")
cc <- corriger_distance(Db, "cailliez")
verifier("Lingoes : la constante est −(plus petite valeur propre) ; Cailliez : celle de cmdscale(add = TRUE) ; plus aucune valeur propre négative",
         proche(cl$constante, -min(ev)) && proche(cc$constante, stats::cmdscale(stats::as.dist(Db), k = 2, add = TRUE)$ac) &&
           all(pcoa_noyau(cl$D)$valeurs > -1e-10) && all(pcoa_noyau(cc$D)$valeurs > -1e-10))
wa_brut <- crossprod(Y, kp$points) / colSums(Y)
mul <- sqrt(diag(stats::cov.wt(kp$points, rowSums(Y), method = "ML")$cov) / diag(stats::cov.wt(wa_brut, colSums(Y), method = "ML")$cov))
ctr <- colSums(wa_brut * colSums(Y)) / sum(Y)
verifier("scores des espèces : moyennes pondérées des scores, dilatées à la variance des observations",
         proche(mp$especes, sweep(sweep(wa_brut, 2, ctr) %*% diag(mul), 2, ctr, "+")))
a <- mp$ajustement
gx <- !is.na(e$ph)
S2 <- kp$points[gx, 1:2]
r2_lm <- c(temp = summary(stats::lm(e$temp[gx] ~ S2))$r.squared, ph = summary(stats::lm(e$ph[gx] ~ S2))$r.squared)
cf <- stats::coef(stats::lm(e$temp[gx] ~ S2))[-1]
verifier(sprintf("ajustement (deux premiers axes, %d relevés : sans pH manquant) : R² des X = celui de lm sur les axes, direction = coefficients normés", a$n),
         a$n == 38 && proche(a$vecteurs$r2, r2_lm) && proche(a$vecteurs$direction["temp", ], cf / sqrt(sum(cf^2))))
Pa <- permutations_lignes(38, B, NULL, 3)
pt <- (sum(vapply(seq_len(B), function(b) summary(stats::lm(e$temp[gx][Pa[b, ]] ~ S2))$r.squared, 0) >= r2_lm["temp"] - sqrt(.Machine$double.eps)) + 1) / (B + 1)
verifier(sprintf("p de temp (%.3f) = la boucle de lm sur les mêmes permutations", a$vecteurs$p["temp"]), isTRUE(all.equal(unname(a$vecteurs$p["temp"]), pt)))
hab <- droplevels(e$habitat[gx])
tot <- sum(scale(S2, scale = FALSE)^2)
intra <- sum(vapply(levels(hab), function(h) sum(scale(S2[hab == h, , drop = FALSE], scale = FALSE)^2), 0))
verifier("habitat : R² = 1 − SC intra / totale ; centroïdes = moyennes des scores", proche(a$facteurs[[1]]$r2, 1 - intra / tot) &&
           proche(a$centroides[, 1:2], apply(S2, 2, function(x) tapply(x, hab, mean))))
if (avec_vegan) {
  cat("PCoA : vegan::wcmdscale, wascores, envfit\n")
  for (corr in CORRECTIONS_DISTANCE) {
    w <- if (corr == "aucune") vegan::wcmdscale(stats::as.dist(Db), eig = TRUE) else vegan::wcmdscale(stats::as.dist(Db), eig = TRUE, add = corr)
    dc <- definir_modele(p, "PCOA", x = NULL, y = u(especes), echelle = "none", options = list(distance = "bray", correction = corr))
    mc <- ajuster(p, dc)
    verifier(sprintf("correction %s : valeurs propres, axes (au signe près) et constante = wcmdscale", corr),
             proche(mc$noyau$valeurs, w$eig) && au_signe_pres(mc$noyau$points, w$points) && proche(mc$constante, if (corr == "aucune") 0 else w$ac))
  }
  verifier("scores des espèces = vegan::wascores(expand = TRUE)", proche(mp$especes, vegan::wascores(kp$points, Y, expand = TRUE)))
  ef <- vegan::envfit(S2, data.frame(temp = e$temp[gx], ph = e$ph[gx], habitat = hab), permutations = pv(Pa))
  verifier("envfit, mêmes permutations : R², directions, p des X quantitatifs ; R², centroïdes, p de l'habitat",
           proche(a$vecteurs$r2, ef$vectors$r) && proche(a$vecteurs$direction, ef$vectors$arrows) && proche(a$vecteurs$p, ef$vectors$pvals) &&
             proche(a$facteurs[[1]]$r2, ef$factors$r) && proche(a$centroides[, 1:2], ef$factors$centroids) && proche(a$facteurs[[1]]$p, ef$factors$pvals))
  dps <- definir_modele(p, "PCOA", x = u("temp", "habitat"), y = u(especes), echelle = "none",
                        options = list(distance = "hellinger", transformation = "racine2", permutations = B, graine = 8, strates = list(by = "variable", variable = u("site"))))
  mps <- ajuster(p, dps)
  Ps <- permutations_lignes(39, B, e$site, 8)
  efs <- vegan::envfit(mps$noyau$points[, 1:2], data.frame(temp = e$temp, habitat = e$habitat), permutations = pv(Ps))
  verifier("Hellinger sur les racines, permutations par site : PCoA = wcmdscale, envfit = vegan",
           proche(mps$noyau$valeurs, vegan::wcmdscale(vegan::vegdist(sqrt(Y), "hellinger"), eig = TRUE)$eig) && all(apply(Ps, 1, function(i) all(e$site[i] == e$site))) &&
             proche(mps$ajustement$vecteurs$p, efs$vectors$pvals) && proche(mps$ajustement$facteurs[[1]]$p, efs$factors$pvals))
}

# ---------------------------------------------------------------- NMDS

cat("NMDS : MASS::isoMDS depuis la même configuration, Shepard, Procrustes\n")
Dz <- Db; Dz[2, 1] <- Dz[1, 2] <- 0  # (une dissimilarité nulle, remplacée par la moitié de la plus petite)
k0 <- nmds_noyau(Dz, 2, 0, 1)
dz <- stats::as.dist(Dz); dz[dz <= 0] <- min(dz[dz > 0]) / 2
iso <- MASS::isoMDS(dz, k = 2, maxit = 200, trace = FALSE)
X0 <- scale(iso$points, scale = FALSE); X0 <- stats::prcomp(X0)$x; X0 <- X0 / (max(stats::dist(X0)) / max(dz))
verifier("sans départ au hasard : stress et configuration (centrée, axes principaux, échelle de la plus grande dissimilarité) d'isoMDS depuis la PCoA",
         proche(k0$stress, iso$stress / 100, 1e-6) && au_signe_pres(k0$points, X0) && k0$nuls == 1 && proche(k0$remplacement, min(dz[dz > 0])))
iso_r <- stats::isoreg(k0$shepard$dissimilarite, k0$shepard$distance)
verifier("Shepard : distances de l'ordination en ordre des dissimilarités ; stress = √(Σ(d − d̂)² / Σd²) de la régression monotone ; stress par observation (Σ des carrés = stress²)",
         proche(sort(stats::dist(k0$points))[1], min(k0$shepard$distance)) && proche(k0$stress, sqrt(sum((k0$shepard$distance - k0$shepard$ajustee)^2) / sum(k0$shepard$distance^2))) &&
           proche(sum(k0$stress_observations^2), k0$stress^2) && all(diff(k0$shepard$ajustee) >= -1e-12))
pr <- procrustes_symetrique(k0$points, k0$points %*% matrix(c(0, 1, -1, 0), 2) * 3 + 5)
verifier("Procrustes symétrique : une rotation, une dilatation et une translation ne laissent aucun écart", pr$rmse < 1e-10 && max(pr$residus) < 1e-10)
dn <- definir_modele(p, "NMDS", x = u("temp", "habitat"), y = u(especes), echelle = "none", options = list(distance = "bray", k = 2, departs = 20, graine = 5, permutations = B))
mn <- ajuster(p, dn)
kn <- mn$noyau
verifier(sprintf("NMDS : 20 départs au hasard en plus du premier, le meilleur gardé (stress %.4f, départ %d, retrouvé %d fois)", kn$stress, kn$meilleur, kn$retrouvee),
         length(kn$stress_departs) == 21 && proche(kn$stress, min(kn$stress_departs), 1e-6) && kn$meilleur %in% 0:20)
verifier("le hasard de la séance intact, les mêmes résultats d'une fois à l'autre", {
  set.seed(99); a1 <- stats::runif(1); set.seed(99)
  k2 <- nmds_noyau(Db, 2, 5, 5); a2 <- stats::runif(1)
  identical(a1, a2) && identical(k2$stress_departs, nmds_noyau(Db, 2, 5, 5)$stress_departs)
})
if (avec_vegan) {
  cat("NMDS : vegan::metaMDS (isoMDS), postMDS, procrustes, goodness, envfit\n")
  set.seed(5)
  mm <- vegan::metaMDS(Y, distance = "bray", k = 2, try = 20, trymax = 20, engine = "isoMDS", autotransform = FALSE, noshare = FALSE, zerodist = "add",
                       trace = 0, halfchange = FALSE)
  verifier(sprintf("metaMDS des mêmes départs (même graine) : même stress (%.4f), même meilleur départ, même nombre de fois retrouvé (%d)", mm$stress / 100, mm$converged),
           proche(kn$stress, mm$stress / 100, 1e-6) && kn$meilleur == mm$bestry && kn$retrouvee == mm$converged)
  verifier("configuration (postMDS sans demi-changement) et scores des espèces (wascores, expand = TRUE) = metaMDS, au signe près",
           au_signe_pres(kn$points, mm$points, 1e-6) && au_signe_pres(mn$especes, mm$species, 1e-6))
  verifier("stress par observation = vegan::goodness", proche(kn$stress_observations * 100, vegan::goodness(mm, vegan::vegdist(Y, "bray")), 1e-6))
  set.seed(123)
  mm2 <- vegan::metaMDS(Y, distance = "bray", k = 2, try = 20, trymax = 20, engine = "isoMDS", autotransform = FALSE, noshare = FALSE, zerodist = "add", trace = 0)
  verifier(sprintf("metaMDS d'autres départs au hasard : stress %.4f contre %.4f (à 2 %% près)", mm2$stress / 100, kn$stress), abs(mm2$stress / 100 - kn$stress) <= 0.02 * kn$stress)
  pro <- vegan::procrustes(k0$points, kn$points, symmetric = TRUE)
  mine <- procrustes_symetrique(k0$points, kn$points)
  verifier("Procrustes symétrique = vegan::procrustes(symmetric = TRUE) : erreur quadratique moyenne et résidus",
           proche(mine$rmse, summary(pro)$rmse) && proche(mine$residus, stats::residuals(pro)))
  Pn <- permutations_lignes(39, B, NULL, 5)
  efn <- vegan::envfit(kn$points, data.frame(temp = e$temp, habitat = e$habitat), permutations = pv(Pn))
  verifier("NMDS : envfit sur les 2 dimensions, mêmes permutations", proche(mn$ajustement$vecteurs$r2, efn$vectors$r) && proche(mn$ajustement$vecteurs$p, efn$vectors$pvals) &&
             proche(mn$ajustement$facteurs[[1]]$p, efn$factors$pvals) && proche(mn$ajustement$centroides, efn$factors$centroids))
  dn3 <- definir_modele(p, "NMDS", x = NULL, y = u(especes), echelle = "none", options = list(distance = "jaccard", binaire = TRUE, transformation = "wisconsin", k = 3, departs = 10, graine = 2))
  mn3 <- ajuster(p, dn3)
  set.seed(2)
  mm3 <- vegan::metaMDS(vegan::wisconsin(Y), distance = "jaccard", binary = TRUE, k = 3, try = 10, trymax = 10, engine = "isoMDS", autotransform = FALSE, noshare = FALSE,
                        zerodist = "add", trace = 0, halfchange = FALSE)
  verifier(sprintf("Jaccard binaire après Wisconsin, 3 dimensions : stress (%.4f) et configuration = metaMDS", mn3$noyau$stress),
           proche(mn3$noyau$stress, mm3$stress / 100, 1e-6) && au_signe_pres(mn3$noyau$points, mm3$points, 1e-6))
}

# ---------------------------------------------------------------- PERMANOVA

cat("PERMANOVA : calculs directs (sommes des carrés des distances d'Anderson 2001)\n")
P1 <- permutations_lignes(39, B, NULL, 4)
d1 <- definir_modele(p, "PERMANOVA", x = u("habitat"), y = u(especes), echelle = "none", options = list(distance = "bray", permutations = B, graine = 4))
m1 <- ajuster(p, d1)
t1 <- m1$noyau$table
sc_anderson <- function(D, f) {
  d2 <- D^2
  totale <- sum(d2[lower.tri(d2)]) / nrow(D)
  intra <- sum(vapply(split(seq_len(nrow(D)), f), function(i) { x <- d2[i, i, drop = FALSE]; sum(x[lower.tri(x)]) / length(i) }, 0))
  c(totale = totale, intra = intra)
}
sc <- sc_anderson(Db, e$habitat)
F_anderson <- function(D, f) { s <- sc_anderson(D, f); ((s[["totale"]] - s[["intra"]]) / (nlevels(f) - 1)) / (s[["intra"]] / (length(f) - nlevels(f))) }
F0 <- F_anderson(Db, e$habitat)
# (permuter les lignes et les colonnes de G par p, c'est permuter les étiquettes par order(p))
Fperm <- vapply(seq_len(B), function(b) F_anderson(Db, e$habitat[order(P1[b, ])]), 0)
verifier(sprintf("un facteur : SC entre et intra = celles d'Anderson (2001), pseudo-F (%.3f) et p (%.3f) = les mêmes étiquettes permutées", t1$F[1], t1$p[1]),
         proche(t1$ss[c(1, 3)], c(sc[["totale"]] - sc[["intra"]], sc[["totale"]])) && proche(t1$ss[2], sc[["intra"]]) && proche(t1$F[1], F0) &&
           isTRUE(all.equal(t1$p[1], (sum(Fperm >= F0 - sqrt(.Machine$double.eps)) + 1) / (B + 1))) && proche(t1$ddl, c(2, 36, 38)))
verifier("R² = SC / SC totale ; le modèle entier d'un seul terme est ce terme", proche(t1$R2, t1$ss / sc[["totale"]]) && proche(m1$noyau$modele$F, t1$F[1]) && identical(m1$noyau$modele$p, t1$p[1]))
dt <- definir_modele(p, "PERMANOVA", x = u("temp", "altitude", "habitat"), y = u(especes), echelle = "none", croises = list(u("temp", "habitat")),
                     options = list(distance = "bray", permutations = B, graine = 6, par = "termes"))
mt <- ajuster(p, dt)
Xt <- stats::model.matrix(~ temp + altitude + habitat + temp:habitat, e)
H <- function(cols) { q <- qr(scale(Xt[, cols, drop = FALSE], scale = FALSE)); B0 <- qr.Q(q)[, seq_len(q$rank), drop = FALSE]; B0 %*% t(B0) }
Gt <- -0.5 * (diag(39) - 1 / 39) %*% Db^2 %*% (diag(39) - 1 / 39)
cumul <- vapply(list(2, 2:3, 2:5, 2:7), function(cols) sum(diag(H(cols) %*% Gt)), 0)
verifier("termes séquentiels (temp, altitude, habitat, temp*habitat) : SC = tr(H G) des modèles emboîtés, calculée directement",
         proche(mt$noyau$table$ss[1:4], diff(c(0, cumul))) && identical(mt$noyau$table$ddl[1:4], c(1, 1, 2, 2)) && identical(mt$noyau$table$terme[1:4], c("temp", "altitude", "habitat", "temp*habitat")))
if (avec_vegan) {
  cat("PERMANOVA : vegan::adonis2, mêmes permutations\n")
  Pt <- permutations_lignes(39, B, NULL, 6)
  compare <- function(t, a) proche(t$ddl, a$Df) && proche(t$ss, a$SumOfSqs) && proche(t$R2, a$R2) && proche(t$F, a$F) && isTRUE(all.equal(t$p, a[["Pr(>F)"]]))
  verifier("un facteur = adonis2", compare(t1, vegan::adonis2(stats::as.dist(Db) ~ habitat, data = e, permutations = pv(P1), by = "terms")))
  verifier("termes séquentiels avec un terme croisé = adonis2(by = \"terms\") : ddl, SC, R², F et p",
           compare(mt$noyau$table, vegan::adonis2(stats::as.dist(Db) ~ temp + altitude + habitat + temp:habitat, data = e, permutations = pv(Pt), by = "terms")))
  dm <- definir_modele(p, "PERMANOVA", x = u("temp", "altitude", "habitat"), y = u(especes), echelle = "none", croises = list(u("temp", "habitat")),
                       options = list(distance = "bray", permutations = B, graine = 6, par = "marges"))
  mm_ <- ajuster(p, dm)
  verifier(sprintf("effets marginaux (%s : ceux qu'un terme croisé contient ne sont pas testés) = adonis2(by = \"margin\")", paste(mm_$noyau$table$terme[1:2], collapse = ", ")),
           compare(mm_$noyau$table, vegan::adonis2(stats::as.dist(Db) ~ temp + altitude + habitat + temp:habitat, data = e, permutations = pv(Pt), by = "margin")))
  dw <- definir_modele(p, "PERMANOVA", x = u("temp", "altitude", "habitat"), y = u(especes), echelle = "none",
                       options = list(distance = "bray", permutations = B, graine = 6, par = "modele", correction = "lingoes", strates = list(by = "variable", variable = u("site"))))
  mw <- ajuster(p, dw)
  Pw <- permutations_lignes(39, B, e$site, 6)
  verifier("le modèle entier, distance corrigée par Lingoes, permutations par site = adonis2(add = \"lingoes\")",
           compare(mw$noyau$table, vegan::adonis2(stats::as.dist(Db) ~ temp + altitude + habitat, data = e, permutations = pv(Pw), add = "lingoes", by = NULL)) &&
             proche(mw$constante, vegan:::addLingoes(Db)))
  verifier("le test du modèle entier, donné avec les termes, = adonis2(by = NULL)",
           proche(mt$noyau$modele$F, vegan::adonis2(stats::as.dist(Db) ~ temp + altitude + habitat + temp:habitat, data = e, permutations = pv(Pt), by = NULL)$F[1]) &&
             identical(mt$noyau$modele$p, vegan::adonis2(stats::as.dist(Db) ~ temp + altitude + habitat + temp:habitat, data = e, permutations = pv(Pt), by = NULL)[["Pr(>F)"]][1]))
}

cat("Dispersion multivariée : calculs directs\n")
De <- distance_communaute(Y, "euclidean")
ds <- dispersion_noyau(De, e$habitat, "centroide", P1)
moyennes <- apply(Y, 2, function(x) tapply(x, e$habitat, mean))
directe <- sqrt(rowSums((Y - moyennes[as.character(e$habitat), ])^2))
verifier("distance euclidienne : distances au centroïde = distances aux moyennes des groupes dans le tableau lui-même", proche(ds$distances, directe))
av <- stats::anova(stats::lm(directe ~ e$habitat))
res <- stats::residuals(stats::lm(directe ~ e$habitat))
Fr <- vapply(seq_len(B), function(b) { a <- stats::anova(stats::lm(res[P1[b, ]] ~ e$habitat)); a[["F value"]][1] }, 0)
tk <- stats::TukeyHSD(stats::aov(directe ~ e$habitat))[[1]]
verifier(sprintf("ANOVA des distances (F = %.3f), p par permutation de ses résidus (%.3f), paires de Tukey", ds$F, ds$p_permutation),
         proche(ds$F, av[["F value"]][1]) && proche(ds$p, av[["Pr(>F)"]][1]) && isTRUE(all.equal(ds$p_permutation, (sum(Fr >= ds$F - sqrt(.Machine$double.eps)) + 1) / (B + 1))) &&
           proche(ds$tukey$difference, tk[, "diff"]) && proche(ds$tukey$p, tk[, "p adj"]))
dsm <- dispersion_noyau(De, e$habitat, "mediane", NULL)
med <- t(sapply(levels(e$habitat), function(h) { X <- Y[e$habitat == h, ]; stats::optim(apply(X, 2, stats::median), function(x) sum(sqrt(rowSums(sweep(X, 2, x)^2))), method = "Nelder-Mead", control = list(maxit = 20000, reltol = 1e-14))$par }))
verifier("médiane spatiale : la somme des distances à elle n'est pas plus grande que celle d'une recherche directe (Nelder-Mead)",
         all(vapply(levels(e$habitat), function(h) sum(dsm$distances[e$habitat == h]) <= sum(sqrt(rowSums(sweep(Y[e$habitat == h, ], 2, med[h, ])^2))) + 1e-6, TRUE)))
if (avec_vegan) {
  cat("Dispersion : vegan::betadisper, permutest, TukeyHSD\n")
  bd <- vegan::betadisper(stats::as.dist(Db), e$habitat, type = "centroid")
  db <- dispersion_noyau(Db, e$habitat, "centroide", P1)
  pt <- vegan::permutest(bd, permutations = pv(P1))
  tv <- stats::TukeyHSD(bd)$group
  verifier("Bray-Curtis (valeurs propres négatives) : distances au centroïde, moyennes par groupe, ANOVA, p par permutation, Tukey = betadisper",
           proche(db$distances, bd$distances) && proche(db$groupes$moyenne, bd$group.distances) && proche(db$F, pt$tab$F[1]) && identical(db$p_permutation, pt$tab[["Pr(>F)"]][1]) &&
             proche(db$anova$ss, pt$tab[["Sum Sq"]]) && proche(db$tukey$difference, tv[, "diff"]) && proche(db$tukey$bas, tv[, "lwr"]) && proche(db$tukey$p, tv[, "p adj"]))
  bm <- vegan::betadisper(stats::as.dist(Db), e$habitat, type = "median")
  verifier("médiane spatiale (sur les axes positifs et sur les négatifs, comme vegan) = betadisper(type = \"median\") à 1e-5",
           proche(dispersion_noyau(Db, e$habitat, "mediane")$distances, bm$distances, 1e-5))
  verifier("la dispersion d'une PERMANOVA = betadisper sur ses observations (le groupement de la variable de classe aussi)", {
    dd <- definir_modele(p, "PERMANOVA", x = u("temp", "habitat"), classe = u("site"), y = u(especes), echelle = "none", options = list(distance = "bray", permutations = B, graine = 4))
    md <- ajuster(p, dd)
    proche(md$dispersions$habitat$distances, bd$distances) && proche(md$dispersions$site$distances, vegan::betadisper(stats::as.dist(Db), factor(e$site), type = "centroid")$distances) &&
      identical(md$dispersions$habitat$p_permutation, vegan::permutest(bd, permutations = pv(permutations_lignes(39, B, NULL, 4)))$tab[["Pr(>F)"]][1])
  })
}

# ---------------------------------------------------------------- Mantel

cat("Mantel : calculs directs\n")
dmt <- definir_modele(p, "MANTEL", x = u("temp", "altitude"), y = u(especes), echelle = "UV", options = list(distance = "bray", permutations = B, graine = 9))
mmt <- ajuster(p, dmt)
Dxt <- as.matrix(stats::dist(scale(e[, c("temp", "altitude")])))
bas <- lower.tri(Db)
verifier("la distance X : euclidienne sur les X centrés-réduits (distances_mantel_x)", proche(distances_mantel_x(p, dmt, numeros_observations(p)[gy])$D, Dxt))
P9 <- permutations_lignes(39, B, NULL, 9)
r0 <- stats::cor(Db[bas], Dxt[bas])
rp <- vapply(seq_len(B), function(b) stats::cor(Db[P9[b, ], P9[b, ]][bas], Dxt[bas]), 0)
verifier(sprintf("r de Mantel (%.3f) = cor des distances ; p (%.3f) = la boucle sur les mêmes permutations de la distance Y", mmt$test$r, mmt$test$p),
         proche(mmt$test$r, r0) && isTRUE(all.equal(mmt$test$p, (sum(rp >= r0 - sqrt(.Machine$double.eps)) + 1) / (B + 1))))
dsp <- definir_modele(p, "MANTEL", x = u("temp", "altitude"), y = u(especes), echelle = "UV", options = list(distance = "bray", permutations = B, graine = 9, methode = "spearman", z = u("ph")))
msp <- ajuster(p, dsp)
gz <- gy & !is.na(df$ph)
Dyz <- distance_communaute(abondances[gz, ], "bray")
Dxz <- as.matrix(stats::dist(scale(env[gz, c("temp", "altitude")])))
Dzz <- as.matrix(stats::dist(scale(env$ph[gz])))
bz <- lower.tri(Dyz)
rxy <- stats::cor(Dyz[bz], Dxz[bz], method = "spearman"); rxz <- stats::cor(Dyz[bz], Dzz[bz], method = "spearman"); ryz <- stats::cor(Dxz[bz], Dzz[bz], method = "spearman")
verifier(sprintf("Spearman, partiel par le pH (%d relevés : sans pH manquant) : r et r partiel = les corrélations des rangs", msp$N),
         msp$N == 38 && proche(msp$test$r, rxy) && proche(msp$test$r_partiel, (rxy - rxz * ryz) / sqrt(1 - rxz^2) / sqrt(1 - ryz^2)))
cg <- mmt$correlogramme
vd <- Dxt[bas]
nc <- ceiling(1 + log2(length(vd)))
verifier(sprintf("corrélogramme : %d classes (règle de Sturges), toutes les paires dans une classe, r de la classe 1 = −cor(appartenance, distance Y)", cg$classes),
         cg$classes == nc && sum(cg$table$paires) == length(vd) &&
           proche(cg$table$r[1], -stats::cor(as.numeric(vd <= cg$bornes[2]), Db[bas])) && all(!cg$table$teste | cg$table$paires > 0))
verifier("corrélogramme : correction progressive de Holm (la classe k avec les k premières)", {
  pc <- cg$table$p; k <- sum(!is.na(pc))
  proche(cg$table$p_corrige[seq_len(k)], vapply(seq_len(k), function(j) stats::p.adjust(pc[seq_len(j)], "holm")[j], 0))
})
if (avec_vegan) {
  cat("Mantel : vegan::mantel, mantel.partial, mantel.correlog\n")
  vm <- vegan::mantel(stats::as.dist(Db), stats::as.dist(Dxt), permutations = pv(P9))
  verifier("Pearson = vegan::mantel(Y, X), mêmes permutations : r et p", proche(mmt$test$r, vm$statistic) && identical(mmt$test$p, vm$signif))
  Pz <- permutations_lignes(38, B, NULL, 9)
  vs <- vegan::mantel(stats::as.dist(Dyz), stats::as.dist(Dxz), method = "spearman", permutations = pv(Pz))
  vp <- vegan::mantel.partial(stats::as.dist(Dyz), stats::as.dist(Dxz), stats::as.dist(Dzz), method = "spearman", permutations = pv(Pz))
  verifier("Spearman et partiel = vegan::mantel et mantel.partial : r et p", proche(msp$test$r, vs$statistic) && identical(msp$test$p, vs$signif) &&
             proche(msp$test$r_partiel, vp$statistic) && identical(msp$test$p_partiel, vp$signif))
  dst <- definir_modele(p, "MANTEL", x = u("temp", "altitude"), y = u(especes), echelle = "UV", options = list(distance = "bray", permutations = B, graine = 9, strates = list(by = "variable", variable = u("site"))))
  mst <- ajuster(p, dst)
  vst <- vegan::mantel(stats::as.dist(Db), stats::as.dist(Dxt), permutations = pv(permutations_lignes(39, B, e$site, 9)))
  verifier("permutations à l'intérieur des sites = mantel avec les mêmes permutations", identical(mst$test$p, vst$signif))
  vc <- vegan::mantel.correlog(Db, Dxt, nperm = B)
  vr <- vc$mantel.res
  verifier("corrélogramme = mantel.correlog : centres des classes, nombre de distances, r de Mantel, classes testées",
           proche(cg$table$centre, vr[, "class.index"]) && identical(as.numeric(2 * cg$table$paires), unname(vr[, "n.dist"])) &&
             identical(is.na(cg$table$r), unname(is.na(vr[, "Mantel.cor"]))) && proche(cg$table$r[cg$table$teste], vr[cg$table$teste, "Mantel.cor"]) && proche(cg$bornes, vc$break.pts))
  pv_classes <- vapply(which(cg$table$teste), function(k) {
    M <- matrix(0, 39, 39); M[bas] <- as.numeric(findInterval(Dxt[bas], cg$bornes, left.open = TRUE) == k); M <- M + t(M)
    t <- vegan::mantel(stats::as.dist(M), stats::as.dist(Db), permutations = pv(P9))
    if (t$statistic < 0) (sum(t$perm <= t$statistic) + 1) / (B + 1) else t$signif
  }, 0)
  verifier("p de chaque classe = vegan::mantel(appartenance, Y) avec les mêmes permutations, du côté du signe (comme mantel.correlog)",
           isTRUE(all.equal(cg$table$p[cg$table$teste], pv_classes)))
  verifier("p de mantel.correlog (ses propres permutations) à l'erreur de Monte-Carlo près (0,08)", all(abs(cg$table$p[cg$table$teste] - vr[cg$table$teste, "Pr(Mantel)"]) <= 0.08))
  # sur des données réelles : les prairies dunaires (vegan::dune) et leur milieu
  ed <- new.env()
  utils::data("dune", "dune.env", package = "vegan", envir = ed)
  pd <- projet_depuis_tableau(data.frame(nom = rownames(ed$dune), A1 = ed$dune.env$A1, Management = factor(as.character(ed$dune.env$Management)), Use = factor(as.character(ed$dune.env$Use)),
                                         ed$dune, check.names = FALSE, stringsAsFactors = FALSE), "dune")
  dd <- definir_modele(pd, "PERMANOVA", x = numeros(pd, c("A1", "Management")), y = numeros(pd, colnames(ed$dune)), echelle = "none", options = list(distance = "bray", permutations = 99, graine = 1))
  md <- ajuster(pd, dd)
  denv <- data.frame(A1 = ed$dune.env$A1, Management = factor(as.character(ed$dune.env$Management)))
  ad <- vegan::adonis2(ed$dune ~ A1 + Management, data = denv, permutations = pv(permutations_lignes(20, 99, NULL, 1)), by = "terms")
  verifier("vegan::dune : PERMANOVA A1 + Management = adonis2 ; PCoA = wcmdscale ; NMDS = metaMDS (même graine)", {
    dpd <- definir_modele(pd, "PCOA", x = NULL, y = numeros(pd, colnames(ed$dune)), echelle = "none", options = list(distance = "bray"))
    dnd <- definir_modele(pd, "NMDS", x = NULL, y = numeros(pd, colnames(ed$dune)), echelle = "none", options = list(distance = "bray", departs = 20, graine = 11))
    set.seed(11)
    mmd <- vegan::metaMDS(ed$dune, distance = "bray", engine = "isoMDS", autotransform = FALSE, noshare = FALSE, zerodist = "add", trace = 0, halfchange = FALSE)
    proche(md$noyau$table$F, ad$F) && isTRUE(all.equal(md$noyau$table$p, ad[["Pr(>F)"]])) &&
      proche(ajuster(pd, dpd)$noyau$valeurs, vegan::wcmdscale(vegan::vegdist(ed$dune), eig = TRUE)$eig) &&
      proche(ajuster(pd, dnd)$noyau$stress, mmd$stress / 100, 1e-6)
  })
}

# ---------------------------------------------------------------- refus

cat("Refus, valeurs manquantes\n")
neg <- p
neg$donnees[1, as.character(u("sp01"))] <- -1
vide <- p
vide$donnees[3, as.character(u(especes))] <- 0
pcoa <- function(pr, ...) definir_modele(pr, "PCOA", x = NULL, y = u(especes), echelle = "none", options = list(...))
verifier("refus : Bray-Curtis et Hellinger de valeurs négatives, une transformation de valeurs négatives, une distance inconnue",
         grepl("positives ou nulles", refus(ajuster(neg, pcoa(neg, distance = "bray")))) && grepl("positives ou nulles", refus(ajuster(neg, pcoa(neg, distance = "hellinger")))) &&
           grepl("positives ou nulles", refus(ajuster(neg, pcoa(neg, distance = "euclidean", transformation = "racine2")))) &&
           !nzchar(refus(ajuster(neg, pcoa(neg, distance = "euclidean")))) && grepl("inconnue", refus(ajuster(p, pcoa(p, distance = "mahalanobis")))))
verifier("refus : une observation sans aucune valeur pour Bray-Curtis et Kulczynski (nommée), acceptée par la distance euclidienne",
         grepl("r03", refus(ajuster(vide, pcoa(vide, distance = "bray")))) && grepl("sans aucune valeur", refus(ajuster(vide, pcoa(vide, distance = "kulczynski")))) &&
           !nzchar(refus(ajuster(vide, pcoa(vide, distance = "euclidean")))))
verifier("refus : sans Y, une Y qualitative, une PERMANOVA ou un Mantel sans X, un terme croisé à ajuster sur une PCoA",
         grepl("tableau Y", refus(ajuster(p, definir_modele(p, "PCOA", x = u("temp"), y = NULL, echelle = "none")))) &&
           grepl("habitat", refus(ajuster(p, definir_modele(p, "NMDS", x = NULL, y = u("habitat", "sp01"), echelle = "none")))) &&
           grepl("variables X", refus(ajuster(p, definir_modele(p, "PERMANOVA", x = NULL, y = u(especes), echelle = "none")))) &&
           grepl("variables X", refus(ajuster(p, definir_modele(p, "MANTEL", x = NULL, y = u(especes), echelle = "none")))) &&
           grepl("termes croisés", refus(ajuster(p, definir_modele(p, "PCOA", x = u("temp", "altitude"), y = u(especes), echelle = "none", croises = list(u("temp", "altitude")))))))
verifier("refus : permutations par observation (des groupes d'une seule), un jeu de test, un NMDS en 4 dimensions de 5 observations",
         grepl("plusieurs", refus(ajuster(p, definir_modele(p, "PERMANOVA", x = u("habitat"), y = u(especes), echelle = "none", options = list(strates = list(by = "variable", variable = u("altitude"))))))) &&
           grepl("jeu de test", refus(definir_modele(p, "PCOA", x = NULL, y = u(especes), echelle = "none", test = 1:5))) &&
           grepl("au moins 6", refus(ajuster(p, definir_modele(p, "NMDS", x = NULL, y = u(especes), echelle = "none", observations = 1:5, options = list(k = 4))))))
options(nuee.langue = "en")
verifier("les refus en anglais", grepl("positive or zero", refus(ajuster(neg, pcoa(neg, distance = "bray")))) && grepl("without any value", refus(ajuster(vide, pcoa(vide, distance = "bray")))))
options(nuee.langue = NULL)

# ---------------------------------------------------------------- l'interface, les méthodes, les exports

cat("Ce que l'interface reçoit, texte des méthodes, tableaux\n")
etat <- new.env()
etat$projets <- list(P1 = c(p, list(id = "P1")))
class(etat$projets$P1) <- class(p)
etat$modeles <- list()
projet <- function(id) etat$projets[[id]]
modele <- function(id) etat$modeles[[id]]
garder <- function(id, d, m) { etat$modeles[[id]] <- list(projet = "P1", definition = d, modele = m); id }
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE)
resultat <- function(id) fonction_analyse("resultat_", etat$modeles[[id]]$definition$type)(id)
modeles <- list(PCOA = list(dp, mp), NMDS = list(dn, mn), PERMANOVA = list(dt, mt), MANTEL = list(dsp, msp))
for (type in names(modeles)) invisible(garder(type, modeles[[type]][[1]], modeles[[type]][[2]]))
r <- resultat("PCOA")
verifier("PCoA : analyse sans prédiction, observations et leurs axes, valeurs propres (négatives marquées), espèces, flèches et centroïdes, réglages",
         isTRUE(r$analyse) && identical(r$prediction, FALSE) && identical(as.character(r$vues_defaut), c("ordination_dist", "valeurs_propres_pcoa")) &&
           identical(names(r$observations)[1:3], c("uid", "nom", "PCoA1")) && nrow(r$observations) == 39 && any(r$valeurs_propres$negative) &&
           nrow(r$especes) == 12 && identical(r$ajustement$vecteurs$terme, c("temp", "ph")) && nrow(r$ajustement$centroides) == 3 &&
           identical(r$reglages$options$distance, "bray") && inherits(json(r), "json"))
r <- resultat("NMDS")
verifier("NMDS : stress, départs, Shepard, stress de chaque observation", proche(r$stress, kn$stress) && nrow(r$stress_departs) == 21 &&
           length(r$shepard$dissimilarite) == 39 * 38 / 2 && "stress" %in% names(r$observations) && inherits(json(r), "json"))
r <- resultat("PERMANOVA")
verifier("PERMANOVA : la table (termes, résidus, total), le modèle, la dispersion de l'habitat (groupes, tests, paires, distances par observation)",
         nrow(r$table) == 6 && length(r$dispersions) == 1 && r$dispersions[[1]]$variable == "habitat" && nrow(r$dispersions[[1]]$tukey) == 3 &&
           length(r$dispersions[[1]]$distance) == 39 && "classe" %in% names(r$observations) && inherits(json(r), "json"))
r <- resultat("MANTEL")
verifier("Mantel : r, p, r partiel, corrélogramme, paires des deux distances", proche(r$test$r, msp$test$r) && !is.null(r$test$r_partiel) && nrow(r$correlogramme$table) > 3 &&
           length(r$paires$y) == 38 * 37 / 2 && inherits(json(r), "json"))
for (langue in c("fr", "en")) {
  t <- lapply(modeles, function(x) texte_methodes(p, x[[1]], x[[2]], langue, "essai"))
  fr <- langue == "fr"
  verifier(sprintf("texte des méthodes (%s) : PCoA, NMDS, PERMANOVA (et dispersion), Mantel ; leurs références", langue),
           any(grepl(if (fr) "coordonnées principales" else "principal coordinates", t$PCOA$paragraphes)) && any(grepl("envfit", t$PCOA$paragraphes)) &&
             any(grepl("Gower, J. C. (1966)", t$PCOA$references, fixed = TRUE)) && any(grepl("isoMDS", t$NMDS$paragraphes)) && any(grepl("Kruskal", t$NMDS$references)) &&
             any(grepl("adonis2", t$PERMANOVA$paragraphes)) && any(grepl("betadisper", t$PERMANOVA$paragraphes)) && any(grepl("Anderson, M. J. (2006)", t$PERMANOVA$references, fixed = TRUE)) &&
             any(grepl(if (fr) "partiel" else "partial", t$MANTEL$paragraphes)) && any(grepl("Holm", t$MANTEL$references)) && any(grepl("Oksanen", t$MANTEL$references)) &&
             all(vapply(t, function(x) any(grepl(if (fr) "laissées? de côté" else "left out", x$paragraphes)), TRUE)))
  verifier(sprintf("texte des méthodes (%s) : les termes de la PERMANOVA entre guillemets, comme les autres noms (l'effet de « temp »)", langue),
           any(grepl(if (fr) "a testé l'effet de « temp »" else "tested the effect of “temp”", t$PERMANOVA$paragraphes, fixed = TRUE)))
}
tc <- texte_methodes(p, dp, mp, "fr")$paragraphes
verifier("texte : la distance, les valeurs propres négatives", grepl("Bray-Curtis", tc[1]) && grepl("valeurs propres sont négatives", tc[2]))
tabs <- lapply(modeles, function(x) tables_resultats(p, x[[2]]))
verifier("tableaux exportés : des data.frame, observations d'abord (PCoA, NMDS), la table de la PERMANOVA et ses dispersions, le corrélogramme",
         all(vapply(unlist(tabs, recursive = FALSE), is.data.frame, TRUE)) && names(tabs$PCOA)[1] == "observations" && names(tabs$NMDS)[1] == "observations" &&
           all(c("valeurs_propres", "especes", "variables_ajustees", "centroides") %in% names(tabs$PCOA)) && all(c("departs", "shepard") %in% names(tabs$NMDS)) &&
           all(c("permanova", "dispersion_tests", "dispersion_paires") %in% names(tabs$PERMANOVA)) && all(c("mantel", "correlogramme") %in% names(tabs$MANTEL)))
dcond <- definir_modele(p, "PCOA", x = NULL, y = u(especes), echelle = "none", options = list(distance = "bray"), condition = 'habitat != "marais"')
mcond <- ajuster(p, dcond)
lc <- which(env$habitat != "marais" & gy)
verifier(sprintf("une condition (onglet Observations) : la PCoA de ses %d relevés, et « the N observations where … » dans le texte", length(lc)),
         identical(mcond$observations, numeros_observations(p)[lc]) && proche(mcond$noyau$valeurs, pcoa_noyau(distance_communaute(abondances[lc, ], "bray"))$valeurs) &&
           grepl(sprintf("of the %d observations where habitat != \"marais\"", length(lc)), texte_methodes(p, dcond, mcond, "en")$paragraphes[1], fixed = TRUE))

cat("Test de Mantel sur la distance géographique (spatial.R)\n")
set.seed(21)
ng <- 30
lon_g <- 4.45 + stats::runif(ng, 0, 0.3)
lat_g <- 43.4 + stats::runif(ng, 0, 0.2)
plan_g <- depuis_lonlat(lon_g, lat_g, 2154)
grad <- (lon_g - 4.45) / 0.3
ab_g <- sapply(1:6, function(j) stats::rpois(ng, exp(1 + sin(j) * 2 * grad)))
colnames(ab_g) <- sprintf("e%d", 1:6)
dg <- data.frame(nom = sprintf("g%02d", 1:ng), X = plan_g$x, Y = plan_g$y, lon = lon_g, lat = lat_g, ab_g, stringsAsFactors = FALSE)
pg <- projet_depuis_tableau(dg, "decroissance")
ug <- function(...) numeros(pg, c(...))
pg <- regler_coordonnees(pg, ug("X"), ug("Y"), 2154)
dmg <- definir_modele(pg, "MANTEL", x = numeric(0), y = ug(sprintf("e%d", 1:6)), observations = numeros_observations(pg),
                      options = list(distance = "bray", distance_x = "geographique", permutations = 199))
mg <- ajuster(pg, dmg)
yb <- distance_communaute(ab_g, "bray")
dgeo <- as.matrix(stats::dist(cbind(plan_g$x, plan_g$y))) / 1000
verifier("Mantel géographique : r = cor(Bray–Curtis, distances en km) sur les paires", proche(mg$test$r, stats::cor(yb[lower.tri(yb)], dgeo[lower.tri(dgeo)])))
verifier("… dans le texte des méthodes", grepl("distance géographique en kilomètres (en ligne droite)", paste(texte_methodes(pg, dmg, mg, "fr")$paragraphes, collapse = " "), fixed = TRUE))
pl <- regler_coordonnees(pg, ug("lon"), ug("lat"), 4326)
ml <- ajuster(pl, dmg)
hv <- distances_points(lon_g, lat_g, 4326) / 1000
verifier("… en longitudes et latitudes : le long des grands cercles", proche(ml$test$r, stats::cor(yb[lower.tri(yb)], hv[lower.tri(hv)])))
verifier("sans coordonnées ni X : refusé", nzchar(refus(ajuster(projet_depuis_tableau(dg, "sans"), definir_modele(projet_depuis_tableau(dg, "sans"), "MANTEL", x = numeric(0),
           y = ug(sprintf("e%d", 1:6)), observations = numeros_observations(pg), options = list(distance_x = "geographique"))))))

# ---------------------------------------------------------------- l'export vers R

cat("Export vers R : scripts et rapports, lancés dans une session neuve\n")
dossier <- tempfile("communautes")
dir.create(dossier)
rscript <- file.path(R.home("bin"), "Rscript")
# lance un script dans une session neuve (sans le moteur, ses graphiques sans fichier), depuis un
# autre dossier, et garde les objets nommés
lanceur <- file.path(dossier, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".r <- mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL))", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, objets) {
  sortie <- tempfile("sortie", dossier, ".rds")
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ","))),
                                       stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
# (les résultats du script, par leurs noms dans sa langue : nom_communautes)
memes_resultats <- function(s, m, langue) {
  n <- function(x) nom_communautes(x, langue)
  champ <- function(x, ...) { for (k in c(...)) x <- if (is.list(x)) x[[n(k)]]; x }
  pareil <- function(a, b) isTRUE(all.equal(unname(a), unname(b), check.attributes = FALSE))
  a <- m$ajustement
  ra <- s[[n("ajustement")]]
  ajustement <- is.null(a) || (proche(a$vecteurs$r2, champ(ra, "vecteurs", "r2"), 1e-10) && pareil(a$vecteurs$p, champ(ra, "vecteurs", "p")) &&
                                 proche(a$vecteurs$direction, champ(ra, "vecteurs", "direction"), 1e-10) &&
                                 pareil(vapply(a$facteurs, `[[`, 0, "p"), vapply(champ(ra, "facteurs"), function(f) f[[n("p")]], 0)) &&
                                 proche(a$centroides, s[[n("centroides")]], 1e-10))
  switch(m$type,
    PCOA = proche(m$noyau$valeurs, champ(s[[n("pcoa")]], "valeurs"), 1e-10) && proche(m$noyau$points, champ(s[[n("pcoa")]], "points"), 1e-10) &&
      (is.null(m$especes) || proche(m$especes, s[[n("especes")]], 1e-10)) && ajustement,
    NMDS = proche(m$noyau$points, champ(s[[n("nmds")]], "points"), 1e-10) && proche(m$noyau$stress_departs, champ(s[[n("nmds")]], "stress_departs"), 1e-10) &&
      identical(c(m$noyau$meilleur, m$noyau$retrouvee), c(champ(s[[n("nmds")]], "meilleur"), champ(s[[n("nmds")]], "retrouvee"))) &&
      proche(m$especes, s[[n("especes")]], 1e-10) && ajustement,
    PERMANOVA = {
      tb <- champ(s[[n("permanova")]], "table")
      proche(m$noyau$table$ss, tb[[n("ss")]], 1e-10) && proche(m$noyau$table$F, tb$F, 1e-10) && pareil(m$noyau$table$p, tb$p) &&
        all(vapply(names(m$dispersions), function(g) proche(m$dispersions[[g]]$distances, s[[n("dispersions")]][[g]][[n("distances")]], 1e-10) &&
                     pareil(m$dispersions[[g]]$p_permutation, s[[n("dispersions")]][[g]][[n("p_permutation")]]), TRUE))
    },
    MANTEL = {
      t <- m$test
      proche(c(t$r, t$r_partiel), c(s[[n("mantel")]]$r, champ(s[[n("mantel")]], "r_partiel")), 1e-10) && pareil(c(t$p, t$p_partiel), c(s[[n("mantel")]]$p, champ(s[[n("mantel")]], "p_partiel"))) &&
        proche(m$correlogramme$table$r, champ(s[[n("correlogramme")]], "table")$r, 1e-10) && pareil(m$correlogramme$table$p_corrige, champ(s[[n("correlogramme")]], "table")[[n("p_corrige")]])
    })
}
objets_script <- function(type, langue) nom_communautes(switch(type, PCOA = c("pcoa", "especes", "ajustement", "centroides"), NMDS = c("nmds", "especes", "ajustement", "centroides"),
                                                                PERMANOVA = c("permanova", "dispersions"), MANTEL = c("mantel", "correlogramme")), langue)
# (le même projet avec un identifiant des observations : la parcelle, 8 de 5 relevés)
pid <- p
pid$observation_id_names <- c(pid$observation_id_names, "parcelle")
pid$observations <- lapply(seq_along(pid$observations), function(k) { o <- pid$observations[[k]]; o$ids <- c(o$ids, sprintf("P%d", (k - 1) %/% 5 + 1)); o })
cas <- list(
  list(id = "pcoa", langue = "fr", quoi = "PCoA de Bray-Curtis, X ajustés (envfit), Lingoes", projet = p,
       d = definir_modele(p, "PCOA", x = u("temp", "ph", "habitat"), y = u(especes), echelle = "none", options = list(distance = "bray", permutations = B, graine = 3, correction = "lingoes"))),
  list(id = "pcoa_hellinger", langue = "en", quoi = "PCoA de Hellinger sur les racines, permutations par site", projet = p,
       d = definir_modele(p, "PCOA", x = u("temp", "habitat"), y = u(especes), echelle = "none",
                          options = list(distance = "hellinger", transformation = "racine2", permutations = B, graine = 8, strates = list(by = "variable", variable = u("site"))))),
  list(id = "pcoa_gower", langue = "fr", quoi = "PCoA de Gower sans X, Cailliez", projet = p,
       d = definir_modele(p, "PCOA", x = NULL, y = u(especes), echelle = "none", options = list(distance = "gower", correction = "cailliez"))),
  list(id = "nmds", langue = "fr", quoi = "NMDS, 20 départs, X ajustés", projet = p, d = dn),
  list(id = "nmds_jaccard", langue = "en", quoi = "NMDS en 3 dimensions, Jaccard binaire après Wisconsin", projet = p,
       d = definir_modele(p, "NMDS", x = NULL, y = u(especes), echelle = "none", options = list(distance = "jaccard", binaire = TRUE, transformation = "wisconsin", k = 3, departs = 10, graine = 2))),
  list(id = "permanova", langue = "fr", quoi = "PERMANOVA séquentielle, un terme croisé, dispersion de l'habitat", projet = p, d = dt),
  list(id = "permanova_marges", langue = "en", quoi = "PERMANOVA marginale, Lingoes, par site, dispersion à la médiane avec la variable de classe", projet = p,
       d = definir_modele(p, "PERMANOVA", x = u("temp", "altitude", "habitat"), classe = u("site"), y = u(especes), echelle = "none", croises = list(u("temp", "habitat")),
                          options = list(distance = "bray", permutations = B, graine = 6, par = "marges", correction = "lingoes", dispersion = "mediane",
                                         strates = list(by = "variable", variable = u("site"))))),
  list(id = "permanova_modele", langue = "fr", quoi = "PERMANOVA du modèle entier, Cailliez, Kulczynski sur log(x + 1)", projet = p,
       d = definir_modele(p, "PERMANOVA", x = u("temp", "habitat"), y = u(especes), echelle = "none",
                          options = list(distance = "kulczynski", transformation = "log1p", permutations = 99, graine = 2, par = "modele", correction = "cailliez"))),
  list(id = "mantel", langue = "fr", quoi = "Mantel de Pearson, X centrés-réduits", projet = p, d = dmt),
  list(id = "mantel_partiel", langue = "en", quoi = "Mantel de Spearman partiel (pH)", projet = p, d = dsp),
  list(id = "mantel_echelles", langue = "en", quoi = "Mantel : Pareto, centrage, un bloc, une catégorie, un terme croisé, 5 classes, par site", projet = p,
       d = definir_modele(p, "MANTEL", x = u("temp", "altitude", "habitat"), y = u(especes), croises = list(u("temp", "altitude")),
                          echelle = stats::setNames(c("UV", "Par", "Ctr"), c("defaut", u("temp"), u("altitude"))),
                          blocs = stats::setNames(list("B1", "B1"), u("temp", "altitude")), modificateurs = stats::setNames(list(2), u("habitat")),
                          options = list(distance = "canberra", permutations = B, graine = 4, classes = 5, strates = list(by = "variable", variable = u("site"))))),
  list(id = "permanova_parcelle", langue = "fr", quoi = "PERMANOVA de Manhattan permutée dans les parcelles (un identifiant)", projet = pid,
       d = definir_modele(pid, "PERMANOVA", x = u("temp", "habitat"), y = u(especes), echelle = "none",
                          options = list(distance = "manhattan", permutations = 99, graine = 3, strates = list(by = "id", id = "parcelle")))),
  list(id = "mantel_sans_permutation", langue = "en", quoi = "Mantel sans permutation, données en CSV", projet = p, donnees = "csv",
       d = definir_modele(p, "MANTEL", x = u("temp"), y = u(especes), echelle = "UV", options = list(distance = "chord", permutations = 0))),
  list(id = "mantel_plan", langue = "fr", quoi = "Mantel géographique, Lambert-93 (en ligne droite)", projet = pg, d = dmg),
  list(id = "mantel_lonlat", langue = "en", quoi = "Mantel géographique, longitudes et latitudes (grands cercles)", projet = pl, d = dmg),
  list(id = "pcoa_condition", langue = "en", quoi = "PCoA choisie par une condition", projet = p, d = dcond))
for (k in cas) {
  m <- ajuster(k$projet, k$d)
  r <- tryCatch(ecrire_script(k$projet, k$d, file.path(dossier, paste0(k$id, ".R")), "moteur", m = m, version = "essai", langue = k$langue, donnees = k$donnees %||% "script"),
                error = function(e) e)
  if (inherits(r, "error")) {
    verifier(sprintf("%s (%s) : écrit", k$id, k$quoi), FALSE)
    cat("    erreur :", conditionMessage(r), "\n")
    next
  }
  s <- lancer(r$script, objets_script(m$type, k$langue))
  texte <- readLines(r$script, encoding = "UTF-8")
  code <- sub("#.*$", "", texte)
  memes <- length(s) > 0 && isTRUE(memes_resultats(s, m, k$langue))
  verifier(sprintf("%s (%s, %s) : sans écart vu par le moteur ; lancé seul, mêmes résultats et mêmes p ; R seul (MASS pour un NMDS), sans vegan", k$id, k$quoi, k$langue),
           is.null(r$ecart) && memes && !any(grepl("library(", code, fixed = TRUE)) && !any(grepl("vegan::", code, fixed = TRUE)) &&
             (k$langue == "fr" || (!any(grepl("distance_tableau|pcoa_noyau|nmds_noyau|permanova_noyau|mantel_noyau|ajustement_environnement", texte)) &&
                                     !any(grepl("^ *#(fr|en)( |$)", texte)))))
  if (!is.null(r$ecart) || !isTRUE(memes)) cat("    écart :", r$ecart, "/", utils::tail(attr(s, "messages"), 3), "\n")
}
texte <- readLines(file.path(dossier, "mantel_lonlat.R"), encoding = "UTF-8")
verifier("Mantel géographique : les coordonnées dans les données du script, la distance en km le long des grands cercles (ou en ligne droite)",
         any(texte == "coordinates <- cbind(d[[\"lon\"]], d[[\"lat\"]])") && any(grepl("lonlat = TRUE) / 1000", texte, fixed = TRUE)) &&
           any(grepl("lonlat = FALSE) / 1000", readLines(file.path(dossier, "mantel_plan.R"), encoding = "UTF-8"), fixed = TRUE)))
texte <- readLines(file.path(dossier, "pcoa_condition.R"), encoding = "UTF-8")
verifier("PCoA à condition : son script choisit ses lignes par la condition, écrite en R", any(texte == "condition <- with(data_as_text, habitat != \"marais\")") &&
           any(texte == "rows <- ticked[condition[ticked] %in% TRUE]"))
texte <- readLines(file.path(dossier, "pcoa.R"), encoding = "UTF-8")
verifier("le script reprend les fonctions du moteur telles qu'il les écrit (sans leurs commentaires), ses messages dans sa langue",
         all(code_definition_moteur("pcoa_noyau", "fr") %in% texte) && any(grepl("stop(\"Distance de Gower : aucune variable du tableau Y ne varie.\", call. = FALSE)", texte, fixed = TRUE)) &&
           any(grepl("stop(\"Gower distance: no variable of the table Y varies.\", call. = FALSE)", readLines(file.path(dossier, "pcoa_hellinger.R"), encoding = "UTF-8"), fixed = TRUE)))
# les rapports : méthodes, code, résultats, références ; compilés avec knitr s'il est installé
compiler <- file.path(dossier, "compiler.R")
writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "a <- commandArgs(TRUE)", "setwd(dirname(a[1]))",
             "knitr::knit(basename(a[1]), output = sub('[.][Rq]md$', '.md', basename(a[1])), quiet = TRUE, envir = new.env())"), compiler)
rapports <- list(list(ext = "Rmd", langue = "fr", d = cas[[7]]$d, references = "Anderson, M. J. \\(2006\\)"), list(ext = "qmd", langue = "en", d = dn, references = "Kruskal"),
                 list(ext = "Rmd", langue = "en", d = dsp, references = "Smouse"))
for (i in seq_along(rapports)) {
  k <- rapports[[i]]
  dir.create(ici <- file.path(dossier, sprintf("rapport%d", i)))
  r <- ecrire_script(p, k$d, file.path(ici, paste0("rapport.", k$ext)), "moteur", m = ajuster(p, k$d), version = "essai", langue = k$langue, donnees = "csv", rdata = TRUE)
  texte <- readLines(r$script, encoding = "UTF-8")
  if (!is.null(r$ecart)) cat("    écart :", r$ecart, "\n")
  debuts <- grep("^```\\{r", texte)
  etiquettes <- sub("^```\\{r ?([^,}]*).*$", "\\1", texte[debuts])
  verifier(sprintf("rapport %s (%s, %s) : méthodes, code, résultats, références ; données en CSV et .RData à côté", k$ext, k$d$type, k$langue),
           is.null(r$ecart) && any(grepl("^```\\{r modele\\}", texte)) && any(grepl("^## (Méthodes|Methods)", texte)) && any(grepl(paste0("^- .*", k$references), texte)) &&
             !anyDuplicated(etiquettes[nzchar(etiquettes)]) && length(r$chemins) == 3 && all(file.exists(r$chemins)))
  if (requireNamespace("knitr", quietly = TRUE)) {
    messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(compiler), shQuote(r$script)), stdout = TRUE, stderr = TRUE))
    md <- file.path(ici, "rapport.md")
    verifier(sprintf("rapport %s : se compile avec knitr, avec ses tableaux et ses graphiques", k$ext),
             file.exists(md) && !any(grepl("^## Error|Error in", readLines(md, encoding = "UTF-8"))) && length(list.files(file.path(ici, "figure"), "\\.png$")) >= 2)
  } else {  # sans knitr : le code de ses blocs R tourne dans une session neuve (sans les tableaux de knitr::kable)
    fins <- vapply(debuts, function(j) j + match("```", texte[-seq_len(j)]), 0)
    blocs <- file.path(ici, "blocs.R")
    code <- unlist(lapply(seq_along(debuts), function(j) texte[seq(debuts[j] + 1, length.out = fins[j] - debuts[j] - 1)]))
    writeLines(enc2utf8(c("pdf(NULL)", code[!grepl("knitr::", code)])), blocs, useBytes = TRUE)
    avant <- setwd(ici)
    messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(blocs)), stdout = TRUE, stderr = TRUE))
    setwd(avant)
    verifier(sprintf("rapport %s : le code de ses blocs R tourne (knitr n'est pas installé : pas compilé)", k$ext), is.null(attr(messages, "status")))
  }
}
unlink(dossier, recursive = TRUE)

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
