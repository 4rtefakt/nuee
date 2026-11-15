# Vérifie les données géographiques (moteur/R/spatial.R, moteur/R/geopackage.R) : les
# conversions de coordonnées contre des valeurs de PROJ relevées, et contre PROJ lui-même (paquet
# sf) quand il est installé ; les distances ; l'I de Moran, celui des résidus d'un modèle linéaire
# et le corrélogramme contre spdep, le semi-variogramme contre gstat, quand ils le sont ; les
# GeoPackage écrits puis relus, et lus par GDAL (sf), ceux de GDAL lus ici (points, polygones,
# lignes, points en 3D) ; les coordonnées d'un projet, réglées, enregistrées et relues ; la
# demande d'autocorrélation spatiale sur une variable et sur les résidus de régressions.
#   Rscript moteur/tests/verifier_spatial.R
# NUEE_SF : une bibliothèque où sont installés sf, spdep, gstat (et RSQLite, s'il n'est pas dans
# celle de R).

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
# (des valeurs égales à `tol` près, les manquantes aux mêmes places)
proche <- function(a, b, tol) {
  a <- as.numeric(unlist(a)); b <- as.numeric(unlist(b))
  length(a) == length(b) && identical(is.na(a), is.na(b)) && isTRUE(max(abs(a - b), na.rm = TRUE) <= tol)
}

if (nzchar(Sys.getenv("NUEE_SF"))) .libPaths(c(Sys.getenv("NUEE_SF"), .libPaths()))
avec <- function(p) requireNamespace(p, quietly = TRUE)
avec_sf <- avec("sf")
avec_spdep <- avec("spdep")
avec_gstat <- avec("gstat")
avec_rsqlite <- avec("RSQLite")
cat(sprintf("(sf %s, spdep %s, gstat %s, RSQLite %s)\n", if (avec_sf) utils::packageVersion("sf") else "absent", if (avec_spdep) utils::packageVersion("spdep") else "absent",
            if (avec_gstat) utils::packageVersion("gstat") else "absent", if (avec_rsqlite) utils::packageVersion("RSQLite") else "absent"))

# ---------------------------------------------------------------- conversions de coordonnées

cat("Conversions de coordonnées\n")
# La Tour du Valat (4,6° E, 43,55° N), convertie par PROJ 9 (sf 1.1-3) : au micromètre près.
releves <- list(`2154` = c(829334.348534, 6273636.080013), `27572` = c(783148.843460, 1841303.620205),
                `32631` = c(629245.013390, 4823137.821481), `3035` = c(3883936.695443, 2286986.427576))
for (code in names(releves)) {
  n <- depuis_lonlat(4.6, 43.55, as.integer(code))
  verifier(sprintf("EPSG:%s : la Tour du Valat comme PROJ (au millimètre)", code), proche(c(n$x, n$y), releves[[code]], 1e-3))
}
set.seed(1)
lon <- stats::runif(300, -5, 9.5)
lat <- stats::runif(300, 41.3, 51.1)
codes <- c(2154, 3942, 3946, 3950, 27572, 3035, 3857, 32630, 32631, 32632, 25830, 25831, 25832, 32733)
for (code in codes) {
  lo <- if (code == 32733) stats::runif(300, 12, 18) else lon
  la <- if (code == 32733) stats::runif(300, -30, -1) else lat
  p <- depuis_lonlat(lo, la, code)
  r <- vers_lonlat(p$x, p$y, code)
  # (le Lambert II étendu passe deux fois par le changement de datum, qui laisse la hauteur : 1 mm)
  verifier(sprintf("EPSG:%d : aller et retour (%s)", code, systeme_coordonnees(code)$nom), proche(c(r$lon, r$lat), c(lo, la), if (code == 27572) 2e-8 else 1e-9))
  if (avec_sf) {
    ref <- sf::sf_project("EPSG:4326", sprintf("EPSG:%d", code), cbind(lo, la))
    verifier(sprintf("EPSG:%d : comme PROJ, vers le système (au millimètre)", code), proche(cbind(p$x, p$y), ref, 1e-3))
    retour <- sf::sf_project(sprintf("EPSG:%d", code), "EPSG:4326", ref)
    g <- vers_lonlat(ref[, 1], ref[, 2], code)
    # (1e-8 degré, environ 1 mm : le PROJ d'Ubuntu 24.04 calcule la latitude inverse de LAEA par une
    # série, à quelques 1e-9 degré de l'itération faite ici)
    verifier(sprintf("EPSG:%d : comme PROJ, vers la longitude et la latitude", code), proche(cbind(g$lon, g$lat), retour, if (code == 27572) 2e-8 else 1e-8))
  }
}
verifier("du Lambert-93 au Lambert II étendu", {
  a <- depuis_lonlat(4.6, 43.55, 2154)
  b <- convertir_coordonnees(a$x, a$y, 2154, 27572)
  proche(c(b$x, b$y), releves[["27572"]], 1e-3)
})
verifier("un système plan n'a pas de longitude ni de latitude", all(is.na(unlist(vers_lonlat(c(1, 2), c(3, 4), "plan")))))
verifier("un code inconnu est refusé", grepl("inconnu|Unknown", refus(vers_lonlat(1, 2, 99999))))
verifier("les systèmes proposés sont tous connus", all(vapply(liste_systemes(), function(s) !is.null(systeme_coordonnees(s$code)), TRUE)))
verifier("deviner : des degrés, le Lambert-93, le Lambert II étendu, sinon plan",
         identical(list(deviner_systeme(lon, lat), deviner_systeme(c(829334, 650000), c(6273636, 6860000)),
                        deviner_systeme(c(783148, 600000), c(1841303, 2400000)), deviner_systeme(c(1000, 5000), c(2000, 7000))),
                   list(4326, 2154, 27572, "plan")))

# ---------------------------------------------------------------- distances

cat("Distances\n")
d <- distances_points(c(2.3522, 5.3698), c(48.8566, 43.2965), 4326)
# (Paris – Marseille le long d'un grand cercle de rayon 6 371 008,8 m, par la loi des cosinus)
loi_cosinus <- RAYON_TERRE * acos(sin(rad(48.8566)) * sin(rad(43.2965)) + cos(rad(48.8566)) * cos(rad(43.2965)) * cos(rad(5.3698 - 2.3522)))
verifier("haversine = la loi des cosinus sphérique (Paris – Marseille)", abs(d[1, 2] - loi_cosinus) < 1e-6 && d[1, 1] == 0)
if (avec_sf) {
  pts <- sf::st_as_sf(data.frame(lon = lon[1:30], lat = lat[1:30]), coords = c("lon", "lat"), crs = 4326)
  ref <- unclass(sf::st_distance(pts))
  # (s2 prend une sphère de 6 371 010 m : les angles sont comparés)
  verifier("haversine comme sf (s2)", max(abs(distances_points(lon[1:30], lat[1:30], 4326) / RAYON_TERRE - ref / s2::s2_earth_radius_meters())) < 1e-12)
}
verifier("coordonnées planes : en ligne droite", isTRUE(all.equal(distances_points(c(0, 3), c(0, 4), 2154)[1, 2], 5)))

# ---------------------------------------------------------------- autocorrélation spatiale

cat("Autocorrélation spatiale\n")
set.seed(3)
n <- 60
x <- stats::runif(n, 0, 10000)
y <- stats::runif(n, 0, 8000)
v <- sin(x / 3000) + cos(y / 2500) + stats::rnorm(n, 0, 0.4)
D <- distances_points(x, y, 2154)
W <- poids_spatiaux(D, "voisins", k = 4)
verifier("voisins : 4 au moins chacun, symétriques, lignes de somme 1", all(rowSums(W > 0) >= 4) && all((W > 0) == t(W > 0)) && isTRUE(all.equal(rowSums(W), rep(1, n))))
Wb <- poids_spatiaux(D, "bande", standardiser = FALSE)
verifier("bande par défaut : un voisin au moins pour chacune", all(rowSums(Wb) >= 1) && isTRUE(all.equal(attr(Wb, "portee"), max(apply(D + diag(Inf, n), 1, min)))))
a <- moran(v, W, permutations = 0)
# (l'I à la main : n / S0 × Σ wij zi zj / Σ zi²)
z <- v - mean(v)
verifier("I de Moran écrit à la main", isTRUE(all.equal(a$I, n / sum(W) * sum(W * outer(z, z)) / sum(z^2))))
verifier("espérance −1/(n − 1)", isTRUE(all.equal(a$esperance, -1 / (n - 1))))
graine_avant <- .Random.seed
m1 <- moran(v, W, permutations = 199)
m2 <- moran(v, W, permutations = 199)
verifier("permutations : la même valeur p à chaque calcul, le hasard de la séance intact", identical(m1$p_permutations, m2$p_permutations) && identical(graine_avant, .Random.seed))
verifier("une variable constante est refusée", grepl("constante|constant", refus(moran(rep(1, n), W))))
if (avec_spdep) {
  co <- cbind(x, y)
  suppressWarnings({
    lw <- spdep::nb2listw(spdep::make.sym.nb(spdep::knn2nb(spdep::knearneigh(co, k = 4))), style = "W")
    b <- spdep::moran.test(v, lw, randomisation = TRUE)
    verifier("k voisins, style W : I, espérance, variance et p comme spdep::moran.test",
             proche(c(a$I, a$esperance, a$variance, a$p), c(b$estimate, b$p.value), 1e-10))
    bb <- spdep::moran.test(v, spdep::nb2listw(spdep::dnearneigh(co, 0, attr(Wb, "portee")), style = "B"), randomisation = TRUE)
    ab <- moran(v, Wb, 0)
    verifier("bande, style B : comme spdep", proche(c(ab$I, ab$variance, ab$z), c(bb$estimate[c(1, 3)], bb$statistic), 1e-10))
    nbi <- spdep::dnearneigh(co, 0, 1e9)
    gl <- lapply(spdep::nbdists(nbi, co), function(d) 1 / d)
    bi <- spdep::moran.test(v, spdep::nb2listw(nbi, glist = gl, style = "W"), randomisation = TRUE)
    ai <- moran(v, poids_spatiaux(D, "inverse"), 0)
    verifier("inverse de la distance, style W : comme spdep", proche(c(ai$I, ai$variance), bi$estimate[c(1, 3)], 1e-10))
    covariable <- stats::rnorm(n)
    ajustement <- stats::lm(v ~ covariable + x)
    al <- moran_residus_lm(stats::residuals(ajustement), stats::model.matrix(ajustement), W)
    bl <- spdep::lm.morantest(ajustement, lw)
    verifier("résidus d'un modèle linéaire : I, espérance, variance comme spdep::lm.morantest",
             proche(c(al$I, al$esperance, al$variance, al$p), c(bl$estimate, bl$p.value), 1e-10))
    cg <- correlogramme_moran(v, D, classes = 5)
    bornes <- c(cg$de, cg$a[5])
    ref <- t(vapply(1:5, function(k) {
      t <- spdep::moran.test(v, spdep::nb2listw(spdep::dnearneigh(co, bornes[k], bornes[k + 1]), style = "B", zero.policy = TRUE),
                             randomisation = TRUE, zero.policy = TRUE, adjust.n = FALSE)
      c(t$estimate[1], t$statistic)
    }, numeric(2)))
    verifier("corrélogramme : I et z de chaque classe comme spdep (anneaux de dnearneigh, style B)", proche(cbind(cg$I, cg$z), ref, 1e-10))
  })
}
vg <- variogramme(v, D, diagonale = sqrt(diff(range(x))^2 + diff(range(y))^2))
i <- which(upper.tri(D) & D <= vg$a[1] & D > 0)
verifier("semi-variogramme : la première classe écrite à la main", isTRUE(all.equal(vg$gamma[1], mean((outer(v, v, "-")^2 / 2)[i]))) && vg$paires[1] == length(i))
if (avec_gstat && avec_sf) {
  g <- gstat::variogram(v ~ 1, sf::st_as_sf(data.frame(x, y, v), coords = c("x", "y")))
  verifier("semi-variogramme comme gstat::variogram (paires, distances, γ)", nrow(vg) == nrow(g) && all(vg$paires == g$np) &&
             proche(cbind(vg$distance, vg$gamma), cbind(g$dist, g$gamma), 1e-9))
}

# ---------------------------------------------------------------- coordonnées d'un projet

cat("Coordonnées d'un projet\n")
lon_s <- 4.5 + stats::runif(n, 0, 0.3)
lat_s <- 43.4 + stats::runif(n, 0, 0.25)
l93 <- depuis_lonlat(lon_s, lat_s, 2154)
df <- data.frame(nom = sprintf("p%02d", 1:n), site = rep(sprintf("S%d", 1:12), each = 5), X_L93 = l93$x, Y_L93 = l93$y,
                 habitat = sample(c("marais", "sansouire", "prairie"), n, TRUE), abondance = stats::rpois(n, exp(1 + v / 2)), v = v,
                 stringsAsFactors = FALSE)
df$X_L93[5] <- NA
p <- projet_depuis_tableau(df, "essai")
pr <- proposer_coordonnees(p)
verifier("proposées d'après le nom : X_L93 et Y_L93, en Lambert-93", identical(list(pr$x, pr$y, pr$systeme), list(numeros(p, "X_L93"), numeros(p, "Y_L93"), 2154)))
verifier("pas de proposition sans noms de coordonnées", is.null(proposer_coordonnees(projet_depuis_tableau(df[c("nom", "v", "abondance")], "sans"))))
verifier("des degrés hors bornes refusés", grepl("180", refus(regler_coordonnees(p, numeros(p, "X_L93"), numeros(p, "Y_L93"), 4326))))
verifier("une variable qualitative refusée", nzchar(refus(regler_coordonnees(p, numeros(p, "habitat"), numeros(p, "Y_L93"), 2154))))
verifier("un système inconnu refusé", nzchar(refus(regler_coordonnees(p, numeros(p, "X_L93"), numeros(p, "Y_L93"), 123))))
p <- regler_coordonnees(p, numeros(p, "X_L93"), numeros(p, "Y_L93"), "EPSG:2154")
co <- coordonnees_projet(p)
verifier("réglées : x, y et le système", identical(list(co$nom_x, co$nom_y, co$systeme), list("X_L93", "Y_L93", 2154L)))
pts <- points_projet(p)
verifier("longitude et latitude retrouvées, NA sans coordonnée", proche(pts$lon[-5], lon_s[-5], 1e-9) && proche(pts$lat[-5], lat_s[-5], 1e-9) && is.na(pts$lon[5]))
f <- tempfile(fileext = ".nuee")
enregistrer_projet(p, f, "1.0.0")
p2 <- lire_projet(f)
verifier("enregistrées avec le projet et relues", identical(coordonnees_projet(p2)[c("x", "y", "systeme")], co[c("x", "y", "systeme")]))
verifier("retirées", is.null(coordonnees_projet(regler_coordonnees(p, NULL, NULL, NULL))))
verifier("sans coordonnées : la demande le dit", grepl("Carte|Map", refus(points_projet(regler_coordonnees(p, NULL, NULL, NULL)))))

cat("Autocorrélation spatiale d'une variable et de résidus\n")
r <- autocorrelation_spatiale(p, variable = numeros(p, "v"), permutations = 99)
verifier("une variable : 59 observations, une laissée de côté (sans coordonnée)", r$n == 59 && r$laissees == 1)
verifier("une variable : l'I de Moran de ses valeurs", isTRUE(all.equal(r$moran$I, moran(df$v[-5], poids_spatiaux(distances_points(df$X_L93[-5], df$Y_L93[-5], 2154)), 0)$I)))
d <- definir_modele(p, "LM", x = numeros(p, "habitat"), y = numeros(p, "v"), observations = numeros_observations(p), echelle = c(defaut = "none"))
m <- ajuster(p, d)
rr <- autocorrelation_spatiale(p, m, permutations = 0)
verifier("résidus d'un modèle linéaire : le test qui tient compte de l'ajustement", !is.null(rr$moran$modele) && rr$n == 59)
if (avec_spdep) {
  ok <- !is.na(df$X_L93)
  a <- stats::lm(v ~ habitat, data = df[ok, ])
  co_ok <- cbind(df$X_L93[ok], df$Y_L93[ok])
  lw <- spdep::nb2listw(spdep::make.sym.nb(spdep::knn2nb(spdep::knearneigh(co_ok, k = 4))), style = "W")
  bl <- suppressWarnings(spdep::lm.morantest(a, lw))
  verifier("… comme spdep::lm.morantest sur lm(v ~ habitat)", proche(c(rr$moran$modele$I, rr$moran$modele$esperance, rr$moran$modele$variance), bl$estimate, 1e-8))
}
dg <- definir_modele(p, "GLM", x = numeros(p, "habitat"), y = numeros(p, "abondance"), observations = numeros_observations(p),
                     echelle = c(defaut = "none"), famille = "poisson")
mg <- ajuster(p, dg)
rq <- autocorrelation_spatiale(p, mg, residus = "quantiles", permutations = 0)
verifier("résidus quantiles d'un modèle de Poisson", rq$n == 59 && is.null(rq$moran$modele) && isTRUE(all.equal(rq$valeurs, mg$resultat$residus_quantiles[-5])))
verifier("résidus quantiles refusés pour la loi normale", grepl("quantile", refus(autocorrelation_spatiale(p, m, residus = "quantiles"))))
pl <- autocorrelation_spatiale(p, variable = numeros(p, "v"), permutations = 0, par_lieu = TRUE)
verifier("par lieu : une valeur par lieu (ici chacun différent)", pl$n == 59 && all(pl$lieux == 1))
dup <- df
dup$X_L93 <- rep(dup$X_L93[seq(1, n, 5)], each = 5)
dup$Y_L93 <- rep(dup$Y_L93[seq(1, n, 5)], each = 5)
pd <- projet_depuis_tableau(dup, "placettes")
pd <- regler_coordonnees(pd, numeros(pd, "X_L93"), numeros(pd, "Y_L93"), 2154)
ad <- autocorrelation_spatiale(pd, variable = numeros(pd, "v"), permutations = 0, par_lieu = TRUE)
verifier("par lieu : 12 placettes de 5 années, la moyenne de chacune", ad$n == 12 && sum(ad$lieux) == 60 &&
           isTRUE(all.equal(ad$valeurs[2], mean(df$v[6:10]))))
ae <- autocorrelation_spatiale(pd, variable = numeros(pd, "v"), permutations = 0)
verifier("sans regrouper : les paires au même endroit comptées", ae$nulles == 12 * 10)
# (le voisinage que la carte envoie pour une bande ou l'inverse de la distance sans portée donnée)
verifier("bande ou inverse de la distance sans portée : la portée par défaut",
         isTRUE(all.equal(autocorrelation_spatiale(p, variable = numeros(p, "v"), voisinage = list(methode = "bande"), permutations = 0)$moran$I,
                          moran(df$v[-5], poids_spatiaux(distances_points(df$X_L93[-5], df$Y_L93[-5], 2154), "bande"), 0)$I)) &&
           is.finite(autocorrelation_spatiale(p, variable = numeros(p, "v"), voisinage = list(methode = "inverse"), permutations = 0)$moran$I))
verifier("l'inverse de la distance avec des observations au même endroit : refusé, la moyenne par lieu proposée ; par lieu, calculé",
         grepl("Moyenne par lieu", refus(autocorrelation_spatiale(pd, variable = numeros(pd, "v"), voisinage = list(methode = "inverse"), permutations = 0))) &&
           is.finite(autocorrelation_spatiale(pd, variable = numeros(pd, "v"), voisinage = list(methode = "inverse"), permutations = 0, par_lieu = TRUE)$moran$I))

# ---------------------------------------------------------------- lissage spatial d'un GAM

cat("Lissage spatial s(x, y) d'un modèle additif
")
set.seed(11)
n2 <- 150
lon2 <- 4.45 + stats::runif(n2, 0, 0.35)
lat2 <- 43.42 + stats::runif(n2, 0, 0.22)
p93 <- depuis_lonlat(lon2, lat2, 2154)
x2 <- stats::rnorm(n2)
compte <- stats::rpois(n2, exp(0.5 + 0.3 * x2 + sin((lon2 - 4.45) * 12)))
y2 <- 2 + 0.8 * x2 + sin((lon2 - 4.45) * 12) + cos((lat2 - 43.42) * 20) + stats::rnorm(n2, 0, 0.3)
df2 <- data.frame(nom = sprintf("o%03d", 1:n2), X = p93$x, Y = p93$y, lon = lon2, lat = lat2, x = x2, y = y2, compte = compte, stringsAsFactors = FALSE)
df2$X[3] <- NA
ps <- projet_depuis_tableau(df2, "lissage")
us <- function(...) numeros(ps, c(...))
dg <- function(..., famille = "gaussienne", y = "y") definir_modele(ps, "GAM", x = us("x"), y = us(y), observations = numeros_observations(ps),
                                                                     echelle = c(defaut = "none"), famille = famille, ...)
d1 <- dg(lisse_spatial = list(variables = us("X", "Y")))
m1 <- ajuster(ps, d1)
ok2 <- !is.na(df2$X)
ref <- mgcv::gam(y ~ x + s(X, Y, k = 30), data = df2[ok2, ], method = "REML")
ref_ml <- mgcv::gam(y ~ x + s(X, Y, k = 30), data = df2[ok2, ], method = "ML")
verifier("plaques minces : le coefficient, l'edf, F et p de mgcv::gam(y ~ x + s(X, Y))",
         proche(c(m1$resultat$coefficients$estimation[2], m1$resultat$courbes$edf, m1$resultat$courbes$statistique),
                c(stats::coef(ref)["x"], summary(ref)$s.table[1, c(1, 3)]), 1e-6))
verifier("… son AIC (ML), une observation sans coordonnée laissée de côté", proche(m1$resultat$AIC, stats::AIC(ref_ml), 1e-6) && m1$N == 149)
verifier("… les prédictions de nouvelles observations", proche(predire(ps, m1, d1, numeros_observations(ps)[1:5])[[1]], stats::predict(ref, df2[1:5, ]), 1e-8))
verifier("… la validation croisée faite avec lui (Q2)", is.finite(m1$vc$Q2) && m1$vc$Q2 > 0.8)
d2 <- dg(lisse_spatial = list(variables = us("lon", "lat"), base = "sos", k = 40))
m2 <- ajuster(ps, d2)
ref2 <- mgcv::gam(y ~ x + s(lat, lon, bs = "sos", k = 40), data = df2, method = "REML")
verifier("sur la sphère : comme mgcv::gam(y ~ x + s(lat, lon, bs = \"sos\", k = 40))",
         proche(c(m2$resultat$coefficients$estimation[2], m2$resultat$courbes$edf), c(stats::coef(ref2)["x"], summary(ref2)$s.table[1, 1]), 1e-6))
d3 <- dg(lisse_spatial = list(variables = us("X", "Y"), k = 20), famille = "poisson", y = "compte")
m3 <- ajuster(ps, d3)
ref3 <- mgcv::gam(compte ~ x + s(X, Y, k = 20), data = df2[ok2, ], family = poisson(), method = "REML")
verifier("Poisson, k = 20 : comme mgcv::gam", proche(c(m3$resultat$coefficients$estimation[2], m3$resultat$courbes$edf), c(stats::coef(ref3)["x"], summary(ref3)$s.table[1, 1]), 1e-6))
# les moyennes ajustées d'un modèle à lissage spatial : chaque catégorie au point moyen des coordonnées
# (et x à sa moyenne), comme emmeans le fait d'une covariable
dfm <- transform(df2[ok2, ], habitat = c("dune", "marais", "pinede")[seq_len(sum(ok2)) %% 3 + 1])
pm <- projet_depuis_tableau(dfm, "lissage_moyennes")
um <- function(...) numeros(pm, c(...))
dm <- definir_modele(pm, "GAM", x = um("x", "habitat"), y = um("y"), observations = numeros_observations(pm), echelle = c(defaut = "none"),
                     famille = "gaussienne", lisse_spatial = list(variables = um("X", "Y")))
mm <- ajuster(pm, dm)
rm <- moyennes_ajustees(pm, mm, dm)
refm <- mgcv::gam(y ~ x + habitat + s(X, Y, k = 30), data = dfm, method = "REML")
gm <- stats::predict(refm, data.frame(x = mean(dfm$x), habitat = c("dune", "marais", "pinede"), X = mean(dfm$X), Y = mean(dfm$Y)), se.fit = TRUE)
verifier("moyennes ajustées avec un lissage spatial : celles de mgcv au point moyen des coordonnées, avec leurs erreurs-types",
         proche(rm$moyennes$estimation, as.vector(gm$fit), 1e-6) && proche(rm$moyennes$erreur_type, as.vector(gm$se.fit), 1e-6) &&
           proche(c(rm$grille$spatial$x, rm$grille$spatial$y), c(mean(dfm$X), mean(dfm$Y)), 1e-10))
mm$moyennes <- rm
verifier("… dites dans le texte des méthodes, en français et en anglais",
         grepl("au point moyen des coordonnées", paste(texte_methodes(pm, dm, mm, "fr")$paragraphes, collapse = " "), fixed = TRUE) &&
           grepl("at the mean point of the coordinates", paste(texte_methodes(pm, dm, mm, "en")$paragraphes, collapse = " "), fixed = TRUE))
pc <- regler_coordonnees(ps, us("X"), us("Y"), 2154)
sans <- autocorrelation_spatiale(pc, ajuster(ps, dg()), permutations = 0)
avec <- autocorrelation_spatiale(pc, m1, permutations = 0)
verifier("les résidus gardent moins d'autocorrélation avec lui", sans$moran$I > 0.5 && avec$moran$I < 0.1)
verifier("dans le texte des méthodes, en français et en anglais",
         grepl("lissage spatial des coordonnées (« X » et « Y »), une spline de régression en plaques minces isotrope (Wood, 2003) de dimension de base 30", paste(texte_methodes(ps, d1, m1, "fr")$paragraphes, collapse = " "), fixed = TRUE) &&
           grepl("spline on the sphere (Wahba, 1981; Wood, 2017) of basis dimension 40", paste(texte_methodes(ps, d2, m2, "en")$paragraphes, collapse = " "), fixed = TRUE))
r1 <- reglages_modele(ps, d1)
verifier("dans la formule : écrite, puis relue", identical(formule_reglages(ps, r1), "y ~ x + s(X, Y)") &&
           identical(as.numeric(lire_formule(ps, "y ~ x + s(lat, lon, bs = \"sos\", k = 40)")$lisse_spatial$variables), us("lon", "lat")) &&
           identical(formule_reglages(ps, reglages_modele(ps, d2)), "y ~ x + s(lat, lon, bs = \"sos\", k = 40)"))
verifier("refus : hors d'un GAM, une variable qualitative, des degrés hors bornes pour la sphère, k hors bornes",
         nzchar(refus(definir_modele(ps, "GLM", x = us("x"), y = us("y"), observations = numeros_observations(ps), famille = "gaussienne", lisse_spatial = list(variables = us("X", "Y"))))) &&
           nzchar(refus(dg(lisse_spatial = list(variables = us("X", "Y"), base = "sos")))) &&
           nzchar(refus(dg(lisse_spatial = list(variables = us("X", "Y"), k = 2)))))
verifier("la sélection par AIC le garde dans chaque modèle", nrow(selection_aic(m1, d1)$modeles) == 2)

# L'export vers R (script.R) : le script écrit les coordonnées dans les colonnes sx et sy du tableau
# du modèle et le terme dans la formule (plaques minces, ou sphère ; sa dimension sur les lignes de
# chaque ajustement) ; il est exécuté dans le moteur avant d'être écrit (`ecart`), puis lancé dans une
# session R neuve, et ses objets comparés au modèle par ecart_regression : coefficients, AIC et
# vraisemblance, R², variances, courbes et la ligne du lissage (edf, test, concurvité), validation
# croisée, valeurs ajustées et résidus, résidus quantiles, jeu de test, sélection par l'AIC.
cat("Export vers R d'un modèle additif à lissage spatial, en français et en anglais, et en rapport R Markdown\n")
dossier_s <- tempfile("lissage_script")
dir.create(dossier_s)
rscript <- file.path(R.home("bin"), "Rscript")
lanceur <- file.path(dossier_s, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".r <- mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL))", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, objets) {
  sortie <- tempfile("sortie", dossier_s, ".rds")
  ici <- setwd(dossier_s)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ","))),
                                       stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
psi <- ps
psi$observation_id_names <- c("Observation", "parcelle")
for (i in seq_along(psi$observations)) psi$observations[[i]]$ids <- c(psi$observations[[i]]$ids[1], sprintf("P%02d", (i - 1) %/% 10 + 1))
dgi <- function(..., famille = "gaussienne", y = "y", x = "x") definir_modele(psi, "GAM", x = numeros(psi, x), y = numeros(psi, y), observations = numeros_observations(psi),
                                                                               echelle = c(defaut = "none"), famille = famille, ...)
essayer_spatial <- function(nom, d, langue, selection = FALSE) {
  m <- ajuster(psi, d)
  if (selection) m$selection_aic <- selection_aic(m, d)
  r <- ecrire_script(psi, d, file.path(dossier_s, paste0(nom, ".R")), "moteur", m = m, version = "essai", langue = langue)
  objets <- nom_script(c("coefficients", "criteres", "r2", "variances", "phi", "parametre_loi", "dispersion", "courbes", "seuils", "vc", "observations", "test", "selection"), langue)
  s <- lancer(r$script, objets)
  pl <- plan_modele(psi, d, m, tableau_script(psi, ids_script(list(d), psi)))
  difference <- if (length(s)) ecart_regression(pl, m, list2env(s), langue) else paste(utils::tail(attr(s, "messages"), 3), collapse = " / ")
  texte <- readLines(r$script, encoding = "UTF-8")
  spatial <- s[[nom_script("observations", langue)]][[nom_script("lissage_spatial", langue)]]
  attendu <- observations_regression(psi, m)[[m$spatial$nom]]
  verifier(sprintf("%s (%s) : sans écart vu par le moteur ; lancé seul, les résultats du moteur, la ligne du lissage dans le tableau des courbes, sa valeur en chaque observation", nom, langue),
           is.null(r$ecart) && is.null(difference) && length(spatial) == m$N && isTRUE(all.equal(spatial, attendu, tolerance = 1e-8)) &&
             any(grepl("tab$sx <- ", texte, fixed = TRUE)) && any(grepl(if (identical(m$spatial$base, "sos")) "s(sy, sx, bs = \\\"sos\\\"" else "s(sx, sy, k = %d)", texte, fixed = TRUE)))
  if (!is.null(r$ecart) || !is.null(difference)) cat("    écart :", r$ecart, "/", difference, "\n")
  invisible(list(r = r, m = m, s = s))
}
essayer_spatial("plaques_minces_selection", dgi(lisse_spatial = list(variables = numeros(psi, c("X", "Y")))), "fr", selection = TRUE)
essayer_spatial("sphere_k40_en", dgi(lisse_spatial = list(variables = numeros(psi, c("lon", "lat")), base = "sos", k = 40)), "en")
essayer_spatial("poisson_k20", dgi(lisse_spatial = list(variables = numeros(psi, c("X", "Y")), k = 20), famille = "poisson", y = "compte"), "fr")
essayer_spatial("courbe_et_lissage_test_en", dgi(x = c("x", "compte"), lisses = numeros(psi, "compte"), lisse_spatial = list(variables = numeros(psi, c("X", "Y")), k = 20),
                                                 test = numeros_observations(psi)[seq(5, n2, 15)]), "en")
essayer_spatial("effet_aleatoire_lissage", dgi(aleatoires = list(list(by = "id", id = "parcelle")), lisse_spatial = list(variables = numeros(psi, c("X", "Y")), k = 15)), "fr")
dr <- dgi(lisse_spatial = list(variables = numeros(psi, c("X", "Y")), k = 20), famille = "poisson", y = "compte", nom = "R1")
mr <- ajuster(psi, dr)
r <- ecrire_script(psi, dr, file.path(dossier_s, "R1.Rmd"), "moteur", m = mr, version = "essai", langue = "fr")
texte <- readLines(r$script, encoding = "UTF-8")
verifier("R1.Rmd : son tableau des courbes (la ligne du lissage), sa carte, Wood (2003) dans les références",
         is.null(r$ecart) && any(grepl("^```\\{r courbes-edf\\}", texte)) && any(grepl("^```\\{r lissage-spatial,", texte)) && any(grepl("^- Wood, S. N. \\(2003\\)", texte)))
if (requireNamespace("knitr", quietly = TRUE)) {
  compiler <- file.path(dossier_s, "compiler.R")
  writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "a <- commandArgs(TRUE)", "setwd(dirname(a[1]))",
               "knitr::knit(basename(a[1]), output = sub('[.][Rq]md$', '.md', basename(a[1])), quiet = TRUE, envir = new.env())"), compiler)
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(compiler), shQuote(r$script)), stdout = TRUE, stderr = TRUE))
  md <- sub("[.]Rmd$", ".md", r$script)
  verifier("R1.Rmd se compile avec knitr, avec ses graphiques", file.exists(md) && !any(grepl("^## Error|Error in", readLines(md, encoding = "UTF-8"))) &&
             length(list.files(file.path(dossier_s, "figure"), "\\.png$")) >= 3)
} else cat("  (knitr absent : le rapport n'est pas compilé)\n")
# la séance : un modèle linéaire, puis le modèle additif à lissage spatial, dans le même script
definitions <- list(definir_modele(psi, "LM", x = numeros(psi, "x"), y = numeros(psi, "y"), echelle = "none", nom = "L1"), dr)
ms <- lapply(definitions, function(d) ajuster(psi, d))
r <- ecrire_script_seance(psi, definitions, ms, file.path(dossier_s, "seance.R"), "moteur", version = "essai", langue = "en")
s <- lancer(r$script, c("L1", "R1"))
verifier("a session with a linear model and the additive model with its spatial smooth: each under its name, the engine's coefficients and curves",
         is.null(r$ecart) && length(s) == 2 && isTRUE(all.equal(as.matrix(s$R1$coefficients[2:8]), as.matrix(mr$resultat$coefficients[c("estimation", "erreur_type", "ddl", "t", "p", "bas", "haut")]),
                                                                check.attributes = FALSE, tolerance = 1e-8)) &&
           isTRUE(all.equal(as.matrix(Filter(is.numeric, s$R1$curves)), as.matrix(Filter(is.numeric, mr$resultat$courbes)), check.attributes = FALSE, tolerance = 1e-8)))
unlink(dossier_s, recursive = TRUE)

# ---------------------------------------------------------------- GeoPackage

if (!avec_rsqlite) {
  cat("(RSQLite absent : GeoPackage non vérifié)\n")
} else {
  cat("GeoPackage\n")
  dossier <- tempfile()
  dir.create(dossier)
  g <- file.path(dossier, "essai.gpkg")
  donnees <- tableau_projet(p)
  ecrire_geopackage(donnees, g, pts$x, pts$y, 2154, "relevés 2024")
  verifier("une couche", identical(couches_geopackage(g), "relevés_2024"))
  lu <- lire_geopackage(g)
  verifier("relu : les colonnes, puis X et Y", identical(names(lu), c(names(donnees), "X", "Y")) && nrow(lu) == n)
  verifier("relu : les coordonnées, une ligne sans point", proche(lu$X[-5], pts$x[-5], 1e-6) && is.na(lu$X[5]) && is.na(lu$Y[5]))
  verifier("relu : le système", identical(attr(lu, "coordonnees")$systeme, 2154L))
  pg <- importer_tableau(g)
  verifier("ouvert comme un projet : ses coordonnées réglées", identical(coordonnees_projet(pg)$nom_x, "X") && identical(coordonnees_projet(pg)$systeme, 2154L))
  tables <- tables_resultats(p, m)
  gr <- file.path(dossier, "resultats.gpkg")
  ecrire_geopackage_resultats(p, m, tables, gr, "M1")
  verifier("les résultats d'un modèle : les observations en points, les autres tableaux à côté",
           identical(couches_geopackage(gr), c("M1", setdiff(names(Filter(Negate(is.null), tables)), "observations"))))
  ro <- lire_geopackage(gr, "M1")
  k <- match(ro$Observation, df$nom)
  verifier("… chaque observation à sa place", proche(ro$X[!is.na(ro$X)], df$X_L93[k][!is.na(ro$X)], 1e-6))
  for (code in c(4326, 27572, 32631, 3035, "plan")) {
    gc <- file.path(dossier, sprintf("s%s.gpkg", code))
    q <- if (identical(code, "plan")) list(x = pts$x, y = pts$y) else convertir_coordonnees(pts$x, pts$y, 2154, code)
    ecrire_geopackage(donnees[c("Observation", "v")], gc, q$x, q$y, code)
    lc <- lire_geopackage(gc)
    verifier(sprintf("écrit et relu dans le système %s", code), identical(as.character(attr(lc, "coordonnees")$systeme), as.character(if (identical(code, "plan")) "plan" else code)))
    if (avec_sf) {
      s <- sf::st_read(gc, quiet = TRUE)
      verifier(sprintf("lu par GDAL (sf) : %s, les mêmes points", code),
               (identical(code, "plan") || identical(as.integer(sf::st_crs(s)$epsg), as.integer(code))) &&
                 proche(sf::st_coordinates(s)[, 1:2], cbind(q$x, ifelse(is.na(q$x), NA, q$y)), 1e-9))  # (un point sans x : vide)
    }
  }
  if (avec_sf) {
    sf::sf_use_s2(FALSE)
    nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
    nc <- sf::st_transform(nc[1:20, c("NAME", "AREA")], 2154)
    f2 <- file.path(dossier, "nc.gpkg")
    sf::st_write(nc, f2, quiet = TRUE)
    r2 <- lire_geopackage(f2)
    cen <- sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_geometry(nc))))
    verifier("polygones de GDAL : leur centre de gravité comme sf::st_centroid", proche(cbind(r2$X, r2$Y), cen, 1e-3) && length(attr(r2, "note")) == 1)
    lignes <- sf::st_cast(sf::st_boundary(nc[1:5, ]), "MULTILINESTRING")
    f3 <- file.path(dossier, "lignes.gpkg")
    sf::st_write(lignes, f3, quiet = TRUE)
    r3 <- lire_geopackage(f3)
    cl <- sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_geometry(lignes))))
    verifier("lignes de GDAL : le centre de leurs segments comme sf::st_centroid", proche(cbind(r3$X, r3$Y), cl, 1e-3))
    pz <- sf::st_as_sf(data.frame(a = 1:3, x = 1:3, y = 4:6, z = 7:9), coords = c("x", "y", "z"), crs = 32631)
    f4 <- file.path(dossier, "z.gpkg")
    sf::st_write(pz, f4, quiet = TRUE)
    r4 <- lire_geopackage(f4)
    verifier("points en 3D de GDAL : x et y", identical(r4$X, c(1, 2, 3)) && identical(r4$Y, c(4, 5, 6)) && identical(attr(r4, "coordonnees")$systeme, 32631L))
  }
}

cat(if (echecs) sprintf("%d échec(s)\n", echecs) else "Tout est bon.\n")
if (echecs) quit(status = 1)
