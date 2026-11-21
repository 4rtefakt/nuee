# The figures of nuee.io: computed by Nuée's own engine, all from one study, the skylarks of the
# monitoring tutorial (docs/site/donnees) and surveys of the same birds made up here (their songs,
# a trial of skylark plots, the other birds of the squares), and written to vitrine/src/donnees.json,
# which construire.mjs puts in the pages. Nothing on the site is drawn by hand: run this again
# after a change to the engine or to the data.
#   Rscript vitrine/donnees.R        (from the repository's folder; needs jsonlite, as the engine)
#
# Each figure is a list of points (x, y, group) and of lines for the swarm, and of "deco" for what
# is drawn crisp over it (axes, ticks, an ellipse, hulls), all in the unit square, y upwards:
# site.js only places them. The map's figure is laid on a picture, src/assets/carte.webp: run
# carte.py before this when the place shown changes.

source("moteur/load.R", encoding = "UTF-8")

arrondi <- function(x, n = 4) round(x, n)
# x from [a, b] to [bas, haut] of the unit square
vers <- function(x, a, b, bas = 0, haut = 1) bas + (x - a) / (b - a) * (haut - bas)
points_json <- function(x, y, g) unname(Map(function(a, b, c) c(arrondi(a), arrondi(b), c), x, y, g))
ligne_json <- function(x, y) unname(Map(function(a, b) c(arrondi(a), arrondi(b)), x, y))
graduations <- function(valeurs, positions, cote) unname(Map(function(v, p) list(v = v, p = arrondi(p), cote = cote), valeurs, positions))

# ------------------------------------------------------------ 1. the skylarks' sites in a PCA
# The 40 sites of the monitoring tutorial's counts (simulated: suivi.R), each described by its 17
# springs: the counts put a column a year, on a log scale (log10(x + 1)) and to unit variance. The
# springs a site missed stay empty: NIPALS fits without them. Two components, for the Scores view
# (cross-validation keeps the first only), and the Hotelling T² ellipse as that view draws it
# (serveur.R: variance_t and ellipse). The site outside the ellipse is named on the figure.
comptes <- utils::read.csv("docs/site/donnees/skylark_counts.csv")
habitat_site <- tapply(comptes$habitat, comptes$site, `[`, 1)
par_printemps <- tapply(comptes$skylarks, list(comptes$site, comptes$year), `[`, 1)
en_large <- data.frame(site = rownames(par_printemps), par_printemps, check.names = FALSE)
names(en_large)[-1] <- paste0("y", colnames(par_printemps))
ps <- projet_depuis_tableau(en_large, "skylark sites")
printemps <- numeros(ps, names(en_large)[-1])
d_acp <- definir_modele(ps, "PCA", x = printemps, transformations = stats::setNames(as.list(rep("log10:1", length(printemps))), printemps))
m_acp <- ajuster(ps, d_acp, A = 2)
stopifnot(identical(en_large$site, unname(ps$noms_obs[as.character(m_acp$observations)])))
t12 <- m_acp$T[, 1:2]
N <- nrow(t12)
t2_95 <- 2 * (N - 1) / (N - 2) * stats::qf(0.95, 2, N - 2)
rayons <- sqrt(colSums(t12^2) / (N - 1) * t2_95)
bx <- ceiling(max(abs(t12[, 1]), rayons[1]) * 1.04 * 2) / 2
by <- ceiling(max(abs(t12[, 2]), rayons[2]) * 1.04 * 2) / 2
angle <- seq(0, 2 * pi, length.out = 97)
comp <- m_acp$statistiques$composantes
couleur_site <- ifelse(habitat_site[en_large$site] == "cropland", 3, 2)  # (as on the first screen)
dehors <- which((t12[, 1] / rayons[1])^2 + (t12[, 2] / rayons[2])^2 > 1)
# what the page says of the figure: the first axis is how many skylarks a site holds, and the one
# site outside the ellipse holds the most
moyenne_site <- tapply(comptes$skylarks, comptes$site, mean, na.rm = TRUE)[en_large$site]
stopifnot(length(dehors) == 1, which.max(moyenne_site) == dehors, bx > 8, by > 3,
          abs(stats::cor(t12[, 1], rowMeans(log10(par_printemps + 1), na.rm = TRUE))) > 0.95)
acp <- list(
  points = points_json(vers(t12[, 1], -bx, bx), vers(t12[, 2], -by, by), couleur_site),
  deco = list(
    axes = list(c(0, 0.5, 1, 0.5), c(0.5, 0, 0.5, 1)),
    ellipse = ligne_json(vers(rayons[1] * cos(angle), -bx, bx), vers(rayons[2] * sin(angle), -by, by)),
    gx = graduations(c(-8, -4, 4, 8), vers(c(-8, -4, 4, 8), -bx, bx), "x"),
    gy = graduations(c(-3, 3), vers(c(-3, 3), -by, by), "y"),
    noms = lapply(dehors, function(i) list(nom = en_large$site[i], x = arrondi(vers(t12[i, 1], -bx, bx)), y = arrondi(vers(t12[i, 2], -by, by)),
                                           g = couleur_site[[i]]))),
  r2x = arrondi(comp$R2X[1:2], 3), n = N, k = m_acp$K, dehors = en_large$site[dehors])

# ------------------------------------------------------------ 2. the skylarks' songs, as spectra
# 120 recordings of singing skylarks, three on each square (made up here, with a fixed seed), each
# as its long-term average spectrum: the level, in dB, from 1 to 7 kHz in steps of 50 Hz. A song
# fills 2 to 7 kHz, louder or fainter with the bird's distance; under it, the hum of the nearest
# road, loud on some squares, where the birds sing a little higher (as birds near roads do). Six
# are drawn: a recording from each of six squares, from the quietest to the loudest, after the
# engine's SNV (spectres.R), which takes each recording's own level and spread out of it: what is
# left is the shape of the song, and the hum.
set.seed(20261117)
frequences <- seq(1, 7, by = 0.05)
route <- stats::runif(nrow(en_large))  # the road's hum on each square, from none (0) to loud (1)
spectre_chant <- function(bruit) {
  centre <- 3.4 + 0.7 * bruit + stats::rnorm(1, 0, 0.1)
  chant <- 10^((62 + stats::rnorm(1, 0, 2.5)) / 10) *
    (exp(-(frequences - centre)^2 / (2 * 0.95^2)) + 0.3 * exp(-(frequences - centre - 2.2)^2 / (2 * 0.7^2)))
  fond <- 10^4.25 / frequences
  trafic <- 10^((42 + 24 * bruit) / 10) * exp(-(frequences - 1) / 0.4)
  grain <- as.numeric(stats::filter(stats::rnorm(length(frequences) + 8, 0, 1.1), rep(1 / 9, 9)))[4 + seq_along(frequences)]
  10 * log10(chant + fond + trafic) + grain
}
bruit_chant <- rep(route, each = 3)
chants <- t(vapply(bruit_chant, spectre_chant, frequences))
pic <- frequences[frequences >= 2.5][apply(chants[, frequences >= 2.5], 1, which.max)]  # each song's loudest frequency
montee <- diff(tapply(pic, bruit_chant >= 0.5, mean))  # how much higher by the loud roads, kHz
stopifnot(stats::cor(pic, bruit_chant) > 0.5, montee > 0.1)
trois <- function(carres) carres[order(route[carres])][round(seq(2, length(carres) - 1, length.out = 3))]
montres <- c(trois(which(route < 1 / 3)), trois(which(route > 2 / 3)))  # (the quiet third and the loud third)
S <- pretraiter_spectres(chants, list(list(type = "snv")))$X[(montres - 1) * 3 + 1, ]
etendue <- range(S)
spectres <- list(
  lignes = lapply(seq_along(montres), function(i) list(
    g = if (route[montres[i]] < 0.5) 0 else 1,
    pts = ligne_json(vers(frequences, 1, 7), vers(S[i, ], etendue[1], etendue[2], 0.04, 0.96)))),
  deco = list(
    axes = list(c(0, 0, 1, 0)),
    gx = graduations(c(2, 4, 6), vers(c(2, 4, 6), 1, 7), "x"),
    # where the road's hum is: under 2 kHz
    bandes = list(arrondi(vers(c(1, 2), 1, 7)))),
  n = nrow(chants), k = length(frequences), montee = as.numeric(round(1000 * montee, -1)))

# ------------------------------------------------------------ 3. skylark plots: before, after, with controls
# Skylark territories per km² on ten farms, 2010–2023 (made up here, with a fixed seed): from 2016,
# five of them leave skylark plots, bare patches in their winter cereals; the five others are the
# controls. Every farm loses 0.3 territory a year, as the counts fall; the plots add 6. The engine's
# BACI view gives each year's gap between the two groups; its mixed model (after × plots, a random
# intercept for the farm, CAR1 along the years) gives the effect: the model the page's document
# cites, named BACI.
set.seed(2016)
annees_p <- 2010:2023
fermes <- c(sprintf("P%d", 1:5), sprintf("T%d", 1:5))
placettes <- rep(c(1, 0), each = 5)
niveau_ferme <- 21 + 1.8 * c(scale(stats::rnorm(5), scale = FALSE), scale(stats::rnorm(5), scale = FALSE))  # (the two groups start level)
effet_an <- stats::rnorm(length(annees_p), 0, 0.9)
parcelles <- do.call(rbind, lapply(seq_along(fermes), function(f) {
  e <- as.numeric(stats::arima.sim(list(ar = 0.5), length(annees_p), sd = 1.35))
  apres <- as.numeric(annees_p >= 2016)
  data.frame(obs = paste(fermes[f], annees_p, sep = "-"), farm = fermes[f], year = annees_p, after = apres, plots = placettes[f],
             territories = round(niveau_ferme[f] - 0.3 * (annees_p - 2010) + effet_an + e + 6 * apres * placettes[f], 1))
}))
pp <- projet_depuis_tableau(parcelles, "skylark plots")
up <- function(nom) numeros(pp, nom)
ecarts_p <- baci(pp, up("territories"), up("year"), up("farm"), impact = fermes[placettes == 1], temoins = fermes[placettes == 0], evenement = 2016)
d_baci <- definir_modele(pp, "LMM", x = up(c("after", "plots")), y = up("territories"), echelle = "none", croises = list(up(c("after", "plots"))),
                         aleatoires = list(list(by = "variable", variable = up("farm"))),
                         correlation = list(type = "CAR1", temps = list(by = "variable", variable = up("year"))), nom = "BACI")
m_baci <- ajuster(pp, d_baci)
effet_baci <- m_baci$resultat$coefficients[m_baci$resultat$coefficients$terme == "after*plots", ]
stopifnot(nrow(effet_baci) == 1, effet_baci$bas > 0)
ymin <- 10
ymax <- 34
stopifnot(parcelles$territories > ymin, parcelles$territories < ymax)
set.seed(3)
decalage <- stats::runif(nrow(parcelles), -0.2, 0.2)  # the farms of a year side by side
moyennes <- stats::aggregate(territories ~ plots + after, parcelles, mean)
segment <- function(avec, apres) {
  y <- vers(moyennes$territories[moyennes$plots == avec & moyennes$after == apres], ymin, ymax)
  x <- vers(if (apres) c(2015.6, 2023.4) else c(2009.6, 2015.4), 2009.3, 2023.7)
  list(g = avec, pts = ligne_json(x, c(y, y)))
}
baci <- list(
  points = points_json(vers(parcelles$year + decalage, 2009.3, 2023.7), vers(parcelles$territories, ymin, ymax), parcelles$plots),
  deco = list(
    axes = list(c(0, 0, 1, 0), c(0, 0, 0, 1)),
    gx = graduations(c(2010, 2013, 2016, 2019, 2022), vers(c(2010, 2013, 2016, 2019, 2022), 2009.3, 2023.7), "x"),
    gy = graduations(c(10, 20, 30), vers(c(10, 20, 30), ymin, ymax), "y"),
    evenement = arrondi(vers(2015.5, 2009.3, 2023.7)),
    moyennes = list(segment(0, 0), segment(0, 1), segment(1, 0), segment(1, 1))),
  n = nrow(parcelles), fermes = length(fermes),
  # the mean of each year's gap, plots − controls, before and after (the BACI view's); the mixed
  # model's effect with its 95% interval and its p
  avant = arrondi(ecarts_p$periodes$moyenne[1], 2), apres = arrondi(ecarts_p$periodes$moyenne[2], 2),
  effet = effet_baci$estimation, bas = effet_baci$bas, haut = effet_baci$haut, p = effet_baci$p)

# ------------------------------------------------------------ 4. the squares' other birds, in an NMDS
# The birds of 27 other species counted on the 40 squares in a spring (made up here, with a fixed
# seed: each species has its place along a gradient from the open plain to the grassy valleys,
# where the squares sit by their habitat). Their Bray–Curtis distances, the NMDS and the PERMANOVA
# of the habitat with its dispersions, all by the engine (communautes.R).
set.seed(3)
especes <- 27
gradient <- ifelse(habitat_site[en_large$site] == "cropland", 0.32, 0.68) + stats::rnorm(nrow(en_large), 0, 0.12)
optimum <- stats::runif(especes, -0.1, 1.1)
tolerance <- stats::runif(especes, 0.16, 0.34)
abondance <- stats::runif(especes, 2, 14)
Y <- sapply(seq_len(especes), function(j) stats::rpois(length(gradient), abondance[j] * exp(-(gradient - optimum[j])^2 / (2 * tolerance[j]^2))))
colnames(Y) <- sprintf("sp%02d", seq_len(especes))
po <- projet_depuis_tableau(data.frame(site = en_large$site, habitat = as.vector(habitat_site[en_large$site]), Y), "birds of the squares")
uo <- function(nom) numeros(po, nom)
m_nmds <- ajuster(po, definir_modele(po, "NMDS", x = NULL, y = uo(colnames(Y)), echelle = "none", options = list(distance = "bray", k = 2, departs = 20, graine = 1)))
m_perm <- ajuster(po, definir_modele(po, "PERMANOVA", x = uo("habitat"), y = uo(colnames(Y)), echelle = "none",
                                     options = list(distance = "bray", permutations = 999, graine = 1)))
permanova <- m_perm$noyau$table[m_perm$noyau$table$terme == "habitat", ]
pts <- m_nmds$noyau$points[, 1:2]
stopifnot(nrow(pts) == nrow(en_large), identical(m_nmds$noms, en_large$site), nrow(permanova) == 1, permanova$p < 0.01)
ex <- range(pts[, 1]) + c(-1, 1) * 0.06 * diff(range(pts[, 1]))
ey <- range(pts[, 2]) + c(-1, 1) * 0.1 * diff(range(pts[, 2]))
enveloppe <- function(couleur) {
  q <- pts[couleur_site == couleur, , drop = FALSE]
  i <- grDevices::chull(q)
  ligne_json(vers(q[c(i, i[1]), 1], ex[1], ex[2]), vers(q[c(i, i[1]), 2], ey[1], ey[2]))
}
communautes <- list(
  points = points_json(vers(pts[, 1], ex[1], ex[2]), vers(pts[, 2], ey[1], ey[2]), couleur_site),
  deco = list(enveloppes = lapply(c(3, 2), function(couleur) list(g = couleur, pts = enveloppe(couleur)))),
  n = nrow(Y), especes = ncol(Y), stress = arrondi(m_nmds$noyau$stress, 3), r2 = permanova$R2, p = permanova$p,
  dispersion_p = m_perm$dispersions[[1]]$p)

# ------------------------------------------------------------ 5. the skylark counts of tutorial 3
# The swarm of the page's first screen lands as these: 586 counts on 40 sites from 2008 to 2024
# (simulated: suivi.R), each a dot, under the tutorial's TRIM model (an effect of each site and of
# each year, overdispersion and serial correlation). The year across, the sites of a year side by
# side, each always at its own place; upwards, the count over its site's level in 2008 (the model's
# exp(alpha)), which the model expects to follow its indices: their line, their 95% band, and the
# overall slope of the imputed totals.
vus <- comptes[!is.na(comptes$skylarks), ]
pc <- importer_tableau("docs/site/donnees/skylark_counts.csv")
uc <- function(nom) numeros(pc, nom)
# (not named definir_trim: the engine takes a function definir_<type> for the definition of its analysis,
# analyses.R, fonction_analyse)
definition_trim <- function(projet, comptage, site, annee) {
  definir_modele(projet, "TRIM", x = NULL, y = numeros(projet, comptage), echelle = "none",
                 options = list(site = list(by = "variable", variable = numeros(projet, site)), temps = list(by = "variable", variable = numeros(projet, annee)),
                                modele = 3, surdispersion = TRUE, correlation = TRUE))
}
d_trim <- definition_trim(pc, "skylarks", "site", "year")
m_trim <- ajuster(pc, d_trim)
pente_trim <- m_trim$pentes[m_trim$pentes$quoi == "imputee", ]
niveaux_sites <- sites_trim(m_trim)
comptes_trim <- observations_trim(pc, m_trim)
comptes_trim <- comptes_trim[!is.na(comptes_trim$compte), ]
rang_site <- match(comptes_trim$site, niveaux_sites$site)
relatif <- comptes_trim$compte / exp(niveaux_sites$alpha[rang_site])
stopifnot(nrow(comptes_trim) == nrow(vus), max(relatif) < 3)
axe_annees <- range(comptes_trim$temps) + c(-0.6, 0.6)
hauteur_indice <- function(r) vers(r, 0, 3, 0.02, 1)  # (a count of 0 just above the axis)
en_x <- function(annee) vers(annee, axe_annees[1], axe_annees[2])
indices <- m_trim$indices
comptages <- list(
  points = points_json(en_x(comptes_trim$temps + (rang_site - (nrow(niveaux_sites) + 1) / 2) / nrow(niveaux_sites) * 0.74),
                       hauteur_indice(relatif), ifelse(comptes$habitat[match(comptes_trim$site, comptes$site)] == "cropland", 3, 2)),
  groupes = c("cropland", "grassland"),
  deco = list(
    axes = list(c(0, 0, 1, 0)),
    gx = graduations(c(2008, 2012, 2016, 2020, 2024), en_x(c(2008, 2012, 2016, 2020, 2024)), "x"),
    gy = graduations(0:3, hauteur_indice(0:3), "y"),
    tirets = list(c(0, arrondi(hauteur_indice(1)), 1, arrondi(hauteur_indice(1)))),
    rubans = list(ligne_json(en_x(c(indices$temps, rev(indices$temps))),
                             hauteur_indice(c(indices$modele - 1.96 * indices$se_modele, rev(indices$modele + 1.96 * indices$se_modele))))),
    courbes = list(ligne_json(en_x(indices$temps), hauteur_indice(indices$modele)))),
  n = nrow(comptes_trim), sites = m_trim$I, annees = m_trim$J,
  pente = pente_trim$multiplicative, pente_bas = pente_trim$bas, pente_haut = pente_trim$haut,
  indice_fin = arrondi(utils::tail(indices$impute, 1), 2))
stopifnot(pente_trim$classe == "declin_modere")

# ------------------------------------------------------------ 6. the line transects of tutorial 3
# 157 skylark detections within 150 m of 24 transects (simulated: suivi.R), in 10 m classes, each
# detection a dot in its class's bar, under the half-normal detection function fitted by maximum
# likelihood.
alouettes <- utils::read.csv("docs/site/donnees/skylark_transects.csv")
w <- 150
x <- alouettes$distance[!is.na(alouettes$distance) & alouettes$distance <= w]
mu <- function(s) s * sqrt(pi / 2) * (2 * stats::pnorm(w / s) - 1)  # the integral of exp(-x²/2s²) from 0 to w
sigma <- stats::optimize(function(s) sum(x^2) / (2 * s^2) + length(x) * log(mu(s)), c(10, 1000))$minimum
largeur <- 10
classe <- pmin(floor(x / largeur), w / largeur - 1)
colonnes <- 3
rang <- stats::ave(seq_along(x), classe, FUN = seq_along) - 1
effectif <- as.vector(table(factor(classe, 0:(w / largeur - 1))))
haut <- ceiling(max(effectif, length(x) * largeur / mu(sigma)) / colonnes) + 1  # rows of dots
courbe <- seq(0, w, length.out = 60)
distance <- list(
  points = points_json(vers(classe * largeur + (rang %% colonnes + 0.5) / colonnes * largeur * 0.84 + largeur * 0.08, 0, w),
                       vers(rang %/% colonnes + 0.5, 0, haut), 0),
  lignes = list(list(g = 1, pts = ligne_json(vers(courbe, 0, w),
                                             vers(length(x) * largeur * exp(-courbe^2 / (2 * sigma^2)) / mu(sigma) / colonnes, 0, haut)))),
  part_lignes = 0.2,
  deco = list(
    axes = list(c(0, 0, 1, 0)),
    gx = graduations(c(0, 50, 100, 150), vers(c(0, 50, 100, 150), 0, w), "x"),
    # the histogram's bars: each class from side to side, as high as its detections in rows
    barres = lapply(which(effectif > 0), function(k) arrondi(c(vers((k - 1 + c(0.03, 0.97)) * largeur, 0, w), effectif[k] / colonnes / haut)))),
  n = length(x), transects = length(unique(alouettes$Sample.Label)), w = w,
  sigma = arrondi(sigma, 1), p = arrondi(mu(sigma) / w, 2), demi_bande = round(mu(sigma)))

# ------------------------------------------------------------ 7. the skylarks' sites on a map
# The 40 sites of the counts on IGN's map between Blois and Orléans (src/assets/carte.webp, made by
# carte.py, which says in src/carte.json where its tiles are). The tutorial gives the sites no
# place: these are made up, like the counts, the cropland squares on the Beauce plain, the
# grassland ones in the valleys of the Loire and of its tributaries (the sites alternate: C01
# cropland, C02 grassland...). Each site is a flock: the swarm's birds shared between the sites by
# their mean counts, set in a round of seeds as a sunflower's. A flock has the colour of the
# quarter its trend falls in: Sen's slope of its counts (the engine's: series.R), relative to its
# mean count.
lieux <- data.frame(
  site = sprintf("C%02d", 1:40),
  lon = as.vector(rbind(
    c(1.4296, 1.4859, 1.3781, 1.5429, 1.5669, 1.6184, 1.6644, 1.4571, 1.5408, 1.6047, 1.5429, 1.5772, 1.6184, 1.5477, 1.5683, 1.4948, 1.4365, 1.4056, 1.3712, 1.4605),
    c(1.5450, 1.5958, 1.6370, 1.6699, 1.6988, 1.7290, 1.7578, 1.7901, 1.8196, 1.5463, 1.3527, 1.3939, 1.6699, 1.7043, 1.6356, 1.4708, 1.5820, 1.6452, 1.7352, 1.7221))),
  lat = as.vector(rbind(
    c(47.9439, 47.9278, 47.9209, 47.9614, 47.9384, 47.9462, 47.9209, 47.8979, 47.8910, 47.8726, 47.8450, 47.8519, 47.8436, 47.8187, 47.8002, 47.7850, 47.7573, 47.7296, 47.7088, 47.7375),
    c(47.6571, 47.7005, 47.7356, 47.7596, 47.7836, 47.8049, 47.8288, 47.8380, 47.8519, 47.7144, 47.7375, 47.9577, 47.8666, 47.9048, 47.9025, 47.6820, 47.6802, 47.7079, 47.7804, 47.8519))))
stopifnot(identical(as.vector(habitat_site[lieux$site]), rep(c("cropland", "grassland"), 20)))
serie <- series_variable(pc, uc("skylarks"), uc("year"), lignes = uc("site"))
tendances <- tendances_series(serie$points)
moyenne_lieu <- tapply(vus$skylarks, vus$site, mean)[lieux$site]
relative <- tendances$pente[match(lieux$site, tendances$ligne)] / moyenne_lieu
quart <- as.integer(cut(relative, stats::quantile(relative, 0:4 / 4), include.lowest = TRUE))
# Web Mercator, as the tiles: a place's column and row of tiles at this zoom, then its place in the
# picture, in pixels from its top left corner
fond <- jsonlite::fromJSON("vitrine/src/carte.json")
x_site <- ((lieux$lon + 180) / 360 * 2^fond$zoom - fond$colonne) * 256
y_site <- ((1 - log(tan(lieux$lat * pi / 180) + 1 / cos(lieux$lat * pi / 180)) / pi) / 2 * 2^fond$zoom - fond$ligne) * 256
# the birds of each site: the swarm's (a bird for each count of the first screen) by the largest
# remainders of the sites' shares
part <- moyenne_lieu / sum(moyenne_lieu) * nrow(comptes_trim)
oiseaux_site <- floor(part)
restent <- nrow(comptes_trim) - sum(oiseaux_site)
oiseaux_site[order(part - oiseaux_site, decreasing = TRUE)[seq_len(restent)]] <- oiseaux_site[order(part - oiseaux_site, decreasing = TRUE)[seq_len(restent)]] + 1
# a flock: its k-th bird at pas × √(k − ½) pixels from the site, a golden angle further round
pas <- 4.6
rayon_site <- pas * sqrt(oiseaux_site)
vol <- do.call(rbind, lapply(seq_len(nrow(lieux)), function(i) {
  k <- seq_len(oiseaux_site[i])
  data.frame(x = x_site[i] + pas * sqrt(k - 0.5) * cos(k * pi * (3 - sqrt(5))), y = y_site[i] + pas * sqrt(k - 0.5) * sin(k * pi * (3 - sqrt(5))),
             g = c(1, 3, 5, 0)[quart[i]])  # (from the fastest fall to the best hold: red, orange, teal, blue)
}))
entre_sites <- as.matrix(stats::dist(cbind(x_site, y_site)))
libre <- entre_sites - outer(rayon_site, rayon_site, "+")
diag(libre) <- Inf
largeur_carte <- 256 * fond$colonnes
hauteur_carte <- 256 * fond$lignes
stopifnot(sum(oiseaux_site) == nrow(comptes_trim), oiseaux_site >= 3, libre > 5,
          x_site - rayon_site > 4, x_site + rayon_site < largeur_carte - 4, y_site - rayon_site > 4, y_site + rayon_site < hauteur_carte - 4)
carte <- list(
  points = points_json(vol$x / largeur_carte, 1 - vol$y / hauteur_carte, vol$g),
  petit = TRUE,
  deco = list(),
  n = nrow(lieux), baisse = sum(relative < 0), stable = sum(relative == 0), oiseaux = range(oiseaux_site))

# ------------------------------------------------------------ the R console, the texts, the script
# What the page quotes of the skylarks' TRIM model (M1 of the counts' project): a command typed in
# the R console on it and its output, its Methods text in both languages, and of its Export to R
# script, the header and the function for the overall slope.
# (the version written in the Methods text's citation and the script's header: the application's own)
version_nuee_site <- jsonlite::fromJSON("app/package.json")$version
m <- m_trim
commande <- "head(round(m$indices, 3))"
sortie_console <- utils::capture.output(print(eval(parse(text = commande))))
textes <- function(langue) {
  options(nuee.langue = langue)
  on.exit(options(nuee.langue = "en"))
  # (the French tutorial's file holds the same counts, its variables named in French)
  if (langue == "fr") {
    pl <- importer_tableau("docs/site/donnees/comptages_alouette.csv")
    dl <- definition_trim(pl, "alouettes", "site", "annee")
  } else {
    pl <- pc
    dl <- d_trim
  }
  ml <- ajuster(pl, dl)
  tm <- texte_methodes(pl, dl, ml, langue, version_nuee_site)
  f <- file.path(tempdir(), if (langue == "fr") "alouettes M1.R" else "skylarks M1.R")
  r <- ecrire_script(pl, dl, f, getOption("nuee.moteur"), m = ml, donnees = "csv", version = version_nuee_site, langue = langue)
  stopifnot(is.null(r$ecart), isTRUE(all.equal(ml$pentes$multiplicative, m_trim$pentes$multiplicative)))  # the script gives the engine's results
  lignes <- readLines(f, encoding = "UTF-8")
  # the script's header (up to the list of its parts), and its function for the overall slope with
  # the comment above it
  debut <- grep("^(overall_slope|pente_globale) <- function", lignes)[1]
  fin <- debut + which(lignes[debut:length(lignes)] == "}")[1] - 1
  while (startsWith(lignes[debut - 1], "#")) debut <- debut - 1
  list(methodes = tm$paragraphes, references = tm$references,
       script = list(entete = lignes[1:grep("^# +4[.]", lignes)[1]], coeur = lignes[debut:fin], n = length(lignes), de = debut, a = fin))
}
texte <- list(en = textes("en"), fr = textes("fr"))

# ------------------------------------------------------------ a few counts for the page
fichiers_moteur <- list.files("moteur/R", "\\.R$", full.names = TRUE)
lignes_de <- function(f) sum(vapply(f, function(x) length(readLines(x, warn = FALSE)), 1L))
verifications <- list.files("moteur/tests", "^verifier_.*\\.R$", full.names = TRUE)
chiffres <- list(
  lignes_moteur = lignes_de(fichiers_moteur),
  fichiers_moteur = length(fichiers_moteur),
  verifications = length(verifications), lignes_verifications = lignes_de(verifications),
  R = paste(R.version$major, R.version$minor, sep = "."))

json <- jsonlite::toJSON(
  list(formes = list(comptages = comptages, acp = acp, spectres = spectres, baci = baci, communautes = communautes, distance = distance,
                     carte = carte),
       console = list(commande = commande, sortie = sortie_console),
       # what the page's document cites: the model BACI's coefficients, as the interface has them
       document = list(terme = m_baci$resultat$coefficients$terme, estimation = m_baci$resultat$coefficients$estimation,
                       bas = m_baci$resultat$coefficients$bas, haut = m_baci$resultat$coefficients$haut, p = m_baci$resultat$coefficients$p),
       texte = texte, chiffres = chiffres),
  auto_unbox = TRUE, digits = NA, pretty = FALSE)
sortie <- file("vitrine/src/donnees.json", "wb")  # (as bytes: the same file on Windows, in UTF-8, its line ending too)
writeLines(enc2utf8(as.character(json)), sortie, useBytes = TRUE)
close(sortie)
cat("vitrine/src/donnees.json written:", file.size("vitrine/src/donnees.json"), "bytes\n")
cat(sprintf("PCA: %d sites, %d springs; R2X %.3f, %.3f; ellipse %.2f x %.2f in %.1f x %.1f; outside: %s at t1 %.2f, t2 %.2f\n", acp$n, acp$k,
            comp$R2X[1], comp$R2X[2], rayons[1], rayons[2], bx, by, acp$dehors, t12[dehors, 1], t12[dehors, 2]))
cat(sprintf("songs: %d spectra of %d frequencies, %.0f Hz higher by the loud roads; shown: squares %s (road %s)\n", spectres$n, spectres$k, spectres$montee,
            paste(en_large$site[montres], collapse = " "), paste(round(route[montres], 2), collapse = " ")))
cat(sprintf("skylark plots: gaps before %.2f, after %.2f; effect %.2f [%.2f, %.2f], p %.2g; territories %.1f to %.1f\n", baci$avant, baci$apres,
            baci$effet, baci$bas, baci$haut, baci$p, min(parcelles$territories), max(parcelles$territories)))
cat(sprintf("birds: NMDS stress %.3f; PERMANOVA habitat R2 %.3f, p %.3f; dispersion p %.3f\n", communautes$stress, communautes$r2, communautes$p, communautes$dispersion_p))
cat(sprintf("transects: half-normal sigma %.1f m, p %.2f, half-width %.0f m, n %d\n", sigma, mu(sigma) / w, mu(sigma), length(x)))
cat(sprintf("map: flocks of %d to %d birds, the closest two %.0f pixels apart\n", min(oiseaux_site), max(oiseaux_site), min(libre)))
cat(sprintf("skylarks: %d counts, %d sites, %d years; TRIM slope %.4f [%.4f, %.4f], index %.2f in the last year; %d sites falling, %d level\n",
            comptages$n, comptages$sites, comptages$annees, comptages$pente, comptages$pente_bas, comptages$pente_haut, comptages$indice_fin,
            carte$baisse, carte$stable))
cat(sprintf("scripts: en %d lines, fr %d lines; engine %d lines in %d files; %d checks (%d lines)\n",
            texte$en$script$n, texte$fr$script$n, chiffres$lignes_moteur, chiffres$fichiers_moteur, chiffres$verifications, chiffres$lignes_verifications))
