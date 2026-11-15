# The figures of nuee.io: computed by Nuée's own engine from the example project and the tutorials'
# data, and written to vitrine/src/donnees.json, which construire.mjs puts in the pages. Nothing on
# the site is drawn by hand: run this again after a change to the engine or to the data.
#   Rscript vitrine/donnees.R        (from the repository's folder; needs jsonlite, as the engine)
#
# Each figure is a list of points (x, y, group) and of lines for the swarm, and of "deco" for what
# is drawn crisp over it (axes, ticks, an ellipse, hulls), all in the unit square, y upwards:
# site.js only places them.

source("moteur/load.R", encoding = "UTF-8")

arrondi <- function(x, n = 4) round(x, n)
# x from [a, b] to [bas, haut] of the unit square
vers <- function(x, a, b, bas = 0, haut = 1) bas + (x - a) / (b - a) * (haut - bas)
points_json <- function(x, y, g) unname(Map(function(a, b, c) c(arrondi(a), arrondi(b), c), x, y, g))
ligne_json <- function(x, y) unname(Map(function(a, b) c(arrondi(a), arrondi(b)), x, y))
graduations <- function(valeurs, positions, cote) unname(Map(function(v, p) list(v = v, p = arrondi(p), cote = cote), valeurs, positions))

# ------------------------------------------------------------ 1. the example project's PCA
# Help › Example project: 120 made-up apples, model M1. The scores, the varieties, and the
# Hotelling T² ellipse as the Scores view draws it (serveur.R: variance_t and ellipse).
p <- lire_projet("exemple/orchards.json")
d1 <- modele_du_projet(p, 1)
m1 <- ajuster(p, d1)
pommes <- utils::read.csv("exemple/orchards.csv", check.names = FALSE)
stopifnot(nrow(pommes) == m1$N, identical(pommes$fruit, unname(p$noms_obs[as.character(m1$observations)])))
varietes <- levels(factor(pommes$variety))
t12 <- m1$T[, 1:2]
N <- nrow(t12)
t2_95 <- 2 * (N - 1) / (N - 2) * stats::qf(0.95, 2, N - 2)
rayons <- sqrt(colSums(t12^2) / (N - 1) * t2_95)
bx <- ceiling(max(abs(t12[, 1]), rayons[1]) * 1.04 * 2) / 2
by <- ceiling(max(abs(t12[, 2]), rayons[2]) * 1.04 * 2) / 2
angle <- seq(0, 2 * pi, length.out = 97)
comp <- m1$statistiques$composantes
acp <- list(
  points = points_json(vers(t12[, 1], -bx, bx), vers(t12[, 2], -by, by), match(pommes$variety, varietes) - 1),
  groupes = varietes,
  deco = list(
    axes = list(c(0, 0.5, 1, 0.5), c(0.5, 0, 0.5, 1)),
    ellipse = ligne_json(vers(rayons[1] * cos(angle), -bx, bx), vers(rayons[2] * sin(angle), -by, by)),
    gx = graduations(c(-4, -2, 2, 4), vers(c(-4, -2, 2, 4), -bx, bx), "x"),
    gy = graduations(c(-4, -2, 2, 4), vers(c(-4, -2, 2, 4), -by, by), "y")),
  r2x = arrondi(comp$R2X[1:2], 3), n = N, k = m1$K, a = m1$A,
  r2xcum = arrondi(comp$R2Xcum[m1$A], 2), q2cum = arrondi(comp$Q2cum[m1$A], 2))

# ------------------------------------------------------------ 2. the spectra of tutorial 2
# Six of the 120 made-up near-infrared soil spectra, from the poorest to the richest in carbon.
sols <- utils::read.csv("docs/site/donnees/soils_spectra.csv", check.names = FALSE)
longueurs <- as.numeric(names(sols)[-(1:3)])
choisis <- order(sols$soc)[round(seq(4, nrow(sols) - 3, length.out = 6))]
S <- as.matrix(sols[choisis, -(1:3)])
etendue <- range(S)
spectres <- list(
  lignes = lapply(seq_along(choisis), function(i) list(
    g = if (sols$land_use[choisis[i]] == "grassland") 2 else 3,
    pts = ligne_json(vers(longueurs, 1100, 2500), vers(S[i, ], etendue[1], etendue[2], 0.04, 0.96)))),
  groupes = c("grassland", "cropland"),
  deco = list(
    axes = list(c(0, 0, 1, 0)),
    gx = graduations(c(1200, 1600, 2000, 2400), vers(c(1200, 1600, 2000, 2400), 1100, 2500), "x"),
    # the two water bands of the tutorial (1450 and 1930 nm), 50 nm on each side
    bandes = list(arrondi(vers(c(1400, 1500), 1100, 2500)), arrondi(vers(c(1880, 1980), 1100, 2500)))),
  n = nrow(sols), k = length(longueurs), soc = range(sols$soc[choisis]))

# ------------------------------------------------------------ 3. the BACI of tutorial 1
# Juvenile trout on three restored stations and three controls, 2010–2023 (simulated: baci.R).
truites <- utils::read.csv("docs/site/donnees/baci_trout.csv")
truites <- truites[!is.na(truites$density), ]
ymax <- 30
set.seed(3)
decalage <- stats::runif(nrow(truites), -0.16, 0.16)  # the stations of a year side by side
moyennes <- stats::aggregate(density ~ group + I(year >= 2016), truites, mean)
names(moyennes) <- c("group", "apres", "density")
par_an <- tapply(truites$density, list(truites$year, truites$group), mean)
ecarts <- par_an[, "impact"] - par_an[, "control"]
segment <- function(groupe, apres) {
  y <- vers(moyennes$density[moyennes$group == groupe & moyennes$apres == apres], 0, ymax)
  x <- vers(if (apres) c(2015.6, 2023.4) else c(2009.6, 2015.4), 2009.3, 2023.7)
  list(g = if (groupe == "impact") 1 else 0, pts = ligne_json(x, c(y, y)))
}
baci <- list(
  points = points_json(vers(truites$year + decalage, 2009.3, 2023.7), vers(truites$density, 0, ymax),
                       ifelse(truites$group == "impact", 1, 0)),
  groupes = c("control", "impact"),
  deco = list(
    axes = list(c(0, 0, 1, 0), c(0, 0, 0, 1)),
    gx = graduations(c(2010, 2013, 2016, 2019, 2022), vers(c(2010, 2013, 2016, 2019, 2022), 2009.3, 2023.7), "x"),
    gy = graduations(c(10, 20, 30), vers(c(10, 20, 30), 0, ymax), "y"),
    evenement = arrondi(vers(2015.5, 2009.3, 2023.7)),
    moyennes = list(segment("control", FALSE), segment("control", TRUE), segment("impact", FALSE), segment("impact", TRUE))),
  n = nrow(truites),
  # the mean of each year's impact − control gap, before and after (the BACI view's)
  avant = arrondi(mean(ecarts[names(ecarts) < 2016]), 2), apres = arrondi(mean(ecarts[names(ecarts) >= 2016]), 2))

# ------------------------------------------------------------ 4. an NMDS of made-up communities
# 72 surveys of 30 species in three habitats along a moisture gradient (simulated here, with a
# fixed seed), their Bray–Curtis distances and the NMDS, both by the engine (communautes.R).
set.seed(20261115)
habitats <- c("reedbed", "meadow", "woodland")
habitat <- rep(habitats, each = 24)
gradient <- c(reedbed = 0.18, meadow = 0.5, woodland = 0.82)[habitat] + stats::rnorm(72, 0, 0.09)
optimum <- stats::runif(30, -0.1, 1.1)
tolerance <- stats::runif(30, 0.12, 0.3)
abondance <- stats::runif(30, 2, 14)
Y <- sapply(1:30, function(j) stats::rpois(72, abondance[j] * exp(-(gradient - optimum[j])^2 / (2 * tolerance[j]^2))))
dimnames(Y) <- list(sprintf("R%02d", 1:72), sprintf("sp%02d", 1:30))
D <- distance_communaute(Y, "bray")
nmds <- nmds_noyau(as.matrix(D), 2, 20, 1)
pts <- nmds$points[, 1:2]
ex <- range(pts[, 1]) + c(-1, 1) * 0.06 * diff(range(pts[, 1]))
ey <- range(pts[, 2]) + c(-1, 1) * 0.1 * diff(range(pts[, 2]))
couleurs_habitats <- c(reedbed = 5, meadow = 3, woodland = 2)  # (site.js: COULEURS)
enveloppe <- function(h) {
  q <- pts[habitat == h, , drop = FALSE]
  i <- grDevices::chull(q)
  ligne_json(vers(q[c(i, i[1]), 1], ex[1], ex[2]), vers(q[c(i, i[1]), 2], ey[1], ey[2]))
}
communautes <- list(
  points = points_json(vers(pts[, 1], ex[1], ex[2]), vers(pts[, 2], ey[1], ey[2]), couleurs_habitats[habitat]),
  groupes = habitats,
  deco = list(enveloppes = lapply(habitats, function(h) list(g = couleurs_habitats[[h]], pts = enveloppe(h)))),
  n = nrow(Y), especes = ncol(Y), stress = arrondi(nmds$stress, 3))

# ------------------------------------------------------------ 5. the line transects of tutorial 3
# 157 skylark detections within 150 m of 24 transects (simulated: suivi.R), in 10 m classes, each
# detection a dot, under the half-normal detection function fitted by maximum likelihood.
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
    gx = graduations(c(0, 50, 100, 150), vers(c(0, 50, 100, 150), 0, w), "x")),
  n = length(x), transects = length(unique(alouettes$Sample.Label)), w = w,
  sigma = arrondi(sigma, 1), p = arrondi(mu(sigma) / w, 2), demi_bande = round(mu(sigma)))

# ------------------------------------------------------------ 6. a made-up map
# 46 sites on the contour lines of a relief (simulated here), coloured by the quarter of their
# value: a response that follows the relief, with noise.
set.seed(46)
relief <- function(x, y) {
  1.1 * exp(-((x - 0.28)^2 + (y - 0.66)^2) / 0.035) + 0.8 * exp(-((x - 0.7)^2 / 0.05 + (y - 0.3)^2 / 0.03)) +
    0.45 * exp(-((x - 0.82)^2 + (y - 0.8)^2) / 0.012) + 0.25 * x - 0.1 * y
}
gx <- seq(0, 1, length.out = 90)
gy <- seq(0, 1, length.out = 70)
z <- outer(gx, gy, relief)
niveaux <- grDevices::contourLines(gx, gy, z, levels = seq(0.2, 1.1, by = 0.15))
sites <- data.frame(x = stats::runif(46, 0.05, 0.95), y = stats::runif(46, 0.06, 0.94))
sites$valeur <- relief(sites$x, sites$y) + stats::rnorm(46, 0, 0.12)
sites$quart <- as.integer(cut(sites$valeur, stats::quantile(sites$valeur, 0:4 / 4), include.lowest = TRUE)) - 1
carte <- list(
  points = points_json(sites$x, sites$y, c(0, 5, 3, 1)[sites$quart + 1]),
  lignes = lapply(niveaux, function(l) list(g = 6, pts = ligne_json(l$x, l$y))),
  part_lignes = 0.72,
  deco = list(),
  n = nrow(sites), courbes = length(niveaux))

# ------------------------------------------------------------ the R console, the texts, the script
# What the page quotes: a command typed in the R console on the example's PCA and its output, the
# Methods and Results texts of the example's PLS (M2) in both languages, and of its Export to R
# script, the header and the NIPALS function.
m <- m1
commande <- 'head(round(m$statistiques$observations[, c("t1", "t2", "T2", "DModX")], 2))'
sortie_console <- utils::capture.output(print(eval(parse(text = commande))))
textes <- function(langue) {
  options(nuee.langue = langue)
  on.exit(options(nuee.langue = "en"))
  # (the French example is the same apples, their variables named in French)
  pl <- if (langue == "fr") lire_projet("exemple/vergers.json") else p
  d2 <- modele_du_projet(pl, 2)
  m2 <- ajuster(pl, d2)
  tm <- texte_methodes(pl, d2, m2, langue, "1.0.0")
  tr <- texte_resultats(pl, d2, m2, langue)
  f <- file.path(tempdir(), if (langue == "fr") "vergers M2.R" else "orchards M2.R")
  r <- ecrire_script(pl, d2, f, getOption("nuee.moteur"), m = m2, donnees = "csv", version = "1.0.0", langue = langue)
  stopifnot(is.null(r$ecart))  # the script gives the engine's results
  lignes <- readLines(f, encoding = "UTF-8")
  # the script's header (up to the list of its parts), and its function for one PLS component with
  # the comment above it
  debut <- grep("<- function(E, ME, F, MF)", lignes, fixed = TRUE)[1]
  fin <- debut + which(lignes[debut:length(lignes)] == "}")[1] - 1
  while (startsWith(lignes[debut - 1], "#")) debut <- debut - 1
  comp2 <- m2$statistiques$composantes
  list(methodes = tm$paragraphes, references = tm$references, resultats = tr$paragraphes,
       # what a document's {{M2.N}}, {{M2.Q2}} and {{M2.R2Y:%}} cite (references.js writes them)
       valeurs = list(N = m2$N, Q2 = utils::tail(comp2$Q2cum, 1), R2Y = utils::tail(comp2$R2Ycum, 1)),
       script = list(entete = lignes[1:grep("^# +4[.]", lignes)[1]], nipals = lignes[debut:fin], n = length(lignes), de = debut, a = fin))
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

jsonlite::write_json(
  list(formes = list(acp = acp, spectres = spectres, baci = baci, communautes = communautes, distance = distance, carte = carte),
       console = list(commande = commande, sortie = sortie_console),
       texte = texte, chiffres = chiffres),
  "vitrine/src/donnees.json", auto_unbox = TRUE, digits = NA, pretty = FALSE)
cat("vitrine/src/donnees.json written:", file.size("vitrine/src/donnees.json"), "bytes\n")
cat(sprintf("PCA: R2X %.3f, %.3f; ellipse %.2f x %.2f\n", comp$R2X[1], comp$R2X[2], rayons[1], rayons[2]))
cat(sprintf("NMDS stress %.3f; half-normal sigma %.1f m, p %.2f, half-width %.0f m, n %d\n", nmds$stress, sigma, mu(sigma) / w, mu(sigma), length(x)))
cat(sprintf("BACI gaps: before %.2f, after %.2f\n", baci$avant, baci$apres))
cat(sprintf("scripts: en %d lines, fr %d lines; engine %d lines in %d files; %d checks (%d lines)\n",
            texte$en$script$n, texte$fr$script$n, chiffres$lignes_moteur, chiffres$fichiers_moteur, chiffres$verifications, chiffres$lignes_verifications))
