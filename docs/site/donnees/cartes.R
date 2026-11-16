# Simulated data for the maps tutorial of Nuée's documentation website.
#   Rscript cartes.R          (writes four files next to it)
#
# The data are made up: no meadow was cut and the terrain is invented. The story: the hay of 90
# mountain meadows is weighed at the first cut, on a plateau of 12 km by 9 km. Does the biomass
# fall with elevation, and what does the map say of what elevation doesn't explain?
#
# 1. An elevation model (DEM) of the plateau, as a GeoTIFF in Lambert-93 (EPSG:2154), cells of
#    50 m, whole metres (16-bit integers), compressed (Deflate, predictor 2), as GDAL writes it:
#      altitude.tif             (French tutorial)
#      elevation.tif            (English tutorial: the same file under another name)
#    elevation = 1220 m
#              + 16 hills and valleys, Gaussian, of 400 to 850 m radius and 50 to 130 m high
#                (a third of them valleys), placed at random
#              + a gentle roughness (sines, 5 m)
# 2. The meadows: one row each, their place in Lambert-93 (metres), the hay's dry biomass at the
#    first cut (g/m2). No elevation: it comes from the DEM, in Nuée (Edit › Add raster values).
#      prairies.csv              prairie, X_L93, Y_L93, biomasse
#      meadows.csv               the same values: meadow, X_L93, Y_L93, biomass
#    biomass = 560 - 0.8 per metre above 1250 m (the DEM's value at the meadow, bilinear)
#            + what the place adds: a smooth field over the plateau (soils, the farms' manuring),
#              Gaussian, exponential covariance of range 3 km, sd 40
#            + the meadow's own deviation, normal, sd 25
#    rounded to 0.1 g. The meadows are drawn at random on the plateau, at least 400 m apart.
# Same seed, same values (R 3.6 or later).

ici <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))),
                error = function(e) ".")
if (length(ici) != 1) ici <- "."
set.seed(208)

# ---------------------------------------------------------------- 1. the elevation model
x0 <- 696000; y1 <- 6399000                 # the north-west corner of the first cell
cellule <- 50
nc <- 240; nl <- 180                        # 12 km by 9 km
xc <- x0 + cellule * (seq_len(nc) - 0.5)    # the cells' centres
yc <- y1 - cellule * (seq_len(nl) - 0.5)    # (row 1 at the north)
nb <- 16                                    # hills and valleys: centre, radius, height
cx <- runif(nb, x0, x0 + cellule * nc); cy <- runif(nb, y1 - cellule * nl, y1)
r <- runif(nb, 400, 850); h <- runif(nb, 50, 130) * ifelse(runif(nb) < 0.35, -1, 1)
relief <- function(x, y) {
  s <- 1220 + 5 * sin(x / 450) * cos(y / 380)
  for (k in seq_len(nb)) s <- s + h[k] * exp(-((x - cx[k])^2 + (y - cy[k])^2) / (2 * r[k]^2))
  s
}
z <- outer(yc, xc, function(y, x) relief(x, y))   # [row, column]
z <- round(z)

# A GeoTIFF of 16-bit integers: one band, strips of 16 rows, Deflate with the horizontal
# predictor, the georeferencing by ModelPixelScale and ModelTiepoint, the system by its EPSG code
# (GeoKeys), GDAL's no-data value.
ecrire_geotiff <- function(chemin, z, x0, y1, cellule, epsg, nodata = -32768) {
  H <- nrow(z); W <- ncol(z)
  court <- function(x) writeBin(as.integer(x), raw(), size = 2, endian = "little")
  long <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")
  reel <- function(x) writeBin(as.numeric(x), raw(), size = 8, endian = "little")
  rps <- 16
  bandes <- lapply(seq(1, H, by = rps), function(a) {
    lignes <- a:min(H, a + rps - 1)
    u <- t(z[lignes, , drop = FALSE]) %% 65536             # (a column per row: the row's values in order)
    d <- u
    d[-1, ] <- (u[-1, , drop = FALSE] - u[-W, , drop = FALSE]) %% 65536
    v <- as.vector(d)
    memCompress(writeBin(as.integer(ifelse(v >= 32768, v - 65536, v)), raw(), size = 2, endian = "little"), "gzip")
  })
  nb <- length(bandes)
  e <- list()
  ajouter <- function(n, type, brut, nombre) e[[length(e) + 1]] <<- list(n = n, type = type, brut = brut, nombre = nombre)
  ajouter(256, 4, long(W), 1); ajouter(257, 4, long(H), 1)
  ajouter(258, 3, court(16), 1); ajouter(259, 3, court(8), 1); ajouter(262, 3, court(1), 1)
  ajouter(277, 3, court(1), 1); ajouter(278, 4, long(rps), 1); ajouter(284, 3, court(1), 1)
  ajouter(317, 3, court(2), 1); ajouter(339, 3, court(2), 1)
  ajouter(33550, 12, reel(c(cellule, cellule, 0)), 3)
  ajouter(33922, 12, reel(c(0, 0, 0, x0, y1, 0)), 6)
  # GTModelType: projected; GTRasterType: pixels as areas; ProjectedCSType: the EPSG code
  ajouter(34735, 3, court(c(1, 1, 0, 3, 1024, 0, 1, 1, 1025, 0, 1, 1, 3072, 0, 1, epsg)), 16)
  nd <- c(charToRaw(as.character(nodata)), as.raw(0))
  ajouter(42113, 2, nd, length(nd))
  corps <- raw(0)
  position <- 8
  offsets <- numeric(nb)
  for (k in seq_len(nb)) { offsets[k] <- position + length(corps); corps <- c(corps, bandes[[k]]); if (length(corps) %% 2) corps <- c(corps, as.raw(0)) }
  ajouter(273, 4, long(offsets), nb)
  ajouter(279, 4, long(vapply(bandes, length, 0)), nb)
  e <- e[order(vapply(e, `[[`, 0, "n"))]
  for (k in seq_along(e)) if (length(e[[k]]$brut) > 4) {
    e[[k]]$ou <- position + length(corps)
    corps <- c(corps, e[[k]]$brut)
    if (length(corps) %% 2) corps <- c(corps, as.raw(0))
  }
  ifd <- position + length(corps)
  entree <- function(x) c(court(x$n), court(x$type), long(x$nombre), if (length(x$brut) > 4) long(x$ou) else c(x$brut, raw(4 - length(x$brut))))
  repertoire <- c(court(length(e)), unlist(lapply(e, entree)), long(0))
  writeBin(c(charToRaw("II"), court(42), long(ifd), corps, repertoire), chemin)
}
for (nom in c("altitude.tif", "elevation.tif")) ecrire_geotiff(file.path(ici, nom), z, x0, y1, cellule, 2154)

# ---------------------------------------------------------------- 2. the meadows
n <- 90
X <- Y <- numeric(0)
while (length(X) < n) {
  x <- runif(1, x0 + 300, x0 + cellule * nc - 300)
  y <- runif(1, y1 - cellule * nl + 300, y1 - 300)
  if (!length(X) || min(sqrt((X - x)^2 + (Y - y)^2)) >= 400) { X <- c(X, x); Y <- c(Y, y) }
}
X <- round(X); Y <- round(Y)
# the DEM's value at each meadow: bilinear between the four nearest centres
bilineaire <- function(x, y) {
  cj <- (x - x0) / cellule + 0.5; ci <- (y1 - y) / cellule + 0.5
  j <- floor(cj); i <- floor(ci); fj <- cj - j; fi <- ci - i
  z[cbind(i, j)] * (1 - fi) * (1 - fj) + z[cbind(i, j + 1)] * (1 - fi) * fj + z[cbind(i + 1, j)] * fi * (1 - fj) + z[cbind(i + 1, j + 1)] * fi * fj
}
altitude <- bilineaire(X, Y)
D <- as.matrix(dist(cbind(X, Y)))
champ <- as.vector(t(chol(40^2 * exp(-D / 3000))) %*% rnorm(n))
biomasse <- round(560 - 0.8 * (altitude - 1250) + champ + rnorm(n, 0, 25), 1)
prairies <- data.frame(prairie = sprintf("S%02d", seq_len(n)), X_L93 = X, Y_L93 = Y, biomasse = biomasse)
write.csv(prairies, file.path(ici, "prairies.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(setNames(prairies, c("meadow", "X_L93", "Y_L93", "biomass")), file.path(ici, "meadows.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat(n, "meadows; elevation", paste(range(round(altitude)), collapse = " to "), "m; biomass", paste(range(biomasse), collapse = " to "), "g/m2\n")
