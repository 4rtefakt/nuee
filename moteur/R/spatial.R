# Les données géographiques des observations : leurs coordonnées et leur système, la longitude et
# la latitude qu'il en donne (la carte et son fond), les distances entre observations et
# l'autocorrélation spatiale d'une variable ou des résidus d'un modèle (I de Moran,
# corrélogramme, semi-variogramme). Les fichiers GeoPackage sont dans geopackage.R.
#
# Les coordonnées d'un projet (p$coordonnees, enregistrées avec lui) : deux de ses variables
# quantitatives, x (l'est, ou la longitude) et y (le nord, ou la latitude), et leur système par
# son code EPSG (systeme_coordonnees : la liste de ceux que Nuée connaît), ou « plan » : des
# coordonnées planes en mètres d'un autre système, dont les distances sont justes mais qui ne se
# placent pas sur un fond de carte.
#
# Les conversions sont écrites ici d'après les formules publiées, sans paquet : la conique
# conforme de Lambert (IGN, notes NTG-71 ; Snyder 1987, p. 104-110), la Mercator transverse des
# zones UTM (séries de Krüger jusqu'à n⁴, Karney 2011), l'azimutale équivalente de Lambert de
# l'Europe (LAEA ; Snyder 1987, p. 182-190) et la Mercator sphérique des fonds de carte du web.
# Le Lambert II étendu (NTF) passe en WGS 84 par les trois translations de l'IGN (−168, −60,
# +320 m), justes au mètre près ; RGF93 et ETRS89 sont pris pour WGS 84 (moins d'un mètre
# d'écart, comme le fait PROJ sans grille). verifier_spatial.R compare tout à PROJ (paquet sf).

rad <- function(x) x * pi / 180
deg <- function(x) x * 180 / pi

ELLIPSOIDES <- list(
  WGS84 = list(a = 6378137, f = 1 / 298.257223563, nom = "WGS 84"),
  GRS80 = list(a = 6378137, f = 1 / 298.257222101, nom = "GRS 1980"),
  CLARKE_IGN = list(a = 6378249.2, f = (6378249.2 - 6356515) / 6378249.2, nom = "Clarke 1880 (IGN)"))

# Le système de coordonnées d'un code EPSG (nombre, « EPSG:2154 », ou « plan ») : son nom, son
# genre (« lonlat », « lcc », « tm », « laea », « merc » ou « plan ») et ses paramètres ; NULL
# pour un code inconnu.
systeme_coordonnees <- function(code) {
  code <- toupper(trimws(as.character(code %||% "")[1]))
  if (identical(code, "PLAN")) return(list(code = "plan", nom = traduire("Coordonnées planes (mètres), système non précisé", "Plane coordinates (metres), unspecified system"), type = "plan"))
  n <- suppressWarnings(as.integer(sub("^EPSG:", "", code)))
  if (is.na(n)) return(NULL)
  lonlat <- function(nom, datum) list(code = n, nom = nom, type = "lonlat", ellipsoide = datum)
  lcc2 <- function(nom, phi0, phi1, phi2, lambda0, x0, y0) list(code = n, nom = nom, type = "lcc", ellipsoide = "GRS80",
    phi0 = phi0, phi1 = phi1, phi2 = phi2, lambda0 = lambda0, x0 = x0, y0 = y0)
  utm <- function(zone, sud, datum, nom_datum) list(code = n, nom = sprintf("%s / UTM zone %d%s", nom_datum, zone, if (sud) "S" else "N"),
    type = "tm", ellipsoide = datum, lambda0 = -183 + 6 * zone, phi0 = 0, k0 = 0.9996, x0 = 500000, y0 = if (sud) 10000000 else 0, zone = zone, sud = sud)
  if (n == 4326) return(lonlat("WGS 84 (longitude, latitude)", "WGS84"))
  if (n == 4258) return(lonlat("ETRS89 (longitude, latitude)", "GRS80"))
  if (n == 4171) return(lonlat("RGF93 (longitude, latitude)", "GRS80"))
  if (n %in% c(2154, 9794)) return(lcc2(if (n == 2154) "RGF93 v1 / Lambert-93" else "RGF93 v2b / Lambert-93", 46.5, 49, 44, 3, 700000, 6600000))
  if (n %in% 3942:3950) {
    z <- n - 3900
    return(lcc2(sprintf("RGF93 v1 / CC%d", z), z, z - 0.75, z + 0.75, 3, 1700000, 1200000 + (z - 42) * 1000000))
  }
  if (n == 27572) return(list(code = n, nom = "NTF (Paris) / Lambert II étendu", type = "lcc", ellipsoide = "CLARKE_IGN",
    phi0 = 46.8, k0 = 0.99987742, lambda0 = 2.337229166667, x0 = 600000, y0 = 2200000, vers_wgs84 = c(-168, -60, 320)))
  if (n == 3035) return(list(code = n, nom = "ETRS89 / LAEA Europe", type = "laea", ellipsoide = "GRS80",
    phi0 = 52, lambda0 = 10, x0 = 4321000, y0 = 3210000))
  if (n == 3857) return(list(code = n, nom = "WGS 84 / Pseudo-Mercator", type = "merc", ellipsoide = "WGS84"))
  if (n %in% 32601:32660) return(utm(n - 32600, FALSE, "WGS84", "WGS 84"))
  if (n %in% 32701:32760) return(utm(n - 32700, TRUE, "WGS84", "WGS 84"))
  if (n %in% 25828:25838) return(utm(n - 25800, FALSE, "GRS80", "ETRS89"))
  NULL
}

# Les systèmes que l'interface propose dans sa liste (les zones UTM se choisissent par leur code).
SYSTEMES_PROPOSES <- c(4326, 2154, 3942:3950, 27572, 3035, 3857, 32630, 32631, 32632, 25830, 25831, 25832)

liste_systemes <- function() {
  lapply(SYSTEMES_PROPOSES, function(n) { s <- systeme_coordonnees(n); list(code = n, nom = s$nom) })
}

# ---------------------------------------------------------------- formules des projections

# Conique conforme de Lambert, sécante (deux parallèles : Lambert-93, CC) ou tangente avec un
# facteur d'échelle (Lambert II étendu) : n, F et ρ0 (Snyder, équations 15-1 à 15-10).
lcc_constantes <- function(s, e) {
  m <- function(phi) cos(phi) / sqrt(1 - e^2 * sin(phi)^2)
  t <- function(phi) tan(pi / 4 - phi / 2) / ((1 - e * sin(phi)) / (1 + e * sin(phi)))^(e / 2)
  if (!is.null(s$phi1)) {
    p1 <- rad(s$phi1)
    p2 <- rad(s$phi2)
    n <- (log(m(p1)) - log(m(p2))) / (log(t(p1)) - log(t(p2)))
    F <- m(p1) / (n * t(p1)^n)
    k0 <- 1
  } else {
    p0 <- rad(s$phi0)
    n <- sin(p0)
    F <- m(p0) / (n * t(p0)^n)
    k0 <- s$k0
  }
  list(n = n, aF = ELLIPSOIDES[[s$ellipsoide]]$a * F * k0, rho0 = ELLIPSOIDES[[s$ellipsoide]]$a * F * k0 * t(rad(s$phi0))^n, t = t)
}

lcc_avant <- function(lon, lat, s, e) {
  k <- lcc_constantes(s, e)
  rho <- k$aF * k$t(rad(lat))^k$n
  theta <- k$n * rad(lon - s$lambda0)
  list(x = s$x0 + rho * sin(theta), y = s$y0 + k$rho0 - rho * cos(theta))
}

lcc_arriere <- function(x, y, s, e) {
  k <- lcc_constantes(s, e)
  dx <- x - s$x0
  dy <- k$rho0 - (y - s$y0)
  rho <- sign(k$n) * sqrt(dx^2 + dy^2)
  theta <- atan2(sign(k$n) * dx, sign(k$n) * dy)
  t <- (rho / k$aF)^(1 / k$n)
  phi <- pi / 2 - 2 * atan(t)
  for (i in 1:30) {  # (la latitude isométrique inverse, comme l'algorithme ALG0002 de l'IGN)
    suivant <- pi / 2 - 2 * atan(t * ((1 - e * sin(phi)) / (1 + e * sin(phi)))^(e / 2))
    fini <- all(abs(suivant - phi) < 1e-14, na.rm = TRUE)
    phi <- suivant
    if (fini) break
  }
  list(lon = s$lambda0 + deg(theta / k$n), lat = deg(phi))
}

# Mercator transverse (UTM) : séries de Krüger en n, le troisième aplatissement (Karney 2011,
# équations 35 et 36, jusqu'à n⁴ : moins d'un dixième de millimètre dans une zone).
tm_constantes <- function(f) {
  n <- f / (2 - f)
  list(n = n, A = (1 + n^2 / 4 + n^4 / 64) / (1 + n),
       alpha = c(n / 2 - 2 * n^2 / 3 + 5 * n^3 / 16 + 41 * n^4 / 180, 13 * n^2 / 48 - 3 * n^3 / 5 + 557 * n^4 / 1440,
                 61 * n^3 / 240 - 103 * n^4 / 140, 49561 * n^4 / 161280),
       beta = c(n / 2 - 2 * n^2 / 3 + 37 * n^3 / 96 - n^4 / 360, n^2 / 48 + n^3 / 15 - 437 * n^4 / 1440,
                17 * n^3 / 480 - 37 * n^4 / 840, 4397 * n^4 / 161280),
       delta = c(2 * n - 2 * n^2 / 3 - 2 * n^3 + 116 * n^4 / 45, 7 * n^2 / 3 - 8 * n^3 / 5 - 227 * n^4 / 45,
                 56 * n^3 / 15 - 136 * n^4 / 35, 4279 * n^4 / 630))
}

tm_avant <- function(lon, lat, s, e) {
  el <- ELLIPSOIDES[[s$ellipsoide]]
  k <- tm_constantes(el$f)
  phi <- rad(lat)
  l <- rad(lon - s$lambda0)
  t <- sinh(atanh(sin(phi)) - e * atanh(e * sin(phi)))
  xi <- atan(t / cos(l))
  eta <- atanh(sin(l) / sqrt(1 + t^2))
  X <- xi
  E <- eta
  for (j in 1:4) {
    X <- X + k$alpha[j] * sin(2 * j * xi) * cosh(2 * j * eta)
    E <- E + k$alpha[j] * cos(2 * j * xi) * sinh(2 * j * eta)
  }
  list(x = s$x0 + s$k0 * el$a * k$A * E, y = s$y0 + s$k0 * el$a * k$A * X)
}

tm_arriere <- function(x, y, s, e) {
  el <- ELLIPSOIDES[[s$ellipsoide]]
  k <- tm_constantes(el$f)
  xi <- (y - s$y0) / (s$k0 * el$a * k$A)
  eta <- (x - s$x0) / (s$k0 * el$a * k$A)
  xp <- xi
  ep <- eta
  for (j in 1:4) {
    xp <- xp - k$beta[j] * sin(2 * j * xi) * cosh(2 * j * eta)
    ep <- ep - k$beta[j] * cos(2 * j * xi) * sinh(2 * j * eta)
  }
  chi <- asin(sin(xp) / cosh(ep))
  phi <- chi
  for (j in 1:4) phi <- phi + k$delta[j] * sin(2 * j * chi)
  list(lon = s$lambda0 + deg(atan2(sinh(ep), cos(xp))), lat = deg(phi))
}

# Azimutale équivalente de Lambert, oblique, sur l'ellipsoïde (Snyder, équations 3-12, 24-11 à
# 24-28) ; la latitude inverse par l'itération de l'équation 3-16.
laea_constantes <- function(s, e) {
  a <- ELLIPSOIDES[[s$ellipsoide]]$a
  q <- function(phi) (1 - e^2) * (sin(phi) / (1 - e^2 * sin(phi)^2) - log((1 - e * sin(phi)) / (1 + e * sin(phi))) / (2 * e))
  qp <- q(pi / 2)
  p1 <- rad(s$phi0)
  b1 <- asin(q(p1) / qp)
  Rq <- a * sqrt(qp / 2)
  D <- a * (cos(p1) / sqrt(1 - e^2 * sin(p1)^2)) / (Rq * cos(b1))
  list(q = q, qp = qp, b1 = b1, Rq = Rq, D = D)
}

laea_avant <- function(lon, lat, s, e) {
  k <- laea_constantes(s, e)
  b <- asin(k$q(rad(lat)) / k$qp)
  l <- rad(lon - s$lambda0)
  B <- k$Rq * sqrt(2 / (1 + sin(k$b1) * sin(b) + cos(k$b1) * cos(b) * cos(l)))
  list(x = s$x0 + B * k$D * cos(b) * sin(l), y = s$y0 + (B / k$D) * (cos(k$b1) * sin(b) - sin(k$b1) * cos(b) * cos(l)))
}

laea_arriere <- function(x, y, s, e) {
  k <- laea_constantes(s, e)
  X <- x - s$x0
  Y <- y - s$y0
  rho <- sqrt((X / k$D)^2 + (k$D * Y)^2)
  ce <- 2 * asin(pmin(1, rho / (2 * k$Rq)))
  q <- ifelse(rho > 0, k$qp * (cos(ce) * sin(k$b1) + k$D * Y * sin(ce) * cos(k$b1) / rho), k$qp * sin(k$b1))
  l <- atan2(X * sin(ce), k$D * rho * cos(k$b1) * cos(ce) - k$D^2 * Y * sin(k$b1) * sin(ce))
  phi <- asin(q / 2)
  for (i in 1:30) {
    sp <- sin(phi)
    suivant <- phi + (1 - e^2 * sp^2)^2 / (2 * cos(phi)) *
      (q / (1 - e^2) - sp / (1 - e^2 * sp^2) + log((1 - e * sp) / (1 + e * sp)) / (2 * e))
    fini <- all(abs(suivant - phi) < 1e-14, na.rm = TRUE)
    phi <- suivant
    if (fini) break
  }
  list(lon = s$lambda0 + deg(l), lat = deg(phi))
}

# Le passage d'un datum à un autre par trois translations : la latitude et la longitude en
# coordonnées cartésiennes géocentriques sur le premier ellipsoïde, déplacées, puis relues sur le
# second (hauteur au-dessus de l'ellipsoïde ignorée : 0 au départ, laissée à l'arrivée).
changer_datum <- function(lon, lat, de, vers, translation) {
  e2a <- de$f * (2 - de$f)
  phi <- rad(lat)
  l <- rad(lon)
  N <- de$a / sqrt(1 - e2a * sin(phi)^2)
  X <- N * cos(phi) * cos(l) + translation[1]
  Y <- N * cos(phi) * sin(l) + translation[2]
  Z <- N * (1 - e2a) * sin(phi) + translation[3]
  e2b <- vers$f * (2 - vers$f)
  p <- sqrt(X^2 + Y^2)
  phi <- atan2(Z, p * (1 - e2b))
  for (i in 1:10) {
    N <- vers$a / sqrt(1 - e2b * sin(phi)^2)
    h <- p / cos(phi) - N
    phi <- atan2(Z, p * (1 - e2b * N / (N + h)))
  }
  list(lon = deg(atan2(Y, X)), lat = deg(phi))
}

# ---------------------------------------------------------------- conversions

# Des coordonnées de `systeme` en longitude et latitude WGS 84 (en degrés) ; NA pour un système
# plan (sans paramètres) ou une valeur manquante.
vers_lonlat <- function(x, y, systeme) {
  s <- if (is.list(systeme)) systeme else systeme_coordonnees(systeme)
  if (is.null(s)) stop(sprintf(traduire("Système de coordonnées inconnu : %s", "Unknown coordinate system: %s"), systeme), call. = FALSE)
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (s$type == "plan") return(list(lon = rep(NA_real_, length(x)), lat = rep(NA_real_, length(y))))
  el <- ELLIPSOIDES[[s$ellipsoide]]
  e <- sqrt(el$f * (2 - el$f))
  g <- switch(s$type,
    lonlat = list(lon = x, lat = y),
    lcc = lcc_arriere(x, y, s, e),
    tm = tm_arriere(x, y, s, e),
    laea = laea_arriere(x, y, s, e),
    merc = list(lon = deg(x / el$a), lat = deg(2 * atan(exp(y / el$a)) - pi / 2)))
  if (!is.null(s$vers_wgs84)) g <- changer_datum(g$lon, g$lat, el, ELLIPSOIDES$WGS84, s$vers_wgs84)
  g
}

# L'inverse : la longitude et la latitude WGS 84 en coordonnées de `systeme`.
depuis_lonlat <- function(lon, lat, systeme) {
  s <- if (is.list(systeme)) systeme else systeme_coordonnees(systeme)
  if (is.null(s)) stop(sprintf(traduire("Système de coordonnées inconnu : %s", "Unknown coordinate system: %s"), systeme), call. = FALSE)
  if (s$type == "plan") stop(traduire("Ce système de coordonnées n'est pas précisé : impossible d'y convertir.", "This coordinate system isn't specified: nothing can be converted to it."), call. = FALSE)
  el <- ELLIPSOIDES[[s$ellipsoide]]
  e <- sqrt(el$f * (2 - el$f))
  if (!is.null(s$vers_wgs84)) {
    g <- changer_datum(lon, lat, ELLIPSOIDES$WGS84, el, -s$vers_wgs84)
    lon <- g$lon
    lat <- g$lat
  }
  switch(s$type,
    lonlat = list(x = lon, y = lat),
    lcc = lcc_avant(lon, lat, s, e),
    tm = tm_avant(lon, lat, s, e),
    laea = laea_avant(lon, lat, s, e),
    merc = list(x = el$a * rad(lon), y = el$a * log(tan(pi / 4 + rad(lat) / 2))))
}

convertir_coordonnees <- function(x, y, de, vers) {
  if (identical(as.character(de), as.character(vers))) return(list(x = as.numeric(x), y = as.numeric(y)))
  g <- vers_lonlat(x, y, de)
  depuis_lonlat(g$lon, g$lat, vers)
}

# ---------------------------------------------------------------- coordonnées d'un projet

# Un système plausible pour des valeurs x et y : des degrés (longitude, latitude), le
# Lambert-93, le Lambert II étendu, sinon des coordonnées planes sans système précisé.
deviner_systeme <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (!any(ok)) return("plan")
  x <- x[ok]
  y <- y[ok]
  dans <- function(v, a, b) all(v >= a & v <= b)
  if (dans(x, -180, 180) && dans(y, -90, 90)) return(4326)
  if (dans(x, 50000, 1300000) && dans(y, 6000000, 7200000)) return(2154)
  if (dans(x, 0, 1250000) && dans(y, 1600000, 2750000)) return(27572)
  "plan"
}

# Deux variables quantitatives qui ressemblent à des coordonnées, d'après leur nom (longitude
# et latitude, X et Y, est et nord), et leur système d'après leurs valeurs ; NULL sinon.
proposer_coordonnees <- function(projet) {
  vars <- Filter(function(v) !length(v$categories) && as.character(v$uid) %in% colnames(projet$donnees), projet$variables)
  if (length(vars) < 2) return(NULL)
  noms <- tolower(vapply(vars, function(v) as.character(v$ids[1]), ""))
  cherche <- function(motif) { i <- grep(motif, noms, perl = TRUE); if (length(i)) i[1] else NA }
  paires <- list(c("^(lon|long|longitude|lng)([ _.]|$)", "^(lat|latitude)([ _.]|$)"),
                 c("^x([ _.]|$)|^x_?l93|^xl93|^coord_?x|^x_?lambert|^easting|^est$|^x_?utm", "^y([ _.]|$)|^y_?l93|^yl93|^coord_?y|^y_?lambert|^northing|^nord$|^y_?utm"))
  for (pr in paires) {
    i <- cherche(pr[1])
    j <- cherche(pr[2])
    if (is.na(i) || is.na(j) || i == j) next
    ux <- as.numeric(vars[[i]]$uid)
    uy <- as.numeric(vars[[j]]$uid)
    return(list(x = ux, y = uy, systeme = deviner_systeme(projet$donnees[, as.character(ux)], projet$donnees[, as.character(uy)])))
  }
  NULL
}

# Les coordonnées d'un projet telles qu'elles sont réglées (p$coordonnees), vérifiées : deux
# variables quantitatives différentes qui existent, et un système connu ; NULL si aucune.
coordonnees_projet <- function(projet, exiger = FALSE) {
  co <- projet$coordonnees
  ok <- is.list(co) && length(co$x) && length(co$y) && !identical(as.numeric(co$x), as.numeric(co$y))
  if (ok) {
    vx <- tryCatch(variable_du_projet(projet, co$x), error = function(e) NULL)
    vy <- tryCatch(variable_du_projet(projet, co$y), error = function(e) NULL)
    ok <- !is.null(vx) && !is.null(vy) && !length(vx$categories) && !length(vy$categories) && !is.null(systeme_coordonnees(co$systeme))
  }
  if (!ok) {
    if (exiger) stop(traduire("Ce projet n'a pas de coordonnées : choisissez-les dans Graphiques › Carte (deux variables et leur système).",
                              "This project has no coordinates: choose them in Plots › Map (two variables and their system)."), call. = FALSE)
    return(NULL)
  }
  list(x = as.numeric(co$x), y = as.numeric(co$y), systeme = systeme_coordonnees(co$systeme)$code, nom_x = as.character(vx$ids[1]), nom_y = as.character(vy$ids[1]))
}

# Règle les coordonnées d'un projet (x, y : numéros de variables ; systeme : code EPSG ou
# « plan ») ; x = NULL les retire. Vérifie que les valeurs vont avec le système.
regler_coordonnees <- function(projet, x, y, systeme) {
  if (is.null(x) || !length(x)) {
    projet$coordonnees <- NULL
    return(projet)
  }
  s <- systeme_coordonnees(systeme)
  if (is.null(s)) stop(sprintf(traduire("Système de coordonnées inconnu : %s", "Unknown coordinate system: %s"), systeme), call. = FALSE)
  projet$coordonnees <- list(x = as.numeric(x)[1], y = as.numeric(y)[1], systeme = s$code)
  co <- coordonnees_projet(projet)
  if (is.null(co)) stop(traduire("Les coordonnées doivent être deux variables quantitatives différentes du projet.", "Coordinates must be two different quantitative variables of the project."), call. = FALSE)
  if (s$type == "lonlat") {
    vx <- projet$donnees[, as.character(co$x)]
    vy <- projet$donnees[, as.character(co$y)]
    if (any(abs(vx) > 180, na.rm = TRUE) || any(abs(vy) > 90, na.rm = TRUE)) {
      stop(sprintf(traduire("Des longitudes et latitudes vont de −180 à 180 et de −90 à 90 : « %s » ou « %s » sort de ces bornes. Choisissez le système des coordonnées planes.",
                            "Longitudes and latitudes run from −180 to 180 and from −90 to 90: “%s” or “%s” goes beyond. Choose the system of the plane coordinates."), co$nom_x, co$nom_y), call. = FALSE)
    }
  }
  projet
}

# Les coordonnées des observations `obs` (dans leur système), avec leur longitude et latitude
# WGS 84 quand le système le permet.
points_projet <- function(projet, obs = NULL) {
  co <- coordonnees_projet(projet, exiger = TRUE)
  obs <- obs %||% as.numeric(rownames(projet$donnees))
  x <- colonne(projet, co$x, obs)
  y <- colonne(projet, co$y, obs)
  g <- vers_lonlat(x, y, co$systeme)
  list(uid = obs, x = x, y = y, lon = g$lon, lat = g$lat, systeme = co$systeme, coordonnees = co)
}

# ---------------------------------------------------------------- distances

RAYON_TERRE <- 6371008.8  # (le rayon moyen de l'ellipsoïde WGS 84, en mètres)

# Les distances (en mètres) entre des points : le long d'un grand cercle (formule de haversine)
# pour des longitudes et latitudes, en ligne droite pour des coordonnées planes. Une matrice ;
# `y2`, `x2` : un second jeu de points (les colonnes), sinon les mêmes.
distances_points <- function(x, y, systeme, x2 = x, y2 = y) {
  s <- systeme_coordonnees(systeme)
  if (s$type == "lonlat") {
    p1 <- rad(y)
    p2 <- rad(y2)
    dp <- outer(p1, p2, "-")
    dl <- outer(rad(x), rad(x2), "-")
    h <- sin(dp / 2)^2 + outer(cos(p1), cos(p2)) * sin(dl / 2)^2
    return(2 * RAYON_TERRE * asin(pmin(sqrt(h), 1)))  # (pmin garde les dimensions de son premier argument)
  }
  sqrt(outer(x, x2, "-")^2 + outer(y, y2, "-")^2)
}

# ---------------------------------------------------------------- autocorrélation spatiale

# Les poids entre observations (W, n × n, diagonale nulle) d'après leurs distances D :
#   "voisins"  les k plus proches voisins de chacune (1, rendus symétriques : i voisin de j ou j de i) ;
#   "bande"    1 jusqu'à la distance `portee` (par défaut, la plus petite qui donne un voisin à
#              chaque observation : la plus grande des distances au plus proche voisin) ;
#   "inverse"  1 / d, jusqu'à `portee` si elle est donnée.
# `standardiser` : chaque ligne ramenée à une somme de 1 (le style « W » de spdep ; sinon « B »,
# les poids tels quels). Une observation sans voisin garde une ligne de zéros.
poids_spatiaux <- function(D, methode = "voisins", k = 4, portee = NULL, standardiser = TRUE) {
  n <- nrow(D)
  W <- matrix(0, n, n)
  diag(D) <- Inf
  methode <- match.arg(methode, c("voisins", "bande", "inverse"))
  if (methode == "voisins") {
    k <- max(1L, min(as.integer(k), n - 1L))
    for (i in seq_len(n)) W[i, order(D[i, ])[seq_len(k)]] <- 1
    W <- pmax(W, t(W))
  } else {
    if (is.null(portee) || !is.finite(portee) || portee <= 0) portee <- if (methode == "bande") max(apply(D, 1, min)) else Inf
    dans <- D <= portee * (1 + 1e-12)
    W[dans] <- if (methode == "bande") 1 else 1 / D[dans]
  }
  diag(W) <- 0
  if (standardiser) {
    s <- rowSums(W)
    W[s > 0, ] <- W[s > 0, ] / s[s > 0]
  }
  attr(W, "portee") <- if (methode == "voisins") NULL else portee
  W
}

# L'I de Moran (1950) de x pour les poids W, son espérance −1/(n − 1) et sa variance sous
# l'hypothèse de randomisation (Cliff et Ord 1981, comme spdep::moran.test), la valeur z et la
# valeur p unilatérale (une autocorrélation positive, l'alternative de spdep par défaut) ; et la
# valeur p de `permutations` permutations de x (comme spdep::moran.mc : (r + 1) / (P + 1), r les
# permutations dont l'I atteint celui des données), d'une graine fixe.
moran <- function(x, W, permutations = 999, graine = 1) {
  n <- length(x)
  z <- x - mean(x)
  S0 <- sum(W)
  if (S0 <= 0) stop(traduire("Aucune paire d'observations voisines : élargissez le voisinage.", "No pair of neighbouring observations: widen the neighbourhood."), call. = FALSE)
  ss <- sum(z^2)
  if (ss <= 0) stop(traduire("La variable est constante : pas d'autocorrélation à mesurer.", "The variable is constant: no autocorrelation to measure."), call. = FALSE)
  I <- (n / S0) * sum(z * (W %*% z)) / ss
  S1 <- 0.5 * sum((W + t(W))^2)
  S2 <- sum((rowSums(W) + colSums(W))^2)
  b2 <- n * sum(z^4) / ss^2
  E <- -1 / (n - 1)
  V <- (n * ((n^2 - 3 * n + 3) * S1 - n * S2 + 3 * S0^2) - b2 * ((n^2 - n) * S1 - 2 * n * S2 + 6 * S0^2)) /
    ((n - 1) * (n - 2) * (n - 3) * S0^2) - E^2
  zv <- (I - E) / sqrt(V)
  r <- list(I = I, esperance = E, variance = V, z = zv, p = stats::pnorm(zv, lower.tail = FALSE), n = n, S0 = S0)
  if (permutations > 0) {
    Ip <- avec_graine(graine, vapply(seq_len(permutations), function(i) { zp <- sample(z); (n / S0) * sum(zp * (W %*% zp)) / ss }, 0))
    r$permutations <- permutations
    r$p_permutations <- (sum(Ip >= I) + 1) / (permutations + 1)
  }
  r
}

# L'I de Moran des résidus d'un modèle linéaire (moindres carrés, matrice X des colonnes du
# modèle), avec l'espérance et la variance qui tiennent compte de l'ajustement (Cliff et Ord
# 1981, comme spdep::lm.morantest) : M = I − X(X'X)⁻¹X', E = (n/S0) tr(MW)/(n − p),
# Var = (n/S0)² [tr(MWMWᵀ) + tr((MW)²) + tr(MW)²] / ((n − p)(n − p + 2)) − E².
moran_residus_lm <- function(e, X, W) {
  n <- length(e)
  S0 <- sum(W)
  q <- qr(X)
  p <- q$rank
  M <- diag(n) - qr.Q(q)[, seq_len(p), drop = FALSE] %*% t(qr.Q(q)[, seq_len(p), drop = FALSE])
  MW <- M %*% W
  I <- (n / S0) * sum(e * (W %*% e)) / sum(e^2)
  E <- (n / S0) * sum(diag(MW)) / (n - p)
  V <- (n / S0)^2 * (sum(MW * t(M %*% t(W))) + sum(MW * t(MW)) + sum(diag(MW))^2) / ((n - p) * (n - p + 2)) - E^2
  zv <- (I - E) / sqrt(V)
  list(I = I, esperance = E, variance = V, z = zv, p = stats::pnorm(zv, lower.tail = FALSE), n = n, rang = p)
}

# Le corrélogramme de Moran : l'I des paires d'observations dans chaque classe de distance
# (poids binaires des paires de la classe, non standardisés, comme un voisinage en anneau de
# spdep::dnearneigh), son espérance, sa variance sous la randomisation, z et la valeur p
# bilatérale, corrigée par Holm sur les classes ; `classes` classes de même largeur jusqu'à la
# moitié de la plus grande distance (au-delà, peu de paires, sur les bords de la zone).
correlogramme_moran <- function(x, D, classes = 10, max_distance = NULL) {
  dmax <- max_distance %||% (max(D[upper.tri(D)]) / 2)
  bornes <- seq(0, dmax, length.out = classes + 1)
  lignes <- lapply(seq_len(classes), function(k) {
    W <- (D > bornes[k] & D <= bornes[k + 1]) * 1
    diag(W) <- 0
    paires <- sum(W) / 2
    r <- if (paires > 0 && any(rowSums(W) > 0)) tryCatch(moran(x, W, permutations = 0), error = function(e) NULL)
    data.frame(classe = k, de = bornes[k], a = bornes[k + 1], distance = if (paires > 0) mean(D[upper.tri(D) & W > 0]) else NA_real_,
               paires = paires, I = r$I %||% NA_real_, esperance = r$esperance %||% NA_real_, z = r$z %||% NA_real_,
               p = if (is.null(r)) NA_real_ else 2 * stats::pnorm(-abs(r$z)))
  })
  out <- do.call(rbind, lignes)
  out$p_holm <- stats::p.adjust(out$p, "holm")
  out
}

# Le semi-variogramme expérimental (Matheron 1963) : dans chaque classe de distance, la moitié
# de la moyenne des carrés des écarts entre les valeurs des paires d'observations, γ(h) = Σ (xi −
# xj)² / (2 N(h)), avec N(h) et la distance moyenne des paires ; par défaut comme gstat::variogram,
# jusqu'au tiers de la diagonale du rectangle qui contient les points, en 15 classes.
variogramme <- function(x, D, portee = NULL, classes = 15, diagonale = NULL) {
  portee <- portee %||% ((diagonale %||% max(D)) / 3)
  largeur <- portee / classes
  i <- which(upper.tri(D) & D <= portee, arr.ind = TRUE)
  d <- D[i]
  g <- (x[i[, 1]] - x[i[, 2]])^2 / 2
  k <- pmax(1L, ceiling(d / largeur))  # (une distance nulle va dans la première classe)
  k <- pmin(k, classes)
  garde <- tabulate(k, classes) > 0
  data.frame(classe = which(garde), de = (which(garde) - 1) * largeur, a = which(garde) * largeur,
             paires = tabulate(k, classes)[garde], distance = as.vector(tapply(d, factor(k, seq_len(classes)), mean))[garde],
             gamma = as.vector(tapply(g, factor(k, seq_len(classes)), mean))[garde])
}

# ---------------------------------------------------------------- la demande

# L'autocorrélation spatiale (Graphiques › Autocorrélation spatiale) d'une variable du projet
# (`variable`, sur les observations du modèle s'il y en a un, sinon toutes) ou des résidus du
# modèle `m` : `residus` "standardises" (ceux des vues du modèle : standardisés, de déviance pour
# mgcv, normalisés avec CAR1) ou "quantiles" (les résidus quantiles randomisés d'un comptage, d'une
# présence-absence ou de valeurs positives). Les observations sans coordonnées ou sans valeur sont
# laissées de côté, et comptées. `voisinage` : list(methode, k, portee en mètres, standardiser)
# (poids_spatiaux) ; l'I de Moran (randomisation et permutations), celui des résidus d'un modèle
# linéaire sans effet aléatoire qui tient compte de son ajustement (moran_residus_lm), le
# corrélogramme et le semi-variogramme. `par_lieu` : les valeurs des observations au même endroit
# (la même placette suivie plusieurs années) remplacées par leur moyenne, une par lieu ; sinon
# leurs paires à distance nulle comptent comme les plus proches voisines.
autocorrelation_spatiale <- function(projet, m = NULL, variable = NULL, residus = "standardises", voisinage = list(),
                                     permutations = 999, classes = 10, par_lieu = FALSE) {
  obs <- if (!is.null(m)) m$observations else as.numeric(rownames(projet$donnees))
  lm_simple <- FALSE
  if (!is.null(variable) && length(variable)) {
    v <- variable_du_projet(projet, variable)
    if (length(v$categories)) stop(traduire("Choisissez une variable quantitative.", "Choose a quantitative variable."), call. = FALSE)
    valeurs <- colonne(projet, variable, obs)
    nom <- as.character(v$ids[1])
  } else {
    if (is.null(m) || !inherits(m, "modele_regression")) {
      stop(traduire("Les résidus ne se calculent ici que pour une régression : choisissez une variable.", "Residuals are computed here only for a regression: choose a variable."), call. = FALSE)
    }
    r <- m$resultat
    quantiles <- identical(residus, "quantiles")
    if (quantiles && is.null(r$residus_quantiles)) {
      stop(traduire("Ce modèle n'a pas de résidus quantiles (seulement les comptages, présences-absences et valeurs positives).", "This model has no quantile residuals (only counts, presence-absence and positive values)."), call. = FALSE)
    }
    valeurs <- if (quantiles) r$residus_quantiles else r$residus
    nom <- if (quantiles) traduire("résidus quantiles", "quantile residuals") else traduire("résidus", "residuals")
    lm_simple <- identical(class(m$ajustement)[1], "lm") && !quantiles
  }
  pts <- points_projet(projet, obs)
  garde <- is.finite(valeurs) & is.finite(pts$x) & is.finite(pts$y)
  if (sum(garde) < 5) stop(traduire("Il faut au moins 5 observations avec des coordonnées et une valeur.", "At least 5 observations with coordinates and a value are needed."), call. = FALSE)
  x <- valeurs[garde]
  px <- pts$x[garde]
  py <- pts$y[garde]
  uid <- obs[garde]
  lieux <- NULL
  if (par_lieu) {
    cle <- factor(paste(px, py), levels = unique(paste(px, py)))
    lieux <- as.vector(table(cle))
    x <- as.vector(tapply(x, cle, mean))
    premier <- !duplicated(cle)
    px <- px[premier]
    py <- py[premier]
    uid <- uid[premier]
    lm_simple <- FALSE  # (des moyennes de résidus ne sont plus ceux de l'ajustement)
    if (length(x) < 5) stop(traduire("Il faut au moins 5 lieux différents.", "At least 5 different places are needed."), call. = FALSE)
  }
  D <- distances_points(px, py, pts$systeme)
  nulles <- sum(D[upper.tri(D)] == 0)  # (des paires au même endroit : la même placette deux années)
  methode <- as.character(voisinage$methode %||% "voisins")
  W <- poids_spatiaux(D, methode, k = as.numeric(voisinage$k %||% 4), portee = suppressWarnings(as.numeric(voisinage$portee)),
                      standardiser = !identical(voisinage$standardiser, FALSE))
  mo <- moran(x, W, permutations = max(0, min(9999, as.integer(permutations))))
  if (lm_simple) {
    a <- m$ajustement
    # (les résidus de l'ajustement sur les seules observations gardées, e = My : sans coordonnées,
    # une observation sort aussi de l'ajustement dont le test suppose les résidus)
    X <- stats::model.matrix(a)[garde, , drop = FALSE]
    mo$modele <- tryCatch(moran_residus_lm(qr.resid(qr(X), m$donnees$y[garde]), X, W), error = function(e) NULL)
  }
  coins <- if (systeme_coordonnees(pts$systeme)$type == "lonlat") {
    distances_points(min(px), min(py), pts$systeme, max(px), max(py))[1, 1]
  } else sqrt(diff(range(px))^2 + diff(range(py))^2)
  list(nom = nom, n = length(x), observations = sum(garde), laissees = sum(!garde), uid = uid, valeurs = x, nulles = nulles, lieux = lieux,
       voisinage = list(methode = methode, k = if (methode == "voisins") as.numeric(voisinage$k %||% 4), portee = attr(W, "portee"),
                        standardiser = !identical(voisinage$standardiser, FALSE), voisins = mean(rowSums(W > 0)),
                        sans_voisin = sum(rowSums(W > 0) == 0)),
       moran = mo, correlogramme = correlogramme_moran(x, D, classes = max(2, min(30, as.integer(classes)))),
       variogramme = variogramme(x, D, diagonale = coins), distance_max = max(D), systeme = pts$systeme)
}
