# Vérifie les valeurs de rasters aux observations (moteur/R/raster.R) : des GeoTIFF écrits ici
# octet par octet (petit- et gros-boutistes, BigTIFF, bandes de lignes et tuiles, bandes
# entrelacées et séparées, chaque type, sans compression, PackBits, Deflate et LZW codés ici,
# prédicteurs 2 et 3, PixelIsPoint, ModelTransformation, un système défini par ses paramètres) et
# relus ; des grilles ASCII d'ESRI et leur .prj ; les systèmes de coordonnées (codes EPSG, WKT
# d'ESRI et de l'OGC) ; les refus (en français et en anglais) ; les valeurs aux points (la cellule,
# l'interpolation bilinéaire, la moyenne et la classe majoritaire dans un cercle) ; l'ajout au
# projet (les noms, les comptes, des tuiles sous un même nom, enregistré et relu, le texte des
# Méthodes). Puis, quand terra est installé (NUEE_TERRA=<bibliothèque> l'ajoute), contre GDAL et
# terra::extract : des rasters écrits par terra::writeRaster avec les options de GDAL
# (COMPRESS=NONE/LZW/DEFLATE/PACKBITS/LZMA/ZSTD, PREDICTOR=2/3, TILED, BIGTIFF, INTERLEAVE, chaque
# type, la valeur manquante, ENDIANNESS, SPARSE_OK, un COG avec ses aperçus, une palette, l'échelle
# et le décalage, le nom des bandes, AAIGrid) en Lambert-93, WGS 84 et UTM 31N, à des points au
# hasard (dedans, dehors, sur des valeurs manquantes), avec method = "simple" et "bilinear" et des
# cercles ; les GeoTIFF écrits ici, lus par GDAL ; le MNT du Luxembourg livré avec terra
# (elev.tif) ; un projet en Lambert-93 et un raster en UTM 31N, les points convertis par PROJ.
#   Rscript moteur/tests/verifier_raster.R
# NUEE_TERRA : une bibliothèque où est installé terra.

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
en_anglais <- function(expr) { ancien <- options(nuee.langue = "en"); on.exit(options(ancien)); refus(expr) }
# (des valeurs égales à `tol` près, les manquantes aux mêmes places)
proche <- function(a, b, tol) {
  a <- as.numeric(unlist(a)); b <- as.numeric(unlist(b))
  length(a) == length(b) && identical(is.na(a), is.na(b)) && (all(is.na(a)) || isTRUE(max(abs(a - b), na.rm = TRUE) <= tol))
}
dossier <- tempfile("rasters")
dir.create(dossier)
f <- function(nom) file.path(dossier, nom)

# ---------------------------------------------------------------- un GeoTIFF écrit octet par octet

TYPES <- list(uint8 = c(1, 1), int8 = c(1, 2), uint16 = c(2, 1), int16 = c(2, 2), uint32 = c(4, 1), int32 = c(4, 2), float32 = c(4, 3), float64 = c(8, 3))

# Des valeurs en octets, dans l'ordre `fin`.
octets_valeurs <- function(v, type, fin) {
  t <- TYPES[[type]]
  if (t[2] == 3) return(writeBin(as.numeric(v), raw(), size = t[1], endian = fin))
  v <- v %% 2^(8 * t[1])  # (le motif non signé)
  if (t[1] == 1) return(as.raw(v))
  if (t[1] == 2) return(writeBin(as.integer(v), raw(), size = 2, endian = fin))
  writeBin(as.integer(ifelse(v >= 2^31, v - 2^32, v)), raw(), size = 4, endian = fin)
}

# LZW de TIFF codé ici (comme libtiff : un code 256 au début, la largeur qui croît quand l'entrée
# suivante ne tiendrait plus, 256 quand le dictionnaire atteint 4094, 257 à la fin), les codes
# l'octet de poids fort d'abord.
coder_lzw <- function(b) {
  x <- as.integer(b)
  dico <- new.env(hash = TRUE)
  codes <- integer(0)
  largeurs <- integer(0)
  libre <- 258
  nbits <- 9
  emettre <- function(code) { codes[length(codes) + 1L] <<- code; largeurs[length(largeurs) + 1L] <<- nbits }
  apres <- function() {
    libre <<- libre + 1
    if (libre == 4094) { emettre(256L); rm(list = ls(dico), envir = dico); libre <<- 258; nbits <<- 9 }
    else if (libre > 2^nbits - 1) nbits <<- nbits + 1
  }
  emettre(256L)
  w <- x[1]
  for (c in x[-1]) {
    cle <- paste(w, c)
    e <- dico[[cle]]
    if (!is.null(e)) { w <- e; next }
    emettre(w)
    dico[[cle]] <- libre
    apres()
    w <- c
  }
  emettre(w)
  libre <- libre + 1
  if (libre == 4094) { emettre(256L); nbits <- 9 } else if (libre > 2^nbits - 1) nbits <- nbits + 1
  emettre(257L)
  bits <- unlist(lapply(seq_along(codes), function(k) (codes[k] %/% 2^((largeurs[k] - 1):0)) %% 2))
  bits <- c(bits, rep(0, (-length(bits)) %% 8))
  as.raw(colSums(matrix(bits, 8) * 2^(7:0)))
}

# PackBits codé ici : les suites d'au moins 3 octets égaux répétées, le reste tel quel.
coder_packbits <- function(b) {
  x <- as.integer(b)
  sortie <- integer(0)
  i <- 1
  n <- length(x)
  while (i <= n) {
    r <- 1
    while (i + r <= n && x[i + r] == x[i] && r < 128) r <- r + 1
    if (r >= 3) { sortie <- c(sortie, 257 - r, x[i]); i <- i + r; next }
    j <- i
    while (j <= n && j - i < 128 && !(j + 2 <= n && x[j] == x[j + 1] && x[j] == x[j + 2])) j <- j + 1
    if (j == i) j <- i + 1
    sortie <- c(sortie, j - i - 1, x[i:(j - 1)])
    i <- j
  }
  as.raw(sortie)
}

# Écrit un GeoTIFF : `v` un tableau [lignes, colonnes, bandes] (ou une matrice), `type`, `fin`,
# BigTIFF (`big`), en tuiles de `tuile` cellules ou en bandes de `rps` lignes, bandes séparées
# (`planaire`), `compression` ("aucune", "packbits", "deflate", "lzw", ou un code), prédicteur,
# `gt` la transformation de GDAL (le coin de la première cellule), `point` (PixelIsPoint),
# `transformation` (ModelTransformation plutôt que ModelPixelScale et ModelTiepoint), `cles` (des
# GeoKeys : list(numéro = valeur), un réel ou un texte allant dans leurs tableaux), `nodata`, `meta`
# (le XML de GDAL), `palette`, `bits` et `format` (pour des fichiers qu'on refuse).
ecrire_tiff <- function(chemin, v, type = "float32", fin = "little", big = FALSE, tuile = NULL, rps = NULL, planaire = FALSE,
                        compression = "aucune", predicteur = 1, gt = c(0, 1, 0, 0, 0, -1), point = FALSE, transformation = FALSE,
                        cles = list(`1024` = 1, `3072` = 2154), nodata = NULL, meta = NULL, palette = FALSE, bits = NULL, format = NULL,
                        sans_geo = FALSE, points_appui = NULL) {
  if (is.matrix(v)) v <- array(v, c(dim(v), 1))
  H <- dim(v)[1]; W <- dim(v)[2]; B <- dim(v)[3]
  t <- TYPES[[type]]
  o <- t[1]
  code_compression <- if (is.numeric(compression)) compression else c(aucune = 1, packbits = 32773, deflate = 8, lzw = 5)[[compression]]
  pas <- if (planaire) 1 else B
  # les blocs : (lignes, colonnes) de chaque bloc, dans l'ordre du fichier, plan par plan
  if (!is.null(tuile)) {
    bl <- tuile; bh <- tuile
    blocs <- expand.grid(tj = seq_len(ceiling(W / bl)) - 1, ti = seq_len(ceiling(H / bh)) - 1)
  } else {
    bl <- W; bh <- rps %||% H
    blocs <- data.frame(tj = 0, ti = seq_len(ceiling(H / bh)) - 1)
  }
  donnees <- list()
  for (plan in if (planaire) seq_len(B) else 1) for (k in seq_len(nrow(blocs))) {
    lignes <- blocs$ti[k] * bh + seq_len(bh) - 1
    if (is.null(tuile)) lignes <- lignes[lignes < H]
    colonnes <- blocs$tj[k] * bl + seq_len(bl) - 1
    bandes <- if (planaire) plan else seq_len(B)
    # les échantillons, ligne par ligne, cellule par cellule, bande par bande (0 hors de l'image)
    m <- matrix(0, length(bandes) * length(colonnes), length(lignes))
    for (a in seq_along(lignes)) for (b in seq_along(bandes)) {
      val <- rep(0, length(colonnes))
      dans <- lignes[a] < H & colonnes < W
      if (lignes[a] < H) val[dans] <- v[lignes[a] + 1, colonnes[dans] + 1, bandes[b]]
      m[(seq_along(colonnes) - 1) * length(bandes) + b, a] <- val
    }
    brut <- if (predicteur == 2) {
      u <- m %% 2^(8 * o)
      d <- u
      if (nrow(u) > pas) d[-seq_len(pas), ] <- (u[-seq_len(pas), , drop = FALSE] - u[seq_len(nrow(u) - pas), , drop = FALSE]) %% 2^(8 * o)
      octets_valeurs(as.vector(d), sub("^int", "uint", type), fin)
    } else if (predicteur == 3) {
      unlist(lapply(seq_len(ncol(m)), function(a) {
        oc <- matrix(as.integer(writeBin(m[, a], raw(), size = o, endian = "big")), nrow = o)  # (une colonne par valeur, l'octet de poids fort d'abord)
        x <- as.vector(t(oc))  # (par plans d'octets)
        d <- x
        if (length(x) > pas) d[-seq_len(pas)] <- (x[-seq_len(pas)] - x[seq_len(length(x) - pas)]) %% 256
        as.raw(d)
      }))
    } else octets_valeurs(as.vector(m), type, fin)
    donnees[[length(donnees) + 1]] <- switch(as.character(code_compression),
      "1" = brut, "32773" = coder_packbits(brut), "8" = memCompress(brut, "gzip"), "5" = coder_lzw(brut), brut)
  }
  # les étiquettes : (numéro, type TIFF, valeurs en octets, nombre)
  court <- function(x) writeBin(as.integer(x), raw(), size = 2, endian = fin)
  long <- function(x) octets_valeurs(x, "uint32", fin)
  long8 <- function(x) { x <- as.numeric(x); unlist(lapply(x, function(y) if (fin == "little") c(long(y %% 2^32), long(y %/% 2^32)) else c(long(y %/% 2^32), long(y %% 2^32)))) }
  reel <- function(x) writeBin(as.numeric(x), raw(), size = 8, endian = fin)
  texte <- function(s) c(charToRaw(enc2utf8(s)), as.raw(0))
  e <- list()
  ajouter <- function(n, type, brut, nombre) e[[length(e) + 1]] <<- list(n = n, type = type, brut = brut, nombre = nombre)
  ajouter(256, 4, long(W), 1); ajouter(257, 4, long(H), 1)
  ajouter(258, 3, court(rep(bits %||% (8 * o), B)), B)
  ajouter(259, 3, court(code_compression), 1)
  ajouter(262, 3, court(if (palette) 3 else 1), 1)
  ajouter(277, 3, court(B), 1)
  ajouter(284, 3, court(if (planaire) 2 else 1), 1)
  if (B > 1) ajouter(338, 3, court(rep(0, B - 1)), B - 1)  # (des bandes en plus de la première, sans rôle particulier)
  if (predicteur > 1) ajouter(317, 3, court(predicteur), 1)
  ajouter(339, 3, court(rep(format %||% t[2], B)), B)
  if (palette) ajouter(320, 3, court(rep(seq(0, 65535, length.out = 256), 3)), 768)
  nb <- length(donnees)
  if (!is.null(tuile)) { ajouter(322, 4, long(bl), 1); ajouter(323, 4, long(bh), 1) } else ajouter(278, 4, long(bh), 1)
  if (!sans_geo) {
    if (!is.null(points_appui)) {
      ajouter(33922, 12, reel(points_appui), length(points_appui))
    } else if (transformation) {
      ajouter(34264, 12, reel(c(gt[2], gt[3], 0, gt[1], gt[5], gt[6], 0, gt[4], 0, 0, 0, 0, 0, 0, 0, 1)), 16)
    } else {
      ajouter(33550, 12, reel(c(gt[2], -gt[6], 0)), 3)
      ajouter(33922, 12, reel(c(0, 0, 0, gt[1], gt[4], 0)), 6)
    }
  }
  if (length(cles) || point) {
    cles <- c(cles, if (point) list(`1025` = 2) else list(`1025` = 1))
    cles <- cles[order(as.numeric(names(cles)))]
    reels <- numeric(0); ascii <- ""
    entrees <- unlist(lapply(names(cles), function(k) {
      x <- cles[[k]]
      if (is.character(x)) { a <- nchar(ascii, "bytes"); ascii <<- paste0(ascii, x, "|"); c(as.numeric(k), 34737, nchar(x, "bytes") + 1, a) }
      else if (x != round(x) || abs(x) > 65535) { a <- length(reels); reels <<- c(reels, x); c(as.numeric(k), 34736, 1, a) }
      else c(as.numeric(k), 0, 1, x)
    }))
    ajouter(34735, 3, court(c(1, 1, 0, length(cles), entrees)), 4 + length(entrees))
    if (length(reels)) ajouter(34736, 12, reel(reels), length(reels))
    if (nzchar(ascii)) ajouter(34737, 2, texte(ascii), nchar(ascii, "bytes") + 1)
  }
  if (!is.null(meta)) ajouter(42112, 2, texte(meta), nchar(meta, "bytes") + 1)
  if (!is.null(nodata)) ajouter(42113, 2, texte(as.character(nodata)), nchar(as.character(nodata)) + 1)
  # le fichier : l'en-tête, les blocs, les valeurs hors des entrées, le répertoire
  tete <- if (big) 16 else 8
  corps <- raw(0)
  position <- tete
  offsets <- numeric(nb)
  for (k in seq_len(nb)) { offsets[k] <- position + length(corps); corps <- c(corps, donnees[[k]]); if (length(corps) %% 2) corps <- c(corps, as.raw(0)) }
  type_off <- if (big) 16 else 4
  enc_off <- if (big) long8 else long
  ajouter(if (is.null(tuile)) 273 else 324, type_off, enc_off(offsets), nb)
  ajouter(if (is.null(tuile)) 279 else 325, type_off, enc_off(vapply(donnees, length, 0)), nb)
  e <- e[order(vapply(e, `[[`, 0, "n"))]
  champ <- if (big) 8 else 4
  for (k in seq_along(e)) if (length(e[[k]]$brut) > champ) {
    e[[k]]$ou <- position + length(corps)
    corps <- c(corps, e[[k]]$brut)
    if (length(corps) %% 2) corps <- c(corps, as.raw(0))
  }
  ifd <- position + length(corps)
  entree <- function(x) {
    valeur <- if (length(x$brut) > champ) enc_off(x$ou) else c(x$brut, raw(champ - length(x$brut)))
    c(court(x$n), court(x$type), if (big) long8(x$nombre) else long(x$nombre), valeur)
  }
  repertoire <- c(if (big) long8(length(e)) else court(length(e)), unlist(lapply(e, entree)), if (big) long8(0) else long(0))
  entete <- c(charToRaw(if (fin == "little") "II" else "MM"), court(if (big) 43 else 42), if (big) c(court(8), court(0), long8(ifd)) else long(ifd))
  writeBin(c(entete, corps, repertoire), chemin)
  invisible(chemin)
}

# toutes les cellules d'un raster ouvert, en tableau [lignes, colonnes, bandes]
toutes <- function(g) {
  ij <- expand.grid(j = seq_len(g$largeur) - 1, i = seq_len(g$hauteur) - 1)
  array(valeurs_cellules(g, ij$i, ij$j), c(g$largeur, g$hauteur, g$bandes))
}
comme_lu <- function(v) aperm(if (is.matrix(v)) array(v, c(dim(v), 1)) else v, c(2, 1, 3))

cat("GeoTIFF écrits ici, relus\n")
set.seed(1)
H <- 23; W <- 31
# (des valeurs de 10 à 118, dans l'étendue de chaque type ; une matrice pour une bande)
valeurs_type <- function(type, B = 1) {
  v <- array(0, c(H, W, B))
  for (b in seq_len(B)) v[, , b] <- outer(seq_len(H), seq_len(W), function(i, j) 60 + 30 * sin(i / 4 + b) + 20 * cos(j / 5) + i / 3)
  v <- switch(type, uint8 = round(v) %% 250, int8 = round(v) %% 200 - 100, uint16 = round(v * 300), int16 = round(v * 100) - 3000,
              uint32 = round(v * 3e7), int32 = round(v * 1e7) - 4e8, float32 = array(readBin(writeBin(as.vector(v), raw(), size = 4), "double", length(v), size = 4), dim(v)), v)
  if (B == 1) v[, , 1] else v
}
cas <- list(
  list(type = "uint8"), list(type = "int8", compression = "packbits"), list(type = "uint16", fin = "big"),
  list(type = "int16", compression = "lzw", predicteur = 2), list(type = "uint32", compression = "deflate", tuile = 16),
  list(type = "int32", compression = "lzw", fin = "big", rps = 5), list(type = "float32", compression = "lzw", predicteur = 3, tuile = 16),
  list(type = "float64", compression = "deflate", predicteur = 3, fin = "big"), list(type = "float32", big = TRUE, compression = "deflate", rps = 4),
  list(type = "int16", B = 3, compression = "lzw", predicteur = 2, rps = 7), list(type = "float32", B = 2, planaire = TRUE, compression = "lzw", predicteur = 3, tuile = 16),
  list(type = "uint16", B = 2, planaire = TRUE, big = TRUE, fin = "big", compression = "packbits", rps = 3),
  list(type = "float64", B = 2, compression = "deflate", predicteur = 3, rps = 6))
for (k in seq_along(cas)) {
  x <- cas[[k]]
  v <- valeurs_type(x$type, x$B %||% 1)
  chemin <- f(sprintf("ici_%d.tif", k))
  do.call(ecrire_tiff, c(list(chemin, v, gt = c(820000, 25, 0, 6270000, 0, -25)), x[setdiff(names(x), "B")]))
  g <- ouvrir_raster(chemin)
  nom <- sprintf("%s, %s%s%s%s, %s%s", x$type, x$compression %||% "sans compression", if ((x$predicteur %||% 1) > 1) paste0(", prédicteur ", x$predicteur) else "",
                 if (isTRUE(x$big)) ", BigTIFF" else "", if (identical(x$fin, "big")) ", gros-boutiste" else "",
                 if (!is.null(x$tuile)) "tuiles" else sprintf("bandes de %d lignes", x$rps %||% H), if ((x$B %||% 1) > 1) sprintf(", %d bandes %s", x$B, if (isTRUE(x$planaire)) "séparées" else "entrelacées") else "")
  verifier(paste("relu :", nom), identical(g$type, x$type) && g$bandes == (x$B %||% 1) && proche(toutes(g), comme_lu(v), 0))
}

# des valeurs manquantes, l'échelle, le décalage et le nom des bandes de GDAL
v <- valeurs_type("int16")
v[3, 4] <- -9999
meta <- '<GDALMetadata>\n  <Item name="OFFSET" sample="0" role="offset">100</Item>\n  <Item name="SCALE" sample="0" role="scale">0.5</Item>\n  <Item name="DESCRIPTION" sample="0" role="description">altitude &amp; pente</Item>\n</GDALMetadata>'
ecrire_tiff(f("meta.tif"), v, "int16", nodata = -9999, meta = meta, gt = c(0, 10, 0, 0, 0, -10))
g <- ouvrir_raster(f("meta.tif"))
attendu <- v * 0.5 + 100
attendu[3, 4] <- NA
verifier("GDAL_NODATA, échelle et décalage (valeur × 0,5 + 100), nom de la bande", proche(toutes(g), comme_lu(attendu), 1e-12) && identical(noms_bandes(g), "altitude & pente"))
ecrire_tiff(f("bande_generique.tif"), array(v, c(dim(v), 2)), "int16", meta = '<GDALMetadata><Item name="DESCRIPTION" sample="0" role="description">Band 1</Item><Item name="DESCRIPTION" sample="1" role="description">lyr.2</Item></GDALMetadata>')
verifier("des noms de bandes sans contenu (« Band 1 », « lyr.2 » de terra) : le nom du fichier, numéroté", identical(noms_bandes(ouvrir_raster(f("bande_generique.tif"))), c("bande_generique_1", "bande_generique_2")))
vf <- valeurs_type("float32")
vf[2, 2] <- -3.4028234663852886e+38
vf[5, 6] <- NaN
ecrire_tiff(f("nodata_f32.tif"), vf, "float32", nodata = "-3.4028234663852886e+38", gt = c(0, 10, 0, 0, 0, -10))
af <- vf; af[2, 2] <- NA; af[5, 6] <- NA
verifier("réels de 32 bits : la valeur manquante arrondie comme eux, NaN manquant", proche(toutes(ouvrir_raster(f("nodata_f32.tif"))), comme_lu(af), 0))
ecrire_tiff(f("nodata_nan.tif"), vf, "float64", nodata = "nan", gt = c(0, 10, 0, 0, 0, -10))
an <- vf; an[5, 6] <- NA
verifier("une valeur manquante « nan »", proche(toutes(ouvrir_raster(f("nodata_nan.tif"))), comme_lu(an), 0))

# géoréférencement : PixelIsPoint (décalé d'une demi-cellule, comme GDAL), ModelTransformation avec rotation
ecrire_tiff(f("point.tif"), valeurs_type("float32"), gt = c(1000, 2, 0, 5000, 0, -2), point = TRUE)
verifier("PixelIsPoint : le point d'appui au centre de la première cellule", proche(ouvrir_raster(f("point.tif"))$gt, c(999, 2, 0, 5001, 0, -2), 1e-12))
gt_tourne <- c(700000, 20 * cos(0.3), -20 * sin(0.3), 6600000, -20 * sin(0.3), -20 * cos(0.3))
vt <- valeurs_type("float64")
ecrire_tiff(f("tourne.tif"), vt, "float64", gt = gt_tourne, transformation = TRUE)
gtr <- ouvrir_raster(f("tourne.tif"))
cc <- centre_cellule(gtr, c(0, 5, 22), c(0, 7, 30))
verifier("ModelTransformation avec une rotation : chaque centre de cellule donne sa valeur", proche(gtr$gt, gt_tourne, 1e-9) &&
           proche(valeurs_points(gtr, cc$x, cc$y, "cellule")$valeurs, vt[cbind(c(1, 6, 23), c(1, 8, 31))], 0))

# des points : la cellule, l'interpolation bilinéaire, un cercle
m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), 4, byrow = TRUE)
m[2, 2] <- NA
ecrire_tiff(f("petit.tif"), m, "float64", gt = c(0, 10, 0, 40, 0, -10), nodata = "nan")
gp <- ouvrir_raster(f("petit.tif"))
px <- c(5, 12, 49.9, 50, -1, 2, 15, 12, 18, 25, 25)
py <- c(35, 33, 20, 20, 20, 38, 25, 28, 22, 40, 0)
verifier("la cellule du point, ses bords compris ; dehors et sur une valeur manquante : NA",
         proche(valeurs_points(gp, px, py, "cellule")$valeurs, c(1, 2, 15, 15, NA, 1, NA, NA, NA, 3, 18), 0))
# (à la main : au centre, la valeur ; entre deux centres, la moyenne pondérée ; au bord, celle du bord ;
# une cellule manquante remplacée par sa voisine sur la ligne, puis sur la colonne, comme terra)
verifier("bilinéaire : au centre, entre des centres, au bord, à côté d'une valeur manquante (comme terra 1.9)",
         proche(valeurs_points(gp, px, py, "bilineaire")$valeurs, c(1, 2.56, 12.5, 12.5, NA, 1, 6, 4.71, 9.29, 3, 18), 1e-12))
cer <- valeurs_points(gp, c(25, 21), c(20, 31), "moyenne", 15)
# (centres à moins de 15 m de (25, 20) : (15, 25), (25, 25), (35, 25), (15, 15), (25, 15), (35, 15) et (25, 5)... à la main)
ce <- expand.grid(j = 0:4, i = 0:3)
xy <- centre_cellule(gp, ce$i, ce$j)
dedans1 <- sqrt((xy$x - 25)^2 + (xy$y - 20)^2) <= 15
verifier("un cercle : les cellules dont le centre est à moins du rayon, leur moyenne sans les manquantes",
         proche(resume_cercles(cer$valeurs, cer$propre[, 1], "moyenne", 1)[1], mean(m[cbind(ce$i + 1, ce$j + 1)][dedans1], na.rm = TRUE), 1e-12) &&
           nrow(cer$valeurs[[1]]) == sum(dedans1))
cl <- matrix(c(4, 4, 4, 1, 3, 4, 4, 4, 4), 3, byrow = TRUE)
ecrire_tiff(f("classes.tif"), cl, "uint8", gt = c(0, 10, 0, 30, 0, -10), palette = TRUE)
gc <- ouvrir_raster(f("classes.tif"))
mj <- valeurs_points(gc, c(10, 9.99), c(15, 15), "majorite", 5.1)
mj3 <- valeurs_points(gc, 15, 15, "majorite", 10.5)
# (le cercle de (10, 15) : les cellules (5, 15) et (15, 15), 1 et 3 à égalité, le point dans celle qui vaut 3 ;
# de (9,99, 15) : les mêmes, le point dans celle qui vaut 1 ; de (15, 15) dans 10,5 m : 3, 1, 4, 4, 4 → 4)
verifier("la classe majoritaire ; à égalité, celle de la cellule du point", proche(resume_cercles(mj$valeurs, mj$propre[, 1], "majorite", 1)[1:2], c(3, 1), 0) &&
           proche(resume_cercles(mj3$valeurs, mj3$propre[, 1], "majorite", 1), 4, 0))
verifier("une palette : la méthode proposée est la cellule, les numéros des classes lus", isTRUE(gc$palette) && identical(methode_proposee(gc), "cellule") && proche(toutes(gc), comme_lu(cl), 0))
verifier("des réels : l'interpolation bilinéaire proposée", identical(methode_proposee(gp), "bilineaire"))

# décompression : LZW de plus de 4094 entrées (le dictionnaire vidé en cours), un texte constant, PackBits
gros <- as.raw(sample(0:255, 30000, TRUE))
verifier("LZW : 30 000 octets au hasard (le dictionnaire vidé plusieurs fois)", identical(decompresser_lzw(coder_lzw(gros)), gros))
constant <- as.raw(rep(7, 20000))
verifier("LZW : 20 000 octets égaux (des chaînes longues)", identical(decompresser_lzw(coder_lzw(constant)), constant))
melange <- as.raw(c(rep(0, 300), sample(0:3, 5000, TRUE), rep(255, 129), 1:10))
verifier("PackBits : suites et littéraux", identical(decompresser_packbits(coder_packbits(melange)), melange))
verifier("Deflate (memDecompress) : le zlib de memCompress", identical(decompresser_bloc(memCompress(melange, "gzip"), 8, length(melange), "x"), melange))

# ---------------------------------------------------------------- grilles ASCII d'ESRI

cat("Grilles ASCII d'ESRI\n")
va <- round(valeurs_type("float64"), 2)
va[4, 5] <- -9999
ecrire_asc <- function(chemin, v, entete, par_ligne = TRUE) {
  lignes <- if (par_ligne) apply(v, 1, function(r) paste(format(r, nsmall = 2, trim = TRUE), collapse = " ")) else
    vapply(split(format(as.vector(t(v)), nsmall = 2, trim = TRUE), ceiling(seq_along(v) / 10)), paste, "", collapse = " ")
  writeLines(c(entete, lignes), chemin)
}
ecrire_asc(f("mnt.asc"), va, c(sprintf("ncols %d", W), sprintf("nrows %d", H), "xllcorner 830000", "yllcorner 6250000", "cellsize 25", "NODATA_value -9999"))
writeLines('PROJCS["RGF_1993_Lambert_93",GEOGCS["GCS_RGF_1993",DATUM["D_RGF_1993",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic"],PARAMETER["False_Easting",700000.0],PARAMETER["False_Northing",6600000.0],PARAMETER["Central_Meridian",3.0],PARAMETER["Standard_Parallel_1",49.0],PARAMETER["Standard_Parallel_2",44.0],PARAMETER["Latitude_Of_Origin",46.5],UNIT["Meter",1.0]]',
           f("mnt.prj"))
ga <- ouvrir_raster(f("mnt.asc"))
aa <- va; aa[4, 5] <- NA
verifier("relue : ses valeurs, NODATA_value manquant", proche(toutes(ga), comme_lu(aa), 1e-12))
verifier("son .prj d'ESRI (sans code) : Lambert-93 d'après ses paramètres", identical(ga$systeme$code, 2154L))
verifier("sa transformation : xllcorner, yllcorner et cellsize", proche(ga$gt, c(830000, 25, 0, 6250000 + H * 25, 0, -25), 1e-9))
ecrire_asc(f("centre.asc"), va, c(sprintf("NCOLS %d", W), sprintf("NROWS %d", H), "XLLCENTER 830012.5", "YLLCENTER 6250012.5", "CELLSIZE 25", "NODATA_VALUE -9999"), par_ligne = FALSE)
gb <- ouvrir_raster(f("centre.asc"))
verifier("xllcenter, des majuscules, des lignes coupées autrement que la grille, sans .prj", proche(gb$gt, ga$gt, 1e-9) && proche(toutes(gb), comme_lu(aa), 1e-12) && is.null(gb$systeme))
verifier("une grille d'entiers : la cellule proposée", {
  writeLines(c("ncols 2", "nrows 2", "xllcorner 0", "yllcorner 0", "cellsize 1", "1 3", "2 4"), f("occupation.asc"))
  identical(methode_proposee(ouvrir_raster(f("occupation.asc"))), "cellule")
})

# ---------------------------------------------------------------- systèmes de coordonnées

cat("Systèmes de coordonnées\n")
sys_cles <- function(cles) { ecrire_tiff(f("cles.tif"), matrix(1, 2, 2), cles = cles); ouvrir_raster(f("cles.tif"))$systeme }
verifier("GeoKeys : Lambert-93, WGS 84, UTM 31N, Lambert II étendu, LAEA Europe, la Mercator du web et son ancien code 900913",
         identical(vapply(list(list(`1024` = 1, `3072` = 2154), list(`1024` = 2, `2048` = 4326), list(`1024` = 1, `3072` = 32631), list(`1024` = 1, `3072` = 27572),
                               list(`1024` = 1, `3072` = 3035), list(`1024` = 1, `3072` = 3857), list(`1024` = 1, `3072` = 900913)), function(k) as.integer(sys_cles(k)$code), 0L),
                   c(2154L, 4326L, 32631L, 27572L, 3035L, 3857L, 3857L)))
lcc_utilisateur <- list(`1024` = 1, `2048` = 4171, `3072` = 32767, `3073` = "RGF93 v1 / Lambert-93 (défini)", `3075` = 8, `3078` = 49.0000000001, `3079` = 44.5 - 0.5,
                        `3084` = 3.0, `3085` = 46.5, `3086` = 700000.5 - 0.5, `3087` = 6600000.5 - 0.5)
verifier("GeoKeys d'un système défini par ses paramètres : reconnu (Lambert-93)", identical(sys_cles(lcc_utilisateur)$code, 2154L))
tm_utilisateur <- list(`1024` = 1, `2048` = 4326, `3072` = 32767, `3075` = 1, `3080` = 15.0000000001, `3081` = 0, `3082` = 500000.5 - 0.5, `3083` = 10000000.5 - 0.5, `3092` = 0.9996)
verifier("… une zone UTM du sud (33S) d'après ses paramètres", identical(sys_cles(tm_utilisateur)$code, 32733L))
inconnu <- sys_cles(list(`1024` = 1, `3072` = 27561))
verifier("un code inconnu (NTF Lambert Nord, EPSG:27561) : refusé, nommé", grepl("EPSG:27561", inconnu$erreur) && grepl("n'est pas de ceux que Nuée convertit", inconnu$erreur))
verifier("… et en anglais", grepl("isn't one Nuée converts", en_anglais(stop(sys_cles(list(`1024` = 1, `3072` = 2056))$erreur))))
verifier("sans GeoKeys : pas de système (pris dans celui du projet)", is.null(sys_cles(list())))
wkt <- function(texte) systeme_wkt(lire_wkt(texte))$code
verifier("WKT d'ESRI : UTM 31N, WGS 84, LAEA Europe, la Mercator du web",
         identical(c(wkt('PROJCS["WGS_1984_UTM_Zone_31N",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Transverse_Mercator"],PARAMETER["False_Easting",500000.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",3.0],PARAMETER["Scale_Factor",0.9996],PARAMETER["Latitude_Of_Origin",0.0],UNIT["Meter",1.0]]'),
                     wkt('GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'),
                     wkt('PROJCS["ETRS_1989_LAEA",GEOGCS["GCS_ETRS_1989",DATUM["D_ETRS_1989",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["False_Easting",4321000.0],PARAMETER["False_Northing",3210000.0],PARAMETER["Central_Meridian",10.0],PARAMETER["Latitude_Of_Origin",52.0],UNIT["Meter",1.0]]'),
                     wkt('PROJCS["WGS_1984_Web_Mercator_Auxiliary_Sphere",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Mercator_Auxiliary_Sphere"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",0.0],PARAMETER["Standard_Parallel_1",0.0],PARAMETER["Auxiliary_Sphere_Type",0.0],UNIT["Meter",1.0]]')),
                   c(32631L, 4326L, 3035L, 3857L)))
verifier("WKT de l'OGC : le code EPSG de la racine, pas celui de l'unité",
         identical(wkt('PROJCS["RGF93 v1 / CC46",GEOGCS["RGF93 v1",DATUM["Reseau_Geodesique_Francais_1993_v1",SPHEROID["GRS 1980",6378137,298.257222101,AUTHORITY["EPSG","7019"]],AUTHORITY["EPSG","6171"]],AUTHORITY["EPSG","4171"]],PROJECTION["Lambert_Conformal_Conic_2SP"],PARAMETER["latitude_of_origin",46],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AUTHORITY["EPSG","3946"]]'), 3946L) &&
           identical(wkt('PROJCRS["WGS 84 / UTM zone 32N",BASEGEOGCRS["WGS 84",DATUM["World Geodetic System 1984",ELLIPSOID["WGS 84",6378137,298.257223563]]],CONVERSION["UTM zone 32N",METHOD["Transverse Mercator"]],ID["EPSG",32632]]'), 32632L))
verifier("un .prj d'un autre système (Suisse, CH1903+) : refusé, nommé", grepl("CH1903", systeme_wkt(lire_wkt('PROJCS["CH1903+_LV95",GEOGCS["GCS_CH1903+",DATUM["D_CH1903+",SPHEROID["Bessel_1841",6377397.155,299.1528128]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Hotine_Oblique_Mercator_Azimuth_Center"],UNIT["Meter",1.0]]'))$erreur))

# ---------------------------------------------------------------- refus

cat("Refus\n")
writeLines("pas un raster", f("texte.tif"))
verifier("un fichier qui n'est pas un TIFF", grepl("n'est pas un fichier TIFF", refus(ouvrir_raster(f("texte.tif")))))
verifier("un autre format (.png)", { writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), f("image.png")); grepl("GeoTIFF \\(.tif\\) et les grilles ASCII", refus(ouvrir_raster(f("image.png")))) })
verifier("un fichier introuvable", grepl("introuvable", refus(ouvrir_raster(f("absent.tif")))))
ecrire_tiff(f("jpeg.tif"), matrix(1:4, 2), "uint8", compression = 7)
verifier("la compression JPEG : refusée, nommée, avec ce qu'il faut faire", grepl("la compression JPEG ne se lit pas ici", refus(ouvrir_raster(f("jpeg.tif")))) &&
           grepl("JPEG compression isn't read here.*gdal_translate", en_anglais(ouvrir_raster(f("jpeg.tif")))))
ecrire_tiff(f("webp.tif"), matrix(1:4, 2), "uint8", compression = 50001)
verifier("WebP refusé", grepl("WebP", refus(ouvrir_raster(f("webp.tif")))))
ecrire_tiff(f("bits4.tif"), matrix(1:4, 2), "uint8", bits = 4)
verifier("des valeurs de 4 bits refusées", grepl("4 bits", refus(ouvrir_raster(f("bits4.tif")))))
ecrire_tiff(f("demi.tif"), matrix(1:4, 2), "uint16", format = 3)
verifier("des demi-réels refusés", grepl("demi-réels", refus(ouvrir_raster(f("demi.tif")))))
ecrire_tiff(f("complexe.tif"), matrix(1:4, 2), "float64", format = 6)
verifier("des nombres complexes refusés", grepl("complexes", refus(ouvrir_raster(f("complexe.tif")))))
ecrire_tiff(f("gcp.tif"), matrix(1:4, 2), "uint8", points_appui = c(0, 0, 0, 10, 20, 0, 1, 1, 0, 11, 19, 0))
verifier("des points d'appui (GCP) refusés", grepl("points d'appui", refus(ouvrir_raster(f("gcp.tif")))))
ecrire_tiff(f("sans_geo.tif"), matrix(1:4, 2), "uint8", sans_geo = TRUE, cles = list())
verifier("sans géoréférencement : refusé", grepl("n'est pas géoréférencé", refus(ouvrir_raster(f("sans_geo.tif")))))
writeLines(c("1", "0", "0", "-1", "0.5", "1.5"), f("sans_geo.tfw"))
verifier("… lu avec son fichier .tfw (le centre de la première cellule)", proche(ouvrir_raster(f("sans_geo.tif"))$gt, c(0, 1, 0, 2, 0, -1), 1e-12))
tronque <- readBin(f("ici_7.tif"), "raw", file.size(f("ici_7.tif")))
writeBin(tronque[1:200], f("tronque.tif"))
verifier("un fichier tronqué", nzchar(refus(ouvrir_raster(f("tronque.tif")))))

# ---------------------------------------------------------------- dans le projet

cat("Dans le projet\n")
set.seed(5)
n <- 40
lon <- 4.52 + stats::runif(n, 0, 0.12)
lat <- 43.45 + stats::runif(n, 0, 0.08)
l93 <- depuis_lonlat(lon, lat, 2154)
df <- data.frame(nom = sprintf("p%02d", 1:n), X = l93$x, Y = l93$y, v = stats::rnorm(n), stringsAsFactors = FALSE)
df$X[7] <- NA
df$X[8] <- 700000  # (hors des rasters)
p <- projet_depuis_tableau(df, "placettes")
p <- regler_coordonnees(p, numeros(p, "X"), numeros(p, "Y"), 2154)
# un MNT en Lambert-93 autour des placettes, en deux tuiles (côte à côte), et un raster en WGS 84
x0 <- floor(min(l93$x) / 1000) * 1000 - 1000
y0 <- ceiling(max(l93$y) / 1000) * 1000 + 1000
nx <- ceiling((max(l93$x) - x0) / 25) + 40
ny <- ceiling((y0 - min(l93$y)) / 25) + 40
alt <- function(i, j) 5 + 0.01 * i + 2 * sin(j / 30)
mnt <- outer(seq_len(ny) - 1, seq_len(nx) - 1, alt)
moitie <- floor(nx / 2)
ecrire_tiff(f("mnt_ouest.tif"), mnt[, seq_len(moitie)], "float32", compression = "lzw", predicteur = 3, tuile = 32, gt = c(x0, 25, 0, y0, 0, -25), nodata = -9999)
ecrire_tiff(f("mnt_est.tif"), mnt[, -seq_len(moitie)], "float32", compression = "deflate", gt = c(x0 + moitie * 25, 25, 0, y0, 0, -25), nodata = -9999)
geo <- outer(seq_len(60) - 1, seq_len(80) - 1, function(i, j) (i * 7 + j) %% 5)
ecrire_tiff(f("habitat.tif"), geo, "uint8", gt = c(4.5, 0.002, 0, 43.55, 0, -0.002), cles = list(`1024` = 2, `2048` = 4326), palette = TRUE)
infos <- infos_rasters(p, c(f("mnt_ouest.tif"), f("habitat.tif"), f("jpeg.tif")))
verifier("ce que montre le dialogue : système, cellules, méthode, noms, observations dedans",
         identical(infos[[1]]$systeme$code, 2154L) && identical(infos[[1]]$resolution, "25 m") && identical(infos[[1]]$methode, "bilineaire") &&
           identical(infos[[2]]$systeme$code, 4326L) && identical(infos[[2]]$methode, "cellule") && grepl("″|°", infos[[2]]$resolution) &&
           identical(as.character(infos[[1]]$noms), "mnt_ouest") && infos[[2]]$dedans == 38 && infos[[1]]$observations == 39 && infos[[1]]$total == 40)
verifier("… un fichier refusé dit pourquoi, sans empêcher les autres", grepl("JPEG", infos[[3]]$erreur) && is.null(infos[[1]]$erreur))
q <- ajouter_valeurs_rasters(p, list(list(chemin = f("mnt_ouest.tif"), methode = "bilineaire", noms = "altitude"),
                                     list(chemin = f("mnt_est.tif"), methode = "bilineaire", noms = "altitude"),
                                     list(chemin = f("habitat.tif"), methode = "cellule", noms = "habitat"),
                                     list(chemin = f("habitat.tif"), methode = "majorite", rayon = 300, noms = "habitat_300m")))
r <- attr(q, "raster")
valeurs <- function(pr, nom) colonne(pr, numeros(pr, nom), numeros_observations(pr))
# (l'altitude à la main : la position dans le MNT entier, interpolée entre les centres des cellules)
u <- (l93$x - x0) / 25 - 0.5
w <- (y0 - l93$y) / 25 - 0.5
alt_main <- alt(w, u)  # (le MNT est linéaire en i, et la courbe en j presque droite à l'échelle d'une cellule)
a <- valeurs(q, "altitude")
loin <- setdiff(which(abs(u - (moitie - 0.5)) > 1), c(7, 8))  # (à moins d'une cellule de la jointure, chaque tuile s'arrête à son bord)
verifier("deux tuiles sous un même nom : une variable, chaque placette de la tuile qui la contient", sum(!is.na(a)) == 38 && proche(a[loin], alt_main[loin], 2e-3))
verifier("… les comptes : 1 sans coordonnées, 1 hors des rasters", r$variables[[1]]$sans_coordonnees == 1 && r$variables[[1]]$hors == 1 && r$variables[[1]]$valeurs == 38 &&
           grepl("38 observation\\(s\\) sur 40 ont une valeur, manquante pour 1 hors du raster, 1 sans coordonnées", r$phrases[1]))
hb <- valeurs(q, "habitat")
gh <- ouvrir_raster(f("habitat.tif"))
verifier("le raster en WGS 84 : les placettes converties, la cellule de chacune", proche(hb[-c(7, 8)], valeurs_points(gh, lon, lat, "cellule")$valeurs[-c(7, 8), 1], 0))
verifier("les variables gardent leur origine (fichiers, méthode, rayon, système)", {
  o <- q$variables[[which(vapply(q$variables, function(v) identical(v$ids[[1]], "habitat_300m"), TRUE))]]$origine
  identical(o$methode, "majorite") && o$rayon == 300 && identical(o$code, 4326L) && identical(as.character(o$fichiers), "habitat.tif")
})
q2 <- ajouter_valeurs_rasters(q, list(list(chemin = f("mnt_ouest.tif"), methode = "cellule", noms = "altitude")))
verifier("un nom déjà pris : « altitude (2) »", "altitude (2)" %in% q2$noms_var && grepl("était déjà pris", attr(q2, "raster")$phrases[1]))
fichier <- f("projet.nuee")
enregistrer_projet(q, fichier, "essai")
relu <- lire_projet(fichier)
verifier("enregistré et relu : les valeurs (à 15 chiffres, comme toute variable) et leur origine", proche(valeurs(relu, "altitude"), a, 1e-12) &&
           identical(relu$variables[[which(vapply(relu$variables, function(v) identical(v$ids[[1]], "altitude"), TRUE))]]$origine$methode, "bilineaire"))
d <- definir_modele(relu, "LM", x = numeros(relu, c("altitude", "habitat_300m")), y = numeros(relu, "v"), observations = numeros_observations(relu), echelle = c(defaut = "none"))
mo <- ajuster(relu, d)
tf <- paste(texte_methodes(relu, d, mo, "fr")$paragraphes, collapse = " ")
te <- paste(texte_methodes(relu, d, mo, "en")$paragraphes, collapse = " ")
verifier("le texte des Méthodes dit d'où viennent les variables, en français",
         grepl("Lues dans des rasters à la position de chaque observation : « altitude » de mnt_ouest.tif et mnt_est.tif (GeoTIFF, cellules de 25 m, RGF93 v1 / Lambert-93 ; par interpolation bilinéaire", tf, fixed = TRUE) &&
           grepl("la classe majoritaire des cellules à moins de 300 m", tf, fixed = TRUE))
verifier("… et en anglais", grepl("Taken from rasters at each observation's position: “altitude” from mnt_ouest.tif and mnt_est.tif (GeoTIFF, 25 m cells, RGF93 v1 / Lambert-93; by bilinear interpolation", te, fixed = TRUE) &&
           grepl("cells, WGS 84 (longitude, latitude); the majority class of the cells within 300 m", te, fixed = TRUE))
dx <- definir_modele(relu, "LM", x = numeros(relu, "X"), y = numeros(relu, "v"), observations = numeros_observations(relu), echelle = c(defaut = "none"))
verifier("un modèle sans ces variables n'en dit rien", !grepl("raster", paste(texte_methodes(relu, dx, ajuster(relu, dx), "fr")$paragraphes, collapse = " ")))
# un cercle à cheval sur les deux tuiles : les cellules des deux
k <- which.min(abs(l93$x - (x0 + moitie * 25)))
qc <- ajouter_valeurs_rasters(p, list(list(chemin = f("mnt_ouest.tif"), methode = "moyenne", rayon = 200, noms = "alt200"),
                                      list(chemin = f("mnt_est.tif"), methode = "moyenne", rayon = 200, noms = "alt200")))
ce <- expand.grid(j = seq_len(nx) - 1, i = seq_len(ny) - 1)
cx <- x0 + (ce$j + 0.5) * 25
cy <- y0 - (ce$i + 0.5) * 25
dk <- sqrt((cx - l93$x[k])^2 + (cy - l93$y[k])^2) <= 200
verifier("un cercle à cheval sur deux tuiles : la moyenne des cellules des deux", sum(dk & ce$j < moitie) > 0 && sum(dk & ce$j >= moitie) > 0 &&
           proche(valeurs(qc, "alt200")[k], mean(mnt[cbind(ce$i + 1, ce$j + 1)][dk]), 1e-4))
verifier("refus : sans coordonnées", grepl("Graphiques › Carte", refus(ajouter_valeurs_rasters(regler_coordonnees(p, NULL, NULL, NULL), list(list(chemin = f("mnt_est.tif")))))))
ecrire_tiff(f("nord.tif"), matrix(1, 2, 2), cles = list(`1024` = 1, `3072` = 27561))
verifier("refus : un système inconnu, nommé", grepl("nord.tif : le système de coordonnées du fichier, EPSG:27561", refus(ajouter_valeurs_rasters(p, list(list(chemin = f("nord.tif"))))), fixed = TRUE))
verifier("refus : un cercle sans rayon, des méthodes différentes sous un nom, des noms manquants",
         grepl("rayon", refus(ajouter_valeurs_rasters(p, list(list(chemin = f("mnt_est.tif"), methode = "moyenne"))))) &&
           grepl("même méthode", refus(ajouter_valeurs_rasters(p, list(list(chemin = f("mnt_est.tif"), methode = "cellule", noms = "a"), list(chemin = f("mnt_ouest.tif"), methode = "bilineaire", noms = "a"))))) &&
           grepl("donnez un nom", refus(ajouter_valeurs_rasters(p, list(list(chemin = f("mnt_est.tif"), noms = " "))))))
verifier("refus en anglais", grepl("give the circle's radius", en_anglais(ajouter_valeurs_rasters(p, list(list(chemin = f("mnt_est.tif"), methode = "moyenne"))))))
# un projet en coordonnées planes sans système : seulement un raster sans système
pp <- regler_coordonnees(p, numeros(p, "X"), numeros(p, "Y"), "plan")
verifier("coordonnées planes sans système : un raster sans système est pris dans le leur, un autre refusé",
         sum(!is.na(valeurs(ajouter_valeurs_rasters(pp, list(list(chemin = f("centre.asc"), noms = "z"))), "z"))) >= 0 &&
           grepl("planes, sans système", refus(ajouter_valeurs_rasters(pp, list(list(chemin = f("mnt_est.tif")))))))

# ---------------------------------------------------------------- contre GDAL et terra

if (nzchar(Sys.getenv("NUEE_TERRA"))) .libPaths(c(Sys.getenv("NUEE_TERRA"), .libPaths()))
if (!requireNamespace("terra", quietly = TRUE)) {
  cat("(terra absent : la comparaison avec GDAL et terra::extract n'est pas faite)\n")
} else {
  cat(sprintf("Contre GDAL %s et terra %s\n", terra::gdal(), utils::packageVersion("terra")))
  # (à côté d'une cellule sans valeur, l'interpolation bilinéaire suit la règle de terra 1.9 : avec un
  # terra plus ancien, seulement les points loin de toute cellule sans valeur sont comparés)
  terra_19 <- utils::packageVersion("terra") >= "1.9"
  loin_des_manques <- function(g, x, y) {
    pg <- position_grille(g, x, y)
    i <- floor(pg$v); j <- floor(pg$u)
    vapply(seq_along(x), function(k) {
      if (!is.finite(i[k]) || !is.finite(j[k])) return(TRUE)
      ii <- max(0, i[k] - 1):min(g$hauteur - 1, i[k] + 1)
      jj <- max(0, j[k] - 1):min(g$largeur - 1, j[k] + 1)
      if (!length(ii) || !length(jj) || ii[1] > ii[length(ii)] || jj[1] > jj[length(jj)]) return(TRUE)
      ce <- expand.grid(i = ii, j = jj)
      !anyNA(valeurs_cellules(g, ce$i, ce$j))
    }, TRUE)
  }
  comparables <- function(g, x, y) if (terra_19) rep(TRUE, length(x)) else loin_des_manques(g, x, y)
  # (une option que ce GDAL n'écrit pas, LZMA ou ZSTD sans leur bibliothèque : la comparaison est sautée)
  ecrire <- function(r, fichier, ...) tryCatch({ terra::writeRaster(r, fichier, overwrite = TRUE, ...); TRUE }, error = function(e) {
    cat(sprintf("  (GDAL n'écrit pas %s : %s)\n", basename(fichier), conditionMessage(e)))
    FALSE
  })
  # des rasters écrits par GDAL : toutes leurs cellules, comme terra les lit
  set.seed(7)
  Hg <- 61; Wg <- 83
  fabriquer <- function(type, bandes = 1, crs = "EPSG:2154", etendue = c(800000, 800000 + Wg * 25, 6250000, 6250000 + Hg * 25), lignes = Hg, colonnes = Wg) {
    r <- terra::rast(nrows = lignes, ncols = colonnes, nlyrs = bandes, xmin = etendue[1], xmax = etendue[2], ymin = etendue[3], ymax = etendue[4], crs = crs)
    m <- vapply(seq_len(bandes), function(b) {
      v <- as.vector(t(outer(seq_len(lignes), seq_len(colonnes), function(i, j) 100 + 50 * sin(i / 10 + b) + 30 * cos(j / 7) + i * 0.3)))
      v <- switch(type, INT1U = round(v) %% 250, INT1S = round(v) %% 120 - 60, INT2S = round(v * 10) - 1500, INT2U = round(v * 100),
                  INT4S = round(v * 1e6) - 2e8, INT4U = round(v * 1e7), v)
      v[sample(length(v), 40)] <- NA
      v
    }, numeric(lignes * colonnes))
    terra::values(r) <- m
    r
  }
  manquante <- c(INT1U = 255, INT1S = -128, INT2U = 65535, INT2S = -9999, INT4U = 4294967295, INT4S = -9999, FLT4S = -3.4e38, FLT8S = -9999)
  ecart_gdal <- function(fichier) {
    g <- ouvrir_raster(fichier)
    ref <- terra::values(terra::rast(fichier), mat = TRUE)
    lu <- matrix(valeurs_cellules(g, rep(seq_len(g$hauteur) - 1, each = g$largeur), rep(seq_len(g$largeur) - 1, g$hauteur)), ncol = g$bandes)
    if (!identical(dim(lu), dim(ref)) || any(is.na(lu) != is.na(ref))) return(Inf)
    if (all(is.na(lu))) 0 else max(abs(lu - ref), na.rm = TRUE)
  }
  options_gdal <- list(c("COMPRESS=NONE"), c("COMPRESS=LZW"), c("COMPRESS=DEFLATE", "TILED=YES", "BLOCKXSIZE=32", "BLOCKYSIZE=32"), c("COMPRESS=PACKBITS"),
                       c("COMPRESS=LZW", "PREDICTOR=2"), c("COMPRESS=DEFLATE", "PREDICTOR=2", "ENDIANNESS=BIG"))
  types <- c("INT1U", "INT1S", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S")
  pires <- c()
  for (type in types) for (o in c(options_gdal, if (grepl("FLT", type)) list(c("COMPRESS=LZW", "PREDICTOR=3"), c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES", "ENDIANNESS=BIG")))) {
    fichier <- f(sprintf("gdal_%s_%s.tif", type, gsub("[^A-Za-z0-9]", "", paste(o, collapse = "_"))))
    terra::writeRaster(fabriquer(type), fichier, datatype = type, gdal = o, NAflag = manquante[[type]], overwrite = TRUE)
    pires[basename(fichier)] <- ecart_gdal(fichier)
  }
  verifier(sprintf("écrits par GDAL : %d fichiers (8 types × compressions, prédicteurs, tuiles, ordre des octets), toutes les cellules et les valeurs manquantes comme terra (le pire écart : %.3g)",
                   length(pires), max(pires)), all(pires == 0))
  autres <- list(list("FLT4S", c("COMPRESS=LZW", "PREDICTOR=3", "TILED=YES"), 3), list("FLT4S", c("COMPRESS=LZW", "PREDICTOR=3", "INTERLEAVE=BAND"), 3),
                 list("INT2S", c("COMPRESS=LZW", "PREDICTOR=2", "INTERLEAVE=PIXEL"), 3), list("INT2S", c("COMPRESS=DEFLATE", "PREDICTOR=2", "INTERLEAVE=BAND", "TILED=YES"), 2),
                 list("FLT4S", c("COMPRESS=DEFLATE", "BIGTIFF=YES"), 1), list("FLT8S", c("COMPRESS=LZW", "BIGTIFF=YES", "TILED=YES", "PREDICTOR=3"), 1),
                 list("FLT8S", c("COMPRESS=LZW", "PREDICTOR=2"), 2), list("FLT4S", c("COMPRESS=LZMA"), 1),
                 list("FLT4S", c("COMPRESS=ZSTD", "PREDICTOR=3"), 1), list("INT2U", c("COMPRESS=NONE", "TILED=YES", "SPARSE_OK=TRUE"), 1))
  for (x in autres) {
    fichier <- f(sprintf("gdal_%s_%s_%d.tif", x[[1]], gsub("[^A-Za-z0-9]", "", paste(x[[2]], collapse = "_")), x[[3]]))
    r <- fabriquer(x[[1]], x[[3]])
    if (any(grepl("SPARSE", x[[2]]))) r[1:40, 1:50] <- NA  # (des tuiles entières sans valeur : absentes du fichier)
    if (!ecrire(r, fichier, datatype = x[[1]], gdal = x[[2]], NAflag = manquante[[x[[1]]]])) next
    if (any(grepl("ZSTD", x[[2]])) && !zstd_lisible()) {
      verifier(sprintf("%s : refusé, ce R ne lit pas ZSTD", basename(fichier)), grepl("ZSTD", refus(ouvrir_raster(fichier))))
      next
    }
    verifier(sprintf("écrit par GDAL avec %s, %d bande(s) : comme terra", paste(x[[2]], collapse = " "), x[[3]]), ecart_gdal(fichier) == 0)
  }
  # un COG (tuiles et aperçus), l'échelle et le décalage, le nom des bandes, une palette, JPEG
  r <- fabriquer("FLT4S", etendue = c(800000, 800000 + 1300 * 25, 6250000, 6250000 + 1100 * 25), lignes = 1100, colonnes = 1300)
  if (ecrire(r, f("cog.tif"), filetype = "COG")) {
    verifier("un COG : les valeurs de la pleine résolution, ses aperçus comptés et laissés de côté", ecart_gdal(f("cog.tif")) == 0 && ouvrir_raster(f("cog.tif"))$images > 1)
  }
  rb <- fabriquer("FLT4S", 2)
  names(rb) <- c("bio1", "bio12")
  terra::writeRaster(rb, f("noms.tif"), overwrite = TRUE)
  verifier("le nom des bandes (les descriptions de GDAL)", identical(noms_bandes(ouvrir_raster(f("noms.tif"))), c("bio1", "bio12")))
  rs <- fabriquer("INT2S")
  if (!inherits(tryCatch(terra::scoff(rs) <- cbind(0.25, 10), error = function(e) e), "error") && ecrire(rs, f("echelle.tif"), datatype = "INT2S")) {
    verifier("l'échelle et le décalage de GDAL appliqués comme terra", ecart_gdal(f("echelle.tif")) < 1e-9)
  }
  rp <- terra::rast(nrows = 30, ncols = 40, xmin = 0, xmax = 4000, ymin = 0, ymax = 3000, crs = "EPSG:3035")
  terra::values(rp) <- sample(1:5, terra::ncell(rp), TRUE)
  terra::coltab(rp) <- data.frame(value = 1:5, col = c("red", "green", "blue", "yellow", "grey"))
  terra::writeRaster(rp, f("palette.tif"), datatype = "INT1U", overwrite = TRUE)
  gpal <- ouvrir_raster(f("palette.tif"))
  verifier("une palette écrite par GDAL : les numéros des classes, en LAEA Europe", isTRUE(gpal$palette) && identical(gpal$systeme$code, 3035L) && ecart_gdal(f("palette.tif")) == 0)
  terra::writeRaster(fabriquer("INT1U"), f("gdal_jpeg.tif"), datatype = "INT1U", gdal = "COMPRESS=JPEG", overwrite = TRUE)
  verifier("JPEG écrit par GDAL : refusé", grepl("JPEG", refus(ouvrir_raster(f("gdal_jpeg.tif")))))
  terra::writeRaster(terra::project(fabriquer("FLT4S"), "EPSG:27561"), f("gdal_27561.tif"), overwrite = TRUE)
  verifier("un système que Nuée ne convertit pas, écrit par GDAL : refusé avec son code", grepl("27561", ouvrir_raster(f("gdal_27561.tif"))$systeme$erreur))
  # les GeoTIFF écrits ici, lus par GDAL : les mêmes valeurs et la même transformation
  ici <- c(sprintf("ici_%d.tif", seq_along(cas)), "point.tif", "meta.tif")
  lus <- vapply(ici, function(x) {
    gi <- ouvrir_raster(f(x))
    ri <- terra::rast(f(x))
    e <- terra::ext(ri)
    isTRUE(all.equal(unname(gi$gt), unname(c(e$xmin, terra::xres(ri), 0, e$ymax, 0, -terra::yres(ri))), tolerance = 1e-12)) && ecart_gdal(f(x)) < 1e-9
  }, TRUE)
  verifier(sprintf("les %d GeoTIFF écrits ici, lus par GDAL : les mêmes valeurs et la même transformation (PixelIsPoint compris)", length(ici)), all(lus))
  # (terra ne lit pas les valeurs d'un raster tourné : la transformation que donne gdalinfo)
  info <- suppressWarnings(terra::describe(f("tourne.tif")))
  k <- grep("^GeoTransform =", info)
  gt_gdal <- as.numeric(unlist(strsplit(trimws(info[k + 1:2]), ",\\s*")))
  verifier("ModelTransformation tournée : la transformation de gdalinfo", length(k) == 1 && proche(gt_gdal, gtr$gt, 1e-6))
  # des points au hasard en Lambert-93, WGS 84 et UTM 31N : la cellule et l'interpolation bilinéaire comme terra::extract
  systemes <- list(`2154` = c(800000, 800000 + Wg * 25, 6250000, 6250000 + Hg * 25), `4326` = c(4.4, 4.4 + Wg * 0.0025, 43.4, 43.4 + Hg * 0.0025),
                   `32631` = c(620000, 620000 + Wg * 30, 4810000, 4810000 + Hg * 30))
  for (code in names(systemes)) {
    et <- systemes[[code]]
    r <- fabriquer("FLT4S", 1, paste0("EPSG:", code), et)
    fichier <- f(sprintf("points_%s.tif", code))
    terra::writeRaster(r, fichier, gdal = c("COMPRESS=LZW", "PREDICTOR=3", "TILED=YES", "BLOCKXSIZE=16", "BLOCKYSIZE=16"), overwrite = TRUE)
    g <- ouvrir_raster(fichier)
    dx <- (et[2] - et[1]) * 0.05
    dy <- (et[4] - et[3]) * 0.05
    xs <- stats::runif(2000, et[1] - dx, et[2] + dx)
    ys <- stats::runif(2000, et[3] - dy, et[4] + dy)
    rr <- terra::rast(fichier)
    na <- which(is.na(terra::values(rr)))
    centres <- terra::xyFromCell(rr, na[1:20])
    xs <- c(xs, centres[, 1]); ys <- c(ys, centres[, 2])  # (sur des cellules sans valeur)
    e1 <- terra::extract(rr, cbind(xs, ys), method = "simple")[[1]]
    e2 <- terra::extract(rr, cbind(xs, ys), method = "bilinear")[[1]]
    m1 <- valeurs_points(g, xs, ys, "cellule")$valeurs[, 1]
    m2 <- valeurs_points(g, xs, ys, "bilineaire")$valeurs[, 1]
    verifier(sprintf("EPSG:%s : %d points (%d dehors, %d sur une valeur manquante), la cellule comme method = \"simple\"", code, length(xs), sum(is.na(e1)) - 20, 20),
             g$systeme$code == as.integer(code) && proche(m1, e1, 0))
    cp <- comparables(g, xs, ys)
    verifier(sprintf("EPSG:%s : l'interpolation bilinéaire comme method = \"bilinear\" (%d points, écart %.2g)", code, sum(cp), max(abs(m2 - e2)[cp], na.rm = TRUE)),
             proche(m2[cp], e2[cp], 1e-9))
    # des cercles : la moyenne et la classe majoritaire des cellules dont le centre y est (terra::buffer, 720 côtés)
    rayon <- if (code == "4326") 400 else 70
    k <- 1:150
    tampons <- terra::buffer(terra::vect(cbind(xs[k], ys[k]), crs = paste0("EPSG:", code)), width = rayon, quadsegs = 180)
    em <- terra::extract(rr, tampons, fun = mean, na.rm = TRUE)[[2]]
    vm <- valeurs_points(g, xs[k], ys[k], "moyenne", rayon)
    mm <- resume_cercles(vm$valeurs, vm$propre[, 1], "moyenne", 1)
    # (terra, quand aucun centre n'est dans le cercle d'un point hors du raster, prend les cellules qu'il touche : Nuée, NA)
    sans_centre <- vapply(vm$valeurs, is.null, TRUE)
    verifier(sprintf("EPSG:%s : la moyenne dans %s m comme terra::extract(buffer(), fun = mean) (%d cercles, %d sans centre de cellule, NA)", code, rayon,
                     length(k), sum(sans_centre)), proche(mm[!sans_centre], em[!sans_centre], 1e-9) && all(is.na(mm[sans_centre])) && all(!vm$dedans[sans_centre]))
    cls <- terra::rast(rr)
    terra::values(cls) <- sample(1:4, terra::ncell(cls), TRUE)
    fc <- f(sprintf("classes_%s.tif", code))
    terra::writeRaster(cls, fc, datatype = "INT1U", overwrite = TRUE)
    ex <- terra::extract(terra::rast(fc), tampons)
    propre <- terra::extract(terra::rast(fc), cbind(xs[k], ys[k]))[[1]]
    majo <- vapply(k, function(i) {
      v <- ex[ex$ID == i, 2]
      v <- v[!is.na(v)]
      if (!length(v)) return(NA_real_)
      t <- table(v)
      gagnantes <- as.numeric(names(t)[t == max(t)])
      if (!is.na(propre[i]) && propre[i] %in% gagnantes) propre[i] else min(gagnantes)
    }, 0)
    vj <- valeurs_points(ouvrir_raster(fc), xs[k], ys[k], "majorite", rayon)
    mj <- resume_cercles(vj$valeurs, vj$propre[, 1], "majorite", 1)
    sc <- vapply(vj$valeurs, is.null, TRUE)
    verifier(sprintf("EPSG:%s : la classe majoritaire dans %s m, des cellules de terra::extract(buffer())", code, rayon), proche(mj[!sc], majo[!sc], 0))
  }
  # le MNT du Luxembourg livré avec terra (elev.tif : entiers de 16 bits, LZW, WGS 84)
  fe <- system.file("ex/elev.tif", package = "terra")
  ge <- ouvrir_raster(fe)
  re <- terra::rast(fe)
  xe <- stats::runif(1000, terra::xmin(re) - 0.05, terra::xmax(re) + 0.05)
  ye <- stats::runif(1000, terra::ymin(re) - 0.05, terra::ymax(re) + 0.05)
  ce <- comparables(ge, xe, ye)
  verifier("le MNT du Luxembourg (elev.tif) : la cellule et l'interpolation bilinéaire comme terra, 1000 points",
           identical(ge$systeme$code, 4326L) && proche(valeurs_points(ge, xe, ye, "cellule")$valeurs, terra::extract(re, cbind(xe, ye))[[1]], 0) &&
             proche(valeurs_points(ge, xe, ye, "bilineaire")$valeurs[ce], terra::extract(re, cbind(xe, ye), method = "bilinear")[[1]][ce], 1e-9))
  tb <- terra::buffer(terra::vect(cbind(xe[1:100], ye[1:100]), crs = "EPSG:4326"), width = 2500, quadsegs = 180)
  vb <- valeurs_points(ge, xe[1:100], ye[1:100], "moyenne", 2500)
  sb <- vapply(vb$valeurs, is.null, TRUE)
  verifier("… la moyenne dans 2,5 km (des cercles géodésiques)", proche(resume_cercles(vb$valeurs, vb$propre[, 1], "moyenne", 1)[!sb], terra::extract(re, tb, fun = mean, na.rm = TRUE)[[2]][!sb], 1e-9))
  # le même MNT en grille ASCII d'ESRI, écrite par GDAL (AAIGrid), avec son .prj
  terra::writeRaster(terra::project(re, "EPSG:2154", res = 100), f("lux.asc"), filetype = "AAIGrid", NAflag = -9999, overwrite = TRUE)
  ga <- ouvrir_raster(f("lux.asc"))
  ra <- terra::rast(f("lux.asc"))
  xa <- stats::runif(500, terra::xmin(ra), terra::xmax(ra))
  ya <- stats::runif(500, terra::ymin(ra), terra::ymax(ra))
  ca <- comparables(ga, xa, ya)
  verifier("le MNT en grille ASCII de GDAL (AAIGrid) et son .prj : Lambert-93, la cellule et l'interpolation bilinéaire comme terra",
           identical(ga$systeme$code, 2154L) && ecart_gdal(f("lux.asc")) < 1e-6 && proche(valeurs_points(ga, xa, ya, "cellule")$valeurs, terra::extract(ra, cbind(xa, ya))[[1]], 1e-6) &&
             proche(valeurs_points(ga, xa, ya, "bilineaire")$valeurs[ca], terra::extract(ra, cbind(xa, ya), method = "bilinear")[[1]][ca], 1e-6))
  # un projet en Lambert-93, le raster en UTM 31N : les placettes converties ici, comme PROJ
  r31 <- fabriquer("FLT4S", 1, "EPSG:32631", c(625000, 625000 + Wg * 100, 4808000, 4808000 + Hg * 100))
  r31 <- terra::focal(r31, 3, mean, na.rm = TRUE)  # (une surface lisse : des écarts de coordonnées au micromètre n'y comptent pas)
  terra::writeRaster(r31, f("utm.tif"), gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3"), overwrite = TRUE)
  pu <- terra::spatSample(terra::ext(r31), 200, lonlat = FALSE)
  pl <- terra::project(terra::vect(pu, crs = "EPSG:32631"), "EPSG:2154")
  dfu <- data.frame(nom = sprintf("u%03d", 1:200), X = terra::geom(pl)[, "x"], Y = terra::geom(pl)[, "y"], stringsAsFactors = FALSE)
  pr <- projet_depuis_tableau(dfu, "utm")
  pr <- regler_coordonnees(pr, numeros(pr, "X"), numeros(pr, "Y"), 2154)
  qu <- ajouter_valeurs_rasters(pr, list(list(chemin = f("utm.tif"), methode = "bilineaire", noms = "z")))
  ref <- terra::extract(terra::rast(f("utm.tif")), pu, method = "bilinear")[[1]]
  verifier(sprintf("projet en Lambert-93, raster en UTM 31N : comme terra aux points convertis par PROJ (écart %.2g)", max(abs(valeurs(qu, "z") - ref), na.rm = TRUE)),
           proche(valeurs(qu, "z"), ref, 1e-5))
}

# ---------------------------------------------------------------- rapidité

cat("Rapidité\n")
grand <- outer(seq_len(600) - 1, seq_len(800) - 1, function(i, j) 100 + sin(i / 20) * cos(j / 30) * 50 + i * 0.01)
ecrire_tiff(f("grand.tif"), grand, "float32", compression = "deflate", predicteur = 3, rps = 1, gt = c(0, 10, 0, 6000, 0, -10))
gg <- ouvrir_raster(f("grand.tif"))
t0 <- Sys.time()
vg <- valeurs_points(gg, stats::runif(300, 0, 8000), stats::runif(300, 0, 6000), "bilineaire")$valeurs
duree <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
verifier(sprintf("300 points bilinéaires dans 600 bandes de lignes Deflate : %.2f s, seulement les blocs utiles", duree), all(is.finite(vg)))

cat(if (echecs) sprintf("%d échec(s)\n", echecs) else "Tout est bon.\n")
if (echecs) quit(status = 1)
