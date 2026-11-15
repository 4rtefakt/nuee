# Les valeurs de rasters aux observations (Édition › Ajouter des valeurs de rasters) : l'altitude,
# l'occupation du sol, le climat... lus dans des fichiers raster à la position de chaque
# observation (les coordonnées du projet, spatial.R) et ajoutés au projet comme de nouvelles
# variables, une par bande, annulables par Ctrl+Z comme une Nouvelle variable.
#
# Les fichiers sont lus ici, sans GDAL (ni sf ni terra : plus de 130 Mo chacun sous Windows) :
#   - GeoTIFF (.tif, .tiff), ce qu'écrivent QGIS, GDAL et les portails de données (BD ALTI et RGE
#     ALTI de l'IGN, Copernicus, CORINE, WorldClim, CHELSA...) : TIFF classique et BigTIFF, petit-
#     et gros-boutiste, en bandes de lignes ou en tuiles, les bandes entrelacées ou séparées ;
#     sans compression, LZW (décodé ici), Deflate, LZMA et ZSTD (memDecompress de R, ZSTD quand
#     R sait le lire), PackBits, avec les prédicteurs 2 (différences horizontales) et 3 (virgule
#     flottante) ; des entiers de 8, 16 et 32 bits, signés ou non, des réels de 32 et 64 bits ; la
#     valeur manquante de GDAL (étiquette 42113), l'échelle, le décalage et le nom des bandes de ses
#     métadonnées (42112) ; le géoréférencement par ModelPixelScale et ModelTiepoint, ou
#     ModelTransformation, ou un fichier .tfw à côté, son système par son code EPSG
#     (GeoKeyDirectory : ProjectedCSTypeGeoKey, GeographicTypeGeoKey, ou les paramètres d'un
#     système « défini par l'utilisateur » reconnus), PixelIsArea ou PixelIsPoint comme GDAL ;
#   - la grille ASCII d'ESRI (.asc), avec son .prj quand il y en a un.
# Seuls les blocs (bandes de lignes ou tuiles) qui contiennent les observations sont lus et
# décompressés, et d'un fichier sans compression seulement les lignes utiles : un grand MNT
# national reste rapide.
#
# Refusés, avec un message qui le dit : les compressions JPEG, WebP, LERC, JPEG 2000, JPEG XL et
# CCITT, des valeurs de 1, 2, 4, 12 ou 24 bits, les demi-réels (16 bits), les nombres complexes,
# l'YCbCr, les points d'appui (GCP) sans transformation, un système de coordonnées que Nuée ne
# convertit pas (nommé par son code). Lus sans les couleurs : une palette (les numéros des
# classes sont lus, pas leurs couleurs). Laissés de côté : les aperçus (overviews : les valeurs
# viennent de la pleine résolution), les images suivantes d'un fichier (la première seulement),
# les masques internes et les fichiers .aux.xml.
#
# Les valeurs en un point (comme terra::extract, que verifier_raster.R compare) : la cellule qui
# le contient ; l'interpolation bilinéaire des quatre centres de cellules qui l'entourent (une
# cellule sans valeur parmi elles remplacée par sa voisine de la même ligne, puis de la même
# colonne ; au bord, la valeur du bord) ; la moyenne, ou la classe majoritaire, des cellules dont
# le centre est à moins d'un rayon (en mètres). NA hors du raster ou sans valeur.

# ---------------------------------------------------------------- lecture binaire

# Des entiers de `taille` octets (1, 2, 4 ou 8) lus dans un vecteur raw, en réels (exacts jusqu'à
# 2^53) ; `fin` : l'ordre des octets ; `signe` : en complément à deux.
entiers_bruts <- function(r, taille, fin, signe = FALSE) {
  n <- length(r) %/% taille
  if (!n) return(numeric(0))
  v <- if (taille == 1) {
    as.numeric(as.integer(r[seq_len(n)]))
  } else if (taille == 2) {
    as.numeric(readBin(r, "integer", n = n, size = 2, signed = FALSE, endian = fin))
  } else {
    # (par moitiés de 16 bits : readBin rend NA pour 0x80000000 lu en entier de 32 bits)
    h <- matrix(as.numeric(readBin(r, "integer", n = n * taille / 2, size = 2, signed = FALSE, endian = fin)), nrow = taille / 2)
    if (fin == "big") h <- h[nrow(h):1, , drop = FALSE]
    colSums(h * 65536^(seq_len(nrow(h)) - 1))
  }
  if (signe) {
    grand <- v >= 2^(8 * taille - 1)
    v[grand] <- v[grand] - 2^(8 * taille)
  }
  v
}

# Un texte lu dans un vecteur raw (sans les zéros de la fin, ceux du milieu en « | »), en UTF-8
# ou, à défaut, en Latin-1.
texte_brut <- function(r) {
  while (length(r) && r[length(r)] == as.raw(0)) r <- r[-length(r)]
  r[r == as.raw(0)] <- as.raw(0x7c)
  if (!length(r)) return("")
  s <- rawToChar(r)
  Encoding(s) <- "UTF-8"
  if (!validUTF8(s)) s <- iconv(s, "latin1", "UTF-8")
  s
}

# Les valeurs d'une étiquette TIFF de type `type` (1 BYTE, 2 ASCII, 3 SHORT, 4 LONG, 5 RATIONAL,
# 6 SBYTE, 7 UNDEFINED, 8 SSHORT, 9 SLONG, 10 SRATIONAL, 11 FLOAT, 12 DOUBLE, 13 IFD, 16 LONG8,
# 17 SLONG8, 18 IFD8) ; un texte reste en raw (ses positions, pour GeoAsciiParams, en octets).
valeurs_etiquette <- function(r, type, fin) {
  switch(as.character(type),
    "1" = , "7" = entiers_bruts(r, 1, fin),
    "2" = r,
    "3" = entiers_bruts(r, 2, fin),
    "4" = , "13" = entiers_bruts(r, 4, fin),
    "5" = { v <- entiers_bruts(r, 4, fin); v[c(TRUE, FALSE)] / v[c(FALSE, TRUE)] },
    "6" = entiers_bruts(r, 1, fin, TRUE),
    "8" = entiers_bruts(r, 2, fin, TRUE),
    "9" = entiers_bruts(r, 4, fin, TRUE),
    "10" = { v <- entiers_bruts(r, 4, fin, TRUE); v[c(TRUE, FALSE)] / v[c(FALSE, TRUE)] },
    "11" = readBin(r, "double", n = length(r) %/% 4, size = 4, endian = fin),
    "12" = readBin(r, "double", n = length(r) %/% 8, size = 8, endian = fin),
    "16" = , "18" = entiers_bruts(r, 8, fin),
    "17" = entiers_bruts(r, 8, fin, TRUE),
    NULL)
}

fichier_abime <- function(chemin) {
  stop(sprintf(traduire("%s n'est pas un fichier TIFF lisible (abîmé, ou tronqué).", "%s isn't a readable TIFF file (damaged, or truncated)."), basename(chemin)), call. = FALSE)
}

# ---------------------------------------------------------------- GeoTIFF : les répertoires

# Les étiquettes lues : la taille et les bandes de l'image, sa compression et ses blocs, le
# géoréférencement (33550 ModelPixelScale, 33922 ModelTiepoint, 34264 ModelTransformation, 34735 à
# 34737 les GeoKeys), les métadonnées et la valeur manquante de GDAL (42112, 42113) ; d'une
# palette (320), sa présence.
ETIQUETTES_TIFF <- c(254, 256, 257, 258, 259, 262, 266, 273, 277, 278, 279, 284, 317, 322, 323, 324, 325, 339,
                     33550, 33922, 34264, 34735, 34736, 34737, 42112, 42113)
TAILLES_TYPES_TIFF <- c(1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8, 4, NA, NA, 8, 8, 8)

# Un répertoire d'image (IFD) à la position `position` : ses étiquettes utiles, et la position du
# suivant (0 au dernier).
lire_ifd <- function(con, position, big, fin, taille, chemin) {
  if (!is.finite(position) || position < 8 || position >= taille) fichier_abime(chemin)
  seek(con, position)
  n <- entiers_bruts(readBin(con, "raw", if (big) 8 else 2), if (big) 8 else 2, fin)
  t <- if (big) 20 else 12
  if (!length(n) || n < 1 || n > 10000 || position + n * t > taille) fichier_abime(chemin)
  e <- matrix(readBin(con, "raw", n * t), nrow = t)
  suivant <- entiers_bruts(readBin(con, "raw", if (big) 8 else 4), if (big) 8 else 4, fin)
  if (!length(suivant)) suivant <- 0
  etiquettes <- entiers_bruts(as.vector(e[1:2, ]), 2, fin)
  types <- entiers_bruts(as.vector(e[3:4, ]), 2, fin)
  comptes <- if (big) entiers_bruts(as.vector(e[5:12, ]), 8, fin) else entiers_bruts(as.vector(e[5:8, ]), 4, fin)
  champ <- if (big) 13:20 else 9:12
  entrees <- list()
  for (k in which(etiquettes %in% c(ETIQUETTES_TIFF, 320))) {
    if (etiquettes[k] == 320) {
      entrees[["320"]] <- TRUE
      next
    }
    s <- if (types[k] >= 1 && types[k] <= 18) TAILLES_TYPES_TIFF[types[k]] else NA
    if (is.na(s)) next
    nb <- s * comptes[k]
    brut <- if (nb <= length(champ)) e[champ[seq_len(nb)], k] else {
      ou <- entiers_bruts(e[champ, k], length(champ), fin)
      if (ou + nb > taille) fichier_abime(chemin)
      seek(con, ou)
      readBin(con, "raw", nb)
    }
    entrees[[as.character(etiquettes[k])]] <- valeurs_etiquette(brut, types[k], fin)
  }
  list(entrees = entrees, suivant = suivant)
}

# La position du répertoire qui suit celui de `position`, sans lire ses étiquettes (pour compter
# les images d'un fichier : aperçus, masques).
ifd_suivant <- function(con, position, big, fin, taille) {
  if (!is.finite(position) || position < 8 || position >= taille) return(0)
  seek(con, position)
  n <- entiers_bruts(readBin(con, "raw", if (big) 8 else 2), if (big) 8 else 2, fin)
  if (!length(n) || position + n * (if (big) 20 else 12) > taille) return(0)
  seek(con, position + (if (big) 8 else 2) + n * (if (big) 20 else 12))
  s <- entiers_bruts(readBin(con, "raw", if (big) 8 else 4), if (big) 8 else 4, fin)
  if (length(s)) s else 0
}

COMPRESSIONS_TIFF <- c("1" = "aucune", "5" = "LZW", "8" = "Deflate", "32946" = "Deflate", "32773" = "PackBits",
                       "34925" = "LZMA", "50000" = "ZSTD", "2" = "CCITT RLE", "3" = "CCITT Fax 3", "4" = "CCITT Fax 4",
                       "6" = "JPEG (ancien)", "7" = "JPEG", "34712" = "JPEG 2000", "34887" = "LERC", "50001" = "WebP",
                       "50002" = "JPEG XL", "34892" = "JPEG (DNG)", "32909" = "Pixar", "34676" = "SGI Log")

# Le zstd de memDecompress (R 4.4 et après, s'il a été compilé avec), essayé une fois.
zstd_lisible <- local({
  lisible <- NULL
  function() {
    if (is.null(lisible)) lisible <<- "zstd" %in% eval(formals(memDecompress)$type) &&
      !inherits(tryCatch(memDecompress(memCompress(as.raw(1:10), "zstd"), "zstd"), error = function(e) e), "error")
    lisible
  }
})

# Un GeoTIFF ouvert : sa première image en pleine résolution (les aperçus et les masques, que
# NewSubfileType signale, laissés de côté), ce qu'il faut pour en lire les blocs, son
# géoréférencement et son système. Les fichiers qu'on ne sait pas lire sont refusés ici.
ouvrir_geotiff <- function(chemin) {
  taille <- file.size(chemin)
  con <- file(chemin, "rb")
  on.exit(close(con))
  tete <- readBin(con, "raw", 16)
  nom <- basename(chemin)
  pas_tiff <- function() stop(sprintf(traduire("%s n'est pas un fichier TIFF.", "%s isn't a TIFF file."), nom), call. = FALSE)
  if (length(tete) < 8) pas_tiff()
  fin <- if (identical(tete[1:2], as.raw(c(0x49, 0x49)))) "little" else if (identical(tete[1:2], as.raw(c(0x4d, 0x4d)))) "big" else pas_tiff()
  version <- entiers_bruts(tete[3:4], 2, fin)
  if (!version %in% c(42, 43)) pas_tiff()
  big <- version == 43
  position <- if (big) entiers_bruts(tete[9:16], 8, fin) else entiers_bruts(tete[5:8], 4, fin)
  ifd <- NULL
  images <- 0
  repeat {
    if (is.null(ifd)) {
      i <- lire_ifd(con, position, big, fin, taille, chemin)
      sous <- i$entrees[["254"]] %||% 0
      if (bitwAnd(as.integer(sous[1] %% 8), 5L) == 0L) ifd <- i$entrees  # (ni un aperçu, bit 0, ni un masque, bit 2)
      suivant <- i$suivant
    } else {
      suivant <- ifd_suivant(con, position, big, fin, taille)
    }
    images <- images + 1
    if (!suivant || images >= 64) break
    position <- suivant
  }
  if (is.null(ifd)) fichier_abime(chemin)
  e <- ifd
  refus <- function(fr, en, ...) stop(sprintf(traduire(paste0("%s : ", fr), paste0("%s: ", en)), nom, ...), call. = FALSE)
  largeur <- e[["256"]][1]
  hauteur <- e[["257"]][1]
  if (is.null(largeur) || is.null(hauteur) || largeur < 1 || hauteur < 1) fichier_abime(chemin)
  bandes <- (e[["277"]] %||% 1)[1]
  bits <- unique(e[["258"]] %||% 1)
  format <- unique(e[["339"]] %||% 1)
  format[format == 4] <- 1  # (« indéfini » : des entiers non signés, comme GDAL)
  if (length(bits) > 1 || length(format) > 1) refus("des bandes de types différents ne se lisent pas ensemble.", "bands of different types aren't read together.")
  compression <- (e[["259"]] %||% 1)[1]
  photometrie <- (e[["262"]] %||% 1)[1]
  nom_compression <- unname(COMPRESSIONS_TIFF[as.character(compression)])
  if (is.na(nom_compression)) nom_compression <- sprintf("n° %d", as.integer(compression))
  lisibles <- c(1, 5, 8, 32946, 32773, 34925, if (zstd_lisible()) 50000)
  if (!compression %in% lisibles) {
    refus("la compression %s ne se lit pas ici (seulement aucune, LZW, Deflate, PackBits, LZMA%s). Réécrivez le fichier, par exemple dans QGIS (Raster › Conversion › Traduire, compression LZW ou DEFLATE) ou avec gdal_translate -co COMPRESS=DEFLATE.",
          "%s compression isn't read here (only none, LZW, Deflate, PackBits, LZMA%s). Write the file again, for instance in QGIS (Raster › Conversion › Translate, LZW or DEFLATE compression) or with gdal_translate -co COMPRESS=DEFLATE.",
          nom_compression, if (50000 %in% lisibles) ", ZSTD" else "")
  }
  if (photometrie == 6) refus("les couleurs YCbCr (des photographies) ne se lisent pas ici.", "YCbCr colours (photographs) aren't read here.")
  if ((e[["266"]] %||% 1)[1] == 2) refus("l'ordre des bits FillOrder = 2 ne se lit pas ici.", "the FillOrder = 2 bit order isn't read here.")
  type <- if (format == 3 && bits %in% c(32, 64)) paste0("float", bits)
          else if (format %in% c(1, 2) && bits %in% c(8, 16, 32)) paste0(if (format == 2) "int" else "uint", bits)
          else if (format == 3 && bits == 16) refus("les demi-réels (16 bits) ne se lisent pas ici.", "half floats (16 bits) aren't read here.")
          else if (format %in% c(5, 6)) refus("les nombres complexes ne se lisent pas ici.", "complex numbers aren't read here.")
          else refus("des valeurs de %d bits ne se lisent pas ici (seulement 8, 16, 32 et 64).", "%d-bit values aren't read here (only 8, 16, 32 and 64).", as.integer(bits))
  predicteur <- (e[["317"]] %||% 1)[1]
  if (compression %in% c(1, 32773)) predicteur <- 1  # (libtiff ne l'applique qu'aux compressions qui l'ont)
  if (!predicteur %in% 1:3 || (predicteur == 3 && format != 3)) {
    refus("le prédicteur %d avec des valeurs %s ne se lit pas ici.", "predictor %d with %s values isn't read here.", as.integer(predicteur), type)
  }
  tuiles <- !is.null(e[["322"]])
  planaire <- (e[["284"]] %||% 1)[1] == 2 && bandes > 1
  if (tuiles) {
    bl <- e[["322"]][1]
    bh <- e[["323"]][1]
    offsets <- e[["324"]]
    comptes <- e[["325"]]
    en_travers <- ceiling(largeur / bl)
    par_plan <- en_travers * ceiling(hauteur / bh)
  } else {
    bl <- largeur
    bh <- min((e[["278"]] %||% hauteur)[1], hauteur)
    offsets <- e[["273"]]
    comptes <- e[["279"]]
    en_travers <- 1
    par_plan <- ceiling(hauteur / bh)
  }
  octets <- bits / 8
  n_blocs <- par_plan * if (planaire) bandes else 1
  if (is.null(offsets) || length(offsets) < n_blocs) fichier_abime(chemin)
  if (is.null(comptes)) {
    if (compression != 1) fichier_abime(chemin)
    comptes <- rep(bl * bh * octets * if (planaire) 1 else bandes, n_blocs)
  }
  # (un bloc compressé se décompresse en entier : au-delà de 1 Go, réécrire le fichier en tuiles)
  taille_bloc <- bl * bh * octets * (if (planaire) 1 else bandes)
  if (compression != 1 && taille_bloc > 1e9) {
    refus("le fichier est fait d'un bloc compressé de %s Mo, trop gros pour n'en lire qu'une partie. Réécrivez-le en tuiles : dans QGIS, Raster › Conversion › Traduire avec l'option TILED=YES, ou gdal_translate -co TILED=YES.",
          "the file is a single compressed block of %s MB, too big to read only part of it. Write it again in tiles: in QGIS, Raster › Conversion › Translate with the option TILED=YES, or gdal_translate -co TILED=YES.",
          format(round(taille_bloc / 1e6)))
  }
  cles <- cles_geotiff(e)
  meta <- metadonnees_gdal(if (!is.null(e[["42112"]])) texte_brut(e[["42112"]]) else "", bandes)
  nodata <- if (!is.null(e[["42113"]])) lire_nodata(texte_brut(e[["42113"]])) else NA_real_
  g <- list(chemin = chemin, nom = nom, taille = taille, format = if (big) "BigTIFF" else "GeoTIFF",
            largeur = largeur, hauteur = hauteur, bandes = bandes, type = type, octets = octets,
            nodata = nodata, echelle = meta$echelle, decalage = meta$decalage, noms_bandes = meta$noms,
            palette = photometrie == 3 || !is.null(e[["320"]]), images = images,
            tiff = list(fin = fin, compression = compression, nom_compression = nom_compression, predicteur = predicteur,
                        planaire = planaire, tuiles = tuiles, bl = bl, bh = bh, en_travers = en_travers, par_plan = par_plan,
                        offsets = offsets, comptes = comptes))
  geo <- georeference_tiff(e, cles, chemin)
  g$gt <- geo$gt
  g$pixel_point <- geo$pixel_point
  g$systeme <- geo$systeme
  g$notes <- geo$notes
  g
}

# Les GeoKeys (étiquette 34735 : version, révision, révision mineure, nombre de clés, puis pour
# chaque clé son numéro, où est sa valeur — 0 dans l'entrée même, 34736 les réels, 34737 le
# texte —, combien, et la valeur ou sa position), par numéro.
cles_geotiff <- function(e) {
  d <- e[["34735"]]
  if (is.null(d) || length(d) < 4) return(list())
  reels <- e[["34736"]]
  texte <- e[["34737"]]
  cles <- list()
  for (k in seq_len(min(d[4], (length(d) - 4) %/% 4))) {
    x <- d[4 * k + 1:4]
    pos <- x[4] + seq_len(x[3])
    v <- if (x[2] == 0) x[4]
         else if (x[2] == 34736 && length(reels) >= max(pos)) reels[pos]
         else if (x[2] == 34737 && length(texte) >= max(pos)) sub("\\|$", "", texte_brut(texte[pos]))
         else if (x[2] == 34735 && length(d) >= max(pos)) d[pos]
    if (!is.null(v)) cles[[as.character(x[1])]] <- v
  }
  cles
}

# L'échelle, le décalage et le nom de chaque bande dans les métadonnées de GDAL (étiquette
# 42112 : <GDALMetadata><Item name="SCALE" sample="0" role="scale">0.1</Item>...).
metadonnees_gdal <- function(xml, bandes) {
  r <- list(echelle = rep(1, bandes), decalage = rep(0, bandes), noms = rep(NA_character_, bandes))
  if (!nzchar(xml)) return(r)
  items <- regmatches(xml, gregexpr("<Item[^>]*>[^<]*</Item>", xml))[[1]]
  attribut <- function(it, a) { m <- regmatches(it, regexec(sprintf("%s=\"([^\"]*)\"", a), it))[[1]]; if (length(m)) m[2] else NA_character_ }
  for (it in items) {
    role <- attribut(it, "role")
    bande <- suppressWarnings(as.integer(attribut(it, "sample"))) + 1L
    if (is.na(role) || is.na(bande) || bande > bandes) next
    valeur <- sub("^<Item[^>]*>([^<]*)</Item>$", "\\1", it)
    valeur <- gsub("&amp;", "&", gsub("&lt;", "<", gsub("&gt;", ">", valeur, fixed = TRUE), fixed = TRUE), fixed = TRUE)
    if (role == "scale") r$echelle[bande] <- suppressWarnings(as.numeric(valeur)) %|na|% 1
    if (role == "offset") r$decalage[bande] <- suppressWarnings(as.numeric(valeur)) %|na|% 0
    if (role == "description" && nzchar(trimws(valeur))) r$noms[bande] <- trimws(valeur)
  }
  r
}

`%|na|%` <- function(a, b) if (length(a) && !is.na(a[1])) a else b

# La valeur manquante écrite en texte par GDAL (« -9999 », « nan », « -3.4028234663852886e+38 »).
lire_nodata <- function(texte) {
  texte <- trimws(texte)
  if (tolower(texte) %in% c("nan", "-nan")) return(NaN)
  suppressWarnings(as.numeric(texte))
}

# Le géoréférencement d'un GeoTIFF : la transformation des pixels en coordonnées, comme celle de
# GDAL (gt : x = gt1 + col × gt2 + ligne × gt3, y = gt4 + col × gt5 + ligne × gt6, col et ligne
# depuis le coin haut-gauche de la première cellule), d'après ModelTransformation, ou un point
# d'appui et la taille des cellules, ou un fichier .tfw ; PixelIsPoint (la clé 1025 à 2) : le
# point d'appui au centre de la cellule, la transformation décalée d'une demi-cellule, comme GDAL.
georeference_tiff <- function(e, cles, chemin) {
  nom <- basename(chemin)
  echelle <- e[["33550"]]
  points <- e[["33922"]]
  transfo <- e[["34264"]]
  notes <- character(0)
  gt <- NULL
  if (length(transfo) >= 16) {
    gt <- c(transfo[4], transfo[1], transfo[2], transfo[8], transfo[5], transfo[6])
  } else if (length(points) >= 6) {
    if (length(points) > 6 || length(echelle) < 2) {
      stop(sprintf(traduire("%s est géoréférencé par des points d'appui (GCP), sans transformation : Nuée ne les lit pas. Géoréférencez-le dans QGIS (Raster › Projections › Projection, ou le Géoréférenceur) ou avec gdalwarp.",
                            "%s is georeferenced by ground control points (GCPs), without a transformation: Nuée doesn't read them. Georeference it in QGIS (Raster › Projections › Warp, or the Georeferencer) or with gdalwarp."), nom), call. = FALSE)
    }
    gt <- c(points[4] - points[1] * echelle[1], echelle[1], 0, points[5] + points[2] * echelle[2], 0, -echelle[2])
  } else {
    gt <- monde_tfw(chemin)
    if (!is.null(gt)) notes <- c(notes, traduire("géoréférencement lu dans le fichier .tfw", "georeferencing read from the .tfw file"))
  }
  if (is.null(gt) || anyNA(gt) || gt[2] * gt[6] - gt[3] * gt[5] == 0) {
    stop(sprintf(traduire("%s n'est pas géoréférencé (ni ModelTiepoint et ModelPixelScale, ni ModelTransformation, ni fichier .tfw) : Nuée ne sait pas où placer ses cellules.",
                          "%s isn't georeferenced (no ModelTiepoint and ModelPixelScale, no ModelTransformation, no .tfw file): Nuée can't tell where its cells are."), nom), call. = FALSE)
  }
  pixel_point <- identical(as.numeric(cles[["1025"]]), 2)
  if (pixel_point) {
    gt[1] <- gt[1] - (gt[2] + gt[3]) * 0.5
    gt[4] <- gt[4] - (gt[5] + gt[6]) * 0.5
  }
  s <- systeme_cles(cles)
  if (is.null(s)) s <- systeme_prj(chemin)  # (sans GeoKeys : un .prj à côté, s'il y en a un)
  list(gt = gt, pixel_point = pixel_point, systeme = s, notes = notes)
}

# Un fichier de monde (.tfw, .wld) : la taille des cellules, la rotation et le centre de la
# première cellule ; en transformation de GDAL (depuis le coin de la cellule).
monde_tfw <- function(chemin) {
  base <- tools::file_path_sans_ext(chemin)
  for (ext in c("tfw", "TFW", "tifw", "wld", "WLD")) {
    f <- paste0(base, ".", ext)
    if (!file.exists(f)) next
    v <- suppressWarnings(as.numeric(trimws(readLines(f, n = 6, warn = FALSE))))
    if (length(v) < 6 || anyNA(v)) return(NULL)
    return(c(v[5] - 0.5 * v[1] - 0.5 * v[3], v[1], v[3], v[6] - 0.5 * v[2] - 0.5 * v[4], v[2], v[4]))
  }
  NULL
}

# ---------------------------------------------------------------- systèmes de coordonnées

# Des codes que d'autres registres donnent aux systèmes de Nuée (ESRI, anciens codes de la
# Mercator du web).
ALIAS_EPSG <- c("3785" = 3857, "900913" = 3857, "102100" = 3857, "102113" = 3857, "102110" = 2154, "4019" = 4258)

# Le système d'un code EPSG : celui de Nuée s'il le connaît (spatial.R : systeme_coordonnees),
# sinon le refus nommé, que l'interface montre sans ajouter de valeurs.
systeme_code <- function(code, nom = NULL) {
  code <- as.numeric(code)[1]
  connu <- if (!is.na(ALIAS_EPSG[as.character(code)])) unname(ALIAS_EPSG[as.character(code)]) else code
  s <- systeme_coordonnees(connu)
  if (!is.null(s) && !identical(s$type, "plan")) return(list(code = as.integer(connu), nom = s$nom))
  list(code = as.integer(code), nom = nom %||% "", erreur = systeme_inconnu(sprintf("EPSG:%d%s", as.integer(code), if (length(nom) && nzchar(nom)) sprintf(" (%s)", nom) else "")))
}

systeme_inconnu <- function(quoi) {
  sprintf(traduire("le système de coordonnées du fichier, %s, n'est pas de ceux que Nuée convertit (WGS 84, ETRS89, RGF93, Lambert-93, CC42 à CC50, Lambert II étendu, UTM, LAEA Europe, Mercator du web). Reprojetez le raster dans l'un d'eux : dans QGIS, Raster › Projections › Projection (Warp), ou avec gdalwarp -t_srs EPSG:2154.",
                   "the file's coordinate system, %s, isn't one Nuée converts (WGS 84, ETRS89, RGF93, Lambert-93, CC42 to CC50, Lambert II étendu, UTM, LAEA Europe, web Mercator). Reproject the raster to one of them: in QGIS, Raster › Projections › Warp (Reproject), or with gdalwarp -t_srs EPSG:2154."), quoi)
}

# L'ellipsoïde de Nuée (spatial.R : ELLIPSOIDES) de demi-grand axe a et d'inverse d'aplatissement
# 1/f donnés : celui du même a dont le 1/f est le plus proche, à 0,001 près (ceux de WGS 84 et du
# GRS 1980 ne diffèrent que de 1,5e-6) ; NULL sinon.
ellipsoide_de <- function(a, inv_f) {
  if (!length(a) || !length(inv_f) || is.na(a) || is.na(inv_f)) return(NULL)
  if (abs(a - 6378137) < 0.01 && inv_f == 0) return("SPHERE")  # (la sphère de la Mercator du web)
  ecarts <- vapply(ELLIPSOIDES, function(el) if (abs(a - el$a) < 0.01) abs(inv_f - 1 / el$f) else Inf, 0)
  if (min(ecarts) < 1e-3) names(ELLIPSOIDES)[which.min(ecarts)] else NULL
}

# Un système défini par ses paramètres (des GeoKeys « définies par l'utilisateur » ou un .prj sans
# code) : celui de Nuée qui a les mêmes, s'il y en a un. `p` : type ("lcc", "lcc1", "tm", "laea",
# "merc", "lonlat"), ellipsoide (ellipsoide_de), phi1, phi2, phi0, lambda0, x0, y0, k0, en degrés
# et en mètres.
systeme_parametres <- function(p, nom = "") {
  proche <- function(a, b, tol = 1e-6) !is.null(a) && length(a) && !is.na(a[1]) && abs(a[1] - b) <= tol
  ell <- p$ellipsoide %||% ""
  code <- NULL
  if (identical(p$type, "lonlat")) {
    code <- switch(ell, WGS84 = 4326, GRS80 = 4258, NULL)
  } else if (identical(p$type, "lcc") && ell %in% c("GRS80", "WGS84")) {
    for (n in c(2154, 3942:3950)) {
      s <- systeme_coordonnees(n)
      if (proche(min(p$phi1, p$phi2), min(s$phi1, s$phi2)) && proche(max(p$phi1, p$phi2), max(s$phi1, s$phi2)) && proche(p$phi0, s$phi0) &&
          proche(p$lambda0, s$lambda0) && proche(p$x0, s$x0, 1e-3) && proche(p$y0, s$y0, 1e-3)) { code <- n; break }
    }
  } else if (identical(p$type, "lcc1") && identical(ell, "CLARKE_IGN")) {
    if (proche(p$k0, 0.99987742, 1e-8) && proche(p$x0, 600000, 1e-3) && proche(p$y0, 2200000, 1e-3)) code <- 27572
  } else if (identical(p$type, "tm") && ell %in% c("WGS84", "GRS80") && proche(p$k0, 0.9996, 1e-9) && proche(p$x0, 500000, 1e-3) &&
             (is.null(p$phi0) || proche(p$phi0, 0)) && !is.null(p$lambda0) && proche((p$lambda0 + 183) / 6, round((p$lambda0 + 183) / 6))) {
    zone <- round((p$lambda0 + 183) / 6)
    sud <- proche(p$y0, 1e7, 1e-3)
    if (sud || proche(p$y0, 0, 1e-3)) {
      code <- if (ell == "WGS84") (if (sud) 32700 else 32600) + zone else if (!sud && zone %in% 28:38) 25800 + zone
    }
  } else if (identical(p$type, "laea") && ell %in% c("GRS80", "WGS84")) {
    if (proche(p$phi0, 52) && proche(p$lambda0, 10) && proche(p$x0, 4321000, 1e-3) && proche(p$y0, 3210000, 1e-3)) code <- 3035
  } else if (identical(p$type, "merc") && ell %in% c("SPHERE", "WGS84") && isTRUE(p$web)) {
    code <- 3857
  }
  if (!is.null(code)) return(systeme_code(code))
  list(code = NA_integer_, nom = nom, erreur = systeme_inconnu(if (nzchar(nom)) sprintf("« %s »", nom) else traduire("défini par ses paramètres", "defined by its parameters")))
}

# Le système de coordonnées des GeoKeys : son code EPSG (ProjectedCSTypeGeoKey 3072,
# GeographicTypeGeoKey 2048), ou ses paramètres quand il est « défini par l'utilisateur » (32767) ;
# NULL sans GeoKeys de système.
systeme_cles <- function(cles) {
  modele <- as.numeric(cles[["1024"]] %||% NA)
  pcs <- as.numeric(cles[["3072"]] %||% NA)
  gcs <- as.numeric(cles[["2048"]] %||% NA)
  citation <- function(...) { for (k in c(...)) if (is.character(cles[[k]]) && nzchar(cles[[k]])) return(cles[[k]]); "" }
  nom_pcs <- citation("3073", "1026")
  if (!is.na(pcs) && pcs != 32767 && pcs > 0 && (is.na(modele) || modele == 1)) return(systeme_code(pcs, nom_pcs))
  ellipsoide <- function() {
    if (!is.na(gcs) && gcs != 32767) return(switch(as.character(gcs), "4326" = "WGS84", "4258" = , "4171" = , "4019" = "GRS80", "4807" = , "4275" = "CLARKE_IGN", NULL))
    e <- as.numeric(cles[["2056"]] %||% NA)
    if (!is.na(e) && e != 32767) return(switch(as.character(e), "7030" = "WGS84", "7019" = "GRS80", "7011" = "CLARKE_IGN", NULL))
    a <- as.numeric(cles[["2057"]] %||% NA)
    inv_f <- as.numeric(cles[["2059"]] %||% NA)
    if (is.na(inv_f) && !is.null(cles[["2058"]])) inv_f <- a / (a - as.numeric(cles[["2058"]]))
    ellipsoide_de(a, inv_f)
  }
  if (identical(modele, 2) || (is.na(modele) && is.na(pcs) && !is.na(gcs))) {
    if (!is.na(gcs) && gcs != 32767 && gcs > 0) return(systeme_code(gcs, citation("2049", "1026")))
    return(systeme_parametres(list(type = "lonlat", ellipsoide = ellipsoide()), citation("2049", "1026")))
  }
  if (identical(modele, 1)) {
    # (un système défini par l'utilisateur : sa méthode, ProjCoordTransGeoKey 3075, et ses paramètres)
    v <- function(...) { for (k in c(...)) if (!is.null(cles[[k]])) return(as.numeric(cles[[k]])[1]); NULL }
    methode <- as.numeric(cles[["3075"]] %||% NA)
    type <- switch(as.character(methode), "1" = "tm", "8" = "lcc", "9" = "lcc1", "10" = "laea", "7" = "merc", NA)
    p <- list(type = type, ellipsoide = ellipsoide(), phi1 = v("3078"), phi2 = v("3079"),
              phi0 = v("3085", "3081", "3089"), lambda0 = v("3084", "3080", "3088"),
              x0 = v("3086", "3082"), y0 = v("3087", "3083"), k0 = v("3092", "3093"),
              web = grepl("Pseudo.Mercator|Auxiliary.Sphere", nom_pcs, ignore.case = TRUE))
    return(systeme_parametres(p, nom_pcs))
  }
  if (length(cles)) return(list(code = NA_integer_, nom = nom_pcs, erreur = systeme_inconnu(if (nzchar(nom_pcs)) sprintf("« %s »", nom_pcs) else traduire("sans code EPSG", "without an EPSG code"))))
  NULL
}

# Le système d'un fichier .prj (WKT, celui d'ESRI ou de l'OGC) à côté de `chemin` ; NULL sans .prj.
systeme_prj <- function(chemin) {
  base <- tools::file_path_sans_ext(chemin)
  f <- Find(file.exists, paste0(base, c(".prj", ".PRJ", ".Prj")))
  if (is.null(f)) return(NULL)
  texte <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  arbre <- tryCatch(lire_wkt(texte), error = function(e) NULL)
  if (is.null(arbre)) return(list(code = NA_integer_, nom = "", erreur = systeme_inconnu(traduire("illisible dans le fichier .prj", "unreadable in the .prj file"))))
  systeme_wkt(arbre)
}

# Un texte WKT en arbre : chaque nœud list(mot, args), ses arguments des textes, des nombres ou
# des nœuds.
lire_wkt <- function(texte) {
  jetons <- regmatches(texte, gregexpr("\"([^\"]|\"\")*\"|[A-Za-z_][A-Za-z0-9_]*|[-+]?[0-9.]+([eE][-+]?[0-9]+)?|[\\[\\]\\(\\),]", texte, perl = TRUE))[[1]]
  i <- 1
  noeud <- function() {
    mot <- jetons[i]
    i <<- i + 1
    if (i > length(jetons) || !jetons[i] %in% c("[", "(")) return(mot)
    i <<- i + 1
    args <- list()
    repeat {
      j <- jetons[i]
      if (j %in% c("]", ")")) { i <<- i + 1; break }
      if (j == ",") { i <<- i + 1; next }
      if (startsWith(j, "\"")) { args[[length(args) + 1]] <- gsub("\"\"", "\"", substr(j, 2, nchar(j) - 1)); i <<- i + 1 }
      else if (grepl("^[-+]?[0-9.]", j)) { args[[length(args) + 1]] <- as.numeric(j); i <<- i + 1 }
      else args[[length(args) + 1]] <- noeud()
      if (i > length(jetons)) break
    }
    list(mot = toupper(mot), args = args)
  }
  noeud()
}

# Le système d'un arbre WKT : son code EPSG (AUTHORITY ou ID à la racine), sinon ses paramètres
# (systeme_parametres).
systeme_wkt <- function(a) {
  enfants <- function(n, mot) Filter(function(x) is.list(x) && x$mot %in% mot, n$args)
  premier <- function(n, mot) { e <- enfants(n, mot); if (length(e)) e[[1]] }
  nom <- if (length(a$args) && is.character(a$args[[1]])) a$args[[1]] else ""
  auto <- premier(a, c("AUTHORITY", "ID"))
  if (!is.null(auto) && length(auto$args) >= 2 && toupper(as.character(auto$args[[1]])) == "EPSG") return(systeme_code(as.numeric(auto$args[[2]]), nom))
  geog <- if (a$mot %in% c("GEOGCS", "GEOGCRS", "GEODCRS")) a else premier(a, c("GEOGCS", "BASEGEOGCRS", "BASEGEODCRS"))
  datum <- if (!is.null(geog)) premier(geog, c("DATUM", "GEODETICDATUM"))
  sph <- if (!is.null(datum)) premier(datum, c("SPHEROID", "ELLIPSOID"))
  ell <- if (!is.null(sph) && length(sph$args) >= 3) ellipsoide_de(as.numeric(sph$args[[2]]), as.numeric(sph$args[[3]]))
  if (identical(a, geog)) return(systeme_parametres(list(type = "lonlat", ellipsoide = ell), nom))
  proj <- premier(a, "PROJECTION")
  methode <- tolower(if (!is.null(proj)) as.character(proj$args[[1]]) else "")
  params <- list()
  for (p in enfants(a, "PARAMETER")) params[[tolower(as.character(p$args[[1]]))]] <- as.numeric(p$args[[2]])
  v <- function(...) { for (k in c(...)) if (!is.null(params[[k]])) return(params[[k]]); NULL }
  type <- if (grepl("lambert_conformal_conic", methode)) {
    if (!is.null(v("standard_parallel_2"))) "lcc" else "lcc1"
  } else if (grepl("transverse_mercator", methode)) "tm"
  else if (grepl("lambert_azimuthal_equal_area", methode)) "laea"
  else if (grepl("mercator", methode)) "merc"
  else NA
  phi0 <- v("latitude_of_origin", "latitude_of_center", "standard_parallel_1")
  if (identical(type, "lcc")) phi0 <- v("latitude_of_origin", "latitude_of_center")
  systeme_parametres(list(type = type, ellipsoide = ell, phi1 = v("standard_parallel_1"), phi2 = v("standard_parallel_2"),
                          phi0 = phi0, lambda0 = v("central_meridian", "longitude_of_center", "longitude_of_origin"),
                          x0 = v("false_easting"), y0 = v("false_northing"), k0 = v("scale_factor"),
                          web = grepl("auxiliary_sphere|pseudo_mercator|popular_visualisation", methode) || grepl("Pseudo.Mercator|Auxiliary.Sphere", nom, ignore.case = TRUE)),
                     nom)
}

# ---------------------------------------------------------------- grille ASCII d'ESRI

# Une grille ASCII d'ESRI (.asc) ouverte : son en-tête (ncols, nrows, xllcorner ou xllcenter,
# yllcorner ou yllcenter, cellsize ou dx et dy, NODATA_value), le nombre de ses lignes, et son
# système d'après le .prj à côté.
ouvrir_asc <- function(chemin) {
  nom <- basename(chemin)
  con <- file(chemin, "r")
  on.exit(close(con))
  entete <- list()
  lignes <- 0
  premiere <- NULL
  repeat {
    l <- readLines(con, n = 1, warn = FALSE)
    if (!length(l)) break
    m <- regmatches(l, regexec("^\\s*([A-Za-z_]+)\\s+(\\S+)\\s*$", l))[[1]]
    if (!length(m)) { premiere <- l; break }
    entete[[tolower(m[2])]] <- suppressWarnings(as.numeric(m[3]))
    lignes <- lignes + 1
    if (lignes > 20) break
  }
  n <- function(k) entete[[k]] %||% NA_real_
  dx <- n("cellsize") %|na|% n("dx")
  dy <- n("cellsize") %|na|% n("dy")
  W <- n("ncols")
  H <- n("nrows")
  if (is.na(W) || is.na(H) || is.na(dx) || is.na(dy) || W < 1 || H < 1 || dx <= 0 || dy <= 0) {
    stop(sprintf(traduire("%s n'est pas une grille ASCII d'ESRI lisible (il lui faut ncols, nrows, xllcorner, yllcorner et cellsize).",
                          "%s isn't a readable ESRI ASCII grid (it needs ncols, nrows, xllcorner, yllcorner and cellsize)."), nom), call. = FALSE)
  }
  x0 <- if (!is.na(n("xllcorner"))) n("xllcorner") else n("xllcenter") - dx / 2
  y0 <- if (!is.na(n("yllcorner"))) n("yllcorner") else n("yllcenter") - dy / 2
  if (is.na(x0) || is.na(y0)) stop(sprintf(traduire("%s : il manque xllcorner (ou xllcenter) ou yllcorner (ou yllcenter).", "%s: xllcorner (or xllcenter) or yllcorner (or yllcenter) is missing."), nom), call. = FALSE)
  # (les valeurs de la première ligne : des entiers, des catégories peut-être)
  entiers <- !is.null(premiere) && !grepl("[.eE]", premiere)
  s <- systeme_prj(chemin)
  list(chemin = chemin, nom = nom, taille = file.size(chemin), format = "ASCII", largeur = W, hauteur = H, bandes = 1,
       type = if (entiers) "texte_entiers" else "texte", octets = NA, nodata = n("nodata_value"), echelle = 1, decalage = 0,
       noms_bandes = NA_character_, palette = FALSE, images = 1, asc = list(entete = lignes, entiers = entiers),
       gt = c(x0, dx, 0, y0 + H * dy, 0, -dy), pixel_point = FALSE, systeme = s, notes = character(0))
}

# Les lignes `lignes` (base 0) d'une grille ASCII, en matrice (une ligne par ligne demandée) : lues
# en parcourant le fichier jusqu'à la dernière, une ligne de texte par ligne de la grille ; une
# grille écrite autrement (des lignes coupées) est lue en entier.
lignes_asc <- function(g, lignes) {
  con <- file(g$chemin, "r")
  on.exit(close(con))
  readLines(con, n = g$asc$entete, warn = FALSE)
  courante <- 0
  M <- matrix(NA_real_, length(lignes), g$largeur)
  for (k in seq_along(lignes)) {
    saut <- lignes[k] - courante
    while (saut > 0) {
      lu <- length(readLines(con, n = min(saut, 5000), warn = FALSE))
      if (!lu) break
      saut <- saut - lu
    }
    l <- readLines(con, n = 1, warn = FALSE)
    courante <- lignes[k] + 1
    v <- if (length(l)) suppressWarnings(as.numeric(strsplit(trimws(l), "[[:space:]]+")[[1]])) else numeric(0)
    if (length(v) != g$largeur) {
      tout <- scan(g$chemin, what = double(), skip = g$asc$entete, quiet = TRUE)
      if (length(tout) != g$largeur * g$hauteur) {
        stop(sprintf(traduire("%s : la grille a %s valeurs au lieu de %s (ncols × nrows).", "%s: the grid has %s values instead of %s (ncols × nrows)."),
                     g$nom, format(length(tout)), format(g$largeur * g$hauteur)), call. = FALSE)
      }
      return(matrix(tout, g$hauteur, g$largeur, byrow = TRUE)[lignes + 1, , drop = FALSE])
    }
    M[k, ] <- v
  }
  M
}

# ---------------------------------------------------------------- décompression

# LZW de TIFF (TIFF 6.0, section 13 : codes de 9 à 12 bits, l'octet de poids fort d'abord, le
# code 256 qui vide le dictionnaire, 257 la fin), décodé par morceaux entre deux codes 256 sans
# boucle sur les codes : la largeur de chaque code ne dépend que de son rang depuis le dernier 256
# (9 bits pour les 254 premiers, 10 jusqu'au 766e, 11 jusqu'au 1790e, puis 12, le changement
# « en avance » de TIFF) ; chaque entrée du dictionnaire a pour parent le code d'avant, sa première
# lettre et sa longueur suivent les parents par sauts doublés, et le texte de chaque code se
# déroule de la fin vers le début par les ancêtres à 2^k pas. `attendu` : la taille du bloc.
LARGEURS_LZW <- c(rep(9L, 254), rep(10L, 512), rep(11L, 1024), rep(12L, 2306))
DECALAGES_LZW <- c(0L, cumsum(LARGEURS_LZW)[-length(LARGEURS_LZW)])

decompresser_lzw <- function(b, attendu = Inf) {
  nb <- length(b)
  if (nb < 2) return(raw(0))
  if (b[1] == as.raw(0) && bitwAnd(as.integer(b[2]), 1L) == 1L) {
    stop(traduire("ce fichier utilise l'ancien LZW de TIFF (avant la version 5.0), qui ne se lit pas ici.", "this file uses TIFF's old LZW (before version 5.0), which isn't read here."), call. = FALSE)
  }
  octets <- c(as.integer(b), 0L, 0L, 0L)
  total <- nb * 8
  lire <- function(pos, w) {
    i <- pos %/% 8L
    v <- octets[i + 1L] * 65536L + octets[i + 2L] * 256L + octets[i + 3L]
    bitwAnd(bitwShiftR(v, 24L - pos %% 8L - w), bitwShiftL(1L, w) - 1L)
  }
  morceaux <- list()
  produit <- 0
  pos <- if (lire(0L, 9L) == 256L) 9L else 0L
  repeat {
    p <- pos + DECALAGES_LZW
    ok <- p + LARGEURS_LZW <= total
    if (!ok[1]) break
    codes <- lire(p[ok], LARGEURS_LZW[ok])
    fin <- which(codes == 256L | codes == 257L)[1]
    n <- if (is.na(fin)) length(codes) else fin - 1L
    if (is.na(fin) && length(codes) == length(LARGEURS_LZW)) stop(traduire("bloc LZW abîmé.", "damaged LZW block."), call. = FALSE)
    if (n > 0) {
      m <- morceau_lzw(codes[seq_len(n)])
      morceaux[[length(morceaux) + 1]] <- m
      produit <- produit + length(m)
    }
    if (is.na(fin) || codes[fin] == 257L || produit >= attendu) break
    pos <- p[fin] + LARGEURS_LZW[fin]
  }
  as.raw(unlist(morceaux))
}

# Le texte des codes d'un morceau (entre deux codes 256), en entiers 0..255.
morceau_lzw <- function(codes) {
  n <- length(codes)
  if (codes[1] > 255L || (n > 1 && any(codes[-1] > 257L + seq_len(n - 1L)))) stop(traduire("bloc LZW abîmé.", "damaged LZW block."), call. = FALSE)
  M <- 257L + n  # (les codes 0 à 256 + n - 1, rangés en position code + 1)
  parent <- c(0:255, 256L, 257L, integer(n - 1L))
  if (n > 1) parent[258L + seq_len(n - 1L)] <- codes[-n]  # (l'entrée 258 + k - 2, créée au k-ième code, a pour parent le code d'avant)
  # la première lettre (la racine) et la longueur de chaque code, par sauts doublés
  racine <- parent
  distance <- c(rep(0L, 258), rep(1L, n - 1L))
  repeat {
    loin <- racine > 257L
    if (!any(loin)) break
    distance[loin] <- distance[loin] + distance[racine[loin] + 1L]
    racine[loin] <- racine[racine[loin] + 1L]
  }
  longueur <- distance + 1L
  lettre <- c(0:255, 0L, 0L, integer(n - 1L))
  if (n > 1) lettre[258L + seq_len(n - 1L)] <- racine[codes[-1] + 1L]  # (la lettre ajoutée : la première du code suivant)
  # le texte, de la fin de chaque chaîne vers son début : au pas t, la lettre du t-ième parent ;
  # au-delà de 4 pas (les rares chaînes longues), chaque lettre par les ancêtres à 2^b pas
  l <- longueur[codes + 1L]
  fin <- cumsum(l)
  sortie <- integer(fin[n])
  k <- seq_len(n)
  noeud <- codes
  t <- 0L
  repeat {
    sortie[fin[k] - t] <- lettre[noeud + 1L]
    garde <- l[k] > t + 1L
    if (!any(garde)) return(sortie)
    k <- k[garde]
    noeud <- parent[noeud[garde] + 1L]
    t <- t + 1L
    if (t == 4L) break
  }
  reste <- l[k] - t
  quel <- rep.int(seq_along(k), reste)
  d <- sequence(reste) - 1L
  noeud <- noeud[quel]
  saut <- parent
  bit <- 1L
  while (any(d >= bit)) {
    s <- bitwAnd(d, bit) != 0L
    noeud[s] <- saut[noeud[s] + 1L]
    saut <- saut[saut + 1L]
    bit <- bit * 2L
  }
  sortie[fin[k][quel] - t - d] <- lettre[noeud + 1L]
  sortie
}

# PackBits (TIFF 6.0, section 9) : un octet n puis n + 1 octets tels quels (n de 0 à 127), ou
# l'octet suivant répété 257 − n fois (n de 129 à 255) ; 128 ne fait rien.
decompresser_packbits <- function(b) {
  v <- as.integer(b)
  n <- length(v)
  debuts <- integer(n)
  m <- 0L
  i <- 1L
  while (i <= n) {
    m <- m + 1L
    debuts[m] <- i
    h <- v[i]
    i <- i + if (h < 128L) h + 2L else if (h > 128L) 2L else 1L
  }
  debuts <- debuts[seq_len(m)]
  h <- v[debuts]
  telle <- h < 128L
  nombre <- ifelse(telle, h + 1L, ifelse(h > 128L, 257L - h, 0L))
  source <- rep.int(debuts + 1L, nombre) + ifelse(rep.int(telle, nombre), sequence(nombre) - 1L, 0L)
  source <- source[source <= n]
  b[source]
}

decompresser_bloc <- function(b, compression, attendu, nom) {
  r <- tryCatch(switch(as.character(compression),
    "1" = b,
    "5" = decompresser_lzw(b, attendu),
    "8" = , "32946" = memDecompress(b, type = "gzip"),
    "32773" = decompresser_packbits(b),
    "34925" = memDecompress(b, type = "xz"),
    "50000" = memDecompress(b, type = "zstd")),
    error = function(e) stop(sprintf(traduire("%s : un bloc ne se décompresse pas (%s).", "%s: a block can't be decompressed (%s)."), nom, conditionMessage(e)), call. = FALSE))
  if (length(r) < attendu) stop(sprintf(traduire("%s : un bloc est plus court qu'attendu (fichier abîmé ?).", "%s: a block is shorter than expected (damaged file?)."), nom), call. = FALSE)
  r[seq_len(attendu)]
}

# ---------------------------------------------------------------- valeurs d'un bloc

# La somme cumulée le long de chaque ligne d'un bloc (une colonne de `m` par ligne), pas à pas de
# `pas` (les bandes entrelacées : chacune la sienne), modulo `modulo` : le prédicteur 2 sur les
# valeurs, le 3 sur les octets. Une seule somme cumulée sur tout le bloc, de laquelle on retire
# celle d'avant chaque ligne (des valeurs de moins de 16 bits : exacte en réels).
cumul_lignes <- function(m, pas, modulo) {
  L <- nrow(m)
  reste <- function(x) {  # (modulo une puissance de 2 : un masque sur des entiers quand ils tiennent)
    if (!is.finite(modulo)) return(x)
    if (length(x) && max(x) < 2^31) array(bitwAnd(as.integer(x), as.integer(modulo - 1)), dim(x)) else x %% modulo
  }
  if (L < 2) return(reste(m))
  cumul <- function(x, longueur) {
    cs <- cumsum(as.numeric(x))
    cs - rep(c(0, cs[seq_len(length(cs) %/% longueur - 1L) * longueur]), each = longueur)
  }
  if (pas == 1) return(reste(matrix(cumul(m, L), L)))
  a <- aperm(array(m, c(pas, L %/% pas, ncol(m))), c(2, 1, 3))  # (chaque bande de chaque ligne d'un seul tenant)
  a[] <- cumul(a, L %/% pas)
  reste(array(aperm(a, c(2, 1, 3)), dim(m)))
}

# Les valeurs d'un bloc décompressé `b` (raw) de `lignes` lignes de `par_ligne` échantillons
# (cellules × bandes entrelacées), en réels, dans l'ordre du fichier ; `pas` : les bandes
# entrelacées dans le bloc (1 pour des bandes séparées).
valeurs_bloc_raster <- function(b, type, fin, predicteur, lignes, par_ligne, pas) {
  octets <- switch(type, uint8 = , int8 = 1, uint16 = , int16 = 2, uint32 = , int32 = , float32 = 4, float64 = 8)
  if (predicteur == 3) {
    # virgule flottante (libtiff, fpAcc) : les octets cumulés pas à pas, puis chaque ligne rangée par
    # plans d'octets, l'octet de poids fort d'abord, quel que soit l'ordre du fichier
    rb <- par_ligne * octets
    m <- cumul_lignes(matrix(as.integer(b), nrow = rb), pas, 256L)
    ordre <- as.vector(t(matrix(seq_len(rb), nrow = par_ligne)))
    return(readBin(as.raw(m[ordre, ]), "double", n = lignes * par_ligne, size = octets, endian = "big"))
  }
  flottant <- type %in% c("float32", "float64")
  signe <- startsWith(type, "int")
  if (predicteur == 2) {
    # différences horizontales : les valeurs entières (le motif binaire d'un réel) cumulées pas à pas,
    # modulo 2^bits ; sur 32 et 64 bits, par morceaux de 16 bits et leurs retenues, exactement
    if (octets <= 2) {
      u <- as.vector(cumul_lignes(matrix(entiers_bruts(b, octets, fin), nrow = par_ligne), pas, 2^(8 * octets)))
      if (signe) u[u >= 2^(8 * octets - 1)] <- u[u >= 2^(8 * octets - 1)] - 2^(8 * octets)
      return(u)
    }
    h <- matrix(as.numeric(readBin(b, "integer", n = length(b) %/% 2, size = 2, signed = FALSE, endian = fin)), nrow = octets / 2)
    if (fin == "big") h <- h[nrow(h):1, , drop = FALSE]  # (la première ligne : les 16 bits de poids faible)
    retenue <- 0
    for (q in seq_len(nrow(h))) {
      s <- as.vector(cumul_lignes(matrix(h[q, ], nrow = par_ligne), pas, Inf)) + retenue
      h[q, ] <- s %% 65536
      retenue <- s %/% 65536
    }
    if (flottant) return(readBin(writeBin(as.integer(as.vector(h)), raw(), size = 2, endian = "little"), "double", n = ncol(h), size = octets, endian = "little"))
    u <- h[1, ] + 65536 * h[2, ]
    if (signe) u[u >= 2^31] <- u[u >= 2^31] - 2^32
    return(u)
  }
  if (flottant) return(readBin(b, "double", n = length(b) %/% octets, size = octets, endian = fin))
  if (octets == 1) return(if (signe) entiers_bruts(b, 1, fin, TRUE) else as.numeric(as.integer(b)))
  if (octets == 2) return(as.numeric(readBin(b, "integer", n = length(b) %/% 2, size = 2, signed = signe, endian = fin)))
  entiers_bruts(b, 4, fin, signe)
}

# Les valeurs des cellules (lignes i, colonnes j, base 0, dans la grille) de chaque bande : une
# matrice d'une ligne par cellule demandée. D'un GeoTIFF, chaque bloc utile est lu une fois (d'un
# fichier sans compression, seulement les lignes utiles du bloc) ; la valeur manquante et les NaN
# deviennent NA, puis l'échelle et le décalage s'appliquent.
valeurs_cellules <- function(g, i, j) {
  n <- length(i)
  sortie <- matrix(NA_real_, n, g$bandes)
  if (!n) return(sortie)
  if (identical(g$format, "ASCII")) {
    lignes <- sort(unique(i))
    M <- lignes_asc(g, lignes)
    sortie[, 1] <- M[cbind(match(i, lignes), j + 1)]
  } else {
    tf <- g$tiff
    con <- file(g$chemin, "rb")
    on.exit(close(con))
    bloc <- (i %/% tf$bh) * tf$en_travers + j %/% tf$bl  # (le bloc de chaque cellule dans un plan, base 0)
    li <- i %% tf$bh
    ci <- j %% tf$bl
    pas <- if (tf$planaire) 1 else g$bandes
    plans <- if (tf$planaire) seq_len(g$bandes) else 1
    for (k in unique(bloc)) {
      dans <- which(bloc == k)
      lignes_bloc <- if (tf$tuiles) tf$bh else min(tf$bh, g$hauteur - k * tf$bh)
      par_ligne <- tf$bl * pas
      for (plan in plans) {
        numero <- k + (plan - 1) * tf$par_plan + 1
        if (tf$offsets[numero] == 0 || tf$comptes[numero] == 0) {
          # (un bloc absent, celui d'un fichier « creux » de GDAL : sa valeur manquante, ou 0)
          de <- 0
          v <- rep(if (is.na(g$nodata)) 0 else g$nodata, lignes_bloc * par_ligne)
        } else if (tf$compression == 1) {
          # sans compression : de la première à la dernière ligne utile du bloc
          de <- min(li[dans])
          a <- max(li[dans])
          seek(con, tf$offsets[numero] + de * par_ligne * g$octets)
          b <- readBin(con, "raw", (a - de + 1) * par_ligne * g$octets)
          if (length(b) < (a - de + 1) * par_ligne * g$octets) fichier_abime(g$chemin)
          v <- valeurs_bloc_raster(b, g$type, tf$fin, 1, a - de + 1, par_ligne, pas)
        } else {
          de <- 0
          seek(con, tf$offsets[numero])
          b <- readBin(con, "raw", tf$comptes[numero])
          b <- decompresser_bloc(b, tf$compression, lignes_bloc * par_ligne * g$octets, g$nom)
          v <- valeurs_bloc_raster(b, g$type, tf$fin, tf$predicteur, lignes_bloc, par_ligne, pas)
        }
        position <- ((li[dans] - de) * tf$bl + ci[dans]) * pas
        if (tf$planaire) sortie[dans, plan] <- v[position + 1]
        else for (s in seq_len(g$bandes)) sortie[dans, s] <- v[position + s]
      }
    }
  }
  # la valeur manquante (pour des réels de 32 bits, arrondie comme eux, comme GDAL) et les NaN
  nd <- g$nodata
  if (!is.na(nd) && identical(g$type, "float32") && is.finite(nd)) nd <- readBin(writeBin(nd, raw(), size = 4), "double", size = 4)
  if (is.nan(nd)) sortie[is.nan(sortie)] <- NA else if (!is.na(nd)) sortie[!is.na(sortie) & sortie == nd] <- NA
  sortie[is.nan(sortie)] <- NA
  for (s in seq_len(g$bandes)) if (g$echelle[s] != 1 || g$decalage[s] != 0) sortie[, s] <- sortie[, s] * g$echelle[s] + g$decalage[s]
  sortie
}

# ---------------------------------------------------------------- ouvrir un fichier

# Un raster ouvert (GeoTIFF ou grille ASCII), d'après son extension, ou, pour une autre, ses
# premiers octets (« II* », « MM* »).
ouvrir_raster <- function(chemin) {
  chemin <- as.character(chemin)[1]
  if (!nzchar(chemin) || !file.exists(chemin) || dir.exists(chemin)) stop(sprintf(traduire("Fichier introuvable : %s", "File not found: %s"), chemin), call. = FALSE)
  ext <- tolower(tools::file_ext(chemin))
  if (ext == "asc") return(ouvrir_asc(chemin))
  if (ext %in% c("tif", "tiff", "gtif", "gtiff")) return(ouvrir_geotiff(chemin))
  tete <- readBin(chemin, "raw", 4)
  if (length(tete) >= 4 && (identical(tete[1:2], as.raw(c(0x49, 0x49))) || identical(tete[1:2], as.raw(c(0x4d, 0x4d))))) return(ouvrir_geotiff(chemin))
  stop(sprintf(traduire("%s : Nuée lit les rasters GeoTIFF (.tif) et les grilles ASCII d'ESRI (.asc). Pour un autre format, exportez-le en GeoTIFF dans QGIS (clic droit sur la couche › Exporter › Enregistrer sous…).",
                        "%s: Nuée reads GeoTIFF rasters (.tif) and ESRI ASCII grids (.asc). For another format, export it as GeoTIFF in QGIS (right-click on the layer › Export › Save As…)."),
               basename(chemin)), call. = FALSE)
}

# ---------------------------------------------------------------- valeurs aux points

# La position des points (x, y, dans le système du raster) dans la grille : u le long des
# colonnes, v le long des lignes, en cellules depuis le coin haut-gauche (l'inverse de gt).
position_grille <- function(g, x, y) {
  gt <- g$gt
  if (gt[3] == 0 && gt[5] == 0) return(list(u = (x - gt[1]) / gt[2], v = (y - gt[4]) / gt[6]))
  d <- gt[2] * gt[6] - gt[3] * gt[5]
  dx <- x - gt[1]
  dy <- y - gt[4]
  list(u = (gt[6] * dx - gt[3] * dy) / d, v = (gt[2] * dy - gt[5] * dx) / d)
}

# Le centre des cellules (i, j) en coordonnées du raster.
centre_cellule <- function(g, i, j) {
  gt <- g$gt
  list(x = gt[1] + (j + 0.5) * gt[2] + (i + 0.5) * gt[3], y = gt[4] + (j + 0.5) * gt[5] + (i + 0.5) * gt[6])
}

geographique <- function(g) !is.null(g$systeme) && !is.null(g$systeme$code) && !is.na(g$systeme$code) && identical(systeme_coordonnees(g$systeme$code)$type, "lonlat")

# Des distances en mètres entre des points et des centres de cellules : dans le plan d'un système
# projeté ; pour des longitudes et latitudes, sur l'ellipsoïde WGS 84 par ses rayons de courbure à
# la latitude moyenne (à moins d'un millionième près jusqu'à une dizaine de kilomètres).
distances_raster <- function(geo, x1, y1, x2, y2) {
  if (!geo) return(sqrt((x2 - x1)^2 + (y2 - y1)^2))
  el <- ELLIPSOIDES$WGS84
  e2 <- el$f * (2 - el$f)
  phi <- rad((y1 + y2) / 2)
  w <- 1 - e2 * sin(phi)^2
  M <- el$a * (1 - e2) / w^1.5
  N <- el$a / sqrt(w)
  sqrt((M * rad(y2 - y1))^2 + (N * cos(phi) * rad(x2 - x1))^2)
}

# Les valeurs d'un raster aux points (x, y), dans son système, par `methode` :
#   "cellule"     la cellule qui contient le point (ses bords compris, comme terra) ;
#   "bilineaire"  l'interpolation des quatre centres de cellules qui l'entourent, comme
#                 terra::extract(method = "bilinear") : la cellule du point et sa voisine du côté
#                 du point en colonne puis en ligne (au bord, la cellule elle-même) ; le long de
#                 chaque ligne, puis entre les deux lignes, une valeur manquante prend la place de
#                 l'autre ;
#   "moyenne", "majorite"  les cellules dont le centre est à moins de `rayon` mètres du point
#                 (aucune : celle du point), rendues une à une (resume_cercles en fait la moyenne ou
#                 la classe majoritaire, celle de la cellule du point en cas d'égalité, sinon la plus
#                 petite).
# Rend `valeurs` (une ligne par point, une colonne par bande ; pour un cercle, une liste par point
# de matrices des valeurs de ses cellules), `dedans` (le point dans le raster) et `propre` (la
# valeur de sa cellule).
valeurs_points <- function(g, x, y, methode = "cellule", rayon = NA) {
  n <- length(x)
  pg <- position_grille(g, x, y)
  W <- g$largeur
  H <- g$hauteur
  u <- pg$u
  v <- pg$v
  dedans <- is.finite(u) & is.finite(v) & u >= 0 & u <= W & v >= 0 & v <= H
  j <- pmin(floor(u), W - 1)
  i <- pmin(floor(v), H - 1)
  propre <- matrix(NA_real_, n, g$bandes)
  if (methode %in% c("cellule", "bilineaire")) {
    if (methode == "cellule") {
      propre[dedans, ] <- valeurs_cellules(g, i[dedans], j[dedans])
      return(list(valeurs = propre, dedans = dedans, propre = propre))
    }
    k <- which(dedans)
    du <- u[k] - (j[k] + 0.5)
    dv <- v[k] - (i[k] + 0.5)
    # (la voisine à droite si le point est à droite du centre, sinon à gauche ; au-dessus s'il est
    # au-dessus du centre ou à sa hauteur, sinon en dessous : comme terra, ce qui ne compte qu'au
    # centre même d'une cellule sans valeur)
    j2 <- ifelse(du > 0, j[k] + 1, j[k] - 1)
    i2 <- ifelse(dv <= 0, i[k] - 1, i[k] + 1)
    wx <- abs(du)
    wy <- abs(dv)
    hors_j <- j2 < 0 | j2 > W - 1
    hors_i <- i2 < 0 | i2 > H - 1
    j2[hors_j] <- j[k][hors_j]
    i2[hors_i] <- i[k][hors_i]
    # les quatre cellules, chacune lue une fois
    cles <- c(i[k] * W + j[k], i[k] * W + j2, i2 * W + j[k], i2 * W + j2)
    uniques <- unique(cles)
    vu <- valeurs_cellules(g, uniques %/% W, uniques %% W)
    m <- length(k)
    q <- function(a) vu[match(cles[(a - 1) * m + seq_len(m)], uniques), , drop = FALSE]
    v11 <- q(1); v12 <- q(2); v21 <- q(3); v22 <- q(4)
    entre <- function(a, b, w) {
      r <- a * (1 - w) + b * w
      r[is.na(a)] <- b[is.na(a)]
      r[is.na(b)] <- a[is.na(b)]
      r
    }
    valeurs <- matrix(NA_real_, n, g$bandes)
    valeurs[k, ] <- entre(entre(v11, v12, wx), entre(v21, v22, wx), wy)
    propre[k, ] <- v11
    return(list(valeurs = valeurs, dedans = dedans, propre = propre))
  }
  # dans un cercle : les cellules du carré qui l'entoure, puis celles dont le centre est à moins du rayon
  geo <- geographique(g)
  propre[dedans, ] <- valeurs_cellules(g, i[dedans], j[dedans])
  taille <- c(sqrt(g$gt[2]^2 + g$gt[5]^2), sqrt(g$gt[3]^2 + g$gt[6]^2))
  cercles <- vector("list", n)
  demandes <- list()
  for (p in which(is.finite(x) & is.finite(y))) {
    # (le rayon en cellules : en degrés pour des longitudes et latitudes, par les rayons de courbure)
    portee <- if (geo) {
      el <- ELLIPSOIDES$WGS84
      e2 <- el$f * (2 - el$f)
      w <- 1 - e2 * sin(rad(y[p]))^2
      c(deg(rayon / (el$a / sqrt(w) * max(cos(rad(y[p])), 1e-6))), deg(rayon / (el$a * (1 - e2) / w^1.5)))
    } else c(rayon, rayon)
    nc <- ceiling(max(portee) / min(taille)) + 1
    jj <- seq(max(0, floor(u[p]) - nc), min(W - 1, floor(u[p]) + nc))
    ii <- seq(max(0, floor(v[p]) - nc), min(H - 1, floor(v[p]) + nc))
    if (!length(jj) || !length(ii) || jj[1] > jj[length(jj)] || ii[1] > ii[length(ii)]) next
    grille <- expand.grid(j = jj, i = ii)
    ce <- centre_cellule(g, grille$i, grille$j)
    d <- distances_raster(geo, x[p], y[p], ce$x, ce$y)
    garde <- d <= rayon
    if (!any(garde) && dedans[p]) garde <- grille$i == i[p] & grille$j == j[p]
    if (any(garde)) demandes[[as.character(p)]] <- grille$i[garde] * W + grille$j[garde]
  }
  if (length(demandes)) {
    toutes <- unique(unlist(demandes))
    vu <- valeurs_cellules(g, toutes %/% W, toutes %% W)
    for (p in names(demandes)) cercles[[as.integer(p)]] <- vu[match(demandes[[p]], toutes), , drop = FALSE]
  }
  list(valeurs = cercles, dedans = dedans, propre = propre)
}

# La moyenne ou la classe majoritaire des cellules d'un cercle, pour la bande `bande` : NA sans
# cellule ou sans valeur ; à égalité, la classe de la cellule du point si elle en est, sinon la plus
# petite.
resume_cercles <- function(cercles, propre, methode, bande) {
  vapply(seq_along(cercles), function(p) {
    v <- cercles[[p]]
    if (is.null(v)) return(NA_real_)
    v <- v[, bande]
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_real_)
    if (methode == "moyenne") return(mean(v))
    t <- table(v)
    gagnantes <- as.numeric(names(t)[t == max(t)])
    if (!is.na(propre[p]) && propre[p] %in% gagnantes) propre[p] else min(gagnantes)
  }, 0)
}

# ---------------------------------------------------------------- dans le projet

METHODES_RASTER <- c("cellule", "bilineaire", "moyenne", "majorite")

# La méthode proposée pour un raster : la cellule pour des catégories (une palette, des entiers de
# 8 bits, une grille ASCII d'entiers), l'interpolation bilinéaire pour des valeurs continues.
methode_proposee <- function(g) {
  if (isTRUE(g$palette) || g$type %in% c("uint8", "int8", "texte_entiers")) "cellule" else "bilineaire"
}

# Le nom proposé de chaque bande : la description de la bande (dans les métadonnées de GDAL) ou le
# nom du fichier, numéroté s'il a plusieurs bandes.
noms_bandes <- function(g) {
  base <- tools::file_path_sans_ext(g$nom)
  if (g$bandes == 1) return(if (!is.na(g$noms_bandes[1])) g$noms_bandes[1] else base)
  ifelse(is.na(g$noms_bandes), sprintf("%s_%d", base, seq_len(g$bandes)), g$noms_bandes)
}

# Le raster prêt pour les points du projet (`pts` : points_projet) : son système, celui du
# fichier ; sans système, celui du projet (`suppose`, et la note le dit) ; un projet en coordonnées
# planes sans système ne se convertit pas. Les points dans ce système (des longitudes ramenées à
# 0..360 pour un raster qui va au-delà de 180°).
raster_pour_projet <- function(g, pts) {
  co <- pts$coordonnees
  s <- g$systeme
  note <- NULL
  if (!is.null(s$erreur)) stop(paste0(g$nom, " : ", s$erreur), call. = FALSE)
  if (is.null(s)) {
    sp <- systeme_coordonnees(co$systeme)
    g$systeme <- list(code = if (identical(sp$type, "plan")) NA_integer_ else sp$code, nom = sp$nom, suppose = TRUE)
    note <- sprintf(traduire("%s ne dit pas son système de coordonnées : il est pris dans celui du projet (%s).",
                             "%s doesn't give its coordinate system: it is taken as the project's (%s)."), g$nom, sp$nom)
  } else if (identical(co$systeme, "plan")) {
    stop(sprintf(traduire("%s est en %s, et les coordonnées du projet sont planes, sans système précisé : choisissez leur système dans Graphiques › Carte.",
                          "%s is in %s, and the project's coordinates are plane, without a stated system: choose their system in Plots › Map."), g$nom, s$nom), call. = FALSE)
  }
  q <- if (isTRUE(g$systeme$suppose) || identical(as.character(g$systeme$code), as.character(pts$systeme))) list(x = pts$x, y = pts$y)
       else convertir_coordonnees(pts$x, pts$y, pts$systeme, g$systeme$code)
  if (geographique(g)) {
    xmax <- max(g$gt[1], g$gt[1] + g$largeur * g$gt[2] + g$hauteur * g$gt[3])
    if (xmax > 180) q$x <- ifelse(!is.na(q$x) & q$x < 0, q$x + 360, q$x)
  }
  list(g = g, x = q$x, y = q$y, note = note)
}

# La taille des cellules (le long des colonnes et des lignes ; en degrés pour des longitudes et
# latitudes, avec la latitude du milieu du raster).
taille_cellules <- function(g) {
  list(x = sqrt(g$gt[2]^2 + g$gt[5]^2), y = sqrt(g$gt[3]^2 + g$gt[6]^2), geographique = geographique(g),
       latitude = g$gt[4] + g$hauteur / 2 * g$gt[6] + g$largeur / 2 * g$gt[5])
}

# La taille des cellules en texte : « 25 m », « 25 × 30 m », « 0,008333° (30″, ≈ 670 m) » ; `fr` :
# la virgule décimale.
texte_cellules <- function(tc, fr = !identical(getOption("nuee.langue"), "en")) {
  f <- function(v) format(signif(v, 6), decimal.mark = if (fr) "," else ".", trim = TRUE, scientific = FALSE)
  if (isTRUE(tc$geographique)) {
    metres <- distances_raster(TRUE, 0, tc$latitude, tc$x, tc$latitude)
    sec <- tc$x * 3600
    entieres <- abs(sec - round(sec)) < 1e-6 * max(1, sec) && sec < 3600
    return(sprintf("%s° (%s≈ %s)", f(tc$x), if (entieres) sprintf("%d″, ", as.integer(round(sec))) else "",
                   if (metres >= 1000) paste(f(round(metres / 100) / 10), "km") else paste(f(round(metres)), "m")))
  }
  if (abs(tc$x - tc$y) < 1e-9 * tc$x) paste(f(tc$x), "m") else sprintf("%s × %s m", f(tc$x), f(tc$y))
}

# Ce que l'interface montre de chaque fichier avant d'ajouter ses valeurs (Édition › Ajouter des
# valeurs de rasters) : son format, sa taille, ses cellules, ses bandes et leur type, sa
# compression, son système, la méthode proposée, le nom proposé de chaque bande (et celui qu'il
# prendrait dans le projet : variables.R, nom_libre), combien d'observations y tombent ; ou, pour
# un fichier qu'on ne lit pas, pourquoi (`erreur`), sans empêcher les autres.
infos_rasters <- function(projet, chemins) {
  co <- coordonnees_projet(projet, exiger = TRUE)
  pts <- points_projet(projet)
  pris <- c(as.character(projet$noms_var), as.character(projet$observation_id_names %||% character(0)))
  lapply(as.character(chemins), function(chemin) {
    r <- tryCatch({
      q <- raster_pour_projet(ouvrir_raster(chemin), pts)
      g <- q$g
      pg <- position_grille(g, q$x, q$y)
      dedans <- is.finite(pg$u) & is.finite(pg$v) & pg$u >= 0 & pg$u <= g$largeur & pg$v >= 0 & pg$v <= g$hauteur
      noms <- noms_bandes(g)
      libres <- character(0)
      for (n in noms) libres <- c(libres, nom_libre(n, c(pris, libres)))
      coins <- list(x = g$gt[1] + c(0, g$largeur, 0, g$largeur) * g$gt[2] + c(0, 0, g$hauteur, g$hauteur) * g$gt[3],
                    y = g$gt[4] + c(0, g$largeur, 0, g$largeur) * g$gt[5] + c(0, 0, g$hauteur, g$hauteur) * g$gt[6])
      tf <- g$tiff
      list(chemin = chemin, nom = g$nom, taille = g$taille, format = g$format, largeur = g$largeur, hauteur = g$hauteur, bandes = g$bandes,
           type = g$type, compression = if (!is.null(tf)) tf$nom_compression else "", predicteur = if (!is.null(tf)) tf$predicteur else 1,
           organisation = if (is.null(tf)) "" else if (tf$tuiles) sprintf(traduire("tuiles de %d × %d", "%d × %d tiles"), as.integer(tf$bl), as.integer(tf$bh))
                          else sprintf(traduire("bandes de %d ligne(s)", "strips of %d row(s)"), as.integer(tf$bh)),
           bandes_separees = isTRUE(tf$planaire), resolution = texte_cellules(taille_cellules(g)),
           etendue = list(xmin = min(coins$x), xmax = max(coins$x), ymin = min(coins$y), ymax = max(coins$y)),
           systeme = list(code = g$systeme$code %||% NA, nom = g$systeme$nom %||% "", suppose = isTRUE(g$systeme$suppose)),
           nodata = if (is.na(g$nodata)) NULL else g$nodata, palette = isTRUE(g$palette), pixel_point = isTRUE(g$pixel_point),
           methode = methode_proposee(g), noms = I(noms), noms_libres = I(libres),
           dedans = sum(dedans, na.rm = TRUE), observations = sum(is.finite(pts$x) & is.finite(pts$y)), total = length(pts$x),
           notes = I(c(g$notes, q$note,
                       if (isTRUE(g$palette)) traduire("Une palette de couleurs : les numéros des classes sont lus, pas leurs couleurs.", "A colour palette: the class numbers are read, not their colours."),
                       if (g$images > 1) traduire("D'autres images du fichier (aperçus, masques) sont laissées de côté : les valeurs viennent de la pleine résolution.", "Other images in the file (overviews, masks) are left aside: the values come from the full resolution."),
                       if (isTRUE(g$pixel_point)) traduire("PixelIsPoint : les coordonnées du fichier sont celles du centre des cellules, comme GDAL les lit.", "PixelIsPoint: the file's coordinates are the cells' centres, as GDAL reads them."))))
    }, error = function(e) list(chemin = chemin, nom = basename(chemin), taille = if (file.exists(chemin)) file.size(chemin) else NA, erreur = conditionMessage(e)))
    r
  })
}

# Les valeurs de rasters ajoutées au projet, comme de nouvelles variables : `fichiers`, une liste de
# list(chemin, methode, rayon (en mètres, pour "moyenne" et "majorite"), noms : un par bande). Des
# bandes de fichiers différents qui reçoivent le même nom font une seule variable, comme les
# tuiles d'un même raster : chaque observation prend la valeur du premier fichier qui la contient
# (dans un cercle, les cellules de tous les fichiers). Un nom déjà pris dans le projet devient
# « nom (2) » (variables.R : nom_libre). Chaque variable garde d'où elle vient (`origine` : le
# fichier, la bande, la méthode, le rayon, le système et la taille des cellules), que le texte des
# Méthodes reprend. Les observations sans coordonnées, hors du raster ou sur une cellule sans valeur
# ont une valeur manquante, et sont comptées. Rend le projet ; attr(, "raster") : par variable, ces
# comptes et les phrases qui les disent.
ajouter_valeurs_rasters <- function(projet, fichiers) {
  if (!length(fichiers)) stop(traduire("Choisissez au moins un fichier raster.", "Choose at least one raster file."), call. = FALSE)
  pts <- points_projet(projet)
  avec_coordonnees <- is.finite(pts$x) & is.finite(pts$y)
  if (!any(avec_coordonnees)) stop(traduire("Aucune observation n'a de coordonnées.", "No observation has coordinates."), call. = FALSE)
  # chaque (fichier, bande) sous son nom
  membres <- list()
  for (f in fichiers) {
    methode <- as.character(f$methode %||% "")[1]
    if (!methode %in% METHODES_RASTER) methode <- ""
    g <- ouvrir_raster(f$chemin)
    if (!nzchar(methode)) methode <- methode_proposee(g)
    rayon <- suppressWarnings(as.numeric(f$rayon %||% NA)[1])
    if (methode %in% c("moyenne", "majorite") && (is.na(rayon) || rayon <= 0)) {
      stop(sprintf(traduire("%s : donnez le rayon du cercle, en mètres.", "%s: give the circle's radius, in metres."), g$nom), call. = FALSE)
    }
    if (!methode %in% c("moyenne", "majorite")) rayon <- NA_real_
    if (!is.na(rayon) && rayon > 50000) stop(sprintf(traduire("%s : un rayon de plus de 50 km ne se calcule pas ici.", "%s: a radius over 50 km isn't computed here."), g$nom), call. = FALSE)
    noms <- trimws(as.character(unlist(f$noms %||% noms_bandes(g))))
    if (length(noms) != g$bandes) stop(sprintf(traduire("%s a %d bande(s) : donnez un nom à chacune.", "%s has %d band(s): give each one a name."), g$nom, as.integer(g$bandes)), call. = FALSE)
    if (any(!nzchar(noms))) stop(sprintf(traduire("%s : donnez un nom à chaque bande.", "%s: give each band a name."), g$nom), call. = FALSE)
    q <- raster_pour_projet(g, pts)
    for (b in seq_len(g$bandes)) membres[[length(membres) + 1]] <- list(g = q$g, bande = b, nom = noms[b], methode = methode, rayon = rayon, x = q$x, y = q$y, note = q$note)
  }
  groupes <- split(seq_along(membres), factor(vapply(membres, `[[`, "", "nom"), levels = unique(vapply(membres, `[[`, "", "nom"))))
  for (gr in groupes) {
    m <- membres[gr]
    if (length(unique(vapply(m, `[[`, "", "methode"))) > 1 || length(unique(vapply(m, function(x) as.character(x$rayon), ""))) > 1) {
      stop(sprintf(traduire("Les fichiers joints sous le nom « %s » doivent avoir la même méthode et le même rayon.", "The files joined under the name “%s” must have the same method and radius."), m[[1]]$nom), call. = FALSE)
    }
  }
  # les valeurs, fichier par fichier (chacun lu une fois pour toutes ses bandes)
  extraits <- list()
  for (k in seq_along(membres)) {
    m <- membres[[k]]
    cle <- paste(m$g$chemin, m$methode, m$rayon)
    if (is.null(extraits[[cle]])) extraits[[cle]] <- valeurs_points(m$g, m$x, m$y, m$methode, m$rayon)
  }
  obs <- pts$uid
  n <- length(obs)
  pris <- c(as.character(projet$noms_var), as.character(projet$observation_id_names %||% character(0)))
  resultats <- list()
  for (gr in groupes) {
    m <- membres[gr]
    methode <- m[[1]]$methode
    valeur <- rep(NA_real_, n)
    dedans <- rep(FALSE, n)
    propre <- rep(NA_real_, n)
    if (methode %in% c("cellule", "bilineaire")) {
      for (x in m) {
        e <- extraits[[paste(x$g$chemin, x$methode, x$rayon)]]
        prendre <- is.na(valeur) & e$dedans & !is.na(e$valeurs[, x$bande])
        valeur[prendre] <- e$valeurs[prendre, x$bande]
        dedans <- dedans | e$dedans
      }
    } else {
      cercles <- vector("list", n)
      for (x in m) {
        e <- extraits[[paste(x$g$chemin, x$methode, x$rayon)]]
        for (p in seq_len(n)) if (!is.null(e$valeurs[[p]])) cercles[[p]] <- rbind(cercles[[p]], e$valeurs[[p]][, x$bande, drop = FALSE])
        prendre <- is.na(propre) & !is.na(e$propre[, x$bande])
        propre[prendre] <- e$propre[prendre, x$bande]
        dedans <- dedans | !vapply(e$valeurs, is.null, TRUE)
      }
      valeur <- resume_cercles(cercles, propre, methode, 1)
    }
    nom <- nom_libre(m[[1]]$nom, pris)
    pris <- c(pris, nom)
    g1 <- m[[1]]$g
    tc <- taille_cellules(g1)
    origine <- list(type = "raster", fichiers = I(vapply(m, function(x) x$g$nom, "")), chemins = I(vapply(m, function(x) x$g$chemin, "")),
                    bandes = I(vapply(m, function(x) as.numeric(x$bande), 0)), methode = methode, rayon = m[[1]]$rayon,
                    format = g1$format, systeme = g1$systeme$nom %||% "", code = g1$systeme$code %||% NA, systeme_suppose = isTRUE(g1$systeme$suppose),
                    cellule_x = tc$x, cellule_y = tc$y, geographique = tc$geographique, latitude = tc$latitude)
    projet <- ajouter_colonne(projet, nom, obs, valeur)
    k <- which(vapply(projet$variables, function(v) identical(as.character(v$ids[1]), nom), TRUE))[1]
    projet$variables[[k]]$origine <- origine
    resultats[[length(resultats) + 1]] <- list(nom = nom, demande = m[[1]]$nom, fichiers = I(origine$fichiers), methode = methode, rayon = m[[1]]$rayon,
                                               valeurs = sum(!is.na(valeur)), sans_coordonnees = sum(!avec_coordonnees),
                                               hors = sum(avec_coordonnees & !dedans), sans_valeur = sum(avec_coordonnees & dedans & is.na(valeur)),
                                               note = unique(unlist(lapply(m, `[[`, "note"))))
  }
  attr(projet, "raster") <- list(variables = resultats, phrases = phrases_raster(resultats, n))
  projet
}

# Ce que l'ajout a donné, en phrases : chaque variable, ses valeurs et ses manques.
phrases_raster <- function(resultats, n) {
  unique(c(unlist(lapply(resultats, function(r) {
    manques <- c(if (r$hors) sprintf(traduire("%d hors du raster", "%d outside the raster"), r$hors),
                 if (r$sans_valeur) sprintf(traduire("%d sur une cellule sans valeur", "%d on a cell without a value"), r$sans_valeur),
                 if (r$sans_coordonnees) sprintf(traduire("%d sans coordonnées", "%d without coordinates"), r$sans_coordonnees))
    c(sprintf(traduire("« %s »%s : %d observation(s) sur %d ont une valeur%s.", "“%s”%s: %d of the %d observations have a value%s."), r$nom,
              if (r$nom != r$demande) sprintf(traduire(" (« %s » était déjà pris)", " (“%s” was already taken)"), r$demande) else "",
              r$valeurs, n, if (length(manques)) paste0(traduire(", manquante pour ", ", missing for "), paste(manques, collapse = ", ")) else ""),
      r$note)
  }))))
}

# ---------------------------------------------------------------- le texte des Méthodes

# La phrase qui dit d'où viennent les variables d'un modèle lues dans des rasters (leur `origine`,
# ajouter_valeurs_rasters) ; NULL s'il n'en utilise pas. `uids` : les variables du modèle.
phrase_methodes_raster <- function(projet, uids, fr) {
  vars <- Filter(function(v) as.numeric(v$uid) %in% uids && identical(v$origine$type, "raster"), projet$variables)
  if (!length(vars)) return(NULL)
  liste <- function(x) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), if (fr) "et" else "and", x[length(x)])
  g <- function(x) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)
  nombre <- function(x) format(x, decimal.mark = if (fr) "," else ".", big.mark = if (fr) " " else ",", trim = TRUE)
  parties <- vapply(vars, function(v) {
    o <- v$origine
    methode <- switch(as.character(o$methode),
      cellule = if (fr) "la valeur de la cellule qui contient chaque observation" else "the value of the cell holding each observation",
      bilineaire = if (fr) "par interpolation bilinéaire des quatre cellules les plus proches" else "by bilinear interpolation of the four nearest cells",
      moyenne = sprintf(if (fr) "la moyenne des cellules à moins de %s m de chaque observation" else "the mean of the cells within %s m of each observation", nombre(o$rayon)),
      majorite = sprintf(if (fr) "la classe majoritaire des cellules à moins de %s m de chaque observation" else "the majority class of the cells within %s m of each observation", nombre(o$rayon)),
      "")
    fichiers <- as.character(unlist(o$fichiers))
    cellules <- texte_cellules(list(x = as.numeric(o$cellule_x), y = as.numeric(o$cellule_y), geographique = isTRUE(o$geographique), latitude = as.numeric(o$latitude)), fr)
    sprintf(if (fr) "%s de %s (%s, cellules de %s, %s ; %s)" else "%s from %s (%s, %s cells, %s; %s)", g(as.character(v$ids[1])),
            if (length(fichiers) > 3) sprintf(if (fr) "%d fichiers (%s…)" else "%d files (%s…)", length(fichiers), fichiers[1]) else liste(fichiers),
            if (identical(o$format, "ASCII")) (if (fr) "grille ASCII" else "ASCII grid") else o$format, cellules,
            if (isTRUE(o$systeme_suppose)) (if (fr) "dans le système du projet" else "in the project's system") else o$systeme, methode)
  }, "")
  if (fr) sprintf("Lues dans des rasters à la position de chaque observation : %s.", liste(parties))
  else sprintf("Taken from rasters at each observation's position: %s.", liste(parties))
}
