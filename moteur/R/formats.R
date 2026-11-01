# Tableaux venant d'autres logiciels, et tableaux écrits pour eux.
#
# Lecture : lire_tableau() fait d'un fichier un data.frame (une ligne par observation, des
# facteurs pour les variables qualitatives), dont importer_tableau() (nouveau.R) fait un projet.
#   .csv .txt .tsv .tab .dat  texte délimité : séparateur, décimale et encodage reconnus
#   .xlsx .xlsm .xls          Excel, une feuille au choix
#   .rds .rdata .rda          R, un tableau au choix (data.frame ou matrice)
#   .sav .zsav .por           SPSS             (paquet haven)
#   .dta                      Stata            (haven)
#   .sas7bdat .xpt            SAS              (haven)
#   .omv                      jamovi           (lu ici : lire_omv)
#   .dbf                      dBase            (lu par foreign : lire_dbf ; écrit ici)
#   .arff                     Weka             (lu ici : lire_arff ; écrit par foreign)
#   .jdx .dx .jcm .spc        spectres JCAMP-DX et Galactic (fichiers_spectres.R ; lecture seule)
# Écriture : ecrire_tableau() écrit un data.frame dans l'un de ces formats (sauf .xls, .por,
# .sas7bdat, que les logiciels lisent mal quand un autre programme les écrit).

FORMATS_LECTURE <- c("csv", "txt", "tsv", "tab", "dat", "xlsx", "xlsm", "xls", "rds", "rdata", "rda",
                     "sav", "zsav", "por", "dta", "sas7bdat", "xpt", "omv", "dbf", "arff",
                     "jdx", "dx", "jcm", "spc")
FORMATS_ECRITURE <- c("xlsx", "csv", "tsv", "txt", "rds", "rdata", "sav", "dta", "xpt", "omv", "dbf", "arff")

extension <- function(chemin) tolower(tools::file_ext(chemin))

# ---------------------------------------------------------------- lecture

# Les tableaux entre lesquels choisir dans un fichier qui peut en avoir plusieurs (feuilles
# d'un classeur Excel, objets d'un .RData, liste de tableaux dans un .rds) ; NULL sinon.
tables_fichier <- function(chemin) {
  switch(extension(chemin),
         xlsx = , xlsm = openxlsx::getSheetNames(chemin),
         xls = readxl::excel_sheets(chemin),
         rdata = , rda = names(objets_r(chemin)),
         rds = { x <- readRDS(chemin); if (est_tableau(x)) NULL else names(Filter(est_tableau, as.list(x))) },
         NULL)
}

# Le tableau d'un fichier, en data.frame. `table` : nom de la feuille ou de l'objet (le
# premier sinon).
lire_tableau <- function(chemin, table = NULL) {
  ext <- extension(chemin)
  un_parmi <- function(objets) {
    if (!length(objets)) stop(traduire("Ce fichier ne contient aucun tableau (data.frame ou matrice)", "This file holds no table (data frame or matrix)"))
    if (is.null(table)) return(objets[[1]])
    objets[[table]] %||% stop(traduire("Tableau introuvable dans le fichier : ", "Table not found in the file: "), table)
  }
  x <- switch(ext,
    csv = , txt = , tsv = , tab = , dat = lire_texte(chemin),
    xlsx = , xlsm = openxlsx::read.xlsx(chemin, sheet = table %||% 1, check.names = FALSE),
    xls = readxl::read_xls(chemin, sheet = table %||% 1, .name_repair = "minimal"),
    rds = { x <- readRDS(chemin); if (est_tableau(x)) x else un_parmi(Filter(est_tableau, as.list(x))) },
    rdata = , rda = un_parmi(objets_r(chemin)),
    sav = , zsav = haven::read_sav(chemin),
    por = haven::read_por(chemin),
    dta = haven::read_dta(chemin),
    sas7bdat = haven::read_sas(chemin),
    xpt = haven::read_xpt(chemin),
    omv = lire_omv(chemin),
    dbf = lire_dbf(chemin),
    arff = lire_arff(chemin),
    jdx = , dx = , jcm = , spc = tableau_spectres(lire_spectres(chemin)),
    stop(traduire("Format de fichier non reconnu : .", "Unrecognised file format: ."), ext))
  vers_data_frame(x)
}

est_tableau <- function(x) is.data.frame(x) || (is.matrix(x) && (is.numeric(x) || is.character(x) || is.logical(x)))

# Les tableaux d'un fichier .RData, par nom. Lire un fichier .rds ou .RData n'exécute pas de
# code depuis R 4.4.
objets_r <- function(chemin) {
  e <- new.env()
  load(chemin, envir = e)
  Filter(est_tableau, mget(sort(ls(e)), envir = e))
}

# Texte délimité. L'encodage (UTF-8, Windows-1252 des fichiers d'Excel, UTF-16 du « texte
# Unicode » d'Excel), puis le séparateur, d'après la première ligne : tabulation, point-virgule,
# virgule, sinon des espaces. Chaque colonne devient numérique si toutes ses valeurs sont des
# nombres, avec un point ou une virgule décimale (« 12,5 », « 1 234,5 »).
lire_texte <- function(chemin) lire_texte_delimite(texte_fichier(chemin))

# Le texte d'un fichier, en UTF-8, quel que soit son encodage.
texte_fichier <- function(chemin) {
  octets <- readBin(chemin, "raw", file.size(chemin))
  if (length(octets) >= 2 && octets[1] == as.raw(0xff) && octets[2] == as.raw(0xfe)) {
    texte <- iconv(list(octets[-(1:2)]), "UTF-16LE", "UTF-8")
  } else {
    if (length(octets) >= 3 && all(octets[1:3] == as.raw(c(0xef, 0xbb, 0xbf)))) octets <- octets[-(1:3)]
    texte <- rawToChar(octets[octets != as.raw(0)])
    if (!validUTF8(texte)) texte <- iconv(texte, "CP1252", "UTF-8")
  }
  Encoding(texte) <- "UTF-8"
  texte
}

lire_texte_delimite <- function(texte) {
  premiere <- sub("[\r\n].*$", "", sub("^[\r\n]+", "", texte))
  compter <- function(s) lengths(regmatches(premiere, gregexpr(s, premiere, fixed = TRUE)))
  n <- c("\t" = compter("\t"), ";" = compter(";"), "," = compter(","))
  sep <- if (max(n) > 0) names(n)[which.max(n)] else ""
  df <- utils::read.table(text = texte, sep = sep, header = TRUE, quote = "\"", colClasses = "character",
                          na.strings = c("", "NA"), check.names = FALSE, comment.char = "", fill = TRUE,
                          strip.white = TRUE, blank.lines.skip = TRUE, encoding = "UTF-8")
  df[] <- lapply(df, convertir_colonne)
  df
}

convertir_colonne <- function(x) {
  v <- trimws(x)
  connues <- !is.na(v) & nzchar(v)
  if (!any(connues)) return(rep(NA_real_, length(x)))
  nombre <- function(s) suppressWarnings(as.numeric(s))
  if (!anyNA(nombre(v[connues]))) return(nombre(v))
  # virgule décimale, espaces (insécables aussi) entre les milliers
  w <- gsub("(?<=\\d)[   ](?=\\d{3})", "", v, perl = TRUE)
  if (!any(grepl(",.*,", w[connues])) && !anyNA(nombre(sub(",", ".", w[connues], fixed = TRUE)))) {
    return(nombre(sub(",", ".", w, fixed = TRUE)))
  }
  v[!connues] <- NA
  v
}

# Un tableau lu (data.frame, tibble, matrice) en data.frame simple : des nombres, des facteurs
# (variables qualitatives, dans l'ordre de leurs modalités), du texte et des valeurs logiques.
# Les noms de lignes choisis (pas 1, 2, 3...) deviennent une première colonne « Observation ».
# Une colonne qui est elle-même une matrice (des spectres rangés ainsi dans un data.frame R)
# donne une colonne par colonne de la matrice.
vers_data_frame <- function(x) {
  if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  noms_lignes <- if (.row_names_info(x) > 0 && !identical(rownames(x), as.character(seq_len(nrow(x))))) rownames(x)
  colonnes <- list()
  noms <- character(0)
  for (k in seq_along(x)) {
    v <- x[[k]]
    nom <- names(x)[k] %||% ""
    if (is.matrix(v) || is.data.frame(v)) {
      v <- as.data.frame(unclass(v), check.names = FALSE, stringsAsFactors = FALSE)
      sous <- names(v)
      if (is.null(colnames(x[[k]]))) sous <- paste0(nom, ".", seq_along(v))
      colonnes <- c(colonnes, lapply(as.list(v), colonne_simple))
      noms <- c(noms, sous)
    } else {
      colonnes[[length(colonnes) + 1]] <- colonne_simple(v)
      noms <- c(noms, nom)
    }
  }
  names(colonnes) <- paste0("V", seq_along(colonnes))
  x <- as.data.frame(colonnes, stringsAsFactors = FALSE)
  names(x) <- noms
  if (!is.null(noms_lignes) && !anyDuplicated(noms_lignes)) {
    x <- cbind(data.frame(Observation = noms_lignes, stringsAsFactors = FALSE), x)
  }
  noms <- names(x)
  vides <- is.na(noms) | !nzchar(trimws(noms))
  noms[vides] <- paste0("V", which(vides))
  names(x) <- make.unique(trimws(noms), sep = "_")
  # lignes vides (fin d'une sélection collée depuis un tableur), colonnes vides sans nom
  remplie <- function(v) if (is.character(v)) !is.na(v) & nzchar(v) else !is.na(v)
  if (ncol(x)) {
    x <- x[Reduce(`|`, lapply(x, remplie), FALSE), , drop = FALSE]
    x <- x[!(vides & !vapply(x, function(v) any(remplie(v)), TRUE))]
  }
  rownames(x) <- NULL
  x
}

colonne_simple <- function(x) {
  if (inherits(x, "haven_labelled")) {
    # Une variable codée (1 = « femme », 2 = « homme »...) devient qualitative si toutes ses
    # valeurs ont une étiquette ; sinon les étiquettes ne marquent que quelques valeurs
    # particulières et la variable reste numérique (ou du texte).
    etiquettes <- attr(x, "labels", exact = TRUE)
    v <- as.vector(unclass(x))
    connues <- unique(v[!is.na(v)])
    if (length(etiquettes) && length(connues) && all(connues %in% etiquettes)) {
      gardees <- sort(etiquettes[etiquettes %in% connues])
      return(factor(v, levels = unname(gardees), labels = names(gardees)))
    }
    x <- v
  }
  if (inherits(x, c("Date", "POSIXt", "hms"))) return(format(x))
  if (inherits(x, "difftime")) return(as.numeric(x))
  if (is.factor(x)) return(factor(as.character(x), levels = intersect(levels(x), unique(as.character(x)))))
  if (is.logical(x)) return(as.vector(x))
  if (is.numeric(x)) {
    x <- as.numeric(x)
    x[is.nan(x)] <- NA
    return(x)
  }
  if (is.character(x)) return(enc2utf8(as.vector(x)))
  if (is.list(x)) return(vapply(x, function(e) paste(format(e), collapse = " "), ""))
  as.character(x)
}

# ---------------------------------------------------------------- Weka (.arff)

# Un fichier ARFF : l'en-tête décrit les colonnes (@attribute nom type : numeric, un ensemble
# de modalités {a,b,c}, string ou date), puis @data, une ligne par observation, les valeurs
# séparées par des virgules, le texte entre apostrophes ou guillemets (\' pour une apostrophe
# dans le texte), ? pour une valeur manquante. (Le lecteur du paquet foreign ne comprend pas
# \' : ce lecteur-ci le remplace.)
lire_arff <- function(chemin) {
  lignes <- trimws(readLines(chemin, encoding = "UTF-8", warn = FALSE))
  lignes <- lignes[nzchar(lignes) & !startsWith(lignes, "%")]
  debut <- grep("^@data", lignes, ignore.case = TRUE)[1]
  if (is.na(debut)) stop(traduire("Fichier ARFF sans section @data", "ARFF file without a @data section"))
  jeton <- "'(?:[^'\\\\]|\\\\.)*'|\"(?:[^\"\\\\]|\\\\.)*\"|[^,'\"]+"
  valeurs <- function(x) {  # les valeurs d'une liste séparée par des virgules, sans leurs guillemets
    v <- trimws(regmatches(x, gregexpr(jeton, x, perl = TRUE))[[1]])
    v <- v[nzchar(v)]
    entre <- grepl("^(['\"]).*\\1$", v)
    v[entre] <- gsub("\\\\(.)", "\\1", substr(v[entre], 2, nchar(v[entre]) - 1), perl = TRUE)
    v
  }
  attributs <- grep("^@attribute", lignes[seq_len(debut)], ignore.case = TRUE, value = TRUE)
  m <- regmatches(attributs, regexec("^@attribute\\s+('(?:[^'\\\\]|\\\\.)*'|\"(?:[^\"\\\\]|\\\\.)*\"|\\S+)\\s+(.*)$", attributs, ignore.case = TRUE, perl = TRUE))
  noms <- vapply(m, function(x) valeurs(x[2])[1], "")
  types <- vapply(m, function(x) x[3], "")
  donnees <- lignes[-seq_len(debut)]
  if (any(startsWith(donnees, "{"))) stop(traduire("Fichier ARFF « creux » (sparse) : non pris en charge", "Sparse ARFF file: not supported"))
  cellules <- lapply(donnees, valeurs)
  faux <- which(lengths(cellules) != length(noms))
  if (length(faux)) stop(sprintf(traduire("Fichier ARFF : la ligne de données %d n'a pas %d valeurs", "ARFF file: data line %d doesn't have %d values"), faux[1], length(noms)))
  M <- matrix(unlist(cellules), length(donnees), length(noms), byrow = TRUE)
  M[M == "?"] <- NA
  colonnes <- lapply(seq_along(noms), function(k) {
    type <- tolower(types[k])
    if (startsWith(type, "{")) return(factor(M[, k], levels = valeurs(sub("^\\{(.*)\\}\\s*$", "\\1", types[k]))))
    if (grepl("^(numeric|real|integer)", type)) return(as.numeric(M[, k]))
    M[, k]
  })
  stats::setNames(as.data.frame(colonnes, stringsAsFactors = FALSE, col.names = paste0("V", seq_along(noms))), noms)
}

# ---------------------------------------------------------------- dBase (.dbf)

# Les textes d'un fichier dBase sont dans la page de code que donne l'octet 29 de l'en-tête,
# ou un fichier .cpg à côté (comme pour les fichiers de formes des SIG) ; sans indication, en
# UTF-8 s'ils en sont, en Windows-1252 sinon.
PAGES_DBF <- c("01" = "CP437", "02" = "CP850", "03" = "CP1252", "57" = "CP1252", "58" = "CP1252",
               "64" = "CP852", "65" = "CP866", "c8" = "CP1250", "c9" = "CP1251", "ca" = "CP1254", "cb" = "CP1253")

lire_dbf <- function(chemin) {
  x <- foreign::read.dbf(chemin, as.is = TRUE)
  page <- unname(PAGES_DBF[as.character(readBin(chemin, "raw", 30)[30])])
  cpg <- sub("\\.dbf$", ".cpg", chemin, ignore.case = TRUE)
  if (is.na(page) && file.exists(cpg)) page <- sub("^(ANSI ?|CP)?([0-9]+)$", "CP\\2", toupper(trimws(readLines(cpg, 1, warn = FALSE))))
  textes <- c(names(x), unlist(Filter(is.character, x)))
  if (is.na(page)) page <- if (all(validUTF8(textes[!is.na(textes)]))) "UTF-8" else "CP1252"
  names(x) <- iconv(names(x), page, "UTF-8")
  x[] <- lapply(x, function(v) if (is.character(v)) iconv(v, page, "UTF-8") else v)
  x
}

# dBase III : une colonne de texte (C) ou de nombres (N) par variable, chaque valeur écrite en
# caractères sur une largeur fixe ; une valeur manquante reste en blanc. Les textes en
# Windows-1252, déclaré dans l'en-tête (0x57, que lisent les SIG et LibreOffice), les noms de
# 10 caractères. Un nombre a 19 caractères au plus : jusqu'à 15 décimales, moins pour les
# grands nombres.
ecrire_dbf <- function(df, chemin) {
  octets <- function(r, largeur, a_droite = FALSE) {  # r complété par des espaces
    r <- r[seq_len(min(length(r), largeur))]
    blanc <- rep(as.raw(0x20), largeur - length(r))
    if (a_droite) c(blanc, r) else c(r, blanc)
  }
  champs <- lapply(df, function(x) {
    if (is.logical(x)) x <- as.integer(x)
    if (is.numeric(x)) {
      fini <- x[is.finite(x)]
      chiffres <- max(1, nchar(format(trunc(abs(fini)), scientific = FALSE, trim = TRUE)))
      signe <- as.integer(any(fini < 0))
      dec <- if (all(fini == round(fini))) 0 else max(0, min(15, 18 - chiffres - signe))
      largeur <- chiffres + signe + if (dec) dec + 1 else 0
      if (largeur > 19) stop(traduire("Nombre trop grand pour un fichier dBase : ", "Number too large for a dBase file: "), max(abs(fini)))
      texte <- ifelse(is.finite(x), formatC(x, format = "f", digits = dec, width = largeur), "")
      valeurs <- lapply(texte, function(t) octets(charToRaw(t), largeur, TRUE))
      return(list(type = "N", largeur = largeur, dec = dec, valeurs = valeurs))
    }
    texte <- enc2utf8(as.character(x))
    brut <- iconv(ifelse(is.na(texte), "", texte), "UTF-8", "CP1252//TRANSLIT", sub = "?", toRaw = TRUE)
    largeur <- min(254, max(1, lengths(brut)))
    list(type = "C", largeur = largeur, dec = 0, valeurs = lapply(brut, octets, largeur))
  })
  n <- nrow(df)
  largeurs <- vapply(champs, function(ch) ch$largeur, 0)
  entier <- function(v, taille) writeBin(as.integer(v), raw(), size = taille, endian = "little")
  date <- as.POSIXlt(Sys.Date())
  en_tete <- c(as.raw(0x03), as.raw(c(date$year %% 256, date$mon + 1, date$mday)), entier(n, 4),
               entier(32 + 32 * length(champs) + 1, 2), entier(1 + sum(largeurs), 2), raw(17), as.raw(0x57), raw(2))
  descripteurs <- unlist(Map(function(nom, ch) {
    c(charToRaw(nom), raw(11 - nchar(nom)), charToRaw(ch$type), raw(4), as.raw(c(ch$largeur, ch$dec)), raw(14))
  }, noms_pour_format(names(df), 10), champs), use.names = FALSE)
  # une ligne par observation : un espace (ligne non effacée), puis les valeurs bout à bout
  lignes <- do.call(rbind, c(list(matrix(as.raw(0x20), 1, n)),
                             lapply(champs, function(ch) matrix(unlist(ch$valeurs), ch$largeur, n))))
  writeBin(c(en_tete, descripteurs, as.raw(0x0D), as.vector(lignes), as.raw(0x1A)), chemin)
}

# ---------------------------------------------------------------- jamovi (.omv)

# Un fichier jamovi est une archive zip : metadata.json décrit les colonnes, xdata.json les
# modalités des variables qualitatives, data.bin les valeurs (colonne après colonne : entiers
# de 4 octets ou réels de 8 octets) et strings.bin le texte des colonnes « ID ». Les colonnes
# calculées gardent leurs valeurs ; les filtres sont ignorés (toutes les lignes sont lues).
lire_omv <- function(chemin) {
  dossier <- tempfile("omv")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE))
  contenu <- utils::unzip(chemin, list = TRUE)$Name
  utils::unzip(chemin, files = intersect(c("metadata.json", "xdata.json", "data.bin", "strings.bin"), contenu), exdir = dossier)
  meta <- jsonlite::fromJSON(file.path(dossier, "metadata.json"), simplifyVector = FALSE)$dataSet
  xdata <- if (file.exists(file.path(dossier, "xdata.json"))) jsonlite::fromJSON(file.path(dossier, "xdata.json"), simplifyVector = FALSE) else list()
  n <- meta$rowCount
  chaines <- if (file.exists(file.path(dossier, "strings.bin"))) readBin(file.path(dossier, "strings.bin"), "raw", file.size(file.path(dossier, "strings.bin"))) else raw(0)
  zeros <- which(chaines == as.raw(0))
  debuts <- c(0, zeros[-length(zeros)])  # position (à partir de 0) de chaque texte
  texte <- function(pos) {
    k <- match(pos, debuts)
    vapply(k, function(i) if (is.na(i)) NA_character_ else if (zeros[i] - 1 < debuts[i] + 1) "" else
      rawToChar(chaines[(debuts[i] + 1):(zeros[i] - 1)]), "")
  }
  bin <- file(file.path(dossier, "data.bin"), "rb")
  on.exit(close(bin), add = TRUE, after = FALSE)
  colonnes <- list()
  for (f in meta$fields) {
    type <- f$type %||% if (identical(f$dataType, "Decimal")) "number" else "integer"
    x <- switch(type,
                number = readBin(bin, "double", n, size = 8, endian = "little"),
                integer = readBin(bin, "integer", n, size = 4, endian = "little"),
                string = { s <- texte(readBin(bin, "integer", n, size = 4, endian = "little")); Encoding(s) <- "UTF-8"; s },
                stop(traduire("Colonne jamovi d'un type inconnu : ", "jamovi column of an unknown type: "), type))
    if (f$columnType %in% c("Filter", "None")) next
    if (is.double(x)) x[is.nan(x)] <- NA
    labels <- xdata[[f$name]]$labels
    if (type == "integer" && length(labels) && f$measureType %in% c("Nominal", "Ordinal")) {
      x <- factor(x, levels = vapply(labels, function(l) as.numeric(l[[1]]), 0),
                  labels = vapply(labels, function(l) enc2utf8(as.character(l[[2]])), ""))
    }
    for (m in f$missingValues) {  # valeurs déclarées manquantes : « == -999 », « == 'NA' »
      val <- sub("^==\\s*", "", m)
      if (!identical(val, m)) x[!is.na(x) & as.character(x) == gsub("^['\"]|['\"]$", "", val)] <- NA
    }
    colonnes[[f$name]] <- x
  }
  df <- as.data.frame(colonnes, check.names = FALSE, stringsAsFactors = FALSE)
  # la colonne « ID » nomme les observations : elle passe en premier
  id <- Filter(function(f) identical(f$measureType, "ID") && f$name %in% names(df), meta$fields)
  if (length(id)) df <- df[c(id[[1]]$name, setdiff(names(df), id[[1]]$name))]
  df
}

ecrire_omv <- function(df, chemin) {
  dossier <- tempfile("omv")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE))
  n <- nrow(df)
  bin <- file(file.path(dossier, "data.bin"), "wb")
  chaines <- list()
  taille <- 0L
  champs <- list()
  xdata <- list()
  for (k in seq_along(df)) {
    x <- df[[k]]
    nom <- names(df)[k]
    if (is.logical(x)) x <- factor(ifelse(x, "TRUE", "FALSE"), levels = c("FALSE", "TRUE"))
    if (is.character(x) && anyDuplicated(x[!is.na(x)])) x <- factor(x, levels = sort(unique(x[!is.na(x)])))
    champ <- list(name = nom, id = k, columnType = "Data", dataType = "Decimal", measureType = "Continuous",
                  formula = "", formulaMessage = "", parentId = 0, width = 100, type = "number",
                  importName = "", description = "", transform = 0, edits = list(), missingValues = list())
    if (is.factor(x)) {
      niv <- levels(x)
      champ[c("dataType", "measureType", "type")] <- list("Text", if (is.ordered(x)) "Ordinal" else "Nominal", "integer")
      champ$trimLevels <- TRUE
      xdata[[nom]] <- list(labels = lapply(seq_along(niv), function(i) list(i - 1L, niv[i], niv[i], FALSE)))
      writeBin(ifelse(is.na(x), NA_integer_, as.integer(x) - 1L), bin, size = 4, endian = "little")
    } else if (is.character(x)) {
      champ[c("dataType", "measureType", "type")] <- list("Text", "ID", "string")
      pos <- rep(NA_integer_, n)
      for (i in which(!is.na(x))) {
        octets <- c(charToRaw(enc2utf8(x[i])), as.raw(0))
        pos[i] <- taille
        chaines[[length(chaines) + 1]] <- octets
        taille <- taille + length(octets)  # entier : writeBin écrirait un réel sur 4 octets
      }
      writeBin(pos, bin, size = 4, endian = "little")
    } else {
      x <- as.numeric(x)
      x[is.na(x)] <- NaN
      writeBin(x, bin, size = 8, endian = "little")
    }
    champs[[k]] <- champ
  }
  close(bin)
  fichiers <- c("META-INF/MANIFEST.MF", "meta", "metadata.json", "xdata.json", "data.bin", "index.html")
  if (taille > 0) {
    writeBin(unlist(chaines), file.path(dossier, "strings.bin"))
    fichiers <- c(fichiers, "strings.bin")
  }
  # des fins de ligne « \n » : jamovi refuse un manifeste aux fins de ligne Windows
  texte <- function(lignes, f) writeBin(charToRaw(enc2utf8(paste0(lignes, "\n", collapse = ""))), file.path(dossier, f))
  json <- function(x, f) texte(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)), f)
  json(list(dataSet = list(rowCount = n, columnCount = ncol(df), removedRows = list(), addedRows = list(),
                           fields = champs, transforms = list())), "metadata.json")
  json(if (length(xdata)) xdata else stats::setNames(list(), character(0)), "xdata.json")
  # le manifeste, en double comme dans les fichiers de jamovi (META-INF/ pour ses anciennes versions)
  dir.create(file.path(dossier, "META-INF"))
  for (f in c("meta", "META-INF/MANIFEST.MF")) {
    texte(c("Manifest-Version: 1.0", "Data-Archive-Version: 1.0.2", "jamovi-Archive-Version: 11.0", "Created-By: Nuee"), f)
  }
  texte("<!DOCTYPE html><html><head><meta charset=\"utf-8\"></head><body></body></html>", "index.html")
  if (file.exists(chemin)) file.remove(chemin)
  zip::zip(normalizePath(chemin, mustWork = FALSE), files = fichiers, root = dossier)
  invisible(chemin)
}

# ---------------------------------------------------------------- écriture

# Noms de variables acceptés par un format : lettres sans accent, chiffres et « _ », commençant
# par une lettre, `max` caractères au plus, tous différents. Le nom d'origine reste en
# étiquette de la variable (SPSS, Stata, SAS).
noms_pour_format <- function(noms, max) {
  n <- iconv(enc2utf8(noms), "UTF-8", "ASCII//TRANSLIT", sub = "")
  n[is.na(n)] <- ""
  n <- gsub("^_+|_+$", "", gsub("[^A-Za-z0-9_]+", "_", n))
  sans_lettre <- !grepl("^[A-Za-z]", n)
  n[sans_lettre] <- paste0("v", n[sans_lettre])
  reserves <- c("all", "and", "by", "eq", "ge", "gt", "le", "lt", "ne", "not", "or", "to", "with", "if", "in", "_n", "_all")
  n[tolower(n) %in% reserves] <- paste0(n[tolower(n) %in% reserves], "_")
  n <- substr(n, 1, max)
  for (i in which(duplicated(tolower(n)))) {
    k <- 2
    repeat {
      essai <- paste0(substr(n[i], 1, max - nchar(k) - 1), "_", k)
      if (!tolower(essai) %in% tolower(n)) break
      k <- k + 1
    }
    n[i] <- essai
  }
  n
}

# Écrit un data.frame dans le format de l'extension du chemin.
ecrire_tableau <- function(df, chemin, objet = "donnees") {
  ext <- extension(chemin)
  renomme <- function(df, max, octets_etiquette = Inf) {
    noms <- noms_pour_format(names(df), max)
    for (k in seq_along(df)) if (!identical(noms[k], names(df)[k])) {
      etiquette <- enc2utf8(names(df)[k])
      while (nchar(etiquette, "bytes") > octets_etiquette) etiquette <- substr(etiquette, 1, nchar(etiquette) - 1)
      attr(df[[k]], "label") <- etiquette
    }
    names(df) <- noms
    df
  }
  logiques_en_nombres <- function(df) { df[] <- lapply(df, function(x) if (is.logical(x)) as.integer(x) else x); df }
  r_noms_lignes <- function(df) {  # en R, les noms des observations sont les noms des lignes
    if (ncol(df) > 1 && is.character(df[[1]]) && !anyNA(df[[1]]) && !anyDuplicated(df[[1]])) {
      rownames(df) <- df[[1]]
      df <- df[-1]
    }
    df
  }
  switch(ext,
    xlsx = openxlsx::write.xlsx(df, chemin, overwrite = TRUE),
    csv = utils::write.csv(df, chemin, row.names = FALSE, na = "", fileEncoding = "UTF-8"),
    tsv = , txt = {  # sans guillemets : une tabulation ou un retour à la ligne dans un texte devient une espace
      df[] <- lapply(df, function(x) if (is.character(x) || is.factor(x)) gsub("[\t\r\n]+", " ", x) else x)
      utils::write.table(df, chemin, sep = "\t", quote = FALSE, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    },
    rds = saveRDS(r_noms_lignes(df), chemin),
    rdata = , rda = { e <- new.env(); assign(objet, r_noms_lignes(df), envir = e); save(list = objet, envir = e, file = chemin) },
    sav = haven::write_sav(renomme(logiques_en_nombres(df), 64), chemin),
    dta = haven::write_dta(renomme(logiques_en_nombres(df), 32), chemin),
    xpt = {  # pas d'étiquettes de valeurs en SAS transport : les modalités s'écrivent en texte.
      # La version 5, que lisent SAS (libname xport), Stata, SPSS et pandas : noms de 8
      # caractères, étiquettes de 40 octets, textes de 200 octets.
      df[] <- lapply(df, function(x) {
        if (is.factor(x)) x <- as.character(x)
        if (is.character(x)) x <- vapply(enc2utf8(x), function(v) {
          while (!is.na(v) && nchar(v, "bytes") > 200) v <- substr(v, 1, nchar(v) - 1)
          v
        }, "", USE.NAMES = FALSE)
        x
      })
      haven::write_xpt(renomme(logiques_en_nombres(df), 8, 40), chemin, version = 5,
                       name = noms_pour_format(objet, 8))
    },
    omv = ecrire_omv(df, chemin),
    dbf = ecrire_dbf(df, chemin),
    arff = foreign::write.arff(logiques_en_nombres(df), chemin, relation = objet),
    stop(traduire("Format d'export non reconnu : .", "Unrecognised export format: ."), ext))
  invisible(chemin)
}

# Plusieurs tableaux (les résultats d'un modèle) : les feuilles d'un classeur Excel, une
# liste nommée dans un fichier R ; dans les autres formats, un fichier par tableau, le premier
# au chemin donné, les suivants à côté (« M1 - variables.sav »). Renvoie les fichiers écrits.
ecrire_tables <- function(tables, chemin, objet = "resultats") {
  tables <- Filter(Negate(is.null), tables)
  ext <- extension(chemin)
  if (ext == "xlsx") {
    wb <- openxlsx::createWorkbook()
    for (n in names(tables)) {
      feuille <- substr(gsub("[\\[\\]:*?/\\\\]", "_", n, perl = TRUE), 1, 31)
      openxlsx::addWorksheet(wb, feuille)
      openxlsx::writeData(wb, feuille, tables[[n]])
    }
    openxlsx::saveWorkbook(wb, chemin, overwrite = TRUE)
    return(chemin)
  }
  if (ext %in% c("rds", "rdata", "rda")) {
    liste <- lapply(tables, function(df) {
      if (is.character(df[[1]]) && !anyNA(df[[1]]) && !anyDuplicated(df[[1]])) { rownames(df) <- df[[1]]; df <- df[-1] }
      df
    })
    if (ext == "rds") saveRDS(liste, chemin) else {
      e <- new.env()
      assign(objet, liste, envir = e)
      save(list = objet, envir = e, file = chemin)
    }
    return(chemin)
  }
  base <- sub("\\.[^.]+$", "", chemin)
  chemins <- c(chemin, sprintf("%s - %s.%s", base, names(tables)[-1], tools::file_ext(chemin)))
  for (i in seq_along(tables)) ecrire_tableau(tables[[i]], chemins[i], objet = sub("\\.[^.]+$", "", basename(chemins[i])))
  chemins
}

# Le tableau des données d'un projet : le nom des observations, leurs autres identifiants, puis
# chaque variable (en facteur pour une variable qualitative).
tableau_projet <- function(projet) {
  obs <- rownames(projet$donnees)
  uid <- vapply(projet$observations, function(o) as.character(o$uid), "")
  noms_id <- as.character(projet$observation_id_names %||% character(0))
  premier <- if (length(noms_id) && nzchar(noms_id[1]) && !startsWith(noms_id[1], "$")) noms_id[1] else "Observation"
  colonnes <- list(unname(projet$noms_obs[obs]))
  noms <- premier
  for (k in seq_along(noms_id)[-1]) {
    if (!nzchar(noms_id[k]) || startsWith(noms_id[k], "$")) next
    ids <- vapply(projet$observations, function(o) if (length(o$ids) >= k) as.character(o$ids[[k]] %||% NA) else NA_character_, "")
    if (!any(nzchar(ids), na.rm = TRUE)) next
    colonnes[[length(colonnes) + 1]] <- ids[match(obs, uid)]
    noms <- c(noms, noms_id[k])
  }
  for (v in projet$variables) {
    if (!as.character(v$uid) %in% colnames(projet$donnees)) next
    x <- projet$donnees[, as.character(v$uid)]
    colonnes[[length(colonnes) + 1]] <- if (length(v$categories)) factor(v$categories[x], levels = v$categories) else unname(x)
    noms <- c(noms, as.character(v$ids[1]))
  }
  names(colonnes) <- make.unique(noms, sep = "_")
  as.data.frame(colonnes, check.names = FALSE, stringsAsFactors = FALSE)
}

# Les résultats d'un modèle ajusté en tableaux : observations (scores, T2, DModX, valeurs
# ajustées), variables (loadings, VIP...), composantes, coefficients et variables Y.
tables_resultats <- function(projet, m) {
  s <- m$statistiques
  obs <- s$observations
  obs <- cbind(data.frame(Observation = unname(projet$noms_obs[rownames(obs)]), stringsAsFactors = FALSE), obs)
  rownames(obs) <- NULL
  list(observations = obs, variables = s$variables, composantes = s$composantes,
       coefficients = s$coefficients, variables_y = s$variables_y)
}
