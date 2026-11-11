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
         xlsx = , xlsm = , xls = readxl::excel_sheets(chemin),
         rdata = , rda = names(objets_r(chemin)),
         rds = { x <- lire_rds(chemin); if (est_tableau(x)) NULL else names(tableaux_r(x)) },
         NULL)
}

# Le tableau d'un fichier, en data.frame. `table` : nom de la feuille ou de l'objet (le
# premier sinon). Dans un fichier texte ou Excel, rien ne dit qu'une colonne est faite de
# nombres : convertir_colonnes() en décide. `quantitatives` : noms de colonnes à lire en
# nombres quel que soit le format (fichier à prédire : les variables quantitatives du modèle).
# attr(, "note") : ce qu'il faut dire de la lecture (valeurs lues comme manquantes, lignes
# incomplètes), une phrase par colonne concernée, nommée par elle ("" pour tout le fichier).
lire_tableau <- function(chemin, table = NULL, quantitatives = NULL) {
  ext <- extension(chemin)
  un_parmi <- function(objets) {
    if (!length(objets)) stop(traduire("Ce fichier ne contient aucun tableau (data.frame ou matrice)", "This file holds no table (data frame or matrix)"))
    if (is.null(table)) return(objets[[1]])
    objets[[table]] %||% stop(traduire("Tableau introuvable dans le fichier : ", "Table not found in the file: "), table)
  }
  x <- switch(ext,
    csv = , txt = , tsv = , tab = , dat = lire_texte(chemin),
    xlsx = , xlsm = , xls = lire_excel(chemin, table),
    rds = { x <- lire_rds(chemin); if (est_tableau(x)) x else un_parmi(tableaux_r(x)) },
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
  finir_tableau(x, quantitatives, toutes = ext %in% c("csv", "txt", "tsv", "tab", "dat", "xlsx", "xlsm", "xls"))
}

# Un tableau lu, en data.frame simple (vers_data_frame), ses colonnes de texte converties
# (convertir_colonnes) : toutes si `toutes`, sinon celles de `quantitatives`.
finir_tableau <- function(x, quantitatives = NULL, toutes = TRUE) {
  note <- attr(x, "note")
  virgule <- attr(x, "virgule") %||% "decimale"
  x <- convertir_colonnes(vers_data_frame(x), virgule, quantitatives, toutes)
  attr(x, "note") <- c(note, attr(x, "note"))
  x
}

est_tableau <- function(x) is.data.frame(x) || (is.matrix(x) && (is.numeric(x) || is.character(x) || is.logical(x)))

# Fichiers R. readRDS() et load() n'exécutent pas de code depuis R 4.4 (avant, un fichier
# pouvait contenir une promesse, évaluée dès qu'on touche à l'objet : CVE-2024-27322). Avec un
# R plus ancien (Ubuntu 22.04, Debian 12 sous Linux), Nuée refuse donc ces fichiers
# (fichier_r_lisible). Un objet lu peut aussi porter du code : un environnement exécute ses
# liaisons actives quand on lit leurs valeurs (as.list, mget, get), et les méthodes d'un objet
# S4 (R5 aussi) viennent du paquet qui le définit ; un objet R6 est un environnement. On ne
# touche donc qu'à des objets faits de vecteurs et de listes (objet_r_sur), et on ne garde
# que les tableaux.

# Refuse un fichier R avant de le lire quand ce R est plus ancien que 4.4.
fichier_r_lisible <- function() {
  if (getRversion() < "4.4.0") {
    stop(sprintf(traduire("Nuée n'ouvre les fichiers .rds et .RData qu'avec R 4.4 ou plus récent (avant, un tel fichier peut exécuter du code dès qu'on le lit). Ce R est en version %s : enregistrez le tableau dans un autre format (CSV, Excel...), ou installez un R plus récent.",
                          "Nuée opens .rds and .RData files only with R 4.4 or newer (before, such a file can run code as soon as it is read). This R is version %s: save the table in another format (CSV, Excel...), or install a newer R."),
                 as.character(getRversion())), call. = FALSE)
  }
}

# L'objet d'un fichier .rds : un tableau, ou une liste dont tableaux_r() prendra les
# tableaux ; tout autre objet est refusé.
lire_rds <- function(chemin) {
  fichier_r_lisible()
  x <- readRDS(chemin)
  liste <- typeof(x) == "list" && !isS4(x) && !is.data.frame(x)
  if (!liste && !objet_r_sur(x)) {
    stop(traduire("Ce fichier R contient un objet que Nuée n'ouvre pas (environnement, fonction, objet S4, R5 ou R6...) : le lire pourrait exécuter du code. Enregistrez-y un data.frame, une matrice ou une liste de data.frames.",
                  "This R file holds an object Nuée doesn't open (environment, function, S4, R5 or R6 object...): reading it could run code. Save a data frame, a matrix or a list of data frames in it."), call. = FALSE)
  }
  x
}

# Les tableaux d'une liste lue dans un fichier R, par nom ; ses autres éléments sont ignorés
# sans y toucher.
tableaux_r <- function(x) {
  if (typeof(x) != "list" || isS4(x)) return(list())
  Filter(function(e) !isS4(e) && est_tableau(e) && objet_r_sur(e), unclass(x))
}

# Les tableaux d'un fichier .RData, par nom ; ses autres objets (fonctions, environnements...)
# sont ignorés sans y toucher.
objets_r <- function(chemin) {
  fichier_r_lisible()
  e <- new.env()
  load(chemin, envir = e)
  Filter(function(x) !isS4(x) && est_tableau(x) && objet_r_sur(x), mget(sort(ls(e)), envir = e))
}

# Un objet qu'on peut parcourir sans exécuter de code : des vecteurs (nombres, texte, valeurs
# logiques ; facteurs, dates...) et des listes (data.frames compris) faites de vecteurs et de
# listes, jusque dans leurs attributs. Pas d'environnement, de fonction, d'objet S4, de
# pointeur externe ni d'expression ; sauf, en attribut, un pointeur externe (celui des
# data.table, vide une fois relu).
objet_r_sur <- function(x, profondeur = 0, attribut = FALSE) {
  if (profondeur > 20 || isS4(x)) return(FALSE)
  sur <- switch(typeof(x),
                logical = , integer = , double = , complex = , character = , raw = , "NULL" = TRUE,
                list = all(vapply(unclass(x), objet_r_sur, TRUE, profondeur + 1)),
                externalptr = attribut,
                FALSE)
  sur && all(vapply(attributes(x), objet_r_sur, TRUE, profondeur + 1, TRUE))
}

# Texte délimité. L'encodage (UTF-8, Windows-1252 des fichiers d'Excel, UTF-16 du « texte
# Unicode » d'Excel), puis les champs (lire_texte_delimite), dont convertir_colonnes() fait
# des nombres quand ce sont des mesures.
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

# Les champs d'un texte délimité, en data.frame de texte. Le séparateur est celui que la
# première ligne a le plus, hors des champs entre guillemets : tabulation, point-virgule,
# virgule, sinon des espaces. Un champ qui commence par un guillemet va jusqu'au guillemet
# suivi du séparateur ou de la fin de la ligne : il peut contenir le séparateur, des retours
# à la ligne, et "" pour un guillemet. Ailleurs, un guillemet est un caractère comme un autre
# (coordonnées 43°31'12"N, tube de 3"). Chaque ligne a les colonnes de la première, l'en-tête :
#   - des champs vides au-delà (séparateurs en trop au bout des lignes) sont ignorés ;
#   - une ligne plus courte est complétée par des valeurs manquantes, et une note le dit ;
#   - une ligne plus longue arrête la lecture, avec son numéro : ses valeurs seraient
#     décalées ;
#   - sauf si toutes les lignes ont une valeur de plus que l'en-tête et que leurs premières
#     valeurs sont toutes différentes : ce sont les noms des lignes, que write.table() de R
#     écrit sans nom de colonne.
# attr(, "virgule") : "milliers" dans un fichier séparé par des virgules, où une virgule n'est
# jamais décimale (lire_nombres), "decimale" sinon ; attr(, "note") : celle des lignes courtes.
lire_texte_delimite <- function(texte) {
  # (une expression régulière serait bien plus lente sur un gros fichier)
  couper <- function(t) strsplit(gsub("\r", "\n", gsub("\r\n", "\n", t, fixed = TRUE), fixed = TRUE), "\n", fixed = TRUE)[[1]]
  lignes <- couper(texte)
  pleines <- grepl("[^[:space:]]", lignes)
  if (!any(pleines)) return(data.frame())
  premiere <- gsub('(?<![^\t;,])[ \t]*"(?:[^"]|"")*"[ \t]*(?![^\t;,])', "", lignes[which(pleines)[1]], perl = TRUE)
  compter <- function(s) nchar(premiere) - nchar(gsub(s, "", premiere, fixed = TRUE))
  n <- c("\t" = compter("\t"), ";" = compter(";"), "," = compter(","))
  sep <- if (max(n) > 0) names(n)[which.max(n)] else ""
  # un champ entre guillemets sur plusieurs lignes : ses retours à la ligne sont mis de côté
  # (\036) le temps de couper les lignes
  saut <- "\036"
  guillemets <- nchar(lignes) - nchar(gsub('"', "", lignes, fixed = TRUE))
  if (any(guillemets %% 2 == 1)) {
    motif <- if (sep == "") paste0("(?<!\\S)", entre_guillemets(sep), "(?=\\s|$)") else {
      s <- if (sep == "\t") "\\t" else sep
      blanc <- if (sep == "\t") " *" else "[ \\t]*"
      sprintf("(?<![^%s\\r\\n])%s%s%s(?=[%s\\r\\n]|$)", s, blanc, entre_guillemets(sep), blanc, s)
    }
    m <- gregexpr(motif, texte, perl = TRUE)
    champs <- regmatches(texte, m)[[1]]
    plusieurs <- grepl("[\r\n]", champs)
    if (any(plusieurs)) {
      champs[plusieurs] <- gsub("\r\n|\r|\n", saut, champs[plusieurs], perl = TRUE)
      regmatches(texte, m) <- list(champs)
      lignes <- couper(texte)
      pleines <- grepl("[^[:space:]]", lignes)
    }
  }
  sauts <- nchar(lignes) - nchar(gsub(saut, "", lignes, fixed = TRUE))
  numero <- cumsum(c(1, 1 + sauts))[seq_along(lignes)][pleines]  # dans le fichier, pour les messages
  champs <- champs_lignes(lignes[pleines], sep)
  v <- trimws(gsub(saut, "\n", champs$valeurs, fixed = TRUE))
  n <- champs$n
  k <- n[1]
  noms <- v[seq_len(k)]
  v <- v[-seq_len(k)]
  n <- n[-1]
  numero <- numero[-1]
  ligne <- rep.int(seq_along(n), n)
  col <- sequence(n)
  vide <- !nzchar(v)
  noms_lignes <- NULL
  if (length(n) && all(n == k + 1) && !all(vide[col == k + 1])) {
    premiers <- v[col == 1]
    if (all(nzchar(premiers)) && !anyDuplicated(premiers)) {  # noms des lignes, écrits par R
      noms_lignes <- premiers
      garde <- col > 1
      v <- v[garde]
      vide <- vide[garde]
      ligne <- ligne[garde]
      col <- col[garde] - 1L
    }
  }
  trop <- unique(ligne[col > k & !vide])
  if (length(trop)) {
    stop(sprintf(traduire("%s : plus de valeurs que la première ligne n'a de colonnes (%d). Les valeurs seraient décalées : une valeur contient peut-être le séparateur (%s) ou un guillemet, ou un nom de colonne manque à la première ligne.",
                          "%s: more values than the first line has columns (%d). The values would be shifted: a value may contain the separator (%s) or a quote, or a column name is missing from the first line."),
                 texte_lignes(numero[trop]), k, nom_separateur(sep)), call. = FALSE)
  }
  # lignes plus courtes que les colonnes nommées (une ligne courte parmi d'autres, complètes)
  nommees <- max(c(0, which(nzchar(noms))))
  courtes <- which(tabulate(ligne, length(n)) < nommees)
  note <- if (length(courtes) && length(courtes) < length(n)) {
    stats::setNames(sprintf(traduire("%s : moins de valeurs que de colonnes (%d) ; celles des dernières colonnes y sont lues comme manquantes.",
                                     "%s: fewer values than columns (%d); those of the last columns are read as missing there."),
                            texte_lignes(numero[courtes]), nommees), "")
  }
  v[vide | v == "NA"] <- NA
  M <- matrix(NA_character_, length(n), k)
  dedans <- col <= k
  M[cbind(ligne[dedans], col[dedans])] <- v[dedans]
  df <- as.data.frame(M, stringsAsFactors = FALSE)
  names(df) <- noms
  if (!is.null(noms_lignes)) rownames(df) <- noms_lignes
  attr(df, "virgule") <- if (sep == ",") "milliers" else "decimale"
  attr(df, "note") <- note
  df
}

# Un champ entre guillemets (expression régulière) : "" y est un guillemet ; \" entre des
# espaces (sep = ""), comme les écrit write.table() de R.
entre_guillemets <- function(sep) if (sep == "") '"(?:[^"\\\\]|\\\\.)*+"' else '"(?:[^"]|"")*+"'

# Les champs de lignes de texte : list(valeurs = les champs de toutes les lignes, bout à bout ;
# n = leur nombre par ligne). Les lignes sans guillemets sont coupées au séparateur ; dans les
# autres, un champ entre guillemets perd ses guillemets.
champs_lignes <- function(lignes, sep) {
  avec <- grepl('"', lignes, fixed = TRUE)
  simples <- if (sep == "") strsplit(trimws(lignes[!avec]), "[ \t]+", perl = TRUE) else strsplit(paste0(lignes[!avec], sep), sep, fixed = TRUE)
  n <- integer(length(lignes))
  n[!avec] <- lengths(simples)
  cites <- character(0)
  if (any(avec)) {
    if (sep == "") {
      l <- lignes[avec]
      m <- gregexpr(paste0(entre_guillemets(sep), "(?=\\s|$)|\\S+"), l, perl = TRUE)
      fin <- 0L
    } else {  # chaque champ suivi du séparateur, ajouté au bout de la ligne
      s <- if (sep == "\t") "\\t" else sep
      blanc <- if (sep == "\t") " *" else "[ \\t]*"
      l <- paste0(lignes[avec], sep)
      m <- gregexpr(sprintf("%s%s%s%s|[^%s]*%s", blanc, entre_guillemets(sep), blanc, s, s, s), l, perl = TRUE)
      fin <- 1L
    }
    n[avec] <- lengths(m)
    debut <- unlist(m, use.names = FALSE)
    longueur <- unlist(lapply(m, attr, "match.length"), use.names = FALSE)
    cites <- substring(rep.int(l, n[avec]), debut, debut + longueur - 1L - fin)
  }
  valeurs <- character(sum(n))
  depart <- cumsum(n) - n
  valeurs[rep.int(depart[!avec], n[!avec]) + sequence(n[!avec])] <- unlist(simples, use.names = FALSE)
  valeurs[rep.int(depart[avec], n[avec]) + sequence(n[avec])] <- cites
  q <- grepl('^\\s*"', valeurs, perl = TRUE)
  q[q] <- grepl(paste0("^\\s*", entre_guillemets(sep), "\\s*$"), valeurs[q], perl = TRUE)
  dedans <- sub('^\\s*"(.*)"\\s*$', "\\1", valeurs[q], perl = TRUE)
  valeurs[q] <- if (sep == "") gsub('\\"', '"', dedans, fixed = TRUE) else gsub('""', '"', dedans, fixed = TRUE)
  list(valeurs = valeurs, n = n)
}

# « Ligne 7 », « Lignes 7, 12 et 15 », « Lignes 7, 12, 15, 21, 30… (52 lignes) »
texte_lignes <- function(numeros) {
  if (length(numeros) == 1) return(sprintf(traduire("Ligne %d", "Line %d"), numeros))
  if (length(numeros) > 5) {
    return(sprintf(traduire("Lignes %s… (%d lignes)", "Lines %s… (%d lines)"), paste(numeros[1:5], collapse = ", "), length(numeros)))
  }
  sprintf(traduire("Lignes %s et %d", "Lines %s and %d"), paste(utils::head(numeros, -1), collapse = ", "), utils::tail(numeros, 1))
}

nom_separateur <- function(sep) {
  switch(sep, "\t" = traduire("tabulation", "tab"), ";" = traduire("point-virgule", "semicolon"),
         "," = traduire("virgule", "comma"), traduire("espace", "space"))
}

# ---------------------------------------------------------------- nombres

# Un nom de colonne comparable d'un fichier à l'autre : sans majuscules ni espaces autour, un
# « . » valant une espace (les versions précédentes de Nuée lisaient « NO3 (mg/L) » d'un
# classeur Excel en « NO3.(mg/L) » ; les projets qui gardent ces noms retrouvent leurs colonnes).
nom_comparable <- function(nom) gsub(".", " ", tolower(trimws(nom)), fixed = TRUE)

# Les colonnes de texte d'un tableau dont le format ne dit pas le type (texte, Excel) :
# convertir_colonne() en fait des nombres quand ce sont des mesures. `quantitatives` : noms de
# colonnes à lire en nombres quoi qu'il arrive, de tout type (fichier à prédire : variables
# quantitatives du modèle) ; seules celles-là si `toutes` est faux (formats qui disent le type
# des colonnes). attr(, "note") : une phrase par colonne dont des valeurs sont lues comme
# manquantes, nommée par la colonne.
convertir_colonnes <- function(df, virgule = "decimale", quantitatives = NULL, toutes = TRUE) {
  note <- character(0)
  forcees <- nom_comparable(names(df)) %in% nom_comparable(quantitatives)
  for (k in seq_along(df)) {
    x <- df[[k]]
    if (!(forcees[k] && (is.character(x) || is.factor(x) || is.logical(x))) && !(toutes && is.character(x))) next
    y <- convertir_colonne(as.character(x), virgule, premiere = k == 1, forcer = forcees[k])
    manquantes <- attr(y, "manquantes")
    if (length(manquantes)) note[names(df)[k]] <- note_manquantes(names(df)[k], manquantes)
    attr(y, "manquantes") <- NULL
    df[[k]] <- y
  }
  attr(df, "note") <- if (length(note)) note
  df
}

# Une colonne de texte, en nombres si c'est une colonne de mesures, sinon en texte (sans les
# espaces autour ; "" et « NA » manquants). Une colonne de mesures a au moins deux nombres, et
# ses autres valeurs (vides et « NA » non comptés) sont, sauf un cinquième au plus, des valeurs
# qui disent qu'une mesure manque (valeur_manquante : « <0,5 », « nd », « #N/A »...) ; elles
# deviennent toutes des valeurs manquantes, données par attr(, "manquantes") pour le dire.
# Une colonne de noms (sites, espèces, traitements) reste ainsi du texte, même avec quelques
# nombres. Un code numérique comme -999 reste un nombre : rien ne le distingue d'une mesure.
# La première colonne, qui nomme les observations quand c'est du texte, ne devient des
# nombres que si ses autres valeurs disent toutes qu'une mesure manque (des noms comme
# « 12bis » la gardent en texte). `forcer` : en nombres quoi qu'il arrive.
convertir_colonne <- function(x, virgule = "decimale", premiere = FALSE, forcer = FALSE) {
  v <- trimws(x)
  v[v %in% c("", "NA")] <- NA
  connues <- !is.na(v)
  if (!any(connues)) return(rep(NA_real_, length(v)))
  n <- lire_nombres(v, virgule)
  autres <- connues & is.na(n)
  if (!any(autres)) return(n)
  texte <- sum(autres & !valeur_manquante(v))
  if (!forcer && (sum(connues & !autres) < 2 || texte > if (premiere) 0 else 0.2 * sum(connues))) return(v)
  n[autres] <- NA
  structure(n, manquantes = v[autres])
}

# Les nombres écrits dans du texte (NA pour le reste). En plus de ce que lit as.numeric()
# (« 12.5 », « -3 », « 1e-3 ») : les espaces entre les milliers (insécables aussi : « 1 234,5 »)
# et une virgule, décimale (« 12,5 »), sauf dans un fichier séparé par des virgules
# (`virgule` = "milliers") ou dans une colonne écrite à l'anglaise (« 1,234,567 »,
# « 1,250.5 ») : elle y sépare les milliers (« 1,250 » : 1250) si toutes les virgules de la
# colonne séparent des groupes de trois chiffres ; sinon ces valeurs ne sont pas des nombres.
lire_nombres <- function(v, virgule = "decimale") {
  n <- suppressWarnings(as.numeric(v))
  reste <- which(is.na(n) & !is.na(v))
  if (!length(reste)) return(n)
  w <- gsub("(?<=\\d)[   ](?=\\d{3})", "", v[reste], perl = TRUE)
  virgules <- grepl("^[+-]?[0-9.,]+([eE][+-]?[0-9]+)?$", w) & grepl(",", w, fixed = TRUE)
  if (virgule == "milliers" || any(grepl(",.*[,.]", w[virgules]))) {
    milliers <- all(grepl("^[+-]?[1-9][0-9]{0,2}(,[0-9]{3})+(\\.[0-9]+)?$", w[virgules]))
    w[virgules] <- if (milliers) gsub(",", "", w[virgules], fixed = TRUE) else NA
  } else {
    w[virgules] <- sub(",", ".", w[virgules], fixed = TRUE)
  }
  n[reste] <- suppressWarnings(as.numeric(w))
  n
}

# Valeurs qui disent qu'une mesure manque, plutôt que du texte : valeurs censurées (« <0,5 »,
# « < LQ », « >2000 », « ≤0,1 »), codes de valeur manquante (nd, n.d., n/a, NaN, -, ?...),
# limites seules (LOD, LQ...), traces, erreurs d'Excel (#N/A, #DIV/0!...).
valeur_manquante <- function(v) {
  code <- gsub("[[:space:]./]", "", tolower(v))
  grepl("^[<>≤≥]", v) |
    code %in% c("nd", "na", "nan", "nm", "nq", "ns", "null", "-", "--", "---", "—", "–", "?", "??",
                "lod", "loq", "ld", "lq", "dl", "ql", "tr", "trace", "traces") |
    grepl("^#(n/a|div/0!|value!|valeur!|ref!|name\\?|nom\\?|num!|nombre!|null!|nul!)$", v, ignore.case = TRUE)
}

# La note d'une colonne dont des valeurs sont lues comme manquantes : combien, lesquelles, et
# combien sont sous la limite de détection (« <0,5 ») ou au-dessus de la limite de mesure
# (« >2000 »).
note_manquantes <- function(nom, valeurs) {
  n <- length(valeurs)
  comptes <- table(factor(valeurs, levels = unique(valeurs)))
  montrees <- ifelse(nchar(names(comptes)) > 20, paste0(substr(names(comptes), 1, 19), "…"), names(comptes))
  liste <- paste0(montrees, " ×", comptes)
  if (length(liste) > 5) liste <- c(liste[1:5], "…")
  dessous <- sum(grepl("^[<≤]", valeurs))
  dessus <- sum(grepl("^[>≥]", valeurs))
  quoi <- if (dessous == n) {
    traduire(if (n == 1) "valeur sous la limite de détection lue comme manquante" else "valeurs sous la limite de détection lues comme manquantes",
             if (n == 1) "value below the detection limit read as missing" else "values below the detection limit read as missing")
  } else if (dessus == n) {
    traduire(if (n == 1) "valeur au-dessus de la limite de mesure lue comme manquante" else "valeurs au-dessus de la limite de mesure lues comme manquantes",
             if (n == 1) "value above the measurement limit read as missing" else "values above the measurement limit read as missing")
  } else {
    traduire(if (n == 1) "valeur non numérique lue comme manquante" else "valeurs non numériques lues comme manquantes",
             if (n == 1) "non-numeric value read as missing" else "non-numeric values read as missing")
  }
  limites <- if (dessous < n && dessus < n) c(
    if (dessous) sprintf(traduire("%d sous la limite de détection", "%d below the detection limit"), dessous),
    if (dessus) sprintf(traduire("%d au-dessus de la limite de mesure", "%d above the measurement limit"), dessus))
  sprintf(traduire("%s : %d %s (%s)%s", "%s: %d %s (%s)%s"), nom, n, quoi, paste(liste, collapse = ", "),
          if (length(limites)) paste0(traduire(", dont ", ", including "), paste(limites, collapse = traduire(" et ", " and "))) else "")
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
  names(colonnes) <- sprintf("V%d", seq_along(colonnes))  # (paste0 donnerait "V" sans colonne)
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

# ---------------------------------------------------------------- Excel

# Une feuille d'un classeur Excel (.xlsx, .xlsm, .xls), lue cellule par cellule (readxl), avec
# les noms de colonnes tels qu'ils sont écrits (espaces compris). Une colonne de nombres reste
# en nombres ; les autres deviennent du texte, que convertir_colonnes() traite comme celui d'un
# fichier texte (nombres saisis en texte comme « 1,2 », valeurs censurées comme « <0,5 »...).
# Une cellule vide ou une erreur (#N/A, #DIV/0!...) est une valeur manquante.
lire_excel <- function(chemin, table = NULL) {
  x <- readxl::read_excel(chemin, sheet = table %||% 1, col_types = "list", .name_repair = "minimal")
  df <- as.data.frame(lapply(stats::setNames(as.list(x), sprintf("V%d", seq_along(x))), cellules_excel), stringsAsFactors = FALSE)
  names(df) <- names(x)
  df
}

# Les cellules d'une colonne Excel (nombres, texte, dates, valeurs logiques ; NA pour une
# cellule vide ou une erreur) : des nombres si ce n'en sont que, sinon du texte (les nombres
# avec 15 chiffres significatifs). Une date devient du texte, comme dans un fichier texte
# (texte_dates), et non le nombre de jours qu'Excel garde.
cellules_excel <- function(cellules) {
  type <- vapply(cellules, function(e) {
    if (inherits(e, "POSIXct")) "date" else if (is.numeric(e)) "nombre" else if (is.character(e)) "texte"
    else if (is.logical(e) && !is.na(e)) "logique" else "vide"
  }, "")
  x <- rep(NA_real_, length(cellules))
  x[type == "nombre"] <- unlist(cellules[type == "nombre"])
  if (all(type %in% c("nombre", "vide"))) return(x)
  x <- as.character(x)
  x[type == "texte"] <- unlist(cellules[type == "texte"])
  x[type == "logique"] <- as.character(unlist(cellules[type == "logique"]))
  if (any(type == "date")) x[type == "date"] <- texte_dates(vapply(cellules[type == "date"], as.numeric, 0))
  x
}

# Des dates et heures d'Excel (secondes depuis 1970, arrondies à la seconde) en texte :
# « 2024-05-03 » pour des jours, « 08:30 » pour des heures seules (le jour 0 d'Excel :
# 1899-12-31, ou 1904-01-01 dans les classeurs des anciens Mac), « 2024-05-03 08:30 » sinon,
# avec les secondes s'il y en a.
texte_dates <- function(secondes) {
  s <- round(secondes)
  heure <- s %% 86400
  format_date <- if (all(s %/% 86400 %in% c(-25568, -24107))) "%H:%M" else if (all(heure == 0)) "%Y-%m-%d" else "%Y-%m-%d %H:%M"
  if (format_date != "%Y-%m-%d" && any(heure %% 60 != 0)) format_date <- paste0(format_date, ":%S")
  format(as.POSIXct(s, origin = "1970-01-01", tz = "UTC"), format_date, tz = "UTC")
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
# grands nombres. L'en-tête donne sa longueur (32 octets par colonne) et celle d'une ligne sur
# 16 bits : 2046 colonnes et 65 535 octets par ligne au plus, ce que lisent LibreOffice et les
# SIG qui passent par GDAL (QGIS) ; au-delà, le fichier serait faux, on refuse. (ArcGIS et
# dBase IV ne lisent que 255 colonnes, dBase III 128.)
ecrire_dbf <- function(df, chemin) {
  if (length(df) > 2046) {
    stop(sprintf(traduire("Trop de colonnes pour un fichier dBase : %d, pour 2046 au plus (255 pour ArcGIS). Choisissez un autre format (Excel, CSV...).",
                          "Too many columns for a dBase file: %d, for at most 2046 (255 for ArcGIS). Choose another format (Excel, CSV...)."), length(df)), call. = FALSE)
  }
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
  if (1 + sum(largeurs) > 65535) {
    stop(sprintf(traduire("Lignes trop longues pour un fichier dBase : %d octets, pour 65 535 au plus (textes longs ou nombreuses colonnes). Choisissez un autre format (Excel, CSV...).",
                          "Rows too long for a dBase file: %d bytes, for at most 65,535 (long texts or many columns). Choose another format (Excel, CSV...)."), 1 + sum(largeurs)), call. = FALSE)
  }
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
  # l'archive est écrite à côté, puis remplace l'ancien fichier (nouveau.R) : un échec en
  # route laisse l'ancien intact
  ecrire_sans_risque(chemin, function(f) {
    unlink(f)
    zip::zip(normalizePath(f, mustWork = FALSE), files = fichiers, root = dossier)
  })
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
  if (inherits(m, "modele_regression")) return(tables_regression(projet, m))
  s <- m$statistiques
  obs <- s$observations
  obs <- cbind(data.frame(Observation = unname(projet$noms_obs[rownames(obs)]), stringsAsFactors = FALSE), obs)
  rownames(obs) <- NULL
  list(observations = obs, variables = s$variables, composantes = s$composantes,
       coefficients = s$coefficients, variables_y = s$variables_y)
}
