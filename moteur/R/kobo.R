# Exports de KoboToolbox et d'ODK Central : Fichier › Importer depuis KoboToolbox ou ODK, et
# Fichier › Ouvrir (ou un fichier déposé sur la fenêtre), qui les reconnaît et propose l'import.
#
# Une équipe de terrain saisit ses relevés sur un téléphone (KoboCollect, ODK Collect, Enketo) ;
# le serveur les exporte :
#   - KoboToolbox : un classeur Excel, une feuille pour les envois et une par groupe répété
#     (reliée à sa table parente par _parent_table_name et _parent_index, l'_index de la ligne
#     parente), ou un CSV séparé par des points-virgules ; les en-têtes sont les noms des
#     questions (« valeurs XML »), précédés de leurs groupes si l'export le demande
#     (group_site/altitude), ou leurs libellés ;
#   - ODK Central : une archive .zip d'un CSV par table (les envois, puis chaque groupe répété,
#     relié par PARENT_KEY à la clé KEY de sa ligne parente), ou le seul CSV des envois ; les
#     groupes séparés par des tirets (group_site-altitude), toujours les noms.
# Ces tables ont des colonnes de métadonnées (identifiants, date d'envoi, appareil, version du
# formulaire…), des réponses à choix multiples écrites en codes séparés par des espaces
# (« soleil vent ») et, si l'export le demande, une colonne 0/1 par choix (meteo/soleil), des points
# GPS en texte (« latitude longitude altitude précision ») ou en colonnes (_gps_latitude… ;
# gps-Latitude… pour ODK), des dates et heures ISO avec leur fuseau (2024-05-03T08:12:45.123+02:00).
# L'import en fait une table d'analyse (importer_collecte, son aperçu : apercu_collecte) :
#   - les métadonnées retirées, sauf celles qu'on garde : la date d'envoi, son auteur, son
#     identifiant, le début, la fin et le jour du relevé, le statut de validation (ROLES_COLLECTE) ;
#   - les préfixes des groupes retirés des noms, chaque nom restant unique (kobo_noms_courts) ;
#   - une réponse à choix unique en variable qualitative, une réponse à choix multiples en colonnes
#     0/1 (celles de l'export quand il les a, sans les doubler) ;
#   - un point GPS en longitude et latitude, les coordonnées du projet en WGS 84 (spatial.R) ;
#   - les dates et heures en texte que Nuée lit comme le temps (« 2024-05-03 08:12 », l'heure du
#     téléphone, sans le fuseau ; variables.R : temps_en_nombres) : une variable qualitative quand
#     des dates reviennent, un identifiant quand toutes diffèrent, comme à l'ouverture d'un tableau ;
#   - la table choisie : les envois, ou un groupe répété avec des colonnes de ses tables parentes
#     jointes à chaque ligne (une ligne par espèce observée, avec l'habitat et les coordonnées du
#     site).
# Le formulaire XLSForm (un classeur aux feuilles survey et choices), s'il est donné, dit le type
# de chaque question, les libellés des choix dans la langue choisie (label::Français (fr),
# label::English (en), ou label) et leur ordre, qui devient celui des catégories ; les colonnes
# prennent alors les noms des questions, et une colonne 0/1 s'appelle « question_code »
# (meteo_soleil). Sans lui, les réponses restent telles que l'export les écrit (des codes, ou des
# libellés), les colonnes gardent les noms de l'export (sans leurs groupes), et une réponse à choix
# multiples sans ses colonnes 0/1 reste du texte.

# Les colonnes que KoboToolbox et ODK Central ajoutent aux réponses : deux d'entre elles
# reconnaissent un export (kobo_outil).
COLONNES_KOBO <- c("_id", "_uuid", "_submission_time", "_index", "__version__", "_validation_status", "_submitted_by",
                   "_status", "_notes", "_tags", "_xform_id_string", "formhub/uuid", "_parent_index", "_parent_table_name",
                   "_attachments", "_geolocation")
COLONNES_ODK <- c("SubmissionDate", "KEY", "PARENT_KEY", "meta-instanceID", "SubmitterID", "SubmitterName", "ReviewState",
                  "FormVersion", "AttachmentsPresent", "AttachmentsExpected", "Status", "DeviceID", "Edits")

# Les métadonnées qu'on peut garder (les autres sont toujours retirées) : leur rôle, et ce qu'elles
# sont, pour l'aperçu.
ROLES_COLLECTE <- c("envoi", "auteur", "identifiant", "releve", "validation")
NOMS_ROLES_COLLECTE <- list(
  envoi = c("la date et l'heure d'envoi", "the submission date and time"),
  auteur = c("l'auteur de l'envoi", "the submitter"),
  identifiant = c("l'identifiant de l'envoi", "the submission ID"),
  releve = c("le début, la fin et le jour du relevé", "the start, end and day of the survey"),
  validation = c("le statut de validation", "the validation status"))

# Les types de questions de métadonnées (XLSForm) et leur rôle.
TYPES_METADONNEES_COLLECTE <- c(start = "releve", end = "releve", today = "releve", username = "auteur", deviceid = "autre",
                                subscriberid = "autre", simserial = "autre", phonenumber = "autre", email = "autre",
                                audit = "autre", "background-audio" = "autre")
# Les types de questions dont les réponses restent du texte (le formulaire le dit), et ceux qui
# sont des nombres.
TYPES_TEXTE_COLLECTE <- c("text", "barcode", "image", "audio", "video", "file", "geotrace", "geoshape", "acknowledge",
                          "rank", "select_one_from_file", "select_multiple_from_file", "osm", "xml-external", "csv-external")
TYPES_NOMBRES_COLLECTE <- c("integer", "int", "decimal", "range")
EXTENSIONS_COLLECTE <- c("csv", "txt", "tsv", "tab", "dat", "xlsx", "xlsm", "xls", "zip")

# Les lectures gardées d'une demande à l'autre (l'aperçu est redemandé à chaque réglage) : les
# quatre dernières, par fichier (son chemin, sa taille et sa date).
MEMO_COLLECTE <- new.env(parent = emptyenv())
kobo_memo <- function(cle, calcul) {
  e <- MEMO_COLLECTE$entrees %||% list()
  if (!is.null(e[[cle]])) return(e[[cle]])
  v <- calcul()
  MEMO_COLLECTE$entrees <- utils::tail(c(e[names(e) != cle], stats::setNames(list(v), cle)), 4)
  v
}
kobo_cle_fichier <- function(chemin) {
  i <- file.info(chemin)
  paste(normalizePath(chemin, winslash = "/", mustWork = FALSE), i$size, as.numeric(i$mtime))
}

# Des valeurs en texte, sans espaces autour, "" manquant ; un nombre lu dans Excel écrit sans
# notation scientifique (l'_index 100000, pas « 1e+05 »).
kobo_texte <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  v <- if (is.numeric(x)) ifelse(is.na(x), NA_character_, sprintf("%.15g", x)) else trimws(as.character(x))
  v[!is.na(v) & !nzchar(v)] <- NA
  v
}

# Les premiers éléments d'une liste de noms, pour une phrase : « a, b, c… (+12) ».
kobo_liste <- function(x, n = 8) {
  x <- as.character(x)
  if (length(x) <= n) return(paste(x, collapse = ", "))
  sprintf("%s… (+%d)", paste(x[seq_len(n)], collapse = ", "), length(x) - n)
}

kobo_guillemets <- function(x) sprintf(traduire("« %s »", "“%s”"), x)

# Un élément d'un vecteur nommé, NA s'il n'y est pas (par sa place : le nom "", celui de la
# colonne label d'un formulaire, ne s'indexe pas).
kobo_element <- function(v, nom) {
  k <- match(nom, names(v))
  if (is.na(k)) NA_character_ else v[[k]]
}

# ---------------------------------------------------------------- reconnaître un export

# L'outil qui a écrit un tableau, d'après ses colonnes : "kobo", "odk", ou NULL.
kobo_outil <- function(noms) {
  k <- sum(noms %in% COLONNES_KOBO)
  o <- sum(noms %in% COLONNES_ODK)
  if (max(k, o) < 2) return(NULL)
  if (k >= o) "kobo" else "odk"
}

# Les noms des colonnes d'un fichier texte : sa première ligne seulement, en UTF-8 quel que soit
# son encodage (formats.R : texte_fichier).
kobo_entete_texte <- function(chemin) {
  octets <- readBin(chemin, "raw", min(file.size(chemin), 262144))
  if (length(octets) >= 2 && octets[1] == as.raw(0xff) && octets[2] == as.raw(0xfe)) {
    o <- octets[-(1:2)]
    texte <- iconv(list(o[seq_len(length(o) - length(o) %% 2)]), "UTF-16LE", "UTF-8")
  } else {
    if (length(octets) >= 3 && all(octets[1:3] == as.raw(c(0xef, 0xbb, 0xbf)))) octets <- octets[-(1:3)]
    fin <- which(octets == as.raw(0x0a))[1]
    if (!is.na(fin)) octets <- octets[seq_len(fin)]
    texte <- rawToChar(octets[octets != as.raw(0)])
    if (!validUTF8(texte)) texte <- iconv(texte, "CP1252", "UTF-8")
  }
  if (is.na(texte)) return(character(0))
  premiere <- sub("\r$", "", strsplit(texte, "\n", fixed = TRUE)[[1]][1])
  if (is.na(premiere) || !nzchar(trimws(premiere))) return(character(0))
  names(lire_texte_delimite(premiere))
}

# Les CSV d'une archive .zip (pas ceux des pièces jointes, media/).
kobo_csv_archive <- function(chemin) {
  liste <- utils::unzip(chemin, list = TRUE)
  liste$Name[grepl("\\.csv$", liste$Name, ignore.case = TRUE) & !grepl("(^|/)(__MACOSX|media)/", liste$Name)]
}

# Un CSV d'une archive, sorti seul dans un dossier temporaire (sans ses dossiers : rien ne peut
# s'écrire ailleurs), que `lire` lit avant que le dossier ne s'efface.
kobo_sortir_csv <- function(chemin, entree, lire) {
  dossier <- tempfile("odk")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE))
  utils::unzip(chemin, files = entree, exdir = dossier, junkpaths = TRUE)
  lire(file.path(dossier, basename(entree)))
}

# Un export de KoboToolbox ou d'ODK Central : list(outil, format) ; NULL pour un autre fichier. Ne lit
# que les en-têtes : Fichier › Ouvrir le demande pour chaque tableau ouvert.
detecter_collecte <- function(chemin) {
  ext <- extension(chemin)
  if (!ext %in% EXTENSIONS_COLLECTE || !file.exists(chemin)) return(NULL)
  tryCatch({
    noms <- switch(ext,
      zip = unlist(lapply(kobo_csv_archive(chemin), function(e) kobo_sortir_csv(chemin, e, kobo_entete_texte))),
      xlsx = , xlsm = , xls = names(readxl::read_excel(chemin, sheet = 1, n_max = 0, .name_repair = "minimal")),
      kobo_entete_texte(chemin))
    outil <- kobo_outil(noms)
    if (is.null(outil)) NULL else list(outil = outil, format = ext)
  }, error = function(e) NULL)
}

# Le refus d'un fichier qui n'est pas un export (un formulaire XLSForm donné à sa place est nommé).
kobo_refus_export <- function(chemin) {
  formulaire <- extension(chemin) %in% c("xlsx", "xlsm", "xls") &&
    tryCatch(all(c("survey", "choices") %in% tolower(readxl::excel_sheets(chemin))), error = function(e) FALSE)
  if (formulaire) {
    stop(sprintf(traduire("%s est un formulaire XLSForm (feuilles survey et choices), pas les données : donnez-le comme formulaire, à côté de l'export des données.",
                          "%s is an XLSForm form (survey and choices sheets), not the data: give it as the form, beside the export of the data."), basename(chemin)), call. = FALSE)
  }
  stop(sprintf(traduire("%s n'est pas un export de KoboToolbox ni d'ODK Central : il n'a pas leurs colonnes (_id, _uuid, _submission_time, _index… ; SubmissionDate, KEY, meta-instanceID…). Ouvrez-le avec Fichier › Ouvrir.",
                        "%s isn't a KoboToolbox or ODK Central export: it doesn't have their columns (_id, _uuid, _submission_time, _index…; SubmissionDate, KEY, meta-instanceID…). Open it with File › Open."), basename(chemin)), call. = FALSE)
}

# ---------------------------------------------------------------- lire un export

# Une table d'un export : ses données telles qu'écrites (formats.R : vers_data_frame), la
# virgule des nombres, la clé de chaque ligne (_index, KEY) et celle de sa ligne parente
# (_parent_index, PARENT_KEY), le nom de la table parente (_parent_table_name).
kobo_table <- function(nom, df, virgule) {
  df <- vers_data_frame(df)
  cle <- function(n) if (n %in% names(df)) kobo_texte(df[[n]])
  parente <- if ("_parent_table_name" %in% names(df)) stats::na.omit(kobo_texte(df[["_parent_table_name"]]))
  list(nom = nom, donnees = df, virgule = virgule, cle = cle("_index") %||% cle("KEY"),
       cle_parent = cle("_parent_index") %||% cle("PARENT_KEY"), nom_parent = if (length(parente)) parente[1], parent = NULL)
}

# Les tables d'un classeur de KoboToolbox : la première feuille (les envois), puis celles des
# groupes répétés (les feuilles qui ont _parent_index).
kobo_tables_classeur <- function(chemin) {
  tables <- list()
  for (f in readxl::excel_sheets(chemin)) {
    df <- lire_excel(chemin, f)
    if (!ncol(df) || (length(tables) && !"_parent_index" %in% names(df))) next
    tables[[length(tables) + 1]] <- kobo_table(f, df, "decimale")
  }
  tables
}

# Les tables d'une archive d'ODK Central : un CSV par table, celui des envois d'abord ; un groupe
# répété (« formulaire-observation.csv ») prend le nom de sa répétition (« observation »).
kobo_tables_archive <- function(chemin) {
  entrees <- kobo_csv_archive(chemin)
  if (!length(entrees)) {
    stop(sprintf(traduire("%s n'a aucun fichier CSV : ce n'est pas un export d'ODK Central.", "%s holds no CSV file: it isn't an ODK Central export."), basename(chemin)), call. = FALSE)
  }
  tables <- lapply(entrees, function(e) kobo_sortir_csv(chemin, e, function(f) {
    df <- lire_texte_delimite(texte_fichier(f))
    kobo_table(tools::file_path_sans_ext(basename(e)), df, attr(df, "virgule") %||% "decimale")
  }))
  principale <- which(vapply(tables, function(t) is.null(t$cle_parent), TRUE))[1]
  if (is.na(principale)) principale <- 1
  tables <- c(tables[principale], tables[-principale])
  racine <- paste0(tables[[1]]$nom, "-")
  for (i in seq_along(tables)[-1]) if (startsWith(tables[[i]]$nom, racine)) tables[[i]]$nom <- substring(tables[[i]]$nom, nchar(racine) + 1)
  tables
}

# La table parente d'une table de répétition : celle que nomme _parent_table_name (KoboToolbox ;
# le nom d'une feuille est coupé à 31 caractères), sinon celle dont les clés KEY ont les
# PARENT_KEY de la table (ODK), sinon la table des envois.
kobo_parent <- function(tables, i) {
  t <- tables[[i]]
  autres <- setdiff(seq_along(tables), i)
  noms <- vapply(tables[autres], function(x) x$nom, "")
  if (!is.null(t$nom_parent)) {
    simple <- function(x) tolower(gsub("[^[:alnum:]]+", "", x))
    np <- simple(t$nom_parent)
    s <- simple(noms)
    j <- which(s == np)
    if (!length(j)) j <- which(nzchar(s) & (startsWith(np, s) | startsWith(s, np)))
    if (length(j)) return(noms[j[which.max(nchar(s[j]))]])
  }
  cles <- unique(stats::na.omit(t$cle_parent))
  for (j in seq_along(autres)) if (length(cles) && any(cles %in% tables[[autres[j]]]$cle)) return(noms[j])
  tables[[1]]$nom
}

# Les tables d'un export (gardées entre deux demandes) : list(outil ("kobo" ou "odk"), format,
# tables (par nom : kobo_table, avec `parent`), principale (le nom de la table des envois)). Refuse
# un fichier qui n'en est pas un.
lire_collecte <- function(chemin) {
  if (!file.exists(chemin)) stop(traduire("Fichier introuvable : ", "File not found: "), chemin, call. = FALSE)
  ext <- extension(chemin)
  if (!ext %in% EXTENSIONS_COLLECTE) kobo_refus_export(chemin)
  kobo_memo(paste("export", kobo_cle_fichier(chemin)), function() {
    tables <- switch(ext,
      zip = kobo_tables_archive(chemin),
      xlsx = , xlsm = , xls = kobo_tables_classeur(chemin),
      { df <- lire_texte_delimite(texte_fichier(chemin)); list(kobo_table(tools::file_path_sans_ext(basename(chemin)), df, attr(df, "virgule") %||% "decimale")) })
    if (!length(tables)) kobo_refus_export(chemin)
    outil <- kobo_outil(unlist(lapply(tables, function(t) names(t$donnees))))
    if (is.null(outil)) kobo_refus_export(chemin)
    noms <- make.unique(vapply(tables, function(t) t$nom, ""), sep = "_")
    for (i in seq_along(tables)) tables[[i]]$nom <- noms[i]
    for (i in seq_along(tables)[-1]) if (!is.null(tables[[i]]$cle_parent)) tables[[i]]$parent <- kobo_parent(tables, i)
    list(outil = outil, format = ext, tables = stats::setNames(tables, noms), principale = noms[1])
  })
}

# Les tables parentes d'une table, de la plus proche à celle des envois.
kobo_ancetres <- function(export, nom) {
  r <- character(0)
  p <- export$tables[[nom]]$parent
  while (!is.null(p) && !p %in% c(r, nom) && length(r) < 20) {
    r <- c(r, p)
    p <- export$tables[[p]]$parent
  }
  r
}

# ---------------------------------------------------------------- le formulaire (XLSForm)

# Un libellé sans balises HTML (un formulaire peut colorer ses libellés).
kobo_nettoyer <- function(x) {
  x <- trimws(gsub("<[^>]*>", "", as.character(x)))
  x[!is.na(x) & !nzchar(x)] <- NA
  x
}

# Le formulaire XLSForm d'un export (gardé entre deux demandes) : list(fichier, titre, id,
# version, defaut (default_language), langues (celles des colonnes label::<langue> ; "" pour la
# colonne label), questions, choix). Chaque question : son nom, son type (select_one, integer,
# date, geopoint…), sa liste de choix, `autre` (or_other), ses groupes et répétitions du plus
# large au plus fin (`groupes`, avec leurs libellés), la répétition qui la contient
# (`repetition`, NA pour la table des envois), ses libellés par langue. Les choix d'une liste :
# leurs noms (les codes), dans l'ordre du formulaire, et leurs libellés par langue. `cles` : les
# noms par lesquels un export désigne ses questions (kobo_cles_formulaire).
lire_formulaire <- function(chemin) {
  if (!file.exists(chemin)) stop(traduire("Fichier introuvable : ", "File not found: "), chemin, call. = FALSE)
  pas_formulaire <- function() {
    stop(sprintf(traduire("%s n'est pas un formulaire XLSForm : il faut un classeur Excel avec une feuille « survey » (et « choices »).",
                          "%s isn't an XLSForm form: an Excel workbook with a “survey” sheet (and “choices”) is needed."), basename(chemin)), call. = FALSE)
  }
  if (!extension(chemin) %in% c("xlsx", "xlsm", "xls")) pas_formulaire()
  kobo_memo(paste("formulaire", kobo_cle_fichier(chemin)), function() {
    feuilles <- tryCatch(readxl::excel_sheets(chemin), error = function(e) pas_formulaire())
    trouver <- function(n) feuilles[tolower(trimws(feuilles)) == n][1]
    if (is.na(trouver("survey"))) pas_formulaire()
    lire <- function(f) {
      if (is.na(f)) return(NULL)
      x <- readxl::read_excel(chemin, sheet = f, col_types = "text", .name_repair = "minimal")
      stats::setNames(lapply(x, as.character), trimws(names(x)))
    }
    colonne <- function(x, ...) {
      k <- match(tolower(c(...)), tolower(names(x)))
      k <- k[!is.na(k)][1]
      n <- length(x[[1]] %||% character(0))
      if (is.na(k)) rep(NA_character_, n) else trimws(x[[k]])
    }
    # les libellés : une colonne par langue, « label » (la langue "") ou « label::Français (fr) »
    libelles <- function(x) {
      n <- length(x[[1]] %||% character(0))
      k <- grep("^label(::|$)", names(x), ignore.case = TRUE)
      langues <- trimws(sub("^label(::)?", "", names(x)[k], ignore.case = TRUE))
      garder <- !duplicated(langues)
      if (!length(k)) return(matrix(NA_character_, n, 0))
      M <- matrix(unlist(lapply(x[k[garder]], kobo_nettoyer)), nrow = n, ncol = sum(garder))
      colnames(M) <- langues[garder]
      M
    }
    # (une ligne de libellés, nommée par langue même quand il n'y en a qu'une)
    ligne <- function(M, i) stats::setNames(as.character(M[i, ]), colnames(M))
    s <- lire(trouver("survey"))
    ch <- lire(trouver("choices"))
    st <- lire(trouver("settings"))
    types <- colonne(s, "type")
    noms <- colonne(s, "name")
    Ls <- libelles(s)
    pile <- list()
    questions <- list()
    for (i in seq_along(types)) {
      ty <- types[i]
      if (is.na(ty) || !nzchar(ty)) next
      mots <- strsplit(ty, "\\s+")[[1]]
      bas <- tolower(mots)
      base <- bas[1]
      reste <- mots[-1]
      if (base %in% c("begin", "end") && length(bas) >= 2) {
        base <- paste(bas[1:2], collapse = "_")
        reste <- mots[-(1:2)]
      }
      if (base == "select" && length(bas) >= 3 && bas[2] %in% c("one", "multiple")) {
        base <- paste0("select_", bas[2])
        reste <- mots[-(1:2)]
      }
      if (base %in% c("begin_group", "begin_repeat")) {
        pile <- c(pile, list(list(nom = if (is.na(noms[i])) "" else noms[i], repetition = base == "begin_repeat", libelles = ligne(Ls, i))))
        next
      }
      if (base %in% c("end_group", "end_repeat")) {
        if (length(pile)) pile <- pile[-length(pile)]
        next
      }
      if (is.na(noms[i]) || !nzchar(noms[i])) next
      nommes <- Filter(function(g) nzchar(g$nom), pile)
      repetitions <- Filter(function(g) g$repetition, nommes)
      questions[[length(questions) + 1]] <- list(
        nom = noms[i], type = base, liste = if (base %in% c("select_one", "select_multiple") && length(reste)) reste[1] else NA_character_,
        autre = "or_other" %in% tolower(reste), groupes = vapply(nommes, function(g) g$nom, ""),
        libelles_groupes = lapply(nommes, function(g) g$libelles),
        repetition = if (length(repetitions)) repetitions[[length(repetitions)]]$nom else NA_character_,
        libelles = ligne(Ls, i))
    }
    choix <- list()
    Lc <- NULL
    if (!is.null(ch)) {
      ln <- colonne(ch, "list_name", "list name")
      nm <- colonne(ch, "name")
      Lc <- libelles(ch)
      for (l in unique(ln[!is.na(ln) & nzchar(ln)])) {
        k <- which(ln == l & !is.na(nm) & nzchar(nm))
        choix[[l]] <- list(noms = nm[k], libelles = Lc[k, , drop = FALSE])
      }
    }
    reglage <- function(n) {
      if (is.null(st)) return(NULL)
      v <- colonne(st, n)
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v)) v[1]
    }
    form <- list(fichier = basename(chemin), titre = reglage("form_title"), id = reglage("form_id"), version = reglage("version"),
                 defaut = reglage("default_language"), langues = unique(c(colnames(Ls), colnames(Lc))), questions = questions, choix = choix)
    form$cles <- kobo_cles_formulaire(form)
    form
  })
}

# La langue des libellés : celle demandée (une langue du formulaire, "" pour la colonne label),
# sinon celle de l'interface si le formulaire l'a (« (fr) », « Français »…), sinon sa langue par
# défaut (default_language), la colonne label, la première.
kobo_langue <- function(form, langue = NULL) {
  if (is.null(form)) return(NULL)
  L <- form$langues
  if (!length(L)) return("")
  if (length(langue) && !is.na(langue[1])) {
    langue <- as.character(langue[1])
    if (!langue %in% L) {
      stop(sprintf(traduire("Le formulaire n'a pas la langue %s ; il a : %s.", "The form doesn't have the language %s; it has: %s."),
                   kobo_guillemets(langue), paste(kobo_guillemets(ifelse(nzchar(L), L, "label")), collapse = ", ")), call. = FALSE)
    }
    return(langue)
  }
  motif <- if (identical(getOption("nuee.langue"), "en")) "\\(en\\)|english|anglais" else "\\(fr\\)|fran[cç]ais|french"
  k <- grep(motif, L, ignore.case = TRUE)
  if (length(k)) return(L[k[1]])
  if (length(form$defaut)) {
    k <- which(L == form$defaut | startsWith(L, form$defaut))
    if (length(k)) return(L[k[1]])
  }
  if ("" %in% L) return("")
  L[1]
}

# Les libellés d'une liste de choix dans une langue : la langue demandée, sinon la colonne label,
# sinon une autre langue, sinon le code ; deux choix au même libellé se distinguent par leur code.
kobo_libelles_choix <- function(liste, langue) {
  L <- liste$libelles
  lib <- rep(NA_character_, length(liste$noms))
  for (col in intersect(unique(c(langue, "", colnames(L))), colnames(L))) {
    v <- L[, match(col, colnames(L))]
    prendre <- is.na(lib) & !is.na(v)
    lib[prendre] <- v[prendre]
  }
  lib[is.na(lib)] <- liste$noms[is.na(lib)]
  double <- lib %in% lib[duplicated(lib)]
  lib[double] <- sprintf("%s (%s)", lib[double], liste$noms[double])
  lib
}

# Le libellé d'une question ou d'un groupe dans une langue (son nom sans libellé).
kobo_libelle <- function(libelles, langue, nom) {
  v <- kobo_element(libelles, langue)
  if (is.na(v)) v <- kobo_element(libelles, "")
  if (is.na(v)) nom else v
}

# Les codes des choix tels qu'un export en libellés d'une langue les écrit : leur libellé dans
# cette langue, sinon celui de la colonne label, sinon leur nom.
kobo_codes_langue <- function(liste, langue) {
  L <- liste$libelles
  v <- rep(NA_character_, length(liste$noms))
  for (col in intersect(c(langue, ""), colnames(L))) v[is.na(v)] <- L[is.na(v), match(col, colnames(L))]
  ifelse(is.na(v), liste$noms, v)
}

# Les noms par lesquels un export désigne les questions du formulaire (calculés une fois par
# formulaire : lire_formulaire) : une ligne par nom possible, avec la question, ce qu'est la
# colonne (la question, un choix d'une question à choix multiples, une partie d'un point GPS), son
# détail (le code du choix, la partie du point), sa longueur (le nombre de groupes et de questions
# qu'il nomme), s'il nomme tous les groupes (`complet`) et sa priorité (0 : fait des noms ; 1 : des
# libellés). Une question : son nom, précédé de ses groupes (tous, les plus proches ou aucun),
# séparés par « / », « - », « . » ou « : » ; ou son libellé dans une langue, précédé de même de
# ceux de ses groupes séparés par « / » (un export en libellés de KoboToolbox). Ses colonnes 0/1 :
# ce nom, le séparateur ou « / », puis le code du choix ; ce libellé, « / », puis le libellé du
# choix dans la même langue. Les colonnes d'un point : _<nom>_latitude (KoboToolbox, aussi
# <groupes>/_<nom>_latitude), <nom>-Latitude (ODK). Les lignes sont rangées pour qu'un nom donné à
# plusieurs questions désigne d'abord celle qu'il nomme en entier.
kobo_cles_formulaire <- function(form) {
  parties_kobo <- c("latitude", "longitude", "altitude", "precision")
  parties_odk <- c("Latitude", "Longitude", "Altitude", "Accuracy")
  champs <- list(cle = list(), question = list(), genre = list(), detail = list(), longueur = list(), complet = list(), priorite = list())
  ajouter <- function(cle, i, genre, detail, longueur, complet, priorite) {
    n <- length(cle)
    k <- length(champs$cle) + 1
    valeurs <- list(cle = cle, question = rep(i, n), genre = rep(genre, n), detail = rep_len(detail, n), longueur = rep_len(longueur, n),
                    complet = rep_len(complet, n), priorite = rep_len(priorite, n))
    for (ch in names(champs)) champs[[ch]][[k]] <<- valeurs[[ch]]
  }
  for (i in seq_along(form$questions)) {
    q <- form$questions[[i]]
    liste <- if (!is.na(q$liste)) form$choix[[q$liste]]
    multiple <- q$type == "select_multiple" && !is.null(liste) && length(liste$noms)
    chemins <- c(list(list(seg = c(q$groupes, q$nom), seps = c("/", "-", ".", ":"), priorite = 0L, codes = if (multiple) liste$noms)),
                 lapply(form$langues, function(L) list(
                   seg = c(vapply(seq_along(q$groupes), function(g) kobo_libelle(q$libelles_groupes[[g]], L, q$groupes[g]), ""), kobo_libelle(q$libelles, L, q$nom)),
                   seps = "/", priorite = 1L, codes = if (multiple) kobo_codes_langue(liste, L))))
    for (ch in chemins) {
      k <- seq_along(ch$seg)
      complet <- k == length(ch$seg)
      for (s in ch$seps) {
        base <- vapply(k, function(j) paste(utils::tail(ch$seg, j), collapse = s), "")
        ajouter(base, i, "question", NA_character_, k, complet, ch$priorite)
        if (multiple) for (s2 in unique(c(s, "/"))) {
          ajouter(c(outer(base, ch$codes, function(b, c) paste0(b, s2, c))), i, "choix", rep(liste$noms, each = length(base)),
                  rep(k + 1L, length(ch$codes)), rep(complet, length(ch$codes)), ch$priorite)
        }
        if (q$type %in% c("geopoint", "start-geopoint")) {
          avant <- vapply(k, function(j) paste(utils::head(utils::tail(ch$seg, j), -1), collapse = s), "")
          dernier <- ch$seg[length(ch$seg)]
          for (j in seq_along(parties_kobo)) {
            ajouter(paste0("_", base, "_", parties_kobo[j]), i, "point", parties_kobo[j], k, complet, ch$priorite)
            if (any(k > 1)) ajouter(paste0(avant[k > 1], s, "_", dernier, "_", parties_kobo[j]), i, "point", parties_kobo[j], k[k > 1], complet[k > 1], ch$priorite)
            ajouter(paste0(base, "-", parties_odk[j]), i, "point", parties_kobo[j], k, complet, ch$priorite)
          }
        }
      }
    }
  }
  cles <- as.data.frame(lapply(champs, function(x) unlist(x, use.names = FALSE)), stringsAsFactors = FALSE)
  cles <- cles[order(!cles$complet, -cles$longueur, cles$priorite, match(cles$genre, c("question", "choix", "point"))), , drop = FALSE]
  cles <- cles[!duplicated(cles[c("cle", "question")]), , drop = FALSE]
  rownames(cles) <- NULL
  cles
}

# La question du formulaire de chaque colonne d'une table : data.frame(question (indice, NA),
# genre, detail). `section` : la répétition de la table (NA : la table des envois), dont les
# questions passent avant celles des autres tables au même nom.
kobo_correspondances <- function(noms, form, cles, section) {
  repetitions <- vapply(form$questions, function(q) q$repetition, "")[cles$question]
  dans <- if (is.na(section)) is.na(repetitions) else !is.na(repetitions) & repetitions == section
  r <- cles[dans, , drop = FALSE][match(noms, cles$cle[dans]), c("question", "genre", "detail"), drop = FALSE]
  autres <- which(is.na(r$question))
  if (length(autres)) r[autres, ] <- cles[match(noms[autres], cles$cle), c("question", "genre", "detail"), drop = FALSE]
  rownames(r) <- NULL
  r
}

# La répétition du formulaire qu'est une table de l'export (son nom, une feuille coupée à 31
# caractères) : NA pour la table des envois, "" si aucune ne lui correspond.
kobo_section <- function(form, export, table) {
  if (identical(table, export$principale)) return(NA_character_)
  reps <- unique(stats::na.omit(vapply(form$questions, function(q) q$repetition, "")))
  simple <- function(x) tolower(gsub("[^[:alnum:]]+", "", x))
  s <- simple(table)
  k <- which(simple(reps) == s)
  if (!length(k)) k <- which(nzchar(s) & startsWith(simple(reps), s))
  if (length(k)) reps[k[1]] else ""
}

# ---------------------------------------------------------------- les colonnes d'une table

# Le rôle d'une colonne de métadonnées d'après son nom, dans un export de `outil` ; NA pour une
# réponse. `type` : celui de la question dans le formulaire, s'il est donné (un formulaire peut
# nommer autrement start et end). Les clés qui relient les tables (_index, KEY…) ont le rôle
# « lien », les autres métadonnées toujours retirées le rôle « autre ». Les copies des métadonnées
# de l'envoi dans les feuilles des répétitions de KoboToolbox (_submission__uuid…) sont des liens :
# celles de la table des envois s'y joignent quand on les garde.
role_metadonnee <- function(nom, outil, type = NA) {
  if (!is.na(type) && type %in% names(TYPES_METADONNEES_COLLECTE)) return(TYPES_METADONNEES_COLLECTE[[type]])
  if (identical(outil, "kobo")) {
    if (nom == "_submission_time") return("envoi")
    if (nom == "_submitted_by") return("auteur")
    if (nom %in% c("_uuid", "meta/instanceName")) return("identifiant")
    if (nom == "_validation_status") return("validation")
    if (nom %in% c("_index", "_parent_index", "_parent_table_name") || startsWith(nom, "_submission_")) return("lien")
    if (nom %in% COLONNES_KOBO || grepl("^_+version_*$", nom) || startsWith(nom, "meta/") || startsWith(nom, "formhub/") ||
        nom %in% c("_bamboo_dataset_id", "_media_count", "_total_media", "_media_all_received", "_supplementalDetails")) return("autre")
  } else {
    if (nom == "SubmissionDate") return("envoi")
    if (nom %in% c("SubmitterName", "SubmitterID")) return("auteur")
    if (nom %in% c("meta-instanceID", "meta-instanceName")) return("identifiant")
    if (nom == "ReviewState") return("validation")
    if (nom %in% c("KEY", "PARENT_KEY")) return("lien")
    if (nom %in% COLONNES_ODK || startsWith(nom, "meta-")) return("autre")
  }
  if (is.na(type)) {  # (sans formulaire : les questions de métadonnées par leur nom habituel)
    if (nom %in% c("start", "end", "today")) return("releve")
    if (nom == "username") return("auteur")
    if (nom %in% c("deviceid", "subscriberid", "simserial", "phonenumber", "audit")) return("autre")
  }
  NA_character_
}

# Un nom XML (celui d'une question ou d'un groupe d'un formulaire), pas un libellé.
kobo_nom_xml <- function(x) grepl("^[\\p{L}_][\\p{L}\\p{N}_.-]*$", x, perl = TRUE)

# Les groupes et le nom d'une colonne sans formulaire : coupé au séparateur des groupes (« / »
# pour KoboToolbox, « - » pour ODK). Les en-têtes d'un export en noms (`noms_xml` : ceux d'ODK
# toujours) se coupent à chaque séparateur ; ceux d'un export en libellés, seulement quand ce qui
# précède est fait de noms de groupes dont le premier est celui d'autres colonnes (« Vent (km/h) »
# reste entier).
kobo_segments <- function(nom, tous, sep, noms_xml) {
  s <- strsplit(nom, sep, fixed = TRUE)[[1]]
  if (length(s) < 2 || !all(nzchar(s))) return(nom)
  if (noms_xml) return(s)
  groupes <- s[-length(s)]
  if (!all(kobo_nom_xml(groupes)) || sum(startsWith(tous, paste0(groupes[1], sep))) < 2) return(nom)
  s
}

# Des noms courts et uniques : le dernier segment de chaque nom (la question) ; ceux qui
# reviennent prennent leurs groupes, du plus proche au plus large, jusqu'à ne plus revenir
# (site1_habitat, site2_habitat). `segments` : une liste de vecteurs (les groupes puis le nom).
kobo_noms_courts <- function(segments) {
  k <- rep(1L, length(segments))
  repeat {
    noms <- vapply(seq_along(segments), function(i) paste(utils::tail(segments[[i]], k[i]), collapse = "_"), "")
    double <- noms %in% noms[duplicated(noms)] & k < lengths(segments)
    if (!any(double)) return(noms)
    k[double] <- k[double] + 1L
  }
}

# Des cases 0/1 (1, 0, TRUE, FALSE…) en nombres, NA pour une case vide ; attr(, "valide") : toutes
# les valeurs écrites en sont.
kobo_zero_un <- function(x) {
  v <- tolower(kobo_texte(x))
  r <- rep(NA_real_, length(v))
  r[v %in% c("1", "true", "vrai", "yes", "oui")] <- 1
  r[v %in% c("0", "false", "faux", "no", "non")] <- 0
  structure(r, valide = all(is.na(v) | !is.na(r)))
}

# Des dates et heures ISO en texte que Nuée lit comme le temps : « 2024-05-03 08:12 », avec les
# secondes quand l'une des valeurs en a, sans fraction de seconde ni fuseau horaire (l'heure du
# téléphone qui a fait le relevé ; avec « Z », l'heure UTC du serveur) ; une date seule reste
# telle quelle, une heure seule devient « 08:12 ». attr(, "valide") : toutes les valeurs écrites
# sont des dates ou des heures (et il y en a).
kobo_dates <- function(x) {
  v <- kobo_texte(x)
  w <- ifelse(is.na(v), "", v)
  fuseau <- "(?:Z|[+-][0-9]{2}(?::?[0-9]{2})?)?"
  motif_date <- paste0("^([0-9]{4}-[0-9]{2}-[0-9]{2})(?:[T ]([0-9]{2}:[0-9]{2})(?::([0-9]{2}))?(?:[.,][0-9]+)?", fuseau, ")?$")
  motif_heure <- paste0("^([0-9]{2}:[0-9]{2})(?::([0-9]{2}))?(?:[.,][0-9]+)?", fuseau, "$")
  d <- regmatches(w, regexec(motif_date, w, perl = TRUE))
  h <- regmatches(w, regexec(motif_heure, w, perl = TRUE))
  date <- lengths(d) > 0
  heure <- !date & lengths(h) > 0
  partie <- function(l, j) vapply(l, function(x) x[j], "")
  secondes <- c(partie(d[date], 4), partie(h[heure], 3))
  avec_secondes <- any(nzchar(secondes) & secondes != "00")
  ecrire <- function(hm, s) ifelse(!nzchar(hm), "", paste0(hm, if (avec_secondes) paste0(":", ifelse(nzchar(s), s, "00")) else ""))
  r <- rep(NA_character_, length(v))
  if (any(date)) {
    hm <- ecrire(partie(d[date], 3), partie(d[date], 4))
    r[date] <- ifelse(nzchar(hm), paste(partie(d[date], 2), hm), partie(d[date], 2))
  }
  if (any(heure)) r[heure] <- ecrire(partie(h[heure], 2), partie(h[heure], 3))
  structure(r, valide = any(!is.na(v)) && all(is.na(v) | date | heure))
}

# Une colonne de points GPS écrits en texte (« 43.6119 3.8772 52.3 4.8 » : latitude, longitude,
# altitude, précision) : une matrice de quatre colonnes ; attr(, "valide") : toutes les valeurs
# écrites sont des points (latitude entre −90 et 90, longitude entre −180 et 180).
kobo_points_texte <- function(x) {
  v <- kobo_texte(x)
  parts <- lapply(strsplit(ifelse(is.na(v), "", v), "\\s+"), function(p) p[nzchar(p)])
  M <- matrix(NA_real_, length(v), 4)
  for (i in which(lengths(parts) == 4)) M[i, ] <- suppressWarnings(as.numeric(parts[[i]]))
  ecrit <- !is.na(v)
  ok <- ecrit & !is.na(M[, 1]) & !is.na(M[, 2]) & abs(M[, 1]) <= 90 & abs(M[, 2]) <= 180
  structure(M, valide = any(ecrit) && all(ok[ecrit]))
}

# Les colonnes d'une table d'un export, prêtes pour l'analyse (`garder` : les rôles des
# métadonnées gardées ; `cles` : kobo_cles_formulaire) : la liste de ses colonnes, chacune avec son
# nom, ses valeurs et ce qu'il faut en faire (`texte` : gardée telle quelle ; `nombres` : lue en
# nombres ; une variable qualitative quand ses valeurs sont un facteur), les noms de la longitude
# et de la latitude de son premier point (NULL sans point), et ce qui a été fait (le résumé).
kobo_preparer <- function(export, nom, form, langue, garder, cles) {
  tab <- export$tables[[nom]]
  df <- tab$donnees
  noms <- names(df)
  K <- length(noms)
  outil <- export$outil
  sep <- if (outil == "odk") "-" else "/"
  m <- if (!is.null(form)) kobo_correspondances(noms, form, cles, kobo_section(form, export, nom))
  question <- function(k) if (!is.null(m) && !is.na(m$question[k]) && m$genre[k] == "question") form$questions[[m$question[k]]]
  pris <- rep(FALSE, K)
  elements <- list()
  ajouter <- function(e) elements[[length(elements) + 1]] <<- e
  # (un export en noms, pas en libellés : chaque partie de chaque en-tête est un nom XML)
  noms_xml <- outil == "odk" || all(vapply(strsplit(noms, "/", fixed = TRUE), function(s) all(kobo_nom_xml(s)), TRUE))
  segments_sans <- function(n) kobo_segments(n, noms, sep, noms_xml)
  segments_q <- function(q) c(q$groupes, q$nom)
  resume <- list(retirees = character(0), gardees = character(0), roles = list(), renommees = list(), qualitatives = list(),
                 multiples = list(), points = list(), dates = character(0), hors_formulaire = character(0), texte_multiples = character(0),
                 du_formulaire = if (!is.null(m)) sum(!is.na(m$question)) else NA)

  # --- métadonnées
  for (k in seq_len(K)) {
    q <- question(k)
    role <- role_metadonnee(noms[k], outil, if (!is.null(q)) q$type else NA)
    if (is.na(role)) next
    pris[k] <- TRUE
    if (role %in% ROLES_COLLECTE) resume$roles[[role]] <- c(resume$roles[[role]], noms[k])
    if (!role %in% garder) {
      resume$retirees <- c(resume$retirees, noms[k])
      next
    }
    resume$gardees <- c(resume$gardees, noms[k])
    seg <- if (!is.null(q)) segments_q(q) else strsplit(noms[k], sep, fixed = TRUE)[[1]]
    seg[length(seg)] <- sub("^_+", "", seg[length(seg)])
    v <- kobo_dates(df[[k]])
    if (!role %in% c("envoi", "releve") || !isTRUE(attr(v, "valide"))) v <- kobo_texte(df[[k]])
    attr(v, "valide") <- NULL
    ajouter(list(position = k, sources = noms[k], genre = "metadonnee", colonnes = list(list(segments = seg, valeurs = v, texte = TRUE))))
  }

  # --- points GPS : les colonnes d'une question geopoint du formulaire, ou d'après leurs noms
  # (_gps_latitude… de KoboToolbox, gps-Latitude… d'ODK), ou une colonne de points en texte
  points <- list()
  if (!is.null(m)) {
    for (k in which(!pris & !is.na(m$question))) {
      q <- form$questions[[m$question[k]]]
      if (!q$type %in% c("geopoint", "start-geopoint") || !m$genre[k] %in% c("question", "point")) next
      cle <- as.character(m$question[k])
      p <- points[[cle]] %||% list(segments = segments_q(q), colonnes = character(0))
      p$colonnes[if (m$genre[k] == "question") "texte" else m$detail[k]] <- noms[k]
      points[[cle]] <- p
      pris[k] <- TRUE
    }
  }
  motifs <- list(c("^_(.+)_(latitude|longitude|altitude|precision)$", "\\1", "\\2"),
                 c("^(.+/)_([^/]+)_(latitude|longitude|altitude|precision)$", "\\1\\2", "\\3"),
                 c("^(.+)-(Latitude|Longitude|Altitude|Accuracy)$", "\\1", "\\2"))
  parties <- c(latitude = "latitude", longitude = "longitude", altitude = "altitude", precision = "precision", accuracy = "precision")
  candidats <- list()
  for (k in which(!pris)) {
    for (mo in motifs) {
      if (!grepl(mo[1], noms[k], perl = TRUE)) next
      base <- sub(mo[1], mo[2], noms[k], perl = TRUE)
      partie <- parties[[tolower(sub(mo[1], mo[3], noms[k], perl = TRUE))]]
      candidats[[base]] <- c(candidats[[base]], stats::setNames(noms[k], partie))
      break
    }
  }
  for (base in names(candidats)) {
    cols <- candidats[[base]]
    if (is.na(kobo_element(cols, "latitude")) || is.na(kobo_element(cols, "longitude"))) next
    if (base %in% noms[!pris]) cols["texte"] <- base
    points[[paste0("n:", base)]] <- list(segments = segments_sans(base), colonnes = cols)
    pris[noms %in% cols] <- TRUE
    if (!is.null(form)) resume$hors_formulaire <- c(resume$hors_formulaire, base)
  }
  # (une colonne de texte que le formulaire ne dit pas être une autre question ; ses valeurs
  # distinctes le disent)
  for (k in which(!pris)) {
    if (!is.character(df[[k]]) || (!is.null(m) && !is.na(m$question[k])) || !isTRUE(attr(kobo_points_texte(unique(df[[k]])), "valide"))) next
    points[[paste0("t:", noms[k])]] <- list(segments = segments_sans(noms[k]), colonnes = c(texte = noms[k]))
    pris[k] <- TRUE
    if (!is.null(form)) resume$hors_formulaire <- c(resume$hors_formulaire, noms[k])
  }
  for (p in points) {
    cols <- p$colonnes
    texte <- if (!is.na(kobo_element(cols, "texte"))) kobo_points_texte(df[[cols[["texte"]]]])
    valeur <- function(partie, j) {
      n <- kobo_element(cols, partie)
      if (!is.na(n)) return(lire_nombres(kobo_texte(df[[n]])))
      if (!is.null(texte)) return(texte[, j])
      rep(NA_real_, nrow(df))
    }
    lat <- valeur("latitude", 1)
    lon <- valeur("longitude", 2)
    hors <- (!is.na(lat) & abs(lat) > 90) | (!is.na(lon) & abs(lon) > 180)
    lat[hors] <- NA
    lon[hors] <- NA
    v <- list(longitude = lon, latitude = lat, altitude = valeur("altitude", 3), precision = valeur("precision", 4))
    v <- v[c(TRUE, TRUE, any(!is.na(v$altitude)), any(!is.na(v$precision)))]
    s <- p$segments
    colonnes <- lapply(names(v), function(n) list(segments = c(utils::head(s, -1), paste0(s[length(s)], "_", n)), valeurs = v[[n]], point = n))
    ajouter(list(position = min(match(cols, noms)), sources = unname(cols), genre = "point", nom = s[length(s)], colonnes = colonnes, hors = sum(hors)))
  }

  # --- choix multiples : une question select_multiple du formulaire (ses colonnes 0/1 dans l'export,
  # sinon ses réponses), ou d'après les noms (question/choix, des colonnes 0/1)
  multiples <- list()
  if (!is.null(m)) {
    for (k in which(!pris & !is.na(m$question))) {
      q <- form$questions[[m$question[k]]]
      if (q$type != "select_multiple" || !m$genre[k] %in% c("question", "choix")) next
      cle <- as.character(m$question[k])
      x <- multiples[[cle]] %||% list(segments = segments_q(q), question = q, texte = NA_character_, choix = character(0), codes = character(0))
      if (m$genre[k] == "question") x$texte <- noms[k] else {
        x$choix <- c(x$choix, noms[k])
        x$codes <- c(x$codes, m$detail[k])
      }
      multiples[[cle]] <- x
      pris[k] <- TRUE
    }
  }
  # (chaque préfixe une fois ; les cases 0/1 d'une colonne d'après ses valeurs distinctes, vues une fois)
  zero_un <- rep(NA, K)
  colonne_zero_un <- function(j) {
    if (is.na(zero_un[j])) zero_un[j] <<- isTRUE(attr(kobo_zero_un(unique(df[[j]])), "valide"))
    zero_un[j]
  }
  prefixes <- character(0)
  for (k in which(!pris & grepl("/", noms, fixed = TRUE))) {
    x <- sub("/[^/]*$", "", noms[k])
    if (pris[k] || !nzchar(x) || x %in% prefixes) next
    prefixes <- c(prefixes, x)
    suite <- substring(noms, nchar(x) + 2)
    freres <- which(!pris & startsWith(noms, paste0(x, "/")) & nzchar(suite) & !grepl("/", suite, fixed = TRUE))
    texte <- if (x %in% noms[!pris]) x else NA_character_
    if (is.na(texte) && length(freres) < 2) next
    if (!all(vapply(freres, colonne_zero_un, TRUE))) next
    multiples[[paste0("n:", x)]] <- list(segments = segments_sans(x), question = NULL, texte = texte, choix = noms[freres], codes = suite[freres])
    pris[freres] <- TRUE
    if (!is.na(texte)) pris[match(texte, noms)] <- TRUE
    if (!is.null(form)) resume$hors_formulaire <- c(resume$hors_formulaire, x)
  }
  for (x in multiples) {
    s <- x$segments
    q <- x$question
    sources <- c(stats::na.omit(x$texte), x$choix)
    colonne_choix <- function(code, valeurs) list(segments = c(utils::head(s, -1), paste0(s[length(s)], "_", code)), valeurs = valeurs, choix = code)
    reponses <- if (!is.na(x$texte)) kobo_texte(df[[x$texte]]) else rep(NA_character_, nrow(df))
    liste <- if (!is.null(q) && !is.na(q$liste)) form$choix[[q$liste]]
    if (length(x$choix)) {
      # les colonnes 0/1 de l'export, dans l'ordre des choix du formulaire ; une case vide d'une ligne
      # qui a répondu compte 0, une ligne sans réponse reste vide
      V <- lapply(x$choix, function(n) as.vector(kobo_zero_un(df[[n]])))
      ordre <- if (!is.null(liste)) order(match(x$codes, liste$noms)) else seq_along(x$choix)
      repondu <- !is.na(reponses) | Reduce(`|`, lapply(V, function(v) !is.na(v)), FALSE)
      colonnes <- lapply(ordre, function(j) { v <- V[[j]]; v[repondu & is.na(v)] <- 0; colonne_choix(x$codes[j], v) })
      origine <- "export"
    } else {
      # les réponses écrites en codes séparés par des espaces : une colonne par choix du formulaire,
      # dans son ordre, puis les codes qu'il n'a pas
      jetons <- strsplit(ifelse(is.na(reponses), "", reponses), "\\s+")
      vus <- unique(unlist(jetons))
      vus <- vus[nzchar(vus)]
      if (is.null(liste) || (length(vus) && !any(vus %in% liste$noms))) {
        # (des libellés, ou des codes qui ne sont pas ceux du formulaire : la réponse reste du texte)
        resume$texte_multiples <- c(resume$texte_multiples, x$texte)
        ajouter(list(position = match(x$texte, noms), sources = x$texte, genre = "question",
                     colonnes = list(list(segments = s, valeurs = reponses, texte = TRUE))))
        next
      }
      codes <- c(liste$noms, sort(setdiff(vus, liste$noms)))
      ligne <- rep.int(seq_along(jetons), lengths(jetons))
      jeton <- unlist(jetons, use.names = FALSE)
      colonnes <- lapply(codes, function(code) {
        v <- ifelse(is.na(reponses), NA_real_, 0)
        v[unique(ligne[jeton == code])] <- 1
        colonne_choix(code, v)
      })
      origine <- "reponses"
    }
    ajouter(list(position = min(match(sources, noms)), sources = sources, genre = "multiple", nom = s[length(s)], origine = origine, colonnes = colonnes))
  }

  # --- les autres colonnes : une question (choix unique, date, nombre, texte), ou une colonne
  # sans formulaire (des dates ISO lues comme telles, le reste comme un tableau ouvert)
  for (k in which(!pris)) {
    q <- question(k)
    x <- df[[k]]
    seg <- if (!is.null(q)) segments_q(q) else segments_sans(noms[k])
    col <- list(segments = seg, valeurs = x)
    genre <- "question"
    if (!is.null(form) && is.null(q)) resume$hors_formulaire <- c(resume$hors_formulaire, noms[k])
    type <- if (!is.null(q)) q$type else NA_character_
    liste <- if (!is.null(q) && !is.na(q$liste)) form$choix[[q$liste]]
    if (identical(type, "note")) {
      resume$retirees <- c(resume$retirees, noms[k])
      next
    }
    if (identical(type, "select_one") && !is.null(liste)) {
      if (q$autre && !"other" %in% liste$noms) {
        liste$noms <- c(liste$noms, "other")
        liste$libelles <- rbind(liste$libelles, matrix("Other", 1, ncol(liste$libelles), dimnames = list(NULL, colnames(liste$libelles))))
      }
      v <- kobo_texte(x)
      libelles <- kobo_libelles_choix(liste, langue)
      j <- match(v, liste$noms)
      for (L in seq_len(ncol(liste$libelles))) {  # (un export en libellés : le libellé d'une langue du formulaire)
        manque <- is.na(j) & !is.na(v)
        if (!any(manque)) break
        j[manque] <- match(v[manque], liste$libelles[, L])
      }
      inconnus <- sort(unique(v[is.na(j) & !is.na(v)]))
      col$valeurs <- factor(ifelse(is.na(j), v, libelles[j]), levels = unique(c(libelles, inconnus)))
      genre <- "choix_unique"
    } else if (type %in% c("date", "datetime", "time", "start", "end", "today") || (is.na(type) && is.character(x) && isTRUE(attr(kobo_dates(unique(x)), "valide")))) {
      v <- kobo_dates(x)
      col$texte <- TRUE
      if (isTRUE(attr(v, "valide")) || all(is.na(kobo_texte(x)))) {
        attr(v, "valide") <- NULL
        col$valeurs <- v
        genre <- "date"
        resume$dates <- c(resume$dates, seg[length(seg)])
      } else col$valeurs <- kobo_texte(x)
    } else if (type %in% TYPES_NOMBRES_COLLECTE) {
      col$nombres <- TRUE
    } else if (type %in% c(TYPES_TEXTE_COLLECTE, "select_one", "select_multiple")) {
      col$valeurs <- kobo_texte(x)
      col$texte <- TRUE
    }
    ajouter(list(position = k, sources = noms[k], genre = genre, colonnes = list(col)))
  }

  # --- l'ordre de l'export, puis les noms (courts, uniques, « Observation » pris par les noms des lignes)
  elements <- elements[order(vapply(elements, function(e) e$position, 0))]
  colonnes <- unlist(lapply(elements, function(e) e$colonnes), recursive = FALSE)
  courts <- kobo_noms_courts(lapply(colonnes, function(c) c$segments))
  pris_noms <- "Observation"
  for (i in seq_along(colonnes)) {
    colonnes[[i]]$nom <- nom_libre(courts[i], pris_noms)
    pris_noms <- c(pris_noms, colonnes[[i]]$nom)
  }
  # ce que le résumé dit de chaque élément, dans l'ordre de l'export, avec les noms donnés
  j <- 0
  coordonnees <- NULL
  for (e in elements) {
    cols <- colonnes[j + seq_along(e$colonnes)]
    faits <- vapply(cols, function(c) c$nom, "")
    j <- j + length(e$colonnes)
    if (e$genre %in% c("question", "metadonnee", "choix_unique", "date") && !identical(faits, e$sources)) {
      resume$renommees[[length(resume$renommees) + 1]] <- c(de = e$sources, vers = faits)
    }
    if (e$genre == "point") {
      resume$points[[length(resume$points) + 1]] <- list(nom = e$nom, colonnes = faits, hors = e$hors)
      if (is.null(coordonnees)) coordonnees <- list(x = faits[1], y = faits[2])
    }
    if (e$genre == "multiple") resume$multiples[[length(resume$multiples) + 1]] <- list(nom = e$nom, origine = e$origine, colonnes = faits)
    if (e$genre == "choix_unique") resume$qualitatives[[length(resume$qualitatives) + 1]] <- list(nom = faits, categories = levels(droplevels(cols[[1]]$valeurs)))
  }
  resume$hors_formulaire <- resume$hors_formulaire[order(match(resume$hors_formulaire, noms))]
  list(colonnes = stats::setNames(colonnes, vapply(colonnes, function(c) c$nom, "")), coordonnees = coordonnees, resume = resume, lignes = nrow(df))
}

# ---------------------------------------------------------------- la table d'analyse

# Les réglages d'un import, vérifiés : la table, les rôles des métadonnées gardées, les colonnes
# jointes de chaque table parente (NULL : toutes).
kobo_reglages <- function(export, table, garder, parents) {
  table <- as.character(table %||% export$principale)[1]
  if (!table %in% names(export$tables)) {
    stop(sprintf(traduire("Table inconnue dans l'export : %s (il a : %s).", "Unknown table in the export: %s (it has: %s)."),
                 kobo_guillemets(table), paste(kobo_guillemets(names(export$tables)), collapse = ", ")), call. = FALSE)
  }
  garder <- unique(as.character(unlist(garder)))
  inconnu <- setdiff(garder, ROLES_COLLECTE)
  if (length(inconnu)) {
    stop(sprintf(traduire("Métadonnée inconnue : %s (on peut garder : %s).", "Unknown metadata: %s (one can keep: %s)."),
                 inconnu[1], paste(ROLES_COLLECTE, collapse = ", ")), call. = FALSE)
  }
  if (!is.null(parents)) parents <- lapply(parents, function(x) as.character(unlist(x)))
  list(table = table, garder = garder, parents = parents)
}

# L'import d'une table d'un export (importer_collecte et apercu_collecte) : la table d'analyse
# (un data.frame, une colonne « Observation » d'abord : les numéros des lignes), ses colonnes à
# garder en texte, à lire en nombres et à garder en variables qualitatives, ses coordonnées, et
# ce qui a été fait (le résumé).
kobo_import <- function(chemin, table = NULL, garder = character(0), parents = NULL, formulaire = NULL, langue = NULL) {
  export <- lire_collecte(chemin)
  r <- kobo_reglages(export, table, garder, parents)
  form <- if (length(formulaire) && !is.na(formulaire[1]) && nzchar(formulaire[1])) lire_formulaire(as.character(formulaire[1]))
  langue <- kobo_langue(form, langue)
  cles <- form$cles
  faites <- list()
  preparer <- function(nom) {
    if (is.null(faites[[nom]])) faites[[nom]] <<- kobo_preparer(export, nom, form, langue, r$garder, cles)
    faites[[nom]]
  }
  prep <- preparer(r$table)
  if (!is.null(form) && !isTRUE(prep$resume$du_formulaire > 0)) {
    stop(sprintf(traduire("Le formulaire %s ne correspond pas à cet export : aucune de ses questions n'est une colonne de la table %s.",
                          "The form %s doesn't match this export: none of its questions is a column of the table %s."), form$fichier, kobo_guillemets(r$table)), call. = FALSE)
  }
  colonnes <- prep$colonnes
  n <- prep$lignes
  coordonnees <- prep$coordonnees
  # les colonnes des tables parentes, jointes à chaque ligne par sa ligne parente
  ancetres <- kobo_ancetres(export, r$table)
  joints <- list()
  infos <- list()
  sans_parent <- 0
  envois <- NA
  if (length(ancetres)) {
    inconnues <- setdiff(names(r$parents), ancetres)
    if (length(inconnues)) {
      stop(sprintf(traduire("%s n'est pas une table parente de %s (ses tables parentes : %s).", "%s isn't a parent table of %s (its parent tables: %s)."),
                   kobo_guillemets(inconnues[1]), kobo_guillemets(r$table), paste(kobo_guillemets(ancetres), collapse = ", ")), call. = FALSE)
    }
    pos <- seq_len(n)
    enfant <- export$tables[[r$table]]
    pris <- c("Observation", names(colonnes))
    for (a in ancetres) {
      parent <- export$tables[[a]]
      pos <- match(enfant$cle_parent[pos], parent$cle)
      if (a == ancetres[1]) sans_parent <- sum(is.na(pos))
      pa <- preparer(a)
      choisies <- if (is.null(r$parents) || !a %in% names(r$parents)) names(pa$colonnes) else r$parents[[a]]
      absentes <- setdiff(choisies, names(pa$colonnes))
      if (length(absentes)) {
        stop(sprintf(traduire("La table %s n'a pas de colonne %s.", "The table %s has no column %s."), kobo_guillemets(a), kobo_guillemets(absentes[1])), call. = FALSE)
      }
      faits <- character(0)
      for (cn in choisies) {
        jointe <- pa$colonnes[[cn]]
        jointe$valeurs <- jointe$valeurs[pos]
        jointe$nom <- nom_libre(cn, pris)
        pris <- c(pris, jointe$nom)
        faits <- c(faits, jointe$nom)
        joints[[jointe$nom]] <- jointe
      }
      if (is.null(coordonnees) && !is.null(pa$coordonnees) && all(unlist(pa$coordonnees) %in% choisies)) {
        coordonnees <- list(x = faits[match(pa$coordonnees$x, choisies)], y = faits[match(pa$coordonnees$y, choisies)], table = a)
      }
      infos[[length(infos) + 1]] <- list(table = a, colonnes = faits, disponibles = names(pa$colonnes))
      enfant <- parent
    }
    envois <- length(unique(stats::na.omit(pos)))
  }
  toutes <- c(colonnes, joints)
  # (le data.frame fait directement de ses colonnes : as.data.frame est lent pour des centaines)
  valeurs <- c(list(as.character(seq_len(n))), lapply(toutes, function(c) c$valeurs))
  df <- structure(stats::setNames(valeurs, c("Observation", names(toutes))), class = "data.frame", row.names = .set_row_names(n))
  drapeau <- function(cle) names(toutes)[vapply(toutes, function(c) isTRUE(c[[cle]]), TRUE)]
  resume <- prep$resume
  resume$table <- r$table
  resume$lignes <- n
  resume$envois <- envois
  resume$sans_parent <- sans_parent
  resume$joints <- infos
  if (!is.null(coordonnees)) resume$coordonnees <- list(x = coordonnees$x, y = coordonnees$y, table = coordonnees$table %||% r$table)
  resume$colonnes <- length(toutes)
  list(export = export, reglages = r, form = form, langue = langue, donnees = df, virgule = export$tables[[r$table]]$virgule,
       textes = c("Observation", drapeau("texte")), nombres = drapeau("nombres"),
       qualitatives = names(toutes)[vapply(toutes, function(c) is.factor(c$valeurs), TRUE)],
       coordonnees = coordonnees, resume = resume)
}

# Les phrases du résumé d'un import : ce qui est fait, une phrase par transformation.
kobo_phrases <- function(imp) {
  r <- imp$resume
  export <- imp$export
  outil <- if (export$outil == "kobo") "KoboToolbox" else "ODK Central"
  format <- switch(export$format, zip = traduire("archive .zip", ".zip archive"), xlsx = , xlsm = , xls = traduire("classeur Excel", "Excel workbook"), "CSV")
  n_tables <- length(export$tables)
  principale <- identical(r$table, export$principale)
  phrases <- sprintf(traduire("Export %s (%s%s) : %s, %d ligne(s)%s.", "%s export (%s%s): %s, %d row(s)%s."), outil, format,
                     if (n_tables > 1) sprintf(traduire(", %d tables", ", %d tables"), n_tables) else "",
                     if (principale) sprintf(traduire("la table des envois %s", "the submissions table %s"), kobo_guillemets(r$table))
                     else sprintf(traduire("le groupe répété %s", "the repeat group %s"), kobo_guillemets(r$table)),
                     r$lignes, if (!principale && !is.na(r$envois)) sprintf(traduire(", de %d envoi(s)", ", from %d submission(s)"), r$envois) else "")
  for (j in r$joints) {
    phrases <- c(phrases, if (length(j$colonnes)) {
      sprintf(traduire("Jointes à chaque ligne, de la table %s (telles que son import les fait) : %s.", "Joined to each row, from the table %s (as its import makes them): %s."),
              kobo_guillemets(j$table), kobo_liste(j$colonnes))
    } else sprintf(traduire("Aucune colonne de la table %s jointe.", "No column of the table %s joined."), kobo_guillemets(j$table)))
  }
  if (isTRUE(r$sans_parent > 0)) {
    phrases <- c(phrases, sprintf(traduire("%d ligne(s) sans leur ligne parente : leurs colonnes jointes sont vides.", "%d row(s) without their parent row: their joined columns are empty."), r$sans_parent))
  }
  if (length(r$retirees)) {
    phrases <- c(phrases, sprintf(traduire("Métadonnées retirées (%d) : %s.", "Metadata dropped (%d): %s."), length(r$retirees), kobo_liste(r$retirees, 12)))
  }
  if (length(r$gardees)) phrases <- c(phrases, sprintf(traduire("Métadonnées gardées : %s.", "Metadata kept: %s."), kobo_liste(r$gardees, 12)))
  if (length(r$renommees)) {
    fleches <- vapply(r$renommees, function(x) sprintf("%s → %s", x[["de"]], x[["vers"]]), "")
    phrases <- c(phrases, sprintf(traduire("Noms changés (%d) : %s.", "Names changed (%d): %s."), length(fleches), kobo_liste(fleches, 6)))
  }
  if (length(r$qualitatives)) {
    textes <- vapply(r$qualitatives, function(x) sprintf("%s (%s)", x$nom, kobo_liste(x$categories, 6)), "")
    phrases <- c(phrases, sprintf(traduire("Choix unique en variable qualitative, catégories dans l'ordre du formulaire%s : %s.", "Single choice as a qualitative variable, categories in the form's order%s: %s."),
                                  if (!is.null(imp$langue) && nzchar(imp$langue)) sprintf(traduire(", libellés en %s", ", labels in %s"), imp$langue) else "",
                                  paste(textes, collapse = traduire(" ; ", "; "))))
  }
  for (x in r$multiples) {
    phrases <- c(phrases, sprintf(traduire("Choix multiples %s en %d colonnes 0/1 (%s ; 1 : choisi, vide : sans réponse) : %s.", "Multiple choice %s as %d 0/1 columns (%s; 1: chosen, empty: no answer): %s."),
                                  kobo_guillemets(x$nom), length(x$colonnes),
                                  if (x$origine == "export") traduire("celles de l'export", "the export's own") else traduire("d'après les réponses", "from the answers"),
                                  kobo_liste(x$colonnes, 6)))
  }
  if (length(r$texte_multiples)) {
    phrases <- c(phrases, sprintf(traduire("Choix multiples gardés en texte (leurs réponses ne sont pas des codes du formulaire) : %s.", "Multiple choices kept as text (their answers aren't codes of the form): %s."), kobo_liste(r$texte_multiples)))
  }
  for (x in r$points) {
    phrases <- c(phrases, sprintf(traduire("Point GPS %s en %s (WGS 84)%s.", "GPS point %s as %s (WGS 84)%s."), kobo_guillemets(x$nom), kobo_liste(x$colonnes),
                                  if (isTRUE(x$hors > 0)) sprintf(traduire(" ; %d point(s) hors des bornes lus comme manquants", "; %d point(s) out of bounds read as missing"), x$hors) else ""))
  }
  co <- r$coordonnees
  if (!is.null(co)) {
    phrases <- c(phrases, sprintf(traduire("Coordonnées du projet : %s et %s, longitude et latitude en WGS 84%s.", "The project's coordinates: %s and %s, longitude and latitude in WGS 84%s."),
                                  co$x, co$y, if (!identical(co$table, r$table)) sprintf(traduire(", de la table %s", ", from the table %s"), kobo_guillemets(co$table)) else ""))
  }
  if (length(r$dates)) {
    phrases <- c(phrases, sprintf(traduire("Dates et heures lues comme le temps (2024-05-03 08:12 : l'heure du téléphone, sans le fuseau) : %s.", "Dates and times read as time (2024-05-03 08:12: the phone's time, without the time zone): %s."), kobo_liste(r$dates)))
  }
  if (length(r$hors_formulaire)) {
    phrases <- c(phrases, sprintf(traduire("Colonnes absentes du formulaire, lues sans lui (%d) : %s.", "Columns not in the form, read without it (%d): %s."), length(r$hors_formulaire), kobo_liste(r$hors_formulaire)))
  }
  if (is.null(imp$form)) {
    phrases <- c(phrases, traduire("Sans formulaire : les réponses restent telles que l'export les écrit (codes ou libellés), les catégories dans l'ordre alphabétique, et un choix multiple sans ses colonnes 0/1 reste du texte. Le formulaire XLSForm donne les libellés, l'ordre des catégories et les colonnes 0/1.",
                                   "Without the form: answers stay as the export writes them (codes or labels), categories in alphabetical order, and a multiple choice without its 0/1 columns stays text. The XLSForm form gives the labels, the order of the categories and the 0/1 columns."))
  }
  phrases
}

# L'aperçu d'un import (le dialogue de l'interface) : l'outil, les tables (leurs lignes, leur table
# parente), la table choisie, les métadonnées qu'on peut garder (par rôle, avec leurs colonnes, dans
# la table et ses tables parentes), les colonnes de chaque table parente qu'on peut joindre, le
# formulaire (ses langues, celle choisie) et ce qui sera fait (ses phrases).
apercu_collecte <- function(chemin, table = NULL, garder = character(0), parents = NULL, formulaire = NULL, langue = NULL) {
  imp <- kobo_import(chemin, table, garder, parents, formulaire, langue)
  export <- imp$export
  r <- imp$resume
  # les rôles présents : ceux de la table et de ses tables parentes (gardées, elles s'y joignent)
  roles <- list()
  for (nom in c(r$table, kobo_ancetres(export, r$table))) {
    for (cn in names(export$tables[[nom]]$donnees)) {
      role <- role_metadonnee(cn, export$outil)
      if (!is.na(role) && role %in% ROLES_COLLECTE) roles[[role]] <- unique(c(roles[[role]], cn))
    }
  }
  for (role in names(r$roles)) roles[[role]] <- unique(c(roles[[role]], r$roles[[role]]))
  roles <- roles[intersect(ROLES_COLLECTE, names(roles))]
  list(outil = export$outil, nom_outil = if (export$outil == "kobo") "KoboToolbox" else "ODK Central", format = export$format,
       fichier = basename(chemin),
       tables = lapply(names(export$tables), function(n) list(nom = n, lignes = nrow(export$tables[[n]]$donnees), parent = export$tables[[n]]$parent,
                                                              principale = identical(n, export$principale), niveau = length(kobo_ancetres(export, n)))),
       table = r$table, lignes = r$lignes, colonnes = r$colonnes, principale = identical(r$table, export$principale),
       metadonnees = lapply(names(roles), function(n) list(role = n, colonnes = I(roles[[n]]), garde = n %in% imp$reglages$garder,
                                                           nom = traduire(NOMS_ROLES_COLLECTE[[n]][1], NOMS_ROLES_COLLECTE[[n]][2]))),
       garder = I(imp$reglages$garder), retirees = I(r$retirees),
       parents = lapply(r$joints, function(j) list(table = j$table, colonnes = I(j$disponibles), choisies = I(j$colonnes))),
       formulaire = if (!is.null(imp$form)) list(fichier = imp$form$fichier, titre = imp$form$titre %||% "", langues = I(imp$form$langues), langue = imp$langue),
       coordonnees = !is.null(r$coordonnees), phrases = I(kobo_phrases(imp)))
}

# Une table d'un export en projet, comme un tableau ouvert (nouveau.R : projet_depuis_tableau) :
# `table` (par défaut, celle des envois), `garder` (les rôles des métadonnées gardées,
# ROLES_COLLECTE), `parents` (les colonnes jointes de chaque table parente, par nom de table ;
# NULL : toutes), `formulaire` (le chemin du formulaire XLSForm) et `langue` (celle de ses
# libellés). Les coordonnées du projet sont le premier point GPS, en WGS 84. p$collecte (enregistré
# avec le projet) dit d'où viennent les données, pour le texte des Méthodes ; p$import_collecte (de
# la séance) les réglages, pour refaire l'import quand le moteur redémarre ; attr(, "collecte") :
# ce qui a été fait.
importer_collecte <- function(chemin, table = NULL, garder = character(0), parents = NULL, formulaire = NULL, langue = NULL) {
  imp <- kobo_import(chemin, table, garder, parents, formulaire, langue)
  df <- convertir_colonnes(imp$donnees, imp$virgule, quantitatives = imp$nombres, toutes = TRUE, textes = imp$textes)
  r <- imp$resume
  principale <- identical(r$table, imp$export$principale)
  nom <- if (principale) basename(chemin) else sprintf("%s (%s).%s", tools::file_path_sans_ext(basename(chemin)), r$table, tools::file_ext(chemin))
  p <- projet_depuis_tableau(df, nom, secondaires = TRUE, qualitatives = imp$qualitatives)
  notes <- unname(attr(df, "note"))
  if (!is.null(imp$coordonnees)) {
    co <- imp$coordonnees
    p <- tryCatch(regler_coordonnees(p, numeros(p, co$x), numeros(p, co$y), 4326),
                  error = function(e) { notes <<- c(notes, conditionMessage(e)); p })
  }
  p$note <- notes
  p$collecte <- list(outil = if (imp$export$outil == "kobo") "KoboToolbox" else "ODK", table = r$table, repetition = !principale,
                     formulaire = if (!is.null(imp$form)) imp$form$titre %||% tools::file_path_sans_ext(imp$form$fichier))
  p$import_collecte <- list(table = r$table, garder = I(imp$reglages$garder), parents = imp$reglages$parents,
                            formulaire = if (!is.null(imp$form)) as.character(formulaire)[1], langue = imp$langue)
  attr(p, "collecte") <- list(phrases = kobo_phrases(imp), coordonnees = !is.null(p$coordonnees), lignes = r$lignes, colonnes = r$colonnes)
  p
}

# Une table d'une archive d'ODK Central telle qu'elle est écrite (Ouvrir tel quel ; un classeur ou
# un CSV s'ouvrent comme tout tableau) : en projet comme un tableau ouvert (formats.R :
# finir_tableau), sans rien de l'import. `table` : la table des envois par défaut.
projet_brut_collecte <- function(chemin, table = NULL) {
  export <- lire_collecte(chemin)
  tab <- export$tables[[kobo_reglages(export, table, NULL, NULL)$table]]
  df <- tab$donnees
  attr(df, "virgule") <- tab$virgule
  df <- finir_tableau(df)
  p <- projet_depuis_tableau(df, basename(chemin))
  p$note <- unname(attr(df, "note"))
  p
}

# L'appel R d'un import, pour le journal de la console (console.R) ; `o` : ses réglages.
appel_collecte <- function(chemin, o) {
  code <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  arguments <- c(code(as.character(chemin)),
                 if (length(o$table)) sprintf("table = %s", code(as.character(o$table)[1])),
                 if (length(unlist(o$garder))) sprintf("garder = %s", code(as.character(unlist(o$garder)))),
                 if (!is.null(o$parents)) sprintf("parents = %s", code(lapply(o$parents, function(x) as.character(unlist(x))))),
                 if (length(o$formulaire) && nzchar(o$formulaire[1])) sprintf("formulaire = %s", code(as.character(o$formulaire)[1])),
                 if (length(o$langue)) sprintf("langue = %s", code(as.character(o$langue)[1])))
  sprintf("importer_collecte(%s)", paste(arguments, collapse = ", "))
}

# La phrase du texte des Méthodes (methodes.R) : d'où viennent les données d'un projet importé de
# KoboToolbox ou d'ODK ; NULL pour un autre projet.
phrase_methodes_collecte <- function(projet, fr) {
  co <- projet$collecte
  if (!is.list(co) || !length(co$outil)) return(NULL)
  outil <- as.character(co$outil)[1]
  formulaire <- if (length(co$formulaire) && nzchar(as.character(co$formulaire)[1])) as.character(co$formulaire)[1]
  if (fr) sprintf("Les données ont été recueillies avec %s%s.", outil, if (!is.null(formulaire)) sprintf(" (formulaire « %s »)", formulaire) else "")
  else sprintf("The data were collected with %s%s.", outil, if (!is.null(formulaire)) sprintf(" (form “%s”)", formulaire) else "")
}
