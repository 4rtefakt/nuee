# Les greffons : une fonction R et son manifeste deviennent un type de modèle, PLUGIN, avec son
# dialogue (fait d'après le manifeste par l'interface : interface/vues_greffons.js), ses résultats
# (ceux d'un modèle de code R, code.R), son export vers R, son texte des méthodes et sa citation.
# Pour les auteurs de greffons : le guide (« Writing a plugin ») et docs/developpement.md.
#
# Un greffon est un dossier : son manifeste, plugin.json, et ses fichiers R. Il se partage en un
# fichier .nueeplugin, une archive zip de ce dossier (plugin.json à sa racine, ou dans un seul
# dossier de l'archive). Le manifeste est un objet JSON (lire_manifeste : tout autre champ, toute
# autre forme est refusée, en disant où et pourquoi) :
#   id            le nom de son dossier : des minuscules, des chiffres, « - », « _ », « . » ;
#   version       X.Y.Z ; nuee : la version de Nuée qu'il demande au moins (X.Y.Z) ;
#   name, description   {en, fr} : l'anglais requis, le français facultatif (l'anglais à sa place) ;
#   authors       [{name, orcid}], au moins un ; l'ORCID facultatif, vérifié (sa clé de contrôle) ;
#   license       sa licence ; citation : {text, bibtex}, ou une liste de ceux-ci (facultatif) ;
#   packages      les paquets R qu'il demande, au-delà de R lui-même (facultatif) ;
#   files         ses fichiers R, lus dans cet ordre ; function : le nom de sa fonction ;
#   inputs        les variables du projet que son dialogue fait choisir : [{id, type : "variable"
#                 (une seule ; optional : true pour pouvoir la laisser vide) ou "variables" (plusieurs :
#                 min, 1 par défaut, et max), accepts : "quantitative", "qualitative" ou "any", label,
#                 help}] ;
#   options       ses réglages : [{id, type : "number" (default, min, max, integer), "choice" (values :
#                 [{value, label}], default), "checkbox" (default) ou "text" (default), label, help}].
# Sa fonction est appelée f(donnees, entrees, options) : donnees, les observations du modèle comme
# pour un modèle de code R (code.R : donnees_code) ; entrees, le nom des variables choisies (une
# chaîne, ou NULL, pour une entrée « variable » ; un vecteur, peut-être vide, pour « variables ») ;
# options, la valeur de chaque option (un entier pour un nombre entier, TRUE ou FALSE pour une case).
# Elle rend ce que rend le code d'un modèle de code R (le modèle, ou list(modele, valeurs,
# tableaux)), dont Nuée tire les mêmes résultats (code.R : modele_valeur_code). Les méthodes S3 que
# ses fichiers définissent pour les génériques que Nuée appelle (GREFFON_GENERIQUES : summary.maclasse,
# logLik.maclasse…) sont enregistrées, pour que Nuée les trouve.
#
# Où sont les greffons : ceux de l'utilisateur dans NUEE_GREFFONS (main.js : le dossier greffons de
# ses réglages), où Installer les écrit ; ceux de l'organisation dans NUEE_GREFFONS_ORGANISATION
# (main.js : la politique PluginsFolder, docs/deploiement.md), lus seulement, offerts à tous sans rien
# demander. Un id que l'organisation donne aussi est le sien. Un modèle fait d'un greffon le garde en
# entier dans sa définition (d$options$greffon : son id, sa version, son empreinte et ses fichiers,
# manifeste compris) : le projet l'emporte, et se refait ailleurs avec cette version-là, sauf si
# l'utilisateur passe ses modèles à la version installée (demande greffon_mettre_a_jour).
#
# La confiance est celle du code R (code.R) : un greffon est du code R qui s'exécute avec les droits
# de l'utilisateur. Son empreinte (empreinte_greffon : le SHA-256 de ses fichiers dans un ordre fixe)
# doit être parmi celles que l'utilisateur a approuvées sur cet ordinateur (installer un greffon
# l'approuve ; Exécuter, dans la barre au-dessus d'un projet qui en apporte un autre), ou être celle
# d'un greffon de l'organisation ; avec la politique DisableCustomCode, seuls ceux-ci s'exécutent. Le
# moteur le vérifie à chaque chemin qui ajuste un modèle, comme pour le code R ; ouvrir un projet ou
# lire un .nueeplugin n'exécute jamais rien (le manifeste est lu, le code seulement analysé).
#
# C'est une analyse de analyses.R : definir_plugin, ajuster_plugin, resultat_plugin, tables_plugin,
# predire_plugin, texte_methodes_plugin, ecrire_script_plugin, entrees_plugin, verifier_plugin ; et
# les demandes greffons, greffon_lire, greffon_installer, greffon_retirer, greffon_projet et
# greffon_mettre_a_jour (demande_analyse_<nom>).

GREFFON_CHAMPS <- c("id", "version", "nuee", "name", "description", "authors", "license", "citation", "packages", "files", "function", "inputs", "options")
GREFFON_REQUIS <- c("id", "version", "nuee", "name", "description", "authors", "license", "files", "function")
GREFFON_TAILLE_MAX <- 5 * 1024^2   # (tous ses fichiers ensemble ; et ce qu'une archive contient)
GREFFON_FICHIER_MAX <- 1024^2      # (un fichier)
GREFFON_GENERIQUES <- c("print", "summary", "coef", "vcov", "logLik", "nobs", "fitted", "residuals", "predict", "formula", "family", "na.action", "df.residual")
VERSION_MOTIF <- "^(0|[1-9][0-9]{0,5})\\.(0|[1-9][0-9]{0,5})\\.(0|[1-9][0-9]{0,5})$"
GREFFON_ID_MOTIF <- "^[a-z][a-z0-9]*([._-][a-z0-9]+)*$"
GREFFON_FICHIER_MOTIF <- "^[A-Za-z0-9_][A-Za-z0-9_.-]*(/[A-Za-z0-9_][A-Za-z0-9_.-]*)*\\.[Rr]$"
PAQUET_MOTIF <- "^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$"
# (des champs qu'on écrit pour d'autres : ce qu'un refus propose à la place)
GREFFON_INDICES <- c(licence = "license", author = "authors", package = "packages", file = "files", input = "inputs", option = "options",
                     fonction = "function", nom = "name", entrees = "inputs", paquets = "packages", fichiers = "files", auteurs = "authors")

# Ce que le moteur garde le temps de la séance : les greffons lus d'après leurs fichiers (par leur texte
# canonique, la langue des messages comprise), et ceux des deux dossiers (par l'état de leurs fichiers).
if (!exists("GREFFONS_SEANCE", envir = globalenv(), inherits = FALSE)) {
  GREFFONS_SEANCE <- new.env(parent = emptyenv())
  GREFFONS_SEANCE$lus <- list()
  GREFFONS_SEANCE$cles <- character(0)
  GREFFONS_SEANCE$dossiers <- NULL
  GREFFONS_SEANCE$signature <- NULL
}

# ---------------------------------------------------------------- le manifeste

est_objet_json <- function(x) is.list(x) && !is.null(names(x)) && all(nzchar(names(x)))
est_liste_json <- function(x) is.list(x) && is.null(names(x))
est_chaine <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
est_nombre <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
est_booleen <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
est_entier_json <- function(x) est_nombre(x) && x == round(x) && abs(x) <= 1e6
nom_r_valide <- function(x) est_chaine(x) && nzchar(x) && nchar(x) <= 64 && identical(make.names(x), x)

# Une version X.Y.Z comparée à une autre : -1, 0 ou 1.
comparer_versions <- function(a, b) {
  x <- as.integer(strsplit(a, ".", fixed = TRUE)[[1]])
  y <- as.integer(strsplit(b, ".", fixed = TRUE)[[1]])
  for (i in 1:3) if (x[i] != y[i]) return(sign(x[i] - y[i]))
  0
}

# Un ORCID (0000-0002-1825-0097), sa clé de contrôle comprise (ISO 7064, 11-2).
orcid_valide <- function(x) {
  if (!grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", x)) return(FALSE)
  ch <- strsplit(gsub("-", "", x, fixed = TRUE), "")[[1]]
  total <- 0
  for (c in ch[1:15]) total <- (total + as.integer(c)) * 2
  r <- (12 - total %% 11) %% 11
  identical(ch[16], if (r == 10) "X" else as.character(r))
}

# Le manifeste d'un greffon, lu de son texte JSON et vérifié champ par champ : list(manifeste (NULL
# quand il a une erreur), erreurs, avertissements), chaque message disant où (« inputs[2].accepts »)
# et quoi, dans la langue de la demande. `presents` : les fichiers qui sont là (ceux que `files` nomme
# doivent y être). Le manifeste rendu a des noms à lui : id, version, nuee, nom, description
# (list(en, fr)), auteurs (list(nom, orcid)), licence, citations (list(texte, bibtex)), paquets,
# fichiers, fonction, entrees et options (decrire ci-dessous).
lire_manifeste <- function(texte, presents = NULL) {
  erreurs <- character(0)
  avertissements <- character(0)
  sans_francais <- character(0)
  dit <- function(ou, fr, en, ...) paste0(ou, traduire(" : ", ": "), sprintf(traduire(fr, en), ...))
  erreur <- function(...) { erreurs <<- c(erreurs, dit(...)); NULL }
  fin <- function(m = NULL) {
    if (length(sans_francais)) avertissements <- c(avertissements, sprintf(traduire("Pas de texte en français (l'anglais sert aussi en français) : %s.",
                                                                                    "No French text (the English one is used in French too): %s."), paste(sans_francais, collapse = ", ")))
    list(manifeste = if (!length(erreurs)) m, erreurs = erreurs, avertissements = avertissements)
  }
  j <- tryCatch(jsonlite::fromJSON(texte, simplifyVector = FALSE), error = function(e) e)
  if (inherits(j, "error")) {
    erreur("plugin.json", "ce n'est pas du JSON valide (%s)", "it isn't valid JSON (%s)", gsub("\\s+", " ", conditionMessage(j)))
    return(fin())
  }
  if (!est_objet_json(j)) {
    erreur("plugin.json", "il faut un objet JSON, { … }", "it must be a JSON object, { … }")
    return(fin())
  }
  cles <- function(x, ou, permises) {
    for (k in setdiff(names(x), permises)) {
      indice <- if (k %in% names(GREFFON_INDICES)) sprintf(traduire(" (vouliez-vous dire « %s » ?)", " (did you mean \"%s\"?)"), GREFFON_INDICES[[k]]) else ""
      erreur(ou, "champ inconnu « %s »%s ; les champs permis : %s", "unknown field \"%s\"%s; the fields allowed: %s", k, indice, paste(permises, collapse = ", "))
    }
    for (k in unique(names(x)[duplicated(names(x))])) erreur(ou, "le champ « %s » revient plusieurs fois", "the field \"%s\" comes more than once", k)
  }
  # un texte dans les deux langues, {"en": …, "fr": …}
  langues <- function(x, ou, max = 200, requis = TRUE) {
    if (is.null(x)) {
      if (requis) erreur(ou, "requis : {\"en\": …, \"fr\": …}", "required: {\"en\": …, \"fr\": …}")
      return(NULL)
    }
    if (!est_objet_json(x)) return(erreur(ou, "un objet {\"en\": …, \"fr\": …}, l'anglais requis", "an object {\"en\": …, \"fr\": …}, the English required"))
    cles(x, ou, c("en", "fr"))
    en <- x$en
    fr <- x$fr
    if (!est_chaine(en) || !nzchar(trimws(en))) return(erreur(paste0(ou, ".en"), "requis, un texte non vide", "required, a text that isn't empty"))
    if (nchar(en) > max) erreur(paste0(ou, ".en"), "%d caractères au plus", "%d characters at most", max)
    if (!is.null(fr) && (!est_chaine(fr) || !nzchar(trimws(fr)))) return(erreur(paste0(ou, ".fr"), "un texte non vide", "a text that isn't empty"))
    if (is.null(fr)) sans_francais <<- c(sans_francais, ou)
    else if (nchar(fr) > max) erreur(paste0(ou, ".fr"), "%d caractères au plus", "%d characters at most", max)
    list(en = trimws(en), fr = trimws(fr %||% en))
  }
  texte_court <- function(x, ou, max, requis = TRUE) {
    if (is.null(x)) { if (requis) erreur(ou, "requis", "required"); return(NULL) }
    if (!est_chaine(x) || !nzchar(trimws(x))) return(erreur(ou, "un texte non vide", "a text that isn't empty"))
    if (nchar(x) > max) return(erreur(ou, "%d caractères au plus", "%d characters at most", max))
    trimws(x)
  }
  liste <- function(x, ou, min = 0, max = 50) {
    if (is.null(x)) { if (min > 0) erreur(ou, "requis : une liste [ … ]", "required: a list [ … ]"); return(list()) }
    if (!est_liste_json(x)) { erreur(ou, "une liste [ … ]", "a list [ … ]"); return(list()) }
    if (length(x) < min) erreur(ou, "au moins %d élément(s)", "at least %d item(s)", min)
    if (length(x) > max) { erreur(ou, "%d éléments au plus", "%d items at most", max); return(list()) }
    x
  }
  uniques <- function(ids, ou) for (k in unique(ids[duplicated(ids)])) erreur(ou, "l'id « %s » revient plusieurs fois", "the id \"%s\" comes more than once", k)

  cles(j, "plugin.json", GREFFON_CHAMPS)
  for (k in setdiff(GREFFON_REQUIS, names(j))) erreur("plugin.json", "il manque le champ « %s »", "the field \"%s\" is missing", k)
  m <- list()
  if (!is.null(j$id)) {
    if (!est_chaine(j$id) || nchar(j$id) > 64 || !grepl(GREFFON_ID_MOTIF, j$id)) erreur("id", "des minuscules, des chiffres et « - », « _ » ou « . » entre eux, 64 au plus, commençant par une lettre (occupancy, mann-kendall)",
                                                                                       "lowercase letters, digits and \"-\", \"_\" or \".\" between them, 64 at most, starting with a letter (occupancy, mann-kendall)")
    else m$id <- j$id
  }
  for (k in c("version", "nuee")) if (!is.null(j[[k]])) {
    if (!est_chaine(j[[k]]) || !grepl(VERSION_MOTIF, j[[k]])) erreur(k, "une version X.Y.Z (1.0.0)", "a version X.Y.Z (1.0.0)")
    else m[[k]] <- j[[k]]
  }
  m$nom <- langues(j$name, "name", 100, requis = "name" %in% names(j))
  m$description <- langues(j$description, "description", 2000, requis = "description" %in% names(j))
  if ("authors" %in% names(j)) {
    auteurs <- liste(j$authors, "authors", 1, 50)
    m$auteurs <- lapply(seq_along(auteurs), function(k) {
      a <- auteurs[[k]]
      ou <- sprintf("authors[%d]", k)
      if (!est_objet_json(a)) return(erreur(ou, "un objet {\"name\": …, \"orcid\": …}", "an object {\"name\": …, \"orcid\": …}"))
      cles(a, ou, c("name", "orcid"))
      orcid <- a$orcid
      if (!is.null(orcid)) {
        o <- if (est_chaine(orcid)) sub("^https?://orcid\\.org/", "", trimws(orcid)) else ""
        if (!orcid_valide(o)) erreur(paste0(ou, ".orcid"), "un ORCID comme 0000-0002-1825-0097, sa dernière clé juste", "an ORCID such as 0000-0002-1825-0097, its last check digit right")
        orcid <- o
      }
      list(nom = texte_court(a$name, paste0(ou, ".name"), 200), orcid = orcid)
    })
  }
  if ("license" %in% names(j)) m$licence <- texte_court(j$license, "license", 100)
  if (!is.null(j$citation)) {
    cs <- if (est_objet_json(j$citation)) list(j$citation) else liste(j$citation, "citation", 1, 20)
    m$citations <- lapply(seq_along(cs), function(k) {
      x <- cs[[k]]
      ou <- if (est_objet_json(j$citation)) "citation" else sprintf("citation[%d]", k)
      if (!est_objet_json(x)) return(erreur(ou, "un objet {\"text\": …, \"bibtex\": …}", "an object {\"text\": …, \"bibtex\": …}"))
      cles(x, ou, c("text", "bibtex"))
      b <- if (!is.null(x$bibtex)) texte_court(x$bibtex, paste0(ou, ".bibtex"), 10000)
      if (!is.null(b) && (!grepl("^@[A-Za-z]+\\s*\\{", b) || lengths(regmatches(b, gregexpr("\\{", b))) != lengths(regmatches(b, gregexpr("\\}", b))))) {
        erreur(paste0(ou, ".bibtex"), "une entrée BibTeX, @article{cle, …}, ses accolades fermées", "a BibTeX entry, @article{key, …}, its braces closed")
      }
      list(texte = texte_court(x$text, paste0(ou, ".text"), 2000), bibtex = b)
    })
  } else m$citations <- list()
  paquets <- liste(j$packages, "packages", 0, 100)
  for (k in seq_along(paquets)) if (!est_chaine(paquets[[k]]) || !grepl(PAQUET_MOTIF, paquets[[k]])) erreur(sprintf("packages[%d]", k), "le nom d'un paquet R (mgcv, unmarked)", "the name of an R package (mgcv, unmarked)")
  m$paquets <- unique(as.character(unlist(Filter(est_chaine, paquets))))
  if ("files" %in% names(j)) {
    fichiers <- liste(j$files, "files", 1, 50)
    for (k in seq_along(fichiers)) {
      f <- fichiers[[k]]
      ou <- sprintf("files[%d]", k)
      if (!est_chaine(f) || !grepl(GREFFON_FICHIER_MOTIF, f) || nchar(f) > 200) erreur(ou, "le chemin d'un fichier .R du dossier du greffon, avec des / (R/occupancy.R)", "the path of a .R file of the plugin's folder, with / (R/occupancy.R)")
      else if (!is.null(presents) && !f %in% presents) erreur(ou, "le fichier %s n'est pas dans le greffon", "the file %s isn't in the plugin", f)
    }
    m$fichiers <- as.character(unlist(Filter(est_chaine, fichiers)))
    for (f in unique(m$fichiers[duplicated(m$fichiers)])) erreur("files", "%s revient plusieurs fois", "%s comes more than once", f)
  }
  if (!is.null(j[["function"]])) {
    if (!nom_r_valide(j[["function"]])) erreur("function", "le nom d'une fonction R (occupancy_fit)", "the name of an R function (occupancy_fit)")
    else m$fonction <- j[["function"]]
  }
  # les entrées : des variables du projet
  entrees <- liste(j$inputs, "inputs", 0, 30)
  m$entrees <- lapply(seq_along(entrees), function(k) {
    x <- entrees[[k]]
    ou <- sprintf("inputs[%d]", k)
    if (!est_objet_json(x)) return(erreur(ou, "un objet {\"id\": …, \"type\": …, \"label\": …}", "an object {\"id\": …, \"type\": …, \"label\": …}"))
    cles(x, ou, c("id", "type", "accepts", "optional", "min", "max", "label", "help"))
    if (!nom_r_valide(x$id)) erreur(paste0(ou, ".id"), "requis, un nom R (lettres, chiffres, « . » et « _ », commençant par une lettre)", "required, an R name (letters, digits, \".\" and \"_\", starting with a letter)")
    type <- x$type
    if (!est_chaine(type) || !type %in% c("variable", "variables")) erreur(paste0(ou, ".type"), "requis : « variable » (une) ou « variables » (plusieurs)", "required: \"variable\" (one) or \"variables\" (several)")
    accepte <- x$accepts %||% "any"
    if (!est_chaine(accepte) || !accepte %in% c("quantitative", "qualitative", "any")) erreur(paste0(ou, ".accepts"), "« quantitative », « qualitative » ou « any »", "\"quantitative\", \"qualitative\" or \"any\"")
    e <- list(id = x$id, type = type, accepte = accepte, min = 1, max = NA_real_, facultatif = FALSE)
    if (identical(type, "variable")) {
      for (k2 in intersect(c("min", "max"), names(x))) erreur(paste0(ou, ".", k2), "seulement pour « variables » (une seule variable : optional)", "for \"variables\" only (a single variable: optional)")
      if (!is.null(x$optional) && !est_booleen(x$optional)) erreur(paste0(ou, ".optional"), "true ou false", "true or false")
      e$facultatif <- isTRUE(x$optional)
      e$min <- if (e$facultatif) 0 else 1
      e$max <- 1
    } else if (identical(type, "variables")) {
      if (!is.null(x$optional)) erreur(paste0(ou, ".optional"), "seulement pour « variable » (plusieurs variables : min = 0)", "for \"variable\" only (several variables: min = 0)")
      if (!is.null(x$min) && (!est_entier_json(x$min) || x$min < 0)) erreur(paste0(ou, ".min"), "un entier, 0 ou plus", "a whole number, 0 or more")
      else if (!is.null(x$min)) e$min <- x$min
      if (!is.null(x$max) && (!est_entier_json(x$max) || x$max < max(1, e$min))) erreur(paste0(ou, ".max"), "un entier, au moins min et au moins 1", "a whole number, at least min and at least 1")
      else if (!is.null(x$max)) e$max <- x$max
      e$facultatif <- e$min == 0
    }
    e$libelle <- langues(x$label, paste0(ou, ".label"))
    e$aide <- langues(x$help, paste0(ou, ".help"), 2000, requis = FALSE)
    e
  })
  uniques(vapply(m$entrees, function(e) as.character(e$id %||% ""), ""), "inputs")
  # les options
  options <- liste(j$options, "options", 0, 50)
  m$options <- lapply(seq_along(options), function(k) {
    x <- options[[k]]
    ou <- sprintf("options[%d]", k)
    if (!est_objet_json(x)) return(erreur(ou, "un objet {\"id\": …, \"type\": …, \"label\": …}", "an object {\"id\": …, \"type\": …, \"label\": …}"))
    if (!nom_r_valide(x$id)) erreur(paste0(ou, ".id"), "requis, un nom R (lettres, chiffres, « . » et « _ », commençant par une lettre)", "required, an R name (letters, digits, \".\" and \"_\", starting with a letter)")
    type <- x$type
    if (!est_chaine(type) || !type %in% c("number", "choice", "checkbox", "text")) {
      cles(x, ou, c("id", "type", "label", "help", "default", "min", "max", "integer", "values"))
      return(erreur(paste0(ou, ".type"), "requis : « number », « choice », « checkbox » ou « text »", "required: \"number\", \"choice\", \"checkbox\" or \"text\""))
    }
    cles(x, ou, c("id", "type", "label", "help", "default", switch(type, number = c("min", "max", "integer"), choice = "values", NULL)))
    o <- list(id = x$id, type = type, libelle = langues(x$label, paste0(ou, ".label")), aide = langues(x$help, paste0(ou, ".help"), 2000, requis = FALSE))
    if (type == "number") {
      for (b in c("min", "max")) if (!is.null(x[[b]]) && !est_nombre(x[[b]])) erreur(paste0(ou, ".", b), "un nombre", "a number")
      if (!is.null(x$integer) && !est_booleen(x$integer)) erreur(paste0(ou, ".integer"), "true ou false", "true or false")
      o$entier <- isTRUE(x$integer)
      o$min <- if (est_nombre(x$min)) x$min else NA_real_
      o$max <- if (est_nombre(x$max)) x$max else NA_real_
      if (!is.na(o$min) && !is.na(o$max) && o$min > o$max) erreur(ou, "min est plus grand que max", "min is greater than max")
      if (!est_nombre(x$default)) erreur(paste0(ou, ".default"), "requis, un nombre", "required, a number")
      else {
        o$defaut <- x$default
        if ((!is.na(o$min) && x$default < o$min) || (!is.na(o$max) && x$default > o$max)) erreur(paste0(ou, ".default"), "hors de [min, max]", "outside [min, max]")
        if (o$entier && x$default != round(x$default)) erreur(paste0(ou, ".default"), "un entier (integer est true)", "a whole number (integer is true)")
      }
    } else if (type == "choice") {
      valeurs <- liste(x$values, paste0(ou, ".values"), 1, 50)
      o$valeurs <- lapply(seq_along(valeurs), function(i) {
        v <- valeurs[[i]]
        ouv <- sprintf("%s.values[%d]", ou, i)
        if (!est_objet_json(v)) return(erreur(ouv, "un objet {\"value\": …, \"label\": …}", "an object {\"value\": …, \"label\": …}"))
        cles(v, ouv, c("value", "label"))
        if (!est_chaine(v$value) || !nzchar(v$value) || nchar(v$value) > 100) erreur(paste0(ouv, ".value"), "requis, un texte non vide", "required, a text that isn't empty")
        list(valeur = if (est_chaine(v$value)) v$value, libelle = langues(v$label, paste0(ouv, ".label")))
      })
      ids <- vapply(o$valeurs, function(v) as.character(v$valeur %||% ""), "")
      for (v in unique(ids[duplicated(ids)])) erreur(paste0(ou, ".values"), "la valeur « %s » revient plusieurs fois", "the value \"%s\" comes more than once", v)
      if (!est_chaine(x$default) || !x$default %in% ids) erreur(paste0(ou, ".default"), "requis, l'une des valeurs (%s)", "required, one of the values (%s)", paste(ids, collapse = ", "))
      else o$defaut <- x$default
    } else if (type == "checkbox") {
      if (!is.null(x$default) && !est_booleen(x$default)) erreur(paste0(ou, ".default"), "true ou false", "true or false")
      o$defaut <- isTRUE(x$default)
    } else {
      if (!is.null(x$default) && (!est_chaine(x$default) || nchar(x$default) > 10000)) erreur(paste0(ou, ".default"), "un texte", "a text")
      o$defaut <- if (est_chaine(x$default)) x$default else ""
    }
    o
  })
  uniques(vapply(m$options, function(o) as.character(o$id %||% ""), ""), "options")
  fin(m)
}

# Les fichiers R d'un greffon analysés sans être exécutés : chacun se lit (une erreur de lecture, avec
# sa ligne), et la fonction du manifeste y est définie au premier niveau (nom <- function(…)) avec
# trois arguments, ou « … ». Rend les erreurs.
erreurs_code_greffon <- function(fichiers, m) {
  erreurs <- character(0)
  definie <- NULL
  for (f in fichiers) {
    if (identical(f$chemin, "plugin.json")) next
    exprs <- tryCatch(parse(text = f$texte, keep.source = FALSE), error = function(e) e)
    if (inherits(exprs, "error")) {
      ligne <- ligne_lecture(conditionMessage(exprs))
      erreurs <- c(erreurs, sprintf(traduire("%s%s : il ne se lit pas comme du R (%s)", "%s%s: it doesn't read as R (%s)"), f$chemin,
                                    if (!is.na(ligne)) sprintf(traduire(", ligne %d", ", line %d"), ligne) else "", gsub("\\s+", " ", sub("^<text>:[0-9]+:[0-9]+: ", "", conditionMessage(exprs)))))
      next
    }
    for (e in as.list(exprs)) {
      if (is.call(e) && length(e) == 3 && as.character(e[[1]]) %in% c("<-", "=") && identical(e[[2]], as.name(m$fonction)) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
        definie <- e[[3]][[2]]
      }
    }
  }
  if (!length(erreurs)) {
    if (is.null(definie)) {
      erreurs <- c(erreurs, sprintf(traduire("function : les fichiers ne définissent pas %s au premier niveau (%s <- function(donnees, entrees, options) …)",
                                             "function: the files don't define %s at their top level (%s <- function(donnees, entrees, options) …)"), m$fonction, m$fonction))
    } else if (length(definie) < 3 && !"..." %in% names(definie)) {
      erreurs <- c(erreurs, sprintf(traduire("function : %s prend %d argument(s) ; Nuée l'appelle avec trois, %s(donnees, entrees, options)",
                                             "function: %s takes %d argument(s); Nuée calls it with three, %s(donnees, entrees, options)"), m$fonction, length(definie), m$fonction))
    }
  }
  erreurs
}

# ---------------------------------------------------------------- l'empreinte et le greffon lu

# Le texte canonique d'un greffon : « nuee plugin 1 », puis chacun de ses fichiers rangé par son
# chemin (octet par octet), son chemin, la longueur de son texte en octets et son texte (en UTF-8, les
# fins de ligne en \n), chacun sur sa ligne.
canon_greffon <- function(fichiers) {
  chemins <- vapply(fichiers, function(f) as.character(f$chemin), "")
  textes <- vapply(fichiers, function(f) texte_code(f$texte), "")
  o <- order(chemins, method = "radix")
  paste0("nuee plugin 1\n", paste0(chemins[o], "\n", nchar(textes[o], type = "bytes"), "\n", textes[o], "\n", collapse = ""))
}

# Son empreinte : le SHA-256 de son texte canonique (code.R : empreinte_code, le même quel que soit le R).
empreinte_greffon <- function(fichiers) empreinte_code(canon_greffon(fichiers))

# Un greffon tel que le moteur le tient, d'après ses fichiers (list(chemin, texte), plugin.json compris) :
# id, version, manifeste (NULL s'il a une erreur), fichiers (le manifeste et ceux qu'il nomme), empreinte,
# erreurs, avertissements ; et `source` (« utilisateur », « organisation », « projet », « fichier ») et
# `dossier`. Gardé par son texte canonique et la langue de ses messages.
faire_greffon <- function(fichiers, source, dossier = NULL) {
  fichiers <- lapply(fichiers, function(f) list(chemin = as.character(f$chemin), texte = texte_code(f$texte)))
  chemins <- vapply(fichiers, `[[`, "", "chemin")
  cle <- paste0(getOption("nuee.langue") %||% "fr", "\r", canon_greffon(fichiers))
  k <- match(cle, GREFFONS_SEANCE$cles)
  g <- if (!is.na(k)) GREFFONS_SEANCE$lus[[k]] else {
    manifeste <- if ("plugin.json" %in% chemins) fichiers[[match("plugin.json", chemins)]]$texte
    lu <- if (is.null(manifeste)) list(erreurs = traduire("Il n'a pas de plugin.json.", "It has no plugin.json."), avertissements = character(0))
          else lire_manifeste(manifeste, presents = setdiff(chemins, "plugin.json"))
    m <- lu$manifeste
    if (!is.null(m)) fichiers <- fichiers[chemins %in% c("plugin.json", m$fichiers)][order(match(chemins[chemins %in% c("plugin.json", m$fichiers)], c("plugin.json", m$fichiers)))]
    taille <- sum(vapply(fichiers, function(f) nchar(f$texte, type = "bytes"), 0))
    erreurs <- c(lu$erreurs, if (taille > GREFFON_TAILLE_MAX) sprintf(traduire("Ses fichiers font %s Mo : 5 Mo au plus.", "Its files make %s MB: 5 MB at most."), format(round(taille / 1024^2, 1))))
    if (!length(erreurs)) erreurs <- erreurs_code_greffon(fichiers, m)
    # (l'id et la version d'un manifeste refusé, pour le nommer quand même)
    brut <- if (is.null(m) && !is.null(manifeste)) tryCatch(jsonlite::fromJSON(manifeste, simplifyVector = FALSE), error = function(e) NULL)
    x <- list(id = m$id %||% (if (est_chaine(brut$id)) brut$id) %||% NA_character_, version = m$version %||% (if (est_chaine(brut$version)) brut$version) %||% NA_character_,
              manifeste = if (!length(erreurs)) m, fichiers = fichiers, empreinte = empreinte_greffon(fichiers), erreurs = erreurs, avertissements = lu$avertissements)
    GREFFONS_SEANCE$lus <- utils::tail(c(GREFFONS_SEANCE$lus, list(x)), 200)
    GREFFONS_SEANCE$cles <- utils::tail(c(GREFFONS_SEANCE$cles, cle), 200)
    x
  }
  g$source <- source
  g$dossier <- dossier
  g
}

# Le texte d'un fichier d'un greffon : de l'UTF-8 (sans sa marque d'ordre d'octets), sans octet nul,
# 1 Mo au plus.
lire_texte_greffon <- function(chemin, nom = basename(chemin)) {
  n <- file.size(chemin)
  if (is.na(n)) stop(sprintf(traduire("%s : introuvable.", "%s: not found."), nom), call. = FALSE)
  if (n > GREFFON_FICHIER_MAX) stop(sprintf(traduire("%s : 1 Mo au plus.", "%s: 1 MB at most."), nom), call. = FALSE)
  octets <- readBin(chemin, "raw", n)
  if (length(octets) >= 3 && identical(octets[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) octets <- octets[-(1:3)]
  if (any(octets == as.raw(0))) stop(sprintf(traduire("%s : ce n'est pas un fichier texte.", "%s: it isn't a text file."), nom), call. = FALSE)
  texte <- rawToChar(octets)
  Encoding(texte) <- "UTF-8"
  if (!validUTF8(texte)) stop(sprintf(traduire("%s : il n'est pas en UTF-8.", "%s: it isn't in UTF-8."), nom), call. = FALSE)
  texte
}

# Les fichiers d'un dossier de greffon (son plugin.json, et chacun des fichiers .R qu'il a, sous-dossiers
# compris) : list(chemin, texte).
fichiers_dossier_greffon <- function(dossier) {
  if (!file.exists(file.path(dossier, "plugin.json"))) stop(sprintf(traduire("Pas de plugin.json dans %s.", "No plugin.json in %s."), dossier), call. = FALSE)
  r <- list.files(dossier, "\\.[Rr]$", recursive = TRUE)
  r <- r[grepl(GREFFON_FICHIER_MOTIF, r)]
  lapply(c("plugin.json", r), function(f) list(chemin = f, texte = lire_texte_greffon(file.path(dossier, f), f)))
}

# Un greffon lu depuis un fichier : un .nueeplugin (une archive zip, plugin.json à sa racine ou dans un
# seul dossier), le plugin.json d'un dossier, ou ce dossier.
lire_fichier_greffon <- function(chemin) {
  chemin <- premier_texte(chemin)
  if (!nzchar(chemin) || !file.exists(chemin)) stop(sprintf(traduire("Fichier introuvable : %s", "File not found: %s"), chemin), call. = FALSE)
  if (dir.exists(chemin)) return(faire_greffon(fichiers_dossier_greffon(chemin), "fichier", chemin))
  if (identical(tolower(basename(chemin)), "plugin.json")) return(faire_greffon(fichiers_dossier_greffon(dirname(chemin)), "fichier", dirname(chemin)))
  liste <- tryCatch(utils::unzip(chemin, list = TRUE), error = function(e) NULL, warning = function(w) NULL)
  if (is.null(liste) || !nrow(liste)) stop(traduire("Ce n'est pas un greffon : un .nueeplugin est une archive zip qui contient plugin.json.",
                                                    "This isn't a plugin: a .nueeplugin is a zip archive holding plugin.json."), call. = FALSE)
  noms <- gsub("\\", "/", liste$Name, fixed = TRUE)
  if (any(startsWith(noms, "/") | grepl("^[A-Za-z]:", noms) | grepl("(^|/)\\.\\.(/|$)", noms))) {
    stop(traduire("L'archive a des chemins hors de son dossier : elle est refusée.", "The archive has paths outside its folder: it is refused."), call. = FALSE)
  }
  if (nrow(liste) > 500 || sum(liste$Length) > GREFFON_TAILLE_MAX) stop(traduire("L'archive est trop grande pour un greffon (5 Mo, 500 fichiers au plus).", "The archive is too large for a plugin (5 MB, 500 files at most)."), call. = FALSE)
  racine <- if ("plugin.json" %in% noms) "" else {
    k <- grep("^[^/]+/plugin\\.json$", noms)
    if (length(k) != 1) stop(traduire("L'archive n'a pas de plugin.json à sa racine (ni dans un seul dossier).", "The archive has no plugin.json at its root (nor in a single folder)."), call. = FALSE)
    sub("plugin\\.json$", "", noms[k])
  }
  dossier <- tempfile("greffon-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  garder <- startsWith(noms, racine) & !endsWith(noms, "/") & (substring(noms, nchar(racine) + 1) == "plugin.json" | grepl("\\.[Rr]$", noms))
  utils::unzip(chemin, files = liste$Name[garder], exdir = dossier)
  faire_greffon(fichiers_dossier_greffon(file.path(dossier, racine)), "fichier", chemin)
}

# ---------------------------------------------------------------- les dossiers des greffons

dossier_greffons <- function() Sys.getenv("NUEE_GREFFONS")
dossier_greffons_organisation <- function() Sys.getenv("NUEE_GREFFONS_ORGANISATION")

# Les greffons des deux dossiers, l'organisation d'abord : chacun lu (un dossier illisible l'est, avec
# son erreur), relus quand un de leurs fichiers change. `etat` : « ok », « invalide », « double » (un
# autre dossier de la même source a cet id), « remplace » (l'organisation donne cet id).
lister_greffons <- function() {
  racines <- c(organisation = dossier_greffons_organisation(), utilisateur = dossier_greffons())
  dossiers <- unlist(lapply(names(racines), function(s) {
    r <- racines[[s]]
    if (!nzchar(r) || !dir.exists(r)) return(NULL)
    d <- list.dirs(r, recursive = FALSE)
    stats::setNames(d[!startsWith(basename(d), ".")], rep(s, sum(!startsWith(basename(d), "."))))
  })) %||% character(0)
  fichiers <- as.character(unlist(lapply(dossiers, function(d) list.files(d, recursive = TRUE, full.names = TRUE))))
  infos <- file.info(fichiers, extra_cols = FALSE)
  signature <- paste(getOption("nuee.langue") %||% "fr", paste(racines, collapse = "|"), paste(fichiers, infos$size, as.numeric(infos$mtime), collapse = "|"))
  if (identical(signature, GREFFONS_SEANCE$signature)) return(GREFFONS_SEANCE$dossiers)
  gs <- lapply(seq_along(dossiers), function(k) {
    d <- dossiers[[k]]
    s <- names(dossiers)[k]
    tryCatch(faire_greffon(fichiers_dossier_greffon(d), s, d), error = function(e) {
      list(id = basename(d), version = NA_character_, manifeste = NULL, fichiers = list(), empreinte = NA_character_, erreurs = conditionMessage(e),
           avertissements = character(0), source = s, dossier = d)
    })
  })
  vus <- character(0)
  for (k in seq_along(gs)) {
    g <- gs[[k]]
    gs[[k]]$etat <- if (length(g$erreurs)) "invalide" else if (paste(g$source, g$id) %in% vus) "double"
                    else if (identical(g$source, "utilisateur") && paste("organisation", g$id) %in% vus) "remplace" else "ok"
    if (!length(g$erreurs)) vus <- c(vus, paste(g$source, g$id))
  }
  GREFFONS_SEANCE$signature <- signature
  GREFFONS_SEANCE$dossiers <- gs
  gs
}

# Ceux qui peuvent servir : l'un par id (celui de l'organisation quand elle le donne).
greffons_installes <- function() Filter(function(g) identical(g$etat, "ok"), lister_greffons())

empreintes_organisation <- function() vapply(Filter(function(g) identical(g$source, "organisation"), greffons_installes()), `[[`, "", "empreinte")

# ---------------------------------------------------------------- la confiance

# L'empreinte d'un greffon peut-elle s'exécuter ici ? Celle d'un greffon de l'organisation, toujours ;
# une autre, approuvée par l'utilisateur (code.R : APPROBATIONS_CODE), sauf avec la politique
# DisableCustomCode (code.R : code_interdit).
greffon_permis <- function(empreinte) {
  if (!est_chaine(empreinte)) return(FALSE)
  if (empreinte %in% empreintes_organisation()) return(TRUE)
  !code_interdit() && empreinte %in% APPROBATIONS_CODE$empreintes
}

# Un refus d'un greffon : une erreur d'une classe à elle, et ce que l'interface en reçoit (`details`).
refus_greffon <- function(classe, message, ...) refus_code(c(classe, "refus_greffon"), message, details = list(...))

nom_greffon <- function(g, fr = !identical(getOption("nuee.langue"), "en")) {
  n <- g$manifeste$nom
  if (is.null(n)) return(premier_texte(g$id, "?"))
  if (fr) n$fr else n$en
}

exiger_greffon_permis <- function(g, nom = "") {
  if (greffon_permis(g$empreinte)) return(invisible(TRUE))
  if (code_interdit()) {
    stop(refus_greffon("code_interdit", sprintf(traduire("Le modèle %s est fait du greffon « %s » %s : sur cet ordinateur, une politique de votre organisation (DisableCustomCode) ne laisse s'exécuter que les greffons qu'elle donne.",
                                                         "Model %s is made with the plugin \"%s\" %s: on this computer, your organisation's policy (DisableCustomCode) only lets the plugins it provides run."),
                                                premier_texte(nom, "?"), nom_greffon(g), premier_texte(g$version)), greffon_interdit = TRUE))
  }
  stop(refus_greffon("greffon_non_approuve", sprintf(traduire("Le modèle %s est fait du greffon « %s » %s, qui n'est ni installé ni approuvé sur cet ordinateur : il ne s'exécute pas tant que vous ne l'avez pas lu et installé (Montrer, Installer, dans la barre au-dessus du projet).",
                                                              "Model %s is made with the plugin \"%s\" %s, which isn't installed or approved on this computer: it doesn't run until you have read and installed it (Show, Install, in the bar above the project)."),
                                                     premier_texte(nom, "?"), nom_greffon(g), premier_texte(g$version)),
                     greffon_non_approuve = TRUE, empreinte = g$empreinte, id = g$id, version = g$version))
}

# La version de Nuée (main.js : NUEE_VERSION ; inconnue, rien n'est vérifié).
version_nuee <- function() Sys.getenv("NUEE_VERSION")
greffon_trop_recent <- function(g) {
  v <- version_nuee()
  !is.null(g$manifeste) && grepl(VERSION_MOTIF, v) && comparer_versions(g$manifeste$nuee, v) > 0
}

# Les paquets qu'il demande : là (sans les charger) et leur version. Les bibliothèques de l'utilisateur
# créées depuis le démarrage du moteur (Installer les paquets manquants, r-installes.js) sont d'abord
# ajoutées, juste avant celle de R.
paquets_greffon <- function(g) {
  biblio <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE))
  biblio <- biblio[nzchar(biblio) & dir.exists(biblio)]
  nouvelles <- setdiff(normalizePath(biblio, winslash = "/"), normalizePath(.libPaths(), winslash = "/"))
  if (length(nouvelles)) .libPaths(unique(c(setdiff(.libPaths(), .Library), nouvelles, .Library)))
  p <- g$manifeste$paquets %||% character(0)
  present <- vapply(p, function(x) nzchar(system.file(package = x)), TRUE, USE.NAMES = FALSE)
  version <- vapply(seq_along(p), function(k) if (present[k]) as.character(utils::packageDescription(p[k], fields = "Version")) else NA_character_, "")
  data.frame(nom = p, present = present, version = version, stringsAsFactors = FALSE)
}

exiger_paquets_greffon <- function(g) {
  pq <- paquets_greffon(g)
  manquants <- pq$nom[!pq$present]
  # (dans le navigateur, installés depuis le dépôt de webR quand il les a : navigateur.R)
  if (length(manquants) && dans_navigateur()) {
    manquants <- installer_paquets_navigateur(manquants)
    if (length(manquants)) stop(refus_greffon("greffon_paquets", paquets_absents_navigateur(manquants), paquets_manquants = I(manquants), id = g$id))
  }
  if (length(manquants)) {
    stop(refus_greffon("greffon_paquets", sprintf(traduire("Le greffon « %s » demande des paquets qui ne sont pas installés pour le R que Nuée utilise (R %s) : %s. Installez-les (Fichier › Greffons…, ou install.packages(%s) dans R), puis ajustez de nouveau.",
                                                           "The plugin \"%s\" needs packages that aren't installed for the R Nuée uses (R %s): %s. Install them (File › Plugins…, or install.packages(%s) in R), then fit again."),
                                                  nom_greffon(g), paste(R.version$major, R.version$minor, sep = "."), paste(manquants, collapse = ", "),
                                                  if (length(manquants) == 1) code_texte(manquants) else sprintf("c(%s)", paste(vapply(manquants, code_texte, ""), collapse = ", "))),
                     paquets_manquants = I(manquants), id = g$id))
  }
}

# ---------------------------------------------------------------- la définition

# Le greffon d'une définition de modèle : celui qu'elle garde (d$options$greffon$fichiers).
greffon_definition <- function(d) {
  f <- d$options$greffon$fichiers
  if (!length(f)) stop(sprintf(traduire("Le modèle %s n'a pas son greffon.", "Model %s doesn't have its plugin."), premier_texte(d$name, "?")), call. = FALSE)
  faire_greffon(f, "projet")
}

# Les greffons que garde un projet (ses modèles PLUGIN), un par empreinte.
greffons_du_projet <- function(projet) {
  ds <- Filter(function(d) identical(d$type, "PLUGIN") && length(d$options$greffon$fichiers), projet$models)
  gs <- lapply(ds, function(d) tryCatch(greffon_definition(d), error = function(e) NULL))
  gs <- Filter(Negate(is.null), gs)
  gs[!duplicated(vapply(gs, `[[`, "", "empreinte"))]
}

# Le greffon que demande l'interface (id ; empreinte, une version précise) : un installé (l'organisation
# d'abord), ou celui que garde un modèle du projet.
trouver_greffon <- function(projet, id, empreinte = "") {
  installes <- greffons_installes()
  if (nzchar(empreinte)) {
    g <- Find(function(g) identical(g$empreinte, empreinte), c(installes, greffons_du_projet(projet)))
    if (is.null(g)) stop(sprintf(traduire("Cette version du greffon %s n'est ni installée ni dans le projet.", "This version of the plugin %s is neither installed nor in the project."), id), call. = FALSE)
    return(g)
  }
  g <- Find(function(g) identical(g$id, id), installes)
  if (is.null(g)) stop(sprintf(traduire("Le greffon %s n'est pas installé (Fichier › Greffons…).", "The plugin %s isn't installed (File › Plugins…)."), id), call. = FALSE)
  g
}

# Les variables choisies pour les entrées du manifeste `m` (q : { <id> : [numéros] }), vérifiées : une
# entrée inconnue, une variable inconnue ou d'un type qu'elle n'accepte pas, trop ou trop peu.
# Rend { <id> : numéros } pour chaque entrée.
verifier_entrees_greffon <- function(projet, m, q) {
  q <- if (is.list(q)) q else list()
  inconnues <- setdiff(names(q), vapply(m$entrees, `[[`, "", "id"))
  if (length(inconnues)) stop(sprintf(traduire("Entrée inconnue du greffon : %s.", "Unknown input of the plugin: %s."), paste(inconnues, collapse = ", ")), call. = FALSE)
  out <- list()
  for (e in m$entrees) {
    u <- unique(suppressWarnings(as.numeric(unlist(q[[e$id]]))))
    libelle <- if (identical(getOption("nuee.langue"), "en")) e$libelle$en else e$libelle$fr
    for (x in u) {
      v <- Find(function(v) identical(as.numeric(v$uid), x), projet$variables)
      if (is.null(v) || is.na(x)) stop(sprintf(traduire("%s : une variable inconnue (%s).", "%s: an unknown variable (%s)."), libelle, format(x)), call. = FALSE)
      qualitative <- length(v$categories) > 0
      if ((identical(e$accepte, "quantitative") && qualitative) || (identical(e$accepte, "qualitative") && !qualitative)) {
        stop(sprintf(traduire("%s : %s est %s ; cette entrée prend des variables %s.", "%s: %s is %s; this input takes %s variables."), libelle, as.character(v$ids[1]),
                     if (qualitative) traduire("qualitative", "qualitative") else traduire("quantitative", "quantitative"),
                     if (identical(e$accepte, "quantitative")) traduire("quantitatives", "quantitative") else traduire("qualitatives", "qualitative")), call. = FALSE)
      }
    }
    if (length(u) < e$min) stop(sprintf(traduire("%s : choisissez %s.", "%s: choose %s."), libelle,
                                        if (identical(e$type, "variable")) traduire("une variable", "a variable") else sprintf(traduire("au moins %d variable(s)", "at least %d variable(s)"), e$min)), call. = FALSE)
    if (!is.na(e$max) && length(u) > e$max) stop(sprintf(traduire("%s : %d variable(s) au plus.", "%s: %d variable(s) at most."), libelle, e$max), call. = FALSE)
    out[[e$id]] <- u
  }
  out
}

# Les valeurs des options (q : { <id> : valeur }), vérifiées ; une absente prend sa valeur par défaut.
verifier_options_greffon <- function(m, q) {
  q <- if (is.list(q)) q else list()
  inconnues <- setdiff(names(q), vapply(m$options, `[[`, "", "id"))
  if (length(inconnues)) stop(sprintf(traduire("Option inconnue du greffon : %s.", "Unknown option of the plugin: %s."), paste(inconnues, collapse = ", ")), call. = FALSE)
  out <- list()
  for (o in m$options) {
    v <- q[[o$id]]
    if (is.list(v)) v <- unlist(v)
    if (!length(v)) v <- o$defaut
    libelle <- if (identical(getOption("nuee.langue"), "en")) o$libelle$en else o$libelle$fr
    refus <- function(fr, en, ...) stop(paste0(libelle, traduire(" : ", ": "), sprintf(traduire(fr, en), ...)), call. = FALSE)
    out[[o$id]] <- switch(o$type,
      number = {
        x <- suppressWarnings(as.numeric(v)[1])
        if (!is.finite(x)) refus("un nombre", "a number")
        if (!is.na(o$min) && x < o$min) refus("%s au moins", "%s at least", format(o$min))
        if (!is.na(o$max) && x > o$max) refus("%s au plus", "%s at most", format(o$max))
        if (o$entier && x != round(x)) refus("un entier", "a whole number")
        x
      },
      choice = {
        x <- as.character(v)[1]
        valeurs <- vapply(o$valeurs, `[[`, "", "valeur")
        if (is.na(x) || !x %in% valeurs) refus("l'une des valeurs %s", "one of the values %s", paste(valeurs, collapse = ", "))
        x
      },
      checkbox = {
        if (!is.logical(v) || is.na(v[1])) refus("vrai ou faux", "true or false")
        v[1]
      },
      text = {
        x <- as.character(v)[1]
        if (is.na(x) || nchar(x) > 10000) refus("un texte (10 000 caractères au plus)", "a text (10,000 characters at most)")
        x
      })
  }
  out
}

# (definir_modele, nouveau.R) La définition d'un modèle de greffon : le greffon qu'elle demande
# (d$options$greffon : id, et empreinte pour une version précise), gardé en entier, ses entrées et ses
# options vérifiées, et les colonnes qu'elles nomment (pour les étapes des données : etapes.R).
definir_plugin <- function(projet, d) {
  o <- d$options %||% list()
  id <- premier_texte(o$greffon$id)
  if (!nzchar(id)) stop(traduire("Quel greffon ? La définition n'en nomme pas (options$greffon$id).", "Which plugin? The definition names none (options$greffon$id)."), call. = FALSE)
  g <- trouver_greffon(projet, id, premier_texte(o$greffon$empreinte))
  if (length(g$erreurs)) stop(sprintf(traduire("Le greffon %s a des erreurs : %s", "The plugin %s has errors: %s"), id, paste(g$erreurs, collapse = " ; ")), call. = FALSE)
  if (code_interdit() && !g$empreinte %in% empreintes_organisation()) {
    stop(sprintf(traduire("Sur cet ordinateur, une politique de votre organisation (DisableCustomCode) ne permet que les greffons qu'elle donne : %s n'en est pas.",
                          "On this computer, your organisation's policy (DisableCustomCode) only allows the plugins it provides: %s isn't one of them."), nom_greffon(g)), call. = FALSE)
  }
  if (greffon_trop_recent(g)) stop(sprintf(traduire("Le greffon « %s » demande Nuée %s ou plus récent (celle-ci : %s).", "The plugin \"%s\" needs Nuée %s or newer (this one: %s)."),
                                           nom_greffon(g), g$manifeste$nuee, version_nuee()), call. = FALSE)
  entrees <- verifier_entrees_greffon(projet, g$manifeste, o$entrees)
  d$options <- list(greffon = list(id = g$id, version = g$version, empreinte = g$empreinte, fichiers = g$fichiers),
                    entrees = entrees, options = verifier_options_greffon(g$manifeste, o$options),
                    colonnes = lapply(unique(unlist(entrees)), function(u) list(by = "variable", variable = u)))
  d
}

# Il vérifie lui-même ses données, comme un modèle de code R.
verifier_plugin <- function(projet, d) list(variables = data.frame(), observations = data.frame())

# ---------------------------------------------------------------- l'ajustement

# Ce que la fonction du greffon reçoit : le nom de chaque variable choisie (une chaîne ou NULL pour une
# entrée « variable »), et la valeur de chaque option (un entier pour un nombre entier).
entrees_fonction_greffon <- function(projet, m, entrees) {
  out <- list()
  for (e in m$entrees) {
    noms <- vapply(as.numeric(unlist(entrees[[e$id]])), function(u) as.character(variable_du_projet(projet, u)$ids[1]), "")
    if (identical(e$type, "variables")) out[[e$id]] <- noms else if (length(noms)) out[[e$id]] <- noms[1]
  }
  out
}
options_fonction_greffon <- function(m, options) {
  out <- list()
  for (o in m$options) {
    v <- options[[o$id]]
    if (is.list(v)) v <- unlist(v)
    if (!length(v)) v <- o$defaut
    out[[o$id]] <- switch(o$type, number = if (o$entier) as.integer(round(as.numeric(v))) else as.numeric(v), checkbox = isTRUE(v), as.character(v))
  }
  out
}

# Les méthodes S3 des génériques que Nuée appelle (GREFFON_GENERIQUES), que définissent les fichiers du
# greffon lus dans `env` : enregistrées, pour que summary(), logLik()… les trouvent hors de `env`. Une
# méthode d'un paquet pour la même classe reste la sienne.
enregistrer_methodes_greffon <- function(env) {
  for (nom in ls(env, all.names = TRUE)) {
    f <- get(nom, envir = env)
    if (!is.function(f)) next
    for (g in GREFFON_GENERIQUES) {
      if (!startsWith(nom, paste0(g, ".")) || nchar(nom) <= nchar(g) + 1) next
      classe <- substring(nom, nchar(g) + 2)
      deja <- utils::getS3method(g, classe, optional = TRUE)
      if (is.function(deja) && isNamespace(topenv(environment(deja)))) next
      registerS3method(g, classe, f, envir = env)
    }
  }
}

# Les fichiers R du greffon lus dans un environnement neuf (son parent : l'environnement global, comme
# pour un modèle de code R), leurs méthodes enregistrées. Une erreur : list(erreur, fichier, ligne).
environnement_greffon <- function(g) {
  env <- new.env(parent = globalenv())
  for (f in g$fichiers) {
    if (identical(f$chemin, "plugin.json")) next
    lignes <- strsplit(f$texte, "\n", fixed = TRUE)[[1]]
    exprs <- tryCatch(parse(text = lignes, keep.source = TRUE, srcfile = srcfilecopy(f$chemin, lignes)), error = function(e) e)
    if (inherits(exprs, "error")) return(list(erreur = conditionMessage(exprs), fichier = f$chemin, ligne = NA_integer_))
    refs <- attr(exprs, "srcref")
    for (k in seq_along(exprs)) {
      r <- tryCatch({ eval(exprs[[k]], env); NULL }, error = function(e) list(erreur = conditionMessage(e), fichier = f$chemin, ligne = as.integer(refs[[k]][1])))
      if (!is.null(r)) return(r)
    }
  }
  enregistrer_methodes_greffon(env)
  env
}

# La fonction du greffon évaluée sur `donnees` comme le code d'un modèle de code R (code.R :
# evaluer_code : sa sortie, ses messages, ses avertissements, ses graphiques), dans un environnement
# dont le parent est celui où ses fichiers ont été lus. `paquets` : ceux qu'il nomme, dans le manifeste
# et dans ses fichiers.
evaluer_greffon <- function(g, donnees, entrees, options) {
  nommes <- unlist(lapply(g$fichiers, function(f) if (!identical(f$chemin, "plugin.json")) paquets_nommes(tryCatch(parse(text = f$texte, keep.source = FALSE), error = function(e) NULL))))
  # (dans le navigateur, les paquets que ses fichiers nomment installés d'abord : navigateur.R)
  ev <- evaluer_navigateur(function() {
    env <- environnement_greffon(g)
    if (!is.environment(env)) return(c(env, list(valeur = NULL, pile = list())))
    evaluer_code(sprintf("%s(donnees, entrees, options)", g$manifeste$fonction), donnees, objets = list(entrees = entrees, options = options), parent = env)
  }, nommes)
  if (is.null(ev$fichier)) ev$paquets <- setdiff(unique(c(g$manifeste$paquets, nommes, ev$paquets)), PAQUETS_DE_R)
  ev
}

# L'erreur d'un greffon qui s'arrête : son message, le fichier et la ligne du greffon où il s'est
# arrêté quand R les sait ; un paquet qui manque, comment l'installer.
erreur_greffon <- function(ev, g) {
  nom <- sprintf("%s %s", guillemets_texte(nom_greffon(g), !identical(getOption("nuee.langue"), "en")), g$version)
  fichiers <- vapply(g$fichiers, `[[`, "", "chemin")
  lieu <- if (!is.null(ev$fichier)) list(fichier = ev$fichier, ligne = ev$ligne) else Find(function(x) x$fichier %in% fichiers, rev(ev$pile))
  ou <- if (is.null(lieu)) "" else if (length(lieu$ligne) && !is.na(lieu$ligne)) sprintf(traduire(" (%s, ligne %d)", " (%s, line %d)"), lieu$fichier, lieu$ligne)
        else sprintf(" (%s)", lieu$fichier)
  message <- sprintf(if (!is.null(ev$fichier)) traduire("Le greffon %s ne se charge pas%s : %s", "The plugin %s doesn't load%s: %s") else traduire("Le greffon %s s'est arrêté%s : %s", "The plugin %s stopped%s: %s"),
                     nom, ou, ev$erreur)
  if (length(ev$paquet) && nzchar(ev$paquet)) {
    # (dans le navigateur, un paquet que le dépôt de webR n'a pas : navigateur.R)
    message <- paste(message, if (dans_navigateur()) paquets_absents_navigateur(ev$paquet)
                              else sprintf(traduire("Le paquet %s n'est pas installé pour le R que Nuée utilise (R %s) : installez-le (install.packages(\"%s\")), puis ajustez de nouveau.",
                                                    "The package %s isn't installed for the R Nuée uses (R %s): install it (install.packages(\"%s\")), then fit again."),
                                           ev$paquet, paste(R.version$major, R.version$minor, sep = "."), ev$paquet))
  }
  refus_code("erreur_code", message)
}

# Ce que le moteur dit du greffon d'un modèle, dans ses résultats et ses textes.
fiche_greffon <- function(g) {
  m <- g$manifeste
  list(id = g$id, version = g$version, empreinte = g$empreinte, nom = m$nom, description = m$description, licence = m$licence,
       auteurs = lapply(m$auteurs, function(a) list(nom = a$nom, orcid = a$orcid)), citations = m$citations, fonction = m$fonction,
       nuee = m$nuee, fichiers = I(vapply(g$fichiers, `[[`, "", "chemin")))
}

ajuster_plugin <- function(projet, d) {
  g <- greffon_definition(d)
  if (length(g$erreurs)) stop(sprintf(traduire("Le greffon du modèle %s a des erreurs : %s", "The plugin of model %s has errors: %s"), premier_texte(d$name, "?"), paste(g$erreurs, collapse = " ; ")), call. = FALSE)
  exiger_greffon_permis(g, d$name)
  if (greffon_trop_recent(g)) stop(sprintf(traduire("Le greffon « %s » %s demande Nuée %s ou plus récent (celle-ci : %s).", "The plugin \"%s\" %s needs Nuée %s or newer (this one: %s)."),
                                           nom_greffon(g), g$version, g$manifeste$nuee, version_nuee()), call. = FALSE)
  exiger_paquets_greffon(g)
  m_g <- g$manifeste
  donnees <- donnees_code(projet, d)
  entrees <- entrees_fonction_greffon(projet, m_g, d$options$entrees)
  options <- options_fonction_greffon(m_g, d$options$options)
  ev <- evaluer_greffon(g, donnees, entrees, options)
  if (!is.null(ev$erreur)) stop(erreur_greffon(ev, g))
  v <- ev$valeur
  if (is.null(v) || (forme_liste_code(v) && is.null(v[["modele"]]) && !length(v[["valeurs"]]) && !length(v[["tableaux"]]))) {
    stop(refus_code("erreur_code", sprintf(traduire("La fonction %s du greffon « %s » n'a rendu ni modèle, ni valeurs, ni tableaux.", "The function %s of the plugin \"%s\" returned no model, values or tables."),
                                           m_g$fonction, nom_greffon(g))))
  }
  m <- modele_valeur_code(projet, d, ev, donnees, type = "PLUGIN")
  m$code <- sprintf("%s(donnees, entrees, options)", m_g$fonction)
  m$empreinte <- g$empreinte
  m$greffon <- fiche_greffon(g)
  m$fichiers_greffon <- g$fichiers
  m$entrees <- entrees
  m$options_greffon <- options
  class(m) <- c("modele_greffon", "modele_code")
  m
}

# ---------------------------------------------------------------- l'interface

# Un texte dans la langue de la demande, d'un manifeste ({en, fr}).
texte_langue <- function(x, fr = !identical(getOption("nuee.langue"), "en")) if (is.null(x)) "" else if (fr) x$fr else x$en

# Chaque entrée et chaque option du modèle, comme ses résultats les disent : son libellé et ce qui a
# été choisi (les noms des variables ; la valeur, le libellé d'un choix).
reglages_greffon <- function(m_g, entrees, options) {
  list(entrees = I(lapply(m_g$entrees, function(e) list(id = e$id, libelle = e$libelle, variables = I(as.character(unlist(entrees[[e$id]] %||% character(0))))))),
       options = I(lapply(m_g$options, function(o) {
         v <- options[[o$id]]
         choix <- if (identical(o$type, "choice")) Find(function(x) identical(x$valeur, v), o$valeurs)
         list(id = o$id, type = o$type, libelle = o$libelle, valeur = v, texte = if (!is.null(choix)) choix$libelle)
       })))
}

resultat_plugin <- function(id) {
  r <- resultat_code(id)
  e <- modele(id)
  m <- e$modele
  d <- e$definition
  fr <- !identical(getOption("nuee.langue"), "en")
  installe <- Find(function(x) identical(x$id, m$greffon$id), greffons_installes())
  r$type <- "PLUGIN"
  m_g <- greffon_definition(d)$manifeste
  # (le manifeste de sa version, pour refaire son dialogue même quand elle n'est pas installée ; ce qui a
  # été choisi ; la version installée de même id)
  r$greffon <- c(m$greffon, list(manifeste = specs_greffon(m_g), reglages = reglages_greffon(m_g, m$entrees, m$options_greffon),
                                 installe = if (!is.null(installe)) list(version = installe$version, empreinte = installe$empreinte, source = installe$source)))
  r$chiffres <- paste(c(texte_langue(m$greffon$nom, fr), sprintf("N %d", as.integer(m$N)), if (!is.null(m$statistiques$AIC)) sprintf("AIC %s", nombre_texte(m$statistiques$AIC, 1, fr))), collapse = " · ")
  # (ses deux vues d'abord, celles qu'il a : ses coefficients, ses valeurs et ses tableaux, ses résidus ;
  # sinon le greffon, ses graphiques : app.js, vuesParDefaut, en ouvre deux)
  vues <- c(if (NROW(m$coefficients)) "code_coefficients", if (length(m$valeurs) || length(m$tableaux)) "code_tableaux", if (length(m$ajustees)) "code_diagnostics")
  r$vues_defaut <- I(utils::head(c(vues, "greffon_source", "code_graphiques"), 2))
  # (sans le code du greffon : ce que l'interface renvoie pour le modifier n'en a pas besoin)
  if (!is.null(r$reglages$options$greffon)) r$reglages$options$greffon$fichiers <- NULL
  r
}

# Le greffon d'un modèle tel que le résumé du projet le dit (serveur.R : resume_modele) : qui il est,
# s'il peut s'exécuter ici, et la version installée de même id.
resume_greffon_modele <- function(d) {
  g <- tryCatch(greffon_definition(d), error = function(e) NULL)
  if (is.null(g)) return(list(valide = FALSE, approuve = FALSE, empreinte = NA_character_))
  installe <- Find(function(x) identical(x$id, g$id), greffons_installes())
  list(id = g$id, version = g$version, empreinte = g$empreinte, valide = !length(g$erreurs), approuve = greffon_permis(g$empreinte),
       nom = g$manifeste$nom, auteurs = I(vapply(g$manifeste$auteurs %||% list(), `[[`, "", "nom")),
       interdit = code_interdit() && !g$empreinte %in% empreintes_organisation(),
       installe = if (!is.null(installe)) list(version = installe$version, empreinte = installe$empreinte, source = installe$source))
}

# Les entrées et les options d'un manifeste, telles que l'interface en fait le dialogue d'un modèle
# (interface/vues_greffons.js).
specs_greffon <- function(m) {
  list(entrees = I(lapply(m$entrees, function(e) list(id = e$id, type = e$type, accepte = e$accepte, min = e$min, max = if (is.na(e$max)) NULL else e$max,
                                                     facultatif = e$facultatif, libelle = e$libelle, aide = e$aide))),
       options = I(lapply(m$options, function(o) c(list(id = o$id, type = o$type, libelle = o$libelle, aide = o$aide, defaut = o$defaut),
                                                  if (identical(o$type, "number")) list(min = if (is.na(o$min)) NULL else o$min, max = if (is.na(o$max)) NULL else o$max, entier = o$entier),
                                                  if (identical(o$type, "choice")) list(valeurs = I(o$valeurs))))))
}

# Un greffon tel que le dialogue des greffons et les menus le montrent (et le dialogue d'installation,
# `code` : avec ses fichiers).
greffon_interface <- function(g, code = FALSE) {
  m <- g$manifeste
  pq <- if (!is.null(m)) paquets_greffon(g) else data.frame(nom = character(0), present = logical(0), version = character(0))
  c(list(id = g$id, version = g$version, source = g$source, dossier = if (g$source %in% c("utilisateur", "organisation")) g$dossier,
         etat = g$etat %||% (if (length(g$erreurs)) "invalide" else "ok"),
         empreinte = g$empreinte, permis = greffon_permis(g$empreinte), trop_recent = greffon_trop_recent(g),
         erreurs = I(g$erreurs), avertissements = I(g$avertissements),
         paquets = list(nom = I(pq$nom), present = I(pq$present), version = I(pq$version))),
    if (!is.null(m)) c(list(nom = m$nom, description = m$description, licence = m$licence, nuee = m$nuee, fonction = m$fonction,
                            auteurs = I(lapply(m$auteurs, function(a) list(nom = a$nom, orcid = a$orcid))), citations = I(m$citations)), specs_greffon(m)),
    if (code) list(fichiers = I(lapply(g$fichiers, function(f) list(chemin = f$chemin, texte = f$texte)))))
}

# Tout ce que l'interface sait des greffons (serveur.R : etat_complet ; la demande greffons) : les deux
# dossiers, la politique, et chaque greffon de ces dossiers.
greffons_interface <- function() {
  list(dossier = if (nzchar(dossier_greffons())) dossier_greffons(), organisation = if (nzchar(dossier_greffons_organisation())) dossier_greffons_organisation(),
       interdit = code_interdit(), installation = nzchar(dossier_greffons()) && !code_interdit(),
       liste = I(lapply(lister_greffons(), greffon_interface)))
}

# ---------------------------------------------------------------- les demandes

demande_analyse_greffons <- function(q) greffons_interface()

# Un greffon lu depuis un fichier (q$chemin : un .nueeplugin, un plugin.json, un dossier), avant de
# l'installer : son manifeste, son code, ses erreurs, la version déjà installée de même id ; rien ne
# s'exécute ni ne s'écrit.
demande_analyse_greffon_lire <- function(q) {
  g <- lire_fichier_greffon(q$chemin)
  installation_greffon(g)
}

# Ce que le dialogue d'installation dit d'un greffon lu : ce qu'il est, son code, s'il peut s'installer
# et sinon pourquoi, et ce qu'il remplace.
installation_greffon <- function(g) {
  installe <- Find(function(x) identical(x$id, g$id), Filter(function(x) !length(x$erreurs), lister_greffons()))
  organisation <- !is.null(installe) && identical(installe$source, "organisation")
  raison <- if (length(g$erreurs)) traduire("Le greffon a des erreurs : il ne peut pas être installé.", "The plugin has errors: it can't be installed.")
            else if (code_interdit()) traduire("Une politique de votre organisation (DisableCustomCode) ne permet que les greffons qu'elle donne.", "Your organisation's policy (DisableCustomCode) only allows the plugins it provides.")
            else if (!nzchar(dossier_greffons())) traduire("Les greffons s'installent depuis l'application de bureau.", "Plugins are installed from the desktop application.")
            else if (organisation) sprintf(traduire("Votre organisation donne déjà ce greffon (version %s) : c'est le sien qui sert.", "Your organisation already provides this plugin (version %s): its own is used."), installe$version)
            else if (greffon_trop_recent(g)) sprintf(traduire("Il demande Nuée %s ou plus récent (celle-ci : %s).", "It needs Nuée %s or newer (this one: %s)."), g$manifeste$nuee, version_nuee())
            else ""
  list(greffon = greffon_interface(g, code = TRUE), peut = !nzchar(raison), raison = raison,
       installe = if (!is.null(installe)) list(version = installe$version, empreinte = installe$empreinte, source = installe$source,
                                               ordre = if (!is.na(installe$version) && !is.na(g$version)) comparer_versions(g$version, installe$version),
                                               meme = identical(installe$empreinte, g$empreinte)))
}

# Installer un greffon (q$chemin, ou q$projet et q$numero : celui que garde un modèle du projet) dans le
# dossier de l'utilisateur, à la place de la version qu'il avait. q$empreinte : celle que l'utilisateur
# a lue et approuvée ; un fichier changé depuis est refusé. (L'approbation elle-même vient de
# l'application : demande code_approuve.)
demande_analyse_greffon_installer <- function(q) {
  g <- if (!is.null(q$chemin)) lire_fichier_greffon(q$chemin)
       else greffon_definition(modele_du_projet(projet(q$projet), q$numero))
  i <- installation_greffon(g)
  if (!i$peut) stop(i$raison, call. = FALSE)
  if (!identical(premier_texte(q$empreinte), g$empreinte)) {
    stop(traduire("Le greffon a changé depuis que vous l'avez lu : lisez-le de nouveau avant de l'installer.", "The plugin changed since you read it: read it again before installing it."), call. = FALSE)
  }
  racine <- dossier_greffons()
  dir.create(racine, recursive = TRUE, showWarnings = FALSE)
  neuf <- file.path(racine, sprintf(".%s-%s", g$id, basename(tempfile(""))))
  dir.create(neuf)
  for (f in g$fichiers) {
    dir.create(dirname(file.path(neuf, f$chemin)), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(enc2utf8(f$texte)), file.path(neuf, f$chemin))
  }
  final <- file.path(racine, g$id)
  ancien <- if (dir.exists(final)) file.path(racine, sprintf(".%s-ancien-%s", g$id, basename(tempfile("")))) else NULL
  if (!is.null(ancien) && !file.rename(final, ancien)) {
    unlink(neuf, recursive = TRUE)
    stop(sprintf(traduire("La version installée de %s ne peut pas être remplacée (un fichier ouvert ?).", "The installed version of %s can't be replaced (a file in use?)."), g$id), call. = FALSE)
  }
  if (!file.rename(neuf, final)) {
    if (!is.null(ancien)) file.rename(ancien, final)
    unlink(neuf, recursive = TRUE)
    stop(sprintf(traduire("Le greffon %s ne peut pas être écrit dans %s.", "The plugin %s can't be written into %s."), g$id, racine), call. = FALSE)
  }
  if (!is.null(ancien)) unlink(ancien, recursive = TRUE)
  g2 <- Find(function(x) identical(x$dossier, final) || identical(normalizePath(x$dossier, winslash = "/", mustWork = FALSE), normalizePath(final, winslash = "/", mustWork = FALSE)), lister_greffons())
  list(greffon = greffon_interface(g2 %||% g), remplace = i$installe$version %||% NULL)
}

# Retirer un greffon de l'utilisateur (q$id). Son approbation reste : les projets qui le gardent se
# refont encore avec lui.
demande_analyse_greffon_retirer <- function(q) {
  id <- premier_texte(q$id)
  g <- Find(function(x) identical(x$id, id) && identical(x$source, "utilisateur"), lister_greffons())
  if (is.null(g)) stop(sprintf(traduire("Pas de greffon %s parmi les vôtres (ceux de votre organisation ne se retirent pas ici).", "No plugin %s among yours (your organisation's aren't removed here)."), id), call. = FALSE)
  unlink(g$dossier, recursive = TRUE)
  if (dir.exists(g$dossier)) stop(sprintf(traduire("Le dossier %s ne peut pas être supprimé (un fichier ouvert ?).", "The folder %s can't be deleted (a file in use?)."), g$dossier), call. = FALSE)
  list(retire = id)
}

# Les greffons que garde le projet q$projet : chacun (une fois par empreinte), ses modèles, s'il peut
# s'exécuter ici, la version installée de même id, et son code (Montrer).
demande_analyse_greffon_projet <- function(q) {
  p <- projet(q$projet)
  ds <- Filter(function(d) identical(d$type, "PLUGIN"), p$models)
  gs <- lapply(greffons_du_projet(p), function(g) {
    modeles <- Filter(function(d) identical(tryCatch(greffon_definition(d)$empreinte, error = function(e) ""), g$empreinte), ds)
    installe <- Find(function(x) identical(x$id, g$id), greffons_installes())
    c(greffon_interface(g, code = TRUE),
      list(modeles = I(lapply(modeles, function(d) list(nom = d$name %||% "", numero = d$number %||% NA))),
           interdit = code_interdit() && !g$empreinte %in% empreintes_organisation(),
           installe = if (!is.null(installe)) list(version = installe$version, empreinte = installe$empreinte, source = installe$source,
                                                   ordre = comparer_versions(g$version, installe$version), meme = identical(installe$empreinte, g$empreinte))))
  })
  list(greffons = I(gs))
}

# Les modèles du projet q$projet faits du greffon q$id (ou seulement ceux de q$numeros) passés à sa
# version installée : chacun redéfini avec ses entrées et ses options (celles que la version installée
# connaît encore ; les nouvelles, à leur valeur par défaut). Rend les numéros de ceux qui ont changé,
# à réajuster, et ceux qui ne le peuvent pas, avec pourquoi.
demande_analyse_greffon_mettre_a_jour <- function(q) {
  p <- projet(q$projet)
  id <- premier_texte(q$id)
  g <- Find(function(x) identical(x$id, id), greffons_installes())
  if (is.null(g)) stop(sprintf(traduire("Le greffon %s n'est pas installé.", "The plugin %s isn't installed."), id), call. = FALSE)
  numeros <- if (length(q$numeros)) as.numeric(unlist(q$numeros))
  changes <- numeric(0)
  refus <- list()
  for (k in seq_along(p$models)) {
    d <- p$models[[k]]
    if (!identical(d$type, "PLUGIN") || !identical(premier_texte(d$options$greffon$id), id)) next
    if (!is.null(numeros) && !as.numeric(d$number %||% NA) %in% numeros) next
    if (identical(premier_texte(d$options$greffon$empreinte), g$empreinte)) next
    o <- d$options
    garder <- function(x, ids) if (is.list(x)) x[intersect(names(x), ids)] else list()
    d1 <- d
    d1$options <- list(greffon = list(id = id, empreinte = g$empreinte), entrees = garder(o$entrees, vapply(g$manifeste$entrees, `[[`, "", "id")),
                       options = garder(o$options, vapply(g$manifeste$options, `[[`, "", "id")))
    d2 <- tryCatch(definir_plugin(p, d1), error = function(e) e)
    if (inherits(d2, "error")) {
      refus <- c(refus, list(list(nom = d$name %||% "", erreur = conditionMessage(d2))))
      next
    }
    d2$fitted <- FALSE
    p$models[[k]] <- d2
    changes <- c(changes, as.numeric(d$number %||% NA))
  }
  if (length(changes)) {
    memoriser(projet(q$projet), sprintf(traduire("passer au greffon %s %s", "update to the plugin %s %s"), id, g$version))
    p$modifie <- TRUE
    etat$projets[[p$id]] <- p
  }
  list(numeros = I(changes), refus = I(refus), version = g$version)
}

# ---------------------------------------------------------------- Exporter, prédire, Ce qui a changé

# Ses tableaux pour Exporter : ceux d'un modèle de code R (code.R : tables_code), le greffon (qui il est,
# ses entrées et ses options), et son code, fichier par fichier, à la place du code d'un modèle de code R.
tables_plugin <- function(projet, m) {
  out <- tables_code(projet, m)
  g <- m$greffon
  fr <- !identical(getOption("nuee.langue"), "en")
  reglages <- c(vapply(names(m$entrees), function(k) sprintf("%s = %s", k, paste(m$entrees[[k]], collapse = ", ")), ""),
                vapply(names(m$options_greffon), function(k) sprintf("%s = %s", k, format(m$options_greffon[[k]])), ""))
  greffon <- data.frame(id = g$id, version = g$version, nom = texte_langue(g$nom, fr), auteurs = paste(vapply(g$auteurs, `[[`, "", "nom"), collapse = ", "),
                        licence = g$licence %||% "", fonction = g$fonction, empreinte = g$empreinte, reglages = paste(reglages, collapse = " ; "), stringsAsFactors = FALSE)
  out$code <- do.call(rbind, lapply(m$fichiers_greffon, function(f) data.frame(fichier = f$chemin, code = strsplit(f$texte, "\n", fixed = TRUE)[[1]], stringsAsFactors = FALSE)))
  c(out["resume"], list(greffon = greffon), out[setdiff(names(out), "resume")])
}

# Ses prédictions, comme celles d'un modèle de code R (predict(fit, newdata)), ses méthodes enregistrées
# de nouveau (un autre greffon de la même classe a pu en enregistrer d'autres depuis).
predire_plugin <- function(projet, m, d, observations) {
  exiger_greffon_permis(faire_greffon(m$fichiers_greffon, "projet"), d$name)
  environnement_greffon(list(fichiers = m$fichiers_greffon))
  predire_code(projet, m, d, observations)
}

entrees_plugin <- function(projet, d, m) entrees_code(projet, d, m)

# ---------------------------------------------------------------- texte des méthodes

# Le court d'une citation de son BibTeX (« MacKenzie et al., 2002 ») ; NULL sans auteur et année.
citation_courte_bibtex <- function(b, fr) {
  if (!length(b) || !nzchar(b)) return(NULL)
  champ <- function(nom) {
    x <- regmatches(b, regexec(sprintf("(?i)\\b%s\\s*=\\s*[{\"]((?:[^{}]|\\{[^{}]*\\})*)[}\"]", nom), b, perl = TRUE))[[1]]
    if (length(x) == 2) trimws(gsub("[{}]", "", x[2])) else ""
  }
  auteurs <- trimws(strsplit(champ("author"), "\\s+and\\s+")[[1]])
  annee <- champ("year")
  if (!length(auteurs) || !nzchar(auteurs[1]) || !nzchar(annee)) return(NULL)
  famille <- vapply(auteurs, function(a) if (grepl(",", a)) trimws(sub(",.*$", "", a)) else utils::tail(strsplit(a, "\\s+")[[1]], 1), "", USE.NAMES = FALSE)
  court <- if (length(famille) >= 3) paste(famille[1], "et al.") else if (length(famille) == 2) paste(famille[1], if (fr) "et" else "and", famille[2]) else famille[1]
  sprintf("%s, %s", court, annee)
}

texte_methodes_plugin <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  g <- m$greffon
  s <- m$statistiques
  nombre <- function(x, chiffres = 1) nombre_texte(x, chiffres, fr)
  titre <- as.character(d$title %||% "")
  auteurs <- vapply(g$auteurs, `[[`, "", "nom")
  obs <- observations_texte(m$N, d, fr)
  if (m$N < m$n_donnees) obs <- paste(obs, sprintf(tx("(sur les %d que Nuée lui a données)", "(of the %d Nuée gave it)"), m$n_donnees))
  # ses entrées et ses options, en mots
  m_g <- list(entrees = greffon_definition(d)$manifeste$entrees, options = greffon_definition(d)$manifeste$options)
  rg <- reglages_greffon(m_g, m$entrees, m$options_greffon)
  deux_points <- tx(" : ", ": ")
  entrees <- unlist(lapply(rg$entrees, function(e) if (length(e$variables)) paste0(tolower_premiere(texte_langue(e$libelle, fr)), deux_points, liste_mots(e$variables, fr))))
  options <- unlist(lapply(rg$options, function(o) paste0(tolower_premiere(texte_langue(o$libelle, fr)), deux_points,
                                                           if (!is.null(o$texte)) tolower_premiere(texte_langue(o$texte, fr)) else if (is.logical(o$valeur)) (if (isTRUE(o$valeur)) tx("oui", "yes") else tx("non", "no"))
                                                           else if (is.numeric(o$valeur)) nombre_texte(o$valeur, max(0, min(6, nchar(sub("^[^.]*\\.?", "", format(o$valeur))))), fr)
                                                           else if (nzchar(o$valeur)) guillemets_texte(o$valeur, fr) else tx("aucun", "none"))))
  citations <- Filter(function(x) nzchar(x$texte %||% ""), g$citations %||% list())
  courtes <- unlist(lapply(citations, function(x) citation_courte_bibtex(x$bibtex, fr)))
  stats <- c(if (!is.null(s$logLik)) sprintf(tx("log-vraisemblance %s (%s ddl)", "log-likelihood %s (%s df)"), nombre(s$logLik, 2), format(s$ddl %||% NA)),
             if (!is.null(s$AIC)) sprintf("AIC = %s", nombre(s$AIC)))
  point_virgule <- tx(" ; ", "; ")
  p1 <- paste(c(
    sprintf(tx("Le modèle %s%s a été ajusté avec le greffon %s %s de Nuée, de %s (sa fonction %s%s), sur %s.",
               "Model %s%s was fitted with the Nuée plugin %s %s by %s (its function %s%s), on %s."),
            d$name %||% "", if (nzchar(titre)) sprintf(" (%s)", titre) else "", guillemets_texte(texte_langue(g$nom, fr), fr), g$version,
            liste_mots(auteurs, fr), g$fonction, if (length(courtes)) sprintf(tx(" ; sa méthode : %s", "; its method: %s"), paste(courtes, collapse = point_virgule)) else "", obs),
    if (length(entrees)) sprintf(tx("Ses entrées : %s.", "Its inputs: %s."), paste(entrees, collapse = point_virgule)),
    if (length(options)) sprintf(tx("Ses options : %s.", "Its options: %s."), paste(options, collapse = point_virgule)),
    if (length(stats)) paste0(toupper(substr(paste(stats, collapse = point_virgule), 1, 1)), substring(paste(stats, collapse = point_virgule), 2), ".")), collapse = " ")
  pq <- m$paquets
  cits <- lapply(pq$nom, citation_paquet, fr = fr)
  paquets <- vapply(seq_along(pq$nom), function(k) sprintf("%s %s%s", pq$nom[k], pq$version[k] %||% "", if (!is.null(cits[[k]])) sprintf(" (%s)", cits[[k]]$court) else ""), "")
  annee_r <- R.version$year
  annee_nuee <- format(Sys.Date(), "%Y")
  p2 <- sprintf(tx("Les calculs ont été faits avec R %s (R Core Team, %s)%s ; Nuée%s (Boutin, %s) a donné au greffon ses données et en a tiré les résultats montrés ; le code du greffon (licence %s, empreinte SHA-256 %s…) est donné en entier par Exporter vers R.",
                   "Computations were done with R %s (R Core Team, %s)%s; Nuée%s (Boutin, %s) gave the plugin its data and took from it the results shown; the plugin's code (licence %s, SHA-256 fingerprint %s…) is given in full by Export to R."),
                m$R, annee_r, if (length(paquets)) sprintf(tx(" et %s %s", " and the %s %s"), if (length(paquets) > 1) tx("les paquets", "packages") else tx("le paquet", "package"), liste_mots(paquets, fr)) else "",
                if (nzchar(version)) paste0(" ", version) else "", annee_nuee, g$licence %||% "?", substr(g$empreinte, 1, 12))
  references <- c(sprintf(REFERENCES$r, annee_r), sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
                  vapply(citations, `[[`, "", "texte"), unlist(lapply(cits, function(x) if (!is.null(x) && nzchar(x$texte)) x$texte)))
  list(paragraphes = c(p1, p2), references = unname(sort(unique(references))))
}

tolower_premiere <- function(x) if (nzchar(x) && !grepl("^[[:upper:]]{2}", x)) paste0(tolower(substr(x, 1, 1)), substring(x, 2)) else x

# ---------------------------------------------------------------- Exporter vers R

# Une valeur d'entrée ou d'option écrite en R.
code_valeur_r <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.character(x)) return(if (length(x) == 1) code_texte(x) else if (!length(x)) "character(0)" else sprintf("c(%s)", paste(vapply(x, code_texte, ""), collapse = ", ")))
  if (is.logical(x)) return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.integer(x)) return(sprintf("%dL", x))
  deparse(x, control = "digits17")
}

# L'export vers R d'un modèle de greffon : le script refait l'appel de Nuée, avec le code du greffon
# tel que le projet le garde. 1 : les fichiers du greffon (ses fonctions et ses méthodes) ; 2 : les
# données que sa fonction voit (comme pour un modèle de code R) ; 3 : ses entrées et ses options, puis
# l'appel ; 4 : ce que Nuée en montre. Exécuté ici avant d'être écrit (il doit être permis), ses
# résultats comparés à ceux du modèle (code_script.R : ecart_code).
ecrire_script_plugin <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  g <- greffon_definition(d)
  exiger_greffon_permis(g, d$name)
  fr <- !identical(langue, "en")
  df <- donnees_code(projet, d, m$recues)
  decrites <- Filter(function(v) as.character(v$ids[1] %||% "") %in% names(df) && (nzchar(v$unite %||% "") || nzchar(v$description %||% "")), projet$variables)
  tab <- list(donnees = df, descriptions = lapply(decrites, function(v) list(nom = as.character(v$ids[1]), unite = as.character(v$unite %||% ""), description = as.character(v$description %||% ""))))
  res <- tx("resultat", "result")
  mod <- tx("modele", "model")
  pq <- m$paquets
  gm <- g$manifeste
  nom <- texte_langue(gm$nom, fr)
  auteurs <- liste_mots(vapply(gm$auteurs, `[[`, "", "nom"), fr)
  fichiers <- Filter(function(f) !identical(f$chemin, "plugin.json"), g$fichiers)
  fonctions <- c(commentaire(sprintf(tx("Le greffon « %s » %s (%s ; licence %s ; empreinte SHA-256 %s), son code tel que le projet le garde : %s.%s",
                                        "The plugin “%s” %s (%s; licence %s; SHA-256 fingerprint %s), its code as the project keeps it: %s.%s"),
                                     nom, g$version, auteurs, gm$licence, g$empreinte, paste(vapply(fichiers, `[[`, "", "chemin"), collapse = ", "),
                                     if (nrow(pq)) sprintf(tx(" Nuée l'a ajusté avec R %s et %s.", " Nuée fitted it with R %s and %s."), m$R, paste(sprintf("%s %s", pq$nom, pq$version), collapse = ", "))
                                     else sprintf(tx(" Nuée l'a ajusté avec R %s seul.", " Nuée fitted it with R %s alone."), m$R))),
                 unlist(lapply(fichiers, function(f) c("", sprintf("# ---- %s", f$chemin), strsplit(sub("\n$", "", f$texte), "\n", fixed = TRUE)[[1]]))), "")
  entrees <- vapply(names(m$entrees), function(k) sprintf("%s = %s", k, code_valeur_r(m$entrees[[k]])), "")
  options <- vapply(names(m$options_greffon), function(k) sprintf("%s = %s", k, code_valeur_r(m$options_greffon[[k]])), "")
  extraire <- if (isTRUE(m$liste)) c(sprintf("%s <- %s$modele", mod, res), sprintf("%s <- %s$valeurs", tx("valeurs", "values"), res), sprintf("%s <- %s$tableaux", tx("tableaux", "tables"), res))
              else sprintf("%s <- %s", mod, res)
  calcul <- c(commentaire(tx(sprintf("La fonction du greffon appelée comme Nuée l'appelle : %s(donnees, entrees, options), donnees étant les données du modèle (%s), entrees les variables choisies, options ses réglages.",
                                     gm$fonction, observations_texte(m$n_donnees, d, TRUE)),
                             sprintf("The plugin's function called as Nuée calls it: %s(donnees, entrees, options), donnees being the model's data (%s), entrees the variables chosen, options its settings.",
                                     gm$fonction, observations_texte(m$n_donnees, d, FALSE)))),
              if (identical(langue, "en")) sprintf("donnees <- %s", nom_script("donnees", langue)),
              sprintf("entrees <- list(%s)", paste(entrees, collapse = ", ")),
              sprintf("options <- list(%s)", paste(options, collapse = ", ")),
              sprintf("%s <- %s(donnees, entrees, options)", res, gm$fonction), extraire)
  piece <- list(
    quoi = sprintf(tx("le greffon « %s » %s", "the plugin “%s” %s"), nom, g$version),
    tab = tab, fonctions = fonctions, calcul = calcul, sortie = code_sortie_code(m, langue), graphiques = code_graphiques_code(m, langue),
    ecart = function(m, e) { enregistrer_methodes_greffon(e); ecart_code(m, e, langue) }, paquets = pq$nom,
    texte_paquets = sprintf(tx("Il refait ce modèle avec le greffon « %s » %s, son code tel que le projet le garde, sur les mêmes données, puis montre ce que Nuée en montre. Ses quatre parties :",
                               "It redoes this model with the plugin “%s” %s, its code as the project keeps it, on the same data, then shows what Nuée shows of it. Its four parts:"), nom, g$version),
    parties = c(tx("#   1. Fonctions : le code du greffon", "#   1. Functions: the plugin's code"),
                tx("#   3. Modèle : ses entrées, ses options, l'appel de sa fonction", "#   3. Model: its inputs, its options, the call of its function")),
    resultats = Filter(Negate(is.null), list(
      if (identical(attr(m$coefficients, "source"), "summary")) list(nom = "coefficients-table", objet = sprintf("coef(summary(%s))", mod), chiffres = 4),
      if (length(m$valeurs)) list(nom = "valeurs-table", objet = sprintf("t(%s)", tx("valeurs", "values")), chiffres = 4))),
    objets = c(res, mod, if (isTRUE(m$liste)) c(tx("valeurs", "values"), tx("tableaux", "tables"))))
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  on.exit(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev), add = TRUE)
  r <- NULL
  utils::capture.output(r <- suppressMessages(ecrire_script_moteur(projet, d, chemin_script, m, piece, donnees, renv, rdata, rproj, version, langue)))
  # (les méthodes du modèle de nouveau celles du greffon lu par le moteur)
  environnement_greffon(g)
  r
}
