# Le paquet de reproductibilité d'un projet (Fichier › Exporter un paquet de reproductibilité… :
# interface/paquet.js) : une archive .zip que quiconque a R, ou Nuée, peut relancer. Dans un dossier
# au nom de l'archive :
#   <projet>.nuee                    le projet (ses données, ses modèles, ses documents), comme
#                                    Enregistrer l'écrit, ses données venant du CSV (source_paquet) ;
#   donnees/<projet>.csv             (data/ en anglais) ses données, et leur dictionnaire : chaque
#   donnees/<projet> - dictionnaire.csv   colonne, son type, son unité, sa description, ses catégories ;
#   analyse.R                        (analysis.R) un seul script R qui refait chaque modèle, fait des
#                                    exports vers R de script.R et des *_script.R (script_paquet) ;
#   documents/                       les documents du projet en fichiers Quarto reproductibles
#                                    (document_reproductible.R ; le .qmd vient de l'interface) ;
#   README.md                        ce qu'il contient, comment le relancer avec R (les paquets et leurs
#                                    versions) et l'ouvrir dans Nuée, dans la langue de l'interface ;
#   LICENSE                          la licence des données choisie, et son texte (moteur/licences) ;
#   CITATION.cff, codemeta.json      titre, description, auteurs (ORCID), mots-clés, licence, la
#                                    version et le DOI de Nuée, les versions de R et des paquets ;
#   MANIFEST.sha256                  l'empreinte SHA-256 de chaque fichier (sha256sum -c la vérifie).
# Les réponses du dialogue (titre, auteurs, description, mots-clés, licence) sont gardées dans le
# projet (p$paquet) : l'export suivant repart d'elles.

# Le DOI de Nuée, celui de toutes ses versions (Zenodo), cité par le paquet.
DOI_NUEE <- "10.5281/zenodo.24263014"

# Les licences proposées pour les données, par leur identifiant SPDX. Leur texte est dans
# moteur/licences, octet pour octet tel que creativecommons.org le donne en texte : le legalcode.txt
# de la CC BY 4.0, de la CC BY-SA 4.0 et de la CC BY-NC 4.0 (https://creativecommons.org/licenses/
# <by, by-sa, by-nc>/4.0/legalcode.txt), et celui de la CC0 1.0, que R livre aussi
# (share/licenses/CC0-1.0). verifier_paquet.R le vérifie par leurs empreintes.
LICENCES_PAQUET <- list(
  "CC-BY-4.0" = list(nom = "Creative Commons Attribution 4.0 International", court = "CC BY 4.0",
                     url = "https://creativecommons.org/licenses/by/4.0/"),
  "CC0-1.0" = list(nom = "Creative Commons Zero v1.0 Universal", court = "CC0 1.0",
                   url = "https://creativecommons.org/publicdomain/zero/1.0/"),
  "CC-BY-SA-4.0" = list(nom = "Creative Commons Attribution-ShareAlike 4.0 International", court = "CC BY-SA 4.0",
                        url = "https://creativecommons.org/licenses/by-sa/4.0/"),
  "CC-BY-NC-4.0" = list(nom = "Creative Commons Attribution-NonCommercial 4.0 International", court = "CC BY-NC 4.0",
                        url = "https://creativecommons.org/licenses/by-nc/4.0/"))

# ---------------------------------------------------------------- ce que le dialogue donne

# Un ORCID écrit 0000-0002-1825-0097 (ou son adresse https://orcid.org/…, ou sans tirets), vérifié
# par son dernier chiffre (ISO 7064 11,2) ; "" quand il n'y en a pas. Un autre texte est refusé.
orcid_paquet <- function(x) {
  x <- trimws(as.character(x %||% "")[1])
  if (is.na(x) || !nzchar(x)) return("")
  chiffres <- toupper(gsub("[-\\s]", "", sub("^(https?://)?(www\\.)?orcid\\.org/", "", x, ignore.case = TRUE), perl = TRUE))
  refus <- function() stop(sprintf(traduire("Ce n'est pas un ORCID : %s (16 chiffres, 0000-0002-1825-0097).", "This isn't an ORCID: %s (16 digits, 0000-0002-1825-0097)."), x), call. = FALSE)
  if (!grepl("^[0-9]{15}[0-9X]$", chiffres)) refus()
  total <- 0
  for (k in 1:15) total <- (total + as.integer(substr(chiffres, k, k))) * 2
  cle <- (12 - total %% 11) %% 11
  if (!identical(substr(chiffres, 16, 16), if (cle == 10) "X" else as.character(cle))) refus()
  paste(substring(chiffres, c(1, 5, 9, 13), c(4, 8, 12, 16)), collapse = "-")
}

# Les réponses du dialogue, nettoyées : le titre, les auteurs (prénoms, nom, ORCID facultatif), la
# description, les mots-clés (une liste, ou un texte séparé par des virgules) et la licence des
# données (LICENCES_PAQUET, la CC BY 4.0 sans réponse). Sans retour à la ligne hors de la
# description. Refusé, en le disant : un titre vide, aucun auteur, un auteur sans nom, un ORCID faux,
# une licence inconnue.
metadonnees_paquet <- function(x) {
  x <- x %||% list()
  ligne <- function(v, max) {
    v <- as.character(unlist(v %||% ""))[1]
    if (is.na(v)) return("")
    substr(trimws(gsub("[[:cntrl:]]+", " ", v)), 1, max)
  }
  titre <- ligne(x$titre, 300)
  if (!nzchar(titre)) stop(traduire("Le paquet n'a pas de titre.", "The package has no title."), call. = FALSE)
  auteurs <- x$auteurs
  if (is.data.frame(auteurs)) auteurs <- lapply(seq_len(nrow(auteurs)), function(i) as.list(auteurs[i, , drop = FALSE]))
  auteurs <- lapply(auteurs %||% list(), function(a) list(prenom = ligne(a$prenom, 200), nom = ligne(a$nom, 200), orcid = orcid_paquet(a$orcid)))
  auteurs <- Filter(function(a) nzchar(a$prenom) || nzchar(a$nom) || nzchar(a$orcid), auteurs)
  if (!length(auteurs)) stop(traduire("Le paquet n'a pas d'auteur.", "The package has no author."), call. = FALSE)
  sans_nom <- Filter(function(a) !nzchar(a$nom), auteurs)
  if (length(sans_nom)) {
    stop(sprintf(traduire("Un auteur sans nom de famille : %s.", "An author without a family name: %s."), sans_nom[[1]]$prenom %||% sans_nom[[1]]$orcid), call. = FALSE)
  }
  description <- as.character(unlist(x$description %||% ""))[1]
  description <- if (is.na(description)) "" else substr(trimws(gsub("[\001-\010\013-\037\177]+", " ", gsub("\r\n?", "\n", description))), 1, 20000)
  mots <- trimws(gsub("[[:cntrl:]]+", " ", unlist(strsplit(as.character(unlist(x$mots_cles %||% character(0))), "[,;\n]"))))
  mots <- unique(substr(mots[!is.na(mots) & nzchar(mots)], 1, 100))
  licence <- as.character(unlist(x$licence %||% "CC-BY-4.0"))[1]
  if (!isTRUE(licence %in% names(LICENCES_PAQUET))) stop(sprintf(traduire("Licence inconnue : %s.", "Unknown licence: %s."), licence), call. = FALSE)
  list(titre = titre, auteurs = auteurs, description = description, mots_cles = mots, licence = licence)
}

# Le DOI du paquet lui-même, celui que Zenodo a réservé pour son brouillon (Déposer sur Zenodo… :
# zenodo.js, avant d'écrire le paquet), écrit 10.5281/zenodo.123 (ou son adresse https://doi.org/…, ou
# doi:…) ; NULL quand il n'y en a pas (Exporter, sans dépôt). Un autre texte est refusé.
doi_paquet <- function(x) {
  x <- trimws(as.character(unlist(x %||% ""))[1])
  if (is.na(x) || !nzchar(x)) return(NULL)
  doi <- sub("^(https?://(dx\\.)?doi\\.org/|doi:)", "", x, ignore.case = TRUE)
  if (!grepl("^10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+$", doi)) stop(sprintf(traduire("Ce n'est pas un DOI : %s.", "This isn't a DOI: %s."), x), call. = FALSE)
  doi
}

# Les réponses gardées dans un projet (p$paquet, relu d'un fichier), nettoyées comme le dialogue les
# donne, ou NULL quand il n'en a pas (ou plus de valables).
metadonnees_gardees <- function(x) if (is.null(x)) NULL else tryCatch(metadonnees_paquet(x), error = function(e) NULL)

# Ce que l'interface en reçoit (resume_projet) : des listes même d'un seul élément.
paquet_pour_interface <- function(x) {
  m <- metadonnees_gardees(x)
  if (is.null(m)) return(NULL)
  m$mots_cles <- I(m$mots_cles)
  m
}

# Un auteur pour un texte : « Prénom Nom », et en liste : « A, B et C ».
nom_auteur <- function(a) trimws(paste(a$prenom, a$nom))
liste_auteurs <- function(meta, langue) liste_textes(vapply(meta$auteurs, nom_auteur, ""), if (identical(langue, "en")) "and" else "et")

# ---------------------------------------------------------------- écrire

# Un fichier texte en UTF-8, des fins de ligne « \n » sur tout système (ses empreintes sont les mêmes).
ecrire_texte_paquet <- function(chemin, lignes) {
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(lignes, "\n", collapse = ""))), chemin)
}

lire_lignes_paquet <- function(chemin) {
  x <- readLines(chemin, encoding = "UTF-8", warn = FALSE)
  sub("\r$", "", x)
}

# Un tableau en CSV comme Fichier › Exporter les données l'écrit (utils::write.csv : virgules,
# guillemets autour des textes, une case vide pour une valeur manquante, 15 chiffres significatifs).
lignes_csv <- function(df) {
  lignes <- character(0)
  con <- textConnection("lignes", "w", local = TRUE)
  utils::write.csv(df, con, row.names = FALSE, na = "")
  close(con)
  enc2utf8(lignes)
}

# Un chemin d'un fichier dans le paquet, venu de l'interface : relatif, en « / », sans « .. » ni
# caractère de contrôle ; refusé sinon (il ne peut rien écrire hors du paquet).
chemin_relatif_paquet <- function(x) {
  x <- gsub("\\\\", "/", as.character(x %||% "")[1])
  parties <- strsplit(x, "/", fixed = TRUE)[[1]]
  if (is.na(x) || !nzchar(x) || grepl("^/|^[A-Za-z]:", x) || any(parties %in% c("", ".", "..")) || grepl("[[:cntrl:]]", x)) {
    stop(traduire("Chemin refusé dans le paquet : ", "Path refused in the package: "), x, call. = FALSE)
  }
  x
}

# Les empreintes SHA-256 de fichiers, quel que soit le R, comme le moteur les calcule : celle
# d'empreinte_sha (etapes.R : tools::sha256sum, de R 4.5) ; un R plus ancien (celui d'une distribution
# Linux : R 4.2 de Debian 12, R 4.1 d'Ubuntu 22.04), où empreinte_sha prend un MD5, la calcule en R
# seul, comme l'empreinte d'un code (code.R : sha256_r ; 30 Ko par seconde environ).
empreintes_sha256 <- function(fichiers) {
  vapply(fichiers, function(f) {
    h <- empreinte_sha(f)
    if (startsWith(h, "md5:")) sha256_r(readBin(f, "raw", file.size(f))) else h
  }, "", USE.NAMES = FALSE)
}

# Le dictionnaire des données (tableau_projet, formats.R) : une ligne par colonne, son type
# (identifiant, quantitative, qualitative), son unité et sa description (Édition › Unités et
# descriptions : atelier.R), ses catégories dans leur ordre ; dans la langue du paquet.
dictionnaire_donnees <- function(projet, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  tab <- tableau_projet(projet)
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(projet$donnees), projet$variables)
  n_id <- ncol(tab) - length(vars)
  type <- c(rep(tx("identifiant", "identifier"), n_id), vapply(vars, function(v) if (length(v$categories)) "qualitative" else "quantitative", ""))
  unite <- c(rep("", n_id), vapply(vars, function(v) as.character(v$unite %||% ""), ""))
  description <- c(tx("Nom de l'observation", "Name of the observation"), rep(tx("Identifiant des observations", "Identifier of the observations"), n_id - 1),
                   vapply(vars, function(v) as.character(v$description %||% ""), ""))
  categories <- c(rep("", n_id), vapply(vars, function(v) paste(as.character(v$categories %||% character(0)), collapse = "; "), ""))
  df <- data.frame(names(tab), type, unite, description, categories, stringsAsFactors = FALSE)
  names(df) <- c("variable", "type", tx("unité", "unit"), "description", tx("catégories", "categories"))
  df
}

# Les paquets R qu'appelle un script (library(x), x::f), sans ceux de R lui-même (stats, graphics…) :
# leur version, et s'ils viennent avec R (recommandés : nlme, mgcv, MASS…) ou s'installent à part.
paquets_appeles <- function(lignes) {
  code <- lignes[!grepl("^\\s*#", lignes)]
  appels <- c(unlist(regmatches(code, gregexpr("(?<![A-Za-z0-9._])[A-Za-z][A-Za-z0-9.]*(?=::)", code, perl = TRUE))),
              sub("^library\\(\"?([^\")]+)\"?\\)$", "\\1", unlist(regmatches(code, gregexpr("library\\(\"?[A-Za-z][A-Za-z0-9.]*\"?\\)", code)))))
  base <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")
  pq <- sort(unique(setdiff(appels, base)))
  pq <- pq[vapply(pq, function(x) nzchar(system.file(package = x)), TRUE)]
  data.frame(paquet = pq, version = vapply(pq, version_paquet, "", USE.NAMES = FALSE),
             avec_r = vapply(pq, function(x) isTRUE(utils::packageDescription(x, fields = "Priority") %in% c("base", "recommended")), TRUE, USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

# Le type d'un modèle, en court (le nom de son analyse dans l'explorateur : ACP, PLS-DA, GLM, RDA…).
type_paquet <- function(d, langue) {
  if (est_regression(d) || est_autre_analyse(d)) return(as.character(d$type))
  type_script(d, langue)
}

# Le nom R du résultat d'une autre analyse dans le script (M5…) : pris à son nom (script.R : noms_r),
# ni celui d'une fonction de R, ni un nom déjà pris (`pris`).
objet_paquet <- function(nom, pris, langue) {
  x <- noms_r(nom)
  while (x %in% pris || exists(x, envir = baseenv()) || x %in% c("T", "F")) x <- paste0(x, if (identical(langue, "en")) "_model" else "_modele")
  x
}

# L'avis de licence qu'un export vers R porte (script.R : avec_avis_licence), retiré de ses lignes :
# le script du paquet le porte une fois, en tête, et non dans chacune de ses parties.
sans_avis_licence <- function(lignes, langue, version) {
  avis <- avis_licence_script(langue, version)
  n <- length(avis)
  for (k in seq_len(max(0, length(lignes) - n + 1))) if (identical(lignes[k:(k + n - 1)], avis)) return(lignes[-(k:(k + n - 1))])
  lignes
}

# Le script R du paquet : un seul fichier qui refait chaque modèle avec R. Les modèles de composantes
# (ACP, PLS, OPLS) et les régressions forment la séance de l'export vers R (script.R :
# ecrire_script_seance ; leurs données et leurs fonctions une fois, chacun gardé dans une liste à son
# nom) ; chaque autre analyse suit avec son propre export vers R (ecrire_script), dans un
# environnement à elle (local()), qui est gardé sous son nom. Les données sont dans le script : il ne
# lit aucun fichier. Chaque export est vérifié comme Fichier › Exporter vers R le vérifie (son `ecart`).
# Son en-tête est celui d'un export vers R : ce qu'il refait, puis l'avis de licence (l'exception à la
# GPL pour ce que Nuée écrit, et comment citer Nuée avec son DOI : avis_licence_script), et que ses
# données, elles, sont sous la licence des données du paquet.
#   modeles   une liste de list(id, definition, modele, erreur, projet), erreur : pourquoi il n'est pas
#             refait ; projet : celui sur lequel écrire l'export d'une autre analyse (le projet tel
#             qu'elle l'a vu, quand l'ordre de ses catégories a changé depuis : serveur.R, paquet_demande).
# Rend, pour chaque modèle, son nom R (objet) ou pourquoi il n'y est pas (erreur) ; les écarts ; les
# paquets R qu'il appelle (paquets_appeles) ; NULL comme script quand aucun modèle n'y est.
script_paquet <- function(projet, modeles, chemin, meta, langue, version) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  tmp <- tempfile("script_paquet")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  infos <- lapply(modeles, function(x) list(id = x$id, nom = as.character(x$definition$name %||% ""), titre = as.character(x$definition$title %||% ""),
                                            type = type_paquet(x$definition, langue), erreur = x$erreur))
  ecarts <- character(0)
  principaux <- which(vapply(modeles, function(x) is.null(x$erreur) && !est_autre_analyse(x$definition), TRUE))
  seance <- character(0)
  ecrire_seance <- function(k) tryCatch(ecrire_script_seance(projet, lapply(modeles[k], `[[`, "definition"), lapply(modeles[k], `[[`, "modele"),
                                                             file.path(tmp, "seance.R"), "moteur", version = version, langue = langue), error = function(e) e)
  if (length(principaux)) {
    r <- ecrire_seance(principaux)
    if (inherits(r, "error")) {
      # (un modèle dont le script ne s'écrit pas : nommé avec son erreur, les autres sans lui)
      for (k in principaux) {
        seul <- ecrire_seance(k)
        if (inherits(seul, "error")) infos[[k]]$erreur <- conditionMessage(seul)
      }
      principaux <- principaux[vapply(infos[principaux], function(i) is.null(i$erreur), TRUE)]
      r <- if (length(principaux)) ecrire_seance(principaux)
    }
    if (inherits(r, "error")) {
      for (k in principaux) infos[[k]]$erreur <- conditionMessage(r)
    } else if (!is.null(r)) {
      seance <- sans_avis_licence(lire_lignes_paquet(r$script), langue, version)
      for (j in seq_along(principaux)) infos[[principaux[j]]]$objet <- r$noms[j]
      if (!is.null(r$ecart)) ecarts <- c(ecarts, r$ecart)
    }
  }
  pris <- c(unlist(lapply(infos, `[[`, "objet")), if (length(seance)) symboles_code(seance), nom_script("donnees", langue))
  blocs <- list()
  for (k in which(vapply(modeles, function(x) is.null(x$erreur) && est_autre_analyse(x$definition), TRUE))) {
    x <- modeles[[k]]
    f <- file.path(tmp, sprintf("analyse%d.R", k))
    r <- tryCatch(ecrire_script(x$projet %||% projet, x$definition, f, "moteur", m = x$modele, version = version, langue = langue), error = function(e) e)
    if (inherits(r, "error")) {
      infos[[k]]$erreur <- conditionMessage(r)
      next
    }
    objet <- objet_paquet(infos[[k]]$nom, pris, langue)
    pris <- c(pris, objet)
    infos[[k]]$objet <- objet
    if (!is.null(r$ecart)) ecarts <- c(ecarts, sprintf(tx("%s : %s", "%s: %s"), objet, r$ecart))
    blocs[[length(blocs) + 1]] <- list(objet = objet, info = infos[[k]], lignes = sans_avis_licence(lire_lignes_paquet(f), langue, version))
  }
  faits <- Filter(function(i) !is.null(i$objet), infos)
  if (!length(faits)) return(list(script = NULL, modeles = infos, ecarts = ecarts, paquets = paquets_appeles(character(0))))
  description <- function(i) sprintf("%s%s%s %s", i$objet, if (nzchar(i$titre)) sprintf(" (%s)", texte_sur(i$titre)) else "",
                                     if (identical(langue, "en")) ":" else " :", i$type)
  sans <- Filter(function(i) is.null(i$objet), infos)
  partie <- 0
  titre_partie_paquet <- function(texte) {
    partie <<- partie + 1
    c("", "", sprintf("# ==== %d. %s ====", partie, texte_sur(texte)), "")
  }
  corps <- c(if (length(seance)) {
    regressions <- vapply(modeles[principaux], function(x) est_regression(x$definition), TRUE)
    quoi <- if (all(regressions)) tx("régressions", "regressions") else if (any(regressions)) tx("modèles de composantes et régressions", "models of components and regressions")
            else tx("modèles de composantes", "models of components")
    c(titre_partie_paquet(sprintf(tx("%s : %s, un script pour eux, leurs données une fois", "%s: %s, one script for them, their data once"),
                                  paste(vapply(infos[principaux], `[[`, "", "objet"), collapse = ", "), quoi)), seance)
  }, unlist(lapply(blocs, function(b) c(
    titre_partie_paquet(sprintf(tx("%s, dans un environnement à lui", "%s, in an environment of its own"), description(b$info))),
    commentaire(sprintf(tx("Son export vers R, tel quel, lancé dans local() : ses objets restent dans %s (%s$...), sans toucher à ceux des autres modèles.",
                           "Its export to R, as it is, run in local(): its objects stay in %s (%s$...), without touching those of the other models."), b$objet, b$objet)),
    sprintf("%s <- local({", b$objet), b$lignes, "environment()", "})"))))
  paquets <- paquets_appeles(corps)
  entete <- c(
    commentaire(sprintf(tx("Paquet de reproductibilité « %s » : chaque modèle du projet « %s », refait avec R à partir de ses données, dans un seul script. Écrit par Nuée %s le %s.",
                           "Reproducibility package “%s”: every model of the project “%s”, redone with R from its data, in one script. Written by Nuée %s on %s."),
                        texte_sur(meta$titre), texte_sur(tools::file_path_sans_ext(as.character(projet$nom %||% projet$source %||% ""))), version, format(Sys.Date()))),
    commentaire(tx("Ses données sont dans le script : il ne lit aucun fichier. Le lancer avec Rscript analyse.R (ses graphiques vont alors dans Rplots.pdf), ou dans R ou RStudio (source). Chaque modèle garde ses résultats sous son nom :",
                   "Its data are in the script: it reads no file. Run it with Rscript analysis.R (its plots then go to Rplots.pdf), or in R or RStudio (source). Each model keeps its results under its name:")),
    paste0("#   ", vapply(faits, description, "")),
    if (length(sans)) c(commentaire(tx("Pas dans ce script :", "Not in this script:")),
                        unlist(lapply(sans, function(i) sub("^# ", "#   ", commentaire(sprintf(tx("%s : %s", "%s: %s"), i$nom, i$erreur)))))),
    commentaire(sprintf(tx("Écrit avec %s.", "Written with %s."), texte_paquets(paquets$paquet, langue, versions = TRUE))),
    commentaire(tx("Dans RStudio, Ctrl+Maj+O affiche ses parties.", "In RStudio, Ctrl+Shift+O shows its parts.")))
  l <- LICENCES_PAQUET[[meta$licence]]
  donnees <- commentaire(sprintf(tx("Ses données sont celles du paquet, sous sa licence des données : %s (%s, LICENSE). L'avis ci-dessus porte sur le code.",
                                    "Its data are the package's, under its licence of the data: %s (%s, LICENSE). The notice above is about the code."), l$court, l$url))
  ecrire_texte_paquet(chemin, c(entete, avis_licence_script(langue, version), donnees, corps))
  list(script = chemin, modeles = infos, ecarts = ecarts, paquets = paquets)
}

# Les documents du paquet, dans `dossier` (documents/) : pour chacun, ce que l'interface en a fait
# (documents.js : documentPourPaquet) : son .qmd reproductible, ses références (.bib) et son style
# CSL, les figures que R ne redessine pas (en PNG, base64), et les fichiers que le moteur a écrits
# pour lui (document_reproductible : les scripts des modèles qu'il cite et leurs données, dans un
# dossier de la session, `fichiers`, à leur place `relatifs`). Rend les chemins écrits (relatifs à
# `dossier`) et ce qu'il faut savoir de chacun.
ecrire_documents_paquet <- function(documents, dossier) {
  ecrits <- character(0)
  notes <- list()
  session <- normalizePath(tempdir(), winslash = "/")
  pris <- character(0)
  for (d in documents) {
    # (son nom tel que l'interface l'a donné au .qmd et à ses dossiers <nom>_R et <nom>_figures, qu'il cite)
    nom <- as.character(d$nom %||% "")[1]
    if (is.na(nom) || !nzchar(trimws(nom)) || grepl("[/\\\\]", nom)) stop(traduire("Nom de document refusé dans le paquet : ", "Document name refused in the package: "), nom, call. = FALSE)
    if (tolower(nom) %in% tolower(pris)) stop(traduire("Deux documents du même nom : ", "Two documents with the same name: "), nom, call. = FALSE)
    pris <- c(pris, nom)
    garder <- function(rel, ecrire) {
      rel <- chemin_relatif_paquet(rel)
      f <- file.path(dossier, rel)
      dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
      ecrire(f)
      ecrits <<- c(ecrits, rel)
    }
    texte <- function(x) function(f) writeBin(charToRaw(enc2utf8(gsub("\r\n?", "\n", as.character(x)[1]))), f)
    garder(paste0(nom, ".qmd"), texte(d$qmd %||% ""))
    if (length(d$bib) && nzchar(as.character(d$bib)[1])) garder(paste0(nom, ".bib"), texte(d$bib))
    if (length(d$csl$nom) && nzchar(as.character(d$csl$nom)[1])) garder(basename(gsub("\\\\", "/", d$csl$nom)), texte(d$csl$texte %||% ""))
    for (g in d$figures %||% list()) {
      garder(g$chemin, function(f) writeBin(jsonlite::base64_dec(sub("^data:[^,]*,", "", as.character(g$png %||% ""))), f))
    }
    sources <- as.character(unlist(d$fichiers))
    relatifs <- as.character(unlist(d$relatifs))
    if (length(sources) != length(relatifs)) stop(traduire("Les fichiers d'un document ne correspondent pas à leurs chemins.", "A document's files don't match their paths."), call. = FALSE)
    for (k in seq_along(sources)) {
      s <- normalizePath(sources[k], winslash = "/", mustWork = FALSE)
      # (seulement ce que le moteur a écrit dans la session : document_reproductible)
      if (!startsWith(s, paste0(session, "/")) || !file.exists(s)) stop(traduire("Fichier refusé dans le paquet : ", "File refused in the package: "), sources[k], call. = FALSE)
      garder(relatifs[k], function(f) file.copy(s, f, overwrite = TRUE))
    }
    notes[[length(notes) + 1]] <- list(nom = nom, titre = as.character(d$titre %||% nom)[1], note = as.character(d$note %||% "")[1])
  }
  list(fichiers = ecrits, documents = notes)
}

# LICENSE : qui donne les données sous quelle licence, puis son texte (moteur/licences, fins de
# ligne « \n », sans la ligne vide qui le finit). En anglais, comme le texte des licences.
lignes_licence <- function(meta) {
  l <- LICENCES_PAQUET[[meta$licence]]
  texte <- lire_lignes_paquet(file.path(getOption("nuee.moteur", "moteur"), "licences", paste0(meta$licence, ".txt")))
  while (length(texte) && !nzchar(trimws(texte[length(texte)]))) texte <- texte[-length(texte)]
  qui <- liste_auteurs(meta, "en")
  c(texte_sur(meta$titre), "",
    if (identical(meta$licence, "CC0-1.0")) sprintf("The data of this package, by %s, are dedicated to the public domain under the %s (%s).", qui, l$nom, l$url)
    else c(sprintf("Copyright (c) %s %s.", format(Sys.Date(), "%Y"), qui), "",
           sprintf("The data of this package are licensed under the %s licence (%s, %s).", l$nom, l$court, l$url)),
    "The legal code of the licence follows.", "", strrep("=", 71), "", texte)
}

# Une chaîne YAML entre guillemets doubles (CITATION.cff) : ses barres obliques inverses, guillemets
# et sauts de ligne échappés.
yaml_texte <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", enc2utf8(as.character(x)))
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\t", "\\\\t", gsub("\n", "\\\\n", x))
  paste0("\"", gsub("[[:cntrl:]]", " ", x), "\"")
}

# Les auteurs d'un paquet R pour CITATION.cff, d'après la citation que R en donne (utils::citation :
# ceux de son article ou de son manuel) : prénoms et nom, ou le nom d'une équipe (entite).
auteurs_paquet_r <- function(paquet) {
  ps <- tryCatch(utils::citation(paquet)[[1]]$author, error = function(e) NULL)
  if (!length(ps)) return(list(list(entite = paquet)))
  lapply(ps, function(x) {
    prenom <- paste(x$given %||% character(0), collapse = " ")
    nom <- paste(x$family %||% character(0), collapse = " ")
    if (nzchar(nom)) list(prenom = prenom, nom = nom) else list(entite = prenom)
  })
}

# CITATION.cff (Citation File Format 1.2.0) : le paquet comme jeu de données (titre, son DOI quand il
# en a un, description, auteurs et leur ORCID, mots-clés, licence), puis ce qui l'a fait : Nuée (sa
# version, son DOI), R et les paquets R qu'appelle son script, avec leurs versions.
lignes_cff <- function(meta, langue, version, paquets, doi = NULL) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  y <- yaml_texte
  # (une personne : ses prénoms, son nom, son ORCID ; une équipe : son nom, `entite`)
  personne <- function(a, retrait) {
    champ <- function(k) as.character(a[[k]] %||% "")
    champs <- c(if (nzchar(champ("entite"))) sprintf("name: %s", y(champ("entite"))),
                if (nzchar(champ("prenom"))) sprintf("given-names: %s", y(champ("prenom"))),
                if (nzchar(champ("nom"))) sprintf("family-names: %s", y(champ("nom"))),
                if (nzchar(champ("orcid"))) sprintf("orcid: %s", y(paste0("https://orcid.org/", champ("orcid")))))
    c(paste0(retrait, "- ", champs[1]), if (length(champs) > 1) paste0(retrait, "  ", champs[-1]))
  }
  logiciel <- function(titre, auteurs, version, autres = character(0)) {
    c(sprintf("  - type: software"), sprintf("    title: %s", y(titre)), "    authors:", unlist(lapply(auteurs, personne, retrait = "      ")),
      sprintf("    version: %s", y(version)), paste0("    ", autres))
  }
  r_version <- paste(R.version$major, R.version$minor, sep = ".")
  c("cff-version: 1.2.0",
    sprintf("message: %s", y(tx("Pour citer ces données ou cette analyse, merci d'utiliser les métadonnées de ce fichier.", "If you use these data or this analysis, please cite them using the metadata from this file."))),
    "type: dataset",
    sprintf("title: %s", y(meta$titre)),
    if (!is.null(doi)) sprintf("doi: %s", y(doi)),
    if (nzchar(meta$description)) sprintf("abstract: %s", y(meta$description)),
    "authors:", unlist(lapply(meta$auteurs, personne, retrait = "  ")),
    if (length(meta$mots_cles)) c("keywords:", sprintf("  - %s", y(meta$mots_cles))),
    sprintf("license: %s", meta$licence),
    "references:",
    logiciel("Nuée", list(list(prenom = "Clément", nom = "Boutin")), version,
             c(sprintf("doi: %s", y(DOI_NUEE)), sprintf("url: %s", y("https://nuee.io")), sprintf("repository-code: %s", y("https://github.com/4rtefakt/nuee")))),
    logiciel("R: A Language and Environment for Statistical Computing", list(list(entite = "R Core Team")), r_version, sprintf("url: %s", y("https://www.R-project.org/"))),
    unlist(lapply(seq_len(nrow(paquets)), function(k) {
      p <- paquets$paquet[k]
      titre <- tryCatch(utils::packageDescription(p, fields = "Title"), error = function(e) NA)
      logiciel(if (!is.na(titre)) sprintf("%s: %s", p, gsub("\\s+", " ", titre)) else p, auteurs_paquet_r(p), paquets$version[k],
               sprintf("url: %s", y(sprintf("https://CRAN.R-project.org/package=%s", p))))
    })))
}

# codemeta.json (CodeMeta 3.0) : le même, pour les outils qui lisent les métadonnées de logiciels de
# recherche (le DOI du paquet en identifier) ; R et ses paquets en softwareRequirements, Nuée (qui
# ouvre le .nuee) en softwareSuggestions.
codemeta_paquet <- function(meta, langue, version, paquets, doi = NULL) {
  personne <- function(a) {
    x <- list("@type" = "Person")
    if (nzchar(a$orcid %||% "")) x[["@id"]] <- paste0("https://orcid.org/", a$orcid)
    if (nzchar(a$prenom %||% "")) x$givenName <- a$prenom
    x$familyName <- a$nom
    x
  }
  l <- LICENCES_PAQUET[[meta$licence]]
  x <- list("@context" = "https://w3id.org/codemeta/3.0", "@type" = "SoftwareSourceCode",
            name = meta$titre, identifier = if (!is.null(doi)) paste0("https://doi.org/", doi), description = if (nzchar(meta$description)) meta$description,
            author = lapply(meta$auteurs, personne), keywords = I(meta$mots_cles),
            license = paste0("https://spdx.org/licenses/", meta$licence), dateCreated = format(Sys.Date()),
            inLanguage = langue,
            programmingLanguage = list("@type" = "ComputerLanguage", name = "R", version = paste(R.version$major, R.version$minor, sep = "."), url = "https://www.r-project.org"),
            runtimePlatform = R.version.string,
            softwareRequirements = lapply(seq_len(nrow(paquets)), function(k) list("@type" = "SoftwareApplication", name = paquets$paquet[k], version = paquets$version[k],
                                                                                   url = sprintf("https://CRAN.R-project.org/package=%s", paquets$paquet[k]))),
            softwareSuggestions = list(list("@type" = "SoftwareApplication", name = "Nuée", version = version, identifier = paste0("https://doi.org/", DOI_NUEE),
                                            url = "https://nuee.io", author = list(list("@type" = "Person", givenName = "Clément", familyName = "Boutin")))))
  x <- Filter(Negate(is.null), x)
  enc2utf8(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null")))
}

# README.md, dans la langue du paquet : le titre, la description, les auteurs, son DOI quand il en a
# un, ce que contient le paquet, ses modèles, comment le relancer avec R (les paquets et leurs
# versions), l'ouvrir dans Nuée (avec son DOI, aussi dans la version de Nuée pour navigateur,
# https://nuee.io/app/?doi=…), vérifier ses fichiers et le citer.
lignes_readme <- function(meta, langue, version, contenu, doi = NULL) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  deux_points <- tx(" : ", ": ")
  l <- LICENCES_PAQUET[[meta$licence]]
  auteur <- function(a) paste0(nom_auteur(a), if (nzchar(a$orcid)) sprintf(" (ORCID [%s](https://orcid.org/%s))", a$orcid, a$orcid) else "")
  code <- function(x) paste0("`", gsub("`", "'", x), "`")
  r_version <- paste(R.version$major, R.version$minor, sep = ".")
  ct <- contenu
  avec_r <- ct$paquets[ct$paquets$avec_r, ]
  a_installer <- ct$paquets[!ct$paquets$avec_r, ]
  versions <- function(p) liste_textes(sprintf("%s %s", p$paquet, p$version), tx("et", "and"))
  faits <- Filter(function(i) !is.null(i$objet), ct$modeles)
  sans <- Filter(function(i) is.null(i$objet), ct$modeles)
  annee <- format(Sys.Date(), "%Y")
  # (APA : Nom, P., & Nom, P. (année). Titre [Jeu de données].)
  noms_cites <- vapply(meta$auteurs, function(a) paste0(a$nom, if (nzchar(a$prenom)) paste0(", ", gsub("([[:alpha:]])[[:alpha:]'’]*", "\\1.", a$prenom)) else ""), "")
  if (length(noms_cites) > 1) noms_cites[length(noms_cites)] <- paste("&", noms_cites[length(noms_cites)])
  citation <- paste0(paste(noms_cites, collapse = ", "), sprintf(" (%s). *%s* [%s]. ", annee, texte_sur(meta$titre), tx("Jeu de données", "Data set")),
                     if (!is.null(doi)) sprintf("Zenodo. https://doi.org/%s ", doi))
  dans_navigateur <- if (!is.null(doi)) paste0("https://nuee.io/app/?doi=", doi)
  c(paste("#", texte_sur(meta$titre)), "",
    if (nzchar(meta$description)) c(gsub("\r", "", meta$description), ""),
    sprintf("- **%s** %s", tx("Auteurs :", "Authors:"), paste(vapply(meta$auteurs, auteur, ""), collapse = ", ")),
    if (length(meta$mots_cles)) sprintf("- **%s** %s", tx("Mots-clés :", "Keywords:"), paste(meta$mots_cles, collapse = ", ")),
    if (!is.null(doi)) sprintf("- **DOI%s** [%s](https://doi.org/%s)", tx(" :", ":"), doi, doi),
    sprintf("- **%s** [%s](%s), %s", tx("Licence des données :", "Licence of the data:"), l$court, l$url, tx("son texte dans `LICENSE`", "its text in `LICENSE`")),
    sprintf("- **%s** %s", tx("Fait avec :", "Made with:"),
            sprintf(tx("Nuée %s ([doi:%s](https://doi.org/%s)) et R %s, le %s", "Nuée %s ([doi:%s](https://doi.org/%s)) and R %s, on %s"), version, DOI_NUEE, DOI_NUEE, r_version, format(Sys.Date()))),
    "",
    paste("##", tx("Ce que contient ce paquet", "What this package holds")), "",
    sprintf("| %s | %s |", tx("Fichier", "File"), tx("Ce que c'est", "What it is")), "|---|---|",
    sprintf("| %s | %s |", code(ct$nuee), tx("Le projet Nuée : ses données, ses modèles et ses documents", "The Nuée project: its data, its models and its documents")),
    sprintf("| %s | %s |", code(ct$csv), sprintf(tx("Les données : %d observations, %d colonnes (CSV en UTF-8, séparé par des virgules ; une case vide pour une valeur manquante)",
                                                    "The data: %d observations, %d columns (CSV in UTF-8, comma-separated; an empty cell for a missing value)"), ct$n_obs, ct$n_colonnes)),
    sprintf("| %s | %s |", code(ct$dictionnaire), tx("Chaque colonne des données : son type, son unité, sa description et ses catégories", "Each column of the data: its type, unit, description and categories")),
    if (!is.null(ct$script)) sprintf("| %s | %s |", code(ct$script), tx("Un seul script R qui refait chaque modèle à partir des données", "One R script that redoes every model from the data")),
    if (length(ct$documents)) sprintf("| %s | %s |", code("documents/"), tx("Les documents du projet, en fichiers Quarto reproductibles", "The project's documents, as reproducible Quarto files")),
    sprintf("| %s | %s |", code("LICENSE"), tx("La licence des données, et son texte", "The licence of the data, with its text")),
    sprintf("| %s | %s |", paste(code("CITATION.cff"), code("codemeta.json"), sep = ", "), tx("Comment citer ce paquet, et ses métadonnées", "How to cite this package, and its metadata")),
    sprintf("| %s | %s |", code("MANIFEST.sha256"), tx("L'empreinte SHA-256 de chaque fichier", "The SHA-256 hash of every file")),
    "",
    paste("##", tx("Les modèles", "The models")), "",
    if (length(faits)) vapply(faits, function(i) sprintf("- **%s**%s%s%s — %s", texte_sur(i$nom), if (nzchar(i$titre)) sprintf(" (%s)", texte_sur(i$titre)) else "", deux_points, i$type,
                                                       sprintf(tx("ses résultats dans %s", "its results in %s"), code(paste0(i$objet, "$…")))), "")
    else tx("Le projet n'a pas encore de modèle.", "The project has no model yet."),
    if (length(sans)) c("", tx("Pas refaits par le script :", "Not redone by the script:"), "", vapply(sans, function(i) sprintf("- **%s**%s%s", texte_sur(i$nom), deux_points, texte_sur(i$erreur)), "")),
    if (length(ct$ecarts)) c("", tx("Le script ne redonne pas exactement les résultats de Nuée pour :", "The script doesn't give back Nuée's results exactly for:"), "", paste("-", texte_sur(ct$ecarts))),
    "",
    if (!is.null(ct$script)) c(
      paste("##", tx("Refaire l'analyse avec R", "Redo the analysis with R")), "",
      paste0(sprintf(tx("%s a été écrit avec R %s", "%s was written with R %s"), code(ct$script), r_version),
             if (!nrow(ct$paquets)) tx(" et n'utilise que R.", " and uses R alone.")
             else paste0(tx(" et utilise ", " and uses "),
                         paste(c(if (nrow(avec_r)) sprintf(if (nrow(avec_r) > 1) tx("%s, livrés avec R", "%s, which come with R") else tx("%s, livré avec R", "%s, which comes with R"), versions(avec_r)),
                                 if (nrow(a_installer)) sprintf(tx("%s, à installer : %s", "%s, to install: %s"), versions(a_installer),
                                                                code(sprintf("install.packages(%s)", paste(sprintf("\"%s\"", a_installer$paquet), collapse = ", "))))),
                               collapse = tx(" ; ", "; ")), ".")), "",
      sprintf(tx("1. Installer R (%s).", "1. Install R (%s)."), "https://cran.r-project.org"),
      sprintf(tx("2. Dans ce dossier, lancer %s : il refait chaque modèle et affiche ses résultats ; ses graphiques vont dans %s. Ou l'ouvrir dans RStudio et le sourcer. Ses données sont dans le script : il ne lit aucun fichier.",
                 "2. In this folder, run %s: it redoes every model and prints its results; its plots go to %s. Or open it in RStudio and source it. Its data are in the script: it reads no file."),
              code(paste("Rscript", ct$script)), code("Rplots.pdf")),
      sprintf(tx("3. Les résultats de chaque modèle restent sous son nom : %s.", "3. Each model's results stay under its name: %s."), paste(vapply(faits, function(i) code(i$objet), ""), collapse = ", ")),
      ""),
    if (length(ct$documents)) c(
      sprintf(tx("Les documents : %s recalcule leurs nombres, leurs graphiques et leurs tableaux à partir des données brutes (il faut [Quarto](https://quarto.org) et les paquets R knitr et rmarkdown) :",
                 "The documents: %s recomputes their numbers, plots and tables from the raw data (it takes [Quarto](https://quarto.org) and the R packages knitr and rmarkdown):"),
              code("quarto render documents/<document>.qmd")), "",
      vapply(ct$documents, function(d) paste0("- ", code(paste0("documents/", d$nom, ".qmd")), if (nzchar(d$note)) paste0(" — ", texte_sur(d$note)) else ""), ""), ""),
    paste("##", tx("L'ouvrir dans Nuée", "Open it in Nuée")), "",
    sprintf(tx("Nuée est libre et gratuit, pour Windows, macOS et Linux : %s (DOI %s). Fichier › Ouvrir, puis %s : les données, les modèles (Nuée les ajuste de nouveau) et les documents, tels qu'ils étaient ; Édition › Mettre à jour les données relit les données du CSV du paquet, changé ou non. Il faut Nuée %s ou plus récent.",
               "Nuée is free software, for Windows, macOS and Linux: %s (DOI %s). File › Open, then %s: the data, the models (Nuée fits them again) and the documents, as they were; Edit › Update the data reads the data again from the package's CSV, changed or not. It takes Nuée %s or later."),
            "https://nuee.io", sprintf("[%s](https://doi.org/%s)", DOI_NUEE, DOI_NUEE), code(ct$nuee), "1.1"),
    if (!is.null(doi)) c("", sprintf(tx("Ou dans un navigateur, sans rien installer : %s ouvre ce paquet dans Nuée, par son DOI.",
                                        "Or in a browser, without installing anything: %s opens this package in Nuée, by its DOI."),
                                     sprintf("[%s](%s)", dans_navigateur, dans_navigateur))),
    "",
    paste("##", tx("Vérifier les fichiers", "Check the files")), "",
    sprintf(tx("%s donne l'empreinte SHA-256 de chaque fichier. Dans ce dossier : %s (Linux), %s (macOS), ou %s dans PowerShell (Windows), fichier par fichier.",
               "%s gives the SHA-256 hash of every file. In this folder: %s (Linux), %s (macOS), or %s in PowerShell (Windows), file by file."),
            code("MANIFEST.sha256"), code("sha256sum -c MANIFEST.sha256"), code("shasum -a 256 -c MANIFEST.sha256"), code(tx("Get-FileHash -Algorithm SHA256 <fichier>", "Get-FileHash -Algorithm SHA256 <file>"))),
    "",
    paste("##", tx("Citer", "Cite")), "",
    paste0(citation, sprintf(tx("Analysé avec Nuée %s (Boutin, %s, https://doi.org/%s) et R %s (R Core Team).", "Analysed with Nuée %s (Boutin, %s, https://doi.org/%s) and R %s (R Core Team)."),
                             version, annee, DOI_NUEE, r_version)),
    "", sprintf(tx("Les métadonnées de citation sont dans %s et %s.", "The citation metadata are in %s and %s."), code("CITATION.cff"), code("codemeta.json")))
}

# Le projet tel que le paquet le garde : la source de ses données est son propre CSV, donnees/<projet>.csv
# (data/ en anglais), par son chemin depuis le dossier du paquet, celui du .nuee, et rien d'autre : ni
# le fichier d'où venaient les données, ni aucun chemin de l'ordinateur où le paquet s'écrit. Édition ›
# Mettre à jour les données relit ainsi ce CSV, du paquet décompressé où qu'il soit (etapes.R :
# fichier_retrouve, par le chemin relatif d'abord) ; un CSV changé y entre. Ses étapes n'y sont plus :
# le CSV est ce qu'elles ont donné, les rejouer dessus les referait, et leurs fichiers (une table jointe,
# un raster) restent chez l'auteur. L'import de ce CSV décide de ses colonnes comme le projet les a : la
# première nomme les observations, les identifiants secondaires en restent, les variables quantitatives
# se lisent en nombres (nouveau.R : projet_depuis_tableau). `fichier` : le CSV écrit ; `relatif` : son
# chemin dans le paquet ; `tab` : ce qu'il contient (tableau_projet).
source_paquet <- function(p, fichier, relatif, tab) {
  vars <- Filter(function(v) nom_numero(v$uid) %in% colnames(p$donnees), p$variables)
  n_id <- ncol(tab) - length(vars)
  variables <- names(tab)[-seq_len(n_id)]
  s <- source_fichier(fichier, "tableau", reglages = list(identifiant = names(tab)[1], secondaires = names(tab)[seq_len(n_id)[-1]],
                                                          quantitatives = variables[vapply(tab[-seq_len(n_id)], is.numeric, TRUE)]))
  s$chemin <- relatif
  s$relatif <- relatif
  p$source_donnees <- s
  p$etapes <- list()
  p$changements_avant_etapes <- NULL
  p$empreinte_donnees <- empreinte_donnees(p)
  p
}

# Le paquet de reproductibilité du projet `projet`, écrit dans l'archive `chemin` (.zip), qui contient
# un dossier du même nom.
#   modeles    list(id, definition, modele, erreur) pour chaque modèle du projet (script_paquet) ;
#   meta       les réponses du dialogue (metadonnees_paquet) ;
#   documents  ses documents, comme l'interface les a faits (ecrire_documents_paquet) ;
#   langue     celle du README, du script, du dictionnaire et des noms des fichiers ;
#   version    celle de Nuée ; session : la disposition de l'interface, gardée dans le .nuee comme
#              Enregistrer la garde ;
#   doi        celui du paquet, réservé par Zenodo pour son brouillon (doi_paquet), que le README,
#              CITATION.cff et codemeta.json donnent ; NULL sans dépôt.
# Rend le chemin de l'archive, les fichiers qu'elle contient (relatifs à son dossier), chaque modèle
# (son nom dans le script, ou pourquoi il n'y est pas), les écarts et les documents.
ecrire_paquet <- function(projet, modeles, chemin, meta, documents = list(), langue = "fr", version = "", session = NULL, doi = NULL) {
  langue <- if (identical(langue, "en")) "en" else "fr"
  doi <- doi_paquet(doi)
  tx <- function(fr, en) if (langue == "en") en else fr
  if (!grepl("\\.zip$", chemin, ignore.case = TRUE)) chemin <- paste0(chemin, ".zip")
  base <- trimws(gsub("[^[:alnum:]_ ().-]", "_", tools::file_path_sans_ext(basename(chemin))))
  if (!nzchar(base) || grepl("^\\.+$", base)) base <- tx("paquet", "package")
  dossier <- tempfile("paquet")
  racine <- file.path(dossier, base)
  dir.create(racine, recursive = TRUE)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  # (le nom du projet, sans son extension, pour ses fichiers : lettres, chiffres, espaces, _ ( ) -)
  nom <- trimws(gsub("[^[:alnum:]_ ()-]", "_", tools::file_path_sans_ext(as.character(projet$nom %||% projet$source %||% "")[1])))
  if (is.na(nom) || !nzchar(nom)) nom <- tx("projet", "project")
  # les données et leur dictionnaire
  donnees <- tx("donnees", "data")
  csv <- paste0(donnees, "/", nom, ".csv")
  dictionnaire <- paste0(donnees, "/", nom, tx(" - dictionnaire.csv", " - dictionary.csv"))
  tab <- tableau_projet(projet)
  ecrire_texte_paquet(file.path(racine, csv), lignes_csv(tab))
  ecrire_texte_paquet(file.path(racine, dictionnaire), lignes_csv(dictionnaire_donnees(projet, langue)))
  # le projet, comme Enregistrer l'écrit (format 3), ses données venant de ce CSV, avec les réponses du
  # dialogue et la disposition de l'interface ; ses chemins tels quels (dossier = NULL : chemins_relatifs
  # les ferait partir du dossier temporaire où le paquet s'écrit)
  p <- source_paquet(projet, file.path(racine, csv), csv, tab)
  p$paquet <- meta
  if (!is.null(session)) p$session <- session
  fichier_nuee <- paste0(nom, ".nuee")
  enregistrer_projet(p, file.path(racine, fichier_nuee), version, dossier = NULL)
  # le script de tous les modèles
  script <- tx("analyse.R", "analysis.R")
  s <- script_paquet(projet, modeles, file.path(racine, script), meta, langue, version)
  # les documents
  docs <- if (length(documents)) ecrire_documents_paquet(documents, file.path(racine, "documents")) else list(fichiers = character(0), documents = list())
  contenu <- list(nuee = fichier_nuee, csv = csv, dictionnaire = dictionnaire, script = if (!is.null(s$script)) script, documents = docs$documents,
                  modeles = s$modeles, ecarts = s$ecarts, paquets = s$paquets, n_obs = nrow(tab), n_colonnes = ncol(tab))
  ecrire_texte_paquet(file.path(racine, "LICENSE"), lignes_licence(meta))
  ecrire_texte_paquet(file.path(racine, "README.md"), lignes_readme(meta, langue, version, contenu, doi))
  ecrire_texte_paquet(file.path(racine, "CITATION.cff"), lignes_cff(meta, langue, version, s$paquets, doi))
  ecrire_texte_paquet(file.path(racine, "codemeta.json"), codemeta_paquet(meta, langue, version, s$paquets, doi))
  # l'empreinte de chaque fichier (dans l'ordre des octets de leurs chemins, comme sort -C)
  fichiers <- sort(list.files(racine, recursive = TRUE, all.files = TRUE, no.. = TRUE), method = "radix")
  ecrire_texte_paquet(file.path(racine, "MANIFEST.sha256"), sprintf("%s  %s", empreintes_sha256(file.path(racine, fichiers)), fichiers))
  # l'archive, écrite à côté puis mise à sa place (nouveau.R : ecrire_sans_risque)
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE)
  ecrire_sans_risque(chemin, function(f) {
    unlink(f)
    zip::zip(normalizePath(f, mustWork = FALSE), files = base, root = dossier)
  })
  list(chemin = chemin, chemins = I(chemin), dossier = base, fichiers = I(c(sort(c(fichiers, "MANIFEST.sha256"), method = "radix"))),
       modeles = lapply(s$modeles, function(i) list(id = i$id, nom = i$nom, objet = i$objet %||% NULL, erreur = i$erreur %||% NULL)),
       ecarts = I(s$ecarts), documents = docs$documents, taille = file.size(chemin))
}
