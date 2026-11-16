# Écrit docs/protocole.md, la page de référence du protocole du moteur, de sa description
# (moteur/protocole.json) : comment démarrer le moteur et lui parler, puis chaque demande.
#   Rscript docs/ecrire_protocole.R   (après avoir changé moteur/protocole.json)
# moteur/tests/verifier_protocole.R lit ce fichier (page_protocole) et vérifie que la page est à jour.

# ---------------------------------------------------------------- le Markdown

# Du texte de la description dans du Markdown : hors des `…`, ce que Markdown lirait autrement (une
# balise <id>, une emphase *…* ou _…_, un barré ~…~) ; dans un tableau, ses | et ses retours à la ligne.
md_texte <- function(x, tableau = FALSE) {
  x <- gsub("[\r\n]+\\s*", " ", as.character(x))
  morceaux <- strsplit(x, "`", fixed = TRUE)[[1]]
  if (endsWith(x, "`")) morceaux <- c(morceaux, "")
  hors_code <- seq_along(morceaux) %% 2 == 1
  morceaux[hors_code] <- vapply(morceaux[hors_code], function(m) {
    m <- gsub("&", "&amp;", m, fixed = TRUE)
    m <- gsub("<", "&lt;", m, fixed = TRUE)
    m <- gsub(">", "&gt;", m, fixed = TRUE)
    m <- gsub("([*~\\\\])", "\\\\\\1", m)
    gsub("(^|[^A-Za-z0-9])_", "\\1\\\\_", m)  # (des classes ASCII : la même page dans toutes les langues de R)
  }, "")
  x <- paste(morceaux, collapse = "`")
  if (tableau) x <- gsub("|", "\\|", x, fixed = TRUE)
  x
}
# (du code tel quel ; dans un tableau, ses | protégés)
code <- function(x) paste0("`", gsub("|", "\\|", x, fixed = TRUE), "`")
# L'ancre que GitHub donne à un titre : en minuscules, sans ponctuation, les espaces en tirets.
ancre <- function(titre) gsub(" ", "-", gsub("[^a-z0-9 _-]", "", tolower(titre)))
lien_type <- function(n) sprintf("[`%s`](#%s)", n, ancre(paste("Type", n)))

# Une expression de type (number|string, Groupement[]…), ses types nommés liés à leur section.
md_type <- function(t, types) {
  parties <- vapply(strsplit(t, "|", fixed = TRUE)[[1]], function(p) {
    p <- trimws(p)
    base <- sub("(\\[\\])+$", "", p)
    tableaux <- substring(p, nchar(base) + 1)
    if (base %in% types) paste0(lien_type(base), if (nzchar(tableaux)) code(tableaux)) else code(p)
  }, "")
  paste(parties, collapse = " \\| ")
}
valeur_json <- function(v) as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"))

tableau_md <- function(entetes, lignes) {
  c(paste0("| ", paste(entetes, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(entetes)), collapse = "|"), "|"),
    vapply(lignes, function(l) paste0("| ", paste(l, collapse = " | "), " |"), ""))
}

# Le sens d'un paramètre, avec ses valeurs permises, son défaut et ses bornes.
sens_parametre <- function(p) {
  s <- md_texte(p$description, TRUE)
  if (length(p$valeurs)) {
    s <- paste0(s, " Values: ", paste(vapply(names(p$valeurs), function(v) {
      sens <- p$valeurs[[v]]
      paste0(code(v), if (nzchar(sens)) paste0(" (", md_texte(sens, TRUE), ")"))
    }, ""), collapse = ", "), ".")
  }
  if (!is.null(p$min) || !is.null(p$max)) {
    s <- paste0(s, " ", if (!is.null(p$min) && !is.null(p$max)) sprintf("From %s to %s.", p$min, p$max)
                else if (!is.null(p$min)) sprintf("At least %s.", p$min) else sprintf("At most %s.", p$max))
  }
  if ("defaut" %in% names(p)) s <- paste0(s, " Default: ", code(valeur_json(p$defaut)), ".")
  if (!is.null(p$depuis)) s <- paste0(s, " Since ", p$depuis, ".")
  s
}
tableau_parametres <- function(ps, types) {
  tableau_md(c("Parameter", "Type", "Required", "Meaning"), lapply(names(ps), function(n) {
    p <- ps[[n]]
    c(code(n), md_type(p$type, types), if (isTRUE(p$requis)) "yes" else "no", sens_parametre(p))
  }))
}
tableau_champs <- function(cs, types) {
  tableau_md(c("Field", "Type", "Meaning"), lapply(names(cs), function(n) {
    c <- cs[[n]]
    c(code(n), md_type(c$type, types), paste0(md_texte(c$description, TRUE), if (!is.null(c$depuis)) paste0(" Since ", c$depuis, ".")))
  }))
}

EFFETS_MD <- c(aucun = "Changes nothing.",
               seance = "Changes the session, not a project's content.",
               projet = "Changes the project, without an undo step.",
               annulable = "Changes the project, with an undo step that `annuler` takes back.")

# ---------------------------------------------------------------- la page

page_protocole <- function(d) {
  types <- names(d$types)
  tr <- d$transport
  groupes <- vapply(d$groupes, function(g) as.character(g$id), "")
  titres <- stats::setNames(vapply(d$groupes, function(g) as.character(g$titre), ""), groupes)
  n <- length(d$demandes)
  paragraphes <- function(x) unlist(lapply(unlist(x), function(p) c(md_texte(p), "")))

  entete <- c(
    "# Nuée's engine protocol", "",
    "<!-- Written by docs/ecrire_protocole.R from moteur/protocole.json: change the description there, then run Rscript docs/ecrire_protocole.R (moteur/tests/verifier_protocole.R checks that this page is up to date). -->", "",
    sprintf(paste("Version **%s** of the protocol by which the interface, and any other client (a plugin, a Python package, QGIS), talks to",
                  "Nuée's R engine: requests in JSON over HTTP on 127.0.0.1, each carrying the session's token. This page says how",
                  "to start the engine and reach it, then describes each of its %d requests: its purpose, its parameters, its",
                  "result, its errors, what it changes and the version it exists since."), d$version, n), "",
    paste("It is written from `moteur/protocole.json`, the description that the request [`protocole`](#protocole) also gives",
          "(with `complet`); `moteur/tests/verifier_protocole.R` checks that every request the engine answers is described",
          "there, and nothing else. `outils/exemple_client.py` is a client in Python, with nothing but its standard",
          "library: it starts the engine from the sources, opens the example project and fits one of its models."), "",
    "- [Versions](#versions)",
    "- [Starting the engine](#starting-the-engine)",
    "- [The port](#the-port)",
    "- [The session's token](#the-sessions-token)",
    "- [Language](#language)",
    "- [Sending a request](#sending-a-request)",
    "- [Answers](#answers)",
    "- [Status codes](#status-codes)",
    "- [Paths](#paths)",
    "- [What a client does](#what-a-client-does)",
    "- [Reading the description](#reading-the-description)",
    "- [Types](#types), [Parameter sets](#parameter-sets), [Errors](#errors)",
    paste0("- [The requests](#the-requests): ", paste(sprintf("[%s](#%s)", titres, ancre(titres)), collapse = " · ")), "")

  versions <- c(
    "## Versions", "",
    paste("The protocol's version is MAJOR.MINOR. MINOR grows when a request, a parameter or a field of a result is added:",
          "a client written for 1.1 works with every later 1.x. MAJOR grows when a request goes or changes its name, or a",
          "parameter or field changes its meaning or goes. Each request says the version it exists since (`depuis`), and so",
          "may a parameter or a field added after its request."), "",
    tableau_md(c("Version", "Changes"), lapply(d$versions, function(v) c(v$version, md_texte(v$changements, TRUE)))), "")

  dem <- tr$demarrage
  transport <- c(
    "## Starting the engine", "",
    "```", dem$commande, "```", "",
    paragraphes(dem$description),
    "Its environment:", "",
    tableau_md(c("Variable", "Meaning"), lapply(names(dem$environnement), function(v) c(code(v), md_texte(dem$environnement[[v]], TRUE)))), "",
    "## The port", "",
    md_texte(tr$port$description), "",
    sprintf("Argument %d of the command; %s by default; the application's: %s.", tr$port$argument, tr$port$defaut, tr$port$application), "",
    "## The session's token", "",
    paragraphes(tr$jeton$description),
    sprintf("Environment variable: %s. Cookie: %s.", code(tr$jeton$variable), code(tr$jeton$cookie)), "",
    "## Language", "",
    md_texte(tr$langue$description), "",
    sprintf("Header: %s, %s; without it: %s.", code(tr$langue$entete),
            paste(sprintf("%s (%s)", vapply(names(tr$langue$valeurs), code, ""), unlist(tr$langue$valeurs)), collapse = " or "), code(tr$langue$defaut)), "",
    "## Sending a request", "",
    sprintf("%s %s", code(tr$demande$methode), code(tr$demande$chemin)), "",
    paragraphes(tr$demande$description),
    "## Answers", "",
    sprintf("Content type: %s.", code(tr$reponse$type)), "",
    paragraphes(tr$reponse$description),
    "## Status codes", "",
    tableau_md(c("Status", "Meaning"), lapply(names(tr$statuts), function(s) c(s, md_texte(tr$statuts[[s]], TRUE)))), "",
    "## Paths", "",
    tableau_md(c("Path", "What"), lapply(names(tr$chemins), function(p) c(code(p), md_texte(tr$chemins[[p]], TRUE)))), "",
    "## What a client does", "",
    paste0(seq_along(tr$client), ". ", vapply(unlist(tr$client), md_texte, "")), "")

  lecture <- c(
    "## Reading the description", "",
    paste("`moteur/protocole.json` holds `version`; `versions`, the changes of each; `groupes`, the sections of this page;",
          "`transport`, what the sections above say; `types`, the shapes several requests share; `ensembles`, the",
          "parameters several requests share; `erreurs`, the errors several requests raise; and `demandes`, each request",
          "by its name:"), "",
    tableau_md(c("Key", "What"), list(
      c("`groupe`", "Its section of this page."),
      c("`but`", "Its purpose, in one sentence."),
      c("`parametres`", "Its parameters, by name: `type`, `requis` (whether it is required), `description`, and when they apply `valeurs` (each value allowed, with its meaning), `defaut` (the value taken when it is left out), `min`, `max` and `depuis`."),
      c("`ensembles`", "The parameter sets it also takes; a parameter of its own of the same name takes the set's place."),
      c("`resultat`", "Its result: `type`, and `champs`, its fields (`type`, `description`). With a named type, `champs` are the fields the request adds to it; with an array of objects, the fields of each."),
      c("`erreurs`", "The errors it raises: the name of one of `erreurs`, or `{ message, quand }`, the English message (`<…>` stands for what the engine writes in) and when."),
      c("`effet`", "What it changes: `aucun`, nothing; `seance`, the session but no project's content (a project opened or closed, a fitted model added, a result kept with a model, a cache); `projet`, a project's content, what saving writes, without an undo step; `annulable`, a project's content, with an undo step that `annuler` takes back. `effet_detail` says how, and on what condition."),
      c("`depuis`", "The protocol version it exists since."))), "",
    paste("A type is written as `string`, `number`, `integer`, `boolean`, `object`, `null`, `any` or the name of one of",
          "`types`, followed by `[]` for an array of it; `a|b` is either. An array of one element may arrive as that element",
          "(see [Answers](#answers)), unless its description says it is always an array."), "")

  sections_types <- c("## Types", "", unlist(lapply(types, function(nom) {
    t <- d$types[[nom]]
    c(paste0("### Type `", nom, "`"), "", md_texte(t$description), "",
      if (!is.null(t$type)) c(paste0("Written as ", md_type(t$type, types), "."), ""),
      if (!is.null(t$champs)) c(tableau_champs(t$champs, types), ""))
  })))
  sections_ensembles <- c("## Parameter sets", "", unlist(lapply(names(d$ensembles), function(nom) {
    utilise <- names(Filter(function(x) nom %in% unlist(x$ensembles), d$demandes))
    c(paste0("### Set `", nom, "`"), "",
      paste0("Taken by ", paste(sprintf("[`%s`](#%s)", utilise, ancre(utilise)), collapse = ", "), "."), "",
      tableau_parametres(d$ensembles[[nom]], types), "")
  })))
  sections_erreurs <- c("## Errors", "",
    paste("Errors several requests raise. Each comes as status 400 with `{\"erreur\": message}`, unless another status is",
          "given; the message is in the request's language, and its words between `<…>` are filled in by the engine."), "",
    tableau_md(c("Name", "Message (in English)", "When"), lapply(names(d$erreurs), function(nom) {
      e <- d$erreurs[[nom]]
      c(code(nom), paste0(md_texte(e$message, TRUE), if (!is.null(e$statut)) sprintf(" (status %s)", e$statut)), md_texte(e$quand, TRUE))
    })), "")

  sections_demandes <- c("## The requests", "", unlist(lapply(groupes, function(g) {
    noms <- names(Filter(function(x) identical(x$groupe, g), d$demandes))
    c(paste("###", titres[[g]]), "",
      paste(vapply(noms, function(x) sprintf("[`%s`](#%s)", x, ancre(x)), ""), collapse = " · "), "",
      unlist(lapply(noms, function(nom) {
        x <- d$demandes[[nom]]
        r <- x$resultat
        tableau_type <- grepl("\\[\\]$", r$type)
        nomme <- r$type %in% types
        # (une erreur commune : son message, son nom lié au tableau des erreurs, et quand)
        erreurs <- lapply(x$erreurs, function(e) {
          if (is.character(e)) c(md_texte(d$erreurs[[e]]$message, TRUE), paste0(md_texte(d$erreurs[[e]]$quand, TRUE), " ([", code(e), "](#errors))"))
          else c(md_texte(e$message, TRUE), md_texte(e$quand, TRUE))
        })
        c(paste0("#### `", nom, "`"), "",
          md_texte(x$but), "",
          paste0("Since ", x$depuis, ". ", EFFETS_MD[[x$effet]], if (!is.null(x$effet_detail)) paste0(" ", md_texte(x$effet_detail))), "",
          if (length(x$parametres)) c(tableau_parametres(x$parametres, types), "") else if (!length(x$ensembles)) c("No parameters.", ""),
          if (length(x$ensembles)) c(paste0(if (length(x$parametres)) "Also the" else "The", " parameters of the set",
                                            if (length(x$ensembles) > 1) "s" else "", " ",
                                            paste(sprintf("[`%s`](#%s)", unlist(x$ensembles), ancre(paste("Set", unlist(x$ensembles)))), collapse = ", "), "."), ""),
          paste0("**Result:** ", md_type(r$type, types),
                 if (length(r$champs)) (if (nomme) ", with the fields below too" else if (tableau_type) ", each with the fields below" else ", with the fields below") else "",
                 ".", if (!is.null(r$description)) paste0(" ", md_texte(r$description))),
          "",
          if (length(r$champs)) c(tableau_champs(r$champs, types), ""),
          if (length(erreurs)) c("**Errors:**", "", tableau_md(c("Message (in English)", "When"), erreurs), "") else c("**Errors:** none of its own.", ""))
      })))
  })))

  page <- c(entete, versions, transport, lecture, sections_types, sections_ensembles, sections_erreurs, sections_demandes)
  # (sans lignes vides en double, ni à la fin)
  page <- page[!(page == "" & c(FALSE, utils::head(page, -1) == ""))]
  while (length(page) && page[length(page)] == "") page <- page[-length(page)]
  enc2utf8(page)
}

# La page écrite en UTF-8, ses lignes finies par \n sur tous les systèmes.
ecrire_page_protocole <- function(d, fichier) {
  f <- file(fichier, "wb")
  on.exit(close(f))
  writeLines(page_protocole(d), f, sep = "\n", useBytes = TRUE)
}

if (sys.nframe() == 0L) {  # (lancé par Rscript, non lu par source())
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  racine_nuee <- normalizePath(file.path(dirname(if (length(f)) f else "docs/ecrire_protocole.R"), ".."), winslash = "/")
  d <- jsonlite::fromJSON(file.path(racine_nuee, "moteur", "protocole.json"), simplifyVector = FALSE)
  ecrire_page_protocole(d, file.path(racine_nuee, "docs", "protocole.md"))
  cat("docs/protocole.md :", length(d$demandes), "demandes, version", d$version, "\n")
}
