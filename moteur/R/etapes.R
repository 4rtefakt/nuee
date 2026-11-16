# Les rapports vivants (1.1). Un projet fait d'un fichier de tableau garde d'où viennent ses données
# (p$source_donnees : le fichier, sa table, ce que l'import a décidé de ses colonnes, sa taille, sa
# date et son empreinte SHA-256 quand il a été lu) et chaque changement fait à ses données dans
# Nuée, en étapes qui se rejouent (p$etapes : la demande, ses paramètres, son étiquette, sa date).
# Mettre à jour les données (serveur.R : mettre_a_jour) relit ce fichier, ou sa version de l'année
# suivante, rejoue les étapes (rejouer_etapes), rapporte ce qui en sort aux variables et aux
# observations du projet par leurs noms (aligner_projet) et remplace les données ; chaque modèle
# garde sa définition, et les nouvelles observations y entrent. Un projet sans fichier (un tableau
# collé, un résumé par groupe, un .usp, un projet de la 1.0) note aussi ses étapes, et se met à
# jour d'un fichier qu'on lui donne.
#
# Les numéros internes (uid) des variables et des observations changent d'un import à l'autre : les
# paramètres d'une étape n'en ont pas. Ses variables y sont écrites par leur nom et ses
# observations par leur identifiant (cles_observations), traduits quand l'étape est notée (par_noms)
# et retraduits quand elle est rejouée (par_numeros), d'après ce que ETAPES_DONNEES dit de chaque
# demande. Ils sont gardés en texte JSON : lire_projet simplifie les listes d'un fichier, et des
# paramètres imbriqués en changeraient de forme ; relus, ils ont celle d'une demande de l'interface.
# p$empreinte_donnees dit ce qu'étaient les données après la dernière étape (empreinte_donnees),
# pour voir qu'elles ont changé autrement depuis (la console R, une version d'avant la 1.1).

# ---------------------------------------------------------------- les fichiers lus

# Ce qu'on garde d'un fichier lu : son chemin absolu, sa taille, sa date et son empreinte SHA-256
# quand on l'a lu, et quand (la source d'un projet, un tableau dont des colonnes ont été ajoutées, un
# raster, un formulaire XLSForm). `relatif`, son chemin depuis le dossier du fichier du projet, s'y
# ajoute quand le projet est enregistré (chemins_relatifs).
fiche_fichier <- function(chemin) {
  i <- file.info(chemin)
  list(chemin = normalizePath(chemin, winslash = "/", mustWork = FALSE), taille = as.numeric(i$size), date = as.numeric(i$mtime),
       sha256 = unname(as.character(tools::sha256sum(chemin))), lu = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
}

# Le premier texte de `x`, `defaut` sans lui (une valeur relue d'un fichier peut être absente, vide,
# une liste ou NA).
premier_texte <- function(x, defaut = "") {
  x <- as.character(unlist(x))
  if (length(x) && !is.na(x[1])) x[1] else defaut
}

# Le fichier d'une fiche tel qu'on le retrouve : par son chemin depuis `dossier` (celui du fichier du
# projet) d'abord, un dossier déplacé avec ses données gardant ainsi les siennes, puis par son
# chemin absolu ; NULL quand il n'est ni à l'un ni à l'autre.
fichier_retrouve <- function(fiche, dossier = NULL) {
  relatif <- premier_texte(fiche$relatif)
  essais <- c(if (length(dossier) && nzchar(relatif)) file.path(dossier, relatif), premier_texte(fiche$chemin))
  for (f in essais) if (nzchar(f) && file_test("-f", f)) return(normalizePath(f, winslash = "/"))
  NULL
}

# Le chemin de `chemin` depuis le dossier `dossier` (« donnees/releves.csv », « ../releves.csv »),
# NULL quand il n'y en a pas : un autre disque, ou un autre serveur, sous Windows.
chemin_relatif <- function(chemin, dossier) {
  morceaux <- function(x) strsplit(normalizePath(x, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1]]
  a <- morceaux(chemin)
  b <- morceaux(dossier)
  windows <- .Platform$OS.type == "windows"
  pareil <- function(x, y) if (windows) identical(tolower(x), tolower(y)) else identical(x, y)
  n <- 0
  while (n < min(length(a), length(b)) && pareil(a[n + 1], b[n + 1])) n <- n + 1
  # (sous Windows, le disque en commun, « C: », ou le serveur et son partage, « //serveur/partage »)
  if (n == 0 || (windows && !nzchar(a[1]) && n < 4)) return(NULL)
  paste(c(rep("..", length(b) - n), a[-seq_len(n)]), collapse = "/")
}

# Le dossier du fichier du projet, d'où se lisent les chemins relatifs : celui où il s'enregistre, ou
# celui du .nuee d'où il vient ; NULL pour un projet qui n'a pas encore de fichier.
dossier_projet <- function(p) {
  f <- premier_texte(p$chemin_enregistrement)
  if (!nzchar(f) && extension(premier_texte(p$chemin)) %in% c("nuee", "json")) f <- premier_texte(p$chemin)
  if (nzchar(f)) dirname(normalizePath(f, winslash = "/", mustWork = FALSE))
}

# Le projet dont la source et les fichiers des étapes ont leur chemin depuis `vers`, le dossier où il
# s'enregistre : chaque fichier cherché d'abord où il est (fichier_retrouve, depuis le dossier du
# projet d'avant), son chemin absolu mis à jour quand il a suivi le projet ailleurs.
chemins_relatifs <- function(p, vers) {
  if (is.null(vers) || (is.null(p$source_donnees) && !length(p$etapes))) return(p)
  depuis <- dossier_projet(p)
  suivre <- function(fiche) {
    if (!is.list(fiche) || !nzchar(premier_texte(fiche$chemin))) return(fiche)
    trouve <- fichier_retrouve(fiche, depuis)
    if (!is.null(trouve)) fiche$chemin <- trouve
    fiche["relatif"] <- list(chemin_relatif(fiche$chemin, vers))
    fiche
  }
  if (!is.null(p$source_donnees)) {
    p$source_donnees <- suivre(p$source_donnees)
    if (is.list(p$source_donnees$formulaire)) p$source_donnees$formulaire <- suivre(p$source_donnees$formulaire)
  }
  if (length(p$etapes)) p$etapes <- lapply(p$etapes, function(e) {
    if (length(e$fichiers)) e$fichiers <- lapply(e$fichiers, suivre)
    e
  })
  p
}

# Où en est le fichier d'une fiche (la demande etapes) : celui qui serait lu (`trouve`), et s'il a
# changé depuis qu'il a été lu (`modifie` ; NA quand il n'est pas là) : une autre taille, ou une autre
# date et un autre contenu (une même taille et une même date : le même fichier, sans le relire ; une
# date seule changée, une copie, ne le change pas).
etat_fiche <- function(fiche, dossier = NULL) {
  date_texte <- function(x) {
    x <- suppressWarnings(as.numeric(unlist(x))[1])
    if (length(x) && is.finite(x)) format(as.POSIXct(x, origin = "1970-01-01"), "%Y-%m-%dT%H:%M:%S")
  }
  trouve <- fichier_retrouve(fiche, dossier)
  r <- list(chemin = premier_texte(fiche$chemin), relatif = if (nzchar(premier_texte(fiche$relatif))) premier_texte(fiche$relatif),
            trouve = trouve, existe = !is.null(trouve), taille = suppressWarnings(as.numeric(unlist(fiche$taille))[1]),
            date = date_texte(fiche$date), sha256 = premier_texte(fiche$sha256), lu = premier_texte(fiche$lu), modifie = NA)
  if (!is.null(trouve)) {
    i <- file.info(trouve)
    r$actuel <- list(taille = as.numeric(i$size), date = date_texte(as.numeric(i$mtime)))
    r$modifie <- !isTRUE(as.numeric(i$size) == r$taille) ||
      (!isTRUE(abs(as.numeric(i$mtime) - as.numeric(unlist(fiche$date))[1]) < 1e-3) && !identical(unname(as.character(tools::sha256sum(trouve))), r$sha256))
  }
  r
}

# Un fichier de la séance (le dossier temporaire de R) : des cellules collées, un fichier envoyé par
# le navigateur ; il disparaît quand Nuée se ferme.
dans_seance <- function(chemin) {
  f <- normalizePath(chemin, winslash = "/", mustWork = FALSE)
  t <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") { f <- tolower(f); t <- tolower(t) }
  startsWith(f, paste0(t, "/"))
}

# ---------------------------------------------------------------- la source

# La source d'un projet fait d'un fichier (serveur.R : ouvrir_fichier, ouvrir_collecte) : sa fiche
# (fiche_fichier), comment le relire (`lecteur` : "tableau", importer_tableau ; "archive", une table
# d'une archive d'ODK Central ouverte telle quelle, projet_brut_collecte ; "collecte", l'import d'un
# export de KoboToolbox ou d'ODK avec ses réglages, `collecte`, son formulaire XLSForm en fiche à
# part), sa table ou sa feuille, et ce que l'import a décidé de ses colonnes (`reglages` : nouveau.R,
# importer_tableau et projet_depuis_tableau), que l'import suivant reprend.
source_fichier <- function(chemin, lecteur, table = NULL, reglages = NULL, collecte = NULL) {
  s <- c(fiche_fichier(chemin), list(lecteur = lecteur))
  if (length(table)) s$table <- as.character(table)[1]
  if (!is.null(reglages)) {
    s$reglages <- list(identifiant = premier_texte(reglages$identifiant), secondaires = I(as.character(unlist(reglages$secondaires))),
                       quantitatives = I(as.character(unlist(reglages$quantitatives))))
  }
  if (!is.null(collecte)) {
    s$collecte <- collecte[setdiff(names(collecte), "formulaire")]
    f <- premier_texte(collecte$formulaire)
    if (nzchar(f) && file_test("-f", f)) s$formulaire <- fiche_fichier(f)
  }
  s
}

# Un projet qui commence ses étapes : aucune encore, et l'empreinte de ses données telles qu'il les a
# reçues (un tableau importé, collé, un résumé par groupe).
debut_etapes <- function(p) {
  p$etapes <- list()
  p$empreinte_donnees <- empreinte_donnees(p)
  p
}

# Le projet importé d'un fichier, avec sa source (source_fichier) : les décisions de l'import
# (attr(, "reglages")) y sont gardées, et retirées du projet.
avec_source <- function(p, chemin, lecteur, table = NULL, collecte = NULL) {
  reglages <- attr(p, "reglages")
  attr(p, "reglages") <- NULL
  p$source_donnees <- source_fichier(chemin, lecteur, table, reglages, collecte)
  debut_etapes(p)
}

# Comment lire le fichier donné pour mettre à jour un projet qui n'a pas de source : un export de
# KoboToolbox ou d'ODK Central par son import (ses réglages par défaut), un autre tableau comme
# Fichier › Ouvrir le lit.
lecteur_fichier <- function(chemin) {
  ext <- extension(chemin)
  if (ext %in% EXTENSIONS_COLLECTE && (ext == "zip" || !is.null(detecter_collecte(chemin)))) return("collecte")
  if (ext %in% FORMATS_LECTURE) return("tableau")
  stop(traduire("Format de fichier non reconnu : .", "Unrecognised file format: ."), ext, call. = FALSE)
}

# Le projet que donne la lecture d'un fichier par son lecteur (source_fichier), avec les décisions
# `reglages` d'un import précédent (list() : un premier import) ; attr(, "reglages") : celles de cette
# lecture. `collecte` : les réglages de l'import d'un export de KoboToolbox ou d'ODK, son formulaire
# (`formulaire`) par son chemin.
importer_source <- function(chemin, lecteur, table = NULL, reglages = list(), collecte = NULL) {
  reglages <- reglages %||% list()
  switch(lecteur,
    tableau = importer_tableau(chemin, table, reglages = reglages),
    archive = projet_brut_collecte(chemin, table, identifiants = reglages),
    collecte = importer_collecte(chemin, table = collecte$table, garder = as.character(unlist(collecte$garder)), parents = collecte$parents,
                                 formulaire = if (nzchar(premier_texte(collecte$formulaire))) premier_texte(collecte$formulaire),
                                 langue = collecte$langue, identifiants = reglages),
    stop(sprintf(traduire("Lecteur de fichier inconnu : %s", "Unknown file reader: %s"), lecteur), call. = FALSE))
}

# ---------------------------------------------------------------- l'empreinte des données

# L'empreinte des données d'un projet : de quoi voir qu'elles ont changé, sans les garder. Ce qui
# s'écrit en texte (les noms des variables, leurs catégories, les identifiants des observations, la
# place des valeurs manquantes) par son empreinte SHA-256 ; les nombres par deux sommes de chaque
# variable, celle de leurs valeurs absolues et celle de leurs valeurs pondérées par le rang de leur
# observation (deux valeurs échangées la changent), comparées à 1e-11 près de la première
# (memes_donnees) : le fichier du projet écrit les nombres à 15 chiffres, et l'empreinte exacte des
# nombres relus ne serait pas celle des nombres écrits.
empreinte_donnees <- function(p) {
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(p$donnees), p$variables)
  obs <- as.character(vapply(p$observations, function(o) as.numeric(o$uid), 0))
  obs <- obs[obs %in% rownames(p$donnees)]
  M <- p$donnees[obs, vapply(vars, function(v) as.character(v$uid), ""), drop = FALSE]
  absentes <- which(!is.finite(M))
  M[absentes] <- 0
  poids <- 1 + (seq_len(nrow(M)) * 0.6180339887498949) %% 1
  texte <- paste(c(vapply(vars, function(v) paste(c(as.character(v$ids[1]), as.character(unlist(v$categories))), collapse = "\u001f"), ""),
                   paste(as.character(unlist(p$observation_id_names)), collapse = "\u001f"),
                   vapply(p$observations, function(o) paste(as.character(unlist(o$ids)), collapse = "\u001f"), "")), collapse = "\u001e")
  octets <- c(charToRaw(enc2utf8(texte)), writeBin(as.integer(absentes), raw(), endian = "little"))
  list(structure = unname(as.character(tools::sha256sum(bytes = octets))), sommes = unname(c(colSums(abs(M)), colSums(M * poids))))
}

# Deux empreintes des mêmes données (empreinte_donnees) : le même texte, et les mêmes sommes à 1e-11
# près de la somme des valeurs absolues de leur variable (relu d'un fichier, chaque nombre diffère
# de celui écrit de 5e-15 au plus, en proportion).
memes_donnees <- function(a, b) {
  if (!is.list(a) || !is.list(b) || !identical(premier_texte(a$structure), premier_texte(b$structure))) return(FALSE)
  x <- as.numeric(unlist(a$sommes))
  y <- as.numeric(unlist(b$sommes))
  if (length(x) != length(y) || length(x) %% 2) return(FALSE)
  k <- length(x) %/% 2
  echelle <- pmax(abs(x[seq_len(k)]), abs(y[seq_len(k)]))
  isTRUE(all(abs(x - y) <= 1e-11 * c(echelle, 2 * echelle)))
}

# Les données d'un projet sont-elles encore celles que ses étapes donnent ? TRUE ; FALSE quand elles
# ont changé autrement depuis la dernière (la console R, une version d'avant la 1.1 : son empreinte
# ne va plus avec elles, ou a été marquée rompue, "") ; NA pour un projet sans empreinte, d'avant la
# 1.1.
donnees_conformes <- function(p) {
  e <- p$empreinte_donnees
  if (is.null(e)) return(NA)
  is.list(e) && memes_donnees(empreinte_donnees(p), e)
}

# ---------------------------------------------------------------- les noms au lieu des numéros

# L'identifiant de chaque observation pour les étapes et la mise à jour : son nom, et le rang de ce nom
# quand il revient (« S1 », puis « S1␟2 »… : un tableau en long dont la première colonne est le site),
# les observations du même nom se retrouvant dans l'ordre où elles viennent. Un tableau sans colonne
# qui les nomme a pour noms les numéros de ses lignes : ses observations se retrouvent par leur place.
cles_observations <- function(p) {
  noms <- vapply(p$observations, function(o) premier_texte(o$ids), "")
  if (!length(noms)) return(character(0))
  rang <- stats::ave(seq_along(noms), noms, FUN = seq_along)
  ifelse(rang > 1, paste0(noms, "\u001f", rang), noms)
}

# Un identifiant d'observation pour une phrase : « S1 (2) » pour la deuxième du nom.
cle_lisible <- function(k) sub("\u001f(\\d+)$", " (\\1)", k)

# Les valeurs de `q` (les paramètres d'une demande) au chemin `chemin` : « variable », ou
# « descriptions$variable » (dans chaque élément d'une liste, ou la colonne d'un data.frame : le
# JSON d'une liste d'objets) ; et `q` avec d'autres valeurs à ce chemin.
valeur_chemin <- function(q, chemin) {
  morceaux <- strsplit(chemin, "$", fixed = TRUE)[[1]]
  x <- q[[morceaux[1]]]
  if (length(morceaux) == 1 || is.null(x)) return(x)
  if (is.data.frame(x) || !is.null(names(x))) return(x[[morceaux[2]]])
  lapply(x, function(e) e[[morceaux[2]]])
}
changer_chemin <- function(q, chemin, valeurs) {
  morceaux <- strsplit(chemin, "$", fixed = TRUE)[[1]]
  if (length(morceaux) == 1) {
    q[morceaux[1]] <- list(valeurs)
    return(q)
  }
  x <- q[[morceaux[1]]]
  if (is.data.frame(x) || !is.null(names(x))) x[[morceaux[2]]] <- valeurs
  else for (i in seq_along(x)) x[[i]][morceaux[2]] <- list(valeurs[[i]])
  q[[morceaux[1]]] <- x
  q
}

# (`f`, faite pour un vecteur, appliquée à `x` ou à chaque élément de la liste `x`)
convertir_valeurs <- function(x, f) if (is.list(x)) lapply(x, f) else f(x)

# Les paramètres `q` d'une demande écrits sans numéros internes, d'après le projet `p` d'avant le
# changement et ce que `def` (ETAPES_DONNEES) dit de ses paramètres : les variables (`variables`)
# par leur nom, les observations (`observations`) par leur identifiant (cles_observations), une
# colonne qui est une variable ou un identifiant des observations (`colonnes` : un numéro, ou "id:k")
# en « var:nom » ou « id:nom ».
par_noms <- function(p, def, q) {
  noms <- stats::setNames(vapply(p$variables, function(v) as.character(v$ids[1]), ""), vapply(p$variables, function(v) as.character(v$uid), ""))
  cles <- stats::setNames(cles_observations(p), vapply(p$observations, function(o) as.character(o$uid), ""))
  ids <- as.character(unlist(p$observation_id_names %||% "Observation"))
  vers <- list(
    variables = function(x) unname(noms[as.character(x)]),
    observations = function(x) unname(cles[as.character(x)]),
    colonnes = function(x) {
      x <- as.character(x)
      id <- startsWith(x, "id:")
      r <- paste0("var:", noms[x])
      r[id] <- paste0("id:", ids[suppressWarnings(as.integer(substring(x[id], 4)))])
      unname(r)
    })
  for (genre in names(vers)) for (ch in def[[genre]]) {
    v <- valeur_chemin(q, ch)
    if (length(v)) q <- changer_chemin(q, ch, convertir_valeurs(v, vers[[genre]]))
  }
  q
}

# Les paramètres d'une étape rapportés aux numéros du projet `p` où elle est rejouée (par_noms, à
# l'envers) : list(q, notes). Une variable, une observation ou une colonne qu'elle nomme et que les
# données n'ont pas la fait sauter (etape_sautee), qui dit lesquelles ; sauf pour les paramètres que
# l'étape fait sans elles (`partielles` : les observations d'un collage, d'une classe), que `notes`
# nomme alors.
par_numeros <- function(p, def, q) {
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(p$donnees), p$variables)
  noms <- vapply(vars, function(v) as.character(v$ids[1]), "")
  garde <- !duplicated(noms, fromLast = TRUE)  # (la dernière de deux variables du même nom, comme une formule la lit)
  uid_nom <- stats::setNames(vapply(vars[garde], function(v) as.numeric(v$uid), 0), noms[garde])
  uid_cle <- stats::setNames(vapply(p$observations, function(o) as.numeric(o$uid), 0), cles_observations(p))
  ids <- as.character(unlist(p$observation_id_names %||% "Observation"))
  vers <- list(
    variables = function(x) unname(uid_nom[as.character(x)]),
    observations = function(x) unname(uid_cle[as.character(x)]),
    colonnes = function(x) {
      x <- as.character(x)
      id <- startsWith(x, "id:")
      r <- unname(as.character(uid_nom[substring(x, 5)]))
      k <- match(substring(x[id], 4), ids)
      r[id] <- ifelse(is.na(k), NA_character_, paste0("id:", k))
      r
    })
  quoi <- list(variables = traduire("variable(s) absente(s) des données", "variable(s) missing from the data"),
               observations = traduire("observation(s) absente(s) des données", "observation(s) missing from the data"),
               colonnes = traduire("colonne(s) absente(s) des données", "column(s) missing from the data"))
  notes <- character(0)
  for (genre in names(vers)) for (ch in def[[genre]]) {
    v <- valeur_chemin(q, ch)
    if (!length(v)) next
    t <- convertir_valeurs(v, vers[[genre]])
    manquent <- unique(as.character(unlist(v))[is.na(unlist(t))])
    if (length(manquent)) {
      lisibles <- if (genre == "observations") cle_lisible(manquent) else sub("^(var|id):", "", manquent)
      phrase <- sprintf(traduire("%d %s : %s", "%d %s: %s"), length(lisibles), quoi[[genre]],
                        paste0(paste(utils::head(lisibles, 5), collapse = ", "), if (length(lisibles) > 5) "…" else ""))
      if (ch %in% def$partielles) notes <- c(notes, phrase) else etape_sautee(phrase)
    }
    q <- changer_chemin(q, ch, t)
  }
  list(q = q, notes = notes)
}

# Une étape qui ne s'applique plus aux données rejouées (une variable, une observation ou une
# catégorie qu'elle nomme n'y est pas ; plus rien à remplacer) : elle est sautée, et dit pourquoi.
etape_sautee <- function(raison) stop(structure(class = c("etape_sautee", "error", "condition"), list(message = raison, call = NULL)))

# Les noms qu'une formule de Nouvelle variable lit et que le projet n'a pas : ni ses variables, ni ses
# identifiants, ni un objet de R (pi, une fonction du moteur) ; une formule qui ne se lit pas n'en a
# pas, son erreur viendra du calcul.
noms_absents_formule <- function(p, formule) {
  e <- tryCatch(parse(text = formule, keep.source = FALSE), error = function(err) NULL)
  if (is.null(e)) return(character(0))
  n <- noms_condition(p)
  Filter(function(x) !exists(x, envir = globalenv()), setdiff(all.vars(e), c(names(n$variables), n$ids)))
}

# ---------------------------------------------------------------- les étapes

# Ce qui se note de chaque demande qui change les données d'un projet (p$donnees, p$variables,
# p$observations, ses coordonnées), et comment la refaire :
#   - `variables`, `observations`, `colonnes` : les paramètres qui en tiennent des numéros (par_noms),
#     « a$b » pour le champ b de chaque élément de a ; `partielles` : ceux que l'étape fait sans les
#     observations absentes ;
#   - `fichiers` : les paramètres qui tiennent le chemin d'un fichier lu, gardé comme la source
#     (fiche_fichier) et retrouvé comme elle (fichier_retrouve) ;
#   - `langue` : la demande nomme ce qu'elle crée dans la langue de l'interface (« x (imputée) ») :
#     rejouée dans celle où elle a été faite, elle crée les mêmes noms ;
#   - `preparer(avant, q, contexte)` : ce qu'il faut garder de plus pour la rejouer (les catégories
#     d'avant, la définition d'un modèle et le projet qu'il a vu) ;
#   - `appliquer(p, q, rejeu)` : le changement, sur les paramètres d'une demande (des numéros), que la
#     demande du serveur fait aussi ; `rejeu`, quand l'étape est rejouée : list(sans_nouvelles), les
#     modèles qui ne prennent pas les nouvelles observations (modele_rejoue). Rejouée, une étape qui
#     n'a plus d'objet est sautée (etape_sautee) plutôt qu'en erreur.
ETAPES_DONNEES <- list(
  modifier_valeur = list(variables = "variable", observations = "observation",
    appliquer = function(p, q, rejeu = NULL) modifier_valeur(p, q$observation, q$variable, q$valeur)),
  variable_calculee = list(
    appliquer = function(p, q, rejeu = NULL) {
      if (!is.null(rejeu)) {
        absents <- noms_absents_formule(p, q$formule)
        if (length(absents)) etape_sautee(sprintf(traduire("%d variable(s) absente(s) des données : %s", "%d variable(s) missing from the data: %s"), length(absents), paste(absents, collapse = ", ")))
      }
      variable_calculee(p, q$nom, q$formule)
    }),
  imputer = list(variables = "variables", langue = TRUE,
    appliquer = function(p, q, rejeu = NULL) {
      obs <- as.numeric(rownames(p$donnees))
      if (!is.null(rejeu) && !any(vapply(as.numeric(unlist(q$variables)), function(u) anyNA(colonne(p, u, obs)), NA))) {
        etape_sautee(traduire("Ces variables n'ont aucune valeur manquante.", "These variables have no missing values."))
      }
      imputer_acp(p, unlist(q$variables), k = q$k, bornes = !isFALSE(q$bornes), remplacer = isTRUE(q$remplacer))
    }),
  classes_valeurs = list(variables = "variable",
    appliquer = function(p, q, rejeu = NULL) classes_valeurs(p, q$nom, q$variable, bornes = q$bornes, n = q$n)),
  # (des cellules collées, que la séance garde dans un fichier qui disparaîtra : leur texte va avec
  # l'étape, `texte`, à la place du chemin)
  ajouter_colonnes = list(colonnes = "cles_projet", fichiers = "chemin",
    preparer = function(avant, q, contexte) {
      chemin <- premier_texte(q$chemin)
      if (isTRUE(q$colle) || (nzchar(chemin) && dans_seance(chemin) && extension(chemin) %in% c("tsv", "csv", "txt") && file.size(chemin) < 5e6)) {
        q$texte <- texte_fichier(chemin)
        q$chemin <- NULL
      }
      q
    },
    appliquer = function(p, q, rejeu = NULL) {
      cles_table <- as.character(unlist(q$cles_table))
      tableau <- tableau_a_joindre(q$chemin, q$table, textes = cles_table)
      ajouter_colonnes_table(p, tableau, as.character(unlist(q$cles_projet)), cles_table, if (!is.null(q$colonnes)) as.character(unlist(q$colonnes)))
    }),
  ajouter_raster = list(fichiers = "fichiers$chemin",
    appliquer = function(p, q, rejeu = NULL) ajouter_valeurs_rasters(p, fichiers_raster_demande(q$fichiers))),
  remplacer = list(variables = "variables",
    appliquer = function(p, q, rejeu = NULL) {
      n <- remplacer_valeurs(p, q$variables, q$chercher, q$remplacer, vides = isTRUE(q$vides), partie = isTRUE(q$partie),
                             casse = isTRUE(q$casse), nombres = !isFALSE(q$nombres))
      if (!isTRUE(q$apercu) && !attr(n, "remplacement")$cellules) etape_sautee(traduire("Aucune cellule à remplacer.", "No cell to replace."))
      n
    }),
  # (rejouée, chaque catégorie prend le nom que l'étape a donné à celle de ce nom, les catégories
  # qu'elle ne connaissait pas gardant le leur : les catégories d'avant, `anciennes`, vont avec elle)
  categories = list(variables = "variable",
    preparer = function(avant, q, contexte) {
      q$anciennes <- I(as.character(variable_du_projet(avant, q$variable)$categories %||% character(0)))
      q
    },
    appliquer = function(p, q, rejeu = NULL) {
      if (is.null(rejeu) || is.null(q$anciennes)) return(changer_categories(p, q$variable, q$noms, q$ordre))
      fr <- !identical(getOption("nuee.langue"), "en")
      cats <- as.character(variable_du_projet(p, q$variable)$categories %||% character(0))
      anciennes <- as.character(unlist(q$anciennes))
      noms <- as.character(unlist(q$noms))
      absentes <- setdiff(anciennes[anciennes != noms], cats)
      nouveaux <- ifelse(cats %in% anciennes, noms[match(cats, anciennes)], cats)
      ordre <- as.character(unlist(q$ordre))
      if (length(ordre)) ordre <- c(intersect(ordre, nouveaux), setdiff(unique(nouveaux), ordre))
      if (identical(if (length(ordre)) ordre else unique(nouveaux), cats)) {
        etape_sautee(if (length(absentes)) sprintf(traduire("Catégories absentes des données : %s.", "Categories missing from the data: %s."), paste(vapply(absentes, guillemets_texte, "", fr = fr), collapse = ", "))
                     else traduire("Les catégories sont déjà celles-là.", "The categories already are these."))
      }
      n <- changer_categories(p, q$variable, nouveaux, if (length(ordre)) ordre)
      if (length(absentes)) attr(n, "note_etape") <- sprintf(traduire("Catégories absentes des données : %s.", "Categories missing from the data: %s."),
                                                         paste(vapply(absentes, guillemets_texte, "", fr = fr), collapse = ", "))
      n
    }),
  coller_cellules = list(variables = "variables", observations = "observations", partielles = "observations",
    appliquer = function(p, q, rejeu = NULL) {
      obs <- as.numeric(unlist(q$observations))
      valeurs <- q$valeurs
      if (anyNA(obs)) {
        if (is.list(valeurs) && !is.data.frame(valeurs)) {
          valeurs <- do.call(rbind, lapply(valeurs, function(l) vapply(l, function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)[1], "")))
        }
        valeurs <- as.matrix(valeurs)[!is.na(obs), , drop = FALSE]
        obs <- obs[!is.na(obs)]
        if (!length(obs)) etape_sautee(traduire("Aucune de ses observations n'est dans les données.", "None of its observations is in the data."))
      }
      coller_cellules(p, obs, q$variables, valeurs)
    }),
  decrire_variables = list(variables = "descriptions$variable", partielles = "descriptions$variable",
    appliquer = function(p, q, rejeu = NULL) {
      d <- q$descriptions
      if (is.data.frame(d)) d <- lapply(seq_len(nrow(d)), function(r) as.list(d[r, , drop = FALSE]))
      if (!is.null(rejeu)) {
        d <- Filter(function(x) !is.na(as.numeric(x$variable)[1]), d)
        if (!length(d)) etape_sautee(traduire("Aucune de ses variables n'est dans les données.", "None of its variables is in the data."))
        n <- tryCatch(decrire_variables(p, d), error = function(err) etape_sautee(conditionMessage(err)))
        return(n)
      }
      decrire_variables(p, d)
    }),
  coordonnees = list(variables = c("x", "y"),
    appliquer = function(p, q, rejeu = NULL) regler_coordonnees(p, q$x, q$y, q$systeme)),
  classe_selection = list(observations = "observations", partielles = "observations",
    appliquer = function(p, q, rejeu = NULL) {
      obs <- as.numeric(unlist(q$observations))
      if (!is.null(rejeu)) {
        obs <- obs[!is.na(obs)]
        if (!length(obs)) etape_sautee(traduire("Aucune de ses observations n'est dans les données.", "None of its observations is in the data."))
      }
      ajouter_classe(p, q$variable, q$etiquette, obs)
    }),
  # (les deux variables 0/1 du modèle mixte BACI, quand il les ajoute : comparaisons.R)
  baci_modele = list(colonnes = c("temps", "unite"), langue = TRUE,
    preparer = function(avant, q, contexte) q[intersect(c("temps", "unite", "impact", "temoins", "evenement"), names(q))],
    appliquer = function(p, q, rejeu = NULL) variables_baci(p, q$temps, q$unite, as.character(unlist(q$impact)), as.character(unlist(q$temoins)), q$evenement)$projet),
  # (les scores d'un modèle, les groupes de k-moyennes : rejoués, le modèle est ajusté à nouveau sur
  # les données rejouées jusque-là, modele_rejoue)
  scores_en_variables = list(
    preparer = function(avant, q, contexte) etape_modele(avant, contexte$definition),
    appliquer = function(p, q, rejeu = NULL) scores_en_variables(p, modele_rejoue(p, q, rejeu))$projet),
  kmeans_variable = list(
    preparer = function(avant, q, contexte) c(list(nom = premier_texte(q$nom)), etape_modele(avant, contexte$definition)),
    appliquer = function(p, q, rejeu = NULL) {
      m <- modele_rejoue(p, q, rejeu)
      ajouter_colonne(p, premier_texte(q$nom), m$observations, m$groupe, categories = as.character(seq_len(m$k)))
    })
)

# Le changement d'une demande (serveur.R) ou d'une étape rejouée : sa fonction de ETAPES_DONNEES.
appliquer_etape <- function(nom, p, q, rejeu = NULL) ETAPES_DONNEES[[nom]]$appliquer(p, q, rejeu)

# Ce qu'une étape qui ajuste un modèle garde pour le refaire : le nom du modèle, sa définition telle
# qu'il a été ajusté, et le projet qu'il a vu (squelette_projet), en JSON l'un et l'autre, écrits et
# relus comme ceux d'un fichier de projet.
etape_modele <- function(avant, d) {
  d$demande <- NULL
  list(modele = premier_texte(d$name),
       definition = as.character(jsonlite::toJSON(d, auto_unbox = TRUE, null = "null", na = "null", digits = NA)),
       squelette = as.character(jsonlite::toJSON(squelette_projet(avant), auto_unbox = TRUE, null = "null", na = "null", digits = NA)))
}

# Ce qu'il faut d'un projet pour y rapporter des données rejouées (aligner_projet) : ses variables
# (numéro, nom, catégories), ses observations (numéro, nom) et les noms de ses identifiants.
squelette_projet <- function(p) {
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(p$donnees), p$variables)
  list(variables = lapply(vars, function(v) list(uid = as.numeric(v$uid), nom = as.character(v$ids[1]), categories = I(as.character(unlist(v$categories))))),
       observations = list(uid = I(vapply(p$observations, function(o) as.numeric(o$uid), 0)), nom = I(vapply(p$observations, function(o) premier_texte(o$ids), ""))),
       identifiants = I(as.character(unlist(p$observation_id_names %||% "Observation"))))
}

# Le projet d'un squelette (squelette_projet, relu du JSON), sans données, avec le modèle `d`.
projet_squelette <- function(s, d) {
  vars <- lapply(s$variables, function(v) list(uid = as.numeric(v$uid), ids = as.character(v$nom),
                                               categories = if (length(v$categories)) as.character(unlist(v$categories))))
  uid <- as.numeric(unlist(s$observations$uid))
  nom <- as.character(unlist(s$observations$nom))
  p <- list(variables = vars, observations = lapply(seq_along(uid), function(i) list(uid = uid[i], ids = nom[i])),
            observation_id_names = as.character(unlist(s$identifiants)), datasets = list(), models = list(d),
            donnees = matrix(numeric(0), 0, length(vars), dimnames = list(NULL, vapply(vars, function(v) as.character(v$uid), ""))),
            noms_obs = stats::setNames(nom, as.character(uid)), noms_var = stats::setNames(vapply(vars, `[[`, "", "ids"), vapply(vars, function(v) as.character(v$uid), "")))
  class(p) <- "projet_nuee"
  p
}

# Le modèle d'une étape (etape_modele) ajusté de nouveau sur les données rejouées `p` : elles sont
# rapportées au projet qu'il avait vu (aligner_projet), sa définition suit leurs observations (les
# nouvelles y entrent, sauf pour les modèles de rejeu$sans_nouvelles), puis il est ajusté ; ses
# observations sont rendues dans la numérotation de `p`. Une variable qu'il utilise et que les
# données n'ont plus : l'étape est sautée.
modele_rejoue <- function(p, q, rejeu = NULL) {
  if (is.null(q$definition)) stop(traduire("Cette étape n'a pas la définition de son modèle.", "This step lacks its model's definition."), call. = FALSE)
  d <- jsonlite::fromJSON(as.character(q$definition), simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
  s <- jsonlite::fromJSON(as.character(q$squelette), simplifyVector = FALSE)
  a <- aligner_projet(projet_squelette(s, d), p, rejeu$sans_nouvelles)
  manquent <- intersect(variables_definition(d), a$absentes)
  if (length(manquent)) {
    noms <- vapply(s$variables, function(v) if (as.numeric(v$uid) %in% manquent) as.character(v$nom) else NA_character_, "")
    etape_sautee(sprintf(traduire("Le modèle %s utilise des variables absentes des données : %s.", "The model %s uses variables missing from the data: %s."),
                         premier_texte(d$name), paste(noms[!is.na(noms)], collapse = ", ")))
  }
  d <- a$projet$models[[1]]
  if (!isTRUE(as.numeric(d$A) > 0) && !sans_composantes(d)) d$A <- max(1, autoajuster(a$projet, d))
  m <- ajuster(a$projet, d)
  m$observations <- unname(a$vers_nouveau[as.character(m$observations)])
  m
}

# Le projet `apres` (le changement d'une demande fait sur `avant`) avec l'étape qui le refait : la
# demande `demande` (un nom de ETAPES_DONNEES), ses paramètres `q` sans numéros internes (par_noms,
# sans `projet` ni `apercu`), les fichiers qu'elle a lus, son étiquette (celle d'Annuler) et sa date.
# `contexte` : ce que la demande sait de plus (la définition d'un modèle ajusté). L'empreinte des
# données devient celle d'après l'étape, sauf quand elles avaient changé autrement depuis la
# précédente (donnees_conformes) : rompue, elle le reste.
noter_etape <- function(avant, apres, demande, q, etiquette, contexte = NULL) {
  def <- ETAPES_DONNEES[[demande]] %||% stop(sprintf("Étape inconnue : %s", demande), call. = FALSE)
  q <- q[setdiff(names(q), c("projet", "apercu"))]
  if (!is.null(def$preparer)) q <- def$preparer(avant, q, contexte)
  fiches <- list()
  for (ch in def$fichiers) for (f in as.character(unlist(valeur_chemin(q, ch)))) fiches[[length(fiches) + 1]] <- fiche_fichier(f)
  q <- par_noms(avant, def, q)
  e <- list(demande = demande, etiquette = premier_texte(etiquette), date = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            langue = getOption("nuee.langue") %||% "fr",
            parametres = as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA)))
  if (length(fiches)) e$fichiers <- fiches
  apres$etapes <- c(avant$etapes %||% list(), list(e))
  apres$empreinte_donnees <- if (isFALSE(donnees_conformes(avant))) "" else empreinte_donnees(apres)
  apres
}

# (un projet sans les attributs qu'une demande ajoute pour dire ce qu'elle a fait)
sans_attributs <- function(p) {
  a <- attributes(p)
  attributes(p) <- a[intersect(names(a), c("names", "class"))]
  p
}

# Les paramètres d'une étape rejouée avec les fichiers qu'elle lit : chacun retrouvé d'après sa fiche
# (fichier_retrouve, depuis `dossier`), ou les cellules collées qu'elle garde écrites dans un fichier
# de la séance. Un fichier qui n'est plus là : une erreur qui le nomme.
fichiers_rejoues <- function(e, def, q, dossier) {
  if (!is.null(q$texte)) {
    f <- file.path(tempfile("colle"), "tableau colle.tsv")
    dir.create(dirname(f))
    writeLines(enc2utf8(as.character(q$texte)), f, useBytes = TRUE)
    q$chemin <- f
    q$texte <- NULL
    return(q)
  }
  fiches <- e$fichiers %||% list()
  k <- 0
  retrouver <- function(x) vapply(as.character(x), function(f) {
    k <<- k + 1
    trouve <- if (k <= length(fiches)) fichier_retrouve(fiches[[k]], dossier)
    if (is.null(trouve)) stop(sprintf(traduire("Fichier introuvable : %s", "File not found: %s"), f), call. = FALSE)
    trouve
  }, "", USE.NAMES = FALSE)
  for (ch in def$fichiers) {
    v <- valeur_chemin(q, ch)
    if (length(v)) q <- changer_chemin(q, ch, convertir_valeurs(v, retrouver))
  }
  q
}

# Les étapes `etapes` (celles d'un projet, p$etapes) rejouées dans l'ordre sur le projet `projet`,
# tout juste importé : chacune traduite dans ses numéros (par_numeros), ses fichiers retrouvés depuis
# `dossier` (celui du fichier du projet), puis faite. Chacune dit ce qu'elle a donné (`statut`) :
# "ok", "sautee" (une observation ou une variable qu'elle nomme n'y est pas, une catégorie qu'elle
# renomme est absente, plus rien à faire : `raison`) ou "erreur" (son message) ; une étape sautée ou
# en erreur n'arrête pas les autres. `sans_nouvelles` : les modèles qui ne prennent pas les
# nouvelles observations, pour les étapes qui ajustent un modèle. list(projet, etapes).
rejouer_etapes <- function(projet, etapes, dossier = NULL, sans_nouvelles = NULL) {
  resultats <- vector("list", length(etapes))
  for (k in seq_along(etapes)) {
    e <- etapes[[k]]
    r <- list(n = k, demande = premier_texte(e$demande), etiquette = premier_texte(e$etiquette), statut = "ok", raison = "", notes = I(character(0)))
    essai <- tryCatch({
      def <- ETAPES_DONNEES[[r$demande]]
      if (is.null(def)) stop(sprintf(traduire("Étape inconnue de cette version de Nuée : %s", "Step unknown to this version of Nuée: %s"), r$demande), call. = FALSE)
      q <- jsonlite::fromJSON(premier_texte(e$parametres, "{}"), simplifyVector = TRUE)
      t <- par_numeros(projet, def, q)
      q <- fichiers_rejoues(e, def, t$q, dossier)
      ancienne <- if (isTRUE(def$langue) && nzchar(premier_texte(e$langue))) options(nuee.langue = premier_texte(e$langue))
      n <- tryCatch(def$appliquer(projet, q, list(sans_nouvelles = sans_nouvelles)), finally = if (!is.null(ancienne)) options(ancienne))
      list(projet = sans_attributs(n), notes = c(t$notes, attr(n, "note_etape")))
    }, error = function(err) err)
    if (inherits(essai, "error")) {
      r$statut <- if (inherits(essai, "etape_sautee")) "sautee" else "erreur"
      r$raison <- conditionMessage(essai)
    } else {
      projet <- essai$projet
      r$notes <- I(as.character(essai$notes))
    }
    resultats[[k]] <- r
  }
  list(projet = projet, etapes = resultats)
}

# ---------------------------------------------------------------- l'alignement

# Les variables (leurs numéros) qu'utilise une définition de modèle : celles de ses termes X et Y (et
# du modèle complet d'une sélection par l'AIC), sa classe, ses courbes, son décalage, son lissage
# spatial, celle de son groupe de modèles de classe, ses covariables (options$z), la réponse, le
# temps et l'unité d'un modèle BACI, et celles des groupements de sa validation croisée, de ses
# effets aléatoires, de son autocorrélation et des réglages d'une autre analyse
# (list(by = "variable", variable = …)).
variables_definition <- function(d) {
  groupements <- function(x) if (is.list(x)) c(if (identical(premier_texte(x$by), "variable")) x$variable, x$pentes, unlist(lapply(x, groupements)))
  u <- c(unlist(lapply(c(d$x, d$y, d$termes_complets), function(t) t$variables)), d$classe, d$lisses, d$decalage, d$lisse_spatial$variables,
         d$groupe_classes$variable, names(d$k_courbes), names(d$par_courbes), unlist(d$par_courbes), d$options$z,
         unlist(lapply(d[intersect(c("groupes_vc", "aleatoires", "correlation", "options"), names(d))], groupements)))
  b <- as.character(unlist(d$baci_origine[c("y", "temps", "unite")]))
  u <- suppressWarnings(as.numeric(c(as.character(unlist(u)), b[!startsWith(b, "id:")])))
  unique(u[!is.na(u)])
}

# Les identifiants des observations (leurs noms) qu'utilise une définition : ceux des groupements de
# sa validation croisée, de ses effets aléatoires, de son autocorrélation et des réglages d'une autre
# analyse (list(by = "id", id = …)), le temps et l'unité d'un modèle BACI ("id:k", du projet `p`).
identifiants_definition <- function(d, p) {
  groupements <- function(x) if (is.list(x)) c(if (identical(premier_texte(x$by), "id")) x$id, unlist(lapply(x, groupements)))
  ids <- as.character(unlist(lapply(d[intersect(c("groupes_vc", "aleatoires", "correlation", "options"), names(d))], groupements)))
  noms <- as.character(unlist(p$observation_id_names))
  for (x in as.character(unlist(d$baci_origine[c("temps", "unite")]))) if (startsWith(x, "id:")) ids <- c(ids, noms[suppressWarnings(as.integer(substring(x, 4)))])
  unique(ids[!is.na(ids)])
}

# Celles des observations `nouvelles` (des numéros de `p`) qu'un modèle de définition `d` prend : une
# PLS-DA, celles dont la classe est connue (comme definir_modele) ; un modèle de classe, celles de sa
# classe ; le modèle mixte BACI, celles des unités d'impact et témoins dont le temps est connu,
# jusqu'à la fin (comme definition_baci) ; un autre modèle, toutes.
nouvelles_admises <- function(d, p, nouvelles) {
  if (!length(nouvelles)) return(nouvelles)
  # (la classe d'une PLS-DA d'un fichier .usp : la variable de ses termes Y)
  classe <- d$classe %||% (if (isTRUE(d$plsda)) unlist(lapply(d$y, function(t) t$variables))[1])
  if (isTRUE(d$plsda) && length(classe)) nouvelles <- nouvelles[!is.na(colonne(p, classe, nouvelles))]
  g <- d$groupe_classes
  if (!is.null(g)) {
    v <- tryCatch(variable_du_projet(p, g$variable), error = function(e) NULL)
    x <- colonne(p, g$variable, nouvelles)
    nouvelles <- nouvelles[!is.na(x) & as.character(v$categories)[x] %in% as.character(g$classe)]
  }
  b <- d$baci_origine
  if (!is.null(b)) {
    garde <- tryCatch({
      temps <- if (startsWith(as.character(b$temps)[1], "id:")) as.character(b$temps)[1] else as.numeric(b$temps)[1]
      unite <- if (startsWith(as.character(b$unite)[1], "id:")) as.character(b$unite)[1] else as.numeric(b$unite)[1]
      vt <- valeurs_a_tester(p, temps, nouvelles)
      tv <- as.vector(temps_en_nombres(if (vt$qualitative) as.character(vt$valeurs) else vt$valeurs))
      u <- as.character(valeurs_a_tester(p, unite, nouvelles)$valeurs)
      fin <- as.numeric(unlist(b$fin))
      u %in% as.character(unlist(c(b$impact, b$temoins))) & !is.na(tv) & (if (length(fin)) tv <= fin[1] else TRUE)
    }, error = function(e) rep(FALSE, length(nouvelles)))
    nouvelles <- nouvelles[garde %in% TRUE]
  }
  nouvelles
}

# La définition `d` d'un modèle après une mise à jour des données : les observations retirées la
# quittent (ses observations, celles cochées sous sa condition, son jeu de test, ses observations
# exclues) ; avec `prend`, les nouvelles qu'il admet (nouvelles_admises) y entrent, parmi les cochées
# quand une condition choisit ses observations (condition.R : refaite à l'ajustement). Celles qu'il
# avait laissées de côté le restent : la mise à jour garde leurs numéros. Des groupes de validation
# croisée venus d'un fichier .usp n'en ont pas pour de nouvelles observations : elles n'y entrent pas.
# Changées, ses observations ne sont plus celles d'un fichier .usp : sa solution enregistrée part,
# comme quand on le réajuste sans des observations (serveur.R : reajuster_demande).
definition_suivie <- function(d, p, nouvelles, retirees, prend = TRUE) {
  nombres <- function(x) as.numeric(unlist(x))
  garder <- function(x) { x <- nombres(x); x[!x %in% retirees] }
  avant <- lapply(d[c("observations", "cochees", "test")], nombres)
  d$observations <- garder(d$observations)
  if (!is.null(d$cochees)) d$cochees <- garder(d$cochees)
  if (length(d$test)) d$test <- garder(d$test)
  if (length(d$exclues)) d$exclues <- garder(d$exclues)
  vc <- d$groupes_vc
  if (identical(premier_texte(vc$by), "groups")) {
    k <- !nombres(vc$observations) %in% retirees
    d$groupes_vc$observations <- nombres(vc$observations)[k]
    d$groupes_vc$groups <- nombres(vc$groups)[k]
    prend <- FALSE
  }
  if (prend) {
    entrent <- setdiff(nouvelles_admises(d, p, nouvelles), c(nombres(d$observations), nombres(d$cochees), nombres(d$test)))
    if (avec_condition(d)) d$cochees <- c(nombres(d$cochees %||% d$observations), entrent)
    else d$observations <- c(d$observations, entrent)
  }
  if (!identical(avant, lapply(d[c("observations", "cochees", "test")], nombres))) {
    d$solution <- NULL
    d$reference <- list(ynames = d[["reference"]][["ynames"]], note = d[["reference"]][["note"]])
  }
  d
}

# (la variable `de` du projet prend le numéro `vers`)
renumeroter_variable <- function(p, de, vers) {
  i <- which(vapply(p$variables, function(v) identical(as.numeric(v$uid), as.numeric(de)), TRUE))
  p$variables[[i]]$uid <- vers
  colnames(p$donnees)[colnames(p$donnees) == as.character(de)] <- as.character(vers)
  names(p$noms_var)[names(p$noms_var) == as.character(de)] <- as.character(vers)
  p$datasets <- lapply(p$datasets, function(d) { d$variables[as.numeric(d$variables) == as.numeric(de)] <- vers; d })
  p
}

# Les données d'un projet rejoué (`nouveau`) dans la numérotation du projet `actuel` :
#   - une variable du même nom (à une capitale, une espace ou un point près : nom_comparable, quand
#     un seul nom de chaque côté s'y rapporte) garde son numéro et son nom ; une nouvelle en prend un ;
#     une variable qualitative garde ses catégories, dans leur ordre (la dernière catégorie observée
#     est la référence d'une régression), les nouvelles venant après, et une variable qui a changé
#     de type dans le fichier reprend le sien (des nombres lus en catégories, ou l'inverse) ;
#   - une observation du même identifiant (cles_observations) garde son numéro ; une nouvelle en prend
#     un ; une absente part ; les identifiants des observations gardent leurs rangs, ceux du fichier
#     qui sont nouveaux venant après ;
#   - une variable du projet que les données n'ont plus : celle d'une formule de Nouvelle variable
#     (variables.R : variable_calculee) est calculée de nouveau sur elles, sous son numéro ; les
#     autres partent, et un modèle qui en utilise une ne se réajuste plus (le rapport les nomme) ;
#   - les définitions des modèles suivent les observations (definition_suivie : les nouvelles y
#     entrent, sauf pour les modèles de `sans_nouvelles`, leurs noms ou leurs numéros).
# Le reste du projet (ses modèles, documents, figures, réglages) est celui d'`actuel`. Rend
# list(projet, rapport : ce qui a changé ; vers_nouveau : le numéro dans `nouveau` de chaque
# observation, nommé par le sien ; absentes : les numéros des variables parties).
aligner_projet <- function(actuel, nouveau, sans_nouvelles = NULL) {
  nom <- function(v) as.character(v$ids[1])
  uid <- function(x) as.numeric(x$uid)
  avec <- function(p) Filter(function(v) as.character(v$uid) %in% colnames(p$donnees), p$variables)
  va <- avec(actuel)
  sans_donnees <- Filter(function(v) !as.character(v$uid) %in% colnames(actuel$donnees), actuel$variables)
  vn <- avec(nouveau)
  noms_a <- vapply(va, nom, "")
  noms_n <- vapply(vn, nom, "")
  k <- match(noms_n, noms_a)
  if (anyNA(k)) {
    ca <- nom_comparable(noms_a)
    cn <- nom_comparable(noms_n)
    libres <- setdiff(seq_along(va), k)
    for (i in which(is.na(k))) {
      j <- libres[ca[libres] == cn[i]]
      if (length(j) == 1 && sum(cn[is.na(k)] == cn[i]) == 1) {
        k[i] <- j
        libres <- setdiff(libres, j)
      }
    }
  }
  uid_v <- vapply(va, uid, 0)[k]
  uid_v[is.na(k)] <- max(c(0, vapply(actuel$variables, uid, 0))) + seq_len(sum(is.na(k)))
  # (les observations)
  uo_a <- vapply(actuel$observations, uid, 0)
  uo_n <- vapply(nouveau$observations, uid, 0)
  m <- match(cles_observations(nouveau), cles_observations(actuel))
  uid_o <- uo_a[m]
  uid_o[is.na(m)] <- max(c(0, uo_a)) + seq_len(sum(is.na(m)))
  ajoutees <- uid_o[is.na(m)]
  retirees <- setdiff(uo_a, uid_o)
  # (les valeurs, et les catégories rapportées à celles du projet)
  X <- nouveau$donnees[as.character(uo_n), vapply(vn, function(v) as.character(v$uid), ""), drop = FALSE]
  vars <- vector("list", length(vn))
  categories <- list()
  types <- list()
  for (i in seq_along(vn)) {
    v <- vn[[i]]
    if (!is.na(k[i])) {
      a <- va[[k[i]]]
      ca <- as.character(unlist(a$categories))
      cn <- as.character(unlist(v$categories))
      x <- X[, i]
      neuves <- character(0)
      if (length(ca) && length(cn)) {
        neuves <- setdiff(cn, ca)
        v$categories <- c(ca, neuves)
        X[, i] <- match(cn[x], v$categories)
      } else if (length(ca)) {
        texte <- ifelse(is.na(x), NA_character_, sprintf("%.15g", x))
        neuves <- sort(setdiff(unique(stats::na.omit(texte)), ca))
        v$categories <- c(ca, neuves)
        X[, i] <- match(texte, v$categories)
        types[[length(types) + 1]] <- list(variable = nom(a), type = "qualitative", perdues = 0L)
      } else if (length(cn)) {
        n <- lire_nombres(cn)[x]
        X[, i] <- n
        v["categories"] <- list(NULL)
        types[[length(types) + 1]] <- list(variable = nom(a), type = "quantitative", perdues = sum(!is.na(x) & is.na(n)))
      }
      if (length(neuves)) categories[[length(categories) + 1]] <- list(variable = nom(a), nouvelles = I(neuves))
      v$ids[1] <- nom(a)
      for (champ in c("unite", "description")) if (is.null(v[[champ]]) && !is.null(a[[champ]])) v[[champ]] <- a[[champ]]
    }
    v$uid <- uid_v[i]
    vars[[i]] <- v
  }
  # (les identifiants des observations : ceux du projet à leurs rangs, puis les nouveaux)
  noms_id_a <- as.character(unlist(actuel$observation_id_names %||% "Observation"))
  noms_id_n <- as.character(unlist(nouveau$observation_id_names %||% "Observation"))
  noms_id <- c(noms_id_a, setdiff(noms_id_n[-1], noms_id_a))
  rangs <- match(noms_id, noms_id_n)
  rangs[1] <- 1L
  obs <- lapply(seq_along(uo_n), function(i) {
    ids <- as.character(unlist(nouveau$observations[[i]]$ids))
    valeurs <- ids[rangs]
    valeurs[-1][is.na(valeurs[-1])] <- ""
    list(uid = uid_o[i], ids = valeurs)
  })
  # (le projet rapporté : les données de `nouveau`, le reste d'`actuel`)
  mv <- stats::setNames(uid_v, vapply(vn, function(v) as.character(v$uid), ""))
  mo <- stats::setNames(uid_o, as.character(uo_n))
  p <- actuel
  p$variables <- c(vars, sans_donnees)
  p$observations <- obs
  p$observation_id_names <- noms_id
  p$datasets <- lapply(nouveau$datasets, function(d) {
    d$values <- NULL
    o <- unname(mo[as.character(unlist(d$observations))])
    w <- unname(mv[as.character(unlist(d$variables))])
    d$observations <- o[!is.na(o)]
    d$variables <- w[!is.na(w)]
    d
  })
  dimnames(X) <- list(as.character(uid_o), as.character(uid_v))
  p$donnees <- X
  p$noms_obs <- stats::setNames(vapply(obs, function(o) as.character(o$ids[1]), ""), as.character(uid_o))
  p$noms_var <- stats::setNames(vapply(p$variables, nom, ""), vapply(p$variables, function(v) as.character(v$uid), ""))
  co <- nouveau$coordonnees
  co <- if (is.list(co) && length(co$x) && length(co$y)) list(x = unname(mv[as.character(co$x)]), y = unname(mv[as.character(co$y)]), systeme = co$systeme)
  p["coordonnees"] <- list(if (!is.null(co) && !anyNA(c(co$x, co$y))) co
                           else if (is.list(actuel$coordonnees) && all(as.numeric(c(actuel$coordonnees$x, actuel$coordonnees$y)) %in% uid_v)) actuel$coordonnees)
  if (!is.null(nouveau$collecte)) p$collecte <- nouveau$collecte
  # (les variables qui manquent : celles d'une formule sont calculées de nouveau, sous leur numéro)
  absentes <- va[setdiff(seq_along(va), k)]
  recalculees <- character(0)
  parties <- list()
  for (a in absentes) {
    formule <- if (identical(premier_texte(a$origine$type), "formule")) premier_texte(a$formula) else ""
    essai <- if (nzchar(formule)) tryCatch(variable_calculee(p, nom(a), formule), error = function(e) NULL)
    if (is.null(essai)) {
      parties[[length(parties) + 1]] <- a
      next
    }
    j <- match(nom(a), vapply(essai$variables, nom, ""))
    p <- renumeroter_variable(essai, uid(essai$variables[[j]]), uid(a))
    j <- match(nom(a), vapply(p$variables, nom, ""))
    for (champ in c("unite", "description")) if (!is.null(a[[champ]])) p$variables[[j]][[champ]] <- a[[champ]]
    recalculees <- c(recalculees, nom(a))
  }
  absentes_uid <- vapply(parties, uid, 0)
  # (les modèles : leurs observations suivent)
  exclus <- as.character(unlist(sans_nouvelles))
  prend <- function(d) !(premier_texte(d$name) %in% exclus || premier_texte(d$number) %in% exclus)
  p$models <- lapply(actuel$models, function(d) definition_suivie(d, p, ajoutees, retirees, prend(d)))
  p$prediction_sets <- lapply(actuel$prediction_sets, function(s) { s$observations <- setdiff(as.numeric(unlist(s$observations)), retirees); s })
  # (ce qui a changé : les cellules des observations et des variables communes, par variable ; celles
  # que des étapes calculent sur toutes les observations, une imputation, des scores, changent avec
  # les nouvelles)
  communes <- which(!is.na(m))
  lignes_a <- as.character(uo_a[m[communes]])
  ici <- lignes_a %in% rownames(actuel$donnees)
  changees <- list()
  for (i in which(!is.na(k))) {
    if (!any(ici)) break
    a <- va[[k[i]]]
    xa <- unname(actuel$donnees[lignes_a[ici], as.character(uid(a))])
    xn <- unname(X[communes[ici], i])
    ca <- as.character(unlist(a$categories))
    n <- if (length(ca)) {
      xa <- ca[xa]
      xn <- as.character(unlist(vars[[i]]$categories))[xn]
      sum(xor(is.na(xa), is.na(xn)) | (!is.na(xa) & !is.na(xn) & xa != xn))
    } else {
      sum(xor(is.na(xa), is.na(xn)) | (!is.na(xa) & !is.na(xn) & abs(xa - xn) > 1e-12 * pmax(1, abs(xa))))
    }
    if (n) changees[[length(changees) + 1]] <- list(variable = nom(a), cellules = n)
  }
  cellules <- sum(vapply(changees, function(x) as.numeric(x$cellules), 0))
  positions <- function(q) {
    n <- vapply(q$observations, function(o) premier_texte(o$ids), "")
    length(n) > 0 && identical(n, as.character(seq_along(n)))
  }
  noms_obs_a <- stats::setNames(vapply(actuel$observations, function(o) premier_texte(o$ids), ""), as.character(uo_a))
  id_absents <- Filter(function(n) any(nzchar(identifiants_observations(actuel, match(n, noms_id_a), uo_a), keepNA = TRUE), na.rm = TRUE), setdiff(noms_id_a[-1], noms_id_n))
  touches <- Filter(Negate(is.null), lapply(actuel$models, function(d) {
    u <- intersect(variables_definition(d), absentes_uid)
    ids <- intersect(identifiants_definition(d, actuel), id_absents)
    if (length(u) || length(ids)) list(nom = premier_texte(d$name), numero = d$number, variables = I(c(vapply(Filter(function(v) uid(v) %in% u, parties), nom, ""), ids)))
  }))
  choisies <- Filter(Negate(is.null), lapply(actuel$models, function(d) {
    admises <- nouvelles_admises(d, actuel, uo_a)
    dedans <- as.numeric(unlist(c(if (avec_condition(d)) d$cochees %||% d$observations else d$observations, d$test)))
    if (length(setdiff(admises, dedans))) list(nom = premier_texte(d$name), numero = d$number, observations = length(intersect(dedans, uo_a)), admises = length(admises))
  }))
  sans <- vapply(Filter(function(d) !prend(d) || identical(premier_texte(d$groupes_vc$by), "groups"), actuel$models), function(d) premier_texte(d$name), "")
  rapport <- list(
    observations = list(ajoutees = length(ajoutees), retirees = length(retirees),
                        premieres_ajoutees = I(utils::head(unname(p$noms_obs[as.character(ajoutees)]), 10)),
                        premieres_retirees = I(utils::head(unname(noms_obs_a[as.character(retirees)]), 10)),
                        cellules = cellules, cellules_par_variable = changees, par_position = positions(actuel) && positions(nouveau),
                        repetees = sum(duplicated(noms_obs_a))),
    variables = list(nouvelles = I(noms_n[is.na(k)]), absentes = I(vapply(parties, nom, "")), recalculees = I(recalculees),
                     categories = categories, types = types),
    identifiants = list(nouveaux = I(setdiff(noms_id_n[-1], noms_id_a)), absents = I(id_absents)),
    modeles = list(variables_absentes = touches, observations_choisies = choisies, sans_nouvelles = I(sans)))
  list(projet = p, rapport = rapport, vers_nouveau = stats::setNames(uo_n, as.character(uid_o)), absentes = absentes_uid)
}

# ---------------------------------------------------------------- la mise à jour

# Les données du projet `actuel` mises à jour : le fichier `chemin` (par défaut sa source, retrouvée
# depuis `dossier`, le dossier du fichier du projet ; `table`, sa feuille ou sa table, celle de la
# source par défaut) lu par le lecteur de sa source avec les mêmes décisions sur ses colonnes
# (importer_source ; sans source, comme Fichier › Ouvrir le lit), les étapes `etapes` rejouées
# (rejouer_etapes), le résultat rapporté au projet (aligner_projet). Rend list(projet : le projet
# mis à jour, sa source le fichier lu, ses étapes et l'empreinte de ses données ; rapport : ce qui a
# changé, avec ce qu'a donné chaque étape (`etapes`), le fichier lu (`fichier`), ce qu'il faut savoir
# de sa lecture (`notes`) et si ses observations se retrouvent par leur place, sans colonne qui les
# nomme (`par_position`)). Un fichier de plusieurs tableaux sans table choisie : list(choisir, chemin).
mise_a_jour <- function(actuel, chemin = NULL, table = NULL, sans_nouvelles = NULL, dossier = dossier_projet(actuel), etapes = actuel$etapes %||% list()) {
  s <- actuel$source_donnees
  chemin <- premier_texte(chemin)
  if (!nzchar(chemin)) {
    if (is.null(s)) stop(traduire("Ce projet n'a pas de fichier source : donnez le fichier à lire.", "This project has no source file: give the file to read."), call. = FALSE)
    chemin <- fichier_retrouve(s, dossier) %||%
      stop(sprintf(traduire("Le fichier source est introuvable : %s", "The source file isn't found: %s"), premier_texte(s$chemin)), call. = FALSE)
  }
  if (!file_test("-f", chemin)) stop(sprintf(traduire("Fichier introuvable : %s", "File not found: %s"), chemin), call. = FALSE)
  lecteur <- if (!is.null(s)) premier_texte(s$lecteur, "tableau") else lecteur_fichier(chemin)
  table <- if (length(table) && nzchar(premier_texte(table))) premier_texte(table)
  if (is.null(table) && lecteur == "tableau") {
    tables <- tables_fichier(chemin)
    if (nzchar(premier_texte(s$table)) && (premier_texte(s$table) %in% tables || !length(tables))) table <- premier_texte(s$table)
    else if (length(tables) > 1) return(list(choisir = I(tables), chemin = normalizePath(chemin, winslash = "/")))
  } else if (is.null(table) && lecteur == "archive") table <- if (nzchar(premier_texte(s$table))) premier_texte(s$table)
  collecte <- NULL
  if (lecteur == "collecte") {
    collecte <- if (!is.null(s)) s$collecte else list()
    if (is.list(s$formulaire)) {
      collecte$formulaire <- fichier_retrouve(s$formulaire, dossier) %||%
        stop(sprintf(traduire("Le formulaire XLSForm de l'import est introuvable : %s", "The import's XLSForm form isn't found: %s"), premier_texte(s$formulaire$chemin)), call. = FALSE)
    }
  }
  brut <- importer_source(chemin, lecteur, table, if (!is.null(s)) s$reglages else list(), collecte)
  reglages <- attr(brut, "reglages")
  notes <- as.character(unlist(brut$note))
  # (les réglages que l'import d'un export de KoboToolbox ou d'ODK a pris, ceux par défaut compris)
  if (lecteur == "collecte") collecte <- brut$import_collecte %||% collecte
  brut$note <- brut$import_collecte <- NULL
  r <- rejouer_etapes(sans_attributs(brut), etapes, dossier, sans_nouvelles)
  a <- aligner_projet(actuel, r$projet, sans_nouvelles)
  n <- a$projet
  n$etapes <- etapes
  n$source_donnees <- source_fichier(chemin, lecteur, table, reglages, collecte)
  n$empreinte_donnees <- empreinte_donnees(n)
  rapport <- a$rapport
  rapport$observations$par_position <- isTRUE(rapport$observations$par_position) || identical(premier_texte(reglages$identifiant, NA_character_), "")
  rapport$etapes <- r$etapes
  rapport$fichier <- list(chemin = normalizePath(chemin, winslash = "/"), table = table, lecteur = lecteur)
  rapport$notes <- I(notes)
  list(projet = n, rapport = rapport)
}
