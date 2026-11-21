# Lecture d'un projet : fichier .json écrit par export_project.py (projet .usp converti) ou
# enregistré par l'application.
#
# Un projet contient :
#   - les observations et les variables (identifiants, catégories des variables qualitatives) ;
#   - les jeux de données, réunis ici dans une seule matrice `donnees`
#     (lignes = observations, colonnes = variables, repérées par leur numéro interne) ;
#   - la définition de chaque modèle (observations retenues, termes X et Y, transformation
#     et mise à l'échelle de chaque terme) et les résultats enregistrés dans le fichier.
#
# Un projet enregistré par l'application dit aussi ce qui l'a écrit (j$nuee : format, version de
# Nuée, de R et des paquets qui calculent les modèles). FORMAT_PROJET augmente quand une version
# de Nuée écrit des projets que les précédentes ne liraient pas en entier : elles le voient au
# format du fichier. 1 : jusqu'à 0.9.9 ; 2 : la 1.0 (les coordonnées du projet, le lissage
# spatial d'un GAM, de nouveaux types d'analyses, qu'une version plus ancienne ne refait pas ;
# les figures composées, p$figures, qu'elle garde sans les montrer) ; 3 : la 1.1, les rapports
# vivants (etapes.R) : d'où viennent les données (p$source_donnees : le fichier, sa table, ce que
# l'import a décidé de ses colonnes, sa taille, sa date et son empreinte), chaque changement fait
# à elles, en étapes qui se rejouent (p$etapes, leurs paramètres en texte JSON), et l'empreinte
# des données après la dernière (p$empreinte_donnees) ; et le modèle écrit en code R (type CODE,
# code.R : son code dans d$options$code, les colonnes qu'il nomme dans d$options$colonnes), et celui
# d'un greffon (type PLUGIN, greffons.R : le greffon gardé en entier dans d$options$greffon, son
# manifeste et ses fichiers R, ses entrées et ses options dans d$options$entrees et d$options$options).
# La 1.0 lit les données et les modèles d'un projet de la 1.1, et garde ces champs sans s'en servir ;
# ce qu'elle change aux données n'est pas une étape, et l'empreinte le montre ; un modèle de code R ou
# de greffon, elle ne sait pas l'ajuster (elle en dit l'erreur). Toute version 1.x ouvre les projets
# de la 1.0.
FORMAT_PROJET <- 3L

lire_projet <- function(chemin) {
  j <- jsonlite::fromJSON(chemin, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                          simplifyMatrix = TRUE)
  # L'état de la séance (où Enregistrer écrit, changements...) ne vient jamais du fichier : un
  # projet reçu ne peut pas faire écrire Enregistrer ailleurs que là où on l'a ouvert, ni se dire
  # l'ancienne version d'un autre projet, que Restaurer écraserait (lecture_seule : serveur.R, ouvrir).
  j[c("id", "chemin", "chemin_enregistrement", "modifie", "table", "brut", "fichier_lu", "note", "import_collecte", "lecture_seule", "cache")] <- NULL
  obs <- unique(unlist(lapply(j$datasets, function(d) d$observations)))
  vars <- unique(unlist(lapply(j$datasets, function(d) d$variables)))
  donnees <- matrix(NA_real_, length(obs), length(vars),
                    dimnames = list(nom_numero(obs), nom_numero(vars)))
  for (d in j$datasets) {
    if (!length(d$observations) || !length(d$variables)) next
    v <- en_matrice(d$values, length(d$observations), length(d$variables))
    donnees[nom_numero(d$observations), nom_numero(d$variables)] <- v
  }
  j$donnees <- donnees
  # (un nom vide qu'une version d'avant la 1.1 gardait manquant, écrit null : relu "", comme l'import le
  # donne maintenant ; les identifiants manquants d'une observation aussi)
  manque <- function(ids) !length(ids) || (if (is.list(ids)) any(vapply(ids, function(x) !length(x) || is.na(x[1]), NA)) else anyNA(ids))
  vides <- which(vapply(j$observations, function(o) manque(o$ids), NA))
  for (i in vides) {
    ids <- j$observations[[i]]$ids
    j$observations[[i]]$ids <- if (!length(ids)) "" else vapply(seq_along(ids), function(k) {
      x <- ids[[k]]
      if (!length(x) || is.na(x[1])) "" else as.character(x[1])
    }, "")
  }
  j$noms_obs <- stats::setNames(
    vapply(j$observations, function(o) premier_texte(o$ids), ""),
    vapply(j$observations, function(o) nom_numero(o$uid), ""))
  j$noms_var <- stats::setNames(
    vapply(j$variables, function(v) as.character(v$ids[1]), ""),
    vapply(j$variables, function(v) nom_numero(v$uid), ""))
  # Projets écrits avant la version 0.5 : la version d'origine s'appelait « <nom>_version »
  # et la solution enregistrée « <nom> » (le même nom) ; ils prennent les noms d'aujourd'hui.
  if (length(j$models)) j$models <- lapply(j$models, function(d) {
    for (v in grep(".+_version$", names(d), value = TRUE)) {
      if (!is.null(d$version_origine)) break
      ancien <- sub("_version$", "", v)
      d$version_origine <- d[[v]]
      d[[v]] <- NULL
      if (!is.null(d[[ancien]])) {
        d$solution <- d[[ancien]]
        d[[ancien]] <- NULL
      }
    }
    d
  })
  class(j) <- "projet_nuee"
  j
}

# Une liste de lignes JSON (avec des null) en matrice numérique.
en_matrice <- function(v, n, k) {
  if (is.matrix(v)) {
    storage.mode(v) <- "double"
    return(v)
  }
  if (is.atomic(v)) return(matrix(as.numeric(v), n, k, byrow = TRUE))
  lignes <- lapply(v, function(r) vapply(r, function(x) if (is.null(x)) NA_real_ else as.numeric(x), 0))
  matrix(unlist(lignes), n, k, byrow = TRUE)
}

# Le nom d'un numéro interne, d'une observation ou d'une variable, tel que le portent les lignes et
# les colonnes de projet$donnees, projet$noms_obs et projet$noms_var : as.character(), sauf qu'un
# nombre entier s'y écrit sans notation scientifique. as.character() écrit le nombre réel 100000
# « 1e+05 » (l'entier, « 100000 ») : passé 99 999 lignes, des observations ne se retrouvaient plus
# dans les données (leurs valeurs lues manquantes, une nouvelle variable refusée).
nom_numero <- function(x) {
  n <- as.character(x)
  if (!is.numeric(x)) return(n)
  entier <- is.finite(x) & x == round(x)
  n[entier] <- sprintf("%.0f", x[entier])
  n
}

# Une valeur d'une demande parmi `choix` (le premier sans elle), ou l'erreur qui le dit dans la langue
# de l'interface, `quoi` nommant le réglage : match.arg() dit en anglais « 'arg' should be one of ».
valeur_parmi <- function(x, choix, quoi) {
  x <- as.character(unlist(x) %||% choix[1])[1]
  if (!isTRUE(x %in% choix)) {
    stop(sprintf(traduire("« %s » n'est pas un choix de %s : %s.", "\"%s\" isn't a choice of %s: %s."), x, quoi, paste(choix, collapse = ", ")), call. = FALSE)
  }
  x
}

# Valeurs brutes d'une variable pour des observations (NA si absentes).
colonne <- function(projet, variable, observations) {
  k <- match(nom_numero(variable), colnames(projet$donnees))
  if (is.na(k)) return(rep(NA_real_, length(observations)))
  projet$donnees[match(nom_numero(observations), rownames(projet$donnees)), k]
}

# Un modèle du projet, par son numéro (3 pour M3) ou son nom ("M3").
modele_du_projet <- function(projet, quel) {
  for (m in projet$models) {
    if (identical(m$number, quel) || identical(as.numeric(m$number), as.numeric(quel)) ||
        identical(m$name, quel)) return(m)
  }
  stop(traduire("Modèle introuvable : ", "Model not found: "), quel)
}

# Historique du fichier (journal d'audit enregistré dans le .usp) : une ligne par événement, avec
# la séance de travail (logiciel, utilisateur, ordinateur), le numéro du modèle concerné quand
# le texte le donne (« model no. 4 ») et la date au format AAAA-MM-JJ hh:mm (le fichier l'écrit
# jour/mois/année, ou mois/jour/année sur un Windows réglé en anglais).
historique_projet <- function(projet) {
  date_iso <- function(d) {
    x <- regmatches(d, regexec("^(\\d{1,2})/(\\d{1,2})/(\\d{4}) (\\d{1,2}:\\d{2})", d))[[1]]
    if (!length(x)) return(d)
    a <- as.numeric(x[2]); b <- as.numeric(x[3])
    jour <- if (b > 12) b else a
    mois <- if (b > 12) a else b
    sprintf("%s-%02d-%02d %s", x[4], mois, jour, x[5])
  }
  lignes <- lapply(projet$audit %||% list(), function(s) {
    ev <- s$events
    if (!length(ev)) return(NULL)
    texte <- vapply(ev, function(e) as.character(e$text %||% ""), "")
    no <- regmatches(texte, regexec("model no\\. ?(\\d+)", texte, ignore.case = TRUE))
    data.frame(date = vapply(ev, function(e) date_iso(as.character(e$date %||% "")), ""),
               logiciel = as.character(s$application %||% ""), utilisateur = as.character(s$user %||% ""),
               ordinateur = as.character(s$computer %||% ""),
               modele = vapply(no, function(x) if (length(x)) as.numeric(x[2]) else NA_real_, 0),
               evenement = texte, details = vapply(ev, function(e) as.character(e$details %||% ""), ""),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, lignes) %||% data.frame()
}

# Les réglages d'un calcul gardé avec le modèle `id` (ses moyennes ajustées, sa puissance, sa
# dépendance partielle), écrits aussi dans sa définition du projet : enregistrés avec lui, et refaits
# quand il est ajusté à nouveau, à l'ouverture du projet (serveur.R : ajuster_et_garder). `modifie` :
# le projet est à enregistrer (des réglages choisis ; pas ceux par défaut d'un premier calcul, que
# l'enregistrement suivant prendra). Gardés tels qu'un fichier les rendra (nombres à 15 chiffres,
# entiers...) : refaits à l'ouverture, ou redemandés par une vue de la séance, ils ne changent pas
# le projet.
garder_avec_definition <- function(id, cle, valeur, modifie = TRUE) {
  valeur <- jsonlite::fromJSON(as.character(jsonlite::toJSON(valeur, auto_unbox = TRUE, null = "null", na = "null", digits = NA)),
                               simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = TRUE)  # (comme lire_projet, projet.R)
  etat$modeles[[id]]$definition[[cle]] <- valeur
  e <- etat$modeles[[id]]
  p <- projet(e$projet)
  for (i in seq_along(p$models)) {
    if (identical(p$models[[i]]$name, e$definition$name) && !identical(p$models[[i]][[cle]], valeur)) {
      p$models[[i]][[cle]] <- valeur
      if (modifie) {
        p$modifie <- TRUE
        # (Rétablir remettrait la définition d'avant, et Annuler un changement d'avant aussi : serveur.R)
        definitions_changees(p$id, e$definition$name)
      }
    }
  }
  etat$projets[[p$id]] <- p
}
