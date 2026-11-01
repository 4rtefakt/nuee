# Lecture d'un projet : fichier .json écrit par export_project.py (projet .usp converti) ou
# enregistré par l'application.
#
# Un projet contient :
#   - les observations et les variables (identifiants, catégories des variables qualitatives) ;
#   - les jeux de données, réunis ici dans une seule matrice `donnees`
#     (lignes = observations, colonnes = variables, repérées par leur numéro interne) ;
#   - la définition de chaque modèle (observations retenues, termes X et Y, transformation
#     et mise à l'échelle de chaque terme) et les résultats enregistrés dans le fichier.

lire_projet <- function(chemin) {
  j <- jsonlite::fromJSON(chemin, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                          simplifyMatrix = TRUE)
  obs <- unique(unlist(lapply(j$datasets, function(d) d$observations)))
  vars <- unique(unlist(lapply(j$datasets, function(d) d$variables)))
  donnees <- matrix(NA_real_, length(obs), length(vars),
                    dimnames = list(as.character(obs), as.character(vars)))
  for (d in j$datasets) {
    if (!length(d$observations) || !length(d$variables)) next
    v <- en_matrice(d$values, length(d$observations), length(d$variables))
    donnees[as.character(d$observations), as.character(d$variables)] <- v
  }
  j$donnees <- donnees
  j$noms_obs <- stats::setNames(
    vapply(j$observations, function(o) as.character(o$ids[1]), ""),
    vapply(j$observations, function(o) as.character(o$uid), ""))
  j$noms_var <- stats::setNames(
    vapply(j$variables, function(v) as.character(v$ids[1]), ""),
    vapply(j$variables, function(v) as.character(v$uid), ""))
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

# Valeurs brutes d'une variable pour des observations (NA si absentes).
colonne <- function(projet, variable, observations) {
  k <- match(as.character(variable), colnames(projet$donnees))
  if (is.na(k)) return(rep(NA_real_, length(observations)))
  projet$donnees[match(as.character(observations), rownames(projet$donnees)), k]
}

# Un modèle du projet, par son numéro (3 pour M3) ou son nom ("M3").
modele_du_projet <- function(projet, quel) {
  for (m in projet$models) {
    if (identical(m$number, quel) || identical(as.numeric(m$number), as.numeric(quel)) ||
        identical(m$name, quel)) return(m)
  }
  stop("Modèle introuvable : ", quel)
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
