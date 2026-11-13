# Variables ajoutées à un projet : scores d'un modèle (pour les modèles hiérarchiques) et
# classes créées à partir d'une sélection d'observations. Elles sont rangées dans le jeu de
# données « Variables ajoutées » et enregistrées avec le projet.

JEU_AJOUTE <- "Variables ajoutées"

# Ajoute une variable au projet, ou met à jour celle qui porte déjà ce nom (par exemple les
# scores $M1.t1(titre) enregistrés dans un fichier .usp). `valeurs` : une par observation de
# `observations`. effacer = TRUE vide d'abord la colonne (scores : seules les observations
# du modèle en ont) ; FALSE garde les autres valeurs (classes ajoutées petit à petit).
ajouter_colonne <- function(projet, nom, observations, valeurs, categories = NULL, effacer = TRUE) {
  i <- which(vapply(projet$variables, function(v) identical(as.character(v$ids[1]), nom), TRUE))
  if (length(i)) {
    i <- i[1]
    uid <- projet$variables[[i]]$uid
    if (!is.null(categories)) {
      if (!length(projet$variables[[i]]$categories) && any(!is.na(projet$donnees[, as.character(uid)]))) {
        stop(sprintf(traduire("La variable « %s » existe déjà et n'est pas qualitative", "The variable “%s” already exists and isn't qualitative"), nom))
      }
      projet$variables[[i]]$categories <- categories
    }
  } else {
    uid <- max(c(0, vapply(projet$variables, function(v) as.numeric(v$uid), 0))) + 1
    projet$variables <- c(projet$variables, list(list(uid = uid, ids = nom, categories = categories,
                                                      formula = "ajoutée par l'application")))
    projet$noms_var[as.character(uid)] <- nom
    jeu <- which(vapply(projet$datasets, function(d) identical(d$name, JEU_AJOUTE), TRUE))
    if (!length(jeu)) {
      projet$datasets <- c(projet$datasets, list(list(name = JEU_AJOUTE,
                                                      observations = as.numeric(rownames(projet$donnees)),
                                                      variables = numeric(0))))
      jeu <- length(projet$datasets)
    }
    projet$datasets[[jeu]]$variables <- c(projet$datasets[[jeu]]$variables, uid)
  }
  k <- as.character(uid)
  if (!(k %in% colnames(projet$donnees))) {
    projet$donnees <- cbind(projet$donnees, matrix(NA_real_, nrow(projet$donnees), 1, dimnames = list(NULL, k)))
  }
  if (effacer) projet$donnees[, k] <- NA
  projet$donnees[as.character(observations), k] <- valeurs
  projet
}

# Range des observations dans une classe : la variable qualitative `variable` (créée si
# besoin) reçoit la catégorie `etiquette` pour ces observations ; les autres gardent la leur.
ajouter_classe <- function(projet, variable, etiquette, observations) {
  v <- Filter(function(v) identical(as.character(v$ids[1]), variable), projet$variables)
  categories <- if (length(v)) as.character(v[[1]]$categories %||% character(0)) else character(0)
  if (!(etiquette %in% categories)) categories <- c(categories, etiquette)
  ajouter_colonne(projet, variable, observations, rep(match(etiquette, categories), length(observations)),
                  categories = categories, effacer = FALSE)
}

# Modèle hiérarchique : les scores t1..tA d'un modèle deviennent des variables du projet,
# nommées comme dans les fichiers .usp : $M1.t1(titre). Un modèle « de niveau supérieur » les utilise
# ensuite comme X. Après avoir réajusté le modèle de base, refaire cette étape met ses
# variables à jour, puis réajuster le modèle supérieur.
scores_en_variables <- function(projet, m) {
  suffixe <- if (nzchar(m$titre %||% "")) paste0("(", m$titre, ")") else ""
  noms <- sprintf("$%s.t%d%s", m$nom, seq_len(m$A), suffixe)
  for (a in seq_len(m$A)) projet <- ajouter_colonne(projet, noms[a], m$observations, m$T[, a])
  list(projet = projet, noms = noms)
}

# Les modèles du projet qui utilisent l'une de ces variables (par leur nom).
modeles_utilisant <- function(projet, noms) {
  uids <- vapply(Filter(function(v) as.character(v$ids[1]) %in% noms, projet$variables), function(v) as.numeric(v$uid), 0)
  utilise <- function(d) any(unlist(lapply(c(d$x, d$y), function(t) t$variables)) %in% uids)
  vapply(Filter(utilise, projet$models), function(d) as.character(d$name %||% ""), "")
}

# Numéros des variables d'après leur nom (pour écrire des scripts lisibles).
numeros <- function(projet, noms) {
  tous <- vapply(projet$variables, function(v) as.character(v$ids[1]), "")
  k <- match(noms, tous)
  if (anyNA(k)) stop(traduire("Variable introuvable : ", "Variable not found: "), paste(noms[is.na(k)], collapse = ", "))
  vapply(projet$variables[k], function(v) as.numeric(v$uid), 0)
}

# Numéros des observations d'après leur nom ; sans noms, toutes les observations.
numeros_observations <- function(projet, noms = NULL) {
  uids <- vapply(projet$observations, function(o) as.numeric(o$uid), 0)
  if (is.null(noms)) return(uids)
  k <- match(noms, unname(projet$noms_obs[as.character(uids)]))
  if (anyNA(k)) stop(traduire("Observation introuvable : ", "Observation not found: "), paste(noms[is.na(k)], collapse = ", "))
  uids[k]
}

# Modifie une valeur des données. Pour une variable qualitative, la valeur est le nom d'une
# catégorie (une nouvelle catégorie est ajoutée au besoin) ; une valeur vide devient manquante.
# Les modèles déjà ajustés ne changent pas : il faut les réajuster.
modifier_valeur <- function(projet, observation, variable, valeur) {
  k <- as.character(variable)
  if (!(k %in% colnames(projet$donnees))) stop(traduire("Variable sans données : ", "Variable without data: "), variable)
  i <- which(vapply(projet$variables, function(v) identical(as.numeric(v$uid), as.numeric(variable)), TRUE))[1]
  texte <- trimws(as.character(valeur %||% ""))
  cats <- as.character(projet$variables[[i]]$categories %||% character(0))
  x <- if (!nzchar(texte)) {
    NA_real_
  } else if (length(cats)) {
    if (!(texte %in% cats)) projet$variables[[i]]$categories <- cats <- c(cats, texte)
    match(texte, cats)
  } else {
    n <- suppressWarnings(as.numeric(sub(",", ".", texte, fixed = TRUE)))  # 3,5 ou 3.5
    if (is.na(n)) stop(sprintf(traduire("Valeur non numérique pour %s : %s", "Non-numeric value for %s: %s"), projet$noms_var[[k]], texte))
    n
  }
  projet$donnees[as.character(observation), k] <- x
  projet
}

# La moyenne de x dans le groupe de chaque observation (les valeurs manquantes ignorées), et
# l'écart de x à cette moyenne : pour une formule de Nouvelle variable, par exemple la moyenne
# d'une variable par secteur, ou une réponse centrée par secteur (ce qui varie d'une année à
# l'autre dans un secteur). `groupe` : une variable qualitative, un identifiant des observations,
# ou interaction(site, secteur). Une observation sans groupe (identifiant vide, catégorie
# manquante) n'a ni moyenne ni écart.
moyenne_groupe <- function(x, groupe) {
  g <- as.character(groupe)
  r <- stats::ave(as.numeric(x), g, FUN = function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE))
  r[is.na(g)] <- NA_real_  # (ave() leur laisse leur propre valeur)
  r
}
ecart_groupe <- function(x, groupe) as.numeric(x) - moyenne_groupe(x, groupe)
group_mean <- moyenne_groupe
group_deviation <- ecart_groupe

# La valeur de x au temps `temps - pas` (precedent) ou `temps + pas` (suivant) dans le groupe de
# chaque observation, et le taux de croissance d'une année à l'autre : pour une formule de
# Nouvelle variable, aligner l'année n sur l'année n - 1 (la densité de l'année d'avant), ou la
# dépendance à la densité (la croissance selon la densité précédente). `temps` : des nombres
# (l'année ; un identifiant fait de nombres convient), ou des dates (temps_en_nombres : en années
# décimales). `groupe` : comme pour moyenne_groupe (le secteur, ou interaction(site, secteur)) ;
# sans groupe, toutes les observations font une seule série. Le temps cherché doit exister : une
# année manquante donne NA, pas la valeur d'une année plus ancienne. Ces fonctions vont donc avec
# des temps réguliers (des années) : avec des dates, le temps exactement un pas avant existe
# rarement, et aucune date « la plus proche » n'est prise à sa place. Une observation sans groupe
# ou sans temps a NA ; un couple (groupe, temps) qui revient est une erreur.
precedent <- function(x, temps, groupe = NULL, pas = 1) valeur_decalee(x, temps, groupe, pas, -1, nom_appel(sys.call(), "precedent"))
suivant <- function(x, temps, groupe = NULL, pas = 1) valeur_decalee(x, temps, groupe, pas, 1, nom_appel(sys.call(), "suivant"))
# Le taux de croissance de la population, log(x_t / x_t-1) ; NA sauf si les deux valeurs sont > 0.
croissance <- function(x, temps, groupe = NULL) {
  nom <- nom_appel(sys.call(), "croissance")
  if (!is.numeric(x)) stop(sprintf(traduire("%s() : x doit être numérique", "%s(): x must be numeric"), nom), call. = FALSE)
  avant <- valeur_decalee(x, temps, groupe, 1, -1, nom)
  r <- rep(NA_real_, length(x))
  ok <- !is.na(x) & !is.na(avant) & x > 0 & avant > 0
  r[ok] <- log(x[ok] / avant[ok])
  r
}
previous <- precedent
following <- suivant
growth_rate <- croissance

# (le nom sous lequel la fonction a été appelée, pour ses messages : precedent ou previous)
nom_appel <- function(appel, defaut) if (is.name(appel[[1]])) as.character(appel[[1]]) else defaut

valeur_decalee <- function(x, temps, groupe, pas, sens, nom) {
  if (!is.numeric(pas) || length(pas) != 1 || !is.finite(pas) || pas <= 0) {
    stop(sprintf(traduire("%s() : le pas doit être un nombre positif (1 : l'année d'avant ou d'après)", "%s(): the step must be a positive number (1: the year before or after)"), nom), call. = FALSE)
  }
  t <- temps_en_nombres(temps)
  if (!is.null(attr(t, "refus"))) stop(refus_temps(t, fonction = nom), call. = FALSE)
  ecrire <- if (isTRUE(attr(t, "dates"))) date_decimale else format
  g <- if (is.null(groupe)) rep("", length(t)) else as.character(groupe)
  if (length(x) != length(t) || length(g) != length(t)) {
    stop(sprintf(traduire("%s() : x, le temps et le groupe doivent avoir une valeur par observation", "%s(): x, the time and the group must have one value per observation"), nom), call. = FALSE)
  }
  connu <- !is.na(t) & !is.na(g)
  cle <- function(tt) ifelse(connu, paste(g, as.character(round(tt, 8)), sep = "\r"), NA_character_)
  ici <- cle(t)
  double <- which(duplicated(ici, incomparables = NA))
  if (length(double)) {
    i <- double[1]
    stop(if (is.null(groupe)) sprintf(traduire("%s() : deux observations ont le temps %s ; chaque temps ne doit venir qu'une fois (donner le groupe : le secteur, ou interaction(site, secteur))",
                                               "%s(): two observations have the time %s; each time must come only once (give the group: the sector, or interaction(site, sector))"), nom, ecrire(t[i]))
         else sprintf(traduire("%s() : deux observations ont le groupe « %s » et le temps %s ; chaque couple (groupe, temps) ne doit venir qu'une fois (un secteur dans un site : interaction(site, secteur))",
                               "%s(): two observations have the group “%s” and the time %s; each (group, time) pair must come only once (a sector within a site: interaction(site, sector))"), nom, g[i], ecrire(t[i])),
         call. = FALSE)
  }
  x[match(cle(t + sens * pas), ici, incomparables = NA)]
}

# Des temps en nombres, la conversion que font les séries, BACI, les décalages de Nouvelle variable,
# l'autocorrélation des résidus et l'autocorrélation CAR1 des régressions (et l'export vers R, qui
# l'écrit dans son script) : des nombres ; du texte fait de nombres (un identifiant des
# observations, l'année ; « 2019,5 » avec la virgule) ; ou des dates, en années décimales
# (annees_decimales : 2019-05-14 ou 14/05/2019, au besoin avec l'heure), ce que deviennent aussi
# les dates de R (Date, POSIXct). Les catégories d'une variable qualitative, un facteur, se lisent
# comme du texte. Un nombre reste un nombre : un numéro de jour d'Excel ne se distingue pas d'une
# année. Les valeurs connues sont toutes des nombres, ou toutes des dates (attr(, "dates") : TRUE),
# selon ce que sont la plupart d'entre elles ; la première qui n'est pas du même genre est refusée :
# attr(, "refus") la donne, et attr(, "raison") pourquoi ("nombre" ou "date" parmi des valeurs de
# l'autre genre, "impossible" pour une date qui n'existe pas comme 31/02/2019, "illisible" pour une
# valeur qui n'est ni l'un ni l'autre) ; refus_temps() en fait le message.
temps_en_nombres <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) x <- format(x, "%Y-%m-%d %H:%M:%S")
  if (is.numeric(x)) {
    x <- as.numeric(x)
    x[!is.finite(x)] <- NA
    return(x)
  }
  texte <- trimws(as.character(x))
  connu <- !is.na(texte) & nzchar(texte)
  n <- suppressWarnings(as.numeric(sub(",", ".", texte, fixed = TRUE)))
  nombre <- connu & is.finite(n)
  # (les dates cherchées seulement parmi les valeurs qui ne sont pas des nombres)
  a <- rep(NA_real_, length(texte))
  forme <- rep(FALSE, length(texte))
  autres <- which(connu & !nombre)
  if (length(autres)) {
    d <- annees_decimales(texte[autres])
    a[autres] <- d
    forme[autres] <- attr(d, "forme")
  }
  date <- !is.na(a)
  dates <- sum(date) > sum(nombre)
  bon <- if (dates) date else nombre
  r <- if (dates) a else n
  r[!bon] <- NA
  if (dates) attr(r, "dates") <- TRUE
  faux <- which(connu & !bon)
  if (length(faux)) {
    k <- faux[1]
    attr(r, "refus") <- texte[k]
    attr(r, "raison") <- if (nombre[k]) "nombre" else if (date[k]) "date" else if (forme[k]) "impossible" else "illisible"
  }
  r
}

# Le message d'un temps refusé par temps_en_nombres() (`t` : son résultat) : `nom`, celui du temps
# (sa variable ou son identifiant), `fonction`, celle de Nouvelle variable qui l'a lu.
refus_temps <- function(t, nom = NULL, fonction = NULL) {
  v <- attr(t, "refus")
  pourquoi <- switch(attr(t, "raison"),
    nombre = sprintf(traduire("« %s » est un nombre parmi des dates", "“%s” is a number among dates"), v),
    date = sprintf(traduire("« %s » est une date parmi des nombres", "“%s” is a date among numbers"), v),
    impossible = sprintf(traduire("« %s » n'est pas une date qui existe", "“%s” isn't a date that exists"), v),
    sprintf(traduire("« %s » n'est ni un nombre ni une date", "“%s” is neither a number nor a date"), v))
  temps <- if (is.null(nom)) traduire("le temps", "the time") else sprintf(traduire("le temps (%s)", "the time (%s)"), nom)
  melange <- attr(t, "raison") %in% c("nombre", "date")
  phrase <- sprintf(if (melange) traduire("%s doit être fait de nombres (l'année) ou de dates (2019-05-14, 14/05/2019), pas des deux : %s",
                                          "%s must be numbers (the year) or dates (2019-05-14, 14/05/2019), not both: %s")
                    else traduire("%s doit être fait de nombres (l'année) ou de dates (2019-05-14, 14/05/2019) : %s",
                                  "%s must be numbers (the year) or dates (2019-05-14, 14/05/2019): %s"), temps, pourquoi)
  if (!is.null(fonction)) return(sprintf(traduire("%s() : %s", "%s(): %s"), fonction, phrase))
  paste0(toupper(substr(phrase, 1, 1)), substring(phrase, 2))
}

# Des dates écrites en texte, en années décimales : l'année + (le jour de l'année − 1 + la fraction
# du jour) / le nombre de jours de l'année (366 les années bissextiles). Ainsi le 1er janvier est
# l'année elle-même, et une pente, un écart ou le φ d'une autocorrélation sont « par an ». Les
# dates : 2019-05-14 (ISO), ou 14/05/2019, toujours le jour d'abord (comme en France ; aussi
# 14-05-2019 et 14.05.2019), l'année en quatre chiffres ; au besoin avec l'heure, 10:30 ou 10:30:15
# (après une espace, ou un T après une date ISO). NA pour une valeur qui n'est pas une date, ou une
# date qui n'existe pas (31/02/2019, 25:00) ; attr(, "forme") : la valeur est écrite comme une date.
# (Le script de l'export vers R refait ce calcul de la même façon : script.R, modele_temps_dates.)
annees_decimales <- function(texte) {
  texte[is.na(texte)] <- ""
  # les morceaux de chaque date, une colonne chacun : année, mois, jour, heure, minutes, secondes
  # (les groupes du motif dans cet ordre ; NA sans date, et pour une heure absente)
  morceaux <- function(motif, ordre) {
    m <- regexpr(motif, texte, perl = TRUE)
    debut <- attr(m, "capture.start")[, ordre, drop = FALSE]
    fin <- debut + attr(m, "capture.length")[, ordre, drop = FALSE] - 1L
    x <- matrix(suppressWarnings(as.numeric(substring(rep(texte, length(ordre)), debut, fin))), ncol = length(ordre))
    x[m == -1L, ] <- NA
    x
  }
  heure <- "([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?"
  p <- morceaux(sprintf("^([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})(?:[ T]%s)?$", heure), 1:6)
  jour <- morceaux(sprintf("^([0-9]{1,2})([/.-])([0-9]{1,2})\\2([0-9]{4})(?: %s)?$", heure), c(4, 3, 1, 5, 6, 7))
  p[is.na(p[, 1]), ] <- jour[is.na(p[, 1]), ]
  forme <- !is.na(p[, 1])
  a <- p[, 1]
  mois <- p[, 2]
  j <- p[, 3]
  h <- p[, 4]
  mi <- p[, 5]
  s <- p[, 6]
  h[is.na(h)] <- 0
  mi[is.na(mi)] <- 0
  s[is.na(s)] <- 0
  bissextile <- (a %% 4 == 0 & a %% 100 != 0) | a %% 400 == 0
  valide <- forme & mois >= 1 & mois <= 12 & h <= 23 & mi <= 59 & s <= 59
  valide[is.na(valide)] <- FALSE
  fin_mois <- rep(NA_real_, length(a))
  fin_mois[valide] <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[mois[valide]] + (mois[valide] == 2 & bissextile[valide])
  valide <- valide & j >= 1 & j <= fin_mois
  valide[is.na(valide)] <- FALSE
  r <- rep(NA_real_, length(a))
  jour_annee <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)[mois[valide]] + j[valide] + (mois[valide] > 2 & bissextile[valide])
  r[valide] <- a[valide] + (jour_annee - 1 + (h[valide] * 3600 + mi[valide] * 60 + s[valide]) / 86400) / (365 + bissextile[valide])
  structure(r, forme = forme)
}

# Une année décimale en date (« 2019-05-14 », avec l'heure quand elle n'est pas minuit) : l'inverse
# d'annees_decimales, pour les textes (la phrase de BACI, les messages).
date_decimale <- function(x) {
  a <- floor(x)
  secondes <- round((x - a) * (365 + ((a %% 4 == 0 & a %% 100 != 0) | a %% 400 == 0)) * 86400)
  d <- ISOdatetime(a, 1, 1, 0, 0, 0, tz = "UTC") + secondes
  format(d, if (any(secondes %% 86400 != 0, na.rm = TRUE)) "%Y-%m-%d %H:%M" else "%Y-%m-%d", tz = "UTC")
}

# Nouvelle variable calculée par une formule R sur les variables du projet, appelées par leur
# nom (entre `accents graves` si le nom contient des espaces ou des signes), par exemple :
#   log10(polyphenols)   sucre / acidite        ifelse(poids >= 170, "gros", "petit")
#   moyenne_groupe(sucre, verger)               ecart_groupe(densite, secteur)
# Une variable qualitative y vaut le nom de sa catégorie ; un identifiant des observations
# (site, secteur...) y vaut ses valeurs, sauf si une variable a le même nom. Un résultat texte
# donne une variable qualitative, un résultat logique aussi (catégories FALSE et TRUE).
variable_calculee <- function(projet, nom, formule) {
  nom <- trimws(nom)
  if (!nzchar(nom)) stop(traduire("Donner un nom à la nouvelle variable", "Give the new variable a name"))
  if (nom %in% projet$noms_var) stop(sprintf(traduire("Une variable s'appelle déjà « %s »", "A variable is already called “%s”"), nom))
  obs <- as.numeric(rownames(projet$donnees))
  env <- new.env(parent = globalenv())
  for (v in Filter(function(v) as.character(v$uid) %in% colnames(projet$donnees), projet$variables)) {
    x <- projet$donnees[, as.character(v$uid)]
    assign(as.character(v$ids[1]), if (length(v$categories)) as.character(v$categories)[x] else unname(x), envir = env)
  }
  noms_id <- as.character(projet$observation_id_names %||% character(0))
  for (k in seq_along(noms_id)[-1]) {
    n <- noms_id[k]
    if (!nzchar(n) || startsWith(n, "$") || exists(n, envir = env, inherits = FALSE)) next
    ids <- identifiants_observations(projet, k, obs)
    ids[!is.na(ids) & !nzchar(trimws(ids))] <- NA
    assign(n, ids, envir = env)
  }
  r <- tryCatch(eval(parse(text = formule), envir = env), error = function(e) {
    m <- conditionMessage(e)
    absent <- regmatches(m, regexec("^obje?c?t '(.*)' (not found|introuvable)", m))[[1]]
    stop(if (length(absent)) sprintf(traduire("Variable introuvable : %s (vérifier le nom ; un nom avec des espaces ou des signes s'écrit entre `accents graves`)", "Variable not found: %s (check the name; a name with spaces or signs is written between `backticks`)"), absent[2]) else m, call. = FALSE)
  })
  if (is.factor(r) || is.logical(r)) r <- as.character(r)
  if (length(r) == 1) r <- rep(r, length(obs))
  if (length(r) != length(obs)) {
    stop(sprintf(traduire("La formule doit donner une valeur par observation (%d) ; elle en donne %d", "The formula must give one value per observation (%d); it gives %d"), length(obs), length(r)))
  }
  if (is.character(r)) {
    cats <- sort(unique(stats::na.omit(r)))
    return(ajouter_colonne(projet, nom, obs, match(r, cats), categories = cats))
  }
  if (!is.numeric(r)) stop(traduire("La formule doit donner des nombres ou du texte", "The formula must give numbers or text"))
  ajouter_colonne(projet, nom, obs, ifelse(is.finite(r), r, NA))
}

# Les valeurs manquantes de variables quantitatives remplies par ACP itérative régularisée
# (Josse et Husson 2012), comme missMDA::imputePCA (méthode « Regularized », variables réduites) :
# les manques d'abord à la moyenne ; à chaque tour, le tableau complété recentré et réduit, l'ACP
# à k composantes, aux valeurs singulières réduites du bruit estimé sur les autres axes, le
# reconstruit, et les manques en prennent les valeurs ; jusqu'à ce que l'écart du tableau à sa
# reconstruction, sur les valeurs observées, change de moins de 1e-6 (au moins 5 tours). Sans
# `k`, le nombre de composantes est choisi par validation croisée généralisée des variables
# réduites, de 0 à 5, comme missMDA::estim_ncpPCA(scale(X)) (0 : la moyenne de chaque
# variable). Les valeurs observées ne changent pas. Une observation à qui manquent plus de la
# moitié de ces variables reste manquante, et ne compte pas dans l'ACP. Avec `bornes`, une valeur
# imputée reste dans l'étendue observée de sa variable (une variable 0/1 entre 0 et 1, une
# surface positive). De nouvelles variables « x (imputée) », ou, avec `remplacer`, les variables
# elles-mêmes, pour celles qui ont reçu des valeurs (les complètes aident seulement à imputer les
# autres). attr(, "imputation") : le nombre de valeurs remplies, de composantes (et si elles ont
# été choisies) et de tours.
imputer_acp <- function(projet, variables, k = NULL, maxit = 1000, bornes = TRUE, remplacer = FALSE) {
  obs <- as.numeric(rownames(projet$donnees))
  variables <- unique(as.numeric(variables))
  for (u in variables) {
    if (length(variable_du_projet(projet, u)$categories)) stop(traduire("L'imputation par ACP va avec des variables quantitatives.", "Imputation by PCA goes with quantitative variables."), call. = FALSE)
  }
  if (length(variables) < 2) stop(traduire("Choisir au moins deux variables : l'ACP complète chacune par les autres.", "Choose at least two variables: the PCA fills each one from the others."), call. = FALSE)
  X <- vapply(variables, function(u) as.numeric(colonne(projet, u, obs)), numeric(length(obs)))
  dimnames(X) <- NULL
  if (!anyNA(X)) stop(traduire("Ces variables n'ont aucune valeur manquante.", "These variables have no missing values."), call. = FALSE)
  utiles <- rowMeans(is.na(X)) <= 0.5
  Xu <- X[utiles, , drop = FALSE]
  s <- apply(Xu, 2, stats::sd, na.rm = TRUE)
  if (any(!is.finite(s) | s == 0)) {
    stop(sprintf(traduire("Une variable constante ou presque vide ne s'impute pas : %s.", "A constant or nearly empty variable can't be imputed: %s."),
                 paste(projet$noms_var[as.character(variables[!is.finite(s) | s == 0])], collapse = ", ")), call. = FALSE)
  }
  n <- nrow(Xu)
  p <- ncol(Xu)
  M <- is.na(Xu)
  if (!any(M)) {
    stop(traduire("Aucune valeur à remplir : il manque à chaque observation incomplète plus de la moitié de ces variables.",
                  "No value to fill: each incomplete observation misses more than half of these variables."), call. = FALSE)
  }
  if (n < 3) stop(traduire("Trop peu d'observations pour une ACP : il en faut au moins 3 à qui manque au plus la moitié de ces variables.",
                           "Too few observations for a PCA: at least 3 are needed that miss at most half of these variables."), call. = FALSE)
  choisies <- is.null(k) || is.na(k[1])
  k <- if (choisies) composantes_imputation(Xu) else max(1L, min(as.integer(k[1]), p - 1L, n - 2L))
  r <- acp_reguliere(Xu, k, maxit = maxit)
  Xi <- r$complet
  Xi[!M] <- Xu[!M]  # (exactement les valeurs observées)
  if (bornes) {
    for (j in seq_len(p)) {
      b <- range(Xu[, j], na.rm = TRUE)
      Xi[M[, j], j] <- pmin(pmax(Xi[M[, j], j], b[1]), b[2])
    }
  }
  X[utiles, ] <- Xi
  for (j in seq_len(p)) {
    if (!any(M[, j])) next  # (une variable sans valeur remplie aide à imputer les autres, sans copie)
    u <- variables[j]
    if (remplacer) {
      projet$donnees[, as.character(u)] <- X[, j]
    } else {
      # (imputée à nouveau, avec d'autres réglages : la même variable, mise à jour)
      projet <- ajouter_colonne(projet, paste0(projet$noms_var[[as.character(u)]], traduire(" (imputée)", " (imputed)")), obs, X[, j])
    }
  }
  attr(projet, "imputation") <- list(valeurs = sum(M), tours = r$tours, composantes = k, choisies = choisies, laissees = sum(!utiles))
  projet
}

# missMDA::imputePCA (méthode « Regularized », variables réduites, chaque observation de poids
# 1/n), avec la SVD complète de R au lieu d'irlba::irlba, qui en approche les premiers axes :
# `complet`, le tableau aux manques remplis, `ajuste`, sa reconstruction par les k composantes,
# dans les unités des variables, et `tours`. Avec k = 0, les manques à la moyenne.
acp_reguliere <- function(X, k, seuil = 1e-6, maxit = 1000) {
  n <- nrow(X)
  p <- ncol(X)
  M <- is.na(X)
  moyennes <- colMeans(X, na.rm = TRUE)
  Z <- sweep(X, 2, moyennes)
  et <- sqrt(colMeans(Z^2, na.rm = TRUE))  # (l'écart-type des valeurs observées, en 1/n)
  Z <- sweep(Z, 2, et, "/")
  Z[M] <- 0
  ajuste <- Z
  ancien <- Inf
  tours <- 0
  while (k > 0) {
    # le tableau complété par la reconstruction du tour d'avant, recentré et réduit
    Z[M] <- ajuste[M]
    Z <- sweep(sweep(Z, 2, et, "*"), 2, moyennes, "+")
    moyennes <- colMeans(Z)
    Z <- sweep(Z, 2, moyennes)
    et <- sqrt(colMeans(Z^2))
    Z <- sweep(Z, 2, et, "/")
    vs <- svd(Z / sqrt(n))
    # le bruit, estimé sur les axes au-delà de k, pas plus grand que la valeur propre k + 1
    sigma2 <- n * p / min(p, n - 1) * (sum(Z^2) / n - sum(vs$d[seq_len(k)]^2)) / ((n - 1) * p - (n - 1) * k - p * k + k^2)
    sigma2 <- min(sigma2, vs$d[k + 1]^2)
    reduites <- (vs$d[seq_len(k)]^2 - sigma2) / vs$d[seq_len(k)]
    ajuste <- sqrt(n) * vs$u[, seq_len(k), drop = FALSE] %*% (reduites * t(vs$v[, seq_len(k), drop = FALSE]))
    ecart <- Z - ajuste
    ecart[M] <- 0
    critere <- sum(ecart^2) / n
    evolution <- abs(1 - critere / ancien)
    ancien <- critere
    tours <- tours + 1
    if ((!is.nan(evolution) && (evolution < seuil || critere < seuil) && tours >= 5) || tours >= maxit) break
  }
  remettre <- function(A) sweep(sweep(A, 2, et, "*"), 2, moyennes, "+")
  complet <- X
  complet[M] <- remettre(Z)[M]
  list(complet = complet, ajuste = remettre(ajuste), tours = tours)
}

# Le nombre de composantes d'une imputation, comme missMDA::estim_ncpPCA sur les variables
# réduites (estim_ncpPCA(scale(X))) : la validation croisée généralisée de 0 à `kmax` composantes
# (0 : l'écart des valeurs observées à leur moyenne), et le premier nombre après lequel le
# critère remonte. Réduites, chaque variable y compte autant, comme dans l'ACP ; dans leurs unités
# (estim_ncpPCA(X)), celles de grande variance feraient seules le critère, souvent jusqu'à 0.
composantes_imputation <- function(X, kmax = 5) {
  X <- scale(X)
  n <- nrow(X)
  p <- ncol(X)
  manques <- sum(is.na(X))
  kmax <- min(kmax, p - 1, n - 2)
  critere <- mean((X - rep(colMeans(X, na.rm = TRUE), each = n))^2, na.rm = TRUE)
  for (q in seq_len(kmax)) {
    ajuste <- acp_reguliere(X, q)$ajuste
    critere <- c(critere, mean(((n * p - manques) * (X - ajuste) / ((n - 1) * p - manques - q * (n + p - q - 1)))^2, na.rm = TRUE))
  }
  hausses <- which(diff(critere) > 0)
  as.integer((if (length(hausses)) hausses[1] else which.min(critere)) - 1)
}

# Variable qualitative d'après les valeurs d'une variable quantitative : des bornes
# (par exemple 10 et 20 donnent « ≤ 10 », « ]10 ; 20] » et « > 20 ») ou un nombre de classes
# d'effectifs à peu près égaux (bornes aux quantiles).
classes_valeurs <- function(projet, nom, variable, bornes = NULL, n = NULL) {
  nom <- trimws(nom)
  if (!nzchar(nom)) stop(traduire("Donner un nom à la nouvelle variable", "Give the new variable a name"))
  if (nom %in% projet$noms_var) stop(sprintf(traduire("Une variable s'appelle déjà « %s »", "A variable is already called “%s”"), nom))
  obs <- as.numeric(rownames(projet$donnees))
  x <- colonne(projet, variable, obs)
  if (length(variable_du_projet(projet, variable)$categories)) stop(traduire("Choisir une variable quantitative", "Choose a quantitative variable"))
  if (is.null(bornes)) {
    q <- stats::quantile(x, seq(0, 1, length.out = n + 1), na.rm = TRUE, names = FALSE)
    bornes <- unique(q[-c(1, length(q))])
  }
  bornes <- sort(unique(as.numeric(bornes)))
  if (!length(bornes)) stop(traduire("Il faut au moins une borne", "At least one limit is needed"))
  f <- function(b) format(signif(b, 4), trim = TRUE)
  etiquettes <- c(sprintf("≤ %s", f(bornes[1])),
                  if (length(bornes) > 1) sprintf("]%s ; %s]", f(bornes[-length(bornes)]), f(bornes[-1])),
                  sprintf("> %s", f(bornes[length(bornes)])))
  classe <- cut(x, c(-Inf, bornes, Inf), labels = FALSE, right = TRUE)
  ajouter_colonne(projet, nom, obs, classe, categories = etiquettes)
}
