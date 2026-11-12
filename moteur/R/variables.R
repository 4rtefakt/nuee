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
# ou interaction(site, secteur).
moyenne_groupe <- function(x, groupe) stats::ave(as.numeric(x), as.character(groupe), FUN = function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE))
ecart_groupe <- function(x, groupe) as.numeric(x) - moyenne_groupe(x, groupe)
group_mean <- moyenne_groupe
group_deviation <- ecart_groupe

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
# (Josse et Husson 2012 ; missMDA::imputePCA, méthode « Regularized ») : les variables centrées
# et réduites, leurs manques d'abord à la moyenne ; à chaque tour, l'ACP à k composantes, aux
# valeurs singulières réduites du bruit estimé sur les autres, reconstruit le tableau, dont les
# manques prennent les valeurs, et les colonnes sont recentrées ; jusqu'à ce que les valeurs
# imputées bougent de moins de `tol` écart-type. Les valeurs observées ne changent pas. Une
# observation à qui manquent plus de la moitié de ces variables reste manquante. Avec `bornes`,
# une valeur imputée reste dans l'étendue observée de sa variable (une variable 0/1 entre 0 et
# 1, une surface positive). De nouvelles variables « x (imputée) », ou, avec `remplacer`, les
# variables elles-mêmes. attr(, "imputation") : le nombre de valeurs remplies et de tours.
imputer_acp <- function(projet, variables, k = 5, tol = 1e-4, maxit = 1000, bornes = TRUE, remplacer = FALSE) {
  obs <- as.numeric(rownames(projet$donnees))
  variables <- unique(as.numeric(variables))
  for (u in variables) {
    if (length(variable_du_projet(projet, u)$categories)) stop(traduire("L'imputation par ACP va avec des variables quantitatives.", "Imputation by PCA goes with quantitative variables."), call. = FALSE)
  }
  if (length(variables) < 2) stop(traduire("Choisir au moins deux variables : l'ACP complète chacune par les autres.", "Choose at least two variables: the PCA fills each one from the others."), call. = FALSE)
  X <- vapply(variables, function(u) as.numeric(colonne(projet, u, obs)), numeric(length(obs)))
  dimnames(X) <- NULL
  utiles <- rowMeans(is.na(X)) <= 0.5
  Xu <- X[utiles, , drop = FALSE]
  mu <- colMeans(Xu, na.rm = TRUE)
  s <- apply(Xu, 2, stats::sd, na.rm = TRUE)
  if (any(!is.finite(s) | s == 0)) {
    stop(sprintf(traduire("Une variable constante ou presque vide ne s'impute pas : %s.", "A constant or nearly empty variable can't be imputed: %s."),
                 paste(projet$noms_var[as.character(variables[!is.finite(s) | s == 0])], collapse = ", ")), call. = FALSE)
  }
  n <- nrow(Xu)
  p <- ncol(Xu)
  k <- max(1, min(as.integer(k), p - 1, n - 2))
  Z <- sweep(sweep(Xu, 2, mu), 2, s, "/")
  M <- is.na(Z)
  Z[M] <- 0
  cumul <- rep(0, p)
  it <- 0
  if (any(M)) for (it in seq_len(maxit)) {
    sv <- svd(Z)
    valeurs <- sv$d^2 / n
    sigma2 <- n * p / min(p, n - 1) * sum(valeurs[-seq_len(k)]) / ((n - 1) * p - (n - 1) * k - p * k + k^2)
    reduites <- (valeurs[seq_len(k)] - sigma2) / sqrt(valeurs[seq_len(k)]) * sqrt(n)
    R <- sv$u[, seq_len(k), drop = FALSE] %*% diag(reduites, k) %*% t(sv$v[, seq_len(k), drop = FALSE])
    ecart <- sqrt(mean((Z[M] - R[M])^2))
    Z[M] <- R[M]
    moyennes <- colMeans(Z)
    Z <- sweep(Z, 2, moyennes)
    cumul <- cumul + moyennes
    if (ecart < tol) break
  }
  Z <- sweep(Z, 2, cumul, "+")
  Xi <- sweep(sweep(Z, 2, s, "*"), 2, mu, "+")
  Xi[!M] <- Xu[!M]  # (exactement les valeurs observées)
  if (bornes) {
    for (j in seq_len(p)) {
      r <- range(Xu[, j], na.rm = TRUE)
      Xi[M[, j], j] <- pmin(pmax(Xi[M[, j], j], r[1]), r[2])
    }
  }
  X[utiles, ] <- Xi
  for (j in seq_len(p)) {
    u <- variables[j]
    if (remplacer) {
      projet$donnees[, as.character(u)] <- X[, j]
    } else {
      # (imputée à nouveau, avec d'autres réglages : la même variable, mise à jour)
      projet <- ajouter_colonne(projet, paste0(projet$noms_var[[as.character(u)]], traduire(" (imputée)", " (imputed)")), obs, X[, j])
    }
  }
  attr(projet, "imputation") <- list(valeurs = sum(M), tours = it, composantes = k, laissees = sum(!utiles))
  projet
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
