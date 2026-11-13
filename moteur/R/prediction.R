# Prédiction de nouvelles observations (jeu de prédiction) avec un modèle ajusté.
#
# Les nouvelles observations sont transformées, centrées et mises à l'échelle avec les
# centres et poids du modèle, puis projetées composante par composante, en ignorant leurs
# valeurs manquantes :  t_a = somme(e * w_a) / somme(w_a^2)  (w = p pour une ACP),
# puis  e <- e - t_a * p_a. Comme pour l'ajustement (nipals.R), le facteur de correction
# des valeurs manquantes est limité à 3 : une observation du modèle retrouve ainsi ses scores.

predire <- function(projet, m, definition, observations) {
  if (est_regression(definition)) return(predire_observations_regression(projet, m, definition, observations))
  if (est_autre_analyse(definition)) return(predire_analyse(projet, m, definition, observations))  # analyses.R
  pls <- identical(m$type, "PLS")
  A <- m$A
  sp <- scores_projetes(projet, m, definition, observations)  # (hierarchique.R)
  T <- sp$T
  E <- sp$E
  X <- list(M = sp$M)
  s <- m$statistiques
  notes <- !is.na(m$T[, 1])
  var_t <- colSums(m$T[notes, , drop = FALSE]^2) / (sum(notes) - 1)
  out <- data.frame(row.names = as.character(observations))
  for (a in seq_len(A)) out[[paste0("t", a)]] <- T[, a]
  out$T2 <- rowSums(sweep(T^2, 2, var_t, "/"))
  Ki <- rowSums(X$M)
  if (!is.null(s$s0)) {
    out$DModX <- sqrt(rowSums(E^2) / ifelse(Ki > A, Ki - A, NA)) / s$s0
    out$DModX_pondere <- sqrt(rowSums(sweep(E, 2, s$pouvoir_modelisation, "*")^2) / ifelse(Ki > A, Ki - A, NA)) / s$sw0
    out$DModX[Ki <= A] <- NA
    out$DModX_pondere[Ki <= A] <- NA
  }
  if (pls) {
    ny <- m$y$noms
    Yp <- ifelse(is.na(T), 0, T) %*% t(m$C)
    for (j in seq_along(ny)) {
      # (un Y constant dans le modèle, de poids 0 : prédit par sa valeur, comme en validation croisée)
      out[[paste(ny[j], "prédit")]] <- Yp[, j] / (if (isTRUE(m$y$poids[j] == 0)) Inf else m$y$poids[j]) + m$y$centre[j]
      out[[paste(ny[j], "prédit")]][is.na(T[, 1])] <- NA
    }
    if (m$plsda) {
      cols <- paste(ny, "prédit")
      out$classe_predite <- apply(out[cols], 1, function(v) if (all(is.na(v))) NA else ny[which.max(v)])
    }
  }
  # hors du domaine du modèle : trop loin du modèle (DModX > DCrit) ou trop loin de son
  # centre (T2 au-delà de la limite à 95 %)
  hors <- !is.na(out$T2) & out$T2 > (s$limites[["T2Crit(95%)"]] %||% Inf)
  dcrit <- s$limites[["DCrit(5%)"]]
  if (!is.null(out$DModX) && !is.null(dcrit)) hors <- hors | (!is.na(out$DModX) & out$DModX > dcrit)
  out$hors_domaine <- hors
  out
}

# Prédiction des observations d'un fichier (CSV ou Excel, une ligne par observation).
# Les colonnes du fichier sont reconnues par le nom des variables du projet (nom_comparable :
# sans tenir compte des majuscules, des espaces autour, ni d'un « . » à la place d'une espace) ;
# une variable qualitative l'est par le nom de ses catégories. La colonne d'une variable
# quantitative du modèle est lue en nombres, même si elle a du texte : ces valeurs (« <0,5 »,
# « nd »...) sont manquantes, et `note` le dit (avec ce qu'il faut dire du fichier). Les
# variables du modèle absentes du fichier restent manquantes (la prédiction les ignore), sauf
# si `completer(nom)` sait les calculer (par exemple les scores d'un modèle de base pour un
# modèle hiérarchique) : il renvoie alors leurs valeurs. Une régression lit aussi l'effort de son
# décalage ; sans lui dans le fichier, elle prédit pour un effort de 1, et `note` le dit.
predire_fichier <- function(projet, m, definition, chemin, completer = NULL) {
  norme <- function(s) tolower(trimws(s))
  regression <- est_regression(definition)
  effort <- if (regression && !is.null(definition$decalage)) as.numeric(definition$decalage)
  utilises <- unique(c(unlist(lapply(c(definition$x, definition$y), function(t) t$variables)), effort))
  variables_x <- unique(unlist(lapply(definition$x, function(t) t$variables)))
  nom_variable <- function(u) as.character(variable_du_projet(projet, u)$ids[1])
  quantitatives <- vapply(Filter(function(u) !length(variable_du_projet(projet, u)$categories), utilises), nom_variable, "")
  df <- lire_tableau(chemin, quantitatives = quantitatives)
  tableau <- projet_depuis_tableau(df, basename(chemin), secondaires = FALSE)
  noms_fichier <- nom_comparable(vapply(tableau$variables, function(v) as.character(v$ids[1]), ""))
  obs <- seq_len(nrow(tableau$donnees))
  donnees <- matrix(NA_real_, length(obs), length(utilises), dimnames = list(as.character(obs), as.character(utilises)))
  absentes <- character(0)
  effort_absent <- FALSE
  inconnue <- rep(FALSE, length(obs))  # (une catégorie d'une variable X qualitative que le projet n'a pas)
  for (u in utilises) {
    v <- variable_du_projet(projet, u)
    nom <- as.character(v$ids[1])
    k <- match(nom_comparable(nom), noms_fichier)
    if (is.na(k)) {
      x <- if (!is.null(completer)) completer(nom)
      # un Y absent est normal (on le prédit) : seuls les X manquants sont signalés
      if (!is.null(x)) donnees[, as.character(u)] <- x else if (u %in% variables_x) absentes <- c(absentes, nom)
      if (u %in% effort) effort_absent <- TRUE
      next
    }
    x <- tableau$donnees[, k]
    if (length(v$categories)) {  # catégories reconnues par leur nom
      etiquettes <- if (length(tableau$variables[[k]]$categories)) tableau$variables[[k]]$categories[x] else as.character(x)
      x <- match(norme(etiquettes), norme(v$categories))
      if (u %in% variables_x) inconnue <- inconnue | (!is.na(etiquettes) & nzchar(trimws(etiquettes)) & is.na(x))
    }
    donnees[, as.character(u)] <- x
  }
  provisoire <- list(donnees = donnees)
  out <- if (regression) predire_observations_regression(provisoire, m, definition, obs, effort_absent) else predire(provisoire, m, definition, obs)
  # (une régression : une catégorie que le projet n'a pas, sans prédiction, est aussi une catégorie que
  # le modèle n'a pas vue)
  if (regression && any(inconnue)) out$categorie_inconnue <- inconnue | (out$categorie_inconnue %||% FALSE)
  out$nom <- unname(tableau$noms_obs[as.character(obs)])
  # ce qu'il faut dire de la lecture : du fichier, et des colonnes des variables X
  note <- attr(df, "note") %||% character(0)
  colonnes <- names(note) %||% character(length(note))
  note <- unname(note[!nzchar(colonnes) | nom_comparable(colonnes) %in% nom_comparable(vapply(c(variables_x, effort), nom_variable, ""))])
  if (effort_absent) note <- c(note, sprintf(traduire("l'effort (%s) n'est pas dans le fichier : prédictions pour un effort de 1",
                                                      "the effort (%s) is not in the file: predictions for an effort of 1"), nom_variable(effort)))
  list(observations = out, absentes = absentes, n = length(obs), fichier = basename(chemin), note = I(note))
}
