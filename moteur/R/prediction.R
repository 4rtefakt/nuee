# Prédiction de nouvelles observations (jeu de prédiction) avec un modèle ajusté.
#
# Les nouvelles observations sont transformées, centrées et mises à l'échelle avec les
# centres et poids du modèle, puis projetées composante par composante, en ignorant leurs
# valeurs manquantes :  t_a = somme(e * w_a) / somme(w_a^2)  (w = p pour une ACP),
# puis  e <- e - t_a * p_a. Comme pour l'ajustement (nipals.R), le facteur de correction
# des valeurs manquantes est limité à 3 : une observation du modèle retrouve ainsi ses scores.

predire <- function(projet, m, definition, observations) {
  pls <- identical(m$type, "PLS")
  bx <- construire_bloc(projet, definition$x, observations, imposees = m$x, spectres = definition$spectres)
  X <- preparer(bx$X)
  E <- X$E
  A <- m$A
  n <- nrow(E)
  T <- matrix(NA_real_, n, A)
  # OPLS : les composantes orthogonales (après les Ap prédictives) sont retirées avant les prédictives
  Ap <- m$Ap %||% 1
  ordre <- if (isTRUE(m$opls)) c(seq_len(A)[-seq_len(Ap)], seq_len(Ap)) else seq_len(A)
  for (a in ordre) {
    v <- if (pls) m$W[, a] else m$P[, a]
    tt <- projeter(E, X$M, v)
    tt[rowSums(X$M) == 0] <- NA
    T[, a] <- tt
    E <- (E - outer(ifelse(is.na(tt), 0, tt), m$P[, a])) * X$M
  }
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
      out[[paste(ny[j], "prédit")]] <- Yp[, j] / m$y$poids[j] + m$y$centre[j]
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
# Les colonnes du fichier sont reconnues par le nom des variables du projet (sans tenir
# compte des majuscules ni des espaces autour) ; une variable qualitative l'est par le nom de
# ses catégories. Les variables du modèle absentes du fichier restent manquantes (la
# prédiction les ignore), sauf si `completer(nom)` sait les calculer (par exemple les scores
# d'un modèle de base pour un modèle hiérarchique) : il renvoie alors leurs valeurs.
predire_fichier <- function(projet, m, definition, chemin, completer = NULL) {
  tableau <- importer_tableau(chemin, secondaires = FALSE)
  norme <- function(s) tolower(trimws(s))
  noms_fichier <- norme(vapply(tableau$variables, function(v) as.character(v$ids[1]), ""))
  utilises <- unique(unlist(lapply(c(definition$x, definition$y), function(t) t$variables)))
  variables_x <- unique(unlist(lapply(definition$x, function(t) t$variables)))
  obs <- seq_len(nrow(tableau$donnees))
  donnees <- matrix(NA_real_, length(obs), length(utilises), dimnames = list(as.character(obs), as.character(utilises)))
  absentes <- character(0)
  for (u in utilises) {
    v <- variable_du_projet(projet, u)
    nom <- as.character(v$ids[1])
    k <- match(norme(nom), noms_fichier)
    if (is.na(k)) {
      x <- if (!is.null(completer)) completer(nom)
      # un Y absent est normal (on le prédit) : seuls les X manquants sont signalés
      if (!is.null(x)) donnees[, as.character(u)] <- x else if (u %in% variables_x) absentes <- c(absentes, nom)
      next
    }
    x <- tableau$donnees[, k]
    if (length(v$categories)) {  # catégories reconnues par leur nom
      etiquettes <- if (length(tableau$variables[[k]]$categories)) tableau$variables[[k]]$categories[x] else as.character(x)
      x <- match(norme(etiquettes), norme(v$categories))
    }
    donnees[, as.character(u)] <- x
  }
  provisoire <- list(donnees = donnees)
  out <- predire(provisoire, m, definition, obs)
  out$nom <- unname(tableau$noms_obs[as.character(obs)])
  list(observations = out, absentes = absentes, n = length(obs), fichier = basename(chemin))
}
