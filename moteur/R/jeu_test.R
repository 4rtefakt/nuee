# Jeu de test (validation externe) : des observations laissées hors du modèle (definition$test,
# voir definir_modele), puis prédites par lui, pour juger ses prédictions sur des données qu'il
# n'a pas vues (ni pour l'ajustement, ni pour la validation croisée).
#   PLS, OPLS : pour chaque Y, RMSEP (racine de la moyenne des carrés des erreurs de
#     prédiction), biais (moyenne de prédit - observé), SEP (écart-type de ces erreurs) et Q2
#     externe : 1 - somme (observé - prédit)^2 / somme (observé - moyenne des observations du
#     modèle)^2 ;
#   PLS-DA, OPLS-DA : la classe prédite (la plus grande valeur de Y) contre la vraie classe ;
#   tous les modèles : le nombre d'observations hors du domaine du modèle (DModX ou T2).
# choisir_test() propose un jeu de test : au hasard, ou par la méthode de Kennard et Stone
# (1969), qui garde pour le modèle les observations les plus éloignées les unes des autres
# (elles couvrent l'étendue des données) et laisse les autres pour le test ; par classe quand
# une variable de classe est donnée (PLS-DA), pour que chaque classe y soit.

evaluer_test <- function(projet, m, definition) {
  test <- definition$test
  pr <- predire(projet, m, definition, test)
  obs <- data.frame(uid = test, nom = unname(projet$noms_obs[as.character(test)]), hors_domaine = pr$hors_domaine,
                    stringsAsFactors = FALSE)
  res <- list(n = length(test), hors_domaine = sum(pr$hors_domaine, na.rm = TRUE))
  if (identical(m$type, "PLS")) {
    by <- construire_bloc(projet, definition$y, test, imposees = m$y)
    ny <- m$y$noms
    stats <- NULL
    for (j in seq_along(ny)) {
      observe <- by$X[, j] / m$y$poids[j] + m$y$centre[j]
      predit <- pr[[paste(ny[j], "prédit")]]
      obs[[paste(ny[j], "observé")]] <- observe
      obs[[paste(ny[j], "prédit")]] <- predit
      e <- predit - observe
      ok <- !is.na(e)
      moyenne <- mean(m$statistiques$observations[[paste(ny[j], "observé")]], na.rm = TRUE)
      stats <- rbind(stats, data.frame(y = ny[j], n = sum(ok), RMSEP = sqrt(mean(e[ok]^2)), biais = mean(e[ok]),
                                       SEP = if (sum(ok) > 1) stats::sd(e[ok]) else NA_real_,
                                       Q2ext = 1 - sum(e[ok]^2) / sum((observe[ok] - moyenne)^2), stringsAsFactors = FALSE))
    }
    res$statistiques <- stats
    if (m$plsda) {
      la_plus_grande <- function(v) if (all(is.na(v))) NA else ny[which.max(v)]
      obs$classe <- apply(as.matrix(obs[paste(ny, "observé")]), 1, la_plus_grande)
      obs$classe_predite <- pr$classe_predite
      connues <- !is.na(obs$classe) & !is.na(obs$classe_predite)
      res$classement <- list(bien_classees = mean(obs$classe[connues] == obs$classe_predite[connues]), classes = ny,
                             table = unname(as.matrix(table(factor(obs$classe, ny), factor(obs$classe_predite, ny)))))
    }
  }
  res$observations <- obs
  res
}

# Les numéros des observations d'un jeu de test, une part `part` de `observations` (par classe
# si `classe` est donnée). Kennard–Stone travaille sur les variables X quantitatives `x`,
# centrées et réduites (une valeur manquante compte pour la moyenne).
choisir_test <- function(projet, x, observations, methode = c("hasard", "kennard_stone"), part = 0.25, classe = NULL) {
  methode <- match.arg(methode)
  groupes <- if (is.null(classe)) rep(1, length(observations)) else colonne(projet, classe, observations)
  if (methode == "kennard_stone") {
    quant <- Filter(function(u) !length(variable_du_projet(projet, u)$categories), as.numeric(x))
    X <- vapply(quant, function(u) colonne(projet, u, observations), numeric(length(observations)))
    X <- matrix(X, length(observations))
    X <- scale(X)
    X[!is.finite(X)] <- 0
  }
  test <- numeric(0)
  for (g in unique(groupes[!is.na(groupes)])) {
    ici <- which(groupes == g)
    k <- round(part * length(ici))
    if (k < 1 || length(ici) - k < 2) next  # trop peu d'observations dans cette classe
    garde <- if (methode == "hasard") setdiff(seq_along(ici), sample.int(length(ici), k))
             else kennard_stone(X[ici, , drop = FALSE], length(ici) - k)
    test <- c(test, observations[ici][-garde])
  }
  sort(test)
}

# Kennard et Stone (1969) : les k lignes de X choisies une à une, en partant des deux plus
# éloignées, puis chaque fois la ligne la plus loin (distance euclidienne) de celles déjà
# choisies. (Toutes les distances nulles : lignes identiques, ou aucun terme quantitatif : les
# deux premières, et non deux fois la même.)
kennard_stone <- function(X, k) {
  D <- as.matrix(stats::dist(X))
  choisies <- if (isTRUE(max(D) > 0)) as.vector(arrayInd(which.max(D), dim(D))) else 1:2
  while (length(choisies) < k) {
    reste <- setdiff(seq_len(nrow(X)), choisies)
    plus_proche <- apply(D[reste, choisies, drop = FALSE], 1, min)
    choisies <- c(choisies, reste[which.max(plus_proche)])
  }
  choisies[seq_len(k)]
}
