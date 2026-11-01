# Modèles de classe (Wold, 1976) : une ACP par classe d'une variable qualitative, ajustée sur
# les seules observations de cette classe, avec ses propres centres et échelles et son nombre
# de composantes. Une observation est acceptée par le modèle d'une classe quand sa distance au
# modèle (DModX, rapportée à l'écart-type résiduel du modèle) ne dépasse pas la limite
# critique DCrit à 5 % : elle peut appartenir à une classe, à plusieurs, ou à aucune (une
# observation d'un autre type que toutes les classes connues). Le graphique de Coomans
# (Coomans et al., 1984) montre les distances de chaque observation à deux de ces modèles.

# Les définitions des modèles de classe : la définition d'ACP `d` (variables, échelles,
# transformations, observations retenues), une fois par catégorie de `classe`, sur les
# observations de cette catégorie (3 au moins). Les modèles d'un même groupe portent
# groupe_classes = list(nom, variable, classe).
modeles_classe <- function(projet, d, classe, nom = d$name) {
  v <- variable_du_projet(projet, classe)
  if (!length(v$categories)) stop(traduire("La variable de classe doit être qualitative", "The class variable must be qualitative"))
  valeurs <- colonne(projet, classe, d$observations)
  ds <- lapply(seq_along(v$categories), function(k) {
    e <- d
    e$observations <- d$observations[!is.na(valeurs) & valeurs == k]
    e$name <- sprintf("%s.%s", nom, v$categories[k])
    e$groupe_classes <- list(nom = nom, variable = as.numeric(classe), classe = as.character(v$categories[k]))
    e
  })
  Filter(function(e) length(e$observations) >= 3, ds)
}

# Les distances des observations à chaque modèle d'un groupe. `modeles` : une liste de
# list(definition, modele), un par classe ; `observations` : celles à classer (toutes celles du
# projet par défaut, y compris celles qu'aucun modèle n'a vues). Chaque observation est
# projetée sur chaque modèle comme une observation à prédire (prediction.R).
# Renvoie, par observation, sa classe connue, sa distance à chaque modèle et les classes qui
# l'acceptent ; par modèle, sa limite DCrit ; et la table des classes connues (en lignes) par
# modèle (en colonnes) : la part des observations acceptées.
appartenance_classes <- function(projet, modeles, observations = NULL) {
  obs <- observations %||% numeros_observations(projet)
  classes <- vapply(modeles, function(x) as.character(x$definition$groupe_classes$classe), "")
  variable <- modeles[[1]]$definition$groupe_classes$variable
  vraie <- as.character(variable_du_projet(projet, variable)$categories)[colonne(projet, variable, obs)]
  distances <- vapply(modeles, function(x) predire(projet, x$modele, x$definition, obs)$DModX %||% rep(NA_real_, length(obs)),
                      numeric(length(obs)))
  distances <- matrix(distances, length(obs), length(modeles), dimnames = list(NULL, classes))
  dcrit <- vapply(modeles, function(x) x$modele$statistiques$limites[["DCrit(5%)"]] %||% NA_real_, 0)
  acceptee <- sweep(distances, 2, dcrit, "<=")
  acceptee[is.na(acceptee)] <- FALSE
  tab <- data.frame(uid = obs, nom = unname(projet$noms_obs[as.character(obs)]), classe = vraie, stringsAsFactors = FALSE)
  for (k in seq_along(classes)) tab[[paste("DModX", classes[k])]] <- distances[, k]
  tab$classes <- apply(acceptee, 1, function(r) paste(classes[r], collapse = ", "))
  lignes <- c(classes, if (anyNA(vraie)) NA)
  parts <- t(vapply(lignes, function(c) {
    ici <- if (is.na(c)) is.na(vraie) else vraie %in% c
    colMeans(acceptee[ici, , drop = FALSE])
  }, numeric(length(classes))))
  parts <- matrix(parts, length(lignes), length(classes), dimnames = list(ifelse(is.na(lignes), "?", lignes), classes))
  list(observations = tab, classes = classes, dcrit = stats::setNames(dcrit, classes), table = parts,
       effectifs = vapply(lignes, function(c) sum(if (is.na(c)) is.na(vraie) else vraie %in% c), 0))
}
