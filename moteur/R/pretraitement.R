# Construction des blocs X et Y d'un modèle : valeur de chaque terme, transformation,
# centrage et mise à l'échelle.
#
# Un terme est :
#   - une variable quantitative ;
#   - une catégorie d'une variable qualitative (0/1, « variable indicatrice ») ;
#   - un terme croisé (produit de deux termes ou plus, par exemple variete*culture).
# Un terme croisé est calculé à partir de ses facteurs déjà centrés et mis à l'échelle,
# puis centre et met à l'échelle le produit à son tour.

# Transformation d'un terme. Type 7 (celui des fichiers .usp) : (C1 * x + C2) ^ C3, par exemple
# C3 = 0.25 pour la racine quatrième ; l'application ajoute "log10" : log10(C1 * x + C2).
# Les valeurs impossibles deviennent manquantes.
transformer <- function(x, transformation) {
  if (is.null(transformation)) return(x)
  p <- transformation$params
  if (identical(as.character(transformation$type), "log10")) {
    base <- p[2] * x + p[3]
    y <- suppressWarnings(log10(base))
    y[which(base <= 0)] <- NA
    return(y)
  }
  if (!identical(as.character(transformation$type), "7")) {
    stop("Transformation de type ", transformation$type, " non prise en charge")
  }
  base <- p[2] * x + p[3]
  interdit <- (base < 0 & p[4] != round(p[4])) | (base == 0 & p[4] < 0)
  y <- base^p[4]
  y[which(interdit)] <- NA
  y
}

# Catégorie du facteur j d'un terme (0 = variable quantitative).
categorie <- function(terme, j = 1) {
  cats <- terme$categories
  if (length(cats) >= j && !is.na(cats[j])) cats[j] else 0
}

# Valeurs d'un terme simple (une seule variable), avant mise à l'échelle.
valeurs_simples <- function(projet, terme, observations, j = 1) {
  x <- colonne(projet, terme$variables[j], observations)
  cat <- categorie(terme, j)
  if (cat != 0) x <- ifelse(is.na(x), NA, as.numeric(x == cat))
  x
}

# Centre et poids d'échelle d'un terme, calculés sur les observations du modèle.
# UV : variance unité (poids 1/écart-type) ; Par : Pareto (1/racine de l'écart-type) ;
# Ctr : centrage seul ; none : ni centrage ni mise à l'échelle.
echelle <- function(x, type) {
  v <- x[!is.na(x)]
  moy <- if (length(v)) mean(v) else 0
  et <- if (length(v) > 1) stats::sd(v) else 0
  switch(type,
         UV = c(centre = moy, poids = if (et > 0) 1 / et else 0),
         Par = c(centre = moy, poids = if (et > 0) 1 / sqrt(et) else 0),
         Ctr = c(centre = moy, poids = 1),
         none = c(centre = 0, poids = 1),
         stop("Mise à l'échelle inconnue : ", type))
}

# Bloc X ou Y d'un modèle : matrice des termes centrés et mis à l'échelle
# (lignes = observations du modèle), avec le centre et le poids de chaque terme.
#   poids = "calcules"   : centres et poids recalculés à partir des données ;
#   poids = "enregistres" : ceux que le fichier .usp avait enregistrés.
# `imposees` : centres et poids d'un modèle déjà ajusté (son bloc), pour y projeter de
# nouvelles observations. `facteurs` sert aux termes croisés du bloc Y, qui prennent leurs
# facteurs dans le bloc X.
construire_bloc <- function(projet, termes, observations, poids = c("calcules", "enregistres"),
                            facteurs = NULL, imposees = NULL) {
  poids <- match.arg(poids)
  K <- length(termes)
  X <- matrix(NA_real_, length(observations), K)
  centre <- poids_k <- rep(NA_real_, K)
  type <- character(K)
  noms <- vapply(termes, function(t) as.character(t$name), "")
  simples <- if (is.null(facteurs)) list() else facteurs

  mettre_a_echelle <- function(k, x) {
    ech <- termes[[k]]$scaling
    connu <- !is.null(ech$type) && !is.na(ech$type)
    if (!is.null(imposees)) {
      cw <- c(centre = imposees$centre[k], poids = imposees$poids[k])
      type[k] <<- imposees$type[k]
    } else if (poids == "enregistres" || !connu) {  # échelle non reconnue : on garde celle du fichier
      cw <- c(centre = ech$centre %||% 0, poids = ech$weight %||% 1)
      type[k] <<- if (connu) ech$type else "fixe"
    } else {
      cw <- echelle(x, ech$type)
      type[k] <<- ech$type
    }
    centre[k] <<- cw[["centre"]]
    poids_k[k] <<- cw[["poids"]]
    (x - cw[["centre"]]) * cw[["poids"]]
  }

  croises <- which(vapply(termes, function(t) length(t$variables) > 1, TRUE))
  for (k in setdiff(seq_len(K), croises)) {
    t <- termes[[k]]
    x <- transformer(valeurs_simples(projet, t, observations), t$transform)
    X[, k] <- mettre_a_echelle(k, x)
    simples[[paste(t$variables[1], categorie(t))]] <- X[, k]
  }
  for (k in croises) {
    t <- termes[[k]]
    x <- rep(1, length(observations))
    for (j in seq_along(t$variables)) {
      f <- simples[[paste(t$variables[j], categorie(t, j))]]
      if (is.null(f)) stop("Terme croisé ", noms[k], " : facteur absent du modèle")
      x <- x * f
    }
    X[, k] <- mettre_a_echelle(k, transformer(x, t$transform))
  }
  colnames(X) <- noms
  rownames(X) <- as.character(observations)
  list(X = X, centre = centre, poids = poids_k, type = type, noms = noms, simples = simples)
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
