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
# C3 = 0.25 pour la racine quatrième. L'application ajoute (params = c(NA, C1, C2)) :
#   "log10" : log10(C1 * x + C2) ;   "ln" : ln(C1 * x + C2) ;   "neglog" : -log10(C1 * x + C2) ;
#   "exp" : exp(C1 * x + C2) ;       "lineaire" : C1 * x + C2 ;
#   "logit" : ln((x - C1) / (C2 - x)), C1 et C2 les bornes des valeurs (0 et 1 pour des
#   proportions, 0 et 100 pour des pourcentages).
# Les valeurs impossibles deviennent manquantes.
transformer <- function(x, transformation) {
  if (is.null(transformation)) return(x)
  p <- as.numeric(transformation$params)
  type <- as.character(transformation$type)
  if (type %in% c("log10", "ln", "neglog")) {
    base <- p[2] * x + p[3]
    y <- suppressWarnings(switch(type, log10 = log10(base), ln = log(base), neglog = -log10(base)))
    y[which(base <= 0)] <- NA
    return(y)
  }
  if (type == "exp") {
    y <- exp(p[2] * x + p[3])
    y[which(!is.finite(y))] <- NA
    return(y)
  }
  if (type == "lineaire") return(p[2] * x + p[3])
  if (type == "logit") {
    y <- suppressWarnings(log((x - p[2]) / (p[3] - x)))
    y[which(!(x > p[2] & x < p[3]))] <- NA
    return(y)
  }
  if (!identical(type, "7")) {
    stop(sprintf(traduire("Transformation de type %s non prise en charge", "Transformation of type %s not supported"), transformation$type))
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
# Ctr : centrage seul ; none : ni centrage ni mise à l'échelle. `ecart` : l'écart-type sur lequel
# le poids est calculé (UV, Pareto ; manquant sinon), par défaut celui de x.
echelle <- function(x, type, ecart = NULL) {
  v <- x[!is.na(x)]
  moy <- if (length(v)) mean(v) else 0
  et <- ecart %||% (if (length(v) > 1) stats::sd(v) else 0)
  switch(type,
         UV = c(centre = moy, poids = if (et > 0) 1 / et else 0, ecart = et),
         Par = c(centre = moy, poids = if (et > 0) 1 / sqrt(et) else 0, ecart = et),
         Ctr = c(centre = moy, poids = 1, ecart = NA),
         none = c(centre = 0, poids = 1, ecart = NA),
         stop(traduire("Mise à l'échelle inconnue : ", "Unknown scaling: "), type))
}

# Pondération d'un terme, en plus de sa mise à l'échelle : son poids (UV, Pareto, centrage) est
# encore multiplié
#   - par son modificateur (scaling$modifier, 1 par défaut), qui augmente ou diminue son
#     influence ;
#   - par 1 / racine(K) s'il fait partie d'un bloc (scaling$block, la pondération par blocs) :
#     K est le nombre de termes du bloc dans le modèle. La variance totale d'un bloc de termes en
#     UV vaut alors 1 au lieu de K : un bloc de 20 variables ne pèse pas 20 fois plus qu'une
#     variable seule.
# Ces réglages viennent des fichiers .usp (le poids enregistré de chaque terme est bien son
# poids UV ou de centrage × modificateur × 1/racine(K) : vérifié sur 2530 termes de 132 modèles)
# ou de Nouveau modèle. Le facteur ne dépend pas des données : chaque sous-modèle de la
# validation croisée a le même.
facteurs_ponderation <- function(termes) {
  blocs <- vapply(termes, function(t) {
    b <- t$scaling$block
    if (is.null(b) || !length(b) || is.na(b[1]) || !nzchar(as.character(b[1]))) NA_character_ else as.character(b[1])
  }, "")
  K <- table(blocs[!is.na(blocs)])
  modificateur <- vapply(termes, function(t) as.numeric(t$scaling$modifier %||% 1), 0)
  modificateur * ifelse(is.na(blocs), 1, 1 / sqrt(as.numeric(K[blocs])))
}

# Bloc X ou Y d'un modèle : matrice des termes centrés et mis à l'échelle
# (lignes = observations du modèle), avec le centre et le poids de chaque terme.
#   poids = "calcules"   : centres et poids recalculés à partir des données (puis pondérés,
#                          facteurs_ponderation) ;
#   poids = "enregistres" : ceux que le fichier .usp avait enregistrés (pondération comprise).
# `imposees` : centres et poids d'un modèle déjà ajusté (son bloc), pour y projeter de
# nouvelles observations. `facteurs` sert aux termes croisés du bloc Y, qui prennent leurs
# facteurs dans le bloc X. `spectres` : prétraitement des spectres (spectres.R), appliqué aux
# termes quantitatifs simples, observation par observation, avant la mise à l'échelle.
# `bruts` : les valeurs des termes simples de ces observations (valeurs_bloc), déjà lues (la
# validation croisée les lit une fois pour tous ses sous-modèles).
# `complet` (un sous-modèle de la validation croisée, validation_croisee.R) : list(ecart, seuil),
# l'écart-type de chaque terme dans le modèle complet ; un terme UV ou Pareto dont l'écart-type
# sur ces observations est sous `seuil` fois le sien prend le poids du modèle complet (seuil
# infini : tous). Le bloc rend dans `gardes` la part de son écart-type que garde chacun de ces
# termes, pour un seuil fini et un terme qui n'est pas constant (constant, il est prédit de même
# avec le poids du modèle complet ou un poids 0).
construire_bloc <- function(projet, termes, observations, poids = c("calcules", "enregistres"),
                            facteurs = NULL, imposees = NULL, spectres = NULL, bruts = NULL, complet = NULL) {
  poids <- match.arg(poids)
  K <- length(termes)
  X <- matrix(NA_real_, length(observations), K)
  centre <- poids_k <- ecart <- rep(NA_real_, K)
  gardes <- stats::setNames(numeric(0), character(0))
  type <- character(K)
  noms <- vapply(termes, function(t) as.character(t$name), "")
  simples <- if (is.null(facteurs)) list() else facteurs
  # la pondération de chaque terme (facteurs_ponderation), comprise dans son poids quand il est
  # calculé ; celle d'un modèle déjà ajusté avec ses poids (`imposees`) ; 1 pour un poids
  # enregistré, qui la comprend déjà
  ponderation <- if (K) facteurs_ponderation(termes) else numeric(0)
  facteur <- rep(1, K)

  mettre_a_echelle <- function(k, x) {
    ech <- termes[[k]]$scaling
    connu <- !is.null(ech$type) && !is.na(ech$type)
    if (!is.null(imposees)) {
      cw <- c(centre = imposees$centre[k], poids = imposees$poids[k])
      type[k] <<- imposees$type[k]
      facteur[k] <<- imposees$facteur[k] %||% 1
    } else if (poids == "enregistres" || !connu) {  # échelle non reconnue : on garde celle du fichier
      cw <- c(centre = ech$centre %||% 0, poids = ech$weight %||% 1)
      type[k] <<- if (connu) ech$type else "fixe"
    } else {
      cw <- echelle(x, ech$type)
      ec <- complet$ecart[k]
      if (!is.null(ec) && ech$type %in% c("UV", "Par") && isTRUE(ec > 0) && cw[["ecart"]] < complet$seuil * ec) {
        if (cw[["ecart"]] > 0 && is.finite(complet$seuil)) gardes[[noms[k]]] <<- cw[["ecart"]] / ec
        cw <- echelle(x, ech$type, ecart = ec)
      }
      cw[["poids"]] <- cw[["poids"]] * ponderation[k]
      facteur[k] <<- ponderation[k]
      type[k] <<- ech$type
      ecart[k] <<- cw[["ecart"]]
    }
    centre[k] <<- cw[["centre"]]
    poids_k[k] <<- cw[["poids"]]
    (x - cw[["centre"]]) * cw[["poids"]]
  }

  croises <- which(vapply(termes, function(t) length(t$variables) > 1, TRUE))
  seuls <- setdiff(seq_len(K), croises)
  if (is.null(bruts)) bruts <- valeurs_bloc(projet, termes, observations)
  reference <- NULL
  if (length(spectres)) {
    q <- seuls[vapply(termes[seuls], function(t) categorie(t) == 0, TRUE)]
    spectre <- bruts[, q, drop = FALSE]
    colnames(spectre) <- noms[q]
    r <- pretraiter_spectres(spectre, spectres, imposees$reference, positions_termes(projet, termes[q]))
    bruts[, q] <- r$X
    reference <- r$reference
  }
  for (k in seuls) X[, k] <- mettre_a_echelle(k, bruts[, k])
  # les termes simples par « variable catégorie », pour les termes croisés (d'un coup : une liste
  # qui grandit terme à terme coûte cher avec des milliers de termes)
  simples[vapply(termes[seuls], function(t) paste(t$variables[1], categorie(t)), "")] <- lapply(seuls, function(k) X[, k])
  for (k in croises) {
    t <- termes[[k]]
    x <- rep(1, length(observations))
    for (j in seq_along(t$variables)) {
      f <- simples[[paste(t$variables[j], categorie(t, j))]]
      if (is.null(f)) stop(sprintf(traduire("Terme croisé %s : facteur absent du modèle", "Cross term %s: factor missing from the model"), noms[k]))
      x <- x * f
    }
    X[, k] <- mettre_a_echelle(k, transformer(x, t$transform))
  }
  colnames(X) <- noms
  rownames(X) <- as.character(observations)
  list(X = X, centre = centre, poids = poids_k, type = type, facteur = facteur, noms = noms, simples = simples, reference = reference,
       ecart = ecart, gardes = gardes)
}

# Les valeurs des termes simples d'un bloc pour des observations, transformées (une colonne par
# terme ; celles des termes croisés restent manquantes : construire_bloc les calcule).
valeurs_bloc <- function(projet, termes, observations) {
  bruts <- matrix(NA_real_, length(observations), length(termes))
  croises <- which(vapply(termes, function(t) length(t$variables) > 1, TRUE))
  for (k in setdiff(seq_along(termes), croises)) {
    bruts[, k] <- transformer(valeurs_simples(projet, termes[[k]], observations), termes[[k]]$transform)
  }
  bruts
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
