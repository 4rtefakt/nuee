# Les analyses de communautés fondées sur une distance entre observations, comme le paquet vegan
# les calcule (vegdist, wcmdscale, metaMDS, envfit, adonis2, betadisper, mantel...) : les mêmes
# distances, valeurs propres, statistiques et, pour les mêmes permutations, les mêmes valeurs p.
# vegan n'est pas livré avec Nuée : moteur/tests/verifier_communautes.R le compare quand il est
# installé. Ce sont des analyses de analyses.R (ajuster_pcoa, resultat_pcoa, texte_methodes_pcoa...) :
#   PCOA       analyse en coordonnées principales (Gower 1966 ; Legendre et Legendre 2012, § 9.3) ;
#   NMDS       positionnement multidimensionnel non métrique (Kruskal 1964 ; MASS::isoMDS) ;
#   PERMANOVA  analyse de variance multivariée par permutation (Anderson 2001), avec la
#              dispersion multivariée des groupes (Anderson 2006) ;
#   MANTEL     test de Mantel (Mantel 1967), partiel, et corrélogramme de Mantel.
# Le tableau Y : les espèces (ou toutes variables quantitatives), une observation par ligne ; les
# observations où une valeur Y manque sont laissées de côté et comptées (comme les k-moyennes).
# Les X : les variables ajustées sur l'ordination (PCOA, NMDS), les termes du modèle (PERMANOVA),
# la distance comparée (MANTEL) ; « classe » (PERMANOVA) : un groupement de plus pour la
# dispersion ; Z (MANTEL) : les covariables du test partiel. Leurs réglages sont dans d$options.
# Permutations : celles de ordination.R (permutations_lignes : reproductibles par leur graine,
# libres ou à l'intérieur de groupes, comme how(blocks = ) du paquet permute).
# Leur export vers R est dans communautes_script.R : son script reprend les fonctions de ce
# fichier telles qu'elles y sont écrites (distance_tableau, pcoa_noyau, nmds_noyau...).

# ---------------------------------------------------------------- les réglages

# Les distances (vegan : vegdist), les transformations du tableau avant la distance, les
# corrections des valeurs propres négatives de la PCoA (vegan : wcmdscale(add = )).
DISTANCES_COMMUNAUTES <- c("bray", "jaccard", "gower", "euclidean", "manhattan", "canberra", "kulczynski", "hellinger", "chord")
TRANSFORMATIONS_COMMUNAUTES <- c("aucune", "racine2", "racine4", "log1p", "presence", "relative", "wisconsin", "racine_wisconsin")
CORRECTIONS_DISTANCE <- c("aucune", "lingoes", "cailliez")
# (les distances qui demandent des valeurs positives ou nulles ; celles qu'une observation sans
# aucune valeur laisse sans définition : 0/0)
DISTANCES_POSITIVES <- c("bray", "jaccard", "kulczynski", "canberra", "hellinger")
DISTANCES_SANS_VIDE <- c("bray", "jaccard", "kulczynski", "canberra", "hellinger", "chord")
ZERO_DISTANCE <- 1e-15  # (vegan : une distance plus petite vaut 0)

# Les réglages d'une analyse de communauté (d$options), complétés : distance, présences-absences
# (binaire : Bray-Curtis devient Sørensen), transformation, correction (PCOA, PERMANOVA), dimensions
# et départs (NMDS), permutations, graine et groupes des permutations, termes testés (PERMANOVA :
# "termes" séquentiels, "marges" chacun après les autres, "modele" entier), centre de la dispersion
# ("centroide" ou "mediane" spatiale), corrélation (MANTEL : "pearson" ou "spearman") et classes
# du corrélogramme (0 : règle de Sturges), covariables Z et leurs transformations et échelles.
options_communaute <- function(d) {
  o <- d$options %||% list()
  texte <- function(nom, defaut) {
    v <- as.character(unlist(o[[nom]]))
    if (length(v) && !is.na(v[1]) && nzchar(v[1])) v[1] else defaut
  }
  parmi <- function(v, choix, fr, en) {
    if (!v %in% choix) stop(sprintf(traduire(fr, en), v), call. = FALSE)
    v
  }
  B <- suppressWarnings(as.integer(unlist(o$permutations %||% 999))[1])
  if (is.na(B) || B < 0) B <- 999L
  graine <- suppressWarnings(as.integer(unlist(o$graine %||% 1))[1])
  list(distance = parmi(texte("distance", "bray"), DISTANCES_COMMUNAUTES, "Distance inconnue : %s", "Unknown distance: %s"),
       binaire = isTRUE(as.logical(unlist(o$binaire))[1]),
       transformation = parmi(texte("transformation", "aucune"), TRANSFORMATIONS_COMMUNAUTES, "Transformation inconnue : %s", "Unknown transformation: %s"),
       correction = parmi(texte("correction", "aucune"), CORRECTIONS_DISTANCE, "Correction inconnue : %s", "Unknown correction: %s"),
       k = option_nombre(d, "k", 2, 1, 4, TRUE), departs = option_nombre(d, "departs", 20, 0, 200, TRUE),
       permutations = min(B, 99999L), graine = if (is.na(graine)) 1L else graine,
       strates = if (length(o$strates$by)) specs_groupement(o$strates)[[1]],
       par = parmi(texte("par", "termes"), c("termes", "marges", "modele"), "Test inconnu : %s", "Unknown test: %s"),
       dispersion = parmi(texte("dispersion", "centroide"), c("centroide", "mediane"), "Centre inconnu : %s", "Unknown centre: %s"),
       methode = parmi(texte("methode", "pearson"), c("pearson", "spearman"), "Corrélation inconnue : %s", "Unknown correlation: %s"),
       classes = option_nombre(d, "classes", 0, 0, 50, TRUE),
       distance_x = parmi(texte("distance_x", "variables"), c("variables", "geographique"), "Distance X inconnue : %s", "Unknown X distance: %s"),
       z = as.numeric(unlist(o$z %||% numeric(0))), z_transformations = o$z_transformations %||% list(), z_echelles = o$z_echelles %||% list())
}

# ---------------------------------------------------------------- le tableau et la distance

# Le tableau Y : des variables quantitatives seules, dans leurs unités (transformations de
# l'onglet Variables comprises ; ordination.R : reponses_ordination).
tableau_communaute <- function(projet, d, obs, quoi) {
  if (!length(d$y)) {
    stop(sprintf(traduire("%s demande un tableau Y : les espèces, ou les variables dont on calcule la distance.",
                          "%s needs a table Y: the species, or the variables whose distance is computed."), quoi), call. = FALSE)
  }
  reponses_ordination(projet, d$y, obs, sprintf(traduire("Le tableau Y (%s)", "The table Y (%s)"), quoi))
}

# Le tableau transformé avant la distance : racine carrée, racine quatrième, log(x + 1),
# présences-absences (1 pour une valeur positive), abondances relatives (chaque observation
# divisée par son total), double standardisation de Wisconsin (chaque variable divisée par son
# maximum, puis chaque observation par son total : vegan::wisconsin), précédée ou non d'une racine
# carrée (ce que vegan::metaMDS fait de lui-même pour des abondances au-delà de 50). Une variable
# ou une observation sans aucune valeur reste nulle (vegan y mettrait 0/0).
transformer_communaute <- function(Y, methode) {
  if (identical(methode, "aucune")) return(Y)
  if (any(Y < 0)) {
    stop(traduire("Cette transformation du tableau Y demande des valeurs positives ou nulles (des abondances).",
                  "This transformation of the table Y needs positive or zero values (abundances)."), call. = FALSE)
  }
  relatives <- function(X) { s <- rowSums(X); s[s <= 0] <- 1; X / s }
  wisconsin <- function(X) {
    m <- apply(X, 2, max)
    m[m <= 0] <- 1
    relatives(sweep(X, 2, m, "/"))
  }
  switch(methode,
    racine2 = sqrt(Y), racine4 = Y^0.25, log1p = log1p(Y), presence = (Y > 0) * 1, relative = relatives(Y),
    wisconsin = wisconsin(Y), racine_wisconsin = wisconsin(sqrt(Y)))
}

# Les noms d'au plus cinq observations, pour un message.
quelques_noms <- function(noms) paste0(paste(utils::head(noms, 5), collapse = ", "), if (length(noms) > 5) "…" else "")

# La distance entre les lignes de Y (une matrice n × n), avec les définitions de vegan::vegdist :
#   bray        Σ|xj − yj| / Σ(xj + yj)                       (Bray et Curtis 1957)
#   jaccard     2B / (1 + B), B celle de Bray-Curtis         (sur des présences-absences : Jaccard)
#   kulczynski  1 − ½ (Σ min(xj, yj) / Σxj + Σ min(xj, yj) / Σyj)
#   gower       la moyenne des |xj − yj| / étendue de j, sur les variables qui varient (Gower 1971)
#   euclidean   √Σ(xj − yj)² ;  manhattan  Σ|xj − yj|
#   canberra    la moyenne des |xj − yj| / (xj + yj) sur les variables non nulles dans l'une ou l'autre
#   hellinger   euclidienne sur √(x / Σx) ;  chord  euclidienne sur x / √Σx²  (Legendre et Gallagher 2001)
# `binaire` : sur les présences-absences (Bray-Curtis devient l'indice de Sørensen). Une distance
# sous 1e-15 vaut 0, comme dans vegan. Refusés : des valeurs négatives là où la distance n'a pas de
# sens avec elles, une observation sans aucune valeur là où sa distance n'est pas définie. Le
# calcul lui-même est distance_tableau, que l'export vers R écrit dans son script.
distance_communaute <- function(Y, methode, binaire = FALSE, noms = rownames(Y)) {
  if ((binaire || methode %in% DISTANCES_POSITIVES) && any(Y < 0)) {
    stop(sprintf(traduire("La distance %s demande des valeurs positives ou nulles (des abondances).",
                          "The %s distance needs positive or zero values (abundances)."), nom_distance(methode, binaire)), call. = FALSE)
  }
  if (methode %in% DISTANCES_SANS_VIDE) {
    vides <- which(rowSums(abs(if (binaire) (Y > 0) * 1 else Y)) == 0)
    if (length(vides)) {
      stop(sprintf(traduire("La distance %s n'est pas définie pour une observation sans aucune valeur (que des zéros) : %s. Retirez-la, ou choisissez une autre distance.",
                            "The %s distance is not defined for an observation without any value (only zeros): %s. Leave it out, or choose another distance."),
                   nom_distance(methode, binaire), quelques_noms(noms[vides])), call. = FALSE)
    }
  }
  distance_tableau(Y, methode, binaire, noms)
}

# La distance entre les lignes de Y, sans les refus de distance_communaute (les formules sont
# dites au-dessus d'elle).
distance_tableau <- function(Y, methode, binaire = FALSE, noms = rownames(Y)) {
  if (binaire) Y <- (Y > 0) * 1
  manhattan <- function(X) as.matrix(stats::dist(X, "manhattan"))
  s <- rowSums(Y)
  bray <- function() manhattan(Y) / outer(s, s, "+")
  D <- switch(methode,
    euclidean = as.matrix(stats::dist(Y)),
    manhattan = manhattan(Y),
    bray = bray(),
    jaccard = { B <- bray(); 2 * B / (1 + B) },
    kulczynski = {
      minimum <- (outer(s, s, "+") - manhattan(Y)) / 2  # (Σ min(x, y) = (Σx + Σy − Σ|x − y|) / 2)
      1 - 0.5 * (minimum / s + t(t(minimum) / s))
    },
    gower = {
      etendue <- apply(Y, 2, function(x) max(x) - min(x))
      varie <- etendue > 0
      if (!any(varie)) stop(traduire("Distance de Gower : aucune variable du tableau Y ne varie.", "Gower distance: no variable of the table Y varies."), call. = FALSE)
      manhattan(sweep(Y[, varie, drop = FALSE], 2, etendue[varie], "/")) / sum(varie)
    },
    canberra = {
      n <- nrow(Y)
      D <- matrix(0, n, n)
      for (i in seq_len(n - 1)) {
        j <- (i + 1):n
        A <- Y[j, , drop = FALSE]
        a <- matrix(Y[i, ], length(j), ncol(Y), byrow = TRUE)
        somme <- A + a  # (des valeurs positives : nulle seulement pour un double zéro, laissé de côté)
        nz <- somme > 0
        D[j, i] <- D[i, j] <- rowSums(ifelse(nz, abs(A - a) / ifelse(nz, somme, 1), 0)) / rowSums(nz)
      }
      D
    },
    hellinger = as.matrix(stats::dist(sqrt(Y / s))),
    chord = as.matrix(stats::dist(Y / sqrt(rowSums(Y^2)))))
  diag(D) <- 0
  D[D < ZERO_DISTANCE] <- 0
  dimnames(D) <- list(noms, noms)
  D
}

# Le nom d'une distance, pour les messages et les textes (la langue de la séance, ou `fr`).
nom_distance <- function(methode, binaire = FALSE, fr = !identical(getOption("nuee.langue"), "en")) {
  if (binaire && methode == "bray") return(if (fr) "de Sørensen" else "Sørensen")
  nom <- switch(methode,
    bray = if (fr) "de Bray-Curtis" else "Bray–Curtis", jaccard = if (fr) "de Jaccard" else "Jaccard",
    gower = if (fr) "de Gower" else "Gower", euclidean = if (fr) "euclidienne" else "Euclidean",
    manhattan = if (fr) "de Manhattan" else "Manhattan", canberra = if (fr) "de Canberra" else "Canberra",
    kulczynski = if (fr) "de Kulczynski" else "Kulczynski", hellinger = if (fr) "de Hellinger" else "Hellinger",
    chord = if (fr) "de corde" else "chord", methode)
  if (binaire && methode != "jaccard") paste(nom, if (fr) "(présences-absences)" else "(presence–absence)") else nom
}

# Le nom court d'une distance, pour la ligne d'un modèle dans l'explorateur.
nom_court_distance <- function(methode, binaire = FALSE) {
  if (binaire && methode == "bray") return("Sørensen")
  nom <- switch(methode, bray = "Bray-Curtis", euclidean = traduire("euclidienne", "Euclidean"), chord = traduire("corde", "chord"),
                paste0(toupper(substr(methode, 1, 1)), substr(methode, 2, nchar(methode))))
  if (binaire && methode != "jaccard") paste(nom, "0/1") else nom
}

# Double centrage de Gower : moyennes des colonnes, puis des lignes, retirées.
centrage_double <- function(M) {
  M <- sweep(M, 2, colMeans(M))
  sweep(M, 1, rowMeans(M))
}

# La correction d'une distance dont la PCoA a des valeurs propres négatives (vegan : wcmdscale(add =),
# Legendre et Legendre 2012, § 9.3.4) : Lingoes (1971), √(d² + 2c) hors de la diagonale, c la plus
# grande valeur absolue des valeurs propres négatives de −½ D² doublement centrée ; Cailliez (1983),
# d + c, c la plus grande valeur propre réelle de la matrice 2n × 2n [0, −Δ1 ; −I, Δ2] (Δ1 : D²
# doublement centrée, Δ2 : 2D doublement centrée). Rend la distance corrigée et sa constante.
corriger_distance <- function(D, correction) {
  hors <- row(D) != col(D)
  if (identical(correction, "lingoes")) {
    c1 <- max(0, -min(eigen(-0.5 * centrage_double(D^2), symmetric = TRUE, only.values = TRUE)$values))
    D[hors] <- sqrt(D[hors]^2 + 2 * c1)
    return(list(D = D, constante = c1))
  }
  if (identical(correction, "cailliez")) {
    n <- nrow(D)
    q1 <- seq_len(n)
    q2 <- n + q1
    Z <- matrix(0, 2 * n, 2 * n)
    Z[cbind(q2, q1)] <- -1
    Z[q1, q2] <- -centrage_double(D^2)
    Z[q2, q2] <- centrage_double(2 * D)
    c2 <- max(0, max(Re(eigen(Z, symmetric = FALSE, only.values = TRUE)$values)))
    D[hors] <- D[hors] + c2
    return(list(D = D, constante = c2))
  }
  list(D = D, constante = 0)
}

# Le tableau Y d'une analyse, ses observations et sa distance : les observations où une valeur Y
# manque laissées de côté (et, avec `aussi`, celles où manque une autre valeur dont l'analyse a
# besoin : TRUE pour les observations connues), le tableau transformé (`W`, les poids des scores des
# espèces quand il est positif ou nul), la distance.
donnees_communaute <- function(projet, d, o, quoi, aussi = NULL) {
  obs <- as.numeric(d$observations)
  if (length(d$spectres)) stop(sprintf(traduire("%s ne prend pas de prétraitement de spectres.", "%s takes no spectral preprocessing."), quoi), call. = FALSE)
  Y <- tableau_communaute(projet, d, obs, quoi)
  connu <- stats::complete.cases(Y) & (if (is.null(aussi)) TRUE else aussi)
  garde <- obs[connu]
  if (length(garde) < 3) {
    stop(sprintf(traduire("Trop peu d'observations ont toutes leurs valeurs pour %s (3 au moins).", "Too few observations have all their values for %s (3 or more)."), quoi), call. = FALSE)
  }
  Y <- Y[connu, , drop = FALSE]
  noms <- unname(projet$noms_obs[nom_numero(garde)])
  W <- transformer_communaute(Y, o$transformation)
  D <- distance_communaute(W, o$distance, o$binaire, noms)
  if (all(D == 0)) stop(traduire("Toutes les distances entre observations sont nulles : rien à analyser.", "All the distances between observations are zero: nothing to analyse."), call. = FALSE)
  list(observations = garde, exclues = setdiff(obs, garde), connu = connu, Y = Y, W = W, D = D, noms = noms)
}

# Les groupes des permutations (un identifiant ou une variable qualitative : le site), pour les
# observations `obs` (NA quand il manque), ou NULL.
strates_communaute <- function(projet, o, obs) if (!is.null(o$strates)) valeurs_groupement(projet, o$strates, obs)

verifier_strates <- function(strates) {
  if (!is.null(strates) && all(table(strates) < 2)) {
    stop(traduire("Les groupes des permutations doivent avoir chacun plusieurs observations.", "The permutation groups must each hold several observations."), call. = FALSE)
  }
}

valeur_p_superieure <- function(perm, stat) if (length(perm)) (sum(perm >= stat - EPS_PERMUTATIONS) + 1) / (length(perm) + 1) else NA_real_

# ---------------------------------------------------------------- PCoA

# L'analyse en coordonnées principales d'une distance (vegan : wcmdscale ; Gower 1966) : les valeurs
# et vecteurs propres de G = −½ D² doublement centrée ; une valeur propre dont la valeur absolue est
# sous √ε fois la première (et sous √ε) est nulle. Les coordonnées des observations : les vecteurs
# × √|λ| (sur un axe de valeur propre négative, des coordonnées « imaginaires » dont les carrés se
# retranchent : betadisper) ; chaque axe orienté pour que sa plus grande coordonnée (en valeur
# absolue) soit positive. L'inertie totale, la trace de G, est la somme de toutes les valeurs propres.
pcoa_noyau <- function(D) {
  G <- -0.5 * centrage_double(D^2)
  e <- eigen(G, symmetric = TRUE)
  ZERO <- sqrt(.Machine$double.eps)
  garde <- abs(e$values) > max(ZERO, ZERO * e$values[1])
  lambda <- e$values[garde]
  V <- e$vectors[, garde, drop = FALSE]
  signes <- apply(V, 2, function(v) if (v[which.max(abs(v))] < 0) -1 else 1)
  V <- sweep(V, 2, signes, "*")
  coord <- sweep(V, 2, sqrt(abs(lambda)), "*")
  noms <- sprintf("PCoA%d", seq_along(lambda))
  dimnames(coord) <- list(rownames(D), noms)
  names(lambda) <- noms
  list(n = nrow(D), totale = sum(diag(G)), valeurs = lambda, positifs = lambda > 0, coord = coord, points = coord[, lambda > 0, drop = FALSE], G = G)
}

# Les valeurs propres : axe, valeur, part de l'inertie totale (la somme de toutes : vegan,
# summary(eigenvals()) ; avec des valeurs propres négatives, les axes positifs en portent plus de
# 100 %), cumul, négative ou non.
table_valeurs_pcoa <- function(k) {
  data.frame(axe = names(k$valeurs), valeur_propre = unname(k$valeurs), part = unname(k$valeurs) / k$totale,
             cumul = cumsum(unname(k$valeurs)) / k$totale, negative = unname(k$valeurs) < 0, stringsAsFactors = FALSE)
}

# Les scores des espèces (des colonnes de Y) : la moyenne des scores des observations pondérée par
# les valeurs de chaque espèce, puis dilatée pour que leur variance pondérée soit celle des
# observations (vegan : wascores(expand = TRUE), comme metaMDS). Seulement pour un tableau positif
# ou nul ; une espèce sans aucune valeur n'a pas de score.
scores_especes <- function(points, W) {
  if (is.null(W) || !ncol(points) || any(W < 0) || sum(W) <= 0) return(NULL)
  total <- colSums(W)
  ok <- total > 0
  wa <- matrix(NA_real_, ncol(W), ncol(points), dimnames = list(colnames(W), colnames(points)))
  wa[ok, ] <- crossprod(W[, ok, drop = FALSE], points) / total[ok]
  if (sum(ok) > 1) {
    cx <- stats::cov.wt(points, rowSums(W), method = "ML")
    cw <- stats::cov.wt(wa[ok, , drop = FALSE], total[ok], method = "ML")
    v <- diag(cw$cov)
    mul <- ifelse(v > 0, sqrt(diag(cx$cov) / pmax(v, .Machine$double.xmin)), 1)
    centre <- cw$center
    wa[ok, ] <- sweep(sweep(sweep(wa[ok, , drop = FALSE], 2, centre), 2, mul, "*"), 2, centre, "+")
  }
  wa
}

# ---------------------------------------------------------------- les variables ajustées

# Les variables X d'une ordination (PCOA, NMDS) : leurs valeurs dans leurs unités (transformations de
# l'onglet Variables comprises), les quantitatives en colonnes, les qualitatives en facteurs de leurs
# catégories. Pas de terme croisé : on ajuste des variables.
variables_ajustees <- function(projet, d, obs) {
  if (any(vapply(d$x, function(t) length(t$variables) > 1, TRUE))) {
    stop(traduire("Les variables ajustées sur l'ordination sont des variables : retirez les termes croisés.",
                  "The variables fitted onto the ordination are variables: remove the cross terms."), call. = FALSE)
  }
  quantitatifs <- Filter(function(t) categorie(t) == 0, d$x)
  Xq <- if (length(quantitatifs)) valeurs_bloc(projet, quantitatifs, obs) else matrix(numeric(0), length(obs), 0)
  colnames(Xq) <- vapply(quantitatifs, function(t) as.character(t$name), "")
  facteurs <- list()
  for (t in d$x) {
    if (categorie(t) == 0) next
    v <- variable_du_projet(projet, t$variables[1])
    nom <- as.character(v$ids[1])
    if (is.null(facteurs[[nom]])) facteurs[[nom]] <- factor(as.character(v$categories)[colonne(projet, t$variables[1], obs)], levels = as.character(v$categories))
  }
  connues <- if (ncol(Xq)) stats::complete.cases(Xq) else rep(TRUE, length(obs))
  for (f in facteurs) connues <- connues & !is.na(f)
  list(Xq = Xq, facteurs = facteurs, connues = connues)
}

# Des corrélations colonne à colonne (A[, j] avec B[, j]).
correlations_colonnes <- function(A, B) {
  A <- scale(A, scale = FALSE)
  B <- scale(B, scale = FALSE)
  colSums(A * B) / sqrt(colSums(A^2) * colSums(B^2))
}

# L'ajustement des variables X sur les axes S d'une ordination (vegan : envfit, vectorfit,
# factorfit) : pour une variable quantitative, sa régression sur les axes centrés, sa direction (les
# coefficients ramenés à une longueur 1) et son R² (le carré de la corrélation de la variable et de
# ses valeurs ajustées) ; pour une qualitative, les centroïdes de ses catégories (la moyenne de leurs
# scores) et R² = 1 − somme des carrés dans les catégories / totale. Leur valeur p : la part des
# permutations P des valeurs de la variable qui font au moins aussi bien (le même R² compté).
ajustement_environnement <- function(S, Xq, facteurs, P) {
  Sc <- scale(S, scale = FALSE)
  B <- nrow(P)
  vecteurs <- NULL
  if (ncol(Xq)) {
    Q <- qr(Sc)
    Xc <- scale(Xq, scale = FALSE)
    r2 <- correlations_colonnes(qr.fitted(Q, Xc), Xc)^2
    r2[!is.finite(r2)] <- 0
    coef <- qr.coef(Q, Xc)
    coef <- sweep(matrix(coef, ncol(S)), 2, pmax(.Machine$double.eps, sqrt(colSums(matrix(coef, ncol(S))^2))), "/")
    direction <- t(coef)
    dimnames(direction) <- list(colnames(Xq), colnames(S))
    p <- rep(NA_real_, ncol(Xq))
    if (B) {
      perm <- vapply(seq_len(B), function(b) {
        Xp <- scale(Xq[P[b, ], , drop = FALSE], scale = FALSE)
        correlations_colonnes(qr.fitted(Q, Xp), Xp)^2
      }, numeric(ncol(Xq)))
      perm <- matrix(perm, ncol(Xq))
      depasse <- sweep(perm, 1, r2 - EPS_PERMUTATIONS, ">=")
      p <- (rowSums(depasse, na.rm = TRUE) + 1) / (rowSums(is.finite(perm)) + 1)
    }
    vecteurs <- list(direction = direction, r2 = stats::setNames(unname(r2), colnames(Xq)), p = stats::setNames(p, colnames(Xq)))
  }
  facteurs_fit <- NULL
  if (length(facteurs)) {
    totale <- sum(Sc^2)
    intra <- function(f) sum(vapply(split(seq_len(nrow(S)), f, drop = TRUE), function(i) sum(scale(S[i, , drop = FALSE], scale = FALSE)^2), 0))
    lignes <- lapply(names(facteurs), function(nom) {
      f <- droplevels(facteurs[[nom]])
      r2 <- 1 - intra(f) / totale
      p <- if (B) valeur_p_superieure(vapply(seq_len(B), function(b) 1 - intra(f[P[b, ]]) / totale, 0), r2) else NA_real_
      list(variable = nom, r2 = r2, p = p, niveaux = levels(f), n = as.vector(table(f)))
    })
    facteurs_fit <- lignes
  }
  list(vecteurs = vecteurs, facteurs = facteurs_fit)
}

# Les centroïdes des catégories des variables qualitatives sur tous les axes `coord` (la moyenne
# des scores de leurs observations : vegan, envfit, sur les axes ajustés) : une ligne par catégorie
# observée, « habitat(prairie) ».
centroides_facteurs <- function(coord, facteurs) {
  if (!length(facteurs)) return(NULL)
  do.call(rbind, lapply(names(facteurs), function(nom) {
    f <- droplevels(facteurs[[nom]])
    M <- do.call(rbind, lapply(levels(f), function(niveau) colMeans(coord[f == niveau, , drop = FALSE])))
    rownames(M) <- paste0(nom, "(", levels(f), ")")
    M
  }))
}

# L'ajustement des variables X d'une ordination (PCOA, NMDS) sur ses axes `axes` (des noms de
# colonnes de `coord`) : les observations où une valeur X (ou leur groupe de permutation) manque en
# sont laissées de côté, comptées ; les permutations de ces observations-là.
ajuster_variables <- function(projet, d, o, observations, coord, axes) {
  if (!length(d$x)) return(NULL)
  v <- variables_ajustees(projet, d, observations)
  strates <- strates_communaute(projet, o, observations)
  connues <- v$connues & (if (is.null(strates)) TRUE else !is.na(strates))
  if (sum(connues) < 3) stop(traduire("Trop peu d'observations ont toutes leurs valeurs X pour les ajuster sur l'ordination.", "Too few observations have all their X values to fit them onto the ordination."), call. = FALSE)
  strates <- if (!is.null(strates)) strates[connues]
  verifier_strates(strates)
  P <- permutations_lignes(sum(connues), o$permutations, strates, o$graine)
  facteurs <- lapply(v$facteurs, function(f) f[connues])
  a <- ajustement_environnement(coord[connues, axes, drop = FALSE], v$Xq[connues, , drop = FALSE], facteurs, P)
  a$centroides <- centroides_facteurs(coord[connues, , drop = FALSE], facteurs)
  a$axes <- axes
  a$exclues <- observations[!connues]
  a$n <- sum(connues)
  a
}

# ---------------------------------------------------------------- les deux ordinations

# Une PCoA (definir_modele, type "PCOA") : le tableau Y (rôle Y), et les variables X à ajuster sur
# ses deux premiers axes (comme envfit), s'il y en a. La correction (Lingoes, Cailliez) est
# appliquée à la distance avant la PCoA.
ajuster_pcoa <- function(projet, d) {
  o <- options_communaute(d)
  dc <- donnees_communaute(projet, d, o, traduire("une PCoA", "a PCoA"))
  corr <- corriger_distance(dc$D, o$correction)
  k <- pcoa_noyau(corr$D)
  if (!any(k$positifs)) stop(traduire("La PCoA n'a aucun axe de valeur propre positive.", "The PCoA has no axis of positive eigenvalue."), call. = FALSE)
  axes <- colnames(k$points)[seq_len(min(2, ncol(k$points)))]
  m <- list(type = "PCOA", observations = dc$observations, exclues = dc$exclues, N = length(dc$observations), K = ncol(dc$Y),
            Y = list(noms = colnames(dc$Y)), options = o, constante = corr$constante, noyau = k[c("n", "totale", "valeurs", "positifs", "coord", "points")],
            especes = scores_especes(k$points, dc$W), noms = dc$noms)
  m$ajustement <- ajuster_variables(projet, d, o, dc$observations, k$points, axes)
  class(m) <- "modele_communaute"
  m
}

# La rotation symétrique de Procrustes de Y sur X (vegan : procrustes(symmetric = TRUE)) : les deux
# centrés, ramenés à une somme des carrés de 1 ; l'erreur quadratique moyenne et le résidu de chaque
# observation. (L'erreur vient des résidus eux-mêmes : vegan la tire de 1 + c² - 2c Σd, qui vaut
# 1 - c² et ne garde, pour deux configurations presque identiques, que la racine de la précision des
# nombres, 1e-8 environ.)
procrustes_symetrique <- function(X, Y) {
  X <- scale(X, scale = FALSE)
  Y <- scale(Y, scale = FALSE)
  X <- X / sqrt(sum(X^2))
  Y <- Y / sqrt(sum(Y^2))
  s <- svd(crossprod(X, Y))
  A <- s$v %*% t(s$u)
  c <- sum(s$d) / sum(Y^2)
  carres <- rowSums((X - c * Y %*% A)^2)
  list(rmse = sqrt(sum(carres) / nrow(X)), residus = sqrt(carres))
}

# Un NMDS en k dimensions (MASS::isoMDS : la descente du stress de Kruskal 1964), comme vegan::metaMDS
# avec isoMDS (sans transformation automatique ni distances par étapes) : un premier ajustement
# depuis la configuration de la PCoA (cmdscale), puis `departs` configurations au hasard (uniformes
# sur [0, 1], graine fixe) ; un départ dont le stress ne dépasse pas celui du meilleur de 0,0005 en
# est comparé par Procrustes : plus bas, il devient le meilleur ; semblable (erreur quadratique
# moyenne < 0,005 et plus grand résidu < 0,01), la meilleure solution est « retrouvée ». Une
# dissimilarité nulle entre deux observations (isoMDS les refuse) devient la moitié de la plus
# petite non nulle (metaMDS : zerodist = "add"). Puis la configuration est centrée, tournée sur ses
# axes principaux et mise à l'échelle de la plus grande dissimilarité (vegan : postMDS sans demi-
# changement), chaque axe orienté comme ceux de la PCoA. Rend le stress (0 à 1) de chaque départ.
nmds_noyau <- function(D, k, departs, graine) {
  dis <- stats::as.dist(D)
  nuls <- sum(dis <= 0)
  if (nuls == length(dis)) stop(traduire("Toutes les dissimilarités sont nulles.", "All the dissimilarities are zero."), call. = FALSE)
  if (nuls) dis[dis <= 0] <- min(dis[dis > 0]) / 2
  n <- attr(dis, "Size")
  if (n <= k + 1) stop(sprintf(traduire("Un NMDS en %d dimensions demande au moins %d observations.", "An NMDS in %d dimensions needs at least %d observations."), k, k + 2), call. = FALSE)
  depart <- suppressWarnings(stats::cmdscale(dis, k))
  if (NCOL(depart) < k) {
    stop(sprintf(traduire("La PCoA de ces distances n'a que %d axe(s) positif(s) : demandez moins de dimensions.", "The PCoA of these distances has only %d positive axis(es): ask for fewer dimensions."), NCOL(depart)), call. = FALSE)
  }
  EPS <- 0.05  # (en pour cent, l'unité du stress d'isoMDS)
  r <- avec_graine(graine, {
    s0 <- MASS::isoMDS(dis, y = depart, k = k, maxit = 200, trace = FALSE)
    stress <- s0$stress
    meilleur <- 0
    retrouvee <- 0
    for (i in seq_len(departs)) {
      s <- MASS::isoMDS(dis, matrix(stats::runif(n * k), n, k), k = k, maxit = 200, tol = 1e-7, trace = FALSE)
      stress <- c(stress, s$stress)
      if (s0$stress - s$stress > -EPS) {
        pr <- procrustes_symetrique(s0$points, s$points)
        if (s$stress < s0$stress) {
          s0 <- s
          meilleur <- i
          retrouvee <- 0
        }
        if (pr$rmse < 0.005 && max(pr$residus) < 0.01) retrouvee <- retrouvee + 1
      }
    }
    # (les autres départs qui atteignent le stress de la meilleure solution à 0,0001 près, quelle que
    # soit leur configuration : des minimums presque égaux, où quelques observations se placent un peu
    # autrement, ne sont pas « retrouvés » par Procrustes, et le stress est pourtant atteint)
    meme_stress <- sum(abs(stress - s0$stress) < 0.01) - 1
    list(s0 = s0, stress = stress, meilleur = meilleur, retrouvee = retrouvee, meme_stress = meme_stress)
  })
  X <- scale(r$s0$points, scale = FALSE)
  X <- stats::prcomp(X, center = TRUE)$x
  X <- X / (max(stats::dist(X)) / max(dis))
  X <- sweep(X, 2, apply(X, 2, function(v) if (v[which.max(abs(v))] < 0) -1 else 1), "*")
  X <- matrix(X, n, k, dimnames = list(rownames(D), sprintf("NMDS%d", seq_len(k))))
  sh <- MASS::Shepard(dis, X)
  ecarts <- (sh$y - sh$yf)^2 / sum(sh$y^2)
  # (le stress de chaque observation : la racine de la moitié de la part du stress² de ses paires,
  # vegan : goodness ; la somme de leurs carrés est le stress²)
  tri <- order(dis)  # (les paires dans l'ordre des dissimilarités, celui de Shepard)
  part <- numeric(length(dis))
  part[tri] <- ecarts
  M <- matrix(0, n, n)
  M[lower.tri(M)] <- part
  M <- M + t(M)
  list(points = X, stress = sqrt(sum(ecarts)), stress_departs = r$stress / 100, meilleur = r$meilleur, retrouvee = r$retrouvee,
       meme_stress = r$meme_stress, nuls = nuls, remplacement = if (nuls) min(dis[dis > 0]) else NA_real_, shepard = list(dissimilarite = sh$x, distance = sh$y, ajustee = sh$yf),
       stress_observations = sqrt(colSums(M) / 2))
}

# Un NMDS (definir_modele, type "NMDS") : le tableau Y (rôle Y), et les variables X à ajuster sur ses
# k dimensions, s'il y en a.
ajuster_nmds <- function(projet, d) {
  o <- options_communaute(d)
  dc <- donnees_communaute(projet, d, o, traduire("un NMDS", "an NMDS"))
  k <- nmds_noyau(dc$D, o$k, o$departs, o$graine)
  m <- list(type = "NMDS", observations = dc$observations, exclues = dc$exclues, N = length(dc$observations), K = ncol(dc$Y),
            Y = list(noms = colnames(dc$Y)), options = o, noyau = k, especes = scores_especes(k$points, dc$W), noms = dc$noms)
  m$ajustement <- ajuster_variables(projet, d, o, dc$observations, k$points, colnames(k$points))
  class(m) <- "modele_communaute"
  m
}

# ---------------------------------------------------------------- PERMANOVA

# La PERMANOVA de G = −½ D² doublement centrée (vegan : adonis2 ; Anderson 2001, McArdle et Anderson
# 2001) sur les colonnes X (centrées ; `groupes` : le terme de chaque colonne, dans l'ordre des
# termes). La somme des carrés expliquée par des colonnes est tr(H G), H la projection sur elles ;
# celle des résidus, tr(G) − tr(H G) pour tous les termes. Les termes séquentiels ("termes",
# by = "terms") : chacun après ceux qui le précèdent ; marginaux ("marges", by = "margin") : chacun
# après tous les autres, sauf ceux qu'un terme croisé contient ; le modèle entier ("modele"). Le
# pseudo-F de chacun, (SC / ddl) / (SC résiduelle / ddl résiduels), est comparé à celui des
# permutations P des observations (les lignes et les colonnes de G permutées ensemble).
permanova_noyau <- function(G, X, groupes, par, P) {
  n <- nrow(G)
  totale <- sum(diag(G))
  Xc <- scale(X, center = TRUE, scale = FALSE)
  Q <- qr(Xc)
  r <- Q$rank
  if (r == 0) stop(traduire("Les variables X n'expliquent rien (constantes).", "The X variables explain nothing (constant)."), call. = FALSE)
  ddl_res <- n - r - 1
  if (ddl_res < 1) stop(traduire("Trop de termes X pour le nombre d'observations : il ne reste aucun degré de liberté.", "Too many X terms for the number of observations: no degree of freedom is left."), call. = FALSE)
  base <- function(q) qr.Q(q)[, seq_len(q$rank), drop = FALSE]
  B0 <- base(Q)
  # (la contribution de chaque colonne de la base, pour G permutée par `p` : tr(B' G[p, p] B) =
  # tr(C' G C), C = B[order(p), ])
  contributions <- function(Bq, p = NULL) {
    if (!ncol(Bq)) return(numeric(0))
    C <- if (is.null(p)) Bq else Bq[order(p), , drop = FALSE]
    colSums((G %*% C) * C)
  }
  # les termes de la base, dans l'ordre (une colonne redondante passe à la fin du QR : son terme
  # garde les autres ; un terme sans colonne gardée disparaît, nommé dans `alias`)
  termes <- groupes[Q$pivot[seq_len(r)]]
  noms <- unique(groupes)
  gardes <- unique(termes)
  alias <- setdiff(noms, gardes)
  modele <- function(f) {
    F0 <- (f / r) / ((totale - f) / ddl_res)
    list(ss = f, ddl = r, F = F0)
  }
  c0 <- contributions(B0)
  ajuste <- sum(c0)
  B <- nrow(P)
  test <- modele(ajuste)
  lignes <- NULL
  Fp_modele <- numeric(B)
  if (par == "termes") {
    ss <- vapply(gardes, function(g) sum(c0[termes == g]), 0)
    ddl <- vapply(gardes, function(g) sum(termes == g), 0)
    F0 <- (ss / ddl) / ((totale - ajuste) / ddl_res)
    Fp <- matrix(NA_real_, B, length(gardes))
    for (b in seq_len(B)) {
      cb <- contributions(B0, P[b, ])
      fb <- sum(cb)
      Fp[b, ] <- (vapply(gardes, function(g) sum(cb[termes == g]), 0) / ddl) / ((totale - fb) / ddl_res)
      Fp_modele[b] <- (fb / r) / ((totale - fb) / ddl_res)
    }
    lignes <- data.frame(terme = gardes, ddl = ddl, ss = ss, F = F0, p = if (B) vapply(seq_along(gardes), function(j) valeur_p_superieure(Fp[, j], F0[j]), 0) else NA_real_,
                         stringsAsFactors = FALSE)
  } else if (par == "marges") {
    testables <- Filter(function(g) !any(vapply(gardes, function(h) contenu_dans(g, h), TRUE)), gardes)
    bases <- lapply(testables, function(g) base(qr(Xc[, groupes != g, drop = FALSE])))
    ddl <- vapply(bases, function(b) r - ncol(b), 0)
    ss <- vapply(bases, function(b) ajuste - sum(contributions(b)), 0)
    F0 <- (ss / ddl) / ((totale - ajuste) / ddl_res)
    Fp <- matrix(NA_real_, B, length(testables))
    for (b in seq_len(B)) {
      fb <- sum(contributions(B0, P[b, ]))
      Fp[b, ] <- vapply(seq_along(bases), function(j) ((fb - sum(contributions(bases[[j]], P[b, ]))) / ddl[j]) / ((totale - fb) / ddl_res), 0)
      Fp_modele[b] <- (fb / r) / ((totale - fb) / ddl_res)
    }
    lignes <- data.frame(terme = unlist(testables), ddl = ddl, ss = ss, F = F0,
                         p = if (B) vapply(seq_along(testables), function(j) valeur_p_superieure(Fp[, j], F0[j]), 0) else NA_real_, stringsAsFactors = FALSE)
  } else {
    for (b in seq_len(B)) {
      fb <- sum(contributions(B0, P[b, ]))
      Fp_modele[b] <- (fb / r) / ((totale - fb) / ddl_res)
    }
  }
  test$p <- valeur_p_superieure(Fp_modele, test$F)
  residus <- totale - ajuste
  tete <- if (!is.null(lignes)) lignes else data.frame(terme = traduire("Modèle", "Model"), ddl = r, ss = ajuste, F = test$F, p = test$p, stringsAsFactors = FALSE)
  table <- rbind(tete, data.frame(terme = c(traduire("Résidus", "Residual"), "Total"), ddl = c(ddl_res, n - 1), ss = c(residus, totale), F = NA_real_, p = NA_real_,
                                  stringsAsFactors = FALSE))
  # (R² après la somme des carrés ; les colonnes par leur place : le script en anglais traduit leurs noms)
  table <- data.frame(table[1:3], R2 = table$ss / totale, table[4:5], stringsAsFactors = FALSE)
  list(table = table, modele = list(ddl = r, ss = ajuste, R2 = ajuste / totale, F = test$F, p = test$p),
       ddl_residus = ddl_res, totale = totale, alias = alias)
}

# La médiane spatiale des lignes de X (le point qui rend minimale la somme des distances
# euclidiennes), par BFGS depuis la médiane de chaque colonne (vegan : ordimedian ; un point
# confondu avec le départ ne compte pas dans le gradient).
mediane_spatiale <- function(X) {
  if (!ncol(X)) return(numeric(0))
  f <- function(x) sum(sqrt(rowSums(sweep(X, 2, x)^2)))
  g <- function(x) {
    u <- -sweep(X, 2, x)
    dn <- sqrt(rowSums(u^2))
    dn[dn == 0] <- Inf
    colSums(u / dn)
  }
  stats::optim(apply(X, 2, stats::median), f, g, method = "BFGS")$par
}

# La dispersion multivariée des groupes `groupe` (un facteur) d'une distance (vegan : betadisper ;
# Anderson 2006) : la PCoA de D (les axes de valeur propre négative compris), le centre de chaque
# groupe (son centroïde, ou sa médiane spatiale, calculée à part sur les axes positifs et sur les
# négatifs), la distance de chaque observation au centre de son groupe, √(d²+ − d²−) (d²− : sur les
# axes négatifs ; une différence négative devient 0, comptée). Puis l'ANOVA de ces distances entre
# les groupes, son pseudo-F comparé à celui des permutations P de ses résidus (vegan :
# permutest.betadisper), et les paires de groupes de Tukey (vegan : TukeyHSD.betadisper).
dispersion_noyau <- function(D, groupe, type = "centroide", P = NULL) {
  groupe <- droplevels(groupe)
  if (nlevels(groupe) < 2) stop(traduire("La dispersion se compare entre deux groupes au moins.", "Dispersion is compared between two groups or more."), call. = FALSE)
  G <- -0.5 * centrage_double(D^2)
  e <- eigen(G, symmetric = TRUE)
  TOL <- sqrt(.Machine$double.eps)
  garde <- abs(e$values) > max(TOL, TOL * e$values[1])
  lambda <- e$values[garde]
  V <- e$vectors[, garde, drop = FALSE] %*% diag(sqrt(abs(lambda)), length(lambda))
  colnames(V) <- sprintf("PCoA%d", seq_along(lambda))
  pos <- lambda > 0
  centres <- if (type == "centroide") {
    do.call(rbind, lapply(levels(groupe), function(g) colMeans(V[groupe == g, , drop = FALSE])))
  } else {
    do.call(rbind, lapply(levels(groupe), function(g) {
      x <- numeric(ncol(V))
      x[pos] <- mediane_spatiale(V[groupe == g, pos, drop = FALSE])
      x[!pos] <- mediane_spatiale(V[groupe == g, !pos, drop = FALSE])
      x
    }))
  }
  dimnames(centres) <- list(levels(groupe), colnames(V))
  ecart <- V - centres[as.integer(groupe), , drop = FALSE]
  d2p <- rowSums(ecart[, pos, drop = FALSE]^2)
  d2n <- if (any(!pos)) rowSums(ecart[, !pos, drop = FALSE]^2) else 0
  negatives <- sum(d2n > d2p)
  z <- sqrt(pmax(d2p - d2n, 0))
  df <- data.frame(z = z, groupe = groupe)
  ajust <- stats::lm(z ~ groupe, data = df)
  a <- stats::anova(ajust)
  F0 <- a[["F value"]][1]
  B <- if (is.null(P)) 0 else nrow(P)
  p_perm <- NA_real_
  if (B) {
    Qm <- ajust$qr
    k <- Qm$rank
    res <- qr.resid(Qm, z)
    n <- length(z)
    Fp <- vapply(seq_len(B), function(b) {
      rb <- res[P[b, ]]
      f <- qr.fitted(Qm, rb)
      ((sum((f - mean(f))^2)) / (k - 1)) / (sum(qr.resid(Qm, rb)^2) / (n - k))
    }, 0)
    p_perm <- valeur_p_superieure(Fp, F0)
  }
  tk <- stats::TukeyHSD(stats::aov(z ~ groupe, data = df))$groupe
  list(type = type, distances = z, groupe = groupe, negatives = negatives,
       groupes = data.frame(groupe = levels(groupe), n = as.vector(table(groupe)), moyenne = as.vector(tapply(z, groupe, mean)),
                            mediane = as.vector(tapply(z, groupe, stats::median)), stringsAsFactors = FALSE),
       anova = data.frame(source = c(traduire("Groupes", "Groups"), traduire("Résidus", "Residuals")), ddl = a$Df, ss = a[["Sum Sq"]], ms = a[["Mean Sq"]],
                          F = c(F0, NA), p = c(a[["Pr(>F)"]][1], NA), p_permutation = c(p_perm, NA), stringsAsFactors = FALSE),
       F = F0, p = a[["Pr(>F)"]][1], p_permutation = p_perm, permutations = B,
       tukey = data.frame(paire = rownames(tk), difference = tk[, "diff"], bas = tk[, "lwr"], haut = tk[, "upr"], p = tk[, "p adj"], stringsAsFactors = FALSE, row.names = NULL),
       coord = V[, seq_len(min(2, ncol(V))), drop = FALSE], centres = centres[, seq_len(min(2, ncol(V))), drop = FALSE])
}

# Une PERMANOVA (definir_modele, type "PERMANOVA") : le tableau Y (rôle Y), les termes X (des
# variables quantitatives ou qualitatives, des termes croisés, dans leur ordre), et la dispersion
# de chaque variable X qualitative et de la variable de classe (rôle Classe), s'il y en a, sur les
# mêmes observations (sans celles où le groupe manque) et les mêmes permutations.
ajuster_permanova <- function(projet, d) {
  o <- options_communaute(d)
  quoi <- traduire("une PERMANOVA", "a PERMANOVA")
  if (!length(d$x)) stop(traduire("Une PERMANOVA demande des variables X (les facteurs ou gradients testés).", "A PERMANOVA needs X variables (the factors or gradients tested)."), call. = FALSE)
  obs <- as.numeric(d$observations)
  xs <- colonnes_ordination(projet, d$x, obs)
  strates <- strates_communaute(projet, o, obs)
  aussi <- stats::complete.cases(xs$X) & (if (is.null(strates)) TRUE else !is.na(strates))
  dc <- donnees_communaute(projet, d, o, quoi, aussi)
  corr <- corriger_distance(dc$D, o$correction)
  X <- xs$X[dc$connu, , drop = FALSE]
  strates <- if (!is.null(strates)) strates[dc$connu]
  verifier_strates(strates)
  n <- length(dc$observations)
  P <- permutations_lignes(n, o$permutations, strates, o$graine)
  G <- -0.5 * centrage_double(corr$D^2)
  k <- permanova_noyau(G, X, xs$groupes, o$par, P)
  # les groupements de la dispersion : les variables X qualitatives, puis la variable de classe
  groupements <- list()
  for (t in d$x) {
    if (length(t$variables) != 1 || categorie(t) == 0) next
    v <- variable_du_projet(projet, t$variables[1])
    nom <- as.character(v$ids[1])
    if (is.null(groupements[[nom]])) groupements[[nom]] <- factor(as.character(v$categories)[colonne(projet, t$variables[1], dc$observations)], levels = as.character(v$categories))
  }
  if (length(d$classe) && !is.na(d$classe[1])) {
    v <- variable_du_projet(projet, d$classe)
    nom <- as.character(v$ids[1])
    if (!length(v$categories)) stop(sprintf(traduire("La variable de classe « %s » doit être qualitative.", "The class variable “%s” must be qualitative."), nom), call. = FALSE)
    if (is.null(groupements[[nom]])) groupements[[nom]] <- factor(as.character(v$categories)[colonne(projet, d$classe, dc$observations)], levels = as.character(v$categories))
  }
  dispersions <- list()
  for (nom in names(groupements)) {
    f <- groupements[[nom]]
    connus <- !is.na(f)
    if (length(unique(f[connus])) < 2) next
    Pd <- if (all(connus)) P else permutations_lignes(sum(connus), o$permutations, if (!is.null(strates)) strates[connus], o$graine)
    r <- dispersion_noyau(corr$D[connus, connus, drop = FALSE], f[connus], o$dispersion, Pd)
    r$variable <- nom
    r$observations <- dc$observations[connus]
    dispersions[[nom]] <- r
  }
  m <- list(type = "PERMANOVA", observations = dc$observations, exclues = dc$exclues, N = n, K = ncol(dc$Y), Y = list(noms = colnames(dc$Y)),
            X = list(noms = colnames(X), groupes = xs$groupes, indicatrices = xs$indicatrices), options = o, constante = corr$constante,
            noyau = k, dispersions = dispersions, noms = dc$noms)
  class(m) <- "modele_communaute"
  m
}

# ---------------------------------------------------------------- Mantel

# La distance euclidienne entre observations sur des termes mis à l'échelle comme le dit l'onglet
# Variables (construire_bloc : une variable qualitative par ses indicatrices 0/1) : `connues`, les
# observations dont tous les termes sont connus ; `D`, leur distance (les échelles calculées sur
# elles seules).
distance_euclidienne_termes <- function(projet, termes, observations) {
  b <- construire_bloc(projet, termes, observations)
  connues <- stats::complete.cases(b$X)
  if (!all(connues)) b <- construire_bloc(projet, termes, observations[connues])
  D <- as.matrix(stats::dist(b$X))
  dimnames(D) <- list(nom_numero(observations[connues]), nom_numero(observations[connues]))
  list(D = D, connues = connues, termes = b$noms, echelles = unique(b$type))
}

# La distance des variables X d'un test de Mantel, pour les observations `observations` : la
# distance euclidienne sur les termes X mis à l'échelle comme le dit l'onglet Variables, ou
# (options$distance_x = "geographique") la distance géographique entre les coordonnées du projet,
# en kilomètres (spatial.R : le long des grands cercles pour des longitudes et latitudes, en ligne
# droite pour des coordonnées planes), pour la décroissance de la ressemblance avec la distance.
# Rend list(D, connues, termes, echelles, texte) : `connues`, les observations dont les valeurs
# sont connues ; `D`, la matrice de leurs distances (nommée par leurs numéros) ; `texte`, ce qu'en
# dit le texte des méthodes, en français et en anglais.
distances_mantel_x <- function(projet, d, observations) {
  if (identical(options_communaute(d)$distance_x, "geographique")) {
    pts <- points_projet(projet, observations)
    connues <- is.finite(pts$x) & is.finite(pts$y)
    if (sum(connues) < 3) stop(traduire("Moins de 3 observations ont des coordonnées : pas de distance géographique.", "Fewer than 3 observations have coordinates: no geographic distance."), call. = FALSE)
    D <- distances_points(pts$x[connues], pts$y[connues], pts$systeme) / 1000
    dimnames(D) <- list(nom_numero(observations[connues]), nom_numero(observations[connues]))
    s <- systeme_coordonnees(pts$systeme)
    co <- pts$coordonnees
    lonlat <- s$type == "lonlat"
    return(list(D = D, connues = connues, termes = c(co$nom_x, co$nom_y), echelles = character(0),
                texte = list(fr = sprintf("la distance géographique en kilomètres (%s) entre les coordonnées « %s » et « %s » (%s)",
                                          if (lonlat) "le long des grands cercles" else "en ligne droite", co$nom_x, co$nom_y, s$nom),
                             en = sprintf("the geographic distance in kilometres (%s) between the coordinates “%s” and “%s” (%s)",
                                          if (lonlat) "along great circles" else "in straight lines", co$nom_x, co$nom_y, s$nom))))
  }
  r <- distance_euclidienne_termes(projet, d$x, observations)
  termes <- function(fr) paste0(liste_mots(utils::head(r$termes, 12), fr), if (length(r$termes) > 12) "…" else "")
  r$texte <- list(fr = sprintf("la distance euclidienne sur %s (%s), %s", if (length(r$termes) == 1) "un terme" else sprintf("%d termes", length(r$termes)), termes(TRUE), echelles_texte(r$echelles, TRUE)),
                  en = sprintf("the Euclidean distance on %s (%s), %s", if (length(r$termes) == 1) "one term" else sprintf("%d terms", length(r$termes)), termes(FALSE), echelles_texte(r$echelles, FALSE)))
  r
}

# Les termes des covariables Z d'un test de Mantel partiel : chacune avec sa transformation et son
# échelle (UV par défaut).
termes_z_mantel <- function(projet, o) {
  unlist(lapply(o$z, function(u) {
    tr <- o$z_transformations[[nom_numero(u)]]
    tr <- if (length(tr) && nzchar(as.character(tr[1]))) lire_transformation(as.character(tr[1]))
    ech <- as.character(unlist(o$z_echelles[[nom_numero(u)]] %||% "UV"))[1]
    termes_variable(projet, u, if (ech %in% c("UV", "Par", "Ctr", "none")) ech else "UV", tr)
  }), recursive = FALSE)
}

# Les paires d'observations d'une matrice n × n (le triangle inférieur, colonne par colonne).
paires_triangle <- function(n) {
  ij <- which(lower.tri(matrix(0, n, n)), arr.ind = TRUE)
  list(i = ij[, 1], j = ij[, 2])
}

# Le test de Mantel (vegan : mantel ; Mantel 1967) : la corrélation (de Pearson, ou de Spearman sur
# les rangs) entre les distances Dy et Dx des mêmes paires d'observations ; sa valeur p unilatérale,
# la part des permutations P des observations de Dy qui donnent une corrélation au moins aussi
# grande (vegan permute la première distance donnée : mantel(Dy, Dx)). Avec Dz, la corrélation
# partielle (vegan : mantel.partial ; Smouse et al. 1986), Dy permutée de même.
mantel_noyau <- function(Dy, Dx, methode, P, Dz = NULL) {
  n <- nrow(Dy)
  pr <- paires_triangle(n)
  y <- Dy[cbind(pr$i, pr$j)]
  x <- Dx[cbind(pr$i, pr$j)]
  z <- if (!is.null(Dz)) Dz[cbind(pr$i, pr$j)]
  if (methode == "spearman") {  # (les rangs d'une distance permutée sont les rangs permutés)
    y <- rank(y)
    x <- rank(x)
    if (!is.null(z)) z <- rank(z)
  }
  Ry <- matrix(0, n, n)
  Ry[cbind(pr$i, pr$j)] <- y
  Ry <- Ry + t(Ry)
  r <- stats::cor(y, x)
  partiel <- function(rxy, rxz, ryz) (rxy - rxz * ryz) / sqrt(1 - rxz^2) / sqrt(1 - ryz^2)
  ryz <- if (!is.null(z)) stats::cor(x, z)
  B <- nrow(P)
  perm <- perm_p <- numeric(B)
  for (b in seq_len(B)) {
    p <- P[b, ]
    yb <- Ry[cbind(p[pr$i], p[pr$j])]
    perm[b] <- stats::cor(yb, x)
    if (!is.null(z)) perm_p[b] <- partiel(perm[b], stats::cor(yb, z), ryz)
  }
  out <- list(r = r, p = if (B) valeur_p_superieure(perm, r) else NA_real_, permutations = B)
  if (!is.null(z)) {
    out$r_partiel <- partiel(r, stats::cor(y, z), ryz)
    out$p_partiel <- if (B) valeur_p_superieure(perm_p, out$r_partiel) else NA_real_
  }
  out
}

# Le corrélogramme de Mantel (vegan : mantel.correlog ; Legendre et Legendre 2012, § 13.1.6) : les
# paires d'observations en `classes` classes d'égale largeur de la distance Dx (0 : la règle de
# Sturges, 1 + log2 du nombre de paires, arrondi au-dessus) ; dans chaque classe, la corrélation de
# Mantel entre l'appartenance des paires à la classe (1 ou 0) et Dy, de signe changé (positive :
# des observations proches en X plus semblables que la moyenne), sa valeur p unilatérale du côté de
# son signe par les permutations P (vegan y permute la matrice d'appartenance) ; au-delà de la
# moitié des classes, une classe n'est testée que si chaque observation y a une paire. Les valeurs
# p corrigées progressivement par la méthode de Holm (la classe k avec les k premières).
correlogramme_noyau <- function(Dy, Dx, methode, P, classes = 0) {
  n <- nrow(Dx)
  pr <- paires_triangle(n)
  dx <- Dx[cbind(pr$i, pr$j)]
  dy <- Dy[cbind(pr$i, pr$j)]
  debut <- min(dx)
  fin <- max(dx)
  if (!(fin > debut)) return(NULL)
  nc <- if (classes > 0) as.integer(classes) else as.integer(ceiling(1 + log(length(dx), base = 2)))
  largeur <- (fin - debut) / nc
  bornes <- c(debut + largeur * (seq_len(nc) - 1), fin)
  bornes[1] <- bornes[1] - .Machine$double.eps
  centres <- bornes[seq_len(nc)] + 0.5 * (bornes[seq_len(nc) + 1] - bornes[seq_len(nc)])
  classe <- findInterval(dx, bornes, left.open = TRUE)  # (la plus petite borne qui n'est pas sous d, moins 1 : comme vegan)
  moitie <- nc %/% 2
  paires <- vapply(seq_len(nc), function(k) sum(classe == k), 0)
  complete <- vapply(seq_len(nc), function(k) length(unique(c(pr$i[classe == k], pr$j[classe == k]))) == n, TRUE)
  teste <- paires > 0 & !(seq_len(nc) > moitie & !complete)
  r <- p <- rep(NA_real_, nc)
  if (any(teste)) {
    M <- vapply(which(teste), function(k) as.numeric(classe == k), numeric(length(dx)))
    M <- matrix(M, length(dx))
    rangs <- methode == "spearman"
    if (rangs) {
      M <- apply(M, 2, rank)
      dy <- rank(dy)
    }
    Ry <- matrix(0, n, n)
    Ry[cbind(pr$i, pr$j)] <- dy
    Ry <- Ry + t(Ry)
    stat <- as.vector(stats::cor(M, dy))
    B <- nrow(P)
    perm <- matrix(NA_real_, B, ncol(M))
    # (permuter les observations de la matrice d'appartenance par p, c'est permuter celles de Dy par
    # order(p) : une permutation pour toutes les classes à la fois)
    for (b in seq_len(B)) {
      q <- order(P[b, ])
      perm[b, ] <- stats::cor(M, Ry[cbind(q[pr$i], q[pr$j])])
    }
    r[teste] <- -stat
    if (B) {
      p[teste] <- vapply(seq_along(stat), function(j) if (stat[j] < 0) (sum(perm[, j] <= stat[j]) + 1) / (B + 1) else valeur_p_superieure(perm[, j], stat[j]), 0)
    }
  }
  # (vegan : la correction progressive porte sur les n.tests premières classes, n.tests le nombre
  # de classes testées)
  n_tests <- sum(!is.na(p))
  corrige <- rep(NA_real_, nc)
  if (n_tests >= 1) {
    corrige[1] <- p[1]
    for (j in seq_len(n_tests)[-1]) corrige[j] <- stats::p.adjust(p[seq_len(j)], method = "holm")[j]
  }
  list(table = data.frame(classe = seq_len(nc), centre = centres, paires = paires, r = r, p = p, p_corrige = corrige, teste = teste),
       bornes = bornes, classes = nc, sturges = classes == 0)
}

# Un test de Mantel (definir_modele, type "MANTEL") : le tableau Y (rôle Y), les variables X dont la
# distance est comparée à celle de Y (distances_mantel_x), les covariables Z d'un test partiel
# (d$options$z) ; les observations où une de ces valeurs manque sont laissées de côté.
ajuster_mantel <- function(projet, d) {
  o <- options_communaute(d)
  quoi <- traduire("un test de Mantel", "a Mantel test")
  if (!length(d$x) && !identical(o$distance_x, "geographique")) {
    stop(traduire("Un test de Mantel demande des variables X (leur distance est comparée à celle du tableau Y), ou la distance géographique des coordonnées du projet.",
                  "A Mantel test needs X variables (their distance is compared with that of the table Y), or the geographic distance of the project's coordinates."), call. = FALSE)
  }
  obs <- as.numeric(d$observations)
  dx <- distances_mantel_x(projet, d, obs)
  tz <- termes_z_mantel(projet, o)
  dz <- if (length(tz)) distance_euclidienne_termes(projet, tz, obs)
  strates <- strates_communaute(projet, o, obs)
  aussi <- dx$connues & (if (is.null(dz)) TRUE else dz$connues) & (if (is.null(strates)) TRUE else !is.na(strates))
  dc <- donnees_communaute(projet, d, o, quoi, aussi)
  garde <- dc$observations
  if (length(garde) < length(obs)) {  # (les échelles des X et des Z recalculées sur ces observations)
    dx <- distances_mantel_x(projet, d, garde)
    if (!is.null(dz)) dz <- distance_euclidienne_termes(projet, tz, garde)
  }
  Dx <- dx$D[nom_numero(garde), nom_numero(garde), drop = FALSE]
  if (all(Dx == 0)) stop(if (identical(o$distance_x, "geographique")) traduire("Toutes les observations sont au même endroit : leurs distances sont nulles.", "All the observations are at the same place: their distances are zero.")
                         else traduire("Les variables X ne varient pas : toutes leurs distances sont nulles.", "The X variables don't vary: all their distances are zero."), call. = FALSE)
  Dz <- if (!is.null(dz)) dz$D[nom_numero(garde), nom_numero(garde), drop = FALSE]
  strates <- if (!is.null(strates)) strates[dc$connu]
  verifier_strates(strates)
  P <- permutations_lignes(length(garde), o$permutations, strates, o$graine)
  test <- mantel_noyau(dc$D, Dx, o$methode, P, Dz)
  correlogramme <- correlogramme_noyau(dc$D, Dx, o$methode, P, o$classes)
  m <- list(type = "MANTEL", observations = garde, exclues = dc$exclues, N = length(garde), K = ncol(dc$Y), Y = list(noms = colnames(dc$Y)),
            X = list(termes = dx$termes, echelles = dx$echelles, texte = dx$texte),
            Z = list(termes = if (!is.null(dz)) dz$termes else character(0), echelles = if (!is.null(dz)) dz$echelles,
                     variables = vapply(o$z, function(u) as.character(variable_du_projet(projet, u)$ids[1]), "")),
            options = o, test = test, correlogramme = correlogramme, noms = dc$noms,
            paires = list(y = dc$D[lower.tri(dc$D)], x = Dx[lower.tri(Dx)]))
  class(m) <- "modele_communaute"
  m
}

print.modele_communaute <- function(x, ...) {
  o <- x$options
  cat(sprintf("%s, %d observations, distance %s, transformation %s\n", x$type, x$N, nom_distance(o$distance, o$binaire), o$transformation))
  switch(x$type,
    PCOA = print(round(utils::head(x$noyau$valeurs / x$noyau$totale, 8), 4)),
    NMDS = cat(sprintf("stress %.4f, k = %d, meilleur départ %d, retrouvé %d fois\n", x$noyau$stress, o$k, x$noyau$meilleur, x$noyau$retrouvee)),
    PERMANOVA = print(x$noyau$table),
    MANTEL = cat(sprintf("r = %.4f, p = %s (%d permutations)\n", x$test$r, format(x$test$p, digits = 3), as.integer(x$test$permutations))))
  invisible(x)
}

# ---------------------------------------------------------------- l'interface

# Une table de scores pour l'interface (au plus AXES_INTERFACE axes), une ligne par ligne de M.
scores_table <- function(M, cle = "terme") {
  if (is.null(M) || !nrow(M)) return(NULL)
  cbind(stats::setNames(data.frame(rownames(M), stringsAsFactors = FALSE), cle), colonnes_axes(M))
}

# Ce que l'interface reçoit de l'ajustement des variables X : les flèches (direction × √R², vegan :
# scores.envfit) sur les axes ajustés, R² et p ; les centroïdes des catégories ; R² et p de chaque
# variable qualitative.
ajustement_interface <- function(a) {
  if (is.null(a)) return(NULL)
  v <- a$vecteurs
  f <- a$facteurs
  list(axes = I(a$axes), n = a$n, exclues = length(a$exclues),
       vecteurs = if (!is.null(v)) cbind(data.frame(terme = rownames(v$direction), r2 = unname(v$r2), p = unname(v$p), stringsAsFactors = FALSE),
                                         colonnes_axes(sweep(v$direction, 1, sqrt(v$r2), "*"))),
       directions = if (!is.null(v)) scores_table(v$direction),
       facteurs = if (!is.null(f)) data.frame(variable = vapply(f, `[[`, "", "variable"), r2 = vapply(f, `[[`, 0, "r2"), p = vapply(f, `[[`, 0, "p"), stringsAsFactors = FALSE),
       centroides = if (!is.null(a$centroides)) cbind(data.frame(terme = rownames(a$centroides), variable = rep(vapply(f, `[[`, "", "variable"), vapply(f, function(x) length(x$niveaux), 0)),
                                                                 n = unlist(lapply(f, `[[`, "n")), stringsAsFactors = FALSE), colonnes_axes(a$centroides)))
}

# Les réglages de la distance, pour l'interface et les exports.
reglages_distance <- function(m) {
  o <- m$options
  list(distance = o$distance, binaire = o$binaire, transformation = o$transformation, correction = o$correction, constante = m$constante %||% 0,
       libelle = nom_distance(o$distance, o$binaire), permutations = o$permutations, graine = o$graine)
}

# Ce qui est commun aux quatre analyses dans ce que l'interface reçoit.
resultat_communaute <- function(id, extra) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  c(list(id = id, projet = p$id, nom = d$name %||% "", titre = d$title %||% "", type = m$type, analyse = TRUE, prediction = FALSE, regression = FALSE,
         opls = FALSE, Ap = 0, A = 0, N = m$N, K = m$K, noms_y = I(m$Y$noms), distance = reglages_distance(m),
         strates = if (!is.null(m$options$strates)) nom_groupement(p, m$options$strates), exclues_manquantes = length(m$exclues),
         reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
         demande = d$demande %||% list(type = d$type, numero = d$number)), extra(p, m, d))
}

# (la ligne de l'explorateur de chacune dans la langue de l'interface : sa virgule décimale)
resultat_pcoa <- function(id) resultat_communaute(id, function(p, m, d) {
  k <- m$noyau
  vp <- table_valeurs_pcoa(k)
  fr <- !identical(getOption("nuee.langue"), "en")
  list(chiffres = sprintf("%s · PCoA1 %s", nom_court_distance(m$options$distance, m$options$binaire), pourcent_texte(vp$part[1], fr)),
       vues_defaut = I(c("ordination_dist", "valeurs_propres_pcoa")),
       observations = cbind(data.frame(uid = m$observations, nom = m$noms, stringsAsFactors = FALSE), colonnes_axes(k$points)),
       axes = I(utils::head(colnames(k$points), AXES_INTERFACE)), valeurs_propres = vp, inertie = list(totale = k$totale),
       negatives = list(n = sum(!k$positifs), somme = sum(k$valeurs[!k$positifs]), part = -sum(k$valeurs[!k$positifs]) / sum(k$valeurs[k$positifs])),
       especes = scores_table(m$especes), ajustement = ajustement_interface(m$ajustement),
       variables = if (!is.null(m$ajustement$vecteurs)) data.frame(terme = names(m$ajustement$vecteurs$r2), R2 = unname(m$ajustement$vecteurs$r2), p = unname(m$ajustement$vecteurs$p), stringsAsFactors = FALSE)
                   else data.frame(terme = character(0)),
       variables_y = scores_table(m$especes))
})

# (le diagramme de Shepard de l'interface : au plus SHEPARD_MAX paires, prises régulièrement dans
# l'ordre des dissimilarités)
SHEPARD_MAX <- 20000
resultat_nmds <- function(id) resultat_communaute(id, function(p, m, d) {
  k <- m$noyau
  sh <- k$shepard
  garde <- if (length(sh$dissimilarite) > SHEPARD_MAX) unique(round(seq(1, length(sh$dissimilarite), length.out = SHEPARD_MAX))) else seq_along(sh$dissimilarite)
  obs <- cbind(data.frame(uid = m$observations, nom = m$noms, stringsAsFactors = FALSE), colonnes_axes(k$points))
  obs$stress <- k$stress_observations
  list(chiffres = sprintf("stress %s · k = %d", nombre_texte(k$stress, 3, !identical(getOption("nuee.langue"), "en")), m$options$k),
       vues_defaut = I(c("ordination_dist", "shepard")), observations = obs, axes = I(colnames(k$points)),
       stress = k$stress, k = m$options$k, departs = m$options$departs, meilleur = k$meilleur, retrouvee = k$retrouvee, meme_stress = k$meme_stress %||% 0, nuls = k$nuls,
       stress_departs = data.frame(depart = seq_along(k$stress_departs) - 1, stress = k$stress_departs),
       shepard = list(dissimilarite = sh$dissimilarite[garde], distance = sh$distance[garde], ajustee = sh$ajustee[garde], total = length(sh$dissimilarite)),
       especes = scores_table(m$especes), ajustement = ajustement_interface(m$ajustement),
       variables = if (!is.null(m$ajustement$vecteurs)) data.frame(terme = names(m$ajustement$vecteurs$r2), R2 = unname(m$ajustement$vecteurs$r2), p = unname(m$ajustement$vecteurs$p), stringsAsFactors = FALSE)
                   else data.frame(terme = character(0)),
       variables_y = scores_table(m$especes))
})

# Ce que l'interface reçoit d'une dispersion : ses groupes, ses tests, ses paires, la distance de
# chaque observation à son centre, les deux premiers axes de sa PCoA et les centres.
dispersion_interface <- function(r) {
  list(variable = r$variable, type = r$type, groupes = r$groupes, anova = r$anova, tukey = r$tukey, F = r$F, p = r$p, p_permutation = r$p_permutation,
       negatives = r$negatives, uid = I(r$observations), groupe = I(as.character(r$groupe)), distance = I(unname(r$distances)),
       coord = colonnes_axes(r$coord), centres = cbind(data.frame(groupe = rownames(r$centres), stringsAsFactors = FALSE), colonnes_axes(r$centres)))
}

resultat_permanova <- function(id) resultat_communaute(id, function(p, m, d) {
  k <- m$noyau
  obs <- data.frame(uid = m$observations, nom = m$noms, stringsAsFactors = FALSE)
  if (length(m$dispersions)) {
    premiere <- m$dispersions[[1]]
    obs$classe <- as.character(premiere$groupe)[match(m$observations, premiere$observations)]
    for (r in m$dispersions) obs[[sprintf(traduire("distance au centre (%s)", "distance to centre (%s)"), r$variable)]] <- unname(r$distances)[match(m$observations, r$observations)]
  }
  fr <- !identical(getOption("nuee.langue"), "en")
  list(chiffres = sprintf("R2 %s · p %s", nombre_texte(k$modele$R2, 2, fr), p_explorateur(k$modele$p, fr)),
       vues_defaut = I(c("permanova", if (length(m$dispersions)) "dispersion" else "liste_obs")), observations = obs,
       table = k$table, par = m$options$par, modele = k$modele, alias = I(k$alias), termes = I(unique(m$X$groupes)),
       dispersions = unname(lapply(m$dispersions, dispersion_interface)), dispersion_type = m$options$dispersion,
       classe = if (length(d$classe) && !is.na(d$classe[1])) as.character(variable_du_projet(p, d$classe)$ids[1]))
})

# (le nuage des paires de Mantel de l'interface : au plus SHEPARD_MAX paires, au hasard d'une graine)
resultat_mantel <- function(id) resultat_communaute(id, function(p, m, d) {
  np <- length(m$paires$y)
  garde <- if (np > SHEPARD_MAX) sort(avec_graine(1, sample.int(np, SHEPARD_MAX))) else seq_len(np)
  t <- m$test
  fr <- !identical(getOption("nuee.langue"), "en")
  list(chiffres = sprintf("r %s · p %s", nombre_texte(t$r, 2, fr), p_explorateur(t$p, fr)),
       vues_defaut = I(c("mantel", if (!is.null(m$correlogramme)) "correlogramme" else "liste_obs")),
       observations = data.frame(uid = m$observations, nom = m$noms, stringsAsFactors = FALSE),
       test = t, methode = m$options$methode, termes_x = I(m$X$termes), echelles_x = I(m$X$echelles), termes_z = I(m$Z$termes),
       distance_x = m$options$distance_x %||% "variables",  # (« geographique » : en kilomètres)
       correlogramme = if (!is.null(m$correlogramme)) c(m$correlogramme[c("table", "classes", "sturges")], list(bornes = I(m$correlogramme$bornes))),
       paires = list(y = m$paires$y[garde], x = m$paires$x[garde], total = np))
})

# ---------------------------------------------------------------- Exporter

tableau_resume_communaute <- function(m) {
  r <- reglages_distance(m)
  data.frame(analyse = m$type, N = m$N, variables_Y = m$K, distance = r$distance, presences_absences = r$binaire, transformation = r$transformation,
             correction = r$correction, constante = r$constante, permutations = r$permutations, graine = r$graine, observations_laissees = length(m$exclues),
             stringsAsFactors = FALSE)
}

tables_ajustement <- function(a) {
  if (is.null(a)) return(list())
  out <- list()
  if (!is.null(a$vecteurs)) {
    out$variables_ajustees <- cbind(data.frame(Variable = names(a$vecteurs$r2), R2 = unname(a$vecteurs$r2), p = unname(a$vecteurs$p), check.names = FALSE),
                                    as.data.frame(a$vecteurs$direction, row.names = NULL))
  }
  if (!is.null(a$facteurs)) {
    out$facteurs_ajustes <- data.frame(Variable = vapply(a$facteurs, `[[`, "", "variable"), R2 = vapply(a$facteurs, `[[`, 0, "r2"), p = vapply(a$facteurs, `[[`, 0, "p"))
    out$centroides <- avec_noms(a$centroides, rownames(a$centroides), "Categorie")
  }
  out
}

tables_pcoa <- function(projet, m) {
  k <- m$noyau
  c(list(observations = avec_noms(k$points, m$noms), valeurs_propres = table_valeurs_pcoa(k), resume = tableau_resume_communaute(m)),
    if (!is.null(m$especes)) list(especes = avec_noms(m$especes, rownames(m$especes), "Variable")), tables_ajustement(m$ajustement))
}

tables_nmds <- function(projet, m) {
  k <- m$noyau
  obs <- avec_noms(k$points, m$noms)
  obs$stress <- k$stress_observations
  resume <- tableau_resume_communaute(m)
  resume$k <- m$options$k
  resume$stress <- k$stress
  resume$departs <- m$options$departs
  resume$meilleur_depart <- k$meilleur
  resume$retrouvee <- k$retrouvee
  resume$dissimilarites_nulles <- k$nuls
  c(list(observations = obs, resume = resume, departs = data.frame(depart = seq_along(k$stress_departs) - 1, stress = k$stress_departs)),
    if (length(k$shepard$dissimilarite) <= 100000) list(shepard = as.data.frame(k$shepard)),
    if (!is.null(m$especes)) list(especes = avec_noms(m$especes, rownames(m$especes), "Variable")), tables_ajustement(m$ajustement))
}

tables_permanova <- function(projet, m) {
  k <- m$noyau
  out <- list(permanova = k$table, modele = as.data.frame(k$modele), resume = tableau_resume_communaute(m))
  if (length(m$dispersions)) {
    ds <- m$dispersions
    out$dispersion_groupes <- do.call(rbind, lapply(ds, function(r) cbind(variable = r$variable, r$groupes)))
    out$dispersion_tests <- do.call(rbind, lapply(ds, function(r) cbind(variable = r$variable, r$anova)))
    out$dispersion_paires <- do.call(rbind, lapply(ds, function(r) cbind(variable = r$variable, r$tukey)))
    out$dispersion_distances <- do.call(rbind, lapply(ds, function(r) data.frame(Observation = unname(projet$noms_obs[nom_numero(r$observations)]), variable = r$variable,
                                                                                 groupe = as.character(r$groupe), distance = unname(r$distances), stringsAsFactors = FALSE)))
    rownames(out$dispersion_groupes) <- rownames(out$dispersion_tests) <- rownames(out$dispersion_paires) <- rownames(out$dispersion_distances) <- NULL
  }
  out
}

tables_mantel <- function(projet, m) {
  t <- m$test
  test <- data.frame(test = c("Mantel", if (!is.null(t$r_partiel)) traduire("Mantel partiel", "partial Mantel")), methode = m$options$methode,
                     r = c(t$r, t$r_partiel), p = c(t$p, t$p_partiel), permutations = t$permutations, stringsAsFactors = FALSE)
  c(list(mantel = test, resume = tableau_resume_communaute(m)), if (!is.null(m$correlogramme)) list(correlogramme = m$correlogramme$table))
}

# ---------------------------------------------------------------- texte des méthodes

REFERENCES_COMMUNAUTES <- list(
  gower66 = "Gower, J. C. (1966). Some distance properties of latent root and vector methods used in multivariate analysis. Biometrika, 53(3–4), 325–338. https://doi.org/10.1093/biomet/53.3-4.325",
  gower71 = "Gower, J. C. (1971). A general coefficient of similarity and some of its properties. Biometrics, 27(4), 857–871. https://doi.org/10.2307/2528823",
  bray = "Bray, J. R., & Curtis, J. T. (1957). An ordination of the upland forest communities of southern Wisconsin. Ecological Monographs, 27(4), 325–349. https://doi.org/10.2307/1942268",
  lingoes = "Lingoes, J. C. (1971). Some boundary conditions for a monotone analysis of symmetric matrices. Psychometrika, 36(2), 195–203. https://doi.org/10.1007/BF02291398",
  cailliez = "Cailliez, F. (1983). The analytical solution of the additive constant problem. Psychometrika, 48(2), 305–308. https://doi.org/10.1007/BF02294026",
  kruskal64a = "Kruskal, J. B. (1964a). Multidimensional scaling by optimizing goodness of fit to a nonmetric hypothesis. Psychometrika, 29(1), 1–27. https://doi.org/10.1007/BF02289565",
  kruskal64b = "Kruskal, J. B. (1964b). Nonmetric multidimensional scaling: A numerical method. Psychometrika, 29(2), 115–129. https://doi.org/10.1007/BF02289694",
  minchin = "Minchin, P. R. (1987). An evaluation of the relative robustness of techniques for ecological ordination. Vegetatio, 69(1–3), 89–107. https://doi.org/10.1007/BF00038690",
  venables = "Venables, W. N., & Ripley, B. D. (2002). Modern applied statistics with S (4th ed.). Springer. https://doi.org/10.1007/978-0-387-21706-2",
  anderson01 = "Anderson, M. J. (2001). A new method for non-parametric multivariate analysis of variance. Austral Ecology, 26(1), 32–46. https://doi.org/10.1111/j.1442-9993.2001.01070.pp.x",
  mcardle = "McArdle, B. H., & Anderson, M. J. (2001). Fitting multivariate models to community data: A comment on distance-based redundancy analysis. Ecology, 82(1), 290–297. https://doi.org/10.1890/0012-9658(2001)082[0290:FMMTCD]2.0.CO;2",
  anderson06 = "Anderson, M. J. (2006). Distance-based tests for homogeneity of multivariate dispersions. Biometrics, 62(1), 245–253. https://doi.org/10.1111/j.1541-0420.2005.00440.x",
  anderson06b = "Anderson, M. J., Ellingsen, K. E., & McArdle, B. H. (2006). Multivariate dispersion as a measure of beta diversity. Ecology Letters, 9(6), 683–693. https://doi.org/10.1111/j.1461-0248.2006.00926.x",
  mantel = "Mantel, N. (1967). The detection of disease clustering and a generalized regression approach. Cancer Research, 27(2), 209–220.",
  smouse = "Smouse, P. E., Long, J. C., & Sokal, R. R. (1986). Multiple regression and correlation extensions of the Mantel test of matrix correspondence. Systematic Zoology, 35(4), 627–632. https://doi.org/10.2307/2413122",
  oden = "Oden, N. L., & Sokal, R. R. (1986). Directional autocorrelation: An extension of spatial correlograms to two dimensions. Systematic Zoology, 35(4), 608–617. https://doi.org/10.2307/2413120",
  holm = "Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6(2), 65–70.")

texte_methodes_pcoa <- function(projet, d, m, langue = "fr", version = "") texte_methodes_communaute(projet, d, m, langue, version)
texte_methodes_nmds <- function(projet, d, m, langue = "fr", version = "") texte_methodes_communaute(projet, d, m, langue, version)
texte_methodes_permanova <- function(projet, d, m, langue = "fr", version = "") texte_methodes_communaute(projet, d, m, langue, version)
texte_methodes_mantel <- function(projet, d, m, langue = "fr", version = "") texte_methodes_communaute(projet, d, m, langue, version)

texte_methodes_communaute <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  nombre <- function(x, chiffres = 2) nombre_texte(x, chiffres, fr)
  pourcent <- function(x) pourcent_texte(x, fr)
  liste <- function(x) liste_mots(x, fr)
  valeur_p <- function(p) if (is.na(p)) "—" else if (p < 0.001) tx("p < 0,001", "p < 0.001") else paste("p =", nombre(p, 3))
  court <- function(x, n = 12) paste0(liste(utils::head(x, n)), if (length(x) > n) "…" else "")
  cites <- "legendre"
  cite <- function(...) cites <<- union(cites, c(...))
  o <- m$options
  B <- o$permutations
  # --- le tableau, sa transformation et sa distance
  exclues <- if (length(m$exclues) == 1) tx(" (une autre laissée de côté : une valeur y manque)", " (one other left out: a value is missing)")
             else if (length(m$exclues)) sprintf(tx(" (%d autres laissées de côté : une valeur y manque)", " (%d others left out: a value is missing)"), length(m$exclues)) else ""
  tableau <- sprintf(tx("le tableau %s et %d variables (%s)%s", "the table %s and %d variables (%s)%s"), observations_texte(m$N, d, fr, de = TRUE), m$K, court(m$Y$noms), exclues)
  transformation <- switch(o$transformation,
    racine2 = tx("transformées en racine carrée", "square-root transformed"), racine4 = tx("transformées en racine quatrième", "fourth-root transformed"),
    log1p = tx("transformées en log(x + 1)", "log(x + 1)-transformed"), presence = tx("réduites aux présences-absences", "reduced to presence–absence"),
    relative = tx("ramenées à des abondances relatives (chaque observation divisée par son total)", "turned into relative abundances (each observation divided by its total)"),
    wisconsin = tx("soumises à la double standardisation de Wisconsin (chaque variable divisée par son maximum, puis chaque observation par son total)",
                   "given Wisconsin double standardization (each variable divided by its maximum, then each observation by its total)"),
    racine_wisconsin = tx("transformées en racine carrée puis soumises à la double standardisation de Wisconsin (chaque variable divisée par son maximum, puis chaque observation par son total)",
                          "square-root transformed then given Wisconsin double standardization (each variable divided by its maximum, then each observation by its total)"),
    NULL)
  distance <- switch(o$distance,
    bray = { cite("bray"); if (o$binaire) tx("l'indice de Sørensen (la dissimilarité de Bray-Curtis sur les présences-absences ; Bray et Curtis, 1957)", "the Sørensen index (the Bray–Curtis dissimilarity on presence–absence; Bray & Curtis, 1957)")
             else tx("la dissimilarité de Bray-Curtis (Bray et Curtis, 1957)", "the Bray–Curtis dissimilarity (Bray & Curtis, 1957)") },
    jaccard = if (o$binaire) tx("la distance de Jaccard (sur les présences-absences)", "the Jaccard distance (on presence–absence)")
              else tx("la distance de Jaccard quantitative, 2B / (1 + B), B la dissimilarité de Bray-Curtis", "the quantitative Jaccard distance, 2B / (1 + B), B the Bray–Curtis dissimilarity"),
    gower = { cite("gower71"); tx("la distance de Gower (1971), la moyenne des écarts divisés par l'étendue de chaque variable", "the Gower (1971) distance, the mean of the differences divided by each variable's range") },
    euclidean = tx("la distance euclidienne", "the Euclidean distance"), manhattan = tx("la distance de Manhattan", "the Manhattan distance"),
    canberra = tx("la distance de Canberra (moyenne sur les variables non nulles dans l'une ou l'autre observation)", "the Canberra distance (averaged over the variables non-zero in either observation)"),
    kulczynski = tx("la dissimilarité de Kulczynski", "the Kulczynski dissimilarity"),
    hellinger = { cite("gallagher"); tx("la distance de Hellinger (euclidienne sur la racine des abondances relatives ; Legendre et Gallagher, 2001)", "the Hellinger distance (Euclidean on the square root of relative abundances; Legendre & Gallagher, 2001)") },
    chord = { cite("gallagher"); tx("la distance de corde (euclidienne sur les observations ramenées à une longueur 1 ; Legendre et Gallagher, 2001)", "the chord distance (Euclidean on the observations scaled to unit length; Legendre & Gallagher, 2001)") })
  if (o$binaire && !o$distance %in% c("bray", "jaccard")) distance <- paste(distance, tx("calculée sur les présences-absences", "computed on presence–absence"))
  donnees <- paste0(if (!is.null(transformation)) sprintf(tx("Les valeurs ont d'abord été %s. ", "The values were first %s. "), transformation) else "",
                    sprintf(tx("La distance entre observations est %s.", "The distance between observations is %s."), distance))
  correction <- if (!is.null(o$correction) && o$correction != "aucune" && m$type %in% c("PCOA", "PERMANOVA")) {
    cite(o$correction)
    sprintf(tx("Pour supprimer les valeurs propres négatives, la distance a été corrigée par la méthode de %s (%s ; constante %s).", "To remove negative eigenvalues, the distance was corrected by the %s method (%s; constant %s)."),
            if (o$correction == "lingoes") "Lingoes" else "Cailliez", if (o$correction == "lingoes") tx("√(d² + 2c) ; Lingoes, 1971", "√(d² + 2c); Lingoes, 1971") else tx("d + c ; Cailliez, 1983", "d + c; Cailliez, 1983"),
            nombre(m$constante, 4))
  } else ""
  groupes_perm <- if (!is.null(o$strates)) sprintf(tx(" à l'intérieur de chaque %s", " within each %s"), guillemets_texte(nom_groupement(projet, o$strates), fr)) else ""
  permutations <- sprintf(tx("%d permutations des observations%s (graine %d)", "%d permutations of the observations%s (seed %d)"), as.integer(B), groupes_perm, o$graine)
  # --- l'ajustement des variables X sur une ordination
  phrase_ajustement <- function(a, axes_txt) {
    if (is.null(a)) return("")
    v <- a$vecteurs
    f <- a$facteurs
    morceaux <- c(if (!is.null(v)) sprintf("%s (R² = %s, %s)", names(v$r2), nombre(v$r2, 2), vapply(v$p, valeur_p, "")),
                  if (!is.null(f)) sprintf("%s (R² = %s, %s)", vapply(f, `[[`, "", "variable"), nombre(vapply(f, `[[`, 0, "r2"), 2), vapply(vapply(f, `[[`, 0, "p"), valeur_p, "")))
    sprintf(tx("Les variables X ont été ajustées sur %s (comme vegan::envfit) : pour une variable quantitative, la direction de sa régression sur les axes et son R² ; pour une variable qualitative, les centroïdes de ses catégories et R² = 1 − somme des carrés dans les catégories / totale ; leur significativité par %s%s : %s.",
               "The X variables were fitted onto %s (as vegan::envfit): for a quantitative variable, the direction of its regression on the axes and its R²; for a qualitative variable, the centroids of its categories and R² = 1 − within-category sum of squares / total; their significance by %s%s: %s."),
            axes_txt, if (B > 0) permutations else tx("aucune permutation", "no permutation"),
            if (length(a$exclues)) sprintf(tx(", sur les %d observations dont les valeurs X sont connues", ", on the %d observations whose X values are known"), a$n) else "",
            paste(morceaux, collapse = tx(" ; ", "; ")))
  }
  especes <- function() if (!is.null(m$especes)) tx("Les scores des variables Y sont les moyennes des scores des observations pondérées par leurs valeurs (transformées), dilatées pour avoir la même variance que ceux-ci (vegan::wascores, expand = TRUE).",
                                                    "The scores of the Y variables are the averages of the observations' scores weighted by their (transformed) values, expanded to the same variance as these (vegan::wascores, expand = TRUE).") else ""
  if (m$type == "PCOA") {
    k <- m$noyau
    cite("gower66")
    vp <- k$valeurs / k$totale
    neg <- sum(!k$positifs)
    p1 <- sprintf(tx("Une analyse en coordonnées principales (PCoA ; Gower, 1966 ; Legendre et Legendre, 2012) a porté sur %s. %s",
                     "A principal coordinates analysis (PCoA; Gower, 1966; Legendre & Legendre, 2012) was performed on %s. %s"), tableau, donnees)
    p2 <- paste0(sprintf(tx("Les axes sont les vecteurs propres de la matrice −½ D² doublement centrée, multipliés par la racine de leur valeur propre ; %s. ",
                            "The axes are the eigenvectors of the doubly centred matrix −½ D², multiplied by the square root of their eigenvalue; %s. "),
                         if (length(vp) > 1) sprintf(tx("les deux premiers portent %s et %s de l'inertie totale (la somme des valeurs propres)", "the first two carry %s and %s of the total inertia (the sum of the eigenvalues)"), pourcent(vp[1]), pourcent(vp[2]))
                         else sprintf(tx("le premier porte %s de l'inertie totale", "the first carries %s of the total inertia"), pourcent(vp[1]))),
                 if (neg) sprintf(tx("La distance n'est pas euclidienne : %d valeurs propres sont négatives (leur somme vaut %s de celle des positives). ", "The distance is not Euclidean: %d eigenvalues are negative (their sum is %s of that of the positive ones). "),
                                  neg, pourcent(-sum(k$valeurs[!k$positifs]) / sum(k$valeurs[k$positifs]))) else "",
                 correction, if (nzchar(correction)) " " else "", especes())
    p3 <- phrase_ajustement(m$ajustement, tx("les deux premiers axes", "the first two axes"))
    paragraphes <- c(p1, p2, p3)
  } else if (m$type == "NMDS") {
    k <- m$noyau
    cite("kruskal64a", "kruskal64b", "minchin", "venables")
    p1 <- sprintf(tx("Un positionnement multidimensionnel non métrique (NMDS ; Kruskal, 1964a, 1964b ; Minchin, 1987) a porté sur %s. %s",
                     "A non-metric multidimensional scaling (NMDS; Kruskal, 1964a, 1964b; Minchin, 1987) was performed on %s. %s"), tableau, donnees)
    p2 <- paste0(sprintf(tx("La configuration en %d dimension%s est celle de la fonction isoMDS du paquet MASS (Venables et Ripley, 2002), qui rend minimal le stress de Kruskal entre les distances de l'ordination et la régression monotone des dissimilarités, depuis la configuration de la PCoA puis %d configurations au hasard (graine %d), comme vegan::metaMDS ; la solution de plus faible stress a été gardée (stress = %s)",
                            "The configuration in %d dimension%s is that of the isoMDS function of the MASS package (Venables & Ripley, 2002), which minimizes Kruskal's stress between the ordination distances and the monotone regression of the dissimilarities, from the PCoA configuration then %d random configurations (seed %d), as vegan::metaMDS; the lowest-stress solution was kept (stress = %s)"),
                         o$k, if (o$k > 1) "s" else "", o$departs, o$graine, nombre(k$stress, 3)),
                 if (o$departs > 0) {
                   if (k$retrouvee > 0) sprintf(tx(" et retrouvée %s (analyse procrustéenne : erreur quadratique moyenne < 0,005 et plus grand résidu < 0,01). ", " and found again %s (Procrustes analysis: root mean square error < 0.005 and largest residual < 0.01). "),
                                                if (k$retrouvee == 1) tx("une fois", "once") else sprintf(tx("%d fois", "%d times"), k$retrouvee))
                   else if (isTRUE(k$meme_stress > 0)) sprintf(tx(" ; aucun autre départ n'a retrouvé sa configuration (analyse procrustéenne : erreur quadratique moyenne < 0,005 et plus grand résidu < 0,01), mais %d ont atteint son stress à 0,0001 près, quelques observations placées un peu autrement : le stress est atteint, la place de ces observations est incertaine. ",
                                                                   "; no other start found its configuration again (Procrustes analysis: root mean square error < 0.005 and largest residual < 0.01), but %d reached its stress to 0.0001, with some observations placed slightly differently: the stress is reached, the place of those observations is uncertain. "), k$meme_stress)
                   else tx(" ; aucun autre départ ne l'a retrouvée : ce peut être un minimum local. ", "; no other start found it again: it may be a local minimum. ")
                 } else ". ",
                 if (k$nuls) sprintf(tx("%d dissimilarités nulles entre observations distinctes ont été remplacées par la moitié de la plus petite non nulle. ", "%d zero dissimilarities between distinct observations were replaced by half the smallest non-zero one. "), k$nuls) else "",
                 tx("La configuration a été centrée, tournée sur ses axes principaux et mise à l'échelle de la plus grande dissimilarité (vegan::postMDS, sans mise à l'échelle des demi-changements). ",
                    "The configuration was centred, rotated to its principal axes and scaled to the largest dissimilarity (vegan::postMDS, without half-change scaling). "), especes())
    p3 <- phrase_ajustement(m$ajustement, if (o$k > 1) sprintf(tx("les %d dimensions", "the %d dimensions"), o$k) else tx("la dimension", "the dimension"))
    paragraphes <- c(p1, p2, p3)
  } else if (m$type == "PERMANOVA") {
    k <- m$noyau
    cite("anderson01", "mcardle")
    termes <- unique(m$X$groupes)
    facon <- switch(o$par,
      termes = tx("de chaque terme après ceux qui le précèdent (sommes des carrés séquentielles, dans l'ordre des termes)", "of each term after those before it (sequential sums of squares, in the order of the terms)"),
      marges = tx("de chaque terme après tous les autres (effets marginaux)", "of each term after all the others (marginal effects)"),
      modele = tx("du modèle entier", "of the whole model"))
    p1 <- sprintf(tx("Une analyse de variance multivariée par permutation (PERMANOVA ; Anderson, 2001 ; McArdle et Anderson, 2001) a testé l'effet de %s sur %s. %s",
                     "A permutational multivariate analysis of variance (PERMANOVA; Anderson, 2001; McArdle & Anderson, 2001) tested the effect of %s on %s. %s"),
                  court(vapply(termes, guillemets_texte, "", fr = fr)), tableau, donnees)  # (les termes entre guillemets, comme les autres textes : l'effet de « habitat »)
    t <- k$table[seq_len(nrow(k$table) - 2), , drop = FALSE]
    resultats <- paste(sprintf("%s : R² = %s, pseudo-F = %s, %s", t$terme, nombre(t$R2, 3), nombre(t$F, 2), vapply(t$p, valeur_p, "")), collapse = tx(" ; ", "; "))
    if (!fr) resultats <- gsub(" : ", ": ", resultats, fixed = TRUE)
    p2 <- paste0(correction, if (nzchar(correction)) " " else "",
                 sprintf(tx("Le pseudo-F (somme des carrés par degré de liberté sur celle des résidus) %s a été comparé à celui de %s, comme vegan::adonis2 : %s. Le modèle explique %s de la somme des carrés totale (pseudo-F = %s, %s).",
                            "The pseudo-F (sum of squares per degree of freedom over that of the residuals) %s was compared with that of %s, as vegan::adonis2: %s. The model explains %s of the total sum of squares (pseudo-F = %s, %s)."),
                         facon, permutations, resultats, pourcent(k$modele$R2), nombre(k$modele$F, 2), valeur_p(k$modele$p)),
                 if (length(k$alias)) sprintf(tx(" Redondants avec les autres, laissés de côté : %s.", " Redundant with the others, left out: %s."), liste(k$alias)) else "")
    p3 <- ""
    if (length(m$dispersions)) {
      cite("anderson06", "anderson06b")
      ds <- m$dispersions
      res <- paste(vapply(ds, function(r) sprintf(tx("%s : F = %s, %s par permutation", "%s: F = %s, %s by permutation"), r$variable, nombre(r$F, 2), valeur_p(r$p_permutation)), ""), collapse = tx(" ; ", "; "))
      inegale <- Filter(function(r) isTRUE(r$p_permutation <= 0.05), ds)
      p3 <- paste0(sprintf(tx("La dispersion multivariée de chaque groupement (%s) a été comparée entre ses groupes (Anderson, 2006 ; Anderson et al., 2006) : les distances des observations %s de leur groupe dans l'espace de la PCoA, les axes de valeur propre négative retranchés (vegan::betadisper), par une ANOVA, ses résidus permutés %d fois (vegan::permutest.betadisper) et les paires de groupes de Tukey : %s.",
                              "The multivariate dispersion of each grouping (%s) was compared between its groups (Anderson, 2006; Anderson et al., 2006): the distances of the observations to the %s of their group in the PCoA space, the axes of negative eigenvalue subtracted (vegan::betadisper), by an ANOVA, its residuals permuted %d times (vegan::permutest.betadisper), and Tukey's pairs of groups: %s."),
                           liste(names(ds)), if (o$dispersion == "centroide") tx("au centroïde", "centroid") else tx("à la médiane spatiale", "spatial median"),
                           as.integer(B), res),
                   if (length(inegale)) sprintf(tx(" La dispersion diffère entre les groupes de %s : le test de position de la PERMANOVA peut alors venir de la dispersion autant que de la position des groupes, et se lit avec prudence (Anderson, 2006).",
                                                   " The dispersion differs between the groups of %s: the location test of the PERMANOVA may then come from the dispersion as much as from the position of the groups, and is to be read with care (Anderson, 2006)."),
                                                liste(vapply(inegale, `[[`, "", "variable"))) else "")
    }
    paragraphes <- c(p1, p2, p3)
  } else {
    t <- m$test
    cite("mantel")
    p1 <- sprintf(tx("Un test de Mantel (Mantel, 1967 ; Legendre et Legendre, 2012) a porté sur %s. %s La distance des variables X est %s.",
                     "A Mantel test (Mantel, 1967; Legendre & Legendre, 2012) was performed on %s. %s The distance of the X variables is %s."),
                  tableau, donnees, m$X$texte[[langue]])  # (distances_mantel_x)
    correlation <- if (o$methode == "spearman") tx("la corrélation des rangs de Spearman", "Spearman's rank correlation") else tx("la corrélation de Pearson", "Pearson's correlation")
    p2 <- sprintf(tx("La statistique de Mantel, %s entre les deux distances des mêmes paires d'observations, vaut r = %s ; sa valeur p unilatérale vient de %s de la distance du tableau Y (vegan::mantel) : %s.",
                     "The Mantel statistic, %s between the two distances of the same pairs of observations, is r = %s; its one-sided p-value comes from %s of the distance of the table Y (vegan::mantel): %s."),
                  correlation, nombre(t$r, 3), permutations, valeur_p(t$p))
    if (!is.null(t$r_partiel)) {
      cite("smouse")
      p2 <- paste(p2, sprintf(tx("Le test partiel (Smouse et al., 1986), la distance euclidienne des covariables Z (%s) retirée, donne r = %s, %s (vegan::mantel.partial).",
                                 "The partial test (Smouse et al., 1986), the Euclidean distance of the covariates Z (%s) removed, gives r = %s, %s (vegan::mantel.partial)."),
                              court(m$Z$variables), nombre(t$r_partiel, 3), valeur_p(t$p_partiel)))
    }
    p3 <- ""
    cg <- m$correlogramme
    if (!is.null(cg)) {
      cite("oden", "holm")
      sig <- cg$table[!is.na(cg$table$p_corrige) & cg$table$p_corrige <= 0.05, , drop = FALSE]
      p3 <- sprintf(tx("Le corrélogramme de Mantel (Oden et Sokal, 1986 ; Legendre et Legendre, 2012) découpe la distance X en %d classes d'égale largeur%s ; dans chacune, la corrélation de Mantel entre l'appartenance des paires à la classe et la distance du tableau Y, de signe changé (positive : des observations proches plus semblables), a été testée du côté de son signe par les mêmes permutations, les classes au-delà de la moitié seulement si chaque observation y a une paire, et les valeurs p corrigées progressivement par la méthode de Holm (1979), comme vegan::mantel.correlog : %s.",
                       "The Mantel correlogram (Oden & Sokal, 1986; Legendre & Legendre, 2012) cuts the X distance into %d classes of equal width%s; in each, the Mantel correlation between the pairs' membership of the class and the distance of the table Y, its sign changed (positive: close observations more alike), was tested on the side of its sign by the same permutations, the classes beyond half only if every observation has a pair in it, and the p-values corrected progressively by Holm's (1979) method, as vegan::mantel.correlog: %s."),
                    cg$classes, if (isTRUE(cg$sturges)) tx(" (règle de Sturges)", " (Sturges' rule)") else "",
                    if (nrow(sig)) paste0(if (nrow(sig) == 1) tx("une classe significative", "one significant class") else sprintf(tx("%d classes significatives", "%d significant classes"), nrow(sig)),
                                          " (", paste(sprintf(tx("classe %d : r = %s", "class %d: r = %s"), sig$classe, nombre(sig$r, 3)), collapse = tx(" ; ", "; ")), ")")
                    else tx("aucune classe significative après correction", "no class significant after correction"))
    }
    paragraphes <- c(p1, p2, p3)
  }
  annee_r <- R.version$year
  annee_nuee <- format(Sys.Date(), "%Y")
  cite("oksanen", "r", "nuee")
  logiciel <- sprintf(tx("Les calculs ont été faits avec Nuée%s (Boutin, %s) et R %s (R Core Team, %s), comme le paquet vegan les fait (Oksanen et al., 2025)%s.",
                         "Computations were done with Nuée%s (Boutin, %s) and R %s (R Core Team, %s), as the vegan package does them (Oksanen et al., 2025)%s."),
                      if (nzchar(version)) paste0(" ", version) else "", annee_nuee, paste(R.version$major, R.version$minor, sep = "."), annee_r,
                      if (m$type == "NMDS") sprintf(tx(", avec le paquet MASS %s (Venables et Ripley, 2002)", ", with the MASS package %s (Venables & Ripley, 2002)"), utils::packageDescription("MASS")$Version) else "")
  references <- vapply(cites, function(c) switch(c,
    r = sprintf(REFERENCES$r, annee_r),
    nuee = sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
    REFERENCES_COMMUNAUTES[[c]] %||% REFERENCES_ORDINATION[[c]]), "")
  list(paragraphes = c(paragraphes[nzchar(paragraphes)], logiciel), references = unname(sort(references)))
}
