# k-moyennes (stats::kmeans, algorithme de Hartigan et Wong, 1979) : k groupes d'observations
# qui rendent minimale la somme des carrés des écarts à leur centre, sur les variables X mises à
# l'échelle comme le dialogue le dit (onglet Variables : échelle, bloc, modificateur, et termes
# croisés). 25 départs au hasard (graine fixe : 1) ; le meilleur est gardé. Les groupes sont
# numérotés du plus grand au plus petit. Les observations où une valeur manque sont laissées de
# côté (les échelles sont calculées sur les autres).
#
# Leur qualité : la part de la variance entre les groupes (R² = somme des carrés inter / totale)
# et la silhouette de chaque observation (Rousseeuw, 1987 ; cluster::silhouette), calculée
# jusqu'à SILHOUETTE_MAX observations. À la demande, le choix de k : pour k = 2 à 10, la somme
# des carrés intra-groupes et la silhouette moyenne. Les groupes se voient sur les deux premières
# composantes principales des mêmes données ; ils peuvent devenir une variable qualitative du
# projet ; une nouvelle observation va au groupe du centre le plus proche.

SILHOUETTE_MAX <- 3000  # (au-delà, la matrice des distances deviendrait lourde)
DEPARTS_KMEANS <- 25

# Les réglages des k-moyennes (d$options).
reglages_kmeans <- function(d) list(k = option_nombre(d, "k", 3, 2, 50, TRUE))

# Les k-moyennes de X, reproductibles (graine 1, 25 départs), les groupes numérotés du plus grand
# au plus petit (à effectifs égaux, dans l'ordre de kmeans).
kmeans_nuee <- function(X, k) {
  a <- avec_graine(1, stats::kmeans(X, centers = k, nstart = DEPARTS_KMEANS, iter.max = 100))
  ordre <- order(-a$size, seq_len(k))
  list(groupe = match(a$cluster, ordre), centres = a$centers[ordre, , drop = FALSE], tailles = a$size[ordre],
       intra = a$withinss[ordre], totale = a$totss, inter = a$betweenss, iterations = a$iter, defaut = a$ifault)
}

# La distance (euclidienne, dans l'espace mis à l'échelle) de chaque ligne de X à chaque centre ;
# une valeur manquante est laissée de côté, la distance ramenée à tous les termes.
distances_centres <- function(X, centres) {
  # (une matrice même pour une seule ligne : vapply rendrait alors un vecteur)
  matrix(vapply(seq_len(nrow(centres)), function(g) {
    e <- sweep(X, 2, centres[g, ])^2
    connus <- rowSums(!is.na(e))
    ifelse(connus > 0, sqrt(rowSums(e, na.rm = TRUE) * ncol(X) / pmax(connus, 1)), NA_real_)
  }, numeric(nrow(X))), nrow(X), nrow(centres))
}
groupe_le_plus_proche <- function(D) {
  D <- matrix(D, ncol = max(1, ncol(D)))
  apply(D, 1, function(v) if (all(is.na(v))) NA_integer_ else which.min(v))
}

# La silhouette de chaque observation : sa largeur et le groupe voisin (NULL au-delà de
# SILHOUETTE_MAX observations, ou avec un seul groupe).
silhouette_kmeans <- function(X, groupe, distances = NULL) {
  if (nrow(X) > SILHOUETTE_MAX || length(unique(groupe)) < 2) return(NULL)
  s <- cluster::silhouette(groupe, distances %||% stats::dist(X))
  data.frame(voisin = s[, "neighbor"], largeur = s[, "sil_width"])
}

ajuster_kmeans <- function(projet, d) {
  r <- reglages_kmeans(d)
  if (length(d$y) || (length(d$classe) && !is.na(d$classe[1]))) {
    stop(traduire("Les k-moyennes n'ont pas de réponse : seulement des variables X.", "k-means have no response: only X variables."), call. = FALSE)
  }
  if (!length(d$x)) stop(traduire("Les k-moyennes demandent au moins une variable X.", "k-means need at least one X variable."), call. = FALSE)
  obs <- d$observations
  complet <- stats::complete.cases(construire_bloc(projet, d$x, obs)$X)
  garde <- obs[complet]
  bx <- construire_bloc(projet, d$x, garde)
  X <- bx$X
  k <- r$k
  distincts <- nrow(unique(X))
  if (distincts <= k) {
    stop(sprintf(traduire("%d groupes demandent plus de %d observations distinctes et complètes (il y en a %d).", "%d clusters need more than %d distinct and complete observations (there are %d)."),
                 k, k, distincts), call. = FALSE)
  }
  avertissements <- character(0)
  a <- withCallingHandlers(kmeans_nuee(X, k), warning = function(w) { avertissements <<- c(avertissements, conditionMessage(w)); invokeRestart("muffleWarning") })
  sil <- silhouette_kmeans(X, a$groupe)
  acp <- stats::prcomp(X, center = TRUE, scale. = FALSE)
  nc <- min(2, ncol(acp$x))
  bruts <- valeurs_bloc(projet, d$x, garde)
  simples <- which(vapply(d$x, function(t) length(t$variables) == 1, TRUE))
  unites <- rowsum(bruts[, simples, drop = FALSE], a$groupe, reorder = TRUE) / a$tailles
  colnames(unites) <- bx$noms[simples]
  m <- list(type = "KMEANS", N = nrow(X), K = ncol(X), observations = garde, exclues = setdiff(obs, garde), reglages_kmeans = r, k = k,
            donnees = X, bloc = bx[c("centre", "poids", "type", "facteur", "noms")], groupe = a$groupe, centres = a$centres, tailles = a$tailles,
            intra = a$intra, totale = a$totale, inter = a$inter, r2 = a$inter / a$totale, silhouette = sil, distincts = distincts,
            distance = distances_centres(X, a$centres)[cbind(seq_len(nrow(X)), a$groupe)],
            acp = list(scores = acp$x[, seq_len(nc), drop = FALSE], part = acp$sdev^2 / sum(acp$sdev^2),
                       centres = scale(a$centres, center = acp$center, scale = FALSE) %*% acp$rotation[, seq_len(nc), drop = FALSE]),
            centres_unites = unites, avertissements = unique(avertissements), echelles = vapply(d$x, function(t) as.character(t$scaling$type %||% "UV")[1], ""))
  m
}

# Les termes X d'observations, mis à l'échelle comme ceux du modèle (pas de jeu de test : une
# nouvelle observation se range par Prédire).
x_kmeans <- function(projet, m, d, observations) construire_bloc(projet, d$x, observations, imposees = m$bloc)$X

# Le choix de k : pour k = 2 à 10 (moins s'il y a peu d'observations distinctes), la somme des
# carrés intra-groupes, le R² et la silhouette moyenne, comme le modèle les calcule.
choix_k <- function(m) {
  ks <- 2:min(10, m$distincts - 1)
  distances <- if (m$N <= SILHOUETTE_MAX) stats::dist(m$donnees)
  do.call(rbind, lapply(ks, function(k) {
    a <- suppressWarnings(kmeans_nuee(m$donnees, k))
    s <- if (!is.null(distances)) silhouette_kmeans(m$donnees, a$groupe, distances)
    data.frame(k = k, intra = sum(a$intra), r2 = a$inter / a$totale, silhouette = if (is.null(s)) NA_real_ else mean(s$largeur))
  }))
}

demande_analyse_kmeans_choix <- function(q) {
  e <- modele(q$modele)
  if (!identical(e$modele$type, "KMEANS")) stop(traduire("Seulement pour des k-moyennes.", "Only for k-means."), call. = FALSE)
  if (is.null(e$modele$choix)) etat$modeles[[q$modele]]$modele$choix <- choix_k(e$modele)
  list(modele = q$modele, choix = etat$modeles[[q$modele]]$modele$choix, k = e$modele$k)
}

# Les groupes en variable qualitative du projet (catégories 1 à k), pour les observations du
# modèle ; annulable comme les autres modifications des données.
demande_analyse_kmeans_variable <- function(q) {
  e <- modele(q$modele)
  if (!identical(e$modele$type, "KMEANS")) stop(traduire("Seulement pour des k-moyennes.", "Only for k-means."), call. = FALSE)
  m <- e$modele
  nom <- trimws(as.character(q$nom %||% ""))
  if (!length(nom) || !nzchar(nom[1])) nom <- sprintf("%s %s", e$definition$name %||% "k", traduire("groupes", "clusters"))
  changer_projet(list(projet = e$projet), function(p) ajouter_colonne(p, nom[1], m$observations, m$groupe, categories = as.character(seq_len(m$k))),
                 sprintf("variable %s", nom[1]))
}

# Les observations des k-moyennes : leur groupe (en texte, pour les couleurs), sa distance au
# centre, la silhouette et le groupe voisin, les deux premières composantes principales.
observations_kmeans <- function(projet, m) {
  obs <- data.frame(uid = m$observations, nom = unname(projet$noms_obs[as.character(m$observations)]), classe = as.character(m$groupe),
                    groupe = m$groupe, distance = m$distance, stringsAsFactors = FALSE)
  if (!is.null(m$silhouette)) {
    obs$silhouette <- m$silhouette$largeur
    obs$voisin <- m$silhouette$voisin
  }
  for (j in seq_len(ncol(m$acp$scores))) obs[[paste0("CP", j)]] <- unname(m$acp$scores[, j])
  obs
}

# Les groupes : effectif, part, somme des carrés intra et silhouette moyenne.
tailles_kmeans <- function(m) {
  data.frame(groupe = seq_len(m$k), n = m$tailles, part = m$tailles / m$N, intra = m$intra,
             silhouette = if (!is.null(m$silhouette)) vapply(seq_len(m$k), function(g) mean(m$silhouette$largeur[m$groupe == g]), 0) else NA_real_)
}

centres_tableau <- function(M) {
  out <- data.frame(groupe = seq_len(nrow(M)), unclass(M), check.names = FALSE, row.names = NULL)
  names(out)[-1] <- colnames(M)
  out
}

resultat_kmeans <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  sil <- if (!is.null(m$silhouette)) mean(m$silhouette$largeur) else NA_real_
  list(id = id, projet = p$id, nom = d$name %||% "", titre = d$title %||% "", type = "KMEANS", analyse = TRUE, prediction = TRUE, regression = FALSE,
       opls = FALSE, Ap = 0, A = 0, N = m$N, K = m$K, noms_y = I(character(0)), k = m$k, observations = observations_kmeans(p, m),
       tailles = tailles_kmeans(m), centres = centres_tableau(m$centres), centres_unites = centres_tableau(m$centres_unites),
       inertie = list(totale = m$totale, intra = sum(m$intra), inter = m$inter, r2 = m$r2), silhouette = sil, silhouette_max = SILHOUETTE_MAX,
       acp = list(part = I(unname(m$acp$part)), centres = stats::setNames(centres_tableau(m$acp$centres), c("groupe", paste0("CP", seq_len(ncol(m$acp$centres)))))),
       choix = m$choix,
       echelles = I(unique(m$echelles)), avertissements = I(m$avertissements), exclues_manquantes = length(m$exclues),
       reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "", test = m$test,
       demande = d$demande %||% list(type = d$type, numero = d$number),
       vues_defaut = I(c("km_plan", if (!is.null(m$silhouette)) "km_silhouette" else "km_centres")),
       chiffres = sprintf("k = %d · R2 %.2f%s", m$k, m$r2, if (is.na(sil)) "" else sprintf(" · %s %.2f", traduire("silhouette", "silhouette"), sil)))
}

predire_kmeans <- function(projet, m, d, observations) {
  D <- distances_centres(x_kmeans(projet, m, d, observations), m$centres)
  g <- groupe_le_plus_proche(D)
  data.frame(`groupe prédit` = g, `distance au centre` = ifelse(is.na(g), NA_real_, D[cbind(seq_along(observations), ifelse(is.na(g), 1L, g))]),
             hors_domaine = FALSE, check.names = FALSE)
}

tables_kmeans <- function(projet, m) {
  obs <- observations_kmeans(projet, m)
  obs$uid <- NULL
  obs$classe <- NULL
  names(obs)[names(obs) == "nom"] <- "Observation"
  out <- list(resume = data.frame(N = m$N, termes = m$K, k = m$k, departs = DEPARTS_KMEANS, graine = 1, inertie_totale = m$totale, inertie_intra = sum(m$intra),
                                  inertie_inter = m$inter, R2 = m$r2, silhouette = if (!is.null(m$silhouette)) mean(m$silhouette$largeur) else NA_real_),
              observations = obs, groupes = tailles_kmeans(m), centres = centres_tableau(m$centres_unites), centres_echelle = centres_tableau(m$centres))
  if (!is.null(m$choix)) out$choix_k <- m$choix
  out
}

# Les échelles des termes, en mots.
echelles_texte <- function(echelles, fr) {
  mots <- c(UV = if (fr) "centrés et réduits" else "centred and scaled to unit variance",
            Par = if (fr) "centrés et mis à l'échelle de Pareto" else "centred and Pareto-scaled",
            Ctr = if (fr) "centrés" else "centred", none = if (fr) "sans mise à l'échelle" else "unscaled")
  e <- unique(echelles)
  e <- ifelse(e %in% names(mots), mots[e], e)
  liste_mots(e, fr)
}

texte_methodes_kmeans <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  noms <- m$bloc$noms
  p1 <- sprintf(if (fr) "Les k-moyennes portent sur %s%s et %s, %s."
                else "The k-means use %s%s and %s, %s.",
                observations_texte(m$N, d, fr), if (length(m$exclues)) sprintf(if (fr) " (%d autres laissées de côté : une valeur y manque)" else " (%d others left out: a value is missing)", length(m$exclues)) else "",
                if (length(noms) == 1) sprintf(if (fr) "1 terme (%s)" else "1 term (%s)", noms)
                else if (length(noms) <= 12) sprintf(if (fr) "%d termes (%s)" else "%d terms (%s)", length(noms), liste_mots(noms, fr)) else sprintf(if (fr) "%d termes" else "%d terms", length(noms)),
                echelles_texte(m$echelles, fr))
  p2 <- sprintf(if (fr) "Les observations ont été réparties en %d groupes par l'algorithme de Hartigan et Wong (1979) (fonction kmeans de R), qui rend minimale la somme des carrés des écarts à leur centre ; le meilleur de %d départs au hasard a été gardé (graine 1)."
                else "The observations were split into %d clusters by the Hartigan and Wong (1979) algorithm (R's kmeans function), which minimizes the sum of squared deviations from their centre; the best of %d random starts was kept (seed 1).",
                m$k, DEPARTS_KMEANS)
  tailles <- sprintf("%d", m$tailles)
  p3 <- sprintf(if (fr) "Les groupes comptent %s observations ; la variance entre les groupes représente %s de la variance totale (R² = %s)%s."
                else "The clusters hold %s observations; the variance between clusters is %s of the total variance (R² = %s)%s.",
                liste_mots(tailles, fr), pourcent_texte(m$r2, fr), nombre_texte(m$r2, 2, fr),
                if (!is.null(m$silhouette)) sprintf(if (fr) " ; leur silhouette moyenne (Rousseeuw, 1987 ; paquet cluster) est de %s" else "; their mean silhouette (Rousseeuw, 1987; cluster package) is %s",
                                                    nombre_texte(mean(m$silhouette$largeur), 2, fr)) else "")
  if (!is.null(m$choix)) {
    meilleur <- m$choix$k[which.max(m$choix$silhouette)]
    p3 <- paste(p3, sprintf(if (fr) "De k = 2 à %d, %s." else "From k = 2 to %d, %s.", max(m$choix$k),
                            if (length(meilleur) && !is.na(meilleur)) sprintf(if (fr) "la silhouette moyenne est la plus grande pour k = %d" else "the mean silhouette is largest for k = %d", meilleur)
                            else (if (fr) "la somme des carrés intra-groupes a été calculée pour chaque k" else "the within-cluster sum of squares was computed for each k")))
  }
  logiciels <- references_classement(c("hartigan", if (!is.null(m$silhouette)) c("rousseeuw", "cluster")), if (!is.null(m$silhouette)) "cluster", fr, version)
  list(paragraphes = c(p1, p2, p3, logiciels$phrase), references = logiciels$references)
}
