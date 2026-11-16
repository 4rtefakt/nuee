# Espèces indicatrices (type "INDVAL") : la valeur indicatrice de chaque espèce pour chaque groupe
# d'observations (Dufrêne et Legendre 1997), ou pour chaque combinaison de groupes (De Cáceres,
# Legendre et Moretti 2010), comme indicspecies::multipatt la calcule : mêmes combinaisons, mêmes
# statistiques et, pour les mêmes permutations, mêmes valeurs p. indicspecies n'est pas livré avec
# Nuée : moteur/tests/verifier_indicatrices.R le compare quand il est installé.
#
# Pour une espèce et un groupe (ou une combinaison de groupes, prise comme un seul groupe) :
#   A, la spécificité : la part de l'abondance de l'espèce qui est dans ce groupe ; IndVal.g
#     (De Cáceres et Legendre 2009, par défaut) compare les abondances moyennes de chaque groupe,
#     qui comptent alors autant quels que soient leurs effectifs : A = (somme des moyennes des
#     groupes de la combinaison) / (somme des moyennes de tous les groupes) ; IndVal compare les
#     sommes : A = (abondance dans le groupe) / (abondance totale) ;
#   B, la fidélité : la part des observations du groupe où l'espèce est présente ;
#   la valeur indicatrice, racine(A × B) (de 0 à 1), comme indicspecies la donne.
# Le groupe (ou la combinaison) de chaque espèce : celui de sa plus grande valeur. Son test par
# permutation (De Cáceres et Legendre 2009) : les groupes des observations permutés (librement ou à
# l'intérieur de strates, avec une graine), la plus grande valeur de l'espèce recalculée ; p = (nombre
# de permutations où elle est au moins aussi grande + 1) / (permutations + 1). Une espèce dont la
# combinaison est celle de tous les groupes (présente partout) n'est pas testée (indicspecies : NA).
# Les p corrigées sur les espèces (Benjamini–Hochberg ou Holm) sont données à côté des p brutes.
# L'export vers R est dans communautes_script.R : son script reprend les fonctions de ce fichier
# telles qu'elles y sont écrites.

# ---------------------------------------------------------------- les calculs

# Les combinaisons de groupes, de 1 à `ordre` groupes (indicspecies : cl.comb) : une colonne par
# combinaison (1 pour ses groupes), dans l'ordre de combn, nommée « a+b ».
combinaisons_groupes <- function(niveaux, ordre) {
  k <- length(niveaux)
  colonnes <- lapply(seq_len(ordre), function(j) {
    co <- utils::combn(k, j)
    M <- matrix(0, k, ncol(co))
    M[cbind(as.vector(co), rep(seq_len(ncol(co)), each = j))] <- 1
    colnames(M) <- apply(co, 2, function(i) paste(niveaux[i], collapse = "+"))
    M
  })
  C <- do.call(cbind, colonnes)
  rownames(C) <- niveaux
  C
}

# A, B et la valeur indicatrice de chaque combinaison (lignes) pour chaque espèce (colonnes) : X, les
# abondances (observations × espèces) ; g, le groupe de chaque observation (1 à k) ; C, les
# combinaisons (combinaisons_groupes) ; `egalises` : IndVal.g (les moyennes des groupes) ou IndVal.
statistiques_indval <- function(X, g, C, egalises = TRUE) {
  k <- nrow(C)
  G <- diag(1, k)[g, , drop = FALSE]
  n_k <- colSums(G)
  presences <- crossprod(G, X > 0)  # (observations du groupe où l'espèce est présente)
  B <- crossprod(C, presences) / as.vector(crossprod(C, n_k))
  sommes <- crossprod(G, X)
  parts <- if (egalises) sommes / n_k else sommes  # (les moyennes des groupes, ou leurs sommes)
  A <- sweep(crossprod(C, parts), 2, colSums(parts), "/")
  list(A = A, B = B, IndVal = sqrt(A * B))
}

# La plus grande valeur de chaque espèce et sa combinaison (la première en cas d'égalité).
meilleures_indval <- function(IV) {
  j <- max.col(t(IV), ties.method = "first")
  list(rang = j, valeur = IV[cbind(j, seq_len(ncol(IV)))])
}

# Le test par permutation de la plus grande valeur de chaque espèce (`valeur`) : pour chaque
# permutation (une ligne de P) des groupes des observations, la plus grande valeur de l'espèce
# recalculée ; p = (nombre de permutations où elle est au moins aussi grande + 1) / (permutations + 1).
test_indval <- function(X, g, C, egalises, valeur, P) {
  plus <- numeric(ncol(X))
  for (b in seq_len(nrow(P))) {
    vb <- meilleures_indval(statistiques_indval(X, g[P[b, ]], C, egalises)$IndVal)$valeur
    plus <- plus + (vb >= valeur - EPS_PERMUTATIONS)
  }
  (plus + 1) / (nrow(P) + 1)
}

# Les réglages (d$options) : l'indice ("IndVal.g" ou "IndVal"), l'ordre le plus grand des
# combinaisons (1 : les groupes seuls ; NULL : jusqu'à tous les groupes, comme multipatt), les
# présences seules (au lieu des abondances), le nombre de permutations (0 : aucun test), la graine,
# leurs strates, la correction des p sur les espèces.
COMBINAISONS_MAX <- 4095  # (2^12 - 1 : au-delà, demander un ordre plus petit)
reglages_indval <- function(d) {
  o <- d$options %||% list()
  B <- suppressWarnings(as.integer(o$permutations %||% 999))
  graine <- suppressWarnings(as.integer(o$graine %||% 1))
  ordre <- suppressWarnings(as.integer(unlist(o$ordre)))
  list(indice = option_choix(d, "indice", c("IndVal.g", "IndVal")),
       ordre = if (length(ordre) && !is.na(ordre[1]) && ordre[1] >= 1) ordre[1] else NA_integer_,
       presence = isTRUE(as.logical(o$presence)),
       permutations = if (is.na(B) || B < 0) 999L else min(B, 99999L), graine = if (is.na(graine)) 1L else graine,
       strates = if (length(o$strates$by)) specs_groupement(o$strates)[[1]],
       correction = option_choix(d, "correction", c("BH", "holm")))
}

# Les espèces indicatrices (definir_modele, type "INDVAL") : les espèces (rôle Y : des abondances,
# ou des présences 0/1), la variable qualitative des groupes (rôle Classe). Les observations sans
# groupe ou où une valeur manque sont laissées de côté ; les espèces absentes de toutes celles qui
# restent aussi.
ajuster_indval <- function(projet, d) {
  o <- reglages_indval(d)
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  if (!length(d$y)) err("Les espèces indicatrices demandent des espèces en Y (des abondances, ou des présences).",
                        "Indicator species need species as Y (abundances, or presences).")
  if (length(d$x)) err("Les espèces indicatrices ne prennent pas de variable X : les espèces en Y, les groupes en Classe.",
                       "Indicator species take no X variable: the species as Y, the groups as Class.")
  if (!length(d$classe) || is.na(d$classe[1])) err("Les espèces indicatrices demandent une variable de groupes (rôle Classe).",
                                                   "Indicator species need a grouping variable (Class role).")
  vc <- variable_du_projet(projet, d$classe)
  if (!length(vc$categories)) err("La variable des groupes doit être qualitative.", "The grouping variable must be qualitative.")
  obs <- as.numeric(d$observations)
  X <- reponses_ordination(projet, d$y, obs, traduire("Les espèces d'une analyse des espèces indicatrices", "The species of an indicator species analysis"))
  if (any(X < 0, na.rm = TRUE)) err("Les espèces indicatrices demandent des valeurs positives ou nulles (des abondances, des présences).",
                                    "Indicator species need positive or zero values (abundances, presences).")
  groupe <- as.character(vc$categories)[colonne(projet, d$classe, obs)]
  strates <- if (!is.null(o$strates)) valeurs_groupement(projet, o$strates, obs)
  connu <- !is.na(groupe) & stats::complete.cases(X) & (if (is.null(strates)) TRUE else !is.na(strates))
  garde <- obs[connu]
  X <- X[connu, , drop = FALSE]
  if (o$presence) X[] <- (X > 0) * 1
  niveaux <- intersect(as.character(vc$categories), groupe[connu])
  if (length(niveaux) < 2) err("Les espèces indicatrices demandent au moins deux groupes d'observations.", "Indicator species need at least two groups of observations.")
  g <- match(groupe[connu], niveaux)
  absentes <- colnames(X)[colSums(X) <= 0]
  X <- X[, colSums(X) > 0, drop = FALSE]
  if (!ncol(X)) err("Aucune espèce n'est présente dans ces observations.", "No species is present in these observations.")
  k <- length(niveaux)
  ordre <- if (is.na(o$ordre)) k else min(o$ordre, k)
  C <- combinaisons_groupes(niveaux, ordre)
  if (ncol(C) > COMBINAISONS_MAX) {
    stop(sprintf(traduire("%d groupes donnent %d combinaisons jusqu'à l'ordre %d : choisissez un ordre plus petit (Modifier le modèle).",
                          "%d groups give %d combinations up to order %d: choose a smaller order (Edit model)."), k, ncol(C), ordre), call. = FALSE)
  }
  egalises <- identical(o$indice, "IndVal.g")
  s <- statistiques_indval(X, g, C, egalises)
  meilleures <- meilleures_indval(s$IndVal)
  tous <- colSums(C) == k  # (la combinaison de tous les groupes, quand l'ordre va jusque-là)
  if (!is.null(strates)) {
    strates <- strates[connu]
    if (all(table(strates) < 2)) err("Les groupes des permutations doivent avoir chacun plusieurs observations.", "The permutation groups must each hold several observations.")
  }
  m <- list(type = "INDVAL", observations = garde, exclues = setdiff(obs, garde), N = length(garde), reglages_indval = o, ordre = ordre,
            especes = colnames(X), absentes = absentes, niveaux = niveaux, groupe = g, effectifs = tabulate(g, k), combinaisons = C,
            A = s$A, B = s$B, IndVal = s$IndVal, meilleure = meilleures$rang, valeur = meilleures$valeur,
            classe_nom = as.character(vc$ids[1]), donnees = X, strates = strates)
  m$p <- rep(NA_real_, ncol(X))
  if (o$permutations > 0) {
    P <- permutations_lignes(m$N, o$permutations, strates, o$graine)
    m$p <- test_indval(X, g, C, egalises, m$valeur, P)
    m$p[tous[m$meilleure]] <- NA
  }
  m$p_ajuste <- rep(NA_real_, ncol(X))
  testees <- !is.na(m$p)
  m$p_ajuste[testees] <- stats::p.adjust(m$p[testees], o$correction)
  m
}

# Le tableau des espèces : sa combinaison, A, B, la valeur indicatrice, p, p corrigée, et ses
# présences (observations où elle est) et son abondance totale ; une colonne 0/1 par groupe.
table_indval <- function(m) {
  j <- m$meilleure
  i <- seq_along(m$especes)
  D <- data.frame(espece = m$especes, groupe = colnames(m$combinaisons)[j], ordre = colSums(m$combinaisons)[j],
                  A = m$A[cbind(j, i)], B = m$B[cbind(j, i)], indval = m$valeur, p = m$p, p_ajuste = m$p_ajuste,
                  presences = colSums(m$donnees > 0), abondance = colSums(m$donnees), stringsAsFactors = FALSE, row.names = NULL)
  for (k in seq_along(m$niveaux)) D[[paste0("s.", m$niveaux[k])]] <- m$combinaisons[k, j]
  D
}

# Les valeurs d'une statistique (A, B, IndVal) en tableau : une ligne par espèce, une colonne par combinaison.
table_statistique <- function(m, M) {
  D <- data.frame(espece = m$especes, t(M), check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
  names(D)[-1] <- colnames(m$combinaisons)
  D
}

# Le nombre d'espèces retenues au seuil de 0,05, d'après les p brutes et d'après les p corrigées.
retenues_indval <- function(m, seuil = 0.05) c(brutes = sum(m$p <= seuil, na.rm = TRUE), corrigees = sum(m$p_ajuste <= seuil, na.rm = TRUE))

# ---------------------------------------------------------------- l'interface

resultat_indval <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  o <- m$reglages_indval
  r <- retenues_indval(m)
  noms_obs <- unname(p$noms_obs[nom_numero(m$observations)])
  groupe <- m$niveaux[m$groupe]
  list(id = id, projet = p$id, nom = d$name %||% "", titre = d$title %||% "", type = "INDVAL", analyse = TRUE, prediction = FALSE, regression = FALSE,
       opls = FALSE, Ap = 0, A = 0, N = m$N, K = length(m$especes), noms_y = I(m$especes),
       chiffres = if (o$permutations > 0) sprintf(traduire("%d espèces indicatrices (p corrigée ≤ 0,05)", "%d indicator species (corrected p ≤ 0.05)"), r[["corrigees"]])
                  else sprintf(traduire("%d espèces, sans test", "%d species, no test"), length(m$especes)),
       vues_defaut = I(c("indicatrices", "indval_graphe")),
       observations = data.frame(uid = m$observations, nom = noms_obs, classe = groupe, groupe = groupe, stringsAsFactors = FALSE),
       especes = table_indval(m), groupes = data.frame(groupe = m$niveaux, n = m$effectifs, stringsAsFactors = FALSE),
       combinaisons = I(colnames(m$combinaisons)), ordre = m$ordre,
       indval = table_statistique(m, m$IndVal), specificite = table_statistique(m, m$A), fidelite = table_statistique(m, m$B),
       retenues = as.list(r), classe_nom = m$classe_nom, absentes = I(m$absentes),
       options = o[c("indice", "ordre", "presence", "permutations", "graine", "correction")],
       strates = if (!is.null(o$strates)) nom_groupement(p, o$strates), exclues_manquantes = length(m$exclues),
       reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       demande = d$demande %||% list(type = d$type, numero = d$number))
}

tables_indval <- function(projet, m) {
  o <- m$reglages_indval
  r <- retenues_indval(m)
  list(especes = table_indval(m),
       resume = data.frame(N = m$N, especes = length(m$especes), groupes = length(m$niveaux), indice = o$indice, ordre = m$ordre,
                           presences = o$presence, permutations = o$permutations, graine = o$graine, correction = o$correction,
                           retenues_p = r[["brutes"]], retenues_p_ajuste = r[["corrigees"]]),
       groupes = data.frame(groupe = m$niveaux, n = m$effectifs),
       indval = table_statistique(m, m$IndVal), A = table_statistique(m, m$A), B = table_statistique(m, m$B))
}

# ---------------------------------------------------------------- texte des méthodes

REFERENCES_INDICATRICES <- list(
  dufrene = "Dufrêne, M., & Legendre, P. (1997). Species assemblages and indicator species: The need for a flexible asymmetrical approach. Ecological Monographs, 67(3), 345–366. https://doi.org/10.1890/0012-9615(1997)067[0345:SAAIST]2.0.CO;2",
  decaceres2009 = "De Cáceres, M., & Legendre, P. (2009). Associations between species and groups of sites: Indices and statistical inference. Ecology, 90(12), 3566–3574. https://doi.org/10.1890/08-1823.1",
  decaceres2010 = "De Cáceres, M., Legendre, P., & Moretti, M. (2010). Improving indicator species analysis by combining groups of sites. Oikos, 119(10), 1674–1684. https://doi.org/10.1111/j.1600-0706.2010.18334.x",
  bh = "Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery rate: A practical and powerful approach to multiple testing. Journal of the Royal Statistical Society: Series B, 57(1), 289–300. https://doi.org/10.1111/j.2517-6161.1995.tb02031.x",
  holm = "Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6(2), 65–70.")

texte_methodes_indval <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  o <- m$reglages_indval
  cites <- c("dufrene", "decaceres2009")
  k <- length(m$niveaux)
  r <- retenues_indval(m)
  court <- function(x, n = 12) paste0(liste_mots(utils::head(x, n), fr), if (length(x) > n) "…" else "")
  exclues <- if (length(m$exclues) == 1) tx(" (une autre laissée de côté : sans groupe, ou une valeur y manque)", " (one other left out: no group, or a value is missing)")
             else if (length(m$exclues)) sprintf(tx(" (%d autres laissées de côté : sans groupe, ou une valeur y manque)", " (%d others left out: no group, or a value is missing)"), length(m$exclues)) else ""
  absentes <- if (length(m$absentes) == 1) sprintf(tx(" ; %s, absente de ces observations, en a été retirée", "; %s, absent from these observations, was left out"), m$absentes)
              else if (length(m$absentes)) sprintf(tx(" ; %s, absentes de ces observations, en ont été retirées", "; %s, absent from these observations, were left out"), liste_mots(m$absentes, fr)) else ""
  donnees <- if (o$presence) tx("leurs présences (0/1)", "their presences (0/1)") else tx("leurs abondances", "their abundances")
  p1 <- sprintf(tx("Les espèces indicatrices des %d groupes de %s (%s, de %s observations) ont été cherchées parmi %d espèces (%s), d'après %s, sur %s%s%s.",
                   "Indicator species of the %d groups of %s (%s, of %s observations) were sought among %d species (%s), from %s, on %s%s%s."),
                k, guillemets_texte(m$classe_nom, fr), liste_mots(m$niveaux, fr), liste_mots(as.character(m$effectifs), fr), length(m$especes), court(m$especes),
                donnees, observations_texte(m$N, d, fr), exclues, absentes)
  indice <- if (identical(o$indice, "IndVal.g")) {
    tx("la valeur indicatrice IndVal.g de De Cáceres et Legendre (2009), qui compare les abondances moyennes des groupes (des groupes d'effectifs différents comptent autant)",
       "the indicator value IndVal.g of De Cáceres and Legendre (2009), which compares the mean abundances of the groups (groups of different sizes count alike)")
  } else tx("la valeur indicatrice IndVal de Dufrêne et Legendre (1997), qui compare les abondances totales des groupes",
            "the indicator value IndVal of Dufrêne and Legendre (1997), which compares the total abundances of the groups")
  combinaisons <- if (m$ordre > 1) {
    cites <- c(cites, "decaceres2010")
    sprintf(tx("pour chaque groupe et chaque combinaison de %s groupes (De Cáceres et al., 2010)", "for each group and each combination of %s groups (De Cáceres et al., 2010)"),
            if (m$ordre >= k) tx(sprintf("2 à %d", k), sprintf("2 to %d", k)) else tx(sprintf("2 à %d", m$ordre), sprintf("2 to %d", m$ordre)))
  } else tx("pour chaque groupe", "for each group")
  p2 <- sprintf(tx("Pour chaque espèce, %s a été calculée %s : la racine du produit de sa spécificité A (la part de son abondance dans le groupe) et de sa fidélité B (la part des observations du groupe où elle est présente) ; chaque espèce a été rattachée au groupe, ou à la combinaison, de sa plus grande valeur (Dufrêne et Legendre, 1997).",
                   "For each species, %s was computed %s: the square root of the product of its specificity A (the share of its abundance in the group) and its fidelity B (the share of the group's observations where it is present); each species was assigned to the group, or combination, of its largest value (Dufrêne and Legendre, 1997)."),
                indice, combinaisons)
  p3 <- if (o$permutations > 0) {
    cites <- c(cites, if (identical(o$correction, "holm")) "holm" else "bh")
    groupes <- if (!is.null(o$strates)) sprintf(tx(" à l'intérieur de chaque %s", " within each %s"), nom_groupement(projet, o$strates)) else ""
    retenues <- if (r[["brutes"]] <= 1) tx("%d espèce a une valeur p au plus de 0,05, %d après correction.", if (r[["brutes"]] == 1) "%d species has a p-value of at most 0.05, %d after correction." else "%d species have a p-value of at most 0.05, %d after correction.")
                else tx("%d espèces ont une valeur p au plus de 0,05, %d après correction.", "%d species have a p-value of at most 0.05, %d after correction.")
    sprintf(paste(tx("Cette plus grande valeur a été testée par %d permutations des groupes des observations%s (graine %d) ; les valeurs p ont été corrigées sur les %d espèces testées par la méthode %s.",
                     "This largest value was tested with %d permutations of the observations' groups%s (seed %d); the p-values were corrected over the %d species tested by %s's method."), retenues),
            as.integer(o$permutations), groupes, as.integer(o$graine), sum(!is.na(m$p)),
            if (identical(o$correction, "holm")) tx("de Holm", "Holm") else tx("de Benjamini et Hochberg", "Benjamini and Hochberg"), r[["brutes"]], r[["corrigees"]])
  } else tx("Aucun test par permutation n'a été demandé.", "No permutation test was asked for.")
  p4 <- sprintf(tx("Les calculs ont été faits avec Nuée%s (Boutin, %s) et R %s (R Core Team, %s), comme le paquet indicspecies les fait (fonction multipatt ; De Cáceres et Legendre, 2009).",
                   "Computations were done with Nuée%s (Boutin, %s) and R %s (R Core Team, %s), as the indicspecies package does them (function multipatt; De Cáceres and Legendre, 2009)."),
                if (nzchar(version)) paste0(" ", version) else "", format(Sys.Date(), "%Y"), paste(R.version$major, R.version$minor, sep = "."), R.version$year)
  references <- c(vapply(unique(cites), function(k) REFERENCES_INDICATRICES[[k]], ""),
                  sprintf(REFERENCES$r, R.version$year), sprintf(REFERENCES$nuee, format(Sys.Date(), "%Y"), if (nzchar(version)) sprintf(" (version %s)", version) else ""))
  list(paragraphes = c(p1, p2, p3, p4), references = unname(sort(references)))
}
