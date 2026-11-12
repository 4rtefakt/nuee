# L'export vers R d'une RDA ou d'une analyse des correspondances (analyses.R :
# ecrire_script_<type> ; script.R pour les autres modèles) : un script qui refait l'analyse avec R
# seul, sans Nuée ni autre paquet. Ses données, ses variables comme le moteur les lit (onglet
# Variables : transformations, catégories), le calcul (celui de ordination.R, qui est celui de
# vegan), ses résultats et des graphiques en R de base ; ou un rapport R Markdown ou Quarto qui y
# ajoute le texte des méthodes et les références. Avant d'être écrit, son code est exécuté ici,
# sans le moteur, et ses résultats comparés à ceux du modèle de l'application : un écart est
# renvoyé dans `ecart`.
#
# Le code des fonctions est écrit en français ; ses commentaires « #fr » et « #en » sont gardés
# selon la langue du script, et ses noms traduits en anglais (NOMS_ORDINATION_EN, puis ceux de
# script.R) pour un script en anglais.

NOMS_ORDINATION_EN <- c(
  reponses_centrees = "centred_responses", reduire = "standardize", analyse_redondance = "redundancy_analysis",
  analyse_correspondances = "correspondence_analysis", totale = "total", conditionnelle = "conditional", contraint = "constrained",
  residuel = "unconstrained", valeurs_propres = "eigenvalues", fleches = "arrows", rang = "rank", rang_x = "rank_x", rang_z = "rank_z",
  rang_qz = "rank_qz", inertie = "inertia", garde = "kept", nuls = "zero", noms = "names", zcol = "zcol", residus = "residuals",
  r2_ajuste = "adjusted_r2", r2_rda = "rda_r2", ajuste = "adjusted", scores_rda = "rda_scores", echelle = "scaling",
  facteurs = "factors", etendre = "widen", centroides = "centroids", reponses = "responses", obs = "obs_scaling", s_rep = "resp_scaling",
  permuter = "permute_rows", strates = "strata", graine = "seed", groupes = "groups", base_orthonormee = "orthonormal_basis",
  stats_permutees = "permuted_stats", premier = "first", partielle = "partial", ip = "perm", Ep = "Ep", Qb = "Qb", ss = "ss",
  num = "num", den = "den", valeur_p = "p_value", test_rda = "rda_test", chi_z = "chi_model", chi_xz = "chi_residual",
  test_emboites = "nested_test", petit = "small", grand = "large", sans_x = "without_x", ddl = "df", res_g = "res_large",
  res_p = "res_small", rp = "df_small", rg = "df_large", num_p = "num_small", sg = "stats_large",
  tests_axes_rda = "axis_tests", tests_termes_rda = "term_tests", testes = "tested", Ybar = "Ybar", terme = "term", axe = "axis",
  contenu_dans = "contained_in", variables_groupe = "group_variables", selection_rda = "forward_selection", seuil = "threshold",
  dedans = "entered", courant = "current", precedent = "previous", candidats = "candidates", noyaux = "fits", etapes = "steps",
  retenu = "kept_term", retenus = "kept_terms", arret = "stop", tous = "all_terms", r2s = "r2s", noyau_de = "fit_of", r2_de = "r2_of",
  etape = "step", partition_rda = "variation_partitioning", blocs = "blocks", ensembles = "sets", ensemble = "set", fractions = "fractions",
  fraction = "fraction", sens = "meaning", sachant = "given", inter = "shared", masses_lignes = "row_masses",
  masses_colonnes = "column_masses", scores_ca = "ca_scores", lignes_ca = "row_scaling", colonnes_ca = "column_scaling",
  contributions_ca = "ca_contributions", contribution = "contribution", cos2 = "cos2", principales = "principal",
  colonnes = "columns", complet = "complete_rows", echelle_1 = "scaling_1", echelle_2 = "scaling_2", echelle_3 = "scaling_3",
  partie = "part", part = "share", cumul = "cumulative", valeur_propre = "eigenvalue", indicatrices = "indicators",
  permutations = "permutations", test = "test", tests_axes = "axis_test_table", tests_termes = "term_test_table",
  selection = "selection", partition = "partitioning", somme = "row_sums", longueur = "row_lengths", contributions = "contributions",
  multiplicateur = "multiplier", limites = "limits", couleurs = "colours", axes_graphe = "plot_axes", etiquette_axe = "axis_label",
  fleches_x = "x_arrows", ctr = "ctr", masse = "mass")

# Les commentaires de la langue du script (« #fr » ou « #en » en tête de ligne), et les noms du
# code en anglais pour un script en anglais.
traduire_ordination <- function(lignes, langue) {
  lignes <- unlist(lapply(lignes, function(l) if (grepl("\n", l, fixed = TRUE)) strsplit(l, "\n", fixed = TRUE)[[1]] else l))
  if (!length(lignes)) return(lignes)
  autre <- if (identical(langue, "en")) "fr" else "en"
  lignes <- lignes[!grepl(sprintf("^\\s*#%s( |$)", autre), lignes)]
  lignes <- sub(sprintf("^(\\s*)#%s( |$)", if (identical(langue, "en")) "en" else "fr"), "\\1#\\2", lignes)
  if (!identical(langue, "en")) return(lignes)
  noms <- c(NOMS_ORDINATION_EN, NOMS_SCRIPT_EN)
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"), includeText = TRUE)
  pd <- pd[pd$terminal & pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB", "SYMBOL_FORMALS") & pd$text %in% names(noms), ]
  pd <- pd[order(pd$line1, -pd$col1), ]
  for (i in seq_len(nrow(pd))) {
    l <- lignes[pd$line1[i]]
    lignes[pd$line1[i]] <- paste0(substr(l, 1, pd$col1[i] - 1), noms[[pd$text[i]]], substr(l, pd$col2[i] + 1, nchar(l)))
  }
  lignes
}
nom_ordination <- function(x, langue) if (identical(langue, "en") && x %in% names(NOMS_ORDINATION_EN)) NOMS_ORDINATION_EN[[x]] else nom_script(x, langue)

# ---------------------------------------------------------------- les fonctions du script

CODE_RDA <- r"---(
#fr Les réponses prêtes pour la RDA : centrées (réduites si demandé : une réponse constante vaut
#fr alors 0), divisées par racine(n - 1) pour que leurs carrés fassent des variances.
#en The responses ready for the RDA: centred (standardized if asked: a constant response is then
#en 0), divided by sqrt(n - 1) so that their squares are variances.
reponses_centrees <- function(Y, reduire = FALSE) {
  Y <- scale(Y, center = TRUE, scale = reduire)
  if (reduire) Y[is.nan(Y)] <- 0
  Y <- Y / sqrt(nrow(Y) - 1)
  attr(Y, "scaled:center") <- attr(Y, "scaled:scale") <- NULL
  Y
}

#fr L'analyse de redondance, comme vegan::rda : la part des covariables Z (centrées) est retirée des
#fr réponses, puis ce qui reste est régressé sur [Z, X] (centrés). La décomposition en valeurs
#fr singulières des valeurs ajustées donne les axes contraints (valeurs propres ; scores u des
#fr observations, combinaisons linéaires des X, et wa, sommes pondérées des réponses ; scores v des
#fr réponses ; flèches des X, leurs corrélations aux axes) ; celle des résidus, les axes non contraints.
#en The redundancy analysis, as vegan::rda: the part of the covariates Z (centred) is removed from the
#en responses, then what remains is regressed on [Z, X] (centred). The singular value decomposition of
#en the fitted values gives the constrained axes (eigenvalues; scores u of the observations, linear
#en combinations of the X, and wa, weighted sums of the responses; scores v of the responses; arrows
#en of the X, their correlations with the axes); that of the residuals, the unconstrained axes.
analyse_redondance <- function(Y, X, Z = NULL) {
  ZERO <- sqrt(.Machine$double.eps)
  n <- nrow(Y)
  totale <- sum(Y^2)
  E <- Y
  QZ <- NULL
  conditionnelle <- 0
  rang_qz <- 0
  zcol <- if (is.null(Z)) 0 else ncol(Z)
  if (zcol) {
    QZ <- qr(scale(Z, center = TRUE, scale = FALSE))
    rang_qz <- QZ$rank
    if (QZ$rank > 0) {
      conditionnelle <- sum(qr.fitted(QZ, Y)^2)
      if (conditionnelle < ZERO) conditionnelle <- 0
      E <- qr.resid(QZ, Y)
    }
    if (conditionnelle == 0) QZ <- NULL
  }
  XZ <- scale(cbind(if (zcol) Z, X), center = TRUE, scale = FALSE)
  Q <- qr(XZ)
  rang_x <- if (ncol(XZ)) sum(Q$pivot[seq_len(Q$rank)] > zcol) else 0
  contraint <- NULL
  residus <- E
  if (rang_x > 0) {
    garde <- seq_along(Q$pivot) <= Q$rank & Q$pivot > zcol
    s <- svd(qr.fitted(Q, E))
    lambda <- s$d^2
    nuls <- abs(lambda) < max(ZERO, ZERO * lambda[1])
    if (any(!nuls)) {
      lambda <- lambda[!nuls]
      noms <- sprintf("RDA%d", seq_along(lambda))
      u <- s$u[, !nuls, drop = FALSE]
      v <- s$v[, !nuls, drop = FALSE]
      wa <- E %*% v %*% diag(1 / sqrt(lambda), length(lambda))
      xx <- XZ[, Q$pivot[garde], drop = FALSE]
      fleches <- (1 / sqrt(colSums(xx^2))) * crossprod(xx, u)
      dimnames(u) <- dimnames(wa) <- list(rownames(Y), noms)
      dimnames(v) <- list(colnames(Y), noms)
      dimnames(fleches) <- list(colnames(xx), noms)
      contraint <- list(valeurs_propres = setNames(lambda, noms), u = u, v = v, wa = wa, fleches = fleches,
                        rang = length(lambda), rang_x = rang_x, inertie = sum(lambda))
      residus <- qr.resid(Q, E)
    }
  }
  s <- svd(residus)
  lambda <- s$d^2
  nuls <- if (length(lambda)) abs(lambda) < max(ZERO, ZERO * lambda[1]) else logical(0)
  lambda <- lambda[!nuls]
  noms <- sprintf("PC%d", seq_along(lambda))
  u <- s$u[, !nuls, drop = FALSE]
  v <- s$v[, !nuls, drop = FALSE]
  dimnames(u) <- list(rownames(Y), noms)
  dimnames(v) <- list(colnames(Y), noms)
  list(n = n, totale = totale, conditionnelle = conditionnelle, contraint = contraint,
       residuel = list(valeurs_propres = setNames(lambda, noms), u = u, v = v, rang = length(lambda), inertie = sum(lambda)),
       E = E, Q = Q, QZ = QZ, rang_z = if (is.null(QZ)) 0 else QZ$rank, rang_qz = rang_qz,
       Xc = XZ[, zcol + seq_len(ncol(XZ) - zcol), drop = FALSE], Zc = if (!is.null(QZ)) XZ[, seq_len(zcol), drop = FALSE])
}

#fr R² et R² ajusté (Ezekiel ; vegan::RsquareAdj) ; d'une RDA partielle, celui des X au-delà des Z
#fr (Peres-Neto et al. 2006).
#en R² and adjusted R² (Ezekiel; vegan::RsquareAdj); of a partial RDA, that of the X beyond the Z
#en (Peres-Neto et al. 2006).
r2_ajuste <- function(R2, n, m) if (m >= n - 1) NA else 1 - (1 - R2) * (n - 1) / (n - m - 1)
r2_rda <- function(m) {
  inertie <- if (is.null(m$contraint)) 0 else m$contraint$inertie
  q <- if (is.null(m$contraint)) 0 else m$contraint$rang_x
  R2 <- inertie / m$totale
  if (m$rang_z == 0) return(list(R2 = R2, ajuste = r2_ajuste(R2, m$n, q)))
  R2z <- m$conditionnelle / m$totale
  list(R2 = R2, ajuste = r2_ajuste(R2 + R2z, m$n, q + m$rang_z) - r2_ajuste(R2z, m$n, m$rang_z))
}

#fr Les scores dans une échelle (vegan::scores) : 1, les distances entre observations ; 2, les
#fr corrélations entre réponses. Axes contraints puis non contraints ; les flèches des X et les
#fr centroïdes des catégories des `facteurs` (la moyenne de leurs scores u) sont nuls sur les seconds.
#en The scores in a scaling (vegan::scores): 1, distances between observations; 2, correlations
#en between responses. Constrained axes, then unconstrained; the arrows of the X and the centroids of
#en the categories of `facteurs` (the mean of their scores u) are zero on the latter.
scores_rda <- function(m, echelle = 2, facteurs = list()) {
  cc <- m$contraint
  rc <- m$residuel
  lambda <- c(cc$valeurs_propres, rc$valeurs_propres)
  slam <- sqrt(lambda / m$totale)
  const <- sqrt(sqrt((m$n - 1) * m$totale))
  obs <- if (echelle == 1) slam else rep(1, length(lambda))
  s_rep <- if (echelle == 1) rep(1, length(lambda)) else slam
  etendre <- function(M) {
    sortie <- matrix(0, nrow(M), length(lambda), dimnames = list(rownames(M), names(lambda)))
    sortie[, seq_len(ncol(M))] <- M
    sortie
  }
  centroides <- if (length(facteurs)) do.call(rbind, lapply(names(facteurs), function(nom) {
    f <- facteurs[[nom]]
    M <- do.call(rbind, lapply(levels(f), function(k) colMeans(cc$u[f == k, , drop = FALSE])))
    rownames(M) <- paste0(nom, "(", levels(f), ")")
    M
  }))
  list(sites = const * sweep(cbind(cc$wa, rc$u), 2, obs, "*"), lc = const * sweep(cbind(cc$u, rc$u), 2, obs, "*"),
       reponses = const * sweep(cbind(cc$v, rc$v), 2, s_rep, "*"), fleches = sweep(etendre(cc$fleches), 2, obs, "*"),
       centroides = if (!is.null(centroides)) const * sweep(etendre(centroides), 2, obs, "*"))
}
)---"

CODE_TEST <- r"---(
#fr Les permutations des observations : libres, ou à l'intérieur des groupes de `strates` (pris dans
#fr l'ordre où ils apparaissent) ; reproductibles par leur graine.
#en The permutations of the observations: free, or within the groups of `strates` (taken in the order
#en they appear); reproducible from their seed.
permuter <- function(n, B, strates = NULL, graine = 1) {
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  groupes <- if (!is.null(strates)) unname(split(seq_len(n), match(strates, unique(strates))))
  P <- matrix(0L, B, n)
  for (b in seq_len(B)) {
    if (is.null(groupes)) {
      P[b, ] <- sample.int(n)
    } else {
      p <- seq_len(n)
      for (g in groupes) if (length(g) > 1) p[g] <- g[sample.int(length(g))]
      P[b, ] <- p
    }
  }
  P
}

#fr Pour chaque permutation : les réponses sans les covariables permutées (et, dans une RDA partielle,
#fr les covariables avec elles : les X sont rendus orthogonaux aux covariables permutées), ce qu'en
#fr expliquent les X (ou la première valeur propre, `premier`) et le reste (vegan::permutest).
#en For each permutation: the responses without the covariates, permuted (and, in a partial RDA, the
#en covariates with them: the X are made orthogonal to the permuted covariates), what the X explain of
#en them (or the first eigenvalue, `premier`) and the rest (vegan::permutest).
base_orthonormee <- function(Q) qr.Q(Q)[, seq_len(Q$rank), drop = FALSE]
stats_permutees <- function(m, P, premier = FALSE) {
  num <- den <- numeric(nrow(P))
  partielle <- !is.null(m$QZ)
  Q1 <- if (!partielle) base_orthonormee(m$Q)
  for (b in seq_len(nrow(P))) {
    ip <- P[b, ]
    Ep <- m$E[ip, , drop = FALSE]
    Qb <- if (partielle) base_orthonormee(qr(qr.resid(qr(m$Zc[ip, , drop = FALSE]), m$Xc))) else Q1
    M <- crossprod(Qb, Ep)
    ss <- sum(M^2)
    num[b] <- if (!premier) ss else if (length(M)) svd(M, 0, 0)$d[1]^2 else 0
    den[b] <- sum(Ep^2) - ss
  }
  list(num = num, den = den)
}
valeur_p <- function(Fp, F0) (sum(Fp >= F0 - sqrt(.Machine$double.eps)) + 1) / (length(Fp) + 1)

#fr Le test de la RDA (ou de son premier axe, `premier`) : pseudo-F et p par permutation.
#en The test of the RDA (or of its first axis, `premier`): pseudo-F and permutation p.
test_rda <- function(m, P, premier = FALSE) {
  q <- if (premier) 1 else m$contraint$rang_x
  r <- m$n - m$Q$rank - 1
  chi_z <- if (premier) unname(m$contraint$valeurs_propres[1]) else m$contraint$inertie
  chi_xz <- m$residuel$inertie
  F0 <- (chi_z / q) / (chi_xz / r)
  s <- stats_permutees(m, P, premier)
  c(F = F0, ddl_modele = q, ddl_residus = r, p = valeur_p((s$num / q) / (s$den / r), F0))
}
)---"

CODE_EMBOITES <- r"---(
#fr Deux RDA emboîtées (les X de la petite parmi ceux de la grande, mêmes covariables) : ce que la
#fr grande explique de plus, testé par les mêmes permutations (vegan::anova(petit, grand)).
#en Two nested RDAs (the X of the small one among those of the large one, same covariates): what the
#en large one explains beyond the small one, tested with the same permutations (vegan::anova(small, large)).
test_emboites <- function(petit, grand, P, sg = stats_permutees(grand, P)) {
  rg <- grand$n - grand$Q$rank - 1
  res_g <- grand$residuel$inertie
  sans_x <- is.null(petit$contraint)
  rp <- if (sans_x) petit$n - petit$rang_qz - 1 else petit$n - petit$Q$rank - 1
  res_p <- if (sans_x) petit$totale - petit$conditionnelle else petit$residuel$inertie
  ddl <- rp - rg
  if (ddl <= 0) return(c(ddl = 0, inertie = 0, F = NA, p = NA))
  F0 <- ((res_p - res_g) / ddl) / (res_g / rg)
  num_p <- if (sans_x) rep(0, nrow(P)) else stats_permutees(petit, P)$num
  c(ddl = ddl, inertie = res_p - res_g, F = F0, p = valeur_p(((sg$num - num_p) / ddl) / (sg$den / rg), F0))
}

#fr Un terme qu'un terme croisé contient (A dans A*B ou A*A) n'est pas testé seul, et n'entre qu'avant lui.
#en A term contained in a cross term (A in A*B or A*A) isn't tested alone, and enters only before it.
variables_groupe <- function(g) strsplit(g, "*", fixed = TRUE)[[1]]
contenu_dans <- function(a, b) {
  va <- table(variables_groupe(a))
  vb <- table(variables_groupe(b))
  length(variables_groupe(b)) > length(variables_groupe(a)) && all(names(va) %in% names(vb)) && all(va <= vb[names(va)])
}
)---"

CODE_TESTS <- r"---(
#fr Les axes contraints l'un après l'autre (vegan::anova(by = "axis")) : l'axe k, premier axe du modèle
#fr où les k - 1 précédents sont retirés des réponses ; p ne décroît pas d'un axe au suivant.
#en The constrained axes one after another (vegan::anova(by = "axis")): axis k, first axis of the model
#en where the k - 1 previous ones are removed from the responses; p doesn't decrease from one axis to the next.
tests_axes_rda <- function(m, Ybar, Z, P) {
  lambda <- m$contraint$valeurs_propres
  u <- m$contraint$u
  ddl <- m$n - length(lambda) - m$rang_z - 1
  Fs <- unname(lambda) / m$residuel$inertie * ddl
  ps <- rep(NA_real_, length(lambda))
  for (i in seq_along(lambda)) {
    mi <- if (i == 1) m else analyse_redondance(qr.resid(qr(u[, seq_len(i - 1), drop = FALSE]), Ybar), u[, -seq_len(i - 1), drop = FALSE], Z)
    tst <- test_rda(mi, P, premier = i < length(lambda))
    Fs[i] <- tst[["F"]]
    ps[i] <- if (i > 1) max(tst[["p"]], ps[i - 1]) else tst[["p"]]
    if (ps[i] >= 1) break
  }
  data.frame(axe = c(names(lambda), "Residual"), ddl = c(rep(1, length(lambda)), ddl), inertie = c(unname(lambda), m$residuel$inertie),
             F = c(Fs, NA), p = c(ps, NA))
}

#fr Chaque terme après tous les autres (vegan::anova(by = "margin")) : le modèle sans ses colonnes de X.
#en Each term after all the others (vegan::anova(by = "margin")): the model without its columns of X.
tests_termes_rda <- function(m, Ybar, X, Z, groupes, P) {
  testes <- Filter(function(g) !any(vapply(unique(groupes), function(h) contenu_dans(g, h), TRUE)), unique(groupes))
  sg <- stats_permutees(m, P)
  res <- t(vapply(testes, function(g) test_emboites(analyse_redondance(Ybar, X[, groupes != g, drop = FALSE], Z), m, P, sg), numeric(4)))
  #fr (les colonnes de res : degrés de liberté, inertie, F, p)
  #en (the columns of res: degrees of freedom, inertia, F, p)
  data.frame(terme = c(testes, "Residual"), ddl = c(res[, 1], m$n - m$Q$rank - 1), inertie = c(res[, 2], m$residuel$inertie),
             F = c(res[, 3], NA), p = c(res[, 4], NA))
}
)---"

CODE_SELECTION <- r"---(
#fr La sélection pas à pas (vegan::ordiR2step ; Blanchet et al. 2008) : le terme au plus grand R²
#fr ajusté entre s'il l'augmente sans dépasser celui de tous les termes, et si son test (modèle avec
#fr lui contre modèle sans lui) a p <= `seuil`.
#en Forward selection (vegan::ordiR2step; Blanchet et al. 2008): the term with the largest adjusted R²
#en enters if it increases it without going beyond that of all the terms, and if its test (model with
#en it against model without it) has p <= `seuil`.
selection_rda <- function(Ybar, X, Z, groupes, P, tous, seuil = 0.05) {
  noyau_de <- function(gs) analyse_redondance(Ybar, X[, groupes %in% gs, drop = FALSE], Z)
  r2_de <- function(m) { v <- r2_rda(m)$ajuste; if (is.na(v)) 0 else v }
  dedans <- character(0)
  courant <- noyau_de(dedans)
  precedent <- 0
  etapes <- NULL
  arret <- "tous"
  etape <- function(terme, r2, tst, retenu) etapes <<- rbind(etapes, data.frame(terme = terme, r2_ajuste = r2, F = tst[["F"]], p = tst[["p"]], retenu = retenu))
  repeat {
    candidats <- Filter(function(g) !g %in% dedans && all(vapply(unique(groupes), function(h) !contenu_dans(h, g) || h %in% dedans, TRUE)), unique(groupes))
    if (!length(candidats)) break
    noyaux <- lapply(candidats, function(g) noyau_de(c(dedans, g)))
    r2s <- vapply(noyaux, r2_de, 0)
    i <- which.max(r2s)
    if (!(r2s[i] > precedent)) { arret <- "r2"; break }
    if (r2s[i] > tous) { etape(candidats[i], r2s[i], c(F = NA, p = NA), FALSE); arret <- "r2_tous"; break }
    tst <- test_emboites(courant, noyaux[[i]], P)
    if (is.na(tst[["p"]]) || tst[["p"]] > seuil) { etape(candidats[i], r2s[i], tst, FALSE); arret <- "p"; break }
    dedans <- c(dedans, candidats[i])
    courant <- noyaux[[i]]
    precedent <- r2s[i]
    etape(candidats[i], r2s[i], tst, TRUE)
  }
  list(etapes = etapes, retenus = dedans, arret = arret)
}
)---"

CODE_PARTITION <- r"---(
#fr La partition de la variance entre deux ou trois ensembles de X, leurs `blocs` (vegan::varpart ;
#fr Peres-Neto et al. 2006) : R² ajustés des ensembles et de leurs réunions, puis les fractions.
#en Variation partitioning between two or three sets of X, their `blocs` (vegan::varpart; Peres-Neto
#en et al. 2006): adjusted R² of the sets and of their unions, then the fractions.
partition_rda <- function(Ybar, X, blocs) {
  e <- unique(blocs)
  r2 <- function(es) {
    Xs <- scale(X[, blocs %in% es, drop = FALSE], center = TRUE, scale = TRUE)
    Xs[!is.finite(Xs)] <- 0
    Q <- qr(Xs, tol = 1e-6)
    R2 <- sum(qr.fitted(Q, Ybar)^2) / sum(Ybar^2)
    data.frame(ensemble = paste(es, collapse = " + "), ddl = Q$rank, R2 = R2, r2_ajuste = r2_ajuste(R2, nrow(Ybar), Q$rank))
  }
  if (length(e) == 2) {
    ensembles <- rbind(r2(e[1]), r2(e[2]), r2(e))
    R <- ensembles$r2_ajuste
    commun <- R[1] + R[2] - R[3]
    fractions <- data.frame(fraction = c("[a]", "[b]", "[c]", "[d]"), r2_ajuste = c(R[1] - commun, R[2] - commun, commun, 1 - R[3]))
  } else {
    ensembles <- rbind(r2(e[1]), r2(e[2]), r2(e[3]), r2(e[1:2]), r2(e[c(1, 3)]), r2(e[2:3]), r2(e))
    R <- ensembles$r2_ajuste
    a <- R[7] - R[6]; b <- R[7] - R[5]; c <- R[7] - R[4]
    d <- R[5] - R[3] - a; e_ <- R[4] - R[1] - b; f <- R[6] - R[2] - c
    fractions <- data.frame(fraction = c("[a]", "[b]", "[c]", "[d]", "[e]", "[f]", "[g]", "[h]"),
                            r2_ajuste = c(a, b, c, d, e_, f, R[1] - a - d - f, 1 - R[7]))
  }
  list(ensembles = ensembles, fractions = fractions)
}
)---"

CODE_CA <- r"---(
#fr L'analyse des correspondances, comme vegan::cca : les écarts du tableau à l'indépendance,
#fr pondérés, décomposés en valeurs singulières ; u et v, les coordonnées standard des lignes et
#fr des colonnes.
#en The correspondence analysis, as vegan::cca: the departures of the table from independence,
#en weighted, decomposed in singular values; u and v, the standard coordinates of rows and columns.
analyse_correspondances <- function(N) {
  ZERO <- sqrt(.Machine$double.eps)
  P <- N / sum(N)
  rw <- rowSums(P)
  cw <- colSums(P)
  rc <- outer(rw, cw)
  S <- (P - rc) / sqrt(rc)
  s <- svd(S)
  lambda <- s$d^2
  nuls <- abs(lambda) < max(ZERO, ZERO * lambda[1])
  lambda <- lambda[!nuls]
  noms <- sprintf("CA%d", seq_along(lambda))
  u <- sweep(s$u[, !nuls, drop = FALSE], 1, sqrt(rw), "/")
  v <- sweep(s$v[, !nuls, drop = FALSE], 1, sqrt(cw), "/")
  dimnames(u) <- list(rownames(N), noms)
  dimnames(v) <- list(colnames(N), noms)
  list(total = sum(N), masses_lignes = rw, masses_colonnes = cw, totale = sum(S^2), valeurs_propres = setNames(lambda, noms), u = u, v = v)
}

#fr Les scores dans une échelle (vegan::scores) : 1, lignes × racine(λ) (distances du khi² entre
#fr observations) ; 2, colonnes × racine(λ) ; 3, les unes et les autres × racine(racine(λ)).
#en The scores in a scaling (vegan::scores): 1, rows × sqrt(λ) (chi-square distances between
#en observations); 2, columns × sqrt(λ); 3, both × sqrt(sqrt(λ)).
scores_ca <- function(m, echelle = 2) {
  slam <- sqrt(m$valeurs_propres)
  lignes_ca <- switch(echelle, slam, rep(1, length(slam)), sqrt(slam))
  colonnes_ca <- switch(echelle, rep(1, length(slam)), slam, sqrt(slam))
  list(lignes = sweep(m$u, 2, lignes_ca, "*"), colonnes = sweep(m$v, 2, colonnes_ca, "*"))
}

#fr Contributions (%) des lignes et des colonnes à chaque axe et qualité de leur représentation
#fr (cos²), d'après leurs coordonnées principales.
#en Contributions (%) of the rows and columns to each axis and quality of their representation
#en (cos²), from their principal coordinates.
contributions_ca <- function(m) {
  part <- function(coord, masse) {
    principales <- sweep(coord, 2, sqrt(m$valeurs_propres), "*")
    list(contribution = 100 * sweep(principales^2 * masse, 2, m$valeurs_propres, "/"), cos2 = principales^2 / rowSums(principales^2))
  }
  list(lignes = part(m$u, m$masses_lignes), colonnes = part(m$v, m$masses_colonnes))
}
)---"

# Le code des fonctions dont le script a besoin.
code_fonctions_ordination <- function(m) {
  morceaux <- if (identical(m$type, "CA")) CODE_CA else c(CODE_RDA, if (m$options$permutations > 0) CODE_TEST,
                                                           if (!is.null(m$tests) || !is.null(m$selection)) CODE_EMBOITES,
                                                           if (!is.null(m$tests)) CODE_TESTS, if (!is.null(m$selection)) CODE_SELECTION,
                                                           if (!is.null(m$partition)) CODE_PARTITION)
  lignes <- unlist(lapply(morceaux, function(x) sub("^\n", "", x)))
  unlist(lapply(lignes, function(l) c(strsplit(l, "\n", fixed = TRUE)[[1]], "")))
}

# ---------------------------------------------------------------- le calcul de ce modèle

# Le code qui lit les termes d'un bloc dans le tableau du script (script.R : code_termes), tels que
# le moteur les lit pour une ordination (sans mise à l'échelle), et ses termes croisés (le
# produit de leurs facteurs, X seulement). `nom` : le nom de la matrice ; `explication` remplace
# celle de code_termes.
code_bloc_ordination <- function(projet, termes, obs, tab, nom, langue, explication = NULL) {
  tx <- function(fr, en) if (langue == "en") en else fr
  bruts <- lapply(termes, function(t) { t$scaling <- list(type = "none"); t })
  plan <- plan_bloc(projet, bruts, construire_bloc(projet, bruts, obs), tab, obs, nom)$termes
  pl <- list(colonnes = names(tab$donnees), plsda = FALSE)
  code <- code_termes(plan, nom, pl, tx)
  if (!is.null(explication)) code <- c(commentaire(explication), code[!startsWith(code, "#")])
  croises <- Filter(function(t) t$croise, plan)
  if (length(croises)) {
    code <- c(code, commentaire(tx("Les termes croisés : le produit de leurs facteurs.", "The cross terms: the product of their factors.")),
              sprintf("%s <- cbind(%s, %s)", nom, nom, paste(vapply(croises, function(t) sprintf("%s = %s", code_texte(t$nom),
                paste(vapply(t$facteurs, function(f) sprintf("%s[, %s]", nom, code_texte(f$nom)), ""), collapse = " * ")), ""), collapse = ", ")))
  }
  code
}

# La partie « modèle » du script d'une RDA ou d'une CA (dans la langue du script, ses noms en
# français : traduire_ordination les traduit).
code_calcul_ordination <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  ca <- identical(m$type, "CA")
  obs <- as.numeric(d$observations)
  rangs <- unname(tab$rangs[as.character(obs)])
  lignes_obs <- if (identical(as.integer(rangs), seq_len(nrow(tab$donnees)))) {
    c(commentaire(tx(sprintf("Les %d observations du tableau.", length(obs)), sprintf("The %d observations of the table.", length(obs)))), "d <- donnees")
  } else {
    c(commentaire(tx(sprintf("Les %d observations du modèle (leurs rangs dans le tableau).", length(obs)), sprintf("The %d observations of the model (their rows in the table).", length(obs)))),
      code_valeurs(morceaux_rangs(rangs), "lignes <- "), "d <- donnees[lignes, ]")
  }
  n_exclues <- length(m$exclues)
  noms_exclues <- if (n_exclues && n_exclues <= 8) sprintf(" (%s)", liste_textes(texte_sur(unname(projet$noms_obs[as.character(m$exclues)])), tx("et", "and"))) else ""
  profil <- ca || m$options$transformation %in% c("hellinger", "corde")
  exclusion <- if (n_exclues) commentaire(tx(sprintf("Ici, %d observation%s%s : une valeur y manque%s.", n_exclues, if (n_exclues > 1) "s" else "", noms_exclues,
                                                     if (profil) ", ou elle n'a aucune abondance" else ""),
                                                 sprintf("Here, %d observation%s%s: a value is missing%s.", n_exclues, if (n_exclues > 1) "s" else "", noms_exclues,
                                                     if (profil) ", or it has no abundance at all" else "")))
  if (ca) {
    return(c(lignes_obs, "",
             code_bloc_ordination(projet, d$x, obs, tab, "X", langue, tx("Le tableau : une colonne par variable X (des effectifs ou des abondances).", "The table: a column per X variable (counts or abundances).")), "",
             commentaire(tx("Les observations sans valeur manquante et avec au moins une abondance ; les variables avec au moins une abondance.",
                            "The observations without a missing value and with at least one abundance; the variables with at least one abundance.")),
             exclusion,
             "complet <- complete.cases(X) & rowSums(X) > 0",
             "X <- X[complet, , drop = FALSE]",
             "X <- X[, colSums(X) > 0, drop = FALSE]",
             "m <- analyse_correspondances(X)",
             "echelle_1 <- scores_ca(m, 1)",
             "echelle_2 <- scores_ca(m, 2)",
             "echelle_3 <- scores_ca(m, 3)",
             "contributions <- contributions_ca(m)",
             "valeurs_propres <- data.frame(axe = names(m$valeurs_propres), valeur_propre = unname(m$valeurs_propres),",
             "                              part = unname(m$valeurs_propres) / m$totale, cumul = cumsum(unname(m$valeurs_propres)) / m$totale)"))
  }
  o <- m$options
  z <- termes_variables(projet, o$z, o$z_transformations)
  strates <- if (!is.null(o$strates)) {
    col <- if (identical(as.character(o$strates$by)[1], "id")) tab$ids[[as.character(o$strates$id)[1]]] else tab$noms[[as.character(o$strates$variable)[1]]]
    c(commentaire(tx(sprintf("Les groupes des permutations : « %s » (les observations ne sont permutées qu'à l'intérieur de chacun).", texte_sur(col)),
                     sprintf("The groups of the permutations: “%s” (the observations are only permuted within each).", texte_sur(col)))),
      sprintf("strates <- d[[%s]]", code_texte(col)))
  }
  transformation <- switch(o$transformation,
    hellinger = c(commentaire(tx("Hellinger : la racine des abondances relatives de chaque observation (Legendre et Gallagher 2001) ; une observation sans aucune abondance n'a pas de profil.",
                                 "Hellinger: the square root of the relative abundances of each observation (Legendre and Gallagher 2001); an observation without any abundance has no profile.")),
                  "somme <- rowSums(Y)", "Y <- sqrt(Y / ifelse(somme > 0, somme, NA))"),
    corde = c(commentaire(tx("Corde : chaque observation ramenée à une longueur 1 (Legendre et Gallagher 2001) ; une observation sans aucune abondance n'a pas de profil.",
                             "Chord: each observation scaled to unit length (Legendre and Gallagher 2001); an observation without any abundance has no profile.")),
              "longueur <- sqrt(rowSums(Y^2))", "Y <- Y / ifelse(longueur > 0, longueur, NA)"),
    log1p = c(commentaire(tx("Les réponses en log(x + 1).", "The responses as log(x + 1).")), "Y <- log1p(Y)"),
    NULL)
  avec_z <- length(z) > 0
  avec_s <- !is.null(o$strates)
  facteurs <- unique(unlist(lapply(d$x, function(t) if (length(t$variables) == 1 && categorie(t) != 0) as.character(t$variables[1]))))
  code_facteurs <- if (length(facteurs)) {
    sprintf("facteurs <- list(%s)", paste(vapply(facteurs, function(u) sprintf("%s = droplevels(d[[%s]][complet])", deparse(as.name(as.character(variable_du_projet(projet, u)$ids[1])), backtick = TRUE),
                                                                       code_texte(tab$noms[[u]])), ""), collapse = ", "))
  }
  arg_f <- if (length(facteurs)) ", facteurs" else ""
  B <- o$permutations
  c(lignes_obs, "",
    code_bloc_ordination(projet, d$y, obs, tab, "Y", langue), "",
    code_bloc_ordination(projet, d$x, obs, tab, "X", langue), "",
    if (avec_z) c(code_bloc_ordination(projet, z, obs, tab, "Z", langue, tx("Les covariables Z : leur effet est retiré d'abord (RDA partielle).", "The covariates Z: their effect is removed first (partial RDA).")), ""),
    if (avec_s) c(strates, ""),
    transformation,
    commentaire(tx("Les observations où une valeur manque sont laissées de côté.", "The observations where a value is missing are left out.")),
    exclusion,
    sprintf("complet <- complete.cases(Y, X%s%s)", if (avec_z) ", Z" else "", if (avec_s) ", strates" else ""),
    code_facteurs,
    "Y <- Y[complet, , drop = FALSE]",
    "X <- X[complet, , drop = FALSE]",
    if (avec_z) "Z <- Z[complet, , drop = FALSE]",
    if (avec_s) "strates <- strates[complet]",
    "n <- nrow(Y)",
    sprintf("Ybar <- reponses_centrees(Y, reduire = %s)", if (o$reduire) "TRUE" else "FALSE"),
    "",
    commentaire(tx("La RDA, son R² et ses scores dans les échelles 1 et 2.", "The RDA, its R² and its scores in scalings 1 and 2.")),
    sprintf("m <- analyse_redondance(Ybar, X%s)", if (avec_z) ", Z" else ""),
    "r2 <- r2_rda(m)",
    sprintf("echelle_1 <- scores_rda(m, 1%s)", arg_f),
    sprintf("echelle_2 <- scores_rda(m, 2%s)", arg_f),
    if (any(m$X$indicatrices)) c(commentaire(tx("(une catégorie d'une variable qualitative a son centroïde, pas de flèche)", "(a category of a qualitative variable has its centroid, no arrow)")),
                                 code_valeurs(vapply(m$X$noms[m$X$indicatrices], code_texte, "", USE.NAMES = FALSE), "indicatrices <- ")),
    sprintf("fleches_x <- echelle_2$fleches[%s, , drop = FALSE]", if (any(m$X$indicatrices)) "!rownames(echelle_2$fleches) %in% indicatrices" else ""),
    sprintf("inertie <- data.frame(partie = %s,", code_textes(tx(c("totale", "conditionnelle", "contrainte", "résiduelle"), c("total", "conditional", "constrained", "unconstrained")))),
    "                      inertie = c(m$totale, m$conditionnelle, m$contraint$inertie, m$residuel$inertie))",
    "inertie$part <- inertie$inertie / m$totale",
    "valeurs_propres <- c(m$contraint$valeurs_propres, m$residuel$valeurs_propres)",
    "valeurs_propres <- data.frame(axe = names(valeurs_propres), valeur_propre = unname(valeurs_propres), part = unname(valeurs_propres) / m$totale,",
    "                              cumul = cumsum(unname(valeurs_propres)) / m$totale, contraint = seq_along(valeurs_propres) <= m$contraint$rang)",
    if (B > 0) c("",
                 commentaire(tx(sprintf("Le test par %d permutations des observations (graine %d : les mêmes que Nuée).", B, o$graine),
                                sprintf("The test with %d permutations of the observations (seed %d: the same as Nuée's).", B, o$graine))),
                 sprintf("permutations <- permuter(n, %d%s, graine = %d)", B, if (avec_s) ", strates" else "", o$graine),
                 "test <- test_rda(m, permutations)"),
    if (!is.null(m$tests) || !is.null(m$selection)) c("", commentaire(tx("Le terme de chaque colonne de X (une variable, toutes ses catégories ensemble).", "The term of each column of X (a variable, all its categories together).")),
                                                     code_valeurs(vapply(m$X$groupes, code_texte, "", USE.NAMES = FALSE), "groupes <- ")),
    if (!is.null(m$tests)) c(commentaire(tx("Les tests par axe et par terme (quelques secondes).", "The tests by axis and by term (a few seconds).")),
                             sprintf("tests_axes <- tests_axes_rda(m, Ybar, %s, permutations)", if (avec_z) "Z" else "NULL"),
                             sprintf("tests_termes <- tests_termes_rda(m, Ybar, X, %s, groupes, permutations)", if (avec_z) "Z" else "NULL")),
    if (!is.null(m$selection)) c(commentaire(tx("La sélection pas à pas (quelques secondes).", "The forward selection (a few seconds).")),
                                 sprintf("selection <- selection_rda(Ybar, X, %s, groupes, permutations, tous = r2$ajuste, seuil = %s)", if (avec_z) "Z" else "NULL", texte_exact(m$selection$seuil))),
    if (!is.null(m$partition)) c("", commentaire(tx("Le bloc de chaque colonne de X, pour la partition de la variance.", "The block of each column of X, for variation partitioning.")),
                                 code_valeurs(vapply(ifelse(is.na(m$X$blocs), traduire("(sans bloc)", "(no block)"), m$X$blocs), code_texte, "", USE.NAMES = FALSE), "blocs <- "),
                                 "partition <- partition_rda(Ybar, X, blocs)"))
}

# Les résultats affichés.
code_sortie_ordination <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  ligne <- function(code, fr, en) {
    code <- traduire_ordination(code, langue)
    paste0(formatC(code, width = -max(34, nchar(code) + 2)), "# ", tx(fr, en))
  }
  if (identical(m$type, "CA")) {
    return(c(ligne("print(m$totale)", "inertie totale", "total inertia"),
             ligne("print(valeurs_propres)", "valeurs propres et parts de l'inertie", "eigenvalues and shares of the inertia"),
             ligne("print(head(echelle_2$lignes))", "scores des observations (échelle 2)", "scores of the observations (scaling 2)"),
             ligne("print(echelle_2$colonnes)", "scores des variables (échelle 2)", "scores of the variables (scaling 2)"),
             ligne("print(round(contributions$colonnes$contribution, 1))", "contributions des variables (%)", "contributions of the variables (%)"),
             ligne("print(round(contributions$colonnes$cos2, 3))", "qualité de leur représentation (cos²)", "quality of their representation (cos²)")))
  }
  c(ligne("print(inertie)", "inerties totale, conditionnelle, contrainte, résiduelle", "total, conditional, constrained, residual inertia"),
    ligne("print(valeurs_propres)", "valeurs propres et parts de l'inertie", "eigenvalues and shares of the inertia"),
    ligne("print(unlist(r2))", "R² et R² ajusté", "R² and adjusted R²"),
    if (m$options$permutations > 0) ligne("print(test)", "pseudo-F, degrés de liberté, p", "pseudo-F, degrees of freedom, p"),
    ligne("print(head(echelle_2$sites))", "scores des observations (échelle 2)", "scores of the observations (scaling 2)"),
    ligne("print(echelle_2$reponses)", "scores des réponses (échelle 2)", "scores of the responses (scaling 2)"),
    ligne("print(fleches_x)", "flèches des X quantitatifs (échelle 2)", "arrows of the quantitative X (scaling 2)"),
    if (length(m$facteurs)) ligne("print(echelle_2$centroides)", "centroïdes des catégories (échelle 2)", "centroids of the categories (scaling 2)"),
    if (!is.null(m$tests)) c(ligne("print(tests_axes)", "tests par axe", "tests by axis"), ligne("print(tests_termes)", "tests par terme", "tests by term")),
    if (!is.null(m$selection)) ligne("print(selection$etapes)", "sélection pas à pas", "forward selection"),
    if (!is.null(m$partition)) ligne("print(partition$fractions)", "partition de la variance (R² ajustés)", "variation partitioning (adjusted R²)"))
}

# Les graphiques en R de base : l'ordination (échelle 2 ; les flèches des X agrandies pour remplir
# le graphique, comme vegan les dessine) et les valeurs propres.
code_graphiques_ordination <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  t <- function(code) traduire_ordination(code, langue)
  part <- "etiquette_axe <- function(k) sprintf(\"%s (%.1f %%)\", valeurs_propres$axe[k], 100 * valeurs_propres$part[k])"
  if (identical(m$type, "CA")) {
    return(list(
      list(nom = "ordination", largeur = 7, hauteur = 6, code = t(c(part,
        "plot(rbind(echelle_2$lignes[, 1:2], echelle_2$colonnes[, 1:2]), type = \"n\", asp = 1, xlab = etiquette_axe(1), ylab = etiquette_axe(2),",
        sprintf("     main = %s)", code_texte(tx("Analyse des correspondances (échelle 2)", "Correspondence analysis (scaling 2)"))),
        "abline(h = 0, v = 0, col = \"grey\")",
        "points(echelle_2$lignes[, 1:2], pch = 19, col = \"grey40\")",
        "text(echelle_2$colonnes[, 1:2], labels = rownames(echelle_2$colonnes), col = \"#d1495b\", cex = 0.8)"))),
      list(nom = "valeurs-propres", largeur = 6, hauteur = 4, code = t(c(
        sprintf("barplot(100 * valeurs_propres$part, names.arg = valeurs_propres$axe, las = 2, ylab = %s, main = %s)",
                code_texte(tx("% de l'inertie", "% of the inertia")), code_texte(tx("Valeurs propres", "Eigenvalues"))))))))
  }
  if (m$noyau$contraint$rang + m$noyau$residuel$rang < 2) return(list())
  list(
    list(nom = "ordination", largeur = 7, hauteur = 6, code = t(c(part,
      "plot(rbind(echelle_2$sites[, 1:2], echelle_2$reponses[, 1:2]), type = \"n\", asp = 1, xlab = etiquette_axe(1), ylab = etiquette_axe(2),",
      sprintf("     main = %s)", code_texte(tx("RDA (échelle 2)", "RDA (scaling 2)"))),
      "abline(h = 0, v = 0, col = \"grey\")",
      "points(echelle_2$sites[, 1:2], pch = 19, col = \"grey40\")",
      "text(echelle_2$reponses[, 1:2], labels = rownames(echelle_2$reponses), col = \"#d1495b\", cex = 0.8)",
      "if (nrow(fleches_x)) {",
      "  limites <- par(\"usr\")",
      "  multiplicateur <- 0.75 * min(abs(limites[1:2]) / max(abs(fleches_x[, 1])), abs(limites[3:4]) / max(abs(fleches_x[, 2])))",
      "  arrows(0, 0, multiplicateur * fleches_x[, 1], multiplicateur * fleches_x[, 2], length = 0.08, col = \"#2f6fb5\")",
      "  text(1.08 * multiplicateur * fleches_x[, 1:2], labels = rownames(fleches_x), col = \"#2f6fb5\", cex = 0.85)",
      "}",
      if (length(m$facteurs)) c("points(echelle_2$centroides[, 1:2], pch = 17, col = \"#2f6fb5\")",
                                "text(echelle_2$centroides[, 1:2], labels = rownames(echelle_2$centroides), col = \"#2f6fb5\", cex = 0.8, pos = 3)")))),
    list(nom = "valeurs-propres", largeur = 6, hauteur = 4, code = t(c(
      sprintf("barplot(100 * valeurs_propres$part, names.arg = valeurs_propres$axe, las = 2, col = ifelse(valeurs_propres$contraint, \"#2f6fb5\", \"#9aa7b8\"), ylab = %s, main = %s)",
              code_texte(tx("% de l'inertie", "% of the inertia")), code_texte(tx("Valeurs propres (en bleu : axes contraints)", "Eigenvalues (in blue: constrained axes)")))))))
}

# ---------------------------------------------------------------- l'écart avec Nuée

# Les résultats du script (dans `e`) comparés à ceux de `m` : NULL, ou ce qui diffère.
ecart_ordination <- function(m, e, langue) {
  r <- function(x) get0(nom_ordination(x, langue), envir = e, inherits = FALSE)
  champ <- function(x, ...) { for (k in c(...)) x <- x[[nom_ordination(k, langue)]]; x }
  au_signe <- function(a, b) {  # (le signe d'un axe est arbitraire : celui de Nuée)
    if (is.null(a) || is.null(b) || !identical(dim(as.matrix(a)), dim(as.matrix(b)))) return(b)
    b <- as.matrix(b)
    for (j in seq_len(ncol(b))) if (sum(as.matrix(a)[, j] * b[, j]) < 0) b[, j] <- -b[, j]
    b
  }
  paires <- if (identical(m$type, "CA")) {
    s2 <- scores_ca(m, 2)
    sc <- r("echelle_2")
    ctr <- r("contributions")
    list("valeurs propres" = list(m$noyau$eig, champ(r("m"), "valeurs_propres")),
         lignes = list(s2$sites, au_signe(s2$sites, champ(sc, "lignes"))), colonnes = list(s2$reponses, au_signe(s2$reponses, champ(sc, "colonnes"))),
         contributions = list(100 * m$contributions$colonnes$contribution, champ(ctr, "colonnes", "contribution")))
  } else {
    k <- m$noyau
    s2 <- scores_rda(m, 2)
    sc <- r("echelle_2")
    rm <- r("m")
    r2 <- r("r2")
    test <- r("test")
    list("valeurs propres" = list(c(k$contraint$eig, k$residuel$eig), c(champ(rm, "contraint", "valeurs_propres"), champ(rm, "residuel", "valeurs_propres"))),
         R2 = list(c(m$r2, m$r2_ajuste), c(r2$R2, champ(r2, "ajuste"))),
         sites = list(s2$sites, au_signe(s2$sites, champ(sc, "sites"))), reponses = list(s2$reponses, au_signe(s2$reponses, champ(sc, "reponses"))),
         fleches = list(fleches_rda(m, s2), au_signe(fleches_rda(m, s2), r("fleches_x"))),
         test = if (m$options$permutations > 0) list(c(m$test$F, m$test$p), unname(test[c("F", "p")])),
         "tests par axe" = if (!is.null(m$tests)) list(as.matrix(m$tests$axes[c("F", "p")]), as.matrix(r("tests_axes")[c("F", "p")])),
         "tests par terme" = if (!is.null(m$tests)) list(as.matrix(m$tests$termes[c("F", "p")]), as.matrix(r("tests_termes")[c("F", "p")])),
         selection = if (!is.null(m$selection$etapes)) list(as.matrix(m$selection$etapes[c("r2_ajuste", "F", "p")]),
                                                            as.matrix(champ(r("selection"), "etapes")[c(nom_ordination("r2_ajuste", langue), "F", "p")])),
         partition = if (!is.null(m$partition)) list(m$partition$fractions$r2_ajuste, champ(r("partition"), "fractions", "r2_ajuste")))
  }
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# ---------------------------------------------------------------- écrire le script

ecrire_script_rda <- function(projet, d, chemin_script, m, donnees, renv, rdata, rproj, version, langue) {
  ecrire_script_ordination(projet, d, chemin_script, m, donnees, renv, rdata, rproj, version, langue)
}
ecrire_script_ca <- function(projet, d, chemin_script, m, donnees, renv, rdata, rproj, version, langue) {
  ecrire_script_ordination(projet, d, chemin_script, m, donnees, renv, rdata, rproj, version, langue)
}

ecrire_script_ordination <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (langue == "en") en else fr
  format <- c(r = "R", rmd = "Rmd", qmd = "qmd")[[tolower(tools::file_ext(chemin_script))]]
  ca <- identical(m$type, "CA")
  st <- if (!ca) m$options$strates
  tab <- tableau_script(projet, if (!is.null(st) && identical(as.character(st$by)[1], "id")) as.character(st$id)[1] else character(0))
  fichiers <- fichiers_script(chemin_script, donnees, rdata, renv, rproj, langue)
  code_donnees <- donnees_script(tab, fichiers, langue)
  fonctions <- traduire_ordination(code_fonctions_ordination(m), langue)
  calcul <- traduire_ordination(code_calcul_ordination(projet, d, m, tab, langue), langue)
  e <- environnement_script(tab, fonctions, langue)
  probleme <- evaluer_script(calcul, e)
  ecart <- if (!is.null(probleme)) probleme else ecart_ordination(m, e, langue)
  sortie <- code_sortie_ordination(m, langue)
  graphiques <- code_graphiques_ordination(m, langue)
  nom <- texte_sur(as.character(d$name %||% ""))
  titre <- texte_sur(as.character(d$title %||% ""))
  type <- if (ca) tx("analyse des correspondances", "correspondence analysis") else tx("analyse de redondance (RDA)", "redundancy analysis (RDA)")
  entete <- c(sprintf(tx("# Modèle %s%s : %s, %d observations", "# Model %s%s: %s, %d observations"), nom, if (nzchar(titre)) sprintf(" (%s)", titre) else "", type, m$N),
              sprintf(tx("# Script R écrit par Nuée %s le %s, à partir du projet « %s ».", "# R script written by Nuée %s on %s, from the project “%s”."), version, format(Sys.Date()),
                      texte_sur(as.character(projet$source %||% "projet"))),
              tx("# Il refait cette analyse avec R seul, sans Nuée ni autre paquet : le calcul est celui du paquet vegan,",
                 "# It redoes this analysis with R alone, without Nuée or any package: the computation is that of the"),
              tx("# écrit pour ce modèle, de ses données à ses résultats, qui sont ceux de Nuée. Ses quatre parties :",
                 "# vegan package, written for this model, from its data to its results, which are those of Nuée. Its four parts:"),
              tx("#   1. Fonctions : l'analyse, ses scores, ses tests", "#   1. Functions: the analysis, its scores, its tests"),
              tx("#   2. Données", "#   2. Data"),
              tx("#   3. Modèle : variables, observations, calcul", "#   3. Model: variables, observations, computation"),
              tx("#   4. Résultats : tableaux et graphiques", "#   4. Results: tables and plots"),
              tx("# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.", "# In RStudio, Ctrl+Shift+O shows these parts; Ctrl+Shift+S runs the whole script."),
              sprintf(tx("# Écrit avec R %s ; il n'utilise que R (stats et graphics sont livrés avec R).", "# Written with R %s; it uses R alone (stats and graphics come with R)."),
                      paste(R.version$major, R.version$minor, sep = ".")),
              fichiers_a_cote(fichiers, langue, FALSE))
  lignes <- if (format == "R") {
    c(entete, "", titre_partie(1, langue), fonctions, titre_partie(2, langue), code_donnees, "", titre_partie(3, langue), calcul, "",
      titre_partie(4, langue), sortie, "", texte_graphiques(langue), unlist(lapply(graphiques, function(g) c(g$code, ""))))
  } else {
    lignes_rapport_ordination(format, d, m, langue, texte_methodes(projet, d, m, langue, version), fonctions, code_donnees, calcul, graphiques)
  }
  objets <- vapply(c("m", "r2", "echelle_1", "echelle_2", "echelle_3", "inertie", "valeurs_propres", "test", "permutations", "contributions",
                     "tests_axes", "tests_termes", "selection", "partition", "Y", "X", "Z", "Ybar"), nom_ordination, "", langue = langue)
  ecrire_fichiers_script(lignes, fichiers, e, c(nom_script("donnees", langue), objets), if (format != "R") c("knitr", "rmarkdown"))
  list(script = chemin_script, chemins = unname(fichiers), ecart = ecart)
}

# Le rapport R Markdown ou Quarto : les méthodes, les données, le code (replié dans le rapport
# compilé), les résultats (tableaux, graphiques en R de base) et les références.
lignes_rapport_ordination <- function(format, d, m, langue, methodes, fonctions, code_donnees, calcul, graphiques) {
  tx <- function(fr, en) if (langue == "en") en else fr
  bloc <- function(options, code) c(sprintf("```{r%s}", if (nzchar(options)) paste0(" ", options) else ""), code, "```", "")
  kable <- function(objet, chiffres) traduire_ordination(sprintf("knitr::kable(%s, digits = %d)", objet, chiffres), langue)
  nom <- texte_sur(as.character(d$name %||% ""))
  titre <- sprintf("%s %s%s", tx("Modèle", "Model"), nom, if (nzchar(as.character(d$title %||% ""))) paste(" —", texte_sur(d$title)) else "")
  yaml <- c("---", sprintf("title: %s", code_texte(titre)), sprintf("date: %s", code_texte(format(Sys.Date()))), sprintf("lang: %s", langue),
            if (format == "Rmd") c("output:", "  html_document:", "    code_folding: hide") else c("format:", "  html:", "    embed-resources: true", "    code-fold: true"),
            "---", "")
  ca <- identical(m$type, "CA")
  resultats <- if (ca) {
    c(bloc("valeurs-propres-table", kable("valeurs_propres", 4)),
      bloc("colonnes-table", kable("cbind(echelle_2$colonnes[, 1:min(3, ncol(echelle_2$colonnes)), drop = FALSE], contributions$colonnes$contribution[, 1:min(3, ncol(echelle_2$colonnes)), drop = FALSE])", 3)))
  } else {
    c(bloc("inertie-table", kable("inertie", 4)),
      bloc("r2-table", kable("t(unlist(r2))", 3)),
      if (m$options$permutations > 0) bloc("test-table", kable("t(test)", 3)),
      bloc("valeurs-propres-table", kable("valeurs_propres", 4)),
      if (!is.null(m$tests)) c(bloc("tests-axes", kable("tests_axes", 3)), bloc("tests-termes", kable("tests_termes", 3))),
      if (!is.null(m$selection)) bloc("selection-table", kable("selection$etapes", 3)),
      if (!is.null(m$partition)) bloc("partition-table", kable("partition$fractions", 3)))
  }
  c(yaml,
    sprintf("## %s", tx("Méthodes", "Methods")), "", rbind(texte_sur(methodes$paragraphes), ""),
    sprintf("## %s", tx("Données", "Data")), "",
    bloc("donnees, echo = FALSE", code_donnees),
    bloc("", traduire_code("str(donnees, list.len = 12)", langue)),
    sprintf("## %s", tx("Modèle", "Model")), "",
    tx("Le calcul, écrit en R pour ce modèle (bouton « Code » pour le voir).", "The computation, written in R for this model (“Code” button to see it)."), "",
    bloc("fonctions", fonctions[seq_len(max(0, length(fonctions) - 1))]),
    bloc("modele", calcul),
    sprintf("## %s", tx("Résultats", "Results")), "",
    resultats,
    unlist(lapply(graphiques, function(g) bloc(sprintf("%s, fig.width = %s, fig.height = %s", g$nom, g$largeur, g$hauteur), g$code))),
    sprintf("## %s", tx("Références", "References")), "", paste("-", methodes$references))
}
