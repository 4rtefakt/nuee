# Les moyennes ajustées d'une régression (Graphiques › Moyennes ajustées), comme les calcule le paquet
# emmeans (Lenth ; ses conventions, sans lui) : pour une variable qualitative X du modèle, la moyenne
# de chaque catégorie ajustée sur les autres termes (les moyennes marginales de la population de
# Searle, Speed et Milliken, 1980), avec son intervalle à 95 %, et les paires de catégories
# comparées ; au choix dans chaque catégorie d'une autre variable qualitative avec laquelle elle est
# croisée (les effets simples), ou la pente d'une variable quantitative croisée avec elle dans chaque
# catégorie (comme emtrends).
#
# La grille de référence (ref_grid d'emmeans, ses réglages par défaut) : chaque combinaison des
# catégories observées des variables qualitatives du modèle ; chaque variable quantitative à sa
# moyenne sur les observations du modèle, celle de la variable avant sa transformation (log10(x) : le
# log10 de la moyenne de x, comme emmeans) ; les termes croisés et les carrés en découlent (le produit
# des moyennes, non la moyenne des produits) ; une courbe (GAM) à la moyenne de sa variable ; l'effort
# d'un décalage à 1 (des densités : emmeans(offset = 0)). Les colonnes du modèle y sont faites comme
# celles des données (construire_bloc, les centres et poids du modèle), et chaque ligne de la grille
# donne sa ligne L du modèle (lm, nlme : la constante et les termes utiles ; mgcv : sa matrice
# lpmatrix, les effets aléatoires exclus). La moyenne d'une catégorie est celle des lignes L qui l'ont,
# à poids égaux sur les catégories des autres variables qualitatives ; son estimation L β, son
# erreur-type √(L V Lᵀ), V la covariance des coefficients (nlme : varFix ; mgcv : Vp, la covariance
# bayésienne, comme emmeans).

# Les références des moyennes ajustées dans le texte des Méthodes (regression.R :
# texte_methodes_regression les cherche ici après REFERENCES_REGRESSION).
REFERENCES_MOYENNES <- list(
  searle = "Searle, S. R., Speed, F. M., & Milliken, G. A. (1980). Population marginal means in the linear model: An alternative to least squares means. The American Statistician, 34(4), 216–221. https://doi.org/10.1080/00031305.1980.10483031",
  lenth = "Lenth, R. V. (2026). emmeans: Estimated marginal means, aka least-squares means (R package version 2.0.4). https://CRAN.R-project.org/package=emmeans",
  tukey = "Tukey, J. W. (1953). The problem of multiple comparisons. Unpublished manuscript, Princeton University. Reprinted in H. I. Braun (Ed.), The collected works of John W. Tukey: Vol. VIII. Multiple comparisons: 1948–1983 (pp. 1–300). Chapman & Hall, 1994.",
  kramer = "Kramer, C. Y. (1956). Extension of multiple range tests to group means with unequal numbers of replications. Biometrics, 12(3), 307–310. https://doi.org/10.2307/3001469",
  holm = "Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6(2), 65–70.")

# La grille de référence a au plus autant de lignes (rg.limit d'emmeans).
MOYENNES_LIGNES_MAX <- 10000

# Les variables qualitatives d'une régression `m` qui ont des moyennes ajustées : celles des termes
# qui peuvent entrer dans l'ajustement (m$candidats : un effet principal, ou un facteur de terme
# croisé), dans leur ordre (les effets principaux d'abord). Chacune, nommée par son numéro : son nom,
# ses catégories observées dans les lignes du modèle (leur numéro dans la variable, leur nom, dans
# l'ordre de la variable) et la colonne t1..tK de chacune.
qualitatives_modele <- function(m) {
  comp <- m$composition
  if (!length(comp)) return(list())
  uids <- unique(unlist(lapply(comp[m$candidats], function(ct) ct$variables[ct$categories != 0])))
  simples <- Filter(function(t) length(comp[[t]]$variables) == 1 && comp[[t]]$categories != 0, names(comp))
  res <- lapply(uids, function(u) {
    ts <- Filter(function(t) comp[[t]]$variables == u, simples)
    ts <- ts[order(vapply(ts, function(t) comp[[t]]$categories, 0))]
    vus <- ts[vapply(ts, function(t) any(m$donnees[[t]] == 1, na.rm = TRUE), TRUE)]
    list(uid = u, nom = comp[[ts[1]]]$noms[1], colonnes = vus, categories = vapply(vus, function(t) comp[[t]]$categories, 0, USE.NAMES = FALSE),
         niveaux = vapply(vus, function(t) as.character(comp[[t]]$niveaux[1]), "", USE.NAMES = FALSE))
  })
  stats::setNames(res, as.character(uids))
}

# Les variables qualitatives croisées avec la variable `u` dans un terme du modèle (m$candidats), par
# leur numéro (en texte).
croisees_avec <- function(m, u) {
  vs <- unlist(lapply(m$composition[m$candidats], function(ct) {
    q <- ct$variables[ct$categories != 0]
    if (length(ct$variables) > 1 && u %in% q) setdiff(q, u)
  }))
  as.character(unique(vs))
}

# Les variables quantitatives croisées avec la variable qualitative `u` (regression.R :
# pentes_croisees, les termes croisés d'une variable quantitative avec des catégories seules), dont la
# pente change d'une catégorie à l'autre : une liste nommée par leur numéro (en texte), le terme
# simple de chacune et son nom.
pentes_avec <- function(m, u) {
  comp <- m$composition
  pc <- pentes_croisees(m)
  garde <- Filter(function(t) u %in% unlist(lapply(comp[pc[[t]]], function(ct) ct$variables[ct$categories != 0])), names(pc))
  stats::setNames(lapply(garde, function(t) list(uid = comp[[t]]$variables, terme = t, nom = m$noms_termes[match(t, m$termes)])),
                  vapply(garde, function(t) as.character(comp[[t]]$variables), ""))
}

# Les réglages des moyennes ajustées (ceux de la demande, sinon ceux par défaut), vérifiés : la
# variable qualitative (`facteur`, son numéro ; par défaut la première du modèle), celle dans chaque
# catégorie de laquelle comparer les siennes (`dans`, croisée avec elle dans un terme du modèle, ou
# rien), la variable quantitative dont donner la pente dans chaque catégorie (`pente`, croisée avec
# elle, ou rien : les moyennes), l'ajustement des paires (`ajustement` : "tukey", "holm" ou "aucun")
# et l'échelle d'un modèle au lien log ou logit (`echelle` : "reponse" ou "lien").
reglages_moyennes <- function(m, quals, q) {
  un <- function(x) {
    x <- as.character(unlist(x))
    if (length(x) && !is.na(x[1]) && nzchar(x[1])) x[1]
  }
  nom <- function(u) if (!is.null(quals[[u]])) quals[[u]]$nom else u
  facteur <- un(q$facteur) %||% names(quals)[1]
  if (!facteur %in% names(quals)) {
    stop(sprintf(traduire("« %s » n'est pas une variable qualitative de ce modèle.", "\"%s\" isn't a qualitative variable of this model."), facteur), call. = FALSE)
  }
  dans <- un(q$dans)
  if (!is.null(dans) && !dans %in% intersect(croisees_avec(m, as.numeric(facteur)), names(quals))) {
    stop(sprintf(traduire("« %s » n'est croisée avec « %s » dans aucun terme du modèle : les catégories de l'une ne se comparent pas dans chaque catégorie de l'autre.",
                          "\"%s\" isn't crossed with \"%s\" in any term of the model: the categories of one aren't compared within each category of the other."),
                 nom(dans), nom(facteur)), call. = FALSE)
  }
  pente <- un(q$pente)
  pentes <- pentes_avec(m, as.numeric(facteur))
  if (!is.null(pente) && !pente %in% names(pentes)) {
    # (son nom, si elle est une variable du modèle)
    ct <- Find(function(ct) pente %in% as.character(ct$variables), m$composition)
    stop(sprintf(traduire("« %s » n'est pas une variable quantitative croisée avec « %s » dans un terme du modèle : sa pente ne change pas d'une catégorie à l'autre.",
                          "\"%s\" isn't a quantitative variable crossed with \"%s\" in a term of the model: its slope doesn't change from one category to another."),
                 if (!is.null(ct)) ct$noms[match(pente, as.character(ct$variables))] else pente, nom(facteur)), call. = FALSE)
  }
  ajustement <- un(q$ajustement) %||% "tukey"
  if (!ajustement %in% c("tukey", "holm", "aucun")) stop(traduire("Ajustement inconnu : tukey, holm ou aucun.", "Unknown adjustment: tukey, holm or aucun (none)."), call. = FALSE)
  echelle <- un(q$echelle) %||% "reponse"
  if (!echelle %in% c("reponse", "lien")) stop(traduire("Échelle inconnue : reponse ou lien.", "Unknown scale: reponse (response) or lien (link)."), call. = FALSE)
  list(facteur = facteur, dans = dans, pente = pente, ajustement = ajustement, echelle = echelle)
}

# Les variables quantitatives des termes simples du modèle (effets principaux, ou facteurs seuls d'un
# terme croisé), chacune avec sa moyenne sur les observations du modèle (avant sa transformation,
# comme emmeans), sa valeur dans la grille (transformée), son étendue et sa transformation ; une liste
# nommée par leur terme t1..tK. Les valeurs sont lues dans le projet pour les observations du modèle,
# et doivent redonner ses colonnes : sinon les données ont changé depuis son ajustement.
covariables_moyennes <- function(projet, m, termes) {
  comp <- m$composition
  ts <- Filter(function(t) length(comp[[t]]$variables) == 1 && comp[[t]]$categories == 0, names(comp))
  stats::setNames(lapply(ts, function(t) {
    k <- match(t, m$termes)
    tr <- termes[[k]]$transform
    brut <- valeurs_simples(projet, termes[[k]], m$observations)
    attendu <- (transformer(brut, tr) - m$bloc$centre[k]) * m$bloc$poids[k]
    if (anyNA(brut) || any(abs(attendu - m$donnees[[t]]) > 1e-8 * pmax(1, abs(m$donnees[[t]])), na.rm = TRUE) || anyNA(attendu)) {
      stop(sprintf(traduire("Les valeurs de « %s » ont changé depuis l'ajustement de ce modèle : ajustez-le de nouveau.",
                            "The values of \"%s\" have changed since this model was fitted: fit it again."), m$noms_termes[k]), call. = FALSE)
    }
    moyenne <- mean(brut)
    list(terme = t, uid = comp[[t]]$variables, nom = m$noms_termes[k], moyenne = moyenne, valeur = transformer(moyenne, tr),
         etendue = diff(range(brut)), transformation = tr, courbe = t %in% m$lisses, seul = isTRUE(comp[[t]]$facteur_seul))
  }), ts)
}

# Les valeurs des termes simples de chaque ligne de la grille `G` (une colonne par variable
# qualitative, le rang de sa catégorie parmi celles observées), avant leur mise à l'échelle : 0 ou 1
# pour une catégorie, la valeur de la grille pour une variable quantitative (`cov` :
# covariables_moyennes ; `decale` : un terme à une autre valeur, pour la pente) ; NA pour les termes
# croisés, que construire_bloc calcule.
bruts_grille <- function(m, quals, G, cov, decale = NULL) {
  comp <- m$composition
  B <- matrix(NA_real_, nrow(G), length(m$termes), dimnames = list(NULL, m$termes))
  for (t in m$termes) {
    ct <- comp[[t]]
    if (length(ct$variables) > 1) next
    if (ct$categories == 0) {
      B[, t] <- if (!is.null(decale[[t]])) decale[[t]] else cov[[t]]$valeur
    } else {
      f <- quals[[as.character(ct$variables)]]
      B[, t] <- if (is.null(f)) 0 else as.numeric(f$categories[G[[as.character(ct$variables)]]] == ct$categories)
    }
  }
  B
}

# Les colonnes t1..tK du modèle aux lignes de la grille (`B` : bruts_grille), comme celles des
# données (pretraitement.R : construire_bloc, les centres et poids du modèle ; les termes croisés, le
# produit de leurs facteurs mis à l'échelle, mis à l'échelle à son tour). `brut` : sans centre ni
# poids (le codage de R sans mise à l'échelle, pour les degrés de liberté de nlme).
colonnes_grille <- function(m, termes, B, brut = FALSE) {
  K <- length(m$termes)
  bloc <- if (brut) list(centre = rep(0, K), poids = rep(1, K), type = rep("none", K), facteur = rep(1, K)) else m$bloc
  X <- construire_bloc(NULL, termes, seq_len(nrow(B)), imposees = bloc, bruts = B)$X
  dimnames(X) <- list(NULL, m$termes)
  X
}

# Les lignes L du modèle aux lignes `X` de la grille (colonnes_grille) : la constante et les termes
# utiles (lm, nlme), ou la matrice lpmatrix de mgcv, ses effets aléatoires exclus (des groupes connus du
# modèle, que l'exclusion annule, comme predire_mgcv) et l'effort à 1 (un décalage nul).
lignes_modele <- function(m, X) {
  a <- m$ajustement
  if (inherits(a, "gamm")) a <- a$gam
  if (inherits(a, "gam")) {
    nd <- as.data.frame(X)
    for (g in intersect(c("g1", "g12", "g123"), names(m$donnees))) nd[[g]] <- factor(rep(levels(m$donnees[[g]])[1], nrow(nd)), levels = levels(m$donnees[[g]]))
    if ("decalage" %in% names(m$donnees)) nd$decalage <- 0
    aleatoires <- vapply(Filter(function(s) inherits(s, "random.effect"), a$smooth), function(s) s$label, "")
    L <- stats::predict(a, nd, type = "lpmatrix", exclude = aleatoires)
    attr(L, "model.offset") <- NULL
    return(unclass(L))
  }
  b <- coefficients_covariance(a)$b
  L <- cbind(1, X[, setdiff(names(b), "(Intercept)"), drop = FALSE])
  colnames(L) <- names(b)
  L
}

# Une base orthonormée de l'espace nul de `X` (les combinaisons de ses colonnes qui y valent 0), NULL
# quand X est de plein rang ; son rang par la décomposition QR pivotée de lm(), à sa tolérance.
base_nulle <- function(X, tol = 1e-7) {
  q <- qr(X, tol = tol)
  r <- q$rank
  p <- ncol(X)
  if (r == p) return(NULL)
  R <- qr.R(q)
  N <- matrix(0, p, p - r)
  N[q$pivot[seq_len(r)], ] <- -backsolve(R[seq_len(r), seq_len(r), drop = FALSE], R[seq_len(r), (r + 1):p, drop = FALSE])
  N[q$pivot[(r + 1):p], ] <- diag(p - r)
  qr.Q(qr(N))
}

# Les lignes de `L` qui sont estimables : orthogonales à l'espace nul `N` (base_nulle) des colonnes
# du modèle, comme les vérifie emmeans (estimability::is.estble, sa tolérance) ; une combinaison de
# catégories qu'aucune observation n'a ne l'est pas.
estimables <- function(L, N, tol = 1e-8) {
  if (is.null(N)) return(rep(TRUE, nrow(L)))
  ssq <- rowSums(L^2)
  ssq[ssq < tol] <- 1
  rowSums((L %*% N)^2) < tol * ssq
}

# Les moyennes, à poids égaux, des lignes de `M` de chaque groupe de `groupes` (des rangs de lignes).
moyennes_lignes <- function(M, groupes) do.call(rbind, lapply(groupes, function(i) colMeans(M[i, , drop = FALSE])))

# Les degrés de liberté de chaque ligne `L` (et `L_brut`, la même dans le codage de R sans mise à
# l'échelle) : ceux qu'emmeans prend par défaut. lm : les degrés de liberté résiduels ; nlme::lme : la
# règle de confinement de nlme (fixDF), le plus petit de ceux des coefficients que la ligne fait
# intervenir (|L| > 1e-4), l'ordonnée à l'origine comptant pour le nombre de groupes du premier effet
# aléatoire moins un ; mgcv : les degrés de liberté résiduels quand l'échelle est estimée (loi
# normale, gamma, Tweedie), l'infini (z, asymptotique) pour une loi à l'échelle connue (Poisson,
# binomiale, binomiale négative), comme le résumé de mgcv et les coefficients de Nuée, et comme
# emmeans pour un glm().
ddl_lignes <- function(m, L_brut) {
  a <- m$ajustement
  if (inherits(a, "lme")) {
    dfx <- a$fixDF$X
    dfx[["(Intercept)"]] <- nlevels(a$groups[[1]]) - 1
    k <- L_brut[, names(dfx), drop = FALSE]
    return(apply(k, 1, function(x) { i <- which(abs(x) > 1e-4); if (length(i)) min(dfx[i]) else NA_real_ }))
  }
  # (mgcv : un gam est aussi un lm, il passe d'abord)
  if (inherits(a, c("gam", "gamm"))) {
    if (m$famille %in% c("poisson", "binomiale", "binomiale_negative")) return(rep(Inf, nrow(L_brut)))
    return(rep(if (inherits(a, "gamm")) a$gam$df.residual else a$df.residual, nrow(L_brut)))
  }
  rep(stats::df.residual(a), nrow(L_brut))
}

# La méthode des degrés de liberté, pour les textes : "residuels" (lm, mgcv à l'échelle estimée),
# "confinement" (nlme), "asymptotique" (z).
methode_ddl <- function(m) {
  if (inherits(m$ajustement, "lme")) "confinement"
  else if (inherits(m$ajustement, c("gam", "gamm")) && m$famille %in% c("poisson", "binomiale", "binomiale_negative")) "asymptotique"
  else "residuels"
}

# Les moyennes ajustées du modèle `m` (de définition `d`, du projet `projet`) : `q`, les réglages
# (reglages_moyennes). Renvoie les réglages, les choix possibles (les variables qualitatives, celles
# croisées avec chacune, les variables quantitatives dont la pente change avec chacune), ce que la
# grille a pris (les variables quantitatives et leur valeur, les variables qualitatives moyennées,
# l'effort), les moyennes (ou les pentes) de chaque catégorie, ou de chaque combinaison avec `dans`,
# avec leur intervalle à 95 %, les paires de catégories (la différence, ou le rapport, son intervalle
# et sa valeur p ajustés), ce qui n'est pas estimable, et une phrase pour un rapport par paire.
moyennes_ajustees <- function(projet, m, d, q = list()) {
  quals <- qualitatives_modele(m)
  if (!length(quals)) {
    stop(traduire("Ce modèle n'a pas de variable qualitative : les moyennes ajustées se calculent pour chaque catégorie d'une variable qualitative X (un effet principal, ou dans un terme croisé).",
                  "This model has no qualitative variable: adjusted means are computed for each category of a qualitative X variable (a main effect, or in a cross term)."), call. = FALSE)
  }
  r <- reglages_moyennes(m, quals, q)
  fa <- quals[[r$facteur]]
  fb <- if (!is.null(r$dans)) quals[[r$dans]]
  G <- expand.grid(lapply(quals, function(f) seq_along(f$colonnes)), KEEP.OUT.ATTRS = FALSE)
  names(G) <- names(quals)
  if (nrow(G) > MOYENNES_LIGNES_MAX) {
    stop(sprintf(traduire("La grille de référence aurait %d lignes (une par combinaison des catégories des variables qualitatives) : %d au plus.",
                          "The reference grid would have %d rows (one per combination of the categories of the qualitative variables): %d at most."),
                 nrow(G), MOYENNES_LIGNES_MAX), call. = FALSE)
  }
  termes <- termes_regression(d$x)
  cov <- covariables_moyennes(projet, m, termes)
  B <- bruts_grille(m, quals, G, cov)
  X <- colonnes_grille(m, termes, B)
  X_brut <- colonnes_grille(m, termes, B, brut = TRUE)
  param <- c("(Intercept)", setdiff(m$candidats, m$lisses))
  avec_un <- function(M) cbind(`(Intercept)` = 1, M[, setdiff(param, "(Intercept)"), drop = FALSE])
  L <- lignes_modele(m, X)
  Lp <- avec_un(X)  # (la partie paramétrique de tous les termes candidats : pour l'estimabilité)
  Lb <- cbind(`(Intercept)` = 1, X_brut)
  pente <- NULL
  if (!is.null(r$pente)) {
    # la pente de la variable quantitative (emtrends) : la différence avant de la prédiction, sa valeur
    # augmentée d'un millième de son étendue, par unité du terme (transformé s'il l'est)
    cv <- cov[[pentes_avec(m, as.numeric(r$facteur))[[r$pente]]$terme]]
    delta <- 0.001 * cv$etendue
    if (!isTRUE(delta > 0)) stop(sprintf(traduire("« %s » ne varie pas dans les observations du modèle.", "\"%s\" doesn't vary in the model's observations."), cv$nom), call. = FALSE)
    plus <- transformer(cv$moyenne + delta, cv$transformation)
    h <- if (is.null(cv$transformation)) delta else plus - cv$valeur  # (emtrends : le pas, ou le changement du terme transformé)
    B2 <- bruts_grille(m, quals, G, cov, decale = stats::setNames(list(plus), cv$terme))
    X2 <- colonnes_grille(m, termes, B2)
    L <- (lignes_modele(m, X2) - L) / h
    Lp <- (avec_un(X2) - Lp) / h
    Lb <- (cbind(`(Intercept)` = 1, colonnes_grille(m, termes, B2, brut = TRUE)) - Lb) / h
    pente <- list(uid = cv$uid, nom = cv$nom, terme = cv$terme, delta = delta, transformation = cv$transformation)
  }
  cb <- coefficients_covariance(if (inherits(m$ajustement, "gamm")) m$ajustement$gam else m$ajustement)
  b <- cb$b
  V <- cb$V
  if (anyNA(b)) {  # (un coefficient que lm() a laissé de côté : 0, comme emmeans)
    ok <- !is.na(b)
    b <- b[ok]
    V <- V[ok, ok, drop = FALSE]
  }
  L <- L[, names(b), drop = FALSE]
  N <- base_nulle(cbind(1, as.matrix(m$donnees[setdiff(param, "(Intercept)")])))

  # --- les moyennes (ou les pentes) de chaque catégorie, ou combinaison de catégories
  cellules <- if (is.null(fb)) data.frame(a = seq_along(fa$colonnes), b = NA_integer_)
              else expand.grid(a = seq_along(fa$colonnes), b = seq_along(fb$colonnes), KEEP.OUT.ATTRS = FALSE)
  groupes <- lapply(seq_len(nrow(cellules)), function(i) which(G[[r$facteur]] == cellules$a[i] & (is.na(cellules$b[i]) | G[[r$dans %||% r$facteur]] == cellules$b[i])))
  Lc <- moyennes_lignes(L, groupes)
  Lpc <- moyennes_lignes(Lp, groupes)
  ec <- estimables(Lpc, N)
  Lbc <- moyennes_lignes(Lb, groupes)
  dfc <- ddl_lignes(m, Lbc)
  est <- as.vector(Lc %*% b)
  se <- sqrt(pmax(0, rowSums((Lc %*% V) * Lc)))
  # (avec le lien log ou logit, sur l'échelle de la réponse ; les pentes restent sur celle du lien, comme emtrends)
  lien <- m$lien %||% "identity"
  reponse <- is.null(pente) && lien %in% c("log", "logit") && r$echelle == "reponse"
  inverse <- if (identical(lien, "logit")) stats::plogis else exp
  derivee <- if (identical(lien, "logit")) stats::dlogis else exp
  qt95 <- stats::qt(0.975, ifelse(is.na(dfc), Inf, dfc))
  moy <- data.frame(niveau = fa$niveaux[cellules$a], groupe = if (is.null(fb)) NA_character_ else fb$niveaux[cellules$b],
                    estimation = est, erreur_type = se, ddl = dfc, bas = est - qt95 * se, haut = est + qt95 * se, stringsAsFactors = FALSE)
  if (reponse) {
    moy$estimation <- inverse(est)
    moy$erreur_type <- abs(derivee(est)) * se
    moy$bas <- inverse(est - qt95 * se)
    moy$haut <- inverse(est + qt95 * se)
  }
  moy$lien <- est
  moy$erreur_type_lien <- se
  # (non estimables : une catégorie, ou une combinaison, qu'aucune observation n'a ; laissées de côté)
  nom_cellule <- function(i) if (is.null(fb)) fa$niveaux[cellules$a[i]] else paste(fa$niveaux[cellules$a[i]], fb$niveaux[cellules$b[i]], sep = " · ")
  non_moy <- vapply(which(!ec), nom_cellule, "")
  moy <- moy[ec, , drop = FALSE]
  rownames(moy) <- NULL

  # --- les paires de catégories, dans chaque catégorie de `dans`
  paires <- list()
  non_paires <- character(0)
  bs <- if (is.null(fb)) NA_integer_ else seq_along(fb$colonnes)
  for (gb in bs) {
    ici <- which(is.na(cellules$b) | cellules$b %in% gb)
    if (length(ici) < 2) next
    ij <- utils::combn(ici, 2)
    for (k in seq_len(ncol(ij))) {
      i <- ij[1, k]
      j <- ij[2, k]
      Ld <- Lc[i, , drop = FALSE] - Lc[j, , drop = FALSE]
      nom <- sprintf("%s − %s", fa$niveaux[cellules$a[i]], fa$niveaux[cellules$a[j]])
      if (!estimables(Lpc[i, , drop = FALSE] - Lpc[j, , drop = FALSE], N)) {
        non_paires <- c(non_paires, if (is.null(fb)) nom else sprintf("%s (%s)", nom, fb$niveaux[gb]))
        next
      }
      d1 <- sum(Ld * b)
      s1 <- sqrt(max(0, sum((Ld %*% V) * Ld)))
      paires[[length(paires) + 1]] <- data.frame(groupe = if (is.null(fb)) NA_character_ else fb$niveaux[gb],
                                                 niveau_1 = fa$niveaux[cellules$a[i]], niveau_2 = fa$niveaux[cellules$a[j]],
                                                 difference = d1, erreur_type_lien = s1, ddl = ddl_lignes(m, Lbc[i, , drop = FALSE] - Lbc[j, , drop = FALSE]),
                                                 stringsAsFactors = FALSE)
    }
  }
  pa <- if (length(paires)) do.call(rbind, paires) else data.frame(groupe = character(0), niveau_1 = character(0), niveau_2 = character(0),
                                                                    difference = numeric(0), erreur_type_lien = numeric(0), ddl = numeric(0))
  # (une paire non estimable rend l'ajustement adaptatif, comme dans emmeans : Tukey même pour une seule paire)
  pa <- ajuster_paires(pa, r$ajustement, adaptatif = length(non_paires) > 0)
  # (sur l'échelle de la réponse, des rapports, de moyennes ou des cotes, et leur intervalle ; testés sur l'échelle du lien)
  pa$estimation <- if (reponse) exp(pa$difference) else pa$difference
  pa$erreur_type <- if (reponse) exp(pa$difference) * pa$erreur_type_lien else pa$erreur_type_lien
  pa$bas <- if (reponse) exp(pa$bas_lien) else pa$bas_lien
  pa$haut <- if (reponse) exp(pa$haut_lien) else pa$haut_lien

  res <- list(reglages = r, type = m$type, famille = m$famille, lien = lien, reponse = m$y$noms, transformation_y = m$y$transformation,
              forme_y = if (!is.null(d$y[[1]]$transform)) formule_transformation(d$y[[1]]$transform, m$y$noms, !identical(getOption("nuee.langue"), "en")),
              log_y = isTRUE(as.character(d$y[[1]]$transform$type) %in% c("log10", "ln")),
              mode = if (is.null(pente)) "moyennes" else "pentes", echelle = if (reponse) "reponse" else if (lien == "identity") "identite" else "lien",
              facteur = list(uid = fa$uid, nom = fa$nom, niveaux = I(fa$niveaux)), dans = if (!is.null(fb)) list(uid = fb$uid, nom = fb$nom, niveaux = I(fb$niveaux)),
              pente = if (!is.null(pente)) list(uid = pente$uid, nom = pente$nom, terme = pente$terme, delta = pente$delta,
                                                forme = if (!is.null(pente$transformation)) formule_transformation(pente$transformation, pente$nom, !identical(getOption("nuee.langue"), "en"))),
              grille = list(lignes = nrow(G), covariables = if (length(cov)) data.frame(terme = names(cov), nom = vapply(cov, `[[`, "", "nom"), moyenne = vapply(cov, `[[`, 0, "moyenne"),
                                                                                       valeur = vapply(cov, `[[`, 0, "valeur"), transformee = vapply(cov, function(x) !is.null(x$transformation), TRUE),
                                                                                       courbe = vapply(cov, `[[`, TRUE, "courbe"), row.names = NULL, stringsAsFactors = FALSE),
                            moyennees = I(unname(vapply(quals[setdiff(names(quals), c(r$facteur, r$dans))], `[[`, "", "nom"))), decalage = m$decalage),
              croisee = I(unname(c(vapply(quals[intersect(croisees_avec(m, fa$uid), names(quals))], `[[`, "", "nom"),
                                   vapply(pentes_avec(m, fa$uid), `[[`, "", "nom")))),
              ddl = list(methode = methode_ddl(m), groupe = if (inherits(m$ajustement, "lme")) m$niveaux[1], groupes = if (inherits(m$ajustement, "lme")) nlevels(m$ajustement$groups[[1]])),
              moyennes = moy, paires = pa, non_estimables = list(moyennes = I(non_moy), paires = I(non_paires)),
              options = options_moyennes(m, quals))
  res$phrases <- I(vapply(seq_len(nrow(pa)), function(i) phrase_paire(res, i), ""))
  res
}

# Les valeurs p et les intervalles à 95 % des paires `pa` (difference : sur l'échelle du lien,
# erreur_type_lien, ddl ; groupe : la catégorie de `dans`), ajustés comme emmeans les ajuste dans
# chaque groupe, sur les paires estimables qu'il a (n) : "tukey", la loi du maximum studentisé
# (ptukey, qtukey) pour une famille de (1 + √(1 + 8n)) / 2 moyennes, n(n − 1)/2 paires (Tukey, 1953 ;
# Kramer, 1956) ; "holm", les valeurs p de Holm (1979) et les intervalles de Bonferroni (emmeans ne
# donne pas d'intervalles de Holm) ; "aucun". Une seule paire dans un groupe n'a rien à ajuster, sauf
# pour Tukey quand des paires ne sont pas estimables (`adaptatif` : emmeans la compte alors comme une
# famille de deux moyennes, les mêmes nombres à la précision de qtukey près). Ajoute t (z avec une
# infinité de degrés de liberté), p_brute, p, famille, bas_lien et haut_lien.
ajuster_paires <- function(pa, ajustement, adaptatif = FALSE) {
  n <- nrow(pa)
  pa$t <- pa$difference / pa$erreur_type_lien
  df <- ifelse(is.na(pa$ddl), Inf, pa$ddl)
  brute <- 2 * stats::pt(abs(pa$t), df, lower.tail = FALSE)
  p <- brute
  critique <- stats::qt(0.975, df)
  famille <- rep(NA_real_, n)
  for (g in unique(pa$groupe)) {
    ici <- which(pa$groupe %in% g)
    nc <- length(ici)
    if (ajustement == "aucun" || nc < 1 || (nc < 2 && !(adaptatif && ajustement == "tukey"))) next
    if (ajustement == "tukey") {
      k <- (1 + sqrt(1 + 8 * nc)) / 2
      famille[ici] <- k
      p[ici] <- stats::ptukey(sqrt(2) * abs(pa$t[ici]), k, zapsmall(df[ici]), lower.tail = FALSE)
      critique[ici] <- stats::qtukey(0.95, k, df[ici]) / sqrt(2)
    } else {
      famille[ici] <- nc
      p[ici] <- stats::p.adjust(brute[ici], "holm")
      critique[ici] <- stats::qt(1 - 0.05 / nc / 2, df[ici])
    }
  }
  pa$p_brute <- brute
  pa$p <- p
  pa$famille <- famille
  pa$bas_lien <- pa$difference - critique * pa$erreur_type_lien
  pa$haut_lien <- pa$difference + critique * pa$erreur_type_lien
  pa
}

# Les choix des moyennes ajustées d'un modèle, pour l'interface : chaque variable qualitative (son
# numéro, son nom, ses catégories), les variables qualitatives croisées avec elle et les variables
# quantitatives dont la pente change avec elle (leur numéro et leur nom).
options_moyennes <- function(m, quals = qualitatives_modele(m)) {
  unname(lapply(quals, function(f) {
    dans <- intersect(croisees_avec(m, f$uid), names(quals))
    pentes <- pentes_avec(m, f$uid)
    list(uid = f$uid, nom = f$nom, niveaux = I(f$niveaux),
         dans = unname(lapply(dans, function(u) list(uid = quals[[u]]$uid, nom = quals[[u]]$nom))),
         pentes = unname(lapply(pentes, function(x) list(uid = x$uid, nom = x$nom))))
  }))
}

# ------------------------------------------------------------ les phrases

# La phrase pour un rapport de la paire `i` des moyennes ajustées `r` (moyennes_ajustees), dans la
# langue des messages : la différence (ou le rapport), son intervalle, t (ou z) et ses degrés de
# liberté, p et son ajustement, et les deux moyennes (ou pentes).
phrase_paire <- function(r, i) {
  fr <- !identical(getOption("nuee.langue"), "en")
  marque <- if (fr) "," else "."
  # (au moins deux décimales, trois chiffres significatifs pour une petite valeur : 0,0614)
  nb <- function(x, ref = x) {
    if (is.na(x)) return("NA")
    a <- abs(ref)
    dec <- if (is.finite(a) && a > 0) max(2, 2 - floor(log10(a))) else 2
    formatC(if (round(x, dec) == 0) 0 else x, format = "f", digits = min(dec, 8), decimal.mark = marque)
  }
  ddl <- function(x) if (is.na(x)) "NA" else if (abs(x - round(x)) < 1e-8) format(round(x)) else formatC(x, format = "f", digits = 1, decimal.mark = marque)
  pv <- function(p) if (is.na(p)) "p = NA" else if (p < 0.001) paste("p <", formatC(0.001, format = "f", digits = 3, decimal.mark = marque))
                    else paste("p =", trimws(formatC(p, digits = 3, format = "g", decimal.mark = marque)))
  guillemets <- function(x) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)
  p <- r$paires[i, ]
  m1 <- r$moyennes[r$moyennes$niveau == p$niveau_1 & (is.na(r$moyennes$groupe) | r$moyennes$groupe %in% p$groupe), ]
  m2 <- r$moyennes[r$moyennes$niveau == p$niveau_2 & (is.na(r$moyennes$groupe) | r$moyennes$groupe %in% p$groupe), ]
  pentes <- identical(r$mode, "pentes")
  quoi <- if (pentes) sprintf(if (fr) "Pentes de %s par %s" else "Slopes of %s by %s", guillemets(r$pente$nom), guillemets(r$facteur$nom))
          else sprintf(if (fr) "Moyennes ajustées de %s par %s" else "Adjusted means of %s by %s", guillemets(r$reponse), guillemets(r$facteur$nom))
  dans <- if (!is.null(r$dans)) sprintf(if (fr) " dans %s = %s" else " within %s = %s", guillemets(r$dans$nom), p$groupe) else ""
  rapport <- identical(r$echelle, "reponse")
  libelle <- if (!rapport) sprintf("%s − %s", p$niveau_1, p$niveau_2)
             else sprintf("%s%s / %s", if (identical(r$lien, "logit")) (if (fr) "rapport des cotes " else "odds ratio ") else "", p$niveau_1, p$niveau_2)
  stat <- if (is.infinite(p$ddl) || is.na(p$ddl)) sprintf("z = %s", nb(p$t, 1)) else sprintf("t(%s) = %s", ddl(p$ddl), nb(p$t, 1))
  k <- p$famille
  ajuste <- if (is.na(k)) (if (identical(r$reglages$ajustement, "aucun")) (if (fr) "sans ajustement" else "unadjusted") else (if (fr) "une seule paire" else "a single pair"))
            else if (identical(r$reglages$ajustement, "tukey")) sprintf("Tukey, %s %s", if (abs(k - round(k)) < 1e-9) format(round(k)) else formatC(k, format = "f", digits = 2, decimal.mark = marque),
                                                                   if (pentes) (if (fr) "pentes" else "slopes") else (if (fr) "moyennes" else "means"))
            else sprintf(if (fr) "Holm, %d paires" else "Holm, %d pairs", as.integer(k))
  deux <- if (nrow(m1) && nrow(m2)) {
    sprintf(if (fr) " ; %s %s = %s et %s = %s%s" else "; %s %s = %s and %s = %s%s", if (pentes) (if (fr) "pentes" else "slopes") else (if (fr) "moyennes" else "means"),
            p$niveau_1, nb(m1$estimation[1]), p$niveau_2, nb(m2$estimation[1]),
            if (pentes) sprintf(if (fr) " par unité de %s" else " per unit of %s", if (!is.null(r$pente$forme)) r$pente$forme else guillemets(r$pente$nom)) else "")
  } else ""
  sprintf(if (fr) "%s%s (%s) : %s = %s [IC 95 %% %s ; %s], %s, %s (%s)%s." else "%s%s (%s): %s = %s [95%% CI %s, %s], %s, %s (%s)%s.",
          quoi, dans, texte_modele_moyennes(r, fr), libelle, nb(p$estimation), nb(p$bas, p$estimation), nb(p$haut, p$estimation), stat, pv(p$p), ajuste, deux)
}

# Le modèle et l'échelle des moyennes ajustées `r`, en mots : « modèle linéaire mixte », « modèle
# linéaire généralisé, loi de Poisson ; échelle de la réponse »...
texte_modele_moyennes <- function(r, fr) {
  lois <- if (fr) c(gaussienne = "normale", poisson = "de Poisson", binomiale_negative = "binomiale négative", binomiale = "binomiale", gamma = "gamma", tweedie = "de Tweedie")
          else c(gaussienne = "normal", poisson = "Poisson", binomiale_negative = "negative binomial", binomiale = "binomial", gamma = "gamma", tweedie = "Tweedie")
  type <- switch(r$type, LM = if (fr) "régression linéaire" else "linear regression", LMM = if (fr) "modèle linéaire mixte" else "linear mixed model",
                 GLM = sprintf(if (fr) "modèle linéaire généralisé, loi %s" else "generalized linear model, %s distribution", lois[[r$famille]]),
                 sprintf(if (fr) "modèle additif généralisé, loi %s" else "generalized additive model, %s distribution", lois[[r$famille]]))
  echelle <- switch(r$echelle, reponse = if (fr) "échelle de la réponse" else "response scale",
                    lien = sprintf(if (fr) "échelle du lien %s" else "%s link scale", r$lien), "")
  paste(c(type, if (nzchar(echelle)) echelle), collapse = if (fr) " ; " else "; ")
}

# Les moyennes ajustées dans le texte des Méthodes (texte_methodes_regression, quand elles ont été
# calculées sur ce modèle : m$moyennes) : ce qui a été calculé et sur quelle grille, les erreurs-types,
# les degrés de liberté, l'échelle, les paires et leur ajustement, ce qui n'était pas estimable. `d` :
# la définition du modèle (les formules de ses transformations, dans la langue du texte) ; `liste`,
# `guillemets` : ceux de texte_methodes_regression. Renvoie le texte et les références à citer.
phrase_methodes_moyennes <- function(m, d, fr, liste, guillemets) {
  r <- m$moyennes
  g <- r$grille
  cites <- c("searle", "lenth")
  pentes <- identical(r$mode, "pentes")
  forme_y <- if (!is.null(d$y[[1]]$transform)) formule_transformation(d$y[[1]]$transform, guillemets(m$y$noms), fr)
  tr_pente <- if (pentes) termes_regression(d$x)[[match(r$pente$terme, m$termes)]]$transform
  par_unite <- if (pentes) (if (!is.null(tr_pente)) formule_transformation(tr_pente, guillemets(r$pente$nom), fr) else guillemets(r$pente$nom))
  quoi <- if (pentes) {
    sprintf(if (fr) "La pente de %s dans chaque catégorie de %s%s (par unité de %s ; la dérivée de la prédiction du modèle, par une différence finie d'un millième de l'étendue de la variable, comme emtrends du paquet emmeans ; Lenth, 2026)"
            else "The slope of %s in each category of %s%s (per unit of %s; the derivative of the model's prediction, by a finite difference of a thousandth of the variable's range, as emtrends of the emmeans package does; Lenth, 2026)",
            guillemets(r$pente$nom), guillemets(r$facteur$nom), if (!is.null(r$dans)) sprintf(if (fr) " et de %s" else " and of %s", guillemets(r$dans$nom)) else "",
            par_unite)
  } else {
    sprintf(if (fr) "Les moyennes ajustées de chaque catégorie de %s%s (des moyennes marginales estimées ; Searle et al., 1980) ont été calculées comme le fait le paquet emmeans (Lenth, 2026)"
            else "The adjusted means of each category of %s%s (estimated marginal means; Searle et al., 1980) were computed as the emmeans package does (Lenth, 2026)",
            guillemets(r$facteur$nom), if (!is.null(r$dans)) sprintf(if (fr) " dans chaque catégorie de %s" else " within each category of %s", guillemets(r$dans$nom)) else "")
  }
  cov <- g$covariables
  valeur <- function(x) trimws(formatC(x, digits = 6, format = "fg", decimal.mark = if (fr) "," else "."))
  valeurs <- if (length(cov) && nrow(cov)) {
    vapply(seq_len(nrow(cov)), function(k) sprintf(if (fr) "%s à sa moyenne sur les observations du modèle (%s)%s" else "%s at its mean over the model's observations (%s)%s",
                                                   guillemets(cov$nom[k]), valeur(cov$moyenne[k]),
                                                   if (cov$courbe[k]) (if (fr) ", sa courbe en ce point" else ", its curve at that point")
                                                   else if (cov$transformee[k]) (if (fr) ", transformée ensuite" else ", transformed afterwards") else ""), "")
  }
  moyennees <- as.character(unlist(g$moyennees))
  grille <- c(if (length(valeurs)) liste(valeurs),
              if (length(moyennees)) sprintf(if (fr) "la moyenne à poids égaux sur les catégories de %s" else "the equally weighted average over the categories of %s", liste(guillemets(moyennees))),
              if (!is.null(g$decalage)) sprintf(if (fr) "un effort de 1 (%s en décalage : des densités)" else "an effort of 1 (%s as an offset: densities)", guillemets(g$decalage)))
  phrase1 <- sprintf(if (fr) "%s : %s du modèle sur une grille de référence formée des catégories de ses variables qualitatives%s%s, leurs erreurs-types tirées de la matrice de covariance des coefficients%s."
                     else "%s: %s of the model on a reference grid made of the categories of its qualitative variables%s%s, their standard errors from the covariance matrix of the coefficients%s.",
                     quoi, if (pentes) (if (fr) "à partir des prédictions" else "from the predictions") else (if (fr) "les prédictions" else "the predictions"),
                     if (length(grille)) sprintf(if (fr) ", avec %s" else ", with %s", liste(grille)) else "",
                     if (length(cov) && nrow(cov) && any(lengths(lapply(m$composition, `[[`, "variables")) > 1)) (if (fr) " (les termes croisés évalués à ces valeurs)" else " (the cross terms evaluated at these values)") else "",
                     if (inherits(m$ajustement, c("gam", "gamm"))) (if (fr) " (la covariance bayésienne de mgcv)" else " (mgcv's Bayesian covariance)") else "")
  dl <- r$ddl
  ddl <- switch(dl$methode,
    confinement = { cites <- c(cites, "pinheiro"); sprintf(if (fr) "Les intervalles à 95 %% et les tests suivent la loi t, avec les degrés de liberté de la règle de confinement de nlme (Pinheiro et Bates, 2000) comme emmeans les prend : le plus petit de ceux des coefficients qu'une combinaison fait intervenir, l'ordonnée à l'origine comptant pour le nombre de %s moins un (%d)."
                                                          else "The 95%% intervals and tests use the t distribution, with the degrees of freedom of nlme's containment rule (Pinheiro & Bates, 2000) as emmeans takes them: the smallest of those of the coefficients a combination involves, the intercept counting for the number of %s minus one (%d).",
                                                          guillemets(dl$groupe), as.integer(dl$groupes) - 1L) },
    asymptotique = if (fr) "Les intervalles à 95 % et les tests suivent la loi normale (z, asymptotiques), l'échelle de la loi étant connue." else "The 95% intervals and tests use the normal distribution (z, asymptotic), the distribution's scale being known.",
    if (fr) "Les intervalles à 95 % et les tests suivent la loi t, avec les degrés de liberté résiduels du modèle." else "The 95% intervals and tests use the t distribution, with the model's residual degrees of freedom.")
  echelle <- if (identical(r$echelle, "reponse")) {
    sprintf(if (fr) "Avec le lien %s, %s ont été ramenées sur l'échelle de la réponse (%s), leurs intervalles transformés depuis l'échelle du lien, et les paires sont des %s, testés sur l'échelle du lien."
            else "With the %s link, %s were back-transformed to the response scale (%s), their intervals transformed from the link scale, and the pairs are %s, tested on the link scale.",
            r$lien, if (fr) "les moyennes" else "the means", if (identical(r$lien, "log")) "exp" else (if (fr) "logit inverse : des probabilités" else "inverse logit: probabilities"),
            if (identical(r$lien, "log")) (if (fr) "rapports de moyennes" else "ratios of means") else (if (fr) "rapports des cotes" else "odds ratios"))
  } else if (identical(r$echelle, "lien")) {
    sprintf(if (fr) "Elles sont données sur l'échelle du lien %s." else "They are given on the %s link scale.", r$lien)
  } else if (!is.null(forme_y) && !pentes) {
    sprintf(if (fr) "Elles sont sur l'échelle de la réponse modélisée, %s%s."
            else "They are on the scale of the modelled response, %s%s.", forme_y,
            if (!as.character(d$y[[1]]$transform$type) %in% c("log10", "ln")) ""
            else if (fr) " : ramenée sur l'échelle de la variable, une moyenne de logarithmes serait une médiane (une moyenne géométrique), non une moyenne"
            else ": back-transformed to the variable's scale, a mean of logarithms would be a median (a geometric mean), not a mean")
  }
  quelles <- if (pentes) (if (fr) "pentes" else "slopes") else (if (fr) "moyennes" else "means")
  dans <- if (!is.null(r$dans)) sprintf(if (fr) " dans chaque catégorie de %s" else " within each category of %s", guillemets(r$dans$nom)) else ""
  ajustement <- switch(r$reglages$ajustement,
    tukey = { cites <- c(cites, "tukey", "kramer"); sprintf(if (fr) "Toutes les paires de catégories%s ont été comparées, les valeurs p et les intervalles ajustés par la méthode de Tukey (HSD, loi du maximum studentisé ; Tukey, 1953 ; Kramer, 1956) pour le nombre de %s comparées."
                                                         else "Every pair of categories%s was compared, the p-values and intervals adjusted by Tukey's method (HSD, studentized range distribution; Tukey, 1953; Kramer, 1956) for the number of %s compared.", dans, quelles) },
    holm = { cites <- c(cites, "holm"); sprintf(if (fr) "Toutes les paires de catégories%s ont été comparées, les valeurs p ajustées par la méthode de Holm (1979) et les intervalles par celle de Bonferroni, comme le fait emmeans (la méthode de Holm ne donne pas d'intervalles)."
                                             else "Every pair of categories%s was compared, the p-values adjusted by Holm's method (1979) and the intervals by Bonferroni's, as emmeans does (Holm's method gives no intervals).", dans) },
    sprintf(if (fr) "Toutes les paires de catégories%s ont été comparées, sans ajustement pour les comparaisons multiples." else "Every pair of categories%s was compared, without adjustment for multiple comparisons.", dans))
  non <- as.character(unlist(r$non_estimables$moyennes))
  plusieurs <- length(non) > 1
  estimable <- if (length(non)) {
    sprintf(if (fr) "%s de %s, sans observation, %s et %s de côté." else "The %s of %s, without observations, %s and %s left out.",
            if (fr) (if (plusieurs) (if (pentes) "Les pentes" else "Les moyennes") else (if (pentes) "La pente" else "La moyenne")) else (if (plusieurs) quelles else sub("s$", "", quelles)),
            liste(non), if (fr) (if (plusieurs) "ne sont pas estimables" else "n'est pas estimable") else (if (plusieurs) "are not estimable" else "is not estimable"),
            if (fr) (if (plusieurs) "ont été laissées" else "a été laissée") else (if (plusieurs) "were" else "was"))
  }
  list(texte = paste(c(phrase1, ddl, echelle, ajustement, estimable), collapse = " "), cites = cites)
}
