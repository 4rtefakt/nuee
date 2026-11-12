# Analyse de redondance (RDA) et analyse des correspondances (CA), avec l'algèbre linéaire de R,
# comme le paquet vegan les calcule (rda(), cca(), anova.cca(), RsquareAdj(), ordiR2step(),
# varpart()) : mêmes valeurs propres, mêmes scores dans les mêmes échelles, mêmes statistiques et,
# pour les mêmes permutations, mêmes valeurs p. vegan n'est pas livré avec Nuée :
# moteur/tests/verifier_ordination.R le compare quand il est installé.
#
# RDA (Legendre et Legendre 2012, § 11.1) : les réponses Y (des abondances d'espèces, des
# mesures), transformées (Hellinger, corde, log(x + 1)), centrées (ou centrées-réduites) et
# divisées par racine(n - 1). Sans la part des covariables Z (RDA partielle : l'inertie
# conditionnelle, retirée d'abord), leurs valeurs ajustées par la régression multiple sur les
# variables explicatives X donnent les axes contraints (RDA1...) ; leurs résidus, les axes non
# contraints (PC1...).
# CA (§ 9.4) : le tableau des effectifs ou abondances, ses écarts à l'indépendance pondérés
# (p_ij - p_i. p_.j) / racine(p_i. p_.j), décomposés en valeurs singulières.
# Ce sont des analyses de analyses.R : ajuster_rda, resultat_rda, texte_methodes_rda... ; leur
# export vers R est dans ordination_script.R.

# ---------------------------------------------------------------- les données

# Les colonnes d'un bloc de termes dans leurs unités (les transformations de l'onglet Variables
# comprises, une indicatrice 0/1 par catégorie d'une variable qualitative, un terme croisé : le
# produit de ses facteurs) pour des observations ; et pour chaque colonne : son groupe (sa
# variable, toutes ses catégories ensemble, ou les variables d'un terme croisé, « A*B »), son
# bloc (colonne Bloc de l'onglet Variables) et si c'est l'indicatrice d'une catégorie.
colonnes_ordination <- function(projet, termes, observations) {
  if (!length(termes)) {
    return(list(X = matrix(numeric(0), length(observations), 0, dimnames = list(as.character(observations), NULL)),
                groupes = character(0), blocs = character(0), indicatrices = logical(0)))
  }
  bruts <- lapply(termes, function(t) { t$scaling <- list(type = "none"); t })
  b <- construire_bloc(projet, bruts, observations)
  nom_variable <- function(u) as.character(variable_du_projet(projet, u)$ids[1])
  list(X = b$X,
       groupes = vapply(termes, function(t) paste(vapply(t$variables, nom_variable, ""), collapse = "*"), ""),
       blocs = vapply(termes, function(t) {
         k <- t$scaling$block
         if (length(k) && !is.na(k[1]) && nzchar(as.character(k[1]))) as.character(k[1]) else NA_character_
       }, ""),
       indicatrices = vapply(termes, function(t) length(t$variables) == 1 && categorie(t) != 0, TRUE))
}

# Les termes des covariables Z (leurs numéros de variables), avec leurs transformations.
termes_variables <- function(projet, uids, transformations = NULL) {
  unlist(lapply(as.numeric(unlist(uids)), function(u) {
    n <- transformations[[as.character(u)]]
    tr <- if (length(n) && nzchar(as.character(n[1]))) lire_transformation(as.character(n[1]))
    termes_variable(projet, u, "none", tr)
  }), recursive = FALSE)
}

# Les réponses d'une RDA ou le tableau d'une CA : des variables quantitatives seules, dans leurs
# unités (transformations de l'onglet Variables comprises).
reponses_ordination <- function(projet, termes, observations, quoi) {
  for (t in termes) {
    if (length(t$variables) != 1 || categorie(t) != 0) {
      v <- as.character(variable_du_projet(projet, t$variables[1])$ids[1])
      stop(sprintf(traduire("%s : des variables quantitatives seulement (« %s » ne l'est pas).", "%s: quantitative variables only (\"%s\" isn't one)."), quoi, v), call. = FALSE)
    }
  }
  Y <- valeurs_bloc(projet, termes, observations)
  dimnames(Y) <- list(as.character(observations), vapply(termes, function(t) as.character(t$name), ""))
  Y
}

# La transformation des réponses d'une RDA (des abondances) : Hellinger, la racine des
# abondances relatives de chaque observation ; corde, chaque observation ramenée à une
# longueur 1 (Legendre et Gallagher 2001) ; log(x + 1). Une observation sans aucune abondance
# (Hellinger, corde) n'a pas de profil : elle devient manquante.
TRANSFORMATIONS_REPONSES <- c("aucune", "hellinger", "corde", "log1p")
transformer_reponses <- function(Y, methode) {
  if (identical(methode, "aucune")) return(Y)
  if (any(Y < 0, na.rm = TRUE)) {
    stop(traduire("Cette transformation des réponses Y demande des valeurs positives ou nulles (des abondances).",
                  "This transformation of the responses Y needs positive or zero values (abundances)."), call. = FALSE)
  }
  switch(methode,
    hellinger = { s <- rowSums(Y); s[s <= 0] <- NA; sqrt(Y / s) },
    corde = { s <- sqrt(rowSums(Y^2)); s[s <= 0] <- NA; Y / s },
    log1p = log1p(Y))
}

# Les réponses prêtes pour la RDA (vegan : initPCA) : centrées, réduites si demandé (une réponse
# constante vaut alors 0), divisées par racine(n - 1) : leurs carrés font des variances.
reponses_centrees <- function(Y, reduire = FALSE) {
  Y <- scale(Y, center = TRUE, scale = reduire)
  if (reduire && any(is.nan(Y))) Y[is.nan(Y)] <- 0
  Y <- Y / sqrt(nrow(Y) - 1)
  attr(Y, "scaled:center") <- attr(Y, "scaled:scale") <- NULL
  Y
}

# ---------------------------------------------------------------- RDA

# Le cœur d'une RDA (vegan : ordConstrained, ordPartial, ordConstrain, ordResid) : Y prêt
# (reponses_centrees), X et Z dans leurs unités (centrés ici). Les inerties totale,
# conditionnelle, contrainte et résiduelle ; les axes contraints (valeurs propres ; scores des
# observations u, combinaisons linéaires des X, et wa, sommes pondérées des réponses ; scores des
# réponses v ; flèches des X, leurs corrélations aux axes) et résiduels ; pour les tests, E (les
# réponses sans Z) et les décompositions QR de Z et de [Z, X].
rda_noyau <- function(Y, X, Z = NULL) {
  ZERO <- sqrt(.Machine$double.eps)
  n <- nrow(Y)
  totale <- sum(Y^2)
  E <- Y
  QZ <- NULL
  conditionnelle <- 0
  rang_qz <- 0
  zcol <- if (!is.null(Z)) ncol(Z) else 0
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
  alias <- if (ncol(XZ)) colnames(XZ)[Q$pivot[seq_along(Q$pivot) > Q$rank]] else character(0)
  contraint <- NULL
  Eres <- E
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
      bp <- (1 / sqrt(colSums(xx^2))) * crossprod(xx, u)
      dimnames(u) <- dimnames(wa) <- list(rownames(Y), noms)
      dimnames(v) <- list(colnames(Y), noms)
      dimnames(bp) <- list(colnames(xx), noms)
      contraint <- list(eig = stats::setNames(lambda, noms), u = u, v = v, wa = wa, biplot = bp, rang = length(lambda),
                        qrang = rang_x, inertie = sum(lambda))
      Eres <- qr.resid(Q, E)
    }
  }
  s <- svd(Eres)
  lambda <- s$d^2
  nuls <- if (length(lambda)) abs(lambda) < max(ZERO, ZERO * lambda[1]) else logical(0)
  lambda <- lambda[!nuls]
  noms <- sprintf("PC%d", seq_along(lambda))  # (sprintf : aucun nom sans axe)
  u <- s$u[, !nuls, drop = FALSE]
  v <- s$v[, !nuls, drop = FALSE]
  dimnames(u) <- list(rownames(Y), noms)
  dimnames(v) <- list(colnames(Y), noms)
  residuel <- list(eig = stats::setNames(lambda, noms), u = u, v = v, rang = length(lambda), inertie = sum(lambda))
  list(n = n, totale = totale, conditionnelle = conditionnelle, contraint = contraint, residuel = residuel,
       E = E, Q = Q, QZ = QZ, rang_z = if (is.null(QZ)) 0 else QZ$rank, rang_qz = rang_qz, alias = alias,
       Xc = XZ[, zcol + seq_len(ncol(XZ) - zcol), drop = FALSE], Zc = if (!is.null(QZ)) XZ[, seq_len(zcol), drop = FALSE])
}

# Une RDA du même modèle avec une partie de ses colonnes X (`colonnes` : logique ou rangs).
sous_rda <- function(m, colonnes) rda_noyau(m$donnees$Ybar, m$donnees$X[, colonnes, drop = FALSE], m$donnees$Z)

# R² ajusté d'Ezekiel (vegan : RsquareAdj) : sans objet quand le modèle a autant de paramètres
# que d'observations moins une.
r2_ajuste <- function(R2, n, m) {
  r <- 1 - (1 - R2) * (n - 1) / (n - m - 1)
  r[m >= n - 1] <- NA
  r
}

# R² et R² ajusté d'une RDA (vegan : RsquareAdj.rda) ; partielle, celui des X au-delà des Z
# (Peres-Neto et al. 2006).
r2_rda <- function(k) {
  contrainte <- if (is.null(k$contraint)) 0 else k$contraint$inertie
  m <- if (is.null(k$contraint)) 0 else k$contraint$qrang
  R2 <- contrainte / k$totale
  ajuste <- if (k$rang_z == 0) r2_ajuste(R2, k$n, m) else {
    R2p <- k$conditionnelle / k$totale
    r2_ajuste(R2 + R2p, k$n, m + k$rang_z) - r2_ajuste(R2p, k$n, k$rang_z)
  }
  list(R2 = R2, ajuste = ajuste)
}

# ---------------------------------------------------------------- tests par permutation

EPS_PERMUTATIONS <- sqrt(.Machine$double.eps)

# B permutations des n observations, libres ou à l'intérieur des groupes de `strates` (un site,
# une année...), pris dans l'ordre où ils apparaissent ; reproductibles par leur graine (le
# générateur de R par défaut), sans toucher au hasard de la session.
permutations_lignes <- function(n, B, strates = NULL, graine = 1) {
  B <- as.integer(B)
  P <- matrix(0L, B, n)
  if (!B) return(P)
  existait <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  ancien <- if (existait) get(".Random.seed", envir = globalenv())
  genre <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(genre[1], genre[2], genre[3]))  # (le genre de la séance rendu, sans redire ses avertissements)
    if (existait) assign(".Random.seed", ancien, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  })
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  groupes <- if (!is.null(strates)) unname(split(seq_len(n), match(strates, unique(strates))))
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

# Pour chaque permutation (une ligne de P) d'une RDA (`k`, rda_noyau) : ses réponses sans les
# covariables (E) permutées, l'inertie qu'en expliquent les X (ou la première valeur propre de
# cette partie, `premier`) et leur inertie résiduelle (vegan : permutest.cca, modèle « réduit »).
# Dans une RDA partielle, les covariables sont permutées avec les réponses, et les X, rendus
# orthogonaux aux covariables permutées, expliquent ce qu'ils peuvent (vegan : getF.c).
base_orthonormee <- function(Q) qr.Q(Q)[, seq_len(Q$rank), drop = FALSE]
stats_permutees <- function(k, P, premier = FALSE) {
  B <- nrow(P)
  num <- den <- numeric(B)
  partielle <- !is.null(k$QZ)
  Q1 <- if (!partielle) base_orthonormee(k$Q)
  for (b in seq_len(B)) {
    ip <- P[b, ]
    Ep <- k$E[ip, , drop = FALSE]
    Qb <- if (partielle) base_orthonormee(qr(qr.resid(qr(k$Zc[ip, , drop = FALSE]), k$Xc))) else Q1
    M <- crossprod(Qb, Ep)
    ss <- sum(M^2)
    num[b] <- if (!premier) ss else if (length(M)) svd(M, 0, 0)$d[1]^2 else 0
    den[b] <- sum(Ep^2) - ss
  }
  list(num = num, den = den)
}

valeur_p_permutations <- function(Fp, F0) if (length(Fp)) (sum(Fp >= F0 - EPS_PERMUTATIONS) + 1) / (length(Fp) + 1) else NA_real_

# Le test d'une RDA par permutation (vegan : permutest.cca) : pseudo-F de tout le modèle
# (inertie contrainte par degré de liberté sur l'inertie résiduelle par degré de liberté), ou de
# son premier axe (`premier`) ; p = (nombre de F permutés ≥ F + 1) / (permutations + 1).
test_rda <- function(k, P, premier = FALSE) {
  if (is.null(k$contraint)) return(NULL)
  q <- if (premier) 1 else k$contraint$qrang
  r <- k$n - k$Q$rank - 1
  chi_z <- if (premier) unname(k$contraint$eig[1]) else k$contraint$inertie
  chi_xz <- k$residuel$inertie
  F0 <- (chi_z / q) / (chi_xz / r)
  s <- stats_permutees(k, P, premier)
  list(F = F0, p = valeur_p_permutations((s$num / q) / (s$den / r), F0), df = c(q, r), inertie = c(chi_z, chi_xz),
       num = s$num, den = s$den, permutations = nrow(P))
}

# Deux RDA emboîtées (mêmes réponses, mêmes covariables, les X de la petite parmi ceux de la
# grande) : ce que la grande explique de plus, testé par les mêmes permutations des réponses
# sans les covariables (vegan : anova(petit, grand), les tests par terme et la sélection pas à
# pas). `sg` : les statistiques permutées de la grande, si elles sont déjà calculées.
test_emboites <- function(kp, kg, P, sg = NULL) {
  rg <- kg$n - kg$Q$rank - 1
  res_g <- kg$residuel$inertie
  sans_x <- is.null(kp$contraint)
  rp <- if (sans_x) kp$n - kp$rang_qz - 1 else kp$n - kp$Q$rank - 1
  res_p <- if (sans_x) kp$totale - kp$conditionnelle else kp$residuel$inertie
  ddl <- rp - rg
  if (ddl <= 0) return(list(ddl = 0, inertie = 0, F = NA_real_, p = NA_real_))
  inertie <- res_p - res_g
  F0 <- (inertie / ddl) / (res_g / rg)
  sg <- sg %||% stats_permutees(kg, P)
  num_p <- if (sans_x) rep(0, nrow(P)) else stats_permutees(kp, P)$num
  list(ddl = ddl, inertie = inertie, F = F0, p = valeur_p_permutations(((sg$num - num_p) / ddl) / (sg$den / rg), F0))
}

# Les permutations d'une RDA ajustée : ses réglages (nombre, groupes, graine).
permutations_rda <- function(m) permutations_lignes(m$N, m$options$permutations, m$donnees$strates, m$options$graine)

# Les axes contraints testés l'un après l'autre (vegan : anova.cca(by = "axis") ; Legendre et al.
# 2011) : l'axe k est le premier axe du modèle où les k - 1 précédents sont retirés des réponses ;
# les valeurs p ne décroissent pas d'un axe au suivant. La dernière ligne : les résidus.
tests_axes_rda <- function(m, P = permutations_rda(m)) {
  k <- m$noyau
  cc <- k$contraint
  eig <- cc$eig
  ddl_residus <- k$n - length(eig) - k$rang_z - 1
  Fs <- unname(eig) / k$residuel$inertie * ddl_residus  # (vegan : celui des axes qui ne seront pas testés)
  ps <- rep(NA_real_, length(eig))
  for (i in seq_along(eig)) {
    ki <- if (i == 1) k else rda_noyau(qr.resid(qr(cc$u[, seq_len(i - 1), drop = FALSE]), m$donnees$Ybar), cc$u[, -seq_len(i - 1), drop = FALSE], m$donnees$Z)
    tst <- test_rda(ki, P, premier = i < length(eig))
    if (is.null(tst)) break
    Fs[i] <- tst$F
    ps[i] <- if (i > 1) max(tst$p, ps[i - 1]) else tst$p
    if (!isTRUE(ps[i] < 1)) break  # (vegan : après un axe à p = 1, les suivants ne sont pas testés)
  }
  data.frame(axe = c(names(eig), traduire("Résidus", "Residual")), ddl = c(rep(1, length(eig)), ddl_residus),
             inertie = c(unname(eig), k$residuel$inertie), F = c(Fs, NA), p = c(ps, NA), stringsAsFactors = FALSE)
}

# Les variables d'un groupe de termes (« A*B » : A et B), pour la marginalité : un terme qu'un
# terme croisé contient (A et B dans A*B, A dans A*A) n'est pas testé seul et n'entre qu'avant lui.
variables_groupe <- function(g) strsplit(g, "*", fixed = TRUE)[[1]]
contenu_dans <- function(a, b) {  # les variables de a (avec leurs répétitions) sont parmi celles de b, plus nombreuses
  va <- table(variables_groupe(a))
  vb <- table(variables_groupe(b))
  length(variables_groupe(b)) > length(variables_groupe(a)) && all(names(va) %in% names(vb)) && all(va <= vb[names(va)])
}

# Chaque terme (une variable, ses catégories ensemble, ou un terme croisé) testé après tous les
# autres (vegan : anova.cca(by = "margin")) : le modèle sans lui, mêmes permutations ; sauf ceux
# qu'un terme croisé contient. La dernière ligne : les résidus du modèle.
tests_termes_rda <- function(m, P = permutations_rda(m)) {
  k <- m$noyau
  groupes <- unique(m$X$groupes)
  testables <- Filter(function(g) !any(vapply(groupes, function(h) contenu_dans(g, h), TRUE)), groupes)
  sg <- stats_permutees(k, P)
  lignes <- lapply(testables, function(g) {
    t <- test_emboites(sous_rda(m, m$X$groupes != g), k, P, sg)
    data.frame(terme = g, ddl = t$ddl, inertie = t$inertie, F = t$F, p = t$p, stringsAsFactors = FALSE)
  })
  rbind(do.call(rbind, lignes),
        data.frame(terme = traduire("Résidus", "Residual"), ddl = k$n - k$Q$rank - 1, inertie = k$residuel$inertie, F = NA, p = NA, stringsAsFactors = FALSE))
}

# ---------------------------------------------------------------- sélection et partition

# La sélection pas à pas des termes X (vegan : ordiR2step ; Blanchet et al. 2008) : à chaque pas,
# parmi les termes qui peuvent entrer (un terme croisé après ses facteurs), celui qui donne le
# plus grand R² ajusté entre s'il l'augmente sans dépasser celui de tous les termes, et si son
# test (le modèle avec lui contre le modèle sans lui, les covariables gardées) a une valeur p
# d'au plus `seuil`. Les pas, et pourquoi la sélection s'arrête.
selection_rda <- function(m, P = permutations_rda(m), seuil = 0.05) {
  tous <- m$r2_ajuste
  if (is.na(tous)) {
    stop(traduire("Pas de sélection : le R² ajusté de tous les termes n'existe pas (trop de termes pour les observations).",
                  "No selection: the adjusted R² of all the terms doesn't exist (too many terms for the observations)."), call. = FALSE)
  }
  groupes <- unique(m$X$groupes)
  noyau_de <- function(gs) sous_rda(m, m$X$groupes %in% gs)
  r2_de <- function(k) { v <- r2_rda(k)$ajuste; if (!length(v) || is.na(v)) 0 else v }
  dedans <- character(0)
  courant <- noyau_de(dedans)
  precedent <- 0
  etapes <- list()
  arret <- "tous"
  etape <- function(terme, r2, t, retenu) {
    etapes[[length(etapes) + 1]] <<- data.frame(terme = terme, r2_ajuste = r2, F = t$F %||% NA_real_, p = t$p %||% NA_real_, retenu = retenu, stringsAsFactors = FALSE)
  }
  repeat {
    candidats <- Filter(function(g) !g %in% dedans && all(vapply(groupes, function(h) !contenu_dans(h, g) || h %in% dedans, TRUE)), groupes)
    if (!length(candidats)) break
    noyaux <- lapply(candidats, function(g) noyau_de(c(dedans, g)))
    r2s <- vapply(noyaux, r2_de, 0)
    i <- which.max(r2s)
    if (!(r2s[i] > precedent)) { arret <- "r2"; break }
    if (r2s[i] > tous) { etape(candidats[i], r2s[i], list(), FALSE); arret <- "r2_tous"; break }
    t <- test_emboites(courant, noyaux[[i]], P)
    if (is.na(t$p) || t$p > seuil) { etape(candidats[i], r2s[i], t, FALSE); arret <- "p"; break }
    dedans <- c(dedans, candidats[i])
    courant <- noyaux[[i]]
    precedent <- r2s[i]
    etape(candidats[i], r2s[i], t, TRUE)
  }
  list(etapes = if (length(etapes)) do.call(rbind, etapes), retenus = dedans, r2_ajuste_tous = tous, arret = arret, seuil = seuil)
}

# La partition de la variance des réponses entre deux ou trois ensembles de X, les blocs de la
# colonne Bloc (les termes sans bloc ensemble) (vegan : varpart ; Peres-Neto et al. 2006) : les
# R² ajustés de chaque ensemble et de leurs réunions, puis les fractions propres à chacun,
# communes, et résiduelles. Sans covariables Z.
partition_rda <- function(m) {
  if (!is.null(m$donnees$Z) && ncol(m$donnees$Z)) {
    stop(traduire("La partition de la variance se fait sans covariables Z.", "Variance partitioning is done without covariates Z."), call. = FALSE)
  }
  blocs <- ifelse(is.na(m$X$blocs), traduire("(sans bloc)", "(no block)"), m$X$blocs)
  e <- unique(blocs)
  if (length(e) < 2 || length(e) > 3) {
    stop(traduire("La partition de la variance demande deux ou trois ensembles de variables X : donnez-leur un bloc (colonne Bloc de l'onglet Variables).",
                  "Variance partitioning needs two or three sets of X variables: give them a block (Block column of the Variables tab)."), call. = FALSE)
  }
  Y <- m$donnees$Ybar
  n <- nrow(Y)
  ss <- sum(Y^2)
  r2 <- function(es) {  # (vegan : chaque ensemble centré-réduit, rang à 1e-6)
    Xs <- scale(m$donnees$X[, blocs %in% es, drop = FALSE], center = TRUE, scale = TRUE)
    Xs[!is.finite(Xs)] <- 0
    Q <- qr(Xs, tol = 1e-6)
    R2 <- sum(qr.fitted(Q, Y)^2) / ss
    list(ensemble = paste(es, collapse = " + "), ddl = Q$rank, R2 = R2, r2_ajuste = r2_ajuste(R2, n, Q$rank))
  }
  inter <- function(...) paste(c(...), collapse = " ∩ ")
  sachant <- function(a, ...) sprintf("%s | %s", a, paste(c(...), collapse = " + "))
  residus <- traduire("résidus", "residuals")
  if (length(e) == 2) {
    s <- list(r2(e[1]), r2(e[2]), r2(e))
    a <- s[[1]]$r2_ajuste; b <- s[[2]]$r2_ajuste; ab <- s[[3]]$r2_ajuste
    commun <- a + b - ab
    fractions <- data.frame(fraction = c("[a]", "[b]", "[c]", "[d]"), sens = c(sachant(e[1], e[2]), sachant(e[2], e[1]), inter(e), residus),
                            r2_ajuste = c(a - commun, b - commun, commun, 1 - ab), stringsAsFactors = FALSE)
  } else {
    s <- list(r2(e[1]), r2(e[2]), r2(e[3]), r2(e[1:2]), r2(e[c(1, 3)]), r2(e[2:3]), r2(e))
    R <- vapply(s, `[[`, 0, "r2_ajuste")  # X1, X2, X3, X1+X2, X1+X3, X2+X3, tous (vegan : varpart3)
    a <- R[7] - R[6]; b <- R[7] - R[5]; c <- R[7] - R[4]
    d <- R[5] - R[3] - a; ee <- R[4] - R[1] - b; f <- R[6] - R[2] - c
    g <- R[1] - a - d - f
    fractions <- data.frame(fraction = c("[a]", "[b]", "[c]", "[d]", "[e]", "[f]", "[g]", "[h]"),
                            sens = c(sachant(e[1], e[2], e[3]), sachant(e[2], e[1], e[3]), sachant(e[3], e[1], e[2]),
                                     inter(e[1], e[2]), inter(e[2], e[3]), inter(e[1], e[3]), inter(e), residus),
                            r2_ajuste = c(a, b, c, d, ee, f, g, 1 - R[7]), stringsAsFactors = FALSE)
  }
  list(ensembles = do.call(rbind, lapply(s, as.data.frame, stringsAsFactors = FALSE)), fractions = fractions)
}

# ---------------------------------------------------------------- scores

# Les scores d'une RDA dans une échelle (vegan : scores.rda) : 1, les distances entre
# observations (observations × racine(λ / inertie totale), réponses telles quelles) ; 2, les
# corrélations entre réponses (réponses × racine(λ / inertie totale), observations telles
# quelles) ; les unes et les autres × racine(racine((n - 1) × inertie totale)). Axes contraints
# puis résiduels ; les flèches des X et les centroïdes des catégories sont nuls sur les résiduels.
scores_rda <- function(m, echelle = 2) {
  k <- m$noyau
  cc <- k$contraint
  rc <- k$residuel
  eig <- c(cc$eig, rc$eig)
  slam <- sqrt(eig / k$totale)
  const <- sqrt(sqrt((k$n - 1) * k$totale))
  s_obs <- if (echelle == 1) slam else rep(1, length(eig))
  s_rep <- if (echelle == 1) rep(1, length(eig)) else slam
  etendre <- function(M) {
    out <- matrix(0, nrow(M), length(eig), dimnames = list(rownames(M), names(eig)))
    out[, seq_len(ncol(M))] <- M
    out
  }
  echelonner <- function(M, s) sweep(M, 2, s, "*")
  out <- list(valeurs_propres = eig, rang_contraint = cc$rang,
              sites = const * echelonner(cbind(cc$wa, rc$u), s_obs), lc = const * echelonner(cbind(cc$u, rc$u), s_obs),
              reponses = const * echelonner(cbind(cc$v, rc$v), s_rep), biplot = echelonner(etendre(cc$biplot), s_obs))
  cn <- centroides_rda(m)
  if (!is.null(cn)) out$centroides <- const * echelonner(etendre(cn), s_obs)
  out
}

# Les centroïdes des catégories des variables X qualitatives sur les axes contraints : la
# moyenne de leurs scores u (vegan : getCentroids).
centroides_rda <- function(m) {
  cc <- m$noyau$contraint
  if (is.null(cc) || !length(m$facteurs)) return(NULL)
  do.call(rbind, lapply(names(m$facteurs), function(nom) {
    f <- m$facteurs[[nom]]
    M <- do.call(rbind, lapply(levels(f), function(niveau) colMeans(cc$u[f == niveau, , drop = FALSE])))
    rownames(M) <- paste0(nom, "(", levels(f), ")")
    M
  }))
}

# Les scores d'une CA dans une échelle (vegan : scores.cca) : 1, observations × racine(λ)
# (distances du khi² entre observations), variables telles quelles ; 2, l'inverse ; 3, les unes
# et les autres × racine(racine(λ)) (symétrique).
scores_ca <- function(m, echelle = 2) {
  k <- m$noyau
  slam <- sqrt(k$eig)
  un <- rep(1, length(slam))
  lignes <- switch(as.character(echelle), "1" = slam, "2" = un, "3" = sqrt(slam))
  colonnes <- switch(as.character(echelle), "1" = un, "2" = slam, "3" = sqrt(slam))
  list(valeurs_propres = k$eig, sites = sweep(k$u, 2, lignes, "*"), reponses = sweep(k$v, 2, colonnes, "*"))
}

# ---------------------------------------------------------------- ajuster

# Les réglages d'une RDA (d$options), complétés : transformation des réponses, réponses
# réduites ou non, nombre de permutations (0 : aucun test), graine, groupes des permutations
# (list(by = "id", id = ...) ou list(by = "variable", variable = ...)), covariables Z et leurs
# transformations (par numéro de variable).
options_ordination <- function(d) {
  o <- d$options %||% list()
  B <- suppressWarnings(as.integer(o$permutations %||% 999))
  if (is.na(B) || B < 0) B <- 999L
  graine <- suppressWarnings(as.integer(o$graine %||% 1))
  list(transformation = as.character(o$transformation %||% "aucune")[1], reduire = isTRUE(as.logical(o$reduire)),
       permutations = min(B, 99999L), graine = if (is.na(graine)) 1L else graine,
       strates = if (length(o$strates$by)) specs_groupement(o$strates)[[1]],
       z = as.numeric(unlist(o$z %||% numeric(0))), z_transformations = o$z_transformations %||% list())
}

# Une RDA (definir_modele, type "RDA") : réponses Y (rôle Y), variables explicatives X (rôle X),
# covariables Z (d$options$z) ; les observations où une de ces valeurs manque (ou, avec Hellinger
# et corde, sans aucune abondance) sont laissées de côté.
ajuster_rda <- function(projet, d) {
  o <- options_ordination(d)
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  if (!o$transformation %in% TRANSFORMATIONS_REPONSES) stop(sprintf(traduire("Transformation inconnue : %s", "Unknown transformation: %s"), o$transformation), call. = FALSE)
  if (!length(d$y)) err("Une RDA demande des réponses Y.", "An RDA needs responses Y.")
  if (!length(d$x)) err("Une RDA demande des variables explicatives X (sans elles, c'est une ACP).", "An RDA needs explanatory variables X (without them, it is a PCA).")
  if (length(d$spectres)) err("Une RDA ne prend pas de prétraitement de spectres.", "An RDA takes no spectral preprocessing.")
  obs <- as.numeric(d$observations)
  Y <- transformer_reponses(reponses_ordination(projet, d$y, obs, traduire("Les réponses Y d'une RDA", "The responses Y of an RDA")), o$transformation)
  xs <- colonnes_ordination(projet, d$x, obs)
  zs <- colonnes_ordination(projet, termes_variables(projet, o$z, o$z_transformations), obs)
  strates <- if (!is.null(o$strates)) valeurs_groupement(projet, o$strates, obs)
  connu <- stats::complete.cases(Y) & stats::complete.cases(xs$X) & stats::complete.cases(zs$X) & (if (is.null(strates)) TRUE else !is.na(strates))
  garde <- obs[connu]
  n <- length(garde)
  if (n < 3) err("Trop peu d'observations ont toutes leurs valeurs pour cette RDA.", "Too few observations have all their values for this RDA.")
  Y <- Y[connu, , drop = FALSE]
  X <- xs$X[connu, , drop = FALSE]
  Z <- if (ncol(zs$X)) zs$X[connu, , drop = FALSE]
  if (!is.null(strates)) {
    strates <- strates[connu]
    if (all(table(strates) < 2)) err("Les groupes des permutations doivent avoir chacun plusieurs observations.", "The permutation groups must each hold several observations.")
  }
  Ybar <- reponses_centrees(Y, o$reduire)
  if (sum(Ybar^2) <= 0) err("Les réponses Y ne varient pas.", "The responses Y don't vary.")
  k <- rda_noyau(Ybar, X, Z)
  if (is.null(k$contraint)) err("Les variables X n'expliquent rien des réponses (constantes, ou toutes redondantes avec les covariables Z).",
                                "The X variables explain nothing of the responses (constant, or all redundant with the covariates Z).")
  if (k$n - k$Q$rank - 1 < 1) err("Trop de variables X et Z pour le nombre d'observations : il ne reste aucun degré de liberté.",
                                  "Too many X and Z variables for the number of observations: no degree of freedom is left.")
  # les variables X qualitatives, pour les centroïdes de leurs catégories
  facteurs <- list()
  for (t in d$x) {
    if (length(t$variables) != 1 || categorie(t) == 0) next
    v <- variable_du_projet(projet, t$variables[1])
    nom <- as.character(v$ids[1])
    if (is.null(facteurs[[nom]])) facteurs[[nom]] <- droplevels(factor(as.character(v$categories)[colonne(projet, t$variables[1], garde)], levels = as.character(v$categories)))
  }
  m <- list(type = "RDA", observations = garde, exclues = setdiff(obs, garde), N = n,
            Y = list(noms = colnames(Y), uids = vapply(d$y, function(t) as.numeric(t$variables[1]), 0)),
            X = list(noms = colnames(X), groupes = xs$groupes, blocs = xs$blocs, indicatrices = xs$indicatrices),
            Z = list(noms = colnames(zs$X) %||% character(0), uids = o$z), options = o, noyau = k, facteurs = facteurs,
            donnees = list(Ybar = Ybar, X = X, Z = Z, strates = strates))
  r2 <- r2_rda(k)
  m$r2 <- r2$R2
  m$r2_ajuste <- r2$ajuste
  q <- k$contraint$qrang
  r <- k$n - k$Q$rank - 1
  m$test <- list(F = (k$contraint$inertie / q) / (k$residuel$inertie / r), p = NA_real_, df = c(q, r), permutations = o$permutations)
  if (o$permutations > 0) m$test$p <- test_rda(k, permutations_rda(m))$p
  class(m) <- "modele_ordination"
  m
}

# Le cœur d'une CA (vegan : initCA, ordResid) : le tableau N (observations × variables, positif),
# les masses de ses lignes et de ses colonnes, son inertie totale (khi² / effectif total), ses axes
# (coordonnées standard des lignes u et des colonnes v).
ca_noyau <- function(N) {
  ZERO <- sqrt(.Machine$double.eps)
  P <- N / sum(N)
  rw <- rowSums(P)
  cw <- colSums(P)
  rc <- outer(rw, cw)
  S <- (P - rc) / sqrt(rc)
  s <- svd(S)
  lambda <- s$d^2
  nuls <- if (length(lambda)) abs(lambda) < max(ZERO, ZERO * lambda[1]) else logical(0)
  lambda <- lambda[!nuls]
  noms <- sprintf("CA%d", seq_along(lambda))
  u <- sweep(s$u[, !nuls, drop = FALSE], 1, sqrt(rw), "/")
  v <- sweep(s$v[, !nuls, drop = FALSE], 1, sqrt(cw), "/")
  dimnames(u) <- list(rownames(N), noms)
  dimnames(v) <- list(colnames(N), noms)
  list(n = nrow(N), total = sum(N), rw = rw, cw = cw, totale = sum(S^2), eig = stats::setNames(lambda, noms), u = u, v = v)
}

# Contributions (part de chaque axe, de 0 à 1) et qualités de représentation (cos²) des lignes et
# des colonnes d'une CA, d'après leurs coordonnées principales ; et la part de chacune dans
# l'inertie totale.
contributions_ca <- function(k) {
  eig <- k$eig
  F <- sweep(k$u, 2, sqrt(eig), "*")
  G <- sweep(k$v, 2, sqrt(eig), "*")
  ctr <- function(M, w) sweep(M^2 * w, 2, eig, "/")
  cos2 <- function(M) { d2 <- rowSums(M^2); d2[d2 <= 0] <- NA; M^2 / d2 }
  list(lignes = list(contribution = ctr(F, k$rw), cos2 = cos2(F), inertie = k$rw * rowSums(F^2) / k$totale),
       colonnes = list(contribution = ctr(G, k$cw), cos2 = cos2(G), inertie = k$cw * rowSums(G^2) / k$totale))
}

# Une CA (definir_modele, type "CA") : le tableau des variables X (rôle X), des effectifs ou des
# abondances positifs ou nuls. Les observations où une valeur manque, ou sans aucune abondance,
# sont laissées de côté ; les variables sans aucune abondance dans celles qui restent aussi.
ajuster_ca <- function(projet, d) {
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  if (length(d$x) < 2) err("Une analyse des correspondances demande au moins deux variables X (les colonnes du tableau).",
                           "A correspondence analysis needs at least two X variables (the columns of the table).")
  if (length(d$spectres)) err("Une analyse des correspondances ne prend pas de prétraitement de spectres.", "A correspondence analysis takes no spectral preprocessing.")
  obs <- as.numeric(d$observations)
  N <- reponses_ordination(projet, d$x, obs, traduire("Le tableau d'une analyse des correspondances", "The table of a correspondence analysis"))
  if (any(N < 0, na.rm = TRUE)) err("Une analyse des correspondances demande des valeurs positives ou nulles (des effectifs, des abondances).",
                                    "A correspondence analysis needs positive or zero values (counts, abundances).")
  connu <- stats::complete.cases(N)
  connu[connu] <- rowSums(N[connu, , drop = FALSE]) > 0
  garde <- obs[connu]
  N <- N[connu, , drop = FALSE]
  vides <- colnames(N)[colSums(N) <= 0]
  N <- N[, colSums(N) > 0, drop = FALSE]
  if (nrow(N) < 3 || ncol(N) < 2) err("Trop peu d'observations ou de variables non vides pour cette analyse des correspondances.",
                                      "Too few non-empty observations or variables for this correspondence analysis.")
  k <- ca_noyau(N)
  if (!length(k$eig)) err("Le tableau est celui de l'indépendance : aucun axe.", "The table is that of independence: no axis.")
  m <- list(type = "CA", observations = garde, exclues = setdiff(obs, garde), N = length(garde), variables_vides = vides,
            colonnes = colnames(N), noyau = k, contributions = contributions_ca(k), donnees = list(N = N))
  class(m) <- "modele_ordination"
  m
}

print.modele_ordination <- function(x, ...) {
  k <- x$noyau
  if (identical(x$type, "RDA")) {
    cat(sprintf("RDA, %d observations : inertie totale %.4g, conditionnelle %.4g, contrainte %.4g, résiduelle %.4g\n",
                x$N, k$totale, k$conditionnelle, k$contraint$inertie, k$residuel$inertie))
    cat(sprintf("R² %.4f, R² ajusté %.4f ; F = %.4g, p = %s (%d permutations)\n", x$r2, x$r2_ajuste, x$test$F, format(x$test$p, digits = 3), as.integer(x$test$permutations)))
    print(round(utils::head(c(k$contraint$eig, k$residuel$eig), 8), 6))
  } else {
    cat(sprintf("CA, %d observations × %d variables : inertie totale %.4g\n", x$N, length(x$colonnes), k$totale))
    print(round(utils::head(k$eig, 8), 6))
  }
  invisible(x)
}

# ---------------------------------------------------------------- l'interface

# Les axes que l'interface reçoit (les tableaux exportés les ont tous).
AXES_INTERFACE <- 10

colonnes_axes <- function(M, n_axes = AXES_INTERFACE) {
  D <- as.data.frame(M[, seq_len(min(n_axes, ncol(M))), drop = FALSE], check.names = FALSE)
  rownames(D) <- NULL
  D
}

# Les valeurs propres d'une ordination : axe, valeur propre, part de l'inertie totale, cumul,
# contraint ou non.
table_valeurs_propres <- function(eig, totale, rang_contraint = 0) {
  data.frame(axe = names(eig), valeur_propre = unname(eig), part = unname(eig) / totale, cumul = cumsum(unname(eig)) / totale,
             contraint = seq_along(eig) <= rang_contraint, stringsAsFactors = FALSE)
}

# Les flèches d'une RDA à montrer : celles des X quantitatifs (une catégorie a son centroïde).
fleches_rda <- function(m, s) s$biplot[!rownames(s$biplot) %in% m$X$noms[m$X$indicatrices], , drop = FALSE]

# Ce que l'interface reçoit d'une RDA (serveur.R : resultat_modele ; analyses.R).
resultat_rda <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  k <- m$noyau
  echelles <- lapply(1:2, function(i) scores_rda(m, i))
  ordination <- function(s) {
    f <- fleches_rda(m, s)
    list(sites = colonnes_axes(s$sites), lc = colonnes_axes(s$lc), reponses = colonnes_axes(s$reponses), noms_reponses = I(rownames(s$reponses)),
         biplot = colonnes_axes(f), noms_biplot = I(rownames(f)),
         centroides = if (!is.null(s$centroides)) colonnes_axes(s$centroides), noms_centroides = I(rownames(s$centroides) %||% character(0)))
  }
  s2 <- echelles[[2]]
  f2 <- fleches_rda(m, s2)
  noms_obs <- unname(p$noms_obs[as.character(m$observations)])
  vp <- table_valeurs_propres(s2$valeurs_propres, k$totale, k$contraint$rang)
  list(id = id, projet = p$id, nom = d$name, titre = d$title %||% "", type = "RDA", analyse = TRUE, prediction = FALSE, regression = FALSE,
       opls = FALSE, Ap = 0, A = 0, N = m$N, K = ncol(m$donnees$X), noms_y = I(m$Y$noms),
       chiffres = sprintf("R2aj %s · p %s", formatC(m$r2_ajuste, format = "f", digits = 2), if (is.na(m$test$p)) "—" else format(signif(m$test$p, 2))),
       vues_defaut = I(c("ordination", "valeurs_propres")),
       observations = cbind(data.frame(uid = m$observations, nom = noms_obs, stringsAsFactors = FALSE), colonnes_axes(s2$sites)),
       variables = cbind(data.frame(terme = rownames(f2), stringsAsFactors = FALSE), colonnes_axes(f2)),
       variables_y = cbind(data.frame(terme = rownames(s2$reponses), stringsAsFactors = FALSE), colonnes_axes(s2$reponses)),
       axes = I(utils::head(names(s2$valeurs_propres), AXES_INTERFACE)), rang_contraint = k$contraint$rang,
       inertie = list(totale = k$totale, conditionnelle = k$conditionnelle, contrainte = k$contraint$inertie, residuelle = k$residuel$inertie),
       # (les termes redondants laissés de côté, sans la catégorie que chaque variable qualitative perd toujours)
       r2 = m$r2, r2_ajuste = m$r2_ajuste, test = m$test, alias = I(setdiff(k$alias, m$X$noms[m$X$indicatrices])), valeurs_propres = vp,
       ordination = list(`1` = ordination(echelles[[1]]), `2` = ordination(s2)),
       options = m$options[c("transformation", "reduire", "permutations", "graine")],
       strates = if (!is.null(m$options$strates)) nom_groupement(p, m$options$strates), covariables = I(m$Z$noms),
       termes = I(unique(m$X$groupes)), blocs = I(unique(m$X$blocs[!is.na(m$X$blocs)])), sans_bloc = anyNA(m$X$blocs),
       exclues_manquantes = length(m$exclues),
       reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       demande = d$demande %||% list(type = d$type, numero = d$number))
}

# Ce que l'interface reçoit d'une CA.
resultat_ca <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  k <- m$noyau
  ordination <- function(s) list(sites = colonnes_axes(s$sites), reponses = colonnes_axes(s$reponses), noms_reponses = I(rownames(s$reponses)))
  echelles <- lapply(1:3, function(i) scores_ca(m, i))
  s2 <- echelles[[2]]
  ctr <- m$contributions
  noms_obs <- unname(p$noms_obs[as.character(m$observations)])
  parts <- function(cote, noms, masses) {
    c_ <- colonnes_axes(100 * cote$contribution)
    q_ <- colonnes_axes(cote$cos2)
    names(c_) <- paste0("ctr_", names(c_))
    names(q_) <- paste0("cos2_", names(q_))
    cbind(data.frame(nom = noms, masse = unname(masses), inertie = 100 * unname(cote$inertie), stringsAsFactors = FALSE), c_, q_)
  }
  list(id = id, projet = p$id, nom = d$name, titre = d$title %||% "", type = "CA", analyse = TRUE, prediction = FALSE, regression = FALSE,
       opls = FALSE, Ap = 0, A = 0, N = m$N, K = length(m$colonnes), noms_y = I(character(0)),
       chiffres = sprintf("%s %s · CA1 %s", traduire("inertie", "inertia"), formatC(k$totale, format = "f", digits = 3),
                          paste0(formatC(100 * k$eig[1] / k$totale, format = "f", digits = 1), " %")),
       vues_defaut = I(c("ordination", "valeurs_propres")),
       observations = cbind(data.frame(uid = m$observations, nom = noms_obs, stringsAsFactors = FALSE), colonnes_axes(s2$sites)),
       variables = cbind(data.frame(terme = m$colonnes, stringsAsFactors = FALSE), colonnes_axes(s2$reponses)), variables_y = NULL,
       axes = I(utils::head(names(k$eig), AXES_INTERFACE)),
       contributions = list(lignes = parts(ctr$lignes, noms_obs, k$rw), colonnes = parts(ctr$colonnes, m$colonnes, k$cw)),
       inertie = list(totale = k$totale), total = k$total, variables_vides = I(m$variables_vides),
       valeurs_propres = table_valeurs_propres(k$eig, k$totale),
       ordination = stats::setNames(lapply(echelles, ordination), c("1", "2", "3")),
       exclues_manquantes = length(m$exclues),
       reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       demande = d$demande %||% list(type = d$type, numero = d$number))
}

# Une RDA ajustée de la session (une demande de l'interface), et l'enregistrement de ce qu'on y
# calcule à la demande (tests, sélection, partition : gardés avec le modèle, pour les exports).
rda_de <- function(id) {
  e <- modele(id)
  if (!identical(e$definition$type, "RDA")) stop(traduire("Seulement pour une RDA.", "Only for an RDA."), call. = FALSE)
  e$modele
}
garder_rda <- function(id, m) etat$modeles[[id]]$modele <- m
permutations_requises <- function(m) {
  if (!m$options$permutations) stop(traduire("Ces tests demandent des permutations : choisissez-en un nombre (Modifier le modèle).",
                                             "These tests need permutations: choose a number of them (Edit model)."), call. = FALSE)
}

# Les tests d'une RDA par axe et par terme, calculés à la demande (chacun réajuste ses modèles à
# chaque permutation).
demande_analyse_rda_tests <- function(q) {
  m <- rda_de(q$modele)
  if (is.null(m$tests)) {
    permutations_requises(m)
    P <- permutations_rda(m)
    m$tests <- list(axes = tests_axes_rda(m, P), termes = tests_termes_rda(m, P))
    garder_rda(q$modele, m)
  }
  list(modele = q$modele, axes = m$tests$axes, termes = m$tests$termes, permutations = m$options$permutations)
}

demande_analyse_rda_selection <- function(q) {
  m <- rda_de(q$modele)
  if (is.null(m$selection)) {
    permutations_requises(m)
    m$selection <- selection_rda(m)
    garder_rda(q$modele, m)
  }
  s <- m$selection
  list(modele = q$modele, etapes = s$etapes, retenus = I(s$retenus), r2_ajuste_tous = s$r2_ajuste_tous, arret = s$arret, seuil = s$seuil,
       permutations = m$options$permutations)
}

demande_analyse_rda_partition <- function(q) {
  m <- rda_de(q$modele)
  if (is.null(m$partition)) {
    m$partition <- partition_rda(m)
    garder_rda(q$modele, m)
  }
  c(list(modele = q$modele), m$partition)
}

# ---------------------------------------------------------------- Exporter

avec_noms <- function(M, noms, colonne = "Observation") {
  D <- data.frame(noms, as.data.frame(M, check.names = FALSE), check.names = FALSE, stringsAsFactors = FALSE)
  names(D)[1] <- colonne
  rownames(D) <- NULL
  D
}

# Les tableaux d'une RDA (formats.R : tables_resultats) : scores des observations, inertie,
# valeurs propres, test, scores des réponses, flèches des X et centroïdes dans les échelles 1 et
# 2 ; les tests par axe et par terme, la sélection et la partition s'ils ont été calculés.
tables_rda <- function(projet, m) {
  k <- m$noyau
  noms <- unname(projet$noms_obs[as.character(m$observations)])
  s1 <- scores_rda(m, 1)
  s2 <- scores_rda(m, 2)
  avec_lc <- function(s) cbind(avec_noms(s$sites, noms), stats::setNames(as.data.frame(s$lc, row.names = NULL), paste0("lc_", colnames(s$lc))))
  inerties <- c(k$totale, k$conditionnelle, k$contraint$inertie, k$residuel$inertie)
  t <- list(
    observations = avec_lc(s2),
    inertie = data.frame(partie = c("totale", "conditionnelle", "contrainte", "residuelle"), inertie = inerties, part = inerties / k$totale, stringsAsFactors = FALSE),
    valeurs_propres = table_valeurs_propres(s2$valeurs_propres, k$totale, k$contraint$rang),
    test = data.frame(R2 = m$r2, R2_ajuste = m$r2_ajuste, F = m$test$F, ddl_modele = m$test$df[1], ddl_residus = m$test$df[2], p = m$test$p,
                      permutations = m$test$permutations),
    reponses_echelle_2 = avec_noms(s2$reponses, rownames(s2$reponses), "Variable"),
    fleches_echelle_2 = avec_noms(fleches_rda(m, s2), rownames(fleches_rda(m, s2)), "Variable"),
    observations_echelle_1 = avec_lc(s1),
    reponses_echelle_1 = avec_noms(s1$reponses, rownames(s1$reponses), "Variable"),
    fleches_echelle_1 = avec_noms(fleches_rda(m, s1), rownames(fleches_rda(m, s1)), "Variable"))
  if (!is.null(s2$centroides)) {
    t$centroides_echelle_2 <- avec_noms(s2$centroides, rownames(s2$centroides), "Categorie")
    t$centroides_echelle_1 <- avec_noms(s1$centroides, rownames(s1$centroides), "Categorie")
  }
  if (!is.null(m$tests)) {
    t$tests_axes <- m$tests$axes
    t$tests_termes <- m$tests$termes
  }
  if (!is.null(m$selection$etapes)) t$selection <- m$selection$etapes
  if (!is.null(m$partition)) {
    t$partition <- m$partition$fractions
    t$partition_ensembles <- m$partition$ensembles
  }
  t
}

tables_ca <- function(projet, m) {
  k <- m$noyau
  noms <- unname(projet$noms_obs[as.character(m$observations)])
  ctr <- m$contributions
  s <- lapply(1:3, function(i) scores_ca(m, i))
  list(lignes_echelle_2 = avec_noms(s[[2]]$sites, noms), valeurs_propres = table_valeurs_propres(k$eig, k$totale),
       colonnes_echelle_2 = avec_noms(s[[2]]$reponses, m$colonnes, "Variable"),
       lignes_echelle_1 = avec_noms(s[[1]]$sites, noms), colonnes_echelle_1 = avec_noms(s[[1]]$reponses, m$colonnes, "Variable"),
       lignes_echelle_3 = avec_noms(s[[3]]$sites, noms), colonnes_echelle_3 = avec_noms(s[[3]]$reponses, m$colonnes, "Variable"),
       contributions_lignes = cbind(avec_noms(100 * ctr$lignes$contribution, noms), masse = unname(k$rw), inertie = 100 * unname(ctr$lignes$inertie)),
       cos2_lignes = avec_noms(ctr$lignes$cos2, noms),
       contributions_colonnes = cbind(avec_noms(100 * ctr$colonnes$contribution, m$colonnes, "Variable"), masse = unname(k$cw), inertie = 100 * unname(ctr$colonnes$inertie)),
       cos2_colonnes = avec_noms(ctr$colonnes$cos2, m$colonnes, "Variable"))
}

# ---------------------------------------------------------------- texte des méthodes

REFERENCES_ORDINATION <- list(
  legendre = "Legendre, P., & Legendre, L. (2012). Numerical ecology (3rd English ed.). Elsevier.",
  gallagher = "Legendre, P., & Gallagher, E. D. (2001). Ecologically meaningful transformations for ordination of species data. Oecologia, 129(2), 271–280. https://doi.org/10.1007/s004420100716",
  borcard = "Borcard, D., Gillet, F., & Legendre, P. (2018). Numerical ecology with R (2nd ed.). Springer. https://doi.org/10.1007/978-3-319-71404-2",
  terbraak = "ter Braak, C. J. F. (1994). Canonical community ordination. Part I: Basic theory and linear methods. Écoscience, 1(2), 127–140.",
  peresneto = "Peres-Neto, P. R., Legendre, P., Dray, S., & Borcard, D. (2006). Variation partitioning of species data matrices: Estimation and comparison of fractions. Ecology, 87(10), 2614–2625. https://doi.org/10.1890/0012-9658(2006)87[2614:VPOSDM]2.0.CO;2",
  legendre2011 = "Legendre, P., Oksanen, J., & ter Braak, C. J. F. (2011). Testing the significance of canonical axes in redundancy analysis. Methods in Ecology and Evolution, 2(3), 269–277. https://doi.org/10.1111/j.2041-210X.2010.00078.x",
  blanchet = "Blanchet, F. G., Legendre, P., & Borcard, D. (2008). Forward selection of explanatory variables. Ecology, 89(9), 2623–2632. https://doi.org/10.1890/07-0986.1",
  greenacre = "Greenacre, M. (2017). Correspondence analysis in practice (3rd ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781315369983",
  oksanen = "Oksanen, J., Simpson, G. L., Blanchet, F. G., Kindt, R., Legendre, P., Minchin, P. R., et al. (2025). vegan: Community ecology package (R package). https://CRAN.R-project.org/package=vegan")

texte_methodes_rda <- function(projet, d, m, langue = "fr", version = "") texte_methodes_ordination(projet, d, m, langue, version)
texte_methodes_ca <- function(projet, d, m, langue = "fr", version = "") texte_methodes_ordination(projet, d, m, langue, version)

texte_methodes_ordination <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  nombre <- function(x, chiffres = 2) formatC(x, format = "f", digits = chiffres, decimal.mark = tx(",", "."))
  pourcent <- function(x) paste0(formatC(100 * x, format = "f", digits = 1, decimal.mark = tx(",", ".")), tx(" %", "%"))
  liste <- function(x) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), tx("et", "and"), x[length(x)])
  valeur_p <- function(p) if (is.na(p)) "—" else if (p < 0.001) tx("p < 0,001", "p < 0.001") else paste("p =", nombre(p, 3))
  court <- function(x, n = 12) paste0(liste(utils::head(x, n)), if (length(x) > n) "…" else "")
  cites <- "legendre"
  cite <- function(...) cites <<- union(cites, c(...))
  exclues <- if (length(m$exclues)) sprintf(tx(" (%d autres laissées de côté faute de valeurs)", " (%d others left out for missing values)"), length(m$exclues)) else ""
  k <- m$noyau
  if (identical(m$type, "RDA")) {
    o <- m$options
    cite("borcard", "terbraak")
    tr <- switch(o$transformation,
      hellinger = { cite("gallagher"); tx("transformées par Hellinger (racine des abondances relatives ; Legendre et Gallagher, 2001)", "Hellinger-transformed (square root of relative abundances; Legendre and Gallagher, 2001)") },
      corde = { cite("gallagher"); tx("transformées en cordes (chaque observation ramenée à une longueur 1 ; Legendre et Gallagher, 2001)", "chord-transformed (each observation scaled to unit length; Legendre and Gallagher, 2001)") },
      log1p = tx("transformées en log(x + 1)", "log(x + 1)-transformed"), NULL)
    centrage <- if (o$reduire) tx("centrées et réduites (RDA sur les corrélations)", "centred and standardized (correlation RDA)") else tx("centrées (RDA sur les covariances)", "centred (covariance RDA)")
    p1 <- sprintf(tx("Une analyse de redondance (RDA ; Legendre et Legendre, 2012 ; Borcard et al., 2018) a mis en relation %d réponses (%s) avec %d variables explicatives (%s), sur %d observations%s. Les réponses ont été %s.",
                     "A redundancy analysis (RDA; Legendre and Legendre, 2012; Borcard et al., 2018) related %d responses (%s) to %d explanatory variables (%s), on %d observations%s. The responses were %s."),
                  length(m$Y$noms), court(m$Y$noms), length(unique(m$X$groupes)), court(unique(m$X$groupes)), m$N, exclues,
                  paste(c(tr, centrage), collapse = tx(", puis ", ", then ")))
    partielle <- if (length(m$Z$noms)) sprintf(tx("L'effet des covariables (%s) a d'abord été retiré (RDA partielle) : elles expliquent %s de l'inertie totale. ",
                                                  "The effect of the covariates (%s) was removed first (partial RDA): they explain %s of the total inertia. "),
                                               liste(m$Z$noms), pourcent(k$conditionnelle / k$totale)) else ""
    cite("peresneto")
    # (la part des deux premiers axes contraints : de l'inertie expliquée ; un seul axe la porte toute)
    axes <- if (length(k$contraint$eig) > 1) {
      sprintf(tx("les deux premiers axes contraints portent %s de cette inertie expliquée", "the first two constrained axes carry %s of this explained inertia"),
              pourcent(sum(utils::head(k$contraint$eig, 2)) / k$contraint$inertie))
    } else tx("un seul axe contraint la porte", "a single constrained axis carries it")
    p2 <- sprintf(tx("%sLes variables explicatives expliquent %s de l'inertie totale (R² = %s, R² ajusté = %s ; Peres-Neto et al., 2006) ; %s. Les scores sont donnés dans l'échelle 1 (distances entre observations) et 2 (corrélations entre réponses ; ter Braak, 1994).",
                     "%sThe explanatory variables explain %s of the total inertia (R² = %s, adjusted R² = %s; Peres-Neto et al., 2006); %s. Scores are given in scaling 1 (distances between observations) and 2 (correlations between responses; ter Braak, 1994)."),
                  partielle, pourcent(m$r2), nombre(m$r2, 3), nombre(m$r2_ajuste, 3), axes)
    p3 <- if (m$test$permutations > 0) {
      groupes <- if (!is.null(o$strates)) sprintf(tx(" à l'intérieur de chaque %s", " within each %s"), nom_groupement(projet, o$strates)) else ""
      plus <- if (!is.null(m$tests)) {
        cite("legendre2011")
        tx(" ; les axes contraints l'un après l'autre (Legendre et al., 2011) et chaque terme après les autres l'ont été de même",
           "; the constrained axes one after another (Legendre et al., 2011) and each term after the others were tested likewise")
      } else ""
      sprintf(tx("Le modèle a été testé par %d permutations des observations%s (pseudo-F = %s, %s)%s.", "The model was tested with %d permutations of the observations%s (pseudo-F = %s, %s)%s."),
              as.integer(m$test$permutations), groupes, nombre(m$test$F, 2), valeur_p(m$test$p), plus)
    } else ""
    p4 <- if (!is.null(m$selection)) {
      cite("blanchet")
      sprintf(tx("Une sélection pas à pas des variables explicatives (double critère de Blanchet et al., 2008 : R² ajusté et test par permutation au seuil de %s) a retenu %s.",
                 "A forward selection of the explanatory variables (double stopping criterion of Blanchet et al., 2008: adjusted R² and permutation test at %s) kept %s."),
              nombre(m$selection$seuil, 2), if (length(m$selection$retenus)) liste(m$selection$retenus) else tx("aucune variable", "no variable"))
    } else ""
    p5 <- if (!is.null(m$partition)) {
      f <- m$partition$fractions
      ens <- m$partition$ensembles$ensemble[seq_len(if (nrow(f) == 4) 2 else 3)]
      sprintf(tx("La variance des réponses a été partagée entre %s (Peres-Neto et al., 2006 ; Borcard et al., 2018) : %s ; résidus %s (R² ajustés).",
                 "The variance of the responses was partitioned between %s (Peres-Neto et al., 2006; Borcard et al., 2018): %s; residuals %s (adjusted R²)."),
              liste(ens), paste(sprintf("%s %s %s", f$fraction[-nrow(f)], f$sens[-nrow(f)], nombre(f$r2_ajuste[-nrow(f)], 3)), collapse = ", "), nombre(f$r2_ajuste[nrow(f)], 3))
    } else ""
    paragraphes <- c(p1, paste(p2, p3), p4, p5)
  } else {
    cite("greenacre")
    p1 <- sprintf(tx("Une analyse des correspondances (Legendre et Legendre, 2012 ; Greenacre, 2017) a porté sur le tableau de %d observations%s et %d variables (%s)%s.",
                     "A correspondence analysis (Legendre and Legendre, 2012; Greenacre, 2017) was performed on the table of %d observations%s and %d variables (%s)%s."),
                  m$N, exclues, length(m$colonnes), court(m$colonnes),
                  if (length(m$variables_vides)) sprintf(tx(" ; %s, sans aucune valeur, en a été retiré", "; %s, without any value, was left out"), liste(m$variables_vides)) else "")
    eig <- k$eig
    axes <- if (length(eig) > 1) sprintf(tx("les deux premiers axes en portent %s et %s", "the first two axes carry %s and %s of it"), pourcent(eig[1] / k$totale), pourcent(eig[2] / k$totale))
            else tx("un seul axe la porte", "a single axis carries it")
    p2 <- sprintf(tx("L'inertie totale (khi² du tableau divisé par son effectif total) vaut %s ; %s. Les lignes et les colonnes sont représentées dans l'échelle 1 (distances du khi² entre observations), 2 (entre variables) ou 3 (symétrique).",
                     "The total inertia (chi-square of the table divided by its grand total) is %s; %s. Rows and columns are shown in scaling 1 (chi-square distances between observations), 2 (between variables) or 3 (symmetric)."),
                  nombre(k$totale, 4), axes)
    paragraphes <- c(p1, p2)
  }
  annee_r <- R.version$year
  annee_nuee <- format(Sys.Date(), "%Y")
  cite("oksanen", "r", "nuee")
  logiciel <- sprintf(tx("Les calculs ont été faits avec Nuée%s (4rtefakt, %s) et R %s (R Core Team, %s), comme le paquet vegan les fait (Oksanen et al., 2025).",
                         "Computations were done with Nuée%s (4rtefakt, %s) and R %s (R Core Team, %s), as the vegan package does them (Oksanen et al., 2025)."),
                      if (nzchar(version)) paste0(" ", version) else "", annee_nuee, paste(R.version$major, R.version$minor, sep = "."), annee_r)
  references <- vapply(cites, function(c) switch(c,
    r = sprintf(REFERENCES$r, annee_r),
    nuee = sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
    REFERENCES_ORDINATION[[c]]), "")
  list(paragraphes = c(paragraphes[nzchar(paragraphes)], logiciel), references = unname(sort(references)))
}
