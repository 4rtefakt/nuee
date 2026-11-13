# Diagnostics des régressions (regression.R), calculés à la demande sur les modèles ajustés :
# l'autocorrélation des résidus dans le temps, à l'intérieur de chaque groupe (une autocorrélation
# CAR1 est-elle nécessaire ? suffit-elle ?), et les tests du rapport de vraisemblance entre des
# modèles emboîtés (Prédiction › Comparer les modèles), comme anova(m1, m2). Et des résidus que
# l'ajustement calcule : les résidus normalisés d'une autocorrélation CAR1, les résidus quantiles
# randomisés d'un modèle de comptages, de présence-absence ou de valeurs positives.

# ------------------------------------------------------------ autocorrélation des résidus

# L'autocorrélation des résidus `e` le long du temps, à l'intérieur de chaque groupe, les groupes
# mis en commun, comme nlme::ACF (Pinheiro et Bates, 2000, chapitre 5) : au décalage k, la somme des
# produits e[i] e[i + k] des observations à k pas l'une de l'autre dans l'ordre du temps de leur
# groupe, sur le nombre de ces paires, rapportée à la même somme au décalage 0 (les résidus ne
# sont pas recentrés). Un décalage compte des observations successives, pas des unités de temps :
# une année manquante est sautée, comme dans nlme. nlme prend les observations dans l'ordre des
# lignes ; ici, elles sont rangées par temps dans chaque groupe. Décalages 0 à L (par défaut
# min(10, la plus longue série moins 1)), avec la bande ±1,96/√n de chaque décalage, n ses paires,
# celle que trace plot.ACF de nlme (alpha = 0.05).
acf_groupes <- function(e, temps, groupe, L = NULL) {
  groupe <- factor(groupe)
  ordre <- order(groupe, temps)
  series <- split(e[ordre], groupe[ordre], drop = TRUE)
  if (is.null(L)) L <- min(10L, max(lengths(series)) - 1L)
  if (L < 1) stop(traduire("Chaque groupe n'a qu'une observation : pas d'autocorrélation à calculer.",
                           "Each group has a single observation: no autocorrelation to compute."), call. = FALSE)
  produits <- paires <- numeric(L + 1)
  for (s in series) {
    n <- length(s)
    for (k in 0:min(L, n - 1)) {
      produits[k + 1] <- produits[k + 1] + sum(s[seq_len(n - k)] * s[(k + 1):n])
      paires[k + 1] <- paires[k + 1] + n - k
    }
  }
  moyennes <- produits / paires
  list(decalage = 0:L, acf = moyennes / moyennes[1], paires = paires, bande = stats::qnorm(0.975) / sqrt(paires))
}

# Les résidus normalisés d'une autocorrélation CAR1 de paramètre φ : dans chaque groupe, rangé par
# temps, chaque résidu moins ce qu'en prédit le précédent (φ^Δt fois lui), sur l'écart-type de
# cette erreur, √(1 − φ^(2Δt)) ; le premier reste tel quel. C'est e multiplié par l'inverse du
# facteur de Cholesky de la matrice de corrélation φ^|ti − tj| du groupe rangé par temps : les
# résidus de residuals(type = "normalized") de nlme quand les lignes sont rangées par groupe puis
# par temps. nlme 3.1 les calcule dans l'ordre des lignes : faux quand les lignes d'un groupe ne
# se suivent pas (rangées par année, puis par site), pris à rebours quand le temps y décroît.
residus_normalises_car1 <- function(e, temps, groupe, phi) {
  z <- e
  for (i in split(seq_along(e), factor(groupe), drop = TRUE)) {
    if (length(i) < 2) next
    i <- i[order(temps[i])]
    r <- phi^diff(temps[i])
    z[i[-1]] <- (e[i[-1]] - r * e[i[-length(i)]]) / sqrt(1 - r^2)
  }
  z
}

# L'autocorrélation des résidus d'une régression le long du temps, dans chaque groupe
# (acf_groupes), sur l'ajustement en REML, ses résidus conditionnels aux effets aléatoires (le
# niveau le plus fin, comme nlme::ACF). Un modèle avec CAR1 : le temps et les groupes de son
# autocorrélation, et deux séries, ses résidus sans l'autocorrélation (de Pearson, resType =
# "pearson" de nlme ; mgcv::gamm : ceux de sa partie $lme) et ses résidus normalisés
# (residus_normalises_car1), qui n'en montrent plus si CAR1 en rend compte. Sinon `temps` et
# `groupe` disent où la chercher, comme les groupements d'un modèle (valeurs_groupement : un
# identifiant, une variable ; le temps fait de nombres ou de dates, lu par temps_en_nombres ; pour
# `groupe`, une liste de groupements emboîtés, du plus large au plus fin : le site, puis le
# secteur dans le site) ; groupe = list(by = "effets") : les groupes de ses effets aléatoires,
# emboîtés ; NULL : toutes les observations en une série. Une série :
# les résidus de Pearson (lm, nlme), ou ceux de `type` pour un modèle de mgcv ("pearson" : (y −
# ajusté) / √variance ; "deviance" : la part signée de chaque observation dans la déviance).
autocorrelation_residus <- function(projet, m, temps = NULL, groupe = NULL, type = "pearson") {
  a <- m$ajustement
  d <- m$donnees
  niveaux <- sprintf("g%d", seq_along(m$niveaux))  # (sprintf : aucun nom sans effet aléatoire)
  mgcv_seul <- inherits(a, "gam")  # (mgcv::gamm a sa propre classe)
  type <- if (mgcv_seul) match.arg(as.character(type)[1], c("pearson", "deviance")) else "pearson"
  e <- as.vector(if (inherits(a, "gamm")) stats::residuals(a$lme, type = "pearson") else stats::residuals(a, type = type))
  car1 <- isTRUE(m$car1)
  if (car1) {
    t <- d$temps
    g <- interaction(d[c(niveaux, intersect("gc", names(d)))], drop = TRUE)
    nom_temps <- m$temps
    nom_groupe <- m$dans %||% m$niveaux[length(m$niveaux)]
  } else {
    if (is.null(temps)) stop(traduire("Choisissez le temps (une année, une date...) : un identifiant ou une variable.",
                                      "Choose the time (a year, a date...): an ID or a variable."), call. = FALSE)
    nom_temps <- nom_groupement(projet, temps)
    t <- temps_en_nombres(valeurs_groupement(projet, temps, m$observations))  # (des nombres, ou des dates en années décimales)
    if (!is.null(attr(t, "refus"))) stop(refus_temps(t, nom_temps), call. = FALSE)
    nom_groupe <- NULL
    if (is.null(groupe)) {
      g <- rep("1", length(e))
    } else {
      # (des groupements emboîtés, du plus large au plus fin : les effets aléatoires du modèle
      # (by = "effets"), puis au besoin un niveau plus fin, le secteur dans son site)
      if (!is.null(groupe$by)) groupe <- list(groupe)
      effets <- vapply(groupe, function(s) identical(as.character(s$by)[1], "effets"), TRUE)
      if (any(effets) && !length(niveaux)) stop(traduire("Ce modèle n'a pas d'effet aléatoire.", "This model has no random effect."), call. = FALSE)
      morceaux <- unlist(lapply(groupe, function(s) if (identical(as.character(s$by)[1], "effets")) as.list(d[niveaux])
                                else list(valeurs_groupement(projet, s, m$observations))), recursive = FALSE)
      g <- interaction(morceaux, drop = TRUE)
      nom_groupe <- paste(vapply(groupe, function(s) if (identical(as.character(s$by)[1], "effets")) m$niveaux[length(m$niveaux)] else nom_groupement(projet, s), ""),
                          collapse = " / ")
    }
  }
  garde <- !is.na(t) & !is.na(g)
  if (!any(garde)) stop(traduire("Aucune observation du modèle n'a à la fois un temps et un groupe.", "No observation of the model has both a time and a group."), call. = FALSE)
  e <- e[garde]
  t <- t[garde]
  g <- factor(as.character(g[garde]))
  if (anyDuplicated(data.frame(g, t))) {
    stop(traduire("Plusieurs observations ont le même temps dans un même groupe : choisissez des groupes plus fins (le secteur dans son site...).",
                  "Several observations have the same time in the same group: choose finer groups (the sector within its site...)."), call. = FALSE)
  }
  base <- acf_groupes(e, t, g)
  series <- list(list(residus = type, acf = base$acf))
  if (car1) series[[2]] <- list(residus = "normalises", acf = acf_groupes(residus_normalises_car1(e, t, g, m$resultat$phi), t, g, max(base$decalage))$acf)
  list(car1 = car1, phi = if (car1) m$resultat$phi, temps = nom_temps, groupe = nom_groupe, n = length(e), groupes = nlevels(g),
       longueur = max(table(g)), laissees = sum(!garde), decalage = base$decalage, paires = base$paires, bande = base$bande, series = series)
}

# ------------------------------------------------------------ résidus quantiles

# Les résidus quantiles randomisés (Dunn et Smyth, 1996) d'un modèle de mgcv `a` à loi de Poisson,
# binomiale négative (son θ estimé), binomiale (une réponse 0 ou 1 : une épreuve) ou gamma (de forme
# 1 / son échelle estimée) : r = Φ⁻¹(u), F la loi du modèle à la moyenne ajustée de chaque
# observation (ses effets aléatoires et son effort compris) ; u = F(y) pour la loi gamma, et pour
# une loi discrète, u tiré uniformément entre F(y − 1) et F(y). Ils suivent la loi normale quand le
# modèle est juste, ce que ne font pas les résidus de Pearson ou de déviance d'un comptage, même
# d'un modèle juste. Au-delà de 0,5, r se calcule par 1 − u, dans la queue de droite de F : un
# comptage loin au-dessus de sa moyenne garde son résidu au lieu de Φ⁻¹(1) = Inf. Les tirages se
# font avec une graine fixe et le générateur de R par défaut, comme seuils_courbe : les mêmes
# résidus à chaque calcul, le hasard de la session et son générateur rendus. NULL pour une autre
# loi : la normale (ses résidus standardisés le sont déjà), Tweedie (les paquets livrés avec R
# n'ont pas sa fonction de répartition).
residus_quantiles <- function(a, famille, graine = 1) {
  y <- as.vector(a$y)
  mu <- as.vector(stats::fitted(a))
  theta <- if (identical(famille, "binomiale_negative")) a$family$getTheta(TRUE)
  repartition <- switch(famille,  # (F ; 1 − F avec lower.tail = FALSE)
    poisson = function(q, ...) stats::ppois(q, mu, ...),
    binomiale_negative = function(q, ...) stats::pnbinom(q, size = theta, mu = mu, ...),
    binomiale = function(q, ...) stats::pbinom(q, 1, mu, ...),
    gamma = function(q, ...) stats::pgamma(q, shape = 1 / a$sig2, rate = 1 / (a$sig2 * mu), ...),
    return(NULL))
  discrete <- !identical(famille, "gamma")
  y0 <- if (discrete) y - 1 else y
  F0 <- repartition(y0)                      # F(y − 1)
  F1 <- repartition(y)                       # F(y)
  S0 <- repartition(y0, lower.tail = FALSE)  # 1 − F(y − 1)
  S1 <- repartition(y, lower.tail = FALSE)   # 1 − F(y)
  v <- 0
  if (discrete) {
    existait <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    ancien <- if (existait) get(".Random.seed", envir = globalenv())
    genre <- RNGkind()
    on.exit({
      suppressWarnings(RNGkind(genre[1], genre[2], genre[3]))  # (le genre de la séance rendu, sans redire ses avertissements)
      if (existait) assign(".Random.seed", ancien, envir = globalenv())
      else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    })
    set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    v <- stats::runif(length(y))
  }
  u <- F0 + v * (F1 - F0)
  ifelse(u <= 0.5, stats::qnorm(u), stats::qnorm(S0 - v * (S0 - S1), lower.tail = FALSE))
}

# ------------------------------------------------- tests du rapport de vraisemblance

# Le tableau des effets fixes d'une régression sur ses observations (dans l'ordre de
# m$observations) : la constante et ses termes utiles (lm, nlme), ou les colonnes de son
# ajustement de mgcv en ML sans celles des effets aléatoires (ses termes, la base de chaque courbe).
effets_fixes_regression <- function(m) {
  a <- m$ajustement_ml
  if (inherits(a, "gamm")) a <- a$gam
  if (!inherits(a, "gam")) return(cbind(1, as.matrix(m$donnees[m$utiles])))
  X <- stats::predict(a, m$donnees, type = "lpmatrix")
  X[, !grepl("^s\\(g(1|12|123)\\)\\.", colnames(X)), drop = FALSE]
}

# Les colonnes de A dans l'espace engendré par celles de B, aux erreurs d'arrondi près.
dans_espace <- function(A, B) {
  r <- qr.resid(qr(B, tol = 1e-9), A)
  all(sqrt(colSums(r^2)) <= 1e-7 * sqrt(colSums(A^2)))
}

# Deux facteurs (alignés) qui font les mêmes groupes, quels que soient leurs noms.
memes_groupes <- function(a, b) {
  a <- factor(a)
  b <- factor(b)
  nlevels(a) == nlevels(b) && nlevels(interaction(a, b, drop = TRUE)) == nlevels(a)
}

# Deux régressions du même ensemble AIC qui ne diffèrent, au plus, que par leurs effets fixes : la
# même loi, la même réponse, le même effort, les mêmes groupes à chaque niveau d'effet aléatoire,
# la même autocorrélation (le même temps, les mêmes groupes). k : les lignes de mb dans l'ordre de ma.
memes_aleatoires <- function(ma, mb, k) {
  egaux <- function(x, y) isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = 1e-10))
  if (!identical(ma$famille, mb$famille) || !identical(ma$decalage, mb$decalage) || !identical(isTRUE(ma$car1), isTRUE(mb$car1)) ||
      length(ma$niveaux) != length(mb$niveaux)) return(FALSE)
  da <- ma$donnees
  db <- mb$donnees[k, , drop = FALSE]
  if (!egaux(da$y, db$y) || (!is.null(ma$decalage) && !egaux(da$decalage, db$decalage))) return(FALSE)
  niveaux <- sprintf("g%d", seq_along(ma$niveaux))
  groupes <- function(x, colonnes) interaction(x[colonnes], drop = TRUE)
  for (l in seq_along(niveaux)) if (!memes_groupes(groupes(da, niveaux[1:l]), groupes(db, niveaux[1:l]))) return(FALSE)
  if (!isTRUE(ma$car1)) return(TRUE)
  egaux(da$temps, db$temps) && memes_groupes(groupes(da, c(niveaux, intersect("gc", names(da)))), groupes(db, c(niveaux, intersect("gc", names(db)))))
}

# Un modèle dont la vraisemblance en ML a des edf plutôt que des paramètres : des courbes, ou des
# effets aléatoires de mgcv (des termes pénalisés).
penalise_regression <- function(m) length(m$lisses) > 0 || (inherits(m$ajustement, "gam") && length(m$niveaux) > 0)

# Les tests du rapport de vraisemblance entre les régressions emboîtées de `modeles` (une liste
# nommée par leurs identifiants, des modèles d'ajuster_regression), comme anova(m1, m2) de nlme
# sur des ajustements en ML, ou les écarts de logLik de lm() et glm() : chaque paire d'un même
# ensemble AIC (même réponse, mêmes observations, même vraisemblance : signature) qui ne diffère
# que par ses effets fixes (memes_aleatoires), les termes fixes du petit modèle étant parmi ceux du
# grand (son tableau dans l'espace de celui du grand). χ² = 2 (logLik du grand − logLik du petit)
# de leurs ajustements en ML, ddl = la différence de leurs nombres de paramètres, p = P(χ²(ddl) >
# χ²). (nlme prend la valeur absolue de l'écart ; ici un grand modèle qui finit sous le petit, ce
# qui n'arrive que quand son optimisation s'est arrêtée trop tôt, donne χ² = 0.) Les effets
# aléatoires et l'autocorrélation ne se testent pas ainsi (memes_aleatoires) : une variance ou un φ
# nuls sont au bord de leur domaine, où la loi du χ² ne tient plus (Pinheiro et Bates, 2000, 2.4) ;
# ils se comparent par l'AIC. Avec des courbes ou des effets aléatoires de mgcv, les ddl sont des
# edf et le test est approché (Wood, 2017) : `approche`. Au plus `max_paires` paires, par ensemble
# puis dans l'ordre des modèles ; `total` : combien il y en a.
rapports_vraisemblance <- function(modeles, max_paires = 2000) {
  modeles <- Filter(function(m) inherits(m, "modele_regression") && isTRUE(is.finite(m$resultat$logLik)), modeles)
  ids <- names(modeles)
  signatures <- vapply(modeles, function(m) as.character(m$signature), "")
  fixes <- new.env()
  tableau <- function(id) {
    if (is.null(fixes[[id]])) assign(id, effets_fixes_regression(modeles[[id]]), envir = fixes)
    fixes[[id]]
  }
  lignes <- list()
  for (s in unique(signatures)) {
    ici <- ids[signatures == s]
    for (i in seq_along(ici)[-length(ici)]) for (j in (i + 1):length(ici)) {
      ma <- modeles[[ici[i]]]
      mb <- modeles[[ici[j]]]
      k <- match(ma$observations, mb$observations)
      if (anyNA(k) || length(k) != length(mb$observations) || !memes_aleatoires(ma, mb, k)) next
      Xa <- tableau(ici[i])
      Xb <- tableau(ici[j])[k, , drop = FALSE]
      a_dans_b <- dans_espace(Xa, Xb)
      if (a_dans_b == dans_espace(Xb, Xa)) next  # (pas emboîtés, ou le même modèle)
      grand <- if (a_dans_b) mb else ma
      petit <- if (a_dans_b) ma else mb
      ddl <- grand$resultat$parametres - petit$resultat$parametres
      khi2 <- max(0, 2 * (grand$resultat$logLik - petit$resultat$logLik))
      lignes[[length(lignes) + 1]] <- data.frame(grand = ici[if (a_dans_b) j else i], petit = ici[if (a_dans_b) i else j], ddl = ddl, khi2 = khi2,
                                                 p = if (ddl > 0) stats::pchisq(khi2, ddl, lower.tail = FALSE) else NA_real_,
                                                 approche = penalise_regression(grand) || penalise_regression(petit), stringsAsFactors = FALSE)
    }
  }
  paires <- do.call(rbind, lignes)
  total <- if (is.null(paires)) 0L else nrow(paires)
  list(paires = if (total) paires[seq_len(min(total, max_paires)), , drop = FALSE] else data.frame(), total = total, max = max_paires)
}
