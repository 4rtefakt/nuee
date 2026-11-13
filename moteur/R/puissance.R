# La puissance pour détecter une tendance (Graphiques › Puissance pour une tendance), une question
# de suivi : avec tant de sites comptés chaque année, en combien d'années une baisse de 3 % par an
# est-elle détectée ? Pour une régression linéaire (lm) ou un modèle linéaire mixte (nlme::lme,
# avec ou sans CAR1) qui a un temps parmi ses X quantitatifs (l'année), avec les variances qu'il a
# estimées (REML).
#
# Un plan : un nombre d'années, un comptage par unité de temps, et un nombre d'unités du premier
# effet aléatoire (les sites), chacune comptée chaque année, ses niveaux emboîtés comme dans les
# données (les secteurs d'un site : leur nombre moyen, arrondi). Une régression linéaire n'a pas
# d'unités : ses plans comptent des observations par année. Les autres termes du modèle restent
# hors des plans (leurs effets tenus pour constants) : la tendance seule, sur les variances du
# modèle, qui sont celles qui restent une fois ces termes pris en compte.
#
# La puissance analytique : la variance de l'estimateur des moindres carrés généralisés (GLS) de
# la pente, les variances tenues pour connues, [(X' V⁻¹ X)⁻¹] de la pente, V la covariance du plan
# (Diggle et al., 2002, 2.4) ; puissance = Φ(|β|/et − z) + Φ(−|β|/et − z), z = Φ⁻¹(1 − α/2).
# Et une vérification par simulation à un plan (Johnson et al., 2015) : des jeux de données tirés
# du modèle avec la tendance, chacun réajusté par nlme (REML, la même structure), la puissance
# étant la part des pentes de p ≤ α.

# Le modèle `m` (de définition `d`) tel que la puissance le lit : ses variances (REML), son
# autocorrélation, ses niveaux d'effets aléatoires et les X qui peuvent être le temps. Refuse ce
# qu'elle ne couvre pas, en disant pourquoi : un modèle généralisé ou additif (sa loi n'est pas
# normale, ou mgcv l'ajuste), des pentes aléatoires (une matrice de variance de plus d'une ligne à
# un niveau), pas de X quantitatif (non transformé) qui puisse être le temps.
lire_modele_puissance <- function(m, d) {
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  a <- m$ajustement
  if (!isTRUE(m$type %in% c("LM", "LMM")) || !inherits(a, c("lm", "lme"))) {
    err("La puissance pour une tendance se calcule pour une régression linéaire ou un modèle linéaire mixte (nlme), à réponse normale : pas pour un modèle généralisé ou additif (GLM, GAM), dont la loi et l'ajustement par mgcv ne sont pas couverts.",
        "Trend power is computed for a linear regression or a linear mixed model (nlme), with a normal response: not for a generalized or additive model (GLM, GAM), whose distribution and mgcv fit aren't covered.")
  }
  L <- length(m$niveaux)
  niveaux <- paste0("g", seq_len(L))
  if (inherits(a, "lme") && !all(vapply(a$modelStruct$reStruct, function(x) identical(dim(as.matrix(x)), c(1L, 1L)), TRUE))) {
    err("Ce modèle a des pentes aléatoires : la puissance pour une tendance ne couvre que des ordonnées à l'origine aléatoires.",
        "This model has random slopes: trend power covers random intercepts only.")
  }
  # les X qui peuvent être le temps : quantitatifs, utiles, sans transformation (une tendance est une pente le long du temps lui-même)
  termes <- termes_regression(d$x)
  candidats <- Filter(function(t) is.null(nom_transformation(termes[[match(t, m$termes)]]$transform)), intersect(m$quantitatifs, m$utiles))
  if (!length(candidats)) {
    err("Ce modèle n'a pas de temps parmi ses X quantitatifs (non transformés) : la tendance est la pente d'un X, l'année. Ajoutez-la comme variable X.",
        "This model has no time among its quantitative X (untransformed): the trend is the slope of an X, the year. Add it as an X variable.")
  }
  sigma2 <- if (inherits(a, "lme")) a$sigma^2 else summary(a)$sigma^2
  list(L = L, candidats = candidats, noms = m$noms_termes[match(candidats, m$termes)], sigma2 = sigma2,
       # (σ² fois la matrice relative de chaque niveau, nommé g1, g2... dans nlme ; comme resultat_gamm)
       variances = if (L) vapply(niveaux, function(n) sigma2 * as.matrix(a$modelStruct$reStruct[[n]])[1, 1], 0, USE.NAMES = FALSE) else numeric(0),
       car1 = isTRUE(m$car1), phi = if (isTRUE(m$car1)) unname(stats::coef(a$modelStruct$corStruct, unconstrained = FALSE)) else 0)
}

# Le plan des données pour le temps `x` (ses valeurs brutes, une par observation du modèle) : le
# nombre d'années qu'elles couvrent (de la première à la dernière, l'année comprise), le nombre
# d'unités du premier effet aléatoire (une régression linéaire : d'observations par année), le
# nombre moyen d'unités de chaque niveau emboîté dans celle du niveau d'avant et de groupes de
# l'autocorrélation (gc) dans l'unité la plus fine, et le nombre moyen d'observations par série et
# par année (sans CAR1 ; une avec). Les plans prennent ces moyennes arrondies (au moins 1) ; une
# série est le groupe où court CAR1, ou l'unité la plus fine.
plan_donnees_puissance <- function(m, lu, x) {
  d <- m$donnees
  L <- lu$L
  distincts <- function(colonnes) nlevels(interaction(d[colonnes], drop = TRUE))
  annees <- round(max(x) - min(x)) + 1
  niveaux <- paste0("g", seq_len(L))
  moyennes <- if (L >= 2) vapply(2:L, function(l) distincts(niveaux[1:l]) / distincts(niveaux[1:(l - 1)]), 0) else numeric(0)
  dans <- if (L && "gc" %in% names(d)) distincts(c(niveaux, "gc")) / distincts(niveaux) else NA_real_
  # (des répétitions : les observations d'une série la même année, l'année étant la partie entière du temps)
  serie <- if (L) interaction(d[c(niveaux, intersect("gc", names(d)))], drop = TRUE)
  repetitions <- if (L && !lu$car1) nrow(d) / nlevels(interaction(serie, floor(x), drop = TRUE)) else 1
  arrondi <- function(v) as.integer(pmax(1, round(v)))
  emboites <- arrondi(moyennes)
  list(annees = as.integer(annees), N = nrow(d),
       unites = if (L) nlevels(droplevels(factor(d$g1))) else max(1L, as.integer(round(nrow(d) / annees))),
       moyennes = moyennes, emboites = emboites, moyenne_dans = dans, dans = if (is.na(dans)) 0L else arrondi(dans),
       moyenne_repetitions = repetitions, repetitions = arrondi(repetitions),
       series_par_unite = as.integer(prod(emboites) * (if (is.na(dans)) 1 else arrondi(dans))))
}

# La pente à détecter sur l'échelle de la réponse du modèle : `tendance` elle-même (unite =
# "pente", dans les unités de Y par unité de temps), ou un changement de `tendance` % par an (unite
# = "pourcent") quand Y est en log10 ou en ln : log(1 + tendance / 100) dans la base de la
# transformation, une pente constante pour log(a·y) ; pour log(a·y + c), c ≠ 0, un changement de
# a·y + c, proche de celui de y quand a·y est grand devant c (`approchee`).
pente_puissance <- function(tendance, unite, transformation) {
  base <- log_reponse(transformation)
  if (identical(unite, "pourcent")) {
    if (is.null(base)) stop(traduire("Une tendance en % par an demande une réponse en log10 ou en ln (Nouveau modèle › Variables).",
                                     "A trend in % per year needs a log10 or ln response (New model › Variables)."), call. = FALSE)
    if (!(tendance > -100)) stop(traduire("Une baisse de 100 % ou plus n'a pas de logarithme.", "A decline of 100 % or more has no logarithm."), call. = FALSE)
    return(list(pente = if (base$type == "log10") log10(1 + tendance / 100) else log(1 + tendance / 100), approchee = base$decalage != 0))
  }
  list(pente = tendance, approchee = FALSE)
}

# La transformation log10 ou ln de la réponse (log(a·y + c), a > 0) : son type et son décalage c ;
# NULL pour une autre.
log_reponse <- function(transformation) {
  if (is.null(transformation)) return(NULL)
  p <- as.numeric(transformation$params)
  type <- as.character(transformation$type)
  if (!type %in% c("log10", "ln") || !isTRUE(p[2] > 0)) return(NULL)
  list(type = type, decalage = p[3])
}

# Le facteur de la variance de la pente d'une série de `annees` comptages réguliers (temps 1, 2...)
# : [(X' R⁻¹ X)⁻¹] de la pente, X = (1, temps), R la corrélation CAR1 de la série, φ^|ti − tj| (φ = 0
# : indépendants). R⁻¹ par le blanchiment des colonnes de X, comme residus_normalises_car1 blanchit
# des résidus (regression_diagnostics.R) : (X' R⁻¹ X) = X̃' X̃. Sans CAR1, 1 / Σ(t − t̄)².
facteur_pente <- function(annees, phi = 0) {
  t <- seq_len(annees)
  X <- cbind(1, t)
  if (phi > 0) X <- apply(X, 2, residus_normalises_car1, temps = t, groupe = rep(1L, annees), phi = phi)
  solve(crossprod(X))[2, 2]
}

# La variance de la pente d'un plan : `unites` unités, chacune de `series` séries de `repetitions`
# observations par année, sur `annees` ans, les variances connues. C'est [(X' V⁻¹ X)⁻¹] de la pente
# pour V, la covariance de tout le plan (ordonnées à l'origine aléatoires, CAR1 dans chaque série) :
# chaque unité étant comptée chaque année, une ordonnée à l'origine aléatoire ajoute à V un terme
# de l'espace de la colonne de la constante de X, qui ne change ni l'estimateur GLS de la pente ni
# sa variance (Diggle et al., 2002, 2.4 : avec une corrélation uniforme, seule la variance
# résiduelle compte). Il reste σ² [(X' R⁻¹ X)⁻¹] de la pente d'une série, sur le nombre de séries.
# verifier_resultats.R la compare à (X' V⁻¹ X)⁻¹ calculé sur le V de tout le plan.
variance_pente <- function(sigma2, phi, annees, unites, series = 1L, repetitions = 1L) {
  sigma2 * facteur_pente(annees, phi) / (unites * series * repetitions)
}

# La puissance d'un test bilatéral de la pente au seuil `alpha` par la loi normale, la pente vraie
# étant `pente` et son erreur-type `erreur_type` : P(|β̂ / et| > z), z = Φ⁻¹(1 − α/2).
puissance_normale <- function(pente, erreur_type, alpha = 0.05) {
  z <- stats::qnorm(1 - alpha / 2)
  stats::pnorm(abs(pente) / erreur_type - z) + stats::pnorm(-abs(pente) / erreur_type - z)
}

# ------------------------------------------------------------ simulation

# Les lignes d'un plan simulé : `unites` unités (g1), chacune avec `emboites` unités à chaque
# niveau emboîté (g2, g3), `dans` groupes de l'autocorrélation (gc) dans l'unité la plus fine, et
# `repetitions` observations par série et par année, aux temps 1 à `annees` ; rangées par unité,
# puis par série, puis par temps. Les unités sont numérotées dans cet ordre (u1, u2... : le numéro
# de chaque unité de chaque niveau, dans l'ordre où elle vient, pour tirer ses effets).
cadre_simulation <- function(L, emboites, dans, repetitions, annees, unites) {
  g <- c(list(temps = seq_len(annees), repetition = seq_len(repetitions)), if (dans > 0) list(gc = seq_len(dans)),
         stats::setNames(lapply(rev(seq_len(max(L, 1))[-1]), function(l) seq_len(emboites[l - 1])), if (L >= 2) paste0("g", rev(2:L))),
         list(g1 = seq_len(unites)))
  cadre <- expand.grid(g, KEEP.OUT.ATTRS = FALSE)
  cadre <- cadre[c(paste0("g", seq_len(max(L, 1))), intersect("gc", names(cadre)), "repetition", "temps")]
  for (l in seq_len(max(L, 1))) cadre[[paste0("u", l)]] <- match(do.call(paste, cadre[paste0("g", seq_len(l))]), unique(do.call(paste, cadre[paste0("g", seq_len(l))])))
  for (n in intersect(c(paste0("g", seq_len(max(L, 1))), "gc"), names(cadre))) cadre[[n]] <- factor(cadre[[n]])
  cadre
}

# Un jeu de données tiré du modèle avec la tendance : y = pente × temps + les effets aléatoires de
# chaque niveau (σ_l z, un tirage par unité, niveau après niveau) + les erreurs (σ z, un tirage par
# ligne ; avec CAR1, dans chaque série, rangée par temps, e1 = σ z1 et et = φ et−1 + σ √(1 − φ²) zt,
# l'autocorrélation φ^|ti − tj|). La constante, sans effet sur le test de la pente, vaut 0. Les
# tirages se font dans cet ordre, de rnorm() multiplié par l'écart-type (qui tire même quand il
# est nul) : le script exporté refait les mêmes.
tirer_simulation <- function(cadre, L, variances, sigma2, phi, pente) {
  y <- pente * cadre$temps
  for (l in seq_len(L)) y <- y + sqrt(variances[l]) * stats::rnorm(max(cadre[[paste0("u", l)]]))[cadre[[paste0("u", l)]]]
  e <- sqrt(sigma2) * stats::rnorm(nrow(cadre))
  if (phi > 0) for (t in seq_len(max(cadre$temps))[-1]) {
    i <- which(cadre$temps == t)
    e[i] <- phi * e[i - 1] + sqrt(1 - phi^2) * e[i]
  }
  y + e
}

# Le p de la pente d'un jeu simulé, réajusté avec la structure du modèle : lm ; nlme::lme en REML,
# ses ordonnées à l'origine aléatoires emboîtées, CAR1 dans chaque série, l'optimisation par optim
# puis nlminb, comme ajuster_lineaire ajuste les modèles de Nuée. NA quand l'ajustement échoue.
p_simulation <- function(cadre, L, car1) {
  if (!L) {
    a <- tryCatch(stats::lm(y ~ temps, data = cadre), error = function(e) NULL)
    return(if (is.null(a)) NA_real_ else unname(summary(a)$coefficients["temps", 4]))
  }
  niveaux <- paste0("g", seq_len(L))
  aleatoire <- stats::as.formula(paste("~ 1 |", paste(niveaux, collapse = " / ")), env = baseenv())
  correlation <- if (car1) nlme::corCAR1(form = stats::as.formula(paste("~ temps |", paste(c(niveaux, intersect("gc", names(cadre))), collapse = " / ")), env = baseenv()))
  essai <- function(reglages) tryCatch(nlme::lme(y ~ temps, random = aleatoire, correlation = correlation, data = cadre, method = "REML", control = reglages),
                                       error = function(e) NULL)
  a <- essai(nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE)) %||%
    essai(nlme::lmeControl(maxIter = 500, msMaxIter = 500, returnObject = TRUE))
  if (is.null(a)) NA_real_ else unname(summary(a)$tTable["temps", "p-value"])
}

# La simulation à un plan (`annees`, `unites`) : `n` jeux de données tirés du modèle (lu :
# lire_modele_puissance ; pd : plan_donnees_puissance) avec la tendance `pente`, chacun réajusté
# (p_simulation) ; la puissance est la part des pentes de p ≤ `alpha` parmi les ajustements réussis,
# avec son erreur de Monte-Carlo, √(P(1 − P) / n). Une graine fixe et le générateur de R par
# défaut, comme residus_quantiles : les mêmes jeux à chaque calcul, et dans le script exporté ; le
# hasard de la session, et son générateur, sont rendus. `duree` : le temps du calcul (secondes).
simuler_puissance <- function(lu, pd, pente, alpha, annees, unites, n, graine = 1) {
  cadre <- cadre_simulation(lu$L, pd$emboites, pd$dans, pd$repetitions, annees, unites)
  existait <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  ancien <- if (existait) get(".Random.seed", envir = globalenv())
  genre <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(genre[1], genre[2], genre[3]))  # (le genre de la séance rendu, sans redire ses avertissements)
    if (existait) assign(".Random.seed", ancien, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  })
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  debut <- proc.time()[["elapsed"]]
  p <- vapply(seq_len(n), function(k) {
    cadre$y <- tirer_simulation(cadre, lu$L, lu$variances, lu$sigma2, lu$phi, pente)
    p_simulation(cadre, lu$L, lu$car1)
  }, 0)
  ok <- !is.na(p)
  puissance <- if (any(ok)) mean(p[ok] <= alpha) else NA_real_
  list(annees = as.integer(annees), unites = as.integer(unites), n = as.integer(n), graine = graine, alpha = alpha, observations = nrow(cadre),
       puissance = puissance, erreur_mc = if (any(ok)) sqrt(puissance * (1 - puissance) / sum(ok)) else NA_real_, echecs = sum(!ok),
       duree = proc.time()[["elapsed"]] - debut, p = p)
}

# Le temps d'un ajustement à ce plan (secondes) : un jeu tiré et réajusté une fois, le hasard de la
# session rendu, pour dire combien `n` jeux prendront (le moteur R calcule un ajustement à la fois).
duree_ajustement <- function(lu, pd, pente, annees, unites) {
  cadre <- cadre_simulation(lu$L, pd$emboites, pd$dans, pd$repetitions, annees, unites)
  existait <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  ancien <- if (existait) get(".Random.seed", envir = globalenv())
  genre <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(genre[1], genre[2], genre[3]))
    if (existait) assign(".Random.seed", ancien, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  })
  set.seed(2)
  cadre$y <- tirer_simulation(cadre, lu$L, lu$variances, lu$sigma2, lu$phi, pente)
  debut <- proc.time()[["elapsed"]]
  p_simulation(cadre, lu$L, lu$car1)
  max(proc.time()[["elapsed"]] - debut, 0.001)
}

# ------------------------------------------------------------ le calcul demandé

# Les limites : les plans des courbes, et ceux d'une simulation (le moteur R calcule un ajustement
# à la fois : une simulation qui prendrait plus d'un quart d'heure est refusée).
PUISSANCE_LIMITES <- list(annees = c(3L, 100L), unites = 10000L, courbes = 10L, simulations = c(10L, 2000L), observations = 20000L, duree = 900)

# Les réglages de la puissance (ceux de l'interface, sinon ceux de la dernière fois, sinon ceux par
# défaut), vérifiés : le terme du temps (son nom), la tendance et son unité ("pente" ou
# "pourcent"), alpha, les années des courbes (de, à), les nombres d'unités (celui des données y est
# toujours), le plan et le nombre de jeux de la simulation.
reglages_puissance <- function(m, d, lu, q) {
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  nombre <- function(v, defaut) { v <- suppressWarnings(as.numeric(unlist(v))); if (length(v) && !anyNA(v)) v else defaut }
  # le temps : celui demandé, ou un X nommé comme un temps, ou celui de CAR1, ou le premier
  terme <- as.character(unlist(q$terme))[1]
  if (is.na(terme) || !nzchar(terme)) {
    nom <- grep("ann[ée]e|year|date|temps|time|saison|season|^an$", lu$noms, ignore.case = TRUE, value = TRUE)
    var_car1 <- if (isTRUE(m$car1) && identical(as.character(d$correlation$temps$by)[1], "variable")) as.numeric(d$correlation$temps$variable)
    termes_x <- termes_regression(d$x)
    du_car1 <- lu$noms[vapply(lu$candidats, function(t) isTRUE(as.numeric(termes_x[[match(t, m$termes)]]$variables) %in% var_car1), TRUE)]
    terme <- c(nom, du_car1, lu$noms)[1]
  }
  if (!terme %in% lu$noms) err(sprintf("« %s » n'est pas un X quantitatif (non transformé) de ce modèle : choisissez le temps parmi %s.", terme, paste(lu$noms, collapse = ", ")),
                               sprintf("\"%s\" isn't a quantitative X (untransformed) of this model: choose the time among %s.", terme, paste(lu$noms, collapse = ", ")))
  pourcent_possible <- !is.null(log_reponse(d$y[[1]]$transform))
  unite <- as.character(unlist(q$unite))[1]
  if (is.na(unite) || !unite %in% c("pente", "pourcent")) unite <- if (pourcent_possible) "pourcent" else "pente"
  y <- m$donnees$y
  defaut <- if (unite == "pourcent") -3 else -signif(if (mean(y) != 0) 0.03 * abs(mean(y)) else if (stats::sd(y) > 0) 0.1 * stats::sd(y) else 0.1, 2)
  tendance <- nombre(q$tendance, defaut)[1]
  if (!is.finite(tendance) || tendance == 0) err("La tendance à détecter est un nombre non nul.", "The trend to detect is a non-zero number.")
  alpha <- nombre(q$alpha, 0.05)[1]
  if (!(alpha >= 0.001 && alpha <= 0.2)) err("α est entre 0,001 et 0,2.", "α is between 0.001 and 0.2.")
  list(terme = terme, unite = unite, tendance = tendance, alpha = alpha, pourcent_possible = pourcent_possible,
       annees = nombre(q$annees, NULL), unites = nombre(q$unites, NULL), simulation = q$simulation)
}

# Les nombres d'années et d'unités, vérifiés (au moins 2 unités d'un modèle mixte : sa variance
# entre unités ; 1 observation par année d'une régression linéaire).
verifier_plan_puissance <- function(annees, unites, L) {
  lim <- PUISSANCE_LIMITES
  if (!length(annees) || any(annees != round(annees)) || any(annees < lim$annees[1] | annees > lim$annees[2])) {
    stop(sprintf(traduire("Les années vont de %d à %d.", "The years go from %d to %d."), lim$annees[1], lim$annees[2]), call. = FALSE)
  }
  minimum <- if (L) 2L else 1L
  if (!length(unites) || any(unites != round(unites)) || any(unites < minimum | unites > lim$unites)) {
    stop(sprintf(traduire("Les nombres d'unités sont des entiers de %d à %d.", "The numbers of units are whole numbers from %d to %d."), minimum, lim$unites), call. = FALSE)
  }
  invisible(TRUE)
}

# La puissance pour détecter une tendance avec le modèle `m` (de définition `d`) : `q`, les réglages
# (reglages_puissance) ; `avant`, le calcul précédent, gardé avec le modèle, dont la simulation
# reste quand elle porte sur les mêmes réglages, et la durée d'un ajustement au même plan.
# q$simuler : faire la simulation au plan q$simulation (annees, unites, n). Renvoie les réglages,
# le plan des données, les variances lues, la pente, le tableau (années, unités, observations,
# erreur-type, puissance) et, pour chaque nombre d'unités, les années qu'il faut pour 80 %, la phrase
# pour un rapport, et la simulation si elle a été faite.
puissance_tendance <- function(m, d, q = list(), avant = NULL) {
  lu <- lire_modele_puissance(m, d)
  r <- reglages_puissance(m, d, lu, q)
  lim <- PUISSANCE_LIMITES
  t <- lu$candidats[match(r$terme, lu$noms)]
  i <- match(t, m$termes)
  poids <- m$bloc$poids[i]
  x <- m$donnees[[t]] / (if (isTRUE(poids > 0)) poids else 1) + m$bloc$centre[i]  # (dans les unités du temps)
  if (lu$car1) {  # (CAR1 court le long de son temps : φ d'une unité de ce temps à la suivante, celle du plan)
    b <- unname(stats::coef(stats::lm(m$donnees$temps ~ x))[2])
    if (!isTRUE(abs(b - 1) <= 0.05)) {
      stop(sprintf(traduire("L'autocorrélation CAR1 court le long de « %s », la tendance le long de « %s » : leurs unités diffèrent (%s unités de « %s » pour une de « %s »), et les plans comptent une fois par unité de temps.",
                            "The CAR1 autocorrelation runs along \"%s\", the trend along \"%s\": their units differ (%s units of \"%s\" for one of \"%s\"), and the designs count once per unit of time."),
                   m$temps, r$terme, format(signif(b, 3)), m$temps, r$terme), call. = FALSE)
    }
  }
  pd <- plan_donnees_puissance(m, lu, x)
  annees <- if (length(r$annees) == 2) seq(min(r$annees), max(r$annees)) else seq(5L, as.integer(min(max(30L, pd$annees), lim$annees[2])))
  # (par défaut, autour du nombre des données : sa moitié, lui, une fois et demie, le double ; il y est toujours)
  unites <- sort(unique(c(if (length(r$unites)) r$unites else pmax(if (lu$L) 2 else 1, round(pd$unites * c(0.5, 1, 1.5, 2))), pd$unites)))
  if (length(unites) > lim$courbes) stop(sprintf(traduire("%d nombres d'unités au plus.", "%d numbers of units at most."), lim$courbes), call. = FALSE)
  verifier_plan_puissance(annees, unites, lu$L)
  unites <- as.integer(unites)
  pente <- pente_puissance(r$tendance, r$unite, d$y[[1]]$transform)

  # --- la puissance analytique de chaque plan
  tableau <- expand.grid(annees = annees, unites = unites, KEEP.OUT.ATTRS = FALSE)
  tableau$observations <- tableau$annees * tableau$unites * pd$series_par_unite * pd$repetitions
  tableau$erreur_type <- sqrt(mapply(variance_pente, annees = tableau$annees, unites = tableau$unites,
                                     MoreArgs = list(sigma2 = lu$sigma2, phi = lu$phi, series = pd$series_par_unite, repetitions = pd$repetitions)))
  tableau$puissance <- puissance_normale(pente$pente, tableau$erreur_type, r$alpha)
  annees_80 <- vapply(unites, function(u) { ok <- tableau$annees[tableau$unites == u & tableau$puissance >= 0.8]; if (length(ok)) min(ok) else NA_real_ }, 0)

  # --- la simulation : son plan (celui des données par défaut), la durée attendue, et celle de la
  # dernière fois quand elle porte sur les mêmes réglages
  s <- r$simulation %||% list()
  sim_annees <- as.integer(round(as.numeric(s$annees %||% min(max(pd$annees, lim$annees[1]), lim$annees[2]))[1]))
  sim_unites <- as.integer(round(as.numeric(s$unites %||% pd$unites)[1]))
  sim_n <- as.integer(round(as.numeric(s$n %||% 200)[1]))
  verifier_plan_puissance(sim_annees, sim_unites, lu$L)
  if (!(sim_n >= lim$simulations[1] && sim_n <= lim$simulations[2])) {
    stop(sprintf(traduire("La simulation tire de %d à %d jeux de données.", "The simulation draws %d to %d data sets."), lim$simulations[1], lim$simulations[2]), call. = FALSE)
  }
  sim_obs <- sim_annees * sim_unites * pd$series_par_unite * pd$repetitions
  cle <- list(terme = r$terme, pente = pente$pente, alpha = r$alpha, annees = sim_annees, unites = sim_unites, n = sim_n,
              sigma2 = lu$sigma2, phi = lu$phi, plan = c(pd$emboites, pd$dans, pd$repetitions))
  par_ajustement <- if (!is.null(avant$estimation) && identical(avant$estimation$plan, c(sim_annees, sim_unites, pd$series_par_unite, pd$repetitions))) avant$estimation$par_ajustement
  simulation <- if (!is.null(avant$simulation) && identical(avant$simulation$cle, cle)) avant$simulation
  if (sim_obs <= lim$observations && is.null(par_ajustement)) par_ajustement <- duree_ajustement(lu, pd, pente$pente, sim_annees, sim_unites)
  if (isTRUE(as.logical(q$simuler)) && is.null(simulation)) {
    if (sim_obs > lim$observations) stop(sprintf(traduire("Ce plan a %d observations par jeu : %d au plus pour la simulation.", "This design has %d observations per data set: %d at most for the simulation."), sim_obs, lim$observations), call. = FALSE)
    if (sim_n * par_ajustement > lim$duree) {
      stop(sprintf(traduire("%d jeux de données prendraient environ %d minutes (%s s par ajustement) : moins de jeux, ou un plan plus petit.",
                            "%d data sets would take about %d minutes (%s s per fit): fewer data sets, or a smaller design."), sim_n, ceiling(sim_n * par_ajustement / 60), format(signif(par_ajustement, 2))), call. = FALSE)
    }
    simulation <- c(simuler_puissance(lu, pd, pente$pente, r$alpha, sim_annees, sim_unites, sim_n), list(cle = cle))
  }
  if (!is.null(simulation)) {
    simulation$analytique <- puissance_normale(pente$pente, sqrt(variance_pente(lu$sigma2, lu$phi, simulation$annees, simulation$unites, pd$series_par_unite, pd$repetitions)), r$alpha)
  }

  # la tendance du modèle, pour comparer (le coefficient du temps dans ses unités)
  co <- m$resultat$coefficients
  pente_modele <- co$estimation[match(r$terme, co$terme)] * (if (isTRUE(poids > 0)) poids else 1)
  termes_x <- termes_regression(d$x)
  variable <- as.numeric(termes_x[[i]]$variables)
  croises <- vapply(Filter(function(tt) length(tt$variables) > 1 && variable %in% as.numeric(tt$variables), termes_x), function(tt) as.character(tt$name), "")
  res <- list(reglages = list(terme = r$terme, unite = r$unite, tendance = r$tendance, alpha = r$alpha, annees = I(range(annees)), unites = I(unites),
                              simulation = list(annees = sim_annees, unites = sim_unites, n = sim_n)),
              termes = I(lu$noms), type = m$type, pourcent_possible = r$pourcent_possible, approchee = pente$approchee,
              reponse = m$y$noms, transformation = m$y$transformation, forme = d$y[[1]]$transform, pente = pente$pente, pente_modele = pente_modele,
              y_modele = if (!is.null(d$y[[1]]$transform)) formule_transformation(d$y[[1]]$transform, m$y$noms, !identical(getOption("nuee.langue"), "en")) else m$y$noms,
              niveaux = I(m$niveaux), dans = m$dans, temps_car1 = m$temps, croises = I(unname(croises)),
              plan = pd[c("annees", "N", "unites", "emboites", "moyennes", "dans", "moyenne_dans", "repetitions", "moyenne_repetitions", "series_par_unite")],
              variances = list(sigma2 = lu$sigma2, niveaux = I(lu$variances), phi = if (lu$car1) lu$phi),
              tableau = tableau, seuil = 0.8, annees_80 = data.frame(unites = unites, annees = annees_80),
              estimation = list(plan = c(sim_annees, sim_unites, pd$series_par_unite, pd$repetitions), par_ajustement = par_ajustement,
                                duree = if (!is.null(par_ajustement)) sim_n * par_ajustement, observations = sim_obs),
              simulation = simulation)
  res$plan$emboites <- I(res$plan$emboites)
  res$plan$moyennes <- I(res$plan$moyennes)
  res$phrase <- phrase_puissance(res)
  res
}

# ------------------------------------------------------------ les phrases

# Une tendance en mots : « une baisse de 3 % par an de « densite » », « une hausse de 0,05 par an
# de log10(« densite » + 1) » (la réponse du modèle, sa transformation comprise : r$forme).
texte_tendance <- function(r, fr) {
  guillemets <- function(x) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)
  marque <- if (fr) "," else "."
  valeur <- trimws(formatC(abs(r$reglages$tendance), digits = 4, format = "fg", decimal.mark = marque))
  baisse <- r$reglages$tendance < 0
  if (r$reglages$unite == "pourcent") {
    return(sprintf(if (fr) "une %s de %s %% par an de %s" else "%s of %s%% per year in %s",
                   if (fr) (if (baisse) "baisse" else "hausse") else (if (baisse) "a decline" else "an increase"), valeur, guillemets(r$reponse)))
  }
  y <- if (!is.null(r$forme)) formule_transformation(r$forme, guillemets(r$reponse), fr) else guillemets(r$reponse)
  sprintf(if (fr) "une %s de %s par an de %s" else "%s of %s per year in %s", if (fr) (if (baisse) "baisse" else "hausse") else (if (baisse) "a decline" else "an increase"), valeur, y)
}

# Le plan d'une unité en mots : « 12 unités de « site » (3 « secteur » dans chacune) », « 12
# observations par an » (une régression linéaire).
texte_plan_unites <- function(r, unites, fr) {
  guillemets <- function(x) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)
  if (!length(r$niveaux)) return(sprintf(if (fr) "%d observations par an" else "%d observations per year", as.integer(unites)))
  p <- r$plan
  dedans <- c(if (length(p$emboites)) vapply(seq_along(p$emboites), function(k) {
                if (k == 1) sprintf(if (fr) "%d %s dans chacune" else "%d %s in each", as.integer(p$emboites[k]), guillemets(r$niveaux[k + 1]))
                else sprintf(if (fr) "%d %s dans chaque %s" else "%d %s in each %s", as.integer(p$emboites[k]), guillemets(r$niveaux[k + 1]), guillemets(r$niveaux[k]))
              }, ""),
              if (p$dans > 0 && !is.null(r$dans)) sprintf(if (fr) "%d %s dans chaque %s" else "%d %s in each %s", as.integer(p$dans), guillemets(r$dans), guillemets(r$niveaux[length(r$niveaux)])),
              if (p$repetitions > 1) sprintf(if (fr) "%d observations par an dans chaque %s" else "%d observations per year in each %s", as.integer(p$repetitions), guillemets(r$niveaux[length(r$niveaux)])))
  sprintf(if (fr) "%d unités de %s%s" else "%d units of %s%s", as.integer(unites), guillemets(r$niveaux[1]),
          if (length(dedans)) sprintf(" (%s)", paste(dedans, collapse = if (fr) " ; " else "; ")) else "")
}

# La phrase pour un rapport, au nombre d'unités des données : « Avec 12 unités de « site » comptées
# chaque année, une baisse de 3 % par an de « densite » est détectée avec une puissance de 80 %
# après 11 ans (α = 0,05, bilatéral). »
phrase_puissance <- function(r) {
  fr <- !identical(getOption("nuee.langue"), "en")
  u <- r$plan$unites
  k <- match(u, r$annees_80$unites)
  a80 <- r$annees_80$annees[k]
  ici <- r$tableau[r$tableau$unites == u, ]
  de <- min(ici$annees)
  a <- max(ici$annees)
  alpha <- sprintf(if (fr) "α = %s, test bilatéral" else "α = %s, two-sided", formatC(r$reglages$alpha, digits = 3, format = "fg", decimal.mark = if (fr) "," else "."))
  qui <- if (length(r$niveaux)) sprintf(if (fr) "Avec %s, chacune comptée chaque année" else "With %s, each counted every year", texte_plan_unites(r, u, fr))
         else sprintf(if (fr) "Avec %s" else "With %s", texte_plan_unites(r, u, fr))
  issue <- if (is.na(a80)) {
    sprintf(if (fr) "n'atteint pas une puissance de 80 %% en %d ans (%s après %d ans)" else "does not reach 80%% power within %d years (%s after %d years)",
            as.integer(a), paste0(formatC(100 * ici$puissance[ici$annees == a], format = "f", digits = 0), if (fr) " %" else "%"), as.integer(a))
  } else if (a80 == de) {
    sprintf(if (fr) "est détectée avec une puissance d'au moins 80 %% dès %d ans, la plus courte série calculée" else "is detected with at least 80%% power already after %d years, the shortest series computed", as.integer(de))
  } else sprintf(if (fr) "est détectée avec une puissance de 80 %% après %d ans" else "is detected with 80%% power after %d years", as.integer(a80))
  sprintf("%s, %s %s (%s).", qui, texte_tendance(r, fr), issue, alpha)
}

# La puissance dans le texte des Méthodes (texte_methodes_regression, quand elle a été calculée) :
# la tendance et sa pente, les plans, la méthode et les variances, la phrase, et la simulation si
# elle a été faite. `nombre`, `liste`, `guillemets` : ceux de texte_methodes_regression. Renvoie le
# texte et les références à citer (diggle, johnson).
phrase_methodes_puissance <- function(m, fr, nombre, liste, guillemets) {
  r <- m$puissance_tendance
  ancien <- options(nuee.langue = if (fr) "fr" else "en")  # (texte_tendance, phrase_puissance : la langue du texte)
  on.exit(options(ancien))
  g <- r$reglages
  pente <- sprintf(if (fr) "une pente de %s par an sur %s" else "a slope of %s per year on %s", formatC(r$pente, digits = 3, format = "g", decimal.mark = if (fr) "," else "."),
                   if (!is.null(r$forme)) formule_transformation(r$forme, guillemets(r$reponse), fr) else guillemets(r$reponse))
  tendance <- sprintf(if (fr) "%s%s" else "%s%s", texte_tendance(r, fr), if (g$unite == "pourcent") sprintf(" (%s%s)", pente, if (isTRUE(r$approchee)) (if (fr) ", une approximation avec ce décalage" else ", an approximation with this offset") else "") else "")
  unites <- if (length(r$niveaux)) sprintf(if (fr) "%s à %s unités de %s, chacune comptée chaque année" else "%s to %s units of %s, each counted every year", min(g$unites), max(g$unites), guillemets(r$niveaux[1]))
            else sprintf(if (fr) "%s à %s observations par an" else "%s to %s observations per year", min(g$unites), max(g$unites))
  variances <- c(sprintf(if (fr) "variance résiduelle %s" else "residual variance %s", formatC(r$variances$sigma2, digits = 3, format = "g", decimal.mark = if (fr) "," else ".")),
                 if (!is.null(r$variances$phi)) sprintf("φ = %s", nombre(r$variances$phi, 2)))
  texte <- sprintf(if (fr) "La puissance de détecter %s a été calculée pour des suivis de %d à %d ans et de %s, avec un modèle de la seule tendance (les autres termes tenus pour constants) : la variance de l'estimateur des moindres carrés généralisés de la pente, les variances du modèle (REML) tenues pour connues (%s%s), et un test bilatéral au seuil α = %s par la loi normale (Diggle et al., 2002)."
                   else "The power to detect %s was computed for monitoring of %d to %d years and %s, with a model of the trend alone (the other terms held constant): the variance of the generalized least squares estimator of the slope, the model's variances (REML) taken as known (%s%s), and a two-sided test at α = %s by the normal distribution (Diggle et al., 2002).",
                   tendance, as.integer(g$annees[1]), as.integer(g$annees[2]), unites, liste(variances),
                   if (length(r$niveaux)) (if (fr) " ; les ordonnées à l'origine aléatoires, chaque unité étant comptée chaque année, n'y changent rien" else "; the random intercepts, each unit being counted every year, change nothing in it") else "",
                   formatC(g$alpha, digits = 3, format = "fg", decimal.mark = if (fr) "," else "."))
  cites <- "diggle"
  texte <- paste(texte, phrase_puissance(r))
  s <- r$simulation
  if (!is.null(s)) {
    cites <- c(cites, "johnson")
    texte <- paste(texte, sprintf(if (fr) "Une simulation de %d jeux de données tirés du modèle avec cette tendance, à %s et %d ans, chacun réajusté avec la même structure (REML) et la pente testée, a donné une puissance de %s (± %s, erreur de Monte-Carlo ; la formule : %s ; Johnson et al., 2015)%s."
                                  else "A simulation of %d data sets drawn from the model with this trend, at %s and %d years, each refitted with the same structure (REML) and the slope tested, gave a power of %s (± %s, Monte Carlo error; the formula: %s; Johnson et al., 2015)%s.",
                                  s$n, texte_plan_unites(r, s$unites, fr), s$annees, nombre(s$puissance, 2), nombre(s$erreur_mc, 2), nombre(s$analytique, 2),
                                  if (s$echecs) sprintf(if (fr) " ; %d ajustements ont échoué" else "; %d fits failed", s$echecs) else ""))
  }
  list(texte = texte, cites = cites)
}
