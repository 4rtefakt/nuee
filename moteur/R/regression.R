# Modèles de régression : linéaire (stats::lm) et linéaire mixte (nlme::lme, Pinheiro et
# Bates, 2000), avec des effets aléatoires emboîtés (site, puis secteur dans le site...) et,
# au choix, une autocorrélation dans le temps à l'intérieur du dernier niveau (corCAR1 : temps
# continu, les années manquantes comprises).
#
# Les données arrivent en un data.frame aux noms sûrs, faits ici et jamais tirés d'un texte :
# y (la réponse), t1..tK (les termes, déjà transformés), g1..gL (les niveaux des effets
# aléatoires, du plus large au plus fin) et temps. Les formules ne sont écrites qu'avec ces
# noms : un projet reçu ne peut rien y glisser.
#
# Les coefficients et les variances viennent de l'ajustement en REML ; la vraisemblance,
# l'AIC et les comparaisons de modèles d'un ajustement en ML (Zuur et al., 2009 : deux modèles
# aux effets fixes différents ne se comparent qu'en ML). Pour un modèle linéaire, les deux
# sont le même ajustement.

# Les termes qui peuvent entrer dans un ajustement sur ces observations : sans les constants
# ni ceux qui se déduisent des autres (une catégorie quand toutes y sont, des proportions qui
# font 1...), dans leur ordre.
termes_utiles <- function(donnees, termes) {
  if (!length(termes)) return(character(0))
  X <- cbind(1, as.matrix(donnees[termes]))
  q <- qr(X, tol = 1e-9)
  garde <- sort(q$pivot[seq_len(q$rank)])
  termes[setdiff(garde, 1) - 1]
}

# Un ajustement, ou NULL quand il échoue (nlme essaie optim, puis nlminb, comme le font les
# scripts qui ont servi de modèle). methode : "REML" ou "ML".
ajuster_lineaire <- function(donnees, termes, niveaux = 0, car1 = FALSE, methode = "REML") {
  f <- stats::reformulate(if (length(termes)) termes else "1", response = "y")
  if (!niveaux) {
    environment(f) <- baseenv()
    return(tryCatch(stats::lm(f, data = donnees), error = function(e) NULL))
  }
  aleatoire <- stats::as.formula(paste("~ 1 |", paste0("g", seq_len(niveaux), collapse = " / ")), env = baseenv())
  correlation <- if (car1) nlme::corCAR1(form = stats::as.formula(paste("~ temps |", paste0("g", seq_len(niveaux), collapse = " / ")), env = baseenv()))
  essai <- function(reglages) tryCatch(nlme::lme(f, random = aleatoire, correlation = correlation, data = donnees, method = methode,
                                                 control = reglages), error = function(e) NULL)
  essai(nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE)) %||%
    essai(nlme::lmeControl(maxIter = 500, msMaxIter = 500, returnObject = TRUE))
}

# AIC corrigé pour les petits échantillons (Hurvich et Tsai, 1989).
aic_corrige <- function(m, n) {
  k <- attr(stats::logLik(m), "df")
  stats::AIC(m) + if (n - k - 1 > 0) 2 * k * (k + 1) / (n - k - 1) else NA_real_
}

# R² d'un modèle mixte (Nakagawa et Schielzeth, 2013) : marginal, la part de variance des
# effets fixes ; conditionnel, celle des effets fixes et aléatoires ensemble.
r2_mixte <- function(m) {
  fixes <- stats::var(as.vector(stats::model.matrix(stats::formula(m), m$data) %*% nlme::fixef(m)))
  vc <- nlme::VarCorr(m)
  aleatoires <- sum(suppressWarnings(as.numeric(vc[grepl("Intercept", rownames(vc)), "Variance"])), na.rm = TRUE)
  totale <- fixes + aleatoires + m$sigma^2
  c(marginal = fixes / totale, conditionnel = (fixes + aleatoires) / totale)
}

# Les coefficients : estimation, erreur-type, degrés de liberté, t, p et intervalle à 95 %.
coefficients_lineaire <- function(m) {
  if (inherits(m, "lme")) {
    tt <- summary(m)$tTable
    q <- stats::qt(0.975, tt[, "DF"])
    return(data.frame(terme = rownames(tt), estimation = tt[, "Value"], erreur_type = tt[, "Std.Error"], ddl = tt[, "DF"],
                      t = tt[, "t-value"], p = tt[, "p-value"], bas = tt[, "Value"] - q * tt[, "Std.Error"],
                      haut = tt[, "Value"] + q * tt[, "Std.Error"], row.names = NULL, stringsAsFactors = FALSE))
  }
  tt <- summary(m)$coefficients
  ic <- stats::confint(m)
  data.frame(terme = rownames(tt), estimation = tt[, 1], erreur_type = tt[, 2], ddl = stats::df.residual(m), t = tt[, 3], p = tt[, 4],
             bas = ic[, 1], haut = ic[, 2], row.names = NULL, stringsAsFactors = FALSE)
}

# Le modèle ajusté sur toutes ses observations : REML pour les coefficients et les variances,
# ML pour la vraisemblance et l'AIC. noms : le nom affiché de chaque terme t1..tK ; groupes :
# ceux dont on mesure l'apport (retrait_groupes), une liste nommée de vecteurs de termes.
# Renvoie des valeurs simples (pour l'interface), et les deux ajustements dans `ajustement`
# et `ajustement_ml` (à ne pas envoyer).
resultat_lineaire <- function(donnees, termes, noms, niveaux = 0, car1 = FALSE, groupes = list()) {
  if (car1 && anyDuplicated(data.frame(interaction(donnees[paste0("g", seq_len(niveaux))], drop = TRUE), donnees$temps))) {
    stop(traduire("L'autocorrélation dans le temps demande un seul temps par groupe : plusieurs observations ont le même temps dans un même groupe.",
                  "Autocorrelation in time needs a single time per group: several observations have the same time in the same group."), call. = FALSE)
  }
  utiles <- termes_utiles(donnees, termes)
  retires <- noms[match(setdiff(termes, utiles), termes)]
  estime <- ajuster_lineaire(donnees, utiles, niveaux, car1, "REML")
  compare <- if (niveaux) ajuster_lineaire(donnees, utiles, niveaux, car1, "ML") else estime
  if (is.null(estime) || is.null(compare)) {
    stop(traduire("Le modèle n'a pas pu être ajusté (l'optimisation n'a pas convergé) : essayez moins de termes ou d'effets aléatoires.",
                  "The model could not be fitted (the optimisation did not converge): try fewer terms or random effects."), call. = FALSE)
  }
  n <- nrow(donnees)
  co <- coefficients_lineaire(estime)
  co$terme <- c("(Intercept)", noms[match(co$terme[-1], termes)])
  ll <- stats::logLik(compare)
  res <- list(N = n, coefficients = co, termes_retires = retires,
              logLik = as.numeric(ll), parametres = attr(ll, "df"), AIC = stats::AIC(compare), AICc = aic_corrige(compare, n), BIC = stats::BIC(compare))
  if (niveaux) {
    vc <- nlme::VarCorr(estime)
    variances <- suppressWarnings(as.numeric(vc[, "Variance"]))
    res$variances <- data.frame(composante = c(paste0("g", seq_len(niveaux)), "residuelle"),
                                variance = c(variances[!is.na(variances)][seq_len(niveaux)], estime$sigma^2), stringsAsFactors = FALSE)
    res$r2 <- as.list(r2_mixte(estime))
    if (car1) res$phi <- unname(stats::coef(estime$modelStruct$corStruct, unconstrained = FALSE))
    res$ajuste <- as.vector(stats::fitted(estime))                       # avec les effets aléatoires
    res$ajuste_fixe <- as.vector(stats::fitted(estime, level = 0))       # les effets fixes seuls
    res$residus <- as.vector(stats::residuals(estime, type = "normalized"))
  } else {
    s <- summary(estime)
    res$r2 <- list(r2 = s$r.squared, r2_ajuste = s$adj.r.squared)
    res$ajuste <- as.vector(stats::fitted(estime))
    res$residus <- as.vector(stats::rstandard(estime))
    res$levier <- as.vector(stats::hatvalues(estime))
    res$cook <- as.vector(stats::cooks.distance(estime))
  }
  if (length(groupes)) {
    res$retrait <- retrait_groupes(donnees, utiles, groupes, compare, niveaux, car1)
    if (!is.null(res$retrait)) res$retrait$groupe <- as.character(res$retrait$groupe)
  }
  res$utiles <- utiles
  res$ajustement <- estime
  res$ajustement_ml <- compare
  res
}

# L'apport de chaque groupe de termes (un bloc, ou un terme seul) : le modèle réajusté en ML
# sans lui. ΔAIC = AIC sans le groupe - AIC du modèle complet (> 0 : le groupe améliore le
# modèle ; > 2, nettement, Burnham et Anderson, 2002) ; test du rapport de vraisemblance.
# groupes : une liste nommée de vecteurs de termes (t1..tK).
retrait_groupes <- function(donnees, utiles, groupes, complet, niveaux = 0, car1 = FALSE) {
  ll <- as.numeric(stats::logLik(complet))
  df <- attr(stats::logLik(complet), "df")
  lignes <- lapply(names(groupes), function(nom) {
    sans <- setdiff(utiles, groupes[[nom]])
    retires <- length(utiles) - length(sans)
    if (!retires) return(NULL)
    m <- ajuster_lineaire(donnees, sans, niveaux, car1, "ML")
    if (is.null(m)) return(data.frame(groupe = nom, termes = retires, delta_aic = NA_real_, khi2 = NA_real_, ddl = NA_real_, p = NA_real_))
    khi2 <- 2 * (ll - as.numeric(stats::logLik(m)))
    ddl <- df - attr(stats::logLik(m), "df")
    data.frame(groupe = nom, termes = retires, delta_aic = stats::AIC(m) - stats::AIC(complet), khi2 = khi2, ddl = ddl,
               p = stats::pchisq(khi2, ddl, lower.tail = FALSE), stringsAsFactors = FALSE)
  })
  do.call(rbind, lignes)
}

# Validation croisée : chaque groupe prédit par le modèle ajusté sans lui (REML), avec ses
# seuls termes utiles sur ces observations ; un modèle mixte prédit un groupe qu'il n'a pas vu
# par ses effets fixes (niveau 0 : un site jamais vu). groupe : le numéro du groupe de chaque
# observation. Renvoie les prédictions (NA quand un ajustement échoue), Q2 et RMSEcv.
validation_lineaire <- function(donnees, termes, groupe, niveaux = 0, car1 = FALSE) {
  prediction <- rep(NA_real_, nrow(donnees))
  echecs <- 0
  for (g in sort(unique(groupe))) {
    ici <- groupe == g
    apprentissage <- droplevels(donnees[!ici, , drop = FALSE])
    m <- ajuster_lineaire(apprentissage, termes_utiles(apprentissage, termes), niveaux, car1, "REML")
    if (is.null(m)) { echecs <- echecs + 1; next }
    prediction[ici] <- if (niveaux) stats::predict(m, donnees[ici, , drop = FALSE], level = 0) else stats::predict(m, donnees[ici, , drop = FALSE])
  }
  ok <- !is.na(prediction)
  y <- donnees$y
  list(prediction = prediction, echecs = echecs,
       Q2 = if (any(ok)) 1 - sum((y[ok] - prediction[ok])^2) / sum((y[ok] - mean(y[ok]))^2) else NA_real_,
       RMSEcv = if (any(ok)) sqrt(mean((y[ok] - prediction[ok])^2)) else NA_real_)
}

# La comparaison de modèles par l'AIC : ΔAIC au meilleur et poids d'Akaike (la probabilité
# que chaque modèle soit le meilleur de l'ensemble, Burnham et Anderson, 2002). Seuls se
# comparent des modèles de la même réponse sur les mêmes observations ; les autres restent
# hors du calcul (NA). aic : les AIC ; cle : ce qui doit être pareil (réponse et observations).
comparaison_aic <- function(aic, cle) {
  delta <- poids <- rep(NA_real_, length(aic))
  for (k in unique(cle[!is.na(aic)])) {
    ici <- which(cle == k & !is.na(aic))
    d <- aic[ici] - min(aic[ici])
    delta[ici] <- d
    poids[ici] <- exp(-d / 2) / sum(exp(-d / 2))
  }
  list(delta_aic = delta, poids = poids)
}

# ------------------------------------------------------------- les modèles de Nuée

# Les types de modèles de régression ; "LMM" a au moins un effet aléatoire.
REGRESSIONS <- c("LM", "LMM")
est_regression <- function(d) isTRUE(as.character(d$type %||% "") %in% REGRESSIONS)

# Les valeurs qui font les groupes d'un effet aléatoire, ou le temps de l'autocorrélation : un
# identifiant des observations (list(by = "id", id = "site")) ou une variable
# (list(by = "variable", variable = 4)), pour les observations `obs` (NA quand il manque).
valeurs_groupement <- function(projet, spec, obs) {
  par <- if (length(spec$by)) as.character(spec$by)[1] else ""
  switch(par,
    id = {
      k <- match(as.character(spec$id)[1], as.character(projet$observation_id_names))
      if (is.na(k) || k < 2) stop(sprintf(traduire("Ce projet n'a pas d'identifiant « %s ».", "This project has no ID \"%s\"."), spec$id), call. = FALSE)
      v <- identifiants_observations(projet, k, obs)
      v[!is.na(v) & !nzchar(trimws(v))] <- NA
      v
    },
    variable = {
      v <- variable_du_projet(projet, spec$variable)
      x <- colonne(projet, spec$variable, obs)
      if (length(v$categories)) as.character(v$categories)[x] else x
    },
    stop(sprintf(traduire("Groupement « %s » inconnu", "Unknown grouping \"%s\""), par), call. = FALSE))
}

# Les groupements tels que l'interface les envoie (un data.frame, une ligne par effet aléatoire,
# ou une liste) en une liste de list(by = "id", id = ...) ou list(by = "variable", variable = ...).
specs_groupement <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) x <- lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
  if (!is.null(x$by)) x <- list(x)
  lapply(x, function(s) {
    par <- as.character(s$by)[1]
    if (identical(par, "id")) return(list(by = "id", id = as.character(s$id)[1]))
    if (identical(par, "variable")) return(list(by = "variable", variable = as.numeric(s$variable)[1]))
    stop(sprintf(traduire("Groupement « %s » inconnu", "Unknown grouping \"%s\""), par), call. = FALSE)
  })
}

nom_groupement <- function(projet, spec) {
  if (identical(as.character(spec$by)[1], "id")) return(as.character(spec$id)[1])
  tryCatch(as.character(variable_du_projet(projet, spec$variable)$ids[1]), error = function(e) as.character(spec$variable))
}

# Une définition de régression qu'on peut ajuster : une réponse quantitative ; des termes X
# sans spectres ni terme croisé d'une variable qualitative ; des effets aléatoires pour un
# modèle mixte (aucun pour un modèle linéaire), une autocorrélation seulement avec eux. Une
# définition lue dans un fichier passe aussi par là.
verifier_regression <- function(projet, d) {
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  if (length(d$y) != 1 || length(d$y[[1]]$variables) != 1 || categorie(d$y[[1]]) != 0 ||
      length(variable_du_projet(projet, d$y[[1]]$variables)$categories)) {
    err("Une régression demande une seule variable Y, quantitative.", "A regression needs a single Y variable, a quantitative one.")
  }
  if (length(d$spectres)) err("Une régression ne prend pas de prétraitement de spectres.", "A regression takes no spectral preprocessing.")
  for (t in d$x) if (length(t$variables) > 1 && any(vapply(seq_along(t$variables), function(j) categorie(t, j) != 0, TRUE))) {
    err("Une régression ne prend pas encore de terme croisé avec une variable qualitative.", "A regression doesn't take a cross term with a qualitative variable yet.")
  }
  aleatoires <- d$aleatoires %||% list()
  if (identical(d$type, "LMM") && !length(aleatoires)) err("Un modèle mixte demande au moins un effet aléatoire.", "A mixed model needs at least one random effect.")
  if (identical(d$type, "LM") && length(aleatoires)) err("Un modèle linéaire n'a pas d'effet aléatoire : choisissez un modèle mixte.", "A linear model has no random effect: choose a mixed model.")
  if (length(aleatoires) > 3) err("Trois effets aléatoires emboîtés au plus.", "Three nested random effects at most.")
  for (s in aleatoires) {
    if (identical(as.character(s$by)[1], "variable") && !length(variable_du_projet(projet, s$variable)$categories)) {
      err("Un effet aléatoire se fait sur un identifiant ou une variable qualitative.", "A random effect is made from an ID or a qualitative variable.")
    }
    valeurs_groupement(projet, s, d$observations[1])
  }
  if (!is.null(d$correlation)) {
    if (!identical(as.character(d$correlation$type)[1], "CAR1")) err("Autocorrélation inconnue.", "Unknown autocorrelation.")
    if (!length(aleatoires)) err("L'autocorrélation dans le temps se fait à l'intérieur des groupes d'un effet aléatoire.", "Autocorrelation in time works within the groups of a random effect.")
    valeurs_groupement(projet, d$correlation$temps, d$observations[1])
  }
  invisible(TRUE)
}

# Les termes X d'une régression : sans la pondération par blocs ni les modificateurs (le bloc
# d'un terme ne sert qu'à la table des apports, apport_groupes) ; les catégories d'une variable
# qualitative restent des indicatrices 0/1, sans centrage ni mise à l'échelle.
termes_regression <- function(termes) lapply(termes, function(t) {
  t$scaling$block <- NULL
  t$scaling$modifier <- NULL
  if (length(t$variables) == 1 && categorie(t) != 0) t$scaling$type <- "none"
  t
})

# Le groupe de chaque terme dans la table des apports : son bloc s'il en a un, sinon sa
# variable (les catégories d'une variable qualitative ensemble) ; un terme croisé, lui-même.
groupes_termes <- function(projet, termes) {
  vapply(termes, function(t) {
    b <- t$scaling$block
    if (length(b) && !is.na(b[1]) && nzchar(as.character(b[1]))) return(as.character(b[1]))
    if (length(t$variables) == 1) as.character(variable_du_projet(projet, t$variables)$ids[1]) else as.character(t$name)
  }, "")
}

# Les données d'une régression pour les observations `obs` : y (la réponse, transformée, sans
# mise à l'échelle), t1..tK (les termes X, construire_bloc), g1..gL (les effets aléatoires,
# du plus large au plus fin) et temps. Seules restent les observations où tout est connu.
# `imposees` : les centres et poids d'un modèle déjà ajusté (pour un jeu de test).
cadre_regression <- function(projet, d, obs = d$observations, imposees = NULL) {
  ty <- d$y[[1]]
  y <- transformer(valeurs_simples(projet, ty, obs), ty$transform)
  termes <- termes_regression(d$x)
  K <- length(termes)
  bruts <- if (K) valeurs_bloc(projet, termes, obs) else matrix(numeric(0), length(obs), 0)
  simples <- which(vapply(termes, function(t) length(t$variables) == 1, TRUE))
  niveaux <- lapply(d$aleatoires %||% list(), function(s) valeurs_groupement(projet, s, obs))
  temps <- NULL
  if (!is.null(d$correlation)) {
    temps <- suppressWarnings(as.numeric(valeurs_groupement(projet, d$correlation$temps, obs)))
    if (all(is.na(temps))) stop(traduire("Le temps de l'autocorrélation doit être un nombre (une année, un jour...).",
                                         "The time of the autocorrelation must be a number (a year, a day...)."), call. = FALSE)
  }
  connu <- !is.na(y) & stats::complete.cases(bruts[, simples, drop = FALSE])
  for (v in niveaux) connu <- connu & !is.na(v)
  if (!is.null(temps)) connu <- connu & !is.na(temps)
  garde <- obs[connu]
  if (!length(garde)) stop(traduire("Aucune observation n'a toutes ses valeurs pour ce modèle.", "No observation has all its values for this model."), call. = FALSE)
  bloc <- if (K) construire_bloc(projet, termes, garde, imposees = imposees)
  cadre <- data.frame(y = y[connu], row.names = NULL)
  for (k in seq_len(K)) cadre[[paste0("t", k)]] <- bloc$X[, k]
  for (l in seq_along(niveaux)) cadre[[paste0("g", l)]] <- factor(niveaux[[l]][connu])
  if (!is.null(temps)) cadre$temps <- temps[connu]
  complet <- stats::complete.cases(cadre)  # (un terme croisé que sa transformation rend manquant)
  list(cadre = cadre[complet, , drop = FALSE], observations = garde[complet], exclues = setdiff(obs, garde[complet]),
       termes = paste0("t", seq_len(K)), noms = if (K) bloc$noms else character(0), bloc = bloc)
}

# Ajuste une définition de régression (definir_modele, type "LM" ou "LMM") : le modèle, sa
# validation croisée par les groupes de la définition (groupes_definition) et son jeu de test.
ajuster_regression <- function(projet, d) {
  # (une définition relue d'un fichier : ses listes ont pu devenir des data.frames)
  d$aleatoires <- specs_groupement(d$aleatoires)
  if (!is.null(d$correlation)) d$correlation <- list(type = as.character(d$correlation$type)[1], temps = specs_groupement(d$correlation$temps)[[1]])
  verifier_regression(projet, d)
  if (identical(d$type, "LMM") && !requireNamespace("nlme", quietly = TRUE)) {
    stop(traduire("Le paquet R nlme manque (il vient avec R) : installez-le pour les modèles mixtes.",
                  "The R package nlme is missing (it comes with R): install it for mixed models."), call. = FALSE)
  }
  cadre <- cadre_regression(projet, d)
  niveaux <- length(d$aleatoires %||% list())
  car1 <- !is.null(d$correlation)
  if (nrow(cadre$cadre) <= length(cadre$termes) + 1) {
    stop(sprintf(traduire("Trop peu d'observations complètes (%d) pour %d termes.", "Too few complete observations (%d) for %d terms."),
                 nrow(cadre$cadre), length(cadre$termes)), call. = FALSE)
  }
  r <- resultat_lineaire(cadre$cadre, cadre$termes, cadre$noms, niveaux, car1)
  G <- d$cv_groups %||% 7
  groupes <- groupes_definition(projet, d, G)[match(cadre$observations, d$observations)]
  vc <- validation_lineaire(cadre$cadre, r$utiles, groupes, niveaux, car1)
  y <- d$y[[1]]
  m <- list(type = d$type, regression = TRUE, plsda = FALSE, opls = FALSE, A = 0, nom = d$name %||% "", titre = d$title %||% "",
            N = nrow(cadre$cadre), K = length(r$utiles), observations = cadre$observations, exclues = cadre$exclues,
            y = list(uid = as.numeric(y$variables), noms = as.character(y$name), transformation = nom_transformation(y$transform) %||% ""),
            noms_termes = cadre$noms, termes = cadre$termes, utiles = r$utiles,
            groupes_termes = stats::setNames(groupes_termes(projet, d$x), cadre$termes),
            niveaux = vapply(d$aleatoires %||% list(), function(s) nom_groupement(projet, s), ""),
            temps = if (car1) nom_groupement(projet, d$correlation$temps), car1 = car1,
            resultat = r[setdiff(names(r), c("ajustement", "ajustement_ml", "utiles"))],
            ajustement = r$ajustement, ajustement_ml = r$ajustement_ml, donnees = cadre$cadre, bloc = cadre$bloc,
            G = length(unique(groupes)), vc_par = nom_groupes_vc(projet, d),
            vc = list(Q2 = vc$Q2, RMSEcv = vc$RMSEcv, prediction = vc$prediction, groupe = groupes, echecs = vc$echecs))
  m$signature <- paste(m$y$uid, m$y$transformation, paste(sort(m$observations), collapse = ","))
  class(m) <- "modele_regression"
  if (length(d$test)) m$test <- tester_regression(projet, m, d)
  m
}

# Le jeu de test d'une régression : ses observations prédites par les effets fixes (un modèle
# mixte ne connaît pas leurs groupes : niveau 0), dans la forme de evaluer_test (jeu_test.R).
tester_regression <- function(projet, m, d) {
  t <- cadre_regression(projet, d, d$test, imposees = m$bloc)
  nd <- t$cadre
  predit <- if (inherits(m$ajustement, "lme")) stats::predict(m$ajustement, nd, level = 0) else stats::predict(m$ajustement, nd)
  y <- m$y$noms
  e <- as.vector(predit) - nd$y
  obs <- data.frame(uid = t$observations, nom = unname(projet$noms_obs[as.character(t$observations)]), hors_domaine = FALSE, stringsAsFactors = FALSE)
  obs[[paste(y, "observé")]] <- nd$y
  obs[[paste(y, "prédit")]] <- as.vector(predit)
  list(n = nrow(nd), hors_domaine = 0, observations = obs,
       statistiques = data.frame(y = y, n = length(e), RMSEP = sqrt(mean(e^2)), biais = mean(e), SEP = if (length(e) > 1) stats::sd(e) else NA_real_,
                                 Q2ext = 1 - sum(e^2) / sum((nd$y - mean(m$donnees$y))^2), stringsAsFactors = FALSE))
}

# L'apport de chaque groupe de termes du modèle (son bloc, sinon sa variable), calculé à la
# demande : retrait_groupes sur les données et l'ajustement en ML du modèle.
apport_groupes <- function(m) {
  if (!length(m$utiles)) return(NULL)
  g <- m$groupes_termes[m$utiles]
  groupes <- split(names(g), factor(unname(g), levels = unique(unname(g))))
  retrait_groupes(m$donnees, m$utiles, groupes, m$ajustement_ml, length(m$niveaux), m$car1)
}

# Le texte d'une section « Méthodes » pour une régression, de la même forme que celui des
# autres modèles (methodes.R : texte_methodes) : données, modèle, validation croisée,
# qualité, logiciel ; et les références.
REFERENCES_REGRESSION <- list(
  pinheiro = "Pinheiro, J. C., & Bates, D. M. (2000). Mixed-effects models in S and S-PLUS. Springer. https://doi.org/10.1007/b98882",
  nlme = "Pinheiro, J., Bates, D., & R Core Team (%s). nlme: Linear and nonlinear mixed effects models (R package version %s). https://CRAN.R-project.org/package=nlme",
  nakagawa = "Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R² from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. https://doi.org/10.1111/j.2041-210x.2012.00261.x",
  burnham = "Burnham, K. P., & Anderson, D. R. (2002). Model selection and multimodel inference: A practical information-theoretic approach (2nd ed.). Springer. https://doi.org/10.1007/b97636",
  zuur = "Zuur, A. F., Ieno, E. N., Walker, N. J., Saveliev, A. A., & Smith, G. M. (2009). Mixed effects models and extensions in ecology with R. Springer. https://doi.org/10.1007/978-0-387-87458-6")

texte_methodes_regression <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  r <- m$resultat
  nombre <- function(x, chiffres = 2) formatC(x, format = "f", digits = chiffres, decimal.mark = if (fr) "," else ".")
  pourcent <- function(x) paste0(formatC(100 * x, format = "f", digits = if (100 * x < 10) 1 else 0, decimal.mark = if (fr) "," else "."), if (fr) " %" else "%")
  liste <- function(x) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), if (fr) "et" else "and", x[length(x)])
  guillemets <- function(x) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)
  cites <- character(0)
  cite <- function(cle, texte) { cites <<- union(cites, cle); texte }
  mixte <- length(m$niveaux) > 0

  # --- données
  ty <- d$y[[1]]
  reponse <- if (is.null(ty$transform)) guillemets(m$y$noms) else formule_transformation(ty$transform, guillemets(m$y$noms), fr)
  p1 <- if (fr) sprintf("Le modèle porte sur %d observations%s ; la réponse est %s.", m$N,
                        if (length(m$exclues)) sprintf(" (%d autres laissées de côté faute de valeurs)", length(m$exclues)) else "", reponse)
        else sprintf("The model uses %d observations%s; the response is %s.", m$N,
                     if (length(m$exclues)) sprintf(" (%d others left out for missing values)", length(m$exclues)) else "", reponse)

  # --- modèle
  noms <- m$noms_termes[match(m$utiles, m$termes)]
  termes <- if (!length(noms)) (if (fr) "sans terme explicatif (le modèle nul)" else "with no explanatory term (the null model)")
            else if (length(noms) <= 12) sprintf(if (fr) "%d terme%s (%s)" else "%d term%s (%s)", length(noms), if (length(noms) > 1) "s" else "", liste(noms))
            else sprintf(if (fr) "%d termes" else "%d terms", length(noms))
  echelles <- unique(vapply(Filter(function(t) length(t$variables) == 1 && categorie(t) == 0, d$x), function(t) as.character(t$scaling$type %||% "UV"), ""))
  unites <- if (!length(echelles) || all(echelles == "none")) {
    if (fr) "Les variables explicatives sont dans leurs unités." else "The explanatory variables are in their own units."
  } else if (fr) "Les variables explicatives ont été centrées et mises à l'échelle avant l'ajustement." else "The explanatory variables were centred and scaled before fitting."
  p2 <- if (!mixte) {
    if (fr) sprintf("Nous avons ajusté une régression linéaire multiple par les moindres carrés ordinaires, avec %s. %s", termes, unites)
    else sprintf("We fitted a multiple linear regression by ordinary least squares, with %s. %s", termes, unites)
  } else {
    niveaux <- vapply(seq_along(m$niveaux), function(l) {
      if (l == 1) guillemets(m$niveaux[l]) else sprintf(if (fr) "%s dans chaque %s" else "%s within each %s", guillemets(m$niveaux[l]), guillemets(m$niveaux[l - 1]))
    }, "")
    car1 <- if (m$car1) {
      if (fr) sprintf(", et une autocorrélation des résidus en temps continu (CAR1) le long de %s à l'intérieur de chaque %s", guillemets(m$temps), guillemets(m$niveaux[length(m$niveaux)]))
      else sprintf(", and a continuous-time autocorrelation of the residuals (CAR1) along %s within each %s", guillemets(m$temps), guillemets(m$niveaux[length(m$niveaux)]))
    } else ""
    c(cite("pinheiro", if (fr) sprintf("Nous avons ajusté un modèle linéaire mixte (Pinheiro et Bates, 2000) avec %s en effets fixes, une ordonnée à l'origine aléatoire par %s%s. %s",
                                       termes, liste(niveaux), car1, unites)
                       else sprintf("We fitted a linear mixed model (Pinheiro & Bates, 2000) with %s as fixed effects, a random intercept for %s%s. %s",
                                    termes, liste(niveaux), car1, unites)),
      cite("zuur", if (fr) "Les coefficients et les variances ont été estimés par maximum de vraisemblance restreint (REML), et les modèles comparés par l'AIC d'ajustements en maximum de vraisemblance (ML ; Zuur et al., 2009)."
                   else "Coefficients and variances were estimated by restricted maximum likelihood (REML), and models compared by the AIC of maximum-likelihood fits (ML; Zuur et al., 2009)."))
  }

  # --- validation croisée
  par <- if (length(d$groupes_vc$by)) as.character(d$groupes_vc$by) else ""
  groupes <- if (par %in% c("id", "variable")) {
    if (fr) sprintf("%d groupes, un par valeur de %s", m$G, guillemets(m$vc_par)) else sprintf("%d groups, one per value of %s", m$G, guillemets(m$vc_par))
  } else if (fr) sprintf("%d groupes (l'observation i dans le groupe i modulo %d)", m$G, m$G) else sprintf("%d groups (observation i in group i modulo %d)", m$G, m$G)
  p3 <- if (fr) sprintf("La validation croisée comptait %s ; chaque groupe a été prédit par le modèle ajusté sans lui%s.", groupes,
                        if (mixte) ", par ses seuls effets fixes (comme un groupe jamais vu)" else "")
        else sprintf("Cross-validation used %s; each group was predicted by the model fitted without it%s.", groupes,
                     if (mixte) ", by its fixed effects only (as a group never seen)" else "")

  # --- qualité
  aic <- cite("burnham", sprintf("AIC = %s (Burnham %s Anderson, 2002)", nombre(r$AIC, 1), if (fr) "et" else "&"))
  p4 <- if (mixte) {
    cite("nakagawa", if (fr) sprintf("Les effets fixes expliquent %s de la variance de la réponse (R² marginal), les effets fixes et aléatoires %s (R² conditionnel ; Nakagawa et Schielzeth, 2013) ; %s ; en validation croisée, Q² = %s et RMSEcv = %s.",
                                     pourcent(r$r2$marginal), pourcent(r$r2$conditionnel), aic, nombre(m$vc$Q2), nombre(m$vc$RMSEcv, 3))
                     else sprintf("The fixed effects explain %s of the response's variance (marginal R²), the fixed and random effects %s (conditional R²; Nakagawa & Schielzeth, 2013); %s; in cross-validation, Q² = %s and RMSEcv = %s.",
                                  pourcent(r$r2$marginal), pourcent(r$r2$conditionnel), aic, nombre(m$vc$Q2), nombre(m$vc$RMSEcv, 3)))
  } else if (fr) sprintf("Le modèle explique %s de la variance de la réponse (R² ajusté = %s) ; %s ; en validation croisée, Q² = %s et RMSEcv = %s.",
                         pourcent(r$r2$r2), nombre(r$r2$r2_ajuste), aic, nombre(m$vc$Q2), nombre(m$vc$RMSEcv, 3))
  else sprintf("The model explains %s of the response's variance (adjusted R² = %s); %s; in cross-validation, Q² = %s and RMSEcv = %s.",
               pourcent(r$r2$r2), nombre(r$r2$r2_ajuste), aic, nombre(m$vc$Q2), nombre(m$vc$RMSEcv, 3))
  if (!is.null(m$test)) {
    st <- m$test$statistiques
    p4 <- c(p4, if (fr) sprintf("Un jeu de test de %d observations, laissées hors du modèle et de la validation croisée, a ensuite été prédit : RMSEP = %s, biais = %s, Q² externe = %s.",
                                m$test$n, nombre(st$RMSEP, 3), nombre(st$biais, 3), nombre(st$Q2ext))
                else sprintf("A test set of %d observations, left out of the model and of the cross-validation, was then predicted: RMSEP = %s, bias = %s, external Q² = %s.",
                             m$test$n, nombre(st$RMSEP, 3), nombre(st$biais, 3), nombre(st$Q2ext)))
  }

  # --- logiciel
  annee_r <- R.version$year
  annee_nuee <- format(Sys.Date(), "%Y")
  nlme <- utils::packageDescription("nlme")
  annee_nlme <- substr(nlme$Date %||% annee_r, 1, 4)
  cites <- union(cites, c("r", "nuee", if (mixte) "nlme"))
  p5 <- sprintf(if (fr) "Les calculs ont été faits avec Nuée%s (4rtefakt, %s) et R %s (R Core Team, %s)%s." else "Computations were done with Nuée%s (4rtefakt, %s) and R %s (R Core Team, %s)%s.",
                if (nzchar(version)) paste0(" ", version) else "", annee_nuee, paste(R.version$major, R.version$minor, sep = "."), annee_r,
                if (mixte) sprintf(if (fr) ", paquet nlme %s (Pinheiro et al., %s)" else ", package nlme %s (Pinheiro et al., %s)", nlme$Version, annee_nlme) else "")
  references <- vapply(cites, function(k) switch(k,
    r = sprintf(REFERENCES$r, annee_r),
    nuee = sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
    nlme = sprintf(REFERENCES_REGRESSION$nlme, annee_nlme, nlme$Version),
    REFERENCES_REGRESSION[[k]]), "")
  list(paragraphes = c(p1, paste(p2, collapse = " "), p3, paste(p4, collapse = " "), p5), references = unname(sort(references)))
}

# Ce que R affiche d'une régression (la console de l'application et son journal).
print.modele_regression <- function(x, ...) {
  r <- x$resultat
  type <- if (identical(x$type, "LMM")) traduire("modèle linéaire mixte", "linear mixed model") else traduire("régression linéaire", "linear regression")
  cat(sprintf(traduire("Modèle %s (%s)%s\n", "Model %s (%s)%s\n"), x$nom, type, if (nzchar(x$titre %||% "")) paste(" :", x$titre) else ""))
  cat(sprintf(traduire("%d observations, réponse %s%s\n", "%d observations, response %s%s\n"), x$N, x$y$noms,
              if (length(x$niveaux)) sprintf(traduire(", effets aléatoires %s%s", ", random effects %s%s"), paste(x$niveaux, collapse = " / "),
                                             if (x$car1) sprintf(traduire(", autocorrélation CAR1 dans le temps (%s)", ", CAR1 autocorrelation in time (%s)"), x$temps) else "") else ""))
  co <- r$coefficients
  print(data.frame(terme = co$terme, estimation = signif(co$estimation, 4), IC95 = sprintf("[%s ; %s]", signif(co$bas, 3), signif(co$haut, 3)),
                   p = signif(co$p, 3)), row.names = FALSE)
  cat(sprintf("AIC %.1f, AICc %.1f, BIC %.1f, logLik %.2f (%d %s)\n", r$AIC, r$AICc, r$BIC, r$logLik, r$parametres, traduire("paramètres", "parameters")))
  cat(paste(names(r$r2), signif(unlist(r$r2), 3), sep = " = ", collapse = ", "), sprintf("; Q2 = %.3f, RMSEcv = %.4g\n", x$vc$Q2, x$vc$RMSEcv))
  invisible(x)
}
