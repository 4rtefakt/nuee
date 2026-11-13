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

# Les groupes à l'intérieur desquels court l'autocorrélation CAR1 : les niveaux des effets
# aléatoires (g1/g2...), puis gc quand les données l'ont (un niveau plus fin, sans effet aléatoire
# à lui : le secteur dans son site, quand seul le site a un effet aléatoire).
groupement_car1 <- function(donnees, niveaux) paste(c(paste0("g", seq_len(niveaux)), if ("gc" %in% names(donnees)) "gc"), collapse = " / ")
correlation_car1 <- function(donnees, niveaux) {
  nlme::corCAR1(form = stats::as.formula(paste("~ temps |", groupement_car1(donnees, niveaux)), env = baseenv()))
}

# L'autocorrélation CAR1 demande un seul temps par groupe.
verifier_temps_uniques <- function(donnees, niveaux) {
  if (anyDuplicated(data.frame(groupe_car1(donnees, niveaux), donnees$temps))) {
    stop(traduire("L'autocorrélation dans le temps demande un seul temps par groupe : plusieurs observations ont le même temps dans un même groupe.",
                  "Autocorrelation in time needs a single time per group: several observations have the same time in the same group."), call. = FALSE)
  }
}

# Le groupe de l'autocorrélation CAR1 de chaque observation : le plus fin des effets aléatoires,
# ou le niveau plus fin où elle court (gc).
groupe_car1 <- function(donnees, niveaux) interaction(donnees[c(paste0("g", seq_len(niveaux)), intersect("gc", names(donnees)))], drop = TRUE)

# Un ajustement, ou NULL quand il échoue (nlme essaie optim, puis nlminb, comme le font les
# scripts qui ont servi de modèle). methode : "REML" ou "ML".
ajuster_lineaire <- function(donnees, termes, niveaux = 0, car1 = FALSE, methode = "REML") {
  f <- stats::reformulate(if (length(termes)) termes else "1", response = "y")
  if (!niveaux) {
    environment(f) <- baseenv()
    return(tryCatch(stats::lm(f, data = donnees), error = function(e) NULL))
  }
  aleatoire <- stats::as.formula(paste("~ 1 |", paste0("g", seq_len(niveaux), collapse = " / ")), env = baseenv())
  correlation <- if (car1) correlation_car1(donnees, niveaux)
  essai <- function(reglages) tryCatch(nlme::lme(f, random = aleatoire, correlation = correlation, data = donnees, method = methode,
                                                 control = reglages), error = function(e) NULL)
  m <- essai(nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE)) %||%
    essai(nlme::lmeControl(maxIter = 500, msMaxIter = 500, returnObject = TRUE))
  # (la formule elle-même dans l'appel gardé, et non son nom : predict() et formula() de nlme
  # 3.1-164 (Ubuntu 24.04) et d'avant réévaluent cet appel, où f n'existe plus)
  if (!is.null(m)) m$call$fixed <- f
  m
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
# ML pour la vraisemblance et l'AIC. noms : le nom affiché de chaque terme t1..tK.
# Renvoie des valeurs simples (pour l'interface), et les deux ajustements dans `ajustement`
# et `ajustement_ml` (à ne pas envoyer).
resultat_lineaire <- function(donnees, termes, noms, niveaux = 0, car1 = FALSE) {
  if (car1) verifier_temps_uniques(donnees, niveaux)
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
    # (CAR1 : les résidus normalisés dans l'ordre du temps de chaque groupe, regression_diagnostics.R ;
    # ceux de nlme ne sont justes que sur des lignes rangées par groupe, puis par temps)
    res$residus <- if (car1) residus_normalises_car1(as.vector(stats::residuals(estime, type = "pearson")), donnees$temps, groupe_car1(donnees, niveaux), res$phi)
                   else as.vector(stats::residuals(estime, type = "normalized"))
  } else {
    s <- summary(estime)
    res$r2 <- list(r2 = s$r.squared, r2_ajuste = s$adj.r.squared)
    res$ajuste <- as.vector(stats::fitted(estime))
    res$residus <- as.vector(stats::rstandard(estime))
    res$levier <- as.vector(stats::hatvalues(estime))
    res$cook <- as.vector(stats::cooks.distance(estime))
  }
  res$utiles <- utiles
  res$ajustement <- estime
  res$ajustement_ml <- compare
  res
}

# Le modèle `m` réajusté en ML avec les seuls termes `termes` (parmi ses termes utiles), tout le
# reste comme dans le modèle : ses effets aléatoires, son autocorrélation, sa loi, son décalage,
# ses courbes (celles de ces termes) et leur dimension. lm ou nlme::lme, mgcv::gam ou mgcv::gamm,
# sur les observations du modèle ; renvoie l'ajustement dont viennent la vraisemblance et l'AIC (la
# partie nlme d'un mgcv::gamm), ou NULL quand il échoue. Le modèle complet n'est pas refait.
ajuster_termes_ml <- function(m, termes) {
  gamm <- inherits(m$ajustement, "gamm")
  if (setequal(termes, m$utiles)) return(if (gamm) m$ajustement_ml$lme else m$ajustement_ml)
  niveaux <- length(m$niveaux)
  if (!inherits(m$ajustement, c("gam", "gamm"))) return(ajuster_lineaire(m$donnees, termes, niveaux, m$car1, "ML"))
  lis <- intersect(m$lisses, termes)
  if (gamm) return(ajuster_gamm(m$donnees, setdiff(termes, lis), lis, m$k, niveaux, "ML")$lme)
  ajuster_mgcv(m$donnees, setdiff(termes, lis), lis, m$k, niveaux, m$famille, !is.null(m$decalage), "ML")
}

# L'apport de chaque groupe de termes (un bloc, ou un terme seul) : le modèle `m` réajusté en ML
# sans lui (ajuster_termes_ml). ΔAIC = AIC sans le groupe - AIC du modèle complet (> 0 : le groupe
# améliore le modèle ; > 2, nettement, Burnham et Anderson, 2002) ; test du rapport de
# vraisemblance. groupes : une liste nommée de vecteurs de termes (t1..tK).
retrait_groupes <- function(m, groupes) {
  complet <- ajuster_termes_ml(m, m$utiles)
  ll <- as.numeric(stats::logLik(complet))
  df <- attr(stats::logLik(complet), "df")
  lignes <- lapply(names(groupes), function(nom) {
    sans <- setdiff(m$utiles, groupes[[nom]])
    retires <- length(m$utiles) - length(sans)
    if (!retires) return(NULL)
    a <- ajuster_termes_ml(m, sans)
    if (is.null(a)) return(data.frame(groupe = nom, termes = retires, delta_aic = NA_real_, khi2 = NA_real_, ddl = NA_real_, p = NA_real_))
    khi2 <- 2 * (ll - as.numeric(stats::logLik(a)))
    ddl <- df - attr(stats::logLik(a), "df")
    data.frame(groupe = nom, termes = retires, delta_aic = stats::AIC(a) - stats::AIC(complet), khi2 = khi2, ddl = ddl,
               p = if (ddl > 0) stats::pchisq(khi2, ddl, lower.tail = FALSE) else NA_real_, stringsAsFactors = FALSE)
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
REGRESSIONS <- c("LM", "LMM", "GLM", "GAM")
# (ajustés par mgcv : une loi de la réponse, des courbes, des effets aléatoires pénalisés)
GENERALISES <- c("GLM", "GAM")
est_regression <- function(d) isTRUE(as.character(d$type %||% "") %in% REGRESSIONS)

# Les valeurs qui font les groupes d'un effet aléatoire, ou le temps de l'autocorrélation (que
# temps_en_nombres lit) : un identifiant des observations (list(by = "id", id = "site")) ou une
# variable (list(by = "variable", variable = 4)), pour les observations `obs` (NA quand il manque).
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
  if (d$type %in% GENERALISES) {
    famille <- as.character(d$famille %||% "gaussienne")[1]
    if (!famille %in% FAMILLES) err("Loi de la réponse inconnue.", "Unknown distribution of the response.")
    # (mgcv::gamm : avec une autre loi que la normale, une quasi-vraisemblance pénalisée, sans AIC)
    if (!is.null(d$correlation) && famille != "gaussienne") {
      err("L'autocorrélation dans le temps d'un modèle généralisé ou additif demande la loi normale : avec une autre loi, mgcv::gamm passe par une quasi-vraisemblance pénalisée (PQL), sans vraie vraisemblance ni AIC.",
          "Autocorrelation in time in a generalized or additive model needs the normal distribution: with another one, mgcv::gamm uses penalized quasi-likelihood (PQL), with no true likelihood nor AIC.")
    }
    if (length(d$lisses) && !identical(d$type, "GAM")) err("Les courbes demandent un modèle additif (GAM).", "Curves need an additive model (GAM).")
    simples_x <- unlist(lapply(d$x, function(t) if (length(t$variables) == 1 && categorie(t) == 0) as.numeric(t$variables)))
    if (!all(as.numeric(d$lisses) %in% simples_x)) err("Une courbe se fait sur une variable X quantitative.", "A curve is made on a quantitative X variable.")
    if (length(d$k_courbes)) {  # (la dimension de base d'une courbe, demandée : k)
      k <- suppressWarnings(as.numeric(unlist(d$k_courbes)))
      if (anyNA(k) || any(k < 3 | k > 100 | k != round(k)) || !all(names(d$k_courbes) %in% as.character(as.numeric(d$lisses)))) {
        err("La dimension d'une courbe (k) est un nombre entier de 3 à 100, pour une variable en courbe.", "A curve's dimension (k) is a whole number from 3 to 100, for a variable in a curve.")
      }
    }
    if (!is.null(d$decalage)) {
      if (!famille %in% FAMILLES_LOG) err("Un décalage (l'effort) va avec une loi au lien log : Poisson, binomiale négative, gamma ou Tweedie.",
                                          "An offset (the effort) goes with a log-link distribution: Poisson, negative binomial, gamma or Tweedie.")
      if (length(variable_du_projet(projet, d$decalage)$categories)) err("Le décalage est une variable quantitative.", "The offset is a quantitative variable.")
    }
  } else if (length(d$lisses) || !is.null(d$decalage) || !is.null(d$famille)) {
    err("Lois, courbes et décalage demandent un modèle généralisé (GLM) ou additif (GAM).", "Distributions, curves and offsets need a generalized (GLM) or additive (GAM) model.")
  }
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
    dans <- d$correlation$dans
    if (!is.null(dans)) {  # (un niveau plus fin que le dernier effet aléatoire : le secteur dans son site)
      if (identical(as.character(dans$by)[1], "variable") && !length(variable_du_projet(projet, dans$variable)$categories)) {
        err("L'autocorrélation se fait à l'intérieur des groupes d'un identifiant ou d'une variable qualitative.", "Autocorrelation works within the groups of an ID or a qualitative variable.")
      }
      valeurs_groupement(projet, dans, d$observations[1])
    }
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
  temps <- dans <- NULL
  if (!is.null(d$correlation)) {
    # (des nombres, ou des dates en années décimales : φ est alors d'une année à l'autre)
    temps <- temps_en_nombres(valeurs_groupement(projet, d$correlation$temps, obs))
    # (un temps de nombres laisse de côté la ligne d'une valeur illisible, comme jusqu'à 0.9.5 et
    # comme le script ; un temps de dates refuse une valeur qui n'en est pas une)
    if (!is.null(attr(temps, "refus")) && isTRUE(attr(temps, "dates"))) stop(refus_temps(temps, nom_groupement(projet, d$correlation$temps)), call. = FALSE)
    if (all(is.na(temps))) stop(traduire("Aucune observation n'a de temps pour l'autocorrélation.", "No observation has a time for the autocorrelation."), call. = FALSE)
    if (!is.null(d$correlation$dans)) dans <- valeurs_groupement(projet, d$correlation$dans, obs)
  }
  decalage <- NULL
  if (!is.null(d$decalage)) {  # (l'effort : log(x) en décalage)
    effort <- colonne(projet, d$decalage, obs)
    if (any(effort <= 0, na.rm = TRUE)) stop(traduire("Le décalage (l'effort) doit être strictement positif.", "The offset (the effort) must be strictly positive."), call. = FALSE)
    decalage <- log(effort)
  }
  connu <- !is.na(y) & stats::complete.cases(bruts[, simples, drop = FALSE])
  # (un modèle choisi par la sélection par AIC, pour son ajustement : les observations où manque un
  # terme du modèle complet, ou que sa transformation rend manquant, restent de côté, comme dans
  # celui-ci ; un jeu de test garde les siennes ; definition_selection)
  complets <- if (is.null(imposees) && length(d$termes_complets)) termes_regression(d$termes_complets)
  simples_complets <- vapply(complets, function(t) length(t$variables) == 1, TRUE)
  if (length(complets)) connu <- connu & stats::complete.cases(valeurs_bloc(projet, complets, obs)[, simples_complets, drop = FALSE])
  for (v in niveaux) connu <- connu & !is.na(v)
  if (!is.null(temps)) connu <- connu & !is.na(temps)
  if (!is.null(dans)) connu <- connu & !is.na(dans)
  if (!is.null(decalage)) connu <- connu & !is.na(decalage)
  garde <- obs[connu]
  if (!length(garde)) stop(traduire("Aucune observation n'a toutes ses valeurs pour ce modèle.", "No observation has all its values for this model."), call. = FALSE)
  bloc <- if (K) construire_bloc(projet, termes, garde, imposees = imposees)
  cadre <- data.frame(y = y[connu], row.names = NULL)
  for (k in seq_len(K)) cadre[[paste0("t", k)]] <- bloc$X[, k]
  for (l in seq_along(niveaux)) cadre[[paste0("g", l)]] <- factor(niveaux[[l]][connu])
  # (mgcv : un niveau emboîté est un facteur à lui seul, le secteur dans son site)
  if (length(niveaux) >= 2) cadre$g12 <- interaction(cadre$g1, cadre$g2, drop = TRUE)
  if (length(niveaux) >= 3) cadre$g123 <- interaction(cadre$g1, cadre$g2, cadre$g3, drop = TRUE)
  if (!is.null(temps)) cadre$temps <- temps[connu]
  if (!is.null(dans)) cadre$gc <- factor(dans[connu])  # (le groupe de l'autocorrélation, plus fin que les effets aléatoires)
  if (!is.null(decalage)) cadre$decalage <- decalage[connu]
  complet <- stats::complete.cases(cadre)  # (un terme croisé que sa transformation rend manquant)
  if (!all(simples_complets)) complet <- complet & stats::complete.cases(construire_bloc(projet, complets, garde)$X)  # (de même dans le modèle complet)
  list(cadre = cadre[complet, , drop = FALSE], observations = garde[complet], exclues = setdiff(obs, garde[complet]),
       termes = if (K) paste0("t", seq_len(K)) else character(0), noms = if (K) bloc$noms else character(0), bloc = bloc)
}

# Ajuste une définition de régression (definir_modele, type "LM", "LMM", "GLM" ou "GAM") : le
# modèle, sa validation croisée par les groupes de la définition (groupes_definition) et son
# jeu de test.
ajuster_regression <- function(projet, d) {
  # (une définition relue d'un fichier : ses listes ont pu devenir des data.frames)
  d$aleatoires <- specs_groupement(d$aleatoires)
  if (!is.null(d$correlation)) {
    correlation <- list(type = as.character(d$correlation$type)[1], temps = specs_groupement(d$correlation$temps)[[1]])
    if (length(d$correlation$dans)) correlation$dans <- specs_groupement(d$correlation$dans)[[1]]
    d$correlation <- correlation
  }
  verifier_regression(projet, d)
  if (identical(d$type, "LMM") && !requireNamespace("nlme", quietly = TRUE)) {
    stop(traduire("Le paquet R nlme manque (il vient avec R) : installez-le pour les modèles mixtes.",
                  "The R package nlme is missing (it comes with R): install it for mixed models."), call. = FALSE)
  }
  generalise <- d$type %in% GENERALISES
  if (generalise && !requireNamespace("mgcv", quietly = TRUE)) {
    stop(traduire("Le paquet R mgcv manque (il vient avec R) : installez-le pour les modèles généralisés et additifs.",
                  "The R package mgcv is missing (it comes with R): install it for generalized and additive models."), call. = FALSE)
  }
  cadre <- cadre_regression(projet, d)
  niveaux <- length(d$aleatoires %||% list())
  car1 <- !is.null(d$correlation)
  if (nrow(cadre$cadre) <= length(cadre$termes) + 1) {
    stop(sprintf(traduire("Trop peu d'observations complètes (%d) pour %d termes.", "Too few complete observations (%d) for %d terms."),
                 nrow(cadre$cadre), length(cadre$termes)), call. = FALSE)
  }
  G <- d$cv_groups %||% 7
  groupes <- groupes_definition(projet, d, G)[match(cadre$observations, d$observations)]
  termes_x <- termes_regression(d$x)
  quantitatifs <- cadre$termes[vapply(termes_x, function(t) length(t$variables) == 1 && categorie(t) == 0, TRUE)]
  famille <- if (generalise) as.character(d$famille %||% "gaussienne")[1] else "gaussienne"
  lisses <- character(0)
  # (un modèle généralisé ou additif avec une autocorrélation : mgcv::gamm, nlme dessous)
  gamm <- generalise && car1
  if (generalise) {
    verifier_reponse(cadre$cadre$y, famille)
    lisses <- quantitatifs[vapply(termes_x[match(quantitatifs, cadre$termes)], function(t) as.numeric(t$variables) %in% as.numeric(d$lisses), TRUE)]
    # la dimension demandée d'une courbe (k), par terme
    k_demandes <- unlist(lapply(lisses, function(t) {
      k <- d$k_courbes[[as.character(as.numeric(termes_x[[match(t, cadre$termes)]]$variables))]]
      if (length(k)) stats::setNames(as.integer(k), t)
    }))
    if (gamm) {
      r <- resultat_gamm(cadre$cadre, cadre$termes, cadre$noms, lisses, niveaux, k_demandes)
      vc <- validation_gamm(cadre$cadre, r$utiles, lisses, r$k, groupes, niveaux)
    } else {
      r <- resultat_mgcv(cadre$cadre, cadre$termes, cadre$noms, lisses, niveaux, famille, !is.null(d$decalage), k_demandes)
      vc <- validation_mgcv(cadre$cadre, r$utiles, lisses, r$k, groupes, niveaux, famille, !is.null(d$decalage))
    }
  } else {
    r <- resultat_lineaire(cadre$cadre, cadre$termes, cadre$noms, niveaux, car1)
    vc <- validation_lineaire(cadre$cadre, r$utiles, groupes, niveaux, car1)
  }
  y <- d$y[[1]]
  m <- list(type = d$type, regression = TRUE, plsda = FALSE, opls = FALSE, A = 0, nom = d$name %||% "", titre = d$title %||% "",
            N = nrow(cadre$cadre), K = length(r$utiles), observations = cadre$observations, exclues = cadre$exclues,
            y = list(uid = as.numeric(y$variables), noms = as.character(y$name), transformation = nom_transformation(y$transform) %||% ""),
            noms_termes = cadre$noms, termes = cadre$termes, utiles = r$utiles, quantitatifs = quantitatifs,
            lisses = intersect(lisses, r$utiles), k = r$k, famille = famille, lien = r$lien %||% "identity",
            decalage = if (!is.null(d$decalage)) nom_groupement(projet, list(by = "variable", variable = d$decalage)),
            groupes_termes = stats::setNames(groupes_termes(projet, d$x), cadre$termes),
            niveaux = vapply(d$aleatoires %||% list(), function(s) nom_groupement(projet, s), ""),
            temps = if (car1) nom_groupement(projet, d$correlation$temps), car1 = car1, gamm = gamm,
            dans = if (car1 && !is.null(d$correlation$dans)) nom_groupement(projet, d$correlation$dans),
            resultat = r[setdiff(names(r), c("ajustement", "ajustement_ml", "utiles", "k"))],
            ajustement = r$ajustement, ajustement_ml = r$ajustement_ml, donnees = cadre$cadre, bloc = cadre$bloc,
            G = length(unique(groupes)), vc_par = nom_groupes_vc(projet, d),
            vc = list(Q2 = vc$Q2, RMSEcv = vc$RMSEcv, prediction = vc$prediction, groupe = groupes, echecs = vc$echecs))
  # les modèles que l'AIC compare : même réponse sur les mêmes observations, une loi de même
  # mesure (une loi discrète ne se compare pas à une continue), et la même vraisemblance (celle
  # d'un modèle mixte de nlme, marginale, ne se compare pas à celle, conditionnelle, des effets
  # aléatoires de mgcv, ni à celle de mgcv::gamm, où les courbes sont des effets aléatoires)
  m$signature <- paste(m$y$uid, m$y$transformation, paste(sort(m$observations), collapse = ","), mesure_famille(famille),
                       if (gamm) "gamm" else if (generalise && niveaux) "conditionnelle" else "marginale")
  class(m) <- "modele_regression"
  if (length(d$test)) m$test <- tester_regression(projet, m, d)
  m
}

# Les prédictions d'une régression pour des observations nouvelles (les colonnes de
# cadre_regression) : par les effets fixes seuls quand le modèle a des effets aléatoires.
predire_regression <- function(m, nd) {
  a <- m$ajustement
  if (inherits(a, "gamm")) return(as.vector(stats::predict(a$gam, nd, type = "response")))  # ($gam : sans les effets aléatoires)
  if (inherits(a, "gam")) return(predire_mgcv(a, nd, m$donnees, length(m$niveaux)))
  if (inherits(a, "lme")) return(as.vector(stats::predict(a, nd, level = 0)))
  as.vector(stats::predict(a, nd))
}

# Le jeu de test d'une régression : ses observations prédites par les effets fixes (un modèle
# mixte ne connaît pas leurs groupes : niveau 0), dans la forme de evaluer_test (jeu_test.R).
tester_regression <- function(projet, m, d) {
  t <- cadre_regression(projet, d, d$test, imposees = m$bloc)
  nd <- t$cadre
  predit <- predire_regression(m, nd)
  y <- m$y$noms
  e <- as.vector(predit) - nd$y
  obs <- data.frame(uid = t$observations, nom = unname(projet$noms_obs[as.character(t$observations)]), hors_domaine = FALSE, stringsAsFactors = FALSE)
  obs[[paste(y, "observé")]] <- nd$y
  obs[[paste(y, "prédit")]] <- as.vector(predit)
  list(n = nrow(nd), hors_domaine = 0, observations = obs,
       statistiques = data.frame(y = y, n = length(e), RMSEP = sqrt(mean(e^2)), biais = mean(e), SEP = if (length(e) > 1) stats::sd(e) else NA_real_,
                                 Q2ext = 1 - sum(e^2) / sum((nd$y - mean(m$donnees$y))^2), stringsAsFactors = FALSE))
}

# Les prédictions d'une régression pour des observations du projet, d'un jeu de prédiction ou
# d'un fichier (prediction.R : predire, predire_fichier) : Y prédit par les effets fixes, sur
# l'échelle de la réponse modélisée (sa transformation comprise) ; Y observé quand il est connu ;
# hors_domaine : une valeur d'un terme hors de l'étendue des observations du modèle (une
# extrapolation). Une observation sans une valeur d'un terme du modèle, ou sans son effort, n'a
# pas de prédiction. `effort_absent` : le fichier n'a pas l'effort ; la prédiction est alors
# pour un effort de 1 (une densité).
predire_observations_regression <- function(projet, m, d, observations, effort_absent = FALSE) {
  n <- length(observations)
  termes <- termes_regression(d$x)
  nd <- data.frame(row.names = seq_len(n))
  if (length(termes)) {
    bloc <- construire_bloc(projet, termes, observations, imposees = m$bloc)
    for (k in seq_along(termes)) nd[[paste0("t", k)]] <- bloc$X[, k]
  }
  if (!is.null(d$decalage)) {
    effort <- if (effort_absent) rep(1, n) else colonne(projet, d$decalage, observations)
    nd$decalage <- ifelse(!is.na(effort) & effort > 0, log(abs(effort)), NA_real_)
  }
  utilisees <- intersect(c(m$utiles, "decalage"), names(nd))
  connu <- if (length(utilisees)) stats::complete.cases(nd[utilisees]) else rep(TRUE, n)  # (sans X : le modèle nul)
  predit <- rep(NA_real_, n)
  if (any(connu)) predit[connu] <- predire_regression(m, nd[connu, , drop = FALSE])
  hors <- rep(FALSE, n)
  for (t in m$utiles) {
    r <- range(m$donnees[[t]])
    marge <- 1e-9 * max(1, abs(r))
    hors <- hors | (!is.na(nd[[t]]) & (nd[[t]] < r[1] - marge | nd[[t]] > r[2] + marge))
  }
  y <- transformer(valeurs_simples(projet, d$y[[1]], observations), d$y[[1]]$transform)
  out <- data.frame(row.names = as.character(observations))
  out[[paste(m$y$noms, "prédit")]] <- predit
  if (any(!is.na(y))) out[[paste(m$y$noms, "observé")]] <- y
  out$hors_domaine <- hors
  out
}

# Le quantile de la loi normale de chaque résidu de `e` (la droite de Henry, positions de Blom), NA
# pour un résidu manquant.
quantiles_henry <- function(e) {
  q <- rep(NA_real_, length(e))
  ok <- !is.na(e)
  q[ok] <- stats::qnorm((rank(e[ok]) - 0.375) / (sum(ok) + 0.25))
  q
}

# Les observations d'une régression : Y observé, ajusté (et par les effets fixes seuls), prédit
# en validation croisée, résidu standardisé et son quantile normal, levier et distance de Cook
# (lm), groupes des effets aléatoires et de la validation croisée.
observations_regression <- function(projet, m) {
  r <- m$resultat
  y <- m$y$noms
  obs <- data.frame(uid = m$observations, nom = unname(projet$noms_obs[as.character(m$observations)]), stringsAsFactors = FALSE)
  obs[[paste(y, "observé")]] <- m$donnees$y
  obs[[paste(y, "ajusté")]] <- r$ajuste
  if (!is.null(r$ajuste_fixe)) obs[[paste(y, "ajusté (effets fixes)")]] <- r$ajuste_fixe
  obs[[paste(y, "prédit (VC)")]] <- m$vc$prediction
  obs$residu <- r$residus
  obs$quantile_normal <- quantiles_henry(r$residus)
  if (!is.null(r$levier)) {
    obs$levier <- r$levier
    obs$cook <- r$cook
  }
  for (l in seq_along(m$niveaux)) obs[[m$niveaux[l]]] <- as.character(m$donnees[[paste0("g", l)]])
  obs$groupe_vc <- m$vc$groupe
  obs
}

# Les variances des effets aléatoires, nommées par leur groupement (nlme : avec la variance
# résiduelle ; mgcv : les effets aléatoires seuls), ou NULL.
variances_regression <- function(m) {
  v <- m$resultat$variances
  if (!is.null(v)) v$composante <- c(m$niveaux, traduire("résiduelle", "residual"))[seq_len(nrow(v))]
  v
}

# Les résultats d'une régression en tableaux (formats.R : tables_resultats, pour Exporter) :
# observations, coefficients, qualité de l'ajustement (R2, Q2, AIC...), effets aléatoires et
# courbes s'il y en a.
tables_regression <- function(projet, m) {
  r <- m$resultat
  obs <- observations_regression(projet, m)
  obs$uid <- NULL
  names(obs)[names(obs) == "nom"] <- "Observation"
  qualite <- c(list(N = m$N), r$r2, list(Q2 = m$vc$Q2, RMSEE = sqrt(mean((m$donnees$y - r$ajuste)^2)), RMSEcv = m$vc$RMSEcv,
                                        AIC = r$AIC, AICc = r$AICc, BIC = r$BIC, logLik = r$logLik, parametres = r$parametres))
  qualite <- as.data.frame(lapply(qualite, function(v) if (is.null(v)) NA_real_ else v), check.names = FALSE)
  tables <- list(observations = obs, coefficients = r$coefficients, qualite = qualite)
  if (!is.null(r$variances)) tables$effets_aleatoires <- variances_regression(m)
  if (!is.null(r$courbes)) tables$courbes <- r$courbes
  tables
}

# Les groupes de termes d'un modèle : le bloc de chaque terme (colonne Bloc de Nouveau modèle),
# sinon sa variable (groupes_termes), parmi ses termes utiles et dans leur ordre ; une liste
# nommée de vecteurs de termes (t1..tK).
groupes_modele <- function(m) {
  g <- m$groupes_termes[m$utiles]
  split(names(g), factor(unname(g), levels = unique(unname(g))))
}

# L'apport de chaque groupe de termes du modèle (son bloc, sinon sa variable), calculé à la
# demande : retrait_groupes.
apport_groupes <- function(m) {
  if (!length(m$utiles)) return(NULL)
  retrait_groupes(m, groupes_modele(m))
}

# ------------------------------------------------------------ sélection par l'AIC

# Les groupes que demande chaque groupe de termes : ceux des facteurs de ses termes croisés et de
# ses carrés (un terme croisé n'entre qu'avec ses effets principaux). `termes` : les termes X de
# la définition, t1..tK dans l'ordre (termes_regression) ; une liste nommée comme `groupes`.
groupes_requis <- function(groupes, termes) {
  cles <- vapply(termes, function(t) if (length(t$variables) == 1) paste(t$variables[1], categorie(t)) else "", "")
  groupe_de <- stats::setNames(rep(names(groupes), lengths(groupes)), unlist(groupes, use.names = FALSE))
  lapply(stats::setNames(names(groupes), names(groupes)), function(nom) {
    facteurs <- unlist(lapply(groupes[[nom]], function(tk) {
      t <- termes[[as.integer(sub("^t", "", tk))]]
      if (length(t$variables) < 2) return(character(0))
      paste0("t", match(vapply(seq_along(t$variables), function(j) paste(t$variables[j], categorie(t, j)), ""), cles))
    }))
    setdiff(unique(unname(groupe_de[intersect(facteurs, names(groupe_de))])), nom)
  })
}

# La sélection des effets fixes par l'AICc (Burnham et Anderson, 2002), calculée à la demande : les
# combinaisons des groupes de termes du modèle `m` (groupes_modele), du modèle nul au modèle
# complet, chacune ajustée en ML sur les observations du modèle (ajuster_termes_ml) ; ses effets
# aléatoires, son autocorrélation, sa loi, son décalage et le réglage de ses courbes restent ceux
# du modèle. Un terme croisé ou un carré n'entre qu'avec ses effets principaux (groupes_requis ;
# `d` : la définition du modèle). Toutes les combinaisons quand 2^K ≤ `limite` (sur 240
# observations, les 256 combinaisons de 8 groupes d'un nlme::lme avec CAR1 prennent une dizaine de
# secondes, celles d'un mgcv::gamm avec deux courbes une demi-minute) ; au-delà, une élimination
# descendante depuis le modèle complet : à chaque pas, le retrait qui baisse le plus l'AICc, jusqu'à
# ce qu'aucun ne la baisse. Les modèles sont rangés par AICc : ΔAICc, poids d'Akaike (sur les
# modèles ajustés) et leur cumul ; l'importance d'un groupe est la somme des poids des modèles qui
# le contiennent. Le meilleur modèle a l'AICc le plus bas (le premier) ; le plus parcimonieux, le
# moins de paramètres parmi ceux à ΔAICc ≤ 2 (Burnham et Anderson, 2002).
selection_aic <- function(m, d, limite = 256) {
  groupes <- groupes_modele(m)
  K <- length(groupes)
  n <- nrow(m$donnees)
  if (!K) return(list(K = 0L, N = n))
  noms <- names(groupes)
  requis <- groupes_requis(groupes, termes_regression(d$x))
  permis <- function(b) all(vapply(noms[b], function(g) all(requis[[g]] %in% noms[b]), TRUE))
  evaluer <- function(b) {  # (un candidat : les groupes qu'il contient)
    a <- ajuster_termes_ml(m, intersect(m$utiles, unlist(groupes[b], use.names = FALSE)))
    if (is.null(a)) return(c(k = NA_real_, logLik = NA_real_, AICc = NA_real_))
    ll <- stats::logLik(a)
    c(k = attr(ll, "df"), logLik = as.numeric(ll), AICc = aic_corrige(a, n))
  }
  meilleure <- function(a, b) !is.na(a) && (is.na(b) || a < b)
  toutes <- 2^K <= limite
  chemin <- NULL
  if (toutes) {
    B <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), K), KEEP.OUT.ATTRS = FALSE))
    B <- B[apply(B, 1, permis), , drop = FALSE]
    B <- B[order(rowSums(B)), , drop = FALSE]
    valeurs <- t(apply(B, 1, evaluer))
    etape <- rep(NA_integer_, nrow(B))
  } else {
    courant <- rep(TRUE, K)
    B <- matrix(courant, 1)
    valeurs <- t(evaluer(courant))
    etape <- 0L
    chemin <- data.frame(etape = 0L, retire = NA_character_, AICc = unname(valeurs[1, "AICc"]), stringsAsFactors = FALSE)
    repeat {
      # (un groupe ne se retire pas tant qu'un groupe gardé le demande)
      retirables <- which(courant & !vapply(noms, function(g) any(vapply(noms[courant], function(h) g %in% requis[[h]], TRUE)), TRUE))
      if (!length(retirables)) break
      essais <- lapply(retirables, function(j) replace(courant, j, FALSE))
      v <- t(vapply(essais, evaluer, c(k = 0, logLik = 0, AICc = 0)))
      pas <- max(etape) + 1L
      B <- rbind(B, do.call(rbind, essais))
      valeurs <- rbind(valeurs, v)
      etape <- c(etape, rep(pas, length(essais)))
      j <- which.min(v[, "AICc"])
      if (!length(j) || !meilleure(v[j, "AICc"], chemin$AICc[nrow(chemin)])) break
      courant <- essais[[j]]
      chemin <- rbind(chemin, data.frame(etape = pas, retire = noms[retirables[j]], AICc = unname(v[j, "AICc"]), stringsAsFactors = FALSE))
    }
    rownames(chemin) <- NULL
  }
  # le classement : AICc croissant (à égalité, le moins de paramètres d'abord), les échecs à la fin
  ordre <- order(valeurs[, "AICc"], valeurs[, "k"], na.last = TRUE)
  B <- B[ordre, , drop = FALSE]
  valeurs <- valeurs[ordre, , drop = FALSE]
  aicc <- valeurs[, "AICc"]
  ok <- !is.na(aicc)
  if (!any(ok)) stop(traduire("Aucun modèle n'a d'AICc : trop peu d'observations pour leurs paramètres.",
                              "No model has an AICc: too few observations for their parameters."), call. = FALSE)
  delta <- aicc - min(aicc[ok])
  poids <- exp(-delta / 2) / sum(exp(-delta[ok] / 2))
  cumul <- cumsum(ifelse(ok, poids, 0))
  cumul[!ok] <- NA
  proches <- which(ok & delta <= 2)
  list(methode = if (toutes) "toutes" else "descendante", limite = limite, N = n, K = K, groupes = I(noms),
       termes = unname(lapply(groupes, function(ts) I(m$noms_termes[match(ts, m$termes)]))), requis = unname(lapply(requis, I)),
       modeles = data.frame(modele = apply(B, 1, function(b) if (any(b)) paste(noms[b], collapse = " + ") else "1"), n_groupes = rowSums(B),
                            k = valeurs[, "k"], logLik = valeurs[, "logLik"], AICc = aicc, delta = delta, poids = poids, poids_cumule = cumul,
                            etape = etape[ordre], row.names = NULL, stringsAsFactors = FALSE),
       contient = lapply(seq_len(K), function(j) I(as.integer(B[, j]))),
       importance = data.frame(groupe = noms, importance = colSums(B[ok, , drop = FALSE] * poids[ok]), row.names = NULL, stringsAsFactors = FALSE),
       meilleur = 1L, parcimonieux = proches[order(valeurs[proches, "k"], delta[proches])][1], chemin = chemin, echecs = sum(!ok))
}

# La définition d'un modèle choisi dans la sélection par l'AIC (Ajuster ce modèle) : celle du
# modèle `m` (de définition `d`) avec les seuls termes des groupes `choisis` (un terme croisé garde
# ses facteurs) ; les mêmes réglages, les courbes et leur dimension de ses variables qui restent ;
# ses observations, celles du modèle : la sélection les a toutes comparées sur elles (une valeur
# manquante d'une variable retirée en écartait peut-être, qui restent écartées).
definition_selection <- function(d, m, choisis) {
  choisis <- as.character(unlist(choisis))
  # (les termes du modèle complet, ou ceux du modèle d'où il venait lui-même : une observation où
  # l'un manque reste de côté, comme dans celui-ci, et le modèle choisi est ajusté sur les lignes où
  # la sélection l'a comparé aux autres ; la liste des observations ne change pas, ni les groupes de
  # la validation croisée « à tour de rôle », qui sont des rangs dans cette liste)
  complets <- if (length(d$termes_complets)) d$termes_complets else d$x
  inconnus <- setdiff(choisis, names(groupes_modele(m)))
  if (length(inconnus)) stop(sprintf(traduire("Groupe de termes inconnu dans ce modèle : %s", "Unknown group of terms in this model: %s"),
                                     paste(inconnus, collapse = ", ")), call. = FALSE)
  garde <- unname(m$groupes_termes) %in% choisis  # (le groupe de chaque terme de d$x, dans l'ordre)
  simple <- vapply(d$x, function(t) length(t$variables) == 1, TRUE)
  cle <- vapply(d$x, function(t) paste(t$variables[1], categorie(t)), "")
  facteurs <- unlist(lapply(d$x[garde & !simple], function(t) vapply(seq_along(t$variables), function(j) paste(t$variables[j], categorie(t, j)), "")))
  d$x <- d$x[garde | (simple & cle %in% facteurs)]
  restantes <- unlist(lapply(d$x, function(t) if (length(t$variables) == 1) as.numeric(t$variables)))
  d$lisses <- intersect(as.numeric(d$lisses), restantes)
  if (!length(d$lisses)) d$lisses <- NULL
  d$k_courbes <- d$k_courbes[names(d$k_courbes) %in% as.character(d$lisses)]
  if (!length(d$k_courbes)) d$k_courbes <- NULL
  d$termes_complets <- complets
  d$selection_origine <- list(modele = as.character(d$name %||% ""), groupes = choisis, n = length(m$observations))
  # (un modèle mixte BACI : sans le terme croisé période × impact, ce n'est plus lui ; avec, il en dérive)
  if (!is.null(d$baci_origine)) {
    if (any(vapply(d$x, function(t) length(t$variables) > 1, TRUE))) d$baci_origine$derive <- TRUE else d$baci_origine <- NULL
  }
  d
}

# ------------------------------------------------ une autocorrélation ajoutée (Ajuster avec CAR1)

# La définition d'un modèle refait avec une autocorrélation CAR1 (Autocorrélation des résidus ›
# Ajuster avec CAR1) : celle du modèle, `d`, avec une autocorrélation le long de `temps` (un
# groupement : un identifiant ou une variable quantitative) à l'intérieur des groupes de ses effets
# aléatoires, ou d'un niveau plus fin en eux (le secteur dans son site). `groupe` : ces groupes tels
# qu'autocorrelation_residus les prend, list(by = "effets"), puis au besoin le niveau plus fin, qui
# devient `dans`. Le reste ne change pas. Refusée quand le modèle ne la prend pas : sans effet
# aléatoire, avec déjà une autocorrélation, d'autres groupes que ceux de ses effets aléatoires (CAR1
# court dans le dernier ou dans un niveau plus fin), une autre loi que la normale (verifier_regression).
definition_car1 <- function(projet, d, temps, groupe) {
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  d$aleatoires <- specs_groupement(d$aleatoires)  # (une définition relue d'un fichier : ses listes ont pu devenir des data.frames)
  if (!length(d$aleatoires)) err("Ce modèle n'a pas d'effet aléatoire : une autocorrélation CAR1 se fait à l'intérieur des groupes d'un effet aléatoire.",
                                 "This model has no random effect: a CAR1 autocorrelation works within the groups of a random effect.")
  if (!is.null(d$correlation)) err("Ce modèle a déjà une autocorrélation CAR1.", "This model already has a CAR1 autocorrelation.")
  if (is.null(temps)) err("Choisissez le temps (une année, un jour...) : un identifiant ou une variable quantitative.",
                          "Choose the time (a year, a day...): an ID or a quantitative variable.")
  if (!is.null(groupe$by)) groupe <- list(groupe)
  effets <- vapply(groupe, function(s) identical(as.character(s$by)[1], "effets"), TRUE)
  if (!length(effets) || !effets[1] || length(effets) > 2 || any(effets[-1])) {
    err("L'autocorrélation CAR1 se fait à l'intérieur des groupes des effets aléatoires du modèle, ou d'un niveau plus fin en eux : choisissez ses effets aléatoires comme groupes.",
        "A CAR1 autocorrelation works within the groups of the model's random effects, or of a finer level within them: choose its random effects as the groups.")
  }
  d$correlation <- c(list(type = "CAR1", temps = specs_groupement(temps)[[1]]), if (length(groupe) == 2) list(dans = specs_groupement(groupe[[2]])[[1]]))
  verifier_regression(projet, d)
  # (un modèle mixte BACI : CAR1 le long de son temps dans ses unités en fait le modèle BACI avec CAR1,
  # qui en dérive ; une autre autocorrélation n'est plus celle que ses phrases décrivent)
  if (!is.null(d$baci_origine)) {
    o <- d$baci_origine
    if (is.null(d$correlation$dans) && identical(d$correlation$temps, groupement_baci(projet, o$temps))) {
      d$baci_origine$car1 <- TRUE
      d$baci_origine$derive <- TRUE
    } else d$baci_origine <- NULL
  }
  d
}

# ------------------------------------------------------------ modèles généralisés et additifs

# Les lois de la réponse (mgcv) : leur lien est le lien canonique, log pour la loi gamma. Une
# loi discrète (comptages, présence-absence) ne se compare pas par l'AIC à une loi continue.
FAMILLES <- c("gaussienne", "poisson", "binomiale_negative", "binomiale", "gamma", "tweedie")
FAMILLES_LOG <- c("poisson", "binomiale_negative", "gamma", "tweedie")
mesure_famille <- function(famille) if (famille %in% c("poisson", "binomiale_negative", "binomiale")) "discrete" else if (identical(famille, "tweedie")) "tweedie" else "continue"

# Une famille de mgcv qui marche sans que mgcv soit attaché : celles de mgcv (tw, nb) appellent
# des fonctions de mgcv depuis un environnement dont le parent est l'environnement global ; on
# les y met.
famille_mgcv <- function(nom) {
  f <- switch(as.character(nom)[1],
    gaussienne = stats::gaussian(), poisson = stats::poisson(), binomiale = stats::binomial(),
    binomiale_negative = mgcv::nb(), gamma = stats::Gamma(link = "log"), tweedie = mgcv::tw(),
    stop(sprintf(traduire("Loi inconnue : %s", "Unknown distribution: %s"), nom), call. = FALSE))
  ns <- asNamespace("mgcv")
  for (g in Filter(is.function, f)) {
    e <- environment(g)
    if (is.null(e) || isNamespace(e) || identical(e, baseenv())) next
    for (n in unique(all.names(body(g)))) {
      if (!exists(n, envir = e) && exists(n, envir = ns, inherits = FALSE)) assign(n, get(n, envir = ns), envir = e)
    }
  }
  f
}

# Une réponse que la loi accepte : des comptages (Poisson, binomiale négative), 0 ou 1
# (binomiale), strictement positive (gamma), positive ou nulle (Tweedie).
verifier_reponse <- function(y, famille) {
  ok <- switch(famille,
    poisson = , binomiale_negative = all(y >= 0 & abs(y - round(y)) < 1e-8),
    binomiale = all(y %in% c(0, 1)),
    gamma = all(y > 0),
    tweedie = all(y >= 0),
    TRUE)
  if (!ok) stop(switch(famille,
    poisson = , binomiale_negative = traduire("Une loi de Poisson ou binomiale négative demande des comptages (des entiers positifs ou nuls).",
                                              "A Poisson or negative binomial distribution needs counts (whole numbers, zero or more)."),
    binomiale = traduire("Une loi binomiale demande une réponse 0 ou 1 (absence ou présence).", "A binomial distribution needs a 0 or 1 response (absence or presence)."),
    gamma = traduire("Une loi gamma demande une réponse strictement positive.", "A gamma distribution needs a strictly positive response."),
    traduire("Une loi de Tweedie demande une réponse positive ou nulle.", "A Tweedie distribution needs a response of zero or more.")), call. = FALSE)
}

# La formule d'un modèle de mgcv, écrite avec les seuls noms internes : termes paramétriques,
# courbes s(tk, k = ...), effets aléatoires s(g1, bs = "re") (g12 : le niveau 2 dans le 1...),
# décalage. Son environnement donne s() de mgcv, qui relit ces termes : ils ne viennent que d'ici.
formule_mgcv <- function(parametriques, lisses, k, niveaux, decalage) {
  aleatoires <- c("s(g1, bs = \"re\")", "s(g12, bs = \"re\")", "s(g123, bs = \"re\")")[seq_len(niveaux)]
  termes <- c(parametriques, if (length(lisses)) sprintf("s(%s, k = %d)", lisses, as.integer(k[lisses])), aleatoires, if (decalage) "offset(decalage)")
  stats::as.formula(paste("y ~", if (length(termes)) paste(termes, collapse = " + ") else "1"), env = new.env(parent = asNamespace("mgcv")))
}

ajuster_mgcv <- function(donnees, parametriques, lisses, k, niveaux, famille, decalage, methode = "REML") {
  tryCatch(mgcv::gam(formule_mgcv(parametriques, lisses, k, niveaux, decalage), data = donnees, family = famille_mgcv(famille), method = methode),
           error = function(e) NULL)
}

# La dimension de base d'une courbe : `k` si elle est demandée, sinon 10 au plus ; moins quand la
# variable a peu de valeurs.
dimension_courbe <- function(x, k = NA) min(if (length(k) && !is.na(k[1])) as.integer(k[1]) else 10L, length(unique(x)) - 1L)

# Les prédictions d'un modèle de mgcv sur l'échelle de la réponse, par ses effets fixes (ses
# effets aléatoires exclus : un groupe jamais vu ; on donne aux observations un groupe connu du
# modèle, que l'exclusion annule).
predire_mgcv <- function(a, nd, appris, niveaux) {
  noms <- c("g1", "g12", "g123")[seq_len(niveaux)]
  for (g in noms) nd[[g]] <- factor(rep(levels(appris[[g]])[1], nrow(nd)), levels = levels(appris[[g]]))
  as.vector(stats::predict(a, nd, type = "response", exclude = sprintf("s(%s)", noms)))
}

parametre_famille <- function(a, famille) {
  if (famille == "binomiale_negative") return(list(nom = "theta", valeur = a$family$getTheta(TRUE)))
  if (famille == "tweedie") return(list(nom = "p", valeur = a$family$getTheta(TRUE)))
  NULL
}

# Le modèle ajusté sur toutes ses observations, comme resultat_lineaire : REML pour les
# coefficients, les courbes et les effets aléatoires (Wood, 2011), ML pour la vraisemblance et
# l'AIC ; un modèle sans courbe ni effet aléatoire n'a qu'un ajustement. lisses : les termes
# en courbe.
# La concurvité de chaque courbe (mgcv::concurvity, full = TRUE), ajoutée au tableau des courbes :
# la part de la courbe que les autres termes du modèle reproduisent, de 0 à 1 (« estimate », et
# « worst », le pire cas) ; au-delà de 0,8, la courbe se confond avec eux et son estimation est
# instable (Wood 2017). Le tableau tel quel quand mgcv ne la calcule pas.
concurvite_courbes <- function(a, lignes, courbes) {
  cc <- tryCatch(mgcv::concurvity(a, full = TRUE), error = function(e) NULL)
  if (is.null(cc) || !all(lignes %in% colnames(cc))) return(courbes)
  courbes$concurvite <- unname(cc["estimate", lignes])
  courbes$concurvite_pire <- unname(cc["worst", lignes])
  courbes
}

resultat_mgcv <- function(donnees, termes, noms, lisses, niveaux, famille, decalage, k_demandes = NULL) {
  utiles <- termes_utiles(donnees, termes)
  retires <- noms[match(setdiff(termes, utiles), termes)]
  lis <- intersect(lisses, utiles)
  k <- vapply(lis, function(t) dimension_courbe(donnees[[t]], if (t %in% names(k_demandes)) k_demandes[[t]] else NA), 0L)
  if (any(k < 3)) {
    stop(sprintf(traduire("Trop peu de valeurs différentes pour une courbe : %s.", "Too few different values for a curve: %s."),
                 paste(noms[match(lis[k < 3], termes)], collapse = ", ")), call. = FALSE)
  }
  par <- setdiff(utiles, lis)
  # (sans courbe ni effet aléatoire, un modèle linéaire généralisé ordinaire : en ML, comme glm()
  # et MASS::glm.nb(), le θ de la binomiale négative compris)
  penalise <- length(lis) > 0 || niveaux > 0
  estime <- ajuster_mgcv(donnees, par, lis, k, niveaux, famille, decalage, if (penalise) "REML" else "ML")
  compare <- if (penalise) ajuster_mgcv(donnees, par, lis, k, niveaux, famille, decalage, "ML") else estime
  if (is.null(estime) || is.null(compare)) {
    stop(traduire("Le modèle n'a pas pu être ajusté : essayez moins de termes, de courbes ou d'effets aléatoires, ou une autre loi.",
                  "The model could not be fitted: try fewer terms, curves or random effects, or another distribution."), call. = FALSE)
  }
  s <- summary(estime)
  pt <- s$p.table
  echelle <- isTRUE(estime$scale.estimated)
  q <- if (echelle) stats::qt(0.975, estime$df.residual) else stats::qnorm(0.975)
  co <- data.frame(terme = c("(Intercept)", noms[match(rownames(pt)[-1], termes)]), estimation = pt[, 1], erreur_type = pt[, 2],
                   ddl = if (echelle) estime$df.residual else NA_real_, t = pt[, 3], p = pt[, 4],
                   bas = pt[, 1] - q * pt[, 2], haut = pt[, 1] + q * pt[, 2], row.names = NULL, stringsAsFactors = FALSE)
  courbes <- NULL
  st <- s$s.table
  if (length(lis)) {
    lignes <- sprintf("s(%s)", lis)
    courbes <- data.frame(terme = noms[match(lis, termes)], edf = st[lignes, 1], ref_ddl = st[lignes, 2], statistique = st[lignes, 3], p = st[lignes, 4],
                          row.names = NULL, stringsAsFactors = FALSE)
    courbes <- concurvite_courbes(estime, lignes, courbes)
  }
  variances <- NULL
  if (niveaux) {
    vc <- tryCatch(mgcv::gam.vcomp(estime), error = function(e) NULL)
    et <- if (is.matrix(vc)) vc[, 1] else if (is.list(vc) && is.matrix(vc$vc)) vc$vc[, 1] else NULL  # (les écarts-types, nommés)
    noms_re <- c("s(g1)", "s(g12)", "s(g123)")[seq_len(niveaux)]
    if (!is.null(et)) variances <- data.frame(composante = paste0("g", seq_len(niveaux)), variance = unname(et[noms_re])^2, stringsAsFactors = FALSE)
  }
  ll <- stats::logLik(compare)
  n <- nrow(donnees)
  list(N = n, coefficients = co, courbes = courbes, termes_retires = retires, logLik = as.numeric(ll), parametres = attr(ll, "df"),
       AIC = stats::AIC(compare), AICc = aic_corrige(compare, n), BIC = stats::BIC(compare),
       r2 = list(r2_ajuste = s$r.sq, deviance_expliquee = s$dev.expl), variances = variances,
       famille = famille, lien = estime$family$link, parametre_loi = parametre_famille(estime, famille), echelle = estime$sig2,
       # (Poisson : la dispersion, khi² de Pearson sur ses degrés de liberté ; au-delà de 1,5, des
       # comptages surdispersés, que la binomiale négative modélise. Une réponse 0 ou 1 ne la montre pas.)
       dispersion = if (famille == "poisson") sum(stats::residuals(estime, type = "pearson")^2) / estime$df.residual,
       ajuste = as.vector(stats::fitted(estime)),
       residus = as.vector(stats::residuals(estime, type = "deviance")) / sqrt(estime$sig2 * pmax(1 - estime$hat, 1e-8)),
       # (comptages, présence-absence, valeurs positives : les résidus quantiles, regression_diagnostics.R ; NULL sinon)
       residus_quantiles = residus_quantiles(estime, famille),
       utiles = utiles, k = k, ajustement = estime, ajustement_ml = compare)
}

# Validation croisée d'un modèle de mgcv : chaque groupe prédit (échelle de la réponse) par le
# modèle ajusté sans lui ; une courbe dont la variable a trop peu de valeurs sur ces
# observations y devient un terme linéaire.
validation_mgcv <- function(donnees, termes, lisses, k, groupe, niveaux, famille, decalage) {
  prediction <- rep(NA_real_, nrow(donnees))
  echecs <- 0
  for (g in sort(unique(groupe))) {
    ici <- groupe == g
    appris <- droplevels(donnees[!ici, , drop = FALSE])
    u <- termes_utiles(appris, termes)
    lis <- intersect(lisses, u)
    kk <- vapply(lis, function(t) dimension_courbe(appris[[t]], k[[t]]), 0L)  # (la dimension du modèle, moins si ces lignes ont peu de valeurs)
    lis <- lis[kk >= 3]
    m <- ajuster_mgcv(appris, setdiff(u, lis), lis, kk, niveaux, famille, decalage, if (length(lis) || niveaux) "REML" else "ML")
    if (is.null(m)) { echecs <- echecs + 1; next }
    prediction[ici] <- tryCatch(predire_mgcv(m, donnees[ici, , drop = FALSE], appris, niveaux), error = function(e) NA_real_)
  }
  ok <- !is.na(prediction)
  y <- donnees$y
  list(prediction = prediction, echecs = echecs,
       Q2 = if (any(ok)) 1 - sum((y[ok] - prediction[ok])^2) / sum((y[ok] - mean(y[ok]))^2) else NA_real_,
       RMSEcv = if (any(ok)) sqrt(mean((y[ok] - prediction[ok])^2)) else NA_real_)
}

# ------------------------------------------- avec une autocorrélation dans le temps (mgcv::gamm)

# Un modèle à loi normale, avec des courbes ou non, des effets aléatoires et une autocorrélation
# CAR1 : mgcv::gamm (Wood, 2017), qui l'ajuste comme un modèle mixte de nlme (Pinheiro et Bates,
# 2000), ses courbes y devenant des effets aléatoires. Les effets aléatoires (g1 = ~1, g2 = ~1... :
# emboîtés, comme nlme lit une liste) et la corrélation (correlation_car1) vont à nlme::lme ; la
# formule est celle de mgcv sans effet aléatoire (formule_mgcv), son environnement donne s(). NULL
# quand l'ajustement échoue, après un second essai par optim.
ajuster_gamm <- function(donnees, parametriques, lisses, k, niveaux, methode = "REML") {
  f <- formule_mgcv(parametriques, lisses, k, 0, FALSE)
  aleatoire <- stats::setNames(lapply(seq_len(niveaux), function(l) stats::as.formula("~ 1", env = baseenv())), paste0("g", seq_len(niveaux)))
  correlation <- correlation_car1(donnees, niveaux)
  essai <- function(...) tryCatch(mgcv::gamm(f, random = aleatoire, correlation = correlation, data = donnees, method = methode, ...), error = function(e) NULL)
  essai() %||% essai(control = nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE))
}

# Le modèle ajusté sur toutes ses observations, comme resultat_mgcv : REML pour les coefficients,
# les courbes, les variances et l'autocorrélation ; la vraisemblance, l'AIC, l'AICc et le BIC de
# la partie nlme d'un ajustement en ML. R² marginal et conditionnel (Nakagawa et Schielzeth,
# 2013), la partie fixe comprenant les courbes. Valeurs ajustées : avec les effets aléatoires
# ($lme) et par les seuls effets fixes ($gam) ; résidus normalisés (l'autocorrélation retirée).
resultat_gamm <- function(donnees, termes, noms, lisses, niveaux, k_demandes = NULL) {
  verifier_temps_uniques(donnees, niveaux)
  utiles <- termes_utiles(donnees, termes)
  retires <- noms[match(setdiff(termes, utiles), termes)]
  lis <- intersect(lisses, utiles)
  k <- vapply(lis, function(t) dimension_courbe(donnees[[t]], if (t %in% names(k_demandes)) k_demandes[[t]] else NA), 0L)
  if (any(k < 3)) {
    stop(sprintf(traduire("Trop peu de valeurs différentes pour une courbe : %s.", "Too few different values for a curve: %s."),
                 paste(noms[match(lis[k < 3], termes)], collapse = ", ")), call. = FALSE)
  }
  par <- setdiff(utiles, lis)
  estime <- ajuster_gamm(donnees, par, lis, k, niveaux, "REML")
  compare <- ajuster_gamm(donnees, par, lis, k, niveaux, "ML")
  if (is.null(estime) || is.null(compare)) {
    stop(traduire("Le modèle n'a pas pu être ajusté (mgcv::gamm n'a pas convergé) : essayez moins de termes, de courbes ou d'effets aléatoires, ou sans l'autocorrélation.",
                  "The model could not be fitted (mgcv::gamm did not converge): try fewer terms, curves or random effects, or no autocorrelation."), call. = FALSE)
  }
  g <- estime$gam
  l <- estime$lme
  s <- summary(g)
  pt <- s$p.table
  q <- stats::qt(0.975, g$df.residual)
  co <- data.frame(terme = c("(Intercept)", noms[match(rownames(pt)[-1], termes)]), estimation = pt[, 1], erreur_type = pt[, 2],
                   ddl = g$df.residual, t = pt[, 3], p = pt[, 4], bas = pt[, 1] - q * pt[, 2], haut = pt[, 1] + q * pt[, 2],
                   row.names = NULL, stringsAsFactors = FALSE)
  courbes <- NULL
  if (length(lis)) {
    st <- s$s.table
    lignes <- sprintf("s(%s)", lis)
    courbes <- data.frame(terme = noms[match(lis, termes)], edf = st[lignes, 1], ref_ddl = st[lignes, 2], statistique = st[lignes, 3], p = st[lignes, 4],
                          row.names = NULL, stringsAsFactors = FALSE)
    courbes <- concurvite_courbes(g, lignes, courbes)
  }
  # la variance de chaque effet aléatoire (nlme : σ² fois sa matrice relative), puis la résiduelle
  niveaux_g <- paste0("g", seq_len(niveaux))
  v <- vapply(niveaux_g, function(n) l$sigma^2 * as.matrix(l$modelStruct$reStruct[[n]])[1, 1], 0)
  variances <- data.frame(composante = c(niveaux_g, "residuelle"), variance = c(unname(v), l$sigma^2), stringsAsFactors = FALSE)
  fixes <- stats::var(as.vector(stats::fitted(g)))
  totale <- fixes + sum(v) + l$sigma^2
  ll <- stats::logLik(compare$lme)
  n <- nrow(donnees)
  phi <- unname(stats::coef(l$modelStruct$corStruct, unconstrained = FALSE))
  list(N = n, coefficients = co, courbes = courbes, termes_retires = retires, logLik = as.numeric(ll), parametres = attr(ll, "df"),
       AIC = stats::AIC(compare$lme), AICc = aic_corrige(compare$lme, n), BIC = stats::BIC(compare$lme),
       r2 = list(marginal = fixes / totale, conditionnel = (fixes + sum(v)) / totale), variances = variances,
       phi = phi, famille = "gaussienne", lien = "identity", echelle = l$sigma^2,
       ajuste = as.vector(stats::fitted(l)), ajuste_fixe = as.vector(stats::fitted(g)),
       # (dans l'ordre du temps de chaque groupe : regression_diagnostics.R, residus_normalises_car1)
       residus = residus_normalises_car1(as.vector(stats::residuals(l, type = "pearson")), donnees$temps, groupe_car1(donnees, niveaux), phi),
       utiles = utiles, k = k, ajustement = estime, ajustement_ml = compare)
}

# Validation croisée d'un modèle de mgcv::gamm : chaque groupe prédit par le modèle ajusté sans
# lui (REML), par ses effets fixes et ses courbes ($gam : un groupe jamais vu).
validation_gamm <- function(donnees, termes, lisses, k, groupe, niveaux) {
  prediction <- rep(NA_real_, nrow(donnees))
  echecs <- 0
  for (g in sort(unique(groupe))) {
    ici <- groupe == g
    appris <- droplevels(donnees[!ici, , drop = FALSE])
    u <- termes_utiles(appris, termes)
    lis <- intersect(lisses, u)
    kk <- vapply(lis, function(t) dimension_courbe(appris[[t]], k[[t]]), 0L)
    lis <- lis[kk >= 3]
    m <- ajuster_gamm(appris, setdiff(u, lis), lis, kk, niveaux, "REML")
    if (is.null(m)) { echecs <- echecs + 1; next }
    prediction[ici] <- tryCatch(as.vector(stats::predict(m$gam, donnees[ici, , drop = FALSE], type = "response")), error = function(e) NA_real_)
  }
  ok <- !is.na(prediction)
  y <- donnees$y
  list(prediction = prediction, echecs = echecs,
       Q2 = if (any(ok)) 1 - sum((y[ok] - prediction[ok])^2) / sum((y[ok] - mean(y[ok]))^2) else NA_real_,
       RMSEcv = if (any(ok)) sqrt(mean((y[ok] - prediction[ok])^2)) else NA_real_)
}

# Les courbes d'effet d'une régression : pour chaque terme quantitatif, son effet sur
# l'échelle du lien quand sa variable parcourt ses valeurs (les autres termes fixés), avec
# son intervalle à 95 % ; une courbe (GAM) telle que mgcv l'a ajustée, un terme linéaire comme
# une droite. Les abscisses sont dans les unités de la variable (transformée s'il y a lieu).
effets_regression <- function(m) {
  a <- m$ajustement
  if (inherits(a, "gamm")) a <- a$gam  # (mgcv::gamm : ses courbes, dans sa partie gam)
  d <- m$donnees
  co <- m$resultat$coefficients
  termes <- intersect(m$quantitatifs, m$utiles)
  lapply(termes, function(t) {
    i <- match(t, m$termes)
    centre <- m$bloc$centre[i]
    poids <- m$bloc$poids[i]
    if (!isTRUE(poids > 0)) poids <- 1
    x <- seq(min(d[[t]]), max(d[[t]]), length.out = 80)
    if (t %in% m$lisses) {
      nd <- d[rep(1, length(x)), , drop = FALSE]
      nd[[t]] <- x
      p <- stats::predict(a, nd, type = "terms", terms = sprintf("s(%s)", t), se.fit = TRUE)
      effet <- as.vector(p$fit[, 1])
      se <- as.vector(p$se.fit[, 1])
    } else {
      j <- match(m$noms_termes[i], co$terme)
      ecart <- x - mean(d[[t]])
      effet <- co$estimation[j] * ecart
      se <- abs(ecart) * co$erreur_type[j]
    }
    list(terme = m$noms_termes[i], courbe = t %in% m$lisses, x = x / poids + centre, effet = effet,
         bas = effet - 1.96 * se, haut = effet + 1.96 * se, valeurs = d[[t]] / poids + centre)
  })
}

# Les seuils d'une courbe (GAM) : son effet et sa pente sur une grille de 200 valeurs entre les
# percentiles 2 et 98 de sa variable, avec des intervalles simultanés à 95 % (ils couvrent toute
# la courbe, pas un point), tirés de 2000 jeux de coefficients (MASS::mvrnorm sur vcov du modèle)
# avec une graine fixe ; les plages où la pente est nettement positive ou négative (sa bande
# simultanée exclut 0), et celles où l'effet est nettement au-dessus ou au-dessous de sa moyenne.
# Une courbe d'edf < 1,5 est presque une droite. C'est la démarche de Simpson (2018), celle des
# scripts de seuils qui ont servi de modèle. `terme` : le nom du terme (noms_termes).
seuils_courbe <- function(m, terme, tirages = 2000, niveau = 0.95, graine = 1) {
  a <- m$ajustement
  if (inherits(a, "gamm")) a <- a$gam
  if (!inherits(a, "gam")) stop(traduire("Les seuils se lisent sur une courbe d'un modèle additif.", "Thresholds are read on a curve of an additive model."), call. = FALSE)
  i <- match(as.character(terme)[1], m$noms_termes)
  t <- if (!is.na(i)) m$termes[i]
  if (is.null(t) || !t %in% m$lisses) stop(sprintf(traduire("« %s » n'est pas une courbe de ce modèle.", "\"%s\" is not a curve of this model."), terme), call. = FALSE)
  d <- m$donnees
  grille <- seq(stats::quantile(d[[t]], 0.02), stats::quantile(d[[t]], 0.98), length.out = 200)
  nd <- d[rep(1, length(grille)), , drop = FALSE]
  nd[[t]] <- grille
  Xp <- stats::predict(a, nd, type = "lpmatrix")
  js <- grep(sprintf("^s\\(%s\\)\\.", t), colnames(Xp))
  # (une graine fixe et le générateur de R par défaut, comme dans le script exporté : les mêmes
  # bandes à chaque calcul, même si la console R en a choisi un autre ; le hasard de la session,
  # et son générateur, sont rendus)
  existait <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  ancien <- if (existait) get(".Random.seed", envir = globalenv())
  genre <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(genre[1], genre[2], genre[3]))  # (le genre de la séance rendu, sans redire ses avertissements)
    if (existait) assign(".Random.seed", ancien, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  })
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  B <- MASS::mvrnorm(tirages, stats::coef(a), stats::vcov(a))
  ajuste <- as.vector(Xp[, js, drop = FALSE] %*% stats::coef(a)[js])
  simules <- Xp[, js, drop = FALSE] %*% t(B[, js, drop = FALSE])
  # (centrées : des écarts à la moyenne de la courbe)
  ajuste <- ajuste - mean(ajuste)
  simules <- sweep(simules, 2, colMeans(simules))
  bande <- function(estime, sims) {
    et <- apply(sims, 1, stats::sd)
    critique <- stats::quantile(apply(abs(sims - estime) / et, 2, max), niveau, names = FALSE)
    list(bas = estime - critique * et, haut = estime + critique * et, critique = critique)
  }
  be <- bande(ajuste, simules)
  # la grille dans les unités de la variable ; la pente par différences, dans ces unités
  centre <- m$bloc$centre[match(t, m$termes)]
  poids <- m$bloc$poids[match(t, m$termes)]
  if (!isTRUE(poids > 0)) poids <- 1
  x <- grille / poids + centre
  h <- diff(x[1:2])
  pente <- diff(ajuste) / h
  bp <- bande(pente, apply(simules, 2, diff) / h)
  milieux <- (x[-1] + x[-length(x)]) / 2
  sens <- ifelse(bp$bas > 0, "hausse", ifelse(bp$haut < 0, "baisse", "ns"))
  plages <- function(etat, xs, xs_fin = xs) {
    r <- rle(etat)
    fin <- cumsum(r$lengths)
    debut <- c(1, utils::head(fin, -1) + 1)
    garde <- r$values != "ns"
    data.frame(sens = r$values[garde], de = xs[debut][garde], a = xs_fin[fin][garde], stringsAsFactors = FALSE)
  }
  position <- ifelse(be$bas > 0, "au_dessus", ifelse(be$haut < 0, "au_dessous", "ns"))
  st <- summary(a)$s.table
  ligne <- match(sprintf("s(%s)", t), rownames(st))
  edf <- unname(st[ligne, 1])
  list(terme = m$noms_termes[i], edf = edf, p = unname(st[ligne, 4]), lineaire = edf < 1.5, amplitude = diff(range(ajuste)),
       x = x, effet = ajuste, bas = be$bas, haut = be$haut, x_pente = milieux, pente = pente, pente_bas = bp$bas, pente_haut = bp$haut,
       sens = sens, plages_pente = plages(sens, milieux), plages_effet = plages(position, x),
       critique_effet = be$critique, critique_pente = bp$critique, tirages = tirages, niveau = niveau, valeurs = d[[t]] / poids + centre)
}

# Le texte d'une section « Méthodes » pour une régression, de la même forme que celui des
# autres modèles (methodes.R : texte_methodes) : données, modèle, validation croisée,
# qualité, logiciel ; et les références.
REFERENCES_REGRESSION <- list(
  pinheiro = "Pinheiro, J. C., & Bates, D. M. (2000). Mixed-effects models in S and S-PLUS. Springer. https://doi.org/10.1007/b98882",
  nlme = "Pinheiro, J., Bates, D., & R Core Team (%s). nlme: Linear and nonlinear mixed effects models (R package version %s). https://CRAN.R-project.org/package=nlme",
  nakagawa = "Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R² from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. https://doi.org/10.1111/j.2041-210x.2012.00261.x",
  burnham = "Burnham, K. P., & Anderson, D. R. (2002). Model selection and multimodel inference: A practical information-theoretic approach (2nd ed.). Springer. https://doi.org/10.1007/b97636",
  zuur = "Zuur, A. F., Ieno, E. N., Walker, N. J., Saveliev, A. A., & Smith, G. M. (2009). Mixed effects models and extensions in ecology with R. Springer. https://doi.org/10.1007/978-0-387-87458-6",
  wood2017 = "Wood, S. N. (2017). Generalized additive models: An introduction with R (2nd ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781315370279",
  wood2011 = "Wood, S. N. (2011). Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models. Journal of the Royal Statistical Society: Series B, 73(1), 3–36. https://doi.org/10.1111/j.1467-9868.2010.00749.x",
  stewart_oaten = "Stewart-Oaten, A., Murdoch, W. W., & Parker, K. R. (1986). Environmental impact assessment: \"Pseudoreplication\" in time? Ecology, 67(4), 929–940. https://doi.org/10.2307/1939815",
  mgcv = "Wood, S. N. (%s). mgcv: Mixed GAM computation vehicle with automatic smoothness estimation (R package version %s). https://CRAN.R-project.org/package=mgcv")

texte_methodes_regression <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  r <- m$resultat
  nombre <- function(x, chiffres = 2) formatC(x, format = "f", digits = chiffres, decimal.mark = if (fr) "," else ".")
  pourcent <- function(x) paste0(formatC(100 * x, format = "f", digits = if (100 * x < 10) 1 else 0, decimal.mark = if (fr) "," else "."), if (fr) " %" else "%")
  liste <- function(x) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), if (fr) "et" else "and", x[length(x)])
  guillemets <- function(x) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)
  cites <- character(0)
  cite <- function(cle, texte) { cites <<- union(cites, cle); texte }
  generalise <- inherits(m$ajustement, c("gam", "gamm"))
  gamm <- isTRUE(m$gamm)
  mixte <- length(m$niveaux) > 0 && !generalise

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
  p2 <- if (generalise) {
    lois <- if (fr) c(gaussienne = "normale", poisson = "de Poisson", binomiale_negative = "binomiale négative", binomiale = "binomiale", gamma = "gamma", tweedie = "de Tweedie")
            else c(gaussienne = "normal", poisson = "Poisson", binomiale_negative = "negative binomial", binomiale = "binomial", gamma = "gamma", tweedie = "Tweedie")
    lis <- m$noms_termes[match(m$lisses, m$termes)]
    courbes <- if (length(lis)) sprintf(if (fr) ", dont %s par des courbes (splines de régression en plaques minces)" else ", %s of them through curves (thin plate regression splines)", liste(lis)) else ""
    aleatoire <- if (gamm) {
      sprintf(if (fr) ", une ordonnée à l'origine aléatoire par %s, et une autocorrélation des résidus en temps continu (CAR1) le long de %s à l'intérieur de chaque %s"
              else ", a random intercept for %s, and a continuous-time autocorrelation of the residuals (CAR1) along %s within each %s",
              liste(niveaux_emboites(m$niveaux, fr, guillemets)), guillemets(m$temps), guillemets(m$dans %||% m$niveaux[length(m$niveaux)]))
    } else if (length(m$niveaux)) sprintf(if (fr) ", une ordonnée à l'origine aléatoire par %s (des termes pénalisés)" else ", a random intercept for %s (penalized terms)",
                                          liste(vapply(m$niveaux, guillemets, ""))) else ""
    effort <- if (!is.null(m$decalage)) sprintf(if (fr) ", et log(%s) en décalage (l'effort)" else ", and log(%s) as an offset (the effort)", guillemets(m$decalage)) else ""
    penalise <- length(lis) || length(m$niveaux)
    c(cite("wood2017", if (fr) sprintf("Nous avons ajusté un modèle %s (loi %s, lien %s ; Wood, 2017) avec %s%s%s%s. %s",
                                       if (identical(m$type, "GAM")) "additif généralisé" else "linéaire généralisé", lois[[m$famille]], m$lien, termes, courbes, aleatoire, effort, unites)
                       else sprintf("We fitted a %s model (%s distribution, %s link; Wood, 2017) with %s%s%s%s. %s",
                                    if (identical(m$type, "GAM")) "generalized additive" else "generalized linear", lois[[m$famille]], m$lien, termes, courbes, aleatoire, effort, unites)),
      if (gamm) cite("pinheiro", if (fr) "Le modèle a été ajusté par mgcv::gamm comme un modèle linéaire mixte de nlme (Pinheiro et Bates, 2000), dont les courbes sont des effets aléatoires : coefficients, courbes, variances et autocorrélation par maximum de vraisemblance restreint (REML), et les modèles comparés par l'AIC d'ajustements en maximum de vraisemblance."
                     else "The model was fitted with mgcv::gamm as a linear mixed model of nlme (Pinheiro & Bates, 2000), whose curves are random effects: coefficients, curves, variances and autocorrelation by restricted maximum likelihood (REML), and models compared by the AIC of maximum-likelihood fits.")
      else if (penalise) cite("wood2011", if (fr) "Les courbes et les effets aléatoires ont été estimés par maximum de vraisemblance restreint (REML ; Wood, 2011), et les modèles comparés par l'AIC d'ajustements en maximum de vraisemblance."
                                     else "Curves and random effects were estimated by restricted maximum likelihood (REML; Wood, 2011), and models compared by the AIC of maximum-likelihood fits.")
      else if (fr) "Le modèle a été ajusté par maximum de vraisemblance." else "The model was fitted by maximum likelihood.")
  } else if (!mixte) {
    if (fr) sprintf("Nous avons ajusté une régression linéaire multiple par les moindres carrés ordinaires, avec %s. %s", termes, unites)
    else sprintf("We fitted a multiple linear regression by ordinary least squares, with %s. %s", termes, unites)
  } else {
    niveaux <- niveaux_emboites(m$niveaux, fr, guillemets)
    car1 <- if (m$car1) {
      if (fr) sprintf(", et une autocorrélation des résidus en temps continu (CAR1) le long de %s à l'intérieur de chaque %s", guillemets(m$temps), guillemets(m$dans %||% m$niveaux[length(m$niveaux)]))
      else sprintf(", and a continuous-time autocorrelation of the residuals (CAR1) along %s within each %s", guillemets(m$temps), guillemets(m$dans %||% m$niveaux[length(m$niveaux)]))
    } else ""
    c(cite("pinheiro", if (fr) sprintf("Nous avons ajusté un modèle linéaire mixte (Pinheiro et Bates, 2000) avec %s en effets fixes, une ordonnée à l'origine aléatoire par %s%s. %s",
                                       termes, liste(niveaux), car1, unites)
                       else sprintf("We fitted a linear mixed model (Pinheiro & Bates, 2000) with %s as fixed effects, a random intercept for %s%s. %s",
                                    termes, liste(niveaux), car1, unites)),
      cite("zuur", if (fr) "Les coefficients et les variances ont été estimés par maximum de vraisemblance restreint (REML), et les modèles comparés par l'AIC d'ajustements en maximum de vraisemblance (ML ; Zuur et al., 2009)."
                   else "Coefficients and variances were estimated by restricted maximum likelihood (REML), and models compared by the AIC of maximum-likelihood fits (ML; Zuur et al., 2009)."))
  }
  # (la sélection par l'AIC, quand elle a été faite sur ce modèle ; un modèle qu'elle a choisi le dit)
  if (isTRUE(m$selection_aic$K > 0)) p2 <- c(p2, cite("burnham", phrase_selection_aic(m, fr, nombre, liste, guillemets)))
  if (!is.null(d$selection_origine)) {
    o <- d$selection_origine
    groupes <- as.character(unlist(o$groupes))
    # (les mêmes observations tant qu'aucune n'a été exclue depuis : ses lignes sont celles du modèle complet)
    memes <- isTRUE(as.numeric(o$n)[1] == length(m$observations))
    p2 <- c(p2, cite("burnham", sprintf(if (fr) "Ses termes sont ceux que la sélection par l'AICc du modèle %s a retenus (%s)%s (Burnham et Anderson, 2002)."
                                        else "Its terms are those the AICc selection of model %s kept (%s)%s (Burnham & Anderson, 2002).",
                                        guillemets(as.character(o$modele)[1]), if (length(groupes)) liste(groupes) else (if (fr) "aucun : le modèle nul" else "none: the null model"),
                                        if (!memes) "" else if (fr) ", sur les mêmes observations que lui" else ", on the same observations as that model")))
  }
  # (le modèle mixte de la vue BACI : comparaisons.R, definition_baci)
  if (!is.null(d$baci_origine)) p2 <- c(p2, cite("stewart_oaten", phrase_methodes_baci(d$baci_origine, fr, liste, guillemets)))

  # --- validation croisée
  par <- if (length(d$groupes_vc$by)) as.character(d$groupes_vc$by) else ""
  groupes <- if (par %in% c("id", "variable")) {
    if (fr) sprintf("%d groupes, un par valeur de %s", m$G, guillemets(m$vc_par)) else sprintf("%d groups, one per value of %s", m$G, guillemets(m$vc_par))
  } else if (fr) sprintf("%d groupes (l'observation i dans le groupe i modulo %d)", m$G, m$G) else sprintf("%d groups (observation i in group i modulo %d)", m$G, m$G)
  p3 <- if (fr) sprintf("La validation croisée comptait %s ; chaque groupe a été prédit par le modèle ajusté sans lui%s.", groupes,
                        if (length(m$niveaux)) ", par ses seuls effets fixes (comme un groupe jamais vu)" else "")
        else sprintf("Cross-validation used %s; each group was predicted by the model fitted without it%s.", groupes,
                     if (length(m$niveaux)) ", by its fixed effects only (as a group never seen)" else "")

  # --- qualité
  aic <- cite("burnham", sprintf("AIC = %s (Burnham %s Anderson, 2002)", nombre(r$AIC, 1), if (fr) "et" else "&"))
  loi <- r$parametre_loi
  p4 <- if (generalise && is.null(r$r2$marginal)) {
    sprintf(if (fr) "Le modèle explique %s de la déviance (R² ajusté = %s)%s ; %s ; en validation croisée, Q² = %s et RMSEcv = %s." else "The model explains %s of the deviance (adjusted R² = %s)%s; %s; in cross-validation, Q² = %s and RMSEcv = %s.",
            pourcent(r$r2$deviance_expliquee), nombre(r$r2$r2_ajuste), if (!is.null(loi)) sprintf(", %s = %s", if (loi$nom == "theta") "θ" else "p", nombre(loi$valeur)) else "",
            aic, nombre(m$vc$Q2), nombre(m$vc$RMSEcv, 3))
  } else if (!is.null(r$r2$marginal)) {
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
  mgcv <- if (generalise) utils::packageDescription("mgcv")
  annee_mgcv <- if (generalise) substr(mgcv$Date %||% annee_r, 1, 4)
  cites <- union(cites, c("r", "nuee", if (mixte || gamm) "nlme", if (generalise) "mgcv"))
  p5 <- sprintf(if (fr) "Les calculs ont été faits avec Nuée%s (4rtefakt, %s) et R %s (R Core Team, %s)%s." else "Computations were done with Nuée%s (4rtefakt, %s) and R %s (R Core Team, %s)%s.",
                if (nzchar(version)) paste0(" ", version) else "", annee_nuee, paste(R.version$major, R.version$minor, sep = "."), annee_r,
                if (gamm) sprintf(if (fr) ", paquets mgcv %s (Wood, %s) et nlme %s (Pinheiro et al., %s)" else ", packages mgcv %s (Wood, %s) and nlme %s (Pinheiro et al., %s)",
                                  mgcv$Version, annee_mgcv, nlme$Version, annee_nlme)
                else if (mixte) sprintf(if (fr) ", paquet nlme %s (Pinheiro et al., %s)" else ", package nlme %s (Pinheiro et al., %s)", nlme$Version, annee_nlme)
                else if (generalise) sprintf(if (fr) ", paquet mgcv %s (Wood, %s)" else ", package mgcv %s (Wood, %s)", mgcv$Version, annee_mgcv) else "")
  references <- vapply(cites, function(k) switch(k,
    r = sprintf(REFERENCES$r, annee_r),
    nuee = sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
    nlme = sprintf(REFERENCES_REGRESSION$nlme, annee_nlme, nlme$Version),
    mgcv = sprintf(REFERENCES_REGRESSION$mgcv, annee_mgcv, mgcv$Version),
    REFERENCES_REGRESSION[[k]]), "")
  list(paragraphes = c(p1, paste(p2, collapse = " "), p3, paste(p4, collapse = " "), p5), references = unname(sort(references)))
}

# La sélection par l'AIC dans le texte des Méthodes (selection_aic, quand elle a été faite) : les
# modèles comparés et comment, le meilleur et le plus parcimonieux, l'importance des groupes en
# tête (cinq au plus). `nombre`, `liste`, `guillemets` : ceux de texte_methodes_regression.
phrase_selection_aic <- function(m, fr, nombre, liste, guillemets) {
  s <- m$selection_aic
  noms <- as.character(s$groupes)
  mo <- s$modeles
  toutes <- identical(s$methode, "toutes")
  contenu <- function(i) {  # (les groupes d'un modèle du classement, en mots)
    g <- noms[vapply(s$contient, function(x) x[i] == 1L, TRUE)]
    if (!length(g)) return(if (fr) "était le modèle nul" else "was the null model")
    if (length(g) == length(noms)) return(if (fr) "était le modèle complet" else "was the full model")
    sprintf(if (fr) "contenait %s" else "contained %s", liste(guillemets(g)))
  }
  avec <- c(if (length(m$niveaux)) (if (fr) "les effets aléatoires" else "the random effects"), if (isTRUE(m$car1)) (if (fr) "l'autocorrélation" else "the autocorrelation"),
            if (!is.null(m$decalage)) (if (fr) "l'effort en décalage" else "the effort offset"), if (length(m$lisses)) (if (fr) "le réglage des courbes" else "the curves' settings"))
  avec <- if (length(avec)) sprintf(if (fr) ", avec %s du modèle" else ", with %s of the model", liste(avec)) else ""
  echecs <- if (s$echecs) sprintf(if (fr) " (%d n'ont pas pu être ajustés)" else " (%d could not be fitted)", s$echecs) else ""
  croises <- any(lengths(s$requis) > 0)
  comparaison <- if (toutes) {
    sprintf(if (fr) "Les effets fixes ont ensuite été choisis par l'AICc (Burnham et Anderson, 2002) : les %d combinaisons de leurs %d groupes de termes (%s), du modèle nul au modèle complet%s, ont été ajustées par maximum de vraisemblance (ML) sur les mêmes %d observations%s%s."
            else "The fixed effects were then selected by AICc (Burnham & Anderson, 2002): the %d combinations of their %d groups of terms (%s), from the null model to the full model%s, were fitted by maximum likelihood (ML) on the same %d observations%s%s.",
            nrow(mo), s$K, liste(guillemets(noms)), if (!croises) "" else if (fr) ", un terme croisé n'y entrant qu'avec ses effets principaux" else ", a cross term entering only with its main effects",
            s$N, avec, echecs)
  } else {
    sprintf(if (fr) "Les effets fixes ont ensuite été choisis par élimination descendante sur l'AICc (Burnham et Anderson, 2002) : leurs %d groupes de termes donnant plus de %d combinaisons, chaque pas a retiré, depuis le modèle complet, le groupe dont le retrait baissait le plus l'AICc%s, jusqu'à ce qu'aucun retrait ne la baisse ; les %d modèles essayés ont été ajustés par maximum de vraisemblance (ML) sur les mêmes %d observations%s%s."
            else "The fixed effects were then selected by backward elimination on AICc (Burnham & Anderson, 2002): their %d groups of terms giving more than %d combinations, each step removed, from the full model on, the group whose removal lowered AICc the most%s, until no removal lowered it; the %d models tried were fitted by maximum likelihood (ML) on the same %d observations%s%s.",
            s$K, as.integer(s$limite), if (!croises) "" else if (fr) " (un effet principal restant tant qu'un terme croisé le demande)" else " (a main effect staying while a cross term needs it)",
            nrow(mo), s$N, avec, echecs)
  }
  p <- s$parcimonieux
  choix <- sprintf(if (fr) "Le meilleur modèle (l'AICc le plus bas, poids d'Akaike %s) %s ; %s." else "The best model (lowest AICc, Akaike weight %s) %s; %s.",
                   nombre(mo$poids[1]), contenu(1),
                   if (p == 1) (if (fr) "c'était aussi le plus parcimonieux à ΔAICc ≤ 2" else "it was also the most parsimonious within ΔAICc ≤ 2")
                   else sprintf(if (fr) "le plus parcimonieux à ΔAICc ≤ 2 (le moins de paramètres) %s (ΔAICc = %s)" else "the most parsimonious within ΔAICc ≤ 2 (fewest parameters) %s (ΔAICc = %s)",
                                contenu(p), nombre(mo$delta[p], 1)))
  # (les groupes en tête, ceux d'une même importance ensemble : « 1,00 pour « a » et « b », 0,35 pour « c » »)
  imp <- s$importance[order(-s$importance$importance), , drop = FALSE][seq_len(min(5, s$K)), , drop = FALSE]
  valeurs <- nombre(imp$importance)
  par_valeur <- split(imp$groupe, factor(valeurs, levels = unique(valeurs)))
  importance <- sprintf(if (fr) "L'importance relative des groupes (la somme des poids d'Akaike des modèles qui les contiennent%s) était de %s." else "The relative importance of the groups (the sum of the Akaike weights of the models that contain them%s) was %s.",
                        if (toutes) "" else if (fr) ", sur les modèles essayés" else ", over the models tried",
                        liste(vapply(names(par_valeur), function(v) sprintf(if (fr) "%s pour %s" else "%s for %s", v, liste(guillemets(par_valeur[[v]]))), "")))
  paste(comparaison, choix, importance)
}

# Le modèle mixte BACI dans le texte des Méthodes (comparaisons.R : definition_baci ; `o` :
# d$baci_origine) : ce que valent ses deux variables 0/1, ce qu'est le coefficient de leur terme
# croisé, et ses observations. `liste`, `guillemets` : ceux de texte_methodes_regression.
phrase_methodes_baci <- function(o, fr, liste, guillemets) {
  quand <- function(x) if (isTRUE(o$dates)) date_decimale(as.numeric(x)) else trimws(formatC(as.numeric(x), digits = 10, format = "fg", decimal.mark = if (fr) "," else "."))
  # (les unités nommées, dix au plus ; au-delà, leur nombre)
  unites <- function(x, un, plusieurs, nombreuses) {
    x <- as.character(unlist(x))
    if (length(x) > 10) sprintf(nombreuses, length(x)) else sprintf(if (length(x) > 1) plusieurs else un, liste(x))
  }
  temps <- as.character(o$noms$temps)
  fin <- if (length(o$fin)) sprintf(if (fr) ", jusqu'à %s = %s," else ", up to %s = %s,", temps, quand(o$fin)) else ""
  sprintf(if (fr) "C'est le modèle BACI (avant/après avec témoin ; Stewart-Oaten et al., 1986) : %s vaut 1 à partir de %s = %s et 0 avant, %s vaut 1 pour %s et 0 pour %s, et le coefficient de leur terme croisé, l'effet BACI, est le changement de l'écart entre l'impact et les témoins après l'événement ; seules les observations de ces unités%s y entrent."
          else "This is the BACI model (before-after control-impact; Stewart-Oaten et al., 1986): %s is 1 from %s = %s on and 0 before, %s is 1 for %s and 0 for %s, and the coefficient of their cross term, the BACI effect, is the change in the gap between the impact and the controls after the event; only the observations of these units%s enter it.",
          guillemets(as.character(o$variables$periode)), temps, quand(o$evenement), guillemets(as.character(o$variables$impact)),
          unites(o$impact, if (fr) "l'unité d'impact %s" else "the impact unit %s", if (fr) "les unités d'impact %s" else "the impact units %s",
                 if (fr) "les %d unités d'impact" else "the %d impact units"),
          unites(o$temoins, if (fr) "le témoin %s" else "the control %s", if (fr) "les témoins %s" else "the controls %s", if (fr) "les %d témoins" else "the %d controls"), fin)
}

# Les niveaux emboîtés d'effets aléatoires, en mots : « site », « secteur » dans chaque « site »...
niveaux_emboites <- function(niveaux, fr, guillemets) vapply(seq_along(niveaux), function(l) {
  if (l == 1) guillemets(niveaux[l]) else sprintf(if (fr) "%s dans chaque %s" else "%s within each %s", guillemets(niveaux[l]), guillemets(niveaux[l - 1]))
}, "")

# Ce que R affiche d'une régression (la console de l'application et son journal).
print.modele_regression <- function(x, ...) {
  r <- x$resultat
  type <- switch(x$type, LMM = traduire("modèle linéaire mixte", "linear mixed model"),
                 GLM = sprintf(traduire("modèle linéaire généralisé, loi %s", "generalized linear model, %s distribution"), sub("_", " ", x$famille)),
                 GAM = sprintf(traduire("modèle additif généralisé, loi %s", "generalized additive model, %s distribution"), sub("_", " ", x$famille)),
                 traduire("régression linéaire", "linear regression"))
  cat(sprintf(traduire("Modèle %s (%s)%s\n", "Model %s (%s)%s\n"), x$nom, type, if (nzchar(x$titre %||% "")) paste(" :", x$titre) else ""))
  cat(sprintf(traduire("%d observations, réponse %s%s\n", "%d observations, response %s%s\n"), x$N, x$y$noms,
              if (length(x$niveaux)) sprintf(traduire(", effets aléatoires %s%s", ", random effects %s%s"), paste(x$niveaux, collapse = " / "),
                                             if (x$car1) sprintf(traduire(", autocorrélation CAR1 dans le temps (%s, dans chaque %s)%s", ", CAR1 autocorrelation in time (%s, within each %s)%s"),
                                                                 x$temps, x$dans %||% x$niveaux[length(x$niveaux)], if (isTRUE(x$gamm)) ", mgcv::gamm" else "") else "") else ""))
  co <- r$coefficients
  print(data.frame(terme = co$terme, estimation = signif(co$estimation, 4), IC95 = sprintf("[%s ; %s]", signif(co$bas, 3), signif(co$haut, 3)),
                   p = signif(co$p, 3)), row.names = FALSE)
  cat(sprintf("AIC %.1f, AICc %.1f, BIC %.1f, logLik %.2f (%s %s)\n", r$AIC, r$AICc, r$BIC, r$logLik, format(round(r$parametres, 1)), traduire("paramètres", "parameters")))
  cat(paste(names(r$r2), signif(unlist(r$r2), 3), sep = " = ", collapse = ", "), sprintf("; Q2 = %.3f, RMSEcv = %.4g\n", x$vc$Q2, x$vc$RMSEcv))
  invisible(x)
}
