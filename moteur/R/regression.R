# Modèles de régression : linéaire (stats::lm) et linéaire mixte (nlme::lme, Pinheiro et
# Bates, 2000), avec des effets aléatoires emboîtés (site, puis secteur dans le site...), au choix
# des pentes aléatoires (chaque site sa pente de l'année autour de la pente fixe) et une
# autocorrélation dans le temps à l'intérieur du dernier niveau (corCAR1 : temps continu, les
# années manquantes comprises).
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

# La partie aléatoire d'un modèle de nlme : ~ 1 | g1 / g2..., des ordonnées à l'origine
# emboîtées ; avec des pentes aléatoires (`pentes` : les termes t1..tK de chaque niveau, du plus
# large au plus fin), ~ t3 | g1 pour un seul niveau, sinon la liste nommée list(g1 = ~ t3, g2 = ~ 1),
# chaque niveau dans le précédent, comme nlme lit une liste (`liste` : toujours elle, pour
# mgcv::gamm, qui ne prend qu'elle). Chaque niveau a la matrice générale de nlme (pdLogChol) : la
# variance de son ordonnée à l'origine, celles de ses pentes et leurs corrélations.
aleatoire_nlme <- function(niveaux, pentes = NULL, liste = FALSE) {
  g <- paste0("g", seq_len(niveaux))
  droites <- vapply(seq_len(niveaux), function(l) if (length(pentes[[l]])) paste(pentes[[l]], collapse = " + ") else "1", "")
  if (!liste && all(droites == "1")) return(stats::as.formula(paste("~ 1 |", paste(g, collapse = " / ")), env = baseenv()))
  if (!liste && niveaux == 1) return(stats::as.formula(paste("~", droites, "| g1"), env = baseenv()))
  stats::setNames(lapply(droites, function(x) stats::as.formula(paste("~", x), env = baseenv())), g)
}

# Un ajustement, ou NULL quand il échoue (nlme essaie optim, puis nlminb, comme le font les
# scripts qui ont servi de modèle). methode : "REML" ou "ML" ; pentes : aleatoire_nlme.
ajuster_lineaire <- function(donnees, termes, niveaux = 0, car1 = FALSE, methode = "REML", pentes = NULL) {
  f <- stats::reformulate(if (length(termes)) termes else "1", response = "y")
  if (!niveaux) {
    environment(f) <- baseenv()
    return(tryCatch(stats::lm(f, data = donnees), error = function(e) NULL))
  }
  aleatoire <- aleatoire_nlme(niveaux, pentes)
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

# Les matrices de variance des effets aléatoires d'un ajustement de nlme `l` (un lme, ou la partie
# $lme d'un mgcv::gamm), une par niveau g1..gL, du plus large au plus fin : σ² fois la matrice
# relative du niveau (nlme les range du plus fin au plus large : par leur nom), ses lignes et
# colonnes nommées par ses termes, (Intercept) puis ses pentes.
matrices_nlme <- function(l, niveaux) lapply(paste0("g", seq_len(niveaux)), function(n) l$sigma^2 * as.matrix(l$modelStruct$reStruct[[n]]))

# La variance de chaque niveau d'effets aléatoires sur les observations `donnees` : celle de son
# ordonnée à l'origine quand il n'a pas de pente aléatoire ; avec, la moyenne sur les observations
# de zᵢᵀ Σ zᵢ, zᵢ = (1, les valeurs de ses termes en pente) et Σ sa matrice (Johnson, 2014) : la
# variance d'un niveau change alors avec x.
variances_niveaux <- function(matrices, donnees, pentes = NULL) vapply(seq_along(matrices), function(l) {
  termes <- c("(Intercept)", pentes[[l]])
  S <- matrices[[l]][termes, termes, drop = FALSE]
  if (length(termes) == 1) return(S[1, 1])
  Z <- cbind(1, as.matrix(donnees[pentes[[l]]]))
  mean(rowSums((Z %*% S) * Z))
}, 0)

# R² d'un modèle mixte de nlme `m` (Nakagawa et Schielzeth, 2013) : marginal, la part de variance
# des effets fixes ; conditionnel, celle des effets fixes et aléatoires ensemble ; avec des pentes
# aléatoires, la variance des effets aléatoires est celle de Johnson (2014), variances_niveaux.
r2_mixte <- function(m, pentes = NULL) {
  fixes <- stats::var(as.vector(stats::model.matrix(stats::formula(m), m$data) %*% nlme::fixef(m)))
  aleatoires <- sum(variances_niveaux(matrices_nlme(m, length(m$modelStruct$reStruct)), m$data, pentes))
  totale <- fixes + aleatoires + m$sigma^2
  c(marginal = fixes / totale, conditionnel = (fixes + aleatoires) / totale)
}

# Le tableau des variances des effets aléatoires (`matrices` : une par niveau, matrices_nlme), puis
# la variance résiduelle s'il y en a une (nlme ; pas mgcv). Sans pente aléatoire, une ligne par
# niveau (composante g1..gL, variance), comme jusqu'à 0.9.6 ; avec, une ligne par terme de chaque
# niveau, son ordonnée à l'origine puis ses pentes (terme), et la corrélation de chaque pente avec
# l'ordonnée à l'origine du niveau (NA quand le modèle les tient pour indépendantes : mgcv).
tableau_variances <- function(matrices, residuelle = NULL, pentes = NULL, independantes = FALSE) {
  L <- length(matrices)
  if (!length(unlist(pentes))) {
    return(data.frame(composante = c(paste0("g", seq_len(L)), if (!is.null(residuelle)) "residuelle"),
                      variance = c(vapply(matrices, function(S) S[1, 1], 0), residuelle), stringsAsFactors = FALSE))
  }
  lignes <- lapply(seq_len(L), function(l) {
    termes <- c("(Intercept)", pentes[[l]])
    S <- matrices[[l]][termes, termes, drop = FALSE]
    r <- if (independantes) rep(NA_real_, length(termes)) else S[, 1] / sqrt(S[1, 1] * diag(S))
    data.frame(composante = paste0("g", l), terme = termes, variance = unname(diag(S)), correlation = c(NA_real_, unname(r[-1])), stringsAsFactors = FALSE)
  })
  if (!is.null(residuelle)) lignes <- c(lignes, list(data.frame(composante = "residuelle", terme = "", variance = residuelle, correlation = NA_real_, stringsAsFactors = FALSE)))
  do.call(rbind, lignes)
}

# La part de chaque niveau d'effets aléatoires dans la variance aléatoire, et celle de la variance
# résiduelle : avec une pente aléatoire, la variance d'un niveau change avec x ; seule sa moyenne sur
# les observations (variances_niveaux, Johnson 2014) a une part, ni son ordonnée à l'origine ni sa
# pente n'en ont une à elles.
parts_variance <- function(matrices, donnees, pentes, residuelle) {
  v <- c(variances_niveaux(matrices, donnees, pentes), residuelle)
  data.frame(composante = c(paste0("g", seq_along(matrices)), "residuelle"), variance = v, part = v / sum(v), stringsAsFactors = FALSE)
}

# Les signes d'un ajustement singulier des pentes aléatoires, au bord du domaine de ses variances :
# deux termes d'un même niveau corrélés au-delà de ±0,99 (`correlation` ; pas pour un modèle qui les
# tient pour indépendants), ou une pente dont l'écart-type, fois l'écart-type de sa variable dans les
# observations, fait moins de 1 % de l'écart-type résiduel `sigma` (`ecart_type`, presque nul). Une
# ligne par pente en cause : son niveau (g1..gL), son terme, l'autre terme de la corrélation, la
# valeur, et la moyenne et l'écart-type de sa variable sur les observations (loin de 0 quand la
# moyenne dépasse l'écart-type : l'ordonnée à l'origine est alors celle d'un x loin des données,
# où l'ajustement trouve mal sa corrélation avec la pente). NULL quand rien ne l'est.
singularites_pentes <- function(matrices, donnees, pentes, sigma, independantes = FALSE) {
  lignes <- list()
  ajouter <- function(l, terme, avec, type, valeur) {
    x <- donnees[[terme]]
    lignes[[length(lignes) + 1]] <<- data.frame(niveau = paste0("g", l), terme = terme, avec = avec, type = type, valeur = valeur,
                                               moyenne = mean(x), ecart_type_x = stats::sd(x), loin = abs(mean(x)) > stats::sd(x), stringsAsFactors = FALSE)
  }
  for (l in seq_along(matrices)) {
    if (!length(pentes[[l]])) next
    termes <- c("(Intercept)", pentes[[l]])
    S <- matrices[[l]][termes, termes, drop = FALSE]
    et <- sqrt(pmax(unname(diag(S)), 0))
    for (j in seq_along(termes)[-1]) {
      if (et[j] * stats::sd(donnees[[termes[j]]]) < 0.01 * sigma) { ajouter(l, termes[j], "", "ecart_type", et[j]); next }
      if (independantes) next
      for (i in seq_len(j - 1)) {
        r <- S[i, j] / (et[i] * et[j])
        if (is.finite(r) && abs(r) > 0.99) ajouter(l, termes[j], termes[i], "correlation", r)
      }
    }
  }
  if (length(lignes)) do.call(rbind, lignes)
}

# Les termes en pente aléatoire de chaque niveau d'effet aléatoire de la définition `d` (une liste,
# du plus large au plus fin) : les colonnes t1..tK (cadre_regression) des termes simples
# quantitatifs de leurs variables, que verifier_regression a vérifiés.
pentes_definition <- function(d) {
  termes <- termes_regression(d$x)
  simples <- vapply(termes, function(t) if (length(t$variables) == 1 && categorie(t) == 0) as.numeric(t$variables) else NA_real_, 0)
  lapply(d$aleatoires %||% list(), function(s) {
    k <- match(as.numeric(s$pentes %||% numeric(0)), simples)
    if (length(k)) paste0("t", k) else character(0)
  })
}

# Une pente aléatoire suit la pente fixe de son terme : un terme que les observations ne permettent
# pas d'estimer (constant, ou déduit des autres : hors des termes utiles) n'en a pas.
verifier_pentes_utiles <- function(pentes, utiles, termes, noms) {
  hors <- setdiff(unlist(pentes), utiles)
  if (length(hors)) {
    stop(sprintf(traduire("« %s » ne peut pas être estimé sur ces observations (constant, ou déduit des autres termes) : sa pente aléatoire non plus.",
                          "\"%s\" can't be estimated on these observations (constant, or implied by the other terms): nor can its random slope."),
                 paste(noms[match(hors, termes)], collapse = ", ")), call. = FALSE)
  }
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
# ML pour la vraisemblance et l'AIC. noms : le nom affiché de chaque terme t1..tK ; pentes : les
# termes en pente aléatoire de chaque niveau (pentes_definition).
# Renvoie des valeurs simples (pour l'interface), et les deux ajustements dans `ajustement`
# et `ajustement_ml` (à ne pas envoyer).
resultat_lineaire <- function(donnees, termes, noms, niveaux = 0, car1 = FALSE, pentes = NULL) {
  if (car1) verifier_temps_uniques(donnees, niveaux)
  utiles <- termes_utiles(donnees, termes)
  verifier_pentes_utiles(pentes, utiles, termes, noms)
  retires <- noms[match(setdiff(termes, utiles), termes)]
  estime <- ajuster_lineaire(donnees, utiles, niveaux, car1, "REML", pentes)
  compare <- if (niveaux) ajuster_lineaire(donnees, utiles, niveaux, car1, "ML", pentes) else estime
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
    S <- matrices_nlme(estime, niveaux)
    res$variances <- tableau_variances(S, estime$sigma^2, pentes)
    res$r2 <- as.list(r2_mixte(estime, pentes))
    if (length(unlist(pentes))) {
      res$parts <- parts_variance(S, donnees, pentes, estime$sigma^2)
      res$singularites <- singularites_pentes(S, donnees, pentes, estime$sigma)
    }
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
  if (!inherits(m$ajustement, c("gam", "gamm"))) return(ajuster_lineaire(m$donnees, termes, niveaux, m$car1, "ML", m$pentes))
  lis <- intersect(m$lisses, termes)
  if (gamm) return(ajuster_gamm(m$donnees, setdiff(termes, lis), lis, m$k, niveaux, "ML", m$pentes, m$par)$lme)
  ajuster_mgcv(m$donnees, setdiff(termes, lis), lis, m$k, niveaux, m$famille, !is.null(m$decalage), "ML", m$pentes, m$par)
}

# L'apport de chaque groupe de termes (un bloc, ou un terme seul) : le modèle `m` réajusté en ML
# sans lui (ajuster_termes_ml). ΔAIC = AIC sans le groupe - AIC du modèle complet (> 0 : le groupe
# améliore le modèle ; > 2, nettement, Burnham et Anderson, 2002) ; test du rapport de
# vraisemblance. groupes : une liste nommée de vecteurs de termes (t1..tK). Les effets aléatoires
# restent ceux du modèle : sans le terme d'une pente aléatoire, sa pente fixe seule est retirée, et
# les pentes des groupes varient alors autour de 0 (le test de la pente moyenne, à effets aléatoires
# égaux).
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
validation_lineaire <- function(donnees, termes, groupe, niveaux = 0, car1 = FALSE, pentes = NULL) {
  prediction <- rep(NA_real_, nrow(donnees))
  echecs <- 0
  for (g in sort(unique(groupe))) {
    ici <- groupe == g
    apprentissage <- droplevels(donnees[!ici, , drop = FALSE])
    m <- ajuster_lineaire(apprentissage, termes_utiles(apprentissage, termes), niveaux, car1, "REML", pentes)
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
# ou une liste) en une liste de list(by = "id", id = ...) ou list(by = "variable", variable = ...) ;
# un effet aléatoire garde les numéros des variables de ses pentes aléatoires (pentes), s'il en a.
specs_groupement <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) x <- lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
  if (!is.null(x$by)) x <- list(x)
  lapply(x, function(s) {
    par <- as.character(s$by)[1]
    spec <- if (identical(par, "id")) list(by = "id", id = as.character(s$id)[1])
            else if (identical(par, "variable")) list(by = "variable", variable = as.numeric(s$variable)[1])
            else stop(sprintf(traduire("Groupement « %s » inconnu", "Unknown grouping \"%s\""), par), call. = FALSE)
    pentes <- suppressWarnings(as.numeric(unlist(s$pentes)))
    pentes <- unique(pentes[!is.na(pentes)])
    if (length(pentes)) spec$pentes <- pentes
    spec
  })
}

nom_groupement <- function(projet, spec) {
  if (identical(as.character(spec$by)[1], "id")) return(as.character(spec$id)[1])
  tryCatch(as.character(variable_du_projet(projet, spec$variable)$ids[1]), error = function(e) as.character(spec$variable))
}

# Une définition de régression qu'on peut ajuster : une réponse quantitative ; des termes X
# sans spectres, une variable qualitative une fois au plus dans un terme croisé, un facteur sans
# effet principal (facteur_seul) facteur d'un terme croisé ; des effets aléatoires pour un
# modèle mixte (aucun pour un modèle linéaire), une autocorrélation seulement avec eux. Une
# définition lue dans un fichier passe aussi par là.
verifier_regression <- function(projet, d) {
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  if (length(d$y) != 1 || length(d$y[[1]]$variables) != 1 || categorie(d$y[[1]]) != 0 ||
      length(variable_du_projet(projet, d$y[[1]]$variables)$categories)) {
    err("Une régression demande une seule variable Y, quantitative.", "A regression needs a single Y variable, a quantitative one.")
  }
  if (length(d$spectres)) err("Une régression ne prend pas de prétraitement de spectres.", "A regression takes no spectral preprocessing.")
  facteurs <- character(0)  # (les facteurs des termes croisés, « variable catégorie »)
  for (t in d$x) {
    if (length(t$variables) < 2) next
    vs <- lapply(t$variables, function(u) variable_du_projet(projet, u))
    qualitative <- vapply(vs, function(v) length(v$categories) > 0, TRUE)
    if (any(duplicated(as.numeric(t$variables)) & qualitative)) stop(refus_qualitative_double(vapply(vs, function(v) as.character(v$ids[1]), "")), call. = FALSE)
    facteurs <- c(facteurs, vapply(seq_along(t$variables), function(j) paste(t$variables[j], categorie(t, j)), ""))
  }
  for (t in Filter(function(t) isTRUE(t$facteur_seul), d$x)) {
    if (length(t$variables) != 1 || !paste(t$variables[1], categorie(t)) %in% facteurs) {
      stop(sprintf(traduire("« %s » n'a pas d'effet principal et n'est le facteur d'aucun terme croisé du modèle.",
                            "\"%s\" has no main effect and is a factor of none of the model's cross terms."), t$name), call. = FALSE)
    }
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
    simples_x <- unlist(lapply(d$x, function(t) if (length(t$variables) == 1 && categorie(t) == 0 && !isTRUE(t$facteur_seul)) as.numeric(t$variables)))
    if (!all(as.numeric(d$lisses) %in% simples_x)) err("Une courbe se fait sur une variable X quantitative.", "A curve is made on a quantitative X variable.")
    if (length(d$k_courbes)) {  # (la dimension de base d'une courbe, demandée : k)
      k <- suppressWarnings(as.numeric(unlist(d$k_courbes)))
      if (anyNA(k) || any(k < 3 | k > 100 | k != round(k)) || !all(names(d$k_courbes) %in% as.character(as.numeric(d$lisses)))) {
        err("La dimension d'une courbe (k) est un nombre entier de 3 à 100, pour une variable en courbe.", "A curve's dimension (k) is a whole number from 3 to 100, for a variable in a curve.")
      }
    }
    verifier_par_courbes(projet, d)
    if (!is.null(d$decalage)) {
      if (!famille %in% FAMILLES_LOG) err("Un décalage (l'effort) va avec une loi au lien log : Poisson, binomiale négative, gamma ou Tweedie.",
                                          "An offset (the effort) goes with a log-link distribution: Poisson, negative binomial, gamma or Tweedie.")
      if (length(variable_du_projet(projet, d$decalage)$categories)) err("Le décalage est une variable quantitative.", "The offset is a quantitative variable.")
    }
  } else if (length(d$lisses) || length(d$par_courbes) || !is.null(d$decalage) || !is.null(d$famille)) {
    err("Lois, courbes et décalage demandent un modèle généralisé (GLM) ou additif (GAM).", "Distributions, curves and offsets need a generalized (GLM) or additive (GAM) model.")
  }
  if (length(aleatoires) > 3) err("Trois effets aléatoires emboîtés au plus.", "Three nested random effects at most.")
  # (une pente aléatoire varie autour de la pente fixe de sa variable : un terme X quantitatif seul,
  # transformé ou non, en droite)
  simples_x <- unlist(lapply(d$x, function(t) if (length(t$variables) == 1 && categorie(t) == 0) as.numeric(t$variables)))
  for (s in aleatoires) {
    if (identical(as.character(s$by)[1], "variable") && !length(variable_du_projet(projet, s$variable)$categories)) {
      err("Un effet aléatoire se fait sur un identifiant ou une variable qualitative.", "A random effect is made from an ID or a qualitative variable.")
    }
    valeurs_groupement(projet, s, d$observations[1])
    for (u in as.numeric(unlist(s$pentes))) {
      nom <- tryCatch(as.character(variable_du_projet(projet, u)$ids[1]), error = function(e) as.character(u))
      if (!u %in% simples_x) {
        stop(sprintf(traduire("« %s » a une pente aléatoire : elle doit être aussi un terme X du modèle, quantitatif et seul (transformé ou non), la pente fixe autour de laquelle varient celles des groupes.",
                              "\"%s\" has a random slope: it must also be an X term of the model, quantitative and on its own (transformed or not), the fixed slope around which the groups' slopes vary."), nom), call. = FALSE)
      }
      if (u %in% as.numeric(d$lisses)) {
        stop(sprintf(traduire("« %s » a une pente aléatoire : elle ne peut pas être aussi une courbe (une pente aléatoire varie autour d'une pente fixe, une droite).",
                              "\"%s\" has a random slope: it can't also be a curve (a random slope varies around a fixed slope, a straight line)."), nom), call. = FALSE)
      }
    }
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

# Les courbes par catégorie d'une définition `d` (d$par_courbes : pour une variable en courbe, par
# son numéro, celui d'une variable qualitative ; mgcv : s(x, by = f)) : chacune sur une courbe du
# modèle, et par une variable qualitative X du modèle, son effet principal. Les courbes de mgcv par
# catégorie sont centrées, comme toute courbe : sans l'effet principal de la variable qualitative,
# chaque catégorie aurait la même moyenne ; Nuée le demande plutôt que de l'ajouter sans le dire.
# Une variable en courbe par catégorie n'est pas aussi dans un terme croisé avec une variable
# qualitative : chaque catégorie a déjà sa courbe.
verifier_par_courbes <- function(projet, d) {
  par <- d$par_courbes %||% list()
  if (!length(par)) return(invisible(TRUE))
  if (!identical(d$type, "GAM")) stop(traduire("Une courbe par catégorie demande un modèle additif (GAM).", "A curve per category needs an additive model (GAM)."), call. = FALSE)
  nom <- function(u) tryCatch(as.character(variable_du_projet(projet, u)$ids[1]), error = function(e) as.character(u))
  principales <- unlist(lapply(d$x, function(t) if (length(t$variables) == 1 && categorie(t) != 0 && !isTRUE(t$facteur_seul)) as.numeric(t$variables)))
  for (u in names(par)) {
    f <- suppressWarnings(as.numeric(unlist(par[[u]]))[1])
    if (!as.numeric(u) %in% as.numeric(d$lisses)) {
      stop(sprintf(traduire("« %s » n'est pas une courbe du modèle : une courbe par catégorie se fait sur une variable en courbe.",
                            "\"%s\" is not a curve of the model: a curve per category is made on a variable in a curve."), nom(as.numeric(u))), call. = FALSE)
    }
    if (is.na(f) || !length(tryCatch(variable_du_projet(projet, f)$categories, error = function(e) NULL))) {
      stop(sprintf(traduire("La courbe de « %s » se fait par catégorie d'une variable qualitative.", "The curve of \"%s\" is made per category of a qualitative variable."),
                   nom(as.numeric(u))), call. = FALSE)
    }
    if (!f %in% principales) {
      stop(sprintf(traduire("« %s » a une courbe par catégorie de « %s » : « %s » doit être aussi une variable X du modèle (son effet principal). Chaque courbe de mgcv est centrée : sans lui, toutes les catégories auraient la même moyenne.",
                            "\"%s\" has a curve per category of \"%s\": \"%s\" must also be an X variable of the model (its main effect). Each mgcv curve is centred: without it, every category would have the same mean."),
                   nom(as.numeric(u)), nom(f), nom(f)), call. = FALSE)
    }
    for (t in d$x) {
      if (length(t$variables) > 1 && as.numeric(u) %in% as.numeric(t$variables) &&
          any(vapply(seq_along(t$variables), function(j) categorie(t, j) != 0, TRUE))) {
        stop(sprintf(traduire("« %s » a une courbe par catégorie de « %s » : elle ne peut pas être aussi dans un terme croisé avec une variable qualitative (chaque catégorie a déjà sa courbe).",
                              "\"%s\" has a curve per category of \"%s\": it can't also be in a cross term with a qualitative variable (each category already has its curve)."),
                     nom(as.numeric(u)), nom(f)), call. = FALSE)
      }
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
# variable (les catégories d'une variable qualitative ensemble) ; un terme croisé, lui-même, ses
# variables jointes par « * » (« annee*secteur » : les produits de chaque catégorie ensemble).
groupes_termes <- function(projet, termes) {
  vapply(termes, function(t) {
    b <- t$scaling$block
    if (length(b) && !is.na(b[1]) && nzchar(as.character(b[1]))) return(as.character(b[1]))
    paste(vapply(t$variables, function(u) as.character(variable_du_projet(projet, u)$ids[1]), ""), collapse = "*")
  }, "")
}

# Les données d'une régression pour les observations `obs` : y (la réponse, transformée, sans
# mise à l'échelle), t1..tK (les termes X, construire_bloc), g1..gL (les effets aléatoires,
# du plus large au plus fin) et temps. Seules restent les observations où tout est connu.
# `imposees` : les centres et poids d'un modèle déjà ajusté (pour un jeu de test). Renvoie aussi
# les termes qui peuvent entrer dans l'ajustement (`candidats`, candidats_regression) et ce que
# chacun croise (`composition`, composition_termes).
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
  noms_t <- if (K) paste0("t", seq_len(K)) else character(0)
  composition <- stats::setNames(composition_termes(projet, termes), noms_t)
  cadre <- cadre[complet, , drop = FALSE]
  # (une courbe par catégorie, s(x, by = f) : le facteur de sa variable qualitative, une colonne)
  facteurs <- if (identical(d$type, "GAM")) facteurs_courbes(d, composition) else list()
  for (f in names(facteurs)) cadre[[f]] <- facteur_courbe(cadre, facteurs[[f]]$colonnes)
  list(cadre = cadre, observations = garde[complet], exclues = setdiff(obs, garde[complet]),
       termes = noms_t, noms = if (K) bloc$noms else character(0), bloc = bloc,
       candidats = candidats_regression(termes, noms_t), composition = composition, facteurs = facteurs)
}

# Les facteurs des courbes par catégorie de la définition `d` (d$par_courbes ; mgcv : s(x, by = f)) :
# pour chaque variable qualitative d'une courbe, la colonne de son facteur dans le tableau du modèle
# (« f » et son numéro), son nom, et les colonnes 0/1 de ses catégories (`composition` :
# composition_termes ; son effet principal, que verifier_par_courbes demande), nommées par leur
# catégorie. Une liste nommée par ces colonnes.
facteurs_courbes <- function(d, composition) {
  par <- d$par_courbes %||% list()
  vars <- unique(as.numeric(unlist(par[names(par) %in% as.character(as.numeric(d$lisses))])))
  if (!length(vars)) return(list())
  stats::setNames(lapply(vars, function(f) {
    ts <- Filter(function(t) length(composition[[t]]$variables) == 1 && composition[[t]]$variables == f && composition[[t]]$categories != 0 &&
                   !composition[[t]]$facteur_seul, names(composition))
    list(variable = f, nom = composition[[ts[1]]]$noms, colonnes = stats::setNames(ts, vapply(ts, function(t) composition[[t]]$niveaux, "")))
  }), paste0("f", vars))
}

# Le facteur d'une courbe par catégorie pour les lignes de `donnees` : la catégorie de chacune, lue
# dans ses colonnes 0/1 `colonnes` (nommées par leur catégorie), NA quand aucune n'y vaut 1 ; ses
# niveaux, `niveaux` (ceux d'un modèle ajusté : une autre catégorie est NA), sinon les catégories
# que ces lignes ont, dans l'ordre des catégories de la variable.
facteur_courbe <- function(donnees, colonnes, niveaux = NULL) {
  x <- rep(NA_character_, nrow(donnees))
  for (k in names(colonnes)) x[!is.na(donnees[[colonnes[[k]]]]) & donnees[[colonnes[[k]]]] == 1] <- k
  factor(x, levels = niveaux %||% intersect(names(colonnes), x))
}

# Un facteur aux niveaux `niveaux` (ceux d'un ajustement de mgcv) : une valeur que l'ajustement n'a
# pas, ou manquante, prend son premier niveau, car mgcv refuse un niveau qu'il n'a pas vu ; ces
# lignes sont celles d'une catégorie que le modèle n'a pas vue, sans prédiction (niveaux_inconnus).
niveaux_appris <- function(x, niveaux) {
  x <- as.character(x)
  factor(ifelse(!is.na(x) & x %in% niveaux, x, niveaux[1]), levels = niveaux)
}

# Les termes t1..tK qui peuvent entrer dans l'ajustement, dans l'ordre où termes_utiles les
# prend : sans les facteurs seuls (ils ne servent qu'à calculer les termes croisés), les termes
# d'ordre plus bas d'abord (le nombre de leurs variables : effets principaux et carrés, puis
# termes croisés de deux variables, puis de trois), dans leur ordre à ordre égal. C'est la règle
# de R (terms() range les termes par ordre) : les colonnes des catégories de référence, et celles
# d'un terme croisé qui se déduisent de ses effets principaux, sont retirées, comme R code un
# facteur par des contrastes quand l'effet de l'autre facteur est dans le modèle, sinon par une
# colonne par catégorie (Chambers et Hastie, 1992).
candidats_regression <- function(termes, noms_t) {
  seuls <- vapply(termes, function(t) isTRUE(t$facteur_seul), TRUE)
  ordre <- vapply(termes, function(t) length(unique(as.numeric(t$variables))), 0L)
  noms_t[!seuls][order(ordre[!seuls])]
}

# Ce que croise chaque terme t1..tK d'une régression : ses variables (leurs numéros et leurs
# noms), la catégorie de chacune (0 pour une variable quantitative) et son nom (NA pour une
# variable quantitative), et s'il est un facteur seul (sans effet principal). Gardé avec le
# modèle (m$composition), pour ne jamais relire le nom d'une colonne.
composition_termes <- function(projet, termes) {
  lapply(termes, function(t) {
    vs <- lapply(t$variables, function(u) variable_du_projet(projet, u))
    cats <- vapply(seq_along(vs), function(j) as.numeric(categorie(t, j)), 0)
    niveaux <- vapply(seq_along(vs), function(j) if (cats[j] != 0 && length(vs[[j]]$categories) >= cats[j]) as.character(vs[[j]]$categories[cats[j]]) else NA_character_, "")
    list(variables = as.numeric(t$variables), noms = vapply(vs, function(v) as.character(v$ids[1]), ""), categories = cats, niveaux = niveaux,
         facteur_seul = isTRUE(t$facteur_seul))
  })
}

# Les lignes de `donnees` (le tableau d'une régression) qui ont toutes les catégories du terme `t`
# (0 ou 1 chacune), lues dans les colonnes 0/1 de ces catégories (leurs termes simples, effets
# principaux ou facteurs seuls) ; TRUE pour un terme sans catégorie.
lignes_cellule <- function(donnees, composition, t) {
  ct <- composition[[t]]
  dans <- rep(TRUE, nrow(donnees))
  for (j in which(ct$categories != 0)) {
    cle <- Find(function(s) length(composition[[s]]$variables) == 1 && composition[[s]]$variables == ct$variables[j] &&
                  composition[[s]]$categories == ct$categories[j], names(composition))
    if (!is.null(cle)) dans <- dans & !is.na(donnees[[cle]]) & donnees[[cle]] == 1
  }
  dans
}

# Le terme `t` porte-t-il une catégorie, ou une combinaison de catégories, qu'aucune ligne de
# `donnees` n'a ? (une combinaison jamais observée : sa colonne est constante, et l'ajustement la
# laisse de côté)
cellule_vide <- function(donnees, composition, t) any(composition[[t]]$categories != 0) && !any(lignes_cellule(donnees, composition, t))

# Les cellules des variables qualitatives d'une régression : chaque catégorie, ou combinaison de
# catégories, d'un terme de `candidats` (l'effet principal d'une variable qualitative, un terme
# croisé qui a une catégorie), une fois chacune, lue dans les colonnes 0/1 de ses catégories (leurs
# termes simples, effets principaux ou facteurs seuls, comme lignes_cellule) ; et, pour chaque
# variable qualitative, l'absence de toutes ses catégories (une catégorie que la définition du
# modèle n'a pas, ajoutée depuis aux données : toutes ses colonnes à 0). Chacune : `valeurs`, la
# valeur de chacune de ses colonnes (1, ou 0 pour l'absence), et `nom`, ses variables (« secteur »,
# « habitat × traitement »).
cellules_qualitatives <- function(composition, candidats) {
  simple <- function(s, u) length(composition[[s]]$variables) == 1 && composition[[s]]$variables == u
  cellules <- list()
  ajouter <- function(valeurs, nom) {
    cle <- paste(sort(paste(names(valeurs), valeurs)), collapse = " ")
    if (is.null(cellules[[cle]])) cellules[[cle]] <<- list(valeurs = valeurs, nom = nom)
  }
  for (t in candidats) {
    ct <- composition[[t]]
    j <- which(ct$categories != 0)
    if (!length(j)) next
    colonnes <- vapply(j, function(i) Find(function(s) simple(s, ct$variables[i]) && composition[[s]]$categories == ct$categories[i], names(composition)) %||% NA_character_, "")
    if (anyNA(colonnes)) next
    ajouter(stats::setNames(rep(1, length(colonnes)), colonnes), paste(ct$noms[j], collapse = " × "))
    for (i in j) {
      toutes <- Filter(function(s) simple(s, ct$variables[i]) && composition[[s]]$categories != 0, names(composition))
      ajouter(stats::setNames(rep(0, length(toutes)), toutes), ct$noms[i])
    }
  }
  unname(cellules)
}

# Les lignes de `nouvelles` d'une catégorie, ou d'une combinaison de catégories, qu'aucune des
# lignes `appris` n'a (`cellules` : cellules_qualitatives) : l'ajustement ne l'a pas vue (sa colonne,
# constante, est laissée de côté). Prédites, elles le seraient comme la catégorie de référence, ou
# sans la pente de leur catégorie : elles restent sans prédiction, comme R refuse un niveau qu'il
# n'a pas vu. Attribut `noms` : les variables de leurs cellules (« secteur », « habitat × traitement »).
niveaux_inconnus <- function(appris, nouvelles, cellules) {
  dans <- function(tab, valeurs) {
    ici <- rep(TRUE, nrow(tab))
    for (k in names(valeurs)) ici <- ici & !is.na(tab[[k]]) & tab[[k]] == valeurs[[k]]
    ici
  }
  sans <- rep(FALSE, nrow(nouvelles))
  noms <- character(0)
  for (cel in cellules) {
    if (any(dans(appris, cel$valeurs))) next
    ici <- dans(nouvelles, cel$valeurs)
    if (any(ici)) noms <- union(noms, cel$nom)
    sans <- sans | ici
  }
  structure(sans, noms = noms)
}

# Q2 et RMSEcv des prédictions de validation croisée `prediction` (sans celles qui manquent).
qualite_prediction <- function(y, prediction) {
  ok <- !is.na(prediction)
  list(Q2 = if (any(ok)) 1 - sum((y[ok] - prediction[ok])^2) / sum((y[ok] - mean(y[ok]))^2) else NA_real_,
       RMSEcv = if (any(ok)) sqrt(mean((y[ok] - prediction[ok])^2)) else NA_real_)
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
  pentes <- pentes_definition(d)  # (les termes en pente aléatoire de chaque niveau)
  car1 <- !is.null(d$correlation)
  # (les termes qui peuvent entrer dans l'ajustement, d'ordre plus bas d'abord : candidats_regression)
  candidats <- cadre$candidats
  noms_candidats <- cadre$noms[match(candidats, cadre$termes)]
  if (nrow(cadre$cadre) <= length(candidats) + 1) {
    stop(sprintf(traduire("Trop peu d'observations complètes (%d) pour %d termes.", "Too few complete observations (%d) for %d terms."),
                 nrow(cadre$cadre), length(candidats)), call. = FALSE)
  }
  G <- d$cv_groups %||% 7
  groupes <- groupes_definition(projet, d, G)[match(cadre$observations, d$observations)]
  termes_x <- termes_regression(d$x)
  quantitatifs <- cadre$termes[vapply(termes_x, function(t) length(t$variables) == 1 && categorie(t) == 0 && !isTRUE(t$facteur_seul), TRUE)]
  famille <- if (generalise) as.character(d$famille %||% "gaussienne")[1] else "gaussienne"
  lisses <- par <- character(0)
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
    # (une courbe par catégorie : la colonne du facteur de sa variable qualitative, par terme ; facteurs_courbes)
    par <- unlist(lapply(lisses, function(t) {
      f <- d$par_courbes[[as.character(as.numeric(termes_x[[match(t, cadre$termes)]]$variables))]]
      if (length(f)) stats::setNames(paste0("f", as.numeric(unlist(f))[1]), t)
    }))
    if (gamm) {
      r <- resultat_gamm(cadre$cadre, candidats, noms_candidats, lisses, niveaux, k_demandes, pentes, par, cadre$facteurs)
      vc <- validation_gamm(cadre$cadre, r$utiles, lisses, r$k, groupes, niveaux, pentes, par)
    } else {
      r <- resultat_mgcv(cadre$cadre, candidats, noms_candidats, lisses, niveaux, famille, !is.null(d$decalage), k_demandes, pentes, par, cadre$facteurs)
      vc <- validation_mgcv(cadre$cadre, r$utiles, lisses, r$k, groupes, niveaux, famille, !is.null(d$decalage), pentes, par)
    }
  } else {
    r <- resultat_lineaire(cadre$cadre, candidats, noms_candidats, niveaux, car1, pentes)
    vc <- validation_lineaire(cadre$cadre, r$utiles, groupes, niveaux, car1, pentes)
  }
  # (les termes laissés de côté qu'aucune observation n'a : une catégorie, une combinaison de catégories)
  retires <- setdiff(candidats, r$utiles)
  r$termes_absents <- cadre$noms[match(retires[vapply(retires, function(t) cellule_vide(cadre$cadre, cadre$composition, t), TRUE)], cadre$termes)]
  # (une catégorie, ou une combinaison de catégories, que les lignes d'apprentissage d'un groupe
  # n'ont pas : pas de prédiction, plutôt que celle de la catégorie de référence ; niveaux_inconnus.
  # `facteurs` : leurs variables ; Q2 et RMSEcv sur les lignes qui ont une prédiction)
  vc$sans_categorie <- 0L
  vc$facteurs <- character(0)
  cellules <- cellules_qualitatives(cadre$composition, candidats)
  if (length(cellules)) {
    for (g in unique(groupes)) {
      ici <- which(groupes == g)
      inconnues <- niveaux_inconnus(cadre$cadre[-ici, , drop = FALSE], cadre$cadre[ici, , drop = FALSE], cellules)
      sans <- ici[inconnues & !is.na(vc$prediction[ici])]
      if (length(sans)) vc$facteurs <- union(vc$facteurs, attr(inconnues, "noms"))
      vc$prediction[sans] <- NA
      vc$sans_categorie <- vc$sans_categorie + length(sans)
    }
    vc[c("Q2", "RMSEcv")] <- qualite_prediction(cadre$cadre$y, vc$prediction)
  }
  y <- d$y[[1]]
  m <- list(type = d$type, regression = TRUE, plsda = FALSE, opls = FALSE, A = 0, nom = d$name %||% "", titre = d$title %||% "",
            N = nrow(cadre$cadre), K = length(r$utiles), observations = cadre$observations, exclues = cadre$exclues,
            y = list(uid = as.numeric(y$variables), noms = as.character(y$name), transformation = nom_transformation(y$transform) %||% ""),
            noms_termes = cadre$noms, termes = cadre$termes, utiles = r$utiles, quantitatifs = quantitatifs,
            candidats = candidats, composition = cadre$composition,
            lisses = intersect(lisses, r$utiles), k = r$k, famille = famille, lien = r$lien %||% "identity",
            # (les courbes par catégorie : le facteur de chacune, et ce qu'est chaque facteur)
            par = par[intersect(names(par), r$utiles)], facteurs_courbes = cadre$facteurs,
            decalage = if (!is.null(d$decalage)) nom_groupement(projet, list(by = "variable", variable = d$decalage)),
            groupes_termes = stats::setNames(groupes_termes(projet, d$x), cadre$termes),
            niveaux = vapply(d$aleatoires %||% list(), function(s) nom_groupement(projet, s), ""), pentes = pentes,
            temps = if (car1) nom_groupement(projet, d$correlation$temps), car1 = car1, gamm = gamm,
            dans = if (car1 && !is.null(d$correlation$dans)) nom_groupement(projet, d$correlation$dans),
            resultat = r[setdiff(names(r), c("ajustement", "ajustement_ml", "utiles", "k"))],
            ajustement = r$ajustement, ajustement_ml = r$ajustement_ml, donnees = cadre$cadre, bloc = cadre$bloc,
            G = length(unique(groupes)), vc_par = nom_groupes_vc(projet, d),
            vc = list(Q2 = vc$Q2, RMSEcv = vc$RMSEcv, prediction = vc$prediction, groupe = groupes, echecs = vc$echecs,
                      sans_categorie = vc$sans_categorie, facteurs = vc$facteurs))
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
# cadre_regression) : par les effets fixes seuls quand le modèle a des effets aléatoires ; sans
# prédiction, une catégorie, ou une combinaison de catégories, que le modèle n'a pas vue
# (niveaux_inconnus : ces lignes, en attribut `inconnues`).
predire_regression <- function(m, nd) {
  a <- m$ajustement
  inconnues <- niveaux_inconnus(m$donnees, nd, cellules_qualitatives(m$composition, m$candidats))
  # (le facteur d'une courbe par catégorie, lu dans les colonnes 0/1 de ses catégories, aux niveaux du modèle)
  for (f in names(m$facteurs_courbes)) {
    nd[[f]] <- niveaux_appris(facteur_courbe(nd, m$facteurs_courbes[[f]]$colonnes, levels(m$donnees[[f]])), levels(m$donnees[[f]]))
  }
  p <- if (inherits(a, "gamm")) as.vector(stats::predict(a$gam, nd, type = "response"))  # ($gam : sans les effets aléatoires)
       else if (inherits(a, "gam")) predire_mgcv(a, nd, m$donnees, length(m$niveaux), m$par)
       else if (inherits(a, "lme")) as.vector(stats::predict(a, nd, level = 0))
       else as.vector(stats::predict(a, nd))
  p[inconnues] <- NA
  structure(p, inconnues = inconnues)
}

# Le jeu de test d'une régression : ses observations prédites par les effets fixes (un modèle
# mixte ne connaît pas leurs groupes : niveau 0), dans la forme de evaluer_test (jeu_test.R). Une
# observation d'une catégorie que le modèle n'a pas vue reste sans prédiction, hors des
# statistiques : `inconnues` les compte, `facteurs_inconnus` nomme leurs variables.
tester_regression <- function(projet, m, d) {
  t <- cadre_regression(projet, d, d$test, imposees = m$bloc)
  nd <- t$cadre
  predit <- predire_regression(m, nd)
  inconnues <- attr(predit, "inconnues")
  predit <- as.vector(predit)
  y <- m$y$noms
  e <- predit - nd$y
  obs <- data.frame(uid = t$observations, nom = unname(projet$noms_obs[as.character(t$observations)]), hors_domaine = FALSE, stringsAsFactors = FALSE)
  obs[[paste(y, "observé")]] <- nd$y
  obs[[paste(y, "prédit")]] <- predit
  ok <- !is.na(e)
  list(n = nrow(nd), hors_domaine = 0, inconnues = sum(inconnues), facteurs_inconnus = I(attr(inconnues, "noms")), observations = obs,
       statistiques = data.frame(y = y, n = sum(ok), RMSEP = sqrt(mean(e[ok]^2)), biais = mean(e[ok]), SEP = if (sum(ok) > 1) stats::sd(e[ok]) else NA_real_,
                                 Q2ext = 1 - sum(e[ok]^2) / sum((nd$y[ok] - mean(m$donnees$y))^2), stringsAsFactors = FALSE))
}

# Les prédictions d'une régression pour des observations du projet, d'un jeu de prédiction ou
# d'un fichier (prediction.R : predire, predire_fichier) : Y prédit par les effets fixes, sur
# l'échelle de la réponse modélisée (sa transformation comprise) ; Y observé quand il est connu ;
# hors_domaine : une valeur d'un terme hors de l'étendue des observations du modèle (une
# extrapolation). Une observation sans une valeur d'un terme du modèle, ou sans son effort, n'a
# pas de prédiction ; une observation d'une catégorie, ou d'une combinaison de catégories, que le
# modèle n'a pas vue non plus (predire_regression) : la colonne categorie_inconnue les marque,
# quand il y en a. `effort_absent` : le fichier n'a pas l'effort ; la prédiction est alors
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
  inconnue <- rep(FALSE, n)
  if (any(connu)) {
    p <- predire_regression(m, nd[connu, , drop = FALSE])
    predit[connu] <- as.vector(p)
    inconnue[connu] <- as.vector(attr(p, "inconnues"))
  }
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
  if (any(inconnue)) out$categorie_inconnue <- inconnue
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
# résiduelle ; mgcv : les effets aléatoires seuls), ou NULL ; avec des pentes aléatoires, chaque
# terme nommé par le nom de son terme X (tableau_variances).
variances_regression <- function(m) {
  v <- m$resultat$variances
  if (is.null(v)) return(NULL)
  v$composante <- c(m$niveaux, traduire("résiduelle", "residual"))[match(v$composante, c(paste0("g", seq_along(m$niveaux)), "residuelle"))]
  if (!is.null(v$terme)) v$terme <- ifelse(v$terme %in% m$termes, m$noms_termes[match(v$terme, m$termes)], v$terme)
  v
}

# Les termes en pente aléatoire de chaque niveau d'effet aléatoire, par leur nom (une liste, du
# niveau le plus large au plus fin).
pentes_regression <- function(m) lapply(m$pentes %||% list(), function(p) I(m$noms_termes[match(p, m$termes)]))

# Les parts de variance des niveaux d'un modèle à pentes aléatoires (parts_variance) et ses signes
# d'ajustement singulier (singularites_pentes), nommés par leurs groupements et leurs termes, ou NULL.
parts_regression <- function(m) {
  p <- m$resultat$parts
  if (!is.null(p)) p$composante <- c(m$niveaux, traduire("résiduelle", "residual"))[match(p$composante, c(paste0("g", seq_along(m$niveaux)), "residuelle"))]
  p
}
singularites_regression <- function(m) {
  s <- m$resultat$singularites
  if (is.null(s)) return(NULL)
  nom <- function(t) ifelse(t %in% m$termes, m$noms_termes[match(t, m$termes)], t)
  s$niveau <- m$niveaux[match(s$niveau, paste0("g", seq_along(m$niveaux)))]
  s$terme <- nom(s$terme)
  s$avec <- nom(s$avec)
  s
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
# demande : retrait_groupes. Un groupe sort avec ceux qui le demandent (groupes_requis), de proche
# en proche : un effet principal avec ses termes croisés, qui n'entrent qu'avec lui ; `avec` les
# nomme.
apport_groupes <- function(m) {
  if (!length(m$utiles)) return(NULL)
  groupes <- groupes_modele(m)
  requis <- groupes_requis(groupes, m$composition, requis_courbes(m))
  avec <- function(g) repeat {
    plus <- union(g, names(requis)[vapply(requis, function(r) any(r %in% g), TRUE)])
    if (length(plus) == length(g)) return(g)
    g <- plus
  }
  a <- retrait_groupes(m, lapply(stats::setNames(names(groupes), names(groupes)), function(g) unlist(groupes[avec(g)], use.names = FALSE)))
  if (!is.null(a)) a$avec <- vapply(a$groupe, function(g) paste(setdiff(avec(g), g), collapse = ", "), "", USE.NAMES = FALSE)
  a
}

# ------------------------------------------------------------ sélection par l'AIC

# Les groupes que demande chaque groupe de termes : ceux des termes d'ordre plus bas que ses termes
# croisés contiennent, ses effets principaux (une variable qualitative par toutes ses catégories)
# et les termes croisés d'une partie de ses facteurs (a × b dans a × b × c, x × x dans x × x ×
# secteur) ; un terme croisé n'entre qu'avec eux. `composition` : ce que croise chaque terme
# (m$composition, composition_termes) ; `courbes` : les termes que demande en plus chaque courbe
# par catégorie, l'effet principal de sa variable qualitative (requis_courbes) ; une liste nommée
# comme `groupes`.
groupes_requis <- function(groupes, composition, courbes = NULL) {
  cles <- function(t) paste(composition[[t]]$variables, composition[[t]]$categories)
  contenu <- function(a, b) length(a) < length(b) && all(vapply(unique(a), function(k) sum(a == k) <= sum(b == k), TRUE))
  groupe_de <- stats::setNames(rep(names(groupes), lengths(groupes)), unlist(groupes, use.names = FALSE))
  lapply(stats::setNames(names(groupes), names(groupes)), function(nom) {
    demandes <- unlist(lapply(groupes[[nom]], function(tk) c(Filter(function(s) contenu(cles(s), cles(tk)), names(groupe_de)), intersect(courbes[[tk]], names(groupe_de)))))
    setdiff(unique(unname(groupe_de[demandes])), nom)
  })
}

# Les termes que demande chaque courbe par catégorie d'une régression `m` (s(x, by = f)) : les
# colonnes 0/1 de sa variable qualitative, son effet principal ; une liste nommée par terme en courbe.
requis_courbes <- function(m) lapply(m$par %||% character(0), function(f) unname(m$facteurs_courbes[[f]]$colonnes))

# Les groupes de termes que la sélection par l'AIC garde dans chaque modèle : ceux qui portent une
# pente aléatoire (elle varie autour de la pente fixe de son terme, qui reste), et ceux qu'ils
# demandent (groupes_requis : les effets principaux de leurs termes croisés).
groupes_gardes <- function(m) {
  tous <- groupes_modele(m)
  gardes <- names(tous)[vapply(tous, function(ts) any(ts %in% unlist(m$pentes)), TRUE)]
  requis <- groupes_requis(tous, m$composition, requis_courbes(m))
  repeat {
    plus <- setdiff(unlist(requis[gardes]), gardes)
    if (!length(plus)) break
    gardes <- c(gardes, plus)
  }
  gardes
}

# La sélection des effets fixes par l'AICc (Burnham et Anderson, 2002), calculée à la demande : les
# combinaisons des groupes de termes du modèle `m` (groupes_modele), du modèle nul au modèle
# complet, chacune ajustée en ML sur les observations du modèle (ajuster_termes_ml) ; ses effets
# aléatoires, son autocorrélation, sa loi, son décalage et le réglage de ses courbes restent ceux
# du modèle. Un terme croisé ou un carré n'entre qu'avec ses effets principaux et les termes croisés
# d'une partie de ses facteurs (groupes_requis, d'après m$composition ; `d`, la définition du
# modèle, n'y sert plus). Toutes les combinaisons quand 2^K ≤ `limite` (sur 240
# observations, les 256 combinaisons de 8 groupes d'un nlme::lme avec CAR1 prennent une dizaine de
# secondes, celles d'un mgcv::gamm avec deux courbes une demi-minute) ; au-delà, une élimination
# descendante depuis le modèle complet : à chaque pas, le retrait qui baisse le plus l'AICc, jusqu'à
# ce qu'aucun ne la baisse. Les modèles sont rangés par AICc : ΔAICc, poids d'Akaike (sur les
# modèles ajustés) et leur cumul ; l'importance d'un groupe est la somme des poids des modèles qui
# le contiennent. Le meilleur modèle a l'AICc le plus bas (le premier) ; le plus parcimonieux, le
# moins de paramètres parmi ceux à ΔAICc ≤ 2 (Burnham et Anderson, 2002).
# Les groupes qui portent une pente aléatoire restent dans chaque modèle, hors des groupes à
# choisir (groupes_gardes : `gardes`).
selection_aic <- function(m, d, limite = 256) {
  tous <- groupes_modele(m)
  gardes <- groupes_gardes(m)
  groupes <- tous[setdiff(names(tous), gardes)]
  fixes <- unlist(tous[gardes], use.names = FALSE)  # (les termes de chaque modèle)
  K <- length(groupes)
  n <- nrow(m$donnees)
  if (!K) return(list(K = 0L, N = n, gardes = I(gardes)))
  noms <- names(groupes)
  requis <- groupes_requis(groupes, m$composition, requis_courbes(m))
  permis <- function(b) all(vapply(noms[b], function(g) all(requis[[g]] %in% noms[b]), TRUE))
  evaluer <- function(b) {  # (un candidat : les groupes qu'il contient)
    a <- ajuster_termes_ml(m, intersect(m$utiles, c(fixes, unlist(groupes[b], use.names = FALSE))))
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
  list(methode = if (toutes) "toutes" else "descendante", limite = limite, N = n, K = K, groupes = I(noms), gardes = I(gardes),
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
  # (les groupes qui portent une pente aléatoire, dans chaque modèle de la sélection : groupes_gardes)
  gardes <- groupes_gardes(m)
  garde <- unname(m$groupes_termes) %in% union(choisis, gardes)  # (le groupe de chaque terme de d$x, dans l'ordre)
  simple <- vapply(d$x, function(t) length(t$variables) == 1, TRUE)
  cle <- vapply(d$x, function(t) paste(t$variables[1], categorie(t)), "")
  facteurs <- unlist(lapply(d$x[garde & !simple], function(t) vapply(seq_along(t$variables), function(j) paste(t$variables[j], categorie(t, j)), "")))
  d$x <- d$x[garde | (simple & cle %in% facteurs)]
  restantes <- unlist(lapply(d$x, function(t) if (length(t$variables) == 1) as.numeric(t$variables)))
  d$lisses <- intersect(as.numeric(d$lisses), restantes)
  if (!length(d$lisses)) d$lisses <- NULL
  d$k_courbes <- d$k_courbes[names(d$k_courbes) %in% as.character(d$lisses)]
  if (!length(d$k_courbes)) d$k_courbes <- NULL
  # (une courbe par catégorie reste avec sa courbe et l'effet principal de sa variable qualitative, que la sélection garde avec elle)
  principales <- unlist(lapply(d$x, function(t) if (length(t$variables) == 1 && categorie(t) != 0 && !isTRUE(t$facteur_seul)) as.numeric(t$variables)))
  d$par_courbes <- Filter(function(f) as.numeric(unlist(f))[1] %in% principales, d$par_courbes[names(d$par_courbes) %in% as.character(d$lisses)])
  if (!length(d$par_courbes)) d$par_courbes <- NULL
  d$termes_complets <- complets
  d$selection_origine <- c(list(modele = as.character(d$name %||% ""), groupes = choisis, n = length(m$observations)), if (length(gardes)) list(gardes = gardes))
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
# courbes s(tk, k = ...), ou s(tk, by = f12, k = ...) pour une courbe par catégorie (`par` : la
# colonne de son facteur, par terme), effets aléatoires s(g1, bs = "re") (g12 : le niveau 2 dans le
# 1...) et leurs pentes aléatoires s(g1, t3, bs = "re") (`pentes` : les termes de chaque niveau ; une
# pente indépendante de l'ordonnée à l'origine, mgcv ne les corrèle pas), décalage. Son environnement
# donne s() de mgcv, qui relit ces termes : ils ne viennent que d'ici.
formule_mgcv <- function(parametriques, lisses, k, niveaux, decalage, pentes = NULL, par = NULL) {
  groupes <- c("g1", "g12", "g123")[seq_len(niveaux)]
  aleatoires <- c(sprintf("s(%s, bs = \"re\")", groupes), unlist(lapply(seq_len(niveaux), function(l) sprintf("s(%s, %s, bs = \"re\")", groupes[l], pentes[[l]]))))
  courbes <- vapply(lisses, function(t) if (t %in% names(par)) sprintf("s(%s, by = %s, k = %d)", t, par[[t]], as.integer(k[[t]]))
                                        else sprintf("s(%s, k = %d)", t, as.integer(k[[t]])), "", USE.NAMES = FALSE)
  termes <- c(parametriques, courbes, aleatoires, if (decalage) "offset(decalage)")
  stats::as.formula(paste("y ~", if (length(termes)) paste(termes, collapse = " + ") else "1"), env = new.env(parent = asNamespace("mgcv")))
}

ajuster_mgcv <- function(donnees, parametriques, lisses, k, niveaux, famille, decalage, methode = "REML", pentes = NULL, par = NULL) {
  tryCatch(mgcv::gam(formule_mgcv(parametriques, lisses, k, niveaux, decalage, pentes, par), data = donnees, family = famille_mgcv(famille), method = methode),
           error = function(e) NULL)
}

# L'étiquette de mgcv de la courbe du terme `t` : s(t3), ou pour une courbe par catégorie (`par` :
# la colonne de son facteur) celle de chaque niveau de son facteur dans `donnees`, s(t3):f12S1...
etiquettes_courbe <- function(t, par, donnees) {
  if (!t %in% names(par)) return(sprintf("s(%s)", t))
  sprintf("s(%s):%s%s", t, par[[t]], levels(donnees[[par[[t]]]]))
}

# Les colonnes (et les coefficients) de la courbe d'étiquette `etiquette` d'un ajustement de mgcv `a`.
colonnes_courbe <- function(a, etiquette) {
  s <- Find(function(s) identical(s$label, etiquette), a$smooth)
  seq(s$first.para, s$last.para)
}

# Le tableau des courbes d'un ajustement de mgcv `a` (sa partie gam pour mgcv::gamm) sur `donnees`,
# ses courbes `lis` (leurs noms : `noms` des `termes`) : une ligne par courbe, et pour une courbe par
# catégorie (`par` ; `facteurs` : facteurs_courbes) une ligne par niveau de son facteur, avec le nom
# de sa variable qualitative (par) et le niveau (niveau) ; edf, ddl de référence, statistique et p de
# summary(), et la concurvité de chacune (concurvite_courbes).
tableau_courbes <- function(a, lis, termes, noms, donnees, par = NULL, facteurs = NULL) {
  st <- summary(a)$s.table
  lignes <- lapply(lis, function(t) {
    e <- etiquettes_courbe(t, par, donnees)
    f <- if (t %in% names(par)) par[[t]]
    data.frame(etiquette = e, terme = noms[match(t, termes)], par = if (is.null(f)) NA_character_ else facteurs[[f]]$nom,
               niveau = if (is.null(f)) NA_character_ else levels(donnees[[f]]), stringsAsFactors = FALSE)
  })
  l <- do.call(rbind, lignes)
  courbes <- data.frame(terme = l$terme, edf = st[l$etiquette, 1], ref_ddl = st[l$etiquette, 2], statistique = st[l$etiquette, 3], p = st[l$etiquette, 4],
                        row.names = NULL, stringsAsFactors = FALSE)
  courbes <- concurvite_courbes(a, l$etiquette, courbes)
  if (any(lis %in% names(par))) {
    courbes$par <- l$par
    courbes$niveau <- l$niveau
  }
  courbes
}

# La dimension de base d'une courbe : `k` si elle est demandée, sinon 10 au plus ; moins quand la
# variable a peu de valeurs.
dimension_courbe <- function(x, k = NA) min(if (length(k) && !is.na(k[1])) as.integer(k[1]) else 10L, length(unique(x)) - 1L)

# Les prédictions d'un modèle de mgcv sur l'échelle de la réponse, par ses effets fixes (ses
# effets aléatoires exclus, ordonnées à l'origine et pentes : un groupe jamais vu ; on donne aux
# observations un groupe connu du modèle, que l'exclusion annule). Le facteur d'une courbe par
# catégorie (`par`) prend les niveaux du modèle (niveaux_appris).
predire_mgcv <- function(a, nd, appris, niveaux, par = NULL) {
  noms <- c("g1", "g12", "g123")[seq_len(niveaux)]
  for (g in noms) nd[[g]] <- factor(rep(levels(appris[[g]])[1], nrow(nd)), levels = levels(appris[[g]]))
  for (f in unique(par)) nd[[f]] <- niveaux_appris(nd[[f]], levels(appris[[f]]))
  aleatoires <- vapply(Filter(function(s) inherits(s, "random.effect"), a$smooth), function(s) s$label, "")
  as.vector(stats::predict(a, nd, type = "response", exclude = aleatoires))
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

# `par` : la colonne du facteur de chaque courbe par catégorie ; `facteurs` : facteurs_courbes.
resultat_mgcv <- function(donnees, termes, noms, lisses, niveaux, famille, decalage, k_demandes = NULL, pentes = NULL, par = NULL, facteurs = NULL) {
  utiles <- termes_utiles(donnees, termes)
  verifier_pentes_utiles(pentes, utiles, termes, noms)
  retires <- noms[match(setdiff(termes, utiles), termes)]
  lis <- intersect(lisses, utiles)
  k <- vapply(lis, function(t) dimension_courbe(donnees[[t]], if (t %in% names(k_demandes)) k_demandes[[t]] else NA), 0L)
  if (any(k < 3)) {
    stop(sprintf(traduire("Trop peu de valeurs différentes pour une courbe : %s.", "Too few different values for a curve: %s."),
                 paste(noms[match(lis[k < 3], termes)], collapse = ", ")), call. = FALSE)
  }
  parametriques <- setdiff(utiles, lis)
  # (sans courbe ni effet aléatoire, un modèle linéaire généralisé ordinaire : en ML, comme glm()
  # et MASS::glm.nb(), le θ de la binomiale négative compris)
  penalise <- length(lis) > 0 || niveaux > 0
  estime <- ajuster_mgcv(donnees, parametriques, lis, k, niveaux, famille, decalage, if (penalise) "REML" else "ML", pentes, par)
  compare <- if (penalise) ajuster_mgcv(donnees, parametriques, lis, k, niveaux, famille, decalage, "ML", pentes, par) else estime
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
  courbes <- if (length(lis)) tableau_courbes(estime, lis, termes, noms, donnees, par, facteurs)
  variances <- singularites <- NULL
  if (niveaux) {
    vc <- tryCatch(mgcv::gam.vcomp(estime), error = function(e) NULL)
    et <- if (is.matrix(vc)) vc[, 1] else if (is.list(vc) && is.matrix(vc$vc)) vc$vc[, 1] else NULL  # (les écarts-types, nommés)
    if (!is.null(et)) {
      # (une matrice diagonale par niveau, son ordonnée à l'origine s(g1) puis ses pentes s(g1,t3) :
      # mgcv les tient pour indépendantes)
      groupes <- c("g1", "g12", "g123")[seq_len(niveaux)]
      S <- lapply(seq_len(niveaux), function(l) {
        termes_l <- c("(Intercept)", pentes[[l]])
        v <- unname(et[c(sprintf("s(%s)", groupes[l]), sprintf("s(%s,%s)", groupes[l], pentes[[l]]))])^2
        matrix(diag(v, length(v)), length(v), dimnames = list(termes_l, termes_l))
      })
      variances <- tableau_variances(S, NULL, pentes, independantes = TRUE)
      singularites <- if (length(unlist(pentes))) singularites_pentes(S, donnees, pentes, sqrt(estime$sig2), independantes = TRUE)
    }
  }
  ll <- stats::logLik(compare)
  n <- nrow(donnees)
  list(N = n, coefficients = co, courbes = courbes, termes_retires = retires, logLik = as.numeric(ll), parametres = attr(ll, "df"),
       AIC = stats::AIC(compare), AICc = aic_corrige(compare, n), BIC = stats::BIC(compare),
       r2 = list(r2_ajuste = s$r.sq, deviance_expliquee = s$dev.expl), variances = variances, singularites = singularites,
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
# observations y devient un terme linéaire. `par` : les courbes par catégorie (formule_mgcv).
validation_mgcv <- function(donnees, termes, lisses, k, groupe, niveaux, famille, decalage, pentes = NULL, par = NULL) {
  prediction <- rep(NA_real_, nrow(donnees))
  echecs <- 0
  for (g in sort(unique(groupe))) {
    ici <- groupe == g
    appris <- droplevels(donnees[!ici, , drop = FALSE])
    u <- termes_utiles(appris, termes)
    lis <- intersect(lisses, u)
    kk <- vapply(lis, function(t) dimension_courbe(appris[[t]], k[[t]]), 0L)  # (la dimension du modèle, moins si ces lignes ont peu de valeurs)
    lis <- lis[kk >= 3]
    m <- ajuster_mgcv(appris, setdiff(u, lis), lis, kk, niveaux, famille, decalage, if (length(lis) || niveaux) "REML" else "ML", pentes, par)
    if (is.null(m)) { echecs <- echecs + 1; next }
    prediction[ici] <- tryCatch(predire_mgcv(m, donnees[ici, , drop = FALSE], appris, niveaux, par[intersect(names(par), lis)]), error = function(e) NA_real_)
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
# emboîtés, comme nlme lit une liste ; g1 = ~ t3 avec une pente aléatoire, aleatoire_nlme) et la
# corrélation (correlation_car1) vont à nlme::lme ; la formule est celle de mgcv sans effet
# aléatoire (formule_mgcv), son environnement donne s(). NULL quand l'ajustement échoue, après un
# second essai par optim.
ajuster_gamm <- function(donnees, parametriques, lisses, k, niveaux, methode = "REML", pentes = NULL, par = NULL) {
  f <- formule_mgcv(parametriques, lisses, k, 0, FALSE, NULL, par)
  aleatoire <- aleatoire_nlme(niveaux, pentes, liste = TRUE)
  correlation <- correlation_car1(donnees, niveaux)
  essai <- function(...) tryCatch(mgcv::gamm(f, random = aleatoire, correlation = correlation, data = donnees, method = methode, ...), error = function(e) NULL)
  essai() %||% essai(control = nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE))
}

# Le modèle ajusté sur toutes ses observations, comme resultat_mgcv : REML pour les coefficients,
# les courbes, les variances et l'autocorrélation ; la vraisemblance, l'AIC, l'AICc et le BIC de
# la partie nlme d'un ajustement en ML. R² marginal et conditionnel (Nakagawa et Schielzeth,
# 2013 ; avec des pentes aléatoires, Johnson 2014), la partie fixe comprenant les courbes. Valeurs
# ajustées : avec les effets aléatoires ($lme) et par les seuls effets fixes ($gam) ; résidus
# normalisés (l'autocorrélation retirée).
resultat_gamm <- function(donnees, termes, noms, lisses, niveaux, k_demandes = NULL, pentes = NULL, par = NULL, facteurs = NULL) {
  verifier_temps_uniques(donnees, niveaux)
  utiles <- termes_utiles(donnees, termes)
  verifier_pentes_utiles(pentes, utiles, termes, noms)
  retires <- noms[match(setdiff(termes, utiles), termes)]
  lis <- intersect(lisses, utiles)
  k <- vapply(lis, function(t) dimension_courbe(donnees[[t]], if (t %in% names(k_demandes)) k_demandes[[t]] else NA), 0L)
  if (any(k < 3)) {
    stop(sprintf(traduire("Trop peu de valeurs différentes pour une courbe : %s.", "Too few different values for a curve: %s."),
                 paste(noms[match(lis[k < 3], termes)], collapse = ", ")), call. = FALSE)
  }
  parametriques <- setdiff(utiles, lis)
  estime <- ajuster_gamm(donnees, parametriques, lis, k, niveaux, "REML", pentes, par)
  compare <- ajuster_gamm(donnees, parametriques, lis, k, niveaux, "ML", pentes, par)
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
  courbes <- if (length(lis)) tableau_courbes(g, lis, termes, noms, donnees, par, facteurs)
  # la variance de chaque effet aléatoire (nlme : σ² fois sa matrice relative), puis la résiduelle
  S <- matrices_nlme(l, niveaux)
  variances <- tableau_variances(S, l$sigma^2, pentes)
  v <- variances_niveaux(S, donnees, pentes)
  fixes <- stats::var(as.vector(stats::fitted(g)))
  totale <- fixes + sum(v) + l$sigma^2
  ll <- stats::logLik(compare$lme)
  n <- nrow(donnees)
  phi <- unname(stats::coef(l$modelStruct$corStruct, unconstrained = FALSE))
  avec_pentes <- length(unlist(pentes)) > 0
  list(N = n, coefficients = co, courbes = courbes, termes_retires = retires, logLik = as.numeric(ll), parametres = attr(ll, "df"),
       AIC = stats::AIC(compare$lme), AICc = aic_corrige(compare$lme, n), BIC = stats::BIC(compare$lme),
       r2 = list(marginal = fixes / totale, conditionnel = (fixes + sum(v)) / totale), variances = variances,
       parts = if (avec_pentes) parts_variance(S, donnees, pentes, l$sigma^2),
       singularites = if (avec_pentes) singularites_pentes(S, donnees, pentes, l$sigma),
       phi = phi, famille = "gaussienne", lien = "identity", echelle = l$sigma^2,
       ajuste = as.vector(stats::fitted(l)), ajuste_fixe = as.vector(stats::fitted(g)),
       # (dans l'ordre du temps de chaque groupe : regression_diagnostics.R, residus_normalises_car1)
       residus = residus_normalises_car1(as.vector(stats::residuals(l, type = "pearson")), donnees$temps, groupe_car1(donnees, niveaux), phi),
       utiles = utiles, k = k, ajustement = estime, ajustement_ml = compare)
}

# Validation croisée d'un modèle de mgcv::gamm : chaque groupe prédit par le modèle ajusté sans
# lui (REML), par ses effets fixes et ses courbes ($gam : un groupe jamais vu) ; le facteur d'une
# courbe par catégorie (`par`) aux niveaux du modèle (niveaux_appris).
validation_gamm <- function(donnees, termes, lisses, k, groupe, niveaux, pentes = NULL, par = NULL) {
  prediction <- rep(NA_real_, nrow(donnees))
  echecs <- 0
  for (g in sort(unique(groupe))) {
    ici <- groupe == g
    appris <- droplevels(donnees[!ici, , drop = FALSE])
    u <- termes_utiles(appris, termes)
    lis <- intersect(lisses, u)
    kk <- vapply(lis, function(t) dimension_courbe(appris[[t]], k[[t]]), 0L)
    lis <- lis[kk >= 3]
    m <- ajuster_gamm(appris, setdiff(u, lis), lis, kk, niveaux, "REML", pentes, par)
    if (is.null(m)) { echecs <- echecs + 1; next }
    nd <- donnees[ici, , drop = FALSE]
    for (f in unique(par[intersect(names(par), lis)])) nd[[f]] <- niveaux_appris(nd[[f]], levels(appris[[f]]))
    prediction[ici] <- tryCatch(as.vector(stats::predict(m$gam, nd, type = "response")), error = function(e) NA_real_)
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
# une droite. Les abscisses sont dans les unités de la variable (transformée s'il y a lieu). Un
# terme quantitatif croisé avec une ou plusieurs variables qualitatives (annee × secteur), ou qui
# n'entre que par un tel terme croisé (sans effet principal), a une droite (ou une courbe) par
# catégorie, dans `niveaux` : effets_par_niveau ; une courbe par catégorie (s(x, by = f)), la courbe
# de chaque catégorie : effets_par_courbe. Un terme croisé avec une autre variable quantitative
# garde sa droite seule : le coefficient de son effet principal, sa pente là où l'autre vaut 0.
effets_regression <- function(m) {
  a <- m$ajustement
  if (inherits(a, "gamm")) a <- a$gam  # (mgcv::gamm : ses courbes, dans sa partie gam)
  d <- m$donnees
  co <- m$resultat$coefficients
  croises <- pentes_croisees(m)
  termes <- c(intersect(m$quantitatifs, m$utiles), setdiff(names(croises), m$quantitatifs))
  lapply(termes, function(t) {
    i <- match(t, m$termes)
    centre <- m$bloc$centre[i]
    poids <- m$bloc$poids[i]
    if (!isTRUE(poids > 0)) poids <- 1
    if (t %in% names(m$par)) return(effets_par_courbe(m, a, t, centre, poids))
    if (!is.null(croises[[t]])) return(effets_par_niveau(m, a, t, croises[[t]], centre, poids))
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

# Les termes croisés (utiles) d'une variable quantitative, une fois, avec des catégories seules
# (annee*secteur(S1), annee*secteur(S1)*habitat(H2)...), par terme simple de cette variable (son
# effet principal, ou son facteur seul) : list(t5 = c("t9", "t10")...). La pente de la variable
# y change d'une catégorie à l'autre.
pentes_croisees <- function(m) {
  comp <- m$composition
  simples <- Filter(function(t) length(comp[[t]]$variables) == 1 && comp[[t]]$categories == 0, names(comp))
  res <- lapply(stats::setNames(simples, simples), function(t) {
    u <- comp[[t]]$variables
    Filter(function(p) {
      ct <- comp[[p]]
      length(ct$variables) > 1 && sum(ct$variables == u) == 1 && ct$categories[ct$variables == u] == 0 && all(ct$categories[ct$variables != u] != 0)
    }, m$utiles)
  })
  res[lengths(res) > 0]
}

# Les coefficients d'un ajustement et leur matrice de covariance (nlme : ses effets fixes).
coefficients_covariance <- function(a) {
  if (inherits(a, "lme")) return(list(b = nlme::fixef(a), V = a$varFix))
  list(b = stats::coef(a), V = stats::vcov(a))
}

# L'effet du terme quantitatif `t` dans chaque catégorie (ou combinaison de catégories) des
# variables qualitatives de ses termes croisés `produits` (pentes_croisees), là où le modèle a des
# observations : sa pente dans la catégorie de référence (le coefficient de son effet principal,
# 0 sans lui ; une courbe pour un GAM), plus le coefficient de son produit avec cette catégorie fois
# le poids de ce produit (sa dérivée), avec une bande à 95 % tirée de la matrice de covariance des
# coefficients ; centré, comme une droite seule, sur la moyenne de la variable dans le modèle, et
# tracé sur l'étendue de ses valeurs dans la catégorie. Une droite donne aussi sa pente par unité
# de la variable, avec son intervalle à 95 %. `a` : l'ajustement (sa partie gam pour mgcv::gamm).
effets_par_niveau <- function(m, a, t, produits, centre, poids) {
  d <- m$donnees
  cb <- coefficients_covariance(a)
  courbe <- t %in% m$lisses
  cellules <- cellules_pentes(m, produits)
  moyenne <- mean(d[[t]])
  niveaux <- lapply(cellules, function(cel) {
    ici <- cel$lignes
    dans <- cel$dans
    x <- seq(min(d[[t]][ici]), max(d[[t]][ici]), length.out = 80)
    # l'effet en chaque point de x : L %*% coefficients
    L <- matrix(0, length(x), length(cb$b), dimnames = list(NULL, names(cb$b)))
    if (courbe) {  # (la courbe : ses colonnes de base, mgcv)
      nd <- d[rep(1, length(x)), , drop = FALSE]
      nd[[t]] <- x
      Xp <- stats::predict(a, nd, type = "lpmatrix")
      js <- grep(sprintf("^s\\(%s\\)\\.", t), colnames(Xp))
      L[, colnames(Xp)[js]] <- Xp[, js]
    } else if (t %in% names(cb$b)) L[, t] <- x - moyenne
    for (p in intersect(dans, names(cb$b))) L[, p] <- (x - moyenne) * m$bloc$poids[match(p, m$termes)]
    effet <- as.vector(L %*% cb$b)
    se <- sqrt(pmax(0, rowSums((L %*% cb$V) * L)))
    pente <- NULL
    if (!courbe) {  # (la pente, par unité de la variable : la dérivée de l'effet, le poids de la variable compris)
      l1 <- stats::setNames(rep(0, length(cb$b)), names(cb$b))
      if (t %in% names(cb$b)) l1[t] <- 1
      for (p in intersect(dans, names(cb$b))) l1[p] <- m$bloc$poids[match(p, m$termes)]
      s <- sum(l1 * cb$b)
      e <- sqrt(max(0, sum(l1 * (cb$V %*% l1))))
      pente <- list(pente = s * poids, pente_bas = (s - 1.96 * e) * poids, pente_haut = (s + 1.96 * e) * poids)
    }
    c(list(niveau = cel$niveau, x = x / poids + centre,
           effet = effet, bas = effet - 1.96 * se, haut = effet + 1.96 * se, valeurs = d[[t]][ici] / poids + centre), pente)
  })
  list(terme = m$noms_termes[match(t, m$termes)], courbe = courbe, par = attr(cellules, "par"), valeurs = d[[t]] / poids + centre, niveaux = niveaux)
}

# Les catégories (ou combinaisons de catégories) des variables qualitatives des termes croisés
# `produits` d'un terme quantitatif (pentes_croisees), là où le modèle a des observations, dans
# l'ordre de leurs catégories : chacune, son nom (« S1 », « S1 · H2 »), ses lignes dans m$donnees
# (`lignes`) et les colonnes 0/1 de ses catégories qui les font (`colonnes`), et ses produits
# (`dans` : ceux dont toutes les catégories sont les siennes) ; en attribut `par`, les noms de ces
# variables qualitatives (« secteur × habitat »).
cellules_pentes <- function(m, produits) {
  d <- m$donnees
  comp <- m$composition
  # la catégorie de chaque ligne pour chaque variable qualitative croisée (lue dans ses colonnes 0/1)
  qual <- unique(unlist(lapply(produits, function(p) comp[[p]]$variables[comp[[p]]$categories != 0])))
  colonnes <- lapply(qual, function(u) Filter(function(s) length(comp[[s]]$variables) == 1 && comp[[s]]$variables == u && comp[[s]]$categories != 0, names(comp)))
  lignes <- matrix(vapply(colonnes, function(ss) {
    r <- rep(NA_real_, nrow(d))
    for (s in ss) r[!is.na(d[[s]]) & d[[s]] == 1] <- comp[[s]]$categories
    r
  }, numeric(nrow(d))), nrow(d))
  connues <- stats::complete.cases(lignes)
  cellules <- unique(lignes[connues, , drop = FALSE])
  cellules <- cellules[do.call(order, unname(as.data.frame(cellules))), , drop = FALSE]
  colonne <- function(j, k) Find(function(s) comp[[s]]$categories == k, colonnes[[j]])
  structure(lapply(seq_len(nrow(cellules)), function(r) {
    cellule <- cellules[r, ]
    list(niveau = paste(vapply(seq_along(qual), function(j) comp[[colonne(j, cellule[j])]]$niveaux, ""), collapse = " · "),
         lignes = connues & apply(lignes, 1, function(l) all(l == cellule)), colonnes = vapply(seq_along(qual), function(j) colonne(j, cellule[j]), ""),
         dans = Filter(function(p) {
           ct <- comp[[p]]
           all(vapply(which(ct$categories != 0), function(j) cellule[match(ct$variables[j], qual)] == ct$categories[j], TRUE))
         }, produits))
  }), par = paste(vapply(colonnes, function(ss) comp[[ss[1]]]$noms, ""), collapse = " × "))
}

# L'effet d'une courbe par catégorie (s(x, by = f)) : la courbe de chaque niveau de son facteur, telle
# que mgcv l'a ajustée (centrée sur les observations de ce niveau), avec sa bande à 95 %, tracée sur
# l'étendue des valeurs de la variable dans ce niveau ; dans la forme d'effets_par_niveau. L'écart
# d'un niveau à l'autre est l'effet principal de la variable qualitative, dans les coefficients.
effets_par_courbe <- function(m, a, t, centre, poids) {
  d <- m$donnees
  f <- m$par[[t]]
  niveaux <- lapply(levels(d[[f]]), function(nv) {
    ici <- !is.na(d[[f]]) & d[[f]] == nv
    x <- seq(min(d[[t]][ici]), max(d[[t]][ici]), length.out = 80)
    nd <- d[rep(which(ici)[1], length(x)), , drop = FALSE]
    nd[[t]] <- x
    etiquette <- sprintf("s(%s):%s%s", t, f, nv)
    p <- stats::predict(a, nd, type = "terms", terms = etiquette, se.fit = TRUE)
    effet <- as.vector(p$fit[, etiquette])
    se <- as.vector(p$se.fit[, etiquette])
    list(niveau = nv, x = x / poids + centre, effet = effet, bas = effet - 1.96 * se, haut = effet + 1.96 * se, valeurs = d[[t]][ici] / poids + centre)
  })
  list(terme = m$noms_termes[match(t, m$termes)], courbe = TRUE, par_categorie = TRUE, par = m$facteurs_courbes[[f]]$nom, valeurs = d[[t]] / poids + centre, niveaux = niveaux)
}

# Les seuils d'une courbe (GAM) : son effet et sa pente sur une grille de 200 valeurs entre les
# percentiles 2 et 98 de sa variable, avec des intervalles simultanés à 95 % (ils couvrent toute
# la courbe, pas un point), tirés de 2000 jeux de coefficients (MASS::mvrnorm sur vcov du modèle)
# avec une graine fixe ; les plages où la pente est nettement positive ou négative (sa bande
# simultanée exclut 0), et celles où l'effet est nettement au-dessus ou au-dessous de sa moyenne.
# Une courbe d'edf < 1,5 est presque une droite. C'est la démarche de Simpson (2018), celle des
# scripts de seuils qui ont servi de modèle. `terme` : le nom du terme (noms_termes). Une courbe qui
# a une courbe par catégorie (courbes_seuils) : celle de la catégorie `categorie` (la première sans
# elle), sur les percentiles de sa variable dans cette catégorie ; son edf et son p, ceux de sa courbe.
seuils_courbe <- function(m, terme, categorie = NULL, tirages = 2000, niveau = 0.95, graine = 1) {
  a <- m$ajustement
  if (inherits(a, "gamm")) a <- a$gam
  if (!inherits(a, "gam")) stop(traduire("Les seuils se lisent sur une courbe d'un modèle additif.", "Thresholds are read on a curve of an additive model."), call. = FALSE)
  i <- match(as.character(terme)[1], m$noms_termes)
  t <- if (!is.na(i)) m$termes[i]
  if (is.null(t) || !t %in% m$lisses) stop(sprintf(traduire("« %s » n'est pas une courbe de ce modèle.", "\"%s\" is not a curve of this model."), terme), call. = FALSE)
  courbes <- courbes_seuils(m, t)
  k <- if (length(categorie) && !is.na(categorie[1]) && nzchar(as.character(categorie)[1])) match(as.character(categorie)[1], vapply(courbes, `[[`, "", "categorie")) else 1L
  if (is.na(k)) {
    stop(sprintf(traduire("« %s » n'est pas une catégorie de la courbe de « %s ».", "\"%s\" is not a category of the curve of \"%s\"."), categorie[1], terme), call. = FALSE)
  }
  cc <- courbes[[k]]
  d <- m$donnees
  grille <- seq(stats::quantile(d[[t]][cc$lignes], 0.02), stats::quantile(d[[t]][cc$lignes], 0.98), length.out = 200)
  nd <- d[rep(which(cc$lignes)[1], length(grille)), , drop = FALSE]
  nd[[t]] <- grille
  Xp <- stats::predict(a, nd, type = "lpmatrix")
  js <- colonnes_courbe(a, cc$etiquette)
  L <- Xp[, js, drop = FALSE]
  # (la courbe d'une catégorie de termes croisés : la pente de chacun de ses produits, son poids fois la variable)
  for (p in intersect(cc$produits, names(stats::coef(a)))) {
    L <- cbind(L, grille * m$bloc$poids[match(p, m$termes)])
    js <- c(js, match(p, names(stats::coef(a))))
  }
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
  ajuste <- as.vector(L %*% stats::coef(a)[js])
  simules <- L %*% t(B[, js, drop = FALSE])
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
  ligne <- match(cc$etiquette, rownames(st))
  edf <- unname(st[ligne, 1])
  c(list(terme = m$noms_termes[i]), if (!is.na(cc$categorie)) list(categorie = cc$categorie, par = cc$par),
    list(edf = edf, p = unname(st[ligne, 4]), lineaire = edf < 1.5, amplitude = diff(range(ajuste)),
         x = x, effet = ajuste, bas = be$bas, haut = be$haut, x_pente = milieux, pente = pente, pente_bas = bp$bas, pente_haut = bp$haut,
         sens = sens, plages_pente = plages(sens, milieux), plages_effet = plages(position, x),
         critique_effet = be$critique, critique_pente = bp$critique, tirages = tirages, niveau = niveau, valeurs = d[[t]][cc$lignes] / poids + centre))
}

# Les courbes du terme en courbe `t` d'une régression dont on lit les seuils : sa courbe seule, ou
# une par catégorie : celle de chaque niveau d'une courbe par catégorie (s(x, by = f)), ou la courbe
# commune plus la pente de chaque catégorie de ses termes croisés (pentes_croisees, comme
# effets_par_niveau). Chacune : sa catégorie (`categorie`, NA pour la courbe seule) et sa variable
# qualitative (`par`), ses lignes dans m$donnees et les colonnes 0/1 qui les font, l'étiquette de
# sa courbe dans mgcv et ses produits (les termes croisés de sa catégorie).
courbes_seuils <- function(m, t) {
  d <- m$donnees
  if (t %in% names(m$par)) {
    f <- m$par[[t]]
    return(lapply(levels(d[[f]]), function(nv) list(categorie = nv, par = m$facteurs_courbes[[f]]$nom, lignes = !is.na(d[[f]]) & d[[f]] == nv,
                                                     colonnes = unname(m$facteurs_courbes[[f]]$colonnes[nv]), etiquette = sprintf("s(%s):%s%s", t, f, nv),
                                                     produits = character(0))))
  }
  produits <- pentes_croisees(m)[[t]]
  if (is.null(produits)) {
    return(list(list(categorie = NA_character_, par = NA_character_, lignes = rep(TRUE, nrow(d)), colonnes = character(0), etiquette = sprintf("s(%s)", t),
                     produits = character(0))))
  }
  cellules <- cellules_pentes(m, produits)
  lapply(cellules, function(cel) list(categorie = cel$niveau, par = attr(cellules, "par"), lignes = cel$lignes, colonnes = cel$colonnes,
                                      etiquette = sprintf("s(%s)", t), produits = cel$dans))
}

# Les seuils qu'a une régression : chaque courbe et, pour une courbe par catégorie, chacune de ses
# catégories (courbes_seuils), dans l'ordre des courbes ; un tableau (terme, categorie : NA pour une
# courbe seule).
variantes_seuils <- function(m) {
  lignes <- lapply(m$lisses, function(t) {
    cs <- courbes_seuils(m, t)
    data.frame(terme = rep(m$noms_termes[match(t, m$termes)], length(cs)), categorie = vapply(cs, `[[`, "", "categorie"), stringsAsFactors = FALSE)
  })
  if (length(lignes)) do.call(rbind, lignes) else data.frame(terme = character(0), categorie = character(0), stringsAsFactors = FALSE)
}

# Le texte d'une section « Méthodes » pour une régression, de la même forme que celui des
# autres modèles (methodes.R : texte_methodes) : données, modèle, validation croisée,
# qualité, logiciel ; et les références.
REFERENCES_REGRESSION <- list(
  pinheiro = "Pinheiro, J. C., & Bates, D. M. (2000). Mixed-effects models in S and S-PLUS. Springer. https://doi.org/10.1007/b98882",
  nlme = "Pinheiro, J., Bates, D., & R Core Team (%s). nlme: Linear and nonlinear mixed effects models (R package version %s). https://CRAN.R-project.org/package=nlme",
  nakagawa = "Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R² from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133–142. https://doi.org/10.1111/j.2041-210x.2012.00261.x",
  johnson = "Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth's R²GLMM to random slopes models. Methods in Ecology and Evolution, 5(9), 944–946. https://doi.org/10.1111/2041-210X.12225",
  burnham = "Burnham, K. P., & Anderson, D. R. (2002). Model selection and multimodel inference: A practical information-theoretic approach (2nd ed.). Springer. https://doi.org/10.1007/b97636",
  zuur = "Zuur, A. F., Ieno, E. N., Walker, N. J., Saveliev, A. A., & Smith, G. M. (2009). Mixed effects models and extensions in ecology with R. Springer. https://doi.org/10.1007/978-0-387-87458-6",
  wood2017 = "Wood, S. N. (2017). Generalized additive models: An introduction with R (2nd ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781315370279",
  wood2011 = "Wood, S. N. (2011). Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models. Journal of the Royal Statistical Society: Series B, 73(1), 3–36. https://doi.org/10.1111/j.1467-9868.2010.00749.x",
  stewart_oaten = "Stewart-Oaten, A., Murdoch, W. W., & Parker, K. R. (1986). Environmental impact assessment: \"Pseudoreplication\" in time? Ecology, 67(4), 929–940. https://doi.org/10.2307/1939815",
  diggle = "Diggle, P. J., Heagerty, P., Liang, K.-Y., & Zeger, S. L. (2002). Analysis of longitudinal data (2nd ed.). Oxford University Press.",
  johnson = "Johnson, P. C. D., Barry, S. J. E., Ferguson, H. M., & Müller, P. (2015). Power analysis for generalized linear mixed models in ecology and evolution. Methods in Ecology and Evolution, 6(2), 133–142. https://doi.org/10.1111/2041-210X.12306",
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
  p1 <- if (fr) sprintf("Le modèle porte sur %s%s ; la réponse est %s.", observations_texte(m$N, d, fr),
                        if (length(m$exclues)) sprintf(" (%d autres laissées de côté faute de valeurs)", length(m$exclues)) else "", reponse)
        else sprintf("The model uses %s%s; the response is %s.", observations_texte(m$N, d, fr),
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
    courbes <- if (!length(lis)) "" else if (!length(m$par)) {
      sprintf(if (fr) ", dont %s par des courbes (splines de régression en plaques minces)" else ", %s of them through curves (thin plate regression splines)", liste(lis))
    } else {
      # (une courbe par catégorie d'une variable qualitative : s(x, by = f), chacune centrée)
      seules <- m$noms_termes[match(setdiff(m$lisses, names(m$par)), m$termes)]
      parties <- c(if (length(seules)) sprintf(if (fr) "%s par %s" else "%s through %s", liste(seules),
                                                if (fr) (if (length(seules) > 1) "des courbes" else "une courbe") else (if (length(seules) > 1) "curves" else "a curve")),
                   vapply(names(m$par), function(t) sprintf(if (fr) "%s par une courbe pour chaque catégorie de %s" else "%s through a curve for each category of %s",
                                                           m$noms_termes[match(t, m$termes)], guillemets(m$facteurs_courbes[[m$par[[t]]]]$nom)), ""))
      sprintf(if (fr) ", dont %s (splines de régression en plaques minces ; chaque courbe d'une catégorie est centrée sur ses observations, et l'effet principal de sa variable qualitative donne l'écart entre les catégories)"
              else ", with %s (thin plate regression splines; each category's curve is centred on its observations, and its qualitative variable's main effect gives the difference between the categories)", liste(parties))
    }
    aleatoire <- if (gamm) {
      sprintf(if (fr) ", une ordonnée à l'origine aléatoire par %s%s, et une autocorrélation des résidus en temps continu (CAR1) le long de %s à l'intérieur de chaque %s"
              else ", a random intercept for %s%s, and a continuous-time autocorrelation of the residuals (CAR1) along %s within each %s",
              liste(niveaux_emboites(m$niveaux, fr, guillemets)), phrase_pentes(m, fr, liste, guillemets), guillemets(m$temps), guillemets(m$dans %||% m$niveaux[length(m$niveaux)]))
    } else if (length(m$niveaux)) sprintf(if (fr) ", une ordonnée à l'origine aléatoire par %s (des termes pénalisés)%s" else ", a random intercept for %s (penalized terms)%s",
                                          liste(vapply(m$niveaux, guillemets, "")), phrase_pentes(m, fr, liste, guillemets, independantes = TRUE)) else ""
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
    c(cite("pinheiro", if (fr) sprintf("Nous avons ajusté un modèle linéaire mixte (Pinheiro et Bates, 2000) avec %s en effets fixes, une ordonnée à l'origine aléatoire par %s%s%s. %s",
                                       termes, liste(niveaux), phrase_pentes(m, fr, liste, guillemets), car1, unites)
                       else sprintf("We fitted a linear mixed model (Pinheiro & Bates, 2000) with %s as fixed effects, a random intercept for %s%s%s. %s",
                                    termes, liste(niveaux), phrase_pentes(m, fr, liste, guillemets), car1, unites)),
      cite("zuur", if (fr) "Les coefficients et les variances ont été estimés par maximum de vraisemblance restreint (REML), et les modèles comparés par l'AIC d'ajustements en maximum de vraisemblance (ML ; Zuur et al., 2009)."
                   else "Coefficients and variances were estimated by restricted maximum likelihood (REML), and models compared by the AIC of maximum-likelihood fits (ML; Zuur et al., 2009)."))
  }
  # (ses variables qualitatives, leur catégorie de référence, et ce que valent les coefficients de leurs termes croisés)
  p2 <- c(p2[1], phrases_qualitatives(m, fr, liste, guillemets), p2[-1])
  # (la sélection par l'AIC, quand elle a été faite sur ce modèle ; un modèle qu'elle a choisi le dit)
  if (isTRUE(m$selection_aic$K > 0)) p2 <- c(p2, cite("burnham", phrase_selection_aic(m, fr, nombre, liste, guillemets)))
  if (!is.null(d$selection_origine)) {
    o <- d$selection_origine
    groupes <- as.character(unlist(o$groupes))
    # (les mêmes observations tant qu'aucune n'a été exclue depuis : ses lignes sont celles du modèle complet)
    memes <- isTRUE(as.numeric(o$n)[1] == length(m$observations))
    # (les groupes qui portaient une pente aléatoire étaient dans chaque modèle de la sélection)
    gardes <- as.character(unlist(o$gardes))
    p2 <- c(p2, cite("burnham", sprintf(if (fr) "Ses termes sont ceux que la sélection par l'AICc du modèle %s a retenus (%s)%s%s (Burnham et Anderson, 2002)."
                                        else "Its terms are those the AICc selection of model %s kept (%s)%s%s (Burnham & Anderson, 2002).",
                                        guillemets(as.character(o$modele)[1]),
                                        if (length(groupes)) liste(groupes) else if (length(gardes)) (if (fr) "aucun" else "none") else (if (fr) "aucun : le modèle nul" else "none: the null model"),
                                        if (!length(gardes)) "" else sprintf(if (fr) ", avec %s, gardé dans chaque modèle pour sa pente aléatoire" else ", with %s, kept in every model for its random slope", liste(guillemets(gardes))),
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
  # (les observations d'une catégorie que les autres groupes n'avaient pas : sans prédiction, hors de Q²)
  inconnues <- m$vc$sans_categorie %||% 0
  facteurs <- liste(guillemets(as.character(m$vc$facteurs)))
  sans_q2 <- is.na(m$vc$Q2)
  if (inconnues && sans_q2) {
    p3 <- paste(p3, sprintf(if (fr) "Aucune observation n'a eu de prédiction : la catégorie de chacune (%s) manquait aux autres groupes, et un modèle ne prédit pas une catégorie qu'il n'a pas vue ; Q² n'a pas pu être calculé."
                            else "No observation got a prediction: the category of each (%s) was missing from the other groups, and a model doesn't predict a category it hasn't seen; Q² couldn't be computed.", facteurs))
  } else if (inconnues) {
    p3 <- paste(p3, if (fr) sprintf("%d observation%s, dont la catégorie (%s) manquait aux autres groupes, %s sans prédiction, hors de Q² et de RMSEcv : un modèle ne prédit pas une catégorie qu'il n'a pas vue.",
                                    inconnues, if (inconnues > 1) "s" else "", facteurs, if (inconnues > 1) "sont restées" else "est restée")
                    else sprintf("%d observation%s, whose category (%s) was missing from the other groups, stayed without a prediction, out of Q² and RMSEcv: a model doesn't predict a category it hasn't seen.",
                                 inconnues, if (inconnues > 1) "s" else "", facteurs))
  }

  # --- qualité
  aic <- cite("burnham", sprintf("AIC = %s (Burnham %s Anderson, 2002)", nombre(r$AIC, 1), if (fr) "et" else "&"))
  vc <- if (sans_q2) (if (fr) "la validation croisée n'a pas donné de Q²" else "cross-validation gave no Q²")
        else sprintf(if (fr) "en validation croisée, Q² = %s et RMSEcv = %s" else "in cross-validation, Q² = %s and RMSEcv = %s", nombre(m$vc$Q2), nombre(m$vc$RMSEcv, 3))
  loi <- r$parametre_loi
  p4 <- if (generalise && is.null(r$r2$marginal)) {
    sprintf(if (fr) "Le modèle explique %s de la déviance (R² ajusté = %s)%s ; %s ; %s." else "The model explains %s of the deviance (adjusted R² = %s)%s; %s; %s.",
            pourcent(r$r2$deviance_expliquee), nombre(r$r2$r2_ajuste), if (!is.null(loi)) sprintf(", %s = %s", if (loi$nom == "theta") "θ" else "p", nombre(loi$valeur)) else "",
            aic, vc)
  } else if (!is.null(r$r2$marginal)) {
    # (avec des pentes aléatoires, la variance des effets aléatoires de Johnson, 2014 : variances_niveaux)
    johnson <- if (length(unlist(m$pentes))) cite("johnson", if (fr) ", étendu aux pentes aléatoires par Johnson, 2014" else ", extended to random slopes by Johnson, 2014") else ""
    cite("nakagawa", if (fr) sprintf("Les effets fixes expliquent %s de la variance de la réponse (R² marginal), les effets fixes et aléatoires %s (R² conditionnel ; Nakagawa et Schielzeth, 2013%s) ; %s ; %s.",
                                     pourcent(r$r2$marginal), pourcent(r$r2$conditionnel), johnson, aic, vc)
                     else sprintf("The fixed effects explain %s of the response's variance (marginal R²), the fixed and random effects %s (conditional R²; Nakagawa & Schielzeth, 2013%s); %s; %s.",
                                  pourcent(r$r2$marginal), pourcent(r$r2$conditionnel), johnson, aic, vc))
  } else if (fr) sprintf("Le modèle explique %s de la variance de la réponse (R² ajusté = %s) ; %s ; %s.",
                         pourcent(r$r2$r2), nombre(r$r2$r2_ajuste), aic, vc)
  else sprintf("The model explains %s of the response's variance (adjusted R² = %s); %s; %s.",
               pourcent(r$r2$r2), nombre(r$r2$r2_ajuste), aic, vc)
  if (!is.null(m$test)) {
    st <- m$test$statistiques
    # (ses observations d'une catégorie absente de celles du modèle : sans prédiction, hors des statistiques)
    ti <- m$test$inconnues %||% 0
    ft <- liste(guillemets(as.character(unlist(m$test$facteurs_inconnus))))
    p4 <- c(p4, if (ti && !st$n) {
      sprintf(if (fr) "Un jeu de test de %d observations, laissées hors du modèle et de la validation croisée, n'a pas pu être prédit : la catégorie de chacune (%s) était absente des observations du modèle."
              else "A test set of %d observations, left out of the model and of the cross-validation, couldn't be predicted: the category of each (%s) was absent from the model's observations.", m$test$n, ft)
    } else {
      paste0(if (fr) sprintf("Un jeu de test de %d observations, laissées hors du modèle et de la validation croisée, a ensuite été prédit : RMSEP = %s, biais = %s, Q² externe = %s",
                             m$test$n, nombre(st$RMSEP, 3), nombre(st$biais, 3), nombre(st$Q2ext))
             else sprintf("A test set of %d observations, left out of the model and of the cross-validation, was then predicted: RMSEP = %s, bias = %s, external Q² = %s",
                          m$test$n, nombre(st$RMSEP, 3), nombre(st$biais, 3), nombre(st$Q2ext)),
             if (!ti) "." else sprintf(if (fr) " ; %d d'entre elles, d'une catégorie (%s) absente des observations du modèle, sont restées sans prédiction, hors de ces statistiques."
                                       else "; %d of them, of a category (%s) absent from the model's observations, stayed without a prediction, out of these statistics.", ti, ft))
    })
  }
  # (la puissance pour détecter une tendance, quand elle a été calculée sur ce modèle : puissance.R)
  p_puissance <- if (!is.null(m$puissance_tendance)) {
    pp <- phrase_methodes_puissance(m, fr, nombre, liste, guillemets)
    cites <- union(cites, pp$cites)
    pp$texte
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
  list(paragraphes = c(p1, paste(p2, collapse = " "), p3, paste(p4, collapse = " "), p_puissance, p5), references = unname(sort(references)))
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
  # (ce qu'un groupe demande : les effets principaux d'un terme croisé ; celui de la variable
  # qualitative d'une courbe par catégorie)
  croises <- any(lengths(groupes_requis(groupes_modele(m)[noms], m$composition)) > 0)
  par_cat <- length(m$par) > 0 && any(lengths(s$requis) > 0)
  qui <- c(if (croises) (if (fr) "un terme croisé" else "a cross term"), if (par_cat) (if (fr) "une courbe par catégorie" else "a curve per category"))
  qui <- paste(qui, collapse = if (fr) " ou " else " or ")
  comparaison <- if (toutes) {
    sprintf(if (fr) "Les effets fixes ont ensuite été choisis par l'AICc (Burnham et Anderson, 2002) : les %d combinaisons de leurs %d groupes de termes (%s), du modèle nul au modèle complet%s, ont été ajustées par maximum de vraisemblance (ML) sur les mêmes %d observations%s%s."
            else "The fixed effects were then selected by AICc (Burnham & Anderson, 2002): the %d combinations of their %d groups of terms (%s), from the null model to the full model%s, were fitted by maximum likelihood (ML) on the same %d observations%s%s.",
            nrow(mo), s$K, liste(guillemets(noms)), if (!nzchar(qui)) "" else if (fr) sprintf(", %s n'y entrant qu'avec ses effets principaux", qui) else sprintf(", %s entering only with its main effects", qui),
            s$N, avec, echecs)
  } else {
    sprintf(if (fr) "Les effets fixes ont ensuite été choisis par élimination descendante sur l'AICc (Burnham et Anderson, 2002) : leurs %d groupes de termes donnant plus de %d combinaisons, chaque pas a retiré, depuis le modèle complet, le groupe dont le retrait baissait le plus l'AICc%s, jusqu'à ce qu'aucun retrait ne la baisse ; les %d modèles essayés ont été ajustés par maximum de vraisemblance (ML) sur les mêmes %d observations%s%s."
            else "The fixed effects were then selected by backward elimination on AICc (Burnham & Anderson, 2002): their %d groups of terms giving more than %d combinations, each step removed, from the full model on, the group whose removal lowered AICc the most%s, until no removal lowered it; the %d models tried were fitted by maximum likelihood (ML) on the same %d observations%s%s.",
            s$K, as.integer(s$limite), if (!nzchar(qui)) "" else if (fr) sprintf(" (un effet principal restant tant qu'%s le demande)", qui) else sprintf(" (a main effect staying while %s needs it)", qui),
            nrow(mo), s$N, avec, echecs)
  }
  # (les groupes qui portent une pente aléatoire : dans chaque modèle, hors des groupes choisis)
  gardes <- as.character(unlist(s$gardes))
  plusieurs <- length(gardes) > 1
  if (length(gardes)) comparaison <- paste(comparaison, sprintf(if (fr) "%s, qui %s dans chaque modèle (une pente aléatoire varie autour de la pente fixe de sa variable)."
                                                                else "%s, which %s in every model (a random slope varies around its variable's fixed slope).",
                                                                liste(guillemets(gardes)),
                                                                if (fr) (if (plusieurs) "portent une pente aléatoire, sont restés" else "porte une pente aléatoire, est resté")
                                                                else (if (plusieurs) "carry a random slope, stayed" else "carries a random slope, stayed")))
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

# La catégorie de référence de chaque variable qualitative du modèle `m` (par son numéro, NA
# quand il n'y en a pas une seule) : la catégorie observée que l'ajustement a laissée de côté dans
# son effet principal ; sans lui, dans ses termes croisés avec une variable quantitative qui a le
# sien (y ~ annee + annee:secteur). Sans l'un ni l'autre, chaque catégorie a son coefficient.
references_qualitatives <- function(m) {
  comp <- m$composition
  qual <- unique(unlist(lapply(comp[m$candidats], function(ct) ct$variables[ct$categories != 0])))
  principal <- function(u, categorie) Filter(function(t) length(comp[[t]]$variables) == 1 && comp[[t]]$variables == u &&
                                                (if (categorie) comp[[t]]$categories != 0 else comp[[t]]$categories == 0), m$candidats)
  # (la catégorie de u des colonnes `ts` laissées de côté parmi celles qu'ont des observations, si elle est seule)
  laissee <- function(u, ts) {
    niveaux <- unique(vapply(Filter(function(t) !t %in% m$utiles && !cellule_vide(m$donnees, comp, t), ts),
                             function(t) comp[[t]]$niveaux[comp[[t]]$variables == u], ""))
    if (length(niveaux) == 1) niveaux else NA_character_
  }
  stats::setNames(lapply(qual, function(u) {
    principaux <- principal(u, TRUE)
    if (length(principaux)) return(laissee(u, principaux))
    pentes <- Filter(function(t) {  # (u croisée avec une variable quantitative qui a son effet principal)
      ct <- comp[[t]]
      length(ct$variables) == 2 && u %in% ct$variables && sum(ct$categories == 0) == 1 && length(principal(ct$variables[ct$categories == 0], FALSE)) > 0
    }, m$candidats)
    if (length(pentes)) laissee(u, pentes) else NA_character_
  }), qual)
}

# Les variables qualitatives d'une régression dans le texte des Méthodes : la catégorie de
# référence de chacune (references_qualitatives), ce que vaut le coefficient d'une catégorie, et
# celui des termes croisés qui en ont une : une pente de la variable quantitative dans une
# catégorie moins sa pente dans la catégorie de référence, ou la pente de chaque catégorie sans
# l'effet principal de cette variable ; l'effet d'une catégorie d'une variable qualitative dans
# une catégorie de l'autre moins ce même effet dans sa catégorie de référence. `liste`,
# `guillemets` : ceux de texte_methodes_regression.
phrases_qualitatives <- function(m, fr, liste, guillemets) {
  comp <- m$composition
  refs <- references_qualitatives(m)
  if (!length(refs)) return(character(0))
  noms <- unlist(lapply(unname(comp), function(ct) stats::setNames(ct$noms, ct$variables)))
  nom <- function(u) guillemets(noms[[as.character(u)]])
  ref <- function(u) refs[[as.character(u)]] %||% NA_character_
  # (le terme de son effet principal parmi les candidats : une catégorie, ou la variable quantitative)
  principal <- function(u, categorie) Find(function(t) length(comp[[t]]$variables) == 1 && comp[[t]]$variables == u && (comp[[t]]$categories != 0) == categorie, m$candidats)
  phrases <- character(0)
  avec <- Filter(function(u) !is.null(principal(u, TRUE)) && !is.na(ref(u)), as.numeric(names(refs)))
  if (length(avec)) {
    quelles <- liste(vapply(avec, function(u) sprintf(if (fr) "%s pour %s" else "%s for %s", guillemets(ref(u)), nom(u)), ""))
    phrases <- sprintf(if (fr) "Une variable qualitative entre par une indicatrice (0 ou 1) par catégorie, sauf sa catégorie de référence (%s) : le coefficient d'une catégorie est son écart à la catégorie de référence."
                       else "A qualitative variable enters through one indicator (0 or 1) per category, except its reference category (%s): a category's coefficient is its difference from the reference category.", quelles)
  }
  # les termes croisés qui ont une variable qualitative, un par groupe de variables
  croises <- Filter(function(t) length(comp[[t]]$variables) > 1 && any(comp[[t]]$categories != 0), m$candidats)
  for (vs in unique(lapply(croises, function(t) comp[[t]]$variables))) {
    ct <- comp[[Find(function(t) identical(comp[[t]]$variables, vs), croises)]]
    terme <- guillemets(paste(ct$noms, collapse = " × "))
    q <- vs[ct$categories != 0]
    x <- vs[ct$categories == 0]
    px <- if (length(x) == 1) principal(x, FALSE)
    phrases <- c(phrases, if (length(x) == 1 && length(q) == 1 && is.null(px)) {
      sprintf(if (fr) "%s n'ayant pas d'effet principal, les coefficients de %s sont la pente de %s dans chaque catégorie de %s."
              else "As %s has no main effect, the coefficients of %s are the slope of %s in each category of %s.", nom(x), terme, nom(x), nom(q))
    } else if (length(x) == 1 && length(q) == 1 && !is.na(ref(q)) && px %in% m$lisses) {
      sprintf(if (fr) "Les coefficients de %s sont, pour chaque autre catégorie de %s, la pente qui s'ajoute dans cette catégorie à la courbe de %s, celle de %s."
              else "The coefficients of %s are, for each other category of %s, the slope added in that category to the curve of %s, that of %s.",
              terme, nom(q), nom(x), guillemets(ref(q)))
    } else if (length(x) == 1 && length(q) == 1 && !is.na(ref(q))) {
      sprintf(if (fr) "Les coefficients de %s sont, pour chaque autre catégorie de %s, l'écart de la pente de %s à sa pente dans %s (le coefficient de %s)."
              else "The coefficients of %s are, for each other category of %s, the difference between the slope of %s in it and its slope in %s (the coefficient of %s).",
              terme, nom(q), nom(x), guillemets(ref(q)), nom(x))
    } else if (!length(x) && length(q) == 2 && !is.null(principal(q[1], TRUE)) && !is.null(principal(q[2], TRUE)) && !anyNA(c(ref(q[1]), ref(q[2])))) {
      sprintf(if (fr) "Les coefficients de %s sont, pour chaque autre catégorie de %s et de %s, l'écart entre l'effet de la catégorie de %s (par rapport à %s) dans celle de %s et ce même effet dans %s."
              else "The coefficients of %s are, for each other category of %s and of %s, the difference between the effect of the category of %s (compared with %s) in that of %s and the same effect in %s.",
              terme, nom(q[1]), nom(q[2]), nom(q[2]), guillemets(ref(q[2])), nom(q[1]), guillemets(ref(q[1])))
    } else {
      sprintf(if (fr) "Les coefficients de %s se lisent par rapport aux catégories de référence de ses variables qualitatives." else "The coefficients of %s read against the reference categories of its qualitative variables.", terme)
    })
  }
  phrases
}

# Les niveaux emboîtés d'effets aléatoires, en mots : « site », « secteur » dans chaque « site »...
niveaux_emboites <- function(niveaux, fr, guillemets) vapply(seq_along(niveaux), function(l) {
  if (l == 1) guillemets(niveaux[l]) else sprintf(if (fr) "%s dans chaque %s" else "%s within each %s", guillemets(niveaux[l]), guillemets(niveaux[l - 1]))
}, "")

# Les pentes aléatoires d'une régression en mots, après ses ordonnées à l'origine aléatoires : la
# variable de chacune, son niveau (niveaux_emboites), et son lien à l'ordonnée à l'origine de son
# niveau, corrélées (nlme : une matrice de covariance générale) ou indépendantes (mgcv) ; "" sans
# pente aléatoire. `liste`, `guillemets` : ceux de texte_methodes_regression.
phrase_pentes <- function(m, fr, liste, guillemets, independantes = FALSE) {
  pentes <- m$pentes %||% list()
  avec <- which(lengths(pentes) > 0)
  if (!length(avec)) return("")
  niveaux <- niveaux_emboites(m$niveaux, fr, guillemets)
  quoi <- vapply(avec, function(l) sprintf(if (fr) "de %s par %s" else "of %s for %s", liste(guillemets(m$noms_termes[match(pentes[[l]], m$termes)])), niveaux[l]), "")
  une <- length(unlist(pentes)) == 1
  lien <- if (independantes) {
    if (fr) (if (une) "un terme pénalisé, indépendant de l'ordonnée à l'origine" else "des termes pénalisés, indépendants des ordonnées à l'origine")
    else (if (une) "a penalized term, independent of the intercept" else "penalized terms, independent of the intercepts")
  } else if (fr) (if (une) "corrélée à l'ordonnée à l'origine de son niveau : une matrice de covariance générale" else "corrélées à l'ordonnée à l'origine de leur niveau : une matrice de covariance générale par niveau")
  else (if (une) "correlated with its level's intercept: a general covariance matrix" else "correlated with their level's intercept: a general covariance matrix per level")
  sprintf(", %s %s (%s)", if (une) (if (fr) "une pente aléatoire" else "a random slope") else (if (fr) "des pentes aléatoires" else "random slopes"), liste(quoi), lien)
}

# Ce que R affiche d'une régression (la console de l'application et son journal).
print.modele_regression <- function(x, ...) {
  r <- x$resultat
  type <- switch(x$type, LMM = traduire("modèle linéaire mixte", "linear mixed model"),
                 GLM = sprintf(traduire("modèle linéaire généralisé, loi %s", "generalized linear model, %s distribution"), sub("_", " ", x$famille)),
                 GAM = sprintf(traduire("modèle additif généralisé, loi %s", "generalized additive model, %s distribution"), sub("_", " ", x$famille)),
                 traduire("régression linéaire", "linear regression"))
  cat(sprintf(traduire("Modèle %s (%s)%s\n", "Model %s (%s)%s\n"), x$nom, type, if (nzchar(x$titre %||% "")) paste(" :", x$titre) else ""))
  # (chaque niveau, et ses pentes aléatoires : « site (pente annee) »)
  niveaux <- vapply(seq_along(x$niveaux), function(l) {
    p <- x$noms_termes[match(x$pentes[[l]], x$termes)]
    if (length(p)) sprintf("%s (%s %s)", x$niveaux[l], traduire("pente", "slope"), paste(p, collapse = ", ")) else x$niveaux[l]
  }, "")
  cat(sprintf(traduire("%d observations, réponse %s%s\n", "%d observations, response %s%s\n"), x$N, x$y$noms,
              if (length(x$niveaux)) sprintf(traduire(", effets aléatoires %s%s", ", random effects %s%s"), paste(niveaux, collapse = " / "),
                                             if (x$car1) sprintf(traduire(", autocorrélation CAR1 dans le temps (%s, dans chaque %s)%s", ", CAR1 autocorrelation in time (%s, within each %s)%s"),
                                                                 x$temps, x$dans %||% x$niveaux[length(x$niveaux)], if (isTRUE(x$gamm)) ", mgcv::gamm" else "") else "") else ""))
  co <- r$coefficients
  print(data.frame(terme = co$terme, estimation = signif(co$estimation, 4), IC95 = sprintf("[%s ; %s]", signif(co$bas, 3), signif(co$haut, 3)),
                   p = signif(co$p, 3)), row.names = FALSE)
  cat(sprintf("AIC %.1f, AICc %.1f, BIC %.1f, logLik %.2f (%s %s)\n", r$AIC, r$AICc, r$BIC, r$logLik, format(round(r$parametres, 1)), traduire("paramètres", "parameters")))
  cat(paste(names(r$r2), signif(unlist(r$r2), 3), sep = " = ", collapse = ", "), sprintf("; Q2 = %.3f, RMSEcv = %.4g\n", x$vc$Q2, x$vc$RMSEcv))
  invisible(x)
}
