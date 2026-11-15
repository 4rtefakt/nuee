# Forêts aléatoires (FOREST ; Breiman, 2001), avec le paquet ranger (Wright et Ziegler, 2017) :
# une forêt de régression (une réponse quantitative) ou de classement (une variable de classe).
# Chaque arbre est grandi sur un tirage bootstrap des observations (avec remise), en ne
# considérant à chaque coupure que `mtry` variables X tirées au hasard, jusqu'à des nœuds de
# `min.node.size` observations ; la forêt prédit la moyenne de ses arbres (régression) ou la
# classe que le plus d'arbres votent (classement). Comme pour un arbre (arbres.R), les X entrent
# par leurs valeurs brutes (transformées s'il y a lieu). ranger ne prend pas de valeurs
# manquantes (pas avant sa version 0.16, celle d'Ubuntu 24.04 ; Debian 12 a la 0.14) : les
# observations où une valeur X manque sont laissées de côté et comptées, comme pour les
# k-moyennes. Les catégories d'une variable qualitative X sont rangées par la réponse moyenne
# avant chaque forêt (respect.unordered.factors = "order" ; Wright et König, 2019).
#
# Graine fixe (GRAINE_FORET) et un seul fil de calcul : les mêmes arbres à chaque ajustement.
# Ce que la forêt en donne :
#   - l'erreur hors sac (out-of-bag) : chaque observation prédite par les seuls arbres dont le
#     tirage ne la contient pas (le MSE et le R² de ranger, ou sa part mal classée et le tableau
#     de classement), et cette erreur selon le nombre d'arbres ;
#   - la validation croisée par les groupes du modèle, comme pour un arbre : chaque groupe prédit
#     par une forêt refaite sans lui (Q² = 1 - PRESS / SS, ou la part bien classée) ;
#   - l'importance de chaque variable, par permutation (la hausse de l'erreur hors sac quand ses
#     valeurs sont permutées : importance = "permutation" de ranger, divisée ou non par son
#     erreur-type) et par la baisse d'impureté de ses coupures (importance = "impurity", la
#     même forêt refaite : la même graine donne les mêmes arbres) ;
#   - à la demande, la dépendance partielle de chaque variable (Friedman, 2001) : la prédiction
#     moyenne sur les observations du modèle quand la variable prend chaque valeur d'une grille
#     (outils communs avec les arbres boostés, boosting.R, en fin de fichier).

GRAINE_FORET <- 1L

# Les réglages d'une forêt (d$options) ; mtry et la taille des nœuds, NA : ceux de ranger par
# défaut (la racine du nombre de variables arrondie en dessous ; 5 en régression, 1 en classement).
reglages_foret <- function(d) list(
  arbres = option_nombre(d, "arbres", 500, 10, 5000, TRUE), mtry = option_nombre(d, "mtry", NA, 1, 1e4, TRUE),
  min_noeud = option_nombre(d, "min_noeud", NA, 1, 1e6, TRUE), fraction = option_nombre(d, "fraction", 1, 0.1, 1),
  importance = option_choix(d, "importance", c("brute", "echelle")))

# La réponse d'une forêt : une classe (forêt de classement) ou une variable Y quantitative, jamais
# les deux ; TRUE pour une forêt de classement.
verifier_foret <- function(projet, d) {
  classement <- length(d$classe) > 0 && !is.na(d$classe[1])
  if (classement && length(d$y)) {
    stop(traduire("Une forêt a une seule réponse : une variable Y (forêt de régression) ou une classe (forêt de classement), pas les deux.",
                  "A forest has a single response: a Y variable (regression forest) or a class (classification forest), not both."), call. = FALSE)
  }
  ys <- unique(vapply(d$y, function(t) as.numeric(t$variables[1]), 0))  # (une variable qualitative : un terme par catégorie)
  if (!classement && length(ys) == 1 && length(variable_du_projet(projet, ys)$categories)) {
    stop(traduire("La réponse d'une forêt de régression est quantitative ; une variable qualitative est la classe d'une forêt de classement (rôle Classe).",
                  "A regression forest's response is quantitative; a qualitative variable is the class of a classification forest (Class role)."), call. = FALSE)
  }
  if (!classement && length(d$y) != 1) {
    stop(traduire("Une forêt demande une réponse : une variable Y quantitative (forêt de régression) ou une variable de classe (forêt de classement).",
                  "A forest needs a response: a quantitative Y variable (regression forest) or a class variable (classification forest)."), call. = FALSE)
  }
  verifier_x_ensemble(d)
  classement
}

# Les X d'un ensemble d'arbres (forêt, ou arbres boostés : `boost`) : au moins un, sans terme
# croisé (les arbres font eux-mêmes leurs interactions).
verifier_x_ensemble <- function(d, boost = FALSE) {
  if (any(lengths(lapply(d$x, `[[`, "variables")) > 1)) {
    stop(if (boost) traduire("Des arbres boostés font eux-mêmes leurs interactions : pas de termes croisés.", "Boosted trees make their own interactions: no cross terms.")
         else traduire("Une forêt fait elle-même ses interactions : pas de termes croisés.", "A forest makes its own interactions: no cross terms."), call. = FALSE)
  }
  if (!length(d$x)) {
    stop(if (boost) traduire("Des arbres boostés demandent au moins une variable X.", "Boosted trees need at least one X variable.")
         else traduire("Une forêt demande au moins une variable X.", "A forest needs at least one X variable."), call. = FALSE)
  }
}

# Les X bruts de ces observations (arbres.R : x_bruts) sous des noms simples (x1, x2...), pour
# ranger et rpart ; une valeur infinie (log de 0) y est une valeur manquante.
x_internes <- function(projet, vars, obs) {
  X <- x_bruts(projet, vars, obs)
  for (j in seq_along(X)) if (is.numeric(X[[j]])) X[[j]][!is.finite(X[[j]])] <- NA
  names(X) <- paste0("x", seq_along(X))
  X
}

# Le paquet ranger, ou un refus qui dit comment l'installer.
exiger_ranger <- function() {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop(traduire("Les forêts aléatoires demandent le paquet R ranger : install.packages(\"ranger\") (sous Debian ou Ubuntu : sudo apt install r-cran-ranger).",
                  "Random forests need the R package ranger: install.packages(\"ranger\") (on Debian or Ubuntu: sudo apt install r-cran-ranger)."), call. = FALSE)
  }
}

# Une forêt de ranger avec les réglages `r` (r$mtry_utilise, r$min_noeud_utilise : ceux qu'on
# a fixés), graine fixe, un seul fil ; `inbag` : les tirages de chaque arbre gardés (l'erreur hors
# sac selon le nombre d'arbres).
foret_ranger <- function(X, y, r, importance = "none", inbag = FALSE) {
  ranger::ranger(x = X, y = y, num.trees = r$arbres, mtry = r$mtry_utilise, min.node.size = r$min_noeud_utilise,
                 replace = TRUE, sample.fraction = r$fraction, importance = importance,
                 scale.permutation.importance = identical(importance, "permutation") && identical(r$importance, "echelle"),
                 respect.unordered.factors = "order", keep.inbag = inbag, write.forest = TRUE,
                 seed = GRAINE_FORET, num.threads = 1, verbose = FALSE)
}

# Les prédictions de la forêt (`tous` : celle de chaque arbre, une colonne par arbre ; le numéro
# de la classe en classement). La graine départage les classes à égalité de votes.
predire_ranger <- function(rf, X, tous = FALSE) {
  p <- stats::predict(rf, data = X, predict.all = tous, num.threads = 1, seed = GRAINE_FORET, verbose = FALSE)$predictions
  if (tous) matrix(p, nrow(X)) else p
}

# La part des votes de chaque classe de `classes` parmi les arbres (colonnes de `P` : le numéro de
# la classe parmi les catégories de la réponse, rf$forest$levels ; class.values les range dans
# l'ordre où les observations les donnent), en ne comptant que ceux où `garder` est vrai (les
# arbres hors sac de chaque observation) ; NA sans arbre. Une classe que la forêt n'a pas vue n'a
# pas de vote.
votes_classes <- function(P, rf, classes, garder = NULL) {
  codes <- match(classes, rf$forest$levels)
  if (is.null(garder)) garder <- matrix(TRUE, nrow(P), ncol(P))
  n <- rowSums(garder)
  V <- matrix(vapply(codes, function(k) if (is.na(k)) numeric(nrow(P)) else as.numeric(rowSums(P == k & garder)), numeric(nrow(P))),
              nrow(P), length(classes), dimnames = list(NULL, classes))
  V / ifelse(n > 0, n, NA)
}

# Un classement (classes observées et prédites, et les parts des votes de chaque classe pour les
# courbes ROC), sous la forme de classification_plsda (validation.R) : la classe prédite est
# celle de ranger, qui tire au sort entre des classes à égalité de votes.
classement_ensemble <- function(observee, predite, votes, classes) {
  observee <- as.character(observee)
  predite <- as.character(predite)
  tab <- table_classement(observee, predite, classes)
  list(predite = predite, table = tab, classes = classes, correct = diag(tab) / pmax(rowSums(tab), 1), global = sum(diag(tab)) / max(sum(tab), 1),
       roc = lapply(seq_along(classes), function(j) {
         g <- !is.na(observee) & !is.na(votes[, j])
         roc(votes[g, j], observee[g] == classes[j])
       }))
}

# L'erreur hors sac après 1, 2... arbres (comme randomForest la donne) : chaque observation prédite
# par ceux des premiers arbres dont le tirage ne la contient pas, parmi les observations qui en ont
# au moins un ; le MSE, ou la part mal classée (à égalité de votes, la première classe : ranger,
# lui, tire au sort, d'où un écart possible au dernier arbre).
courbe_oob <- function(P, hors_sac, y, classement, rf) {
  n <- nrow(P)
  erreur <- numeric(ncol(P))
  if (!classement) {
    s <- numeric(n)
    k <- numeric(n)
    for (t in seq_len(ncol(P))) {
      o <- hors_sac[, t]
      s[o] <- s[o] + P[o, t]
      k[o] <- k[o] + 1
      ok <- k > 0
      erreur[t] <- mean((y[ok] - s[ok] / k[ok])^2)
    }
  } else {
    classes <- levels(y)
    codes <- match(classes, rf$forest$levels)
    yi <- as.integer(y)
    V <- matrix(0, n, length(classes))
    for (t in seq_len(ncol(P))) {
      o <- which(hors_sac[, t])
      ij <- cbind(o, match(P[o, t], codes))
      V[ij] <- V[ij] + 1
      ok <- rowSums(V) > 0
      erreur[t] <- mean(max.col(V[ok, , drop = FALSE], ties.method = "first") != yi[ok])
    }
  }
  data.frame(arbres = seq_len(ncol(P)), erreur = erreur)
}

# La validation croisée : chaque groupe prédit par une forêt refaite sans lui (mêmes réglages, même
# graine). Sans ce groupe, une seule classe : toute forêt la prédit. Une forêt qui ne se fait pas
# laisse son groupe sans prédiction (`echecs`).
validation_foret <- function(X, y, groupes, r, classement) {
  classes <- if (classement) levels(y)
  pred <- if (classement) rep(NA_character_, nrow(X)) else rep(NA_real_, nrow(X))
  votes <- if (classement) matrix(NA_real_, nrow(X), length(classes), dimnames = list(NULL, classes))
  echecs <- 0
  for (g in unique(groupes)) {
    k <- groupes == g
    ya <- if (classement) droplevels(y[!k]) else y[!k]
    if (classement && nlevels(ya) == 1) {
      pred[k] <- levels(ya)
      votes[k, ] <- 0
      votes[k, levels(ya)] <- 1
      next
    }
    rf <- tryCatch(foret_ranger(X[!k, , drop = FALSE], ya, r), error = function(e) NULL)
    if (is.null(rf)) { echecs <- echecs + 1; next }
    if (classement) {
      pred[k] <- as.character(predire_ranger(rf, X[k, , drop = FALSE]))
      votes[k, ] <- votes_classes(predire_ranger(rf, X[k, , drop = FALSE], TRUE), rf, classes)
    } else pred[k] <- predire_ranger(rf, X[k, , drop = FALSE])
  }
  list(prediction = pred, votes = votes, echecs = echecs)
}

ajuster_forest <- function(projet, d) {
  exiger_ranger()
  r <- reglages_foret(d)
  classement <- verifier_foret(projet, d)
  vars <- variables_x_brutes(projet, d)
  obs <- d$observations
  X <- x_internes(projet, vars, obs)
  if (classement) {
    y <- classe_observations(projet, d$classe, obs)
    nom_y <- as.character(variable_du_projet(projet, d$classe)$ids[1])
    avec_y <- !is.na(y)
  } else {
    ty <- d$y[[1]]
    y <- suppressWarnings(transformer(valeurs_simples(projet, ty, obs), ty$transform))
    nom_y <- as.character(ty$name)
    avec_y <- is.finite(y)
  }
  complet <- avec_y & stats::complete.cases(X)
  if (sum(complet) < 5) {
    stop(traduire("Trop peu d'observations avec une réponse et toutes leurs valeurs X (il en faut au moins 5).",
                  "Too few observations with a response and all their X values (at least 5 are needed)."), call. = FALSE)
  }
  garde <- obs[complet]
  groupes <- groupes_observations(projet, d, garde)
  if (length(unique(groupes)) < 2) {
    stop(traduire("Une forêt demande au moins deux groupes de validation croisée parmi ses observations complètes : chaque groupe est prédit par une forêt refaite sans lui.",
                  "A forest needs at least two cross-validation groups among its complete observations: each group is predicted by a forest rebuilt without it."), call. = FALSE)
  }
  manquantes_x <- sum(avec_y & !stats::complete.cases(X))
  X <- X[complet, , drop = FALSE]
  y <- if (classement) droplevels(y[complet]) else y[complet]
  if (classement && nlevels(y) < 2) stop(traduire("Une forêt de classement demande au moins deux classes.", "A classification forest needs at least two classes."), call. = FALSE)
  K <- ncol(X)
  r$mtry_utilise <- if (is.na(r$mtry)) max(1L, as.integer(floor(sqrt(K)))) else as.integer(r$mtry)
  if (r$mtry_utilise > K) {
    stop(sprintf(traduire("mtry (%d) ne peut pas dépasser le nombre de variables X (%d).", "mtry (%d) can't exceed the number of X variables (%d)."), r$mtry_utilise, K), call. = FALSE)
  }
  r$min_noeud_utilise <- as.integer(if (is.na(r$min_noeud)) (if (classement) 1 else 5) else r$min_noeud)
  rf <- foret_ranger(X, y, r, "permutation", inbag = TRUE)
  impurete <- foret_ranger(X, y, r, "impurity")$variable.importance
  P <- predire_ranger(rf, X, TRUE)
  hors_sac <- do.call(cbind, rf$inbag.counts) == 0
  vc <- validation_foret(X, y, groupes, r, classement)
  noms <- vapply(vars, `[[`, "", "nom")
  importance <- data.frame(variable = noms, bloc = vapply(vars, `[[`, "", "bloc"), permutation = unname(rf$variable.importance),
                           impurete = unname(impurete), stringsAsFactors = FALSE)
  importance <- importance[order(-importance$permutation, seq_len(K)), , drop = FALSE]
  rownames(importance) <- NULL
  m <- list(type = "FOREST", classement = classement, N = nrow(X), K = K, observations = garde, exclues = setdiff(obs, garde), manquantes_x = manquantes_x,
            y = list(nom = nom_y, transform = if (!classement) d$y[[1]]$transform), variables = vars, reglages_foret = r, donnees = X, y_valeurs = y,
            groupes = groupes, G = length(unique(groupes)), vc_par = nom_groupes_vc(projet, d), ajustement = rf, classes = if (classement) levels(y),
            importance = importance, courbe = courbe_oob(P, hors_sac, y, classement, rf),
            qualitatives = any(vapply(vars, function(v) length(v$categories) > 0, TRUE)))
  if (classement) {
    classes <- levels(y)
    m$votes_oob <- votes_classes(P, rf, classes, hors_sac)
    m$classification <- list(oob = classement_ensemble(y, rf$predictions, m$votes_oob, classes),
                             vc = classement_ensemble(y, vc$prediction, vc$votes, classes),
                             ajuste = classement_ensemble(y, predire_ranger(rf, X), votes_classes(P, rf, classes), classes))
    m$oob <- list(erreur = rf$prediction.error, sans_prediction = sum(is.na(rf$predictions)))
    m$vc <- vc
  } else {
    e <- y - vc$prediction
    ok <- !is.na(e)
    m$ajuste <- predire_ranger(rf, X)
    m$oob <- list(prediction = rf$predictions, mse = rf$prediction.error, r2 = rf$r.squared, sans_prediction = sum(is.na(rf$predictions)))
    m$vc <- list(prediction = vc$prediction, Q2 = 1 - sum(e[ok]^2) / sum((y[ok] - mean(y))^2), RMSEcv = sqrt(mean(e[ok]^2)), echecs = vc$echecs)
  }
  m
}

# Le jeu de test d'une forêt : ses observations prédites (une observation où une valeur X manque
# reste sans prédiction) ; ajuster_analyse (analyses.R) l'appelle après l'ajustement.
tester_forest <- function(projet, m, d) {
  test <- as.numeric(d$test)
  p <- prediction_ensemble(projet, m, test)
  noms <- unname(projet$noms_obs[as.character(test)])
  if (m$classement) {
    y <- as.character(classe_observations(projet, d$classe, test))
    obs <- data.frame(uid = test, nom = noms, hors_domaine = FALSE, classe = y, classe_predite = p$prediction, stringsAsFactors = FALSE)
    return(list(n = length(test), hors_domaine = 0, observations = obs, classement = classement_test(y, p$prediction, m$classes), incompletes = sum(!p$complet)))
  }
  y <- suppressWarnings(transformer(valeurs_simples(projet, d$y[[1]], test), d$y[[1]]$transform))
  test_regression(m, test, noms, y, p$prediction, sum(!p$complet))
}

# Le jeu de test d'une réponse quantitative (forêt, arbres boostés), comme celui d'un arbre :
# RMSEP, biais, SEP et Q² externe (par la moyenne des observations du modèle).
test_regression <- function(m, test, noms, y, p, incompletes) {
  y[!is.finite(y)] <- NA
  e <- p - y
  ok <- !is.na(e)
  obs <- data.frame(uid = test, nom = noms, hors_domaine = FALSE, stringsAsFactors = FALSE)
  obs[[paste(m$y$nom, "observé")]] <- y
  obs[[paste(m$y$nom, "prédit")]] <- p
  list(n = length(test), hors_domaine = 0, observations = obs, incompletes = incompletes,
       statistiques = data.frame(y = m$y$nom, n = sum(ok), RMSEP = if (any(ok)) sqrt(mean(e[ok]^2)) else NA_real_,
                                 biais = if (any(ok)) mean(e[ok]) else NA_real_, SEP = if (sum(ok) > 1) stats::sd(e[ok]) else NA_real_,
                                 Q2ext = if (any(ok)) 1 - sum(e[ok]^2) / sum((y[ok] - mean(m$y_valeurs))^2) else NA_real_, stringsAsFactors = FALSE))
}

# Les prédictions d'une forêt ou d'arbres boostés pour des observations (`projet` peut n'avoir que
# des données : la prédiction d'un fichier) : la prédiction (la classe en classement ; NA pour une
# observation incomplète), et en classement la part des votes de chaque classe.
prediction_ensemble <- function(projet, m, observations) {
  X <- x_internes(projet, m$variables, observations)
  complet <- stats::complete.cases(X)
  if (identical(m$type, "BOOST")) {
    f <- rep(NA_real_, length(observations))
    if (any(complet)) f[complet] <- predire_boost_f(m, X[complet, , drop = FALSE])
    return(list(prediction = reponse_boost(f, m$loi), lien = f, complet = complet))
  }
  if (!m$classement) {
    p <- rep(NA_real_, length(observations))
    if (any(complet)) p[complet] <- predire_ranger(m$ajustement, X[complet, , drop = FALSE])
    return(list(prediction = p, complet = complet))
  }
  p <- rep(NA_character_, length(observations))
  V <- matrix(NA_real_, length(observations), length(m$classes), dimnames = list(NULL, m$classes))
  if (any(complet)) {
    p[complet] <- as.character(predire_ranger(m$ajustement, X[complet, , drop = FALSE]))
    V[complet, ] <- votes_classes(predire_ranger(m$ajustement, X[complet, , drop = FALSE], TRUE), m$ajustement, m$classes)
  }
  list(prediction = p, votes = V, complet = complet)
}

predire_forest <- function(projet, m, d, observations) {
  p <- prediction_ensemble(projet, m, observations)
  out <- data.frame(row.names = seq_along(observations))
  if (m$classement) {
    out[["classe prédite"]] <- p$prediction
    for (k in m$classes) out[[paste("probabilité", k)]] <- unname(p$votes[, k])
    if (length(projet$variables)) out[["classe observée"]] <- as.character(classe_observations(projet, d$classe, observations))
  } else {
    out[[paste(m$y$nom, "prédit")]] <- p$prediction
    y <- suppressWarnings(transformer(valeurs_simples(projet, d$y[[1]], observations), d$y[[1]]$transform))
    if (any(!is.na(y))) out[[paste(m$y$nom, "observé")]] <- y
  }
  out$hors_domaine <- FALSE
  rownames(out) <- NULL
  out
}

# Les observations d'une forêt : la réponse observée, prédite hors sac, en validation croisée et
# ajustée par tous les arbres (en classement : les classes, et la part des votes hors sac de
# chaque classe), le groupe de validation croisée.
observations_foret <- function(projet, m) {
  obs <- data.frame(uid = m$observations, nom = unname(projet$noms_obs[as.character(m$observations)]), stringsAsFactors = FALSE)
  if (m$classement) {
    cl <- m$classification
    obs$classe <- as.character(m$y_valeurs)
    obs[["classe prédite (OOB)"]] <- cl$oob$predite
    obs[["classe prédite (VC)"]] <- cl$vc$predite
    obs[["classe ajustée"]] <- cl$ajuste$predite
    for (k in m$classes) obs[[paste("probabilité", k)]] <- unname(m$votes_oob[, k])
  } else {
    y <- m$y$nom
    obs[[paste(y, "observé")]] <- m$y_valeurs
    obs[[paste(y, "prédit (OOB)")]] <- m$oob$prediction
    obs[[paste(y, "prédit (VC)")]] <- m$vc$prediction
    obs[[paste(y, "ajusté")]] <- m$ajuste
  }
  obs$groupe_vc <- m$groupes
  obs
}

resultat_forest <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  y <- m$y$nom
  r <- m$reglages_foret
  qualite <- if (m$classement) list(bien_classes_oob = 1 - m$oob$erreur, bien_classes_vc = m$classification$vc$global, erreur_oob = m$oob$erreur)
             else list(mse_oob = m$oob$mse, r2_oob = m$oob$r2, Q2 = m$vc$Q2, RMSEcv = m$vc$RMSEcv, RMSE_oob = sqrt(m$oob$mse))
  chiffres <- if (m$classement) sprintf("%s %s · %s %s", fmt_pourcent(1 - m$oob$erreur), traduire("bien classées (OOB)", "correct (OOB)"),
                                        fmt_pourcent(m$classification$vc$global), traduire("(VC)", "(CV)"))
              else sprintf("R2 OOB %.2f · Q2 %.2f · %d %s", m$oob$r2, m$vc$Q2, r$arbres, traduire("arbres", "trees"))
  list(id = id, projet = p$id, nom = d$name %||% "", titre = d$title %||% "", type = "FOREST", analyse = TRUE, prediction = TRUE,
       classement = m$classement, regression = FALSE, opls = FALSE, Ap = 0, A = 0, N = m$N, K = m$K, noms_y = I(y),
       transformation_y = nom_transformation(m$y$transform) %||% "", observations = observations_foret(p, m),
       importance = m$importance, courbe = m$courbe, qualite = qualite, reglages_foret = r, classes = I(m$classes %||% character(0)),
       classification = m$classification, oob_sans_prediction = m$oob$sans_prediction, echecs_vc = m$vc$echecs %||% 0,
       RMSEcv = if (!m$classement) stats::setNames(list(m$vc$RMSEcv), y),
       comparaison = if (!m$classement) list(R2 = m$oob$r2, Q2 = m$vc$Q2, RMSEE = sqrt(m$oob$mse), RMSEcv = m$vc$RMSEcv),
       dependance = unname(m$dependance), exclues_manquantes = length(m$exclues), manquantes_x = m$manquantes_x,
       variables_x = I(vapply(m$variables, `[[`, "", "nom")), qualitatives_x = m$qualitatives,
       G = m$G, vc_par = m$vc_par %||% "", reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app),
       note = d$note %||% "", test = m$test, demande = d$demande %||% list(type = d$type, numero = d$number),
       vues_defaut = I(c("ens_importance", if (m$classement) "ens_classement" else "ens_obspred")), chiffres = chiffres)
}

tables_forest <- function(projet, m) {
  obs <- observations_foret(projet, m)
  obs$uid <- NULL
  names(obs)[names(obs) == "nom"] <- "Observation"
  r <- m$reglages_foret
  resume <- data.frame(N = m$N, variables = m$K, groupes_vc = m$G, arbres = r$arbres, mtry = r$mtry_utilise, min_noeud = r$min_noeud_utilise,
                       fraction = r$fraction, importance = if (r$importance == "echelle") "permutation / erreur-type" else "permutation", graine = GRAINE_FORET,
                       stringsAsFactors = FALSE)
  if (m$classement) {
    resume$erreur_oob <- m$oob$erreur
    resume$bien_classees_vc <- m$classification$vc$global
  } else {
    resume$MSE_oob <- m$oob$mse
    resume$R2_oob <- m$oob$r2
    resume$Q2 <- m$vc$Q2
    resume$RMSEcv <- m$vc$RMSEcv
  }
  out <- list(resume = resume, observations = obs, importance = m$importance, erreur_oob = m$courbe)
  if (m$classement) {
    out$classement_oob <- tableau_classes(m$classification$oob$table, m$classes)
    out$classement_vc <- tableau_classes(m$classification$vc$table, m$classes)
  }
  if (length(m$dependance)) out$dependance <- tableau_dependance(m$dependance)
  out
}

texte_methodes_forest <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  r <- m$reglages_foret
  g <- function(x) guillemets_texte(x, fr)
  noms <- vapply(m$variables, `[[`, "", "nom")
  laissees <- if (length(m$exclues)) sprintf(if (fr) " (%d autres laissées de côté : leur réponse ou une valeur X manque)"
                                             else " (%d others left out: their response or an X value is missing)", length(m$exclues)) else ""
  reponse <- if (m$classement) sprintf(if (fr) "la classe %s (%d classes)" else "the class %s (%d classes)", g(m$y$nom), length(m$classes))
             else if (!is.null(m$y$transform)) formule_transformation(m$y$transform, paste0(g(m$y$nom), unite_texte(projet, m$y$nom)), fr)
             else paste0(g(m$y$nom), unite_texte(projet, m$y$nom))  # (avec son unité : atelier.R)
  p1 <- sprintf(if (fr) "La forêt aléatoire porte sur %s%s ; sa réponse est %s." else "The random forest uses %s%s; its response is %s.",
                observations_texte(m$N, d, fr), laissees, reponse)
  explicatives <- if (length(noms) <= 12) sprintf(if (fr) "%d variables explicatives (%s)" else "%d explanatory variables (%s)", length(noms), liste_mots(noms, fr))
                  else sprintf(if (fr) "%d variables explicatives" else "%d explanatory variables", length(noms))
  p2 <- sprintf(if (fr) "Une forêt aléatoire de %s (Breiman, 2001) de %d arbres a été construite avec le paquet ranger (Wright & Ziegler, 2017) sur les valeurs brutes de %s. Chaque arbre est grandi sur un tirage avec remise de %s des observations, en choisissant chaque coupure (%s) parmi %d variables tirées au hasard (mtry), jusqu'à des nœuds de %d observations (min.node.size)%s ; la graine des tirages est fixée (%d) et le calcul fait sur un seul fil, pour des résultats reproductibles."
                else "A random %s forest (Breiman, 2001) of %d trees was built with the ranger package (Wright & Ziegler, 2017) on the raw values of %s. Each tree is grown on a draw with replacement of %s of the observations, each split (%s) being chosen among %d randomly drawn variables (mtry), down to nodes of %d observations (min.node.size)%s; the random seed is fixed (%d) and the computation runs on a single thread, for reproducible results.",
                if (m$classement) (if (fr) "classement" else "classification") else (if (fr) "régression" else "regression"),
                r$arbres, explicatives, pourcent_texte(r$fraction, fr),
                if (m$classement) (if (fr) "indice de Gini" else "Gini index") else (if (fr) "réduction de la variance" else "variance reduction"),
                r$mtry_utilise, r$min_noeud_utilise,
                if (m$qualitatives) (if (fr) " ; les catégories d'une variable qualitative sont rangées par la réponse moyenne avant chaque forêt (Wright & König, 2019)"
                                     else "; the categories of a qualitative variable are ordered by the mean response before each forest (Wright & König, 2019)") else "",
                GRAINE_FORET)
  manquantes <- if (m$manquantes_x > 0) sprintf(if (fr) " Les %d observations où une valeur X manquait ont été laissées de côté (ranger ne les prend pas)."
                                                else " The %d observations with a missing X value were left out (ranger doesn't take them).", m$manquantes_x) else ""
  p2 <- paste0(p2, manquantes)
  p3 <- if (m$classement) {
    sprintf(if (fr) "Chaque observation prédite par les seuls arbres dont le tirage ne la contient pas (erreur hors sac), la forêt classe bien %s des observations. Sa capacité de prédiction a aussi été estimée par une validation croisée %s, chaque groupe étant prédit par une forêt refaite sans lui : %s bien classées."
            else "Each observation being predicted by the trees whose draw doesn't contain it (out-of-bag error), the forest classifies %s of the observations correctly. Its predictive ability was also estimated by a cross-validation %s, each group being predicted by a forest rebuilt without it: %s correctly classified.",
            pourcent_texte(1 - m$oob$erreur, fr), groupes_texte(m$vc_par, m$G, fr), pourcent_texte(m$classification$vc$global, fr))
  } else {
    sprintf(if (fr) "Chaque observation prédite par les seuls arbres dont le tirage ne la contient pas, l'erreur hors sac est MSE = %s (R² hors sac = %s). Sa capacité de prédiction a aussi été estimée par une validation croisée %s, chaque groupe étant prédit par une forêt refaite sans lui : Q² = %s (1 − PRESS / SS) et RMSEcv = %s."
            else "Each observation being predicted by the trees whose draw doesn't contain it, the out-of-bag error is MSE = %s (out-of-bag R² = %s). Its predictive ability was also estimated by a cross-validation %s, each group being predicted by a forest rebuilt without it: Q² = %s (1 − PRESS / SS) and RMSEcv = %s.",
            format(signif(m$oob$mse, 4), decimal.mark = if (fr) "," else "."), nombre_texte(m$oob$r2, 2, fr), groupes_texte(m$vc_par, m$G, fr),
            nombre_texte(m$vc$Q2, 2, fr), format(signif(m$vc$RMSEcv, 4), decimal.mark = if (fr) "," else "."))
  }
  premieres <- utils::head(m$importance$variable, 3)
  p4 <- sprintf(if (fr) "L'importance de chaque variable a été mesurée par permutation (la hausse de l'erreur hors sac quand ses valeurs sont permutées, %s ; Breiman, 2001) et par la baisse d'impureté de ses coupures ; par permutation, les plus importantes sont %s."
                else "The importance of each variable was measured by permutation (the increase in out-of-bag error when its values are permuted, %s; Breiman, 2001) and by the impurity decrease of its splits; by permutation, the most important are %s.",
                if (r$importance == "echelle") (if (fr) "divisée par son erreur-type" else "divided by its standard error") else (if (fr) "sans mise à l'échelle" else "unscaled"),
                liste_mots(vapply(premieres, g, ""), fr))
  if (length(m$dependance)) {
    p4 <- paste(p4, if (fr) "L'effet de chaque variable est montré par sa dépendance partielle (Friedman, 2001) : la prédiction moyenne de la forêt sur ses observations quand la variable prend chaque valeur d'une grille."
                    else "The effect of each variable is shown by its partial dependence (Friedman, 2001): the forest's mean prediction over its observations when the variable takes each value of a grid.")
  }
  if (!is.null(m$test)) {
    p4 <- paste(p4, if (m$classement) sprintf(if (fr) "Un jeu de test de %d observations, laissées hors de la forêt et de la validation croisée, a ensuite été prédit : %s bien classées%s."
                                               else "A test set of %d observations, left out of the forest and of the cross-validation, was then predicted: %s correctly classified%s.",
                                               m$test$n, pourcent_texte(m$test$classement$bien_classees, fr), classes_inconnues_texte(m$test$classement$inconnues, fr))
                else sprintf(if (fr) "Un jeu de test de %d observations, laissées hors de la forêt et de la validation croisée, a ensuite été prédit : RMSEP = %s, biais = %s, Q² externe = %s."
                             else "A test set of %d observations, left out of the forest and of the cross-validation, was then predicted: RMSEP = %s, bias = %s, external Q² = %s.",
                             m$test$n, nombre_texte(m$test$statistiques$RMSEP, 3, fr), nombre_texte(m$test$statistiques$biais, 3, fr), nombre_texte(m$test$statistiques$Q2ext, 2, fr)))
  }
  logiciels <- references_classement(c("breiman2001", "wright", if (m$qualitatives) "wright_konig", if (length(m$dependance)) "friedman2001"), "ranger", fr, version)
  list(paragraphes = c(p1, p2, p3, p4, logiciels$phrase), references = logiciels$references)
}

# ---------------------------------------------------------------- dépendance partielle
# (Friedman, 2001), commune aux forêts et aux arbres boostés (boosting.R) : pour une variable X, la
# prédiction moyenne du modèle sur ses observations, chacune prise avec la valeur de la grille à la
# place de la sienne, les autres X inchangés.

POINTS_DEPENDANCE <- 25

# La grille d'une variable : ses valeurs distinctes s'il y en a au plus POINTS_DEPENDANCE, sinon
# autant de valeurs régulièrement espacées de son minimum à son maximum (comme
# randomForest::partialPlot) ; les catégories observées d'une variable qualitative.
grille_dependance <- function(x) {
  if (is.factor(x)) return(factor(levels(droplevels(x)), levels = levels(x)))
  v <- sort(unique(x))
  if (length(v) <= POINTS_DEPENDANCE) v else seq(v[1], v[length(v)], length.out = POINTS_DEPENDANCE)
}

# Les observations du modèle une fois par valeur de la grille, la variable j remplacée par elle.
donnees_grille <- function(X, j, grille) {
  n <- nrow(X)
  G <- X[rep(seq_len(n), length(grille)), , drop = FALSE]
  G[[j]] <- rep(grille, each = n)
  rownames(G) <- NULL
  G
}

# Les valeurs observées d'une variable, pour le tapis du graphique : ses valeurs distinctes (au plus
# 300 : sinon ses quantiles), ou l'effectif de chaque catégorie.
tapis_dependance <- function(x) {
  if (is.factor(x)) return(as.vector(table(droplevels(x))))
  v <- sort(unique(x))
  if (length(v) <= 300) v else unique(unname(stats::quantile(x, seq(0, 1, length.out = 300), type = 1)))
}

# La dépendance partielle de la variable j d'un modèle (FOREST ou BOOST : dependance_<type>
# calcule la prédiction moyenne de chaque valeur de la grille).
dependance_variable <- function(m, j) {
  x <- m$donnees[[j]]
  grille <- grille_dependance(x)
  out <- list(variable = m$variables[[j]]$nom, qualitative = is.factor(x), x = I(if (is.factor(x)) as.character(grille) else grille), tapis = I(tapis_dependance(x)))
  c(out, fonction_analyse("dependance_", m$type)(m, j, grille))
}

# Une forêt : la prédiction moyenne (régression) ou la part moyenne des votes de chaque classe.
dependance_forest <- function(m, j, grille) {
  X <- m$donnees
  if (!m$classement) {
    p <- predire_ranger(m$ajustement, donnees_grille(X, j, grille))
    return(list(y = I(as.vector(tapply(p, rep(seq_along(grille), each = nrow(X)), mean)))))
  }
  V <- t(vapply(seq_along(grille), function(i) colMeans(votes_classes(predire_ranger(m$ajustement, donnees_grille(X, j, grille[i]), TRUE), m$ajustement, m$classes)),
                numeric(length(m$classes))))
  list(probabilites = stats::setNames(as.data.frame(matrix(V, length(grille)), stringsAsFactors = FALSE), m$classes))
}

# La dépendance partielle à la demande, gardée avec le modèle : les variables demandées (leurs
# noms ; toutes sans nom), dans l'ordre des X.
demande_analyse_dependance_partielle <- function(q) {
  e <- modele(q$modele)
  if (!isTRUE(e$modele$type %in% c("FOREST", "BOOST"))) {
    stop(traduire("Seulement pour une forêt aléatoire ou des arbres boostés.", "Only for a random forest or boosted trees."), call. = FALSE)
  }
  m <- e$modele
  noms <- vapply(m$variables, `[[`, "", "nom")
  voulues <- as.character(unlist(q$variables))
  voulues <- if (length(voulues)) voulues else noms
  inconnues <- setdiff(voulues, noms)
  if (length(inconnues)) stop(sprintf(traduire("Variable inconnue du modèle : %s.", "Variable unknown to the model: %s."), paste(inconnues, collapse = ", ")), call. = FALSE)
  deja <- m$dependance %||% list()
  for (v in setdiff(voulues, names(deja))) deja[[v]] <- dependance_variable(m, match(v, noms))
  deja <- deja[intersect(noms, names(deja))]
  etat$modeles[[q$modele]]$modele$dependance <- deja
  list(modele = q$modele, dependance = unname(deja[intersect(noms, voulues)]))
}

# Les dépendances partielles calculées, pour Exporter : une ligne par variable et valeur de la
# grille (la prédiction moyenne, ou la part des votes de chaque classe ; pour des arbres boostés,
# aussi à l'échelle de la réponse).
tableau_dependance <- function(dep) {
  do.call(rbind, lapply(dep, function(x) {
    out <- data.frame(variable = x$variable, valeur = as.character(unclass(x$x)), stringsAsFactors = FALSE)
    if (!is.null(x$probabilites)) for (k in names(x$probabilites)) out[[paste("probabilité", k)]] <- x$probabilites[[k]]
    if (!is.null(x$y)) out$prediction <- as.vector(x$y)
    if (!is.null(x$reponse)) out$reponse <- as.vector(x$reponse)
    out
  }))
}
