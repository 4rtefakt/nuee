# L'export vers R d'une forêt aléatoire (analyses.R : ecrire_script_<type> ; forets.R), et ce qu'il
# partage avec celui des arbres boostés (boosting_script.R) : les X, le jeu de test, la dépendance
# partielle. Le script refait la forêt avec R et le paquet ranger, le seul paquet hors de R dont il a
# besoin (Nuée en dépend : il est installé avec lui), avec la même graine, un seul fil et les mêmes
# réglages : la même forêt, arbre pour arbre, quand la version de ranger est la même. Ses fonctions
# sont celles du moteur (forets.R, recopiées : script_moteur.R) ; puis ses données, ses X par leurs
# valeurs brutes (x1, x2… comme le moteur les nomme), la forêt, l'erreur hors sac et sa courbe, la
# validation croisée par les groupes du modèle, les importances, le classement, le jeu de test et la
# dépendance partielle quand elle a été calculée ; des graphiques en R de base. Ou un rapport R
# Markdown ou Quarto. Avant d'être écrit, il est exécuté ici et comparé au modèle de l'application.

# Les noms du script en anglais (traduire_script) : ceux des fonctions de forets.R, arbres.R et
# validation.R, puis ceux du calcul.
NOMS_FORET_EN <- c(
  foret_ranger = "ranger_forest", predire_ranger = "predict_ranger", votes_classes = "class_votes", courbe_oob = "oob_curve",
  validation_foret = "forest_cross_validation", classement_ensemble = "classification_of", classement_test = "test_classification",
  table_classement = "classification_table", grille_dependance = "dependence_grid", donnees_grille = "grid_data", exiger_ranger = "require_ranger",
  GRAINE_FORET = "SEED_FOREST", POINTS_DEPENDANCE = "DEPENDENCE_POINTS",
  arbres = "trees", bien_classees = "correct_share", classement = "classification", erreur = "error", garder = "counted", groupes = "groups",
  hors_sac = "out_of_bag", inconnues = "unseen", min_noeud_utilise = "min_node_used", mtry_utilise = "mtry_used", observee = "observed",
  positif = "positive", predite = "predicted", seuils = "thresholds", grille = "grid", echecs = "failures",
  # (le calcul du script)
  noms_x = "x_names", variables_x = "x_variables", reglages = "settings", impurete = "impurity", courbe = "curve", ajuste = "fitted",
  qualite = "quality", votes_oob = "oob_votes", prediction_test = "test_prediction", complet = "complete", y_test = "y_test", X_test = "X_test",
  dependance = "partial_dependence", probabilites = "probabilities", couleurs = "colours", reponse = "response", lien = "link",
  erreur_oob = "oob_error", bien_classees_vc = "correct_share_cv", mse_oob = "oob_mse", r2_oob = "oob_r2")

# ---------------------------------------------------------------- partagé avec les arbres boostés

# Les X d'une forêt ou d'arbres boostés : une fonction du tableau `d` qui rend leurs valeurs brutes
# (transformées s'il y a lieu ; une valeur infinie, le log de 0, manquante), sous les noms simples
# du moteur (x1, x2… : forets.R, x_internes), et `noms_x`, leurs noms dans Nuée.
code_x_ensemble <- function(m, tab, tx) {
  vars <- m$variables
  valeurs <- vapply(seq_along(vars), function(j) {
    v <- vars[[j]]
    col <- sprintf("d[[%s]]", code_texte(tab$noms[[nom_numero(v$uid)]]))
    sprintf("x%d = %s", j, if (!is.null(v$transform)) code_transformation(v$transform, col, TRUE) else col)
  }, "")
  transformees <- vapply(Filter(function(v) !is.null(v$transform), vars), `[[`, "", "nom")
  c(commentaire(tx(sprintf("Les variables X, par leurs valeurs brutes%s ; une variable qualitative reste un facteur de toutes ses catégories ; une valeur infinie (le log de 0) est manquante. Elles s'appellent x1, x2… comme dans le moteur de Nuée (`noms_x` : leurs noms dans Nuée).",
                           if (length(transformees)) sprintf(", transformées pour %s", texte_sur(paste(transformees, collapse = ", "))) else ""),
                   sprintf("The X variables, by their raw values%s; a qualitative variable stays a factor of all its categories; an infinite value (the log of 0) is missing. They are called x1, x2… as in Nuée's engine (`x_names`: their names in Nuée).",
                           if (length(transformees)) sprintf(", transformed for %s", texte_sur(paste(transformees, collapse = ", "))) else ""))),
    code_valeurs(sprintf("x%d = %s", seq_along(vars), vapply(vars, function(v) code_texte(v$nom), "")), "noms_x <- "),
    "variables_x <- function(d) {",
    code_valeurs(valeurs, "  X <- data.frame(", ")", ouvrir = ""),
    "  for (j in seq_along(X)) if (is.numeric(X[[j]])) X[[j]][!is.finite(X[[j]])] <- NA",
    "  X",
    "}",
    "X <- variables_x(d)")
}

# La réponse d'une forêt de régression ou d'arbres boostés : une variable Y, transformée s'il y a lieu.
code_y_ensemble <- function(m, d, tab, tx, nom = "y", lignes = "d") {
  col <- sprintf("%s[[%s]]", lignes, code_texte(tab$noms[[nom_numero(d$y[[1]]$variables[1])]]))
  sprintf("%s <- %s", nom, if (!is.null(m$y$transform)) code_transformation(m$y$transform, col, TRUE) else col)
}

# Le jeu de test d'une réponse quantitative (forêt de régression, arbres boostés), comme le moteur
# (forets.R : test_regression) : RMSEP, biais, SEP et Q² externe par rapport à la moyenne des
# observations du modèle ; une observation où une valeur X manque reste sans prédiction.
code_test_regression <- function(m, d, tab, tx, prediction) {
  c(commentaire(tx("Le jeu de test, prédit par le modèle : RMSEP, biais (moyenne des erreurs), SEP (leur écart-type) et Q² externe (par rapport à la moyenne des observations du modèle) ; une observation où une valeur X manque reste sans prédiction.",
                   "The test set, predicted by the model: RMSEP, bias (mean of the errors), SEP (their standard deviation) and external Q² (against the mean of the model's observations); an observation with a missing X value stays without a prediction.")),
    "X_test <- variables_x(donnees[lignes_test, ])",
    code_y_ensemble(m, d, tab, tx, "y_test", "donnees[lignes_test, ]"),
    "y_test[!is.finite(y_test)] <- NA",
    "complet <- complete.cases(X_test)",
    "prediction_test <- rep(NA_real_, nrow(X_test))",
    sprintf("prediction_test[complet] <- %s", prediction),
    "erreur <- prediction_test - y_test",
    "ok <- !is.na(erreur)",
    "test <- c(n = sum(ok), RMSEP = sqrt(mean(erreur[ok]^2)), biais = mean(erreur[ok]), SEP = if (sum(ok) > 1) sd(erreur[ok]) else NA_real_,",
    "          Q2ext = 1 - sum(erreur[ok]^2) / sum((y_test[ok] - mean(y))^2))")
}

# La dépendance partielle des variables pour lesquelles Nuée l'a calculée (m$dependance : Friedman
# 2001), dans l'ordre des X : la prédiction moyenne du modèle sur ses observations quand la variable
# prend chaque valeur de sa grille (grille_dependance) ; `calcul` : le code qui la donne pour la
# variable j et sa grille (une fonction de j et grille).
code_dependance_ensemble <- function(m, tx, explication, calcul) {
  if (!length(m$dependance)) return(NULL)
  noms <- vapply(m$variables, `[[`, "", "nom")
  j <- match(names(m$dependance), noms)
  c(commentaire(tx(sprintf("La dépendance partielle (Friedman 2001), calculée dans Nuée pour %s : %s",
                           liste_textes(texte_sur(names(m$dependance)), "et"), explication[1]),
                   sprintf("The partial dependence (Friedman 2001), computed in Nuée for %s: %s",
                           liste_textes(texte_sur(names(m$dependance)), "and"), explication[2]))),
    code_valeurs(sprintf("%s = %d", vapply(noms[j], code_texte, ""), j), "dependance <- lapply(c(", "), function(j) {", ouvrir = ""),
    "  grille <- grille_dependance(X[[j]])",
    paste0("  ", calcul),
    "})")
}

# La partie « Résultats » d'une dépendance partielle, et son graphique.
graphique_dependance <- function(m, tx, y, ylab) {
  if (!length(m$dependance)) return(NULL)
  n <- length(m$dependance)
  list(nom = "dependance-partielle", largeur = min(4 * n, 12), hauteur = 4 * ceiling(n / 3), code = c(
    sprintf("op <- par(mfrow = c(%d, %d))", ceiling(n / 3), min(n, 3)),
    "for (v in names(dependance)) {",
    "  x <- dependance[[v]]$x",
    sprintf("  if (is.factor(x)) barplot(%s, names.arg = as.character(x), xlab = v, ylab = %s, las = 2)", y, code_texte(ylab)),
    sprintf("  else plot(x, %s, type = \"l\", lwd = 2, xlab = v, ylab = %s)", y, code_texte(ylab)),
    "}",
    "par(op)"))
}

# Les écarts d'une liste de paires (résultat du moteur, résultat du script) : NULL, ou ce qui diffère.
ecarts_paires <- function(paires) {
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# ---------------------------------------------------------------- les forêts

ENTETES_FORET_EN <- c(
  exiger_ranger ="The ranger package, or a refusal that says how to install it.",
  foret_ranger = "A ranger forest with the settings `r` (r$mtry_used, r$min_node_used: those set), a fixed seed, a single thread; `inbag`: the draws of each tree kept (the out-of-bag error by the number of trees).",
  predire_ranger = "The predictions of the forest (`tous`: that of each tree, one column per tree; the number of the class in classification). The seed breaks the ties between classes with as many votes.",
  votes_classes = "The share of the votes of each class of `classes` among the trees (columns of `P`: the number of the class among the response's categories, rf$forest$levels; class.values orders them as the observations give them), counting only those where `counted` is TRUE (each observation's out-of-bag trees); NA without a tree. A class the forest hasn't seen has no vote.",
  classement_ensemble = "A classification (observed and predicted classes, and the shares of the votes of each class for the ROC curves), in the form of Nuée's classification_plsda: the predicted class is ranger's, which draws lots between classes with as many votes.",
  courbe_oob = "The out-of-bag error after 1, 2... trees (as randomForest gives it): each observation predicted by those of the first trees whose draw doesn't contain it, among the observations that have at least one; the MSE, or the share misclassified (with as many votes, the first class: ranger draws lots, hence a possible difference at the last tree).",
  validation_foret = "The cross-validation: each group predicted by a forest rebuilt without it (same settings, same seed). Without this group, a single class: any forest predicts it. A forest that can't be made leaves its group without a prediction (`failures`).",
  table_classement = "The classification table (observed classes in rows, predicted in columns) of the observations where both are known.",
  classement_test = "The test set of a classification: the share correctly classified, the classes, the table, over the observations of a class the model knows (it can't predict a class it hasn't seen: `unseen` counts them).",
  roc = "The ROC curve of a score for the observations where `positive` is TRUE: the true and false positive rates at each threshold, and the area under the curve (the trapezoids).",
  grille_dependance = "The grid of a variable: its distinct values if there are at most DEPENDENCE_POINTS, otherwise as many evenly spaced values from its minimum to its maximum (as randomForest::partialPlot); the observed categories of a qualitative variable.",
  donnees_grille = "The model's observations once per value of the grid, the variable j replaced by it.")

racines_foret <- function(m) {
  c("exiger_ranger", "foret_ranger", "predire_ranger", "courbe_oob", "validation_foret", if (m$classement) c("votes_classes", "classement_ensemble"),
    if (!is.null(m$test) && m$classement) "classement_test", if (length(m$dependance)) c("grille_dependance", "donnees_grille"))
}

code_calcul_foret <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  r <- m$reglages_foret
  test <- !is.null(m$test)
  classement <- m$classement
  col_y <- tab$noms[[nom_numero(if (classement) d$classe else d$y[[1]]$variables[1])]]
  reponse <- if (classement) c(commentaire(tx(sprintf("La réponse : la classe « %s » (ses classes présentes).", texte_sur(m$y$nom)), sprintf("The response: the class “%s” (its classes present).", texte_sur(m$y$nom)))),
                               sprintf("y <- droplevels(d[[%s]])", code_texte(col_y)), "classes <- levels(y)")
             else c(commentaire(tx(sprintf("La réponse : « %s »%s.", texte_sur(m$y$nom), if (!is.null(m$y$transform)) ", transformée" else ""),
                                   sprintf("The response: “%s”%s.", texte_sur(m$y$nom), if (!is.null(m$y$transform)) ", transformed" else ""))),
                    code_y_ensemble(m, d, tab, tx))
  dependance <- code_dependance_ensemble(m, tx,
    if (classement) c("la part moyenne des votes de chaque classe sur les observations du modèle quand la variable prend chaque valeur de sa grille.",
                      "the mean share of the votes of each class over the model's observations when the variable takes each value of its grid.")
    else c("la prédiction moyenne de la forêt sur les observations du modèle quand la variable prend chaque valeur de sa grille.",
           "the forest's mean prediction over the model's observations when the variable takes each value of its grid."),
    if (classement) c("probabilites <- t(vapply(seq_along(grille), function(i) colMeans(votes_classes(predire_ranger(rf, donnees_grille(X, j, grille[i]), TRUE), rf, classes)),",
                      "                       numeric(length(classes))))",
                      "list(x = grille, probabilites = matrix(probabilites, length(grille), dimnames = list(NULL, classes)))")
    else c("p <- predire_ranger(rf, donnees_grille(X, j, grille))",
           "list(x = grille, y = as.vector(tapply(p, rep(seq_along(grille), each = nrow(X)), mean)))"))
  c(code_lignes_modele(projet, d, m, tab, tx, c("réponse ou une valeur X manquante : ranger n'en prend pas", "response or an X value missing: ranger takes none")), "",
    commentaire(tx("ranger, le paquet des forêts aléatoires (Wright et Ziegler 2017) : le seul dont ce script a besoin hors de R.", "ranger, the random forest package (Wright and Ziegler 2017): the only one this script needs beyond R.")),
    "exiger_ranger()", "",
    code_x_ensemble(m, tab, tx), "",
    reponse, "",
    code_groupes_modele(m, tx), "",
    commentaire(tx(sprintf("La forêt : %d arbres, chacun grandi sur un tirage avec remise de %s des observations, chaque coupure choisie parmi %d variables tirées au hasard (mtry%s), jusqu'à des nœuds de %d observations (min.node.size%s) ; la graine %d et un seul fil : les arbres de Nuée. L'importance de chaque variable par permutation (%s), puis par l'impureté (la même forêt refaite : la même graine, les mêmes arbres).",
                           r$arbres, pourcent_texte(r$fraction, TRUE), r$mtry_utilise, if (is.na(r$mtry)) " de ranger par défaut : la racine du nombre de variables, arrondie en dessous" else "",
                           r$min_noeud_utilise, if (is.na(r$min_noeud)) sprintf(" de ranger par défaut en %s", if (classement) "classement" else "régression") else "", GRAINE_FORET,
                           if (r$importance == "echelle") "divisée par son erreur-type" else "sans mise à l'échelle"),
                   sprintf("The forest: %d trees, each grown on a draw with replacement of %s of the observations, each split chosen among %d randomly drawn variables (mtry%s), down to nodes of %d observations (min.node.size%s); the seed %d and a single thread: Nuée's trees. The importance of each variable by permutation (%s), then by impurity (the same forest refitted: the same seed, the same trees).",
                           r$arbres, pourcent_texte(r$fraction, FALSE), r$mtry_utilise, if (is.na(r$mtry)) ", ranger's default: the square root of the number of variables, rounded down" else "",
                           r$min_noeud_utilise, if (is.na(r$min_noeud)) sprintf(", ranger's default in %s", if (classement) "classification" else "regression") else "", GRAINE_FORET,
                           if (r$importance == "echelle") "divided by its standard error" else "unscaled"))),
    sprintf("reglages <- list(arbres = %d, mtry_utilise = %dL, min_noeud_utilise = %dL, fraction = %s, importance = %s)",
            as.integer(r$arbres), as.integer(r$mtry_utilise), as.integer(r$min_noeud_utilise), texte_exact(r$fraction), code_texte(r$importance)),
    "rf <- foret_ranger(X, y, reglages, \"permutation\", inbag = TRUE)",
    "impurete <- foret_ranger(X, y, reglages, \"impurity\")$variable.importance",
    "importance <- data.frame(variable = unname(noms_x), permutation = unname(rf$variable.importance), impurete = unname(impurete))",
    "importance <- importance[order(-importance$permutation, seq_len(ncol(X))), ]",
    "rownames(importance) <- NULL",
    "",
    commentaire(tx("L'erreur hors sac : chaque observation prédite par les seuls arbres dont le tirage ne la contient pas ; et selon le nombre d'arbres, d'après la prédiction de chaque arbre et ses tirages.",
                   "The out-of-bag error: each observation predicted by the trees whose draw doesn't contain it; and by the number of trees, from each tree's prediction and its draws.")),
    "P <- predire_ranger(rf, X, TRUE)",
    "hors_sac <- do.call(cbind, rf$inbag.counts) == 0",
    sprintf("courbe <- courbe_oob(P, hors_sac, y, %s, rf)", if (classement) "TRUE" else "FALSE"),
    "",
    commentaire(tx(sprintf("La validation croisée par les %d groupes : chaque groupe prédit par une forêt refaite sans lui (quelques secondes).", m$G),
                   sprintf("The cross-validation by the %d groups: each group predicted by a forest rebuilt without it (a few seconds).", m$G))),
    sprintf("vc <- validation_foret(X, y, groupes_vc, reglages, %s)", if (classement) "TRUE" else "FALSE"),
    if (classement) c(
      commentaire(tx("Le classement hors sac, en validation croisée et par toute la forêt : la classe prédite et la part des votes de chaque classe.",
                     "The classification out of bag, in cross-validation and by the whole forest: the predicted class and the share of the votes of each class.")),
      "votes_oob <- votes_classes(P, rf, classes, hors_sac)",
      "classement <- list(oob = classement_ensemble(y, rf$predictions, votes_oob, classes), vc = classement_ensemble(y, vc$prediction, vc$votes, classes),",
      "                   ajuste = classement_ensemble(y, predire_ranger(rf, X), votes_classes(P, rf, classes), classes))",
      "qualite <- c(erreur_oob = rf$prediction.error, bien_classees_vc = classement$vc$global)")
    else c(
      commentaire(tx("Toute la forêt sur ses observations ; l'erreur hors sac (MSE et R² de ranger) ; Q² = 1 - PRESS / SS et RMSEcv de la validation croisée.",
                     "The whole forest on its observations; the out-of-bag error (ranger's MSE and R²); Q² = 1 - PRESS / SS and RMSEcv of the cross-validation.")),
      "ajuste <- predire_ranger(rf, X)",
      "erreur <- y - vc$prediction",
      "ok <- !is.na(erreur)",
      "qualite <- c(mse_oob = rf$prediction.error, r2_oob = rf$r.squared, Q2 = 1 - sum(erreur[ok]^2) / sum((y[ok] - mean(y))^2), RMSEcv = sqrt(mean(erreur[ok]^2)))"),
    if (test && classement) c("",
      commentaire(tx("Le jeu de test, classé par la forêt (une observation où une valeur X manque reste sans classe ; une classe que la forêt n'a pas vue n'est pas comptée).",
                     "The test set, classified by the forest (an observation with a missing X value stays without a class; a class the forest hasn't seen isn't counted).")),
      "X_test <- variables_x(donnees[lignes_test, ])",
      sprintf("y_test <- as.character(donnees[lignes_test, ][[%s]])", code_texte(col_y)),
      "complet <- complete.cases(X_test)",
      "prediction_test <- rep(NA_character_, nrow(X_test))",
      "prediction_test[complet] <- as.character(predire_ranger(rf, X_test[complet, , drop = FALSE]))",
      "test <- classement_test(y_test, prediction_test, classes)"),
    if (test && !classement) c("", code_test_regression(m, d, tab, tx, "predire_ranger(rf, X_test[complet, , drop = FALSE])")),
    if (!is.null(dependance)) c("", dependance))
}

code_sortie_foret <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  ligne <- function(code, fr, en) ligne_sortie(traduire_script(code, langue, c(NOMS_FORET_EN, NOMS_SCRIPT_EN)), tx(fr, en))
  c(ligne("print(rf)", "la forêt, telle que ranger la décrit", "the forest, as ranger describes it"),
    ligne("print(qualite)", if (m$classement) "erreur hors sac, part bien classée en validation croisée" else "MSE et R² hors sac, Q² et RMSEcv",
          if (m$classement) "out-of-bag error, share correctly classified in cross-validation" else "out-of-bag MSE and R², Q² and RMSEcv"),
    ligne("print(importance)", "l'importance de chaque variable, par permutation et par l'impureté", "the importance of each variable, by permutation and by impurity"),
    if (m$classement) ligne("print(classement$vc$table)", "classement en validation croisée (observées en lignes)", "cross-validated classification (observed in rows)"),
    if (!is.null(m$test)) ligne(if (m$classement) "print(test$table)" else "print(test)", "jeu de test", "test set"),
    if (length(m$dependance)) ligne("str(dependance)", "la dépendance partielle", "the partial dependence"))
}

code_graphiques_foret <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  t <- function(code) traduire_script(code, langue, c(NOMS_FORET_EN, NOMS_SCRIPT_EN))
  lapply(Filter(Negate(is.null), list(
    list(nom = "importance", largeur = 7, hauteur = 5, code = c(
      "op <- par(mar = c(4, 9, 2, 1))",
      sprintf("barplot(rev(importance$permutation), names.arg = rev(importance$variable), horiz = TRUE, las = 1, xlab = %s, main = %s)",
              code_texte(tx("Importance par permutation", "Permutation importance")), code_texte(texte_sur(m$y$nom))),
      "par(op)")),
    list(nom = "erreur-oob", largeur = 7, hauteur = 4.5, code = c(
      sprintf("plot(courbe$arbres, courbe$erreur, type = \"l\", xlab = %s, ylab = %s, main = %s)", code_texte(tx("Arbres", "Trees")),
              code_texte(if (m$classement) tx("Part mal classée hors sac", "Out-of-bag share misclassified") else tx("MSE hors sac", "Out-of-bag MSE")),
              code_texte(tx("Erreur hors sac selon le nombre d'arbres", "Out-of-bag error by the number of trees"))))),
    if (!m$classement) list(nom = "observe-predit", largeur = 6, hauteur = 6, code = c(
      sprintf("plot(y, rf$predictions, pch = 19, col = \"#2f6fb580\", xlab = %s, ylab = %s)", code_texte(tx("observé", "observed")), code_texte(tx("prédit", "predicted"))),
      "points(y, vc$prediction, pch = 1, col = \"#d1495b\")",
      "abline(0, 1, lty = 3)",
      sprintf("legend(\"topleft\", c(%s, %s), pch = c(19, 1), col = c(\"#2f6fb5\", \"#d1495b\"), bty = \"n\")",
              code_texte(tx("hors sac", "out of bag")), code_texte(tx("validation croisée", "cross-validation"))))),
    graphique_dependance(m, tx, if (m$classement) "dependance[[v]]$probabilites[, 1]" else "dependance[[v]]$y",
                         if (m$classement) sprintf(tx("part des votes « %s »", "share of the votes “%s”"), m$classes[1]) else tx("prédiction moyenne", "mean prediction")))),
    function(g) { g$code <- t(g$code); g })
}

ecart_foret <- function(m, e, langue) {
  r <- function(x) get0(nom_traduit(x, langue, NOMS_FORET_EN), envir = e, inherits = FALSE)
  champ <- function(x, ...) { for (k in c(...)) x <- if (!is.null(x)) x[[nom_traduit(k, langue, NOMS_FORET_EN)]]; x }
  rf <- r("rf")
  imp <- r("importance")
  dep <- r("dependance")
  cl <- r("classement")
  paires <- list(
    "hors sac" = list(if (m$classement) as.numeric(factor(m$classification$oob$predite, m$classes)) else m$oob$prediction,
                      if (!is.null(rf)) { if (m$classement) as.numeric(factor(as.character(rf$predictions), m$classes)) else rf$predictions }),
    "erreur hors sac" = list(if (m$classement) m$oob$erreur else c(m$oob$mse, m$oob$r2), if (!is.null(rf)) { if (m$classement) rf$prediction.error else c(rf$prediction.error, rf$r.squared) }),
    importance = list(as.matrix(m$importance[c("permutation", "impurete")]), if (!is.null(imp)) as.matrix(imp[2:3])),
    "erreur selon les arbres" = list(m$courbe$erreur, champ(r("courbe"), "erreur")),
    "validation croisée" = list(if (m$classement) as.numeric(factor(m$classification$vc$predite, m$classes)) else m$vc$prediction,
                                if (m$classement) as.numeric(factor(champ(r("vc"), "prediction"), m$classes)) else champ(r("vc"), "prediction")),
    "ajustement" = if (!m$classement) list(m$ajuste, r("ajuste")),
    qualite = list(if (m$classement) c(m$oob$erreur, m$classification$vc$global) else c(m$oob$mse, m$oob$r2, m$vc$Q2, m$vc$RMSEcv), unname(r("qualite"))),
    "votes hors sac" = if (m$classement) list(m$votes_oob, r("votes_oob")),
    classements = if (m$classement) list(rbind(m$classification$oob$table, m$classification$vc$table, m$classification$ajuste$table),
                                         if (!is.null(cl)) rbind(champ(cl, "oob", "table"), champ(cl, "vc", "table"), champ(cl, "ajuste", "table"))),
    test = if (!is.null(m$test)) {
      if (m$classement) list(m$test$classement$table, champ(r("test"), "table"))
      else list(unlist(m$test$statistiques[c("n", "RMSEP", "biais", "SEP", "Q2ext")]), unname(r("test")))
    },
    "dépendance partielle" = if (length(m$dependance)) list(unlist(lapply(m$dependance, function(x) c(if (!x$qualitative) x$x, unlist(x$y), unlist(x$probabilites)))),
                                                            unlist(lapply(unname(dep), function(x) c(if (!is.factor(x$x)) x$x, x$y, x[[nom_traduit("probabilites", langue, NOMS_FORET_EN)]])))))
  ecarts_paires(paires)
}

ecrire_script_forest <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  tab <- tableau_script(projet, ids_condition(projet, d))
  noms <- c(NOMS_FORET_EN, NOMS_SCRIPT_EN)
  fc <- plan_condition(projet, d, tab)$fonctions  # (celles de Nouvelle variable qu'appelle sa condition : condition.R)
  fonctions <- traduire_script(c(code_moteur(racines_foret(m), langue, ENTETES_FORET_EN), if (length(fc)) code_fonctions_condition(fc, tx)), langue, noms, elements = TRUE)
  version_ranger <- as.character(utils::packageVersion("ranger"))
  piece <- list(
    quoi = sprintf(tx("forêt aléatoire de %s, %d arbres, %d observations, %d variables X", "random %s forest, %d trees, %d observations, %d X variables"),
                   if (m$classement) tx("classement", "classification") else tx("régression", "regression"), m$reglages_foret$arbres, m$N, m$K),
    tab = tab, fonctions = fonctions,
    calcul = traduire_script(code_calcul_foret(projet, d, m, tab, langue), langue, noms),
    sortie = code_sortie_foret(m, langue), graphiques = code_graphiques_foret(m, langue),
    ecart = function(m, e) ecart_foret(m, e, langue), paquets = "ranger",
    texte_paquets = sprintf(tx("Il refait cette forêt avec R et le paquet ranger (Wright et Ziegler 2017), le seul paquet hors de R dont il a besoin : Nuée en dépend et l'installe avec lui ; ailleurs, install.packages(\"ranger\"). Avec la même graine, un seul fil et les mêmes réglages, c'est la forêt de Nuée, arbre pour arbre, quand la version de ranger est la même (%s ici). Ses fonctions sont celles du moteur de Nuée, recopiées telles quelles ; de ses données à ses résultats, qui sont ceux de Nuée. Ses quatre parties :",
                               "It redoes this forest with R and the ranger package (Wright and Ziegler 2017), the only package beyond R it needs: Nuée depends on it and installs it with itself; elsewhere, install.packages(\"ranger\"). With the same seed, a single thread and the same settings, it is Nuée's forest, tree for tree, when the version of ranger is the same (%s here). Its functions are those of Nuée's engine, copied as they are; from its data to its results, which are those of Nuée. Its four parts:"),
                            version_ranger),
    parties = c(tx("#   1. Fonctions : celles des forêts du moteur de Nuée (ranger)", "#   1. Functions: those of the forests of Nuée's engine (ranger)"),
                tx("#   3. Modèle : X et réponse, forêt, erreur hors sac, validation croisée, importances", "#   3. Model: X and response, forest, out-of-bag error, cross-validation, importances")),
    resultats = Filter(Negate(is.null), list(
      list(nom = "qualite-table", objet = traduire_script("t(qualite)", langue, noms), chiffres = 3),
      list(nom = "importance-table", objet = "importance", chiffres = 4),
      if (m$classement) list(nom = "classement-table", objet = traduire_script("classement$vc$table", langue, noms), chiffres = 0),
      if (!is.null(m$test)) list(nom = "test-table", objet = if (m$classement) "test$table" else "t(test)", chiffres = 3))),
    objets = nom_traduit(c("rf", "X", "y", "importance", "courbe", "vc", "qualite", "classement", "votes_oob", "ajuste", "test", "dependance"), langue, NOMS_FORET_EN))
  ecrire_script_moteur(projet, d, chemin_script, m, piece, donnees, renv, rdata, rproj, version, langue)
}
