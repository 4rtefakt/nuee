# L'export vers R d'arbres boostés (analyses.R : ecrire_script_<type> ; boosting.R) : un script qui
# refait le boosting avec R et rpart (livré avec R), sans Nuée ni autre paquet. Ses fonctions sont
# celles du moteur, recopiées de boosting.R (script_moteur.R : code_moteur) : le gradient boosting
# comme gbm le calcule, chaque arbre fait par rpart sur un tirage des observations, avec la même
# graine, donc les mêmes tirages et les mêmes arbres que Nuée ; puis ses données, ses X (x1, x2…),
# le nombre d'arbres par la validation croisée par les groupes du modèle, le modèle de toutes les
# observations arrêté à ce nombre, l'influence relative, la qualité de l'ajustement, le jeu de test
# et la dépendance partielle quand elle a été calculée ; des graphiques en R de base (les parties
# communes avec les forêts : forets_script.R). Ou un rapport R Markdown ou Quarto. Avant d'être
# écrit, il est exécuté ici et comparé au modèle de l'application (ecart_boost).

# Les noms du script en anglais (traduire_script) : ceux des fonctions de boosting.R, arbres.R,
# forets.R et validation.R, puis ceux du calcul.
NOMS_BOOST_EN <- c(
  avec_graine = "with_seed", initiale_boost = "initial_boost", tirage_sac = "bag_draw", meilleures_coupures = "best_splits",
  arbre_court = "short_tree", feuilles_arbre = "tree_leaves", matrice_codes = "code_matrix", valeurs_feuilles = "leaf_values",
  predire_boost_f = "predict_boost_f", reponse_boost = "response_boost", classement_ensemble = "classification_of",
  classement_test = "test_classification", table_classement = "classification_table", grille_dependance = "dependence_grid",
  donnees_grille = "grid_data", GRAINE_BOOST = "SEED_BOOST", FORMULE_BOOST = "FORMULA_BOOST", POINTS_DEPENDANCE = "DEPENDENCE_POINTS",
  ap = "train_rows", arb = "tree", arbres = "trees", arret = "stop_at", avant = "before", bas = "lower", bien_classees = "correct_share",
  candidats = "candidates", chemin = "path", choisis = "chosen", colonnes = "columns", cote = "side", dans = "in_bag", echecs = "failures",
  ev = "eval_rows", f_arret = "f_stop", feuille = "leaf", feuilles = "leaves", garder = "keep_trees", gauche = "left", genre = "rng_kind",
  graine = "seed", grille = "grid", groupes = "groups", haut = "upper", inconnues = "unseen", inferieur = "below", interne = "internal",
  ligne = "split_row", loi = "distribution", majorite = "majority", meilleur = "best", min_noeud = "min_node", noeud = "node", noeuds = "nodes",
  observee = "observed", positif = "positive", predite = "predicted", predites = "predicted_rows", pris = "taken", profondeur = "depth",
  seuil = "threshold", seuils = "thresholds", somme = "leaf_sum", taux = "rate", termes = "model_terms", tous = "all_nodes",
  valeurs = "values",
  # (le calcul du script)
  noms_x = "x_names", variables_x = "x_variables", reglages = "settings", modele = "model", ajuste_lien = "fitted_link", ajuste = "fitted",
  pourcentage = "percentage", nulle = "null_dev", deviance_nulle = "null_deviance", courbe = "curve", apprentissage = "training", prediction_vc = "cv_prediction",
  qualite = "quality", vote = "votes_of", classement = "classification", complet = "complete", prediction_test = "test_prediction",
  erreur = "error", dependance = "partial_dependence", lien = "link", reponse = "response", y_test = "y_test", X_test = "X_test")

# Le commentaire anglais de chaque définition que le script reprend (en français : le sien, celui du
# moteur ; celles qu'il partage avec les forêts : ENTETES_FORET_EN).
ENTETES_BOOST_EN <- c(
  avec_graine = "Evaluates `expr` after set.seed(seed) with R's default generator (whatever generator the session chose), then gives the generator back its kind and state: a reproducible computation that doesn't change the randomness of the rest of the session.",
  FORMULE_BOOST ="(the formula of each tree, made here: its environment is the global environment, and the trees kept don't hold on to the data of the step that made them)",
  initiale_boost = "f0: the constant of the smallest deviance.",
  gradient_boost = "The negative gradient of the deviance (up to a factor 2) at f.",
  deviance_boost = "The deviance of each observation (y and f of the same length, or f a matrix with as many rows): (y - f)²; -2 [y f - log(1 + exp(f))]; 2 [y log(y / μ) - (y - μ)]. gbm gives the same, except the Poisson one, from which it removes the term that doesn't depend on the model (2 [y log(y) - y]): it can be negative there; the number of trees chosen is the same.",
  reponse_boost = "f on the response scale: the mean, the probability, the mean of the counts.",
  tirage_sac = "The draw of k observations among n, as gbm does it (gbm_engine.cpp, CGBM::iterate): observation i is taken if u_i (n - i + 1) < k - (those already taken), u_i uniform; n random numbers at each tree, even when all are taken.",
  meilleures_coupures = "The `depth` splits of an rpart tree (grown to that depth) that gbm would have made: the best first, each on the leaf where it reduces the sum of squares most (rpart's improve × dev: the reduction, not its share; one row of splits per split, without competing or surrogate splits), the oldest leaf in a tie, as gbm. Returns the nodes split (rpart's numbers: the children of n are 2n and 2n + 1), and the variable and reduction of each split (the relative influence).",
  arbre_court = "The tree made of these splits, in a short form: for each node split (in the order of their numbers, a parent before its children), the column of its variable, its threshold and its direction (rpart: ncat = -1, left if x < threshold; +1, left if x >= threshold), or for a qualitative variable the side of each category (csplit: 1 left, 3 right, 2 absent from the node: on the side with the most observations, as rpart without surrogate variables); its leaves.",
  feuilles_arbre = "The leaf (its number) of each row of Xm (a matrix: the categories by their number).",
  matrice_codes = "The X as a matrix of numbers, a qualitative variable by the number of its category.",
  valeurs_feuilles = "The value of each leaf from the observations drawn (`leaf`: the number of their leaf among the nn; z, y and f: their values).",
  boosting = "A boosting: on the rows `train_rows` of X (simple names, x1, x2…) and y, each row of `eval_rows` predicted after each tree (`path`: one column per tree). The first `keep_trees` trees are kept (short form, with the contribution λ v of each leaf), with the sum of squares reduction of each variable in each; `stop_at`: a number of trees at which to keep f of the rows `train_rows`. rpart receives its data model ready-made (its `model` argument): it doesn't make one again at each tree.",
  validation_boost = "The cross-validation: each group predicted after each tree by the boosting redone without it (same seed); the mean deviance of these predictions by number of trees (as gbm's cv.error: the mean over all the observations predicted), and the number of trees where it is smallest (gbm.perf(method = \"cv\")). A group whose others have a single value of the response stays without a prediction (`failures`).",
  predire_boost_f = "The f of observations (X under the simple names, without missing values, their categories those of the project): f0 and the contribution of each of the model's trees.")

COMMENTAIRES_BOOST_EN <- c(
  "# (le genre de la séance rendu, sans redire ses avertissements)" = "# (the session's kind given back, without repeating its warnings)",
  "# (le fils gauche prend sa place, le droit vient en dernier : l'ordre des feuilles de gbm)" = "# (the left child takes its place, the right one comes last: gbm's order of the leaves)")

racines_boost <- function(m) {
  c("validation_boost", "boosting", "reponse_boost", "initiale_boost", "deviance_boost", if (!is.null(m$test) || length(m$dependance)) "predire_boost_f",
    if (identical(m$loi, "binomiale")) "classement_ensemble", if (length(m$dependance)) c("grille_dependance", "donnees_grille"))
}

code_calcul_boost <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  r <- m$reglages_boost
  lois <- c(gaussienne = tx("normale (la perte quadratique ; f est la moyenne)", "normal (the squared-error loss; f is the mean)"),
            binomiale = tx("binomiale (la déviance de Bernoulli d'une réponse 0/1 ; f est le logit de la probabilité)", "binomial (the Bernoulli deviance of a 0/1 response; f is the logit of the probability)"),
            poisson = tx("de Poisson (la déviance de Poisson de comptages ; f est le log de la moyenne)", "Poisson (the Poisson deviance of counts; f is the log of the mean)"))
  dependance <- code_dependance_ensemble(m, tx,
    c("la valeur moyenne de f (sur l'échelle du lien) sur les observations du modèle quand la variable prend chaque valeur de sa grille, et elle à l'échelle de la réponse.",
      "the mean value of f (on the link scale) over the model's observations when the variable takes each value of its grid, and it on the response scale."),
    c("f <- predire_boost_f(modele, donnees_grille(X, j, grille))",
      "lien <- as.vector(tapply(f, rep(seq_along(grille), each = nrow(X)), mean))",
      "list(x = grille, y = lien, reponse = reponse_boost(lien, reglages$loi))"))
  c(code_lignes_modele(projet, d, m, tab, tx, c("réponse ou une valeur X manquante", "response or an X value missing")), "",
    code_x_ensemble(m, tab, tx), "",
    commentaire(tx(sprintf("La réponse : « %s »%s.", texte_sur(m$y$nom), if (!is.null(m$y$transform)) ", transformée" else ""),
                   sprintf("The response: “%s”%s.", texte_sur(m$y$nom), if (!is.null(m$y$transform)) ", transformed" else ""))),
    code_y_ensemble(m, d, tab, tx), "",
    code_groupes_modele(m, tx), "",
    commentaire(tx(sprintf("Les réglages : la loi %s ; au plus %d arbres, chacun de %d coupures (la profondeur d'interaction), au moins %d observations par feuille, fait sur un tirage sans remise de %s des observations (graine %d : les tirages de Nuée), ajouté avec un taux d'apprentissage de %s. La loi garde son nom dans le moteur : « gaussienne », « binomiale » ou « poisson ».",
                           lois[[r$loi]], as.integer(r$arbres), as.integer(r$profondeur), as.integer(r$min_noeud), pourcent_texte(r$fraction, TRUE), GRAINE_BOOST, format(r$taux, decimal.mark = ",")),
                   sprintf("The settings: the %s distribution; at most %d trees, each of %d splits (the interaction depth), at least %d observations per leaf, grown on a draw without replacement of %s of the observations (seed %d: Nuée's draws), added with a learning rate of %s. The distribution keeps its name in the engine: \"gaussienne\" (normal), \"binomiale\" (binomial) or \"poisson\".",
                           lois[[r$loi]], as.integer(r$arbres), as.integer(r$profondeur), as.integer(r$min_noeud), pourcent_texte(r$fraction, FALSE), GRAINE_BOOST, format(r$taux)))),
    sprintf("reglages <- list(loi = %s, arbres = %d, profondeur = %d, min_noeud = %d, taux = %s, fraction = %s)", code_texte(r$loi), as.integer(r$arbres),
            as.integer(r$profondeur), as.integer(r$min_noeud), texte_exact(r$taux), texte_exact(r$fraction)),
    "",
    commentaire(tx(sprintf("Le nombre d'arbres par la validation croisée par les %d groupes : chaque groupe prédit après chaque arbre par le boosting refait sans lui ; le nombre d'arbres de la plus petite déviance moyenne (une minute ou plus pour beaucoup d'arbres).", m$G),
                   sprintf("The number of trees by the cross-validation by the %d groups: each group predicted after each tree by the boosting redone without it; the number of trees of the smallest mean deviance (a minute or more for many trees).", m$G))),
    "vc <- validation_boost(X, y, groupes_vc, reglages)",
    "meilleur <- vc$meilleur",
    "",
    commentaire(tx("Le modèle : le boosting de toutes les observations, arrêté à ce nombre d'arbres (`modele` : f0 et ses arbres, ce que predire_boost_f prend) ; l'influence relative de chaque variable, la somme des réductions de la somme des carrés de ses coupures, ramenée à 100.",
                   "The model: the boosting of all the observations, stopped at that number of trees (`model`: f0 and its trees, what predict_boost_f takes); the relative influence of each variable, the sum of the sum of squares reductions of its splits, scaled to 100.")),
    "b <- boosting(X, y, seq_along(y), integer(0), reglages, garder = meilleur, arret = meilleur)",
    "modele <- list(f0 = b$f0, arbres = b$arbres)",
    "ajuste_lien <- b$f_arret",
    "ajuste <- reponse_boost(ajuste_lien, reglages$loi)",
    "gains <- colSums(b$gains[seq_len(meilleur), , drop = FALSE])",
    "influence <- data.frame(variable = unname(noms_x), influence = unname(gains), pourcentage = if (sum(gains) > 0) 100 * unname(gains) / sum(gains) else 0)",
    "influence <- influence[order(-influence$influence, seq_along(noms_x)), ]",
    "rownames(influence) <- NULL",
    "",
    commentaire(sprintf(tx("La déviance après chaque arbre, du modèle et en validation croisée ; la part de la déviance expliquée (D², par rapport à celle de la constante f0)%s.",
                           "The deviance after each tree, of the model and in cross-validation; the share of the deviance explained (D², against that of the constant f0)%s."),
                        switch(r$loi, gaussienne = tx(", R², Q², RMSEE et RMSEcv", ", R², Q², RMSEE and RMSEcv"),
                               binomiale = tx(", l'aire sous la courbe ROC et le classement à une probabilité de 0,5", ", the area under the ROC curve and the classification at a probability of 0.5"), ""))),
    "courbe <- data.frame(arbres = seq_len(reglages$arbres), apprentissage = b$deviance, validation = vc$deviance)",
    "nulle <- mean(deviance_boost(y, rep(initiale_boost(y, reglages$loi), length(y)), reglages$loi))",
    "prediction_vc <- reponse_boost(vc$chemin[, meilleur], reglages$loi)",
    "ok <- vc$predites",
    "qualite <- c(D2 = 1 - b$deviance[meilleur] / nulle, D2_vc = 1 - vc$deviance[meilleur] / nulle, deviance = b$deviance[meilleur],",
    "             deviance_vc = vc$deviance[meilleur], deviance_nulle = nulle)",
    if (r$loi == "gaussienne") c(
      "qualite <- c(qualite, r2 = 1 - sum((y - ajuste)^2) / sum((y - mean(y))^2), Q2 = 1 - sum((y[ok] - prediction_vc[ok])^2) / sum((y[ok] - mean(y))^2),",
      "             RMSEE = sqrt(mean((y - ajuste)^2)), RMSEcv = sqrt(mean((y[ok] - prediction_vc[ok])^2)))"),
    if (r$loi == "binomiale") c(
      "classes <- c(\"0\", \"1\")",
      "vote <- function(p) cbind(`0` = 1 - p, `1` = p)",
      "classement <- list(ajuste = classement_ensemble(y, ifelse(ajuste >= 0.5, \"1\", \"0\"), vote(ajuste), classes),",
      "                   vc = classement_ensemble(y, ifelse(is.na(prediction_vc), NA, ifelse(prediction_vc >= 0.5, \"1\", \"0\")), vote(prediction_vc), classes))",
      "qualite <- c(qualite, auc = classement$ajuste$roc[[2]]$auc, auc_vc = classement$vc$roc[[2]]$auc)"),
    if (!is.null(m$test)) c("", code_test_regression(m, d, tab, tx, "reponse_boost(predire_boost_f(modele, X_test[complet, , drop = FALSE]), reglages$loi)")),
    if (!is.null(dependance)) c("", dependance))
}

code_sortie_boost <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  ligne <- function(code, fr, en) ligne_sortie(traduire_script(code, langue, c(NOMS_BOOST_EN, NOMS_SCRIPT_EN)), tx(fr, en))
  c(ligne("print(meilleur)", "le nombre d'arbres retenu par la validation croisée", "the number of trees retained by the cross-validation"),
    ligne("print(qualite)", "la qualité de l'ajustement et de la validation croisée", "the quality of the fit and of the cross-validation"),
    ligne("print(influence)", "l'influence relative de chaque variable (%)", "the relative influence of each variable (%)"),
    if (identical(m$loi, "binomiale")) ligne("print(classement$vc$table)", "classement en validation croisée, à 0,5 (observées en lignes)", "cross-validated classification, at 0.5 (observed in rows)"),
    if (!is.null(m$test)) ligne("print(test)", "jeu de test", "test set"),
    if (length(m$dependance)) ligne("str(dependance)", "la dépendance partielle", "the partial dependence"))
}

code_graphiques_boost <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  t <- function(code) traduire_script(code, langue, c(NOMS_BOOST_EN, NOMS_SCRIPT_EN))
  lapply(Filter(Negate(is.null), list(
    list(nom = "influence", largeur = 7, hauteur = 5, code = c(
      "op <- par(mar = c(4, 9, 2, 1))",
      sprintf("barplot(rev(influence$pourcentage), names.arg = rev(influence$variable), horiz = TRUE, las = 1, xlab = %s, main = %s)",
              code_texte(tx("Influence relative (%)", "Relative influence (%)")), code_texte(texte_sur(m$y$nom))),
      "par(op)")),
    list(nom = "deviance", largeur = 7, hauteur = 4.5, code = c(
      sprintf("matplot(courbe$arbres, cbind(courbe$apprentissage, courbe$validation), type = \"l\", lty = 1, col = c(\"#2f6fb5\", \"#d1495b\"), xlab = %s, ylab = %s, main = %s)",
              code_texte(tx("Arbres", "Trees")), code_texte(tx("Déviance moyenne", "Mean deviance")), code_texte(tx("Déviance selon le nombre d'arbres", "Deviance by the number of trees"))),
      "abline(v = meilleur, lty = 3)",
      sprintf("legend(\"topright\", c(%s, %s), lty = 1, col = c(\"#2f6fb5\", \"#d1495b\"), bty = \"n\")",
              code_texte(tx("ajustement", "fit")), code_texte(tx("validation croisée", "cross-validation"))))),
    if (!identical(m$loi, "binomiale")) list(nom = "observe-predit", largeur = 6, hauteur = 6, code = c(
      sprintf("plot(y, ajuste, pch = 19, col = \"#2f6fb580\", xlab = %s, ylab = %s)", code_texte(tx("observé", "observed")), code_texte(tx("ajusté, prédit", "fitted, predicted"))),
      "points(y, prediction_vc, pch = 1, col = \"#d1495b\")",
      "abline(0, 1, lty = 3)",
      sprintf("legend(\"topleft\", c(%s, %s), pch = c(19, 1), col = c(\"#2f6fb5\", \"#d1495b\"), bty = \"n\")",
              code_texte(tx("ajusté", "fitted")), code_texte(tx("validation croisée", "cross-validation"))))),
    graphique_dependance(m, tx, "dependance[[v]]$y", tx("f moyen (échelle du lien)", "mean f (link scale)")))),
    function(g) { g$code <- t(g$code); g })
}

ecart_boost <- function(m, e, langue) {
  r <- function(x) get0(nom_traduit(x, langue, NOMS_BOOST_EN), envir = e, inherits = FALSE)
  champ <- function(x, ...) { for (k in c(...)) x <- if (!is.null(x)) x[[nom_traduit(k, langue, NOMS_BOOST_EN)]]; x }
  vc <- r("vc")
  cl <- r("classement")
  dep <- r("dependance")
  paires <- list(
    "déviance selon les arbres" = list(as.matrix(m$courbe[c("apprentissage", "validation")]), if (!is.null(r("courbe"))) as.matrix(r("courbe")[2:3])),
    "nombre d'arbres" = list(m$meilleur, r("meilleur")),
    ajustement = list(m$ajuste_lien, r("ajuste_lien")),
    "validation croisée" = list(m$vc$lien, if (!is.null(vc)) champ(vc, "chemin")[, m$meilleur]),
    influence = list(m$influence$influence, champ(r("influence"), "influence")),
    "influence relative" = list(m$influence$pourcentage, champ(r("influence"), "pourcentage")),
    qualite = list(unlist(m$qualite), unname(r("qualite"))),
    classement = if (identical(m$loi, "binomiale")) list(rbind(m$classification$ajuste$table, m$classification$vc$table),
                                                        if (!is.null(cl)) rbind(champ(cl, "ajuste", "table"), champ(cl, "vc", "table"))),
    test = if (!is.null(m$test)) list(unlist(m$test$statistiques[c("n", "RMSEP", "biais", "SEP", "Q2ext")]), unname(r("test"))),
    "dépendance partielle" = if (length(m$dependance)) list(unlist(lapply(m$dependance, function(x) c(if (!x$qualitative) x$x, unlist(x$y), unlist(x$reponse)))),
                                                            unlist(lapply(unname(dep), function(x) c(if (!is.factor(x$x)) x$x, x$y, x[[nom_traduit("reponse", langue, NOMS_BOOST_EN)]])))))
  ecarts_paires(paires)
}

ecrire_script_boost <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  tab <- tableau_script(projet, ids_condition(projet, d))
  noms <- c(NOMS_BOOST_EN, NOMS_SCRIPT_EN)
  fc <- plan_condition(projet, d, tab)$fonctions  # (celles de Nouvelle variable qu'appelle sa condition : condition.R)
  fonctions <- traduire_script(c(code_moteur(racines_boost(m), langue, c(ENTETES_BOOST_EN, ENTETES_FORET_EN), COMMENTAIRES_BOOST_EN),
                                 if (length(fc)) code_fonctions_condition(fc, tx)), langue, noms, elements = TRUE)
  lois <- c(gaussienne = tx("loi normale", "normal distribution"), binomiale = tx("loi binomiale", "binomial distribution"), poisson = tx("loi de Poisson", "Poisson distribution"))
  piece <- list(
    quoi = sprintf(tx("arbres boostés, %s, %d arbres sur %d au plus, %d observations, %d variables X", "boosted trees, %s, %d trees of at most %d, %d observations, %d X variables"),
                   lois[[m$loi]], m$meilleur, as.integer(m$reglages_boost$arbres), m$N, m$K),
    tab = tab, fonctions = fonctions,
    calcul = traduire_script(code_calcul_boost(projet, d, m, tab, langue), langue, noms),
    sortie = code_sortie_boost(m, langue), graphiques = code_graphiques_boost(m, langue),
    ecart = function(m, e) ecart_boost(m, e, langue), paquets = "rpart",
    texte_paquets = tx("Il refait ce modèle avec R et son paquet rpart (livré avec R), sans Nuée ni autre paquet : ses fonctions sont celles du moteur de Nuée, recopiées telles quelles (le gradient boosting comme le paquet gbm le calcule, écrit sur rpart) ; avec la même graine, les tirages et les arbres de Nuée, de ses données à ses résultats, qui sont ceux de Nuée. Ses quatre parties :",
                       "It redoes this model with R and its package rpart (it comes with R), without Nuée or any other package: its functions are those of Nuée's engine, copied as they are (gradient boosting as the gbm package computes it, written on rpart); with the same seed, Nuée's draws and trees, from its data to its results, which are those of Nuée. Its four parts:"),
    parties = c(tx("#   1. Fonctions : celles du boosting du moteur de Nuée", "#   1. Functions: those of the boosting of Nuée's engine"),
                tx("#   3. Modèle : X et réponse, nombre d'arbres par validation croisée, modèle, influence, qualité", "#   3. Model: X and response, number of trees by cross-validation, model, influence, quality")),
    resultats = Filter(Negate(is.null), list(
      list(nom = "qualite-table", objet = traduire_script("t(qualite)", langue, noms), chiffres = 3),
      list(nom = "influence-table", objet = "influence", chiffres = 2),
      if (identical(m$loi, "binomiale")) list(nom = "classement-table", objet = traduire_script("classement$vc$table", langue, noms), chiffres = 0),
      if (!is.null(m$test)) list(nom = "test-table", objet = "t(test)", chiffres = 3))),
    objets = nom_traduit(c("X", "y", "reglages", "vc", "meilleur", "b", "modele", "ajuste", "influence", "courbe", "qualite", "classement", "test", "dependance"), langue, NOMS_BOOST_EN))
  ecrire_script_moteur(projet, d, chemin_script, m, piece, donnees, renv, rdata, rproj, version, langue)
}
