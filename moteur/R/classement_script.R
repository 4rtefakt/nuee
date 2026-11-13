# L'export vers R d'un arbre de décision, d'une LDA ou de k-moyennes (analyses.R :
# ecrire_script_<type>) : un script qui refait l'analyse avec R et le paquet qu'elle utilise
# (rpart, MASS, cluster : livrés avec R), ses données, ses variables comme le moteur les lit
# (onglet Variables), le calcul, ses résultats et des graphiques en R de base ; ou un rapport R
# Markdown ou Quarto qui y ajoute le texte des méthodes et les références. Avant d'être écrit, son
# code est exécuté ici, sans le moteur, et ses résultats comparés à ceux du modèle de
# l'application : un écart est renvoyé dans `ecart`.
#
# Le code est écrit avec des noms français, traduits en anglais pour un script en anglais
# (NOMS_CLASSEMENT_EN, puis ceux de script.R).

NOMS_CLASSEMENT_EN <- c(
  arbre_elague = "pruned_tree", plis = "folds", controle = "tree_control", regle = "rule", arbre = "tree", proba_arbre = "tree_probabilities",
  validation_externe = "external_validation", prediction = "prediction", groupes = "groups", regles_feuilles = "leaf_rules", feuilles = "leaves",
  feuille = "leaf", valeur = "value", premiere_coupure = "first_split", premiere = "first", seuil = "threshold", stabilite = "stability",
  premieres = "first_splits", quoi = "which_var", seuil_median = "median_threshold", frequence = "frequency", us = "drawn_from",
  ss = "drawn", variables_x = "x_variables", importance = "importance", pourcentage = "percentage", elagage = "pruning",
  qualite = "quality", classement = "classification", classement_vc = "classification_cv", classe_ajustee = "class_fitted",
  bien_classees = "correct", bien_classees_vc = "correct_cv", proba = "probabilities", resultat = "result", tirage = "draw",
  prediction_test = "test_prediction", classe_test = "test_class", X_test = "X_test", y_test = "y_test",
  colonnes_utiles = "useful_columns", lda_reduite = "lda_standardized", probabilites_lda = "lda_probabilities", et = "sds",
  priors = "priors", Z = "Z", validation_lda = "lda_cross_validation", ca = "classes_left", scores = "scores", ok = "ok",
  coefficients = "coefficients", coefficients_std = "coefficients_std", structure_lda = "structure_correlations", part = "share",
  regles = "rules", niveaux = "class_levels", classement_test = "test_classification", proba_test = "test_probabilities",
  moyennes = "class_means", utiles = "useful", echelle_terme = "scale_term", kmeans_ordonne = "kmeans_by_size", depart = "start",
  tailles = "sizes", intra = "within", inter = "between", totale = "total", r2 = "r2", centres = "centres",
  centres_unites = "centres_units", echelles = "scalings", ponderations = "weightings", bruts = "raw", sil = "silhouette_widths",
  acp = "pca", choix = "k_choice", distances = "distances", plan = "plane", k = "k", couleurs = "colours", resultats = "results")

# Les noms du code en anglais pour un script en anglais (comme traduire_ordination, avec ceux
# de ce fichier).
traduire_classement <- function(lignes, langue) {
  lignes <- unlist(lapply(lignes, function(l) if (grepl("\n", l, fixed = TRUE)) strsplit(l, "\n", fixed = TRUE)[[1]] else l))
  if (!length(lignes) || !identical(langue, "en")) return(lignes)
  noms <- c(NOMS_CLASSEMENT_EN, NOMS_SCRIPT_EN)
  pd <- utils::getParseData(parse(text = lignes, keep.source = TRUE, encoding = "UTF-8"), includeText = TRUE)
  pd <- pd[pd$terminal & pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB", "SYMBOL_FORMALS") & pd$text %in% names(noms), ]
  pd <- pd[order(pd$line1, -pd$col1), ]
  for (i in seq_len(nrow(pd))) {
    l <- lignes[pd$line1[i]]
    lignes[pd$line1[i]] <- paste0(substr(l, 1, pd$col1[i] - 1), noms[[pd$text[i]]], substr(l, pd$col2[i] + 1, nchar(l)))
  }
  lignes
}
nom_classement <- function(x, langue) {
  if (!identical(langue, "en")) return(x)
  vapply(x, function(n) if (n %in% names(NOMS_CLASSEMENT_EN)) NOMS_CLASSEMENT_EN[[n]] else nom_script(n, langue), "", USE.NAMES = FALSE)
}

# Les lignes du modèle (et du jeu de test) dans le tableau du script. Un modèle à condition les
# choisit par elle (condition.R : code_condition), puis écarte celles que le moteur a laissées de
# côté faute de valeurs.
code_lignes_modele <- function(projet, d, m, tab, tx, quoi) {
  rangs <- unname(tab$rangs[as.character(m$observations)])
  test <- unname(tab$rangs[as.character(m$test$observations$uid %||% numeric(0))])
  exclues <- length(m$exclues)
  pourquoi <- if (exclues) tx(sprintf(" (%d autre%s laissée%s de côté, %s)", exclues, if (exclues > 1) "s" else "", if (exclues > 1) "s" else "", quoi[1]),
                               sprintf(" (%d other%s left out, %s)", exclues, if (exclues > 1) "s" else "", quoi[2])) else ""
  pc <- plan_condition(projet, d, tab)
  lignes <- if (!is.null(pc)) {
    c(code_condition(pc, tx, tx(sprintf("Les %d observations du modèle%s.", length(rangs), pourquoi), sprintf("The %d observations of the model%s.", length(rangs), pourquoi))),
      if (exclues) c(commentaire(tx(sprintf("Sans celles que le modèle laisse de côté (%s).", quoi[1]), sprintf("Without those the model leaves out (%s).", quoi[2]))),
                     code_valeurs(morceaux_rangs(unname(tab$rangs[as.character(m$exclues)])), "lignes <- setdiff(lignes, ", ")")),
      "d <- donnees[lignes, ]")
  } else if (!length(test) && identical(as.integer(rangs), seq_len(nrow(tab$donnees)))) {
    c(commentaire(tx(sprintf("Les %d observations du tableau%s.", length(rangs), pourquoi), sprintf("The %d observations of the table%s.", length(rangs), pourquoi))), "d <- donnees")
  } else {
    c(commentaire(tx(sprintf("Les %d observations du modèle%s : leurs rangs dans le tableau.", length(rangs), pourquoi),
                     sprintf("The %d observations of the model%s: their rows in the table.", length(rangs), pourquoi))),
      code_valeurs(morceaux_rangs(rangs), "lignes <- "), "d <- donnees[lignes, ]")
  }
  if (length(test)) {
    lignes <- c(lignes, commentaire(tx(sprintf("Les %d observations du jeu de test, laissées hors du modèle et de la validation croisée.", length(test)),
                                       sprintf("The %d observations of the test set, left out of the model and of the cross-validation.", length(test)))),
                code_valeurs(morceaux_rangs(test), "lignes_test <- "))
  }
  lignes
}

# Le groupe de validation croisée de chaque observation du modèle.
code_groupes_modele <- function(m, tx) {
  c(commentaire(tx(sprintf("Le groupe de validation croisée de chaque observation%s (%d groupes).",
                           if (nzchar(m$vc_par %||% "")) sprintf(" : un par valeur de « %s »", texte_sur(m$vc_par)) else ", à tour de rôle", m$G),
                   sprintf("The cross-validation group of each observation%s (%d groups).",
                           if (nzchar(m$vc_par %||% "")) sprintf(": one per value of “%s”", texte_sur(m$vc_par)) else ", in turn", m$G))),
    code_groupes_vc(m$groupes))
}

# Le code d'une fonction de `d` qui rend les termes X bruts d'un bloc (code_bloc_ordination).
code_fonction_bloc <- function(projet, termes, obs, tab, langue, nom = "variables_x") {
  code <- code_bloc_ordination(projet, termes, obs, tab, "X", langue)
  explication <- code[startsWith(code, "#")]
  c(explication, sprintf("%s <- function(d) {", nom), paste0("  ", code[!startsWith(code, "#")]), "  X", "}")
}

# ---------------------------------------------------------------- les arbres

code_fonctions_arbre <- function(m, tx) {
  aucune <- code_texte(tx("(aucune coupure)", "(no split)"))
  c(commentaire(tx("Un arbre grand, puis élagué par sa validation croisée interne. `plis`, le groupe de chaque ligne, va DANS le contrôle : donné à côté d'un argument control, xval serait remplacé par les 10 groupes tirés au hasard de ce contrôle. Règle « 1se » : le plus petit arbre dont l'erreur de validation croisée ne dépasse pas l'erreur minimale de plus d'une erreur-type ; « min » : celui de l'erreur minimale. Un arbre de classement n'a que les classes de ses lignes.",
                   "A large tree, then pruned by its internal cross-validation. `folds`, the group of each row, goes INSIDE the control: given next to a control argument, xval would be replaced by the 10 random groups of that control. Rule \"1se\": the smallest tree whose cross-validated error is within one standard error of the minimum; \"min\": that of the minimum error. A classification tree has only the classes of its rows.")),
    "arbre_elague <- function(cadre, plis, methode, controle, regle = \"1se\") {",
    "  if (methode == \"class\") cadre$.y <- droplevels(cadre$.y)",
    "  controle$xval <- as.integer(factor(plis))",
    "  complet <- rpart::rpart(.y ~ ., data = cadre, method = methode, control = controle)",
    "  ct <- complet$cptable",
    "  i <- which.min(ct[, \"xerror\"])",
    "  cp <- if (regle == \"min\") ct[i, \"CP\"] else ct[which(ct[, \"xerror\"] <= ct[i, \"xerror\"] + ct[i, \"xstd\"])[1], \"CP\"]",
    "  list(complet = complet, arbre = rpart::prune(complet, cp = cp), cp = unname(cp))",
    "}",
    "",
    if (m$classement) c(
      commentaire(tx("La probabilité de chaque classe (0 pour une classe que l'arbre n'a pas vue).", "The probability of each class (0 for a class the tree has not seen).")),
      "proba_arbre <- function(arbre, cadre, classes) {",
      "  P <- matrix(0, nrow(cadre), length(classes), dimnames = list(NULL, classes))",
      "  p <- predict(arbre, cadre, type = \"prob\")",
      "  k <- intersect(colnames(p), classes)",
      "  P[, k] <- p[, k, drop = FALSE]",
      "  P",
      "}",
      ""),
    commentaire(tx("La validation croisée externe : chaque groupe prédit par un arbre refait sans lui, sa validation croisée interne (par les autres groupes) et son élagage compris.",
                   "The external cross-validation: each group predicted by a tree rebuilt without it, its internal cross-validation (by the other groups) and pruning included.")),
    "validation_externe <- function(cadre, groupes, methode, controle, regle = \"1se\") {",
    "  classes <- if (methode == \"class\") levels(cadre$.y)",
    "  prediction <- if (is.null(classes)) rep(NA_real_, nrow(cadre)) else matrix(NA_real_, nrow(cadre), length(classes), dimnames = list(NULL, classes))",
    "  for (g in unique(groupes)) {",
    "    k <- groupes == g",
    "    arbre <- arbre_elague(cadre[!k, , drop = FALSE], groupes[!k], methode, controle, regle)$arbre",
    "    if (is.null(classes)) prediction[k] <- predict(arbre, cadre[k, , drop = FALSE])",
    "    else prediction[k, ] <- proba_arbre(arbre, cadre[k, , drop = FALSE], classes)",
    "  }",
    "  prediction",
    "}",
    "",
    commentaire(tx("Les feuilles : effectif, valeur (moyenne de la réponse, ou classe) et règle (le chemin depuis la racine).",
                   "The leaves: size, value (mean of the response, or class) and rule (the path from the root).")),
    "regles_feuilles <- function(arbre) {",
    "  f <- arbre$frame",
    "  feuilles <- which(f$var == \"<leaf>\")",
    sprintf("  regle <- if (nrow(f) == 1) %s else", aucune),
    "    sapply(rpart::path.rpart(arbre, nodes = as.numeric(rownames(f)[feuilles]), print.it = FALSE), function(x) paste(x[-1], collapse = \" & \"))",
    "  niveaux <- attr(arbre, \"ylevels\")",
    "  data.frame(feuille = seq_along(feuilles), n = f$n[feuilles], valeur = if (is.null(niveaux)) f$yval[feuilles] else niveaux[f$yval[feuilles]],",
    "             regle = unname(regle))",
    "}",
    "",
    commentaire(tx("La première coupure (celle de la racine) : sa variable et son seuil (pour une variable quantitative).",
                   "The first split (that of the root): its variable and threshold (for a quantitative variable).")),
    "premiere_coupure <- function(arbre) {",
    "  if (nrow(arbre$frame) == 1) return(list(variable = NA_character_, seuil = NA_real_))",
    "  s <- arbre$splits[1, ]",
    "  list(variable = as.character(arbre$frame$var[1]), seuil = if (abs(s[[\"ncat\"]]) == 1) unname(s[[\"index\"]]) else NA_real_)",
    "}",
    if (!is.null(m$stabilite)) c(
      "",
      commentaire(tx("La stabilité de la première coupure : des tirages bootstrap des groupes (avec remise ; chaque groupe tiré est un pli de la validation croisée interne), l'arbre refait et élagué sur chacun ; pour chaque variable, le nombre et la part des tirages où elle fait la première coupure, et son seuil médian.",
                     "The stability of the first split: bootstrap draws of the groups (with replacement; each group drawn is a fold of the internal cross-validation), the tree rebuilt and pruned on each; for each variable, the number and share of the draws where it makes the first split, and its median threshold.")),
      "stabilite <- function(cadre, groupes, methode, controle, regle = \"1se\", tirages = 200, graine = 2026) {",
      "  set.seed(graine, kind = \"Mersenne-Twister\", normal.kind = \"Inversion\", sample.kind = \"Rejection\")",
      "  us <- unique(groupes)",
      "  premieres <- lapply(seq_len(tirages), function(tirage) {",
      "    ss <- us[sample.int(length(us), length(us), replace = TRUE)]",
      "    k <- unlist(lapply(ss, function(s) which(groupes == s)))",
      "    plis <- rep(seq_along(ss), vapply(ss, function(s) sum(groupes == s), 0L))",
      "    arbre <- tryCatch(arbre_elague(cadre[k, , drop = FALSE], plis, methode, controle, regle)$arbre, error = function(e) NULL)",
      "    if (is.null(arbre)) return(list(variable = NA_character_, seuil = NA_real_))",
      "    p <- premiere_coupure(arbre)",
      sprintf("    if (is.na(p$variable)) p$variable <- %s", aucune),
      "    p",
      "  })",
      "  v <- vapply(premieres, function(p) p$variable, \"\")",
      "  s <- vapply(premieres, function(p) as.numeric(p$seuil), 0)",
      "  quoi <- sort(unique(v[!is.na(v)]), method = \"radix\")",
      "  n <- vapply(quoi, function(x) sum(v == x, na.rm = TRUE), 0)",
      "  out <- data.frame(variable = quoi, tirages = n, frequence = 100 * n / tirages,",
      "                    seuil_median = vapply(quoi, function(x) median(s[!is.na(v) & v == x], na.rm = TRUE), 0), row.names = NULL)",
      "  out <- out[order(-out$tirages), ]",
      "  rownames(out) <- NULL",
      "  out",
      "}"))
}

# Les X bruts d'un arbre (une fonction de `d`) : une colonne par variable, transformée s'il y a
# lieu ; une variable qualitative reste un facteur.
code_x_arbre <- function(m, tab, tx) {
  valeurs <- vapply(m$variables, function(v) {
    col <- sprintf("d[[%s]]", code_texte(tab$noms[[as.character(v$uid)]]))
    sprintf("  %s = %s,", code_texte(v$colonne), if (!is.null(v$transform)) code_transformation(v$transform, col, TRUE) else col)
  }, "")
  transformees <- vapply(Filter(function(v) !is.null(v$transform), m$variables), `[[`, "", "nom")
  c(commentaire(tx(sprintf("Les variables X, par leurs valeurs brutes (les seuils de l'arbre se lisent dans leurs unités)%s ; une variable qualitative reste un facteur, ses valeurs manquantes restent manquantes (rpart les prend en charge par des variables de substitution).",
                           if (length(transformees)) sprintf(", transformées pour %s", texte_sur(paste(transformees, collapse = ", "))) else ""),
                   sprintf("The X variables, by their raw values (the tree's thresholds read in their units)%s; a qualitative variable stays a factor, missing values stay missing (rpart handles them with surrogate variables).",
                           if (length(transformees)) sprintf(", transformed for %s", texte_sur(paste(transformees, collapse = ", "))) else ""))),
    "variables_x <- function(d) data.frame(", valeurs, "  check.names = FALSE)")
}

code_calcul_arbre <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  r <- m$reglages_arbre
  methode <- if (m$classement) "class" else "anova"
  col_y <- tab$noms[[as.character(if (m$classement) d$classe else d$y[[1]]$variables[1])]]
  ycode <- sprintf("d[[%s]]", code_texte(col_y))
  reponse <- if (m$classement) c(commentaire(tx(sprintf("La réponse : la classe « %s » (ses classes présentes).", texte_sur(m$y$nom)), sprintf("The response: the class “%s” (its classes present).", texte_sur(m$y$nom)))),
                                 sprintf("y <- droplevels(%s)", ycode), "classes <- levels(y)")
             else c(commentaire(tx(sprintf("La réponse : « %s »%s.", texte_sur(m$y$nom), if (!is.null(m$y$transform)) ", transformée" else ""),
                                   sprintf("The response: “%s”%s.", texte_sur(m$y$nom), if (!is.null(m$y$transform)) ", transformed" else ""))),
                    sprintf("y <- %s", if (!is.null(m$y$transform)) code_transformation(m$y$transform, ycode, TRUE) else ycode))
  regle <- code_texte(r$elagage)
  mb <- m$controle$minbucket  # (au plus le sixième des observations : arbres.R, minimum_feuille)
  arguments <- sprintf("\"%s\", controle, %s", methode, regle)
  test <- !is.null(m$test)
  c(code_lignes_modele(projet, d, m, tab, tx, c("réponse ou toutes les valeurs X manquantes", "response or all X values missing")), "",
    code_x_arbre(m, tab, tx), "X <- variables_x(d)", "",
    reponse, "",
    code_groupes_modele(m, tx), "",
    commentaire(tx(sprintf("L'arbre : au moins %d observations par feuille%s, au plus %d niveaux, cp = %s, au plus %d variables de substitution par coupure ; élagué par la règle %s.",
                           mb, if (mb < r$minbucket) sprintf(" (%d demandées, ramenées au sixième des %d observations pour que l'arbre puisse se couper)", r$minbucket, m$N) else "",
                           r$maxdepth, format(r$cp), r$maxsurrogate, if (r$elagage == "1se") "1-ET (1se)" else "de l'erreur minimale (min)"),
                   sprintf("The tree: at least %d observations per leaf%s, at most %d levels, cp = %s, at most %d surrogate variables per split; pruned by the %s rule.",
                           mb, if (mb < r$minbucket) sprintf(" (%d requested, lowered to a sixth of the %d observations so that the tree can split)", r$minbucket, m$N) else "",
                           r$maxdepth, format(r$cp), r$maxsurrogate, if (r$elagage == "1se") "1-SE (1se)" else "minimum error (min)"))),
    "cadre <- data.frame(.y = y, X, check.names = FALSE)",
    sprintf("controle <- rpart::rpart.control(minbucket = %d, cp = %s, maxdepth = %d, maxsurrogate = %d)", mb, texte_exact(r$cp), r$maxdepth, r$maxsurrogate),
    sprintf("resultat <- arbre_elague(cadre, groupes_vc, %s)", arguments),
    "arbre <- resultat$arbre",
    "elagage <- resultat$complet$cptable",
    "regles <- regles_feuilles(arbre)",
    "importance <- data.frame(variable = names(arbre$variable.importance), importance = unname(arbre$variable.importance), row.names = NULL)",
    "importance$pourcentage <- 100 * importance$importance / sum(importance$importance)",
    "premiere <- premiere_coupure(arbre)",
    "",
    commentaire(tx(sprintf("La validation croisée externe, par les %d groupes (quelques secondes).", m$G), sprintf("The external cross-validation, by the %d groups (a few seconds).", m$G))),
    sprintf("vc <- validation_externe(cadre, groupes_vc, %s)", arguments),
    if (m$classement) c(
      "proba <- proba_arbre(arbre, cadre, classes)",
      "classe_ajustee <- factor(classes[max.col(proba, ties.method = \"first\")], classes)",
      "classe_vc <- factor(classes[max.col(vc, ties.method = \"first\")], classes)",
      "classement <- table(observee = y, predite = classe_ajustee)",
      "classement_vc <- table(observee = y, predite = classe_vc)",
      "qualite <- c(bien_classees = mean(classe_ajustee == y), bien_classees_vc = mean(classe_vc == y))")
    else c(
      "ajuste <- unname(predict(arbre))",
      "qualite <- c(R2 = 1 - sum((y - ajuste)^2) / sum((y - mean(y))^2), Q2 = 1 - sum((y - vc)^2) / sum((y - mean(y))^2),",
      "             RMSEcv = sqrt(mean((y - vc)^2)))"),
    if (!is.null(m$stabilite)) c("",
      commentaire(tx(sprintf("La stabilité de la première coupure sur %d tirages bootstrap des groupes (graine 2026 : les mêmes que Nuée ; une minute ou plus).", r$tirages),
                     sprintf("The stability of the first split over %d bootstrap draws of the groups (seed 2026: the same as Nuée's; a minute or more).", r$tirages))),
      sprintf("stab <- stabilite(cadre, groupes_vc, %s, tirages = %d)", arguments, r$tirages)),
    if (test) c("",
      commentaire(tx("Le jeu de test, prédit par l'arbre (une observation sans aucune valeur X reste sans prédiction).", "The test set, predicted by the tree (an observation without any X value stays without a prediction).")),
      "X_test <- variables_x(donnees[lignes_test, ])",
      if (m$classement) c(sprintf("y_test <- factor(donnees[lignes_test, %s], classes)", code_texte(col_y)),
                          "proba_test <- proba_arbre(arbre, X_test, classes)",
                          "classe_test <- factor(classes[max.col(proba_test, ties.method = \"first\")], classes)",
                          "classe_test[rowSums(!is.na(X_test)) == 0] <- NA",
                          "classement_test <- table(observee = y_test, predite = classe_test)")
      else c(sprintf("y_test <- %s", if (!is.null(m$y$transform)) code_transformation(m$y$transform, sprintf("donnees[lignes_test, %s]", code_texte(col_y)), TRUE) else sprintf("donnees[lignes_test, %s]", code_texte(col_y))),
             "prediction_test <- unname(predict(arbre, X_test))",
             "prediction_test[rowSums(!is.na(X_test)) == 0] <- NA",
             "erreur <- prediction_test - y_test",
             "test <- c(RMSEP = sqrt(mean(erreur^2, na.rm = TRUE)), biais = mean(erreur, na.rm = TRUE),",
             "          Q2ext = 1 - sum(erreur^2, na.rm = TRUE) / sum((y_test[!is.na(erreur)] - mean(y))^2))")))
}

code_sortie_arbre <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  ligne <- function(code, fr, en) {
    code <- traduire_classement(code, langue)
    paste0(formatC(code, width = -max(34, nchar(code) + 2)), "# ", tx(fr, en))
  }
  c(ligne("print(elagage)", "table d'élagage : cp, coupures, erreurs relative et de validation croisée", "pruning table: cp, splits, relative and cross-validated errors"),
    ligne("print(arbre)", "l'arbre retenu, nœud par nœud", "the tree retained, node by node"),
    ligne("print(regles)", "les règles des feuilles", "the rules of the leaves"),
    ligne("print(importance)", "l'importance des variables (coupures et substitutions)", "the variable importance (splits and surrogates)"),
    ligne("print(unlist(premiere))", "la première coupure", "the first split"),
    ligne("print(qualite)", if (m$classement) "bien classées, en validation croisée" else "R², Q² et RMSEcv", if (m$classement) "correctly classified, in cross-validation" else "R², Q² and RMSEcv"),
    if (m$classement) ligne("print(classement_vc)", "classement en validation croisée", "cross-validated classification"),
    if (!is.null(m$stabilite)) ligne("print(stab)", "stabilité de la première coupure", "stability of the first split"),
    if (!is.null(m$test)) ligne(if (m$classement) "print(classement_test)" else "print(test)", "jeu de test", "test set"))
}

code_graphiques_arbre <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  t <- function(code) traduire_classement(code, langue)
  c(list(list(nom = "arbre", largeur = 9, hauteur = 6, code = t(c(
      "if (nrow(arbre$frame) > 1) {",
      "  plot(arbre, uniform = TRUE, margin = 0.08)",
      "  text(arbre, use.n = TRUE, cex = 0.75, digits = 3)",
      sprintf("} else plot.new()  # %s", tx("(un arbre sans coupure)", "(a tree without a split)")),
      sprintf("title(%s)", code_texte(tx(sprintf("%s : %d feuilles", texte_sur(m$y$nom), sum(m$ajustement$frame$var == "<leaf>")),
                                         sprintf("%s: %d leaves", texte_sur(m$y$nom), sum(m$ajustement$frame$var == "<leaf>")))))))),
    list(nom = "elagage", largeur = 7, hauteur = 5, code = t(c(
      sprintf("rpart::plotcp(resultat$complet)  # %s", tx("(pointillé : l'erreur minimale plus son erreur-type)", "(dotted: the minimum error plus its standard error)")))))),
    if (!m$classement) list(list(nom = "observe-predit", largeur = 6, hauteur = 6, code = t(c(
      sprintf("plot(y, ajuste, xlab = %s, ylab = %s, pch = 19, col = \"#2f6fb580\")", code_texte(tx("observé", "observed")), code_texte(tx("ajusté (moyenne de la feuille)", "fitted (mean of the leaf)"))),
      "points(y, vc, pch = 1, col = \"#d1495b\")",
      "abline(0, 1, lty = 3)",
      sprintf("legend(\"topleft\", c(%s, %s), pch = c(19, 1), col = c(\"#2f6fb5\", \"#d1495b\"), bty = \"n\")",
              code_texte(tx("ajusté", "fitted")), code_texte(tx("validation croisée", "cross-validation"))))))))
}

ecart_arbre <- function(m, e, langue) {
  r <- function(x) get0(nom_classement(x, langue), envir = e, inherits = FALSE)
  regles <- r("regles")
  imp <- r("importance")
  paires <- list(
    "table d'élagage" = list(m$complet$cptable, r("elagage")),
    "valeurs des feuilles" = if (m$classement) list(as.numeric(factor(regles_arbre(m$ajustement, m$classes)$valeur, m$classes)), as.numeric(factor(regles[[nom_classement("valeur", langue)]], m$classes)))
                             else list(regles_arbre(m$ajustement)$valeur, regles[[nom_classement("valeur", langue)]]),
    "effectifs des feuilles" = list(regles_arbre(m$ajustement, m$classes)$n, regles$n),
    importance = list(importance_arbre(m$ajustement, m$variables)$pourcentage, imp[[nom_classement("pourcentage", langue)]]),
    "validation croisée" = list(if (m$classement) m$vc$proba else m$vc$prediction, r("vc")),
    "ajustement" = if (m$classement) list(m$proba, r("proba")) else list(m$ajuste, r("ajuste")),
    stabilite = if (!is.null(m$stabilite)) {
      s <- r("stab")
      list(as.matrix(m$stabilite[c("tirages", "frequence", "seuil_median")]), if (!is.null(s)) as.matrix(s[nom_classement(c("tirages", "frequence", "seuil_median"), langue)]))
    },
    test = if (!is.null(m$test)) {
      if (m$classement) list(m$test$classement$table, r("classement_test"))
      else list(m$test$observations[[paste(m$y$nom, "prédit")]], r("prediction_test"))
    })
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# ---------------------------------------------------------------- la LDA

code_fonctions_lda <- function(m, tx) {
  c(commentaire(tx("Les colonnes de X qui peuvent entrer dans le modèle : sans les constantes ni celles qui se déduisent des autres (une catégorie quand toutes y sont).",
                   "The columns of X that can enter the model: without the constant ones nor those implied by the others (a category when all of them are there).")),
    "colonnes_utiles <- function(X) {",
    "  q <- qr(cbind(1, X), tol = 1e-9)",
    "  setdiff(sort(q$pivot[seq_len(q$rank)]), 1) - 1",
    "}",
    "",
    commentaire(tx("La LDA de MASS sur X divisé par ses écarts-types (les scores et les probabilités n'en dépendent pas ; le seuil de MASS pour une variable constante devient relatif) ; les probabilités a priori proportionnelles aux effectifs des classes, ou égales.",
                   "MASS's LDA on X divided by its standard deviations (the scores and probabilities don't depend on it; MASS's threshold for a constant variable becomes relative); the prior probabilities proportional to the class sizes, or equal.")),
    "lda_reduite <- function(X, classe, priors = \"proportionnelles\") {",
    "  classe <- droplevels(classe)",
    "  et <- apply(X, 2, sd)",
    "  prior <- if (priors == \"egales\") rep(1 / nlevels(classe), nlevels(classe)) else as.vector(table(classe)) / length(classe)",
    "  a <- MASS::lda(sweep(X, 2, et, \"/\"), grouping = classe, prior = prior)",
    "  a$et <- et",
    "  a",
    "}",
    "",
    commentaire(tx("Les probabilités a posteriori (0 pour une classe que le modèle n'a pas vue) et les scores discriminants ; une ligne incomplète reste sans prédiction.",
                   "The posterior probabilities (0 for a class the model has not seen) and the discriminant scores; an incomplete row stays without a prediction.")),
    "probabilites_lda <- function(a, X, classes) {",
    "  P <- matrix(NA_real_, nrow(X), length(classes), dimnames = list(NULL, classes))",
    "  scores <- matrix(NA_real_, nrow(X), ncol(a$scaling), dimnames = list(NULL, colnames(a$scaling)))",
    "  ok <- complete.cases(X)",
    "  if (any(ok)) {",
    "    p <- predict(a, sweep(X[ok, , drop = FALSE], 2, a$et, \"/\"))",
    "    P[ok, ] <- 0",
    "    P[ok, colnames(p$posterior)] <- p$posterior",
    "    scores[ok, ] <- p$x",
    "  }",
    "  list(proba = P, scores = scores)",
    "}",
    "",
    commentaire(tx("La validation croisée : chaque groupe classé par le modèle ajusté sans lui (ses colonnes utiles et ses probabilités a priori recalculées) ; un groupe dont les autres n'ont qu'une classe reste sans prédiction.",
                   "The cross-validation: each group classified by the model fitted without it (its useful columns and prior probabilities recomputed); a group whose others have a single class stays without a prediction.")),
    "validation_lda <- function(X, classe, groupes, priors = \"proportionnelles\") {",
    "  classes <- levels(classe)",
    "  P <- matrix(NA_real_, nrow(X), length(classes), dimnames = list(NULL, classes))",
    "  for (g in unique(groupes)) {",
    "    k <- groupes == g",
    "    ca <- droplevels(classe[!k])",
    "    utiles <- colonnes_utiles(X[!k, , drop = FALSE])",
    "    a <- if (nlevels(ca) >= 2 && length(utiles)) tryCatch(suppressWarnings(lda_reduite(X[!k, utiles, drop = FALSE], ca, priors)), error = function(e) NULL)",
    "    if (!is.null(a)) P[k, ] <- probabilites_lda(a, X[k, utiles, drop = FALSE], classes)$proba",
    "  }",
    "  P",
    "}")
}

code_calcul_lda <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  col <- tab$noms[[as.character(d$classe)]]
  priors <- code_texte(m$reglages_lda$priors)
  c(code_lignes_modele(projet, d, m, tab, tx, c("classe ou une valeur X manquante", "class or an X value missing")), "",
    code_fonction_bloc(projet, m$termes, m$observations, tab, langue), "X <- variables_x(d)", "",
    commentaire(tx(sprintf("La classe : « %s » (ses classes présentes).", texte_sur(m$classe_nom)), sprintf("The class: “%s” (its classes present).", texte_sur(m$classe_nom)))),
    sprintf("classe <- droplevels(d[[%s]])", code_texte(col)),
    "classes <- levels(classe)",
    "utiles <- colonnes_utiles(X)",
    "X <- X[, utiles, drop = FALSE]",
    "",
    code_groupes_modele(m, tx), "",
    commentaire(tx(sprintf("La LDA (probabilités a priori %s), ses scores, ses probabilités a posteriori et son classement.", if (m$reglages_lda$priors == "egales") "égales" else "proportionnelles aux effectifs"),
                   sprintf("The LDA (prior probabilities %s), its scores, posterior probabilities and classification.", if (m$reglages_lda$priors == "egales") "equal" else "proportional to the sizes"))),
    sprintf("a <- lda_reduite(X, classe, %s)", priors),
    "resultat <- probabilites_lda(a, X, classes)",
    "classe_ajustee <- factor(classes[max.col(resultat$proba, ties.method = \"first\")], classes)",
    "coefficients <- a$scaling / a$et",
    "coefficients_std <- unclass(a$scaling)",
    "structure_lda <- cor(X, resultat$scores)",
    "part <- a$svd^2 / sum(a$svd^2)",
    "moyennes <- rowsum(X, classe) / as.vector(table(classe))",
    "",
    commentaire(tx(sprintf("La validation croisée, par les %d groupes.", m$G), sprintf("The cross-validation, by the %d groups.", m$G))),
    sprintf("vc <- validation_lda(X, classe, groupes_vc, %s)", priors),
    "classe_vc <- factor(ifelse(rowSums(is.na(vc)) > 0, NA, classes[max.col(replace(vc, is.na(vc), 0), ties.method = \"first\")]), classes)",
    "classement <- table(observee = classe, predite = classe_ajustee)",
    "classement_vc <- table(observee = classe, predite = classe_vc)",
    "qualite <- c(bien_classees = mean(classe_ajustee == classe), bien_classees_vc = mean(classe_vc == classe, na.rm = TRUE))",
    if (!is.null(m$test)) c("",
      commentaire(tx("Le jeu de test, classé par le modèle (une observation incomplète reste sans classe).", "The test set, classified by the model (an incomplete observation stays without a class).")),
      "X_test <- variables_x(donnees[lignes_test, ])[, colnames(X), drop = FALSE]",
      sprintf("y_test <- factor(donnees[lignes_test, %s], classes)", code_texte(col)),
      "proba_test <- probabilites_lda(a, X_test, classes)$proba",
      "classe_test <- factor(ifelse(rowSums(is.na(proba_test)) > 0, NA, classes[max.col(replace(proba_test, is.na(proba_test), 0), ties.method = \"first\")]), classes)",
      "classement_test <- table(observee = y_test, predite = classe_test)"))
}

code_sortie_lda <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  ligne <- function(code, fr, en) {
    code <- traduire_classement(code, langue)
    paste0(formatC(code, width = -max(34, nchar(code) + 2)), "# ", tx(fr, en))
  }
  c(ligne("print(a$prior)", "probabilités a priori", "prior probabilities"),
    ligne("print(part)", "part de la séparation portée par chaque fonction", "share of the separation carried by each function"),
    ligne("print(coefficients)", "coefficients, dans les unités des variables", "coefficients, in the units of the variables"),
    ligne("print(structure_lda)", "corrélations de structure (variables et scores)", "structure correlations (variables and scores)"),
    ligne("print(moyennes)", "moyennes des classes", "class means"),
    ligne("print(qualite)", "bien classées, en validation croisée", "correctly classified, in cross-validation"),
    ligne("print(classement_vc)", "classement en validation croisée", "cross-validated classification"),
    if (!is.null(m$test)) ligne("print(classement_test)", "jeu de test", "test set"))
}

code_graphiques_lda <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  t <- function(code) traduire_classement(code, langue)
  couleurs <- "couleurs <- c(\"#2f6fb5\", \"#d1495b\", \"#3b9a5b\", \"#e09f3e\", \"#7b5ea7\", \"#2a9d8f\", \"#8c564b\", \"#c2549d\", \"#6b6b6b\", \"#a3a322\")"
  list(list(nom = "scores", largeur = 7, hauteur = 6, code = t(c(couleurs,
    if (length(m$axes) > 1) c("plot(resultat$scores[, 1:2], col = couleurs[(as.integer(classe) - 1) %% 10 + 1], pch = 19,",
                              sprintf("     xlab = sprintf(\"LD1 (%%.1f %%%%)\", 100 * part[1]), ylab = sprintf(\"LD2 (%%.1f %%%%)\", 100 * part[2]), main = %s)", code_texte(tx("Scores discriminants", "Discriminant scores"))))
    else c("stripchart(resultat$scores[, 1] ~ classe, method = \"jitter\", pch = 19, col = couleurs[seq_along(classes)],",
           sprintf("           xlab = \"LD1\", main = %s)", code_texte(tx("Scores discriminants", "Discriminant scores")))),
    if (length(m$axes) > 1) "legend(\"topright\", classes, col = couleurs[(seq_along(classes) - 1) %% 10 + 1], pch = 19, bty = \"n\")"))))
}

ecart_lda <- function(m, e, langue) {
  r <- function(x) get0(nom_classement(x, langue), envir = e, inherits = FALSE)
  res <- r("resultat")
  paires <- list(probabilites = list(m$proba, res[[nom_classement("proba", langue)]]), scores = list(m$scores, res[[nom_classement("scores", langue)]]),
                 coefficients = list(m$coefficients, r("coefficients")), structure = list(m$structure, r("structure_lda")), part = list(m$part, r("part")),
                 "validation croisée" = list(m$vc$proba, r("vc")),
                 test = if (!is.null(m$test)) list(m$test$classement$table, r("classement_test")))
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# ---------------------------------------------------------------- les k-moyennes

code_fonctions_kmeans <- function(m, tx) {
  c(commentaire(tx("La mise à l'échelle d'un terme (pretraitement.R de Nuée) : UV, centré et divisé par son écart-type ; Par (Pareto), par sa racine ; Ctr, centré ; none, tel quel ; puis multiplié par sa pondération (bloc, modificateur).",
                   "The scaling of a term (Nuée's pretraitement.R): UV, centred and divided by its standard deviation; Par (Pareto), by its square root; Ctr, centred; none, as is; then multiplied by its weighting (block, modifier).")),
    "echelle_terme <- function(x, type, ponderation = 1) {",
    "  et <- sd(x)",
    "  centre <- if (type == \"none\") 0 else mean(x)",
    "  poids <- switch(type, UV = if (et > 0) 1 / et else 0, Par = if (et > 0) 1 / sqrt(et) else 0, Ctr = 1, none = 1)",
    "  (x - centre) * poids * ponderation",
    "}",
    "",
    commentaire(tx(sprintf("Les k-moyennes (algorithme de Hartigan et Wong), le meilleur de %d départs au hasard (graine 1), les groupes numérotés du plus grand au plus petit.", DEPARTS_KMEANS),
                   sprintf("The k-means (Hartigan and Wong algorithm), the best of %d random starts (seed 1), the clusters numbered from the largest to the smallest.", DEPARTS_KMEANS))),
    "kmeans_ordonne <- function(X, k) {",
    "  set.seed(1, kind = \"Mersenne-Twister\", normal.kind = \"Inversion\", sample.kind = \"Rejection\")",
    sprintf("  a <- kmeans(X, centers = k, nstart = %d, iter.max = 100)", DEPARTS_KMEANS),
    "  ordre <- order(-a$size, seq_len(k))",
    "  list(groupe = match(a$cluster, ordre), centres = a$centers[ordre, , drop = FALSE], tailles = a$size[ordre],",
    "       intra = a$withinss[ordre], inter = a$betweenss, totale = a$totss)",
    "}")
}

code_calcul_kmeans <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  simples <- Filter(function(t) length(t$variables) == 1, d$x)
  croises <- Filter(function(t) length(t$variables) > 1, d$x)
  termes <- c(simples, croises)
  if (!identical(vapply(termes, function(t) as.character(t$name), ""), vapply(d$x, function(t) as.character(t$name), ""))) {
    stop(traduire("Export vers R : les termes croisés de ces k-moyennes doivent suivre leurs termes simples.", "Export to R: the cross terms of these k-means must come after their simple terms."), call. = FALSE)
  }
  echelles <- vapply(simples, function(t) as.character(t$scaling$type %||% "UV")[1], "")
  ponderations <- facteurs_ponderation(d$x)
  noms <- m$bloc$noms
  code_croises <- unlist(lapply(seq_along(croises), function(j) {
    t <- croises[[j]]
    k <- length(simples) + j
    facteurs <- vapply(seq_along(t$variables), function(i) {
      s <- which(vapply(simples, function(x) identical(as.numeric(x$variables), as.numeric(t$variables[i])) && categorie(x) == categorie(t, i), TRUE))
      sprintf("X[, %s]", code_texte(noms[s]))
    }, "")
    produit <- paste(facteurs, collapse = " * ")
    if (!is.null(t$transform)) produit <- code_transformation(t$transform, sprintf("(%s)", produit), TRUE)
    sprintf("X <- cbind(X, %s = echelle_terme(%s, %s, %s))", code_texte(noms[k]), produit, code_texte(as.character(t$scaling$type %||% "UV")[1]), texte_exact(ponderations[k]))
  }))
  c(code_lignes_modele(projet, d, m, tab, tx, c("une valeur manquante", "a value missing")), "",
    code_fonction_bloc(projet, termes_bruts(simples), m$observations, tab, langue), "bruts <- variables_x(d)", "",
    commentaire(tx("Chaque terme mis à l'échelle comme dans Nuée (onglet Variables : son échelle ; sa pondération : bloc et modificateur), sur les observations du modèle.",
                   "Each term scaled as in Nuée (Variables tab: its scaling; its weighting: block and modifier), over the model's observations.")),
    code_valeurs(vapply(echelles, code_texte, "", USE.NAMES = FALSE), "echelles <- "),
    code_valeurs(texte_exact(ponderations[seq_along(simples)]), "ponderations <- "),
    "X <- bruts",
    "for (j in seq_len(ncol(X))) X[, j] <- echelle_terme(bruts[, j], echelles[j], ponderations[j])",
    if (length(croises)) c(commentaire(tx("Les termes croisés : le produit de leurs facteurs mis à l'échelle, lui-même mis à l'échelle.", "The cross terms: the product of their scaled factors, itself scaled.")), code_croises),
    "",
    commentaire(tx(sprintf("Les k-moyennes en %d groupes, leur part de la variance (R²), la silhouette de chaque observation%s, les deux premières composantes principales, les centres dans les unités des variables.",
                           m$k, if (is.null(m$silhouette)) " (au-delà de 3000 observations : sans)" else ""),
                   sprintf("The k-means in %d clusters, their share of the variance (R²), the silhouette of each observation%s, the first two principal components, the centres in the units of the variables.",
                           m$k, if (is.null(m$silhouette)) " (beyond 3000 observations: none)" else ""))),
    sprintf("resultat <- kmeans_ordonne(X, %d)", m$k),
    "groupe <- resultat$groupe",
    "r2 <- resultat$inter / resultat$totale",
    "tailles <- data.frame(groupe = seq_len(nrow(resultat$centres)), n = resultat$tailles, intra = resultat$intra)",
    if (!is.null(m$silhouette)) c("sil <- cluster::silhouette(groupe, dist(X))",
                                  "tailles$silhouette <- tapply(sil[, \"sil_width\"], groupe, mean)"),
    "acp <- prcomp(X)",
    "plan <- acp$x[, seq_len(min(2, ncol(acp$x))), drop = FALSE]",
    "centres_unites <- rowsum(bruts, groupe) / resultat$tailles",
    if (!is.null(m$choix)) c("",
      commentaire(tx(sprintf("Le choix de k : de 2 à %d groupes, la somme des carrés intra-groupes, le R² et la silhouette moyenne.", max(m$choix$k)),
                     sprintf("The choice of k: from 2 to %d clusters, the within-cluster sum of squares, the R² and the mean silhouette.", max(m$choix$k)))),
      if (m$N <= SILHOUETTE_MAX) "distances <- dist(X)",
      sprintf("choix <- do.call(rbind, lapply(2:%d, function(k) {", max(m$choix$k)),
      "  a <- kmeans_ordonne(X, k)",
      sprintf("  data.frame(k = k, intra = sum(a$intra), r2 = a$inter / a$totale, silhouette = %s)",
              if (m$N <= SILHOUETTE_MAX) "mean(cluster::silhouette(a$groupe, distances)[, \"sil_width\"])" else "NA_real_"),
      "}))"))
}

code_sortie_kmeans <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  ligne <- function(code, fr, en) {
    code <- traduire_classement(code, langue)
    paste0(formatC(code, width = -max(34, nchar(code) + 2)), "# ", tx(fr, en))
  }
  c(ligne("print(tailles)", "effectif, somme des carrés intra et silhouette de chaque groupe", "size, within sum of squares and silhouette of each cluster"),
    ligne("print(r2)", "part de la variance entre les groupes", "share of the variance between clusters"),
    ligne("print(centres_unites)", "centres dans les unités des variables", "centres in the units of the variables"),
    ligne("print(resultat$centres)", "centres mis à l'échelle", "scaled centres"),
    if (!is.null(m$silhouette)) ligne("print(mean(sil[, \"sil_width\"]))", "silhouette moyenne", "mean silhouette"),
    if (!is.null(m$choix)) ligne("print(choix)", "choix de k", "choice of k"))
}

code_graphiques_kmeans <- function(m, langue) {
  tx <- function(fr, en) if (langue == "en") en else fr
  t <- function(code) traduire_classement(code, langue)
  couleurs <- "couleurs <- c(\"#2f6fb5\", \"#d1495b\", \"#3b9a5b\", \"#e09f3e\", \"#7b5ea7\", \"#2a9d8f\", \"#8c564b\", \"#c2549d\", \"#6b6b6b\", \"#a3a322\")"
  part <- "part <- acp$sdev^2 / sum(acp$sdev^2)"
  c(list(list(nom = "groupes", largeur = 7, hauteur = 6, code = t(c(couleurs, part,
      if (ncol(m$acp$scores) > 1) c("plot(plan, col = couleurs[(groupe - 1) %% 10 + 1], pch = 19,",
                                    sprintf("     xlab = sprintf(\"PC1 (%%.1f %%%%)\", 100 * part[1]), ylab = sprintf(\"PC2 (%%.1f %%%%)\", 100 * part[2]), main = %s)", code_texte(tx("Groupes sur les deux premières composantes", "Clusters on the first two components"))),
                                    "points(scale(resultat$centres, center = acp$center, scale = FALSE) %*% acp$rotation[, 1:2], pch = 4, cex = 2, lwd = 2)")
      else c("stripchart(plan[, 1] ~ groupe, method = \"jitter\", pch = 19, col = couleurs[seq_len(max(groupe))], xlab = \"PC1\")"),
      sprintf("legend(\"topright\", paste(%s, seq_len(max(groupe))), col = couleurs[(seq_len(max(groupe)) - 1) %%%% 10 + 1], pch = 19, bty = \"n\")", code_texte(tx("groupe", "cluster"))))))),
    if (!is.null(m$silhouette)) list(list(nom = "silhouette", largeur = 7, hauteur = 6, code = t(c(couleurs,
      sprintf("plot(sil, col = couleurs[seq_len(max(groupe))], border = NA, main = %s)", code_texte(tx("Silhouettes", "Silhouettes"))))))),
    if (!is.null(m$choix)) list(list(nom = "choix-k", largeur = 8, hauteur = 4, code = t(c(
      "op <- par(mfrow = c(1, 2))",
      sprintf("plot(choix$k, choix$intra, type = \"b\", pch = 19, xlab = \"k\", ylab = %s)", code_texte(tx("somme des carrés intra-groupes", "within-cluster sum of squares"))),
      sprintf("plot(choix$k, choix$silhouette, type = \"b\", pch = 19, xlab = \"k\", ylab = %s)", code_texte(tx("silhouette moyenne", "mean silhouette"))),
      "par(op)")))))
}

ecart_kmeans <- function(m, e, langue) {
  r <- function(x) get0(nom_classement(x, langue), envir = e, inherits = FALSE)
  res <- r("resultat")
  sil <- r("sil")
  paires <- list(groupes = list(m$groupe, r("groupe")), centres = list(m$centres, res[[nom_classement("centres", langue)]]),
                 "inertie intra" = list(m$intra, res[[nom_classement("intra", langue)]]), R2 = list(m$r2, r("r2")),
                 silhouette = if (!is.null(m$silhouette)) list(m$silhouette$largeur, if (!is.null(sil)) unname(sil[, "sil_width"])),
                 composantes = list(m$acp$scores, r("plan")), "centres (unités)" = list(m$centres_unites, r("centres_unites")),
                 "choix de k" = if (!is.null(m$choix)) list(as.matrix(m$choix), as.matrix(r("choix"))))
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# ---------------------------------------------------------------- écrire le script

# (les trois analyses s'exportent par la même fonction)
ecrire_script_tree <- ecrire_script_lda <- ecrire_script_kmeans <- function(...) ecrire_script_classement(...)

ecrire_script_classement <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (langue == "en") en else fr
  format <- c(r = "R", rmd = "Rmd", qmd = "qmd")[[tolower(tools::file_ext(chemin_script))]]
  type <- m$type
  tab <- tableau_script(projet, ids_condition(projet, d))  # (les identifiants que lit sa condition : condition.R)
  fichiers <- fichiers_script(chemin_script, donnees, rdata, renv, rproj, langue)
  code_donnees <- donnees_script(tab, fichiers, langue)
  piece <- switch(type,
    TREE = list(fonctions = code_fonctions_arbre(m, tx), calcul = code_calcul_arbre(projet, d, m, tab, langue), sortie = code_sortie_arbre, graphiques = code_graphiques_arbre,
                ecart = ecart_arbre, paquet = "rpart",
                quoi = if (m$classement) tx("arbre de classement", "classification tree") else tx("arbre de régression", "regression tree"),
                objets = c("arbre", "resultat", "elagage", "regles", "importance", "premiere", "vc", "qualite", "proba", "ajuste", "classement", "classement_vc", "stab", "test", "classement_test")),
    LDA = list(fonctions = code_fonctions_lda(m, tx), calcul = code_calcul_lda(projet, d, m, tab, langue), sortie = code_sortie_lda, graphiques = code_graphiques_lda,
               ecart = ecart_lda, paquet = "MASS", quoi = tx("analyse discriminante linéaire (LDA)", "linear discriminant analysis (LDA)"),
               objets = c("a", "resultat", "coefficients", "coefficients_std", "structure_lda", "part", "moyennes", "vc", "classement", "classement_vc", "qualite", "classement_test")),
    KMEANS = list(fonctions = code_fonctions_kmeans(m, tx), calcul = code_calcul_kmeans(projet, d, m, tab, langue), sortie = code_sortie_kmeans, graphiques = code_graphiques_kmeans,
                  ecart = ecart_kmeans, paquet = if (!is.null(m$silhouette)) "cluster", quoi = tx(sprintf("k-moyennes en %d groupes", m$k), sprintf("k-means in %d clusters", m$k)),
                  objets = c("resultat", "groupe", "r2", "tailles", "sil", "acp", "plan", "centres_unites", "choix", "X", "bruts")))
  fc <- plan_condition(projet, d, tab)$fonctions  # (celles de Nouvelle variable qu'appelle sa condition : condition.R)
  fonctions <- traduire_classement(c(piece$fonctions, if (length(fc)) c("", utils::head(code_fonctions_condition(fc, tx), -1))), langue)
  calcul <- traduire_classement(piece$calcul, langue)
  e <- environnement_script(tab, fonctions, langue)
  probleme <- evaluer_script(calcul, e, avertissements = FALSE)
  ecart <- if (!is.null(probleme)) probleme else piece$ecart(m, e, langue)
  sortie <- piece$sortie(m, langue)
  graphiques <- piece$graphiques(m, langue)
  nom <- texte_sur(as.character(d$name %||% ""))
  titre <- texte_sur(as.character(d$title %||% ""))
  paquet <- piece$paquet
  entete <- c(sprintf(tx("# Modèle %s%s : %s, %d observations", "# Model %s%s: %s, %d observations"), nom, if (nzchar(titre)) sprintf(" (%s)", titre) else "", piece$quoi, m$N),
              sprintf(tx("# Script R écrit par Nuée %s le %s, à partir du projet « %s ».", "# R script written by Nuée %s on %s, from the project “%s”."), version, format(Sys.Date()),
                      texte_sur(as.character(projet$source %||% "projet"))),
              tx("# Il refait cette analyse avec R, de ses données à ses résultats, qui sont ceux de Nuée. Ses quatre parties :",
                 "# It redoes this analysis with R, from its data to its results, which are those of Nuée. Its four parts:"),
              tx("#   1. Fonctions", "#   1. Functions"), tx("#   2. Données", "#   2. Data"),
              tx("#   3. Modèle : variables, observations, calcul", "#   3. Model: variables, observations, computation"),
              tx("#   4. Résultats : tableaux et graphiques", "#   4. Results: tables and plots"),
              tx("# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.", "# In RStudio, Ctrl+Shift+O shows these parts; Ctrl+Shift+S runs the whole script."),
              sprintf(tx("# Écrit avec R %s%s.", "# Written with R %s%s."), paste(R.version$major, R.version$minor, sep = "."),
                      if (!is.null(paquet)) sprintf(tx(" et le paquet %s %s, livré avec R (appelé par %s::)", " and the package %s %s, which comes with R (called as %s::)"),
                                                    paquet, utils::packageVersion(paquet), paquet) else ""),
              fichiers_a_cote(fichiers, langue, FALSE))
  lignes <- if (format == "R") {
    c(entete, "", titre_partie(1, langue), fonctions, "", titre_partie(2, langue), code_donnees, "", titre_partie(3, langue), calcul, "",
      titre_partie(4, langue), sortie, "", texte_graphiques(langue), unlist(lapply(graphiques, function(g) c(g$code, ""))))
  } else {
    lignes_rapport_classement(format, d, m, langue, texte_methodes(projet, d, m, langue, version), fonctions, code_donnees, calcul, graphiques)
  }
  objets <- c(nom_classement(piece$objets, langue), nom_script("donnees", langue))
  # (la formule d'un arbre garde l'environnement où le script a tourné ici : le .RData garde
  # l'environnement global à sa place)
  for (n in intersect(objets, ls(e))) assign(n, sans_environnement(get(n, envir = e)), envir = e)
  ecrire_fichiers_script(lignes, fichiers, e, objets, c(paquet, if (format != "R") c("knitr", "rmarkdown")))
  list(script = chemin_script, chemins = unname(fichiers), ecart = ecart)
}

# Un objet du script pour le .RData : les termes d'un arbre (et des arbres d'une liste) rattachés
# à l'environnement global.
sans_environnement <- function(x) {
  if (inherits(x, "rpart")) {
    attr(x$terms, ".Environment") <- globalenv()
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) x[] <- lapply(x, sans_environnement)
  x
}

# Le rapport R Markdown ou Quarto : les méthodes, les données, le code (replié dans le rapport
# compilé), les résultats (tableaux, graphiques en R de base) et les références.
lignes_rapport_classement <- function(format, d, m, langue, methodes, fonctions, code_donnees, calcul, graphiques) {
  tx <- function(fr, en) if (langue == "en") en else fr
  bloc <- function(options, code) c(sprintf("```{r%s}", if (nzchar(options)) paste0(" ", options) else ""), code, "```", "")
  kable <- function(objet, chiffres) traduire_classement(sprintf("knitr::kable(%s, digits = %d)", objet, chiffres), langue)
  nom <- texte_sur(as.character(d$name %||% ""))
  titre <- sprintf("%s %s%s", tx("Modèle", "Model"), nom, if (nzchar(as.character(d$title %||% ""))) paste(" —", texte_sur(d$title)) else "")
  yaml <- c("---", sprintf("title: %s", code_texte(titre)), sprintf("date: %s", code_texte(format(Sys.Date()))), sprintf("lang: %s", langue),
            if (format == "Rmd") c("output:", "  html_document:", "    code_folding: hide") else c("format:", "  html:", "    embed-resources: true", "    code-fold: true"),
            "---", "")
  resultats <- switch(m$type,
    TREE = c(bloc("qualite-table", kable("t(qualite)", 3)),
             bloc("regles-table", kable("regles", 3)),
             bloc("importance-table", kable("importance", 2)),
             if (m$classement) bloc("classement-table", kable("classement_vc", 0)),
             if (!is.null(m$stabilite)) bloc("stabilite-table", kable("stab", 3)),
             bloc("elagage-table", kable("elagage", 4))),
    LDA = c(bloc("qualite-table", kable("t(qualite)", 3)),
            bloc("classement-table", kable("classement_vc", 0)),
            bloc("coefficients-table", kable("coefficients", 4)),
            bloc("structure-table", kable("structure_lda", 3))),
    KMEANS = c(bloc("tailles-table", kable("tailles", 3)),
               bloc("centres-table", kable("centres_unites", 3)),
               if (!is.null(m$choix)) bloc("choix-table", kable("choix", 3))))
  c(yaml,
    sprintf("## %s", tx("Méthodes", "Methods")), "", rbind(texte_sur(methodes$paragraphes), ""),
    sprintf("## %s", tx("Données", "Data")), "",
    bloc("donnees, echo = FALSE", code_donnees),
    bloc("", traduire_code("str(donnees, list.len = 12)", langue)),
    sprintf("## %s", tx("Modèle", "Model")), "",
    tx("Le calcul, écrit en R pour ce modèle (bouton « Code » pour le voir).", "The computation, written in R for this model (“Code” button to see it)."), "",
    bloc("fonctions", fonctions),
    bloc("modele", calcul),
    sprintf("## %s", tx("Résultats", "Results")), "",
    resultats,
    unlist(lapply(graphiques, function(g) bloc(sprintf("%s, fig.width = %s, fig.height = %s", g$nom, g$largeur, g$hauteur), g$code))),
    sprintf("## %s", tx("Références", "References")), "", paste("-", methodes$references))
}
