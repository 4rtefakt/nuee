# Tests variable par variable : chaque variable quantitative comparée entre les classes
# d'une variable qualitative, avec la correction pour comparaisons multiples (le test est
# répété pour chaque variable). Pour deux classes A (la référence) et B :
#   "welch"        test t de Welch (variances inégales, par défaut) ;
#   "student"      test t de Student (variances égales) ;
#   "mann_whitney" test de Mann–Whitney (Wilcoxon, sur les rangs) ;
# pour toutes les classes ensemble :
#   "anova"        ANOVA à un facteur ;
#   "kruskal"      test de Kruskal–Wallis (sur les rangs).
# Correction : "BH" (Benjamini et Hochberg, 1995 : taux de fausses découvertes), "bonferroni"
# ou "aucune". L'effet est le rapport des moyennes B / A en log2 (quand les deux moyennes sont
# positives) et le d de Cohen (différence des moyennes sur l'écart-type commun) : avec la
# valeur p, ils font le graphique « volcan ».
tests_variables <- function(projet, classe, groupes = NULL, test = "welch", correction = "BH", observations = NULL) {
  obs <- observations %||% vapply(projet$observations, function(o) as.numeric(o$uid), 0)
  v_classe <- variable_du_projet(projet, classe)
  cats <- as.character(v_classe$categories)
  if (!length(cats)) stop("La variable de classe doit être qualitative")
  g <- factor(cats[colonne(projet, classe, obs)], levels = cats)
  deux <- test %in% c("welch", "student", "mann_whitney")
  if (deux) {
    groupes <- as.character(groupes %||% cats[1:2])
    if (length(groupes) != 2 || !all(groupes %in% cats) || groupes[1] == groupes[2]) stop("Il faut deux classes différentes à comparer")
  } else groupes <- cats
  if (!test %in% c("welch", "student", "mann_whitney", "anova", "kruskal")) stop("Test inconnu : ", test)
  vars <- Filter(function(v) !length(v$categories) && as.character(v$uid) %in% colnames(projet$donnees) &&
                   !identical(as.numeric(v$uid), as.numeric(classe)), projet$variables)
  lignes <- lapply(vars, function(v) {
    x <- colonne(projet, v$uid, obs)
    ligne <- data.frame(uid = as.numeric(v$uid), nom = as.character(v$ids[1]), statistique = NA_real_, p = NA_real_, stringsAsFactors = FALSE)
    if (deux) {
      a <- x[!is.na(x) & g %in% groupes[1]]
      b <- x[!is.na(x) & g %in% groupes[2]]
      ligne <- cbind(ligne, data.frame(n_a = length(a), n_b = length(b), moyenne_a = if (length(a)) mean(a) else NA,
                                       moyenne_b = if (length(b)) mean(b) else NA, log2_rapport = NA_real_, d = NA_real_))
      if (length(a) >= 2 && length(b) >= 2) {
        r <- tryCatch(switch(test,
          welch = stats::t.test(b, a),
          student = stats::t.test(b, a, var.equal = TRUE),
          mann_whitney = suppressWarnings(stats::wilcox.test(b, a))), error = function(e) NULL)
        if (!is.null(r)) ligne[c("statistique", "p")] <- list(unname(r$statistic), r$p.value)
        commun <- sqrt(((length(a) - 1) * stats::var(a) + (length(b) - 1) * stats::var(b)) / (length(a) + length(b) - 2))
        if (commun > 0) ligne$d <- (mean(b) - mean(a)) / commun
        if (mean(a) > 0 && mean(b) > 0) ligne$log2_rapport <- log2(mean(b) / mean(a))
      }
    } else {
      ok <- !is.na(x) & !is.na(g)
      gx <- droplevels(g[ok])
      if (nlevels(gx) >= 2 && sum(ok) > nlevels(gx)) {
        r <- tryCatch(if (test == "anova") stats::oneway.test(x[ok] ~ gx, var.equal = TRUE) else stats::kruskal.test(x[ok], gx),
                      error = function(e) NULL)
        if (!is.null(r)) ligne[c("statistique", "p")] <- list(unname(r$statistic), r$p.value)
      }
      for (k in cats) ligne[[paste("moyenne", k)]] <- if (any(ok & g %in% k)) mean(x[ok & g %in% k]) else NA_real_
    }
    ligne
  })
  res <- do.call(rbind, lignes)
  if (is.null(res)) stop("Aucune variable quantitative à comparer")
  res$p_ajuste <- if (correction == "aucune") res$p else stats::p.adjust(res$p, method = if (correction == "bonferroni") "bonferroni" else "BH")
  list(tableau = res, groupes = groupes, test = test, correction = correction, classe = as.character(v_classe$ids[1]),
       effectifs = as.list(table(g)))
}
