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
# Avec `appariement` (une variable ou "id:k" : l'année, quand A et B sont deux secteurs suivis
# les mêmes années), deux classes comparées sur des paires, comme dans test_deux_variables() :
# pour chaque variable, les lignes de A et de B qui ont la même clé et une valeur (paires_cle() ;
# une clé en double dans une classe est refusée), au moins trois paires, et
#   "t_apparie"    test t apparié (par défaut) ;
#   "wilcoxon"     test des rangs signés de Wilcoxon ;
# sur les différences B − A ; le nombre de paires, les moyennes de A et de B sur ces paires, le
# log2 du rapport de ces moyennes et le d_z de Cohen (différence moyenne sur l'écart-type des
# différences) à la place du d.
tests_variables <- function(projet, classe, groupes = NULL, test = "welch", correction = "BH", observations = NULL, appariement = NULL) {
  obs <- observations %||% vapply(projet$observations, function(o) as.numeric(o$uid), 0)
  v_classe <- variable_du_projet(projet, classe)
  cats <- as.character(v_classe$categories)
  if (!length(cats)) stop(traduire("La variable de classe doit être qualitative", "The class variable must be qualitative"))
  g <- factor(cats[colonne(projet, classe, obs)], levels = cats)
  apparie <- test %in% c("t_apparie", "wilcoxon")
  deux <- apparie || test %in% c("welch", "student", "mann_whitney")
  if (deux) {
    groupes <- as.character(groupes %||% cats[1:2])
    if (length(groupes) != 2 || !all(groupes %in% cats) || groupes[1] == groupes[2]) stop(traduire("Il faut deux classes différentes à comparer", "Two different classes are needed to compare"))
  } else groupes <- cats
  if (!test %in% c("welch", "student", "mann_whitney", "anova", "kruskal", "t_apparie", "wilcoxon")) stop(traduire("Test inconnu : ", "Unknown test: "), test)
  if (apparie != (length(appariement) > 0)) {
    stop(traduire("Un test apparié (t apparié, Wilcoxon) se fait avec une clé d'appariement, et une clé d'appariement avec un test apparié",
                  "A paired test (paired t, Wilcoxon) takes a pairing key, and a pairing key a paired test"))
  }
  if (apparie) {
    if (identical(as.character(appariement), as.character(classe))) {
      stop(traduire("La clé d'appariement doit être une autre variable que la classe", "The pairing key must be another variable than the class"))
    }
    k <- valeurs_a_tester(projet, appariement, obs)
    cle <- as.character(k$valeurs)
  }
  # (ni la classe ni la clé ne sont testées)
  vars <- Filter(function(v) !length(v$categories) && as.character(v$uid) %in% colnames(projet$donnees) &&
                   !identical(as.numeric(v$uid), as.numeric(classe)) && !identical(as.character(v$uid), as.character(appariement)), projet$variables)
  lignes <- lapply(vars, function(v) {
    x <- colonne(projet, v$uid, obs)
    ligne <- data.frame(uid = as.numeric(v$uid), nom = as.character(v$ids[1]), statistique = NA_real_, p = NA_real_, stringsAsFactors = FALSE)
    if (apparie) {
      pc <- paires_cle(x, g, cle, groupes, k$nom)
      va <- x[pc$ja]
      vb <- x[pc$jb]
      d <- vb - va
      n <- length(d)
      ligne <- cbind(ligne, data.frame(n_paires = n, moyenne_a = if (n) mean(va) else NA_real_, moyenne_b = if (n) mean(vb) else NA_real_,
                                       log2_rapport = NA_real_, d = NA_real_))
      if (n >= 3) {
        # (les règles du test apparié de deux variables : des différences constantes, à 1e-8 près
        # de leur moyenne, n'ont ni t ni d_z ; Wilcoxon, le même calcul sous toutes les versions de R)
        constantes <- differences_constantes(d)
        if (test == "t_apparie") {
          tt <- if (!constantes) t_apparie(d)
          if (!is.null(tt)) ligne[c("statistique", "p")] <- list(tt$t, tt$p)
        } else if (any(d != 0)) {
          w <- wilcoxon_apparie(d)
          ligne[c("statistique", "p")] <- list(w$V, w$p)
        }
        if (!constantes) ligne$d <- mean(d) / stats::sd(d)
        if (mean(va) > 0 && mean(vb) > 0) ligne$log2_rapport <- log2(mean(vb) / mean(va))
      }
    } else if (deux) {
      a <- x[!is.na(x) & g %in% groupes[1]]
      b <- x[!is.na(x) & g %in% groupes[2]]
      ligne <- cbind(ligne, data.frame(n_a = length(a), n_b = length(b), moyenne_a = if (length(a)) mean(a) else NA,
                                       moyenne_b = if (length(b)) mean(b) else NA, log2_rapport = NA_real_, d = NA_real_))
      if (length(a) >= 2 && length(b) >= 2) {
        r <- tryCatch(switch(test,
          welch = stats::t.test(b, a),
          student = stats::t.test(b, a, var.equal = TRUE),
          mann_whitney = mann_whitney(b, a)), error = function(e) NULL)
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
  if (is.null(res)) stop(traduire("Aucune variable quantitative à comparer", "No quantitative variable to compare"))
  res$p_ajuste <- if (correction == "aucune") res$p else stats::p.adjust(res$p, method = if (correction == "bonferroni") "bonferroni" else "BH")
  c(list(tableau = res, groupes = groupes, test = test, correction = correction, classe = as.character(v_classe$ids[1]),
         effectifs = as.list(table(g))), if (apparie) list(cle = k[c("uid", "nom", "qualitative")]))
}

# ------------------------------------------------------------------------ deux variables

# Le test qui convient à deux variables d'après leurs types, sur les observations où les deux
# sont connues, ses conditions vérifiées, et le test à lire quand elles ne le sont pas :
#   - deux qualitatives : le tableau de contingence, le khi² d'indépendance de Pearson (sans
#     correction de continuité), le V de Cramér et les résidus ajustés (Agresti, 2007). Le
#     test exact de Fisher quand les effectifs attendus sont trop petits pour le khi² (règle
#     de Cochran, 1954 : un attendu sous 1, ou plus de 20 % sous 5), et toujours pour un
#     tableau 2 × 2, avec le rapport des cotes ; au-delà de 2 × 2 et de 200 observations, sa
#     valeur p est simulée (100 000 tableaux) ;
#   - une quantitative et une qualitative : la quantitative comparée entre les groupes (ceux
#     d'au moins deux observations). Deux groupes : t de Welch et Mann–Whitney ; plus :
#     ANOVA (comparaisons deux à deux de Tukey), ANOVA de Welch (Games et Howell, 1976) et
#     Kruskal–Wallis (Dunn, 1964, correction de Holm). Le test conseillé est sur les rangs si
#     la normalité d'un groupe est douteuse (Shapiro–Wilk, p < 0,05 ; un groupe de moins de
#     trois observations ne se vérifie pas, un de plus de 5 000 est tenu pour normal), l'ANOVA
#     de Welch si les variances diffèrent (Brown et Forsythe, 1974 : p < 0,05) ;
#   - deux quantitatives : les corrélations de Pearson et de Spearman, Spearman conseillée si
#     la normalité de l'une des deux est douteuse.
# x et y : le numéro d'une variable, ou "id:k" pour le k-ième identifiant des observations
# (site, secteur, année...), qui fait alors des groupes comme une variable qualitative.
# Avec `appariement`, un test apparié :
#   - une mesure et une variable qualitative, appariées par une clé (une variable ou "id:k" :
#     l'année par exemple), entre deux de ses niveaux (`niveaux` : A et B, les deux premiers par
#     défaut) : voir comparer_apparies(). Avec `dans` (une variable qualitative ou "id:k" : le
#     site), toutes les paires de niveaux se limitent à celles d'un même groupe ;
#   - deux mesures, appariées par l'observation (appariement = "observation" : la même ligne
#     porte les deux valeurs, avant et après par exemple) : y − x, sur les observations où les
#     deux sont connues.
#   `memo` (le serveur) garde le tableau de toutes les paires d'une demande à l'autre.
# Renvoie le type de comparaison, les tests (statistique, degrés de liberté, p, taille de
# l'effet), le test conseillé et pourquoi (raison : "attendus", "normalite", "variances" ou
# ""), ce qu'il faut pour les graphiques, et une phrase qui donne le résultat du test conseillé.
test_deux_variables <- function(projet, x, y, observations = NULL, appariement = NULL, niveaux = NULL, dans = NULL, memo = NULL) {
  obs <- observations %||% numeros_observations(projet)
  if (identical(as.character(x), as.character(y))) stop(traduire("Choisissez deux variables différentes", "Choose two different variables"))
  a <- valeurs_a_tester(projet, x, obs)
  b <- valeurs_a_tester(projet, y, obs)
  if (length(appariement)) {
    ligne <- identical(as.character(appariement), "observation")
    if (a$qualitative && b$qualitative) {
      stop(traduire("Un test apparié compare une mesure entre deux niveaux d'une variable qualitative, ou deux mesures d'une même observation",
                    "A paired test compares a measurement between two levels of a qualitative variable, or two measurements of the same observation"))
    }
    if (!a$qualitative && !b$qualitative) {
      if (!ligne) stop(traduire("Deux mesures s'apparient par l'observation : la même ligne porte les deux valeurs",
                                "Two measurements are paired by the observation: the same row holds both values"))
      ok <- !is.na(a$valeurs) & !is.na(b$valeurs)
      if (sum(ok) < 3) stop(sprintf(traduire("Il faut au moins trois paires : %d observation(s) seulement ont les deux valeurs",
                                             "At least three pairs are needed: only %d observation(s) have both values"), sum(ok)))
      noms <- unname(projet$noms_obs[as.character(obs[ok])])
      r <- c(list(type = "apparie", ligne = TRUE, a = a$nom, b = b$nom, n = sum(ok), manquantes = sum(!ok),
                  paires = list(cle = noms, a = a$valeurs[ok], b = b$valeurs[ok], difference = b$valeurs[ok] - a$valeurs[ok], nom_a = noms, nom_b = noms),
                  sans_partenaire = list(a = character(0), b = character(0))),
             tests_apparies(a$valeurs[ok], b$valeurs[ok], a$nom, b$nom))
      k <- list(uid = "observation", nom = traduire("observation", "observation"), qualitative = TRUE)
    } else {
      if (ligne) stop(traduire("Une mesure et des groupes s'apparient par une troisième variable (l'année, pour deux secteurs)",
                               "A measurement and groups are paired by a third variable (the year, for two sectors)"))
      if (as.character(appariement) %in% c(as.character(x), as.character(y))) {
        stop(traduire("La clé d'appariement doit être une troisième variable", "The pairing key must be a third variable"))
      }
      k <- valeurs_a_tester(projet, appariement, obs)
      mesure <- if (a$qualitative) b else a
      groupe <- if (a$qualitative) a else b
      g <- NULL
      if (length(dans)) {
        if (as.character(dans) %in% as.character(c(x, y, appariement))) {
          stop(traduire("Le groupe des paires doit être une quatrième variable", "The group of the pairs must be a fourth variable"))
        }
        g <- valeurs_a_tester(projet, dans, obs)
        if (!g$qualitative) stop(traduire("Les paires se groupent par une variable qualitative ou un identifiant", "Pairs are grouped by a qualitative variable or an ID"))
      }
      r <- comparer_apparies(mesure$valeurs, groupe$valeurs, as.character(k$valeurs), niveaux, unname(projet$noms_obs[as.character(obs)]), k$nom,
                             g$valeurs, g$nom, memo)
      if (!is.null(g)) r$dans <- g[c("uid", "nom", "qualitative")]
    }
    r <- c(list(x = a[c("uid", "nom", "qualitative")], y = b[c("uid", "nom", "qualitative")], cle = k[c("uid", "nom", "qualitative")]), r)
    if (is.null(r$erreur)) r$phrase <- phrase_test(r)
    return(r)
  }
  ok <- !is.na(a$valeurs) & !is.na(b$valeurs)
  noms <- unname(projet$noms_obs[as.character(obs[ok])])
  r <- if (a$qualitative && b$qualitative) contingence(a$valeurs[ok], b$valeurs[ok])
       else if (a$qualitative) comparer_groupes(b$valeurs[ok], a$valeurs[ok], noms)
       else if (b$qualitative) comparer_groupes(a$valeurs[ok], b$valeurs[ok], noms)
       else correler(a$valeurs[ok], b$valeurs[ok], noms)
  r <- c(list(x = a[c("uid", "nom", "qualitative")], y = b[c("uid", "nom", "qualitative")], n = sum(ok), manquantes = sum(!ok)), r)
  r$phrase <- phrase_test(r)
  r
}

# Les valeurs d'une variable (un facteur si elle est qualitative) ou d'un identifiant des
# observations ("id:k" : un facteur, ses valeurs dans l'ordre des nombres quand elles en sont).
valeurs_a_tester <- function(projet, v, obs) {
  if (is.character(v) && startsWith(v, "id:")) {
    k <- suppressWarnings(as.integer(substring(v, 4)))
    noms <- as.character(projet$observation_id_names %||% character(0))
    if (is.na(k) || k < 2 || k > length(noms)) stop(traduire("Identifiant inconnu : ", "Unknown identifier: "), v)
    ids <- identifiants_observations(projet, k, obs)
    ids[!is.na(ids) & !nzchar(trimws(ids))] <- NA
    niveaux <- unique(ids[!is.na(ids)])
    niveaux <- niveaux[order(suppressWarnings(as.numeric(niveaux)), niveaux)]
    return(list(uid = v, nom = noms[k], qualitative = TRUE, valeurs = factor(ids, levels = niveaux)))
  }
  u <- as.numeric(v)
  variable <- variable_du_projet(projet, u)
  cats <- as.character(variable$categories)
  x <- colonne(projet, u, obs)
  list(uid = u, nom = as.character(variable$ids[1]), qualitative = length(cats) > 0,
       valeurs = if (length(cats)) factor(cats[x], levels = cats) else x)
}

# Les niveaux d'une variable qualitative ou d'un identifiant que portent les observations, dans
# l'ordre des tests : ceux que l'interface propose pour A et B d'un test apparié.
niveaux_a_tester <- function(projet, v, observations = NULL) {
  x <- valeurs_a_tester(projet, v, observations %||% numeros_observations(projet))
  if (!x$qualitative) stop(traduire("Cette variable n'est pas qualitative", "This variable is not qualitative"))
  levels(droplevels(x$valeurs))
}

# (le résultat d'un test, ou NULL quand R ne peut pas le faire : valeurs toutes égales...)
essai_test <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NULL)

# Le test de Mann–Whitney de b contre a, le même calcul sous toutes les versions de R : exact
# sous 50 valeurs de chaque côté et sans ex aequo ; sinon par la loi normale, avec la correction
# de continuité (et celle des ex aequo). R 4.6 calculerait une p exacte avec des ex aequo ; avant
# 4.6 (les R des paquets Linux), R passe alors par la loi normale : partout, ce calcul-là.
mann_whitney <- function(b, a) {
  exact <- length(a) < 50 && length(b) < 50 && !anyDuplicated(c(a, b))
  essai_test(stats::wilcox.test(b, a, exact = exact, correct = TRUE))
}
champ_test <- function(r, k) if (is.null(r) || is.null(r[[k]])) NA_real_ else unname(r[[k]])[1]

contingence <- function(f1, f2) {
  tab <- table(f1, f2)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    stop(traduire("Chaque variable doit avoir au moins deux catégories sur ces observations", "Each variable needs at least two categories in these observations"))
  }
  n <- sum(tab)
  kh <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  attendus <- unname(kh$expected)
  cochran <- all(attendus >= 1) && mean(attendus < 5) <= 0.2
  deux <- all(dim(tab) == 2)
  tests <- data.frame(test = "khi2", statistique = unname(kh$statistic), ddl1 = unname(kh$parameter), ddl2 = NA_real_, p = kh$p.value,
                      effet = sqrt(unname(kh$statistic) / (n * (min(dim(tab)) - 1))), nom_effet = "V", stringsAsFactors = FALSE)
  fi <- NULL
  simule <- FALSE
  if (!cochran || deux) {
    if (deux || n <= 200) fi <- essai_test(stats::fisher.test(tab))
    if (is.null(fi)) {
      simule <- TRUE
      set.seed(1)
      fi <- stats::fisher.test(tab, simulate.p.value = TRUE, B = 1e5)
    }
    tests <- rbind(tests, data.frame(test = "fisher", statistique = NA_real_, ddl1 = NA_real_, ddl2 = NA_real_, p = fi$p.value,
                                     effet = if (deux) champ_test(fi, "estimate") else NA_real_, nom_effet = if (deux) "OR" else "",
                                     stringsAsFactors = FALSE))
  }
  list(type = "contingence", lignes = rownames(tab), colonnes = colnames(tab), effectifs = unclass(unname(tab)) + 0,
       attendus = attendus + 0, residus = unclass(unname(kh$stdres)) + 0, part_attendus_faibles = mean(attendus < 5), attendu_min = min(attendus),
       cochran = cochran, fisher_simule = simule, ic_cotes = if (deux && !is.null(fi$conf.int)) as.numeric(fi$conf.int),
       tests = tests, conseille = if (cochran) "khi2" else "fisher", raison = if (cochran) "" else "attendus")
}

# x quantitative, f les groupes (un facteur), noms ceux des observations. Les comparaisons
# deux à deux se font jusqu'à MAX_GROUPES_COMPARAISONS groupes.
MAX_GROUPES_COMPARAISONS <- 50
comparer_groupes <- function(x, f, noms) {
  f <- droplevels(f)
  effectifs <- table(f)
  ecartes <- names(effectifs)[effectifs < 2]
  garde <- f %in% names(effectifs)[effectifs >= 2]
  x <- x[garde]
  f <- droplevels(f[garde])
  noms <- noms[garde]
  k <- nlevels(f)
  if (k < 2) stop(traduire("Il faut au moins deux groupes d'au moins deux observations", "At least two groups of two observations or more are needed"))
  par <- split(x, f)
  n <- vapply(par, length, 0)
  shapiro <- vapply(par, function(v) if (length(v) >= 3 && length(v) <= 5000 && diff(range(v)) > 0) stats::shapiro.test(v)$p.value else NA_real_, 0)
  normale <- all(n > 5000 | (!is.na(shapiro) & shapiro >= 0.05))
  bf <- essai_test(stats::oneway.test(abs(x - stats::ave(x, f, FUN = stats::median)) ~ f, var.equal = TRUE))
  groupes <- data.frame(groupe = levels(f), n = n, moyenne = vapply(par, mean, 0), ecart_type = vapply(par, stats::sd, 0),
                        mediane = vapply(par, stats::median, 0), q1 = vapply(par, stats::quantile, 0, probs = 0.25, names = FALSE),
                        q3 = vapply(par, stats::quantile, 0, probs = 0.75, names = FALSE), shapiro_p = shapiro,
                        row.names = NULL, stringsAsFactors = FALSE)
  res <- list(type = "groupes", groupes = groupes, ecartes = ecartes,
              variances = list(statistique = champ_test(bf, "statistic"), p = champ_test(bf, "p.value")),
              valeurs = list(groupe = as.character(f), valeur = x, nom = noms))
  if (k == 2) {
    a <- par[[1]]
    b <- par[[2]]
    w <- essai_test(stats::t.test(b, a))
    mw <- mann_whitney(b, a)
    commun <- sqrt(((length(a) - 1) * stats::var(a) + (length(b) - 1) * stats::var(b)) / (length(a) + length(b) - 2))
    res$tests <- data.frame(test = c("welch", "mann_whitney"), statistique = c(champ_test(w, "statistic"), champ_test(mw, "statistic")),
                            ddl1 = c(champ_test(w, "parameter"), NA), ddl2 = NA_real_, p = c(champ_test(w, "p.value"), champ_test(mw, "p.value")),
                            effet = c(if (commun > 0) (mean(b) - mean(a)) / commun else NA, 2 * champ_test(mw, "statistic") / (length(a) * length(b)) - 1),
                            nom_effet = c("d", "r_rb"), stringsAsFactors = FALSE)
    res$difference <- c(mean(b) - mean(a), if (is.null(w)) c(NA, NA) else as.numeric(w$conf.int))
    res$conseille <- if (normale) "welch" else "mann_whitney"
    res$raison <- if (normale) "" else "normalite"
  } else {
    av <- essai_test(stats::oneway.test(x ~ f, var.equal = TRUE))
    wa <- essai_test(stats::oneway.test(x ~ f, var.equal = FALSE))
    kw <- essai_test(stats::kruskal.test(x, f))
    totale <- sum((x - mean(x))^2)
    entre <- sum(n * (vapply(par, mean, 0) - mean(x))^2)
    ddl <- function(r, i) if (is.null(r)) NA_real_ else unname(r$parameter)[i]
    res$tests <- data.frame(test = c("anova", "welch_anova", "kruskal"),
                            statistique = c(champ_test(av, "statistic"), champ_test(wa, "statistic"), champ_test(kw, "statistic")),
                            ddl1 = c(ddl(av, 1), ddl(wa, 1), ddl(kw, 1)), ddl2 = c(ddl(av, 2), ddl(wa, 2), NA),
                            p = c(champ_test(av, "p.value"), champ_test(wa, "p.value"), champ_test(kw, "p.value")),
                            effet = c(if (totale > 0) entre / totale else NA, NA, champ_test(kw, "statistic") / (length(x) - 1)),
                            nom_effet = c("eta2", "", "epsilon2"), stringsAsFactors = FALSE)
    # (au-delà, trop de paires à lire, et Games–Howell compte en secondes : 3 s pour 50 groupes)
    if (k <= MAX_GROUPES_COMPARAISONS) res$posthoc <- list(anova = tukey(x, f), welch_anova = games_howell(par), kruskal = dunn(x, f))
    else res$note_comparaisons <- sprintf(traduire("Plus de %d groupes : pas de comparaisons deux à deux.", "More than %d groups: no pairwise comparisons."),
                                          MAX_GROUPES_COMPARAISONS)
    bf_p <- res$variances$p
    res$conseille <- if (!normale) "kruskal" else if (!is.na(bf_p) && bf_p < 0.05) "welch_anova" else "anova"
    res$raison <- if (!normale) "normalite" else if (res$conseille == "welch_anova") "variances" else ""
  }
  res
}

# Les paires de groupes (a, b) dans l'ordre de TukeyHSD() : b - a pour chaque b après a.
paires_groupes <- function(k) {
  ij <- which(lower.tri(diag(k)), arr.ind = TRUE)
  list(a = ij[, 2], b = ij[, 1])
}

tukey <- function(x, f) {
  t <- essai_test(stats::TukeyHSD(stats::aov(x ~ f))$f)
  pg <- paires_groupes(nlevels(f))
  if (is.null(t) || nrow(t) != length(pg$a)) return(NULL)
  data.frame(a = levels(f)[pg$a], b = levels(f)[pg$b], difference = t[, "diff"], bas = t[, "lwr"], haut = t[, "upr"], p = t[, "p adj"],
             row.names = NULL, stringsAsFactors = FALSE)
}

# Games et Howell (1976) : chaque paire comparée comme par un t de Welch, la valeur p et
# l'intervalle à 95 % tirés de la loi du maximum studentisé (comme Tukey).
games_howell <- function(par) {
  k <- length(par)
  m <- vapply(par, mean, 0)
  n <- vapply(par, length, 0)
  s <- vapply(par, stats::var, 0) / n
  pg <- paires_groupes(k)
  a <- pg$a
  b <- pg$b
  difference <- unname(m[b] - m[a])
  se <- unname(sqrt(s[a] + s[b]))
  ddl <- unname((s[a] + s[b])^2 / (s[a]^2 / (n[a] - 1) + s[b]^2 / (n[b] - 1)))
  marge <- stats::qtukey(0.95, k, ddl) / sqrt(2) * se
  data.frame(a = names(par)[a], b = names(par)[b], difference = difference, bas = difference - marge, haut = difference + marge,
             p = stats::ptukey(abs(difference) / se * sqrt(2), k, ddl, lower.tail = FALSE), row.names = NULL, stringsAsFactors = FALSE)
}

# Dunn (1964) : les rangs moyens de deux groupes comparés, sur les rangs de toutes les
# observations, avec la correction pour les ex aequo ; valeurs p corrigées par Holm (1979).
dunn <- function(x, f) {
  N <- length(x)
  rangs <- rank(x)
  n <- as.vector(table(f))
  moyens <- as.vector(tapply(rangs, f, mean))
  liens <- as.vector(table(x))
  pg <- paires_groupes(nlevels(f))
  a <- pg$a
  b <- pg$b
  z <- (moyens[b] - moyens[a]) / sqrt((N * (N + 1) / 12 - sum(liens^3 - liens) / (12 * (N - 1))) * (1 / n[a] + 1 / n[b]))
  p <- 2 * stats::pnorm(-abs(z))
  data.frame(a = levels(f)[a], b = levels(f)[b], z = z, p = p, p_ajuste = stats::p.adjust(p, "holm"), row.names = NULL, stringsAsFactors = FALSE)
}

correler <- function(x, y, noms) {
  n <- length(x)
  if (n < 3) stop(traduire("Il faut au moins trois observations où les deux variables sont connues", "At least three observations with both variables known are needed"))
  pe <- essai_test(stats::cor.test(x, y))
  sp <- essai_test(stats::cor.test(x, y, method = "spearman"))
  shapiro <- function(v) if (n <= 5000 && diff(range(v)) > 0) stats::shapiro.test(v)$p.value else NA_real_
  normalite <- c(shapiro(x), shapiro(y))
  normale <- n > 5000 || all(!is.na(normalite) & normalite >= 0.05)
  droite <- stats::coef(stats::lm(y ~ x))
  list(type = "correlation",
       tests = data.frame(test = c("pearson", "spearman"), statistique = c(champ_test(pe, "statistic"), champ_test(sp, "statistic")),
                          ddl1 = c(champ_test(pe, "parameter"), NA), ddl2 = NA_real_, p = c(champ_test(pe, "p.value"), champ_test(sp, "p.value")),
                          effet = c(champ_test(pe, "estimate"), champ_test(sp, "estimate")), nom_effet = c("r", "rho"), stringsAsFactors = FALSE),
       ic = if (!is.null(pe$conf.int)) as.numeric(pe$conf.int), normalite = normalite,
       ordonnee = unname(droite[1]), pente = unname(droite[2]),
       conseille = if (normale) "pearson" else "spearman", raison = if (normale) "" else "normalite",
       valeurs = list(x = x, y = y, nom = noms))
}

# Une mesure x comparée entre deux niveaux A et B des groupes f, sur des paires : les lignes de
# A et de B qui ont la même clé (cle, en texte : l'année, pour deux secteurs suivis les mêmes
# années). Les données restent en long, une ligne par secteur et par année : les clés que les
# deux niveaux ont toutes deux font les paires, les autres lignes restent sans partenaire. Une
# clé ne désigne qu'une ligne par niveau, et il faut au moins trois paires (paire_niveaux()).
# Avec trois niveaux ou plus, toutes leurs paires aussi (tableau_paires()), ou, avec le groupe
# de chaque ligne (groupe_ligne, un facteur : le site ; nommé nom_groupe), celles des niveaux
# d'un même groupe. Ce tableau ne dépend pas de A et B : il se fait d'abord (memo le garde d'une
# demande à l'autre), et une paire A–B qui ne se teste pas laisse son erreur à part (`erreur`).
comparer_apparies <- function(x, f, cle, niveaux, noms, nom_cle, groupe_ligne = NULL, nom_groupe = NULL, memo = NULL) {
  presents <- levels(droplevels(f))
  niveaux <- as.character(unlist(niveaux))
  if (!length(niveaux)) niveaux <- presents[1:2]
  if (length(niveaux) != 2 || anyNA(niveaux) || niveaux[1] == niveaux[2] || !all(niveaux %in% presents)) {
    stop(traduire("Il faut deux niveaux différents à comparer", "Two different levels are needed to compare"))
  }
  r <- list(type = "apparie", ligne = FALSE, a = niveaux[1], b = niveaux[2])
  if (length(presents) > 2) {
    calcul <- function() tableau_paires(x, f, cle, presents, groupe_ligne, nom_groupe, nom_cle)
    r <- c(r, if (is.null(memo)) calcul() else memo(calcul))
  }
  ab <- tryCatch(paire_niveaux(x, f, cle, niveaux, noms, nom_cle), error = function(e) e)
  if (!inherits(ab, "error")) return(c(r, ab))
  if (is.null(r$toutes)) stop(ab)
  r$erreur <- conditionMessage(ab)
  r
}

ordre_cles <- function(v) v[order(suppressWarnings(as.numeric(v)), v)]

# La paire A–B : ses clés communes, les lignes sans partenaire ou sans valeur, et ses tests.
paire_niveaux <- function(x, f, cle, niveaux, noms, nom_cle) {
  pc <- paires_cle(x, f, cle, niveaux, nom_cle)  # (le même appariement que les tests par variable)
  n <- length(pc$cle)
  if (n < 3) {
    stop(sprintf(traduire("Il faut au moins trois paires : %s et %s n'ont que %d valeur(s) de %s en commun",
                          "At least three pairs are needed: %s and %s share only %d value(s) of %s"), niveaux[1], niveaux[2], n, nom_cle))
  }
  ja <- pc$ja
  jb <- pc$jb
  c(list(n = n, manquantes = pc$manquantes,
         paires = list(cle = pc$cle, a = x[ja], b = x[jb], difference = x[jb] - x[ja], nom_a = noms[ja], nom_b = noms[jb]),
         sans_partenaire = list(a = pc$sans_a, b = pc$sans_b)),
    tests_apparies(x[ja], x[jb], niveaux[1], niveaux[2]))
}

# Le tableau de toutes les paires de niveaux (paires_niveaux()), ou de celles d'un même groupe :
# un niveau est du groupe que portent ses lignes, ceux qui n'en portent aucun sont listés
# (sans_groupe), et un niveau aux lignes de plusieurs groupes est refusé.
tableau_paires <- function(x, f, cle, presents, groupe_ligne, nom_groupe, nom_cle) {
  r <- list()
  groupes <- NULL
  if (!is.null(groupe_ligne)) {
    groupes <- factor(vapply(presents, function(l) {
      v <- levels(droplevels(groupe_ligne[f %in% l]))
      if (length(v) > 1) {
        stop(sprintf(traduire("%s a des lignes pour plusieurs valeurs de %s (%s) : un niveau doit appartenir à un seul groupe",
                              "%s has rows for several values of %s (%s): a level must belong to a single group"),
                     l, nom_groupe, paste(c(utils::head(v, 5), if (length(v) > 5) "…"), collapse = ", ")))
      }
      if (length(v)) v else NA_character_
    }, "", USE.NAMES = FALSE), levels = levels(groupe_ligne))
    if (anyNA(groupes)) r$sans_groupe <- presents[is.na(groupes)]
  }
  # (dans des groupes, le plus grand compte : ce sont ses paires qu'il faudrait lire)
  taille <- if (is.null(groupes)) length(presents) else max(0, table(groupes))
  if (taille > MAX_GROUPES_COMPARAISONS) {
    r$note_comparaisons <- sprintf(traduire("Plus de %d groupes : pas de comparaisons deux à deux.", "More than %d groups: no pairwise comparisons."),
                                   MAX_GROUPES_COMPARAISONS)
  } else if (taille < 2) {
    r$note_comparaisons <- sprintf(traduire("Aucune valeur de %s ne réunit deux niveaux : pas de paires à comparer.",
                                            "No value of %s holds two levels: no pairs to compare."), nom_groupe)
  } else r$toutes <- paires_niveaux(x, f, cle, presents, groupes, nom_cle)
  r
}

# Les paires d'une mesure x entre deux niveaux A et B (niveaux) des groupes f : les lignes de A
# et de B qui ont la même clé (cle, en texte), parmi celles qui ont une valeur et une clé. Une clé
# ne désigne qu'une ligne par niveau : une clé en double est refusée, les premières dites.
# Renvoie les clés communes (dans l'ordre des nombres quand elles en sont), les lignes de A et de
# B de chaque paire (ja, jb), les clés de chaque niveau restées sans partenaire et le nombre de
# lignes des deux niveaux sans valeur ou sans clé.
paires_cle <- function(x, f, cle, niveaux, nom_cle) {
  dans <- f %in% niveaux
  ok <- dans & !is.na(x) & !is.na(cle)
  lignes <- lapply(niveaux, function(n) which(ok & f %in% n))
  for (k in 1:2) {
    doubles <- ordre_cles(unique(cle[lignes[[k]]][duplicated(cle[lignes[[k]]])]))
    if (length(doubles)) {
      stop(sprintf(traduire("%s a plusieurs lignes pour la même valeur de %s (%s) : la clé d'appariement doit désigner une seule ligne par niveau",
                            "%s has several rows for the same value of %s (%s): the pairing key must identify one row per level"),
                   niveaux[k], nom_cle, paste(c(utils::head(doubles, 5), if (length(doubles) > 5) "…"), collapse = ", ")))
    }
  }
  ia <- lignes[[1]]
  ib <- lignes[[2]]
  communes <- ordre_cles(intersect(cle[ia], cle[ib]))
  list(cle = communes, ja = ia[match(communes, cle[ia])], jb = ib[match(communes, cle[ib])],
       sans_a = ordre_cles(setdiff(cle[ia], communes)), sans_b = ordre_cles(setdiff(cle[ib], communes)), manquantes = sum(dans & !ok))
}

# Les tests de valeurs appariées va (A) et vb (B), sur les différences B − A : le test t
# apparié (la différence moyenne et son intervalle à 95 %, le d_z de Cohen : la différence
# moyenne sur l'écart-type des différences) et le test des rangs signés de Wilcoxon
# (wilcoxon_apparie()). Le test conseillé et pourquoi (raison) :
#   - "" : la normalité des différences tient (Shapiro–Wilk, p ≥ 0,05) : le t apparié ;
#   - "normalite" : elle est douteuse (p < 0,05) : Wilcoxon ;
#   - "nombre" : plus de 5 000 paires, elle n'est pas testée et le t apparié tient ;
#   - "constantes" : les différences sont constantes (à 1e-8 près de leur moyenne : le bruit des
#     nombres à virgule) : ni test t ni d_z, Wilcoxon ;
#   - "non_testee" : Shapiro–Wilk n'a pas pu se faire : Wilcoxon.
tests_apparies <- function(va, vb, a, b) {
  d <- vb - va
  n <- length(d)
  if (all(d == 0)) {
    stop(sprintf(traduire("%s et %s ont la même valeur dans chaque paire : rien à comparer", "%s and %s have the same value in every pair: nothing to compare"), a, b))
  }
  constantes <- differences_constantes(d)
  tt <- if (!constantes) t_apparie(d)
  w <- wilcoxon_apparie(d)
  shapiro <- if (!constantes && n <= 5000) champ_test(essai_test(stats::shapiro.test(d)), "p.value") else NA_real_
  raison <- if (constantes) "constantes" else if (n > 5000) "nombre" else if (is.na(shapiro)) "non_testee" else if (shapiro < 0.05) "normalite" else ""
  list(difference = c(mean(d), if (is.null(tt)) c(NA, NA) else tt$ic), mediane = stats::median(d), shapiro_p = shapiro, exact = w$exact,
       tests = data.frame(test = c("t_apparie", "wilcoxon"), statistique = c(tt$t %||% NA_real_, w$V), ddl1 = c(tt$ddl %||% NA_real_, NA),
                          ddl2 = NA_real_, p = c(tt$p %||% NA_real_, w$p), effet = c(if (constantes) NA_real_ else mean(d) / stats::sd(d), w$r),
                          nom_effet = c("dz", "r_rb"), stringsAsFactors = FALSE),
       conseille = if (raison %in% c("", "nombre")) "t_apparie" else "wilcoxon", raison = raison)
}

differences_constantes <- function(d) stats::sd(d) <= 1e-8 * abs(mean(d))

# Le test t d'un échantillon de différences, le calcul de t.test(d) sans son coût (toutes les
# paires de niveaux en font des centaines) : NULL quand elles sont constantes, comme t.test().
t_apparie <- function(d) {
  n <- length(d)
  m <- mean(d)
  se <- sqrt(stats::var(d) / n)
  if (n < 2 || se < 10 * .Machine$double.eps * abs(m)) return(NULL)
  t <- m / se
  q <- stats::qt(0.975, n - 1)
  list(t = t, ddl = n - 1, p = 2 * stats::pt(-abs(t), n - 1), ic = m + c(-q, q) * se)
}

# Le test des rangs signés de Wilcoxon sur les différences d, le même calcul sous toutes les
# versions de R : exact sous 50 différences, aucune nulle et sans ex aequo ; sinon, les
# différences nulles retirées, par la loi normale avec la correction de continuité (et celle
# des ex aequo). R 4.6 calculerait une p exacte avec des zéros et des ex aequo, ses zéros classés
# (Pratt) ; avant 4.6 (les R des paquets Linux), R passe alors par la loi normale : partout, ce
# calcul-là. V : la somme des rangs des différences positives ; r : la corrélation
# rang-bisériale appariée sur ces mêmes rangs (Kerby, 2014), 2V/S − 1, S = n(n + 1)/2 sur les n
# différences non nulles.
wilcoxon_apparie <- function(d) {
  non_nulles <- sum(d != 0)
  exact <- non_nulles == length(d) && length(d) < 50 && !anyDuplicated(abs(d))
  w <- essai_test(stats::wilcox.test(d, exact = exact, correct = TRUE))
  v <- champ_test(w, "statistic")
  list(V = v, p = champ_test(w, "p.value"), exact = exact, r = 2 * v / (non_nulles * (non_nulles + 1) / 2) - 1)
}

# Toutes les paires de niveaux (dans l'ordre de TukeyHSD() : b − a pour chaque b après a),
# chacune sur les clés que ses deux niveaux ont toutes deux : leur nombre, la différence moyenne
# et son intervalle à 95 %, la différence médiane, les valeurs p du t apparié et de Wilcoxon,
# et ces valeurs corrigées par Holm (1979) sur les paires testées. Quand tous les niveaux ont
# les mêmes clés, ce sont celles de pairwise.t.test() et pairwise.wilcox.test() avec
# paired = TRUE. Une paire n'est pas testée, et sa raison est dite (non_teste), quand un de ses
# niveaux a une clé deux fois, qu'elle a moins de trois clés communes ou la même valeur dans
# chaque paire ; avec des différences constantes, seul Wilcoxon l'est. Avec le groupe de chaque
# niveau (`groupes`, un facteur ; NA hors des groupes), seulement les paires de niveaux d'un
# même groupe, groupe après groupe (colonne groupe), Holm sur ces paires-là.
paires_niveaux <- function(x, f, cle, presents, groupes = NULL, nom_cle = "") {
  ok <- !is.na(x) & !is.na(cle)
  par <- lapply(presents, function(l) { i <- which(ok & f %in% l); stats::setNames(x[i], cle[i]) })
  doublon <- vapply(par, function(v) anyDuplicated(names(v)) > 0, TRUE)
  pg <- paires_groupes(length(presents))
  if (!is.null(groupes)) {
    garde <- which(!is.na(groupes[pg$a]) & groupes[pg$a] == groupes[pg$b])
    garde <- garde[order(as.integer(groupes[pg$a[garde]]))]
    pg <- list(a = pg$a[garde], b = pg$b[garde])
  }
  m <- length(pg$a)
  n <- integer(m)
  difference <- bas <- haut <- p_t <- mediane <- p_w <- rep(NA_real_, m)
  non_teste <- character(m)
  for (j in seq_len(m)) {
    if (doublon[pg$a[j]] || doublon[pg$b[j]]) {
      n[j] <- NA_integer_
      non_teste[j] <- sprintf(traduire("%s a deux lignes pour une même valeur de %s", "%s has two rows for one value of %s"),
                              presents[if (doublon[pg$a[j]]) pg$a[j] else pg$b[j]], nom_cle)
      next
    }
    va <- par[[pg$a[j]]]
    vb <- par[[pg$b[j]]]
    communes <- ordre_cles(intersect(names(va), names(vb)))
    n[j] <- length(communes)
    if (n[j] < 3) {
      non_teste[j] <- if (n[j]) sprintf(traduire("%d valeur(s) de %s en commun, il en faut 3", "%d shared value(s) of %s, 3 needed"), n[j], nom_cle)
                      else sprintf(traduire("aucune valeur de %s en commun", "no shared value of %s"), nom_cle)
      if (!n[j]) next
    }
    d <- unname(vb[communes] - va[communes])
    difference[j] <- mean(d)
    mediane[j] <- stats::median(d)
    if (n[j] < 3) next
    if (all(d == 0)) {
      non_teste[j] <- traduire("la même valeur dans chaque paire", "the same value in every pair")
      next
    }
    tt <- if (!differences_constantes(d)) t_apparie(d)
    if (is.null(tt)) non_teste[j] <- traduire("différences constantes : pas de test t", "constant differences: no t-test")
    else {
      bas[j] <- tt$ic[1]
      haut[j] <- tt$ic[2]
      p_t[j] <- tt$p
    }
    p_w[j] <- wilcoxon_apparie(d)$p
  }
  res <- data.frame(a = presents[pg$a], b = presents[pg$b], n = n, difference = difference, bas = bas, haut = haut, p_t = p_t,
                    mediane = mediane, p_w = p_w, non_teste = non_teste, stringsAsFactors = FALSE)
  if (!is.null(groupes)) res <- cbind(data.frame(groupe = as.character(groupes[pg$a]), stringsAsFactors = FALSE), res)
  res$p_t_holm <- stats::p.adjust(res$p_t, "holm")
  res$p_w_holm <- stats::p.adjust(res$p_w, "holm")
  res
}

# La phrase qui donne le résultat du test conseillé, dans la langue des messages, pour un
# rapport : statistique, degrés de liberté, valeur p, taille de l'effet, effectifs.
phrase_test <- function(r) {
  fr <- !identical(getOption("nuee.langue"), "en")
  marque <- if (fr) "," else "."
  nb <- function(x, chiffres = 2) if (is.na(x)) "NA" else formatC(if (round(x, chiffres) == 0) 0 else x, format = "f", digits = chiffres, decimal.mark = marque)
  ddl <- function(x) if (is.na(x)) "NA" else if (abs(x - round(x)) < 1e-8) format(round(x)) else nb(x, 1)
  # (3 chiffres significatifs : 0,0497 ne s'écrit pas 0,05)
  pv <- function(p) if (is.na(p)) "p = NA" else if (p < 0.001) paste("p <", nb(0.001, 3)) else paste("p =", trimws(formatC(p, digits = 3, format = "g", decimal.mark = marque)))
  ic <- function(v) if (length(v) == 2 && !anyNA(v)) sprintf(if (fr) " [IC 95 %% %s ; %s]" else " [95%% CI %s, %s]", nb(v[1]), nb(v[2])) else ""
  # avant/après avec témoin (baci()) : le test choisi, les unités, les périodes
  if (identical(r$type, "baci")) {
    t <- r$tests[r$tests$test == r$test, ]
    p <- r$periodes
    noms <- function(v) if (length(v) > 3) sprintf("%s… (%d)", paste(v[1:3], collapse = ", "), length(v)) else paste(v, collapse = ", ")
    temps <- function(x) trimws(formatC(x, digits = 10, format = "fg", decimal.mark = marque))
    apres <- if (length(r$fin)) sprintf("%s ≤ %s ≤ %s", temps(r$evenement), r$temps$nom, temps(r$fin)) else sprintf("%s ≥ %s", r$temps$nom, temps(r$evenement))
    quoi <- sprintf(if (fr) "écart de %s impact (%s) − témoins (%s), après contre avant" else "%s gap impact (%s) − control (%s), after against before",
                    r$y$nom, noms(r$impact), noms(r$temoins))
    n <- sprintf(if (fr) "n = %d avant et %d après ; après : %s" else "n = %d before and %d after; after: %s", p$n[1], p$n[2], apres)
    if (r$test == "mann_whitney") {
      return(sprintf(if (fr) "Test de Mann–Whitney (BACI), %s : U = %s, %s ; écart médian %s avant et %s après, corrélation rang-bisériale = %s (%s)."
                     else "Mann–Whitney test (BACI), %s: U = %s, %s; median gap %s before and %s after, rank-biserial correlation = %s (%s).",
                     quoi, ddl(t$statistique), pv(t$p), nb(p$mediane[1]), nb(p$mediane[2]), nb(t$effet), n))
    }
    return(sprintf(if (fr) "Test t de %s (BACI), %s : t(%s) = %s, %s ; écart moyen %s avant et %s après, changement = %s%s, d de Cohen = %s (%s)."
                   else "%s's t-test (BACI), %s: t(%s) = %s, %s; mean gap %s before and %s after, change = %s%s, Cohen's d = %s (%s).",
                   if (r$test == "welch") "Welch" else "Student", quoi, ddl(t$ddl1), nb(t$statistique), pv(t$p), nb(p$moyenne[1]), nb(p$moyenne[2]),
                   nb(p$moyenne[2] - p$moyenne[1]), ic(c(t$bas, t$haut)), nb(t$effet), n))
  }
  t <- r$tests[r$tests$test == r$conseille, ]
  et <- if (fr) "et" else "and"
  if (r$type == "groupes") {
    g <- r$groupes
    effectifs <- if (nrow(g) == 2) sprintf("n = %d %s %d", g$n[1], et, g$n[2]) else sprintf(if (fr) "N = %d, %d groupes" else "N = %d, %d groups", sum(g$n), nrow(g))
  }
  if (r$type == "apparie") {
    paires <- if (r$ligne) sprintf(if (fr) "n = %d paires, une par observation" else "n = %d pairs, one per observation", r$n)
              else sprintf(if (fr) "n = %d paires, appariées par %s" else "n = %d pairs matched on %s", r$n, r$cle$nom)
  }
  switch(r$conseille,
    khi2 = sprintf(if (fr) "Khi² d'indépendance de Pearson : χ²(%s, N = %d) = %s, %s ; V de Cramér = %s." else "Pearson's chi-square test of independence: χ²(%s, N = %d) = %s, %s; Cramér's V = %s.",
                   ddl(t$ddl1), r$n, nb(t$statistique), pv(t$p), nb(t$effet)),
    fisher = sprintf(if (fr) "Test exact de Fisher%s : %s (N = %d)%s." else "Fisher's exact test%s: %s (N = %d)%s.",
                     if (r$fisher_simule) (if (fr) " (p simulée sur 100 000 tableaux)" else " (p simulated from 100,000 tables)") else "", pv(t$p), r$n,
                     if (identical(t$nom_effet, "OR")) sprintf(if (fr) " ; rapport des cotes = %s%s" else "; odds ratio = %s%s", nb(t$effet), ic(r$ic_cotes)) else ""),
    welch = sprintf(if (fr) "Test t de Welch : t(%s) = %s, %s ; différence %s − %s = %s%s, d de Cohen = %s (%s)." else "Welch's t-test: t(%s) = %s, %s; difference %s − %s = %s%s, Cohen's d = %s (%s).",
                    ddl(t$ddl1), nb(t$statistique), pv(t$p), r$groupes$groupe[2], r$groupes$groupe[1], nb(r$difference[1]), ic(r$difference[2:3]), nb(t$effet), effectifs),
    mann_whitney = sprintf(if (fr) "Test de Mann–Whitney : U = %s, %s ; médianes %s = %s et %s = %s, corrélation rang-bisériale = %s (%s)."
                           else "Mann–Whitney test: U = %s, %s; medians %s = %s and %s = %s, rank-biserial correlation = %s (%s).",
                           ddl(t$statistique), pv(t$p), r$groupes$groupe[1], nb(r$groupes$mediane[1]), r$groupes$groupe[2], nb(r$groupes$mediane[2]),
                           nb(t$effet), effectifs),
    t_apparie = sprintf(if (fr) "Test t apparié : t(%s) = %s, %s ; différence moyenne %s − %s = %s%s, d_z de Cohen = %s (%s)." else "Paired t-test: t(%s) = %s, %s; mean difference %s − %s = %s%s, Cohen's d_z = %s (%s).",
                        ddl(t$ddl1), nb(t$statistique), pv(t$p), r$b, r$a, nb(r$difference[1]), ic(r$difference[2:3]), nb(t$effet), paires),
    wilcoxon = sprintf(if (fr) "Test des rangs signés de Wilcoxon (%s) : V = %s, %s ; différence médiane %s − %s = %s, corrélation rang-bisériale = %s (%s)."
                       else "Wilcoxon signed-rank test (%s): V = %s, %s; median difference %s − %s = %s, rank-biserial correlation = %s (%s).",
                       if (r$exact) (if (fr) "p exacte" else "exact p")
                       else (if (fr) "p approchée par la loi normale, avec correction de continuité" else "normal approximation with continuity correction"),
                       ddl(t$statistique), pv(t$p), r$b, r$a, nb(r$mediane), nb(t$effet), paires),
    anova = sprintf(if (fr) "ANOVA à un facteur : F(%s, %s) = %s, %s ; η² = %s (%s)." else "One-way ANOVA: F(%s, %s) = %s, %s; η² = %s (%s).",
                    ddl(t$ddl1), ddl(t$ddl2), nb(t$statistique), pv(t$p), nb(t$effet), effectifs),
    welch_anova = sprintf(if (fr) "ANOVA de Welch : F(%s, %s) = %s, %s (%s)." else "Welch's ANOVA: F(%s, %s) = %s, %s (%s).",
                          ddl(t$ddl1), ddl(t$ddl2), nb(t$statistique), pv(t$p), effectifs),
    kruskal = sprintf(if (fr) "Test de Kruskal–Wallis : H(%s) = %s, %s ; ε² = %s (%s)." else "Kruskal–Wallis test: H(%s) = %s, %s; ε² = %s (%s).",
                      ddl(t$ddl1), nb(t$statistique), pv(t$p), nb(t$effet), effectifs),
    pearson = sprintf(if (fr) "Corrélation de Pearson : r = %s%s, t(%s) = %s, %s (n = %d)." else "Pearson correlation: r = %s%s, t(%s) = %s, %s (n = %d).",
                      nb(t$effet), ic(r$ic), ddl(t$ddl1), nb(t$statistique), pv(t$p), r$n),
    spearman = sprintf(if (fr) "Corrélation de Spearman : ρ = %s, %s (n = %d)." else "Spearman correlation: ρ = %s, %s (n = %d).",
                       nb(t$effet), pv(t$p), r$n))
}

# ---------------------------------------------------------- avant/après avec témoin (BACI)

# Avant/après avec témoin (BACI, before-after control-impact : Stewart-Oaten et al., 1986), comme
# la section 4 de son script 13 : à chaque temps t (l'année), l'écart d_t = la moyenne de y sur
# les lignes des unités d'impact à t − celle sur les lignes des unités témoins à t, aux temps où
# les unes et les autres en ont au moins une. Une ligne par unité et par temps donne la moyenne
# des unités ; avec plusieurs (les secteurs d'un site, quand l'unité est le site), chaque ligne
# compte une fois, comme dans le script (aggregate() sur les lignes des témoins) : `repetees` dit
# combien de temps chaque unité en a plusieurs. Les périodes : avant, t < evenement (le premier
# temps « après ») ; après, t ≥ evenement, jusqu'à fin comprise si elle est donnée (les temps
# suivants restent hors du test). Il faut au moins deux temps dans chaque période (son
# contraste()). Les écarts d'après sont comparés à ceux d'avant : t de Welch (par défaut), t de
# Student (variances égales : le lm(écart ~ période) du script, son intervalle et sa valeur p) ou
# Mann–Whitney (sur les rangs) ; l'effet est la moyenne des écarts d'après moins celle d'avant,
# avec son intervalle à 95 % (celui de Welch, ou de Student si c'est le test choisi). Les temps y
# sont tenus pour indépendants : l'autocorrélation des écarts n'est pas prise en compte (un modèle
# mixte y ~ période × impact + (1 | unité), avec CAR1, le fait).
# y : le numéro d'une variable quantitative ; temps : une variable ou "id:k" aux valeurs
# numériques (l'année) ; unite : une variable qualitative ou "id:k" (le secteur) ; impact et
# temoins : ses niveaux. Sans evenement, la série des écarts seule.
baci <- function(projet, y, temps, unite, impact, temoins, evenement = NULL, fin = NULL, test = "welch", observations = NULL) {
  obs <- observations %||% numeros_observations(projet)
  liste <- function(v) paste(c(utils::head(v, 5), if (length(v) > 5) "…"), collapse = ", ")
  if (anyDuplicated(as.character(c(y, temps, unite)))) {
    stop(traduire("La réponse, le temps et l'unité doivent être trois variables différentes", "The response, the time and the unit must be three different variables"))
  }
  if (!test %in% c("welch", "student", "mann_whitney")) stop(traduire("Test inconnu : ", "Unknown test: "), test)
  vy <- valeurs_a_tester(projet, y, obs)
  if (vy$qualitative) stop(traduire("La réponse doit être une variable quantitative", "The response must be a quantitative variable"))
  vt <- valeurs_a_tester(projet, temps, obs)
  tv <- as.vector(temps_en_nombres(if (vt$qualitative) as.character(vt$valeurs) else vt$valeurs))
  autres <- if (vt$qualitative) levels(droplevels(vt$valeurs[is.na(tv)])) else character(0)
  if (length(autres)) {
    stop(sprintf(traduire("Le temps doit être numérique : %s a des valeurs qui ne sont pas des nombres (%s)",
                          "The time must be numeric: %s has values that aren't numbers (%s)"), vt$nom, liste(autres)))
  }
  vu <- valeurs_a_tester(projet, unite, obs)
  if (!vu$qualitative) stop(traduire("L'unité doit être une variable qualitative ou un identifiant", "The unit must be a qualitative variable or an ID"))
  impact <- unique(as.character(unlist(impact)))
  temoins <- unique(as.character(unlist(temoins)))
  if (!length(impact) || !length(temoins)) stop(traduire("Choisissez au moins une unité d'impact et une unité témoin", "Choose at least one impact unit and one control unit"))
  inconnus <- setdiff(c(impact, temoins), levels(vu$valeurs))
  if (length(inconnus)) stop(sprintf(traduire("%s n'a pas de niveau %s", "%s has no level %s"), vu$nom, liste(inconnus)))
  deux_fois <- intersect(impact, temoins)
  if (length(deux_fois)) stop(sprintf(traduire("Une unité ne peut être à la fois impact et témoin : %s", "A unit can't be both impact and control: %s"), liste(deux_fois)))
  u <- as.character(vu$valeurs)
  role <- ifelse(u %in% impact, "impact", ifelse(u %in% temoins, "temoin", NA_character_))
  i <- which(!is.na(role) & !is.na(vy$valeurs) & !is.na(tv))
  temps_vus <- sort(unique(tv[i]))
  ti <- match(tv[i], temps_vus)
  par_temps <- function(r, f, vide) vapply(seq_along(temps_vus), function(k) {
    s <- role[i] == r & ti == k
    if (any(s)) f(vy$valeurs[i][s]) else vide
  }, 0)
  serie <- data.frame(temps = temps_vus, impact = par_temps("impact", mean, NA_real_), temoins = par_temps("temoin", mean, NA_real_),
                      n_impact = par_temps("impact", length, 0), n_temoins = par_temps("temoin", length, 0))
  deux <- serie$n_impact > 0 & serie$n_temoins > 0
  seuls <- list(impact = temps_vus[serie$n_impact > 0 & !deux], temoins = temps_vus[serie$n_temoins > 0 & !deux])
  serie <- serie[deux, ]
  rownames(serie) <- NULL
  if (!nrow(serie)) {
    stop(sprintf(traduire("Aucune valeur de %s où l'impact et les témoins ont tous deux une valeur de %s",
                          "No value of %s where both the impact and the controls have a value of %s"), vt$nom, vy$nom))
  }
  serie$ecart <- serie$impact - serie$temoins
  cles <- paste(u[i], ti, sep = "\r")
  plusieurs <- table(factor(sub("\r.*", "", unique(cles[duplicated(cles)])), levels = c(impact, temoins)))
  r <- list(type = "baci", y = vy[c("uid", "nom")], temps = vt[c("uid", "nom")], unite = vu[c("uid", "nom")], impact = impact, temoins = temoins,
            serie = serie, seuls = seuls, manquantes = sum(!is.na(role)) - length(i),
            repetees = if (any(plusieurs > 0)) data.frame(unite = names(plusieurs)[plusieurs > 0], temps = as.integer(plusieurs[plusieurs > 0]), stringsAsFactors = FALSE))
  if (!length(evenement)) return(r)
  evenement <- suppressWarnings(as.numeric(evenement))
  if (length(evenement) != 1 || is.na(evenement)) stop(traduire("Le temps de l'événement doit être un nombre", "The event time must be a number"))
  if (length(fin)) {
    fin <- suppressWarnings(as.numeric(fin))
    if (length(fin) != 1 || is.na(fin)) stop(traduire("La fin de la période après doit être un nombre", "The end of the after period must be a number"))
    if (fin < evenement) {
      stop(sprintf(traduire("La fin de la période après (%s) vient avant l'événement (%s)", "The end of the after period (%s) comes before the event (%s)"),
                   format(fin), format(evenement)))
    }
  } else fin <- NULL
  hors <- if (length(fin)) serie$temps > fin else rep(FALSE, nrow(serie))
  serie$periode <- ifelse(serie$temps < evenement, "avant", ifelse(hors, "hors", "apres"))
  r$serie <- serie
  db <- serie$ecart[serie$periode == "avant"]
  da <- serie$ecart[serie$periode == "apres"]
  nb <- length(db)
  na <- length(da)
  if (nb < 2 || na < 2) {
    stop(sprintf(traduire("Il faut au moins deux valeurs de %s dans chaque période, avant et après %s : il y en a %d avant et %d après",
                          "At least two values of %s are needed in each period, before and after %s: there are %d before and %d after"),
                 vt$nom, format(evenement), nb, na))
  }
  if (diff(range(c(db, da))) == 0) stop(traduire("L'écart est le même à chaque temps : rien à comparer", "The gap is the same at every time: nothing to compare"))
  w <- essai_test(stats::t.test(da, db))
  s <- essai_test(stats::t.test(da, db, var.equal = TRUE))
  mw <- mann_whitney(da, db)
  commun <- sqrt(((nb - 1) * stats::var(db) + (na - 1) * stats::var(da)) / (nb + na - 2))
  d <- if (commun > 0) (mean(da) - mean(db)) / commun else NA_real_
  ic <- function(x) if (is.null(x)) c(NA_real_, NA_real_) else as.numeric(x$conf.int)
  shapiro <- function(v) if (length(v) >= 3 && length(v) <= 5000 && diff(range(v)) > 0) stats::shapiro.test(v)$p.value else NA_real_
  tp <- list(serie$temps[serie$periode == "avant"], serie$temps[serie$periode == "apres"])
  r$periodes <- data.frame(periode = c("avant", "apres"), n = c(nb, na), debut = vapply(tp, min, 0), fin = vapply(tp, max, 0),
                           moyenne = c(mean(db), mean(da)), ecart_type = c(stats::sd(db), stats::sd(da)),
                           mediane = c(stats::median(db), stats::median(da)), shapiro_p = c(shapiro(db), shapiro(da)), stringsAsFactors = FALSE)
  r$tests <- data.frame(test = c("welch", "student", "mann_whitney"),
                        statistique = c(champ_test(w, "statistic"), champ_test(s, "statistic"), champ_test(mw, "statistic")),
                        ddl1 = c(champ_test(w, "parameter"), champ_test(s, "parameter"), NA), ddl2 = NA_real_,
                        p = c(champ_test(w, "p.value"), champ_test(s, "p.value"), champ_test(mw, "p.value")),
                        effet = c(d, d, 2 * champ_test(mw, "statistic") / (na * nb) - 1), nom_effet = c("d", "d", "r_rb"),
                        bas = c(ic(w)[1], ic(s)[1], NA), haut = c(ic(w)[2], ic(s)[2], NA), stringsAsFactors = FALSE)
  r$difference <- c(mean(da) - mean(db), ic(if (test == "student") s else w))
  r$evenement <- evenement
  r["fin"] <- list(fin)
  r$hors <- sum(hors)
  r$test <- test
  r$phrase <- phrase_test(r)
  r
}

# --------------------------------------------------------------- matrice des corrélations

# La matrice des corrélations des variables quantitatives (Pearson ou Spearman), chaque paire
# sur les observations où les deux sont connues, et ses valeurs p (approximation par la loi t,
# n - 2 degrés de liberté) corrigées pour les K(K - 1)/2 paires : "BH", "bonferroni" ou
# "aucune". Au-delà de MAX_CORRELATIONS variables (des spectres), elle est refusée.
MAX_CORRELATIONS <- 300
correlations_variables <- function(projet, methode = "pearson", correction = "BH", observations = NULL) {
  if (!methode %in% c("pearson", "spearman")) stop(traduire("Méthode inconnue : ", "Unknown method: "), methode)
  obs <- observations %||% numeros_observations(projet)
  vars <- Filter(function(v) !length(v$categories) && as.character(v$uid) %in% colnames(projet$donnees), projet$variables)
  K <- length(vars)
  if (K < 2) stop(traduire("Il faut au moins deux variables quantitatives", "At least two quantitative variables are needed"))
  if (K > MAX_CORRELATIONS) {
    stop(sprintf(traduire("La matrice des corrélations se limite à %d variables quantitatives (ce projet en a %d) : testez-les deux à deux.",
                          "The correlation matrix is limited to %d quantitative variables (this project has %d): test them two at a time."), MAX_CORRELATIONS, K))
  }
  X <- matrix(vapply(vars, function(v) colonne(projet, v$uid, obs), numeric(length(obs))), length(obs))
  r <- suppressWarnings(stats::cor(X, use = "pairwise.complete.obs", method = methode))
  n <- crossprod(!is.na(X))
  stat_t <- r * sqrt((n - 2) / pmax(1 - r^2, 0))
  p <- 2 * stats::pt(-abs(stat_t), n - 2)
  p[n < 3] <- NA
  diag(p) <- NA
  haut <- upper.tri(p)
  pa <- matrix(NA_real_, K, K)
  pa[haut] <- if (correction == "aucune") p[haut] else stats::p.adjust(p[haut], method = if (correction == "bonferroni") "bonferroni" else "BH")
  pa[lower.tri(pa)] <- t(pa)[lower.tri(pa)]
  list(uid = vapply(vars, function(v) as.numeric(v$uid), 0), noms = vapply(vars, function(v) as.character(v$ids[1]), ""),
       r = unname(r), n = unname(n) + 0, p = unname(p), p_ajuste = pa, methode = methode, correction = correction)
}
