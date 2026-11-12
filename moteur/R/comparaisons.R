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
  if (!length(cats)) stop(traduire("La variable de classe doit être qualitative", "The class variable must be qualitative"))
  g <- factor(cats[colonne(projet, classe, obs)], levels = cats)
  deux <- test %in% c("welch", "student", "mann_whitney")
  if (deux) {
    groupes <- as.character(groupes %||% cats[1:2])
    if (length(groupes) != 2 || !all(groupes %in% cats) || groupes[1] == groupes[2]) stop(traduire("Il faut deux classes différentes à comparer", "Two different classes are needed to compare"))
  } else groupes <- cats
  if (!test %in% c("welch", "student", "mann_whitney", "anova", "kruskal")) stop(traduire("Test inconnu : ", "Unknown test: "), test)
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
  if (is.null(res)) stop(traduire("Aucune variable quantitative à comparer", "No quantitative variable to compare"))
  res$p_ajuste <- if (correction == "aucune") res$p else stats::p.adjust(res$p, method = if (correction == "bonferroni") "bonferroni" else "BH")
  list(tableau = res, groupes = groupes, test = test, correction = correction, classe = as.character(v_classe$ids[1]),
       effectifs = as.list(table(g)))
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
#     défaut) : voir comparer_apparies() ;
#   - deux mesures, appariées par l'observation (appariement = "observation" : la même ligne
#     porte les deux valeurs, avant et après par exemple) : y − x, sur les observations où les
#     deux sont connues.
# Renvoie le type de comparaison, les tests (statistique, degrés de liberté, p, taille de
# l'effet), le test conseillé et pourquoi (raison : "attendus", "normalite", "variances" ou
# ""), ce qu'il faut pour les graphiques, et une phrase qui donne le résultat du test conseillé.
test_deux_variables <- function(projet, x, y, observations = NULL, appariement = NULL, niveaux = NULL) {
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
      r <- comparer_apparies(mesure$valeurs, groupe$valeurs, as.character(k$valeurs), niveaux, unname(projet$noms_obs[as.character(obs)]), k$nom)
    }
    r <- c(list(x = a[c("uid", "nom", "qualitative")], y = b[c("uid", "nom", "qualitative")], cle = k[c("uid", "nom", "qualitative")]), r)
    r$phrase <- phrase_test(r)
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
    mw <- essai_test(stats::wilcox.test(b, a))
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
# clé ne désigne qu'une ligne par niveau, et il faut au moins trois paires. Les tests sont ceux
# de tests_apparies() ; avec trois niveaux ou plus, toutes leurs paires aussi
# (paires_niveaux()).
comparer_apparies <- function(x, f, cle, niveaux, noms, nom_cle) {
  presents <- levels(droplevels(f))
  niveaux <- as.character(unlist(niveaux))
  if (!length(niveaux)) niveaux <- presents[1:2]
  if (length(niveaux) != 2 || anyNA(niveaux) || niveaux[1] == niveaux[2] || !all(niveaux %in% presents)) {
    stop(traduire("Il faut deux niveaux différents à comparer", "Two different levels are needed to compare"))
  }
  dans <- f %in% niveaux
  ok <- dans & !is.na(x) & !is.na(cle)
  lignes <- lapply(niveaux, function(n) which(ok & f %in% n))
  ordre <- function(v) v[order(suppressWarnings(as.numeric(v)), v)]
  for (k in 1:2) {
    doubles <- ordre(unique(cle[lignes[[k]]][duplicated(cle[lignes[[k]]])]))
    if (length(doubles)) {
      stop(sprintf(traduire("%s a plusieurs lignes pour la même valeur de %s (%s) : la clé d'appariement doit désigner une seule ligne par niveau",
                            "%s has several rows for the same value of %s (%s): the pairing key must identify one row per level"),
                   niveaux[k], nom_cle, paste(c(utils::head(doubles, 5), if (length(doubles) > 5) "…"), collapse = ", ")))
    }
  }
  ia <- lignes[[1]]
  ib <- lignes[[2]]
  communes <- ordre(intersect(cle[ia], cle[ib]))
  n <- length(communes)
  if (n < 3) {
    stop(sprintf(traduire("Il faut au moins trois paires : %s et %s n'ont que %d valeur(s) de %s en commun",
                          "At least three pairs are needed: %s and %s share only %d value(s) of %s"), niveaux[1], niveaux[2], n, nom_cle))
  }
  ja <- ia[match(communes, cle[ia])]
  jb <- ib[match(communes, cle[ib])]
  r <- c(list(type = "apparie", ligne = FALSE, a = niveaux[1], b = niveaux[2], n = n, manquantes = sum(dans & !ok),
              paires = list(cle = communes, a = x[ja], b = x[jb], difference = x[jb] - x[ja], nom_a = noms[ja], nom_b = noms[jb]),
              sans_partenaire = list(a = ordre(setdiff(cle[ia], communes)), b = ordre(setdiff(cle[ib], communes)))),
         tests_apparies(x[ja], x[jb], niveaux[1], niveaux[2]))
  if (length(presents) > MAX_GROUPES_COMPARAISONS) {
    r$note_comparaisons <- sprintf(traduire("Plus de %d groupes : pas de comparaisons deux à deux.", "More than %d groups: no pairwise comparisons."),
                                   MAX_GROUPES_COMPARAISONS)
  } else if (length(presents) > 2) r$toutes <- paires_niveaux(x, f, cle, presents)
  r
}

# Les tests de valeurs appariées va (A) et vb (B), sur les différences B − A : le test t
# apparié (la différence moyenne et son intervalle à 95 %, le d_z de Cohen : la différence
# moyenne sur l'écart-type des différences) et le test des rangs signés de Wilcoxon (exact quand
# R le peut, sous 50 paires ; sinon par la loi normale), avec la corrélation rang-bisériale
# appariée des différences non nulles (Kerby, 2014). Wilcoxon est conseillé quand la normalité
# des différences est douteuse (Shapiro–Wilk, p < 0,05 ; tenue pour vraie au-delà de 5 000
# paires).
tests_apparies <- function(va, vb, a, b) {
  d <- vb - va
  n <- length(d)
  if (all(d == 0)) {
    stop(sprintf(traduire("%s et %s ont la même valeur dans chaque paire : rien à comparer", "%s and %s have the same value in every pair: nothing to compare"), a, b))
  }
  tt <- essai_test(stats::t.test(vb, va, paired = TRUE))
  w <- essai_test(stats::wilcox.test(vb, va, paired = TRUE))
  non_nulles <- d[d != 0]
  rangs <- rank(abs(non_nulles))
  shapiro <- if (n <= 5000 && diff(range(d)) > 0) stats::shapiro.test(d)$p.value else NA_real_
  normale <- n > 5000 || (!is.na(shapiro) && shapiro >= 0.05)
  list(difference = c(mean(d), if (is.null(tt)) c(NA, NA) else as.numeric(tt$conf.int)), mediane = stats::median(d),
       shapiro_p = shapiro, exact = !is.null(w) && grepl("exact", w$method, fixed = TRUE),
       tests = data.frame(test = c("t_apparie", "wilcoxon"), statistique = c(champ_test(tt, "statistic"), champ_test(w, "statistic")),
                          ddl1 = c(champ_test(tt, "parameter"), NA), ddl2 = NA_real_, p = c(champ_test(tt, "p.value"), champ_test(w, "p.value")),
                          effet = c(if (stats::sd(d) > 0) mean(d) / stats::sd(d) else NA,
                                    (sum(rangs[non_nulles > 0]) - sum(rangs[non_nulles < 0])) / sum(rangs)),
                          nom_effet = c("dz", "r_rb"), stringsAsFactors = FALSE),
       conseille = if (normale) "t_apparie" else "wilcoxon", raison = if (normale) "" else "normalite")
}

# Toutes les paires de niveaux (dans l'ordre de TukeyHSD() : b − a pour chaque b après a),
# chacune sur les clés que ses deux niveaux ont toutes deux : leur nombre, la différence moyenne
# et son intervalle à 95 %, la différence médiane, les valeurs p du t apparié et de Wilcoxon,
# et ces valeurs corrigées par Holm (1979) sur les paires testées. Quand tous les niveaux ont
# les mêmes clés, ce sont celles de pairwise.t.test() et pairwise.wilcox.test() avec
# paired = TRUE. Une paire de moins de trois clés communes, ou dont un niveau a une clé deux
# fois, n'est pas testée.
paires_niveaux <- function(x, f, cle, presents) {
  ok <- !is.na(x) & !is.na(cle)
  par <- lapply(presents, function(l) { i <- which(ok & f %in% l); stats::setNames(x[i], cle[i]) })
  pg <- paires_groupes(length(presents))
  lignes <- lapply(seq_along(pg$a), function(j) {
    va <- par[[pg$a[j]]]
    vb <- par[[pg$b[j]]]
    doublon <- anyDuplicated(names(va)) > 0 || anyDuplicated(names(vb)) > 0
    communes <- if (doublon) character(0) else intersect(names(va), names(vb))
    d <- vb[communes] - va[communes]
    teste <- length(d) >= 3 && any(d != 0)
    tt <- if (teste) essai_test(stats::t.test(vb[communes], va[communes], paired = TRUE))
    w <- if (teste) essai_test(stats::wilcox.test(vb[communes], va[communes], paired = TRUE))
    data.frame(n = if (doublon) NA_integer_ else length(communes), difference = if (length(d)) mean(d) else NA_real_,
               bas = if (is.null(tt)) NA_real_ else tt$conf.int[1], haut = if (is.null(tt)) NA_real_ else tt$conf.int[2],
               p_t = champ_test(tt, "p.value"), mediane = if (length(d)) stats::median(d) else NA_real_, p_w = champ_test(w, "p.value"))
  })
  res <- cbind(data.frame(a = presents[pg$a], b = presents[pg$b], stringsAsFactors = FALSE), do.call(rbind, lignes))
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
                       if (r$exact) (if (fr) "p exacte" else "exact p") else (if (fr) "p approchée par la loi normale" else "normal approximation"),
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
