# Les résultats clés d'un modèle ajusté, en nombres nommés, et ce qui change de l'un à l'autre.
# Nuée réajuste les modèles d'un projet quand ses données sont mises à jour, et deux versions d'un
# projet se comparent : la vue « Ce qui a changé » dit alors d'un coup d'œil ce qui a bougé dans les
# résultats, et quelles valeurs citées dans les documents ont changé.
#   resume_resultats(projet, e)       les résultats clés d'un modèle de la séance (e : sa définition et
#                                     son modèle ajusté), une liste d'entrées (entree_resultat) ;
#   comparer_resultats(avant, apres)  deux de ces résumés comparés entrée par entrée, par leur clé :
#                                     de combien chaque valeur change, ce qui change de signe, passe le
#                                     seuil de 5 %, sort de son ancien intervalle, apparaît ou
#                                     disparaît, l'importance de chaque changement et, pour ceux qui
#                                     comptent, une phrase écrite comme le texte des résultats
#                                     (resultats_texte.R : ses nombres, une virgule décimale en français).
# Ce que résume chaque type : les statistiques de l'ajustement (n, R², Q², AIC, déviance expliquée,
# Q² cumulé par composante…), chaque estimation (coefficients, effets, pentes, indices) avec son
# intervalle à 95 % et sa valeur p, chaque test avec sa valeur p ; pour une ACP ou une PLS, ce qu'en
# a chaque variable (ses coefficients, son VIP, ses loadings), dans un groupe à part, car il y en a
# des centaines pour des spectres. Seulement ce que l'ajustement calcule toujours, et ce qu'un
# réajustement refait avec le modèle (les moyennes ajustées et la puissance d'une tendance gardées
# avec sa définition : serveur.R, ajuster_et_garder). Un calcul fait à la demande et que le
# réajustement ne refait pas (les tests par axe d'une RDA, la sélection par AIC) paraîtrait disparu
# après lui : il n'en fait pas partie.
# Une autre analyse (analyses.R) donne ses entrées par entrees_<type>(projet, d, m), écrite ici ; sans
# elle, le nombre de ses observations seulement.

# Les axes d'une ordination dont la part d'inertie est résumée : les premiers, ceux qu'un article donne.
AXES_RESULTATS_CLES <- 3

# Un nombre fini, ou NULL : une valeur non calculée (NA, un NULL) ne fait pas d'entrée.
nombre_ou_rien <- function(x) {
  x <- suppressWarnings(as.numeric(unlist(x)))
  if (length(x) && is.finite(x[1])) x[1]
}

# Un nom de variable, de terme ou de catégorie entre les guillemets de la langue des messages.
nom_resultat <- function(x) guillemets_resultat(x, !identical(getOption("nuee.langue"), "en"))

# Une entrée du résumé, NULL quand elle n'a ni valeur ni catégorie :
#   cle      stable d'un ajustement à l'autre et d'une langue à l'autre : la référence d'un document
#            quand il y en a une ({{M1.Q2}} : « Q2 » ; {{M1.coef.annee}} : « coef.annee ») ;
#   libelle  ce qu'elle est, dans la langue des messages, sans article (« Coefficient de « annee » ») :
#            comparer_resultats en fait le sujet de ses phrases ;
#   groupe   ajustement, estimations, tests, aleatoires, composantes, variables, especes, moyennes,
#            puissance, jeu_test, stocke (les résultats enregistrés dans un fichier .usp) ;
#   valeur   le nombre ; ic : son intervalle à 95 % (bas, haut) ; p : sa valeur p ; n : son effectif ;
#   limite   la valeur nulle d'une estimation (0 ; 1 pour une pente multiplicative, un indice, un
#            rapport) : passer de l'autre côté, c'est changer de signe, et l'intervalle qui l'exclut ou
#            non dit, sans valeur p, si elle est significative ;
#   repere   un seuil de la valeur elle-même (1 pour un VIP) ;
#   forme    « pourcentage » ou « entier », pour l'écrire ; genre : « f » pour un nom féminin, que les
#            phrases en français accordent ;
#   alias    les autres références d'un document qui donnent la même valeur (est.annee, ic.annee et
#            p.annee pour coef.annee) ;
#   axe      la composante d'une valeur dont le signe est arbitraire (un loading d'ACP), que
#            comparer_resultats remet dans le sens de l'autre ajustement avant de comparer ;
#   classe   une catégorie qui va avec la valeur (la classe de la tendance de TRIM, le groupe d'une
#            espèce indicatrice) ou qui fait l'entrée (la fonction de détection retenue), et
#            classe_texte, ce qu'elle est en mots quand son code ne se lit pas.
entree_resultat <- function(cle, libelle, groupe, valeur = NULL, ic = NULL, p = NULL, n = NULL, limite = NULL, repere = NULL,
                            forme = NULL, genre = NULL, alias = NULL, axe = NULL, classe = NULL, classe_texte = NULL) {
  valeur <- nombre_ou_rien(valeur)
  classe <- as.character(unlist(classe))
  classe <- if (length(classe) && !is.na(classe[1]) && nzchar(classe[1])) classe[1]
  if (is.null(valeur) && is.null(classe)) return(NULL)
  ic <- suppressWarnings(as.numeric(unlist(ic)))
  e <- list(cle = as.character(cle), libelle = as.character(libelle), groupe = groupe, valeur = valeur,
            ic = if (!is.null(valeur) && length(ic) == 2 && all(is.finite(ic))) sort(ic), p = nombre_ou_rien(p), n = nombre_ou_rien(n),
            limite = nombre_ou_rien(limite), repere = nombre_ou_rien(repere), forme = forme, genre = genre,
            alias = if (length(alias)) I(as.character(alias)), axe = if (length(axe)) as.character(axe)[1],
            classe = classe, classe_texte = if (!is.null(classe) && length(classe_texte)) as.character(classe_texte)[1])
  Filter(Negate(is.null), e)
}

entree_n <- function(N) entree_resultat("N", traduire("Nombre d'observations", "Number of observations"), "ajustement", N, forme = "entier")

# Les résultats clés d'un modèle de la séance `e` (serveur.R : modele(id)) : ceux de son type, sans
# entrée vide ; une clé en double ne compte qu'une fois (la première), pour que la comparaison
# trouve une seule entrée par clé.
resume_resultats <- function(projet, e) {
  d <- e$definition
  m <- e$modele
  f <- if (est_regression(d)) entrees_regression else if (est_autre_analyse(d)) fonction_analyse("entrees_", d$type) else entrees_composantes
  if (is.null(f)) f <- entrees_communes
  entrees <- Filter(Negate(is.null), f(projet, d, m))
  unname(entrees[!duplicated(vapply(entrees, `[[`, "", "cle"))])  # (sans noms : un tableau en JSON)
}

entrees_communes <- function(projet, d, m) list(entree_n(m$N))

# ---------------------------------------------------------------- ACP, PLS, OPLS, PLS-DA

# Un modèle de composantes : les valeurs que ses documents citent (references.js : VALEURS_COMPOSANTES,
# sous les mêmes clés), le Q² cumulé et les R² cumulés de chaque composante, CV-ANOVA, et chaque
# variable (le VIP et les coefficients d'une PLS, les loadings et le R²V d'une ACP) ; un modèle importé
# d'un fichier .usp, aussi les résultats que le fichier a enregistrés pour son nombre de composantes.
entrees_composantes <- function(projet, d, m) {
  s <- m$statistiques
  cp <- s$composantes
  g <- nom_resultat
  pls <- identical(m$type, "PLS")
  A <- if (is.null(cp)) 0 else nrow(cp)
  if (!A) return(list(entree_n(m$N)))
  der <- cp[A, ]
  ny <- as.character(m$y$noms %||% character(0))
  h <- m$hierarchique
  E <- list(entree_n(m$N),
            entree_resultat("K", traduire("Nombre de termes X", "Number of X terms"), "ajustement", m$K, forme = "entier"),
            entree_resultat("A", traduire("Nombre de composantes", "Number of components"), "ajustement", m$A, forme = "entier"),
            entree_resultat("G", traduire("Nombre de groupes de validation croisée", "Number of cross-validation groups"), "ajustement", m$G, forme = "entier"),
            entree_resultat("R2X", traduire("R²X cumulé", "Cumulative R²X"), "ajustement", der$R2Xcum, forme = "pourcentage"),
            if (pls) entree_resultat("R2Y", traduire("R²Y cumulé", "Cumulative R²Y"), "ajustement", der$R2Ycum, forme = "pourcentage"),
            entree_resultat("Q2", traduire("Q² cumulé", "Cumulative Q²"), "ajustement", der$Q2cum),
            entree_resultat("Q2_vide", traduire("Q² du modèle vide", "Q² of the empty model"), "ajustement", m$q2_vide),
            entree_resultat("gain", traduire("Gain sur le modèle vide", "Gain over the empty model"), "ajustement", m$gain),
            # (le Q² d'un modèle hiérarchique dont les modèles de base sont refaits sans le groupe caché : hierarchique.R)
            if (!is.null(h$q2cum)) entree_resultat("Q2_honnete", traduire("Q² honnête (modèles de base refaits)", "Honest Q² (base models refitted)"), "ajustement", utils::tail(h$q2cum, 1)),
            entree_resultat("gain_honnete", traduire("Gain honnête sur le modèle vide", "Honest gain over the empty model"), "ajustement", h$gain))
  # (le premier Y sous la clé sans son nom, celle qu'un document cite le plus souvent ; RMSEE.<y> en alias)
  for (j in seq_along(ny)) {
    y <- ny[j]
    E <- c(E, list(entree_resultat(if (j == 1) "RMSEE" else paste0("RMSEE.", y), sprintf("RMSEE %s", g(y)), "ajustement", s$RMSEE[y], alias = if (j == 1) paste0("RMSEE.", y)),
                   entree_resultat(if (j == 1) "RMSEcv" else paste0("RMSEcv.", y), sprintf("RMSEcv %s", g(y)), "ajustement", s$RMSEcv[y], alias = if (j == 1) paste0("RMSEcv.", y))))
  }
  # (une PLS de plusieurs Y : le R² et le Q² de chacun ; ceux d'une PLS-DA sont ceux de ses classes, que
  # leur classement dit mieux)
  yv <- s$variables_y
  if (pls && !isTRUE(m$plsda) && length(ny) > 1 && !is.null(yv)) {
    E <- c(E, unlist(lapply(seq_along(ny), function(j) list(
      entree_resultat(paste0("R2V.", ny[j]), sprintf(traduire("R² de %s", "R² of %s"), g(ny[j])), "ajustement", yv$R2V_cum[j], forme = "pourcentage"),
      entree_resultat(paste0("Q2V.", ny[j]), sprintf(traduire("Q² de %s", "Q² of %s"), g(ny[j])), "ajustement", yv$Q2V_cum[j]))), recursive = FALSE))
  }
  cl <- s$classification
  if (!is.null(cl)) {
    effectifs <- table(factor(s$observations$classe, levels = ny))
    E <- c(E, list(entree_resultat("bien_classes", traduire("Part bien classée en validation croisée", "Share correctly classified in cross-validation"), "ajustement",
                                   cl$vc$global, forme = "pourcentage", genre = "f")),
           lapply(names(cl$vc$correct), function(k) entree_resultat(paste0("bien_classes.", k), sprintf(traduire("Part bien classée de %s en validation croisée", "Share of %s correctly classified in cross-validation"), g(k)),
                                                                    "ajustement", cl$vc$correct[[k]], n = if (k %in% names(effectifs)) effectifs[[k]], forme = "pourcentage", genre = "f")))
  }
  E <- c(E, unlist(lapply(seq_len(A), function(a) list(
    entree_resultat(paste0("R2X.", a), sprintf(traduire("R²X cumulé à la composante %d", "Cumulative R²X at component %d"), a), "composantes", cp$R2Xcum[a], forme = "pourcentage"),
    if (pls) entree_resultat(paste0("R2Y.", a), sprintf(traduire("R²Y cumulé à la composante %d", "Cumulative R²Y at component %d"), a), "composantes", cp$R2Ycum[a], forme = "pourcentage"),
    entree_resultat(paste0("Q2.", a), sprintf(traduire("Q² cumulé à la composante %d", "Cumulative Q² at component %d"), a), "composantes", cp$Q2cum[a]))), recursive = FALSE))
  ca <- s$cv_anova
  for (j in seq_along(ca)) {
    y <- names(ca)[j]
    E <- c(E, list(entree_resultat(if (j == 1) "cvanova.F" else paste0("cvanova.F.", y), sprintf(traduire("F du test CV-ANOVA de %s", "CV-ANOVA F of %s"), g(y)), "tests",
                                   ca[[j]]$F[2], p = ca[[j]]$p[2], alias = if (j == 1) c("cvanova.p", paste0("cvanova.F.", y), paste0("cvanova.p.", y)) else paste0("cvanova.p.", y))))
  }
  v <- s$variables
  termes <- as.character(v$terme)
  if (pls) {
    if (!is.null(v$VIP)) E <- c(E, lapply(seq_along(termes), function(i) entree_resultat(paste0("vip.", termes[i]), sprintf(traduire("VIP de %s", "VIP of %s"), g(termes[i])), "variables", v$VIP[i], repere = 1)))
    # (les coefficients centrés-réduits et leur intervalle par jack-knife, ceux de {{M1.coef.x}} pour le
    # premier Y ; ceux d'un autre Y sous coef[<y>].x ; la constante, sans coefficient centré-réduit, n'en a pas)
    co <- s$coefficients
    for (j in seq_along(ny)) {
      est <- co[[paste0("CoeffCS ", ny[j])]]
      demi <- co[[paste0("CoeffCS ", ny[j], " IC")]]
      if (is.null(est)) next
      E <- c(E, lapply(seq_len(nrow(co)), function(i) {
        t <- as.character(co$terme[i])
        entree_resultat(if (j == 1) paste0("coef.", t) else sprintf("coef[%s].%s", ny[j], t),
                        if (length(ny) > 1) sprintf(traduire("Coefficient de %s pour %s", "Coefficient of %s for %s"), g(t), g(ny[j])) else sprintf(traduire("Coefficient de %s", "Coefficient of %s"), g(t)),
                        "variables", est[i], ic = est[i] + c(-1, 1) * demi[i], limite = 0)
      }))
    }
  } else {
    E <- c(E, unlist(lapply(seq_len(A), function(a) {
      pa <- v[[paste0("p", a)]]
      ia <- v[[paste0("p", a, "_IC")]] %||% rep(NA_real_, length(termes))
      lapply(seq_along(termes), function(i) entree_resultat(sprintf("p%d.%s", a, termes[i]), sprintf(traduire("Loading p%d de %s", "Loading p%d of %s"), a, g(termes[i])), "variables",
                                                             pa[i], ic = pa[i] + c(-1, 1) * ia[i], limite = 0, axe = a))
    }), recursive = FALSE),
    lapply(seq_along(termes), function(i) entree_resultat(paste0("R2V.", termes[i]), sprintf(traduire("R²V cumulé de %s", "Cumulative R²V of %s"), g(termes[i])), "variables", v$R2V_cum[i], forme = "pourcentage")))
  }
  E <- c(E, entrees_jeu_test(m$test))
  # (un modèle d'un fichier .usp : ce que le fichier a enregistré pour ce nombre de composantes)
  ref <- d[["reference"]][["components"]]
  if (length(ref) >= A) {
    lu <- function(k) ref[[A]][[k]]
    E <- c(E, list(entree_resultat("stocke.R2X", traduire("R²X cumulé enregistré dans le fichier", "Cumulative R²X stored in the file"), "stocke", lu("R2X(cum)"), forme = "pourcentage"),
                   if (pls) entree_resultat("stocke.R2Y", traduire("R²Y cumulé enregistré dans le fichier", "Cumulative R²Y stored in the file"), "stocke", lu("R2Y(cum)"), forme = "pourcentage"),
                   entree_resultat("stocke.Q2", traduire("Q² cumulé enregistré dans le fichier", "Cumulative Q² stored in the file"), "stocke", lu("Q2(cum)"))))
  }
  E
}

# Le jeu de test d'un modèle (jeu_test.R ; regression.R, arbres.R, forets.R… : tester_<type>) : RMSEP,
# Q² externe et biais de chaque Y, ou la part bien classée ; une ACP, ses observations hors du domaine.
entrees_jeu_test <- function(t) {
  if (is.null(t)) return(list())
  st <- t$statistiques
  E <- list()
  if (!is.null(st) && nrow(st)) {
    for (j in seq_len(nrow(st))) {
      suffixe <- if (j == 1) "" else paste0(".", st$y[j])
      de_y <- if (nrow(st) > 1) sprintf(traduire(" de %s", " of %s"), nom_resultat(st$y[j])) else ""
      E <- c(E, list(entree_resultat(paste0("jeu_test.RMSEP", suffixe), paste0(traduire("RMSEP du jeu de test", "Test set RMSEP"), de_y), "jeu_test", st$RMSEP[j], n = st$n[j]),
                     entree_resultat(paste0("jeu_test.Q2", suffixe), paste0(traduire("Q² externe du jeu de test", "Test set external Q²"), de_y), "jeu_test", st$Q2ext[j], n = st$n[j]),
                     entree_resultat(paste0("jeu_test.biais", suffixe), paste0(traduire("Biais du jeu de test", "Test set bias"), de_y), "jeu_test", st$biais[j], n = st$n[j])))
    }
  }
  if (!is.null(t$classement)) {
    E <- c(E, list(entree_resultat("jeu_test.bien_classes", traduire("Part du jeu de test bien classée", "Share of the test set correctly classified"), "jeu_test",
                                   t$classement$bien_classees, n = t$n, forme = "pourcentage", genre = "f")))
  }
  if (!length(E)) {
    E <- list(entree_resultat("jeu_test.hors_domaine", traduire("Nombre d'observations du jeu de test hors du domaine du modèle", "Number of test set observations outside the model's domain"),
                              "jeu_test", t$hors_domaine, n = t$n, forme = "entier"))
  }
  E
}

# ---------------------------------------------------------------- régressions

# Une régression (regression.R) : les valeurs que ses documents citent (references.js :
# VALEURS_REGRESSION, et chaque coefficient sous coef.<terme>), les paramètres de sa loi, ses courbes
# (edf et test), les écarts-types de ses effets aléatoires et φ de CAR1, son jeu de test ; ses moyennes
# ajustées et la puissance de sa tendance quand elles sont gardées avec elle (un réajustement les refait).
entrees_regression <- function(projet, d, m) {
  r <- m$resultat
  g <- nom_resultat
  # (chaque valeur lue par son nom exact : `$` prendrait le R² ajusté d'un GLM pour son R², qu'il n'a pas)
  r2 <- r[["r2"]] %||% list()
  E <- list(entree_n(m$N),
            entree_resultat("K", traduire("Nombre de termes", "Number of terms"), "ajustement", m$K, forme = "entier"),
            entree_resultat("R2", "R²", "ajustement", r2[["r2"]]),
            entree_resultat("R2_ajuste", traduire("R² ajusté", "Adjusted R²"), "ajustement", r2[["r2_ajuste"]]),
            entree_resultat("R2_marginal", traduire("R² marginal (effets fixes)", "Marginal R² (fixed effects)"), "ajustement", r2[["marginal"]]),
            entree_resultat("R2_conditionnel", traduire("R² conditionnel (avec les effets aléatoires)", "Conditional R² (with the random effects)"), "ajustement", r2[["conditionnel"]]),
            entree_resultat("deviance_expliquee", traduire("Part de la déviance expliquée", "Share of the deviance explained"), "ajustement", r2[["deviance_expliquee"]],
                            forme = "pourcentage", genre = "f"),
            entree_resultat("Q2", traduire("Q² en validation croisée", "Cross-validated Q²"), "ajustement", m$vc[["Q2"]]),
            entree_resultat("RMSEcv", "RMSEcv", "ajustement", m$vc[["RMSEcv"]]),
            entree_resultat("AIC", "AIC", "ajustement", r[["AIC"]]),
            entree_resultat("AICc", "AICc", "ajustement", r[["AICc"]]),
            entree_resultat("BIC", "BIC", "ajustement", r[["BIC"]]),
            entree_resultat("dispersion", traduire("Dispersion (khi² de Pearson sur ses degrés de liberté)", "Dispersion (Pearson chi-square over its degrees of freedom)"), "ajustement",
                            r[["dispersion"]], genre = "f"))
  pl <- r$parametre_loi
  if (!is.null(pl)) {
    noms <- list(theta = c(traduire("θ de la binomiale négative", "θ of the negative binomial"), ""), p = c(traduire("Puissance p de Tweedie", "Power p of the Tweedie distribution"), "f"),
                 theta1 = c(traduire("θ₁ du Poisson à excès de zéros", "θ₁ of the zero-inflated Poisson"), ""), theta2 = c(traduire("θ₂ du Poisson à excès de zéros", "θ₂ of the zero-inflated Poisson"), ""),
                 phi = c(traduire("Précision φ de la loi bêta", "Precision φ of the beta distribution"), "f"))
    E <- c(E, lapply(seq_along(pl$nom), function(k) {
      x <- noms[[as.character(pl$nom[k])]] %||% c(as.character(pl$nom[k]), "")
      entree_resultat(paste0("loi.", pl$nom[k]), x[1], "ajustement", pl$valeur[k], genre = if (nzchar(x[2])) x[2])
    }))
  }
  co <- r$coefficients
  E <- c(E, lapply(seq_len(nrow(co)), function(i) {
    t <- as.character(co$terme[i])
    constante <- identical(t, "(Intercept)")
    entree_resultat(paste0("coef.", t), if (constante) traduire("Ordonnée à l'origine", "Intercept") else sprintf(traduire("Coefficient de %s", "Coefficient of %s"), g(t)), "estimations",
                    co$estimation[i], ic = c(co$bas[i], co$haut[i]), p = co$p[i], limite = 0, genre = if (constante) "f", alias = paste0(c("est.", "ic.", "p."), t))
  }))
  cb <- r$courbes
  if (!is.null(cb) && nrow(cb)) {
    noms <- if (!is.null(cb$niveau)) ifelse(is.na(cb$niveau), cb$terme, sprintf("%s (%s %s)", cb$terme, cb$par, cb$niveau)) else cb$terme
    E <- c(E, lapply(seq_len(nrow(cb)), function(i) entree_resultat(paste0("courbe.", noms[i]), sprintf(traduire("Courbe de %s (edf)", "Curve of %s (edf)"), g(noms[i])), "estimations",
                                                                     cb$edf[i], p = cb$p[i], genre = "f")))
  }
  E <- c(E, entrees_variances(m), list(entree_resultat("phi", traduire("φ de l'autocorrélation CAR1", "φ of the CAR1 autocorrelation"), "aleatoires", r[["phi"]])),
         entrees_jeu_test(m[["test"]]), entrees_moyennes(m[["moyennes"]]), entrees_puissance(m[["puissance_tendance"]]))
  E
}

# Les écarts-types des effets aléatoires d'une régression (ses variances : regression.R,
# tableau_variances), nommés par leur groupement (un nom des données) et, pour une pente aléatoire,
# par son terme ; la corrélation d'une pente avec l'ordonnée à l'origine de son niveau. La clé ne
# prend pas le nom traduit de la résiduelle (variances_regression) : elle ne change pas avec la langue.
entrees_variances <- function(m) {
  v <- m$resultat$variances
  if (is.null(v) || !nrow(v)) return(list())
  g <- nom_resultat
  niveau <- function(k) if (identical(k, "residuelle")) k else m$niveaux[match(k, paste0("g", seq_along(m$niveaux)))]
  terme <- function(t) {
    if (is.null(t) || is.na(t) || !nzchar(t) || identical(t, "(Intercept)")) return("")
    nom <- m$noms_termes[match(t, m$termes)]
    if (is.na(nom)) t else nom
  }
  unlist(lapply(seq_len(nrow(v)), function(i) {
    k <- niveau(as.character(v$composante[i]))
    t <- terme(if (!is.null(v$terme)) as.character(v$terme[i]))
    lib <- if (identical(k, "residuelle")) traduire("Écart-type résiduel", "Residual standard deviation")
           else if (nzchar(t)) sprintf(traduire("Écart-type de la pente de %s selon %s", "Standard deviation of the slope of %s by %s"), g(t), g(k))
           else sprintf(traduire("Écart-type de l'effet aléatoire %s", "Standard deviation of the random effect %s"), g(k))
    list(entree_resultat(paste0("sigma.", k, if (nzchar(t)) paste0(".", t)), lib, "aleatoires", sqrt(v$variance[i])),
         if (nzchar(t) && !is.null(v$correlation)) entree_resultat(paste0("rho.", k, ".", t), sprintf(traduire("Corrélation de la pente de %s avec l'ordonnée à l'origine selon %s", "Correlation of the slope of %s with the intercept by %s"), g(t), g(k)),
                                                                   "aleatoires", v$correlation[i], limite = 0, genre = "f"))
  }), recursive = FALSE)
}

# Les moyennes ajustées d'une régression, gardées avec elle (moyennes.R : moyennes_ajustees) : chaque
# moyenne (ou pente par catégorie) avec son intervalle, chaque paire avec son intervalle et sa valeur p
# ajustée ; leurs clés nomment le facteur, pour que d'autres réglages ne se comparent pas à elles.
entrees_moyennes <- function(mo) {
  if (is.null(mo$moyennes) || !nrow(mo$moyennes)) return(list())
  g <- nom_resultat
  f <- mo$facteur$nom
  pentes <- identical(mo$mode, "pentes")
  base <- if (pentes) paste0("pente.", mo$pente$nom, ".", f) else paste0("moyenne.", f)
  mm <- mo$moyennes
  cellule <- ifelse(is.na(mm$groupe), mm$niveau, paste(mm$niveau, mm$groupe, sep = " · "))
  E <- lapply(seq_len(nrow(mm)), function(i) entree_resultat(paste0(base, ".", cellule[i]),
    if (pentes) sprintf(traduire("Pente de %s pour %s = %s", "Slope of %s for %s = %s"), g(mo$pente$nom), g(f), cellule[i]) else sprintf(traduire("Moyenne ajustée de %s = %s", "Adjusted mean of %s = %s"), g(f), cellule[i]),
    "moyennes", mm$estimation[i], ic = c(mm$bas[i], mm$haut[i]), limite = if (pentes) 0, genre = "f"))
  pa <- mo$paires
  if (is.null(pa) || !nrow(pa)) return(E)
  # (sur l'échelle de la réponse d'un lien log ou logit, une paire est un rapport : sa valeur nulle est 1)
  rapport <- identical(mo$echelle, "reponse")
  paire <- ifelse(is.na(pa$groupe), sprintf("%s − %s", pa$niveau_1, pa$niveau_2), sprintf("%s − %s (%s)", pa$niveau_1, pa$niveau_2, pa$groupe))
  c(E, lapply(seq_len(nrow(pa)), function(i) entree_resultat(paste0(if (pentes) "paire_pente." else "paire.", if (pentes) paste0(mo$pente$nom, ".") else "", f, ".", paire[i]),
    sprintf(if (rapport) traduire("Rapport %s de %s", "Ratio %s of %s") else traduire("Différence %s de %s", "Difference %s of %s"), paire[i], g(if (pentes) mo$pente$nom else f)),
    "moyennes", pa$estimation[i], ic = c(pa$bas[i], pa$haut[i]), p = pa$p[i], limite = if (rapport) 1 else 0, genre = if (!rapport) "f")))
}

# La puissance pour détecter une tendance, gardée avec la régression (puissance.R) : les années qu'il
# faut pour 80 % avec les unités des données, et la puissance sur leur nombre d'années.
entrees_puissance <- function(pt) {
  if (is.null(pt$tableau) || !nrow(pt$tableau)) return(list())
  u <- pt$plan$unites
  a80 <- pt$annees_80$annees[match(u, pt$annees_80$unites)]
  ici <- pt$tableau[pt$tableau$unites == u & pt$tableau$annees == pt$plan$annees, , drop = FALSE]
  list(entree_resultat("puissance.annees_80", traduire("Nombre d'années pour une puissance de 80 %", "Number of years for 80% power"), "puissance", a80, forme = "entier"),
       entree_resultat("puissance", traduire("Puissance sur les années des données", "Power over the data's years"), "puissance", if (nrow(ici)) ici$puissance[1], forme = "pourcentage", genre = "f"))
}

# ---------------------------------------------------------------- autres analyses (analyses.R)

# La part de l'inertie totale portée par les premiers axes d'une ordination (`valeurs` nommées par axe).
entrees_axes <- function(valeurs, totale, libelle = traduire("Part de l'inertie sur %s", "Share of the inertia on %s")) {
  axes <- utils::head(names(valeurs), AXES_RESULTATS_CLES)
  lapply(axes, function(a) entree_resultat(paste0("axe.", a), sprintf(libelle, a), "ajustement", valeurs[[a]] / totale, forme = "pourcentage", genre = "f"))
}

# Une RDA ou une CCA (ordination.R) : R², R² ajusté, inertie, part des covariables d'une analyse
# partielle, le test du modèle par permutations, la part de l'inertie de ses premiers axes contraints.
entrees_rda <- function(projet, d, m) {
  k <- m$noyau
  c(list(entree_n(m$N),
         entree_resultat("K", traduire("Nombre de colonnes explicatives", "Number of explanatory columns"), "ajustement", ncol(m$donnees$X), forme = "entier"),
         entree_resultat("R2", traduire("R² (part de l'inertie expliquée)", "R² (share of the inertia explained)"), "ajustement", m$r2),
         entree_resultat("R2_ajuste", traduire("R² ajusté", "Adjusted R²"), "ajustement", m$r2_ajuste),
         entree_resultat("inertie.totale", traduire("Inertie totale", "Total inertia"), "ajustement", k$totale, genre = "f"),
         if (length(m$Z$noms)) entree_resultat("inertie.covariables", traduire("Part de l'inertie des covariables Z", "Share of the inertia of the covariates Z"), "ajustement",
                                               k$conditionnelle / k$totale, forme = "pourcentage", genre = "f"),
         entree_resultat("test.modele", traduire("Pseudo-F du modèle", "Pseudo-F of the model"), "tests", m$test$F, p = m$test$p)),
    entrees_axes(k$contraint$eig, k$totale))
}
entrees_cca <- function(projet, d, m) entrees_rda(projet, d, m)

entrees_ca <- function(projet, d, m) {
  k <- m$noyau
  c(list(entree_n(m$N),
         entree_resultat("K", traduire("Nombre de colonnes du tableau", "Number of columns of the table"), "ajustement", length(m$colonnes), forme = "entier"),
         entree_resultat("inertie.totale", traduire("Inertie totale", "Total inertia"), "ajustement", k$totale, genre = "f")),
    entrees_axes(k$eig, k$totale))
}

# Les variables X ajustées sur une PCoA ou un NMDS (communautes.R : ajuster_variables, comme envfit) :
# le R² et la valeur p de chacune.
entrees_ajustement <- function(a) {
  if (is.null(a)) return(list())
  g <- nom_resultat
  lib <- function(x) sprintf(traduire("R² de %s ajusté à l'ordination", "R² of %s fitted onto the ordination"), g(x))
  v <- a$vecteurs
  c(if (!is.null(v)) lapply(names(v$r2), function(t) entree_resultat(paste0("ajuste.", t), lib(t), "tests", v$r2[[t]], p = v$p[[t]], n = a$n)),
    lapply(a$facteurs, function(x) entree_resultat(paste0("ajuste.", x$variable), lib(x$variable), "tests", x$r2, p = x$p, n = a$n)))
}

entrees_pcoa <- function(projet, d, m) {
  k <- m$noyau
  vp <- table_valeurs_pcoa(k)
  positifs <- vp[!vp$negative, , drop = FALSE]
  c(list(entree_n(m$N), entree_resultat("K", traduire("Nombre de variables Y", "Number of Y variables"), "ajustement", m$K, forme = "entier")),
    entrees_axes(stats::setNames(positifs$valeur_propre, positifs$axe), k$totale), entrees_ajustement(m$ajustement))
}

entrees_nmds <- function(projet, d, m) {
  c(list(entree_n(m$N), entree_resultat("K", traduire("Nombre de variables Y", "Number of Y variables"), "ajustement", m$K, forme = "entier"),
         entree_resultat("stress", "Stress", "ajustement", m$noyau$stress)),
    entrees_ajustement(m$ajustement))
}

# Une PERMANOVA : le R² et la valeur p du modèle entier et de chaque terme testé (ses lignes, sauf les
# résidus et le total ; le modèle seul en a une, nommée dans la langue des messages, que sa clé ne
# prend pas), et le test de la dispersion de chaque groupement.
entrees_permanova <- function(projet, d, m) {
  g <- nom_resultat
  k <- m$noyau
  tb <- k$table
  termes <- if (m$options$par %in% c("termes", "marges")) tb[seq_len(nrow(tb) - 2), , drop = FALSE] else tb[0, , drop = FALSE]
  c(list(entree_n(m$N), entree_resultat("K", traduire("Nombre de variables Y", "Number of Y variables"), "ajustement", m$K, forme = "entier"),
         entree_resultat("permanova.modele", traduire("R² du modèle (PERMANOVA)", "R² of the model (PERMANOVA)"), "tests", k$modele$R2, p = k$modele$p)),
    lapply(seq_len(nrow(termes)), function(i) entree_resultat(paste0("permanova.", termes$terme[i]), sprintf(traduire("R² de %s (PERMANOVA)", "R² of %s (PERMANOVA)"), g(termes$terme[i])), "tests",
                                                              termes$R2[i], p = termes$p[i])),
    lapply(m$dispersions, function(r) entree_resultat(paste0("dispersion.", r$variable), sprintf(traduire("F de la dispersion selon %s", "Dispersion F by %s"), g(r$variable)), "tests",
                                                      r$F, p = if (is.finite(r$p_permutation)) r$p_permutation else r$p, n = length(r$observations))))
}

entrees_mantel <- function(projet, d, m) {
  t <- m$test
  list(entree_n(m$N), entree_resultat("K", traduire("Nombre de variables Y", "Number of Y variables"), "ajustement", m$K, forme = "entier"),
       entree_resultat("mantel", traduire("Corrélation r du test de Mantel", "Correlation r of the Mantel test"), "tests", t$r, p = t$p, limite = 0, genre = "f"),
       entree_resultat("mantel.partiel", traduire("Corrélation partielle r du test de Mantel", "Partial correlation r of the Mantel test"), "tests", t$r_partiel, p = t$p_partiel, limite = 0, genre = "f"))
}

# Les espèces indicatrices (indicatrices.R) : leur nombre au seuil de 5 % (p corrigées), et pour chaque
# espèce sa valeur indicatrice, sa valeur p corrigée (celle qui la dit indicatrice) et le groupe (ou
# la combinaison de groupes) qu'elle indique : changer de groupe est un changement de catégorie.
entrees_indval <- function(projet, d, m) {
  g <- nom_resultat
  groupes <- colnames(m$combinaisons)[m$meilleure]
  presences <- colSums(m$donnees > 0)
  c(list(entree_n(m$N), entree_resultat("K", traduire("Nombre d'espèces", "Number of species"), "ajustement", length(m$especes), forme = "entier"),
         if (m$reglages_indval$permutations > 0) entree_resultat("indicatrices", traduire("Nombre d'espèces indicatrices (p corrigée ≤ 0,05)", "Number of indicator species (corrected p ≤ 0.05)"),
                                                                 "ajustement", retenues_indval(m)[["corrigees"]], forme = "entier")),
    lapply(seq_along(m$especes), function(i) entree_resultat(paste0("indval.", m$especes[i]), sprintf(traduire("Valeur indicatrice de %s", "Indicator value of %s"), g(m$especes[i])), "especes",
                                                             m$valeur[i], p = m$p_ajuste[i], n = presences[[i]], classe = groupes[i], genre = "f")))
}

# TRIM (trim.R) : la pente globale des totaux imputés, celle que TRIM rapporte (multiplicative, son
# intervalle, sa valeur p, sa classe ; celle des totaux du modèle, presque la même, n'en dirait pas
# plus), les indices des totaux imputés (± 1,96 erreur-type, comme la vue), les pentes d'un modèle 2
# entre ses points de changement, les tests de Wald, l'ajustement, σ² et ρ.
entrees_trim <- function(projet, d, m) {
  fr <- !identical(getOption("nuee.langue"), "en")
  g <- nom_resultat
  z <- stats::qnorm(0.975)
  temps <- function(x) trimws(formatC(x, format = "fg", digits = 15))
  temps_lu <- function(x) trimws(formatC(x, format = "fg", digits = 15, decimal.mark = if (fr) "," else "."))
  pente <- function(cle, quoi, libelle) {
    l <- m$pentes[m$pentes$quoi == quoi, , drop = FALSE]
    if (nrow(l)) entree_resultat(cle, libelle, "estimations", l$multiplicative[1], ic = c(l$bas[1], l$haut[1]), p = l$p[1], limite = 1, genre = "f",
                                 classe = l$classe[1], classe_texte = libelle_classe_trim(l$classe[1], fr))
  }
  a <- m$ajustement
  E <- list(entree_n(m$N),
            entree_resultat("sites", traduire("Nombre de sites", "Number of sites"), "ajustement", m$I, forme = "entier"),
            entree_resultat("temps", traduire("Nombre de temps", "Number of times"), "ajustement", m$J, forme = "entier"),
            pente("pente", "imputee", traduire("Pente globale (totaux imputés)", "Overall slope (imputed totals)")),
            entree_resultat("sigma2", traduire("Surdispersion σ²", "Overdispersion σ²"), "ajustement", m$sigma2, genre = "f"),
            entree_resultat("rho", traduire("Autocorrélation ρ d'un temps au suivant", "Serial correlation ρ"), "ajustement", m$rho, genre = "f"),
            entree_resultat("AIC", "AIC", "ajustement", a$AIC),
            entree_resultat("ajustement.khi2", traduire("Khi² de Pearson de l'ajustement", "Goodness-of-fit Pearson chi-square"), "tests", a$chi2, p = a$p_chi2),
            entree_resultat("ajustement.LR", traduire("Rapport de vraisemblance de l'ajustement", "Goodness-of-fit likelihood ratio"), "tests", a$LR, p = a$p_LR))
  ix <- m$indices
  E <- c(E, lapply(setdiff(seq_len(nrow(ix)), m$base), function(j) entree_resultat(paste0("indice.", temps(ix$temps[j])), sprintf(traduire("Indice de %s", "Index of %s"), temps_lu(ix$temps[j])),
                                                                                    "estimations", ix$impute[j], ic = ix$impute[j] + c(-1, 1) * z * ix$se_impute[j], limite = 1)))
  pa <- m$parametres
  if (identical(as.numeric(m$modele), 2) && !is.null(pa) && nrow(pa)) {
    E <- c(E, lapply(seq_len(nrow(pa)), function(i) {
      bloc <- if (!is.na(pa$covariable[i])) sprintf("%s = %s", pa$covariable[i], pa$categorie[i])
      entree_resultat(paste0("pente.", temps(pa$de[i]), "-", temps(pa$a[i]), if (!is.null(bloc)) paste0(".", bloc)),
                      paste0(sprintf(traduire("Pente de %s à %s", "Slope from %s to %s"), temps_lu(pa$de[i]), temps_lu(pa$a[i])), if (!is.null(bloc)) sprintf(traduire(" pour %s", " for %s"), bloc) else ""),
                      "estimations", pa$multiplicatif[i], ic = exp(pa$additif[i] + c(-1, 1) * z * pa$se_additif[i]), limite = 1, genre = "f")
    }))
  }
  tw <- m$tests
  E <- c(E, lapply(seq_len(nrow(tw)), function(i) {
    quoi <- as.character(tw$quoi[i])
    x <- switch(as.character(tw$test[i]),
      pente = list("wald.pente", traduire("Test de la pente (Wald)", "Slope test (Wald)")),
      changement = list(paste0("wald.changement.", quoi), sprintf(traduire("Test du changement de pente en %s (Wald)", "Test of the slope change at %s (Wald)"), quoi)),
      deviations = list("wald.deviations", traduire("Test des écarts à la droite (Wald)", "Test of the deviations from the line (Wald)")),
      covariable = list(paste0("wald.covariable.", quoi), sprintf(traduire("Test de la covariable %s (Wald)", "Test of the covariate %s (Wald)"), g(quoi))),
      list(paste0("wald.", tw$test[i], ".", quoi), sprintf("%s %s (Wald)", tw$test[i], quoi)))
    entree_resultat(x[[1]], x[[2]], "tests", tw$W[i], p = tw$p[i])
  }))
  E
}

# L'échantillonnage par distances (distance.R) : la fonction de détection retenue (une catégorie :
# en changer est un changement de modèle), son AIC, la probabilité de détection et ESW ou EDR, la
# densité et l'effectif au total et par strate avec leurs intervalles, l'adéquation, les coefficients
# des covariables sur ln σ.
entrees_distance <- function(projet, d, m) {
  g <- nom_resultat
  t <- m$donnees
  det <- m$detection
  dens <- m$densite
  tot <- dens[nrow(dens), ]
  strates <- nrow(dens) > 1
  esw <- identical(m$effectif$quoi, "ESW")
  E <- list(entree_n(m$N),
            entree_resultat("detections", traduire("Nombre de détections", "Number of detections"), "ajustement", t$n, forme = "entier"),
            entree_resultat("modele", traduire("Fonction de détection retenue", "Detection function chosen"), "ajustement", classe = libelle_detection(det$fd), genre = "f"),
            entree_resultat("AIC", "AIC", "ajustement", det$AIC),
            entree_resultat("p", traduire("Probabilité moyenne de détection", "Average probability of detection"), "estimations", m$p, genre = "f"),
            entree_resultat(m$effectif$quoi, if (esw) traduire("Demi-largeur effective de bande (ESW)", "Effective strip half-width (ESW)") else traduire("Rayon effectif de détection (EDR)", "Effective detection radius (EDR)"),
                            "estimations", m$effectif$valeur, genre = if (esw) "f"),
            entree_resultat("D", if (strates) traduire("Densité totale", "Overall density") else traduire("Densité", "Density"), "estimations", tot$D, ic = c(tot$bas_D, tot$haut_D), n = tot$n, genre = "f"),
            entree_resultat("abondance", if (strates) traduire("Effectif total", "Overall abundance") else traduire("Effectif", "Abundance"), "estimations", tot$N, ic = c(tot$bas_N, tot$haut_N), n = tot$n),
            entree_resultat("adequation.cvm", traduire("Adéquation (Cramér–von Mises)", "Goodness of fit (Cramér–von Mises)"), "tests", m$adequation$cvm$W, p = m$adequation$cvm$p, genre = "f"),
            entree_resultat("adequation.khi2", traduire("Adéquation (khi²)", "Goodness of fit (chi-square)"), "tests", m$adequation$khi2$chi2, p = m$adequation$khi2$p, genre = "f"))
  if (strates) {
    st <- dens[!is.na(dens$strate), , drop = FALSE]
    E <- c(E, lapply(seq_len(nrow(st)), function(i) entree_resultat(paste0("D.", st$strate[i]), sprintf(traduire("Densité de la strate %s", "Density of the stratum %s"), g(st$strate[i])), "estimations",
                                                                     st$D[i], ic = c(st$bas_D[i], st$haut_D[i]), n = st$n[i], genre = "f")))
  }
  pa <- m$parametres
  if (!is.null(det$fd$formule) && !is.null(pa)) {
    pa <- pa[pa$parametre %in% det$fd$noms_echelle[-1], , drop = FALSE]
    E <- c(E, lapply(seq_len(nrow(pa)), function(i) entree_resultat(paste0("coef.", pa$parametre[i]), sprintf(traduire("Coefficient de %s sur ln σ", "Coefficient of %s on ln σ"), g(pa$parametre[i])),
                                                                     "estimations", pa$estimation[i], limite = 0)))
  }
  E
}

# Une forêt aléatoire (forets.R) : son erreur hors du sac et sa validation croisée, l'importance par
# permutation de chaque variable, et la plus importante (une catégorie : qu'une autre la remplace se
# dit, quand les importances, sans intervalle, n'ont pas de drapeau).
entrees_forest <- function(projet, d, m) {
  g <- nom_resultat
  qualite <- if (m$classement) {
    list(entree_resultat("bien_classes_oob", traduire("Part bien classée hors du sac", "Share correctly classified out of bag"), "ajustement", 1 - m$oob$erreur, forme = "pourcentage", genre = "f"),
         entree_resultat("bien_classes_vc", traduire("Part bien classée en validation croisée", "Share correctly classified in cross-validation"), "ajustement", m$classification$vc$global, forme = "pourcentage", genre = "f"))
  } else {
    list(entree_resultat("R2_oob", traduire("R² hors du sac", "Out-of-bag R²"), "ajustement", m$oob$r2),
         entree_resultat("Q2", traduire("Q² en validation croisée", "Cross-validated Q²"), "ajustement", m$vc$Q2),
         entree_resultat("RMSEcv", "RMSEcv", "ajustement", m$vc$RMSEcv))
  }
  im <- m$importance
  c(list(entree_n(m$N), entree_resultat("K", traduire("Nombre de variables X", "Number of X variables"), "ajustement", m$K, forme = "entier")), qualite,
    list(entree_resultat("variable_principale", traduire("Variable la plus importante (permutation)", "Most important variable (permutation)"), "estimations",
                         classe = im$variable[which.max(im$permutation)], genre = "f")),
    lapply(seq_len(nrow(im)), function(i) entree_resultat(paste0("importance.", im$variable[i]), sprintf(traduire("Importance de %s (permutation)", "Importance of %s (permutation)"), g(im$variable[i])),
                                                          "variables", im$permutation[i], genre = "f")),
    entrees_jeu_test(m$test))
}

# Des arbres boostés (boosting.R) : le nombre d'arbres retenu, la déviance expliquée, R² et Q² d'une
# réponse normale, l'AUC d'une présence, l'influence relative de chaque variable et la plus influente.
entrees_boost <- function(projet, d, m) {
  g <- nom_resultat
  q <- m$qualite
  im <- m$influence
  # (par leur nom exact : `$` prendrait l'AUC en validation croisée pour celle de l'ajustement)
  c(list(entree_n(m$N), entree_resultat("K", traduire("Nombre de variables X", "Number of X variables"), "ajustement", m$K, forme = "entier"),
         entree_resultat("arbres", traduire("Nombre d'arbres retenu", "Number of trees chosen"), "ajustement", m$meilleur, forme = "entier"),
         entree_resultat("D2", traduire("Part de la déviance expliquée (D²)", "Share of the deviance explained (D²)"), "ajustement", q[["D2"]], forme = "pourcentage", genre = "f"),
         entree_resultat("D2_vc", traduire("Part de la déviance expliquée en validation croisée (D²)", "Cross-validated share of the deviance explained (D²)"), "ajustement", q[["D2_vc"]],
                         forme = "pourcentage", genre = "f"),
         entree_resultat("R2", "R²", "ajustement", q[["r2"]]),
         entree_resultat("Q2", traduire("Q² en validation croisée", "Cross-validated Q²"), "ajustement", q[["Q2"]]),
         entree_resultat("RMSEcv", "RMSEcv", "ajustement", q[["RMSEcv"]]),
         entree_resultat("AUC", "AUC", "ajustement", q[["auc"]]),
         entree_resultat("AUC_vc", traduire("AUC en validation croisée", "Cross-validated AUC"), "ajustement", q[["auc_vc"]]),
         entree_resultat("variable_principale", traduire("Variable la plus influente", "Most influential variable"), "estimations",
                         classe = if (sum(im$influence) > 0) im$variable[which.max(im$influence)], genre = "f")),
    lapply(seq_len(nrow(im)), function(i) entree_resultat(paste0("influence.", im$variable[i]), sprintf(traduire("Influence relative de %s", "Relative influence of %s"), g(im$variable[i])),
                                                          "variables", im$pourcentage[i] / 100, forme = "pourcentage", genre = "f")),
    entrees_jeu_test(m$test))
}

# Un arbre de décision (arbres.R) : ses feuilles, sa qualité, la variable de sa première coupure (une
# catégorie : qu'elle change, l'arbre a changé de tête), l'importance de chaque variable.
entrees_tree <- function(projet, d, m) {
  g <- nom_resultat
  a <- m$ajustement
  qualite <- if (m$classement) {
    list(entree_resultat("bien_classes", traduire("Part bien classée", "Share correctly classified"), "ajustement", m$classification$ajuste$global, forme = "pourcentage", genre = "f"),
         entree_resultat("bien_classes_vc", traduire("Part bien classée en validation croisée", "Share correctly classified in cross-validation"), "ajustement", m$classification$vc$global, forme = "pourcentage", genre = "f"))
  } else {
    list(entree_resultat("R2", "R²", "ajustement", m$r2),
         entree_resultat("Q2", traduire("Q² en validation croisée", "Cross-validated Q²"), "ajustement", m$vc$Q2),
         entree_resultat("RMSEcv", "RMSEcv", "ajustement", m$vc$RMSEcv))
  }
  im <- importance_arbre(a, m$variables)
  c(list(entree_n(m$N), entree_resultat("K", traduire("Nombre de variables X", "Number of X variables"), "ajustement", m$K, forme = "entier"),
         entree_resultat("feuilles", traduire("Nombre de feuilles", "Number of leaves"), "ajustement", sum(a$frame$var == "<leaf>"), forme = "entier")), qualite,
    list(entree_resultat("premiere_coupure", traduire("Variable de la première coupure", "First split variable"), "estimations", classe = premiere_coupure(a, m$variables)$variable, genre = "f")),
    lapply(seq_len(nrow(im)), function(i) entree_resultat(paste0("importance.", im$variable[i]), sprintf(traduire("Importance de %s", "Importance of %s"), g(im$variable[i])), "variables",
                                                          im$pourcentage[i] / 100, forme = "pourcentage", genre = "f")),
    entrees_jeu_test(m$test))
}

# Une analyse discriminante linéaire (discrimination.R) : la part bien classée, ajustée et en
# validation croisée, au total et par classe, et la part de la variance discriminante de ses axes.
entrees_lda <- function(projet, d, m) {
  g <- nom_resultat
  r <- m$classification
  effectifs <- stats::setNames(m$effectifs, m$classes)
  c(list(entree_n(m$N), entree_resultat("K", traduire("Nombre de termes X", "Number of X terms"), "ajustement", m$K, forme = "entier"),
         entree_resultat("bien_classes", traduire("Part bien classée", "Share correctly classified"), "ajustement", r$ajuste$global, forme = "pourcentage", genre = "f"),
         entree_resultat("bien_classes_vc", traduire("Part bien classée en validation croisée", "Share correctly classified in cross-validation"), "ajustement", r$vc$global, forme = "pourcentage", genre = "f")),
    lapply(names(r$vc$correct), function(k) entree_resultat(paste0("bien_classes.", k), sprintf(traduire("Part bien classée de %s en validation croisée", "Share of %s correctly classified in cross-validation"), g(k)),
                                                            "ajustement", r$vc$correct[[k]], n = if (k %in% names(effectifs)) effectifs[[k]], forme = "pourcentage", genre = "f")),
    entrees_axes(stats::setNames(as.numeric(m$part), m$axes), 1, traduire("Part de la variance discriminante sur %s", "Share of the discriminant variance on %s")),
    entrees_jeu_test(m$test))
}

entrees_kmeans <- function(projet, d, m) {
  sil <- if (!is.null(m$silhouette)) mean(m$silhouette$largeur)
  list(entree_n(m$N), entree_resultat("K", traduire("Nombre de termes X", "Number of X terms"), "ajustement", m$K, forme = "entier"),
       entree_resultat("k", traduire("Nombre de groupes", "Number of clusters"), "ajustement", m$k, forme = "entier"),
       entree_resultat("R2", traduire("R² (inertie entre les groupes sur l'inertie totale)", "R² (between-cluster over total inertia)"), "ajustement", m$r2),
       entree_resultat("silhouette", traduire("Largeur moyenne de silhouette", "Average silhouette width"), "ajustement", sil, genre = "f"))
}

# ---------------------------------------------------------------- la comparaison

# Ce que pèse un changement selon le groupe de son entrée, pour l'importance : l'ajustement, les
# estimations et les tests d'abord ; les valeurs de chaque variable, nombreuses, et les composantes,
# que l'ajustement résume déjà, moins ; les résultats enregistrés dans un fichier presque pas.
POIDS_GROUPES_RESULTATS <- c(ajustement = 1, estimations = 1, tests = 1, aleatoires = 0.6, moyennes = 0.8, puissance = 0.6, jeu_test = 0.8,
                             especes = 0.6, variables = 0.3, composantes = 0.3, stocke = 0.1)

# Les entrées d'un résumé telles qu'une demande les rend (jsonlite en fait un data.frame, ses
# intervalles et ses alias des colonnes de listes, une valeur absente NA), ou telles que
# resume_resultats les rend : une liste d'entrées aux champs de entree_resultat, sans ceux qui
# manquent. `x` peut aussi être le résultat entier de la demande resultats_cles, { modele, entrees }.
entrees_recues <- function(x) {
  if (is.list(x) && !is.data.frame(x) && is.null(x$cle) && !is.null(x$entrees)) x <- x$entrees
  if (is.null(x) || !length(x)) return(list())
  # (une entrée par ligne, lue colonne par colonne : extraire les lignes d'un data.frame une à une
  # prendrait des secondes pour les milliers de loadings d'une ACP de spectres)
  if (is.data.frame(x)) {
    colonnes <- as.list(x)
    x <- lapply(seq_len(nrow(x)), function(i) lapply(colonnes, function(v) if (is.list(v)) v[[i]] else v[i]))
  } else if (!is.null(x$cle)) x <- list(x)
  texte <- function(v) {
    v <- as.character(unlist(v))
    if (length(v) && !is.na(v[1]) && nzchar(v[1])) v[1]
  }
  # (chaque champ par son nom exact : `$` prendrait classe_texte pour une classe absente)
  Filter(Negate(is.null), lapply(x, function(e) {
    cle <- texte(e[["cle"]])
    if (is.null(cle)) return(NULL)
    ic <- suppressWarnings(as.numeric(unlist(e[["ic"]])))
    alias <- as.character(unlist(e[["alias"]]))
    alias <- alias[!is.na(alias) & nzchar(alias)]
    Filter(Negate(is.null), list(cle = cle, libelle = texte(e[["libelle"]]) %||% cle, groupe = texte(e[["groupe"]]) %||% "", valeur = nombre_ou_rien(e[["valeur"]]),
                                 ic = if (length(ic) == 2 && all(is.finite(ic))) sort(ic), p = nombre_ou_rien(e[["p"]]), n = nombre_ou_rien(e[["n"]]),
                                 limite = nombre_ou_rien(e[["limite"]]), repere = nombre_ou_rien(e[["repere"]]), forme = texte(e[["forme"]]), genre = texte(e[["genre"]]),
                                 alias = if (length(alias)) I(alias), axe = texte(e[["axe"]]), classe = texte(e[["classe"]]),
                                 classe_texte = if (!is.null(texte(e[["classe"]]))) texte(e[["classe_texte"]])))
  }))
}

# L'ordre des entrées comparées : celui d'après, chaque entrée disparue placée après la dernière
# entrée qui la précédait avant et qui reste après (au début sans elle), les disparues dans leur ordre.
ordre_comparaison <- function(avant, apres) {
  restent <- avant %in% apres
  if (all(restent)) return(apres)
  derniere <- cummax(ifelse(restent, seq_along(avant), 0))
  ou <- ifelse(derniere > 0, match(avant[pmax(derniere, 1)], apres), 0)
  parties <- !restent
  c(apres, avant[parties])[order(c(seq_along(apres), ou[parties] + which(parties) / (length(avant) + 1)))]
}

# Les composantes sorties retournées d'un ajustement à l'autre : le signe d'une composante d'ACP est
# arbitraire, et un réajustement peut la rendre opposée, tous ses loadings changeant de signe ensemble.
# Une composante l'est quand le produit de ses valeurs d'avant et d'après, sommé sur ses entrées
# communes, est négatif.
axes_retournes <- function(avant, apres) {
  axe <- function(es) vapply(es, function(e) e$axe %||% NA_character_, "")
  cle <- function(es) vapply(es, `[[`, "", "cle")
  aa <- axe(avant)
  ap <- axe(apres)
  ca <- cle(avant)
  cp <- cle(apres)
  Filter(function(x) {
    k <- intersect(ca[aa %in% x], cp[ap %in% x])
    va <- vapply(avant[match(k, ca)], function(e) e$valeur %||% NA_real_, 0)
    vb <- vapply(apres[match(k, cp)], function(e) e$valeur %||% NA_real_, 0)
    length(k) > 0 && sum(va * vb, na.rm = TRUE) < 0
  }, unique(ap[!is.na(ap)]))
}

# L'écriture d'une ancienne valeur `a` et d'une nouvelle `b` (et de leurs intervalles), comme le texte
# des résultats l'écrit : 3 chiffres significatifs, ou plus (6 au plus) pour que leur écart se lise,
# son premier chiffre au moins, sans qu'un arrondi le grossisse (un AIC de 1234,5 à 1236,8 : de 1235 à
# 1237, pas de 1230 à 1240) ; un pourcentage à une décimale, ou plus de même. Rend une fonction qui
# écrit un nombre à cette précision.
ecriture_resultat <- function(a, b, forme, fr) {
  ecrire <- function(k) function(x) {
    if (identical(forme, "entier")) return(formatC(round(x), format = "d"))
    if (identical(forme, "pourcentage")) return(paste0(formatC(100 * x, format = "f", digits = k - 2, decimal.mark = if (fr) "," else "."), if (fr) " %" else "%"))
    nombre_resultat(x, fr, k)
  }
  if (is.null(a) || is.null(b) || a == b) return(ecrire(3))
  ecart <- abs(b - a)
  k <- if (identical(forme, "pourcentage")) 2 + max(1, -floor(log10(100 * ecart))) else floor(log10(max(abs(a), abs(b)))) + 1 - floor(log10(ecart))
  k <- max(3, min(6, k))
  for (j in k:6) if (!identical(ecrire(j)(a), ecrire(j)(b))) return(ecrire(j))
  ecrire(k)
}

# Le sujet d'une phrase : le libellé de l'entrée avec son article (« le coefficient de « x » »,
# « l'AIC », « the cumulative Q² ») ; la première lettre en minuscule quand elle commence un mot
# (pas celle d'un sigle : R², AIC, VIP).
sujet_resultat <- function(libelle, genre, fr) {
  l <- if (grepl("^.[[:lower:]]", libelle)) paste0(tolower(substr(libelle, 1, 1)), substring(libelle, 2)) else libelle
  if (!fr) return(paste("the", l))
  paste0(if (grepl("^[aeiouyàâéèêëîïôûAEIOUYÀÂÉÈÊËÎÏÔÛ]", l)) "l'" else if (identical(genre, "f")) "la " else "le ", l)
}

# Significative ou non : par sa valeur p (5 %), sinon par son intervalle, qui exclut ou non sa
# valeur nulle `limite` ; NA quand rien ne le dit.
significatif_resultat <- function(x, limite) {
  if (is.null(x)) return(NA)
  if (!is.null(x$p)) return(x$p <= 0.05)
  if (!is.null(x$ic) && !is.null(limite)) return(limite < x$ic[1] || limite > x$ic[2])
  NA
}

# Une entrée d'avant `a` et d'après `b` (l'une des deux NULL quand l'entrée n'est que d'un côté)
# comparées : les deux valeurs, l'écart absolu et relatif, les drapeaux (signe : la valeur est passée
# de l'autre côté de sa limite ; seuil : sa valeur p a passé 0,05, ou sans valeur p son intervalle
# exclut maintenant sa limite, ou ne l'exclut plus, ou elle a passé son repère ; ic : la nouvelle valeur
# est hors de l'ancien intervalle ; nouveau, disparu ; categorie : sa catégorie a changé), son
# importance et sa phrase quand le changement compte. `retournes` : les composantes sorties retournées
# (axes_retournes), dont les valeurs d'après sont comparées de signe changé.
comparer_entree <- function(a, b, fr, retournes = character(0)) {
  e <- b %||% a
  nouveau <- is.null(a)
  disparu <- is.null(b)
  retourne <- !nouveau && !disparu && !is.null(b$axe) && b$axe %in% retournes
  if (retourne) {
    if (!is.null(b$valeur)) b$valeur <- -b$valeur
    if (!is.null(b$ic)) b$ic <- sort(-b$ic)
  }
  va <- a$valeur
  vb <- b$valeur
  deux <- !is.null(va) && !is.null(vb)
  ecart <- if (deux) vb - va
  relatif <- if (deux && va != 0) ecart / abs(va)
  pareil <- function(x, y) (is.null(x) && is.null(y)) || (!is.null(x) && !is.null(y) && length(x) == length(y) && all(abs(x - y) <= 1e-10 * pmax(abs(x), abs(y))))
  categorie <- !nouveau && !disparu && !identical(a[["classe"]], b[["classe"]])
  change <- nouveau || disparu || categorie || !pareil(va, vb) || !pareil(a$p, b$p) || !pareil(a$ic, b$ic)
  limite <- e$limite
  repere <- e$repere
  cote <- function(v) if (!is.null(v) && !is.null(limite)) sign(v - limite) else 0
  signe <- deux && cote(va) * cote(vb) < 0
  sa <- significatif_resultat(a, limite)
  sb <- significatif_resultat(b, limite)
  passe_repere <- deux && !is.null(repere) && (va > repere) != (vb > repere)
  seuil <- (!is.na(sa) && !is.na(sb) && sa != sb) || passe_repere
  hors_ic <- deux && !is.null(a$ic) && (vb < a$ic[1] || vb > a$ic[2])

  # l'importance : des points pour chaque drapeau (un changement de signe compte plus quand la valeur
  # est significative d'un côté au moins), et l'ampleur du changement (en demi-largeurs de l'ancien
  # intervalle ; sans intervalle, l'écart d'un pourcentage ou d'une valeur qui reste entre -1 et 1, un
  # R², un Q², une corrélation, dont le rapport grossirait les petits écarts autour de 0 ; sinon l'écart
  # relatif), fois le poids du groupe ; 0 sans changement
  poids <- if (identical(e$cle, "coef.(Intercept)")) 0.3 else if (e$groupe %in% names(POIDS_GROUPES_RESULTATS)) POIDS_GROUPES_RESULTATS[[e$groupe]] else 0.5
  petite <- deux && (identical(e$forme, "pourcentage") || max(abs(va), abs(vb)) <= 1)
  ampleur <- if (!deux || !change) 0
             else if (!is.null(a$ic)) { demi <- diff(a$ic) / 2; if (demi > 0) min(3, abs(ecart) / demi) else 3 }
             else if (petite) min(3, 20 * abs(ecart))
             else if (!is.null(relatif)) min(3, 10 * abs(relatif)) else 3
  signe_fort <- signe && (isTRUE(sa) || isTRUE(sb) || (is.na(sa) && is.na(sb)))
  # (le groupe d'une espèce indicatrice ni avant ni après : celui de sa plus grande valeur, sans portée)
  categorie_forte <- categorie && !(e$groupe %in% c("variables", "especes") && isFALSE(sa) && isFALSE(sb))
  points <- 4 * (nouveau || disparu) + (if (signe) (if (signe_fort) 6 else 1.5) else 0) + 5 * seuil + 3 * hors_ic + (if (categorie) (if (categorie_forte) 5 else 1.5) else 0) + ampleur
  importance <- if (change) round(poids * points, 4) else 0

  # une phrase pour un changement qui compte : un drapeau (un changement de signe, ou de la catégorie
  # d'une variable, quand la valeur est significative d'un côté au moins : sinon ils étaient déjà
  # incertains), un nombre entier qui change (n, A…), ou, sans intervalle, un pourcentage qui bouge de
  # 2 points, une valeur entre -1 et 1 de 0,05, une autre de 10 % (25 % pour la statistique d'un test) ;
  # pas pour les composantes, que l'ajustement résume, ni pour l'ordonnée à l'origine, ni pour les
  # résultats d'un fichier
  test <- !is.null(a$p) || !is.null(b$p)
  notable <- change && !(e$groupe %in% c("composantes", "stocke")) && !identical(e$cle, "coef.(Intercept)") &&
    (nouveau || disparu || signe_fort || seuil || hors_ic || categorie_forte || identical(e$forme, "entier") ||
       (deux && is.null(a$ic) && !(e$groupe %in% c("variables", "especes")) &&
          (if (identical(e$forme, "pourcentage")) abs(ecart) >= 0.02 else if (petite) abs(ecart) >= 0.05
           else !is.null(relatif) && abs(relatif) >= (if (test) 0.25 else 0.1))))
  x <- list(cle = e$cle, libelle = e$libelle, groupe = e$groupe, alias = e$alias, forme = e$forme, axe = e$axe,
            avant = va, apres = vb, ic_avant = a$ic, ic_apres = b$ic, p_avant = a$p, p_apres = b$p, n_avant = a$n, n_apres = b$n,
            classe_avant = a[["classe"]], classe_apres = b[["classe"]], classe_texte_avant = a[["classe_texte"]], classe_texte_apres = b[["classe_texte"]],
            ecart = ecart, ecart_relatif = relatif, change = change, signe = signe, seuil = seuil, ic = hors_ic, nouveau = nouveau, disparu = disparu,
            categorie = categorie, retourne = retourne, importance = importance, phrase = NULL)
  if (notable) x["phrase"] <- list(phrase_changement(a, b, e, x, sa, sb, passe_repere, fr))
  x
}

# La phrase d'un changement, écrite comme le texte des résultats : « Le coefficient de « annee » passe
# de -0,0210 à -0,0260 (IC 95 % [-0,0310 ; -0,0210]), toujours significatif (p < 0,001). »
phrase_changement <- function(a, b, e, x, sa, sb, passe_repere, fr) {
  sujet <- sujet_resultat(e$libelle, e$genre, fr)
  sujet <- paste0(toupper(substr(sujet, 1, 1)), substring(sujet, 2))
  ecrire <- ecriture_resultat(a$valeur, b$valeur, e$forme, fr)
  crochets <- function(ic) sprintf(if (fr) "[%s ; %s]" else "[%s, %s]", ecrire(ic[1]), ecrire(ic[2]))
  categorie <- function(y) guillemets_resultat(y[["classe_texte"]] %||% y[["classe"]], fr)
  details <- function(y) {
    morceaux <- c(if (!is.null(y$ic)) paste(if (fr) "IC 95 %" else "95% CI", crochets(y$ic)), if (!is.null(y$p)) p_resultat(y$p, fr))
    if (length(morceaux)) paste(morceaux, collapse = ", ") else ""
  }
  valeur <- function(y) paste(c(if (!is.null(y$valeur)) ecrire(y$valeur), if (!is.null(y[["classe"]])) categorie(y)), collapse = " ")
  if (x$disparu) {
    d <- details(a)
    return(sprintf(if (fr) "%s disparaît (avant : %s%s)." else "%s is gone (before: %s%s).", sujet, valeur(a), if (nzchar(d)) paste0(", ", d) else ""))
  }
  if (x$nouveau) {
    d <- details(b)
    return(sprintf(if (fr) "%s apparaît : %s%s." else "%s is new: %s%s.", sujet, valeur(b), if (nzchar(d)) sprintf(" (%s)", d) else ""))
  }
  if (is.null(a$valeur) || is.null(b$valeur)) return(sprintf(if (fr) "%s passe de %s à %s." else "%s goes from %s to %s.", sujet, categorie(a), categorie(b)))
  s <- sprintf(if (fr) "%s passe de %s à %s" else "%s goes from %s to %s", sujet, ecrire(a$valeur), ecrire(b$valeur))
  ancien <- if (x$ic) paste(if (fr) "hors de l'ancien" else "outside the former", crochets(a$ic))
  if (!is.null(b$ic)) s <- paste0(s, sprintf(" (%s %s%s)", if (fr) "IC 95 %" else "95% CI", crochets(b$ic), if (!is.null(ancien)) paste0(", ", ancien) else ""))
  else if (!is.null(ancien)) s <- paste0(s, " (", ancien, ")")
  cote <- function(v, l) sprintf(if (fr) ", désormais %s %s" else ", now %s %s", if (v > l) (if (fr) "au-dessus de" else "above") else (if (fr) "sous" else "below"), ecrire_limite(l, fr))
  if (x$signe) s <- paste0(s, if (identical(e$limite, 0)) (if (fr) ", change de signe" else ", changing sign") else cote(b$valeur, e$limite))
  if (passe_repere) s <- paste0(s, cote(b$valeur, e$repere))
  if (!is.na(sa) && !is.na(sb)) {
    adjectif <- if (identical(e$genre, "f")) "significative" else "significatif"
    p <- if (!is.null(b$p)) sprintf(" (%s)", p_resultat(b$p, fr)) else ""
    s <- paste0(s, if (sa && sb) (if (fr) paste(", toujours", adjectif) else ", still significant")
                   else if (sb) (if (fr) paste(", désormais", adjectif) else ", now significant")
                   else if (sa) (if (fr) paste(", et n'est plus", adjectif) else ", no longer significant")
                   else (if (fr) paste(", toujours non", adjectif) else ", still not significant"), p)
  }
  if (x$categorie) s <- paste0(s, sprintf(if (fr) " ; %s avant, %s maintenant" else "; %s before, %s now", categorie(a), categorie(b)))
  paste0(s, ".")
}

# (une limite ou un repère, 0 ou 1, comme le texte les écrit)
ecrire_limite <- function(l, fr) trimws(formatC(l, format = "fg", digits = 6, decimal.mark = if (fr) "," else "."))

# Les phrases des valeurs par variable ou par espèce quand elles sont nombreuses à changer (plus de 8) :
# une par drapeau, qui les compte et nomme les premières, à la place d'une phrase chacune.
phrases_variables <- function(xs, fr) {
  drapeaux <- list(signe = c("%d valeurs changent de signe : %s.", "%d values changed sign: %s."),
                   seuil = c("%d valeurs passent leur seuil : %s.", "%d values crossed their threshold: %s."),
                   ic = c("%d valeurs sortent de leur ancien intervalle à 95 %% : %s.", "%d values left their former 95%% interval: %s."),
                   categorie = c("%d valeurs changent de catégorie : %s.", "%d values changed category: %s."),
                   nouveau = c("%d valeurs apparaissent : %s.", "%d values are new: %s."),
                   disparu = c("%d valeurs disparaissent : %s.", "%d values are gone: %s."))
  Filter(Negate(is.null), lapply(names(drapeaux), function(f) {
    ici <- Filter(function(x) isTRUE(x[[f]]), xs)
    if (!length(ici)) return(NULL)
    ici <- ici[order(-vapply(ici, `[[`, 0, "importance"))]
    noms <- vapply(utils::head(ici, 5), function(x) {
      l <- x$libelle
      if (grepl("^.[[:lower:]]", l)) paste0(tolower(substr(l, 1, 1)), substring(l, 2)) else l
    }, "")
    reste <- length(ici) - length(noms)
    autres <- if (reste == 1) (if (fr) " et 1 autre" else " and 1 other") else if (reste > 1) sprintf(if (fr) " et %d autres" else " and %d others", reste) else ""
    liste <- paste0(paste(noms, collapse = if (fr) " ; " else "; "), autres)
    list(texte = sprintf(drapeaux[[f]][if (fr) 1 else 2], length(ici), liste), cles = I(vapply(ici, `[[`, "", "cle")), importance = max(vapply(ici, `[[`, 0, "importance")))
  }))
}

# Deux résumés de resume_resultats (ou tels qu'une demande les rend : entrees_recues) comparés entrée
# par entrée, par leur clé. Rend `entrees` (chaque entrée comparée : comparer_entree), `phrases` (celles
# des changements qui comptent, la plus importante d'abord : { texte, cles, importance }) et `resume`
# (combien d'entrées comparées, changées, identiques, nouvelles, disparues, de chaque drapeau, et de
# phrases). Les libellés sont ceux des entrées : le résumé et la comparaison se demandent dans la même
# langue, `langue` celle des phrases.
comparer_resultats <- function(avant, apres, langue = if (identical(getOption("nuee.langue"), "en")) "en" else "fr") {
  fr <- !identical(langue, "en")
  avant <- entrees_recues(avant)
  apres <- entrees_recues(apres)
  avant <- avant[!duplicated(vapply(avant, `[[`, "", "cle"))]
  apres <- apres[!duplicated(vapply(apres, `[[`, "", "cle"))]
  ca <- vapply(avant, `[[`, "", "cle")
  cp <- vapply(apres, `[[`, "", "cle")
  retournes <- axes_retournes(avant, apres)
  cles <- ordre_comparaison(ca, cp)
  ia <- match(cles, ca)  # (les places une fois pour toutes : des milliers de loadings)
  ib <- match(cles, cp)
  entrees <- lapply(seq_along(cles), function(i) comparer_entree(if (!is.na(ia[i])) avant[[ia[i]]], if (!is.na(ib[i])) apres[[ib[i]]], fr, retournes))
  notables <- Filter(function(x) !is.null(x$phrase), entrees)
  par_variable <- vapply(notables, function(x) x$groupe %in% c("variables", "especes"), TRUE)
  une <- function(x) list(texte = x$phrase, cles = I(x$cle), importance = x$importance)
  phrases <- c(lapply(notables[!par_variable], une),
               if (sum(par_variable) > 8) phrases_variables(notables[par_variable], fr) else lapply(notables[par_variable], une),
               lapply(retournes, function(axe) {
                 k <- vapply(Filter(function(x) isTRUE(x$retourne) && identical(x$axe, axe), entrees), `[[`, "", "cle")
                 list(texte = sprintf(if (fr) "La composante %s est sortie retournée (son signe est arbitraire) : ses loadings sont comparés de signe changé."
                                      else "Component %s came out reflected (its sign is arbitrary): its loadings are compared with their sign changed.", axe),
                      cles = I(k), importance = 0)
               }))
  if (length(phrases)) phrases <- phrases[order(-vapply(phrases, `[[`, 0, "importance"))]
  compte <- function(f) sum(vapply(entrees, function(x) isTRUE(x[[f]]), TRUE))
  list(entrees = entrees, phrases = phrases,
       resume = list(entrees = length(entrees), changees = compte("change"), identiques = length(entrees) - compte("change"), nouvelles = compte("nouveau"),
                     disparues = compte("disparu"), signe = compte("signe"), seuil = compte("seuil"), ic = compte("ic"), categorie = compte("categorie"),
                     retournees = length(retournes), phrases = length(phrases)))
}
