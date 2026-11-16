# Le texte des résultats d'un modèle ({{M1.resultats}} dans un document) : ce que montre le
# modèle, en phrases, avec ses nombres, en français ou en anglais, comme le texte des méthodes
# (methodes.R) dit comment il a été fait. Une régression : la qualité de l'ajustement (R², déviance
# expliquée, Q² en validation croisée), chaque coefficient avec son intervalle à 95 % et sa valeur
# p (sur l'échelle de la réponse pour un lien log ou logit : un facteur, un rapport de cotes), les
# termes significatifs au seuil de 5 %, les courbes (edf et test), les effets aléatoires. Une ACP,
# une PLS ou une OPLS : la variance expliquée par composante, Q², le test CV-ANOVA s'il a été
# calculé, les variables les plus importantes. Une autre analyse : son texte_resultats_<type> si
# elle en a un (analyses.R), sinon le résumé de ses chiffres.

texte_resultats <- function(projet, d, m, langue = c("fr", "en"), chiffres = "") {
  langue <- match.arg(langue)
  fr <- langue == "fr"
  if (est_regression(d)) return(list(paragraphes = resultats_regression(m, fr, projet, d)))
  if (est_autre_analyse(d)) {
    f <- fonction_analyse("texte_resultats_", d$type)
    if (!is.null(f)) return(f(projet, d, m, langue))
    # (les chiffres du résumé, écrits avec un point décimal partout : une virgule en français)
    if (fr) chiffres <- gsub("([0-9])\\.([0-9])", "\\1,\\2", chiffres)
    return(list(paragraphes = sprintf(if (fr) "Résultats de %s (%s) : %s." else "Results of %s (%s): %s.", d$name %||% "", d$type, chiffres)))
  }
  list(paragraphes = resultats_composantes(projet, d, m, fr))
}

# (des nombres et des valeurs p écrits comme dans le texte des méthodes ; un infini « ∞ », un nombre de
# 1e15 ou plus en notation scientifique, que « fg » écrirait chiffre à chiffre : une centaine pour
# exp(240), le rapport de cotes d'une séparation complète)
nombre_resultat <- function(x, fr, chiffres = 3) {
  if (is.null(x) || !length(x) || is.na(x)) return(if (fr) "non calculé" else "not computed")
  if (is.infinite(x)) return(if (x > 0) "∞" else "−∞")
  if (abs(x) >= 1e15) return(formatC(x, format = "g", digits = chiffres, decimal.mark = if (fr) "," else "."))
  trimws(sub("[.,]$", "", formatC(signif(x, chiffres), format = "fg", digits = chiffres, decimal.mark = if (fr) "," else ".", flag = "#")))
}
# (une valeur p manquante : non calculée, plutôt que « p = NA » ; trimws, formatC écrivant 1 «   1 »)
p_resultat <- function(p, fr) {
  if (is.null(p) || is.na(p)) return(if (fr) "p non calculée" else "p not computed")
  if (p < 0.001) return(if (fr) "p < 0,001" else "p < 0.001")
  paste("p =", trimws(formatC(p, digits = 2, format = "g", decimal.mark = if (fr) "," else ".")))
}
guillemets_resultat <- function(x, fr) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)

# (`projet` et `d` : les unités des variables, atelier.R ; le coefficient d'une variable quantitative
# dans ses unités, ni transformée ni mise à l'échelle autrement que centrée, est « par m »)
resultats_regression <- function(m, fr, projet = NULL, d = NULL) {
  r <- m$resultat
  nb <- function(x, k = 3) nombre_resultat(x, fr, k)
  par_unite <- function(terme) {
    t <- if (!is.null(projet) && !is.null(d)) Find(function(t) identical(as.character(t$name), terme), d$x)
    if (is.null(t) || length(t$variables) != 1 || categorie(t) != 0 || !is.null(nom_transformation(t$transform))) return("")
    if (!as.character(t$scaling$type %||% "UV")[1] %in% c("none", "Ctr")) return("")
    u <- unite_variable(projet, t$variables[1])
    if (nzchar(u)) sprintf(if (fr) " par %s" else " per %s", u) else ""
  }
  r2 <- r$r2 %||% list()
  qualite <- if (!is.null(r2$marginal)) {
    sprintf(if (fr) "R² marginal = %s (effets fixes), R² conditionnel = %s (avec les effets aléatoires)" else "marginal R² = %s (fixed effects), conditional R² = %s (with the random effects)",
            nb(r2$marginal, 2), nb(r2$conditionnel, 2))
  } else if (!is.null(r2$deviance_expliquee)) {
    paste0(sprintf(if (fr) "%s de la déviance expliquée" else "%s of the deviance explained", pourcent_texte(r2$deviance_expliquee, fr)),
           if (!is.null(r2$r2_ajuste)) sprintf(if (fr) " (R² ajusté = %s)" else " (adjusted R² = %s)", nb(r2$r2_ajuste, 2)) else "")
  } else sprintf(if (fr) "R² = %s (ajusté : %s)" else "R² = %s (adjusted: %s)", nb(r2$r2, 2), nb(r2$r2_ajuste, 2))
  p1 <- sprintf(if (fr) "Le modèle de %s porte sur %d observations : %s ; en validation croisée, Q² = %s (RMSEcv = %s). AIC = %s."
                else "The model of %s uses %d observations: %s; in cross-validation, Q² = %s (RMSEcv = %s). AIC = %s.",
                paste0(guillemets_resultat(m$y$noms, fr), if (!is.null(projet)) unite_texte(projet, m$y$uid[1]) else ""), m$N, qualite, nb(m$vc$Q2, 2), nb(m$vc$RMSEcv), nb(r$AIC, 4))
  co <- r$coefficients
  co <- co[co$terme != "(Intercept)", , drop = FALSE]
  lien <- m$lien %||% "identity"
  phrases <- vapply(seq_len(nrow(co)), function(i) {
    x <- co[i, ]
    u <- par_unite(x$terme)
    base <- sprintf(if (fr) "%s : β = %s%s (IC 95 %% [%s ; %s], %s)" else "%s: β = %s%s (95%% CI [%s, %s], %s)",
                    guillemets_resultat(x$terme, fr), nb(x$estimation), u, nb(x$bas), nb(x$haut), p_resultat(x$p, fr))
    echelle <- if (lien == "log") sprintf(if (fr) ", soit un facteur %s [%s ; %s]%s" else ", i.e. a factor of %s [%s, %s]%s", nb(exp(x$estimation)), nb(exp(x$bas)), nb(exp(x$haut)), u)
               else if (lien == "logit") sprintf(if (fr) ", soit un rapport de cotes de %s [%s ; %s]%s" else ", i.e. an odds ratio of %s [%s, %s]%s", nb(exp(x$estimation)), nb(exp(x$bas)), nb(exp(x$haut)), u)
               else ""
    paste0(base, echelle)
  }, "")
  sig <- co$terme[!is.na(co$p) & co$p <= 0.05]
  p2 <- if (nrow(co)) {
    c(paste0(if (fr) paste0("Coefficients (sur l'échelle du lien", if (lien == "identity") "" else paste0(" ", lien), ") : ")
             else paste0("Coefficients (on the ", if (lien == "identity") "" else paste0(lien, " "), "link scale): "),
             paste(phrases, collapse = if (fr) " ; " else "; "), "."),
      if (length(sig)) sprintf(if (fr) "Au seuil de 5 %%, %s %s un effet significatif%s." else "At the 5%% level, %s %s a significant effect%s.",
                               liste_mots(vapply(sig, guillemets_resultat, "", fr = fr), fr),
                               if (fr) (if (length(sig) > 1) "ont" else "a") else (if (length(sig) > 1) "have" else "has"),
                               if (length(sig) == nrow(co)) ""
                               else sprintf(if (fr) " ; pas %s" else "; not %s", liste_mots(vapply(setdiff(co$terme, sig), guillemets_resultat, "", fr = fr), fr)))
      else if (fr) "Aucun terme n'a d'effet significatif au seuil de 5 %." else "No term has a significant effect at the 5% level.")
  }
  cb <- r$courbes
  p3 <- if (!is.null(cb) && nrow(cb)) {
    noms <- if (!is.null(cb$niveau)) ifelse(is.na(cb$niveau), cb$terme, sprintf("%s (%s %s)", cb$terme, cb$par, cb$niveau)) else cb$terme
    sprintf(if (fr) "Courbes : %s. Un edf de 1 est une droite." else "Curves: %s. An edf of 1 is a straight line.",
            paste(sprintf(if (fr) "%s, edf = %s (%s)" else "%s, edf = %s (%s)", noms, vapply(cb$edf, nb, "", k = 2), vapply(cb$p, p_resultat, "", fr = fr)),
                  collapse = if (fr) " ; " else "; "))
  }
  # (les composantes nommées dans la langue du texte : le groupement de chaque niveau, la résiduelle)
  v <- r$variances
  if (!is.null(v)) v$composante <- c(m$niveaux, if (fr) "résiduelle" else "residual")[match(v$composante, c(paste0("g", seq_along(m$niveaux)), "residuelle"))]
  p4 <- if (!is.null(v) && nrow(v) && "variance" %in% names(v)) {
    simples <- if ("terme" %in% names(v)) v[!nzchar(v$terme %||% "") | v$terme == "(Intercept)" | is.na(v$terme), , drop = FALSE] else v
    if (nrow(simples)) sprintf(if (fr) "Écarts-types des effets aléatoires : %s." else "Standard deviations of the random effects: %s.",
                               paste(sprintf("%s σ = %s", simples$composante, vapply(sqrt(simples$variance), nb, "")), collapse = if (fr) " ; " else "; "))
  }
  c(p1, p2, p3, p4)
}

resultats_composantes <- function(projet, d, m, fr) {
  s <- m$statistiques
  cp <- s$composantes
  nb <- function(x, k = 3) nombre_resultat(x, fr, k)
  if (is.null(cp) || !nrow(cp)) return(character(0))
  dernier <- cp[nrow(cp), ]
  type <- paste0(if (isTRUE(d$opls)) "O" else "", if (isTRUE(m$plsda)) "PLS-DA" else d$type)
  if (fr && type == "PCA") type <- "ACP"
  par_comp <- paste(sprintf(if (fr) "composante %d : R²X = %s%s, Q² = %s" else "component %d: R²X = %s%s, Q² = %s", seq_len(nrow(cp)), vapply(cp$R2X, pourcent_texte, "", fr = fr),
                            if (!is.null(cp$R2Y)) sprintf(", R²Y = %s", vapply(cp$R2Y, pourcent_texte, "", fr = fr)) else "", vapply(cp$Q2, nb, "", k = 2)),
                    collapse = if (fr) " ; " else "; ")
  p1 <- sprintf(if (fr) "Le modèle %s à %d composante%s explique %s de la variance de X%s ; Q² cumulé = %s (%s)."
                else "The %s model with %d component%s explains %s of the variance of X%s; cumulative Q² = %s (%s).",
                type, nrow(cp), if (nrow(cp) > 1) "s" else "", pourcent_texte(dernier$R2Xcum, fr),
                if (!is.null(dernier$R2Ycum)) sprintf(if (fr) " et %s de celle de Y" else " and %s of that of Y", pourcent_texte(dernier$R2Ycum, fr)) else "",
                nb(dernier$Q2cum, 2), par_comp)
  if (length(m$gain)) p1 <- paste(p1, sprintf(if (fr) "Son gain sur le modèle vide est de %s." else "Its gain over the empty model is %s.", nb(m$gain, 2)))
  p2 <- NULL
  ca <- s$cv_anova
  if (length(ca)) {
    p2 <- paste(vapply(names(ca), function(y) {
      t <- ca[[y]]
      # (sans valeur p, ses degrés de liberté résiduels ne sont pas positifs : son F ne veut rien dire)
      if (is.na(t$p[2])) return(sprintf(if (fr) "CV-ANOVA de %s non calculée : trop peu d'observations pour ses degrés de liberté" else "CV-ANOVA of %s not computed: too few observations for its degrees of freedom", guillemets_resultat(y, fr)))
      sprintf(if (fr) "CV-ANOVA de %s : F = %s, %s" else "CV-ANOVA of %s: F = %s, %s", guillemets_resultat(y, fr), nb(t$F[2]), p_resultat(t$p[2], fr))
    }, ""), collapse = if (fr) " ; " else "; ")
    p2 <- paste0(p2, ".")
  }
  v <- s$variables
  p3 <- NULL
  if (!is.null(v$VIP)) {
    noms <- as.character(v$terme %||% rownames(v))
    o <- order(-v$VIP)
    hauts <- o[v$VIP[o] > 1][1:min(8, sum(v$VIP > 1, na.rm = TRUE))]
    hauts <- hauts[!is.na(hauts)]
    if (length(hauts)) p3 <- sprintf(if (fr) "Les variables les plus importantes (VIP > 1) : %s%s." else "The most important variables (VIP > 1): %s%s.",
                                     paste(sprintf("%s (%s)", noms[hauts], vapply(v$VIP[hauts], nb, "", k = 2)), collapse = ", "),
                                     if (sum(v$VIP > 1, na.rm = TRUE) > length(hauts)) (if (fr) ", entre autres" else ", among others") else "")
  }
  c(p1, p2, p3)
}
