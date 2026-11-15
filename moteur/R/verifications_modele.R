# Les conditions d'une régression, vérifiées après l'ajustement et dites en mots (le Résumé du
# modèle) : chacune un statut, « ok » ou « attention », et une phrase qui dit ce qu'elle a vu et,
# quand il le faut, ce qu'on peut faire. Ce sont des repères, pas des tests à suivre à la lettre :
# avec beaucoup d'observations, un test détecte des écarts sans conséquence, et avec peu il n'en
# voit aucun ; les graphiques des résidus restent la référence.
#   - la loi des résidus : Shapiro et Wilk (1965) sur les résidus standardisés (loi normale), ou
#     sur les résidus quantiles (Dunn et Smyth 1996) d'un comptage, d'une présence-absence ou de
#     valeurs positives, qui suivent la loi normale quand la loi choisie convient ;
#   - une variance constante : la corrélation de Spearman entre la valeur absolue des résidus et
#     les valeurs ajustées (une dispersion qui croît avec la moyenne) ;
#   - les observations influentes d'un modèle linéaire : la distance de Cook au-delà de 4/n ;
#   - la surdispersion d'un modèle de Poisson (le χ² de Pearson sur ses degrés de liberté) ;
#   - l'excès de zéros d'un comptage : les zéros observés contre ceux qu'attend le modèle ;
#   - la colinéarité des termes quantitatifs : le facteur d'inflation de la variance (VIF) ;
#   - la concurvité des courbes d'un GAM ;
#   - l'autocorrélation spatiale des résidus quand le projet a des coordonnées (spatial.R : l'I de
#     Moran des 4 plus proches voisins, jusqu'à MAX_VERIFICATION_SPATIALE observations).

MAX_VERIFICATION_SPATIALE <- 3000

verifications_regression <- function(projet, m) {
  fr <- !identical(getOption("nuee.langue"), "en")
  r <- m$resultat
  n <- m$N
  out <- list()
  ajouter <- function(cle, statut, texte) out[[length(out) + 1]] <<- list(cle = cle, statut = statut, texte = texte)
  nb <- function(x, k = 2) formatC(x, format = "f", digits = k, decimal.mark = if (fr) "," else ".")
  pv <- function(p) if (p < 0.001) (if (fr) "p < 0,001" else "p < 0.001") else paste("p =", formatC(p, digits = 2, format = "g", decimal.mark = if (fr) "," else "."))
  famille <- m$famille %||% "gaussienne"
  # --- la loi des résidus
  e <- if (famille == "gaussienne") r$residus else r$residus_quantiles
  e <- e[is.finite(e)]
  if (length(e) >= 8 && length(e) <= 5000 && stats::sd(e) > 0) {
    sw <- stats::shapiro.test(e)
    quoi <- if (famille == "gaussienne") (if (fr) "Les résidus" else "The residuals") else (if (fr) "Les résidus quantiles" else "The quantile residuals")
    if (sw$p.value >= 0.05) {
      ajouter("loi", "ok", sprintf(if (fr) "%s ne s'écartent pas nettement de la loi normale (Shapiro–Wilk W = %s, %s)." else "%s don't depart clearly from the normal distribution (Shapiro–Wilk W = %s, %s).",
                                   quoi, nb(sw$statistic, 3), pv(sw$p.value)))
    } else {
      ajouter("loi", "attention", sprintf(if (fr) "%s s'écartent de la loi normale (Shapiro–Wilk W = %s, %s) : regardez leur graphique quantile-quantile (Graphiques › Résidus)%s. Avec beaucoup d'observations, un petit écart suffit au test."
                                          else "%s depart from the normal distribution (Shapiro–Wilk W = %s, %s): look at their quantile-quantile plot (Plots › Residuals)%s. With many observations, a small departure is enough for the test.",
                                          quoi, nb(sw$statistic, 3), pv(sw$p.value),
                                          if (famille == "gaussienne") (if (fr) " ; une transformation de Y (log) ou une autre loi convient peut-être mieux" else "; a transformation of Y (log) or another distribution may suit better")
                                          else (if (fr) " ; une autre loi convient peut-être mieux" else "; another distribution may suit better")))
    }
  }
  # --- une variance constante
  aj <- r$ajuste
  ab <- abs(r$residus)
  ok <- is.finite(aj) & is.finite(ab)
  if (sum(ok) >= 10 && stats::sd(aj[ok]) > 0 && stats::sd(ab[ok]) > 0) {
    ct <- suppressWarnings(stats::cor.test(ab[ok], aj[ok], method = "spearman", exact = FALSE))
    if (ct$p.value < 0.05 && famille == "gaussienne") {
      ajouter("variance", "attention", sprintf(if (fr) "La dispersion des résidus %s avec les valeurs ajustées (ρ de Spearman = %s, %s) : %s."
                                               else "The spread of the residuals %s with the fitted values (Spearman's ρ = %s, %s): %s.",
                                               if (ct$estimate > 0) (if (fr) "augmente" else "increases") else (if (fr) "diminue" else "decreases"),
                                               nb(ct$estimate), pv(ct$p.value),
                                               if (fr) "une transformation de Y (log, racine) ou une loi dont la variance suit la moyenne (Poisson, gamma) conviendrait peut-être" else "a transformation of Y (log, root) or a distribution whose variance follows the mean (Poisson, gamma) may suit"))
    } else if (famille == "gaussienne") {
      ajouter("variance", "ok", sprintf(if (fr) "La dispersion des résidus ne change pas nettement avec les valeurs ajustées (ρ de Spearman = %s, %s)." else "The spread of the residuals doesn't change clearly with the fitted values (Spearman's ρ = %s, %s).",
                                        nb(ct$estimate), pv(ct$p.value)))
    }
  }
  # --- les observations influentes (modèle linéaire)
  if (!is.null(r$cook)) {
    seuil <- 4 / n
    fortes <- which(r$cook > seuil)
    if (length(fortes)) {
      o <- fortes[order(-r$cook[fortes])]
      noms <- unname(projet$noms_obs[as.character(m$observations[o])])
      ajouter("influence", if (any(r$cook[o] > 1)) "attention" else "info",
              sprintf(if (fr) "%d observation%s %s une distance de Cook au-delà de 4/n = %s : %s%s. Vérifiez leurs valeurs, et refaites le modèle sans elles pour voir s'il change."
                      else "%d observation%s %s a Cook's distance beyond 4/n = %s: %s%s. Check their values, and refit without them to see whether the model changes.",
                      length(o), if (length(o) > 1) "s" else "", if (fr) (if (length(o) > 1) "ont" else "a") else (if (length(o) > 1) "have" else "has"),
                      nb(seuil, 3), paste(sprintf("%s (%s)", utils::head(noms, 5), nb(utils::head(r$cook[o], 5))), collapse = ", "),
                      if (length(o) > 5) (if (fr) "…" else "…") else ""))
    } else ajouter("influence", "ok", if (fr) "Aucune observation n'a une distance de Cook au-delà de 4/n." else "No observation has a Cook's distance beyond 4/n.")
  }
  # --- surdispersion et zéros d'un comptage
  if (famille == "poisson" && !is.null(r$dispersion)) {
    if (r$dispersion > 1.5) ajouter("dispersion", "attention", sprintf(if (fr) "Les comptages sont surdispersés (χ² de Pearson / ddl = %s) : les valeurs p sont trop petites ; la binomiale négative convient mieux." else "The counts are overdispersed (Pearson's χ² / df = %s): the p values are too small; the negative binomial suits better.", nb(r$dispersion)))
    else ajouter("dispersion", "ok", sprintf(if (fr) "Pas de surdispersion (χ² de Pearson / ddl = %s)." else "No overdispersion (Pearson's χ² / df = %s).", nb(r$dispersion)))
  }
  if (famille %in% c("poisson", "binomiale_negative") && !is.null(m$ajustement)) {
    a <- if (inherits(m$ajustement, "gamm")) m$ajustement$gam else m$ajustement
    mu <- r$ajuste
    y <- m$donnees$y
    attendus <- if (famille == "poisson") sum(stats::dpois(0, mu)) else {
      theta <- tryCatch(a$family$getTheta(TRUE), error = function(e) NA_real_)
      if (is.na(theta)) NA_real_ else sum(stats::dnbinom(0, size = theta, mu = mu))
    }
    observes <- sum(y == 0)
    if (is.finite(attendus) && observes > 0) {
      trop <- observes > 1.2 * attendus && observes - attendus >= 5
      ajouter("zeros", if (trop) "attention" else "ok",
              sprintf(if (fr) "%d zéros observés, %s attendus par le modèle%s." else "%d zeros observed, %s expected by the model%s.", observes, nb(attendus, 1),
                      if (trop) (if (fr) " : un excès de zéros, qu'un modèle à zéros en excès ou la binomiale négative prendrait peut-être en compte" else ": an excess of zeros, which a zero-inflated model or the negative binomial may account for") else ""))
    }
  }
  # --- colinéarité des termes quantitatifs (VIF : la diagonale de l'inverse de leur matrice de corrélation)
  q <- intersect(m$quantitatifs, m$utiles)
  q <- q[vapply(q, function(t) stats::sd(m$donnees[[t]]) > 0, TRUE)]
  if (length(q) >= 2) {
    R <- stats::cor(m$donnees[q])
    vif <- tryCatch(diag(solve(R)), error = function(e) rep(Inf, length(q)))
    noms <- m$noms_termes[match(q, m$termes)]
    forts <- which(vif > 5)
    if (length(forts)) ajouter("colinearite", "attention", sprintf(if (fr) "Des termes quantitatifs sont colinéaires (VIF > 5) : %s. Leurs coefficients sont instables ; gardez-en un, ou combinez-les (une ACP)." else "Some quantitative terms are collinear (VIF > 5): %s. Their coefficients are unstable; keep one of them, or combine them (a PCA).",
                                                                   paste(sprintf("%s (%s)", noms[forts], nb(vif[forts], 1)), collapse = ", ")))
    else ajouter("colinearite", "ok", sprintf(if (fr) "Pas de colinéarité forte entre les termes quantitatifs (VIF le plus grand : %s)." else "No strong collinearity between the quantitative terms (largest VIF: %s).", nb(max(vif), 1)))
  }
  # --- concurvité des courbes
  cb <- r$courbes
  if (!is.null(cb$concurvite)) {
    fortes <- which(cb$concurvite > 0.8)
    if (length(fortes)) ajouter("concurvite", "attention", sprintf(if (fr) "Des courbes se confondent avec les autres termes (concurvité > 0,8) : %s." else "Some curves are confounded with the other terms (concurvity > 0.8): %s.",
                                                                   paste(sprintf("%s (%s)", cb$terme[fortes], nb(cb$concurvite[fortes])), collapse = ", ")))
  }
  # --- autocorrélation spatiale des résidus
  if (!is.null(coordonnees_projet(projet)) && n <= MAX_VERIFICATION_SPATIALE) {
    s <- tryCatch(autocorrelation_spatiale(projet, m, permutations = 0, classes = 2), error = function(e) NULL)
    if (!is.null(s)) {
      if (s$moran$p < 0.05) ajouter("spatial", "attention", sprintf(if (fr) "Les résidus sont autocorrélés dans l'espace (I de Moran = %s, %s, 4 plus proches voisins) : des observations proches se ressemblent plus que le modèle ne le dit ; un lissage spatial s(x, y) (GAM) ou un effet aléatoire du site en tiendrait compte (Graphiques › Autocorrélation spatiale)."
                                                                    else "The residuals are autocorrelated in space (Moran's I = %s, %s, 4 nearest neighbours): nearby observations are more alike than the model says; a spatial smooth s(x, y) (GAM) or a random effect of the site would account for it (Plots › Spatial autocorrelation).",
                                                                    nb(s$moran$I, 3), pv(s$moran$p)))
      else ajouter("spatial", "ok", sprintf(if (fr) "Pas d'autocorrélation spatiale nette des résidus (I de Moran = %s, %s)." else "No clear spatial autocorrelation of the residuals (Moran's I = %s, %s).", nb(s$moran$I, 3), pv(s$moran$p)))
    }
  }
  out
}
