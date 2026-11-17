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
#   - une variable quantitative loin de 0 (une année) croisée avec une variable qualitative sans être
#     centrée : pourquoi les coefficients des catégories sont si grands, et le réajustement centré ;
#   - l'autocorrélation spatiale des résidus quand le projet a des coordonnées (spatial.R : l'I de
#     Moran des 4 plus proches voisins, jusqu'à MAX_VERIFICATION_SPATIALE observations).

MAX_VERIFICATION_SPATIALE <- 3000

# `d` : la définition du modèle (les transformations de ses termes), quand elle est donnée.
verifications_regression <- function(projet, m, d = NULL) {
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
      noms <- unname(projet$noms_obs[nom_numero(m$observations[o])])
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
  # --- la séparation d'une présence-absence : un coefficient démesuré, et son erreur-type plus encore.
  # Les coefficients des termes qui séparent les présences des absences, tout à fait (un seuil d'un X
  # au-delà duquel il n'y a que des présences) ou pour une catégorie (sans présence, ou sans absence),
  # croissent sans fin, et leurs erreurs-types plus vite encore, jusqu'à ce que l'ajustement s'arrête :
  # leurs tests ne disent rien de leur effet (Albert et Anderson, 1984). Un coefficient diverge quand il
  # change le logit de plus de 10 sur l'étendue de sa colonne et que son erreur-type le dépasse ; des
  # probabilités ajustées de 0 ou 1 n'y suffisent pas : une pente raide mais finie, bien estimée, en
  # donne aussi. Les observations dont la probabilité ajustée est numériquement 0 ou 1, à 10 ε près
  # (comme glm.fit le dit, « fitted probabilities numerically 0 or 1 occurred »), sont comptées quand il
  # y en a ; une catégorie sans présence n'en a souvent pas (l'ajustement s'y arrête vers 1e-9).
  # Inversement, un coefficient démesuré et incertain ne suffit pas : celui d'une interaction ou d'un
  # carré d'une année non centrée (2001 à 2012) l'est aussi, par colinéarité, ses probabilités ajustées
  # loin de 0 et de 1 (0,33 à 0,58). La séparation, elle, en mène toujours près de 0 ou de 1 : un
  # coefficient diverge donc quand, en plus, une probabilité ajustée est à moins de 1e-4 de 0 ou de 1
  # (une catégorie rare, sans présence, s'arrête vers 1e-6 à 1e-5 dans un GAM, où mgcv s'arrête plus tôt
  # que glm). Une courbe qui sépare tout à fait (au-delà d'une valeur de son X, rien que des présences)
  # n'a pas de coefficient ici : quand aucun ne diverge, ses probabilités ajustées numériquement 0 ou 1
  # le disent, les trois quarts au moins (119 des 120 d'un seuil net ; une pente raide mais finie en met
  # beaucoup au bord, 90 des 200, pas tant).
  if (famille == "binomiale" && (NROW(r$coefficients) || NROW(r$courbes))) {
    co <- r$coefficients
    eps <- 10 * .Machine$double.eps
    bords <- sum(r$ajuste < eps | r$ajuste > 1 - eps, na.rm = TRUE)
    diverge <- integer(0)
    if (NROW(co) && any(r$ajuste < 1e-4 | r$ajuste > 1 - 1e-4, na.rm = TRUE)) {
      t_co <- m$termes[match(co$terme, m$noms_termes)]
      etendue <- vapply(t_co, function(s) if (!is.na(s) && s %in% names(m$donnees)) diff(range(m$donnees[[s]], na.rm = TRUE)) else NA_real_, 0)
      diverge <- which(is.finite(etendue) & etendue > 0 & abs(co$estimation) * etendue > 10 & is.finite(co$erreur_type) & co$erreur_type > abs(co$estimation))
    }
    if (length(diverge)) {
      v3 <- function(z) trimws(formatC(z, digits = 3, format = "fg", decimal.mark = if (fr) "," else "."))
      termes <- vapply(diverge, function(i) sprintf(if (fr) "« %s » (β = %s, erreur-type %s%s)" else "“%s” (β = %s, standard error %s%s)", co$terme[i], v3(co$estimation[i]),
                                                   v3(co$erreur_type[i]), if (is.finite(co$p[i])) paste0(", ", sub("= +", "= ", pv(co$p[i]))) else ""), "")
      plusieurs <- length(diverge) > 1
      quoi <- paste(termes, collapse = ", ")
      au_bord <- if (bords) sprintf(if (fr) " ; %d des %d observations %s une probabilité ajustée numériquement de 0 ou 1" else "; %d of the %d observations %s a fitted probability numerically 0 or 1",
                                    bords, n, if (fr) (if (bords > 1) "ont" else "a") else (if (bords > 1) "have" else "has")) else ""
      ajouter("separation", "attention",
              if (fr) sprintf(if (plusieurs) "Les coefficients de %s sont démesurés, et leurs erreurs-types plus encore : des termes du modèle séparent les présences des absences (une catégorie sans présence ou sans absence, ou un seuil d'un X au-delà duquel il n'y a que des présences)%s. Leurs coefficients croîtraient sans fin si l'ajustement continuait, et leurs tests ne disent rien de leur effet : retirez le terme qui sépare, ou regroupez ses catégories."
                              else "Le coefficient de %s est démesuré, et son erreur-type plus encore : ce terme sépare les présences des absences (une catégorie sans présence ou sans absence, ou un seuil d'un X au-delà duquel il n'y a que des présences)%s. Son coefficient croîtrait sans fin si l'ajustement continuait, et son test ne dit rien de son effet : retirez le terme qui sépare, ou regroupez ses catégories.",
                              quoi, au_bord)
              else sprintf(if (plusieurs) "The coefficients of %s are huge, and their standard errors larger still: terms of the model separate the presences from the absences (a category without presences or without absences, or a threshold of an X beyond which there are only presences)%s. Their coefficients would grow without bound if the fit went on, and their tests say nothing of their effect: remove the term that separates, or merge its categories."
                           else "The coefficient of %s is huge, and its standard error larger still: this term separates the presences from the absences (a category without presences or without absences, or a threshold of an X beyond which there are only presences)%s. Its coefficient would grow without bound if the fit went on, and its test says nothing of its effect: remove the term that separates, or merge its categories.",
                           quoi, au_bord))
    } else if (NROW(r$courbes) && bords && bords >= 0.75 * sum(!is.na(r$ajuste))) {
      ajouter("separation", "attention",
              sprintf(if (fr) "%d des %d observations %s une probabilité ajustée numériquement de 0 ou 1 : une courbe du modèle sépare sans doute les présences des absences (au-delà d'une valeur de son X, rien que des présences, ou que des absences). Son test et sa bande n'en disent alors rien : regardez les données de part et d'autre de cette valeur."
                      else "%d of the %d observations %s a fitted probability numerically 0 or 1: a curve of the model likely separates the presences from the absences (beyond a value of its X, only presences, or only absences). Its test and its band then say nothing of it: look at the data on either side of that value.",
                      bords, n, if (fr) (if (bords > 1) "ont" else "a") else (if (bords > 1) "have" else "has")))
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
  # --- une variable quantitative loin de 0 croisée avec une variable qualitative, sans être centrée :
  # le coefficient de chaque catégorie de celle-ci est son écart là où la variable vaut 0 (l'an 0 d'une
  # année), loin des données, d'où sa taille et la largeur de son intervalle ; centrée sur sa moyenne
  # (l'échelle Ctr), le même modèle donne cet écart à sa moyenne (avec une pente aléatoire de mgcv,
  # indépendante de l'ordonnée à l'origine, le modèle change un peu : rien n'en est dit). Loin : 0 hors
  # de ses valeurs, et sa moyenne à plus de 5 écarts-types de 0 (une année, une date ; pas une
  # température de 10 à 25 °C). L'entrée porte la variable à centrer, pour le bouton du Résumé
  # (serveur.R : reajuster_demande, q$centrer).
  comp <- m$composition
  croisees <- pentes_croisees(m)
  for (t in names(croisees)) {
    i <- match(t, m$termes)
    x <- m$donnees[[t]]
    if (isTRUE(m$bloc$centre[i] != 0) || !length(x) || !(min(x) > 0 || max(x) < 0) || !isTRUE(abs(mean(x)) > 5 * stats::sd(x))) next
    # (les variables qualitatives croisées avec elle qui ont leurs coefficients de catégorie, un effet principal)
    qual <- unique(unlist(lapply(comp[croisees[[t]]], function(ct) ct$variables[ct$categories != 0])))
    avec <- Filter(function(u) any(vapply(m$utiles, function(s) length(comp[[s]]$variables) == 1 && comp[[s]]$variables == u && comp[[s]]$categories != 0, TRUE)), qual)
    if (!length(avec)) next
    nom_q <- vapply(avec, function(u) {
      s <- Find(function(s) length(comp[[s]]$variables) == 1 && comp[[s]]$variables == u && comp[[s]]$categories != 0, names(comp))
      comp[[s]]$noms[1]
    }, "")
    g <- function(z) if (fr) sprintf("« %s »", z) else sprintf("“%s”", z)
    liste <- function(z) if (length(z) <= 1) paste(z) else paste(paste(z[-length(z)], collapse = ", "), if (fr) "et" else "and", z[length(z)])
    nom <- m$noms_termes[i]
    tr <- if (!is.null(d)) termes_regression(d$x)[[i]]$transform
    forme <- if (is.null(tr)) g(nom) else formule_transformation(tr, g(nom), fr)
    v6 <- function(z) trimws(formatC(z, digits = 6, format = "fg", decimal.mark = if (fr) "," else "."))
    meme <- !t %in% unlist(m$pentes)
    ajouter(paste0("centrage.", nom), "info",
            sprintf(if (fr) "%s est croisée avec %s sans être centrée, et ses valeurs (de %s à %s) sont loin de 0 : le coefficient de chaque catégorie de %s est son écart là où %s vaut 0, loin des données, d'où sa taille et son large intervalle. Centrée sur sa moyenne (%s), elle donnerait cet écart à sa moyenne%s."
                    else "%s is crossed with %s without being centred, and its values (%s to %s) are far from 0: each category's coefficient of %s is its difference where %s is 0, far from the data, hence its size and its wide interval. Centred on its mean (%s), it would give that difference at its mean%s.",
                    forme, liste(g(nom_q)), v6(min(x)), v6(max(x)), liste(g(nom_q)), forme, v6(mean(x)),
                    if (!meme) "" else if (fr) " : le même modèle, lu ailleurs" else ": the same model, read elsewhere"))
    out[[length(out)]]$centrer <- comp[[t]]$variables
    out[[length(out)]]$nom <- nom
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
