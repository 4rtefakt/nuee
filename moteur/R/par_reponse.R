# Un modèle par réponse (Modèle › Un modèle par réponse) : le même modèle (une régression : ses
# termes, ses effets aléatoires, sa loi, ses courbes, ses observations) ajusté à chacune de
# plusieurs variables réponses, une espèce après l'autre, et leurs résultats réunis : un tableau
# d'une ligne par réponse et par terme (son coefficient, son intervalle à 95 %, sa valeur p, et la
# valeur p corrigée sur les réponses, terme par terme : Benjamini et Hochberg (1995) par défaut,
# Holm ou Bonferroni), ses courbes (edf et test), la qualité de chaque modèle (N, AIC, R² ou
# déviance expliquée, Q²), et l'effet de chaque terme quantitatif pour les petits graphiques côte à
# côte. Une réponse dont le modèle ne s'ajuste pas est nommée avec la raison ; les autres restent.
# La transformation de la réponse du modèle d'origine (log10(x + 1)...) s'applique à chacune.

CORRECTIONS_REPONSES <- c("BH", "holm", "bonferroni")

# La définition `d` (d'une régression) avec la réponse `uid` à la place de la sienne, sous le nom
# `nom` ; sa transformation et sa mise à l'échelle gardées.
definition_pour_reponse <- function(projet, d, uid, nom) {
  v <- variable_du_projet(projet, uid)
  if (length(v$categories)) {
    stop(sprintf(traduire("« %s » est qualitative : une réponse est une variable quantitative.", "“%s” is qualitative: a response is a quantitative variable."), v$ids[1]), call. = FALSE)
  }
  y <- d$y[[1]]
  y$variables <- as.numeric(uid)
  y$name <- as.character(v$ids[1])
  d$y <- list(y)
  d$name <- nom
  d$title <- as.character(v$ids[1])
  d$app <- TRUE
  d$demande <- NULL
  d
}

# Les modèles des réponses `reponses` (numéros de variables), d'après le modèle `m` de définition
# `d` : list(modeles = list par réponse (son modèle et sa définition, ou son erreur), tableau,
# resume, effets). `correction` : celle des valeurs p sur les réponses (CORRECTIONS_REPONSES).
modeles_par_reponse <- function(projet, d, reponses, correction = "BH") {
  if (!est_regression(d)) stop(traduire("Un modèle par réponse se fait d'après une régression (LM, LMM, GLM ou GAM).", "One model per response is made from a regression (LM, LMM, GLM or GAM)."), call. = FALSE)
  correction <- match.arg(as.character(correction)[1], CORRECTIONS_REPONSES)
  reponses <- unique(as.numeric(reponses))
  if (!length(reponses)) stop(traduire("Choisissez au moins une réponse.", "Choose at least one response."), call. = FALSE)
  x <- unlist(lapply(d$x, function(t) as.numeric(t$variables)))
  if (any(reponses %in% x)) {
    stop(sprintf(traduire("Une variable X ne peut pas être aussi une réponse : %s.", "An X variable can't also be a response: %s."),
                 paste(unname(projet$noms_var[nom_numero(intersect(reponses, x))]), collapse = ", ")), call. = FALSE)
  }
  base <- d$name %||% "M"
  modeles <- lapply(reponses, function(u) {
    nom <- unname(projet$noms_var[nom_numero(u)])
    tryCatch({
      dk <- definition_pour_reponse(projet, d, u, sprintf("%s · %s", base, nom))
      list(reponse = nom, uid = u, definition = dk, modele = ajuster(projet, dk))
    }, error = function(e) list(reponse = nom, uid = u, erreur = conditionMessage(e)))
  })
  ok <- Filter(function(r) is.null(r$erreur), modeles)
  lignes <- lapply(ok, function(r) {
    co <- r$modele$resultat$coefficients
    co <- co[co$terme != "(Intercept)", , drop = FALSE]
    if (!nrow(co)) return(NULL)
    data.frame(reponse = r$reponse, terme = co$terme, estimation = co$estimation, erreur_type = co$erreur_type, bas = co$bas, haut = co$haut,
               p = co$p, stringsAsFactors = FALSE)
  })
  # (les courbes d'un GAM : leur edf et leur test, une ligne comme un terme, sans coefficient)
  courbes <- lapply(ok, function(r) {
    cb <- r$modele$resultat$courbes
    if (is.null(cb) || !nrow(cb)) return(NULL)
    data.frame(reponse = r$reponse, terme = if (!is.null(cb$niveau)) ifelse(is.na(cb$niveau), cb$terme, sprintf("%s (%s %s)", cb$terme, cb$par, cb$niveau)) else cb$terme,
               estimation = NA_real_, erreur_type = NA_real_, bas = NA_real_, haut = NA_real_, p = cb$p, edf = cb$edf, stringsAsFactors = FALSE)
  })
  tableau <- do.call(rbind, c(lapply(lignes, function(l) if (!is.null(l)) cbind(l, edf = NA_real_)), courbes))
  if (!is.null(tableau) && nrow(tableau)) {
    tableau$p_corrige <- stats::ave(tableau$p, tableau$terme, FUN = function(p) stats::p.adjust(p, correction))
    tableau <- tableau[order(match(tableau$terme, unique(tableau$terme)), match(tableau$reponse, vapply(ok, `[[`, "", "reponse"))), , drop = FALSE]
    rownames(tableau) <- NULL
  }
  resume <- do.call(rbind, lapply(modeles, function(r) {
    if (!is.null(r$erreur)) return(data.frame(reponse = r$reponse, N = NA_integer_, AIC = NA_real_, R2 = NA_real_, Q2 = NA_real_, erreur = r$erreur, stringsAsFactors = FALSE))
    m <- r$modele
    r2 <- m$resultat$r2
    data.frame(reponse = r$reponse, N = m$N, AIC = m$resultat$AIC %||% NA_real_,
               R2 = as.numeric(r2$r2 %||% r2$marginal %||% r2$deviance_expliquee %||% r2$r2_ajuste %||% NA_real_)[1],
               Q2 = m$vc$Q2 %||% NA_real_, erreur = "", stringsAsFactors = FALSE)
  }))
  effets <- lapply(ok, function(r) list(reponse = r$reponse, effets = tryCatch(effets_regression(r$modele), error = function(e) list())))
  list(modeles = modeles, tableau = tableau, resume = resume, effets = effets, correction = correction)
}

# Les phrases des méthodes d'un ensemble de modèles par réponse : combien de réponses, le modèle
# commun, la correction.
phrase_methodes_par_reponse <- function(n, correction, fr = TRUE) {
  noms <- c(BH = if (fr) "de Benjamini et Hochberg (1995)" else "Benjamini and Hochberg's (1995)",
            holm = if (fr) "de Holm (1979)" else "Holm's (1979)", bonferroni = if (fr) "de Bonferroni" else "Bonferroni's")
  sprintf(if (fr) "Le même modèle a été ajusté séparément à chacune des %d réponses ; pour chaque terme, les valeurs p ont été corrigées sur les réponses par la méthode %s."
          else "The same model was fitted separately to each of the %d responses; for each term, the p-values were corrected over the responses by %s method.",
          n, noms[[correction]])
}
