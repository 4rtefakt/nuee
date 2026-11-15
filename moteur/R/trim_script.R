# L'export vers R d'un modèle TRIM (analyses.R : ecrire_script_<type> ; trim.R) : un script qui
# refait ses indices et ses tendances avec R seul, sans Nuée ni autre paquet. Ses fonctions sont
# celles de TRIM du moteur, recopiées de trim.R (script_moteur.R : code_moteur) ; puis ses données ;
# le tableau sites × temps des comptages comme donnees_trim le fait (les sites sans comptage positif
# laissés de côté, la grille régulière des temps, le poids et la catégorie des covariables d'une case
# sans ligne) ; le modèle comme ajuster_trim l'ajuste (ses points de changement, le pas à pas, σ² et
# ρ), ses totaux, indices, pentes, paramètres, tests et son ajustement ; des graphiques en R de base.
# Ou un rapport R Markdown ou Quarto. Avant d'être écrit, son code est exécuté ici, sans le moteur,
# et ses résultats comparés à ceux du modèle de l'application (ecart_trim).

# Les noms du script en anglais (traduire_script) : ceux des fonctions de trim.R, puis ceux du calcul.
NOMS_TRIM_EN <- c(
  ajustement_trim = "goodness_of_fit_trim", alphas_trim = "site_effects_trim", cases_utiles = "useful_cells",
  changements_estimables = "estimable_changepoints", classe_pente = "slope_class", dessin_site = "site_design", dessins_trim = "designs_trim",
  droite_ecarts_trim = "line_and_deviations_trim", estimer_trim = "estimate_trim", nom_parametre_trim = "parameter_name_trim",
  parametres_trim = "parameters_trim", pas_a_pas_trim = "stepwise_trim", pente_globale = "overall_slope", prolonger = "carry_over",
  rapport_vraisemblance = "likelihood_ratio", score_changement = "changepoint_score", systeme_trim = "equations_trim", totaux_trim = "totals_trim",
  variance_totaux_modele = "model_totals_variance", TRIM_ENTREE = "TRIM_ENTER", TRIM_SORTIE = "TRIM_LEAVE", TRIM_PARAMETRE_MAX = "TRIM_PARAMETER_MAX",
  actifs = "active", additif = "additive", ajout = "added", ajouter = "add_test", avant = "before", avec_gee = "with_gee", bas = "lower",
  blocs = "blocks", bornes = "bounds", candidats = "candidates", categorie = "category", changement = "change", classe = "trend_class",
  connus = "known_values", converge = "converged", covariable = "covariate", covariables = "covariates", cp_actifs = "cp_active", ddl = "df",
  de = "from", depart = "initial_points", des = "design", entre = "entered", etape = "step", etapes = "steps", fins = "ends", gardes = "kept", haut = "upper",
  impute = "imputed", indice = "index", methode = "method", modele = "model", multiplicatif = "multiplicative", niveau = "level", nom = "name",
  nouveau = "new_beta", observe = "observed", observees = "observed_only", pas = "step_size", pas_a_pas = "stepwise", pas_db = "step_db",
  positif = "positive", premier = "first", produits = "products", quoi = "what", retires = "removed", se_additif = "se_additive",
  se_multiplicatif = "se_multiplicative", se_impute = "se_imputed", se_modele = "se_model", sorti = "left", surdispersion = "overdispersion",
  temps = "times", tete = "block_columns", tour = "turn", trop = "too_large", V_impute = "V_imputed", V_modele = "V_model", vide = "empty",
  vides = "empty_pieces",
  # (le calcul du script)
  compte = "count", poids = "weight", variables_cov = "covariate_values", connu = "known", garde = "kept_rows", pas_temps = "time_step",
  grille = "grid", case = "cell", remplir = "fill", rang = "rank_of", totaux = "totals", pentes = "slopes", libelles_classes = "class_labels",
  sens = "trend", parametres = "parameters", ecarts = "deviations", ajustement = "goodness_of_fit", libelles_tests = "test_labels",
  etapes_pas_a_pas = "stepwise_steps", sites_vides = "empty_sites", couleurs = "colours", bande = "band")

# Le commentaire anglais de chaque définition de trim.R que le script reprend (en français : le sien,
# celui du moteur).
ENTETES_TRIM_EN <- c(
  prolonger = "A missing value in a site's row (a time without a row, or a row without a value) takes that of its previous time that has one, otherwise that of the next.",
  dessins_trim = "The time design (J × p0) of a model: nothing (model 1), the pieces between the changepoints `cp` (ranks of times, model 2), one column per time after the first (model 3); with covariates, the design of each site (a list), the columns of each category after the reference added to those of the time design, zero at the times when the site is in another category. `blocks`: the reference, then each category of the covariates after the first.",
  dessin_site = "The time design of a site.",
  systeme_trim = "The model's equations for given α, β, σ² and ρ (rtrim § 3.2–3.4): μ (every cell), and for each site, its observed cells o, Ωi = diag(μo) Vi⁻¹ diag(μo) with Vi = σ² A½ Ri A½, di = 1ᵀΩi1 and Fi = (1ᵀΩi Bo) / di; the score of the time parameters U = Σ Boᵀ diag(μo) Vi⁻¹ (fo − μo) and their information with the site effects concentrated out, I* = Σ Boᵀ (Ωi − Ωi11ᵀΩi / di) Bo.",
  alphas_trim = "The site effects for given β (and ρ): each solves its site's equation, zᵀ (fo − μo) = 0 with z = Vi⁻¹ μo, that is αi = ln(zᵀ fo) − ln(zᵀ eo), eo = exp(Bo β − ln wo); z doesn't depend on αi (up to a factor, eo^−½ ⊙ Ri⁻¹ eo^½), and is 1 without serial correlation (maximum likelihood: αi = ln Σ fo − ln Σ eo). If zᵀ fo isn't positive (a strong correlation and a single positive count), the site takes the equation without correlation, as rtrim does.",
  rapport_vraisemblance = "The likelihood ratio (rtrim (25)): 2 Σ f ln(f / μ) over the observed positive counts.",
  dispersion_trim = "σ² and ρ from the Pearson residuals rij = (fij − μij) / √μij of the observed counts (rtrim (38) and (39)): σ² = Σ r² / (O − p), p the parameters (sites and time); ρ = Σ rij ri,j+1 / (N σ²) over the N pairs of successive observed times of a site.",
  nom_parametre_trim = "The name of a time parameter, for the messages (the time of a model 3, the changepoint of a model 2).",
  estimer_trim = "The fit: Poisson maximum likelihood by the scoring method (Fisher), the site effects concentrated out, the step halved while the likelihood ratio increases; then, with overdispersion or serial correlation, the GEE: at each round, α for β, σ² and ρ from the residuals, a scoring step for β; until β, σ² and ρ stop moving (TRIM_TOLERANCE).",
  cases_utiles = "Sites with a single positive count teach nothing about the time parameters (rtrim: check_model3, check_model2): the positive cells of the others.",
  changements_estimables = "The changepoints of a model 2 whose piece (from the time after the point to the next point, or to the last time) has no useful positive count (of a site that has two, or of each category of a covariate) are removed one by one as rtrim does (autodelete): the last empty piece (the second if it is the first). Returns the points kept and those removed (ranks of times).",
  variance_totaux_modele = "The covariance of the model totals of each time (rtrim (72)): G diag(d)⁻¹ Gᵀ + (GF − H) I*⁻¹ (GF − H)ᵀ, Gji = wij μij, Hjk = Σi wij μij (Bi)jk; `observed_only`: the observed cells only (for that of the imputed totals).",
  totaux_trim = "The totals of each time: observed (Σ wij fij over the observed counts), model (Σi wij μij) and imputed (fij observed, μij otherwise); the covariance of the imputed totals (rtrim (81)): that of the model, minus its share from the observed cells, plus the variance of the observed counts, σ² wij² μij, correlated ρ^|j−k| within a site.",
  indices_trim = "The indices: the total of each time over that of the base time b, their standard errors by the delta method (rtrim (89)); 0 at the base time.",
  classe_pente = "The class of a multiplicative slope from its confidence interval (TRIM 3; rtrim: overall): strong increase if its lower bound exceeds 1.05, moderate increase if it exceeds 1; steep decline if its upper bound is below 0.95, moderate decline if it is below 1; otherwise stable when the interval is within (0.95, 1.05), uncertain otherwise. The class comes from the 95% interval; `level` says whether the 99% one gives it too (p < 0.01) or only the 95% one (p < 0.05).",
  pente_globale = "The overall slope (rtrim (15), (92); overall): the least-squares line through ln tj against the rank of the time (1 … J), its standard error from the covariance of ln t (diag(1/t) V diag(1/t)), p by the t distribution with J − 2 degrees of freedom, the 95% interval of the multiplicative slope, its class.",
  tests_trim = "The Wald tests (rtrim § 2.8.2; wald): W = θᵀ var(θ)⁻¹ θ, χ² with rank(var θ) degrees of freedom. Model 2, a single point at the first time, without a covariate: the slope; model 2 otherwise: at each point, the change in slope (βl − βl−1, β0 = 0), of the reference and of each category of the covariates together; model 3 without a covariate: the deviations from the line (γ* of rtrim § 4.2's decomposition, J − 2 degrees of freedom; none for two times); each covariate: all its parameters.",
  droite_ecarts_trim = "Model 3 without a covariate as a line and deviations from it (rtrim § 4.2; coef(, \"trend\"), coef(, \"deviations\")): β* = Σ dj γj / D, γ*j = γj − mean(γ) − dj β*, dj = j − mean(j), D = Σ dj²; their covariance T cov(γ) Tᵀ (γ1 = 0 included).",
  score_changement = "The score test of a removed changepoint (rtrim § 4.1.1; Score): the model with it, its slope equal to the one before (the same μ as the model without it), S = Uᵀ I*⁻¹ U at the point of the model without it (its α, σ² and ρ); χ² with as many degrees of freedom as blocks.",
  pas_a_pas_trim = "The stepwise selection of the changepoints (rtrim § 4.1; trim_refine): at each round, the point with the largest Wald p leaves if it exceeds TRIM_LEAVE, then the removed point with the smallest score p comes back if it is below TRIM_ENTER; until nothing moves. Returns the model and the steps.",
  parametres_trim = "The parameters in a table: for each block (the reference, then each category of the covariates after theirs), a model 2 per piece (from, to), a model 3 per time (the first at 0); additive and multiplicative (exp), their standard errors (that of a multiplicative parameter: exp(θ) × se(θ)).",
  ajustement_trim = "The goodness of fit (Pearson chi-square, likelihood ratio, AIC = LR − 2 df; rtrim: gof), over the observed counts; df = O − (sites + time parameters).")

# Les commentaires du corps de ces définitions, en anglais (script_moteur.R : commentaires_anglais).
COMMENTAIRES_TRIM_EN <- c(
  "# (convergence : plus grand changement de β, σ² et ρ)" = "# (convergence: largest change of β, σ² and ρ)",
  "# (|β| au-delà : le modèle ne s'estime pas, comme rtrim)" = "# (|β| beyond: the model can't be estimated, as in rtrim)",
  "# (pas à pas : p de Wald au-delà duquel un point de changement sort ; rtrim premove)" = "# (stepwise: the Wald p beyond which a changepoint leaves; rtrim premove)",
  "# (p du test du score sous lequel il revient ; rtrim penter)" = "# (the score test p below which it comes back; rtrim penter)",
  "# (les changements de pente seulement : un dernier point au premier temps, sans covariable," = "# (the changes in slope only: a last point at the first time, without a covariate,",
  "# n'a qu'un test de la pente et reste, comme dans rtrim)" = "# has only a test of the slope and stays, as in rtrim)")

# Les définitions de trim.R dont le script de `m` a besoin (et ce qu'elles appellent : dependances_moteur).
racines_trim <- function(m) {
  t <- m$donnees
  c("estimer_trim", "dessins_trim", if (m$modele == 2) "changements_estimables", if (!is.null(m$pas_a_pas)) "pas_a_pas_trim",
    if (t$ponderee || length(t$covariables)) "prolonger", "totaux_trim", "indices_trim", "pente_globale", if (!is.null(m$ecarts)) "droite_ecarts_trim",
    "tests_trim", "parametres_trim", "ajustement_trim")
}

# Le code d'une colonne du tableau du script (une variable ou un identifiant : `spec`, comme
# list(by = "variable", variable = ...)) et si elle est qualitative, ou faite de nombres.
colonne_trim <- function(projet, spec, tab) {
  if (identical(as.character(spec$by)[1], "id")) return(list(code = sprintf("d[[%s]]", code_texte(tab$ids[[as.character(spec$id)[1]]])), type = "id"))
  v <- variable_du_projet(projet, spec$variable)
  list(code = sprintf("d[[%s]]", code_texte(tab$noms[[as.character(v$uid)]])), type = if (length(v$categories)) "qualitative" else "nombre")
}

# La partie « modèle » du script d'un TRIM (en français, ses commentaires « #fr » et « #en » ;
# traduire_script la met dans la langue du script).
code_calcul_trim <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  r <- m$reglages_trim
  t <- m$donnees
  g <- function(x) tx(sprintf("« %s »", texte_sur(x)), sprintf("“%s”", texte_sur(x)))
  site <- colonne_trim(projet, r$site, tab)
  temps <- colonne_trim(projet, r$temps, tab)
  code_site <- switch(site$type, nombre = sprintf("ifelse(is.na(%s), NA_character_, sprintf(\"%%.15g\", %s))", site$code, site$code), sprintf("as.character(%s)", site$code))
  code_temps <- if (temps$type == "nombre") temps$code else sprintf("suppressWarnings(as.numeric(sub(\",\", \".\", trimws(as.character(%s)), fixed = TRUE)))", temps$code)
  ordre_sites <- switch(site$type,
    nombre = c("sites <- unique(site[connu])", "sites <- sites[order(as.numeric(sites))]"),
    qualitative = sprintf("sites <- intersect(levels(%s), site[connu])", site$code),
    c("sites <- unique(site[connu])", "sites <- sites[order(suppressWarnings(as.numeric(sites)), sites)]"))
  y <- variable_du_projet(projet, d$y[[1]]$variables[1])
  # les covariables : chaque variable qualitative de X, une fois (ses catégories font ses termes)
  ucov <- unique(vapply(d$x, function(x) as.numeric(x$variables[1]), 0))
  noms_cov <- vapply(ucov, function(u) as.character(variable_du_projet(projet, u)$ids[1]), "")
  valeurs_cov <- sprintf("%s = d[[%s]]", vapply(noms_cov, code_texte, ""), vapply(ucov, function(u) code_texte(tab$noms[[as.character(u)]]), ""))
  cov <- if (length(ucov) == 1) sprintf("variables_cov <- list(%s)", valeurs_cov) else if (length(ucov)) code_valeurs(valeurs_cov, "variables_cov <- ", ouvrir = "list(")
  poids <- if (t$ponderee) sprintf("poids <- d[[%s]]", code_texte(tab$noms[[as.character(r$poids)]]))
  modele <- m$modele
  rangs <- function(x) sprintf("vapply(%s, rang, 0L)", code_nombres(x))
  cp <- if (modele == 2) {
    if (isTRUE(r$tous)) "cp <- seq_len(J - 1)" else if (length(r$changements)) sprintf("cp <- %s", rangs(r$changements)) else "cp <- 1L"
  }
  options_fit <- sprintf("surdispersion = %s, correlation = %s", if (r$surdispersion) "TRUE" else "FALSE", if (r$correlation) "TRUE" else "FALSE")
  quoi_cp <- if (isTRUE(r$tous)) tx("chaque temps sauf le dernier", "every time but the last")
             else if (length(r$changements)) liste_textes(format(r$changements, digits = 15), tx("et", "and"))
             else tx("le premier temps (une seule pente)", "the first time (a single slope)")
  c(code_lignes_definition(projet, d, tab, tx), "",
    commentaire(tx(sprintf("Le site (%s), le temps (%s) et le comptage (%s) de chaque ligne%s%s.", g(t$noms$site), g(t$noms$temps), g(t$noms$y),
                           if (t$ponderee) sprintf(", son poids (%s)", g(t$noms$poids)) else "", if (length(ucov)) sprintf(", ses covariables (%s)", liste_textes(vapply(noms_cov, g, ""), "et")) else ""),
                   sprintf("The site (%s), the time (%s) and the count (%s) of each row%s%s.", g(t$noms$site), g(t$noms$temps), g(t$noms$y),
                           if (t$ponderee) sprintf(", its weight (%s)", g(t$noms$poids)) else "", if (length(ucov)) sprintf(", its covariates (%s)", liste_textes(vapply(noms_cov, g, ""), "and")) else ""))),
    sprintf("site <- %s", code_site),
    sprintf("temps <- %s", code_temps),
    sprintf("compte <- round(d[[%s]])", code_texte(tab$noms[[as.character(y$uid)]])),
    poids, cov,
    "connu <- !is.na(site) & !is.na(temps)",
    "",
    commentaire(tx(sprintf("Les sites, dans l'ordre %s ; ceux sans aucun comptage positif sont laissés de côté, comme TRIM le fait%s.",
                           switch(site$type, nombre = "de leurs nombres", qualitative = "des catégories", "de leurs noms (les nombres d'abord)"),
                           if (length(t$sites_vides)) sprintf(" (ici %d)", length(t$sites_vides)) else ""),
                   sprintf("The sites, in the order %s; those without any positive count are left out, as TRIM does%s.",
                           switch(site$type, nombre = "of their numbers", qualitative = "of the categories", "of their names (numbers first)"),
                           if (length(t$sites_vides)) sprintf(" (here %d)", length(t$sites_vides)) else ""))),
    ordre_sites,
    "positif <- vapply(sites, function(s) any(connu & site == s & !is.na(compte) & compte > 0), TRUE)",
    "sites_vides <- sites[!positif]",
    "sites <- sites[positif]",
    "garde <- connu & site %in% sites",
    commentaire(tx("Les temps : la grille régulière du premier au dernier, au plus petit pas entre deux temps des données ; un temps sans aucune ligne y a sa place, tous ses comptages manquants.",
                   "The times: the regular grid from the first to the last, at the smallest step between two times of the data; a time without any row has its place, all its counts missing.")),
    "tv <- sort(unique(temps[garde]))",
    "pas_temps <- min(diff(tv))",
    "grille <- tv[1] + pas_temps * (seq_len(round((tv[length(tv)] - tv[1]) / pas_temps) + 1) - 1)",
    "I <- length(sites)",
    "J <- length(grille)",
    "",
    commentaire(tx(sprintf("Le tableau sites × temps des comptages (NA : manquant, TRIM l'impute)%s%s.",
                           if (t$ponderee) ", et celui des poids (une case sans ligne prend le poids du temps précédent de son site, sinon du suivant)" else "",
                           if (length(ucov)) ", la catégorie de chaque covariable dans chaque case (de même ; la première catégorie présente de chacune est sa référence, une covariable d'une seule catégorie est laissée de côté)" else ""),
                   sprintf("The sites × times table of the counts (NA: missing, TRIM imputes it)%s%s.",
                           if (t$ponderee) ", and that of the weights (a cell without a row takes the weight of its site's previous time, otherwise of the next)" else "",
                           if (length(ucov)) ", the category of each covariate in each cell (likewise; the first category present of each is its reference, a covariate with a single category is left out)" else ""))),
    "ij <- cbind(match(site[garde], sites), round((temps[garde] - tv[1]) / pas_temps) + 1)",
    "case <- function(v) {",
    "  M <- matrix(NA_real_, I, J)",
    "  M[ij] <- v[garde]",
    "  M",
    "}",
    if (t$ponderee || length(ucov)) c("remplir <- function(M) {", "  R <- t(apply(M, 1, prolonger))", "  dim(R) <- dim(M)", "  R", "}"),
    "f <- case(compte)",
    if (t$ponderee) "w <- remplir(case(poids))" else "w <- matrix(1, I, J)",
    "cov <- list()",
    "covariables <- list()",
    if (length(ucov)) c(
      "for (nom in names(variables_cov)) {",
      "  Z <- remplir(case(as.integer(variables_cov[[nom]])))",
      "  codes <- sort(unique(as.vector(Z[!is.na(Z)])))",
      "  if (length(codes) < 2) next",
      "  cov[[length(cov) + 1]] <- matrix(match(Z, codes), I, J)",
      "  covariables[[length(covariables) + 1]] <- list(nom = nom, categories = levels(variables_cov[[nom]])[codes])",
      "}"),
    "t <- list(f = f, w = w, cov = cov, covariables = covariables, sites = sites, temps = grille)",
    "",
    switch(as.character(modele),
      "1" = commentaire(tx("Le modèle 1 : un effet de chaque site, aucun effet du temps (ln μij = αi).", "Model 1: an effect of each site, no time effect (ln μij = αi).")),
      "2" = c(commentaire(tx(sprintf("Le modèle 2 : un effet de chaque site et une tendance linéaire sur l'échelle log dont la pente change aux points de changement, %s (par leur rang dans la grille)%s.",
                                     quoi_cp, if (length(t$covariables)) " ; avec les covariables, chaque catégorie après la référence a ses propres pentes" else ""),
                             sprintf("Model 2: an effect of each site and a linear trend on the log scale whose slope changes at the changepoints, %s (by their rank in the grid)%s.",
                                     quoi_cp, if (length(t$covariables)) "; with the covariates, each category after the reference has its own slopes" else ""))),
              "rang <- function(x) which(abs(grille - x) < 1e-8 * max(1, abs(x)))[1]",
              cp,
              commentaire(tx("Un point dont le morceau n'a aucun comptage positif utile est retiré, comme TRIM le fait.", "A point whose piece has no useful positive count is removed, as TRIM does.")),
              "ce <- changements_estimables(t, sort(unique(cp)))",
              "cp <- ce$cp"),
      commentaire(tx(sprintf("Le modèle 3 : un effet de chaque site et de chaque temps (ln μij = αi + γj, γ1 = 0)%s.", if (length(t$covariables)) " ; avec les covariables, chaque catégorie après la référence a ses propres effets du temps" else ""),
                     sprintf("Model 3: an effect of each site and of each time (ln μij = αi + γj, γ1 = 0)%s.", if (length(t$covariables)) "; with the covariates, each category after the reference has its own time effects" else "")))),
    commentaire(if (r$surdispersion || r$correlation) {
      tx(sprintf("Ajusté par des équations d'estimation généralisées (GEE) qui tiennent compte %s, après le maximum de vraisemblance de Poisson qui les commence.",
                 liste_textes(c(if (r$surdispersion) "de la surdispersion (σ²)", if (r$correlation) "de la corrélation sérielle entre deux temps successifs d'un site (ρ)"), "et")),
         sprintf("Fitted by generalized estimating equations (GEE) that account for %s, after the Poisson maximum likelihood that starts them.",
                 liste_textes(c(if (r$surdispersion) "overdispersion (σ²)", if (r$correlation) "the serial correlation between successive times of a site (ρ)"), "and")))
    } else tx("Ajusté par le maximum de vraisemblance de Poisson (sans surdispersion ni corrélation sérielle).", "Fitted by Poisson maximum likelihood (without overdispersion or serial correlation).")),
    if (!is.null(m$pas_a_pas)) c(
      commentaire(tx(sprintf("Les points de changement choisis pas à pas parmi ceux-là (un point sort quand le test de Wald de son changement de pente dépasse p = %s, revient quand son test du score passe sous p = %s).",
                             format(TRIM_SORTIE, decimal.mark = ","), format(TRIM_ENTREE, decimal.mark = ",")),
                     sprintf("The changepoints chosen stepwise among those (a point leaves when the Wald test of its change in slope exceeds p = %s, comes back when its score test falls below p = %s).",
                             format(TRIM_SORTIE), format(TRIM_ENTREE)))),
      sprintf("fit <- pas_a_pas_trim(t, cp, list(%s))", options_fit))
    else sprintf("fit <- estimer_trim(t, dessins_trim(t, %d%s), %s)", modele, if (modele == 2) ", cp" else "", options_fit),
    "",
    commentaire(tx(sprintf("Les totaux de chaque temps (observés, du modèle, imputés : les comptages observés, et ceux du modèle pour les manquants) et leurs erreurs-types ; les indices, rapportés au temps de base (%s), par la méthode delta.",
                           format(t$temps[m$base], digits = 15)),
                   sprintf("The totals of each time (observed, model, imputed: the observed counts, and the model's for the missing ones) and their standard errors; the indices, relative to the base time (%s), by the delta method.",
                           format(t$temps[m$base], digits = 15)))),
    if (m$base == 1) "base <- 1" else sprintf("base <- which(abs(grille - %s) < 1e-8 * max(1, abs(%s)))[1]", texte_exact(t$temps[m$base]), texte_exact(t$temps[m$base])),
    "tt <- totaux_trim(t, fit)",
    "im <- indices_trim(tt$modele, tt$V_modele, base)",
    "ii <- indices_trim(tt$impute, tt$V_impute, base)",
    "totaux <- data.frame(temps = grille, observe = tt$observe, modele = tt$modele, se_modele = sqrt(pmax(0, diag(tt$V_modele))),",
    "                     impute = tt$impute, se_impute = sqrt(pmax(0, diag(tt$V_impute))))",
    "indices <- data.frame(temps = grille, modele = im$indice, se_modele = im$se, impute = ii$indice, se_impute = ii$se)",
    commentaire(tx("Les pentes globales, des totaux imputés puis de ceux du modèle : la droite des moindres carrés à travers leur logarithme, multiplicative par pas de temps, son intervalle à 95 % (loi t à J − 2 degrés de liberté) et sa classe selon TRIM.",
                   "The overall slopes, of the imputed totals then of the model's: the least-squares line through their logarithm, multiplicative per time step, its 95% interval (t distribution with J − 2 degrees of freedom) and its class by TRIM.")),
    sprintf("pentes <- rbind(cbind(quoi = %s, pente_globale(tt$impute, tt$V_impute), stringsAsFactors = FALSE),", code_texte(tx("imputés", "imputed"))),
    sprintf("                cbind(quoi = %s, pente_globale(tt$modele, tt$V_modele), stringsAsFactors = FALSE))", code_texte(tx("modèle", "model"))),
    # (des libellés nommés par des chaînes : les valeurs des fonctions de TRIM, que la traduction ne change pas)
    sprintf("libelles_classes <- c(%s)", paste(sprintf("\"%s\" = %s", TRIM_CLASSES, vapply(libelle_classe_trim(TRIM_CLASSES, !identical(langue, "en")), code_texte, "")), collapse = ", ")),
    "pentes$sens <- unname(libelles_classes[pentes$classe])",
    if (!is.null(m$ecarts)) c(commentaire(tx("Le modèle 3 en droite et écarts à la droite.", "Model 3 as a line and deviations from it.")), "ecarts <- droite_ecarts_trim(fit)"),
    commentaire(tx("Les paramètres du temps, les tests de Wald et l'ajustement (khi² de Pearson, rapport de vraisemblance, AIC).",
                   "The time parameters, the Wald tests and the goodness of fit (Pearson chi-square, likelihood ratio, AIC).")),
    "parametres <- parametres_trim(t, fit)",
    if (identical(langue, "en") && modele == 2) "if (!is.null(parametres)) names(parametres)[names(parametres) == \"a\"] <- \"to\"",
    sprintf("tests <- tests_trim(t, fit%s)", if (!is.null(m$ecarts)) ", ecarts" else ""),
    sprintf("libelles_tests <- c(\"pente\" = %s, \"changement\" = %s, \"deviations\" = %s, \"covariable\" = %s)",
            code_texte(tx("pente", "slope")), code_texte(tx("changement de pente", "change in slope")), code_texte(tx("écarts à la droite", "deviations from the line")),
            code_texte(tx("covariable", "covariate"))),
    "tests$test <- unname(libelles_tests[tests$test])",
    "ajustement <- ajustement_trim(t, fit)",
    sprintf("dispersion <- c(%s)", paste(c(if (r$surdispersion) "sigma2 = fit$sig2", if (r$correlation) "rho = fit$rho"), collapse = ", ")),
    if (!is.null(m$pas_a_pas)) c(
      "etapes_pas_a_pas <- fit$pas_a_pas$etapes",
      sprintf("if (!is.null(etapes_pas_a_pas)) etapes_pas_a_pas$action <- unname(c(\"retire\" = %s, \"remis\" = %s)[etapes_pas_a_pas$action])",
              code_texte(tx("retiré", "removed")), code_texte(tx("remis", "put back")))))
}

# Les colonnes des pentes globales que le script affiche, dans sa langue.
colonnes_pentes_trim <- function(langue) sprintf("pentes[%s]", code_textes(nom_traduit(c("quoi", "multiplicative", "se_multiplicative", "p", "bas", "haut", "sens"), langue, NOMS_TRIM_EN)))

code_sortie_trim <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  ligne <- function(code, fr, en) ligne_sortie(traduire_script(code, langue, c(NOMS_TRIM_EN, NOMS_SCRIPT_EN)), tx(fr, en))
  c(if (!is.na(m$sigma2) || !is.na(m$rho)) ligne("print(dispersion)", "la surdispersion σ² et la corrélation sérielle ρ", "the overdispersion σ² and the serial correlation ρ"),
    ligne("print(indices)", "les indices du modèle et imputés, et leurs erreurs-types", "the model and imputed indices, and their standard errors"),
    ligne("print(totaux)", "les totaux de chaque temps", "the totals of each time"),
    ligne(sprintf("print(%s)", colonnes_pentes_trim(langue)), "les pentes globales et leur classe", "the overall slopes and their class"),
    if (!is.null(m$parametres)) ligne("print(parametres)", "les paramètres du temps", "the time parameters"),
    if (nrow(m$tests)) ligne("print(tests)", "les tests de Wald", "the Wald tests"),
    ligne("print(unlist(ajustement))", "khi², rapport de vraisemblance, degrés de liberté, p, AIC", "chi-square, likelihood ratio, degrees of freedom, p, AIC"),
    if (!is.null(m$ecarts)) ligne("print(c(pente = ecarts$bstar, se = ecarts$se_bstar))", "la pente de la droite du modèle 3 (additive)", "the slope of model 3's line (additive)"),
    if (!is.null(m$pas_a_pas)) ligne("print(etapes_pas_a_pas)", "les étapes du pas à pas", "the stepwise steps"),
    if (!is.null(m$pas_a_pas)) ligne("print(grille[fit$des$cp])", "les points de changement gardés", "the changepoints kept"))
}

# Les graphiques en R de base : les indices imputés et leur bande à 95 %, ceux du modèle ; les totaux.
code_graphiques_trim <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  t <- function(code) traduire_script(code, langue, c(NOMS_TRIM_EN, NOMS_SCRIPT_EN))
  y <- texte_sur(m$donnees$noms$y)
  list(
    list(nom = "indices", largeur = 7, hauteur = 5, code = t(c(
      "bande <- cbind(indices$impute - 1.96 * indices$se_impute, indices$impute + 1.96 * indices$se_impute)",
      sprintf("plot(indices$temps, indices$impute, type = \"n\", ylim = range(0, bande, indices$modele), xlab = %s, ylab = %s, main = %s)",
              code_texte(texte_sur(m$donnees$noms$temps)), code_texte(tx("Indice", "Index")), code_texte(sprintf(tx("Indices de %s (TRIM, modèle %d)", "Indices of %s (TRIM, model %d)"), y, m$modele))),
      "polygon(c(indices$temps, rev(indices$temps)), c(bande[, 1], rev(bande[, 2])), col = \"#2f6fb533\", border = NA)",
      "lines(indices$temps, indices$modele, col = \"#d1495b\", lwd = 2)",
      "points(indices$temps, indices$impute, pch = 19, col = \"#2f6fb5\")",
      "abline(h = 1, lty = 3)",
      sprintf("legend(\"topleft\", c(%s, %s), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2), col = c(\"#2f6fb5\", \"#d1495b\"), bty = \"n\")",
              code_texte(tx("imputés (bande à 95 %)", "imputed (95% band)")), code_texte(tx("modèle", "model")))))),
    list(nom = "totaux", largeur = 7, hauteur = 5, code = t(c(
      sprintf("matplot(totaux$temps, cbind(totaux$observe, totaux$impute, totaux$modele), type = \"b\", pch = c(1, 19, NA), lty = c(3, 1, 1), col = c(\"grey40\", \"#2f6fb5\", \"#d1495b\"), xlab = %s, ylab = %s, main = %s)",
              code_texte(texte_sur(m$donnees$noms$temps)), code_texte(tx("Total", "Total")), code_texte(tx("Totaux de chaque temps", "Totals of each time"))),
      sprintf("legend(\"topleft\", c(%s, %s, %s), pch = c(1, 19, NA), lty = c(3, 1, 1), col = c(\"grey40\", \"#2f6fb5\", \"#d1495b\"), bty = \"n\")",
              code_texte(tx("observés", "observed")), code_texte(tx("imputés", "imputed")), code_texte(tx("modèle", "model")))))))
}

# Les résultats du script (dans `e`) comparés à ceux de `m` : NULL, ou ce qui diffère.
ecart_trim <- function(m, e, langue) {
  r <- function(x) get0(nom_traduit(x, langue, NOMS_TRIM_EN), envir = e, inherits = FALSE)
  champ <- function(x, k) if (!is.null(x)) x[[nom_traduit(k, langue, NOMS_TRIM_EN)]]
  nombres <- function(x) if (!is.null(x)) as.matrix(Filter(is.numeric, x))
  fit <- r("fit")
  v <- m$ajustement_noyau
  pentes <- r("pentes")
  numeriques <- c("additive", "se", "multiplicative", "se_multiplicative", "p", "bas", "haut", "niveau")
  grille <- r("grille")
  paires <- list(
    "paramètres du temps" = list(v$beta, champ(fit, "beta")), covariance = list(v$V, champ(fit, "V")), "effets des sites" = list(v$alpha, champ(fit, "alpha")),
    "σ² et ρ" = list(c(v$sig2, v$rho), c(champ(fit, "sig2"), champ(fit, "rho"))),
    "points de changement" = list(m$changements, if (!is.null(fit)) grille[champ(champ(fit, "des"), "cp")]),
    "points retirés" = if (m$modele == 2) list(m$changements_retires, grille[champ(r("ce"), "retires")]),
    totaux = list(as.matrix(m$totaux), nombres(r("totaux"))), indices = list(as.matrix(m$indices), nombres(r("indices"))),
    pentes = list(as.matrix(m$pentes[numeriques]), if (!is.null(pentes)) as.matrix(pentes[nom_traduit(numeriques, langue, NOMS_TRIM_EN)])),
    "classes des pentes" = list(as.numeric(factor(m$pentes$classe, TRIM_CLASSES)), if (!is.null(pentes)) as.numeric(factor(pentes[[nom_traduit("classe", langue, NOMS_TRIM_EN)]], TRIM_CLASSES))),
    parametres = if (!is.null(m$parametres)) list(nombres(m$parametres), nombres(r("parametres"))),
    "tests de Wald" = if (nrow(m$tests)) list(nombres(m$tests), nombres(r("tests"))),
    ajustement = list(unlist(m$ajustement), unlist(r("ajustement"))),
    "droite et écarts" = if (!is.null(m$ecarts)) list(unlist(m$ecarts), unlist(r("ecarts"))),
    "pas à pas" = if (!is.null(m$pas_a_pas$etapes)) list(nombres(m$pas_a_pas$etapes), nombres(r("etapes_pas_a_pas"))))
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  # (les libellés du script : ceux de chaque classe et de chaque test)
  libelles <- c(champ(pentes, "sens"), champ(r("tests"), "test"))
  if (anyNA(libelles)) ecarts["libellés"] <- Inf
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

ecrire_script_trim <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  r <- m$reglages_trim
  t <- m$donnees
  # (les identifiants du site et du temps, et ceux que lit sa condition : condition.R)
  ids <- unlist(lapply(list(r$site, r$temps), function(s) if (identical(as.character(s$by)[1], "id")) as.character(s$id)[1]))
  tab <- tableau_script(projet, unique(c(ids, ids_condition(projet, d))))
  noms <- c(NOMS_TRIM_EN, NOMS_SCRIPT_EN)
  fc <- plan_condition(projet, d, tab)$fonctions  # (celles de Nouvelle variable qu'appelle sa condition : condition.R)
  fonctions <- traduire_script(c(code_moteur(racines_trim(m), langue, ENTETES_TRIM_EN, COMMENTAIRES_TRIM_EN),
                                 if (length(fc)) code_fonctions_condition(fc, tx)), langue, noms, elements = TRUE)
  methode <- if (r$surdispersion || r$correlation) tx("GEE", "GEE") else tx("maximum de vraisemblance de Poisson", "Poisson maximum likelihood")
  piece <- list(
    quoi = sprintf(tx("TRIM, modèle %d (%s), %d sites × %d temps, %d comptages observés", "TRIM, model %d (%s), %d sites × %d times, %d observed counts"),
                   m$modele, methode, m$I, m$J, m$O),
    tab = tab, fonctions = fonctions,
    calcul = traduire_script(code_calcul_trim(projet, d, m, tab, langue), langue, noms),
    sortie = code_sortie_trim(m, langue), graphiques = code_graphiques_trim(m, langue),
    ecart = function(m, e) ecart_trim(m, e, langue), paquets = character(0),
    texte_paquets = tx("Il refait ce modèle avec R seul, sans Nuée ni autre paquet, de ses données à ses résultats, qui sont ceux de Nuée : ses fonctions sont celles de TRIM du moteur de Nuée, recopiées telles quelles (écrites d'après la méthode publiée de TRIM, elles donnent les résultats du paquet rtrim). Ses quatre parties :",
                       "It redoes this model with R alone, without Nuée or any package, from its data to its results, which are those of Nuée: its functions are TRIM's from Nuée's engine, copied as they are (written from TRIM's published method, they give the results of the rtrim package). Its four parts:"),
    parties = c(tx("#   1. Fonctions : celles de TRIM du moteur de Nuée", "#   1. Functions: TRIM's, from Nuée's engine"),
                tx("#   3. Modèle : sites, temps et comptages, ajustement, totaux, indices, pentes, tests", "#   3. Model: sites, times and counts, fit, totals, indices, slopes, tests")),
    resultats = Filter(Negate(is.null), list(
      list(nom = "indices-table", objet = traduire_script("indices", langue, noms), chiffres = 4),
      list(nom = "pentes-table", objet = traduire_script(colonnes_pentes_trim(langue), langue, noms), chiffres = 4),
      list(nom = "totaux-table", objet = traduire_script("totaux", langue, noms), chiffres = 1),
      if (nrow(m$tests)) list(nom = "tests-table", objet = "tests", chiffres = 4),
      list(nom = "ajustement-table", objet = traduire_script("t(unlist(ajustement))", langue, noms), chiffres = 3))),
    objets = nom_traduit(c("t", "fit", "tt", "totaux", "indices", "pentes", "parametres", "tests", "ajustement", "ecarts", "dispersion", "etapes_pas_a_pas"), langue, NOMS_TRIM_EN))
  ecrire_script_moteur(projet, d, chemin_script, m, piece, donnees, renv, rdata, rproj, version, langue)
}
