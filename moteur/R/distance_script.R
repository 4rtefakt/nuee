# L'export vers R d'un échantillonnage par distances (analyses.R : ecrire_script_<type> ; distance.R) :
# un script qui refait la fonction de détection, la densité et l'effectif avec R seul, sans Nuée ni
# Distance ni mrds. Ses fonctions sont celles du moteur, recopiées de distance.R (script_moteur.R :
# code_moteur) : la fonction de détection et sa vraisemblance, l'ajustement sous la contrainte de
# monotonie, le choix des termes d'ajustement par l'AIC comme ds(), les covariables de son échelle
# (ds(formula = ) : leur matrice, la probabilité de chaque détection, p̄, le modèle sans covariables
# comparé), p et son gradient, l'adéquation, la densité et l'effectif par strate et au total comme
# dht2() (de Horvitz–Thompson avec des covariables) ; elles lisent les éléments de leurs
# listes par `$`, jamais par une chaîne (vapply(l, `[[`, 0, "nom")), pour qu'un script en anglais les
# renomme avec elles. Puis ses données ; les distances, transects, strates, efforts, aires et tailles
# des groupes de ses lignes comme donnees_echantillonnage les lit (les lignes du modèle ou sa
# condition, le transect et la strate d'une variable ou d'un identifiant, en texte, la troncature à
# droite en distance ou en pourcentage, à gauche, les intervalles, les transects et les strates dans
# l'ordre du moteur) ; le modèle comme ajuster_distance l'ajuste ; des graphiques en R de base (la
# fonction de détection sur l'histogramme des distances, la densité des distances pour des points, le
# diagramme quantile-quantile). Ou un rapport R Markdown ou Quarto. Avant d'être écrit, son code est
# exécuté ici, sans le moteur, et ses résultats comparés à ceux du modèle de l'application
# (ecart_distance).

# Les noms du script en anglais (traduire_script) : ceux des fonctions de distance.R, puis ceux du
# calcul. (Ni le nom d'un argument d'une fonction de R que le script appelle, ni celui d'un élément
# de ce qu'elle rend, comme $par et $value de nlminb() et integrate().)
NOMS_DISTANCE_EN <- c(
  integrer_detection = "integrate_detection", indices_detection = "detection_indices", noms_parametres_detection = "detection_parameter_names",
  hermite_proba = "hermite_prob", serie_ajustement = "adjustment_series", cle_detection = "detection_key", detection_distance = "detection_function",
  poids_detection = "detection_weight", integrale_detection = "detection_integral", classes_detection = "detection_classes",
  vraisemblance_detection = "detection_likelihood", probabilite_detection = "detection_probability", contraintes_monotonie = "monotonicity_constraints",
  depart_detection = "detection_start", bornes_detection = "detection_bounds", minimiser_detection = "minimize_detection",
  sous_contraintes_detection = "constrained_detection", ajuster_detection = "fit_detection", inverse_information = "information_inverse",
  ordres_ajustement = "adjustment_orders", trop_de_parametres = "too_many_parameters", libelle_detection = "detection_label",
  choisir_detection = "choose_detection", candidats_detection = "detection_candidates", courbe_detection = "detection_curve",
  repartition_detection = "detection_cdf", bords_classes = "class_edges", adequation_detection = "detection_goodness_of_fit",
  variance_rencontre = "encounter_rate_variance", densite_echantillonnage = "sampling_density", facteur_echantillonnage = "units_factor",
  histogramme_echantillonnage = "sampling_histogram",
  tests_adequation = "goodness_of_fit_tests", matrice_covariables = "covariate_matrix", avec_covariables = "with_covariates",
  sans_covariables = "without_covariates", theta_combinaison = "combination_theta", jacobien_combinaison = "combination_jacobian",
  vraisemblance_covariables = "covariate_likelihood", probabilites_covariables = "covariate_probabilities", moyenne_detection = "average_detection",
  reduction_covariables = "covariate_reduction", choisir_covariables = "choose_covariates", candidats_covariables = "covariate_candidates",
  courbes_covariables = "covariate_curves", adequation_covariables = "covariate_goodness_of_fit", detection_covariables = "covariate_detection",
  DISTANCE_AJUSTEMENTS_MAX = "DISTANCE_MAX_ADJUSTMENTS", DISTANCE_POINTS_MONO = "DISTANCE_MONO_POINTS", DISTANCE_TOLERANCE = "DISTANCE_TOLERANCE",
  DISTANCE_UNITES_LONGUEUR = "DISTANCE_LENGTH_UNITS", DISTANCE_UNITES_AIRE = "DISTANCE_AREA_UNITS",
  # (les noms des fonctions : leurs arguments, leurs variables, les éléments de leurs listes)
  cle = "key", serie = "series", ordres = "orders", echelle_ajustements = "adjustment_scale", coupures = "cutpoints", forme = "shape",
  echelle = "scale", ajust = "adj", ajustements = "adjustments", dechelle = "dscale", dforme = "dshape", derivees = "derivatives",
  par_echelle = "by_scale", valeur = "integrand", derivee = "derivative_of", vus = "seen", bas = "lower", haut = "upper",
  libre_en_bas = "free_at_lower", libre = "free", pas = "step", essai = "trial", multiplicateurs = "multipliers", depart = "start",
  monotonie = "monotonicity", contraint = "constrained", a_la_borne = "at_bound", singuliere = "singular", converge = "converged",
  meilleur = "best", candidats = "candidates", raison = "reason", choisi = "chosen", fd_de = "fd_of", n_fixe = "n_fixed",
  modele = "model", parametres = "parameters", bords = "edges", observe = "observed", attendu = "expected", tot = "total", ddl = "df",
  khi2 = "chi_square", ajustee = "fitted_cdf", empirique_bas = "empirical_below", empirique = "empirical", de = "from",
  ligne = "stratum_row", lignes = "rows", couverte = "covered", aire = "area", individus = "individuals",
  rencontre = "encounter_rate", se_rencontre = "se_encounter_rate", cv_rencontre = "cv_encounter_rate", ddl_rencontre = "df_encounter_rate",
  er_ddl = "er_df", tailles = "sizes", g_moy = "g_mean", taille_moyenne = "mean_size", se_taille = "se_size", var_groupes_N = "var_groups_N",
  cv_groupes = "cv_groups", poids = "weights", de_chaque = "of_each", strate = "stratum", strates = "strata", densite_seule = "density_only",
  groupes = "groups", taille_x = "size_x", strate_x = "stratum_x", bas_D = "lower_D", haut_D = "upper_D", bas_N = "lower_N", haut_N = "upper_N",
  part_rencontre = "share_encounter_rate", part_detection = "share_detection", part_groupes = "share_groups", unites = "units",
  facteur = "conversion_factor", comptes = "counts", compte = "count", ic = "ci",
  # (les covariables : leurs fonctions, leurs variables, les éléments de leurs listes)
  vraisemblance = "likelihood", reduction = "reduction", vers_theta = "to_theta", hors_bornes = "out_of_bounds", cle_seule = "key_only",
  colonnes = "columns", termes = "terms", colonne = "column", covariable = "covariate", covariables = "covariates", niveau = "level", niveaux = "levels",
  terme = "term", combinaisons = "combinations", formule = "formula", noms_echelle = "scale_names", p_combinaisons = "p_combinations",
  dp_combinaisons = "dp_combinations", aucune = "no_series", cands = "candidate_rows", moyennes_ponderees = "weighted_means", toutes = "all_detections",
  par_niveau = "by_level", valeurs = "values", gradient = "gradient", si = "s_i",
  # (le calcul du script)
  taille = "size", garde = "kept", dans = "in_truncation", releve = "survey", choix = "choice", raisons = "reasons", raison_choix = "choice_reason",
  largeur = "width", adequation = "goodness_of_fit", reglages = "settings", densite = "densities", releve_groupes = "survey_groups",
  densite_groupes = "group_densities", courbe = "curve", histogramme = "histogram", aires = "areas", parametre = "parameter",
  estimation = "estimate", statistique = "statistic", choix_sans = "choice_without", p_moyenne = "p_mean", courbes = "curves", niveaux_k = "levels_k")

# Le commentaire anglais de chaque définition de distance.R que le script reprend (en français : le
# sien, celui du moteur).
ENTETES_DISTANCE_EN <- c(
  integrer_detection = "An integral of `f` from a to b by R's adaptive Gauss–Kronrod quadrature (integrate): a smooth function takes 21 points, a stepped hazard-rate (large b) or a function that reaches zero (adjustments that make it negative) takes more.",
  indices_detection = "The positions of the parameters in θ: shape (ln b, hazard-rate), scale (ln σ, half-normal and hazard-rate; with covariates, the coefficients of ln σ, the intercept first: with_covariates), adjustments (aj).",
  noms_parametres_detection = "The names of the parameters, in the order of θ: ln_b, ln_sigma (with covariates, the intercept of ln σ, then the coefficient of each covariate, named as its term: “OBs(TJS)”, “MAS”), then the series and order of each adjustment term (cos_2…).",
  hermite_proba = "The (probabilists') Hermite polynomials: He0 = 1, He1 = u, Hek+1 = u Hek − k Hek−1.",
  serie_ajustement = "The adjustment terms fj(u) and their derivatives in u, one column per order.",
  cle_detection = "The key k(x) and its derivatives by ln b and ln σ. Hazard-rate: k = 1 − exp(−z), z = (x/σ)^−b; dk/dln σ = b z e^−z, dk/dln b = z e^−z ln z (zero at x = 0, where z is infinite).",
  detection_distance = "g̃(x) = k(x) (1 + Σ aj fj(x/h)), without the division by its value at 0, 1 + s0 (k(0) = 1 for the three keys), and, on request, their derivatives by θ (one column per parameter).",
  poids_detection = "The density of the observed distances, up to a constant: 1 / (w − L) (lines), 2x / w² (points), as mrds (fx, fr): the likelihood keeps the same terms.",
  integrale_detection = "The integrals of detection_weight × g̃ from each `a` to each `b` (a single `a`: the same start), and their derivatives by θ, a value below 1e-16 counting as 0 as in mrds (gstdint). A key alone, without adjustment terms: the half-normal integrated exactly, as gstdint (lines: σ √(2π) (Φ(b/σ) − Φ(a/σ)) / (w − L); points: 2 σ² (exp(−a²/2σ²) − exp(−b²/2σ²)) / w²), and the derivative by ln σ drawn from the integral, g depending on x/σ only (∂g/∂ln σ = −x ∂g/∂x, integrated by parts: I − (b g(b) − a g(a)) / (w − L) for lines, 2 I − 2 (b² g(b) − a² g(a)) / w² for points); the hazard-rate's integral and its derivative by ln b stay numerical.",
  classes_detection = "Distances recorded in intervals (fd$cutpoints, as ds(cutpoints = ): the first the left truncation, the last w): the class of each distance, right-closed (the first closed on both sides), as Distance's create_bins.",
  vraisemblance_detection = "The log-likelihood of the exact distances `x` (between L and w): Σ ln f(xi), f = weight × g̃ / ∫ weight × g̃; a density below 1e-15 counting as 1e-15 (mrds: flpt.lnl). Distances in intervals: Σ ln (∫ over its class / ∫ from L to w), the multinomial distribution of the classes' counts. With `scores`, the derivative of each term by θ (one row per observation).",
  probabilite_detection = "The average probability of detection p = ∫ weight × g over [L, w] (g relative to g(0)) and its gradient.",
  contraintes_monotonie = "mrds's monotonicity constraints (flnl.constr) at DISTANCE_MONO_POINTS points xr = w (i/10)^1.5: strict, g(xr[i−1]) − g(xr[i]) ≥ 0 (g(0) = 1 before the first); weak, 1 − g(xr[i]) ≥ 0; and g(xr[i]) ≥ 0. Their values `c` and their Jacobian `J`.",
  depart_detection = "mrds's starting values (setinitial.ds): ln σ = the mean of ln(x + w/1000) (half-normal); for the hazard-rate, the best likelihood of a grid (sethazard: σ = s/2 + 0.2 j s, s the root mean square of the x, j from 1 to 10; b = 1, 3… 29; the first cell keeps b = 2, as mrds); the adjustments at 1e-16. Distances in intervals count there as the midpoint of their class. With covariates, the coefficients of ln σ are those of the linear regression of ln(x + w/1000) on them (lm), and the hazard-rate's shape that of the grid, without covariates.",
  bornes_detection = "The bounds of the parameters: the hazard-rate's ln b ≥ 0 (b ≥ 1, mrds's bound); the others, wide, only stop a fit that runs off to infinity (σ more than 1000 times the truncation w or 160,000 times smaller, b beyond 1000, an adjustment term beyond 1000): a degenerate model (a half-normal turned flat, the uniform key under another name), which doesn't converge. With covariates, these are the bounds of the reduced coefficients (covariate_reduction): the intercept, ln σ at the reference categories and at the means of the quantitative covariates, within those of ln σ; each other coefficient, a difference of ln σ between two categories or for one standard deviation of a covariate, below 19 (the span of the bounds of ln σ); and the σ of each combination of covariates is kept within the bounds of σ (fit_detection).",
  reduction_covariables = "The reduction of the coefficients of a detection function with covariates for the fit, θ = R θ̃: each quantitative covariate centred and scaled over the detections (a 0/1 indicator as it is), ln σ = Z β = Z̃ β̃; the fit, better conditioned (an intercept that no longer depends on the slope of a covariate far from 0), finds the same maximum, returned in θ.",
  minimiser_detection = "Minimizes `f` (gradient `gr`) from `theta` within the bounds, then a few Newton steps on the free parameters (the Hessian by differences of the gradient) for a precise optimum.",
  sous_contraintes_detection = "The maximum likelihood under the constraints `cons` (c ≥ 0) by an augmented Lagrangian (Powell–Hestenes–Rockafellar): each subproblem minimized as above, the multipliers λ ← max(0, λ − ρ c), ρ multiplied by ten while the violation doesn't fall enough.",
  ajuster_detection = "A detection function fitted to the distances `x`: the key, the series and its orders, from `start` (or mrds's starting values), under the constraint `monotonicity` (\"stricte\": strict, \"faible\": weak, \"aucune\": none; a key alone is never constrained, as in mrds). Its parameters, the log-likelihood, the AIC, the matrix of the sum of the outer products of the scores (`H`, mrds: flt.var) and the covariance of the parameters (its inverse, mrds: solvecov). `likelihood`: detection_likelihood, or covariate_likelihood for a detection function with covariates (never constrained, as in ds), fitted on its reduced coefficients (`reduction`, R of covariate_reduction: θ = R θ̃, the bounds and the conditioning of the information are those of θ̃, the covariance R H̃⁻¹ R'); the σ of each of its combinations of covariates kept within the bounds of σ (`out_of_bounds`: those that leave them).",
  inverse_information = "The inverse of an information matrix as mrds (solvecov): by its eigenvectors, an eigenvalue below 1e−10 replaced (its inverse is 1e10); NA if it can't be inverted.",
  ordres_ajustement = "The orders of Distance's adjustment terms (get_adj_orders): cosine 2, 3, 4… (1, 2, 3… with the uniform key); Hermite and polynomials 4, 6, 8… (2, 4, 6… with the uniform key).",
  trop_de_parametres = "More parameters than distinct distances, or than classes minus one (mrds, ddf.ds: those of the key and of the adjustment terms, without the coefficients of the covariates).",
  libelle_detection = "The label of a detection function: “hn”, “hn + cos(2, 3)”, and its covariates: “hr ~ OBs + MAS”.",
  choisir_detection = "ds()'s choice: the key alone (without constraint), then 1, 2… adjustment terms (at most `n_max`), each starting from the parameters of the previous one (and 0 for the new term; the first, from mrds's starting values), while the AIC doesn't rise; a model that doesn't converge also stops the search. With `n_fixed`, that number of terms only. Returns the model retained and the candidates.",
  candidats_detection = "The models fitted along the way by choose_detection, as a table: their label, their key, their series and its orders, their number of parameters, their log-likelihood and their AIC, whether they converge, whether the monotonicity constraint acted; each AIC's difference from the smallest of those that converge, and the model retained.",
  courbe_detection = "The fitted detection function `det` at 101 distances from L to w (the data `t`), and the density of the distances, g / ((w − L) p) for lines, f(r) = 2r g(r) / (w² p) for points.",
  repartition_detection = "The fitted distribution function of the distances at each x: ∫ from L to x over ∫ from L to w.",
  pcramer_detection = "The limiting distribution of the Cramér–von Mises criterion (Anderson and Darling, 1952), its first four terms, as mrds (pcramer).",
  bords_classes = "The classes of the histogram and of the chi-square: those of the distances in intervals, otherwise √n equal classes from L to w (gof.ds, plot.ds).",
  adequation_detection = "mrds's goodness-of-fit tests (ddf.gof): Cramér–von Mises on the exact distances (W = 1/12n + Σ (F(x(i)) − (i − ½)/n)², asymptotic p) and the chi-square on the classes (gof.ds: right-closed, df = classes − 1 − parameters); the QQ plot (the fitted distribution of each distance against the empirical distribution, as qqplot.ddf). Distances in intervals: the chi-square only, on their classes.",
  DISTANCE_UNITES_LONGUEUR = "Distance's lengths and areas (units_table), in metres and square metres.",
  facteur_echantillonnage = "The factor that puts the truncation w in the unit of the areas, as Distance's convert_units: lines, (distance × effort) / area, 2 w L is then an area; points, distance / √area, π w² is one.",
  variance_rencontre = "mrds's encounter rate variance (varn): R2 (lines) and P2 (points) of Fewster et al. (2009); `l` the transects' efforts, `n` their detections; 0 with a single transect (dht2).",
  densite_echantillonnage = "The density and abundance by stratum and overall, as dht2 (geographical stratification, without multiplier): `t` the data, `p`, `var_p` the probability of detection and its variance, `q` the parameters of the detection function, `cu` the units factor. A detection function with covariates (`ht`: the probability of detection of each detection, its gradient by the parameters and their covariance V): the Horvitz–Thompson covered abundance Σ si / pi (si the group size, 1 without groups), and the variance of the detection by the delta method, ∇N' V ∇N, ∇N = −(A / a) Σ si ∇pi / pi² (for the total, the sum of the strata's gradients weighted as their abundances).",
  tests_adequation = "The goodness-of-fit tests from the classes (`edges`) and their expected counts, and the fitted distribution `F` of each distance (NULL for distances in intervals): the chi-square (df = classes − 1 − parameters), Cramér–von Mises and the QQ plot (detection_goodness_of_fit).",
  matrice_covariables = "The matrix of the covariates of the scale, as model.matrix(~ x1 + x2) of ds(formula = ~ x1 + x2) (mrds: setcov): the intercept, then each covariate in its order, a quantitative one as it is, a qualitative one (a factor) by a 0/1 indicator of each of its categories present but the first, its reference category (R's treatment contrasts; mrds leaves out the categories absent from the detections, process.data). `columns`: the covariates of the detections, a named list. Its columns are named as R names them (“OBsTJS”); attr(, \"termes\"): the covariate, the category (NA for a quantitative one) and the Nuée name (“OBs(TJS)”) of each; attr(, \"references\"): the reference category of each qualitative covariate.",
  avec_covariables = "The detection function `fd` whose scale depends on the covariates `Z` (covariate_matrix, one row per detection): σ = exp(Z β) for each detection, ln σ linear as in ds(formula = ) (Marques and Buckland, 2003). It keeps the distinct combinations of the covariates (the detection function depends on the covariates through them only) and that of each detection, the formula (“OBs + MAS”, for its label) and the names of its coefficients.",
  sans_covariables = "The same detection function without its covariates: that of a combination, its parameters given by combination_theta.",
  theta_combinaison = "The parameters of the key for the combination of covariates `z`: (ln b, ln σ = z'β, aj), those of the detection function without covariates (without_covariates).",
  jacobien_combinaison = "The derivatives of the parameters of a combination (combination_theta) by θ: one row per parameter of the combination (ln b, ln σ, aj), one column per parameter of θ; ln σ = z'β.",
  vraisemblance_covariables = "The log-likelihood of the distances `x` of a detection function with covariates: the sum, over the combinations of covariates, of that of their detections (detection_likelihood, the detection function of the combination: f(x | z) = weight × g̃(x | z) / ∫ weight × g̃(· | z), as mrds's flpt.lnl, or the multinomial distribution of their classes). With `scores`, its derivatives by θ (one row per detection).",
  probabilites_covariables = "The probability of detection of each detection of a detection function with covariates, given its covariates (pi, that of its combination: detection_probability; mrds: predict.ds), and its gradient by θ (one row per detection); and the same by combination.",
  moyenne_detection = "The average probability of detection of a detection function with covariates, as mrds's summary.ds: p̄ = n / N̂, N̂ = Σ 1/pi the Horvitz–Thompson covered abundance (the detections, without the group sizes); its variance (calc.se.Np) combines that of N̂ (the delta method, from `V` the covariance of the parameters, plus Σ (1 − pi) / pi²) and that of the sum of the pi: var(p̄) = p̄² [var(N̂) / N̂² + Σ (1 − pi) / n² − 2 Σ (1 − pi) / pi / (n N̂)]. `pr`: covariate_probabilities.",
  choisir_covariables = "ds()'s choice with covariates: without a number of adjustment terms given, the key and its covariates alone (ds: “Model contains covariate term(s): no adjustment terms will be included”, reason “covariables”); with `n_fixed`, that number of terms; never a monotonicity constraint (ds: “Monotonicity cannot be enforced with covariates”). mrds's start (detection_start), and the fit on the reduced coefficients (covariate_reduction).",
  candidats_covariables = "The models fitted of a detection function with covariates (detection_candidates), preceded, to compare the AICs (as Distance's summarize_ds_models), by the model retained without the covariates (`ch0`, choose_detection with the same settings; NULL if it doesn't converge); the formula of each (empty without covariates), each AIC's difference from the smallest, and the one retained (that with covariates).",
  courbes_covariables = "The same curves of a detection function with covariates `det`, at the same distances: the average detection function, as mrds's plot.ds (the mean of the g(x | zi) of the detections weighted by 1/pi, `pr`: covariate_probabilities), and the density of the observed distances, the mean of the f(x | zi) = weight × g(x | zi) / pi of the detections (the one the histogram of the distances shows). Then, for each qualitative covariate, these means over the detections of each of its categories (plot.ds(subset = )): one column per category, and the number of its detections.",
  adequation_covariables = "The same tests of a detection function with covariates (mrds: gof.ds, cdf.ds): the fitted distribution of each distance, that of its combination of covariates (∫ from L to x over ∫ from L to w, given its covariates), for Cramér–von Mises; the expected counts of each class, the sum over the detections of the share of the class given their covariates.",
  detection_covariables = "g(x | zi) at the distance of each detection of a detection function with covariates, given its covariates (that of its combination, relative to its value at 0).",
  histogramme_echantillonnage = "The histogram of the distances under the detection function (as mrds's plot.ds): √n equal classes; each bar, the count over the one g = 1 would give (lines: Δ n / (p (w − L)); points: Δ(r²) n / (p w²)); and the density of the distances (the count over n Δ), for the curve f. `t`: the data, `p`: the probability of detection.")

# Les commentaires du corps de ces définitions, en anglais (script_moteur.R : commentaires_anglais).
COMMENTAIRES_DISTANCE_EN <- c(
  "# (ds : max_adjustments)" = "# (ds: max_adjustments)",
  "# (mrds : mono.points, les points où la monotonie est contrainte)" = "# (mrds: mono.points, the points where the monotonicity is constrained)",
  "# (la précision relative des intégrales, integrate ; mrds : 1e-7)" = "# (the relative precision of the integrals, integrate; mrds: 1e-7)",
  "# (mrds intègre la grille depuis 0, même tronquée à gauche)" = "# (mrds integrates the grid from 0, even with a left truncation)",
  "# (une matrice d'information presque singulière, que solvecov comble par 1e10 : des distances qui ne" = "# (an almost singular information matrix, which solvecov fills with 1e10: distances that say nothing",
  "# disent rien d'un paramètre, toutes égales par exemple ; un ajustement sans variance, qui ne converge pas)" = "# about a parameter, all equal for example; a fit without variance, which doesn't converge)",
  "# (une taille de données trop petite pour tant de paramètres : la recherche s'arrête là, comme mrds)" = "# (a data size too small for so many parameters: the search stops there, as mrds)",
  "# (une strate sans détection : N = 0, erreur-type 0, ni CV ni ddl, comme dht2)" = "# (a stratum without detection: N = 0, standard error 0, neither CV nor df, as dht2)",
  "# (une grandeur de chaque strate : f de sa ligne)" = "# (a quantity of each stratum: f of its row)",
  "# (les détections de chaque combinaison)" = "# (the detections of each combination)")

# Les définitions de distance.R dont le script de `m` a besoin (et ce qu'elles appellent :
# dependances_moteur) ; un facteur des unités donné tel quel n'a pas besoin de la table des unités ;
# des covariables, leurs fonctions (et choisir_detection pour le modèle sans covariables comparé).
racines_distance <- function(m) {
  covariables <- !is.null(m$detection$fd$formule)
  c("choisir_detection", "candidats_detection", "noms_parametres_detection", "densite_echantillonnage",
    if (is.null(m$reglages_echantillonnage$facteur)) "facteur_echantillonnage", "histogramme_echantillonnage",
    if (covariables) c("matrice_covariables", "avec_covariables", "sans_covariables", "choisir_covariables", "candidats_covariables", "probabilites_covariables",
                       "moyenne_detection", "adequation_covariables", "courbes_covariables", "detection_covariables")
    else c("probabilite_detection", "adequation_detection", "courbe_detection", "detection_distance"))
}

# La partie « modèle » du script d'un échantillonnage par distances (en français ; traduire_script la
# met dans la langue du script) : les lignes du modèle, leurs distances, transects, strates, efforts,
# aires et tailles des groupes, comme donnees_echantillonnage les lit, puis le calcul d'ajuster_distance.
code_calcul_distance <- function(projet, d, m, tab, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  r <- m$reglages_echantillonnage
  t <- m$donnees
  g <- function(x) tx(sprintf("« %s »", texte_sur(x)), sprintf("“%s”", texte_sur(x)))
  col <- function(uid) sprintf("d[[%s]]", code_texte(tab$noms[[as.character(uid)]]))
  point <- t$point
  intervalles <- !is.null(t$coupures)
  avec_strates <- !is.null(r$strate)
  aire <- !t$densite_seule
  groupes <- t$groupes
  # le transect et la strate (une variable ou un identifiant : colonne_trim), en texte, un nombre tel
  # qu'il s'écrit (textes_echantillonnage) ; leur ordre (ordre_echantillonnage) : `u` leurs valeurs
  tr <- colonne_trim(projet, r$echantillon, tab)
  st <- if (avec_strates) colonne_trim(projet, r$strate, tab)
  en_texte <- function(x) if (x$type == "nombre") sprintf("ifelse(is.na(%s), NA_character_, sprintf(\"%%.15g\", %s))", x$code, x$code) else sprintf("as.character(%s)", x$code)
  ordre <- function(x, nom, u, retrait = "") paste0(retrait, switch(x$type,
    qualitative = sprintf("%s <- intersect(levels(%s), %s)", nom, x$code, u),
    nombre = c(sprintf("%s <- unique(%s)", nom, u), sprintf("%s <- %s[order(as.numeric(%s))]", nom, nom, nom)),
    c(sprintf("%s <- unique(%s)", nom, u), sprintf("%s <- %s[order(suppressWarnings(as.numeric(%s)), %s)]", nom, nom, nom, nom))))
  mots_ordre <- function(x) switch(x$type, qualitative = tx("de leurs catégories", "of their categories"), nombre = tx("de leurs nombres", "of their numbers"),
                                   tx("de leurs noms (les nombres d'abord)", "of their names (numbers first)"))
  nombre <- function(x) format(x, digits = 15, decimal.mark = tx(",", "."))
  # les mots de la fonction de détection
  cles <- c(hn = tx("la clé demi-normale exp(−x²/2σ²)", "the half-normal key exp(−x²/2σ²)"), hr = tx("la clé hasard 1 − exp(−(x/σ)^−b)", "the hazard-rate key 1 − exp(−(x/σ)^−b)"),
            unif = tx("la clé uniforme (1)", "the uniform key (1)"))
  series <- c(cos = tx("en cosinus cos(jπu)", "cosine terms cos(jπu)"), herm = tx("en polynômes d'Hermite Hej(u)", "Hermite polynomial terms Hej(u)"),
              poly = tx("en polynômes simples u^j", "simple polynomial terms u^j"))
  monotonies <- c(stricte = tx("sous la contrainte de monotonie stricte de mrds (g décroissante et positive en 10 distances de 0 à w)",
                               "under mrds's strict monotonicity constraint (g decreasing and positive at 10 distances from 0 to w)"),
                  faible = tx("sous la contrainte de monotonie faible de mrds (g sous g(0) et positive en 10 distances de 0 à w)",
                              "under mrds's weak monotonicity constraint (g below g(0) and positive at 10 distances from 0 to w)"),
                  aucune = tx("sans contrainte de monotonie", "without monotonicity constraint"))
  serie <- if (r$serie != "aucune" && !identical(r$ajustements, 0L)) r$serie
  aic <- identical(r$ajustements, "aic") && !is.null(serie)
  u_ajust <- if (identical(r$echelle_ajustements, "echelle") && r$cle != "unif") "x/σ" else "x/w"
  # les covariables de la fonction de détection (ds(formula = )) : leurs noms dans le moteur, leurs colonnes
  cv <- t$covariables
  covariables <- !is.null(cv)
  formule <- m$detection$fd$formule
  p_texte <- if (covariables) "p_moyenne$p" else "pr$p"  # (p, ou p̄ avec des covariables)
  ht <- if (covariables) ", list(p = pr$p, dp = pr$dp, V = det$V)" else ""  # (les pi, leur gradient et la covariance : la densité de Horvitz–Thompson)
  # (avec des covariables, sans nombre de termes donné, le modèle n'en a pas : seul celui sans covariables comparé)
  sans_terme <- covariables && aic
  parametres <- paste(c(if (r$cle == "hr") "ln b", if (r$cle != "unif") (if (covariables) tx("β (les coefficients de ln σ, la constante d'abord)", "β (the coefficients of ln σ, the intercept first)") else "ln σ"),
                        if (!is.null(serie) && !sans_terme) "aj"), collapse = ", ")
  mots_covariables <- if (covariables) vapply(seq_along(cv$noms), function(j) {
    if (cv$qualitatives[j]) sprintf(tx("%s (qualitative, sa référence %s)", "%s (qualitative, its reference %s)"), g(cv$noms[j]), cv$references[[cv$noms[j]]])
    else sprintf(tx("%s (quantitative)", "%s (quantitative)"), g(cv$noms[j]))
  }, "")
  # (le choix de ds() parmi des modèles sans covariables : les arguments de choisir_detection)
  arguments_choix <- paste(c(if (is.null(serie)) "NULL" else code_texte(serie), if (!is.null(serie) && !aic) sprintf("n_fixe = %dL", as.integer(r$ajustements)),
                             if (!is.null(serie)) sprintf("monotonie = %s", code_texte(r$monotonie))), collapse = ", ")
  # les unités
  u <- unites_echantillonnage(m)
  facteur <- if (!is.null(r$facteur)) sprintf("cu <- %s", texte_exact(r$facteur))
             else c(sprintf("reglages <- list(transect = %s, unites = list(distance = %s, effort = %s, aire = %s))", code_texte(r$transect), code_texte(r$unites$distance),
                            code_texte(r$unites$effort), code_texte(r$unites$aire)),
                    "cu <- facteur_echantillonnage(reglages)")
  mots_facteur <- if (!is.null(r$facteur)) tx(sprintf("`cu` : le facteur de conversion donné (%s), qui met la troncature w dans l'unité des aires.", nombre(r$facteur)),
                                              sprintf("`cu`: the conversion factor given (%s), which puts the truncation w in the unit of the areas.", nombre(r$facteur)))
                  else if (point) tx(sprintf("`cu` met les distances (%s) dans l'unité des aires (%s), comme convert_units de Distance.", u$distance, u$aire),
                                     sprintf("`cu` puts the distances (%s) in the unit of the areas (%s), as Distance's convert_units.", u$distance, u$aire))
                  else tx(sprintf("`cu` met les distances (%s) et l'effort (%s) dans l'unité des aires (%s), comme convert_units de Distance.", u$distance, u$effort, u$aire),
                          sprintf("`cu` puts the distances (%s) and the effort (%s) in the unit of the areas (%s), as Distance's convert_units.", u$distance, u$effort, u$aire))
  colonnes <- c(sprintf(tx("la distance (%s)", "the distance (%s)"), g(t$noms$y)), sprintf(tx("le transect (%s)", "the transect (%s)"), g(t$noms$transect)),
                if (avec_strates) sprintf(tx("la strate (%s)", "the stratum (%s)"), g(t$noms$strate)),
                if (!is.null(r$effort)) sprintf(if (point) tx("le nombre de passages au point (%s)", "the number of visits to the point (%s)")
                                                else tx("l'effort du transect, la longueur de la ligne (%s)", "the transect's effort, the length of the line (%s)"), g(t$noms$effort)),
                if (aire) sprintf(tx("l'aire de la strate (%s)", "the stratum's area (%s)"), g(t$noms$aire)),
                if (groupes) sprintf(tx("la taille du groupe (%s)", "the group size (%s)"), g(t$noms$taille)))
  troncature <- if (intervalles) {
    tx(sprintf("Les distances relevées par %d intervalles, comme ds(cutpoints = ) : leurs bornes, la première la troncature à gauche L, la dernière la troncature w ; chaque distance compte dans sa classe.", length(t$coupures) - 1),
       sprintf("The distances recorded in %d intervals, as ds(cutpoints = ): their bounds, the first the left truncation L, the last the truncation w; each distance counts in its class.", length(t$coupures) - 1))
  } else {
    droite <- switch(r$troncature$type,
      aucune = tx("la plus grande distance, comme ds par défaut", "the largest distance, as ds by default"),
      distance = tx(sprintf("%s %s", nombre(t$w), u$distance), sprintf("%s %s", nombre(t$w), u$distance)),
      pourcentage = tx(sprintf("les %s %% des plus grandes distances laissées de côté (leur quantile à %s %%, comme ds)", nombre(r$troncature$valeur), nombre(100 - r$troncature$valeur)),
                       sprintf("the %s%% largest distances left out (their %s%% quantile, as ds)", nombre(r$troncature$valeur), nombre(100 - r$troncature$valeur))))
    tx(sprintf("La troncature w : %s ; %s.", droite, if (t$L > 0) sprintf("à gauche, L = %s %s", nombre(t$L), u$distance) else "sans troncature à gauche (L = 0)"),
       sprintf("The truncation w: %s; %s.", droite, if (t$L > 0) sprintf("on the left, L = %s %s", nombre(t$L), u$distance) else "no left truncation (L = 0)"))
  }
  couverte <- if (point) tx("π w² K, K les passages du point", "π w² K, K the point's visits") else if (t$L > 0) tx("2 (w − L) L, L l'effort", "2 (w − L) L, L the effort")
              else tx("2 w L, L l'effort", "2 w L, L the effort")
  plusieurs <- nrow(t$strates) > 1
  c(code_lignes_definition(projet, d, tab, tx), "",
    commentaire(tx(sprintf("%s de chaque ligne%s. Le transect%s en texte (un nombre tel qu'il s'écrit). Un transect sans détection garde une ligne, sa distance vide, pour compter son effort ; une ligne sans transect ni distance (une ligne vide du tableau) est laissée de côté.",
                           paste0(toupper(substr(liste_textes(colonnes, "et"), 1, 1)), substring(liste_textes(colonnes, "et"), 2)),
                           if (is.null(r$effort)) " (des points sans effort donné : un passage par point)" else "", if (avec_strates) " et la strate" else ""),
                   sprintf("%s of each row%s. The transect%s as text (a number as it is written). A transect without detection keeps a row, its distance empty, so that its effort counts; a row without a transect or a distance (an empty row of the table) is left out.",
                           paste0(toupper(substr(liste_textes(colonnes, "and"), 1, 1)), substring(liste_textes(colonnes, "and"), 2)),
                           if (is.null(r$effort)) " (points without an effort given: one visit per point)" else "", if (avec_strates) " and the stratum" else ""))),
    sprintf("distance <- %s", col(as.numeric(d$y[[1]]$variables[1]))),
    sprintf("transect <- %s", en_texte(tr)),
    if (avec_strates) sprintf("strate <- %s", en_texte(st)) else "strate <- rep(\"\", nrow(d))",
    if (!is.null(r$effort)) sprintf("effort <- %s", col(r$effort)) else "effort <- rep(1, nrow(d))",
    if (aire) sprintf("aire <- %s", col(r$aire)),
    if (groupes) sprintf("taille <- %s", col(r$taille)),
    "garde <- !(is.na(transect) & is.na(distance))",
    "",
    commentaire(c(troncature, tx("Les détections hors de [L, w] sortent du modèle ; leur transect reste.", "The detections outside [L, w] leave the model; their transect stays."))),
    if (intervalles) c(sprintf("coupures <- %s", code_nombres(t$coupures)), "L <- coupures[1]", "w <- coupures[length(coupures)]")
    else c(if (r$troncature$type != "distance") "xs <- distance[garde & !is.na(distance)]",
           switch(r$troncature$type, aucune = "w <- max(xs)", distance = sprintf("w <- %s", texte_exact(r$troncature$valeur)),
                  pourcentage = sprintf("w <- unname(stats::quantile(xs, 1 - %s / 100))", texte_exact(r$troncature$valeur))),
           sprintf("L <- %s", texte_exact(r$gauche))),
    "dans <- garde & !is.na(distance) & distance >= L & distance <= w",
    if (covariables) c("",
      commentaire(tx(sprintf("Les covariables de l'échelle de la fonction de détection, comme ds(formula = ~%s), pour les détections dans la troncature : %s. Leur matrice, comme model.matrix : la constante, puis chaque covariable, une quantitative telle quelle, une qualitative par une indicatrice de chacune de ses catégories présentes sauf la première, sa référence.",
                             formule, liste_textes(mots_covariables, "et")),
                     sprintf("The covariates of the scale of the detection function, as ds(formula = ~%s), for the detections within the truncation: %s. Their matrix, as model.matrix: the intercept, then each covariate, a quantitative one as it is, a qualitative one by an indicator of each of its categories present but the first, its reference.",
                             formule, liste_textes(mots_covariables, "and")))),
      code_valeurs(sprintf("%s = %s[dans]", vapply(cv$noms, function(n) deparse(as.name(n), backtick = TRUE), ""), vapply(r$covariables, col, "")), "covariables <- list(", ")", ouvrir = ""),
      "Z <- matrice_covariables(covariables)"),
    "",
    commentaire(c(if (avec_strates) tx(sprintf("Les strates, dans l'ordre %s, et les transects de chacune, dans l'ordre %s : l'effort de chaque transect, ses détections et %s dans la troncature ; %s.",
                                               mots_ordre(st), mots_ordre(tr), if (groupes) "ses individus (la somme des tailles de ses groupes)" else "ses individus (ses détections)",
                                               if (aire) "l'aire de chaque strate" else "sans aire, la densité seulement"),
                                       sprintf("The strata, in the order %s, and the transects of each, in the order %s: the effort of each transect, its detections and %s within the truncation; %s.",
                                               mots_ordre(st), mots_ordre(tr), if (groupes) "its individuals (the sum of its groups' sizes)" else "its individuals (its detections)",
                                               if (aire) "the area of each stratum" else "without areas, the density only"))
                  else tx(sprintf("Les transects, dans l'ordre %s : l'effort de chacun, ses détections et %s dans la troncature ; %s.", mots_ordre(tr),
                                  if (groupes) "ses individus (la somme des tailles de ses groupes)" else "ses individus (ses détections)",
                                  if (aire) "l'aire de la région" else "sans aire, la densité seulement"),
                          sprintf("The transects, in the order %s: the effort of each, its detections and %s within the truncation; %s.", mots_ordre(tr),
                                  if (groupes) "its individuals (the sum of its groups' sizes)" else "its individuals (its detections)",
                                  if (aire) "the area of the region" else "without an area, the density only")),
                  if (!is.null(r$aire) && !aire) tx("(Les aires sont toutes nulles : la densité seulement, comme dht2.)", "(The areas are all zero: the density only, as dht2.)"))),
    if (avec_strates) ordre(st, "strates", "strate[garde]") else "strates <- \"\"",
    "transects <- NULL",
    "for (s in strates) {",
    ordre(tr, "ts", "transect[garde & strate == s]", "  "),
    "  for (e in ts) {",
    "    i <- garde & strate == s & transect == e",
    sprintf("    transects <- rbind(transects, data.frame(strate = s, transect = e, effort = effort[i][1], n = sum(dans & i), individus = %s))",
            if (groupes) "sum(taille[dans & i])" else "sum(dans & i)"),
    "  }",
    "}",
    if (aire) "aires <- data.frame(strate = strates, aire = vapply(strates, function(s) aire[garde & strate == s][1], 0, USE.NAMES = FALSE))"
    else "aires <- data.frame(strate = strates, aire = NA_real_)",
    commentaire(tx(sprintf("Les données du calcul, comme le moteur les range : les distances dans la troncature, leur observation, leur strate%s%s, les troncatures, des %s, %sla table des transects et celle des strates.",
                           if (groupes) " et la taille de leur groupe" else "", if (covariables) ", leurs covariables" else "", if (point) "points (point = TRUE)" else "lignes (point = FALSE)",
                           if (intervalles) "les bornes des intervalles, " else ""),
                   sprintf("The data of the computation, as the engine arranges them: the distances within the truncation, their observation, their stratum%s%s, the truncations, %s, %sthe table of the transects and that of the strata.",
                           if (groupes) " and their group size" else "", if (covariables) ", their covariates" else "", if (point) "points (point = TRUE)" else "lines (point = FALSE)",
                           if (intervalles) "the bounds of the intervals, " else ""))),
    code_valeurs(c("x = distance[dans]", "uid = d[[\"Observation\"]][dans]", "strate_x = strate[dans]", if (groupes) "taille_x = taille[dans]",
                   if (covariables) "covariables = list(valeurs = covariables)", "w = w", "L = L",
                   sprintf("point = %s", if (point) "TRUE" else "FALSE"), if (intervalles) "coupures = coupures", "transects = transects", "strates = aires",
                   sprintf("densite_seule = %s", if (aire) "FALSE" else "TRUE"), sprintf("groupes = %s", if (groupes) "TRUE" else "FALSE"), "n = sum(dans)"),
                 "releve <- list(", ")", ouvrir = ""),
    "",
    commentaire(c(tx(sprintf("La fonction de détection g(x), la probabilité de détecter un animal à la distance x (1 en 0) : %s%s, rapportée à sa valeur en 0. %s",
                                     cles[[r$cle]], if (is.null(serie)) "" else if (sans_terme) sprintf(" (le modèle sans covariables comparé : multipliée par 1 + Σ aj fj(u), des termes d'ajustement %s, u = %s)", series[[serie]], u_ajust)
                                                    else sprintf(", multipliée par 1 + Σ aj fj(u), des termes d'ajustement %s, u = %s", series[[serie]], u_ajust),
                                     if (!nzchar(parametres)) "La clé uniforme seule n'a pas de paramètre : g vaut 1 de L à w."
                                     else sprintf("Ses paramètres (%s, dans cet ordre, comme mrds) sont ceux du maximum de vraisemblance des distances entre L et w%s.", parametres,
                                                  if (intervalles) " (la vraisemblance multinomiale des comptes des classes)" else "")),
                     sprintf("The detection function g(x), the probability of detecting an animal at distance x (1 at 0): %s%s, relative to its value at 0. %s",
                             cles[[r$cle]], if (is.null(serie)) "" else if (sans_terme) sprintf(" (the model without covariates compared: multiplied by 1 + Σ aj fj(u), %s, u = %s)", series[[serie]], u_ajust)
                                            else sprintf(", multiplied by 1 + Σ aj fj(u), %s, u = %s", series[[serie]], u_ajust),
                             if (!nzchar(parametres)) "The uniform key alone has no parameter: g is 1 from L to w."
                             else sprintf("Its parameters (%s, in this order, as mrds) are those of the maximum likelihood of the distances between L and w%s.", parametres,
                                          if (intervalles) " (the multinomial likelihood of the classes' counts)" else ""))),
                  if (covariables) tx(sprintf("Son échelle σ dépend des covariables, ln σ = Z β pour chaque détection (Marques et Buckland, 2003), comme ds(formula = ~%s) : chaque détection a la fonction de détection de sa combinaison de covariables. L'ajustement se fait sur les coefficients des covariables quantitatives centrées et réduites, rendus ensuite dans leurs unités.", formule),
                                      sprintf("Its scale σ depends on the covariates, ln σ = Z β for each detection (Marques and Buckland, 2003), as ds(formula = ~%s): each detection has the detection function of its combination of covariates. The fit is made on the coefficients of the quantitative covariates centred and scaled, then returned in their units.", formule)),
                  if (covariables && aic) tx(sprintf("Avec des covariables, ds n'ajoute pas de terme d'ajustement : la clé et ses covariables seules, sans contrainte de monotonie. Pour comparer les AIC, le même modèle sans covariables, choisi comme ds le fait : la clé seule, puis un terme, deux… tant que l'AIC baisse (au plus %d), dans l'ordre de Distance (ordres %s…), %s.",
                                                     DISTANCE_AJUSTEMENTS_MAX, paste(ordres_ajustement(3, r$cle, serie), collapse = ", "), monotonies[[r$monotonie]]),
                                             sprintf("With covariates, ds adds no adjustment term: the key and its covariates alone, without monotonicity constraint. To compare the AICs, the same model without covariates, chosen as ds does: the key alone, then one term, two… while the AIC falls (at most %d), in Distance's order (orders %s…), %s.",
                                                     DISTANCE_AJUSTEMENTS_MAX, paste(ordres_ajustement(3, r$cle, serie), collapse = ", "), monotonies[[r$monotonie]]))
                  else if (covariables && !is.null(serie)) tx(sprintf("Ici %d terme%s (d'ordre%s %s), sans contrainte de monotonie (ds n'en met pas avec des covariables). Pour comparer les AIC, le même modèle sans covariables, %s.",
                                                                      r$ajustements, if (r$ajustements > 1) "s" else "", if (r$ajustements > 1) "s" else "",
                                                                      liste_textes(ordres_ajustement(r$ajustements, r$cle, serie), "et"), monotonies[[r$monotonie]]),
                                                              sprintf("Here %d term%s (of order%s %s), without monotonicity constraint (ds sets none with covariates). To compare the AICs, the same model without covariates, %s.",
                                                                      r$ajustements, if (r$ajustements > 1) "s" else "", if (r$ajustements > 1) "s" else "",
                                                                      liste_textes(ordres_ajustement(r$ajustements, r$cle, serie), "and"), monotonies[[r$monotonie]]))
                  else if (covariables) tx("Sans terme d'ajustement ; pour comparer les AIC, la clé seule, sans covariables.", "Without adjustment terms; to compare the AICs, the key alone, without covariates.")
                  else if (aic) tx(sprintf("Le nombre de termes est choisi comme ds() le fait : la clé seule, puis un terme, deux… tant que l'AIC baisse (au plus %d), dans l'ordre de Distance (ordres %s…), %s.",
                                      DISTANCE_AJUSTEMENTS_MAX, paste(ordres_ajustement(3, r$cle, serie), collapse = ", "), monotonies[[r$monotonie]]),
                              sprintf("The number of terms is chosen as ds() does: the key alone, then one term, two… while the AIC falls (at most %d), in Distance's order (orders %s…), %s.",
                                      DISTANCE_AJUSTEMENTS_MAX, paste(ordres_ajustement(3, r$cle, serie), collapse = ", "), monotonies[[r$monotonie]]))
                  else if (!is.null(serie)) tx(sprintf("Ici %d terme%s (d'ordre%s %s), %s.", r$ajustements, if (r$ajustements > 1) "s" else "", if (r$ajustements > 1) "s" else "",
                                                       liste_textes(ordres_ajustement(r$ajustements, r$cle, serie), "et"), monotonies[[r$monotonie]]),
                                               sprintf("Here %d term%s (of order%s %s), %s.", r$ajustements, if (r$ajustements > 1) "s" else "", if (r$ajustements > 1) "s" else "",
                                                       liste_textes(ordres_ajustement(r$ajustements, r$cle, serie), "and"), monotonies[[r$monotonie]]))
                  else tx("Sans terme d'ajustement.", "Without adjustment terms."),
                  if (!is.null(serie)) tx("Les réglages gardent leurs noms du moteur : la monotonie « stricte », « faible » ou « aucune » ; les termes divisés par w, « largeur », ou par σ, « echelle ».",
                                          "The settings keep their names in the engine: the monotonicity \"stricte\" (strict), \"faible\" (weak) or \"aucune\" (none); the terms divided by w, \"largeur\", or by σ, \"echelle\"."))),
    code_valeurs(c(sprintf("cle = %s", code_texte(r$cle)), "w = releve$w", "L = releve$L", "point = releve$point",
                   if (!is.null(serie)) sprintf("echelle_ajustements = %s", code_texte(r$echelle_ajustements)), "ordres = integer(0)", if (intervalles) "coupures = releve$coupures"),
                 "base <- list(", ")", ouvrir = ""),
    if (covariables) c(
      "base <- avec_covariables(base, Z)",
      sprintf("choix <- choisir_covariables(base, releve$x, %s)", paste(c(if (is.null(serie)) "NULL" else code_texte(serie), if (!is.null(serie) && !aic) sprintf("n_fixe = %dL", as.integer(r$ajustements))),
                                                                       collapse = ", ")),
      commentaire(tx("(le même modèle sans covariables, pour comparer les AIC ; laissé de côté s'il ne converge pas)", "(the same model without covariates, to compare the AICs; left out if it doesn't converge)")),
      sprintf("choix_sans <- tryCatch(choisir_detection(sans_covariables(base), releve$x, %s), error = function(e) NULL)", arguments_choix),
      "if (!isTRUE(choix_sans$choisi$converge)) choix_sans <- NULL")
    else sprintf("choix <- choisir_detection(base, releve$x, %s)", arguments_choix),
    "det <- choix$choisi",
    if (covariables) "candidats <- candidats_covariables(choix, choix_sans)" else "candidats <- candidats_detection(choix)",
    if (aic) c(
      sprintf("raisons <- c(\"aic\" = %s, \"max\" = %s,", code_texte(tx("Un terme de plus faisait monter l'AIC : la recherche s'est arrêtée là.", "One more term raised the AIC: the search stopped there.")),
              code_texte(sprintf(tx("Le maximum de %d termes est atteint.", "The maximum of %d terms was reached."), DISTANCE_AJUSTEMENTS_MAX))),
      sprintf("             \"convergence\" = %s,", code_texte(tx("Le modèle suivant ne converge pas : la recherche s'est arrêtée là.", "The next model doesn't converge: the search stopped there."))),
      sprintf("             \"parametres\" = %s,", code_texte(tx("Trop peu de distances distinctes (ou de classes) pour un terme de plus : la recherche s'est arrêtée là.",
                                                               "Too few distinct distances (or classes) for one more term: the search stopped there."))),
      if (covariables) sprintf("             \"covariables\" = %s,", code_texte(tx("Avec des covariables, ds n'ajoute pas de terme d'ajustement : la clé et ses covariables seules.",
                                                                                   "With covariates, ds adds no adjustment term: the key and its covariates alone."))),
      sprintf("             \"echec\" = %s)", code_texte(tx("La clé seule ne converge pas.", "The key alone doesn't converge."))),
      "raison_choix <- unname(raisons[choix$raison])"),
    "parametres <- if (det$q) data.frame(parametre = noms_parametres_detection(det$fd), estimation = det$theta, se = det$se)",
    "covariance <- matrix(det$V, det$q, det$q, dimnames = list(noms_parametres_detection(det$fd), noms_parametres_detection(det$fd)))",
    "",
    if (covariables) commentaire(tx(sprintf("La probabilité de détection dans la troncature de chaque détection, sachant ses covariables (pi, celle de sa combinaison), et son gradient par les paramètres ; leur moyenne p̄ = n / Σ 1/pi et sa variance, comme summary.ds de mrds (celle de Σ 1/pi par la méthode delta, de la covariance des paramètres, l'inverse de la somme des produits des scores des distances, plus Σ (1 − pi) / pi², et celle de la somme des pi). Puis %s, et l'effectif dans l'aire couverte, %s (Horvitz–Thompson).",
                                            if (point) "le rayon effectif de détection EDR = w √p̄ (son CV, la moitié de celui de p̄)" else "la demi-largeur effective de bande ESW = p̄ (w − L)",
                                            if (groupes) "Σ si / pi, si la taille du groupe" else "Σ 1 / pi"),
                                    sprintf("The probability of detection within the truncation of each detection, given its covariates (pi, that of its combination), and its gradient by the parameters; their mean p̄ = n / Σ 1/pi and its variance, as mrds's summary.ds (that of Σ 1/pi by the delta method, from the covariance of the parameters, the inverse of the sum of the outer products of the distances' scores, plus Σ (1 − pi) / pi², and that of the sum of the pi). Then %s, and the abundance in the covered area, %s (Horvitz–Thompson).",
                                            if (point) "the effective detection radius EDR = w √p̄ (its CV, half that of p̄)" else "the effective strip half-width ESW = p̄ (w − L)",
                                            if (groupes) "Σ si / pi, si the group size" else "Σ 1 / pi")))
    else commentaire(tx(sprintf("p, la probabilité moyenne de détection dans la troncature (l'intégrale de g de L à w, pondérée comme les distances %s), et sa variance par la méthode delta : son gradient par les paramètres et leur covariance, l'inverse de la somme des produits des scores des distances (comme mrds). Puis %s, et l'effectif dans l'aire couverte, %s / p.",
                           if (point) "à un point" else "à une ligne", if (point) "le rayon effectif de détection EDR = w √p (son CV, la moitié de celui de p)" else "la demi-largeur effective de bande ESW = p (w − L)",
                           if (groupes) "la somme des tailles des groupes" else "n"),
                   sprintf("p, the average probability of detection within the truncation (the integral of g from L to w, weighted as the distances %s), and its variance by the delta method: its gradient by the parameters and their covariance, the inverse of the sum of the outer products of the distances' scores (as mrds). Then %s, and the abundance in the covered area, %s / p.",
                           if (point) "from a point" else "from a line", if (point) "the effective detection radius EDR = w √p (its CV, half that of p)" else "the effective strip half-width ESW = p (w − L)",
                           if (groupes) "the sum of the group sizes" else "n"))),
    if (covariables) c("pr <- probabilites_covariables(det$fd, det$theta)",
                       "p_moyenne <- moyenne_detection(pr, det$V)",
                       "var_p <- p_moyenne$var_p",
                       "se_p <- p_moyenne$se_p",
                       if (groupes) "Nc <- sum(releve$taille_x / pr$p)" else "Nc <- sum(1 / pr$p)")
    else c("pr <- probabilite_detection(det$fd, det$theta)",
           "var_p <- if (det$q) drop(t(pr$dp) %*% det$V %*% pr$dp) else 0",
           "se_p <- sqrt(max(0, var_p))",
           if (groupes) "Nc <- sum(releve$taille_x) / pr$p" else "Nc <- releve$n / pr$p"),
    if (point) c(sprintf("edr <- releve$w * sqrt(%s)", p_texte),
                 sprintf("detection <- data.frame(statistique = c(\"p\", \"EDR\", %s), estimation = c(%s, edr, Nc), se = c(se_p, edr * se_p / %s / 2, NA))", code_texte(tx("N couverts", "N covered")), p_texte, p_texte))
    else c("largeur <- releve$w - releve$L",
           sprintf("detection <- data.frame(statistique = c(\"p\", \"ESW\", %s), estimation = c(%s, %s * largeur, Nc), se = c(se_p, se_p * largeur, NA))", code_texte(tx("N couverts", "N covered")), p_texte, p_texte)),
    "detection$cv <- detection$se / detection$estimation",
    "",
    commentaire(if (intervalles) tx(sprintf("L'adéquation : le khi² sur les classes des intervalles (ddl = classes − 1 − paramètres ; sans distances exactes, ni Cramér–von Mises ni diagramme quantile-quantile)%s.",
                                            if (covariables) " ; les comptes attendus de chaque classe, la somme sur les détections de sa part sachant leurs covariables" else ""),
                                    sprintf("The goodness of fit: the chi-square on the classes of the intervals (df = classes − 1 − parameters; without exact distances, neither Cramér–von Mises nor QQ plot)%s.",
                                            if (covariables) "; the expected counts of each class, the sum over the detections of its share given their covariates" else ""))
                else if (covariables) tx("L'adéquation, comme ddf.gof de mrds, chaque distance rapportée à la fonction de détection de ses covariables : Cramér–von Mises sur la répartition ajustée de chaque distance sachant ses covariables, le khi² sur √n classes égales de L à w (fermées à droite ; les comptes attendus, la somme sur les détections de la part de la classe sachant leurs covariables ; ddl = classes − 1 − paramètres) ; ces répartitions et leur répartition empirique, pour le diagramme quantile-quantile.",
                                         "The goodness of fit, as mrds's ddf.gof, each distance against the detection function of its covariates: Cramér–von Mises on the fitted distribution of each distance given its covariates, the chi-square on √n equal classes from L to w (right-closed; the expected counts, the sum over the detections of the class's share given their covariates; df = classes − 1 − parameters); these distributions and their empirical distribution, for the QQ plot.")
                else tx("L'adéquation, comme ddf.gof de mrds : Cramér–von Mises sur les distances exactes, le khi² sur √n classes égales de L à w (fermées à droite ; ddl = classes − 1 − paramètres) ; la répartition ajustée de chaque distance et sa répartition empirique, pour le diagramme quantile-quantile.",
                        "The goodness of fit, as mrds's ddf.gof: Cramér–von Mises on the exact distances, the chi-square on √n equal classes from L to w (right-closed; df = classes − 1 − parameters); the fitted distribution of each distance and its empirical distribution, for the QQ plot.")),
    if (covariables) "adequation <- adequation_covariables(det, releve$x, releve$uid)" else "adequation <- adequation_detection(det, releve$x, releve$uid)",
    if (identical(langue, "en")) "names(adequation$khi2$classes)[2] <- \"to\"",
    if (intervalles) sprintf("tests <- data.frame(test = %s, statistique = adequation$khi2$chi2, ddl = adequation$khi2$ddl, p = adequation$khi2$p)", code_texte(tx("khi²", "chi-square")))
    else c(sprintf("tests <- data.frame(test = c(\"Cramér–von Mises\", %s), statistique = c(adequation$cvm$W, adequation$khi2$chi2), ddl = c(NA, adequation$khi2$ddl),", code_texte(tx("khi²", "chi-square"))),
           "                    p = c(adequation$cvm$p, adequation$khi2$p))"),
    "",
    commentaire(c(tx(sprintf("La densité%s par strate%s, comme dht2 du paquet Distance : D = %s, a l'aire couverte (%s)%s ; sa variance réunit celle du taux de rencontre n / L entre les transects (l'estimateur %s de Fewster et al., 2009), celle de %s (méthode delta)%s ; les degrés de liberté de Satterthwaite et l'intervalle log-normal à 95 %%.%s",
                             if (aire) " et l'effectif" else "", if (plusieurs) " et au total" else "",
                             if (!covariables) "n / (a p)" else if (groupes) "Σ si / pi / a, la somme sur ses détections (Horvitz–Thompson)" else "Σ 1 / pi / a, la somme sur ses détections (Horvitz–Thompson)",
                             couverte, if (aire) ", N = A D, A l'aire de la strate" else "",
                             if (point) "P2" else "R2", if (covariables) "la fonction de détection (∇N' V ∇N, ∇N le gradient de N par ses paramètres)" else "p",
                             if (groupes) " et celle de la taille moyenne des groupes (D compte les individus)" else "",
                             if (!plusieurs) "" else if (aire) " Le total : la somme des effectifs des strates, sur la somme de leurs aires." else " Le total : les densités des strates pondérées par leurs aires couvertes."),
                     sprintf("The density%s by stratum%s, as dht2 of the Distance package: D = %s, a the covered area (%s)%s; its variance combines that of the encounter rate n / L between transects (the %s estimator of Fewster et al., 2009), that of %s (delta method)%s; Satterthwaite degrees of freedom and the log-normal 95%% interval.%s",
                             if (aire) " and abundance" else "", if (plusieurs) " and overall" else "",
                             if (!covariables) "n / (a p)" else if (groupes) "Σ si / pi / a, the sum over its detections (Horvitz–Thompson)" else "Σ 1 / pi / a, the sum over its detections (Horvitz–Thompson)",
                             couverte, if (aire) ", N = A D, A the stratum's area" else "",
                             if (point) "P2" else "R2", if (covariables) "the detection function (∇N' V ∇N, ∇N the gradient of N by its parameters)" else "p",
                             if (groupes) " and that of the mean group size (D counts individuals)" else "",
                             if (!plusieurs) "" else if (aire) " The total: the sum of the strata's abundances, over the sum of their areas." else " The total: the strata's densities weighted by their covered areas.")),
                  mots_facteur,
                  if (is.null(r$facteur)) tx(sprintf("Le transect garde son nom du moteur : « %s » (%s).", r$transect, if (point) "des points" else "des lignes"),
                                             sprintf("The transect keeps its name in the engine: \"%s\" (%s).", r$transect, if (point) "points" else "lines")))),
    facteur,
    sprintf("densite <- densite_echantillonnage(releve, %s, var_p, det$q, cu%s)", p_texte, ht),
    if (groupes) c(
      commentaire(tx("La densité des groupes, chaque détection comptée une fois (dht2 : son attribut « grouped »).", "The density of groups, each detection counted once (dht2: its \"grouped\" attribute).")),
      "releve_groupes <- releve",
      "releve_groupes$groupes <- FALSE",
      "releve_groupes$transects$individus <- releve_groupes$transects$n",
      sprintf("densite_groupes <- densite_echantillonnage(releve_groupes, %s, var_p, det$q, cu%s)", p_texte, ht)),
    "",
    if (covariables) commentaire(tx(sprintf("Pour les graphiques : sur 101 distances de L à w, la fonction de détection moyenne, celle de chaque détection (sachant ses covariables) pondérée par 1/pi comme plot.ds de mrds, et la densité des distances observées, la moyenne de celles des détections%s ; g à la distance de chaque détection, sachant ses covariables ; l'histogramme des distances (%s), chaque barre son compte sur celui qu'elle aurait si tous les animaux étaient détectés (comme plot.ds).",
                                            if (any(cv$qualitatives)) " ; les mêmes pour les détections de chaque catégorie de chaque covariable qualitative" else "", if (intervalles) "les intervalles" else "√n classes égales de L à w"),
                                    sprintf("For the plots: at 101 distances from L to w, the average detection function, that of each detection (given its covariates) weighted by 1/pi as mrds's plot.ds, and the density of the observed distances, the mean of those of the detections%s; g at the distance of each detection, given its covariates; the histogram of the distances (%s), each bar its count over the one it would have if every animal were detected (as plot.ds).",
                                            if (any(cv$qualitatives)) "; the same for the detections of each category of each qualitative covariate" else "", if (intervalles) "the intervals" else "√n equal classes from L to w")))
    else commentaire(tx(sprintf("Pour les graphiques : la fonction de détection g sur 101 distances de L à w, %s ; g à la distance de chaque détection ; l'histogramme des distances (%s), chaque barre son compte sur celui qu'elle aurait si tous les animaux étaient détectés (comme plot.ds de mrds).",
                           if (point) "et la densité des distances f(r) = 2r g(r) / (w² p)" else "et la densité des distances g / ((w − L) p)",
                           if (intervalles) "les intervalles" else "√n classes égales de L à w"),
                   sprintf("For the plots: the detection function g at 101 distances from L to w, %s; g at the distance of each detection; the histogram of the distances (%s), each bar its count over the one it would have if every animal were detected (as mrds's plot.ds).",
                           if (point) "and the density of the distances f(r) = 2r g(r) / (w² p)" else "and the density of the distances g / ((w − L) p)",
                           if (intervalles) "the intervals" else "√n equal classes from L to w"))),
    if (covariables) c("courbes <- courbes_covariables(det, releve, pr)",
                       "courbe <- courbes$courbe",
                       "g_x <- detection_covariables(det$fd, det$theta, releve$x)")
    else c("courbe <- courbe_detection(det, releve, pr$p)",
           "ex <- detection_distance(det$fd, det$theta, releve$x)",
           "g_x <- ex$gt / (1 + ex$s0)"),
    sprintf("histogramme <- histogramme_echantillonnage(releve, %s)", p_texte),
    if (identical(langue, "en")) "names(histogramme)[2] <- \"to\"")
}

# Les colonnes d'un tableau du script qu'une ligne des résultats affiche, dans sa langue.
colonnes_distance <- function(objet, x, langue) sprintf("%s[%s]", objet, code_textes(nom_traduit(x, langue, NOMS_DISTANCE_EN)))

code_sortie_distance <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  noms <- c(NOMS_DISTANCE_EN, NOMS_SCRIPT_EN)
  ligne <- function(code, fr, en) ligne_sortie(traduire_script(code, langue, noms), tx(fr, en))
  r <- m$reglages_echantillonnage
  t <- m$donnees
  u <- unites_echantillonnage(m)
  st <- if (!is.null(r$strate)) "strate"
  aic <- identical(r$ajustements, "aic") && r$serie != "aucune" && !identical(r$ajustements, 0L)
  par_aire <- if (nzchar(u$aire)) sprintf(tx("par %s", "per %s"), u$aire) else tx("par unité d'aire", "per unit of area")
  covariables <- !is.null(t$covariables)
  c(ligne(sprintf("print(%s)", colonnes_distance("candidats", c("modele", if (covariables) "covariables", "parametres", "logL", "AIC", "delta_AIC", "contraint", "choisi"), langue)),
          if (covariables) "les fonctions de détection ajustées (et la même sans covariables) et leur AIC ; la retenue" else "les fonctions de détection ajustées et leur AIC ; la retenue",
          if (covariables) "the detection functions fitted (and the same without covariates) and their AIC; the one retained" else "the detection functions fitted and their AIC; the one retained"),
    if (aic) ligne("print(raison_choix)", "pourquoi le choix s'est arrêté là", "why the choice stopped there"),
    if (covariables && any(t$covariables$qualitatives)) ligne("print(unlist(attr(Z, \"references\")))", "la catégorie de référence de chaque covariable qualitative",
                                                              "the reference category of each qualitative covariate"),
    if (!is.null(m$parametres)) ligne("print(parametres)", if (covariables) "ses paramètres (les coefficients des covariables sur ln σ) et leurs erreurs-types" else "ses paramètres et leurs erreurs-types",
                                      if (covariables) "its parameters (the coefficients of the covariates on ln σ) and their standard errors" else "its parameters and their standard errors"),
    if (!is.null(m$parametres)) ligne("print(covariance)", "leur covariance", "their covariance"),
    if (covariables) ligne("print(summary(pr$p))", "la probabilité de détection des détections, sachant leurs covariables", "the probability of detection of the detections, given their covariates"),
    ligne("print(detection)", sprintf("%s, %s et l'effectif dans l'aire couverte ; erreurs-types et CV", if (covariables) "p̄" else "p", m$effectif$quoi),
          sprintf("%s, %s and the abundance in the covered area; standard errors and CVs", if (covariables) "p̄" else "p", m$effectif$quoi)),
    ligne("print(tests)", "l'adéquation de la fonction de détection", "the goodness of fit of the detection function"),
    ligne("print(adequation$khi2$classes)", "les classes du khi² : comptes observés et attendus", "the chi-square's classes: observed and expected counts"),
    ligne(sprintf("print(%s)", colonnes_distance("densite", c(st, "k", "effort", "n", "rencontre", "cv_rencontre", "D", "se_D", "cv", "bas_D", "haut_D", "ddl"), langue)),
          sprintf("la densité %s, son erreur-type, son CV, son intervalle à 95 %% et ses ddl", par_aire), sprintf("the density %s, its standard error, CV, 95%% interval and df", par_aire)),
    if (!t$densite_seule) ligne(sprintf("print(%s)", colonnes_distance("densite", c(st, "aire", "N", "se_N", "bas_N", "haut_N"), langue)),
                                "l'effectif, son erreur-type et son intervalle à 95 %", "the abundance, its standard error and 95% interval"),
    ligne(sprintf("print(%s)", colonnes_distance("densite", c(st, "part_rencontre", "part_detection", if (t$groupes) "part_groupes"), langue)),
          sprintf("la part de la variance due au taux de rencontre, %s (%%)", if (t$groupes) "à la détection et à la taille des groupes" else "à la détection"),
          sprintf("the share of the variance due to the encounter rate, %s (%%)", if (t$groupes) "to detection and to group size" else "to detection")),
    if (t$groupes) ligne(sprintf("print(%s)", colonnes_distance("densite", c(st, "taille_moyenne", "se_taille"), langue)), "la taille moyenne des groupes", "the mean group size"),
    if (t$groupes) ligne(sprintf("print(%s)", colonnes_distance("densite_groupes", c(st, "D", "se_D", "cv", "bas_D", "haut_D", "ddl"), langue)),
                         sprintf("la densité des groupes %s", par_aire), sprintf("the density of groups %s", par_aire)))
}

# Les graphiques en R de base : la fonction de détection sur l'histogramme des distances (comme
# plot.ds), et pour des points la densité des distances ; le diagramme quantile-quantile des distances
# exactes (comme qqplot.ddf).
code_graphiques_distance <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  t <- m$donnees
  u <- unites_echantillonnage(m)
  a <- if (identical(langue, "en")) "to" else "a"
  xlab <- code_texte(sprintf("%s (%s)", texte_sur(t$noms$y), u$distance))
  intervalles <- !is.null(t$coupures)
  covariables <- !is.null(t$covariables)
  traduit <- function(g) { g$code <- traduire_script(g$code, langue, c(NOMS_DISTANCE_EN, NOMS_SCRIPT_EN)); g }
  # (avec des covariables qualitatives : pour chacune, la fonction de détection moyenne de chacune de ses catégories)
  par_categorie <- lapply(seq_along(m$courbes_niveaux), function(k) list(nom = sprintf("fonction-detection-%d", k), largeur = 7, hauteur = 5, code = c(
    sprintf("niveaux_k <- courbes$niveaux[[%d]]", k),
    sprintf("plot(NA, xlim = c(min(0, histogramme$de[1]), releve$w), ylim = c(0, max(1.05, 1.05 * histogramme$g, 1.05 * niveaux_k$g)), xlab = %s, ylab = %s,",
            xlab, code_texte(tx("Probabilité de détection", "Probability of detection"))),
    sprintf("     main = paste(libelle_detection(det$fd), %s, niveaux_k$covariable))", code_texte(tx("— par", "— by"))),
    sprintf("rect(histogramme$de, 0, histogramme$%s, histogramme$g, col = \"#2f6fb522\", border = \"#2f6fb5\")", a),
    "teintes <- grDevices::hcl.colors(length(niveaux_k$niveaux), \"Dark 3\")",
    "matlines(courbe$x, niveaux_k$g, lty = 1, lwd = 2, col = teintes)",
    "legend(\"topright\", sprintf(\"%s (n = %d)\", niveaux_k$niveaux, niveaux_k$n), lty = 1, lwd = 2, col = teintes, bty = \"n\")")))
  lapply(Filter(Negate(is.null), c(list(
    list(nom = "fonction-detection", largeur = 7, hauteur = 5, code = c(
      sprintf("plot(NA, xlim = c(min(0, histogramme$de[1]), releve$w), ylim = c(0, max(1.05, 1.05 * histogramme$g, 1.05 * courbe$g)), xlab = %s, ylab = %s, main = libelle_detection(det$fd))",
              xlab, code_texte(tx("Probabilité de détection", "Probability of detection"))),
      sprintf("rect(histogramme$de, 0, histogramme$%s, histogramme$g, col = \"#2f6fb533\", border = \"#2f6fb5\")", a),
      "lines(courbe$x, courbe$g, col = \"#d1495b\", lwd = 2)",
      if (!intervalles) "points(releve$x, g_x, pch = 19, cex = 0.6, col = \"#d1495b80\")",
      sprintf("legend(\"topright\", c(%s, %s%s), pch = c(15, NA%s), lty = c(NA, 1%s), lwd = c(NA, 2%s), col = c(\"#2f6fb566\", \"#d1495b\"%s), bty = \"n\")",
              code_texte(tx("histogramme (comme plot.ds)", "histogram (as plot.ds)")),
              code_texte(if (covariables) tx("fonction de détection moyenne (comme plot.ds)", "average detection function (as plot.ds)") else tx("fonction de détection g", "detection function g")),
              if (!intervalles) paste0(", ", code_texte(tx("détections", "detections"))) else "", if (!intervalles) ", 19" else "", if (!intervalles) ", NA" else "",
              if (!intervalles) ", NA" else "", if (!intervalles) ", \"#d1495b80\"" else "")))), par_categorie, list(
    if (t$point) list(nom = "densite-distances", largeur = 7, hauteur = 5, code = c(
      sprintf("plot(NA, xlim = c(min(0, histogramme$de[1]), releve$w), ylim = c(0, 1.05 * max(histogramme$f, courbe$f)), xlab = %s, ylab = %s, main = %s)",
              xlab, code_texte(tx("Densité de probabilité", "Probability density")),
              code_texte(if (covariables) tx("Densité des distances, la moyenne des f(r | z) = 2r g(r | z) / (w² p)", "Density of the distances, the mean of the f(r | z) = 2r g(r | z) / (w² p)")
                         else tx("Densité des distances f(r) = 2r g(r) / (w² p)", "Density of the distances f(r) = 2r g(r) / (w² p)"))),
      sprintf("rect(histogramme$de, 0, histogramme$%s, histogramme$f, col = \"#2f6fb533\", border = \"#2f6fb5\")", a),
      "lines(courbe$x, courbe$f, col = \"#d1495b\", lwd = 2)")),
    if (!intervalles) list(nom = "quantile-quantile", largeur = 6, hauteur = 6, code = c(
      sprintf("plot(adequation$qq$empirique, adequation$qq$ajustee, xlim = c(0, 1), ylim = c(0, 1), asp = 1, pch = 19, col = \"#2f6fb5bf\", xlab = %s, ylab = %s,",
              code_texte(tx("Répartition empirique", "Empirical distribution")), code_texte(tx("Répartition ajustée", "Fitted distribution"))),
      sprintf("     main = sprintf(%s, adequation$cvm$W, adequation$cvm$p))", code_texte(tx("Cramér–von Mises : W = %.4f, p = %.3f", "Cramér–von Mises: W = %.4f, p = %.3f"))),
      "abline(0, 1, lty = 3)"))))), traduit)
}

# Les résultats du script (dans `e`) comparés à ceux de `m` : NULL, ou ce qui diffère.
ecart_distance <- function(m, e, langue) {
  r <- function(x) get0(nom_traduit(x, langue, NOMS_DISTANCE_EN), envir = e, inherits = FALSE)
  champ <- function(x, ...) { for (k in c(...)) x <- if (!is.null(x)) x[[nom_traduit(k, langue, NOMS_DISTANCE_EN)]]; x }
  # (les colonnes de nombres, et les logiques en 0 et 1, d'un tableau)
  nombres <- function(x) if (!is.null(x)) as.matrix(Filter(function(v) is.numeric(v) || is.logical(v), as.data.frame(x)))
  det <- r("det")
  pr <- r("pr")
  ad <- r("adequation")
  rel <- r("releve")
  a <- m$adequation
  detection <- r("detection")
  covariables <- !is.null(m$p_detections)
  # (des covariables : p̄ et la probabilité de chaque détection ; la fonction de détection moyenne de chaque catégorie)
  niveaux_script <- champ(r("courbes"), "niveaux")
  paires <- list(
    "modèles candidats" = list(nombres(m$candidats), nombres(r("candidats"))),
    "paramètres" = list(m$detection$theta, champ(det, "theta")), "erreurs-types" = list(m$detection$se, champ(det, "se")),
    covariance = list(m$detection$V, champ(det, "V")), information = list(m$detection$H, champ(det, "H")),
    "log-vraisemblance et AIC" = list(c(m$detection$ll, m$detection$AIC, m$detection$q), c(champ(det, "ll"), champ(det, "AIC"), champ(det, "q"))),
    p = list(c(m$p, m$var_p, m$se_p), c(if (covariables) champ(r("p_moyenne"), "p") else champ(pr, "p"), r("var_p"), r("se_p"))),
    "p des détections" = if (covariables) list(m$p_detections, champ(pr, "p")),
    "courbes par catégorie" = if (length(m$courbes_niveaux)) list(unlist(lapply(m$courbes_niveaux, function(cn) c(cn$g, cn$f, cn$n))),
                                                                   unlist(lapply(niveaux_script, function(cn) c(champ(cn, "g"), champ(cn, "f"), champ(cn, "n"))))),
    detection = list(cbind(c(m$p, m$effectif$valeur, m$Nc), c(m$se_p, m$effectif$se, NA), c(m$se_p / m$p, m$effectif$se / m$effectif$valeur, NA)),
                     if (!is.null(detection)) as.matrix(detection[2:4])),
    "Cramér–von Mises" = if (!is.null(a$cvm)) list(unlist(a$cvm), unlist(champ(ad, "cvm"))),
    "khi²" = list(c(a$khi2$chi2, a$khi2$ddl, a$khi2$p), c(champ(ad, "khi2", "chi2"), champ(ad, "khi2", "ddl"), champ(ad, "khi2", "p"))),
    "classes du khi²" = list(nombres(a$khi2$classes), nombres(champ(ad, "khi2", "classes"))),
    "diagramme quantile-quantile" = if (!is.null(a$qq)) list(as.matrix(a$qq[2:5]), if (!is.null(champ(ad, "qq"))) as.matrix(champ(ad, "qq")[2:5])),
    "troncature" = list(c(m$donnees$w, m$donnees$L, m$donnees$n), c(champ(rel, "w"), champ(rel, "L"), champ(rel, "n"))),
    distances = list(m$donnees$x, champ(rel, "x")),
    transects = list(nombres(m$donnees$transects), nombres(champ(rel, "transects"))),
    densite = list(nombres(m$densite), nombres(r("densite"))),
    "densité des groupes" = if (!is.null(m$densite_groupes)) list(nombres(m$densite_groupes), nombres(r("densite_groupes"))),
    courbe = list(as.matrix(m$courbe), nombres(r("courbe"))), "g des détections" = list(m$g_x, r("g_x")),
    histogramme = list(nombres(histogramme_echantillonnage(m$donnees, m$p)), nombres(r("histogramme"))))
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  # (les libellés : ceux des modèles candidats, des strates et des transects, la raison du choix ; les
  # catégories des courbes par catégorie, les noms des paramètres)
  if (!identical(m$candidats$modele, champ(r("candidats"), "modele"))) ecarts["libellés des modèles"] <- Inf
  if (covariables && !identical(m$candidats$covariables, champ(r("candidats"), "covariables"))) ecarts["covariables des modèles"] <- Inf
  if (!identical(lapply(m$courbes_niveaux, function(cn) cn$niveaux), lapply(niveaux_script, function(cn) champ(cn, "niveaux")))) ecarts["catégories des courbes"] <- Inf
  if (!identical(m$parametres$parametre, champ(r("parametres"), "parametre"))) ecarts["noms des paramètres"] <- Inf
  if (!identical(m$densite$strate, champ(r("densite"), "strate")) || !identical(m$donnees$transects$transect, champ(rel, "transects", "transect"))) ecarts["strates et transects"] <- Inf
  if (!identical(m$selection, champ(r("choix"), "raison"))) ecarts["raison du choix"] <- Inf
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

ecrire_script_distance <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  d <- resoudre_observations(projet, d)  # (sa condition, s'il en a une, d'après les données du moment : condition.R)
  r <- m$reglages_echantillonnage
  t <- m$donnees
  # (les identifiants du transect et de la strate, et ceux que lit sa condition : condition.R)
  ids <- unlist(lapply(list(r$echantillon, r$strate), function(s) if (identical(as.character(s$by)[1], "id")) as.character(s$id)[1]))
  tab <- tableau_script(projet, unique(c(ids, ids_condition(projet, d))))
  noms <- c(NOMS_DISTANCE_EN, NOMS_SCRIPT_EN)
  fc <- plan_condition(projet, d, tab)$fonctions  # (celles de Nouvelle variable qu'appelle sa condition : condition.R)
  fonctions <- traduire_script(c(code_moteur(racines_distance(m), langue, ENTETES_DISTANCE_EN, COMMENTAIRES_DISTANCE_EN),
                                 if (length(fc)) code_fonctions_condition(fc, tx)), langue, noms, elements = TRUE)
  u <- unites_echantillonnage(m)
  unite_densite <- if (nzchar(u$aire)) sprintf(" /%s", u$aire) else ""
  tot <- m$densite[nrow(m$densite), ]
  covariables <- !is.null(t$covariables)
  piece <- list(
    quoi = sprintf(tx("échantillonnage par distances, %s, %s, %d détections, D = %s%s", "distance sampling, %s, %s, %d detections, D = %s%s"),
                   if (t$point) tx("points", "point transects") else tx("transects linéaires", "line transects"), libelle_detection(m$detection$fd), t$n,
                   formatC(signif(tot$D, 4), format = "fg", digits = 4, decimal.mark = tx(",", ".")), unite_densite),
    tab = tab, fonctions = fonctions,
    calcul = traduire_script(code_calcul_distance(projet, d, m, tab, langue), langue, noms),
    sortie = code_sortie_distance(m, langue), graphiques = code_graphiques_distance(m, langue),
    ecart = function(m, e) ecart_distance(m, e, langue), paquets = character(0),
    texte_paquets = tx("Il refait ce modèle avec R seul, sans Nuée ni autre paquet (ni Distance ni mrds), de ses données à ses résultats, qui sont ceux de Nuée : ses fonctions sont celles de l'échantillonnage par distances du moteur de Nuée, recopiées telles quelles (écrites d'après la méthode publiée, Buckland et al. 2001, elles donnent les résultats des fonctions ds() et dht2() du paquet Distance). Ses quatre parties :",
                       "It redoes this model with R alone, without Nuée or any package (neither Distance nor mrds), from its data to its results, which are those of Nuée: its functions are the distance sampling functions of Nuée's engine, copied as they are (written from the published method, Buckland et al. 2001, they give the results of the ds() and dht2() functions of the Distance package). Its four parts:"),
    parties = c(tx("#   1. Fonctions : celles de l'échantillonnage par distances du moteur de Nuée", "#   1. Functions: the distance sampling functions of Nuée's engine"),
                if (covariables) tx("#   3. Modèle : distances, transects et covariables, troncature, fonction de détection, p̄, adéquation, densité",
                                    "#   3. Model: distances, transects and covariates, truncation, detection function, average p, goodness of fit, density")
                else tx("#   3. Modèle : distances et transects, troncature, fonction de détection, p, adéquation, densité", "#   3. Model: distances and transects, truncation, detection function, p, goodness of fit, density")),
    resultats = Filter(Negate(is.null), list(
      list(nom = "candidats-table", objet = traduire_script(colonnes_distance("candidats", c("modele", if (covariables) "covariables", "parametres", "logL", "AIC", "delta_AIC", "contraint", "choisi"), langue),
                                                            langue, noms), chiffres = 2),
      if (!is.null(m$parametres)) list(nom = "parametres-table", objet = traduire_script("parametres", langue, noms), chiffres = 4),
      list(nom = "detection-table", objet = "detection", chiffres = 4),
      list(nom = "adequation-table", objet = "tests", chiffres = 4),
      list(nom = "densite-table", objet = traduire_script(colonnes_distance("densite", c(if (!is.null(r$strate)) "strate", "n", "k", "D", "se_D", "cv", "bas_D", "haut_D", "ddl",
                                                                                     if (!t$densite_seule) c("N", "bas_N", "haut_N")), langue), langue, noms), chiffres = 4),
      if (t$groupes) list(nom = "groupes-table", objet = traduire_script(colonnes_distance("densite_groupes", c(if (!is.null(r$strate)) "strate", "D", "se_D", "cv", "bas_D", "haut_D"), langue), langue, noms),
                          chiffres = 4))),
    objets = nom_traduit(c("releve", "choix", "det", "candidats", "parametres", "covariance", "pr", "detection", "adequation", "tests", "densite", "densite_groupes",
                           "courbe", "histogramme", if (covariables) c("Z", "choix_sans", "p_moyenne", "courbes")), langue, NOMS_DISTANCE_EN))
  ecrire_script_moteur(projet, d, chemin_script, m, piece, donnees, renv, rdata, rproj, version, langue)
}
