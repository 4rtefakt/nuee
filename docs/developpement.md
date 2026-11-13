# Developing Nuée

Every calculation is plain R in `moteur/R/`, commented in French, so that the person using the
application can read and correct it. The interface is an Electron window with dockable panels,
pop-out windows and Plotly plots, talking to a local R server.

## Layout

| Folder | What it holds |
|---|---|
| `moteur/R/` | The engine: preprocessing, NIPALS PCA/PLS with missing values, cross-validation, OPLS, statistics, prediction, HCA, new analyses |
| `moteur/serveur.R` | Local HTTP server (httpuv) answering the interface's JSON requests (`reajuster`: a model refitted without some observations, as the AIC selection's Fit this model, or with a CAR1 autocorrelation, Fit with CAR1; `condition_observations`: how many ticked observations a condition keeps, for New model's Observations tab; `ajuster_et_garder` keeps a definition with the observations its condition gives; `series`: a refused trend option, such as seasons by month without dates, leaves the plot and is returned as `tendances_erreur`; `table_a_joindre` and `ajouter_colonnes`: Edit › Add columns from a table, the table in any format File › Open reads, `tableau_a_joindre`: a table with its key columns as written, or a project's data; pasted cells kept in a file of the session; undone by Ctrl+Z; `resumer_groupes`: Edit › Summarize by group, a new unsaved project) |
| `moteur/console.R` | The R console: log of engine calls (a series with its trends as `tendances_series(series_variable(...)$points, ...)`), prompt in the application's session (Add columns from a table and Summarize by group: the call, then in a comment the same computation with `match()` or `aggregate()` on the project's data table) |
| `moteur/R/formats.R` | Reading and writing tables: text, Excel, R, SPSS, Stata, SAS, jamovi `.omv` (read and written here), dBase, ARFF; `lire_tableau(textes = )`: columns kept as written, never read as numbers (a key's "01") |
| `moteur/R/spectres.R` | Spectral preprocessing: SNV, MSC, detrend, baseline (lower convex hull), Savitzky–Golay (each range of neighbouring variables on its own) |
| `moteur/R/ipls.R` | iPLS (Nørgaard et al. 2000): the spectrum preprocessed once, whole, then cut into intervals, each a PLS on its range; RMSEcv for 1 to A components |
| `moteur/R/jeu_test.R` | Test set: left out of the model, predicted after fitting (RMSEP, bias, SEP, external Q2, classification); chosen at random or by Kennard–Stone |
| `moteur/R/langue.R` | `traduire("français", "English")`: the engine's messages in the interface's language (the `X-Langue` header of each request) |
| `moteur/R/fichiers_spectres.R` | Spectra files: JCAMP-DX (AFFN, PAC, SQZ, DIF, DUP), SPC (new and old format), text; merged into one table, with a table of sample information joined by file name |
| `moteur/R/script.R` | Export to R: a standalone script or R Markdown / Quarto report written for one model (or the session): the data, then only the steps that model uses (its terms and scaling, NIPALS, cross-validation, statistics, plots), in plain R with no package (a regression: its rows, terms and the same fits with `lm`, `nlme::lme` or `mgcv::gam`, cross-validation, statistics and plots); refitted and compared with the engine before it is written; a regression's AIC selection once it has run, redone by its script; a CAR1 model's normalized residuals in each group's time order; the quantile residuals of a Poisson, negative binomial, binomial or gamma model, with the engine's seed, and their plot; a CAR1 time made of dates read by the script's own `temps_en_nombres()` (`modele_temps_dates`: `annees_decimales`'s operations in the same order); a model chosen by a condition (condition.R): the ticked rows, the condition evaluated with `with()` on every row of the table, written with the script's own column names (one the English translation would change between backticks), the qualitative variables as text, then the ticked rows where it is TRUE, and the New variable functions it calls written in the scriptregression's candidate terms in the engine's order, without the factors that have no main effect (`termes_candidats`), and its unseen categories left without prediction (`niveaux_inconnus()`)random slopes (`random = ~ year \| site`, nlme's list for nested levels, `s(site, year, bs = "re")` for mgcv), their variances and correlations (`variances_pentes()`) and the R² of Johnson (2014) (`variances_niveaux()`), the AIC selection keeping a slope's group in every model (`termes_gardes`) |
| `moteur/R/series.R` | Series in time (Plots › Series): a variable over time in long format, for the observations with a known time and value, by line (the sector) and panel (the site); the time step (a gap breaks a line), each panel's mean per time, what was left out, the times that come twice in a line; whether the time was dates; the trends (`tendances_series`: for each line, the Mann–Kendall S, its variance corrected for ties and its two-sided p by the normal approximation with the continuity correction, computed here rather than by `cor.test`, Kendall's tau-b, Sen's slope with its 95 % interval from the ranked pairwise slopes (Gilbert 1987, `intervalle_sen`), the p corrected over the lines by BH or Holm; no test under 4 times; the test of each line named in `test`); Hamed and Rao's (1998) correction for autocorrelation (`facteur_hamed_rao`, as `modifiedmk::mmkh`: the line detrended by its Sen slope over its actual times, the autocorrelations of its ranks beyond ±1.96/√n, n/n* multiplying Var(S), the p and the slope's interval from it; a factor that isn't positive leaves the line untested); the seasonal Kendall test (`kendall_saisonnier`, Hirsch, Slack and Smith 1982, as `trend::smk.test` and `EnvStats::kendallSeasonalTrendTest`: the season of each point from the month of the dates or a qualitative variable or ID, `ajouter_saisons`; the year the whole part of the time; the median of several values in a season of one year; S and Var(S) summed over the seasons seen in at least 2 years; the seasonal Sen slope and its Gilbert interval from Var(S), the ranks rounded where EnvStats interpolates; EnvStats' weighted τ; no test under 4 values in those seasons) |
| `moteur/R/methodes.R` | The Methods paragraph and its references, in French or English; a model chosen by a condition: "the N observations where …" (`observations_texte`, condition.R, in every type's Methods text) |
| `moteur/R/regression.R` | Linear, generalized and mixed models: `lm` and `nlme::lme` (nested random effects, CAR1), REML estimates and ML likelihoods, marginal and conditional R2; `mgcv::gam` for GLMs, GLMMs and GAMs (normal, Poisson, negative binomial, binomial, gamma, Tweedie; an offset; smooth curves; random effects as `s(g, bs = "re")`), fitted by ML without a penalty and by REML with one, the AIC then from an ML refit; `mgcv::gamm` for a normal GLM or GAM with random effects and CAR1 (an `nlme` mixed model whose curves are random effects; its likelihood from an ML refit); cross-validation by groups, block contributions by AIC, the AIC selection (`selection_aic`: every combination of the groups of terms, a cross term only with its main effects, or a backward elimination beyond 2^K = 256, each refitted by ML through `ajuster_termes_ml`; AICc, Akaike weights, importance, the best and the most parsimonious model; `definition_selection` for Fit this model), `definition_car1` for Fit with CAR1 (the same definition with CAR1 within the random effects' groups or a finer level, refused where `verifier_regression` would refuse it), effect curves, a curve's thresholds (`seuils_courbe`: its slope with simultaneous bands from 2000 draws of the coefficients, as Simpson 2018) and concurvity (`mgcv::concurvity`), Methods text (a model chosen by the AIC selection, and the BACI mixed model: `phrase_methodes_baci`); the CAR1 time read by `temps_en_nombres` (numbers, or dates as decimal years); qualitative variables in cross terms (one product column per category; `candidats_regression` orders the terms lower-order first, as R's `terms()`, so that the pivoted QR keeps R's treatment coding with the last category as reference; a factor without a main effect as a hidden `facteur_seul` term; `m$composition`: the variables and categories of each column t1..tK; one slope per category in Effects, `effets_par_niveau`, from the coefficients' covariance; a main effect leaving with its cross terms in the contributions,random slopes (`pentes` of each level of `d$aleatoires`: a quantitative X term's column, `pentes_definition`): nlme's general matrix (`aleatoire_nlme`: `~ t3 \| g1`, or the list `list(g1 = ~ t3, g2 = ~ 1)`; also for `mgcv::gamm`), `s(g1, t3, bs = "re")` independent of the intercept in `mgcv::gam`; their variances and correlations (`tableau_variances`), each level's share and the marginality of `groupes_requis`; `termes_absents`, combinations never observed; `niveaux_inconnus`: an unseen category in a slope without main effect left without prediction; `phrases_qualitatives`: reference categories and what the cross terms' coefficients are in the Methods text)R² of Johnson 2014 (`variances_niveaux`: the mean of zᵢᵀΣzᵢ), the signs of a singular fit (`singularites_pentes`), a slope's group kept in every model of the AIC selection (`groupes_gardes`) |
| `moteur/R/regression_diagnostics.R` | Diagnostics of the regressions, computed on request: the autocorrelation of the residuals in time within each group, pooled as `nlme::ACF` (the time read by `temps_en_nombres`: numbers or dates; lags in each group's time order, bands ±1.96/√n; a CAR1 model's normalized residuals whitened in each group's time order, which nlme gets right only on rows sorted by group, then by time), and the likelihood-ratio tests between the nested regressions of an AIC set (same distribution, effort, random effects and autocorrelation, the same random slopes: `memes_pentes`, the same span with the intercept for nlme, proportional columns for mgcv; the smaller fixed design within the bigger one's columns), as `anova(m1, m2)` on the ML fits; computed with the fit, the randomized quantile residuals (Dunn and Smyth 1996) of a Poisson, negative binomial, binomial or gamma model (`residus_quantiles`: drawn between F(y − 1) and F(y) with a fixed seed, the session's random numbers restored; the upper tail through 1 − u) |
| `moteur/R/formule.R` | The R formula of a regression: written from New model's settings, and read back into them with `str2lang` without ever being evaluated (only the project's variables and IDs, numbers and a few functions, each in its place); a:b without a, a factor without a main effect; a qualitative variable once at most in a productrandom slopes as lme4 writes them, `(1 + year \| site)`, `(year \| site)`, `(1 + year \| site/sector)` or one term per level `(1 + year \| site) + (1 \| site:sector)`, uncorrelated ones (`0 +`, `\|\|`, a term apart) refused |
| `moteur/R/variables.R` | New variables: an R formula on the variables and observation IDs (with `moyenne_groupe` / `group_mean` and `ecart_groupe` / `group_deviation` by group, and `precedent` / `previous`, `suivant` / `following`, `croissance` / `growth_rate`: another year's value in the same group, the growth rate; `richesse` / `richness`, `shannon`, `simpson` and `pielou`: diversity indices over species columns listed or given as a range in the table's column order, read unevaluated by `indice_diversite`, as vegan computes them, a row of zeros giving a Simpson of 0, a missing value a missing index, a negative value refused), classes from values, and the imputation of missing values by regularized iterative PCA (`imputer_acp`, as `missMDA::imputePCA`); the one conversion of a time (`temps_en_nombres`, used by the series, BACI, the lags, the residual autocorrelation and CAR1): numbers, or dates (ISO, or day first, with a time of day) as decimal years (`annees_decimales`), all of one kind, the first value that isn't refused and named (`refus_temps`); `date_decimale`, the inverse, for texts; `environnement_formule`, where a formula is evaluated: the variables and IDs by their names (shared with a model's condition) Edit › Add columns from a table (`ajouter_colonnes_table`: another table's rows matched to the observations by a key of one or more columns, as `match()`, compared as numbers when the project's column is a quantitative variable, otherwise as text as written, trimmed, capitals counting; a key that comes twice in the table refused, named; no match at all refused; the observations without a match and the table's keys that match none counted and named; numbers stay numbers, the rest qualitative; a name taken becomes "x (2)", `nom_libre`; `proposer_cles`: same-named columns, the combination unique in the table where the most observations find their key, then the one starting earliest in the table, then the smallest) |
| `moteur/R/condition.R` | A model's observations chosen by a condition (New model › Observations, "Keep only the observations where…"): read without running anything (`lire_condition`: the project's variables and IDs, numbers, text and the functions of `FONCTIONS_CONDITION` only, since it is kept in the project file), evaluated like a New variable formula on every observation (`valeurs_condition`: TRUE or FALSE, or 1 and 0, one per observation, missing counting as FALSE), the definition's ticked observations (`d$cochees`) where it is TRUE taken again at each fit (`resoudre_observations`, called by `ajuster` and `ajuster_et_garder`); the Methods sentence; for Export to R, the condition with the script's names (`plan_condition`, `code_condition`) and the New variable functions it calls (`code_fonctions_condition`) |
| `moteur/R/nouveau.R` | A table made into a project (`importer_tableau`, `projet_depuis_tableau`), a model's definition and its settings, autofit, saving a project; Edit › Summarize by group (`resumer_par_groupe`: a new project of one row per group of one or more variables or IDs, named from its values, the group columns kept as they were; mean, sum, median, min, max, sd or n of the chosen quantitative variables, missing values ignored, as `aggregate()`; qualitative variables and IDs constant within every group kept, the others left out and named; rows without a group value left out and counted) |
| `moteur/R/analyses.R` | The other analyses (RDA, correspondence analysis, LDA, decision trees, k-means): their types, and how the engine finds each one's functions by name (`ajuster_<type>`, `resultat_<type>`, `texte_methodes_<type>`, `tables_<type>`, `predire_<type>`, `ecrire_script_<type>`, requests `demande_analyse_<name>`) |
| `moteur/R/ordination.R` | Redundancy analysis (RDA, partial RDA) and correspondence analysis (CA) with R's linear algebra, as vegan computes them: scores in scalings 1 to 3, adjusted R², permutation tests of the model, the axes and the terms (free or within groups, from a seed), forward selection (ordiR2step's double criterion), variation partitioning, Methods text, export tables |
| `moteur/R/arbres.R` | Decision trees (TREE) with rpart: regression or classification, surrogates, pruning by the 1-SE rule through an internal cross-validation by the model's groups (the folds inside `rpart.control`), external validation by groups, leaf rules, importance, first-split stability; the helpers the three analyses share |
| `moteur/R/discrimination.R` | Linear discriminant analysis (LDA) with `MASS::lda`: priors, cross-validation by groups, scores, structure correlations, coefficients, posterior probabilities |
| `moteur/R/kmoyennes.R` | k-means (KMEANS): clusters numbered by size, silhouettes (`cluster::silhouette`), choice of k, centres, clusters saved as a variable |
| `moteur/R/classement_script.R` | Export to R of a tree, an LDA or a k-means: rpart, MASS or cluster, compared with the engine before it is written; a model chosen by a condition: its rows chosen by it (condition.R), then those left out for missing values |
| `moteur/R/ordination_script.R` | Export to R of an RDA or a CA: plain R, the same permutations, compared with the engine before it is written; a model chosen by a condition: its rows chosen by it (condition.R) |
| `moteur/R/comparaisons.R` | Statistical tests: by variable (t, Mann–Whitney, ANOVA, Kruskal–Wallis; two classes paired by a key: paired t or Wilcoxon signed-rank on the keys both have) with multiple-testing correction; between two variables (chi-square or Fisher, groups with Tukey, Games–Howell or Dunn, correlations), the test chosen from their conditions; paired tests (paired t, Wilcoxon signed-rank) between two levels, on the keys both have (`paires_cle()`, shared with the tests by variable), every pair of levels (or those within each group: the sectors of a site) with Holm's correction, after Friedman's omnibus test (`tests_friedman()`: on the keys where every level has a value, with Kendall's W; one per group, Holm over the groups), or two measurements of the same rows; before/after with a control (`baci()`: at each time the gap between the impact's and the controls' means, after against before by Welch's t, Student's t or Mann–Whitney; the time, the event and the end numbers or dates) and its mixed model (`modele_baci()`: the two 0/1 variables of the period and the impact, `variables_baci()`, reused when they exist with these values, refused when another variable has their name; `definition_baci()`, y ~ period × impact + (1 \| unit) with CAR1 within each unit if chosen, its settings kept in `d$baci_origine`; `effet_baci()`, the period × impact coefficient and its sentence); the correlation matrix |
| `moteur/R/modeles_classe.R` | Class models (one PCA per class) and the distances behind the Coomans plot |
| `moteur/tests/verifier_references.R` | Refits every model of a folder of exported `.usp` projects and compares with the results stored in them; for PLS models, also the files' own cross-validation (scaling from all the observations) with the groups read in the files, which gives back the stored Q2 when the groups are right |
| `moteur/tests/verifier_formats.R` | Every export format written and read back; text files as spreadsheets write them; jamovi files compared with `jmvReadWrite` when it is installed |
| `moteur/tests/verifier_spectres.R` | Published Savitzky–Golay coefficients, exact derivatives of polynomials, SNV, MSC, detrend, baseline, Savitzky–Golay by range, and a PLS on made-up spectra; the same results as `prospectr` when it is installed; data with no variation left: a clear message, and automatic fitting stops without error |
| `moteur/tests/verifier_fichiers_spectres.R` | Small made-up JCAMP-DX (every compression form), SPC (each layout) and text files read back; the spectra table and the joined information |
| `moteur/tests/verifier_ipls.R` | iPLS on made-up spectra with one informative band: intervals cover the spectrum in order, each RMSEcv equals that of a PLS on the range's columns of the whole preprocessed spectrum, the whole spectrum gives back the model, the band's interval comes out best |
| `moteur/tests/verifier_jeu_test.R` | The test set: the model doesn't see it, RMSEP / bias / SEP / Q2ext recomputed by hand, PLS-DA classification, random and Kennard–Stone choice (the same observations as `prospectr::kenStone` when installed) |
| `moteur/tests/verifier_opls.R` | OPLS against a second, plain-matrix writing of Trygg and Wold's algorithm (one Y, two Y, three classes); orthogonal scores unrelated to Y |
| `moteur/tests/verifier_script.R` | Scripts and reports written for many kinds of models, and a whole session in one script, run in a fresh `Rscript --vanilla` without the engine, give the engine's results (regressions too: LM with categories and cross terms, nested LMM with CAR1 and a test set, GLMs with an effort, a gamma GLM, GAMMs, Tweedie, null models, a mixed session; each curve's thresholds, bands included; the quantile residuals, the same draws); reports compile with `knitr` when it is installed; the AIC selection (all subsets and backward, French and English, and in an R Markdown report); CAR1 models on rows in year order; the BACI mixed model (CAR1, an end, in English); CAR1 along a qualitative variable of dates (day first), French and English, data in the script or in a CSV, the model of the decimal years; models chosen by a condition (a PLS with unticked observations and a qualitative variable, in English with a name the translation would change; a mixed model with a test set and `precedent()`; a tree with rows left out for missing values; k-means on an ID with `ecart_groupe()`; a session): their rows chosen by the condition in the script, the engine's resultsqualitative cross terms (an LM witha random slope of the year by site (its variances and correlation, the R² of Johnson, its AIC selection, an LMMselection keeping the year), with CAR1 in English, a Poisson GLM, one slope per category without the year's main effect, its unseen categories left without prediction)and a nested sector in English, and in a Poisson GLMM (mgcv's independent slope) |
| `moteur/tests/verifier_observations.R` | A model's observations chosen by a condition (condition.R): the condition gives what `with()` gives on the same data (qualitative variables as text, IDs by name, `` `backticks` ``, the New variable functions, a missing value counting as FALSE); each type (PCA, PLS, OPLS, PLS-DA with unknown classes, OPLS-DA's predictive components, class models, LM, LMM, GLM, GAM, tree, LDA, k-means, RDA, CA) gives the model of the same observations ticked by hand, and the regressions those of `lm()`, `nlme::lme()`, `glm()` and `mgcv::gam()` on these rows; refusals (an unknown name, a function not allowed, never run, from a received project too; a result of the wrong length or type; no observation left), in French and English; the ticks kept, the condition removed; refits (the data changed, automatic fitting, Fit this model, Fit with CAR1); saved and read back; Methods text; prediction of the observations set aside |
| `moteur/tests/verifier_regression.R` | The regressions against `lm()`, `nlme::lme()`, `glm()`, `MASS::glm.nb()` and `mgcv::gam()`: coefficients, AIC, R2, θ, Tweedie's p, a random effect's variance, overdispersion, cross-validation, test set, block contributions, effect curves, CAR1, AIC sets, refused definitions, a saved definition read back, the prediction of observations and files, R formulas (read, written back, fitted like R fits them, refused, never run); `mgcv::gamm` with CAR1; a curve's thresholds against the same procedure written out by hand, on a made-up threshold and a straight line; concurvity against `mgcv::concurvity()`; group means and deviations; the diversity indices (hand-computed values, ranges, lists and both, zeros, missing values, refusals, the English and French names; against `vegan::specnumber` and `vegan::diversity` when `NUEE_VEGAN` points at a library with vegan, skipped otherwise); PCA imputation against the iterative algorithm written out by hand (bounds, replacing, refitting, refusals); `precedent()`, `suivant()` and `croissance()` (a missing year, a step of 2, groups by a variable, IDs or `interaction()`, a duplicate refused, the English names, dates matched only exactly one year apart); the residual autocorrelation and a CAR1 mixed model along dates (a qualitative variable or an ID) giving those of the decimal years and of `nlme::lme`, an impossible date refused; the residuals' autocorrelation against `nlme::ACF()` (with and without CAR1, `gls()`, mgcv, `mgcv::gamm`, nested groups, rows sorted by year or by site) and the likelihood-ratio tests against `anova()` of nlme, `lm()` and `glm()`; CAR1 normalized residuals the same whatever the order of the rows; the AIC selection against `lm()`, `nlme::lme()` with and without CAR1, `mgcv::gam()` and `mgcv::gamm()` in ML, its weights and importance, the backward path, the definition of the chosen model and its Methods sentence; Fit with CAR1 against `nlme::lme()` and `mgcv::gamm()` with `corCAR1` (within the site, within the sector in its site) and its refusals; the quantile residuals redone with R's distribution functions from `glm()` and `mgcv::gam()` fits (Poisson, negative binomial with its θ, binomial 0/1, gamma with its scale; a count far in the tail), the same on a second call, the session's random numbers untouched; qualitative variables in cross terms against `lm()`, `nlme::lme()` with and without CAR1, `mgcv::gam()` (Poisson, a curve) and `mgcv::gamm()` with factors whose first level is Nuée's reference (year × sector, habitat × treatment, without either main effect, three variables listed in any order), the slopes per category against `vcov()`, contributions, AIC selection, nested models, a combination never observed, unseen categories in prediction, cross-validation and a test set, random slopes against `nlme::lme(random = ~ year | site)` (fixed effects, variances, correlation, REML and ML log-likelihoods, AIC), nested slopes (`list(site = ~ year, sector = ~ year)` or `~ 1`), CAR1, `mgcv::gamm`, `mgcv::gam` with `s(site, year, bs = "re")` and `gam.vcomp()`, the R² of Johnson (2014) and each level's share written out by hand, the year left uncentred signalled as a singular fit, predictions, cross-validation by site, refusals, the R formula formula box (read, written back, uncorrelated slopes refused), a saved definition read back, likelihood-ratio pairs, the AIC selection keeping the year, block contributions, Fit with CAR1, the export table and the Methods text |
| `moteur/tests/verifier_ordination.R` | RDA and CA against vegan when it is installed (`NUEE_VEGAN=<library>` points at one): eigenvalues, scores, R², the tests with the same permutations, ordiR2step, varpart; CA against `MASS::corresp`; refusals, exports and scripts; an RDA chosen by a condition, against the same rows computed directly and vegan, its script |
| `moteur/tests/verifier_classement.R` | Trees, LDA and k-means against rpart, `MASS::lda`, `kmeans` and `cluster::silhouette`: the same splits, pruning, predictions and classes, the cross-validation by groups; their exported scripts and reports, run in a fresh R, FR and EN |
| `moteur/tests/verifier_comparaisons.R` | The statistical tests against R's own test functions (Dunn and Games–Howell through their two-group identities with Kruskal–Wallis and Welch's t; the paired tests against `t.test()` and `wilcox.test()` with `paired = TRUE` on long and wide data, every pair of levels against `pairwise.t.test()` and `pairwise.wilcox.test()`, the pairs within each site against `t.test()`, `wilcox.test()` and `p.adjust()` built site by site; two classes paired by a key, variable by variable, against `t.test()` and `wilcox.test()` with `paired = TRUE` and `p.adjust()`; BACI against `t.test()`, `wilcox.test()` and the `lm()` of the gap on the period, on gaps computed directly; Friedman's test before the pairs against `friedman.test()` on complete blocks built directly, Kendall's W by hand, the keys left out counted, one test per site with `p.adjust()` over the sites, the sites not tested and why; the BACI mixed model: its two 0/1 variables, the model against `nlme::lme(y ~ periode * impact, random = ~ 1 \| unite)` with and without `corCAR1()` and with an end, units and time as IDs, the BACI effect and its sentence against nlme's coefficient, a name taken by another variable refused, the settings recognised after a save, the Methods text), class models, OPLS with several predictive components, the Methods text; dates as time (ISO and day first, a time of day, leap years, R dates, a mix or an impossible date refused) against the day of the year of `as.POSIXlt`, and Series and BACI on dates giving those of the decimal years; the trends of Series against Mann–Kendall's S, its variance with ties, its classic p, Sen's slope and its Gilbert interval and `cor(method = "kendall")` written out by hand, the correction over the lines against `p.adjust()`, the lines not tested; Hamed and Rao's correction against its formula written out by hand (an autocorrelated series, none kept, two missing years, a factor that isn't positive, a straight line) and against `modifiedmk::mmkh`'s n/n*, variance and p on `Nile` and `LakeHuron` (recorded from modifiedmk 1.6); the seasonal Kendall test against its formula written out by hand (months of day-first dates, a missing season-year, a season seen once, several values in a season-year, ties, seasons by a qualitative variable or an ID, observations without a season, too few years per season, constant seasons) and against `trend::smk.test`, `trend::sea.sens.slope` and EnvStats' τ on `nottem` (recorded); the refused options |
| `moteur/tests/verifier_validation.R` | The cross-validation: the engine's PRESS equals refitting each group's model by hand and predicting the group with `predire()`; on pure noise the median Q2 stays at or below 0 for 6 to 30 observations (PCA, PLS, OPLS-DA, automatic or fixed A); no more components than the data's rank; the example's models keep a high Q2; the permutation test refits from the project; groups by the value of a variable or of an ID, or given as they are (the same groups and Q2 each way, observations without a value in a group of their own, a clear error for a single group); block scaling and modifiers (each weight, a block of UV terms of total variance 1, the same weights in prediction and in each sub-model, kept by the settings of New model from this one) |
| `moteur/tests/verifier_tableaux.R` | Add columns from a table against `match()`: one key and two, text keys (trimmed, capitals counting) and numeric ones (2019 = "2020,0" read as a number), the observation names as key, keys read as written from CSV and Excel ("01" isn't 1), a repeated key refused and named, no match refused, the observations without a match and the table's keys alone counted and named, rows without a key, names already taken ("x (2)", "x (3)", an ID's name), the types of the added columns (numbers, text, a factor's order, logical), the key proposed; Summarize by group against `aggregate()` and `tapply()`: every statistic, missing values, a group without a known value (n = 0), sd of one value, two and three grouping columns, a qualitative variable as group, qualitative variables and IDs kept or left out, rows without a group value, the names in English, the project saved and read back, refusals |
| `app/` | Electron main process, preload, interface (`app/interface/`), build script |
| `app/installation.js` | Windows: whether Nuée is installed for all users or for this user only (its uninstall entry under HKLM or HKCU; without one, Program Files decides), for the updater |
| `app/tests/essai_installation.js` | That decision with stand-in registries, then `reg.exe` itself |
| `app/interface/vues_classement.js` | The views of a tree, an LDA and a k-means (tree, pruning, rules, importance, stability, scores, coefficients, probabilities, clusters, silhouettes, choice of k), their New model fields and report parts |
| `app/interface/vues_ordination.js` | The views of an RDA and a CA (ordination, eigenvalues, permutation tests, contributions), their New model fields and report parts |
| `app/tests/essai_api.js` | Check run before each build |
| `app/tests/essai_appli.js` | The built program used end to end over the Chrome DevTools Protocol: the engine starts, the example project, a PLS model's views, an image of a plot, each fit in the R console, a `.nuee` given to a second start opens in the running window, then quitting stops R and removes its temporary folder (`node tests/essai_appli.js <program> [screenshot.png] [options]`; on Linux the program starts with no `LANG` and a short `PATH`, as from the desktop menu) |
| `app/icone/` | The logo, drawn by `dessiner_icone.py` |
| `exemple/` | The example project (made-up data) and `creer_exemple.js`, which makes it |
| `convertisseur/` | `.usp` to JSON converter; `usp_extract.py` is also used standalone to extract tables and model results |

## Running from the sources

Requires R 4.6 with `jsonlite`, `httpuv`, `openxlsx`, `haven` and `readxl` (and `foreign`,
`nlme`, `mgcv`, `MASS`, `rpart` and `cluster`, among R's recommended packages), Node, and
Python 3 for the converter.

```bash
cd app && npm install && npm run vendor
Rscript moteur/serveur.R 8765 app/interface   # then open the address it prints
npx electron app                               # or the desktop window, which starts R itself
```

The engine answers only requests carrying the session's token, in the cookie `nuee_<port>`.
The desktop app draws it at each start and passes it to R as `NUEE_JETON`. Started by hand,
the server draws its own and prints `http://127.0.0.1:8765/api/connexion?jeton=…`: opening
that sets the cookie and shows the interface. Add `&ouvrir=<path>` to open a file. The checks
(`essai_api.js`, `creer_exemple.js`) send the cookie themselves. The server also refuses any
other host name than `127.0.0.1` or `localhost` (DNS rebinding).

`node exemple/creer_exemple.js` rebuilds the example project after an engine change.

Check the file formats, the spectral preprocessing, the export to R, the cross-validation, the
statistical tests, the class models, the columns added from a table and the summaries by group
(`jmvReadWrite`, `prospectr` and `knitr`, if installed, are only used to
compare with other implementations and to compile the reports):

```bash
Rscript moteur/tests/verifier_formats.R
Rscript moteur/tests/verifier_spectres.R
Rscript moteur/tests/verifier_fichiers_spectres.R
Rscript moteur/tests/verifier_opls.R
Rscript moteur/tests/verifier_jeu_test.R
Rscript moteur/tests/verifier_ipls.R
Rscript moteur/tests/verifier_script.R
Rscript moteur/tests/verifier_comparaisons.R
Rscript moteur/tests/verifier_regression.R
Rscript moteur/tests/verifier_validation.R
Rscript moteur/tests/verifier_ordination.R
Rscript moteur/tests/verifier_classement.R
Rscript moteur/tests/verifier_observations.R
Rscript moteur/tests/verifier_tableaux.R
```

Before 0.9.0, the spectra readers were run on real files and compared with other readers:
- **JCAMP-DX:** the 21 official test files of jcamp-dx.org and R. J. Lancashire's 41 test files
  (from the `jcamp` Python package's repository). Every (X++(Y..Y)) table passes the files' own
  checks: the abscissa that starts each line matches the number of values decoded before it,
  and the count matches NPOINTS. The same spectrum in AFFN, PAC, SQZ and DIF/DUP decodes to the
  same values. The Python reader `jcamp` 1.3.2 gives identical values on 28 spectra; on three
  files it drops the repeat (DUP) that follows a line's check value, and falls short of NPOINTS.
  Peak tables and NTUPLES (complex NMR, mass spectra) are not read; one deliberately corrupted
  file is refused.
- **SPC:** the 25 sample files of the `spc` Python package (Galactic's own examples among them),
  against that package's text output: the same values to 1e-12 in both formats. Where they
  differ, that package is wrong: it treats a subfile's negative exponent as 0 (values 2 or 4
  times too large; the files' exponents are the tightest that fit only under our reading), and
  its abscissas for two files with per-subfile x lie outside the files' own first and last x.
- **Preprocessing:** detrend and the baseline give `prospectr`'s results (detrend: up to each
  spectrum's mean, which `prospectr` keeps when it fits the polynomial without a constant;
  Barnes et al. fit it with one).
- **Reports:** the R Markdown and Quarto reports render to HTML with rmarkdown 2.32 and Quarto
  1.10.20 (French and English, data in the script or in a CSV).

Before 0.8.0, the exported files were also opened in the programs themselves: jamovi 2.7 (its
own reader, `jamovi.server.formatio.omv`), PSPP 2.1 (`pspp` with `GET FILE` and `LIST`), Weka 3.8
(`weka.core.converters.ArffLoader`), LibreOffice 25.8 (headless conversion of the Excel, CSV and
dBase files) and pandas 3.1 (`read_stata`, and `read_sas` for SAS transport, both its own
readers). They found three faults, now fixed and covered by `verifier_formats.R`: Windows line
endings in the jamovi manifest, which jamovi refused; SAS transport written in version 8, which
`libname xport` and pandas don't read (now version 5); dBase text in UTF-8 without a code page,
missing numbers written as asterisks. pandas still drops the last row of some small SAS transport
files: it counts every block of eight spaces in the last 80 bytes as padding, including those
inside the last row's text.

Check the engine against the results stored in `.usp` projects (exported with
`convertisseur/export_project.py`):

```bash
python convertisseur/export_project.py <folder of .usp> -o exported -j 16
Rscript moteur/tests/verifier_references.R exported results.csv 16
```

## Build

```bash
cd app && node construire.js <output folder>
```

It builds for the system it runs on: the Windows program on Windows, the Linux one on Linux
(see [On Linux](#on-linux)). It needs Node 22.12 or later (`@electron/packager` requires it, and
the syntax check below relies on Node telling ES modules apart by itself).

The build first runs `app/tests/essai_api.js`. It syntax-checks the interface files, then starts
the R server and runs the main requests on a small invented table: open, fit PCA/PLS/OPLS/PLS-DA,
refit, predict, edit, undo, notes, save with session and reopen, a model's observations chosen
by a condition (the count request, its refusals, refits, save and reopen), standalone R script with its
options, console, example project, statistical tests, Methods text, class models, an OPLS-DA
with four classes, paste, columns added from a table by one key and by two then undone, a summary
by group, export to every format and reopen, a workbook with two sheets. Any failure stops
the build. `SANS_ESSAI=1` skips it, and `node tests/essai_api.js` runs
it alone.

On Windows it then produces two things.

**`Nuee/`**, a folder that runs without installing anything, for example from a network share:

- `application/`: Electron (French and English only), a portable R with the packages the engine
  needs plus R's recommended ones (copied from the local installation, without manuals, Tcl/Tk
  or other message languages), the engine, the example project, and the converter compiled with
  PyInstaller.
- `Lancer Nuee.cmd`: the launcher.
- `LISEZ-MOI.txt` and `USER-GUIDE.txt`: the user guide, in French and in English (both are also
  in `application/resources/`, where *Help › User guide › Full guide* opens the one in the
  interface's language).

Electron crashes when started from a network share, so the launcher mirrors `application/` to
`%LOCALAPPDATA%\Nuee` with robocopy and starts it from there. The first launch copies everything;
later launches copy only what changed on the share. The launcher puts "Nuée" shortcuts on the
desktop and in the Start menu, and starts the app with `--lanceur-nas`: the app then lists that
copy in Settings › Apps (key `HKCU\…\Uninstall\Nuee`), with `resources\desinstaller.cmd` as its
uninstaller. No administrator rights are needed.

**`installateur/Nuee-Setup-<version>.exe`**, made from the same `application/` folder with
electron-builder (NSIS): install for the current user (no administrator rights) or for all
users, Start menu and desktop shortcuts, and an entry in Settings › Apps with its uninstaller.
It is not code-signed, so SmartScreen warns on first run. `SANS_INSTALLATEUR=1` skips it.
An installed copy asks `api.github.com/repos/4rtefakt/nuee/releases/latest` once per start
whether a newer version exists, and shows a link in the status bar (off in Help; never for the
network-share copy or when running from the sources). If the release has a
`Nuee-Setup-<version>.exe` asset with a SHA-256 `digest` (GitHub computes it on upload), the
link offers the one-click update (`app/main.js`): the installer is downloaded from
`github.com/4rtefakt/nuee/releases/download/` into `%TEMP%\nuee-mise-a-jour`, its SHA-256 must
match, unsaved projects are saved or left as the user says, R is stopped, and a `cmd.exe`
without a console waits two seconds (by running `Nuee.exe` as Node: `ping`, a console program,
opened a black window up to 0.9.5), runs the installer silently (`/S`, `/currentuser`, or
`/allusers` when Nuée is installed for all users, which Windows asks to allow: its entry in
Settings › Apps is under HKLM, else Program Files decides, `app/installation.js`; `/D=` the
current folder), then starts Nuée again with the same `--user-data-dir`. That `cmd.exe` is
started by a go-between that ends at once (`Nuee.exe` with `ELECTRON_RUN_AS_NODE`), so it isn't
in Nuée's process tree: the installer closes `Nuee.exe` with everything it started, and in one
test out of three it killed a `cmd.exe` started directly by Nuée, before installing anything.
The installer removes the old version first (keeping the settings) and keeps each file's
original date; the next start deletes the downloaded installer. The new version then shows its
own interface: the engine serves it with `Cache-Control: no-cache` (a `304` when nothing
changed), and the first start of a new version empties Chromium's HTTP cache (`reglages.json`
keeps the version). Up to 0.9.5 the interface came without `Cache-Control`, Chromium kept it
fresh for a tenth of its age, and right after an update the new version showed the old
interface.

To test it without a release, set `NUEE_SOURCE_MAJ` to an address answering like the GitHub
API (a `tag_name` above the installed version and an asset with `name`, `size`,
`browser_download_url` and `digest`); the installer may then come from anywhere. Checked
end to end: install the build, offer a "0.9.9" whose installer is that same build
from a local server, first with a wrong SHA-256 (refused, nothing installed), then the right
one (downloaded, installed, restarted with the same profile), and uninstall it.

Settings live in `%APPDATA%\Nuee` (`~/.config/Nuee` on Linux); `--user-data-dir=<folder>` uses
another one (a clean profile for tests or screenshots).

### On Linux

R is not brought along: a Linux R build is tied to its distribution's libraries (BLAS, ICU,
readline…), so the application uses the system's R, found on the `PATH`, then in
`/usr/local/bin`, `/usr/bin` and `/opt/R/<version>/bin` (`RSCRIPT=<Rscript>` picks another, for
the build too). If R or one of the engine's packages is missing, the error at start-up says
what to install. The `.usp` converter needs only Python's standard library: it is shipped as
`resources/convertisseur/*.py` and run with `python3` (`PYTHON=<python>` picks another).
`moteur/serveur.R` switches R to `C.UTF-8` when it starts without a UTF-8 locale (a launch
from the desktop menu with no `LANG`), since the engine's files are in UTF-8.

The build produces:

- **`Nuee/`**: the application (`./nuee` starts it) with the engine, the example project, the
  converter and `LISEZ-MOI.txt`, readable by every user (the packager builds in a folder made
  by `mkdtemp`, mode 700, which the `.deb` would otherwise install as `/opt/Nuée` for root
  alone).
- **`installateur/`**, made from that folder with electron-builder:
  - `Nuee-<version>-amd64.deb`, which depends on electron-builder's list plus `libgbm1` and
    `libasound2` (Electron loads them; a minimal Ubuntu 22.04 lacks `libgbm1`), then on
    `r-base-core` and `r-cran-httpuv`, `r-cran-jsonlite`, `r-cran-openxlsx`, `r-cran-haven`,
    `r-cran-readxl`, `r-cran-foreign`, `r-cran-nlme`, `r-cran-mgcv`, `r-cran-mass`,
    `r-cran-rpart`, `r-cran-cluster` (Debian keeps R's recommended packages, these six among
    them, out of `r-base-core`), and recommends `python3` and `r-recommended` (for the R
    console, as on Windows). It installs into `/opt/Nuée`, adds `nuee.desktop` to the
    applications menu and, on Ubuntu 24.04 and later, the AppArmor profile Electron's sandbox
    needs;
  - `Nuee-<version>-x86_64.AppImage` and `Nuee-<version>-x64.tar.gz`, for other distributions,
    with R and the packages installed apart. The AppImage uses electron-builder's default
    runtime, which mounts it with FUSE 2 (`libfuse2`). Ubuntu 24.04 and later keep
    unprivileged user namespaces to programs with an AppArmor profile, so an AppImage there
    starts only with `--no-sandbox`.

  `SANS_INSTALLATEUR=1` skips them. Building the `.deb` needs `fpm`, which electron-builder
  downloads; on NixOS, where its `#!/bin/bash` doesn't run, set `USE_SYSTEM_FPM=true` with
  `fpm` installed.

`package.json` names the desktop file (`desktopName`), so the windows' app id (`nuee`) matches
the menu entry and docks group them under its icon. The tests run the same way:
`node tests/essai_api.js` and `Rscript moteur/tests/verifier_*.R` find `Rscript` on the `PATH`.

## Checks on GitHub

`.github/workflows/verifications.yml` runs the engine's checks (`moteur/tests/verifier_*.R`,
except `verifier_references.R`, whose exported projects aren't in the repository) and
`app/tests/essai_api.js` on every push and pull request:

- on Windows, with R 4.6.2 from CRAN, the version the Windows build ships (change both
  together);
- on Ubuntu 24.04, with Ubuntu's own R (4.3) and R packages. This job then builds the Linux
  packages, installs the `.deb` with apt, and runs `app/tests/essai_appli.js` as the runner's
  ordinary user on the installed program and on the AppImage (mounted with FUSE 2), under
  Xvfb. The packages are kept with the run (artifact `nuee-linux`), and so are the
  screenshots (`capture-*`);
- in Debian 12 (R 4.2) and Ubuntu 22.04 (R 4.1) containers: that `.deb` installed with apt,
  which brings R and its packages, then a check that the program finds every library it
  loads, the engine's checks, `essai_api.js` and `essai_appli.js` with that R.

Electron's and electron-builder's downloaders fetch GitHub release files anonymously, which
GitHub throttles at times (HTTP 504 for hours on 2026-11-09). The Linux job has them fetched
through GitHub's API with its token instead (`.github/miroir-github.js`, through
`ELECTRON_MIRROR` and `ELECTRON_BUILDER_BINARIES_MIRROR`); both still check the files against
their own checksums. The files are also cached between runs.

## Releasing

1. The version in `app/package.json` (and `package-lock.json`) and in `CITATION.cff`.
2. Its entry at the top of `app/interface/nouveautes.js` (French and English): the build
   writes `CHANGELOG.md` from it (`node app/changelog.js` does it alone).
3. `node app/construire.js <folder>`, then the installer's checks.
4. A GitHub release `v<version>` with `Nuee-Setup-<version>.exe`: installed copies offer it as a
   one-click update. With the repository switched on at zenodo.org, each release also gets a DOI.
   Publishing the release starts the checks on GitHub: the Linux jobs build the `.deb`, the
   AppImage and the `.tar.gz` from the tag and check them, then `linux-version-publiee` adds
   them to the release.

## How it matches the stored results

Checked on 944 fitted models from 133 distinct `.usp` projects (file versions v10, v11 and
v16/17). Thresholds are 1e-4 for the fit and 1e-3 relative for derived statistics.

- **Scaled data, rebuilt from raw values:** all 944 match.
  - UV / Pareto / centring weights are computed over the workset observations.
  - The power transform is supported.
  - Qualitative variables expand into 0/1 terms.
  - A cross term is the product of its already-scaled factors, then scaled again.
- **PLS, complete data:** R2X, R2Y and scores match in 222/222 models.
- **With missing values:** 606–623 of 633 PLS models match, depending on the statistic.
  - NIPALS skips missing cells.
  - The correction factor is capped at 3.
- **Derived statistics:**
  - Eigenvalue (= R2X·min(N, K)), T2, T2Crit, DCrit, VIP, coefficients and RMSEE match in every complete-data model.
  - The stored absolute DModX, and the weighted DModX, match in 264 of 267 complete-data models.
- **Signs:** models read from a `.usp` are oriented like the stored solution.

Known differences:

- **Q2, and everything that comes from the cross-validation** (Q2 per variable, RMSEcv, CV-ANOVA, the cross-validated classification and ROC, jack-knife intervals, automatic fitting): Nuée's cross-validation is its own, and is reported apart from the comparison.
  - Observations go to 7 groups in turn (observation i to group i mod 7). Each group is predicted by a sub-model fitted without it, through the path of new observations (`prediction.R`): everything the engine computes from the observations — centring and scaling weights, MSC's mean spectrum, 0/1 terms, cross and square terms — is computed again on the other observations; transformations and stored weights stay fixed. The sub-model has the same number of components (fewer if its observations have no variation left) and no cross-validation of its own.
  - PLS, PLS-DA: PRESS is the sum over the left-out observations of ((y − ŷ)·w)², w being the full model's scaling weight of y, so it is in the full model's scaled units; Q2(cum) = 1 − PRESS(a)/SS, Q2 = 1 − PRESS(a)/PRESS(a−1) with PRESS(0) = SS. OPLS: the same, the sub-model an OPLS with the same predictive and orthogonal components; row r of the components table is predicted with min(r, Ap) predictive components after max(0, r − Ap) orthogonal ones. PCA: each cell is predicted from its observation's scores computed without the cell's group of variables (variables also go to 7 groups in turn), treated as missing values.
  - Against the stored Q2(cum), the median absolute difference is 0.003 for PLS models with more than 200 observations, 0.02 from 50 to 200, 0.11 from 20 to 50 and 0.27 up to 20 (Nuée's Q2 is lower in 84 to 100 % of these models below 200 observations), and a few hundredths for PCA. 18 of the 944 models lose more than 1: they have a variable that varies on about one observation, which the scaling of the other observations blows up.
  - The stored values come from a validation done on the matrices scaled with all the observations (PLS: component by component, on the full model's residuals). At small N that inflates Q2: on pure noise with 20 variables, it gave a median Q2(cum) of 0.98 for an automatic OPLS-DA of 6 observations and 0.45 for a 2-component PLS; the left-out row of a centred matrix is minus the sum of the training rows, so enough components "predict" it (a 6-observation PLS with 5 components had Q2 = 1.000). Nuée's medians are now all below 0 (`verifier_validation.R`).
- **OPLS:** none of the projects has an OPLS model, so it is checked against theory and against a second writing of the published algorithm (`verifier_opls.R`), not stored results. With one predictive component its fitted values equal PLS's to 1e-14, and its scores are orthogonal. With several (several Y, or OPLS-DA with more than two classes), each orthogonal weight is the first loading made orthogonal to the space of the weights X'y of each column of Y (Trygg and Wold, 2002, OPLS for several Y), so the orthogonal scores are unrelated to Y; on complete data they match the second writing to 1e-8 and To'Y = 0. (0.8.0 made them orthogonal to the PLS weights instead, which left the orthogonal scores correlated with Y, up to a cosine of 0.1 on the example data.)
- **Standalone R scripts:** every model of the exported `.usp` projects under 20 MB (1,583 models from 207 files: 157 PCA, 926 PLS, 500 PLS-DA; 1,181 with missing values, 64 with cross terms) was exported. Each script, run without the engine, gives the engine's R2/Q2, scores, T2, DModX, limits, VIP, coefficients, CV-ANOVA and classification to 1e-8. That is the check `ecrire_script()` runs before writing.
- **Transform suggestions:** the published rule doesn't say whether both the skewness and the Min/Max criteria are needed; the app requires both.
- **Autofit:** the published rules, applied to Nuée's Q2 and Q2V, so the number of components can differ (the example's PLS now gets 7 instead of 4). PCA's significance limit isn't published, so the engine uses a degrees-of-freedom limit instead.
- **Rank:** a component is refused when what is left of X (or of Y) is below 1e-12 of its sum of squares at the start: a model can't have more components than its data's rank ("Component 5 is not possible…" instead of components of R2X 1e-32, or a singular matrix).
- **HCA:** this is exact Ward linkage. Every pair of leaves joined first matches the stored tree, but its top merges differ.
- **Jack-knife intervals:** over the 7 sub-models of the cross-validation (each in its own scaled units, signs aligned with the full model); they differ from the stored ones.
- **VIP cut-off:** the app's own method. It takes the largest drop in the sorted VIPs near Wold's 0.8 rule of thumb. The usual guidance only says VIP > 1.

## Speed

On a 9-component PCA of 797 × 58 values with 28 % missing, fitting takes about 1.7 s, most of it
in the cross-validation (7 sub-models, each predicting its left-out observations once per group
of variables). PLS models of the same size take well under a second.
