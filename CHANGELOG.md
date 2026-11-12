# Changes

What changed in each version: the same list as *Help › What's new* in the application
(in French: *Aide › Quoi de neuf*). Downloads: [Releases](https://github.com/4rtefakt/nuee/releases).

## 0.9.5 — 2026-11-12

- AIC selection: under each regression (and in Plots), Nuée compares by AICc every combination of the model’s groups of terms (its blocks, otherwise its variables), from the null model to the full one, each fitted by ML on the same observations, with the model’s random effects, autocorrelation, effort and curves; a cross term enters only with its main effects. Beyond 8 groups, a backward elimination. The view suggests the best model and, when it differs, the most parsimonious within ΔAICc ≤ 2; it shows each group’s importance and the best models with their Akaike weights.
- “Fit this model”, on the suggestion or on any row, makes it a new model: the same settings with only those groups’ variables, on the same observations, fitted like the others. The Methods text then describes the selection, the model report can include it and Export to R redoes it.
- Paired tests: in Plots › Statistical tests, Two variables mode, a measurement and a qualitative variable (or an ID) can be paired by a third one (“Pair by”: the year). Two levels A and B, two sectors of a site for instance, are compared on the years both have, the data staying in long format (one row per sector and year): paired t-test (mean difference B − A, its 95 % interval, Cohen’s d_z) and Wilcoxon’s signed-rank test (exact below 50 pairs with no zero or tied difference; otherwise, zero differences dropped, by the normal approximation with continuity correction, the same calculation on every R version), recommended when the normality of the differences is doubtful. The number of pairs and the rows left without a partner are given, the result as a sentence ready for a report, and the plot joins each pair.
- A two-variable test’s sentence follows the interface language: switching language rewrites it.
- Paired tests of two measurements of the same row (before and after, say): in Plots › Statistical tests, Two variables mode, two quantitative variables can be paired by “the observation (same row)”. The differences Variable 2 − Variable 1, on the observations where both are known, go through the paired t-test and Wilcoxon’s signed-rank test, as for two levels paired by a key.
- Every pair of levels: when a measurement is paired by a key between the levels of a variable that has three or more (sectors, by the year), the table below can list each pair of levels, on its own shared years, with the difference, its 95 % interval and the chosen test’s Holm-corrected p-values; a click on a pair compares it. When all the levels share the same years, these are the values of pairwise.t.test() and pairwise.wilcox.test() with paired = TRUE. With “Pairs of levels within each” (a qualitative variable or an ID: the site), the table keeps only the pairs of levels of the same group, the sectors of each site compared two by two; Holm’s correction covers the pairs tested, all sites together, and the raw p-value reads one site at a time. A sector whose rows are in several sites is refused.
- The Mann–Whitney sentence names both groups with their medians, so the sign of the rank-biserial correlation reads.
- Mann–Whitney’s test (between classes, two variables, BACI) does the same calculation on every R version: exact below 50 values on each side with no ties; otherwise by the normal approximation with continuity correction, as R did before 4.6. With ties, its p-value therefore changes slightly from 0.9.3 on Windows, where R 4.6 computed it exactly.
- Before/after with a control (BACI): a fourth mode of Plots › Statistical tests. A response, a time (the year) and a unit (the sector), whose impact and control units are ticked, then the event, the first “after” time, with an optional end. At each time, the gap d_t between the mean of the impact and that of the controls; the gaps after compared with those before by Welch’s t, Student’s t or Mann–Whitney, with the change in the mean gap and its 95 % interval, as a sentence ready for a report. The plot shows the gaps over time, the event and each period’s mean.
- Two classes paired, variable by variable: in the Between classes mode, “Pair by” a key (the year, for two sectors followed over the same years) compares each variable on the keys both classes have, by the paired t-test or Wilcoxon’s signed-rank test, with the number of pairs, log2 of the ratio of the means or Cohen’s d_z, the volcano plot and the Benjamini–Hochberg or Bonferroni corrections. A key found twice in a class is refused.
- Values from another year: in a New variable formula, previous(x, year, sector) gives the value of x the year before in the same sector and following(x, year, sector) that of the year after (pas = 2: two years); growth_rate(x, year, sector) gives the growth rate log(x / x of the year before), missing unless both values are positive (precedent, suivant and croissance in French). A missing year gives a missing value, never that of an older year; the group can be interaction(site, sector), and the same sector twice in one year is refused, naming it. This lines up year n with year n − 1, or shows density dependence: the growth rate against the previous year’s density.
- Series: Plots › Series (or in the explorer, under the project) shows a variable over time, one line per sector and, optionally, one small plot per site, in a grid with shared axes. A missing year breaks the line; optionally, the mean of each panel’s lines, a log scale and events (“2015; 2019”) as dashed lines in every panel. Colours go by line within each panel and the legend by panel, readable with 30 sectors; hovering gives the observation, the time and the value, a click or a box selects observations (in the project’s models too, when the selection is shared), and the image is copied or saved like the others.
- Residual autocorrelation: for a regression, Plots › Residual autocorrelation shows the autocorrelation of its residuals in time within each group, the groups pooled as nlme::ACF does, with the band ±1.96/√n of each lag. A model with CAR1 shows its residuals without the autocorrelation and its normalized residuals, which show none left when CAR1 is enough; for another model, the time and the groups are chosen (by default those of its random effects), and beyond the band at lag 1 the note suggests a CAR1 autocorrelation. Pearson or deviance residuals for a GLM or a GAM; the model report can include the plot.
- Nested models: under the AIC table of Prediction › Compare models, the likelihood-ratio test between each pair of regressions of the same AIC set whose smaller model has its fixed terms among the bigger one’s, with the same distribution, random effects and autocorrelation: Δdf, χ² and p from the ML fits, as nlme’s anova(m1, m2). Random effects and autocorrelation are compared by AIC; with curves, the test is marked approximate.
- Fixed: the views that keep results computed from a project’s data (data, variable statistics, statistical tests) compute them again when its data change, in other windows too. Until now, an open test kept its result after an edit or a new variable.
- Fixed: the normalized residuals of a model with a CAR1 autocorrelation (Residuals view, exports, Export to R) were wrong when the table’s rows were not sorted by group, then by time, in year order for instance: nlme gets them right only in that order. They are now computed in each group’s time order; the fit itself (coefficients, φ, AIC) was not affected.

## 0.9.3 — 2026-11-12

- Redundancy analysis (RDA): New model › type “RDA”. Responses Y (species abundances, measurements) explained by variables X, optionally with covariates Z whose effect is removed first (partial RDA); responses transformed by Hellinger, chord or log(x + 1), centred or standardized; a triplot in scaling 1 or 2 (arrows of the X, centroids of the categories) and the adjusted R²; permutation tests of the model, of each axis and of each term, within a site if needed, with a fixed seed; forward selection (Blanchet’s double criterion) and variation partitioning between blocks of variables. The results are those of the vegan package.
- Correspondence analysis (CA) of a table of counts or abundances: rows and columns in scaling 1, 2 or 3, eigenvalues, contributions and cos².
- RDA and CA have their Methods text with its references, their export tables and their PDF or Word report; File › Export to R writes a script or an R Markdown / Quarto report that redoes them with R alone.
- Decision trees (rpart): New model › type “Decision tree”, a regression or classification tree on the raw X (readable thresholds, missing values handled by surrogate variables), pruned by the 1-SE rule with an internal cross-validation by the groups (a site) and validated by predicting each group with a tree rebuilt without it (Q2, RMSEcv); the tree drawn, pruning, leaf rules, variable importance, first-split stability over 200 bootstrap draws of the groups.
- Linear discriminant analysis (LDA, MASS::lda): discriminant scores, structure correlations and coefficients, classification cross-validated by groups, posterior probabilities; prior probabilities proportional or equal.
- k-means: k clusters on the scaled variables (25 starts, seed 1), shown on the first two components, silhouettes, choice of k from 2 to 10, centres in the variables’ units; “Clusters → variable” makes them a qualitative variable of the project.
- These three analyses have their Methods text, tables, PDF or Word report and Export to R (rpart, MASS, cluster); the tree and the LDA predict a test set and new observations, and k-means assigns a new observation to the nearest cluster.
- Autocorrelation in time in additive and generalized models: with the normal distribution and random effects, a GAM or GLM takes the CAR1 autocorrelation and is fitted by mgcv::gamm, as gamm(y ~ s(x, k = 5) + z, random = list(site = ~1), correlation = corCAR1(form = ~ year | site/sector)). Coefficients, curves, variances and φ by REML, AIC by ML, marginal and conditional R²; Export to R refits it.
- The autocorrelation can run within a finer level than the last random effect (“within each”: the sector within its site, only the site having a random effect), in a mixed model too.
- A curve’s basis dimension (k): in the box beside its tick, Curve column, or s(x, k = 5) in the R formula.
- A curve’s thresholds: in an additive model’s Effects view, the Thresholds box shows the curve’s effect and slope with 95 % simultaneous intervals (2000 draws of the coefficients, as in Simpson 2018), shades and lists the ranges where the slope clearly rises or falls, and gives the edf (below 1.5, nearly a straight line). The model report includes them, and Export to R computes them with the same draws.
- Concurvity: an additive model’s Summary gives for each curve the share the other terms reproduce (mgcv::concurvity), in red above 0.8: the curve is then confounded with them.
- Variables by group: in a New variable formula, group_mean(x, sector) gives the mean of x in each observation’s group and group_deviation(x, sector) the deviation from it (moyenne_groupe and ecart_groupe in French): a sector’s mean as a covariate, or a response centred by sector. Observation IDs (site, sector…) can be used there too.
- Edit › New variable › Imputation (PCA): the missing values of the chosen quantitative variables, filled from each other by regularized iterative PCA, as missMDA::imputePCA does, with the number of components chosen by generalized cross-validation of the standardized variables (missMDA::estim_ncpPCA) unless you give it; observed values don’t change, and an observation missing more than half of these variables stays missing. As new variables “x (imputed)”, or in the variables themselves (Ctrl+Z undoes it).
- Cross terms: each has its own scaling, block and modifier, in the Cross terms tab. Edit model keeps them; until now it lost those of the cross terms (of a model imported from a .usp file, for instance), and refitting changed their weighting.
- One-click update: an installation for all users placed outside Program Files was updated as a second copy, for the current user only; Nuée now reads its installation mode from Windows.
- Linux: the .deb also installs the R packages MASS, rpart and cluster; with the AppImage or the .tar.gz archive, add them to the packages you install in R.

## 0.9.2 — 2026-11-12

- Regression models, next to PLS: New model › type “Linear regression” or “Linear mixed model” (nlme), built from the Variables tab like the other models. A mixed model takes nested random effects (the site, then the sector within the site: an observation ID or a qualitative variable) and, optionally, an autocorrelation in time (CAR1, continuous time). Coefficients and variances by REML, AIC by ML; marginal and conditional R²; cross-validation defaults to the first random effect, each site predicted as a site never seen.
- Generalized linear and additive models (mgcv): the distribution of Y (Poisson or negative binomial for counts, binomial for presence-absence, gamma for positive values, Tweedie for densities with zeros, or normal), an effort as an offset, random effects (GLMM, GAMM), and in an additive model curves for the variables ticked in the Curve column of the Variables tab: an effect that rises then falls, or a threshold. A Poisson model warns when its counts are overdispersed.
- A regression’s results: Summary (R², deviance explained, Q², AIC, variances, curves), Coefficients (95 % intervals; the multiplicative effect or odds ratio with a log or logit link), Effects (each variable’s effect with its 95 % band, an additive model’s curves), Residuals, and Block contributions: the AIC change when each block of variables is removed, with a likelihood-ratio test.
- Prediction › Compare models: AIC, ΔAIC and Akaike weights (the probability that each model is the best of its set) between the regressions of the same response on the same observations whose likelihoods compare, next to the cross-validated Q² of every model, PLS included.
- R formula (New model › General): the model written as in R, for instance y ~ a * b + log10(c) + s(d) + offset(log(effort)) + (1 | site/sector); edited and applied (Ctrl+Enter), it sets the tabs. R reads it without ever running it, and it isn’t kept: a project someone sends can’t carry code.
- Regressions predict (observations, a prediction set or a file, its effort included; an extrapolation is flagged), export (File › Export), make a PDF or Word report (File › Model report), and File › Export to R writes a script or an R Markdown / Quarto report that refits them with lm, nlme or mgcv.
- Plots › Statistical tests (formerly Tests by variable), with two new modes. Two variables: the test their types call for, its conditions checked (chi-square or Fisher’s exact test; Welch’s t, ANOVA or Kruskal–Wallis with pairwise comparisons; Pearson’s or Spearman’s correlation), and its result as a sentence ready for a report, with its effect size; an observation ID (site, year…) can make the groups.
- Correlation matrix: Pearson or Spearman for every pair of quantitative variables, p-values corrected for the number of pairs; a click on a pair tests it in Two variables.
- Linux: the .deb also installs the R packages nlme and mgcv; with the AppImage or the .tar.gz archive, add them to the packages you install in R.

## 0.9.1 — 2026-11-11

- One-click update: when the status bar mentions a new version, a click downloads its installer, checks it (SHA-256 checksum given by GitHub), offers to save your projects, installs it and starts Nuée again.
- Test set: in New model › Observations, observations left out of the model and of the cross-validation (by hand, at random or by Kennard–Stone, by class for a PLS-DA), then predicted: RMSEP, bias, SEP, external Q2, or the classification of a PLS-DA. Observed / predicted shows them; the Methods text and Export to R include them.
- Projects are saved as .nuee: double-clicking one in Windows Explorer opens it in Nuée. .json projects still open.
- Dropping files on the window opens them; JCAMP-DX or SPC spectra dropped together make one project.
- The R engine’s messages (errors, notes) are in English when the interface is, and so are exported scripts in English.
- The full user guide in English (USER-GUIDE.txt); Help › User guide › Full guide opens the one in the interface’s language.
- The Methods text cites Nuée and its version; GitHub’s “Cite this repository” button cites it too.
- Nuée also runs on Linux: a .deb package (Debian, Ubuntu and derivatives), an AppImage or a .tar.gz archive, with the computer’s R.
- Highlighted variables: clicking a variable (VIP, coefficients, loadings, biplot, S-plot, profiles, variable list) highlights it in all the model’s plots. On the VIP plot, keeping or dropping a variable is now Ctrl+click.
- Plots › iPLS: each interval of the spectrum fitted on its own, to see which ranges predict best; then a new model with them, or without them.
- File › Export to R: the whole session in one script, the data once, then each model under its name.
- Ctrl+C copies the view’s table, ready to paste into Excel; View › Find a command (Ctrl+K) finds a command by typing its name.
- Help › Check for updates, and Help › Diagnostic information: the versions to paste into a report on GitHub (with forms for problems and ideas).
- A plot keeps its zoom when a selection redraws it.
- A model asking for more components than its data allow says so plainly (“Component 3 is not possible…”) instead of an internal R error; automatic fitting then stops at the last possible one.
- Models from .usp files keep their cross-validation groups: one group per value of an observation ID (site, sector, year…) when the file groups them so, or the file’s own groups, instead of 7 groups in turn. Their Q² now answers the question the model was built for (predicting a new site, a new sector…). New model can make such groups too: Cross-validation groups › one group per value of an ID or of a variable; the Methods text and Export to R say so.
- Block scaling and modifiers: models from .usp files that weight blocks of variables (1/√K, K the block’s number of terms) or multiply a variable’s weight by a modifier are refitted as stored; until now their R²X, R²Y, scores and everything after were wrong. New model has a Block and a Modifier column in the Variables tab; the Methods text and Export to R include them.
- New model › Variables, Transform › custom…: log10, ln or −log10 of (C1·x + C2), exp(C1·x + C2), C1·x + C2, a logit between two bounds, or the power (C1·x + C2)^C3, with the formula shown as it will be applied; the Methods text and Export to R include them. A power stored in a .usp file other than the square or fourth root is no longer dropped by Edit model.
- New model › Cross terms: terms of three variables (A × B × C) and cubes (A × A × A), as .usp files have them; Edit model shows them in full.
- Cross-validation: each group is now predicted by a model fitted without it, its centring and scaling included (and MSC’s mean spectrum). Until now these models kept the scaling computed with all the observations, which inflated Q² when there are few of them: on random data, an OPLS-DA of 6 observations had a Q² of 0.98 (0.71 with 8) and a 5-component PLS 1.00; in median, they now stay below 0. Every model’s Q² changes a little, a lot with few observations, and so do RMSEcv, CV-ANOVA, the cross-validated classification and the number of components chosen automatically; the Q² of a model imported from a .usp file no longer equals the one stored in it.
- Security: the R engine now answers only Nuée’s own windows (a token drawn at each start). Until now, a web page open in a browser, or another user of the same computer, could make it run code. The windows no longer go to another site, and a file received from someone (a project, an R file, an export to R) can no longer run code or choose where Nuée saves. R files (.rds, .RData) open only with R 4.4 or newer, as on Windows: with an older R (Ubuntu, Debian), reading one could run code (CVE-2024-27322), and Nuée refuses it.
- Saving never leaves a project half-written (network cut, full disk); if the file was changed elsewhere since it was opened (a colleague, another window), Nuée asks before overwriting it. The file records which versions of Nuée and R wrote it.
- Recovery copies are kept until they are recovered or set aside, and never deleted when the R engine stops answering. Restarting the R engine reopens each project from the file it saves to, or from its recovery copy, pasted tables and imported spectra included. Opening a file that is already open shows the project instead of making a second copy.
- Field data: a text cell (<0.5, <LOD, nd, #N/A…) in a column of measurements is read as missing, and Nuée says so, instead of turning the column into a qualitative variable or dropping it; the same in a file to predict, which gave wrong predictions until now. Excel dates stay dates, column names keep their spaces, and a malformed CSV line is reported instead of shifting the columns.
- R console: each fitted model shows there with its code and what R prints of it; Tab completes names (objects, m$…, arguments); the cursor stays in the prompt, and shortcuts (Ctrl+S, Ctrl+K…) work there. The application’s actions didn’t appear once the console was open: fixed.
- New model stays open while the model is fitted: an error shows in it, with every setting kept.
- Export to R: the script now holds only the exported model’s calculation, written for it alone (scaling, NIPALS, cross-validation, statistics, plots), in plain R with no package: a PLS goes from over 1,000 lines of code to about 170. It still gives Nuée’s results, checked before writing; an English export also translates the names in the code.
- Spectra: a malformed JCAMP-DX, SPC or text file is refused at once, with its name, instead of freezing Nuée or exhausting its memory; a file with many labels reads much faster.
- Data: a very wide table (spectra) shows its first 200 columns, or those whose name holds the text typed; the loadings and the S-plot write variable names only up to 100 variables.
- For IT departments: silent installation, a Group Policy that turns the update check off, what Nuée writes where and sends over the network (docs/deploiement.md). An installed copy ignores the user’s .Rprofile and loads its own packages first; R’s temporary files are deleted when Nuée closes.

## 0.9.0 — 2026-11-01

- File › Import spectra: JCAMP-DX, SPC (Galactic, Thermo) or text files, one spectrum or several per file, made into one project; a table of information (contents, classes) can be joined to them by file name. A JCAMP-DX or SPC file also opens with File › Open.
- Spectral preprocessing: detrend and baseline (convex hull) added, up to three steps.
- Plots › Profiles (spectra) › Choose ranges: drag across the spectra to mark wavelength ranges, then a new model without them or with them only. Savitzky–Golay treats each range on its own.
- OPLS with several Y and OPLS-DA with more than two classes: the orthogonal components are no longer related to Y at all, as Trygg and Wold (2002) intend. These models change a little when refitted.
- R Markdown and Quarto reports checked through to HTML; they now declare their language.

## 0.8.0 — 2026-11-01

- File › Export to R: a script, or an R Markdown or Quarto report, that refits the model without Nuée, in plain R (the engine’s code is copied into it). Data in the script or in a CSV next to it; optionally also renv.lock, results as .RData, an RStudio project.
- Model › Text for the Methods section: a paragraph written from the model, in French or English, with its references.
- Plots › Tests by variable: each variable compared between two classes (Welch’s or Student’s t, Mann–Whitney) or across all of them (ANOVA, Kruskal–Wallis), with the Benjamini–Hochberg correction and a volcano plot.
- New model › Class models: one PCA per class, and the Coomans plot to tell which class an observation belongs to (or none).
- OPLS with several Y and OPLS-DA with more than two classes (several predictive components).
- Exported files checked in jamovi, PSPP (SPSS), Weka, LibreOffice and pandas: jamovi now opens the .omv files; SAS transport in version 5, which SAS reads; dBase in Windows-1252, missing values left blank.

## 0.7.0 — 2026-11-01

- File › Open also reads R (.rds, .RData), SPSS, Stata, SAS, jamovi, dBase and Weka files, and old Excel workbooks (.xls). For a workbook with several sheets, the application asks which one to open.
- File › Paste a table (Ctrl+V): cells copied from a spreadsheet become a project.
- Export a project’s data and a model’s results to Excel, CSV, R, SPSS, Stata, SAS, jamovi, dBase or Weka.
- Spectra: SNV, MSC and Savitzky–Golay preprocessing (smoothing, derivatives) in New model, and the Profiles (spectra) view.
- Tab-separated text files, Windows-1252 files and Excel’s “Unicode text” now open correctly.
- The installed version mentions a new version at start-up (Help to turn it off).

## 0.6.0 — 2026-11-01

- Help › Example project: made-up data (120 apples) with four models ready to explore, to try Nuée.
- New icon, easier to read at small sizes; the logo on the welcome screen and in About.
- A lighter application (65 MB and a thousand files less): it copies and installs faster.
- Nuée is now free software (GPL-3.0 licence): github.com/4rtefakt/nuee.

## 0.5.0 — 2026-11-01

- The application is now called Nuée. Settings, preferences and recent projects carry over from the previous version.
- Nuée shows in the Start menu and in Settings › Installed apps, where it can be uninstalled.
- A new icon; the desktop shortcut is called “Nuée”.
- Help › Results and known differences: what the engine reproduces exactly from .usp files, and the known differences.

## 0.4.0 — 2026-11-01

- Closing with unsaved changes now offers “Save and close”.
- Automatic recovery copies of unsaved projects: after an unexpected stop, the application offers to recover them.
- If the R engine stops, the application says so and offers to restart it (also in Help › Restart the R engine).
- Edit › Undo (Ctrl+Z): reverse a data change, a new variable, a class…
- Removing a model asks for confirmation; a model made in the application can also leave the project.
- Notes: each model has a notes box in its summary, saved with the project and included in the report.
- R console: verifier_moteur() refits the imported models and compares them with the stored values.
- Settings (theme, language, plot format, console commands) are kept from one launch to the next.

## 0.3.0 — 2026-10-31

- One page per model: a central tab for each model, with its summary, scores and loadings.
- R console (View › R console, Ctrl+J): the R code of each action, and an R prompt.
- Saved projects keep the session: models, panels, plot settings, selections.
- Variable statistics, proposed transformations, histograms, missing-value map.
- OPLS and OPLS-DA; biplot, 3D scores, S-plot; DModXPS for predictions.
- Model report as PDF or Word; plot formatting for publication.
- Edit data, new variables (R formula, classes); history of each .usp project.
- Predict a file, hierarchical models, R script export, contributions, data checks before fitting.
