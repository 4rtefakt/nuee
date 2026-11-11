# Changes

What changed in each version: the same list as *Help › What's new* in the application
(in French: *Aide › Quoi de neuf*). Downloads: [Releases](https://github.com/4rtefakt/nuee/releases).

## 1.0.0 — 2026-11-01

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
- New model › Variables, Transformation › custom…: log10, ln or −log10 of (C1·x + C2), exp(C1·x + C2), C1·x + C2, a logit between two bounds, or the power (C1·x + C2)^C3, with the formula shown as it will be applied; the Methods text and Export to R include them. A power stored in a .usp file other than the square or fourth root is no longer dropped by Edit model.
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
