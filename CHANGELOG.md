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
