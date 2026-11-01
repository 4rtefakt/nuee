<p align="center"><img src="app/interface/nuee.svg" width="112" alt=""></p>

<h1 align="center">Nuée</h1>

<p align="center">
  <b>PCA, PLS, PLS-DA and OPLS on Windows, computed in plain R.</b><br>
  Interactive plots, models side by side, and every calculation open to read.
</p>

<p align="center">
  <img alt="Windows 10 and 11" src="https://img.shields.io/badge/Windows-10%20%7C%2011-1b4a9c">
  <img alt="Interface in French and English" src="https://img.shields.io/badge/interface-FR%20%7C%20EN-2f7dcd">
  <a href="LICENSE"><img alt="GPL-3.0" src="https://img.shields.io/badge/license-GPL--3.0-1b4a9c"></a>
</p>

![The example project in Nuée: a PCA with its summary, scores coloured by variety, and loadings](docs/captures/acp.png)

Nuée (French for *swarm*) is a desktop application for multivariate data analysis. Open a
table from Excel, R, SPSS, Stata, SAS or jamovi (or paste one), pick your variables, and get
the model with its scores, loadings, diagnostics and validation, in plots you can zoom, select
and colour. Send the data and the results back to any of those programs. Every calculation is
written in R, with comments, so you can check exactly what was computed, and change it.

The interface is in French and English (*Affichage › Langue* / *View › Language*); the
screenshots show it in French.

## Install

1. Download `Nuee-Setup-<version>.exe` from the [Releases](https://github.com/4rtefakt/nuee/releases) page.
2. Run it and choose to install for yourself (no administrator rights needed) or for everyone
   on the computer.
3. Windows may say it protected your PC, because the installer isn't signed: click
   *More info*, then *Run anyway*.

R is included: there is nothing else to install. Nuée appears in the Start menu, and can be
uninstalled from *Settings › Apps*. Your projects stay where you saved them. At start-up it asks
GitHub whether a newer version exists and shows a link in the status bar if so (nothing else is
sent; *Aide › Chercher une nouvelle version au démarrage* turns it off).

## First steps

- **Try the example:** *Aide › Projet d'exemple* (*Help › Example project*) opens 120 made-up
  apples (four varieties, organic or conventional, a dozen measurements and a tasting score)
  with four models ready: a PCA, a PLS predicting the score, a PLS-DA of the varieties and an
  OPLS-DA of the growing method.
- **Open your data:** *Fichier › Ouvrir* (Ctrl+O) reads a table with one row per observation
  and the observation names in the first column (see [Files](#files) for the formats), or
  *Fichier › Coller un tableau* (Ctrl+V) takes cells copied from any spreadsheet. Text columns
  and factors become qualitative variables you can colour by, or use as classes.
- **Make a model:** *Modèle › Nouveau modèle* (Ctrl+N): type, the role of each variable (X, Y
  or left out), scaling and transformation, observations to include. The number of components
  is chosen by cross-validation unless you set it.
- **Explore:** each model gets its own page with its summary, scores and loadings; add any
  other view from the *Graphiques* menu. Select points with a box or a lasso: the selection
  follows you in every plot of the model.

The full user guide, in French, comes with the application (*Aide › Guide d'utilisation*,
and [LISEZ-MOI-APPLICATION.txt](LISEZ-MOI-APPLICATION.txt)).

## What it does

**Methods:** PCA, PLS, PLS-DA, and OPLS / OPLS-DA, with missing values handled by NIPALS and
7-fold cross-validation (R2, Q2, and Q2 per variable).

**Validation:** permutation tests, CV-ANOVA, PLS-DA classification tables and ROC curves,
jack-knife confidence intervals.

![A PLS model: observed against predicted, and VIP with an automatic cut-off](docs/captures/pls.png)

**Variables:** VIP with a cut-off placed automatically (or dragged by hand), coefficients,
loadings, biplot, refit with the selected variables only. Variable statistics with suggested
log transformations, histograms and a map of missing values.

**Observations:** scores in 2D and 3D, Hotelling's T2 and DModX with their limits,
contribution plots explaining why an observation stands out, hierarchical clustering on the
scores, classes made from a selection.

![An OPLS-DA: the S-plot shows which variables separate the two classes](docs/captures/oplsda.png)

**Spectra:** SNV, MSC and Savitzky–Golay smoothing or derivatives before the model, and
loadings, coefficients, VIP or the spectra themselves drawn as lines against the wavelength.

![A PLS on made-up spectra after SNV and a first derivative: scores coloured by batch, and loadings p drawn as lines against the wavelength](docs/captures/spectres.png)

**Prediction:** apply a model to new observations from a table, with flags for those outside
the model's domain, and export the results.

**Your work:**
- saving a project keeps its models, the layout of your panels, plot settings and selections;
- a recovery copy every two minutes, offered back after a crash, and undo for data changes;
- notes on each model;
- data editing and new variables from R formulas or value ranges;
- several windows, and views that pop out onto a second screen;
- plots formatted for publication (fonts, palettes, size) and exported as PNG or SVG;
- a model report as PDF or Word.

## Files

| | Opens | Exports |
|---|---|---|
| Excel | `.xlsx`, `.xlsm`, `.xls` (choice of sheet) | `.xlsx` (one sheet per table) |
| Text | `.csv`, `.txt`, `.tsv`: separator, decimal comma and encoding detected | `.csv`, `.tsv` |
| R | `.rds`, `.RData` (choice of table; factors and row names kept) | `.rds`, `.RData` |
| SPSS | `.sav`, `.zsav`, `.por` (value labels become categories) | `.sav` |
| Stata | `.dta` | `.dta` |
| SAS | `.sas7bdat`, `.xpt` | `.xpt` |
| jamovi | `.omv` | `.omv` |
| dBase, Weka | `.dbf`, `.arff` | `.dbf`, `.arff` |
| Clipboard | cells copied from Excel, LibreOffice, Google Sheets, jamovi… | |

*Exporter les données* writes the project's table; *Exporter les résultats du modèle* (Ctrl+E)
writes the observations (scores, T2, DModX, fitted values), the variables (loadings, VIP),
the components and the coefficients. For SPSS, Stata and SAS, variable names are made valid and
the original name becomes the variable label.

## R all the way down

The engine is plain R in [`moteur/R/`](moteur/R), commented in French: preprocessing, NIPALS,
cross-validation, statistics, prediction. No statistic is computed anywhere else.

- **R console** (*Affichage › Console R*, Ctrl+J): see the R code behind each action, and run
  your own in the application's session, with `m` and `p` standing for the model and project
  on screen.
- **Export as R script:** any model becomes a commented script that rebuilds it with the
  engine's functions, to rerun or adapt in RStudio.
- **Fix it yourself:** edit a file of the engine, type `recharger_moteur()` in the console, and
  refit.

![Dark theme, with the R console under a PCA](docs/captures/console.png)

## Opening `.usp` projects

Nuée also opens `.usp` project files: their data, the models stored in them and the values
those models were saved with. It recomputes each model in R and marks every value of the
summary as identical, close or different. On 944 models from 133 projects, R2X, R2Y, the PLS
Q2, scores, loadings, T2, DModX, VIP and coefficients match the stored values, apart from a
few models with missing data; *Aide › Résultats et écarts connus* lists the known
differences.

## Building from source

See [docs/developpement.md](docs/developpement.md): running from the sources, the build
(installer and network-share version), the checks, and how the results were compared.

## License

Nuée is free software under the [GNU General Public License v3.0](LICENSE) or later.

The installer bundles R (GPL-2 | GPL-3) with its recommended packages and the R packages
httpuv, jsonlite, openxlsx, haven, readxl and their dependencies, Electron (MIT), Plotly.js (MIT),
dockview (MIT) and Lucide icons (ISC), each under its own license.
