# Developing Nuée

Every calculation is plain R in `moteur/R/`, commented in French, so that the person using the
application can read and correct it. The interface is an Electron window with dockable panels,
pop-out windows and Plotly plots, talking to a local R server.

## Layout

| Folder | What it holds |
|---|---|
| `moteur/R/` | The engine: preprocessing, NIPALS PCA/PLS with missing values, cross-validation, OPLS, statistics, prediction, HCA, new analyses |
| `moteur/serveur.R` | Local HTTP server (httpuv) answering the interface's JSON requests |
| `moteur/console.R` | The R console: log of engine calls, prompt in the application's session |
| `moteur/R/formats.R` | Reading and writing tables: text, Excel, R, SPSS, Stata, SAS, jamovi `.omv` (read and written here), dBase, ARFF |
| `moteur/R/spectres.R` | Spectral preprocessing: SNV, MSC, Savitzky–Golay |
| `moteur/tests/verifier_references.R` | Refits every model of a folder of exported `.usp` projects and compares with the results stored in them |
| `moteur/tests/verifier_formats.R` | Every export format written and read back; text files as spreadsheets write them; jamovi files compared with `jmvReadWrite` when it is installed |
| `moteur/tests/verifier_spectres.R` | Published Savitzky–Golay coefficients, exact derivatives of polynomials, SNV, MSC, and a PLS on made-up spectra |
| `app/` | Electron main process, preload, interface (`app/interface/`), build script |
| `app/tests/essai_api.js` | Check run before each build |
| `app/icone/` | The logo, drawn by `dessiner_icone.py` |
| `exemple/` | The example project (made-up data) and `creer_exemple.js`, which makes it |
| `convertisseur/` | `.usp` to JSON converter; `usp_extract.py` is also used standalone to extract tables and model results |

## Running from the sources

Requires R 4.6 with `jsonlite`, `httpuv`, `openxlsx`, `haven` and `readxl` (and `foreign`, one of R's
recommended packages), Node, and Python 3 for the converter.

```bash
cd app && npm install && npm run vendor
Rscript moteur/serveur.R 8765 app/interface   # then open http://127.0.0.1:8765/?ouvrir=<path>
npx electron app                               # or the desktop window, which starts R itself
```

`node exemple/creer_exemple.js` rebuilds the example project after an engine change.

Check the file formats and the spectral preprocessing (`jmvReadWrite`, if installed, is only used
to compare the jamovi reader and writer with another implementation):

```bash
Rscript moteur/tests/verifier_formats.R
Rscript moteur/tests/verifier_spectres.R
```

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

The build first runs `app/tests/essai_api.js`. It syntax-checks the interface files, then starts
the R server and runs the main requests on a small invented table: open, fit PCA/PLS/OPLS/PLS-DA,
refit, predict, edit, undo, notes, save with session and reopen, R script, console, example
project, paste, export to every format and reopen, a workbook with two sheets. Any failure stops
the build. `SANS_ESSAI=1` skips it, and `node tests/essai_api.js` runs
it alone.

It then produces two things.

**`Nuee/`**, a folder that runs without installing anything, for example from a network share:

- `application/`: Electron (French and English only), a portable R with the packages the engine
  needs plus R's recommended ones (copied from the local installation, without manuals, Tcl/Tk
  or other message languages), the engine, the example project, and the converter compiled with
  PyInstaller.
- `Lancer Nuee.cmd`: the launcher.
- `LISEZ-MOI.txt`: the user guide, in French.

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
network-share copy or when running from the sources).

Settings live in `%APPDATA%\Nuee`; `--user-data-dir=<folder>` uses another one (a clean
profile for tests or screenshots).

## How it matches the stored results

Checked on 944 fitted models from 133 distinct `.usp` projects (file versions v10, v11 and
v16/17). Thresholds are 1e-4 for the fit and 1e-3 relative for derived statistics.

- **Scaled data, rebuilt from raw values:** all 944 match.
  - UV / Pareto / centring weights are computed over the workset observations.
  - The power transform is supported.
  - Qualitative variables expand into 0/1 terms.
  - A cross term is the product of its already-scaled factors, then scaled again.
- **PLS, complete data:** R2X, R2Y and scores match in 222/222 models, Q2 in 221/222.
  - Cross-validation uses 7 groups, assigned in turn (observation i goes to group i mod 7).
  - Q2 is partial cross-validation on the full model's residuals.
- **With missing values:** 596–623 of 633 PLS models match, depending on the statistic.
  - NIPALS skips missing cells.
  - The correction factor is capped at 3.
- **Derived statistics:**
  - Eigenvalue (= R2X·min(N, K)), T2, T2Crit, DCrit, VIP, coefficients and RMSEE match in every complete-data model.
  - The stored absolute DModX, and the weighted DModX, match in 264 of 267 complete-data models.
- **Signs:** models read from a `.usp` are oriented like the stored solution.

Known differences:

- **PCA Q2:** the stored values include an unpublished step.
  - The Krzanowski method used here gives a median largest difference over components of 0.015 to 0.023 against the stored Q2(cum). It is about 0.01 for v10 models and 0.12 for v11.
  - Four other schemes did worse on all 149 PCA models: one-step projections from the full component, a single NIPALS step, and Wold's element-wise deletion in two patterns.
- **OPLS:** none of the projects has an OPLS model, so it is checked against theory, not stored results. Its fitted values equal PLS's to 1e-14, and its scores are orthogonal. The cross-validation is full: the whole model is refitted without each group of observations.
- **Transform suggestions:** the published rule doesn't say whether both the skewness and the Min/Max criteria are needed; the app requires both.
- **Autofit:** PCA's significance limit isn't published, so the engine uses a degrees-of-freedom limit instead.
- **HCA:** this is exact Ward linkage. Every pair of leaves joined first matches the stored tree, but its top merges differ.
- **Jack-knife intervals:** close to the stored ones, not identical.
- **VIP cut-off:** the app's own method. It takes the largest drop in the sorted VIPs near Wold's 0.8 rule of thumb. The usual guidance only says VIP > 1.

## Speed

On a 9-component PCA of 807 × 57 values with 27 % missing, fitting takes about 3.7 s, almost all
of it in the matrix products of the cross-validation (15 NIPALS runs per component). PLS models
of the same size take well under a second.
