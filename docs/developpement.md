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
| `moteur/R/spectres.R` | Spectral preprocessing: SNV, MSC, detrend, baseline (lower convex hull), Savitzky–Golay (each range of neighbouring variables on its own) |
| `moteur/R/jeu_test.R` | Test set: left out of the model, predicted after fitting (RMSEP, bias, SEP, external Q2, classification); chosen at random or by Kennard–Stone |
| `moteur/R/langue.R` | `traduire("français", "English")`: the engine's messages in the interface's language (the `X-Langue` header of each request) |
| `moteur/R/fichiers_spectres.R` | Spectra files: JCAMP-DX (AFFN, PAC, SQZ, DIF, DUP), SPC (new and old format), text; merged into one table, with a table of sample information joined by file name |
| `moteur/R/script.R` | Export to R: a standalone script or R Markdown / Quarto report holding the data, the engine functions a model needs (found by following the names each function uses, and copied with their comments) and the model |
| `moteur/R/methodes.R` | The Methods paragraph and its references, in French or English |
| `moteur/R/comparaisons.R` | Tests by variable (t, Mann–Whitney, ANOVA, Kruskal–Wallis) with multiple-testing correction |
| `moteur/R/modeles_classe.R` | Class models (one PCA per class) and the distances behind the Coomans plot |
| `moteur/tests/verifier_references.R` | Refits every model of a folder of exported `.usp` projects and compares with the results stored in them |
| `moteur/tests/verifier_formats.R` | Every export format written and read back; text files as spreadsheets write them; jamovi files compared with `jmvReadWrite` when it is installed |
| `moteur/tests/verifier_spectres.R` | Published Savitzky–Golay coefficients, exact derivatives of polynomials, SNV, MSC, detrend, baseline, Savitzky–Golay by range, and a PLS on made-up spectra; the same results as `prospectr` when it is installed |
| `moteur/tests/verifier_fichiers_spectres.R` | Small made-up JCAMP-DX (every compression form), SPC (each layout) and text files read back; the spectra table and the joined information |
| `moteur/tests/verifier_jeu_test.R` | The test set: the model doesn't see it, RMSEP / bias / SEP / Q2ext recomputed by hand, PLS-DA classification, random and Kennard–Stone choice (the same observations as `prospectr::kenStone` when installed) |
| `moteur/tests/verifier_opls.R` | OPLS against a second, plain-matrix writing of Trygg and Wold's algorithm (one Y, two Y, three classes); orthogonal scores unrelated to Y |
| `moteur/tests/verifier_script.R` | Scripts and reports written for many kinds of models, run in a fresh `Rscript --vanilla` without the engine, give the engine's results; reports compile with `knitr` when it is installed |
| `moteur/tests/verifier_comparaisons.R` | Tests by variable against R's own test functions, class models, OPLS with several predictive components, the Methods text |
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

Check the file formats, the spectral preprocessing, the export to R, the tests by variable and
the class models (`jmvReadWrite`, `prospectr` and `knitr`, if installed, are only used to
compare with other implementations and to compile the reports):

```bash
Rscript moteur/tests/verifier_formats.R
Rscript moteur/tests/verifier_spectres.R
Rscript moteur/tests/verifier_fichiers_spectres.R
Rscript moteur/tests/verifier_opls.R
Rscript moteur/tests/verifier_jeu_test.R
Rscript moteur/tests/verifier_script.R
Rscript moteur/tests/verifier_comparaisons.R
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
refit, predict, edit, undo, notes, save with session and reopen, standalone R script with its
options, console, example project, tests by variable, Methods text, class models, an OPLS-DA
with four classes, paste, export to every format and reopen, a workbook with two sheets. Any failure stops
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
match, unsaved projects are saved or left as the user says, R is stopped, and a hidden
`cmd.exe` waits two seconds, runs the installer silently (`/S`, `/currentuser`, or `/allusers`
when Nuée is in Program Files, which Windows asks to allow; `/D=` the current folder), then
starts Nuée again with the same `--user-data-dir`. That `cmd.exe` is started by a go-between
that ends at once (`Nuee.exe` with `ELECTRON_RUN_AS_NODE`), so it isn't in Nuée's process
tree: the installer closes `Nuee.exe` with everything it started, and in one test out of three
it killed a `cmd.exe` started directly by Nuée, before installing anything. The installer
removes the old version first (keeping the settings) and keeps each file's original date; the
next start deletes the downloaded installer.

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
  converter and `LISEZ-MOI.txt`.
- **`installateur/`**, made from that folder with electron-builder:
  - `Nuee-<version>-amd64.deb`, which depends on `r-base-core` and `r-cran-httpuv`,
    `r-cran-jsonlite`, `r-cran-openxlsx`, `r-cran-haven`, `r-cran-readxl` (recommends `python3`),
    installs into `/opt/Nuée`, adds `nuee.desktop` to the applications menu and, on Ubuntu 24.04
    and later, the AppArmor profile Electron's sandbox needs;
  - `Nuee-<version>-x86_64.AppImage` and `Nuee-<version>-x64.tar.gz`, for other distributions,
    with R and the packages installed apart.

  `SANS_INSTALLATEUR=1` skips them. Building the `.deb` needs `fpm`, which electron-builder
  downloads; on NixOS, where its `#!/bin/bash` doesn't run, set `USE_SYSTEM_FPM=true` with
  `fpm` installed.

`package.json` names the desktop file (`desktopName`), so the windows' app id (`nuee`) matches
the menu entry and docks group them under its icon. The tests run the same way:
`node tests/essai_api.js` and `Rscript moteur/tests/verifier_*.R` find `Rscript` on the `PATH`.

## Checks on GitHub

`.github/workflows/verifications.yml` runs the engine's checks (`moteur/tests/verifier_*.R`,
except `verifier_references.R`, whose exported projects aren't in the repository) and
`app/tests/essai_api.js` on every push and pull request, on Windows with the current R.

## Releasing

1. The version in `app/package.json` (and `package-lock.json`) and in `CITATION.cff`.
2. Its entry at the top of `app/interface/nouveautes.js` (French and English): the build
   writes `CHANGELOG.md` from it (`node app/changelog.js` does it alone).
3. `node app/construire.js <folder>`, then the installer's checks.
4. A GitHub release `v<version>` with `Nuee-Setup-<version>.exe`: installed copies offer it as a
   one-click update. With the repository switched on at zenodo.org, each release also gets a DOI.

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
- **OPLS:** none of the projects has an OPLS model, so it is checked against theory and against a second writing of the published algorithm (`verifier_opls.R`), not stored results. With one predictive component its fitted values equal PLS's to 1e-14, and its scores are orthogonal. With several (several Y, or OPLS-DA with more than two classes), each orthogonal weight is the first loading made orthogonal to the space of the weights X'y of each column of Y (Trygg and Wold, 2002, OPLS for several Y), so the orthogonal scores are unrelated to Y; on complete data they match the second writing to 1e-8 and To'Y = 0. (0.8.0 made them orthogonal to the PLS weights instead, which left the orthogonal scores correlated with Y, up to a cosine of 0.1 on the example data.) The cross-validation is full: the whole model is refitted without each group of observations.
- **Standalone R scripts:** for 61 models sampled from the 133 projects (PCA, PLS, PLS-DA), the exported script, run in a fresh R session without the engine, gives the same scores and R2/Q2 to 1e-10.
- **Transform suggestions:** the published rule doesn't say whether both the skewness and the Min/Max criteria are needed; the app requires both.
- **Autofit:** PCA's significance limit isn't published, so the engine uses a degrees-of-freedom limit instead.
- **HCA:** this is exact Ward linkage. Every pair of leaves joined first matches the stored tree, but its top merges differ.
- **Jack-knife intervals:** close to the stored ones, not identical.
- **VIP cut-off:** the app's own method. It takes the largest drop in the sorted VIPs near Wold's 0.8 rule of thumb. The usual guidance only says VIP > 1.

## Speed

On a 9-component PCA of 807 × 57 values with 27 % missing, fitting takes about 3.7 s, almost all
of it in the matrix products of the cross-validation (15 NIPALS runs per component). PLS models
of the same size take well under a second.
