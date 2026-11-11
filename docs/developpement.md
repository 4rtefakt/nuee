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
| `moteur/R/ipls.R` | iPLS (Nørgaard et al. 2000): the spectrum preprocessed once, whole, then cut into intervals, each a PLS on its range; RMSEcv for 1 to A components |
| `moteur/R/jeu_test.R` | Test set: left out of the model, predicted after fitting (RMSEP, bias, SEP, external Q2, classification); chosen at random or by Kennard–Stone |
| `moteur/R/langue.R` | `traduire("français", "English")`: the engine's messages in the interface's language (the `X-Langue` header of each request) |
| `moteur/R/fichiers_spectres.R` | Spectra files: JCAMP-DX (AFFN, PAC, SQZ, DIF, DUP), SPC (new and old format), text; merged into one table, with a table of sample information joined by file name |
| `moteur/R/script.R` | Export to R: a standalone script or R Markdown / Quarto report written for one model (or the session): the data, then only the steps that model uses (its terms and scaling, NIPALS, cross-validation, statistics, plots), in plain R with no package (a regression: its rows, terms and the same fits with `lm`, `nlme::lme` or `mgcv::gam`, cross-validation, statistics and plots); refitted and compared with the engine before it is written |
| `moteur/R/methodes.R` | The Methods paragraph and its references, in French or English |
| `moteur/R/regression.R` | Linear, generalized and mixed models: `lm` and `nlme::lme` (nested random effects, CAR1), REML estimates and ML likelihoods, marginal and conditional R2; `mgcv::gam` for GLMs, GLMMs and GAMs (normal, Poisson, negative binomial, binomial, gamma, Tweedie; an offset; smooth curves; random effects as `s(g, bs = "re")`), fitted by ML without a penalty and by REML with one, the AIC then from an ML refit; cross-validation by groups, block contributions by AIC, effect curves, Methods text |
| `moteur/R/formule.R` | The R formula of a regression: written from New model's settings, and read back into them with `str2lang` without ever being evaluated (only the project's variables and IDs, numbers and a few functions, each in its place) |
| `moteur/R/comparaisons.R` | Statistical tests: by variable (t, Mann–Whitney, ANOVA, Kruskal–Wallis) with multiple-testing correction; between two variables (chi-square or Fisher, groups with Tukey, Games–Howell or Dunn, correlations), the test chosen from their conditions; the correlation matrix |
| `moteur/R/modeles_classe.R` | Class models (one PCA per class) and the distances behind the Coomans plot |
| `moteur/tests/verifier_references.R` | Refits every model of a folder of exported `.usp` projects and compares with the results stored in them; for PLS models, also the files' own cross-validation (scaling from all the observations) with the groups read in the files, which gives back the stored Q2 when the groups are right |
| `moteur/tests/verifier_formats.R` | Every export format written and read back; text files as spreadsheets write them; jamovi files compared with `jmvReadWrite` when it is installed |
| `moteur/tests/verifier_spectres.R` | Published Savitzky–Golay coefficients, exact derivatives of polynomials, SNV, MSC, detrend, baseline, Savitzky–Golay by range, and a PLS on made-up spectra; the same results as `prospectr` when it is installed; data with no variation left: a clear message, and automatic fitting stops without error |
| `moteur/tests/verifier_fichiers_spectres.R` | Small made-up JCAMP-DX (every compression form), SPC (each layout) and text files read back; the spectra table and the joined information |
| `moteur/tests/verifier_ipls.R` | iPLS on made-up spectra with one informative band: intervals cover the spectrum in order, each RMSEcv equals that of a PLS on the range's columns of the whole preprocessed spectrum, the whole spectrum gives back the model, the band's interval comes out best |
| `moteur/tests/verifier_jeu_test.R` | The test set: the model doesn't see it, RMSEP / bias / SEP / Q2ext recomputed by hand, PLS-DA classification, random and Kennard–Stone choice (the same observations as `prospectr::kenStone` when installed) |
| `moteur/tests/verifier_opls.R` | OPLS against a second, plain-matrix writing of Trygg and Wold's algorithm (one Y, two Y, three classes); orthogonal scores unrelated to Y |
| `moteur/tests/verifier_script.R` | Scripts and reports written for many kinds of models, and a whole session in one script, run in a fresh `Rscript --vanilla` without the engine, give the engine's results (regressions too: LM with categories and cross terms, nested LMM with CAR1 and a test set, GLMs with an effort, GAMMs, Tweedie, null models, a mixed session); reports compile with `knitr` when it is installed |
| `moteur/tests/verifier_regression.R` | The regressions against `lm()`, `nlme::lme()`, `glm()`, `MASS::glm.nb()` and `mgcv::gam()`: coefficients, AIC, R2, θ, Tweedie's p, a random effect's variance, overdispersion, cross-validation, test set, block contributions, effect curves, CAR1, AIC sets, refused definitions, a saved definition read back, the prediction of observations and files, R formulas (read, written back, fitted like R fits them, refused, never run) |
| `moteur/tests/verifier_comparaisons.R` | The statistical tests against R's own test functions (Dunn and Games–Howell through their two-group identities with Kruskal–Wallis and Welch's t), class models, OPLS with several predictive components, the Methods text |
| `moteur/tests/verifier_validation.R` | The cross-validation: the engine's PRESS equals refitting each group's model by hand and predicting the group with `predire()`; on pure noise the median Q2 stays at or below 0 for 6 to 30 observations (PCA, PLS, OPLS-DA, automatic or fixed A); no more components than the data's rank; the example's models keep a high Q2; the permutation test refits from the project; groups by the value of a variable or of an ID, or given as they are (the same groups and Q2 each way, observations without a value in a group of their own, a clear error for a single group); block scaling and modifiers (each weight, a block of UV terms of total variance 1, the same weights in prediction and in each sub-model, kept by the settings of New model from this one) |
| `app/` | Electron main process, preload, interface (`app/interface/`), build script |
| `app/tests/essai_api.js` | Check run before each build |
| `app/tests/essai_appli.js` | The built program used end to end over the Chrome DevTools Protocol: the engine starts, the example project, a PLS model's views, an image of a plot, each fit in the R console, a `.nuee` given to a second start opens in the running window, then quitting stops R and removes its temporary folder (`node tests/essai_appli.js <program> [screenshot.png] [options]`; on Linux the program starts with no `LANG` and a short `PATH`, as from the desktop menu) |
| `app/icone/` | The logo, drawn by `dessiner_icone.py` |
| `exemple/` | The example project (made-up data) and `creer_exemple.js`, which makes it |
| `convertisseur/` | `.usp` to JSON converter; `usp_extract.py` is also used standalone to extract tables and model results |

## Running from the sources

Requires R 4.6 with `jsonlite`, `httpuv`, `openxlsx`, `haven` and `readxl` (and `foreign`, `nlme` and
`mgcv`, among R's recommended packages), Node, and Python 3 for the converter.

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
statistical tests and the class models (`jmvReadWrite`, `prospectr` and `knitr`, if installed, are only used to
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
options, console, example project, statistical tests, Methods text, class models, an OPLS-DA
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
  converter and `LISEZ-MOI.txt`, readable by every user (the packager builds in a folder made
  by `mkdtemp`, mode 700, which the `.deb` would otherwise install as `/opt/Nuée` for root
  alone).
- **`installateur/`**, made from that folder with electron-builder:
  - `Nuee-<version>-amd64.deb`, which depends on electron-builder's list plus `libgbm1` and
    `libasound2` (Electron loads them; a minimal Ubuntu 22.04 lacks `libgbm1`), then on
    `r-base-core` and `r-cran-httpuv`, `r-cran-jsonlite`, `r-cran-openxlsx`, `r-cran-haven`,
    `r-cran-readxl`, `r-cran-foreign`, `r-cran-nlme`, `r-cran-mgcv` (Debian keeps R's recommended
    packages, `foreign`, `nlme` and `mgcv` among them, out of `r-base-core`), and recommends `python3` and `r-recommended` (for the R
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
