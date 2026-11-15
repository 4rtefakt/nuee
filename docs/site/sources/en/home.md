<!-- description: Nuée, a desktop application for ecologists' analyses: PCA, PLS, community ecology, mixed models, monitoring and maps, computed in R, on Windows and Linux. -->
# Nuée

<p class="chapeau">PCA, PLS, community ecology, linear, generalized and mixed models, population monitoring and maps, on Windows and Linux, computed in R.</p>

<div class="boutons">
<a class="bouton" href="{{telechargement:windows}}">Download for Windows</a>
<a class="bouton second" href="page:installation#linux">Linux</a>
<a class="bouton second" href="page:tutoriels">Tutorials</a>
</div>

Nuée (version {{version}}) is a desktop application to analyse field and laboratory data. Open a
table from Excel, R, SPSS, Stata, SAS, jamovi or QGIS, pick your variables, and get the model with
its plots, diagnostics and validation. The results go back to any of those programs, with the
paragraph for your Methods section, or as an R script that redoes the calculation without Nuée.

![Nuée's window: a PLS on spectra, its summary, its scores and its loadings drawn against the wavelength](@/images/en/accueil-fenetre.png "A PLS on near-infrared spectra (tutorial 2): the explorer on the left, the model's summary, its scores and its loadings.")

## Who it is for

Field ecologists, managers of natural areas and environmental scientists, statisticians or not.
Nuée helps you choose:

- **Which analysis?** (Model menu) asks your question in plain words: compare groups, the effect
  of variables, many species, a trend, a density, a before-after with controls… It proposes the
  response and the design from your columns, recommends an analysis and opens the model's window
  already filled in.
- Each regression's Summary gives **its conditions in words**: the residuals' normality, a spread
  that changes, influential observations, overdispersion, collinear terms, with what to do.
- **The Methods text** and **the Results text** are written from the model, in English or French,
  with their references.

## What Nuée does

<ul class="cartes">
<li><h3>Multivariate</h3><p>PCA, PLS, PLS-DA, OPLS, near-infrared spectra (SNV, MSC, derivatives), VIP, cross-validation, permutation tests.</p><p><a href="page:guide.acp-pls">The guide</a></p></li>
<li><h3>Regressions</h3><p>Linear and mixed models (nlme), GLMs and GAMs (mgcv) for counts, presences and proportions, with autocorrelation in time, adjusted means and AIC selection.</p><p><a href="page:guide.regressions">The guide</a></p></li>
<li><h3>Communities</h3><p>RDA, CA, CCA, PCoA, NMDS, PERMANOVA, Mantel test, indicator species, rarefaction.</p><p><a href="page:guide.communautes">The guide</a></p></li>
<li><h3>Monitoring</h3><p>Trends in series, before-after with controls (BACI), count indices (TRIM), densities by distance sampling.</p><p><a href="page:guide.suivis">The guide</a></p></li>
<li><h3>Maps</h3><p>Coordinates and their systems (Lambert-93, UTM…), GeoPackage for QGIS, raster values, spatial autocorrelation.</p><p><a href="page:guide.cartes">The guide</a></p></li>
<li><h3>Publishing</h3><p>Figures composed at a journal's size, PDF and Word reports, documents with citations, Export to R, R Markdown and Quarto.</p><p><a href="page:guide.resultats">The guide</a></p></li>
</ul>

## All in R, on your computer

Every calculation is written in R, with comments, in the application's folder: you can read what
was computed, and change it. The R console (View › R console) shows the call behind each action.
The results are those of the reference packages (vegan, nlme, mgcv, emmeans, rtrim, Distance…),
checked at every change.

Your data stay on your computer: Nuée sends nothing, except a question to GitHub at start-up about
a newer version (it can be turned off in the Help menu), and the area shown to OpenStreetMap's or
IGN's servers when you choose a background map.

## Download

Nuée {{version}}:

- **Windows 10 and 11**: [{{fichier:windows}}]({{telechargement:windows}}), R included;
- **Debian, Ubuntu and their derivatives**: [{{fichier:deb}}]({{telechargement:deb}});
- **other Linux distributions**: [{{fichier:appimage}}]({{telechargement:appimage}}) or [{{fichier:targz}}]({{telechargement:targz}}).

How to install: see [Install](page:installation). Every version and its notes: the
[Releases page on GitHub]({{telechargement:toutes}}); what changed: [What’s new](page:nouveautes).

## Getting started

- [The tutorials](page:tutoriels) follow three studies from start to finish: a before-after with
  controls (BACI), a PLS on spectra, and the monitoring of a bird population.
- In the application, Help › Example project opens 120 made-up apples with four ready models.
- [The user guide](page:guide) describes every window and every calculation.

Nuée is free software ([GPL-3.0](https://github.com/4rtefakt/nuee/blob/main/LICENSE)). The
interface is in English and French (View › Language). A problem, a question:
[open an issue on GitHub](https://github.com/4rtefakt/nuee/issues), in English or French; Help ›
Diagnostic information gives the versions to paste into it.
