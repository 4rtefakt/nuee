<!-- description: Three Nuée tutorials: a before-after with controls (BACI), a PLS on near-infrared spectra, and the monitoring of a bird population (TRIM and distance sampling). -->
# Tutorials

Three studies followed from start to finish: the question, the data, Nuée's windows one after
the other, how to read the results, and the Methods text. Each has its table to download.

<ul class="cartes">
<li><h3><a href="page:tutoriels.baci">Before-after with controls (BACI)</a></h3><p>Did removing a weir raise the density of young trout? Six stations over 14 years: the impact − control gap, the BACI mixed model and its conditions, the interaction read correctly, the adjusted means, the Methods text and Export to R.</p></li>
<li><h3><a href="page:tutoriels.pls">PLS on spectra</a></h3><p>Predicting the organic carbon of soils from their near-infrared spectrum: the preprocessing (SNV, detrending), the number of components by cross-validation, VIP, observed against predicted, the permutation test, then predicting new samples.</p></li>
<li><h3><a href="page:tutoriels.suivi">Monitoring a population</a></h3><p>A skylark counted on 40 sites over 17 years: from Which analysis? to TRIM's indices and trend and its class, then the density from line transects (distance sampling), and a figure for the report.</p></li>
</ul>

## Before you start

- [Install Nuée](page:installation): version {{version}} or later.
- The tutorials' data are **simulated**: they were made for the exercise by an R script, with a
  fixed seed; the script is next to each table. No result is to be cited as an observation.
- The screenshots come from Nuée {{version}} in English (the French pages show the interface in
  French). They were taken in Nuée's browser version: the desktop application's window is the
  same, but for a few buttons (*Download* there becomes *Choose where…*).
- The interface is in English and French (*View › Language*).

## The data

| Tutorial | Table | Script that made it |
|---|---|---|
| BACI | [baci_trout.csv](@/donnees/baci_trout.csv) | [baci.R](@/donnees/baci.R) |
| PLS | [soils_spectra.csv](@/donnees/soils_spectra.csv), [soils_new.csv](@/donnees/soils_new.csv) | [spectres.R](@/donnees/spectres.R) |
| Monitoring | [skylark_counts.csv](@/donnees/skylark_counts.csv), [skylark_transects.csv](@/donnees/skylark_transects.csv) | [suivi.R](@/donnees/suivi.R) |

The same tables exist with French column names (see the [French tutorials](@/fr/tutoriels/index.html)).
`Rscript baci.R` (and the two others) writes them again, identical, with R 3.6 or later.
