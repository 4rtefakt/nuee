<!-- description: Six Nuée tutorials: a before-after with controls (BACI), a PLS on near-infrared spectra, the monitoring of a bird population (TRIM and distance sampling), community ecology, maps and spatial models, and a report document. -->
# Tutorials

Six studies followed from start to finish: the question, the data, Nuée's windows one after
the other, how to read the results, and the Methods text. Each has its table to download.

<ul class="cartes">
<li><h3><a href="page:tutoriels.baci">Before-after with controls (BACI)</a></h3><p>Did removing a weir raise the density of young trout? Six stations over 14 years: the impact − control gap, the BACI mixed model and its conditions, the interaction read correctly, the adjusted means, the Methods text and Export to R.</p></li>
<li><h3><a href="page:tutoriels.pls">PLS on spectra</a></h3><p>Predicting the organic carbon of soils from their near-infrared spectrum: the preprocessing (SNV, detrending), the number of components by cross-validation, VIP, observed against predicted, the permutation test, then predicting new samples.</p></li>
<li><h3><a href="page:tutoriels.suivi">Monitoring a population</a></h3><p>A skylark counted on 40 sites over 17 years: from Which analysis? to TRIM's indices and trend and its class, then the density from line transects (distance sampling), and a figure for the report.</p></li>
<li><h3><a href="page:tutoriels.communautes">Community ecology</a></h3><p>The ground beetles of woodland, hedgerows and fields: rarefaction and species accumulation, an NMDS on Bray–Curtis, a PERMANOVA and its dispersion test, the indicator species, and what to write in a paper.</p></li>
<li><h3><a href="page:tutoriels.cartes">Maps and spatial models</a></h3><p>Hay biomass on a mountain plateau: coordinates in Lambert-93 and the map, the elevation read from a raster, Moran's I of a model's residuals, a GAM with a spatial smooth, and a GeoPackage for QGIS.</p></li>
<li><h3><a href="page:tutoriels.rapport">A report document</a></h3><p>The meadows' study written up: values that follow the models, the Methods and Results texts, citations from a BibTeX file, a two-panel figure at a journal's size, Word and PDF, and a reproducible Quarto file.</p></li>
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
| Communities | [ground_beetles.csv](@/donnees/ground_beetles.csv) | [communautes.R](@/donnees/communautes.R) |
| Maps | [meadows.csv](@/donnees/meadows.csv), [elevation.tif](@/donnees/elevation.tif) | [cartes.R](@/donnees/cartes.R) |
| Report | the maps tutorial's, and [references.bib](@/donnees/references.bib) (four published papers and books) | [cartes.R](@/donnees/cartes.R) |

The same tables exist with French column names (see the [French tutorials](@/fr/tutoriels/index.html)).
`Rscript baci.R` (and the others) writes them again, identical, with R 3.6 or later.
