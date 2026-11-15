# Simulated data for the PLS tutorial of Nuée's documentation website.
#   Rscript spectres.R        (writes four files next to it)
#
# The data are made up: no soil was measured. 120 soil samples, half from grasslands and half
# from croplands, with their organic carbon content (SOC, g/kg, as a laboratory would measure
# it) and their near-infrared spectrum: the absorbance, log10(1/R), from 1100 to 2500 nm every
# 10 nm (141 columns, named by their wavelength). Then 20 new samples, made the same way, whose
# carbon is not given: the model will predict it. The last one, N020, is unlike the others: a
# peat soil, much richer in carbon (95 g/kg) and in water, with little clay.
#   sols_spectres.csv, sols_nouveaux.csv      French column names (echantillon, occupation, cos)
#   soils_spectra.csv, soils_new.csv          the same values, English names (sample, land_use, soc)
#
# How a spectrum is made:
#   - a gently rising absorbance (0.25 at 1100 nm, 0.40 at 2500 nm);
#   - organic matter darkens the soil (a broad absorbance that grows with the carbon, stronger at
#     short wavelengths) and absorbs at 1720, 2060, 2310 and 2350 nm (C-H and N-H bands);
#   - clay (10 to 45 %) absorbs at 1410 and 2200 nm (O-H and Al-OH);
#   - the water left in the air-dried sample (1 to 4 %, more with more clay and carbon) absorbs at
#     1450 and 1930 nm;
#   - each sample's grain size and packing scale the spectrum (x 0.85 to 1.15) and shift and tilt it,
#     as real spectra are: the SNV and the detrend of the tutorial remove most of this;
#   - noise, its sd 3 % of the spectrum's own spread; values rounded to 4 decimals.
# Carbon: lognormal, median 28 g/kg in grasslands and 14 in croplands.
# Same seeds, same values (R 3.6 or later).

set.seed(1100)
lambda <- seq(1100, 2500, by = 10)
bande <- function(centre, largeur) exp(-0.5 * ((lambda - centre) / largeur)^2)

spectre <- function(cos, argile, eau) {
  base <- 0.25 + 0.15 * (lambda - 1100) / 1400
  matiere <- 0.004 * cos * exp(-(lambda - 1100) / 900) +
    cos * (0.0024 * bande(1720, 30) + 0.0012 * bande(2060, 40) + 0.003 * bande(2310, 35) + 0.0016 * bande(2350, 30))
  mineral <- argile * (0.002 * bande(1410, 30) + 0.003 * bande(2200, 25))
  humidite <- eau * (0.02 * bande(1450, 45) + 0.035 * bande(1930, 50))
  m <- runif(1, 0.85, 1.15)
  propre <- m * (base + matiere + mineral + humidite) + rnorm(1, 0, 0.03) + rnorm(1, 0, 0.02) * (lambda - 1800) / 700
  round(propre + rnorm(length(lambda), 0, 0.03 * stats::sd(propre)), 4)
}

echantillons <- function(n, prefixe) {
  occupation <- rep(c("prairie", "culture"), length.out = n)
  cos <- round(exp(rnorm(n, ifelse(occupation == "prairie", log(28), log(14)), 0.35)), 1)
  argile <- runif(n, 10, 45)
  eau <- pmax(0.5, 1 + 0.05 * argile + 0.03 * cos + rnorm(n, 0, 0.4))
  spectres <- t(vapply(seq_len(n), function(i) spectre(cos[i], argile[i], eau[i]), numeric(length(lambda))))
  colnames(spectres) <- lambda
  list(id = sprintf("%s%03d", prefixe, seq_len(n)), occupation = occupation, cos = cos, spectres = spectres)
}

calibration <- echantillons(120, "S")
set.seed(5)                                           # (the new samples: a seed of their own)
nouveaux <- echantillons(20, "N")
nouveaux$occupation[20] <- "prairie"
nouveaux$cos[20] <- 95
nouveaux$spectres[20, ] <- spectre(95, 8, 6)          # the peat soil

ici <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))),
                error = function(e) ".")
if (length(ici) != 1) ici <- "."
ecrire <- function(x, fichier, noms, avec_cos = TRUE) {
  d <- data.frame(x$id, x$occupation, check.names = FALSE)
  if (avec_cos) d <- cbind(d, x$cos)
  d <- cbind(d, as.data.frame(x$spectres, check.names = FALSE))
  names(d)[seq_along(noms)] <- noms
  write.csv(d, file.path(ici, fichier), row.names = FALSE, fileEncoding = "UTF-8")
}
ecrire(calibration, "sols_spectres.csv", c("echantillon", "occupation", "cos"))
ecrire(nouveaux, "sols_nouveaux.csv", c("echantillon", "occupation"), avec_cos = FALSE)
anglais <- function(x) { x$occupation <- ifelse(x$occupation == "prairie", "grassland", "cropland"); x }
ecrire(anglais(calibration), "soils_spectra.csv", c("sample", "land_use", "soc"))
ecrire(anglais(nouveaux), "soils_new.csv", c("sample", "land_use"), avec_cos = FALSE)
cat("120 + 20 samples written\n")
