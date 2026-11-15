# Simulated data for the monitoring tutorial of Nuée's documentation website.
#   Rscript suivi.R           (writes four files next to it)
#
# The data are made up: no bird was counted. The species is a farmland bird, the skylark.
#
# 1. Counts on 40 sites (2 km squares), every spring from 2008 to 2024, as in a common bird
#    monitoring scheme: one row per site and year, the count empty when the site wasn't
#    surveyed that year (sites that joined late, years missed).
#      comptages_alouette.csv      obs, site, habitat (cultures, prairies), annee, alouettes
#      skylark_counts.csv          the same values: obs, site, habitat (cropland, grassland), year, skylarks
#    log(mean count) = the site's level (median 6 birds, more in grasslands)
#                    + a decline of 3.5 % a year in croplands, 2 % in grasslands
#                    + the year's conditions, the same for every site (sd 0.06)
#                    + a site-year deviation that carries over from one year to the next
#                      (AR(1), phi = 0.6, sd 0.4);
#    the count: a negative binomial of that mean (size 15: more spread than a Poisson).
#    10 sites join between 2009 and 2014; each site misses about one year in ten.
#
# 2. A line-transect survey of the same species in May 2024, in a study area of 120 km2: 24
#    transects of 1 to 2.5 km, the perpendicular distance of each singing skylark seen or heard,
#    in metres, and the size of each group (1 or 2 birds). One row per detection; a transect
#    without any detection keeps a row with an empty distance, so that its length counts.
#      transects_alouette.csv      region, aire, transect, longueur, distance, taille
#      skylark_transects.csv       Region.Label, Area, Sample.Label, Effort, distance, size
#    Density 25 groups per km2, varying between transects (lognormal, sd 0.3), none along the
#    last transect (T24 crosses a village); detection: half-normal, sigma 70 m; distances beyond
#    250 m not recorded.
# Same seeds, same values (R 3.6 or later).

ici <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))),
                error = function(e) ".")
if (length(ici) != 1) ici <- "."

# ---------------------------------------------------------------- 1. counts by site and year
set.seed(9)
annees <- 2008:2024
nsites <- 40
sites <- sprintf("C%02d", seq_len(nsites))
habitat <- rep(c("cultures", "prairies"), length.out = nsites)
niveau <- log(6) + ifelse(habitat == "prairies", 0.3, 0) + rnorm(nsites, 0, 0.6)
pente <- ifelse(habitat == "prairies", log(0.98), log(0.965))
effet_annee <- rnorm(length(annees), 0, 0.06)
debut <- rep(2008, nsites)
debut[sample(nsites, 10)] <- sample(2009:2014, 10, replace = TRUE)
phi <- 0.6
sd_e <- 0.4
lignes <- list()
for (s in seq_len(nsites)) {
  e <- numeric(length(annees))
  e[1] <- rnorm(1, 0, sd_e)
  for (i in seq_along(annees)[-1]) e[i] <- phi * e[i - 1] + rnorm(1, 0, sd_e * sqrt(1 - phi^2))
  mu <- exp(niveau[s] + pente[s] * (annees - 2008) + effet_annee + e)
  n <- rnbinom(length(annees), size = 15, mu = mu)
  n[annees < debut[s] | runif(length(annees)) < 0.1] <- NA
  lignes[[s]] <- data.frame(site = sites[s], habitat = habitat[s], annee = annees, alouettes = n)
}
comptages <- do.call(rbind, lignes)
comptages <- cbind(obs = paste(comptages$site, comptages$annee, sep = "-"), comptages)
write.csv(comptages, file.path(ici, "comptages_alouette.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
en <- setNames(comptages, c("obs", "site", "habitat", "year", "skylarks"))
en$habitat <- ifelse(en$habitat == "prairies", "grassland", "cropland")
write.csv(en, file.path(ici, "skylark_counts.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

# ---------------------------------------------------------------- 2. line transects
set.seed(2024)
w <- 250                                   # m, distances beyond it not recorded
sigma <- 70                                # m, half-normal detection
ntransects <- 24
longueur <- round(runif(ntransects, 1, 2.5), 2)            # km
densite <- 25 * exp(rnorm(ntransects, -0.3^2 / 2, 0.3))   # groups per km2 around each transect
densite[ntransects] <- 0                                   # the last one crosses a village: no skylark
detections <- list()
for (k in seq_len(ntransects)) {
  aire_couverte <- 2 * (w / 1000) * longueur[k]             # km2
  groupes <- rpois(1, densite[k] * aire_couverte)
  x <- runif(groupes, 0, w)                                  # groups spread evenly in the strip
  vus <- x[runif(groupes) < exp(-x^2 / (2 * sigma^2))]
  taille <- ifelse(runif(length(vus)) < 0.12, 2, 1)
  id <- sprintf("T%02d", k)
  detections[[k]] <- if (length(vus)) {
    data.frame(transect = id, longueur = longueur[k], distance = round(vus), taille = taille)
  } else {
    data.frame(transect = id, longueur = longueur[k], distance = NA, taille = NA)
  }
}
transects <- do.call(rbind, detections)
transects <- cbind(region = "zone", aire = 120, transects)
write.csv(transects, file.path(ici, "transects_alouette.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
en <- setNames(transects, c("Region.Label", "Area", "Sample.Label", "Effort", "distance", "size"))
en$Region.Label <- "area"
write.csv(en, file.path(ici, "skylark_transects.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(nrow(comptages), "site-years,", sum(!is.na(transects$distance)), "detections\n")
