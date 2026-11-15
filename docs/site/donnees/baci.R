# Simulated data for the BACI tutorial of Nuée's documentation website.
#   Rscript baci.R            (writes baci_truite.csv and baci_trout.csv next to it)
#
# The data are made up: no fish was counted. The story: a mill weir was removed in the winter of
# 2015-2016. Juvenile trout (young of the year) are counted by electrofishing every summer on
# six stations, from 2010 to 2023: three upstream of the former weir (A1, A2, A3: the impact) and
# three on a neighbouring tributary that has no weir (T1, T2, T3: the controls), as the column
# groupe (group) says.
#
# How the values are made, for each station s and year t:
#   density = 14                 the mean density, juveniles per 100 m2
#           + station[s]         each station's own level, normal, sd 3
#           + year[t]            the year's conditions (floods, a dry summer), the same for every
#                                station, normal, sd 2: the controls are there to remove them
#           + 6 if s is upstream of the weir and t >= 2016: the effect of the removal
#           + e[s, t]            what is left, autocorrelated from one year to the next within a
#                                station (AR(1), phi = 0.6, sd 2.2)
# rounded to 0.1, at least 0. Two counts are missing (high water): T2 in 2013 and A3 in 2019.
# The two files hold the same values, with French or English column names.
# Same seed, same values (R 3.6 or later).

set.seed(27)
stations <- c("A1", "A2", "A3", "T1", "T2", "T3")
annees <- 2010:2023
phi <- 0.6
sd_e <- 2.2
niveau <- setNames(rnorm(length(stations), 0, 3), stations)
effet_annee <- setNames(rnorm(length(annees), 0, 2), annees)

lignes <- list()
for (s in stations) {
  e <- numeric(length(annees))
  e[1] <- rnorm(1, 0, sd_e)
  for (i in seq_along(annees)[-1]) e[i] <- phi * e[i - 1] + rnorm(1, 0, sd_e * sqrt(1 - phi^2))
  impact <- startsWith(s, "A")
  densite <- 14 + niveau[[s]] + effet_annee + ifelse(impact & annees >= 2016, 6, 0) + e
  lignes[[s]] <- data.frame(station = s, groupe = if (impact) "impact" else "témoin", annee = annees,
                            densite = pmax(0, round(densite, 1)))
}
d <- do.call(rbind, lignes)
d$densite[d$station == "T2" & d$annee == 2013] <- NA
d$densite[d$station == "A3" & d$annee == 2019] <- NA
d <- cbind(obs = paste(d$station, d$annee, sep = "-"), d)
rownames(d) <- NULL

ici <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))),
                error = function(e) ".")
if (length(ici) != 1) ici <- "."
write.csv(d, file.path(ici, "baci_truite.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
en <- setNames(d, c("obs", "station", "group", "year", "density"))
en$group <- ifelse(en$group == "impact", "impact", "control")
write.csv(en, file.path(ici, "baci_trout.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(nrow(d), "rows written\n")
