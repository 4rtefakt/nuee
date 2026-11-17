# Simulated data for the community ecology tutorial of Nuée's documentation website.
#   Rscript communautes.R     (writes carabes.csv and ground_beetles.csv next to it)
#
# The data are made up: no beetle was caught. The story: ground beetles (carabids) are caught in
# 36 pitfall traps left open for three weeks in May, in a farmed landscape: 12 traps in woodland,
# 12 along hedgerows and 12 in arable fields (P01 to P12, P13 to P24, P25 to P36). One row per
# trap, one column per species (26 species, in alphabetical order), the number of individuals
# caught.
#
# How the counts are made, for each trap i and species s:
#   mean = the species' mean in the trap's habitat (the table below, individuals per trap)
#        x the trap's catch (normal on the log scale, sd 0.3: a trap flooded or dug in deeper)
#   in a hedgerow, the mean is the hedge's own species plus a share of the woodland's and of the
#   field's: a hedge between two fields gets more field species, one along a wood more woodland
#   ones (each hedge's position drawn between 0 and 1), so the hedgerows vary more than the
#   other habitats;
#   count = a negative binomial of that mean (size 1.5: beetles are caught in bunches).
# The species' habitats follow what is known of them in western Europe (Abax parallelepipedus in
# woods, Poecilus cupreus and Pterostichus melanarius in fields...), but the numbers are made up.
# The two files hold the same values, with French or English column names and habitats.
# Same seed, same values (R 3.6 or later).

set.seed(1797)
moyennes <- read.table(header = TRUE, text = "
espece                         bois  haie  culture
Abax_parallelepipedus          10.0  1.0   0.0
Abax_parallelus                 3.0  0.3   0.0
Agonum_muelleri                 0.0  0.5   2.5
Amara_aenea                     0.0  0.6   2.0
Anchomenus_dorsalis             0.1  1.5   7.0
Bembidion_lampros               0.1  1.2   5.0
Brachinus_crepitans             0.0  0.4   0.6
Calathus_fuscipes               0.0  1.0   1.5
Carabus_auratus                 0.0  0.3   1.2
Carabus_nemoralis               1.5  2.5   0.1
Carabus_problematicus           2.5  0.5   0.0
Clivina_fossor                  0.0  0.2   1.5
Cychrus_caraboides              0.8  0.2   0.0
Harpalus_affinis                0.0  0.6   2.5
Harpalus_rufipes                0.2  2.0   6.0
Leistus_rufomarginatus          1.0  0.4   0.0
Leistus_spinibarbis             0.3  1.5   0.0
Loricera_pilicornis             0.4  0.8   1.2
Nebria_brevicollis              2.0  4.0   0.6
Notiophilus_biguttatus          1.2  1.5   0.2
Poecilus_cupreus                0.0  0.5   9.0
Pterostichus_madidus            5.0  4.0   0.8
Pterostichus_melanarius         0.8  2.0  14.0
Pterostichus_niger              2.5  0.6   0.0
Pterostichus_oblongopunctatus   2.5  0.3   0.0
Trechus_quadristriatus          0.1  0.8   4.0
")
especes <- gsub("_", " ", moyennes$espece)
habitats <- rep(c("bois", "haie", "culture"), each = 12)
pieges <- sprintf("P%02d", seq_along(habitats))

comptes <- matrix(0L, length(pieges), length(especes), dimnames = list(pieges, especes))
for (i in seq_along(pieges)) {
  prise <- exp(rnorm(1, -0.3^2 / 2, 0.3))
  mu <- switch(habitats[i],
    bois = moyennes$bois,
    culture = moyennes$culture,
    haie = { g <- runif(1); moyennes$haie + 0.5 * (1 - g) * moyennes$bois + 0.5 * g * moyennes$culture })
  comptes[i, ] <- rnbinom(length(especes), size = 1.5, mu = mu * prise)
}

ici <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))),
                error = function(e) ".")
if (length(ici) != 1) ici <- "."
fr <- data.frame(piege = pieges, habitat = habitats, comptes, check.names = FALSE)
write.csv(fr, file.path(ici, "carabes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
en <- data.frame(trap = pieges, habitat = c(bois = "woodland", haie = "hedgerow", culture = "field")[habitats], comptes, check.names = FALSE)
write.csv(en, file.path(ici, "ground_beetles.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat(nrow(fr), "traps,", sum(comptes), "beetles of", sum(colSums(comptes) > 0), "species\n")
