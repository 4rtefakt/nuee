# Simulated data for the site occupancy example plugin (exemple/plugins/occupancy), used by the user
# guide's Plugins pages and the plugins' checks.
#   Rscript occupancy.R           (writes woodpecker_occupancy.csv next to it)
#
# The data are made up: no bird was surveyed. 80 woodland plots, each visited 4 times in spring by
# a birder listening for a woodpecker for 10 minutes: one row per plot,
#   plot        P01…P80
#   forest      the forest cover within 500 m, in % (5 to 95)
#   habitat     the plot's woodland: deciduous, mixed or conifer
#   noise       a traffic-noise index measured once at the plot (0 quiet, 10 loud)
#   visit1…4    1 when the woodpecker was heard or seen, 0 when not, empty when the visit wasn't made
# The model behind them is the one the plugin fits:
#   occupancy   logit(psi) = -1.2 + 0.04 forest        (psi from 0.26 to 0.92)
#   detection   logit(p)   =  0.6 - 0.25 noise          (p from 0.65 down to 0.13)
# and 12 visits of the 320 not made (at random).
# Same seed, same values (R 3.6 or later).

ici <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))), error = function(e) ".")
if (!length(ici) || !nzchar(ici)) ici <- "."
set.seed(20261116)
n <- 80
forest <- round(stats::runif(n, 5, 95))
habitat <- sample(c("deciduous", "mixed", "conifer"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25))
noise <- round(stats::runif(n, 0, 10), 1)
psi <- stats::plogis(-1.2 + 0.04 * forest)
p <- stats::plogis(0.6 - 0.25 * noise)
z <- stats::rbinom(n, 1, psi)
y <- matrix(stats::rbinom(n * 4, 1, rep(z * p, 4)), n, 4)
y[sample(n * 4, 12)] <- NA
d <- data.frame(plot = sprintf("P%02d", seq_len(n)), forest = forest, habitat = habitat, noise = noise,
                visit1 = y[, 1], visit2 = y[, 2], visit3 = y[, 3], visit4 = y[, 4])
utils::write.csv(d, file.path(ici, "woodpecker_occupancy.csv"), row.names = FALSE, na = "")
cat(sprintf("woodpecker_occupancy.csv: %d plots, %d occupied, %d with a detection\n", n, sum(z), sum(rowSums(y, na.rm = TRUE) > 0)))
