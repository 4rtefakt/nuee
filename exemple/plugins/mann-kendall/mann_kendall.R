# The Mann–Kendall trend test and Sen's slope, an example plugin for Nuée.
#
# One series: its values y and their times t (a variable, or the rows' order); an observation
# whose value or time is missing is left out, and the others are taken in the order of their times.
#   S       the sum of sign(y_j - y_i) over every pair of times i before j (Mann 1945; Kendall 1975);
#   Var(S)  n(n - 1)(2n + 5) / 18, less sum_k t_k(t_k - 1)(2t_k + 5) / 18 for the groups of tied
#           values (t_k values each);
#   z       (S - sign(S)) / sqrt(Var(S)) with the continuity correction, S / sqrt(Var(S)) without;
#           p from the normal distribution, two-sided or one-sided;
#   tau     Kendall's tau-b: S / sqrt(N (N - sum_k t_k(t_k - 1) / 2)), N = n(n - 1) / 2 pairs;
#   slope   Sen's slope (Sen 1968), the median of the N slopes (y_j - y_i) / (t_j - t_i), per unit
#           of time; its interval from Var(S) (Gilbert 1987): the M-th and (N + 1 - M)-th of the
#           sorted slopes, M = (N - C) / 2 rounded, C = z_(1 - alpha/2) sqrt(Var(S));
#   the line  intercept + slope t, the intercept being the median of y - slope t.
# The test takes the observations as independent: positive autocorrelation (years that resemble
# each other) makes p too small.
#
# Nuée calls mann_kendall_fit(donnees, entrees, options) (plugin.json: "inputs", "options"); it
# returns the test (class nuee_mannkendall: its print and summary methods below show it in Nuée's
# summary, and its fitted values and residuals, the line's, go to Nuée's residual plots), its values
# and tables.

mann_kendall_message <- function(en, fr) if (identical(getOption("nuee.langue"), "fr")) fr else en

mann_kendall_fit <- function(donnees, entrees, options) {
  y <- donnees[[entrees$series]]
  if (!is.numeric(y)) stop(mann_kendall_message("The series must be a quantitative variable.", "La série doit être une variable quantitative."), call. = FALSE)
  t <- if (length(entrees$time)) donnees[[entrees$time]] else seq_along(y)
  if (!is.numeric(t)) stop(mann_kendall_message("The time must be a quantitative variable (a year, a number of days...).",
                                                "Le temps doit être une variable quantitative (une année, un nombre de jours…)."), call. = FALSE)
  kept <- !is.na(y) & !is.na(t)
  n <- sum(kept)
  if (n < 4) stop(sprintf(mann_kendall_message("%d times with a value: the test needs 4 at least.", "%d temps avec une valeur : le test en demande au moins 4."), n), call. = FALSE)
  o <- order(t[kept])
  ty <- t[kept][o]
  yy <- y[kept][o]
  if (anyDuplicated(ty)) stop(sprintf(mann_kendall_message("The time %s comes twice: a series has one value per time.", "Le temps %s revient deux fois : une série a une valeur par temps."),
                                     format(ty[duplicated(ty)][1])), call. = FALSE)
  dy <- outer(yy, yy, "-")
  dt <- outer(ty, ty, "-")
  pairs <- lower.tri(dy)  # (row j, column i: the later time minus the earlier)
  S <- sum(sign(dy[pairs]))
  ties <- as.vector(table(yy))
  ties <- ties[ties > 1]
  var_S <- (n * (n - 1) * (2 * n + 5) - sum(ties * (ties - 1) * (2 * ties + 5))) / 18
  if (var_S <= 0) stop(mann_kendall_message("The series has the same value at every time.", "La série a la même valeur à chaque temps."), call. = FALSE)
  z <- (if (isTRUE(options$continuity)) S - sign(S) else S) / sqrt(var_S)
  p <- switch(options$alternative, greater = stats::pnorm(z, lower.tail = FALSE), less = stats::pnorm(z), 2 * stats::pnorm(-abs(z)))
  N <- n * (n - 1) / 2
  tau <- S / sqrt(N * (N - sum(ties * (ties - 1) / 2)))
  slopes <- sort(dy[pairs] / dt[pairs])
  slope <- stats::median(slopes)
  C <- stats::qnorm(1 - (1 - options$level) / 2) * sqrt(var_S)
  M <- round((length(slopes) - C) / 2)
  interval <- if (M >= 1) c(slopes[M], slopes[length(slopes) + 1 - M]) else c(NA_real_, NA_real_)
  intercept <- stats::median(yy - slope * ty)
  # the line at the observations kept, in the data's order (Nuée matches them to its observations
  # through na.action)
  line <- intercept + slope * t[kept]
  unit <- if (is.null(options$unit)) "" else trimws(as.character(options$unit))
  test <- structure(list(S = S, var_S = var_S, z = z, p = p, tau = tau, n = n, ties = length(ties), slope = slope, lower = interval[1], upper = interval[2],
                         intercept = intercept, level = options$level, alternative = options$alternative, continuity = isTRUE(options$continuity),
                         series = entrees$series, time = if (is.null(entrees$time)) "" else entrees$time, unit = unit,
                         nobs = n, fitted.values = line, residuals = y[kept] - line,
                         na.action = if (any(!kept)) structure(which(!kept), class = "omit")),
                    class = "nuee_mannkendall")
  list(modele = test,
       valeurs = c(S = S, var_S = var_S, z = z, p = p, tau = tau, n = n, slope = slope, slope_lower = interval[1], slope_upper = interval[2], intercept = intercept),
       tableaux = list(test = data.frame(series = entrees$series, n = n, S = S, var_S = var_S, z = z, p = p, tau = tau, slope = slope,
                                         lower = interval[1], upper = interval[2], intercept = intercept),
                       series = data.frame(time = ty, value = yy, line = intercept + slope * ty)))
}

summary.nuee_mannkendall <- function(object, ...) structure(object, class = c("summary.nuee_mannkendall", class(object)))

print.summary.nuee_mannkendall <- function(x, ...) print.nuee_mannkendall(x, ...)

print.nuee_mannkendall <- function(x, ...) {
  m <- mann_kendall_message
  per <- if (nzchar(x$unit)) sprintf(m(" per %s", " par %s"), x$unit) else m(" per unit of time", " par unité de temps")
  side <- switch(x$alternative, greater = m("one-sided, an increasing trend", "unilatéral, une tendance à la hausse"),
                 less = m("one-sided, a decreasing trend", "unilatéral, une tendance à la baisse"), m("two-sided", "bilatéral"))
  cat(m("Mann-Kendall trend test and Sen's slope\n", "Test de tendance de Mann-Kendall et pente de Sen\n"))
  cat(sprintf(m("Series %s%s: %d values%s\n", "Série %s%s : %d valeurs%s\n"), x$series,
              if (nzchar(x$time)) sprintf(m(" over %s", " selon %s"), x$time) else m(" in the rows' order", " dans l'ordre des lignes"), x$n,
              if (x$ties) sprintf(m(", %d group(s) of tied values", ", %d groupe(s) d'ex aequo"), x$ties) else ""))
  cat(sprintf("S = %g, Var(S) = %.4f, z = %.4f%s, p = %.4g (%s)\n", x$S, x$var_S, x$z,
              if (x$continuity) m(" (continuity correction)", " (correction de continuité)") else "", x$p, side))
  cat(sprintf(m("Kendall's tau-b = %.4f\n", "Tau-b de Kendall = %.4f\n"), x$tau))
  cat(sprintf(m("Sen's slope = %.6g%s, %g%% interval %s to %s; intercept %.6g\n", "Pente de Sen = %.6g%s, intervalle à %g %% de %s à %s ; ordonnée à l'origine %.6g\n"),
              x$slope, per, 100 * x$level, format(x$lower, digits = 6), format(x$upper, digits = 6), x$intercept))
  invisible(x)
}
