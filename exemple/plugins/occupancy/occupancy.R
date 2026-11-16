# Single-season site occupancy model (MacKenzie et al. 2002), an example plugin for Nuée.
#
# Each row of the data is a site, and each column chosen as a detection history is a visit: 1 when
# the species was detected, 0 when it was looked for and not detected, empty when the site wasn't
# visited that time. The model has two parts, each on the logit scale:
#   occupancy   logit(psi_i) = x_i' beta    (the site covariates for occupancy, or a constant)
#   detection   logit(p_i)   = z_i' alpha   (the site covariates for detection, or a constant)
# A site where the species was detected at least once is occupied; one where it never was is
# either occupied and missed at each of its n_i visits, or not occupied:
#   L_i = psi_i prod_t p_i^y_it (1 - p_i)^(1 - y_it)      (detected at least once)
#   L_i = psi_i (1 - p_i)^n_i + 1 - psi_i                  (never detected)
# The log-likelihood, sum_i log L_i, is maximised by optim() with its exact gradient; the standard
# errors come from the inverse of the Hessian at the maximum. A site without any visit, or with a
# covariate missing, is left out.
#
# Nuée calls occupancy_fit(donnees, entrees, options): `donnees` holds the model's observations,
# `entrees` the names of the variables chosen in the dialog (plugin.json, "inputs"), `options` the
# values of its options. It returns the fitted model (class nuee_occupancy, with the methods below,
# which Nuée's summary, coefficients, likelihood and AIC use) with named values and tables.

# A message in the language of Nuée's interface (English outside Nuée).
occupancy_message <- function(en, fr) if (identical(getOption("nuee.langue"), "fr")) fr else en

occupancy_fit <- function(donnees, entrees, options) {
  visits <- entrees$detections
  y <- as.matrix(donnees[visits])
  storage.mode(y) <- "double"
  odd <- !is.na(y) & !(y %in% c(0, 1))
  if (any(odd)) {
    k <- which(odd, arr.ind = TRUE)[1, ]
    stop(sprintf(occupancy_message("Detections are 1 (detected), 0 (not detected) or empty (not visited): %s is %s in row %d.",
                                   "Les détections valent 1 (détecté), 0 (pas détecté) ou rien (pas de visite) : %s vaut %s à la ligne %d."),
                 visits[k[2]], format(y[k[1], k[2]]), k[1]), call. = FALSE)
  }
  covariates <- unique(c(entrees$occupancy, entrees$detection))
  used <- rowSums(!is.na(y)) > 0 & stats::complete.cases(donnees[covariates])
  if (sum(used) < 2) stop(occupancy_message("Fewer than 2 sites have a visit and every covariate.", "Moins de 2 sites ont une visite et toutes les covariables."), call. = FALSE)
  d <- droplevels(donnees[used, , drop = FALSE])
  y <- y[used, , drop = FALSE]
  X <- occupancy_design(d, entrees$occupancy)
  Z <- occupancy_design(d, entrees$detection)
  visited <- rowSums(!is.na(y))
  detected <- rowSums(y, na.rm = TRUE)
  if (!any(detected > 0)) stop(occupancy_message("The species was never detected: occupancy and detection can't be told apart.",
                                                "L'espèce n'a jamais été détectée : l'occupation et la détection ne peuvent pas être distinguées."), call. = FALSE)
  k1 <- ncol(X)
  k2 <- ncol(Z)
  parts <- function(theta) list(a = drop(X %*% theta[seq_len(k1)]), b = drop(Z %*% theta[k1 + seq_len(k2)]))
  # log P(y_i, site occupied), and the log-likelihood of each site
  site_ll <- function(a, b) {
    occupied <- stats::plogis(a, log.p = TRUE) + detected * stats::plogis(b, log.p = TRUE) + (visited - detected) * stats::plogis(-b, log.p = TRUE)
    empty <- stats::plogis(-a, log.p = TRUE)
    top <- pmax(occupied, empty)
    list(occupied = occupied, ll = ifelse(detected > 0, occupied, top + log(exp(occupied - top) + exp(empty - top))))
  }
  nll <- function(theta) { e <- parts(theta); -sum(site_ll(e$a, e$b)$ll) }
  # the gradient: w is the probability that a site is occupied given its history (1 when detected)
  gradient <- function(theta) {
    e <- parts(theta)
    s <- site_ll(e$a, e$b)
    w <- exp(s$occupied - s$ll)
    -c(crossprod(X, w - stats::plogis(e$a)), crossprod(Z, w * (detected - visited * stats::plogis(e$b))))
  }
  fit <- stats::optim(rep(0, k1 + k2), nll, gradient, method = options$method, hessian = TRUE,
                      control = list(maxit = options$maxit, reltol = 1e-12))
  if (fit$convergence != 0) warning(sprintf(occupancy_message("optim() didn't converge (code %d): try more iterations or the other method.",
                                                              "optim() n'a pas convergé (code %d) : essayez plus d'itérations ou l'autre méthode."), fit$convergence), call. = FALSE)
  noms <- c(paste0("psi.", colnames(X)), paste0("p.", colnames(Z)))
  theta <- stats::setNames(fit$par, noms)
  V <- tryCatch(solve(fit$hessian), error = function(e) {
    warning(occupancy_message("The Hessian can't be inverted: no standard errors (an estimate at a boundary, or too few sites).",
                              "La hessienne ne s'inverse pas : pas d'erreurs-types (une estimation à une borne, ou trop peu de sites)."), call. = FALSE)
    matrix(NA_real_, length(theta), length(theta))
  })
  dimnames(V) <- list(noms, noms)
  if (any(abs(theta) > 12)) warning(occupancy_message("An estimate is very large on the logit scale: psi or p is close to 0 or 1, at a boundary.",
                                                      "Une estimation est très grande sur l'échelle logit : psi ou p est proche de 0 ou de 1, à une borne."), call. = FALSE)
  e <- parts(theta)
  psi <- stats::plogis(e$a)
  p <- stats::plogis(e$b)
  z <- stats::qnorm(1 - (1 - options$level) / 2)
  # psi and p averaged over the sites, with their delta-method standard errors and an interval
  # computed on the logit scale (with a constant, exactly the interval of the coefficient, transformed)
  mean_se <- function(v, M, columns) {
    m <- mean(v)
    g <- colMeans(v * (1 - v) * M)
    se <- sqrt(max(0, drop(t(g) %*% V[columns, columns, drop = FALSE] %*% g)))
    half <- z * se / (m * (1 - m))
    c(estimate = m, se = se, lower = stats::plogis(stats::qlogis(m) - half), upper = stats::plogis(stats::qlogis(m) + half))
  }
  psi_mean <- mean_se(psi, X, seq_len(k1))
  p_mean <- mean_se(p, Z, k1 + seq_len(k2))
  cumulative <- 1 - (1 - p)^visited
  conditional <- ifelse(detected > 0, 1, psi * (1 - p)^visited / (psi * (1 - p)^visited + 1 - psi))
  model <- structure(list(coefficients = theta, vcov = V, loglik = -fit$value, df = length(theta), nobs = nrow(y), convergence = fit$convergence,
                          iterations = unname(fit$counts[1]), method = options$method, visits = visits, occupancy = entrees$occupancy,
                          detection = entrees$detection, sites = nrow(y), detected_sites = sum(detected > 0), naive = mean(detected > 0),
                          psi = psi_mean, p = p_mean, level = options$level),
                     class = "nuee_occupancy")
  sites <- data.frame(row = which(used), visits = visited, detections = detected, psi = psi, p = p, p_cumulative = cumulative,
                      psi_conditional = conditional)
  list(modele = model,
       valeurs = c(psi = psi_mean[["estimate"]], psi_se = psi_mean[["se"]], p = p_mean[["estimate"]], p_se = p_mean[["se"]],
                   naive_occupancy = mean(detected > 0), sites = nrow(y), detected_sites = sum(detected > 0),
                   occupied_sites = sum(conditional)),
       tableaux = c(list(estimates = data.frame(parameter = c("psi", "p"), estimate = c(psi_mean[["estimate"]], p_mean[["estimate"]]),
                                                se = c(psi_mean[["se"]], p_mean[["se"]]), lower = c(psi_mean[["lower"]], p_mean[["lower"]]),
                                                upper = c(psi_mean[["upper"]], p_mean[["upper"]]))),
                    if (isTRUE(options$sites)) list(sites = sites)))
}

# The design matrix of a part: a constant, then the covariates (a qualitative one by its categories,
# the first the reference).
occupancy_design <- function(d, covariates) {
  if (!length(covariates)) return(matrix(1, nrow(d), 1, dimnames = list(NULL, "(Intercept)")))
  f <- stats::reformulate(sprintf("`%s`", gsub("`", "\\\\`", covariates)))
  X <- stats::model.matrix(f, d)
  colnames(X) <- gsub("`", "", colnames(X))
  X
}

vcov.nuee_occupancy <- function(object, ...) object$vcov
logLik.nuee_occupancy <- function(object, ...) structure(object$loglik, df = object$df, nobs = object$nobs, class = "logLik")
nobs.nuee_occupancy <- function(object, ...) object$nobs

# The coefficients of each part, on the logit scale, with their Wald tests.
summary.nuee_occupancy <- function(object, ...) {
  se <- sqrt(pmax(0, diag(object$vcov)))
  z <- object$coefficients / se
  table <- cbind(Estimate = object$coefficients, `Std. Error` = se, `z value` = z, `Pr(>|z|)` = 2 * stats::pnorm(-abs(z)))
  part <- sub("\\..*$", "", rownames(table))
  each <- function(k) {
    x <- table[part == k, , drop = FALSE]
    rownames(x) <- substring(rownames(x), nchar(k) + 2)
    x
  }
  structure(list(coefficients = list(psi = each("psi"), p = each("p")), model = object), class = "summary.nuee_occupancy")
}

print.summary.nuee_occupancy <- function(x, digits = 4, ...) {
  m <- x$model
  msg <- occupancy_message
  cat(msg("Single-season site occupancy model (MacKenzie et al. 2002)\n", "Modèle d'occupation des sites sur une saison (MacKenzie et al. 2002)\n"))
  cat(sprintf(msg("Sites: %d, %d with a detection (naive occupancy %.3f); visits: %s\n", "Sites : %d, dont %d avec une détection (occupation naïve %.3f) ; visites : %s\n"),
              m$sites, m$detected_sites, m$naive, paste(m$visits, collapse = ", ")))
  cat(msg("\nOccupancy, logit(psi):\n", "\nOccupation, logit(psi) :\n"))
  stats::printCoefmat(x$coefficients$psi, digits = digits, signif.legend = FALSE)
  cat(msg("\nDetection, logit(p):\n", "\nDétection, logit(p) :\n"))
  stats::printCoefmat(x$coefficients$p, digits = digits, signif.legend = FALSE)
  level <- sprintf("%g%%", 100 * m$level)
  moyenne <- function(nom, e) cat(sprintf(msg("%s (mean over the sites): %.4f (SE %.4f, %s interval %.4f to %.4f)\n", "%s (moyenne sur les sites) : %.4f (erreur-type %.4f, intervalle à %s de %.4f à %.4f)\n"),
                                          nom, e[["estimate"]], e[["se"]], level, e[["lower"]], e[["upper"]]))
  cat("\n")
  moyenne("psi", m$psi)
  moyenne("p", m$p)
  cat(sprintf(msg("Log-likelihood %.4f (%d df), AIC %.4f; optim (%s): %s after %d evaluations\n", "Log-vraisemblance %.4f (%d ddl), AIC %.4f ; optim (%s) : %s après %d évaluations\n"),
              m$loglik, m$df, -2 * m$loglik + 2 * m$df, m$method,
              if (m$convergence == 0) msg("converged", "convergé") else sprintf(msg("not converged (code %d)", "pas convergé (code %d)"), m$convergence), m$iterations))
  invisible(x)
}

print.nuee_occupancy <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}
