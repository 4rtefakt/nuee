# Vérifie TRIM (moteur/R/trim.R) : contre des calculs directs (le maximum de vraisemblance de
# Poisson par glm() et sa surdispersion par quasipoisson ; les équations GEE, σ², ρ, la covariance
# des paramètres et celle des totaux écrites avec les matrices entières de tous les paramètres, sites
# compris ; les indices, la pente globale par lm() et sa classe ; les tests de Wald), et, quand il est
# installé (NUEE_RTRIM=<bibliothèque> le désigne), contre le paquet rtrim : mêmes paramètres et
# erreurs-types, totaux, indices, pentes globales et leur classe, σ², ρ, khi², LR, AIC et tests de
# Wald, pour les modèles 1, 2 et 3, avec ou sans surdispersion et corrélation sérielle, des points de
# changement, la sélection pas à pas, une covariable, des poids, des comptages manquants, sur les
# données de rtrim (skylark, oystercatcher) et des données simulées. Puis les refus, ce que
# l'interface reçoit, le texte des méthodes, les tableaux exportés, une condition sur les
# observations.
#   Rscript moteur/tests/verifier_trim.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
# l'écart relatif le plus grand (rapporté à 1 pour les petites valeurs)
ecart <- function(a, b) max(abs(as.numeric(a) - as.numeric(b)) / pmax(1, abs(as.numeric(b))))
proche <- function(a, b, tol = 1e-7) length(a) == length(b) && all(is.finite(as.numeric(a)) == is.finite(as.numeric(b))) &&
  isTRUE(ecart(a[is.finite(as.numeric(a))], b[is.finite(as.numeric(b))]) < tol)
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

if (nzchar(Sys.getenv("NUEE_RTRIM"))) .libPaths(c(Sys.getenv("NUEE_RTRIM"), .libPaths()))
avec_rtrim <- requireNamespace("rtrim", quietly = TRUE)
cat(if (avec_rtrim) sprintf("(rtrim %s installé : comparaisons avec lui)\n", utils::packageVersion("rtrim")) else "(rtrim absent : calculs directs seulement)\n")

# Un projet d'un tableau long, et la définition d'un TRIM sur lui (le site ou le temps "id:nom" : un
# identifiant des observations).
projet_trim <- function(df, nom = "comptages") projet_depuis_tableau(df, nom)
definition_trim <- function(p, modele = 3, sd = TRUE, sc = TRUE, cov = NULL, cp = NULL, poids = NULL, base = NULL, pas_a_pas = FALSE,
                            site = "site", temps = "annee", y = "compte", observations = NULL, condition = NULL) {
  u <- function(...) numeros(p, c(...))
  spec <- function(x) if (startsWith(x, "id:")) list(by = "id", id = substring(x, 4)) else list(by = "variable", variable = u(x))
  o <- list(site = spec(site), temps = spec(temps), modele = modele, surdispersion = sd, correlation = sc)
  if (!is.null(cp)) o$changements <- cp
  if (!is.null(poids)) o$poids <- u(poids)
  if (!is.null(base)) o$base <- base
  if (pas_a_pas) o$pas_a_pas <- TRUE
  definir_modele(p, "TRIM", x = if (length(cov)) u(cov), y = u(y), echelle = "none", options = o, observations = observations, condition = condition)
}
trim_de <- function(p, ...) ajuster(p, definition_trim(p, ...))

# ---------------------------------------------------------------- les données simulées

# 30 sites suivis 12 ans (2000–2011) : un effet par site, une hausse puis une baisse après 2005 (plus
# forte en marais), un bruit autocorrélé d'une année à l'autre (des comptages surdispersés et corrélés),
# 30 % de comptages manquants ; un habitat par site ; une gestion qui change en 2006 sur un tiers des
# sites (une covariable des sites et des temps) ; un poids par site ; un site sans aucun comptage
# positif. Le tableau complet (une ligne par site et par année) est celui que rtrim reçoit.
simuler <- function(graine = 1, I = 30, J = 12) {
  set.seed(graine)
  site <- sprintf("S%02d", seq_len(I))
  annee <- 2000 + seq_len(J) - 1
  habitat <- sample(c("bocage", "marais"), I, TRUE)
  a <- stats::rnorm(I, 2, 0.8)
  g <- expand.grid(j = seq_len(J), i = seq_len(I))
  pente <- ifelse(habitat[g$i] == "marais", -0.12, -0.05)
  lin <- ifelse(g$j <= 6, 0.06 * (g$j - 1), 0.3 + pente * (g$j - 6))
  bruit <- as.vector(vapply(seq_len(I), function(i) as.numeric(stats::arima.sim(list(ar = 0.5), J, sd = 0.25)), numeric(J)))
  compte <- stats::rpois(nrow(g), exp(a[g$i] + lin + bruit))
  compte[stats::runif(nrow(g)) < 0.3] <- NA
  compte[g$i == I] <- ifelse(is.na(compte[g$i == I]), NA, 0)
  gestion <- ifelse(g$i %% 3 == 0 & g$j >= 7, "intensive", "extensive")
  data.frame(nom = sprintf("r%03d", seq_len(nrow(g))), site = site[g$i], annee = annee[g$j], compte = compte, habitat = habitat[g$i],
             gestion = gestion, poids = c(1, 2.5, 4)[g$i %% 3 + 1], stringsAsFactors = FALSE)
}
sim <- simuler()
ps <- projet_trim(sim, "simulés")

# ---------------------------------------------------------------- calculs directs

cat("Calculs directs : glm(), les équations GEE et les covariances avec les matrices entières\n")

# Les lignes observées du modèle et leur dessin complet : une indicatrice par site, puis les colonnes
# du temps de chaque ligne (dessin_site), comme on les écrirait à la main pour glm().
dessin_complet <- function(m) {
  t <- m$donnees
  fit <- m$ajustement_noyau
  I <- length(t$sites)
  toutes <- which(matrix(TRUE, I, length(t$temps)), arr.ind = TRUE)
  obs <- !is.na(t$f[toutes])
  A <- outer(toutes[, 1], seq_len(I), "==") * 1
  B <- t(vapply(seq_len(nrow(toutes)), function(k) dessin_site(fit$des, toutes[k, 1])[toutes[k, 2], ], numeric(fit$des$p)))
  if (fit$des$p == 1) B <- t(B)
  if (!fit$des$p) B <- matrix(0, nrow(toutes), 0)
  list(cases = toutes, obs = obs, X = cbind(A, B), f = t$f[toutes], w = t$w[toutes], I = I, p = fit$des$p)
}

# 1. Le maximum de vraisemblance de Poisson (ni σ² ni ρ) : glm() sur le même dessin.
mml <- trim_de(ps, 3, FALSE, FALSE)
dc <- dessin_complet(mml)
gl <- stats::glm(dc$f[dc$obs] ~ 0 + dc$X[dc$obs, ], offset = -log(dc$w[dc$obs]), family = stats::poisson())
pb <- dc$I + seq_len(dc$p)
verifier("modèle 3, maximum de vraisemblance : les paramètres du temps, leurs erreurs-types, les effets des sites et μ de glm(poisson)",
         proche(mml$ajustement_noyau$beta, stats::coef(gl)[pb], 1e-8) && proche(sqrt(diag(mml$ajustement_noyau$V)), sqrt(diag(stats::vcov(gl)))[pb], 1e-7) &&
           proche(mml$ajustement_noyau$alpha, stats::coef(gl)[seq_len(dc$I)], 1e-8) && proche(mml$ajustement_noyau$mu[dc$cases[dc$obs, ]], stats::fitted(gl), 1e-8))
verifier(sprintf("un site sans comptage positif laissé de côté (%s) et compté ; %d sites, %d temps, %d comptages observés", paste(mml$donnees$sites_vides, collapse = ", "), mml$I, mml$J, mml$O),
         identical(mml$donnees$sites_vides, "S30") && mml$I == 29 && mml$J == 12 && mml$O == sum(!is.na(sim$compte) & sim$site != "S30"))
verifier("le khi² de Pearson, le rapport de vraisemblance (la déviance de glm, les sites concentrés), AIC = LR − 2 ddl, ddl = O − I − p",
         proche(mml$ajustement$chi2, sum(stats::residuals(gl, "pearson")^2), 1e-8) && proche(mml$ajustement$LR, stats::deviance(gl), 1e-8) &&
           mml$ajustement$ddl == stats::df.residual(gl) && proche(mml$ajustement$AIC, stats::deviance(gl) - 2 * stats::df.residual(gl), 1e-8))

# 2. La surdispersion seule : les estimations du maximum de vraisemblance, σ² = khi² / (O − p), les
# erreurs-types de quasipoisson.
msd <- trim_de(ps, 3, TRUE, FALSE)
gq <- stats::glm(dc$f[dc$obs] ~ 0 + dc$X[dc$obs, ], offset = -log(dc$w[dc$obs]), family = stats::quasipoisson())
# (la dispersion de quasipoisson d'après ses résidus de Pearson : summary.glm la prend des poids de
# son dernier tour, à 2e-8 près)
verifier(sprintf("surdispersion seule : les mêmes estimations, σ² = %.4f celui de quasipoisson, ses erreurs-types", msd$sigma2),
         proche(msd$ajustement_noyau$beta, stats::coef(gq)[pb], 1e-8) && proche(msd$sigma2, sum(stats::residuals(gq, "pearson")^2) / stats::df.residual(gq), 1e-10) &&
           proche(sqrt(diag(msd$ajustement_noyau$V)), sqrt(diag(stats::vcov(gq)))[pb], 1e-7) && is.na(msd$rho))

# 3. Les covariables (l'habitat des sites, la gestion des sites et des temps), les poids, le modèle 2 à
# points de changement : glm() sur le dessin écrit à la main.
B0 <- function(J, cp) { fins <- c(cp[-1], J); sapply(seq_along(cp), function(l) pmin(pmax(seq_len(J) - cp[l], 0), fins[l] - cp[l])) }
mcov <- trim_de(ps, 2, FALSE, FALSE, cov = c("habitat", "gestion"), cp = c(2000, 2007), poids = "poids")
t <- mcov$donnees
g <- which(!is.na(t$f), arr.ind = TRUE)
b0 <- B0(12, c(1, 8))[g[, 2], ]
hab <- t$cov[[1]][g] == 2
ges <- t$cov[[2]][g] == 2
Xm <- cbind(stats::model.matrix(~ 0 + factor(g[, 1])), b0, b0 * hab, b0 * ges)
gc <- stats::glm(t$f[g] ~ 0 + Xm, offset = -log(t$w[g]), family = stats::poisson())
verifier("modèle 2 (2000, 2007), habitat (des sites) et gestion (qui change en 2006 : des sites et des temps), poids : les paramètres et erreurs-types de glm()",
         proche(mcov$ajustement_noyau$beta, utils::tail(stats::coef(gc), 6), 1e-8) && proche(sqrt(diag(mcov$ajustement_noyau$V)), utils::tail(sqrt(diag(stats::vcov(gc))), 6), 1e-7) &&
           identical(t$covariables[[2]]$categories, c("extensive", "intensive")) && nrow(mcov$parametres) == 6)
sans_lignes <- sim[!(sim$site == "S06" & sim$annee %in% c(2004, 2008)), ]  # (S06 : gestion intensive à partir de 2006, poids 1 ; S05 : poids 4)
sans_lignes <- sans_lignes[!(sans_lignes$site == "S05" & sans_lignes$annee == 2011), ]
mtr <- trim_de(projet_trim(sans_lignes), 2, FALSE, FALSE, cov = c("habitat", "gestion"), cp = c(2000, 2007), poids = "poids")
verifier("une case sans ligne prend la catégorie et le poids du temps d'avant de son site (du suivant au début) : son μ refait à la main",
         {
           t2 <- mtr$donnees
           v <- mtr$ajustement_noyau
           i <- match("S06", t2$sites)
           mu <- function(i, j) exp(v$alpha[i] + sum(B0(12, c(1, 8))[j, ] * (v$beta[1:2] + v$beta[3:4] * (t2$cov[[1]][i, j] == 2) + v$beta[5:6] * (t2$cov[[2]][i, j] == 2)))) / t2$w[i, j]
           t2$cov[[2]][i, 5] == 1 && t2$cov[[2]][i, 9] == 2 && t2$w[match("S05", t2$sites), 12] == 4 && proche(v$mu[i, 9], mu(i, 9), 1e-10) && proche(v$mu[i, 5], mu(i, 5), 1e-10)
         })

# 4. La corrélation sérielle et la surdispersion : les équations GEE résolues, σ² et ρ des résidus,
# la covariance des paramètres et des totaux avec la matrice d'information de tous les paramètres.
equations_gee <- function(m) {
  dc <- dessin_complet(m)
  v <- m$ajustement_noyau
  J <- length(m$donnees$temps)
  sig2 <- if (is.na(m$sigma2)) 1 else m$sigma2
  rho <- if (is.na(m$rho)) 0 else m$rho
  o <- which(dc$obs)
  mu <- v$mu[dc$cases]
  R <- rho^abs(outer(dc$cases[o, 2], dc$cases[o, 2], "-")) * outer(dc$cases[o, 1], dc$cases[o, 1], "==")
  Vf <- sig2 * sqrt(outer(mu[o], mu[o])) * R
  D <- mu[o] * dc$X[o, , drop = FALSE]
  Vinv <- solve(Vf)
  U <- drop(t(D) %*% Vinv %*% (dc$f[o] - mu[o]))
  echelle <- max(abs(t(D) %*% Vinv %*% dc$f[o]))
  info <- t(D) %*% Vinv %*% D
  Vt <- solve(info)
  r <- (dc$f - mu) / sqrt(mu)
  Rm <- matrix(NA, dc$I, J)
  Rm[dc$cases] <- r
  Rm[is.na(m$donnees$f)] <- NA
  paires <- Rm[, -J] * Rm[, -1]
  # la covariance des totaux du modèle et des totaux imputés (rtrim (69), (81)) avec les matrices entières
  C <- outer(seq_len(J), dc$cases[, 2], "==") * 1
  wmu <- dc$w * mu
  Dall <- wmu * dc$X
  Vmod <- C %*% Dall %*% Vt %*% t(Dall) %*% t(C)
  Co <- C[, o, drop = FALSE]
  Vobs <- Co %*% (dc$w[o] * Vf * rep(dc$w[o], each = length(o))) %*% t(Co)
  Vimp <- Vobs + Vmod - Co %*% Dall[o, , drop = FALSE] %*% Vt %*% t(Dall[o, , drop = FALSE]) %*% t(Co)
  list(U = max(abs(U)) / echelle, Vbeta = Vt[dc$I + seq_len(dc$p), dc$I + seq_len(dc$p), drop = FALSE],
       sig2 = sum(r[o]^2) / (length(o) - dc$I - dc$p), rho = sum(paires, na.rm = TRUE) / (sum(!is.na(paires)) * sig2), Vmod = Vmod, Vimp = Vimp)
}
mgee <- trim_de(ps, 3, TRUE, TRUE, poids = "poids")
eq <- equations_gee(mgee)
verifier(sprintf("modèle 3, σ² = %.3f et ρ = %.3f, poids : les équations GEE de tous les paramètres résolues (écart %.1e), σ² et ρ des résidus de Pearson", mgee$sigma2, mgee$rho, eq$U),
         eq$U < 1e-9 && proche(mgee$sigma2, eq$sig2, 1e-8) && proche(mgee$rho, eq$rho, 1e-8))
verifier("la covariance des paramètres du temps : le bloc de l'inverse de l'information de tous les paramètres (sites compris)", proche(mgee$ajustement_noyau$V, eq$Vbeta, 1e-7))
verifier("la covariance des totaux du modèle et des totaux imputés : rtrim (69) et (81) avec les matrices entières",
         proche(mgee$V_totaux$modele, eq$Vmod, 1e-7) && proche(mgee$V_totaux$impute, eq$Vimp, 1e-7))
mgee2 <- trim_de(ps, 2, TRUE, TRUE, cov = c("habitat", "gestion"), cp = c(2000, 2007, 2009))
eq2 <- equations_gee(mgee2)
verifier("modèle 2 à trois points, habitat et gestion, σ² et ρ : les équations résolues, σ², ρ, les covariances des paramètres et des totaux",
         eq2$U < 1e-9 && proche(mgee2$sigma2, eq2$sig2, 1e-8) && proche(mgee2$rho, eq2$rho, 1e-8) && proche(mgee2$ajustement_noyau$V, eq2$Vbeta, 1e-7) &&
           proche(mgee2$V_totaux$modele, eq2$Vmod, 1e-7) && proche(mgee2$V_totaux$impute, eq2$Vimp, 1e-7))
m1 <- trim_de(ps, 1, TRUE, TRUE)
eq1 <- equations_gee(m1)
verifier("modèle 1 (sans effet du temps), σ² et ρ : ses équations, σ² = khi² / (O − I), ρ ; ses indices du modèle valent 1",
         eq1$U < 1e-9 && proche(m1$sigma2, eq1$sig2, 1e-8) && proche(m1$rho, eq1$rho, 1e-8) && proche(m1$V_totaux$modele, eq1$Vmod, 1e-7) &&
           proche(m1$V_totaux$impute, eq1$Vimp, 1e-7) && all(abs(m1$indices$modele - 1) < 1e-12) && is.null(m1$parametres) && !nrow(m1$tests))

# 5. Les totaux, les indices et leurs erreurs-types ; la pente globale par lm() et sa covariance.
tt <- mgee$totaux
f <- mgee$donnees$f
w <- mgee$donnees$w
mu <- mgee$ajustement_noyau$mu
verifier("les totaux observés, du modèle et imputés de chaque temps (comptages pondérés)",
         proche(tt$observe, colSums(w * f, na.rm = TRUE), 1e-12) && proche(tt$modele, colSums(w * mu), 1e-12) && proche(tt$impute, colSums(ifelse(is.na(f), w * mu, w * f)), 1e-12))
ix <- mgee$indices
b <- 1
vt <- mgee$V_totaux$impute
verifier("les indices imputés (temps de base 2000) et leurs erreurs-types par la méthode delta, 0 au temps de base",
         proche(ix$impute, tt$impute / tt$impute[b], 1e-12) && ix$se_impute[b] == 0 &&
           proche(ix$se_impute[-b], sqrt((tt$impute[-b] / tt$impute[b])^2 * (diag(vt)[-b] / tt$impute[-b]^2 + vt[b, b] / tt$impute[b]^2 - 2 * vt[b, -b] / (tt$impute[-b] * tt$impute[b]))), 1e-9))
mb <- trim_de(ps, 3, TRUE, TRUE, poids = "poids", base = 2004)
verifier("un autre temps de base (2004) : les indices rapportés à lui, son erreur-type 0", mb$base == 5 && mb$indices$se_impute[5] == 0 &&
           proche(mb$indices$impute, tt$impute / tt$impute[5], 1e-10))
y <- log(tt$impute)
x <- seq_along(y)
ol <- stats::lm(y ~ x)
P <- solve(crossprod(cbind(1, x))) %*% t(cbind(1, x))
se <- sqrt((P %*% (vt / outer(tt$impute, tt$impute)) %*% t(P))[2, 2])
pg <- mgee$pentes[1, ]
verifier(sprintf("la pente globale imputée : celle de lm(log(totaux) ~ temps) (%.4f, ×%.4f), son erreur-type d'après la covariance des totaux, p et l'IC par t à J − 2 ddl",
                 pg$additive, pg$multiplicative),
         proche(pg$additive, stats::coef(ol)[2], 1e-10) && proche(pg$se, se, 1e-9) && proche(pg$p, 2 * stats::pt(-abs(stats::coef(ol)[2] / se), 10), 1e-8) &&
           proche(c(pg$bas, pg$haut), exp(stats::coef(ol)[2] + c(-1, 1) * stats::qt(0.975, 10) * se), 1e-9))
verifier("modèle 3 sans poids ni corrélation sérielle : indices imputés = indices du modèle (rtrim § 2.9)",
         proche(msd$indices$impute, msd$indices$modele, 1e-9) && all(msd$indices$se_impute[-1] > 0))
de <- mml$ecarts
lg <- stats::lm(c(0, mml$ajustement_noyau$beta) ~ seq_len(12))
verifier("modèle 3 sans covariable : la droite (β*, la pente de la droite des moindres carrés à travers les γ) et ses écarts, qui somment à 0 ; la pente globale du modèle est β*",
         proche(de$bstar, stats::coef(lg)[2], 1e-10) && proche(de$gstar, stats::residuals(lg), 1e-9) && abs(sum(de$gstar)) < 1e-10 &&
           proche(mml$pentes$additive[2], de$bstar, 1e-9) && proche(mml$pentes$se[2], de$se_bstar, 1e-8))
verifier("la classe d'une pente : l'intervalle à 95 % contre 1, 0,95 et 1,05 ; p < 0,01 quand celui à 99 % la donne aussi",
         identical(unlist(classe_pente(log(1.10), 0.01, 20)), c(classe = "forte_hausse", niveau = "0.01")) &&
           identical(classe_pente(log(1.07), 0.006, 10)$classe, "forte_hausse") && identical(classe_pente(log(1.07), 0.006, 10)$niveau, 0.05) && identical(classe_pente(log(1.07), 0.009, 10)$classe, "hausse_moderee") &&
           identical(classe_pente(log(1.03), 0.005, 20)$classe, "hausse_moderee") && identical(classe_pente(log(1.00), 0.01, 20)$classe, "stable") &&
           identical(classe_pente(log(1.00), 0.04, 20)$classe, "incertaine") && identical(classe_pente(log(0.97), 0.005, 20)$classe, "declin_modere") &&
           identical(classe_pente(log(0.90), 0.01, 20)$classe, "fort_declin") && identical(classe_pente(0, 0.01, 0)$classe, "inconnue"))
wt <- mgee2$tests
v2 <- mgee2$ajustement_noyau
dpente <- v2$beta[2] - v2$beta[1]
k <- c(2, 5, 8)
A <- matrix(0, 3, 9)
A[cbind(1:3, k)] <- 1
A[cbind(1:3, k - 1)] <- -1
th <- drop(A %*% v2$beta)
Wc <- drop(t(th) %*% solve(A %*% v2$V %*% t(A), th))
verifier("tests de Wald : le changement de pente en 2007 (référence, marais et gestion ensemble, 3 ddl), l'effet de chaque covariable",
         identical(wt$test, c("changement", "changement", "changement", "covariable", "covariable")) && proche(wt$W[2], Wc, 1e-8) && all(wt$ddl[1:3] == 3) &&
           proche(wt$W[4], drop(t(v2$beta[4:6]) %*% solve(v2$V[4:6, 4:6], v2$beta[4:6])), 1e-8) && wt$ddl[4] == 3)
ms <- trim_de(ps, 2, TRUE, TRUE)
verifier("modèle 2 à une seule pente : son test de Wald, W = β² / var(β)", identical(ms$tests$test, "pente") && proche(ms$tests$W, ms$ajustement_noyau$beta^2 / ms$ajustement_noyau$V[1, 1], 1e-10))

# 6. Les lignes : un temps sans aucune ligne ajouté (2004, aucun comptage cette année-là), des
# lignes sans comptage omises, le site vide omis : les mêmes résultats que le tableau complet.
sim_trou <- sim
sim_trou$compte[sim_trou$annee == 2004] <- NA
plein <- trim_de(projet_trim(sim_trou), 2, TRUE, TRUE, cp = c(2000, 2006))
troue <- sim_trou[sim_trou$annee != 2004 & !(is.na(sim_trou$compte) & seq_len(nrow(sim_trou)) %% 2 == 0) & sim_trou$site != "S30", ]
pt <- projet_trim(troue, "troué")
mt <- trim_de(pt, 2, TRUE, TRUE, cp = c(2000, 2006))
verifier(sprintf("sans les lignes de 2004, la moitié des lignes sans comptage et le site vide : 2004 ajouté (%s), les mêmes résultats", paste(mt$donnees$ajoutes, collapse = ", ")),
         identical(mt$donnees$ajoutes, 2004) && proche(mt$ajustement_noyau$beta, plein$ajustement_noyau$beta, 1e-8) && proche(mt$indices$se_impute, plein$indices$se_impute, 1e-7) &&
           proche(mt$sigma2, plein$sigma2, 1e-8) && length(mt$donnees$sites_vides) == 0)
verifier("modèle 3 sans aucun comptage en 2004 : refusé, le temps nommé", grepl("2004", refus(trim_de(pt, 3, TRUE, TRUE))))

# 7. Le pas à pas : un seul vrai changement de pente (en 2005), retrouvé parmi tous les temps.
set.seed(3)
pa <- expand.grid(annee = 2000:2014, site = sprintf("P%02d", 1:40), KEEP.OUT.ATTRS = FALSE)
pa$compte <- stats::rpois(nrow(pa), exp(stats::rnorm(40, 2.5, 0.7)[as.integer(factor(pa$site))] + ifelse(pa$annee <= 2005, 0.12 * (pa$annee - 2000), 0.6 - 0.15 * (pa$annee - 2005))))
pa$compte[stats::runif(nrow(pa)) < 0.2] <- NA
pa <- data.frame(nom = sprintf("q%03d", seq_len(nrow(pa))), site = as.character(pa$site), annee = pa$annee, compte = pa$compte, stringsAsFactors = FALSE)
ppa <- projet_trim(pa, "pas à pas")
mpa <- trim_de(ppa, 2, TRUE, FALSE, cp = "tous", pas_a_pas = TRUE)
verifier(sprintf("pas à pas parmi les 14 premiers temps : gardés %s (le vrai changement en 2005 et le départ en 2000)", paste(mpa$changements, collapse = ", ")),
         all(c(2000, 2005) %in% mpa$changements) && length(mpa$changements) <= 3 && nrow(mpa$pas_a_pas$etapes) >= 11 && identical(mpa$pas_a_pas$depart, 2000:2013 + 0))

# ---------------------------------------------------------------- contre rtrim

if (avec_rtrim) {
  cat("Contre rtrim", as.character(utils::packageVersion("rtrim")), "\n")
  rtrim_de <- function(data, modele, sd, sc, cov = NULL, cp = NULL, poids = NULL, pas_a_pas = FALSE, temps = "year") {
    args <- list(object = stats::as.formula(paste("count ~ site +", temps, if (length(cov)) paste("+", paste(cov, collapse = " + ")) else "")), data = data,
                 model = modele, overdisp = sd, serialcor = sc, conv_crit = 1e-11, max_iter = 2000)
    if (modele == 2) args$changepoints <- if (is.null(cp)) 1L else cp
    if (pas_a_pas) args$stepwise <- TRUE
    if (!is.null(poids)) args$weights <- poids
    utils::capture.output(z <- suppressWarnings(do.call(rtrim::trim, args)))
    z
  }
  # Tout ce que les deux donnent, comparé à `tol` près (écart relatif, ou absolu sous 1).
  comparer <- function(nom, m, z, tol = 1e-6) {
    v <- m$ajustement_noyau
    ix <- rtrim::index(z, "both")
    tot <- function(w) { o <- rtrim::overall(z, w)$slope; o }
    oi <- tot("imputed")
    of <- tot("fitted")
    g <- rtrim::gof(z)
    co <- stats::coef(z)
    w <- suppressWarnings(rtrim::wald(z))  # (rtrim : pas de test des écarts avec des covariables)
    sens <- c(forte_hausse = "Strong increase", hausse_moderee = "Moderate increase", stable = "Stable", incertaine = "Uncertain",
              declin_modere = "Moderate decrease", fort_declin = "Strong decrease")
    sens_de <- function(pl) if (pl$classe %in% c("stable", "incertaine")) unname(sens[pl$classe]) else sprintf("%s (p<%s)", sens[pl$classe], format(pl$niveau))
    params <- if (!is.null(co)) {
      nos <- m$parametres
      proche(nos$additif, co$add, tol) && proche(nos$se_additif, co$se_add, tol) && proche(nos$multiplicatif, co$mul, tol) && proche(nos$se_multiplicatif, co$se_mul, tol)
    } else is.null(m$parametres)
    nos_w <- m$tests
    ses_w <- rbind(if (!is.null(w$slope)) data.frame(test = "pente", W = w$slope$W, df = w$slope$df, p = w$slope$p),
                   if (!is.null(w$dslope)) data.frame(test = "changement", W = w$dslope$W, df = w$dslope$df, p = w$dslope$p),
                   if (!is.null(w$deviations)) data.frame(test = "deviations", W = w$deviations$W, df = w$deviations$df, p = w$deviations$p),
                   if (!is.null(w$covar)) data.frame(test = "covariable", W = w$covar$W, df = w$covar$df, p = w$covar$p))
    tests <- if (is.null(ses_w)) !nrow(nos_w) else identical(nos_w$test, ses_w$test) && proche(nos_w$W, ses_w$W, tol) && all(nos_w$ddl == ses_w$df) && proche(nos_w$p, ses_w$p, tol)
    res <- c(parametres = params,
             totaux = proche(m$totaux$modele, z$tt_mod, tol) && proche(m$totaux$impute, z$tt_imp, tol) && proche(m$totaux$se_modele, sqrt(diag(z$var_tt_mod)), tol) &&
               proche(m$totaux$se_impute, sqrt(diag(z$var_tt_imp)), tol) && all(abs(m$totaux$observe - z$time.totals$observed) <= 0.5 + 1e-9),  # (rtrim les arrondit)
             indices = proche(m$indices$modele, ix$fitted, tol) && proche(m$indices$se_modele, ix$se_fit, tol) && proche(m$indices$impute, ix$imputed, tol) && proche(m$indices$se_impute, ix$se_imp, tol),
             pentes = proche(m$pentes$additive, c(oi$add, of$add), tol) && proche(m$pentes$se, c(oi$se_add, of$se_add), tol) && proche(m$pentes$multiplicative, c(oi$mul, of$mul), tol) &&
               proche(m$pentes$se_multiplicative, c(oi$se_mul, of$se_mul), tol) && proche(m$pentes$p, c(oi$p, of$p), 1e-5),
             classes = identical(c(sens_de(m$pentes[1, ]), sens_de(m$pentes[2, ])), c(oi$meaning, of$meaning)),
             dispersion = (if (is.null(z$sig2)) is.na(m$sigma2) else proche(m$sigma2, z$sig2, tol)) && (if (is.null(z$rho)) is.na(m$rho) else proche(m$rho, z$rho, tol)),
             ajustement = proche(m$ajustement$chi2, g$chi2$chi2, tol) && proche(m$ajustement$LR, g$LR$LR, tol) && m$ajustement$ddl == g$LR$df && proche(m$ajustement$AIC, g$AIC, tol) &&
               proche(m$ajustement$p_chi2, g$chi2$p, 1e-5),
             wald = tests)
    verifier(sprintf("%s : %s", nom, if (all(res)) "paramètres, erreurs-types, totaux, indices, pentes et classes, σ², ρ, khi², LR, AIC, Wald" else paste("écart :", paste(names(res)[!res], collapse = ", "))),
             all(res))
    if (!all(res)) cat(sprintf("    β %.1e, totaux %.1e, indices %.1e, pentes %.1e | %s / %s\n", if (length(z$beta)) ecart(v$beta, z$beta) else 0, ecart(m$totaux$se_impute, sqrt(diag(z$var_tt_imp))),
                               ecart(m$indices$se_impute, ix$se_imp), ecart(m$pentes$se, c(oi$se_add, of$se_add)), paste(sens_de(m$pentes[1, ]), sens_de(m$pentes[2, ])), paste(oi$meaning, of$meaning)))
  }
  e <- new.env()
  utils::data(list = c("skylark", "skylark2", "oystercatcher"), package = "rtrim", envir = e)
  sk <- e$skylark2
  pk <- projet_trim(data.frame(nom = sprintf("r%03d", seq_len(nrow(sk))), site = as.character(sk$site), annee = sk$year, compte = sk$count,
                               habitat = as.character(sk$habitat), depot = as.character(sk$deposition), poids = sk$weight, stringsAsFactors = FALSE), "skylark2")
  cat(sprintf("  skylark2 : %d lignes, %d comptages manquants\n", nrow(sk), sum(is.na(sk$count))))
  for (k in list(list(3, FALSE, FALSE), list(3, TRUE, FALSE), list(3, FALSE, TRUE), list(3, TRUE, TRUE), list(2, FALSE, FALSE), list(2, TRUE, TRUE))) {
    comparer(sprintf("skylark2, modèle %d%s%s", k[[1]], if (k[[2]]) ", surdispersion" else "", if (k[[3]]) ", corrélation sérielle" else ""),
             trim_de(pk, k[[1]], k[[2]], k[[3]]), rtrim_de(sk, k[[1]], k[[2]], k[[3]]))
  }
  comparer("skylark2, modèle 2 à points de changement 1984, 1987, 1989", trim_de(pk, 2, TRUE, TRUE, cp = c(1984, 1987, 1989)), rtrim_de(sk, 2, TRUE, TRUE, cp = c(1984, 1987, 1989)))
  comparer("skylark2, modèle 2 dont la pente commence en 1986 (aucune avant)", trim_de(pk, 2, TRUE, FALSE, cp = c(1986, 1989)), rtrim_de(sk, 2, TRUE, FALSE, cp = c(1986, 1989)))
  comparer("skylark2, modèle 3 avec l'habitat", trim_de(pk, 3, TRUE, TRUE, cov = "habitat"), rtrim_de(sk, 3, TRUE, TRUE, cov = "habitat"))
  comparer("skylark2, modèle 2 (1984, 1988) avec l'habitat", trim_de(pk, 2, TRUE, TRUE, cov = "habitat", cp = c(1984, 1988)), rtrim_de(sk, 2, TRUE, TRUE, cov = "habitat", cp = c(1984, 1988)))
  comparer("skylark2, modèle 2 avec l'habitat, une seule pente", trim_de(pk, 2, FALSE, FALSE, cov = "habitat"), rtrim_de(sk, 2, FALSE, FALSE, cov = "habitat"))
  comparer("skylark2, modèle 3 pondéré (poids 1 ou 10)", trim_de(pk, 3, TRUE, TRUE, poids = "poids"), rtrim_de(sk, 3, TRUE, TRUE, poids = "weight"))
  comparer("skylark2, modèle 2 pondéré avec l'habitat", trim_de(pk, 2, TRUE, FALSE, cov = "habitat", poids = "poids"), rtrim_de(sk, 2, TRUE, FALSE, cov = "habitat", poids = "weight"))
  sk2 <- sk
  sk2$deposition <- factor(sk2$deposition)
  comparer("skylark2, modèle 2 avec l'habitat et le dépôt (4 catégories)", trim_de(pk, 2, TRUE, TRUE, cov = c("habitat", "depot")), rtrim_de(sk2, 2, TRUE, TRUE, cov = c("habitat", "deposition")))
  mp <- trim_de(pk, 2, TRUE, TRUE, cp = "tous", pas_a_pas = TRUE)
  zp <- rtrim_de(sk, 2, TRUE, TRUE, cp = "auto")
  verifier(sprintf("skylark2, pas à pas depuis tous les temps : les mêmes points gardés (%s)", paste(mp$changements, collapse = ", ")),
           identical(mp$changements, as.numeric(zp$time.id[zp$changepoints])))
  comparer("skylark2, le modèle choisi pas à pas", mp, zp)
  mph <- trim_de(pk, 2, TRUE, FALSE, cov = "habitat", cp = "tous", pas_a_pas = TRUE)
  zph <- rtrim_de(sk, 2, TRUE, FALSE, cov = "habitat", cp = "auto")
  verifier(sprintf("skylark2, pas à pas avec l'habitat : les mêmes points gardés (%s)", paste(mph$changements, collapse = ", ")),
           identical(mph$changements, as.numeric(zph$time.id[zph$changepoints])))
  ib <- rtrim::index(rtrim_de(sk, 3, TRUE, TRUE), "imputed", base = 1987)
  mb <- trim_de(pk, 3, TRUE, TRUE, base = 1987)
  verifier("skylark2, indices rapportés à 1987 : ceux de rtrim::index(base = 1987)", proche(mb$indices$impute, ib$imputed, 1e-6) && proche(mb$indices$se_impute, ib$se_imp, 1e-6))
  # le modèle 1 : rtrim compte un paramètre du temps qu'il n'estime pas dans les degrés de liberté de σ²
  m1 <- trim_de(pk, 1, FALSE, TRUE)
  z1 <- rtrim_de(sk, 1, FALSE, TRUE)
  m1s <- trim_de(pk, 1, TRUE, FALSE)
  z1s <- rtrim_de(sk, 1, TRUE, FALSE)
  r1 <- (m1s$donnees$f - m1s$ajustement_noyau$mu) / sqrt(m1s$ajustement_noyau$mu)
  verifier("skylark2, modèle 1 : sans surdispersion, les effets des sites, ρ, les totaux et l'ajustement de rtrim ; avec, σ² = khi² / (O − I) quand rtrim divise par O − I − 1",
           proche(m1$ajustement_noyau$alpha, z1$alpha, 1e-6) && proche(m1$rho, z1$rho, 1e-6) && proche(m1$totaux$se_impute, sqrt(diag(z1$var_tt_imp)), 1e-6) &&
             proche(m1$ajustement$chi2, rtrim::gof(z1)$chi2$chi2, 1e-6) && proche(m1s$sigma2, sum(r1^2, na.rm = TRUE) / (m1s$O - m1s$I), 1e-10) &&
             proche(z1s$sig2, sum(r1^2, na.rm = TRUE) / (m1s$O - m1s$I - 1), 1e-6))
  # skylark : des sites et des temps numérotés, deux covariables entières (en catégories ici)
  s1 <- e$skylark
  p1 <- projet_trim(data.frame(nom = sprintf("r%03d", seq_len(nrow(s1))), site = s1$site, temps = s1$time, compte = s1$count, habitat = as.character(s1$Habitat),
                               stringsAsFactors = FALSE), "skylark")
  s1h <- s1
  s1h$Habitat <- factor(s1h$Habitat)
  comparer("skylark (sites et temps numérotés), modèle 3 avec Habitat", trim_de(p1, 3, TRUE, TRUE, cov = "habitat", temps = "temps"), rtrim_de(s1h, 3, TRUE, TRUE, cov = "Habitat", temps = "time"))
  # l'huîtrier pie : janvier de chaque année, 138 sites, 20 ans, 60 % de comptages manquants
  oc <- e$oystercatcher
  oc <- oc[oc$month == 1, c("site", "year", "count")]
  po <- projet_trim(data.frame(nom = sprintf("h%04d", seq_len(nrow(oc))), site = oc$site, annee = oc$year, compte = oc$count, stringsAsFactors = FALSE), "huîtrier pie")
  for (k in list(list(3, TRUE, TRUE), list(2, TRUE, FALSE), list(2, TRUE, FALSE, c(1995, 2002, 2008)))) {
    mo <- trim_de(po, k[[1]], k[[2]], k[[3]], cp = if (length(k) > 3) k[[4]])
    comparer(sprintf("huîtrier pie (janvier ; %d sites, %d ans, %d comptages, %d sites sans comptage positif), modèle %d%s", mo$I, mo$J, mo$O, length(mo$donnees$sites_vides), k[[1]],
                     if (length(k) > 3) " à points de changement" else if (k[[3]]) ", σ² et ρ" else ", σ²"),
             mo, rtrim_de(oc, k[[1]], k[[2]], k[[3]], cp = if (length(k) > 3) k[[4]]))
  }
  # (une corrélation sérielle estimée négative : rtrim refuse le modèle, Nuée le garde, ses équations résolues)
  mneg <- trim_de(po, 2, TRUE, TRUE, cp = c(1995, 2002, 2008))
  eqn <- equations_gee(mneg)
  verifier(sprintf("huîtrier pie, modèle 2 à points de changement, σ² et ρ : ρ = %.4f, que rtrim refuse (négatif) ; Nuée résout ses équations", mneg$rho),
           mneg$rho < 0 && grepl("rho .* < 0", refus(rtrim_de(oc, 2, TRUE, TRUE, cp = c(1995, 2002, 2008)))) && eqn$U < 1e-9 && proche(mneg$rho, eqn$rho, 1e-8))
  # les données simulées : le tableau complet pour rtrim, des lignes omises pour Nuée
  sz <- sim
  sz$count <- sz$compte
  sz$year <- sz$annee
  szt <- sim_trou
  szt$count <- szt$compte
  szt$year <- szt$annee
  comparer("simulées (30 sites, 12 ans, un site vide), modèle 3, σ² et ρ", trim_de(ps, 3, TRUE, TRUE), rtrim_de(sz, 3, TRUE, TRUE))
  comparer("simulées, modèle 2 à points de changement (2000, 2005), l'habitat, σ² et ρ", trim_de(ps, 2, TRUE, TRUE, cov = "habitat", cp = c(2000, 2005)), rtrim_de(sz, 2, TRUE, TRUE, cov = "habitat", cp = c(2000, 2005)))
  comparer("simulées sans les lignes de 2004 et la moitié des lignes sans comptage (rtrim : le tableau complet), modèle 2, poids", trim_de(pt, 2, TRUE, TRUE, cp = c(2000, 2006), poids = "poids"),
           rtrim_de(szt, 2, TRUE, TRUE, cp = c(2000, 2006), poids = "poids"))
  verifier("simulées : rtrim refuse une covariable qui change dans un site (la gestion), que Nuée prend (vérifiée plus haut contre glm())",
           grepl("Multiple covariate values", refus(rtrim_de(sz, 2, FALSE, FALSE, cov = "gestion"))))
}

# ---------------------------------------------------------------- refus

cat("Refus\n")
mal <- function(f) { x <- sim; f(x) }
refus_de <- function(df, ...) refus(trim_de(projet_trim(df), ...))
verifier("un comptage qui n'est pas un entier, un comptage négatif : refusés, l'observation nommée",
         grepl("r005 vaut 2.5", refus_de(mal(function(x) { x$compte[5] <- 2.5; x }))) && grepl("r007 vaut -1", refus_de(mal(function(x) { x$compte[7] <- -1; x }))))
deux <- rbind(sim, transform(sim[3, ], nom = "double"))
verifier("deux lignes pour le même site et le même temps : refusées, le site, le temps et les observations nommés",
         grepl("S01.*2002.*r003.*double", refus_de(deux)))
verifier("des temps irréguliers (… 2010, 2011,5) : refusés, la valeur nommée", grepl("2011.5", refus_de(mal(function(x) { x$annee[x$annee == 2011] <- 2011.5; x }))))
dates <- sim
dates$annee <- sprintf("%d-05-14", sim$annee)
verifier("un temps fait de dates : refusé", grepl("dates", refus_de(dates)))
verifier("Y qualitative, une covariable quantitative, un terme croisé, sans site ni temps, le site pris pour temps",
         grepl("qualitative", refus(trim_de(ps, y = "habitat"))) && grepl("poids", refus(trim_de(ps, cov = "poids"))) &&
           grepl("croisé", refus(ajuster(ps, definir_modele(ps, "TRIM", x = numeros(ps, c("habitat", "gestion")), y = numeros(ps, "compte"), echelle = "none",
                                                            croises = list(numeros(ps, c("habitat", "gestion"))), options = definition_trim(ps)$options)))) &&
           grepl("site de chaque", refus(ajuster(ps, definir_modele(ps, "TRIM", x = NULL, y = numeros(ps, "compte"), echelle = "none", options = list(temps = list(by = "variable", variable = numeros(ps, "annee"))))))) &&
           grepl("deux colonnes", refus(trim_de(ps, site = "annee"))))
verifier("un point de changement qui n'est pas un temps, le dernier temps en point de changement, un temps de base hors des données",
         grepl("1999", refus(trim_de(ps, 2, cp = c(1999)))) && grepl("2011", refus(trim_de(ps, 2, cp = c(2000, 2011)))) && grepl("1990", refus(trim_de(ps, base = 1990))))
k <- which(!is.na(sim$compte))[1]
verifier(sprintf("des poids nuls, un poids ou une covariable manquants là où un comptage est connu (%s) : refusés, l'observation nommée", sim$nom[k]),
         grepl("r002", refus_de(mal(function(x) { x$poids[2] <- 0; x }), poids = "poids")) &&
           grepl(sim$nom[k], refus_de(mal(function(x) { x$poids[k] <- NA; x }), poids = "poids")) &&
           grepl(paste0(sim$nom[k], ".*habitat"), refus_de(mal(function(x) { x$habitat[k] <- NA; x }), cov = "habitat")))
# (une seule année : chaque site n'y vient qu'une fois, et le tableau en fait un identifiant)
verifier("aucun comptage positif, un seul temps : refusés", grepl("positif", refus_de(mal(function(x) { x$compte <- ifelse(is.na(x$compte), NA, 0); x }))) &&
           grepl("deux temps", refus_de(sim[sim$annee == 2003, ], site = "id:site")))
options(nuee.langue = "en")
verifier("les refus en anglais", grepl("^Two rows for the same site and time", refus_de(deux)) && grepl("whole numbers", refus_de(mal(function(x) { x$compte[5] <- 2.5; x }))) &&
           grepl("evenly spaced", refus_de(mal(function(x) { x$annee[x$annee == 2011] <- 2011.5; x }))))
options(nuee.langue = "fr")
verifier("un jeu de test refusé (TRIM ne prédit pas)", grepl("jeu de test", refus(definir_modele(ps, "TRIM", x = NULL, y = numeros(ps, "compte"), echelle = "none", test = 1:5,
                                                                                               options = definition_trim(ps)$options))))

# ---------------------------------------------------------------- les observations du modèle

cat("Observations choisies, ce que l'interface reçoit, texte des méthodes, tableaux\n")
dc1 <- definition_trim(ps, 2, TRUE, TRUE, condition = "habitat == \"bocage\" & annee >= 2002")
mcond <- ajuster(ps, dc1)
lignes <- which(sim$habitat == "bocage" & sim$annee >= 2002)
mmain <- trim_de(ps, 2, TRUE, TRUE, observations = lignes)
verifier(sprintf("une condition (habitat == \"bocage\" & annee >= 2002) : le modèle des %d lignes cochées à la main", length(lignes)),
         identical(sort(mcond$observations), sort(as.numeric(lignes[sim$site[lignes] %in% mcond$donnees$sites]))) && proche(mcond$ajustement_noyau$beta, mmain$ajustement_noyau$beta, 1e-12) &&
           identical(mcond$donnees$temps, 2002:2011 + 0))
verifier("la vérification des données avant l'ajustement ne signale pas les comptages manquants de TRIM", identical(verifier_donnees(ps, definition_trim(ps)), verifier_trim(ps, NULL)))
pid <- ps
pid$observation_id_names <- c(pid$observation_id_names, "code du site")
pid$observations <- lapply(seq_along(pid$observations), function(k) { o <- pid$observations[[k]]; o$ids <- c(o$ids, sim$site[k]); o })
mid <- trim_de(pid, 3, TRUE, TRUE, site = "id:code du site")
verifier("le site donné par un identifiant des observations : le même modèle que par la variable", proche(mid$ajustement_noyau$beta, trim_de(ps, 3, TRUE, TRUE)$ajustement_noyau$beta, 1e-12) &&
           identical(mid$donnees$noms$site, "code du site"))

etat <- new.env()
etat$projets <- list(P1 = c(ps, list(id = "P1")))
class(etat$projets$P1) <- class(ps)
etat$modeles <- list()
projet <- function(id) etat$projets[[id]]
modele <- function(id) etat$modeles[[id]]
garder <- function(id, d, m) { etat$modeles[[id]] <- list(projet = "P1", definition = d, modele = m); id }
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE)
dg <- definition_trim(ps, 2, TRUE, TRUE, cov = "habitat", cp = c(2000, 2005), poids = "poids")
id <- garder("M1", dg, ajuster(ps, dg))
r <- resultat_trim(id)
verifier("ce que l'interface reçoit : une analyse sans prédiction, ses vues, ses lignes, ses matrices sites × temps, en JSON",
         isTRUE(r$analyse) && identical(r$prediction, FALSE) && identical(as.character(r$vues_defaut), c("trim_indices", "trim_totaux")) && grepl("^pente ", r$chiffres) &&
           nrow(r$observations) == r$N && all(c("uid", "nom", "site", "temps", "compte", "ajuste", "impute", "residu") %in% names(r$observations)) &&
           identical(dim(r$comptes), c(29L, 12L)) && identical(dim(r$ajustes), c(29L, 12L)) && nrow(r$indices) == 12 && nrow(r$pentes) == 2 && inherits(json(r), "json"))
verifier("ses réglages pour Modifier le modèle (les options, la covariable en X)", identical(r$reglages$options$modele, 2) && identical(as.numeric(r$reglages$x), numeros(ps, "habitat")) &&
           identical(as.numeric(r$reglages$options$poids), numeros(ps, "poids")))
options(nuee.langue = "en")
verifier("en anglais, sa ligne de l'explorateur", grepl("^slope [0-9.]+ · (strong|moderate|stable|uncertain|steep)", resultat_trim("M1")$chiffres))
options(nuee.langue = "fr")
for (langue in c("fr", "en")) {
  tm <- texte_methodes(ps, dg, etat$modeles$M1$modele, langue, "essai")
  t3 <- texte_methodes(ps, definition_trim(pt, 3), trim_de(pt, 2, TRUE, TRUE, cp = c(2000, 2006)), langue, "essai")
  tp <- texte_methodes(ppa, definition_trim(ppa, 2, cp = "tous", pas_a_pas = TRUE), mpa, langue, "essai")
  fr <- langue == "fr"
  verifier(sprintf("texte des méthodes (%s) : les données, le modèle 2, la covariable, les poids, GEE, la pente et sa classe, l'ajustement, Wald ; le temps ajouté ; le pas à pas ; les références", langue),
           length(tm$paragraphes) == 5 && grepl(if (fr) "modèle 2 de TRIM" else "TRIM model 2", tm$paragraphes[2]) && grepl("habitat", tm$paragraphes[2]) &&
             grepl(if (fr) "pondérés" else "weighted", tm$paragraphes[2]) && grepl("Liang", tm$paragraphes[2]) &&
             grepl(libelle_classe_trim(etat$modeles$M1$modele$pentes$classe[1], fr), tm$paragraphes[3]) && grepl("AIC", tm$paragraphes[4]) && grepl("Wald", tm$paragraphes[4]) &&
             grepl(if (fr) "Un site sans aucun comptage positif a été laissé de côté" else "One site without any positive count was left out", tm$paragraphes[1]) &&
             grepl(if (fr) "Le temps 2004 n'avait aucune ligne" else "Time 2004 had no row", t3$paragraphes[1]) &&
             grepl(if (fr) "pas à pas" else "stepwise", tp$paragraphes[2]) &&
             any(grepl("TRIM 3 manual", tm$references)) && any(grepl("Liang", tm$references)) && any(grepl("rtrim", tm$references)) && any(grepl("R Core Team", tm$references)))
}
tr <- tables_resultats(ps, etat$modeles$M1$modele)
verifier("tableaux exportés : des data.frame (résumé, indices, totaux, pentes, paramètres, tests, ajustement, sites, observations)",
         all(vapply(tr, is.data.frame, TRUE)) && all(c("resume", "indices", "totaux", "pentes", "parametres", "tests", "ajustement", "sites", "observations") %in% names(tr)) &&
           nrow(tr$observations) == etat$modeles$M1$modele$N && names(tr$observations)[1] == "Observation")
t3 <- tables_resultats(ps, mml)
verifier("modèle 3 sans covariable : la droite et ses écarts exportés ; le pas à pas : ses étapes", all(c("droite", "ecarts") %in% names(t3)) && "pas_a_pas" %in% names(tables_resultats(ppa, mpa)))
dossier <- tempfile("trim")
dir.create(dossier)
ch <- ecrire_tables(tr, file.path(dossier, "trim.xlsx"), objet = "trim")
verifier("écrits en Excel", length(ch) == 1 && file.exists(ch))
unlink(dossier, recursive = TRUE)

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
