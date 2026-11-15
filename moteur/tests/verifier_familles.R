# Vérifie les deux familles étendues de mgcv des régressions (moteur/R/regression.R) : le Poisson à
# excès de zéros, ziP(), et la loi bêta, betar(), contre mgcv::gam() appelé ici sur les mêmes lignes
# (coefficients et erreurs-types, edf, AIC et vraisemblance, θ1 et θ2, φ, la variance d'un effet
# aléatoire, les valeurs ajustées, les prédictions de nouvelles observations et du jeu de test, la
# validation croisée refaite à la main sur deux groupes), leurs résidus quantiles randomisés contre
# leur définition écrite ici (Dunn et Smyth, 1996 : la loi du Poisson à excès de zéros telle que mgcv
# la définit, un zéro de probabilité 1 − p puis la loi de Poisson tronquée en zéro ; pbeta de formes
# μ φ et (1 − μ) φ), les résidus de Pearson du Poisson à excès de zéros contre ses moments, les
# ensembles AIC (avec Poisson et la binomiale négative ; avec la loi normale), la sélection par
# l'AIC, le lissage spatial, les refus, les coefficients côte à côte et le texte des Méthodes en
# français et en anglais.
#   Rscript moteur/tests/verifier_familles.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-6) isTRUE(all.equal(as.vector(unclass(a)), as.vector(unclass(b)), tolerance = tol, check.attributes = FALSE))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
env <- new.env(parent = asNamespace("mgcv"))  # (les formules de référence avec s() de mgcv, sans l'attacher)
gam_ref <- function(formule, ...) mgcv::gam(stats::as.formula(formule, env = env), ...)

# huit sites de 30 relevés, deux zones de quatre sites, trois habitats, un effort ; des comptages à
# excès de zéros tirés de la loi de ziP (θ1 = −0,8, θ2 = 1,2 : log(−log(1 − p)) = θ1 + θ2 log(λ)) et
# des proportions de loi bêta (φ = 7), une courbe de x1, un effet du site
set.seed(17)
n <- 240
tf <- data.frame(nom = sprintf("F%03d", 1:n), site = rep(sprintf("S%d", 1:8), each = 30), x1 = round(runif(n), 4), x2 = round(rnorm(n), 4),
                 effort = round(runif(n, 1, 3), 2), habitat = rep(c("bois", "haie", "prairie"), length.out = n),
                 X = round(runif(n, 0, 10), 3), Y = round(runif(n, 0, 10), 3), stringsAsFactors = FALSE)
tf$ordre <- seq_len(n)
tf$zone <- ifelse(tf$site %in% c("S1", "S2", "S3", "S4"), "nord", "sud")
effet_site <- rnorm(8, 0, 0.3)[as.integer(factor(tf$site))]
g <- -0.2 + 0.9 * sin(3 * tf$x1) + 0.4 * tf$x2 + effet_site + log(tf$effort) + c(bois = 0.3, haie = -0.2, prairie = 0)[tf$habitat]
lambda <- exp(g)
p_vrai <- 1 - exp(-exp(-0.8 + 1.2 * g))
present <- runif(n) < p_vrai
tf$compte <- 0
tf$compte[present] <- qpois(runif(sum(present), dpois(0, lambda[present]), 1), lambda[present])
mu <- plogis(-0.3 + 0.8 * sin(3 * tf$x1) - 0.4 * tf$x2 + effet_site + c(bois = 0.4, haie = -0.2, prairie = 0)[tf$habitat] + 0.4 * sin(tf$X / 2))
tf$prop <- round(rbeta(n, mu * 7, (1 - mu) * 7), 6)
pf <- projet_depuis_tableau(tf, "familles")
uf <- function(...) numeros(pf, c(...))
obs <- numeros_observations(pf)
modele_f <- function(type, y, x, ...) definir_modele(pf, type, x = uf(x), y = uf(y), echelle = "none", ...)
# le tableau de référence : la catégorie de référence de Nuée, « prairie » (la dernière), hors des indicatrices
rdf <- transform(tf, bois = as.numeric(habitat == "bois"), haie = as.numeric(habitat == "haie"), site = factor(site))
cat(sprintf("Données : %d relevés, %.0f %% de comptages nuls, proportions de %.3f à %.3f\n", n, 100 * mean(tf$compte == 0), min(tf$prop), max(tf$prop)))

cat("Poisson à excès de zéros (ziP), un GLM avec un effort et un jeu de test\n")
test <- obs[seq(5, n, by = 12)]
dz <- modele_f("GLM", "compte", c("x1", "x2", "habitat"), famille = "poisson_zi", decalage = uf("effort"), G = 2, test = test)
mz <- ajuster(pf, dz)
appris <- !obs %in% test
rz <- mgcv::gam(compte ~ x1 + x2 + bois + haie + offset(log(effort)), family = mgcv::ziP(), data = rdf[appris, ], method = "ML")
co <- mz$resultat$coefficients
sz <- summary(rz)$p.table
verifier(sprintf("coefficients, erreurs-types, z et p = mgcv::gam(family = ziP(), method = \"ML\") ; la catégorie de référence « prairie » (%s)",
                 paste(co$terme, collapse = ", ")),
         identical(co$terme, c("(Intercept)", "x1", "x2", "habitat(bois)", "habitat(haie)")) && proche(co$estimation, sz[, 1]) && proche(co$erreur_type, sz[, 2]) &&
           proche(co$t, sz[, 3]) && proche(co$p, sz[, 4]) && proche(co$bas, sz[, 1] - qnorm(0.975) * sz[, 2]) && all(is.na(co$ddl)))
th <- rz$family$getTheta(TRUE)
verifier(sprintf("θ1 = %.3f et θ2 = %.3f, ceux du résumé de mgcv (θ2 = b + exp(θ2 interne)) ; le lien log de μ, que mgcv dit « identity »", th[1], th[2]),
         identical(mz$resultat$parametre_loi$nom, c("theta1", "theta2")) && proche(mz$resultat$parametre_loi$valeur, th) && identical(mz$lien, "log") &&
           identical(rz$family$link, "identity"))
verifier(sprintf("AIC (%.1f), vraisemblance, BIC et 7 paramètres (5 coefficients, θ1 et θ2) = mgcv ; la déviance expliquée, sans R² ajusté", mz$resultat$AIC),
         proche(mz$resultat$AIC, AIC(rz)) && proche(mz$resultat$logLik, as.numeric(logLik(rz))) && proche(mz$resultat$BIC, BIC(rz)) && abs(mz$resultat$parametres - 7) < 1e-6 &&
           proche(mz$resultat$r2$deviance_expliquee, summary(rz)$dev.expl) && is.null(mz$resultat$r2$r2_ajuste) && is.null(summary(rz)$r.sq))
# (la vraisemblance de ziP écrite ici : log(1 − p) pour un zéro, log p + log P(X = y) − log P(X > 0) sinon)
gz <- rz$linear.predictors
lz <- exp(gz)
pz <- 1 - exp(-exp(th[1] + th[2] * gz))
yz <- tf$compte[appris]
ll <- sum(ifelse(yz == 0, log(1 - pz), log(pz) + dpois(yz, lz, log = TRUE) - log(1 - exp(-lz))))
verifier(sprintf("cette vraisemblance est celle de la loi écrite ici (%.3f), qui redonne celle de Poisson quand θ1 = 0 et θ2 = 1", ll),
         proche(mz$resultat$logLik, ll, 1e-8) &&
           proche(as.numeric(logLik(mgcv::gam(compte ~ x1 + x2 + bois + haie + offset(log(effort)), family = mgcv::ziP(theta = c(0, 0)), data = rdf[appris, ], method = "ML"))),
                  as.numeric(logLik(glm(compte ~ x1 + x2 + bois + haie + offset(log(effort)), family = poisson, data = rdf[appris, ]))), 1e-6))
attendu <- pz * lz / (1 - exp(-lz))
verifier("les valeurs ajustées : la valeur attendue p μ / (1 − e^(−μ)), predict(type = \"response\") de mgcv, non fitted() (log μ)",
         proche(mz$resultat$ajuste, attendu, 1e-8) && proche(mz$resultat$ajuste, predict(rz, type = "response"), 1e-8) && !proche(mz$resultat$ajuste, fitted(rz), 1e-2))
nd <- rdf[!appris, ]
verifier(sprintf("le jeu de test (%d relevés) : prédit par mgcv (sa valeur attendue, l'effort compris)", sum(!appris)),
         proche(mz$test$observations[["compte prédit"]], predict(rz, nd, type = "response"), 1e-8) &&
           proche(mz$test$statistiques$RMSEP, sqrt(mean((predict(rz, nd, type = "response") - nd$compte)^2)), 1e-8))
pred_nuee <- predire(pf, mz, dz, obs[1:6])[["compte prédit"]]
verifier("Prédire : des observations du projet, comme predict(type = \"response\")", proche(pred_nuee, predict(rz, rdf[1:6, ], type = "response"), 1e-8))
# la validation croisée refaite à la main : deux groupes, les relevés à tour de rôle
groupes <- (seq_len(sum(appris)) - 1) %% 2 + 1
vc <- rep(NA_real_, sum(appris))
for (k in 1:2) {
  a <- mgcv::gam(compte ~ x1 + x2 + bois + haie + offset(log(effort)), family = mgcv::ziP(), data = rdf[appris, ][groupes != k, ], method = "ML")
  vc[groupes == k] <- predict(a, rdf[appris, ][groupes == k, ], type = "response")
}
verifier(sprintf("validation croisée à la main sur deux groupes : chaque groupe prédit par ziP ajusté sans lui (Q2 = %.3f)", mz$vc$Q2),
         mz$G == 2 && proche(mz$vc$prediction, vc, 1e-6) && proche(mz$vc$Q2, 1 - sum((yz - vc)^2) / sum((yz - mean(yz))^2), 1e-6) &&
           proche(mz$vc$RMSEcv, sqrt(mean((yz - vc)^2)), 1e-6))

cat("Poisson à excès de zéros, un GAMM : une courbe et le site aléatoire\n")
dzg <- modele_f("GAM", "compte", c("x1", "x2", "habitat"), famille = "poisson_zi", decalage = uf("effort"), lisses = uf("x1"),
                aleatoires = list(list(by = "variable", variable = uf("site"))))
mzg <- ajuster(pf, dzg)
formule_zg <- "compte ~ x2 + bois + haie + s(x1, k = 10) + s(site, bs = \"re\") + offset(log(effort))"
rzg <- gam_ref(formule_zg, family = mgcv::ziP(), data = rdf, method = "REML")
rzg_ml <- gam_ref(formule_zg, family = mgcv::ziP(), data = rdf, method = "ML")
vz <- mgcv::gam.vcomp(rzg)
verifier(sprintf("coefficients et edf de la courbe (%.2f) = mgcv en REML ; AIC = mgcv en ML ; θ1 et θ2 ; la variance du site = gam.vcomp()", mzg$resultat$courbes$edf),
         proche(mzg$resultat$coefficients$estimation, summary(rzg)$p.table[, 1], 1e-5) && proche(mzg$resultat$courbes$edf, summary(rzg)$s.table["s(x1)", 1], 1e-5) &&
           proche(mzg$resultat$AIC, AIC(rzg_ml), 1e-6) && proche(mzg$resultat$parametre_loi$valeur, rzg$family$getTheta(TRUE), 1e-5) &&
           proche(mzg$resultat$variances$variance, (if (is.matrix(vz)) vz else vz$vc)["s(site)", 1]^2, 1e-4))
ndz <- rdf[1:6, ]
ndz$site <- factor(rep(levels(rdf$site)[1], 6), levels = levels(rdf$site))
verifier("les prédictions par les effets fixes (le site exclu), la valeur attendue : = predict(exclude = \"s(site)\")",
         proche(predire(pf, mzg, dzg, obs[1:6])[["compte prédit"]], predict(rzg, ndz, type = "response", exclude = "s(site)"), 1e-8))
# (les seuils d'une courbe et les courbes d'effet, sur l'échelle du lien log(μ))
sz6 <- seuils_courbe(mzg, "x1")
ez <- effets_regression(mzg)
verifier("courbes d'effet et seuils de la courbe de x1, sur l'échelle du lien", length(ez) == 2 && isTRUE(ez[[1]]$courbe) && length(sz6$x) == 200 && all(sz6$haut >= sz6$bas))

cat("Loi bêta (betar), un GLM et un GAMM\n")
db <- modele_f("GLM", "prop", c("x1", "x2", "habitat"), famille = "beta")
mb <- ajuster(pf, db)
# (le maximum de vraisemblance écrit ici : la loi bêta de moyenne plogis(X b), de précision φ, par optim)
Xb <- cbind(1, rdf$x1, rdf$x2, rdf$bois, rdf$haie)
moins_ll_beta <- function(par) {
  m <- plogis(drop(Xb %*% par[1:5]))
  -sum(dbeta(tf$prop, m * exp(par[6]), (1 - m) * exp(par[6]), log = TRUE))
}
o <- optim(c(rep(0, 5), log(3)), moins_ll_beta, method = "BFGS", control = list(reltol = 1e-15, maxit = 5000))
# (mgcv 1.9 s'arrête sur ces données, sans terme pénalisé, en ML : regression.R, profil_loi ; à φ fixé, il
# s'ajuste, et ses erreurs-types sont celles, conditionnelles à φ, de son ajustement où φ est estimé)
rb_ml <- tryCatch(mgcv::gam(prop ~ x1 + x2 + bois + haie, family = mgcv::betar(link = "logit"), data = rdf, method = "ML"), error = function(e) NULL)
rb <- mgcv::gam(prop ~ x1 + x2 + bois + haie, family = mgcv::betar(theta = exp(o$par[6]), link = "logit"), data = rdf)
sb <- summary(rb)
verifier(sprintf("GLM : le maximum de vraisemblance de la loi bêta écrite ici (optim) : coefficients, φ = %.3f, logLik, AIC à 6 paramètres%s",
                 exp(o$par[6]), if (is.null(rb_ml)) " ; mgcv::gam(method = \"ML\") s'arrête sur ces données (LAPACK), profil_loi l'obtient" else " ; = mgcv::gam(method = \"ML\")"),
         proche(mb$resultat$coefficients$estimation, o$par[1:5], 1e-5) && proche(mb$resultat$parametre_loi$valeur, exp(o$par[6]), 1e-5) &&
           proche(mb$resultat$logLik, -o$value, 1e-9) && abs(mb$resultat$parametres - 6) < 1e-6 && proche(mb$resultat$AIC, 2 * o$value + 12, 1e-9) &&
           identical(mb$resultat$parametre_loi$nom, "phi") && identical(mb$lien, "logit") &&
           (is.null(rb_ml) || (proche(mb$resultat$coefficients$estimation, coef(rb_ml)) && proche(mb$resultat$AIC, AIC(rb_ml)))))
verifier("GLM : erreurs-types, z, p, R² ajusté et déviance expliquée = mgcv::gam(family = betar()) à ce φ",
         proche(mb$resultat$coefficients$erreur_type, sb$p.table[, 2], 1e-5) && proche(mb$resultat$coefficients$t, sb$p.table[, 3], 1e-5) &&
           proche(mb$resultat$coefficients$p, sb$p.table[, 4], 1e-4) && proche(mb$resultat$r2$r2_ajuste, sb$r.sq, 1e-5) &&
           proche(mb$resultat$r2$deviance_expliquee, sb$dev.expl, 1e-5) && all(is.na(mb$resultat$coefficients$ddl)))
mu_b <- fitted(rb)
phi_b <- mb$resultat$parametre_loi$valeur
verifier("sa vraisemblance est celle de dbeta de formes μ φ et (1 − μ) φ ; ses valeurs ajustées, les proportions moyennes μ",
         proche(mb$resultat$logLik, sum(dbeta(tf$prop, mb$resultat$ajuste * phi_b, (1 - mb$resultat$ajuste) * phi_b, log = TRUE)), 1e-10) &&
           proche(mb$resultat$ajuste, mu_b, 1e-6))
# (le même profil pour ziP, où mgcv s'ajuste en ML : il retrouve son maximum)
pz_prof <- profil_loi(stats::as.formula("compte ~ x1 + x2 + bois + haie + offset(log(effort))", env = env), rdf[appris, ], "poisson_zi")
verifier(sprintf("profil_loi sur ziP, où mgcv s'ajuste en ML : son maximum retrouvé (θ1 = %.4f, θ2 = %.4f ; logLik à 1e-9 près, 7 paramètres)",
                 pz_prof$family$getTheta(TRUE)[1], pz_prof$family$getTheta(TRUE)[2]),
         proche(pz_prof$family$getTheta(TRUE), th, 1e-4) && proche(coef(pz_prof), coef(rz), 1e-4) && proche(as.numeric(logLik(pz_prof)), as.numeric(logLik(rz)), 1e-9) &&
           abs(attr(logLik(pz_prof), "df") - 7) < 1e-6 && proche(AIC(pz_prof), AIC(rz), 1e-8))
# un GAMM : la courbe de x1, le site aléatoire ; validé par zone (deux groupes de quatre sites jamais vus)
dbg <- modele_f("GAM", "prop", c("x1", "x2", "habitat"), famille = "beta", lisses = uf("x1"), aleatoires = list(list(by = "variable", variable = uf("site"))),
                vc = list(by = "variable", variable = uf("zone")))
mbg <- ajuster(pf, dbg)
formule_bg <- "prop ~ x2 + bois + haie + s(x1, k = 10) + s(site, bs = \"re\")"
rbg <- gam_ref(formule_bg, family = mgcv::betar(link = "logit"), data = rdf, method = "REML")
rbg_ml <- gam_ref(formule_bg, family = mgcv::betar(link = "logit"), data = rdf, method = "ML")
vb <- mgcv::gam.vcomp(rbg)
verifier(sprintf("GAMM : coefficients, edf (%.2f), test de la courbe, φ (REML), AIC (ML) et variance du site = mgcv", mbg$resultat$courbes$edf),
         proche(mbg$resultat$coefficients$estimation, summary(rbg)$p.table[, 1], 1e-5) && proche(mbg$resultat$courbes$edf, summary(rbg)$s.table["s(x1)", 1], 1e-5) &&
           proche(mbg$resultat$courbes$p, summary(rbg)$s.table["s(x1)", 4], 1e-4) && proche(mbg$resultat$parametre_loi$valeur, rbg$family$getTheta(TRUE), 1e-5) &&
           proche(mbg$resultat$AIC, AIC(rbg_ml), 1e-6) && proche(mbg$resultat$variances$variance, (if (is.matrix(vb)) vb else vb$vc)["s(site)", 1]^2, 1e-4))
vcb <- rep(NA_real_, n)
for (z in c("nord", "sud")) {
  ici <- tf$zone == z
  a <- gam_ref(formule_bg, family = mgcv::betar(link = "logit"), data = droplevels(rdf[!ici, ]), method = "REML")
  ndb <- rdf[ici, ]
  ndb$site <- factor(rep(levels(droplevels(rdf[!ici, ])$site)[1], sum(ici)), levels = levels(droplevels(rdf[!ici, ])$site))
  vcb[ici] <- predict(a, ndb, type = "response", exclude = "s(site)")
}
verifier(sprintf("validation croisée à la main par zone (deux groupes) : chaque zone, ses sites jamais vus, prédite par les effets fixes (Q2 = %.3f)", mbg$vc$Q2),
         mbg$G == 2 && proche(mbg$vc$prediction, vcb, 1e-6) && proche(mbg$vc$Q2, 1 - sum((tf$prop - vcb)^2) / sum((tf$prop - mean(tf$prop))^2), 1e-6))

cat("Résidus quantiles randomisés (Dunn et Smyth, 1996)\n")
# (la loi de ziP écrite ici : F(q) = 0 pour q < 0, 1 − p en 0, 1 − p + p (P(X ≤ q) − P(X = 0)) / P(X > 0) au-delà)
F_zip <- function(q, lambda, p) ifelse(q < 0, 0, (1 - p) + p * (ppois(q, lambda) - dpois(0, lambda)) / (1 - dpois(0, lambda)))
tirer_entre <- function(a, b) {
  set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  stats::runif(length(a), a, b)
}
u_z <- tirer_entre(F_zip(yz - 1, lz, pz), F_zip(yz, lz, pz))
rq_z <- mz$resultat$residus_quantiles
verifier(sprintf("ziP : u tiré entre F(y − 1) et F(y) de la loi écrite ici, r = qnorm(u) ; normaux sur des comptages de cette loi (moyenne %.2f, écart-type %.2f)", mean(rq_z), sd(rq_z)),
         proche(pnorm(rq_z), u_z, 1e-8) && proche(rq_z[u_z <= 0.5], qnorm(u_z[u_z <= 0.5]), 1e-8) && all(is.finite(rq_z)) && abs(mean(rq_z)) < 0.2 && abs(sd(rq_z) - 1) < 0.15)
# (la fonction de répartition elle-même : ses sauts sont les probabilités de mgcv, 1 − p en 0 puis
# p λ^y / ((e^λ − 1) y!), ses deux queues font 1 ; λ de 10⁻¹² à 40, et λ nul en double précision)
grille <- expand.grid(y = 0:60, g = c(-27.6, -5, -1, 0, 0.7, 2, 3.7), KEEP.OUT.ATTRS = FALSE)
Fg <- repartition_zip(grille$g, c(-0.5, 1.4))
lg <- exp(grille$g)
eg <- exp(-0.5 + 1.4 * grille$g)
masse <- ifelse(grille$y == 0, exp(-eg), -expm1(-eg) * exp(grille$y * grille$g - lgamma(grille$y + 1) - log(expm1(lg))))
saut <- Fg(grille$y) - Fg(grille$y - 1)
verifier(sprintf("repartition_zip : F(y) − F(y − 1) = la probabilité de mgcv (à %.0e près ; à 1e-9 près relatif au-delà de 1e-6), F + (1 − F) = 1, de λ = 10⁻¹² à 40",
                 max(abs(saut - masse))),
         max(abs(saut - masse)) < 1e-14 && proche(saut[masse > 1e-6], masse[masse > 1e-6], 1e-9) &&
           proche(Fg(grille$y) + Fg(grille$y, lower.tail = FALSE), rep(1, nrow(grille)), 1e-12) && all(Fg(rep(-1, nrow(grille))) == 0) && proche(Fg(rep(0, nrow(grille))), exp(-eg), 1e-12))
F0 <- repartition_zip(c(-800, -800), c(805, 1))
verifier("λ nul en double précision (p presque 1) : la loi tronquée est toute en 1 (sa limite), sans NaN",
         identical(F0(c(0, 1)), c(exp(-exp(5)), 1)) && identical(F0(c(0, 1), lower.tail = FALSE), c(-expm1(-exp(5)), 0)))
lg2 <- exp(rzg$linear.predictors)
pg2 <- 1 - exp(-exp(rzg$family$getTheta(TRUE)[1] + rzg$family$getTheta(TRUE)[2] * rzg$linear.predictors))
verifier("ziP en GAMM : la loi à la valeur ajustée, le site et l'effort compris, = mgcv::gam() et ses θ",
         proche(mzg$resultat$residus_quantiles, qnorm(tirer_entre(F_zip(tf$compte - 1, lg2, pg2), F_zip(tf$compte, lg2, pg2))), 1e-5))
mu_bg <- fitted(rbg)
phi_bg <- rbg$family$getTheta(TRUE)
verifier("bêta : u = pbeta(y) de formes μ φ et (1 − μ) φ, sans tirage, le site compris ; presque normaux",
         proche(mb$resultat$residus_quantiles, qnorm(pbeta(tf$prop, mb$resultat$ajuste * phi_b, (1 - mb$resultat$ajuste) * phi_b)), 1e-10) &&
           proche(mbg$resultat$residus_quantiles, qnorm(pbeta(tf$prop, mu_bg * phi_bg, (1 - mu_bg) * phi_bg)), 1e-5) &&
           abs(mean(mbg$resultat$residus_quantiles)) < 0.15 && abs(sd(mbg$resultat$residus_quantiles) - 1) < 0.15)
# (les résidus de Pearson du ZIP, que mgcv ne donne pas : ses moments sommés ici sur sa loi, de 0 à 400)
k <- 0:400
moments <- t(vapply(seq_along(yz), function(i) {
  pr <- ifelse(k == 0, 1 - pz[i], pz[i] * dpois(k, lz[i]) / (1 - exp(-lz[i])))
  e <- sum(k * pr)
  c(e, sum((k - e)^2 * pr))
}, numeric(2)))
verifier("ziP : les résidus de Pearson, (y − E) / √V, de la moyenne et de la variance de sa loi (l'autocorrélation des résidus s'en sert)",
         proche(residus_pearson_mgcv(mz$ajustement, "poisson_zi"), (yz - moments[, 1]) / sqrt(moments[, 2]), 1e-8) && proche(moments[, 1], attendu, 1e-8) &&
           length(autocorrelation_residus(pf, mz, temps = list(by = "variable", variable = uf("ordre")))$series[[1]]$acf) > 1)

cat("Ensembles AIC, sélection, lissage spatial\n")
mp <- ajuster(pf, modele_f("GLM", "compte", c("x1", "x2", "habitat"), famille = "poisson", decalage = uf("effort"), G = 2, test = test))
mn <- ajuster(pf, modele_f("GLM", "prop", c("x1", "x2", "habitat")))
verifier(sprintf("ziP se compare par l'AIC à Poisson sur les mêmes comptages (ΔAIC = %.1f) ; la loi bêta à la loi normale ; pas l'une à l'autre", mp$resultat$AIC - mz$resultat$AIC),
         mz$signature == mp$signature && mb$signature == mn$signature && mb$signature != mz$signature && mp$resultat$AIC > mz$resultat$AIC)
sel <- selection_aic(mz, dz)
aicc_ref <- function(f) { a <- mgcv::gam(stats::as.formula(f), family = mgcv::ziP(), data = rdf[appris, ], method = "ML"); aic_corrige(a, sum(appris)) }
verifier(sprintf("la sélection par l'AIC d'un ziP : ses %d combinaisons, chacune = mgcv::gam(family = ziP(), method = \"ML\")", nrow(sel$modeles)),
         nrow(sel$modeles) == 8 && proche(min(sel$modeles$AICc), min(vapply(c("compte ~ x1 + x2 + bois + haie", "compte ~ x1 + x2", "compte ~ x1 + bois + haie", "compte ~ x2 + bois + haie",
                                                                              "compte ~ x1", "compte ~ x2", "compte ~ bois + haie", "compte ~ 1"),
                                                                            function(f) aicc_ref(paste(f, "+ offset(log(effort))")), 0)), 1e-6))
ds <- modele_f("GAM", "prop", c("x2", "habitat"), famille = "beta", lisse_spatial = list(variables = uf("X", "Y"), k = 20))
ms <- ajuster(pf, ds)
rs <- gam_ref("prop ~ x2 + bois + haie + s(X, Y, k = 20)", family = mgcv::betar(link = "logit"), data = rdf, method = "REML")
rs_ml <- gam_ref("prop ~ x2 + bois + haie + s(X, Y, k = 20)", family = mgcv::betar(link = "logit"), data = rdf, method = "ML")
verifier(sprintf("la loi bêta avec le lissage spatial s(X, Y) : coefficients, edf (%.2f), φ et AIC = mgcv", ms$resultat$courbes$edf),
         proche(ms$resultat$coefficients$estimation, summary(rs)$p.table[, 1], 1e-5) && proche(ms$resultat$courbes$edf, summary(rs)$s.table[1, 1], 1e-5) &&
           proche(ms$resultat$parametre_loi$valeur, rs$family$getTheta(TRUE), 1e-5) && proche(ms$resultat$AIC, AIC(rs_ml), 1e-6))
dzs <- modele_f("GAM", "compte", c("x2", "habitat"), famille = "poisson_zi", decalage = uf("effort"), lisse_spatial = list(variables = uf("X", "Y"), k = 20))
mzs <- ajuster(pf, dzs)
rzs <- gam_ref("compte ~ x2 + bois + haie + s(X, Y, k = 20) + offset(log(effort))", family = mgcv::ziP(), data = rdf, method = "REML")
verifier("ziP avec le lissage spatial : comme mgcv", proche(mzs$resultat$coefficients$estimation, summary(rzs)$p.table[, 1], 1e-5) &&
           proche(mzs$resultat$courbes$edf, summary(rzs)$s.table[1, 1], 1e-5))

cat("Refus\n")
pv <- projet_depuis_tableau(transform(tf, demi = compte + 0.5, positif = compte + 1, bord = ifelse(seq_len(n) %% 40 == 0, 1, prop), zero = ifelse(seq_len(n) %% 50 == 0, 0, prop)), "refus")
uv <- function(...) numeros(pv, c(...))
essai <- function(y, famille, ...) refus(ajuster(pv, definir_modele(pv, "GLM", x = uv("x1"), y = uv(y), echelle = "none", famille = famille, ...)))
verifier("ziP sur des valeurs non entières, ou sur des comptages sans zéro : refusé, en le disant",
         grepl("excès de zéros demande des comptages", essai("demi", "poisson_zi")) && grepl("Aucun comptage de la réponse n'est nul", essai("positif", "poisson_zi")))
verifier("bêta avec des 1 ou des 0, ou des comptages : refusé, le nombre de valeurs dit (6, 4), la binomiale et la remise dans l'intervalle proposées",
         grepl("6 valeur(s) de la réponse sont à 0, à 1 ou au-delà", essai("bord", "beta"), fixed = TRUE) && grepl("binomiale", essai("bord", "beta")) &&
           grepl("4 valeur(s)", essai("zero", "beta"), fixed = TRUE) && grepl("strictement comprises entre 0 et 1", essai("compte", "beta")))
verifier("bêta avec un effort en décalage (lien logit) : refusé ; ziP en a un (lien log de μ)",
         grepl("lien log", essai("prop", "beta", decalage = uv("effort"))) && !nzchar(essai("compte", "poisson_zi", decalage = uv("effort"))))
verifier("une autocorrélation CAR1 avec ziP ou la loi bêta : refusée (mgcv::gamm passerait par la PQL)",
         all(vapply(c("compte", "prop"), function(y) grepl("loi normale", essai(y, if (y == "compte") "poisson_zi" else "beta",
                                                                             aleatoires = list(list(by = "variable", variable = uv("site"))),
                                                                             correlation = list(type = "CAR1", temps = list(by = "variable", variable = uv("x1"))))), TRUE)))
verifier("la formule R : l'effort avec la loi bêta refusé, avec ziP accepté",
         grepl("Poisson à excès de zéros", refus(lire_formule(pf, "prop ~ x1 + offset(log(effort))", "GLM", "beta"))) &&
           !nzchar(refus(lire_formule(pf, "compte ~ x1 + offset(log(effort))", "GLM", "poisson_zi"))))
options(nuee.langue = "en")
verifier("les refus en anglais", grepl("A beta distribution needs proportions strictly between 0 and 1: 6 value", essai("bord", "beta"), fixed = TRUE) &&
           grepl("No count of the response is zero", essai("positif", "poisson_zi")))
options(nuee.langue = "fr")

cat("Coefficients côte à côte, texte des Méthodes\n")
cc <- coefficients_cote_a_cote(list(Z = mz, B = mb, P = mp), "ic")
v <- function(nom, j) cc$valeurs[match(nom, cc$statistiques), j]
verifier(sprintf("côte à côte : θ₁, θ₂ et φ sur leurs lignes (%s ; %s ; %s), vides pour les autres modèles", v("θ₁ (Poisson à excès de zéros)", 1),
                 v("θ₂ (Poisson à excès de zéros)", 1), v("φ (bêta)", 2)),
         identical(v("θ₁ (Poisson à excès de zéros)", 1), trois_chiffres(th[1])) && identical(v("θ₂ (Poisson à excès de zéros)", 1), trois_chiffres(th[2])) &&
           identical(v("φ (bêta)", 2), trois_chiffres(phi_b)) && v("φ (bêta)", 1) == "" && v("θ₁ (Poisson à excès de zéros)", 3) == "")
for (l in c("fr", "en")) {
  tz <- texte_methodes(pf, dzg, mzg, l, "essai")
  tb <- texte_methodes(pf, dbg, mbg, l, "essai")
  pz2 <- paste(tz$paragraphes, collapse = " ")
  pb2 <- paste(tb$paragraphes, collapse = " ")
  fr <- l == "fr"
  verifier(sprintf("texte des Méthodes (%s) : ziP décrit (la loi, θ₁ et θ₂ et leurs valeurs, lien log, l'effort), Wood, Pya et Säfken (2016) cité ; sans R² ajusté", l),
           grepl(if (fr) "loi de Poisson à excès de zéros, lien log" else "zero-inflated Poisson distribution, log link", pz2, fixed = TRUE) &&
             grepl("log(−log(1 − p)) = θ₁ + θ₂ log(μ)", pz2, fixed = TRUE) &&
             grepl(sprintf("θ₁ = %s", formatC(mzg$resultat$parametre_loi$valeur[1], format = "f", digits = 2, decimal.mark = if (fr) "," else ".")), pz2, fixed = TRUE) &&
             any(grepl("^Wood, S. N., Pya, N., & Säfken, B. \\(2016\\)", tz$references)) && !grepl(if (fr) "R² ajusté" else "adjusted R²", pz2, fixed = TRUE) &&
             !grepl("NA", pz2, fixed = TRUE))
  verifier(sprintf("texte des Méthodes (%s) : la loi bêta décrite (sa variance, φ et sa valeur), Ferrari et Cribari-Neto (2004) cités", l),
           grepl(if (fr) "loi bêta, lien logit" else "beta distribution, logit link", pb2, fixed = TRUE) && grepl("μ (1 − μ) / (1 + φ)", pb2, fixed = TRUE) &&
             grepl(sprintf("φ = %s", formatC(mbg$resultat$parametre_loi$valeur, format = "f", digits = 2, decimal.mark = if (fr) "," else ".")), pb2, fixed = TRUE) &&
             any(grepl("^Ferrari, S. L. P., & Cribari-Neto, F. \\(2004\\)", tb$references)) && !grepl("NA", pb2, fixed = TRUE))
}
verifier("ce que R affiche du modèle : sa loi et ses coefficients", any(grepl("loi poisson zi", utils::capture.output(print(mz)), fixed = TRUE)))

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
