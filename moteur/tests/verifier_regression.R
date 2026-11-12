# Vérifie les régressions (moteur/R/regression.R) contre les fonctions de R elles-mêmes
# (lm, nlme::lme) : coefficients, AIC en ML, R², validation croisée par groupes, jeu de test,
# apport des blocs, autocorrélation dans le temps ; et que les définitions mal formées (un
# fichier reçu) sont refusées, qu'une définition enregistrée se relit.
#   Rscript moteur/tests/verifier_regression.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-6) isTRUE(all.equal(unname(a), unname(b), tolerance = tol, check.attributes = FALSE))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)
p <- lire_projet("exemple/vergers.json")
obs <- numeros_observations(p)
u <- function(...) numeros(p, c(...))
col <- function(nom) colonne(p, numeros(p, nom), obs)
df <- data.frame(note = col("note_gout"), sucre = col("sucre"), fermete = col("fermete"), acidite = col("acidite"), polyphenols = col("polyphenols"),
                 variete = factor(as.character(variable_du_projet(p, u("variete"))$categories)[col("variete")]),
                 verger = factor(as.character(variable_du_projet(p, u("verger"))$categories)[col("verger")]),
                 culture = factor(as.character(variable_du_projet(p, u("culture"))$categories)[col("culture")]))

cat("Régression linéaire\n")
d <- definir_modele(p, "LM", x = u("sucre", "fermete", "acidite", "polyphenols"), y = u("note_gout"), echelle = "none")
m <- ajuster(p, d)
complet <- stats::complete.cases(df[c("note", "sucre", "fermete", "acidite", "polyphenols")])
l <- lm(note ~ sucre + fermete + acidite + polyphenols, data = df[complet, ])
verifier(sprintf("coefficients, AIC, R² = lm() (%d observations complètes sur %d)", m$N, length(obs)),
         proche(m$resultat$coefficients$estimation, coef(l)) && proche(m$resultat$AIC, AIC(l)) && proche(m$resultat$r2$r2, summary(l)$r.squared) &&
           m$N == sum(complet) && setequal(m$exclues, obs[!complet]))
verifier("intervalles à 95 % = confint()", proche(m$resultat$coefficients$bas, confint(l)[, 1]) && proche(m$resultat$coefficients$haut, confint(l)[, 2]))
verifier("noms des termes", identical(m$resultat$coefficients$terme, c("(Intercept)", "sucre", "fermete", "acidite", "polyphenols")))
m0 <- ajuster(p, definir_modele(p, "LM", x = NULL, y = u("note_gout"), echelle = "none"))
l0 <- lm(note ~ 1, data = df)
verifier("sans X : le modèle nul = lm(y ~ 1)", proche(m0$resultat$coefficients$estimation, coef(l0)) && proche(m0$resultat$AIC, AIC(l0)))
d2 <- definir_modele(p, "LM", x = u("sucre", "variete"), y = u("note_gout"), echelle = "none")
m2 <- ajuster(p, d2)
l2 <- lm(note ~ sucre + variete, data = df)
verifier("une variable qualitative : une catégorie de référence retirée, mêmes valeurs ajustées et AIC que lm()",
         length(m2$resultat$termes_retires) == 1 && proche(m2$resultat$ajuste, fitted(l2)) && proche(m2$resultat$AIC, AIC(l2)))
G <- 7
groupes <- ((seq_along(obs) - 1) %% G + 1)[complet]  # (à tour de rôle sur les observations du modèle, comme une PLS)
pred <- rep(NA_real_, sum(complet))
for (g in 1:G) pred[groupes == g] <- predict(lm(note ~ sucre + fermete + acidite + polyphenols, data = df[complet, ][groupes != g, ]), df[complet, ][groupes == g, ])
yc <- df$note[complet]
verifier("validation croisée à tour de rôle : Q2 et RMSEcv recalculés à la main",
         proche(m$vc$Q2, 1 - sum((yc - pred)^2) / sum((yc - mean(yc))^2)) && proche(m$vc$RMSEcv, sqrt(mean((yc - pred)^2))))

cat("Modèle mixte\n")
d3 <- definir_modele(p, "LMM", x = u("sucre", "fermete"), y = u("note_gout"), echelle = "none", aleatoires = list(list(by = "variable", variable = u("verger"))))
verifier("validé par son effet aléatoire quand rien d'autre n'est dit", identical(d3$groupes_vc$by, "variable") && d3$groupes_vc$variable == u("verger"))
m3 <- ajuster(p, d3)
ctrl <- nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE)
r_reml <- nlme::lme(note ~ sucre + fermete, random = ~ 1 | verger, data = df, method = "REML", control = ctrl)
r_ml <- nlme::lme(note ~ sucre + fermete, random = ~ 1 | verger, data = df, method = "ML", control = ctrl)
verifier("coefficients (REML) et AIC (ML) = nlme::lme()", proche(m3$resultat$coefficients$estimation, nlme::fixef(r_reml), 1e-5) &&
           proche(m3$resultat$AIC, AIC(r_ml), 1e-6))
vf <- var(as.vector(model.matrix(note ~ sucre + fermete, df) %*% nlme::fixef(r_reml)))
vr <- as.numeric(nlme::VarCorr(r_reml)[1, "Variance"])
verifier("R² marginal et conditionnel (Nakagawa et Schielzeth)", proche(m3$resultat$r2$marginal, vf / (vf + vr + r_reml$sigma^2), 1e-5) &&
           proche(m3$resultat$r2$conditionnel, (vf + vr) / (vf + vr + r_reml$sigma^2), 1e-5))
pred <- rep(NA_real_, nrow(df))
for (g in levels(df$verger)) {
  ici <- df$verger == g
  pred[ici] <- predict(nlme::lme(note ~ sucre + fermete, random = ~ 1 | verger, data = droplevels(df[!ici, ]), method = "REML", control = ctrl), df[ici, ], level = 0)
}
verifier(sprintf("validation par verger : un verger jamais vu prédit par les effets fixes (Q2 = %.3f)", m3$vc$Q2),
         proche(m3$vc$Q2, 1 - sum((df$note - pred)^2) / sum((df$note - mean(df$note))^2), 1e-5) && m3$G == 3)
d4 <- definir_modele(p, "LMM", x = u("sucre"), y = u("note_gout"), echelle = "none",
                     aleatoires = list(list(by = "variable", variable = u("verger")), list(by = "variable", variable = u("culture"))))
m4 <- ajuster(p, d4)
r4 <- nlme::lme(note ~ sucre, random = ~ 1 | verger / culture, data = df, method = "ML", control = ctrl)
verifier("effets aléatoires emboîtés (verger / culture) = nlme::lme()", proche(m4$resultat$AIC, AIC(r4), 1e-6) && nrow(m4$resultat$variances) == 3)

cat("Apport des blocs\n")
d5 <- definir_modele(p, "LM", x = u("sucre", "fermete", "acidite"), y = u("note_gout"), echelle = "none",
                     blocs = stats::setNames(c("chimie", "chimie"), u("sucre", "acidite")))
a <- apport_groupes(ajuster(p, d5))
sans <- lm(note ~ fermete, data = df)
avec <- lm(note ~ sucre + fermete + acidite, data = df)
verifier("un bloc retiré d'un coup : ΔAIC et rapport de vraisemblance = lm()", identical(a$groupe, c("chimie", "fermete")) &&
           proche(a$delta_aic[1], AIC(sans) - AIC(avec)) && a$ddl[1] == 2 &&
           proche(a$p[1], pchisq(2 * (as.numeric(logLik(avec)) - as.numeric(logLik(sans))), 2, lower.tail = FALSE)))
a3 <- apport_groupes(m3)
sans3 <- nlme::lme(note ~ fermete, random = ~ 1 | verger, data = df, method = "ML", control = ctrl)
verifier("modèle mixte : retraits ajustés en ML", proche(a3$delta_aic[a3$groupe == "sucre"], AIC(sans3) - AIC(r_ml), 1e-6))

cat("Jeu de test\n")
test <- obs[seq(5, length(obs), 10)]
d6 <- definir_modele(p, "LM", x = u("sucre", "fermete"), y = u("note_gout"), echelle = "UV", test = test)
m6 <- ajuster(p, d6)
appr <- !obs %in% test
l6 <- lm(note ~ sucre + fermete, data = df[appr, ])
e <- predict(l6, df[!appr, ]) - df$note[!appr]
verifier("RMSEP et Q2 externe = predict(lm()) (centrage et échelle du modèle)", proche(m6$test$statistiques$RMSEP, sqrt(mean(e^2))) &&
           proche(m6$test$statistiques$Q2ext, 1 - sum(e^2) / sum((df$note[!appr] - mean(df$note[appr]))^2)))

cat("Autocorrélation dans le temps\n")
set.seed(4)
t5 <- data.frame(nom = sprintf("O%03d", 1:60), site = rep(c("A", "B", "C", "D", "E", "F"), each = 10), annee = rep(c(2001:2006, 2008:2011), 6))
t5$x <- rnorm(60)
t5$y <- 1 + 0.5 * t5$x + rep(rnorm(6), each = 10) + as.vector(stats::filter(rnorm(60), 0.6, "recursive"))
p5 <- projet_depuis_tableau(t5, "temps")
d7 <- definir_modele(p5, "LMM", x = numeros(p5, "x"), y = numeros(p5, "y"), echelle = "none",
                     aleatoires = list(list(by = "variable", variable = numeros(p5, "site"))),
                     correlation = list(type = "CAR1", temps = list(by = "variable", variable = numeros(p5, "annee"))))
m7 <- ajuster(p5, d7)
r7 <- nlme::lme(y ~ x, random = ~ 1 | site, correlation = nlme::corCAR1(form = ~ annee | site), data = t5, method = "REML", control = ctrl)
verifier(sprintf("CAR1 (années manquantes comprises) : coefficients et phi = nlme (phi = %.3f)", m7$resultat$phi),
         proche(m7$resultat$coefficients$estimation, nlme::fixef(r7), 1e-4) &&
           proche(m7$resultat$phi, coef(r7$modelStruct$corStruct, unconstrained = FALSE), 1e-3))
t5$annee[2] <- t5$annee[1]
p6 <- projet_depuis_tableau(t5, "temps")
m8 <- refus(ajuster(p6, definir_modele(p6, "LMM", x = numeros(p6, "x"), y = numeros(p6, "y"), echelle = "none",
                                       aleatoires = list(list(by = "variable", variable = numeros(p6, "site"))),
                                       correlation = list(type = "CAR1", temps = list(by = "variable", variable = numeros(p6, "annee"))))))
verifier("deux observations au même temps dans un groupe : refusé, et le message le dit", grepl("un seul temps par groupe", m8))

cat("Définitions refusées\n")
verifier("une réponse qualitative", grepl("une seule variable Y", refus(definir_modele(p, "LM", x = u("sucre"), y = u("variete")))))
verifier("un modèle mixte sans effet aléatoire", grepl("au moins un effet aléatoire", refus(definir_modele(p, "LMM", x = u("sucre"), y = u("note_gout")))))
verifier("un effet aléatoire sur une variable quantitative", grepl("identifiant ou une variable qualitative",
         refus(definir_modele(p, "LMM", x = u("sucre"), y = u("note_gout"), aleatoires = list(list(by = "variable", variable = u("poids")))))))
dm <- d3
dm$aleatoires <- list(list(by = "system", id = "x"))
verifier("un groupement inconnu (un fichier modifié)", grepl("inconnu", refus(ajuster(p, dm))))
dm <- d3
dm$correlation <- list(type = "evil", temps = list(by = "variable", variable = u("stockage")))
verifier("une autocorrélation inconnue", grepl("Autocorrélation inconnue", refus(ajuster(p, dm))))
dm <- d3
dm$aleatoires <- list(list(by = "id", id = "absent"))
verifier("un identifiant absent du projet", grepl("pas d'identifiant", refus(ajuster(p, dm))))

cat("Enregistrer et relire\n")
f <- tempfile(fileext = ".nuee")
p$models <- c(p$models, list(d3))
enregistrer_projet(p, f)
q <- lire_projet(f)
d9 <- q$models[[length(q$models)]]
m9 <- ajuster(q, d9)
verifier("une définition de modèle mixte relue donne le même modèle", proche(m9$resultat$coefficients$estimation, m3$resultat$coefficients$estimation) &&
           proche(m9$vc$Q2, m3$vc$Q2))
verifier("ce que R affiche du modèle", any(grepl("AIC", utils::capture.output(print(m3)))))

cat("Modèles généralisés et additifs (mgcv)\n")
set.seed(7)
n <- 240
tg <- data.frame(nom = sprintf("O%03d", 1:n), site = rep(sprintf("S%d", 1:8), each = 30), x1 = runif(n), x2 = rnorm(n), effort = runif(n, 1, 3))
site_effet <- rnorm(8, 0, 0.4)[as.integer(factor(tg$site))]
mu <- exp(0.3 + sin(3 * tg$x1) + 0.4 * tg$x2 + site_effet) * tg$effort
tg$compte <- stats::rnbinom(n, mu = mu, size = 4)
tg$presence <- as.numeric(runif(n) < stats::plogis(-0.5 + 1.2 * tg$x2))
tg$masse <- stats::rgamma(n, shape = 3, rate = 3 / exp(0.5 + 0.3 * tg$x2))
tg$densite <- ifelse(runif(n) < 0.25, 0, tg$masse)
pg <- projet_depuis_tableau(tg, "generalises")
ug <- function(...) numeros(pg, c(...))
modele_g <- function(type, y, x, ...) ajuster(pg, definir_modele(pg, type, x = ug(x), y = ug(y), echelle = "none", ...))
ref <- glm(compte ~ x1 + x2, family = poisson, data = tg)
mg1 <- modele_g("GLM", "compte", c("x1", "x2"), famille = "poisson")
verifier("Poisson : coefficients et AIC = glm()", proche(mg1$resultat$coefficients$estimation, coef(ref)) && proche(mg1$resultat$AIC, AIC(ref)))
verifier(sprintf("Poisson sur des comptages binomiaux négatifs : la surdispersion se voit (khi² de Pearson / ddl = %.1f, comme glm())", mg1$resultat$dispersion),
         proche(mg1$resultat$dispersion, sum(residuals(ref, type = "pearson")^2) / ref$df.residual) && mg1$resultat$dispersion > 1.5)
ref <- glm(compte ~ x1 + x2, family = poisson, data = tg, offset = log(effort))
mg2 <- modele_g("GLM", "compte", c("x1", "x2"), famille = "poisson", decalage = ug("effort"))
verifier("Poisson avec l'effort en décalage = glm(offset = log(effort))", proche(mg2$resultat$coefficients$estimation, coef(ref)) && proche(mg2$resultat$AIC, AIC(ref)))
ref <- MASS::glm.nb(compte ~ x1 + x2, data = tg)
mg3 <- modele_g("GLM", "compte", c("x1", "x2"), famille = "binomiale_negative")
verifier(sprintf("binomiale négative : coefficients et θ = MASS::glm.nb() (θ = %.2f)", mg3$resultat$parametre_loi$valeur),
         proche(mg3$resultat$coefficients$estimation, coef(ref), 1e-3) && proche(mg3$resultat$parametre_loi$valeur, ref$theta, 1e-2))
ref <- glm(presence ~ x2, family = binomial, data = tg)
mg4 <- modele_g("GLM", "presence", "x2", famille = "binomiale")
verifier("binomiale (présence-absence) = glm()", proche(mg4$resultat$coefficients$estimation, coef(ref)) && proche(mg4$resultat$AIC, AIC(ref)))
ref <- glm(masse ~ x2, family = Gamma(link = "log"), data = tg)
mg5 <- modele_g("GLM", "masse", "x2", famille = "gamma")
verifier("gamma (lien log) = glm()", proche(mg5$resultat$coefficients$estimation, coef(ref), 1e-5))
env <- new.env(parent = asNamespace("mgcv"))
ref <- mgcv::gam(stats::as.formula("compte ~ x2 + s(x1, k = 10) + s(site, bs = \"re\")", env = env), data = transform(tg, site = factor(site)),
                 family = mgcv::nb(), method = "ML", offset = log(effort))
mg6 <- modele_g("GAM", "compte", c("x1", "x2"), famille = "binomiale_negative", lisses = ug("x1"), decalage = ug("effort"),
               aleatoires = list(list(by = "variable", variable = ug("site"))))
verifier(sprintf("GAMM binomial négatif (courbe, site aléatoire, effort) : AIC = mgcv::gam() en ML (edf %.1f, Q2 = %.2f)", mg6$resultat$courbes$edf, mg6$vc$Q2),
         proche(mg6$resultat$AIC, AIC(ref), 1e-4) && mg6$resultat$courbes$edf > 2 && !is.na(mg6$vc$Q2) && mg6$vc$echecs == 0 && mg6$G == 8)
refr <- mgcv::gam(stats::as.formula("compte ~ x2 + s(x1, k = 10) + s(site, bs = \"re\")", env = env), data = transform(tg, site = factor(site)),
                  family = mgcv::nb(), method = "REML", offset = log(effort))
vcomp <- mgcv::gam.vcomp(refr)  # (une liste, ou la matrice seule selon la version de mgcv)
verifier("GAMM : la variance du site = gam.vcomp() d'un ajustement en REML",
         proche(mg6$resultat$variances$variance, (if (is.matrix(vcomp)) vcomp else vcomp$vc)["s(site)", 1]^2, 1e-3))
eg6 <- effets_regression(mg6)
verifier("courbes d'effet : la courbe en x1, la droite en x2, avec leurs intervalles", length(eg6) == 2 && isTRUE(eg6[[1]]$courbe) &&
         all(eg6[[1]]$haut >= eg6[[1]]$bas) && length(eg6[[2]]$x) == 80)
ag6 <- apport_groupes(mg6)
verifier("apport des termes d'un GAMM (réajusté en ML)", nrow(ag6) == 2 && all(!is.na(ag6$delta_aic)))
# les seuils d'une courbe : la démarche des scripts qui ont servi de modèle, refaite ici sur
# l'ajustement de mgcv fait à part (refr : mêmes termes dans le même ordre), avec la même graine
s6 <- seuils_courbe(mg6, "x1")
xs <- seq(quantile(tg$x1, .02), quantile(tg$x1, .98), length.out = 200)
nds <- transform(tg[rep(1, 200), ], x1 = xs, site = factor(tg$site[1], levels = sort(unique(tg$site))))
Xp <- predict(refr, nds, type = "lpmatrix")
js <- grep("^s\\(x1\\)", colnames(Xp))
set.seed(1)
B <- MASS::mvrnorm(2000, coef(refr), vcov(refr))
fit <- as.vector(Xp[, js] %*% coef(refr)[js])
fit <- fit - mean(fit)
sims <- sweep(Xp[, js] %*% t(B[, js]), 2, colMeans(Xp[, js] %*% t(B[, js])))
se_f <- apply(sims, 1, sd)
cf <- quantile(apply(abs(sims - fit) / se_f, 2, max), .95)
der <- diff(fit) / diff(xs[1:2])
dsim <- apply(sims, 2, diff) / diff(xs[1:2])
se_d <- apply(dsim, 1, sd)
cd <- quantile(apply(abs(dsim - der) / se_d, 2, max), .95)
signe <- ifelse(der - cd * se_d > 0, "hausse", ifelse(der + cd * se_d < 0, "baisse", "ns"))
verifier(sprintf("seuils d'une courbe = la démarche des scripts (2000 tirages, bandes simultanées ; %d plage(s) de pente nette)", nrow(s6$plages_pente)),
         proche(s6$x, xs) && proche(s6$effet, fit, 1e-4) && proche(s6$bas, fit - cf * se_f, 1e-3) && proche(s6$pente, der, 1e-4) &&
           proche(s6$pente_haut, der + cd * se_d, 1e-3) && identical(s6$sens, signe) && !s6$lineaire)
set.seed(3)
ts <- data.frame(nom = sprintf("S%03d", 1:300), x = runif(300), z = runif(300))
ts$y <- 1 + 3 * pmax(0, ts$x - 0.5) + rnorm(300, sd = 0.3)
ts$y_droit <- 1 + 2 * ts$x + rnorm(300, sd = 0.3)
ps <- projet_depuis_tableau(ts, "seuil")
seuil <- function(y) seuils_courbe(ajuster(ps, definir_modele(ps, "GAM", x = numeros(ps, c("x", "z")), y = numeros(ps, y), echelle = "none",
                                                             lisses = numeros(ps, "x"))), "x")
sc <- seuil("y")
sd2 <- seuil("y_droit")
verifier(sprintf("un seuil connu (pente nulle, puis 3 à partir de 0,5) : la pente devient nettement positive vers %.2f", sc$plages_pente$de[1]),
         identical(sc$plages_pente$sens, "hausse") && sc$plages_pente$de[1] > 0.45 && sc$plages_pente$de[1] < 0.7 && !sc$lineaire)
verifier(sprintf("une droite : edf %.2f, dite linéaire sous 1,5 seulement, une seule plage de hausse", sd2$edf),
         sd2$edf < 2 && identical(sd2$lineaire, sd2$edf < 1.5) && identical(sd2$plages_pente$sens, "hausse"))
verifier("des seuils sans courbe : refusés", grepl("courbe", refus(seuils_courbe(mg6, "x2"))))
genre <- RNGkind()
RNGkind("L'Ecuyer-CMRG", "Box-Muller")
set.seed(11)
avant <- .Random.seed
s6b <- seuils_courbe(mg6, "x1")
verifier("seuils : les mêmes bandes quel que soit le générateur de la session, rendu tel quel avec son état",
         proche(s6b$bas, s6$bas, 1e-12) && proche(s6b$pente_haut, s6$pente_haut, 1e-12) && identical(RNGkind()[1:2], c("L'Ecuyer-CMRG", "Box-Muller")) &&
           identical(.Random.seed, avant))
RNGkind(genre[1], genre[2], genre[3])
set.seed(8)
tcv <- data.frame(nom = sprintf("C%03d", 1:200), a = runif(200), c = runif(200))
tcv$b <- tcv$a + rnorm(200, sd = 0.02)
tcv$y <- sin(3 * tcv$a) + tcv$c + rnorm(200, sd = 0.2)
pcv <- projet_depuis_tableau(tcv, "concurvite")
mcv <- ajuster(pcv, definir_modele(pcv, "GAM", x = numeros(pcv, c("a", "b", "c")), y = numeros(pcv, "y"), echelle = "none", lisses = numeros(pcv, c("a", "b", "c"))))
cc <- mgcv::concurvity(mgcv::gam(y ~ s(a) + s(b) + s(c), data = tcv, method = "REML"), full = TRUE)
verifier(sprintf("concurvité de chaque courbe = mgcv::concurvity() ; deux variables presque égales au-delà de 0,8 (%s)",
                 paste(formatC(mcv$resultat$courbes$concurvite, digits = 2, format = "f"), collapse = ", ")),
         proche(mcv$resultat$courbes$concurvite, unname(cc["estimate", c("s(a)", "s(b)", "s(c)")]), 1e-3) &&
           all(mcv$resultat$courbes$concurvite[1:2] > 0.8) && mcv$resultat$courbes$concurvite[3] < 0.5)
mg7 <- modele_g("GAM", "densite", c("x1", "x2"), famille = "tweedie", lisses = ug("x1"))
verifier(sprintf("Tweedie (des zéros et des valeurs positives) : p = %.2f", mg7$resultat$parametre_loi$valeur),
         mg7$resultat$parametre_loi$valeur > 1 && mg7$resultat$parametre_loi$valeur < 2)
mg8 <- modele_g("LM", "compte", c("x1", "x2"))
verifier("une loi discrète et la loi normale ne sont pas dans le même ensemble AIC ; Poisson et binomiale négative, si",
         mg8$signature != mg1$signature && mg3$signature == mg1$signature && mg6$signature != mg3$signature)
pv <- projet_depuis_tableau(transform(tg, compte = compte + 0.5), "demi")
verifier("Poisson sur des valeurs non entières : refusé", grepl("comptages", refus(ajuster(pv, definir_modele(pv, "GLM", x = numeros(pv, "x2"), y = numeros(pv, "compte"), famille = "poisson")))))
verifier("binomiale sur une réponse qui n'est pas 0/1 : refusé", grepl("0 ou 1", refus(modele_g("GLM", "compte", "x2", famille = "binomiale"))))
verifier("un décalage avec la loi normale : refusé", grepl("lien log", refus(modele_g("GLM", "masse", "x2", decalage = ug("effort")))))
verifier("une courbe dans un GLM : refusée", grepl("additif", refus(modele_g("GLM", "masse", c("x1", "x2"), famille = "gamma", lisses = ug("x1")))))
verifier("l'autocorrélation avec une loi gamma : refusée (mgcv::gamm passerait par la PQL, sans AIC)",
         grepl("loi normale", refus(modele_g("GLM", "masse", "x2", famille = "gamma",
         aleatoires = list(list(by = "variable", variable = ug("site"))), correlation = list(type = "CAR1", temps = list(by = "variable", variable = ug("x1")))))))

cat("Modèle additif mixte avec une autocorrélation dans le temps (mgcv::gamm)\n")
# comme les scripts de seuils : y ~ s(x, k = 5) + moyenne de x dans le secteur, un effet aléatoire
# site, une autocorrélation CAR1 le long des années dans chaque secteur de chaque site
set.seed(23)
ta <- expand.grid(annee = c(2001:2005, 2007:2011), secteur = c("a", "b", "c"), site = sprintf("S%d", 1:7), stringsAsFactors = FALSE)
na <- nrow(ta)
ta$nom <- sprintf("A%03d", seq_len(na))
ta$x <- round(runif(na, 0, 10), 3)
ta$xbar <- ave(ta$x, ta$site, ta$secteur)
bruit <- numeric(na)
for (i in split(seq_len(na), paste(ta$site, ta$secteur))) bruit[i[order(ta$annee[i])]] <- as.vector(stats::filter(rnorm(length(i), 0, 0.4), 0.5, "recursive"))
ta$y <- 1 + 1.2 * sin(ta$x / 2) + 0.25 * ta$xbar + rnorm(7, 0, 0.5)[as.integer(factor(ta$site))] + bruit
pa <- projet_depuis_tableau(ta[c("nom", "site", "secteur", "annee", "x", "xbar", "y")], "seuils")
ua <- function(...) numeros(pa, c(...))
par_variable <- function(n) list(by = "variable", variable = ua(n))
car1_a <- list(type = "CAR1", temps = par_variable("annee"), dans = par_variable("secteur"))
da <- definir_modele(pa, "GAM", x = ua("x", "xbar"), y = ua("y"), echelle = "none", famille = "gaussienne", lisses = ua("x"),
                     k_courbes = stats::setNames(list(5), ua("x")), aleatoires = list(par_variable("site")), correlation = car1_a)
ma <- ajuster(pa, da)
ta$site_f <- factor(ta$site)
ta$secteur_f <- factor(ta$secteur)
fa <- stats::as.formula("y ~ s(x, k = 5) + xbar", env = new.env(parent = asNamespace("mgcv")))
cor_a <- nlme::corCAR1(form = ~ annee | site_f / secteur_f)
ref_reml <- mgcv::gamm(fa, random = list(site_f = ~1), correlation = cor_a, data = ta, method = "REML")
ref_ml <- mgcv::gamm(fa, random = list(site_f = ~1), correlation = cor_a, data = ta, method = "ML")
verifier(sprintf("= mgcv::gamm() : coefficients, edf de la courbe (%.2f, k = 5), φ (%.3f)", ma$resultat$courbes$edf, ma$resultat$phi),
         isTRUE(ma$gamm) && ma$k[[1]] == 5 && proche(ma$resultat$coefficients$estimation, summary(ref_reml$gam)$p.table[, 1], 1e-5) &&
           proche(ma$resultat$courbes$edf, summary(ref_reml$gam)$s.table[1, "edf"], 1e-5) &&
           proche(ma$resultat$phi, unname(stats::coef(ref_reml$lme$modelStruct$corStruct, unconstrained = FALSE)), 1e-4))
verifier(sprintf("= mgcv::gamm() : la variance du site (REML) ; logLik et AIC de l'ajustement en ML (AIC %.1f)", ma$resultat$AIC),
         proche(ma$resultat$variances$variance[1], ref_reml$lme$sigma^2 * as.matrix(ref_reml$lme$modelStruct$reStruct$site_f)[1, 1], 1e-4) &&
           proche(ma$resultat$variances$variance[2], ref_reml$lme$sigma^2, 1e-5) &&
           proche(ma$resultat$logLik, as.numeric(stats::logLik(ref_ml$lme)), 1e-6) && proche(ma$resultat$AIC, stats::AIC(ref_ml$lme), 1e-6))
verifier("valeurs ajustées : avec le site ($lme) et par les effets fixes ($gam) ; résidus normalisés",
         proche(ma$resultat$ajuste, as.vector(stats::fitted(ref_reml$lme)), 1e-5) && proche(ma$resultat$ajuste_fixe, as.vector(stats::fitted(ref_reml$gam)), 1e-5) &&
           proche(ma$resultat$residus, as.vector(stats::residuals(ref_reml$lme, type = "normalized")), 1e-4) &&
           !is.null(ma$resultat$r2$marginal) && ma$resultat$r2$conditionnel >= ma$resultat$r2$marginal)
pred_a <- rep(NA_real_, na)
for (s in unique(ta$site)) {
  ici <- ta$site == s
  appris <- droplevels(ta[!ici, ])
  mm <- mgcv::gamm(fa, random = list(site_f = ~1), correlation = cor_a, data = appris, method = "REML")
  pred_a[ici] <- predict(mm$gam, ta[ici, ])
}
verifier(sprintf("validation croisée par site (le premier effet aléatoire) : Q2 = %.3f, recalculé à la main", ma$vc$Q2),
         ma$G == 7 && proche(ma$vc$Q2, 1 - sum((ta$y - pred_a)^2) / sum((ta$y - mean(ta$y))^2), 1e-5))
pra <- predire(pa, ma, da, numeros_observations(pa)[1:5])
verifier("prédiction : par les effets fixes et la courbe, comme predict() de $gam",
         proche(pra[["y prédit"]], as.vector(predict(ref_reml$gam, ta[1:5, ])), 1e-5))
aa <- apport_groupes(ma)
ea <- effets_regression(ma)
verifier("apport des termes (réajustés en ML) ; la courbe d'effet de x", nrow(aa) == 2 && all(!is.na(aa$delta_aic)) && isTRUE(ea[[1]]$courbe) && length(ea[[1]]$x) == 80)
ma0 <- ajuster(pa, definir_modele(pa, "GAM", x = ua("x", "xbar"), y = ua("y"), echelle = "none", famille = "gaussienne", lisses = ua("x"),
                                  k_courbes = stats::setNames(list(5), ua("x")), aleatoires = list(par_variable("site"))))
verifier("la vraisemblance de mgcv::gamm a son propre ensemble AIC (pas celui du même modèle sans autocorrélation)",
         grepl("gamm$", ma$signature) && ma$signature != ma0$signature && !isTRUE(ma0$gamm))
fg <- stats::as.formula("y ~ s(x, k = 5) + xbar + s(site_f, bs = \"re\")", env = new.env(parent = asNamespace("mgcv")))
ref_gam <- mgcv::gam(fg, data = ta, method = "REML")
verifier(sprintf("sans autocorrélation, la courbe de dimension 5 aussi : = mgcv::gam() (edf %.2f, AIC de l'ajustement en ML)", ma0$resultat$courbes$edf),
         ma0$k[[1]] == 5 && proche(ma0$resultat$courbes$edf, summary(ref_gam)$s.table["s(x)", "edf"], 1e-4) &&
           proche(ma0$resultat$AIC, stats::AIC(mgcv::gam(fg, data = ta, method = "ML")), 1e-6))
dl <- definir_modele(pa, "LMM", x = ua("x", "xbar"), y = ua("y"), echelle = "none", aleatoires = list(par_variable("site")), correlation = car1_a)
ml_ref <- nlme::lme(y ~ x + xbar, random = ~ 1 | site_f, correlation = cor_a, data = ta, method = "ML")
verifier("modèle linéaire mixte : l'autocorrélation dans chaque secteur, seul le site aléatoire (= nlme::lme)", proche(ajuster(pa, dl)$resultat$AIC, stats::AIC(ml_ref), 1e-6))
verifier("une autocorrélation sans effet aléatoire : refusée", grepl("effet aléatoire", refus(definir_modele(pa, "GAM", x = ua("x"), y = ua("y"), famille = "gaussienne",
         correlation = car1_a))))
verifier("une dimension de courbe impossible (k = 2) : refusée", grepl("dimension", refus(definir_modele(pa, "GAM", x = ua("x"), y = ua("y"), famille = "gaussienne",
         lisses = ua("x"), k_courbes = stats::setNames(list(2), ua("x"))))))
for (langue_a in c("fr", "en")) {
  tma <- texte_methodes(pa, da, ma, langue_a, "essai")
  verifier(sprintf("texte des méthodes (%s) : mgcv::gamm, l'autocorrélation dans chaque secteur, Wood et Pinheiro cités, R² marginal", langue_a),
           any(grepl("gamm", tma$paragraphes)) && any(grepl("CAR1", tma$paragraphes)) && any(grepl("secteur", tma$paragraphes)) &&
             any(grepl("Pinheiro", tma$references)) && any(grepl("Wood", tma$references)) && any(grepl("marginal", tma$paragraphes)) &&
             !any(grepl("NA", tma$paragraphes, fixed = TRUE)))
}
fa_proj <- tempfile(fileext = ".nuee")
pa$models <- list(da)
enregistrer_projet(pa, fa_proj)
qa <- lire_projet(fa_proj)
ma2 <- ajuster(qa, qa$models[[1]])
verifier("enregistré puis relu : le même modèle (autocorrélation dans chaque secteur, k = 5)", proche(ma2$resultat$AIC, ma$resultat$AIC, 1e-8) && ma2$k[[1]] == 5)
# (les mêmes lignes dans l'ordre des années : nlme ne donne des résidus normalisés justes que sur
# des lignes rangées par groupe, puis par temps ; ceux de Nuée ne dépendent pas de l'ordre)
oa <- order(ta$annee, ta$site, ta$secteur)
pa_an <- projet_depuis_tableau(ta[oa, c("nom", "site", "secteur", "annee", "x", "xbar", "y")], "seuils par année")
ua_an <- function(...) numeros(pa_an, c(...))
car1_an <- list(type = "CAR1", temps = list(by = "variable", variable = ua_an("annee")), dans = list(by = "variable", variable = ua_an("secteur")))
site_an <- list(list(by = "variable", variable = ua_an("site")))
ma_an <- ajuster(pa_an, definir_modele(pa_an, "GAM", x = ua_an("x", "xbar"), y = ua_an("y"), echelle = "none", famille = "gaussienne", lisses = ua_an("x"),
                                       k_courbes = stats::setNames(list(5), ua_an("x")), aleatoires = site_an, correlation = car1_an))
ml_an <- ajuster(pa_an, definir_modele(pa_an, "LMM", x = ua_an("x", "xbar"), y = ua_an("y"), echelle = "none", aleatoires = site_an, correlation = car1_an))
ml_gr <- ajuster(pa, definir_modele(pa, "LMM", x = ua("x", "xbar"), y = ua("y"), echelle = "none", aleatoires = list(par_variable("site")), correlation = car1_a))
par_nom <- function(pr, m) stats::setNames(m$resultat$residus, unname(unlist(pr$noms_obs[as.character(m$observations)])))
rg <- par_nom(pa, ma); ra <- par_nom(pa_an, ma_an)
lg <- par_nom(pa, ml_gr); la <- par_nom(pa_an, ml_an)
nlme_an <- stats::setNames(as.vector(stats::residuals(ml_an$ajustement, type = "normalized")), names(la))
verifier(sprintf("CAR1, lignes dans l'ordre des années : les mêmes résidus normalisés (GAMM et modèle mixte ; écart-type %.2f), là où ceux de nlme s'écartent jusqu'à %.1f",
                 stats::sd(la), max(abs(nlme_an[names(lg)] - lg))),
         proche(ra[names(rg)], rg, 1e-5) && proche(la[names(lg)], lg, 1e-5) && max(abs(nlme_an[names(lg)] - lg)) > 0.1 &&
           proche(lg, as.vector(stats::residuals(ml_gr$ajustement, type = "normalized")), 1e-8))

cat("Autocorrélation des résidus (regression_diagnostics.R)\n")
# 8 sites suivis 10 ans (2005 manque), des résidus autocorrélés dans le temps ; des lignes rangées
# par site puis par année, l'ordre où nlme::ACF lit juste
set.seed(12)
tac <- data.frame(nom = sprintf("R%03d", 1:80), site = rep(sprintf("S%d", 1:8), each = 10), annee = rep(c(2001:2004, 2006:2011), 8))
tac$x <- rnorm(80)
tac$z <- rnorm(80)
tac$y <- 1 + 0.5 * tac$x + 0.3 * tac$z + rep(rnorm(8), each = 10) + as.vector(stats::filter(rnorm(80), 0.6, "recursive"))
tac$compte <- stats::rpois(80, exp(0.5 + 0.3 * tac$x))
pac <- projet_depuis_tableau(tac, "acf")
uac <- function(...) numeros(pac, c(...))
annee_ac <- list(by = "variable", variable = uac("annee"))
site_ac <- list(by = "variable", variable = uac("site"))
car1_ac <- list(type = "CAR1", temps = annee_ac)
regr_ac <- function(type, x, ...) ajuster(pac, definir_modele(pac, type, x = if (length(x)) uac(x), y = uac("y"), echelle = "none", ...))
# (l'autocorrélation refaite autrement : chaque décalage sur les paires de chaque groupe, rangé par temps)
acf_main <- function(e, temps, groupe, L) {
  r <- vapply(0:L, function(k) {
    s <- n <- 0
    for (g in unique(groupe)) {
      x <- e[groupe == g][order(temps[groupe == g])]
      if (length(x) > k) { s <- s + sum(x[seq_len(length(x) - k)] * x[(k + 1):length(x)]); n <- n + length(x) - k }
    }
    s / n
  }, 0)
  r / r[1]
}
mc_ac <- regr_ac("LMM", c("x", "z"), aleatoires = list(site_ac), correlation = car1_ac)
rc_ac <- nlme::lme(y ~ x + z, random = ~ 1 | site, correlation = nlme::corCAR1(form = ~ annee | site), data = tac, method = "REML", control = ctrl)
ac <- autocorrelation_residus(pac, mc_ac)
Lac <- max(ac$decalage)
verifier(sprintf("CAR1 : résidus et résidus normalisés = nlme::ACF() (décalages 0 à %d, les paires de chaque décalage, la bande ±1,96/√n)", Lac),
         Lac == 9 && proche(ac$series[[1]]$acf, nlme::ACF(rc_ac, maxLag = Lac, resType = "pearson")$ACF) &&
           proche(ac$series[[2]]$acf, nlme::ACF(rc_ac, maxLag = Lac, resType = "normalized")$ACF, 1e-5) &&
           identical(as.numeric(ac$paires), as.numeric(attr(nlme::ACF(rc_ac, maxLag = Lac), "n.used"))) && proche(ac$bande, qnorm(0.975) / sqrt(ac$paires)) &&
           identical(c(ac$temps, ac$groupe), c("annee", "site")) && ac$groupes == 8 && isTRUE(ac$car1))
verifier("résidus normalisés de CAR1 = residuals(type = \"normalized\") de nlme, sur ces lignes rangées",
         proche(residus_normalises_car1(residuals(rc_ac, type = "pearson"), tac$annee, tac$site, ac$phi), as.vector(residuals(rc_ac, type = "normalized")), 1e-5))
o_ac <- order(tac$annee, tac$site)
pac2 <- projet_depuis_tableau(tac[o_ac, ], "acf")
mc2_ac <- ajuster(pac2, definir_modele(pac2, "LMM", x = numeros(pac2, c("x", "z")), y = numeros(pac2, "y"), echelle = "none",
                                       aleatoires = list(list(by = "variable", variable = numeros(pac2, "site"))),
                                       correlation = list(type = "CAR1", temps = list(by = "variable", variable = numeros(pac2, "annee")))))
ac2 <- autocorrelation_residus(pac2, mc2_ac)
rc2_ac <- nlme::lme(y ~ x + z, random = ~ 1 | site, correlation = nlme::corCAR1(form = ~ annee | site), data = tac[o_ac, ], method = "REML", control = ctrl)
verifier("lignes rangées par année puis par site : la même autocorrélation (celle des résidus normalisés de nlme est alors fausse)",
         proche(ac2$series[[1]]$acf, ac$series[[1]]$acf, 1e-5) && proche(ac2$series[[2]]$acf, ac$series[[2]]$acf, 1e-5) &&
           !proche(nlme::ACF(rc2_ac, maxLag = Lac, resType = "normalized")$ACF, ac$series[[2]]$acf, 1e-2))
m0_ac <- regr_ac("LMM", c("x", "z"), aleatoires = list(site_ac))
r0_ac <- nlme::lme(y ~ x + z, random = ~ 1 | site, data = tac, method = "REML", control = ctrl)
a0 <- autocorrelation_residus(pac, m0_ac, temps = annee_ac, groupe = list(by = "effets"))
verifier(sprintf("sans CAR1, dans les groupes de l'effet aléatoire : = nlme::ACF() ; au décalage 1, %.2f, au-delà de la bande (%.2f)", a0$series[[1]]$acf[2], a0$bande[2]),
         length(a0$series) == 1 && proche(a0$series[[1]]$acf, nlme::ACF(r0_ac, maxLag = 9, resType = "pearson")$ACF) && a0$series[[1]]$acf[2] > a0$bande[2] &&
           identical(a0$groupe, "site") && !isTRUE(a0$car1))
ml_ac <- regr_ac("LM", c("x", "z"))
al <- autocorrelation_residus(pac, ml_ac, temps = annee_ac, groupe = site_ac)
verifier("régression linéaire, les groupes d'une variable : = nlme::ACF() de gls(), form = ~ 1 | site",
         proche(al$series[[1]]$acf, nlme::ACF(nlme::gls(y ~ x + z, data = tac), maxLag = 9, form = ~ 1 | site)$ACF, 1e-8))
mg_ac <- regr_ac("GLM", c("x", "z"), famille = "gaussienne", aleatoires = list(site_ac))
verifier("mgcv (GLMM normal, le site aléatoire en REML) : = nlme::ACF() du même modèle ajusté par nlme",
         proche(autocorrelation_residus(pac, mg_ac, temps = annee_ac, groupe = list(by = "effets"))$series[[1]]$acf, nlme::ACF(r0_ac, maxLag = 9)$ACF, 1e-5))
mp_ac <- ajuster(pac, definir_modele(pac, "GLM", x = uac("x"), y = uac("compte"), echelle = "none", famille = "poisson"))
gp_ac <- glm(compte ~ x, family = poisson, data = tac)
ad <- autocorrelation_residus(pac, mp_ac, temps = annee_ac, groupe = site_ac, type = "deviance")
verifier("GLM de Poisson : résidus de déviance ou de Pearson de glm(), l'autocorrélation refaite à la main",
         proche(ad$series[[1]]$acf, acf_main(residuals(gp_ac, type = "deviance"), tac$annee, tac$site, 9)) && identical(ad$series[[1]]$residus, "deviance") &&
           proche(autocorrelation_residus(pac, mp_ac, temps = annee_ac, groupe = site_ac)$series[[1]]$acf, acf_main(residuals(gp_ac, type = "pearson"), tac$annee, tac$site, 9)))
mgm_ac <- regr_ac("GAM", c("x", "z"), famille = "gaussienne", lisses = uac("x"), aleatoires = list(site_ac), correlation = car1_ac)
agm <- autocorrelation_residus(pac, mgm_ac)
verifier("mgcv::gamm avec CAR1 : sa partie $lme, résidus et résidus normalisés = nlme::ACF()",
         isTRUE(mgm_ac$gamm) && proche(agm$series[[1]]$acf, nlme::ACF(mgm_ac$ajustement$lme, maxLag = 9, resType = "pearson")$ACF) &&
           proche(agm$series[[2]]$acf, nlme::ACF(mgm_ac$ajustement$lme, maxLag = 9, resType = "normalized")$ACF, 1e-5))
asect <- autocorrelation_residus(pa, ma)
secteurs <- paste(ta$site, ta$secteur)
verifier("CAR1 dans chaque secteur de chaque site (mgcv::gamm) : 21 groupes, les résidus normalisés de nlme (lignes rangées), l'autocorrélation refaite à la main",
         asect$groupes == 21 && identical(asect$groupe, "secteur") && max(asect$decalage) == 9 &&
           proche(asect$series[[2]]$acf, acf_main(as.vector(residuals(ref_reml$lme, type = "normalized")), ta$annee, secteurs, 9), 1e-4))
mla <- ajuster(pa, definir_modele(pa, "LM", x = ua("x", "xbar"), y = ua("y"), echelle = "none"))
ala <- autocorrelation_residus(pa, mla, temps = par_variable("annee"), groupe = list(par_variable("site"), par_variable("secteur")))
verifier("des groupes emboîtés, le secteur dans son site (ses noms se répètent d'un site à l'autre) : 21 séries ; le secteur seul, refusé",
         ala$groupes == 21 && identical(ala$groupe, "site / secteur") && proche(ala$series[[1]]$acf, acf_main(residuals(lm(y ~ x + xbar, data = ta)), ta$annee, secteurs, 9)) &&
           grepl("même temps", refus(autocorrelation_residus(pa, mla, temps = par_variable("annee"), groupe = par_variable("secteur")))))
verifier("plusieurs observations au même temps dans un groupe, ou pas de temps : refusé, en disant pourquoi",
         grepl("même temps", refus(autocorrelation_residus(pac, ml_ac, temps = annee_ac))) && grepl("temps", refus(autocorrelation_residus(pac, ml_ac))))

cat("Tests du rapport de vraisemblance (regression_diagnostics.R)\n")
mx_ac <- regr_ac("LMM", "x", aleatoires = list(site_ac))
mn_ac <- regr_ac("LMM", NULL, aleatoires = list(site_ac))
mz_ac <- regr_ac("LMM", "z", aleatoires = list(site_ac))
rv <- rapports_vraisemblance(list(xz = m0_ac, x = mx_ac, nul = mn_ac, z = mz_ac, lm = ml_ac, car1 = mc_ac, glmm = mg_ac))
paire_rv <- function(r, g, p) which(r$paires$grand == g & r$paires$petit == p)
ml_lme <- function(f, ...) nlme::lme(f, random = ~ 1 | site, data = tac, method = "ML", control = ctrl, ...)
an <- anova(ml_lme(y ~ x), ml_lme(y ~ x + z))
i_rv <- paire_rv(rv, "xz", "x")
verifier(sprintf("modèles mixtes emboîtés (nlme, en ML) : χ² et p = anova(m1, m2) de nlme (χ² = %.3f, p = %.3f)", an$L.Ratio[2], an$`p-value`[2]),
         length(i_rv) == 1 && proche(rv$paires$khi2[i_rv], an$L.Ratio[2]) && proche(rv$paires$p[i_rv], an$`p-value`[2]) && rv$paires$ddl[i_rv] == 1)
verifier("les paires : x + z avec x, z et le modèle nul, x et z avec le modèle nul ; ni x avec z, ni la régression sans effet aléatoire, le modèle avec CAR1 ou celui de mgcv",
         setequal(paste(rv$paires$grand, rv$paires$petit), c("xz x", "xz nul", "xz z", "x nul", "z nul")) && rv$total == 5 && !any(rv$paires$approche))
mcx_ac <- regr_ac("LMM", "x", aleatoires = list(site_ac), correlation = car1_ac)
rv1 <- rapports_vraisemblance(list(grand = mc_ac, petit = mcx_ac))
an1 <- anova(ml_lme(y ~ x, correlation = nlme::corCAR1(form = ~ annee | site)), ml_lme(y ~ x + z, correlation = nlme::corCAR1(form = ~ annee | site)))
verifier("la même autocorrélation CAR1 dans les deux : = anova() de nlme", rv1$total == 1 && identical(rv1$paires$grand, "grand") &&
           proche(rv1$paires$khi2, an1$L.Ratio[2], 1e-5) && proche(rv1$paires$p, an1$`p-value`[2], 1e-5))
l1_ac <- lm(y ~ x, data = tac)
l2_ac <- lm(y ~ x + z, data = tac)
khi2_lm <- 2 * (as.numeric(logLik(l2_ac)) - as.numeric(logLik(l1_ac)))
rl <- rapports_vraisemblance(list(petit = regr_ac("LM", "x"), grand = ml_ac))
# (anova(test = "Chisq") de deux lm() rapporte l'écart des sommes de carrés à σ² du grand modèle : un autre test)
verifier("régressions linéaires : χ² = 2 Δ logLik de lm(), sur 1 ddl", rl$total == 1 && identical(rl$paires$grand, "grand") &&
           proche(rl$paires$khi2, khi2_lm) && proche(rl$paires$p, pchisq(khi2_lm, 1, lower.tail = FALSE)) && rl$paires$ddl == 1)
mg2b <- modele_g("GLM", "compte", "x2", famille = "poisson", decalage = ug("effort"))
rg <- rapports_vraisemblance(list(petit = mg2b, grand = mg2, nb = mg3, sans_effort = mg1))
an_glm <- anova(glm(compte ~ x2, family = poisson, data = tg, offset = log(effort)), glm(compte ~ x1 + x2, family = poisson, data = tg, offset = log(effort)), test = "Chisq")
verifier("GLM de Poisson avec l'effort : χ² et p = anova(test = \"Chisq\") de glm() ; ni la binomiale négative (une autre loi), ni sans l'effort",
         rg$total == 1 && identical(rg$paires$grand, "grand") && proche(rg$paires$khi2, an_glm$Deviance[2]) && proche(rg$paires$p, an_glm$`Pr(>Chi)`[2]))
mga <- modele_g("GAM", "compte", c("x1", "x2"), famille = "poisson", lisses = ug("x1"), decalage = ug("effort"))
rga <- rapports_vraisemblance(list(gam = mga, glm = mg2, petit = mg2b))
verifier("une courbe : la droite de x1 est dans sa courbe, et le test est approché (des edf)",
         rga$total == 3 && all(rga$paires$approche[rga$paires$grand == "gam"]) && !rga$paires$approche[paire_rv(rga, "glm", "petit")] &&
           length(paire_rv(rga, "gam", "glm")) == 1)
rcap <- rapports_vraisemblance(list(xz = m0_ac, x = mx_ac, nul = mn_ac, z = mz_ac), max_paires = 2)
verifier("au plus max_paires paires, et combien il y en a ; un modèle et lui-même ne font pas une paire",
         nrow(rcap$paires) == 2 && rcap$total == 5 && rapports_vraisemblance(list(a = mx_ac, b = mx_ac))$total == 0)

cat("Prédiction\n")
pr <- predire(p, m, d, obs)
xc <- stats::complete.cases(df[c("sucre", "fermete", "acidite", "polyphenols")])
verifier("linéaire : chaque observation dont les X sont connus prédite comme predict(lm()), son Y connu ou non",
         identical(!is.na(pr[["note_gout prédit"]]), xc) && proche(pr[["note_gout prédit"]][xc], predict(l, df[xc, ])) &&
           proche(pr[["note_gout observé"]], df$note) && !any(pr$hors_domaine))
pr3 <- predire(p, m3, d3, obs)
xc3 <- stats::complete.cases(df[c("sucre", "fermete")])
verifier("mixte : prédit par les effets fixes, comme predict(lme(), level = 0)",
         proche(pr3[["note_gout prédit"]][xc3], as.vector(predict(r_reml, df[xc3, ], level = 0)), 1e-5))
dossier <- tempfile("prediction")
dir.create(dossier)
f <- file.path(dossier, "vergers.csv")
utils::write.csv(data.frame(nom = c("A", "B", "C"), sucre = c(12, 40, 13), fermete = c(7, 7, NA), acidite = c(5, 5, 5), polyphenols = c(1000, 1000, 1000)),
                 f, row.names = FALSE)
rf <- predire_fichier(p, m, d, f)
nf <- data.frame(sucre = 12, fermete = 7, acidite = 5, polyphenols = 1000)
verifier("un fichier : la prédiction de lm(), une valeur hors de l'étendue du modèle signalée, une observation incomplète sans prédiction",
         proche(rf$observations[["note_gout prédit"]][1], predict(l, nf)) && identical(rf$observations$hors_domaine, c(FALSE, TRUE, FALSE)) &&
           is.na(rf$observations[["note_gout prédit"]][3]) && identical(as.character(rf$observations$nom), c("A", "B", "C")) && !length(rf$absentes))
fg <- file.path(dossier, "comptes.csv")
utils::write.csv(tg[1:5, c("nom", "x1", "x2", "effort")], fg, row.names = FALSE)
ref <- glm(compte ~ x1 + x2, family = poisson, data = tg, offset = log(effort))
rg <- predire_fichier(pg, mg2, definir_modele(pg, "GLM", x = ug("x1", "x2"), y = ug("compte"), echelle = "none", famille = "poisson", decalage = ug("effort")), fg)
verifier("Poisson avec l'effort : la prédiction de glm() pour l'effort de chaque observation",
         proche(rg$observations[["compte prédit"]], unname(predict(ref, tg[1:5, ], type = "response"))) && !length(rg$note))
utils::write.csv(tg[1:5, c("nom", "x1", "x2")], fg, row.names = FALSE)
rg1 <- predire_fichier(pg, mg2, definir_modele(pg, "GLM", x = ug("x1", "x2"), y = ug("compte"), echelle = "none", famille = "poisson", decalage = ug("effort")), fg)
verifier("sans l'effort dans le fichier : la prédiction pour un effort de 1, et la note le dit",
         proche(rg1$observations[["compte prédit"]], unname(predict(ref, transform(tg[1:5, ], effort = 1), type = "response"))) &&
           any(grepl("effort", rg1$note)))
dg6 <- definir_modele(pg, "GAM", x = ug("x1", "x2"), y = ug("compte"), echelle = "none", famille = "binomiale_negative", lisses = ug("x1"), decalage = ug("effort"),
                      aleatoires = list(list(by = "variable", variable = ug("site"))))
pg6 <- predire(pg, mg6, dg6, numeros_observations(pg)[1:5])
nd6 <- transform(tg[1:5, ], site = factor(tg$site[1], levels = sort(unique(tg$site))))
# (predict.gam ignore l'argument offset de gam() : log(effort) est ajouté ici)
verifier("GAMM : prédit sans l'effet aléatoire, comme predict.gam(exclude = \"s(site)\"), l'effort compris",
         proche(pg6[["compte prédit"]], exp(as.vector(predict(refr, nd6, type = "link", exclude = "s(site)")) + log(tg$effort[1:5])), 1e-4))
unlink(dossier, recursive = TRUE)
xr3 <- tables_resultats(p, m3)
xg6 <- tables_resultats(pg, mg6)
verifier("Exporter : observations, coefficients, qualité, effets aléatoires (et les courbes d'un GAMM)",
         identical(names(xr3), c("observations", "coefficients", "qualite", "effets_aleatoires")) && nrow(xr3$observations) == m3$N &&
           proche(xr3$qualite$AIC, m3$resultat$AIC) && identical(names(xg6), c("observations", "coefficients", "qualite", "effets_aleatoires", "courbes")))

cat("Formule R\n")
reglees <- function(projet, r, ...) definir_modele(projet, r$type, x = r$x, y = r$y, echelle = "none", transformations = unlist(r$transformations),
                                                    croises = r$croises, aleatoires = r$aleatoires, lisses = r$lisses, decalage = r$decalage, ...)
r1 <- lire_formule(p, "note_gout ~ sucre * fermete + log10(acidite) + I(sucre^2) + (1 | verger)")
verifier("lue : variables, transformation, terme croisé, carré, effet aléatoire ; le type devient mixte",
         r1$type == "LMM" && identical(as.numeric(r1$x), u("sucre", "fermete", "acidite")) && r1$y == u("note_gout") &&
           identical(r1$transformations[[as.character(u("acidite"))]], "log10") &&
           setequal(lapply(r1$croises, sort), list(sort(u("sucre", "fermete")), u("sucre", "sucre"))) &&
           identical(r1$aleatoires, I(list(list(by = "variable", variable = u("verger"))))))
mr1 <- ajuster(p, reglees(p, r1))
ref <- nlme::lme(note ~ sucre * fermete + log10(acidite) + I(sucre^2), random = ~ 1 | verger, data = df, method = "ML", control = ctrl)
verifier("ajusté, le modèle de cette formule = nlme::lme() de la même formule (AIC en ML)", proche(mr1$resultat$AIC, AIC(ref), 1e-6))
texte1 <- formule_reglages(p, r1)
r1b <- lire_formule(p, texte1, "LMM")
verifier(sprintf("écrite puis relue, les mêmes réglages : %s", texte1),
         identical(r1b$x, r1$x) && identical(r1b$transformations, r1$transformations) && identical(r1b$aleatoires, r1$aleatoires) &&
           setequal(lapply(r1b$croises, sort), lapply(r1$croises, sort)))
codes <- c("log10", "log10:1:2", "ln:2:1", "neglog:1:0", "exp:0.5:0", "lineaire:2:-3", "logit:0:20", "logit:1:30", "racine2", "racine4",
           "puissance:1:0:3", "puissance:2:1:0.33")
relus <- vapply(codes, function(code) {
  texte <- formule_reglages(p, list(type = "LM", x = u("sucre"), y = u("note_gout"), transformations = stats::setNames(list(code), u("sucre"))))
  lu <- lire_formule(p, texte)$transformations[[as.character(u("sucre"))]] %||% ""
  if (identical(nom_transformation(lire_transformation(lu)), nom_transformation(lire_transformation(code)))) "" else sprintf("%s → %s → %s", code, texte, lu)
}, "")
verifier(sprintf("chaque transformation écrite en R se relit (%d)", length(codes)), !any(nzchar(relus)))
if (any(nzchar(relus))) cat("         ", relus[nzchar(relus)], sep = "\n          ")
r2 <- lire_formule(p, "note_gout ~ s(sucre) + `fermete`", "LMM")
verifier("une courbe : le type devient additif ; un nom entre accents graves", r2$type == "GAM" && r2$lisses == u("sucre") && setequal(r2$x, u("sucre", "fermete")))
r2k <- lire_formule(p, "note_gout ~ s(sucre, k = 5) + fermete", "LM")
verifier(sprintf("une courbe de dimension fixée, s(sucre, k = 5) : lue, et réécrite (%s)", formule_reglages(p, r2k)),
         identical(r2k$k_courbes[[as.character(u("sucre"))]], 5L) && grepl("s(sucre, k = 5)", formule_reglages(p, r2k), fixed = TRUE))
r3 <- lire_formule(p, "log(note_gout) ~ I(sucre^2) + variete", "LMM")
verifier("sans effet aléatoire, un modèle linéaire ; x² seul, une transformation de x ; une réponse transformée",
         r3$type == "LM" && identical(r3$transformations[[as.character(u("sucre"))]], "puissance:1:0:2") && !length(r3$croises) &&
           identical(r3$transformations[[as.character(u("note_gout"))]], "ln:1:0") && u("variete") %in% r3$x)
rg <- lire_formule(pg, "compte ~ s(x1) + x2 + offset(log(effort)) + (1 | site)", "GLM", "binomiale_negative")
mrg <- ajuster(pg, reglees(pg, rg, famille = "binomiale_negative"))
verifier("un GAMM avec l'effort, lu d'une formule : le modèle de mgcv (AIC)", rg$type == "GAM" && rg$decalage == ug("effort") && proche(mrg$resultat$AIC, mg6$resultat$AIC, 1e-6))
verifier("l'effort avec une loi normale : refusé", grepl("offset", refus(lire_formule(pg, "compte ~ x2 + offset(log(effort))", "GLM", "gaussienne"))))
marqueur <- file.path(tempdir(), "formule_executee")
dangereuses <- c(sprintf("note_gout ~ sucre + file.create(\"%s\")", marqueur), sprintf("file.create(\"%s\") ~ sucre", marqueur),
                 sprintf("note_gout ~ log(file.create(\"%s\"))", marqueur), sprintf("note_gout ~ I(eval(parse(text = 'file.create(\"%s\")')))", marqueur),
                 sprintf("note_gout ~ s(sucre, k = file.create(\"%s\"))", marqueur), "note_gout ~ sucre[1]", "note_gout ~ get(\"sucre\")", "note_gout ~ \"sucre\"",
                 "note_gout ~ (function() sucre)()", "note_gout ~ sucre; file.remove(\"x\")")
verifier("du code dans une formule : refusé, jamais exécuté", all(vapply(dangereuses, function(f) nzchar(refus(lire_formule(p, f))), TRUE)) && !file.exists(marqueur))
messages <- c("note_gout ~ sucre - 1" = "ordonnée", "note_gout ~ (sucre | verger)" = "aléatoires", "note_gout ~ sucre + (1 | verger) + (1 | culture)" = "emboîtés",
              "note_gout ~ s(sucre, bs = \"cr\")" = "dimension", "note_gout ~ s(sucre, k = 2)" = "entier de 3", "note_gout ~ sucre:fermete" = "demande aussi",
              "note_gout ~ inconnue" = "pas une variable",
              "note_gout ~ sucre^2" = "I\\(x\\^2\\)", "variete ~ sucre" = "qualitative", "note_gout ~ sucre + log(sucre)" = "deux façons",
              "note_gout ~ s(variete)" = "quantitative", "note_gout ~ sucre +" = "illisible")
mauvais <- names(messages)[!vapply(names(messages), function(f) grepl(messages[[f]], refus(lire_formule(p, f))), TRUE)]
verifier("une formule que Nuée ne sait pas faire : refusée, en disant pourquoi", !length(mauvais))
if (length(mauvais)) cat("          ", mauvais, sep = "\n          ")

cat("Variables par groupe (Nouvelle variable)\n")
pv2 <- variable_calculee(variable_calculee(p, "sucre_verger", "moyenne_groupe(sucre, verger)"), "sucre_intra", "ecart_groupe(sucre, verger)")
attendu <- ave(df$sucre, df$verger, FUN = function(v) mean(v, na.rm = TRUE))
verifier("moyenne par groupe et écart à la moyenne = ave(), les valeurs manquantes ignorées",
         proche(colonne(pv2, numeros(pv2, "sucre_verger"), obs), attendu) && proche(colonne(pv2, numeros(pv2, "sucre_intra"), obs), df$sucre - attendu))
pid <- projet_depuis_tableau(data.frame(nom = sprintf("O%d", 1:6), x = c(1, 2, 3, 10, NA, 14)), "ids")
pid$observation_id_names <- c("Observation", "secteur")
for (i in seq_along(pid$observations)) pid$observations[[i]]$ids <- c(pid$observations[[i]]$ids[1], c("a", "a", "a", "b", "b", "b")[i])
pid <- variable_calculee(pid, "x_secteur", "group_mean(x, secteur)")
verifier("un identifiant des observations fait les groupes d'une formule (le secteur)",
         proche(colonne(pid, numeros(pid, "x_secteur"), numeros_observations(pid)), c(2, 2, 2, 12, 12, 12)))
pna <- pid
pna$observations[[6]]$ids[2] <- ""
pna <- variable_calculee(variable_calculee(pna, "x_moy", "moyenne_groupe(x, secteur)"), "x_ecart", "ecart_groupe(x, secteur)")
verifier("une observation sans groupe (identifiant vide) n'a ni moyenne ni écart de groupe",
         proche(colonne(pna, numeros(pna, "x_moy"), numeros_observations(pna)), c(2, 2, 2, 10, 10, NA)) &&
           proche(colonne(pna, numeros(pna, "x_ecart"), numeros_observations(pna)), c(-1, 0, 1, 0, NA, NA)))
# precedent(), suivant() et croissance() (previous, following, growth_rate) : deux sites qui ont
# chacun un secteur S1, dans le désordre ; A-S1 n'a pas 2003, A-S2 n'a pas de densité en 2002
ds <- data.frame(nom = sprintf("L%02d", 1:10),
                 site = c("A", "B", "A", "A", "B", "A", "B", "A", "A", "B"),
                 secteur = c("S1", "S1", "S1", "S2", "S2", "S1", "S1", "S1", "S2", "S1"),
                 annee = c(2004, 2002, 2001, 2002, 2003, 2002, 2001, 2005, 2001, 2003),
                 densite = c(5, 2, 2, NA, 6, 3, 1, 0, 4, 4))
ds$code <- paste0(ds$site, "-", ds$secteur)
pl <- projet_depuis_tableau(ds, "series")
calcule <- function(pr, formule) {
  pr <- variable_calculee(pr, "essai", formule)
  colonne(pr, numeros(pr, "essai"), numeros_observations(pr))
}
avant <- c(NA, 1, NA, 4, NA, 2, NA, 5, NA, 2)
apres <- c(0, 4, 3, NA, NA, NA, 2, NA, NA, NA)
verifier("precedent() : la valeur de l'année d'avant du même secteur ; une année manquante donne NA, pas celle d'encore avant",
         proche(calcule(pl, "precedent(densite, annee, interaction(site, secteur))"), avant))
verifier("suivant() : celle de l'année d'après ; avec pas = 2, deux ans avant",
         proche(calcule(pl, "suivant(densite, annee, interaction(site, secteur))"), apres) &&
           proche(calcule(pl, "precedent(densite, annee, interaction(site, secteur), pas = 2)"), c(3, NA, NA, NA, NA, NA, NA, NA, NA, 1)))
verifier("le groupe : une variable qualitative, comme interaction(site, secteur)", proche(calcule(pl, "precedent(densite, annee, code)"), avant))
m_double <- refus(calcule(pl, "precedent(densite, annee, secteur)"))
m_sans <- refus(calcule(pl, "suivant(densite, annee)"))
verifier(sprintf("un couple (groupe, temps) qui revient est refusé, et nommé : « %s » ; sans groupe, un temps : « %s »", m_double, m_sans),
         grepl("« S1 »", m_double, fixed = TRUE) && grepl("2002", m_double) && grepl("interaction(site, secteur)", m_double, fixed = TRUE) &&
           startsWith(m_sans, "suivant()") && grepl("2002", m_sans))
verifier("croissance() = log(x / x de l'année d'avant), NA sauf si les deux sont > 0 (un zéro, une valeur manquante)",
         proche(calcule(pl, "croissance(densite, annee, code)"), c(NA, log(2), NA, NA, NA, log(1.5), NA, NA, NA, log(2))))
verifier("previous(), following() et growth_rate() : les mêmes, en anglais",
         proche(calcule(pl, "previous(densite, annee, code)"), avant) && proche(calcule(pl, "following(densite, annee, code)"), apres) &&
           proche(calcule(pl, "growth_rate(densite, annee, code)"), calcule(pl, "croissance(densite, annee, code)")) &&
           startsWith(refus(calcule(pl, "previous(densite, annee, secteur)")), "previous()"))
pl_ids <- projet_depuis_tableau(ds[c("nom", "densite")], "series par identifiants")
pl_ids$observation_id_names <- c("Observation", "site", "secteur", "an")
for (i in seq_along(pl_ids$observations)) pl_ids$observations[[i]]$ids <- c(pl_ids$observations[[i]]$ids[1], ds$site[i], ds$secteur[i], as.character(ds$annee[i]))
verifier("des identifiants des observations : le site et le secteur, et le temps fait de nombres (l'année)",
         proche(calcule(pl_ids, "precedent(densite, an, interaction(site, secteur))"), avant))
pl_vides <- pl_ids
pl_vides$observations[[6]]$ids[4] <- ""  # (L06, A-S1 2002, sans année)
pl_vides$observations[[2]]$ids[3] <- ""  # (L02, B-S1 2002, sans secteur)
verifier("une observation sans temps ou sans groupe a NA, et ne compte pour aucune autre",
         proche(calcule(pl_vides, "precedent(densite, an, interaction(site, secteur))"), c(NA, NA, NA, 4, NA, NA, NA, 5, NA, NA)) &&
           proche(calcule(pl_vides, "suivant(densite, an, interaction(site, secteur))"), c(0, NA, NA, NA, NA, NA, NA, NA, NA, NA)))
m_temps <- refus(calcule(pl_ids, "precedent(densite, secteur, site)"))
verifier(sprintf("le temps doit être numérique : « %s » ; le pas, positif", m_temps),
         grepl("numérique", m_temps) && grepl("« S1 »", m_temps, fixed = TRUE) && grepl("pas", refus(calcule(pl, "suivant(densite, annee, code, pas = 0)"))))

cat("Imputation par ACP (Nouvelle variable)\n")
set.seed(5)
ni <- 80
Xs <- matrix(rnorm(ni * 2), ni, 2) %*% matrix(runif(12, -1, 1), 2, 6) + matrix(rnorm(ni * 6, sd = 0.3), ni, 6)
Xs[, 6] <- as.numeric(Xs[, 6] > 0)  # (une variable 0/1)
Xs[matrix(runif(ni * 6) < 0.12, ni, 6)] <- NA
Xs[3, 1:4] <- NA  # (une observation à qui manquent plus de la moitié des variables)
pim <- projet_depuis_tableau(data.frame(nom = sprintf("I%02d", 1:ni), Xs), "imputation")
vars_i <- numeros(pim, paste0("X", 1:6))
imputees <- function(pr, suffixe = " (imputée)") sapply(paste0("X", 1:6, suffixe), function(n) colonne(pr, numeros(pr, n), numeros_observations(pr)))
garde <- rowMeans(is.na(Xs)) <= 0.5
# missMDA 1.23 sur Xs[garde, ] : imputePCA(, ncp = 2) et imputePCA(, ncp = 5), leurs $completeObs
# aux valeurs manquantes (missMDA y prend la SVD complète, comme Nuée ; avec moins de composantes
# que la moitié des variables, irlba en approche les axes, à 1e-4 écart-type près), et
# estim_ncpPCA(scale(), method.cv = "gcv")$ncp = 2
missmda_k2 <- c(-1.09850517903, 1.04203959962, -1.73667473728, -0.438784883781, 0.663782517243, -0.0286459197537, 0.203155717142,
                -0.540916303339, 2.30493561298, -0.914166498774, 2.41190510925, 0.570652419912, -0.95297584875, 0.78306643049,
                2.32153138477, -0.886453770496, 0.584939589276, 1.14015737256, -0.779022590792, -1.41463154333, -0.648719873629,
                -1.4168851179, 0.113247444619, -0.855629269944, 0.556388291345, -1.01725244041, -0.629672060415, 1.31277881186,
                -0.220629888486, -0.0204866993164, 0.152427337166, 0.743183162362, -0.485333815705, -0.123861140842, 0.00251778628088,
                0.0892256365373, 0.45814831693, -0.179704318574, 0.0939651735591, -0.0735342424854, 0.00522104634458, 0.312763361681,
                0.531569437827, 0.965269128194, 1.15244835102, 0.767184542815, 0.284176867314)
missmda_k5 <- c(-1.12796455212, 1.07927088679, -1.75942242282, -0.431529947865, 0.775702367417, -0.0529214947845, 0.182114337709,
                -0.561827231334, 2.54093418285, -0.833614672184, 2.61410366385, 0.845555081331, -1.14702157319, 1.00849467597,
                2.46840004897, -0.94800899879, 0.465373713578, 0.996247063975, -0.494338974798, -1.66761875381, -1.05832269284,
                -1.58882196161, -0.123239027108, -0.678874525175, 0.472324334004, -1.14706277089, -0.589011765487, 1.36753981623,
                -0.442065499629, -0.15134121905, -0.0385084137836, 0.673409564667, -0.914683270068, -0.100853649257, -0.0539450398052,
                0.115293616534, 0.320892251987, -0.13324437654, -0.281450863166, -0.00315337399132, 0.172338171825, 0.336642830671,
                0.481419250049, 0.771627313328, 1.07302143933, 0.697801557394, 0.392247443088)
manques_i <- is.na(Xs[garde, ])
pi1 <- imputer_acp(pim, vars_i, k = 2, bornes = FALSE)
got <- imputees(pi1)
verifier(sprintf("imputation = missMDA::imputePCA, 2 et 5 composantes (%d valeurs, %d tours) ; les valeurs observées restent",
                 attr(pi1, "imputation")$valeurs, attr(pi1, "imputation")$tours),
         proche(got[garde, ][manques_i], missmda_k2, 1e-8) && proche(imputees(imputer_acp(pim, vars_i, k = 5, bornes = FALSE))[garde, ][manques_i], missmda_k5, 1e-8) &&
           proche(got[!is.na(Xs)], Xs[!is.na(Xs)]) && attr(pi1, "imputation")$valeurs == sum(manques_i))
pa <- imputer_acp(pim, vars_i, bornes = FALSE)
verifier("sans nombre de composantes : 2, choisies comme missMDA::estim_ncpPCA des variables réduites (validation croisée généralisée)",
         attr(pa, "imputation")$composantes == 2 && isTRUE(attr(pa, "imputation")$choisies) && !isTRUE(attr(pi1, "imputation")$choisies) &&
           proche(imputees(pa)[garde, ][manques_i], missmda_k2, 1e-8))
# (les variables de l'exemple, d'unités diverses : estim_ncpPCA(scale(X)) en choisit 5 ;
# estim_ncpPCA(X), son critère dans leurs unités, 0, la moyenne de chaque variable)
quantitatives <- Filter(function(v) !length(v$categories), p$variables)
Xv <- sapply(quantitatives, function(v) as.numeric(colonne(p, as.numeric(v$uid), obs)))
verifier("les variables de l'exemple, d'unités diverses : 5 composantes, comme estim_ncpPCA(scale(X)) (0 dans leurs unités)",
         composantes_imputation(Xv[rowMeans(is.na(Xv)) <= 0.5, ]) == 5)
# (15 observations, 6 variables, 5 composantes : sans la borne de missMDA, le bruit estimé dépasse
# la 5e valeur propre et retourne cette composante ; imputePCA(, ncp = 5)$completeObs aux manques)
set.seed(1)
Xn <- matrix(rnorm(15 * 2), 15, 2) %*% matrix(runif(12, -1, 1), 2, 6) + matrix(rnorm(15 * 6, sd = 0.6), 15, 6)
Xn[matrix(runif(15 * 6) < 0.1, 15, 6)] <- NA
missmda_n <- c(0.935854308735, 0.655633624315, 0.897695637794, -1.36113634282, 0.481978961689, 1.18750581482, 0.483075381276,
               0.0385306604179, 0.794020054969, -0.161098049444)
pn <- projet_depuis_tableau(data.frame(nom = sprintf("N%02d", 1:15), Xn), "imputation")
pn <- imputer_acp(pn, numeros(pn, paste0("X", 1:6)), k = 5, bornes = FALSE, remplacer = TRUE)
xn <- sapply(paste0("X", 1:6), function(v) colonne(pn, numeros(pn, v), numeros_observations(pn)))
verifier("autant de composantes ou presque que de variables : le bruit borné comme dans missMDA", proche(xn[is.na(Xn)], missmda_n, 1e-8))
verifier("une observation à qui manquent plus de la moitié des variables reste manquante", all(is.na(got[3, 1:4])) && attr(pi1, "imputation")$laissees == 1)
pi2 <- imputer_acp(pim, vars_i, k = 5)
x6 <- imputees(pi2)[, 6]
verifier("bornées : une variable 0/1 imputée reste entre 0 et 1", all(x6[!is.na(x6)] >= 0 & x6[!is.na(x6)] <= 1) && any(x6[garde & is.na(Xs[, 6])] %% 1 != 0))
pi3 <- imputer_acp(pim, vars_i, k = 2, remplacer = TRUE)
verifier("remplir les variables elles-mêmes : aucune nouvelle variable, plus de manques (sauf l'observation trop incomplète)",
         length(pi3$variables) == length(pim$variables) && !anyNA(imputees(pi3, "")[garde, ]))
pi4 <- imputer_acp(pi1, vars_i, k = 3, bornes = FALSE)
verifier("imputée à nouveau, avec d'autres réglages : les mêmes variables, mises à jour", length(pi4$variables) == length(pi1$variables) &&
           !proche(imputees(pi4)[garde, ], imputees(pi1)[garde, ], 1e-8))
verifier("une seule variable, ou une variable qualitative : refusé", grepl("deux variables", refus(imputer_acp(pim, vars_i[1]))) &&
           grepl("quantitatives", refus(imputer_acp(p, u("variete", "sucre")))))
pc <- projet_depuis_tableau(data.frame(nom = sprintf("I%02d", 1:ni), Xs, C = rnorm(ni), D = rnorm(ni)), "imputation")
pi5 <- imputer_acp(pc, numeros(pc, c(paste0("X", 1:6), "C")), k = 5)
verifier("une variable complète aide à imputer les autres, sans copie « (imputée) » ; rien à remplir : refusé",
         length(pi5$variables) == length(pc$variables) + 6 && !"C (imputée)" %in% unlist(pi5$noms_var) &&
           grepl("aucune valeur manquante", refus(imputer_acp(pc, numeros(pc, c("C", "D"))))) &&
           grepl("plus de la moitié", refus(imputer_acp(pi3, vars_i))))

cat("Texte des méthodes\n")
for (l in c("fr", "en")) {
  t3 <- texte_methodes(p, d3, m3, l, "essai")
  t1 <- texte_methodes(p, d, m, l, "essai")
  verifier(sprintf("modèle mixte et régression (%s) : 5 paragraphes, nlme et Nakagawa cités, AIC et Q² donnés", l),
           length(t3$paragraphes) == 5 && any(grepl("Pinheiro", t3$references)) && any(grepl("Nakagawa", t3$references)) &&
             any(grepl("AIC", t3$paragraphes)) && any(grepl("Q²", t1$paragraphes)) && !any(grepl("NA", c(t3$paragraphes, t1$paragraphes), fixed = TRUE)) &&
             !any(grepl("nlme", t1$references)))
}

for (l in c("fr", "en")) {
  tg6 <- texte_methodes(pg, dg6, mg6, l, "essai")
  verifier(sprintf("GAMM (%s) : loi, courbe, effort et site aléatoire décrits, mgcv et Wood cités", l),
           any(grepl("binomial", tg6$paragraphes)) && any(grepl("REML", tg6$paragraphes)) && any(grepl("log\\(", tg6$paragraphes)) &&
             any(grepl("mgcv", tg6$references)) && any(grepl("Wood, S. N. \\(2011\\)", tg6$references)) && !any(grepl("NA", tg6$paragraphes, fixed = TRUE)))
}

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
