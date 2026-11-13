# Vérifie les régressions (moteur/R/regression.R) contre les fonctions de R elles-mêmes
# (lm, nlme::lme) : coefficients, AIC en ML, R², validation croisée par groupes, jeu de test,
# apport des blocs, autocorrélation dans le temps, sélection par l'AIC (chaque combinaison contre
# lm, nlme::lme avec ou sans CAR1, mgcv::gam et mgcv::gamm en ML ; l'élimination descendante ; le
# modèle choisi), Ajuster avec CAR1 (contre nlme::lme et mgcv::gamm avec corCAR1), les résidus
# quantiles randomisés (refaits avec les fonctions de répartition de R d'après glm() et mgcv::gam(),
# et leur graine) ; et que les définitions mal formées (un fichier reçu) sont refusées, qu'une
# définition enregistrée se relit.
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

cat("Résidus quantiles randomisés (Dunn et Smyth, 1996)\n")
# (refaits ici d'après R : u tiré entre F(y − 1) et F(y) avec la même graine, F la loi ajustée par
# glm(), MASS::glm.nb() ou mgcv::gam(), puis r = qnorm(u))
tirer_entre <- function(a, b) {
  set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  stats::runif(length(a), a, b)
}
ref_p <- glm(compte ~ x1 + x2, family = poisson, data = tg)
u_p <- tirer_entre(ppois(tg$compte - 1, fitted(ref_p)), ppois(tg$compte, fitted(ref_p)))
rq_p <- mg1$resultat$residus_quantiles
# (des comptages surdispersés : loin dans la queue de droite, qnorm(u) de R perd ses chiffres, que
# Nuée garde en calculant 1 − u ; les probabilités se comparent partout, les résidus sous 0,5)
verifier(sprintf("Poisson = glm() : u entre ppois(y − 1) et ppois(y), r = qnorm(u) ; sur des comptages surdispersés, un écart-type de %.1f", stats::sd(rq_p)),
         proche(pnorm(rq_p), u_p, 1e-6) && proche(rq_p[u_p <= 0.5], qnorm(u_p[u_p <= 0.5]), 1e-6) && all(is.finite(rq_p)) && stats::sd(rq_p) > 1.5)
# (la binomiale négative de mgcv et son θ : celle que Nuée ajuste, comparée plus haut à MASS::glm.nb())
a3 <- mg3$ajustement
rq_nb <- mg3$resultat$residus_quantiles
verifier(sprintf("binomiale négative : pnbinom() de son θ (%.2f) à ses valeurs ajustées ; des comptages de cette loi, des résidus presque normaux (moyenne %.2f, écart-type %.2f)",
                 a3$family$getTheta(TRUE), mean(rq_nb), stats::sd(rq_nb)),
         proche(rq_nb, qnorm(tirer_entre(pnbinom(tg$compte - 1, size = a3$family$getTheta(TRUE), mu = fitted(a3)), pnbinom(tg$compte, size = a3$family$getTheta(TRUE), mu = fitted(a3)))), 1e-10) &&
           abs(mean(rq_nb)) < 0.2 && abs(stats::sd(rq_nb) - 1) < 0.15)
theta6 <- refr$family$getTheta(TRUE)
verifier("GAMM binomial négatif (courbe, site aléatoire, effort) : la loi à la valeur ajustée, le site et l'effort compris, = mgcv::gam() et son θ",
         proche(mg6$resultat$residus_quantiles, qnorm(tirer_entre(pnbinom(tg$compte - 1, size = theta6, mu = fitted(refr)), pnbinom(tg$compte, size = theta6, mu = fitted(refr)))), 1e-4))
ref_b <- glm(presence ~ x2, family = binomial, data = tg)
verifier("binomiale 0/1 = glm() : une épreuve, u entre 1 − p et 1 pour une présence, entre 0 et 1 − p pour une absence",
         proche(mg4$resultat$residus_quantiles, qnorm(tirer_entre(pbinom(tg$presence - 1, 1, fitted(ref_b)), pbinom(tg$presence, 1, fitted(ref_b)))), 1e-6))
a5 <- mg5$ajustement
verifier("gamma : u = pgamma(y) de la loi ajustée (sa moyenne, sa forme 1 / l'échelle estimée), sans tirage",
         proche(mg5$resultat$residus_quantiles, qnorm(pgamma(tg$masse, shape = 1 / a5$sig2, rate = 1 / (a5$sig2 * fitted(a5)))), 1e-10))
verifier("ni pour la loi normale (ses résidus standardisés le sont déjà), ni pour Tweedie, ni pour une régression linéaire",
         is.null(modele_g("GLM", "masse", "x2")$resultat$residus_quantiles) && is.null(mg7$resultat$residus_quantiles) && is.null(mg8$resultat$residus_quantiles))
# (un comptage loin au-dessus de sa moyenne : ppois(y − 1) y vaut 1 en double précision, et qnorm(u) donnerait Inf)
pq <- projet_depuis_tableau(data.frame(nom = sprintf("Q%02d", 1:31), x = c(seq(0, 1, length.out = 30), 0.5), compte = c(rep(1:3, 10), 45)), "queue")
mq <- ajuster(pq, definir_modele(pq, "GLM", x = numeros(pq, "x"), y = numeros(pq, "compte"), echelle = "none", famille = "poisson"))
mu45 <- unname(fitted(mq$ajustement)[31])
r45 <- mq$resultat$residus_quantiles[31]
verifier(sprintf("un comptage de 45 pour une moyenne de %.1f : un résidu fini (%.1f), P(Y > 45) ≤ 1 − Φ(r) ≤ P(Y ≥ 45)", mu45, r45),
         ppois(44, mu45) == 1 && is.finite(r45) && pnorm(r45, lower.tail = FALSE) >= ppois(45, mu45, lower.tail = FALSE) * (1 - 1e-10) &&
           pnorm(r45, lower.tail = FALSE) <= ppois(44, mu45, lower.tail = FALSE) * (1 + 1e-10))
genre <- RNGkind()
RNGkind("L'Ecuyer-CMRG", "Box-Muller")
set.seed(11)
avant <- .Random.seed
rq1 <- residus_quantiles(mg1$ajustement, "poisson")
rq2 <- residus_quantiles(mg1$ajustement, "poisson")
verifier("la graine : les mêmes résidus à un second calcul et quel que soit le générateur de la session, rendu tel quel avec son état",
         identical(rq1, rq2) && identical(rq1, rq_p) && identical(RNGkind()[1:2], c("L'Ecuyer-CMRG", "Box-Muller")) && identical(.Random.seed, avant))
RNGkind(genre[1], genre[2], genre[3])
rm(".Random.seed", envir = globalenv())
mg1b <- modele_g("GLM", "compte", c("x1", "x2"), famille = "poisson")
verifier("une session qui n'a encore rien tiré au hasard : l'ajustement ne lui laisse pas de .Random.seed, et donne les mêmes résidus",
         !exists(".Random.seed", envir = globalenv(), inherits = FALSE) && identical(mg1b$resultat$residus_quantiles, rq_p))

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
# (un modèle mixte au site aléatoire, sans CAR1 : ses groupes, puis le secteur dans chacun)
mle_a <- ajuster(pa, definir_modele(pa, "LMM", x = ua("x", "xbar"), y = ua("y"), echelle = "none", aleatoires = list(par_variable("site"))))
r_lme_a <- nlme::lme(y ~ x + xbar, random = ~ 1 | site, data = ta, method = "REML", control = ctrl)
aef <- autocorrelation_residus(pa, mle_a, temps = par_variable("annee"), groupe = list(list(by = "effets"), par_variable("secteur")))
verifier("les groupes des effets aléatoires, puis un niveau plus fin (le secteur dans son site) : 21 séries, l'autocorrélation refaite à la main ; les effets seuls, refusés",
         aef$groupes == 21 && identical(aef$groupe, "site / secteur") &&
           proche(aef$series[[1]]$acf, acf_main(as.vector(residuals(r_lme_a, type = "pearson")), ta$annee, secteurs, 9), 1e-5) &&
           grepl("même temps", refus(autocorrelation_residus(pa, mle_a, temps = par_variable("annee"), groupe = list(by = "effets")))))
verifier("plusieurs observations au même temps dans un groupe, ou pas de temps : refusé, en disant pourquoi",
         grepl("même temps", refus(autocorrelation_residus(pac, ml_ac, temps = annee_ac))) && grepl("temps", refus(autocorrelation_residus(pac, ml_ac))))

cat("Ajuster avec CAR1 (Autocorrélation des résidus)\n")
# la définition du modèle sans CAR1, avec l'autocorrélation le long du temps choisi dans les groupes
# de ses effets aléatoires (regression.R : definition_car1), ajustée comme nlme::lme() et
# mgcv::gamm() l'ajustent avec corCAR1
d0_ac <- definir_modele(pac, "LMM", x = uac("x", "z"), y = uac("y"), echelle = "none", aleatoires = list(site_ac))
dcar_ac <- definition_car1(pac, d0_ac, annee_ac, list(list(by = "effets")))
mcar_ac <- ajuster(pac, dcar_ac)
mlcar_ac <- nlme::lme(y ~ x + z, random = ~ 1 | site, correlation = nlme::corCAR1(form = ~ annee | site), data = tac, method = "ML", control = ctrl)
verifier(sprintf("modèle mixte : la même définition, CAR1 dans chaque site en plus ; = nlme::lme(correlation = corCAR1(form = ~ annee | site)) (φ = %.3f), dans l'ensemble AIC du modèle sans CAR1",
                 mcar_ac$resultat$phi),
         identical(dcar_ac$correlation, list(type = "CAR1", temps = annee_ac)) && identical(dcar_ac[names(dcar_ac) != "correlation"], d0_ac) &&
           proche(mcar_ac$resultat$coefficients$estimation, nlme::fixef(rc_ac), 1e-5) &&
           proche(mcar_ac$resultat$phi, coef(rc_ac$modelStruct$corStruct, unconstrained = FALSE), 1e-4) && proche(mcar_ac$resultat$AIC, AIC(mlcar_ac), 1e-6) &&
           identical(mcar_ac$signature, m0_ac$signature))
dle_a <- definir_modele(pa, "LMM", x = ua("x", "xbar"), y = ua("y"), echelle = "none", aleatoires = list(par_variable("site")))
dsec_a <- definition_car1(pa, dle_a, par_variable("annee"), list(list(by = "effets"), par_variable("secteur")))
msec_a <- ajuster(pa, dsec_a)
reml_sec <- nlme::lme(y ~ x + xbar, random = ~ 1 | site_f, correlation = cor_a, data = ta, method = "REML", control = ctrl)
verifier("un niveau plus fin, le secteur dans son site (dans) : = nlme::lme(correlation = corCAR1(form = ~ annee | site/secteur)), dans l'ensemble AIC du modèle sans CAR1",
         identical(dsec_a$correlation, car1_a) && proche(msec_a$resultat$coefficients$estimation, nlme::fixef(reml_sec), 1e-5) &&
           proche(msec_a$resultat$phi, coef(reml_sec$modelStruct$corStruct, unconstrained = FALSE), 1e-4) && proche(msec_a$resultat$AIC, AIC(ml_ref), 1e-6) &&
           identical(msec_a$signature, mle_a$signature))
da0 <- definir_modele(pa, "GAM", x = ua("x", "xbar"), y = ua("y"), echelle = "none", famille = "gaussienne", lisses = ua("x"),
                      k_courbes = stats::setNames(list(5), ua("x")), aleatoires = list(par_variable("site")))
mgam_a <- ajuster(pa, definition_car1(pa, da0, par_variable("annee"), list(list(by = "effets"), par_variable("secteur"))))
verifier("un GAM à loi normale et au site aléatoire : mgcv::gamm, = mgcv::gamm(correlation = corCAR1(form = ~ annee | site/secteur)) ; un autre ensemble AIC que le GAM (mgcv::gam)",
         isTRUE(mgam_a$gamm) && proche(mgam_a$resultat$coefficients$estimation, summary(ref_reml$gam)$p.table[, 1], 1e-5) &&
           proche(mgam_a$resultat$phi, unname(stats::coef(ref_reml$lme$modelStruct$corStruct, unconstrained = FALSE)), 1e-4) &&
           proche(mgam_a$resultat$AIC, stats::AIC(ref_ml$lme), 1e-6) && mgam_a$signature != ma0$signature)
car1_effets <- list(list(by = "effets"))
verifier("refusé, en disant pourquoi : sans effet aléatoire, une loi de Poisson, d'autres groupes que ceux des effets aléatoires (ou aucun), déjà une autocorrélation",
         grepl("pas d'effet aléatoire", refus(definition_car1(pac, definir_modele(pac, "LM", x = uac("x"), y = uac("y")), annee_ac, car1_effets))) &&
           grepl("loi normale", refus(definition_car1(pac, definir_modele(pac, "GLM", x = uac("x"), y = uac("compte"), famille = "poisson", aleatoires = list(site_ac)),
                                                      annee_ac, car1_effets))) &&
           grepl("choisissez ses effets aléatoires", refus(definition_car1(pac, d0_ac, annee_ac, list(site_ac)))) &&
           grepl("choisissez ses effets aléatoires", refus(definition_car1(pac, d0_ac, annee_ac, NULL))) &&
           grepl("déjà", refus(definition_car1(pac, dcar_ac, annee_ac, car1_effets))))
cat("Le temps en dates : l'autocorrélation des résidus et CAR1\n")
# les mêmes 8 sites, comptés chaque année un même jour de mai ou de juin (qui change d'une année à
# l'autre) : la date en texte, le jour d'abord (une variable qualitative : chaque date revient pour
# les 8 sites), et la même en années décimales refaite avec R (le jour de l'année de as.POSIXlt)
set.seed(13)
jours_an <- as.Date(sprintf("%d-05-01", 2001:2011)) + sample(0:45, 11, TRUE)
tdt <- tac
tdt$date <- format(jours_an[tac$annee - 2000], "%d/%m/%Y")
tdt$decimale <- tac$annee + as.POSIXlt(jours_an[tac$annee - 2000])$yday / ifelse(tac$annee %% 4 == 0, 366, 365)
pdt <- projet_depuis_tableau(tdt, "dates")
udt <- function(...) numeros(pdt, c(...))
temps_d <- list(by = "variable", variable = udt("date"))
temps_n <- list(by = "variable", variable = udt("decimale"))
site_d <- list(by = "variable", variable = udt("site"))
ml_d <- ajuster(pdt, definir_modele(pdt, "LM", x = udt("x", "z"), y = udt("y"), echelle = "none"))
ad_d <- autocorrelation_residus(pdt, ml_d, temps = temps_d, groupe = site_d)
ad_n <- autocorrelation_residus(pdt, ml_d, temps = temps_n, groupe = site_d)
verifier("sans CAR1, le temps en dates (une variable qualitative) : la même autocorrélation qu'en années décimales, et qu'en années (le même ordre dans chaque site)",
         length(variable_du_projet(pdt, udt("date"))$categories) == 10 && proche(ad_d$series[[1]]$acf, ad_n$series[[1]]$acf, 1e-12) &&
           identical(ad_d$paires, ad_n$paires) && ad_d$n == 80 && ad_d$temps == "date" && proche(ad_d$series[[1]]$acf, al$series[[1]]$acf, 1e-10))
car1_d <- function(tps) definir_modele(pdt, "LMM", x = udt("x", "z"), y = udt("y"), echelle = "none", aleatoires = list(site_d), correlation = list(type = "CAR1", temps = tps))
mc_d <- ajuster(pdt, car1_d(temps_d))
mc_n <- ajuster(pdt, car1_d(temps_n))
rc_d <- nlme::lme(y ~ x + z, random = ~ 1 | site, correlation = nlme::corCAR1(form = ~ decimale | site), data = tdt, method = "REML", control = ctrl)
rc_d_ml <- nlme::lme(y ~ x + z, random = ~ 1 | site, correlation = nlme::corCAR1(form = ~ decimale | site), data = tdt, method = "ML", control = ctrl)
verifier(sprintf("CAR1 le long des dates : coefficients, φ (%.3f par an), AIC et résidus normalisés = ceux des années décimales, et = nlme::lme(corCAR1(form = ~ decimale | site))", mc_d$resultat$phi),
         proche(mc_d$resultat$coefficients$estimation, mc_n$resultat$coefficients$estimation, 1e-10) && proche(mc_d$resultat$phi, mc_n$resultat$phi, 1e-10) &&
           proche(mc_d$resultat$AIC, mc_n$resultat$AIC, 1e-10) && proche(mc_d$resultat$residus, mc_n$resultat$residus, 1e-10) &&
           proche(mc_d$resultat$coefficients$estimation, nlme::fixef(rc_d), 1e-5) && proche(mc_d$resultat$phi, unname(stats::coef(rc_d$modelStruct$corStruct, unconstrained = FALSE)), 1e-4) &&
           proche(mc_d$resultat$AIC, AIC(rc_d_ml), 1e-6))
ac_d <- autocorrelation_residus(pdt, mc_d)
verifier("CAR1 le long des dates, l'autocorrélation de ses résidus et de ses résidus normalisés : celle des années décimales",
         isTRUE(ac_d$car1) && proche(ac_d$series[[1]]$acf, autocorrelation_residus(pdt, mc_n)$series[[1]]$acf, 1e-10) &&
           proche(ac_d$series[[2]]$acf, autocorrelation_residus(pdt, mc_n)$series[[2]]$acf, 1e-8))
pdi_t <- pdt
pdi_t$observation_id_names <- c("Observation", "jour")
for (i in seq_along(pdi_t$observations)) pdi_t$observations[[i]]$ids <- c(pdi_t$observations[[i]]$ids[1], format(jours_an[tac$annee[i] - 2000]))
verifier("CAR1 le long de dates ISO en identifiant des observations : le même modèle",
         proche(ajuster(pdi_t, definir_modele(pdi_t, "LMM", x = udt("x", "z"), y = udt("y"), echelle = "none", aleatoires = list(site_d),
                                              correlation = list(type = "CAR1", temps = list(by = "id", id = "jour"))))$resultat$AIC, mc_d$resultat$AIC, 1e-10))
dcar_d <- definition_car1(pdt, definir_modele(pdt, "LMM", x = udt("x", "z"), y = udt("y"), echelle = "none", aleatoires = list(site_d)),
                          temps_d, list(list(by = "effets")))
verifier("Ajuster avec CAR1 le long de dates : le même modèle que CAR1 défini sur ces dates",
         identical(dcar_d$correlation$temps, temps_d) && proche(ajuster(pdt, dcar_d)$resultat$AIC, mc_d$resultat$AIC, 1e-10))
# (Ajuster avec CAR1 d'un modèle mixte BACI : le long de son temps dans ses unités, le modèle BACI
# avec CAR1, qui en dérive ; le long d'un autre temps, plus de réglages BACI)
bq <- expand.grid(annee = 2001:2012, secteur = c("S1", "S2", "S3"), stringsAsFactors = FALSE)
set.seed(5)
bq$densite <- round(5 + rnorm(nrow(bq)), 3)
bq$autre_temps <- bq$annee * 2
bq <- data.frame(nom = paste0("q", seq_len(nrow(bq))), bq)
pbq <- projet_depuis_tableau(bq, "baci car1")
mbq <- modele_baci(pbq, numeros(pbq, "densite"), numeros(pbq, "annee"), numeros(pbq, "secteur"), "S1", c("S2", "S3"), 2007)
c1bq <- definition_car1(mbq$projet, mbq$definition, list(by = "variable", variable = numeros(pbq, "annee")), list(list(by = "effets")))
c2bq <- definition_car1(mbq$projet, mbq$definition, list(by = "variable", variable = numeros(pbq, "autre_temps")), list(list(by = "effets")))
verifier("Ajuster avec CAR1 d'un modèle BACI : le long de son temps, ses réglages avec CAR1, marqués dérivés ; le long d'un autre temps, plus de réglages BACI",
         isTRUE(c1bq$baci_origine$car1) && isTRUE(c1bq$baci_origine$derive) && is.null(c2bq$baci_origine))
# (un temps de nombres avec une valeur illisible : sa ligne laissée de côté, comme jusqu'à 0.9.5)
tnd <- tac
tnd$an <- as.character(tnd$annee)
tnd$an[7] <- "nd"
pnd <- projet_depuis_tableau(tnd, "temps nd")
dnd <- definir_modele(pnd, "LMM", x = numeros(pnd, c("x", "z")), y = numeros(pnd, "y"), echelle = "none",
                      aleatoires = list(list(by = "variable", variable = numeros(pnd, "site"))),
                      correlation = list(type = "CAR1", temps = list(by = "variable", variable = numeros(pnd, "an"))))
mnd <- ajuster(pnd, dnd)
and <- autocorrelation_residus(pnd, ajuster(pnd, definir_modele(pnd, "LM", x = numeros(pnd, c("x", "z")), y = numeros(pnd, "y"), echelle = "none")),
                               temps = list(by = "variable", variable = numeros(pnd, "an")), groupe = list(by = "variable", variable = numeros(pnd, "site")))
verifier("CAR1 et l'autocorrélation des résidus le long d'un temps de nombres où une valeur est illisible (« nd ») : sa ligne laissée de côté, comme jusqu'à 0.9.5",
         mnd$N == nrow(tnd) - 1 && length(mnd$exclues) == 1 && and$laissees == 1)
tdt2 <- tdt
tdt2$date[tdt2$date == tdt2$date[5]] <- "31/02/2005"
pdt2 <- projet_depuis_tableau(tdt2, "date impossible")
m_imp <- refus(ajuster(pdt2, definir_modele(pdt2, "LMM", x = numeros(pdt2, c("x", "z")), y = numeros(pdt2, "y"), echelle = "none",
                                            aleatoires = list(list(by = "variable", variable = numeros(pdt2, "site"))),
                                            correlation = list(type = "CAR1", temps = list(by = "variable", variable = numeros(pdt2, "date"))))))
verifier(sprintf("une date qui n'existe pas dans le temps de CAR1 ou de l'autocorrélation des résidus : refusée, nommée (« %s »)", m_imp),
         grepl("^Le temps \\(date\\) doit être fait de nombres .*« 31/02/2005 » n'est pas une date qui existe$", m_imp) &&
           grepl("« 31/02/2005 »", refus(autocorrelation_residus(pdt2, ml_d, temps = list(by = "variable", variable = numeros(pdt2, "date")))), fixed = TRUE))

cat("Tests du rapport de vraisemblance (regression_diagnostics.R)\n")
mx_ac <- regr_ac("LMM", "x", aleatoires = list(site_ac))
mn_ac <- regr_ac("LMM", NULL, aleatoires = list(site_ac))
mz_ac <- regr_ac("LMM", "z", aleatoires = list(site_ac))
rv <- rapports_vraisemblance(list(xz = m0_ac, x = mx_ac, nul = mn_ac, z = mz_ac, lm = ml_ac, car1 = mc_ac, glmm = mg_ac))
paire_rv <- function(r, g, p) which(r$paires$grand == g & r$paires$petit == p)
# (la formule écrite dans l'appel du modèle : anova() de nlme 3.1-155, celui d'Ubuntu 22.04, lit
# la réponse de chaque modèle dans son appel, où `f` ou `...` ne diraient rien)
ml_lme <- function(f, correlation = NULL) eval(bquote(nlme::lme(.(f), random = ~ 1 | site, data = tac, method = "ML", control = ctrl, correlation = .(correlation))))
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

cat("Sélection par AIC\n")
aicc_direct <- function(f, n) { k <- attr(stats::logLik(f), "df"); stats::AIC(f) + 2 * k * (k + 1) / (n - k - 1) }
contenus_sel <- function(s) lapply(seq_len(nrow(s$modeles)), function(i) as.character(s$groupes)[vapply(s$contient, `[`, 0L, i) == 1L])
cle_sel <- function(g) paste(sort(g), collapse = " + ")
# une régression linéaire : cinq variables (une qualitative) et un terme croisé, sur les observations complètes du modèle
dsel <- definir_modele(p, "LM", x = u("sucre", "fermete", "acidite", "polyphenols", "variete"), y = u("note_gout"), echelle = "none",
                       croises = list(u("sucre", "fermete")))
msel <- ajuster(p, dsel)
ssel <- selection_aic(msel, dsel)
csel <- stats::complete.cases(df[c("note", "sucre", "fermete", "acidite", "polyphenols", "variete")])
morceaux <- c(sucre = "sucre", fermete = "fermete", acidite = "acidite", polyphenols = "polyphenols", variete = "variete", "sucre*fermete" = "I(sucre * fermete)")
permises <- Filter(function(g) !"sucre*fermete" %in% g || all(c("sucre", "fermete") %in% g),
                   lapply(0:63, function(b) names(morceaux)[bitwAnd(b, 2^(0:5)) > 0]))
gsel <- contenus_sel(ssel)
aicc_lm <- vapply(gsel, function(g) aicc_direct(lm(reformulate(if (length(g)) morceaux[g] else "1", "note"), data = df[csel, ]), sum(csel)), 0)
verifier(sprintf("lm : les %d combinaisons permises des 6 groupes (le terme croisé avec ses deux effets principaux), chacune = lm() et son AICc", length(permises)),
         ssel$methode == "toutes" && nrow(ssel$modeles) == length(permises) && setequal(vapply(gsel, cle_sel, ""), vapply(permises, cle_sel, "")) &&
           proche(ssel$modeles$AICc, aicc_lm, 1e-8) && ssel$N == sum(csel) && sum(csel) < length(obs) && !is.unsorted(ssel$modeles$AICc))
dl_aic <- aicc_lm - min(aicc_lm)
w_aic <- exp(-dl_aic / 2) / sum(exp(-dl_aic / 2))
imp_aic <- vapply(names(morceaux), function(g) sum(w_aic[vapply(gsel, function(x) g %in% x, TRUE)]), 0)
verifier("ΔAICc, poids d'Akaike et leur cumul, l'importance de chaque groupe (la somme des poids des modèles qui le contiennent), recalculés",
         proche(ssel$modeles$delta, dl_aic) && proche(ssel$modeles$poids, w_aic) && proche(ssel$modeles$poids_cumule, cumsum(w_aic)) &&
           proche(ssel$importance$importance, imp_aic[ssel$importance$groupe]) && ssel$meilleur == which.min(aicc_lm))
set.seed(8)
tp <- data.frame(nom = sprintf("P%02d", 1:50), x1 = rnorm(50), x2 = rnorm(50), x3 = rnorm(50))
tp$y <- 1 + 0.6 * tp$x1 + 0.2 * tp$x2 + rnorm(50)
pp <- projet_depuis_tableau(tp, "parcimonie")
dpar <- definir_modele(pp, "LM", x = numeros(pp, c("x1", "x2", "x3")), y = numeros(pp, "y"), echelle = "none")
spar <- selection_aic(ajuster(pp, dpar), dpar)
gpar <- contenus_sel(spar)
proches <- which(spar$modeles$delta <= 2)
verifier(sprintf("le plus parcimonieux à ΔAICc ≤ 2 : le moins de paramètres (%s, ΔAICc %.2f), quand le meilleur en a plus (%s)",
                 cle_sel(gpar[[spar$parcimonieux]]), spar$modeles$delta[spar$parcimonieux], cle_sel(gpar[[1]])),
         spar$parcimonieux == proches[order(spar$modeles$k[proches], spar$modeles$delta[proches])][1] && spar$parcimonieux != 1 &&
           identical(gpar[[spar$parcimonieux]], "x1") && identical(gpar[[1]], c("x1", "x2")))
dsel3 <- definir_modele(p, "LMM", x = u("sucre", "fermete", "acidite"), y = u("note_gout"), echelle = "none", aleatoires = list(list(by = "variable", variable = u("verger"))))
ssel3 <- selection_aic(ajuster(p, dsel3), dsel3)
c3 <- stats::complete.cases(df[c("note", "sucre", "fermete", "acidite")])
aicc_lme <- vapply(contenus_sel(ssel3), function(g) aicc_direct(nlme::lme(reformulate(if (length(g)) g else "1", "note"), random = ~ 1 | verger, data = df[c3, ],
                                                                          method = "ML", control = ctrl), sum(c3)), 0)
verifier("modèle mixte : les 8 combinaisons = nlme::lme(method = \"ML\"), l'effet aléatoire gardé", nrow(ssel3$modeles) == 8 && proche(ssel3$modeles$AICc, aicc_lme, 1e-6))
msel_l <- ajuster(pa, dl)
ssel_l <- selection_aic(msel_l, dl)
aicc_car1 <- vapply(contenus_sel(ssel_l), function(g) aicc_direct(nlme::lme(reformulate(if (length(g)) g else "1", "y"), random = ~ 1 | site_f, correlation = cor_a,
                                                                            data = ta, method = "ML"), nrow(ta)), 0)
verifier("avec l'autocorrélation CAR1 dans chaque secteur : = nlme::lme(method = \"ML\") avec la même corCAR1", nrow(ssel_l$modeles) == 4 && proche(ssel_l$modeles$AICc, aicc_car1, 1e-6))
sma <- selection_aic(ma, da)
aicc_gamm <- vapply(contenus_sel(sma), function(g) {
  droite <- c(if ("x" %in% g) "s(x, k = 5)", if ("xbar" %in% g) "xbar")
  r <- mgcv::gamm(stats::as.formula(paste("y ~", if (length(droite)) paste(droite, collapse = " + ") else "1"), env = new.env(parent = asNamespace("mgcv"))),
                  random = list(site_f = ~1), correlation = cor_a, data = ta, method = "ML")
  aicc_direct(r$lme, nrow(ta))
}, 0)
verifier("mgcv::gamm avec CAR1 : chaque combinaison = mgcv::gamm(method = \"ML\"), la courbe gardant sa dimension (k = 5)", proche(sma$modeles$AICc, aicc_gamm, 1e-6))
dsel6 <- definir_modele(pg, "GAM", x = ug("x1", "x2"), y = ug("compte"), echelle = "none", famille = "binomiale_negative", lisses = ug("x1"), decalage = ug("effort"),
                        aleatoires = list(list(by = "variable", variable = ug("site"))))
ssel6 <- selection_aic(mg6, dsel6)
tgf <- transform(tg, site = factor(site))
aicc_gam <- vapply(contenus_sel(ssel6), function(g) {
  droite <- c(if ("x2" %in% g) "x2", if ("x1" %in% g) "s(x1, k = 10)", "s(site, bs = \"re\")")
  aicc_direct(mgcv::gam(stats::as.formula(paste("compte ~", paste(droite, collapse = " + ")), env = env), data = tgf, family = mgcv::nb(), method = "ML",
                        offset = log(effort)), nrow(tg))
}, 0)
verifier("GAMM binomial négatif (courbe, site aléatoire, effort) : chaque combinaison = mgcv::gam(method = \"ML\")", nrow(ssel6$modeles) == 4 && proche(ssel6$modeles$AICc, aicc_gam, 1e-5))
# au-delà de 8 groupes : l'élimination descendante, refaite ici avec lm() (un effet principal reste tant que son terme croisé est là)
set.seed(31)
tb <- data.frame(nom = sprintf("B%02d", 1:80), matrix(rnorm(800), 80, 10, dimnames = list(NULL, paste0("x", 1:10))))
tb$y <- 1 + 0.8 * tb$x1 - 0.5 * tb$x2 + 0.3 * tb$x3 + rnorm(80)
pb <- projet_depuis_tableau(tb, "descendante")
db <- definir_modele(pb, "LM", x = numeros(pb, paste0("x", 1:10)), y = numeros(pb, "y"), echelle = "none", croises = list(numeros(pb, c("x1", "x2"))))
sb <- selection_aic(ajuster(pb, db), db)
morceaux_b <- c(stats::setNames(paste0("x", 1:10), paste0("x", 1:10)), "x1*x2" = "I(x1 * x2)")
aicc_b <- function(v) aicc_direct(lm(reformulate(if (length(v)) morceaux_b[v] else "1", "y"), data = tb), 80)
courant <- names(morceaux_b)
a_b <- aicc_b(courant)
retires_b <- character(0)
essayes_b <- 1
repeat {
  retirables_b <- setdiff(courant, if ("x1*x2" %in% courant) c("x1", "x2"))
  if (!length(retirables_b)) break
  essais_b <- vapply(retirables_b, function(g) aicc_b(setdiff(courant, g)), 0)
  essayes_b <- essayes_b + length(essais_b)
  if (min(essais_b) >= a_b) break
  retires_b <- c(retires_b, names(which.min(essais_b)))
  courant <- setdiff(courant, names(which.min(essais_b)))
  a_b <- min(essais_b)
}
gb <- contenus_sel(sb)
verifier(sprintf("11 groupes (plus de 256 combinaisons) : élimination descendante = celle faite avec lm() (retirés : %s ; %d modèles essayés)", paste(retires_b, collapse = ", "), essayes_b),
         sb$methode == "descendante" && identical(sb$chemin$retire[-1], retires_b) && setequal(gb[[1]], courant) && proche(sb$modeles$AICc[1], a_b, 1e-8) &&
           nrow(sb$modeles) == essayes_b && all(vapply(gb, function(g) !"x1*x2" %in% g || all(c("x1", "x2") %in% g), TRUE)) &&
           proche(sb$importance$importance, vapply(as.character(sb$groupes), function(g) sum(sb$modeles$poids[vapply(gb, function(x) g %in% x, TRUE)]), 0)))
# Ajuster ce modèle : la définition du modèle choisi
dch <- definition_selection(dsel, msel, c("sucre", "sucre*fermete"))
verifier("Ajuster ce modèle : les seules variables des groupes choisis, un terme croisé gardant ses facteurs ; les autres réglages restent",
         identical(vapply(dch$x, function(t) t$name, ""), c("sucre", "fermete", "sucre*fermete")) && identical(dch[c("type", "y", "cv_groups", "title")], dsel[c("type", "y", "cv_groups", "title")]))
isf <- match("fermete + sucre", vapply(gsel, cle_sel, ""))
mch <- ajuster(p, definition_selection(dsel, msel, c("sucre", "fermete")))
verifier(sprintf("ajusté, le modèle choisi a l'AICc de sa ligne (%.2f), sur les observations de la sélection (%d : les valeurs manquantes d'une variable retirée n'en ajoutent pas)",
                 mch$resultat$AICc, mch$N),
         proche(mch$resultat$AICc, ssel$modeles$AICc[isf], 1e-8) && mch$N == msel$N && identical(sort(mch$observations), sort(msel$observations)) &&
           identical(mch$resultat$coefficients$terme, c("(Intercept)", "sucre", "fermete")))
# (les mêmes groupes de validation croisée « à tour de rôle » : la liste des observations ne change
# pas, celles où manque une variable retirée restent de côté faute de valeurs, et le texte le dit)
set.seed(2)
dv <- data.frame(nom = sprintf("o%02d", 1:60), x1 = rnorm(60), x2 = rnorm(60), x3 = rnorm(60))
dv$y <- 1 + dv$x1 + 0.5 * dv$x2 + rnorm(60)
dv$x3[c(4, 11, 17, 30, 41)] <- NA
pvc <- projet_depuis_tableau(dv, "vc")
dvc <- definir_modele(pvc, "LM", x = numeros(pvc, c("x1", "x2", "x3")), y = numeros(pvc, "y"), echelle = "none")
mvc <- ajuster(pvc, dvc)
svc <- selection_aic(mvc, dvc)
mtous <- ajuster(pvc, definition_selection(dvc, mvc, as.character(svc$groupes)))
dx1 <- definition_selection(dvc, mvc, c("x1", "x2"))
mx1 <- ajuster(pvc, dx1)
tx1 <- paste(texte_methodes(pvc, dx1, mx1, "fr", "essai")$paragraphes, collapse = " ")
verifier(sprintf("Ajuster ce modèle : les mêmes groupes de validation croisée que le modèle complet (Q2 %.4f, le même) ; 5 observations sans x3 laissées de côté faute de valeurs, dites dans le texte",
                 mtous$vc$Q2),
         proche(mtous$vc$Q2, mvc$vc$Q2, 1e-12) && identical(dx1$observations, dvc$observations) && length(mx1$exclues) == 5 && mx1$N == 55 &&
           grepl("5 autres", tx1) && grepl("sélection par l'AICc du modèle", tx1))
# (un log10 que des comptages nuls mettent hors de son domaine : le modèle complet laisse ces lignes
# de côté ; le modèle choisi sans ce terme aussi, et son AICc est celui de sa ligne de la sélection)
set.seed(31)
dz <- data.frame(nom = sprintf("z%02d", 1:60), x1 = rnorm(60), x2 = rnorm(60), compte = stats::rpois(60, 1.5))
dz$y <- 1 + dz$x1 + 0.4 * dz$x2 + rnorm(60)
pz <- projet_depuis_tableau(dz, "zéros")
uz <- function(...) numeros(pz, c(...))
complet_z <- function(...) definir_modele(pz, "LM", x = uz("x1", "x2", "compte"), y = uz("y"), echelle = "none", nom = "M1",
                                          transformations = stats::setNames(list("log10"), uz("compte")), ...)
dzc <- complet_z()
mzc <- ajuster(pz, dzc)
szc <- selection_aic(mzc, dzc)
dz1 <- definition_selection(dzc, mzc, c("x1", "x2"))
mz1 <- ajuster(pz, dz1)
zeros <- sum(dz$compte == 0)
tz1 <- paste(texte_methodes(pz, dz1, mz1, "fr", "essai")$paragraphes, collapse = " ")
dz2 <- dz1
dz2$observations <- setdiff(dz2$observations, mz1$observations[1:2])  # (deux observations exclues ensuite)
tz2 <- paste(texte_methodes(pz, dz2, ajuster(pz, dz2), "fr", "essai")$paragraphes, collapse = " ")
verifier(sprintf("Ajuster ce modèle sans un log10 que %d comptages nuls mettent hors de son domaine : les %d lignes du modèle complet, l'AICc de sa ligne de la sélection ; « les mêmes observations » tant qu'aucune n'est exclue",
                 zeros, mzc$N),
         zeros > 0 && mzc$N == 60 - zeros && identical(mz1$observations, mzc$observations) &&
           proche(mz1$resultat$AICc, szc$modeles$AICc[match("x1 + x2", vapply(contenus_sel(szc), cle_sel, ""))], 1e-8) &&
           grepl("du modèle .{1,2}M1.{1,2} a retenus", tz1) && grepl("sur les mêmes observations que lui", tz1) &&
           grepl("a retenus", tz2) && !grepl("mêmes observations", tz2))
test_z <- numeros_observations(pz)[51:60]
mzt <- ajuster(pz, complet_z(test = test_z))
mzt1 <- ajuster(pz, definition_selection(complet_z(test = test_z), mzt, c("x1", "x2")))
nuls_test <- sum(colonne(pz, uz("compte"), test_z) == 0)
verifier(sprintf("un jeu de test garde ses observations : le modèle choisi prédit aussi les %d au comptage nul", nuls_test),
         nuls_test > 0 && mzt1$test$statistiques$n == 10 && mzt$test$statistiques$n == 10 - nuls_test && identical(mzt1$observations, mzt$observations))
dch6 <- definition_selection(dsel6, mg6, "x2")
mch6 <- ajuster(pg, dch6)
dcha <- definition_selection(da, ma, "x")
verifier("sans la variable en courbe, ni courbe ni dimension ; l'effort, le site aléatoire, l'autocorrélation et la dimension d'une courbe gardée restent",
         is.null(dch6$lisses) && is.null(dch6$k_courbes) && identical(dch6$decalage, dsel6$decalage) && identical(dch6$aleatoires, dsel6$aleatoires) &&
           proche(mch6$resultat$AICc, ssel6$modeles$AICc[match("x2", vapply(contenus_sel(ssel6), cle_sel, ""))], 1e-6) &&
           identical(dcha$lisses, da$lisses) && identical(dcha$k_courbes, da$k_courbes) && identical(dcha$correlation, da$correlation) && ajuster(pa, dcha)$k[[1]] == 5)
verifier("Ajuster ce modèle : le modèle nul, et un groupe inconnu refusé",
         !length(definition_selection(dsel, msel, character(0))$x) && grepl("inconnu", refus(definition_selection(dsel, msel, "absent"))))
msel$selection_aic <- ssel
mbs <- ajuster(pb, db)
mbs$selection_aic <- sb
for (langue_s in c("fr", "en")) {
  tsel <- texte_methodes(p, dsel, msel, langue_s, "essai")
  tdes <- texte_methodes(pb, db, mbs, langue_s, "essai")$paragraphes[2]
  verifier(sprintf("texte des méthodes (%s) : la sélection (40 combinaisons, ML, AICc), le meilleur modèle, l'importance des groupes, Burnham et Anderson cités ; l'élimination descendante", langue_s),
           grepl("AICc", tsel$paragraphes[2]) && grepl("40 ", tsel$paragraphes[2]) && grepl("(ML)", tsel$paragraphes[2], fixed = TRUE) &&
             grepl("importance", tsel$paragraphes[2]) && length(tsel$paragraphes) == 5 && any(grepl("Burnham", tsel$references)) &&
             grepl(if (langue_s == "fr") "élimination descendante" else "backward elimination", tdes) && !any(grepl("NA", c(tsel$paragraphes, tdes), fixed = TRUE)) &&
             !grepl("AICc", texte_methodes(p, dsel, ajuster(p, dsel), langue_s, "essai")$paragraphes[2]))
}
verifier("le modèle nul : aucun groupe à choisir", identical(selection_aic(m0, definir_modele(p, "LM", x = NULL, y = u("note_gout"), echelle = "none"))$K, 0L))

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
              "note_gout ~ s(sucre, bs = \"cr\")" = "dimension", "note_gout ~ s(sucre, k = 2)" = "entier de 3", "note_gout ~ sucre:log(fermete) + fermete" = "autre façon",
              "note_gout ~ inconnue" = "pas une variable", "note_gout ~ variete:sucre:variete" = "une fois", "note_gout ~ variete + I(variete^2)" = "une fois",
              "note_gout ~ sucre:note_gout" = "la réponse",
              "note_gout ~ sucre^2" = "I\\(x\\^2\\)", "variete ~ sucre" = "qualitative", "note_gout ~ sucre + log(sucre)" = "deux façons",
              "note_gout ~ s(variete)" = "quantitative", "note_gout ~ sucre +" = "illisible")
mauvais <- names(messages)[!vapply(names(messages), function(f) grepl(messages[[f]], refus(lire_formule(p, f))), TRUE)]
verifier("une formule que Nuée ne sait pas faire : refusée, en disant pourquoi", !length(mauvais))
if (length(mauvais)) cat("          ", mauvais, sep = "\n          ")
r_sf <- lire_formule(p, "note_gout ~ sucre:fermete")
verifier("a:b sans a ni b : le terme croisé seul, ses facteurs sans effet principal (= lm(y ~ sucre:fermete))",
         !length(r_sf$x) && proche(ajuster(p, reglees(p, r_sf))$resultat$AIC, AIC(lm(note ~ sucre:fermete, data = df))))

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
verifier(sprintf("le temps doit être fait de nombres ou de dates : « %s » ; le pas, positif", m_temps),
         startsWith(m_temps, "precedent() : le temps doit être fait de nombres") && grepl("« S1 » n'est ni un nombre ni une date", m_temps, fixed = TRUE) &&
           grepl("pas", refus(calcule(pl, "suivant(densite, annee, code, pas = 0)"))))
# (des dates : la même règle, le temps exactement un an avant en années décimales : le 1er janvier
# d'une année est l'année même ; le 15 mai 2004, d'une année bissextile, n'est pas 2003,367 + 1)
pl_dates <- projet_depuis_tableau(data.frame(nom = paste0("j", 1:4), secteur = "a", date = c("01/01/2003", "01/01/2004", "15/05/2003", "15/05/2004"),
                                             densite = c(2, 3, 4, 5)), "décalages en dates")
verifier("des dates : le temps exactement un pas avant, comme avec des années (le 1er janvier 2003 un an avant le 1er janvier 2004 ; pas le 15 mai 2003 avant le 15 mai 2004)",
         proche(calcule(pl_dates, "precedent(densite, date, secteur)"), c(NA, 2, NA, NA)))

cat("Indices de diversité (Nouvelle variable)\n")
# cinq relevés de quatre espèces, le site (qualitatif) devant elles : un relevé ordinaire (10, 5, 0,
# 1), un relevé vide, une valeur manquante, une seule espèce, quatre espèces à égalité ; les indices
# calculés ici à la main
dv <- data.frame(nom = paste0("r", 1:5), site = c("A", "A", "B", "B", "B"),
                 Ablette = c(10, 0, 3, 0, 2), Brochet = c(5, 0, NA, 7, 2), Gardon = c(0, 0, 2, 0, 2), Vandoise = c(1, 0, 1, 0, 2))
pdv <- projet_depuis_tableau(dv, "diversite")
H1 <- -(10 / 16 * log(10 / 16) + 5 / 16 * log(5 / 16) + 1 / 16 * log(1 / 16))
verifier(sprintf("richesse, shannon, simpson et pielou à la main : S = 3, H′ = %.4f, 1 − Σp² = 130/256, J = H′/ln 3 ; quatre à égalité : ln 4, 3/4 et 1", H1),
         proche(calcule(pdv, "richesse(Ablette, Brochet, Gardon, Vandoise)"), c(3, 0, NA, 1, 4)) &&
           proche(calcule(pdv, "shannon(Ablette, Brochet, Gardon, Vandoise)"), c(H1, 0, NA, 0, log(4)), 1e-12) &&
           proche(calcule(pdv, "simpson(Ablette, Brochet, Gardon, Vandoise)"), c(130 / 256, 0, NA, 0, 0.75), 1e-12) &&
           proche(calcule(pdv, "pielou(Ablette, Brochet, Gardon, Vandoise)"), c(H1 / log(3), NA, NA, NA, 1), 1e-12))
verifier("un relevé de zéros : S = 0, H′ = 0, Simpson = 0, pas de J ; une espèce seule : pas de J ; une valeur manquante : aucun indice",
         identical(vapply(c("richesse", "shannon", "simpson", "pielou"), function(f) calcule(pdv, sprintf("%s(Ablette:Vandoise)", f))[2], 0),
                   c(richesse = 0, shannon = 0, simpson = 0, pielou = NA_real_)) &&
           is.na(calcule(pdv, "pielou(Ablette:Vandoise)")[4]) && all(is.na(vapply(c("richesse", "shannon", "simpson", "pielou"), function(f) calcule(pdv, sprintf("%s(Ablette:Vandoise)", f))[3], 0))))
h_liste <- calcule(pdv, "shannon(Ablette, Brochet, Gardon, Vandoise)")
verifier("une plage dans l'ordre du tableau (Ablette:Vandoise, à l'envers aussi), une liste, les deux mêlées ; une colonne nommée deux fois compte une fois",
         proche(calcule(pdv, "shannon(Ablette:Vandoise)"), h_liste, 1e-14) && proche(calcule(pdv, "shannon(Vandoise:Ablette)"), h_liste, 1e-14) &&
           proche(calcule(pdv, "shannon(Ablette:Brochet, Gardon, Vandoise)"), h_liste, 1e-14) &&
           proche(calcule(pdv, "shannon(Ablette:Gardon, Brochet, Vandoise)"), h_liste, 1e-14) &&
           proche(calcule(pdv, "shannon(Brochet:Gardon)"), calcule(pdv, "shannon(Brochet, Gardon)"), 1e-14))
verifier("richness() : richesse() en anglais ; une expression compte comme une colonne de ses valeurs",
         proche(calcule(pdv, "richness(Ablette:Vandoise)"), c(3, 0, NA, 1, 4)) &&
           proche(calcule(pdv, "shannon(Ablette + Brochet, Gardon)"), c(0, 0, NA, 0, -(4 / 6 * log(4 / 6) + 2 / 6 * log(2 / 6))), 1e-12))
pneg <- pdv
pneg$donnees[4, as.character(numeros(pdv, "Gardon"))] <- -2
m_neg <- refus(calcule(pneg, "shannon(Ablette:Vandoise)"))
verifier(sprintf("refusés, en nommant la variable : une valeur négative (« %s »), une variable qualitative, une plage hors du tableau, aucune colonne ; le nom appelé dans le message",
                 m_neg),
         m_neg == "shannon() : Gardon a une valeur négative (-2) ; les indices de diversité demandent des abondances positives ou nulles" &&
           startsWith(refus(calcule(pdv, "simpson(Ablette, site)")), "simpson() : site n'est pas une variable quantitative") &&
           startsWith(refus(calcule(pdv, "pielou(Ablette:Truite)")), "pielou() : Truite n'est pas une colonne du tableau, pour la plage Ablette:Truite") &&
           startsWith(refus(calcule(pdv, "richness()")), "richness() : donner les colonnes des espèces") &&
           startsWith(refus(calcule(pdv, "richesse(Ablette, Truite)")), "Variable introuvable : Truite"))
# contre vegan::diversity et vegan::specnumber, s'il est installé (NUEE_VEGAN : une bibliothèque où il
# l'est, mise après les autres : nlme et mgcv restent ceux de R), sur 40 relevés de 8 espèces tirés
# au hasard (des absences, des relevés vides) ; vegan donne 1 au Simpson d'un relevé vide (1 moins
# une somme vide), Nuée 0, et ln 0 au dénominateur de J
if (nzchar(Sys.getenv("NUEE_VEGAN"))) .libPaths(c(.libPaths(), Sys.getenv("NUEE_VEGAN")))
if (requireNamespace("vegan", quietly = TRUE)) {
  set.seed(53)
  comptes <- matrix(rpois(40 * 8, rep(c(0.3, 1, 3, 8), each = 80)) * rbinom(320, 1, 0.6), 40, 8, dimnames = list(NULL, paste0("sp", 1:8)))
  comptes[c(5, 17), ] <- 0
  pc <- projet_depuis_tableau(data.frame(nom = paste0("c", 1:40), zone = rep(c("a", "b"), 20), comptes), "vegan")
  vide <- rowSums(comptes) == 0
  sv <- vegan::specnumber(comptes)
  hv <- vegan::diversity(comptes, "shannon")
  dvv <- vegan::diversity(comptes, "simpson")
  verifier(sprintf("contre vegan %s : richesse = specnumber(), shannon = diversity(), simpson = diversity(index = \"simpson\") (relevés vides : 0 contre 1), pielou = H′ / ln S quand S > 1",
                   as.character(utils::packageVersion("vegan"))),
           proche(calcule(pc, "richesse(sp1:sp8)"), sv, 1e-14) && proche(calcule(pc, "shannon(sp1:sp8)"), hv, 1e-12) &&
             proche(calcule(pc, "simpson(sp1:sp8)")[!vide], dvv[!vide], 1e-12) && all(calcule(pc, "simpson(sp1:sp8)")[vide] == 0) && all(dvv[vide] == 1) &&
             proche(calcule(pc, "pielou(sp1:sp8)")[sv > 1], (hv / log(sv))[sv > 1], 1e-12) && all(is.na(calcule(pc, "pielou(sp1:sp8)")[sv <= 1])))
} else {
  cat("  (vegan absent : les indices de diversité ne sont pas comparés à vegan ; NUEE_VEGAN=<bibliothèque> pour le faire)\n")
}

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

cat("Termes croisés avec une variable qualitative\n")
# six sites suivis dix ans (2005 manque), trois secteurs dans chacun (leurs noms se répètent d'un site
# à l'autre), deux habitats (trois sites chacun), trois traitements ; la pente de l'année change d'un
# secteur à l'autre, l'effet du traitement d'un habitat à l'autre. La catégorie de référence de Nuée
# est la dernière (la colonne que l'ajustement laisse de côté) : les facteurs de R ont la même en premier.
set.seed(21)
tq <- expand.grid(annee = c(2001:2004, 2006:2011), secteur = c("S1", "S2", "S3"), site = c("A", "B", "C", "D", "E", "F"), stringsAsFactors = FALSE)
nq <- nrow(tq)
tq$nom <- sprintf("Q%03d", seq_len(nq))
tq$habitat <- ifelse(tq$site %in% c("A", "B", "C"), "H1", "H2")
tq$traitement <- c("T1", "T2", "T3")[(seq_len(nq) %% 3) + 1]
tq$x <- round(runif(nq, 0, 10), 3)
tq$secteur_site <- paste0(tq$site, tq$secteur)  # (des secteurs propres à chaque site)
bruit_q <- numeric(nq)
for (i in split(seq_len(nq), paste(tq$site, tq$secteur))) bruit_q[i[order(tq$annee[i])]] <- as.vector(stats::filter(rnorm(length(i), 0, 0.5), 0.5, "recursive"))
tq$densite <- 3 + c(S1 = 0.05, S2 = 0.25, S3 = -0.1)[tq$secteur] * (tq$annee - 2000) + sin(tq$x / 2) + rnorm(6, 0, 0.6)[as.integer(factor(tq$site))] + bruit_q
tq$compte <- stats::rpois(nq, exp(0.4 + 0.5 * (tq$habitat == "H1") + 0.4 * (tq$traitement == "T2") * (tq$habitat == "H2") + 0.3 * sin(tq$x / 2)))
pq <- projet_depuis_tableau(tq[c("nom", "site", "secteur", "secteur_site", "habitat", "traitement", "annee", "x", "densite", "compte")], "croisés")
uq <- function(...) numeros(pq, c(...))
tq$secteur_f <- factor(tq$secteur, levels = c("S3", "S1", "S2"))
tq$habitat_f <- factor(tq$habitat, levels = c("H2", "H1"))
tq$traitement_f <- factor(tq$traitement, levels = c("T3", "T1", "T2"))
tq$site_f <- factor(tq$site)
site_q <- list(by = "variable", variable = uq("site"))
car1_q <- list(type = "CAR1", temps = list(by = "variable", variable = uq("annee")), dans = list(by = "variable", variable = uq("secteur")))
lm_q <- function(x, croises, ...) definir_modele(pq, "LM", x = uq(x), y = uq("densite"), echelle = "none", croises = lapply(croises, function(c) uq(c)), ...)
ajuste_q <- function(m, l, tol = 1e-8) proche(m$resultat$coefficients$estimation, coef(l), tol) && proche(m$resultat$coefficients$erreur_type, coef(summary(l))[, 2], tol) &&
  proche(m$resultat$logLik, as.numeric(logLik(l)), tol) && proche(m$resultat$AIC, AIC(l), tol) && proche(m$resultat$ajuste, fitted(l), tol)
dq1 <- lm_q(c("annee", "secteur"), list(c("annee", "secteur")))
mq1 <- ajuster(pq, dq1)
lq1 <- lm(densite ~ annee * secteur_f, data = tq)
verifier(sprintf("annee × secteur = lm(y ~ annee * secteur) : coefficients, erreurs-types, logLik, AIC, valeurs ajustées ; « %s » la catégorie de référence", "S3"),
         ajuste_q(mq1, lq1) && identical(mq1$resultat$coefficients$terme, c("(Intercept)", "annee", "secteur(S1)", "secteur(S2)", "annee*secteur(S1)", "annee*secteur(S2)")) &&
           identical(as.character(mq1$resultat$termes_retires), c("secteur(S3)", "annee*secteur(S3)")) && !length(mq1$resultat$termes_absents))
mq2 <- ajuster(pq, lm_q(c("habitat", "traitement"), list(c("habitat", "traitement"))))
verifier("habitat × traitement = lm(y ~ habitat * traitement) (deux variables qualitatives)", ajuste_q(mq2, lm(densite ~ habitat_f * traitement_f, data = tq)) &&
           identical(mq2$resultat$coefficients$terme[6], "habitat(H1)*traitement(T2)"))
dq3 <- lm_q("secteur", list(c("annee", "secteur")))
mq3 <- ajuster(pq, dq3)
lq3 <- lm(densite ~ secteur_f + secteur_f:annee, data = tq)
# (R range les pentes par niveau, la référence S3 d'abord)
verifier("secteur + annee × secteur (l'année sans effet principal) = lm(y ~ secteur + secteur:annee) : une pente par secteur",
         proche(mq3$resultat$coefficients$estimation, coef(lq3)[c(1:3, 5, 6, 4)]) && proche(mq3$resultat$coefficients$erreur_type, coef(summary(lq3))[c(1:3, 5, 6, 4), 2]) &&
           proche(mq3$resultat$AIC, AIC(lq3)) && proche(mq3$resultat$ajuste, fitted(lq3)) && isTRUE(dq3$x[[4]]$facteur_seul) &&
           identical(mq3$resultat$coefficients$terme[4:6], c("annee*secteur(S1)", "annee*secteur(S2)", "annee*secteur(S3)")))
mq4 <- ajuster(pq, lm_q("annee", list(c("annee", "secteur"))))
verifier("annee + annee × secteur (le secteur sans effet principal) = lm(y ~ annee + annee:secteur)", ajuste_q(mq4, lm(densite ~ annee + annee:secteur_f, data = tq), 1e-7))
lq5 <- lm(densite ~ habitat_f + habitat_f:traitement_f, data = tq)
mq5 <- ajuster(pq, lm_q("habitat", list(c("habitat", "traitement"))))
verifier("habitat + habitat × traitement (le traitement sans effet principal) = lm(y ~ habitat + habitat:traitement)",
         proche(sort(mq5$resultat$coefficients$estimation), sort(coef(lq5))) && proche(mq5$resultat$AIC, AIC(lq5)) && proche(mq5$resultat$ajuste, fitted(lq5)))
# trois variables, et le terme de trois listé avant ceux de deux : les termes d'ordre plus bas d'abord, comme R
lq6 <- lm(densite ~ annee * secteur_f * habitat_f, data = tq)
trois <- list(c("annee", "secteur", "habitat"), c("annee", "secteur"), c("annee", "habitat"), c("secteur", "habitat"))
mq6 <- ajuster(pq, lm_q(c("annee", "secteur", "habitat"), trois))
verifier("annee × secteur × habitat, le terme de trois listé en premier = lm(y ~ annee * secteur * habitat) (les termes d'ordre plus bas d'abord)",
         proche(sort(mq6$resultat$coefficients$estimation), sort(coef(lq6))) && proche(mq6$resultat$AIC, AIC(lq6)) &&
           identical(mq6$resultat$coefficients$terme[11:12], c("annee*secteur(S1)*habitat(H1)", "annee*secteur(S2)*habitat(H1)")))
mq7 <- ajuster(pq, definir_modele(pq, "LMM", x = uq("annee", "secteur"), y = uq("densite"), echelle = "none", croises = list(uq("annee", "secteur")), aleatoires = list(site_q)))
rq7 <- nlme::lme(densite ~ annee * secteur_f, random = ~ 1 | site_f, data = tq, method = "REML", control = ctrl)
rq7_ml <- nlme::lme(densite ~ annee * secteur_f, random = ~ 1 | site_f, data = tq, method = "ML", control = ctrl)
verifier("modèle mixte = nlme::lme(y ~ annee * secteur, random = ~ 1 | site) : coefficients et erreurs-types (REML), logLik et AIC (ML), valeurs ajustées",
         proche(mq7$resultat$coefficients$estimation, nlme::fixef(rq7), 1e-5) && proche(mq7$resultat$coefficients$erreur_type, summary(rq7)$tTable[, 2], 1e-4) &&
           proche(mq7$resultat$logLik, as.numeric(logLik(rq7_ml)), 1e-6) && proche(mq7$resultat$AIC, AIC(rq7_ml), 1e-6) && proche(mq7$resultat$ajuste, fitted(rq7), 1e-5))
dq8 <- definir_modele(pq, "LMM", x = uq("annee", "secteur"), y = uq("densite"), echelle = "none", croises = list(uq("annee", "secteur")), aleatoires = list(site_q), correlation = car1_q)
mq8 <- ajuster(pq, dq8)
cor_q <- nlme::corCAR1(form = ~ annee | site_f / secteur_f)
rq8 <- nlme::lme(densite ~ annee * secteur_f, random = ~ 1 | site_f, correlation = cor_q, data = tq, method = "REML", control = ctrl)
rq8_ml <- nlme::lme(densite ~ annee * secteur_f, random = ~ 1 | site_f, correlation = cor_q, data = tq, method = "ML", control = ctrl)
verifier(sprintf("avec CAR1 dans chaque secteur = nlme::lme(correlation = corCAR1(form = ~ annee | site/secteur)) (φ = %.3f)", mq8$resultat$phi),
         proche(mq8$resultat$coefficients$estimation, nlme::fixef(rq8), 1e-4) && proche(mq8$resultat$coefficients$erreur_type, summary(rq8)$tTable[, 2], 1e-3) &&
           proche(mq8$resultat$AIC, AIC(rq8_ml), 1e-6) && proche(mq8$resultat$phi, coef(rq8$modelStruct$corStruct, unconstrained = FALSE), 1e-3) &&
           proche(mq8$resultat$ajuste, fitted(rq8), 1e-4))
mq9 <- ajuster(pq, definir_modele(pq, "GLM", x = uq("habitat", "traitement"), y = uq("compte"), echelle = "none", famille = "poisson", croises = list(uq("habitat", "traitement"))))
rq9 <- mgcv::gam(compte ~ habitat_f * traitement_f, family = poisson, data = tq)
verifier("GLM de Poisson = mgcv::gam(compte ~ habitat * traitement, family = poisson) et glm() : coefficients, erreurs-types, AIC, valeurs ajustées",
         proche(mq9$resultat$coefficients$estimation, coef(rq9)) && proche(mq9$resultat$coefficients$erreur_type, summary(rq9)$p.table[, 2]) &&
           proche(mq9$resultat$AIC, AIC(glm(compte ~ habitat_f * traitement_f, family = poisson, data = tq))) && proche(mq9$resultat$ajuste, fitted(rq9)))
env_q <- new.env(parent = asNamespace("mgcv"))
f_q <- stats::as.formula("compte ~ s(x, k = 10) + habitat_f * traitement_f", env = env_q)
mq10 <- ajuster(pq, definir_modele(pq, "GAM", x = uq("x", "habitat", "traitement"), y = uq("compte"), echelle = "none", famille = "poisson",
                                   croises = list(uq("habitat", "traitement")), lisses = uq("x")))
rq10 <- mgcv::gam(f_q, family = poisson, data = tq, method = "REML")
verifier(sprintf("GAM, une courbe et un terme croisé qualitatif = mgcv::gam(compte ~ s(x) + habitat * traitement) (edf %.2f ; AIC de l'ajustement en ML)", mq10$resultat$courbes$edf),
         proche(mq10$resultat$coefficients$estimation, summary(rq10)$p.table[, 1], 1e-5) && proche(mq10$resultat$coefficients$erreur_type, summary(rq10)$p.table[, 2], 1e-4) &&
           proche(mq10$resultat$AIC, AIC(mgcv::gam(f_q, family = poisson, data = tq, method = "ML")), 1e-6) && proche(mq10$resultat$ajuste, fitted(rq10), 1e-5))
f_qg <- stats::as.formula("densite ~ s(x, k = 10) + annee * secteur_f", env = env_q)
mq11 <- ajuster(pq, definir_modele(pq, "GAM", x = uq("x", "annee", "secteur"), y = uq("densite"), echelle = "none", famille = "gaussienne", croises = list(uq("annee", "secteur")),
                                   lisses = uq("x"), aleatoires = list(site_q), correlation = car1_q))
rq11 <- mgcv::gamm(f_qg, random = list(site_f = ~1), correlation = cor_q, data = tq, method = "REML")
verifier("mgcv::gamm (courbe, site aléatoire, CAR1) avec annee × secteur = mgcv::gamm() : coefficients, AIC de la partie nlme en ML",
         isTRUE(mq11$gamm) && proche(mq11$resultat$coefficients$estimation, summary(rq11$gam)$p.table[, 1], 1e-4) &&
           proche(mq11$resultat$AIC, AIC(mgcv::gamm(f_qg, random = list(site_f = ~1), correlation = cor_q, data = tq, method = "ML")$lme), 1e-6))
# les effets : une droite par secteur, de pente le coefficient de l'année plus celui de son produit avec le secteur
eq7 <- effets_regression(mq7)[[1]]
b7 <- nlme::fixef(rq7)
V7 <- stats::vcov(rq7)
l7 <- function(k) { l <- stats::setNames(rep(0, length(b7)), names(b7)); l["annee"] <- 1; if (nzchar(k)) l[k] <- 1; l }
pentes7 <- vapply(c("annee:secteur_fS1", "annee:secteur_fS2", ""), function(k) sum(l7(k) * b7), 0)
se7 <- vapply(c("annee:secteur_fS1", "annee:secteur_fS2", ""), function(k) sqrt(as.numeric(t(l7(k)) %*% V7 %*% l7(k))), 0)
verifier(sprintf("Effets, modèle mixte : une droite par secteur, de pente %s, son intervalle tiré de vcov() de nlme",
                 paste(sprintf("%.3f", pentes7), collapse = ", ")),
         identical(eq7$terme, "annee") && identical(eq7$par, "secteur") && identical(vapply(eq7$niveaux, `[[`, "", "niveau"), c("S1", "S2", "S3")) &&
           proche(vapply(eq7$niveaux, `[[`, 0, "pente"), pentes7, 1e-5) && proche(vapply(eq7$niveaux, function(n) (n$pente_haut - n$pente) / 1.96, 0), se7, 1e-4) &&
           proche(eq7$niveaux[[2]]$effet, pentes7[2] * (eq7$niveaux[[2]]$x - mean(tq$annee)), 1e-5) && is.null(eq7$x))
eq3 <- effets_regression(mq3)
verifier("Effets, l'année sans effet principal : la pente de chaque secteur, le coefficient de son produit",
         length(eq3) == 1 && proche(vapply(eq3[[1]]$niveaux, `[[`, 0, "pente"), coef(lq3)[c(5, 6, 4)]) &&
           proche(vapply(eq3[[1]]$niveaux, function(n) (n$pente_haut - n$pente) / 1.96, 0), coef(summary(lq3))[c(5, 6, 4), 2]))
eq11 <- effets_regression(mq11)
verifier("Effets d'un GAMM : la courbe de x telle quelle, une droite par secteur pour l'année",
         length(eq11) == 2 && isTRUE(eq11[[1]]$courbe) && length(eq11[[1]]$x) == 80 && length(eq11[[2]]$niveaux) == 3 && !isTRUE(eq11[[2]]$courbe))
# l'apport des groupes, la sélection par l'AIC, Ajuster ce modèle, les modèles emboîtés
aq1 <- apport_groupes(mq1)
verifier("apport des groupes : un effet principal sort avec le terme croisé qui le contient (= lm() réajustés) ; le terme croisé, seul",
         identical(aq1$groupe, c("annee", "secteur", "annee*secteur")) && identical(aq1$avec, c("annee*secteur", "annee*secteur", "")) &&
           identical(as.numeric(aq1$termes), c(3, 4, 2)) && proche(aq1$delta_aic, c(AIC(lm(densite ~ secteur_f, tq)), AIC(lm(densite ~ annee, tq)),
                                                                                     AIC(lm(densite ~ annee + secteur_f, tq))) - AIC(lq1)))
sq1 <- selection_aic(mq1, dq1)
aicc_q <- function(f) { l <- lm(f, data = tq); k <- attr(logLik(l), "df"); AIC(l) + 2 * k * (k + 1) / (nq - k - 1) }
attendus_q <- c("annee + secteur + annee*secteur" = aicc_q(densite ~ annee * secteur_f), "annee + secteur" = aicc_q(densite ~ annee + secteur_f),
                "secteur" = aicc_q(densite ~ secteur_f), "annee" = aicc_q(densite ~ annee), "1" = aicc_q(densite ~ 1))
verifier("sélection par l'AIC : les 5 combinaisons permises (le terme croisé avec ses deux effets principaux, le secteur par toutes ses catégories), = lm()",
         nrow(sq1$modeles) == 5 && setequal(sq1$modeles$modele, names(attendus_q)) && proche(sq1$modeles$AICc, attendus_q[sq1$modeles$modele]) &&
           identical(as.character(sq1$requis[[3]]), c("annee", "secteur")))
mq1b <- ajuster(pq, definition_selection(dq1, mq1, c("annee", "secteur")))
verifier("Ajuster ce modèle (annee + secteur) : = lm(y ~ annee + secteur)", ajuste_q(mq1b, lm(densite ~ annee + secteur_f, data = tq)))
sq6 <- selection_aic(mq6, lm_q(c("annee", "secteur", "habitat"), trois))
verifier("un terme croisé de trois variables n'entre qu'avec ses effets principaux et ses termes croisés de deux",
         setequal(unlist(sq6$requis[sq6$groupes == "annee*secteur*habitat"]), c("annee", "secteur", "habitat", "annee*secteur", "annee*habitat", "secteur*habitat")))
rvq <- rapports_vraisemblance(list(croise = mq1, additif = ajuster(pq, lm_q(c("annee", "secteur"), list())), seul = mq4))
khi2_q <- 2 * (as.numeric(logLik(lq1)) - as.numeric(logLik(lm(densite ~ annee + secteur_f, tq))))
i_q <- which(rvq$paires$grand == "croise" & rvq$paires$petit == "additif")
verifier(sprintf("y ~ annee + secteur emboîté dans y ~ annee * secteur : χ² = 2 Δ logLik de lm() = %.3f sur 2 ddl", khi2_q),
         length(i_q) == 1 && proche(rvq$paires$khi2[i_q], khi2_q) && rvq$paires$ddl[i_q] == 2 && any(rvq$paires$grand == "croise" & rvq$paires$petit == "seul"))
# une combinaison jamais observée : laissée de côté, et nommée comme telle
pz_q <- projet_depuis_tableau(tq[!(tq$habitat == "H2" & tq$traitement == "T3"), c("nom", "habitat", "traitement", "densite")], "cellule vide")
mz_q <- ajuster(pz_q, definir_modele(pz_q, "LM", x = numeros(pz_q, c("habitat", "traitement")), y = numeros(pz_q, "densite"), echelle = "none",
                                     croises = list(numeros(pz_q, c("habitat", "traitement")))))
lz_q <- lm(densite ~ habitat_f * traitement_f, data = tq[!(tq$habitat == "H2" & tq$traitement == "T3"), ])
verifier(sprintf("une combinaison jamais observée (H2 avec T3) : laissée de côté et nommée (« %s »), un produit qui s'en déduit laissé de côté comme par lm() (NA)",
                 paste(mz_q$resultat$termes_absents, collapse = ", ")),
         identical(as.character(mz_q$resultat$termes_absents), "habitat(H2)*traitement(T3)") && "habitat(H1)*traitement(T2)" %in% mz_q$resultat$termes_retires &&
           proche(mz_q$resultat$coefficients$estimation, stats::na.omit(coef(lz_q))) && proche(mz_q$resultat$ajuste, fitted(lz_q)))
# une catégorie que l'ajustement n'a pas vue (des secteurs propres à chaque site, la validation croisée par site)
vc_site <- list(by = "variable", variable = uq("site"))
mv1 <- ajuster(pq, definir_modele(pq, "LM", x = uq("annee", "secteur_site"), y = uq("densite"), echelle = "none", croises = list(uq("annee", "secteur_site")), vc = vc_site))
pred_v1 <- rep(NA_real_, nq)
for (s in unique(tq$site)) {  # (le secteur jamais vu : prédit comme la catégorie de référence du sous-modèle, la dernière qu'il a)
  ici <- tq$site == s
  appris <- tq[!ici, ]
  ref <- sort(unique(appris$secteur_site))
  l <- lm(densite ~ annee * f, data = transform(appris, f = factor(secteur_site, levels = c(ref[length(ref)], ref[-length(ref)]))))
  pred_v1[ici] <- coef(l)[1] + coef(l)[2] * tq$annee[ici]
}
mv2 <- ajuster(pq, definir_modele(pq, "LM", x = uq("secteur_site"), y = uq("densite"), echelle = "none", croises = list(uq("annee", "secteur_site")), vc = vc_site))
verifier(sprintf("validation croisée par site : un secteur jamais vu prédit comme la catégorie de référence (avec l'effet principal de l'année) ; sans lui, pas de pente, pas de prédiction (%d)", mv2$vc$sans_pente),
         proche(mv1$vc$prediction, pred_v1) && mv1$vc$sans_pente == 0 && mv2$vc$sans_pente == nq && all(is.na(mv2$vc$prediction)) && is.na(mv2$vc$Q2))
# prédire : comme predict(lm()) ; une catégorie inconnue du projet, sans prédiction ; une catégorie hors du modèle, comme la référence, ou sans prédiction sans effet principal
obs_q <- numeros_observations(pq)
garde_q <- tq$secteur != "S2"
dg1 <- lm_q(c("annee", "secteur"), list(c("annee", "secteur")), observations = obs_q[garde_q])
mg1 <- ajuster(pq, dg1)
lg1 <- lm(densite ~ annee * secteur_f, data = droplevels(tq[garde_q, ]))
prq <- predire(pq, mg1, dg1, obs_q)
dg2 <- lm_q("secteur", list(c("annee", "secteur")), observations = obs_q[garde_q])
prq2 <- predire(pq, ajuster(pq, dg2), dg2, obs_q)
verifier("prédire : les observations du modèle = predict(lm()) ; une catégorie hors du modèle (S2) prédite comme la référence, ou sans prédiction quand l'année n'a pas d'effet principal",
         proche(prq[["densite prédit"]][garde_q], predict(lg1, tq[garde_q, ])) && proche(prq[["densite prédit"]][!garde_q], coef(lg1)[1] + coef(lg1)[2] * tq$annee[!garde_q]) &&
           all(is.na(prq2[["densite prédit"]][!garde_q])) && !anyNA(prq2[["densite prédit"]][garde_q]))
dossier_q <- tempfile("prediction")
dir.create(dossier_q)
fq <- file.path(dossier_q, "secteurs.csv")
utils::write.csv(data.frame(nom = c("a", "b"), annee = c(2005, 2005), secteur = c("S1", "S9")), fq, row.names = FALSE)
rfq <- predire_fichier(pq, mq1, dq1, fq)
verifier("prédire un fichier : une catégorie connue = predict(lm()), une catégorie inconnue du projet (S9) sans prédiction",
         proche(rfq$observations[["densite prédit"]][1], predict(lq1, data.frame(annee = 2005, secteur_f = factor("S1", levels = levels(tq$secteur_f))))) &&
           is.na(rfq$observations[["densite prédit"]][2]))
unlink(dossier_q, recursive = TRUE)
mt_q <- ajuster(pq, lm_q("secteur_site", list(c("annee", "secteur_site")), test = obs_q[tq$site == "F"]))
verifier("jeu de test d'un site entier, une pente par secteur sans effet principal : ses secteurs jamais vus restent sans prédiction, hors des statistiques",
         mt_q$test$statistiques$n == 0 && all(is.na(mt_q$test$observations[["densite prédit"]])))
# refusés, en nommant le terme
verifier("une variable qualitative au carré ou deux fois dans un terme croisé : refusé, en nommant le terme",
         grepl("« secteur × secteur »", refus(lm_q("secteur", list(c("secteur", "secteur"))))) &&
           grepl("une fois", refus(lm_q(c("annee", "secteur"), list(c("secteur", "annee", "secteur"))))))
dq_mal <- dq1
dq_mal$x[[7]] <- list(variables = uq("secteur", "secteur"), categories = c(1, 2), name = "secteur(S1)*secteur(S2)", transform = NULL, scaling = list(type = "none"))
dq_seul <- dq3
dq_seul$x <- dq_seul$x[-(5:7)]  # (l'année, facteur seul, sans le terme croisé qui la demandait)
verifier("une définition lue d'un fichier : un terme croisé d'une variable qualitative deux fois, un facteur seul d'aucun terme croisé, refusés",
         grepl("une fois", refus(ajuster(pq, dq_mal))) && grepl("aucun terme croisé", refus(ajuster(pq, dq_seul))))
# les réglages, la formule R, enregistrer et relire
rgq <- reglages_modele(pq, dq3)
verifier("les réglages du modèle sans effet principal (Modifier le modèle) : X le secteur, le terme croisé annee × secteur",
         identical(as.numeric(rgq$x), uq("secteur")) && identical(rgq$croises, list(uq("annee", "secteur"))))
fq3 <- formule_reglages(pq, rgq)
lu_q <- lire_formule(pq, fq3)
lu_q2 <- lire_formule(pq, "densite ~ log(annee):secteur + secteur")
mq_lu <- ajuster(pq, definir_modele(pq, "LM", x = lu_q2$x, y = lu_q2$y, echelle = "none", croises = lu_q2$croises, transformations = unlist(lu_q2$transformations)))
verifier(sprintf("formule R : %s, relue en les mêmes réglages ; log(annee):secteur + secteur = lm(y ~ secteur + secteur:log(annee))", fq3),
         identical(fq3, "densite ~ secteur + annee:secteur") && identical(as.numeric(lu_q$x), uq("secteur")) && identical(unclass(lu_q$croises), list(uq("annee", "secteur"))) &&
           identical(lu_q2$transformations[[as.character(uq("annee"))]], "ln:1:0") &&
           proche(mq_lu$resultat$AIC, AIC(lm(densite ~ secteur_f + secteur_f:log(annee), data = tq))))
lu_q3 <- lire_formule(pq, "densite ~ annee * secteur + (1 | site)")
verifier("formule R : annee * secteur avec une variable qualitative règle le terme croisé ; réécrite, la même",
         lu_q3$type == "LMM" && identical(unclass(lu_q3$croises), list(uq("annee", "secteur"))) &&
           identical(formule_reglages(pq, lu_q3), "densite ~ annee + secteur + annee:secteur + (1 | site)"))
fsq <- tempfile(fileext = ".nuee")
pq$models <- list(dq3)
enregistrer_projet(pq, fsq)
qq <- lire_projet(fsq)
verifier("enregistré puis relu : le modèle sans effet principal, le même", proche(ajuster(qq, qq$models[[1]])$resultat$AIC, mq3$resultat$AIC, 1e-10))
for (l in c("fr", "en")) {
  tq1 <- paste(texte_methodes(pq, dq1, mq1, l, "essai")$paragraphes, collapse = " ")
  tq3 <- paste(texte_methodes(pq, dq3, mq3, l, "essai")$paragraphes, collapse = " ")
  tq2 <- paste(texte_methodes(pq, lm_q(c("habitat", "traitement"), list(c("habitat", "traitement"))), mq2, l, "essai")$paragraphes, collapse = " ")
  verifier(sprintf("texte des méthodes (%s) : la catégorie de référence nommée, ce que valent les coefficients des termes croisés (écart de pente, pente de chaque secteur, écart d'effet)", l),
           grepl(if (l == "fr") "« S3 » pour « secteur »" else "“S3” for “secteur”", tq1) &&
             grepl(if (l == "fr") "l'écart de la pente de « annee » à sa pente dans « S3 »" else "the difference between the slope of “annee” in it and its slope in “S3”", tq1) &&
             grepl(if (l == "fr") "n'ayant pas d'effet principal, les coefficients de « annee × secteur » sont la pente" else "has no main effect, the coefficients of “annee × secteur” are the slope", tq3) &&
             grepl(if (l == "fr") "ce même effet dans « H2 »" else "the same effect in “H2”", tq2) && !any(grepl("NA", c(tq1, tq2, tq3), fixed = TRUE)))
}

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
