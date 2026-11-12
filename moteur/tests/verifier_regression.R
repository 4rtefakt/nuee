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
