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

cat("Texte des méthodes\n")
for (l in c("fr", "en")) {
  t3 <- texte_methodes(p, d3, m3, l, "essai")
  t1 <- texte_methodes(p, d, m, l, "essai")
  verifier(sprintf("modèle mixte et régression (%s) : 5 paragraphes, nlme et Nakagawa cités, AIC et Q² donnés", l),
           length(t3$paragraphes) == 5 && any(grepl("Pinheiro", t3$references)) && any(grepl("Nakagawa", t3$references)) &&
             any(grepl("AIC", t3$paragraphes)) && any(grepl("Q²", t1$paragraphes)) && !any(grepl("NA", c(t3$paragraphes, t1$paragraphes), fixed = TRUE)) &&
             !any(grepl("nlme", t1$references)))
}

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
