# Vérifie les moyennes ajustées des régressions (moteur/R/moyennes.R), comme le paquet emmeans les
# calcule : contre les valeurs d'emmeans 2.0.4 enregistrées ici (et contre emmeans lui-même quand
# NUEE_EMMEANS désigne une bibliothèque où il est installé), et contre un calcul indépendant écrit ici
# (la grille de référence de R : model.matrix() sur ses propres facteurs, ou la matrice lpmatrix de
# mgcv, moyennée à la main ; ptukey(), p.adjust()). Une régression linéaire avec un facteur et une
# covariable transformée (la moyenne de x avant sa transformation), facteur × facteur et les effets
# simples (Holm, intervalles de Bonferroni), un modèle mixte de nlme avec facteur × année et CAR1
# (les degrés de liberté de confinement, les pentes par catégorie et leurs paires, égales à celles de
# la vue Effets), un GLM de Poisson sur l'échelle de la réponse (rapports, z), avec un effort, un GLM
# binomial (rapports des cotes), un GAM et un mgcv::gamm (la covariance bayésienne), un carré et un
# terme croisé de deux nombres, une combinaison jamais observée (laissée de côté, la famille de Tukey
# des paires estimables) ; les refus, les phrases et le texte des Méthodes.
#   Rscript moteur/tests/verifier_moyennes.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-6) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

# six sites suivis dix ans (2005 manque), trois secteurs dans chacun, deux habitats (trois sites
# chacun), trois traitements ; la pente de l'année change d'un secteur à l'autre ; des comptages, une
# présence, un effort
set.seed(21)
tq <- expand.grid(annee = c(2001:2004, 2006:2011), secteur = c("S1", "S2", "S3"), site = c("A", "B", "C", "D", "E", "F"), stringsAsFactors = FALSE)
nq <- nrow(tq)
tq$nom <- sprintf("Q%03d", seq_len(nq))
tq$habitat <- ifelse(tq$site %in% c("A", "B", "C"), "H1", "H2")
tq$traitement <- c("T1", "T2", "T3")[(seq_len(nq) %% 3) + 1]
tq$x <- round(runif(nq, 0, 10), 3) + 0.5
bruit <- numeric(nq)
for (i in split(seq_len(nq), paste(tq$site, tq$secteur))) bruit[i[order(tq$annee[i])]] <- as.vector(stats::filter(rnorm(length(i), 0, 0.5), 0.5, "recursive"))
tq$densite <- 3 + c(S1 = 0.05, S2 = 0.25, S3 = -0.1)[tq$secteur] * (tq$annee - 2000) + sin(tq$x / 2) + rnorm(6, 0, 0.6)[as.integer(factor(tq$site))] + bruit
tq$compte <- stats::rpois(nq, exp(0.4 + 0.5 * (tq$habitat == "H1") + 0.4 * (tq$traitement == "T2") * (tq$habitat == "H2") + 0.3 * sin(tq$x / 2)))
tq$presence <- as.numeric(tq$compte > 1)
tq$effort <- round(runif(nq, 1, 3), 2)
tq$compte_e <- stats::rpois(nq, tq$effort * exp(0.2 + 0.5 * (tq$habitat == "H1")))
tq$site <- factor(tq$site)
pq <- projet_depuis_tableau(tq[c("nom", "site", "secteur", "habitat", "traitement", "annee", "x", "densite", "compte", "presence", "effort", "compte_e")], "moyennes")
uq <- function(...) numeros(pq, c(...))
site_q <- list(by = "variable", variable = uq("site"))
car1_q <- list(type = "CAR1", temps = list(by = "variable", variable = uq("annee")), dans = list(by = "variable", variable = uq("secteur")))
ctrl <- nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE)
env_mgcv <- new.env(parent = asNamespace("mgcv"))

# Les valeurs d'emmeans 2.0.4 sur ces données, par les modèles de R écrits plus bas (lA... lJ) :
# emmeans(), pairs(..., infer = TRUE), emtrends(), type = "response", offset = 0 (E). Leurs facteurs
# ont les niveaux par défaut de R (dans l'ordre alphabétique, celui des catégories de Nuée).
EMMEANS <- list(
  A = list(emmean = c(2.80635048582, 4.28298777695, 1.85541474512), SE = c(0.128091279837, 0.126267497073, 0.126696816386), df = c(175, 175, 175),
           lower = c(2.55354793966, 4.03378467119, 1.60536432942), estimate = c(-1.47663729113, 0.950935740694, 2.42757303183),
           p = c(8.18234369149e-14, 7.77956421838e-07, 0), p_lower = c(-1.89643414707, 0.531544016494, 2.00852834297), p_upper = c(-1.0568404352, 1.37032746489, 2.84661772069)),
  B = list(emmean = c(3.09227978138, 3.19858756267, 3.23346013762), SE = c(0.178413915562, 0.179167929512, 0.179258847939), p = c(0.90717964129, 0.842464930169, 0.989708827397),
           holm_estimate = c(-0.126772155898, -0.116320832338, 0.0104513235594, -0.0858434066827, -0.166039880131, -0.0801964734485), holm_p = c(1, 1, 1, 1, 1, 1),
           holm_lower = c(-0.989398422976, -0.980748284266, -0.854301064778, -0.951296832777, -1.02927810375, -0.948926819585)),
  C = list(emmean = c(3.05462279662, 4.42484091192, 2.03632758079), SE = c(0.258101197796, 0.258101197796, 0.258101197793), df = c(5, 5, 5), p_df = c(169, 169, 169),
           p = c(8.47066861098e-12, 2.55927006587e-07, 0), p_lower = c(-1.80023499168, 0.58827833945, 1.95849645476),
           trend = c(0.0348460079274, 0.301667493785, -0.111687062155), trend_SE = c(0.0385974919964, 0.0385974919964, 0.0385974919964), trend_df = c(169, 169, 169),
           trend_p = c(7.00739928794e-06, 0.0216722748625, 6.79856171359e-12)),
  D = list(rate = c(1.77951304201, 2.53135887969, 1.87794213614), SE = c(0.177169047708, 0.205480465609, 0.178729967671), lower = c(1.46404761289, 2.15902816144, 1.55836985548),
           ratio = c(0.702987259643, 0.947586726856, 1.34794295893), ratio_SE = c(0.0903044245703, 0.130513462839, 0.1686123804), p = c(0.0167402222042, 0.919239423224, 0.0447734369807),
           ratio_lower = c(0.520230190709, 0.686162711377, 1.00542398379), link = c(0.576339754969, 0.928756265146, 0.630176568882)),
  E = list(rate = c(2.17688563146, 1.13461640422), lower = c(1.97318922294, 0.992343705856)),
  F = list(prob = c(0.737581568339, 0.474690056101), SE = c(0.0479017969381, 0.0550750072079), odds_ratio = 3.1104355548, p = 0.000695633492484),
  G = list(emmean = c(3.09833298488, 4.48876866649, 2.11232503096), SE = c(0.11992111336, 0.111518398705, 0.116346583076), df = c(172.179379325, 172.179379325, 172.179379325)),
  H = list(emmean = c(3.24531458819, 4.57196705711, 2.3086216839), SE = c(0.278463742699, 0.276399257362, 0.279072030845), df = c(168.99088797, 168.99088797, 168.99088797)),
  I = list(emmean = c(3.12587234865, 4.52153077237, 2.131138963), trend = c(-0.240617659186, -0.303200056634, -0.249650868197),
           trend_SE = c(0.0375026616947, 0.0431290715539, 0.0406096754191), trend_p = c(0.515192007392, 0.984855510242, 0.637042533479)),
  J = list(emmean = c(3.0993725189, 3.31980344577), estimate = -0.220430926871, p = 0.428646036941, lower = -0.769319602983,
           simple_estimate = c(-0.141439994161, 0.0543330010519, 0.195772995213, -0.29942185958), simple_p = c(0.931043454108, 0.989505128902, 0.872153512459, 0.447070926171),
           simple_lower = c(-1.07147225657, -0.875699261357, -0.734259267196, -1.07566766957)))

# emmeans lui-même, s'il est installé (NUEE_EMMEANS : une bibliothèque où il l'est) : les valeurs
# enregistrées refaites, à la fin
if (nzchar(Sys.getenv("NUEE_EMMEANS"))) .libPaths(c(Sys.getenv("NUEE_EMMEANS"), .libPaths()))
avec_emmeans <- requireNamespace("emmeans", quietly = TRUE)

# Le calcul indépendant : la moyenne de chaque groupe `par` (des rangs de lignes) de la matrice `X` d'un
# modèle de R aux lignes de sa grille de référence, à poids égaux, son erreur-type tirée de `V` ; les
# paires de groupes (la différence, son erreur-type, t sur `ddl` degrés de liberté, p de Tukey pour k
# moyennes et son intervalle, ou p brut).
a_la_main <- function(X, par, b, V, ddl) {
  L <- do.call(rbind, lapply(par, function(i) colMeans(X[i, , drop = FALSE])))
  ij <- utils::combn(length(par), 2)
  Ld <- L[ij[1, ], , drop = FALSE] - L[ij[2, ], , drop = FALSE]
  d <- drop(Ld %*% b)
  sd <- sqrt(rowSums((Ld %*% V) * Ld))
  k <- length(par)
  list(L = L, est = drop(L %*% b), se = sqrt(rowSums((L %*% V) * L)), d = d, sd = sd, t = d / sd,
       p_tukey = stats::ptukey(sqrt(2) * abs(d / sd), k, ddl, lower.tail = FALSE), p_brut = 2 * stats::pt(-abs(d / sd), ddl),
       bas_tukey = d - stats::qtukey(0.95, k, ddl) / sqrt(2) * sd)
}

cat("Régression linéaire : un facteur et une covariable transformée\n")
dA <- definir_modele(pq, "LM", x = uq("secteur", "x", "annee"), y = uq("densite"), echelle = "UV", transformations = stats::setNames("log10", uq("x")))
mA <- ajuster(pq, dA)
rA <- moyennes_ajustees(pq, mA, dA)
lA <- lm(densite ~ secteur + log10(x) + annee, data = tq)
e <- EMMEANS$A
verifier("les moyennes de « secteur » = emmeans (estimation, erreur-type, 175 ddl, intervalle à 95 %)",
         proche(rA$moyennes$estimation, e$emmean) && proche(rA$moyennes$erreur_type, e$SE) && identical(as.numeric(rA$moyennes$ddl), e$df) && proche(rA$moyennes$bas, e$lower))
verifier("les paires (Tukey) = pairs() d'emmeans : différence, p, intervalle de Tukey",
         identical(paste(rA$paires$niveau_1, rA$paires$niveau_2), c("S1 S2", "S1 S3", "S2 S3")) && proche(rA$paires$estimation, e$estimate) &&
           isTRUE(all.equal(rA$paires$p, e$p, tolerance = 1e-6, scale = 1)) && proche(rA$paires$bas, e$p_lower, 1e-5) && proche(rA$paires$haut, e$p_upper, 1e-5))
# (la grille de R : x à la moyenne de x, non de log10(x), comme emmeans ; l'année à sa moyenne)
grille <- data.frame(secteur = c("S1", "S2", "S3"), x = mean(tq$x), annee = mean(tq$annee))
hA <- a_la_main(model.matrix(~ secteur + log10(x) + annee, grille), as.list(1:3), coef(lA), vcov(lA), lA$df.residual)
verifier(sprintf("calculées ici sur la grille de R (log10 de la moyenne de x : %.4f, pas %.4f)", log10(mean(tq$x)), mean(log10(tq$x))),
         proche(rA$moyennes$estimation, hA$est, 1e-8) && proche(rA$moyennes$erreur_type, hA$se, 1e-8) && proche(rA$paires$p, hA$p_tukey, 1e-8) &&
           proche(rA$paires$bas, hA$bas_tukey, 1e-8) && proche(rA$grille$covariables$moyenne, c(mean(tq$x), mean(tq$annee)), 1e-12))
r0 <- moyennes_ajustees(pq, mA, dA, list(ajustement = "aucun"))
verifier("sans ajustement : la valeur p brute de t et l'intervalle de t", proche(r0$paires$p, hA$p_brut, 1e-8) &&
           proche(r0$paires$bas, hA$d - stats::qt(0.975, 175) * hA$sd, 1e-8) && all(is.na(r0$paires$famille)))

cat("Facteur × facteur, les effets simples\n")
dB <- definir_modele(pq, "LM", x = uq("habitat", "traitement", "x"), y = uq("densite"), echelle = "UV", croises = list(uq("habitat", "traitement")))
mB <- ajuster(pq, dB)
lB <- lm(densite ~ habitat * traitement + x, data = tq)
rB <- moyennes_ajustees(pq, mB, dB, list(facteur = uq("traitement")))
e <- EMMEANS$B
grilleB <- expand.grid(habitat = c("H1", "H2"), traitement = c("T1", "T2", "T3"), stringsAsFactors = FALSE)
grilleB$x <- mean(tq$x)
XB <- model.matrix(~ habitat * traitement + x, grilleB)
hB <- a_la_main(XB, split(seq_len(6), grilleB$traitement), coef(lB), vcov(lB), lB$df.residual)
verifier("les moyennes de « traitement », à poids égaux sur les deux habitats, = emmeans et le calcul d'ici ; « habitat » en moyenne",
         proche(rB$moyennes$estimation, e$emmean) && proche(rB$moyennes$erreur_type, e$SE) && proche(rB$paires$p, e$p) &&
           proche(rB$moyennes$estimation, hB$est, 1e-8) && identical(as.character(rB$grille$moyennees), "habitat") &&
           identical(as.character(rB$croisee), "habitat"))
rBh <- moyennes_ajustees(pq, mB, dB, list(facteur = uq("traitement"), dans = uq("habitat"), ajustement = "holm"))
par_h1 <- a_la_main(XB, as.list(c(1, 3, 5)), coef(lB), vcov(lB), lB$df.residual)
verifier("dans chaque habitat (Holm) : les paires de chacun, p de Holm sur ses 3 paires, intervalles de Bonferroni, = emmeans",
         identical(rBh$paires$groupe, rep(c("H1", "H2"), each = 3)) && proche(rBh$paires$estimation, e$holm_estimate) && proche(rBh$paires$p, e$holm_p) &&
           proche(rBh$paires$bas, e$holm_lower, 1e-6) && proche(rBh$paires$p[1:3], stats::p.adjust(par_h1$p_brut, "holm"), 1e-8) &&
           proche(rBh$paires$bas[1:3], par_h1$d - stats::qt(1 - 0.05 / 3 / 2, lB$df.residual) * par_h1$sd, 1e-8) && all(rBh$paires$famille == 3))

cat("Modèle mixte (nlme) : facteur × année, CAR1\n")
dC <- definir_modele(pq, "LMM", x = uq("annee", "secteur"), y = uq("densite"), echelle = "UV", croises = list(uq("annee", "secteur")), aleatoires = list(site_q), correlation = car1_q)
mC <- ajuster(pq, dC)
rC <- moyennes_ajustees(pq, mC, dC)
lC <- eval(bquote(nlme::lme(densite ~ annee * secteur, random = ~ 1 | site, correlation = nlme::corCAR1(form = ~ annee | site / secteur), data = tq,
                            method = "REML", control = .(ctrl))))
e <- EMMEANS$C
verifier("les moyennes = emmeans pour nlme::lme ; ddl de confinement : 5 pour une moyenne (6 sites moins un), 169 pour une paire",
         proche(rC$moyennes$estimation, e$emmean, 1e-4) && proche(rC$moyennes$erreur_type, e$SE, 1e-3) && identical(as.numeric(rC$moyennes$ddl), e$df) &&
           identical(as.numeric(rC$paires$ddl), e$p_df) && isTRUE(all.equal(rC$paires$p, e$p, tolerance = 1e-4, scale = 1)) && proche(rC$paires$bas, e$p_lower, 1e-4) &&
           identical(rC$ddl$methode, "confinement") && rC$ddl$groupes == 6)
grilleC <- data.frame(secteur = c("S1", "S2", "S3"), annee = mean(tq$annee))
hC <- a_la_main(model.matrix(~ annee * secteur, grilleC), as.list(1:3), nlme::fixef(lC), vcov(lC), 169)
verifier("calculées ici sur la grille de R et la covariance de nlme (varFix)", proche(rC$moyennes$estimation, hC$est, 1e-5) && proche(rC$moyennes$erreur_type, hC$se, 1e-4) &&
           proche(rC$paires$p, hC$p_tukey, 1e-3))
rCp <- moyennes_ajustees(pq, mC, dC, list(pente = uq("annee")))
delta <- 0.001 * diff(range(tq$annee))
grilleC2 <- transform(grilleC, annee = annee + delta)
hCp <- a_la_main((model.matrix(~ annee * secteur, grilleC2) - model.matrix(~ annee * secteur, grilleC)) / delta, as.list(1:3), nlme::fixef(lC), vcov(lC), 169)
ef <- effets_regression(mC)[[1]]
verifier("les pentes de l'année par secteur = emtrends (169 ddl) et leurs paires ; = les pentes de la vue Effets, et la différence avant calculée ici",
         identical(rCp$mode, "pentes") && proche(rCp$moyennes$estimation, e$trend, 1e-4) && proche(rCp$moyennes$erreur_type, e$trend_SE, 1e-3) &&
           identical(as.numeric(rCp$moyennes$ddl), e$trend_df) && proche(rCp$paires$p, e$trend_p, 1e-3) &&
           proche(rCp$moyennes$estimation, vapply(ef$niveaux, `[[`, 0, "pente"), 1e-9) && proche(rCp$moyennes$estimation, hCp$est, 1e-5) &&
           identical(rCp$echelle, "identite"))
verifier("les choix : « secteur », sans variable qualitative croisée, la pente de « annee »",
         length(rC$options) == 1 && identical(rC$options[[1]]$nom, "secteur") && !length(rC$options[[1]]$dans) && identical(rC$options[[1]]$pentes[[1]]$nom, "annee"))

cat("GLM de Poisson : l'échelle de la réponse\n")
dD <- definir_modele(pq, "GLM", x = uq("habitat", "traitement"), y = uq("compte"), echelle = "none", famille = "poisson", croises = list(uq("habitat", "traitement")))
mD <- ajuster(pq, dD)
gD <- glm(compte ~ habitat * traitement, family = poisson, data = tq)
rD <- moyennes_ajustees(pq, mD, dD, list(facteur = uq("traitement")))
e <- EMMEANS$D
verifier("sur l'échelle de la réponse : exp de la moyenne, son erreur-type par la méthode delta, l'intervalle transformé ; z (ddl infinis) ; = emmeans(type = \"response\")",
         identical(rD$echelle, "reponse") && proche(rD$moyennes$estimation, e$rate, 1e-6) && proche(rD$moyennes$erreur_type, e$SE, 1e-5) && proche(rD$moyennes$bas, e$lower, 1e-6) &&
           all(is.infinite(rD$moyennes$ddl)) && identical(rD$ddl$methode, "asymptotique"))
verifier("les paires sont des rapports de moyennes, testés sur l'échelle du lien (Tukey, z) : = emmeans",
         proche(rD$paires$estimation, e$ratio, 1e-6) && proche(rD$paires$erreur_type, e$ratio_SE, 1e-5) && proche(rD$paires$p, e$p, 1e-6) && proche(rD$paires$bas, e$ratio_lower, 1e-5))
grilleD <- expand.grid(habitat = c("H1", "H2"), traitement = c("T1", "T2", "T3"), stringsAsFactors = FALSE)
hD <- a_la_main(model.matrix(~ habitat * traitement, grilleD), split(seq_len(6), grilleD$traitement), coef(gD), vcov(gD), Inf)
rDl <- moyennes_ajustees(pq, mD, dD, list(facteur = uq("traitement"), echelle = "lien"))
verifier("sur l'échelle du lien : les moyennes du glm() calculées ici, les paires des différences",
         identical(rDl$echelle, "lien") && proche(rDl$moyennes$estimation, e$link, 1e-6) && proche(rDl$moyennes$estimation, hD$est, 1e-6) && proche(rDl$paires$estimation, hD$d, 1e-6) &&
           proche(exp(hD$est), rD$moyennes$estimation, 1e-6))
dE <- definir_modele(pq, "GLM", x = uq("habitat"), y = uq("compte_e"), echelle = "none", famille = "poisson", decalage = uq("effort"))
mE <- ajuster(pq, dE)
rE <- moyennes_ajustees(pq, mE, dE)
verifier("avec un effort en décalage : des densités, l'effort à 1 (= emmeans(offset = 0))", proche(rE$moyennes$estimation, EMMEANS$E$rate, 1e-6) &&
           proche(rE$moyennes$bas, EMMEANS$E$lower, 1e-6) && identical(rE$grille$decalage, "effort"))
dF <- definir_modele(pq, "GLM", x = uq("habitat", "x"), y = uq("presence"), echelle = "none", famille = "binomiale")
mF <- ajuster(pq, dF)
rF <- moyennes_ajustees(pq, mF, dF)
verifier("binomiale (logit) : des probabilités et un rapport des cotes, = emmeans", proche(rF$moyennes$estimation, EMMEANS$F$prob, 1e-6) &&
           proche(rF$moyennes$erreur_type, EMMEANS$F$SE, 1e-5) && proche(rF$paires$estimation, EMMEANS$F$odds_ratio, 1e-6) && proche(rF$paires$p, EMMEANS$F$p, 1e-5))

cat("GAM et mgcv::gamm\n")
dG <- definir_modele(pq, "GAM", x = uq("x", "annee", "secteur"), y = uq("densite"), echelle = "none", famille = "gaussienne", croises = list(uq("annee", "secteur")), lisses = uq("x"))
mG <- ajuster(pq, dG)
rG <- moyennes_ajustees(pq, mG, dG)
gG <- mgcv::gam(stats::as.formula("densite ~ s(x, k = 10) + annee * secteur", env = env_mgcv), data = tq, method = "REML")
grilleG <- data.frame(secteur = c("S1", "S2", "S3"), x = mean(tq$x), annee = mean(tq$annee))
hG <- a_la_main(stats::predict(gG, grilleG, type = "lpmatrix"), as.list(1:3), coef(gG), vcov(gG), gG$df.residual)
# (les valeurs enregistrées viennent de mgcv 1.9 : une autre version lisse un peu autrement, d'où
# leur tolérance ; le calcul d'ici, sur l'ajustement de cette session, reste serré)
verifier("GAM : la courbe de x à sa moyenne, la covariance bayésienne de mgcv (Vp), ses ddl résiduels ; = emmeans et la matrice lpmatrix de R ici",
         proche(rG$moyennes$estimation, EMMEANS$G$emmean, 1e-3) && proche(rG$moyennes$erreur_type, EMMEANS$G$SE, 1e-3) && proche(rG$moyennes$ddl, EMMEANS$G$df, 1e-3) &&
           proche(rG$moyennes$ddl, rep(gG$df.residual, 3), 1e-6) && proche(rG$moyennes$estimation, hG$est, 1e-5) && proche(rG$moyennes$erreur_type, hG$se, 1e-4) &&
           isTRUE(rG$grille$covariables$courbe[rG$grille$covariables$nom == "x"]))
dH <- definir_modele(pq, "GAM", x = uq("x", "annee", "secteur"), y = uq("densite"), echelle = "none", famille = "gaussienne", croises = list(uq("annee", "secteur")),
                     lisses = uq("x"), aleatoires = list(site_q), correlation = car1_q)
mH <- ajuster(pq, dH)
rH <- moyennes_ajustees(pq, mH, dH)
gH <- mgcv::gamm(stats::as.formula("densite ~ s(x, k = 10) + annee * secteur", env = env_mgcv), random = list(site = ~1),
                 correlation = nlme::corCAR1(form = ~ annee | site / secteur), data = tq, method = "REML")
hH <- a_la_main(stats::predict(gH$gam, grilleG, type = "lpmatrix"), as.list(1:3), coef(gH$gam), vcov(gH$gam), gH$gam$df.residual)
verifier("mgcv::gamm (CAR1) : sa partie gam, = emmeans et le calcul d'ici", isTRUE(mH$gamm) && proche(rH$moyennes$estimation, EMMEANS$H$emmean, 1e-3) &&
           proche(rH$moyennes$erreur_type, EMMEANS$H$SE, 1e-3) && proche(rH$moyennes$ddl, EMMEANS$H$df, 1e-3) &&
           proche(rH$moyennes$estimation, hH$est, 1e-4) && proche(rH$moyennes$erreur_type, hH$se, 1e-3) && proche(rH$moyennes$ddl, rep(gH$gam$df.residual, 3), 1e-4))
# une courbe par secteur (s(annee, by = secteur)) : la grille porte le facteur de la courbe
dI <- definir_modele(pq, "GAM", x = uq("annee", "secteur"), y = uq("densite"), echelle = "none", famille = "gaussienne",
                     lisses = uq("annee"), par_courbes = stats::setNames(list(uq("secteur")), uq("annee")))
mI <- ajuster(pq, dI)
rI <- moyennes_ajustees(pq, mI, dI)
kI <- unlist(mI$k)[1]  # (la dimension que Nuée a prise pour la courbe : dimension_courbe)
gI <- mgcv::gam(stats::as.formula(sprintf("densite ~ secteur + s(annee, by = secteur, k = %d)", kI), env = env_mgcv),
                data = transform(tq, secteur = factor(secteur)), method = "REML")
grilleI <- data.frame(secteur = factor(c("S1", "S2", "S3"), levels = sort(unique(tq$secteur))), annee = mean(tq$annee))
hI <- a_la_main(stats::predict(gI, grilleI, type = "lpmatrix"), as.list(1:3), coef(gI), vcov(gI), gI$df.residual)
verifier(sprintf("une courbe par secteur : la grille porte le facteur de la courbe ; = la matrice lpmatrix de mgcv (%s)",
                 paste(signif(rI$moyennes$estimation, 4), collapse = ", ")),
         proche(rI$moyennes$estimation, hI$est, 1e-5) && proche(rI$moyennes$erreur_type, hI$se, 1e-4))

cat("Un carré et un terme croisé de deux nombres\n")
dI <- definir_modele(pq, "LM", x = uq("x", "annee", "secteur"), y = uq("densite"), echelle = "UV", croises = list(uq("x", "annee"), uq("x", "x"), uq("x", "secteur")))
mI <- ajuster(pq, dI)
rI <- moyennes_ajustees(pq, mI, dI)
lI <- lm(densite ~ x * annee + I(x^2) + secteur + x:secteur, data = tq)
grilleI <- data.frame(secteur = c("S1", "S2", "S3"), x = mean(tq$x), annee = mean(tq$annee))
hI <- a_la_main(model.matrix(~ x * annee + I(x^2) + secteur + x:secteur, grilleI), as.list(1:3), coef(lI), vcov(lI), lI$df.residual)
verifier("x × annee et x² au produit des moyennes (x̄ · ānnee, x̄²), = emmeans et le calcul d'ici", proche(rI$moyennes$estimation, EMMEANS$I$emmean, 1e-7) &&
           proche(rI$moyennes$estimation, hI$est, 1e-8))
rIp <- moyennes_ajustees(pq, mI, dI, list(pente = uq("x")))
verifier("la pente de x par secteur, avec x² et x × annee : la différence avant d'emtrends (un millième de l'étendue de x), = emmeans",
         proche(rIp$moyennes$estimation, EMMEANS$I$trend, 1e-7) && proche(rIp$moyennes$erreur_type, EMMEANS$I$trend_SE, 1e-7) && proche(rIp$paires$p, EMMEANS$I$trend_p, 1e-6) &&
           proche(rIp$pente$delta, 0.001 * diff(range(tq$x)), 1e-12))

cat("Une combinaison jamais observée\n")
tz <- tq[!(tq$habitat == "H2" & tq$traitement == "T3"), ]
pz <- projet_depuis_tableau(tz[c("nom", "habitat", "traitement", "densite")], "cellule vide")
dJ <- definir_modele(pz, "LM", x = numeros(pz, c("habitat", "traitement")), y = numeros(pz, "densite"), echelle = "none", croises = list(numeros(pz, c("habitat", "traitement"))))
mJ <- ajuster(pz, dJ)
rJ <- moyennes_ajustees(pz, mJ, dJ, list(facteur = numeros(pz, "traitement")))
e <- EMMEANS$J
verifier("T3 (sans observation dans H2) n'est pas estimable : laissée de côté avec ses paires ; T1 − T2, seule paire, Tukey pour 2 moyennes (= emmeans)",
         identical(rJ$moyennes$niveau, c("T1", "T2")) && proche(rJ$moyennes$estimation, e$emmean) && identical(as.character(rJ$non_estimables$moyennes), "T3") &&
           identical(as.character(rJ$non_estimables$paires), c("T1 − T3", "T2 − T3")) && proche(rJ$paires$estimation, e$estimate) && proche(rJ$paires$p, e$p, 1e-6) &&
           proche(rJ$paires$bas, e$lower, 1e-4) && rJ$paires$famille == 2)
rJd <- moyennes_ajustees(pz, mJ, dJ, list(facteur = numeros(pz, "traitement"), dans = numeros(pz, "habitat")))
verifier("dans chaque habitat : la combinaison T3 · H2 laissée de côté ; H1 (3 paires, Tukey pour 3) et H2 (1 paire, Tukey pour 2) = emmeans",
         nrow(rJd$moyennes) == 5 && identical(as.character(rJd$non_estimables$moyennes), "T3 · H2") && identical(rJd$paires$groupe, c("H1", "H1", "H1", "H2")) &&
           proche(rJd$paires$estimation, e$simple_estimate) && proche(rJd$paires$p, e$simple_p, 1e-6) && proche(rJd$paires$bas, e$simple_lower, 1e-4) &&
           identical(as.numeric(rJd$paires$famille), c(3, 3, 3, 2)))

cat("Refus\n")
d0 <- definir_modele(pq, "LM", x = uq("x", "annee"), y = uq("densite"))
verifier("un modèle sans variable qualitative", grepl("pas de variable qualitative", refus(moyennes_ajustees(pq, ajuster(pq, d0), d0))))
verifier("« dans » une variable qui n'est pas croisée avec la première", grepl("n'est croisée avec « secteur » dans aucun terme", refus(moyennes_ajustees(pq, mC, dC, list(dans = uq("habitat"))))))
verifier("la pente d'une variable qui n'est pas croisée avec elle", grepl("« x » n'est pas une variable quantitative croisée avec « secteur »", refus(moyennes_ajustees(pq, mA, dA, list(pente = uq("x"))))))
verifier("un ajustement ou une échelle inconnus", grepl("Ajustement inconnu", refus(moyennes_ajustees(pq, mA, dA, list(ajustement = "sidak")))) &&
           grepl("Échelle inconnue", refus(moyennes_ajustees(pq, mA, dA, list(echelle = "probabilite")))))
p_change <- modifier_valeur(pq, numeros_observations(pq)[1], uq("annee"), "1999")
verifier("des données changées depuis l'ajustement", grepl("ont changé depuis l'ajustement", refus(moyennes_ajustees(p_change, mC, dC))))

cat("Phrases et texte des Méthodes\n")
verifier("la phrase d'une paire, en français", identical(as.character(rA$phrases[1]),
  "Moyennes ajustées de « densite » par « secteur » (régression linéaire) : S1 − S2 = -1,48 [IC 95 % -1,90 ; -1,06], t(175) = -8,31, p < 0,001 (Tukey, 3 moyennes) ; moyennes S1 = 2,81 et S2 = 4,28."))
ancien <- options(nuee.langue = "en")
rD_en <- moyennes_ajustees(pq, mD, dD, list(facteur = uq("traitement")))
rCp_en <- moyennes_ajustees(pq, mC, dC, list(pente = uq("annee")))
options(ancien)
verifier("en anglais : un rapport de moyennes (z), des pentes", identical(as.character(rD_en$phrases[1]),
  "Adjusted means of “compte” by “traitement” (generalized linear model, Poisson distribution; response scale): T1 / T2 = 0.703 [95% CI 0.520, 0.950], z = -2.74, p = 0.0167 (Tukey, 3 means); means T1 = 1.78 and T2 = 2.53.") &&
           identical(as.character(rCp_en$phrases[1]),
  "Slopes of “annee” by “secteur” (linear mixed model): S1 − S2 = -0.267 [95% CI -0.396, -0.138], t(169) = -4.89, p < 0.001 (Tukey, 3 slopes); slopes S1 = 0.0348 and S2 = 0.302 per unit of “annee”."))
verifier("dans chaque habitat, avec Holm (l'intervalle aux décimales de l'estimation)", identical(as.character(rBh$phrases[4]),
  "Moyennes ajustées de « densite » par « traitement » dans « habitat » = H2 (régression linéaire) : T1 − T2 = -0,0858 [IC 95 % -0,9513 ; 0,7796], t(173) = -0,24, p = 1 (Holm, 3 paires) ; moyennes T1 = 3,07 et T2 = 3,15."))
mC$moyennes <- rC
tm <- texte_methodes(pq, dC, mC, "fr")
par_moy <- tm$paragraphes[grepl("Searle", tm$paragraphes)]
verifier("texte des Méthodes (modèle mixte, Tukey) : la grille, la covariance, le confinement (6 sites), Tukey et Kramer ; leurs références",
         length(par_moy) == 1 && grepl("« annee » à sa moyenne sur les observations du modèle (2006,1)", par_moy, fixed = TRUE) &&
           grepl("le nombre de « site » moins un (5)", par_moy, fixed = TRUE) && grepl("Tukey, 1953 ; Kramer, 1956", par_moy, fixed = TRUE) &&
           all(vapply(c("^Searle", "^Lenth", "^Tukey", "^Kramer", "^Pinheiro, J. C."), function(x) any(grepl(x, tm$references)), TRUE)))
mD$moyennes <- rD
mB$moyennes <- rBh
tD <- texte_methodes(pq, dD, mD, "en")
tB <- texte_methodes(pq, dB, mB, "fr")
verifier("en anglais, l'échelle de la réponse (z, des rapports) ; dans chaque habitat avec Holm : Bonferroni et la référence de Holm",
         any(grepl("ratios of means, tested on the link scale", tD$paragraphes)) && any(grepl("normal distribution (z, asymptotic)", tD$paragraphes, fixed = TRUE)) &&
           any(grepl("méthode de Holm (1979) et les intervalles par celle de Bonferroni", tB$paragraphes, fixed = TRUE)) && any(grepl("^Holm, S.", tB$references)) &&
           any(grepl("de « traitement » dans chaque catégorie de « habitat »", tB$paragraphes, fixed = TRUE)) && !any(grepl("^Kramer", tB$references)))
dL <- definir_modele(pq, "LM", x = uq("secteur"), y = uq("densite"), echelle = "none", transformations = stats::setNames("ln:1:1", uq("densite")))
mL <- ajuster(pq, dL)
mL$moyennes <- moyennes_ajustees(pq, mL, dL)
verifier("une réponse en log : les moyennes restent sur son échelle, et une moyenne de logarithmes ramenée est une médiane",
         isTRUE(mL$moyennes$log_y) && identical(mL$moyennes$echelle, "identite") &&
           any(grepl("ln(« densite » + 1) : ramenée sur l'échelle de la variable, une moyenne de logarithmes serait une médiane", texte_methodes(pq, dL, mL, "fr")$paragraphes, fixed = TRUE)))

if (avec_emmeans) {
  cat(sprintf("emmeans %s lui-même\n", utils::packageVersion("emmeans")))
  suppressMessages(requireNamespace("emmeans"))
  emmeans::emm_options(msg.interaction = FALSE)
  sA <- summary(emmeans::emmeans(lA, ~ secteur))
  sC <- summary(emmeans::emmeans(lC, ~ secteur))
  sD <- summary(graphics::pairs(emmeans::emmeans(glm(compte ~ habitat * traitement, family = poisson, data = tq), ~ traitement, type = "response")), infer = TRUE)
  verifier("les valeurs enregistrées d'emmeans refaites (régression linéaire, modèle mixte, rapports de Poisson)",
           proche(sA$emmean, EMMEANS$A$emmean, 1e-9) && proche(sC$emmean, EMMEANS$C$emmean, 1e-5) && proche(sC$df, EMMEANS$C$df) && proche(sD$ratio, EMMEANS$D$ratio, 1e-9))
} else cat("(emmeans n'est pas installé : NUEE_EMMEANS désigne une bibliothèque où il l'est ; les valeurs enregistrées font foi)\n")

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
