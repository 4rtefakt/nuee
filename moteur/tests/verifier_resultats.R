# Vérifie deux façons de lire les résultats des régressions :
#   - les coefficients côte à côte (moteur/R/coefficients_modeles.R) : chaque case recalculée ici
#     d'après lm(), nlme::lme(), glm() et mgcv::gam() (l'estimation, son intervalle à 95 % ou son
#     erreur-type, aux décimales de deux chiffres significatifs de l'erreur-type), les lignes dans
#     l'ordre où elles viennent, les cases vides, n, AIC, R², σ, edf, les étoiles des p, l'échelle du
#     lien, l'anglais, les refus, l'export relu ;
#   - la puissance pour détecter une tendance (moteur/R/puissance.R) : les variances lues contre
#     nlme::lme(), la variance de la pente contre (X' V⁻¹ X)⁻¹ calculé ici sur le V de tout le plan
#     (ordonnées à l'origine aléatoires emboîtées, CAR1 dans chaque série, ou des répétitions ; une
#     régression linéaire), la puissance par la loi normale, la conversion des % pour log10 et ln, le
#     plan des données, la simulation contre la formule à l'erreur de Monte-Carlo près sur un modèle
#     mixte simple, ses tirages refaits à l'identique et le hasard de la session rendu, les refus
#     (GLM, pentes aléatoires, un temps qui n'est pas un X quantitatif, des unités de temps
#     différentes, les limites), le calcul gardé, la phrase et le texte des Méthodes.
#   Rscript moteur/tests/verifier_resultats.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-8) isTRUE(all.equal(as.vector(unclass(a)), as.vector(unclass(b)), tolerance = tol, check.attributes = FALSE))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

cat("Coefficients côte à côte\n")
p <- lire_projet("exemple/vergers.json")
obs <- numeros_observations(p)
u <- function(...) numeros(p, c(...))
col <- function(nom) colonne(p, numeros(p, nom), obs)
df <- data.frame(note = col("note_gout"), sucre = col("sucre"), fermete = col("fermete"), acidite = col("acidite"), stockage = col("stockage"),
                 verger = factor(as.character(variable_du_projet(p, u("verger"))$categories)[col("verger")]))
m1 <- ajuster(p, definir_modele(p, "LM", x = u("sucre", "fermete"), y = u("note_gout"), echelle = "none", nom = "R1"))
m2 <- ajuster(p, definir_modele(p, "LMM", x = u("sucre", "acidite"), y = u("note_gout"), echelle = "none", nom = "R2",
                                aleatoires = list(list(by = "variable", variable = u("verger")))))
m3 <- ajuster(p, definir_modele(p, "GLM", x = u("sucre"), y = u("stockage"), echelle = "none", famille = "poisson", nom = "R3"))
m4 <- ajuster(p, definir_modele(p, "GAM", x = u("sucre", "fermete"), y = u("note_gout"), echelle = "none", famille = "gaussienne", lisses = u("sucre"), nom = "R4"))
m5 <- ajuster(p, definir_modele(p, "LM", x = u("sucre", "variete"), y = u("note_gout"), echelle = "none", nom = "R5"))
l1 <- lm(note ~ sucre + fermete, data = df)
ctrl <- nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE)
l2 <- nlme::lme(note ~ sucre + acidite, random = ~ 1 | verger, data = df, method = "REML", control = ctrl)
l2ml <- nlme::lme(note ~ sucre + acidite, random = ~ 1 | verger, data = df, method = "ML", control = ctrl)
g3 <- glm(stockage ~ sucre, family = poisson(), data = df)
g4 <- mgcv::gam(note ~ s(sucre) + fermete, data = df, method = "REML")
# la case attendue, écrite ici : l'estimation et son intervalle (ou son erreur-type), aux décimales de deux chiffres significatifs de l'erreur-type
case <- function(est, et, bas, haut, fr = TRUE, ic = TRUE) {
  d <- max(0, min(8, 1 - floor(log10(et))))
  n <- function(x) formatC(if (round(x, d) == 0) 0 else x, format = "f", digits = d, decimal.mark = if (fr) "," else ".")
  if (!ic) sprintf("%s (%s)", n(est), n(et)) else sprintf(if (fr) "%s [%s ; %s]" else "%s [%s, %s]", n(est), n(bas), n(haut))
}
options(nuee.langue = "fr")
r <- coefficients_cote_a_cote(list(R1 = m1, R2 = m2, R3 = m3, R4 = m4, R5 = m5), "ic")
verifier("les lignes : la constante et les termes du premier modèle, puis ceux des suivants qui n'y sont pas, dans leur ordre",
         identical(as.character(r$termes), c("(Intercept)", "sucre", "fermete", "acidite", m5$resultat$coefficients$terme[-(1:2)])) && length(r$modeles) == 5)
s1 <- summary(l1)$coefficients
ic1 <- confint(l1)
attendu1 <- vapply(1:3, function(k) case(s1[k, 1], s1[k, 2], ic1[k, 1], ic1[k, 2]), "")
verifier(sprintf("régression linéaire : chaque case = lm() et confint() (%s)", attendu1[2]), identical(r$cellules[1:3, 1], attendu1))
t2 <- summary(l2)$tTable
q2 <- qt(0.975, t2[, "DF"])
attendu2 <- vapply(1:3, function(k) case(t2[k, 1], t2[k, 2], t2[k, 1] - q2[k] * t2[k, 2], t2[k, 1] + q2[k] * t2[k, 2]), "")
verifier("modèle mixte : nlme::lme() en REML, l'intervalle de t à ses ddl ; vide pour fermete, qu'il n'a pas",
         identical(r$cellules[c(1, 2, 4), 2], attendu2) && r$cellules[3, 2] == "" && is.na(r$p[3, 2]))
s3 <- summary(g3)$coefficients
attendu3 <- vapply(1:2, function(k) case(s3[k, 1], s3[k, 2], s3[k, 1] - qnorm(0.975) * s3[k, 2], s3[k, 1] + qnorm(0.975) * s3[k, 2]), "")
verifier("GLM de Poisson : glm(), l'intervalle de la loi normale (l'échelle n'est pas estimée)", identical(r$cellules[1:2, 3], attendu3))
verifier("les p : ceux de lm(), nlme::lme(), glm() et mgcv::gam() ; NA sans le terme",
         proche(r$p[1:3, 1], s1[, 4]) && proche(r$p[c(1, 2, 4), 2], t2[, "p-value"]) && proche(r$p[1:2, 3], s3[, 4]) &&
           proche(r$p[c(1, 3), 4], summary(g4)$p.table[, 4]) && all(is.na(r$p[c(3, 4), 3])))
v <- setNames(as.list(r$valeurs[, 1]), r$statistiques)
nb <- function(x, d) formatC(x, format = "f", digits = d, decimal.mark = ",")
vc2 <- as.numeric(nlme::VarCorr(l2)[1, "Variance"])
verifier("n, AIC, AICc et R² d'une régression linéaire : ceux de lm()",
         identical(v$n, "120") && identical(v$AIC, nb(AIC(l1), 1)) && identical(v[["R²"]], nb(summary(l1)$r.squared, 3)) &&
           identical(v$AICc, nb(AIC(l1) + 2 * 4 * 5 / (120 - 4 - 1), 1)))
k <- function(nom) match(nom, r$statistiques)
verifier(sprintf("modèle mixte : AIC de l'ajustement en ML, R² marginal et conditionnel, σ de chaque effet aléatoire et résiduel (%s)", r$valeurs[k("σ verger"), 2]),
         identical(r$valeurs[k("AIC"), 2], nb(AIC(l2ml), 1)) && identical(r$valeurs[k("R² marginal"), 2], nb(m2$resultat$r2$marginal, 3)) &&
           identical(r$valeurs[k("σ verger"), 2], formatC(sqrt(vc2), format = "f", digits = 2 - floor(log10(sqrt(vc2))), decimal.mark = ",")) &&
           identical(r$valeurs[k("σ résiduelle"), 2], formatC(l2$sigma, format = "f", digits = 2 - floor(log10(l2$sigma)), decimal.mark = ",")) &&
           r$valeurs[k("σ verger"), 1] == "" && r$valeurs[k("R²"), 2] == "")
verifier("GLM et GAM : la part de la déviance expliquée (1 − déviance / déviance nulle), l'edf de la courbe de mgcv",
         identical(r$valeurs[k("Déviance expliquée"), 3], paste(nb(100 * (1 - g3$deviance / g3$null.deviance), 1), "%")) &&
           identical(r$valeurs[k("edf sucre"), 4], nb(summary(g4)$s.table[1, 1], 2)) && r$valeurs[k("edf sucre"), 1] == "")
verifier("les étoiles des p, comme symnum de R : *** jusqu'à 0,001, ** jusqu'à 0,01, * jusqu'à 0,05",
         identical(etoiles_p(c(0.0005, 0.001, 0.005, 0.01, 0.03, 0.05, 0.051, NA)), c("***", "***", "**", "**", "*", "*", "", "")) &&
           identical(r$copie$lignes[2, 2], paste0(r$cellules[2, 1], etoiles_p(r$p[2, 1]))))
verifier("la copie : l'en-tête (le format des cases, puis les modèles), les termes et les lignes de l'ajustement, la légende des étoiles",
         identical(as.character(r$copie$entetes), c("Estimation [IC 95 %]", "R1", "R2", "R3", "R4", "R5")) &&
           nrow(r$copie$lignes) == length(r$termes) + length(r$statistiques) && r$copie$lignes[length(r$termes) + 1, 1] == "n" &&
           r$copie$notes[1] == "* p ≤ 0,05 ; ** p ≤ 0,01 ; *** p ≤ 0,001")
verifier(sprintf("l'échelle du lien : dite pour le modèle de Poisson (« %s »)", r$note_lien),
         grepl("^R3 \\(lien log\\) : estimations sur l'échelle du lien", r$note_lien) && grepl("effet multiplicatif", r$note_lien) &&
           identical(as.character(r$copie$notes[2]), r$note_lien) && is.null(coefficients_cote_a_cote(list(R1 = m1, R2 = m2))$note_lien))
options(nuee.langue = "en")
re <- coefficients_cote_a_cote(list(R1 = m1, R2 = m2), "et")
verifier(sprintf("en anglais, avec l'erreur-type : %s", re$cellules[2, 1]),
         identical(re$cellules[1:3, 1], vapply(1:3, function(k) case(s1[k, 1], s1[k, 2], NA, NA, fr = FALSE, ic = FALSE), "")) &&
           re$entete == "Estimate (SE)" && "Marginal R²" %in% re$statistiques && "σ residual" %in% re$statistiques)
options(nuee.langue = "fr")
mp <- ajuster(p, definir_modele(p, "PCA", x = u("sucre", "fermete", "acidite"), A = 2))
verifier("refusés : aucun modèle, ou un modèle qui n'est pas une régression",
         grepl("Cochez au moins une régression", refus(coefficients_cote_a_cote(list()))) &&
           grepl("Seules des régressions", refus(coefficients_cote_a_cote(list(R1 = m1, A = mp)))))
dossier <- tempfile("resultats")
dir.create(dossier)
tc <- tables_cote_a_cote(list(R1 = m1, R2 = m2), "ic")
f <- ecrire_tables(tc, file.path(dossier, "coefficients.xlsx"), objet = "coefficients")
relu <- openxlsx::read.xlsx(f, sheet = 1, colNames = TRUE, skipEmptyRows = FALSE, sep.names = " ")
relu[is.na(relu)] <- ""
nombres <- openxlsx::read.xlsx(f, sheet = 2)
cote <- coefficients_cote_a_cote(list(R1 = m1, R2 = m2), "ic")$copie
verifier("l'export : le tableau tel qu'il se copie (avec les étoiles, puis la légende), et les coefficients de chaque modèle en nombres",
         identical(names(relu), as.character(cote$entetes)) && identical(unname(as.matrix(relu[seq_len(nrow(cote$lignes)), ])), unname(cote$lignes)) &&
           relu[nrow(relu), 1] == cote$notes[1] && identical(nombres$modele, rep(c("R1", "R2"), c(3, 3))) &&
           proche(nombres$estimation, c(s1[, 1], t2[, 1]), 1e-6) && proche(nombres$p, c(s1[, 4], t2[, "p-value"]), 1e-6))
fc <- ecrire_tables(tc, file.path(dossier, "coefficients.csv"), objet = "coefficients")
verifier("en CSV : un fichier par tableau", length(fc) == 2 && all(file.exists(fc)))

cat("Puissance pour détecter une tendance\n")
# 8 sites (6 de 3 secteurs, 2 de 2 : 2,75 en moyenne ; les noms des secteurs se répètent d'un site
# à l'autre), suivis de 2001 à 2012 ; une baisse, un effet du site et du secteur, un bruit
# autocorrélé dans chaque secteur ; une densité, et son jour (365 par an) pour un temps d'autres unités
set.seed(21)
sites <- sprintf("S%d", 1:8)
lignes <- do.call(rbind, lapply(seq_along(sites), function(k) expand.grid(annee = 2001:2012, secteur = c("a", "b", "c")[seq_len(if (k <= 6) 3 else 2)],
                                                                          site = sites[k], stringsAsFactors = FALSE)))
serie <- paste(lignes$site, lignes$secteur)
bruit <- unlist(lapply(split(seq_len(nrow(lignes)), factor(serie, unique(serie))), function(i) as.vector(stats::filter(rnorm(length(i), 0, 0.15), 0.4, "recursive"))))
lignes$densite <- round(10^(1.6 - 0.012 * (lignes$annee - 2000) + rnorm(8, 0, 0.25)[match(lignes$site, sites)] +
                            rnorm(length(unique(serie)), 0, 0.1)[match(serie, unique(serie))] + bruit), 3)
lignes$temperature <- round(rnorm(nrow(lignes), 12, 2), 2)
lignes$jour <- (lignes$annee - 2000) * 365 + 120
pt <- projet_depuis_tableau(data.frame(nom = sprintf("o%03d", seq_len(nrow(lignes))), lignes[sample(nrow(lignes)), ]), "suivi")
ut <- function(...) numeros(pt, c(...))
site <- list(by = "variable", variable = ut("site"))
secteur <- list(by = "variable", variable = ut("secteur"))
car1 <- function(temps = "annee", dans = NULL) c(list(type = "CAR1", temps = list(by = "variable", variable = ut(temps))), if (!is.null(dans)) list(dans = dans))
log10_densite <- stats::setNames(list("log10"), ut("densite"))
dc <- definir_modele(pt, "LMM", x = ut("annee", "temperature"), y = ut("densite"), echelle = "UV", aleatoires = list(site, secteur), correlation = car1(),
                     transformations = log10_densite)
mc <- ajuster(pt, dc)
lu <- lire_modele_puissance(mc, dc)
dt <- data.frame(y = log10(lignes$densite), annee = lignes$annee, temperature = lignes$temperature, site = lignes$site, secteur = lignes$secteur)
rc <- eval(bquote(nlme::lme(y ~ annee + temperature, random = ~ 1 | site / secteur, correlation = nlme::corCAR1(form = ~ annee | site / secteur),
                            data = dt, method = "REML", control = .(ctrl))))
verifier(sprintf("les variances lues : celles de nlme::lme() en REML (σ² = %.4f, φ = %.3f)", lu$sigma2, lu$phi),
         proche(lu$sigma2, rc$sigma^2, 1e-5) && proche(lu$phi, coef(rc$modelStruct$corStruct, unconstrained = FALSE), 1e-4) &&
           proche(lu$variances, as.numeric(nlme::VarCorr(rc)[c(2, 4), "Variance"]), 1e-4) && identical(lu$noms, c("annee", "temperature")))
rpc <- puissance_tendance(mc, dc, list(annees = c(5, 20), unites = c(4, 12)))
verifier("le plan des données : 8 sites, 12 ans, 2,75 secteurs par site (3 dans les plans), une observation par série et par an ; le temps : annee",
         rpc$plan$unites == 8 && rpc$plan$annees == 12 && proche(rpc$plan$moyennes, 22 / 8) && rpc$plan$emboites == 3 && rpc$plan$repetitions == 1 &&
           rpc$reglages$terme == "annee" && identical(as.integer(rpc$reglages$unites), c(4L, 8L, 12L)))
# (X' V⁻¹ X)⁻¹ sur le V de tout le plan, écrit ici : S sites de `secteurs` séries comptées aux temps 1..T
variance_gls <- function(S, T, secteurs, v_site, v_secteur, s2, phi, repetitions = 1) {
  pl <- expand.grid(rep = seq_len(repetitions), t = seq_len(T), secteur = seq_len(secteurs), site = seq_len(S))
  meme <- function(x) outer(x, x, "==")
  serie <- paste(pl$site, pl$secteur)
  V <- v_site * meme(pl$site) + v_secteur * meme(serie) + s2 * (if (phi > 0) meme(serie) * phi^abs(outer(pl$t, pl$t, "-")) else diag(nrow(pl)))
  X <- cbind(1, pl$t)
  solve(t(X) %*% solve(V, X))[2, 2]
}
et <- function(r, a, s) r$tableau$erreur_type[r$tableau$annees == a & r$tableau$unites == s]
verifier(sprintf("la variance de la pente = (X' V⁻¹ X)⁻¹ sur V entier : site et secteur aléatoires, CAR1 dans chaque secteur (4 sites, 9 ans : %.3g)", et(rpc, 9, 4)^2),
         proche(et(rpc, 9, 4)^2, variance_gls(4, 9, 3, lu$variances[1], lu$variances[2], lu$sigma2, lu$phi)) &&
           proche(et(rpc, 13, 12)^2, variance_gls(12, 13, 3, lu$variances[1], lu$variances[2], lu$sigma2, lu$phi)))
verifier("les ordonnées à l'origine aléatoires n'y changent rien : la même variance avec des variances d'effets aléatoires multipliées par 100",
         proche(variance_gls(4, 9, 3, 100 * lu$variances[1], 100 * lu$variances[2], lu$sigma2, lu$phi), variance_gls(4, 9, 3, 0, 0, lu$sigma2, lu$phi)))
z <- qnorm(0.975)
ici <- rpc$tableau
verifier("la puissance : Φ(|β|/et − z) + Φ(−|β|/et − z), β = log10(0,97) (une baisse de 3 % par an), et ses observations",
         proche(rpc$pente, log10(0.97)) && rpc$reglages$unite == "pourcent" && rpc$reglages$tendance == -3 &&
           proche(ici$puissance, pnorm(abs(rpc$pente) / ici$erreur_type - z) + pnorm(-abs(rpc$pente) / ici$erreur_type - z)) &&
           identical(as.numeric(ici$observations), as.numeric(ici$annees * ici$unites * 3)))
a80 <- min(ici$annees[ici$unites == 8 & ici$puissance >= 0.8])
verifier(sprintf("la phrase, au nombre de sites des données : 80 %% après %d ans (« %s »)", a80, rpc$phrase),
         grepl(sprintf("^Avec 8 unités de « site » \\(3 « secteur » dans chacune\\), chacune comptée chaque année, une baisse de 3 %% par an de « densite » est détectée avec une puissance de 80 %% après %d ans \\(α = 0,05, test bilatéral\\)\\.$", a80), rpc$phrase) &&
           rpc$annees_80$annees[rpc$annees_80$unites == 8] == a80)
# CAR1 dans chaque secteur, un niveau sans effet aléatoire (dans) ; des répétitions sans CAR1 ; une régression linéaire
dd <- definir_modele(pt, "LMM", x = ut("annee"), y = ut("densite"), echelle = "none", aleatoires = list(site), correlation = car1(dans = secteur), transformations = log10_densite)
md <- ajuster(pt, dd)
rd <- puissance_tendance(md, dd, list(annees = c(5, 15), unites = c(4, 8)))
ld <- lire_modele_puissance(md, dd)
verifier("CAR1 dans chaque secteur, sans effet aléatoire du secteur : 3 groupes de CAR1 par site, (X' V⁻¹ X)⁻¹ sur V entier",
         rd$plan$dans == 3 && proche(rd$plan$moyenne_dans, 2.75) && length(rd$plan$emboites) == 0 &&
           proche(et(rd, 7, 4)^2, variance_gls(4, 7, 3, ld$variances[1], 0, ld$sigma2, ld$phi)))
dr <- definir_modele(pt, "LMM", x = ut("annee"), y = ut("densite"), echelle = "none", aleatoires = list(site), transformations = log10_densite)
mr <- ajuster(pt, dr)
rr <- puissance_tendance(mr, dr, list(annees = c(5, 15), unites = c(4, 8)))
lr <- lire_modele_puissance(mr, dr)
verifier("sans CAR1, les secteurs d'un site en répétitions (2,75 par an, 3 dans les plans) : (X' V⁻¹ X)⁻¹ sur V entier",
         rr$plan$repetitions == 3 && proche(rr$plan$moyenne_repetitions, 2.75) && is.null(rr$variances$phi) &&
           proche(et(rr, 6, 4)^2, variance_gls(4, 6, 1, lr$variances[1], 0, lr$sigma2, 0, repetitions = 3)))
dl <- definir_modele(pt, "LM", x = ut("annee", "temperature"), y = ut("densite"), echelle = "none", transformations = stats::setNames(list("ln:1:0"), ut("densite")))
ml <- ajuster(pt, dl)
rl <- puissance_tendance(ml, dl, list(annees = c(4, 10), unites = c(10, 22)))
ll <- lm(log(densite) ~ annee + temperature, data = lignes)
verifier(sprintf("régression linéaire (ln) : β = ln(0,97), %d observations par an, et σ² / (n Σ(t − t̄)²) avec σ² de lm()", rl$plan$unites),
         proche(rl$pente, log(0.97)) && rl$plan$unites == 22 && proche(rl$variances$sigma2, summary(ll)$sigma^2) &&
           proche(et(rl, 7, 10)^2, summary(ll)$sigma^2 / (10 * sum((1:7 - 4)^2))) && grepl("^Avec 22 observations par an, une baisse de 3 % par an", rl$phrase))
# la conversion des % : log10(x + 1) (une approximation), et refusée sans logarithme
d1 <- definir_modele(pt, "LMM", x = ut("annee"), y = ut("densite"), echelle = "none", aleatoires = list(site), transformations = stats::setNames(list("log10:1"), ut("densite")))
r1 <- puissance_tendance(ajuster(pt, d1), d1, list(tendance = 2.5))
d0 <- definir_modele(pt, "LMM", x = ut("annee"), y = ut("densite"), echelle = "none", aleatoires = list(site))
m0 <- ajuster(pt, d0)
r0 <- puissance_tendance(m0, d0)
verifier(sprintf("%% par an : log10(1 + 2,5/100) pour log10(densite + 1), dit approché ; sans logarithme, une pente par défaut (%s) et le %% refusé", r0$reglages$tendance),
         proche(r1$pente, log10(1.025)) && isTRUE(r1$approchee) && grepl("une hausse de 2,5 % par an", r1$phrase) &&
           r0$reglages$unite == "pente" && proche(r0$reglages$tendance, -signif(0.03 * mean(m0$donnees$y), 2)) && !r0$pourcent_possible &&
           grepl("demande une réponse en log10 ou en ln", refus(puissance_tendance(m0, d0, list(unite = "pourcent")))))
options(nuee.langue = "en")
ren <- puissance_tendance(mc, dc, list(unite = "pente", tendance = -0.01, annees = c(5, 20)))
verifier(sprintf("en anglais, une pente dans les unités de Y : « %s »", ren$phrase),
         proche(ren$pente, -0.01) && grepl("^With 8 units of “site” \\(3 “secteur” in each\\), each counted every year, a decline of 0.01 per year in log10\\(“densite”\\)", ren$phrase))
options(nuee.langue = "fr")

# la simulation contre la formule, sur un modèle mixte simple : 20 sites, un comptage par an, sans CAR1
set.seed(5)
simple <- expand.grid(annee = 1:10, site = sprintf("T%02d", 1:20), stringsAsFactors = FALSE)
simple$y <- 2 - 0.02 * simple$annee + rnorm(20, 0, 0.3)[match(simple$site, unique(simple$site))] + rnorm(nrow(simple), 0, 0.2)
ps <- projet_depuis_tableau(data.frame(nom = paste0("s", seq_len(nrow(simple))), simple), "simple")
ds <- definir_modele(ps, "LMM", x = numeros(ps, "annee"), y = numeros(ps, "y"), echelle = "none", aleatoires = list(list(by = "variable", variable = numeros(ps, "site"))))
ms <- ajuster(ps, ds)
s2 <- lire_modele_puissance(ms, ds)$sigma2
beta <- -signif(2.2 * sqrt(s2 / (20 * sum((1:6 - 3.5)^2))), 3)  # (une puissance d'environ 0,6 à 20 sites et 6 ans)
debut <- proc.time()[["elapsed"]]
rs <- puissance_tendance(ms, ds, list(unite = "pente", tendance = beta, simuler = TRUE, simulation = list(annees = 6, unites = 20, n = 200)))
sim <- rs$simulation
verifier(sprintf("la simulation (200 jeux, %.1f s) s'accorde à la formule à l'erreur de Monte-Carlo près : %.3f ± %.3f contre %.3f",
                 proc.time()[["elapsed"]] - debut, sim$puissance, sim$erreur_mc, sim$analytique),
         sim$n == 200 && sim$echecs == 0 && abs(sim$puissance - sim$analytique) <= 3 * sim$erreur_mc + 0.01 && sim$analytique > 0.4 && sim$analytique < 0.8 &&
           proche(sim$erreur_mc, sqrt(sim$puissance * (1 - sim$puissance) / 200)) && proche(sim$puissance, mean(sim$p <= 0.05)))
# un jeu refait ici : les tirages dans l'ordre dit (l'effet de chaque site, puis les erreurs), le même p
set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
jeu <- expand.grid(temps = 1:6, site = factor(1:20))
jeu$y <- beta * jeu$temps + sqrt(lire_modele_puissance(ms, ds)$variances) * rnorm(20)[as.integer(jeu$site)] + sqrt(s2) * rnorm(120)
p1 <- summary(nlme::lme(y ~ temps, random = ~ 1 | site, data = jeu, method = "REML", control = ctrl))$tTable["temps", "p-value"]
verifier("le premier jeu simulé, refait ici avec rnorm() dans l'ordre dit, donne le même p", proche(sim$p[1], p1, 1e-6))
set.seed(99)
RNGkind("L'Ecuyer-CMRG")
set.seed(99)
attendu <- runif(2)
set.seed(99)
rs2 <- puissance_tendance(ms, ds, list(unite = "pente", tendance = beta, simuler = TRUE, simulation = list(annees = 6, unites = 20, n = 20)))
apres <- runif(2)
verifier("une graine fixe : les mêmes p d'un calcul à l'autre ; le hasard de la session et son générateur rendus",
         identical(rs2$simulation$p, sim$p[1:20]) && identical(attendu, apres) && RNGkind()[1] == "L'Ecuyer-CMRG")
RNGkind("default", "default", "default")
# le calcul gardé : la simulation reste tant qu'elle porte sur les mêmes réglages
garde <- puissance_tendance(ms, ds, list(unite = "pente", tendance = beta, annees = c(3, 12), unites = c(10, 30), simulation = list(annees = 6, unites = 20, n = 200)), avant = rs)
autre <- puissance_tendance(ms, ds, list(unite = "pente", tendance = 2 * beta, simulation = list(annees = 6, unites = 20, n = 200)), avant = rs)
verifier("gardée avec les mêmes réglages (d'autres courbes), refaite seulement sur demande quand la tendance change ; la durée d'un ajustement gardée",
         identical(garde$simulation$p, sim$p) && is.null(autre$simulation) && identical(garde$estimation$par_ajustement, rs$estimation$par_ajustement) &&
           proche(garde$estimation$duree, 200 * rs$estimation$par_ajustement))
# les refus
verifier("refusé : un modèle généralisé (GLM, GAM)", grepl("GLM, GAM", refus(puissance_tendance(ajuster(pt, dg <- definir_modele(pt, "GLM", x = ut("annee"), y = ut("densite"),
                                                                                                              famille = "gamma", echelle = "none")), dg))))
pentes <- mr
pentes$ajustement <- nlme::lme(y ~ t1, random = ~ 1 + t1 | g1, data = mr$donnees, method = "REML", control = ctrl)
verifier("refusé : des pentes aléatoires (la matrice de variance d'un niveau a plus d'une ligne)", grepl("pentes aléatoires", refus(puissance_tendance(pentes, dr))))
dq <- definir_modele(pt, "LM", x = ut("site"), y = ut("densite"), echelle = "none")
dlog <- definir_modele(pt, "LM", x = ut("annee"), y = ut("densite"), echelle = "none", transformations = stats::setNames(list("log10", "log10"), ut("annee", "densite")))
verifier("refusés : pas de X quantitatif, un temps transformé (log10), un temps qui n'est pas un X quantitatif du modèle",
         grepl("pas de temps parmi ses X quantitatifs", refus(puissance_tendance(ajuster(pt, dq), dq))) &&
           grepl("pas de temps parmi ses X quantitatifs", refus(puissance_tendance(ajuster(pt, dlog), dlog))) &&
           grepl("« secteur » n'est pas un X quantitatif", refus(puissance_tendance(mc, dc, list(terme = "secteur")))))
dj <- definir_modele(pt, "LMM", x = ut("annee"), y = ut("densite"), echelle = "none", aleatoires = list(site, secteur), correlation = car1("jour"), transformations = log10_densite)
verifier("refusé : CAR1 le long d'un temps d'autres unités (des jours) que celui de la tendance (des années)",
         grepl("leurs unités diffèrent \\(365 unités de « jour » pour une de « annee »\\)", refus(puissance_tendance(ajuster(pt, dj), dj))))
verifier("refusés : les limites des plans et de la simulation (années, unités, jeux, α, observations par jeu)",
         grepl("de 3 à 100", refus(puissance_tendance(mc, dc, list(annees = c(2, 10))))) &&
           grepl("entiers de 2 à", refus(puissance_tendance(mc, dc, list(unites = c(1, 5))))) &&
           grepl("de 10 à 2000 jeux", refus(puissance_tendance(mc, dc, list(simulation = list(n = 5))))) &&
           grepl("α est entre", refus(puissance_tendance(mc, dc, list(alpha = 0.5)))) &&
           grepl("au plus pour la simulation", refus(puissance_tendance(mc, dc, list(simuler = TRUE, simulation = list(annees = 30, unites = 1000, n = 20))))))

cat("Puissance dans le texte des Méthodes\n")
mc$puissance_tendance <- puissance_tendance(mc, dc, list(annees = c(5, 20), simuler = TRUE, simulation = list(annees = 8, unites = 6, n = 20)))
tm <- texte_methodes(pt, dc, mc, "fr", "essai")
pp <- tm$paragraphes[5]
verifier("le texte des Méthodes : la tendance et sa pente, les plans, la méthode, la phrase, la simulation ; Diggle et al. et Johnson et al. dans les références",
         length(tm$paragraphes) == 6 && grepl("La puissance de détecter une baisse de 3 % par an de « densite » (une pente de -0,0132 par an sur log10(« densite »))", pp, fixed = TRUE) &&
           grepl("moindres carrés généralisés", pp) && grepl("Avec 8 unités de « site »", pp) && grepl("Une simulation de 20 jeux de données", pp) &&
           any(grepl("^Diggle, P. J.", tm$references)) && any(grepl("^Johnson, P. C. D.", tm$references)))
tme <- texte_methodes(pt, dc, mc, "en", "essai")
verifier("en anglais", grepl("The power to detect a decline of 3% per year in “densite”", tme$paragraphes[5]) && grepl("With 8 units of “site”", tme$paragraphes[5]))
mr$puissance_tendance <- rr
verifier("sans simulation : Diggle et al. seuls", !any(grepl("^Johnson", texte_methodes(pt, dr, mr, "fr", "essai")$references)) &&
           any(grepl("^Diggle", texte_methodes(pt, dr, mr, "fr", "essai")$references)))

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
