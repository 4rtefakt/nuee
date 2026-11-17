# Vérifie le texte des résultats (moteur/R/resultats_texte.R) : les nombres qu'il écrit sont ceux
# des modèles (glm(), nlme::lme(), mgcv::gam(), la PLS de l'exemple), sur l'échelle de la réponse
# pour un lien log (un facteur) ou logit (un rapport de cotes), les termes significatifs nommés,
# les courbes et les effets aléatoires, en français et en anglais ; une autre analyse sans texte à
# elle donne ses chiffres ; l'échantillonnage par distances a le sien (sa fonction de détection, avec
# des covariables leurs coefficients, p, la densité). Puis les conditions d'une régression dites en mots
# (moteur/R/verifications_modele.R) : chacune vue où les données la mettent en défaut, et pas
# ailleurs (la séparation d'une présence-absence comprise).
#   Rscript moteur/tests/verifier_texte_resultats.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
contient <- function(textes, motif) any(grepl(motif, textes, fixed = TRUE))
nb <- function(x, fr = TRUE, k = 3) nombre_resultat(x, fr, k)

set.seed(2)
df <- data.frame(nom = paste0("o", 1:60), an = rep(2001:2012, 5), site = rep(c("A", "B", "C", "D", "E"), each = 12), x = stats::rnorm(60), stringsAsFactors = FALSE)
df$n <- stats::rpois(60, exp(1 + 0.05 * (df$an - 2006) + 0.3 * df$x))
df$y <- 3 + 0.2 * df$x + rep(stats::rnorm(5), each = 12) + stats::rnorm(60, 0, 0.5)
df$present <- as.numeric(df$n > 2)
p <- projet_depuis_tableau(df, "t")
u <- function(...) numeros(p, c(...))
obs <- numeros_observations(p)

d1 <- definir_modele(p, "GLM", x = u("an", "x"), y = u("n"), observations = obs, echelle = c(defaut = "none"), famille = "poisson")
t1 <- texte_resultats(p, d1, ajuster(p, d1), "fr")$paragraphes
g1 <- stats::glm(n ~ an + x, data = df, family = poisson())
b <- stats::coef(g1)[["x"]]
verifier("Poisson : le coefficient de x et son facteur exp(β), comme glm()", contient(t1, sprintf("β = %s", nb(b))) && contient(t1, sprintf("soit un facteur %s", nb(exp(b)))))
verifier("Poisson : la déviance expliquée et le nombre d'observations", contient(t1, "60 observations") && contient(t1, "de la déviance expliquée"))
verifier("les termes significatifs nommés", contient(t1, "Au seuil de 5 %, « an » et « x » ont un effet significatif."))
d2 <- definir_modele(p, "GLM", x = u("x"), y = u("present"), observations = obs, echelle = c(defaut = "none"), famille = "binomiale")
t2 <- texte_resultats(p, d2, ajuster(p, d2), "en")$paragraphes
g2 <- stats::glm(present ~ x, data = df, family = binomial())
verifier("binomiale, en anglais : le rapport de cotes", contient(t2, sprintf("an odds ratio of %s", nb(exp(stats::coef(g2)[["x"]]), FALSE))))
d3 <- definir_modele(p, "LMM", x = u("x"), y = u("y"), observations = obs, echelle = c(defaut = "none"), aleatoires = list(list(by = "variable", variable = u("site"))))
t3 <- texte_resultats(p, d3, ajuster(p, d3), "en")$paragraphes
l3 <- nlme::lme(y ~ x, random = ~ 1 | site, data = df, method = "REML")
verifier("modèle mixte : R² marginal et conditionnel, l'écart-type du site comme nlme::lme", contient(t3, "marginal R²") &&
           contient(t3, sprintf("site σ = %s", nb(as.numeric(nlme::VarCorr(l3)[1, 2]), FALSE))) && contient(t3, "residual σ"))
d4 <- definir_modele(p, "GAM", x = u("an", "x"), y = u("n"), observations = obs, echelle = c(defaut = "none"), famille = "poisson", lisses = u("an"))
t4 <- texte_resultats(p, d4, ajuster(p, d4), "fr")$paragraphes
a4 <- mgcv::gam(n ~ x + s(an, k = 10), data = df, family = poisson(), method = "REML")
verifier("GAM : l'edf de la courbe comme mgcv::gam", contient(t4, sprintf("an, edf = %s", nb(summary(a4)$s.table[1, 1], TRUE, 2))))
pe <- lire_projet("exemple/vergers.json")
dp <- modele_du_projet(pe, 2)
mp <- ajuster(pe, dp)
tp <- texte_resultats(pe, dp, mp, "fr")$paragraphes
vip <- mp$statistiques$variables
haut <- vip$terme[which.max(vip$VIP)]
verifier("PLS : R²X, Q² cumulé, CV-ANOVA, la variable de plus grand VIP en tête", contient(tp, "Le modèle PLS à") && contient(tp, "CV-ANOVA") &&
           grepl(sprintf("VIP > 1) : %s (", haut), paste(tp, collapse = " "), fixed = TRUE))
dk <- definir_modele(p, "KMEANS", x = u("x", "y"), observations = obs, options = list(k = 2))
tk <- texte_resultats(p, dk, ajuster(p, dk), "fr", chiffres = "k = 2 · R2 0.50")$paragraphes
verifier("une autre analyse sans texte à elle : ses chiffres, avec une virgule décimale en français", identical(tk, sprintf("Résultats de %s (KMEANS) : k = 2 · R2 0,50.", dk$name)) &&
           identical(texte_resultats(p, dk, ajuster(p, dk), "en", chiffres = "k = 2 · R2 0.50")$paragraphes, sprintf("Results of %s (KMEANS): k = 2 · R2 0.50.", dk$name)))

# un échantillonnage par distances, son texte à lui : la fonction de détection et son AIC, avec des
# covariables chaque coefficient et son facteur sur σ, p et ESW, la densité au total et par strate
set.seed(5)
dd <- do.call(rbind, lapply(1:16, function(j) {
  x <- stats::runif(stats::rpois(1, 40), 0, 100)
  obs <- sample(c("A", "B"), length(x), TRUE)
  vu <- stats::runif(length(x)) < exp(-(x / (30 * ifelse(obs == "B", 1.5, 1)))^2 / 2)
  data.frame(strate = if (j <= 8) "Nord" else "Sud", aire = if (j <= 8) 50 else 30, ligne = sprintf("L%02d", j), effort = 2, distance = round(x[vu], 1), observateur = obs[vu])
}))
pd <- projet_depuis_tableau(cbind(nom = sprintf("d%04d", seq_len(nrow(dd))), dd, stringsAsFactors = FALSE), "distances")
ud <- function(...) numeros(pd, c(...))
od <- list(transect = "ligne", echantillon = list(by = "variable", variable = ud("ligne")), strate = list(by = "variable", variable = ud("strate")), effort = ud("effort"),
           aire = ud("aire"), troncature = list(type = "distance", valeur = 100), cle = "hn", serie = "cos", ajustements = "aic", monotonie = "stricte",
           unites = list(distance = "m", effort = "km", aire = "km2"))
dd1 <- definir_modele(pd, "DISTANCE", x = NULL, y = ud("distance"), echelle = "none", options = od)
md1 <- ajuster(pd, dd1)
td1 <- texte_resultats(pd, dd1, md1, "fr")$paragraphes
dd2 <- definir_modele(pd, "DISTANCE", x = NULL, y = ud("distance"), echelle = "none", options = c(od, list(z = ud("observateur"))))
md2 <- ajuster(pd, dd2)
td2 <- texte_resultats(pd, dd2, md2, "en")$paragraphes
verifier("échantillonnage par distances : sa fonction de détection et son AIC, p et ESW, la densité au total et celle de chaque strate",
         contient(td1, libelle_detection(md1$detection$fd)) && contient(td1, sprintf("p = %s", nb(md1$p))) && contient(td1, sprintf("ESW = %s m", nb(md1$effectif$valeur))) &&
           contient(td1, sprintf("D = %s par km²", nb(md1$densite$D[3], TRUE, 4))) && contient(td1, sprintf("Nord D = %s", nb(md1$densite$D[1], TRUE, 4))))
verifier("échantillonnage par distances à covariables, en anglais : le coefficient de l'observateur B et son facteur sur σ, p̄, l'écart d'AIC au modèle sans covariables",
         contient(td2, sprintf("observateur(B) β = %s", nb(md2$parametres$estimation[2], FALSE))) && contient(td2, sprintf("σ × %s", nb(exp(md2$parametres$estimation[2]), FALSE))) &&
           contient(td2, sprintf("p̄ = %s", nb(md2$p, FALSE))) && contient(td2, "Without covariates (hn") && contient(td2, "Reference categories: observateur: A."))

cat("Conditions du modèle\n")
set.seed(4)
n <- 80
dv <- data.frame(nom = paste0("o", 1:n), x1 = stats::rnorm(n), stringsAsFactors = FALSE)
dv$x2 <- dv$x1 + stats::rnorm(n, 0, 0.2)
dv$x3 <- stats::rnorm(n)
dv$bon <- 1 + dv$x1 + dv$x3 + stats::rnorm(n)
dv$y <- exp(1 + dv$x1 + stats::rnorm(n, 0, 0.6))
dv$n <- ifelse(stats::runif(n) < 0.4, 0, stats::rpois(n, exp(1 + 0.4 * dv$x1)))
dv$separee <- as.integer(dv$x1 > 0)
dv$presence <- as.integer(dv$x3 + stats::rnorm(n) > 0)
pv <- projet_depuis_tableau(dv, "conditions")
uv <- function(...) numeros(pv, c(...))
statuts <- function(d) { v <- verifications_regression(pv, ajuster(pv, d)); stats::setNames(vapply(v, `[[`, "", "statut"), vapply(v, `[[`, "", "cle")) }
dm <- function(type, x, y, ...) definir_modele(pv, type, x = uv(x), y = uv(y), observations = numeros_observations(pv), echelle = c(defaut = "none"), ...)
s_bon <- statuts(dm("LM", c("x1", "x3"), "bon"))
verifier("un modèle juste : loi, variance et colinéarité sans alerte", all(s_bon[c("loi", "variance", "colinearite")] == "ok"))
s_mauvais <- statuts(dm("LM", c("x1", "x2"), "y"))
verifier("une réponse log-normale et deux termes colinéaires : loi, variance et colinéarité signalées",
         all(s_mauvais[c("loi", "variance", "colinearite")] == "attention"))
s_zero <- statuts(dm("GLM", "x1", "n", famille = "poisson"))
verifier("des comptages avec trop de zéros : surdispersion et excès de zéros signalés", all(s_zero[c("dispersion", "zeros")] == "attention"))
s_separee <- statuts(dm("GLM", "x1", "separee", famille = "binomiale"))
s_presence <- statuts(dm("GLM", "x1", "presence", famille = "binomiale"))
verifier("une présence que x1 sépare tout à fait : la séparation signalée, pas pour une présence ordinaire",
         identical(unname(s_separee["separation"]), "attention") && !"separation" %in% names(s_presence))
# la séparation : un coefficient démesuré dont l'erreur-type l'est plus encore, pas des probabilités
# ajustées de 0 ou 1 seules (une pente raide mais finie en donne, à moins de 1e-8 du bord, et même
# numériquement 0 ou 1 comme glm.fit le dit) ; une catégorie sans présence, que ce seuil ne voit pas
texte_separation <- function(p, d) Find(function(v) identical(v$cle, "separation"), verifications_regression(p, ajuster(p, d)))$texte
v_separee <- texte_separation(pv, dm("GLM", "x1", "separee", famille = "binomiale"))
set.seed(4)
da <- data.frame(nom = sprintf("a%03d", 1:200), altitude = round(stats::runif(200, 0, 2000)), stringsAsFactors = FALSE)
da$presence <- stats::rbinom(200, 1, stats::plogis(-25 + 0.025 * da$altitude))
da$raide <- stats::rbinom(200, 1, stats::plogis(-50 + 0.05 * da$altitude))
pa <- projet_depuis_tableau(da, "altitude")
d_alt <- function(y) definir_modele(pa, "GLM", x = numeros(pa, "altitude"), y = numeros(pa, y), observations = numeros_observations(pa), echelle = c(defaut = "none"), famille = "binomiale")
g_raide <- stats::glm(raide ~ altitude, family = stats::binomial, data = da)
set.seed(9)
dh <- data.frame(nom = sprintf("h%03d", 1:150), hab = sample(c("a", "b", "c"), 150, TRUE), x = stats::rnorm(150), stringsAsFactors = FALSE)
dh$y <- stats::rbinom(150, 1, stats::plogis(0.3 + 0.8 * dh$x))
dh$y[dh$hab == "c"] <- 0
ph <- projet_depuis_tableau(dh, "habitats")
v_categorie <- texte_separation(ph, definir_modele(ph, "GLM", x = numeros(ph, c("hab", "x")), y = numeros(ph, "y"), observations = numeros_observations(ph),
                                                   echelle = c(defaut = "none"), famille = "binomiale"))
verifier(sprintf("une pente raide mais finie de l'altitude (le logit de -25 à 25) : pas de séparation, ni pour une plus raide dont %d probabilités ajustées sont numériquement 0 ou 1",
                 sum(stats::fitted(g_raide) < 10 * .Machine$double.eps | stats::fitted(g_raide) > 1 - 10 * .Machine$double.eps)),
         is.null(texte_separation(pa, d_alt("presence"))) && is.null(texte_separation(pa, d_alt("raide"))) &&
           any(stats::fitted(g_raide) < 10 * .Machine$double.eps))
verifier(sprintf("une catégorie sans présence : la séparation signalée, les coefficients de ses catégories nommés, pas celui de x (« %s »)", substr(v_categorie %||% "", 1, 90)),
         grepl("^Les? coefficients? de « hab\\([abc]\\) » \\(β = -?1[89]", v_categorie %||% "") && !grepl("« x »", v_categorie %||% "", fixed = TRUE) &&
           !grepl("numériquement", v_categorie %||% "") &&
           grepl("^Le coefficient de « x1 » \\(β = ", v_separee %||% "") && grepl("observations ont une probabilité ajustée numériquement de 0 ou 1", v_separee %||% "", fixed = TRUE))
# (une année non centrée, 2001 à 2012, croisée avec l'habitat, sans interaction : un coefficient
# démesuré et incertain par colinéarité, ses probabilités ajustées loin de 0 et de 1, n'est pas une
# séparation ; une courbe qui sépare, si : ses probabilités numériquement 0 ou 1 le disent)
set.seed(2)
dn <- data.frame(nom = sprintf("n%03d", 1:240), an = rep(2001:2012, 20), hab = rep(c("a", "b"), each = 120))
dn$presence <- stats::rbinom(240, 1, stats::plogis(-0.2 + 0.05 * (dn$an - 2006)))
pn <- projet_depuis_tableau(dn, "annees")
d_an <- definir_modele(pn, "GLM", x = numeros(pn, c("an", "hab")), y = numeros(pn, "presence"), observations = numeros_observations(pn),
                       echelle = c(defaut = "none"), famille = "binomiale", croises = list(numeros(pn, c("an", "hab"))))
m_an <- ajuster(pn, d_an)
set.seed(3)
dg <- data.frame(nom = sprintf("g%03d", 1:120), x = stats::runif(120, 0, 10))
dg$sep <- as.integer(dg$x > 5)
pg <- projet_depuis_tableau(dg, "courbe")
v_courbe <- texte_separation(pg, definir_modele(pg, "GAM", x = numeros(pg, "x"), y = numeros(pg, "sep"), observations = numeros_observations(pg),
                                                echelle = c(defaut = "none"), famille = "binomiale", lisses = numeros(pg, "x")))
verifier(sprintf("une année non centrée croisée avec l'habitat (des coefficients de p = %s, ses probabilités entre %.2f et %.2f) : pas de séparation ; une courbe qui sépare : signalée",
                 paste(round(m_an$resultat$coefficients$p, 2), collapse = ", "), min(m_an$resultat$ajuste), max(m_an$resultat$ajuste)),
         is.null(Find(function(v) identical(v$cle, "separation"), verifications_regression(pn, m_an, d_an))) &&
           grepl("une courbe du modèle sépare sans doute les présences des absences", v_courbe %||% "", fixed = TRUE))
cat(if (echecs) sprintf("%d échec(s)\n", echecs) else "Tout est bon.\n")
if (echecs) quit(status = 1)
