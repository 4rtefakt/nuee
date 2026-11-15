# Vérifie le texte des résultats (moteur/R/resultats_texte.R) : les nombres qu'il écrit sont ceux
# des modèles (glm(), nlme::lme(), mgcv::gam(), la PLS de l'exemple), sur l'échelle de la réponse
# pour un lien log (un facteur) ou logit (un rapport de cotes), les termes significatifs nommés,
# les courbes et les effets aléatoires, en français et en anglais ; une autre analyse sans texte à
# elle donne ses chiffres. Puis les conditions d'une régression dites en mots
# (moteur/R/verifications_modele.R) : chacune vue où les données la mettent en défaut, et pas
# ailleurs.
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
verifier("une autre analyse sans texte à elle : ses chiffres", identical(tk, sprintf("Résultats de %s (KMEANS) : k = 2 · R2 0.50.", dk$name)))

cat("Conditions du modèle\n")
set.seed(4)
n <- 80
dv <- data.frame(nom = paste0("o", 1:n), x1 = stats::rnorm(n), stringsAsFactors = FALSE)
dv$x2 <- dv$x1 + stats::rnorm(n, 0, 0.2)
dv$x3 <- stats::rnorm(n)
dv$bon <- 1 + dv$x1 + dv$x3 + stats::rnorm(n)
dv$y <- exp(1 + dv$x1 + stats::rnorm(n, 0, 0.6))
dv$n <- ifelse(stats::runif(n) < 0.4, 0, stats::rpois(n, exp(1 + 0.4 * dv$x1)))
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
cat(if (echecs) sprintf("%d échec(s)\n", echecs) else "Tout est bon.\n")
if (echecs) quit(status = 1)
