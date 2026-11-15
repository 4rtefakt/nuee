# Vérifie Un modèle par réponse (moteur/R/par_reponse.R) : chaque réponse ajustée comme le modèle
# d'origine (contre glm() et mgcv::gam() sur la même réponse), la valeur p de chaque terme
# corrigée sur les réponses comme p.adjust(), une réponse qui ne s'ajuste pas nommée sans arrêter
# les autres, les refus, la transformation de la réponse gardée, les effets pour les petits
# graphiques, la phrase des méthodes.
#   Rscript moteur/tests/verifier_par_reponse.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

# 60 relevés sur 12 ans, 5 espèces dont les comptages suivent l'année (tendances différentes), un
# habitat ; « vide » n'a aucune valeur.
set.seed(8)
n <- 60
annee <- rep(2011:2022, 5)
habitat <- rep(c("marais", "prairie"), length.out = n)
pentes <- c(sp1 = 0.08, sp2 = -0.06, sp3 = 0, sp4 = 0.02, sp5 = -0.1)
especes <- sapply(pentes, function(b) stats::rpois(n, exp(1.5 + b * (annee - 2016) + 0.3 * (habitat == "marais"))))
df <- data.frame(nom = sprintf("r%02d", 1:n), annee = annee, habitat = habitat, especes, vide = NA_real_, stringsAsFactors = FALSE)
p <- projet_depuis_tableau(df, "especes")
u <- function(...) numeros(p, c(...))
d <- definir_modele(p, "GLM", x = u("annee", "habitat"), y = u("sp1"), observations = numeros_observations(p), echelle = c(defaut = "none"), famille = "poisson")
r <- modeles_par_reponse(p, d, u("sp1", "sp2", "sp3", "sp4", "sp5", "vide"))
ok <- r$resume$erreur == ""
verifier("5 modèles ajustés, la réponse sans valeur nommée avec sa raison", sum(ok) == 5 && grepl("vide", r$resume$reponse[!ok]) && nzchar(r$resume$erreur[!ok]))
co <- r$tableau[r$tableau$terme == "annee", ]
ref <- t(sapply(names(pentes), function(s) summary(stats::glm(stats::reformulate(c("annee", "habitat"), s), data = df, family = poisson()))$coefficients["annee", c(1, 2, 4)]))
verifier("le coefficient de l'année, son erreur type et p de chaque espèce comme glm()", isTRUE(all.equal(unname(as.matrix(co[c("estimation", "erreur_type", "p")])), unname(ref), tolerance = 1e-4)))  # (mgcv et glm convergent un peu différemment)
verifier("p corrigée sur les réponses, terme par terme, comme p.adjust(BH)", isTRUE(all.equal(co$p_corrige, stats::p.adjust(co$p, "BH"))))
rh <- modeles_par_reponse(p, d, u("sp1", "sp2", "sp3"), "holm")
verifier("Holm aussi", isTRUE(all.equal(rh$tableau$p_corrige[rh$tableau$terme == "annee"], stats::p.adjust(rh$tableau$p[rh$tableau$terme == "annee"], "holm"))))
verifier("les effets de l'année pour chaque espèce (les petits graphiques)", length(r$effets) == 5 && all(vapply(r$effets, function(e) any(vapply(e$effets, function(x) identical(x$terme, "annee"), TRUE)), TRUE)))
verifier("la qualité de chaque modèle : N, AIC", all(r$resume$N[ok] == n) && all(is.finite(r$resume$AIC[ok])))
dt <- definir_modele(p, "LM", x = u("annee"), y = u("sp1"), observations = numeros_observations(p), echelle = c(defaut = "none"),
                     transformations = stats::setNames("log10:1", u("sp1")))
rt <- modeles_par_reponse(p, dt, u("sp2", "sp4"))
ref_t <- stats::coef(stats::lm(log10(sp4 + 1) ~ annee, data = df))[["annee"]]
verifier("la transformation de la réponse gardée (log10(x + 1)) pour chaque espèce", isTRUE(all.equal(rt$tableau$estimation[rt$tableau$reponse == "sp4"], ref_t)))
dg <- definir_modele(p, "GAM", x = u("annee"), y = u("sp1"), observations = numeros_observations(p), echelle = c(defaut = "none"), famille = "poisson", lisses = u("annee"))
rg <- modeles_par_reponse(p, dg, u("sp1", "sp2"))
ref_g <- summary(mgcv::gam(sp2 ~ s(annee, k = 10), data = df, family = poisson(), method = "REML"))$s.table
verifier("un GAM : l'edf et p de la courbe de chaque espèce comme mgcv::gam", isTRUE(all.equal(c(rg$tableau$edf[rg$tableau$reponse == "sp2"], rg$tableau$p[rg$tableau$reponse == "sp2"]), unname(ref_g[1, c(1, 4)]), tolerance = 1e-6)))
verifier("refus : une variable X en réponse, aucune réponse, une ACP",
         grepl("X", refus(modeles_par_reponse(p, d, u("annee")))) &&
           nzchar(refus(modeles_par_reponse(p, d, numeric(0)))) &&
           nzchar(refus(modeles_par_reponse(p, definir_modele(p, "PCA", x = u("sp1", "sp2"), observations = numeros_observations(p)), u("sp3")))))
r2 <- modeles_par_reponse(p, definir_modele(p, "GLM", x = u("annee"), y = u("sp1"), observations = numeros_observations(p), echelle = c(defaut = "none"), famille = "poisson"), u("sp1", "habitat"))
verifier("une réponse qualitative : nommée dans les échecs", grepl("qualitative", r2$resume$erreur[2]))
verifier("la phrase des méthodes, en français et en anglais", grepl("Benjamini et Hochberg", phrase_methodes_par_reponse(5, "BH", TRUE)) &&
           grepl("Holm's", phrase_methodes_par_reponse(5, "holm", FALSE)))

cat(if (echecs) sprintf("%d échec(s)\n", echecs) else "Tout est bon.\n")
if (echecs) quit(status = 1)
