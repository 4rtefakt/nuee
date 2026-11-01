# Vérifie le jeu de test (moteur/R/jeu_test.R) sur le projet d'exemple : le modèle ne voit pas
# ses observations, ses statistiques sont celles qu'on recalcule à la main, le classement d'une
# PLS-DA, le choix au hasard ou par Kennard–Stone (les mêmes observations que le paquet
# prospectr quand il est installé), les réglages et le texte des méthodes.
#   Rscript moteur/tests/verifier_jeu_test.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-10) isTRUE(all.equal(unname(a), unname(b), tolerance = tol, check.attributes = FALSE))
p <- lire_projet("exemple/vergers.json")
obs <- numeros_observations(p)
quant <- Filter(function(v) !length(v$categories), p$variables)
x <- vapply(quant[1:10], function(v) as.numeric(v$uid), 0)
y <- numeros(p, "note_gout")
x <- setdiff(x, y)

cat("PLS avec un jeu de test\n")
test <- obs[seq(3, length(obs), by = 4)]
d <- definir_modele(p, "PLS", x = x, y = y, A = 2, test = test)
m <- ajuster(p, d)
verifier("les observations du test sont hors du modèle", !any(test %in% d$observations) && length(d$observations) == length(obs) - length(test))
m0 <- ajuster(p, definir_modele(p, "PLS", x = x, y = y, A = 2, observations = setdiff(obs, test)))
verifier("le modèle est celui des seules autres observations (le test n'y entre pas)", proche(m$T, m0$T) && proche(m$statistiques$composantes$Q2cum, m0$statistiques$composantes$Q2cum))
pr <- predire(p, m, d, test)
observe <- colonne(p, y, test)
e <- pr[["note_gout prédit"]] - observe
ok <- !is.na(e)
moyenne <- mean(colonne(p, y, setdiff(obs, test)), na.rm = TRUE)
st <- m$test$statistiques
verifier("RMSEP, biais, SEP et Q2 externe recalculés à la main", m$test$n == length(test) && st$n == sum(ok) &&
         proche(st$RMSEP, sqrt(mean(e[ok]^2))) && proche(st$biais, mean(e[ok])) && proche(st$SEP, sd(e[ok])) &&
         proche(st$Q2ext, 1 - sum(e[ok]^2) / sum((observe[ok] - moyenne)^2)))
verifier("observé et prédit de chaque observation du test", proche(m$test$observations[["note_gout observé"]], observe) &&
         proche(m$test$observations[["note_gout prédit"]], pr[["note_gout prédit"]]) && identical(m$test$observations$uid, test))
r <- reglages_modele(p, d)
verifier("réglages : le test à part, pas parmi les exclues", identical(as.numeric(r$test), test) && !length(r$exclues))
tm <- texte_methodes(p, d, m, "fr")
verifier("texte des méthodes : le jeu de test et son RMSEP", any(grepl(sprintf("jeu de test de %d observations", length(test)), tm$paragraphes)) &&
         any(grepl("RMSEP = ", tm$paragraphes)))
verifier("sans jeu de test : rien de plus", is.null(m0$test) && is.null(definir_modele(p, "PLS", x = x, y = y, A = 2)$test))

cat("PLS-DA et ACP\n")
variete <- numeros(p, "variete")
d <- definir_modele(p, "PLS-DA", x = x, classe = variete, A = 3, test = test)
m <- ajuster(p, d)
categories <- variable_du_projet(p, variete)$categories
vraies <- paste0("variete(", categories[colonne(p, variete, test)], ")")
predites <- predire(p, m, d, test)$classe_predite
verifier("PLS-DA : la part bien classée est celle des classes prédites", proche(m$test$classement$bien_classees, mean(vraies == predites)) &&
         sum(m$test$classement$table) == length(test) && sum(diag(m$test$classement$table)) == sum(vraies == predites))
d <- definir_modele(p, "PCA", x = x, A = 2, test = test)
m <- ajuster(p, d)
verifier("ACP : le nombre d'observations du test hors du domaine", m$test$hors_domaine == sum(predire(p, m, d, test)$hors_domaine) && is.null(m$test$statistiques))

cat("Choisir le jeu de test\n")
set.seed(1)
h <- choisir_test(p, x, obs, "hasard", 0.25)
verifier("au hasard : un quart des observations", length(h) == round(0.25 * length(obs)) && all(h %in% obs))
h <- choisir_test(p, x, obs, "hasard", 0.2, classe = variete)
cl <- colonne(p, variete, obs)
verifier("au hasard, par classe : un cinquième de chaque classe", all(vapply(unique(cl), function(k) sum(h %in% obs[cl == k]) == round(0.2 * sum(cl == k)), TRUE)))
ks <- choisir_test(p, x, obs, "kennard_stone", 0.25)
verifier("Kennard–Stone : un quart, toujours le même", length(ks) == round(0.25 * length(obs)) && identical(ks, choisir_test(p, x, obs, "kennard_stone", 0.25)))
X <- scale(vapply(x, function(u) colonne(p, u, obs), numeric(length(obs))))
X[!is.finite(X)] <- 0
garde <- kennard_stone(X, 90)
verifier("Kennard–Stone : les deux plus éloignées d'abord", { D <- as.matrix(dist(X)); proche(D[garde[1], garde[2]], max(D)) })
if (requireNamespace("prospectr", quietly = TRUE)) {
  verifier("Kennard–Stone : les mêmes observations que prospectr::kenStone()", setequal(garde, prospectr::kenStone(X, k = 90, metric = "euclid")$model))
}

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
