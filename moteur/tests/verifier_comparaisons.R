# Vérifie les tests par variable (moteur/R/comparaisons.R) contre les fonctions de R
# elles-mêmes, et le texte des méthodes (moteur/R/methodes.R) des modèles du projet d'exemple.
#   Rscript moteur/tests/verifier_comparaisons.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
p <- lire_projet("exemple/vergers.json")
obs <- numeros_observations(p)
culture <- numeros(p, "culture")
variete <- numeros(p, "variete")
sucre <- colonne(p, numeros(p, "sucre"), obs)
g2 <- colonne(p, culture, obs)
g4 <- factor(colonne(p, variete, obs))
ligne <- function(r, nom) r$tableau[r$tableau$nom == nom, ]

cat("Deux classes\n")
r <- tests_variables(p, culture, test = "welch")
verifier("t de Welch = t.test()", isTRUE(all.equal(ligne(r, "sucre")$p, t.test(sucre[g2 == 2], sucre[g2 == 1])$p.value)))
verifier("Benjamini–Hochberg = p.adjust()", isTRUE(all.equal(r$tableau$p_ajuste, p.adjust(r$tableau$p, "BH"))))
a <- sucre[g2 == 1 & !is.na(sucre)]; b <- sucre[g2 == 2 & !is.na(sucre)]
verifier("log2 du rapport des moyennes, d de Cohen", isTRUE(all.equal(ligne(r, "sucre")$log2_rapport, log2(mean(b) / mean(a)))) &&
         isTRUE(all.equal(ligne(r, "sucre")$d, (mean(b) - mean(a)) / sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) / (length(a) + length(b) - 2)))))
r <- tests_variables(p, culture, test = "student", correction = "bonferroni")
verifier("t de Student = t.test(var.equal = TRUE), Bonferroni", isTRUE(all.equal(ligne(r, "sucre")$p, t.test(b, a, var.equal = TRUE)$p.value)) &&
         isTRUE(all.equal(r$tableau$p_ajuste, p.adjust(r$tableau$p, "bonferroni"))))
r <- tests_variables(p, variete, groupes = c("Gala", "Granny"), test = "mann_whitney")
x <- colonne(p, numeros(p, "poids"), obs)
verifier("Mann–Whitney = wilcox.test()", isTRUE(all.equal(ligne(r, "poids")$p,
         suppressWarnings(wilcox.test(x[g4 == 3 & !is.na(x)], x[g4 == 1 & !is.na(x)])$p.value))))

cat("Toutes les classes\n")
r <- tests_variables(p, variete, test = "anova")
verifier("ANOVA = oneway.test(var.equal = TRUE)", isTRUE(all.equal(ligne(r, "sucre")$p, oneway.test(sucre ~ g4, var.equal = TRUE)$p.value)))
r <- tests_variables(p, variete, test = "kruskal")
verifier("Kruskal–Wallis = kruskal.test()", isTRUE(all.equal(ligne(r, "sucre")$p, kruskal.test(sucre, g4)$p.value)))
verifier("la variable de classe n'est pas testée", !"variete" %in% r$tableau$nom)

cat("Modèles de classe\n")
quant <- Filter(function(v) !length(v$categories), p$variables)
x <- vapply(quant[1:14], function(v) as.numeric(v$uid), 0)
ds <- modeles_classe(p, definir_modele(p, "PCA", x = x, nom = "MC"), variete)
verifier("une ACP par variété, sur ses seules observations", length(ds) == 4 &&
         all(vapply(ds, function(e) all(colonne(p, variete, e$observations) == match(e$groupe_classes$classe, variable_du_projet(p, variete)$categories)), TRUE)))
modeles <- lapply(ds, function(e) { e$A <- max(1, autoajuster(p, e)); list(definition = e, modele = ajuster(p, e)) })
r <- appartenance_classes(p, modeles)
verifier(sprintf("chaque modèle accepte sa classe (%s %%) et refuse les autres", paste(round(100 * diag(r$table)), collapse = ", ")),
         all(diag(r$table) >= 0.85) && all(r$table[row(r$table) != col(r$table)] <= 0.05))
e <- modeles[[1]]
verifier("distance = DModX d'une observation à prédire", isTRUE(all.equal(r$observations[[paste("DModX", ds[[1]]$groupe_classes$classe)]],
         predire(p, e$modele, e$definition, numeros_observations(p))$DModX)))

cat("OPLS à plusieurs composantes prédictives\n")
d <- definir_modele(p, "OPLS-DA", x = x, classe = variete, A = 5)
m <- ajuster(p, d)
verifier("OPLS-DA à 4 classes : 3 prédictives, orthogonales sans Y", d$Ap == 3 && m$A == 5 && all(m$C[, 4:5] == 0))
pr <- predire(p, m, d, d$observations)
verifier("prédire les observations du modèle rend leurs scores", isTRUE(all.equal(unname(as.matrix(pr[paste0("t", 1:5)])), unname(m$T), tolerance = 1e-8)))
d1 <- p$models[[4]]
verifier("une seule prédictive : Ap = 1 par défaut (projets enregistrés avant)", is.null(d1$Ap) && ajuster(p, d1)$Ap == 1)
d2 <- definir_modele(p, "OPLS", x = x[1:10], y = x[11:12], A = 0)
verifier("OPLS à 2 Y : 2 prédictives", d2$Ap == 2 && autoajuster(p, d2) >= 2)

cat("Texte des méthodes\n")
for (d in p$models) {
  m <- ajuster(p, d)
  for (l in c("fr", "en")) {
    t <- texte_methodes(p, d, m, l, "essai")
    verifier(sprintf("%s (%s) : %d paragraphes, %d références, R2 et Q2 donnés", d$name, l, length(t$paragraphes), length(t$references)),
             length(t$paragraphes) == 5 && any(grepl("Q²", t$paragraphes)) && any(grepl("R Core Team", t$references)) &&
             !any(grepl("NA", t$paragraphes, fixed = TRUE)))
  }
}

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
