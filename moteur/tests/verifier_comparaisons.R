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

cat("Deux variables qualitatives\n")
verger <- numeros(p, "verger")
tab <- table(colonne(p, verger, obs), g2)
k <- suppressWarnings(chisq.test(tab, correct = FALSE))
r <- test_deux_variables(p, verger, culture)
verifier("khi² = chisq.test(correct = FALSE), résidus ajustés, V de Cramér", r$type == "contingence" && r$conseille == "khi2" &&
         isTRUE(all.equal(r$tests$p[1], k$p.value)) && isTRUE(all.equal(r$residus, unclass(unname(k$stdres)) + 0)) &&
         isTRUE(all.equal(r$tests$effet[1], sqrt(unname(k$statistic) / sum(tab)))) && nrow(r$tests) == 1)
peu <- obs[c(1:6, 31:36, 61:64)]
r <- test_deux_variables(p, verger, culture, observations = peu)
verifier("attendus sous 5 : Fisher exact conseillé = fisher.test()", r$conseille == "fisher" && r$raison == "attendus" && !r$fisher_simule &&
         isTRUE(all.equal(r$tests$p[2], fisher.test(table(colonne(p, verger, peu), colonne(p, culture, peu)))$p.value)))
p2 <- classes_valeurs(p, "lourd", numeros(p, "poids"), n = 2)
lourd <- numeros(p2, "lourd")
r <- test_deux_variables(p2, lourd, culture, observations = peu)
f <- fisher.test(table(colonne(p2, lourd, peu), colonne(p2, culture, peu)))
verifier("2 × 2 : rapport des cotes et son intervalle = fisher.test()", isTRUE(all.equal(r$tests$effet[2], unname(f$estimate))) &&
         isTRUE(all.equal(r$ic_cotes, as.numeric(f$conf.int))) && grepl("rapport des cotes", r$phrase))
r <- test_deux_variables(p, verger, variete)
verifier("le même tableau quel que soit l'ordre des variables", isTRUE(all.equal(r$tests$p[1], test_deux_variables(p, variete, verger)$tests$p[1])))

cat("Une mesure entre groupes\n")
r <- test_deux_variables(p, variete, numeros(p, "sucre"))
verifier("4 groupes normaux, variances égales : ANOVA conseillée = oneway.test(), η²", r$type == "groupes" && r$conseille == "anova" &&
         isTRUE(all.equal(r$tests$p[1], oneway.test(sucre ~ g4, var.equal = TRUE)$p.value)) &&
         isTRUE(all.equal(r$tests$effet[1], { a <- anova(lm(sucre ~ g4)); a[1, 2] / sum(a[, 2]) })))
verifier("ANOVA de Welch, Kruskal–Wallis, ε² = H / (n − 1)", isTRUE(all.equal(r$tests$p[2], oneway.test(sucre ~ g4)$p.value)) &&
         isTRUE(all.equal(r$tests$p[3], kruskal.test(sucre, g4)$p.value)) &&
         isTRUE(all.equal(r$tests$effet[3], unname(kruskal.test(sucre, g4)$statistic) / (length(sucre) - 1))))
tk <- TukeyHSD(aov(sucre ~ g4))$g4
cats <- as.character(variable_du_projet(p, variete)$categories)
verifier("Tukey = TukeyHSD(), paires dans le même ordre", isTRUE(all.equal(r$posthoc$anova$p, unname(tk[, "p adj"]))) &&
         identical(paste(match(r$posthoc$anova$b, cats), match(r$posthoc$anova$a, cats), sep = "-"), rownames(tk)))
verifier("Brown–Forsythe = ANOVA des écarts à la médiane du groupe",
         isTRUE(all.equal(r$variances$p, anova(lm(abs(sucre - ave(sucre, g4, FUN = median)) ~ g4))[1, 5])))
verifier("Dunn : correction de Holm = p.adjust()", isTRUE(all.equal(r$posthoc$kruskal$p_ajuste, p.adjust(r$posthoc$kruskal$p, "holm"))))
deux <- droplevels(g4[g4 %in% c(1, 3)])
s2 <- sucre[g4 %in% c(1, 3)]
verifier("Dunn à deux groupes : z² = H de Kruskal–Wallis", isTRUE(all.equal(dunn(s2, deux)$z^2, unname(kruskal.test(s2, deux)$statistic))))
verifier("Games–Howell à deux groupes = t de Welch", isTRUE(all.equal(games_howell(split(s2, deux))$p, t.test(s2[deux == 3], s2[deux == 1])$p.value)))
r <- test_deux_variables(p, numeros(p, "sucre"), culture)
a <- sucre[g2 == 1]; b <- sucre[g2 == 2]
w <- suppressWarnings(wilcox.test(b, a))
verifier("2 groupes : t de Welch = t.test(), Mann–Whitney = wilcox.test(), r rang-bisérial", r$type == "groupes" &&
         isTRUE(all.equal(r$tests$p, c(t.test(b, a)$p.value, w$p.value))) &&
         isTRUE(all.equal(r$tests$effet[2], 2 * unname(w$statistic) / (length(a) * length(b)) - 1)) && is.null(r$posthoc))
verifier("normalité douteuse (Shapiro–Wilk) : Mann–Whitney conseillé",
         r$conseille == "mann_whitney" && r$raison == "normalite" && min(r$groupes$shapiro_p) < 0.05)
set.seed(2)
x <- c(rnorm(30, 10, 0.5), rnorm(30, 10.5, 2), rnorm(30, 11, 4))
gx <- factor(rep(c("A", "B", "C"), each = 30))
r <- comparer_groupes(x, gx, paste0("o", 1:90))
verifier("variances inégales (Brown–Forsythe) : ANOVA de Welch conseillée", r$conseille == "welch_anova" && r$raison == "variances" && r$variances$p < 0.05)
r <- comparer_groupes(c(1, 2, 3, 4, 5, 6, 9), factor(c("A", "A", "A", "B", "B", "B", "C")), paste0("o", 1:7))
verifier("un groupe d'une seule observation est laissé de côté", identical(r$ecartes, "C") && identical(r$groupes$groupe, c("A", "B")))
p3 <- p
p3$observation_id_names <- c("Observation", "annee")
annees <- c("100", "9", "10")[obs %% 3 + 1]
for (i in seq_along(p3$observations)) p3$observations[[i]]$ids <- c(p3$observations[[i]]$ids[[1]], annees[i])
r <- test_deux_variables(p3, "id:2", numeros(p, "sucre"))
verifier("un identifiant des observations fait les groupes, dans l'ordre des nombres", r$type == "groupes" && r$x$nom == "annee" &&
         identical(r$groupes$groupe, c("9", "10", "100")) && isTRUE(all.equal(r$tests$p[3], kruskal.test(sucre, factor(annees))$p.value)))
x <- rep(1:60, 2)
r <- comparer_groupes(x + rnorm(120), factor(x), paste0("o", 1:120))
verifier("plus de 50 groupes : pas de comparaisons deux à deux, et la note le dit", is.null(r$posthoc) && grepl("50", r$note_comparaisons))

cat("Deux mesures\n")
poids <- colonne(p, numeros(p, "poids"), obs)
diametre <- colonne(p, numeros(p, "diametre"), obs)
r <- test_deux_variables(p, numeros(p, "poids"), numeros(p, "diametre"))
verifier("Pearson et Spearman = cor.test()", r$type == "correlation" &&
         isTRUE(all.equal(r$tests$p, c(cor.test(poids, diametre)$p.value, suppressWarnings(cor.test(poids, diametre, method = "spearman"))$p.value))) &&
         isTRUE(all.equal(r$ic, as.numeric(cor.test(poids, diametre)$conf.int))))
r <- correler(exp(poids / 40), diametre, paste0("o", seq_along(poids)))
verifier("une variable non normale : Spearman conseillée", r$conseille == "spearman" && r$raison == "normalite")

cat("Phrases\n")
r <- test_deux_variables(p, variete, numeros(p, "sucre"))
verifier(sprintf("en français : « %s »", r$phrase), grepl("^ANOVA à un facteur : F\\(3, 116\\) = [0-9]+,[0-9]{2}, p < 0,001 ; η² = 0,[0-9]{2} \\(N = 120, 4 groupes\\)\\.$", r$phrase))
options(nuee.langue = "en")
r <- test_deux_variables(p, verger, culture)
verifier(sprintf("en anglais : « %s »", r$phrase), grepl("^Pearson's chi-square test of independence: χ²\\(2, N = 120\\) = [0-9.]+, p = 0\\.[0-9]+; Cramér's V = 0\\.[0-9]{2}\\.$", r$phrase))
options(nuee.langue = NULL)
phrases <- vapply(list(c(verger, culture), c(variete, numeros(p, "sucre")), c(numeros(p, "sucre"), culture), c(numeros(p, "poids"), numeros(p, "diametre"))),
                  function(xy) test_deux_variables(p, xy[1], xy[2])$phrase, "")
verifier("aucune phrase avec NA", !any(grepl("NA", phrases, fixed = TRUE)))
verifier("deux fois la même variable : refusé", grepl("deux variables différentes", tryCatch(test_deux_variables(p, culture, culture), error = conditionMessage)))

cat("Matrice des corrélations\n")
m <- correlations_variables(p)
quant <- Filter(function(v) !length(v$categories), p$variables)
X <- vapply(quant, function(v) colonne(p, v$uid, obs), numeric(length(obs)))
i <- which(m$noms == "poids"); j <- which(m$noms == "polyphenols")
verifier("r = cor(use = \"pairwise.complete.obs\"), n par paire (valeurs manquantes)", isTRUE(all.equal(m$r, unname(cor(X, use = "pairwise.complete.obs")))) &&
         m$n[i, j] == sum(!is.na(X[, i]) & !is.na(X[, j])) && m$n[i, j] < length(obs))
verifier("p d'une paire = cor.test()", isTRUE(all.equal(m$p[i, j], cor.test(X[, i], X[, j])$p.value)))
haut <- upper.tri(m$p)
verifier("Benjamini–Hochberg sur les K(K − 1)/2 paires, matrice symétrique", isTRUE(all.equal(m$p_ajuste[haut], p.adjust(m$p[haut], "BH"))) &&
         isTRUE(all.equal(m$p_ajuste, t(m$p_ajuste))))
ms <- correlations_variables(p, "spearman", "bonferroni")
verifier("Spearman (approximation t) = cor.test(exact = FALSE), Bonferroni",
         isTRUE(all.equal(ms$p[i, j], suppressWarnings(cor.test(X[, i], X[, j], method = "spearman", exact = FALSE))$p.value)) &&
         isTRUE(all.equal(ms$p_ajuste[haut], p.adjust(ms$p[haut], "bonferroni"))))

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
d1$Ap <- NULL  # comme dans un projet enregistré avant (le projet d'exemple, refait, a maintenant Ap)
verifier("une seule prédictive : Ap = 1 par défaut (projets enregistrés avant)", ajuster(p, d1)$Ap == 1)
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
