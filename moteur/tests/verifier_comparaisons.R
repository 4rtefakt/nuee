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
verifier(sprintf("la phrase de Mann–Whitney nomme les groupes avec leurs médianes : « %s »", r$phrase),
         grepl(sprintf("; médianes bio = %s et conventionnel = %s, corrélation rang-bisériale = -?[0-9],[0-9]{2} \\(n = 60 et 60\\)\\.$",
                       sub(".", ",", sprintf("%.2f", median(a)), fixed = TRUE), sub(".", ",", sprintf("%.2f", median(b)), fixed = TRUE)), r$phrase))
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

cat("Mesures appariées\n")
# Wilcoxon apparié de référence, le même sous toutes les versions de R : exact sous 50 différences
# sans zéro ni ex aequo, sinon par la loi normale avec la correction de continuité (zéros retirés)
wilcox_ref <- function(b, a) {
  d <- b - a
  suppressWarnings(wilcox.test(b, a, paired = TRUE, exact = all(d != 0) && length(d) < 50 && !anyDuplicated(abs(d)), correct = TRUE))
}
# en long, une ligne par secteur et par année, dans le désordre : S1 commence en 2003, S2
# s'arrête en 2011 et n'a pas de densité en 2005
set.seed(4)
long <- expand.grid(annee = 2001:2012, secteur = c("S1", "S2", "S3"), stringsAsFactors = FALSE)
long$densite <- round(5 + 0.8 * (long$secteur == "S2") + rnorm(nrow(long)), 3)
long <- long[!(long$secteur == "S1" & long$annee %in% 2001:2002) & !(long$secteur == "S2" & long$annee == 2012), ]
long$densite[long$secteur == "S2" & long$annee == 2005] <- NA
long <- data.frame(nom = paste0("o", seq_len(nrow(long))), long[sample(nrow(long)), c("secteur", "annee", "densite")])
pl <- projet_depuis_tableau(long, "secteurs")
densite <- numeros(pl, "densite"); secteur <- numeros(pl, "secteur"); annee <- numeros(pl, "annee")
communes <- c(2003, 2004, 2006:2011)
serie <- function(s) long$densite[long$secteur == s][match(communes, long$annee[long$secteur == s])]
a <- serie("S1"); b <- serie("S2")
tt <- t.test(b, a, paired = TRUE)
w <- wilcox_ref(b, a)
r <- test_deux_variables(pl, densite, secteur, appariement = annee, niveaux = c("S1", "S2"))
verifier("t apparié = t.test(paired = TRUE) sur les années communes, quel que soit l'ordre des lignes", r$type == "apparie" && r$n == 8 &&
         identical(as.numeric(r$paires$cle), communes) && isTRUE(all.equal(r$tests$statistique[1], unname(tt$statistic))) && r$tests$ddl1[1] == 7 &&
         isTRUE(all.equal(r$tests$p[1], tt$p.value)) && isTRUE(all.equal(r$difference, c(unname(tt$estimate), as.numeric(tt$conf.int)))))
verifier("d_z de Cohen = différence moyenne / écart-type des différences", isTRUE(all.equal(r$tests$effet[1], mean(b - a) / sd(b - a))))
verifier("Wilcoxon = wilcox.test(paired = TRUE), exact sous 50 paires sans zéro ni ex aequo ; r rang-bisérial = 2V / (n(n + 1)/2) − 1",
         isTRUE(all.equal(r$tests$statistique[2], unname(w$statistic))) && isTRUE(all.equal(r$tests$p[2], w$p.value)) &&
         r$exact && grepl("exact", w$method) && isTRUE(all.equal(r$tests$effet[2], 2 * unname(w$statistic) / (8 * 9 / 2) - 1)))
verifier("lignes sans partenaire (avec leur année) et sans valeur, comptées", identical(r$sans_partenaire$a, c("2005", "2012")) &&
         identical(r$sans_partenaire$b, c("2001", "2002")) && r$manquantes == 1)
verifier("le même test quel que soit l'ordre des variables, A et B par défaut les deux premiers niveaux",
         isTRUE(all.equal(test_deux_variables(pl, secteur, densite, appariement = annee)$tests, r$tests)))
pid <- pl
pid$observation_id_names <- c("Observation", "secteur", "annee")
for (i in seq_along(pid$observations)) pid$observations[[i]]$ids <- c(long$nom[i], long$secteur[i], long$annee[i])
ri <- test_deux_variables(pid, "id:2", densite, appariement = "id:3", niveaux = c("S1", "S2"))
verifier("des identifiants pour les groupes et pour la clé : les mêmes paires", isTRUE(all.equal(ri$tests, r$tests)) && ri$cle$nom == "annee")
verifier("les niveaux d'un identifiant, dans l'ordre des nombres", identical(niveaux_a_tester(p3, "id:2"), c("9", "10", "100")) &&
         identical(niveaux_a_tester(pid, "id:2"), c("S1", "S2", "S3")))
verifier("phrase du t apparié, en français", grepl(paste0("^Test t apparié : t\\(7\\) = -?[0-9]+,[0-9]{2}, p [=<] 0,[0-9]+ ; différence moyenne S2 − S1 = -?[0-9]+,[0-9]{2} ",
                                                    "\\[IC 95 % -?[0-9]+,[0-9]{2} ; -?[0-9]+,[0-9]{2}\\], d_z de Cohen = -?[0-9]+,[0-9]{2} \\(n = 8 paires, appariées par annee\\)\\.$"), r$phrase))
options(nuee.langue = "en")
re <- test_deux_variables(pl, densite, secteur, appariement = annee, niveaux = c("S3", "S1"))
verifier(sprintf("en anglais : « %s »", re$phrase), grepl(paste0("^Paired t-test: t\\(9\\) = -?[0-9.]+, p [=<] 0\\.[0-9]+; mean difference S1 − S3 = -?[0-9.]+ ",
                                                                  "\\[95% CI -?[0-9.]+, -?[0-9.]+\\], Cohen's d_z = -?[0-9.]+ \\(n = 10 pairs matched on annee\\)\\.$"), re$phrase))
options(nuee.langue = NULL)
appariees <- function(base, ecart, k = seq_along(base)) {
  projet_depuis_tableau(data.frame(nom = paste0("o", seq_len(2 * length(base))), g = rep(c("A", "B"), each = length(base)), k = rep(k, 2),
                                   v = c(base, base + ecart)), "paires")
}
pa <- appariees(c(3, 1, 4, 1, 5, 9, 2, 6), c(0.1, 0.2, 0.15, 0.3, 0.25, 0.12, 0.2, 5))
r <- test_deux_variables(pa, numeros(pa, "v"), numeros(pa, "g"), appariement = numeros(pa, "k"))
verifier(sprintf("différences non normales : Wilcoxon conseillé, « %s »", r$phrase), r$conseille == "wilcoxon" && r$raison == "normalite" && r$shapiro_p < 0.05 &&
         grepl("^Test des rangs signés de Wilcoxon \\(p exacte\\) : V = 36, p = 0,00781 ; différence médiane B − A = 0,20, corrélation rang-bisériale = 1,00 \\(n = 8 paires, appariées par k\\)\\.$", r$phrase))
ecart <- c(0, 1, 1, 2, -1, 0, 2, 2, 1, 3)
pa <- appariees(1:10, ecart)
r <- test_deux_variables(pa, numeros(pa, "v"), numeros(pa, "g"), appariement = numeros(pa, "k"))
w <- wilcox.test((1:10) + ecart, 1:10, paired = TRUE, exact = FALSE, correct = TRUE)
r$conseille <- "wilcoxon"
verifier("ex aequo et différences nulles : zéros retirés, loi normale avec correction de continuité = wilcox.test(exact = FALSE) ; r de ces rangs",
         !r$exact && isTRUE(all.equal(r$tests$statistique[2], unname(w$statistic))) && isTRUE(all.equal(r$tests$p[2], w$p.value)) &&
         r$tests$statistique[2] == 33.5 && isTRUE(all.equal(r$tests$effet[2], 31 / 36)) &&
         grepl("p approchée par la loi normale, avec correction de continuité", phrase_test(r)))
# les données de la revue : sous R 4.6, wilcox.test() donnerait V = 29 (zéros classés) et une p exacte ;
# avant 4.6 (Linux), V = 19 par la loi normale : Nuée donne partout ce dernier calcul
ra <- c(5.1, 4.8, 6.0, 5.5, 4.9, 5.2, 6.1, 5.0)
rv <- tests_apparies(ra, ra + c(0, 0, 0.5, 0.5, 1, -0.5, 1.5, 2), "A", "B")
w <- wilcox.test(ra + c(0, 0, 0.5, 0.5, 1, -0.5, 1.5, 2), ra, paired = TRUE, exact = FALSE, correct = TRUE)
verifier("le même Wilcoxon sous toutes les versions de R : V = 19, p de la loi normale, r = 2V/21 − 1",
         rv$tests$statistique[2] == 19 && isTRUE(all.equal(rv$tests$p[2], w$p.value)) && !rv$exact && isTRUE(all.equal(rv$tests$effet[2], 2 * 19 / 21 - 1)))
# des différences constantes (après = avant + 0,1, au bruit des nombres à virgule près) : ni
# test t ni d_z, la normalité pas testée, Wilcoxon ; au-delà de 5 000 paires, la normalité pas
# testée et le t apparié
rk <- tests_apparies(c(5.1, 4.3, 6.7, 5.5, 3.9, 4.4), c(5.2, 4.4, 6.8, 5.6, 4.0, 4.5), "avant", "apres")
set.seed(9)
rg5 <- tests_apparies(rnorm(5001), rnorm(5001, 0.1), "A", "B")
verifier("différences constantes : pas de test t ni de d_z, Wilcoxon conseillé ; plus de 5 000 paires : normalité pas testée, t apparié",
         rk$raison == "constantes" && rk$conseille == "wilcoxon" && all(is.na(c(rk$tests$statistique[1], rk$tests$p[1], rk$tests$effet[1], rk$shapiro_p))) &&
         !is.na(rk$tests$p[2]) && rg5$raison == "nombre" && rg5$conseille == "t_apparie" && is.na(rg5$shapiro_p) && !is.na(rg5$tests$p[1]))
set.seed(5)
base <- rnorm(60)
pa <- appariees(base, rnorm(60, 0.3))
r <- test_deux_variables(pa, numeros(pa, "v"), numeros(pa, "g"), appariement = numeros(pa, "k"))
r$conseille <- "wilcoxon"
verifier("60 paires : Wilcoxon par la loi normale = wilcox.test(), et la phrase le dit", !r$exact &&
         isTRUE(all.equal(r$tests$p[2], wilcox.test(r$paires$b, r$paires$a, paired = TRUE)$p.value)) &&
         grepl("p approchée par la loi normale, avec correction de continuité", phrase_test(r)))
erreur <- function(...) tryCatch({ test_deux_variables(...); "" }, error = conditionMessage)
double <- long
double$annee[double$secteur == "S1" & double$annee == 2004] <- 2003
pd <- projet_depuis_tableau(double, "secteurs")
rd <- test_deux_variables(pd, densite, secteur, appariement = annee, niveaux = c("S1", "S2"))
verifier("une année deux fois pour un secteur : la paire A–B refusée, l'année dite, ses paires non testées et pourquoi",
         grepl("S1 a plusieurs lignes pour la même valeur de annee \\(2003\\)", rd$erreur) && is.null(rd$tests) && is.null(rd$phrase) &&
         identical(rd$toutes$non_teste, c(rep("S1 a deux lignes pour une même valeur de annee", 2), "")) && !is.na(rd$toutes$p_t[3]))
peu <- numeros_observations(pl)[long$annee %in% c(2003, 2004)]
p2b <- appariees(c(1, 2), c(1, 2))
verifier("moins de trois paires : l'erreur de la paire A–B, le tableau des paires à côté ; deux niveaux seulement : refusé",
         grepl("au moins trois paires : S1 et S2 n'ont que 2 valeur", test_deux_variables(pl, densite, secteur, observations = peu, appariement = annee)$erreur) &&
         grepl("au moins trois paires : A et B n'ont que 2 valeur", erreur(p2b, numeros(p2b, "v"), numeros(p2b, "g"), appariement = numeros(p2b, "k"))))
pa <- appariees(c(1, 2, 3, 5), 0)
verifier("la même valeur dans chaque paire : refusé", grepl("même valeur dans chaque paire", erreur(pa, numeros(pa, "v"), numeros(pa, "g"), appariement = numeros(pa, "k"))))
verifier("deux mesures appariées par une clé, la clé parmi les deux variables, deux qualitatives : refusé",
         grepl("s'apparient par l'observation", erreur(pl, densite, annee, appariement = secteur)) &&
         grepl("troisième variable", erreur(pl, densite, secteur, appariement = densite)) &&
         grepl("deux mesures d'une même observation", erreur(p, culture, variete, appariement = verger)))
rt <- test_deux_variables(pl, densite, secteur, appariement = annee)
ref_paire <- function(sa, sb) {
  a <- long[long$secteur == sa & !is.na(long$densite), ]
  b <- long[long$secteur == sb & !is.na(long$densite), ]
  an <- intersect(a$annee, b$annee)
  t.test(b$densite[match(an, b$annee)], a$densite[match(an, a$annee)], paired = TRUE)
}
pt <- lapply(list(c("S1", "S2"), c("S1", "S3"), c("S2", "S3")), function(s) ref_paire(s[1], s[2]))
p2 <- appariees(1:5, c(1, 3, 2, 5, 4))
verifier("trois secteurs : toutes les paires, chacune sur ses années communes = t.test(paired = TRUE), corrigées par Holm ; deux : aucune",
         identical(paste(rt$toutes$a, rt$toutes$b), c("S1 S2", "S1 S3", "S2 S3")) && identical(rt$toutes$n, c(8L, 10L, 10L)) &&
         isTRUE(all.equal(rt$toutes$p_t, vapply(pt, `[[`, 0, "p.value"))) && isTRUE(all.equal(rt$toutes$p_t_holm, p.adjust(rt$toutes$p_t, "holm"))) &&
         isTRUE(all.equal(c(rt$toutes$bas[1], rt$toutes$haut[1]), as.numeric(pt[[1]]$conf.int))) &&
         is.null(test_deux_variables(p2, numeros(p2, "v"), numeros(p2, "g"), appariement = numeros(p2, "k"))$toutes))
s4 <- rbind(long, data.frame(nom = c("x1", "x2"), secteur = "S4", annee = c(2001, 2002), densite = c(4, 5)))
p4 <- projet_depuis_tableau(s4, "secteurs")
r4 <- test_deux_variables(p4, densite, secteur, appariement = annee)
verifier("une paire de moins de trois années communes n'est pas testée, ni comptée dans la correction, et dit pourquoi",
         identical(r4$toutes$n[r4$toutes$b == "S4"], c(0L, 2L, 2L)) && all(is.na(r4$toutes$p_t[r4$toutes$b == "S4"])) &&
         isTRUE(all.equal(r4$toutes$p_t_holm[r4$toutes$b != "S4"], rt$toutes$p_t_holm)) &&
         identical(r4$toutes$non_teste[r4$toutes$b == "S4"], c("aucune valeur de annee en commun", rep("2 valeur(s) de annee en commun, il en faut 3", 2))) &&
         all(r4$toutes$non_teste[r4$toutes$b != "S4"] == ""))
# B = A partout, C = A + 1 : la paire A–B n'a que des différences nulles, A–C et B–C des
# différences constantes (Wilcoxon seul)
trois <- projet_depuis_tableau(data.frame(nom = paste0("t", 1:12), g = rep(c("A", "B", "C"), each = 4), k = rep(1:4, 3),
                                          v = c(1, 2, 3, 5, 1, 2, 3, 5, 2, 3, 4, 6)), "trois")
rz <- test_deux_variables(trois, numeros(trois, "v"), numeros(trois, "g"), appariement = numeros(trois, "k"), niveaux = c("A", "C"))
verifier("des différences toutes nulles : la paire pas testée ; constantes : Wilcoxon seul, et pourquoi pas de t",
         identical(rz$toutes$non_teste, c("la même valeur dans chaque paire", rep("différences constantes : pas de test t", 2))) &&
         all(is.na(rz$toutes$p_t)) && all(is.na(rz$toutes$p_w[1])) && all(!is.na(rz$toutes$p_w[2:3])) && rz$raison == "constantes" && !grepl("NA", rz$phrase))
# le tableau de toutes les paires se garde d'une demande à l'autre (memo : le serveur, par
# révision du projet) : un autre A–B ne le refait pas
calculs <- 0
garde <- NULL
memo <- function(calcul) { if (is.null(garde)) { calculs <<- calculs + 1; garde <<- calcul() }; garde }
m1 <- test_deux_variables(pl, densite, secteur, appariement = annee, niveaux = c("S1", "S2"), memo = memo)
m2 <- test_deux_variables(pl, densite, secteur, appariement = annee, niveaux = c("S2", "S3"), memo = memo)
verifier("le tableau de toutes les paires gardé : fait une fois pour deux paires A–B", calculs == 1 && identical(m1$toutes, m2$toutes) &&
         identical(m1$toutes, rt$toutes) && m2$a == "S2")
set.seed(7)
complet <- expand.grid(annee = 2001:2008, secteur = c("S1", "S2", "S3", "S4"), stringsAsFactors = FALSE)
# (sans ex aequo : le Wilcoxon de chaque paire est exact, comme celui de pairwise.wilcox.test())
complet$densite <- 3 + rnorm(nrow(complet)) + 0.9 * (complet$secteur == "S3")
pc <- projet_depuis_tableau(data.frame(nom = paste0("c", seq_len(nrow(complet))), complet[c("secteur", "annee", "densite")]), "complet")
rc <- test_deux_variables(pc, numeros(pc, "densite"), numeros(pc, "secteur"), appariement = numeros(pc, "annee"))
ptt <- pairwise.t.test(complet$densite, complet$secteur, paired = TRUE, p.adjust.method = "holm")$p.value
pwt <- suppressWarnings(pairwise.wilcox.test(complet$densite, complet$secteur, paired = TRUE, p.adjust.method = "holm"))$p.value
verifier("les mêmes années partout : = pairwise.t.test() et pairwise.wilcox.test() avec paired = TRUE, Holm",
         isTRUE(all.equal(rc$toutes$p_t_holm, ptt[!is.na(ptt)])) && isTRUE(all.equal(rc$toutes$p_w_holm, pwt[!is.na(pwt)])))
# les paires dans chaque site (script 13, section 3 : les secteurs d'un même groupe) : S1, S2, S3
# dans A, S4 et S5 dans B, S6 seul dans C ; S3 commence en 2004, S4 n'a pas de densité en 2005
set.seed(8)
sites <- c(S1 = "A", S2 = "A", S3 = "A", S4 = "B", S5 = "B", S6 = "C")
lg <- expand.grid(annee = 2001:2010, secteur = names(sites), stringsAsFactors = FALSE)
lg$site <- unname(sites[lg$secteur])
lg$densite <- round(4 + rnorm(nrow(lg)) + 0.7 * (lg$secteur %in% c("S2", "S5")), 3)
lg$effort <- round(runif(nrow(lg), 1, 3), 1)
lg <- lg[!(lg$secteur == "S3" & lg$annee <= 2003), ]
lg$densite[lg$secteur == "S4" & lg$annee == 2005] <- NA
lg <- data.frame(nom = paste0("g", seq_len(nrow(lg))), lg[sample(nrow(lg)), c("secteur", "site", "annee", "densite", "effort")])
pg <- projet_depuis_tableau(lg, "sites")
gv <- function(nom) numeros(pg, nom)
dans_site <- function(projet, ...) test_deux_variables(projet, gv("densite"), gv("secteur"), appariement = gv("annee"), ...)
rg <- dans_site(pg, dans = gv("site"))
ref <- do.call(rbind, lapply(split(names(sites), sites), function(s) {
  if (length(s) < 2) return(NULL)
  do.call(rbind, lapply(combn(s, 2, simplify = FALSE), function(cb) {
    a <- lg[lg$secteur == cb[1] & !is.na(lg$densite), ]
    b <- lg[lg$secteur == cb[2] & !is.na(lg$densite), ]
    an <- intersect(a$annee, b$annee)
    va <- a$densite[match(an, a$annee)]
    vb <- b$densite[match(an, b$annee)]
    tt <- t.test(vb, va, paired = TRUE)
    data.frame(groupe = sites[[cb[1]]], a = cb[1], b = cb[2], n = length(an), bas = tt$conf.int[1], haut = tt$conf.int[2], p_t = tt$p.value,
               p_w = wilcox_ref(vb, va)$p.value, stringsAsFactors = FALSE)
  }))
}))
verifier("dans chaque site : les paires de ses secteurs, site après site, = t.test() et wilcox.test(paired = TRUE), Holm sur ces paires seules",
         identical(paste(rg$toutes$groupe, rg$toutes$a, rg$toutes$b), c("A S1 S2", "A S1 S3", "A S2 S3", "B S4 S5")) &&
         identical(paste(rg$toutes$groupe, rg$toutes$a, rg$toutes$b), paste(ref$groupe, ref$a, ref$b)) && identical(rg$toutes$n, c(10L, 7L, 7L, 9L)) &&
         isTRUE(all.equal(rg$toutes[c("bas", "haut", "p_t", "p_w")], ref[c("bas", "haut", "p_t", "p_w")], check.attributes = FALSE)) &&
         isTRUE(all.equal(rg$toutes$p_t_holm, p.adjust(ref$p_t, "holm"))) && isTRUE(all.equal(rg$toutes$p_w_holm, p.adjust(ref$p_w, "holm"))))
r0 <- dans_site(pg)
verifier("le test de A et B ne change pas, le groupe des paires est nommé", identical(rg$tests, r0$tests) && nrow(r0$toutes) == 15 &&
         identical(rg$dans$nom, "site") && is.null(rg$sans_groupe) && is.null(rg$note_comparaisons))
pgi <- pg
pgi$observation_id_names <- c("Observation", "site")
for (i in seq_along(pgi$observations)) pgi$observations[[i]]$ids <- c(lg$nom[i], lg$site[i])
verifier("un identifiant des observations (le site) fait les groupes : le même tableau", isTRUE(all.equal(dans_site(pgi, dans = "id:2")$toutes, rg$toutes)))
deux_sites <- lg
deux_sites$site[deux_sites$secteur == "S2" & deux_sites$annee == 2004] <- "B"
verifier("un secteur dans deux sites : refusé, le secteur et les sites dits",
         grepl("^S2 a des lignes pour plusieurs valeurs de site \\(A, B\\) : un niveau doit appartenir à un seul groupe$",
               erreur(projet_depuis_tableau(deux_sites, "sites"), gv("densite"), gv("secteur"), appariement = gv("annee"), dans = gv("site"))))
verifier("le groupe des paires : la clé, l'une des deux variables ou une mesure, refusé",
         grepl("quatrième variable", erreur(pg, gv("densite"), gv("secteur"), appariement = gv("annee"), dans = gv("annee"))) &&
         grepl("quatrième variable", erreur(pg, gv("densite"), gv("secteur"), appariement = gv("annee"), dans = gv("secteur"))) &&
         grepl("variable qualitative ou un identifiant", erreur(pg, gv("densite"), gv("secteur"), appariement = gv("annee"), dans = gv("effort"))))
sans <- lg
sans$site[sans$secteur == "S6"] <- NA
rs <- dans_site(projet_depuis_tableau(sans, "sites"), dans = gv("site"))
verifier("un secteur sans site : listé, hors des paires", identical(rs$sans_groupe, "S6") && isTRUE(all.equal(rs$toutes, rg$toutes)))
seuls <- lg
seuls$site <- seuls$secteur
rn <- dans_site(projet_depuis_tableau(seuls, "sites"), dans = gv("site"))
verifier("chaque secteur seul dans son site : pas de paires, et la note le dit", is.null(rn$toutes) && grepl("Aucune valeur de site", rn$note_comparaisons))
nombreux <- expand.grid(annee = 1:4, secteur = sprintf("s%02d", 1:60), stringsAsFactors = FALSE)
nombreux$site <- sprintf("site%02d", (match(nombreux$secteur, sprintf("s%02d", 1:60)) + 1) %/% 2)
nombreux$densite <- round(rnorm(nrow(nombreux), 5), 2)
pn <- projet_depuis_tableau(data.frame(nom = paste0("n", seq_len(nrow(nombreux))), nombreux[c("secteur", "site", "annee", "densite")]), "nombreux")
nv <- function(nom) numeros(pn, nom)
rgn <- test_deux_variables(pn, nv("densite"), nv("secteur"), appariement = nv("annee"), dans = nv("site"))
decale <- lg
decale$annee[decale$secteur == "S4"] <- decale$annee[decale$secteur == "S4"] + 20
rsep <- dans_site(projet_depuis_tableau(decale, "sites"), niveaux = c("S1", "S4"), dans = gv("site"))
verifier("dans chaque site, A et B de deux sites sans année commune : leur erreur à part, le tableau des paires des sites toujours là",
         grepl("au moins trois paires : S1 et S4 n'ont que 0 valeur", rsep$erreur) && is.null(rsep$tests) &&
         identical(paste(rsep$toutes$a, rsep$toutes$b), c("S1 S2", "S1 S3", "S2 S3", "S4 S5")) &&
         identical(rsep$toutes$non_teste, c("", "", "", "aucune valeur de annee en commun")) && isTRUE(all.equal(rsep$toutes$p_t[1:3], rg$toutes$p_t[1:3])))
verifier("60 secteurs, 2 par site : les 30 paires de chaque site (au-delà de 50 niveaux, c'est le plus grand groupe qui compte)",
         nrow(rgn$toutes) == 30 && all(rgn$toutes$n == 4) && is.null(rgn$note_comparaisons) &&
         is.null(test_deux_variables(pn, nv("densite"), nv("secteur"), appariement = nv("annee"))$toutes))
# le test de Friedman avant les paires : les blocs complets (les années où chaque secteur a une
# valeur) en matrice, faits ici, pour friedman.test() ; le W de Kendall = χ² / (n (k − 1)) à la main
blocs_complets <- function(donnees, niveaux) {
  vus <- lapply(niveaux, function(s) donnees[donnees$secteur == s & !is.na(donnees$densite), ])
  annees <- sort(Reduce(intersect, lapply(vus, `[[`, "annee")))
  list(M = vapply(vus, function(x) x$densite[match(annees, x$annee)], numeric(length(annees))),
       laissees = length(unique(unlist(lapply(vus, `[[`, "annee")))) - length(annees))
}
bc <- blocs_complets(long, c("S1", "S2", "S3"))
ft <- friedman.test(bc$M)
fr3 <- rt$friedman
verifier(sprintf("Friedman avant les paires : = friedman.test() sur les %d années où les trois secteurs ont une valeur, %d laissées de côté ; W = χ² / (n (k − 1))",
                 nrow(bc$M), bc$laissees),
         nrow(fr3) == 1 && fr3$n == 8 && fr3$k == 3 && fr3$laissees == 4 && bc$laissees == 4 && isTRUE(all.equal(fr3$statistique, unname(ft$statistic))) &&
           fr3$ddl == 2 && isTRUE(all.equal(fr3$p, ft$p.value)) && isTRUE(all.equal(fr3$W, unname(ft$statistic) / (8 * 2))) && fr3$non_teste == "")
verifier(sprintf("la phrase de Friedman, en français : « %s »", fr3$phrase),
         grepl(paste0("^Test de Friedman : χ²\\(2\\) = [0-9]+,[0-9]{2}, p [=<] 0,[0-9]+ ; W de Kendall = [0-9],[0-9]{2} ",
                      "\\(n = 8 valeurs de annee où les 3 niveaux ont tous une valeur ; 4 laissées de côté, un niveau sans valeur\\)\\.$"), fr3$phrase))
# (W de Kendall à la main, sans ex aequo : 12 S / (n² k (k² − 1)), S la somme des carrés des écarts des
# sommes de rangs des k = 4 secteurs à leur moyenne, les rangs pris dans chacune des n = 8 années)
M4 <- vapply(c("S1", "S2", "S3", "S4"), function(s) complet$densite[complet$secteur == s][order(complet$annee[complet$secteur == s])], numeric(8))
R4 <- colSums(t(apply(M4, 1, rank)))
w_main <- 12 * sum((R4 - 8 * 5 / 2)^2) / (8^2 * 4 * (4^2 - 1))
fc <- friedman.test(densite ~ secteur | annee, data = complet)
verifier("toutes les années complètes : = friedman.test(densite ~ secteur | annee), W de Kendall = 12 S / (n² k (k² − 1)) à la main",
         rc$friedman$n == 8 && rc$friedman$laissees == 0 && isTRUE(all.equal(rc$friedman$p, fc$p.value)) && isTRUE(all.equal(rc$friedman$W, w_main)) &&
           isTRUE(all.equal(rc$friedman$statistique, unname(fc$statistic))) && !grepl("laissée", rc$friedman$phrase))
verifier("le test de Friedman ne dépend pas de A et B (il est celui des deux tests appariés)",
         identical(test_deux_variables(pc, numeros(pc, "densite"), numeros(pc, "secteur"), appariement = numeros(pc, "annee"), niveaux = c("S3", "S4"))$friedman, rc$friedman))
# dans chaque site : A et B ont trois secteurs (A sans 2001 pour S3), C deux, D trois qui n'ont
# qu'une année en commun ; un test par site testable, Holm sur ces sites
set.seed(12)
sites2 <- c(S1 = "A", S2 = "A", S3 = "A", S4 = "B", S5 = "B", S6 = "B", S7 = "C", S8 = "C", S9 = "D", S10 = "D", S11 = "D")
lf <- expand.grid(annee = 2001:2009, secteur = names(sites2), stringsAsFactors = FALSE)
lf$site <- unname(sites2[lf$secteur])
lf$densite <- round(3 + rnorm(nrow(lf)) + 0.8 * (lf$secteur %in% c("S2", "S6")), 3)
lf <- lf[!(lf$secteur == "S3" & lf$annee == 2001) & !(lf$secteur == "S9" & lf$annee > 2003) & !(lf$secteur == "S10" & lf$annee < 2003) &
           !(lf$secteur == "S11" & lf$annee == 2005), ]
lf <- data.frame(nom = paste0("f", seq_len(nrow(lf))), lf[sample(nrow(lf)), c("secteur", "site", "annee", "densite")])
pf <- projet_depuis_tableau(lf, "sites friedman")
rf2 <- test_deux_variables(pf, numeros(pf, "densite"), numeros(pf, "secteur"), appariement = numeros(pf, "annee"), dans = numeros(pf, "site"))$friedman
fa <- friedman.test(blocs_complets(lf, c("S1", "S2", "S3"))$M)
fb <- friedman.test(blocs_complets(lf, c("S4", "S5", "S6"))$M)
verifier("dans chaque site : un test de Friedman par site sur ses secteurs et ses années complètes = friedman.test(), Holm sur les sites testés = p.adjust()",
         identical(rf2$groupe, c("A", "B", "C", "D")) && identical(rf2$n[1:2], c(8L, 9L)) && isTRUE(all.equal(rf2$p[1:2], c(fa$p.value, fb$p.value))) &&
           isTRUE(all.equal(rf2$p_holm[1:2], p.adjust(c(fa$p.value, fb$p.value), "holm"))) && all(is.na(rf2$p[3:4])) && all(is.na(rf2$p_holm[3:4])) &&
           grepl("^Test de Friedman dans A \\(site\\) : χ²\\(2\\) = .*, p de Holm [=<] [0-9]+(,[0-9]+)? sur 2 tests ; W de Kendall = .* où ses 3 niveaux ont tous une valeur ; 1 laissée de côté, un niveau sans valeur\\)\\.$",
                 rf2$phrase[1]))
verifier("un site de deux secteurs, un site sans deux années complètes : pas de test, et pourquoi",
         identical(rf2$non_teste[3:4], c("2 niveau(x), il en faut 3", "1 valeur(s) de annee où chaque niveau a une valeur, il en faut 2")) &&
           all(is.na(rf2$phrase[3:4])) && rf2$n[4] == 1)
verifier("une année deux fois pour un secteur : pas de test de Friedman, et pourquoi", is.na(rd$friedman$p) &&
           identical(rd$friedman$non_teste, "S1 a deux lignes pour une même valeur de annee"))
options(nuee.langue = "en")
fen <- test_deux_variables(pf, numeros(pf, "densite"), numeros(pf, "secteur"), appariement = numeros(pf, "annee"), dans = numeros(pf, "site"))$friedman
options(nuee.langue = NULL)
verifier(sprintf("en anglais : « %s »", fen$phrase[2]),
         grepl("^Friedman test within B \\(site\\): χ²\\(2\\) = [0-9.]+, p [=<] [0-9.]+, Holm's p [=<] [0-9.]+ over 2 tests; Kendall's W = [0-9.]+ \\(n = 9 values of annee where its 3 levels all have a value\\)\\.$",
               fen$phrase[2]) && identical(fen$non_teste[3], "2 level(s), 3 needed"))
# en large : la même ligne porte les deux mesures (avant, après)
set.seed(6)
large <- data.frame(nom = paste0("o", 1:25), site = rep(c("A", "B"), length.out = 25), avant = round(rnorm(25, 5), 2))
large$apres <- round(large$avant + rnorm(25, 0.3, 0.6), 2)
large$apres[c(4, 9)] <- NA
large$avant[17] <- NA
pw <- projet_depuis_tableau(large, "avant-après")
avant <- numeros(pw, "avant"); apres <- numeros(pw, "apres")
ok <- complete.cases(large$avant, large$apres)
tt <- t.test(large$apres[ok], large$avant[ok], paired = TRUE)
w <- wilcox_ref(large$apres[ok], large$avant[ok])
r <- test_deux_variables(pw, avant, apres, appariement = "observation")
verifier(sprintf("deux mesures d'une même ligne : après − avant = t.test() et wilcox.test(paired = TRUE), « %s »", r$phrase),
         r$type == "apparie" && r$ligne && r$n == 22 && r$manquantes == 3 && r$a == "avant" && r$b == "apres" && is.null(r$toutes) &&
         isTRUE(all.equal(r$tests$p, c(tt$p.value, w$p.value))) && isTRUE(all.equal(r$difference, c(unname(tt$estimate), as.numeric(tt$conf.int)))) &&
         identical(r$paires$cle, large$nom[ok]) && grepl("\\(n = 22 paires, une par observation\\)\\.$", r$phrase))
verifier("deux mesures par une clé, une mesure et des groupes par la ligne, moins de trois lignes complètes : refusé",
         grepl("s'apparient par l'observation", erreur(pw, avant, apres, appariement = numeros(pw, "site"))) &&
         grepl("par une troisième variable", erreur(pw, avant, numeros(pw, "site"), appariement = "observation")) &&
         grepl("2 observation\\(s\\) seulement", erreur(pw, avant, apres, observations = c(1, 2, 4), appariement = "observation")))

cat("Entre classes, appariées par une clé\n")
# les secteurs S1 et S2 de `long`, chaque variable sur ses années communes : nnicad manque pour S1
# en 2008, tmnic est négative (pas de log2 du rapport), rare n'a que deux années communes
set.seed(9)
lv <- long
lv$nnicad <- round(3 + 0.4 * (lv$secteur == "S2") + rnorm(nrow(lv)), 3)
lv$tmnic <- round(-1 + rnorm(nrow(lv)), 3)
lv$rare <- ifelse(lv$annee %in% c(2003, 2004), round(rnorm(nrow(lv)), 3), NA)
lv$nnicad[lv$secteur == "S1" & lv$annee == 2008] <- NA
pv <- projet_depuis_tableau(lv, "secteurs")
cl <- numeros(pv, "secteur")
an <- numeros(pv, "annee")
paire <- function(nom) {
  a <- lv[lv$secteur == "S1" & !is.na(lv[[nom]]), ]
  b <- lv[lv$secteur == "S2" & !is.na(lv[[nom]]), ]
  k <- sort(intersect(a$annee, b$annee))
  list(a = a[[nom]][match(k, a$annee)], b = b[[nom]][match(k, b$annee)])
}
vt <- c("densite", "nnicad", "tmnic")
ref <- lapply(vt, paire)
rv <- tests_variables(pv, cl, c("S1", "S2"), test = "t_apparie", appariement = an)
tb <- rv$tableau[match(vt, rv$tableau$nom), ]
tt <- lapply(ref, function(x) t.test(x$b, x$a, paired = TRUE))
verifier("t apparié de chaque variable = t.test(paired = TRUE) sur ses années communes, le nombre de paires par variable",
         identical(tb$n_paires, c(8L, 8L, 9L)) && identical(tb$n_paires, vapply(ref, function(x) length(x$a), 0L)) &&
         isTRUE(all.equal(tb$p, vapply(tt, `[[`, 0, "p.value"))) && isTRUE(all.equal(tb$statistique, vapply(tt, function(x) unname(x$statistic), 0))))
verifier("moyennes des paires, log2 de leur rapport (positives), d_z = différence moyenne / écart-type des différences",
         isTRUE(all.equal(tb$moyenne_a, vapply(ref, function(x) mean(x$a), 0))) && isTRUE(all.equal(tb$moyenne_b, vapply(ref, function(x) mean(x$b), 0))) &&
         isTRUE(all.equal(tb$log2_rapport[1:2], vapply(ref[1:2], function(x) log2(mean(x$b) / mean(x$a)), 0))) && is.na(tb$log2_rapport[3]) &&
         isTRUE(all.equal(tb$d, vapply(ref, function(x) mean(x$b - x$a) / sd(x$b - x$a), 0))))
rr <- rv$tableau[rv$tableau$nom == "rare", ]
verifier("deux paires seulement : pas de test, hors de la correction ; Benjamini–Hochberg = p.adjust() ; ni la classe ni la clé testées",
         rr$n_paires == 2 && is.na(rr$p) && is.na(rr$p_ajuste) && isTRUE(all.equal(tb$p_ajuste, p.adjust(tb$p, "BH"))) &&
         identical(sort(rv$tableau$nom), sort(c(vt, "rare"))) && rv$test == "t_apparie" && rv$cle$nom == "annee")
rw <- tests_variables(pv, cl, c("S1", "S2"), test = "wilcoxon", correction = "bonferroni", appariement = an)
pw_ref <- vapply(ref, function(x) wilcox.test(x$b, x$a, paired = TRUE)$p.value, 0)
verifier("Wilcoxon de chaque variable = wilcox.test(paired = TRUE), Bonferroni = p.adjust()",
         isTRUE(all.equal(rw$tableau$p[match(vt, rw$tableau$nom)], pw_ref)) && isTRUE(all.equal(rw$tableau$p_ajuste, p.adjust(rw$tableau$p, "bonferroni"))))
verifier("chaque variable : le même t apparié que Deux variables",
         all(vapply(vt, function(nom) isTRUE(all.equal(tb$p[tb$nom == nom],
                    test_deux_variables(pv, numeros(pv, nom), cl, appariement = an, niveaux = c("S1", "S2"))$tests$p[1])), TRUE)))
pvi <- pv
pvi$observation_id_names <- c("Observation", "an")
for (i in seq_along(pvi$observations)) pvi$observations[[i]]$ids <- c(lv$nom[i], lv$annee[i])
ri <- tests_variables(pvi, cl, c("S1", "S2"), test = "t_apparie", appariement = "id:2")
verifier("un identifiant des observations pour clé : les mêmes paires (la variable annee, testée alors, ne change dans aucune paire : pas de p)",
         isTRUE(all.equal(ri$tableau[match(vt, ri$tableau$nom), c("n_paires", "p", "d")], tb[c("n_paires", "p", "d")], check.attributes = FALSE)) &&
         is.na(ri$tableau$p[ri$tableau$nom == "annee"]) && ri$cle$nom == "an")
dv <- lv
dv$annee[dv$secteur == "S1" & dv$annee == 2004] <- 2003
erreur_tv <- function(...) tryCatch({ tests_variables(...); "" }, error = conditionMessage)
verifier("une année deux fois pour une classe : refusé comme dans Deux variables",
         grepl("^S1 a plusieurs lignes pour la même valeur de annee \\(2003\\) : la clé d'appariement doit désigner une seule ligne par niveau$",
               erreur_tv(projet_depuis_tableau(dv, "secteurs"), cl, c("S1", "S2"), test = "t_apparie", appariement = an)))
verifier("un test apparié sans clé, une clé sans test apparié, la classe pour clé : refusé",
         grepl("clé d'appariement", erreur_tv(pv, cl, c("S1", "S2"), test = "t_apparie")) &&
         grepl("avec un test apparié", erreur_tv(pv, cl, c("S1", "S2"), test = "welch", appariement = an)) &&
         grepl("autre variable que la classe", erreur_tv(pv, cl, c("S1", "S2"), test = "t_apparie", appariement = cl)))

cat("Avant/après avec témoin (BACI)\n")
# quatre secteurs suivis de 2001 à 2012, en long et dans le désordre : S1 (l'impact) gagne 1,2 à
# partir de 2007 et n'a pas de densité en 2003 ; les témoins S2 et S3 n'ont pas de ligne en 2012 ;
# S4 n'est ni l'un ni l'autre
set.seed(11)
bl <- expand.grid(annee = 2001:2012, secteur = c("S1", "S2", "S3", "S4"), stringsAsFactors = FALSE)
bl$densite <- round(5 + rnorm(nrow(bl)) + 1.2 * (bl$secteur == "S1" & bl$annee >= 2007), 3)
bl <- bl[!(bl$secteur %in% c("S2", "S3") & bl$annee == 2012), ]
bl$densite[bl$secteur == "S1" & bl$annee == 2003] <- NA
bl <- data.frame(nom = paste0("b", seq_len(nrow(bl))), bl[sample(nrow(bl)), c("secteur", "annee", "densite")])
pb <- projet_depuis_tableau(bl, "baci")
bv <- function(nom) numeros(pb, nom)
# d_t calculé ici : la moyenne de l'impact moins celle des témoins, les années où les deux en ont une
ecarts <- function(impact, temoins) {
  ok <- !is.na(bl$densite)
  mi <- tapply(bl$densite[ok & bl$secteur %in% impact], bl$annee[ok & bl$secteur %in% impact], mean)
  mt <- tapply(bl$densite[ok & bl$secteur %in% temoins], bl$annee[ok & bl$secteur %in% temoins], mean)
  annees <- sort(as.numeric(intersect(names(mi), names(mt))))
  data.frame(annee = annees, d = unname(mi[as.character(annees)] - mt[as.character(annees)]))
}
e <- ecarts("S1", c("S2", "S3"))
rb <- baci(pb, bv("densite"), bv("annee"), bv("secteur"), "S1", c("S2", "S3"), evenement = 2007)
db <- e$d[e$annee < 2007]
da <- e$d[e$annee >= 2007]
w <- t.test(da, db)
verifier("d_t = moyenne de l'impact − moyenne des témoins, les années où les deux en ont ; les années de l'un seul et les lignes sans valeur, dites",
         isTRUE(all.equal(rb$serie$temps, e$annee)) && isTRUE(all.equal(rb$serie$ecart, e$d)) && all(rb$serie$n_temoins == 2) &&
         identical(rb$seuls$impact, 2012) && identical(rb$seuls$temoins, 2003) && rb$manquantes == 1 && is.null(rb$repetees))
verifier("après contre avant : t de Welch = t.test(), Mann–Whitney = wilcox.test(), sur les d_t calculés ici ; r rang-bisérial",
         isTRUE(all.equal(rb$tests$p[1], w$p.value)) && isTRUE(all.equal(rb$tests$statistique[1], unname(w$statistic))) &&
         isTRUE(all.equal(rb$tests$ddl1[1], unname(w$parameter))) && isTRUE(all.equal(rb$difference, c(mean(da) - mean(db), as.numeric(w$conf.int)))) &&
         isTRUE(all.equal(rb$tests$p[3], wilcox.test(da, db)$p.value)) && identical(rb$periodes$n, c(length(db), length(da))) &&
         isTRUE(all.equal(rb$tests$effet[3], 2 * unname(wilcox.test(da, db)$statistic) / (length(da) * length(db)) - 1)))
periode <- factor(ifelse(e$annee < 2007, "avant", "pendant"), levels = c("avant", "pendant"))
lm_script <- lm(e$d ~ periode)
verifier("t de Student = son contraste() : lm(d_t ~ période), l'effet, son intervalle et sa valeur p",
         isTRUE(all.equal(rb$tests$p[2], summary(lm_script)$coefficients[2, 4])) && isTRUE(all.equal(mean(da) - mean(db), unname(coef(lm_script)[2]))) &&
         isTRUE(all.equal(c(rb$tests$bas[2], rb$tests$haut[2]), unname(confint(lm_script)[2, ]))) &&
         isTRUE(all.equal(rb$tests$p[2], t.test(da, db, var.equal = TRUE)$p.value)))
rf <- baci(pb, bv("densite"), bv("annee"), bv("secteur"), "S1", c("S2", "S3"), evenement = 2005, fin = 2009, test = "mann_whitney")
af <- e$d[e$annee >= 2005 & e$annee <= 2009]
bf <- e$d[e$annee < 2005]
verifier("une fin : après = de l'événement à la fin comprise, les années suivantes hors du test",
         identical(rf$serie$periode, ifelse(e$annee < 2005, "avant", ifelse(e$annee <= 2009, "apres", "hors"))) && rf$hors == 2 && rf$fin == 2009 &&
         isTRUE(all.equal(rf$tests$p, c(t.test(af, bf)$p.value, t.test(af, bf, var.equal = TRUE)$p.value, wilcox.test(af, bf)$p.value))))
r2 <- baci(pb, bv("densite"), bv("annee"), bv("secteur"), c("S1", "S4"), c("S2", "S3"), evenement = 2007)
verifier("deux unités d'impact : la moyenne de leurs lignes chaque année (S4 seul en 2003)",
         isTRUE(all.equal(r2$serie$ecart, ecarts(c("S1", "S4"), c("S2", "S3"))$d)) && r2$serie$n_impact[r2$serie$temps == 2003] == 1 &&
         all(r2$serie$n_impact[r2$serie$temps != 2003] == 2))
pbi <- pb
pbi$observation_id_names <- c("Observation", "secteur_id", "an")
for (i in seq_along(pbi$observations)) pbi$observations[[i]]$ids <- c(bl$nom[i], bl$secteur[i], bl$annee[i])
ri <- baci(pbi, bv("densite"), "id:3", "id:2", "S1", c("S2", "S3"), evenement = 2007)
verifier("le temps et l'unité en identifiants des observations : les mêmes écarts et les mêmes tests",
         isTRUE(all.equal(ri$serie$ecart, rb$serie$ecart)) && isTRUE(all.equal(ri$tests, rb$tests)) && ri$temps$nom == "an" && ri$unite$nom == "secteur_id")
bs <- bl
bs$site <- c(S1 = "A", S2 = "B", S3 = "B", S4 = "C")[bs$secteur]
pbs <- projet_depuis_tableau(bs, "baci")
rsite <- baci(pbs, numeros(pbs, "densite"), numeros(pbs, "annee"), numeros(pbs, "site"), "A", "B", evenement = 2007)
verifier("l'unité = le site : chaque ligne (secteur) compte une fois, les mêmes écarts ; le nombre d'années à plusieurs lignes, dit",
         isTRUE(all.equal(rsite$serie$ecart, rb$serie$ecart)) && identical(rsite$repetees$unite, "B") && rsite$repetees$temps == 11)
rs <- baci(pb, bv("densite"), bv("annee"), bv("secteur"), "S1", c("S2", "S3"))
verifier("sans événement : la série des écarts seule", isTRUE(all.equal(rs$serie$ecart, rb$serie$ecart)) && is.null(rs$tests) && is.null(rs$phrase))
erreur_b <- function(...) tryCatch({ baci(...); "" }, error = conditionMessage)
verifier("moins de deux années dans une période, une unité impact et témoin, pas de témoin : refusé",
         grepl("au moins deux valeurs de annee dans chaque période, avant et après 2002 : il y en a 1 avant et 9 après",
               erreur_b(pb, bv("densite"), bv("annee"), bv("secteur"), "S1", c("S2", "S3"), evenement = 2002)) &&
         grepl("à la fois impact et témoin : S2", erreur_b(pb, bv("densite"), bv("annee"), bv("secteur"), c("S1", "S2"), c("S2", "S3"), evenement = 2007)) &&
         grepl("au moins une unité d'impact et une unité témoin", erreur_b(pb, bv("densite"), bv("annee"), bv("secteur"), "S1", character(0), evenement = 2007)))
verifier("une fin avant l'événement, un temps qui n'est pas un nombre, une unité ou une réponse du mauvais type, une variable pour deux rôles : refusé",
         grepl("vient avant l'événement", erreur_b(pb, bv("densite"), bv("annee"), bv("secteur"), "S1", "S2", evenement = 2007, fin = 2005)) &&
         grepl("^Le temps \\(secteur_id\\) doit être fait de nombres \\(l'année\\) ou de dates \\([^)]*\\) : « S[1-4] » n'est ni un nombre ni une date$",
               erreur_b(pbi, bv("densite"), "id:2", bv("secteur"), "S1", "S2", evenement = 2007)) &&
         grepl("qualitative ou un identifiant", erreur_b(pbi, bv("densite"), "id:3", bv("annee"), "S1", "S2", evenement = 2007)) &&
         grepl("réponse doit être une variable quantitative", erreur_b(pb, bv("secteur"), bv("annee"), "id:1", "S1", "S2")) &&
         grepl("trois variables différentes", erreur_b(pb, bv("densite"), bv("annee"), bv("annee"), "S1", "S2", evenement = 2007)))
verifier(sprintf("phrase du t de Welch, en français : « %s »", rb$phrase),
         grepl(paste0("^Test t de Welch \\(BACI\\), écart de densite impact \\(S1\\) − témoins \\(S2, S3\\), après contre avant : ",
                      "t\\([0-9]+(,[0-9])?\\) = -?[0-9]+,[0-9]{2}, p [=<] 0,[0-9]+ ; écart moyen -?[0-9]+,[0-9]{2} avant et -?[0-9]+,[0-9]{2} après, ",
                      "changement = -?[0-9]+,[0-9]{2} \\[IC 95 % -?[0-9]+,[0-9]{2} ; -?[0-9]+,[0-9]{2}\\], d de Cohen = -?[0-9]+,[0-9]{2} ",
                      "\\(n = 5 avant et 5 après ; après : annee ≥ 2007\\)\\.$"), rb$phrase))
options(nuee.langue = "en")
re <- baci(pb, bv("densite"), bv("annee"), bv("secteur"), "S1", c("S2", "S3"), evenement = 2005, fin = 2009, test = "mann_whitney")
rt <- baci(pb, bv("densite"), bv("annee"), bv("secteur"), c("S1", "S4"), c("S2", "S3"), evenement = 2007, test = "student")
options(nuee.langue = NULL)
verifier(sprintf("en anglais, Mann–Whitney avec une fin : « %s »", re$phrase),
         grepl(paste0("^Mann–Whitney test \\(BACI\\), densite gap impact \\(S1\\) − control \\(S2, S3\\), after against before: U = [0-9.]+, p = 0\\.[0-9]+; ",
                      "median gap -?[0-9.]+ before and -?[0-9.]+ after, rank-biserial correlation = -?[0-9.]+ \\(n = 3 before and 5 after; after: 2005 ≤ annee ≤ 2009\\)\\.$"),
               re$phrase))
verifier(sprintf("en anglais, Student, deux unités d'impact (2003 compte alors : S4 en a une) : « %s »", rt$phrase),
         grepl("^Student's t-test \\(BACI\\), densite gap impact \\(S1, S4\\) − control \\(S2, S3\\), after against before: t\\(9\\) = .*\\[95% CI .*\\(n = 6 before and 5 after; after: annee ≥ 2007\\)\\.$",
               rt$phrase) && isTRUE(all.equal(rt$difference[2:3], c(rt$tests$bas[2], rt$tests$haut[2]))))
verifier("aucune phrase BACI avec NA", !any(grepl("NA", c(rb$phrase, re$phrase, rt$phrase, rf$phrase), fixed = TRUE)))

cat("Le modèle mixte BACI\n")
# quatre secteurs suivis de 2001 à 2014, un effet par secteur et un bruit autocorrélé dans chacun :
# S1 (l'impact) gagne 1,5 à partir de 2008 et n'a pas de densité en 2004 ; S3 n'a pas de ligne en
# 2014 ; S4 n'est ni impact ni témoin ; dans le désordre
set.seed(13)
bm <- expand.grid(annee = 2001:2014, secteur = c("S1", "S2", "S3", "S4"), stringsAsFactors = FALSE)
bm$densite <- round(5 + c(S1 = 0.4, S2 = -0.6, S3 = 0.3, S4 = 0)[bm$secteur] + 1.5 * (bm$secteur == "S1" & bm$annee >= 2008) +
                      unlist(lapply(1:4, function(k) as.vector(stats::filter(rnorm(14, 0, 0.5), 0.6, "recursive")))), 3)
bm$densite[bm$secteur == "S1" & bm$annee == 2004] <- NA
bm <- bm[!(bm$secteur == "S3" & bm$annee == 2014), ]
bm <- data.frame(nom = paste0("m", seq_len(nrow(bm))), bm[sample(nrow(bm)), c("secteur", "annee", "densite")])
pm <- projet_depuis_tableau(bm, "baci mixte")
mv <- function(nom) numeros(pm, nom)
proche <- function(a, b, tol) isTRUE(all.equal(unname(a), unname(b), tolerance = tol, check.attributes = FALSE))
erreur_m <- function(...) tryCatch({ modele_baci(...); "" }, error = conditionMessage)
# (les témoins dans le désordre : les noms et les valeurs suivent l'ordre des niveaux)
b0 <- modele_baci(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S3", "S2"), 2008)
pv0 <- b0$projet
o0 <- numeros_observations(pv0)
periode0 <- colonne(pv0, numeros(pv0, "période BACI (2008)"), o0)
impact0 <- colonne(pv0, numeros(pv0, "impact BACI (S1 contre S2, S3)"), o0)
verifier("les deux variables 0/1 : la période (1 à partir de 2008, 0 avant), l'impact (1 pour S1, 0 pour les témoins S2 et S3, manquante pour S4), nommées d'après les réglages",
         identical(b0$nouvelles, c("période BACI (2008)", "impact BACI (S1 contre S2, S3)")) && identical(unname(b0$variables), b0$nouvelles) &&
           identical(unname(periode0), as.numeric(bm$annee >= 2008)) && identical(unname(impact0), ifelse(bm$secteur == "S1", 1, ifelse(bm$secteur %in% c("S2", "S3"), 0, NA))) &&
           length(pv0$variables) == length(pm$variables) + 2)
# les lignes du modèle, faites ici : S1, S2 et S3 avec une densité (nlme::lme les veut toutes connues), jusqu'à la fin
lignes_baci <- function(fin = Inf) {
  r <- bm[bm$secteur %in% c("S1", "S2", "S3") & !is.na(bm$densite) & bm$annee <= fin, ]
  transform(r, periode = as.numeric(annee >= 2008), impact = as.numeric(secteur == "S1"))
}
ctrl_b <- nlme::lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200, returnObject = TRUE)
d0 <- lignes_baci()
l0 <- nlme::lme(densite ~ periode * impact, random = ~ 1 | secteur, data = d0, method = "REML", control = ctrl_b)
m0 <- ajuster(pv0, b0$definition)
verifier("sans CAR1 : = nlme::lme(densite ~ periode * impact, random = ~ 1 | secteur, method = \"REML\") sur les lignes de l'impact et des témoins ; la densité manquante laissée de côté",
         proche(m0$resultat$coefficients$estimation, nlme::fixef(l0), 1e-6) && proche(m0$resultat$coefficients$p, summary(l0)$tTable[, "p-value"], 1e-5) &&
           m0$N == nrow(d0) && length(m0$exclues) == 1 && is.null(b0$definition$correlation) && b0$definition$type == "LMM" &&
           identical(b0$definition$name, "BACI densite") && identical(m0$niveaux, "secteur"))
b1 <- modele_baci(pv0, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2008, car1 = TRUE)
m1 <- ajuster(b1$projet, b1$definition)
l1 <- nlme::lme(densite ~ periode * impact, random = ~ 1 | secteur, correlation = nlme::corCAR1(form = ~ annee | secteur), data = d0, method = "REML", control = ctrl_b)
verifier(sprintf("avec CAR1 : = nlme::lme(..., correlation = corCAR1(form = ~ annee | secteur)) (φ = %.3f) ; les variables, qui existent avec ces valeurs, resservent", m1$resultat$phi),
         proche(m1$resultat$coefficients$estimation, nlme::fixef(l1), 1e-4) && proche(m1$resultat$phi, coef(l1$modelStruct$corStruct, unconstrained = FALSE), 1e-3) &&
           m1$resultat$phi > 0.1 && !length(b1$nouvelles) && identical(b1$projet$variables, pv0$variables) && m1$car1 && identical(m1$temps, "annee"))
b2 <- modele_baci(pv0, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2008, fin = 2012, car1 = TRUE)
m2 <- ajuster(b2$projet, b2$definition)
d2 <- lignes_baci(2012)
l2 <- nlme::lme(densite ~ periode * impact, random = ~ 1 | secteur, correlation = nlme::corCAR1(form = ~ annee | secteur), data = d2, method = "REML", control = ctrl_b)
verifier("avec une fin (2012) : les seules lignes jusqu'à la fin, = nlme::lme() sur elles",
         m2$N == nrow(d2) && proche(m2$resultat$coefficients$estimation, nlme::fixef(l2), 1e-4) &&
           all(colonne(pm, mv("annee"), b2$definition$observations) <= 2012))
e1 <- effet_baci(m1, b1$definition)
e0 <- effet_baci(m0, b0$definition)
c1 <- summary(l1)$tTable["periode:impact", ]
nb2 <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
p_texte <- function(p) if (p < 0.001) "p < 0,001" else paste("p =", formatC(p, digits = 3, format = "g", decimal.mark = ","))
verifier(sprintf("l'effet BACI = le coefficient periode:impact de nlme, son intervalle à 95 %% (la loi t de ses degrés de liberté), son t et sa valeur p, dans la phrase : « %s »", e1$phrase),
         proche(e1$estimation, c1[["Value"]], 1e-4) && proche(c(e1$bas, e1$haut), c1[["Value"]] + c(-1, 1) * qt(0.975, c1[["DF"]]) * c1[["Std.Error"]], 1e-4) &&
           proche(e1$p, c1[["p-value"]], 1e-3) && e1$ddl == c1[["DF"]] && e1$car1 &&
           grepl(sprintf("effet BACI (période × impact) = %s [IC 95 %% %s ; %s], t(%d) = %s, %s (n = %d observations de 3 unités ; après : annee ≥ 2008).",
                         nb2(e1$estimation), nb2(e1$bas), nb2(e1$haut), as.integer(c1[["DF"]]), nb2(e1$t), p_texte(e1$p), m1$N), e1$phrase, fixed = TRUE) &&
           startsWith(e1$phrase, sprintf("Modèle linéaire mixte (BACI), avec une ordonnée à l'origine aléatoire par secteur et une autocorrélation CAR1 le long de annee (φ = %s), qui tiennent compte des unités et de l'autocorrélation dans le temps ; densite, impact (S1) contre témoins (S2, S3) : ",
                                         nb2(m1$resultat$phi))))
verifier(sprintf("sans CAR1, la phrase dit qu'il tient compte des unités : « %s »", e0$phrase),
         startsWith(e0$phrase, "Modèle linéaire mixte (BACI), avec une ordonnée à l'origine aléatoire par secteur, qui tient compte des unités ; densite, impact (S1) contre témoins (S2, S3) : effet BACI") &&
           !e0$car1 && proche(e0$estimation, nlme::fixef(l0)[["periode:impact"]], 1e-6))
options(nuee.langue = "en")
e1en <- effet_baci(m1, b1$definition)
bm_en <- modele_baci(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2008)
t2en <- texte_methodes(b2$projet, b2$definition, m2, "en", "essai")
options(nuee.langue = NULL)
verifier(sprintf("en anglais : « %s » ; les variables nommées en anglais", e1en$phrase),
         grepl(paste0("^Linear mixed model \\(BACI\\), with a random intercept per secteur and a CAR1 autocorrelation along annee \\(φ = [0-9.]+\\), which account for the units and for the autocorrelation in time; ",
                      "densite, impact \\(S1\\) against control \\(S2, S3\\): BACI effect \\(period × impact\\) = [0-9.-]+ \\[95% CI [0-9.-]+, [0-9.-]+\\], t\\([0-9]+\\) = [0-9.-]+, p [=<] [0-9.]+ ",
                      "\\(n = [0-9]+ observations of 3 units; after: annee ≥ 2008\\)\\.$"), e1en$phrase) &&
           identical(bm_en$nouvelles, c("BACI period (2008)", "BACI impact (S1 vs S2, S3)")))
t1 <- texte_methodes(b1$projet, b1$definition, m1, "fr", "essai")
t2 <- texte_methodes(b2$projet, b2$definition, m2, "fr", "essai")
verifier("le texte des Méthodes dit que c'est le modèle BACI, ce que valent ses deux variables, et cite Stewart-Oaten et al. (1986) ; avec une fin, jusqu'où",
         grepl("C'est le modèle BACI (avant/après avec témoin ; Stewart-Oaten et al., 1986) : « période BACI (2008) » vaut 1 à partir de annee = 2008 et 0 avant, « impact BACI (S1 contre S2, S3) » vaut 1 pour l'unité d'impact S1 et 0 pour les témoins S2 et S3, et le coefficient de leur terme croisé, l'effet BACI,",
               t1$paragraphes[2], fixed = TRUE) && grepl("seules les observations de ces unités y entrent.", t1$paragraphes[2], fixed = TRUE) &&
           grepl("seules les observations de ces unités, jusqu'à annee = 2012, y entrent.", t2$paragraphes[2], fixed = TRUE) &&
           any(grepl("^Stewart-Oaten, A\\., Murdoch", t1$references)) && grepl("only the observations of these units, up to annee = 2012, enter it.", t2en$paragraphes[2], fixed = TRUE) &&
           !any(grepl("NA", c(t1$paragraphes, t2$paragraphes, t2en$paragraphes), fixed = TRUE)) &&
           !grepl("C'est le modèle BACI", texte_methodes(pv0, utils::modifyList(b0$definition, list(baci_origine = NULL)), m0, "fr", "essai")$paragraphes[2], fixed = TRUE))
# les réglages gardés avec la définition : le même modèle se reconnaît, même relu d'un fichier (une
# seule unité d'impact, pas de fin : ce que le JSON en fait)
f_baci <- tempfile(fileext = ".nuee")
pq_baci <- b1$projet
pq_baci$models <- list(b1$definition)
enregistrer_projet(pq_baci, f_baci)
relu <- lire_projet(f_baci)$models[[1]]
autre <- b1$definition$baci_origine
autre$temoins <- c("S3", "S2")
verifier("les mêmes réglages se reconnaissent (les témoins dans un autre ordre, la définition relue d'un fichier) ; un autre événement, une fin ou CAR1, non",
         memes_reglages_baci(relu$baci_origine, b1$definition$baci_origine) && memes_reglages_baci(autre, b1$definition$baci_origine) &&
           !memes_reglages_baci(b0$definition$baci_origine, b1$definition$baci_origine) && memes_reglages_baci(b0$definition$baci_origine, b1$definition$baci_origine, car1 = FALSE) &&
           !memes_reglages_baci(b2$definition$baci_origine, b1$definition$baci_origine) &&
           !memes_reglages_baci(modele_baci(pv0, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2009, car1 = TRUE)$definition$baci_origine, b1$definition$baci_origine) &&
           proche(ajuster(lire_projet(f_baci), relu)$resultat$coefficients$estimation, m1$resultat$coefficients$estimation, 1e-10) &&
           identical(texte_methodes(lire_projet(f_baci), relu, m1, "fr", "essai")$paragraphes, t1$paragraphes))
# l'unité et le temps en identifiants des observations : le même modèle
pmi <- pv0
pmi$observation_id_names <- c("Observation", "secteur_id", "an")
for (i in seq_along(pmi$observations)) pmi$observations[[i]]$ids <- c(bm$nom[i], bm$secteur[i], bm$annee[i])
bi <- modele_baci(pmi, mv("densite"), "id:3", "id:2", "S1", c("S2", "S3"), 2008, car1 = TRUE)
mi <- ajuster(bi$projet, bi$definition)
verifier("l'unité et le temps en identifiants : un effet aléatoire et CAR1 par identifiants, le même modèle ; les variables d'avant resservent",
         identical(bi$definition$aleatoires, list(list(by = "id", id = "secteur_id"))) && identical(bi$definition$correlation$temps, list(by = "id", id = "an")) &&
           proche(mi$resultat$coefficients$estimation, m1$resultat$coefficients$estimation, 1e-6) && !length(bi$nouvelles))
pc0 <- variable_calculee(pm, "période BACI (2008)", "ifelse(annee >= 2009, 1, 0)")
pc1 <- variable_calculee(pm, "période BACI (2008)", "ifelse(annee >= 2008, 1, 0)")
pc2 <- variable_calculee(pm, "impact BACI (S1 contre S2, S3)", "ifelse(secteur == \"S1\", \"oui\", \"non\")")
verifier("un nom pris par une variable aux autres valeurs, ou qualitative : le nom libre suivant, « … (2) » ; aux mêmes valeurs (faite à la main) : elle ressert",
         identical(modele_baci(pc0, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2008)$nouvelles,
                   c("période BACI (2008) (2)", "impact BACI (S1 contre S2, S3)")) &&
           identical(modele_baci(pc2, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2008)$nouvelles,
                     c("période BACI (2008)", "impact BACI (S1 contre S2, S3) (2)")) &&
           identical(modele_baci(pc1, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2008)$nouvelles, "impact BACI (S1 contre S2, S3)"))
bs_m <- bm
bs_m$site <- c(S1 = "A", S2 = "B", S3 = "B", S4 = "C")[bs_m$secteur]
pms <- projet_depuis_tableau(bs_m, "baci sites")
bms <- modele_baci(pms, numeros(pms, "densite"), numeros(pms, "annee"), numeros(pms, "site"), "A", "B", 2008)
verifier("sans événement, ou CAR1 avec plusieurs lignes d'une unité au même temps (l'unité = le site) : refusé ; sans CAR1, le site va",
         grepl("Entrez l'événement", erreur_m(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), NULL)) &&
           grepl("^Plusieurs lignes d'une même unité au même temps \\(B\\) : l'autocorrélation CAR1 demande un seul temps par unité",
                 erreur_m(pms, numeros(pms, "densite"), numeros(pms, "annee"), numeros(pms, "site"), "A", "B", 2008, car1 = TRUE)) &&
           # (deux unités : l'effet principal de l'impact n'a pas de degré de liberté entre unités, nlme le dit)
           suppressWarnings(ajuster(bms$projet, bms$definition))$N == nrow(d0) &&
           grepl("au moins deux valeurs de annee dans chaque période", erreur_m(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2014)))
rbm <- baci(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), evenement = 2008, fin = 2010)
verifier("baci() dit le plus petit nombre de temps d'une unité, jusqu'à la fin (CAR1 par défaut à partir de trois)", rbm$temps_unites == 9 &&
           baci(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), evenement = 2008)$temps_unites == 13)

# (le temps en dates, le jour d'abord, une variable qualitative ; l'événement tapé en date)
bmd <- bm
bmd$jour <- format(as.Date(sprintf("%d-06-15", bmd$annee)), "%d/%m/%Y")
pmd <- projet_depuis_tableau(bmd[c("nom", "secteur", "jour", "densite")], "baci mixte en dates")
bd <- modele_baci(pmd, numeros(pmd, "densite"), numeros(pmd, "jour"), numeros(pmd, "secteur"), "S1", c("S2", "S3"), "15/06/2008", car1 = TRUE)
md <- ajuster(bd$projet, bd$definition)
dd <- transform(lignes_baci(), decimale = annee + (as.POSIXlt(as.Date(sprintf("%d-06-15", annee)))$yday) / ifelse(annee %% 4 == 0, 366, 365))
ld <- nlme::lme(densite ~ periode * impact, random = ~ 1 | secteur, correlation = nlme::corCAR1(form = ~ decimale | secteur), data = dd, method = "REML", control = ctrl_b)
ed <- effet_baci(md, bd$definition)
verifier(sprintf("le modèle mixte BACI sur des dates (jj/mm/aaaa) et un événement tapé en date : la période nommée par sa date, = nlme::lme() le long des années décimales ; « %s »", ed$phrase),
         identical(bd$nouvelles[1], "période BACI (2008-06-15)") && proche(md$resultat$coefficients$estimation, nlme::fixef(ld), 1e-4) &&
           proche(md$resultat$phi, coef(ld$modelStruct$corStruct, unconstrained = FALSE), 1e-3) && grepl("2008-06-15", ed$phrase, fixed = TRUE) &&
           isTRUE(bd$definition$baci_origine$dates) && grepl("2008-06-15", bd$definition$title, fixed = TRUE))

cat("Le modèle mixte BACI : ses cas limites\n")
# (huit secteurs : deux réglages qui ne diffèrent qu'au-delà des trois unités nommées)
set.seed(17)
b8 <- expand.grid(annee = 2001:2012, secteur = sprintf("S%d", 1:8), stringsAsFactors = FALSE)
b8$densite <- round(5 + rnorm(nrow(b8)), 3)
b8 <- data.frame(nom = paste0("h", seq_len(nrow(b8))), b8)
p8 <- projet_depuis_tableau(b8, "huit secteurs")
v8 <- function(n) numeros(p8, n)
a8 <- modele_baci(p8, v8("densite"), v8("annee"), v8("secteur"), sprintf("S%d", 1:5), c("S7", "S8"), 2007)
b8b <- modele_baci(a8$projet, v8("densite"), v8("annee"), v8("secteur"), sprintf("S%d", c(1:4, 6)), c("S7", "S8"), 2007)
en8 <- local({ ancien <- options(nuee.langue = "en"); on.exit(options(ancien)); modele_baci(b8b$projet, v8("densite"), v8("annee"), v8("secteur"), sprintf("S%d", 1:5), c("S7", "S8"), 2007) })
verifier("d'autres unités au-delà des trois nommées : le nom libre suivant, « (2) », au lieu d'un refus ; les mêmes réglages dans l'autre langue reprennent les variables de même valeurs",
         identical(b8b$nouvelles, "impact BACI (S1, S2, S3… (5) contre S7, S8) (2)") && identical(unname(b8b$variables[1]), "période BACI (2007)") &&
           !length(en8$nouvelles) && identical(unname(en8$variables), unname(a8$variables)))
# (S1 a deux lignes en 2012, après la fin 2010 : le modèle ne les prend pas)
b2l <- rbind(b8, data.frame(nom = "h_double", annee = 2012, secteur = "S1", densite = 6))
p2l <- projet_depuis_tableau(b2l, "deux lignes après la fin")
r2l <- baci(p2l, numeros(p2l, "densite"), numeros(p2l, "annee"), numeros(p2l, "secteur"), "S1", c("S2", "S3"), evenement = 2007, fin = 2010)
m2l <- modele_baci(p2l, numeros(p2l, "densite"), numeros(p2l, "annee"), numeros(p2l, "secteur"), "S1", c("S2", "S3"), 2007, fin = 2010, car1 = TRUE)
verifier("deux lignes d'une unité au même temps, après la fin : dites pour la série des écarts, mais CAR1 accepté (le modèle s'arrête à la fin) ; avant la fin, refusé",
         identical(r2l$repetees$unite, "S1") && is.null(r2l$repetees_modele) && !is.null(m2l$definition$correlation) &&
           grepl("plusieurs lignes|Plusieurs lignes", erreur_m(p2l, numeros(p2l, "densite"), numeros(p2l, "annee"), numeros(p2l, "secteur"), "S1", c("S2", "S3"), 2007, car1 = TRUE)))
erreur_k <- function(...) tryCatch({ baci(...); "" }, error = conditionMessage)
verifier("l'événement et la fin du genre du temps : une date pour un temps d'années, refusée ; avec des dates, une fin en année refusée, un événement en année pris pour son 1er janvier",
         grepl("est fait de nombres : entrez l'événement en nombre", erreur_k(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), evenement = "2008-06-01")) &&
           grepl("est fait de nombres : entrez la fin en nombre", erreur_k(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), evenement = 2008, fin = "31/12/2012")) &&
           grepl("est fait de dates : entrez la fin en date", erreur_k(pmd, numeros(pmd, "densite"), numeros(pmd, "jour"), numeros(pmd, "secteur"), "S1", c("S2", "S3"), evenement = "15/06/2008", fin = 2012)) &&
           baci(pmd, numeros(pmd, "densite"), numeros(pmd, "jour"), numeros(pmd, "secteur"), "S1", c("S2", "S3"), evenement = 2008)$evenement == 2008 &&
           baci(pm, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), evenement = "2008")$evenement == 2008)
vd <- variables_baci(pmd, numeros(pmd, "jour"), numeros(pmd, "secteur"), "S1", c("S2", "S3"), "15/06/2008")
vy <- variables_baci(pm, mv("annee"), mv("secteur"), "S1", c("S2", "S3"), "2008,5")
verifier("le journal de la console refait les variables d'un événement tapé en date ou avec une virgule : les mêmes valeurs",
         identical(unname(vd$noms[1]), "période BACI (2008-06-15)") &&
           identical(unname(colonne(vd$projet, vd$uid[["periode"]], numeros_observations(pmd))), as.numeric(bmd$annee >= 2008)) &&
           identical(unname(colonne(vy$projet, vy$uid[["periode"]], numeros_observations(pm))), as.numeric(bm$annee >= 2008.5)))
tmd <- paste(texte_methodes(bd$projet, bd$definition, md, "fr", "essai")$paragraphes, collapse = " ")
verifier("le texte des méthodes du modèle BACI sur des dates : l'événement en date, pas en année décimale",
         grepl("à partir de jour = 2008-06-15", tmd, fixed = TRUE) && !grepl("2008,4", tmd, fixed = TRUE))
# (la sélection par l'AIC d'un modèle BACI : sans le terme croisé, ce n'est plus lui)
mb0 <- ajuster(pv0, b0$definition)
sb0 <- selection_aic(mb0, b0$definition)
sans_croise <- definition_selection(b0$definition, mb0, setdiff(as.character(sb0$groupes), grep("\\*", as.character(sb0$groupes), value = TRUE)))
avec_croise <- definition_selection(b0$definition, mb0, as.character(sb0$groupes))
verifier("Ajuster ce modèle d'un modèle BACI : sans le terme croisé, plus de réglages BACI ni d'effet (la vue BACI garde son test) ; avec, un modèle qui en dérive, que les mêmes réglages ne reprennent pas",
         is.null(sans_croise$baci_origine) && is.null(effet_baci(ajuster(pv0, sans_croise), sans_croise)) &&
           is.null(effet_baci(ajuster(pv0, sans_croise), c(sans_croise, list(baci_origine = b0$definition$baci_origine)))) &&
           isTRUE(avec_croise$baci_origine$derive) && !memes_reglages_baci(avec_croise$baci_origine, b0$definition$baci_origine) &&
           memes_reglages_baci(b0$definition$baci_origine, b0$definition$baci_origine))

cat("Le même calcul sous toutes les versions de R (ex aequo, différences nulles)\n")
# des comptages : S1 et S2 suivis 10 ans, des différences nulles et ex aequo ; une variable aux
# différences constantes à 1e-9 près
set.seed(21)
eq <- data.frame(nom = paste0("e", 1:20), secteur = rep(c("S1", "S2"), each = 10), annee = rep(2001:2010, 2),
                 compte = c(c(3, 5, 4, 6, 2, 5, 4, 3, 6, 5), c(4, 5, 6, 6, 3, 7, 4, 5, 8, 6)),
                 constante = c(rep(1, 10), rep(2, 10)) + c(rnorm(10, 0, 1e-12), rnorm(10, 0, 1e-12)))
pe <- projet_depuis_tableau(eq, "ex aequo")
de <- eq$compte[11:20] - eq$compte[1:10]  # (S2 − S1, par année)
re <- tests_variables(pe, numeros(pe, "secteur"), c("S1", "S2"), test = "wilcoxon", appariement = numeros(pe, "annee"))
ligne_compte <- re$tableau[re$tableau$nom == "compte", ]
w_ancien <- suppressWarnings(stats::wilcox.test(de[de != 0], exact = FALSE, correct = TRUE))  # (le calcul de R < 4.6)
verifier("variable par variable, Wilcoxon avec des différences nulles et ex aequo : V et p de R < 4.6, comme le test apparié de deux variables",
         isTRUE(all.equal(ligne_compte$statistique, unname(w_ancien$statistic))) && isTRUE(all.equal(ligne_compte$p, w_ancien$p.value)) &&
           isTRUE(all.equal(ligne_compte$p, wilcoxon_apparie(de)$p)))
rt <- tests_variables(pe, numeros(pe, "secteur"), c("S1", "S2"), test = "t_apparie", appariement = numeros(pe, "annee"))
ligne_cst <- rt$tableau[rt$tableau$nom == "constante", ]
verifier("variable par variable, des différences constantes (à 1e-8 près) : ni t, ni p, ni d_z",
         is.na(ligne_cst$statistique) && is.na(ligne_cst$p) && is.na(ligne_cst$d))
mw_ancien <- suppressWarnings(stats::wilcox.test(eq$compte[11:20], eq$compte[1:10], exact = FALSE, correct = TRUE))
rm2 <- tests_variables(pe, numeros(pe, "secteur"), c("S1", "S2"), test = "mann_whitney")
deux_mw <- test_deux_variables(pe, numeros(pe, "compte"), numeros(pe, "secteur"))
verifier("Mann–Whitney avec des ex aequo : la loi normale avec correction de continuité (R < 4.6), entre classes et en deux variables",
         isTRUE(all.equal(rm2$tableau$p[rm2$tableau$nom == "compte"], mw_ancien$p.value)) &&
           isTRUE(all.equal(deux_mw$tests$p[deux_mw$tests$test == "mann_whitney"], mw_ancien$p.value)))
# BACI : des comptages entiers, des écarts ex aequo
bq <- data.frame(nom = paste0("q", 1:24), secteur = rep(c("I", "T"), each = 12), annee = rep(2001:2012, 2),
                 compte = c(c(5, 6, 5, 6, 5, 6, 9, 10, 9, 10, 9, 10), rep(5, 12)))
pq <- projet_depuis_tableau(bq, "baci comptages")
rq <- baci(pq, numeros(pq, "compte"), numeros(pq, "annee"), numeros(pq, "secteur"), "I", "T", evenement = 2007, test = "mann_whitney")
dq <- bq$compte[1:12] - 5
mq <- suppressWarnings(stats::wilcox.test(dq[7:12], dq[1:6], exact = FALSE, correct = TRUE))
verifier("BACI, Mann–Whitney sur des écarts ex aequo : la loi normale avec correction de continuité (R < 4.6)",
         isTRUE(all.equal(rq$tests$p[rq$tests$test == "mann_whitney"], mq$p.value)))
bq2 <- bq
bq2$annee <- sub(".", ",", sprintf("%.1f", bq2$annee), fixed = TRUE)  # (« 2001,0 » : un temps en texte, avec la virgule)
pq2 <- projet_depuis_tableau(bq2, "baci virgule")
rq2 <- baci(pq2, numeros(pq2, "compte"), numeros(pq2, "annee"), numeros(pq2, "secteur"), "I", "T", evenement = 2007, test = "welch")
verifier("BACI, un temps écrit avec une virgule décimale : lu comme dans les séries", isTRUE(all.equal(rq2$tests$p, baci(pq, numeros(pq, "compte"), numeros(pq, "annee"),
         numeros(pq, "secteur"), "I", "T", evenement = 2007, test = "welch")$tests$p)))

cat("Le temps en dates (variables.R : temps_en_nombres)\n")
# l'année décimale refaite avec les dates de R (strptime, son jour de l'année) : l'année + (le jour
# de l'année − 1 + la fraction du jour) / le nombre de jours de l'année
annee_r <- function(x, format = "%Y-%m-%d") {
  l <- as.POSIXlt(strptime(x, format, tz = "UTC"))
  a <- 1900 + l$year
  a + (l$yday + (l$hour * 3600 + l$min * 60 + l$sec) / 86400) / as.numeric(as.Date(sprintf("%d-01-01", a + 1)) - as.Date(sprintf("%d-01-01", a)))
}
egal <- function(a, b) isTRUE(all.equal(as.vector(a), as.vector(b), tolerance = 1e-12))
jours <- c("2019-05-14", "2020-02-29", "2020-12-31", "2019-12-31", "2019-01-01", "2000-12-31", "1900-03-01")
tj <- temps_en_nombres(jours)
verifier("des dates ISO : l'année + (le jour de l'année − 1) / ses jours, 366 les années bissextiles (2000 l'est, 1900 non) ; le 1er janvier, l'année même",
         isTRUE(attr(tj, "dates")) && egal(tj, annee_r(jours)) && tj[5] == 2019 && egal(tj[2], 2020 + 59 / 366) && egal(tj[3], 2020 + 365 / 366) &&
           egal(tj[4], 2019 + 364 / 365) && egal(tj[6], 2000 + 365 / 366) && egal(tj[7], 1900 + 59 / 365))
verifier("avec l'heure : la fraction du jour (10:30, 23:59:59 ; un T après la date ISO)",
         egal(temps_en_nombres(c("2019-05-14 10:30", "2019-12-31 23:59")), annee_r(c("2019-05-14 10:30", "2019-12-31 23:59"), "%Y-%m-%d %H:%M")) &&
           egal(temps_en_nombres("2019-12-31 23:59:59"), annee_r("2019-12-31 23:59:59", "%Y-%m-%d %H:%M:%S")) &&
           egal(temps_en_nombres("2019-05-14T10:30"), temps_en_nombres("2019-05-14 10:30")) && egal(temps_en_nombres("2019-05-14 10:30"), 2019 + (133 + 10.5 / 24) / 365))
verifier("le jour d'abord, toujours (05/04/2019 : le 5 avril), avec /, - ou . ; avec l'heure aussi",
         egal(temps_en_nombres(c("14/05/2019", "14-05-2019", "14.05.2019", "4/5/2019", "05/04/2019")), annee_r(c("2019-05-14", "2019-05-14", "2019-05-14", "2019-05-04", "2019-04-05"))) &&
           egal(temps_en_nombres("14/05/2019 10:30"), temps_en_nombres("2019-05-14 10:30")))
verifier("des dates de R (Date, POSIXct à l'heure de sa zone), un facteur de dates : les mêmes années décimales",
         egal(temps_en_nombres(as.Date(jours)), tj) && egal(temps_en_nombres(factor(jours)), tj) &&
           egal(temps_en_nombres(as.POSIXct("2019-05-14 10:30", tz = "Europe/Paris")), temps_en_nombres("2019-05-14 10:30")))
tnb <- temps_en_nombres(c("2019", "2020,5", "", NA))
verifier("des nombres restent des nombres (la virgule décimale lue), sans être des dates",
         egal(tnb, c(2019, 2020.5, NA, NA)) && is.null(attr(tnb, "dates")) && is.null(attr(tnb, "refus")) && egal(temps_en_nombres(c(2019, NA, Inf)), c(2019, NA, NA)))
refus_t <- function(...) { r <- temps_en_nombres(c(...)); c(attr(r, "refus") %||% "", attr(r, "raison") %||% "") }
verifier("un mélange de nombres et de dates est refusé, la première valeur de l'autre genre nommée : une année parmi des dates, une date parmi des années",
         identical(refus_t("2019-05-14", "14/06/2020", "2021"), c("2021", "nombre")) && identical(refus_t("2019", "2020", "14/05/2021"), c("14/05/2021", "date")))
verifier("une date qui n'existe pas (31/02/2019, 29/02/2019, 24:00), une année en deux chiffres ou un texte : refusés, en disant pourquoi",
         identical(refus_t("31/02/2019", "01/03/2019"), c("31/02/2019", "impossible")) && identical(refus_t("2019-02-29", "2020-02-29"), c("2019-02-29", "impossible")) &&
           identical(refus_t("2019-05-14 24:00", "2019-05-15"), c("2019-05-14 24:00", "impossible")) && identical(refus_t("14/05/19", "14/05/2020"), c("14/05/19", "illisible")) &&
           identical(refus_t("S1", "S2"), c("S1", "illisible")) &&
           identical(refus_temps(temps_en_nombres(c("31/02/2019", "01/03/2019")), "date"),
                     "Le temps (date) doit être fait de nombres (l'année) ou de dates (2019-05-14, 14/05/2019) : « 31/02/2019 » n'est pas une date qui existe") &&
           grepl(", pas des deux : « 2021 » est un nombre parmi des dates$", refus_temps(temps_en_nombres(c("2019-05-14", "14/06/2020", "2021")), "date")))
verifier("une année décimale en date, pour les textes : l'inverse", identical(date_decimale(tj[1:2]), c("2019-05-14", "2020-02-29")) &&
           identical(date_decimale(temps_en_nombres("2019-05-14 10:30")), "2019-05-14 10:30"))
# trois secteurs comptés chaque printemps de 2001 à 2010, le même jour dans une année (qui change
# d'une année à l'autre) : la date en texte, le jour d'abord (une variable qualitative, chaque date
# revient pour les trois secteurs), et la même en années décimales, refaite avec R
set.seed(31)
jd <- expand.grid(secteur = c("S1", "S2", "S3"), annee = 2001:2010, stringsAsFactors = FALSE)
jour <- as.Date(sprintf("%d-04-20", 2001:2010)) + sample(0:30, 10, TRUE)
jd$date <- format(jour[jd$annee - 2000], "%d/%m/%Y")
jd$decimale <- annee_r(format(jour[jd$annee - 2000]))
jd$densite <- round(5 + 0.3 * (jd$annee - 2000) * (jd$secteur == "S1") + 1.5 * (jd$secteur == "S1" & jd$annee >= 2006) + rnorm(nrow(jd)), 2)
pd <- projet_depuis_tableau(data.frame(nom = paste0("d", seq_len(nrow(jd))), jd[c("secteur", "date", "decimale", "densite")]), "dates")
dv <- function(nom) numeros(pd, nom)
sd <- series_variable(pd, dv("densite"), dv("date"), dv("secteur"))
sn <- series_variable(pd, dv("densite"), dv("decimale"), dv("secteur"))
verifier("Séries : une variable qualitative de dates donne les mêmes points et moyennes que ses années décimales, et le dit",
         length(variable_du_projet(pd, dv("date"))$categories) == 10 && isTRUE(sd$dates) && identical(sn$dates, FALSE) &&
           isTRUE(all.equal(sd$points, sn$points)) && isTRUE(all.equal(sd$moyennes, sn$moyennes)))
pdi <- pd
pdi$observation_id_names <- c("Observation", "jour")
for (i in seq_along(pdi$observations)) pdi$observations[[i]]$ids <- c(pdi$observations[[i]]$ids[1], format(jour[jd$annee[i] - 2000]))
verifier("Séries : des dates ISO en identifiant des observations, de même", isTRUE(all.equal(series_variable(pdi, dv("densite"), "id:2", dv("secteur"))$points, sn$points)))
bd <- baci(pd, dv("densite"), dv("date"), dv("secteur"), "S1", c("S2", "S3"), evenement = "2006-01-01")
bn <- baci(pd, dv("densite"), dv("decimale"), dv("secteur"), "S1", c("S2", "S3"), evenement = 2006)
verifier(sprintf("BACI sur des dates : les mêmes écarts et tests qu'en années décimales, l'événement en date ou en année ; la phrase avec la date (« …%s »)", sub("^.*\\(n = ", "(n = ", bd$phrase)),
         isTRUE(bd$dates) && identical(bn$dates, FALSE) && isTRUE(all.equal(bd$serie, bn$serie)) && isTRUE(all.equal(bd$tests, bn$tests)) &&
           isTRUE(all.equal(baci(pd, dv("densite"), dv("date"), dv("secteur"), "S1", c("S2", "S3"), evenement = 2006)$tests, bd$tests)) &&
           grepl("après : date ≥ 2006-01-01\\)\\.$", bd$phrase))
bdf <- baci(pd, dv("densite"), dv("date"), dv("secteur"), "S1", c("S2", "S3"), evenement = "01/01/2006", fin = "31/12/2008", test = "mann_whitney")
bnf <- baci(pd, dv("densite"), dv("decimale"), dv("secteur"), "S1", c("S2", "S3"), evenement = 2006, fin = 2009, test = "mann_whitney")
verifier("BACI, la fin en date : les mêmes périodes et tests ; un événement qui n'est pas une date, refusé",
         bdf$hors == 2 && identical(bdf$serie$periode, bnf$serie$periode) && isTRUE(all.equal(bdf$tests, bnf$tests)) &&
           grepl("doit être un nombre ou une date", erreur_b(pd, dv("densite"), dv("date"), dv("secteur"), "S1", "S2", evenement = "31/02/2006")))

cat("Tendances des séries (series.R : tendances_series)\n")
# Mann–Kendall et la pente de Sen refaits à la main : S (les signes de toutes les paires dans
# l'ordre du temps) et sa variance corrigée pour les ex aequo, la p de la formule classique (loi
# normale, correction de continuité), les pentes de toutes les paires, leur médiane et ses rangs à
# 95 % (Gilbert, 1987), le tau-b de cor()
mk_main <- function(t, y) {
  n <- length(y)
  S <- 0
  pentes <- numeric(0)
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    S <- S + sign(y[j] - y[i])
    pentes <- c(pentes, (y[j] - y[i]) / (t[j] - t[i]))
  }
  ex <- table(y)
  v <- (n * (n - 1) * (2 * n + 5) - sum(ex * (ex - 1) * (2 * ex + 5))) / 18
  z <- if (S > 0) (S - 1) / sqrt(v) else if (S < 0) (S + 1) / sqrt(v) else 0
  N <- length(pentes)
  C <- qnorm(0.975) * sqrt(v)
  o <- sort(pentes)
  list(S = S, var_S = v, p = 2 * (1 - pnorm(abs(z))), pente = median(pentes), ic = c(o[round((N - C) / 2)], o[round((N + C) / 2 + 1)]),
       tau = cor(t, y, method = "kendall"))
}
# deux sites : A-s1, 12 ans aux valeurs ex aequo ; A-s2, 3 ans ; A-s3, 4 ans ; B-s1, des années
# irrégulières, en baisse ; B-s2, la même valeur chaque année ; B-s3, l'année 2002 deux fois
ts <- rbind(data.frame(site = "A", secteur = "s1", annee = 2001:2012, y = c(3, 5, 5, 4, 7, 7, 7, 9, 8, 10, 10, 12)),
            data.frame(site = "A", secteur = "s2", annee = c(2001, 2005, 2010), y = c(4, 6, 5)),
            data.frame(site = "A", secteur = "s3", annee = 2001:2004, y = c(1, 3, 2, 4)),
            data.frame(site = "B", secteur = "s1", annee = c(2001, 2002, 2004, 2007, 2008, 2011), y = c(9, 8, 8, 6, 7, 3)),
            data.frame(site = "B", secteur = "s2", annee = 2003:2007, y = 5),
            data.frame(site = "B", secteur = "s3", annee = c(2001, 2002, 2002, 2003, 2004), y = c(2, 3, 4, 5, 6)))
pt <- projet_depuis_tableau(data.frame(nom = paste0("t", seq_len(nrow(ts))), ts[sample(nrow(ts)), ]), "tendances")
st <- series_variable(pt, numeros(pt, "y"), numeros(pt, "annee"), numeros(pt, "secteur"), numeros(pt, "site"))
tr <- tendances_series(st$points)
ligne_tr <- function(s, se) tr[tr$panneau == s & tr$ligne == se, ]
verif_mk <- function(s, se) {
  r <- ligne_tr(s, se)
  d <- ts[ts$site == s & ts$secteur == se, ]
  h <- mk_main(d$annee, d$y)
  isTRUE(all.equal(c(r$S, r$var_S, r$p, r$pente, r$bas, r$haut, r$tau), c(h$S, h$var_S, h$p, h$pente, h$ic, h$tau))) &&
    r$n == nrow(d) && r$debut == min(d$annee) && r$fin == max(d$annee) && !nzchar(r$non_teste)
}
verifier(sprintf("A-s1, des ex aequo : S = %g et sa variance (%g) refaits à la main, p de la formule classique, τ = cor(method = \"kendall\"), Sen = la médiane des pentes et son intervalle (%.3f ; %.3f)",
                 ligne_tr("A", "s1")$S, ligne_tr("A", "s1")$var_S, ligne_tr("A", "s1")$bas, ligne_tr("A", "s1")$haut),
         verif_mk("A", "s1") && ligne_tr("A", "s1")$var_S < 12 * 11 * 29 / 18)
verifier("B-s1, des années irrégulières : les pentes sur les vrais écarts de temps, la même chose", verif_mk("B", "s1") && ligne_tr("B", "s1")$S < 0)
a3 <- ligne_tr("A", "s3")
h3 <- mk_main(2001:2004, c(1, 3, 2, 4))
verifier("4 temps : testé (S, p, pente, τ), mais trop peu pour un intervalle à 95 % de la pente (le rang (N − C)/2 arrondi est 0)",
         isTRUE(all.equal(c(a3$S, a3$var_S, a3$p, a3$pente, a3$tau), c(h3$S, h3$var_S, h3$p, h3$pente, h3$tau))) && is.na(a3$bas) && is.na(a3$haut))
verifier("3 temps, la même valeur chaque année, un même temps deux fois : pas de test, la ligne dit pourquoi",
         is.na(ligne_tr("A", "s2")$p) && ligne_tr("A", "s2")$n == 3 && ligne_tr("A", "s2")$non_teste == "3 temps : il en faut au moins 4" &&
           is.na(ligne_tr("B", "s2")$pente) && grepl("même valeur", ligne_tr("B", "s2")$non_teste) && grepl("deux fois", ligne_tr("B", "s3")$non_teste) &&
           identical(paste(tr$panneau, tr$ligne), c("A s1", "A s2", "A s3", "B s1", "B s2", "B s3")))
th <- tendances_series(st$points, "holm")
verifier("la valeur p corrigée sur les lignes testées : Benjamini–Hochberg par défaut, Holm au choix (= p.adjust())",
         isTRUE(all.equal(tr$p_ajuste, p.adjust(tr$p, "BH"))) && isTRUE(all.equal(th$p_ajuste, p.adjust(tr$p, "holm"))) && sum(!is.na(tr$p_ajuste)) == 3 &&
           grepl("Correction inconnue", tryCatch(tendances_series(st$points, "bonferroni"), error = conditionMessage)))
tdd <- tendances_series(sd$points)
verifier(sprintf("sur des dates : les mêmes tendances qu'en années décimales, la pente par an (S1 : %.2f par an)", tdd$pente[1]),
         isTRUE(all.equal(tdd, tendances_series(sn$points))) && tdd$pente[1] > 0 &&
           isTRUE(all.equal(tdd$pente[1], mk_main(jd$decimale[jd$secteur == "S1"], jd$densite[jd$secteur == "S1"])$pente)))
verifier("sans option, chaque ligne dit son test : Mann–Kendall", all(tr$test == "mann_kendall") && all(is.na(tr$n_n_etoile)))

cat("Tendances : la correction de Hamed et Rao (series.R : facteur_hamed_rao)\n")
# Hamed et Rao (1998) refait à la main : la pente de Sen de mk_main, la ligne moins pente × temps,
# ses rangs, leur autocorrélation à chaque décalage écrite en sommes, gardée au-delà de 1,96/√n,
# n/n* = 1 + 2 / (n(n − 1)(n − 2)) Σ (n − k)(n − k − 1)(n − k − 2) ρ(k) ; Var(S) × n/n*, p et
# l'intervalle de Gilbert d'après elle
hr_main <- function(t, y) {
  h <- mk_main(t, y)
  n <- length(y)
  R <- rank(round(y - h$pente * (t - t[1]), 9))
  m <- mean(R)
  rho <- vapply(1:(n - 1), function(k) sum((R[1:(n - k)] - m) * (R[(k + 1):n] - m)) / sum((R - m)^2), 0)
  rho[abs(rho) <= qnorm(0.975) / sqrt(n)] <- 0
  f <- 1 + 2 / (n * (n - 1) * (n - 2)) * sum(vapply(1:(n - 1), function(k) (n - k) * (n - k - 1) * (n - k - 2) * rho[k], 0))
  if (f <= 0) return(list(f = f))
  v <- h$var_S * f
  z <- if (h$S > 0) (h$S - 1) / sqrt(v) else if (h$S < 0) (h$S + 1) / sqrt(v) else 0
  o <- sort(unlist(lapply(1:(n - 1), function(i) (y[(i + 1):n] - y[i]) / (t[(i + 1):n] - t[i]))))
  N <- length(o)
  C <- qnorm(0.975) * sqrt(v)
  list(f = f, var_S = v, p = 2 * (1 - pnorm(abs(z))), S = h$S, pente = h$pente,
       ic = if (round((N - C) / 2) >= 1) c(o[round((N - C) / 2)], o[round((N + C) / 2 + 1)]) else c(NA, NA))
}
# des lignes d'un site : A, 40 ans autocorrélés (un AR(1) de 0,6 et une hausse, tiré une fois) ;
# B, 12 ans aux ex aequo (A-s1 plus haut) ; C, 30 ans dont 2010 et 2015 manquent ; D, 20 ans d'une
# autocorrélation fortement négative (un AR(1) de −0,8) ; E, une droite
yA <- c(8.47, 7.99, 8.88, 9.19, 10.51, 9.83, 9.38, 9.1, 9.61, 9.51, 10.26, 10.44, 10.51, 10.36, 9.72, 9.89, 9.25, 8.76, 8.64, 10.21,
        8.91, 8.85, 10.19, 10.33, 12.17, 11.13, 10.79, 9.31, 10.09, 11.45, 10.52, 11.74, 11.25, 9.13, 10.94, 11.89, 11.97, 12.6, 11.98, 11.03)
anC <- setdiff(2001:2032, c(2010, 2015))
yD <- c(6.6, 4.1, 7.1, 3.3, 6.8, 3.5, 4.8, 4.7, 4.8, 5.1, 6, 4.9, 4.9, 4.8, 5.8, 4.9, 4.4, 4.8, 5.5, 5.3)
th <- rbind(data.frame(secteur = "A", annee = 1981:2020, y = yA),
            data.frame(secteur = "B", annee = 2001:2012, y = c(3, 5, 5, 4, 7, 7, 7, 9, 8, 10, 10, 12)),
            data.frame(secteur = "C", annee = anC, y = yA[seq_along(anC)]),
            data.frame(secteur = "D", annee = 2001:2020, y = yD),
            data.frame(secteur = "E", annee = 2001:2010, y = 2 * (2001:2010) + 1))
ph <- projet_depuis_tableau(data.frame(nom = paste0("h", seq_len(nrow(th))), th[sample(nrow(th)), ]), "hamed_rao")
sh <- series_variable(ph, numeros(ph, "y"), numeros(ph, "annee"), numeros(ph, "secteur"))
trh <- tendances_series(sh$points, autocorrelation = TRUE)
ligne_h <- function(s) trh[trh$ligne == s, ]
verif_hr <- function(s) {
  r <- ligne_h(s)
  d <- th[th$secteur == s, ]
  h <- hr_main(d$annee, d$y)
  isTRUE(all.equal(c(r$n_n_etoile, r$var_S, r$p, r$S, r$pente, r$bas, r$haut), c(h$f, h$var_S, h$p, h$S, h$pente, h$ic))) && r$test == "hamed_rao" && !nzchar(r$non_teste)
}
verifier(sprintf("A, 40 ans autocorrélés : n/n* = %.3f, Var(S) corrigée, p (%.4f, %.6f sans correction) et l'intervalle de la pente d'après elle, refaits à la main",
                 ligne_h("A")$n_n_etoile, ligne_h("A")$p, mk_main(1981:2020, yA)$p),
         verif_hr("A") && ligne_h("A")$n_n_etoile > 1.5 && ligne_h("A")$p > 10 * mk_main(1981:2020, yA)$p)
verifier("B, 12 ans : aucune autocorrélation au-delà de ±1,96/√12, n/n* = 1, le test de Mann–Kendall lui-même",
         verif_hr("B") && ligne_h("B")$n_n_etoile == 1 && isTRUE(all.equal(ligne_h("B")$p, tr[tr$panneau == "A" & tr$ligne == "s1", "p"])))
verifier("C, deux années manquantes : la tendance retirée sur les vrais temps, les décalages d'un temps au suivant", verif_hr("C"))
verifier(sprintf("D, une forte autocorrélation négative : n/n* = %.3f n'est pas positif, pas de test (« %s »)", ligne_h("D")$n_n_etoile, ligne_h("D")$non_teste),
         isTRUE(all.equal(ligne_h("D")$n_n_etoile, hr_main(2001:2020, yD)$f)) && ligne_h("D")$n_n_etoile < 0 && is.na(ligne_h("D")$p) &&
           startsWith(ligne_h("D")$non_teste, "n/n* = -0,341 n'est pas positif") && !is.na(ligne_h("D")$pente))
verifier("E, une droite : ce qui reste est constant, aucune autocorrélation (n/n* = 1)",
         ligne_h("E")$n_n_etoile == 1 && isTRUE(all.equal(ligne_h("E")$p, mk_main(2001:2010, 2 * (2001:2010) + 1)$p)))
verifier("la correction sur les lignes, la même : p.adjust() des p corrigées", isTRUE(all.equal(trh$p_ajuste, p.adjust(trh$p, "BH"))))
# modifiedmk::mmkh (version 1.6, ci = 0.95), lancé une fois sur deux séries des données de R : son
# n/n*, sa variance corrigée et sa valeur p, ici à 1e-12 près
mmkh_nile <- c(n_n_etoile = 2.1428983270987203, var_S = 241565.35691662686, p = 0.0048026763101827385)
mmkh_huron <- c(n_n_etoile = 3.2865665584217432, var_S = 348825.21928902238, p = 0.0044245889154761785)
hr_serie <- function(t, y) unlist(mann_kendall(t, y, TRUE)[c("n_n_etoile", "var_S", "p")])
verifier(sprintf("le Nil (1871–1970) et le lac Huron (1875–1972) : n/n* (%.4f, %.4f), Var(S) et p de modifiedmk::mmkh",
                 mmkh_nile[["n_n_etoile"]], mmkh_huron[["n_n_etoile"]]),
         isTRUE(all.equal(hr_serie(1871:1970, c(Nile)), mmkh_nile, tolerance = 1e-12)) &&
           isTRUE(all.equal(hr_serie(1875:1972, c(LakeHuron)), mmkh_huron, tolerance = 1e-12)))

cat("Tendances : le test de Kendall saisonnier (series.R : kendall_saisonnier)\n")
# le test saisonnier refait à la main : une valeur par saison et par année (la médiane), dans chaque
# saison vue au moins 2 années S_g et sa variance avec les ex aequo, les pentes entre années ; S et
# Var(S) additionnés, p de la formule classique, la médiane des pentes et son intervalle de Gilbert,
# le τ d'EnvStats (la moyenne des 2 S_g / (n_g(n_g − 1)) pondérée par n_g)
saisonnier_main <- function(annee, saison, y) {
  m <- aggregate(list(y = y), list(saison = saison, annee = annee), median)
  S <- 0
  V <- 0
  pentes <- numeric(0)
  taus <- numeric(0)
  ns <- numeric(0)
  for (g in unique(m$saison)) {
    d <- m[m$saison == g, ]
    d <- d[order(d$annee), ]
    n <- nrow(d)
    if (n < 2) next
    Sg <- 0
    for (i in 1:(n - 1)) for (j in (i + 1):n) {
      Sg <- Sg + sign(d$y[j] - d$y[i])
      pentes <- c(pentes, (d$y[j] - d$y[i]) / (d$annee[j] - d$annee[i]))
    }
    ex <- table(d$y)
    S <- S + Sg
    V <- V + (n * (n - 1) * (2 * n + 5) - sum(ex * (ex - 1) * (2 * ex + 5))) / 18
    taus <- c(taus, 2 * Sg / (n * (n - 1)))
    ns <- c(ns, n)
  }
  z <- if (S > 0) (S - 1) / sqrt(V) else if (S < 0) (S + 1) / sqrt(V) else 0
  N <- length(pentes)
  C <- qnorm(0.975) * sqrt(V)
  o <- sort(pentes)
  list(S = S, var_S = V, p = 2 * (1 - pnorm(abs(z))), pente = median(pentes), tau = sum(ns * taus) / sum(ns), saisons = length(ns),
       ic = if (round((N - C) / 2) >= 1) c(o[round((N - C) / 2)], o[round((N + C) / 2 + 1)]) else c(NA, NA))
}
# trois secteurs relevés en avril, juin, août et octobre de 2008 à 2015, le même jour pour les trois
# (les dates, le jour d'abord, une variable qualitative) ; des comptages entiers (des ex aequo) ; en
# S1 juin 2010 manque, un relevé de décembre 2012 (une saison vue une année : elle ne compte pas) et
# deux relevés en août 2011 (leur médiane) ; S3, un relevé par saison (trop peu d'années)
set.seed(41)
js <- expand.grid(mois = c(4, 6, 8, 10), annee = 2008:2015)
js$jour <- sprintf("%02d/%02d/%d", sample(5:25, nrow(js), TRUE), js$mois, js$annee)
ds <- do.call(rbind, lapply(c("S1", "S2"), function(s) data.frame(secteur = s, date = js$jour, mois = js$mois, annee = js$annee,
  y = round(20 + (s == "S1") * 1.5 * (js$annee - 2008) + 3 * sin(js$mois) + rnorm(nrow(js), sd = 3)))))
ds <- ds[!(ds$secteur == "S1" & ds$mois == 6 & ds$annee == 2010), ]
ds <- rbind(ds, data.frame(secteur = "S1", date = c("14/12/2012", "27/08/2011"), mois = c(12, 8), annee = c(2012, 2011), y = c(60, 1)),
            data.frame(secteur = "S3", date = js$jour[c(1, 6, 11)], mois = js$mois[c(1, 6, 11)], annee = js$annee[c(1, 6, 11)], y = c(3, 5, 4)),
            data.frame(secteur = "S4", date = js$jour[c(1, 2, 5, 6, 9, 10)], mois = js$mois[c(1, 2, 5, 6, 9, 10)], annee = js$annee[c(1, 2, 5, 6, 9, 10)], y = c(5, 7, 5, 7, 5, 7)))
o <- sample(nrow(ds))  # (les observations dans le désordre, chacune nommée par sa ligne de ds)
ps <- projet_depuis_tableau(data.frame(nom = paste0("s", o), ds[o, c("secteur", "date", "y")]), "saisons")
ss <- series_variable(ps, numeros(ps, "y"), numeros(ps, "date"), numeros(ps, "secteur"), saisons = "mois")
trs <- tendances_series(ss$points)
ligne_s <- function(s) trs[trs$ligne == s, ]
verif_s <- function(s) {
  r <- ligne_s(s)
  d <- ds[ds$secteur == s, ]
  h <- saisonnier_main(d$annee, d$mois, d$y)
  isTRUE(all.equal(c(r$S, r$var_S, r$p, r$pente, r$bas, r$haut, r$tau, r$saisons), c(h$S, h$var_S, h$p, h$pente, h$ic, h$tau, h$saisons))) &&
    r$test == "saisonnier" && r$n == nrow(d) && !nzchar(r$non_teste)
}
verifier(sprintf("le mois des dates (le jour d'abord) : la saison de chaque point, de 1 à 12 ; une variable qualitative de dates, %s",
                 paste(range(ss$points$saison), collapse = " à ")),
         isTRUE(ss$dates) && identical(ss$saisons$uid, "mois") && ss$sans_saison == 0 &&
           identical(as.integer(ss$points$saison), as.integer(format(as.Date(ds$date[match(ss$points$nom, paste0("s", seq_len(nrow(ds))))], "%d/%m/%Y"), "%m"))))
verifier(sprintf("S1 : une saison-année manquante, une saison vue une seule année, deux valeurs en août 2011 (leur médiane), des ex aequo : S = %g, Var(S) = %g, p = %.2g, pente %.3f par an [%.3f ; %.3f], τ = %.3f, refaits à la main",
                 ligne_s("S1")$S, ligne_s("S1")$var_S, ligne_s("S1")$p, ligne_s("S1")$pente, ligne_s("S1")$bas, ligne_s("S1")$haut, ligne_s("S1")$tau),
         verif_s("S1") && ligne_s("S1")$saisons == 4 && ligne_s("S1")$medianes == 1 && ligne_s("S1")$p < 0.05)
verifier("S2, sans tendance : de même", verif_s("S2") && ligne_s("S2")$medianes == 0)
verifier(sprintf("S3, une année par saison : pas de test (« %s ») ; S4, la même valeur chaque année dans chaque saison : pas de test",
                 ligne_s("S3")$non_teste),
         is.na(ligne_s("S3")$p) && ligne_s("S3")$saisons == 0 &&
           ligne_s("S3")$non_teste == "trop peu d'années par saison : 0 valeur(s) dans les saisons vues au moins 2 années, il en faut au moins 4" &&
           is.na(ligne_s("S4")$p) && ligne_s("S4")$non_teste == "la même valeur chaque année dans chaque saison" &&
           isTRUE(all.equal(trs$p_ajuste, p.adjust(trs$p, "BH"))))
# les mêmes comptages, la saison par une variable qualitative (printemps, été, automne : avril et juin
# font le printemps, donc deux valeurs par printemps, leur médiane), et le temps en années
ds$saison <- c(`4` = "printemps", `6` = "printemps", `8` = "ete", `10` = "automne", `12` = "hiver")[as.character(ds$mois)]
pq <- projet_depuis_tableau(data.frame(nom = paste0("q", seq_len(nrow(ds))), ds[c("secteur", "annee", "saison", "y")]), "saisons_variable")
sq <- series_variable(pq, numeros(pq, "y"), numeros(pq, "annee"), numeros(pq, "secteur"), saisons = numeros(pq, "saison"))
trq <- tendances_series(sq$points)
hq <- saisonnier_main(ds$annee[ds$secteur == "S1"], ds$saison[ds$secteur == "S1"], ds$y[ds$secteur == "S1"])
rq <- trq[trq$ligne == "S1", ]
verifier(sprintf("la saison par une variable qualitative, le temps en années (le même temps deux fois dans une saison : la médiane) : S = %g, p = %.4f, refaits à la main",
                 rq$S, rq$p),
         isTRUE(all.equal(c(rq$S, rq$var_S, rq$p, rq$pente, rq$bas, rq$haut, rq$tau), c(hq$S, hq$var_S, hq$p, hq$pente, hq$ic, hq$tau))) &&
           identical(sq$saisons$nom, "saison") && rq$saisons == 3 && rq$medianes == 8)
pqi <- pq
pqi$observation_id_names <- c("Observation", "periode")
for (i in seq_along(pqi$observations)) pqi$observations[[i]]$ids <- c(pqi$observations[[i]]$ids[1], ds$saison[i])
verifier("la saison par un identifiant des observations : le même test",
         isTRUE(all.equal(tendances_series(series_variable(pqi, numeros(pq, "y"), numeros(pq, "annee"), numeros(pq, "secteur"), saisons = "id:2")$points), trq)))
# nottem (des températures mensuelles à Nottingham, 1920–1939, dans les données de R) : trend::smk.test
# et trend::sea.sens.slope (version 1.1.10) donnent S = 224, Var(S) = 11364, p = 0.0364481815726198
# et une pente de 0,05 ; EnvStats::kendallSeasonalTrendTest (3.1.0) le τ, 224 / (12 × 190)
pn <- projet_depuis_tableau(data.frame(nom = paste0("n", 1:240), temps = as.numeric(time(nottem)), mois = month.abb[cycle(nottem)], temperature = c(nottem)), "nottem")
rn <- tendances_series(series_variable(pn, numeros(pn, "temperature"), numeros(pn, "temps"), saisons = numeros(pn, "mois"))$points)
verifier("nottem : S, Var(S), p et la pente de trend::smk.test et trend::sea.sens.slope, le τ d'EnvStats",
         isTRUE(all.equal(c(rn$S, rn$var_S, rn$p, rn$pente, rn$tau), c(224, 11364, 0.0364481815726198, 0.05, 224 / (12 * 190)), tolerance = 1e-12)) &&
           rn$saisons == 12 && rn$n == 240)
# des observations sans saison : laissées de côté et comptées
pm <- pq
pm$donnees[ds$secteur == "S2" & ds$annee == 2015, as.character(numeros(pq, "saison"))] <- NA
sm <- series_variable(pm, numeros(pq, "y"), numeros(pq, "annee"), numeros(pq, "secteur"), saisons = numeros(pq, "saison"))
hm <- with(ds[ds$secteur == "S2" & ds$annee != 2015, ], saisonnier_main(annee, saison, y))
rm2 <- tendances_series(sm$points)
verifier("des observations sans saison : comptées, laissées de côté par le test (pas par le graphique)",
         sm$sans_saison == 4 && nrow(sm$points) == nrow(sq$points) && rm2$n[rm2$ligne == "S2"] == sum(ds$secteur == "S2") - 4 &&
           isTRUE(all.equal(rm2$S[rm2$ligne == "S2"], hm$S)) && isTRUE(all.equal(rm2$p[rm2$ligne == "S2"], hm$p)))
k4 <- kendall_saisonnier(c(2001.3, 2001.7, 2002.3, 2002.7), c(1, 5, 2, 6), c("a", "b", "a", "b"))
k3 <- kendall_saisonnier(c(2001.3, 2002.3, 2003.3, 2003.7), c(1, 2, 3, 9), c("a", "a", "a", "b"))
verifier(sprintf("4 valeurs dans les saisons qui comptent, testé (deux saisons de 2 ans : S = 2, Var(S) = 2) ; 3, pas de test (« %s »)", k3$non_teste),
         k4$S == 2 && k4$var_S == 2 && isTRUE(all.equal(k4$p, 2 * pnorm(-1 / sqrt(2)))) && !nzchar(k4$non_teste) &&
           is.na(k3$p) && k3$saisons == 1 && startsWith(k3$non_teste, "trop peu d'années par saison : 3 valeur(s)"))
refus_s <- function(expr) tryCatch({ expr; "" }, error = conditionMessage)
verifier("refusés : le mois avec un temps fait de nombres, une variable quantitative pour saisons, la correction de Hamed et Rao avec le test saisonnier",
         refus_s(series_variable(pq, numeros(pq, "y"), numeros(pq, "annee"), saisons = "mois")) == "Les saisons par mois de l'année demandent un temps fait de dates" &&
           refus_s(series_variable(pq, numeros(pq, "y"), numeros(pq, "annee"), saisons = numeros(pq, "y"))) == "Les saisons se font par une variable qualitative ou un identifiant" &&
           startsWith(refus_s(tendances_series(sq$points, autocorrelation = TRUE)), "Le test saisonnier tient les saisons pour indépendantes"))

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
