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
         grepl("^Le temps doit être numérique : secteur_id a des valeurs qui ne sont pas des nombres \\(S1, S2, S3, S4\\)$",
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
verifier("un nom pris par une variable aux autres valeurs, ou qualitative : refusé, et le message dit pourquoi ; aux mêmes valeurs (faite à la main) : elle ressert",
         grepl("^Une variable s'appelle déjà « période BACI \\(2008\\) », sans les valeurs que le modèle BACI lui donne \\(1 quand annee ≥ 2008, 0 avant\\) : elle n'est pas remplacée, et le modèle n'est pas ajusté\\.$",
               erreur_m(pc0, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2008)) &&
           grepl("« impact BACI \\(S1 contre S2, S3\\) », sans les valeurs que le modèle BACI lui donne \\(1 pour S1, 0 pour S2, S3, manquante ailleurs\\)",
                 erreur_m(pc2, mv("densite"), mv("annee"), mv("secteur"), "S1", c("S2", "S3"), 2008)) &&
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
