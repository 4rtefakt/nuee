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
w <- wilcox.test(b, a, paired = TRUE)
r <- test_deux_variables(pl, densite, secteur, appariement = annee, niveaux = c("S1", "S2"))
verifier("t apparié = t.test(paired = TRUE) sur les années communes, quel que soit l'ordre des lignes", r$type == "apparie" && r$n == 8 &&
         identical(as.numeric(r$paires$cle), communes) && isTRUE(all.equal(r$tests$statistique[1], unname(tt$statistic))) && r$tests$ddl1[1] == 7 &&
         isTRUE(all.equal(r$tests$p[1], tt$p.value)) && isTRUE(all.equal(r$difference, c(unname(tt$estimate), as.numeric(tt$conf.int)))))
verifier("d_z de Cohen = différence moyenne / écart-type des différences", isTRUE(all.equal(r$tests$effet[1], mean(b - a) / sd(b - a))))
verifier("Wilcoxon = wilcox.test(paired = TRUE), exact sous 50 paires ; r rang-bisérial = 2V / (n(n + 1)/2) − 1",
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
w <- wilcox.test((1:10) + ecart, 1:10, paired = TRUE)
verifier("ex aequo et différences nulles : Wilcoxon = wilcox.test(), r rang-bisérial sur les différences non nulles",
         isTRUE(all.equal(r$tests$p[2], w$p.value)) && identical(r$exact, grepl("exact", w$method)) && isTRUE(all.equal(r$tests$effet[2], 31 / 36)))
set.seed(5)
base <- rnorm(60)
pa <- appariees(base, rnorm(60, 0.3))
r <- test_deux_variables(pa, numeros(pa, "v"), numeros(pa, "g"), appariement = numeros(pa, "k"))
r$conseille <- "wilcoxon"
verifier("60 paires : Wilcoxon par la loi normale = wilcox.test(), et la phrase le dit", !r$exact &&
         isTRUE(all.equal(r$tests$p[2], wilcox.test(r$paires$b, r$paires$a, paired = TRUE)$p.value)) && grepl("p approchée par la loi normale", phrase_test(r)))
erreur <- function(...) tryCatch({ test_deux_variables(...); "" }, error = conditionMessage)
double <- long
double$annee[double$secteur == "S1" & double$annee == 2004] <- 2003
pd <- projet_depuis_tableau(double, "secteurs")
verifier("une année deux fois pour un secteur : refusé, l'année dite", grepl("S1 a plusieurs lignes pour la même valeur de annee \\(2003\\)",
         erreur(pd, densite, secteur, appariement = annee, niveaux = c("S1", "S2"))))
peu <- numeros_observations(pl)[long$annee %in% c(2003, 2004)]
verifier("moins de trois paires : refusé", grepl("au moins trois paires : S1 et S2 n'ont que 2 valeur", erreur(pl, densite, secteur, observations = peu, appariement = annee)))
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
verifier("une paire de moins de trois années communes n'est pas testée, ni comptée dans la correction",
         identical(r4$toutes$n[r4$toutes$b == "S4"], c(0L, 2L, 2L)) && all(is.na(r4$toutes$p_t[r4$toutes$b == "S4"])) &&
         isTRUE(all.equal(r4$toutes$p_t_holm[r4$toutes$b != "S4"], rt$toutes$p_t_holm)))
set.seed(7)
complet <- expand.grid(annee = 2001:2008, secteur = c("S1", "S2", "S3", "S4"), stringsAsFactors = FALSE)
complet$densite <- round(3 + rnorm(nrow(complet)) + 0.9 * (complet$secteur == "S3"), 2)
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
               p_w = suppressWarnings(wilcox.test(vb, va, paired = TRUE))$p.value, stringsAsFactors = FALSE)
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
verifier("60 secteurs, 2 par site : les 30 paires de chaque site (au-delà de 50 niveaux, c'est le plus grand groupe qui compte)",
         nrow(rgn$toutes) == 30 && all(rgn$toutes$n == 4) && is.null(rgn$note_comparaisons) &&
         is.null(test_deux_variables(pn, nv("densite"), nv("secteur"), appariement = nv("annee"))$toutes))
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
w <- wilcox.test(large$apres[ok], large$avant[ok], paired = TRUE)
r <- test_deux_variables(pw, avant, apres, appariement = "observation")
verifier(sprintf("deux mesures d'une même ligne : après − avant = t.test() et wilcox.test(paired = TRUE), « %s »", r$phrase),
         r$type == "apparie" && r$ligne && r$n == 22 && r$manquantes == 3 && r$a == "avant" && r$b == "apres" && is.null(r$toutes) &&
         isTRUE(all.equal(r$tests$p, c(tt$p.value, w$p.value))) && isTRUE(all.equal(r$difference, c(unname(tt$estimate), as.numeric(tt$conf.int)))) &&
         identical(r$paires$cle, large$nom[ok]) && grepl("\\(n = 22 paires, une par observation\\)\\.$", r$phrase))
verifier("deux mesures par une clé, une mesure et des groupes par la ligne, moins de trois lignes complètes : refusé",
         grepl("s'apparient par l'observation", erreur(pw, avant, apres, appariement = numeros(pw, "site"))) &&
         grepl("par une troisième variable", erreur(pw, avant, numeros(pw, "site"), appariement = "observation")) &&
         grepl("2 observation\\(s\\) seulement", erreur(pw, avant, apres, observations = c(1, 2, 4), appariement = "observation")))

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
