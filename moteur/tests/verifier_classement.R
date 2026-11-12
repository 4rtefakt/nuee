# Vérifie les arbres de décision (moteur/R/arbres.R), l'analyse discriminante linéaire
# (discrimination.R) et les k-moyennes (kmoyennes.R) contre rpart, MASS::lda, kmeans et
# cluster::silhouette appelés directement : pour les arbres, la procédure du script d'arbres de
# régression de référence refaite ici (validation croisée interne par site, règle 1-ET,
# validation externe site par site, règles des feuilles, première coupure, importance,
# stabilité sur 200 tirages bootstrap des sites), sur des données construites comme les siennes
# (des sites, des années, des X manquants). Puis ce que l'interface en reçoit, le texte des
# méthodes, les tableaux exportés, les prédictions, les refus, et l'export vers R (scripts lancés
# dans une session neuve, rapports R Markdown et Quarto).
#   Rscript moteur/tests/verifier_classement.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-8) isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = tol, check.attributes = FALSE))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

etat <- new.env()
etat$modeles <- list()
projet <- function(id) etat$projets[[id]]
modele <- function(id) etat$modeles[[id]]
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE)
garder <- function(id, d, m = ajuster(etat$projets$P1, d)) { etat$modeles[[id]] <- list(projet = "P1", definition = d, modele = m); invisible(id) }
resultat <- function(id) { force(id); fonction_analyse("resultat_", etat$modeles[[id]]$definition$type)(id) }  # (serveur.R : resultat_modele)

# ---------------------------------------------------------------- les données

# 30 sites suivis 8 ans (240 lignes) : une densité qui baisse au-delà de 12 jours de gel, monte
# avec les haies, un effet du site ; des X manquants (comme dans les relevés), un sol qualitatif ;
# une classe de densité (basse, moyenne, haute).
set.seed(11)
n <- 240
site <- rep(sprintf("S%02d", 1:30), each = 8)
annee <- rep(2015:2022, 30)
effet_site <- rep(stats::rnorm(30, 0, 0.5), each = 8)
X <- data.frame(h_nbgelee = round(stats::runif(n, 0, 30)), t_moy = stats::rnorm(n, 12, 2), pluie = stats::rgamma(n, 4, 0.02), haie = stats::runif(n),
                prairie = stats::runif(n), vent = stats::rnorm(n, 5, 1), sol = factor(sample(c("argile", "limon", "sable"), n, TRUE)), stringsAsFactors = FALSE)
y <- 2 + ifelse(X$h_nbgelee > 12, -1.2, 0) + 0.6 * (X$haie > 0.5) + 0.05 * (X$t_moy - 12) + effet_site + stats::rnorm(n, 0, 0.4)
for (k in c("t_moy", "pluie", "haie", "vent")) X[[k]][sample(n, 25)] <- NA
classe <- ifelse(y > 2, "haute", ifelse(y > 1.2, "moyenne", "basse"))
# (code_classe : constant dans chaque classe, une LDA le refuse)
df <- data.frame(nom = paste(site, annee), site = site, annee = annee, X, densite = y, classe = classe, code_classe = as.numeric(factor(classe)), stringsAsFactors = FALSE)
p <- projet_depuis_tableau(df, "essai")
etat$projets <- list(P1 = c(p, list(id = "P1")))
class(etat$projets$P1) <- class(p)
p1 <- etat$projets$P1
u <- function(...) numeros(p, c(...))
vc <- list(by = "variable", variable = u("site"))
xs <- u(names(X))
Xd <- X
Xd$sol <- factor(Xd$sol, levels = sort(unique(as.character(Xd$sol))))  # (les catégories dans l'ordre du projet)

# La procédure du script de référence, refaite avec rpart : un arbre grand (au moins 20 lignes
# par feuille, 5 niveaux, cp = 0,001, 5 substitutions), la validation croisée interne par site
# DANS le contrôle, la règle 1-ET.
CTL <- function(plis) rpart::rpart.control(minbucket = 20, cp = 0.001, maxdepth = 5, maxsurrogate = 5, xval = as.integer(factor(plis)))
ajuste <- function(y, X, site) {
  m <- rpart::rpart(.y ~ ., data.frame(.y = y, X, check.names = FALSE), method = "anova", control = CTL(site))
  ct <- m$cptable
  i <- which.min(ct[, "xerror"])
  rpart::prune(m, cp = ct[which(ct[, "xerror"] <= ct[i, "xerror"] + ct[i, "xstd"])[1], "CP"])
}
q2_loso <- function(y, X, site) {
  pr <- rep(NA, length(y))
  for (s in unique(site)) {
    k <- site == s
    pr[k] <- stats::predict(ajuste(y[!k], X[!k, , drop = FALSE], site[!k]), X[k, , drop = FALSE])
  }
  list(q2 = 1 - sum((y - pr)^2) / sum((y - mean(y))^2), prediction = pr)
}
coupures <- function(m) {
  f <- m$frame
  feu <- which(f$var == "<leaf>")
  ch <- rpart::path.rpart(m, nodes = as.numeric(rownames(f)[feu]), print.it = FALSE)
  data.frame(feuille = seq_along(feu), n = f$n[feu], y_moyen = f$yval[feu], regle = sapply(ch, function(x) paste(x[-1], collapse = " & ")))
}
premiere <- function(m) if (nrow(m$frame) == 1) c(var = "(aucune coupure)", seuil = NA) else c(var = as.character(m$frame$var[1]), seuil = unname(m$splits[1, "index"]))

cat("Arbre de régression : la procédure du script de référence, refaite avec rpart\n")
d <- definir_modele(p, "TREE", x = xs, y = u("densite"), echelle = "none", vc = vc)
m <- ajuster(p, d)
a <- ajuste(y, Xd, site)
loso <- q2_loso(y, Xd, site)
r2 <- 1 - sum(stats::residuals(a)^2) / sum((y - mean(y))^2)
verifier(sprintf("240 observations, 30 groupes (les sites), %d feuilles", sum(a$frame$var == "<leaf>")),
         m$N == 240 && m$G == 30 && identical(m$vc_par, "site") && identical(nrow(m$ajustement$frame), nrow(a$frame)))
verifier(sprintf("table d'élagage = rpart, folds par site dans le contrôle (cp retenu %.4g)", m$cp), proche(m$complet$cptable, rpart::rpart(.y ~ ., data.frame(.y = y, Xd, check.names = FALSE), method = "anova", control = CTL(site))$cptable))
verifier(sprintf("R² = %.4f, comme le script (%.4f)", m$r2, r2), proche(m$r2, r2))
verifier(sprintf("Q² externe (un site retiré, tout refait) = %.4f, comme le script (%.4f) ; mêmes prédictions", m$vc$Q2, loso$q2),
         proche(m$vc$Q2, loso$q2) && proche(m$vc$prediction, loso$prediction))
cp <- coupures(a)
rg <- regles_arbre(m$ajustement)
verifier(sprintf("règles des feuilles (path.rpart) : effectif, moyenne, chemin — %s", rg$regle[1]),
         identical(unname(rg$regle), unname(cp$regle)) && identical(rg$n, cp$n) && proche(rg$valeur, cp$y_moyen))
pc <- premiere(a)
pm <- premiere_coupure(m$ajustement, m$variables)
verifier(sprintf("première coupure : %s < %s", pm$variable, pm$seuil), identical(pm$variable, pc[["var"]]) && proche(pm$seuil, as.numeric(pc[["seuil"]])))
vi <- a$variable.importance
im <- importance_arbre(m$ajustement, m$variables)
verifier(sprintf("importance des variables (%% de leur somme) : %s %.1f %%", im$variable[1], im$pourcentage[1]),
         identical(im$variable, names(vi)) && proche(im$pourcentage, 100 * vi / sum(vi)))
st <- local({
  set.seed(2026)
  us <- unique(site)
  bt <- t(replicate(200, {
    ss <- us[sample.int(length(us), length(us), replace = TRUE)]
    k <- unlist(lapply(ss, function(s) which(site == s)))
    premiere(ajuste(y[k], Xd[k, , drop = FALSE], rep(seq_along(ss), sapply(ss, function(s) sum(site == s)))))
  }))
  agg <- stats::aggregate(as.numeric(bt[, "seuil"]), list(var = bt[, "var"]), function(x) c(n = length(x), seuil_median = stats::median(x, na.rm = TRUE)))
  data.frame(var = agg$var, frequence = 100 * agg$x[, 1] / 200, seuil_median = agg$x[, 2])
})
garder("A1", d, m)
sa <- demande_analyse_arbre_stabilite(list(modele = "A1"))$stabilite
sb <- st[match(sa$variable, st$var), ]
verifier(sprintf("stabilité sur 200 tirages bootstrap des sites (graine 2026) : %s %.0f %%, seuil médian %s",
                 sa$variable[1], sa$frequence[1], format(sa$seuil_median[1])),
         setequal(sa$variable, st$var) && proche(sa$frequence, sb$frequence) && proche(sa$seuil_median, sb$seuil_median) && !is.unsorted(-sa$frequence))
# (le script de référence donne les plis à côté d'un argument control : rpart garde alors les 10
# groupes au hasard du contrôle ; ici, les plis sont dans le contrôle)
dat <- data.frame(.y = y, Xd, check.names = FALSE)
plis <- as.integer(factor(site))
ctl0 <- rpart::rpart.control(minbucket = 20, cp = 0.001, maxdepth = 5, maxsurrogate = 5)
set.seed(1); h1 <- rpart::rpart(.y ~ ., dat, method = "anova", xval = plis, control = ctl0)$cptable[, "xerror"]
set.seed(2); h2 <- rpart::rpart(.y ~ ., dat, method = "anova", xval = plis, control = ctl0)$cptable[, "xerror"]
set.seed(1); n1 <- arbre_elague(dat, site, "anova", ctl0, "1se")$complet$cptable[, "xerror"]
set.seed(2); n2 <- arbre_elague(dat, site, "anova", ctl0, "1se")$complet$cptable[, "xerror"]
verifier("xval à côté de control : 10 groupes au hasard (deux graines, deux résultats) ; dans le contrôle : par site, sans hasard",
         !isTRUE(all.equal(h1, h2)) && identical(n1, n2) && proche(n1, m$complet$cptable[, "xerror"]))

# 48 observations (6 sites) : 20 par feuille, il faudrait 60 observations pour une coupure ; le
# minimum ramené au sixième (8), l'arbre se coupe, comme rpart avec 8 par feuille
d48 <- definir_modele(p, "TREE", x = xs, y = u("densite"), echelle = "none", vc = vc, observations = 1:48)
m48 <- ajuster(p, d48)
a48 <- rpart::rpart(.y ~ ., data.frame(.y = y[1:48], Xd[1:48, ], check.names = FALSE), method = "anova",
                    control = rpart::rpart.control(minbucket = 8, cp = 0.001, maxdepth = 5, maxsurrogate = 5, xval = as.integer(factor(site[1:48]))))
garder("S1", d48, m48)
verifier(sprintf("48 observations : 20 par feuille ramenées à 8 (coupure à partir de 24), l'arbre complet de rpart (%d nœuds) ; dit dans le texte et le résultat",
                 nrow(m48$complet$frame)),
         m48$controle$minbucket == 8 && m48$controle$minsplit == 24 && nrow(m48$complet$frame) > 1 && identical(as.character(m48$complet$frame$var), as.character(a48$frame$var)) &&
           proche(m48$complet$cptable, a48$cptable) && any(grepl("ramenées au sixième des 48", texte_methodes(p, d48, m48, "fr", "essai")$paragraphes)) &&
           identical(resultat("S1")$minbucket_utilise, 8L) && minimum_feuille(20, 120) == 20 && minimum_feuille(20, 119) == 19)

cat("Arbre de classement, réglages, prédiction, jeu de test\n")
dc <- definir_modele(p, "TREE", x = xs, classe = u("classe"), echelle = "none", vc = vc, options = list(minbucket = 10, elagage = "min"))
mc <- ajuster(p, dc)
cl <- factor(classe)
ctlc <- rpart::rpart.control(minbucket = 10, cp = 0.001, maxdepth = 5, maxsurrogate = 5, xval = plis)
ac <- rpart::rpart(.y ~ ., data.frame(.y = cl, Xd, check.names = FALSE), method = "class", control = ctlc)
ctc <- ac$cptable
ac <- rpart::prune(ac, cp = ctc[which.min(ctc[, "xerror"]), "CP"])
pa <- stats::predict(ac, type = "class")
pv <- rep(NA_character_, n)
for (s in unique(site)) {
  k <- site == s
  b <- rpart::rpart(.y ~ ., data.frame(.y = droplevels(cl[!k]), Xd[!k, ], check.names = FALSE), method = "class",
                    control = rpart::rpart.control(minbucket = 10, cp = 0.001, maxdepth = 5, maxsurrogate = 5, xval = as.integer(factor(site[!k]))))
  ct <- b$cptable
  b <- rpart::prune(b, cp = ct[which.min(ct[, "xerror"]), "CP"])
  pr <- stats::predict(b, Xd[k, ], type = "prob")
  pv[k] <- colnames(pr)[max.col(pr, ties.method = "first")]
}
verifier(sprintf("arbre de classement (élagage à l'erreur minimale, 10 par feuille) : %.1f %% bien classées, %.1f %% en validation croisée par site",
                 100 * mc$classification$ajuste$global, 100 * mc$classification$vc$global),
         mc$classement && identical(nrow(mc$ajustement$frame), nrow(ac$frame)) && proche(mc$classification$ajuste$global, mean(as.character(pa) == classe)) &&
           proche(mc$classification$vc$global, mean(pv == classe)) && identical(mc$classification$vc$predite, pv))
nouvelles <- data.frame(h_nbgelee = c(5, 20, NA), t_moy = c(NA, 11, 13), pluie = c(100, NA, 300), haie = c(0.9, 0.2, NA), prairie = c(0.5, 0.5, 0.5),
                        vent = c(5, NA, 5), sol = factor(c("sable", NA, "argile"), levels = levels(Xd$sol)), check.names = FALSE)
pn <- predire(p, m, d, as.numeric(rownames(p$donnees))[1:3])
dn <- data.frame(.y = 0, X[1:3, ], check.names = FALSE)
dn$sol <- factor(dn$sol, levels = levels(Xd$sol))
an <- a
an$frame$yval <- seq_len(nrow(a$frame))  # (la ligne de chaque nœud : predict rend celle de sa feuille)
verifier("prédire : la réponse de rpart (substitutions pour les X manquants), sa feuille",
         proche(pn[["densite prédit"]], stats::predict(a, dn)) && proche(pn[["densite observé"]], y[1:3]) &&
           identical(pn$feuille, match(stats::predict(an, dn), which(a$frame$var == "<leaf>"))))
pf <- predire(list(donnees = matrix(c(nouvelles$h_nbgelee, nouvelles$t_moy, nouvelles$pluie, nouvelles$haie, nouvelles$prairie, nouvelles$vent, as.integer(nouvelles$sol)),
                                    3, dimnames = list(1:3, as.character(xs)))), m, d, 1:3)
verifier("prédire un fichier (un projet sans variables) : comme rpart sur les mêmes valeurs, manquantes comprises", proche(pf[["densite prédit"]], stats::predict(a, nouvelles)))
dt <- definir_modele(p, "TREE", x = xs, y = u("densite"), echelle = "none", vc = vc, test = 221:240)
mt <- ajuster(p, dt)
verifier(sprintf("jeu de test (20 observations hors du modèle) : RMSEP %.3f, Q² externe %.3f", mt$test$statistiques$RMSEP, mt$test$statistiques$Q2ext),
         mt$N == 220 && mt$test$n == 20 && proche(mt$test$observations[["densite prédit"]], stats::predict(mt$ajustement, x_bruts(p, mt$variables, 221:240))))
verifier("refus : deux réponses, aucune, Y qualitative, termes croisés, moins de trois groupes",
         grepl("pas les deux", refus(ajuster(p, definir_modele(p, "TREE", x = xs, y = u("densite"), classe = u("classe"), echelle = "none", vc = vc)))) &&
           grepl("demande une réponse", refus(ajuster(p, definir_modele(p, "TREE", x = xs, echelle = "none", vc = vc)))) &&
           grepl("qualitative", refus(ajuster(p, definir_modele(p, "TREE", x = u("haie"), y = u("sol"), echelle = "none", vc = vc)))) &&
           grepl("croisés", refus(ajuster(p, definir_modele(p, "TREE", x = u("haie", "vent"), y = u("densite"), echelle = "none", vc = vc, croises = list(u("haie", "vent")))))) &&
           grepl("trois groupes", refus(ajuster(p, definir_modele(p, "TREE", x = xs, y = u("densite"), echelle = "none", vc = vc, observations = 1:16)))))

# une classe rare, dans un seul site : sans lui, une seule classe reste (rpart ne s'ajuste pas)
rare <- ifelse(site == "S01" & seq_len(n) %% 2 == 0, "rare", "courante")
pr1 <- projet_depuis_tableau(data.frame(df, rare = rare), "rare")
dr <- definir_modele(pr1, "TREE", x = numeros(pr1, names(X)), classe = numeros(pr1, "rare"), echelle = "none",
                     vc = list(by = "variable", variable = numeros(pr1, "site")), options = list(minbucket = 2))
mr <- tryCatch(ajuster(pr1, dr), error = function(e) e)
verifier("une classe présente dans un seul site : l'arbre s'ajuste, ce site prédit « courante » (sans lui, seule classe des autres)",
         !inherits(mr, "error") && all(mr$vc$proba[site == "S01", "courante"] == 1) && all(mr$vc$proba[site == "S01", "rare"] == 0))
# (le modèle sans aucune observation « haute » ; le jeu de test : 6 « haute » et 14 autres)
sans_haute <- which(classe != "haute")
dti <- definir_modele(p, "TREE", x = xs, classe = u("classe"), echelle = "none", vc = vc, observations = c(sans_haute, which(classe == "haute")[1:6]),
                      test = c(which(classe == "haute")[1:6], sans_haute[1:14]))
mti <- ajuster(p, dti)
tab <- mti$test$classement$table
verifier(sprintf("jeu de test avec une classe que l'arbre n'a pas vue (%d) : la part bien classée est celle du tableau (%.0f %%)",
                 mti$test$classement$inconnues, 100 * mti$test$classement$bien_classees),
         mti$test$classement$inconnues == sum(!classe[mti$test$observations$uid] %in% mti$classes) &&
           proche(mti$test$classement$bien_classees, sum(diag(tab)) / sum(tab)) &&
           any(grepl("n'a pas vue", texte_methodes(p, dti, mti, "fr", "essai")$paragraphes)))

cat("Analyse discriminante linéaire : MASS::lda\n")
dl <- definir_modele(p, "LDA", x = u("h_nbgelee", "t_moy", "haie", "sol"), classe = u("classe"), echelle = "UV", vc = vc)
ml <- ajuster(p, dl)
comp <- stats::complete.cases(X[c("h_nbgelee", "t_moy", "haie")])
XL <- cbind(as.matrix(X[comp, c("h_nbgelee", "t_moy", "haie")]), `sol(argile)` = as.numeric(X$sol[comp] == "argile"), `sol(limon)` = as.numeric(X$sol[comp] == "limon"))
fl <- MASS::lda(XL, factor(classe[comp]))
pl <- stats::predict(fl, XL)
verifier(sprintf("%d observations complètes ; la dernière catégorie du sol laissée de côté (déduite des autres)", ml$N),
         ml$N == sum(comp) && identical(ml$retires, "sol(sable)") && identical(ml$colonnes, colnames(XL)))
verifier("probabilités a posteriori, classes et scores = MASS::lda sur les valeurs brutes (l'échelle du dialogue n'y fait rien)",
         proche(ml$proba, pl$posterior) && identical(ml$classification$ajuste$predite, as.character(pl$class)) && proche(abs(ml$scores), abs(pl$x)))
verifier(sprintf("coefficients (unités des variables), parts de la séparation (%.1f %%, %.1f %%)", 100 * ml$part[1], 100 * ml$part[2]),
         proche(abs(ml$coefficients), abs(fl$scaling)) && proche(ml$part, fl$svd^2 / sum(fl$svd^2)) && proche(abs(ml$structure), abs(stats::cor(XL, pl$x))))
gl <- site[comp]
pvl <- matrix(NA_real_, nrow(XL), 3)
for (s in unique(gl)) {
  k <- gl == s
  b <- MASS::lda(XL[!k, ], factor(classe[comp][!k]))
  pvl[k, ] <- stats::predict(b, XL[k, , drop = FALSE])$posterior
}
verifier(sprintf("validation croisée par site : %.1f %% bien classées, = MASS::lda refait sans chaque site", 100 * ml$classification$vc$global), proche(ml$vc$proba, pvl))
de <- definir_modele(p, "LDA", x = u("h_nbgelee", "t_moy", "haie"), classe = u("classe"), echelle = "none", vc = vc, options = list(priors = "egales"))
me <- ajuster(p, de)
fe <- MASS::lda(XL[, 1:3], factor(classe[comp]), prior = rep(1 / 3, 3))
verifier("probabilités a priori égales", proche(me$proba, stats::predict(fe, XL[, 1:3])$posterior) && proche(me$ajustement$prior, rep(1 / 3, 3)))
dlt <- definir_modele(p, "LDA", x = u("h_nbgelee", "t_moy", "haie", "sol"), classe = u("classe"), echelle = "none", vc = vc, test = 1:12)
mlt <- ajuster(p, dlt)
X5 <- cbind(as.matrix(X[c("h_nbgelee", "t_moy", "haie")]), `sol(argile)` = as.numeric(X$sol == "argile"), `sol(limon)` = as.numeric(X$sol == "limon"))
apprend <- stats::complete.cases(X5) & !(seq_len(n) %in% 1:12)
okt <- stats::complete.cases(X5[1:12, ])
pt <- rep(NA_character_, 12)
pt[okt] <- as.character(stats::predict(MASS::lda(X5[apprend, ], factor(classe[apprend])), X5[1:12, ][okt, , drop = FALSE])$class)
verifier(sprintf("jeu de test (12 observations, %d complètes) : classé par MASS::lda ajustée sans lui ; %.0f %% bien classées", sum(okt), 100 * mlt$test$classement$bien_classees),
         mlt$test$n == 12 && identical(mlt$test$observations$classe_predite, pt) && sum(mlt$test$classement$table) == sum(okt))
pl2 <- predire(p, ml, dl, c(1, 2, 3))
xn <- XL[match(c(1, 2, 3), which(comp)), , drop = FALSE]
verifier("prédire : les probabilités de MASS ; une observation incomplète sans classe",
         proche(as.matrix(pl2[comp[1:3], paste("probabilité", ml$classes)]), stats::predict(fl, xn[stats::complete.cases(xn), , drop = FALSE])$posterior) &&
           all(is.na(pl2[!comp[1:3], "classe prédite"])))
langue_r <- Sys.getenv("LANGUAGE")
pco <- projet_depuis_tableau(data.frame(df, somme = df$t_moy + df$haie + stats::rnorm(n, 0, 1e-6)), "colineaires")
dco <- definir_modele(pco, "LDA", x = numeros(pco, c("h_nbgelee", "t_moy", "haie", "somme")), classe = numeros(pco, "classe"), echelle = "none",
                      vc = list(by = "variable", variable = numeros(pco, "site")))
colin <- vapply(c("en", "fr"), function(l) {
  Sys.setenv(LANGUAGE = l)
  avert <- character(0)
  mco <- withCallingHandlers(ajuster(pco, dco), warning = function(w) { avert <<- c(avert, conditionMessage(w)); invokeRestart("muffleWarning") })
  isTRUE(mco$colineaires) && !length(avert)
}, TRUE)
Sys.setenv(LANGUAGE = langue_r)
verifier("variables colinéaires : signalées, que R parle anglais ou français (le message de MASS traduit)", all(colin))
huit <- c(which(classe == "basse")[1:3], which(classe == "moyenne")[1:3], which(classe == "haute")[1:2])
verifier("refus : sans classe, une Y, trop de termes, une variable constante dans chaque classe (par son nom)",
         grepl("classe", refus(ajuster(p, definir_modele(p, "LDA", x = u("haie"), echelle = "none", vc = vc)))) &&
           grepl("pas de variable Y", refus(ajuster(p, definir_modele(p, "LDA", x = u("haie"), y = u("vent"), classe = u("classe"), echelle = "none", vc = vc)))) &&
           grepl("Trop de termes", refus(ajuster(p, definir_modele(p, "LDA", x = u("h_nbgelee", "prairie", "annee", "densite", "sol"), classe = u("classe"), echelle = "none",
                                                                 observations = huit)))) &&
           grepl("code_classe", refus(ajuster(p, definir_modele(p, "LDA", x = u("prairie", "code_classe"), classe = u("classe"), echelle = "none", vc = vc)))))

cat("k-moyennes : kmeans et cluster::silhouette\n")
dk <- definir_modele(p, "KMEANS", x = u("h_nbgelee", "t_moy", "haie", "prairie"), echelle = "UV", options = list(k = 3))
mk <- ajuster(p, dk)
ck <- stats::complete.cases(X[c("h_nbgelee", "t_moy", "haie", "prairie")])
XK <- scale(as.matrix(X[ck, c("h_nbgelee", "t_moy", "haie", "prairie")]))
set.seed(1)
kk <- stats::kmeans(XK, 3, nstart = 25, iter.max = 100)
ordre <- order(-kk$size, 1:3)
verifier(sprintf("k = 3 sur %d observations complètes, UV : mêmes groupes (numérotés par taille : %s), somme des carrés intra, R² %.3f", mk$N, paste(mk$tailles, collapse = ", "), mk$r2),
         mk$N == sum(ck) && identical(mk$groupe, match(kk$cluster, ordre)) && proche(sum(mk$intra), kk$tot.withinss) && proche(mk$r2, kk$betweenss / kk$totss) &&
           proche(mk$centres, kk$centers[ordre, ]))
sk <- cluster::silhouette(match(kk$cluster, ordre), stats::dist(XK))
verifier(sprintf("silhouettes = cluster::silhouette (moyenne %.3f)", mean(sk[, "sil_width"])), proche(mk$silhouette$largeur, sk[, "sil_width"]) && proche(mk$silhouette$voisin, sk[, "neighbor"]))
verifier("centres dans les unités des variables ; deux premières composantes = prcomp",
         proche(mk$centres_unites, stats::aggregate(X[ck, c("h_nbgelee", "t_moy", "haie", "prairie")], list(match(kk$cluster, ordre)), mean)[-1]) &&
           proche(abs(mk$acp$scores), abs(stats::prcomp(XK)$x[, 1:2])))
garder("K1", dk, mk)
ch <- demande_analyse_kmeans_choix(list(modele = "K1"))$choix
verifier(sprintf("choix de k (2 à 10) : silhouette la plus grande pour k = %d ; k = 3 comme le modèle", ch$k[which.max(ch$silhouette)]),
         identical(ch$k, 2:10) && proche(ch$intra[ch$k == 3], sum(mk$intra)) && proche(ch$silhouette[ch$k == 3], mean(mk$silhouette$largeur)))
pk <- predire(p, mk, dk, c(1, 2, 300))
pkk <- predire(p, mk, dk, as.numeric(rownames(p$donnees))[which(ck)[1:5]])
verifier("prédire : le groupe du centre le plus proche (une observation du modèle retrouve le sien)", identical(pkk[["groupe prédit"]], mk$groupe[1:5]) && nrow(pk) == 3)
p1o <- predire(p, mk, dk, as.numeric(rownames(p$donnees))[which(ck)[2]])
verifier("prédire une seule observation : une ligne, son groupe et sa distance au centre",
         nrow(p1o) == 1 && identical(p1o[["groupe prédit"]], mk$groupe[2]) && proche(p1o[["distance au centre"]], mk$distance[2]))
changer_projet <- function(q, f, etiquette) { pp <- f(projet(q$projet)); etat$projets[[q$projet]] <- pp; list(id = q$projet) }
invisible(demande_analyse_kmeans_variable(list(modele = "K1", nom = "groupes k3")))
pp <- etat$projets$P1
v <- variable_du_projet(pp, numeros(pp, "groupes k3"))
verifier("les groupes en variable qualitative du projet (catégories 1 à 3, les observations incomplètes sans valeur)",
         identical(as.character(v$categories), c("1", "2", "3")) && identical(unname(pp$donnees[as.character(mk$observations), as.character(v$uid)]), as.numeric(mk$groupe)) &&
           all(is.na(pp$donnees[as.character(setdiff(as.numeric(rownames(pp$donnees)), mk$observations)), as.character(v$uid)])))
etat$projets$P1 <- p1
rk <- resultat("K1")
verifier("le plan des groupes : les centres sous les noms des composantes des observations (CP1, CP2)",
         identical(names(rk$acp$centres), c("groupe", "CP1", "CP2")) && all(c("CP1", "CP2") %in% names(rk$observations)) &&
           proche(as.matrix(rk$acp$centres[c("CP1", "CP2")]), mk$acp$centres))
verifier("refus : une réponse, trop de groupes pour les observations",
         grepl("pas de réponse", refus(ajuster(p, definir_modele(p, "KMEANS", x = u("haie"), y = u("vent"), echelle = "UV")))) &&
           grepl("distinctes", refus(ajuster(p, definir_modele(p, "KMEANS", x = u("haie"), echelle = "UV", observations = 1:4, options = list(k = 4))))))

cat("Ce que l'interface reçoit, texte des méthodes, tableaux\n")
garder("A2", dc, mc)
garder("L1", dl, ml)
garder("T1", dt, mt)
for (id in c("A1", "A2", "L1", "K1", "T1")) {
  r <- resultat(id)
  verifier(sprintf("%s (%s) : analyse qui prédit, chiffres « %s », vues %s, sérialisable", id, r$type, r$chiffres, paste(r$vues_defaut, collapse = ", ")),
           isTRUE(r$analyse) && isTRUE(r$prediction) && nzchar(r$chiffres) && length(r$vues_defaut) == 2 && r$N == etat$modeles[[id]]$modele$N &&
             inherits(json(r), "json") && is.data.frame(r$observations) && identical(names(r$observations)[1:2], c("uid", "nom")))
}
r <- resultat("A1")
verifier("arbre de régression : observé, ajusté, résidus ; Q² et RMSEcv pour Comparer les modèles ; ses nœuds pour le dessin",
         all(c("densite observé", "densite ajusté", "residu", "quantile_normal", "feuille") %in% names(r$observations)) &&
           proche(r$comparaison$Q2, m$vc$Q2) && proche(r$RMSEcv$densite, m$vc$RMSEcv) && identical(r$arbre$id[1], 1) && !is.null(r$stabilite))
r <- resultat("L1")
verifier("LDA : sa classe pour Modifier le modèle, ses scores et ses probabilités", identical(as.numeric(r$reglages$classe), as.numeric(u("classe"))) &&
           all(c("classe", "LD1", "LD2", "probabilité haute") %in% names(r$observations)) && nrow(r$coefficients) == 5)
for (langue in c("fr", "en")) {
  tt <- texte_methodes(p, d, etat$modeles$A1$modele, langue, "essai")
  tl <- texte_methodes(p, dl, ml, langue, "essai")
  tk <- texte_methodes(p, dk, etat$modeles$K1$modele, langue, "essai")
  verifier(sprintf("texte des méthodes (%s) : rpart, 1-ET, stabilité, Breiman ; MASS, Fisher, Venables ; kmeans, Hartigan, Rousseeuw", langue),
           any(grepl("rpart", tt$paragraphes)) && any(grepl("1-(ET|SE)", tt$paragraphes)) && any(grepl("bootstrap", tt$paragraphes)) && any(grepl("^Breiman", tt$references)) &&
             any(grepl("^Therneau", tt$references)) && any(grepl("MASS", tl$paragraphes)) && any(grepl("^Fisher", tl$references)) && any(grepl("^Venables", tl$references)) &&
             any(grepl("Hartigan", tk$paragraphes)) && any(grepl("^Rousseeuw", tk$references)) && any(grepl("^Maechler", tk$references)))
}
ta <- tables_resultats(p, etat$modeles$A2$modele)
tl <- tables_resultats(p, ml)
tk <- tables_resultats(p, etat$modeles$K1$modele)
verifier("tableaux exportés : des data.frame (règles, importance, élagage, classement ; coefficients ; centres, choix de k)",
         all(vapply(c(ta, tl, tk), is.data.frame, TRUE)) && all(c("regles", "importance", "elagage", "classement_vc") %in% names(ta)) &&
           all(c("coefficients", "structure", "classement_vc") %in% names(tl)) && all(c("groupes", "centres", "choix_k") %in% names(tk)))

genre <- RNGkind()
s1 <- stabilite_arbre(etat$modeles$A1$modele)
suppressWarnings(RNGkind(sample.kind = "Rounding"))  # (R prévient que ce tirage n'est pas uniforme)
s2 <- stabilite_arbre(etat$modeles$A1$modele)
verifier("stabilité : les mêmes tirages quel que soit le générateur de la session, rendu tel quel",
         identical(s1, s2) && identical(RNGkind()[3], "Rounding"))
RNGkind(genre[1], genre[2], genre[3])
d1 <- definir_modele(p, "TREE", x = u("prairie"), y = u("densite"), echelle = "none", vc = vc, observations = 1:50, options = list(cp = 0.9))
m1 <- ajuster(p, d1)
m1$stabilite <- stabilite_arbre(m1)
t_en <- texte_methodes(p, d1, m1, "en", "essai")$paragraphes
verifier("un arbre sans coupure : « une seule feuille », aucune coupure dans les tirages, dans la langue du texte",
         any(grepl("a single leaf", t_en)) && any(grepl("has no split in", t_en)) && !any(grepl("aucune coupure|1 leaves", t_en)) &&
           identical(stabilite_affichee(m1$stabilite, fr = FALSE)$variable[1], "(no split)") &&
           identical(stabilite_affichee(m1$stabilite, fr = TRUE)$variable[1], "(aucune coupure)"))

cat("Export vers R : scripts autonomes et rapports\n")
dossier <- tempfile("classement")
dir.create(dossier)
rscript <- file.path(R.home("bin"), "Rscript")
lanceur <- file.path(dossier, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".r <- mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL))", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, objets) {
  sortie <- tempfile("sortie", dossier, ".rds")
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ","))),
                                       stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
cas <- list(list(id = "A1", langue = "fr", quoi = "arbre de régression, stabilité"), list(id = "T1", langue = "en", quoi = "arbre de régression, jeu de test"),
            list(id = "A2", langue = "en", quoi = "arbre de classement"), list(id = "S1", langue = "fr", quoi = "arbre sur 48 observations, 8 par feuille"), list(id = "L1", langue = "fr", quoi = "LDA"), list(id = "K1", langue = "en", quoi = "k-moyennes, choix de k"))
for (k in cas) {
  e <- etat$modeles[[k$id]]
  r <- ecrire_script(p, e$definition, file.path(dossier, paste0(k$id, ".R")), "moteur", m = e$modele, version = "essai", langue = k$langue)
  nm <- function(x) nom_classement(x, k$langue)
  quoi <- switch(e$modele$type, TREE = c("elagage", "vc"), LDA = c("resultat", "vc"), KMEANS = c("groupe", "r2"))
  s <- lancer(r$script, nm(quoi))
  memes <- length(s) && switch(e$modele$type,
    TREE = proche(s[[nm("elagage")]], e$modele$complet$cptable) && proche(s[[nm("vc")]], if (e$modele$classement) e$modele$vc$proba else e$modele$vc$prediction),
    LDA = proche(s[[nm("resultat")]][[nm("proba")]], e$modele$proba) && proche(s[[nm("vc")]], e$modele$vc$proba),
    KMEANS = identical(as.numeric(s[[nm("groupe")]]), as.numeric(e$modele$groupe)) && proche(s[[nm("r2")]], e$modele$r2))
  texte <- readLines(r$script, encoding = "UTF-8")
  verifier(sprintf("%s (%s, %s) : sans écart vu par le moteur ; lancé seul, mêmes résultats ; sans library()", k$id, k$quoi, k$langue),
           is.null(r$ecart) && memes && !any(grepl("library(", sub("#.*$", "", texte), fixed = TRUE)) &&
             (k$langue == "fr" || !any(grepl("arbre_elague|validation_lda|kmeans_ordonne", texte))))
  if (!is.null(r$ecart) || !isTRUE(memes)) cat("    écart :", r$ecart, "/", utils::tail(attr(s, "messages"), 3), "\n")
}
for (ext in c("Rmd", "qmd")) {
  e <- etat$modeles$A2
  r <- ecrire_script(p, e$definition, file.path(dossier, paste0("rapport.", ext)), "moteur", m = e$modele, version = "essai", langue = if (ext == "Rmd") "fr" else "en",
                     donnees = "csv", rdata = TRUE)
  texte <- readLines(r$script, encoding = "UTF-8")
  if (!is.null(r$ecart)) cat("    écart :", r$ecart, "\n")
  verifier(sprintf("rapport %s : méthodes, code, résultats, références ; données en CSV et .RData à côté", ext),
           is.null(r$ecart) && any(grepl("^```\\{r modele\\}", texte)) && any(grepl("^## (Méthodes|Methods)", texte)) && any(grepl("^- .*Breiman", texte)) &&
             length(r$chemins) == 3 && all(file.exists(r$chemins)))
}

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
