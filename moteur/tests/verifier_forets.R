# Vérifie les forêts aléatoires (moteur/R/forets.R) contre ranger appelé directement, avec la même
# graine et les mêmes réglages : les mêmes prédictions hors sac, erreur, R², importances (par
# permutation, divisée ou non par son erreur-type, et par l'impureté), prédictions de toute la
# forêt et de nouvelles observations ; l'erreur hors sac selon le nombre d'arbres refaite à la main
# à partir de la prédiction de chaque arbre et de ses tirages ; la validation croisée par site
# (chaque site prédit par ranger refait sans lui) ; la dépendance partielle refaite à la main ; une
# forêt de classement (classes hors sac, tableau de classement de ranger, parts des votes) ; les
# réglages par défaut de ranger ; le jeu de test ; les refus ; ce que l'interface reçoit, le texte
# des méthodes en français et en anglais, les tableaux exportés ; le hasard de la séance intact.
#   Rscript moteur/tests/verifier_forets.R
# ranger doit être installé (le moteur s'en sert) ; NUEE_RANGER=<bibliothèque> en ajoute une où il l'est.

if (nzchar(Sys.getenv("NUEE_RANGER"))) .libPaths(c(.libPaths(), Sys.getenv("NUEE_RANGER")))
if (!requireNamespace("ranger", quietly = TRUE)) stop("ranger n'est pas installé (NUEE_RANGER=<bibliothèque> pour en donner une où il l'est)")
source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-10) isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = tol, check.attributes = FALSE))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

etat <- new.env()
etat$modeles <- list()
projet <- function(id) etat$projets[[id]]
modele <- function(id) etat$modeles[[id]]
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE)
garder <- function(id, d, m) { etat$modeles[[id]] <- list(projet = "P1", definition = d, modele = m); invisible(id) }
resultat <- function(id) { force(id); fonction_analyse("resultat_", etat$modeles[[id]]$definition$type)(id) }  # (serveur.R : resultat_modele)

# ---------------------------------------------------------------- les données
# 30 sites suivis 8 ans : une densité qui baisse au-delà de 12 jours de gel, monte avec les haies et
# sur sable ; des X manquants ; une classe de densité.
set.seed(11)
n <- 240
site <- rep(sprintf("S%02d", 1:30), each = 8)
annee <- rep(2015:2022, 30)
X <- data.frame(h_gel = round(stats::runif(n, 0, 30)), t_moy = stats::rnorm(n, 12, 2), pluie = stats::rgamma(n, 4, 0.02), haie = stats::runif(n),
                sol = factor(sample(c("argile", "limon", "sable"), n, TRUE)), stringsAsFactors = FALSE)
y <- 2 + ifelse(X$h_gel > 12, -1.2, 0) + 0.6 * (X$haie > 0.5) + 0.05 * (X$t_moy - 12) + 0.3 * (X$sol == "sable") +
  rep(stats::rnorm(30, 0, 0.4), each = 8) + stats::rnorm(n, 0, 0.4)
for (k in c("t_moy", "haie")) X[[k]][sample(n, 12)] <- NA
classe <- ifelse(y > 2, "haute", ifelse(y > 1.2, "moyenne", "basse"))
df <- data.frame(nom = paste(site, annee), site = site, annee = annee, X, densite = y, classe = classe, stringsAsFactors = FALSE)
p <- projet_depuis_tableau(df, "essai")
etat$projets <- list(P1 = c(p, list(id = "P1")))
class(etat$projets$P1) <- class(p)
u <- function(...) numeros(p, c(...))
vc <- list(by = "variable", variable = u("site"))
xs <- u(names(X))
cc <- stats::complete.cases(X)
Xd <- X[cc, ]
Xd$sol <- factor(Xd$sol, levels = sort(unique(as.character(X$sol))))  # (les catégories dans l'ordre du projet)
yd <- y[cc]
sd <- site[cc]
K <- ncol(X)
ref <- function(x, y, ..., mtry = floor(sqrt(K)), min.node.size = 5) {
  ranger::ranger(x = x, y = y, num.trees = 500, mtry = mtry, min.node.size = min.node.size, replace = TRUE, respect.unordered.factors = "order",
                 seed = 1, num.threads = 1, verbose = FALSE, ...)
}
pred <- function(rf, x, ...) stats::predict(rf, x, num.threads = 1, seed = 1, verbose = FALSE, ...)$predictions

cat("Forêt de régression : ranger appelé directement, même graine, mêmes réglages\n")
avant <- .Random.seed
d <- definir_modele(p, "FOREST", x = xs, y = u("densite"), echelle = "none", vc = vc)
m <- ajuster(p, d)
verifier("le hasard de la séance n'a pas bougé (ranger reçoit sa graine)", identical(avant, .Random.seed))
rf <- ref(Xd, yd, importance = "permutation", keep.inbag = TRUE)
verifier(sprintf("%d observations complètes sur %d (%d laissées de côté, une valeur X manque), 30 groupes, mtry = %d, 5 par nœud",
                 m$N, n, length(m$exclues), m$reglages_foret$mtry_utilise),
         m$N == sum(cc) && length(m$exclues) == sum(!cc) && m$manquantes_x == sum(!cc) && m$G == 30 && m$reglages_foret$mtry_utilise == 2 &&
           m$reglages_foret$min_noeud_utilise == 5)
verifier(sprintf("hors sac : les prédictions de ranger, MSE %.4f et R² %.4f", m$oob$mse, m$oob$r2),
         proche(m$oob$prediction, rf$predictions, 1e-12) && proche(m$oob$mse, rf$prediction.error, 1e-12) && proche(m$oob$r2, rf$r.squared, 1e-12))
imp <- function(m, quoi) m$importance[[quoi]][match(names(X), m$importance$variable)]
ri <- ref(Xd, yd, importance = "impurity")
verifier(sprintf("importances : par permutation (%s en tête) et par l'impureté, celles de ranger", m$importance$variable[1]),
         proche(imp(m, "permutation"), rf$variable.importance, 1e-12) && proche(imp(m, "impurete"), ri$variable.importance, 1e-12) &&
           !is.unsorted(-m$importance$permutation))
verifier("toute la forêt sur ses observations : la prédiction de ranger", proche(m$ajuste, pred(rf, Xd), 1e-12))
# l'erreur hors sac selon le nombre d'arbres, refaite à la main (randomForest la donne ainsi)
P <- pred(rf, Xd, predict.all = TRUE)
dedans <- do.call(cbind, rf$inbag.counts) > 0
somme <- t(apply(ifelse(dedans, 0, P), 1, cumsum))
compte <- t(apply(!dedans, 1, cumsum))
mse <- vapply(seq_len(500), function(k) { ok <- compte[, k] > 0; mean((yd[ok] - somme[ok, k] / compte[ok, k])^2) }, 0)
verifier(sprintf("erreur hors sac après 1 à 500 arbres : à la main (%.3f après 10 arbres), %.4f au 500e = celle de ranger", mse[10], mse[500]),
         proche(m$courbe$erreur, mse, 1e-10) && proche(m$courbe$erreur[500], rf$prediction.error, 1e-10))
vcm <- rep(NA_real_, length(yd))
for (s in unique(sd)) {
  k <- sd == s
  vcm[k] <- pred(ref(Xd[!k, ], yd[!k]), Xd[k, ])
}
verifier(sprintf("validation croisée par site : chaque site prédit par ranger refait sans lui ; Q² = %.4f, RMSEcv = %.4f", m$vc$Q2, m$vc$RMSEcv),
         proche(m$vc$prediction, vcm, 1e-12) && proche(m$vc$Q2, 1 - sum((yd - vcm)^2) / sum((yd - mean(yd))^2)) && proche(m$vc$RMSEcv, sqrt(mean((yd - vcm)^2))))
d0 <- ranger::ranger(x = Xd, y = yd, num.trees = 500, respect.unordered.factors = "order", seed = 1, num.threads = 1, verbose = FALSE)
verifier("réglages par défaut : ceux de ranger (mtry = racine du nombre de variables arrondie en dessous, 5 par nœud en régression)",
         proche(d0$predictions, m$oob$prediction, 1e-12) && d0$mtry == m$reglages_foret$mtry_utilise && d0$min.node.size == m$reglages_foret$min_noeud_utilise)

garder("F1", d, m)
dep <- demande_analyse_dependance_partielle(list(modele = "F1", variables = c("h_gel", "sol")))$dependance
grille <- seq(min(Xd$h_gel), max(Xd$h_gel), length.out = 25)
pd <- vapply(grille, function(v) { z <- Xd; z$h_gel <- v; mean(pred(rf, z)) }, 0)
pds <- vapply(levels(Xd$sol), function(v) { z <- Xd; z$sol <- factor(v, levels = levels(Xd$sol)); mean(pred(rf, z)) }, 0)
verifier(sprintf("dépendance partielle (Friedman, 2001) refaite à la main : gel sur une grille de 25 valeurs (%.3f à %.3f), le sol par catégorie",
                 min(pd), max(pd)),
         length(dep) == 2 && identical(dep[[1]]$variable, "h_gel") && proche(dep[[1]]$x, grille) && proche(dep[[1]]$y, pd, 1e-12) &&
           identical(as.character(dep[[2]]$x), levels(Xd$sol)) && proche(dep[[2]]$y, pds, 1e-12) && isTRUE(dep[[2]]$qualitative) &&
           identical(as.numeric(dep[[2]]$tapis), as.numeric(table(Xd$sol))) && identical(as.numeric(dep[[1]]$tapis), as.numeric(sort(unique(Xd$h_gel)))))
dep2 <- demande_analyse_dependance_partielle(list(modele = "F1"))$dependance
verifier("dépendance partielle de toutes les variables, gardée avec le modèle (celles déjà calculées reprises), dans l'ordre des X",
         identical(vapply(dep2, `[[`, "", "variable"), names(X)) && identical(dep2[[1]], dep[[1]]) && length(etat$modeles$F1$modele$dependance) == K &&
           grepl("inconnue", refus(demande_analyse_dependance_partielle(list(modele = "F1", variables = "vent")))))

cat("Forêt de classement\n")
dc <- definir_modele(p, "FOREST", x = xs, classe = u("classe"), echelle = "none", vc = vc)
mc <- ajuster(p, dc)
cl <- factor(classe[cc])
rc <- ref(Xd, cl, importance = "permutation", keep.inbag = TRUE, min.node.size = 1)
tab <- unclass(rc$confusion.matrix)[levels(cl), levels(cl)]
verifier(sprintf("hors sac : les classes de ranger, %.1f %% mal classées, son tableau de classement ; 1 par nœud par défaut", 100 * mc$oob$erreur),
         identical(mc$classification$oob$predite, as.character(rc$predictions)) && proche(mc$oob$erreur, rc$prediction.error, 1e-12) &&
           proche(mc$classification$oob$table, tab) && mc$reglages_foret$min_noeud_utilise == 1 &&
           proche(mc$importance$permutation[match(names(X), mc$importance$variable)], rc$variable.importance, 1e-12))
Pc <- pred(rc, Xd, predict.all = TRUE)
hors <- do.call(cbind, rc$inbag.counts) == 0
votes <- vapply(seq_along(levels(cl)), function(j) rowSums(Pc == j & hors) / rowSums(hors), numeric(nrow(Xd)))
verifier("parts des votes hors sac de chaque classe (les arbres dont le tirage ne contient pas l'observation), à la main", proche(mc$votes_oob, votes, 1e-12))
gagnante <- levels(cl)[max.col(votes, ties.method = "first")]
egalites <- rowSums(votes == apply(votes, 1, max)) > 1
verifier(sprintf("erreur hors sac après 500 arbres : %.4f à la main (la première classe à égalité de votes) ; hors des %d égalités, les classes de ranger",
                 mc$courbe$erreur[500], sum(egalites)),
         proche(mc$courbe$erreur[500], mean(gagnante != as.character(cl))) && identical(as.character(rc$predictions)[!egalites], gagnante[!egalites]))
vcc <- rep(NA_character_, length(cl))
for (s in unique(sd)) {
  k <- sd == s
  vcc[k] <- as.character(pred(ref(Xd[!k, ], droplevels(cl[!k]), min.node.size = 1), Xd[k, ]))
}
verifier(sprintf("validation croisée par site : %.1f %% bien classées, les classes de ranger refait sans chaque site", 100 * mc$classification$vc$global),
         identical(mc$classification$vc$predite, vcc) && proche(mc$classification$vc$global, mean(vcc == as.character(cl))))
garder("F2", dc, mc)
depc <- demande_analyse_dependance_partielle(list(modele = "F2", variables = "haie"))$dependance[[1]]
g2 <- seq(min(Xd$haie), max(Xd$haie), length.out = 25)
pv <- t(vapply(g2[c(1, 13, 25)], function(v) { z <- Xd; z$haie <- v; Q <- pred(rc, z, predict.all = TRUE); vapply(1:3, function(j) mean(rowMeans(Q == j)), 0) }, numeric(3)))
verifier("dépendance partielle d'une classe : la part moyenne de ses votes, à la main", proche(as.matrix(depc$probabilites)[c(1, 13, 25), ], pv, 1e-12))

cat("Réglages, jeu de test, prédiction\n")
dr <- definir_modele(p, "FOREST", x = xs, y = u("densite"), echelle = "none", vc = vc,
                     options = list(arbres = 300, mtry = 3, min_noeud = 10, fraction = 0.632, importance = "echelle"))
mr <- ajuster(p, dr)
rr <- ranger::ranger(x = Xd, y = yd, num.trees = 300, mtry = 3, min.node.size = 10, sample.fraction = 0.632, replace = TRUE, importance = "permutation",
                     scale.permutation.importance = TRUE, respect.unordered.factors = "order", seed = 1, num.threads = 1, verbose = FALSE)
verifier("300 arbres, mtry = 3, 10 par nœud, tirages de 63,2 %, importance divisée par son erreur-type : comme ranger",
         proche(mr$oob$prediction, rr$predictions, 1e-12) && proche(mr$importance$permutation[match(names(X), mr$importance$variable)], rr$variable.importance, 1e-12) &&
           nrow(mr$courbe) == 300)
dt <- definir_modele(p, "FOREST", x = xs, y = u("densite"), echelle = "none", vc = vc, test = 221:240)
mt <- ajuster(p, dt)
app <- cc & !(seq_len(n) %in% 221:240)
Xa <- X[app, ]
Xa$sol <- factor(Xa$sol, levels = levels(Xd$sol))
rt <- ref(Xa, y[app])
Xt <- X[221:240, ]
Xt$sol <- factor(Xt$sol, levels = levels(Xd$sol))
ct <- stats::complete.cases(Xt)
attendu <- rep(NA_real_, 20)
attendu[ct] <- pred(rt, Xt[ct, ])
verifier(sprintf("jeu de test (20 observations hors de la forêt, %d complètes) : prédit par ranger ajusté sans lui ; RMSEP %.3f", sum(ct), mt$test$statistiques$RMSEP),
         mt$N == sum(app) && mt$test$n == 20 && mt$test$incompletes == sum(!ct) && proche(mt$test$observations[["densite prédit"]], attendu, 1e-12))
pn <- predire(p, m, d, as.numeric(rownames(p$donnees))[1:6])
attendu <- rep(NA_real_, 6)
attendu[cc[1:6]] <- pred(rf, Xd[seq_len(sum(cc[1:6])), ])
verifier("prédire des observations : la prédiction de ranger, sans prédiction où une valeur X manque",
         proche(pn[["densite prédit"]], attendu, 1e-12) && proche(pn[["densite observé"]], y[1:6]))
nouvelles <- data.frame(h_gel = c(5, 20, 8), t_moy = c(11, 13, 12), pluie = c(100, 50, NA), haie = c(0.9, 0.2, 0.5), sol = factor(c("sable", "argile", "limon"), levels = levels(Xd$sol)))
pf <- predire(list(donnees = matrix(c(nouvelles$h_gel, nouvelles$t_moy, nouvelles$pluie, nouvelles$haie, as.integer(nouvelles$sol)), 3,
                                    dimnames = list(1:3, as.character(xs)))), m, d, 1:3)
verifier("prédire un fichier (un projet sans variables) : comme ranger, une ligne incomplète sans prédiction",
         proche(pf[["densite prédit"]][1:2], pred(rf, nouvelles[1:2, ]), 1e-12) && is.na(pf[["densite prédit"]][3]))
pc <- predire(p, mc, dc, as.numeric(rownames(p$donnees))[which(cc)[1:4]])
verifier("prédire avec une forêt de classement : la classe de ranger et la part des votes de chaque classe",
         identical(pc[["classe prédite"]], as.character(pred(rc, Xd[1:4, ]))) &&
           proche(as.matrix(pc[paste("probabilité", levels(cl))]), vapply(1:3, function(j) rowMeans(pred(rc, Xd[1:4, ], predict.all = TRUE) == j), numeric(4)), 1e-12))
# (un seul site parmi les observations complètes : l'autre n'a qu'une observation, incomplète)
j <- which(!cc)[1]
s2 <- Find(function(s) s != site[j] && sum(cc & site == s) >= 5, unique(site))
un_site <- c(which(site == s2 & cc), j)
verifier("refus : deux réponses, aucune, une Y qualitative, des termes croisés, mtry trop grand, un seul groupe parmi les observations complètes, trop peu d'observations",
         grepl("pas les deux", refus(ajuster(p, definir_modele(p, "FOREST", x = xs, y = u("densite"), classe = u("classe"), echelle = "none", vc = vc)))) &&
           grepl("demande une réponse", refus(ajuster(p, definir_modele(p, "FOREST", x = xs, echelle = "none", vc = vc)))) &&
           grepl("qualitative", refus(ajuster(p, definir_modele(p, "FOREST", x = u("haie"), y = u("sol"), echelle = "none", vc = vc)))) &&
           grepl("croisés", refus(ajuster(p, definir_modele(p, "FOREST", x = u("haie", "pluie"), y = u("densite"), echelle = "none", vc = vc, croises = list(u("haie", "pluie")))))) &&
           grepl("mtry", refus(ajuster(p, definir_modele(p, "FOREST", x = xs, y = u("densite"), echelle = "none", vc = vc, options = list(mtry = 9))))) &&
           grepl("observations complètes", refus(ajuster(p, definir_modele(p, "FOREST", x = xs, y = u("densite"), echelle = "none", vc = vc, observations = un_site)))) &&
           grepl("Trop peu", refus(ajuster(p, definir_modele(p, "FOREST", x = xs, y = u("densite"), echelle = "none", observations = 1:4)))))

cat("Ce que l'interface reçoit, texte des méthodes, tableaux\n")
garder("T1", dt, mt)
for (id in c("F1", "F2", "T1")) {
  r <- resultat(id)
  verifier(sprintf("%s : analyse qui prédit, chiffres « %s », vues %s, sérialisable", id, r$chiffres, paste(r$vues_defaut, collapse = ", ")),
           isTRUE(r$analyse) && isTRUE(r$prediction) && identical(r$type, "FOREST") && nzchar(r$chiffres) && length(r$vues_defaut) == 2 &&
             inherits(json(r), "json") && identical(names(r$observations)[1:2], c("uid", "nom")) && nrow(r$courbe) == 500 && nrow(r$importance) == K)
}
r <- resultat("F1")
verifier("forêt de régression : observée, hors sac, en validation croisée, ajustée ; Q² et RMSEcv pour Comparer les modèles ; ses dépendances partielles",
         all(c("densite observé", "densite prédit (OOB)", "densite prédit (VC)", "densite ajusté") %in% names(r$observations)) &&
           proche(r$comparaison$Q2, m$vc$Q2) && proche(r$comparaison$R2, m$oob$r2) && length(r$dependance) == K)
r <- resultat("F2")
verifier("forêt de classement : classes hors sac, en validation croisée, ajustées, part des votes ; tableaux et courbes ROC",
         all(c("classe", "classe prédite (OOB)", "classe prédite (VC)", "probabilité haute") %in% names(r$observations)) &&
           all(c("oob", "vc", "ajuste") %in% names(r$classification)) && length(r$classification$oob$roc) == 3 && is.null(r$comparaison))
for (langue in c("fr", "en")) {
  tt <- texte_methodes(p, d, etat$modeles$F1$modele, langue, "essai")
  tc <- texte_methodes(p, dr, mr, langue, "essai")
  verifier(sprintf("texte des méthodes (%s) : ranger, hors sac, validation par site, importance, dépendance partielle ; Breiman, Wright, Friedman", langue),
           any(grepl("ranger", tt$paragraphes)) && any(grepl(if (langue == "fr") "hors sac" else "out-of-bag", tt$paragraphes)) &&
             any(grepl("30 (groupes|groups)", tt$paragraphes)) && any(grepl(if (langue == "fr") "dépendance partielle" else "partial dependence", tt$paragraphes)) &&
             any(grepl("^Breiman, L. \\(2001\\)", tt$references)) && any(grepl("^Wright, M. N., & Ziegler", tt$references)) &&
             any(grepl("^Friedman, J. H. \\(2001\\)", tt$references)) && any(grepl("^Wright, M. N., & König", tt$references)) &&
             any(grepl(if (langue == "fr") "erreur-type" else "standard error", tc$paragraphes)) && !any(grepl("Friedman", tc$references)) &&
             any(grepl("12 observations|24 observations", tt$paragraphes)))
}
tf <- tables_resultats(p, etat$modeles$F1$modele)
tc <- tables_resultats(p, mc)
verifier("tableaux exportés : des data.frame (résumé, observations, importance, erreur hors sac, dépendance partielle ; tableaux de classement)",
         all(vapply(c(tf, tc), is.data.frame, TRUE)) && all(c("resume", "observations", "importance", "erreur_oob", "dependance") %in% names(tf)) &&
           all(c("classement_oob", "classement_vc") %in% names(tc)) && nrow(tf$dependance) == 25 + 25 + 25 + 25 + 3)

# ---------------------------------------------------------------- l'export vers R

# Chaque script est écrit (ecrire_script : exécuté d'abord dans le moteur et comparé, `ecart`), puis
# lancé dans une session R neuve (Rscript --vanilla, sans le moteur, ranger trouvé dans les
# bibliothèques de celle-ci) ; ses objets en reviennent et ecart_foret les compare au modèle du moteur :
# les prédictions et l'erreur hors sac, les importances, l'erreur selon le nombre d'arbres, la
# validation croisée, la qualité, les votes et les classements, le jeu de test et la dépendance
# partielle, à 1e-8 près : la même forêt.
cat("Export vers R : des scripts lancés seuls (avec ranger), en français et en anglais, et un rapport R Markdown\n")
dossier <- tempfile("foret_script")
dir.create(dossier)
rscript <- file.path(R.home("bin"), "Rscript")
lanceur <- file.path(dossier, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".r <- mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL))", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, langue) {
  objets <- nom_traduit(c("rf", "importance", "courbe", "vc", "ajuste", "qualite", "votes_oob", "classement", "test", "dependance"), langue, NOMS_FORET_EN)
  sortie <- tempfile("sortie", dossier, ".rds")
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ","))),
                                       stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
hors_donnees <- function(texte) texte[-(grep("^# 2\\. ", texte):(grep("^# 3\\. ", texte) - 1))]
paquets_appeles <- function(texte) unique(regmatches(texte, regexpr("[A-Za-z.]+(?=::)", texte, perl = TRUE)))
essayer_script <- function(nom, d, langue, dependance = character(0), ext = "R", ...) {
  m <- ajuster(p, d)
  if (length(dependance)) {
    garder(nom, d, m)
    demande_analyse_dependance_partielle(list(modele = nom, variables = dependance))
    m <- etat$modeles[[nom]]$modele
  }
  r <- ecrire_script(p, d, file.path(dossier, paste0(nom, ".", ext)), "moteur", m = m, version = "essai", langue = langue, ...)
  if (ext != "R") return(invisible(list(r = r, m = m)))
  s <- lancer(r$script, langue)
  texte <- readLines(r$script, encoding = "UTF-8")
  code <- sub("#.*$", "", hors_donnees(texte))
  difference <- if (length(s)) ecart_foret(m, list2env(s), langue) else paste(utils::tail(attr(s, "messages"), 3), collapse = " / ")
  francais <- langue == "fr" || !any(grepl("foret_ranger|predire_ranger|votes_classes|reglages", code))
  collisions <- if (langue == "fr") collisions_traduction(hors_donnees(texte), c(NOMS_FORET_EN, NOMS_SCRIPT_EN)) else list()
  verifier(sprintf("%s (%s) : sans écart vu par le moteur ; lancé seul, la même forêt et les mêmes résultats ; R et ranger seuls (ni library() ni autre paquet), sans traduire()%s",
                   nom, langue, if (langue == "en") ", ses noms en anglais" else ", aucun nom confondu en anglais"),
           is.null(r$ecart) && is.null(difference) && !any(grepl("library\\(|traduire\\(", code)) &&
             all(paquets_appeles(code) %in% c("stats", "utils", "ranger")) && francais && !length(collisions) &&
             any(grepl("install.packages(\"ranger\")", texte, fixed = TRUE)))
  if (!is.null(r$ecart) || !is.null(difference) || length(collisions)) cat("    écart :", r$ecart, "/", difference, "/", names(collisions), "\n")
  invisible(list(r = r, m = m, s = s))
}
df1 <- definir_modele(p, "FOREST", x = xs, y = u("densite"), echelle = "none", vc = vc, options = list(arbres = 200), test = 221:240)
x <- essayer_script("foret_regression_test", df1, "fr", dependance = c("h_gel", "sol"))
hors_test <- !(which(cc) %in% 221:240)
r200 <- ranger::ranger(x = Xd[hors_test, ], y = yd[hors_test], num.trees = 200, mtry = floor(sqrt(K)), min.node.size = 5, replace = TRUE, respect.unordered.factors = "order",
                       seed = 1, num.threads = 1, verbose = FALSE)
verifier("forêt de régression : les prédictions hors sac du script, celles de ranger appelé ici directement",
         length(x$s) > 0 && proche(x$s$rf$predictions, r200$predictions, 1e-12))
essayer_script("foret_regression_test_en", df1, "en", dependance = "haie")
df2 <- definir_modele(p, "FOREST", x = xs, classe = u("classe"), echelle = "none", vc = vc, options = list(arbres = 150, mtry = 3, min_noeud = 2, importance = "echelle"),
                      test = 221:240)
essayer_script("foret_classement_test", df2, "fr", dependance = "haie")
x <- essayer_script("foret_classement_test_en", df2, "en")
verifier("forêt de classement : les parts des votes hors sac et le tableau de classement en validation croisée du script",
         length(x$s) > 0 && proche(x$s$oob_votes, x$m$votes_oob, 1e-12) && identical(unname(x$s$classification$vc$table), unname(x$m$classification$vc$table)))
dfc <- definir_modele(p, "FOREST", x = u("h_gel", "haie", "sol"), y = u("densite"), echelle = "none", vc = vc, options = list(arbres = 100, fraction = 0.632),
                      condition = "annee >= 2017 & sol != \"argile\"")
essayer_script("foret_condition", dfc, "fr")
x <- essayer_script("rapport_foret", df2, "en", dependance = "sol", ext = "Rmd", donnees = "csv", rdata = TRUE)
texte <- readLines(x$r$script, encoding = "UTF-8")
verifier("R Markdown report: methods, code, results, references; data in a CSV and .RData next to it",
         is.null(x$r$ecart) && texte[1] == "---" && any(grepl("^## Methods", texte)) && any(grepl("^```\\{r modele\\}", texte)) && any(grepl("^```\\{r importance-table\\}", texte)) &&
           any(grepl("^- Breiman", texte)) && length(x$r$chemins) == 3 && all(file.exists(x$r$chemins)) && !anyDuplicated(sub("^```\\{r ([^,} ]+).*$", "\\1", grep("^```\\{r [^,} ]", texte, value = TRUE))))
if (requireNamespace("knitr", quietly = TRUE)) {
  compiler <- file.path(dossier, "compiler.R")
  writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "a <- commandArgs(TRUE)", "setwd(dirname(a[1]))",
               "knitr::knit(basename(a[1]), output = sub('[.][Rq]md$', '.md', basename(a[1])), quiet = TRUE, envir = new.env())"), compiler)
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(compiler), shQuote(x$r$script)), stdout = TRUE, stderr = TRUE))
  md <- sub("[.]Rmd$", ".md", x$r$script)
  verifier("le rapport se compile avec knitr, avec ses graphiques", file.exists(md) && !any(grepl("^## Error|Error in", readLines(md, encoding = "UTF-8"))) &&
             length(list.files(file.path(dossier, "figure"), "\\.png$")) >= 3)
} else cat("  (knitr absent : le rapport n'est pas compilé)\n")
unlink(dossier, recursive = TRUE)

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
