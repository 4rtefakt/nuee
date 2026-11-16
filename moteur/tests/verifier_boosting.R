# Vérifie les arbres boostés (moteur/R/boosting.R) : contre le boosting aux moindres carrés écrit
# ici à la main sur rpart (des souches, puis des arbres de deux coupures choisies la meilleure
# d'abord, avec et sans tirage) ; contre gbm (NUEE_GBM=<bibliothèque> où il est installé ; sans lui,
# ces comparaisons sont sautées), pour les lois normale, binomiale et Poisson, sans tirage et avec
# le tirage de la moitié des observations (la même graine donne les mêmes tirages) : la valeur
# initiale, f, la déviance à chaque arbre, l'influence relative, la déviance de la validation
# croisée par secteur (gbm refait sans chaque secteur) et le nombre d'arbres choisi ; la valeur des
# feuilles (Newton–Raphson pour la binomiale, log(Σy / Σμ) pour Poisson) à la main ; la forme courte
# des arbres contre predict.rpart ; puis la dépendance partielle, le jeu de test, la prédiction, les
# refus, ce que l'interface reçoit, le texte des méthodes et les tableaux exportés.
#   Rscript moteur/tests/verifier_boosting.R

if (nzchar(Sys.getenv("NUEE_GBM"))) .libPaths(c(.libPaths(), Sys.getenv("NUEE_GBM")))
avec_gbm <- requireNamespace("gbm", quietly = TRUE)
source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-10) isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = tol, check.attributes = FALSE))
ecart <- function(a, b) max(abs(as.numeric(a) - as.numeric(b)))
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

etat <- new.env()
etat$modeles <- list()
projet <- function(id) etat$projets[[id]]
modele <- function(id) etat$modeles[[id]]
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE)
garder <- function(id, d, m) { etat$modeles[[id]] <- list(projet = "P1", definition = d, modele = m); invisible(id) }
resultat <- function(id) { force(id); fonction_analyse("resultat_", etat$modeles[[id]]$definition$type)(id) }

# ---------------------------------------------------------------- les données
# 30 sites (6 secteurs de 5 sites) suivis 8 ans : une densité, une présence (0/1) et des comptages
# qui dépendent du gel, des haies (avec une interaction), du sol ; des X manquants.
set.seed(7)
n <- 240
site <- rep(sprintf("S%02d", 1:30), each = 8)
secteur <- rep(sprintf("Z%d", 1:6), each = 40)
X <- data.frame(h_gel = round(stats::runif(n, 0, 30)), t_moy = stats::rnorm(n, 12, 2), haie = stats::runif(n), pluie = stats::rgamma(n, 4, 0.02),
                sol = factor(sample(c("argile", "limon", "sable", "tourbe"), n, TRUE)), stringsAsFactors = FALSE)
eta <- 0.5 + ifelse(X$h_gel > 12, -0.8, 0) + 0.8 * X$haie * (X$t_moy > 12) + 0.4 * (X$sol %in% c("limon", "tourbe"))
densite <- eta + stats::rnorm(n, 0, 0.4)
presence <- stats::rbinom(n, 1, stats::plogis(2 * eta - 0.5))
comptage <- stats::rpois(n, exp(eta + 0.5))
X$t_moy[sample(n, 10)] <- NA
X$pluie[sample(n, 10)] <- NA
df <- data.frame(nom = paste(site, rep(2015:2022, 30)), site = site, secteur = secteur, X, densite = densite, presence = presence, comptage = comptage,
                 classe = ifelse(densite > 0.5, "haute", "basse"), stringsAsFactors = FALSE)
p <- projet_depuis_tableau(df, "essai")
etat$projets <- list(P1 = c(p, list(id = "P1")))
class(etat$projets$P1) <- class(p)
u <- function(...) numeros(p, c(...))
vc <- list(by = "variable", variable = u("secteur"))
xs <- u(names(X))
cc <- stats::complete.cases(X)
Xd <- X[cc, ]
Xd$sol <- factor(Xd$sol, levels = sort(unique(as.character(X$sol))))
Xi <- stats::setNames(Xd, paste0("x", seq_along(Xd)))  # (les noms simples du moteur)
sect <- secteur[cc]
Y <- list(gaussienne = densite[cc], binomiale = presence[cc], poisson = comptage[cc])
N <- sum(cc)
graine <- function() set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

cat("Boosting aux moindres carrés écrit à la main (rpart)\n")
# le tirage de gbm : l'observation i prise si u (n - i + 1) < k - (celles déjà prises)
sac <- function(n, k) { u <- stats::runif(n); pris <- 0; d <- logical(n); for (i in 1:n) if (u[i] * (n - i + 1) < k - pris) { d[i] <- TRUE; pris <- pris + 1 }; which(d) }
souche <- function(z, x, m) rpart::rpart(.z ~ ., data.frame(.z = z, x), method = "anova",
                                         control = rpart::rpart.control(maxdepth = 1, minbucket = m, minsplit = 2 * m, cp = 0, xval = 0, maxcompete = 0, maxsurrogate = 0))
cote <- function(a, x) { b <- a; b$frame$yval <- as.numeric(rownames(a$frame)); stats::predict(b, x) }  # (2 : à gauche, 3 : à droite)
gain <- function(a) if (nrow(a$frame) == 1) 0 else a$splits[1, "improve"] * a$frame$dev[1]
# deux coupures, la meilleure d'abord : la racine, puis le fils où une coupure réduit le plus l'erreur
deux_coupures <- function(z, x, m) {
  a <- souche(z, x, m)
  if (nrow(a$frame) == 1) return(function(xn) rep(mean(z), nrow(xn)))
  g <- cote(a, x) == 2
  fils <- list(souche(z[g], x[g, , drop = FALSE], m), souche(z[!g], x[!g, , drop = FALSE], m))
  k <- if (gain(fils[[2]]) > gain(fils[[1]])) 2 else 1
  dans <- if (k == 1) g else !g
  b <- fils[[k]]
  if (nrow(b$frame) == 1) return(function(xn) ifelse(cote(a, xn) == 2, mean(z[g]), mean(z[!g])))
  h <- cote(b, x[dans, , drop = FALSE]) == 2
  moy <- c(mean(z[dans][h]), mean(z[dans][!h]), mean(z[!dans]))
  function(xn) {
    ga <- cote(a, xn) == 2
    ici <- if (k == 1) ga else !ga
    out <- rep(moy[3], nrow(xn))
    if (any(ici)) out[ici] <- ifelse(cote(b, xn[ici, , drop = FALSE]) == 2, moy[1], moy[2])
    out
  }
}
a_la_main <- function(y, x, arbres, profondeur, m, taux, fraction) {
  f <- rep(mean(y), length(y))
  dev <- numeric(arbres)
  graine()
  for (t in 1:arbres) {
    z <- y - f
    d <- sac(length(y), floor(fraction * length(y)))
    h <- if (profondeur == 1) { a <- souche(z[d], x[d, , drop = FALSE], m); function(xn) stats::predict(a, xn) } else deux_coupures(z[d], x[d, , drop = FALSE], m)
    f <- f + taux * h(x)
    dev[t] <- mean((y - f)^2)
  }
  list(f = f, deviance = dev)
}
for (cas in list(list(profondeur = 1, fraction = 1), list(profondeur = 1, fraction = 0.5), list(profondeur = 2, fraction = 1), list(profondeur = 2, fraction = 0.5))) {
  r <- list(loi = "gaussienne", arbres = 60, profondeur = cas$profondeur, min_noeud = 10, taux = 0.1, fraction = cas$fraction)
  b <- boosting(Xi, Y$gaussienne, seq_len(N), integer(0), r, arret = 60)
  h <- a_la_main(Y$gaussienne, Xi, 60, cas$profondeur, 10, 0.1, cas$fraction)
  verifier(sprintf("%s, tirage de %s : f et la déviance de chaque arbre (écart %.1e)", if (cas$profondeur == 1) "souches" else "deux coupures, la meilleure d'abord",
                   pourcent_texte(cas$fraction), ecart(b$f_arret, h$f)),
           proche(b$f_arret, h$f, 1e-10) && proche(b$deviance, h$deviance, 1e-10))
}

cat("Une étape à la main : la valeur des feuilles de chaque loi (gbm : FitBestConstant)\n")
for (loi in c("binomiale", "poisson")) {
  y <- Y[[loi]]
  r <- list(loi = loi, arbres = 1, profondeur = 1, min_noeud = 10, taux = 1, fraction = 1)
  b <- boosting(Xi, y, seq_len(N), integer(0), r, arret = 1)
  f0 <- if (loi == "binomiale") log(mean(y) / (1 - mean(y))) else log(mean(y))
  z <- if (loi == "binomiale") y - stats::plogis(f0) else y - exp(f0)
  g <- cote(souche(z, Xi, 10), Xi) == 2
  v <- if (loi == "binomiale") vapply(list(g, !g), function(k) sum(z[k]) / sum(stats::plogis(f0) * (1 - stats::plogis(f0)) * k[k]), 0)
       else vapply(list(g, !g), function(k) log(sum(y[k]) / sum(exp(f0) * k[k])), 0)
  verifier(sprintf("%s : f0 = %.4f, chaque feuille %s", loi, f0, if (loi == "binomiale") "Σz / Σp(1 - p) (un pas de Newton–Raphson)" else "log(Σy / Σμ)"),
           proche(b$f0, f0) && proche(b$f_arret, f0 + ifelse(g, v[1], v[2])))
}

cat("La forme courte des arbres : les feuilles de predict.rpart, coupures numériques et qualitatives\n")
ok <- TRUE
for (t in 1:20) {
  graine()
  set.seed(t)
  z <- stats::rnorm(N) + (Xi$x5 %in% c("limon", "tourbe")) + Xi$x1 / 10
  d <- sort(sample(N, 120))
  mt <- data.frame(.z = z[d], Xi[d, ])
  a <- rpart::rpart(.z ~ ., mt, method = "anova", control = rpart::rpart.control(maxdepth = 3, minbucket = 5, minsplit = 10, cp = 0, xval = 0, maxcompete = 0, maxsurrogate = 0))
  cb <- meilleures_coupures(a, 3)
  arb <- arbre_court(a, cb$noeuds, names(Xi))
  tous <- as.numeric(rownames(a$frame))
  enlever <- setdiff(tous[a$frame$var != "<leaf>"], cb$noeuds)
  enlever <- enlever[floor(enlever / 2) %in% cb$noeuds | enlever == 1]
  s <- if (length(enlever)) rpart::snip.rpart(a, toss = enlever) else a
  ok <- ok && identical(as.numeric(feuilles_arbre(arb, matrice_codes(Xi))), as.numeric(cote(s, Xi)))
}
verifier("20 arbres de trois coupures (snip.rpart des autres, puis predict.rpart sur toutes les observations) : les mêmes feuilles", ok)

cat("Les arbres de la fonction C de rpart (arbre_rpart) : ceux de rpart::rpart, à l'identique\n")
ok <- TRUE
ncat <- vapply(Xi, function(x) if (is.factor(x)) length(levels(x)) else 0L, 0L, USE.NAMES = FALSE)
Xm <- matrice_codes(Xi)
c_rpart <- utils::getFromNamespace("C_rpart", "rpart")
for (t in 1:30) {
  set.seed(t)
  # (une réponse constante, la dernière fois : un arbre sans coupure)
  z <- if (t == 30) rep(1, N) else stats::rnorm(N) + (Xi$x5 %in% c("limon", "tourbe")) + Xi$x1 / 10
  d <- sort(sample(N, min(N, 60 + 5 * t)))
  for (profondeur in 1:4) {
    ctl <- rpart::rpart.control(maxdepth = profondeur, minbucket = 2 + t %% 7, minsplit = 2 * (2 + t %% 7), cp = 0, xval = 0, maxcompete = 0, maxsurrogate = 0, usesurrogate = 0)
    a <- rpart::rpart(.z ~ ., data.frame(.z = z[d], Xi[d, ]), method = "anova", control = ctl)
    b <- arbre_rpart(Xm[d, , drop = FALSE], z[d], ncat, as.double(unlist(ctl)), c_rpart)
    ok <- ok && identical(parties_arbre(a), parties_arbre(b))
  }
}
verifier("120 arbres de 1 à 4 coupures, des X quantitatifs et qualitatifs, un sans coupure : nœuds, coupures et csplit identiques", ok)

if (avec_gbm) {
  cat("Contre gbm : même graine, mêmes réglages\n")
  suppressMessages(requireNamespace("gbm"))
  dist <- c(gaussienne = "gaussian", binomiale = "bernoulli", poisson = "poisson")
  dev_gbm <- function(y, f, loi) switch(loi, gaussienne = (y - f)^2, binomiale = -2 * (y * f - log(1 + exp(f))), poisson = -2 * (y * f - exp(f)))
  for (loi in names(Y)) {
    y <- Y[[loi]]
    constante <- if (loi == "poisson") 2 * mean(ifelse(y > 0, y * log(y), 0) - y) else 0  # (gbm retire le terme de la déviance de Poisson qui ne dépend pas du modèle)
    # sans tirage, puis avec la moitié : le boosting du moteur, et celui de gbm.fit
    for (fraction in c(1, 0.5)) {
      r <- list(loi = loi, arbres = 300, profondeur = 3, min_noeud = 10, taux = 0.05, fraction = fraction)
      b <- boosting(Xi, y, seq_len(N), integer(0), r, garder = 300, arret = 300)
      graine()
      g <- gbm::gbm.fit(Xd, y, distribution = dist[[loi]], n.trees = 300, interaction.depth = 3, n.minobsinnode = 10, shrinkage = 0.05, bag.fraction = fraction,
                        keep.data = FALSE, verbose = FALSE)
      ri <- colSums(b$gains)
      rg <- gbm::summary.gbm(g, n.trees = 300, plotit = FALSE, order = FALSE)
      verifier(sprintf("%s, tirage de %s : initF, f et la déviance de chacun des 300 arbres, influence relative de gbm (écarts %.1e, %.1e, %.1e)",
                       loi, pourcent_texte(fraction), ecart(b$f_arret, gbm::predict.gbm(g, Xd, n.trees = 300)), ecart(b$deviance - constante, g$train.error),
                       ecart(100 * ri / sum(ri), rg$rel.inf[match(names(Xd), rg$var)])),
               proche(b$f0, g$initF, 1e-12) && proche(b$f_arret, gbm::predict.gbm(g, Xd, n.trees = 300), 1e-10) &&
                 proche(b$deviance - constante, g$train.error, 1e-10) && proche(100 * ri / sum(ri), rg$rel.inf[match(names(Xd), rg$var)], 1e-10))
    }
    # le modèle du moteur (validation croisée par secteur) contre gbm refait sans chaque secteur
    d <- definir_modele(p, "BOOST", x = xs, y = u(c(gaussienne = "densite", binomiale = "presence", poisson = "comptage")[[loi]]), echelle = "none", vc = vc,
                        options = list(loi = loi, arbres_max = 300, profondeur = 3, min_feuille = 10, taux = 0.05, part = 0.5))
    m <- ajuster(p, d)
    chemin <- matrix(NA_real_, N, 300)
    for (s in unique(sect)) {
      k <- sect == s
      graine()
      gs <- gbm::gbm.fit(Xd[!k, ], y[!k], distribution = dist[[loi]], n.trees = 300, interaction.depth = 3, n.minobsinnode = 10, shrinkage = 0.05, bag.fraction = 0.5,
                         keep.data = FALSE, verbose = FALSE)
      chemin[k, ] <- gbm::predict.gbm(gs, Xd[k, ], n.trees = 1:300)
    }
    cv <- colMeans(dev_gbm(y, chemin, loi))
    graine()
    g <- gbm::gbm.fit(Xd, y, distribution = dist[[loi]], n.trees = 300, interaction.depth = 3, n.minobsinnode = 10, shrinkage = 0.05, bag.fraction = 0.5,
                      keep.data = FALSE, verbose = FALSE)
    meilleur <- which.min(cv)
    rg <- gbm::summary.gbm(g, n.trees = meilleur, plotit = FALSE, order = FALSE)
    verifier(sprintf("%s : la déviance de validation croisée par secteur de gbm (%.4f au mieux), le même nombre d'arbres (%d) ; f, déviance et influence de gbm à ce nombre",
                     loi, min(cv), m$meilleur),
             proche(m$courbe$validation - constante, cv, 1e-10) && m$meilleur == meilleur && proche(m$ajuste_lien, gbm::predict.gbm(g, Xd, n.trees = meilleur), 1e-10) &&
               proche(m$courbe$apprentissage - constante, g$train.error, 1e-10) && proche(m$influence$pourcentage, rg$rel.inf[match(m$influence$variable, rg$var)], 1e-10))
  }
} else cat("  (gbm absent : les comparaisons avec gbm sont sautées ; NUEE_GBM=<bibliothèque> pour les faire)\n")

cat("Le modèle du moteur : validation croisée, prédictions, dépendance partielle\n")
avant <- .Random.seed
d <- definir_modele(p, "BOOST", x = xs, y = u("densite"), echelle = "none", vc = vc, options = list(arbres_max = 200, taux = 0.05), test = 221:240)
m <- ajuster(p, d)
verifier("le hasard de la séance est rendu tel quel (avec_graine)", identical(avant, .Random.seed))
app <- cc & !(seq_len(n) %in% 221:240)
r <- m$reglages_boost
chemin <- matrix(NA_real_, sum(app), 200)
Xa <- stats::setNames(X[app, ], names(Xi))
Xa$x5 <- factor(Xa$x5, levels = levels(Xd$sol))
sa <- secteur[app]
for (s in unique(sa)) chemin[sa == s, ] <- boosting(Xa, densite[app], which(sa != s), which(sa == s), r)$chemin
cv <- colMeans((densite[app] - chemin)^2)
verifier(sprintf("par défaut : loi normale, profondeur 3, 10 par feuille, tirage de 50 %% ; %d observations, %d secteurs ; le nombre d'arbres de la plus petite déviance de validation croisée (%d)",
                 m$N, m$G, m$meilleur),
         identical(r$loi, "gaussienne") && r$profondeur == 3 && r$min_noeud == 10 && r$fraction == 0.5 && m$N == sum(app) && m$G == 6 &&
           proche(m$courbe$validation, cv) && m$meilleur == which.min(cv) && proche(m$vc$prediction, chemin[, m$meilleur]) &&
           proche(m$qualite$Q2, 1 - sum((densite[app] - chemin[, m$meilleur])^2) / sum((densite[app] - mean(densite[app]))^2)) && length(m$arbres) == m$meilleur)
verifier("le modèle garde ses arbres jusqu'au meilleur : la prédiction de ses observations est f ajusté",
         proche(predire_boost_f(m, Xa), m$ajuste_lien, 1e-12) && proche(m$ajuste, m$ajuste_lien))
Xt <- stats::setNames(X[221:240, ], names(Xi))
Xt$x5 <- factor(Xt$x5, levels = levels(Xd$sol))
ct <- stats::complete.cases(Xt)
attendu <- rep(NA_real_, 20)
attendu[ct] <- predire_boost_f(m, Xt[ct, ])
verifier(sprintf("jeu de test (20 observations, %d complètes) : RMSEP %.3f, Q² externe %.3f", sum(ct), m$test$statistiques$RMSEP, m$test$statistiques$Q2ext),
         m$test$n == 20 && m$test$incompletes == sum(!ct) && proche(m$test$observations[["densite prédit"]], attendu, 1e-12) &&
           proche(m$test$statistiques$RMSEP, sqrt(mean((attendu - densite[221:240])^2, na.rm = TRUE))))
garder("B1", d, m)
dep <- demande_analyse_dependance_partielle(list(modele = "B1", variables = c("haie", "sol")))$dependance
g1 <- seq(min(Xa$x3), max(Xa$x3), length.out = 25)
pd <- vapply(g1, function(v) { z <- Xa; z$x3 <- v; mean(predire_boost_f(m, z)) }, 0)
pds <- vapply(levels(Xa$x5), function(v) { z <- Xa; z$x5 <- factor(v, levels = levels(Xa$x5)); mean(predire_boost_f(m, z)) }, 0)
verifier("dépendance partielle à la main : la moyenne de f sur les observations, la haie sur 25 valeurs, le sol par catégorie",
         proche(dep[[1]]$y, pd, 1e-12) && proche(dep[[1]]$x, g1) && proche(dep[[2]]$y, pds, 1e-12) && proche(dep[[2]]$reponse, pds))
db <- definir_modele(p, "BOOST", x = xs, y = u("presence"), echelle = "none", vc = vc, options = list(loi = "binomiale", arbres_max = 150, taux = 0.05))
mb <- ajuster(p, db)
garder("B2", db, mb)
depb <- demande_analyse_dependance_partielle(list(modele = "B2", variables = "h_gel"))$dependance[[1]]
verifier(sprintf("loi binomiale : probabilités ajustées et en validation croisée, AUC %.2f (VC %.2f), classement à 0,5 ; dépendance partielle à l'échelle du logit et de la probabilité",
                 mb$qualite$auc, mb$qualite$auc_vc),
         all(mb$ajuste > 0 & mb$ajuste < 1) && proche(mb$ajuste, stats::plogis(mb$ajuste_lien)) && proche(depb$reponse, stats::plogis(depb$y)) &&
           identical(mb$classification$vc$predite[!is.na(mb$vc$prediction)], ifelse(mb$vc$prediction >= 0.5, "1", "0")[!is.na(mb$vc$prediction)]) &&
           proche(mb$qualite$auc, roc(mb$ajuste, mb$y_valeurs == 1)$auc))
pn <- predire(p, mb, db, as.numeric(rownames(p$donnees))[1:6])
attendu <- rep(NA_real_, 6)
Xp <- stats::setNames(X[1:6, ], names(Xi))
Xp$x5 <- factor(Xp$x5, levels = levels(Xd$sol))
attendu[cc[1:6]] <- predire_boost_f(mb, Xp[cc[1:6], ])
verifier("prédire : la probabilité et le logit, sans prédiction où une valeur X manque",
         proche(pn[["presence prédit (lien)"]], attendu, 1e-12) && proche(pn[["presence prédit"]], stats::plogis(attendu), 1e-12) && proche(pn[["presence observé"]], presence[1:6]))
verifier("refus : une classe, une Y qualitative, une réponse binomiale qui n'est pas 0/1, des comptages qui ne sont pas entiers, trop peu d'observations par tirage, des termes croisés",
         grepl("pas une classe", refus(ajuster(p, definir_modele(p, "BOOST", x = xs, classe = u("classe"), echelle = "none", vc = vc)))) &&
           grepl("quantitative", refus(ajuster(p, definir_modele(p, "BOOST", x = u("haie"), y = u("sol"), echelle = "none", vc = vc)))) &&
           grepl("0 ou 1", refus(ajuster(p, definir_modele(p, "BOOST", x = xs, y = u("densite"), echelle = "none", vc = vc, options = list(loi = "binomiale"))))) &&
           grepl("comptages", refus(ajuster(p, definir_modele(p, "BOOST", x = xs, y = u("densite"), echelle = "none", vc = vc, options = list(loi = "poisson"))))) &&
           grepl("trop peu", refus(ajuster(p, definir_modele(p, "BOOST", x = xs, y = u("densite"), echelle = "none", vc = vc, options = list(min_feuille = 100))))) &&
           grepl("croisés", refus(ajuster(p, definir_modele(p, "BOOST", x = u("haie", "pluie"), y = u("densite"), echelle = "none", vc = vc, croises = list(u("haie", "pluie")))))))

cat("Ce que l'interface reçoit, texte des méthodes, tableaux\n")
for (id in c("B1", "B2")) {
  r <- resultat(id)
  verifier(sprintf("%s : analyse qui prédit, chiffres « %s », vues %s, sérialisable", id, r$chiffres, paste(r$vues_defaut, collapse = ", ")),
           isTRUE(r$analyse) && isTRUE(r$prediction) && identical(r$type, "BOOST") && nzchar(r$chiffres) && identical(as.character(r$vues_defaut), c("ens_importance", "ens_erreur")) &&
             inherits(json(r), "json") && identical(names(r$observations)[1:2], c("uid", "nom")) && nrow(r$courbe) == etat$modeles[[id]]$modele$reglages_boost$arbres)
}
r <- resultat("B1")
verifier("loi normale : observée, ajustée, en validation croisée ; R², Q² et RMSEcv pour Comparer les modèles ; la courbe de déviance ; ses dépendances partielles",
         all(c("densite observé", "densite ajusté", "densite prédit (VC)") %in% names(r$observations)) && proche(r$comparaison$Q2, m$qualite$Q2) &&
           all(c("arbres", "apprentissage", "validation") %in% names(r$courbe)) && length(r$dependance) == 2 && is.null(resultat("B2")$comparaison))
for (langue in c("fr", "en")) {
  tt <- texte_methodes(p, d, etat$modeles$B1$modele, langue, "essai")
  tb <- texte_methodes(p, db, mb, langue, "essai")
  verifier(sprintf("texte des méthodes (%s) : la loi, les arbres (profondeur, tirage, taux), le nombre d'arbres par validation croisée, l'influence ; Friedman, Elith, Ridgeway, rpart", langue),
           any(grepl("rpart", tt$paragraphes)) && any(grepl("gbm", tt$paragraphes)) && any(grepl("6 (groupes|groups)", tt$paragraphes)) &&
             any(grepl("Newton", tb$paragraphes)) && any(grepl("AUC|ROC", tb$paragraphes)) && any(grepl("^Friedman, J. H. \\(2001\\)", tt$references)) &&
             any(grepl("^Friedman, J. H. \\(2002\\)", tt$references)) && any(grepl("^Elith", tt$references)) && any(grepl("^Ridgeway", tt$references)) && any(grepl("^Therneau", tt$references)) &&
             any(grepl(if (langue == "fr") "jeu de test" else "test set", tt$paragraphes)))
}
tb <- tables_resultats(p, etat$modeles$B1$modele)
tc <- tables_resultats(p, mb)
verifier("tableaux exportés : des data.frame (résumé, observations, influence, déviance, dépendance partielle ; classement d'une réponse 0/1)",
         all(vapply(c(tb, tc), is.data.frame, TRUE)) && all(c("resume", "observations", "influence", "deviance", "dependance") %in% names(tb)) &&
           "classement_vc" %in% names(tc) && nrow(tb$deviance) == 200)

# ---------------------------------------------------------------- l'export vers R

# Chaque script est écrit (ecrire_script : exécuté d'abord dans le moteur et comparé, `ecart`), puis
# lancé dans une session R neuve (Rscript --vanilla, sans le moteur) ; ses objets en reviennent et
# ecart_boost les compare au modèle du moteur : la déviance après chaque arbre (ajustement et
# validation croisée), le nombre d'arbres, f ajusté et en validation croisée, l'influence relative,
# la qualité (D², R², Q², AUC), le classement, le jeu de test et la dépendance partielle, à 1e-8 près.
cat("Export vers R : des scripts lancés seuls, en français et en anglais, et un rapport R Markdown\n")
dossier <- tempfile("boost_script")
dir.create(dossier)
rscript <- file.path(R.home("bin"), "Rscript")
lanceur <- file.path(dossier, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".r <- mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL))", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, langue) {
  objets <- nom_traduit(c("courbe", "meilleur", "ajuste_lien", "vc", "influence", "qualite", "classement", "test", "dependance", "modele", "X"), langue, NOMS_BOOST_EN)
  sortie <- tempfile("sortie", dossier, ".rds")
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ","))),
                                       stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
hors_donnees <- function(texte) texte[-(grep("^# 2\\. ", texte):(grep("^# 3\\. ", texte) - 1))]
paquets_appeles <- function(texte) unique(regmatches(texte, regexpr("[A-Za-z.]+(?=::)", texte, perl = TRUE)))
# `id` : le modèle gardé (pour la dépendance partielle, calculée d'abord pour `dependance`)
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
  difference <- if (length(s)) ecart_boost(m, list2env(s), langue) else paste(utils::tail(attr(s, "messages"), 3), collapse = " / ")
  francais <- langue == "fr" || !any(grepl("meilleures_coupures|tirage_sac|valeurs_feuilles|reglages", code))
  collisions <- if (langue == "fr") collisions_traduction(hors_donnees(texte), c(NOMS_BOOST_EN, NOMS_SCRIPT_EN)) else list()
  verifier(sprintf("%s (%s) : sans écart vu par le moteur ; lancé seul, les mêmes arbres et résultats ; R et rpart seuls (ni library() ni autre paquet), sans traduire()%s",
                   nom, langue, if (langue == "en") ", ses noms en anglais" else ", aucun nom confondu en anglais"),
           is.null(r$ecart) && is.null(difference) && !any(grepl("library\\(|require\\(|traduire\\(", code)) &&
             all(paquets_appeles(code) %in% c("stats", "utils", "rpart")) && francais && !length(collisions))
  if (!is.null(r$ecart) || !is.null(difference) || length(collisions)) cat("    écart :", r$ecart, "/", difference, "/", names(collisions), "\n")
  invisible(list(r = r, m = m, s = s))
}
db1 <- definir_modele(p, "BOOST", x = xs, y = u("densite"), echelle = "none", vc = vc, options = list(arbres_max = 150, taux = 0.05), test = 221:240)
x <- essayer_script("boost_normale_test", db1, "fr", dependance = c("haie", "sol"))
essayer_script("boost_normale_test_en", db1, "en", dependance = c("h_gel", "sol"))
db2 <- definir_modele(p, "BOOST", x = xs, y = u("presence"), echelle = "none", vc = vc, options = list(loi = "binomiale", arbres_max = 120, taux = 0.05, part = 1, profondeur = 2))
x2 <- essayer_script("boost_binomiale_sans_tirage_en", db2, "en")
verifier("loi binomiale : l'AUC et le classement à 0,5 du script, en validation croisée",
         length(x2$s) > 0 && isTRUE(all.equal(unname(x2$s$quality[c("auc", "auc_vc")]), c(x2$m$qualite$auc, x2$m$qualite$auc_vc), tolerance = 1e-12)) &&
           identical(unname(x2$s$classification$vc$table), unname(x2$m$classification$vc$table)))
db3 <- definir_modele(p, "BOOST", x = xs, y = u("comptage"), echelle = "none", vc = vc, options = list(loi = "poisson", arbres_max = 100, taux = 0.1, profondeur = 1),
                      condition = "secteur != \"Z6\" & h_gel < 28")
x3 <- essayer_script("boost_poisson_souches_condition", db3, "fr")
if (avec_gbm && length(x3$s)) {
  # les arbres du script contre gbm, avec les mêmes réglages et la même graine, sur ses observations
  Xs <- x3$s$X
  Xg <- stats::setNames(Xs, names(X))
  graine()
  g <- gbm::gbm.fit(Xg, x3$m$y_valeurs, distribution = "poisson", n.trees = 100, interaction.depth = 1, n.minobsinnode = 10, shrinkage = 0.1, bag.fraction = 0.5,
                    keep.data = FALSE, verbose = FALSE)
  verifier(sprintf("le script lancé seul et gbm : le même f après %d arbres (écart %.1e)", x3$s$meilleur, ecart(x3$s$ajuste_lien, gbm::predict.gbm(g, Xg, n.trees = x3$s$meilleur))),
           proche(x3$s$ajuste_lien, gbm::predict.gbm(g, Xg, n.trees = x3$s$meilleur), 1e-10))
}
x <- essayer_script("rapport_boost", db1, "fr", dependance = "haie", ext = "Rmd", donnees = "csv", rdata = TRUE)
texte <- readLines(x$r$script, encoding = "UTF-8")
verifier("rapport R Markdown : méthodes, code, résultats, références ; données en CSV et .RData à côté",
         is.null(x$r$ecart) && texte[1] == "---" && any(grepl("^## Méthodes", texte)) && any(grepl("^```\\{r modele\\}", texte)) && any(grepl("^```\\{r influence-table\\}", texte)) &&
           any(grepl("^- Friedman", texte)) && length(x$r$chemins) == 3 && all(file.exists(x$r$chemins)) && !anyDuplicated(sub("^```\\{r ([^,} ]+).*$", "\\1", grep("^```\\{r [^,} ]", texte, value = TRUE))))
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
