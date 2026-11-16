# Arbres boostés (BOOST) : le gradient boosting d'arbres de régression de Friedman (2001), rendu
# stochastique par Friedman (2002), comme le calcule le paquet gbm et comme Elith, Leathwick et
# Hastie (2008) le présentent aux écologues (boosted regression trees, BRT). Écrit ici en R sur
# rpart. Le modèle est une somme d'arbres, f(x) = f0 + λ Σ h_t(x), sur l'échelle du lien de la loi
# de la réponse, choisie comme celle d'un GLM :
#   - normale (la perte quadratique) : f est la moyenne ;
#   - binomiale (une réponse 0/1, la déviance de Bernoulli) : f est le logit de la probabilité ;
#   - Poisson (des comptages, la déviance de Poisson) : f est le log de la moyenne.
# f0 est la constante qui rend la déviance minimale (la moyenne, son logit, son log). À chaque
# étape t :
#   1. le gradient négatif de la déviance de chaque observation (son « résidu de travail ») :
#      z = y - f (normale), y - p avec p = 1 / (1 + exp(-f)) (binomiale), y - exp(f) (Poisson) ;
#   2. un tirage sans remise de la part `fraction` des observations (le bag.fraction de gbm, 0,5
#      par défaut : le boosting stochastique), tiré comme gbm le tire : observation par observation
#      (l'algorithme de sélection séquentielle de Knuth), avec les nombres au hasard de R et une
#      graine fixe (GRAINE_BOOST) ;
#   3. un arbre de régression ajusté à z sur ces observations par rpart, aux moindres carrés comme
#      gbm, de `profondeur` coupures (l'interaction.depth de gbm, qui compte des coupures) choisies
#      comme gbm les choisit : chacune sur la feuille où elle réduit le plus la somme des carrés de
#      z. rpart fait l'arbre complet de cette profondeur (cp = 0) ; on en garde ces coupures, la
#      meilleure d'abord, sous une forme courte qui sert aux prédictions (arbre_court). Au moins
#      `min_noeud` observations par feuille (n.minobsinnode). Une profondeur de 1 donne un modèle
#      additif ;
#   4. la valeur de chaque feuille réestimée comme gbm le fait pour la déviance : la moyenne de z
#      (normale), un pas de Newton–Raphson Σz / Σp(1 - p) (binomiale ; 0 si Σp(1 - p) = 0), le
#      minimum exact log(Σy / Σexp(f)) (Poisson ; -19 si Σy = 0, borné pour que f reste entre -19
#      et 19) ;
#   5. f += λ × la valeur de la feuille de chaque observation (λ : le taux d'apprentissage, le
#      shrinkage de gbm, 0,01 par défaut).
# Le nombre d'arbres est choisi par la validation croisée par les groupes du modèle, comme
# gbm.perf(method = "cv") : chaque groupe prédit, après chaque arbre, par le boosting refait sans
# lui (même graine) ; la déviance moyenne de ces prédictions selon le nombre d'arbres, jusqu'au
# maximum demandé, et le nombre d'arbres où elle est la plus petite. Le modèle final est le
# boosting de toutes les observations arrêté à ce nombre d'arbres.
# L'influence relative de chaque variable (Friedman, 2001 ; relative.influence de gbm) : la somme
# sur les arbres du modèle des réductions de la somme des carrés de z de ses coupures, ramenée à 100.
# Comme pour une forêt (forets.R), les observations où une valeur X manque sont laissées de côté et
# comptées (les forêts et les arbres boostés d'un même projet portent ainsi sur les mêmes
# observations) ; la dépendance partielle de chaque variable est calculée à la demande
# (forets.R : demande_analyse_dependance_partielle).

GRAINE_BOOST <- 1L
LOIS_BOOST <- c("gaussienne", "binomiale", "poisson")
# (la formule de chaque arbre, faite ici : son environnement est l'environnement global, et les
# arbres gardés ne retiennent pas les données de l'étape qui les a faits)
FORMULE_BOOST <- .z ~ .

# Les réglages d'arbres boostés (d$options, sous des noms que la forêt n'a pas : un modèle refait
# d'un autre type ne les reprend pas) ; par défaut, ceux d'Elith et al. (2008) et de gbm.
reglages_boost <- function(d) list(
  loi = option_choix(d, "loi", LOIS_BOOST), arbres = option_nombre(d, "arbres_max", 2000, 10, 20000, TRUE),
  profondeur = option_nombre(d, "profondeur", 3, 1, 10, TRUE), min_noeud = option_nombre(d, "min_feuille", 10, 1, 1e6, TRUE),
  taux = option_nombre(d, "taux", 0.01, 1e-4, 1), fraction = option_nombre(d, "part", 0.5, 0.05, 1))

# La réponse d'arbres boostés : une variable Y quantitative (pas de classe).
verifier_boost <- function(projet, d) {
  if (length(d$classe) && !is.na(d$classe[1])) {
    stop(traduire("Des arbres boostés prennent une réponse Y quantitative (une variable 0/1 pour la loi binomiale), pas une classe.",
                  "Boosted trees take a quantitative Y response (a 0/1 variable for the binomial distribution), not a class."), call. = FALSE)
  }
  ys <- unique(vapply(d$y, function(t) as.numeric(t$variables[1]), 0))
  if (length(ys) == 1 && length(variable_du_projet(projet, ys)$categories)) {
    stop(traduire("La réponse d'arbres boostés est quantitative : une mesure, une variable 0/1 (loi binomiale) ou des comptages (loi de Poisson).",
                  "The response of boosted trees is quantitative: a measurement, a 0/1 variable (binomial distribution) or counts (Poisson distribution)."), call. = FALSE)
  }
  if (length(d$y) != 1) stop(traduire("Des arbres boostés demandent une réponse : une variable Y quantitative.", "Boosted trees need a response: a quantitative Y variable."), call. = FALSE)
  verifier_x_ensemble(d, boost = TRUE)
}

# f0 : la constante de la plus petite déviance.
initiale_boost <- function(y, loi) switch(loi, gaussienne = mean(y), binomiale = log(mean(y) / (1 - mean(y))), poisson = log(mean(y)))

# Le gradient négatif de la déviance (à un facteur 2 près) en f.
gradient_boost <- function(y, f, loi) switch(loi, gaussienne = y - f, binomiale = y - 1 / (1 + exp(-f)), poisson = y - exp(f))

# La déviance de chaque observation (y et f de même longueur, ou f une matrice d'autant de
# lignes) : (y - f)² ; -2 [y f - log(1 + exp(f))] ; 2 [y log(y / μ) - (y - μ)]. gbm donne les mêmes,
# sauf celle de Poisson, dont il retire le terme qui ne dépend pas du modèle (2 [y log(y) - y]) :
# elle peut y être négative ; le nombre d'arbres choisi est le même.
deviance_boost <- function(y, f, loi) switch(loi,
  gaussienne = (y - f)^2,
  binomiale = -2 * (y * f - (pmax(f, 0) + log1p(exp(-abs(f))))),
  poisson = 2 * (ifelse(y > 0, y * log(pmax(y, 1e-300)), 0) - y * f - y + exp(f)))

# f à l'échelle de la réponse : la moyenne, la probabilité, la moyenne des comptages.
reponse_boost <- function(f, loi) switch(loi, gaussienne = f, binomiale = 1 / (1 + exp(-f)), poisson = exp(f))

# Le tirage de k observations parmi n, comme gbm le fait (gbm_engine.cpp, CGBM::iterate) :
# l'observation i est prise si u_i (n - i + 1) < k - (celles déjà prises), u_i uniforme ; n nombres
# au hasard à chaque arbre, même quand toutes sont prises.
tirage_sac <- function(n, k) {
  u <- stats::runif(n)
  if (k >= n) return(seq_len(n))
  dans <- logical(n)
  pris <- 0L
  for (i in seq_len(n)) {
    if (u[i] * (n - i + 1) < k - pris) {
      dans[i] <- TRUE
      pris <- pris + 1L
    }
  }
  which(dans)
}

# Les `profondeur` coupures d'un arbre de rpart (fait jusqu'à cette profondeur) que gbm aurait
# faites : la meilleure d'abord, chacune sur la feuille où elle réduit le plus la somme des carrés
# (improve × dev de rpart : la réduction, pas sa part ; une ligne de splits par coupure, sans
# coupure concurrente ni de substitution), à égalité la feuille la plus ancienne, comme gbm. Rend
# les nœuds coupés (les numéros de rpart : les fils de n sont 2n et 2n + 1), et la variable et la
# réduction de chaque coupure (l'influence relative).
meilleures_coupures <- function(a, profondeur) {
  f <- a$frame
  interne <- f$var != "<leaf>"
  if (!any(interne)) return(list(noeuds = numeric(0), variables = character(0), gains = numeric(0)))
  ligne <- 1L + c(0L, cumsum(interne + f$ncompete + f$nsurrogate))[seq_len(nrow(f))]
  gain <- ifelse(interne, a$splits[pmin(ligne, nrow(a$splits)), "improve"] * f$dev, 0)
  noeuds <- as.numeric(rownames(f))
  candidats <- 1
  choisis <- numeric(0)
  for (k in seq_len(profondeur)) {
    i <- match(candidats, noeuds)
    ok <- !is.na(i) & interne[pmax(i, 1)]
    if (!any(ok)) break
    j <- which(ok)[which.max(gain[i[ok]])]
    meilleur <- candidats[j]
    choisis <- c(choisis, meilleur)
    candidats[j] <- 2 * meilleur  # (le fils gauche prend sa place, le droit vient en dernier : l'ordre des feuilles de gbm)
    candidats <- c(candidats, 2 * meilleur + 1)
  }
  k <- match(choisis, noeuds)
  list(noeuds = choisis, variables = as.character(f$var[k]), gains = gain[k])
}

# L'arbre fait de ces coupures, sous une forme courte : pour chaque nœud coupé (dans l'ordre de
# leurs numéros, un père avant ses fils), la colonne de sa variable, son seuil et son sens (rpart :
# ncat = -1, la gauche si x < seuil ; +1, la gauche si x >= seuil), ou pour une variable qualitative
# le côté de chaque catégorie (csplit : 1 à gauche, 3 à droite, 2 absente du nœud : du côté qui a
# le plus d'observations, comme rpart sans variable de substitution) ; ses feuilles.
arbre_court <- function(a, noeuds, colonnes) {
  if (!length(noeuds)) return(list(noeud = numeric(0), feuilles = 1))
  f <- a$frame
  interne <- f$var != "<leaf>"
  ligne <- 1L + c(0L, cumsum(interne + f$ncompete + f$nsurrogate))[seq_len(nrow(f))]
  tous <- as.numeric(rownames(f))
  noeuds <- sort(noeuds)
  i <- match(noeuds, tous)
  s <- a$splits[ligne[i], , drop = FALSE]
  ncat <- s[, "ncat"]
  list(noeud = noeuds, j = match(as.character(f$var[i]), colonnes), seuil = unname(s[, "index"]), inferieur = unname(ncat == -1),
       cat = lapply(seq_along(noeuds), function(k) if (ncat[k] > 1) as.integer(a$csplit[s[k, "index"], seq_len(ncat[k])])),
       majorite = f$n[match(2 * noeuds, tous)] > f$n[match(2 * noeuds + 1, tous)],
       feuilles = sort(setdiff(c(2 * noeuds, 2 * noeuds + 1), noeuds)))
}

# La feuille (son numéro) de chaque ligne de Xm (une matrice : les catégories par leur numéro).
feuilles_arbre <- function(arb, Xm) {
  noeud <- rep(1, nrow(Xm))
  for (i in seq_along(arb$noeud)) {
    k <- which(noeud == arb$noeud[i])
    if (!length(k)) next
    x <- Xm[k, arb$j[i]]
    gauche <- if (is.null(arb$cat[[i]])) (if (arb$inferieur[i]) x < arb$seuil[i] else x >= arb$seuil[i])
              else { cote <- arb$cat[[i]][x]; ifelse(cote == 2L, arb$majorite[i], cote == 1L) }
    noeud[k] <- 2 * arb$noeud[i] + !gauche
  }
  noeud
}

# Les X en matrice de nombres, une variable qualitative par le numéro de sa catégorie.
matrice_codes <- function(X) {
  M <- matrix(0, nrow(X), ncol(X), dimnames = list(NULL, names(X)))
  for (j in seq_along(X)) M[, j] <- if (is.factor(X[[j]])) as.integer(X[[j]]) else X[[j]]
  M
}

# La valeur de chaque feuille d'après les observations tirées (`feuille` : le numéro de leur
# feuille parmi les nn ; z, y et f : leurs valeurs).
valeurs_feuilles <- function(feuille, z, y, f, loi, nn) {
  somme <- function(v) {
    s <- numeric(nn)
    r <- rowsum(v, feuille)
    s[as.integer(rownames(r))] <- r
    s
  }
  switch(loi,
    gaussienne = {
      k <- tabulate(feuille, nn)
      ifelse(k > 0, somme(z) / pmax(k, 1), 0)
    },
    binomiale = {
      p <- 1 / (1 + exp(-f))
      num <- somme(z)
      den <- somme(p * (1 - p))
      ifelse(den == 0, 0, num / ifelse(den == 0, 1, den))
    },
    poisson = {
      num <- somme(y)
      den <- somme(exp(f))
      v <- ifelse(num == 0, -19, ifelse(den == 0, 0, log(pmax(num, 1e-300) / ifelse(den == 0, 1, den))))
      haut <- rep(0, nn)
      bas <- rep(0, nn)
      k <- sort(unique(feuille))
      haut[k] <- tapply(f, feuille, max)[as.character(k)]
      bas[k] <- tapply(f, feuille, min)[as.character(k)]
      pmax(pmin(v, 19 - haut), -19 - bas)
    })
}

# Un boosting : sur les lignes `ap` de X (des noms simples, forets.R : x_internes) et y, chaque
# ligne de `ev` prédite après chaque arbre (`chemin` : une colonne par arbre). Les `garder` premiers
# arbres sont gardés (forme courte, avec la contribution λ v de chaque feuille), avec la réduction
# de la somme des carrés de chaque variable dans chacun ; `arret` : un nombre d'arbres où garder f
# des lignes `ap`. rpart reçoit son modèle de données tout fait (son argument `model`) : il n'en
# refait pas un à chaque arbre.
boosting <- function(X, y, ap, ev, r, garder = 0, arret = NULL) {
  ya <- y[ap]
  f0 <- initiale_boost(ya, r$loi)
  if (!is.finite(f0)) stop(traduire("Une seule valeur de la réponse : pas de boosting.", "A single response value: no boosting."), call. = FALSE)
  fa <- rep(f0, length(ap))
  fe <- rep(f0, length(ev))
  k <- floor(r$fraction * length(ap))
  ctl <- rpart::rpart.control(maxdepth = r$profondeur, minbucket = r$min_noeud, minsplit = 2 * r$min_noeud, cp = 0, xval = 0,
                              maxcompete = 0, maxsurrogate = 0, usesurrogate = 0)
  base <- X
  base$.z <- 0
  mf <- stats::model.frame(FORMULE_BOOST, base)
  termes <- attr(mf, "terms")
  Xm <- matrice_codes(X)
  deviance <- numeric(r$arbres)
  chemin <- if (length(ev)) matrix(NA_real_, length(ev), r$arbres)
  arbres <- vector("list", garder)
  gains <- matrix(0, garder, ncol(X), dimnames = list(NULL, names(X)))
  f_arret <- NULL
  avec_graine(GRAINE_BOOST, for (t in seq_len(r$arbres)) {
    z <- gradient_boost(ya, fa, r$loi)
    dans <- tirage_sac(length(ap), k)
    mt <- mf[ap[dans], , drop = FALSE]
    mt$.z <- z[dans]
    attr(mt, "terms") <- termes
    a <- rpart::rpart(model = mt, method = "anova", control = ctl, x = FALSE, y = FALSE)
    cb <- meilleures_coupures(a, r$profondeur)
    arb <- arbre_court(a, cb$noeuds, names(X))
    noeud <- feuilles_arbre(arb, Xm)
    nap <- match(noeud[ap], arb$feuilles)
    v <- valeurs_feuilles(nap[dans], z[dans], ya[dans], fa[dans], r$loi, length(arb$feuilles))
    fa <- fa + r$taux * v[nap]
    if (length(ev)) {
      fe <- fe + r$taux * v[match(noeud[ev], arb$feuilles)]
      chemin[, t] <- fe
    }
    deviance[t] <- mean(deviance_boost(ya, fa, r$loi))
    if (t <= garder) {
      arb$valeurs <- r$taux * v
      arbres[[t]] <- arb
      for (i in seq_along(cb$gains)) gains[t, cb$variables[i]] <- gains[t, cb$variables[i]] + cb$gains[i]
    }
    if (identical(t, as.integer(arret))) f_arret <- fa
  })
  list(f0 = f0, deviance = deviance, chemin = chemin, arbres = arbres, gains = gains, f_arret = f_arret)
}

# La validation croisée : chaque groupe prédit après chaque arbre par le boosting refait sans lui
# (même graine) ; la déviance moyenne de ces prédictions par nombre d'arbres (comme le cv.error de
# gbm : la moyenne sur toutes les observations prédites), et le nombre d'arbres où elle est la plus
# petite (gbm.perf(method = "cv")). Un groupe dont les autres n'ont qu'une valeur de la réponse
# reste sans prédiction (`echecs`).
validation_boost <- function(X, y, groupes, r) {
  chemin <- matrix(NA_real_, nrow(X), r$arbres)
  echecs <- 0
  for (g in unique(groupes)) {
    k <- which(groupes == g)
    b <- tryCatch(boosting(X, y, which(groupes != g), k, r), error = function(e) NULL)
    if (is.null(b)) { echecs <- echecs + 1; next }
    chemin[k, ] <- b$chemin
  }
  predites <- !is.na(chemin[, 1])
  if (!any(predites)) stop(traduire("Aucun groupe de validation croisée n'a pu être prédit.", "No cross-validation group could be predicted."), call. = FALSE)
  deviance <- colMeans(deviance_boost(y[predites], chemin[predites, , drop = FALSE], r$loi))
  list(chemin = chemin, deviance = deviance, meilleur = which.min(deviance), echecs = echecs, predites = predites)
}

# Le f d'observations (des X sous les noms simples, sans valeur manquante, leurs catégories celles
# du projet, comme x_internes les donne) : f0 et la contribution de chacun des arbres du modèle.
predire_boost_f <- function(m, X) {
  f <- rep(m$f0, nrow(X))
  if (!length(m$arbres) || !nrow(X)) return(f)
  Xm <- matrice_codes(X)
  for (arb in m$arbres) f <- f + arb$valeurs[match(feuilles_arbre(arb, Xm), arb$feuilles)]
  f
}

ajuster_boost <- function(projet, d) {
  r <- reglages_boost(d)
  verifier_boost(projet, d)
  vars <- variables_x_brutes(projet, d)
  obs <- d$observations
  X <- x_internes(projet, vars, obs)
  ty <- d$y[[1]]
  y <- suppressWarnings(transformer(valeurs_simples(projet, ty, obs), ty$transform))
  avec_y <- is.finite(y)
  complet <- avec_y & stats::complete.cases(X)
  if (sum(complet) < 5) {
    stop(traduire("Trop peu d'observations avec une réponse et toutes leurs valeurs X (il en faut au moins 5).",
                  "Too few observations with a response and all their X values (at least 5 are needed)."), call. = FALSE)
  }
  garde <- obs[complet]
  groupes <- groupes_observations(projet, d, garde)
  if (length(unique(groupes)) < 2) {
    stop(traduire("Des arbres boostés demandent au moins deux groupes de validation croisée parmi leurs observations complètes : le nombre d'arbres est choisi en prédisant chaque groupe par le boosting refait sans lui.",
                  "Boosted trees need at least two cross-validation groups among their complete observations: the number of trees is chosen by predicting each group with the boosting redone without it."), call. = FALSE)
  }
  manquantes_x <- sum(avec_y & !stats::complete.cases(X))
  X <- X[complet, , drop = FALSE]
  y <- y[complet]
  verifier_reponse(y, r$loi)  # (regression.R : des comptages pour Poisson, 0 ou 1 pour la binomiale)
  if (r$loi == "binomiale" && length(unique(y)) < 2) stop(traduire("Une réponse 0/1 demande des 0 et des 1.", "A 0/1 response needs both 0s and 1s."), call. = FALSE)
  if (r$loi == "poisson" && all(y == 0)) stop(traduire("Des comptages tous nuls : pas de modèle de Poisson.", "Counts all zero: no Poisson model."), call. = FALSE)
  N <- length(y)
  if (floor(r$fraction * N) < 2 * r$min_noeud) {
    stop(sprintf(traduire("Chaque arbre est fait sur %d observations (%s des %d), trop peu pour une coupure qui en laisse %d de chaque côté : baisser le minimum par feuille ou tirer une plus grande part.",
                          "Each tree is grown on %d observations (%s of %d), too few for a split leaving %d on each side: lower the minimum per leaf or draw a larger share."),
                 as.integer(floor(r$fraction * N)), pourcent_texte(r$fraction, !identical(getOption("nuee.langue"), "en")), N, r$min_noeud), call. = FALSE)
  }
  vc <- validation_boost(X, y, groupes, r)
  meilleur <- vc$meilleur
  b <- boosting(X, y, seq_len(N), integer(0), r, garder = meilleur, arret = meilleur)
  noms <- vapply(vars, `[[`, "", "nom")
  gains <- colSums(b$gains[seq_len(meilleur), , drop = FALSE])
  influence <- data.frame(variable = noms, bloc = vapply(vars, `[[`, "", "bloc"), influence = unname(gains),
                          pourcentage = if (sum(gains) > 0) 100 * unname(gains) / sum(gains) else 0, stringsAsFactors = FALSE)
  influence <- influence[order(-influence$influence, seq_along(noms)), , drop = FALSE]
  rownames(influence) <- NULL
  f0 <- initiale_boost(y, r$loi)
  nulle <- mean(deviance_boost(y, rep(f0, N), r$loi))
  f_vc <- vc$chemin[, meilleur]
  ok <- vc$predites
  m <- list(type = "BOOST", loi = r$loi, N = N, K = ncol(X), observations = garde, exclues = setdiff(obs, garde), manquantes_x = manquantes_x,
            y = list(nom = as.character(ty$name), transform = ty$transform), variables = vars, reglages_boost = r, donnees = X, y_valeurs = y,
            groupes = groupes, G = length(unique(groupes)), vc_par = nom_groupes_vc(projet, d), f0 = b$f0, arbres = b$arbres,
            meilleur = meilleur, au_maximum = meilleur == r$arbres, influence = influence,
            courbe = data.frame(arbres = seq_len(r$arbres), apprentissage = b$deviance, validation = vc$deviance),
            ajuste_lien = b$f_arret, ajuste = reponse_boost(b$f_arret, r$loi),
            vc = list(lien = f_vc, prediction = reponse_boost(f_vc, r$loi), echecs = vc$echecs, sans_prediction = sum(!ok)),
            deviance = list(nulle = nulle, apprentissage = b$deviance[meilleur], validation = vc$deviance[meilleur]))
  m$qualite <- list(D2 = 1 - m$deviance$apprentissage / nulle, D2_vc = 1 - m$deviance$validation / nulle,
                    deviance = m$deviance$apprentissage, deviance_vc = m$deviance$validation, deviance_nulle = nulle)
  if (r$loi == "gaussienne") {
    ss <- sum((y - mean(y))^2)
    m$qualite$r2 <- 1 - sum((y - m$ajuste)^2) / ss
    m$qualite$Q2 <- 1 - sum((y[ok] - f_vc[ok])^2) / sum((y[ok] - mean(y))^2)
    m$qualite$RMSEE <- sqrt(mean((y - m$ajuste)^2))
    m$qualite$RMSEcv <- sqrt(mean((y[ok] - f_vc[ok])^2))
  }
  if (r$loi == "binomiale") {
    classes <- c("0", "1")
    vote <- function(p) cbind(`0` = 1 - p, `1` = p)
    m$classes <- classes
    m$classification <- list(ajuste = classement_ensemble(y, ifelse(m$ajuste >= 0.5, "1", "0"), vote(m$ajuste), classes),
                             vc = classement_ensemble(y, ifelse(is.na(m$vc$prediction), NA, ifelse(m$vc$prediction >= 0.5, "1", "0")), vote(m$vc$prediction), classes))
    m$qualite$auc <- m$classification$ajuste$roc[[2]]$auc
    m$qualite$auc_vc <- m$classification$vc$roc[[2]]$auc
  }
  m
}

# Le jeu de test : ses observations prédites (à l'échelle de la réponse ; une observation où une
# valeur X manque reste sans prédiction), comme celui d'une forêt de régression.
tester_boost <- function(projet, m, d) {
  test <- as.numeric(d$test)
  p <- prediction_ensemble(projet, m, test)
  y <- suppressWarnings(transformer(valeurs_simples(projet, d$y[[1]], test), d$y[[1]]$transform))
  test_regression(m, test, unname(projet$noms_obs[as.character(test)]), y, p$prediction, sum(!p$complet))
}

predire_boost <- function(projet, m, d, observations) {
  p <- prediction_ensemble(projet, m, observations)
  out <- data.frame(row.names = seq_along(observations))
  out[[paste(m$y$nom, "prédit")]] <- p$prediction
  if (m$loi != "gaussienne") out[[paste(m$y$nom, "prédit (lien)")]] <- p$lien
  y <- suppressWarnings(transformer(valeurs_simples(projet, d$y[[1]], observations), d$y[[1]]$transform))
  if (any(!is.na(y))) out[[paste(m$y$nom, "observé")]] <- y
  out$hors_domaine <- FALSE
  rownames(out) <- NULL
  out
}

# Une dépendance partielle (forets.R : dependance_variable) : la moyenne de f sur les observations
# (l'échelle du lien, comme plot.gbm), et elle à l'échelle de la réponse.
dependance_boost <- function(m, j, grille) {
  f <- predire_boost_f(m, donnees_grille(m$donnees, j, grille))
  lien <- as.vector(tapply(f, rep(seq_along(grille), each = nrow(m$donnees)), mean))
  list(y = I(lien), reponse = I(reponse_boost(lien, m$loi)))
}

# Les observations : la réponse observée, ajustée (le modèle de toutes les observations) et prédite
# en validation croisée (à l'échelle de la réponse), le groupe de validation croisée.
observations_boost <- function(projet, m) {
  obs <- data.frame(uid = m$observations, nom = unname(projet$noms_obs[as.character(m$observations)]), stringsAsFactors = FALSE)
  y <- m$y$nom
  obs[[paste(y, "observé")]] <- m$y_valeurs
  obs[[paste(y, "ajusté")]] <- m$ajuste
  obs[[paste(y, "prédit (VC)")]] <- m$vc$prediction
  if (m$loi == "binomiale") obs$classe <- as.character(m$y_valeurs)
  obs$groupe_vc <- m$groupes
  obs
}

resultat_boost <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  y <- m$y$nom
  q <- m$qualite
  r <- m$reglages_boost
  fr <- !identical(getOption("nuee.langue"), "en")  # (la ligne de l'explorateur : sa virgule décimale)
  nb <- function(x) nombre_texte(x, 2, fr)
  chiffres <- switch(m$loi,
    gaussienne = sprintf("R2 %s · Q2 %s · %d %s", nb(q$r2), nb(q$Q2), m$meilleur, traduire("arbres", "trees")),
    binomiale = sprintf("AUC %s %s · D2 %s %s · %d %s", traduire("VC", "CV"), nb(q$auc_vc), traduire("VC", "CV"), nb(q$D2_vc), m$meilleur, traduire("arbres", "trees")),
    sprintf("D2 %s · D2 %s %s · %d %s", nb(q$D2), traduire("VC", "CV"), nb(q$D2_vc), m$meilleur, traduire("arbres", "trees")))
  gauss <- m$loi == "gaussienne"
  list(id = id, projet = p$id, nom = d$name %||% "", titre = d$title %||% "", type = "BOOST", analyse = TRUE, prediction = TRUE,
       classement = FALSE, loi = m$loi, regression = FALSE, opls = FALSE, Ap = 0, A = 0, N = m$N, K = m$K, noms_y = I(y),
       transformation_y = nom_transformation(m$y$transform) %||% "", observations = observations_boost(p, m),
       influence = m$influence, courbe = m$courbe, meilleur = m$meilleur, au_maximum = m$au_maximum, qualite = q, reglages_boost = r,
       classes = I(m$classes %||% character(0)), classification = m$classification, echecs_vc = m$vc$echecs,
       RMSEE = if (gauss) stats::setNames(list(q$RMSEE), y), RMSEcv = if (gauss) stats::setNames(list(q$RMSEcv), y),
       comparaison = if (gauss) list(R2 = q$r2, Q2 = q$Q2, RMSEE = q$RMSEE, RMSEcv = q$RMSEcv),
       dependance = unname(m$dependance), exclues_manquantes = length(m$exclues), manquantes_x = m$manquantes_x,
       variables_x = I(vapply(m$variables, `[[`, "", "nom")),
       G = m$G, vc_par = m$vc_par %||% "", reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app),
       note = d$note %||% "", test = m$test, demande = d$demande %||% list(type = d$type, numero = d$number),
       vues_defaut = I(c("ens_importance", "ens_erreur")), chiffres = chiffres)
}

tables_boost <- function(projet, m) {
  obs <- observations_boost(projet, m)
  obs$uid <- NULL
  obs$classe <- NULL
  names(obs)[names(obs) == "nom"] <- "Observation"
  r <- m$reglages_boost
  q <- m$qualite
  resume <- data.frame(N = m$N, variables = m$K, groupes_vc = m$G, loi = r$loi, arbres_max = r$arbres, arbres = m$meilleur, profondeur = r$profondeur,
                       min_noeud = r$min_noeud, taux = r$taux, fraction = r$fraction, graine = GRAINE_BOOST, deviance_nulle = q$deviance_nulle,
                       deviance = q$deviance, deviance_vc = q$deviance_vc, D2 = q$D2, D2_vc = q$D2_vc, stringsAsFactors = FALSE)
  if (m$loi == "gaussienne") {
    resume$R2 <- q$r2
    resume$Q2 <- q$Q2
    resume$RMSEcv <- q$RMSEcv
  }
  if (m$loi == "binomiale") {
    resume$AUC <- q$auc
    resume$AUC_vc <- q$auc_vc
  }
  out <- list(resume = resume, observations = obs, influence = m$influence, deviance = m$courbe)
  if (m$loi == "binomiale") out$classement_vc <- tableau_classes(m$classification$vc$table, m$classes)
  if (length(m$dependance)) out$dependance <- tableau_dependance(m$dependance)
  out
}

texte_methodes_boost <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  r <- m$reglages_boost
  q <- m$qualite
  g <- function(x) guillemets_texte(x, fr)
  num <- function(x, k = 2) nombre_texte(x, k, fr)
  noms <- vapply(m$variables, `[[`, "", "nom")
  laissees <- if (length(m$exclues)) sprintf(if (fr) " (%d autres laissées de côté : leur réponse ou une valeur X manque)"
                                             else " (%d others left out: their response or an X value is missing)", length(m$exclues)) else ""
  nom_reponse <- paste0(g(m$y$nom), unite_texte(projet, m$y$nom))  # (avec son unité : atelier.R)
  reponse <- if (!is.null(m$y$transform)) formule_transformation(m$y$transform, nom_reponse, fr) else nom_reponse
  p1 <- sprintf(if (fr) "Les arbres boostés portent sur %s%s ; leur réponse est %s." else "The boosted trees use %s%s; their response is %s.",
                observations_texte(m$N, d, fr), laissees, reponse)
  explicatives <- if (length(noms) <= 12) sprintf(if (fr) "%d variables explicatives (%s)" else "%d explanatory variables (%s)", length(noms), liste_mots(noms, fr))
                  else sprintf(if (fr) "%d variables explicatives" else "%d explanatory variables", length(noms))
  loi <- switch(m$loi,
    gaussienne = if (fr) "la perte quadratique (loi normale)" else "the squared-error loss (normal distribution)",
    binomiale = if (fr) "la déviance de Bernoulli (réponse 0/1, lien logit)" else "the Bernoulli deviance (0/1 response, logit link)",
    if (fr) "la déviance de Poisson (comptages, lien log)" else "the Poisson deviance (counts, log link)")
  feuilles <- switch(m$loi,
    gaussienne = if (fr) "la moyenne de ses résidus" else "the mean of its residuals",
    binomiale = if (fr) "un pas de Newton–Raphson, Σ(y − p) / Σp(1 − p)" else "one Newton–Raphson step, Σ(y − p) / Σp(1 − p)",
    if (fr) "log(Σy / Σμ), qui rend la déviance minimale" else "log(Σy / Σμ), which minimizes the deviance")
  p2 <- sprintf(if (fr) "Un modèle d'arbres de régression boostés (gradient boosting, Friedman, 2001 ; boosting stochastique, Friedman, 2002 ; Elith et al., 2008) a été ajusté sur les valeurs brutes de %s avec %s, comme le calcule le paquet gbm (Ridgeway, 2007). À chaque étape, un arbre de %d coupures (profondeur d'interaction ; chaque coupure faite sur la feuille où elle réduit le plus l'erreur) est ajusté par rpart (Therneau & Atkinson) au gradient de la déviance, sur un tirage sans remise de %s des observations (graine %d), avec au moins %d observations par feuille ; la valeur de chaque feuille est réestimée pour la déviance (%s), puis ajoutée au modèle avec un taux d'apprentissage de %s."
                else "A boosted regression tree model (gradient boosting, Friedman, 2001; stochastic boosting, Friedman, 2002; Elith et al., 2008) was fitted on the raw values of %s with %s, as the gbm package computes it (Ridgeway, 2007). At each step, a tree of %d splits (interaction depth; each split made on the leaf where it reduces the error most) is fitted by rpart (Therneau & Atkinson) to the gradient of the deviance, on a draw without replacement of %s of the observations (seed %d), with at least %d observations per leaf; the value of each leaf is re-estimated for the deviance (%s), then added to the model with a learning rate of %s.",
                explicatives, loi, r$profondeur, pourcent_texte(r$fraction, fr), GRAINE_BOOST, r$min_noeud, feuilles, format(r$taux, decimal.mark = if (fr) "," else "."))
  if (m$manquantes_x > 0) p2 <- paste(p2, sprintf(if (fr) "Les %d observations où une valeur X manquait ont été laissées de côté." else "The %d observations with a missing X value were left out.", m$manquantes_x))
  qualite <- switch(m$loi,
    gaussienne = sprintf(if (fr) "R² = %s, Q² = %s (1 − PRESS / SS) et RMSEcv = %s" else "R² = %s, Q² = %s (1 − PRESS / SS) and RMSEcv = %s",
                         num(q$r2), num(q$Q2), format(signif(q$RMSEcv, 4), decimal.mark = if (fr) "," else ".")),
    binomiale = sprintf(if (fr) "la part de la déviance expliquée est de %s (%s en validation croisée) et l'aire sous la courbe ROC de %s (%s en validation croisée)"
                        else "the share of the deviance explained is %s (%s in cross-validation) and the area under the ROC curve %s (%s in cross-validation)",
                        num(q$D2), num(q$D2_vc), num(q$auc), num(q$auc_vc)),
    sprintf(if (fr) "la part de la déviance expliquée est de %s (%s en validation croisée)" else "the share of the deviance explained is %s (%s in cross-validation)",
            num(q$D2), num(q$D2_vc)))
  p3 <- sprintf(if (fr) "Le nombre d'arbres, %d sur %d au plus, est celui de la plus petite déviance en validation croisée %s, chaque groupe étant prédit par le boosting refait sans lui%s ; %s."
                else "The number of trees, %d of at most %d, is that of the smallest cross-validated deviance %s, each group being predicted by the boosting redone without it%s; %s.",
                m$meilleur, r$arbres, groupes_texte(m$vc_par, m$G, fr),
                if (m$au_maximum) (if (fr) " (elle baissait encore au dernier arbre)" else " (it was still falling at the last tree)") else "", qualite)
  premieres <- utils::head(m$influence, 3)
  p4 <- sprintf(if (fr) "L'influence relative des variables (Friedman, 2001 : la somme des réductions d'erreur de leurs coupures sur les arbres, ramenée à 100) est la plus grande pour %s."
                else "The relative influence of the variables (Friedman, 2001: the sum of the error reductions of their splits over the trees, scaled to 100) is largest for %s.",
                liste_mots(sprintf("%s (%s)", vapply(premieres$variable, g, ""), vapply(premieres$pourcentage, function(x) pourcent_texte(x / 100, fr), "")), fr))
  if (length(m$dependance)) {
    p4 <- paste(p4, if (fr) "L'effet de chaque variable est montré par sa dépendance partielle (Friedman, 2001) : la valeur moyenne du modèle (à l'échelle du lien) sur ses observations quand la variable prend chaque valeur d'une grille."
                    else "The effect of each variable is shown by its partial dependence (Friedman, 2001): the model's mean value (on the link scale) over its observations when the variable takes each value of a grid.")
  }
  if (!is.null(m$test)) {
    p4 <- paste(p4, sprintf(if (fr) "Un jeu de test de %d observations, laissées hors du modèle et de la validation croisée, a ensuite été prédit : RMSEP = %s, biais = %s, Q² externe = %s."
                            else "A test set of %d observations, left out of the model and of the cross-validation, was then predicted: RMSEP = %s, bias = %s, external Q² = %s.",
                            m$test$n, nombre_texte(m$test$statistiques$RMSEP, 3, fr), nombre_texte(m$test$statistiques$biais, 3, fr), nombre_texte(m$test$statistiques$Q2ext, 2, fr)))
  }
  logiciels <- references_classement(c("friedman2001", "friedman2002", "elith", "ridgeway", "rpart"), "rpart", fr, version)
  list(paragraphes = c(p1, p2, p3, p4, logiciels$phrase), references = logiciels$references)
}
