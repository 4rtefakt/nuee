# Vérifie que les raccourcis de calcul de la 1.0 et d'après (la vitesse sur les grands tableaux, les
# spectres et les autres analyses) rendent exactement ce que rend le calcul qu'ils évitent
# (identical() avec le BLAS de référence, celui de R sous Windows ; à 1e-12 près avec un BLAS
# optimisé, OpenBLAS ou MKL, qui ne fait pas ses sommes dans le même ordre selon le produit) :
#   - NIPALS : la projection des colonnes (crossprod) et celle d'un masque sans case manquante
#     (somme_blas) redonnent celles d'avant (t(E), M %*% v^2) ; retirer une composante sans
#     multiplier par un masque de 1 ; preparer() d'une matrice sans valeur manquante ;
#   - des spectres prétraités ligne à ligne (SNV, Savitzky–Golay, detrend, ligne de base), valeurs
#     manquantes comprises : ceux d'une partie des observations sont ces lignes de ceux de toutes ;
#   - l'ajustement automatique : le modèle qu'il garde (modele_tronque) est celui qu'ajuster refait,
#     ACP, PLS à un et deux Y, PLS-DA, OPLS à une et deux prédictives, OPLS-DA à trois classes, avec
#     ou sans valeurs manquantes, spectres prétraités ou MSC, chacune des trois façons de valider ;
#   - le test de permutation, le côté X calculé une fois : les mêmes R2 et Q2 qu'en refaisant tout ;
#   - valeurs_bloc et definir_modele : les mêmes valeurs et les mêmes termes qu'en cherchant chaque
#     terme seul ;
#   - enregistrer_projet : octet pour octet le fichier qu'écrit jsonlite, chaque observation et
#     chaque terme un à un ;
#   - construire_bloc : ses termes simples mis à l'échelle d'un coup, comme un à un par echelle()
#     (UV, Pareto, centrage, aucune ; blocs et modificateurs ; valeurs manquantes, une colonne
#     constante ou vide ; les centres et poids d'un autre bloc, l'écart-type du modèle complet) ;
#   - les arbres boostés : le tirage de chaque arbre comme la boucle de gbm l'écrit, et les arbres
#     faits par la fonction C de rpart comme par rpart() ;
#   - l'échantillonnage par distances : les intégrales de la fonction de détection (une clé seule,
#     des termes d'ajustement ; lignes et points) comme avec detection_distance en chaque point ;
#   - TRIM : l'inverse de la corrélation sérielle de chaque site, calculée une fois par motif de
#     temps observés, comme site par site.
#   Rscript moteur/tests/verifier_vitesse.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
blas <- tolower(paste(extSoftVersion()[["BLAS"]], La_library()))
# (le nom ne dit pas tout : sous Linux, libblas.so.3 peut être OpenBLAS ; un BLAS optimisé, ou des
# multiplications-additions fusionnées, ne somment pas crossprod et %*% dans le même ordre)
meme_ordre <- local({
  P <- matrix(sin(seq_len(400 * 50)), 400)
  w <- cos(seq_len(400))
  identical(crossprod(P, w), t(P) %*% w)
})
reference <- meme_ordre && !grepl("openblas|mkl|blis|atlas|accelerate|veclib|flexiblas", blas)
pareil <- function(a, b) if (reference) identical(a, b) else isTRUE(all.equal(a, b, tolerance = 1e-12))
cat(if (reference) "BLAS de référence : identical()\n" else sprintf("BLAS optimisé (%s) : à 1e-12 près\n", blas))

cat("NIPALS\n")
set.seed(1)
E <- matrix(rnorm(40 * 25), 40, 25)
M <- matrix(1, 40, 25)
Mt <- M
Mt[sample(length(Mt), 60)] <- 0
u <- rnorm(40)
v <- rnorm(25)
verifier("projeter_colonnes = projeter de la matrice transposée (sans et avec cases manquantes)",
         pareil(projeter_colonnes(E, M, u), projeter(t(E), t(M), u)) && pareil(projeter_colonnes(E * Mt, Mt, u), projeter(t(E * Mt), t(Mt), u)))
verifier("un masque plein : la même projection sans lui, des lignes et des colonnes",
         masque_plein(M) && !masque_plein(Mt) && pareil(projeter(E, M, v, TRUE), projeter(E, M, v)) &&
           pareil(projeter_colonnes(E, M, u, TRUE), projeter_colonnes(E, M, u)))
verifier("retirer une composante : sans multiplier par un masque plein, le même résultat",
         pareil(retirer_composante(E, M, u, v), (E - outer(u, v)) * M) && pareil(retirer_composante(E, Mt, u, v), (E - outer(u, v)) * Mt))
X <- E
dimnames(X) <- list(paste0("o", 1:40), paste0("v", 1:25))
Xm <- X
Xm[3, 4] <- NA
verifier("preparer : une matrice sans valeur manquante telle quelle, un masque de 1 ; avec, comme avant",
         pareil(preparer(X), list(E = X, M = 1 * !is.na(X))) && pareil(preparer(Xm)$M, 1 * !is.na(Xm)) && pareil(preparer(Xm)$E[3, 4], 0))

cat("Spectres prétraités ligne à ligne\n")
spectres_essai <- function(N, K, graine) {
  set.seed(graine)
  onde <- seq(1100, 2400, length.out = K)
  bandes <- vapply(c(1300, 1700, 2100), function(c) exp(-0.5 * ((onde - c) / 60)^2), numeric(K))
  conc <- matrix(runif(N * 3), N, 3)
  X <- rnorm(N, 0, 0.1) + rlnorm(N, 0, 0.2) * (conc %*% t(bandes)) + matrix(rnorm(N * K, 0, 0.003), N, K)
  colnames(X) <- sprintf("%.1f", onde)
  list(X = X, conc = conc)
}
s <- spectres_essai(30, 60, 2)
Xs <- s$X
Xs[4, 10] <- NA
Xs[7, c(1, 60)] <- NA
lignes <- c(2, 4, 5, 7, 11, 19, 30)
etapes <- list(list(list(type = "snv")), list(list(type = "sg", derivee = 1, fenetre = 11, degre = 2)),
               list(list(type = "sg", derivee = 0, fenetre = 7, degre = 3)), list(list(type = "detrend", degre = 2)),
               list(list(type = "ligne_base")), list(list(type = "snv"), list(type = "sg", derivee = 2, fenetre = 15, degre = 3)))
for (e in etapes) {
  verifier(sprintf("%s : les lignes prétraitées seules = ces lignes de toutes", paste(vapply(e, `[[`, "", "type"), collapse = " + ")),
           pareil(pretraiter_spectres(Xs[lignes, ], e)$X, pretraiter_spectres(Xs, e)$X[lignes, ]) && spectres_par_ligne(e))
}
verifier("le MSC n'est pas prétraité ligne à ligne (son spectre moyen vient de toutes)", !spectres_par_ligne(list(list(type = "snv"), list(type = "msc"))))

cat("L'ajustement automatique garde le modèle qu'ajuster refait\n")
s <- spectres_essai(45, 70, 3)
df <- data.frame(echantillon = sprintf("E%02d", 1:45), classe = c("a", "b", "c")[findInterval(s$conc[, 1], quantile(s$conc[, 1], c(1 / 3, 2 / 3))) + 1],
                 y1 = round(5 * s$conc[, 2] + rnorm(45, 0, 0.05), 3), y2 = round(3 * s$conc[, 3] + rnorm(45, 0, 0.05), 3), round(s$X, 5), check.names = FALSE)
p <- projet_depuis_tableau(df, "essai.csv")
K <- ncol(s$X)
x <- 4:(K + 3)
pm <- p
pm$donnees[cbind(c(3, 8, 8, 20, 33), c(10, 11, 40, 60, 70))] <- NA
etapes_sg <- list(list(type = "snv"), list(type = "sg", derivee = 1, fenetre = 11, degre = 2))
etapes_msc <- list(list(type = "msc"), list(type = "sg", derivee = 1, fenetre = 11, degre = 2))
cas <- list(list("PCA", NULL, NULL), list("PLS", 2, NULL), list("PLS", c(2, 3), NULL), list("PLS-DA", NULL, 1), list("OPLS", 2, NULL),
            list("OPLS", c(2, 3), NULL), list("OPLS-DA", NULL, 1))
reglages <- list(list(projet = p, spectres = etapes_sg, validation = "centres", nom = "spectres prétraités, centres recalculés"),
                 list(projet = pm, spectres = etapes_msc, validation = "sans_fuite", nom = "valeurs manquantes, MSC, sans fuite"),
                 list(projet = pm, spectres = NULL, validation = "complet", nom = "valeurs manquantes, modèle complet"))
for (r in reglages) for (k in cas) {
  d <- definir_modele(r$projet, k[[1]], x = x, y = k[[2]], classe = k[[3]], spectres = r$spectres, validation = r$validation)
  auto <- autoajuster_modele(r$projet, d)
  d$A <- max(1, auto$A)
  verifier(sprintf("%s%s (%d composantes), %s", k[[1]], if (length(k[[2]]) > 1) " à deux Y" else "", d$A, r$nom),
           !is.null(auto$modele) && identical(autoajuster(r$projet, d), auto$A) && pareil(ajuster(r$projet, d, calcule = auto$modele), ajuster(r$projet, d)))
}

cat("Test de permutation : le côté X calculé une fois\n")
# (la boucle de permutations(), chaque modèle refait en entier, comme avant la 1.0)
permutations_en_entier <- function(projet, m, d, y = 1, n = 5, graine = 1) {
  d$test <- NULL
  lignes_y <- match(as.character(m$observations), rownames(projet$donnees))
  variables <- unique(vapply(if (m$plsda) d$y else d$y[y], function(t) as.character(t$variables[1]), ""))
  valeurs <- projet$donnees[lignes_y, variables, drop = FALSE]
  set.seed(graine)
  t(vapply(seq_len(n), function(i) {
    ordre <- sample(length(lignes_y))
    pp <- projet
    pp$donnees[lignes_y, variables] <- valeurs[ordre, , drop = FALSE]
    r <- calculer_modele(pp, d, m$A, m$G)
    c(R2 = 1 - unname(r$ssyv[y, m$A + 1] / r$ssyv[y, 1]), Q2 = unname(r$q2v_cum[y]))
  }, c(R2 = 0, Q2 = 0)))
}
for (k in list(list("PLS", 2, NULL, etapes_sg, p), list("PLS-DA", NULL, 1, etapes_sg, pm), list("PLS", 2, NULL, etapes_msc, pm))) {
  d <- definir_modele(k[[5]], k[[1]], x = x, y = k[[2]], classe = k[[3]], spectres = k[[4]], A = 2)
  m <- ajuster(k[[5]], d)
  r <- permutations(k[[5]], m, d, n = 5)
  verifier(sprintf("%s, %s : les mêmes R2 et Q2", k[[1]], paste(vapply(k[[4]], `[[`, "", "type"), collapse = " + ")),
           pareil(unname(as.matrix(r$points[-1, c("R2", "Q2")])), unname(permutations_en_entier(k[[5]], m, d))))
}

cat("Valeurs des termes et définitions\n")
dt <- data.frame(id = sprintf("o%d", 1:12), lieu = rep(c("nord", "sud", "est"), 4), a = c(1:11, NA), b = exp(1:12 / 4), c = rep(c(2, 5), 6))
pt <- projet_depuis_tableau(dt, "t.csv")
d <- definir_modele(pt, "PLS", x = c(1, 2, 3), y = 4, transformations = c(`3` = "log10"), croises = list(c(1, 2)))
obs <- rev(d$observations)[-2]
simples <- which(vapply(d$x, function(t) length(t$variables) == 1, TRUE))
a_la_main <- vapply(d$x[simples], function(t) transformer(valeurs_simples(pt, t, obs), t$transform), numeric(length(obs)))
vb <- valeurs_bloc(pt, d$x, obs)
verifier("valeurs_bloc : chaque terme simple comme lu seul (catégories, transformation, ordre des observations)",
         pareil(vb[, simples], unname(a_la_main)) && all(is.na(vb[, -simples])))
verifier("definir_modele : les termes de chaque variable, cherchée par son numéro",
         pareil(definir_modele(pt, "PCA", x = c(3, 1, 4))$x, unlist(lapply(c(3, 1, 4), function(u) termes_variable(pt, u, "UV")), recursive = FALSE)))

cat("Enregistrer un projet : le même fichier\n")
# (enregistrer_projet d'avant la 1.0 : chaque observation et chaque terme écrits un à un par jsonlite)
enregistrer_un_a_un <- function(projet, chemin, version = "") {
  p <- unclass(projet)
  for (i in seq_along(p$datasets)) {
    d <- p$datasets[[i]]
    p$datasets[[i]]$values <- unname(projet$donnees[as.character(d$observations), as.character(d$variables), drop = FALSE])
  }
  p$donnees <- p$noms_obs <- p$noms_var <- NULL
  p$id <- p$chemin <- p$chemin_enregistrement <- p$modifie <- p$table <- p$fichier_lu <- p$note <- NULL
  p$nuee <- list(format = FORMAT_PROJET, version = as.character(version), R = paste(R.version$major, R.version$minor, sep = "."),
                 paquets = as.list(vapply(PAQUETS_CALCUL, version_paquet, "")))
  jsonlite::write_json(p, chemin, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
}
octets <- function(f) readBin(f, "raw", file.size(f))
meme_fichier <- function(p) {
  a <- tempfile(fileext = ".nuee")
  b <- tempfile(fileext = ".nuee")
  enregistrer_projet(p, a, "1.0")
  enregistrer_un_a_un(p, b, "1.0")
  identical(octets(a), octets(b))
}
pe <- lire_projet("exemple/vergers.json")
ps <- p
for (i in 1:3) ps$models <- c(ps$models, list(definir_modele(ps, if (i == 2) "PCA" else "PLS", x = x, y = if (i != 2) 2, spectres = etapes_sg, nom = paste0("M", i))))
ps$models <- c(ps$models, list(definir_modele(ps, "PLS", x = x[1:30], y = 3, nom = "M4")))
dt2 <- data.frame(code = sprintf("c%d", 1:30), autre = sprintf("n° %d « é »", 1:30), lieu = rep(c("nord", "sud"), 15), v = c(1:29, NA))
verifier("le projet d'exemple (ses modèles de tous les types)", meme_fichier(pe))
verifier("des spectres à quatre modèles, trois aux mêmes termes", meme_fichier(ps))
verifier("un tableau à deux identifiants par observation, du texte accentué", meme_fichier(projet_depuis_tableau(dt2, "t.csv")))
verifier("un numéro d'observation non entier parmi des entiers", {
  pn <- projet_depuis_tableau(dt2, "t.csv")
  pn$observations[[3]]$uid <- 3.5
  meme_fichier(pn)
})
verifier("des observations aux identifiants plus ou moins nombreux (écrites une à une)", {
  pn <- projet_depuis_tableau(dt2, "t.csv")
  pn$observations[[4]]$ids <- c(pn$observations[[4]]$ids, "x")
  meme_fichier(pn)
})

cat("Les termes simples d'un bloc mis à l'échelle d'un coup\n")
# (construire_bloc d'avant, pour des termes simples : chacun mis à l'échelle seul, par echelle())
bloc_terme_a_terme <- function(termes, B, imposees = NULL, complet = NULL) {
  K <- length(termes)
  X <- matrix(NA_real_, nrow(B), K)
  centre <- poids <- ecart <- rep(NA_real_, K)
  type <- character(K)
  facteur <- rep(1, K)
  gardes <- stats::setNames(numeric(0), character(0))
  ponderation <- facteurs_ponderation(termes)
  noms <- vapply(termes, function(t) as.character(t$name), "")
  for (k in seq_len(K)) {
    x <- B[, k]
    ech <- termes[[k]]$scaling
    if (!is.null(imposees)) {
      cw <- c(centre = imposees$centre[k], poids = imposees$poids[k])
      type[k] <- imposees$type[k]
      facteur[k] <- imposees$facteur[k] %||% 1
    } else {
      ec <- complet$ecart[k]
      remplacable <- !is.null(ec) && ech$type %in% c("UV", "Par") && isTRUE(ec > 0)
      if (remplacable && !is.finite(complet$seuil)) {
        cw <- echelle(x, ech$type, ecart = ec)
      } else {
        cw <- echelle(x, ech$type)
        if (remplacable && cw[["ecart"]] < complet$seuil * ec) {
          if (cw[["ecart"]] > 0 && is.finite(complet$seuil)) gardes[[noms[k]]] <- cw[["ecart"]] / ec
          cw <- echelle(x, ech$type, ecart = ec)
        }
      }
      cw[["poids"]] <- cw[["poids"]] * ponderation[k]
      facteur[k] <- ponderation[k]
      type[k] <- ech$type
      ecart[k] <- cw[["ecart"]]
    }
    centre[k] <- cw[["centre"]]
    poids[k] <- cw[["poids"]]
    X[, k] <- (x - cw[["centre"]]) * cw[["poids"]]
  }
  list(X = X, centre = centre, poids = poids, type = type, facteur = facteur, ecart = ecart, gardes = gardes)
}
meme_bloc <- function(b, a) {
  pareil(unname(b$X), a$X) && identical(b[c("centre", "poids", "type", "facteur", "ecart", "gardes")], a[c("centre", "poids", "type", "facteur", "ecart", "gardes")])
}
set.seed(4)
nb <- 40
db <- data.frame(id = sprintf("o%02d", seq_len(nb)), matrix(round(rnorm(nb * 12, 10, 3), 3), nb, 12, dimnames = list(NULL, sprintf("v%02d", 1:12))))
pb <- projet_depuis_tableau(db, "bloc.csv")
d <- definir_modele(pb, "PCA", x = 1:12, echelle = c(defaut = "UV", `2` = "Par", `3` = "Ctr", `4` = "none", `9` = "Par"),
                    blocs = c(`5` = "A", `6` = "A", `7` = "A"), modificateurs = c(`8` = 2.5, `9` = 0.5))
obs <- d$observations
B <- valeurs_bloc(pb, d$x, obs)
B[c(2, 9, 30), 1] <- NA                                 # (des valeurs manquantes)
B[c(5, 6), 2] <- NA
B[, 10] <- NA                                           # (une colonne vide)
B[, 11] <- 7                                            # (constante)
B[, 12] <- c(15, 2, 19, 4, 10 + stats::rnorm(nb - 4, 0, 0.05))  # (presque constante sans ses quatre premières lignes)
hors <- seq_len(nb) <= 4
complet <- construire_bloc(pb, d$x, obs, bruts = B)
verifier("le bloc du modèle complet : centres, poids, types, facteurs, écarts-types, matrice", meme_bloc(complet, bloc_terme_a_terme(d$x, B)))
for (seuil in c(Inf, SEUIL_ECART_VC)) {
  cpl <- list(ecart = complet$ecart, seuil = seuil)
  sous <- construire_bloc(pb, d$x, obs[!hors], bruts = B[!hors, , drop = FALSE], complet = cpl, garder_simples = FALSE)
  avant <- bloc_terme_a_terme(d$x, B[!hors, , drop = FALSE], complet = cpl)
  verifier(sprintf("un sous-modèle, seuil %s des écarts-types du modèle complet%s", format(seuil),
                   if (length(avant$gardes)) sprintf(" (%d gardé)", length(avant$gardes)) else ""),
           meme_bloc(sous, avant) && (is.finite(seuil) == (length(avant$gardes) > 0)))
  verifier("  et son groupe caché, aux centres et poids du sous-modèle",
           meme_bloc(construire_bloc(pb, d$x, obs[hors], imposees = sous, bruts = B[hors, , drop = FALSE], garder_simples = FALSE),
                     bloc_terme_a_terme(d$x, B[hors, , drop = FALSE], imposees = sous)))
}

cat("Arbres boostés : le tirage de chaque arbre, et les arbres de la fonction C de rpart\n")
# (le tirage d'avant : la boucle de gbm telle quelle)
tirage_boucle <- function(n, k) {
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
verifier("le tirage de k observations parmi n : les mêmes, et le même hasard ensuite", all(vapply(1:40, function(s) {
  n <- 5L + 13L * s
  k <- floor(c(0.1, 0.5, 0.9, 1)[s %% 4 + 1] * n)
  set.seed(s)
  a <- tirage_sac(n, k)
  ua <- stats::runif(1)
  set.seed(s)
  b <- tirage_boucle(n, k)
  identical(a, b) && identical(ua, stats::runif(1))
}, TRUE)))
set.seed(5)
nx <- 300
Xb <- data.frame(x1 = round(stats::runif(nx, 0, 30)), x2 = stats::rnorm(nx), x3 = factor(sample(c("a", "b", "c", "d"), nx, TRUE)), x4 = stats::runif(nx))
eta <- 0.5 * (Xb$x1 > 12) + Xb$x2 * (Xb$x3 %in% c("a", "c"))
ys <- list(gaussienne = eta + stats::rnorm(nx, 0, 0.3), binomiale = stats::rbinom(nx, 1, stats::plogis(eta)), poisson = stats::rpois(nx, exp(eta)))
arbre_c <- arbre_rpart
for (loi in names(ys)) {
  rb <- list(loi = loi, arbres = 120, profondeur = 3, min_noeud = 5, taux = 0.05, fraction = 0.5)
  appels <- 0
  arbre_rpart <- function(...) { appels <<- appels + 1; arbre_c(...) }
  direct <- boosting(Xb, ys[[loi]], seq_len(250), 251:nx, rb, garder = 120, arret = 80)
  arbre_rpart <- function(...) stop("la fonction C de rpart n'est pas appelée")
  par_rpart <- boosting(Xb, ys[[loi]], seq_len(250), 251:nx, rb, garder = 120, arret = 80)
  arbre_rpart <- arbre_c
  verifier(sprintf("loi %s, 120 arbres : ceux de la fonction C (vérifiée sur le premier, qui a ses coupures) = ceux de rpart()", loi),
           appels == 120 && identical(direct, par_rpart))
}

cat("Échantillonnage par distances : les intégrales de la fonction de détection\n")
# (integrale_detection d'avant : g̃ et ses dérivées par detection_distance, en chaque point)
integrale_avant <- function(fd, theta, a, b, derivees = FALSE) {
  valeur <- function(x) {
    v <- poids_detection(fd, x) * detection_distance(fd, theta, x)$gt
    v[!(v >= 1e-16)] <- 0
    v
  }
  a <- rep_len(a, length(b))
  ix <- indices_detection(fd)
  cle_seule <- !length(fd$ordres) && fd$cle != "unif"
  if (cle_seule && fd$cle == "hn") {
    s <- exp(theta[ix$echelle])
    ua <- a^2 / (2 * s^2)
    ub <- b^2 / (2 * s^2)
    I <- if (fd$point) 2 * s^2 * ifelse(ua > 0.5, exp(-ua) - exp(-ub), expm1(-ua) - expm1(-ub)) / fd$w^2
         else s * sqrt(2 * pi) * (stats::pnorm(a / s, lower.tail = FALSE) - stats::pnorm(b / s, lower.tail = FALSE)) / (fd$w - fd$L)
  } else I <- vapply(seq_along(b), function(i) integrer_detection(valeur, a[i], b[i]), 0)
  e0 <- detection_distance(fd, theta, 0, derivees)
  if (!derivees) return(list(I = I, s0 = e0$s0))
  derivee <- function(j) function(x) {
    e <- detection_distance(fd, theta, x, TRUE)
    pd <- poids_detection(fd, x)
    v <- pd * e$gt
    pd * e$dgt[, j] * (!is.na(v) & v >= 1e-16)
  }
  dI <- matrix(0, length(b), ix$q)
  for (j in if (cle_seule) setdiff(seq_len(ix$q), ix$echelle) else seq_len(ix$q)) {
    dI[, j] <- vapply(seq_along(b), function(i) integrer_detection(derivee(j), a[i], b[i]), 0)
  }
  if (cle_seule) {
    k <- function(x) cle_detection(fd, theta, x)$k
    dI[, ix$echelle] <- if (fd$point) 2 * I - 2 * (b^2 * k(b) - a^2 * k(a)) / fd$w^2 else I - (b * k(b) - a * k(a)) / (fd$w - fd$L)
  }
  list(I = I, dI = dI, s0 = e0$s0, ds0 = e0$ds0)
}
fdd <- function(cle, ordres = integer(0), serie = NULL, point = FALSE, L = 0) {
  list(cle = cle, serie = serie, ordres = as.integer(ordres), w = 120, L = L, point = point, echelle_ajustements = "largeur")
}
cas_distance <- list(list("hasard, lignes", fdd("hr"), c(log(2.5), log(30))), list("hasard, points", fdd("hr", point = TRUE), c(log(3), log(40))),
                     list("hasard, tronqué à gauche", fdd("hr", L = 5), c(log(1.5), log(20))), list("demi-normale seule", fdd("hn"), log(30)),
                     list("demi-normale + cos(2, 3)", fdd("hn", c(2, 3), "cos"), c(log(30), 0.2, -0.1)),
                     list("hasard + cos(2)", fdd("hr", 2, "cos"), c(log(2), log(35), 0.1)),
                     list("demi-normale + Hermite(4), points", fdd("hn", 4, "herm", point = TRUE), c(log(25), 0.05)),
                     list("uniforme + cos(1, 2)", fdd("unif", c(1, 2), "cos"), c(0.3, -0.1)))
for (k in cas_distance) {
  bornes <- list(list(k[[2]]$L, k[[2]]$w), list(c(k[[2]]$L, 30, 60), c(30, 60, k[[2]]$w)))
  verifier(sprintf("%s : intégrales et dérivées, de L à w et par intervalles", k[[1]]),
           all(vapply(bornes, function(ab) identical(integrale_detection(k[[2]], k[[3]], ab[[1]], ab[[2]]), integrale_avant(k[[2]], k[[3]], ab[[1]], ab[[2]])) &&
                                             identical(integrale_detection(k[[2]], k[[3]], ab[[1]], ab[[2]], TRUE), integrale_avant(k[[2]], k[[3]], ab[[1]], ab[[2]], TRUE)), TRUE)))
}

cat("TRIM : l'inverse de la corrélation sérielle de chaque site\n")
set.seed(6)
gt <- expand.grid(j = 1:10, i = 1:25)
compte <- stats::rpois(nrow(gt), exp(2 + stats::rnorm(25)[gt$i] + 0.05 * gt$j))
compte[stats::runif(nrow(gt)) < 0.25] <- NA
compte[gt$i <= 6] <- ifelse(gt$j[gt$i <= 6] %in% c(3, 7), NA, pmax(1, compte[gt$i <= 6], na.rm = TRUE))  # (six sites au même motif)
ptr <- projet_depuis_tableau(data.frame(nom = sprintf("r%03d", seq_len(nrow(gt))), site = sprintf("S%02d", gt$i), annee = 2000 + gt$j, compte = compte), "trim.csv")
mtr <- ajuster(ptr, definir_modele(ptr, "TRIM", x = NULL, y = 3, echelle = "none",
                                   options = list(site = list(by = "variable", variable = 1), temps = list(by = "variable", variable = 2), modele = 3)))
tt <- mtr$donnees
Jt <- length(tt$temps)
Rg <- mtr$rho^abs(outer(seq_len(Jt), seq_len(Jt), "-"))
inverses <- inverses_trim(tt, mtr$rho)
verifier(sprintf("ρ = %.3f, %d sites, %d motifs de temps observés : chaque inverse comme calculé pour son site seul", mtr$rho, length(tt$sites),
                 length(unique(apply(!is.na(tt$f), 1, paste, collapse = "")))),
         is.null(inverses_trim(tt, 0)) && all(vapply(seq_along(tt$sites), function(i) {
           o <- which(!is.na(tt$f[i, ]))
           identical(inverses[[i]], chol2inv(chol(Rg[o, o, drop = FALSE])))
         }, TRUE)))

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
