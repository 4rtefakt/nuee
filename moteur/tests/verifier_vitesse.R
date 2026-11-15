# Vérifie que les raccourcis de calcul de la 1.0 (la vitesse sur les grands tableaux et les spectres)
# rendent exactement ce que rend le calcul qu'ils évitent (identical() avec le BLAS de référence, celui
# de R sous Windows ; à 1e-12 près avec un BLAS optimisé, OpenBLAS ou MKL, qui ne fait pas ses sommes
# dans le même ordre selon le produit) :
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
#     chaque terme un à un.
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

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
