# Vérifie la validation croisée (moteur/R/validation_croisee.R) : chaque groupe est prédit par un
# modèle ajusté sans lui, son prétraitement compris.
#   - elle redonne ce que donnent, à la main, un modèle ajusté sur les autres observations
#     (ajuster) et la prédiction des observations du groupe (predire) ;
#   - sur du bruit pur, le Q2 n'est plus gonflé quand les observations sont peu nombreuses
#     (médiane de Q2cum au plus 0, ACP, PLS et OPLS-DA, ajustement automatique ou A fixé) ; en
#     particulier une PLS de 6 observations à 5 composantes n'a plus un Q2 de 1 ;
#     (une OPLS-DA dont une classe est toute dans un groupe, absente de son sous-modèle, comprise) ;
#   - un modèle ne peut pas avoir plus de composantes que le rang de ses données ;
#   - un vrai signal garde un Q2 élevé (les quatre modèles du projet d'exemple) ;
#   - le test de permutation réajuste tout le modèle à partir du projet ;
#   - les groupes par valeur d'une variable ou d'un identifiant (toutes les observations d'un
#     verger dans le même groupe), ou donnés tels quels, et la pondération par blocs et les
#     modificateurs (pretraitement.R), y compris dans les sous-modèles ;
#   - un terme qui ne varie presque plus sans le groupe caché prend le poids du modèle complet ;
#     validée avec les centres seuls recalculés, ou avec les centres et poids du modèle complet
#     (chaque composante sur les résidus
#     du modèle complet, écrit à nouveau d'après Wold, 1978 ; l'ACP comme Nuée 0.8) ;
#   - le Q2 honnête d'un modèle hiérarchique refait ses modèles de base sans le groupe caché ;
#   - comparer_references() valide comme le fichier et compte le Q2 dans son statut.
#   Rscript moteur/tests/verifier_validation.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}

p <- lire_projet("exemple/vergers.json")
uid <- function(nom) numeros(p, nom)
complets <- uid(c("poids", "diametre", "fermete", "sucre", "acidite", "amidon", "mat_seche", "couleur_a", "couleur_b", "azote", "stockage"))

cat("La validation croisée redonne : un modèle ajusté sans chaque groupe, qui prédit ce groupe (predire)\n")
# PRESS refait à la main, dans les unités du modèle complet (OPLS : sa dernière ligne, toutes les
# composantes orthogonales retirées comme pour une nouvelle observation)
press_a_la_main <- function(p, d) {
  m <- ajuster(p, d)
  obs <- d$observations
  groupes <- groupes_definition(p, d)
  pls <- identical(m$type, "PLS")
  press <- matrix(0, m$A, if (pls) ncol(m$Y) else m$K)
  vers_complet <- function(z, centre, poids) sweep(sweep(z, 2, centre, "-"), 2, poids, "*")
  for (g in sort(unique(groupes))) {
    hors <- groupes == g
    dg <- d
    dg$observations <- obs[!hors]
    mg <- ajuster(p, dg, orienter = FALSE)
    if (pls) {
      for (a in if (isTRUE(m$opls)) m$A else seq_len(m$A)) {
        ma <- mg
        if (!isTRUE(m$opls)) {  # les a premières composantes seulement
          ma$A <- a
          for (k in c("T", "P", "W", "C", "U")) ma[[k]] <- mg[[k]][, seq_len(a), drop = FALSE]
        }
        y <- as.matrix(predire(p, ma, dg, obs[hors])[paste(m$y$noms, "prédit")])
        press[a, ] <- press[a, ] + colSums(((m$Y[hors, , drop = FALSE] - vers_complet(y, m$y$centre, m$y$poids)) * m$MY[hors, , drop = FALSE])^2)
      }
    } else {  # ACP : chaque groupe de termes manquant à son tour
      termes <- groupes_validation(m$K, m$G)
      for (h in sort(unique(termes))) {
        q <- p
        q$donnees[as.character(obs[hors]), as.character(unlist(lapply(d$x[termes == h], `[[`, "variables")))] <- NA
        Tq <- as.matrix(predire(q, mg, dg, obs[hors])[paste0("t", seq_len(m$A))])
        Tq[is.na(Tq)] <- 0
        k <- termes == h
        for (a in seq_len(m$A)) {
          x <- Tq[, seq_len(a), drop = FALSE] %*% t(mg$P[k, seq_len(a), drop = FALSE])
          x <- sweep(sweep(x, 2, ifelse(mg$x$poids[k] > 0, mg$x$poids[k], Inf), "/"), 2, mg$x$centre[k], "+")
          press[a, k] <- press[a, k] + colSums(((m$X[hors, k, drop = FALSE] - vers_complet(x, m$x$centre[k], m$x$poids[k])) * m$MX[hors, k, drop = FALSE])^2)
        }
      }
    }
  }
  lignes <- if (isTRUE(m$opls)) m$A else seq_len(m$A)
  max(abs(press[lignes, ] - m$press[lignes, ])) / max(abs(m$press[lignes, ]))
}
q <- p
q$donnees[c(5, 17, 40), as.character(uid("note_gout"))] <- NA
q$donnees[9, as.character(complets[1:4])] <- NA
q$donnees[c(11, 12), as.character(complets[5])] <- NA
set.seed(3)
onde <- seq(1000, 1600, length.out = 40)
y_s <- runif(30)
df <- data.frame(nom = sprintf("E%02d", 1:30), y = y_s, t(vapply(1:30, function(i) (1 + y_s[i] / 2) * sin(onde / 100 + i / 10) + y_s[i] * exp(-((onde - 1300) / 50)^2) +
                                                              rnorm(40, 0, 0.01), numeric(40))), check.names = FALSE)
names(df)[-(1:2)] <- sprintf("%.0f nm", onde)
s <- projet_depuis_tableau(df, "spectres")
# une classe dont toutes les observations (1 et 8) sont dans le groupe 1 : le sous-modèle sans ce
# groupe n'a pas cette classe (sa colonne de Y y est constante, de poids 0)
set.seed(7)
pk <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:15), classe = c("c", rep(c("a", "b"), 3), "c", rep(c("a", "b"), 3), "a"),
                                       matrix(rnorm(75), 15, 5, dimnames = list(NULL, paste0("V", 1:5))), check.names = FALSE), "classe_groupe")
cas <- list(
  "PLS" = list(p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 3)),
  "PLS à 2 Y, transformations et échelles mêlées" = list(p, definir_modele(p, "PLS", x = complets, y = uid(c("note_gout", "jus")), A = 3,
    transformations = stats::setNames(c("log10", "racine2"), as.character(complets[1:2])), echelle = stats::setNames(c("Par", "Ctr"), as.character(complets[3:4])))),
  "PLS, termes qualitatifs, croisés et carrés" = list(p, definir_modele(p, "PLS", x = c(uid(c("variete", "culture")), complets[1:5]), y = uid("note_gout"), A = 3,
    croises = list(c(uid("variete"), complets[4]), c(complets[4], complets[4])))),
  "PLS-DA" = list(p, definir_modele(p, "PLS-DA", x = complets, classe = uid("variete"), A = 3)),
  "PLS, valeurs manquantes" = list(q, definir_modele(q, "PLS", x = complets, y = uid("note_gout"), A = 3)),
  "PLS, spectres (MSC, dérivée)" = list(s, definir_modele(s, "PLS", x = numeros(s, names(df)[-(1:2)]), y = numeros(s, "y"), A = 2,
    spectres = list(list(type = "msc"), list(type = "sg", derivee = 1, fenetre = 5, degre = 2)))),
  "ACP" = list(p, definir_modele(p, "PCA", x = complets, A = 3)),
  "ACP, valeurs manquantes" = list(q, definir_modele(q, "PCA", x = complets[1:6], A = 2)),
  "OPLS" = list(p, definir_modele(p, "OPLS", x = complets, y = uid("note_gout"), A = 3)),
  "OPLS-DA à 4 classes" = list(p, definir_modele(p, "OPLS-DA", x = complets, classe = uid("variete"), A = 4)),
  "OPLS-DA à 3 classes, dont une toute dans un groupe" = list(pk, definir_modele(pk, "OPLS-DA", x = numeros(pk, paste0("V", 1:5)), classe = numeros(pk, "classe"), A = 3)),
  "PLS, un groupe par verger" = list(p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 3, vc = list(by = "variable", variable = uid("verger")))),
  "PLS, blocs et modificateur, un groupe par verger, valeurs manquantes" = list(q, definir_modele(q, "PLS", x = complets, y = uid("note_gout"), A = 3,
    vc = list(by = "variable", variable = uid("verger")), echelle = stats::setNames("Ctr", as.character(complets[11])),
    blocs = stats::setNames(c("taille", "taille", "gout", "gout", "gout"), as.character(complets[1:5])),
    modificateurs = stats::setNames(c(0.5, 3), as.character(complets[c(4, 11)])))),
  "ACP, un groupe par verger, blocs" = list(p, definir_modele(p, "PCA", x = complets, A = 3, vc = list(by = "variable", variable = uid("verger")),
    blocs = stats::setNames(rep("couleur", 2), as.character(uid(c("couleur_a", "couleur_b")))))),
  "OPLS-DA, un groupe par verger" = list(p, definir_modele(p, "OPLS-DA", x = complets, classe = uid("variete"), A = 3, vc = list(by = "variable", variable = uid("verger")))),
  "PLS, transformations personnalisées, terme à trois facteurs et cube" = list(p, definir_modele(p, "PLS", x = c(uid("variete"), complets[1:6]), y = uid("note_gout"), A = 3,
    transformations = stats::setNames(c("ln:1:1", "puissance:2:1:0.33", "neglog:0.5:2", "lineaire:-1:300", "exp:0.01:0", "logit:0:40"), as.character(complets[1:6])),
    croises = list(c(uid("variete"), complets[1], complets[2]), c(complets[3], complets[3], complets[3])))))
for (k in names(cas)) {
  dk <- cas[[k]][[2]]
  dk$validation <- "sans_fuite"  # (les sous-modèles faits à la main recalculent aussi les poids)
  e <- press_a_la_main(cas[[k]][[1]], dk)
  verifier(sprintf("%s : PRESS du moteur = PRESS à la main (écart relatif %.1g)", k, e), e < 1e-10)
}

cat("Bruit pur : le Q2 n'est plus gonflé avec peu d'observations\n")
bruit <- function(N, graine, K = 20) {
  set.seed(graine)
  X <- matrix(rnorm(N * K), N, K, dimnames = list(NULL, paste0("X", seq_len(K))))
  df <- data.frame(nom = sprintf("O%02d", seq_len(N)), y = rnorm(N), classe = sample(rep(c("a", "b"), length.out = N)), X, check.names = FALSE)
  projet_depuis_tableau(df, "bruit")
}
q2_final <- function(pb, type, A) {
  x <- numeros(pb, paste0("X", 1:20))
  d <- switch(type, PCA = definir_modele(pb, "PCA", x = x, A = A), PLS = definir_modele(pb, "PLS", x = x, y = numeros(pb, "y"), A = A),
              "OPLS-DA" = definir_modele(pb, "OPLS-DA", x = x, classe = numeros(pb, "classe"), A = A))
  if (!d$A) d$A <- max(1, autoajuster(pb, d))  # comme l'application
  m <- ajuster(pb, d)
  c(A = m$A, Q2cum = m$q2cum[m$A])
}
tableau <- NULL
for (type in c("PCA", "PLS", "OPLS-DA")) for (N in c(6, 8, 12, 30)) for (A in c(0, 2)) {
  r <- t(vapply(1:20, function(graine) q2_final(bruit(N, 1000 * N + graine), type, A), c(A = 0, Q2cum = 0)))
  tableau <- rbind(tableau, data.frame(type = type, N = N, A = if (A) "A = 2" else "auto", A_median = stats::median(r[, "A"]),
                                       Q2cum_median = round(stats::median(r[, "Q2cum"]), 3), Q2cum_max = round(max(r[, "Q2cum"]), 3)))
}
print(tableau, row.names = FALSE)
for (type in unique(tableau$type)) {
  t <- tableau[tableau$type == type, ]
  verifier(sprintf("%s : médiane de Q2cum au plus 0 pour N = 6, 8, 12, 30, automatique et A = 2 (la plus haute : %.3f)", type, max(t$Q2cum_median)),
           all(t$Q2cum_median <= 0))
}
pb <- bruit(6, 6)
m <- ajuster(pb, definir_modele(pb, "PLS", x = numeros(pb, paste0("X", 1:20)), y = numeros(pb, "y"), A = 5))
verifier(sprintf("PLS de 6 observations sur du bruit, 5 composantes : Q2cum %.3f < 0 (il valait 1 quand les sous-modèles gardaient l'échelle du modèle complet)", m$q2cum[5]),
         m$q2cum[5] < 0)
verifier("ses sous-modèles (5 observations) n'ont que 4 composantes : les intervalles des coefficients sont manquants, le modèle s'ajuste",
         m$A == 5 && all(is.na(m$statistiques$coefficients[["CoeffCS y IC"]][seq_len(m$K)])) && all(is.finite(m$statistiques$coefficients[["CoeffCS y"]][seq_len(m$K)])))

cat("Pas plus de composantes que le rang des données\n")
set.seed(4)
pr <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:30), y = rnorm(30), matrix(rnorm(120), 30, 4, dimnames = list(NULL, paste0("V", 1:4))),
                                       check.names = FALSE), "rang")
x4 <- numeros(pr, paste0("V", 1:4))
message_5 <- trop_de_composantes(5)
for (type in c("PCA", "PLS")) {
  d <- if (type == "PCA") definir_modele(pr, "PCA", x = x4, A = 6) else definir_modele(pr, "PLS", x = x4, y = numeros(pr, "y"), A = 6)
  e <- tryCatch({ ajuster(pr, d); "ajusté" }, error = function(e) conditionMessage(e))
  verifier(sprintf("%s, 4 variables, 6 composantes demandées : « %s »", type, e), identical(e, message_5))
}
d <- definir_modele(pr, "PCA", x = x4, A = 4)
verifier("ACP, 4 variables, 4 composantes : s'ajuste", ajuster(pr, d)$A == 4)
# 5 termes X de rang 4 (V5 = V1) : l'ajustement automatique d'une OPLS-DA en essaie 5 (1 prédictive,
# 4 orthogonales) et s'arrête à ce que les données permettent
set.seed(5)
V <- matrix(rnorm(120), 30, 4, dimnames = list(NULL, paste0("V", 1:4)))
po <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:30), classe = rep(c("a", "b"), 15), V, V5 = V[, 1], check.names = FALSE), "rang")
do <- definir_modele(po, "OPLS-DA", x = numeros(po, paste0("V", 1:5)), classe = numeros(po, "classe"), A = 0)
ao <- autoajuster(po, do)
verifier(sprintf("OPLS-DA automatique, 5 termes X de rang 4 : %d composante(s), 4 au plus", ao), ao >= 1 && ao <= 4)
do$A <- 5
e <- tryCatch({ ajuster(po, do); "ajusté" }, error = function(e) conditionMessage(e))
verifier(sprintf("la même OPLS-DA à 5 composantes demandées : « %s »", e), identical(e, message_5))

cat("Un vrai signal garde un Q2 élevé : les modèles du projet d'exemple\n")
# avant : la validation croisée sur les matrices du modèle complet (jusqu'à la version 0.9.0)
avant <- list(M1 = c(0.1716, 0.4035, 0.3481, 0.5278), M2 = c(0.6731, 0.6964, 0.7086, 0.7245), M3 = c(0.3048, 0.5850, 0.8946), M4 = c(0.5924, 0.7417))
for (d in p$models) {
  m <- ajuster(p, d)
  cp <- m$statistiques$composantes
  ancien <- avant[[d$name]]
  cat(sprintf("    %s (%s, A = %d) Q2cum par composante : %s ; avant : %s\n", d$name, paste0(if (m$opls) "O" else "", if (m$plsda) "PLS-DA" else m$type), m$A,
              paste(sprintf("%.3f", cp$Q2cum), collapse = " "), paste(sprintf("%.3f", ancien), collapse = " ")))
  verifier(sprintf("%s : Q2cum %.3f, au moins 0,6 et pas plus de 0,05 sous l'ancien au même nombre de composantes", d$name, cp$Q2cum[m$A]),
           cp$Q2cum[m$A] >= 0.6 && all(cp$Q2cum[seq_along(ancien)] >= ancien - 0.05))
}

cat("Test de permutation : tout le modèle réajusté à partir du projet\n")
d <- definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2)
m <- ajuster(p, d)
r <- permutations(p, m, d, n = 4)
verifier("le premier point est le modèle lui-même (R2, Q2)", isTRUE(all.equal(unname(unlist(r$points[1, c("R2", "Q2")])), c(m$statistiques$composantes$R2Ycum[2], m$q2cum[2]))))
verifier(sprintf("Y permuté : Q2 négatif (ordonnée à l'origine de Q2 %.2f < 0,05, celle de R2 %.2f < 0,4)", r$ordonnee_Q2, r$ordonnee_R2),
         all(r$points$Q2[-1] < 0) && r$ordonnee_Q2 < 0.05 && r$ordonnee_R2 < 0.4)
dd <- definir_modele(p, "PLS-DA", x = complets, classe = uid("variete"), A = 2)
rd <- permutations(p, ajuster(p, dd), dd, n = 2)
verifier("PLS-DA : les classes sont permutées ensemble et le modèle réajusté à chaque fois",
         nrow(rd$points) == 3 && all(rd$points$correlation[-1] < 1))

cat("Groupes par valeur d'une variable ou d'un identifiant, ou donnés tels quels\n")
d <- definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2, vc = list(by = "variable", variable = uid("verger")))
g <- groupes_definition(p, d)
verger <- colonne(p, uid("verger"), d$observations)
verifier(sprintf("un groupe par verger : %d groupes, chacun un seul verger, chaque verger un seul groupe", max(g)),
         max(g) == length(unique(verger)) && all(tapply(verger, g, function(v) length(unique(v))) == 1) && all(tapply(g, verger, function(v) length(unique(v))) == 1))
m <- ajuster(p, d)
verifier(sprintf("le modèle compte %d groupes (G) et dit par quoi (« %s »)", m$G, m$vc_par), m$G == max(g) && identical(m$vc_par, "verger"))
# le même verger en identifiant secondaire des observations (comme « site » dans un fichier .usp)
pi <- p
pi$observation_id_names <- c(p$observation_id_names, "lot")
lettres <- as.character(variable_du_projet(p, uid("verger"))$categories)[colonne(p, uid("verger"), vapply(p$observations, function(o) as.numeric(o$uid), 0))]
pi$observations <- Map(function(o, l) { o$ids <- c(as.character(o$ids), l); o }, p$observations, lettres)
di <- definir_modele(pi, "PLS", x = complets, y = uid("note_gout"), A = 2, vc = list(by = "id", id = "lot"))
verifier("par identifiant : les mêmes groupes, le même Q2", identical(groupes_definition(pi, di), g) && isTRUE(all.equal(ajuster(pi, di)$q2cum, m$q2cum)))
dg <- d
dg$groupes_vc <- list(by = "groups", observations = rev(d$observations), groups = rev(10 * g))  # (numéros quelconques)
verifier("donnés tels quels (dans un autre ordre) : les mêmes groupes, le même Q2", identical(groupes_definition(p, dg), g) && isTRUE(all.equal(ajuster(p, dg)$q2cum, m$q2cum)))
dg$observations <- c(d$observations, 999)
e <- tryCatch({ groupes_definition(p, dg); "" }, error = function(e) conditionMessage(e))
verifier(sprintf("une observation sans groupe : « %s »", substr(e, 1, 60)), grepl("groupes de ce modèle viennent du fichier", e))
qn <- p
qn$donnees[c(3, 30), as.character(uid("verger"))] <- NA
gn <- groupes_definition(qn, definir_modele(qn, "PLS", x = complets, y = uid("note_gout"), A = 2, vc = list(by = "variable", variable = uid("verger"))))
verifier("les observations sans valeur forment un groupe à part", gn[3] == gn[30] && sum(gn == gn[3]) == 2 && max(gn) == max(g) + 1)
seul <- d$observations[verger == verger[1]]
e <- tryCatch({ definir_modele(p, "PLS", x = complets, y = uid("note_gout"), observations = seul, vc = list(by = "variable", variable = uid("verger"))); "" },
              error = function(e) conditionMessage(e))
verifier(sprintf("un seul verger parmi les observations : « %s »", substr(e, 1, 60)), grepl("un seul groupe", e))
e <- tryCatch({ definir_modele(p, "PLS", x = complets, y = uid("note_gout"), vc = list(by = "id", id = "site")); "" }, error = function(e) conditionMessage(e))
verifier(sprintf("un identifiant absent du projet : « %s »", e), grepl("pas d'identifiant « site »", e))
verifier("sans réglage (ou « order ») : à tour de rôle en G groupes", identical(groupes_definition(p, definir_modele(p, "PCA", x = complets, G = 5)), groupes_validation(120, 5)))
mc <- modeles_classe(p, definir_modele(p, "PCA", x = complets, A = 2, vc = list(by = "variable", variable = uid("verger"))), uid("variete"))
verifier("modèles de classe groupés par verger : une classe d'un seul verger est validée à tour de rôle, les autres par verger",
         length(mc) == 4 && all(vapply(mc, function(e) {
           un_seul <- length(unique(colonne(p, uid("verger"), e$observations))) == 1
           if (un_seul) is.null(e$groupes_vc) && ajuster(p, e)$G == 7 else identical(e$groupes_vc$by, "variable")
         }, TRUE)))
mc <- modeles_classe(p, definir_modele(p, "PCA", x = complets, A = 2, vc = list(by = "variable", variable = uid("variete"))), uid("variete"))
verifier("groupés par la variable des classes : chaque classe n'a qu'une valeur, elle est validée à tour de rôle (au lieu d'une erreur)",
         length(mc) == 4 && all(vapply(mc, function(e) is.null(e$groupes_vc) && ajuster(p, e)$G == 7, TRUE)))

cat("Pondération par blocs et modificateurs : poids UV × modificateur × 1/racine(K)\n")
blocs <- stats::setNames(c("taille", "taille", "gout", "gout", "gout"), as.character(complets[1:5]))
db <- definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2, blocs = blocs,
                     modificateurs = stats::setNames(c(0.5, 3), as.character(complets[c(4, 11)])), echelle = stats::setNames("Ctr", as.character(complets[11])))
mb <- ajuster(p, db)
et <- apply(sapply(complets, function(u) colonne(p, u, db$observations)), 2, sd, na.rm = TRUE)
attendu <- 1 / et * c(1 / sqrt(c(2, 2, 3)), 0.5 / sqrt(3), 1 / sqrt(3), rep(1, 5), 3 * et[11])
verifier("le poids de chaque terme", isTRUE(all.equal(unname(mb$x$poids), unname(attendu))) && isTRUE(all.equal(mb$x$facteur, unname(attendu * c(et[-11], 1)))))
verifier("un bloc de 3 termes UV pèse autant qu'une variable : variance totale 1, au lieu de 3",
         isTRUE(all.equal(sum(apply(mb$X[, c(3, 5)], 2, var)) + var(mb$X[, 4]) / 0.25, 1)))
pr <- predire(p, mb, db, db$observations[1:5])
verifier("une prédiction projette avec les mêmes poids : elle redonne les scores du modèle",
         isTRUE(all.equal(unname(as.matrix(pr[c("t1", "t2")])), unname(mb$T[1:5, ]))))
r <- reglages_modele(p, db)
verifier("Nouveau modèle depuis celui-ci garde les blocs et les modificateurs",
         identical(unlist(r$blocs), blocs) && isTRUE(all.equal(unlist(r$modificateurs), stats::setNames(c(0.5, 3), as.character(complets[c(4, 11)])))))
r <- reglages_modele(p, d)
verifier("… et les groupes de la validation croisée", identical(r$vc, d$groupes_vc))
dq <- definir_modele(p, "PLS-DA", x = uid(c("culture", "poids")), classe = uid("variete"), A = 1, blocs = stats::setNames("b", as.character(uid("culture"))))
verifier("une variable qualitative dans un bloc : chacune de ses catégories compte (1/racine(2) pour 2)",
         isTRUE(all.equal(ajuster(p, dq)$x$facteur, c(1 / sqrt(2), 1 / sqrt(2), 1))))

cat("Transformations personnalisées et termes à trois facteurs (Nouveau modèle)\n")
x <- c(-1, 0, 0.5, 2, 39.9, 40, 100, NA)
formules <- suppressWarnings(list(
  "ln:2:1" = ifelse(2 * x + 1 > 0, log(2 * x + 1), NA), "log10:3" = ifelse(x + 3 > 0, log10(x + 3), NA), "log10:2:0" = ifelse(2 * x > 0, log10(2 * x), NA),
  "neglog:1:2" = ifelse(x + 2 > 0, -log10(x + 2), NA), "exp:0.5:-1" = exp(0.5 * x - 1), "lineaire:-2:5" = -2 * x + 5,
  "logit:0:40" = ifelse(x > 0 & x < 40, log(x / (40 - x)), NA), "puissance:1:1:0.5" = ifelse(x + 1 >= 0, sqrt(x + 1), NA),
  "puissance:3" = x^3, "racine4" = ifelse(x >= 0, x^0.25, NA)))
ok <- vapply(names(formules), function(k) isTRUE(all.equal(suppressWarnings(transformer(x, lire_transformation(k))), formules[[k]])), TRUE)
verifier(sprintf("chaque transformation donne sa formule, hors domaine manquante (%s)", paste(names(formules)[!ok], collapse = ", ")), all(ok))
codes <- c("racine2", "racine4", "log10", "log10:1", "ln:1:1", "log10:2:0.5", "neglog:1:0", "exp:0.01:0", "lineaire:-1:300", "logit:0:100", "puissance:2:1:0.33")
retour <- vapply(codes, function(k) nom_transformation(lire_transformation(k)), "")
verifier("le nom de chaque transformation la relit telle quelle (Nouveau modèle depuis un modèle)", identical(unname(retour), codes))
verifier("une puissance enregistrée dans un fichier .usp a maintenant un nom : elle reste dans le modèle refait",
         identical(nom_transformation(list(type = 7, params = c(-99, 1, 0, 0.33))), "puissance:1:0:0.33") && is.null(nom_transformation(list(type = 7, params = c(-99, 1, 0, 1)))))
e <- tryCatch({ lire_transformation("logit:5:1"); "" }, error = function(e) conditionMessage(e))
verifier(sprintf("un logit aux bornes à l'envers : « %s »", e), grepl("borne basse", e))
e <- tryCatch({ lire_transformation("ln:a:1"); "" }, error = function(e) conditionMessage(e))
verifier(sprintf("une transformation mal écrite : « %s »", e), grepl("Transformation inconnue", e))
d3 <- definir_modele(p, "PLS", x = c(uid("variete"), complets[1:3]), y = uid("note_gout"), A = 2,
                     croises = list(c(uid("variete"), complets[1], complets[2]), c(complets[3], complets[3], complets[3])))
noms3 <- vapply(d3$x, `[[`, "", "name")
verifier(sprintf("A × B × C (une variable qualitative par ses catégories) et un cube : %d termes croisés", sum(lengths(lapply(d3$x, `[[`, "variables")) == 3)),
         all(c("variete(Gala)*poids*diametre", "fermete*fermete*fermete") %in% noms3) && sum(lengths(lapply(d3$x, `[[`, "variables")) == 3) == 5)
m3 <- ajuster(p, d3)
attendu <- m3$x$X[, "variete(Gala)"] * m3$x$X[, "poids"] * m3$x$X[, "diametre"]
verifier("un terme à trois facteurs est le produit de ses facteurs mis à l'échelle, mis à l'échelle à son tour",
         isTRUE(all.equal(unname(m3$x$X[, "variete(Gala)*poids*diametre"]), unname((attendu - mean(attendu)) / sd(attendu)))))
verifier("Nouveau modèle depuis celui-ci garde les termes à trois facteurs", length(reglages_modele(p, d3)$croises) == 2 && all(lengths(reglages_modele(p, d3)$croises) == 3))
# l'échelle, le bloc et le modificateur d'un terme croisé (ceux d'un fichier .usp aussi) : gardés
# par Nouveau modèle depuis celui-ci, qui redonne le même modèle
dc <- definir_modele(p, "PLS", x = complets[1:3], y = uid("note_gout"), A = 2, croises = list(complets[1:2], complets[c(3, 3)]),
                     croises_reglages = stats::setNames(list(list(echelle = "Par", bloc = "C", modificateur = 0.5)), paste(complets[1:2], collapse = "*")))
rc <- reglages_modele(p, dc)
dc2 <- definir_modele(p, "PLS", x = rc$x, y = rc$y, A = 2, croises = rc$croises, croises_reglages = rc$croises_reglages)
echelles_croises <- function(d) lapply(Filter(function(t) length(t$variables) > 1, d$x), `[[`, "scaling")
verifier("Nouveau modèle depuis celui-ci garde l'échelle, le bloc et le modificateur des termes croisés",
         identical(echelles_croises(dc)[[1]], list(type = "Par", block = "C", modifier = 0.5)) && identical(echelles_croises(dc2), echelles_croises(dc)) &&
           isTRUE(all.equal(ajuster(p, dc2)$x$X, ajuster(p, dc)$x$X)))
e <- tryCatch({ definir_modele(p, "PLS", x = c(uid("variete"), complets[1]), y = uid("note_gout"), croises = list(c(uid("variete"), complets[1], uid("variete")))); "" },
              error = function(e) conditionMessage(e))
verifier(sprintf("une variable qualitative deux fois dans un terme : « %s »", e), grepl("qualitative", e))

cat("Trois façons de valider : sans fuite (garde-fou des termes presque constants), centres seuls, centres et poids du modèle complet\n")
# Six sites de six ans ; « rare » ne varie que sur le site S6 (ailleurs 1, au millième près), comme
# a_nbjtx0 sans le site 208 d'un projet réel (0,5 % de son écart-type) ; validation par site.
set.seed(11)
site <- rep(sprintf("S%d", 1:6), each = 6)
Xr <- matrix(rnorm(36 * 4), 36, 4, dimnames = list(NULL, paste0("x", 1:4)))
rare <- ifelse(site == "S6", rnorm(36, 3, 1), 1 + rnorm(36, 0, 1e-3))
pr <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:36), site = site, y = Xr[, 1] + 0.5 * Xr[, 2] + 0.3 * rare + rnorm(36, 0, 0.5),
                                       Xr, rare = rare, check.names = FALSE), "rare")
pr$donnees[c("3", "20"), as.character(numeros(pr, "x2"))] <- NA
par_site <- list(by = "variable", variable = numeros(pr, "site"))
xr <- numeros(pr, c(paste0("x", 1:4), "rare"))
dr <- definir_modele(pr, "PLS", x = xr, y = numeros(pr, "y"), A = 2, vc = par_site)
verifier("par défaut, un modèle est validé avec les centres recalculés et les poids du modèle complet",
         identical(mode_validation(dr), "centres") && identical(ajuster(pr, dr)$validation, "centres"))
dr$validation <- "sans_fuite"
mr <- ajuster(pr, dr)
gr <- gardes_vc(pr, dr, mr)
verifier(sprintf("sans fuite : sans S6, « rare » garde %.2g %% de son écart-type et prend le poids du modèle complet (le seul terme)",
                 100 * gr$part[1]), identical(mr$validation, "sans_fuite") &&
           nrow(gr) == 1 && identical(gr$terme, "rare") && identical(gr$groupe, "S6") && gr$part < 0.01)
# à la main : un modèle ajusté sans chaque site (le poids d'un terme imposé quand il le faut,
# comme une échelle enregistrée), qui prédit ce site (predire)
press_impose <- function(p, d, m, imposer) {
  obs <- d$observations
  g <- groupes_definition(p, d)
  press <- matrix(0, m$A, ncol(m$Y))
  for (h in sort(unique(g))) {
    dg <- imposer(d, g != h)
    dg$observations <- obs[g != h]
    mg <- ajuster(p, dg, orienter = FALSE)
    for (a in seq_len(m$A)) {
      ma <- mg
      ma$A <- a
      for (k in c("T", "P", "W", "C", "U")) ma[[k]] <- mg[[k]][, seq_len(a), drop = FALSE]
      y <- as.matrix(predire(p, ma, dg, obs[g == h])[paste(m$y$noms, "prédit")])
      press[a, ] <- press[a, ] + colSums(((m$Y[g == h, , drop = FALSE] - sweep(sweep(y, 2, m$y$centre), 2, m$y$poids, "*")) * m$MY[g == h, , drop = FALSE])^2)
    }
  }
  max(abs(press - m$press)) / max(abs(m$press))
}
# un terme au centre des lignes d'apprentissage et au poids du modèle complet (échelle enregistrée)
au_poids_complet <- function(p, d, garde, k) {
  x <- colonne(p, d$x[[k]]$variables, d$observations[garde])
  d$x[[k]]$scaling <- list(centre = mean(x, na.rm = TRUE), weight = 1 / sd(colonne(p, d$x[[k]]$variables, d$observations), na.rm = TRUE))
  d
}
k_rare <- which(vapply(dr$x, `[[`, "", "name") == "rare")
e <- press_impose(pr, dr, mr, function(d, garde) if (!garde[length(garde)]) au_poids_complet(pr, d, garde, k_rare) else d)
verifier(sprintf("sans fuite : PRESS du moteur = PRESS à la main, « rare » au poids du modèle complet sans S6 (écart relatif %.1g)", e), e < 1e-10)
ancien <- SEUIL_ECART_VC
SEUIL_ECART_VC <- 0
m0 <- ajuster(pr, dr)
SEUIL_ECART_VC <- ancien
verifier(sprintf("sans le garde-fou, le Q2 explose (%.3g) ; avec, il reste sensé (%.3f)", m0$q2cum[2], mr$q2cum[2]), m0$q2cum[2] < -5 && mr$q2cum[2] > -1)
dc <- dr
dc$validation <- "centres"
mc <- ajuster(pr, dc)
e <- press_impose(pr, dc, mc, function(d, garde) { for (k in seq_along(d$x)) d <- au_poids_complet(pr, d, garde, k); d })
verifier(sprintf("centres seuls : chaque terme au centre des autres sites et au poids du modèle complet (écart relatif %.1g), sans terme signalé", e),
         e < 1e-10 && is.null(mc$gardes) && identical(reglages_modele(pr, dc)$validation, "centres"))

# Centres et poids du modèle complet, écrit à nouveau d'après Wold (1978) : chaque composante validée sur les résidus du
# modèle complet, une composante ajustée sans chaque groupe ; Q2(cum) = 1 - produit des (1 - Q2),
# chacun ramené à -0,1 au plus bas
q2_complet_main <- function(m, groupes) {
  E <- m$X; ME <- m$MX; F <- m$Y; MF <- m$MY
  q2 <- numeric(m$A)
  q2v <- matrix(0, m$A, ncol(F))
  for (a in seq_len(m$A)) {
    press <- 0
    for (g in sort(unique(groupes))) {
      h <- groupes == g
      k <- composante_pls(E[!h, , drop = FALSE], ME[!h, , drop = FALSE], F[!h, , drop = FALSE], MF[!h, , drop = FALSE])
      press <- press + colSums(((F[h, , drop = FALSE] - outer(projeter(E[h, , drop = FALSE], ME[h, , drop = FALSE], k$w), k$c)) * MF[h, , drop = FALSE])^2)
    }
    q2[a] <- 1 - sum(press) / sum(F^2)
    q2v[a, ] <- 1 - press / colSums(F^2)
    t0 <- ifelse(is.na(m$T[, a]), 0, m$T[, a])
    E <- (E - outer(t0, m$P[, a])) * ME
    F <- (F - outer(t0, m$C[, a])) * MF
  }
  list(q2 = q2, cum = 1 - cumprod(1 - pmax(q2, -0.1)), cumv = 1 - apply(1 - pmax(q2v, -0.1), 2, prod))
}
for (k in c("PLS, valeurs manquantes", "PLS à 2 Y, transformations et échelles mêlées", "PLS-DA", "PLS, blocs et modificateur, un groupe par verger, valeurs manquantes")) {
  pk <- cas[[k]][[1]]
  dk <- cas[[k]][[2]]
  dk$validation <- "complet"
  mk <- ajuster(pk, dk)
  attendu <- q2_complet_main(mk, groupes_definition(pk, dk))
  ecart <- max(abs(c(mk$q2 - attendu$q2, mk$q2cum - attendu$cum, mk$statistiques$variables_y$Q2V_cum - attendu$cumv)))
  verifier(sprintf("modèle complet, %s : Q2, Q2(cum) et Q2V(cum) de l'article (écart %.1g)", k, ecart), ecart < 1e-10)
}
ds <- dr
ds$validation <- "complet"
ms <- ajuster(pr, ds)
# le Y prédit en validation croisée (RMSEcv, CV-ANOVA, classement) : celui de sous-modèles ajustés
# sur les autres lignes des matrices du modèle complet, avec A composantes
gs <- groupes_definition(pr, ds)
attendu <- matrix(NA_real_, nrow(ms$Y), ncol(ms$Y))
for (h in sort(unique(gs))) {
  i <- gs == h
  sm <- ajuster_matrices(list(E = ms$X[!i, , drop = FALSE], M = ms$MX[!i, , drop = FALSE]), list(E = ms$Y[!i, , drop = FALSE], M = ms$MY[!i, , drop = FALSE]), ms$A)
  E <- ms$X[i, , drop = FALSE]
  M <- ms$MX[i, , drop = FALSE]
  y <- 0
  for (a in seq_len(ms$A)) {  # (comme une nouvelle observation, prediction.R)
    th <- projeter(E, M, sm$W[, a])
    E <- (E - outer(th, sm$P[, a])) * M
    y <- y + outer(th, sm$C[, a])
  }
  attendu[i, ] <- y
}
verifier("modèle complet : le Y prédit en validation croisée vient de sous-modèles ajustés sans chaque groupe, sur les matrices du modèle complet",
         isTRUE(all.equal(unname(ms$vc$predictions[[ms$A]]), unname(attendu))))
# sur du bruit pur, ces prédictions ne valent pas mieux que la moyenne : la CV-ANOVA n'est que
# rarement significative, et une PLS-DA de classes tirées au hasard classe au hasard
significatifs <- 0
bien_classes <- numeric(0)
for (graine in 1:20) {
  set.seed(500 + graine)
  pb <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:24), y = rnorm(24), classe = rep(c("a", "b"), 12),
                                         matrix(rnorm(24 * 40), 24, 40, dimnames = list(NULL, paste0("X", 1:40))), check.names = FALSE), "bruit")
  x <- numeros(pb, paste0("X", 1:40))
  m1 <- ajuster(pb, definir_modele(pb, "PLS", x = x, y = numeros(pb, "y"), A = 3, validation = "complet"))
  significatifs <- significatifs + (m1$statistiques$cv_anova[[1]]$p[2] < 0.05)
  m2 <- ajuster(pb, definir_modele(pb, "PLS-DA", x = x, classe = numeros(pb, "classe"), A = 3, validation = "complet"))
  bien_classes <- c(bien_classes, m2$statistiques$classification$vc$global)
}
verifier(sprintf("modèle complet, bruit pur (20 jeux) : CV-ANOVA significative %d fois, classement en validation croisée %.0f %% en médiane",
                 significatifs, 100 * stats::median(bien_classes)), significatifs <= 3 && stats::median(bien_classes) < 0.7)
du <- definir_modele(pr, "PLS", x = xr, y = numeros(pr, "y"), A = 2, vc = par_site)
du$reference <- list(components = list(list(`R2X(cum)` = 0.5)))
verifier("un modèle venu d'un fichier .usp est validé comme les autres par défaut (centres recalculés), sauf s'il en choisit une autre façon",
         identical(mode_validation(du), "centres") && identical(mode_validation(c(du, list(validation = "complet"))), "complet"))
# l'ACP avec les centres et poids du modèle complet : la validation de Nuée 0.8 (Krzanowski, 1987 : p sans le groupe
# d'observations, t sans le groupe de termes), écrite à nouveau
press_acp_main <- function(m, groupes, groupes_var) {
  E <- m$X; ME <- m$MX
  press <- matrix(0, m$A, ncol(E))
  for (a in seq_len(m$A)) {
    complet <- list(t = ifelse(is.na(m$T[, a]), 0, m$T[, a]), p = m$P[, a])
    P <- matrix(0, ncol(E), nrow(E))
    Tc <- matrix(0, nrow(E), ncol(E))
    for (g in sort(unique(groupes))) {
      h <- groupes == g
      k <- composante_acp(E[!h, , drop = FALSE], ME[!h, , drop = FALSE])
      P[, h] <- k$p * sign(sum(k$p * complet$p))
    }
    for (g in sort(unique(groupes_var))) {
      cases <- groupes_var == g
      k <- composante_acp(E[, !cases, drop = FALSE], ME[, !cases, drop = FALSE])
      Tc[, cases] <- k$t * sign(sum(k$t * complet$t))
    }
    press[a, ] <- colSums(((E - Tc * t(P)) * ME)^2)
    E <- (E - outer(complet$t, complet$p)) * ME
  }
  press
}
for (k in c("ACP", "ACP, valeurs manquantes")) {
  dk <- cas[[k]][[2]]
  dk$validation <- "complet"
  mk <- ajuster(cas[[k]][[1]], dk)
  e <- max(abs(mk$press - press_acp_main(mk, groupes_definition(cas[[k]][[1]], dk), groupes_validation(mk$K, mk$G)))) / max(mk$press)
  verifier(sprintf("modèle complet, %s : la validation de Nuée 0.8 (écart relatif %.1g)", k, e), e < 1e-10)
}
# l'OPLS avec les centres et poids du modèle complet : chaque sous-modèle ajusté sur les lignes des matrices du modèle complet (déjà
# centrées et mises à l'échelle), toutes ses composantes orthogonales retirées pour prédire ; sur 12
# observations, ce Q2 s'écarte nettement de celui sans fuite
for (k in c("OPLS", "OPLS-DA à 4 classes")) {
  pk <- cas[[k]][[1]]
  dk <- cas[[k]][[2]]
  classe <- colonne(pk, uid("variete"), dk$observations)
  dk$observations <- dk$observations[unlist(lapply(split(seq_along(classe), classe), `[`, 1:3))]  # (3 de chaque variété)
  dk$validation <- "complet"
  mk <- ajuster(pk, dk)
  g <- groupes_definition(pk, dk)
  press <- 0
  for (h in sort(unique(g))) {
    i <- g == h
    nk <- opls_noyau(mk$X[!i, , drop = FALSE], mk$MX[!i, , drop = FALSE], mk$Y[!i, , drop = FALSE], mk$MY[!i, , drop = FALSE], mk$A - mk$Ap, mk$Ap, jusqua = TRUE)
    y <- opls_predire(nk, mk$X[i, , drop = FALSE], mk$MX[i, , drop = FALSE], min(mk$A - mk$Ap, ncol(nk$Wo) %||% 0), min(mk$Ap, length(nk$pred)))
    press <- press + colSums(((mk$Y[i, , drop = FALSE] - y) * mk$MY[i, , drop = FALSE])^2)
  }
  sf <- cas[[k]][[2]]
  sf$observations <- dk$observations
  e <- max(abs(press - mk$press[mk$A, ])) / max(press)
  verifier(sprintf("modèle complet, %s sur 12 observations (3 par variété) : sous-modèles sur les matrices du modèle complet (écart relatif %.1g) ; Q2cum %.3f, par défaut %.3f",
                   k, e, mk$q2cum[mk$A], ajuster(pk, sf)$q2cum[mk$A]), e < 1e-10 && identical(mk$validation, "complet"))
}

cat("Modèle hiérarchique : le Q2 honnête refait les modèles de base sans le groupe caché\n")
db <- definir_modele(pr, "PLS", x = numeros(pr, paste0("x", 1:4)), y = numeros(pr, "y"), A = 2, vc = par_site, nom = "B1")
db$fitted <- TRUE
ph <- pr
ph$models <- list(db)
sv <- scores_en_variables(ph, ajuster(ph, db))
ph <- sv$projet
dh <- definir_modele(ph, "PLS", x = c(numeros(ph, sv$noms), numeros(ph, "rare")), y = numeros(ph, "y"), A = 1, vc = par_site)
mh <- ajuster(ph, dh)
h <- mh$hierarchique
verifier(sprintf("son modèle de base est retrouvé par le nom de ses scores (%s) ; Q2 honnête %.3f, Q2 aux scores tels quels %.3f",
                 paste(sv$noms, collapse = ", "), h$q2cum[1], mh$q2cum[1]), identical(as.character(h$bases), "B1") && is.finite(h$q2cum[1]) &&
           identical(h$validation, "centres") && !isTRUE(all.equal(h$q2cum, mh$q2cum)))
gh <- groupes_definition(ph, dh)
p1 <- projet_sans_groupe(ph, dh, gh == 1, modeles_de_base(ph, dh)$bases)
d1 <- db
d1$observations <- db$observations[gh != 1]
t1 <- predire(ph, ajuster(ph, d1, orienter = FALSE), d1, db$observations)$t1
v1 <- p1$donnees[as.character(db$observations), as.character(numeros(ph, sv$noms[1]))]
verifier("dans un tour, ses scores sont ceux d'un modèle de base ajusté sans le groupe caché, pour toutes les observations (au signe près)",
         isTRUE(all.equal(abs(unname(v1)), abs(unname(t1)))))
ph2 <- ph
ph2$models <- list()
h2 <- ajuster(ph2, dh)$hierarchique
verifier("sans son modèle de base dans le projet, ses scores sont pris tels quels, et le résumé le dit", is.null(h2$q2cum) && identical(sort(as.character(h2$absents)), sort(sv$noms)))
verifier("un modèle sans score d'un autre modèle n'a pas de Q2 honnête", is.null(mr$hierarchique))
# un modèle de base retrouvé deux fois (directement, par sa 2e composante, et par un autre modèle
# de base, par sa 1re) : ses deux variables sont refaites, quel que soit l'ordre des termes
db2 <- definir_modele(ph, "PLS", x = c(numeros(ph, sv$noms[1]), numeros(ph, "rare")), y = numeros(ph, "y"), A = 1, vc = par_site, nom = "B2")
db2$fitted <- TRUE
ph3 <- ph
ph3$models <- list(db, db2)
sv2 <- scores_en_variables(ph3, ajuster(ph3, db2))
ph3 <- sv2$projet
haut <- function(ordre) definir_modele(ph3, "PLS", x = numeros(ph3, c(sv$noms[2], sv2$noms[1])[ordre]), y = numeros(ph3, "y"), A = 1, vc = par_site)
b1 <- modeles_de_base(ph3, haut(1:2))$bases
b2 <- modeles_de_base(ph3, haut(2:1))$bases
q_1 <- ajuster(ph3, haut(1:2))$hierarchique$q2cum
q_2 <- ajuster(ph3, haut(2:1))$hierarchique$q2cum
verifier(sprintf("un modèle de base retrouvé deux fois : ses deux composantes sont refaites, B1 avant B2, dans les deux ordres (Q2 honnête %.4f et %.4f)", q_1, q_2),
         identical(names(b1), c("B1", "B2")) && identical(names(b2), c("B1", "B2")) && length(b1$B1$variables) == 2 && length(b2$B1$variables) == 2 &&
           isTRUE(all.equal(q_1, q_2)))
dacp <- definir_modele(ph, "PCA", x = c(numeros(ph, sv$noms), numeros(ph, "rare")), A = 1, vc = par_site)
verifier("une ACP sur des scores n'a pas de Q2 honnête (pas de Y dont ses scores se serviraient)", is.null(ajuster(ph, dacp)$hierarchique))

cat("W* et coefficients : les scores des observations complètes sont X W*, avec ou sans valeurs manquantes (PLS, OPLS)\n")
pm <- p
pm$donnees[c(3, 8, 15, 21), as.character(uid("sucre"))] <- NA
pm$donnees[c(5, 8, 30), as.character(uid("fermete"))] <- NA
for (type in c("PLS", "OPLS")) {
  for (manquants in c(FALSE, TRUE)) {
    pp <- if (manquants) pm else p
    m <- ajuster(pp, definir_modele(pp, type, x = complets, y = uid("note_gout"), A = 3))
    cf <- coefficients_pls(m$W, m$P, m$C, ordre_retrait(m))
    comp <- rowSums(m$MX) == ncol(m$MX)
    ecart <- max(abs(m$X[comp, , drop = FALSE] %*% cf$Wetoile - m$T[comp, , drop = FALSE]))
    pareil <- isTRUE(all.equal(unname(m$statistiques$coefficients[["CoeffCS note_gout"]][seq_len(m$K)]), unname(cf$coef_cs[, 1])))
    if (manquants) {
      verifier(sprintf("%s, valeurs manquantes : T = X W* pour les %d observations complètes (écart %.1e), et les coefficients du résumé en viennent", type, sum(comp), ecart),
               ecart < 1e-8 && pareil && sum(!comp) > 0)
    } else {
      inverse <- m$W %*% solve(crossprod(m$P, m$W))
      verifier(sprintf("%s, données complètes : T = X W* (écart %.1e), et W* = W (P'W)^-1 comme avant (écart %.1e)", type, ecart, max(abs(cf$Wetoile - inverse))),
               ecart < 1e-8 && max(abs(cf$Wetoile - inverse)) < 1e-8 && pareil)
    }
  }
}

cat("Le Q2 du modèle vide et le gain sur lui : chaque groupe caché prédit par la moyenne des autres\n")
par_verger <- list(by = "variable", variable = uid("verger"))
vide_a_la_main <- function(pp, d, noms_y) {
  g <- groupes_definition(pp, d)
  Y <- sapply(noms_y, function(n) colonne(pp, uid(n), d$observations))
  w <- 1 / apply(Y, 2, stats::sd)  # (UV : les poids du modèle complet)
  press0 <- sum(sapply(unique(g), function(k) sum(sweep(sweep(Y[g == k, , drop = FALSE], 2, colMeans(Y[g != k, , drop = FALSE])), 2, w, "*")^2)))
  1 - press0 / sum(sweep(sweep(Y, 2, colMeans(Y)), 2, w, "*")^2)
}
for (cas in list(list("PLS", "note_gout", "centres"), list("PLS", "note_gout", "sans_fuite"), list("OPLS", "note_gout", "centres"),
                 list("PLS", c("note_gout", "jus"), "centres"))) {
  dv <- definir_modele(p, cas[[1]], x = complets, y = uid(cas[[2]]), A = 2, vc = par_verger, validation = cas[[3]])
  m <- ajuster(p, dv)
  ref <- vide_a_la_main(p, dv, cas[[2]])
  gain <- (utils::tail(m$q2cum, 1) - ref) / (1 - ref)
  verifier(sprintf("%s de %s, %s : Q2 du modèle vide %.4f (à la main %.4f), gain %.4f (à la main %.4f)", cas[[1]], paste(cas[[2]], collapse = " et "), cas[[3]], m$q2_vide, ref, m$gain, gain),
           isTRUE(all.equal(m$q2_vide, ref)) && isTRUE(all.equal(m$gain, gain)) && ref < 0)
}
mc <- ajuster(p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2, vc = par_verger, validation = "complet"))
ma <- ajuster(p, definir_modele(p, "PCA", x = complets, A = 2, vc = par_verger))
verifier("avec les centres et poids du modèle complet, et pour une ACP : ni Q2 du modèle vide ni gain", is.null(mc$q2_vide) && is.null(mc$gain) && is.null(ma$q2_vide) && is.null(ma$gain))
hh <- ajuster(ph, dh)$hierarchique
verifier(sprintf("le Q2 honnête d'un hiérarchique a son gain sur le modèle vide (%.4f, Q2 honnête %.4f, modèle vide %.4f)", hh$gain, utils::tail(hh$q2cum, 1), hh$q2_vide),
         isTRUE(all.equal(hh$gain, (utils::tail(hh$q2cum, 1) - hh$q2_vide) / (1 - hh$q2_vide))))

cat("comparer_references() : validé comme le fichier, le Q2 compte dans le statut\n")
dref <- ds
dref$validation <- "sans_fuite"  # (le modèle du projet est validé sans fuite : la comparaison le valide comme le fichier)
dref$fitted <- TRUE
cs <- resume_composantes(ms)
dref$reference <- list(components = lapply(seq_len(ms$A), function(a) list(`R2X(cum)` = cs$R2Xcum[a], `R2Y(cum)` = cs$R2Ycum[a], `Q2(cum)` = ms$q2cum[a])))
pref <- pr
pref$models <- list(dref)
r1 <- comparer_references(pref)
pref$models[[1]]$reference$components[[2]][["Q2(cum)"]] <- ms$q2cum[2] + 0.05
r2 <- comparer_references(pref)
verifier(sprintf("le même Q2 que le fichier : « %s » ; 0,05 de plus dans le fichier : « %s » (écart de Q2 %s)", r1$statut, r2$statut, r2$Q2),
         identical(r1$statut, "identique") && identical(r2$statut, "différent") && isTRUE(all.equal(r2$Q2, 0.05)))

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
