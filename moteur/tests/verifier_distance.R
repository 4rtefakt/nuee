# Vérifie l'échantillonnage par distances (moteur/R/distance.R) : contre des calculs directs (la
# quadrature contre integrate(), les scores contre des différences finies, la demi-normale contre sa
# vraisemblance écrite avec pnorm(), p et sa variance, les polynômes d'Hermite, les conditions de
# Karush–Kuhn–Tucker d'un ajustement sous contrainte de monotonie, la densité et sa variance écrites à
# la main, Cramér–von Mises et le khi² à la main, les unités), et, quand ils sont installés
# (NUEE_DISTANCE=<bibliothèque> les désigne), contre Distance et mrds : le modèle choisi par l'AIC et
# l'AIC de chaque candidat, la log-vraisemblance et les paramètres, puis — les paramètres de Nuée mis
# dans l'objet de mrds, pour comparer les calculs eux-mêmes sans dépendre de la précision de son
# optimiseur — sa log-vraisemblance, son hessien (flt.var), p et son erreur-type (summary), les tests
# d'adéquation (ddf.gof) et la densité et l'effectif de dht2 (par strate et au total : N, erreur-type,
# CV, intervalle, ddl, taux de rencontre et sa variance, aire couverte ; sans aire de strate, les ddl
# du total avec les poids des strates, recalculés d'après les sorties de dht2, et les siens, que Nuée
# garde en ddl_dht2), sur les données de Distance
# (LTExercise, PTExercise, ducknest, minke, wren_5min, Savannah_sparrow_1980, ClusterExercise,
# capercaillie) et des données simulées (lignes et points, strates, transects vides, troncature en
# pourcentage et à gauche, groupes, clés hn, hr et unif, séries cos, herm et poly, monotonie stricte,
# faible ou aucune, ajustements à l'échelle de σ). Les covariables de la fonction de détection
# (ds(formula = )) : la matrice de model.matrix, la vraisemblance et ses scores, les probabilités de
# chaque détection, puis contre Distance (amakihi, ETP_Dolphin, ClusterExercise, unimak, golftees :
# des points et des lignes, des covariables qualitatives et quantitatives, la taille des groupes,
# des termes d'ajustement, des intervalles, une troncature à gauche) les mêmes calculs aux paramètres
# de Nuée, plus p̄ (summary.ds), la probabilité de chaque détection (predict.ds), la fonction de
# détection moyenne et celle de chaque catégorie (plot.ds) et la densité de Horvitz–Thompson de dht2.
# Puis les refus, une condition sur les observations, ce que l'interface reçoit, le texte des
# méthodes, les tableaux exportés ; et l'export vers R : des scripts lancés seuls dans une session R
# neuve et comparés au modèle, des rapports compilés avec knitr.
#   Rscript moteur/tests/verifier_distance.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
# l'écart relatif le plus grand (rapporté à 1e-300 au moins, pour les valeurs nulles)
ecart <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (length(a) != length(b)) return(Inf)
  k <- is.finite(a) | is.finite(b)
  if (!any(k)) return(0)
  if (any(is.finite(a[k]) != is.finite(b[k]))) return(Inf)
  max(abs(a[k] - b[k]) / pmax(abs(b[k]), 1e-300))
}
proche <- function(a, b, tol = 1e-8) isTRUE(ecart(a, b) < tol)
refus <- function(expr) tryCatch({ force(expr); "" }, error = conditionMessage)

if (nzchar(Sys.getenv("NUEE_DISTANCE"))) .libPaths(c(Sys.getenv("NUEE_DISTANCE"), .libPaths()))
avec_distance <- requireNamespace("Distance", quietly = TRUE) && requireNamespace("mrds", quietly = TRUE)
cat(if (avec_distance) sprintf("(Distance %s et mrds %s installés : comparaisons avec eux)\n", utils::packageVersion("Distance"), utils::packageVersion("mrds"))
    else "(Distance absent : calculs directs seulement)\n")

# Un projet d'un tableau à plat (une ligne par détection, comme le « flatfile » de Distance), et la
# définition d'un échantillonnage par distances sur lui.
projet_distance <- function(df, nom = "distances") projet_depuis_tableau(cbind(nom = sprintf("r%04d", seq_len(nrow(df))), df, stringsAsFactors = FALSE), nom)
definition_distance <- function(p, transect = "ligne", w = NULL, pct = NULL, gauche = 0, cle = "hn", serie = "cos", aj = "aic", mono = "stricte",
                                echelle = "largeur", strate = "Region.Label", aire = "Area", effort = "Effort", taille = NULL, echantillon = "Sample.Label",
                                unites = list(distance = "m", effort = "km", aire = "km2"), facteur = NULL, observations = NULL, condition = NULL, intervalles = NULL, z = NULL) {
  u <- function(x) numeros(p, x)
  spec <- function(x) if (startsWith(x, "id:")) list(by = "id", id = substring(x, 4)) else list(by = "variable", variable = u(x))
  o <- list(transect = transect, echantillon = spec(echantillon), strate = if (!is.null(strate)) spec(strate), effort = if (!is.null(effort)) u(effort),
            aire = if (!is.null(aire)) u(aire), taille = if (!is.null(taille)) u(taille),
            troncature = if (!is.null(w)) list(type = "distance", valeur = w) else if (!is.null(pct)) list(type = "pourcentage", valeur = pct) else list(type = "aucune"),
            gauche = gauche, cle = cle, serie = serie, ajustements = aj, monotonie = mono, echelle_ajustements = echelle, unites = unites, facteur = facteur, intervalles = intervalles,
            z = if (!is.null(z)) u(z))
  definir_modele(p, "DISTANCE", x = NULL, y = u("distance"), echelle = "none", options = o, observations = observations, condition = condition)
}
distance_de <- function(p, ...) ajuster(p, definition_distance(p, ...))
fd_de <- function(cle, ordres = integer(0), serie = NULL, w = 1, L = 0, point = FALSE, echelle = "largeur") {
  list(cle = cle, serie = serie, ordres = as.integer(ordres), w = w, L = L, point = point, echelle_ajustements = echelle)
}

# ---------------------------------------------------------------- données simulées

# Des transects linéaires (ou des points) dans des strates : les animaux à moins de `wmax` de chaque
# ligne (de chaque point), de densité D par km², détectés selon une demi-normale ou un hasard ; les
# distances en mètres arrondies au décimètre, l'effort en km (des passages pour les points), les aires
# en km² ; un transect sans détection garde une ligne, sa distance vide ; des groupes au besoin.
simuler <- function(graine = 1, point = FALSE, strates = c(Nord = 60, Sud = 35), k = c(10, 8), D = c(Nord = 30, Sud = 55), cle = "hn",
                    sigma = 30, b = 2.5, wmax = 120, groupes = FALSE) {
  set.seed(graine)
  lignes <- list()
  obj <- 0
  g <- function(x) if (cle == "hn") exp(-(x / sigma)^2 / 2) else 1 - exp(-(x / sigma)^(-b))
  for (s in names(strates)) for (j in seq_len(k[[match(s, names(strates))]])) {
    effort <- if (point) sample(1:3, 1) else round(stats::runif(1, 0.5, 3), 2)
    surface <- if (point) pi * (wmax / 1000)^2 * effort else 2 * wmax / 1000 * effort
    N <- stats::rpois(1, D[[s]] * surface)
    x <- if (point) wmax * sqrt(stats::runif(N)) else stats::runif(N, 0, wmax)
    x <- round(x[stats::runif(N) < g(x)], 1)
    lab <- sprintf("%s-%02d", s, j)
    if (!length(x)) {
      lignes[[length(lignes) + 1]] <- data.frame(Region.Label = s, Area = strates[[s]], Sample.Label = lab, Effort = effort, object = NA, distance = NA, size = NA)
      next
    }
    taille <- if (groupes) 1 + stats::rpois(length(x), 1.4) else rep(1, length(x))
    lignes[[length(lignes) + 1]] <- data.frame(Region.Label = s, Area = strates[[s]], Sample.Label = lab, Effort = effort, object = obj + seq_along(x), distance = x, size = taille)
    obj <- obj + length(x)
  }
  df <- do.call(rbind, lignes)
  rownames(df) <- NULL
  df
}

# Les mêmes, l'échelle σ de chaque animal dépendant de covariables : l'observateur (A, B ou C, σ × 1,4
# pour B, × 0,8 pour C), le vent (de 0 à 4, σ × exp(−0,15 vent)), et avec des groupes leur taille
# (σ × exp(0,1 (taille − 2))).
simuler_covariables <- function(graine = 1, point = FALSE, strates = c(Nord = 60, Sud = 35), k = c(12, 10), D = c(Nord = 40, Sud = 70), cle = "hn",
                                sigma = 30, b = 2.5, wmax = 120, groupes = FALSE) {
  set.seed(graine)
  lignes <- list()
  obj <- 0
  for (s in names(strates)) for (j in seq_len(k[[match(s, names(strates))]])) {
    effort <- if (point) sample(1:3, 1) else round(stats::runif(1, 0.5, 3), 2)
    surface <- if (point) pi * (wmax / 1000)^2 * effort else 2 * wmax / 1000 * effort
    N <- stats::rpois(1, D[[s]] * surface)
    x <- if (point) wmax * sqrt(stats::runif(N)) else stats::runif(N, 0, wmax)
    obs <- sample(c("A", "B", "C"), N, TRUE, prob = c(0.5, 0.3, 0.2))
    vent <- round(stats::runif(N, 0, 4), 1)
    taille <- if (groupes) 1 + stats::rpois(N, 1.4) else rep(1, N)
    sig <- sigma * exp(log(1.4) * (obs == "B") + log(0.8) * (obs == "C") - 0.15 * vent + if (groupes) 0.1 * (taille - 2) else 0)
    vu <- stats::runif(N) < (if (cle == "hn") exp(-(x / sig)^2 / 2) else 1 - exp(-(x / sig)^(-b)))
    lab <- sprintf("%s-%02d", s, j)
    if (!any(vu)) {
      lignes[[length(lignes) + 1]] <- data.frame(Region.Label = s, Area = strates[[s]], Sample.Label = lab, Effort = effort, object = NA, distance = NA, size = NA,
                                                observateur = NA, vent = NA)
      next
    }
    lignes[[length(lignes) + 1]] <- data.frame(Region.Label = s, Area = strates[[s]], Sample.Label = lab, Effort = effort, object = obj + seq_len(sum(vu)),
                                              distance = round(x[vu], 1), size = taille[vu], observateur = obs[vu], vent = vent[vu])
    obj <- obj + sum(vu)
  }
  df <- do.call(rbind, lignes)
  rownames(df) <- NULL
  df
}

# ---------------------------------------------------------------- calculs directs

cat("Calculs directs : la quadrature, les scores, la demi-normale, p, Hermite, la monotonie, la densité, l'adéquation\n")

# la quadrature contre integrate()
cas <- list(
  list(fd_de("hn"), log(0.4)), list(fd_de("hn", 2:3, "cos"), c(log(0.5), 0.1, -0.05)), list(fd_de("hr"), c(log(3), log(0.3))),
  list(fd_de("hr"), c(log(1.5), log(0.002))), list(fd_de("hr"), c(log(50), log(0.4))), list(fd_de("unif", 1, "cos"), 0.3),
  list(fd_de("unif", c(2, 4), "poly"), c(-0.5, 0.2)), list(fd_de("hn", 4, "herm"), c(log(0.6), 0.1)), list(fd_de("hn", w = 50, point = TRUE), log(20)),
  list(fd_de("hr", 2, "cos", w = 50, point = TRUE), c(log(2.2), log(12), 0.08)), list(fd_de("hn", L = 0.1), log(0.45)),
  list(fd_de("hn", 2, "cos", echelle = "echelle"), c(log(0.3), -0.15)), list(fd_de("hr", c(4, 6), "herm", L = 0.05, point = TRUE), c(log(4), log(0.5), 0.2, -0.04)))
# (la référence : Gauss–Legendre à 8 points sur 100 000 panneaux égaux, les nœuds des valeurs propres de
# la matrice de Jacobi)
gl8 <- local({
  i <- 1:7
  J <- matrix(0, 8, 8)
  J[cbind(i, i + 1)] <- J[cbind(i + 1, i)] <- i / sqrt(4 * i^2 - 1)
  e <- eigen(J, symmetric = TRUE)
  list(x = e$values, w = 2 * e$vectors[1, ]^2)
})
reference_integrale <- function(fd, th, panneaux = 1e5) {
  h <- (fd$w - fd$L) / panneaux
  milieux <- fd$L + h * (seq_len(panneaux) - 0.5)
  x <- as.vector(outer(gl8$x * h / 2, milieux, "+"))
  v <- poids_detection(fd, x) * detection_distance(fd, th, x)$gt
  v[!(v >= 1e-16)] <- 0
  sum(v * rep(gl8$w * h / 2, panneaux))
}
e_quad <- max(vapply(cas, function(k) ecart(integrale_detection(k[[1]], k[[2]], k[[1]]$L, k[[1]]$w)$I, reference_integrale(k[[1]], k[[2]])), 0))
s_hn <- 0.4
verifier(sprintf("les intégrales (integrate, adaptative) contre Gauss–Legendre sur 100 000 panneaux : 13 fonctions de détection (σ du hasard 500 fois plus petit que w, b = 50, une fonction qui s'annule, points, troncature à gauche ; écart %.1e) ; la demi-normale contre σ √(2π) (Φ(w/σ) − ½)", e_quad),
         e_quad < 1e-9 && proche(integrale_detection(fd_de("hn"), log(s_hn), 0, 1)$I, s_hn * sqrt(2 * pi) * (stats::pnorm(1 / s_hn) - 0.5), 1e-12))

# les scores contre des différences finies des termes de la vraisemblance
termes_vraisemblance <- function(fd, th, x) {
  p1 <- pmax(poids_detection(fd, x) * detection_distance(fd, th, x)$gt, 1e-15)
  log(p1) - log(integrale_detection(fd, th, fd$L, fd$w)$I)
}
set.seed(11)
xs <- round(abs(stats::rnorm(150, 0, 0.35)), 3)
xs <- xs[xs <= 1]
e_sc <- max(vapply(cas[c(1, 2, 3, 6, 7, 8, 12)], function(k) {
  fd <- k[[1]]
  th <- k[[2]]
  S <- vraisemblance_detection(fd, th, xs, TRUE)$scores
  J <- vapply(seq_along(th), function(j) {
    h <- 1e-5 * max(1, abs(th[j]))
    (termes_vraisemblance(fd, replace(th, j, th[j] + h), xs) - termes_vraisemblance(fd, replace(th, j, th[j] - h), xs)) / (2 * h)
  }, numeric(length(xs)))
  max(abs(S - J)) / max(abs(J))
}, 0))
verifier(sprintf("les scores de chaque distance (ln b, ln σ, aj ; σ en échelle des ajustements) contre des différences finies (écart %.1e)", e_sc), e_sc < 1e-7)

# la demi-normale seule contre sa vraisemblance écrite avec pnorm() (lignes, points, troncature à
# gauche) : Σ ln g(xi) − n ln ∫ g, et pour des points Σ ln xi + Σ ln g(xi) − n ln ∫ x g (les
# constantes 1/(w − L) et 2/w² de mrds s'en vont)
hn_direct <- function(x, w, L, point) {
  f <- function(ls) {
    s <- exp(ls)
    I <- if (point) s^2 * (exp(-L^2 / (2 * s^2)) - exp(-w^2 / (2 * s^2))) else s * sqrt(2 * pi) * (stats::pnorm(w / s) - stats::pnorm(L / s))
    sum(-x^2 / (2 * s^2)) - length(x) * log(I) + if (point) sum(log(x)) else 0
  }
  o <- stats::optimize(f, c(log(w) - 5, log(w) + 3), maximum = TRUE, tol = 1e-12)
  list(theta = o$maximum, ll = o$objective)
}
ok_hn <- TRUE
for (k in list(list(w = 1, L = 0, point = FALSE), list(w = 1, L = 0.08, point = FALSE), list(w = 1, L = 0, point = TRUE), list(w = 0.9, L = 0.05, point = TRUE))) {
  x <- pmax(xs[xs >= k$L & xs <= k$w], 0.0005)
  a <- ajuster_detection(fd_de("hn", w = k$w, L = k$L, point = k$point), x)
  dd <- hn_direct(x, k$w, k$L, k$point)
  ok_hn <- ok_hn && abs(a$theta - dd$theta) < 1e-7 && abs(a$ll - dd$ll) < 1e-9 * abs(a$ll)
}
verifier("la demi-normale seule : ln σ et la log-vraisemblance de la vraisemblance écrite avec pnorm() (lignes, points, troncature à gauche)", ok_hn)

fd1 <- fd_de("hn", w = 1)
a1 <- ajuster_detection(fd1, xs)
pp <- probabilite_detection(fd1, a1$theta)
s1 <- exp(a1$theta)
p_direct <- s1 * sqrt(2 * pi) * (stats::pnorm(1 / s1) - 0.5)
dp_num <- (probabilite_detection(fd1, a1$theta + 1e-6)$p - probabilite_detection(fd1, a1$theta - 1e-6)$p) / 2e-6
verifier("p de la demi-normale : σ √(2π) (Φ(w/σ) − ½) / w, et sa dérivée par ln σ contre des différences finies", proche(pp$p, p_direct, 1e-12) && proche(pp$dp, dp_num, 1e-7))
Jt <- vapply(1, function(j) (termes_vraisemblance(fd1, a1$theta + 1e-6, xs) - termes_vraisemblance(fd1, a1$theta - 1e-6, xs)) / 2e-6, numeric(length(xs)))
verifier("la matrice d'information : la somme des produits des scores de chaque distance (mrds : flt.var), sa covariance l'inverse",
         proche(a1$H, crossprod(Jt), 1e-7) && proche(a1$V, 1 / crossprod(Jt), 1e-7))

u <- seq(-2, 2, 0.25)
verifier("les polynômes d'Hermite des probabilistes : He2, He4, He6, He8 écrits en entier", proche(hermite_proba(u, 2), u^2 - 1, 1e-14) &&
           proche(hermite_proba(u, 4), u^4 - 6 * u^2 + 3, 1e-13) && proche(hermite_proba(u, 6), u^6 - 15 * u^4 + 45 * u^2 - 15, 1e-12) &&
           proche(hermite_proba(u, 8), u^8 - 28 * u^6 + 210 * u^4 - 420 * u^2 + 105, 1e-12))
verifier("les ordres des termes d'ajustement de Distance (cos 2, 3… ; unif + cos 1, 2… ; herm et poly 4, 6… ; unif + poly 2, 4…)",
         identical(ordres_ajustement(3, "hn", "cos"), 2:4) && identical(ordres_ajustement(3, "unif", "cos"), 1:3) && identical(ordres_ajustement(2, "hr", "herm"), c(4L, 6L)) &&
           identical(ordres_ajustement(3, "unif", "poly"), c(2L, 4L, 6L)))

# des distances qui évitent la ligne (une bosse vers 0,3) : la contrainte de monotonie agit
set.seed(3)
bosse <- local({
  out <- numeric(0)
  while (length(out) < 250) {
    x <- stats::runif(1000)
    out <- c(out, x[stats::runif(1000) < (0.4 + 1.5 * x) * exp(-(x / 0.45)^2 / 2)])
  }
  round(out[1:250], 3)
})
fdc <- fd_de("hn", 2:3, "cos")
libre <- ajuster_detection(fdc, bosse, monotonie = "aucune")
sous <- ajuster_detection(fdc, bosse, monotonie = "faible")
k <- contraintes_monotonie(fdc, sous$theta, FALSE)
actives <- which(abs(k$c) < 1e-7)
gr <- -colSums(vraisemblance_detection(fdc, sous$theta, bosse, TRUE)$scores)
lambda <- if (length(actives)) qr.solve(t(k$J[actives, , drop = FALSE]), gr) else numeric(0)
verifier(sprintf("monotonie faible sur une bosse : sans contrainte g dépasse 1 ; avec, les contraintes tiennent, %d actives, multiplicateurs positifs, ∇f = Jᵀλ (Karush–Kuhn–Tucker)", length(actives)),
         any(contraintes_monotonie(fdc, libre$theta, FALSE)$c < -1e-3) && isTRUE(sous$contraint) && min(k$c) > -1e-9 && length(actives) >= 1 &&
           all(lambda > 0) && max(abs(gr - drop(crossprod(k$J[actives, , drop = FALSE], lambda)))) < 1e-5 * max(abs(gr), 1) && sous$ll < libre$ll)

ch <- choisir_detection(fd_de("unif"), bosse, "cos", monotonie = "stricte")
aics <- vapply(ch$candidats, `[[`, 0, "AIC")
verifier(sprintf("le choix de ds() : la clé seule puis un terme de plus tant que l'AIC baisse (%s)", paste(round(aics, 2), collapse = ", ")),
         identical(ch$raison, "aic") && all(diff(aics[-length(aics)]) < 0) && aics[length(aics)] > aics[length(aics) - 1] &&
           identical(ch$choisi$fd$ordres, seq_len(length(aics) - 2)) && isTRUE(ch$candidats[[1]]$monotonie == "aucune"))

# la densité écrite à la main, sur des lignes simulées en deux strates
sim <- simuler(5)
ps <- projet_distance(sim)
m <- distance_de(ps, w = 100)
t <- m$donnees
cu <- 1 * 1000 / 1e6
main <- lapply(c("Nord", "Sud"), function(s) {
  tr <- t$transects[t$transects$strate == s, ]
  k <- nrow(tr)
  L <- sum(tr$effort)
  n <- sum(tr$n)
  er <- n / L
  var_er <- k / (L^2 * (k - 1)) * sum(tr$effort^2 * (tr$n / tr$effort - er)^2)
  A <- unique(sim$Area[sim$Region.Label == s])
  N <- A * n / (2 * 100 * cu * L * m$p)
  cv2 <- var_er / er^2 + m$var_p / m$p^2
  ddl <- cv2^2 / ((var_er / er^2)^2 / (k - 1) + (m$var_p / m$p^2)^2 / (t$n - 1))
  C <- exp(stats::qt(0.975, ddl) * sqrt(log(1 + cv2)))
  c(N = N, se = N * sqrt(cv2), ddl = ddl, bas = N / C, haut = N * C, er = er, var_er = var_er, var_er_N = N^2 * var_er / er^2)
})
N <- main[[1]][["N"]] + main[[2]][["N"]]
v_er <- main[[1]][["var_er_N"]] + main[[2]][["var_er_N"]]
v_det <- N^2 * m$var_p / m$p^2
ddl_er <- v_er^2 / (main[[1]][["var_er_N"]]^2 / (sum(t$transects$strate == "Nord") - 1) + main[[2]][["var_er_N"]]^2 / (sum(t$transects$strate == "Sud") - 1))
ddl <- (v_er + v_det)^2 / N^4 / ((v_er / N^2)^2 / ddl_er + (v_det / N^2)^2 / (t$n - 1))
C <- exp(stats::qt(0.975, ddl) * sqrt(log(1 + (v_er + v_det) / N^2)))
dn <- m$densite
verifier("la densité écrite à la main (deux strates de lignes) : N = A n / (2 w L p), la variance R2 du taux de rencontre, celle de p, Satterthwaite, l'intervalle log-normal ; et le total",
         proche(dn$N[1:2], c(main[[1]][["N"]], main[[2]][["N"]]), 1e-12) && proche(dn$se_N[1:2], c(main[[1]][["se"]], main[[2]][["se"]]), 1e-12) &&
           proche(dn$ddl[1:2], c(main[[1]][["ddl"]], main[[2]][["ddl"]]), 1e-12) && proche(dn$bas_N[1:2], c(main[[1]][["bas"]], main[[2]][["bas"]]), 1e-12) &&
           proche(dn$haut_N[1:2], c(main[[1]][["haut"]], main[[2]][["haut"]]), 1e-12) && proche(dn$se_rencontre[1:2]^2, c(main[[1]][["var_er"]], main[[2]][["var_er"]]), 1e-12) &&
           proche(dn$N[3], N, 1e-12) && proche(dn$se_N[3], sqrt(v_er + v_det), 1e-12) && proche(dn$ddl[3], ddl, 1e-12) && proche(dn$bas_N[3], N / C, 1e-12) &&
           proche(dn$D[1:2], dn$N[1:2] / c(60, 35), 1e-12) && proche(dn$D[3], N / 95, 1e-12))
# sans aire de strate, à la main : le total, la moyenne des densités des strates pondérée par leurs
# aires couvertes a, sa variance du taux de rencontre Σ wi² vi et ses degrés de liberté de
# Satterthwaite avec ces poids, (Σ wi² vi)² / Σ (wi² vi)² / (ki − 1) ; ceux de dht2, sans les poids au
# dénominateur, (Σ wi² vi)² / Σ vi² / (ki − 1), en ddl_dht2 ; avec des aires, ddl_dht2 n'a rien
m0 <- distance_de(ps, w = 100, aire = NULL)
d0 <- m0$densite
k0 <- vapply(c("Nord", "Sud"), function(s) sum(t$transects$strate == s), 0)
a0 <- 2 * 100 * cu * vapply(c("Nord", "Sud"), function(s) sum(t$transects$effort[t$transects$strate == s]), 0)
w0 <- a0 / sum(a0)
D_s <- vapply(main, function(x) x[["N"]], 0) / c(60, 35)
v_s <- vapply(main, function(x) x[["var_er_N"]], 0) / c(60, 35)^2
D0 <- sum(w0 * D_s)
v_er0 <- sum(w0^2 * v_s)
v_det0 <- D0^2 * m$var_p / m$p^2
satterthwaite0 <- function(er_ddl) ((v_er0 + v_det0) / D0^2)^2 / ((v_er0 / D0^2)^2 / er_ddl + (v_det0 / D0^2)^2 / (t$n - 1))
ddl_er0 <- v_er0^2 / sum((w0^2 * v_s)^2 / (k0 - 1))
ddl0 <- satterthwaite0(ddl_er0)
ddl0_dht2 <- satterthwaite0(v_er0^2 / sum(v_s^2 / (k0 - 1)))
C0 <- exp(stats::qt(0.975, ddl0) * sqrt(log(1 + (v_er0 + v_det0) / D0^2)))
verifier(sprintf("sans aire de strate, à la main : le total pondéré par les aires couvertes, ses %.1f degrés de liberté de Satterthwaite avec les poids (%.2f sans eux, ceux de dht2, en ddl_dht2), son intervalle",
                 ddl0, ddl0_dht2),
         m0$donnees$densite_seule && proche(d0$D[1:2], D_s, 1e-12) && proche(d0$D[3], D0, 1e-12) && proche(d0$se_D[3], sqrt(v_er0 + v_det0), 1e-12) &&
           proche(d0$ddl_rencontre[3], ddl_er0, 1e-12) && proche(d0$ddl[3], ddl0, 1e-12) && proche(d0$ddl_dht2[3], ddl0_dht2, 1e-12) && ddl0 > ddl0_dht2 &&
           proche(d0$bas_D[3], D0 / C0, 1e-12) && proche(d0$haut_D[3], D0 * C0, 1e-12) && proche(d0$ddl[1:2], dn$ddl[1:2], 1e-12) &&
           all(is.na(d0$ddl_dht2[1:2])) && all(is.na(dn$ddl_dht2)))

# l'adéquation à la main
F <- sort(repartition_detection(m$detection$fd, m$detection$theta, t$x))
W <- 1 / (12 * t$n) + sum((F - (seq_len(t$n) - 0.5) / t$n)^2)
nc <- round(sqrt(t$n))
br <- 100 / nc * (0:nc)
obs <- tabulate(findInterval(t$x, br, left.open = TRUE, rightmost.closed = TRUE) + (t$x == 0), nc)
att <- t$n * diff(vapply(br, function(b) stats::integrate(function(x) exp(-(x / exp(m$detection$theta))^2 / 2), 0, max(b, 1e-12), rel.tol = 1e-12)$value, 0)) /
  stats::integrate(function(x) exp(-(x / exp(m$detection$theta))^2 / 2), 0, 100, rel.tol = 1e-12)$value
a <- m$adequation
verifier(sprintf("Cramér–von Mises (W = %.4f) et le khi² sur %d classes égales (fermées à droite) écrits à la main ; la loi de W : 1 − F(0,46136) = 0,05, 1 − F(0,74346) = 0,01 (Anderson et Darling, 1952)", W, nc),
         proche(a$cvm$W, W, 1e-10) && identical(as.numeric(a$khi2$classes$observe), as.numeric(obs)) && proche(a$khi2$classes$attendu, att, 1e-9) &&
           proche(a$khi2$chi2, sum((obs - att)^2 / att), 1e-9) && a$khi2$ddl == nc - 2 && abs(1 - pcramer_detection(0.46136) - 0.05) < 2e-5 &&
           abs(1 - pcramer_detection(0.74346) - 0.01) < 2e-5)
# des distances par intervalles : la loi multinomiale des comptes des classes, écrite avec pnorm()
bornes <- c(0, 0.1, 0.25, 0.45, 0.7, 1)
xb <- xs[xs <= 1]
fdb <- c(fd_de("hn", w = 1), list(coupures = bornes))
ab <- ajuster_detection(fdb, xb)
nk <- tabulate(cut(xb, bornes, include.lowest = TRUE, labels = FALSE), 5)
ll_b <- function(ls) { s <- exp(ls); P <- diff(stats::pnorm(bornes / s)) / (stats::pnorm(1 / s) - 0.5); sum(nk * log(P)) }
ob <- stats::optimize(ll_b, c(-5, 3), maximum = TRUE, tol = 1e-12)
fdb2 <- c(fd_de("hr", 2:3, "cos", point = TRUE), list(coupures = bornes))
thb <- c(log(2.5), log(0.4), 0.1, -0.05)
Sb <- vraisemblance_detection(fdb2, thb, xb, TRUE)$scores
Jb <- vapply(seq_along(thb), function(j) {
  h <- 1e-5
  (vraisemblance_detection(fdb2, replace(thb, j, thb[j] + h), xb)$ll - vraisemblance_detection(fdb2, replace(thb, j, thb[j] - h), xb)$ll) / (2 * h)
}, 0)
verifier(sprintf("des distances par intervalles (5 classes) : la vraisemblance multinomiale de la demi-normale écrite avec pnorm() ; les scores (hasard + cosinus, points) contre des différences finies (écart %.1e)",
                 max(abs(colSums(Sb) - Jb)) / max(abs(Jb))),
         abs(ab$theta - ob$maximum) < 1e-7 && abs(ab$ll - ob$objective) < 1e-9 * abs(ob$objective) && max(abs(colSums(Sb) - Jb)) < 1e-6 * max(abs(Jb)) &&
           is.null(adequation_detection(ab, xb, seq_along(xb))$cvm) && identical(as.numeric(adequation_detection(ab, xb, seq_along(xb))$khi2$classes$observe), as.numeric(nk)))
verifier("les unités : m, km, km² donnent 0,001 (lignes) ; m et ha, 0,01 (points) ; un facteur donné tel quel",
         proche(facteur_echantillonnage(list(transect = "ligne", unites = list(distance = "m", effort = "km", aire = "km2"))), 0.001, 1e-15) &&
           proche(facteur_echantillonnage(list(transect = "point", unites = list(distance = "m", effort = "m", aire = "ha"))), 0.01, 1e-15) &&
           identical(facteur_echantillonnage(list(facteur = 0.25)), 0.25))

# ---------------------------------------------------------------- covariables : calculs directs

cat("Covariables : la matrice de model.matrix, la vraisemblance et ses scores, les probabilités, p̄, l'ajustement\n")
sc <- simuler_covariables(6, groupes = TRUE)
dans_sc <- !is.na(sc$distance) & sc$distance <= 100
colonnes_sc <- list(observateur = factor(sc$observateur[dans_sc], levels = c("A", "B", "C")), vent = sc$vent[dans_sc], size = sc$size[dans_sc])
Zc <- matrice_covariables(colonnes_sc)
mm <- stats::model.matrix(~ observateur + vent + size, data.frame(colonnes_sc))
Zb <- matrice_covariables(list(o = factor(c("b", "c", "b", "c"), levels = c("a", "b", "c"))))
verifier("la matrice des covariables : celle de model.matrix(~ observateur + vent + size) (la constante, une indicatrice par catégorie sauf la première, A, les quantitatives telles quelles), ses colonnes nommées comme R ; une catégorie sans détection laissée de côté (la référence : la première présente, comme factor() dans mrds)",
         identical(colnames(Zc), colnames(mm)) && proche(Zc, mm, 1e-15) && identical(attr(Zc, "references"), list(observateur = "A")) &&
           identical(attr(Zc, "termes")$terme, c("ln_sigma", "observateur(B)", "observateur(C)", "vent", "size")) &&
           identical(colnames(Zb), c("(Intercept)", "oc")) && identical(attr(Zb, "references")$o, "b"))
xc <- sc$distance[dans_sc]
e_nul <- 0
e_gr <- 0
e_dp <- 0
for (cfg in list(list("hn", FALSE, NULL), list("hr", FALSE, NULL), list("hn", TRUE, NULL), list("hr", TRUE, c(0, 15, 35, 60, 100)), list("hn", FALSE, c(0, 20, 40, 70, 100)))) {
  base <- c(fd_de(cfg[[1]], w = 100, point = cfg[[2]]), list(coupures = cfg[[3]]))
  fdz <- avec_covariables(base, Zc)
  th0 <- c(if (cfg[[1]] == "hr") log(2.5), log(35))
  thz <- c(th0, 0, 0, 0, 0)
  # (des coefficients nuls : la fonction de détection sans covariables)
  e_nul <- max(e_nul, ecart(vraisemblance_covariables(fdz, thz, xc)$ll, vraisemblance_detection(base, th0, xc)$ll))
  thz <- c(th0, 0.3, -0.2, -0.12, 0.08)
  S <- vraisemblance_covariables(fdz, thz, xc, TRUE)$scores
  num <- vapply(seq_along(thz), function(j) { h <- 1e-6; (vraisemblance_covariables(fdz, replace(thz, j, thz[j] + h), xc)$ll - vraisemblance_covariables(fdz, replace(thz, j, thz[j] - h), xc)$ll) / (2 * h) }, 0)
  e_gr <- max(e_gr, max(abs(colSums(S) - num)) / max(abs(num)))
  pr <- probabilites_covariables(fdz, thz)
  dnum <- vapply(seq_along(thz), function(j) { h <- 1e-6; (probabilites_covariables(fdz, replace(thz, j, thz[j] + h))$p - probabilites_covariables(fdz, replace(thz, j, thz[j] - h))$p) / (2 * h) }, numeric(length(xc)))
  e_dp <- max(e_dp, max(abs(pr$dp - dnum)) / max(abs(dnum)))
}
verifier(sprintf("la vraisemblance à covariables : leurs coefficients nuls, celle sans covariables (écart %.1e) ; son gradient (lignes, points, hn, hr, distances par intervalles) contre des différences finies (écart %.1e) ; celui de la probabilité de chaque détection aussi (écart %.1e)",
                 e_nul, e_gr, e_dp), e_nul < 1e-13 && e_gr < 1e-6 && e_dp < 1e-6)
# p̄ et sa variance (calc.se.Np de mrds) : des probabilités toutes égales donnent la méthode delta sur p
fdz <- avec_covariables(fd_de("hn", w = 100), Zc)
prz <- probabilites_covariables(fdz, c(log(35), 0, 0, 0, 0))
Vz <- crossprod(matrix(stats::rnorm(25), 5)) / 50
gp <- colMeans(prz$dp)
mz <- moyenne_detection(prz, Vz)
verifier("p̄ = n / Σ 1/pi et sa variance comme summary.ds : des pi tous égaux, p et la variance de la méthode delta sur p",
         proche(mz$p, prz$p[1], 1e-14) && proche(mz$var_p, drop(t(gp) %*% Vz %*% gp), 1e-9) && proche(mz$N, sum(1 / prz$p), 1e-14))
# l'ajustement sur les coefficients réduits trouve le maximum de l'ajustement direct
fdz <- avec_covariables(fd_de("hr", w = 100), Zc)
reduit <- choisir_covariables(fdz, xc, NULL)$choisi
direct <- ajuster_detection(fdz, xc, monotonie = "aucune", vraisemblance = vraisemblance_covariables)
verifier(sprintf("l'ajustement à covariables, sur les coefficients centrés et réduits : le maximum de l'ajustement direct (écart des paramètres %.1e)", max(abs(reduit$theta - direct$theta))),
         isTRUE(reduit$converge) && isTRUE(direct$converge) && max(abs(reduit$theta - direct$theta)) < 1e-5 && abs(reduit$ll - direct$ll) < 1e-9 * abs(direct$ll))
psc <- projet_distance(sc)
msc <- distance_de(psc, w = 100, taille = "size", z = c("observateur", "vent"))
verifier("un modèle à covariables : le même sans covariables d'abord dans les candidats (pour comparer les AIC), puis celui à covariables, retenu, qui l'emporte ici ; sans terme d'ajustement (comme ds) ; la probabilité de chaque détection",
         identical(msc$candidats$covariables, c("", "observateur + vent")) && identical(msc$candidats$choisi, c(FALSE, TRUE)) && msc$candidats$AIC[2] < msc$candidats$AIC[1] &&
           identical(msc$selection, "covariables") && !length(msc$detection$fd$ordres) && length(msc$p_detections) == msc$donnees$n &&
           identical(msc$parametres$parametre, c("ln_sigma", "observateur(B)", "observateur(C)", "vent")) && proche(msc$p, msc$donnees$n / sum(1 / msc$p_detections), 1e-14))

# ---------------------------------------------------------------- contre Distance

if (avec_distance) {
  cat("Contre Distance (ds, dht2) et mrds : le modèle choisi, l'optimum, puis les calculs aux paramètres de Nuée\n")
  # le libellé d'un modèle de mrds, comme libelle_detection
  libelle_mrds <- function(ddf) {
    o <- ddf$ds$aux$ddfobj
    if (is.null(o$adjustment)) o$type else sprintf("%s + %s(%s)", o$type, o$adjustment$series, paste(o$adjustment$order, collapse = ", "))
  }
  # Nuée's θ dans l'objet de mrds (même clé, mêmes termes) : ses calculs à ces paramètres
  injecter <- function(ddf, th) {
    ddf$par <- th
    ddf$ds$aux$ddfobj <- mrds:::assign.par(ddf$ds$aux$ddfobj, th)
    ddf$hessian <- mrds:::flt.var(ddf$ds$aux$ddfobj, ddf$ds$aux)
    ddf$fitted <- stats::predict(ddf, esw = FALSE, compute = TRUE)$fitted
    ddf$Nhat <- mrds:::NCovered.ds(ddf$par, ddf, group = TRUE)
    ddf$lnl <- -mrds:::flnl(th, ddf$ds$aux$ddfobj, ddf$ds$aux)
    ddf
  }
  ds_de <- function(df, ...) suppressMessages(suppressWarnings(Distance::ds(df, quiet = TRUE, ...)))
  # Les degrés de liberté et l'intervalle de chaque ligne de dht2 (`d2`) que Nuée doit donner : les
  # siens, sauf au total d'une densité sans aire de strate en plusieurs strates (`pond`), où Nuée prend
  # ceux de Satterthwaite avec les poids des strates wi (leurs aires couvertes), que dht2 laisse hors
  # du dénominateur. Ceux-là sont calculés d'après les seules sorties de dht2 : les variances du taux
  # de rencontre vi (ER_var_Nhat) et les ddl (ER_df) des strates, et au total ses variances de détection
  # et des groupes, n_ddf, n_par, D et son CV ; avec eux, les ddl du taux de rencontre (`er_df`) et
  # l'intervalle log-normal. `sans_poids` : la même formule sans les poids, qui doit redonner ceux de
  # dht2 (les ddl_dht2 de Nuée).
  attendu_dht2 <- function(d2, pond) {
    a <- list(df = d2$df, LCI = d2$LCI, UCI = d2$UCI)
    if (!pond) return(a)
    k <- which(d2$Region.Label == "Total")
    st <- d2[-k, ]
    tot <- d2[k, ]
    w <- st$Covered_area / sum(st$Covered_area)
    er <- sum(w^2 * st$ER_var_Nhat)
    A2 <- tot$Abundance^2
    satterthwaite <- function(er_df) sum(c(er, tot$df_var, tot$group_var_Nhat) / A2, na.rm = TRUE)^2 /
      sum(c((er / A2)^2 / er_df, (tot$df_var / A2)^2 / (tot$n_ddf - tot$n_par), (tot$group_var_Nhat / A2)^2 / (tot$n_ddf - 1)), na.rm = TRUE)
    a$er_df <- er^2 / sum((w^2 * st$ER_var_Nhat)^2 / st$ER_df)
    a$df[k] <- satterthwaite(a$er_df)
    a$sans_poids <- satterthwaite(er^2 / sum(st$ER_var_Nhat^2 / st$ER_df))
    C <- exp(stats::qt(0.975, a$df[k]) * sqrt(log(1 + tot$Abundance_CV^2)))
    a$LCI[k] <- tot$Abundance / C
    a$UCI[k] <- tot$Abundance * C
    a$k <- k
    a
  }
  # (le total d'une densité sans aire de strate : ses ddl pondérés, ceux du taux de rencontre aussi ; les
  # ddl de dht2 en ddl_dht2, et la formule sans les poids qui les redonne ; ailleurs, ddl_dht2 vide)
  ddl_dht2_comme <- function(dn, d2, j, a, tol) {
    if (is.null(a$k)) return(all(is.na(dn$ddl_dht2)))
    i <- j[a$k]
    proche(dn$ddl_rencontre[i], a$er_df, tol) && proche(dn$ddl_dht2[i], d2$df[a$k], tol) && proche(a$sans_poids, d2$df[a$k], 1e-9) &&
      all(is.na(dn$ddl_dht2[-i])) && dn$ddl[i] > dn$ddl_dht2[i]
  }
  # Une comparaison : `df` le tableau à plat, `args` ceux de ds(), `nuee` ceux de definition_distance ;
  # `cu` le facteur des unités de dht2 ; `spline` : le hasard sans ajustement sur des lignes, que mrds
  # intègre par un spline de lissage (gstdint) et non exactement.
  comparer <- function(nom, df, args, nuee, cu, strates = TRUE, tol_par = 5e-3, gauche_lignes = FALSE) {
    mD <- do.call(ds_de, c(list(df), args))
    p <- projet_distance(df)
    mN <- do.call(distance_de, c(list(p), nuee))
    det <- mN$detection
    lab <- libelle_detection(det$fd)
    spline <- det$fd$cle == "hr" && !length(det$fd$ordres) && !det$fd$point && det$fd$L == 0
    tol_f <- if (spline) 1e-3 else 1e-8
    res <- list()
    res$modele <- identical(lab, libelle_mrds(mD$ddf))
    # le même modèle chez mrds, pour comparer ses calculs aux paramètres de Nuée
    dD <- mD$ddf
    if (!res$modele && length(det$fd$ordres)) dD <- do.call(ds_de, c(list(df), modifyList(args, list(nadj = length(det$fd$ordres)))))$ddf
    if (!res$modele && !length(det$fd$ordres)) dD <- do.call(ds_de, c(list(df), modifyList(args, list(adjustment = NULL))))$ddf
    # le même optimum (à la précision de l'optimiseur de mrds : SLSQP s'arrête à 1e-6 près de la
    # log-vraisemblance) ; sinon un autre optimum local, admis s'il ne cède pas plus de 0,005 de
    # log-vraisemblance à celui de mrds (une différence d'AIC de 0,01), et dit
    ec_par <- if (det$q) max(abs(det$theta - dD$par) / pmax(1, abs(dD$par))) else 0
    d_ll <- det$ll - dD$lnl
    meme <- ec_par < tol_par && (abs(d_ll) < 1e-6 * abs(dD$lnl) || spline)
    autre <- !meme && !spline && d_ll > -0.005
    res$optimum <- meme || autre || (spline && abs(d_ll) < 0.1)
    # (les dérivées numériques de mrds, numDeriv : son hessien à 1e-6 près, une erreur que l'inverse
    # multiplie par le conditionnement κ de la matrice ; les erreurs-types comparées à 3e-6 κ près, 1e-5 au moins)
    kappa <- if (det$q) { ev <- eigen(det$H, symmetric = TRUE, only.values = TRUE)$values; max(ev) / min(ev) } else 1
    tol_v <- if (spline) 1e-2 else max(1e-5, 3e-6 * kappa)
    if (det$q) {
      dI <- injecter(dD, det$theta)
      sI <- summary(dI)
      res$logL <- proche(det$ll, dI$lnl, if (spline) 1e-3 else 1e-9)
      res$hessien <- proche(det$H, dI$hessian, if (spline) 1e-2 else 1e-5)
      res$p <- proche(mN$p, sI$average.p, if (spline) 1e-3 else 1e-9) && proche(mN$se_p, sI$average.p.se, tol_v)
      g <- suppressMessages(mrds::ddf.gof(dI, qq = FALSE))
      # (W est une somme de petits carrés : l'intégrale de mrds à 1e-7 près s'y voit davantage ; des
      # distances par intervalles : le khi² seulement, comme ddf.gof)
      res$adequation <- (if (is.null(mN$adequation$cvm)) is.null(g$dsgof)
                         else proche(mN$adequation$cvm$W, g$dsgof$CvM$W, if (spline) 1e-2 else 1e-5) && proche(mN$adequation$cvm$p, g$dsgof$CvM$p, if (spline) 1e-2 else 1e-6)) &&
        proche(mN$adequation$khi2$chi2, g$chisquare$chi1$chisq, if (spline) 1e-2 else 1e-6) && identical(as.numeric(mN$adequation$khi2$classes$observe), as.numeric(g$chisquare$chi1$observed))
    } else dI <- dD
    d2 <- as.data.frame(Distance::dht2(dI, flatfile = df, strat_formula = if (strates) ~Region.Label else ~1, convert_units = cu))
    dens <- attr(Distance::dht2(dI, flatfile = df, strat_formula = if (strates) ~Region.Label else ~1, convert_units = cu), "density")
    dn <- mN$densite
    lab_d <- if (strates) as.character(d2$Region.Label) else "Total"
    lab_n <- ifelse(is.na(dn$strate), "Total", dn$strate)
    if (nrow(dn) == 1) lab_n <- lab_d[1]
    j <- match(lab_d, lab_n)
    seule <- mN$donnees$densite_seule
    a <- attendu_dht2(d2, seule && nrow(dn) > 1)
    # (des lignes tronquées à gauche : l'aire couverte de Nuée est 2 (w − L) L, celle de dht2 2 w L)
    f_g <- if (gauche_lignes) (mN$donnees$w - mN$donnees$L) / mN$donnees$w else 1
    tol_d <- if (spline) 1e-3 else 1e-9
    res$densite <- !anyNA(j) && proche(if (seule) dn$D[j] else dn$N[j] * f_g, d2$Abundance, tol_d) && proche(dn$cv[j], d2$Abundance_CV, tol_v) &&
      proche(if (seule) dn$se_D[j] else dn$se_N[j] * f_g, d2$Abundance_se, tol_v) && proche(dn$ddl[j], a$df, tol_v) &&
      proche(if (seule) dn$bas_D[j] else dn$bas_N[j] * f_g, a$LCI, tol_v) && proche(if (seule) dn$haut_D[j] else dn$haut_N[j] * f_g, a$UCI, tol_v) &&
      proche(dn$rencontre[j], d2$ER, 1e-12) && identical(as.numeric(dn$k[j]), as.numeric(d2$k)) && identical(as.numeric(dn$n[j]), as.numeric(d2$n)) && proche(dn$effort[j], d2$Effort, 1e-12) &&
      proche(dn$couverte[j] / f_g, d2$Covered_area, 1e-12) &&
      (if (!seule) proche(dn$D[j] * f_g, dens$Density, tol_d) && proche(dn$se_D[j] * f_g, dens$Density_se, tol_v) else TRUE)
    res$ddl_dht2 <- !anyNA(j) && ddl_dht2_comme(dn, d2, j, a, tol_v)
    # (le total d'un modèle à groupes : dht2 y calcule la variance du taux de rencontre sur les individus, Nuée sur les détections)
    tot_groupes <- isTRUE(mN$donnees$groupes) && nrow(dn) > 1
    res$rencontre <- proche(dn$se_rencontre[j]^2, d2$ER_var, 1e-9) || (tot_groupes && proche(dn$se_rencontre[j][-length(j)]^2, d2$ER_var[-length(j)], 1e-9))
    ok <- unlist(res)
    verifier(sprintf("%s : %s (Distance : %s), écart des paramètres %.1e%s%s%s — %s", nom, lab, libelle_mrds(mD$ddf), ec_par, if (spline) " [hasard sans ajustement : spline de mrds]" else "",
                     if (autre) sprintf(" [un autre optimum local : logL de Nuée %+.4f]", d_ll) else "", if (kappa > 100) sprintf(" [κ = %.0f]", kappa) else "",
                     if (all(ok)) paste0("le modèle, l'optimum, logL, hessien, p, adéquation, densité",
                                         if (!is.null(a$k)) sprintf(" ; au total, %.1f ddl pondérés (dht2 : %.2f, en ddl_dht2)", a$df[a$k], d2$df[a$k]) else "")
                     else paste(names(ok)[!ok], collapse = ", ")), all(ok))
    invisible(list(mN = mN, mD = mD, dI = dI))
  }
  cu_lkm <- Distance::convert_units("meter", "kilometer", "square kilometer")
  cu_pha <- Distance::convert_units("meter", NULL, "hectare")
  dat <- function(nom) { e <- new.env(); utils::data(list = nom, package = "Distance", envir = e); get(nom, envir = e) }
  lt <- dat("LTExercise")
  for (cfg in list(list("hn", "cos"), list("hr", "cos"), list("unif", "cos"), list("hn", "herm"), list("unif", "poly"))) {
    comparer(sprintf("LTExercise %s + %s (AIC)", cfg[[1]], cfg[[2]]), lt, list(truncation = 20, key = cfg[[1]], adjustment = cfg[[2]]),
             list(w = 20, cle = cfg[[1]], serie = cfg[[2]], strate = NULL), cu_lkm, strates = FALSE)
  }
  comparer("LTExercise hn + cos(2, 3) fixés", lt, list(truncation = 20, key = "hn", adjustment = "cos", nadj = 2), list(w = 20, cle = "hn", serie = "cos", aj = 2, strate = NULL), cu_lkm, strates = FALSE)
  comparer("LTExercise unif + cos, sans monotonie", lt, list(truncation = 20, key = "unif", adjustment = "cos", monotonicity = "none"),
           list(w = 20, cle = "unif", serie = "cos", mono = "aucune", strate = NULL), cu_lkm, strates = FALSE)
  pt <- dat("PTExercise")
  for (cfg in list(list("hn", "cos"), list("hr", "cos"), list("unif", "cos"))) {
    comparer(sprintf("PTExercise (points, densité seule) %s + %s", cfg[[1]], cfg[[2]]), pt, list(truncation = 20, transect = "point", key = cfg[[1]], adjustment = cfg[[2]]),
             list(transect = "point", w = 20, cle = cfg[[1]], serie = cfg[[2]], strate = NULL, effort = "Effort", unites = list(distance = "m", effort = "m", aire = "ha")), cu_pha, strates = FALSE)
  }
  duck <- dat("ducknest")
  for (cfg in list(list("hn", "cos"), list("unif", "cos"), list("unif", "poly"), list("hr", "cos"))) {
    comparer(sprintf("ducknest %s + %s", cfg[[1]], cfg[[2]]), duck, list(truncation = 2.4, key = cfg[[1]], adjustment = cfg[[2]]),
             list(w = 2.4, cle = cfg[[1]], serie = cfg[[2]], strate = NULL), cu_lkm, strates = FALSE)
  }
  comparer("ducknest hn + cos(2, 3) monotonie faible", duck, list(truncation = 2.4, key = "hn", adjustment = "cos", nadj = 2, monotonicity = "weak"),
           list(w = 2.4, cle = "hn", serie = "cos", aj = 2, mono = "faible", strate = NULL), cu_lkm, strates = FALSE)
  comparer("ducknest hr + cos(2, 3) fixés (b à sa borne 1)", duck, list(truncation = 2.4, key = "hr", adjustment = "cos", nadj = 2),
           list(w = 2.4, cle = "hr", serie = "cos", aj = 2, strate = NULL), cu_lkm, strates = FALSE)
  minke <- dat("minke")
  comparer("minke (deux strates) hn + cos", minke, list(truncation = 1.5), list(w = 1.5, unites = list(distance = "km", effort = "km", aire = "km2")), 1)
  comparer("minke (deux strates) hr + cos", minke, list(truncation = 1.5, key = "hr"), list(w = 1.5, cle = "hr", unites = list(distance = "km", effort = "km", aire = "km2")), 1)
  wren <- dat("wren_5min")
  comparer("wren_5min (points, 2 passages) hn + cos, troncature 110 m", wren, list(truncation = 110, transect = "point"),
           list(transect = "point", w = 110, strate = NULL, unites = list(distance = "m", effort = "m", aire = "ha")), cu_pha, strates = FALSE)
  comparer("wren_5min (points) hr + cos", wren, list(truncation = 110, transect = "point", key = "hr"),
           list(transect = "point", w = 110, cle = "hr", strate = NULL, unites = list(distance = "m", effort = "m", aire = "ha")), cu_pha, strates = FALSE)
  sparrow <- dat("Savannah_sparrow_1980")
  comparer("Savannah_sparrow_1980 (points, trois strates, 5 % tronqués)", sparrow, list(truncation = "5%", transect = "point"),
           list(transect = "point", pct = 5, unites = list(distance = "m", effort = "m", aire = "ha")), cu_pha)
  cluster <- dat("ClusterExercise")
  r <- comparer("ClusterExercise (groupes, deux strates)", cluster, list(truncation = 1.5), list(w = 1.5, taille = "size", unites = list(distance = "km", effort = "km", aire = "km2")), 1)
  gD <- attr(Distance::dht2(r$dI, flatfile = cluster, strat_formula = ~Region.Label, convert_units = 1), "grouped")
  gN <- r$mN$densite_groupes
  verifier("ClusterExercise : la densité des groupes (dht2 : attribut grouped), la taille moyenne et sa variance par strate",
           proche(gN$N, gD$Abundance, 1e-6) && proche(gN$se_N, gD$Abundance_se, 1e-6) && proche(gN$ddl, gD$df, 1e-6) &&
             proche(r$mN$densite$taille_moyenne[1:2], as.data.frame(Distance::dht2(r$dI, flatfile = cluster, strat_formula = ~Region.Label, convert_units = 1))$group_mean[1:2], 1e-12))
  caper <- dat("capercaillie")
  comparer("capercaillie (un seul transect : pas de variance du taux de rencontre)", caper, list(truncation = 60, key = "hn"),
           list(w = 60, strate = NULL, unites = list(distance = "m", effort = "km", aire = "ha")), Distance::convert_units("meter", "kilometer", "hectare"), strates = FALSE)

  # des données simulées
  s1 <- simuler(1)
  comparer("simulées, lignes, deux strates, hn + cos", s1, list(truncation = 100), list(w = 100), cu_lkm)
  comparer("simulées, lignes, hr + cos, 8 % tronqués", s1, list(truncation = "8%", key = "hr"), list(pct = 8, cle = "hr"), cu_lkm)
  comparer("simulées, lignes, hn + herm(4) fixé, ajustements à l'échelle de σ", s1, list(truncation = 100, key = "hn", adjustment = "herm", nadj = 1, scale = "scale"),
           list(w = 100, serie = "herm", aj = 1, echelle = "echelle"), cu_lkm)
  comparer("simulées, lignes, unif + poly", s1, list(truncation = 100, key = "unif", adjustment = "poly"), list(w = 100, cle = "unif", serie = "poly"), cu_lkm)
  comparer("simulées, lignes, tronquées à gauche (5 m) : la densité de dht2 × w / (w − L)", s1, list(truncation = list(left = 5, right = 100)), list(w = 100, gauche = 5), cu_lkm, gauche_lignes = TRUE)
  s2 <- simuler(2, point = TRUE, cle = "hr", sigma = 35, D = c(Nord = 80, Sud = 140))
  comparer("simulées, points (1 à 3 passages), deux strates, hn + cos", s2, list(truncation = 100, transect = "point"), list(transect = "point", w = 100), Distance::convert_units("meter", NULL, "square kilometer"))
  comparer("simulées, points, hr + cos, 10 % tronqués", s2, list(truncation = "10%", transect = "point", key = "hr"), list(transect = "point", pct = 10, cle = "hr"),
           Distance::convert_units("meter", NULL, "square kilometer"))
  comparer("simulées, points, tronqués à gauche (4 m)", s2, list(truncation = list(left = 4, right = 100), transect = "point"), list(transect = "point", w = 100, gauche = 4),
           Distance::convert_units("meter", NULL, "square kilometer"))
  s3 <- simuler(3, groupes = TRUE, D = c(Nord = 15, Sud = 25))
  comparer("simulées, lignes, groupes, deux strates", s3, list(truncation = 100), list(w = 100, taille = "size"), cu_lkm)
  s4 <- simuler(4)
  s4$Area <- 0
  comparer("simulées, lignes, aires nulles : la densité seule, le total pondéré par les aires couvertes", s4, list(truncation = 100), list(w = 100), cu_lkm)
  # la monotonie qui agit (la bosse) : l'optimiseur de mrds (SLSQP, ftol 1e-6) s'arrête près de l'optimum de Nuée
  df_b <- data.frame(Region.Label = "A", Area = 100, Sample.Label = rep(1:10, length.out = 250), Effort = 2, object = 1:250, distance = bosse)
  for (cfg in list(list("hn", "cos", "weak", "faible"), list("unif", "cos", "strict", "stricte"), list("unif", "poly", "strict", "stricte"))) {
    comparer(sprintf("une bosse, %s + %s, monotonie %s (contrainte active)", cfg[[1]], cfg[[2]], cfg[[4]]), df_b, list(truncation = 1, key = cfg[[1]], adjustment = cfg[[2]], monotonicity = cfg[[3]]),
             list(w = 1, cle = cfg[[1]], serie = cfg[[2]], mono = cfg[[4]], strate = NULL, unites = list(distance = "km", effort = "km", aire = "km2")), 1, strates = FALSE)
  }
  # des distances par intervalles (ds(cutpoints = )) : la vraisemblance multinomiale, le khi² sur ces classes
  cp_lt <- c(0, 2.5, 5, 7.5, 10, 15, 20)
  for (k in c("hn", "hr", "unif")) {
    comparer(sprintf("LTExercise par intervalles (6 classes), %s + cos", k), lt, list(cutpoints = cp_lt, key = k), list(cle = k, strate = NULL, intervalles = cp_lt), cu_lkm, strates = FALSE)
  }
  comparer("ducknest par intervalles (8 classes), unif + cos", duck, list(cutpoints = seq(0, 2.4, 0.3), key = "unif"), list(cle = "unif", strate = NULL, intervalles = seq(0, 2.4, 0.3)), cu_lkm, strates = FALSE)
  comparer("PTExercise par intervalles (4 classes), points, hn + cos", pt, list(cutpoints = c(0, 5, 10, 15, 20), transect = "point"),
           list(transect = "point", strate = NULL, intervalles = c(0, 5, 10, 15, 20), unites = list(distance = "m", effort = "m", aire = "ha")), cu_pha, strates = FALSE)
  comparer("simulées, points par intervalles, hr + cos, deux strates", s2, list(cutpoints = c(0, 10, 20, 35, 50, 70, 100), transect = "point", key = "hr"),
           list(transect = "point", cle = "hr", intervalles = c(0, 10, 20, 35, 50, 70, 100)), Distance::convert_units("meter", NULL, "square kilometer"))
  comparer("simulées, lignes par intervalles, la première borne à 5 m (troncature à gauche)", s1, list(truncation = list(left = 5, right = 100), cutpoints = c(5, 20, 40, 60, 80, 100)),
           list(intervalles = c(5, 20, 40, 60, 80, 100)), cu_lkm, gauche_lignes = TRUE)
  # le hasard sans ajustement sur des lignes : mrds intègre la clé par un spline de lissage (gstdint), Nuée exactement
  r <- comparer("LTExercise hr seul", lt, list(truncation = 20, key = "hr", adjustment = NULL), list(w = 20, cle = "hr", serie = "aucune", strate = NULL), cu_lkm, strates = FALSE)
  dD <- r$mD$ddf
  fd <- r$mN$detection$fd
  x <- r$mN$donnees$x
  I_mrds <- mrds:::integratepdf(dD$ds$aux$ddfobj, select = c(TRUE, rep(FALSE, length(x) - 1)), width = 20, int.range = c(0, 20), standardize = FALSE, point = FALSE, left = 0)
  I_exact <- integrale_detection(fd, dD$par, 0, 20)$I
  verifier(sprintf("hasard seul sur des lignes : l'écart des log-vraisemblances aux paramètres de mrds est n ln(∫ de mrds / ∫ exacte) (%.2e), et l'optimum exact de Nuée la dépasse",
                   dD$lnl - vraisemblance_detection(fd, dD$par, x)$ll),
           proche(dD$lnl - vraisemblance_detection(fd, dD$par, x)$ll, -length(x) * log(I_mrds / I_exact), 1e-6) && r$mN$detection$ll > vraisemblance_detection(fd, dD$par, x)$ll)

  # ---------------------------------------------------------------- avec des covariables
  cat("Contre Distance (ds(formula = ), dht2) et mrds, avec des covariables : l'optimum, puis les calculs aux paramètres de Nuée\n")
  # Une comparaison d'un modèle à covariables, comme comparer : l'optimum (le même, ou un autre admis
  # s'il ne cède rien à celui de mrds), puis aux paramètres de Nuée la log-vraisemblance, le hessien, la
  # probabilité de chaque détection (predict.ds, rangée par objet), p̄ et son erreur-type (summary.ds),
  # l'effectif couvert Σ 1/pi et le sien, l'adéquation (ddf.gof : la répartition de chaque distance
  # sachant ses covariables, les comptes attendus sommés sur les détections), la fonction de détection
  # moyenne et celle de chaque catégorie (plot.ds : les g(x | zi) de detfct pondérés par 1/pi), et la
  # densité de Horvitz–Thompson de dht2. Les erreurs-types comparées à 3e-6 κ près (3e-5 au moins), κ
  # le conditionnement de l'information des coefficients réduits (reduction_covariables : sans
  # l'unité des covariables) ; `spline` : le hasard sans ajustement sur des lignes (gstdint).
  comparer_covariables <- function(nom, df, args, nuee, cu, strates = TRUE, gauche_lignes = FALSE) {
    mD <- do.call(ds_de, c(list(df), args))
    p <- projet_distance(df)
    mN <- do.call(distance_de, c(list(p), nuee))
    det <- mN$detection
    spline <- det$fd$cle == "hr" && !length(det$fd$ordres) && !det$fd$point && det$fd$L == 0
    tol_f <- if (spline) 1e-3 else 1e-9
    res <- list()
    dD <- mD$ddf
    res$modele <- identical(libelle_detection(sans_covariables(det$fd)), libelle_mrds(dD))
    ec_par <- max(abs(det$theta - dD$par) / pmax(1, abs(dD$par)))
    d_ll <- det$ll - dD$lnl
    meme <- ec_par < 5e-3 && (abs(d_ll) < 1e-6 * abs(dD$lnl) || spline)
    autre <- !meme && !spline && d_ll > -0.005
    res$optimum <- meme || autre || (spline && abs(d_ll) < 0.1)
    R <- reduction_covariables(det$fd)
    ev <- eigen(crossprod(R, det$H %*% R), symmetric = TRUE, only.values = TRUE)$values
    kappa <- max(ev) / min(ev)
    tol_v <- if (spline) 1e-2 else max(3e-5, 3e-6 * kappa)
    dI <- injecter(dD, det$theta)
    sI <- summary(dI)
    res$logL <- proche(det$ll, dI$lnl, tol_f)
    res$hessien <- proche(det$H, dI$hessian, tol_v)
    obj <- df$object[mN$donnees$uid]
    # (la probabilité de chaque détection : mrds intègre exactement la demi-normale seule, les autres avec
    # integrate() à 1e-7 ; leur moyenne p̄ s'en ressent moins)
    tol_pi <- if (spline) 1e-3 else if (det$fd$cle == "hn" && !length(det$fd$ordres)) 1e-9 else 1e-7
    res$p_detections <- proche(mN$p_detections[order(obj)], dI$fitted[order(as.numeric(names(dI$fitted)))], tol_pi)
    res$p <- proche(mN$p, sI$average.p, tol_f) && proche(mN$se_p, sI$average.p.se, tol_v)
    my <- moyenne_detection(probabilites_covariables(det$fd, det$theta), det$V)
    res$couvert <- proche(my$N, sI$Nhat, tol_f) && proche(my$se_N, sI$Nhat.se, tol_v)
    g <- suppressMessages(mrds::ddf.gof(dI, qq = FALSE))
    res$adequation <- (if (is.null(mN$adequation$cvm)) is.null(g$dsgof)
                       else proche(mN$adequation$cvm$W, g$dsgof$CvM$W, if (spline) 1e-2 else 1e-5) && proche(mN$adequation$cvm$p, g$dsgof$CvM$p, if (spline) 1e-2 else 1e-6)) &&
      proche(mN$adequation$khi2$chi2, g$chisquare$chi1$chisq, if (spline) 1e-2 else 1e-6) &&
      proche(mN$adequation$khi2$classes$attendu, g$chisquare$chi1$expected, if (spline) 1e-2 else 1e-6) &&
      identical(as.numeric(mN$adequation$khi2$classes$observe), as.numeric(g$chisquare$chi1$observed))
    ddfobj <- dI$ds$aux$ddfobj
    moyenne_mrds <- function(dans) vapply(mN$courbe$x, function(x0) {
      gx <- mrds:::detfct(rep(x0, sum(dans)), ddfobj, select = dans, width = mN$donnees$w)
      sum(gx / dI$fitted[dans]) / sum(1 / dI$fitted[dans])
    }, 0)
    res$courbes <- proche(mN$courbe$g, moyenne_mrds(rep(TRUE, nrow(ddfobj$xmat))), tol_f) &&
      all(vapply(mN$courbes_niveaux, function(cn) proche(cn$g, unlist(lapply(cn$niveaux, function(l) moyenne_mrds(ddfobj$xmat[[cn$covariable]] == l))), tol_f), TRUE))
    d2 <- as.data.frame(Distance::dht2(dI, flatfile = df, strat_formula = if (strates) ~Region.Label else ~1, convert_units = cu))
    dens <- attr(Distance::dht2(dI, flatfile = df, strat_formula = if (strates) ~Region.Label else ~1, convert_units = cu), "density")
    dn <- mN$densite
    lab_d <- if (strates) as.character(d2$Region.Label) else "Total"
    lab_n <- ifelse(is.na(dn$strate), "Total", dn$strate)
    if (nrow(dn) == 1) lab_n <- lab_d[1]
    j <- match(lab_d, lab_n)
    seule <- mN$donnees$densite_seule
    a <- attendu_dht2(d2, seule && nrow(dn) > 1)
    f_g <- if (gauche_lignes) (mN$donnees$w - mN$donnees$L) / mN$donnees$w else 1
    res$densite <- !anyNA(j) && proche((if (seule) dn$D[j] else dn$N[j]) * f_g, d2$Abundance, tol_f) && proche(dn$cv[j], d2$Abundance_CV, tol_v) &&
      proche((if (seule) dn$se_D[j] else dn$se_N[j]) * f_g, d2$Abundance_se, tol_v) && proche(dn$ddl[j], a$df, tol_v) &&
      proche((if (seule) dn$bas_D[j] else dn$bas_N[j]) * f_g, a$LCI, tol_v) && proche((if (seule) dn$haut_D[j] else dn$haut_N[j]) * f_g, a$UCI, tol_v) &&
      proche(dn$rencontre[j], d2$ER, 1e-12) && identical(as.numeric(dn$k[j]), as.numeric(d2$k)) && identical(as.numeric(dn$n[j]), as.numeric(d2$n)) &&
      proche(dn$effort[j], d2$Effort, 1e-12) && proche(dn$couverte[j] / f_g, d2$Covered_area, 1e-12) &&
      (if (!seule) proche(dn$D[j] * f_g, dens$Density, tol_f) && proche(dn$se_D[j] * f_g, dens$Density_se, tol_v) else TRUE)
    res$ddl_dht2 <- !anyNA(j) && ddl_dht2_comme(dn, d2, j, a, tol_v)
    tot_groupes <- isTRUE(mN$donnees$groupes) && nrow(dn) > 1
    res$rencontre <- proche(dn$se_rencontre[j]^2, d2$ER_var, 1e-9) || (tot_groupes && proche(dn$se_rencontre[j][-length(j)]^2, d2$ER_var[-length(j)], 1e-9))
    ok <- unlist(res)
    verifier(sprintf("%s : %s (Distance : %s ~ …), écart des paramètres %.1e%s%s%s — %s", nom, libelle_detection(det$fd), libelle_mrds(dD), ec_par,
                     if (spline) " [hasard sans ajustement : spline de mrds]" else "", if (autre) sprintf(" [un autre optimum : logL de Nuée %+.4f]", d_ll) else "",
                     if (kappa > 100) sprintf(" [κ = %.0f]", kappa) else "",
                     if (all(ok)) paste0("l'optimum, logL, hessien, pi, p̄, Σ 1/pi, adéquation, courbes moyennes, densité",
                                         if (!is.null(a$k)) sprintf(" ; au total, %.1f ddl pondérés (dht2 : %.2f, en ddl_dht2)", a$df[a$k], d2$df[a$k]) else "")
                     else paste(names(ok)[!ok], collapse = ", ")), all(ok))
    invisible(list(mN = mN, mD = mD, dI = dI, d_ll = d_ll))
  }
  am <- dat("amakihi")
  # (des transects uniques : leurs étiquettes se répètent d'une strate à l'autre, et dht2 ne garde un
  # transect dont toutes les détections sont au-delà de la troncature que si son Sample.Label n'a de
  # détection dans aucune strate)
  am$Sample.Label <- paste(am$Region.Label, am$Sample.Label, sep = "-")
  u_am <- list(distance = "m", effort = "m", aire = "ha")
  comparer_covariables("amakihi (points, 7 strates, densité seule) hr ~ OBs + MAS", am, list(transect = "point", key = "hr", formula = ~OBs + MAS, truncation = 82.5),
                       list(transect = "point", w = 82.5, cle = "hr", z = c("OBs", "MAS"), unites = u_am), cu_pha)
  comparer_covariables("amakihi hn ~ OBs + MAS", am, list(transect = "point", key = "hn", formula = ~OBs + MAS, truncation = 82.5),
                       list(transect = "point", w = 82.5, z = c("OBs", "MAS"), unites = u_am), cu_pha)
  comparer_covariables("amakihi hn + cos(2) ~ OBs (un terme fixé, sans contrainte de monotonie)", am, list(transect = "point", key = "hn", formula = ~OBs, truncation = 82.5, nadj = 1),
                       list(transect = "point", w = 82.5, aj = 1, z = "OBs", unites = u_am), cu_pha)
  cp_am <- c(0, 10, 20, 30, 40, 50, 60, 70, 82.5)
  comparer_covariables("amakihi par intervalles (8 classes), hn ~ OBs", am, list(transect = "point", key = "hn", formula = ~OBs, cutpoints = cp_am),
                       list(transect = "point", z = "OBs", intervalles = cp_am, unites = u_am), cu_pha)
  comparer_covariables("amakihi tronqués à gauche (5 m), hn + herm(4) à l'échelle de σ ~ OBs", am,
                       list(transect = "point", key = "hn", formula = ~OBs, truncation = list(left = 5, right = 82.5), adjustment = "herm", nadj = 1, scale = "scale"),
                       list(transect = "point", w = 82.5, gauche = 5, serie = "herm", aj = 1, echelle = "echelle", z = "OBs", unites = u_am), cu_pha)
  etp <- dat("ETP_Dolphin")
  # (les facteurs de Distance, factor(Cue.type) : des catégories dans l'ordre des nombres)
  etp$Cue <- factor(etp$Cue.type, levels = sort(unique(etp$Cue.type)))
  etp$Beauf <- factor(etp$Beauf.class, levels = sort(unique(etp$Beauf.class)))
  u_etp <- list(distance = "nmi", effort = "nmi", aire = "nmi2")
  comparer_covariables("ETP_Dolphin (lignes, un transect, groupes) hn ~ Cue + LnCluster", etp, list(key = "hn", formula = ~Cue + LnCluster, truncation = 5),
                       list(w = 5, z = c("Cue", "LnCluster"), taille = "size", strate = NULL, unites = u_etp), 1, strates = FALSE)
  comparer_covariables("ETP_Dolphin hr ~ Beauf", etp, list(key = "hr", formula = ~Beauf, truncation = 5),
                       list(w = 5, cle = "hr", z = "Beauf", taille = "size", strate = NULL, unites = u_etp), 1, strates = FALSE)
  comparer_covariables("ETP_Dolphin hr + cos(2) ~ Cue", etp, list(key = "hr", formula = ~Cue, truncation = 5, nadj = 1),
                       list(w = 5, cle = "hr", aj = 1, z = "Cue", taille = "size", strate = NULL, unites = u_etp), 1, strates = FALSE)
  cp_etp <- c(0, 0.5, 1, 1.5, 2, 3, 4, 5)
  comparer_covariables("ETP_Dolphin par intervalles (7 classes), hn + cos(2) ~ Beauf", etp, list(key = "hn", formula = ~Beauf, cutpoints = cp_etp, nadj = 1),
                       list(aj = 1, z = "Beauf", taille = "size", intervalles = cp_etp, strate = NULL, unites = u_etp), 1, strates = FALSE)
  comparer_covariables("ETP_Dolphin tronqués à gauche (0,1 nmi), hn ~ Beauf : la densité de dht2 × w / (w − L)", etp, list(key = "hn", formula = ~Beauf, truncation = list(left = 0.1, right = 5)),
                       list(w = 5, gauche = 0.1, z = "Beauf", taille = "size", strate = NULL, unites = u_etp), 1, strates = FALSE, gauche_lignes = TRUE)
  r <- comparer_covariables("ClusterExercise (deux strates et leurs aires) hn ~ size : la taille des groupes en covariable", cluster, list(truncation = 1.5, formula = ~size),
                            list(w = 1.5, taille = "size", z = "size", unites = list(distance = "km", effort = "km", aire = "km2")), 1)
  # la densité des groupes : dht2 (varNhat) recalcule la probabilité de chaque détection avec des tailles
  # de 1 pour la méthode delta de son attribut « grouped » ; Nuée garde celle de chaque détection
  # sachant la taille de son groupe
  gD <- attr(Distance::dht2(r$dI, flatfile = cluster, strat_formula = ~Region.Label, convert_units = 1), "grouped")
  gN <- r$mN$densite_groupes
  fdc <- r$mN$detection$fd
  Z1 <- fdc$Z
  Z1[, "size"] <- 1
  pr1 <- probabilites_covariables(avec_covariables(sans_covariables(fdc), Z1), r$mN$detection$theta)
  t_cl <- r$mN$donnees
  var_un <- vapply(t_cl$strates$strate, function(s) {
    i <- t_cl$strate_x == s
    a <- gN$couverte[match(s, gN$strate)]
    gr <- -(t_cl$strates$aire[t_cl$strates$strate == s] / a) * colSums(pr1$dp[i, , drop = FALSE] / pr1$p[i]^2)
    drop(crossprod(gr, r$mN$detection$V %*% gr))
  }, 0)
  verifier("ClusterExercise ~ size, la densité des groupes : l'effectif de dht2 (attribut grouped) ; sa variance de détection, celle de dht2 quand la probabilité de chaque détection est recalculée avec une taille de 1, comme dht2 le fait",
           proche(gN$N, gD$Abundance, 1e-9) && proche(var_un, gD$df_var[match(t_cl$strates$strate, as.character(gD$Region.Label))], 1e-6))
  un <- dat("unimak")
  r <- comparer_covariables("unimak (lignes, une strate) hn ~ MSTDO + Hour", un, list(key = "hn", formula = ~MSTDO + Hour),
                            list(cle = "hn", z = c("MSTDO", "Hour"), strate = NULL, unites = list(distance = "km", effort = "km", aire = "km2")), 1, strates = FALSE)
  mD2 <- ds_de(un, key = "hn", formula = ~MSTDO + Hour, initial_values = list(scale = r$mN$detection$theta))
  verifier(sprintf("unimak : ds s'arrête sur un plateau (σ vers e^%.0f, une demi-normale plate : son gradient s'y annule), %.1f de log-vraisemblance sous le maximum de Nuée ; parti des paramètres de Nuée, ds y reste",
                   max(drop(r$mD$ddf$ds$aux$ddfobj$scale$dm %*% r$mD$ddf$par)), r$d_ll),
           r$d_ll > 20 && proche(mD2$ddf$lnl, r$mN$detection$ll, 1e-9))
  gt <- dat("golftees")
  gt <- gt[gt$observer == 1 & gt$detected == 1, ]  # (le premier observateur seul : ds sur des données d'observateurs doubles)
  gt$sexe <- factor(ifelse(gt$sex == 1, "male", "femelle"), levels = c("femelle", "male"))
  comparer_covariables("golftees (un transect, une aire, groupes) hn ~ sexe + exposure (une qualitative, une quantitative 0/1)", gt, list(key = "hn", formula = ~sexe + exposure, truncation = 4),
                       list(w = 4, z = c("sexe", "exposure"), taille = "size", unites = list(distance = "m", effort = "m", aire = "m2")), 1)
}

# ---------------------------------------------------------------- refus

cat("Refus\n")
mal <- function(f) { x <- sim; f(x) }
refus_de <- function(df, ...) refus(distance_de(projet_distance(df), ...))
verifier("une distance négative, un effort nul ou manquant, deux efforts pour un transect : refusés, l'observation ou le transect nommés",
         grepl("r0003 vaut -2", refus_de(mal(function(x) { x$distance[3] <- -2; x }), w = 100)) &&
           grepl("r0004 vaut 0", refus_de(mal(function(x) { x$Effort[4] <- 0; x }), w = 100)) &&
           grepl("r0005 n'a pas d'effort", refus_de(mal(function(x) { x$Effort[5] <- NA; x }), w = 100)) &&
           grepl(sprintf("Le transect %s a deux efforts", sim$Sample.Label[1]), refus_de(mal(function(x) { x$Effort[1] <- x$Effort[1] + 1; x }), w = 100)))
verifier("deux aires pour une strate, une aire nulle parmi d'autres, une distance sans transect : refusées",
         grepl("La strate Nord a deux aires", refus_de(mal(function(x) { x$Area[1] <- 61; x }), w = 100)) &&
           grepl("L'aire de la strate Nord est nulle", refus_de(mal(function(x) { x$Area[x$Region.Label == "Nord"] <- 0; x }), w = 100)) &&
           grepl("r0002 a une distance mais pas de transect", refus_de(mal(function(x) { x$Sample.Label[2] <- NA; x }), w = 100)))
verifier("une troncature sous la troncature à gauche, un pourcentage hors de [0, 100), moins de deux distances, Y qualitative, une variable X",
         grepl("doit dépasser", refus_de(sim, w = 3, gauche = 5)) && grepl("pourcentage", refus_de(sim, pct = 100)) && grepl("Moins de deux", refus_de(sim, w = 0.05)) &&
           grepl("qualitative", refus(ajuster(ps, definir_modele(ps, "DISTANCE", x = NULL, y = numeros(ps, "Region.Label"), echelle = "none", options = definition_distance(ps)$options)))) &&
           grepl("X", refus(ajuster(ps, definir_modele(ps, "DISTANCE", x = numeros(ps, "Area"), y = numeros(ps, "distance"), echelle = "none", options = definition_distance(ps)$options)))))
verifier("des lignes sans effort, sans transect : refusées ; une taille de groupe manquante pour une détection",
         grepl("longueur de la ligne", refus_de(sim, effort = NULL)) && grepl("transect de chaque ligne", refus(ajuster(ps, definir_modele(ps, "DISTANCE", x = NULL, y = numeros(ps, "distance"), echelle = "none", options = list(transect = "ligne"))))) &&
           grepl("r0001 n'a pas de taille", refus_de(mal(function(x) { x$size[1] <- NA; x }), w = 100, taille = "size")))
set.seed(8)
verifier("un modèle qui ne converge pas (une demi-normale devenue plate : des distances plus nombreuses au loin) : refusé, avec la raison",
         grepl("ne converge pas.*infini", refus_de(transform(sim, distance = ifelse(is.na(distance), NA, round(20 * sqrt(stats::runif(nrow(sim))), 2))), w = 20, serie = "aucune")))
egales <- mal(function(x) { x$distance[!is.na(x$distance)] <- 10; x })
verifier("des distances toutes égales : la fonction de détection ne converge pas (une information nulle), refusée avec la raison",
         grepl("ne disent rien", refus_de(egales, w = 100)))
vide <- rbind(sim, data.frame(Region.Label = "Vide", Area = 20, Sample.Label = c("V-1", "V-2"), Effort = c(1, 2), object = NA, distance = NA, size = NA))
mv <- distance_de(projet_distance(vide), w = 100)
verifier("une strate sans aucune détection : D = 0, erreur-type 0, ni CV ni ddl (comme dht2) ; le total des trois strates",
         identical(mv$densite$strate[3], "Vide") && mv$densite$D[3] == 0 && mv$densite$se_D[3] == 0 && is.na(mv$densite$cv[3]) && is.na(mv$densite$ddl[3]) &&
           proche(mv$densite$N[4], sum(mv$densite$N[1:3]), 1e-12) && mv$densite$k[4] == 20)
options(nuee.langue = "en")
verifier("les refus en anglais", grepl("^The distances .* are positive or zero: r0003 is -2", refus_de(mal(function(x) { x$distance[3] <- -2; x }), w = 100)) &&
           grepl("^Transect .* has two efforts", refus_de(mal(function(x) { x$Effort[1] <- x$Effort[1] + 1; x }), w = 100)))
options(nuee.langue = "fr")
verifier("un jeu de test refusé (l'échantillonnage par distances ne prédit pas)",
         grepl("jeu de test", refus(definir_modele(ps, "DISTANCE", x = NULL, y = numeros(ps, "distance"), echelle = "none", test = 1:5, options = definition_distance(ps)$options))))
# des covariables refusées
k_sc <- which(!is.na(sc$distance) & sc$distance <= 100)
sc_plat <- sc
sc_plat$distance[k_sc][sc$observateur[k_sc] == "C"] <- round(100 * sqrt(stats::runif(sum(sc$observateur[k_sc] == "C"))), 1)  # (des distances plus nombreuses au loin pour C)
verifier("des covariables refusées : une valeur manquante pour une détection (nommée), une seule catégorie parmi les détections, une valeur constante, deux covariables liées, la clé uniforme (comme ds) ; une catégorie dont l'échelle σ part à l'infini, nommée",
         grepl(sprintf("r%04d n'a pas de valeur de la covariable vent", k_sc[2]), refus_de(transform(sc, vent = replace(vent, k_sc[2], NA)), w = 100, z = "vent")) &&
           grepl("n'a qu'une catégorie", refus_de(transform(sc, observateur = ifelse(is.na(observateur), NA, "A")), w = 100, z = "observateur")) &&
           grepl("la même valeur pour toutes", refus_de(transform(sc, vent = ifelse(is.na(vent), NA, 2)), w = 100, z = "vent")) &&
           grepl("liées entre elles", refus_de(transform(sc, vent2 = 2 * vent), w = 100, z = c("vent", "vent2"))) &&
           grepl("La clé uniforme ne prend pas de covariables", refus_de(sc, w = 100, cle = "unif", z = "vent")) &&
           grepl("l'échelle σ de observateur = C part à l'infini", refus_de(sc_plat, w = 100, serie = "aucune", z = "observateur")))

# ---------------------------------------------------------------- observations, interface, textes

cat("Observations choisies, ce que l'interface reçoit, texte des méthodes, tableaux\n")
mc <- distance_de(ps, w = 100, condition = "Region.Label == \"Nord\"")
mm <- distance_de(ps, w = 100, observations = which(sim$Region.Label == "Nord"))
verifier("une condition (Region.Label == \"Nord\") : le modèle des lignes cochées à la main, une seule strate",
         identical(sort(mc$observations), sort(mm$observations)) && proche(mc$detection$theta, mm$detection$theta, 1e-12) && nrow(mc$densite) == 1 &&
           identical(mc$donnees$strates$strate, "Nord"))
verifier("la vérification avant l'ajustement ne signale pas les distances vides des transects sans détection", identical(verifier_donnees(ps, definition_distance(ps)), verifier_distance(ps, NULL)))
pid <- ps
pid$observation_id_names <- c(pid$observation_id_names, "ligne")
pid$observations <- lapply(seq_along(pid$observations), function(k) { o <- pid$observations[[k]]; o$ids <- c(o$ids, sim$Sample.Label[k]); o })
mid <- distance_de(pid, w = 100, echantillon = "id:ligne")
verifier("le transect donné par un identifiant des observations : le même modèle que par la variable", proche(mid$densite$N, distance_de(ps, w = 100)$densite$N, 1e-12) &&
           identical(mid$donnees$noms$transect, "ligne"))
pp <- projet_distance(simuler(2, point = TRUE))
mp <- distance_de(pp, transect = "point", w = 100, effort = NULL, unites = list(distance = "m", effort = "m", aire = "ha"))
verifier("des points sans variable d'effort : un passage par point", all(mp$donnees$transects$effort == 1) && identical(mp$effectif$quoi, "EDR"))

etat <- new.env()
etat$projets <- list(P1 = c(ps, list(id = "P1")))
class(etat$projets$P1) <- class(ps)
etat$modeles <- list()
projet <- function(id) etat$projets[[id]]
modele <- function(id) etat$modeles[[id]]
garder <- function(id, d, m) { etat$modeles[[id]] <- list(projet = "P1", definition = d, modele = m); id }
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "columns", na = "null", null = "null", digits = NA, force = TRUE)
dg <- definition_distance(ps, w = 100, taille = "size")
id <- garder("M1", dg, ajuster(ps, dg))
r <- resultat_distance(id)
verifier("ce que l'interface reçoit : une analyse sans prédiction, ses vues, ses lignes, la courbe, l'histogramme, le diagramme quantile-quantile, la densité, en JSON",
         isTRUE(r$analyse) && identical(r$prediction, FALSE) && identical(as.character(r$vues_defaut), c("distance_detection", "distance_densite")) &&
           grepl("^hn.* · D ", r$chiffres) && nrow(r$observations) == r$N && all(c("uid", "nom", "transect", "strate", "distance", "dans_troncature", "g", "taille") %in% names(r$observations)) &&
           nrow(r$courbe) == 101 && sum(r$histogramme$compte) == r$n && nrow(r$adequation$qq) == r$n && nrow(r$densite) == 3 && inherits(json(r), "json"))
db <- definition_distance(ps, intervalles = c(0, 20, 40, 60, 80, 100))
etat$modeles$M2 <- list(projet = "P1", definition = db, modele = ajuster(ps, db))
rb <- resultat_distance("M2")
tb <- texte_methodes(ps, db, etat$modeles$M2$modele, "fr", "essai")
verifier("des distances par intervalles : les bornes font la troncature, le khi² sur ces classes, ni Cramér–von Mises ni diagramme quantile-quantile ; le texte des méthodes, les tableaux ; des bornes qui ne croissent pas refusées",
         identical(as.numeric(rb$intervalles), c(0, 20, 40, 60, 80, 100)) && is.null(rb$adequation$cvm) && is.null(rb$adequation$qq) && nrow(rb$histogramme) == 5 &&
           rb$troncature$w == 100 && inherits(json(rb), "json") && grepl("5 intervalles", tb$paragraphes[1]) && !grepl("Cramér", tb$paragraphes[2]) &&
           identical(tables_resultats(ps, etat$modeles$M2$modele)$adequation$test, "khi2") &&
           grepl("bornes croissantes", refus(distance_de(ps, intervalles = c(0, 50, 40, 100)))))
verifier("ses réglages pour Modifier le modèle (les options, les distances en Y)", identical(r$reglages$options$cle, "hn") && identical(as.numeric(r$reglages$y), numeros(ps, "distance")) &&
           identical(as.numeric(r$reglages$options$taille), numeros(ps, "size")))
options(nuee.langue = "en")
verifier("en anglais, sa ligne de l'explorateur (le point décimal)", grepl("^hn.* · D [0-9]+\\.[0-9]+ /km² \\(CV [0-9.]+%\\)$", resultat_distance("M1")$chiffres))
options(nuee.langue = "fr")
for (langue in c("fr", "en")) {
  fr <- langue == "fr"
  tm <- texte_methodes(ps, dg, etat$modeles$M1$modele, langue, "essai")
  mpt <- distance_de(pp, transect = "point", pct = 10, cle = "hr", unites = list(distance = "m", effort = "m", aire = "ha"))
  tp <- texte_methodes(pp, definition_distance(pp, transect = "point", pct = 10, cle = "hr"), mpt, langue, "essai")
  verifier(sprintf("texte des méthodes (%s) : les données, la fonction de détection et son choix, p et ESW (EDR pour des points), l'adéquation, la densité et ses variances, les références", langue),
           length(tm$paragraphes) == 4 && grepl(if (fr) "transects linéaires" else "line transects", tm$paragraphes[1]) && grepl("ds\\(\\)", tm$paragraphes[2]) &&
             grepl("AIC", tm$paragraphes[2]) && grepl("Cramér–von Mises", tm$paragraphes[2]) && grepl(if (fr) "demi-largeur effective" else "effective strip half-width", tm$paragraphes[2]) &&
             grepl("R2", tm$paragraphes[3]) && grepl("Satterthwaite", tm$paragraphes[3]) && grepl(if (fr) "groupes" else "groups", tm$paragraphes[3]) && grepl("km²", tm$paragraphes[3]) &&
             grepl(if (fr) "rayon effectif" else "effective detection radius", tp$paragraphes[2]) && grepl("P2", tp$paragraphes[3]) && grepl("10", tp$paragraphes[1]) &&
             any(grepl("^Buckland", tm$references)) && any(grepl("^Miller", tm$references)) && any(grepl("^Fewster", tm$references)) && any(grepl("R Core Team", tm$references)))
}
# sans aire de strate : le texte des méthodes dit les degrés de liberté du total, de Satterthwaite avec
# les poids des strates, et ceux de dht2, qui laisse les poids de côté (densite_echantillonnage), avec
# leurs nombres ; avec des aires, il n'en dit rien ; des strates d'un seul transect (sans variance du
# taux de rencontre, les deux sont les mêmes) : la pondération seulement
dsa <- definition_distance(ps, aire = NULL)
msa <- ajuster(ps, dsa)
tot_sa <- msa$densite[nrow(msa$densite), ]
p_un <- projet_distance(sim[sim$Sample.Label %in% c("Nord-01", "Sud-01"), ])
d_un <- definition_distance(p_un, w = 100, aire = NULL)
m_un <- ajuster(p_un, d_un)
for (langue in c("fr", "en")) {
  fr <- langue == "fr"
  ts <- texte_methodes(ps, dsa, msa, langue, "essai")$paragraphes[3]
  ta <- texte_methodes(ps, dg, etat$modeles$M1$modele, langue, "essai")$paragraphes[3]
  tu <- texte_methodes(p_un, d_un, m_un, langue, "essai")$paragraphes[3]
  marque <- if (fr) "Sans aire de strate, la densité totale" else "Without stratum areas, the overall density"
  n_ddl <- nombre_texte(tot_sa$ddl, 1, fr)
  n_dht2 <- nombre_texte(tot_sa$ddl_dht2, 1, fr)
  verifier(sprintf("sans aire de strate (%s) : le texte des méthodes dit les %s degrés de liberté du total, de Satterthwaite avec les poids des strates, et les %s de dht2 ; avec des aires, rien ; des strates d'un seul transect, la pondération seulement",
                   langue, n_ddl, n_dht2),
           nrow(msa$densite) == 3 && is.na(tot_sa$N) && isTRUE(tot_sa$ddl > tot_sa$ddl_dht2) && grepl(marque, ts, fixed = TRUE) &&
             grepl(sprintf(if (fr) "ses %s degrés de liberté sont ceux de la formule de Satterthwaite avec ces poids" else "its %s degrees of freedom are those of Satterthwaite's formula with these weights", n_ddl), ts, fixed = TRUE) &&
             grepl(sprintf(if (fr) "dht2 les laisse hors du dénominateur et n'en donne que %s" else "dht2 leaves them out of the denominator and gives only %s", n_dht2), ts, fixed = TRUE) &&
             !grepl(marque, ta, fixed = TRUE) && all(m_un$densite$k[1:2] == 1) && all(is.na(m_un$densite$ddl_dht2)) &&
             grepl(sprintf(if (fr) "%s est la moyenne des strates pondérée par leurs aires couvertes." else "%s is the strata's mean weighted by their covered areas.", marque), tu, fixed = TRUE) &&
             !grepl("Satterthwaite's formula with|formule de Satterthwaite avec", tu))
}
# un modèle à covariables : ce que l'interface reçoit, ses textes, ses tableaux
etat$projets$P2 <- c(psc, list(id = "P2"))
class(etat$projets$P2) <- class(psc)
dz <- definition_distance(psc, w = 100, taille = "size", z = c("observateur", "vent"))
etat$modeles$M3 <- list(projet = "P2", definition = dz, modele = msc)
rz <- resultat_distance("M3")
cn <- rz$courbes_niveaux[[1]]
verifier("un modèle à covariables, ce que l'interface reçoit : ses covariables et la référence, la probabilité de chaque détection (liste des observations), la fonction de détection moyenne et celle de chaque catégorie de l'observateur, les coefficients, en JSON",
         identical(rz$formule, "observateur + vent") && isTRUE(rz$p_moyenne) && identical(rz$covariables$reference, c("A", NA)) && identical(rz$covariables$qualitative, c(TRUE, FALSE)) &&
           "p" %in% names(rz$observations) && sum(!is.na(rz$observations$p)) == rz$n && identical(as.character(cn$niveaux), c("A", "B", "C")) &&
           length(cn$g) == 3 && all(lengths(cn$g) == 101) && sum(cn$n) == rz$n && identical(as.character(rz$termes_covariables), c("observateur(B)", "observateur(C)", "vent")) &&
           grepl("^hn ~ observateur \\+ vent · D ", rz$chiffres) && inherits(json(rz), "json"))
for (langue in c("fr", "en")) {
  fr <- langue == "fr"
  tz <- texte_methodes(psc, dz, msc, langue, "essai")
  rt <- texte_resultats(psc, dz, msc, langue)$paragraphes
  pa <- msc$parametres
  verifier(sprintf("un modèle à covariables, ses textes (%s) : ds(formula = ~…), ln σ et ses termes, la référence, l'AIC sans covariables, p̄, Horvitz–Thompson, Marques ; les résultats, chaque coefficient et son facteur sur σ", langue),
           grepl("ds(formula = ~observateur + vent)", tz$paragraphes[2], fixed = TRUE) && grepl("ln σ = β0 + β1 observateur(B) + β2 observateur(C) + β3 vent", tz$paragraphes[2], fixed = TRUE) &&
             grepl(if (fr) "observateur : A" else "observateur: A", tz$paragraphes[2], fixed = TRUE) && grepl(if (fr) "Sans covariables" else "Without covariates", tz$paragraphes[2]) &&
             grepl("p̄", tz$paragraphes[2], fixed = TRUE) && grepl("Horvitz–Thompson", tz$paragraphes[3]) && any(grepl("^Marques, F", tz$references)) && any(grepl("^Marques, T", tz$references)) &&
             grepl(sprintf("observateur(B) β = %s", nombre_resultat(pa$estimation[2], fr)), rt[1], fixed = TRUE) &&
             grepl(sprintf("σ × %s", nombre_resultat(exp(pa$estimation[2]), fr)), rt[1], fixed = TRUE) && grepl(nombre_resultat(msc$densite$D[3], fr, 4), rt[3], fixed = TRUE))
}
tz <- tables_resultats(psc, msc)
verifier("un modèle à covariables, ses tableaux : les covariables et leur référence, la formule au résumé, les coefficients, la probabilité de chaque détection",
         identical(tz$covariables$covariable, c("observateur", "vent")) && identical(tz$resume$covariables, "observateur + vent") &&
           identical(tz$parametres$parametre, c("ln_sigma", "observateur(B)", "observateur(C)", "vent")) && "p" %in% names(tz$observations))
tr <- tables_resultats(ps, etat$modeles$M1$modele)
verifier("tableaux exportés : des data.frame (résumé, candidats, paramètres, détection, adéquation, classes, densité, densité des groupes, transects, observations)",
         all(vapply(tr, is.data.frame, TRUE)) && all(c("resume", "candidats", "parametres", "detection", "adequation", "classes", "densite", "densite_groupes", "transects", "observations") %in% names(tr)) &&
           nrow(tr$observations) == etat$modeles$M1$modele$N && names(tr$observations)[1] == "Observation")
dossier <- tempfile("distance")
dir.create(dossier)
chm <- ecrire_tables(tr, file.path(dossier, "distance.xlsx"), objet = "distance")
verifier("écrits en Excel", length(chm) == 1 && file.exists(chm))
unlink(dossier, recursive = TRUE)

# ---------------------------------------------------------------- l'export vers R

# Chaque script est écrit (ecrire_script : il est d'abord exécuté dans le moteur et comparé, `ecart`),
# puis lancé dans une session R neuve (Rscript --vanilla, sans le moteur) ; ses objets en reviennent
# et ecart_distance les compare au modèle du moteur, à 1e-8 près : les modèles candidats (paramètres,
# logL, AIC, convergence, contrainte, ΔAIC, le retenu), les paramètres, leurs erreurs-types, leur
# covariance et la matrice d'information, la log-vraisemblance et l'AIC, p et sa variance, ESW ou EDR
# et l'effectif couvert, Cramér–von Mises, le khi² et ses classes, le diagramme quantile-quantile, la
# troncature, les distances, les transects, la densité et l'effectif par strate et au total, la densité
# des groupes, la courbe, g des détections et l'histogramme ; avec des covariables, p̄, la probabilité de
# chaque détection et les courbes de chaque catégorie ; les libellés des modèles (et leurs covariables),
# des strates, des transects, des catégories et des paramètres, et la raison du choix, identiques.
cat("Export vers R : des scripts lancés seuls, en français et en anglais, et des rapports R Markdown et Quarto\n")
dossier <- tempfile("distance_script")
dir.create(dossier)
rscript <- file.path(R.home("bin"), "Rscript")
lanceur <- file.path(dossier, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".r <- mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL))", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script) {
  objets <- nom_traduit(c("releve", "choix", "det", "candidats", "parametres", "pr", "var_p", "se_p", "detection", "adequation", "densite", "densite_groupes", "courbe", "g_x",
                          "histogramme", "courbes", "p_moyenne", "choix_sans"), attr(script, "langue"), NOMS_DISTANCE_EN)
  sortie <- tempfile("sortie", dossier, ".rds")
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ","))),
                                       stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
# Le code d'un script hors de ses données (ses lignes « # 2. » à « # 3. » retirées).
hors_donnees <- function(texte) texte[-(grep("^# 2\\. ", texte):(grep("^# 3\\. ", texte) - 1))]
# Les paquets qu'un script appelle par `paquet::`.
paquets_appeles <- function(texte) unique(regmatches(texte, regexpr("[A-Za-z.]+(?=::)", texte, perl = TRUE)))
# Les noms du code (variables, fonctions, arguments, éléments de listes) ; ceux de distance.R et du calcul
# qui changent en anglais, et des mots français dans les commentaires (après la ligne du titre).
noms_code <- function(texte) {
  pd <- utils::getParseData(parse(text = texte, keep.source = TRUE, encoding = "UTF-8"))
  unique(pd$text[pd$terminal & pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB", "SYMBOL_FORMALS")])
}
francais_distance <- names(NOMS_DISTANCE_EN)[names(NOMS_DISTANCE_EN) != NOMS_DISTANCE_EN]
commentaires_francais <- function(texte) {
  com <- sub("^[^#]*#", "", grep("#", hors_donnees(texte)[-1], value = TRUE))
  grep("\\b(les|des|une|chaque|données|valeurs|modèle|détections?|sans|avec|pour|dans|leurs?|sont)\\b", com, value = TRUE, perl = TRUE)
}
essayer_script <- function(nom, p, d, langue, ext = "R", ...) {
  m <- ajuster(p, d)
  r <- ecrire_script(p, d, file.path(dossier, paste0(nom, ".", ext)), "moteur", m = m, version = "essai", langue = langue, ...)
  if (ext != "R") return(invisible(list(r = r, m = m)))
  s <- lancer(structure(r$script, langue = langue))
  texte <- readLines(r$script, encoding = "UTF-8")
  code <- sub("#.*$", "", hors_donnees(texte))
  difference <- if (length(s)) ecart_distance(m, list2env(s), langue) else paste(utils::tail(attr(s, "messages"), 3), collapse = " / ")
  restes <- if (langue == "en") c(intersect(noms_code(hors_donnees(texte)), francais_distance), commentaires_francais(texte)) else character(0)
  collisions <- if (langue == "fr") collisions_traduction(hors_donnees(texte), c(NOMS_DISTANCE_EN, NOMS_SCRIPT_EN)) else list()
  verifier(sprintf("%s (%s) : sans écart vu par le moteur ; lancé seul, les mêmes résultats ; R seul (ni library() ni autre paquet), sans traduire()%s",
                   nom, langue, if (langue == "en") ", ses noms et ses commentaires en anglais" else ", aucun nom confondu en anglais"),
           is.null(r$ecart) && is.null(difference) && !any(grepl("library\\(|require\\(|traduire\\(", code)) &&
             all(paquets_appeles(code) %in% c("stats", "utils")) && !length(restes) && !length(collisions))
  if (!is.null(r$ecart) || !is.null(difference) || length(restes) || length(collisions)) cat("    écart :", r$ecart, "/", difference, "/", restes, "/", names(collisions), "\n")
  invisible(list(r = r, m = m, s = s))
}
p1 <- projet_distance(simuler(1))
p2 <- projet_distance(simuler(2, point = TRUE, cle = "hr", sigma = 35, D = c(Nord = 80, Sud = 140)))
p3 <- projet_distance(simuler(3, groupes = TRUE, D = c(Nord = 15, Sud = 25)))
x <- essayer_script("lignes_strates", p1, definition_distance(p1, w = 100), "fr")
verifier(sprintf("le script refait le choix de ds() (%s, %s) et la densité des deux strates et du total", paste(x$m$candidats$modele, collapse = ", "), x$m$selection),
         identical(x$s$candidats$modele, x$m$candidats$modele) && nrow(x$s$densite) == 3 && identical(x$s$choix$raison, x$m$selection))
essayer_script("lignes_strates_en", p1, definition_distance(p1, w = 100), "en")
essayer_script("lignes_hasard_8_pourcent_en", p1, definition_distance(p1, pct = 8, cle = "hr"), "en")
essayer_script("lignes_gauche_unif_poly", p1, definition_distance(p1, w = 100, gauche = 5, cle = "unif", serie = "poly"), "fr")
x <- essayer_script("lignes_hermite_fixe_sigma", p1, definition_distance(p1, w = 100, serie = "herm", aj = 1, echelle = "echelle"), "fr")
verifier("un modèle fixé (hn + herm(4), à l'échelle de σ) : un seul candidat, le même", nrow(x$s$candidats) == 1 && identical(x$s$candidats$modele, "hn + herm(4)"))
essayer_script("lignes_cosinus_fixe_faible_en", p1, definition_distance(p1, w = 100, aj = 2, mono = "faible"), "en")
essayer_script("lignes_intervalles_gauche_en", p1, definition_distance(p1, intervalles = c(5, 20, 40, 60, 80, 100)), "en")
essayer_script("points_strates", p2, definition_distance(p2, transect = "point", w = 100), "fr")
x <- essayer_script("points_intervalles_hasard", p2, definition_distance(p2, transect = "point", cle = "hr", intervalles = c(0, 10, 20, 35, 50, 70, 100)), "fr")
verifier("des points par intervalles : EDR, le khi² seul, ni Cramér–von Mises ni diagramme quantile-quantile", identical(x$s$detection$statistique[2], "EDR") &&
           is.null(x$s$adequation$cvm) && is.null(x$s$adequation$qq) && nrow(x$s$adequation$khi2$classes) == 6)
essayer_script("points_sans_effort_densite_seule_en", pp, definition_distance(pp, transect = "point", w = 100, effort = NULL, strate = NULL, aire = NULL,
                                                                             unites = list(distance = "m", effort = "m", aire = "ha")), "en")
x <- essayer_script("lignes_groupes", p3, definition_distance(p3, w = 100, taille = "size"), "fr")
verifier("des groupes : la densité des individus et celle des groupes", !is.null(x$s$densite_groupes) && all(x$s$densite$D > x$s$densite_groupes$D))
essayer_script("lignes_groupes_en", p3, definition_distance(p3, w = 100, taille = "size"), "en")
x <- essayer_script("condition_en", ps, definition_distance(ps, w = 100, condition = "Region.Label == \"Nord\""), "en")
verifier("une condition (Region.Label == \"Nord\") : les lignes du modèle, une seule strate", nrow(x$s$densities) == 1 && identical(x$s$densities$stratum, "Nord"))
essayer_script("transect_identifiant", pid, definition_distance(pid, w = 100, echantillon = "id:ligne"), "fr")
pn <- projet_distance(transform(sim, Sample.Label = 10 * match(Sample.Label, unique(Sample.Label))))
essayer_script("transect_nombre_en", pn, definition_distance(pn, w = 100), "en")
x <- essayer_script("uniforme_seule_facteur", ps, definition_distance(ps, cle = "unif", serie = "aucune", facteur = 0.001), "fr")
verifier("la clé uniforme seule (sans paramètre), la troncature à la plus grande distance, un facteur des unités donné", x$m$detection$q == 0 && x$s$pr$p == 1 &&
           !any(grepl("facteur_echantillonnage", readLines(x$r$script, encoding = "UTF-8"))))
x <- essayer_script("strate_vide", projet_distance(vide), definition_distance(projet_distance(vide), w = 100), "fr")
verifier("une strate sans détection : D = 0, ni CV ni ddl, dans le script aussi", x$s$densite$D[3] == 0 && is.na(x$s$densite$cv[3]) && is.na(x$s$densite$ddl[3]))
s4 <- simuler(4)
s4$Area <- 0
p4 <- projet_distance(s4)
essayer_script("aires_nulles_en", p4, definition_distance(p4, w = 100), "en")
# des covariables : la matrice, la fonction de détection à covariables et le modèle sans covariables
# comparé, la probabilité de chaque détection, p̄, la densité de Horvitz–Thompson, les courbes par catégorie
x <- essayer_script("lignes_covariables_groupes", psc, definition_distance(psc, w = 100, taille = "size", z = c("observateur", "vent")), "fr")
verifier("le script d'un modèle à covariables : les mêmes candidats (sans covariables, puis à covariables), p̄, la probabilité de chaque détection, la fonction de détection moyenne de chaque observateur",
         identical(x$s$candidats$covariables, c("", "observateur + vent")) && proche(x$s$p_moyenne$p, x$m$p, 1e-12) && proche(x$s$pr$p, x$m$p_detections, 1e-12) &&
           length(x$s$courbes$niveaux) == 1 && identical(x$s$courbes$niveaux[[1]]$niveaux, c("A", "B", "C")))
essayer_script("lignes_covariables_groupes_en", psc, definition_distance(psc, w = 100, taille = "size", z = c("observateur", "vent")), "en")
pzc <- projet_distance(simuler_covariables(7, point = TRUE, cle = "hr", sigma = 35, D = c(Nord = 90, Sud = 150)))
essayer_script("points_hasard_observateur_en", pzc, definition_distance(pzc, transect = "point", w = 100, cle = "hr", z = "observateur"), "en")
# (une covariable au nom que le script en anglais traduirait s'il était un nom du code : « taille »)
sct <- sc
names(sct)[names(sct) == "size"] <- "taille"
pzt <- projet_distance(sct)
x <- essayer_script("lignes_taille_cosinus_intervalles_en", pzt, definition_distance(pzt, intervalles = c(0, 15, 30, 50, 75, 100), aj = 1, taille = "taille", z = "taille"), "en")
verifier("le script en anglais garde le nom d'une covariable du projet, même un nom du code (taille)", identical(x$s$candidates$covariates[2], "taille") &&
           identical(x$s$parameters$parameter, c("ln_sigma", "taille", "cos_2")))
# un rapport R Markdown : ses parties, ses blocs, et il se compile avec knitr quand il est installé ; un Quarto en anglais
x <- essayer_script("rapport_distance", p3, definition_distance(p3, w = 100, taille = "size"), "fr", ext = "Rmd", donnees = "csv", rdata = TRUE)
texte <- readLines(x$r$script, encoding = "UTF-8")
q <- essayer_script("rapport_distance_en", p2, definition_distance(p2, transect = "point", pct = 10, cle = "hr"), "en", ext = "qmd")
texte_q <- readLines(q$r$script, encoding = "UTF-8")
rz <- essayer_script("rapport_covariables", psc, definition_distance(psc, w = 100, taille = "size", z = c("observateur", "vent")), "fr", ext = "Rmd")
verifier("rapport R Markdown : méthodes, code, résultats, références ; données en CSV et .RData à côté ; un Quarto en anglais",
         is.null(x$r$ecart) && texte[1] == "---" && any(grepl("^## Méthodes", texte)) && any(grepl("^```\\{r modele\\}", texte)) && any(grepl("^```\\{r densite-table\\}", texte)) &&
           any(grepl("^```\\{r groupes-table\\}", texte)) && any(grepl("^- Buckland", texte)) && any(grepl("^- Fewster", texte)) && length(x$r$chemins) == 3 && all(file.exists(x$r$chemins)) &&
           !anyDuplicated(sub("^```\\{r ([^,} ]+).*$", "\\1", grep("^```\\{r [^,} ]", texte, value = TRUE))) &&
           is.null(q$r$ecart) && any(grepl("^## Methods", texte_q)) && any(grepl("^format:", texte_q)) && any(grepl("^```\\{r densite-distances", texte_q)))
if (requireNamespace("knitr", quietly = TRUE)) {
  compiler <- file.path(dossier, "compiler.R")
  writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "a <- commandArgs(TRUE)", "setwd(dirname(a[1]))",
               "knitr::knit(basename(a[1]), output = sub('[.][Rq]md$', '.md', basename(a[1])), quiet = TRUE, envir = new.env())"), compiler)
  # (le nombre de graphiques d'un rapport compilé, chacun dans son fichier ; -1 : pas de .md, ou une erreur)
  compiler_rapport <- function(rapport) {
    suppressWarnings(system2(rscript, c("--vanilla", shQuote(compiler), shQuote(rapport)), stdout = TRUE, stderr = TRUE))
    md <- sub("[.][Rq]md$", ".md", rapport)
    if (!file.exists(md)) return(-1)
    texte_md <- readLines(md, encoding = "UTF-8")
    figures <- sub("^!\\[[^]]*\\]\\(([^)]+)\\).*$", "\\1", grep("^!\\[", texte_md, value = TRUE))
    if (any(grepl("^## Error|Error in", texte_md)) || !all(file.exists(file.path(dossier, figures)))) -1 else length(figures)
  }
  verifier("les rapports se compilent avec knitr, avec leurs graphiques : la fonction de détection et le diagramme quantile-quantile, et la densité des distances dans le Quarto en anglais (des points) ; avec des covariables, la fonction de détection moyenne, celle de chaque observateur et le diagramme quantile-quantile",
           compiler_rapport(x$r$script) == 2 && compiler_rapport(q$r$script) == 3 && is.null(rz$r$ecart) && compiler_rapport(rz$r$script) == 3)
} else cat("  (knitr absent : les rapports ne sont pas compilés)\n")
unlink(dossier, recursive = TRUE)

cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
