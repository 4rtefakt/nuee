# Échantillonnage par distances (distance sampling) : la densité et l'effectif d'une population
# d'après les distances des animaux détectés depuis des lignes (transects linéaires) ou des points
# (points d'écoute), comme les fonctions ds() et dht2() du paquet Distance (Miller, Rexstad, Thomas,
# Marshall et Laake, 2019, qui ajuste la fonction de détection avec mrds), écrit ici d'après la
# méthode publiée (Buckland et al., 2001). Distance et mrds ne sont pas livrés avec Nuée :
# moteur/tests/verifier_distance.R les compare quand ils sont installés (NUEE_DISTANCE).
#
# Les données : le tableau du projet, une ligne par détection (sa distance, rôle Y), avec son
# transect (l'échantillon : un identifiant ou une variable), l'effort de ce transect (la longueur de
# la ligne, ou le nombre de passages au point), sa strate (une région, facultative) et l'aire de
# cette strate (facultative : sans elle, la densité seulement), la taille du groupe (facultative) ;
# un transect sans détection reste une ligne, sa distance vide, pour compter son effort.
#
# La fonction de détection g(x) (la probabilité de détecter un animal à la distance x, 1 en 0) est
# une clé, demi-normale exp(−x²/2σ²), hasard 1 − exp(−(x/σ)^−b) ou uniforme 1, multipliée par
# 1 + Σ aj fj(x/h), des termes d'ajustement en cosinus cos(jπu), en polynômes d'Hermite Hej(u) ou en
# polynômes simples u^j (h : la troncature w, ou σ), puis rapportée à sa valeur en 0. Ses paramètres
# (ln b, ln σ, aj, dans cet ordre, comme mrds) sont ceux du maximum de vraisemblance des distances
# entre la troncature à gauche L et la troncature w : f(x) = g(x) / ∫ g (lignes), r g(r) / ∫ r g
# (points). Le nombre de termes d'ajustement est choisi comme ds() le fait : la clé seule, puis un
# terme, deux… tant que l'AIC baisse (au plus 5), dans l'ordre de Distance (cosinus : 2, 3, 4… ;
# Hermite et polynômes : 4, 6, 8… ; la clé uniforme : cosinus 1, 2, 3…, polynômes 2, 4, 6…), sous la
# contrainte de monotonie de mrds (stricte par défaut : g décroissante et positive en 10 points de
# 0 à w). La variance des paramètres vient de la somme des produits des scores des observations
# (comme mrds : flt.var), celle de p et des densités de la méthode delta.
#
# La densité (dht2, stratification géographique) : par strate, N = A n / (a p) (a : l'aire
# couverte, 2 (w − L) L pour des lignes, π w² K pour des points, K les passages) ; la variance du
# taux de rencontre n / L entre les transects (R2 pour les lignes, P2 pour les points ; Fewster et
# al., 2009), celle de p par la méthode delta, et celle de la taille moyenne des groupes ; leur
# somme, les degrés de liberté de Satterthwaite et l'intervalle log-normal (Buckland et al., 2001) ;
# le total sur les strates, de même. C'est une analyse de analyses.R (ajuster_distance,
# resultat_distance, texte_methodes_distance, tables_distance ; son export vers R : distance_script.R) ;
# elle ne prédit pas de nouvelles observations.

DISTANCE_CLES <- c("hn", "hr", "unif")
DISTANCE_SERIES <- c("cos", "herm", "poly")
DISTANCE_AJUSTEMENTS_MAX <- 5    # (ds : max_adjustments)
DISTANCE_POINTS_MONO <- 10       # (mrds : mono.points, les points où la monotonie est contrainte)
DISTANCE_TOLERANCE <- 1e-11      # (la précision relative des intégrales, integrate ; mrds : 1e-7)

# Une intégrale de `f` de a à b par la quadrature adaptative de Gauss–Kronrod de R (integrate) : une
# fonction lisse l'est en 21 points, un hasard en marche d'escalier (b grand) ou une fonction qui
# s'annule (des ajustements qui la rendent négative) en plus.
integrer_detection <- function(f, a, b) {
  if (!(b > a)) return(0)
  stats::integrate(f, a, b, rel.tol = DISTANCE_TOLERANCE, abs.tol = 0, subdivisions = 2000L, stop.on.error = FALSE)$value
}

# ---------------------------------------------------------------- la fonction de détection

# Les positions des paramètres dans θ : forme (ln b, hasard), échelle (ln σ, demi-normale et hasard ;
# avec des covariables, les coefficients de ln σ, la constante d'abord : avec_covariables),
# ajustements (aj).
indices_detection <- function(fd) {
  k <- 0
  forme <- if (fd$cle == "hr") (k <- k + 1)
  ke <- if (fd$cle == "unif") 0 else if (is.null(fd$Z)) 1 else ncol(fd$Z)
  echelle <- if (ke) k + seq_len(ke)
  k <- k + ke
  ajust <- if (length(fd$ordres)) k + seq_along(fd$ordres) else integer(0)
  list(forme = forme, echelle = echelle, ajustements = ajust, q = k + length(fd$ordres))
}

# Les noms des paramètres, dans l'ordre de θ : ln_b, ln_sigma (avec des covariables, la constante de
# ln σ, puis le coefficient de chaque covariable, nommé comme son terme : « OBs(TJS) », « MAS »), puis
# la série et l'ordre de chaque terme d'ajustement (cos_2…).
noms_parametres_detection <- function(fd) {
  c(if (fd$cle == "hr") "ln_b", if (fd$cle != "unif") (if (is.null(fd$Z)) "ln_sigma" else fd$noms_echelle),
    if (length(fd$ordres)) paste0(fd$serie, "_", fd$ordres))
}

# Les polynômes d'Hermite (des probabilistes) : He0 = 1, He1 = u, Hek+1 = u Hek − k Hek−1.
hermite_proba <- function(u, k) {
  if (k == 0) return(rep(1, length(u)))
  h0 <- rep(1, length(u))
  h1 <- u
  if (k >= 2) for (j in 1:(k - 1)) {
    h2 <- u * h1 - j * h0
    h0 <- h1
    h1 <- h2
  }
  h1
}

# Les termes d'ajustement fj(u) et leurs dérivées en u, une colonne par ordre.
serie_ajustement <- function(serie, u, ordres) {
  F <- D <- matrix(0, length(u), length(ordres))
  for (j in seq_along(ordres)) {
    k <- ordres[j]
    if (serie == "cos") {
      F[, j] <- cos(k * pi * u)
      D[, j] <- -k * pi * sin(k * pi * u)
    } else if (serie == "poly") {
      F[, j] <- u^k
      D[, j] <- k * u^(k - 1)
    } else {
      F[, j] <- hermite_proba(u, k)
      D[, j] <- k * hermite_proba(u, k - 1)
    }
  }
  list(F = F, D = D)
}

# La clé k(x) et ses dérivées par ln b et ln σ. Hasard : k = 1 − exp(−z), z = (x/σ)^−b ; dk/dln σ =
# b z e^−z, dk/dln b = z e^−z ln z (nulles en x = 0, où z est infini).
cle_detection <- function(fd, theta, x) {
  ix <- indices_detection(fd)
  if (fd$cle == "unif") return(list(k = rep(1, length(x))))
  sigma <- exp(theta[ix$echelle])
  if (fd$cle == "hn") {
    k <- exp(-(x / sigma)^2 / 2)
    return(list(k = k, dechelle = k * (x / sigma)^2))
  }
  b <- exp(theta[ix$forme])
  lz <- -b * log(x / sigma)
  z <- exp(lz)
  k <- -expm1(-z)
  ze <- z * exp(-z)
  ze[!is.finite(z)] <- 0
  dforme <- ze * lz
  dforme[ze == 0] <- 0
  list(k = k, dechelle = b * ze, dforme = dforme)
}

# g̃(x) = k(x) (1 + Σ aj fj(x/h)), sans la division par sa valeur en 0, 1 + s0 (k(0) = 1 pour les
# trois clés), et, sur demande, leurs dérivées par θ (une colonne par paramètre).
detection_distance <- function(fd, theta, x, derivees = FALSE) {
  ix <- indices_detection(fd)
  kk <- cle_detection(fd, theta, x)
  m <- length(fd$ordres)
  s <- 0
  s0 <- 0
  if (m) {
    par_echelle <- identical(fd$echelle_ajustements, "echelle") && fd$cle != "unif"
    h <- if (par_echelle) exp(theta[ix$echelle]) else fd$w
    a <- theta[ix$ajustements]
    u <- x / h
    S <- serie_ajustement(fd$serie, u, fd$ordres)
    s <- drop(S$F %*% a)
    F0 <- serie_ajustement(fd$serie, 0, fd$ordres)$F
    s0 <- sum(F0 * a)
  }
  gt <- kk$k * (1 + s)
  if (!derivees) return(list(gt = gt, s0 = s0))
  dgt <- matrix(0, length(x), ix$q)
  if (!is.null(ix$forme)) dgt[, ix$forme] <- kk$dforme * (1 + s)
  if (!is.null(ix$echelle)) {
    dgt[, ix$echelle] <- kk$dechelle * (1 + s)
    if (m && par_echelle) dgt[, ix$echelle] <- dgt[, ix$echelle] - kk$k * drop(S$D %*% a) * u
  }
  ds0 <- numeric(ix$q)
  if (m) {
    dgt[, ix$ajustements] <- kk$k * S$F
    ds0[ix$ajustements] <- F0
  }
  list(gt = gt, dgt = dgt, s0 = s0, ds0 = ds0)
}

# La densité des distances observées, à une constante près : 1 / (w − L) (lignes), 2x / w² (points),
# comme mrds (fx, fr) : la vraisemblance en garde les mêmes termes.
poids_detection <- function(fd, x) if (fd$point) 2 * x / fd$w^2 else rep(1 / (fd$w - fd$L), length(x))

# Les intégrales de poids_detection × g̃ de chaque `a` à chaque `b` (`a` seul : le même début), et leurs
# dérivées par θ, une valeur sous 1e-16 comptant pour 0 comme dans mrds (gstdint). Une clé seule, sans
# terme d'ajustement : la demi-normale intégrée exactement, comme gstdint (lignes : σ √(2π) (Φ(b/σ) −
# Φ(a/σ)) / (w − L) ; points : 2 σ² (exp(−a²/2σ²) − exp(−b²/2σ²)) / w²), et la dérivée par ln σ tirée
# de l'intégrale, g ne dépendant que de x/σ (∂g/∂ln σ = −x ∂g/∂x, intégrée par parties : I − (b g(b) −
# a g(a)) / (w − L) pour des lignes, 2 I − 2 (b² g(b) − a² g(a)) / w² pour des points) ; restent
# numériques l'intégrale du hasard et sa dérivée par ln b.
integrale_detection <- function(fd, theta, a, b, derivees = FALSE) {
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

# Des distances relevées par intervalles (fd$coupures, comme ds(cutpoints = ) : le premier la
# troncature à gauche, le dernier w) : la classe de chaque distance, fermée à droite (la première
# des deux côtés), comme create_bins de Distance.
classes_detection <- function(fd, x) cut(x, fd$coupures, include.lowest = TRUE, labels = FALSE)

# La log-vraisemblance des distances exactes `x` (entre L et w) : Σ ln f(xi), f = poids × g̃ / ∫ poids
# × g̃ ; une densité sous 1e-15 comptant pour 1e-15 (mrds : flpt.lnl). Des distances par intervalles :
# Σ ln (∫ de sa classe / ∫ de L à w), la loi multinomiale des comptes des classes. Avec `scores`, la
# dérivée de chaque terme par θ (une ligne par observation).
vraisemblance_detection <- function(fd, theta, x, scores = FALSE) {
  if (!is.null(fd$coupures)) {
    K <- length(fd$coupures) - 1
    k <- classes_detection(fd, x)
    nk <- tabulate(k, K)
    ib <- integrale_detection(fd, theta, fd$coupures[-(K + 1)], fd$coupures[-1], scores)
    ia <- integrale_detection(fd, theta, fd$L, fd$w, scores)
    vus <- nk > 0
    if (!is.finite(ia$I) || ia$I <= 0 || !all(ib$I[vus] > 0)) return(list(ll = -Inf))
    ll <- sum(nk[vus] * (log(ib$I[vus]) - log(ia$I)))
    if (!scores) return(list(ll = ll))
    Sk <- sweep(ib$dI / ib$I, 2, ia$dI[1, ] / ia$I)
    return(list(ll = ll, scores = Sk[k, , drop = FALSE]))
  }
  e <- detection_distance(fd, theta, x, scores)
  p1 <- poids_detection(fd, x) * e$gt
  bas <- !(p1 >= 1e-15)
  bas[is.na(bas)] <- TRUE
  p1[bas] <- 1e-15
  int <- integrale_detection(fd, theta, fd$L, fd$w, scores)
  if (!is.finite(int$I) || int$I <= 0) return(list(ll = -Inf))
  ll <- sum(log(p1)) - length(x) * log(int$I)
  if (!scores) return(list(ll = ll))
  S <- e$dgt / e$gt
  S[bas, ] <- 0
  S <- sweep(S, 2, int$dI[1, ] / int$I)
  list(ll = ll, scores = S)
}

# La probabilité moyenne de détection p = ∫ poids × g sur [L, w] (g rapportée à g(0)) et son gradient.
probabilite_detection <- function(fd, theta) {
  if (!indices_detection(fd)$q) return(list(p = 1, dp = numeric(0)))
  int <- integrale_detection(fd, theta, fd$L, fd$w, TRUE)
  d0 <- 1 + int$s0
  list(p = int$I / d0, dp = int$dI[1, ] / d0 - int$I * int$ds0 / d0^2)
}

# Les contraintes de monotonie de mrds (flnl.constr) en DISTANCE_POINTS_MONO points xr = w (i/10)^1,5 :
# stricte, g(xr[i−1]) − g(xr[i]) ≥ 0 (g(0) = 1 avant le premier) ; faible, 1 − g(xr[i]) ≥ 0 ; et
# g(xr[i]) ≥ 0. Leurs valeurs `c` et leur jacobien `J`.
contraintes_monotonie <- function(fd, theta, strict = TRUE) {
  xr <- (seq_len(DISTANCE_POINTS_MONO))^1.5 * fd$w / DISTANCE_POINTS_MONO^1.5
  e <- detection_distance(fd, theta, xr, TRUE)
  d0 <- 1 + e$s0
  g <- e$gt / d0
  dg <- e$dgt / d0 - outer(e$gt, e$ds0) / d0^2
  if (strict) {
    c_m <- c(1, g[-length(g)]) - g
    J_m <- rbind(0, dg[-length(g), , drop = FALSE]) - dg
  } else {
    c_m <- 1 - g
    J_m <- -dg
  }
  list(c = c(c_m, g), J = rbind(J_m, dg))
}

# ---------------------------------------------------------------- les covariables

# La matrice des covariables de l'échelle, comme model.matrix(~ x1 + x2) de ds(formula = ~ x1 + x2)
# (mrds : setcov) : la constante, puis chaque covariable dans son ordre, une quantitative telle quelle,
# une qualitative (un facteur) par une indicatrice 0/1 de chacune de ses catégories présentes sauf la
# première, sa catégorie de référence (les contrastes de traitement de R ; mrds laisse de côté les
# catégories absentes des détections, process.data). `colonnes` : les covariables des détections, une
# liste nommée. Ses colonnes sont nommées comme R les nomme (« OBsTJS ») ; attr(, "termes") : la
# covariable, la catégorie (NA pour une quantitative) et le nom Nuée (« OBs(TJS) ») de chacune ;
# attr(, "references") : la catégorie de référence de chaque covariable qualitative.
matrice_covariables <- function(colonnes) {
  Z <- matrix(1, length(colonnes[[1]]), 1)
  termes <- data.frame(colonne = "(Intercept)", covariable = NA_character_, niveau = NA_character_, terme = "ln_sigma", stringsAsFactors = FALSE)
  references <- list()
  for (covariable in names(colonnes)) {
    v <- colonnes[[covariable]]
    if (is.factor(v)) {
      lv <- levels(droplevels(v))
      references[[covariable]] <- lv[1]
      for (l in lv[-1]) {
        Z <- cbind(Z, as.numeric(v == l))
        termes <- rbind(termes, data.frame(colonne = paste0(covariable, l), covariable = covariable, niveau = l, terme = sprintf("%s(%s)", covariable, l), stringsAsFactors = FALSE))
      }
    } else {
      Z <- cbind(Z, as.numeric(v))
      termes <- rbind(termes, data.frame(colonne = covariable, covariable = covariable, niveau = NA_character_, terme = covariable, stringsAsFactors = FALSE))
    }
  }
  colnames(Z) <- termes$colonne
  attr(Z, "termes") <- termes
  attr(Z, "references") <- references
  Z
}

# La fonction de détection `fd` dont l'échelle dépend des covariables `Z` (matrice_covariables, une
# ligne par détection) : σ = exp(Z β) pour chaque détection, ln σ linéaire comme dans ds(formula = )
# (Marques et Buckland, 2003). Elle garde les combinaisons distinctes des covariables (la fonction de
# détection n'en dépend que par elles) et celle de chaque détection, la formule (« OBs + MAS », pour
# son libellé) et les noms de ses coefficients.
avec_covariables <- function(fd, Z) {
  cle <- do.call(paste, c(lapply(seq_len(ncol(Z)), function(j) sprintf("%.17g", Z[, j])), sep = "\r"))
  u <- !duplicated(cle)
  termes <- attr(Z, "termes")
  fd$Z <- Z
  fd$combinaisons <- list(Z = Z[u, , drop = FALSE], k = match(cle, cle[u]))
  fd$formule <- paste(unique(termes$covariable[-1]), collapse = " + ")
  fd$noms_echelle <- termes$terme
  fd
}

# La même fonction de détection sans ses covariables : celle d'une combinaison, ses paramètres
# donnés par theta_combinaison.
sans_covariables <- function(fd) {
  fd$Z <- NULL
  fd$combinaisons <- NULL
  fd$formule <- NULL
  fd$noms_echelle <- NULL
  fd
}

# Les paramètres de la clé pour la combinaison de covariables `z` : (ln b, ln σ = z'β, aj), ceux de la
# fonction de détection sans covariables (sans_covariables).
theta_combinaison <- function(fd, theta, z) {
  ix <- indices_detection(fd)
  c(theta[ix$forme], sum(z * theta[ix$echelle]), theta[ix$ajustements])
}

# Les dérivées des paramètres d'une combinaison (theta_combinaison) par θ : une ligne par paramètre de
# la combinaison (ln b, ln σ, aj), une colonne par paramètre de θ ; ln σ = z'β.
jacobien_combinaison <- function(fd, z) {
  ix <- indices_detection(fd)
  J <- matrix(0, ix$q - length(ix$echelle) + 1, ix$q)
  r <- 0
  if (!is.null(ix$forme)) {
    r <- r + 1
    J[r, ix$forme] <- 1
  }
  J[r + 1, ix$echelle] <- z
  for (j in ix$ajustements) J[r + 1 + match(j, ix$ajustements), j] <- 1
  J
}

# La log-vraisemblance des distances `x` d'une fonction de détection à covariables : la somme, sur les
# combinaisons de covariables, de celle de leurs détections (vraisemblance_detection, la fonction de
# détection de la combinaison : f(x | z) = poids × g̃(x | z) / ∫ poids × g̃(· | z), comme flpt.lnl
# de mrds, ou la loi multinomiale de leurs classes). Avec `scores`, ses dérivées par θ (une ligne
# par détection).
vraisemblance_covariables <- function(fd, theta, x, scores = FALSE) {
  cl <- sans_covariables(fd)
  co <- fd$combinaisons
  ll <- 0
  S <- if (scores) matrix(0, length(x), indices_detection(fd)$q)
  for (k in seq_len(nrow(co$Z))) {
    i <- which(co$k == k)
    v <- vraisemblance_detection(cl, theta_combinaison(fd, theta, co$Z[k, ]), x[i], scores)
    if (!is.finite(v$ll)) return(list(ll = -Inf))
    ll <- ll + v$ll
    if (scores) S[i, ] <- v$scores %*% jacobien_combinaison(fd, co$Z[k, ])
  }
  if (!scores) return(list(ll = ll))
  list(ll = ll, scores = S)
}

# La probabilité de détection de chaque détection d'une fonction de détection à covariables, sachant
# ses covariables (pi, celle de sa combinaison : probabilite_detection ; mrds : predict.ds), et son
# gradient par θ (une ligne par détection) ; et les mêmes par combinaison.
probabilites_covariables <- function(fd, theta) {
  cl <- sans_covariables(fd)
  co <- fd$combinaisons
  pc <- numeric(nrow(co$Z))
  dpc <- matrix(0, nrow(co$Z), indices_detection(fd)$q)
  for (k in seq_along(pc)) {
    pr <- probabilite_detection(cl, theta_combinaison(fd, theta, co$Z[k, ]))
    pc[k] <- pr$p
    dpc[k, ] <- drop(pr$dp %*% jacobien_combinaison(fd, co$Z[k, ]))
  }
  list(p = pc[co$k], dp = dpc[co$k, , drop = FALSE], p_combinaisons = pc, dp_combinaisons = dpc)
}

# La probabilité moyenne de détection d'une fonction de détection à covariables, comme summary.ds de
# mrds : p̄ = n / N̂, N̂ = Σ 1/pi l'effectif couvert de Horvitz–Thompson (les détections, sans la
# taille des groupes) ; sa variance (calc.se.Np) réunit celle de N̂ (la méthode delta, de `V` la
# covariance des paramètres, plus Σ (1 − pi) / pi²) et celle de la somme des pi :
# var(p̄) = p̄² [var(N̂) / N̂² + Σ (1 − pi) / n² − 2 Σ (1 − pi) / pi / (n N̂)]. `pr` :
# probabilites_covariables.
moyenne_detection <- function(pr, V) {
  p <- pr$p
  n <- length(p)
  N <- sum(1 / p)
  gN <- -colSums(pr$dp / p^2)
  var_N <- drop(t(gN) %*% V %*% gN) + sum((1 - p) / p^2)
  pm <- n / N
  var_p <- pm^2 * (var_N / N^2 + sum(1 - p) / n^2 - 2 * sum((1 - p) / p) / (n * N))
  list(p = pm, var_p = var_p, se_p = sqrt(max(0, var_p)), N = N, se_N = sqrt(max(0, var_N)))
}

# ---------------------------------------------------------------- l'ajustement

# Les valeurs de départ de mrds (setinitial.ds) : ln σ = la moyenne de ln(x + w/1000) (demi-normale) ;
# pour le hasard, la meilleure vraisemblance d'une grille (sethazard : σ = s/2 + 0,2 j s, s la racine
# de la moyenne des x², j de 1 à 10 ; b = 1, 3… 29 ; la première case retient b = 2, comme mrds) ;
# les ajustements à 1e-16. Des distances par intervalles y comptent pour le milieu de leur classe. Avec
# des covariables, les coefficients de ln σ sont ceux de la régression linéaire de ln(x + w/1000) sur
# elles (lm), et la forme du hasard celle de la grille, sans covariables.
depart_detection <- function(fd, x) {
  ix <- indices_detection(fd)
  theta <- numeric(ix$q)
  if (!is.null(fd$coupures)) {
    k <- classes_detection(fd, x)
    x <- (fd$coupures[k] + fd$coupures[k + 1]) / 2
  }
  regression <- if (!is.null(fd$Z) && fd$cle != "unif") unname(stats::lm.fit(fd$Z, log(x + fd$w / 1000))$coefficients)
  if (fd$cle == "hn") theta[ix$echelle] <- if (is.null(fd$Z)) mean(log(x + fd$w / 1000)) else regression
  if (fd$cle == "hr") {
    cle <- fd
    cle$ordres <- integer(0)
    cle$Z <- NULL
    cle$L <- 0  # (mrds intègre la grille depuis 0, même tronquée à gauche)
    s <- sqrt(mean(x^2))
    meilleur <- Inf
    ij <- c(2, 1)
    for (j in 1:10) for (i in seq(1, 30, 2)) {
      v <- -vraisemblance_detection(cle, c(log(i), log(s / 2 + j * 0.2 * s)), x)$ll
      if (!is.finite(v)) next
      if (i == 1 && j == 1) { meilleur <- v; ij <- c(2, 1) }
      else if (v < meilleur) { meilleur <- v; ij <- c(i, j) }
    }
    theta[ix$forme] <- log(ij[1])
    theta[ix$echelle] <- if (is.null(fd$Z)) log(s / 2 + ij[2] * 0.2 * s) else regression
  }
  if (length(ix$ajustements)) theta[ix$ajustements] <- 1e-16
  theta
}

# Les bornes des paramètres : ln b du hasard ≥ 0 (b ≥ 1, la borne de mrds) ; les autres, larges,
# n'arrêtent qu'un ajustement qui part à l'infini (σ plus de 1000 fois la troncature w ou 160 000
# fois plus petit, b au-delà de 1000, un terme d'ajustement au-delà de 1000) : un modèle dégénéré
# (une demi-normale devenue plate, la clé uniforme sous un autre nom), qui ne converge pas. Avec des
# covariables, ce sont les bornes des coefficients réduits (reduction_covariables) : la constante,
# ln σ aux catégories de référence et aux moyennes des covariables quantitatives, dans celles de ln σ ;
# chaque autre coefficient, un écart de ln σ entre deux catégories ou pour un écart-type d'une
# covariable, à moins de 19 (l'étendue des bornes de ln σ) ; et le σ de chaque combinaison de
# covariables est tenu dans les bornes de σ (ajuster_detection).
bornes_detection <- function(fd) {
  ix <- indices_detection(fd)
  bas <- rep(-1e3, ix$q)
  haut <- rep(1e3, ix$q)
  if (!is.null(ix$forme)) { bas[ix$forme] <- 0; haut[ix$forme] <- 7 }
  if (!is.null(ix$echelle)) { bas[ix$echelle] <- log(fd$w) - 12; haut[ix$echelle] <- log(fd$w) + 7 }
  if (length(ix$echelle) > 1) { bas[ix$echelle[-1]] <- -19; haut[ix$echelle[-1]] <- 19 }
  list(bas = bas, haut = haut, libre_en_bas = if (!is.null(ix$forme)) ix$forme else integer(0))
}

# La réduction des coefficients d'une fonction de détection à covariables pour l'ajustement, θ = R θ̃ :
# chaque covariable quantitative centrée et réduite sur les détections (une indicatrice 0/1 telle
# quelle), ln σ = Z β = Z̃ β̃ ; l'ajustement, mieux conditionné (une constante qui ne dépend plus
# de la pente d'une covariable loin de 0), trouve le même maximum, rendu en θ.
reduction_covariables <- function(fd) {
  ix <- indices_detection(fd)
  R <- diag(ix$q)
  e <- ix$echelle
  for (j in seq_len(ncol(fd$Z))[-1]) {
    z <- fd$Z[, j]
    if (all(z %in% c(0, 1))) next
    s <- stats::sd(z)
    R[e[j], e[j]] <- 1 / s
    R[e[1], e[j]] <- -mean(z) / s
  }
  R
}

# Minimise `f` (gradient `gr`) depuis `theta` entre les bornes, puis quelques pas de Newton sur les
# paramètres libres (le hessien par différences du gradient) pour un optimum précis. Il converge si
# l'optimum est atteint : un gradient nul sur les paramètres libres, ou la baisse que promet le pas
# de Newton, g'H⁻¹g / 2, sous 5e−7 (H défini positif).
minimiser_detection <- function(f, gr, theta, bas, haut) {
  o <- tryCatch(stats::nlminb(theta, f, gr, lower = bas, upper = haut, control = list(eval.max = 3000, iter.max = 2000, rel.tol = 1e-15, x.tol = 1e-13)),
                error = function(e) NULL)
  if (is.null(o) || !all(is.finite(o$par)) || !is.finite(o$objective)) return(list(par = theta, value = f(theta), convergence = 1))
  th <- o$par
  v <- o$objective
  atteint <- FALSE
  for (k in 1:30) {
    g <- gr(th)
    libre <- !(th <= bas + 1e-12 & g > 0) & !(th >= haut - 1e-12 & g < 0)
    if (!any(libre) || max(abs(g[libre])) < 1e-9) {
      atteint <- TRUE
      break
    }
    h <- 1e-5 * pmax(1, abs(th))
    H <- vapply(seq_along(th), function(j) { e <- replace(numeric(length(th)), j, h[j]); (gr(th + e) - gr(th - e)) / (2 * h[j]) }, numeric(length(th)))
    H <- (H + t(H)) / 2
    pas <- tryCatch(solve(H[libre, libre, drop = FALSE], g[libre]), error = function(e) NULL)
    if (is.null(pas) || !all(is.finite(pas))) break
    atteint <- sum(g[libre] * pas) >= 0 && sum(g[libre] * pas) < 1e-6
    essai <- th
    essai[libre] <- pmin(haut[libre], pmax(bas[libre], th[libre] - pas))
    ve <- f(essai)
    if (!is.finite(ve) || ve > v + 1e-12 * max(1, abs(v))) break
    th <- essai
    v <- ve
  }
  list(par = th, value = v, convergence = if (atteint) 0 else 1, message = o$message)
}

# Le maximum de vraisemblance sous les contraintes `cons` (c ≥ 0) par un lagrangien augmenté
# (Powell–Hestenes–Rockafellar) : chaque sous-problème minimisé comme ci-dessus, les multiplicateurs
# λ ← max(0, λ − ρ c), ρ décuplé tant que la violation ne baisse pas assez. Puis le point atteint
# précisé par la méthode de Newton sur les conditions de Karush–Kuhn–Tucker : ses contraintes actives
# (c < 1e−7) tenues comme des égalités, ∇f = Jᵀλ sur ses paramètres libres (le hessien du lagrangien
# par différences de son gradient, λ de départ par les moindres carrés). Quand ρ a dû monter très
# haut, le dernier sous-problème est si mal conditionné que sa convergence se joue au dernier bit,
# d'un processeur à l'autre (Apple silicon) ; ce système-là converge en quelques pas à la précision
# de la machine. Il converge si ∇f = Jᵀλ (à 1e−9 près, relativement), les contraintes tenues et les
# multiplicateurs des actives positifs ; sinon, si le dernier sous-problème converge, les contraintes
# tenues et les multiplicateurs complémentaires (λ c = 0).
sous_contraintes_detection <- function(f, gr, cons, theta, bas, haut) {
  nc <- length(cons(theta)$c)
  lambda <- numeric(nc)
  rho <- 10
  violation <- Inf
  fini <- FALSE
  for (it in 1:60) {
    L <- function(th) {
      cc <- cons(th)$c
      f(th) + sum(pmax(0, lambda - rho * cc)^2 - lambda^2) / (2 * rho)
    }
    Lg <- function(th) {
      k <- cons(th)
      gr(th) - drop(crossprod(k$J, pmax(0, lambda - rho * k$c)))
    }
    o <- minimiser_detection(L, Lg, theta, bas, haut)
    if (!all(is.finite(o$par))) break
    theta <- o$par
    cc <- cons(theta)$c
    v <- max(0, -cc)
    lambda <- pmax(0, lambda - rho * cc)
    if (v < 1e-11 && max(abs(pmin(lambda, pmax(cc, 0)))) < 1e-9) {
      fini <- o$convergence == 0
      break
    }
    if (v > 0.25 * violation) rho <- min(rho * 10, 1e12)
    violation <- v
  }
  certifie <- FALSE
  actives <- which(cons(theta)$c < 1e-7)
  libre <- theta > bas + 1e-12 & theta < haut - 1e-12
  if (length(actives) && length(actives) < sum(libre)) {
    th <- theta
    k <- cons(th)
    g <- gr(th)
    mu <- tryCatch(qr.solve(t(k$J[actives, libre, drop = FALSE]), g[libre]), error = function(e) NULL)
    for (it in 1:20) {
      if (is.null(mu) || !all(is.finite(c(th, mu))) || any(th < bas | th > haut)) break
      Ja <- k$J[actives, libre, drop = FALSE]
      residu <- max(abs(g[libre] - drop(crossprod(Ja, mu)))) / max(1, abs(g))
      if (residu < 1e-9 && max(abs(k$c[actives])) < 1e-12) {
        certifie <- min(k$c) > -1e-10 && all(mu > 0)
        break
      }
      gL <- function(x) gr(x) - drop(crossprod(cons(x)$J[actives, , drop = FALSE], mu))
      h <- 1e-5 * pmax(1, abs(th))
      W <- vapply(which(libre), function(j) { e <- replace(numeric(length(th)), j, h[j]); (gL(th + e) - gL(th - e))[libre] / (2 * h[j]) }, numeric(sum(libre)))
      W <- (W + t(W)) / 2
      pas <- tryCatch(solve(rbind(cbind(W, -t(Ja)), cbind(Ja, matrix(0, length(actives), length(actives)))),
                            -c(g[libre] - drop(crossprod(Ja, mu)), k$c[actives])), error = function(e) NULL)
      if (is.null(pas) || !all(is.finite(pas))) break
      th[libre] <- th[libre] + pas[seq_len(sum(libre))]
      mu <- mu + pas[-seq_len(sum(libre))]
      k <- cons(th)
      g <- gr(th)
    }
    if (certifie) {
      theta <- th
      lambda <- replace(numeric(nc), actives, mu)
    }
  }
  list(par = theta, value = f(theta), convergence = if (certifie || (fini && max(0, -cons(theta)$c) < 1e-8)) 0 else 1, multiplicateurs = lambda)
}

# Une fonction de détection ajustée aux distances `x` : la clé, la série et ses ordres, depuis
# `depart` (ou les valeurs de départ de mrds), sous la contrainte `monotonie` ("stricte", "faible",
# "aucune" ; une clé seule n'est jamais contrainte, comme dans mrds). Ses paramètres, la
# log-vraisemblance, l'AIC, la matrice de la somme des produits des scores (`H`, mrds : flt.var) et la
# covariance des paramètres (son inverse, mrds : solvecov). `vraisemblance` : vraisemblance_detection,
# ou vraisemblance_covariables pour une fonction de détection à covariables (jamais contrainte, comme
# dans ds), ajustée sur ses coefficients réduits (`reduction`, R de reduction_covariables : θ = R θ̃,
# les bornes et le conditionnement de l'information sont ceux de θ̃, la covariance R H̃⁻¹ R') ; le σ
# de chacune de ses combinaisons de covariables tenu dans les bornes de σ (`hors_bornes` : celles qui en
# sortent).
ajuster_detection <- function(fd, x, depart = NULL, monotonie = "stricte", vraisemblance = vraisemblance_detection, reduction = NULL) {
  ix <- indices_detection(fd)
  q <- ix$q
  base <- list(fd = fd, n = length(x), q = q, monotonie = if (length(fd$ordres)) monotonie else "aucune")
  if (!q) {
    v <- vraisemblance(fd, numeric(0), x)
    return(c(base, list(theta = numeric(0), ll = v$ll, AIC = -2 * v$ll, H = matrix(0, 0, 0), V = matrix(0, 0, 0), se = numeric(0),
                        converge = is.finite(v$ll), contraint = FALSE)))
  }
  theta <- if (is.null(depart)) depart_detection(fd, x) else depart
  b <- bornes_detection(fd)
  bas <- b$bas
  haut <- b$haut
  R <- reduction
  vers_theta <- function(th) if (is.null(R)) th else drop(R %*% th)
  if (!is.null(R)) theta <- drop(solve(R, theta))
  theta <- pmin(haut, pmax(theta, bas))
  f <- function(th) {
    v <- vraisemblance(fd, vers_theta(th), x)$ll
    if (is.finite(v)) -v else 1e300
  }
  gr <- function(th) {
    v <- vraisemblance(fd, vers_theta(th), x, TRUE)
    if (!is.finite(v$ll)) return(rep(0, q))
    g <- -colSums(v$scores)
    if (is.null(R)) g else drop(crossprod(R, g))
  }
  if (!is.finite(f(theta)) || f(theta) >= 1e300) return(c(base, list(theta = vers_theta(theta), ll = -Inf, AIC = Inf, converge = FALSE, contraint = FALSE)))
  o <- minimiser_detection(f, gr, theta, bas, haut)
  contraint <- FALSE
  if (length(fd$ordres) && monotonie != "aucune") {
    cons <- function(th) contraintes_monotonie(fd, th, monotonie == "stricte")
    if (any(cons(o$par)$c < -1e-10)) {
      o <- sous_contraintes_detection(f, gr, cons, theta, bas, haut)
      contraint <- TRUE
    }
  }
  par <- vers_theta(o$par)
  v <- vraisemblance(fd, par, x, TRUE)
  H <- crossprod(v$scores)
  Hr <- if (is.null(R)) H else crossprod(R, H %*% R)
  V <- if (is.null(R)) inverse_information(H) else R %*% inverse_information(Hr) %*% t(R)
  a_la_borne <- o$par >= haut - 1e-6 | (o$par <= bas + 1e-6 & !seq_len(q) %in% b$libre_en_bas)
  hors_bornes <- integer(0)
  if (!is.null(fd$Z)) {
    ln_sigma <- drop(fd$combinaisons$Z %*% par[ix$echelle])
    hors_bornes <- which(ln_sigma >= log(fd$w) + 7 - 1e-6 | ln_sigma <= log(fd$w) - 12 + 1e-6)
  }
  # (une matrice d'information presque singulière, que solvecov comble par 1e10 : des distances qui ne
  # disent rien d'un paramètre, toutes égales par exemple ; un ajustement sans variance, qui ne converge pas)
  singuliere <- anyNA(V) || min(eigen(Hr, symmetric = TRUE, only.values = TRUE)$values) < 1e-10
  c(base, list(theta = par, ll = v$ll, AIC = -2 * v$ll + 2 * q, H = H, V = V, se = sqrt(pmax(0, diag(V))),
               converge = o$convergence == 0 && is.finite(v$ll) && !any(a_la_borne) && !length(hors_bornes) && !singuliere,
               a_la_borne = any(a_la_borne) || length(hors_bornes) > 0, singuliere = singuliere, contraint = contraint, hors_bornes = hors_bornes))
}

# L'inverse d'une matrice d'information comme mrds (solvecov) : par ses vecteurs propres, une valeur
# propre sous 1e−10 remplacée (son inverse vaut 1e10) ; NA si elle n'est pas inversible.
inverse_information <- function(H, cmax = 1e10) {
  if (inherits(tryCatch(solve(H), error = function(e) e), "error")) return(matrix(NA_real_, nrow(H), ncol(H)))
  e <- eigen(H, symmetric = TRUE)
  inv <- ifelse(e$values < 1 / cmax, cmax, 1 / e$values)
  e$vectors %*% (inv * t(e$vectors))
}

# Les ordres des termes d'ajustement de Distance (get_adj_orders) : cosinus 2, 3, 4… (1, 2, 3… avec
# la clé uniforme) ; Hermite et polynômes 4, 6, 8… (2, 4, 6… avec la clé uniforme).
ordres_ajustement <- function(n, cle, serie) {
  if (!n) return(integer(0))
  if (serie == "cos") return(as.integer(seq(if (cle == "unif") 1 else 2, by = 1, length.out = n)))
  as.integer(seq(if (cle == "unif") 2 else 4, by = 2, length.out = n))
}

# Plus de paramètres que de distances différentes, ou que de classes moins une (mrds, ddf.ds : ceux de la
# clé et des termes d'ajustement, sans les coefficients des covariables).
trop_de_parametres <- function(fd, x) {
  ix <- indices_detection(fd)
  q <- ix$q - max(0, length(ix$echelle) - 1)
  if (!is.null(fd$coupures)) length(fd$coupures) - 2 < q else length(unique(x)) < q
}

# Le libellé d'une fonction de détection : « hn », « hn + cos(2, 3) », et ses covariables : « hr ~ OBs + MAS ».
libelle_detection <- function(fd) {
  x <- if (!length(fd$ordres)) fd$cle else sprintf("%s + %s(%s)", fd$cle, fd$serie, paste(fd$ordres, collapse = ", "))
  if (is.null(fd$formule)) x else paste(x, "~", fd$formule)
}

# Le choix de ds() : la clé seule (sans contrainte), puis 1, 2… termes d'ajustement (au plus `n_max`),
# chacun partant des paramètres du précédent (et 0 pour le nouveau terme ; le premier, des valeurs de
# départ de mrds), tant que l'AIC ne monte pas ; un modèle qui ne converge pas arrête aussi la
# recherche. Avec `n_fixe`, ce nombre de termes seulement. Rend le modèle retenu et les candidats.
choisir_detection <- function(base, x, serie, n_fixe = NULL, n_max = DISTANCE_AJUSTEMENTS_MAX, monotonie = "stricte") {
  fd_de <- function(ordres) {
    fd <- base
    fd$serie <- if (length(ordres)) serie
    fd$ordres <- ordres
    fd
  }
  if (is.null(serie) || identical(serie, "aucune") || identical(n_fixe, 0L) || identical(n_fixe, 0)) {
    m <- ajuster_detection(fd_de(integer(0)), x, monotonie = "aucune")
    return(list(choisi = m, candidats = list(m), raison = "cle"))
  }
  if (!is.null(n_fixe)) {
    m <- ajuster_detection(fd_de(ordres_ajustement(n_fixe, base$cle, serie)), x, monotonie = monotonie)
    return(list(choisi = m, candidats = list(m), raison = "fixe"))
  }
  ordres <- ordres_ajustement(n_max, base$cle, serie)
  meilleur <- ajuster_detection(fd_de(integer(0)), x, monotonie = "aucune")
  candidats <- list(meilleur)
  raison <- "max"
  if (!meilleur$converge) return(list(choisi = meilleur, candidats = candidats, raison = "echec"))
  for (i in seq_along(ordres)) {
    fd <- fd_de(ordres[seq_len(i)])
    depart <- if (i > 1) c(meilleur$theta, 0)
    # (une taille de données trop petite pour tant de paramètres : la recherche s'arrête là, comme mrds)
    if (trop_de_parametres(fd, x)) { raison <- "parametres"; break }
    m <- tryCatch(ajuster_detection(fd, x, depart, monotonie), error = function(e) NULL)
    if (is.null(m) || !m$converge) {
      if (!is.null(m)) candidats[[length(candidats) + 1]] <- m
      raison <- "convergence"
      break
    }
    candidats[[length(candidats) + 1]] <- m
    if (m$AIC > meilleur$AIC) { raison <- "aic"; break }
    meilleur <- m
  }
  list(choisi = meilleur, candidats = candidats, raison = raison)
}

# Les modèles ajustés en chemin par choisir_detection, en un tableau : leur libellé, leur clé, leur
# série et ses ordres, leur nombre de paramètres, leur log-vraisemblance et leur AIC, s'ils convergent,
# si la contrainte de monotonie a agi ; l'écart de chaque AIC au plus petit de ceux qui convergent, et
# le modèle retenu.
candidats_detection <- function(ch) {
  candidats <- do.call(rbind, lapply(ch$candidats, function(c) data.frame(modele = libelle_detection(c$fd), cle = c$fd$cle, serie = if (is.null(c$fd$serie)) "" else c$fd$serie,
    ordres = paste(c$fd$ordres, collapse = " "), parametres = c$q, logL = c$ll, AIC = c$AIC, converge = isTRUE(c$converge), contraint = isTRUE(c$contraint), stringsAsFactors = FALSE)))
  candidats$delta_AIC <- candidats$AIC - min(candidats$AIC[candidats$converge], Inf)
  candidats$choisi <- candidats$modele == libelle_detection(ch$choisi$fd)
  candidats
}

# Le choix de ds() avec des covariables : sans nombre de termes d'ajustement donné, la clé et ses
# covariables seules (ds : « Model contains covariate term(s): no adjustment terms will be included »,
# raison « covariables ») ; avec `n_fixe`, ce nombre de termes ; jamais de contrainte de monotonie
# (ds : « Monotonicity cannot be enforced with covariates »). Le départ de mrds (depart_detection), et
# l'ajustement sur les coefficients réduits (reduction_covariables).
choisir_covariables <- function(base, x, serie, n_fixe = NULL) {
  aucune <- is.null(serie) || identical(serie, "aucune")
  ordres <- if (!aucune && length(n_fixe)) ordres_ajustement(n_fixe, base$cle, serie) else integer(0)
  fd <- base
  fd$serie <- if (length(ordres)) serie
  fd$ordres <- ordres
  m <- ajuster_detection(fd, x, monotonie = "aucune", vraisemblance = vraisemblance_covariables, reduction = reduction_covariables(fd))
  list(choisi = m, candidats = list(m), raison = if (length(ordres)) "fixe" else if (aucune || length(n_fixe)) "cle" else "covariables")
}

# Les modèles ajustés d'une fonction de détection à covariables (candidats_detection), précédés, pour
# comparer les AIC (comme summarize_ds_models de Distance), du modèle retenu sans les covariables
# (`ch0`, choisir_detection avec les mêmes réglages ; NULL s'il ne converge pas) ; la formule de chacun
# (vide sans covariables), l'écart de chaque AIC au plus petit, et le retenu (celui à covariables).
candidats_covariables <- function(ch, ch0) {
  cands <- candidats_detection(ch)
  cands$covariables <- ch$choisi$fd$formule
  if (!is.null(ch0)) {
    c0 <- candidats_detection(ch0)
    c0 <- c0[c0$choisi, , drop = FALSE]
    c0$covariables <- ""
    c0$choisi <- FALSE
    cands <- rbind(c0, cands)
  }
  cands$delta_AIC <- cands$AIC - min(cands$AIC[cands$converge], Inf)
  rownames(cands) <- NULL
  cands
}

# La fonction de détection ajustée `det` sur 101 distances de L à w (les données `t`), et la densité
# des distances, g / ((w − L) p) pour des lignes, f(r) = 2r g(r) / (w² p) pour des points.
courbe_detection <- function(det, t, p) {
  xg <- seq(t$L, t$w, length.out = 101)
  eg <- detection_distance(det$fd, det$theta, xg)
  g <- eg$gt / (1 + eg$s0)
  data.frame(x = xg, g = g, f = if (t$point) 2 * xg * g / (t$w^2 * p) else g / ((t$w - t$L) * p))
}

# Les mêmes courbes d'une fonction de détection à covariables `det`, sur les mêmes distances : la
# fonction de détection moyenne, comme plot.ds de mrds (la moyenne des g(x | zi) des détections
# pondérées par 1/pi, `pr` : probabilites_covariables), et la densité des distances observées, la
# moyenne des f(x | zi) = poids × g(x | zi) / pi des détections (celle que montre l'histogramme des
# distances). Puis, pour chaque covariable qualitative, ces moyennes sur les détections de chacune de
# ses catégories (plot.ds(subset = )) : une colonne par catégorie, et le nombre de ses détections.
courbes_covariables <- function(det, t, pr) {
  fd <- det$fd
  cl <- sans_covariables(fd)
  co <- fd$combinaisons
  xg <- seq(t$L, t$w, length.out = 101)
  G <- matrix(vapply(seq_len(nrow(co$Z)), function(k) {
    e <- detection_distance(cl, theta_combinaison(fd, det$theta, co$Z[k, ]), xg)
    e$gt / (1 + e$s0)
  }, numeric(length(xg))), length(xg))
  pc <- pr$p_combinaisons
  F <- sweep(G, 2, pc, "/") * (if (t$point) 2 * xg / t$w^2 else 1 / (t$w - t$L))
  moyennes_ponderees <- function(dans) {
    nk <- tabulate(co$k[dans], nrow(co$Z))  # (les détections de chaque combinaison)
    list(g = drop(G %*% (nk / pc)) / sum(nk / pc), f = drop(F %*% nk) / sum(nk))
  }
  toutes <- moyennes_ponderees(rep(TRUE, length(co$k)))
  par_niveau <- list()
  for (covariable in names(t$covariables$valeurs)) {
    v <- t$covariables$valeurs[[covariable]]
    if (!is.factor(v)) next
    lv <- levels(droplevels(v))
    mc <- lapply(lv, function(l) moyennes_ponderees(v == l))
    par_niveau[[length(par_niveau) + 1]] <- list(covariable = covariable, niveaux = lv, g = vapply(mc, function(z) z$g, numeric(length(xg))),
                                                 f = vapply(mc, function(z) z$f, numeric(length(xg))), n = vapply(lv, function(l) sum(v == l), 0, USE.NAMES = FALSE))
  }
  list(courbe = data.frame(x = xg, g = toutes$g, f = toutes$f), niveaux = par_niveau)
}

# ---------------------------------------------------------------- l'adéquation

# La fonction de répartition des distances ajustée en chaque x : ∫ de L à x sur ∫ de L à w.
repartition_detection <- function(fd, theta, x) {
  if (!length(x)) return(numeric(0))
  integrale_detection(fd, theta, fd$L, x)$I / integrale_detection(fd, theta, fd$L, fd$w)$I
}

# La loi limite du critère de Cramér–von Mises (Anderson et Darling, 1952), ses quatre premiers termes,
# comme mrds (pcramer).
pcramer_detection <- function(q, eps = 1e-5) {
  sum(vapply(0:3, function(k) {
    z <- gamma(k + 0.5) * sqrt(4 * k + 1) / (gamma(k + 1) * pi^1.5 * sqrt(q))
    u <- (4 * k + 1)^2 / (16 * q)
    if (u > -log(eps)) 0 else z * exp(-u) * besselK(u, 0.25)
  }, 0))
}

# Les classes de l'histogramme et du khi² : celles des distances par intervalles, sinon √n classes
# égales de L à w (gof.ds, plot.ds).
bords_classes <- function(fd, n) {
  if (!is.null(fd$coupures)) return(fd$coupures)
  nc <- max(1, round(sqrt(n)))
  fd$L + (fd$w - fd$L) / nc * (0:nc)
}

# Les tests d'adéquation de mrds (ddf.gof) : Cramér–von Mises sur les distances exactes (W = 1/12n +
# Σ (F(x(i)) − (i − ½)/n)², p asymptotique) et le khi² sur les classes (gof.ds : fermées à droite,
# ddl = classes − 1 − paramètres) ; le diagramme quantile-quantile (la répartition ajustée de chaque
# distance contre la répartition empirique, comme qqplot.ddf). Des distances par intervalles : le khi²
# seulement, sur leurs classes.
adequation_detection <- function(det, x, uid) {
  fd <- det$fd
  n <- length(x)
  bords <- bords_classes(fd, n)
  nc <- length(bords) - 1
  tot <- integrale_detection(fd, det$theta, fd$L, fd$w)$I
  attendu <- n * integrale_detection(fd, det$theta, bords[-(nc + 1)], bords[-1])$I / tot
  tests_adequation(det, x, uid, bords, attendu, if (is.null(fd$coupures)) repartition_detection(fd, det$theta, x))
}

# Les mêmes tests d'une fonction de détection à covariables (mrds : gof.ds, cdf.ds) : la répartition
# ajustée de chaque distance, celle de sa combinaison de covariables (∫ de L à x sur ∫ de L à w,
# sachant ses covariables), pour Cramér–von Mises ; les comptes attendus de chaque classe, la somme sur
# les détections de la part de la classe sachant leurs covariables.
adequation_covariables <- function(det, x, uid) {
  fd <- det$fd
  cl <- sans_covariables(fd)
  co <- fd$combinaisons
  bords <- bords_classes(fd, length(x))
  nc <- length(bords) - 1
  attendu <- numeric(nc)
  F <- numeric(length(x))
  for (k in seq_len(nrow(co$Z))) {
    i <- which(co$k == k)
    th <- theta_combinaison(fd, det$theta, co$Z[k, ])
    tot <- integrale_detection(cl, th, fd$L, fd$w)$I
    attendu <- attendu + length(i) * integrale_detection(cl, th, bords[-(nc + 1)], bords[-1])$I / tot
    if (is.null(fd$coupures)) F[i] <- integrale_detection(cl, th, fd$L, x[i])$I / tot
  }
  tests_adequation(det, x, uid, bords, attendu, if (is.null(fd$coupures)) F)
}

# Les tests d'adéquation d'après les classes (`bords`) et leurs comptes attendus, et la répartition
# ajustée `F` de chaque distance (NULL pour des distances par intervalles) : le khi² (ddl = classes − 1 −
# paramètres), Cramér–von Mises et le diagramme quantile-quantile (adequation_detection).
tests_adequation <- function(det, x, uid, bords, attendu, F) {
  n <- length(x)
  nc <- length(bords) - 1
  observe <- as.vector(table(cut(x, bords, include.lowest = TRUE)))
  chi2 <- sum((observe - attendu)^2 / attendu, na.rm = TRUE)
  ddl <- nc - 1 - det$q
  khi2 <- list(classes = data.frame(de = bords[-length(bords)], a = bords[-1], observe = observe, attendu = attendu),
               chi2 = chi2, ddl = if (ddl > 0) ddl else NA_integer_, p = if (ddl > 0) stats::pchisq(chi2, ddl, lower.tail = FALSE) else NA_real_)
  if (is.null(F)) return(list(khi2 = khi2))
  o <- order(F)
  Fs <- F[o]
  W <- 1 / (12 * n) + sum((Fs - (seq_len(n) - 0.5) / n)^2)
  qq <- data.frame(uid = uid[o], distance = x[o], ajustee = Fs, empirique_bas = vapply(Fs, function(v) sum(Fs < v), 0) / n,
                   empirique = vapply(Fs, function(v) sum(Fs <= v), 0) / n)
  list(cvm = list(W = W, p = 1 - pcramer_detection(W)), khi2 = khi2, qq = qq)
}

# ---------------------------------------------------------------- les unités

# Les longueurs et les aires de Distance (units_table), en mètres et en mètres carrés.
DISTANCE_UNITES_LONGUEUR <- c(m = 1, km = 1000, ft = 0.3048, yd = 0.9144, mi = 1609.344, nmi = 1852)
DISTANCE_UNITES_AIRE <- c(m2 = 1, ha = 1e4, km2 = 1e6, acre = 4046.873, mi2 = 2589988, nmi2 = 3429904)

# Le facteur qui met la troncature w dans l'unité des aires, comme convert_units de Distance : lignes,
# (distance × effort) / aire, 2 w L est alors une aire ; points, distance / √aire, π w² en est une.
facteur_echantillonnage <- function(r) {
  if (!is.null(r$facteur)) return(r$facteur)
  du <- DISTANCE_UNITES_LONGUEUR[[r$unites$distance]]
  au <- DISTANCE_UNITES_AIRE[[r$unites$aire]]
  if (r$transect == "point") du / sqrt(au) else du * DISTANCE_UNITES_LONGUEUR[[r$unites$effort]] / au
}

# ---------------------------------------------------------------- réglages et données

# Les réglages (d$options), complétés : les lignes ou les points ; le transect, la strate (des
# list(by = "id", id = ...) ou list(by = "variable", variable = ...)) ; les variables de l'effort, de
# l'aire de la strate et de la taille des groupes ; la troncature à droite (aucune : la plus grande
# distance, comme ds ; une distance ; un pourcentage des plus grandes) et à gauche ; la clé, la série
# d'ajustement, leur nombre ("aic" : choisi par l'AIC, ou 0 à 5), leur échelle (la troncature w,
# comme ds par défaut, ou σ), la monotonie ; les unités ou un facteur de conversion donné tel quel ;
# les bornes des intervalles de distances relevées par classes (ds : cutpoints), qui font alors les
# troncatures (la première à gauche, la dernière w) ; les covariables de la fonction de détection, des
# numéros de variables (les variables Z de l'onglet Variables, d$options$z ; ds(formula = )).
reglages_echantillonnage <- function(d) {
  o <- d$options %||% list()
  spec <- function(x) if (length(x$by)) specs_groupement(x)[[1]]
  nombre <- function(x) {
    v <- suppressWarnings(as.numeric(unlist(x)))
    if (length(v) && is.finite(v[1])) v[1]
  }
  texte <- function(x, permis, defaut) {
    v <- as.character(unlist(x))
    if (length(v) && v[1] %in% permis) v[1] else defaut
  }
  tr <- o$troncature %||% list()
  aj <- as.character(unlist(o$ajustements %||% "aic"))[1]
  u <- o$unites %||% list()
  coupures <- suppressWarnings(as.numeric(unlist(o$intervalles)))
  coupures <- coupures[!is.na(coupures)]
  z <- suppressWarnings(as.numeric(unlist(o$z)))
  list(transect = texte(o$transect, c("ligne", "point"), "ligne"), echantillon = spec(o$echantillon), strate = spec(o$strate),
       intervalles = if (length(coupures)) coupures,
       effort = nombre(o$effort), aire = nombre(o$aire), taille = nombre(o$taille),
       troncature = list(type = texte(tr$type, c("aucune", "distance", "pourcentage"), "aucune"), valeur = nombre(tr$valeur)),
       gauche = nombre(o$gauche) %||% 0, cle = texte(o$cle, DISTANCE_CLES, "hn"), serie = texte(o$serie, c(DISTANCE_SERIES, "aucune"), "cos"),
       ajustements = if (identical(aj, "aic") || is.na(aj)) "aic" else as.integer(nombre(aj) %||% 0),
       echelle_ajustements = texte(o$echelle_ajustements, c("largeur", "echelle"), "largeur"),
       monotonie = texte(o$monotonie, c("stricte", "faible", "aucune"), "stricte"),
       unites = list(distance = texte(u$distance, names(DISTANCE_UNITES_LONGUEUR), "m"), effort = texte(u$effort, names(DISTANCE_UNITES_LONGUEUR), "km"),
                     aire = texte(u$aire, names(DISTANCE_UNITES_AIRE), "km2")),
       facteur = nombre(o$facteur), covariables = unique(z[!is.na(z)]))
}

# Les clés d'un transect ou d'une strate en texte (un nombre tel qu'il s'écrit).
textes_echantillonnage <- function(v) if (is.numeric(v)) ifelse(is.na(v), NA_character_, sprintf("%.15g", v)) else as.character(v)

# L'ordre des strates et des transects : celui des catégories d'une variable qualitative, des nombres,
# sinon des noms (comparaisons.R : ordre_cles).
ordre_echantillonnage <- function(projet, spec, v) {
  u <- unique(v[!is.na(v)])
  if (identical(spec$by, "variable")) {
    vv <- variable_du_projet(projet, spec$variable)
    if (length(vv$categories)) return(intersect(as.character(vv$categories), u))
    return(u[order(as.numeric(u))])
  }
  ordre_cles(u)
}

# Les données (`r` : reglages_echantillonnage) : les distances de chaque ligne, son transect, sa strate,
# l'effort de son transect, l'aire de sa strate, la taille du groupe ; la troncature ; la table des
# transects (effort, détections et individus dans la troncature) et celle des strates. Les refus
# nomment la ligne, le transect ou la strate en cause.
donnees_echantillonnage <- function(projet, d, r) {
  err <- function(fr, en, ...) stop(sprintf(traduire(fr, en), ...), call. = FALSE)
  nombre <- function(x) format(x, digits = 15)
  if (length(d$spectres)) err("L'échantillonnage par distances ne prend pas de prétraitement de spectres.", "Distance sampling takes no spectral preprocessing.")
  if (length(d$x)) err("L'échantillonnage par distances ne prend pas de variable X : les covariables de la fonction de détection sont des variables Z (onglet Variables).",
                       "Distance sampling takes no X variable: the covariates of the detection function are Z variables (Variables tab).")
  uy <- unique(vapply(d$y, function(t) as.numeric(t$variables[1]), 0))
  if (length(uy) != 1 || any(vapply(d$y, function(t) length(t$variables) != 1, TRUE))) {
    err("L'échantillonnage par distances demande une seule variable Y : les distances.", "Distance sampling needs a single Y variable: the distances.")
  }
  vy <- variable_du_projet(projet, uy)
  nom_y <- as.character(vy$ids[1])
  if (length(vy$categories)) err("Les distances (Y) doivent être une variable quantitative : « %s » est qualitative.", "The distances (Y) must be a quantitative variable: “%s” is qualitative.", nom_y)
  if (!is.null(d$y[[1]]$transform)) err("Les distances sont prises telles qu'elles sont : retirez la transformation de « %s ».", "The distances are taken as they are: remove the transformation of “%s”.", nom_y)
  if (is.null(r$echantillon)) err("L'échantillonnage par distances demande le transect de chaque ligne (onglet Général).", "Distance sampling needs the transect of each row (General tab).")
  if (identical(r$echantillon, r$strate)) err("Le transect et la strate doivent être deux colonnes différentes.", "The transect and the stratum must be two different columns.")
  point <- r$transect == "point"
  if (is.null(r$effort) && !point) err("Des transects linéaires demandent l'effort de chaque transect : la longueur de la ligne (onglet Général).", "Line transects need the effort of each transect: the length of the line (General tab).")
  quant <- function(u, quoi_fr, quoi_en) {
    v <- variable_du_projet(projet, u)
    if (length(v$categories)) err("%s (%s) doit être une variable quantitative.", "%s (%s) must be a quantitative variable.", traduire(quoi_fr, quoi_en), v$ids[1])
    as.character(v$ids[1])
  }
  noms <- list(y = nom_y, transect = nom_groupement(projet, r$echantillon), strate = if (!is.null(r$strate)) nom_groupement(projet, r$strate),
               effort = if (!is.null(r$effort)) quant(r$effort, "L'effort", "The effort"), aire = if (!is.null(r$aire)) quant(r$aire, "L'aire", "The area"),
               taille = if (!is.null(r$taille)) quant(r$taille, "La taille des groupes", "The group size"))
  obs <- as.numeric(d$observations)
  noms_obs <- unname(projet$noms_obs[as.character(obs)])
  x <- colonne(projet, uy, obs)
  tr <- textes_echantillonnage(valeurs_groupement(projet, r$echantillon, obs))
  st <- if (!is.null(r$strate)) textes_echantillonnage(valeurs_groupement(projet, r$strate, obs)) else rep("", length(obs))
  # (une ligne sans transect ni distance, une ligne vide du tableau, est laissée de côté)
  vide <- is.na(tr) & is.na(x)
  k <- which(is.na(tr) & !is.na(x))
  if (length(k)) err("%s a une distance mais pas de transect (%s).", "%s has a distance but no transect (%s).", noms_obs[k[1]], noms$transect)
  garde <- !vide
  k <- which(garde & is.na(st))
  if (length(k)) err("%s n'a pas de strate (%s).", "%s has no stratum (%s).", noms_obs[k[1]], noms$strate)
  k <- which(garde & !is.na(x) & (!is.finite(x) | x < 0))
  if (length(k)) err("Les distances (%s) sont positives ou nulles : %s vaut %s.", "The distances (%s) are positive or zero: %s is %s.", nom_y, noms_obs[k[1]], nombre(x[k[1]]))
  effort <- if (!is.null(r$effort)) colonne(projet, r$effort, obs) else rep(1, length(obs))
  k <- which(garde & (is.na(effort) | !(effort > 0)))
  if (length(k)) {
    if (is.na(effort[k[1]])) err("%s n'a pas d'effort (%s).", "%s has no effort (%s).", noms_obs[k[1]], noms$effort)
    err("L'effort (%s) est positif : %s vaut %s.", "The effort (%s) is positive: %s is %s.", noms$effort, noms_obs[k[1]], nombre(effort[k[1]]))
  }
  cle <- paste(st, tr, sep = "\r")
  for (c in unique(cle[garde])) {
    e <- unique(effort[garde & cle == c])
    if (length(e) > 1) {
      i <- which(garde & cle == c)
      err("Le transect %s a deux efforts (%s) : %s et %s.", "Transect %s has two efforts (%s): %s and %s.", tr[i[1]], noms$effort, nombre(e[1]), nombre(e[2]))
    }
  }
  aire <- NULL
  if (!is.null(r$aire)) {
    aire <- colonne(projet, r$aire, obs)
    k <- which(garde & (is.na(aire) | aire < 0))
    if (length(k)) {
      if (is.na(aire[k[1]])) err("%s n'a pas d'aire (%s).", "%s has no area (%s).", noms_obs[k[1]], noms$aire)
      err("L'aire (%s) est positive : %s vaut %s.", "The area (%s) is positive: %s is %s.", noms$aire, noms_obs[k[1]], nombre(aire[k[1]]))
    }
    for (s in unique(st[garde])) {
      a <- unique(aire[garde & st == s])
      if (length(a) > 1) {
        if (nzchar(s)) err("La strate %s a deux aires (%s) : %s et %s.", "Stratum %s has two areas (%s): %s and %s.", s, noms$aire, nombre(a[1]), nombre(a[2]))
        err("L'aire (%s) est celle de toute la région : une seule valeur sans strate (%s et %s).", "The area (%s) is that of the whole region: a single value without strata (%s and %s).", noms$aire, nombre(a[1]), nombre(a[2]))
      }
    }
    a <- unique(aire[garde])
    # (des aires toutes nulles : la densité seulement, comme dht2)
    if (all(a == 0)) aire <- NULL
    else if (any(a == 0)) {
      s <- st[garde & aire == 0][1]
      err("L'aire de la strate %s est nulle quand d'autres en ont une.", "The area of stratum %s is zero while others have one.", s)
    }
  }
  taille <- NULL
  if (!is.null(r$taille)) {
    taille <- colonne(projet, r$taille, obs)
    k <- which(garde & !is.na(x) & (is.na(taille) | !(taille > 0)))
    if (length(k)) {
      if (is.na(taille[k[1]])) err("%s n'a pas de taille de groupe (%s) : une détection en demande une.", "%s has no group size (%s): a detection needs one.", noms_obs[k[1]], noms$taille)
      err("La taille des groupes (%s) est positive : %s vaut %s.", "The group size (%s) is positive: %s is %s.", noms$taille, noms_obs[k[1]], nombre(taille[k[1]]))
    }
  }
  # la troncature
  xs <- x[garde & !is.na(x)]
  if (!length(xs)) err("Aucune distance : %s n'a que des valeurs vides.", "No distances: %s has only empty values.", nom_y)
  L <- r$gauche
  w <- switch(r$troncature$type,
    aucune = max(xs),
    distance = r$troncature$valeur %||% err("La troncature est une distance.", "The truncation is a distance."),
    pourcentage = {
      v <- r$troncature$valeur %||% NA_real_
      if (!is.finite(v) || v < 0 || v >= 100) err("La troncature est un pourcentage de 0 à 100 (exclu).", "The truncation is a percentage from 0 to 100 (excluded).")
      unname(stats::quantile(xs, 1 - v / 100))
    })
  coupures <- r$intervalles
  if (!is.null(coupures)) {
    # (des distances par intervalles : les bornes font les troncatures, comme ds(cutpoints = ))
    if (length(coupures) < 3 || any(diff(coupures) <= 0) || coupures[1] < 0) {
      err("Les intervalles de distances sont au moins trois bornes croissantes, positives ou nulles (0 10 25 50…).", "The distance intervals are at least three increasing bounds, positive or zero (0 10 25 50…).")
    }
    L <- coupures[1]
    w <- coupures[length(coupures)]
  }
  if (!is.finite(L) || L < 0) err("La troncature à gauche est une distance positive ou nulle.", "The left truncation is a distance, positive or zero.")
  if (!is.finite(w) || w <= L) err("La troncature (%s) doit dépasser la troncature à gauche (%s).", "The truncation (%s) must exceed the left truncation (%s).", nombre(w), nombre(L))
  dans <- garde & !is.na(x) & x >= L & x <= w
  n <- sum(dans)
  if (n < 2) err("Moins de deux distances entre %s et %s.", "Fewer than two distances between %s and %s.", nombre(L), nombre(w))
  # les covariables de la fonction de détection, connues pour chaque détection dans la troncature
  # (mrds : create.model.frame refuse une valeur manquante) ; une qualitative en facteur, ses catégories
  # dans leur ordre (la première présente, la référence) ; puis leur matrice (matrice_covariables)
  valeurs <- list()
  for (u in r$covariables) {
    v <- variable_du_projet(projet, u)
    nom <- as.character(v$ids[1])
    if (r$cle == "unif") err("La clé uniforme ne prend pas de covariables (comme ds) : choisissez la clé demi-normale ou hasard.", "The uniform key takes no covariates (as ds): choose the half-normal or hazard-rate key.")
    xv <- colonne(projet, u, obs)
    k <- which(dans & !is.finite(xv))
    if (length(k)) err("%s n'a pas de valeur de la covariable %s : chaque détection en demande une.", "%s has no value of the covariate %s: each detection needs one.", noms_obs[k[1]], nom)
    valeurs[[nom]] <- if (length(v$categories)) factor(as.character(v$categories)[xv[dans]], levels = as.character(v$categories)) else xv[dans]
    if (length(v$categories) && nlevels(droplevels(valeurs[[nom]])) < 2) {
      err("La covariable %s n'a qu'une catégorie parmi les détections (%s) : elle ne distingue rien.", "The covariate %s has a single category among the detections (%s): it distinguishes nothing.",
          nom, levels(droplevels(valeurs[[nom]])))
    }
    if (!length(v$categories) && length(unique(valeurs[[nom]])) < 2) {
      err("La covariable %s a la même valeur pour toutes les détections : elle ne distingue rien.", "The covariate %s has the same value for all the detections: it distinguishes nothing.", nom)
    }
  }
  Z <- if (length(valeurs)) matrice_covariables(valeurs)
  if (!is.null(Z) && qr(Z)$rank < ncol(Z)) {
    err("Les covariables (%s) sont liées entre elles parmi les détections : l'une se déduit des autres, retirez-la.",
        "The covariates (%s) are linked among the detections: one follows from the others, remove it.", paste(names(valeurs), collapse = ", "))
  }
  # les strates et leurs transects, dans l'ordre
  strates <- if (!is.null(r$strate)) ordre_echantillonnage(projet, r$strate, st[garde]) else ""
  lignes <- list()
  for (s in strates) {
    ts <- ordre_echantillonnage(projet, r$echantillon, tr[garde & st == s])
    for (e in ts) {
      i <- garde & st == s & tr == e
      lignes[[length(lignes) + 1]] <- data.frame(strate = s, transect = e, effort = effort[i][1], n = sum(dans & i),
                                                individus = if (is.null(taille)) sum(dans & i) else sum(taille[dans & i]), stringsAsFactors = FALSE)
    }
  }
  transects <- do.call(rbind, lignes)
  strates_t <- data.frame(strate = strates, aire = if (is.null(aire)) NA_real_ else vapply(strates, function(s) aire[garde & st == s][1], 0),
                          stringsAsFactors = FALSE)
  rownames(strates_t) <- NULL
  list(x = x[dans], uid = obs[dans], strate_x = st[dans], taille_x = if (!is.null(taille)) taille[dans], w = w, L = L, point = point,
       transects = transects, strates = strates_t, densite_seule = is.null(aire), groupes = !is.null(taille),
       observations = obs[garde], lignes = data.frame(uid = obs[garde], nom = noms_obs[garde], transect = tr[garde], strate = st[garde], distance = x[garde],
                                                      dans = dans[garde], taille = if (!is.null(taille)) taille[garde] else NA_real_, stringsAsFactors = FALSE),
       vides = sum(vide), au_dela = sum(garde & !is.na(x) & x > w), en_deca = sum(garde & !is.na(x) & x < L), n = n, noms = noms,
       pourcentage = if (r$troncature$type == "pourcentage" && is.null(coupures)) r$troncature$valeur, coupures = coupures,
       covariables = if (length(valeurs)) list(noms = names(valeurs), valeurs = valeurs, qualitatives = vapply(valeurs, is.factor, TRUE, USE.NAMES = FALSE),
                                              references = attr(Z, "references"), termes = attr(Z, "termes")), Z = Z)
}

# ---------------------------------------------------------------- densité et effectif

# La variance du taux de rencontre de mrds (varn) : R2 (lignes) et P2 (points) de Fewster et al.
# (2009) ; `l` les efforts des transects, `n` leurs détections ; 0 avec un seul transect (dht2).
variance_rencontre <- function(l, n, type) {
  k <- length(l)
  if (k < 2) return(0)
  L <- sum(l)
  if (type == "R2") return(k * sum(l^2 * (n / l - sum(n) / L)^2) / (L^2 * (k - 1)))
  sum((n / l - mean(n / l))^2) / (k * (k - 1))
}

# La densité et l'effectif par strate et au total, comme dht2 (stratification géographique, sans
# multiplicateur) : `t` les données, `p`, `var_p` la probabilité de détection et sa variance, `q` les
# paramètres de la fonction de détection, `cu` le facteur des unités. Une fonction de détection à
# covariables (`ht` : la probabilité de détection de chaque détection, son gradient par les paramètres
# et leur covariance V) : l'effectif couvert de Horvitz–Thompson Σ si / pi (si la taille du groupe, 1
# sans groupes), et la variance de la détection par la méthode delta, ∇N' V ∇N, ∇N = −(A / a) Σ si
# ∇pi / pi² (pour le total, la somme des gradients des strates pondérés comme leurs effectifs). Sans
# aire de strate, le total pondère les densités des strates par leurs aires couvertes wi : les
# variances du taux de rencontre et de la taille des groupes de chaque strate y entrent avec wi², et
# ses degrés de liberté sont ceux de Satterthwaite avec ces poids. dht2 laisse les poids hors de la
# variance de la taille des groupes et du dénominateur des degrés de liberté (se_D_dht2 et ddl_dht2 :
# ce qu'il donne, quand ils diffèrent).
densite_echantillonnage <- function(t, p, var_p, q, cu, ht = NULL) {
  type <- if (t$point) "P2" else "R2"
  nddf <- t$n
  ic <- function(N, cv, ddl) {
    C <- if (is.finite(cv) && cv > 0) exp(abs(stats::qt(0.025, ddl)) * sqrt(log(1 + cv^2))) else 1
    c(N / C, N * C)
  }
  ligne <- function(s) {
    tr <- t$transects[t$transects$strate == s, , drop = FALSE]
    k <- nrow(tr)
    effort <- sum(tr$effort)
    n <- sum(tr$n)
    couverte <- if (t$point) pi * (t$w * cu)^2 * effort else 2 * (t$w - t$L) * cu * effort
    A <- if (t$densite_seule) 1 else t$strates$aire[t$strates$strate == s]
    gN <- NULL
    if (is.null(ht)) Nc <- sum(tr$individus) / p
    else {
      i <- t$strate_x == s
      si <- if (t$groupes) t$taille_x[i] else rep(1, sum(i))
      Nc <- sum(si / ht$p[i])
      gN <- -(A / couverte) * colSums(si / ht$p[i]^2 * ht$dp[i, , drop = FALSE])
    }
    N <- A / couverte * Nc
    er <- n / effort
    er_var <- variance_rencontre(tr$effort, tr$n, type)
    er_cv <- if (er == 0) 0 else sqrt(er_var) / er
    er_ddl <- max(1, k - 1)
    var_er_N <- if (n > 0) (A / couverte * Nc * effort)^2 * er_var / n^2 else 0
    var_det_N <- if (is.null(ht)) N^2 * var_p / p^2 else drop(crossprod(gN, ht$V %*% gN))
    det_cv <- if (N == 0) 0 else sqrt(var_det_N) / N
    tailles <- if (t$groupes) t$taille_x[t$strate_x == s] else rep(1, n)
    g_var <- if (n > 1) stats::var(tailles) / n else 0
    g_moy <- if (n > 0) mean(tailles) else 1
    g_cv <- if (g_var == 0) 0 else sqrt(g_var) / g_moy
    # (une strate sans détection : N = 0, erreur-type 0, ni CV ni ddl, comme dht2)
    se <- sqrt(var_er_N + var_det_N + N^2 * g_cv^2)
    cv <- if (N > 0) se / N else NA_real_
    ddl <- cv^4 / sum(c(if (k == 1) 0 else er_cv^4 / er_ddl, det_cv^4 / (nddf - q), g_cv^4 / (n - 1)), na.rm = TRUE)
    if (is.finite(cv) && cv == 0) ddl <- 1
    if (is.finite(ddl) && ddl < 1) ddl <- 1
    list(strate = s, aire = if (t$densite_seule) NA_real_ else A, couverte = couverte, effort = effort, k = k, n = n, individus = sum(tr$individus),
         rencontre = er, se_rencontre = sqrt(er_var), cv_rencontre = er_cv, ddl_rencontre = er_ddl, N = N, se = se, cv = cv, ddl = ddl, ddl_dht2 = NA_real_,
         se_dht2 = NA_real_, var_er_N = var_er_N, var_det_N = var_det_N, var_groupes_N = N^2 * g_cv^2, taille_moyenne = g_moy, se_taille = sqrt(g_var), Nc = Nc, cv_detection = det_cv,
         cv_groupes = g_cv, gradient = gN)
  }
  lignes <- lapply(t$strates$strate, ligne)
  if (length(lignes) > 1) {
    de_chaque <- function(f) vapply(lignes, f, 0)  # (une grandeur de chaque strate : f de sa ligne)
    couverte <- de_chaque(function(l) l$couverte)
    poids <- if (t$densite_seule) couverte / sum(couverte) else rep(1, length(lignes))
    N <- sum(poids * de_chaque(function(l) l$N))
    var_er_N <- sum(poids^2 * de_chaque(function(l) l$var_er_N))
    # (les degrés de liberté de Satterthwaite de cette somme pondérée, (Σ wi² vi)² / Σ (wi² vi)² / ddli,
    # vi la variance du taux de rencontre de la strate i ; dht2 laisse les poids hors du dénominateur,
    # (Σ wi² vi)² / Σ vi² / ddli, ce qui en donne bien moins sans aire de strate : ceux de son total,
    # ddl_dht2, quand ils diffèrent ; avec des aires, wi = 1 et les deux sont les mêmes)
    er_ddl <- var_er_N^2 / sum((poids^2 * de_chaque(function(l) l$var_er_N))^2 / de_chaque(function(l) l$ddl_rencontre))
    er_ddl_dht2 <- var_er_N^2 / sum(de_chaque(function(l) l$var_er_N^2 / l$ddl_rencontre))
    gN <- if (!is.null(ht)) Reduce(`+`, Map(function(l, a) a * l$gradient, lignes, poids))
    var_det_N <- if (is.null(ht)) N^2 * var_p / p^2 else drop(crossprod(gN, ht$V %*% gN))
    # (la variance de la taille des groupes : celle de chaque strate avec le carré de son poids ; dht2
    # les additionne sans les poids, var_g_dht2, d'où son erreur-type, se_dht2, quand elle diffère)
    var_g_N <- sum(poids^2 * de_chaque(function(l) l$var_groupes_N))
    var_g_dht2 <- sum(de_chaque(function(l) l$var_groupes_N))
    tvar <- var_er_N + var_det_N + var_g_N
    cv <- if (N > 0) sqrt(tvar) / N else 0
    satterthwaite <- function(er_ddl, var_g) (var_er_N / N^2 + var_det_N / N^2 + var_g / N^2)^2 /
      sum(c((var_er_N / N^2)^2 / er_ddl, (var_det_N / N^2)^2 / (nddf - q), (var_g / N^2)^2 / (nddf - 1)), na.rm = TRUE)
    ddl <- satterthwaite(er_ddl, var_g_N)
    ddl_dht2 <- if (t$densite_seule) satterthwaite(er_ddl_dht2, var_g_dht2) else NA_real_
    if (isTRUE(all.equal(ddl_dht2, ddl))) ddl_dht2 <- NA_real_
    se_dht2 <- if (t$densite_seule) sqrt(var_er_N + var_det_N + var_g_dht2) else NA_real_
    if (isTRUE(all.equal(se_dht2, sqrt(tvar)))) se_dht2 <- NA_real_
    effort <- sum(de_chaque(function(l) l$effort))
    n <- sum(de_chaque(function(l) l$n))
    individus <- sum(de_chaque(function(l) l$individus))
    er_var <- variance_rencontre(t$transects$effort, t$transects$n, type)
    lignes[[length(lignes) + 1]] <- list(strate = NA_character_, aire = if (t$densite_seule) NA_real_ else sum(de_chaque(function(l) l$aire)), couverte = sum(couverte),
      effort = effort, k = sum(de_chaque(function(l) l$k)), n = n, individus = individus, rencontre = n / effort, se_rencontre = sqrt(er_var),
      cv_rencontre = if (n == 0) 0 else sqrt(er_var) / (n / effort), ddl_rencontre = er_ddl, N = N, se = sqrt(tvar), cv = cv, ddl = ddl, ddl_dht2 = ddl_dht2, se_dht2 = se_dht2, var_er_N = var_er_N,
      var_det_N = var_det_N, var_groupes_N = var_g_N, taille_moyenne = individus / max(1, n), se_taille = NA_real_, Nc = sum(poids * de_chaque(function(l) l$Nc)),
      cv_detection = if (N > 0) sqrt(var_det_N) / N else 0, cv_groupes = if (N > 0) sqrt(var_g_N) / N else 0)
  }
  out <- do.call(rbind, lapply(lignes, function(l) {
    b <- ic(l$N, l$cv, l$ddl)
    A <- if (t$densite_seule) 1 else l$aire
    data.frame(strate = l$strate, aire = l$aire, couverte = l$couverte, effort = l$effort, k = l$k, n = l$n, individus = l$individus,
               rencontre = l$rencontre, se_rencontre = l$se_rencontre, cv_rencontre = l$cv_rencontre, ddl_rencontre = l$ddl_rencontre,
               D = l$N / A, se_D = l$se / A, cv = l$cv, bas_D = b[1] / A, haut_D = b[2] / A,
               N = if (t$densite_seule) NA_real_ else l$N, se_N = if (t$densite_seule) NA_real_ else l$se,
               bas_N = if (t$densite_seule) NA_real_ else b[1], haut_N = if (t$densite_seule) NA_real_ else b[2], ddl = l$ddl, ddl_dht2 = l$ddl_dht2, se_D_dht2 = l$se_dht2 / A,
               cv_detection = l$cv_detection, cv_groupes = l$cv_groupes, taille_moyenne = l$taille_moyenne, se_taille = l$se_taille,
               part_rencontre = if (isTRUE(l$cv > 0)) 100 * (l$var_er_N / l$N^2) / l$cv^2 else NA_real_,
               part_detection = if (isTRUE(l$cv > 0)) 100 * (l$var_det_N / l$N^2) / l$cv^2 else NA_real_,
               part_groupes = if (isTRUE(l$cv > 0)) 100 * (l$var_groupes_N / l$N^2) / l$cv^2 else NA_real_, stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------- ajuster

# g(x | zi) à la distance de chaque détection d'une fonction de détection à covariables, sachant ses
# covariables (celle de sa combinaison, rapportée à sa valeur en 0).
detection_covariables <- function(fd, theta, x) {
  cl <- sans_covariables(fd)
  co <- fd$combinaisons
  g <- numeric(length(x))
  for (k in seq_len(nrow(co$Z))) {
    i <- which(co$k == k)
    e <- detection_distance(cl, theta_combinaison(fd, theta, co$Z[k, ]), x[i])
    g[i] <- e$gt / (1 + e$s0)
  }
  g
}

# L'échantillonnage par distances (definir_modele, type "DISTANCE") : Y les distances, et les
# réglages de reglages_echantillonnage (d$options). Avec des covariables (ds(formula = ), Marques et
# Buckland, 2003) : la fonction de détection à covariables choisie comme ds (choisir_covariables), et,
# pour comparer les AIC, la même sans covariables ; la probabilité de détection de chaque détection,
# leur moyenne p̄ (moyenne_detection), et la densité de Horvitz–Thompson de dht2.
ajuster_distance <- function(projet, d) {
  r <- reglages_echantillonnage(d)
  t <- donnees_echantillonnage(projet, d, r)
  if (r$cle == "unif" && r$serie == "aucune" && identical(r$ajustements, "aic")) r$ajustements <- 0L
  if (!identical(r$ajustements, "aic") && (r$ajustements < 0 || r$ajustements > DISTANCE_AJUSTEMENTS_MAX)) {
    stop(sprintf(traduire("Le nombre de termes d'ajustement va de 0 à %d.", "The number of adjustment terms goes from 0 to %d."), DISTANCE_AJUSTEMENTS_MAX), call. = FALSE)
  }
  base <- list(cle = r$cle, w = t$w, L = t$L, point = t$point, echelle_ajustements = r$echelle_ajustements, ordres = integer(0), coupures = t$coupures)
  serie <- if (r$serie != "aucune") r$serie
  if (!is.null(t$coupures) && trop_de_parametres(base, t$x)) {
    stop(sprintf(traduire("Trop peu d'intervalles (%d) pour estimer la clé %s : il en faut au moins un de plus que de paramètres.",
                          "Too few intervals (%d) to estimate the %s key: it takes at least one more than parameters."), length(t$coupures) - 1, r$cle), call. = FALSE)
  }
  n_fixe <- if (!identical(r$ajustements, "aic")) r$ajustements
  covariables <- !is.null(t$Z)
  if (covariables) {
    ch <- choisir_covariables(avec_covariables(base, t$Z), t$x, serie, n_fixe)
    # (le même modèle sans covariables, pour comparer les AIC ; laissé de côté s'il ne converge pas)
    ch0 <- tryCatch(choisir_detection(base, t$x, serie, n_fixe = n_fixe, monotonie = r$monotonie), error = function(e) NULL)
    if (!isTRUE(ch0$choisi$converge)) ch0 <- NULL
  } else ch <- choisir_detection(base, t$x, serie, n_fixe = n_fixe, monotonie = r$monotonie)
  det <- ch$choisi
  # (la combinaison de covariables `z`, en mots : « OBs = TJS, MAS = 50 »)
  combinaison <- function(z) {
    tm <- t$covariables$termes
    paste(vapply(t$covariables$noms, function(nom) {
      j <- which(tm$covariable == nom)
      if (is.na(tm$niveau[j[1]])) return(sprintf("%s = %s", nom, format(z[j[1]], digits = 6)))
      un <- j[z[j] == 1]
      sprintf("%s = %s", nom, if (length(un)) tm$niveau[un[1]] else t$covariables$references[[nom]])
    }, ""), collapse = ", ")
  }
  if (!det$converge) {
    pourquoi <- if (length(det$hors_bornes)) sprintf(traduire(" (l'échelle σ de %s part à l'infini)", " (the scale σ of %s runs off to infinity)"), combinaison(det$fd$combinaisons$Z[det$hors_bornes[1], ]))
                else if (isTRUE(det$a_la_borne)) traduire(" (un paramètre part à l'infini)", " (a parameter runs off to infinity)")
                else if (isTRUE(det$singuliere)) traduire(" (les distances ne disent rien d'un de ses paramètres)", " (the distances say nothing about one of its parameters)") else ""
    stop(sprintf(traduire("La fonction de détection %s ne converge pas%s : essayez une autre clé, moins de termes d'ajustement ou une autre troncature%s.",
                          "The detection function %s doesn't converge%s: try another key, fewer adjustment terms or another truncation%s."),
                 libelle_detection(det$fd), pourquoi, if (covariables) traduire(", ou d'autres covariables (des catégories regroupées)", ", or other covariates (categories grouped)") else ""), call. = FALSE)
  }
  cu <- facteur_echantillonnage(r)
  # (des groupes : la densité des groupes aussi, comme dht2 la donne en attribut « grouped »)
  tg <- t
  tg$groupes <- FALSE
  tg$transects$individus <- tg$transects$n
  tailles <- if (t$groupes) t$taille_x else rep(1, t$n)
  pr <- NULL
  if (covariables) {
    pr <- probabilites_covariables(det$fd, det$theta)
    moy <- moyenne_detection(pr, det$V)
    p <- moy$p
    var_p <- moy$var_p
    ht <- list(p = pr$p, dp = pr$dp, V = det$V)
    dens <- densite_echantillonnage(t, p, var_p, det$q, cu, ht)
    dens_groupes <- if (t$groupes) densite_echantillonnage(tg, p, var_p, det$q, cu, ht)
    Nc <- sum(tailles / pr$p)
    courbes <- courbes_covariables(det, t, pr)
    adequation <- adequation_covariables(det, t$x, t$uid)
    g_x <- detection_covariables(det$fd, det$theta, t$x)
    candidats <- candidats_covariables(ch, ch0)
    # (des termes d'ajustement sans contrainte : g croît-elle, dépasse-t-elle 1 ou devient-elle négative
    # pour une combinaison de covariables, en 10 distances de 0 à w ? ds le signale, mrds : check.mono)
    co <- det$fd$combinaisons
    monotone <- if (length(det$fd$ordres)) all(vapply(seq_len(nrow(co$Z)), function(k)
      min(contraintes_monotonie(sans_covariables(det$fd), theta_combinaison(det$fd, det$theta, co$Z[k, ]), TRUE)$c) >= -1e-8, TRUE))
  } else {
    pp <- probabilite_detection(det$fd, det$theta)
    p <- pp$p
    var_p <- if (det$q) drop(t(pp$dp) %*% det$V %*% pp$dp) else 0
    dens <- densite_echantillonnage(t, p, var_p, det$q, cu)
    dens_groupes <- if (t$groupes) densite_echantillonnage(tg, p, var_p, det$q, cu)
    Nc <- sum(tailles) / p
    courbes <- list(courbe = courbe_detection(det, t, p), niveaux = list())
    adequation <- adequation_detection(det, t$x, t$uid)
    ex <- detection_distance(det$fd, det$theta, t$x)
    g_x <- ex$gt / (1 + ex$s0)
    candidats <- candidats_detection(ch)
    monotone <- NULL
  }
  se_p <- sqrt(max(0, var_p))
  largeur <- t$w - t$L
  # (lignes : la demi-largeur effective de bande ESW = p (w − L) ; points : le rayon effectif de détection
  # EDR = w √p, son CV la moitié de celui de p ; avec des covariables, de p̄)
  effectif <- if (t$point) list(quoi = "EDR", valeur = t$w * sqrt(p), se = t$w * sqrt(p) * se_p / p / 2)
              else list(quoi = "ESW", valeur = p * largeur, se = se_p * largeur)
  parametres <- if (det$q) data.frame(parametre = noms_parametres_detection(det$fd), estimation = det$theta, se = det$se, stringsAsFactors = FALSE)
  m <- list(type = "DISTANCE", N = length(t$observations), observations = t$observations, reglages_echantillonnage = r, donnees = t,
            detection = det, candidats = candidats, selection = ch$raison, parametres = parametres,
            p = p, se_p = se_p, effectif = effectif, Nc = Nc,
            adequation = adequation, courbe = courbes$courbe, courbes_niveaux = courbes$niveaux, g_x = g_x, densite = dens, densite_groupes = dens_groupes,
            facteur = cu, var_p = var_p, p_detections = if (covariables) pr$p, monotone = monotone)
  class(m) <- "modele_distance"
  m
}

print.modele_distance <- function(x, ...) {
  cat(sprintf("Échantillonnage par distances (%s) : %s, n = %d, w = %s ; p = %s (se %s), AIC %s\n", if (x$donnees$point) "points" else "lignes",
              libelle_detection(x$detection$fd), x$donnees$n, format(x$donnees$w, digits = 6), format(x$p, digits = 5), format(x$se_p, digits = 4), format(x$detection$AIC, digits = 7)))
  print(x$densite[, c("strate", "n", "k", "rencontre", "D", "se_D", "cv", "bas_D", "haut_D", "ddl")], digits = 5, row.names = FALSE)
  invisible(x)
}

# L'échantillonnage par distances vérifie lui-même ses données (diagnostic.R : verifier_donnees) : les
# transects sans détection ont une distance vide, ce n'est pas un défaut à signaler.
verifier_distance <- function(projet, d) list(variables = data.frame(), observations = data.frame())

# ---------------------------------------------------------------- l'interface

DISTANCE_SYMBOLES_LONGUEUR <- c(m = "m", km = "km", ft = "ft", yd = "yd", mi = "mi", nmi = "nmi")
DISTANCE_SYMBOLES_AIRE <- c(m2 = "m²", ha = "ha", km2 = "km²", acre = "acre", mi2 = "mi²", nmi2 = "nmi²")

# Les unités du modèle en symboles (un facteur donné tel quel : l'aire sans nom).
unites_echantillonnage <- function(m) {
  r <- m$reglages_echantillonnage
  list(distance = DISTANCE_SYMBOLES_LONGUEUR[[r$unites$distance]], effort = if (r$transect == "ligne") DISTANCE_SYMBOLES_LONGUEUR[[r$unites$effort]] else "",
       aire = if (is.null(r$facteur)) DISTANCE_SYMBOLES_AIRE[[r$unites$aire]] else "", facteur = m$facteur, donne = !is.null(r$facteur))
}

# Les lignes du modèle : leur transect, leur strate, leur distance, si elle est dans la troncature,
# la probabilité de détection à cette distance g(x), la taille du groupe ; avec des covariables, g(x)
# sachant les covariables de la détection, et sa probabilité de détection dans la troncature pi.
observations_echantillonnage <- function(m) {
  l <- m$donnees$lignes
  k <- match(l$uid[l$dans], m$donnees$uid)
  g <- rep(NA_real_, nrow(l))
  g[l$dans] <- m$g_x[k]
  out <- data.frame(uid = l$uid, nom = l$nom, transect = l$transect, strate = l$strate, distance = l$distance, dans_troncature = l$dans, g = g, stringsAsFactors = FALSE)
  if (!is.null(m$p_detections)) {
    out$p <- NA_real_
    out$p[l$dans] <- m$p_detections[k]
  }
  if (m$donnees$groupes) out$taille <- l$taille
  if (!nzchar(paste(unique(l$strate), collapse = ""))) out$strate <- NULL
  out
}

# Les covariables d'un modèle, pour l'interface et les tableaux : chacune, son type, et pour une
# qualitative sa catégorie de référence et ses catégories (celles des détections) ; NULL sans
# covariables.
covariables_echantillonnage <- function(m) {
  cv <- m$donnees$covariables
  if (is.null(cv)) return(NULL)
  data.frame(covariable = cv$noms, qualitative = cv$qualitatives,
             reference = vapply(cv$noms, function(n) cv$references[[n]] %||% NA_character_, "", USE.NAMES = FALSE),
             categories = vapply(cv$noms, function(n) { v <- cv$valeurs[[n]]; if (is.factor(v)) paste(levels(droplevels(v)), collapse = ", ") else NA_character_ }, "", USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

# L'histogramme des distances sous la fonction de détection (comme plot.ds de mrds) : √n classes
# égales ; chaque barre, le compte sur celui qu'aurait g = 1 (lignes : Δ n / (p (w − L)) ; points :
# Δ(r²) n / (p w²)) ; et la densité des distances (le compte sur n Δ), pour la courbe f. `t` : les
# données, `p` : la probabilité de détection.
histogramme_echantillonnage <- function(t, p) {
  bords <- bords_classes(list(coupures = t$coupures, L = t$L, w = t$w), t$n)
  comptes <- as.vector(table(cut(t$x, bords, include.lowest = TRUE)))
  Nc <- t$n / p
  g1 <- if (t$point) diff(bords^2) * Nc / t$w^2 else diff(bords) * Nc / (t$w - t$L)
  data.frame(de = bords[-length(bords)], a = bords[-1], compte = comptes, g = comptes / g1, f = comptes / (t$n * diff(bords)))
}

resultat_distance <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  t <- m$donnees
  r <- m$reglages_echantillonnage
  det <- m$detection
  u <- unites_echantillonnage(m)
  tot <- m$densite[nrow(m$densite), ]
  fr <- !identical(getOption("nuee.langue"), "en")
  nb <- function(x, k = 4) formatC(signif(x, k), format = "fg", digits = k, decimal.mark = if (fr) "," else ".")
  list(id = id, projet = p$id, nom = d$name, titre = d$title %||% "", type = "DISTANCE", analyse = TRUE, prediction = FALSE, regression = FALSE,
       opls = FALSE, Ap = 0, A = 0, N = m$N, K = 0, noms_y = I(t$noms$y),
       chiffres = sprintf("%s · D %s%s%s", libelle_detection(det$fd), nb(tot$D), if (nzchar(u$aire)) paste0(" /", u$aire) else "",
                          if (is.finite(tot$cv)) sprintf(" (CV %s)", pourcent_texte(tot$cv, fr)) else ""),
       vues_defaut = I(c("distance_detection", "distance_densite")),
       observations = observations_echantillonnage(m), transect = r$transect, cle = det$fd$cle, serie = det$fd$serie %||% "", ordres = I(det$fd$ordres),
       serie_demandee = r$serie, ajustements = r$ajustements, monotonie = det$monotonie, monotonie_demandee = r$monotonie, contraint = isTRUE(det$contraint),
       echelle_ajustements = r$echelle_ajustements, selection = m$selection, modele_detection = libelle_detection(det$fd),
       candidats = m$candidats, parametres = m$parametres, logL = det$ll, AIC = det$AIC, q = det$q, n = t$n,
       p = m$p, se_p = m$se_p, cv_p = m$se_p / m$p, effectif = m$effectif, Nc = m$Nc,
       troncature = list(w = t$w, L = t$L, type = r$troncature$type, pourcentage = t$pourcentage), au_dela = t$au_dela, en_deca = t$en_deca, vides = t$vides,
       intervalles = if (!is.null(t$coupures)) I(t$coupures),
       courbe = m$courbe, histogramme = histogramme_echantillonnage(t, m$p), distances = I(t$x), uid_distances = I(t$uid), g_distances = I(m$g_x),
       covariables = covariables_echantillonnage(m), formule = det$fd$formule %||% "", p_moyenne = !is.null(m$p_detections), monotone = m$monotone,
       termes_covariables = I(if (!is.null(det$fd$noms_echelle)) det$fd$noms_echelle[-1] else character(0)),
       # (pour chaque covariable qualitative, la fonction de détection moyenne de chacune de ses catégories)
       courbes_niveaux = lapply(m$courbes_niveaux, function(cn) list(covariable = cn$covariable, niveaux = I(cn$niveaux), n = I(cn$n),
                                                                    g = lapply(seq_along(cn$niveaux), function(j) I(cn$g[, j])),
                                                                    f = lapply(seq_along(cn$niveaux), function(j) I(cn$f[, j])))),
       adequation = list(cvm = m$adequation$cvm, khi2 = m$adequation$khi2, qq = m$adequation$qq),
       densite = m$densite, densite_groupes = m$densite_groupes, densite_seule = t$densite_seule, groupes = t$groupes,
       transects = t$transects, unites = u, noms = t$noms, strates = I(t$strates$strate), avec_strates = !is.null(r$strate),
       reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       demande = d$demande %||% list(type = d$type, numero = d$number))
}

# ---------------------------------------------------------------- Exporter

# Les tableaux de l'échantillonnage par distances (formats.R : tables_resultats) : le résumé, les
# modèles candidats et leur AIC, les covariables (leur catégorie de référence), les paramètres, la
# détection (p, ESW ou EDR ; avec des covariables, p̄), l'adéquation et ses classes, la densité et
# l'effectif par strate et au total (des groupes aussi), les transects, les observations.
tables_distance <- function(projet, m) {
  t <- m$donnees
  det <- m$detection
  u <- unites_echantillonnage(m)
  a <- m$adequation
  obs <- observations_echantillonnage(m)
  obs$uid <- NULL
  names(obs)[names(obs) == "nom"] <- "Observation"
  out <- list(
    resume = data.frame(transect = m$reglages_echantillonnage$transect, modele = libelle_detection(det$fd), monotonie = det$monotonie, n = t$n,
                        troncature = t$w, troncature_gauche = t$L, au_dela = t$au_dela, transects = nrow(t$transects), strates = nrow(t$strates),
                        logL = det$ll, AIC = det$AIC, parametres = det$q, unite_distance = u$distance, unite_effort = u$effort, unite_aire = u$aire,
                        facteur_unites = m$facteur, stringsAsFactors = FALSE),
    candidats = m$candidats)
  if (!is.null(det$fd$formule)) {
    out$resume$covariables <- det$fd$formule
    out$covariables <- covariables_echantillonnage(m)
  }
  if (!is.null(m$parametres)) out$parametres <- m$parametres
  out$detection <- data.frame(statistique = c("p", m$effectif$quoi, "N_couvert"), estimation = c(m$p, m$effectif$valeur, m$Nc),
                              se = c(m$se_p, m$effectif$se, NA), cv = c(m$se_p / m$p, m$effectif$se / m$effectif$valeur, NA))
  out$adequation <- rbind(if (!is.null(a$cvm)) data.frame(test = "Cramer-von Mises", statistique = a$cvm$W, ddl = NA, p = a$cvm$p),
                          data.frame(test = "khi2", statistique = a$khi2$chi2, ddl = a$khi2$ddl, p = a$khi2$p))
  out$classes <- a$khi2$classes
  out$densite <- m$densite
  if (!is.null(m$densite_groupes)) out$densite_groupes <- m$densite_groupes
  out$transects <- t$transects
  out$observations <- obs
  out
}

# ---------------------------------------------------------------- texte des méthodes

REFERENCES_DISTANCE <- list(
  buckland2001 = "Buckland, S. T., Anderson, D. R., Burnham, K. P., Laake, J. L., Borchers, D. L., & Thomas, L. (2001). Introduction to distance sampling: Estimating abundance of biological populations. Oxford University Press.",
  miller2019 = "Miller, D. L., Rexstad, E., Thomas, L., Marshall, L., & Laake, J. L. (2019). Distance sampling in R. Journal of Statistical Software, 89(1), 1–28. https://doi.org/10.18637/jss.v089.i01",
  fewster2009 = "Fewster, R. M., Buckland, S. T., Burnham, K. P., Borchers, D. L., Jupp, P. E., Laake, J. L., & Thomas, L. (2009). Estimating the encounter rate variance in distance sampling. Biometrics, 65(1), 225–236. https://doi.org/10.1111/j.1541-0420.2008.01018.x",
  marques2003 = "Marques, F. F. C., & Buckland, S. T. (2003). Incorporating covariates into standard line transect analyses. Biometrics, 59(4), 924–935. https://doi.org/10.1111/j.0006-341X.2003.00107.x",
  marques2007 = "Marques, T. A., Thomas, L., Fancy, S. G., & Buckland, S. T. (2007). Improving estimates of bird density using multiple-covariate distance sampling. The Auk, 124(4), 1229–1243. https://doi.org/10.1642/0004-8038(2007)124[1229:IEOBDU]2.0.CO;2")

# Le nom d'une fonction de détection en mots : « une clé demi-normale avec des termes en cosinus d'ordres 2 et 3 ».
mots_detection <- function(fd, fr) {
  cle <- if (fr) c(hn = "demi-normale", hr = "hasard (hazard-rate)", unif = "uniforme")[[fd$cle]] else c(hn = "half-normal", hr = "hazard-rate", unif = "uniform")[[fd$cle]]
  x <- sprintf(if (fr) "la clé %s" else "the %s key", cle)
  if (!length(fd$ordres)) return(paste(x, if (fr) "seule" else "alone"))
  serie <- if (fr) c(cos = "en cosinus", herm = "en polynômes d'Hermite", poly = "en polynômes simples")[[fd$serie]] else c(cos = "cosine", herm = "Hermite polynomial", poly = "simple polynomial")[[fd$serie]]
  ordres <- liste_mots(as.character(fd$ordres), fr)
  if (fr) sprintf("%s avec %s %s d'ordre%s %s", x, if (length(fd$ordres) > 1) "des termes" else "un terme", serie, if (length(fd$ordres) > 1) "s" else "", ordres)
  else sprintf("%s with %s %s term%s of order%s %s", x, if (length(fd$ordres) > 1) "" else "a", serie, if (length(fd$ordres) > 1) "s" else "", if (length(fd$ordres) > 1) "s" else "", ordres)
}

texte_methodes_distance <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  t <- m$donnees
  r <- m$reglages_echantillonnage
  det <- m$detection
  u <- unites_echantillonnage(m)
  nombre <- function(x, chiffres = 2) nombre_texte(x, chiffres, fr)
  sig <- function(x, k = 3) trimws(formatC(signif(x, k), format = "fg", digits = k, decimal.mark = tx(",", ".")))
  dist <- function(x) paste(sig(x, 4), u$distance)
  valeur_p <- function(p) if (is.na(p)) "—" else if (p < 0.001) tx("p < 0,001", "p < 0.001") else paste("p =", nombre(p, 3))
  cites <- c("buckland2001", "miller2019", "fewster2009")
  point <- t$point
  # les données
  k <- nrow(t$transects)
  effort <- sum(t$transects$effort)
  plan <- if (point) sprintf(tx("des points d'écoute (points transects) : %d points (%s passages)", "point transects: %d points (%s visits)"), k, sig(effort, 6))
          else sprintf(tx("des transects linéaires : %d lignes (effort total %s %s)", "line transects: %d lines (total effort %s %s)"), k, sig(effort, 6), u$effort)
  if (nrow(t$strates) > 1) plan <- paste0(plan, sprintf(tx(", en %d strates (%s)", ", in %d strata (%s)"), nrow(t$strates), liste_mots(t$strates$strate, fr)))
  trunc <- sprintf(tx("jusqu'à la troncature w = %s", "up to the truncation distance w = %s"), dist(t$w))
  if (!is.null(t$pourcentage)) trunc <- paste0(trunc, sprintf(tx(" (les %s %% des plus grandes distances tronquées)", " (the %s%% largest distances truncated)"), sig(t$pourcentage, 3)))
  if (t$L > 0) trunc <- paste0(trunc, sprintf(tx(", et à gauche à %s", ", and on the left at %s"), dist(t$L)))
  if (!is.null(t$coupures)) {
    trunc <- sprintf(tx("relevées par %d intervalles (bornes %s %s)", "recorded in %d intervals (bounds %s %s)"), length(t$coupures) - 1,
                     paste(vapply(t$coupures, sig, "", 4), collapse = tx(" ; ", ", ")), u$distance)
  }
  p1 <- sprintf(tx("Les distances de %s ont été analysées par échantillonnage par distances (distance sampling ; Buckland et al., 2001) depuis %s : %s, dont %d détections %s.",
                   "The distances of %s were analysed by distance sampling (Buckland et al., 2001) from %s: %s, of which %d detections %s."),
                guillemets_texte(t$noms$y, fr), plan, observations_texte(m$N, d, fr), t$n, trunc)
  hors <- t$au_dela + t$en_deca
  if (hors) p1 <- paste(p1, sprintf(tx("%d détections hors de la troncature ont été laissées de côté.", "%d detections outside the truncation were left out."), hors))
  if (t$groupes) p1 <- paste(p1, sprintf(tx("Chaque détection est un groupe (taille : %s).", "Each detection is a group (size: %s)."), guillemets_texte(t$noms$taille, fr)))
  # la fonction de détection
  echelle <- if (identical(r$echelle_ajustements, "echelle")) tx("par σ", "by σ") else tx("par w", "by w")
  mono <- switch(det$monotonie, stricte = tx(", sous la contrainte de monotonie stricte de mrds (g décroissante et positive en 10 distances de 0 à w)",
                                            ", under the strict monotonicity constraint of mrds (g decreasing and positive at 10 distances from 0 to w)"),
                 faible = tx(", sous la contrainte de monotonie faible de mrds (g sous g(0) et positive en 10 distances de 0 à w)",
                             ", under the weak monotonicity constraint of mrds (g below g(0) and positive at 10 distances from 0 to w)"), "")
  covariables <- !is.null(det$fd$formule)
  p2 <- if (covariables) {
    # (l'échelle selon les covariables, ds(formula = ) : ln σ = β0 + β1 OBs(TJS) + …, les catégories de référence)
    cites <- c(cites, "marques2003", "marques2007")
    cv <- t$covariables
    equation <- paste0("ln σ = β0", paste(sprintf(" + β%d %s", seq_len(nrow(cv$termes) - 1), cv$termes$terme[-1]), collapse = ""))
    refs <- cv$noms[cv$qualitatives]
    mots_refs <- if (length(refs)) sprintf(tx(", une covariable qualitative entrant par une indicatrice de chacune de ses catégories sauf la première, sa référence (%s)",
                                              ", a qualitative covariate entering through an indicator of each of its categories but the first, its reference (%s)"),
                                           liste_mots(vapply(refs, function(n) sprintf(tx("%s : %s", "%s: %s"), n, cv$references[[n]]), "", USE.NAMES = FALSE), fr)) else ""
    ajust <- if (length(det$fd$ordres)) tx(" ; sans contrainte de monotonie, que ds ne met pas avec des covariables", "; without monotonicity constraint, which ds doesn't set with covariates")
             else if (identical(m$selection, "covariables")) tx(" ; avec des covariables, ds n'ajoute pas de terme d'ajustement", "; with covariates, ds adds no adjustment term") else ""
    x <- sprintf(tx("La fonction de détection est %s, son échelle σ dépendant de covariables (échantillonnage par distances à covariables multiples ; Marques et Buckland, 2003 ; Marques et al., 2007), comme le fait ds(formula = ~%s) du paquet Distance (Miller et al., 2019) : %s%s%s (AIC = %s).",
                    "The detection function is %s, its scale σ depending on covariates (multiple-covariate distance sampling; Marques and Buckland, 2003; Marques et al., 2007), as ds(formula = ~%s) of the Distance package does (Miller et al., 2019): %s%s%s (AIC = %s)."),
                 mots_detection(sans_covariables(det$fd), fr), det$fd$formule, equation, mots_refs, ajust, nombre(det$AIC, 2))
    sans <- m$candidats[!nzchar(m$candidats$covariables), , drop = FALSE]
    if (nrow(sans)) {
      ecart_aic <- sans$AIC[1] - det$AIC
      x <- paste(x, sprintf(tx("Sans covariables, les mêmes réglages donnent %s, d'un AIC %s de %s.", "Without covariates, the same settings give %s, with an AIC %s by %s."), sans$modele[1],
                            if (ecart_aic >= 0) tx("plus haut", "higher") else tx("plus bas", "lower"), nombre(abs(ecart_aic), 2)))
    }
    if (isFALSE(m$monotone)) x <- paste(x, tx("Pour certaines combinaisons de covariables, la fonction de détection n'est pas monotone (ce que signale check.mono de mrds).",
                                              "For some combinations of covariates, the detection function isn't monotonic (as mrds's check.mono warns)."))
    x
  } else if (identical(r$ajustements, "aic") && r$serie != "aucune") {
    cands <- m$candidats
    liste <- paste(sprintf("%s (AIC = %s)", cands$modele, nombre(cands$AIC, 2)), collapse = tx(", ", ", "))
    sprintf(tx("La fonction de détection a été choisie comme le fait la fonction ds() du paquet Distance (Miller et al., 2019) : la clé %s seule, puis des termes d'ajustement %s (les distances divisées %s) ajoutés un à un tant que l'AIC baissait (cinq au plus)%s. Modèles ajustés : %s ; retenu : %s (AIC = %s).",
               "The detection function was chosen as the ds() function of the Distance package does (Miller et al., 2019): the %s key alone, then %s adjustment terms (the distances divided %s) added one at a time while the AIC decreased (five at most)%s. Models fitted: %s; retained: %s (AIC = %s)."),
            tx(c(hn = "demi-normale", hr = "hasard", unif = "uniforme")[[r$cle]], c(hn = "half-normal", hr = "hazard-rate", unif = "uniform")[[r$cle]]),
            tx(c(cos = "en cosinus", herm = "en polynômes d'Hermite", poly = "en polynômes simples")[[r$serie]], c(cos = "cosine", herm = "Hermite polynomial", poly = "simple polynomial")[[r$serie]]),
            echelle, mono, liste, mots_detection(det$fd, fr), nombre(det$AIC, 2))
  } else sprintf(tx("La fonction de détection est %s%s (AIC = %s ; Miller et al., 2019).", "The detection function is %s%s (AIC = %s; Miller et al., 2019)."),
                 mots_detection(det$fd, fr), if (length(det$fd$ordres)) mono else "", nombre(det$AIC, 2))
  eff <- if (point) sprintf(tx("un rayon effectif de détection de %s (erreur-type %s)", "an effective detection radius of %s (standard error %s)"), dist(m$effectif$valeur), dist(m$effectif$se))
         else sprintf(tx("une demi-largeur effective de bande de %s (erreur-type %s)", "an effective strip half-width of %s (standard error %s)"), dist(m$effectif$valeur), dist(m$effectif$se))
  p2 <- paste(p2, if (covariables) sprintf(tx("La probabilité moyenne de détection dans la troncature, p̄ = n / Σ 1/pi (pi celle de chaque détection, sachant ses covariables, comme summary.ds de mrds), est de %s (erreur-type %s, CV %s), soit %s.",
                                              "The average probability of detection within the truncation, p̄ = n / Σ 1/pi (pi that of each detection, given its covariates, as mrds's summary.ds), is %s (standard error %s, CV %s), that is %s."),
                                           nombre(m$p, 3), nombre(m$se_p, 3), pourcent_texte(m$se_p / m$p, fr), eff)
                  else sprintf(tx("La probabilité moyenne de détection dans la troncature est p = %s (erreur-type %s, CV %s), soit %s.",
                                  "The average probability of detection within the truncation is p = %s (standard error %s, CV %s), that is %s."),
                               nombre(m$p, 3), nombre(m$se_p, 3), pourcent_texte(m$se_p / m$p, fr), eff))
  a <- m$adequation
  khi2 <- sprintf(tx("khi² = %s sur %d classes (%s)", "chi-square = %s on %d classes (%s)"), nombre(a$khi2$chi2, 2), nrow(a$khi2$classes),
                  if (is.na(a$khi2$ddl)) tx("sans degré de liberté", "no degree of freedom") else sprintf(tx("%d ddl, %s", "%d df, %s"), as.integer(a$khi2$ddl), valeur_p(a$khi2$p)))
  adeq <- if (covariables) tx("Adéquation, chaque distance rapportée à la fonction de détection de ses covariables (les comptes attendus des classes sommés sur les détections)",
                              "Goodness of fit, each distance against the detection function of its covariates (the classes' expected counts summed over the detections)")
          else tx("Adéquation", "Goodness of fit")
  p2 <- paste(p2, if (is.null(a$cvm)) sprintf(tx("%s : %s.", "%s: %s."), adeq, khi2)
                  else sprintf(tx("%s : Cramér–von Mises W = %s (%s) ; %s.", "%s: Cramér–von Mises W = %s (%s); %s."), adeq, nombre(a$cvm$W, 3), valeur_p(a$cvm$p), khi2))
  # la densité
  dens <- m$densite
  tot <- dens[nrow(dens), ]
  aire <- if (nzchar(u$aire)) paste0(" ", tx("par", "per"), " ", u$aire) else tx(" par unité d'aire", " per unit of area")
  couverte <- if (point) tx("π w² K, K les passages", "π w² K, K the visits") else if (t$L > 0) tx("2 (w − L) L, L l'effort", "2 (w − L) L, L the effort") else tx("2 w L, L l'effort", "2 w L, L the effort")
  p3 <- if (covariables) {
    sprintf(tx("La densité de chaque strate est celle de type Horvitz–Thompson de dht2, D = Σ %s / a, la somme sur ses détections, a l'aire couverte (%s) ; sa variance réunit celle du taux de rencontre entre les transects (estimateur %s de Fewster et al., 2009), celle de la fonction de détection (méthode delta sur ses paramètres, d'après la somme des produits des scores des distances)%s ; l'intervalle de confiance à 95 %% est log-normal, avec les degrés de liberté de Satterthwaite (Buckland et al., 2001).",
               "The density of each stratum is dht2's Horvitz–Thompson-like estimator, D = Σ %s / a, the sum over its detections, a the covered area (%s); its variance combines that of the encounter rate between transects (estimator %s of Fewster et al., 2009), that of the detection function (delta method on its parameters, from the outer product of the distances' scores)%s; the 95%% confidence interval is log-normal, with Satterthwaite degrees of freedom (Buckland et al., 2001)."),
            if (t$groupes) tx("si / pi (si la taille du groupe)", "si / pi (si the group size)") else "1 / pi", couverte, if (point) "P2" else "R2",
            if (t$groupes) tx(" et celle de la taille moyenne des groupes", " and that of the mean group size") else "")
  } else sprintf(tx("La densité de chaque strate est D = n / (a p), a l'aire couverte (%s), comme le fait dht2 ; sa variance réunit celle du taux de rencontre entre les transects (estimateur %s de Fewster et al., 2009), celle de p (méthode delta, d'après la somme des produits des scores des distances)%s ; l'intervalle de confiance à 95 %% est log-normal, avec les degrés de liberté de Satterthwaite (Buckland et al., 2001).",
                    "The density of each stratum is D = n / (a p), a the covered area (%s), as dht2 computes it; its variance combines that of the encounter rate between transects (estimator %s of Fewster et al., 2009), that of p (delta method, from the outer product of the distances' scores)%s; the 95%% confidence interval is log-normal, with Satterthwaite degrees of freedom (Buckland et al., 2001)."),
                 couverte, if (point) "P2" else "R2", if (t$groupes) tx(" et celle de la taille moyenne des groupes", " and that of the mean group size") else "")
  quoi <- if (nrow(dens) > 1) tx("Au total", "Overall") else tx("La densité", "The density")
  p3 <- paste(p3, sprintf(tx("%s : D = %s%s (erreur-type %s, CV %s ; IC à 95 %% %s–%s ; %s ddl)%s.", "%s: D = %s%s (standard error %s, CV %s; 95%% CI %s–%s; %s df)%s."),
                          quoi, sig(tot$D, 4), aire, sig(tot$se_D, 3), pourcent_texte(tot$cv, fr), sig(tot$bas_D, 4), sig(tot$haut_D, 4), nombre(tot$ddl, 1),
                          if (!t$densite_seule) sprintf(tx(", soit un effectif N = %s (IC à 95 %% %s–%s) sur %s %s", ", that is an abundance N = %s (95%% CI %s–%s) over %s %s"),
                                                        sig(tot$N, 4), sig(tot$bas_N, 4), sig(tot$haut_N, 4), sig(tot$aire, 6), u$aire) else ""))
  if (t$groupes) {
    tg <- m$densite_groupes[nrow(m$densite_groupes), ]
    p3 <- paste(p3, sprintf(tx("Ces valeurs comptent les individus ; les groupes : D = %s%s (CV %s), de taille moyenne %s.", "These values count individuals; groups: D = %s%s (CV %s), of mean size %s."),
                            sig(tg$D, 4), aire, pourcent_texte(tg$cv, fr), nombre(tot$D / tg$D, 2)))
  }
  # (sans aire de strate : la variance de la taille des groupes et les degrés de liberté du total avec
  # les poids des strates, et ce que donne dht2 quand cela diffère, densite_echantillonnage)
  if (t$densite_seule && nrow(dens) > 1) {
    moyenne <- tx("Sans aire de strate, la densité totale est la moyenne des strates pondérée par leurs aires couvertes",
                  "Without stratum areas, the overall density is the strata's mean weighted by their covered areas")
    p3 <- paste(p3, if (is.finite(tot$se_D_dht2)) {
      sprintf(tx("%s : les variances du taux de rencontre et de la taille des groupes de chaque strate y entrent avec le carré de son poids, et ses %s degrés de liberté sont ceux de la formule de Satterthwaite avec ces poids. dht2 laisse les poids hors de la variance de la taille des groupes et du dénominateur des degrés de liberté : il donne une erreur-type de %s et %s degrés de liberté, d'où un intervalle plus large.",
                 "%s: each stratum's encounter rate and group size variances enter it with the square of its weight, and its %s degrees of freedom are those of Satterthwaite's formula with these weights. dht2 leaves the weights out of the group size variance and of the degrees of freedom's denominator: it gives a standard error of %s and %s degrees of freedom, hence a wider interval."),
              moyenne, nombre(tot$ddl, 1), sig(tot$se_D_dht2, 3), nombre(if (is.finite(tot$ddl_dht2)) tot$ddl_dht2 else tot$ddl, 1))
    } else if (is.finite(tot$ddl_dht2)) {
      sprintf(tx("%s, et ses %s degrés de liberté sont ceux de la formule de Satterthwaite avec ces poids ; dht2 les laisse hors du dénominateur et n'en donne que %s, d'où un intervalle plus large.",
                 "%s, and its %s degrees of freedom are those of Satterthwaite's formula with these weights; dht2 leaves them out of the denominator and gives only %s, hence a wider interval."),
              moyenne, nombre(tot$ddl, 1), nombre(tot$ddl_dht2, 1))
    } else paste0(moyenne, "."))
  }
  if (any(dens$k[!is.na(dens$strate)] == 1)) p3 <- paste(p3, tx("Une strate d'un seul transect n'a pas de variance du taux de rencontre.", "A stratum with a single transect has no encounter rate variance."))
  if (!nzchar(u$aire)) p3 <- paste(p3, sprintf(tx("Les distances ont été converties par le facteur %s.", "The distances were converted by the factor %s."), format(u$facteur, digits = 6)))
  cites <- union(cites, c("r", "nuee"))
  annee_r <- R.version$year
  annee_nuee <- format(Sys.Date(), "%Y")
  logiciel <- sprintf(tx("Les calculs ont été faits avec Nuée%s (Boutin, %s) et R %s (R Core Team, %s), d'après la méthode publiée (Buckland et al., 2001) et les fonctions ds() et dht2() du paquet Distance (Miller et al., 2019).",
                         "Computations were done with Nuée%s (Boutin, %s) and R %s (R Core Team, %s), from the published method (Buckland et al., 2001) and the ds() and dht2() functions of the Distance package (Miller et al., 2019)."),
                      if (nzchar(version)) paste0(" ", version) else "", annee_nuee, paste(R.version$major, R.version$minor, sep = "."), annee_r)
  references <- vapply(cites, function(c) switch(c,
    r = sprintf(REFERENCES$r, annee_r),
    nuee = sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
    REFERENCES_DISTANCE[[c]]), "")
  list(paragraphes = c(p1, p2, p3, logiciel), references = unname(sort(references)))
}

# ---------------------------------------------------------------- texte des résultats

# Le texte des résultats d'un échantillonnage par distances (resultats_texte.R : texte_resultats) : la
# fonction de détection retenue et son AIC (avec des covariables, l'écart à celle sans covariables, et
# le coefficient de chaque covariable sur ln σ, avec son facteur exp(β) sur σ), la probabilité moyenne
# de détection et ESW ou EDR, l'adéquation, puis la densité et l'effectif au total et par strate.
texte_resultats_distance <- function(projet, d, m, langue = "fr") {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  nb <- function(x, k = 3) vapply(x, nombre_resultat, "", fr, k, USE.NAMES = FALSE)
  t <- m$donnees
  det <- m$detection
  u <- unites_echantillonnage(m)
  par_aire <- if (nzchar(u$aire)) sprintf(tx(" par %s", " per %s"), u$aire) else tx(" par unité d'aire", " per unit of area")
  p1 <- sprintf(tx("Le modèle de %s porte sur %d détections jusqu'à %s %s, depuis %d %s : la fonction de détection %s (AIC = %s).",
                   "The model of %s uses %d detections up to %s %s, from %d %s: the detection function %s (AIC = %s)."),
                guillemets_resultat(t$noms$y, fr), t$n, nb(t$w, 4), u$distance, nrow(t$transects),
                if (t$point) tx("points", "points") else tx("lignes", "lines"), libelle_detection(det$fd), nb(det$AIC, 6))
  if (!is.null(det$fd$formule)) {
    sans <- m$candidats[!nzchar(m$candidats$covariables), , drop = FALSE]
    if (nrow(sans)) {
      ecart_aic <- sans$AIC[1] - det$AIC
      p1 <- paste(p1, sprintf(tx("Sans covariables (%s), l'AIC serait %s de %s.", "Without covariates (%s), the AIC would be %s by %s."), sans$modele[1],
                              if (ecart_aic >= 0) tx("plus haut", "higher") else tx("plus bas", "lower"), nb(abs(ecart_aic), 3)))
    }
    pa <- m$parametres[m$parametres$parametre %in% det$fd$noms_echelle[-1], , drop = FALSE]
    quantitative <- pa$parametre %in% t$covariables$termes$terme[is.na(t$covariables$termes$niveau)]
    p1 <- paste(p1, sprintf(tx("Coefficients des covariables sur ln σ : %s.", "Coefficients of the covariates on ln σ: %s."),
                            paste(sprintf(tx("%s β = %s (erreur-type %s ; σ × %s%s)", "%s β = %s (standard error %s; σ × %s%s)"), pa$parametre, nb(pa$estimation), nb(pa$se),
                                          nb(exp(pa$estimation)), ifelse(quantitative, tx(" par unité", " per unit"), "")), collapse = tx(", ", ", "))))
    refs <- t$covariables$noms[t$covariables$qualitatives]
    if (length(refs)) p1 <- paste(p1, sprintf(tx("Catégories de référence : %s.", "Reference categories: %s."),
                                              paste(vapply(refs, function(n) sprintf(tx("%s : %s", "%s: %s"), n, t$covariables$references[[n]]), "", USE.NAMES = FALSE), collapse = tx(", ", ", "))))
  }
  a <- m$adequation
  p2 <- sprintf(tx("Probabilité moyenne de détection %s = %s (CV %s), %s = %s %s. Adéquation : %skhi² %s.",
                   "Average probability of detection %s = %s (CV %s), %s = %s %s. Goodness of fit: %schi-square %s."),
                if (!is.null(m$p_detections)) "p̄" else "p", nb(m$p), pourcent_texte(m$se_p / m$p, fr), m$effectif$quoi, nb(m$effectif$valeur), u$distance,
                if (!is.null(a$cvm)) sprintf(tx("Cramér–von Mises %s, ", "Cramér–von Mises %s, "), p_resultat(a$cvm$p, fr)) else "", p_resultat(a$khi2$p, fr))
  dens <- m$densite
  tot <- dens[nrow(dens), ]
  p3 <- sprintf(tx("%s : D = %s%s (IC à 95 %% %s – %s ; CV %s)%s.", "%s: D = %s%s (95%% CI %s – %s; CV %s)%s."),
                if (nrow(dens) > 1) tx("Densité totale", "Overall density") else tx("Densité", "Density"), nb(tot$D, 4), par_aire, nb(tot$bas_D, 4), nb(tot$haut_D, 4),
                pourcent_texte(tot$cv, fr),
                if (!t$densite_seule) sprintf(tx(", soit un effectif N = %s (IC à 95 %% %s – %s)", ", that is an abundance N = %s (95%% CI %s – %s)"), nb(tot$N, 4), nb(tot$bas_N, 4), nb(tot$haut_N, 4)) else "")
  if (nrow(dens) > 1) {
    st <- dens[!is.na(dens$strate), , drop = FALSE]
    p3 <- paste(p3, sprintf(tx("Par strate : %s.", "By stratum: %s."),
                            paste(sprintf("%s D = %s (CV %s)", st$strate, nb(st$D, 4), vapply(st$cv, function(v) if (is.finite(v)) pourcent_texte(v, fr) else "—", "")),
                                  collapse = tx(" ; ", "; "))))
  }
  list(paragraphes = c(p1, p2, p3))
}
