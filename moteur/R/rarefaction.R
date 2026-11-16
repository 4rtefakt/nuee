# Raréfaction et accumulation des espèces (Graphiques › Raréfaction ; serveur.R : la demande
# `rarefaction`), sur des colonnes d'espèces du projet (des effectifs : une ligne par observation),
# comme le paquet vegan les calcule (rarefy, rarecurve, specaccum, estimateR, specpool) ; vegan n'est
# pas livré avec Nuée : moteur/tests/verifier_rarefaction.R le compare quand il est installé.
#
#   La raréfaction de chaque observation (Hurlbert 1971) : la richesse attendue dans m individus
#   tirés sans remise parmi ses J, E(S_m) = Σ_i [1 - C(J - x_i, m) / C(J, m)], et sa variance
#   (Heck, van Belle et Simberloff 1975), comme vegan::rarefy(se = TRUE) ; la courbe pour m de 1 à J
#   (par pas, comme rarecurve), et la richesse de chaque observation raréfiée à une même taille (la
#   plus petite des observations par défaut). Elle demande des effectifs entiers.
#   L'accumulation des espèces sur les observations, exacte (Ugland, Gray et Ellingsen 2003 ;
#   Kindt : vegan::specaccum(method = "exact")) : la richesse attendue dans k observations tirées
#   parmi n, Σ_i [1 - C(n - f_i, k) / C(n, k)], f_i le nombre d'observations où l'espèce i est, et
#   son écart-type (celui de vegan, conditionné aux espèces vues : les variances de chaque espèce et
#   leurs covariances, d'après les corrélations entre les présences des espèces).
#   Les estimateurs de la richesse : pour chaque observation, Chao1 corrigé du biais et ACE
#   (vegan::estimateR ; effectifs entiers) ; sur les observations, Chao2, jackknife d'ordre 1 et 2
#   et bootstrap (vegan::specpool, d'après les présences seules). Avec des groupes (une variable
#   qualitative ou un identifiant), une courbe d'accumulation et des estimateurs par groupe.

PAS_RAREFACTION <- 100  # (une courbe de raréfaction : au plus une centaine de points, par défaut)

# ---------------------------------------------------------------- les calculs

# La richesse raréfiée d'une observation (ses effectifs x) à chaque taille `tailles` (au plus J,
# son effectif total), et son écart-type : celui de vegan::rarefy(se = TRUE), la variance des
# indicatrices d'absence de chaque espèce et de leurs covariances, les paires d'espèces regroupées
# par leurs effectifs (deux espèces de mêmes effectifs ont la même covariance).
rarefaction_hurlbert <- function(x, tailles, se = TRUE) {
  x <- x[x > 0]
  J <- sum(x)
  absente <- function(reste, m, l) ifelse(reste < m, 0, exp(lchoose(reste, m) - l))
  effectifs <- as.numeric(names(table(x)))
  nombres <- as.vector(table(x))
  paires <- outer(nombres, nombres)
  diag(paires) <- nombres * (nombres - 1) / 2
  haut <- upper.tri(paires, diag = TRUE)
  t(vapply(tailles, function(m) {
    l <- lchoose(J, m)
    q <- absente(J - x, m, l)
    S <- sum(1 - q)
    if (!se) return(c(S, NA_real_))
    qv <- absente(J - effectifs, m, l)
    q2 <- absente(J - outer(effectifs, effectifs, "+"), m, l)
    V <- sum(q * (1 - q)) + 2 * sum((paires * (q2 - outer(qv, qv)))[haut])
    c(S, sqrt(max(V, 0)))
  }, numeric(2)))
}

# L'accumulation exacte des espèces sur les n lignes d'un tableau de présences (logique), et son
# écart-type (vegan::specaccum(method = "exact"), conditionné) ; 1 à n observations.
accumulation_exacte <- function(P) {
  P <- P[, colSums(P) > 0, drop = FALSE]
  n <- nrow(P)
  if (!ncol(P)) return(list(sites = seq_len(n), richesse = rep(0, n), et = rep(0, n)))
  f <- colSums(P)
  absente <- matrix(vapply(seq_len(n), function(k) ifelse(n - f < k, 0, exp(lchoose(n - f, k) - lchoose(n, k))), numeric(length(f))), ncol = n)
  absente <- t(absente)  # (une ligne par nombre d'observations, une colonne par espèce)
  V <- absente * (1 - absente)
  co <- if (ncol(P) > 1) suppressWarnings(stats::cor(P)) else matrix(0, 1, 1)
  co[is.na(co)] <- 0
  diag(co) <- 0
  E <- sqrt(V)
  list(sites = seq_len(n), richesse = rowSums(1 - absente), et = sqrt(pmax(rowSums(V) + rowSums((E %*% co) * E), 0)))
}

# Chao1 corrigé du biais et ACE d'une observation (ses effectifs entiers x), comme vegan::estimateR :
#   Chao1 = S + F1 (F1 - 1) / (2 (F2 + 1)), F1 et F2 les espèces vues 1 et 2 fois ; sa variance, celle
#   de vegan (Chao 1987 pour la forme corrigée du biais, moins F1² (F1 + F2)² / (4 (F2 + 1)⁴ Chao1)) ;
#   ACE (Chao et Lee 1992) : les espèces rares (10 individus au plus), leur couverture C = 1 - F1 / N_rare,
#   leur coefficient de variation γ² ; ACE = S_abondantes + S_rares / C + max(γ², 0) F1 / C. Son
#   écart-type par la méthode delta (la covariance des F_i : F_i (δ_ij - F_j / ACE)) avec le gradient
#   d'ACE lui-même (vegan::estimateR en prend un autre : écart-type différent, voir les vérifications).
#   Sans espèce rare, ACE = S ; quand toutes les rares sont vues une fois (C = 0), ACE n'existe pas.
estimateurs_abondance <- function(x) {
  x <- round(x[x > 0])
  S <- length(x)
  if (!S) return(c(S_obs = 0, chao1 = 0, se_chao1 = 0, ace = 0, se_ace = 0))
  Fq <- tabulate(x, 10)
  f1 <- Fq[1]
  f2 <- Fq[2]
  chao1 <- S + f1 * (f1 - 1) / (2 * (f2 + 1))
  v1 <- f1 * (f1 - 1) / (2 * (f2 + 1)) + f1 * (2 * f1 - 1)^2 / (4 * (f2 + 1)^2) + f1^2 * f2 * (f1 - 1)^2 / (4 * (f2 + 1)^4) -
    f1^2 * (f1 + f2)^2 / (4 * (f2 + 1)^4 * chao1)
  i <- 1:10
  rares <- x[x <= 10]
  S_rares <- length(rares)
  N <- sum(rares)
  ace <- se_ace <- NA_real_
  if (!S_rares) {
    ace <- S
    se_ace <- 0
  } else if (f1 < N && N > 1) {
    C <- 1 - f1 / N
    somme <- sum(i * (i - 1) * Fq)
    D <- C * N * (N - 1)
    g2 <- S_rares * somme / D - 1
    ace <- (S - S_rares) + S_rares / C + max(g2, 0) * f1 / C
    # le gradient d'ACE par rapport à F1..F10
    dC <- (f1 * i - (i == 1) * N) / N^2
    dD <- dC * N * (N - 1) + C * i * (2 * N - 1)
    dg2 <- ((somme + S_rares * i * (i - 1)) * D - S_rares * somme * dD) / D^2
    grad <- 1 / C - S_rares * dC / C^2
    if (g2 > 0) grad <- grad + dg2 * f1 / C + g2 * ((i == 1) * C - f1 * dC) / C^2
    se_ace <- sqrt(max(sum(grad %o% grad * (diag(Fq) - Fq %o% Fq / ace)), 0))
  }
  c(S_obs = S, chao1 = chao1, se_chao1 = sqrt(max(v1, 0)), ace = ace, se_ace = se_ace)
}

# Chao2, jackknife d'ordre 1 et 2 et bootstrap sur les n lignes d'un tableau de présences
# (logique), comme vegan::specpool (avec sa correction des petits échantillons, (n - 1) / n) ; leurs
# écarts-types (Chao2, jackknife 1, bootstrap).
estimateurs_incidence <- function(P) {
  n <- nrow(P)
  f <- colSums(P)
  S <- sum(f > 0)
  if (!n || !S) return(c(n = n, S = S, chao = NA, se_chao = NA, jack1 = NA, se_jack1 = NA, jack2 = NA, boot = NA, se_boot = NA))
  k <- (n - 1) / n
  q1 <- sum(f == 1)
  q2 <- if (n >= 2) sum(f == 2) else 0
  chao <- S + if (q2 > 0) k * q1^2 / (2 * q2) else k * q1 * (q1 - 1) / 2
  v_chao <- if (q2 > 0) { r <- q1 / q2; q2 * (k * r^2 / 2 + k^2 * (r^3 + r^4 / 4)) } else k * (k * (q1 * (2 * q1 - 1)^2 / 4 - q1^4 / (4 * chao)) + q1 * (q1 - 1) / 2)
  jack1 <- S + q1 * (n - 1) / n
  uniques <- rowSums(P[, f == 1, drop = FALSE])  # (les espèces vues une seule fois, dans chaque observation)
  v_jack1 <- if (q1 > 0) (sum(uniques^2) - q1 / n) * (n - 1) / n else 0
  jack2 <- if (n > 1) S + q1 * (2 * n - 3) / n - q2 * (n - 2)^2 / (n * (n - 1)) else S
  p <- f[f > 0] / n
  pn <- (1 - p)^n
  absentes <- !P[, f > 0, drop = FALSE]
  Z <- (crossprod(absentes) / n)^n - outer(pn, pn)
  v_boot <- sum(pn * (1 - pn)) + 2 * sum(Z[lower.tri(Z)])
  c(n = n, S = S, chao = chao, se_chao = sqrt(max(v_chao, 0)), jack1 = jack1, se_jack1 = sqrt(max(v_jack1, 0)), jack2 = jack2,
    boot = S + sum(pn), se_boot = sqrt(max(v_boot, 0)))
}

# Des effectifs entiers (à 1e-8 près, comme vegan les reconnaît) ?
effectifs_entiers <- function(X) all(abs(X - round(X)) < 1e-8)

# ---------------------------------------------------------------- la demande

# Les espèces `especes` (numéros de variables quantitatives, dans l'ordre du tableau) des
# observations du projet ; `groupe` (une variable qualitative ou "id:k") pour des courbes
# d'accumulation et des estimateurs par groupe ; `taille`, la taille commune de la raréfaction (la
# plus petite des observations par défaut) ; `pas`, celui des courbes de raréfaction. Les
# observations où une espèce manque sont laissées de côté (comptées) ; celles sans aucun individu
# n'ont pas de courbe de raréfaction, mais comptent dans l'accumulation (une observation sans espèce).
rarefaction_projet <- function(projet, especes, groupe = NULL, taille = NULL, pas = NULL) {
  err <- function(fr, en) stop(traduire(fr, en), call. = FALSE)
  uids <- unique(as.numeric(unlist(especes)))
  if (length(uids) < 2) err("Choisir au moins deux espèces (des colonnes d'effectifs).", "Choose at least two species (columns of counts).")
  obs <- numeros_observations(projet)
  termes <- unlist(lapply(uids, function(u) termes_variable(projet, u, "none")), recursive = FALSE)
  X <- reponses_ordination(projet, termes, obs, traduire("Les espèces de la raréfaction", "The species of the rarefaction"))
  if (any(X < 0, na.rm = TRUE)) err("La raréfaction demande des effectifs positifs ou nuls.", "Rarefaction needs counts that are positive or zero.")
  g <- if (length(groupe) && nzchar(as.character(groupe)[1])) valeurs_a_tester(projet, as.character(groupe)[1], obs)
  if (!is.null(g) && !g$qualitative) err("Les groupes se font par une variable qualitative ou un identifiant.", "Groups are made by a qualitative variable or an ID.")
  complet <- stats::complete.cases(X)
  manquantes <- sum(!complet)
  sans_groupe <- if (!is.null(g)) sum(complet & is.na(g$valeurs)) else 0
  garde <- complet & (if (is.null(g)) TRUE else !is.na(g$valeurs))
  if (sum(garde) < 2) err("Trop peu d'observations ont toutes leurs valeurs.", "Too few observations have all their values.")
  X <- X[garde, , drop = FALSE]
  uid_obs <- obs[garde]
  noms <- unname(projet$noms_obs[nom_numero(uid_obs)])
  grp <- if (!is.null(g)) droplevels(g$valeurs[garde])
  J <- rowSums(X)
  S <- rowSums(X > 0)
  entiers <- effectifs_entiers(X)
  vides <- sum(J <= 0)
  pleines <- J > 0
  taille_min <- if (any(pleines)) min(J[pleines]) else 0
  taille <- suppressWarnings(as.numeric(taille %||% NA)[1])
  if (!is.finite(taille) || taille < 1) taille <- taille_min
  taille <- round(taille)
  pas <- suppressWarnings(as.integer(pas %||% NA)[1])
  if (is.na(pas) || pas < 1) pas <- max(1L, as.integer(ceiling(max(J) / PAS_RAREFACTION)))
  observations <- data.frame(uid = uid_obs, nom = noms, groupe = if (is.null(grp)) NA_character_ else as.character(grp), N = J, S = S,
                             Sm = NA_real_, se = NA_real_, chao1 = NA_real_, se_chao1 = NA_real_, ace = NA_real_, se_ace = NA_real_, stringsAsFactors = FALSE)
  courbes <- NULL
  if (entiers) {
    X <- round(X)
    for (i in which(pleines)) {
      x <- X[i, ]
      if (J[i] >= taille) observations[i, c("Sm", "se")] <- rarefaction_hurlbert(x, taille)
      observations[i, c("chao1", "se_chao1", "ace", "se_ace")] <- estimateurs_abondance(x)[c("chao1", "se_chao1", "ace", "se_ace")]
    }
    courbes <- do.call(rbind, lapply(which(pleines), function(i) {
      m <- unique(c(seq(1, J[i], by = pas), J[i]))
      r <- rarefaction_hurlbert(X[i, ], m)
      data.frame(uid = uid_obs[i], taille = m, S = r[, 1], se = r[, 2])
    }))
  }
  parties <- if (is.null(grp)) list(tout = rep(TRUE, nrow(X))) else stats::setNames(lapply(levels(grp), function(k) grp == k), levels(grp))
  accumulation <- do.call(rbind, lapply(names(parties), function(k) {
    a <- accumulation_exacte(X[parties[[k]], , drop = FALSE] > 0)
    data.frame(groupe = if (is.null(grp)) NA_character_ else k, sites = a$sites, richesse = a$richesse, et = a$et, stringsAsFactors = FALSE)
  }))
  estimateurs <- do.call(rbind, lapply(names(parties), function(k) {
    e <- estimateurs_incidence(X[parties[[k]], , drop = FALSE] > 0)
    cbind(data.frame(groupe = if (is.null(grp)) NA_character_ else k, stringsAsFactors = FALSE), as.data.frame(as.list(e)))
  }))
  list(especes = I(colnames(X)), n = nrow(X), entiers = entiers, taille = taille, taille_min = taille_min, pas = pas,
       manquantes = manquantes, sans_groupe = sans_groupe, vides = vides, petites = sum(pleines & J < taille),
       groupe = if (!is.null(g)) list(uid = g$uid, nom = g$nom), niveaux = I(if (is.null(grp)) character(0) else levels(grp)),
       observations = observations, courbes = courbes, accumulation = accumulation, estimateurs = estimateurs)
}
