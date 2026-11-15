# TRIM (TRends and Indices for Monitoring data) : les indices et les tendances de comptages
# d'animaux suivis sur des sites année après année, avec des comptages manquants, comme le logiciel
# TRIM 3 de Statistics Netherlands (Pannekoek et van Strien, 2005) et son successeur, le paquet
# rtrim (Bogaart, van der Loo et Pannekoek ; ses méthodes : Pannekoek, Bogaart et van der Loo,
# 2018), écrit ici d'après la méthode publiée. rtrim n'est pas livré avec Nuée :
# moteur/tests/verifier_trim.R le compare quand il est installé (NUEE_RTRIM).
#
# Les données : le tableau long du projet, une ligne par site et par temps ; le site est un
# identifiant ou une variable, le temps (l'année) une variable quantitative ou un identifiant fait de
# nombres, régulièrement espacés (un temps sans aucune ligne est ajouté, tous ses comptages
# manquants) ; Y (rôle Y) : les comptages, entiers positifs ou nuls, manquants au besoin : c'est le
# but de TRIM, ils sont imputés. Les sites sans aucun comptage positif sont laissés de côté. Le
# modèle est log-linéaire, avec un effet par site (αi) :
#   modèle 1 : ln μij = αi (aucun effet du temps) ;
#   modèle 2 : ln μij = αi + Σl βl xjl, une tendance linéaire sur l'échelle log, de pente βl entre
#              les points de changement kl et kl+1 (xjl : le nombre de pas de temps du morceau l
#              passés au temps j ; avant k1, pas de pente) ; un seul point, au premier temps : une
#              seule pente ;
#   modèle 3 : ln μij = αi + γj (un effet par temps, γ1 = 0).
# Des covariables qualitatives (rôle X ; une valeur par site, ou par site et par temps) ajoutent à
# chaque paramètre du temps l'effet de leurs catégories, la première de chacune étant la
# référence ; des poids wij (une variable) font le modèle des comptages pondérés,
# ln wij μij = αi + ... (rtrim § 2.6).
# Les paramètres sont estimés par les équations d'estimation généralisées (GEE ; Liang et Zeger,
# 1986), avec var(fij) = σ² μij (la surdispersion) et cor(fij, fik) = ρ^|j−k| entre deux temps d'un
# même site (la corrélation sérielle, d'un temps au suivant), chacune au choix ; sans elles, c'est le
# maximum de vraisemblance de Poisson, qui est aussi le point de départ. σ² et ρ viennent des résidus
# de Pearson (rtrim § 3.3). Les effets des sites sont concentrés : pour β donné, chaque αi résout
# l'équation de son site, et seule la matrice d'information des paramètres du temps est inversée
# (rtrim § 3.4).
#
# Les résultats (rtrim § 2.5, 2.8, 5) : les paramètres et leurs erreurs-types ; les totaux de chaque
# temps, du modèle (Σi wij μij), imputés (les comptages observés, et μ pour les manquants) et
# observés, et leurs erreurs-types (la covariance des totaux du modèle par la méthode delta ; celle
# des totaux imputés, rtrim (81)) ; les indices (le total d'un temps sur celui du temps de base) et
# leurs erreurs-types (méthode delta) ; la pente globale, celle de la droite des moindres carrés à
# travers le log des totaux (imputés, la « overall slope imputed » de TRIM, et du modèle), son erreur-
# type d'après la covariance des totaux, son intervalle à 95 % (loi t à J − 2 degrés de liberté,
# comme TRIM et rtrim) et sa classe (TRIM 3 : l'intervalle de la pente multiplicative contre 1, 0,95
# et 1,05) ; l'ajustement (khi² de Pearson, rapport de vraisemblance, AIC = LR − 2 ddl) et les tests
# de Wald (la pente, les changements de pente, les écarts à la droite du modèle 3, les covariables).
# Le modèle 2 peut choisir ses points de changement pas à pas (rtrim § 4.1 : un point retiré quand
# son test de Wald dépasse p = 0,2, remis quand son test du score passe sous p = 0,15).
# C'est une analyse de analyses.R (ajuster_trim, resultat_trim, texte_methodes_trim, tables_trim) ;
# elle ne prédit pas de nouvelles observations.

TRIM_TEMPS_MAX <- 500       # (des temps au-delà : sans doute pas des années)
TRIM_TOLERANCE <- 1e-10     # (convergence : plus grand changement de β, σ² et ρ)
TRIM_ITERATIONS <- 1000
TRIM_PARAMETRE_MAX <- 50    # (|β| au-delà : le modèle ne s'estime pas, comme rtrim)
TRIM_SORTIE <- 0.2          # (pas à pas : p de Wald au-delà duquel un point de changement sort ; rtrim premove)
TRIM_ENTREE <- 0.15         # (p du test du score sous lequel il revient ; rtrim penter)
TRIM_CLASSES <- c("forte_hausse", "hausse_moderee", "stable", "incertaine", "declin_modere", "fort_declin", "inconnue")

# ---------------------------------------------------------------- réglages et données

# Les réglages de TRIM (d$options), complétés : le site et le temps (list(by = "id", id = ...) ou
# list(by = "variable", variable = ...)), le modèle (1, 2, 3), ses points de changement (des temps,
# ou "tous" : chaque temps sauf le dernier), la sélection pas à pas, la surdispersion et la
# corrélation sérielle (estimées par défaut, comme le recommande le manuel de TRIM), la variable des
# poids et le temps de base des indices (le premier par défaut).
reglages_trim <- function(d) {
  o <- d$options %||% list()
  spec <- function(x) if (length(x$by)) specs_groupement(x)[[1]]
  oui <- function(x, defaut) {
    v <- suppressWarnings(as.logical(unlist(x)))
    if (length(v) && !is.na(v[1])) v[1] else defaut
  }
  nombre <- function(x) {
    v <- suppressWarnings(as.numeric(unlist(x)))
    if (length(v) && is.finite(v[1])) v[1]
  }
  modele <- suppressWarnings(as.integer(unlist(o$modele %||% 3))[1])
  if (is.na(modele) || !modele %in% 1:3) modele <- 3L
  cp <- unlist(o$changements %||% numeric(0))
  tous <- is.character(cp) && length(cp) > 0 && identical(cp[1], "tous")
  cp <- if (tous) numeric(0) else suppressWarnings(as.numeric(cp))
  list(site = spec(o$site), temps = spec(o$temps), modele = modele, changements = sort(unique(cp[is.finite(cp)])), tous = tous,
       pas_a_pas = oui(o$pas_a_pas, FALSE), surdispersion = oui(o$surdispersion, TRUE), correlation = oui(o$correlation, TRUE),
       poids = nombre(o$poids), base = nombre(o$base))
}

# Une valeur manquante d'une ligne d'un site (un temps sans ligne, ou une ligne sans valeur) prend
# celle de son temps précédent qui en a une, sinon celle du suivant.
prolonger <- function(v) {
  connus <- which(!is.na(v))
  if (!length(connus) || length(connus) == length(v)) return(v)
  k <- findInterval(seq_along(v), connus)
  v[connus[pmax(k, 1)]]
}

# Les données de TRIM (`r` : reglages_trim) : la matrice sites × temps des comptages f (NA :
# manquant), des poids w, de la catégorie de chaque covariable (1 : sa référence), du numéro de
# l'observation de chaque case ; les sites, les temps (la grille régulière), ce qui a été laissé de
# côté. Les refus nomment l'observation, le site et le temps en cause.
donnees_trim <- function(projet, d, r) {
  err <- function(fr, en, ...) stop(sprintf(traduire(fr, en), ...), call. = FALSE)
  if (is.null(r$site)) err("TRIM demande le site de chaque comptage (onglet Général).", "TRIM needs the site of each count (General tab).")
  if (is.null(r$temps)) err("TRIM demande le temps de chaque comptage, l'année (onglet Général).", "TRIM needs the time of each count, the year (General tab).")
  if (identical(r$site, r$temps)) err("Le site et le temps doivent être deux colonnes différentes.", "The site and the time must be two different columns.")
  if (length(d$spectres)) err("TRIM ne prend pas de prétraitement de spectres.", "TRIM takes no spectral preprocessing.")
  uy <- unique(vapply(d$y, function(t) as.numeric(t$variables[1]), 0))
  if (length(uy) != 1 || any(vapply(d$y, function(t) length(t$variables) != 1, TRUE))) err("TRIM demande une seule variable Y : les comptages.", "TRIM needs a single Y variable: the counts.")
  vy <- variable_du_projet(projet, uy)
  nom_y <- as.character(vy$ids[1])
  if (length(vy$categories)) err("Les comptages (Y) doivent être une variable quantitative : « %s » est qualitative.", "The counts (Y) must be a quantitative variable: “%s” is qualitative.", nom_y)
  if (!is.null(d$y[[1]]$transform)) err("TRIM prend les comptages tels qu'ils sont : retirez la transformation de « %s ».", "TRIM takes the counts as they are: remove the transformation of “%s”.", nom_y)
  if (any(vapply(d$x, function(t) length(t$variables) > 1, TRUE))) {
    err("TRIM ne prend pas de terme croisé : chaque covariable agit seule sur les effets du temps.", "TRIM takes no cross term: each covariate acts alone on the time effects.")
  }
  ucov <- unique(vapply(d$x, function(t) as.numeric(t$variables), 0))
  vcov <- lapply(ucov, function(u) variable_du_projet(projet, u))
  for (v in vcov) {
    if (!length(v$categories)) err("Les covariables de TRIM (X) sont des variables qualitatives : « %s » ne l'est pas.", "TRIM's covariates (X) are qualitative variables: “%s” isn't one.", v$ids[1])
  }
  obs <- as.numeric(d$observations)
  noms_obs <- unname(projet$noms_obs[as.character(obs)])
  nom_site <- nom_groupement(projet, r$site)
  nom_temps <- nom_groupement(projet, r$temps)
  s <- valeurs_groupement(projet, r$site, obs)
  site <- if (is.numeric(s)) ifelse(is.na(s), NA_character_, sprintf("%.15g", s)) else as.character(s)
  tt <- temps_en_nombres(valeurs_groupement(projet, r$temps, obs))
  if (!is.null(attr(tt, "refus"))) stop(refus_temps(tt, nom_temps), call. = FALSE)
  if (isTRUE(attr(tt, "dates"))) {
    err("Le temps de TRIM (%s) est fait de nombres régulièrement espacés, l'année : pas de dates.", "TRIM's time (%s) is made of evenly spaced numbers, the year: not dates.", nom_temps)
  }
  temps <- as.vector(tt)
  y <- colonne(projet, vy$uid, obs)
  connu <- !is.na(site) & !is.na(temps)
  faux <- which(connu & !is.na(y) & (!is.finite(y) | y < 0 | abs(y - round(y)) > 1e-8))
  if (length(faux)) {
    err("Les comptages (%s) sont des entiers positifs ou nuls : %s vaut %s.", "The counts (%s) are whole numbers, positive or zero: %s is %s.", nom_y, noms_obs[faux[1]], format(y[faux[1]], digits = 15))
  }
  y <- round(y)
  cle <- ifelse(connu, paste(site, sprintf("%.15g", temps), sep = "\r"), NA_character_)
  double <- which(duplicated(cle, incomparables = NA))
  if (length(double)) {
    k <- double[1]
    a <- which(connu & cle == cle[k])[1]
    err("Deux lignes pour le même site et le même temps : %s %s en %s %s (%s et %s).", "Two rows for the same site and time: %s %s at %s %s (%s and %s).",
        nom_site, site[k], nom_temps, format(temps[k], digits = 15), noms_obs[a], noms_obs[k])
  }
  # les poids : connus et positifs là où un comptage l'est
  nom_poids <- NULL
  w <- rep(1, length(obs))
  if (!is.null(r$poids)) {
    vw <- variable_du_projet(projet, r$poids)
    nom_poids <- as.character(vw$ids[1])
    if (length(vw$categories)) err("Les poids (%s) doivent être une variable quantitative.", "The weights (%s) must be a quantitative variable.", nom_poids)
    w <- colonne(projet, r$poids, obs)
    k <- which(connu & !is.na(y) & is.na(w))
    if (length(k)) err("%s n'a pas de poids (%s) : un comptage connu en demande un.", "%s has no weight (%s): a known count needs one.", noms_obs[k[1]], nom_poids)
    k <- which(connu & !is.na(w) & !(w > 0))
    if (length(k)) err("Les poids (%s) doivent être positifs : %s vaut %s.", "The weights (%s) must be positive: %s is %s.", nom_poids, noms_obs[k[1]], format(w[k[1]], digits = 15))
  }
  zs <- lapply(vcov, function(v) {
    z <- colonne(projet, v$uid, obs)
    k <- which(connu & !is.na(y) & is.na(z))
    if (length(k)) err("%s n'a pas de valeur de %s, une covariable : un comptage connu en demande une.", "%s has no value of %s, a covariate: a known count needs one.", noms_obs[k[1]], v$ids[1])
    z
  })
  # les sites, dans l'ordre des catégories, des nombres ou des noms ; ceux sans comptage positif sortent
  niveaux <- unique(site[connu])
  niveaux <- if (is.numeric(s)) niveaux[order(as.numeric(niveaux))]
             else if (identical(r$site$by, "variable") && length(variable_du_projet(projet, r$site$variable)$categories)) intersect(as.character(variable_du_projet(projet, r$site$variable)$categories), niveaux)
             else ordre_cles(niveaux)
  positif <- vapply(niveaux, function(x) any(connu & site == x & !is.na(y) & y > 0), TRUE)
  vides <- niveaux[!positif]
  niveaux <- niveaux[positif]
  if (!length(niveaux)) err("Aucun site n'a de comptage positif de %s.", "No site has a positive count of %s.", nom_y)
  garde <- connu & site %in% niveaux
  # les temps : une grille régulière, du premier au dernier
  tv <- sort(unique(temps[garde]))
  if (length(tv) < 2) err("TRIM demande au moins deux temps (%s n'en a qu'un).", "TRIM needs at least two times (%s has only one).", nom_temps)
  pas <- min(diff(tv))
  q <- (tv - tv[1]) / pas
  hors <- which(abs(q - round(q)) > 1e-6)
  if (length(hors)) {
    err("Les temps de TRIM (%s) sont régulièrement espacés : %s ne tombe pas sur le pas de %s depuis %s.", "TRIM's times (%s) are evenly spaced: %s isn't on the step of %s from %s.",
        nom_temps, format(tv[hors[1]], digits = 15), format(pas, digits = 15), format(tv[1], digits = 15))
  }
  J <- round(q[length(q)]) + 1
  if (J > TRIM_TEMPS_MAX) {
    err("Trop de temps de %s à %s (%d pas de %s) : TRIM en prend %d au plus. %s est-il bien l'année ?", "Too many times from %s to %s (%d steps of %s): TRIM takes %d at most. Is %s really the year?",
        format(tv[1], digits = 15), format(tv[length(tv)], digits = 15), J, format(pas, digits = 15), TRIM_TEMPS_MAX, nom_temps)
  }
  grille <- tv[1] + pas * (seq_len(J) - 1)
  I <- length(niveaux)
  ij <- cbind(match(site[garde], niveaux), round((temps[garde] - tv[1]) / pas) + 1)
  case <- function(v) { M <- matrix(NA_real_, I, J); M[ij] <- v[garde]; M }
  f <- case(y)
  remplir <- function(M) { R <- t(apply(M, 1, prolonger)); dim(R) <- dim(M); R }
  W <- remplir(case(w))
  # les covariables : la catégorie de chaque case ; une seule catégorie parmi les sites gardés : laissée de côté
  covariables <- list()
  cov <- list()
  retirees <- character(0)
  for (k in seq_along(vcov)) {
    Z <- remplir(case(zs[[k]]))
    codes <- sort(unique(as.vector(Z[!is.na(Z)])))
    nom <- as.character(vcov[[k]]$ids[1])
    if (length(codes) < 2) {
      retirees <- c(retirees, nom)
      next
    }
    cov[[length(cov) + 1]] <- matrix(match(Z, codes), I, J)
    covariables[[length(covariables) + 1]] <- list(nom = nom, uid = vcov[[k]]$uid, categories = as.character(vcov[[k]]$categories)[codes])
  }
  list(f = f, w = W, ponderee = !is.null(r$poids), cov = cov, covariables = covariables, covariables_retirees = retirees,
       sites = niveaux, temps = grille, pas = pas, ajoutes = grille[!seq_len(J) %in% (round(q) + 1)], uid = case(obs),
       observations = obs[garde], exclues = obs[!garde], sans_site_temps = sum(!connu), sites_vides = vides, lignes_vides = sum(connu & site %in% vides),
       noms = list(y = nom_y, site = nom_site, temps = nom_temps, poids = nom_poids))
}

# ---------------------------------------------------------------- les modèles

# La matrice du temps (J × p0) d'un modèle : rien (modèle 1), les morceaux des points de
# changement `cp` (des rangs de temps, modèle 2), une colonne par temps après le premier (modèle 3) ;
# avec des covariables, la matrice de chaque site (une liste), les colonnes de chaque catégorie après
# la référence ajoutées à celles de la matrice du temps, nulles aux temps où le site est d'une autre
# catégorie. `noms` : chaque paramètre ; `bloc` : son bloc (0 : la référence).
dessins_trim <- function(t, modele, cp = integer(0)) {
  J <- length(t$temps)
  B0 <- if (modele == 1) matrix(0, J, 0)
        else if (modele == 3) diag(J)[, -1, drop = FALSE]
        else {
          fins <- c(cp[-1], J)
          matrix(vapply(seq_along(cp), function(l) pmin(pmax(seq_len(J) - cp[l], 0), fins[l] - cp[l]), numeric(J)), J, length(cp))
        }
  p0 <- ncol(B0)
  blocs <- list(list(covariable = NA_character_, categorie = NA_character_))
  for (cv in t$covariables) for (k in cv$categories[-1]) blocs[[length(blocs) + 1]] <- list(covariable = cv$nom, categorie = k)
  Bs <- NULL
  if (length(t$cov) && p0 > 0) {
    Bs <- lapply(seq_along(t$sites), function(i) {
      cbind(B0, do.call(cbind, lapply(t$cov, function(Z) {
        K <- max(Z, na.rm = TRUE)
        do.call(cbind, lapply(seq_len(K)[-1], function(k) B0 * (Z[i, ] == k)))
      })))
    })
  }
  list(B0 = B0, Bs = Bs, p0 = p0, p = p0 * length(blocs), blocs = blocs, modele = modele, cp = cp)
}

# La matrice du temps d'un site.
dessin_site <- function(des, i) if (is.null(des$Bs)) des$B0 else des$Bs[[i]]

# Les équations du modèle pour α, β, σ² et ρ donnés (rtrim § 3.2–3.4) : μ (toutes les cases), et
# pour chaque site, ses cases observées o, Ωi = diag(μo) Vi⁻¹ diag(μo) avec Vi = σ² A½ Ri A½,
# di = 1ᵀΩi1 et Fi = (1ᵀΩi Bo) / di ; le score des paramètres du temps U = Σ Boᵀ diag(μo) Vi⁻¹ (fo − μo)
# et leur information, les effets des sites concentrés, I* = Σ Boᵀ (Ωi − Ωi11ᵀΩi / di) Bo.
systeme_trim <- function(t, des, alpha, beta, sig2, rho) {
  I <- length(t$sites)
  J <- length(t$temps)
  p <- des$p
  Rg <- if (rho != 0) rho^abs(outer(seq_len(J), seq_len(J), "-")) else NULL
  mu <- matrix(0, I, J)
  U <- numeric(p)
  Is <- matrix(0, p, p)
  d <- numeric(I)
  F <- matrix(0, I, p)
  for (i in seq_len(I)) {
    B <- dessin_site(des, i)
    mu[i, ] <- exp(alpha[i] + (if (p) drop(B %*% beta) else 0) - log(t$w[i, ]))
    o <- which(!is.na(t$f[i, ]))
    m <- mu[i, o]
    s <- sqrt(m)
    r <- (t$f[i, o] - m) / s
    if (is.null(Rg)) {
      w <- m / sig2
      Ur <- s * r / sig2
    } else {
      Rinv <- chol2inv(chol(Rg[o, o, drop = FALSE]))
      w <- s * drop(Rinv %*% s) / sig2
      Ur <- s * drop(Rinv %*% r) / sig2
    }
    d[i] <- sum(w)
    if (p) {
      Bo <- B[o, , drop = FALSE]
      bw <- drop(crossprod(Bo, w))
      F[i, ] <- bw / d[i]
      U <- U + drop(crossprod(Bo, Ur))
      Om <- if (is.null(Rg)) Bo * (m / sig2) else (outer(s, s) * Rinv / sig2) %*% Bo
      Is <- Is + crossprod(Bo, Om) - outer(bw, bw) / d[i]
    }
  }
  list(mu = mu, U = U, I = Is, d = d, F = F)
}

# Les effets des sites pour β (et ρ) donnés : chacun résout l'équation de son site,
# zᵀ (fo − μo) = 0 avec z = Vi⁻¹ μo, soit αi = ln(zᵀ fo) − ln(zᵀ eo), eo = exp(Bo β − ln wo) ; z ne
# dépend pas de αi (à un facteur près, eo^−½ ⊙ Ri⁻¹ eo^½), et vaut 1 sans corrélation sérielle (le
# maximum de vraisemblance : αi = ln Σ fo − ln Σ eo). Si zᵀ fo n'est pas positif (une corrélation
# forte et un seul comptage positif), le site prend l'équation sans corrélation, comme rtrim.
alphas_trim <- function(t, des, beta, rho) {
  I <- length(t$sites)
  J <- length(t$temps)
  Rg <- if (rho != 0) rho^abs(outer(seq_len(J), seq_len(J), "-")) else NULL
  vapply(seq_len(I), function(i) {
    o <- which(!is.na(t$f[i, ]))
    B <- dessin_site(des, i)
    e <- exp((if (des$p) drop(B[o, , drop = FALSE] %*% beta) else 0) - log(t$w[i, o]))
    fo <- t$f[i, o]
    if (!is.null(Rg)) {
      z <- drop(chol2inv(chol(Rg[o, o, drop = FALSE])) %*% sqrt(e)) / sqrt(e)
      if (sum(z * fo) > 0 && sum(z * e) > 0) return(log(sum(z * fo)) - log(sum(z * e)))
    }
    log(sum(fo)) - log(sum(e))
  }, 0)
}

# Le rapport de vraisemblance (rtrim (25)) : 2 Σ f ln(f / μ) sur les comptages observés positifs.
rapport_vraisemblance <- function(f, mu) {
  k <- !is.na(f) & f > 0
  2 * sum(f[k] * log(f[k] / mu[k]))
}

# σ² et ρ d'après les résidus de Pearson rij = (fij − μij) / √μij des comptages observés (rtrim
# (38) et (39)) : σ² = Σ r² / (O − p), p les paramètres (sites et temps) ; ρ = Σ rij ri,j+1 / (N σ²) sur
# les N paires de temps successifs observés d'un même site.
dispersion_trim <- function(f, mu, p, surdispersion, correlation) {
  r <- (f - mu) / sqrt(mu)
  O <- sum(!is.na(f))
  sig2 <- if (surdispersion) { ddl <- O - p; if (ddl > 0) sum(r^2, na.rm = TRUE) / ddl else 1 } else 1
  rho <- 0
  if (correlation) {
    J <- ncol(f)
    produits <- r[, -J, drop = FALSE] * r[, -1, drop = FALSE]
    n <- sum(!is.na(produits))
    rho <- if (n > 0) sum(produits, na.rm = TRUE) / (n * sig2) else 0
  }
  list(sig2 = sig2, rho = rho)
}

# Le nom d'un paramètre du temps, pour les messages (le temps d'un modèle 3, le point de changement
# d'un modèle 2).
nom_parametre_trim <- function(t, des, k) {
  k0 <- (k - 1) %% des$p0 + 1
  if (des$modele == 3) format(t$temps[k0 + 1], digits = 15) else format(t$temps[des$cp[k0]], digits = 15)
}

# L'ajustement : le maximum de vraisemblance de Poisson par la méthode du score (Fisher), les effets
# des sites concentrés, un pas divisé par deux tant que le rapport de vraisemblance augmente ; puis,
# avec la surdispersion ou la corrélation sérielle, les GEE : à chaque tour, α pour β, σ² et ρ des
# résidus, un pas de score pour β ; jusqu'à ce que β, σ² et ρ ne bougent plus (TRIM_TOLERANCE).
estimer_trim <- function(t, des, surdispersion, correlation) {
  I <- length(t$sites)
  p <- des$p
  O <- sum(!is.na(t$f))
  if (O <= I + p) {
    stop(sprintf(traduire("Trop peu de comptages (%d) pour les %d paramètres du modèle (%d sites et %d du temps).", "Too few counts (%d) for the model's %d parameters (%d sites and %d for the time)."),
                 O, I + p, I, p), call. = FALSE)
  }
  beta <- numeric(p)
  sig2 <- 1
  rho <- 0
  gee <- FALSE
  avec_gee <- surdispersion || correlation
  converge <- FALSE
  trop <- function(beta) {
    k <- which(!is.finite(beta) | abs(beta) > TRIM_PARAMETRE_MAX)
    if (length(k)) {
      stop(sprintf(traduire("Le modèle ne s'estime pas : le paramètre du temps %s s'en va à l'infini (trop peu de comptages positifs autour de lui ?).",
                            "The model can't be estimated: the time parameter %s goes to infinity (too few positive counts around it?)."), nom_parametre_trim(t, des, k[1])), call. = FALSE)
    }
  }
  for (iter in seq_len(TRIM_ITERATIONS)) {
    alpha <- alphas_trim(t, des, beta, rho)
    s <- systeme_trim(t, des, alpha, beta, sig2, rho)
    if (gee) {
      ds <- dispersion_trim(t$f, s$mu, I + p, surdispersion, correlation)
      if (correlation && abs(ds$rho) >= 1) {
        stop(sprintf(traduire("La corrélation sérielle estimée (ρ = %s) sort de ]−1 ; 1[ : le modèle ne lui convient pas ; essayez sans elle.",
                              "The estimated serial correlation (ρ = %s) is outside (−1, 1): the model doesn't suit it; try without it."), format(signif(ds$rho, 3))), call. = FALSE)
      }
      changement <- max(abs(ds$sig2 - sig2), abs(ds$rho - rho))
      if (ds$rho != rho || ds$sig2 != sig2) {
        sig2 <- ds$sig2
        rho <- ds$rho
        if (ds$rho != 0) alpha <- alphas_trim(t, des, beta, rho)
        s <- systeme_trim(t, des, alpha, beta, sig2, rho)
      }
    } else changement <- 0
    db <- if (p) solve(s$I, s$U) else numeric(0)
    if (p) {
      pas <- 1
      lr0 <- if (!gee) rapport_vraisemblance(t$f, s$mu)
      repeat {
        nouveau <- beta + pas * db
        ok <- all(is.finite(nouveau)) && all(abs(nouveau) <= 2 * TRIM_PARAMETRE_MAX)
        if (ok && !gee && pas > 1 / 128) {
          a <- alphas_trim(t, des, nouveau, rho)
          mu <- t(vapply(seq_along(t$sites), function(i) exp(a[i] + drop(dessin_site(des, i) %*% nouveau) - log(t$w[i, ])), numeric(length(t$temps))))
          ok <- is.finite(rapport_vraisemblance(t$f, mu)) && rapport_vraisemblance(t$f, mu) <= lr0 * (1 + 1e-12) + 1e-12
        }
        if (ok || pas <= 1 / 128) break
        pas <- pas / 2
      }
      beta <- nouveau
      trop(beta)
    }
    if (max(abs(pas_db <- if (p) pas * db else 0), changement) < TRIM_TOLERANCE) {
      if (avec_gee && !gee) {
        gee <- TRUE
        next
      }
      converge <- TRUE
      break
    }
  }
  if (!converge) {
    stop(sprintf(traduire("L'ajustement ne converge pas après %d tours : essayez un modèle plus simple, ou sans la corrélation sérielle.",
                          "The fit doesn't converge after %d rounds: try a simpler model, or without the serial correlation."), TRIM_ITERATIONS), call. = FALSE)
  }
  alpha <- alphas_trim(t, des, beta, rho)
  s <- systeme_trim(t, des, alpha, beta, sig2, rho)
  V <- if (p) solve(s$I) else matrix(0, 0, 0)
  list(alpha = alpha, beta = beta, V = V, sig2 = sig2, rho = rho, mu = s$mu, d = s$d, F = s$F, I = s$I, U = s$U, iterations = iter,
       methode = if (avec_gee) "GEE" else "ML", des = des, surdispersion = surdispersion, correlation = correlation)
}

# ---------------------------------------------------------------- l'estimabilité

# Les sites à un seul comptage positif n'apprennent rien des paramètres du temps (rtrim :
# check_model3, check_model2) : les cases positives des autres.
cases_utiles <- function(f) {
  positif <- !is.na(f) & f > 0
  positif[rowSums(positif) < 2, ] <- FALSE
  positif
}

# Ce qu'un modèle demande des données, comme rtrim le vérifie avant d'ajuster : pas de blocs
# indépendants (un temps j que ne relie aucun site compté avant lui et à partir de lui) ; pour le
# modèle 3, un comptage positif d'un site qui en a au moins deux à chaque temps (dans chaque catégorie
# de chaque covariable). Les refus nomment le temps et la catégorie.
verifier_estimable_trim <- function(t, modele) {
  if (modele == 1) return(invisible(TRUE))
  f <- t$f
  J <- ncol(f)
  utile <- !is.na(f) & f > 0
  for (j in seq_len(J)[-1]) {
    if (!any(utile[, j])) next
    if (!any(rowSums(utile[, seq_len(j - 1), drop = FALSE]) > 0 & rowSums(utile[, j:J, drop = FALSE]) > 0)) {
      stop(sprintf(traduire("Les données forment deux blocs indépendants à partir de %s : aucun site n'a de comptage positif avant et après ce temps.",
                            "The data form two independent blocks from %s on: no site has a positive count both before and after this time."), format(t$temps[j], digits = 15)), call. = FALSE)
    }
  }
  if (modele != 3) return(invisible(TRUE))
  positif <- cases_utiles(f)
  for (j in seq_len(J)) {
    if (!any(positif[, j])) {
      stop(sprintf(traduire("Le modèle 3 demande à chaque temps un comptage positif d'un site qui en a au moins deux : %s n'en a pas. Essayez le modèle 2.",
                            "Model 3 needs at each time a positive count of a site that has at least two: %s has none. Try model 2."), format(t$temps[j], digits = 15)), call. = FALSE)
    }
  }
  for (k in seq_along(t$cov)) {
    Z <- t$cov[[k]]
    cv <- t$covariables[[k]]
    for (c in seq_along(cv$categories)) for (j in seq_len(J)) {
      if (!any(positif[, j] & Z[, j] == c)) {
        stop(sprintf(traduire("Le modèle 3 demande à chaque temps un comptage positif dans chaque catégorie de %s : %s n'en a pas en %s.",
                              "Model 3 needs at each time a positive count in each category of %s: %s has none at %s."),
                     cv$nom, cv$categories[c], format(t$temps[j], digits = 15)), call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

# Les points de changement d'un modèle 2 dont le morceau (du temps qui suit le point jusqu'au point
# suivant, ou au dernier temps) n'a aucun comptage positif utile (d'un site qui en a deux, ou de chaque
# catégorie d'une covariable) sont retirés un à un comme rtrim (autodelete) : le dernier morceau vide
# (le deuxième si c'est le premier). Rend les points gardés et ceux retirés (des rangs de temps).
changements_estimables <- function(t, cp) {
  f <- t$f
  J <- ncol(f)
  retires <- integer(0)
  vides <- function(cp, positif) {
    fins <- c(cp[-1], J)
    which(vapply(seq_along(cp), function(l) sum(positif[, (cp[l] + 1):fins[l]]) < 1, TRUE))
  }
  repeat {
    v <- vides(cp, cases_utiles(f))
    if (!length(v)) for (k in seq_along(t$cov)) {
      for (c in seq_len(max(t$cov[[k]]))) {
        v <- vides(cp, !is.na(f) & f > 0 & t$cov[[k]] == c)
        if (length(v)) break
      }
      if (length(v)) break
    }
    if (!length(v)) break
    if (length(cp) == 1) {
      stop(sprintf(traduire("Aucun comptage positif après le point de changement %s : la pente ne s'estime pas.", "No positive count after the changepoint %s: the slope can't be estimated."),
                   format(t$temps[cp], digits = 15)), call. = FALSE)
    }
    k <- v[length(v)]
    if (k == 1) k <- 2
    retires <- c(retires, cp[k])
    cp <- cp[-k]
  }
  list(cp = cp, retires = sort(retires))
}

# ---------------------------------------------------------------- les totaux, les indices, la pente

# La covariance des totaux du modèle de chaque temps (rtrim (72)) : G diag(d)⁻¹ Gᵀ + (GF − H) I*⁻¹ (GF − H)ᵀ,
# Gji = wij μij, Hjk = Σi wij μij (Bi)jk ; `observees` : les cases observées seulement (pour celle des
# totaux imputés).
variance_totaux_modele <- function(t, fit, observees = FALSE) {
  G <- t(t$w * fit$mu)
  if (observees) G[t(is.na(t$f))] <- 0
  V <- G %*% (t(G) / fit$d)
  if (fit$des$p) {
    H <- Reduce(`+`, lapply(seq_along(t$sites), function(i) G[, i] * dessin_site(fit$des, i)))
    GFH <- G %*% fit$F - H
    V <- V + GFH %*% fit$V %*% t(GFH)
  }
  V
}

# Les totaux de chaque temps : observés (Σ wij fij sur les comptages observés), du modèle
# (Σi wij μij) et imputés (fij observé, μij sinon) ; la covariance des totaux imputés (rtrim (81)) :
# celle du modèle, moins sa part des cases observées, plus la variance des comptages observés,
# σ² wij² μij, corrélés ρ^|j−k| dans un site.
totaux_trim <- function(t, fit) {
  f <- t$f
  obs <- !is.na(f)
  wm <- t$w * fit$mu
  Vm <- variance_totaux_modele(t, fit)
  J <- length(t$temps)
  Rg <- if (fit$rho != 0) fit$rho^abs(outer(seq_len(J), seq_len(J), "-")) else diag(J)
  Vf <- matrix(0, J, J)
  for (i in seq_along(t$sites)) {
    s <- ifelse(obs[i, ], sqrt(t$w[i, ]^2 * fit$mu[i, ]), 0)
    Vf <- Vf + fit$sig2 * outer(s, s) * Rg
  }
  Vi <- Vm - variance_totaux_modele(t, fit, observees = TRUE) + Vf
  list(observe = colSums(ifelse(obs, t$w * f, 0)), modele = colSums(wm), impute = colSums(ifelse(obs, t$w * f, wm)), V_modele = Vm, V_impute = Vi)
}

# Les indices : le total de chaque temps sur celui du temps de base b, leur erreur-type par la méthode
# delta (rtrim (89)) ; 0 au temps de base.
indices_trim <- function(tt, V, b) {
  tau <- tt / tt[b]
  var_tau <- vapply(seq_along(tt), function(j) {
    if (j == b) return(0)
    g <- c(-tt[j] / tt[b]^2, 1 / tt[b])
    drop(g %*% V[c(b, j), c(b, j)] %*% g)
  }, 0)
  list(indice = tau, se = sqrt(pmax(var_tau, 0)))
}

# La classe d'une pente multiplicative d'après son intervalle de confiance (TRIM 3 ; rtrim :
# overall) : forte hausse si sa borne basse dépasse 1,05, hausse modérée si elle dépasse 1 ; fort
# déclin si sa borne haute est sous 0,95, déclin modéré si elle est sous 1 ; sinon stable quand
# l'intervalle est dans ]0,95 ; 1,05[, incertaine autrement. La classe vient de l'intervalle à 95 % ;
# `niveau` dit si celui à 99 % la donne aussi (p < 0,01) ou seulement celui à 95 % (p < 0,05).
classe_pente <- function(b, se, ddl) {
  if (!is.finite(b) || !is.finite(se) || ddl <= 0) return(list(classe = "inconnue", niveau = NA_real_))
  bornes <- function(niveau) exp(b + c(-1, 1) * stats::qt(1 - (1 - niveau) / 2, ddl) * se)
  q95 <- bornes(0.95)
  q99 <- bornes(0.99)
  eps <- 1e-7
  classe <- if (q95[1] > 1.05) "forte_hausse" else if (q95[2] < 0.95) "fort_declin" else if (q95[1] > 1 + eps) "hausse_moderee"
            else if (q95[2] < 1 - eps) "declin_modere" else if (q95[1] > 0.95 && q95[2] < 1.05) "stable" else "incertaine"
  niveau <- switch(classe,
    forte_hausse = if (q99[1] > 1.05) 0.01 else 0.05, fort_declin = if (q99[2] < 0.95) 0.01 else 0.05,
    hausse_moderee = if (q99[1] > 1 + eps) 0.01 else 0.05, declin_modere = if (q99[2] < 1 - eps) 0.01 else 0.05, NA_real_)
  list(classe = classe, niveau = niveau)
}

# La pente globale (rtrim (15), (92) ; overall) : la droite des moindres carrés à travers ln tj
# contre le rang du temps (1 … J), son erreur-type d'après la covariance de ln t (diag(1/t) V diag(1/t)),
# p par la loi t à J − 2 degrés de liberté, l'intervalle à 95 % de la pente multiplicative, sa classe.
pente_globale <- function(tt, V) {
  J <- length(tt)
  vide <- data.frame(additive = NA_real_, se = NA_real_, multiplicative = NA_real_, se_multiplicative = NA_real_, p = NA_real_,
                     bas = NA_real_, haut = NA_real_, classe = "inconnue", niveau = NA_real_, stringsAsFactors = FALSE)
  if (J < 3 || any(!(tt > 0))) return(vide)
  X <- cbind(1, seq_len(J))
  XtXi <- solve(crossprod(X))
  P <- XtXi %*% t(X)
  b <- drop(P %*% log(tt))
  Vy <- V / outer(tt, tt)
  se <- sqrt(max(0, (P %*% Vy %*% t(P))[2, 2]))
  ddl <- J - 2
  q <- stats::qt(0.975, ddl)
  cl <- classe_pente(b[2], se, ddl)
  data.frame(additive = b[2], se = se, multiplicative = exp(b[2]), se_multiplicative = exp(b[2]) * se,
             p = if (se > 0) 2 * stats::pt(abs(b[2] / se), ddl, lower.tail = FALSE) else NA_real_,
             bas = exp(b[2] - q * se), haut = exp(b[2] + q * se), classe = cl$classe, niveau = cl$niveau, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------- les tests

# Les tests de Wald (rtrim § 2.8.2 ; wald) : W = θᵀ var(θ)⁻¹ θ, χ² à rang(var θ) degrés de liberté.
#   modèle 2, un seul point au premier temps, sans covariable : la pente ;
#   modèle 2 sinon : à chaque point, le changement de pente (βl − βl−1, β0 = 0), de la référence et de
#   chaque catégorie des covariables ensemble ;
#   modèle 3 sans covariable : les écarts à la droite (γ* de la décomposition de rtrim § 4.2, J − 2
#   degrés de liberté ; aucun pour deux temps) ;
#   chaque covariable : tous ses paramètres.
tests_trim <- function(t, fit, devs = NULL) {
  des <- fit$des
  beta <- fit$beta
  V <- fit$V
  wald <- function(theta, Vt) {
    W <- drop(t(theta) %*% solve(Vt, theta))
    ddl <- length(theta)
    c(W = W, ddl = ddl, p = stats::pchisq(W, ddl, lower.tail = FALSE))
  }
  lignes <- list()
  ajouter <- function(test, quoi, x) lignes[[length(lignes) + 1]] <<- data.frame(test = test, quoi = quoi, W = x[["W"]], ddl = x[["ddl"]], p = x[["p"]], stringsAsFactors = FALSE)
  nb <- length(des$blocs)
  if (des$modele == 2 && des$p0) {
    if (des$p0 == 1 && des$cp[1] == 1 && nb == 1) {
      ajouter("pente", format(t$temps[1], digits = 15), wald(beta, V))
    } else {
      A <- diag(des$p0)
      A[row(A) == col(A) + 1] <- -1
      Ab <- kronecker(diag(nb), A)
      db <- drop(Ab %*% beta)
      Vd <- Ab %*% V %*% t(Ab)
      for (l in seq_len(des$p0)) {
        k <- l + des$p0 * (seq_len(nb) - 1)
        ajouter("changement", format(t$temps[des$cp[l]], digits = 15), wald(db[k], Vd[k, k, drop = FALSE]))
      }
    }
  }
  J <- length(t$temps)
  if (!is.null(devs) && J > 2) {
    k <- 3:J
    ajouter("deviations", "", wald(devs$gstar[k], devs$V[k + 1, k + 1, drop = FALSE]))
  }
  premier <- des$p0
  for (cv in t$covariables) {
    k <- premier + seq_len(des$p0 * (length(cv$categories) - 1))
    if (des$p0) ajouter("covariable", cv$nom, wald(beta[k], V[k, k, drop = FALSE]))
    premier <- premier + des$p0 * (length(cv$categories) - 1)
  }
  if (length(lignes)) do.call(rbind, lignes) else data.frame(test = character(0), quoi = character(0), W = numeric(0), ddl = numeric(0), p = numeric(0))
}

# Le modèle 3 sans covariable en droite et écarts à la droite (rtrim § 4.2 ; coef(, "trend"),
# coef(, "deviations")) : β* = Σ dj γj / D, γ*j = γj − moyenne(γ) − dj β*, dj = j − moyenne(j),
# D = Σ dj² ; leur covariance T cov(γ) Tᵀ (γ1 = 0 compris).
droite_ecarts_trim <- function(fit) {
  J <- fit$des$p0 + 1
  j <- seq_len(J)
  dj <- j - mean(j)
  D <- sum(dj^2)
  Tm <- rbind(dj / D, diag(J) - 1 / J - outer(dj, dj) / D)
  g <- c(0, fit$beta[seq_len(J - 1)])
  Vg <- rbind(0, cbind(0, fit$V[seq_len(J - 1), seq_len(J - 1), drop = FALSE]))
  x <- drop(Tm %*% g)
  Vx <- Tm %*% Vg %*% t(Tm)
  list(bstar = x[1], se_bstar = sqrt(max(0, Vx[1, 1])), gstar = x[-1], se_gstar = sqrt(pmax(0, diag(Vx)[-1])), V = Vx)
}

# Le test du score d'un point de changement retiré (rtrim § 4.1.1 ; Score) : le modèle avec lui,
# sa pente égale à celle d'avant (le même μ que le modèle sans lui), S = Uᵀ I*⁻¹ U au point du modèle
# sans lui (ses α, σ² et ρ) ; χ² à autant de degrés de liberté que de blocs.
score_changement <- function(t, fit, cp_actifs, c) {
  cp <- sort(c(cp_actifs, c))
  des <- dessins_trim(t, 2, cp)
  nb <- length(des$blocs)
  avant <- sum(cp_actifs < c)
  b <- matrix(fit$beta, length(cp_actifs), nb)
  ajout <- if (avant == 0) matrix(0, 1, nb) else b[avant, , drop = FALSE]
  bt <- rbind(b[seq_len(avant), , drop = FALSE], ajout, b[avant + seq_len(length(cp_actifs) - avant), , drop = FALSE])
  s <- systeme_trim(t, des, fit$alpha, as.vector(bt), fit$sig2, fit$rho)
  S <- drop(t(s$U) %*% solve(s$I, s$U))
  stats::pchisq(S, nb, lower.tail = FALSE)
}

# La sélection pas à pas des points de changement (rtrim § 4.1 ; trim_refine) : à chaque tour, le point
# au plus grand p de Wald sort s'il dépasse TRIM_SORTIE, puis le point retiré au plus petit p du score
# revient s'il est sous TRIM_ENTREE ; jusqu'à ce que rien ne bouge. Rend le modèle et les étapes.
pas_a_pas_trim <- function(t, cp, r) {
  depart <- cp
  actifs <- rep(TRUE, length(cp))
  fit <- estimer_trim(t, dessins_trim(t, 2, cp), r$surdispersion, r$correlation)
  etapes <- list()
  for (tour in 1:100) {
    sorti <- FALSE
    if (any(actifs)) {
      # (les changements de pente seulement : un dernier point au premier temps, sans covariable,
      # n'a qu'un test de la pente et reste, comme dans rtrim)
      w <- tests_trim(t, fit)
      w <- w[w$test == "changement", , drop = FALSE]
      k <- which.max(w$p)
      if (length(k) && w$p[k] > TRIM_SORTIE) {
        c <- depart[actifs][k]
        actifs[depart == c] <- FALSE
        etapes[[length(etapes) + 1]] <- data.frame(etape = length(etapes) + 1, action = "retire", temps = t$temps[c], test = "wald", p = w$p[k], stringsAsFactors = FALSE)
        fit <- estimer_trim(t, dessins_trim(t, 2, depart[actifs]), r$surdispersion, r$correlation)
        sorti <- TRUE
      }
    }
    entre <- FALSE
    candidats <- depart[!actifs]
    if (length(candidats)) {
      ps <- vapply(candidats, function(c) score_changement(t, fit, depart[actifs], c), 0)
      k <- which.min(ps)
      if (ps[k] < TRIM_ENTREE) {
        c <- candidats[k]
        actifs[depart == c] <- TRUE
        etapes[[length(etapes) + 1]] <- data.frame(etape = length(etapes) + 1, action = "remis", temps = t$temps[c], test = "score", p = ps[k], stringsAsFactors = FALSE)
        fit <- estimer_trim(t, dessins_trim(t, 2, depart[actifs]), r$surdispersion, r$correlation)
        entre <- TRUE
      }
    }
    if (!sorti && !entre) break
  }
  fit$pas_a_pas <- list(depart = t$temps[depart], etapes = if (length(etapes)) do.call(rbind, etapes) else NULL, gardes = t$temps[depart[actifs]])
  fit
}

# ---------------------------------------------------------------- ajuster

# Les paramètres en tableau : pour chaque bloc (la référence, puis chaque catégorie des covariables
# après la leur), un modèle 2 par morceau (de, à), un modèle 3 par temps (le premier à 0) ; additifs et
# multiplicatifs (exp), leurs erreurs-types (celle d'un paramètre multiplicatif : exp(θ) × se(θ)).
parametres_trim <- function(t, fit) {
  des <- fit$des
  if (!des$p0) return(NULL)
  se <- sqrt(pmax(0, diag(fit$V)))
  J <- length(t$temps)
  do.call(rbind, lapply(seq_along(des$blocs), function(b) {
    k <- (b - 1) * des$p0 + seq_len(des$p0)
    bl <- des$blocs[[b]]
    tete <- data.frame(covariable = bl$covariable, categorie = bl$categorie, stringsAsFactors = FALSE)
    if (des$modele == 2) {
      fins <- c(des$cp[-1], J)
      cbind(tete, data.frame(de = t$temps[des$cp], a = t$temps[fins], additif = fit$beta[k], se_additif = se[k],
                             multiplicatif = exp(fit$beta[k]), se_multiplicatif = exp(fit$beta[k]) * se[k]))
    } else {
      cbind(tete, data.frame(temps = t$temps, additif = c(0, fit$beta[k]), se_additif = c(0, se[k]),
                             multiplicatif = c(1, exp(fit$beta[k])), se_multiplicatif = c(0, exp(fit$beta[k]) * se[k])))
    }
  }))
}

# L'ajustement (khi² de Pearson, rapport de vraisemblance, AIC = LR − 2 ddl ; rtrim : gof), sur les
# comptages observés ; ddl = O − (sites + paramètres du temps).
ajustement_trim <- function(t, fit) {
  f <- t$f
  k <- !is.na(f)
  chi2 <- sum((f[k] - fit$mu[k])^2 / fit$mu[k])
  lr <- rapport_vraisemblance(f, fit$mu)
  ddl <- sum(k) - length(t$sites) - fit$des$p
  list(chi2 = chi2, LR = lr, ddl = ddl, p_chi2 = stats::pchisq(chi2, ddl, lower.tail = FALSE), p_LR = stats::pchisq(lr, ddl, lower.tail = FALSE), AIC = lr - 2 * ddl)
}

# TRIM (definir_modele, type "TRIM") : Y les comptages, X des covariables qualitatives, et les
# réglages de reglages_trim (d$options).
ajuster_trim <- function(projet, d) {
  r <- reglages_trim(d)
  t <- donnees_trim(projet, d, r)
  J <- length(t$temps)
  rang <- function(x) {
    k <- which(abs(t$temps - x) < 1e-8 * max(1, abs(x)))
    if (length(k)) k[1] else NA_integer_
  }
  cp <- integer(0)
  retires <- integer(0)
  if (r$modele == 2) {
    cp <- if (r$tous) seq_len(J - 1) else if (length(r$changements)) vapply(r$changements, rang, 0L) else 1L
    if (anyNA(cp)) {
      x <- r$changements[is.na(cp)][1]
      stop(sprintf(traduire("Le point de changement %s n'est pas un temps des données (de %s à %s, au pas de %s).", "The changepoint %s isn't a time of the data (from %s to %s, by %s)."),
                   format(x, digits = 15), format(t$temps[1], digits = 15), format(t$temps[J], digits = 15), format(t$pas, digits = 15)), call. = FALSE)
    }
    if (any(cp == J)) {
      stop(sprintf(traduire("Le dernier temps (%s) ne peut pas être un point de changement : aucune pente n'en part.", "The last time (%s) can't be a changepoint: no slope starts from it."),
                   format(t$temps[J], digits = 15)), call. = FALSE)
    }
    ce <- changements_estimables(t, sort(unique(cp)))
    cp <- ce$cp
    retires <- ce$retires
    if (r$pas_a_pas && length(cp) < 2) {
      stop(traduire("La sélection pas à pas demande au moins deux points de changement.", "Stepwise selection needs at least two changepoints."), call. = FALSE)
    }
  }
  verifier_estimable_trim(t, r$modele)
  fit <- if (r$modele == 2 && r$pas_a_pas) pas_a_pas_trim(t, cp, r) else estimer_trim(t, dessins_trim(t, r$modele, cp), r$surdispersion, r$correlation)
  base <- 1L
  if (!is.null(r$base)) {
    base <- rang(r$base)
    if (is.na(base)) {
      stop(sprintf(traduire("Le temps de base des indices (%s) n'est pas un temps des données (de %s à %s).", "The base time of the indices (%s) isn't a time of the data (from %s to %s)."),
                   format(r$base, digits = 15), format(t$temps[1], digits = 15), format(t$temps[J], digits = 15)), call. = FALSE)
    }
  }
  tt <- totaux_trim(t, fit)
  im <- indices_trim(tt$modele, tt$V_modele, base)
  ii <- indices_trim(tt$impute, tt$V_impute, base)
  devs <- if (fit$des$modele == 3 && !length(t$covariables)) droite_ecarts_trim(fit)
  pentes <- rbind(cbind(quoi = "imputee", pente_globale(tt$impute, tt$V_impute), stringsAsFactors = FALSE),
                  cbind(quoi = "modele", pente_globale(tt$modele, tt$V_modele), stringsAsFactors = FALSE))
  m <- list(type = "TRIM", N = length(t$observations), observations = t$observations, exclues = t$exclues, reglages_trim = r, donnees = t,
            modele = fit$des$modele, changements = t$temps[fit$des$cp], changements_retires = t$temps[retires], pas_a_pas = fit$pas_a_pas,
            ajustement_noyau = fit, base = base, sigma2 = if (r$surdispersion) fit$sig2 else NA_real_, rho = if (r$correlation) fit$rho else NA_real_,
            totaux = data.frame(temps = t$temps, observe = tt$observe, modele = tt$modele, se_modele = sqrt(pmax(0, diag(tt$V_modele))),
                                impute = tt$impute, se_impute = sqrt(pmax(0, diag(tt$V_impute)))),
            V_totaux = list(modele = tt$V_modele, impute = tt$V_impute),
            indices = data.frame(temps = t$temps, modele = im$indice, se_modele = im$se, impute = ii$indice, se_impute = ii$se),
            pentes = pentes, parametres = parametres_trim(t, fit), ecarts = devs, tests = tests_trim(t, fit, devs), ajustement = ajustement_trim(t, fit),
            O = sum(!is.na(t$f)), I = length(t$sites), J = J)
  class(m) <- "modele_trim"
  m
}

print.modele_trim <- function(x, ...) {
  cat(sprintf("TRIM, modèle %d : %d sites × %d temps, %d comptages observés ; σ² %s, ρ %s\n", x$modele, x$I, x$J, x$O,
              format(x$sigma2, digits = 4), format(x$rho, digits = 4)))
  print(x$indices, digits = 4, row.names = FALSE)
  print(x$pentes[, c("quoi", "multiplicative", "se_multiplicative", "p", "classe")], digits = 4, row.names = FALSE)
  invisible(x)
}

# TRIM vérifie lui-même ses données (diagnostic.R : verifier_donnees) : des comptages manquants
# sont imputés, ce n'est pas un défaut à signaler avant l'ajustement.
verifier_trim <- function(projet, d) list(variables = data.frame(), observations = data.frame())

# ---------------------------------------------------------------- l'interface

# La classe d'une pente en mots (TRIM 3 : strong increase, moderate increase, stable, uncertain,
# moderate decline, steep decline).
libelle_classe_trim <- function(classe, fr) {
  mots <- if (fr) c(forte_hausse = "forte augmentation", hausse_moderee = "augmentation modérée", stable = "stable", incertaine = "incertaine",
                    declin_modere = "déclin modéré", fort_declin = "fort déclin", inconnue = "inconnue")
          else c(forte_hausse = "strong increase", hausse_moderee = "moderate increase", stable = "stable", incertaine = "uncertain",
                 declin_modere = "moderate decline", fort_declin = "steep decline", inconnue = "unknown")
  unname(mots[classe])
}

# Les observations du modèle (ses lignes) : leur site et leur temps, le comptage, le comptage attendu
# du modèle (μ), le comptage imputé (le comptage s'il est connu, μ sinon) et le résidu de Pearson.
observations_trim <- function(projet, m) {
  t <- m$donnees
  ij <- which(!is.na(t$uid), arr.ind = TRUE)
  k <- match(m$observations, t$uid[ij])
  ij <- ij[k, , drop = FALSE]
  f <- t$f[ij]
  mu <- m$ajustement_noyau$mu[ij]
  data.frame(uid = m$observations, nom = unname(projet$noms_obs[as.character(m$observations)]), site = t$sites[ij[, 1]], temps = t$temps[ij[, 2]],
             compte = f, ajuste = mu, impute = ifelse(is.na(f), mu, f), residu = (f - mu) / sqrt(mu), stringsAsFactors = FALSE)
}

# Par site : ses comptages observés, positifs, leur total, son effet (α, et exp(α) : son comptage
# attendu au premier temps, sans poids ni covariable).
sites_trim <- function(m) {
  t <- m$donnees
  f <- t$f
  data.frame(site = t$sites, observes = rowSums(!is.na(f)), positifs = rowSums(!is.na(f) & f > 0), total = rowSums(f, na.rm = TRUE),
             total_ajuste = rowSums(ifelse(is.na(f), 0, m$ajustement_noyau$mu)), alpha = m$ajustement_noyau$alpha, stringsAsFactors = FALSE)
}

resultat_trim <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  t <- m$donnees
  fit <- m$ajustement_noyau
  pi <- m$pentes[1, ]
  matrice <- function(M) { M <- unname(M); M }
  list(id = id, projet = p$id, nom = d$name, titre = d$title %||% "", type = "TRIM", analyse = TRUE, prediction = FALSE, regression = FALSE,
       opls = FALSE, Ap = 0, A = 0, N = m$N, K = length(t$covariables), noms_y = I(t$noms$y),
       chiffres = sprintf("%s %s · %s", traduire("pente", "slope"), if (is.na(pi$multiplicative)) "—" else formatC(pi$multiplicative, format = "f", digits = 3),
                          libelle_classe_trim(pi$classe, !identical(getOption("nuee.langue"), "en"))),
       vues_defaut = I(c("trim_indices", "trim_totaux")),
       observations = observations_trim(p, m), sites = sites_trim(m),
       modele_trim = m$modele, changements = I(m$changements), changements_retires = I(m$changements_retires), pas_a_pas = m$pas_a_pas,
       temps = I(t$temps), pas = t$pas, temps_ajoutes = I(t$ajoutes), base = t$temps[m$base],
       comptes = matrice(t$f), ajustes = matrice(fit$mu), poids = if (t$ponderee) matrice(t$w),
       totaux = m$totaux, indices = m$indices, pentes = m$pentes, parametres = m$parametres, tests = m$tests, ajustement = m$ajustement,
       ecarts = if (!is.null(m$ecarts)) list(pente = m$ecarts$bstar, se_pente = m$ecarts$se_bstar,
                                             ecarts = data.frame(temps = t$temps, additif = m$ecarts$gstar, se_additif = m$ecarts$se_gstar,
                                                                 multiplicatif = exp(m$ecarts$gstar), se_multiplicatif = exp(m$ecarts$gstar) * m$ecarts$se_gstar)),
       sigma2 = m$sigma2, rho = m$rho, methode = fit$methode, surdispersion = fit$surdispersion, correlation = fit$correlation,
       I = m$I, J = m$J, O = m$O, noms = t$noms, covariables = I(vapply(t$covariables, `[[`, "", "nom")),
       categories = lapply(t$covariables, function(cv) I(cv$categories)), covariables_retirees = I(t$covariables_retirees),
       sites_vides = I(t$sites_vides), lignes_vides = t$lignes_vides, sans_site_temps = t$sans_site_temps, exclues_manquantes = length(m$exclues),
       iterations = fit$iterations,
       reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       demande = d$demande %||% list(type = d$type, numero = d$number))
}

# ---------------------------------------------------------------- Exporter

# Les tableaux de TRIM (formats.R : tables_resultats) : le résumé, les indices, les totaux, les pentes
# globales, les paramètres, les tests de Wald, l'ajustement, la droite et ses écarts (modèle 3 sans
# covariable), les étapes du pas à pas, les sites et les observations.
tables_trim <- function(projet, m) {
  t <- m$donnees
  fit <- m$ajustement_noyau
  a <- m$ajustement
  obs <- observations_trim(projet, m)
  obs$uid <- NULL
  names(obs)[names(obs) == "nom"] <- "Observation"
  out <- list(
    resume = data.frame(modele = m$modele, sites = m$I, temps = m$J, premier_temps = t$temps[1], dernier_temps = t$temps[m$J], pas = t$pas,
                        comptages_observes = m$O, cases = m$I * m$J, methode = fit$methode, sigma2 = m$sigma2, rho = m$rho, temps_de_base = t$temps[m$base],
                        changements = paste(format(m$changements, digits = 15), collapse = " "), covariables = paste(vapply(t$covariables, `[[`, "", "nom"), collapse = " "),
                        poids = t$noms$poids %||% "", sites_sans_comptage_positif = length(t$sites_vides), stringsAsFactors = FALSE),
    indices = m$indices, totaux = m$totaux, pentes = m$pentes)
  if (!is.null(m$parametres)) out$parametres <- m$parametres
  if (nrow(m$tests)) out$tests <- m$tests
  out$ajustement <- data.frame(statistique = c("chi2", "LR", "AIC"), valeur = c(a$chi2, a$LR, a$AIC), ddl = c(a$ddl, a$ddl, NA), p = c(a$p_chi2, a$p_LR, NA))
  if (!is.null(m$ecarts)) {
    out$droite <- data.frame(additif = m$ecarts$bstar, se_additif = m$ecarts$se_bstar, multiplicatif = exp(m$ecarts$bstar), se_multiplicatif = exp(m$ecarts$bstar) * m$ecarts$se_bstar)
    out$ecarts <- data.frame(temps = t$temps, additif = m$ecarts$gstar, se_additif = m$ecarts$se_gstar, multiplicatif = exp(m$ecarts$gstar), se_multiplicatif = exp(m$ecarts$gstar) * m$ecarts$se_gstar)
  }
  if (!is.null(m$pas_a_pas$etapes)) out$pas_a_pas <- m$pas_a_pas$etapes
  out$sites <- sites_trim(m)
  out$observations <- obs
  out
}

# ---------------------------------------------------------------- texte des méthodes

REFERENCES_TRIM <- list(
  pannekoek2005 = "Pannekoek, J., & van Strien, A. (2005). TRIM 3 manual (TRends & Indices for Monitoring data). Statistics Netherlands, Voorburg.",
  pannekoek2018 = "Pannekoek, J., Bogaart, P., & van der Loo, M. (2018). Models and statistical methods in rtrim. Statistics Netherlands. https://CRAN.R-project.org/package=rtrim",
  bogaart = "Bogaart, P., van der Loo, M., & Pannekoek, J. (2016). rtrim: Trends and indices for monitoring data (R package). https://CRAN.R-project.org/package=rtrim",
  liang = "Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using generalized linear models. Biometrika, 73(1), 13–22. https://doi.org/10.1093/biomet/73.1.13")

texte_methodes_trim <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  t <- m$donnees
  fit <- m$ajustement_noyau
  nombre <- function(x, chiffres = 2) nombre_texte(x, chiffres, fr)
  temps <- function(x) trimws(formatC(x, format = "fg", digits = 15, decimal.mark = tx(",", ".")))
  valeur_p <- function(p) if (is.na(p)) "—" else if (p < 0.001) tx("p < 0,001", "p < 0.001") else paste("p =", nombre(p, 3))
  cites <- c("pannekoek2005", "pannekoek2018")
  cite <- function(...) cites <<- union(cites, c(...))
  cases <- m$I * m$J
  # les données
  p1 <- sprintf(tx("Les comptages de %s ont été analysés par TRIM (TRends and Indices for Monitoring data ; Pannekoek et van Strien, 2005) : %s, %d sites suivis de %s à %s (%d temps, au pas de %s), soit %d comptages observés sur les %d cases site × temps (%s) ; les %d autres ont été imputés par le modèle.",
                   "The counts of %s were analysed with TRIM (TRends and Indices for Monitoring data; Pannekoek and van Strien, 2005): %s, %d sites surveyed from %s to %s (%d times, in steps of %s), that is %d observed counts out of the %d site × time cells (%s); the %d others were imputed by the model."),
                guillemets_texte(t$noms$y, fr), observations_texte(m$N, d, fr), m$I, temps(t$temps[1]), temps(t$temps[m$J]), m$J, temps(t$pas),
                m$O, cases, pourcent_texte(m$O / cases, fr), cases - m$O)
  if (length(t$sites_vides) == 1) p1 <- paste(p1, tx("Un site sans aucun comptage positif a été laissé de côté, comme le fait TRIM.", "One site without any positive count was left out, as TRIM does."))
  if (length(t$sites_vides) > 1) {
    p1 <- paste(p1, sprintf(tx("%d sites sans aucun comptage positif ont été laissés de côté, comme le fait TRIM.", "%d sites without any positive count were left out, as TRIM does."), length(t$sites_vides)))
  }
  if (length(t$ajoutes)) {
    plusieurs <- length(t$ajoutes) > 1
    p1 <- paste(p1, sprintf(if (plusieurs) tx("Les temps %s n'avaient aucune ligne : tous leurs comptages ont été imputés.", "Times %s had no row: all their counts were imputed.")
                            else tx("Le temps %s n'avait aucune ligne : tous ses comptages ont été imputés.", "Time %s had no row: all its counts were imputed."),
                            liste_mots(temps(t$ajoutes), fr)))
  }
  # le modèle
  modele <- switch(as.character(m$modele),
    "1" = tx("Le modèle n'a qu'un effet de chaque site (modèle 1 de TRIM, sans effet du temps : ln μij = αi).",
             "The model has only an effect of each site (TRIM model 1, no time effect: ln μij = αi)."),
    "2" = {
      cp <- m$changements
      pente <- if (!length(cp)) tx("sans pente (tous les points de changement retirés)", "without a slope (every changepoint removed)")
               else if (length(cp) == 1 && cp[1] == t$temps[1]) tx("d'une seule pente", "with a single slope")
               else sprintf(tx("dont la pente change en %s", "whose slope changes at %s"), liste_mots(temps(cp), fr))
      x <- sprintf(tx("Le modèle a un effet de chaque site et une tendance linéaire sur l'échelle logarithmique (modèle 2 de TRIM), %s.",
                      "The model has an effect of each site and a linear trend on the log scale (TRIM model 2), %s."), pente)
      if (!is.null(m$pas_a_pas)) {
        x <- paste(x, sprintf(tx("Les points de changement ont été choisis pas à pas parmi %s (un point retiré quand le test de Wald de son changement de pente dépasse p = %s, remis quand son test du score passe sous p = %s ; Pannekoek et al., 2018).",
                                 "The changepoints were chosen stepwise among %s (a point removed when the Wald test of its change in slope exceeds p = %s, put back when its score test falls below p = %s; Pannekoek et al., 2018)."),
                              if (length(m$pas_a_pas$depart) > 6) sprintf(tx("%d temps", "%d times"), length(m$pas_a_pas$depart)) else liste_mots(temps(m$pas_a_pas$depart), fr),
                              nombre(TRIM_SORTIE, 2), nombre(TRIM_ENTREE, 2)))
      }
      if (length(m$changements_retires)) {
        x <- paste(x, sprintf(if (length(m$changements_retires) > 1)
                                tx("Les points de changement %s, sans comptage positif après eux, ont été retirés, comme le fait TRIM.", "Changepoints %s, without positive counts after them, were removed, as TRIM does.")
                              else tx("Le point de changement %s, sans comptage positif après lui, a été retiré, comme le fait TRIM.", "Changepoint %s, without a positive count after it, was removed, as TRIM does."),
                              liste_mots(temps(m$changements_retires), fr)))
      }
      x
    },
    tx("Le modèle a un effet de chaque site et de chaque temps (modèle 3 de TRIM : ln μij = αi + γj).",
       "The model has an effect of each site and of each time (TRIM model 3: ln μij = αi + γj)."))
  covs <- vapply(t$covariables, `[[`, "", "nom")
  if (length(covs)) {
    s <- if (length(covs) > 1) "s" else ""
    modele <- paste(modele, sprintf(tx("Les paramètres du temps varient selon %s (covariable%s qualitative%s ; la première catégorie de chacune sert de référence).",
                                       "The time parameters vary with %s (qualitative covariate%s%s; the first category of each is the reference)."),
                                    liste_mots(vapply(covs, guillemets_texte, "", fr), fr), s, if (fr) s else ""))
  }
  if (t$ponderee) modele <- paste(modele, sprintf(tx("Les comptages sont pondérés par %s (TRIM : ln wij μij = …).", "The counts are weighted by %s (TRIM: ln wij μij = …)."), guillemets_texte(t$noms$poids, fr)))
  estimation <- if (fit$methode == "GEE") {
    cite("liang")
    parts <- c(if (fit$surdispersion) sprintf(tx("de la surdispersion (σ² = %s)", "overdispersion (σ² = %s)"), nombre(m$sigma2, 2)),
               if (fit$correlation) sprintf(tx("de la corrélation sérielle entre deux temps successifs d'un même site (ρ = %s)", "serial correlation between successive times of a site (ρ = %s)"), nombre(m$rho, 3)))
    sprintf(tx("Les paramètres ont été estimés par des équations d'estimation généralisées (Liang et Zeger, 1986) qui tiennent compte %s.",
               "The parameters were estimated by generalized estimating equations (Liang and Zeger, 1986) that account for %s."), liste_mots(parts, fr))
  } else tx("Les paramètres ont été estimés par le maximum de vraisemblance de Poisson, sans surdispersion ni corrélation sérielle.",
            "The parameters were estimated by Poisson maximum likelihood, without overdispersion or serial correlation.")
  p2 <- paste(modele, estimation)
  # les résultats
  pi <- m$pentes[1, ]
  base <- temps(t$temps[m$base])
  p3 <- sprintf(tx("Les indices sont les totaux de chaque temps (imputés : les comptages observés et, pour les manquants, ceux du modèle) rapportés à celui de %s, avec leurs erreurs-types par la méthode delta.",
                   "The indices are the totals of each time (imputed: the observed counts and, for the missing ones, the model's) relative to that of %s, with their standard errors by the delta method."), base)
  if (!is.na(pi$multiplicative)) {
    p3 <- paste(p3, sprintf(tx("La pente globale des totaux imputés (celle de la droite des moindres carrés à travers leur logarithme) est de %s par pas de temps (erreur-type %s ; IC à 95 %% %s–%s, loi t à %d degrés de liberté), soit %s%s par pas : %s, selon la classification de TRIM (l'intervalle de confiance de la pente comparé à 1, à 0,95 et à 1,05)%s.",
                               "The overall slope of the imputed totals (that of the least-squares line through their logarithm) is %s per time step (standard error %s; 95%% CI %s–%s, t distribution with %d degrees of freedom), that is %s%s per step: %s, by TRIM's classification (the slope's confidence interval compared with 1, 0.95 and 1.05)%s."),
                            nombre(pi$multiplicative, 4), nombre(pi$se_multiplicative, 4), nombre(pi$bas, 4), nombre(pi$haut, 4), m$J - 2,
                            if (pi$multiplicative >= 1) "+" else "−", pourcent_texte(abs(pi$multiplicative - 1), fr), libelle_classe_trim(pi$classe, fr),
                            if (!is.na(pi$niveau)) sprintf(tx(", %s", ", %s"), valeur_p(pi$p)) else ""))
  }
  a <- m$ajustement
  p4 <- sprintf(tx("Ajustement : khi² de Pearson = %s, rapport de vraisemblance = %s (%d degrés de liberté ; %s et %s), AIC = %s%s.",
                   "Goodness of fit: Pearson chi-square = %s, likelihood ratio = %s (%d degrees of freedom; %s and %s), AIC = %s%s."),
                nombre(a$chi2, 1), nombre(a$LR, 1), a$ddl, valeur_p(a$p_chi2), valeur_p(a$p_LR), nombre(a$AIC, 1),
                if (fit$methode == "GEE") tx(" ; ces tests supposent des comptages de Poisson indépendants, ce que démentent σ² ou ρ estimés : les tests de Wald, eux, en tiennent compte",
                                             "; these tests assume independent Poisson counts, which an estimated σ² or ρ contradicts: the Wald tests account for them") else "")
  w <- m$tests
  wald <- character(0)
  for (i in seq_len(nrow(w))) {
    x <- w[i, ]
    stat <- sprintf("W = %s, %d %s, %s", nombre(x$W, 2), as.integer(x$ddl), tx("ddl", "df"), valeur_p(x$p))
    wald <- c(wald, switch(x$test,
      pente = sprintf(tx("la pente (%s)", "the slope (%s)"), stat),
      changement = sprintf(tx("le changement de pente en %s (%s)", "the change in slope at %s (%s)"), x$quoi, stat),
      deviations = sprintf(tx("les écarts à la tendance linéaire (%s)", "the deviations from the linear trend (%s)"), stat),
      covariable = sprintf(tx("l'effet de %s (%s)", "the effect of %s (%s)"), guillemets_texte(x$quoi, fr), stat)))
  }
  if (length(wald)) p4 <- paste(p4, sprintf(tx("Tests de Wald : %s.", "Wald tests: %s."), paste(wald, collapse = tx(" ; ", "; "))))
  cite("bogaart", "r", "nuee")
  annee_r <- R.version$year
  annee_nuee <- format(Sys.Date(), "%Y")
  logiciel <- sprintf(tx("Les calculs ont été faits avec Nuée%s (Boutin, %s) et R %s (R Core Team, %s), d'après la méthode publiée de TRIM (Pannekoek et al., 2018) ; ils donnent les résultats du paquet rtrim (Bogaart et al., 2016).",
                         "Computations were done with Nuée%s (Boutin, %s) and R %s (R Core Team, %s), from TRIM's published method (Pannekoek et al., 2018); they give the results of the rtrim package (Bogaart et al., 2016)."),
                      if (nzchar(version)) paste0(" ", version) else "", annee_nuee, paste(R.version$major, R.version$minor, sep = "."), annee_r)
  references <- vapply(cites, function(c) switch(c,
    r = sprintf(REFERENCES$r, annee_r),
    nuee = sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
    REFERENCES_TRIM[[c]]), "")
  list(paragraphes = c(p1, p2, p3, p4, logiciel), references = unname(sort(references)))
}
