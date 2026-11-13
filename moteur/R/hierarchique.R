# Modèles hiérarchiques : un modèle dont des termes X sont les scores d'autres modèles du projet
# (des variables $M1.t1(titre)... faites par Modèle › Scores en variables, ou venues d'un fichier
# .usp). Sa validation croisée prend ces scores tels quels : ils ont été calculés avec toutes les
# observations, Y du groupe caché compris, si bien que son Q2 se sert deux fois de Y. Le Q2
# honnête (q2_hierarchique) refait à chaque tour les modèles de base sans le groupe caché, puis
# leurs scores pour toutes les observations, et valide le modèle sur ces scores.

# Le modèle de base et la composante de chaque terme X de `d` qui est un score (« $M1.t2... » :
# le modèle M1 du projet, composante 2), dans l'ordre où les refaire : les modèles de base
# d'un modèle de base avant lui. Un modèle absent du projet ou sans composante n'y est pas :
# `absents` donne le nom de ses variables.
modeles_de_base <- function(projet, d, vus = character(0)) {
  bases <- list()
  absents <- character(0)
  noms <- vapply(projet$variables, function(v) as.character(v$ids[1]), "")
  uids <- vapply(projet$variables, function(v) as.numeric(v$uid), 0)
  for (t in d$x) {
    if (length(t$variables) != 1) next
    nom <- noms[match(as.numeric(t$variables[1]), uids)]
    if (is.na(nom) || !startsWith(nom, "$")) next
    x <- regmatches(nom, regexec("^\\$([^.]+)\\.t(\\d+)", nom))[[1]]
    if (!length(x)) next
    db <- Filter(function(m) identical(as.character(m$name), x[2]), projet$models)
    db <- if (length(db)) db[[length(db)]]
    if (is.null(db) || !isTRUE(db$A > 0) || est_regression(db) || est_autre_analyse(db) || x[2] %in% vus) {
      absents <- c(absents, nom)
      next
    }
    if (is.null(bases[[x[2]]])) {
      plus_bas <- modeles_de_base(projet, db, c(vus, x[2]))
      for (n in names(plus_bas$bases)) {  # (un modèle déjà là garde ses variables, et sa place : avant ceux qui en dépendent)
        b <- plus_bas$bases[[n]]
        if (is.null(bases[[n]])) bases[[n]] <- b
        else bases[[n]]$variables <- c(bases[[n]]$variables, b$variables[setdiff(names(b$variables), names(bases[[n]]$variables))])
      }
      absents <- c(absents, plus_bas$absents)
      bases[[x[2]]] <- list(definition = db, variables = list())
    }
    bases[[x[2]]]$variables[[as.character(t$variables[1])]] <- as.integer(x[3])
  }
  list(bases = bases, absents = unique(absents))
}

# Un modèle ACP, PLS ou OPLS ajusté sans validation croisée (le modèle de base d'un tour), avec
# moins de A composantes si ses données n'en permettent pas autant ; sans aucune, une erreur de
# classe « trop_de_composantes ». `complet` : list(x, y), les écarts-types et le seuil de
# construire_bloc pour ses blocs X et Y (ceux d'un sous-modèle de la validation croisée).
ajuster_sans_validation <- function(projet, d, A = d$A, complet = NULL) {
  obs <- d$observations
  pls <- identical(d$type, "PLS")
  bx <- construire_bloc(projet, d$x, obs, spectres = d$spectres, complet = complet$x)
  by <- if (pls) construire_bloc(projet, d$y, obs, facteurs = bx$simples, complet = complet$y)
  X <- preparer(bx$X)
  Y <- if (pls) preparer(by$X)
  Ap <- d$Ap %||% 1
  res <- if (isTRUE(d$opls)) ajuster_opls(X, Y, max(0, A - Ap), Ap, jusqua = TRUE) else ajuster_matrices(X, Y, A, arret = function(res, a) FALSE)
  c(list(type = d$type, opls = isTRUE(d$opls), Ap = Ap, A = ncol(res$T), x = bx), res)
}

# Les scores d'observations projetées sur les composantes du modèle `m` (prediction.R : predire),
# avec les résidus de X qui leur restent : list(T, E, M).
scores_projetes <- function(projet, m, definition, observations) {
  pls <- identical(m$type, "PLS")
  bx <- construire_bloc(projet, definition$x, observations, imposees = m$x, spectres = definition$spectres)
  X <- preparer(bx$X)
  E <- X$E
  A <- m$A
  T <- matrix(NA_real_, nrow(E), A)
  # OPLS : les composantes orthogonales (après les Ap prédictives) sont retirées avant les prédictives
  Ap <- m$Ap %||% 1
  ordre <- if (isTRUE(m$opls)) c(seq_len(A)[-seq_len(Ap)], seq_len(Ap)) else seq_len(A)
  for (a in ordre) {
    v <- if (pls) m$W[, a] else m$P[, a]
    tt <- projeter(E, X$M, v)
    tt[rowSums(X$M) == 0] <- NA
    T[, a] <- tt
    E <- (E - outer(ifelse(is.na(tt), 0, tt), m$P[, a])) * X$M
  }
  list(T = T, E = E, M = X$M)
}

# Le projet d'un tour de la validation croisée de `d` : les modèles de base (`bases`,
# modeles_de_base) refaits sans les observations du groupe caché (`hors`, parmi celles de d), et
# avec les groupes par identifiant ou par variable, sans les autres observations de même valeur
# (le site caché tout entier) ; leurs scores remplacent ceux des variables, pour les observations
# qui en avaient. Chaque modèle de base y est mis à l'échelle comme un sous-modèle de sa propre
# validation croisée, au `seuil` de construire_bloc (sans fuite : SEUIL_ECART_VC ; centres seuls :
# infini, les poids de son modèle complet). Un modèle de base qui n'a plus de variation sans le
# groupe caché (ou moins de composantes) donne des scores nuls : le sous-modèle n'apprend rien de
# ces variables, comme d'un terme constant.
projet_sans_groupe <- function(projet, d, hors, bases, seuil = SEUIL_ECART_VC) {
  cles <- if (as.character(d$groupes_vc$by %||% "")[1] %in% c("id", "variable")) cles_groupes_vc(projet, d)
  cles_hors <- if (!is.null(cles)) unique(cles[hors])
  uids_hors <- d$observations[hors]
  p <- projet
  for (b in bases) {
    db <- resoudre_observations(p, b$definition)
    bx <- construire_bloc(p, db$x, db$observations, spectres = db$spectres)
    by <- if (identical(db$type, "PLS")) construire_bloc(p, db$y, db$observations, facteurs = bx$simples)
    complet <- list(x = list(ecart = bx$ecart, seuil = seuil), y = if (!is.null(by)) list(ecart = by$ecart, seuil = seuil))
    retirer <- db$observations %in% uids_hors
    if (!is.null(cles_hors)) {
      cb <- cles_groupes_vc(p, c(db[setdiff(names(db), "groupes_vc")], list(groupes_vc = d$groupes_vc)))
      retirer <- retirer | (!is.na(cb) & cb %in% cles_hors)
    }
    db$observations <- db$observations[!retirer]
    A <- max(unlist(b$variables), if (isTRUE(db$opls)) db$A)
    mb <- tryCatch(ajuster_sans_validation(p, db, A, complet), trop_de_composantes = function(e) NULL)
    for (k in names(b$variables)) {
      avec <- !is.na(p$donnees[, k])
      a <- b$variables[[k]]
      p$donnees[avec, k] <- if (is.null(mb) || a > mb$A) 0 else scores_projetes(p, mb, db, as.numeric(rownames(p$donnees)[avec]))$T[, a]
    }
  }
  p
}

# Le Q2 honnête d'un modèle hiérarchique `m` (ajusté d'après `d`) : sa validation croisée, chaque
# modèle de base étant refait à chaque tour sans le groupe caché (projet_sans_groupe). Elle suit
# la façon de valider du modèle (modèles de base compris), sauf avec les centres et poids du
# modèle complet, dont les sous-modèles n'ont pas de données à eux : alors avec les centres
# recalculés (la validation par défaut). Seulement pour une PLS : une ACP n'a pas de Y dont ses
# scores se serviraient deux fois. NULL pour un modèle sans score d'un autre modèle du projet ; sinon
# list(q2, q2cum, validation, bases : les modèles refaits, absents : les variables de scores prises
# telles quelles, leur modèle n'étant pas dans le projet), ou list(erreur) si un modèle de base
# ne peut pas être refait.
q2_hierarchique <- function(projet, d, m) {
  if (!identical(d$type, "PLS")) return(NULL)
  mb <- modeles_de_base(projet, d)
  if (!length(mb$bases)) {
    if (length(mb$absents)) return(list(bases = character(0), absents = I(mb$absents)))
    return(NULL)
  }
  dh <- d
  if (identical(mode_validation(d), "complet")) dh$validation <- "centres"
  seuil <- if (identical(mode_validation(dh), "centres")) Inf else SEUIL_ECART_VC
  r <- tryCatch({
    groupes <- groupes_definition(projet, dh, m$G)
    projets <- lapply(sort(unique(groupes)), function(g) projet_sans_groupe(projet, dh, groupes == g, mb$bases, seuil))
    calculer_modele(projet, dh, A = m$A, G = m$G, projets_vc = projets)
  }, error = function(e) conditionMessage(e))
  if (is.character(r)) return(list(erreur = r, bases = I(names(mb$bases)), absents = I(mb$absents)))
  list(q2 = r$q2, q2cum = r$q2cum, validation = mode_validation(dh), bases = I(names(mb$bases)), absents = I(mb$absents))
}
