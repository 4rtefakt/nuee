# Validation croisée.
#
# Les observations du modèle sont réparties à tour de rôle dans G groupes (7 par défaut) : la
# 1re, la 8e, la 15e... dans le groupe 1, la 2e, la 9e... dans le groupe 2, etc. Ou bien un groupe
# par valeur d'un identifiant ou d'une variable (groupes_definition) : toutes les années d'un site
# dans le même groupe, pour savoir si le modèle prédit un site qu'il n'a jamais vu. Chaque groupe est
# prédit par un sous-modèle ajusté sans lui, comme de nouvelles observations (prediction.R) :
#   - tout ce que le moteur calcule à partir des observations est recalculé sur celles des autres
#     groupes : centres et poids de la mise à l'échelle, spectre moyen du MSC, termes qualitatifs
#     0/1, termes croisés et carrés (construire_bloc) ; ce que la définition fixe reste fixe
#     (transformations, centres et poids enregistrés d'un modèle importé) ;
#   - le sous-modèle a le même nombre de composantes, sans validation croisée à lui (moins si les
#     observations qui lui restent n'ont plus de variation : ses prédictions restent alors celles
#     de sa dernière composante) ;
#   - les observations du groupe sont mises à l'échelle avec les centres et poids du sous-modèle,
#     projetées sur ses composantes une à une (valeurs manquantes ignorées, nipals.R), et leurs
#     prédictions, avec 1, 2... A composantes, sont ramenées dans les unités des données, puis
#     dans celles du modèle complet (ses centres et poids), où elles sont comparées aux valeurs
#     observées.
# PRESS = somme des carrés des erreurs de prédiction, par composante et par variable ; SS = somme
# des carrés de la variable dans le modèle complet, avant la 1re composante ;
# Q2(cum) = 1 - PRESS(a) / SS et Q2 = 1 - PRESS(a) / PRESS(a - 1), avec PRESS(0) = SS (si bien que
# 1 - Q2(cum) est le produit des 1 - Q2) ; de même pour chaque variable (Q2V).
# PLS, PLS-DA : les variables sont celles de Y. ACP : chaque case de X est prédite par les scores
# de son observation calculés sans les termes de son groupe de termes (les termes sont répartis à
# tour de rôle en G groupes, comme les observations), traités comme manquants. OPLS : opls.R.
# Le sous-modèle ne voit rien des observations qu'il prédit, pas même leur part dans les centres
# et les poids : avec peu d'observations, une validation faite sur les matrices du modèle complet
# (centrées avec toutes) donnait des Q2 très optimistes (Q2 de 0,98 sur du bruit pur avec 6
# observations).
#
# Trois façons de valider (d$validation, mode_validation) :
#   - "sans_fuite" : comme ci-dessus. Un terme UV ou Pareto qui ne varie presque
#     plus sans le groupe caché (écart-type des lignes d'apprentissage sous SEUIL_ECART_VC fois
#     celui du modèle complet) prend le poids du modèle complet : sinon son poids, 1 / écart-type,
#     est multiplié d'autant, et les prédictions du groupe caché explosent (Q2 de -8e11 sur un
#     projet réel, où une variable ne variait que sur un site). Le modèle dit lesquels
#     (fin_validation : gardes) ;
#   - "centres" (par défaut, pour tous les modèles) : seuls les centres sont recalculés sans le
#     groupe caché ; tous les termes gardent le poids du modèle complet. Le sous-modèle ne sait
#     rien du Y du groupe caché (ni de son niveau moyen, le centre de Y, ni de rien d'autre) ; ses
#     poids viennent des écarts-types de X sur toutes les observations, qui ne dépendent pas de Y
#     (une seule variable Y : son poids ne change pas les prédictions). Rien n'y est remis à
#     l'échelle sur moins d'observations : pas de prédiction qui explose ;
#   - "complet" : les centres et les poids du modèle complet ; une ACP ou une PLS valide chaque
#     composante sur les résidus du modèle complet (validation_par_composante). C'est la
#     validation des fichiers .usp, dont elle redonne les Q2 ; plus optimiste, puisque le centre
#     de Y et les composantes précédentes connaissent déjà le groupe caché.

MODES_VALIDATION <- c("sans_fuite", "centres", "complet")
SEUIL_ECART_VC <- 0.1

mode_validation <- function(d) {
  m <- as.character(d$validation %||% "")[1]
  if (m %in% MODES_VALIDATION) m else "centres"
}

groupes_validation <- function(n, G = 7) ((seq_len(n) - 1) %% G) + 1

# Le groupe de validation croisée de chaque observation du modèle `d` (d$observations), d'après
# d$groupes_vc :
#   - absent : à tour de rôle en G groupes (d$cv_groups, 7 par défaut) ;
#   - list(by = "id", id = nom) : un groupe par valeur de cet identifiant des observations
#     (« site », « annee »... : les observations de même valeur ensemble) ;
#   - list(by = "variable", variable = numéro) : de même, un groupe par valeur d'une variable
#     (par catégorie pour une variable qualitative) ;
#   - list(by = "groups", observations, groups) : le groupe de chaque observation, donné tel quel
#     (fichier .usp dont les groupes ne suivent ni l'ordre ni un identifiant).
# Les observations sans valeur forment un groupe à part. Les groupes sont numérotés dans l'ordre
# des valeurs (croissant pour des nombres, alphabétique pour du texte).
groupes_definition <- function(projet, d, G = d$cv_groups %||% 7) {
  obs <- d$observations
  v <- cles_groupes_vc(projet, d)
  if (is.null(v)) {
    if (!isTRUE(G >= 2)) stop(traduire("La validation croisée demande au moins deux groupes.", "Cross-validation needs at least two groups."), call. = FALSE)
    return(groupes_validation(length(obs), G))
  }
  niveaux <- niveaux_cles(v)
  groupes <- match(v, niveaux)
  groupes[is.na(groupes)] <- length(niveaux) + 1
  if (length(unique(groupes)) < 2) {
    stop(sprintf(traduire("Validation croisée par « %s » : les observations du modèle n'ont qu'une valeur, donc un seul groupe. Il en faut au moins deux.",
                          "Cross-validation by \"%s\": the model's observations have a single value, so a single group. At least two are needed."),
                 nom_groupes_vc(projet, d)), call. = FALSE)
  }
  groupes
}

# Les valeurs distinctes de `v` dans l'ordre des groupes : croissant pour des nombres,
# alphabétique pour du texte.
niveaux_cles <- function(v) {
  niveaux <- unique(v[!is.na(v)])
  nombres <- suppressWarnings(as.numeric(niveaux))
  if (!anyNA(nombres)) niveaux[order(nombres)] else sort(niveaux)
}

# Le nom de chaque groupe de la validation croisée de `d` (« 208 » pour le site 208), dans l'ordre
# de leurs numéros ; NULL quand les observations y sont réparties à tour de rôle.
noms_groupes_vc <- function(projet, d) {
  v <- tryCatch(cles_groupes_vc(projet, d), error = function(e) NULL)
  if (is.null(v)) return(NULL)
  niveaux <- niveaux_cles(v)
  c(niveaux, if (anyNA(v)) traduire("sans valeur", "no value"))
}

# Ce qui fait le groupe de chaque observation de `d` (texte, NA sans valeur) ; NULL quand elles
# sont réparties à tour de rôle.
cles_groupes_vc <- function(projet, d) {
  obs <- d$observations
  vc <- d$groupes_vc
  par <- if (length(vc$by)) as.character(vc$by) else ""
  if (!nzchar(par) || identical(par, "order")) return(NULL)
  valeurs <- switch(par,
    id = {
      k <- match(as.character(vc$id), as.character(projet$observation_id_names))
      if (is.na(k)) stop(sprintf(traduire("Validation croisée : ce projet n'a pas d'identifiant « %s ».",
                                          "Cross-validation: this project has no ID \"%s\"."), vc$id), call. = FALSE)
      identifiants_observations(projet, k, obs)
    },
    variable = {
      v <- variable_du_projet(projet, vc$variable)
      x <- colonne(projet, vc$variable, obs)
      if (length(v$categories)) v$categories[x] else x
    },
    groups = {
      g <- as.numeric(vc$groups)[match(as.character(obs), as.character(vc$observations))]
      if (anyNA(g)) stop(traduire("Validation croisée : les groupes de ce modèle viennent du fichier .usp, et certaines de ses observations n'en ont pas. Choisissez d'autres groupes dans Nouveau modèle.",
                                  "Cross-validation: this model's groups come from the .usp file, and some of its observations have none. Choose other groups in New model."), call. = FALSE)
      g
    },
    stop(sprintf(traduire("Validation croisée : groupes « %s » inconnus", "Cross-validation: unknown groups \"%s\""), par), call. = FALSE))
  v <- as.character(valeurs)
  v[!nzchar(v)] <- NA
  v
}

# Le k-ième identifiant des observations `obs` (NA s'il manque).
identifiants_observations <- function(projet, k, obs) {
  uid <- vapply(projet$observations, function(o) as.numeric(o$uid), 0)
  ids <- vapply(projet$observations, function(o) if (length(o$ids) >= k) as.character(o$ids[[k]] %||% NA) else NA_character_, "")
  unname(ids[match(as.numeric(obs), uid)])
}

# Ce qui fait les groupes de la validation croisée de `d`, en un mot (pour les messages et
# l'interface) : le nom de l'identifiant ou de la variable ; "" à tour de rôle.
nom_groupes_vc <- function(projet, d) {
  vc <- d$groupes_vc
  switch(if (length(vc$by)) as.character(vc$by) else "",
         id = as.character(vc$id),
         variable = tryCatch(as.character(variable_du_projet(projet, vc$variable)$ids[1]), error = function(e) as.character(vc$variable)),
         groups = traduire("groupes du fichier", "groups from the file"),
         "")
}

# Les sous-modèles de la validation croisée de la définition `d`, prêts à être ajustés : pour
# chaque groupe, leurs blocs X (et Y) préparés (preparer), construits sur les observations des
# autres groupes, et les lignes X du groupe, construites avec les centres et poids du sous-modèle.
#   X, Y : les blocs préparés du modèle complet, où les erreurs de prédiction sont mesurées ;
#   bx, by : ses centres et poids ; vx : les valeurs des termes X de ses observations
#   (valeurs_bloc), lues une fois pour tous les sous-modèles ; A : le nombre de composantes au
#   plus ; G : le nombre de groupes quand les observations y sont réparties à tour de rôle
#   (groupes_definition) ; projets (iPLS) : le projet de chaque groupe, quand ses valeurs en
#   dépendent.
debut_validation <- function(projet, d, X, Y, bx, by, A, G, poids, vx, projets = NULL) {
  obs <- d$observations
  pls <- !is.null(Y)
  mode <- mode_validation(d)
  groupes <- groupes_definition(projet, d, G)
  # G : celui de la définition pour des groupes à tour de rôle (même avec moins d'observations que
  # de groupes), sinon le nombre de groupes formés ; les termes (ACP) en font autant
  if (!identical(groupes, groupes_validation(length(obs), G))) G <- max(groupes)
  groupes_termes <- groupes_validation(ncol(X$E), G)
  Z <- if (pls) Y else X  # les variables prédites
  # centres et poids du modèle complet : une ACP ou une PLS avance composante par composante sur
  # les résidus du modèle complet (validation_par_composante) ; une OPLS (opls.R) a des sous-modèles faits sur les lignes des
  # matrices du modèle complet, déjà centrées et mises à l'échelle
  complet <- identical(mode, "complet")
  if (complet && !isTRUE(d$opls)) return(debut_par_composante(X, Y, bx, by, A, G, groupes, groupes_termes))
  seuil <- if (identical(mode, "centres")) Inf else SEUIL_ECART_VC
  tours <- lapply(sort(unique(groupes)), function(g) {
    hors <- groupes == g
    if (complet) {
      sx <- bx
      sy <- by
      Xs <- list(E = X$E[!hors, , drop = FALSE], M = X$M[!hors, , drop = FALSE])
      Ys <- list(E = Y$E[!hors, , drop = FALSE], M = Y$M[!hors, , drop = FALSE])
      Xh <- list(E = X$E[hors, , drop = FALSE], M = X$M[hors, , drop = FALSE])
    } else {
      p <- if (is.null(projets)) projet else projets[[g]]
      v <- if (is.null(projets)) vx else valeurs_bloc(p, d$x, obs)
      sx <- construire_bloc(p, d$x, obs[!hors], poids, spectres = d$spectres, bruts = v[!hors, , drop = FALSE],
                            complet = list(ecart = bx$ecart, seuil = seuil))
      hx <- construire_bloc(p, d$x, obs[hors], imposees = sx, spectres = d$spectres, bruts = v[hors, , drop = FALSE])
      sy <- if (pls) construire_bloc(p, d$y, obs[!hors], poids, facteurs = sx$simples, complet = list(ecart = by$ecart, seuil = seuil))
      Xs <- preparer(sx$X)
      Ys <- if (pls) preparer(sy$X)
      Xh <- preparer(hx$X)
    }
    r <- list(hors = hors, X = Xs, Y = Ys, E = Xs$E, F = Ys$E, depart = c(sum(Xs$E^2), if (pls) sum(Ys$E^2)),
              sous = if (pls) sy else sx, arrete = FALSE, prediction = matrix(0, sum(hors), ncol(Z$E)),
              gardes = c(sx$gardes, if (pls) sy$gardes),
              P = matrix(NA_real_, ncol(X$E), A), W = if (pls) matrix(NA_real_, ncol(X$E), A), C = if (pls) matrix(NA_real_, ncol(Y$E), A))
    if (pls) {
      r$Eh <- Xh$E
      r$Mh <- Xh$M
    } else {
      # ACP : pour chaque groupe de termes, les lignes du groupe sans ces termes (comme manquants)
      r$Mh <- lapply(seq_len(max(groupes_termes)), function(h) { M <- Xh$M; M[, groupes_termes == h] <- 0; M })
      r$Eh <- lapply(r$Mh, function(M) Xh$E * M)
    }
    r
  })
  # le PRESS du modèle vide (PLS, OPLS) : chaque groupe caché prédit par le centre de Y de son
  # sous-modèle, la moyenne des autres observations (fin_validation : q2_vide)
  press0 <- if (pls) Reduce(`+`, lapply(tours, function(r) {
    centre <- vers_modele_complet(matrix(0, sum(r$hors), ncol(Z$E)), r$sous, by)
    colSums(((Z$E[r$hors, , drop = FALSE] - centre) * Z$M[r$hors, , drop = FALSE])^2)
  }))
  list(mode = mode, pls = pls, tours = tours, Z = Z, complet = if (pls) by else bx, ss = colSums(Z$E^2), G = G,
       groupes = groupes, groupes_termes = groupes_termes, press = NULL, press0 = press0, predictions = list())
}

# Centres et poids du modèle complet, pour une ACP ou une PLS. À chaque composante a, chaque
# groupe est retiré des résidus du modèle complet après a - 1 composantes, une composante est
# ajustée sur les autres lignes, et les valeurs du groupe en sont prédites : PRESS, Q2 et les
# poids et loadings des tours (jack-knife) en viennent (validation_par_composante). Pour une PLS,
# chaque tour a aussi un sous-modèle ajusté sur les autres lignes des matrices du modèle complet,
# composante après composante, qui prédit le Y du groupe (Y prédit en validation croisée : RMSEcv,
# CV-ANOVA, classement).
debut_par_composante <- function(X, Y, bx, by, A, G, groupes, groupes_termes) {
  pls <- !is.null(Y)
  Z <- if (pls) Y else X
  tours <- lapply(sort(unique(groupes)), function(g) {
    h <- groupes == g
    r <- list(hors = h, P = matrix(NA_real_, ncol(X$E), A), W = if (pls) matrix(NA_real_, ncol(X$E), A),
              C = if (pls) matrix(NA_real_, ncol(Y$E), A))
    if (pls) {
      r$sous <- list(E = X$E[!h, , drop = FALSE], ME = X$M[!h, , drop = FALSE], F = Y$E[!h, , drop = FALSE], MF = Y$M[!h, , drop = FALSE],
                     Eh = X$E[h, , drop = FALSE], Mh = X$M[h, , drop = FALSE], prediction = matrix(0, sum(h), ncol(Y$E)), arrete = FALSE)
      r$sous$depart <- c(sum(r$sous$E^2), sum(r$sous$F^2))
    }
    r
  })
  list(mode = "complet", pls = pls, tours = tours, Z = Z, MX = X$M, complet = if (pls) by else bx, ss = colSums(Z$E^2), G = G,
       groupes = groupes, groupes_termes = groupes_termes, press = NULL, ss_a = NULL, predictions = list(),
       depart = c(sum(X$E^2), if (pls) sum(Y$E^2)))
}

# La composante `a` de chaque sous-modèle (ACP, PLS), et la ligne de PRESS qu'elle donne. Comme
# centres et poids du modèle complet (validation_par_composante) : E et F, les résidus de X et de
# Y du modèle complet avant cette
# composante, et `comp`, sa composante a.
composante_validation <- function(vc, a, E = NULL, F = NULL, comp = NULL) {
  if (!is.null(vc$depart)) return(validation_par_composante(vc, a, E, F, comp))
  press <- 0
  if (vc$pls) vc$predictions[[a]] <- matrix(NA_real_, nrow(vc$Z$E), ncol(vc$Z$E))
  for (i in seq_along(vc$tours)) {
    r <- vc$tours[[i]]
    comp <- if (!r$arrete) tryCatch(if (vc$pls) composante_pls(r$E, r$X$M, r$F, r$Y$M, r$depart) else composante_acp(r$E, r$X$M, r$depart),
                                    sans_variation = function(e) NULL)
    r$arrete <- is.null(comp)
    if (!r$arrete) {
      r$E <- (r$E - outer(comp$t, comp$p)) * r$X$M
      r$P[, a] <- comp$p
      if (vc$pls) {
        r$F <- (r$F - outer(comp$t, comp$c)) * r$Y$M
        r$W[, a] <- comp$w
        r$C[, a] <- comp$c
        th <- projeter(r$Eh, r$Mh, comp$w)
        r$Eh <- (r$Eh - outer(th, comp$p)) * r$Mh
        r$prediction <- r$prediction + outer(th, comp$c)
      } else {
        for (h in seq_along(r$Eh)) {
          cases <- vc$groupes_termes == h
          th <- projeter(r$Eh[[h]], r$Mh[[h]], comp$p)
          r$Eh[[h]] <- (r$Eh[[h]] - outer(th, comp$p)) * r$Mh[[h]]
          r$prediction[, cases] <- r$prediction[, cases] + outer(th, comp$p[cases])
        }
      }
    }
    prediction <- vers_modele_complet(r$prediction, r$sous, vc$complet)
    press <- press + colSums(((vc$Z$E[r$hors, , drop = FALSE] - prediction) * vc$Z$M[r$hors, , drop = FALSE])^2)
    if (vc$pls) vc$predictions[[a]][r$hors, ] <- prediction
    vc$tours[[i]] <- r
  }
  vc$press <- rbind(vc$press, press, deparse.level = 0)
  vc
}

# La composante `a` validée avec les centres et poids du modèle complet, sur E et F, ses résidus avant elle
# (centrés et mis à l'échelle avec toutes ses observations). Pour chaque groupe :
#   - PLS : une composante est ajustée sur les autres lignes de E et F, et le F du groupe en est
#     prédit ; PRESS(a) compare ces prédictions à F, et SS(a) est la somme des carrés de F. Le Y
#     prédit avec a composantes vient du sous-modèle du tour (debut_par_composante), qui avance
#     d'une composante : il n'a vu le groupe que par les centres et les poids du modèle complet ;
#   - ACP : chaque case d'un groupe d'observations est prédite par t * p, le loading p ajusté sans
#     son groupe d'observations et le score t sans son groupe de termes (Krzanowski, 1987),
#     chacun orienté comme la composante du modèle complet ; SS(a) est la somme des carrés de E.
#     Le détail de celle des fichiers .usp n'est pas publié : sur les ACP des projets d'essai, ce Q2 s'en
#     écarte de 0,02 en médiane (la PLS redonne le sien).
# Un groupe dont les autres lignes n'ont plus de variation (sous 1e-12 de la somme des carrés du
# modèle complet) n'a pas de composante : sa prédiction est 0.
validation_par_composante <- function(vc, a, E, F, comp) {
  ME <- vc$MX
  sans <- function(e) NULL
  if (vc$pls) {
    MF <- vc$Z$M
    press <- 0
    prediction <- matrix(NA_real_, nrow(F), ncol(F))
    for (i in seq_along(vc$tours)) {
      r <- vc$tours[[i]]
      h <- r$hors
      k <- tryCatch(composante_pls(E[!h, , drop = FALSE], ME[!h, , drop = FALSE], F[!h, , drop = FALSE], MF[!h, , drop = FALSE], vc$depart),
                    sans_variation = sans)
      if (!is.null(k)) {
        ph <- outer(projeter(E[h, , drop = FALSE], ME[h, , drop = FALSE], k$w), k$c)
        press <- press + colSums(((F[h, , drop = FALSE] - ph) * MF[h, , drop = FALSE])^2)
        r$W[, a] <- k$w
        r$P[, a] <- k$p
        r$C[, a] <- k$c
      } else {
        press <- press + colSums((F[h, , drop = FALSE] * MF[h, , drop = FALSE])^2)
      }
      # le sous-modèle du tour, une composante de plus (moins s'il n'a plus de variation : il
      # prédit alors avec celles qu'il a)
      s <- r$sous
      k <- if (!s$arrete) tryCatch(composante_pls(s$E, s$ME, s$F, s$MF, s$depart), sans_variation = sans)
      s$arrete <- is.null(k)
      if (!s$arrete) {
        s$E <- (s$E - outer(k$t, k$p)) * s$ME
        s$F <- (s$F - outer(k$t, k$c)) * s$MF
        th <- projeter(s$Eh, s$Mh, k$w)
        s$Eh <- (s$Eh - outer(th, k$p)) * s$Mh
        s$prediction <- s$prediction + outer(th, k$c)
      }
      prediction[h, ] <- s$prediction
      r$sous <- s
      vc$tours[[i]] <- r
    }
    vc$predictions[[a]] <- prediction
    ss <- colSums(F^2)
  } else {
    sens <- function(v, ref) { s <- sign(sum(v * ref)); if (s == 0) 1 else s }
    P <- matrix(0, ncol(E), nrow(E))  # le loading de chaque observation, d'après son groupe
    Tc <- matrix(0, nrow(E), ncol(E))  # le score de chaque terme, d'après son groupe
    for (i in seq_along(vc$tours)) {
      h <- vc$tours[[i]]$hors
      k <- tryCatch(composante_acp(E[!h, , drop = FALSE], ME[!h, , drop = FALSE], vc$depart[1]), sans_variation = sans)
      if (is.null(k)) next
      p <- k$p * sens(k$p, comp$p)
      P[, h] <- p
      vc$tours[[i]]$P[, a] <- p
    }
    for (g in sort(unique(vc$groupes_termes))) {
      cases <- vc$groupes_termes == g
      if (all(cases)) next
      k <- tryCatch(composante_acp(E[, !cases, drop = FALSE], ME[, !cases, drop = FALSE], vc$depart[1]), sans_variation = sans)
      if (!is.null(k)) Tc[, cases] <- k$t * sens(k$t, comp$t)
    }
    press <- colSums(((E - Tc * t(P)) * ME)^2)
    ss <- colSums(E^2)
  }
  vc$press <- rbind(vc$press, press, deparse.level = 0)
  vc$ss_a <- rbind(vc$ss_a, ss, deparse.level = 0)
  vc
}

# La somme des carrés à laquelle le PRESS de chaque composante (lignes 1 à a) est rapporté : celle
# des résidus du modèle complet avant elle (centres et poids du modèle complet) ; sinon le PRESS de la composante
# précédente (celle de Z pour la 1re).
ss_validation <- function(vc, a) {
  if (!is.null(vc$depart)) return(vc$ss_a[seq_len(a), , drop = FALSE])
  rbind(vc$ss, vc$press[seq_len(a) - 1, , drop = FALSE], deparse.level = 0)
}

# Des valeurs prédites dans les unités d'un sous-modèle (ses termes centrés et mis à l'échelle,
# bloc `sous`) ramenées dans les unités des données, puis centrées et mises à l'échelle comme le
# modèle complet (bloc `complet`). Un terme constant dans le sous-modèle (poids 0) y est prédit par
# sa valeur constante.
vers_modele_complet <- function(Z, sous, complet) {
  donnees <- sweep(sweep(Z, 2, ifelse(sous$poids > 0, sous$poids, Inf), "/"), 2, sous$centre, "+")
  sweep(sweep(donnees, 2, complet$centre, "-"), 2, complet$poids, "*")
}

# Les résultats de la validation croisée d'un modèle de A composantes : PRESS et SS (une ligne
# par composante ; SS de la composante a = PRESS de la précédente, ou avec les centres et poids
# du modèle complet la somme des
# carrés des résidus du modèle complet avant elle), Q2 et Q2(cum), Q2(cum) de chaque variable
# prédite (q2v_cum), et dans `vc` le Y prédit avec 1, 2... A composantes (dans les unités du modèle
# complet, `predictions`) et les paramètres de chaque sous-modèle (`tours` : W, P, C, une colonne
# par composante ; manquants pour une composante qu'il n'a pas pu avoir), pour les intervalles de
# confiance (jack-knife). Avec les centres et poids du modèle complet, Q2(cum) = 1 - produit des
# (1 - Q2) de ses composantes,
# chaque Q2 compté pour -0,1 au plus bas (ce que donnent les fichiers .usp) ; sinon
# Q2(cum) = 1 - PRESS(A) / SS. `gardes` : les termes qui ont pris le poids du modèle complet dans
# un sous-modèle (debut_validation), avec la part de leur écart-type qu'ils y gardaient.
# Quand les centres sont recalculés (PLS, OPLS), le Q2 du modèle vide (q2_vide : chaque groupe
# caché prédit par la moyenne de Y des autres observations, négatif dès que les groupes diffèrent)
# et le gain du modèle sur lui, (Q2(cum) - q2_vide) / (1 - q2_vide) = 1 - PRESS(A) / PRESS du
# modèle vide : la part de l'erreur de prédiction du modèle vide que le modèle évite. Avec les
# centres et poids du modèle complet, le modèle vide prédit la moyenne de toutes les observations,
# groupe caché compris (son Q2 vaut 0) : ni l'un ni l'autre.
fin_validation <- function(vc, A) {
  press <- vc$press[seq_len(A), , drop = FALSE]
  ss_vc <- ss_validation(vc, A)
  q2 <- 1 - rowSums(press) / rowSums(ss_vc)
  q2v <- ifelse(ss_vc > 0, 1 - press / ss_vc, NA_real_)
  complet <- identical(vc$mode, "complet")
  q2cum <- if (complet) 1 - cumprod(1 - pmax(q2, -0.1)) else 1 - rowSums(press) / sum(vc$ss)
  q2v_cum <- if (complet) 1 - apply(1 - pmax(q2v, -0.1), 2, prod) else ifelse(vc$ss > 0, 1 - press[A, ] / vc$ss, NA_real_)
  q2_vide <- if (!complet && length(vc$press0)) 1 - sum(vc$press0) / sum(vc$ss)
  gain <- if (length(q2_vide) && q2_vide < 1) (q2cum[A] - q2_vide) / (1 - q2_vide)
  niveaux <- sort(unique(vc$groupes))
  gardes <- do.call(rbind, lapply(seq_along(vc$tours), function(i) {
    g <- vc$tours[[i]]$gardes
    if (length(g)) data.frame(groupe = niveaux[i], terme = names(g), part = unname(g), stringsAsFactors = FALSE)
  }))
  colonnes <- function(M) if (!is.null(M)) M[, seq_len(A), drop = FALSE]
  list(press = press, ss_vc = ss_vc, q2 = q2, q2cum = q2cum, q2v_cum = q2v_cum, q2_vide = q2_vide, gain = gain, validation = vc$mode, gardes = gardes,
       vc = list(groupes = vc$groupes, predictions = if (vc$pls) vc$predictions[seq_len(A)],
                 tours = lapply(vc$tours, function(r) list(W = colonnes(r$W), P = colonnes(r$P), C = colonnes(r$C)))))
}

# Les termes qui ont pris le poids du modèle complet dans un sous-modèle de la validation croisée
# du modèle `m` (fin_validation : gardes), avec le nom de leur groupe (« 208 » pour un site, le
# numéro du groupe à tour de rôle) et la part de leur écart-type qu'ils y gardaient, de la plus
# petite à la plus grande ; NULL s'il n'y en a pas.
gardes_vc <- function(projet, d, m) {
  g <- m$gardes
  if (is.null(g) || !nrow(g)) return(NULL)
  noms <- noms_groupes_vc(projet, d)
  g$groupe <- if (is.null(noms)) as.character(g$groupe) else noms[g$groupe]
  g[order(g$part), , drop = FALSE]
}
