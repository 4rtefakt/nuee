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
  vc <- d$groupes_vc
  par <- if (length(vc$by)) as.character(vc$by) else ""
  if (!nzchar(par) || identical(par, "order")) {
    if (!isTRUE(G >= 2)) stop(traduire("La validation croisée demande au moins deux groupes.", "Cross-validation needs at least two groups."), call. = FALSE)
    return(groupes_validation(length(obs), G))
  }
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
  niveaux <- unique(v[!is.na(v)])
  nombres <- suppressWarnings(as.numeric(niveaux))
  niveaux <- if (!anyNA(nombres)) niveaux[order(nombres)] else sort(niveaux)
  groupes <- match(v, niveaux)
  groupes[is.na(groupes)] <- length(niveaux) + 1
  if (length(unique(groupes)) < 2) {
    stop(sprintf(traduire("Validation croisée par « %s » : les observations du modèle n'ont qu'une valeur, donc un seul groupe. Il en faut au moins deux.",
                          "Cross-validation by \"%s\": the model's observations have a single value, so a single group. At least two are needed."),
                 nom_groupes_vc(projet, d)), call. = FALSE)
  }
  groupes
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
  groupes <- groupes_definition(projet, d, G)
  # G : celui de la définition pour des groupes à tour de rôle (même avec moins d'observations que
  # de groupes), sinon le nombre de groupes formés ; les termes (ACP) en font autant
  if (!identical(groupes, groupes_validation(length(obs), G))) G <- max(groupes)
  groupes_termes <- groupes_validation(ncol(X$E), G)
  Z <- if (pls) Y else X  # les variables prédites
  tours <- lapply(sort(unique(groupes)), function(g) {
    hors <- groupes == g
    p <- if (is.null(projets)) projet else projets[[g]]
    v <- if (is.null(projets)) vx else valeurs_bloc(p, d$x, obs)
    sx <- construire_bloc(p, d$x, obs[!hors], poids, spectres = d$spectres, bruts = v[!hors, , drop = FALSE])
    hx <- construire_bloc(p, d$x, obs[hors], imposees = sx, spectres = d$spectres, bruts = v[hors, , drop = FALSE])
    sy <- if (pls) construire_bloc(p, d$y, obs[!hors], poids, facteurs = sx$simples)
    Xs <- preparer(sx$X)
    Ys <- if (pls) preparer(sy$X)
    Xh <- preparer(hx$X)
    r <- list(hors = hors, X = Xs, Y = Ys, E = Xs$E, F = Ys$E, depart = c(sum(Xs$E^2), if (pls) sum(Ys$E^2)),
              sous = if (pls) sy else sx, arrete = FALSE, prediction = matrix(0, sum(hors), ncol(Z$E)),
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
  list(pls = pls, tours = tours, Z = Z, complet = if (pls) by else bx, ss = colSums(Z$E^2), G = G,
       groupes = groupes, groupes_termes = groupes_termes, press = NULL, predictions = list())
}

# La composante `a` de chaque sous-modèle (ACP, PLS), et la ligne de PRESS qu'elle donne.
composante_validation <- function(vc, a) {
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

# Des valeurs prédites dans les unités d'un sous-modèle (ses termes centrés et mis à l'échelle,
# bloc `sous`) ramenées dans les unités des données, puis centrées et mises à l'échelle comme le
# modèle complet (bloc `complet`). Un terme constant dans le sous-modèle (poids 0) y est prédit par
# sa valeur constante.
vers_modele_complet <- function(Z, sous, complet) {
  donnees <- sweep(sweep(Z, 2, ifelse(sous$poids > 0, sous$poids, Inf), "/"), 2, sous$centre, "+")
  sweep(sweep(donnees, 2, complet$centre, "-"), 2, complet$poids, "*")
}

# Les résultats de la validation croisée d'un modèle de A composantes : PRESS et SS (une ligne
# par composante ; SS de la composante a = PRESS de la précédente), Q2 et Q2(cum), et dans `vc` le
# Y prédit avec 1, 2... A composantes (dans les unités du modèle complet, `predictions`) et les
# paramètres de chaque sous-modèle (`tours` : W, P, C, une colonne par composante ; manquants
# pour une composante qu'il n'a pas pu avoir), pour les intervalles de confiance (jack-knife).
fin_validation <- function(vc, A) {
  press <- vc$press[seq_len(A), , drop = FALSE]
  ss_vc <- rbind(vc$ss, press[-A, , drop = FALSE], deparse.level = 0)
  colonnes <- function(M) if (!is.null(M)) M[, seq_len(A), drop = FALSE]
  list(press = press, ss_vc = ss_vc, q2 = 1 - rowSums(press) / rowSums(ss_vc), q2cum = 1 - rowSums(press) / sum(vc$ss),
       vc = list(groupes = vc$groupes, predictions = if (vc$pls) vc$predictions[seq_len(A)],
                 tours = lapply(vc$tours, function(r) list(W = colonnes(r$W), P = colonnes(r$P), C = colonnes(r$C)))))
}
