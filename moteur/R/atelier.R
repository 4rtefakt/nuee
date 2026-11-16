# L'atelier des données (Édition) : ce qu'on faisait dans un tableur avant d'ouvrir le tableau
# dans Nuée.
#   - Chercher et remplacer (remplacer_valeurs) : dans une variable, plusieurs ou toutes, du texte
#     ou des nombres, la cellule entière ou une partie du texte, les majuscules comptant ou non,
#     les cellules vides comme cible (« vide → 0 », « -9999 → vide ») ; une variable qualitative
#     dont toutes les valeurs deviennent des nombres devient quantitative ;
#   - les catégories d'une variable qualitative (changer_categories) : les renommer, en fusionner
#     plusieurs, les ranger ; dans une régression, la dernière catégorie observée est celle de
#     référence (regression.R : candidats_regression), la mettre en dernier la choisit ;
#   - l'unité et la description de chaque variable (decrire_variables), gardées dans le projet
#     (p$variables[[i]]$unite et $description, enregistrées avec lui ; un projet qui ne les a pas
#     s'ouvre comme avant) : titres des axes, vue Données, textes des Méthodes et des résultats,
#     export des données (formats.R : ecrire_tableau ; geopackage.R) et script R (script.R) ;
#   - un bloc de cellules copié dans un tableur, collé dans la vue Données (coller_cellules).
# Chaque changement est un seul retour en arrière (Ctrl+Z ; serveur.R : changer_projet). Les
# modèles déjà ajustés gardent leurs résultats jusqu'à ce qu'on les ajuste à nouveau, comme après
# toute modification des données. Les définitions des modèles du projet suivent leurs catégories
# (suivre_categories) ; un modèle de la séance dont une catégorie a changé de nom, ou disparu, est à
# ajuster à nouveau avant d'être réajusté ou de prédire (aligner_categories).

# ---------------------------------------------------------------- chercher et remplacer

# Le texte de catégories où `cherche` est remplacé par `par` partout où il apparaît (une partie du
# texte : « , » dans « 3,5 »), les majuscules comptant si `casse`, sinon non ; le texte cherché et
# celui qui remplace sont pris tels quels (ni motif ni référence arrière).
remplacer_partie <- function(textes, cherche, par, casse) {
  if (casse) return(gsub(cherche, par, textes, fixed = TRUE))
  motif <- gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", cherche)
  gsub(motif, gsub("\\", "\\\\", par, fixed = TRUE), textes, ignore.case = TRUE)
}

# Une valeur pour les phrases : « foret », « -9999 », ou « vide ».
valeur_ecrite <- function(x, fr = !identical(getOption("nuee.langue"), "en")) {
  if (is.na(x)) return(if (fr) "vide" else "empty")
  if (is.numeric(x)) x <- format(x, digits = 15)
  guillemets_texte(x, fr)
}

# Édition › Chercher et remplacer : dans les variables `variables` (leurs numéros ; toutes sans
# elles), les cellules qui valent `chercher` reçoivent `remplacer`.
#   - `vides` : chercher les cellules vides (les valeurs manquantes) ; `chercher` est ignoré ;
#   - une variable qualitative se compare en texte : sa cellule entière (sans les espaces
#     autour), ou, avec `partie`, `chercher` partout où il apparaît dans le texte (« , » dans
#     « 3,5 »), les majuscules comptant si `casse`, sinon non (« Forêt » vaut alors « forêt ») ;
#   - une variable quantitative se compare en nombres, sur la cellule entière (« -9999 » ;
#     « 3,5 » vaut 3.5 : lire_nombres, comme à l'ouverture d'un tableau) ; la valeur qui remplace
#     y doit être un nombre, ou rien : sinon ses cellules restent telles quelles, comptées
#     (`ignorees`), car un texte n'a pas de place parmi des nombres ;
#   - `remplacer` vide : la cellule devient vide (manquante).
# Une catégorie remplacée par une autre qui existe s'y fond ; un nouveau nom prend sa place dans
# l'ordre des catégories ; une catégorie remplie dans des cellules vides vient en dernier ; une
# catégorie vidée de ses cellules disparaît. Une variable qualitative dont des cellules changent
# et dont toutes les valeurs connues sont alors des nombres (« 3,5 » après « , » → « . », « <0,5 »
# remplacé par « 0,25 ») devient quantitative si `nombres`. Les définitions des modèles suivent
# (suivre_categories). attr(, "remplacement") : par variable (`variables`), son nom, le nombre de
# cellules changées, celles laissées (`ignorees`), ce qu'elle devient (`devient` : "quantitative",
# "qualitative" ou "" quand elle reste ce qu'elle était), son nombre de catégories avant et après,
# les premiers changements (`exemples`) ; le total (`cellules`) et les phrases qui le disent.
remplacer_valeurs <- function(projet, variables = NULL, chercher = "", remplacer = "", vides = FALSE,
                              partie = FALSE, casse = FALSE, nombres = TRUE) {
  fr <- !identical(getOption("nuee.langue"), "en")
  chercher <- as.character(chercher %||% "")[1]
  remplacer <- as.character(remplacer %||% "")[1]
  if (is.na(chercher)) chercher <- ""
  if (is.na(remplacer)) remplacer <- ""
  vides <- isTRUE(vides)
  partie <- isTRUE(partie) && !vides
  casse <- isTRUE(casse)
  # (une partie du texte se cherche telle qu'écrite, espaces comprises ; une cellule entière, sans
  # les espaces autour, comme les catégories à l'ouverture d'un tableau)
  cherche <- if (partie) chercher else trimws(chercher)
  par <- trimws(remplacer)
  if (!vides && !nzchar(cherche)) {
    stop(traduire("Donner le texte ou le nombre à chercher, ou chercher les cellules vides.", "Give the text or number to find, or find the empty cells."), call. = FALSE)
  }
  vars <- Filter(function(v) nom_numero(v$uid) %in% colnames(projet$donnees), projet$variables)
  if (length(variables)) {
    variables <- as.numeric(unlist(variables))
    inconnues <- setdiff(variables, vapply(vars, function(v) as.numeric(v$uid), 0))
    if (length(inconnues)) stop(traduire("Variable introuvable : ", "Variable not found: "), inconnues[1], call. = FALSE)
    vars <- Filter(function(v) as.numeric(v$uid) %in% variables, vars)
  }
  if (!length(vars)) stop(traduire("Aucune variable où chercher.", "No variable to search."), call. = FALSE)
  n_cherche <- if (!vides) lire_nombres(cherche) else NA_real_
  n_par <- if (nzchar(par)) lire_nombres(par) else NA_real_
  infos <- list()
  # (la place de chaque variable dans le projet, par son numéro : un tableau de spectres en a des milliers)
  place <- stats::setNames(seq_along(projet$variables), vapply(projet$variables, function(w) nom_numero(w$uid), ""))
  for (v in vars) {
    k <- nom_numero(v$uid)
    nom <- as.character(v$ids[1])
    i <- place[[k]]
    x <- projet$donnees[, k]
    cats <- as.character(v$categories %||% character(0))
    info <- list(variable = as.numeric(v$uid), nom = nom, cellules = 0L, ignorees = 0L, devient = "",
                 categories_avant = length(cats), categories_apres = length(cats), exemples = character(0))
    if (!length(cats)) {
      # (des nombres se comparent toujours entiers, même pour une partie du texte)
      cible <- if (vides) is.na(x) else if (is.na(n_cherche)) rep(FALSE, length(x))
               else !is.na(x) & abs(x - n_cherche) <= 1e-12 * max(1, abs(n_cherche))
      if (nzchar(par) && is.na(n_par)) {
        info$ignorees <- sum(cible)
      } else if (any(cible)) {
        apres <- x
        apres[cible] <- n_par
        change <- cible & !(is.na(x) & is.na(apres)) & !(!is.na(x) & !is.na(apres) & x == apres)
        info$cellules <- sum(change)
        if (info$cellules) {
          info$exemples <- sprintf("%s → %s (%d)", valeur_ecrite(x[change][1], fr), valeur_ecrite(apres[change][1], fr), info$cellules)
          projet$donnees[, k] <- apres
        }
      }
      infos[[length(infos) + 1]] <- info
      next
    }
    avant <- cats[x]
    if (vides) {
      cible <- is.na(x)
      nouvelles <- cats
      codes <- x
      if (any(cible) && nzchar(par)) {
        if (!par %in% nouvelles) nouvelles <- c(nouvelles, par)
        codes[cible] <- match(par, nouvelles)
      }
      carte <- seq_along(cats)
    } else {
      noms <- if (partie) trimws(remplacer_partie(cats, cherche, remplacer, casse))
              else ifelse(if (casse) cats == cherche else tolower(cats) == tolower(cherche), par, cats)
      nouvelles <- unique(noms[nzchar(noms)])
      carte <- match(noms, nouvelles)  # (NA : une catégorie vidée)
      carte[!nzchar(noms)] <- NA
      codes <- carte[x]
    }
    apres <- nouvelles[codes]
    change <- !(is.na(avant) & is.na(apres)) & !(!is.na(avant) & !is.na(apres) & avant == apres)
    info$cellules <- sum(change)
    if (!info$cellules) {
      infos[[length(infos) + 1]] <- info
      next
    }
    paires <- paste(avant[change], apres[change], sep = "\r")
    comptes <- table(factor(paires, levels = unique(paires)))
    info$exemples <- c(vapply(utils::head(names(comptes), 3), function(cle) {
      j <- match(cle, paires)
      sprintf("%s → %s (%d)", valeur_ecrite(avant[change][j], fr), valeur_ecrite(apres[change][j], fr), comptes[[cle]])
    }, "", USE.NAMES = FALSE), if (length(comptes) > 3) "…")
    connus <- apres[!is.na(apres)]
    en_nombres <- isTRUE(nombres) && length(connus) > 0 && !anyNA(lire_nombres(connus))
    if (en_nombres) {
      projet$donnees[, k] <- lire_nombres(apres)
      projet$variables[[i]]$categories <- NULL
      info$devient <- "quantitative"
      info$categories_apres <- 0L
      projet <- suivre_categories(projet, v$uid, NULL, cats)
    } else {
      projet$donnees[, k] <- codes
      projet$variables[[i]]$categories <- nouvelles
      info$devient <- "qualitative"
      info$categories_apres <- length(nouvelles)
      if (!identical(nouvelles, cats)) projet <- suivre_categories(projet, v$uid, carte, cats)
    }
    infos[[length(infos) + 1]] <- info
  }
  total <- sum(vapply(infos, function(x) as.numeric(x$cellules), 0))
  avec <- Filter(function(x) x$cellules > 0, infos)
  ignorees <- Filter(function(x) x$ignorees > 0, infos)
  phrases <- c(
    if (!total) traduire("Aucune cellule ne change.", "No cell changes.")
    else sprintf(traduire("%d cellule(s) dans %d variable(s).", "%d cell(s) in %d variable(s)."), total, length(avec)),
    vapply(avec, function(x) sprintf(traduire("%s : %s.", "%s: %s."), x$nom, paste(x$exemples, collapse = ", ")), ""),
    vapply(Filter(function(x) x$devient == "quantitative", avec), function(x)
      sprintf(traduire("%s devient quantitative : toutes ses valeurs sont des nombres.", "%s becomes quantitative: all its values are numbers."), guillemets_texte(x$nom, fr)), ""),
    vapply(Filter(function(x) x$devient == "qualitative" && x$categories_apres != x$categories_avant, avec), function(x)
      sprintf(traduire("%s passe de %d à %d catégories.", "%s goes from %d to %d categories."), guillemets_texte(x$nom, fr), x$categories_avant, x$categories_apres), ""),
    vapply(ignorees, function(x) sprintf(traduire("%s est quantitative : ses %d cellule(s) restent telles quelles (remplacer par un nombre, ou par rien).",
                                                  "%s is quantitative: its %d cell(s) stay as they are (replace by a number, or by nothing)."),
                                         guillemets_texte(x$nom, fr), x$ignorees), ""))
  attr(projet, "remplacement") <- list(cellules = total, variables = infos, phrases = phrases)
  projet
}

# Un bloc de cellules copié dans un tableur et collé dans la vue Données, à partir de la case
# choisie : `valeurs`, une matrice de textes (NA : une case laissée telle quelle), ses lignes les
# observations `observations` et ses colonnes les variables `variables`, dans l'ordre de la vue.
# Chaque case comme on la modifie à la main (variables.R : modifier_valeur) : vide, elle devient
# manquante ; dans une variable qualitative, le nom d'une catégorie (une nouvelle s'ajoute) ; dans
# une variable quantitative, un nombre (« 3,5 » compris), un texte étant refusé, nommé, sans que
# rien ne change. attr(, "collage") : le nombre de cases changées.
coller_cellules <- function(projet, observations, variables, valeurs) {
  if (is.list(valeurs) && !is.data.frame(valeurs)) {
    valeurs <- do.call(rbind, lapply(valeurs, function(l) vapply(l, function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x)[1], "")))
  }
  valeurs <- as.matrix(valeurs)
  observations <- as.numeric(unlist(observations))
  variables <- as.numeric(unlist(variables))
  if (nrow(valeurs) != length(observations) || ncol(valeurs) != length(variables)) {
    stop(traduire("Le bloc collé doit avoir une ligne par observation et une colonne par variable.", "The pasted block must have one row per observation and one column per variable."), call. = FALSE)
  }
  # (une colonne à la fois, ses cases comme modifier_valeur les lit, et une seule écriture dans le
  # tableau des données : case par case, chacune en copiait tout le tableau)
  n <- 0L
  lignes <- nom_numero(observations)
  for (j in seq_along(variables)) {
    k <- nom_numero(variables[j])
    a <- which(!is.na(valeurs[, j]))
    if (!length(a)) next
    if (!(k %in% colnames(projet$donnees))) stop(traduire("Variable sans données : ", "Variable without data: "), variables[j])
    iv <- which(vapply(projet$variables, function(v) identical(as.numeric(v$uid), variables[j]), TRUE))[1]
    textes <- trimws(as.character(valeurs[a, j]))
    vides <- !nzchar(textes)
    cats <- as.character(projet$variables[[iv]]$categories %||% character(0))
    if (length(cats)) {
      nouvelles <- setdiff(unique(textes[!vides]), cats)  # (dans leur ordre d'arrivée, comme une à une)
      if (length(nouvelles)) projet$variables[[iv]]$categories <- cats <- c(cats, nouvelles)
      x <- ifelse(vides, NA_real_, match(textes, cats))
    } else {
      x <- suppressWarnings(as.numeric(sub(",", ".", textes, fixed = TRUE)))  # 3,5 ou 3.5
      x[vides] <- NA_real_
      mauvaise <- which(!vides & is.na(x))[1]
      if (!is.na(mauvaise)) {
        stop(sprintf(traduire("%s (observation %s)", "%s (observation %s)"),
                     sprintf(traduire("Valeur non numérique pour %s : %s", "Non-numeric value for %s: %s"), projet$noms_var[[k]], textes[mauvaise]),
                     projet$noms_obs[[lignes[a[mauvaise]]]]), call. = FALSE)
      }
    }
    avant <- projet$donnees[lignes[a], k]
    n <- n + sum(!((is.na(avant) & is.na(x)) | (!is.na(avant) & !is.na(x) & avant == x)))
    projet$donnees[lignes[a], k] <- x
  }
  attr(projet, "collage") <- list(cellules = n)
  projet
}

# ---------------------------------------------------------------- catégories

# Les catégories d'une variable qualitative changées (Édition › Catégories) : `noms`, le nouveau
# nom de chacune de ses catégories, dans leur ordre d'avant (deux catégories du même nom n'en
# font plus qu'une : fusionner « foret » dans « forêt ») ; `ordre`, l'ordre des catégories qui en
# résultent, par leurs noms (par défaut, celui où elles viennent ; une fusion prend la place de la
# première). L'ordre est celui des graphiques et des tableaux ; dans une régression, la dernière
# catégorie observée est la catégorie de référence (regression.R : candidats_regression), celle
# dont les autres coefficients sont l'écart. Un nom vide est refusé (Chercher et remplacer vide
# des cellules). Les observations gardent leur catégorie, sous son nouveau nom ; les définitions
# des modèles du projet suivent (suivre_categories). attr(, "categories") : la variable, ses
# catégories, la catégorie de référence et les phrases qui disent ce qui a changé.
changer_categories <- function(projet, variable, noms, ordre = NULL) {
  fr <- !identical(getOption("nuee.langue"), "en")
  uid <- as.numeric(variable)[1]
  i <- which(vapply(projet$variables, function(v) identical(as.numeric(v$uid), uid), TRUE))[1]
  if (is.na(i) || !nom_numero(uid) %in% colnames(projet$donnees)) stop(traduire("Variable introuvable : ", "Variable not found: "), variable, call. = FALSE)
  v <- projet$variables[[i]]
  nom <- as.character(v$ids[1])
  cats <- as.character(v$categories %||% character(0))
  if (!length(cats)) stop(sprintf(traduire("%s est quantitative : elle n'a pas de catégories.", "%s is quantitative: it has no categories."), guillemets_texte(nom, fr)), call. = FALSE)
  noms <- trimws(gsub("[[:cntrl:]]+", " ", as.character(unlist(noms))))
  if (length(noms) != length(cats)) {
    stop(sprintf(traduire("Donner un nom à chacune des %d catégories de %s.", "Give a name to each of the %d categories of %s."), length(cats), guillemets_texte(nom, fr)), call. = FALSE)
  }
  if (any(is.na(noms) | !nzchar(noms))) {
    stop(traduire("Une catégorie ne peut pas avoir un nom vide (Édition › Chercher et remplacer vide des cellules).",
                  "A category can't have an empty name (Edit › Find and replace empties cells)."), call. = FALSE)
  }
  nouvelles <- unique(noms)
  if (length(ordre)) {
    ordre <- trimws(as.character(unlist(ordre)))
    if (anyDuplicated(ordre) || !setequal(ordre, nouvelles)) {
      stop(sprintf(traduire("L'ordre doit donner chacune des catégories une fois : %s.", "The order must give each category once: %s."),
                   paste(vapply(nouvelles, guillemets_texte, "", fr = fr), collapse = ", ")), call. = FALSE)
    }
    nouvelles <- ordre
  }
  if (identical(nouvelles, cats)) stop(traduire("Les catégories n'ont pas changé.", "The categories didn't change."), call. = FALSE)
  carte <- match(noms, nouvelles)
  x <- projet$donnees[, nom_numero(uid)]
  effectifs <- tabulate(x[!is.na(x)], length(cats))
  projet$donnees[, nom_numero(uid)] <- carte[x]
  projet$variables[[i]]$categories <- nouvelles
  projet <- suivre_categories(projet, uid, carte, cats)
  g <- function(x) guillemets_texte(x, fr)
  fusions <- Filter(function(n) sum(noms == n) > 1, nouvelles)
  renommees <- Filter(function(n) sum(noms == n) == 1 && cats[match(n, noms)] != n, nouvelles)
  # (l'ordre change quand les catégories qui restent ne viennent plus dans l'ordre où leurs anciennes venaient)
  premieres <- vapply(nouvelles, function(n) min(which(noms == n)), 0)
  range <- is.unsorted(premieres)
  phrases <- c(
    vapply(fusions, function(n) sprintf(traduire("%s fusionnées en %s (%d observations).", "%s merged into %s (%d observations)."),
                                        liste_mots(vapply(cats[noms == n], g, ""), fr), g(n), sum(effectifs[noms == n])), ""),
    vapply(renommees, function(n) sprintf(traduire("%s renommée %s.", "%s renamed %s."), g(cats[match(n, noms)]), g(n)), ""),
    if (range) sprintf(traduire("Ordre : %s.", "Order: %s."), paste(nouvelles, collapse = ", ")),
    sprintf(traduire("Dans une régression, la catégorie de référence de %s est la dernière observée : %s.",
                     "In a regression, the reference category of %s is the last one observed: %s."), g(nom), g(utils::tail(nouvelles, 1))))
  attr(projet, "categories") <- list(variable = uid, nom = nom, categories = I(nouvelles), reference = utils::tail(nouvelles, 1), phrases = phrases)
  projet
}

# Le nom d'un terme fait de `variables` et de leurs `categories` (0 : la variable en nombres),
# écrit comme Nouveau modèle l'écrit (nouveau.R : termes_variable, definir_modele) : « habitat(forêt) »,
# « annee*secteur(S2) ».
nom_terme <- function(projet, variables, categories) {
  paste(vapply(seq_along(variables), function(j) {
    v <- variable_du_projet(projet, variables[j])
    nom <- as.character(v$ids[1])
    if (categories[j] != 0) sprintf("%s(%s)", nom, as.character(v$categories)[categories[j]]) else nom
  }, ""), collapse = "*")
}

# Les termes d'une définition après un changement des catégories de la variable `uid` : chaque
# facteur d'une de ses catégories prend son nouveau numéro, `carte[ancien numéro]` (NA : la
# catégorie a disparu, et le terme avec elle ; `carte` NULL : la variable est devenue
# quantitative, le facteur la prend en nombres), et le terme son nouveau nom ; avec `dedoubler`,
# un terme devenu le même qu'un autre (deux catégories fusionnées) s'en va, et les termes des
# catégories de la variable, à leurs places, viennent dans l'ordre des catégories (ceux d'un même
# terme croisé entre eux) : dans une régression, c'est l'ordre des termes qui laisse de côté la
# dernière catégorie, celle de référence (regression.R : candidats_regression, termes_utiles).
# Sans `dedoubler` (un modèle déjà ajusté, aligner_categories), les termes gardent leurs places,
# celles des colonnes du modèle. attr(, "change") : si quelque chose a changé.
suivre_termes <- function(termes, projet, uid, carte, dedoubler = TRUE) {
  cats_terme <- function(t) vapply(seq_along(t$variables), function(j) as.numeric(categorie(t, j)), 0)
  cle <- function(t) paste(c(as.numeric(t$variables), cats_terme(t)), collapse = " ")
  vues <- character(0)
  garde <- list()
  change <- FALSE
  for (t in termes) {
    vs <- as.numeric(t$variables)
    cats <- cats_terme(t)
    j <- which(vs == uid & cats != 0)
    if (length(j)) {
      avant <- cats
      cats[j] <- if (is.null(carte)) 0 else carte[cats[j]]
      if (anyNA(cats)) {
        change <- TRUE
        next
      }
      nom <- if (length(t$lags)) t$name else nom_terme(projet, vs, cats)
      if (!identical(cats, avant) || !identical(as.character(nom), as.character(t$name))) change <- TRUE
      t$categories <- if (length(vs) == 1) cats[1] else cats
      t$name <- nom
      if (dedoubler && cle(t) %in% vues) {
        change <- TRUE
        next
      }
    }
    vues <- c(vues, cle(t))
    garde[[length(garde) + 1]] <- t
  }
  if (dedoubler && length(garde) > 1) {
    # (chaque famille : les termes qui ne diffèrent que par la catégorie de la variable)
    famille <- vapply(garde, function(t) {
      vs <- as.numeric(t$variables)
      cats <- cats_terme(t)
      if (!any(vs == uid & cats != 0)) return(NA_character_)
      cats[vs == uid & cats != 0] <- -1
      paste(c(vs, cats), collapse = " ")
    }, "")
    for (f in unique(famille[!is.na(famille)])) {
      k <- which(famille %in% f)
      rang <- vapply(garde[k], function(t) { cats <- cats_terme(t); cats[as.numeric(t$variables) == uid & cats != 0][1] }, 0)
      if (is.unsorted(rang)) {
        garde[k] <- garde[k][order(rang)]
        change <- TRUE
      }
    }
  }
  structure(garde, change = change)
}

# Les définitions des modèles du projet (p$models) après un changement des catégories de la
# variable `uid` (`carte` : suivre_termes ; `anciennes` : ses catégories d'avant) : leurs termes X
# et Y (les classes d'une PLS-DA) et ceux du modèle complet d'une sélection par AIC ; le nom de la
# classe d'un modèle de classe ; les catégories que nomme leur condition d'observations. Une définition changée n'est plus celle d'un fichier .usp : sa
# solution enregistrée part, comme quand on la réajuste sans des observations (serveur.R).
suivre_categories <- function(projet, uid, carte, anciennes) {
  uid <- as.numeric(uid)
  v <- variable_du_projet(projet, uid)
  nouvelles <- as.character(v$categories %||% character(0))
  # (l'ancien nom de chaque catégorie → son nouveau, pour les conditions d'observations)
  renommer <- if (!is.null(carte)) stats::setNames(nouvelles[carte], anciennes)
  renommer <- renommer[!is.na(renommer) & renommer != names(renommer)]
  projet$models <- lapply(projet$models, function(d) {
    change <- FALSE
    if (length(renommer) && avec_condition(d)) {
      condition <- suivre_condition(d$condition, as.character(v$ids[1]), renommer)
      if (!is.null(condition)) {
        d$condition <- condition
        change <- TRUE
      }
    }
    for (champ in c("x", "y", "termes_complets")) {
      if (!length(d[[champ]])) next
      r <- suivre_termes(d[[champ]], projet, uid, carte)
      if (isTRUE(attr(r, "change"))) {
        attr(r, "change") <- NULL
        d[[champ]] <- r
        change <- TRUE
      }
    }
    g <- d$groupe_classes
    if (!is.null(g) && identical(as.numeric(g$variable), uid) && !is.null(carte)) {
      k <- carte[match(as.character(g$classe), anciennes)]
      if (!is.na(k) && !identical(nouvelles[k], as.character(g$classe))) {
        d$groupe_classes$classe <- nouvelles[k]
        change <- TRUE
      }
    }
    if (change) {
      d$solution <- NULL
      d$reference <- list(ynames = d[["reference"]][["ynames"]], note = d[["reference"]][["note"]])
    }
    d
  })
  projet
}

# La condition d'observations d'un modèle (condition.R) après un changement des catégories de la
# variable `nom` : ses comparaisons de la variable à des catégories (nom == "a", "a" != nom,
# nom %in% c("a", "b")) suivent leurs nouveaux noms (`renommer` : nouveau nom, nommé par l'ancien).
# Le texte réécrit (ses espaces peuvent changer), ou NULL quand rien n'y change.
suivre_condition <- function(texte, nom, renommer) {
  e <- tryCatch(str2lang(texte_condition(texte)), error = function(err) NULL)
  if (is.null(e)) return(NULL)
  change <- FALSE
  variable <- function(x) is.name(x) && identical(as.character(x), nom)
  categories <- function(x) {  # (un texte, ou c() de textes)
    if (is.character(x)) {
      y <- ifelse(x %in% names(renommer), unname(renommer[x]), x)
      if (!identical(y, x)) change <<- TRUE
      return(y)
    }
    if (is.call(x) && identical(x[[1]], as.name("c"))) for (i in seq_along(x)[-1]) if (!is.null(x[[i]])) x[[i]] <- categories(x[[i]])
    x
  }
  marcher <- function(x) {
    if (!is.call(x)) return(x)
    f <- as.character(x[[1]])[1]
    if (length(x) == 3 && f %in% c("==", "!=")) {
      if (variable(x[[2]])) x[[3]] <- categories(x[[3]]) else if (variable(x[[3]])) x[[2]] <- categories(x[[2]])
    } else if (length(x) == 3 && identical(f, "%in%") && variable(x[[2]])) {
      x[[3]] <- categories(x[[3]])
    }
    for (i in seq_along(x)[-1]) if (!is.null(x[[i]]) && !identical(x[[i]], quote(expr = ))) x[[i]] <- marcher(x[[i]])
    x
  }
  e <- marcher(e)
  if (change) paste(deparse(e, width.cutoff = 500L), collapse = " ")
}

# Les catégories des variables qualitatives d'une définition, telles qu'elles sont dans le projet
# (gardées avec un modèle de la séance quand il est ajusté : serveur.R, ajuster_et_garder), par
# numéro de variable ; une liste vide sans variable qualitative.
categories_definition <- function(projet, d) {
  uids <- unique(unlist(lapply(c(d$x, d$y, d$termes_complets), function(t) {
    vs <- as.numeric(t$variables)
    vs[vapply(seq_along(vs), function(j) categorie(t, j) != 0, TRUE)]
  })))
  vus <- lapply(uids, function(u) {
    v <- tryCatch(variable_du_projet(projet, u), error = function(e) NULL)
    as.character(v$categories %||% character(0))
  })
  stats::setNames(vus, nom_numero(uids))
}

# Une définition ajustée avec les catégories `vues` (categories_definition) rapportée aux
# catégories du projet d'aujourd'hui : un terme d'une catégorie prend le numéro qu'elle a
# maintenant (l'ordre a changé, une catégorie s'est ajoutée ; un retour en arrière les remet). Une
# catégorie qui n'est plus là sous son nom (renommée, fusionnée, vidée), ou une variable qui n'est
# plus qualitative : le modèle est à ajuster à nouveau ; attr(, "perimee") le dit, et la définition
# est rendue telle quelle.
aligner_categories <- function(projet, d, vues) {
  fr <- !identical(getOption("nuee.langue"), "en")
  aligne <- d
  for (u in names(vues)) {
    v <- tryCatch(variable_du_projet(projet, as.numeric(u)), error = function(e) NULL)
    actuelles <- as.character(v$categories %||% character(0))
    avant <- as.character(vues[[u]])
    if (identical(actuelles, avant)) next
    carte <- match(avant, actuelles)
    if (is.null(v) || !length(actuelles) || anyNA(carte)) {
      nom <- if (is.null(v)) u else as.character(v$ids[1])
      return(structure(d, perimee = sprintf(traduire("Les catégories de %s ont changé depuis l'ajustement de %s : ajustez-le à nouveau (Modèle › Modifier le modèle, ou un double-clic sur sa définition dans l'explorateur).",
                                                     "The categories of %s changed since %s was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer)."),
                                            guillemets_texte(nom, fr), as.character(d$name %||% ""))))
    }
    for (champ in c("x", "y", "termes_complets")) {
      if (!length(aligne[[champ]])) next
      r <- suivre_termes(aligne[[champ]], projet, as.numeric(u), carte, dedoubler = FALSE)
      attr(r, "change") <- NULL
      aligne[[champ]] <- r
    }
  }
  aligne
}

# ---------------------------------------------------------------- unités et descriptions

# L'unité et la description des variables (Édition › Variables, ou l'en-tête d'une colonne de la
# vue Données) : `descriptions`, une liste de list(variable = numéro, unite = texte,
# description = texte), ou un data.frame de ces colonnes (le JSON d'une liste d'objets) ; un champ
# absent (ou NA) reste tel quel, un texte vide l'efface. Sans retour à la ligne (une unité finit
# dans un titre d'axe, une description dans un commentaire du script R), 60 et 1000 caractères au
# plus.
decrire_variables <- function(projet, descriptions) {
  if (is.data.frame(descriptions)) descriptions <- lapply(seq_len(nrow(descriptions)), function(r) as.list(descriptions[r, , drop = FALSE]))
  if (!length(descriptions)) stop(traduire("Aucune variable à décrire.", "No variable to describe."), call. = FALSE)
  propre <- function(x, max) {
    x <- trimws(gsub("[[:cntrl:]]+", " ", as.character(x)[1]))
    if (nchar(x) > max) x <- substr(x, 1, max)
    x
  }
  change <- FALSE
  for (e in descriptions) {
    u <- as.numeric(e$variable)[1]
    i <- which(vapply(projet$variables, function(v) identical(as.numeric(v$uid), u), TRUE))[1]
    if (is.na(i)) stop(traduire("Variable introuvable : ", "Variable not found: "), e$variable, call. = FALSE)
    for (champ in c("unite", "description")) {
      valeur <- e[[champ]]
      if (is.null(valeur) || !length(valeur) || is.na(valeur[1])) next
      valeur <- propre(valeur, if (champ == "unite") 60 else 1000)
      avant <- as.character(projet$variables[[i]][[champ]] %||% "")
      if (identical(valeur, avant)) next
      projet$variables[[i]][[champ]] <- if (nzchar(valeur)) valeur  # (vide : le champ part du projet)
      change <- TRUE
    }
  }
  if (!change) stop(traduire("Les unités et les descriptions n'ont pas changé.", "The units and descriptions didn't change."), call. = FALSE)
  projet
}

# L'unité d'une variable (son numéro, ou son nom), "" sans unité ; et la même entre parenthèses
# après une espace, pour un texte : « densite (ind./ha) ».
unite_variable <- function(projet, variable) {
  for (v in projet$variables) {
    if (identical(as.numeric(v$uid), suppressWarnings(as.numeric(variable))) || identical(as.character(v$ids[1]), as.character(variable))) {
      return(as.character(v$unite %||% ""))
    }
  }
  ""
}
unite_texte <- function(projet, variable) {
  u <- unite_variable(projet, variable)
  if (nzchar(u)) sprintf(" (%s)", u) else ""
}

# Les unités et descriptions des variables qui en ont, pour l'export des données (une feuille
# « Variables » d'un classeur, une table d'un GeoPackage) : un data.frame variable, unité,
# description (les noms des colonnes dans la langue de l'interface), NULL quand aucune n'en a.
descriptions_variables <- function(projet) {
  vars <- Filter(function(v) nom_numero(v$uid) %in% colnames(projet$donnees) && (nzchar(v$unite %||% "") || nzchar(v$description %||% "")), projet$variables)
  if (!length(vars)) return(NULL)
  df <- data.frame(variable = vapply(vars, function(v) as.character(v$ids[1]), ""),
                   unite = vapply(vars, function(v) as.character(v$unite %||% ""), ""),
                   description = vapply(vars, function(v) as.character(v$description %||% ""), ""), stringsAsFactors = FALSE)
  names(df) <- c("variable", traduire("unité", "unit"), "description")
  df
}

# Les unités et descriptions d'un tableau de variables relu (descriptions_variables, écrit par
# l'export : une feuille « Variables », une table d'un GeoPackage) rendues aux variables du projet
# du même nom ; les autres lignes sont laissées.
appliquer_descriptions <- function(projet, tableau) {
  if (is.null(tableau) || !nrow(tableau)) return(projet)
  noms <- tolower(trimws(names(tableau)))
  col <- function(cands) { k <- which(noms %in% cands)[1]; if (is.na(k)) rep(NA_character_, nrow(tableau)) else as.character(tableau[[k]]) }
  variable <- col("variable")
  unite <- col(c("unité", "unite", "unit", "units"))
  description <- col("description")
  for (r in seq_len(nrow(tableau))) {
    i <- which(vapply(projet$variables, function(v) identical(as.character(v$ids[1]), variable[r]), TRUE))[1]
    if (is.na(i)) next
    for (champ in c("unite", "description")) {
      x <- trimws(gsub("[[:cntrl:]]+", " ", c(unite = unite[r], description = description[r])[[champ]]))
      if (!is.na(x) && nzchar(x)) projet$variables[[i]][[champ]] <- x
    }
  }
  projet
}

# Une feuille ou une table de variables écrite par l'export des données (descriptions_variables) :
# son nom (« Variables », comme Nuée l'écrit), et des colonnes variable, unité (ou unit) et
# description.
est_tableau_variables <- function(nom, tableau) {
  identical(tolower(trimws(nom)), "variables") && is.data.frame(tableau) &&
    all(c("variable", "description") %in% tolower(trimws(names(tableau)))) && any(tolower(trimws(names(tableau))) %in% c("unité", "unite", "unit", "units"))
}

# La table des variables d'un classeur Excel ou d'un GeoPackage écrit par l'export des données (une
# feuille ou une table « Variables » à côté d'au moins un autre tableau : est_tableau_variables),
# ou NULL.
tableau_variables_fichier <- function(chemin) {
  ext <- extension(chemin)
  tryCatch({
    if (ext %in% c("xlsx", "xlsm", "xls")) {
      tables <- readxl::excel_sheets(chemin)
      nom <- tables[tolower(tables) == "variables"][1]
      if (is.na(nom) || length(tables) < 2) return(NULL)
      t <- as.data.frame(readxl::read_excel(chemin, sheet = nom, col_types = "text"), stringsAsFactors = FALSE)
    } else if (ext == "gpkg") {
      tables <- couches_geopackage(chemin)
      nom <- tables[tolower(tables) == "variables"][1]
      if (is.na(nom) || length(tables) < 2) return(NULL)
      t <- lire_geopackage(chemin, nom)
    } else {
      return(NULL)
    }
    if (est_tableau_variables(nom, t)) t else NULL
  }, error = function(e) NULL)
}
