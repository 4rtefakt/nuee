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
#   - un bloc de cellules copié dans un tableur, collé dans la vue Données (coller_cellules) ;
#   - une variable renommée, supprimée, ou traitée en catégories ou en nombres (renommer_variable,
#     supprimer_variable, changer_type_variable), d'un clic droit sur l'en-tête de sa colonne.
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
  # (une observation que le projet n'a pas : R écrirait « subscript out of bounds », ou ajouterait sa ligne)
  inconnues <- observations[!lignes %in% rownames(projet$donnees)]
  if (length(inconnues)) stop(traduire("Observation inconnue : ", "Unknown observation: "), paste(utils::head(inconnues, 5), collapse = ", "), call. = FALSE)
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
# numéro de variable ; une liste vide sans variable qualitative. Sa classe aussi (d$classe : une
# LDA, un arbre ou une forêt de classement, des espèces indicatrices, la classe d'une PERMANOVA),
# qui n'est pas faite de termes : un nom changé ou une fusion de ses catégories le rend à ajuster à
# nouveau, comme pour une variable X.
categories_definition <- function(projet, d) {
  classe <- as.numeric(d$classe)
  uids <- unique(c(unlist(lapply(c(d$x, d$y, d$termes_complets), function(t) {
    vs <- as.numeric(t$variables)
    vs[vapply(seq_along(vs), function(j) categorie(t, j) != 0, TRUE)]
  })), classe[!is.na(classe)]))
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

# Les catégories de chaque variable qualitative du projet, dans leur ordre, par numéro de variable
# (gardées avec un modèle de la séance quand il est ajusté : serveur.R, ajuster_et_garder).
ordres_categories <- function(projet) {
  q <- Filter(function(v) length(v$categories) > 0, projet$variables)
  stats::setNames(lapply(q, function(v) as.character(v$categories)), vapply(q, function(v) as.character(v$uid), ""))
}

# Le projet tel qu'un modèle l'a vu (`ordres` : ordres_categories à son ajustement) : les catégories
# de chaque variable dans l'ordre d'alors, celles venues depuis après elles ; une variable dont une
# catégorie d'alors n'est plus là reste telle qu'elle est (le modèle est alors à ajuster à nouveau
# s'il la prend : aligner_categories). Un nouvel ordre ne rend pas un modèle à ajuster à nouveau,
# mais les résultats d'une autre analyse suivent l'ordre des catégories (les classes d'une LDA et
# ses coefficients, les centroïdes d'une RDA ou d'une PCoA, les sites et covariables de TRIM, les
# strates d'un échantillonnage par distances), et un arbre ou une forêt de classement en dépendent :
# son export vers R s'écrit sur ce projet, avec la définition de son ajustement, et redonne ainsi
# ses résultats (serveur.R : script_demande).
projet_tel_ajuste <- function(projet, ordres) {
  uids <- vapply(projet$variables, function(v) as.character(v$uid), "")
  for (u in intersect(names(ordres), colnames(projet$donnees))) {
    i <- match(u, uids)
    actuelles <- as.character(projet$variables[[i]]$categories %||% character(0))
    vues <- as.character(ordres[[u]])
    if (!length(actuelles) || !all(vues %in% actuelles)) next
    ordre <- c(vues, setdiff(actuelles, vues))
    if (identical(ordre, actuelles)) next
    projet$donnees[, u] <- match(actuelles, ordre)[projet$donnees[, u]]
    projet$variables[[i]]$categories <- ordre
  }
  projet
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

# ---------------------------------------------------------------- une variable renommée, supprimée, d'un autre type

# Le code R `texte` (une condition d'observations, une formule de Nouvelle variable) où le nom de
# variable `ancien` devient `nouveau`, entre `accents graves` quand ce n'est pas un nom de R : seuls ses
# symboles changent, ni ses textes (« site == "S1" » garde son "S1"), ni ce qui suit $, @ ou ::, ni les
# noms des arguments ou des fonctions ; le reste est gardé tel qu'il est écrit. NULL quand le code ne se
# lit pas, ou que rien n'y change. (Chaque morceau est retrouvé dans le texte à la suite du précédent :
# les colonnes que donne l'analyse de R comptent une tabulation jusqu'au taquet suivant.)
renommer_symbole <- function(texte, ancien, nouveau) {
  texte <- premier_texte(texte)
  if (!nzchar(texte) || !grepl(ancien, texte, fixed = TRUE)) return(NULL)
  pd <- tryCatch(utils::getParseData(parse(text = texte, keep.source = TRUE, encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(NULL)
  pd <- pd[pd$terminal, , drop = FALSE]
  pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
  n <- nrow(pd)
  avant <- c("", pd$token[-n])
  cibles <- which(pd$token == "SYMBOL" & gsub("^`|`$", "", pd$text) == ancien & !avant %in% c("'$'", "'@'", "NS_GET", "NS_GET_INT"))
  if (!length(cibles)) return(NULL)
  debuts <- integer(n)
  depuis <- 1L
  for (i in seq_len(n)) {
    k <- regexpr(pd$text[i], substring(texte, depuis), fixed = TRUE)
    if (k < 0) return(NULL)
    debuts[i] <- depuis + as.integer(k) - 1L
    depuis <- debuts[i] + nchar(pd$text[i])
  }
  ecrit <- if (identical(make.names(nouveau), nouveau)) nouveau else paste0("`", gsub("`", "\\`", nouveau, fixed = TRUE), "`")
  for (i in rev(cibles)) texte <- paste0(substr(texte, 1L, debuts[i] - 1L), ecrit, substring(texte, debuts[i] + nchar(pd$text[i])))
  texte
}

# Les variables qu'une condition d'observations nomme (ses symboles).
noms_condition_definition <- function(d) {
  if (!avec_condition(d)) return(character(0))
  e <- tryCatch(str2lang(texte_condition(d$condition)), error = function(err) NULL)
  if (is.null(e)) character(0) else all.vars(e)
}

# Les modèles du projet qui utilisent la variable `uid` (de nom `nom`) : `definitions`, ceux dont la
# définition l'utilise (etapes.R : variables_definition) ou dont la condition d'observations la nomme,
# sauf les modèles de code R, à part (`code`) : leur code la nomme (code.R : les colonnes relevées dans
# le code), et il n'est jamais réécrit.
modeles_variable <- function(projet, uid, nom) {
  uid <- as.numeric(uid)
  if (!length(projet$models)) return(list(definitions = character(0), code = character(0)))
  utilise <- function(d) uid %in% variables_definition(d) || nom %in% noms_condition_definition(d)
  code <- vapply(projet$models, function(d) identical(premier_texte(d$type), "CODE"), TRUE)
  avec <- vapply(projet$models, utilise, TRUE)
  noms <- vapply(projet$models, function(d) premier_texte(d$name), "")
  list(definitions = noms[avec & !code], code = noms[avec & code])
}

# Les termes de la variable `uid` parmi `termes` ; et si une définition l'a ailleurs qu'en X ou en Y : la
# classe (PLS-DA, LDA, arbre...), une courbe, un décalage, un lissage spatial, un effet aléatoire, des
# groupes de validation croisée, des réglages d'une autre analyse (changer_type_variable).
termes_de <- function(termes, uid) Filter(function(t) uid %in% as.numeric(t$variables), termes %||% list())
utilise_hors_termes <- function(d, uid) {
  sans <- d
  sans[intersect(c("x", "y", "termes_complets"), names(sans))] <- NULL
  uid %in% variables_definition(sans)
}

# Les définitions des modèles du projet après le nom changé de la variable `uid` (`ancien`, et
# `nouveau` qu'elle porte maintenant dans `projet`) : le nom de leurs termes X et Y (et de ceux du modèle
# complet d'une sélection par l'AIC), écrit comme Nouveau modèle l'écrit (nom_terme ; un terme aux
# décalages, d'un fichier .usp, garde le sien), leur condition d'observations (renommer_symbole), et les
# réglages gardés qui la nomment : le terme de la puissance pour une tendance, les variables de la
# dépendance partielle d'une forêt. attr(, "termes") : pour chaque modèle (par son nom), le nouveau nom
# de chacun de ses termes renommés, nommé par l'ancien (les références des documents les suivent :
# serveur.R, renommer_variable_demande) ; attr(, "suivis") : les modèles qui ont changé.
suivre_renommage <- function(projet, uid, ancien, nouveau) {
  uid <- as.numeric(uid)
  termes <- list()
  suivis <- character(0)
  projet$models <- lapply(projet$models, function(d) {
    carte <- character(0)
    change <- FALSE
    for (champ in c("x", "y", "termes_complets")) {
      if (!length(d[[champ]])) next
      d[[champ]] <- lapply(d[[champ]], function(t) {
        vs <- as.numeric(t$variables)
        if (!uid %in% vs || length(t$lags)) return(t)
        nom <- nom_terme(projet, vs, vapply(seq_along(vs), function(j) as.numeric(categorie(t, j)), 0))
        if (!identical(nom, as.character(t$name))) {
          carte[[as.character(t$name)]] <<- nom
          t$name <- nom
        }
        t
      })
    }
    if (avec_condition(d)) {
      cond <- renommer_symbole(d$condition, ancien, nouveau)
      if (!is.null(cond)) {
        d$condition <- cond
        change <- TRUE
      }
    }
    terme <- premier_texte(d$puissance_tendance$reglages$terme)
    if (nzchar(terme) && terme %in% names(carte)) d$puissance_tendance$reglages$terme <- carte[[terme]]
    if (length(d$dependance)) {
      dep <- as.character(unlist(d$dependance))
      d$dependance <- ifelse(dep == ancien, nouveau, dep)
    }
    if (length(carte) || change) suivis <<- c(suivis, premier_texte(d$name))
    if (length(carte)) termes[[premier_texte(d$name)]] <<- carte
    d
  })
  structure(projet, termes = termes, suivis = suivis)
}

# Le nom d'une variable changé (un clic droit sur l'en-tête de sa colonne › Renommer…) : `nom`, sans
# retour à la ligne ni espaces autour, 200 caractères au plus ; refusé vide, quand une autre variable ou
# un identifiant des observations le porte (une condition ou une formule ne saurait plus lequel elle
# lit), ou quand il commence par $ (les scores d'un modèle en variables : $M1.t1). Les définitions des
# modèles du projet suivent (suivre_renommage), et les formules des variables calculées qui la lisent
# (leur `formula`, que la mise à jour refait). attr(, "renommage") : la variable, son ancien nom, le
# nouveau, les modèles qui ont suivi, ceux de code R qui la nomment dans leur code (il n'est pas
# réécrit), les variables calculées dont la formule a suivi, et les phrases qui le disent.
renommer_variable <- function(projet, variable, nom) {
  fr <- !identical(getOption("nuee.langue"), "en")
  g <- function(x) guillemets_texte(x, fr)
  uid <- as.numeric(variable)[1]
  i <- which(vapply(projet$variables, function(v) identical(as.numeric(v$uid), uid), TRUE))[1]
  if (is.na(i) || !nom_numero(uid) %in% colnames(projet$donnees)) stop(traduire("Variable introuvable : ", "Variable not found: "), premier_texte(variable), call. = FALSE)
  ancien <- as.character(projet$variables[[i]]$ids[1])
  nom <- trimws(gsub("[[:cntrl:]]+", " ", premier_texte(nom)))
  if (!nzchar(nom)) stop(traduire("Donner un nom à la variable.", "Give the variable a name."), call. = FALSE)
  if (nchar(nom) > 200) stop(traduire("Un nom de variable a 200 caractères au plus.", "A variable's name has 200 characters at most."), call. = FALSE)
  if (identical(nom, ancien)) stop(traduire("Le nom n'a pas changé.", "The name didn't change."), call. = FALSE)
  if (startsWith(nom, "$")) {
    stop(traduire("Un nom qui commence par $ est celui des scores d'un modèle en variables ($M1.t1) : choisissez-en un autre.",
                  "A name that starts with $ is that of a model's scores as variables ($M1.t1): choose another one."), call. = FALSE)
  }
  if (nom %in% vapply(projet$variables[-i], function(v) as.character(v$ids[1]), "")) {
    stop(sprintf(traduire("Une variable s'appelle déjà %s.", "A variable is already called %s."), g(nom)), call. = FALSE)
  }
  if (nom %in% as.character(unlist(projet$observation_id_names %||% character(0)))) {
    stop(sprintf(traduire("%s est le nom d'un identifiant des observations : une condition ou une formule ne saurait plus lequel elle lit.",
                          "%s is the name of an identifier of the observations: a condition or a formula would no longer know which one it reads."), g(nom)), call. = FALSE)
  }
  code <- modeles_variable(projet, uid, ancien)$code
  projet$variables[[i]]$ids[1] <- nom
  projet$noms_var[nom_numero(uid)] <- nom
  formules <- character(0)
  for (j in seq_along(projet$variables)) {
    v <- projet$variables[[j]]
    if (!identical(premier_texte(v$origine$type), "formule")) next
    f <- renommer_symbole(v$formula, ancien, nom)
    if (is.null(f)) next
    projet$variables[[j]]$formula <- f
    formules <- c(formules, as.character(v$ids[1]))
  }
  projet <- suivre_renommage(projet, uid, ancien, nom)
  suivis <- setdiff(attr(projet, "suivis"), code)
  phrases <- c(sprintf(traduire("%s renommée %s.", "%s renamed %s."), g(ancien), g(nom)),
               if (length(suivis)) sprintf(traduire("Les définitions de %s suivent.", "The definitions of %s follow."), liste_mots(suivis, fr)),
               if (length(formules)) sprintf(traduire("La formule de %s suit.", "The formula of %s follows."), liste_mots(vapply(formules, g, ""), fr)),
               vapply(code, function(m) sprintf(traduire("%s (code R) nomme %s dans son code : changez-le (il n'est pas réécrit).",
                                                         "%s (R code) names %s in its code: change it there (it isn't rewritten)."), m, g(ancien)), ""))
  attr(projet, "renommage") <- list(variable = uid, ancien = ancien, nom = nom, modeles = I(suivis), code = I(code), formules = I(formules), phrases = I(phrases))
  projet
}

# Une variable supprimée (un clic droit sur l'en-tête de sa colonne › Supprimer) : sa colonne, sa place
# dans les jeux de données, les coordonnées quand elle en est une. Refusée quand un modèle du projet
# l'utilise (sa définition ne s'ajusterait plus, et son numéro pourrait revenir à une autre variable) :
# modifier ou supprimer le modèle d'abord ; `modeles` FALSE (une étape rejouée sur des données sans
# modèles) ne le regarde pas. Un modèle de code R qui la nomme dans son code ne l'empêche pas : il est
# nommé, son code est à changer. attr(, "suppression") : la variable, son nom, ces modèles, les variables
# calculées dont la formule la lit, si elle était une coordonnée, et les phrases qui le disent.
supprimer_variable <- function(projet, variable, modeles = TRUE) {
  fr <- !identical(getOption("nuee.langue"), "en")
  g <- function(x) guillemets_texte(x, fr)
  uid <- as.numeric(variable)[1]
  i <- which(vapply(projet$variables, function(v) identical(as.numeric(v$uid), uid), TRUE))[1]
  k <- nom_numero(uid)
  if (is.na(i) || !k %in% colnames(projet$donnees)) stop(traduire("Variable introuvable : ", "Variable not found: "), premier_texte(variable), call. = FALSE)
  nom <- as.character(projet$variables[[i]]$ids[1])
  if (ncol(projet$donnees) <= 1) stop(traduire("C'est la seule variable du projet : elle ne se supprime pas.", "It's the project's only variable: it can't be deleted."), call. = FALSE)
  u <- if (modeles) modeles_variable(projet, uid, nom) else list(definitions = character(0), code = character(0))
  if (length(u$definitions)) {
    stop(sprintf(traduire("%s utilise %s : modifiez-le ou supprimez-le d'abord (Modèle › Modifier le modèle).",
                          "%s uses %s: modify or delete it first (Model › Modify model)."), liste_mots(u$definitions, fr), g(nom)), call. = FALSE)
  }
  formules <- as.character(unlist(lapply(projet$variables[-i], function(v) {
    if (!identical(premier_texte(v$origine$type), "formule")) return(NULL)
    e <- tryCatch(parse(text = premier_texte(v$formula), keep.source = FALSE), error = function(err) NULL)
    if (!is.null(e) && nom %in% all.vars(e)) as.character(v$ids[1])
  })))
  co <- projet$coordonnees
  coordonnee <- is.list(co) && uid %in% suppressWarnings(as.numeric(c(unlist(co$x), unlist(co$y))))
  projet$donnees <- projet$donnees[, colnames(projet$donnees) != k, drop = FALSE]
  projet$variables <- projet$variables[-i]
  projet$noms_var <- projet$noms_var[names(projet$noms_var) != k]
  projet$datasets <- lapply(projet$datasets, function(d) {
    d$variables <- d$variables[!as.numeric(unlist(d$variables)) %in% uid]
    d
  })
  if (coordonnee) projet["coordonnees"] <- list(NULL)
  phrases <- c(sprintf(traduire("%s supprimée.", "%s deleted."), g(nom)),
               if (coordonnee) traduire("C'était une coordonnée : le projet n'en a plus (la carte les demande de nouveau).",
                                        "It was a coordinate: the project has none any more (the map asks for them again)."),
               if (length(formules)) sprintf(traduire("%s, calculée(s) d'après elle, garde(nt) ses valeurs, mais sa formule ne se refait plus.",
                                                      "%s, computed from it, keep(s) its values, but its formula can't be computed again."), liste_mots(vapply(formules, g, ""), fr)),
               vapply(u$code, function(m) sprintf(traduire("%s (code R) nomme %s dans son code : changez-le (il n'est pas réécrit).",
                                                           "%s (R code) names %s in its code: change it there (it isn't rewritten)."), m, g(nom)), ""))
  attr(projet, "suppression") <- list(variable = uid, nom = nom, code = I(u$code), formules = I(formules), coordonnee = coordonnee, phrases = I(phrases))
  projet
}

# Les termes d'une définition après que la variable `uid` est passée en catégories : chaque terme où elle
# est en nombres (catégorie 0) devient un terme par catégorie, dans leur ordre, comme Nouveau modèle les
# fait pour une variable qualitative (nouveau.R : termes_variable, termes_croises), aux réglages du terme
# (échelle, bloc, modificateur), sans sa transformation. Un terme où elle vient deux fois (son carré) est
# refusé, comme dans Nouveau modèle. attr(, "change").
termes_en_categories <- function(termes, projet, uid) {
  v <- variable_du_projet(projet, uid)
  K <- length(v$categories)
  garde <- list()
  change <- FALSE
  for (t in termes) {
    vs <- as.numeric(t$variables)
    cats <- vapply(seq_along(vs), function(j) as.numeric(categorie(t, j)), 0)
    j <- which(vs == uid & cats == 0)
    if (!length(j) || length(t$lags)) {
      garde[[length(garde) + 1]] <- t
      next
    }
    if (length(j) > 1) stop(refus_qualitative_double(vapply(vs, function(u) as.character(variable_du_projet(projet, u)$ids[1]), "")), call. = FALSE)
    change <- TRUE
    for (k in seq_len(K)) {
      n <- t
      cats[j] <- k
      n$categories <- if (length(vs) == 1) k else cats
      n$name <- nom_terme(projet, vs, cats)
      n["transform"] <- list(NULL)
      garde[[length(garde) + 1]] <- n
    }
  }
  structure(garde, change = change)
}

# Une variable traitée en catégories (`type` "qualitative" : un site codé 1, 2, 3, qu'un effet aléatoire
# demande qualitatif) ou en nombres ("quantitative"), d'un clic droit sur l'en-tête de sa colonne. En
# catégories, chaque valeur distincte en est une, écrite à 15 chiffres significatifs (2019, 2.5), dans
# l'ordre des nombres ; en nombres, chaque catégorie lue comme un nombre (« 3,5 » compris : lire_nombres),
# refusé quand l'une n'en est pas un (Chercher et remplacer, d'abord). Les définitions des modèles du
# projet suivent dans leurs termes X : la variable y prend un terme par catégorie, ou un seul en nombres
# (suivre_categories) ; un modèle qui l'a en Y, ou ailleurs (une classe, une courbe, un effet aléatoire,
# sa condition, un réglage d'une autre analyse), la refuse : le modifier d'abord. Un modèle de code R la
# verra autrement (un facteur, ou des nombres) : il est nommé. `modeles` FALSE : une étape rejouée sur des
# données sans modèles. attr(, "type") : la variable, son nom, son type, ses catégories, les modèles qui
# ont suivi, ceux de code R, et les phrases qui le disent.
changer_type_variable <- function(projet, variable, type, modeles = TRUE) {
  fr <- !identical(getOption("nuee.langue"), "en")
  g <- function(x) guillemets_texte(x, fr)
  type <- premier_texte(type)
  if (!type %in% c("qualitative", "quantitative")) {
    stop(sprintf(traduire("Type inconnu : %s (qualitative ou quantitative).", "Unknown type: %s (qualitative or quantitative)."), type), call. = FALSE)
  }
  uid <- as.numeric(variable)[1]
  i <- which(vapply(projet$variables, function(v) identical(as.numeric(v$uid), uid), TRUE))[1]
  k <- nom_numero(uid)
  if (is.na(i) || !k %in% colnames(projet$donnees)) stop(traduire("Variable introuvable : ", "Variable not found: "), premier_texte(variable), call. = FALSE)
  v <- projet$variables[[i]]
  nom <- as.character(v$ids[1])
  cats <- as.character(v$categories %||% character(0))
  if (identical(type, "qualitative") == (length(cats) > 0)) {
    stop(sprintf(if (length(cats)) traduire("%s est déjà en catégories.", "%s already is categories.") else traduire("%s est déjà en nombres.", "%s already is numbers."), g(nom)), call. = FALSE)
  }
  u <- if (modeles) modeles_variable(projet, uid, nom) else list(definitions = character(0), code = character(0))
  ailleurs <- Filter(function(d) premier_texte(d$name) %in% u$definitions &&
                       (length(termes_de(d$y, uid)) > 0 || utilise_hors_termes(d, uid) || nom %in% noms_condition_definition(d)), projet$models)
  if (length(ailleurs)) {
    stop(sprintf(traduire("%s utilise %s ailleurs qu'en variable X (en Y, en classe, en courbe, en effet aléatoire, dans sa condition ou ses réglages) : modifiez-le d'abord (Modèle › Modifier le modèle).",
                          "%s uses %s otherwise than as an X variable (as Y, a class, a curve, a random effect, in its condition or its settings): modify it first (Model › Modify model)."),
                 liste_mots(vapply(ailleurs, function(d) premier_texte(d$name), ""), fr), g(nom)), call. = FALSE)
  }
  x <- projet$donnees[, k]
  if (identical(type, "qualitative")) {
    valeurs <- sort(unique(x[!is.na(x)]))
    projet$donnees[, k] <- match(x, valeurs)
    projet$variables[[i]]$categories <- sprintf("%.15g", valeurs + 0)  # (+ 0 : -0 s'écrit 0)
    projet$models <- lapply(projet$models, function(d) {
      change <- FALSE
      for (champ in c("x", "termes_complets")) {
        if (!length(d[[champ]])) next
        r <- termes_en_categories(d[[champ]], projet, uid)
        if (isTRUE(attr(r, "change"))) {
          attr(r, "change") <- NULL
          d[[champ]] <- r
          change <- TRUE
        }
      }
      if (change) {
        d$solution <- NULL
        d$reference <- list(ynames = d[["reference"]][["ynames"]], note = d[["reference"]][["note"]])
      }
      d
    })
  } else {
    n <- lire_nombres(cats)
    if (anyNA(n)) {
      stop(sprintf(traduire("Ces catégories de %s ne sont pas des nombres : %s. Remplacez-les d'abord (Édition › Chercher et remplacer), ou videz-les.",
                            "These categories of %s aren't numbers: %s. Replace them first (Edit › Find and replace), or empty them."),
                   g(nom), paste0(paste(vapply(utils::head(cats[is.na(n)], 5), g, ""), collapse = ", "), if (sum(is.na(n)) > 5) "…" else "")), call. = FALSE)
    }
    projet$donnees[, k] <- n[x]
    projet$variables[[i]]["categories"] <- list(NULL)
    projet <- suivre_categories(projet, uid, NULL, cats)
  }
  apres <- as.character(projet$variables[[i]]$categories %||% character(0))
  suivis <- u$definitions
  phrases <- c(if (identical(type, "qualitative")) sprintf(traduire("%s en catégories : %d (%s).", "%s as categories: %d (%s)."), g(nom), length(apres),
                                                          paste0(paste(utils::head(apres, 6), collapse = ", "), if (length(apres) > 6) "…" else ""))
               else sprintf(traduire("%s en nombres.", "%s as numbers."), g(nom)),
               if (length(suivis)) sprintf(traduire("Les définitions de %s suivent ; leurs ajustements gardent leurs résultats jusqu'à ce que vous les ajustiez à nouveau.",
                                                    "The definitions of %s follow; their fits keep their results until you fit them again."), liste_mots(suivis, fr)),
               vapply(u$code, function(m) sprintf(if (identical(type, "qualitative")) traduire("%s (code R) verra %s en facteur quand il sera ajusté à nouveau.", "%s (R code) will see %s as a factor when it is fitted again.")
                                                  else traduire("%s (code R) verra %s en nombres quand il sera ajusté à nouveau.", "%s (R code) will see %s as numbers when it is fitted again."), m, g(nom)), ""))
  attr(projet, "type") <- list(variable = uid, nom = nom, type = type, categories = I(apres), modeles = I(suivis), code = I(u$code), phrases = I(phrases))
  projet
}

# Le nom et le type (qualitative ou non) de chaque variable qu'utilise une définition (etapes.R :
# variables_definition), par numéro, tels qu'ils sont dans le projet : gardés avec un modèle de la séance
# quand il est ajusté (serveur.R : ajuster_et_garder), pour voir ensuite qu'ils ont changé.
variables_vues <- function(projet, d) {
  uids <- variables_definition(d)
  if (!length(uids)) return(list())
  index <- stats::setNames(seq_along(projet$variables), vapply(projet$variables, function(v) nom_numero(v$uid), ""))
  vues <- lapply(nom_numero(uids), function(u) {
    i <- index[u]
    if (is.na(i)) return(NULL)
    v <- projet$variables[[i]]
    list(nom = as.character(v$ids[1]), qualitative = length(v$categories) > 0)
  })
  stats::setNames(vues, nom_numero(uids))
}

# Ce qui a changé depuis l'ajustement du modèle de définition `d` dans les variables qu'il utilise
# (`vues` : variables_vues à son ajustement) : un nom changé, une variable supprimée ou passée en
# catégories ou en nombres. Le message qui le dit, NULL si rien (serveur.R : modele, avant de le réajuster,
# de prédire ou de l'exporter).
variables_changees <- function(projet, d, vues) {
  if (!length(vues)) return(NULL)
  fr <- !identical(getOption("nuee.langue"), "en")
  g <- function(x) guillemets_texte(x, fr)
  modele <- as.character(d$name %||% "")
  index <- stats::setNames(seq_along(projet$variables), vapply(projet$variables, function(v) nom_numero(v$uid), ""))
  for (u in names(vues)) {
    x <- vues[[u]]
    if (is.null(x)) next
    i <- index[u]
    if (is.na(i) || !u %in% colnames(projet$donnees)) {
      return(sprintf(traduire("%s n'est plus dans les données depuis l'ajustement de %s : il ne s'ajuste plus sans elle (Ctrl+Z la rend).",
                              "%s is no longer in the data since %s was fitted: it can't be fitted again without it (Ctrl+Z brings it back)."), g(x$nom), modele))
    }
    v <- projet$variables[[i]]
    if (!identical(as.character(v$ids[1]), as.character(x$nom))) {
      return(sprintf(traduire("%s s'appelle %s depuis l'ajustement de %s : ajustez-le à nouveau (Modèle › Modifier le modèle, ou un double-clic sur sa définition dans l'explorateur).",
                              "%s is called %s since %s was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer)."),
                     g(x$nom), g(as.character(v$ids[1])), modele))
    }
    if (!identical(length(v$categories) > 0, isTRUE(x$qualitative))) {
      return(sprintf(traduire("%s est en %s depuis l'ajustement de %s : ajustez-le à nouveau (Modèle › Modifier le modèle, ou un double-clic sur sa définition dans l'explorateur).",
                              "%s is %s since %s was fitted: fit it again (Model › Modify model, or a double-click on its definition in the explorer)."), g(x$nom),
                     if (length(v$categories)) traduire("catégories", "categories") else traduire("nombres", "numbers"), modele))
    }
  }
  NULL
}

# Les références d'un texte de document ({{M1.coef.site}} : interface/references.js) aux termes d'un modèle
# dont le nom a changé : `carte`, pour chaque modèle (par son nom), le nouveau nom de chacun de ses termes
# renommés, nommé par l'ancien ; `noms`, ceux de tous les modèles du projet (le plus long nom suivi d'un
# point est celui de la référence, comme references.js : separerCible). Sont suivis les chemins qui nomment
# un terme (coef, est, p, ic, se, vip ; RMSEE et RMSEcv d'un Y ; cvanova.p et cvanova.F d'un Y) ; le
# format, l'étiquette et la légende restent. Le texte, et attr(, "n") : le nombre de références réécrites.
suivre_references_texte <- function(texte, noms, carte) {
  texte <- premier_texte(texte)
  if (!length(carte) || !grepl("{{", texte, fixed = TRUE)) return(structure(texte, n = 0L))
  noms <- unique(noms[order(-nchar(noms))])
  m <- gregexpr("\\{\\{([\\s\\S]*?)\\}\\}", texte, perl = TRUE)
  refs <- regmatches(texte, m)[[1]]
  if (!length(refs)) return(structure(texte, n = 0L))
  n <- 0L
  nouvelles <- vapply(refs, function(r) {
    interieur <- substr(r, 3, nchar(r) - 2)
    debut <- regmatches(interieur, regexpr("^\\s*", interieur))
    corps <- substring(interieur, nchar(debut) + 1L)
    if (grepl("^[<%]", corps)) return(r)
    # (la cible : sans la légende, après |, ni l'étiquette, #nom, ni le format, :3)
    cible <- sub("\\|.*$", "", corps, perl = TRUE)
    cible <- sub("\\s#[\\p{L}\\p{N}_-]+\\s*$", "", cible, perl = TRUE)
    cible <- trimws(sub(":(%[0-9]?|[0-9])$", "", trimws(cible)))
    for (nm in noms) {
      if (!startsWith(cible, paste0(nm, ".")) || nchar(cible) <= nchar(nm) + 1L) next
      tc <- carte[[nm]]
      if (is.null(tc)) return(r)
      chemin <- strsplit(substring(cible, nchar(nm) + 2L), ".", fixed = TRUE)[[1]]
      k <- if (chemin[1] %in% c("coef", "est", "p", "ic", "se", "vip", "RMSEE", "RMSEcv")) 2L
           else if (identical(chemin[1], "cvanova") && length(chemin) >= 3 && chemin[2] %in% c("p", "F")) 3L else 0L
      if (!k || length(chemin) < k) return(r)
      terme <- paste(chemin[k:length(chemin)], collapse = ".")
      if (!terme %in% names(tc)) return(r)
      n <<- n + 1L
      nouvelle <- paste(c(nm, chemin[seq_len(k - 1L)], tc[[terme]]), collapse = ".")
      return(paste0("{{", debut, nouvelle, substring(corps, nchar(cible) + 1L), "}}"))
    }
    r
  }, "", USE.NAMES = FALSE)
  regmatches(texte, m) <- list(nouvelles)
  structure(texte, n = n)
}

# Les documents d'un projet dont les références suivent `carte` (suivre_references_texte) ; attr(, "changes") :
# l'identifiant de chaque document réécrit, attr(, "n") : le nombre de références réécrites.
suivre_references <- function(documents, noms, carte) {
  changes <- character(0)
  total <- 0L
  documents <- lapply(documents %||% list(), function(d) {
    t <- suivre_references_texte(d$texte, noms, carte)
    if (attr(t, "n") > 0) {
      d$texte <- as.character(t)
      changes <<- c(changes, premier_texte(d$id))
      total <<- total + attr(t, "n")
    }
    d
  })
  structure(documents, changes = changes, n = total)
}

# Une carte de suivre_references dans l'autre sens (Annuler un nom changé, ou le rétablir).
carte_inverse <- function(carte) lapply(carte, function(v) stats::setNames(names(v), unname(v)))
