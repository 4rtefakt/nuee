# La fusion de deux versions d'un projet (1.1), par le sens et non par le texte. Deux personnes ont
# changé le même projet chacune de leur côté (deux copies d'un dépôt GitHub ou GitLab, ou le même
# fichier .nuee ouvert par deux personnes sur un dossier partagé) : chacune garde ses changements et
# reçoit ceux de l'autre, et seuls les vrais désaccords (la même chose changée autrement des deux
# côtés) leur restent à trancher. fusionner_projets(base, mien, leur) : `base` est leur dernier état
# commun, `mien` et `leur` les deux côtés.
#   - Les modèles (p$models), par leur nom, champ par champ de leur définition, comparée avec les
#     numéros des variables et des observations écrits dans ceux de la fusion : le même modèle fait des
#     deux côtés est « le même ». Deux modèles nouveaux du même nom (chacun a fait « M5 ») restent tous
#     les deux, le leur sous le nom et le numéro libres suivants, et leurs documents et leurs figures
#     le suivent.
#   - Les documents (p$documents), par leur identifiant ; leur texte ligne à ligne contre la base (diff3).
#   - Les figures (p$figures), par leur identifiant, comme les modèles ; leurs panneaux suivent les
#     numéros des modèles.
#   - Les données, par les étapes quand les deux côtés n'ont fait qu'en ajouter à celles de la base et
#     que leurs données sont celles que leurs étapes donnent (etapes.R) : les étapes de la base, puis
#     les miennes, puis les leurs, rejouées depuis la source ; sinon cellule par cellule, les variables
#     par leur nom et les observations par leur identifiant.
#   - Le reste, champ par champ ; la séance de la fenêtre (p$session) est la mienne.
# Chaque désaccord est un conflit (son identifiant, son genre, ce que chaque côté a fait), tranché par
# `choix` ("mien" ou "leur" pour chaque identifiant ; sans choix, le mien) : la même fusion refaite avec
# les choix donne le projet (serveur.R : fusionner, puis fusionner_finir).

# ---------------------------------------------------------------- outils

# Le texte JSON d'une valeur écrit comme un fichier de projet l'écrit (nouveau.R : enregistrer_projet) :
# deux versions d'une même chose relues d'un fichier sont la même quand leurs textes sont égaux.
json_fusion <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA))

# Le texte d'un champ `f` de la liste `x` (json_fusion), NA quand elle ne l'a pas : un champ absent n'est
# pas un champ vide.
texte_champ <- function(x, f) if (f %in% names(x)) json_fusion(x[[f]]) else NA_character_

# Les champs `champs` d'une liste, ceux qu'elle a, en texte (json_fusion).
texte_champs <- function(x, champs) json_fusion(x[intersect(champs, names(x))])

# Le côté dont vient une chose après fusion, d'après ses trois textes (NA : absente de ce côté) : celui
# qui l'a changée, "mien" quand les deux l'ont changée de la même façon, "conflit" quand ils l'ont
# changée autrement.
cote_fusion <- function(b, m, l) {
  if (identical(m, l)) return("mien")
  if (identical(m, b)) return("leur")
  if (identical(l, b)) return("mien")
  "conflit"
}

# Un champ d'une liste mis à une valeur, ou retiré pour NULL (un champ NULL s'écrirait « null » dans le
# fichier du projet, qui ne l'avait pas).
avec_champ <- function(x, f, valeur) {
  if (is.null(valeur)) x[[f]] <- NULL else x[[f]] <- valeur
  x
}

# Une phrase dans la langue de la demande (langue.R).
phrase_fusion <- function(fr, en, ...) sprintf(traduire(fr, en), ...)

# Ce qu'un projet a de la séance et non du fichier (serveur.R) : jamais fusionné, toujours le mien.
CHAMPS_SEANCE_FUSION <- c("id", "nom", "chemin", "chemin_enregistrement", "modifie", "table", "brut", "fichier_lu", "note",
                          "import_collecte", "lecture_seule", "cache")

# Un projet tel qu'un fichier le rend : enregistré dans un fichier temporaire, puis relu (les nombres à
# 15 chiffres, les listes comme jsonlite les relit). Le projet de la séance se compare ainsi à des
# versions lues de fichiers : une valeur qu'il n'a pas changée est la même des deux côtés.
projet_relu <- function(p, dossier = dossier_projet(p)) {
  f <- tempfile("fusion", fileext = ".nuee")
  on.exit(unlink(f))
  enregistrer_projet(p, f, dossier = dossier)
  lire_projet(f)
}

# Deux empreintes d'un même fichier (etapes.R : empreinte_sha) : la même quand elles sont du même
# algorithme et égales ; un SHA-256 et le MD5 d'un R plus ancien ne se comparent pas, seule leur taille.
memes_fiches <- function(a, b) {
  sa <- premier_texte(a$sha256)
  sb <- premier_texte(b$sha256)
  if (nzchar(sa) && nzchar(sb) && startsWith(sa, "md5:") == startsWith(sb, "md5:")) return(identical(sa, sb))
  isTRUE(as.numeric(unlist(a$taille))[1] == as.numeric(unlist(b$taille))[1]) && nzchar(sa) == nzchar(sb)
}

# La même source de données (etapes.R : source_fichier), où qu'elle soit sur un ordinateur et quand
# qu'elle ait été lue : le même contenu, le même lecteur, la même table, les mêmes décisions de son import
# et le même formulaire d'un import de KoboToolbox ou d'ODK.
meme_source <- function(a, b) {
  if (!is.list(a) || !is.list(b)) return(!is.list(a) && !is.list(b))
  reglages <- function(s) json_fusion(list(lecteur = premier_texte(s$lecteur), table = premier_texte(s$table), reglages = s$reglages, collecte = s$collecte))
  fa <- a$formulaire
  fb <- b$formulaire
  memes_fiches(a, b) && identical(reglages(a), reglages(b)) &&
    (if (is.list(fa) && is.list(fb)) memes_fiches(fa, fb) else !is.list(fa) && !is.list(fb))
}

# Une étape (etapes.R) par ce qu'elle fait : sa demande, ses paramètres et la langue où elle nomme ce
# qu'elle crée. Deux étapes de même clé font le même changement ; avec `date`, c'est la même étape,
# notée une seule fois puis gardée des deux côtés.
cle_etape <- function(e, date = FALSE) {
  paste(c(premier_texte(e$demande), premier_texte(e$parametres), premier_texte(e$langue), if (date) premier_texte(e$date)), collapse = "\u001e")
}
cles_etapes <- function(p, date = TRUE) vapply(p$etapes %||% list(), cle_etape, "", date = date)

# Le contenu d'un projet en texte, sans ce qui ne compte pas pour savoir s'il a changé : la séance, ce
# qui l'a écrit, son nom, où ses fichiers sont sur un ordinateur et quand ils ont été lus.
contenu_fusion <- function(p) {
  x <- unclass(p)
  for (i in seq_along(x$datasets)) {
    d <- x$datasets[[i]]
    x$datasets[[i]]$values <- unname(p$donnees[as.character(d$observations), as.character(d$variables), drop = FALSE])
  }
  x[c("donnees", "noms_obs", "noms_var", "session", "nuee", CHAMPS_SEANCE_FUSION)] <- NULL
  sans_lieu <- function(f) if (is.list(f)) f[setdiff(names(f), c("chemin", "relatif", "date", "lu"))] else f
  if (is.list(x$source_donnees)) {
    x$source_donnees <- sans_lieu(x$source_donnees)
    if (is.list(x$source_donnees$formulaire)) x$source_donnees$formulaire <- sans_lieu(x$source_donnees$formulaire)
  }
  if (length(x$etapes)) x$etapes <- lapply(x$etapes, function(e) { if (length(e$fichiers)) e$fichiers <- lapply(e$fichiers, sans_lieu); e })
  json_fusion(x)
}

# Un conflit de la fusion `ctx` : son identifiant (le même d'une fusion à la même), son genre ('modele',
# 'document', 'figure', 'source', 'variable' ou 'champ'), sa clé, son titre, ce que chaque côté a fait
# (`mien`, `leur` : des textes courts) et ses détails. Rend le choix fait pour lui ("mien" sans choix) ;
# un conflit déjà noté ne l'est pas deux fois.
conflit_fusion <- function(ctx, id, genre, cle, titre, mien, leur, details = NULL) {
  if (!any(vapply(ctx$conflits, function(x) identical(x$id, id), TRUE))) {
    ctx$conflits[[length(ctx$conflits) + 1L]] <- list(id = id, genre = genre, cle = cle, titre = titre, mien = mien, leur = leur, details = details)
  }
  if (identical(premier_texte(ctx$choix[[id]], "mien"), "leur")) "leur" else "mien"
}

# Une phrase de ce qu'un côté ("mien" ou "leur") a apporté.
apport_fusion <- function(ctx, cote, phrase) ctx$apports[[cote]] <- c(ctx$apports[[cote]], phrase)

# Une liste de noms pour une phrase : les premiers, puis combien d'autres.
noms_fusion <- function(x, n = 4) {
  x <- as.character(x)
  if (length(x) <= n) return(paste(x, collapse = ", "))
  paste0(paste(utils::head(x, n), collapse = ", "), phrase_fusion(" et %d autres", " and %d more", length(x) - n))
}

# Un nom entre les guillemets de la langue de la demande.
guillemets_fusion <- function(x) guillemets_texte(x, !identical(getOption("nuee.langue"), "en"))

# Un nombre négatif pour ce qu'un côté nomme et que la fusion n'a pas (une variable ou une observation
# retirée) : le même d'un côté à l'autre pour la même chose, jamais un numéro du projet.
pseudo_fusion <- function(ctx, genre, cle) {
  n <- paste(genre, cle, sep = "\u001e")
  if (is.null(ctx$pseudo[[n]])) ctx$pseudo[[n]] <- -(length(ctx$pseudo) + 1)
  ctx$pseudo[[n]]
}

# ---------------------------------------------------------------- les données sous leurs noms

# Des noms en clés uniques : un nom qui revient prend son rang (« x », puis « x␟2 »), comme les
# identifiants des observations (etapes.R : cles_observations).
cles_rangs <- function(noms) {
  if (!anyDuplicated(noms)) return(noms)
  o <- order(noms, method = "radix")
  tries <- noms[o]
  debut <- cummax(ifelse(!duplicated(tries), seq_along(tries), 0L))
  rang <- integer(length(noms))
  rang[o] <- seq_along(tries) - debut + 1L
  ifelse(rang > 1, paste0(noms, "\u001f", rang), noms)
}

# La clé d'un identifiant secondaire des observations (« site »), parmi celles des variables : ses
# valeurs se fusionnent comme celles d'une variable.
ID_FUSION <- "\u001did:"

# Les données d'un projet sous leurs noms, pour les comparer d'une version à l'autre : ses variables qui
# ont des données (`cles_v` : leur nom, avec son rang s'il revient ; `uid_v` : leur numéro par clé), ses
# observations (`cles_o` : cles_observations ; `uid_o`), les noms de ses identifiants (`noms_id`) et les
# clés de ses identifiants secondaires (`cles_id`).
tableau_fusion <- function(p) {
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(p$donnees), p$variables)
  cles_v <- cles_rangs(vapply(vars, function(v) premier_texte(v$ids), ""))
  cles_o <- cles_observations(p)
  noms_id <- as.character(unlist(p$observation_id_names %||% "Observation"))
  list(p = p, vars = stats::setNames(vars, cles_v), cles_v = cles_v,
       uid_v = stats::setNames(numeros_entiers(vapply(vars, function(v) as.numeric(v$uid), 0)), cles_v),
       cles_o = cles_o, uid_o = stats::setNames(numeros_entiers(vapply(p$observations, function(o) as.numeric(o$uid), 0)), cles_o),
       noms_id = noms_id, cles_id = if (length(noms_id) > 1) paste0(ID_FUSION, cles_rangs(noms_id[-1])) else character(0))
}

# Les valeurs de la variable de clé `k` du tableau `t` (tableau_fusion) pour les observations de clés
# `cles` : des nombres pour une variable quantitative, les noms des catégories pour une qualitative, le
# texte d'un identifiant secondaire ("" quand il est vide) ; NA pour une observation ou une variable que
# le tableau n'a pas.
valeurs_fusion <- function(t, k, cles) {
  n <- length(cles)
  if (startsWith(k, ID_FUSION)) {
    r <- rep(NA_character_, n)
    j <- match(k, t$cles_id) + 1L
    if (is.na(j)) return(r)
    i <- match(cles, t$cles_o)
    ok <- which(!is.na(i))
    r[ok] <- vapply(t$p$observations[i[ok]], function(o) {
      x <- as.character(unlist(o$ids))
      if (length(x) >= j && !is.na(x[j])) x[j] else ""
    }, "")
    return(r)
  }
  v <- t$vars[[k]]
  if (is.null(v)) return(rep(NA_real_, n))
  i <- match(as.character(t$uid_o[cles]), rownames(t$p$donnees))
  x <- unname(t$p$donnees[i, as.character(t$uid_v[[k]])])
  cats <- as.character(unlist(v$categories))
  if (length(cats)) cats[x] else as.numeric(x)
}

# Des valeurs (valeurs_fusion) en texte : un nombre écrit à 15 chiffres, comme un fichier de projet.
texte_valeurs <- function(x) if (is.numeric(x)) ifelse(is.na(x), NA_character_, sprintf("%.15g", x)) else as.character(x)

# Deux colonnes de valeurs égales case par case : deux valeurs manquantes, deux nombres égaux à 1e-12
# près (relu d'un fichier, un nombre écrit à 15 chiffres diffère de 5e-15 au plus, en proportion), deux
# textes égaux.
egales_fusion <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) {
    return((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & (a == b | abs(a - b) <= 1e-12 * pmax(abs(a), abs(b)))))
  }
  a <- texte_valeurs(a)
  b <- texte_valeurs(b)
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

# Une valeur pour une phrase : un nombre avec la virgule décimale en français, « vide » sans valeur.
valeur_lisible <- function(x) {
  if (is.na(x)) return(traduire("vide", "empty"))
  if (!is.numeric(x)) {
    n <- suppressWarnings(as.numeric(x))
    if (is.na(n) || !identical(sprintf("%.15g", n), x)) return(if (nzchar(x)) as.character(x) else traduire("vide", "empty"))
    x <- n
  }
  trimws(formatC(x, digits = 6, format = "fg", decimal.mark = traduire(",", ".")))
}

# Ce qu'un côté `tX` a changé aux données de la base `tB` (tableau_fusion) : les clés des variables (et
# des identifiants secondaires) et des observations ajoutées et retirées ; pour chaque variable commune,
# les clés des observations communes dont la valeur a changé (`cellules`), ses champs changés (unité,
# description, formule… : `champs`), ses catégories nouvelles (`categories`) et si sa liste de catégories
# a changé (`listes` : un ordre, une catégorie retirée).
changements_fusion <- function(tB, tX) {
  kb <- c(tB$cles_v, tB$cles_id)
  kx <- c(tX$cles_v, tX$cles_id)
  oc <- intersect(tB$cles_o, tX$cles_o)
  meme <- identical(tB$p$donnees, tX$p$donnees) && identical(tB$cles_o, tX$cles_o) && identical(tB$cles_v, tX$cles_v) &&
    identical(json_fusion(tB$p$variables), json_fusion(tX$p$variables))
  cellules <- list()
  for (k in intersect(kb, kx)) {
    if (meme && !startsWith(k, ID_FUSION)) next
    change <- !egales_fusion(valeurs_fusion(tB, k, oc), valeurs_fusion(tX, k, oc))
    if (any(change)) cellules[[k]] <- oc[change]
  }
  champs <- list()
  categories <- list()
  listes <- character(0)
  for (k in intersect(tB$cles_v, tX$cles_v)) {
    vb <- tB$vars[[k]]
    vx <- tX$vars[[k]]
    noms <- setdiff(union(names(vb), names(vx)), c("uid", "categories"))
    f <- noms[vapply(noms, function(n) !identical(texte_champ(vb, n), texte_champ(vx, n)), TRUE)]
    if (length(f)) champs[[k]] <- f
    lb <- as.character(unlist(vb$categories))
    lx <- as.character(unlist(vx$categories))
    neuves <- setdiff(lx, lb)
    if (length(neuves) && length(lb)) categories[[k]] <- neuves
    if (!identical(lb, lx)) listes <- c(listes, k)
  }
  list(ajoutees = setdiff(kx, kb), retirees = setdiff(kb, kx), obs_ajoutees = setdiff(tX$cles_o, tB$cles_o),
       obs_retirees = setdiff(tB$cles_o, tX$cles_o), cellules = cellules, champs = champs, categories = categories, listes = listes)
}

# Rien de ce que les données d'un côté ont (changements_fusion) n'a changé.
donnees_inchangees <- function(d) {
  !length(c(d$ajoutees, d$retirees, d$obs_ajoutees, d$obs_retirees, names(d$cellules), names(d$champs), d$listes))
}

# Le nom d'une clé de variable ou d'identifiant (tableau_fusion), pour une phrase.
nom_cle_fusion <- function(k) cle_lisible(if (startsWith(k, ID_FUSION)) substring(k, nchar(ID_FUSION) + 1L) else k)
titre_cle_fusion <- function(k) {
  if (startsWith(k, ID_FUSION)) phrase_fusion("Identifiant %s", "Identifier %s", guillemets_fusion(nom_cle_fusion(k)))
  else phrase_fusion("Variable %s", "Variable %s", guillemets_fusion(nom_cle_fusion(k)))
}

# Les cellules changées autrement des deux côtés d'une variable, pour un conflit : ce que chaque côté en
# a fait (les trois premières) et leur liste (les cent premières). `i` : leurs rangs dans `obs` ; `vb`,
# `vm`, `vl` : les valeurs de la base et des deux côtés ; `ob` : celles que la base a.
cellules_conflit <- function(obs, i, vb, vm, vl, ob) {
  b <- function(j) if (ob[j]) valeur_lisible(texte_valeurs(vb)[j]) else "—"
  exemples <- function(x) {
    t <- vapply(utils::head(i, 3), function(j) sprintf(traduire("%s : %s → %s", "%s: %s → %s"), cle_lisible(obs[j]), b(j), valeur_lisible(texte_valeurs(x)[j])), "")
    paste0(phrase_fusion("%d cellule(s) : ", "%d cell(s): ", length(i)), paste(t, collapse = ", "), if (length(i) > 3) ", …" else "")
  }
  list(mien = exemples(vm), leur = exemples(vl),
       details = list(n = length(i), cellules = lapply(utils::head(i, 100), function(j) list(observation = cle_lisible(obs[j]), base = if (ob[j]) b(j) else "",
                                                                                              mien = valeur_lisible(texte_valeurs(vm)[j]), leur = valeur_lisible(texte_valeurs(vl)[j])))))
}

# Les catégories qu'un côté `tX` a renommées dans la variable de clé `k` (tableau_fusion) : chacune de la
# base qu'il n'a plus, dont toutes les cellules ont pris chez lui un même nom nouveau ; le nouveau nom de
# chacune, nommé par l'ancien.
renommages_categories <- function(tB, tX, k) {
  cb <- as.character(unlist(tB$vars[[k]]$categories))
  cx <- as.character(unlist(tX$vars[[k]]$categories))
  r <- character(0)
  if (!length(cb) || !length(cx)) return(r)
  oc <- intersect(tB$cles_o, tX$cles_o)
  vb <- valeurs_fusion(tB, k, oc)
  vx <- valeurs_fusion(tX, k, oc)
  for (o in setdiff(cb, cx)) {
    n <- unique(vx[!is.na(vb) & vb == o])
    if (length(n) == 1 && !is.na(n) && !n %in% cb) r[[o]] <- n
  }
  r
}

# Les catégories d'une variable qualitative après fusion. `cb`, `cm`, `cl` : celles de la base, la mienne
# et la leur (NULL quand le côté ne l'a pas en qualitative) ; `depart` : la liste d'où partir (la mienne,
# d'ordinaire) ; `utilisees` : celles qu'ont les valeurs fusionnées ; `renommees` : celles qu'un côté a
# renommées (renommages_categories). Une catégorie renommée prend la place de l'ancienne (comme Édition ›
# Catégories la lui donne) ; une catégorie qu'un côté a retirée (fusionnée dans une autre) part, sauf si
# une valeur l'a encore ; celles qu'un côté a ajoutées, et toute catégorie qu'une valeur a, viennent juste
# avant la dernière, qui reste la dernière (etapes.R : categories_alignees ; la référence d'une régression
# ne change pas). L'ordre des catégories communes est celui du côté qui l'a changé ; changé autrement des
# deux côtés, un conflit.
categories_fusion <- function(cb, cm, cl, depart, utilisees, ctx, k, renommees = character(0)) {
  for (o in names(renommees)) {
    i <- match(o, depart)
    if (!is.na(i) && !renommees[[o]] %in% depart && !o %in% utilisees) depart[i] <- renommees[[o]]
  }
  retirees <- c(if (length(cb) && !is.null(cm)) setdiff(cb, cm), if (length(cb) && !is.null(cl)) setdiff(cb, cl))
  liste <- depart[!(depart %in% retirees) | depart %in% utilisees]
  neuves <- unique(c(if (!is.null(cm)) setdiff(cm, cb), if (!is.null(cl)) setdiff(cl, cb), utilisees))
  neuves <- neuves[!neuves %in% liste & (!(neuves %in% retirees) | neuves %in% utilisees)]
  liste <- categories_alignees(liste, neuves)
  if (!is.null(cb) && !is.null(cm) && !is.null(cl)) {
    communes <- liste[liste %in% cb & liste %in% cm & liste %in% cl]
    om <- cm[cm %in% communes]
    ol <- cl[cl %in% communes]
    if (!identical(om, ol)) {
      ob <- cb[cb %in% communes]
      cote <- if (identical(om, ob)) "leur" else if (identical(ol, ob)) "mien"
              else conflit_fusion(ctx, paste0("champ:categories:", k), "champ", k,
                                  phrase_fusion("Ordre des catégories de %s", "Order of the categories of %s", guillemets_fusion(nom_cle_fusion(k))),
                                  paste(om, collapse = ", "), paste(ol, collapse = ", "), list(base = I(ob), mien = I(om), leur = I(ol)))
      liste[liste %in% communes] <- if (cote == "leur") ol else om
    }
  }
  liste
}

# ---------------------------------------------------------------- les données, cellule par cellule

# Le type d'une variable d'un tableau : "qualitative", "quantitative", NA quand il ne l'a pas.
type_fusion <- function(t, k) {
  v <- t$vars[[k]]
  if (startsWith(k, ID_FUSION) || is.null(v)) return(NA_character_)
  if (length(unlist(v$categories))) "qualitative" else "quantitative"
}

# Les données de la base `tB`, de mon côté `tM` et du leur `tL` (tableau_fusion ; `dM`, `dL` : ce que
# chaque côté a changé, changements_fusion) fusionnées cellule par cellule. Une variable (ou une
# observation) ajoutée d'un côté est gardée ; retirée d'un côté et inchangée de l'autre, elle est
# retirée ; retirée d'un côté et changée de l'autre, c'est un conflit. Chaque cellule prend la valeur
# du côté qui l'a changée ; changée autrement des deux côtés, c'est un conflit pour sa variable, qui
# liste ses cellules. Les champs d'une variable (unité, description, formule) se fusionnent un à un ; ses
# catégories, categories_fusion. Mes variables et mes observations gardent leurs numéros ; les leurs, les
# leurs quand ils sont libres, sinon les suivants. Rend les champs des données du projet fusionné
# (variables, observations, identifiants, jeux de données, tableau des données) et des `notes`.
fusion_cellules <- function(tB, tM, tL, dM, dL, ctx) {
  M <- tM$p
  L <- tL$p
  kB <- c(tB$cles_v, tB$cles_id)
  kM <- c(tM$cles_v, tM$cles_id)
  kL <- c(tL$cles_v, tL$cles_id)
  inchangee <- function(d, k) is.null(d$cellules[[k]]) && is.null(d$champs[[k]]) && !k %in% d$listes
  retiree <- traduire("retirée", "removed")
  resume_variable <- function(d, k) {
    n <- length(d$cellules[[k]])
    paste(c(if (n) phrase_fusion("%d cellule(s) changée(s)", "%d cell(s) changed", n),
            if (length(d$champs[[k]])) paste(d$champs[[k]], collapse = ", "),
            if (k %in% d$listes) traduire("catégories", "categories")), collapse = traduire(" ; ", "; "))
  }
  garder_v <- character(0)
  for (k in kM) {
    if (k %in% kL || !(k %in% kB)) {
      garder_v <- c(garder_v, k)
      next
    }
    if (inchangee(dM, k)) next  # (retirée de leur côté, inchangée du mien : retirée)
    if (conflit_fusion(ctx, paste0("variable:", k), "variable", k, titre_cle_fusion(k), resume_variable(dM, k), retiree, list(retiree = "leur")) == "mien") {
      garder_v <- c(garder_v, k)
    }
  }
  for (k in setdiff(kL, kM)) {
    if (!(k %in% kB)) {
      garder_v <- c(garder_v, k)
      next
    }
    if (inchangee(dL, k)) next
    if (conflit_fusion(ctx, paste0("variable:", k), "variable", k, titre_cle_fusion(k), retiree, resume_variable(dL, k), list(retiree = "mien")) == "leur") {
      garder_v <- c(garder_v, k)
    }
  }
  # (les observations : de même, une seule question pour celles retirées d'un côté et changées de l'autre)
  oB <- tB$cles_o
  oM <- tM$cles_o
  oL <- tL$cles_o
  changees_m <- unique(unlist(dM$cellules))
  changees_l <- unique(unlist(dL$cellules))
  par_l <- oM[!oM %in% oL & oM %in% oB]
  garder_m <- oM[oM %in% oL | !oM %in% oB | oM %in% changees_m]
  conflit_l <- par_l[par_l %in% changees_m]
  if (length(conflit_l)) {
    choix <- conflit_fusion(ctx, "champ:observations:leur", "champ", "observations",
                            traduire("Observations retirées de leur côté, changées du mien", "Observations removed on their side, changed on mine"),
                            phrase_fusion("%d observation(s) changée(s) : %s", "%d observation(s) changed: %s", length(conflit_l), noms_fusion(cle_lisible(conflit_l))),
                            phrase_fusion("%d observation(s) retirée(s)", "%d observation(s) removed", length(conflit_l)), list(observations = I(cle_lisible(conflit_l))))
    if (choix == "leur") garder_m <- setdiff(garder_m, conflit_l)
  }
  par_m <- oL[!oL %in% oM & oL %in% oB]
  conflit_m <- par_m[par_m %in% changees_l]
  rendues <- character(0)
  if (length(conflit_m)) {
    choix <- conflit_fusion(ctx, "champ:observations:mien", "champ", "observations",
                            traduire("Observations retirées de mon côté, changées du leur", "Observations removed on my side, changed on theirs"),
                            phrase_fusion("%d observation(s) retirée(s)", "%d observation(s) removed", length(conflit_m)),
                            phrase_fusion("%d observation(s) changée(s) : %s", "%d observation(s) changed: %s", length(conflit_m), noms_fusion(cle_lisible(conflit_m))),
                            list(observations = I(cle_lisible(conflit_m))))
    if (choix == "leur") rendues <- conflit_m
  }
  obs <- c(garder_m, oL[(!oL %in% oM & !oL %in% oB) | oL %in% rendues])
  n <- length(obs)
  # (les valeurs de chaque variable gardée, cellule par cellule ; une variable ajoutée des deux côtés et
  # faite autrement, une variable calculée de deux formules : un seul conflit, dont le choix donne aussi
  # ses champs, `entiere`)
  valeurs <- list()
  entiere <- list()
  for (k in garder_v) {
    ob <- if (k %in% kB) obs %in% oB else rep(FALSE, n)
    om <- if (k %in% kM) obs %in% oM else rep(FALSE, n)
    ol <- if (k %in% kL) obs %in% oL else rep(FALSE, n)
    vb <- valeurs_fusion(tB, k, obs)
    vm <- valeurs_fusion(tM, k, obs)
    vl <- valeurs_fusion(tL, k, obs)
    if (!(is.numeric(vb) && is.numeric(vm) && is.numeric(vl))) {
      vb <- texte_valeurs(vb)
      vm <- texte_valeurs(vm)
      vl <- texte_valeurs(vl)
    }
    r <- vm
    seul_l <- !om & ol
    r[seul_l] <- vl[seul_l]
    deux <- om & ol
    eml <- egales_fusion(vm, vl)
    emb <- ob & egales_fusion(vm, vb)
    elb <- ob & egales_fusion(vl, vb)
    pris_l <- deux & !eml & emb
    r[pris_l] <- vl[pris_l]
    conflit <- deux & !eml & !emb & !elb
    champs <- character(0)
    if (!startsWith(k, ID_FUSION) && k %in% kM && k %in% kL && !k %in% kB) {
      wm <- tM$vars[[k]]
      wl <- tL$vars[[k]]
      champs <- setdiff(unique(c(names(wm), names(wl))), c("uid", "categories"))
      champs <- champs[vapply(champs, function(f) !identical(texte_champ(wm, f), texte_champ(wl, f)), TRUE)]
    }
    if (any(conflit) || length(champs)) {
      x <- if (any(conflit)) cellules_conflit(obs, which(conflit), vb, vm, vl, ob) else list(mien = character(0), leur = character(0))
      decrire <- function(w, cellules) paste(c(vapply(champs, function(f) sprintf(traduire("%s : %s", "%s: %s"), nom_champ_variable(f), paste(as.character(unlist(w[[f]])), collapse = " ")), ""),
                                               cellules), collapse = traduire(" ; ", "; "))
      choix <- conflit_fusion(ctx, paste0("variable:", k), "variable", k, titre_cle_fusion(k), decrire(tM$vars[[k]], x$mien), decrire(tL$vars[[k]], x$leur),
                              c(x$details, if (length(champs)) list(champs = I(champs))))
      if (choix == "leur") r[conflit] <- vl[conflit]
      if (length(champs)) entiere[[k]] <- choix
    }
    valeurs[[k]] <- r
  }
  # (les variables : leurs champs, leur type, leurs catégories, leurs numéros)
  vars_k <- garder_v[!startsWith(garder_v, ID_FUSION)]
  ids_k <- garder_v[startsWith(garder_v, ID_FUSION)]
  pris <- numeros_entiers(vapply(M$variables, function(v) as.numeric(v$uid), 0))
  uid_v <- stats::setNames(numeric(length(vars_k)), vars_k)
  for (k in vars_k) {
    if (k %in% tM$cles_v) {
      uid_v[[k]] <- tM$uid_v[[k]]
      next
    }
    u <- if (k %in% tL$cles_v) tL$uid_v[[k]] else tB$uid_v[[k]]
    if (u %in% pris) u <- max(c(0, pris)) + 1
    pris <- c(pris, u)
    uid_v[[k]] <- u
  }
  uid_v <- stats::setNames(numeros_entiers(uid_v), vars_k)
  notes <- character(0)
  variable <- function(k) {
    vb <- tB$vars[[k]]
    vm <- tM$vars[[k]]
    vl <- tL$vars[[k]]
    v <- vm %||% vl %||% vb
    for (f in setdiff(unique(c(names(vb), names(vm), names(vl))), c("uid", "categories"))) {
      cote <- entiere[[k]] %||% cote_fusion(texte_champ(vb, f), texte_champ(vm, f), texte_champ(vl, f))
      if (cote == "conflit") cote <- conflit_champ_variable(ctx, k, f, vm, vl)
      source <- if (cote == "leur" || is.null(vm)) vl else vm
      v <- avec_champ(v, f, if (f %in% names(source)) source[[f]])
      if (f %in% names(source) && is.null(source[[f]])) v[f] <- list(NULL)
    }
    # (son type : celui du côté qui l'a changé ; ses valeurs dans ce type)
    tb <- type_fusion(tB, k)
    tm <- type_fusion(tM, k)
    tl <- type_fusion(tL, k)
    type <- if (!is.na(tm) && !is.na(tl)) (if (tm == tl || !identical(tm, tb)) tm else tl) else if (!is.na(tm)) tm else tl
    x <- valeurs[[k]]
    if (type == "quantitative") {
      if (!is.numeric(x)) {
        nombres <- suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
        perdues <- sum(!is.na(x) & is.na(nombres))
        if (perdues) notes <<- c(notes, phrase_fusion("%s : %d valeur(s) qui ne sont pas des nombres laissée(s) vide(s)", "%s: %d value(s) that aren't numbers left empty",
                                                      guillemets_fusion(nom_cle_fusion(k)), perdues))
        x <- nombres
      }
      v["categories"] <- list(NULL)
      codes <- x
    } else {
      x <- texte_valeurs(x)
      cats <- function(w) if (!is.null(w) && length(unlist(w$categories))) as.character(unlist(w$categories))
      depart <- cats(vm) %||% cats(vl) %||% cats(vb) %||% character(0)
      liste <- categories_fusion(cats(vb), cats(vm), cats(vl), depart, unique(x[!is.na(x)]), ctx, k,
                                 c(renommages_categories(tB, tM, k), renommages_categories(tB, tL, k)))
      v$categories <- liste
      codes <- match(x, liste)
    }
    v$uid <- uid_v[[k]]
    list(v = v, codes = as.numeric(codes))
  }
  faites <- lapply(stats::setNames(vars_k, vars_k), variable)
  cle_de_m <- stats::setNames(tM$cles_v, as.character(tM$uid_v))
  variables <- list()
  for (v in M$variables) {
    k <- unname(cle_de_m[as.character(numeros_entiers(v$uid))])
    if (is.na(k)) variables[[length(variables) + 1L]] <- v  # (une variable sans données, d'un fichier .usp)
    else if (k %in% vars_k) variables[[length(variables) + 1L]] <- faites[[k]]$v
  }
  for (k in setdiff(vars_k, tM$cles_v)) variables[[length(variables) + 1L]] <- faites[[k]]$v
  # (les observations : leurs numéros, leurs noms et leurs identifiants secondaires)
  pris_o <- numeros_entiers(vapply(M$observations, function(o) as.numeric(o$uid), 0))
  uid_o <- numeric(n)
  for (i in seq_len(n)) {
    o <- obs[i]
    if (o %in% oM) {
      uid_o[i] <- tM$uid_o[[o]]
      next
    }
    u <- if (o %in% oL) tL$uid_o[[o]] else tB$uid_o[[o]]
    if (u %in% pris_o) u <- max(c(0, pris_o)) + 1
    pris_o <- c(pris_o, u)
    uid_o[i] <- u
  }
  uid_o <- numeros_entiers(uid_o)
  nom_obs <- function(t, cles) {
    i <- match(cles, t$cles_o)
    r <- rep(NA_character_, length(cles))
    r[!is.na(i)] <- vapply(t$p$observations[i[!is.na(i)]], function(x) premier_texte(x$ids), "")
    r
  }
  noms <- nom_obs(tM, obs)
  noms[is.na(noms)] <- nom_obs(tL, obs[is.na(noms)])
  noms[is.na(noms)] <- ""
  noms_id <- sub("\u001f\\d+$", "", c(tM$noms_id[1], substring(ids_k, nchar(ID_FUSION) + 1L)))
  ids <- if (length(ids_k)) do.call(cbind, lapply(ids_k, function(k) { x <- valeurs[[k]]; x[is.na(x)] <- ""; x }))
  observations <- lapply(seq_len(n), function(i) list(uid = uid_o[i], ids = c(noms[i], if (!is.null(ids)) unname(ids[i, ]))))
  # (le tableau des données, et les jeux de données : les miens, auxquels s'ajoutent les variables et les
  # observations des leurs, par leur nom ; une variable ou une observation qu'aucun jeu n'aurait ne
  # serait pas écrite dans le fichier : elle rejoint le premier)
  X <- matrix(NA_real_, n, length(vars_k), dimnames = list(as.character(uid_o), as.character(uid_v)))
  for (k in vars_k) X[, as.character(uid_v[[k]])] <- faites[[k]]$codes
  vers_o <- stats::setNames(uid_o, obs)
  jeux <- list()
  ajouter <- function(t, d) {
    cles_o <- stats::setNames(t$cles_o, as.character(t$uid_o))
    cles_v <- stats::setNames(t$cles_v, as.character(t$uid_v))
    o <- unname(vers_o[cles_o[as.character(numeros_entiers(d$observations))]])
    w <- unname(uid_v[cles_v[as.character(numeros_entiers(d$variables))]])
    o <- o[!is.na(o)]
    w <- w[!is.na(w)]
    j <- which(vapply(jeux, function(x) identical(premier_texte(x$name), premier_texte(d$name)), TRUE))
    if (!length(j)) {
      d$values <- NULL
      d$observations <- o
      d$variables <- w
      jeux[[length(jeux) + 1L]] <<- d
    } else {
      jeux[[j]]$observations <<- c(jeux[[j]]$observations, setdiff(o, jeux[[j]]$observations))
      jeux[[j]]$variables <<- c(jeux[[j]]$variables, setdiff(w, jeux[[j]]$variables))
    }
  }
  for (d in M$datasets) ajouter(tM, d)
  for (d in L$datasets) ajouter(tL, d)
  if (!length(jeux)) jeux <- list(list(name = premier_texte(M$source, "donnees"), observations = integer(0), variables = integer(0)))
  jeux[[1]]$observations <- c(jeux[[1]]$observations, setdiff(uid_o, unlist(lapply(jeux, `[[`, "observations"))))
  jeux[[1]]$variables <- c(jeux[[1]]$variables, setdiff(unname(uid_v), unlist(lapply(jeux, `[[`, "variables"))))
  jeux <- lapply(jeux, function(d) { d$observations <- numeros_entiers(d$observations); d$variables <- numeros_entiers(d$variables); d })
  list(variables = variables, observations = observations, observation_id_names = noms_id, datasets = jeux, donnees = X,
       noms_obs = stats::setNames(noms, as.character(uid_o)),
       noms_var = stats::setNames(vapply(variables, function(v) premier_texte(v$ids), ""), vapply(variables, function(v) as.character(numeros_entiers(v$uid)), "")),
       notes = notes)
}

# ---------------------------------------------------------------- les données, par les étapes

# Les données fusionnées par les étapes (etapes.R), quand c'est possible : les trois versions ont une
# source et des étapes, leurs données sont celles que leurs étapes donnent (donnees_conformes), et chaque
# côté n'a fait qu'ajouter des étapes à celles de la base. La source est celle du côté qui a mis les
# données à jour (les deux d'un autre fichier : un conflit, « source des données ») ; les étapes, les
# siennes, puis celles que l'autre a ajoutées (une étape faite des deux côtés, une fois), rejouées depuis
# elle et rapportées à ma numérotation (mise_a_jour). Une catégorie qu'un côté a retirée et qu'aucune
# valeur n'a plus part, et l'ordre est celui du côté qui l'a changé (categories_fusion). Une cellule
# changée autrement des deux côtés reste un conflit : le choix qui n'est pas ce que les étapes rejouées
# donnent devient une étape de plus, un collage des cellules, et les données restent celles que leurs
# étapes donnent. NULL quand ce n'est pas possible (une source absente de cet ordinateur, une étape de
# l'autre côté qui ne s'applique plus après les miennes…), et ctx$raison dit pourquoi : la fusion se fait
# alors cellule par cellule.
fusion_etapes <- function(B, M, L, tB, tM, tL, ctx, dossier) {
  rien <- function(raison) {
    ctx$raison <- raison
    NULL
  }
  cotes <- list(B, M, L)
  if (!all(vapply(cotes, function(p) is.list(p$source_donnees) && !is.null(p$etapes), TRUE))) {
    return(rien(traduire("un côté n'a pas de source de données ou d'étapes", "a side has no data source or steps")))
  }
  if (!all(vapply(cotes, function(p) isTRUE(donnees_conformes(p)), TRUE))) {
    return(rien(traduire("des données ont changé hors des étapes", "data changed outside the steps")))
  }
  eb <- cles_etapes(B)
  prefixe <- function(p) {
    e <- cles_etapes(p)
    length(e) >= length(eb) && identical(e[seq_along(eb)], eb)
  }
  if (!prefixe(M) || !prefixe(L)) return(rien(traduire("des étapes de la base ont été retirées", "steps of the base were removed")))
  # (la source : celle du côté qui a mis les données à jour)
  cm <- !meme_source(B$source_donnees, M$source_donnees)
  cl <- !meme_source(B$source_donnees, L$source_donnees)
  cote <- "mien"
  if (cm && cl && !meme_source(M$source_donnees, L$source_donnees)) cote <- conflit_source(ctx, M$source_donnees, L$source_donnees)
  else if (cl && !cm) cote <- "leur"
  S <- if (cote == "mien") M else L
  O <- if (cote == "mien") L else M
  nouvelles <- function(p) utils::tail(p$etapes, length(p$etapes) - length(eb))
  deja <- vapply(nouvelles(S), cle_etape, "")
  etapes <- c(S$etapes, Filter(function(e) !cle_etape(e) %in% deja, nouvelles(O)))
  if (is.null(fichier_retrouve(S$source_donnees, dossier))) {
    return(rien(phrase_fusion("la source %s est introuvable sur cet ordinateur", "the source %s isn't found on this computer", basename(premier_texte(S$source_donnees$chemin)))))
  }
  cible <- M
  cible$source_donnees <- S$source_donnees
  cible$models <- list()
  cible$prediction_sets <- list()
  r <- tryCatch(mise_a_jour(cible, dossier = dossier, etapes = etapes), error = function(e) e)
  if (inherits(r, "error")) return(rien(conditionMessage(r)))
  if (!is.null(r$choisir)) return(rien(traduire("la source a plusieurs tableaux", "the source has several tables")))
  for (e in utils::tail(r$rapport$etapes, length(etapes) - length(S$etapes))) {
    if (!identical(e$statut, "ok")) {
      return(rien(phrase_fusion("l'étape %s ne s'applique plus après les autres (%s)", "the step %s no longer applies after the others (%s)",
                                guillemets_fusion(premier_texte(e$etiquette)), premier_texte(e$raison))))
    }
  }
  D <- r$projet
  tD <- tableau_fusion(D)
  for (k in tD$cles_v) {
    v <- tD$vars[[k]]
    cats <- as.character(unlist(v$categories))
    if (!length(cats)) next
    de <- function(t) if (k %in% t$cles_v && length(unlist(t$vars[[k]]$categories))) as.character(unlist(t$vars[[k]]$categories))
    x <- valeurs_fusion(tD, k, tD$cles_o)
    liste <- categories_fusion(de(tB), de(tM), de(tL), de(tM) %||% cats, unique(x[!is.na(x)]), ctx, k,
                               c(renommages_categories(tB, tM, k), renommages_categories(tB, tL, k)))
    if (identical(liste, cats)) next
    i <- which(vapply(D$variables, function(w) identical(as.numeric(w$uid), as.numeric(v$uid)), TRUE))
    D$variables[[i]]$categories <- liste
    D$donnees[, as.character(v$uid)] <- match(cats[D$donnees[, as.character(v$uid)]], liste)
  }
  D$empreinte_donnees <- empreinte_donnees(D)
  # (les cellules changées autrement des deux côtés)
  oc <- intersect(tM$cles_o, tL$cles_o)
  tD <- tableau_fusion(D)
  for (k in intersect(tM$cles_v, tL$cles_v)) {
    vb <- valeurs_fusion(tB, k, oc)
    vm <- valeurs_fusion(tM, k, oc)
    vl <- valeurs_fusion(tL, k, oc)
    ob <- oc %in% tB$cles_o & k %in% tB$cles_v
    conflit <- !egales_fusion(vm, vl) & !(ob & egales_fusion(vm, vb)) & !(ob & egales_fusion(vl, vb))
    if (!any(conflit)) next
    i <- which(conflit)
    x <- cellules_conflit(oc, i, vb, vm, vl, ob)
    choix <- conflit_fusion(ctx, paste0("variable:", k), "variable", k, titre_cle_fusion(k), x$mien, x$leur, x$details)
    if (!k %in% tD$cles_v) next
    voulu <- texte_valeurs(if (choix == "mien") vm else vl)[i]
    a_faire <- which(!egales_fusion(voulu, texte_valeurs(valeurs_fusion(tD, k, oc[i]))) & oc[i] %in% tD$cles_o)
    if (!length(a_faire)) next
    q <- list(observations = unname(tD$uid_o[oc[i][a_faire]]), variables = tD$uid_v[[k]],
              valeurs = matrix(ifelse(is.na(voulu[a_faire]), "", voulu[a_faire]), ncol = 1))
    D <- noter_etape(D, sans_attributs(appliquer_etape("coller_cellules", D, q)), "coller_cellules", q,
                     etiquette_fusion(ctx, choix, phrase_fusion("valeurs de %s", "values of %s", nom_cle_fusion(k))))
    tD <- tableau_fusion(D)
  }
  # (l'unité et la description d'une variable changées autrement des deux côtés, de même ; une étape qui
  # les décrit, quand le choix n'est pas ce que les étapes rejouées donnent)
  for (k in intersect(intersect(tM$cles_v, tL$cles_v), tD$cles_v)) for (f in c("unite", "description")) {
    vm <- tM$vars[[k]]
    vl <- tL$vars[[k]]
    if (cote_fusion(texte_champ(tB$vars[[k]], f), texte_champ(vm, f), texte_champ(vl, f)) != "conflit") next
    choix <- conflit_champ_variable(ctx, k, f, vm, vl)
    voulu <- premier_texte((if (choix == "mien") vm else vl)[[f]])
    if (identical(voulu, premier_texte(tD$vars[[k]][[f]]))) next
    q <- list(descriptions = list(stats::setNames(list(tD$uid_v[[k]], voulu), c("variable", f))))
    D <- noter_etape(D, appliquer_etape("decrire_variables", D, q), "decrire_variables", q,
                     etiquette_fusion(ctx, choix, phrase_fusion("%s de %s", "%s of %s", nom_champ_variable(f), nom_cle_fusion(k))))
    tD <- tableau_fusion(D)
  }
  # (les coordonnées réglées autrement des deux côtés, de même)
  cm <- coordonnees_canon(tM)
  cl <- coordonnees_canon(tL)
  if (cote_fusion(json_fusion(coordonnees_canon(tB)), json_fusion(cm), json_fusion(cl)) == "conflit") {
    choix <- conflit_coordonnees(ctx, cm, cl)
    voulu <- if (choix == "mien") cm else cl
    if (!identical(json_fusion(voulu), json_fusion(coordonnees_canon(tD))) && (is.null(voulu) || all(c(voulu$x, voulu$y) %in% tD$cles_v))) {
      q <- if (is.null(voulu)) list(x = NULL, y = NULL) else list(x = tD$uid_v[[voulu$x]], y = tD$uid_v[[voulu$y]], systeme = premier_texte(voulu$systeme))
      D <- noter_etape(D, appliquer_etape("coordonnees", D, q), "coordonnees", q, etiquette_fusion(ctx, choix, traduire("coordonnées", "coordinates")))
    }
  }
  ctx$mode <- "etapes"
  D
}

# L'étiquette d'une étape qu'une fusion ajoute pour donner à un conflit le côté choisi : « fusion :
# valeurs de x1 de mon côté », « fusion : unité de x1 de Alice ».
etiquette_fusion <- function(ctx, choix, quoi) {
  if (choix == "mien") phrase_fusion("fusion : %s de mon côté", "merge: %s from my side", quoi) else phrase_fusion("fusion : %s de %s", "merge: %s from %s", quoi, ctx$qui)
}

# Le nom d'un champ d'une variable dans une phrase.
nom_champ_variable <- function(f) {
  switch(f, unite = traduire("unité", "unit"), description = traduire("description", "description"), formula = traduire("formule", "formula"),
         origine = traduire("origine", "origin"), ids = traduire("identifiants", "identifiers"), f)
}

# Le conflit d'un champ d'une variable (son unité, sa description, sa formule) changé autrement des
# deux côtés (`vm`, `vl` : la variable de chaque côté).
conflit_champ_variable <- function(ctx, k, f, vm, vl) {
  texte <- function(x) if (f %in% names(x)) paste(as.character(unlist(x[[f]])), collapse = " ") else traduire("retiré", "removed")
  titre <- phrase_fusion("%s de %s", "%s of %s", nom_champ_variable(f), guillemets_fusion(nom_cle_fusion(k)))
  conflit_fusion(ctx, paste0("champ:variable:", k, ":", f), "champ", paste(k, f, sep = "."),
                 paste0(toupper(substr(titre, 1, 1)), substring(titre, 2)), texte(vm), texte(vl))
}

# Les coordonnées d'un projet (spatial.R : p$coordonnees) par les clés de leurs variables (tableau_fusion),
# NULL sans elles ; et le conflit de coordonnées réglées autrement des deux côtés.
coordonnees_canon <- function(t) {
  co <- t$p$coordonnees
  if (!is.list(co) || !length(co$x) || !length(co$y)) return(NULL)
  cle <- function(u) t$cles_v[match(numeros_entiers(u), t$uid_v)]
  list(x = cle(co$x), y = cle(co$y), systeme = co$systeme)
}
conflit_coordonnees <- function(ctx, cm, cl) {
  texte <- function(x) if (is.null(x)) traduire("aucune", "none") else sprintf("%s, %s (%s)", nom_cle_fusion(x$x), nom_cle_fusion(x$y), premier_texte(x$systeme))
  conflit_fusion(ctx, "champ:coordonnees", "champ", "coordonnees", traduire("Coordonnées", "Coordinates"), texte(cm), texte(cl))
}

# Le conflit de la source des données : les deux côtés ont mis les données à jour d'un autre fichier.
conflit_source <- function(ctx, sm, sl) {
  texte <- function(s) phrase_fusion("données mises à jour de %s", "data updated from %s", basename(premier_texte(s$chemin)))
  conflit_fusion(ctx, "source", "source", "source", traduire("Source des données", "Data source"), texte(sm), texte(sl),
                 list(mien = basename(premier_texte(sm$chemin)), leur = basename(premier_texte(sl$chemin))))
}

# La source, les étapes et l'empreinte des données d'une fusion cellule par cellule : la source du côté
# qui l'a changée (changée autrement des deux côtés, le conflit de la source) ; mes étapes, sans celles
# qu'ils ont retirées, puis celles qu'ils ont ajoutées (une étape faite des deux côtés, une fois) ;
# l'empreinte des données fusionnées quand elles sont celles d'un côté et que ses étapes sont celles-là,
# sinon rompue (""), les données n'étant plus ce que les étapes donnent.
source_etapes_cellules <- function(B, M, L, p, ctx) {
  sb <- B$source_donnees
  sm <- M$source_donnees
  sl <- L$source_donnees
  cote <- if (meme_source(sm, sl) || (!meme_source(sb, sm) && meme_source(sb, sl))) "mien" else if (meme_source(sb, sm)) "leur" else conflit_source(ctx, sm, sl)
  p <- avec_champ(p, "source_donnees", if (cote == "leur") sl else sm)
  eb <- cles_etapes(B)
  em <- M$etapes %||% list()
  garde <- em[!cles_etapes(M) %in% setdiff(eb, cles_etapes(L))]
  neuves_m <- vapply(Filter(function(e) !cle_etape(e, TRUE) %in% eb, em), cle_etape, "")
  ajout <- Filter(function(e) !cle_etape(e, TRUE) %in% c(eb, cles_etapes(M)) && !cle_etape(e) %in% neuves_m, L$etapes %||% list())
  etapes <- c(garde, ajout)
  p <- avec_champ(p, "etapes", if (!is.null(M$etapes) || !is.null(L$etapes)) etapes)
  e <- empreinte_donnees(p)
  cles <- vapply(etapes, cle_etape, "", date = TRUE)
  conforme <- function(x) isTRUE(donnees_conformes(x)) && memes_donnees(e, empreinte_donnees(x)) && identical(cles_etapes(x), cles)
  avec_champ(p, "empreinte_donnees", if (!is.null(M$empreinte_donnees) || !is.null(L$empreinte_donnees)) (if (conforme(M) || conforme(L)) e else ""))
}

# ---------------------------------------------------------------- les numéros d'un côté dans ceux de la fusion

# De quoi rapporter ce qu'un côté `tX` numérote à la numérotation de la fusion `tF` (tableau_fusion) :
# `v(u)` ses numéros de variables, `o(u)` d'observations, `c(u, code)` le numéro d'une catégorie de sa
# variable u, `id(k)` le rang d'un identifiant ; par les noms (une variable ou une observation que la
# fusion n'a pas prend un nombre négatif, pseudo_fusion, le même d'un côté à l'autre).
carte_fusion <- function(tX, tF, ctx) {
  cle_v <- stats::setNames(tX$cles_v, as.character(tX$uid_v))
  cle_o <- stats::setNames(tX$cles_o, as.character(tX$uid_o))
  tous_f <- stats::setNames(vapply(tF$p$variables, function(v) as.numeric(v$uid), 0), vapply(tF$p$variables, function(v) premier_texte(v$ids), ""))
  noms_x <- stats::setNames(vapply(tX$p$variables, function(v) premier_texte(v$ids), ""), vapply(tX$p$variables, function(v) as.character(numeros_entiers(v$uid)), ""))
  v <- function(u) {
    u <- as.numeric(unlist(u))
    if (!length(u)) return(u)
    k <- unname(cle_v[as.character(numeros_entiers(u))])
    r <- unname(as.numeric(tF$uid_v[k]))
    for (i in which(is.na(r))) {
      if (!is.na(k[i])) {
        r[i] <- pseudo_fusion(ctx, "v", k[i])
        next
      }
      nom <- unname(noms_x[as.character(numeros_entiers(u[i]))])
      r[i] <- if (!is.na(nom) && nom %in% names(tous_f)) tous_f[[nom]] else pseudo_fusion(ctx, "v", paste0("#", if (is.na(nom)) u[i] else nom))
    }
    r
  }
  o <- function(u) {
    u <- as.numeric(unlist(u))
    if (!length(u)) return(u)
    k <- unname(cle_o[as.character(numeros_entiers(u))])
    r <- unname(as.numeric(tF$uid_o[k]))
    for (i in which(is.na(r))) r[i] <- pseudo_fusion(ctx, "o", if (is.na(k[i])) paste0("#", u[i]) else k[i])
    r
  }
  categorie <- function(u, code) {
    if (code == 0) return(0)
    k <- unname(cle_v[as.character(numeros_entiers(u))])
    if (is.na(k) || !k %in% tF$cles_v) return(NA_real_)
    nom <- as.character(unlist(tX$vars[[k]]$categories))[code]
    as.numeric(match(nom, as.character(unlist(tF$vars[[k]]$categories))))
  }
  id <- function(k) match(tX$noms_id[k], tF$noms_id)
  list(v = v, o = o, c = categorie, id = id)
}

# Une définition de modèle (p$models) dans la numérotation de la fusion (`carte` : carte_fusion) : ses
# termes (leurs variables et les numéros de leurs catégories ; le terme d'une catégorie qui n'est plus là
# part avec elle, comme atelier.R : suivre_termes), sa classe, ses courbes, son décalage, son lissage
# spatial, son groupe de modèles de classe, ses groupements (validation croisée, effets aléatoires et
# leurs pentes, autocorrélation, réglages d'une autre analyse : list(by = "variable", variable = …)), les
# variables des réglages d'une autre analyse et des moyennes ajustées, la réponse, le temps et l'unité
# d'un modèle BACI ; ses observations (retenues, cochées sous une condition, de test, exclues, des groupes
# d'une validation croisée d'un fichier .usp). Sa condition nomme les variables : elle ne change pas.
# Quand un numéro change, la demande de l'interface qui a fait le modèle (ses numéros sont ceux de
# l'autre côté) part.
renumeroter_definition <- function(d, carte) {
  change <- FALSE
  V <- function(x) {
    if (!length(x)) return(x)
    u <- as.numeric(unlist(x))
    r <- carte$v(u)
    if (!isTRUE(all(r == u))) change <<- TRUE
    r
  }
  O <- function(x) {
    if (!length(x)) return(x)
    u <- as.numeric(unlist(x))
    r <- carte$o(u)
    if (!isTRUE(all(r == u))) change <<- TRUE
    r
  }
  noms_v <- function(x) {
    n <- suppressWarnings(as.numeric(names(x)))
    if (!length(x) || anyNA(n)) return(x)
    names(x) <- as.character(numeros_entiers(V(n)))
    x
  }
  termes <- function(ts) {
    if (!length(ts)) return(ts)
    garde <- list()
    for (t in ts) {
      u <- as.numeric(unlist(t$variables))
      if (length(u)) {
        c0 <- vapply(seq_along(u), function(j) as.numeric(categorie(t, j)), 0)
        c1 <- vapply(seq_along(u), function(j) carte$c(u[j], c0[j]), 0)
        if (anyNA(c1)) {
          change <<- TRUE
          next
        }
        if (!identical(c1, c0)) change <<- TRUE
        t$variables <- V(u)
        if (!is.null(t$categories)) t$categories <- c1
      }
      garde[[length(garde) + 1L]] <- t
    }
    garde
  }
  groupement <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(x$by)) {
      by <- premier_texte(x$by)
      if (identical(by, "variable") && length(x$variable)) x$variable <- V(x$variable)
      if (length(x$pentes)) x$pentes <- V(x$pentes)
      if (identical(by, "groups") && length(x$observations)) x$observations <- O(x$observations)
    }
    for (i in seq_along(x)) if (is.list(x[[i]])) x[[i]] <- groupement(x[[i]])
    x
  }
  for (f in c("x", "y", "termes_complets")) if (length(d[[f]])) d[[f]] <- termes(d[[f]])
  for (f in c("classe", "lisses", "decalage")) if (length(d[[f]])) d[[f]] <- V(d[[f]])
  if (length(d$lisse_spatial$variables)) d$lisse_spatial$variables <- V(d$lisse_spatial$variables)
  if (length(d$groupe_classes$variable)) d$groupe_classes$variable <- V(d$groupe_classes$variable)
  if (length(d$k_courbes)) d$k_courbes <- noms_v(d$k_courbes)
  if (length(d$par_courbes)) d$par_courbes <- lapply(noms_v(d$par_courbes), V)
  for (f in c("groupes_vc", "aleatoires", "correlation", "options")) if (is.list(d[[f]])) d[[f]] <- groupement(d[[f]])
  if (is.list(d$options)) {
    if (length(d$options$z)) d$options$z <- V(d$options$z)
    for (f in c("z_transformations", "z_echelles")) if (is.list(d$options[[f]]) && length(d$options[[f]])) d$options[[f]] <- noms_v(d$options[[f]])
    for (f in switch(premier_texte(d$type), TRIM = "poids", DISTANCE = c("effort", "aire", "taille"), character(0))) {
      if (is.numeric(d$options[[f]])) d$options[[f]] <- V(d$options[[f]])
    }
  }
  if (is.list(d$moyennes)) for (f in c("facteur", "dans", "pente")) {
    x <- suppressWarnings(as.numeric(premier_texte(d$moyennes[[f]])))
    if (!is.na(x)) d$moyennes[[f]] <- as.character(numeros_entiers(V(x)))
  }
  if (is.list(d$baci_origine)) for (f in c("y", "temps", "unite")) {
    x <- premier_texte(d$baci_origine[[f]])
    if (!nzchar(x)) next
    if (startsWith(x, "id:")) {
      k <- carte$id(suppressWarnings(as.integer(substring(x, 4))))
      y <- if (!is.na(k)) paste0("id:", k) else x
      if (!identical(y, x)) change <- TRUE
      d$baci_origine[[f]] <- y
    } else if (!is.na(suppressWarnings(as.numeric(x)))) {
      d$baci_origine[[f]] <- V(as.numeric(x))
    }
  }
  for (f in c("observations", "cochees", "test", "exclues")) if (length(d[[f]])) d[[f]] <- O(d[[f]])
  if (change) d$demande <- NULL
  d
}

# ---------------------------------------------------------------- les modèles

# Une définition comparée à une autre : sans son nom, son numéro, ni la demande de l'interface qui l'a
# faite (ses numéros sont ceux d'une séance), ses champs dans l'ordre de leurs noms.
canon_definition <- function(d) {
  if (is.null(d)) return(NA_character_)
  d <- d[setdiff(names(d), c("name", "number", "demande"))]
  json_fusion(d[sort(names(d))])
}

# Les observations d'une définition (retenues, cochées, de test, exclues) après fusion, comme des
# ensembles : les miennes, sans celles qu'ils ont retirées, puis celles qu'ils ont ajoutées (une mise à
# jour des données d'un côté, et de nouvelles observations dans ses modèles).
ensemble_fusion <- function(b, m, l) {
  b <- as.numeric(unlist(b))
  m <- as.numeric(unlist(m))
  l <- as.numeric(unlist(l))
  r <- m[!m %in% setdiff(b, l)]
  c(r, setdiff(setdiff(l, b), r))
}

# Un terme d'un modèle par ce qui le fait (ses variables, les numéros de ses catégories, un facteur seul,
# ses décalages), et sa famille : les termes qui ne diffèrent que par la catégorie de leurs variables
# qualitatives (habitat(forêt), habitat(lande)… ; NA pour un terme sans catégorie).
cle_terme <- function(t) {
  u <- as.numeric(unlist(t$variables))
  paste(c(u, vapply(seq_along(u), function(j) as.numeric(categorie(t, j)), 0), isTRUE(t$facteur_seul), as.numeric(unlist(t$lags))), collapse = " ")
}
famille_terme <- function(t) {
  u <- as.numeric(unlist(t$variables))
  cats <- vapply(seq_along(u), function(j) as.numeric(categorie(t, j)), 0)
  if (!any(cats != 0) || length(unlist(t$lags))) return(NA_character_)
  paste(c(u, ifelse(cats != 0, -1, 0), isTRUE(t$facteur_seul)), collapse = " ")
}

# Les termes de chaque famille (famille_terme) rangés à leurs places dans l'ordre des catégories : dans
# une régression, la dernière catégorie dans l'ordre des termes est la référence (regression.R :
# candidats_regression), qui doit rester la dernière des catégories de la variable (atelier.R :
# suivre_termes ; etapes.R : termes_nouvelles_categories).
ranger_familles <- function(termes) {
  if (length(termes) < 2) return(termes)
  famille <- vapply(termes, famille_terme, "")
  for (f in unique(famille[!is.na(famille)])) {
    k <- which(famille %in% f)
    rang <- vapply(termes[k], function(t) {
      u <- as.numeric(unlist(t$variables))
      cats <- vapply(seq_along(u), function(j) as.numeric(categorie(t, j)), 0)
      sum(cats * 1000^(rev(seq_along(cats)) - 1))
    }, 0)
    if (is.unsorted(rang)) termes[k] <- termes[k][order(rang)]
  }
  termes
}

# Les termes d'un modèle (x, y) des trois versions fusionnés terme par terme (cle_terme) : ajouté d'un
# côté, gardé (après les termes de sa famille) ; retiré d'un côté et inchangé de l'autre, retiré ; changé
# d'un côté, ce côté. Deux mises à jour des données ajoutent chacune le terme de leur catégorie nouvelle :
# les deux restent. Rend list(termes, conflit : un terme changé autrement des deux côtés, ou retiré d'un
# côté et changé de l'autre).
termes_fusion <- function(b, m, l) {
  kb <- vapply(b, cle_terme, "")
  km <- vapply(m, cle_terme, "")
  kl <- vapply(l, cle_terme, "")
  tb <- vapply(b, json_fusion, "")
  tm <- vapply(m, json_fusion, "")
  tl <- vapply(l, json_fusion, "")
  conflit <- FALSE
  sortie <- list()
  for (i in seq_along(m)) {
    k <- km[i]
    jb <- match(k, kb)
    if (k %in% kl) {
      cote <- cote_fusion(if (!is.na(jb)) tb[jb] else NA_character_, tm[i], tl[match(k, kl)])
      if (cote == "conflit") conflit <- TRUE
      sortie[[length(sortie) + 1L]] <- if (cote == "leur") l[[match(k, kl)]] else m[[i]]
    } else if (!is.na(jb)) {
      if (identical(tm[i], tb[jb])) next  # (retiré de leur côté, inchangé du mien)
      conflit <- TRUE
      sortie[[length(sortie) + 1L]] <- m[[i]]
    } else {
      sortie[[length(sortie) + 1L]] <- m[[i]]
    }
  }
  for (j in seq_along(l)) {
    k <- kl[j]
    if (k %in% km) next
    jb <- match(k, kb)
    if (!is.na(jb)) {
      if (!identical(tl[j], tb[jb])) conflit <- TRUE
      next
    }
    f <- famille_terme(l[[j]])
    familles <- vapply(sortie, famille_terme, "")
    apres <- if (!is.na(f) && any(familles %in% f)) max(which(familles %in% f)) else length(sortie)
    sortie <- append(sortie, list(l[[j]]), after = apres)
  }
  list(termes = sortie, conflit = conflit)
}

# Une définition fusionnée champ par champ (dans la numérotation de la fusion) : chaque champ du côté
# qui l'a changé ; ses termes un à un (termes_fusion) ; ses observations comme des ensembles
# (ensemble_fusion). Rend list(d, conflits : les champs changés autrement des deux côtés).
fusion_definition <- function(b, m, l) {
  d <- m
  conflits <- character(0)
  for (f in setdiff(unique(c(names(m), names(l), names(b))), c("name", "number", "demande"))) {
    tous <- f %in% names(b) && f %in% names(m) && f %in% names(l)
    if (tous && f %in% c("observations", "cochees", "test", "exclues")) {
      d[[f]] <- ensemble_fusion(b[[f]], m[[f]], l[[f]])
      next
    }
    if (tous && f %in% c("x", "y", "termes_complets") && is.list(b[[f]]) && is.list(m[[f]]) && is.list(l[[f]])) {
      t <- termes_fusion(b[[f]], m[[f]], l[[f]])
      if (t$conflit) conflits <- c(conflits, f) else d[[f]] <- t$termes
      next
    }
    cote <- cote_fusion(texte_champ(b, f), texte_champ(m, f), texte_champ(l, f))
    if (cote == "conflit") conflits <- c(conflits, f)
    else if (cote == "leur") {
      if (f %in% names(l)) d[f] <- list(l[[f]]) else d[[f]] <- NULL
    }
  }
  list(d = d, conflits = conflits)
}

# Le type d'un modèle pour une phrase : PCA, PLS, OPLS-DA, LM…
type_modele_fusion <- function(d) paste0(if (isTRUE(d$opls)) "O" else "", if (isTRUE(d$plsda)) "PLS-DA" else premier_texte(d$type))

# Ce qu'un côté a fait d'un modèle depuis la base (`b`, `x` : ses définitions, NULL quand il n'y est pas),
# en peu de mots : « M3 : 2 composantes → 4 composantes ; X : + x5 ».
resume_modele_fusion <- function(nom, b, x) {
  if (is.null(x)) return(phrase_fusion("%s : retiré", "%s: removed", nom))
  if (is.null(b)) return(phrase_fusion("%s : ajouté (%s)", "%s: added (%s)", nom, type_modele_fusion(x)))
  morceaux <- character(0)
  if (!identical(type_modele_fusion(b), type_modele_fusion(x))) morceaux <- c(morceaux, sprintf("%s → %s", type_modele_fusion(b), type_modele_fusion(x)))
  ab <- as.numeric(b$A %||% 0)
  ax <- as.numeric(x$A %||% 0)
  auto <- function(a) if (a > 0) a else traduire("auto", "auto")
  if (!identical(ab, ax)) morceaux <- c(morceaux, phrase_fusion("%s composante(s) → %s composante(s)", "%s component(s) → %s component(s)", auto(ab), auto(ax)))
  for (f in c("x", "y")) {
    nb <- vapply(b[[f]] %||% list(), function(t) premier_texte(t$name), "")
    nx <- vapply(x[[f]] %||% list(), function(t) premier_texte(t$name), "")
    plus <- setdiff(nx, nb)
    moins <- setdiff(nb, nx)
    if (length(plus) || length(moins)) {
      morceaux <- c(morceaux, paste0(toupper(f), traduire(" : ", ": "), paste(c(if (length(plus)) paste("+", noms_fusion(plus, 3)), if (length(moins)) paste("−", noms_fusion(moins, 3))), collapse = ", ")))
    } else if (!identical(texte_champ(b, f), texte_champ(x, f))) {
      morceaux <- c(morceaux, phrase_fusion("réglages de %s", "%s settings", toupper(f)))
    }
  }
  nob <- length(unlist(b$observations))
  nox <- length(unlist(x$observations))
  if (nob != nox) morceaux <- c(morceaux, phrase_fusion("%d → %d observations", "%d → %d observations", nob, nox))
  libelles <- list(condition = traduire("condition", "condition"), test = traduire("jeu de test", "test set"), note = traduire("note", "note"),
                   title = traduire("titre", "title"))
  for (f in names(libelles)) if (!identical(texte_champ(b, f), texte_champ(x, f))) morceaux <- c(morceaux, libelles[[f]])
  if (!length(morceaux)) morceaux <- traduire("réglages", "settings")
  sprintf(traduire("%s : %s", "%s: %s"), nom, paste(morceaux, collapse = traduire(" ; ", "; ")))
}

# Le nom libre suivant pour un modèle : « M5 » devient « M6 », « M7 »… ; un nom sans numéro, « nom (2) ».
nom_modele_libre <- function(nom, pris) {
  m <- regmatches(nom, regexec("^(.*?)(\\d+)$", nom))[[1]]
  if (length(m)) {
    k <- as.numeric(m[3])
    repeat {
      k <- k + 1
      n <- paste0(m[2], k)
      if (!n %in% pris) return(n)
    }
  }
  nom_libre(nom, pris)
}

# Les modèles des trois versions (leurs définitions déjà dans la numérotation de la fusion : `dB`, `dM`,
# `dL`, des listes nommées par leur nom) fusionnés par leur nom : ajouté d'un côté, gardé ; retiré d'un
# côté et inchangé de l'autre, retiré ; changé d'un côté, ce côté (fusion_definition, champ par champ) ;
# changé autrement des deux côtés, ou retiré d'un côté et changé de l'autre, un conflit, la définition
# entière du côté choisi. Deux modèles nouveaux du même nom et de définitions différentes restent tous
# deux : le leur prend le nom libre suivant. Les numéros : les miens restent ; les leurs, quand aucun des
# miens ne les a, sinon les suivants. Rend list(modeles, noms_leur : le nom dans la fusion de chacun de
# leurs modèles ; seul : le côté de chaque modèle nouveau d'un seul côté ; numeros : pour chaque côté, le
# numéro dans la fusion de chacun de ses numéros).
fusion_modeles <- function(B, M, L, dB, dM, dL, ctx) {
  nM <- names(dM)
  nL <- names(dL)
  pris <- unique(c(names(dB), nM, nL))
  conflit_modele <- function(nom, b, m, l, champs) {
    conflit_fusion(ctx, paste0("modele:", nom), "modele", nom, phrase_fusion("Modèle %s", "Model %s", nom),
                   resume_modele_fusion(nom, b, m), resume_modele_fusion(nom, b, l), list(champs = I(champs)))
  }
  resultat <- list()
  origine <- character(0)
  seul <- character(0)  # (les modèles nouveaux d'un seul côté : ce côté)
  vers_leur <- stats::setNames(character(0), character(0))
  renommes <- character(0)
  for (nom in nM) {
    b <- dB[[nom]]
    m <- dM[[nom]]
    l <- dL[[nom]]
    d <- m
    if (!is.null(l) && !is.null(b)) {
      f <- fusion_definition(b, m, l)
      d <- f$d
      if (length(f$conflits)) d <- if (conflit_modele(nom, b, m, l, f$conflits) == "leur") l else m
      else if (canon_definition(d) == canon_definition(l) && canon_definition(d) != canon_definition(m)) d$demande <- l$demande
      vers_leur[[nom]] <- nom
    } else if (!is.null(l)) {
      vers_leur[[nom]] <- nom
      if (canon_definition(m) != canon_definition(l)) {
        nouveau <- nom_modele_libre(nom, pris)
        pris <- c(pris, nouveau)
        vers_leur[[nom]] <- nouveau
        renommes[[nom]] <- nouveau
        seul[[nom]] <- "mien"
      }
    } else if (!is.null(b)) {
      if (canon_definition(m) == canon_definition(b) || conflit_modele(nom, b, m, NULL, "retire") == "leur") next
    } else {
      seul[[nom]] <- "mien"
    }
    resultat[[nom]] <- d
    origine[[nom]] <- "mien"
  }
  for (nom in nL) {
    l <- dL[[nom]]
    if (nom %in% names(renommes)) {
      l$name <- renommes[[nom]]
      resultat[[renommes[[nom]]]] <- l
      origine[[renommes[[nom]]]] <- "leur"
      seul[[renommes[[nom]]]] <- "leur"
      ctx$renommes <- c(ctx$renommes, phrase_fusion("leur %s renommé %s (un %s a été fait des deux côtés)", "their %s renamed %s (an %s was made on both sides)",
                                                    nom, renommes[[nom]], nom))
      next
    }
    if (nom %in% nM) next
    b <- dB[[nom]]
    if (!is.null(b) && (canon_definition(l) == canon_definition(b) || conflit_modele(nom, b, NULL, l, "retire") == "mien")) next
    resultat[[nom]] <- l
    origine[[nom]] <- "leur"
    if (is.null(b)) seul[[nom]] <- "leur"
    vers_leur[[nom]] <- nom
  }
  # (les numéros : les miens restent ; les leurs, quand aucun des miens ne les a, sinon les suivants)
  numero <- function(d) suppressWarnings(as.numeric(premier_texte(d$number, NA)))
  numeros_de <- function(ms) stats::setNames(vapply(ms %||% list(), numero, 0), vapply(ms %||% list(), function(d) premier_texte(d$name), ""))
  du_mien <- numeros_de(M$models)
  du_leur <- numeros_de(L$models)
  prochain <- max(c(0, du_mien, du_leur, numeros_de(B$models)), na.rm = TRUE)
  depuis_leur <- stats::setNames(names(vers_leur), vers_leur)
  utilises <- numeric(0)
  for (nom in names(resultat)) {
    n <- if (identical(origine[[nom]], "mien")) unname(du_mien[nom]) else {
      ancien <- unname(depuis_leur[nom])
      x <- unname(du_leur[if (is.na(ancien)) nom else ancien])
      if (!is.na(x) && x %in% du_mien) NA_real_ else x
    }
    if (!length(n) || is.na(n) || n %in% utilises) {
      prochain <- prochain + 1
      n <- prochain
    }
    utilises <- c(utilises, n)
    resultat[[nom]]$number <- numeros_entiers(n)
  }
  numeros_cote <- function(ms, vers) {
    r <- stats::setNames(numeric(0), character(0))
    for (d in ms %||% list()) {
      n <- numero(d)
      nom <- unname(vers[premier_texte(d$name)])
      if (is.na(n) || is.na(nom) || is.null(resultat[[nom]])) next
      r[[as.character(n)]] <- as.numeric(resultat[[nom]]$number)
    }
    r
  }
  identite <- function(ms) {
    n <- vapply(ms %||% list(), function(d) premier_texte(d$name), "")
    stats::setNames(n, n)
  }
  list(modeles = unname(resultat), noms_leur = vers_leur, seul = seul,
       numeros = list(base = numeros_cote(B$models, identite(B$models)), mien = numeros_cote(M$models, identite(M$models)), leur = numeros_cote(L$models, vers_leur)))
}

# Un modèle nouveau d'un seul côté (`tX` : le tableau de ce côté) dans les données fusionnées (`p`, `tF`) :
# ce que l'autre côté a ajouté aux données le rejoint, comme après une mise à jour des données (etapes.R :
# aligner_projet). Les observations nouvelles pour lui qu'il admet (definition_suivie : la classe d'une
# PLS-DA, celle d'un modèle de classe, les unités d'un modèle BACI ; celles qu'il avait laissées de côté
# le restent), et les termes des catégories nouvelles pour lui, avec les réglages des autres
# (termes_nouvelles_categories ; une PLS-DA garde ses classes). Fait sur les données d'avant, il serait
# sinon un modèle des seules données de son côté.
suivre_donnees_fusion <- function(d, p, tF, tX) {
  nouvelles <- unname(as.numeric(tF$uid_o[setdiff(tF$cles_o, tX$cles_o)]))
  if (length(nouvelles)) d <- definition_suivie(d, p, nouvelles, numeric(0))
  for (k in intersect(tF$cles_v, tX$cles_v)) {
    cats_f <- as.character(unlist(tF$vars[[k]]$categories))
    cats_x <- as.character(unlist(tX$vars[[k]]$categories))
    neuves <- setdiff(cats_f, cats_x)
    if (!length(cats_x) || !length(neuves)) next
    for (f in c("x", if (!isTRUE(d$plsda)) "y", "termes_complets")) {
      if (!length(d[[f]])) next
      r <- termes_nouvelles_categories(d[[f]], p, as.numeric(tF$uid_v[[k]]), match(neuves, cats_f))
      if (length(attr(r, "ajoutees"))) {
        d$solution <- NULL
        d$reference <- list(ynames = d[["reference"]][["ynames"]], note = d[["reference"]][["note"]])
      }
      attr(r, "ajoutees") <- NULL
      d[[f]] <- r
    }
  }
  d
}

# Ce que chaque côté a fait des modèles, en phrases : ajoutés, retirés, changés (dB, dX : les définitions
# de la base et du côté, dans la numérotation de la fusion).
apports_modeles <- function(dB, dX, ctx, cote) {
  ajoutes <- setdiff(names(dX), names(dB))
  retires <- setdiff(names(dB), names(dX))
  changes <- Filter(function(n) canon_definition(dB[[n]]) != canon_definition(dX[[n]]), intersect(names(dX), names(dB)))
  if (length(ajoutes)) apport_fusion(ctx, cote, phrase_fusion("modèle(s) ajouté(s) : %s", "model(s) added: %s", noms_fusion(ajoutes)))
  if (length(retires)) apport_fusion(ctx, cote, phrase_fusion("modèle(s) retiré(s) : %s", "model(s) removed: %s", noms_fusion(retires)))
  for (n in utils::head(changes, 4)) apport_fusion(ctx, cote, resume_modele_fusion(n, dB[[n]], dX[[n]]))
  if (length(changes) > 4) apport_fusion(ctx, cote, phrase_fusion("%d autres modèles changés", "%d more models changed", length(changes) - 4))
}

# ---------------------------------------------------------------- les documents et les figures

# Les lignes d'un texte (un texte vide n'en a aucune ; une fin de ligne finale fait une dernière ligne
# vide, pour le récrire tel quel).
lignes_texte <- function(texte) {
  texte <- premier_texte(texte)
  if (!nzchar(texte)) return(character(0))
  x <- strsplit(texte, "\n", fixed = TRUE)[[1]]
  if (endsWith(texte, "\n")) x <- c(x, "")
  x
}

# Les lignes communes de deux textes (des vecteurs de lignes), par l'algorithme de Myers (1986) : pour
# chaque ligne de `a`, le numéro de la ligne de `b` qui lui correspond, NA pour une ligne qui n'y est
# plus. Le début et la fin communs d'abord : un document ne change d'ordinaire qu'à quelques endroits.
# Au-delà de 2 000 différences, le milieu est tenu pour entièrement changé (une fusion ligne à ligne n'y
# aurait plus de sens, et la recherche coûterait trop).
lignes_communes <- function(a, b) {
  n <- length(a)
  m <- length(b)
  r <- rep(NA_integer_, n)
  d <- 0L
  while (d < n && d < m && identical(a[d + 1L], b[d + 1L])) d <- d + 1L
  f <- 0L
  while (f < n - d && f < m - d && identical(a[n - f], b[m - f])) f <- f + 1L
  if (d) r[seq_len(d)] <- seq_len(d)
  if (f) r[seq.int(n - f + 1L, n)] <- seq.int(m - f + 1L, m)
  if (n - f <= d || m - f <= d) return(r)
  ia <- seq.int(d + 1L, n - f)
  ib <- seq.int(d + 1L, m - f)
  codes <- match(c(a[ia], b[ib]), unique(c(a[ia], b[ib])))
  x <- codes[seq_along(ia)]
  y <- codes[length(ia) + seq_along(ib)]
  N <- length(x)
  Mb <- length(y)
  MAX <- min(N + Mb, 2000L)
  off <- MAX + 2L
  V <- integer(2L * MAX + 4L)
  traces <- vector("list", MAX + 1L)
  fin <- -1L
  for (D in 0:MAX) {
    traces[[D + 1L]] <- V
    for (k in seq.int(-D, D, by = 2L)) {
      x0 <- if (k == -D || (k != D && V[k - 1L + off] < V[k + 1L + off])) V[k + 1L + off] else V[k - 1L + off] + 1L
      y0 <- x0 - k
      while (x0 < N && y0 < Mb && x[x0 + 1L] == y[y0 + 1L]) {
        x0 <- x0 + 1L
        y0 <- y0 + 1L
      }
      V[k + off] <- x0
      if (x0 >= N && y0 >= Mb) {
        fin <- D
        break
      }
    }
    if (fin >= 0L) break
  }
  if (fin < 0L) return(r)
  xx <- N
  yy <- Mb
  for (D in fin:0) {
    Vp <- traces[[D + 1L]]
    k <- xx - yy
    kp <- if (k == -D || (k != D && Vp[k - 1L + off] < Vp[k + 1L + off])) k + 1L else k - 1L
    xp <- Vp[kp + off]
    yp <- xp - kp
    while (xx > xp && yy > yp) {
      r[ia[xx]] <- ib[yy]
      xx <- xx - 1L
      yy <- yy - 1L
    }
    xx <- xp
    yy <- yp
  }
  r
}

# Trois versions d'un texte (des vecteurs de lignes) fusionnées ligne à ligne (diff3) : chaque passage
# changé d'un seul côté prend ce côté, changé de la même façon des deux côtés aussi ; changé autrement
# des deux côtés, c'est un passage en conflit, qui prend le côté `cote`. Rend list(lignes, passages : les
# passages en conflit, list(debut : le numéro de sa première ligne dans la base, base, mien, leur)).
fusion_lignes <- function(base, mien, leur, cote = "mien") {
  am <- lignes_communes(base, mien)
  al <- lignes_communes(base, leur)
  nb <- length(base)
  nm <- length(mien)
  nl <- length(leur)
  ib <- 0L
  im <- 0L
  il <- 0L
  sortie <- character(0)
  passages <- list()
  repeat {
    i <- 1L
    while (ib + i <= nb && isTRUE(am[ib + i] == im + i) && isTRUE(al[ib + i] == il + i)) i <- i + 1L
    if (i > 1L) {
      sortie <- c(sortie, base[ib + seq_len(i - 1L)])
      ib <- ib + i - 1L
      im <- im + i - 1L
      il <- il + i - 1L
      next
    }
    j <- 1L
    while (ib + j <= nb && (is.na(am[ib + j]) || is.na(al[ib + j]))) j <- j + 1L
    if (ib + j <= nb) {
      jb <- ib + j
      jm <- am[jb]
      jl <- al[jb]
    } else {
      jb <- nb + 1L
      jm <- nm + 1L
      jl <- nl + 1L
    }
    cb <- base[ib + seq_len(jb - 1L - ib)]
    cm <- mien[im + seq_len(jm - 1L - im)]
    cl <- leur[il + seq_len(jl - 1L - il)]
    if (identical(cm, cb)) sortie <- c(sortie, cl)
    else if (identical(cl, cb) || identical(cm, cl)) sortie <- c(sortie, cm)
    else {
      passages[[length(passages) + 1L]] <- list(debut = ib + 1L, base = cb, mien = cm, leur = cl)
      sortie <- c(sortie, if (cote == "leur") cl else cm)
    }
    ib <- jb - 1L
    im <- jm - 1L
    il <- jl - 1L
    if (jb > nb) break
  }
  list(lignes = sortie, passages = passages)
}

# Ce qu'un côté a fait des passages en conflit d'un texte, en peu de mots : « paragraphe 3 réécrit »,
# « paragraphes 2 et 3 retirés », « texte ajouté après le paragraphe 4 » (les paragraphes de la base,
# séparés par des lignes vides).
resume_passages <- function(base, passages, cote) {
  fr <- !identical(getOption("nuee.langue"), "en")
  pleines <- nzchar(trimws(base))
  paragraphe <- cumsum(pleines & !c(FALSE, utils::head(pleines, -1)))
  morceaux <- vapply(passages, function(p) {
    lignes <- p$debut + seq_along(p$base) - 1L
    numeros <- unique(paragraphe[lignes])
    numeros <- numeros[numeros > 0]
    if (!length(p$base) || !length(numeros)) {
      avant <- if (p$debut > 1L) paragraphe[p$debut - 1L] else 0L
      return(if (avant > 0) phrase_fusion("texte ajouté après le paragraphe %d", "text added after paragraph %d", avant)
             else traduire("texte ajouté au début", "text added at the start"))
    }
    plusieurs <- length(numeros) > 1
    quels <- liste_mots(numeros, fr)
    if (!any(nzchar(trimws(p[[cote]])))) {
      return(if (plusieurs) phrase_fusion("paragraphes %s retirés", "paragraphs %s removed", quels) else phrase_fusion("paragraphe %s retiré", "paragraph %s removed", quels))
    }
    if (plusieurs) phrase_fusion("paragraphes %s réécrits", "paragraphs %s rewritten", quels) else phrase_fusion("paragraphe %s réécrit", "paragraph %s rewritten", quels)
  }, "")
  paste(unique(morceaux), collapse = traduire(" ; ", "; "))
}

# Les références aux modèles d'un texte de document ({{M5.Q2}} : interface/references.js) qui suivent
# leur nouveau nom : `renommer`, le nouveau nom de chaque modèle renommé, nommé par l'ancien ; `noms`,
# ceux de tous les modèles du côté du document (le plus long nom suivi d'un point est celui de la
# référence, comme references.js : separerCible).
renommer_references <- function(texte, noms, renommer) {
  texte <- premier_texte(texte)
  if (!length(renommer) || !grepl("{{", texte, fixed = TRUE)) return(texte)
  noms <- noms[order(-nchar(noms))]
  m <- gregexpr("\\{\\{([\\s\\S]*?)\\}\\}", texte, perl = TRUE)
  refs <- regmatches(texte, m)[[1]]
  if (!length(refs)) return(texte)
  nouvelles <- vapply(refs, function(r) {
    interieur <- substr(r, 3, nchar(r) - 2)
    debut <- regmatches(interieur, regexpr("^\\s*", interieur))
    cible <- substring(interieur, nchar(debut) + 1L)
    if (grepl("^[<%]", cible)) return(r)
    for (n in noms) {
      if (startsWith(cible, paste0(n, ".")) && nchar(cible) > nchar(n) + 1L) {
        if (!n %in% names(renommer)) return(r)
        return(paste0("{{", debut, renommer[[n]], substring(cible, nchar(n) + 1L), "}}"))
      }
    }
    r
  }, "", USE.NAMES = FALSE)
  regmatches(texte, m) <- list(nouvelles)
  texte
}

# Le contenu d'une figure (le texte JSON que l'interface seule relit : interface/figures.js) dont les
# panneaux suivent les numéros des modèles dans la fusion : `numeros`, le numéro dans la fusion de chaque
# numéro de son côté ; `noms`, le nom de chaque modèle de la fusion par son numéro. Un panneau garde son
# modèle par son numéro (numero, nom) et ses réglages en codes « m@3|t1 ». Rendu tel quel quand rien n'y
# change, ou quand il ne se lit pas.
renumeroter_figure <- function(contenu, numeros, noms) {
  contenu <- premier_texte(contenu)
  if (!length(numeros) || !grepl("numero|@", contenu)) return(contenu)
  x <- tryCatch(jsonlite::fromJSON(contenu, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(x) || !is.list(x$panneaux)) return(contenu)
  vers <- function(n) {
    v <- unname(numeros[as.character(n)])
    if (length(v) != 1 || is.na(v)) n else v
  }
  change <- FALSE
  for (i in seq_along(x$panneaux)) {
    pn <- x$panneaux[[i]]
    if (!is.list(pn)) next
    n <- suppressWarnings(as.numeric(unlist(pn$numero)))
    if (length(n) == 1 && !is.na(n) && vers(n) != n) {
      pn$numero <- numeros_entiers(vers(n))
      nom <- unname(noms[as.character(pn$numero)])
      if (!is.null(pn$nom) && length(nom) == 1 && !is.na(nom)) pn$nom <- nom
      change <- TRUE
    }
    if (is.list(pn$loc)) for (k in seq_along(pn$loc)) {
      v <- pn$loc[[k]]
      if (!is.character(v) || length(v) != 1) next
      r <- regmatches(v, regexec("^(m|[rq]:)@(\\d+)(\\|.*)?$", v))[[1]]
      if (!length(r) || vers(as.numeric(r[3])) == as.numeric(r[3])) next
      pn$loc[[k]] <- paste0(r[2], "@", numeros_entiers(vers(as.numeric(r[3]))), r[4])
      change <- TRUE
    }
    x$panneaux[[i]] <- pn
  }
  if (change) json_fusion(x) else contenu
}

# Les éléments (documents ou figures) des trois versions fusionnés par leur identifiant : ajouté d'un
# côté, gardé ; nouveau des deux côtés sous le même identifiant, les deux gardés (le leur sous un nouvel
# identifiant : `prefixe` et le numéro libre suivant) sauf s'ils sont les mêmes ; retiré d'un côté et
# inchangé de l'autre, retiré ; retiré d'un côté et changé de l'autre, un conflit. Les autres passent
# par `fusionner(b, m, l)`. `champs` : ceux qui font l'élément (pour savoir s'il a changé).
fusion_elements <- function(eb, em, el, champs, prefixe, genre, titre, fusionner, ctx) {
  ids <- function(x) vapply(x, function(e) premier_texte(e$id), "")
  ib <- ids(eb)
  im <- ids(em)
  il <- ids(el)
  meme <- function(a, b) identical(texte_champs(a, champs), texte_champs(b, champs))
  numero <- function(x) suppressWarnings(as.numeric(sub(paste0("^", prefixe), "", x)))
  prochain <- max(c(0, numero(c(ib, im, il))), na.rm = TRUE)
  sortie <- list()
  for (k in seq_along(em)) {
    m <- em[[k]]
    id <- im[k]
    b <- if (id %in% ib) eb[[match(id, ib)]]
    l <- if (id %in% il) el[[match(id, il)]]
    if (is.null(l)) {
      if (!is.null(b) && (meme(m, b) || conflit_fusion(ctx, paste0(genre, ":", id), genre, id, titre(m), traduire("changé", "changed"), traduire("retiré", "removed")) == "leur")) next
      sortie[[length(sortie) + 1L]] <- m
    } else if (is.null(b)) {
      sortie[[length(sortie) + 1L]] <- m
      if (!meme(m, l)) {
        prochain <- prochain + 1
        l$id <- paste0(prefixe, prochain)
        sortie[[length(sortie) + 1L]] <- l
      }
    } else {
      sortie[[length(sortie) + 1L]] <- fusionner(b, m, l)
    }
  }
  for (k in seq_along(el)) {
    if (il[k] %in% im) next
    l <- el[[k]]
    if (il[k] %in% ib) {
      b <- eb[[match(il[k], ib)]]
      if (meme(l, b) || conflit_fusion(ctx, paste0(genre, ":", il[k]), genre, il[k], titre(l), traduire("retiré", "removed"), traduire("changé", "changed")) == "mien") next
    }
    sortie[[length(sortie) + 1L]] <- l
  }
  sortie
}

# Les documents des trois versions fusionnés (fusion_elements) : le titre, la langue, les références et
# le style de citation champ par champ, le texte ligne à ligne (fusion_lignes) ; les passages et les
# champs changés autrement des deux côtés font un conflit pour le document, et chacun prend le côté
# choisi. Leurs documents d'abord avec les références qui suivent leurs modèles renommés (`noms_leur` :
# le nom dans la fusion de chacun de leurs modèles).
fusion_documents <- function(B, M, L, noms_leur, ctx) {
  renommer <- noms_leur[names(noms_leur) != noms_leur]
  noms <- vapply(L$models %||% list(), function(x) premier_texte(x$name), "")
  el <- lapply(L$documents %||% list(), function(d) {
    if (length(renommer)) d$texte <- renommer_references(d$texte, noms, renommer)
    d
  })
  champs <- c("titre", "langue", "texte", "bib", "style")
  titre <- function(d) phrase_fusion("Document %s", "Document %s", guillemets_fusion(premier_texte(d$titre, premier_texte(d$id))))
  fusionner <- function(b, m, l) {
    d <- m
    conflits <- character(0)
    for (f in setdiff(champs, "texte")) {
      cote <- cote_fusion(texte_champ(b, f), texte_champ(m, f), texte_champ(l, f))
      if (cote == "conflit") conflits <- c(conflits, f)
      else if (cote == "leur") d <- avec_champ(d, f, l[[f]])
    }
    lb <- lignes_texte(b$texte)
    lm <- lignes_texte(m$texte)
    ll <- lignes_texte(l$texte)
    t <- fusion_lignes(lb, lm, ll)
    if (length(conflits) || length(t$passages)) {
      resume <- function(cote) paste(c(if (length(t$passages)) resume_passages(lb, t$passages, cote),
                                       vapply(conflits, function(f) phrase_fusion("%s changé", "%s changed", f), "")), collapse = traduire(" ; ", "; "))
      passage <- function(p) list(ligne = p$debut, base = paste(p$base, collapse = "\n"), mien = paste(p$mien, collapse = "\n"), leur = paste(p$leur, collapse = "\n"))
      cote <- conflit_fusion(ctx, paste0("document:", premier_texte(m$id)), "document", premier_texte(m$id), titre(m), resume("mien"), resume("leur"),
                             list(passages = lapply(t$passages, passage), champs = I(conflits)))
      if (cote == "leur") {
        t <- fusion_lignes(lb, lm, ll, "leur")
        for (f in conflits) d <- avec_champ(d, f, l[[f]])
      }
    }
    d$texte <- paste(t$lignes, collapse = "\n")
    if (!identical(texte_champs(d, champs), texte_champs(m, champs))) d$change <- max(premier_texte(m$change), premier_texte(l$change))
    d
  }
  fusion_elements(B$documents %||% list(), M$documents %||% list(), el, champs, "D", "document", titre, fusionner, ctx)
}

# Les figures des trois versions fusionnées (fusion_elements), comme les modèles : changée d'un côté, ce
# côté ; changée autrement des deux côtés, un conflit (la figure entière du côté choisi). Leurs panneaux
# d'abord rapportés aux numéros des modèles dans la fusion (renumeroter_figure ; `numeros`, pour chaque
# côté ; `noms`, le nom de chaque modèle de la fusion par son numéro).
fusion_figures <- function(B, M, L, numeros, noms, ctx) {
  suivre <- function(fs, n) lapply(fs %||% list(), function(f) { f$contenu <- renumeroter_figure(f$contenu, n, noms); f })
  champs <- c("titre", "contenu")
  titre <- function(f) phrase_fusion("Figure %s", "Figure %s", guillemets_fusion(premier_texte(f$titre, premier_texte(f$id))))
  panneaux <- function(f) length(tryCatch(jsonlite::fromJSON(premier_texte(f$contenu, "{}"), simplifyVector = FALSE), error = function(e) NULL)$panneaux)
  resume <- function(b, x) {
    paste(c(if (!identical(texte_champ(b, "titre"), texte_champ(x, "titre"))) phrase_fusion("titre %s", "title %s", guillemets_fusion(premier_texte(x$titre))),
            if (!identical(texte_champ(b, "contenu"), texte_champ(x, "contenu"))) {
              if (panneaux(b) != panneaux(x)) phrase_fusion("%d → %d panneaux", "%d → %d panels", panneaux(b), panneaux(x))
              else traduire("mise en page ou panneaux changés", "layout or panels changed")
            }), collapse = traduire(" ; ", "; "))
  }
  fusionner <- function(b, m, l) {
    d <- m
    conflits <- character(0)
    for (f in champs) {
      cote <- cote_fusion(texte_champ(b, f), texte_champ(m, f), texte_champ(l, f))
      if (cote == "conflit") conflits <- c(conflits, f)
      else if (cote == "leur") d <- avec_champ(d, f, l[[f]])
    }
    if (length(conflits)) {
      cote <- conflit_fusion(ctx, paste0("figure:", premier_texte(m$id)), "figure", premier_texte(m$id), titre(m), resume(b, m), resume(b, l), list(champs = I(conflits)))
      d <- if (cote == "leur") l else m
    }
    if (!identical(texte_champs(d, champs), texte_champs(m, champs))) d$change <- max(premier_texte(m$change), premier_texte(l$change))
    d
  }
  fusion_elements(suivre(B$figures, numeros$base), suivre(M$figures, numeros$mien), suivre(L$figures, numeros$leur), champs, "F", "figure", titre, fusionner, ctx)
}

# Ce que chaque côté a fait des documents ou des figures, en phrases.
apports_elements <- function(eb, ex, champs, ctx, cote, phrases) {
  ids <- function(x) vapply(x %||% list(), function(e) premier_texte(e$id), "")
  titres <- function(x, quels) vapply(Filter(function(e) premier_texte(e$id) %in% quels, x %||% list()), function(e) guillemets_fusion(premier_texte(e$titre, premier_texte(e$id))), "")
  ib <- ids(eb)
  ix <- ids(ex)
  changes <- Filter(function(id) !identical(texte_champs(eb[[match(id, ib)]], champs), texte_champs(ex[[match(id, ix)]], champs)), intersect(ix, ib))
  if (length(setdiff(ix, ib))) apport_fusion(ctx, cote, sprintf(phrases[1], noms_fusion(titres(ex, setdiff(ix, ib)))))
  if (length(setdiff(ib, ix))) apport_fusion(ctx, cote, sprintf(phrases[2], noms_fusion(titres(eb, setdiff(ib, ix)))))
  if (length(changes)) apport_fusion(ctx, cote, sprintf(phrases[3], noms_fusion(titres(ex, changes))))
}

# ---------------------------------------------------------------- le reste du projet

# Les coordonnées (spatial.R : p$coordonnees, deux variables et leur système) des trois versions
# fusionnées par les noms de leurs variables ; changées autrement des deux côtés, un conflit. `tF` : le
# tableau de la fusion, où elles prennent leurs numéros.
fusion_coordonnees <- function(tB, tM, tL, tF, ctx) {
  m <- coordonnees_canon(tM)
  l <- coordonnees_canon(tL)
  cote <- cote_fusion(json_fusion(coordonnees_canon(tB)), json_fusion(m), json_fusion(l))
  if (cote == "conflit") cote <- conflit_coordonnees(ctx, m, l)
  x <- if (cote == "leur") l else m
  if (is.null(x) || is.na(x$x) || is.na(x$y) || !x$x %in% tF$cles_v || !x$y %in% tF$cles_v) return(NULL)
  list(x = tF$uid_v[[x$x]], y = tF$uid_v[[x$y]], systeme = x$systeme)
}

# Les jeux de prédiction (p$prediction_sets) des trois versions fusionnés par leur nom, leurs
# observations par leur identifiant ; changé autrement des deux côtés, un conflit.
fusion_predictions <- function(tB, tM, tL, cartes, ctx) {
  canon <- function(t) {
    cles <- stats::setNames(t$cles_o, as.character(t$uid_o))
    s <- t$p$prediction_sets %||% list()
    stats::setNames(lapply(s, function(x) { x$observations <- unname(cles[as.character(numeros_entiers(x$observations))]); json_fusion(x) }),
                    vapply(s, function(x) premier_texte(x$name), ""))
  }
  b <- canon(tB)
  m <- canon(tM)
  l <- canon(tL)
  pour <- function(t, carte, nom) {
    x <- Find(function(s) identical(premier_texte(s$name), nom), t$p$prediction_sets %||% list())
    o <- carte$o(x$observations)
    x$observations <- numeros_entiers(o[o > 0])
    x
  }
  sortie <- list()
  for (nom in unique(c(names(m), names(l)))) {
    vb <- if (nom %in% names(b)) b[[nom]] else NA_character_
    vm <- if (nom %in% names(m)) m[[nom]] else NA_character_
    vl <- if (nom %in% names(l)) l[[nom]] else NA_character_
    cote <- cote_fusion(vb, vm, vl)
    if (cote == "conflit") {
      dit <- function(v) if (is.na(v)) traduire("retiré", "removed") else traduire("changé", "changed")
      cote <- conflit_fusion(ctx, paste0("champ:prediction:", nom), "champ", nom, phrase_fusion("Jeu de prédiction %s", "Prediction set %s", guillemets_fusion(nom)), dit(vm), dit(vl))
    }
    if (cote == "leur" && !is.na(vl)) sortie[[length(sortie) + 1L]] <- pour(tL, cartes$leur, nom)
    else if (cote == "mien" && !is.na(vm)) sortie[[length(sortie) + 1L]] <- pour(tM, cartes$mien, nom)
  }
  sortie
}

# Les champs d'un projet que la fusion fait elle-même ; les autres (le format, le nom de la source,
# l'historique d'un fichier .usp, les réglages d'un import de KoboToolbox…) se fusionnent champ par champ.
CHAMPS_FUSIONNES <- c("observations", "variables", "datasets", "observation_id_names", "donnees", "noms_obs", "noms_var", "models",
                      "documents", "figures", "source_donnees", "etapes", "empreinte_donnees", "session", "nuee", "coordonnees",
                      "prediction_sets", CHAMPS_SEANCE_FUSION)

# ---------------------------------------------------------------- la fusion

# Les phrases de ce qu'un côté a changé aux données (`d` : changements_fusion ; `X` : son projet, `B` la
# base) : la mise à jour depuis un fichier, les étapes ajoutées, les variables et les observations
# ajoutées ou retirées, les cellules changées, les nouvelles catégories, les unités et descriptions.
apports_donnees <- function(B, X, d, ctx, cote) {
  if (is.list(X$source_donnees) && !meme_source(B$source_donnees, X$source_donnees)) {
    apport_fusion(ctx, cote, phrase_fusion("données mises à jour de %s", "data updated from %s", basename(premier_texte(X$source_donnees$chemin))))
  }
  eb <- cles_etapes(B)
  neuves <- Filter(function(e) !cle_etape(e, TRUE) %in% eb, X$etapes %||% list())
  if (length(neuves)) apport_fusion(ctx, cote, phrase_fusion("%d étape(s) : %s", "%d step(s): %s", length(neuves), noms_fusion(vapply(neuves, function(e) premier_texte(e$etiquette), ""), 3)))
  vars <- function(k) k[!startsWith(k, ID_FUSION)]
  if (length(vars(d$ajoutees))) apport_fusion(ctx, cote, phrase_fusion("variable(s) ajoutée(s) : %s", "variable(s) added: %s", noms_fusion(cle_lisible(vars(d$ajoutees)))))
  if (length(vars(d$retirees))) apport_fusion(ctx, cote, phrase_fusion("variable(s) retirée(s) : %s", "variable(s) removed: %s", noms_fusion(cle_lisible(vars(d$retirees)))))
  if (length(d$obs_ajoutees)) apport_fusion(ctx, cote, phrase_fusion("%d observation(s) ajoutée(s)", "%d observation(s) added", length(d$obs_ajoutees)))
  if (length(d$obs_retirees)) apport_fusion(ctx, cote, phrase_fusion("%d observation(s) retirée(s)", "%d observation(s) removed", length(d$obs_retirees)))
  n <- sum(lengths(d$cellules))
  if (n) apport_fusion(ctx, cote, phrase_fusion("%d cellule(s) changée(s) (%s)", "%d cell(s) changed (%s)", n, noms_fusion(vapply(names(d$cellules), nom_cle_fusion, ""), 3)))
  for (k in utils::head(names(d$categories), 3)) {
    apport_fusion(ctx, cote, phrase_fusion("nouvelles catégories de %s : %s", "new categories of %s: %s", nom_cle_fusion(k), noms_fusion(d$categories[[k]], 3)))
  }
  if (length(d$champs)) apport_fusion(ctx, cote, phrase_fusion("unités, descriptions ou formules de %s", "units, descriptions or formulas of %s", noms_fusion(vapply(names(d$champs), nom_cle_fusion, ""), 3)))
}

# Deux versions d'un projet fusionnées par le sens (voir le début de ce fichier). `base`, `mien`, `leur` :
# des projets (lire_projet ; `mien` peut être celui de la séance) ; `choix` : "mien" ou "leur" par
# identifiant de conflit ; `dossier` : celui du fichier du projet, d'où se retrouvent la source de ses
# données et les fichiers de ses étapes (etapes.R : fichier_retrouve) ; `qui` : l'autre côté, pour les
# étiquettes (« fusion : valeurs de x de Alice ») ; `relus` : les trois projets sont déjà tels qu'un
# fichier les rend (projet_relu ; sinon ils le deviennent ici) ; `seance` : mon côté tel qu'il est dans
# la séance, dont le projet fusionné prend ce qui est de la séance (serveur.R : son identifiant, son
# fichier…), rendu tel quel quand leur côté n'a rien changé. Rend list(projet : le projet fusionné ;
# conflits ; apports : list(mien, leur), ce que chaque côté a apporté, en phrases ; donnees : list(mode :
# "etapes", "cellules" ou "inchangees", raison : pourquoi pas par les étapes) ; notes). Fusionner un côté
# avec lui-même, ou avec la base, rend ce côté tel quel.
fusionner_projets <- function(base, mien, leur, choix = list(), dossier = NULL, qui = NULL, relus = FALSE, seance = mien) {
  force(seance)
  if (!relus) {
    base <- projet_relu(base, dossier)
    mien <- projet_relu(mien, dossier %||% dossier_projet(mien))
    leur <- projet_relu(leur, dossier)
  }
  ctx <- new.env()
  ctx$choix <- as.list(choix)
  ctx$conflits <- list()
  ctx$apports <- list(mien = character(0), leur = character(0))
  ctx$pseudo <- list()
  ctx$renommes <- character(0)
  ctx$qui <- premier_texte(qui, traduire("l'autre version", "the other version"))
  ctx$mode <- "cellules"
  ctx$raison <- ""
  notes <- character(0)
  tB <- tableau_fusion(base)
  tM <- tableau_fusion(mien)
  tL <- tableau_fusion(leur)
  dM <- changements_fusion(tB, tM)
  dL <- changements_fusion(tB, tL)
  # (les données : inchangées des deux côtés, par les étapes si c'est possible, sinon cellule par cellule)
  p <- mien
  if (donnees_inchangees(dM) && donnees_inchangees(dL) && meme_source(base$source_donnees, mien$source_donnees) &&
      meme_source(base$source_donnees, leur$source_donnees) && identical(cles_etapes(mien), cles_etapes(leur))) {
    ctx$mode <- "inchangees"
  } else {
    n0 <- length(ctx$conflits)
    D <- fusion_etapes(base, mien, leur, tB, tM, tL, ctx, dossier)
    if (!is.null(D)) {
      for (f in c("variables", "observations", "observation_id_names", "datasets", "donnees", "noms_obs", "noms_var", "source_donnees",
                  "etapes", "empreinte_donnees", "coordonnees", "collecte")) p <- avec_champ(p, f, D[[f]])
    } else {
      if (length(ctx$conflits) > n0) ctx$conflits <- ctx$conflits[seq_len(n0)]
      x <- fusion_cellules(tB, tM, tL, dM, dL, ctx)
      for (f in c("variables", "observations", "observation_id_names", "datasets", "donnees", "noms_obs", "noms_var")) p[[f]] <- x[[f]]
      p <- source_etapes_cellules(base, mien, leur, p, ctx)
      notes <- x$notes
    }
  }
  tF <- tableau_fusion(p)
  cartes <- list(base = carte_fusion(tB, tF, ctx), mien = carte_fusion(tM, tF, ctx), leur = carte_fusion(tL, tF, ctx))
  if (!identical(ctx$mode, "etapes")) p <- avec_champ(p, "coordonnees", fusion_coordonnees(tB, tM, tL, tF, ctx))
  if (!is.null(base$prediction_sets) || !is.null(mien$prediction_sets) || !is.null(leur$prediction_sets)) p$prediction_sets <- fusion_predictions(tB, tM, tL, cartes, ctx)
  # (les modèles, dans la numérotation de la fusion, puis les documents et les figures qui les citent)
  definitions <- function(X, carte) {
    ds <- lapply(X$models %||% list(), renumeroter_definition, carte = carte)
    stats::setNames(ds, cles_rangs(vapply(X$models %||% list(), function(d) premier_texte(d$name), "")))
  }
  dB <- definitions(base, cartes$base)
  dMm <- definitions(mien, cartes$mien)
  dLl <- definitions(leur, cartes$leur)
  modeles <- fusion_modeles(base, mien, leur, dB, dMm, dLl, ctx)
  p$models <- lapply(modeles$modeles, function(d) {
    for (f in c("observations", "cochees", "test", "exclues")) if (length(d[[f]])) {
      x <- as.numeric(unlist(d[[f]]))
      d[[f]] <- numeros_entiers(x[x > 0])
    }
    cote <- unname(modeles$seul[premier_texte(d$name)])
    if (!is.na(cote)) d <- suivre_donnees_fusion(d, p, tF, if (cote == "mien") tM else tL)
    for (f in c("x", "y", "termes_complets")) if (length(d[[f]])) d[[f]] <- ranger_familles(d[[f]])
    d
  })
  noms <- stats::setNames(vapply(p$models, function(d) premier_texte(d$name), ""), vapply(p$models, function(d) as.character(numeros_entiers(d$number)), ""))
  p <- avec_champ(p, "documents", if (!is.null(base$documents) || !is.null(mien$documents) || !is.null(leur$documents)) fusion_documents(base, mien, leur, modeles$noms_leur, ctx))
  p <- avec_champ(p, "figures", if (!is.null(base$figures) || !is.null(mien$figures) || !is.null(leur$figures)) fusion_figures(base, mien, leur, modeles$numeros, noms, ctx))
  # (le reste, champ par champ)
  for (f in setdiff(unique(c(names(mien), names(leur), names(base))), CHAMPS_FUSIONNES)) {
    if (identical(ctx$mode, "etapes") && identical(f, "collecte")) next
    cote <- cote_fusion(texte_champ(base, f), texte_champ(mien, f), texte_champ(leur, f))
    if (cote == "conflit") {
      dit <- function(x) if (f %in% names(x)) traduire("changé", "changed") else traduire("retiré", "removed")
      cote <- conflit_fusion(ctx, paste0("champ:", f), "champ", f, phrase_fusion("Champ %s du projet", "Project field %s", guillemets_fusion(f)), dit(mien), dit(leur))
    }
    if (cote != "leur") next
    if (f %in% names(leur)) p[f] <- list(leur[[f]]) else p[[f]] <- NULL
  }
  # (ce que chaque côté a apporté)
  apports_donnees(base, mien, dM, ctx, "mien")
  apports_donnees(base, leur, dL, ctx, "leur")
  apports_modeles(dB, dMm, ctx, "mien")
  apports_modeles(dB, dLl, ctx, "leur")
  for (n in ctx$renommes) apport_fusion(ctx, "leur", n)
  for (cote in c("mien", "leur")) {
    X <- if (cote == "mien") mien else leur
    apports_elements(base$documents, X$documents, c("titre", "langue", "texte", "bib", "style"), ctx, cote,
                     c(traduire("document(s) ajouté(s) : %s", "document(s) added: %s"), traduire("document(s) retiré(s) : %s", "document(s) removed: %s"),
                       traduire("document(s) modifié(s) : %s", "document(s) edited: %s")))
    apports_elements(base$figures, X$figures, c("titre", "contenu"), ctx, cote,
                     c(traduire("figure(s) ajoutée(s) : %s", "figure(s) added: %s"), traduire("figure(s) retirée(s) : %s", "figure(s) removed: %s"),
                       traduire("figure(s) modifiée(s) : %s", "figure(s) changed: %s")))
  }
  # (la séance est la mienne ; ce qui a écrit le fichier, ce qui l'enregistrera ; un côté fusionné avec
  # lui-même ou avec la base, tel quel)
  p <- avec_champ(p, "session", mien$session)
  p$nuee <- NULL
  cb <- contenu_fusion(base)
  cm <- contenu_fusion(mien)
  cl <- contenu_fusion(leur)
  if (identical(cl, cb) || identical(cm, cl)) {
    p <- seance
  } else if (identical(cm, cb)) {
    p <- avec_champ(leur, "session", mien$session)
    p$nuee <- NULL
  }
  for (f in intersect(CHAMPS_SEANCE_FUSION, names(seance))) p[f] <- list(seance[[f]])
  class(p) <- "projet_nuee"
  list(projet = p, conflits = ctx$conflits, apports = ctx$apports, donnees = list(mode = ctx$mode, raison = ctx$raison), notes = notes)
}
