# Serveur local de l'application : sert l'interface et fait tous les calculs.
#   Rscript moteur/serveur.R <port> <dossier de l'interface>
# L'interface (fenêtre de l'application ou navigateur) envoie ses demandes en JSON à
# /api/<demande> ; chaque résultat affiché vient des fonctions du moteur.

# Le moteur est écrit en UTF-8. R pour Windows l'est toujours ; sous Linux, une session
# lancée sans langue (LANG vide ou C, depuis un menu par exemple) ne l'est pas : on passe à
# C.UTF-8, présent sur tous les systèmes récents.
if (!l10n_info()[["UTF-8"]]) {
  for (l in c("C.UTF-8", "C.utf8", "en_US.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) break
}

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  racine <<- normalizePath(file.path(dirname(if (length(f)) f else "moteur/serveur.R"), ".."), winslash = "/")
})
source(file.path(racine, "moteur", "charger.R"))

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[1]) else 8765L
interface <- if (length(args) >= 2) args[2] else file.path(racine, "app", "interface")

# Seule l'interface de l'application peut faire calculer le moteur. Le serveur n'écoute que
# 127.0.0.1, mais d'autres que l'application y ont accès : une page web ouverte dans un
# navigateur (elle peut envoyer des demandes à 127.0.0.1, ou s'y faire passer pour son propre
# site en changeant l'adresse de son nom de domaine) et les autres utilisateurs du même
# ordinateur (bureau à distance). Or la console R exécute ce qu'on lui envoie. Chaque demande
# doit donc porter le jeton de la séance : l'application le tire au hasard à chaque lancement
# (NUEE_JETON) et le donne à ses fenêtres dans un cookie que les navigateurs n'envoient jamais
# depuis un autre site. Lancé à la main sans NUEE_JETON (développement), le serveur tire
# lui-même le jeton et affiche l'adresse à ouvrir dans le navigateur.
JETON <- Sys.getenv("NUEE_JETON")
jeton_donne <- nzchar(JETON)
if (!jeton_donne) JETON <- paste(sample(c(0:9, letters[1:6]), 64, replace = TRUE), collapse = "")
Sys.unsetenv("NUEE_JETON")  # ni la console ni les programmes qu'elle lance n'en héritent
COOKIE <- paste0("nuee_", port)  # les cookies ne distinguent pas les ports : un nom par serveur
HOTES <- sprintf(c("127.0.0.1:%d", "localhost:%d"), port)

jeton_recu <- function(cookies) {
  for (x in trimws(strsplit(cookies %||% "", ";", fixed = TRUE)[[1]])) {
    if (startsWith(x, paste0(COOKIE, "="))) return(substring(x, nchar(COOKIE) + 2L))
  }
  ""
}

# /api/connexion?jeton=… (navigateur, développement) : pose le cookie, puis ouvre l'interface
# avec les autres paramètres de l'adresse (ouvrir=…).
connexion <- function(req) {
  q <- sub("^\\?", "", req$QUERY_STRING %||% "")
  parametres <- strsplit(q, "&", fixed = TRUE)[[1]]
  j <- grep("^jeton=", parametres, value = TRUE)
  if (!length(j) || !identical(utils::URLdecode(sub("^jeton=", "", j[1])), JETON)) {
    return(list(status = 403L, headers = list(), body = "jeton refusé"))
  }
  reste <- paste(grep("^jeton=", parametres, value = TRUE, invert = TRUE), collapse = "&")
  list(status = 302L, body = "",
       headers = list(Location = paste0("/", if (nzchar(reste)) paste0("?", reste)),
                      "Set-Cookie" = sprintf("%s=%s; Path=/; HttpOnly; SameSite=Strict", COOKIE, JETON)))
}

etat <- new.env()
etat$projets <- list()
etat$modeles <- list()
etat$predictions <- list()  # dernière prédiction de chaque modèle (pour l'export Excel)
etat$compteur <- 0
etat$annulations <- list()  # par projet : les états précédents, pour « Annuler » (Ctrl+Z)
etat$revisions <- list()  # par projet : le nombre de changements de ses données (resume_projet)
etat$paires <- list()  # les tableaux de toutes les paires des tests appariés (memo_paires)

nouvel_id <- function(prefixe) {
  etat$compteur <- etat$compteur + 1
  paste0(prefixe, etat$compteur)
}

source(file.path(racine, "moteur", "console.R"), encoding = "UTF-8")  # journal et console R

# ---------------------------------------------------------------- projets

# Un fichier .usp passe par le convertisseur (export_project), qui écrit un .json : le
# programme compilé sous Windows, le script Python ailleurs (python3 sous Linux).
convertir_usp <- function(chemin) {
  dossier <- file.path(tempdir(), nouvel_id("usp"))
  dir.create(dossier)
  exe <- Sys.getenv("CONVERTISSEUR_USP", file.path(racine, "outils", "export_project.exe"))
  sortie <- if (file.exists(exe)) {
    system2(exe, c(shQuote(chemin), "-o", shQuote(dossier)), stdout = TRUE, stderr = TRUE)
  } else {
    python <- Sys.getenv("PYTHON", if (.Platform$OS.type == "windows") "python" else "python3")
    system2(python, c(shQuote(file.path(racine, "convertisseur", "export_project.py")),
                      shQuote(chemin), "-o", shQuote(dossier)), stdout = TRUE, stderr = TRUE)
  }
  json <- list.files(dossier, "\\.json$", full.names = TRUE)
  if (!length(json)) stop(traduire("Conversion du fichier .usp impossible : ", "The .usp file couldn't be converted: "), paste(sortie, collapse = " "))
  json[1]
}

# Garde l'état actuel d'un projet avant de le changer, pour pouvoir revenir en arrière
# (20 changements au plus par projet).
memoriser <- function(p, etiquette) {
  pile <- c(list(list(etiquette = etiquette, projet = p)), etat$annulations[[p$id]] %||% list())
  etat$annulations[[p$id]] <- utils::head(pile, 20)
}
# Chaque changement des données d'un projet, une fois fait, et chaque retour en arrière comptent
# une révision du projet : les vues qui gardent des calculs sur ses données les refont.
reviser <- function(id) etat$revisions[[id]] <- (etat$revisions[[id]] %||% 0) + 1

# Le tableau de toutes les paires d'un test apparié (comparaisons.R : comparer_apparies), gardé
# d'une demande à l'autre (A et B, la langue mise à part, ne le changent pas) tant que les données
# du projet ne changent pas : 20 au plus.
memo_paires <- function(p, q) {
  cle <- paste(c(p$id, etat$revisions[[p$id]] %||% 0, q$x, q$y, q$appariement, q$dans %||% "", getOption("nuee.langue") %||% "fr"), collapse = "|")
  function(calcul) {
    if (is.null(etat$paires[[cle]])) etat$paires <- utils::tail(c(etat$paires, stats::setNames(list(calcul()), cle)), 20)
    etat$paires[[cle]]
  }
}

annuler_demande <- function(q) {
  pile <- etat$annulations[[q$projet]]
  if (!length(pile)) stop(traduire("Rien à annuler", "Nothing to undo"))
  actuel <- projet(q$projet)
  ancien <- pile[[1]]$projet
  etat$annulations[[q$projet]] <- pile[-1]
  ancien$chemin_enregistrement <- actuel$chemin_enregistrement  # un enregistrement fait depuis reste valable
  ancien$fichier_lu <- actuel$fichier_lu
  ancien$modifie <- TRUE
  etat$projets[[q$projet]] <- ancien
  reviser(q$projet)
  list(projet = resume_projet(ancien), etiquette = pile[[1]]$etiquette)
}

# Un fichier qui a plusieurs tableaux (feuilles Excel, objets R) et dont le tableau n'est pas
# choisi renvoie leur liste (`choisir`) : l'interface demande lequel ouvrir.
ouvrir_fichier <- function(chemin, nom = basename(chemin), table = NULL) {
  ext <- extension(nom)
  if (!ext %in% c("nuee", "json", "usp", FORMATS_LECTURE)) stop(traduire("Format de fichier non reconnu : .", "Unrecognised file format: ."), ext)
  if (!ext %in% FORMATS_LECTURE) table <- NULL
  if (ext %in% FORMATS_LECTURE && is.null(table)) {
    tables <- tables_fichier(chemin)
    if (length(tables) > 1) return(list(choisir = I(tables), chemin = chemin, nom = nom))
  }
  p <- switch(ext,
              nuee = , json = lire_projet(chemin),
              usp = lire_projet(convertir_usp(chemin)),
              importer_tableau(chemin, table))
  p$id <- nouvel_id("P")
  p$nom <- if (is.null(table)) nom else sprintf("%s (%s).%s", tools::file_path_sans_ext(nom), table, tools::file_ext(nom))
  p$chemin <- chemin
  p$table <- table
  if (ext %in% c("nuee", "json")) p$fichier_lu <- etat_fichier(chemin)
  etat$projets[[p$id]] <- p
  r <- resume_projet(p)
  # ce qu'il faut savoir de la lecture (valeurs non numériques lues comme manquantes...), et
  # d'un projet venant d'une version plus récente
  notes <- c(p$note, if (isTRUE(as.numeric(p$nuee$format %||% 0) > FORMAT_PROJET))
    sprintf(traduire("Ce projet a été enregistré par Nuée %s, plus récente que celle-ci : certaines de ses parties ne sont peut-être pas lues. Mettez Nuée à jour.",
                     "This project was saved by Nuée %s, newer than this one: some of its parts may not be read. Update Nuée."), p$nuee$version %||% "?"))
  if (length(notes)) r$note <- I(as.character(notes))
  r
}

# Ce qu'on sait d'un fichier de projet quand on l'ouvre ou l'enregistre, pour voir avant de
# l'écraser s'il a changé depuis (un collègue, sur un dossier partagé ; une autre copie de Nuée).
etat_fichier <- function(chemin) {
  i <- file.info(chemin)
  list(chemin = normalizePath(chemin, winslash = "/", mustWork = FALSE), taille = i$size, date = as.numeric(i$mtime))
}

# Un fichier dans l'état qu'on en connaissait : même taille, même date à la milliseconde près
# (un état gardé avec une copie de secours est passé par du JSON).
meme_etat <- function(a, b) {
  isTRUE(as.numeric(a$taille) == as.numeric(b$taille)) && isTRUE(abs(as.numeric(a$date) - as.numeric(b$date)) < 1e-3)
}

# L'état du fichier d'un projet gardé avec sa copie de secours (sauvegarde_secours), rendu au
# projet repris de la copie, s'il est bien celui du fichier où il s'enregistre (`origine`) :
# la question avant d'écraser un fichier changé par ailleurs vaut encore après un arrêt
# inattendu ou un redémarrage du moteur. NULL sinon.
fichier_lu_repris <- function(lu, origine) {
  if (!is.list(lu) || is.null(origine) || !identical(as.character(lu$chemin), etat_fichier(origine)$chemin)) return(NULL)
  list(chemin = as.character(lu$chemin), taille = as.numeric(lu$taille), date = as.numeric(lu$date))
}

# Une erreur que l'interface reconnaît : le fichier a changé depuis qu'on l'a lu.
conflit_fichier <- function(chemin) {
  structure(class = c("conflit_fichier", "error", "condition"),
            list(message = sprintf(traduire("%s a été modifié par ailleurs depuis que vous l'avez ouvert ou enregistré.",
                                            "%s was changed elsewhere since you opened or saved it."), basename(chemin)),
                 call = NULL, date = etat_fichier(chemin)$date))
}

# Un nom de fichier (ou d'objet R) tiré du nom d'un projet ou d'un modèle.
nom_fichier <- function(nom) gsub("[^[:alnum:]_ ()-]", "_", tools::file_path_sans_ext(nom %||% "donnees"))

# Chemin d'un fichier écrit pour le navigateur (qui le télécharge ensuite).
chemin_temporaire <- function(nom) {
  dossier <- file.path(tempdir(), nouvel_id("export"))
  dir.create(dossier)
  file.path(dossier, nom)
}

composantes_stockees <- function(d) {
  ref <- d$reference$components
  if (is.null(ref) || !length(ref)) return(NULL)
  cle <- function(k) vapply(ref, function(x) as.numeric(x[[k]] %||% NA), 0)
  list(R2X = cle("R2X"), R2Xcum = cle("R2X(cum)"), R2Y = cle("R2Y"), R2Ycum = cle("R2Y(cum)"),
       Q2 = cle("Q2"), Q2cum = cle("Q2(cum)"))
}

resume_modele <- function(d) {
  list(numero = d$number %||% NA, nom = d$name %||% "", titre = d$title %||% "",
       type = paste0(if (isTRUE(d$opls)) "O" else "", if (isTRUE(d$plsda)) "PLS-DA" else d$type %||% ""), A = d$A %||% 0,
       ajuste = isTRUE(d$fitted) || !is.null(d$app), erreur = d$error %||% NULL,
       n_obs = length(d$observations), x = vapply(d$x, function(t) as.character(t$name), ""),
       y = vapply(d$y, function(t) as.character(t$name), ""), stocke = composantes_stockees(d), note = d$note %||% "",
       version_origine = if (!isTRUE(d$app)) d$version_origine %||% NULL)
}

# Les variables qui ont des valeurs dans les jeux de données. Les fichiers .usp listent aussi des variables
# internes sans données (par exemple « $constant »), qui ne servent ni à l'affichage ni aux modèles.
variables_avec_donnees <- function(p) {
  Filter(function(v) as.character(v$uid) %in% colnames(p$donnees), p$variables)
}

resume_projet <- function(p) {
  manquantes <- colSums(is.na(p$donnees))
  vars <- lapply(variables_avec_donnees(p), function(v) list(
    uid = v$uid, nom = as.character(v$ids[1]), qualitative = length(v$categories) > 0,
    categories = I(as.character(v$categories %||% character(0))), manquantes = unname(manquantes[as.character(v$uid)])))
  json <- tolower(tools::file_ext(p$chemin %||% "")) %in% c("nuee", "json")
  pile <- etat$annulations[[p$id]]
  list(id = p$id, nom = p$nom, source = p$source %||% "", modifie = isTRUE(p$modifie), chemin = p$chemin %||% NULL,
       table = p$table %||% NULL,
       annulable = if (length(pile)) pile[[1]]$etiquette, revision = etat$revisions[[p$id]] %||% 0,
       chemin_enregistrement = p$chemin_enregistrement %||% (if (json) p$chemin),
       observations = list(uid = as.numeric(names(p$noms_obs)), nom = unname(p$noms_obs)),
       identifiants = identifiants_secondaires(p),
       variables = vars,
       jeux = lapply(p$datasets, function(d) list(nom = d$name %||% "", n_obs = length(d$observations),
                                                  n_var = length(d$variables))),
       modeles = lapply(p$models, resume_modele), session = !is.null(p$session),
       historique = sum(vapply(p$audit %||% list(), function(s) length(s$events), 0)),
       predictions = lapply(p$prediction_sets, function(s) list(nom = s$name %||% "", n_obs = length(s$observations))))
}

donnees_projet <- function(p) {
  vars <- variables_avec_donnees(p)
  valeurs <- lapply(seq_along(vars), function(k) {
    x <- p$donnees[, as.character(vars[[k]]$uid)]
    cats <- vars[[k]]$categories
    if (length(cats)) cats[x] else x
  })
  list(observations = unname(p$noms_obs[rownames(p$donnees)]), uid_observations = as.numeric(rownames(p$donnees)),
       variables = vapply(vars, function(v) as.character(v$ids[1]), ""),
       uid_variables = vapply(vars, function(v) as.numeric(v$uid), 0), valeurs = valeurs)
}

# ---------------------------------------------------------------- modèles

projet <- function(id) etat$projets[[id]] %||% stop(traduire("Projet inconnu : ", "Unknown project: "), id)
modele <- function(id) etat$modeles[[id]] %||% stop(traduire("Modèle inconnu : ", "Unknown model: "), id)

# Définition d'un modèle décrit par l'interface (type, variables, observations exclues...).
definition_interface <- function(p, q) {
  obs <- vapply(p$observations, function(o) as.numeric(o$uid), 0)
  if (length(q$exclues)) obs <- setdiff(obs, as.numeric(q$exclues))
  ech <- c(defaut = q$echelle %||% "UV", unlist(q$echelles))
  d <- definir_modele(p, q$type, x = as.numeric(q$x), y = as.numeric(q$y %||% numeric(0)),
                      classe = q$classe, observations = obs, echelle = ech,
                      A = q$A %||% 0, nom = q$nom, titre = q$titre %||% "", G = q$G %||% 7,
                      transformations = if (length(q$transformations)) unlist(q$transformations),
                      croises = if (is.matrix(q$croises)) lapply(seq_len(nrow(q$croises)), function(i) q$croises[i, ])
                                else q$croises,
                      spectres = q$spectres, test = as.numeric(q$test %||% numeric(0)),
                      vc = q$vc, blocs = if (length(q$blocs)) unlist(q$blocs), modificateurs = if (length(q$modificateurs)) unlist(q$modificateurs),
                      aleatoires = q$aleatoires, correlation = q$correlation, famille = q$famille,
                      lisses = if (length(q$lisses)) as.numeric(unlist(q$lisses)), decalage = if (length(q$decalage)) as.numeric(q$decalage),
                      croises_reglages = if (length(q$croises_reglages)) q$croises_reglages, k_courbes = if (length(q$k_courbes)) q$k_courbes,
                      options = q$options)
  d$app <- TRUE
  d$exclues <- as.numeric(q$exclues %||% numeric(0))
  d$demande <- q
  d
}

# Modèles de classe (modeles_classe.R) : une ACP par catégorie de la variable de classe,
# chacune ajoutée au projet comme un modèle de l'application.
modeles_classe_demande <- function(q) {
  p <- projet(q$projet)
  d <- definition_interface(p, utils::modifyList(q, list(type = "PCA", classe = NULL)))
  ds <- modeles_classe(p, d, as.numeric(q$classe))
  if (!length(ds)) stop(traduire("Aucune classe n'a assez d'observations (3 au moins)", "No class has enough observations (3 or more)"))
  list(modeles = I(lapply(ds, function(e) ajuster_et_garder(projet(q$projet), e))))
}

# Distances de toutes les observations aux modèles du groupe de classes du modèle demandé ;
# ceux qui ne sont pas encore calculés le sont ici.
appartenance_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  g <- e$definition$groupe_classes
  if (is.null(g)) stop(traduire("Ce modèle ne fait pas partie d'un groupe de modèles de classe", "This model isn't part of a group of class models"))
  defs <- Filter(function(d) identical(d$groupe_classes$nom, g$nom), p$models)
  modeles <- lapply(defs, function(d) {
    ids <- Filter(function(id) identical(etat$modeles[[id]]$projet, p$id) && identical(etat$modeles[[id]]$definition$name, d$name), names(etat$modeles))
    if (length(ids)) etat$modeles[[ids[length(ids)]]][c("definition", "modele")] else list(definition = d, modele = ajuster(p, d))
  })
  r <- appartenance_classes(p, modeles)
  r$table <- lapply(seq_len(nrow(r$table)), function(i) list(classe = rownames(r$table)[i], n = unname(r$effectifs[i]), parts = I(unname(r$table[i, ]))))
  r$effectifs <- NULL
  r$variable <- as.character(variable_du_projet(p, g$variable)$ids[1])
  r
}

ajuster_demande <- function(q) {
  p <- projet(q$projet)
  if (!is.null(q$numero)) {  # un modèle du projet (importé d'un .usp ou enregistré par l'application)
    d <- modele_du_projet(p, q$numero)
    if (!is.null(q$A) && q$A > 0 && !identical(as.numeric(d$A %||% 0), as.numeric(q$A))) d$A <- q$A
  } else {
    d <- definition_interface(p, q)
  }
  ajuster_et_garder(p, d)
}

# Les groupements d'une demande ({ by: "id" | "variable" }, ou { by: "effets" } : les effets
# aléatoires d'une régression) en ceux du moteur (regression.R : specs_groupement) : le premier, ou
# tous (`tous`, du plus large au plus fin) ; NULL sans groupement.
groupements_demande <- function(s, tous = FALSE) {
  if (!length(s$by)) return(NULL)
  # (plusieurs groupements arrivent en data.frame, une ligne chacun)
  if (is.data.frame(s)) s <- lapply(seq_len(nrow(s)), function(i) as.list(s[i, , drop = FALSE]))
  if (!is.null(s$by)) s <- list(s)
  specs <- lapply(s, function(x) if (identical(as.character(x$by)[1], "effets")) list(by = "effets") else specs_groupement(x)[[1]])
  if (tous) specs else specs[[1]]
}

# Réajuste un modèle sans certaines observations, ou avec un autre nombre de composantes ; une
# régression, aussi avec une autocorrélation CAR1 (q$car1 : son temps et ses groupes, comme pour
# l'autocorrélation des résidus).
reajuster_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  d <- e$definition
  if (sans_composantes(d)) {  # (une régression, une autre analyse : sans composantes ni VIP, seulement des observations en moins)
    if (length(q$garder)) stop(if (est_regression(d)) traduire("Une régression n'a pas de VIP.", "A regression has no VIP.")
                               else traduire("Cette analyse n'a pas de VIP.", "This analysis has no VIP."))
    q$A <- NULL
  }
  if (!is.null(q$groupes_aic)) {  # Sélection par AIC › Ajuster ce modèle : les seuls termes de ces groupes (regression.R)
    if (!est_regression(d)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    d <- definition_selection(d, e$modele, q$groupes_aic)
  }
  if (!is.null(q$car1)) {  # Autocorrélation des résidus › Ajuster avec CAR1 : la même définition, avec CAR1 (regression.R)
    if (!est_regression(d)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    d <- definition_car1(p, d, groupements_demande(q$car1$temps), groupements_demande(q$car1$groupe, TRUE))
  }
  exclues <- as.numeric(q$exclues %||% numeric(0))
  if (length(q$garder)) {  # ne garder que ces termes X (et les facteurs de leurs termes croisés)
    noms <- vapply(d$x, function(t) as.character(t$name), "")
    facteurs <- unlist(lapply(d$x[noms %in% q$garder], function(t) if (length(t$variables) > 1)
      vapply(seq_along(t$variables), function(j) paste(t$variables[j], categorie(t, j)), "")))
    cle <- vapply(d$x, function(t) paste(t$variables[1], categorie(t)), "")
    simple <- vapply(d$x, function(t) length(t$variables) == 1, TRUE)
    d$x <- d$x[noms %in% q$garder | (simple & cle %in% facteurs)]
    d$solution <- NULL
    d$reference <- list(ynames = d[["reference"]][["ynames"]], note = d[["reference"]][["note"]])
    if (is.null(q$A)) d$A <- 0
  }
  if (length(exclues)) {
    d$observations <- setdiff(d$observations, exclues)
    d$exclues <- c(d$exclues, exclues)
    d$solution <- NULL
    d$reference <- list(ynames = d[["reference"]][["ynames"]], note = d[["reference"]][["note"]])  # ce n'est plus le modèle du fichier
  }
  d$A <- if (!is.null(q$A)) q$A else if (length(exclues) || length(q$garder)) 0 else d$A
  base <- sub(" \\(\\d+\\)$", "", d$name %||% "M")
  d$name <- sprintf("%s (%d)", base, sum(vapply(etat$modeles, function(x) startsWith(x$definition$name %||% "", base), TRUE)))
  d$number <- NULL
  d$app <- TRUE
  ajuster_et_garder(p, d)
}

ajuster_et_garder <- function(p, d) {
  if (!d$A && !sans_composantes(d)) d$A <- max(1, autoajuster(p, d))  # (ni une régression ni une autre analyse n'a de composantes)
  m <- ajuster(p, d)
  id <- nouvel_id("M")
  if (isTRUE(d$app)) {  # le modèle rejoint le projet (enregistré avec lui)
    d$number <- d$number %||% (max(c(0, vapply(p$models, function(x) as.numeric(x$number %||% 0), 0))) + 1)
    d$fitted <- TRUE
    ancien <- Find(function(x) identical(x$name, d$name), p$models)
    p$models <- c(Filter(function(x) !identical(x$name, d$name), p$models), list(d))
    if (!identical(ancien, d)) p$modifie <- TRUE  # recalculer un modèle enregistré ne change pas le projet
    etat$projets[[p$id]] <- p
  }
  etat$modeles[[id]] <- list(projet = p$id, definition = d, modele = m)
  resultat_modele(id)
}

# Les demandes qui supposent des composantes et des scores : refusées pour une régression
# (regression.R), avant d'arriver à un code qui ne les trouverait pas.
SANS_REGRESSION <- c("hca", "permutations", "scores_en_variables",
                     "appartenance", "contributions", "spectres", "biplot", "ipls")

# Ce que l'interface reçoit d'une régression : comme resultat_modele pour ce qui est commun
# (observations observées et ajustées, RMSEE, RMSEcv, jeu de test, réglages), et ses propres
# résultats (coefficients, qualité, variances, apports sur demande ; les résidus quantiles d'un
# modèle de comptages, de présence-absence ou de valeurs positives, et leurs quantiles normaux).
resultat_regression <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  r <- m$resultat
  y <- m$y$noms
  obs <- observations_regression(p, m)
  variances <- variances_regression(m)
  list(id = id, projet = p$id, nom = m$nom, titre = m$titre, type = m$type, regression = TRUE, opls = FALSE, Ap = 0, A = 0,
       N = m$N, K = m$K, noms_y = I(y), transformation_y = m$y$transformation, observations = obs,
       residus_quantiles = if (!is.null(r$residus_quantiles)) list(residu = r$residus_quantiles, quantile_normal = quantiles_henry(r$residus_quantiles)),
       coefficients_regression = r$coefficients, termes_retires = I(r$termes_retires),
       famille = m$famille, lien = m$lien, parametre_loi = r$parametre_loi, courbes = r$courbes, decalage = m$decalage, dispersion = r$dispersion,
       gamm = isTRUE(m$gamm), dans = m$dans,
       qualite = c(r$r2, list(AIC = r$AIC, AICc = r$AICc, BIC = r$BIC, logLik = r$logLik, parametres = r$parametres,
                              Q2 = m$vc$Q2, RMSEcv = m$vc$RMSEcv, echecs_vc = m$vc$echecs)),
       variances = variances, phi = r$phi, niveaux = I(m$niveaux), temps = m$temps, exclues_manquantes = length(m$exclues),
       signature = m$signature, RMSEE = stats::setNames(list(sqrt(mean((m$donnees$y - r$ajuste)^2))), y),
       RMSEcv = stats::setNames(list(m$vc$RMSEcv), y), composantes = NULL,
       G = m$G, vc_par = m$vc_par %||% "", reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app),
       note = d$note %||% "", test = m$test, demande = d$demande %||% list(type = d$type, numero = d$number))
}

resultat_modele <- function(id) {
  e <- modele(id)
  if (est_regression(e$definition)) return(resultat_regression(id))
  if (est_autre_analyse(e$definition)) return(fonction_analyse("resultat_", e$definition$type)(id))  # analyses.R
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  s <- m$statistiques
  N <- m$N
  notes <- !is.na(m$T[, 1])
  Nt <- sum(notes)
  obs <- s$observations
  obs$uid <- m$observations
  obs$nom <- unname(p$noms_obs[as.character(m$observations)])
  list(id = id, projet = p$id, nom = m$nom, titre = m$titre,
       type = paste0(if (isTRUE(m$opls)) "O" else "", if (m$plsda) "PLS-DA" else m$type), opls = isTRUE(m$opls), Ap = m$Ap %||% 1, A = m$A, N = N, K = m$K,
       groupe = d$groupe_classes$nom %||% NULL, classe_modele = d$groupe_classes$classe %||% NULL,
       composantes = s$composantes, observations = obs, variables = s$variables,
       variables_y = s$variables_y, coefficients = s$coefficients,
       limites = s$limites, RMSEE = as.list(s$RMSEE), RMSEcv = as.list(s$RMSEcv),
       seuil_vip = if (!is.null(s$seuil_vip)) c(s$seuil_vip[c("seuil", "chute", "marche")],
                                                list(retenues = I(s$seuil_vip$retenues))),
       variance_t = colSums(m$T[notes, , drop = FALSE]^2) / (Nt - 1),
       ellipse = list(`95` = 2 * (Nt - 1) / (Nt - 2) * stats::qf(0.95, 2, Nt - 2),
                      `99` = 2 * (Nt - 1) / (Nt - 2) * stats::qf(0.99, 2, Nt - 2)),
       stocke = composantes_stockees(d), version_origine = d$version_origine %||% NULL,
       noms_y = I(m$y$noms %||% character(0)),  # I() : un seul Y reste une liste
       G = m$G, vc_par = m$vc_par %||% "", reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       cv_anova = s$cv_anova, classification = s$classification, test = m$test,
       demande = d$demande %||% list(type = d$type, numero = d$number))
}

predire_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  obs <- if (!is.null(q$jeu)) p$prediction_sets[[q$jeu]]$observations
         else vapply(p$observations, function(o) as.numeric(o$uid), 0)
  out <- predire(p, e$modele, e$definition, obs)
  out$uid <- obs
  out$nom <- unname(p$noms_obs[as.character(obs)])
  out$dans_modele <- obs %in% e$modele$observations
  etat$predictions[[q$modele]] <- out
  list(modele = q$modele, observations = out)
}

hca_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  arbre <- classification(e$modele, q$composantes %||% e$modele$A)
  k <- q$groupes %||% 3
  list(modele = q$modele, composantes = arbre$composantes, merge = arbre$merge,
       hauteurs = arbre$hauteur_ward, ordre = arbre$order,
       noms = unname(p$noms_obs[arbre$labels]), uid = as.numeric(arbre$labels),
       groupes = unname(couper(arbre, k)), k = k)
}

# Prédiction d'un fichier. Pour un modèle hiérarchique, les scores des modèles de base
# ($M1.t1...) absents du fichier sont calculés par le modèle de base du même nom ajusté dans
# la session (le plus récent), qui prédit lui-même le fichier.
predire_fichier_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  base <- function(nom_modele) {
    ids <- Filter(function(id) identical(etat$modeles[[id]]$projet, p$id) &&
                    identical(etat$modeles[[id]]$definition$name, nom_modele), names(etat$modeles))
    if (length(ids)) etat$modeles[[ids[length(ids)]]]
  }
  completer <- function(nom) {
    x <- regmatches(nom, regexec("^\\$([^.]+)\\.t(\\d+)", nom))[[1]]
    b <- if (length(x)) base(x[2])
    if (is.null(b) || identical(b$definition$name, e$definition$name)) return(NULL)
    predire_fichier(p, b$modele, b$definition, q$chemin, completer)$observations[[paste0("t", x[3])]]
  }
  r <- predire_fichier(p, e$modele, e$definition, q$chemin, completer)
  r$observations$dans_modele <- FALSE
  etat$predictions[[q$modele]] <- r$observations
  c(list(modele = q$modele), r)
}

exporter_prediction_demande <- function(q) {
  out <- etat$predictions[[q$modele]] %||% stop(traduire("Aucune prédiction à exporter pour ce modèle", "No prediction to export for this model"))
  chemin <- q$chemin %||% chemin_temporaire(paste0("predictions ", nom_fichier(modele(q$modele)$definition$name), ".", q$format %||% "xlsx"))
  out <- out[c("nom", setdiff(names(out), c("nom", "uid")))]
  names(out)[1] <- "Observation"
  rownames(out) <- NULL
  ecrire_tableau(out, chemin, objet = "predictions")
  list(chemin = chemin, chemins = I(chemin))
}

# Les scores d'un modèle deviennent des variables du projet (modèles hiérarchiques).
scores_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  memoriser(p, sprintf("scores de %s en variables", e$definition$name %||% q$modele))
  r <- scores_en_variables(p, e$modele)
  r$projet$modifie <- TRUE
  etat$projets[[p$id]] <- r$projet
  reviser(p$id)
  list(projet = resume_projet(r$projet), noms = I(r$noms), modeles = I(modeles_utilisant(r$projet, r$noms)))
}

changer_projet <- function(q, f, etiquette) {
  memoriser(projet(q$projet), etiquette)
  p <- f(projet(q$projet))
  p$modifie <- TRUE
  etat$projets[[p$id]] <- p
  reviser(p$id)
  resume_projet(p)
}

classe_demande <- function(q) {
  memoriser(projet(q$projet), sprintf("classe %s", q$etiquette))
  p <- ajouter_classe(projet(q$projet), q$variable, q$etiquette, as.numeric(q$observations))
  p$modifie <- TRUE
  etat$projets[[p$id]] <- p
  reviser(p$id)
  resume_projet(p)
}

methodes_demande <- function(q) {
  e <- modele(q$modele)
  texte_methodes(projet(e$projet), e$definition, e$modele, if (identical(q$langue, "en")) "en" else "fr", as.character(q$version %||% ""))
}

# Un modèle (q$modele), ou plusieurs du même projet (q$modeles : la séance, dans un script .R).
script_demande <- function(q) {
  ids <- as.character(unlist(q$modeles %||% q$modele))
  # (le script de la séance laisse de côté les autres analyses : elles s'exportent seules)
  if (length(ids) > 1) ids <- Filter(function(id) !est_autre_analyse(modele(id)$definition), ids)
  if (!length(ids)) stop(traduire("Ces analyses s'exportent une à une.", "These analyses are exported one by one."))
  e <- modele(ids[1])
  seance <- length(ids) > 1
  nom <- gsub("[^[:alnum:]_ -]", "_", if (seance) as.character(projet(e$projet)$source %||% "seance") else e$definition$name %||% "modele")
  format <- if (!seance && isTRUE(q$format %in% c("Rmd", "qmd"))) q$format else "R"
  chemin <- q$chemin %||% file.path(tempdir(), nouvel_id("script"), paste0(nom, ".", format))
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE)
  options <- list(donnees = if (identical(q$donnees, "csv")) "csv" else "script", renv = isTRUE(q$renv),
                  rdata = isTRUE(q$rdata), rproj = isTRUE(q$rproj), version = as.character(q$version %||% ""),
                  langue = if (identical(q$langue, "en")) "en" else "fr")
  if (!seance) return(do.call(ecrire_script, c(list(projet(e$projet), e$definition, chemin, file.path(racine, "moteur"), m = e$modele), options)))
  es <- lapply(ids, modele)
  if (!all(vapply(es, function(x) identical(x$projet, e$projet), FALSE)))
    stop(traduire("Les modèles doivent venir du même projet.", "The models must come from the same project."))
  do.call(ecrire_script_seance, c(list(projet(e$projet), lapply(es, `[[`, "definition"), lapply(es, `[[`, "modele"), chemin,
                                       file.path(racine, "moteur")), options))
}

# Les identifiants secondaires des observations (« site », « ANNEE »...), sans les
# identifiants internes des fichiers .usp ($ClassID...) ni les colonnes vides.
identifiants_secondaires <- function(p) {
  noms <- as.character(p$observation_id_names %||% character(0))
  rangs <- Filter(function(k) {
    nzchar(noms[k]) && !startsWith(noms[k], "$") && any(nzchar(identifiants_obs(p, k)), na.rm = TRUE)
  }, seq_along(noms)[-1])
  lapply(rangs, function(k) list(rang = k, nom = noms[k]))
}

# Le k-ième identifiant de chaque observation (NA s'il manque ; validation_croisee.R).
identifiants_obs <- function(p, k) identifiants_observations(p, k, vapply(p$observations, function(o) as.numeric(o$uid), 0))

valeurs_demande <- function(q) {
  p <- projet(q$projet)
  if (!is.null(q$identifiant)) {
    uid <- vapply(p$observations, function(o) as.numeric(o$uid), 0)
    ids <- identifiants_obs(p, q$identifiant)
    return(list(valeurs = unname(ids[match(as.numeric(q$observations), uid)]), qualitative = TRUE))
  }
  v <- variable_du_projet(p, q$variable)
  x <- colonne(p, q$variable, as.numeric(q$observations))
  list(variable = q$variable, nom = as.character(v$ids[1]),
       valeurs = if (length(v$categories)) v$categories[x] else x,
       qualitative = length(v$categories) > 0)
}

# Les résultats d'un modèle (formats.R : ecrire_tables) : une feuille par tableau en Excel, une
# liste en R, un fichier par tableau ailleurs.
exporter_demande <- function(q) {
  e <- modele(q$modele)
  nom <- nom_fichier(e$definition$name %||% "modele")
  if (is.null(q$chemin)) q$chemin <- chemin_temporaire(paste0(nom, ".", q$format %||% "xlsx"))
  chemins <- ecrire_tables(tables_resultats(projet(e$projet), e$modele), q$chemin, objet = make.names(nom))
  list(chemin = chemins[1], chemins = I(chemins))
}

# Les données d'un projet, dans le format de l'extension du chemin.
exporter_donnees_demande <- function(q) {
  p <- projet(q$projet)
  chemin <- q$chemin %||% chemin_temporaire(paste0(nom_fichier(p$nom), ".", q$format %||% "xlsx"))
  ecrire_tableau(tableau_projet(p), chemin, objet = make.names(nom_fichier(p$nom)))
  list(chemin = chemin, chemins = I(chemin))
}

# Un tableau collé depuis un tableur : écrit dans un fichier de la session (que le journal de
# la console R relit), puis importé comme un tableau ouvert.
coller_demande <- function(q) {
  texte <- as.character(q$texte %||% "")
  if (!grepl("[\t;,]", texte) || !grepl("\n.", texte)) stop(traduire("Le presse-papiers ne contient pas de tableau (au moins deux lignes et deux colonnes)", "The clipboard holds no table (at least two rows and two columns)"))
  chemin <- chemin_temporaire("tableau colle.tsv")
  writeLines(enc2utf8(texte), chemin, useBytes = TRUE)
  p <- importer_tableau(chemin)
  p$id <- nouvel_id("P")
  p$nom <- q$nom %||% "Tableau"
  p$modifie <- TRUE  # à enregistrer : il n'a pas de fichier
  etat$projets[[p$id]] <- p
  r <- resume_projet(p)
  r$fichier_colle <- chemin
  r$note <- I(p$note %||% character(0))
  r
}

# Des spectres (JCAMP-DX, SPC, texte), un par fichier ou plusieurs, réunis en un projet, avec
# un tableau d'informations sur les échantillons s'il est donné (fichiers_spectres.R).
importer_spectres_demande <- function(q) {
  p <- importer_spectres(as.character(unlist(q$chemins)), q$infos, q$nom)
  p$id <- nouvel_id("P")
  p$nom <- p$source
  p$modifie <- TRUE  # à enregistrer : il n'a pas de fichier
  etat$projets[[p$id]] <- p
  r <- resume_projet(p)
  r$note <- I(p$note %||% character(0))
  r
}

# ---------------------------------------------------------------- HTTP

# Tout ce qui est ouvert : pour une nouvelle fenêtre de l'application.
etat_complet <- function() {
  list(projets = lapply(unname(etat$projets), resume_projet),
       modeles = lapply(names(etat$modeles), resultat_modele))
}

DEMANDES <- list(
  etat = function(q) etat_complet(),
  # les noms des projets aux changements non enregistrés (main.js, avant de fermer)
  projets_modifies = function(q) list(noms = I(unname(vapply(Filter(function(p) isTRUE(p$modifie), etat$projets), function(p) p$nom, "")))),
  fermer_projet = function(q) {
    etat$modeles <- Filter(function(e) !identical(e$projet, q$projet), etat$modeles)
    etat$projets[[q$projet]] <- NULL
    list(ferme = q$projet)
  },
  # Retire un modèle calculé ; avec definition = TRUE, sa définition quitte aussi le projet
  # (modèle fait dans l'application ; annulable).
  supprimer_modele = function(q) {
    e <- modele(q$modele)
    etat$modeles[[q$modele]] <- NULL
    if (isTRUE(q$definition)) {
      p <- projet(e$projet)
      memoriser(p, sprintf("suppression de %s", e$definition$name))
      p$models <- Filter(function(x) !identical(x$name, e$definition$name), p$models)
      p$modifie <- TRUE
      etat$projets[[p$id]] <- p
    }
    list(supprime = q$modele, projet = resume_projet(projet(e$projet)))
  },
  # Note libre d'un modèle, gardée avec sa définition dans le projet.
  note_modele = function(q) {
    e <- modele(q$modele)
    note <- as.character(q$note %||% "")
    e$definition$note <- note
    etat$modeles[[q$modele]] <- e
    p <- projet(e$projet)
    for (i in seq_along(p$models)) {
      if (identical(p$models[[i]]$name, e$definition$name) && !identical(p$models[[i]]$note %||% "", note)) {
        p$models[[i]]$note <- note
        p$modifie <- TRUE
      }
    }
    etat$projets[[p$id]] <- p
    list(note = note, projet = resume_projet(p))
  },
  # Copie de secours d'un projet pas encore enregistré, sans toucher à son état ; `lu` : l'état
  # de son fichier quand il l'a lu ou enregistré, que l'interface garde avec la copie
  sauvegarde_secours = function(q) {
    p <- projet(q$projet)
    if (!is.null(q$session)) p$session <- q$session
    enregistrer_projet(p, q$chemin, as.character(q$version %||% ""))
    list(chemin = q$chemin, lu = p$fichier_lu)
  },
  # q$secours : une copie de secours (q$chemin), rouverte comme le projet qu'elle garde : son nom,
  # son fichier (q$origine : celui où il s'enregistre, ou d'où il vient ; aucun pour un tableau
  # collé ou des spectres importés jamais enregistrés), l'état de ce fichier quand il l'avait lu
  # (q$lu), des changements à enregistrer. Jamais la copie elle-même comme fichier du projet :
  # elle est effacée quand Nuée se ferme.
  ouvrir = function(q) {
    r <- ouvrir_fichier(q$chemin, table = q$table)
    if (!(isTRUE(q$secours) || !is.null(q$origine)) || !is.null(r$choisir)) return(r)
    p <- projet(r$id)
    p$nom <- q$nom %||% basename(q$origine %||% p$nom)
    p$chemin <- q$origine
    p$fichier_lu <- fichier_lu_repris(q$lu, q$origine)
    p$modifie <- TRUE
    etat$projets[[p$id]] <- p
    resume_projet(p)
  },
  # Le projet d'exemple (données inventées, exemple/vergers.json) : ouvert comme un projet
  # nouveau, que Enregistrer range où l'on veut.
  exemple = function(q) {
    r <- ouvrir_fichier(file.path(racine, "exemple", "vergers.json"))
    p <- projet(r$id)
    p$chemin <- NULL
    etat$projets[[p$id]] <- p
    resume_projet(p)
  },
  projet = function(q) resume_projet(projet(q$projet)),
  donnees = function(q) donnees_projet(projet(q$projet)),
  ajuster = ajuster_demande,
  reajuster = reajuster_demande,
  modele = function(q) resultat_modele(q$modele),
  modeles = function(q) lapply(names(etat$modeles), function(id) {
    e <- etat$modeles[[id]]
    list(id = id, projet = e$projet, nom = e$definition$name %||% id)
  }),
  predire = predire_demande,
  hca = hca_demande,
  permutations = function(q) {
    e <- modele(q$modele)
    if (!identical(e$modele$type, "PLS")) stop(traduire("Le test de permutation ne concerne que les modèles PLS", "The permutation test only applies to PLS models"))
    r <- permutations(projet(e$projet), e$modele, e$definition, y = q$y %||% 1, n = max(1, min(q$n %||% 20, 999)))
    c(list(modele = q$modele), r)
  },
  valeurs = valeurs_demande,
  exporter = exporter_demande,
  exporter_donnees = exporter_donnees_demande,
  coller = coller_demande,
  # un jeu de test proposé (jeu_test.R) : au hasard ou par Kennard–Stone, par classe s'il y en a une
  jeu_test = function(q) {
    p <- projet(q$projet)
    obs <- setdiff(vapply(p$observations, function(o) as.numeric(o$uid), 0), as.numeric(q$exclues %||% numeric(0)))
    list(test = I(choisir_test(p, as.numeric(q$x), obs, q$methode %||% "hasard", as.numeric(q$part %||% 0.25),
                               if (length(q$classe)) as.numeric(q$classe))))
  },
  importer_spectres = importer_spectres_demande,
  predire_fichier = predire_fichier_demande,
  exporter_prediction = exporter_prediction_demande,
  scores_en_variables = scores_demande,
  classe_selection = classe_demande,
  script_r = script_demande,
  methodes = methodes_demande,
  modeles_classe = modeles_classe_demande,
  appartenance = appartenance_demande,
  contributions = function(q) {
    e <- modele(q$modele)
    c(list(modele = q$modele),
      as.list(contributions(e$modele, as.numeric(q$observations), q$type %||% "DModX", q$composante %||% 1, isTRUE(q$ponderee %||% TRUE))))
  },
  statistiques_variables = function(q) statistiques_variables(projet(q$projet)),
  # une variable au fil du temps, par lignes (le secteur) et par panneaux (le site), en long (series.R)
  series = function(q) series_variable(projet(q$projet), q$variable, q$temps, q$lignes, q$panneaux),
  # avec q$appariement (une variable ou "id:k") : les tests appariés de deux classes, t apparié par défaut
  tests_variables = function(q) tests_variables(projet(q$projet), as.numeric(q$classe), if (length(q$groupes)) as.character(unlist(q$groupes)),
                                               q$test %||% (if (length(q$appariement)) "t_apparie" else "welch"), q$correction %||% "BH",
                                               appariement = q$appariement),
  # avant/après avec témoin (comparaisons.R : baci) ; sans q$evenement, la série des écarts seule
  baci = function(q) baci(projet(q$projet), q$y, q$temps, q$unite, as.character(unlist(q$impact)), as.character(unlist(q$temoins)),
                          evenement = q$evenement, fin = q$fin, test = q$test %||% "welch"),
  # avec q$appariement (une variable ou "id:k") : le test apparié entre les niveaux q$niveaux, et
  # avec q$dans (une variable ou "id:k" : le site), toutes les paires de niveaux d'un même groupe
  test_deux_variables = function(q) test_deux_variables(projet(q$projet), q$x, q$y, appariement = q$appariement,
                                                        niveaux = if (length(q$niveaux)) as.character(unlist(q$niveaux)), dans = q$dans,
                                                        memo = memo_paires(projet(q$projet), q)),
  # les niveaux d'une variable qualitative ou d'un identifiant ("id:k"), dans l'ordre des tests
  niveaux = function(q) list(niveaux = I(niveaux_a_tester(projet(q$projet), q$variable))),
  # l'apport de chaque groupe de termes d'une régression (regression.R), calculé une fois
  # les courbes d'effet d'une régression (regression.R : effets_regression), calculées une fois
  # la formule R des réglages d'une régression (Nouveau modèle), et les réglages d'une formule :
  # lue sans jamais être exécutée (formule.R)
  formule = function(q) list(formule = formule_reglages(projet(q$projet), q)),
  lire_formule = function(q) lire_formule(projet(q$projet), q$texte %||% "", q$type, q$famille),
  # les seuils d'une courbe (regression.R : seuils_courbe), gardés avec le modèle
  seuils = function(q) {
    e <- modele(q$modele)
    if (!est_regression(e$definition)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    terme <- as.character(q$terme)[1]
    s <- e$modele$seuils[[terme]]
    if (is.null(s)) {
      s <- seuils_courbe(e$modele, terme)
      etat$modeles[[q$modele]]$modele$seuils[[terme]] <- s
    }
    c(list(modele = q$modele), s)
  },
  effets = function(q) {
    e <- modele(q$modele)
    if (!est_regression(e$definition)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    if (is.null(e$modele$effets)) etat$modeles[[q$modele]]$modele$effets <- effets_regression(e$modele)
    list(modele = q$modele, lien = e$modele$lien, effets = etat$modeles[[q$modele]]$modele$effets)
  },
  # l'autocorrélation des résidus d'une régression dans le temps (regression_diagnostics.R) ; sans
  # CAR1, q$temps : un groupement ({ by: "id" | "variable" }) ; q$groupe : un ou plusieurs
  # (emboîtés, du plus large au plus fin), dont { by: "effets" } (ses effets aléatoires), ou rien ;
  # q$type : les résidus d'un modèle de mgcv
  autocorrelation_residus = function(q) {
    e <- modele(q$modele)
    if (!est_regression(e$definition)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    c(list(modele = q$modele), autocorrelation_residus(projet(e$projet), e$modele, groupements_demande(q$temps), groupements_demande(q$groupe, TRUE),
                                                       q$type %||% "pearson"))
  },
  # les tests du rapport de vraisemblance entre les régressions emboîtées de q$modeles
  # (regression_diagnostics.R), pour Comparer les modèles
  rapports_vraisemblance = function(q) {
    ids <- Filter(function(id) !is.null(etat$modeles[[id]]) && est_regression(etat$modeles[[id]]$definition), as.character(unlist(q$modeles)))
    rapports_vraisemblance(stats::setNames(lapply(ids, function(id) etat$modeles[[id]]$modele), ids))
  },
  apports = function(q) {
    e <- modele(q$modele)
    if (!est_regression(e$definition)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    if (is.null(e$modele$apports)) etat$modeles[[q$modele]]$modele$apports <- apport_groupes(e$modele) %||% data.frame()
    list(modele = q$modele, apports = etat$modeles[[q$modele]]$modele$apports)
  },
  # la sélection des effets fixes par l'AIC (regression.R : selection_aic), calculée une fois et
  # gardée avec le modèle (le texte des Méthodes la décrit alors)
  selection_aic = function(q) {
    e <- modele(q$modele)
    if (!est_regression(e$definition)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    if (is.null(e$modele$selection_aic)) etat$modeles[[q$modele]]$modele$selection_aic <- selection_aic(e$modele, e$definition)
    c(list(modele = q$modele), etat$modeles[[q$modele]]$modele$selection_aic)
  },
  correlations_variables = function(q) correlations_variables(projet(q$projet), q$methode %||% "pearson", q$correction %||% "BH"),
  historique = function(q) historique_projet(projet(q$projet)),
  # données modifiées et variables ajoutées : le projet est à enregistrer
  modifier_valeur = function(q) changer_projet(q, function(p) modifier_valeur(p, q$observation, q$variable, q$valeur),
    sprintf("modification de %s", projet(q$projet)$noms_var[[as.character(q$variable)]])),
  variable_calculee = function(q) changer_projet(q, function(p) variable_calculee(p, q$nom, q$formule), sprintf("variable %s", q$nom)),
  # les valeurs manquantes remplies par ACP itérative régularisée (variables.R : imputer_acp)
  imputer = function(q) {
    memoriser(projet(q$projet), traduire("imputation", "imputation"))
    p <- imputer_acp(projet(q$projet), unlist(q$variables), k = q$k, bornes = !isFALSE(q$bornes), remplacer = isTRUE(q$remplacer))
    info <- attr(p, "imputation")
    attr(p, "imputation") <- NULL
    p$modifie <- TRUE
    etat$projets[[p$id]] <- p
    reviser(p$id)
    c(resume_projet(p), list(imputation = info))
  },
  classes_valeurs = function(q) changer_projet(q, function(p) classes_valeurs(p, q$nom, q$variable, bornes = q$bornes, n = q$n),
    sprintf("variable %s", q$nom)),
  annuler = annuler_demande,
  # Les spectres d'un modèle (variables X quantitatives, dans leur ordre), avant et après leur prétraitement.
  spectres = function(q) {
    e <- modele(q$modele)
    s <- spectres_modele(projet(e$projet), e$definition)
    list(modele = q$modele, noms = I(colnames(s$bruts)), variables = I(s$variables), uid = as.numeric(rownames(s$bruts)),
         bruts = unname(s$bruts), pretraites = unname(s$pretraites))
  },
  biplot = function(q) {
    e <- modele(q$modele)
    b <- correlations_biplot(e$modele)
    colonnes <- function(M) if (!is.null(M)) lapply(seq_len(ncol(M)), function(a) I(M[, a]))  # I() : un seul Y reste une liste
    list(modele = q$modele, t = colonnes(b$t), p = colonnes(b$p), c = colonnes(b$c),
         noms_x = I(e$modele$x$noms), noms_y = I(e$modele$y$noms %||% character(0)))
  },
  # iPLS (ipls.R) : q$n intervalles, 1 à q$A composantes (le A du modèle par défaut)
  ipls = function(q) {
    e <- modele(q$modele)
    c(list(modele = q$modele), pls_par_intervalles(projet(e$projet), e$definition, n = q$n %||% 20, A = q$A %||% e$modele$A))
  },
  verifier = function(q) {
    p <- projet(q$projet)
    if (identical(q$type, "CLASSES")) q <- utils::modifyList(q, list(type = "PCA", classe = NULL))
    verifier_donnees(p, definition_interface(p, q))
  },
  # La session (modèles calculés, vues ouvertes, sélections) est décrite par l'interface, en
  # texte JSON qu'elle seule relit ; elle est enregistrée avec le projet.
  session = function(q) list(session = projet(q$projet)$session %||% NULL),
  enregistrer = function(q) {
    p <- projet(q$projet)
    if (!is.null(q$session)) p$session <- q$session
    chemin <- q$chemin %||% file.path(tempdir(), paste0(tools::file_path_sans_ext(p$nom), ".nuee"))
    # le fichier qu'on avait lu ou écrit, changé depuis : l'interface demande avant de l'écraser
    lu <- p$fichier_lu
    if (!is.null(q$chemin) && !isTRUE(q$ecraser) && !is.null(lu) && file.exists(chemin) &&
        identical(etat_fichier(chemin)$chemin, lu$chemin) && !meme_etat(etat_fichier(chemin), lu)) {
      stop(conflit_fichier(chemin))
    }
    enregistrer_projet(p, chemin, as.character(q$version %||% ""))
    if (!is.null(q$chemin)) {
      p$chemin_enregistrement <- chemin
      p$fichier_lu <- etat_fichier(chemin)
    }
    p$modifie <- FALSE
    etat$projets[[p$id]] <- p
    list(chemin = chemin)
  },
  journal = function(q) list(entrees = journal_depuis(as.numeric(q$depuis %||% 0)), dernier = etat$n_journal),
  console = function(q) evaluer_console(q$code, q$modele, q$projet),
  completer = function(q) completer_console(as.character(q$texte %||% ""), as.integer(q$curseur %||% nchar(q$texte %||% "")), q$modele, q$projet),
  version = function(q) list(R = R.version.string, plateforme = R.version$platform, systeme = utils::osVersion %||% "",
                             locale = Sys.getlocale("LC_CTYPE"),
                             paquets = as.list(vapply(c("jsonlite", "httpuv", "openxlsx", "haven", "readxl", "foreign", "rmarkdown", "knitr"), function(x)
                               tryCatch(as.character(utils::packageVersion(x)), error = function(e) "-"), ""))))

# Les demandes propres d'une autre analyse (analyses.R) : chaque fonction demande_analyse_<nom>(q)
# du moteur est la demande <nom>.
for (f in ls(globalenv(), pattern = "^demande_analyse_")) DEMANDES[[sub("^demande_analyse_", "", f)]] <- get(f, envir = globalenv())

# Les valeurs non finies (NaN, Inf) deviennent des valeurs manquantes en JSON.
fini <- function(x) {
  if (is.data.frame(x) || is.list(x)) {
    for (i in seq_along(x)) if (!is.null(x[[i]])) x[[i]] <- fini(x[[i]])
    return(x)
  }
  if (is.numeric(x)) x[!is.finite(x)] <- NA
  x
}

repondre <- function(statut, contenu) {
  list(status = statut, headers = list("Content-Type" = "application/json; charset=utf-8"),
       body = enc2utf8(as.character(jsonlite::toJSON(fini(contenu), auto_unbox = TRUE, dataframe = "columns",
                                                     na = "null", null = "null", digits = NA, force = TRUE))))
}

appli <- list(
  call = function(req) {
    chemin <- req$PATH_INFO
    options(nuee.langue = if (identical(req$HTTP_X_LANGUE, "en")) "en" else "fr")  # langue des messages (langue.R)
    # un autre nom que 127.0.0.1 : un site qui se fait passer pour l'ordinateur
    if (!isTRUE(req$HTTP_HOST %in% HOTES)) return(list(status = 403L, headers = list(), body = "interdit"))
    if (identical(chemin, "/api/connexion")) return(connexion(req))
    if (!startsWith(chemin, "/api/")) return(list(status = 404L, headers = list(), body = "introuvable"))
    if (!identical(jeton_recu(req$HTTP_COOKIE), JETON)) {
      return(repondre(403L, list(erreur = traduire("Demande refusée : elle ne vient pas de la fenêtre de Nuée.",
                                                   "Request refused: it doesn't come from Nuée's window."))))
    }
    nom <- sub("^/api/", "", chemin)
    corps <- req$rook.input$read()
    tryCatch({
      if (nom == "fichier") {  # téléchargement d'un fichier écrit par le serveur (navigateur)
        f <- normalizePath(utils::URLdecode(sub(".*chemin=([^&]*).*", "\\1", req$QUERY_STRING)), winslash = "/")
        if (!startsWith(f, normalizePath(tempdir(), winslash = "/"))) stop(traduire("Fichier non autorisé", "File not allowed"))
        return(list(status = 200L, headers = list("Content-Type" = "application/octet-stream",
                                                  "Content-Disposition" = sprintf('attachment; filename="%s"', basename(f))),
                    body = readBin(f, "raw", file.size(f))))
      }
      if (nom %in% c("televerser", "deposer")) {  # fichier envoyé par l'interface (navigateur)
        nom_f <- utils::URLdecode(sub(".*nom=([^&]*).*", "\\1", req$QUERY_STRING))
        dest <- file.path(tempdir(), nouvel_id("f"))
        dir.create(dest)
        dest <- file.path(dest, basename(nom_f))
        writeBin(corps, dest)
        # televerser : ouvert comme projet ; deposer : seulement enregistré (fichier à prédire)
        if (nom == "deposer") return(repondre(200L, list(chemin = dest)))
        r <- ouvrir_fichier(dest, basename(nom_f))
        tryCatch(journaliser("ouvrir", list(chemin = dest), r), error = function(e) NULL)
        return(repondre(200L, r))
      }
      f <- DEMANDES[[nom]]
      if (is.null(f)) stop(traduire("Demande inconnue : ", "Unknown request: "), nom)
      q <- if (length(corps)) jsonlite::fromJSON(rawToChar(corps), simplifyVector = TRUE) else list()
      if (nom %in% SANS_REGRESSION && any(vapply(as.character(c(q$modele, unlist(q$modeles))), function(id) sans_composantes(modele(id)$definition), TRUE))) {
        stop(traduire("Ce n'est pas pour ce modèle : une régression ou une autre analyse n'a pas les composantes et les scores d'une ACP ou d'une PLS.",
                      "This isn't for this model: a regression or another analysis doesn't have the components and scores of a PCA or a PLS."))
      }
      r <- f(q)
      tryCatch(journaliser(nom, q, r), error = function(e) NULL)  # l'appel au moteur, pour la console R
      repondre(200L, r)
    }, error = function(e) {
      tryCatch(noter("erreur", sprintf("(%s) %s", nom, conditionMessage(e))), error = function(x) NULL)
      if (inherits(e, "conflit_fichier")) return(repondre(409L, list(erreur = conditionMessage(e), conflit = TRUE, date = e$date)))
      repondre(400L, list(erreur = conditionMessage(e)))
    })
  },
  # (l'interface relue à chaque chargement : no-cache, et 304 quand rien n'a changé. Sans
  # Cache-Control, Chromium la tenait pour fraîche un dixième de son âge, et une nouvelle version
  # montrait celle d'avant : app/main.js, demarrer)
  staticPaths = list("/" = httpuv::staticPath(interface, indexhtml = TRUE, headers = list("Cache-Control" = "no-cache")),
                     "/api" = httpuv::excludeStaticPath())  # les demandes vont à call()
)

serveur <- httpuv::startServer("127.0.0.1", port, appli)
cat("PRET", port, "\n")
if (!jeton_donne) cat(sprintf("Interface : http://127.0.0.1:%d/api/connexion?jeton=%s\n", port, JETON))
flush(stdout())
repeat httpuv::service(100)
