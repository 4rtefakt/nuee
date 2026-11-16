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
  ancien$documents <- actuel$documents  # (les documents ne passent pas par l'annulation : ce qu'on y a écrit reste)
  ancien$figures <- actuel$figures  # (les figures non plus)
  ancien$modifie <- TRUE
  suivre_conditions_seance(actuel, ancien)
  etat$projets[[q$projet]] <- ancien
  reviser(q$projet)
  list(projet = resume_projet(ancien), etiquette = pile[[1]]$etiquette)
}

# Les modèles de la séance du projet `p` dont la condition d'observations était celle de leur
# définition dans `avant` (le projet d'avant un changement) prennent celle de `p` : renommer ou
# fusionner des catégories la réécrit (atelier.R : suivre_categories), annuler la rend. Réajuster
# ces modèles part de leur propre définition (reajuster_demande).
suivre_conditions_seance <- function(avant, p) {
  for (id in names(etat$modeles)) {
    e <- etat$modeles[[id]]
    if (!identical(e$projet, p$id) || !avec_condition(e$definition)) next
    da <- Find(function(x) identical(x$name, e$definition$name), avant$models)
    dp <- Find(function(x) identical(x$name, e$definition$name), p$models)
    if (is.null(da) || is.null(dp) || !avec_condition(da) || !avec_condition(dp)) next
    ancienne <- texte_condition(da$condition)
    if (!identical(ancienne, texte_condition(dp$condition)) && identical(texte_condition(e$definition$condition), ancienne)) {
      etat$modeles[[id]]$definition$condition <- dp$condition
    }
  }
}

# Un fichier qui a plusieurs tableaux (feuilles Excel, objets R) et dont le tableau n'est pas
# choisi renvoie leur liste (`choisir`) : l'interface demande lequel ouvrir. Un export de
# KoboToolbox ou d'ODK Central (kobo.R ; une archive .zip n'est ouverte que comme tel) renvoie
# l'aperçu de son import (`kobo`), que l'interface propose, sauf pour l'ouvrir tel quel (`brut`) ;
# `collecte` : l'import est demandé (Fichier › Importer depuis KoboToolbox ou ODK), un autre
# fichier est refusé ; `kobo` : les réglages de l'import, qui le font (le moteur relancé).
ouvrir_fichier <- function(chemin, nom = basename(chemin), table = NULL, brut = FALSE, collecte = FALSE, kobo = NULL) {
  ext <- extension(nom)
  if (!is.null(kobo)) return(ouvrir_collecte(chemin, nom, kobo))
  if (!brut && is.null(table) && (collecte || (ext %in% EXTENSIONS_COLLECTE && (ext == "zip" || !is.null(detecter_collecte(chemin)))))) {
    return(list(kobo = apercu_collecte(chemin), chemin = chemin, nom = nom))
  }
  if (!ext %in% c("nuee", "json", "usp", "zip", FORMATS_LECTURE)) stop(traduire("Format de fichier non reconnu : .", "Unrecognised file format: ."), ext)
  if (!ext %in% c("zip", FORMATS_LECTURE)) table <- NULL
  if (ext %in% FORMATS_LECTURE && is.null(table)) {
    tables <- tables_fichier(chemin)
    if (length(tables) > 1) return(list(choisir = I(tables), chemin = chemin, nom = nom))
  }
  # (un tableau importé garde d'où viennent ses données, et ce que l'import en a décidé, pour les
  # relire, le fichier changé ou celui de l'année suivante : etapes.R ; un .nuee garde la sienne)
  p <- switch(ext,
              nuee = , json = lire_projet(chemin),
              usp = lire_projet(convertir_usp(chemin)),
              zip = avec_source(projet_brut_collecte(chemin, table, identifiants = list()), chemin, "archive", table),  # (un CSV d'une archive d'ODK, tel qu'écrit : kobo.R)
              avec_source(importer_tableau(chemin, table, reglages = list()), chemin, "tableau", table))
  p$id <- nouvel_id("P")
  p$nom <- if (is.null(table)) nom else sprintf("%s (%s).%s", tools::file_path_sans_ext(nom), table, tools::file_ext(nom))
  p$chemin <- chemin
  p$table <- table
  if (brut) p$brut <- TRUE  # (ouvert tel quel : un export de KoboToolbox ou d'ODK se rouvre de même, le moteur relancé)
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

# Une table d'un export de KoboToolbox ou d'ODK Central importée en projet (kobo.R :
# importer_collecte), avec les réglages `o` du dialogue de l'import : nommée comme une feuille d'un
# classeur quand c'est un groupe répété, « inventaire (observation).xlsx ». Son fichier reste
# celui de l'export, et sa table celle importée (un autre import du même fichier reste possible).
# Sa source (etapes.R) garde les réglages de l'import et son formulaire, pour le refaire sur l'export
# suivant.
ouvrir_collecte <- function(chemin, nom, o) {
  p <- importer_collecte(chemin, table = o$table, garder = o$garder, parents = o$parents, formulaire = o$formulaire, langue = o$langue, identifiants = list())
  info <- attr(p, "collecte")
  attr(p, "collecte") <- NULL
  p <- avec_source(p, chemin, "collecte", collecte = p$import_collecte)
  p$id <- nouvel_id("P")
  p$nom <- if (isTRUE(p$collecte$repetition)) sprintf("%s (%s).%s", tools::file_path_sans_ext(nom), p$collecte$table, tools::file_ext(nom)) else nom
  p$chemin <- chemin
  p$table <- p$collecte$table
  etat$projets[[p$id]] <- p
  r <- resume_projet(p)
  if (length(p$note)) r$note <- I(as.character(p$note))
  r$collecte <- c(info[setdiff(names(info), "phrases")], list(phrases = I(info$phrases)))
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
  # (I() : une liste même pour une seule composante, que l'interface lit composante par composante)
  cle <- function(k) I(vapply(ref, function(x) as.numeric(x[[k]] %||% NA), 0))
  list(R2X = cle("R2X"), R2Xcum = cle("R2X(cum)"), R2Y = cle("R2Y"), R2Ycum = cle("R2Y(cum)"),
       Q2 = cle("Q2"), Q2cum = cle("Q2(cum)"))
}

resume_modele <- function(d) {
  list(numero = d$number %||% NA, nom = d$name %||% "", titre = d$title %||% "",
       type = paste0(if (isTRUE(d$opls)) "O" else "", if (isTRUE(d$plsda)) "PLS-DA" else d$type %||% ""), A = d$A %||% 0,
       ajuste = isTRUE(d$fitted) || !is.null(d$app), erreur = d$error %||% NULL,
       n_obs = length(d$observations), x = vapply(Filter(function(t) !isTRUE(t$facteur_seul), d$x), function(t) as.character(t$name), ""),  # (sans les facteurs seuls d'une régression)
       y = vapply(d$y, function(t) as.character(t$name), ""), stocke = composantes_stockees(d), note = d$note %||% "",
       version_origine = if (!isTRUE(d$app)) d$version_origine %||% NULL)
}

# Les variables qui ont des valeurs dans les jeux de données. Les fichiers .usp listent aussi des variables
# internes sans données (par exemple « $constant »), qui ne servent ni à l'affichage ni aux modèles.
variables_avec_donnees <- function(p) {
  Filter(function(v) as.character(v$uid) %in% colnames(p$donnees), p$variables)
}

# (temps : une variable qualitative dont les catégories sont toutes des dates, ou toutes des
# nombres, qui peut servir de temps comme une variable quantitative : temps_en_nombres)
resume_projet <- function(p) {
  manquantes <- colSums(is.na(p$donnees))
  # (l'unité et la description de chaque variable : atelier.R, decrire_variables)
  vars <- lapply(variables_avec_donnees(p), function(v) list(
    uid = v$uid, nom = as.character(v$ids[1]), qualitative = length(v$categories) > 0,
    categories = I(as.character(v$categories %||% character(0))), manquantes = unname(manquantes[as.character(v$uid)]),
    temps = length(v$categories) > 0 && is.null(attr(temps_en_nombres(as.character(v$categories)), "refus")),
    unite = as.character(v$unite %||% ""), description = as.character(v$description %||% "")))
  json <- tolower(tools::file_ext(p$chemin %||% "")) %in% c("nuee", "json")
  pile <- etat$annulations[[p$id]]
  list(id = p$id, nom = p$nom, source = p$source %||% "", modifie = isTRUE(p$modifie), chemin = p$chemin %||% NULL,
       table = p$table %||% NULL, brut = isTRUE(p$brut),
       annulable = if (length(pile)) pile[[1]]$etiquette, revision = etat$revisions[[p$id]] %||% 0,
       chemin_enregistrement = p$chemin_enregistrement %||% (if (json) p$chemin),
       observations = list(uid = as.numeric(names(p$noms_obs)), nom = unname(p$noms_obs)),
       identifiants = identifiants_secondaires(p),
       variables = vars,
       jeux = lapply(p$datasets, function(d) list(nom = d$name %||% "", n_obs = length(d$observations),
                                                  n_var = length(d$variables))),
       modeles = lapply(p$models, resume_modele), session = !is.null(p$session),
       coordonnees = coordonnees_projet(p),
       documents = lapply(p$documents %||% list(), function(d) list(id = d$id, titre = d$titre %||% "", langue = d$langue %||% "fr")),
       figures = lapply(p$figures %||% list(), function(f) list(id = f$id, titre = f$titre %||% "")),
       kobo = p$import_collecte,  # (les réglages d'un import de KoboToolbox ou d'ODK, pour le refaire : kobo.R)
       lecture_seule = p$lecture_seule,  # (une ancienne version ouverte en lecture seule : sa source, son oid, sa date ; ouvrir)
       historique = sum(vapply(p$audit %||% list(), function(s) length(s$events), 0)),
       predictions = lapply(p$prediction_sets, function(s) list(nom = s$name %||% "", n_obs = length(s$observations))))
}

# Les données d'un projet pour la vue Données : `colonnes`, les numéros des variables dont elle montre
# les valeurs (une vue de spectres n'en montre que les premières, ou celles que son filtre garde) ; les
# autres ont des valeurs nulles (NULL), et leur nom. Sans `colonnes`, toutes.
donnees_projet <- function(p, colonnes = NULL) {
  vars <- variables_avec_donnees(p)
  montrees <- if (!is.null(colonnes)) as.numeric(unlist(colonnes))
  valeurs <- lapply(seq_along(vars), function(k) {
    if (!is.null(montrees) && !as.numeric(vars[[k]]$uid) %in% montrees) return(NULL)
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
# Un modèle de la séance, sa définition rapportée aux catégories du projet d'aujourd'hui (atelier.R :
# aligner_categories ; elles ont pu changer depuis qu'il a été ajusté). `a_jour` : pour le réajuster,
# prédire ou refaire un calcul sur les données, refusé quand une catégorie qu'il a vue a changé de
# nom ou disparu ; ses résultats, eux, se montrent toujours.
modele <- function(id, a_jour = FALSE) {
  e <- etat$modeles[[id]] %||% stop(traduire("Modèle inconnu : ", "Unknown model: "), id)
  p <- etat$projets[[e$projet]]
  if (length(e$categories) && !is.null(p)) {
    d <- aligner_categories(p, e$definition, e$categories)
    if (is.null(attr(d, "perimee"))) e$definition <- d
    else if (a_jour) stop(attr(d, "perimee"), call. = FALSE)
  }
  e
}

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
                      options = q$options, condition = q$condition, par_courbes = if (length(q$par_courbes)) q$par_courbes,
                      lisse_spatial = if (length(q$lisse_spatial)) q$lisse_spatial,
                      validation = q$validation)
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
  e <- modele(q$modele, TRUE)
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
    if (!is.null(d$baci_origine)) d$baci_origine$derive <- TRUE  # (le modèle BACI sans ces observations : il en dérive)
    if (avec_condition(d)) d$cochees <- setdiff(as.numeric(unlist(d$cochees %||% d$observations)), exclues)  # (décochées : condition.R)
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

# (m : le modèle déjà ajusté sur la définition `d`, s'il l'est : baci_modele_demande). Une définition
# à condition est gardée avec les observations qu'elle donne sur les données du moment (condition.R).
# L'ajustement automatique d'une ACP ou d'une PLS a déjà calculé le modèle de ses composantes
# (nouveau.R : autoajuster_modele) : ajuster le reprend, sans refaire sa validation croisée.
ajuster_et_garder <- function(p, d, m = NULL) {
  d <- resoudre_observations(p, d)
  calcule <- NULL
  if (!d$A && !sans_composantes(d)) {  # (ni une régression ni une autre analyse n'a de composantes)
    auto <- autoajuster_modele(p, d, garder = is.null(m))
    d$A <- max(1, auto$A)
    calcule <- auto$modele
  }
  if (is.null(m)) m <- ajuster(p, d, calcule = calcule)
  # (les moyennes ajustées, la puissance et la dépendance partielle gardées avec un modèle
  # enregistré, refaites avec leurs réglages ; oubliées quand elles ne s'appliquent plus à lui)
  for (cle in intersect(c("moyennes", "puissance_tendance", "dependance"), names(d))) {
    m[[cle]] <- tryCatch(refaire_garde(p, d, m, cle), error = function(err) NULL)
    if (!length(m[[cle]])) d[[cle]] <- NULL
  }
  id <- nouvel_id("M")
  if (isTRUE(d$app)) {  # le modèle rejoint le projet (enregistré avec lui)
    d$number <- d$number %||% (max(c(0, vapply(p$models, function(x) as.numeric(x$number %||% 0), 0))) + 1)
    d$fitted <- TRUE
    ancien <- Find(function(x) identical(x$name, d$name), p$models)
    p$models <- c(Filter(function(x) !identical(x$name, d$name), p$models), list(d))
    if (!identical(ancien, d)) p$modifie <- TRUE  # recalculer un modèle enregistré ne change pas le projet
    etat$projets[[p$id]] <- p
  }
  # (les catégories de ses variables qualitatives, telles qu'il les a vues : modele(), atelier.R)
  etat$modeles[[id]] <- list(projet = p$id, definition = d, modele = m, categories = categories_definition(p, d))
  resultat_modele(id)
}

# Un calcul gardé avec le modèle `m` (de définition `d`, du projet `p`), refait avec les réglages
# que sa définition a gardés (garder_avec_definition) : les moyennes ajustées et la puissance d'une
# régression (dont la simulation reste, quand elle porte sur les mêmes réglages), la dépendance
# partielle des variables d'une forêt ou d'arbres boostés (forets.R) ; NULL s'il ne s'applique pas.
refaire_garde <- function(p, d, m, cle) {
  g <- d[[cle]]
  switch(cle,
    moyennes = if (est_regression(d)) moyennes_ajustees(p, m, d, g),
    puissance_tendance = if (est_regression(d)) puissance_tendance(m, d, g$reglages, g),
    dependance = if (isTRUE(m$type %in% c("FOREST", "BOOST"))) {
      noms <- vapply(m$variables, `[[`, "", "nom")
      v <- intersect(noms, as.character(unlist(g)))
      stats::setNames(lapply(v, function(x) dependance_variable(m, match(x, noms))), v)
    })
}

# Avant/après (BACI) › Ajuster le modèle mixte (comparaisons.R : modele_baci) : ses deux variables
# 0/1, ajoutées au projet comme une Nouvelle variable (Ctrl+Z les retire) ou reprises, puis le
# modèle, qui rejoint le projet comme ceux d'Ajuster ce modèle. Le modèle des mêmes réglages déjà
# dans le projet garde son nom et son numéro (il est ajusté à nouveau) ; un autre prend un nom libre,
# « BACI y (2) »... Le modèle est ajusté avant que le projet ne change : un échec ne laisse rien.
baci_modele_demande <- function(q) {
  p <- projet(q$projet)
  b <- modele_baci(p, q$y, q$temps, q$unite, as.character(unlist(q$impact)), as.character(unlist(q$temoins)), q$evenement, q$fin, isTRUE(q$car1))
  d <- b$definition
  d$app <- TRUE
  meme <- Find(function(x) !is.null(x$baci_origine) && memes_reglages_baci(x$baci_origine, d$baci_origine), p$models)
  if (!is.null(meme)) {
    d$name <- meme$name
    d$number <- meme$number
  } else {
    pris <- c(vapply(p$models, function(x) as.character(x$name %||% ""), ""),
              vapply(Filter(function(e) identical(e$projet, p$id), etat$modeles), function(e) as.character(e$definition$name %||% ""), ""))
    base <- d$name
    k <- 1
    while (d$name %in% pris) {
      k <- k + 1
      d$name <- sprintf("%s (%d)", base, k)
    }
  }
  m <- ajuster(b$projet, d)
  if (length(b$nouvelles)) {
    etiquette <- sprintf(if (length(b$nouvelles) > 1) "variables %s" else "variable %s", paste(b$nouvelles, collapse = ", "))
    b$projet <- noter_etape(p, b$projet, "baci_modele", q, etiquette)
    memoriser(p, etiquette)
    b$projet$modifie <- TRUE
    etat$projets[[p$id]] <- b$projet
    reviser(p$id)
  }
  list(modele = ajuster_et_garder(projet(p$id), d, m), variables = I(unname(b$variables)), nouvelles = I(b$nouvelles))
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
       coefficients_regression = r$coefficients, termes_retires = I(r$termes_retires), termes_absents = I(r$termes_absents %||% character(0)),
       famille = m$famille, lien = m$lien, parametre_loi = r$parametre_loi, courbes = r$courbes, decalage = m$decalage, dispersion = r$dispersion,
       gamm = isTRUE(m$gamm), dans = m$dans,
       qualite = c(r$r2, list(AIC = r$AIC, AICc = r$AICc, BIC = r$BIC, logLik = r$logLik, parametres = r$parametres,
                              Q2 = m$vc$Q2, RMSEcv = m$vc$RMSEcv, echecs_vc = m$vc$echecs,
                              # (les observations sans prédiction en validation croisée : leur catégorie manquait aux autres groupes)
                              sans_categorie_vc = m$vc$sans_categorie %||% 0, facteurs_vc = I(m$vc$facteurs %||% character(0)))),
       variances = variances, phi = r$phi, niveaux = I(m$niveaux), temps = m$temps, exclues_manquantes = length(m$exclues),
       # (les pentes aléatoires de chaque niveau, les parts de variance des niveaux et les signes d'un ajustement singulier)
       pentes = pentes_regression(m), parts_variance = parts_regression(m), singularites = singularites_regression(m),
       # (ses variables qualitatives : celles qui ont des moyennes ajustées, moyennes.R)
       qualitatives = I(unname(vapply(qualitatives_modele(m), `[[`, "", "nom"))),
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
       # (la façon de valider, et les termes qui ont pris le poids du modèle complet dans un
       # sous-modèle, avec le nom de son groupe : validation_croisee.R)
       validation = m$validation, gardes_vc = gardes_vc(p, d, m),
       q2_vide = m$q2_vide, gain = m$gain,  # (le modèle vide et le gain sur lui : validation_croisee.R, fin_validation)
       hierarchique = m$hierarchique,  # (le Q2 honnête d'un modèle hiérarchique : hierarchique.R)
       noms_y = I(m$y$noms %||% character(0)),  # I() : un seul Y reste une liste
       G = m$G, vc_par = m$vc_par %||% "", reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       cv_anova = s$cv_anova, classification = s$classification, test = m$test,
       demande = d$demande %||% list(type = d$type, numero = d$number))
}

predire_demande <- function(q) {
  e <- modele(q$modele, TRUE)
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
  e <- modele(q$modele, TRUE)
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
  # (ses colonnes dans la langue de l'interface, comme celles d'Exporter les résultats : formats.R)
  out <- noms_tables_langue(list(predictions = out), projet(modele(q$modele)$projet))[[1]]
  ecrire_tableau(out, chemin, objet = "predictions")
  list(chemin = chemin, chemins = I(chemin))
}

# Les scores d'un modèle deviennent des variables du projet (modèles hiérarchiques). L'étape garde
# la définition du modèle telle qu'il a été ajusté : rejouée, elle l'ajuste de nouveau sur les
# données du moment (etapes.R : modele_rejoue).
scores_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  r <- scores_en_variables(p, e$modele)
  etiquette <- sprintf("scores de %s en variables", e$definition$name %||% q$modele)
  r$projet <- noter_etape(p, r$projet, "scores_en_variables", q, etiquette, list(definition = e$definition))
  memoriser(p, etiquette)
  r$projet$modifie <- TRUE
  etat$projets[[p$id]] <- r$projet
  reviser(p$id)
  list(projet = resume_projet(r$projet), noms = I(r$noms), modeles = I(modeles_utilisant(r$projet, r$noms)))
}

# Un changement du projet q$projet par f, retenu pour Ctrl+Z (memoriser) une fois fait : un
# changement refusé (une formule qui ne se calcule pas...) ne laisse pas de retour en arrière vide.
# `etape` : la demande, que le projet garde en étape pour la rejouer (etapes.R : noter_etape ;
# `contexte`, ce qu'elle sait de plus) ; Ctrl+Z retire l'étape avec le changement.
changer_projet <- function(q, f, etiquette, etape = NULL, contexte = NULL) {
  avant <- projet(q$projet)
  p <- f(avant)
  if (!is.null(etape)) p <- noter_etape(avant, p, etape, q, etiquette, contexte)
  suivre_conditions_seance(avant, p)
  memoriser(avant, etiquette)
  p$modifie <- TRUE
  etat$projets[[p$id]] <- p
  reviser(p$id)
  resume_projet(p)
}

# Comme changer_projet, avec ce que le changement dit de lui-même (attr(, attribut) du projet
# changé : atelier.R), rendu à côté du résumé du projet.
changer_projet_dit <- function(q, f, etiquette, attribut, etape = NULL) {
  info <- NULL
  r <- changer_projet(q, function(p) {
    n <- f(p)
    info <<- attr(n, attribut)
    attr(n, attribut) <- NULL
    n
  }, etiquette, etape)
  c(r, stats::setNames(list(info), attribut))
}

classe_demande <- function(q) {
  avant <- projet(q$projet)
  etiquette <- sprintf("classe %s", q$etiquette)
  p <- noter_etape(avant, appliquer_etape("classe_selection", avant, q), "classe_selection", q, etiquette)
  memoriser(avant, etiquette)
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
  e <- modele(ids[1], TRUE)
  seance <- length(ids) > 1
  nom <- gsub("[^[:alnum:]_ -]", "_", if (seance) as.character(projet(e$projet)$source %||% "seance") else e$definition$name %||% "modele")
  format <- if (!seance && isTRUE(q$format %in% c("Rmd", "qmd"))) q$format else "R"
  chemin <- q$chemin %||% file.path(tempdir(), nouvel_id("script"), paste0(nom, ".", format))
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE)
  options <- list(donnees = if (identical(q$donnees, "csv")) "csv" else "script", renv = isTRUE(q$renv),
                  rdata = isTRUE(q$rdata), rproj = isTRUE(q$rproj), version = as.character(q$version %||% ""),
                  langue = if (identical(q$langue, "en")) "en" else "fr")
  if (!seance) return(do.call(ecrire_script, c(list(projet(e$projet), e$definition, chemin, file.path(racine, "moteur"), m = e$modele), options)))
  es <- lapply(ids, modele, a_jour = TRUE)
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
  tables <- noms_tables_langue(tables_resultats(projet(e$projet), e$modele), projet(e$projet))
  if (extension(q$chemin) == "gpkg") {  # (les observations en points, les autres tableaux à côté : geopackage.R)
    ecrire_geopackage_resultats(projet(e$projet), e$modele, tables, q$chemin, nom)
    return(list(chemin = q$chemin, chemins = I(q$chemin)))
  }
  chemins <- ecrire_tables(tables, q$chemin, objet = make.names(nom))
  list(chemin = chemins[1], chemins = I(chemins))
}

# Les régressions `ids` de la séance, dans cet ordre, nommées comme leurs colonnes dans les
# coefficients côte à côte (coefficients_modeles.R) : leur nom, numéroté quand deux modèles de
# projets différents ont le même.
regressions_demande <- function(ids) {
  ms <- lapply(as.character(unlist(ids)), function(id) modele(id)$modele)
  stats::setNames(ms, make.unique(vapply(ms, function(m) as.character(m$nom %||% ""), ""), sep = " "))
}

# Les données d'un projet, dans le format de l'extension du chemin ; un GeoPackage : une couche
# de points de ses coordonnées (geopackage.R). Les unités et descriptions des variables (atelier.R)
# vont où le format a leur place : une table « Variables » d'un GeoPackage, une feuille d'un
# classeur Excel (formats.R : ecrire_tableau).
exporter_donnees_demande <- function(q) {
  p <- projet(q$projet)
  chemin <- q$chemin %||% chemin_temporaire(paste0(nom_fichier(p$nom), ".", q$format %||% "xlsx"))
  variables <- descriptions_variables(p)
  if (extension(chemin) == "gpkg") {
    pts <- points_projet(p)
    ecrire_geopackage(tableau_projet(p), chemin, pts$x, pts$y, pts$systeme, couche = nom_fichier(p$nom),
                      tables = if (!is.null(variables)) list(Variables = variables) else list())
  } else {
    ecrire_tableau(tableau_projet(p), chemin, objet = make.names(nom_fichier(p$nom)), variables = variables)
  }
  list(chemin = chemin, chemins = I(chemin))
}

# Un tableau collé depuis un tableur : écrit dans un fichier de la session (que le journal de
# la console R relit), puis importé comme un tableau ouvert. Sans fichier qui dure, il n'a pas de
# source, mais il note ses étapes (etapes.R) : on peut le mettre à jour d'un fichier.
coller_demande <- function(q) {
  texte <- as.character(q$texte %||% "")
  if (!grepl("[\t;,]", texte) || !grepl("\n.", texte)) stop(traduire("Le presse-papiers ne contient pas de tableau (au moins deux lignes et deux colonnes)", "The clipboard holds no table (at least two rows and two columns)"))
  chemin <- chemin_temporaire("tableau colle.tsv")
  writeLines(enc2utf8(texte), chemin, useBytes = TRUE)
  p <- debut_etapes(importer_tableau(chemin))
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
# un tableau d'informations sur les échantillons s'il est donné (fichiers_spectres.R). Fait de
# plusieurs fichiers, il n'a pas de source qu'on relise (etapes.R), mais il note ses étapes.
importer_spectres_demande <- function(q) {
  p <- debut_etapes(importer_spectres(as.character(unlist(q$chemins)), q$infos, q$nom))
  p$id <- nouvel_id("P")
  p$nom <- p$source
  p$modifie <- TRUE  # à enregistrer : il n'a pas de fichier
  etat$projets[[p$id]] <- p
  r <- resume_projet(p)
  r$note <- I(p$note %||% character(0))
  r
}

# Le tableau d'un fichier dont on ajoute des colonnes au projet (Édition › Ajouter des colonnes d'un
# tableau), dans tout format que lit Fichier › Ouvrir : un tableau (formats.R : lire_tableau ;
# `textes` : les colonnes de la clé, lues telles qu'écrites), ou les données d'un projet (.nuee,
# .json, .usp ; formats.R : tableau_projet).
tableau_a_joindre <- function(chemin, table = NULL, textes = NULL) {
  switch(extension(chemin),
         nuee = , json = tableau_projet(lire_projet(chemin)),
         usp = tableau_projet(lire_projet(convertir_usp(chemin))),
         lire_tableau(chemin, table, textes = textes))
}

# Ajouter des colonnes d'un tableau, avant d'ajouter : le tableau d'un fichier (q$chemin ; q$table,
# sa feuille ou son tableau, le premier sinon) ou des cellules collées (q$texte), gardées dans un
# fichier de la session comme Coller un tableau ; ses tableaux, ses colonnes (des nombres ou non,
# et le nom qu'elles prendraient dans le projet : variables.R, nom_libre) et les clés proposées
# (variables.R : proposer_cles).
table_a_joindre_demande <- function(q) {
  p <- projet(q$projet)
  chemin <- as.character(q$chemin %||% "")
  colle <- !is.null(q$texte)
  if (colle) {
    texte <- as.character(q$texte)
    if (!grepl("[\t;,]", texte) || !grepl("\n.", texte)) stop(traduire("Le presse-papiers ne contient pas de tableau (au moins deux lignes et deux colonnes)", "The clipboard holds no table (at least two rows and two columns)"))
    chemin <- chemin_temporaire("tableau colle.tsv")
    writeLines(enc2utf8(texte), chemin, useBytes = TRUE)
  }
  ext <- extension(chemin)
  if (!ext %in% c("nuee", "json", "usp", FORMATS_LECTURE)) stop(traduire("Format de fichier non reconnu : .", "Unrecognised file format: ."), ext)
  tables <- if (ext %in% FORMATS_LECTURE) tables_fichier(chemin)
  # (named even when there's one: tables_fichier may have set aside a codebook sheet, Variables,
  # and the first sheet of the file would be it)
  table <- if (length(tables)) as.character(q$table %||% tables[1])
  df <- tableau_a_joindre(chemin, table)
  pris <- c(as.character(p$noms_var), as.character(p$observation_id_names %||% character(0)))
  noms <- character(0)
  for (n in names(df)) noms <- c(noms, nom_libre(n, c(pris, noms)))
  list(chemin = chemin, nom = if (colle) traduire("cellules collées", "pasted cells") else basename(chemin), colle = colle,
       tables = if (length(tables) > 1) I(tables), table = table, n = nrow(df),
       colonnes = list(nom = I(names(df)), nombres = I(unname(vapply(df, is.numeric, TRUE))), nom_ajoute = I(noms)),
       cles = proposer_cles(p, df))
}

# Les colonnes d'un tableau ajoutées au projet par une clé (variables.R : ajouter_colonnes_table), le
# tableau relu avec les colonnes de la clé telles qu'écrites ; comme une Nouvelle variable, Ctrl+Z
# les retire. q$nom : le nom du tableau, pour Annuler. L'étape garde le fichier comme la source du
# projet (etapes.R), ou le texte des cellules collées.
ajouter_colonnes_demande <- function(q) {
  p <- projet(q$projet)
  etiquette <- sprintf(traduire("colonnes de %s", "columns from %s"), q$nom %||% basename(q$chemin))
  n <- noter_etape(p, appliquer_etape("ajouter_colonnes", p, q), "ajouter_colonnes", q, etiquette)
  memoriser(p, etiquette)
  info <- attr(n, "jointure")
  attr(n, "jointure") <- NULL
  n$modifie <- TRUE
  etat$projets[[p$id]] <- n
  reviser(p$id)
  c(resume_projet(n), list(jointure = info))
}

# Les fichiers d'Ajouter des valeurs de rasters, comme l'interface les envoie (un tableau d'objets,
# que jsonlite lit en data.frame) : une liste de list(chemin, methode, rayon, noms).
fichiers_raster_demande <- function(x) {
  if (is.data.frame(x)) return(lapply(seq_len(nrow(x)), function(i) lapply(as.list(x[i, , drop = FALSE]), function(v) if (is.list(v)) v[[1]] else v)))
  if (!is.null(x$chemin)) return(list(x))
  x
}

# Les valeurs de rasters ajoutées au projet (raster.R : ajouter_valeurs_rasters), une variable par
# bande ; comme une Nouvelle variable, Ctrl+Z les retire. L'étape garde chaque fichier comme la
# source du projet (etapes.R).
ajouter_raster_demande <- function(q) {
  p <- projet(q$projet)
  noms <- unique(basename(vapply(fichiers_raster_demande(q$fichiers), function(f) as.character(f$chemin)[1], "")))
  etiquette <- sprintf(traduire("valeurs de %s", "values from %s"), paste0(paste(utils::head(noms, 2), collapse = ", "), if (length(noms) > 2) "…" else ""))
  n <- noter_etape(p, appliquer_etape("ajouter_raster", p, q), "ajouter_raster", q, etiquette)
  memoriser(p, etiquette)
  info <- attr(n, "raster")
  attr(n, "raster") <- NULL
  n$modifie <- TRUE
  etat$projets[[p$id]] <- n
  reviser(p$id)
  c(resume_projet(n), list(raster = info))
}

# Résumer par groupe : un nouveau projet, une ligne par groupe (nouveau.R : resumer_par_groupe), sans
# fichier, à enregistrer comme un tableau collé ; comme lui, il note ses étapes (etapes.R).
resumer_groupes_demande <- function(q) {
  p <- resumer_par_groupe(projet(q$projet), as.character(unlist(q$groupes)),
                          variables = if (!is.null(q$variables)) as.numeric(unlist(q$variables)), statistiques = as.character(unlist(q$statistiques %||% "moyenne")))
  info <- attr(p, "resume")
  attr(p, "resume") <- NULL
  p <- debut_etapes(p)
  p$id <- nouvel_id("P")
  p$nom <- p$source
  p$modifie <- TRUE
  etat$projets[[p$id]] <- p
  c(resume_projet(p), list(resume = info))
}

# ---------------------------------------------------------------- les rapports vivants

# La source et les étapes d'un projet (etapes.R), pour l'interface : DEMANDES$etapes.
etapes_demande <- function(q) {
  p <- projet(q$projet)
  dossier <- dossier_projet(p)
  s <- p$source_donnees
  source <- if (is.list(s)) c(etat_fiche(s, dossier), list(table = if (nzchar(premier_texte(s$table))) premier_texte(s$table), lecteur = premier_texte(s$lecteur)),
                              if (is.list(s$formulaire)) list(formulaire = etat_fiche(s$formulaire, dossier)))
  conformes <- donnees_conformes(p)
  list(source = source,
       etapes = lapply(seq_along(p$etapes), function(k) {
         e <- p$etapes[[k]]
         list(n = k, etiquette = premier_texte(e$etiquette), date = premier_texte(e$date), demande = premier_texte(e$demande),
              fichiers = lapply(e$fichiers %||% list(), etat_fiche, dossier))
       }),
       conformes = conformes,
       raison = if (isFALSE(conformes)) traduire("Les données ont changé hors des étapes (dans la console R, ou dans une version de Nuée d'avant la 1.1) : les mettre à jour reprendrait celles que la source et les étapes donnent.",
                                                  "The data changed outside the steps (in the R console, or in a version of Nuée before 1.1): updating them would bring back those the source and the steps give.")
                else if (is.na(conformes)) traduire("Projet d'une version de Nuée d'avant la 1.1 : ses changements d'alors ne sont pas des étapes.",
                                                    "A project from a version of Nuée before 1.1: its changes from then aren't steps.") else "")
}

# Un projet mis à jour (etapes.R : mise_a_jour) qui prend la place de `actuel`, un seul Ctrl+Z. Les
# modèles de la séance suivent leurs définitions comme après un changement des catégories (leurs
# conditions ; leurs catégories, par leurs noms : modele, aligner_categories).
remplacer_par_mise_a_jour <- function(actuel, r, etiquette) {
  memoriser(actuel, etiquette)
  n <- r$projet
  suivre_conditions_seance(actuel, n)
  n$modifie <- TRUE
  etat$projets[[actuel$id]] <- n
  reviser(actuel$id)
  c(r$rapport, list(projet = resume_projet(n)))
}

# Les données mises à jour : DEMANDES$mettre_a_jour.
mettre_a_jour_demande <- function(q) {
  actuel <- projet(q$projet)
  r <- mise_a_jour(actuel, q$chemin, q$table, as.character(unlist(q$sans_nouvelles)))
  if (!is.null(r$choisir)) return(r)
  if (isTRUE(q$apercu)) return(c(r$rapport, list(apercu = TRUE)))
  remplacer_par_mise_a_jour(actuel, r, traduire("mise à jour des données", "data update"))
}

# Une étape retirée, les données refaites sans elle : DEMANDES$retirer_etape. La source est relue telle
# qu'elle est : changée depuis sa lecture (etat_fiche), le retrait prendrait aussi ses nouvelles
# données, et en ferait la source sans le dire. Il est alors refusé (une erreur de classe
# « source_modifiee », que la réponse dit, appli) ; son aperçu (q$apercu) montre ce qu'il ferait.
retirer_etape_demande <- function(q) {
  actuel <- projet(q$projet)
  n <- suppressWarnings(as.integer(unlist(q$n))[1])
  etapes <- actuel$etapes %||% list()
  if (is.na(n) || n < 1 || n > length(etapes)) stop(sprintf(traduire("Étape inconnue : %s", "Unknown step: %s"), premier_texte(q$n)), call. = FALSE)
  s <- actuel$source_donnees
  if (is.null(s)) stop(traduire("Ce projet n'a pas de fichier source : ses étapes ne se refont pas sans lui.", "This project has no source file: its steps can't be redone without it."), call. = FALSE)
  dossier <- dossier_projet(actuel)
  if (is.null(fichier_retrouve(s, dossier))) {
    stop(sprintf(traduire("Le fichier source est introuvable : %s", "The source file isn't found: %s"), premier_texte(s$chemin)), call. = FALSE)
  }
  if (!isTRUE(q$apercu) && isTRUE(etat_fiche(s, dossier)$modifie)) {
    stop(structure(class = c("source_modifiee", "error", "condition"), list(call = NULL, message = traduire(
      "Le fichier source a changé depuis qu'il a été lu : retirer l'étape y prendrait aussi ses nouvelles données. Mettez d'abord les données à jour, ou regardez l'aperçu du retrait.",
      "The source file changed since it was read: removing the step would also bring in its new data. Update the data first, or look at the removal's preview."))))
  }
  r <- mise_a_jour(actuel, etapes = etapes[-n])
  if (!is.null(r$choisir)) return(r)
  if (isTRUE(q$apercu)) return(c(r$rapport, list(apercu = TRUE)))
  remplacer_par_mise_a_jour(actuel, r, sprintf(traduire("retrait de l'étape %d", "removal of step %d"), n))
}

# ---------------------------------------------------------------- HTTP

# Tout ce qui est ouvert : pour une nouvelle fenêtre de l'application.
etat_complet <- function() {
  # (sans les projets que l'interface ne montre pas, ni leurs modèles : ouvrir, q$cache)
  vus <- Filter(function(p) !isTRUE(p$cache), etat$projets)
  list(projets = lapply(unname(vus), resume_projet),
       modeles = lapply(Filter(function(id) !is.null(vus[[etat$modeles[[id]]$projet]]), names(etat$modeles)), resultat_modele))
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
  # Les documents d'un projet (interface/documents.js) : du texte Markdown qui renvoie à ses
  # résultats, gardé dans le projet (p$documents : id, titre, langue, texte), donc enregistré avec
  # lui et dans ses copies de secours. L'annulation ne les touche pas (annuler_demande).
  document = function(q) {
    d <- Find(function(d) identical(d$id, q$id), projet(q$projet)$documents %||% list())
    if (is.null(d)) stop(traduire("Document introuvable", "Document not found"))
    d
  },
  # Crée (sans q$id) ou change un document : ses champs donnés (titre, langue, texte, bib : les
  # références BibTeX gardées dans le document, style : celui de ses citations) ; le projet n'est
  # marqué modifié que si l'un d'eux change.
  document_enregistrer = function(q) {
    p <- projet(q$projet)
    docs <- p$documents %||% list()
    id <- as.character(q$id %||% "")[1]
    k <- which(vapply(docs, function(d) identical(d$id, id), TRUE))
    if (!nzchar(id) || !length(k)) {
      numeros <- suppressWarnings(as.numeric(sub("^D", "", vapply(docs, function(d) as.character(d$id), ""))))
      id <- paste0("D", max(c(0, numeros), na.rm = TRUE) + 1)
      docs[[length(docs) + 1]] <- list(id = id, titre = "", langue = "fr", texte = "", cree = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
      k <- length(docs)
      p$modifie <- TRUE
    }
    for (champ in c("titre", "langue", "texte", "bib", "style")) {
      v <- q[[champ]]
      if (!is.null(v) && !identical(docs[[k]][[champ]], as.character(v)[1])) {
        docs[[k]][[champ]] <- as.character(v)[1]
        docs[[k]]$change <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
        p$modifie <- TRUE
      }
    }
    p$documents <- docs
    etat$projets[[p$id]] <- p
    list(id = id, projet = resume_projet(p))
  },
  document_supprimer = function(q) {
    p <- projet(q$projet)
    p$documents <- Filter(function(d) !identical(d$id, q$id), p$documents %||% list())
    p$modifie <- TRUE
    etat$projets[[p$id]] <- p
    list(projet = resume_projet(p))
  },
  # Les figures d'un projet (interface/figures.js) : des graphiques du projet assemblés en une
  # figure pour une revue (panneaux A, B, C…, taille en millimètres, texte en points), gardées
  # dans le projet (p$figures : id, titre, contenu, le texte JSON que l'interface seule relit),
  # donc enregistrées avec lui et dans ses copies de secours, pour les exporter de nouveau après
  # avoir réajusté les modèles. L'annulation ne les touche pas (annuler_demande).
  figure = function(q) {
    f <- Find(function(f) identical(f$id, q$id), projet(q$projet)$figures %||% list())
    if (is.null(f)) stop(traduire("Figure introuvable", "Figure not found"))
    f
  },
  # Crée (sans q$id) ou change une figure : son titre, son contenu ; le projet n'est marqué
  # modifié que si l'un d'eux change.
  figure_enregistrer = function(q) {
    p <- projet(q$projet)
    figures <- p$figures %||% list()
    id <- as.character(q$id %||% "")[1]
    k <- which(vapply(figures, function(f) identical(f$id, id), TRUE))
    if (!nzchar(id) || !length(k)) {
      numeros <- suppressWarnings(as.numeric(sub("^F", "", vapply(figures, function(f) as.character(f$id), ""))))
      id <- paste0("F", max(c(0, numeros), na.rm = TRUE) + 1)
      figures[[length(figures) + 1]] <- list(id = id, titre = "", contenu = "{}", cree = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
      k <- length(figures)
      p$modifie <- TRUE
    }
    for (champ in c("titre", "contenu")) {
      v <- q[[champ]]
      if (!is.null(v) && !identical(figures[[k]][[champ]], as.character(v)[1])) {
        if (champ == "contenu" && nchar(as.character(v)[1], "bytes") > 5e6) stop(traduire("Figure trop grande pour être gardée dans le projet", "Figure too big to be kept in the project"))
        figures[[k]][[champ]] <- as.character(v)[1]
        figures[[k]]$change <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
        p$modifie <- TRUE
      }
    }
    p$figures <- figures
    etat$projets[[p$id]] <- p
    list(id = id, projet = resume_projet(p))
  },
  figure_supprimer = function(q) {
    p <- projet(q$projet)
    p$figures <- Filter(function(f) !identical(f$id, q$id), p$figures %||% list())
    p$modifie <- TRUE
    etat$projets[[p$id]] <- p
    list(projet = resume_projet(p))
  },
  # Un fichier de bibliographie d'un document (BibTeX, CSL-JSON) ou un style CSL : son texte. Un
  # chemin relatif l'est au dossier du fichier du projet (comme Quarto le lit à côté du document).
  bibliographie_fichier = function(q) {
    p <- projet(q$projet)
    chemin <- as.character(q$chemin %||% "")[1]
    if (!grepl("\\.(bib|bibtex|json|csl)$", chemin, ignore.case = TRUE)) {
      stop(traduire("Pas un fichier de bibliographie (.bib, .json) ni un style (.csl) : ", "Not a bibliography file (.bib, .json) or a style (.csl): "), chemin)
    }
    if (!grepl("^([A-Za-z]:)?[/\\\\]", chemin)) {
      dossier <- if (length(p$chemin) && nzchar(p$chemin)) dirname(p$chemin) else NULL
      if (is.null(dossier)) stop(traduire("Chemin relatif, et le projet n'a pas encore de fichier : ", "A relative path, and the project has no file yet: "), chemin)
      chemin <- file.path(dossier, chemin)
    }
    if (!file.exists(chemin)) stop(traduire("Fichier introuvable : ", "File not found: "), chemin)
    taille <- file.info(chemin)$size
    if (taille > 50e6) stop(traduire("Fichier trop gros (plus de 50 Mo) : ", "File too big (over 50 MB): "), chemin)
    texte <- if (taille) readChar(chemin, taille, useBytes = TRUE) else ""
    Encoding(texte) <- "UTF-8"
    if (!validUTF8(texte)) texte <- iconv(texte, "latin1", "UTF-8")  # (un vieux .bib en Latin-1)
    list(chemin = normalizePath(chemin, winslash = "/"), texte = sub("^﻿", "", texte),
         date = format(file.info(chemin)$mtime, "%Y-%m-%dT%H:%M:%S"))
  },
  # Les modèles qu'un document cite (q$noms) dont le type ne s'exporte pas encore vers R, et
  # pourquoi : le dialogue de Compiler › Quarto le dit avant le document reproductible.
  export_r_possible = function(q) {
    p <- projet(q$projet)
    noms <- as.character(unlist(q$noms))
    sans <- Filter(Negate(is.null), lapply(Filter(function(d) isTRUE(as.character(d$name) %in% noms), p$models), function(d) {
      r <- raison_sans_export(d)
      if (!is.null(r)) list(nom = as.character(d$name), type = as.character(d$type), raison = r)
    }))
    list(sans_export = sans)
  },
  # Un document compilé pour Quarto, reproductible (document_reproductible.R) : l'export vers R de
  # chacun des modèles q$modeles (ceux de la séance que le document cite) écrit dans q$dossier
  # (celui du .qmd ; sans lui, un dossier de la session, que le navigateur télécharge), dans
  # « q$nom_R », et ce que le .qmd en écrit pour chacune des références q$references (du JSON,
  # en texte : une liste d'objets, ou null), dans la langue du document q$langue.
  document_reproductible = function(q) {
    p <- projet(q$projet)
    ids <- as.character(unlist(q$modeles))
    es <- lapply(ids, function(id) {
      e <- modele(id)
      if (!identical(e$projet, p$id)) stop(traduire("Les modèles doivent venir du même projet.", "The models must come from the same project."))
      # (une catégorie qu'il a vue a changé de nom ou disparu : son script ne le refait plus, il reste en dur)
      perime <- tryCatch({ modele(id, TRUE); NULL }, error = function(err) conditionMessage(err))
      list(definition = e$definition, modele = e$modele, erreur = perime)
    })
    dossier <- if (length(q$dossier) && nzchar(as.character(q$dossier)[1])) as.character(q$dossier)[1] else dirname(chemin_temporaire("document"))
    references <- jsonlite::fromJSON(as.character(q$references %||% "[]"), simplifyVector = FALSE)
    ecrire_document_reproductible(p, stats::setNames(es, ids), references, dossier, as.character(q$nom %||% "document")[1],
                                  if (identical(q$langue, "en")) "en" else "fr", as.character(q$version %||% ""))
  },
  # Copie de secours d'un projet pas encore enregistré, sans toucher à son état ; `lu` : l'état
  # de son fichier quand il l'a lu ou enregistré, que l'interface garde avec la copie
  sauvegarde_secours = function(q) {
    p <- projet(q$projet)
    if (!is.null(q$session)) p$session <- q$session
    # (les chemins relatifs de sa source restent ceux depuis le dossier du projet : etapes.R)
    enregistrer_projet(p, q$chemin, as.character(q$version %||% ""), dossier = dossier_projet(p))
    list(chemin = q$chemin, lu = p$fichier_lu)
  },
  # q$secours : une copie de secours (q$chemin), rouverte comme le projet qu'elle garde : son nom,
  # son fichier (q$origine : celui où il s'enregistre, ou d'où il vient ; aucun pour un tableau
  # collé ou des spectres importés jamais enregistrés), l'état de ce fichier quand il l'avait lu
  # (q$lu), des changements à enregistrer. Jamais la copie elle-même comme fichier du projet :
  # elle est effacée quand Nuée se ferme.
  # q$lecture_seule : une ancienne version d'un projet (interface/versions.js), lue d'un fichier
  # temporaire et ouverte sous le nom q$nom (« vergers — version du 12 nov. 2026, 14:32 ») ; sa
  # source (le fichier du projet), son oid et sa date, que l'interface montre et dont elle se sert
  # pour la restaurer. Enregistrer ne l'écrit jamais sur ce fichier temporaire ; enregistrée ailleurs,
  # c'est une copie, un projet comme un autre. q$cache : un projet que l'interface ne montre pas
  # (l'ancienne version comparée au projet d'aujourd'hui, le temps d'en réajuster les modèles), hors
  # de l'état de la séance que les autres fenêtres demandent (etat_complet).
  ouvrir = function(q) {
    r <- ouvrir_fichier(q$chemin, table = q$table, brut = isTRUE(q$brut), collecte = isTRUE(q$collecte), kobo = q$kobo)
    secours <- isTRUE(q$secours) || !is.null(q$origine)
    if (!(secours || !is.null(q$lecture_seule) || isTRUE(q$cache)) || !is.null(r$choisir) || !is.null(r$kobo)) return(r)
    p <- projet(r$id)
    if (secours) {
      p$nom <- q$nom %||% basename(q$origine %||% p$nom)
      p$chemin <- q$origine
      p$fichier_lu <- fichier_lu_repris(q$lu, q$origine)
      p$modifie <- TRUE
    }
    if (!is.null(q$lecture_seule)) {
      p$nom <- as.character(q$nom %||% p$nom)[1]
      texte <- function(x) as.character(x %||% "")[1]
      p$lecture_seule <- list(source = texte(q$lecture_seule$source), oid = texte(q$lecture_seule$oid), date = texte(q$lecture_seule$date))
    }
    if (isTRUE(q$cache)) p$cache <- TRUE
    etat$projets[[p$id]] <- p
    resume_projet(p)
  },
  # Un export de KoboToolbox ou d'ODK Central (kobo.R) : l'aperçu de son import avec les réglages
  # du dialogue (la table, les métadonnées gardées, les colonnes jointes des tables parentes, le
  # formulaire XLSForm et sa langue), puis l'import, un nouveau projet.
  kobo_apercu = function(q) apercu_collecte(q$chemin, q$table, q$garder, q$parents, q$formulaire, q$langue),
  kobo_importer = function(q) ouvrir_collecte(q$chemin, q$nom %||% basename(q$chemin), q),
  # Le projet d'exemple (données inventées) dans la langue de l'interface : exemple/orchards.json
  # en anglais, exemple/vergers.json en français (les mêmes pommes : exemple/creer_exemple.js) ;
  # ouvert comme un projet nouveau, que Enregistrer range où l'on veut.
  exemple = function(q) {
    r <- ouvrir_fichier(file.path(racine, "exemple", traduire("vergers.json", "orchards.json")))
    p <- projet(r$id)
    p$chemin <- NULL
    etat$projets[[p$id]] <- p
    resume_projet(p)
  },
  projet = function(q) resume_projet(projet(q$projet)),
  donnees = function(q) donnees_projet(projet(q$projet), q$colonnes),
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
    e <- modele(q$modele, TRUE)
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
  # combien d'observations cochées (sans q$exclues ni le jeu de test q$test) remplissent la condition
  # q$condition (condition.R), pour l'onglet Observations de Nouveau modèle ; une condition qui ne se
  # lit pas, ou qui ne donne pas une valeur logique par observation, est une erreur qui le dit
  condition_observations = function(q) {
    p <- projet(q$projet)
    cochees <- setdiff(numeros_observations(p), as.numeric(c(unlist(q$exclues), unlist(q$test))))
    if (!nzchar(texte_condition(q$condition))) return(list(n = length(cochees), total = length(cochees)))
    vraie <- valeurs_condition(p, q$condition)
    list(n = sum(vraie[match(as.character(cochees), rownames(p$donnees))], na.rm = TRUE), total = length(cochees))
  },
  importer_spectres = importer_spectres_demande,
  predire_fichier = predire_fichier_demande,
  exporter_prediction = exporter_prediction_demande,
  scores_en_variables = scores_demande,
  classe_selection = classe_demande,
  script_r = script_demande,
  methodes = methodes_demande,
  # Les conditions d'une régression vérifiées et dites en mots (verifications_modele.R), dans la
  # langue de la demande, calculées une fois par langue et gardées avec le modèle.
  verifications_modele = function(q) {
    e <- modele(q$modele)
    if (!inherits(e$modele, "modele_regression")) return(list(verifications = list()))
    langue <- getOption("nuee.langue") %||% "fr"
    if (is.null(e$verifications[[langue]])) etat$modeles[[q$modele]]$verifications[[langue]] <- verifications_regression(projet(e$projet), e$modele)
    list(verifications = etat$modeles[[q$modele]]$verifications[[langue]])
  },
  # Le texte des résultats d'un modèle (resultats_texte.R), dans la langue demandée.
  resultats_texte = function(q) {
    e <- modele(q$modele)
    autre <- est_autre_analyse(e$definition) && is.null(fonction_analyse("texte_resultats_", e$definition$type))
    texte_resultats(projet(e$projet), e$definition, e$modele, if (identical(q$langue, "en")) "en" else "fr",
                    chiffres = if (autre) as.character(resultat_modele(q$modele)$chiffres %||% "") else "")
  },
  # Les résultats clés d'un modèle de la séance (resultats_cles.R : resume_resultats), pour la vue
  # « Ce qui a changé » : demandés avant et après un réajustement sur des données mises à jour, ou pour
  # le modèle d'une ancienne version du projet ouverte comme un autre projet, puis renvoyés à
  # comparer_resultats. Chaque entrée : sa clé (la référence d'un document quand il y en a une : Q2,
  # coef.annee… ; alias, les autres qui donnent la même valeur), son libellé dans la langue de la
  # demande, son groupe, sa valeur, et s'il y en a son intervalle à 95 % (ic), sa valeur p, son effectif
  # (n), sa catégorie (classe).
  resultats_cles = function(q) {
    e <- modele(q$modele)
    list(modele = q$modele, entrees = resume_resultats(etat$projets[[e$projet]], e))
  },
  # Deux résumés de resultats_cles comparés entrée par entrée, par leur clé (resultats_cles.R :
  # comparer_resultats) : q$avant et q$apres, { entrees } tels que resultats_cles les rend ; q$langue,
  # celle des phrases (sans elle, celle de la demande). Rend chaque entrée comparée (avant, après, écart
  # et écart relatif, les drapeaux signe, seuil, ic, nouveau, disparu et categorie, son importance, sa
  # phrase), les phrases des changements qui comptent, le plus important d'abord, et leurs comptes (resume).
  comparer_resultats = function(q) {
    langue <- if (length(q$langue)) (if (identical(q$langue, "en")) "en" else "fr") else getOption("nuee.langue") %||% "fr"
    comparer_resultats(q$avant, q$apres, langue)
  },
  modeles_classe = modeles_classe_demande,
  appartenance = appartenance_demande,
  contributions = function(q) {
    e <- modele(q$modele)
    c(list(modele = q$modele),
      as.list(contributions(e$modele, as.numeric(q$observations), q$type %||% "DModX", q$composante %||% 1, isTRUE(q$ponderee %||% TRUE))))
  },
  statistiques_variables = function(q) statistiques_variables(projet(q$projet)),
  # une variable au fil du temps, par lignes (le secteur) et par panneaux (le site), en long (series.R) ;
  # avec q$tendances, la tendance de chaque ligne (Mann–Kendall, pente de Sen), p corrigée par
  # q$correction ("BH" par défaut, ou "holm") ; avec q$autocorrelation, la correction de Hamed et Rao ;
  # avec q$saisons ("mois", une variable qualitative ou "id:k"), le test de Kendall saisonnier. Des
  # options refusées (des saisons par mois sans dates...) n'empêchent pas le graphique :
  # tendances_erreur le dit, et l'interface garde le tableau et ses options pour les changer.
  # Un modèle par réponse (par_reponse.R) : le modèle q$modele ajusté à chaque réponse
  # q$reponses, leurs résultats réunis (q$correction : BH, holm ou bonferroni) ; avec q$garder,
  # chaque modèle rejoint aussi le projet, et ses résultats sont rendus (resultats).
  par_reponse = function(q) {
    e <- modele(q$modele, TRUE)
    r <- modeles_par_reponse(projet(e$projet), e$definition, as.numeric(unlist(q$reponses)), q$correction %||% "BH")
    garde <- if (isTRUE(q$garder)) lapply(Filter(function(x) is.null(x$erreur), r$modeles), function(x) ajuster_et_garder(projet(e$projet), x$definition, x$modele))
    list(modele = q$modele, tableau = r$tableau, resume = r$resume, effets = r$effets, correction = r$correction,
         resultats = if (length(garde)) I(garde), projet = if (length(garde)) resume_projet(projet(e$projet)),
         methodes = phrase_methodes_par_reponse(sum(r$resume$erreur == ""), r$correction, !identical(getOption("nuee.langue"), "en")))
  },
  # Les coordonnées du projet (spatial.R) : celles qui sont réglées, une proposition d'après le
  # nom des variables, les systèmes de la liste ; avec q$regler, les règle (q$x, q$y, q$systeme ;
  # sans q$x, les retire), annulable par Ctrl+Z.
  coordonnees = function(q) {
    p <- projet(q$projet)
    r <- NULL
    if (isTRUE(q$regler)) {
      r <- changer_projet(q, function(p) appliquer_etape("coordonnees", p, q), traduire("coordonnées", "coordinates"), "coordonnees")
      p <- projet(q$projet)
    }
    list(coordonnees = coordonnees_projet(p), proposition = proposer_coordonnees(p), systemes = liste_systemes(), projet = r)
  },
  # Les points de la carte (Graphiques › Carte) : les coordonnées de chaque observation, et leur
  # longitude et latitude WGS 84 pour le fond de carte.
  carte = function(q) {
    p <- projet(q$projet)
    pts <- points_projet(p)
    s <- systeme_coordonnees(pts$systeme)
    list(uid = pts$uid, nom = unname(p$noms_obs[as.character(pts$uid)]), x = pts$x, y = pts$y, lon = pts$lon, lat = pts$lat,
         coordonnees = pts$coordonnees, systeme = list(code = s$code, nom = s$nom, type = s$type))
  },
  # L'autocorrélation spatiale d'une variable (q$variable) ou des résidus d'une régression
  # (q$modele, q$residus) : spatial.R.
  autocorrelation_spatiale = function(q) {
    e <- if (length(q$modele)) modele(q$modele)
    p <- projet(if (!is.null(e)) e$projet else q$projet)
    autocorrelation_spatiale(p, e$modele, if (length(q$variable)) as.numeric(q$variable), q$residus %||% "standardises",
                             q$voisinage %||% list(), q$permutations %||% 999, q$classes %||% 10, par_lieu = isTRUE(q$par_lieu))
  },
  series = function(q) {
    p <- projet(q$projet)
    r <- series_variable(p, q$variable, q$temps, q$lignes, q$panneaux)
    if (!isTRUE(q$tendances)) return(r)
    tryCatch({
      s <- if (length(q$saisons) && nzchar(as.character(q$saisons))) ajouter_saisons(p, r, q$saisons) else r
      s$tendances <- tendances_series(s$points, q$correction %||% "BH", isTRUE(q$autocorrelation))
      s
    }, error = function(e) c(r, list(tendances_erreur = conditionMessage(e))))
  },
  # la raréfaction et l'accumulation des espèces q$especes (numéros de variables), par groupe de
  # q$groupe (une variable qualitative ou "id:k") s'il y en a un (rarefaction.R)
  rarefaction = function(q) rarefaction_projet(projet(q$projet), q$especes, q$groupe, q$taille, q$pas),
  # avec q$appariement (une variable ou "id:k") : les tests appariés de deux classes, t apparié par défaut
  tests_variables = function(q) tests_variables(projet(q$projet), as.numeric(q$classe), if (length(q$groupes)) as.character(unlist(q$groupes)),
                                               q$test %||% (if (length(q$appariement)) "t_apparie" else "welch"), q$correction %||% "BH",
                                               appariement = q$appariement),
  # avant/après avec témoin (comparaisons.R : baci) ; sans q$evenement, la série des écarts seule.
  # Avec un événement, `modeles` : l'effet des modèles mixtes BACI de ces réglages ajustés dans la
  # séance (avec ou sans CAR1), du plus ancien au plus récent (comparaisons.R : effet_baci)
  baci = function(q) {
    r <- baci(projet(q$projet), q$y, q$temps, q$unite, as.character(unlist(q$impact)), as.character(unlist(q$temoins)),
              evenement = q$evenement, fin = q$fin, test = q$test %||% "welch")
    if (is.null(r$periodes)) return(r)
    o <- list(y = r$y$uid, temps = r$temps$uid, unite = r$unite$uid, impact = r$impact, temoins = r$temoins, evenement = r$evenement, fin = r$fin)
    ids <- Filter(function(id) identical(etat$modeles[[id]]$projet, q$projet) && !is.null(etat$modeles[[id]]$definition$baci_origine) &&
                    memes_reglages_baci(etat$modeles[[id]]$definition$baci_origine, o, car1 = FALSE), names(etat$modeles))
    r$modeles <- unname(Filter(Negate(is.null), lapply(ids, function(id) {
      # (un modèle qui ne donnerait pas son effet n'empêche pas la vue de donner le test des écarts)
      e <- tryCatch(effet_baci(etat$modeles[[id]]$modele, etat$modeles[[id]]$definition), error = function(err) NULL)
      if (!is.null(e)) c(list(id = id), e)
    })))
    r
  },
  baci_modele = baci_modele_demande,
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
  # les seuils d'une courbe (regression.R : seuils_courbe), gardés avec le modèle ; q$categorie :
  # la catégorie d'une courbe par catégorie (sans elle, la première)
  seuils = function(q) {
    e <- modele(q$modele)
    if (!est_regression(e$definition)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    terme <- as.character(q$terme)[1]
    categorie <- if (length(q$categorie) && nzchar(as.character(q$categorie)[1])) as.character(q$categorie)[1]
    cle <- paste(c(terme, categorie), collapse = "\r")
    s <- e$modele$seuils[[cle]]
    if (is.null(s)) {
      s <- seuils_courbe(e$modele, terme, categorie)
      etat$modeles[[q$modele]]$modele$seuils[[cle]] <- s
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
  # les coefficients de plusieurs régressions côte à côte (coefficients_modeles.R), pour Comparer
  # les modèles : q$modeles, leurs identifiants dans l'ordre des colonnes ; q$erreur, "ic" ou "et" ;
  # exporter_coefficients : le même tableau en fichiers (formats.R : ecrire_tables)
  coefficients_modeles = function(q) coefficients_cote_a_cote(regressions_demande(q$modeles), q$erreur %||% "ic"),
  exporter_coefficients = function(q) {
    if (is.null(q$chemin)) q$chemin <- chemin_temporaire(paste0("coefficients.", q$format %||% "xlsx"))
    chemins <- ecrire_tables(tables_cote_a_cote(regressions_demande(q$modeles), q$erreur %||% "ic"), q$chemin, objet = "coefficients")
    list(chemin = chemins[1], chemins = I(chemins))
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
  # la puissance pour détecter une tendance (puissance.R), gardée avec le modèle (le texte des
  # Méthodes, le rapport et le script la reprennent alors) : q$terme, q$tendance, q$unite, q$alpha,
  # q$annees (de, à), q$unites, q$simulation (annees, unites, n) ; q$simuler : la simulation, faite.
  # Sans réglage, celle de la dernière fois (sa phrase dans la langue de la demande), ou celle des
  # réglages par défaut.
  puissance_tendance = function(q) {
    e <- modele(q$modele)
    if (!est_regression(e$definition)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    avant <- e$modele$puissance_tendance
    r <- if (!length(setdiff(names(q), "modele")) && !is.null(avant)) utils::modifyList(avant, list(phrase = phrase_puissance(avant)))
         else puissance_tendance(e$modele, e$definition, q, avant)
    etat$modeles[[q$modele]]$modele$puissance_tendance <- r
    choisis <- length(setdiff(names(q), "modele")) > 0
    if (choisis || is.null(e$definition$puissance_tendance)) garder_avec_definition(q$modele, "puissance_tendance", r[c("reglages", "simulation")], choisis)
    c(list(modele = q$modele), r)
  },
  # les moyennes ajustées d'une régression (moyennes.R), gardées avec le modèle (le texte des
  # Méthodes, le rapport et le script les reprennent alors) : q$facteur, q$dans, q$pente (des numéros
  # de variables), q$ajustement, q$echelle ; sans réglage, ceux de la dernière fois (ses phrases dans la
  # langue de la demande), ou ceux par défaut
  moyennes = function(q) {
    e <- modele(q$modele, TRUE)
    if (!est_regression(e$definition)) stop(traduire("Seulement pour une régression.", "Only for a regression."))
    reglages <- q[setdiff(names(q), "modele")]
    r <- moyennes_ajustees(projet(e$projet), e$modele, e$definition, if (length(reglages)) reglages else e$modele$moyennes$reglages %||% list())
    etat$modeles[[q$modele]]$modele$moyennes <- r
    if (length(reglages) || is.null(e$definition$moyennes)) garder_avec_definition(q$modele, "moyennes", r$reglages, length(reglages) > 0)
    c(list(modele = q$modele), r)
  },
  correlations_variables = function(q) correlations_variables(projet(q$projet), q$methode %||% "pearson", q$correction %||% "BH"),
  historique = function(q) historique_projet(projet(q$projet)),
  # données modifiées et variables ajoutées : le projet est à enregistrer ; chaque changement est
  # une étape du projet, qu'il rejoue quand ses données sont mises à jour (etapes.R)
  modifier_valeur = function(q) changer_projet(q, function(p) appliquer_etape("modifier_valeur", p, q),
    sprintf("modification de %s", projet(q$projet)$noms_var[[as.character(q$variable)]]), "modifier_valeur"),
  variable_calculee = function(q) changer_projet(q, function(p) appliquer_etape("variable_calculee", p, q), sprintf("variable %s", q$nom), "variable_calculee"),
  # les valeurs manquantes remplies par ACP itérative régularisée (variables.R : imputer_acp)
  imputer = function(q) {
    avant <- projet(q$projet)
    etiquette <- traduire("imputation", "imputation")
    p <- noter_etape(avant, appliquer_etape("imputer", avant, q), "imputer", q, etiquette)
    memoriser(avant, etiquette)
    info <- attr(p, "imputation")
    attr(p, "imputation") <- NULL
    p$modifie <- TRUE
    etat$projets[[p$id]] <- p
    reviser(p$id)
    c(resume_projet(p), list(imputation = info))
  },
  classes_valeurs = function(q) changer_projet(q, function(p) appliquer_etape("classes_valeurs", p, q), sprintf("variable %s", q$nom), "classes_valeurs"),
  # Édition › Ajouter des colonnes d'un tableau : le tableau et les clés proposées, puis l'ajout
  table_a_joindre = table_a_joindre_demande,
  ajouter_colonnes = ajouter_colonnes_demande,
  # Édition › Ajouter des valeurs de rasters (raster.R) : ce que sont les fichiers, puis l'ajout
  raster_infos = function(q) list(fichiers = I(infos_rasters(projet(q$projet), as.character(unlist(q$chemins))))),
  ajouter_raster = ajouter_raster_demande,
  # Édition › Résumer par groupe : un nouveau projet
  resumer_groupes = resumer_groupes_demande,
  # L'atelier des données (atelier.R), chaque changement annulé par un seul Ctrl+Z. Chercher et
  # remplacer : avec q$apercu, ce qui changerait (le nombre de cellules, ce que devient chaque
  # variable), sans rien changer ; sinon le remplacement, refusé quand aucune cellule ne change.
  remplacer = function(q) {
    f <- function(p) appliquer_etape("remplacer", p, q)
    if (isTRUE(q$apercu)) return(list(remplacement = attr(f(projet(q$projet)), "remplacement")))
    quoi <- if (isTRUE(q$vides)) traduire("cellules vides", "empty cells") else as.character(q$chercher %||% "")[1]
    changer_projet_dit(q, f, sprintf(traduire("remplacement de %s", "replacement of %s"), quoi), "remplacement", "remplacer")
  },
  # Les catégories d'une variable qualitative : renommées, fusionnées, rangées (q$noms, q$ordre)
  categories = function(q) {
    nom <- as.character(variable_du_projet(projet(q$projet), q$variable)$ids[1])
    changer_projet_dit(q, function(p) appliquer_etape("categories", p, q), sprintf(traduire("catégories de %s", "categories of %s"), nom), "categories", "categories")
  },
  # Un bloc de cellules collé dans la vue Données, à partir de la case choisie (q$observations,
  # q$variables : ses lignes et ses colonnes ; q$valeurs : ses textes, null pour une case laissée)
  coller_cellules = function(q) changer_projet_dit(q, function(p) appliquer_etape("coller_cellules", p, q),
                                                   traduire("cellules collées", "pasted cells"), "collage", "coller_cellules"),
  # L'unité et la description des variables (q$descriptions : variable, unite, description)
  decrire_variables = function(q) changer_projet(q, function(p) appliquer_etape("decrire_variables", p, q), traduire("unités et descriptions", "units and descriptions"),
                                                 "decrire_variables"),
  # Les rapports vivants (etapes.R). La source et les étapes du projet q$projet : `source` (le
  # fichier d'où viennent ses données : `chemin`, `relatif`, `table`, `lecteur` ; `trouve`, celui qui
  # serait lu, d'abord par son chemin relatif ; `existe` ; `modifie`, s'il a changé depuis qu'il a
  # été lu, sa taille, sa date ou son contenu ; `taille`, `date`, `sha256` et `lu` de quand il a été
  # lu, `actuel` : sa taille et sa date maintenant ; null sans source), `etapes` (n, etiquette, date,
  # demande, et les fichiers qu'elle lit, comme `source`), `conformes` (les données sont encore
  # celles que la source et les étapes donnent ; false : changées autrement, dans la console R ou par
  # une version d'avant la 1.1 ; null : on ne sait pas, un projet d'avant la 1.1) et sa `raison`.
  etapes = etapes_demande,
  # L'étape q$n retirée : la source relue, les autres étapes rejouées, les données remplacées comme
  # par mettre_a_jour (le même rapport, et `projet`), un seul Ctrl+Z ; refusé sans le fichier source.
  # Avec q$apercu, le rapport seul, sans rien changer (et `apercu`) ; sans lui, refusé quand le fichier
  # source a changé depuis sa lecture (sa réponse d'erreur porte `source_modifiee`) : mettre d'abord
  # les données à jour, ou regarder l'aperçu.
  retirer_etape = retirer_etape_demande,
  # Les données mises à jour : le fichier q$chemin (sa source par défaut ; q$table, sa feuille ou sa
  # table) relu comme la source, les étapes rejouées, le résultat rapporté au projet par les noms des
  # variables et les identifiants des observations. q$sans_nouvelles : les modèles (noms ou numéros)
  # qui ne prennent pas les nouvelles observations. Avec q$apercu, le rapport seul, sans rien
  # changer : `observations` (ajoutees, retirees, premieres_ajoutees, premieres_retirees, cellules
  # changées et cellules_par_variable, celles des observations communes ; par_position : sans colonne
  # qui les nomme, retrouvées par leur place ; repetees : des noms qui reviennent, retrouvés dans leur
  # ordre), `variables` (nouvelles, absentes, recalculees
  # d'après leur formule, categories : les nouvelles de chaque variable, types : celles qui ont changé
  # de type dans le fichier et l'ont repris), `identifiants` (nouveaux, absents), `etapes` (n,
  # demande, etiquette, statut "ok", "sautee" ou "erreur", raison, notes), `modeles`
  # (variables_absentes : ceux qui ne se réajustent plus et les variables qui leur manquent ;
  # observations_choisies : ceux qui n'avaient pas toutes les observations du projet, à qui demander
  # s'ils prennent les nouvelles ; sans_nouvelles : ceux qui ne les prendront pas ;
  # nouvelles_categories : ceux qui prennent le terme d'une catégorie nouvelle, mise avant la dernière
  # de sa variable ; references_changees : les régressions dont la catégorie de référence change quand
  # même, la dernière n'ayant plus de lignes, avant et après), `fichier`,
  # `notes` (la lecture du fichier). Sans q$apercu, la mise à jour faite, un seul Ctrl+Z (la source et
  # les étapes comprises), le fichier lu devenant la source : le même rapport et `projet`.
  # L'interface réajuste ensuite les modèles. Un fichier de plusieurs tableaux sans q$table : `choisir`.
  mettre_a_jour = mettre_a_jour_demande,
  annuler = annuler_demande,
  # Les spectres d'un modèle (variables X quantitatives, dans leur ordre), avant et après leur
  # prétraitement : q$quoi, ceux que la vue dessine ("bruts" ou "pretraites" ; "noms" pour aucun,
  # seulement leurs variables ; sans lui, les deux). Arrondis à 6 chiffres significatifs : la vue ne
  # fait que les dessiner (le survol en montre 4), et 2 000 spectres de 2 000 longueurs d'onde, les
  # deux à 15 chiffres, faisaient 150 Mo de JSON, et 14 s pour l'écrire.
  spectres = function(q) {
    e <- modele(q$modele)
    s <- spectres_modele(projet(e$projet), e$definition)
    quoi <- as.character(q$quoi %||% "")[1]
    dessin <- function(M) signif(unname(M), 6)
    list(modele = q$modele, noms = I(colnames(s$bruts)), variables = I(s$variables), uid = as.numeric(rownames(s$bruts)),
         bruts = if (quoi %in% c("", "bruts")) dessin(s$bruts), pretraites = if (quoi %in% c("", "pretraites")) dessin(s$pretraites))
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
    e <- modele(q$modele, TRUE)
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
  # Le projet tel qu'Enregistrer l'écrirait (un .nuee), en texte : joint à un rapport de problème
  # quand l'utilisateur le demande (app.js : rapportProbleme).
  projet_texte = function(q) {
    p <- projet(q$projet)
    f <- file.path(tempdir(), paste0(nouvel_id("rapport"), ".nuee"))
    on.exit(unlink(f))
    enregistrer_projet(p, f, as.character(q$version %||% ""), dossier = dossier_projet(p))
    list(nom = nom_fichier(p$nom), texte = paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
  },
  enregistrer = function(q) {
    p <- projet(q$projet)
    # (une ancienne version ouverte en lecture seule : jamais sur le fichier temporaire d'où elle a été lue)
    if (!is.null(p$lecture_seule) && !is.null(q$chemin) && !is.null(p$chemin) && identical(etat_fichier(q$chemin)$chemin, etat_fichier(p$chemin)$chemin)) {
      stop(traduire("Cette ancienne version est en lecture seule : Enregistrer sous en garde une copie ailleurs.",
                    "This old version is read-only: Save as keeps a copy elsewhere."))
    }
    if (!is.null(q$session)) p$session <- q$session
    chemin <- q$chemin %||% file.path(tempdir(), paste0(tools::file_path_sans_ext(p$nom), ".nuee"))
    # le fichier qu'on avait lu ou écrit, changé depuis : l'interface demande avant de l'écraser
    lu <- p$fichier_lu
    if (!is.null(q$chemin) && !isTRUE(q$ecraser) && !is.null(lu) && file.exists(chemin) &&
        identical(etat_fichier(chemin)$chemin, lu$chemin) && !meme_etat(etat_fichier(chemin), lu)) {
      stop(conflit_fichier(chemin))
    }
    # (sans q$chemin, un fichier à télécharger : les chemins relatifs restent ceux depuis le dossier du projet)
    enregistrer_projet(p, chemin, as.character(q$version %||% ""), dossier = if (!is.null(q$chemin)) dirname(chemin) else dossier_projet(p))
    if (!is.null(q$chemin)) {
      p <- chemins_relatifs(p, dirname(chemin))  # (comme le fichier les écrit : etapes.R)
      p$chemin_enregistrement <- chemin
      p$fichier_lu <- etat_fichier(chemin)
      p$lecture_seule <- NULL  # (une ancienne version enregistrée ailleurs : une copie, un projet comme un autre)
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
                             paquets = as.list(vapply(c("jsonlite", "httpuv", "openxlsx", "haven", "readxl", "foreign", "RSQLite", "rmarkdown", "knitr"), function(x)
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
      # (une étape à retirer d'une source changée depuis sa lecture : retirer_etape_demande)
      if (inherits(e, "source_modifiee")) return(repondre(400L, list(erreur = conditionMessage(e), source_modifiee = TRUE)))
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
