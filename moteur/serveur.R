# Serveur local de l'application : sert l'interface et fait tous les calculs.
#   Rscript moteur/serveur.R <port> <dossier de l'interface>
# L'interface (fenêtre de l'application ou navigateur) envoie ses demandes en JSON à
# /api/<demande> ; chaque résultat affiché vient des fonctions du moteur.

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  racine <<- normalizePath(file.path(dirname(if (length(f)) f else "moteur/serveur.R"), ".."), winslash = "/")
})
source(file.path(racine, "moteur", "charger.R"))

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[1]) else 8765L
interface <- if (length(args) >= 2) args[2] else file.path(racine, "app", "interface")

etat <- new.env()
etat$projets <- list()
etat$modeles <- list()
etat$predictions <- list()  # dernière prédiction de chaque modèle (pour l'export Excel)
etat$compteur <- 0
etat$annulations <- list()  # par projet : les états précédents, pour « Annuler » (Ctrl+Z)

nouvel_id <- function(prefixe) {
  etat$compteur <- etat$compteur + 1
  paste0(prefixe, etat$compteur)
}

source(file.path(racine, "moteur", "console.R"), encoding = "UTF-8")  # journal et console R

# ---------------------------------------------------------------- projets

# Un fichier .usp passe par le convertisseur (export_project), qui écrit un .json.
convertir_usp <- function(chemin) {
  dossier <- file.path(tempdir(), nouvel_id("usp"))
  dir.create(dossier)
  exe <- Sys.getenv("CONVERTISSEUR_USP", file.path(racine, "outils", "export_project.exe"))
  sortie <- if (file.exists(exe)) {
    system2(exe, c(shQuote(chemin), "-o", shQuote(dossier)), stdout = TRUE, stderr = TRUE)
  } else {
    system2(Sys.getenv("PYTHON", "python"), c(shQuote(file.path(racine, "convertisseur", "export_project.py")),
                                               shQuote(chemin), "-o", shQuote(dossier)), stdout = TRUE, stderr = TRUE)
  }
  json <- list.files(dossier, "\\.json$", full.names = TRUE)
  if (!length(json)) stop("Conversion du fichier .usp impossible : ", paste(sortie, collapse = " "))
  json[1]
}

# Garde l'état actuel d'un projet avant de le changer, pour pouvoir revenir en arrière
# (20 changements au plus par projet).
memoriser <- function(p, etiquette) {
  pile <- c(list(list(etiquette = etiquette, projet = p)), etat$annulations[[p$id]] %||% list())
  etat$annulations[[p$id]] <- utils::head(pile, 20)
}

annuler_demande <- function(q) {
  pile <- etat$annulations[[q$projet]]
  if (!length(pile)) stop("Rien à annuler")
  actuel <- projet(q$projet)
  ancien <- pile[[1]]$projet
  etat$annulations[[q$projet]] <- pile[-1]
  ancien$chemin_enregistrement <- actuel$chemin_enregistrement  # un enregistrement fait depuis reste valable
  ancien$modifie <- TRUE
  etat$projets[[q$projet]] <- ancien
  list(projet = resume_projet(ancien), etiquette = pile[[1]]$etiquette)
}

ouvrir_fichier <- function(chemin, nom = basename(chemin)) {
  ext <- tolower(tools::file_ext(nom))
  p <- switch(ext,
              json = lire_projet(chemin),
              usp = lire_projet(convertir_usp(chemin)),
              csv = , txt = , xlsx = , xlsm = importer_tableau(chemin),
              stop("Format de fichier non reconnu : .", ext))
  p$id <- nouvel_id("P")
  p$nom <- nom
  p$chemin <- chemin
  etat$projets[[p$id]] <- p
  resume_projet(p)
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
  vars <- lapply(variables_avec_donnees(p), function(v) list(
    uid = v$uid, nom = as.character(v$ids[1]), qualitative = length(v$categories) > 0,
    categories = I(as.character(v$categories %||% character(0)))))
  json <- identical(tolower(tools::file_ext(p$chemin %||% "")), "json")
  pile <- etat$annulations[[p$id]]
  list(id = p$id, nom = p$nom, source = p$source %||% "", modifie = isTRUE(p$modifie), chemin = p$chemin %||% NULL,
       annulable = if (length(pile)) pile[[1]]$etiquette,
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

projet <- function(id) etat$projets[[id]] %||% stop("Projet inconnu : ", id)
modele <- function(id) etat$modeles[[id]] %||% stop("Modèle inconnu : ", id)

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
                                else q$croises)
  d$app <- TRUE
  d$exclues <- as.numeric(q$exclues %||% numeric(0))
  d$demande <- q
  d
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

# Réajuste un modèle sans certaines observations, ou avec un autre nombre de composantes.
reajuster_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  d <- e$definition
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
  if (!d$A) d$A <- max(1, autoajuster(p, d))
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

resultat_modele <- function(id) {
  e <- modele(id)
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
       type = paste0(if (isTRUE(m$opls)) "O" else "", if (m$plsda) "PLS-DA" else m$type), opls = isTRUE(m$opls), A = m$A, N = N, K = m$K,
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
       G = m$G, reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       cv_anova = s$cv_anova, classification = s$classification,
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
  out <- etat$predictions[[q$modele]] %||% stop("Aucune prédiction à exporter pour ce modèle")
  chemin <- q$chemin %||% file.path(tempdir(), paste0("predictions ", modele(q$modele)$definition$name %||% "", ".xlsx"))
  openxlsx::write.xlsx(out[c("nom", setdiff(names(out), c("nom", "uid")))], chemin, overwrite = TRUE)
  list(chemin = chemin)
}

# Les scores d'un modèle deviennent des variables du projet (modèles hiérarchiques).
scores_demande <- function(q) {
  e <- modele(q$modele)
  p <- projet(e$projet)
  memoriser(p, sprintf("scores de %s en variables", e$definition$name %||% q$modele))
  r <- scores_en_variables(p, e$modele)
  r$projet$modifie <- TRUE
  etat$projets[[p$id]] <- r$projet
  list(projet = resume_projet(r$projet), noms = I(r$noms), modeles = I(modeles_utilisant(r$projet, r$noms)))
}

changer_projet <- function(q, f, etiquette) {
  memoriser(projet(q$projet), etiquette)
  p <- f(projet(q$projet))
  p$modifie <- TRUE
  etat$projets[[p$id]] <- p
  resume_projet(p)
}

classe_demande <- function(q) {
  memoriser(projet(q$projet), sprintf("classe %s", q$etiquette))
  p <- ajouter_classe(projet(q$projet), q$variable, q$etiquette, as.numeric(q$observations))
  p$modifie <- TRUE
  etat$projets[[p$id]] <- p
  resume_projet(p)
}

script_demande <- function(q) {
  e <- modele(q$modele)
  nom <- gsub("[^[:alnum:]_ -]", "_", e$definition$name %||% "modele")
  chemin <- q$chemin %||% file.path(tempdir(), nouvel_id("script"), paste0(nom, ".R"))
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE)
  ecrire_script(projet(e$projet), e$definition, chemin, file.path(racine, "moteur"))
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

# Le k-ième identifiant de chaque observation (NA s'il manque).
identifiants_obs <- function(p, k) {
  vapply(p$observations, function(o) if (length(o$ids) >= k) as.character(o$ids[[k]] %||% NA) else NA_character_, "")
}

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

exporter_demande <- function(q) {
  e <- modele(q$modele)
  s <- e$modele$statistiques
  if (is.null(q$chemin)) q$chemin <- file.path(tempdir(), paste0(gsub("[^[:alnum:]_ -]", "_", e$definition$name %||% "modele"), ".xlsx"))
  wb <- openxlsx::createWorkbook()
  feuilles <- list(Composantes = s$composantes, Variables = s$variables, Observations = s$observations)
  if (!is.null(s$coefficients)) feuilles$Coefficients <- s$coefficients
  if (!is.null(s$variables_y)) feuilles$`Variables Y` <- s$variables_y
  for (n in names(feuilles)) {
    openxlsx::addWorksheet(wb, n)
    openxlsx::writeData(wb, n, feuilles[[n]], rowNames = n == "Observations")
  }
  openxlsx::saveWorkbook(wb, q$chemin, overwrite = TRUE)
  list(chemin = q$chemin)
}

# ---------------------------------------------------------------- HTTP

# Tout ce qui est ouvert : pour une nouvelle fenêtre de l'application.
etat_complet <- function() {
  list(projets = lapply(unname(etat$projets), resume_projet),
       modeles = lapply(names(etat$modeles), resultat_modele))
}

DEMANDES <- list(
  etat = function(q) etat_complet(),
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
  # Copie de secours d'un projet pas encore enregistré, sans toucher à son état
  sauvegarde_secours = function(q) {
    p <- projet(q$projet)
    if (!is.null(q$session)) p$session <- q$session
    enregistrer_projet(p, q$chemin)
    list(chemin = q$chemin)
  },
  ouvrir = function(q) {
    r <- ouvrir_fichier(q$chemin)
    if (is.null(q$origine)) return(r)
    p <- projet(r$id)
    p$nom <- q$nom %||% basename(q$origine)
    p$chemin <- q$origine
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
    if (!identical(e$modele$type, "PLS")) stop("Le test de permutation ne concerne que les modèles PLS")
    r <- permutations(e$modele, y = q$y %||% 1, n = max(1, min(q$n %||% 20, 999)))
    c(list(modele = q$modele), r)
  },
  valeurs = valeurs_demande,
  exporter = exporter_demande,
  predire_fichier = predire_fichier_demande,
  exporter_prediction = exporter_prediction_demande,
  scores_en_variables = scores_demande,
  classe_selection = classe_demande,
  script_r = script_demande,
  contributions = function(q) {
    e <- modele(q$modele)
    c(list(modele = q$modele),
      as.list(contributions(e$modele, as.numeric(q$observations), q$type %||% "DModX", q$composante %||% 1, isTRUE(q$ponderee %||% TRUE))))
  },
  statistiques_variables = function(q) statistiques_variables(projet(q$projet)),
  historique = function(q) historique_projet(projet(q$projet)),
  # données modifiées et variables ajoutées : le projet est à enregistrer
  modifier_valeur = function(q) changer_projet(q, function(p) modifier_valeur(p, q$observation, q$variable, q$valeur),
    sprintf("modification de %s", projet(q$projet)$noms_var[[as.character(q$variable)]])),
  variable_calculee = function(q) changer_projet(q, function(p) variable_calculee(p, q$nom, q$formule), sprintf("variable %s", q$nom)),
  classes_valeurs = function(q) changer_projet(q, function(p) classes_valeurs(p, q$nom, q$variable, bornes = q$bornes, n = q$n),
    sprintf("variable %s", q$nom)),
  annuler = annuler_demande,
  biplot = function(q) {
    e <- modele(q$modele)
    b <- correlations_biplot(e$modele)
    colonnes <- function(M) if (!is.null(M)) lapply(seq_len(ncol(M)), function(a) I(M[, a]))  # I() : un seul Y reste une liste
    list(modele = q$modele, t = colonnes(b$t), p = colonnes(b$p), c = colonnes(b$c),
         noms_x = I(e$modele$x$noms), noms_y = I(e$modele$y$noms %||% character(0)))
  },
  verifier = function(q) {
    p <- projet(q$projet)
    verifier_donnees(p, definition_interface(p, q))
  },
  # La session (modèles calculés, vues ouvertes, sélections) est décrite par l'interface, en
  # texte JSON qu'elle seule relit ; elle est enregistrée avec le projet.
  session = function(q) list(session = projet(q$projet)$session %||% NULL),
  enregistrer = function(q) {
    p <- projet(q$projet)
    if (!is.null(q$session)) p$session <- q$session
    chemin <- q$chemin %||% file.path(tempdir(), paste0(tools::file_path_sans_ext(p$nom), ".json"))
    enregistrer_projet(p, chemin)
    if (!is.null(q$chemin)) p$chemin_enregistrement <- chemin
    p$modifie <- FALSE
    etat$projets[[p$id]] <- p
    list(chemin = chemin)
  },
  journal = function(q) list(entrees = journal_depuis(as.numeric(q$depuis %||% 0)), dernier = etat$n_journal),
  console = function(q) evaluer_console(q$code, q$modele, q$projet),
  version = function(q) list(R = R.version.string,
                             paquets = vapply(c("jsonlite", "httpuv", "openxlsx"), function(x) as.character(utils::packageVersion(x)), "")))

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
    if (!startsWith(chemin, "/api/")) return(list(status = 404L, headers = list(), body = "introuvable"))
    nom <- sub("^/api/", "", chemin)
    corps <- req$rook.input$read()
    tryCatch({
      if (nom == "fichier") {  # téléchargement d'un fichier écrit par le serveur (navigateur)
        f <- normalizePath(utils::URLdecode(sub(".*chemin=([^&]*).*", "\\1", req$QUERY_STRING)), winslash = "/")
        if (!startsWith(f, normalizePath(tempdir(), winslash = "/"))) stop("Fichier non autorisé")
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
      if (is.null(f)) stop("Demande inconnue : ", nom)
      q <- if (length(corps)) jsonlite::fromJSON(rawToChar(corps), simplifyVector = TRUE) else list()
      r <- f(q)
      tryCatch(journaliser(nom, q, r), error = function(e) NULL)  # l'appel au moteur, pour la console R
      repondre(200L, r)
    }, error = function(e) {
      tryCatch(noter("erreur", sprintf("(%s) %s", nom, conditionMessage(e))), error = function(x) NULL)
      repondre(400L, list(erreur = conditionMessage(e)))
    })
  },
  staticPaths = list("/" = httpuv::staticPath(interface, indexhtml = TRUE),
                     "/api" = httpuv::excludeStaticPath())  # les demandes vont à call()
)

serveur <- httpuv::startServer("127.0.0.1", port, appli)
cat("PRET", port, "\n")
flush(stdout())
repeat httpuv::service(100)
