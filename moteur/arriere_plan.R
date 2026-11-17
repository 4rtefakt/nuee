# Les calculs en arrière-plan (1.2), lus par serveur.R.
#
# Le moteur de l'application de bureau est un R qui répond aux demandes de la fenêtre l'une après
# l'autre. Pendant un long calcul (un modèle ajusté, un test par permutations...), rien d'autre ne lui
# parvenait : la fenêtre ne pouvait ni montrer une autre vue, ni dessiner un autre modèle, et Arrêter
# attendait que le calcul regarde son fichier entre deux tours (arret.R), ce qu'un seul calcul en code
# compilé ne fait jamais.
#
# Désormais ce R, le serveur, fait faire les demandes longues (CALCULS) par un second R, le
# calculateur (moteur/calculateur.R), qu'il lance à son démarrage et qui a le même moteur. Pour
# chacune, il lui donne le projet qu'elle concerne, ses modèles et leurs prédictions (seulement ce
# qui en a changé depuis la tâche d'avant) ; le calculateur fait la demande avec traiter_demande, comme
# le serveur l'aurait faite, et rend sa réponse et ce qu'elle a changé dans sa séance, que le serveur
# reporte dans la sienne avant de répondre à la fenêtre. Pendant ce temps, le serveur répond au reste.
# Une demande qui changerait un projet en calcul (toute demande qui n'est pas une des LECTURES) attend
# la fin du calcul, dans l'ordre où elles sont venues ; les autres projets restent libres.
#
# Arrêter (le fichier que main.js écrit) arrête le calculateur entre deux tours, comme avant ; s'il n'a
# pas répondu une seconde et demie plus tard, il est tué et un autre prend sa place : la séance du
# serveur, elle, n'a rien perdu.
#
# Sans calculateur (il n'a pas démarré, NUEE_SANS_ARRIERE_PLAN=1, la version navigateur, les
# vérifications du moteur, qui chargent serveur.R sans écouter), tout se fait ici, comme avant.

# Les demandes faites par le calculateur : celles qui peuvent calculer longtemps, sur un seul projet.
CALCULS <- c(
  # les ajustements
  "ajuster", "reajuster", "modeles_classe", "par_reponse",
  # les calculs sur un modèle : ses prédictions, des permutations, des simulations, des modèles refaits...
  "predire", "predire_fichier", "permutations", "selection_aic", "rapports_vraisemblance", "puissance_tendance", "moyennes", "effets",
  "arbre_stabilite", "dependance_partielle", "kmeans_choix", "rda_tests", "rda_selection", "rda_partition",
  "ipls", "appartenance", "comparer_resultats", "verifications_modele", "autocorrelation_residus",
  # les statistiques des données
  "tests_variables", "test_deux_variables", "rarefaction", "autocorrelation_spatiale", "series", "baci",
  # Mettre à jour les données (le fichier relu, ses étapes refaites)
  "mettre_a_jour")

# Les demandes qui ne changent rien à la séance (ni un projet, ni un modèle, ni ses prédictions, ni ce
# qu'Annuler et Rétablir gardent) : elles se font aussitôt, même sur un projet en calcul, qu'elles
# voient tel qu'il était avant lui. (Le cache des paires des tests appariés, qu'elles peuvent remplir,
# ne compte pas.) moteur/tests/verifier_arriere_plan.R vérifie qu'aucune ne peut écrire dans la séance.
LECTURES <- c(
  "protocole", "version", "etat", "projets_modifies", "progression", "projet", "donnees", "valeurs", "historique", "session",
  "projet_texte", "sauvegarde_secours", "guide", "journal",
  "modele", "modeles", "verifier", "jeu_test", "condition_observations", "formule", "lire_formule",
  "empreinte_code", "code_approuve", "code_modeles",
  "greffons", "greffon_lire", "greffon_installer", "greffon_retirer", "greffon_projet",
  "hca", "permutations", "contributions", "appartenance", "spectres", "biplot", "ipls", "autocorrelation_residus",
  "rapports_vraisemblance", "coefficients_modeles", "methodes", "resultats_texte", "resultats_cles", "comparer_resultats",
  "statistiques_variables", "correlations_variables", "tests_variables", "test_deux_variables", "niveaux", "baci",
  "series", "rarefaction", "carte", "autocorrelation_spatiale", "etapes", "table_a_joindre", "raster_infos", "kobo_apercu",
  "exporter", "exporter_donnees", "exporter_prediction", "exporter_coefficients", "script_r",
  "document", "figure", "bibliographie_fichier", "export_r_possible", "document_reproductible")

# Les numéros réservés au calculateur pendant une tâche (nouvel_id) : le serveur, qui peut en donner
# pendant ce temps (un fichier ouvert), prend les suivants.
RESERVE_IDS <- 1000
# Le temps qu'un Arrêter laisse au calculateur pour s'arrêter entre deux tours, en secondes.
ARRET_PATIENCE <- 1.5
# Le temps qu'un calcul attend un calculateur qui démarre, en secondes.
ATTENTE_DEMARRAGE <- 5
# Ce qu'une entrée d'Annuler rendue par le calculateur met à la place du projet tel qu'il l'a reçu.
PROJET_ENVOYE <- structure(list(), class = "nuee_projet_envoye")

# ---------------------------------------------------------------- communs au serveur et au calculateur

# Le projet que concerne une demande : le sien (q$projet), ou celui de son modèle ; NULL sinon.
projet_concerne <- function(q) {
  if (!is.list(q)) return(NULL)
  p <- q[["projet"]]
  if (is.character(p) && length(p) == 1 && !is.null(etat$projets[[p]])) return(p)
  for (id in as.character(c(q[["modele"]], unlist(q[["modeles"]])))) {
    e <- etat$modeles[[id]]
    if (!is.null(e)) return(e$projet)
  }
  NULL
}

# Une pile d'Annuler ou de Rétablir telle que le calculateur la reçoit : ce que resume_projet en dit
# (l'étiquette de chaque entrée), sans les états du projet qu'elle garde.
talons <- function(pile) lapply(pile %||% list(), function(e) list(etiquette = e$etiquette, talon = TRUE))

# Un port de 127.0.0.1 qui prend la connexion : le R qui l'écoute est là (son serveur HTTP répond,
# même pendant qu'il calcule).
port_ouvert <- function(port) {
  if (!length(port) || is.na(port)) return(FALSE)
  con <- tryCatch(suppressWarnings(socketConnection("127.0.0.1", port, blocking = TRUE, open = "r+b", timeout = 1)), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
}

# Un processus arrêté sur-le-champ. (Sous Windows, tools::SIGKILL n'existe pas — NA, et pskill n'y fait
# rien — ; SIGTERM y est TerminateProcess.)
tuer_processus <- function(pid) {
  if (!length(pid) || is.na(pid)) return(invisible(FALSE))
  invisible(isTRUE(tryCatch(tools::pskill(pid, if (.Platform$OS.type == "windows") tools::SIGTERM else tools::SIGKILL), error = function(e) FALSE)))
}

# (le calculateur) une tâche faite : la séance prend ce que le serveur donne, la demande se fait, puis
# ce qu'elle a changé.
faire_tache <- function(t) {
  unlink(getOption("nuee.arret"))
  # (où en est cette tâche : arret.R, noter_progres ; arriere_plan_progression le lit)
  PROGRES$niveaux <- list()
  PROGRES$ecrit <- 0
  PROGRES$tache <- t$n
  unlink(getOption("nuee.progression"))
  P <- t$projet
  if (!is.null(t$projet_objet)) etat$projets[[P]] <- t$projet_objet
  for (k in t$projets_retires) etat$projets[[k]] <- NULL
  for (id in names(t$modeles)) etat$modeles[[id]] <- t$modeles[[id]]
  for (id in t$modeles_retires) etat$modeles[[id]] <- NULL
  for (id in names(t$predictions)) etat$predictions[[id]] <- t$predictions[[id]]
  for (id in t$predictions_retirees) etat$predictions[[id]] <- NULL
  etat$revisions[[P]] <- t$revision
  etat$annulations[[P]] <- t$annulations
  etat$retablissements[[P]] <- t$retablissements
  etat$compteur <- t$compteur
  etat$journal <- list()
  etat$n_journal <- 0
  APPROBATIONS_CODE$empreintes <- t$approbations
  depart <- as.list(etat, all.names = TRUE)
  r <- traiter_demande(t$nom, t$corps, t$langue, t$code)
  list(n = t$n, reponse = r, changements = changements_tache(depart, P))
}

# (le calculateur) ce qu'une tâche a changé dans la séance, depuis `depart` : les projets, les modèles
# et les prédictions changés ou retirés, la révision du projet P, les entrées qu'elle a mises en haut
# de sa pile d'Annuler (le projet tel qu'il était reçu y est PROJET_ENVOYE : le serveur l'a), si elle
# a vidé celle de Rétablir, les paires calculées, le compteur des numéros, son journal. `autres` : ce
# qu'elle a changé d'autre, que le serveur ne reprend pas (il le note).
changements_tache <- function(depart, P) {
  ecart <- function(liaison) {
    avant <- depart[[liaison]] %||% list()
    apres <- get0(liaison, envir = etat, inherits = FALSE) %||% list()
    cles <- union(names(avant), names(apres))
    ch <- cles[!vapply(cles, function(k) identical(avant[[k]], apres[[k]]), TRUE)]
    list(valeurs = apres[intersect(ch, names(apres))], retires = setdiff(ch, names(apres)))
  }
  pr <- ecart("projets")
  mo <- ecart("modeles")
  pd <- ecart("predictions")
  pa <- ecart("paires")
  pile <- etat$annulations[[P]] %||% list()
  k <- Position(function(e) isTRUE(e$talon), pile)
  nouvelles <- if (is.na(k)) pile else utils::head(pile, k - 1L)
  recu <- depart$projets[[P]]
  nouvelles <- lapply(nouvelles, function(e) { if (identical(e$projet, recu)) e$projet <- PROJET_ENVOYE; e })
  refaire <- etat$retablissements[[P]] %||% list()
  autres <- character(0)
  if (length(refaire) && !all(vapply(refaire, function(e) isTRUE(e$talon), TRUE))) autres <- c(autres, "retablissements")
  # (la pile d'Annuler, celle de Rétablir et la révision d'un autre projet)
  autres_projets <- function(x) {
    x <- (x %||% list())[setdiff(names(x %||% list()), P)]
    if (length(x)) x else list()
  }
  for (l in c("annulations", "retablissements", "revisions")) {
    if (!identical(autres_projets(depart[[l]]), autres_projets(get0(l, envir = etat, inherits = FALSE)))) autres <- c(autres, l)
  }
  connues <- c("projets", "modeles", "predictions", "paires", "annulations", "retablissements", "revisions", "compteur", "journal", "n_journal")
  for (n in setdiff(ls(etat, all.names = TRUE), connues)) if (!identical(depart[[n]], get(n, envir = etat))) autres <- c(autres, n)
  list(projets = pr$valeurs, projets_retires = pr$retires, modeles = mo$valeurs, modeles_retires = mo$retires,
       predictions = pd$valeurs, predictions_retirees = pd$retires, paires = pa$valeurs,
       revision = (etat$revisions[[P]] %||% 0) - (depart$revisions[[P]] %||% 0),
       annulations = nouvelles, retablissements_vides = length(depart$retablissements[[P]]) > 0 && !length(refaire),
       compteur = etat$compteur, journal = etat$journal, autres = unique(autres),
       # (les paquets chargés : le serveur charge ceux qu'il n'a pas, dont les méthodes des objets
       # rendus — predict d'une forêt de ranger —, comme s'il avait fait le calcul lui-même)
       espaces = loadedNamespaces())
}

# ---------------------------------------------------------------- le serveur

# Le calculateur, vu du serveur. etat : "eteint" (pas encore lancé), "demarre" (lancé, pas encore
# prêt), "pret", "sans" (pas de calculateur : tout se fait ici). file : les demandes qui attendent,
# dans l'ordre où elles sont venues (des calculs pour le calculateur, et des demandes qui attendent la
# fin d'un calcul sur leur projet) ; en_cours : la tâche qu'il fait. vu : ce qu'il a de la séance
# (les projets, les modèles et les prédictions qu'il a reçus ou rendus), pour ne lui donner que ce qui
# en a changé.
ARRIERE_PLAN <- new.env(parent = emptyenv())
ARRIERE_PLAN$etat <- "eteint"
ARRIERE_PLAN$file <- list()
ARRIERE_PLAN$en_cours <- NULL
ARRIERE_PLAN$n <- 0
ARRIERE_PLAN$k <- 0
ARRIERE_PLAN$programme <- FALSE
ARRIERE_PLAN$vu <- list(projets = list(), modeles = list(), predictions = list())

# Le calculateur lancé (au démarrage du serveur ; un nouveau après un calculateur tué) : le Rscript du
# R du serveur (sous Windows, celui de bin/x64, qui est R lui-même), avec ses options (main.js :
# NUEE_R_OPTIONS, --no-init-file pour une copie installée), dans un dossier à lui du dossier temporaire
# du serveur, où vont ses tâches et ses réponses.
arriere_plan_lancer <- function() {
  a <- ARRIERE_PLAN
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (identical(Sys.getenv("NUEE_SANS_ARRIERE_PLAN"), "1") || !file.exists(rscript)) {
    a$etat <- "sans"
    return(invisible(FALSE))
  }
  a$k <- a$k + 1
  a$dossier <- file.path(tempdir(), sprintf("calculateur%d", a$k))
  unlink(a$dossier, recursive = TRUE)
  dir.create(a$dossier, showWarnings = FALSE)
  options_r <- Filter(nzchar, strsplit(Sys.getenv("NUEE_R_OPTIONS"), " ", fixed = TRUE)[[1]])
  lance <- tryCatch({
    system2(rscript, c(options_r, shQuote(file.path(racine, "moteur", "calculateur.R")), shQuote(a$dossier), as.character(port)),
            wait = FALSE, stdout = FALSE, stderr = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (!lance) {
    a$etat <- "sans"
    return(invisible(FALSE))
  }
  a$etat <- "demarre"
  a$depuis <- proc.time()[[3]]
  a$pid <- NA_integer_
  a$port <- NA_integer_
  a$vu <- list(projets = list(), modeles = list(), predictions = list())
  arriere_plan_programmer()
  invisible(TRUE)
}

# Ce que le serveur fait d'une demande quand un calculateur est là : un calcul pour lui, ou une demande
# qui attend la fin d'un calcul sur son projet, rendus plus tard (une promesse, que httpuv attend) ;
# NULL : la demande se fait ici, tout de suite.
arriere_plan_demande <- function(nom, corps, langue, code) {
  a <- ARRIERE_PLAN
  if (!a$etat %in% c("demarre", "pret")) return(NULL)
  calcul <- nom %in% CALCULS
  if (!calcul && (nom %in% LECTURES || (!length(a$file) && is.null(a$en_cours)))) return(NULL)
  q <- tryCatch(lire_demande(nom, corps), error = function(e) NULL)
  P <- if (!is.null(q)) projet_concerne(q)
  if (is.null(P)) return(NULL)
  if (!calcul && !P %in% c(if (!is.null(a$en_cours)) a$en_cours$projet, vapply(a$file, function(x) x$projet, ""))) return(NULL)
  a$n <- a$n + 1
  it <- new.env(parent = emptyenv())
  it$n <- a$n
  it$nom <- nom
  it$corps <- corps
  it$langue <- langue
  it$code <- code
  it$projet <- P
  it$calcul <- calcul
  p <- promises::promise(function(resolve, reject) it$resolve <- resolve)
  a$file <- c(a$file, list(it))
  arriere_plan_programmer()
  p
}

arriere_plan_programmer <- function(delai = 0.02) {
  if (isTRUE(ARRIERE_PLAN$programme)) return(invisible())
  ARRIERE_PLAN$programme <- TRUE
  later::later(arriere_plan_suivre, delai)
}

# Ce que le serveur regarde, cinquante fois par seconde tant qu'il y a du travail : le calculateur prêt,
# un Arrêter, la réponse d'une tâche, puis ce qui peut se faire.
arriere_plan_suivre <- function() {
  a <- ARRIERE_PLAN
  a$programme <- FALSE
  tryCatch({
    if (identical(a$etat, "demarre")) arriere_plan_demarrage()
    arriere_plan_arret()
    if (!is.null(a$en_cours)) arriere_plan_reponse()
    arriere_plan_avancer()
  }, error = function(e) tryCatch(noter("erreur", sprintf("(%s) %s", traduire("arrière-plan", "background"), conditionMessage(e))), error = function(x) NULL))
  if (identical(a$etat, "demarre") || !is.null(a$en_cours) || length(a$file)) arriere_plan_programmer()
}

# Le calculateur prêt (son fichier « pret » : son numéro de processus et le port qui dit qu'il est là),
# ou qui ne le sera pas (une erreur à son démarrage, ou deux minutes sans rien) : tout se fait alors ici.
arriere_plan_demarrage <- function() {
  a <- ARRIERE_PLAN
  f <- file.path(a$dossier, "pret")
  if (file.exists(f)) {
    x <- suppressWarnings(as.integer(readLines(f, warn = FALSE)))
    a$pid <- x[1]
    a$port <- x[2]
    a$verifie <- proc.time()[[3]]
    a$etat <- "pret"
    return(invisible())
  }
  echec <- file.path(a$dossier, "echec")
  if (file.exists(echec) || proc.time()[[3]] - a$depuis > 120) {
    a$etat <- "sans"
    texte <- if (file.exists(echec)) paste(readLines(echec, warn = FALSE), collapse = " ") else traduire("pas prêt après deux minutes", "not ready after two minutes")
    tryCatch(noter("erreur", sprintf(traduire("Le R des calculs en arrière-plan n'a pas démarré (%s) : les calculs se font dans celui de la fenêtre.",
                                              "The R for background computations didn't start (%s): computations run in the window's R."), texte)),
             error = function(x) NULL)
  }
}

# Arrêter : les calculs qui n'ont pas commencé, arrêtés aussitôt ; celui du calculateur, entre deux
# tours (son fichier d'arrêt), puis tué s'il n'a pas répondu après ARRET_PATIENCE secondes. Le fichier
# de main.js est effacé quand plus rien ne calcule ici (serveur.R l'efface aussi à chaque demande).
arriere_plan_arret <- function() {
  a <- ARRIERE_PLAN
  f <- getOption("nuee.arret", "")
  if (length(f) && nzchar(f[1]) && file.exists(f[1])) {
    attente <- Filter(function(x) x$calcul, a$file)
    if (length(attente)) {
      a$file <- Filter(function(x) !x$calcul, a$file)
      for (it in attente) arriere_plan_repondre(it, reponse_arretee(it))
    }
    it <- a$en_cours
    if (is.null(it)) unlink(f[1])
    else if (is.null(it$arret)) {
      it$arret <- proc.time()[[3]]
      file.create(file.path(a$dossier, "arreter"))
    }
  }
  it <- a$en_cours
  if (!is.null(it) && !is.null(it$arret) && proc.time()[[3]] - it$arret > ARRET_PATIENCE) arriere_plan_tuer()
}

# Ce qui peut se faire maintenant, dans l'ordre de la file : une demande attend celles d'avant elle
# sur son projet ; un calcul attend un calculateur libre (sans calculateur, il se fait ici) ; une
# demande qui attendait la fin d'un calcul sur son projet se fait ici.
arriere_plan_avancer <- function() {
  a <- ARRIERE_PLAN
  repeat {
    occupes <- if (!is.null(a$en_cours)) a$en_cours$projet else character(0)
    fait <- FALSE
    for (i in seq_along(a$file)) {
      it <- a$file[[i]]
      if (it$projet %in% occupes) next
      # (un calculateur qui démarre se fait attendre ATTENTE_DEMARRAGE secondes au plus : celui qui
      # remplace un calculateur tué est prêt en une ou deux ; un premier démarrage plus lent laisse le
      # calcul se faire ici, comme avant)
      demarre <- identical(a$etat, "demarre") && proc.time()[[3]] - a$depuis <= ATTENTE_DEMARRAGE
      if (it$calcul && (demarre || (identical(a$etat, "pret") && !is.null(a$en_cours)))) {
        occupes <- c(occupes, it$projet)
        next
      }
      a$file[[i]] <- NULL
      # (une tâche que le calculateur n'a pas pu recevoir se fait ici : aucune demande ne reste sans réponse)
      envoyee <- it$calcul && identical(a$etat, "pret") && tryCatch({ arriere_plan_envoyer(it); TRUE }, error = function(e) FALSE)
      if (!envoyee) arriere_plan_repondre(it, traiter_demande(it$nom, it$corps, it$langue, it$code))
      fait <- TRUE
      break
    }
    if (!fait) break
  }
}

# Une tâche donnée au calculateur : la demande, et ce qui a changé de son projet (ses modèles, leurs
# prédictions) depuis ce qu'il en a ; les numéros RESERVE_IDS suivants, réservés à elle.
arriere_plan_envoyer <- function(it) {
  a <- ARRIERE_PLAN
  P <- it$projet
  vu <- a$vu
  ids <- names(Filter(function(e) identical(e$projet, P), etat$modeles))
  changes <- function(liaison, cles) {
    cles <- cles[!vapply(cles, function(k) identical(etat[[liaison]][[k]], vu[[liaison]][[k]]), TRUE)]
    etat[[liaison]][cles]
  }
  t <- list(n = it$n, nom = it$nom, corps = it$corps, langue = it$langue, code = it$code, projet = P,
            projet_objet = if (!identical(etat$projets[[P]], vu$projets[[P]])) etat$projets[[P]],
            projets_retires = setdiff(names(vu$projets), names(etat$projets)),
            modeles = changes("modeles", ids), modeles_retires = setdiff(names(vu$modeles), names(etat$modeles)),
            predictions = changes("predictions", intersect(ids, names(etat$predictions))),
            predictions_retirees = setdiff(names(vu$predictions), names(etat$predictions)),
            revision = etat$revisions[[P]], annulations = talons(etat$annulations[[P]]),
            retablissements = talons(etat$retablissements[[P]]), compteur = etat$compteur,
            approbations = APPROBATIONS_CODE$empreintes)
  vu$projets[[P]] <- etat$projets[[P]]
  for (k in t$projets_retires) vu$projets[[k]] <- NULL
  for (id in names(t$modeles)) vu$modeles[[id]] <- etat$modeles[[id]]
  for (id in t$modeles_retires) vu$modeles[[id]] <- NULL
  for (id in names(t$predictions)) vu$predictions[[id]] <- etat$predictions[[id]]
  for (id in t$predictions_retirees) vu$predictions[[id]] <- NULL
  a$vu <- vu
  it$depart <- vu
  it$compteur <- etat$compteur
  etat$compteur <- etat$compteur + RESERVE_IDS
  unlink(file.path(a$dossier, "arreter"))
  f <- file.path(a$dossier, sprintf("tache-%d.rds", it$n))
  saveRDS(t, paste0(f, ".tmp"), compress = FALSE)
  file.rename(paste0(f, ".tmp"), f)
  it$debut <- proc.time()[[3]]
  a$verifie <- it$debut
  a$en_cours <- it
}

# La réponse de la tâche en cours, quand le calculateur l'a rendue ; toutes les deux secondes sinon,
# s'il est toujours là. Parti sans l'avoir lue, la tâche va à un nouveau calculateur ; parti pendant
# qu'il la faisait (la mémoire lui a manqué ?), la demande échoue, sans rien changer.
arriere_plan_reponse <- function() {
  a <- ARRIERE_PLAN
  it <- a$en_cours
  f <- file.path(a$dossier, sprintf("resultat-%d.rds", it$n))
  if (file.exists(f)) {
    r <- tryCatch(readRDS(f), error = function(e) NULL)
    unlink(f)
    a$en_cours <- NULL
    if (is.null(r)) arriere_plan_repondre(it, reponse_calcul_perdu(it))
    else tryCatch(arriere_plan_reporter(it, r), error = function(e) arriere_plan_repondre(it, repondre_erreur(it$nom, e)))
    return(invisible())
  }
  maintenant <- proc.time()[[3]]
  if (maintenant - a$verifie < 2) return(invisible())
  a$verifie <- maintenant
  if (port_ouvert(a$port)) return(invisible())
  pas_lue <- file.exists(file.path(a$dossier, sprintf("tache-%d.rds", it$n)))
  a$en_cours <- NULL
  etat$compteur <- max(etat$compteur, it$compteur + RESERVE_IDS)
  ancien <- a$dossier
  arriere_plan_lancer()
  unlink(ancien, recursive = TRUE)
  if (pas_lue) a$file <- c(list(it), a$file)
  else arriere_plan_repondre(it, reponse_calcul_perdu(it))
}

# Ce que la tâche a changé, reporté dans la séance du serveur, puis sa réponse rendue à la fenêtre. Ce
# qu'elle a changé devait être ici tel qu'elle l'a trouvé : sinon (une demande ici l'a changé pendant
# ce temps), elle est refaite sur la séance d'aujourd'hui, par le calculateur deux fois, puis ici.
arriere_plan_reporter <- function(it, r) {
  a <- ARRIERE_PLAN
  ch <- r$changements
  if (is.null(ch)) return(arriere_plan_repondre(it, r$reponse))
  P <- it$projet
  base <- it$depart
  change_ici <- function(liaison, cles) any(vapply(cles, function(k) !identical(etat[[liaison]][[k]], base[[liaison]][[k]]), TRUE))
  conflit <- change_ici("projets", c(names(ch$projets), ch$projets_retires)) || change_ici("modeles", c(names(ch$modeles), ch$modeles_retires)) ||
    change_ici("predictions", c(names(ch$predictions), ch$predictions_retirees)) ||
    (ch$compteur > it$compteur + RESERVE_IDS && etat$compteur != it$compteur + RESERVE_IDS)
  if (conflit) {
    it$essais <- (it$essais %||% 0) + 1
    if (it$essais > 2) it$calcul <- FALSE
    it$arret <- NULL
    a$file <- c(list(it), a$file)
    return(invisible())
  }
  vu <- a$vu
  for (l in c("projets", "modeles", "predictions")) {
    for (k in names(ch[[l]])) etat[[l]][[k]] <- ch[[l]][[k]]
    for (k in ch[[paste0(l, if (l == "predictions") "_retirees" else "_retires")]]) etat[[l]][[k]] <- NULL
    for (k in c(names(ch[[l]]), ch[[paste0(l, if (l == "predictions") "_retirees" else "_retires")]])) vu[[l]][[k]] <- etat[[l]][[k]]
  }
  a$vu <- vu
  if (length(ch$revision) && ch$revision != 0) etat$revisions[[P]] <- (etat$revisions[[P]] %||% 0) + ch$revision
  if (length(ch$annulations)) {
    nouvelles <- lapply(ch$annulations, function(e) { if (inherits(e$projet, "nuee_projet_envoye")) e$projet <- base$projets[[P]]; e })
    etat$annulations[[P]] <- utils::head(c(nouvelles, etat$annulations[[P]] %||% list()), 20)
  }
  if (isTRUE(ch$retablissements_vides)) etat$retablissements[[P]] <- NULL
  if (length(ch$paires)) {
    garde <- etat$paires[setdiff(names(etat$paires), names(ch$paires))]
    etat$paires <- utils::tail(c(garde, ch$paires), 20)
  }
  if (identical(etat$compteur, it$compteur + RESERVE_IDS)) etat$compteur <- max(it$compteur, ch$compteur)
  for (e in ch$journal) {
    etat$n_journal <- etat$n_journal + 1
    e$n <- etat$n_journal
    etat$journal[[length(etat$journal) + 1]] <- e
  }
  if (length(etat$journal) > 2000) etat$journal <- utils::tail(etat$journal, 1500)
  for (ns in setdiff(ch$espaces, loadedNamespaces())) try(suppressPackageStartupMessages(loadNamespace(ns)), silent = TRUE)
  if (length(ch$autres)) {
    tryCatch(noter("erreur", sprintf("(%s, %s) %s", it$nom, traduire("arrière-plan", "background"), paste(ch$autres, collapse = ", "))), error = function(x) NULL)
  }
  arriere_plan_repondre(it, r$reponse)
}

# La demande progression : le calcul que le calculateur fait (sa demande, depuis combien de secondes,
# et, quand ses boucles comptent leurs tours, ce qui en est fait, d'après le fichier où il les écrit,
# arret.R : noter_progres), et les demandes qui attendent, dans l'ordre.
arriere_plan_progression <- function() {
  a <- ARRIERE_PLAN
  it <- a$en_cours
  calcul <- NULL
  if (!is.null(it)) {
    niveaux <- if (!is.null(a$dossier)) tryCatch(readRDS(file.path(a$dossier, "progression.rds")), error = function(e) NULL, warning = function(w) NULL)
    if (!identical(attr(niveaux, "tache"), it$n)) niveaux <- NULL
    calcul <- list(nom = it$nom, secondes = round(proc.time()[[3]] - it$debut, 1), fraction = fraction_progres(niveaux),
                   etapes = I(lapply(niveaux, function(x) list(quoi = x$quoi, fait = x$fait, total = x$total))))
  }
  list(calcul = calcul, attente = I(vapply(a$file, function(x) x$nom, "")))
}

arriere_plan_repondre <- function(it, reponse) {
  it$reponse <- reponse
  if (is.function(it$resolve)) it$resolve(reponse)
  invisible()
}

# Le calculateur tué (un Arrêter qu'il n'a pas pris entre deux tours) : sa tâche rend « calcul arrêté »,
# rien n'a changé ici ; un autre calculateur démarre, à qui la séance sera donnée de nouveau.
arriere_plan_tuer <- function() {
  a <- ARRIERE_PLAN
  tuer_processus(a$pid)
  it <- a$en_cours
  a$en_cours <- NULL
  if (!is.null(it)) {
    etat$compteur <- max(etat$compteur, it$compteur + RESERVE_IDS)
    arriere_plan_repondre(it, reponse_arretee(it))
  }
  ancien <- a$dossier
  arriere_plan_lancer()
  unlink(ancien, recursive = TRUE)
}

reponse_arretee <- function(it) {
  options(nuee.langue = if (identical(it$langue, "en")) "en" else "fr")
  tryCatch(noter("erreur", sprintf("(%s) %s", it$nom, traduire("calcul arrêté", "computation stopped"))), error = function(x) NULL)
  repondre(400L, list(erreur = traduire("Calcul arrêté.", "Computation stopped."), arrete = TRUE))
}

reponse_calcul_perdu <- function(it) {
  options(nuee.langue = if (identical(it$langue, "en")) "en" else "fr")
  message <- traduire("Le R qui faisait ce calcul s'est arrêté (la mémoire lui a peut-être manqué) : rien n'a changé.",
                      "The R doing this computation stopped (it may have run out of memory): nothing changed.")
  tryCatch(noter("erreur", sprintf("(%s) %s", it$nom, message)), error = function(x) NULL)
  repondre(400L, list(erreur = message))
}
