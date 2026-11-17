# Vérifie les calculs en arrière-plan (moteur/arriere_plan.R, moteur/calculateur.R), avec un vrai
# calculateur, lancé comme le serveur le lance, et les demandes telles que l'interface les envoie, par
# le même chemin que le serveur HTTP (appli$call : arriere_plan_demande, sinon traiter_demande) :
#   - les mêmes réponses, au caractère près, qu'en un seul R : l'exemple des pommes ouvert, une ACP et
#     une PLS ajustées, la PLS réajustée sans deux observations, ses permutations, refaite par son
#     numéro, un test des variables, sa note ; un tableau ouvert de son fichier, le fichier changé,
#     Mettre à jour les données, Annuler, Rétablir ; la seconde fois, le calculateur ne reçoit pas de
#     nouveau le projet qu'il a déjà ;
#   - pendant un long calcul sur un projet : une lecture du même projet répond aussitôt ; une demande
#     qui le changerait attend la fin du calcul, puis se fait ; un autre projet s'ouvre aussitôt ; les
#     numéros donnés pendant ce temps ne se croisent pas ;
#   - Arrêter : des permutations arrêtées entre deux tours, par le même calculateur ; du code qui ne
#     regarde jamais le fichier d'arrêt, arrêté en tuant le calculateur, le calcul qui attendait son
#     tour arrêté aussi, un autre calculateur prêt, le calcul suivant fait ; la séance telle qu'elle était ;
#   - un calculateur qui meurt pendant un calcul : la demande échoue, sans rien changer, un autre le
#     remplace ;
#   - CALCULS et LECTURES sont des demandes du moteur ; aucune LECTURE ne peut écrire dans la séance
#     (les fonctions que chacune peut atteindre).
#   Rscript moteur/tests/verifier_arriere_plan.R

options(warn = 1, stringsAsFactors = FALSE)
racine_nuee <- normalizePath(".", winslash = "/")
# Le serveur sans son écoute (comme verifier_etapes.R) : chaque expression de serveur.R jusqu'à
# l'interface HTTP, sa racine et ses arguments donnés ici.
local({
  assign("racine", racine_nuee, globalenv())
  for (e in parse(file.path(racine_nuee, "moteur", "serveur.R"), encoding = "UTF-8", keep.source = FALSE)) {
    texte <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
    if (startsWith(texte, "appli <- list(")) break
    if (startsWith(texte, "local(") && grepl("racine <<-", texte, fixed = TRUE)) next
    if (startsWith(texte, "args <- commandArgs")) {
      assign("args", c("8799", file.path(racine_nuee, "app", "interface")), globalenv())
      next
    }
    eval(e, globalenv())
  }
})
options(nuee.langue = "en")
# (en anglais : l'exemple des pommes a les noms de orchards.csv)
LANGUE <- "en"

echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
dossier <- file.path(dirname(tempdir()), basename(tempfile("nuee_arriere_plan")))
dir.create(dossier)

# Le port du serveur, où le calculateur regarde s'il est toujours là ; le fichier d'Arrêter de main.js.
port <- httpuv::randomPort()
ecoute <- httpuv::startServer("127.0.0.1", port, list(call = function(req) list(status = 404L, headers = list(), body = "")))
fichier_arret <- file.path(dossier, "arreter")
options(nuee.arret = fichier_arret)

attendre <- function(condition, secondes = 60) {
  fin <- proc.time()[[3]] + secondes
  while (!isTRUE(condition()) && proc.time()[[3]] < fin) later::run_now(0.02)
  isTRUE(condition())
}
corps_de <- function(q) charToRaw(enc2utf8(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA))))
# Une demande par le chemin du serveur HTTP (appli$call), qui rend aussitôt : `e$fait` quand sa
# réponse est là (`e$reponse`, `e$quand`).
envoyer <- function(nom, q = list()) {
  corps <- corps_de(q)
  arriere_plan_arret()
  unlink(fichier_arret)
  e <- new.env()
  e$fait <- FALSE
  p <- arriere_plan_demande(nom, corps, LANGUE, NULL)
  if (is.null(p)) {
    e$reponse <- traiter_demande(nom, corps, LANGUE)
    e$fait <- TRUE
    e$quand <- proc.time()[[3]]
  } else {
    promises::then(p, function(v) { e$reponse <- v; e$fait <- TRUE; e$quand <- proc.time()[[3]] })
  }
  e
}
recevoir <- function(e, secondes = 180) {
  if (!attendre(function() e$fait, secondes)) stop("pas de réponse après ", secondes, " s")
  e$reponse
}
lu <- function(r) jsonlite::fromJSON(r$body, simplifyVector = FALSE)
par_serveur <- function(nom, q = list()) recevoir(envoyer(nom, q))
ici <- function(nom, q = list()) traiter_demande(nom, corps_de(q), LANGUE)
taches <- 0
suivre_taches <- local({
  avant <- arriere_plan_envoyer
  function() {
    assign("arriere_plan_envoyer", function(it) { taches <<- taches + 1; avant(it) }, globalenv())
  }
})
suivre_taches()

cat("Le calculateur\n")
arriere_plan_lancer()
verifier("lancé, puis prêt", attendre(function() identical(ARRIERE_PLAN$etat, "pret"), 120))
if (!identical(ARRIERE_PLAN$etat, "pret")) {
  cat(readLines(file.path(ARRIERE_PLAN$dossier, "journal.txt"), warn = FALSE), sep = "\n")
  quit(status = 1)
}
premier <- ARRIERE_PLAN$k

# ---------------------------------------------------------------- les mêmes réponses qu'en un seul R

# La séance remise à neuf, comme au démarrage du serveur, avec un nouveau calculateur (les numéros
# recommencent : celui d'avant garderait des modèles de mêmes numéros).
neuve <- function() {
  for (n in c("projets", "modeles", "predictions", "annulations", "retablissements", "revisions", "paires", "copies", "fusions", "journal")) assign(n, list(), envir = etat)
  etat$compteur <- 0
  etat$n_fusions <- 0
  etat$n_journal <- 0
  arriere_plan_tuer()
  if (!attendre(function() identical(ARRIERE_PLAN$etat, "pret"), 120)) stop("pas de calculateur prêt")
}
csv_source <- file.path(dossier, "vergers.csv")
original <- readLines(file.path(racine_nuee, "exemple", "orchards.csv"), encoding = "UTF-8")
heure <- as.POSIXct("2026-01-05 10:00:00", tz = "UTC")
# Chaque demande et sa réponse (son corps JSON), faite par `faire` : par le serveur (une partie
# dans le calculateur), ou toute ici.
scenario <- function(faire) {
  reponses <- list()
  d <- function(nom, q = list()) {
    r <- faire(nom, q)
    reponses[[length(reponses) + 1]] <<- list(nom = nom, statut = r$status, corps = r$body)
    lu(r)
  }
  p <- d("exemple", list(nom = "pommes"))
  vars <- Filter(function(v) !isTRUE(v$qualitative), p$variables)
  noms <- vapply(vars, function(v) v$nom, "")
  uid <- function(n) vars[[match(n, noms)]]$uid
  x <- vapply(setdiff(noms, c("fruit", "taste_score")), uid, 0)
  acp <- d("ajuster", list(projet = p$id, type = "PCA", x = x, y = list(), A = 0, G = 7, echelle = "UV"))
  pls <- d("ajuster", list(projet = p$id, type = "PLS", x = x, y = uid("taste_score"), A = 0, G = 7, echelle = "UV"))
  obs <- unlist(p$observations$uid)
  d("reajuster", list(modele = pls$id, exclues = obs[c(3, 17)]))
  d("permutations", list(modele = pls$id, y = 1, n = 20))
  d("ajuster", list(projet = p$id, numero = 2))
  qual <- Filter(function(v) isTRUE(v$qualitative) && identical(v$nom, "farming"), p$variables)[[1]]
  d("tests_variables", list(projet = p$id, classe = qual$uid))
  d("note_modele", list(modele = acp$id, note = "La première ACP."))
  d("modele", list(modele = pls$id))
  d("projet", list(projet = p$id))
  # un tableau ouvert de son fichier, le fichier changé, Mettre à jour, Annuler, Rétablir
  writeLines(original, csv_source, useBytes = TRUE)
  Sys.setFileTime(csv_source, heure)
  t <- d("ouvrir", list(chemin = csv_source))
  change <- original
  change[5] <- sub(",[0-9.]+$", ",9.9", change[5])
  writeLines(change, csv_source, useBytes = TRUE)
  Sys.setFileTime(csv_source, heure + 3600)
  d("mettre_a_jour", list(projet = t$id))
  d("annuler", list(projet = t$id))
  d("retablir", list(projet = t$id))
  d("projet", list(projet = t$id))
  reponses
}
cat("Les mêmes réponses qu'en un seul R\n")
taches <- 0
par_le_serveur <- scenario(par_serveur)
donnees_serveur <- etat$projets
taches_premiere <- taches
neuve()
tout_ici <- scenario(ici)
differentes <- which(!vapply(seq_along(tout_ici), function(i) identical(par_le_serveur[[i]], tout_ici[[i]]), TRUE))
verifier(sprintf("%d demandes, %d faites par le calculateur, les mêmes réponses%s", length(tout_ici), taches_premiere,
                 if (length(differentes)) paste0(" — différentes : ", paste(vapply(tout_ici[differentes], function(x) x$nom, ""), collapse = ", ")) else ""),
         !length(differentes) && length(par_le_serveur) == length(tout_ici) && taches_premiere >= 7)
for (i in utils::head(differentes, 2)) {
  cat("    ", tout_ici[[i]]$nom, " (serveur) ", substr(par_le_serveur[[i]]$corps, 1, 400), "\n", sep = "")
  cat("    ", tout_ici[[i]]$nom, " (ici)     ", substr(tout_ici[[i]]$corps, 1, 400), "\n", sep = "")
}
verifier("les mêmes données à la fin", identical(lapply(donnees_serveur, function(p) p$donnees), lapply(etat$projets, function(p) p$donnees)))

# La séance de nouveau, faite par le serveur : ce qui va au calculateur la seconde fois.
neuve()
premier <- ARRIERE_PLAN$k
p <- lu(par_serveur("exemple", list(nom = "pommes")))
vars <- Filter(function(v) !isTRUE(v$qualitative), p$variables)
noms <- vapply(vars, function(v) v$nom, "")
x <- vapply(setdiff(noms, c("fruit", "taste_score")), function(n) vars[[match(n, noms)]]$uid, 0)
acp <- lu(par_serveur("ajuster", list(projet = p$id, type = "PCA", x = x, y = list(), A = 0, G = 7, echelle = "UV")))
pls <- lu(par_serveur("ajuster", list(projet = p$id, type = "PLS", x = x, y = vars[[match("taste_score", noms)]]$uid, A = 0, G = 7, echelle = "UV")))
envoye <- NULL
local({
  avant <- saveRDS
  assign("saveRDS", function(object, file, ...) { if (grepl("tache-", file, fixed = TRUE)) envoye <<- object; avant(object, file, ...) }, globalenv())
})
invisible(par_serveur("permutations", list(modele = pls$id, y = 1, n = 5)))
rm(saveRDS, envir = globalenv())
verifier("la seconde tâche du projet : ni le projet, ni ses modèles de nouveau", !is.null(envoye) && is.null(envoye$projet_objet) && !length(envoye$modeles))

# ---------------------------------------------------------------- pendant un long calcul

cat("Pendant un long calcul\n")
# (le calcul : du code R qui attend, que le calculateur fait comme un calcul)
CALCULS <- c(CALCULS, "console")
long <- envoyer("console", list(code = "Sys.sleep(4); 1", projet = p$id))
verifier("le calcul donné au calculateur", attendre(function() !is.null(ARRIERE_PLAN$en_cours), 10) && !long$fait)
lecture <- envoyer("projet", list(projet = p$id))
verifier("une lecture du même projet répond aussitôt, avant la fin du calcul", lecture$fait && !long$fait && identical(lu(lecture$reponse)$id, p$id))
note <- envoyer("note_modele", list(modele = acp$id, note = "Écrite pendant le calcul."))
verifier("une demande qui changerait le projet attend", !note$fait && !identical(etat$modeles[[acp$id]]$definition$note, "Écrite pendant le calcul."))
autre <- envoyer("exemple", list(nom = "alouettes_comptages"))
verifier("un autre projet s'ouvre aussitôt", autre$fait && !long$fait && !is.null(etat$projets[[lu(autre$reponse)$id]]))
invisible(recevoir(note))
verifier("la demande qui attendait faite après le calcul", long$fait && note$fait && note$quand >= long$quand && identical(long$reponse$status, 200L))
verifier("sa note gardée", identical(lu(par_serveur("modele", list(modele = acp$id)))$note, "Écrite pendant le calcul."))
ids <- c(names(etat$projets), names(etat$modeles))
verifier(sprintf("des numéros qui ne se croisent pas (%s)", paste(ids, collapse = " ")), !anyDuplicated(ids))

# ---------------------------------------------------------------- Arrêter

cat("Arrêter\n")
avant <- list(projet = etat$projets[[p$id]], modeles = etat$modeles)
perm <- envoyer("permutations", list(modele = pls$id, y = 1, n = 999))
invisible(attendre(function() !is.null(ARRIERE_PLAN$en_cours), 10))
invisible(attendre(function() FALSE, 1))
# (où en est le calcul : la demande progression, faite ici pendant qu'il se fait)
pr1 <- lu(envoyer("progression")$reponse)
invisible(attendre(function() FALSE, 0.8))
pr2 <- lu(envoyer("progression")$reponse)
f1 <- pr1$calcul$fraction %||% NA
f2 <- pr2$calcul$fraction %||% NA
etape <- pr2$calcul$etapes[[1]]
verifier(sprintf("où en est le calcul : %s, %.3f puis %.3f, %s %s/%s, depuis %s s", pr2$calcul$nom %||% "?", f1, f2, etape$quoi %||% "?", etape$fait %||% "?", etape$total %||% "?", pr2$calcul$secondes %||% "?"),
         identical(pr2$calcul$nom, "permutations") && is.finite(f1) && is.finite(f2) && f2 > f1 && f2 < 1 && identical(etape$quoi, "permutations") &&
           identical(etape$total, 999L) && pr2$calcul$secondes > pr1$calcul$secondes)
invisible(file.create(fichier_arret))
debut <- proc.time()[[3]]
r <- lu(recevoir(perm, 20))
verifier(sprintf("des permutations arrêtées entre deux tours (%.1f s), par le même calculateur", proc.time()[[3]] - debut),
         isTRUE(r$arrete) && ARRIERE_PLAN$k == premier && proc.time()[[3]] - debut < 5)
verifier("la séance telle qu'elle était", identical(etat$projets[[p$id]], avant$projet) && identical(etat$modeles, avant$modeles))
pr <- lu(envoyer("progression")$reponse)
verifier("plus de calcul en cours", is.null(pr$calcul) && !length(pr$attente))

bloque <- envoyer("console", list(code = "Sys.sleep(60); 1", projet = p$id))
invisible(attendre(function() !is.null(ARRIERE_PLAN$en_cours), 10))
suivant <- envoyer("permutations", list(modele = pls$id, y = 1, n = 5))
invisible(attendre(function() FALSE, 0.5))
pr <- lu(envoyer("progression")$reponse)
verifier(sprintf("du code qui ne compte pas ses tours : %s, sans fraction ; en attente : %s", pr$calcul$nom %||% "?", paste(unlist(pr$attente), collapse = ", ")),
         identical(pr$calcul$nom, "console") && is.null(pr$calcul$fraction) && identical(unlist(pr$attente), "permutations"))
invisible(file.create(fichier_arret))
debut <- proc.time()[[3]]
r <- lu(recevoir(bloque, 20))
duree <- proc.time()[[3]] - debut
verifier(sprintf("du code qui attend, arrêté en tuant le calculateur (%.1f s)", duree), isTRUE(r$arrete) && duree < ARRET_PATIENCE + 3 && ARRIERE_PLAN$k == premier + 1)
verifier("le calcul qui attendait son tour, arrêté aussi", suivant$fait && isTRUE(lu(suivant$reponse)$arrete))
verifier("un autre calculateur, prêt", attendre(function() identical(ARRIERE_PLAN$etat, "pret"), 120))
r <- lu(par_serveur("permutations", list(modele = pls$id, y = 1, n = 5)))
verifier("le calcul suivant fait par lui, le projet reçu de nouveau", length(r$permutations %||% r$lignes %||% r) > 0 && is.null(r$erreur) && !is.null(ARRIERE_PLAN$vu$projets[[p$id]]))
verifier("la séance telle qu'elle était", identical(etat$projets[[p$id]], avant$projet) && identical(etat$modeles, avant$modeles))

cat("Un calculateur qui meurt\n")
mort <- envoyer("console", list(code = "Sys.sleep(60); 1", projet = p$id))
invisible(attendre(function() !is.null(ARRIERE_PLAN$en_cours), 10))
invisible(attendre(function() FALSE, 0.5))
tuer_processus(ARRIERE_PLAN$pid)
r <- lu(recevoir(mort, 20))
verifier(sprintf("la demande échoue : %s", r$erreur %||% "(rien)"), grepl("doing this computation stopped", r$erreur %||% "", fixed = TRUE))
verifier("un autre calculateur, prêt", ARRIERE_PLAN$k == premier + 2 && attendre(function() identical(ARRIERE_PLAN$etat, "pret"), 120))
r <- lu(par_serveur("ajuster", list(projet = p$id, numero = 1)))
verifier("le calcul suivant fait", is.null(r$erreur) && !is.null(r$id))
CALCULS <- setdiff(CALCULS, "console")

# ---------------------------------------------------------------- les listes

cat("CALCULS et LECTURES\n")
verifier("des demandes du moteur", all(c(CALCULS, LECTURES) %in% names(DEMANDES)))
fonctions <- Filter(function(n) is.function(get(n, globalenv())), ls(globalenv(), all.names = TRUE))
appels <- new.env()
for (n in fonctions) appels[[n]] <- tryCatch(intersect(codetools::findGlobals(get(n, globalenv()), merge = FALSE)$functions, fonctions), error = function(e) character(0))
ecrit <- function(f) {
  texte <- paste(deparse(f, width.cutoff = 500L), collapse = "\n")
  any(vapply(c("projets", "modeles", "predictions", "annulations", "retablissements", "revisions", "copies", "fusions"),
             function(l) grepl(sprintf("etat\\$%s[^<\n]{0,80}<-", l), texte, perl = TRUE), TRUE)) || grepl("assign\\([^)]*envir = etat", texte)
}
ecrivent <- vapply(fonctions, function(n) ecrit(get(n, globalenv())), TRUE)
atteintes <- function(f) {
  vus <- character(0)
  a_voir <- intersect(codetools::findGlobals(f, merge = FALSE)$functions, fonctions)
  while (length(a_voir)) {
    x <- a_voir[1]
    a_voir <- a_voir[-1]
    if (x %in% vus) next
    vus <- c(vus, x)
    a_voir <- c(a_voir, setdiff(appels[[x]], vus))
  }
  vus
}
ecrivains <- Filter(function(nom) ecrit(DEMANDES[[nom]]) || any(ecrivent[atteintes(DEMANDES[[nom]])]), LECTURES)
verifier(sprintf("aucune lecture n'écrit dans la séance%s", if (length(ecrivains)) paste0(" : ", paste(ecrivains, collapse = ", ")) else ""), !length(ecrivains))

httpuv::stopServer(ecoute)
tuer_processus(ARRIERE_PLAN$pid)
unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
