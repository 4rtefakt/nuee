# Le calculateur des calculs en arrière-plan (moteur/arriere_plan.R) : un second R, que le serveur lance
# à son démarrage, avec le même moteur, sans fenêtre à qui répondre. Il prend ses tâches dans son
# dossier (tache-<n>.rds, que le serveur y écrit), fait chacune avec traiter_demande, comme le serveur
# l'aurait faite (faire_tache), et y rend resultat-<n>.rds : la réponse et ce que la demande a changé
# dans sa séance. Le fichier « arreter » de son dossier l'arrête entre deux tours (arret.R).
#   Rscript moteur/calculateur.R <son dossier> <port du serveur>
# Il s'arrête de lui-même quand le serveur n'est plus là (son port ne prend plus de connexion).
local({
  a <- commandArgs(trailingOnly = TRUE)
  dossier_calculateur <<- a[1]
  port_serveur <<- as.integer(a[2])
})
journal_calculateur <- file(file.path(dossier_calculateur, "journal.txt"), open = "wt")
sink(journal_calculateur)
sink(journal_calculateur, type = "message")

tryCatch({
  if (!l10n_info()[["UTF-8"]]) {
    for (l in c("C.UTF-8", "C.utf8", "en_US.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) break
  }
  local({
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    racine <<- normalizePath(file.path(dirname(f), ".."), winslash = "/")
  })
  # le serveur sans son écoute, comme les vérifications du moteur le chargent (moteur/tests/verifier_etapes.R)
  for (e in parse(file.path(racine, "moteur", "serveur.R"), encoding = "UTF-8", keep.source = FALSE)) {
    texte <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
    if (startsWith(texte, "appli <- list(")) break
    if (startsWith(texte, "local(") && grepl("racine <<-", texte, fixed = TRUE)) next
    if (startsWith(texte, "args <- commandArgs")) {
      assign("args", c("0", file.path(racine, "app", "interface")), globalenv())
      next
    }
    eval(e, globalenv())
  }
  options(nuee.arret = file.path(dossier_calculateur, "arreter"))
  # (où en est la tâche, que le serveur lit : arret.R, noter_progres)
  options(nuee.progression = file.path(dossier_calculateur, "progression.rds"))
  # (les copies du fichier d'un projet sont au serveur, qui les range : le calculateur n'en efface aucune)
  ranger_copies <- function(id) invisible(NULL)
  # le port qui dit au serveur que le calculateur est là, même pendant un calcul (le serveur HTTP de
  # httpuv prend les connexions sans attendre R)
  port_calculateur <- httpuv::randomPort()
  serveur_calculateur <- httpuv::startServer("127.0.0.1", port_calculateur,
                                             list(call = function(req) list(status = 204L, headers = list(), body = "")))
  pret <- file.path(dossier_calculateur, "pret")
  writeLines(as.character(c(Sys.getpid(), port_calculateur)), paste0(pret, ".tmp"))
  file.rename(paste0(pret, ".tmp"), pret)
}, error = function(e) {
  writeLines(conditionMessage(e), file.path(dossier_calculateur, "echec"))
  quit(save = "no", status = 1)
})

verifie <- proc.time()[[3]]
repeat {
  taches <- list.files(dossier_calculateur, "^tache-[0-9]+\\.rds$", full.names = TRUE)
  if (!length(taches)) {
    if (proc.time()[[3]] - verifie > 2) {
      verifie <- proc.time()[[3]]
      if (!port_ouvert(port_serveur)) quit(save = "no")
    }
    Sys.sleep(0.02)
    next
  }
  t <- tryCatch(readRDS(taches[1]), error = function(e) NULL)
  unlink(taches[1])
  if (is.null(t)) next
  r <- tryCatch(faire_tache(t), error = function(e) list(n = t$n, reponse = repondre_erreur(t$nom, e), changements = NULL))
  sortie <- file.path(dossier_calculateur, sprintf("resultat-%d.rds", t$n))
  saveRDS(r, paste0(sortie, ".tmp"), compress = FALSE)
  file.rename(paste0(sortie, ".tmp"), sortie)
  verifie <- proc.time()[[3]]
}
