# Charge le moteur d'analyse : source("moteur/charger.R") depuis le dossier du projet.

local({
  dossier <- "moteur"
  for (i in rev(seq_len(sys.nframe()))) {  # le dossier de ce fichier, s'il est lu par source()
    f <- sys.frame(i)$ofile
    if (!is.null(f)) {
      dossier <- dirname(f)
      break
    }
  }
  # (le dossier du moteur, où l'export vers R relit les fonctions qu'il écrit dans un script : script_moteur.R)
  options(nuee.moteur = normalizePath(dossier, winslash = "/", mustWork = FALSE))
  for (f in sort(list.files(file.path(dossier, "R"), "\\.R$", full.names = TRUE))) {
    sys.source(f, envir = globalenv(), keep.source = FALSE)
  }
})
