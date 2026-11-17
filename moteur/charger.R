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
  # (les noms que les fichiers du moteur définissent, ou changent quand ils sont relus : fonction_analyse,
  # analyses.R, ne prend qu'eux pour les fonctions d'une analyse, pas une fonction du même nom écrite par
  # l'utilisateur dans l'environnement global, comme un definir_trim de son script qui charge le moteur)
  avant <- mget(ls(globalenv(), all.names = TRUE), envir = globalenv())
  for (f in sort(list.files(file.path(dossier, "R"), "\\.R$", full.names = TRUE))) {
    sys.source(f, envir = globalenv(), keep.source = FALSE)
  }
  noms <- getOption("nuee.noms_moteur")
  if (!is.environment(noms)) noms <- new.env(parent = emptyenv())
  for (n in ls(globalenv(), all.names = TRUE)) {
    if (!exists(n, envir = noms, inherits = FALSE) && (!n %in% names(avant) || !identical(avant[[n]], get(n, envir = globalenv())))) {
      assign(n, TRUE, envir = noms)
    }
  }
  options(nuee.noms_moteur = noms)
})
