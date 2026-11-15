# Loads Nuée's analysis engine in R, in English: source("moteur/load.R") from the repository's
# folder, or source(".../resources/moteur/load.R") from the installed application. Its messages,
# statuses, column names and texts are in English, unless options(nuee.langue = "fr") is set
# before; its English names: import_table(), define_model(), fit_model() (R/alias.R).
# charger.R loads the same engine in French, the default before 1.0: the scripts written for it
# keep getting the same words.

local({
  dossier <- "moteur"
  for (i in rev(seq_len(sys.nframe()))) {  # this file's folder, when it is read by source()
    f <- sys.frame(i)$ofile
    if (!is.null(f)) {
      dossier <- dirname(f)
      break
    }
  }
  if (is.null(getOption("nuee.langue"))) options(nuee.langue = "en")
  source(file.path(dossier, "charger.R"))
})
