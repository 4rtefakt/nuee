# Vérifie les deux façons de charger le moteur dans R : charger.R en français, comme avant la 1.0
# (les scripts écrits pour lui lisent ses mots), load.R en anglais (sauf si la langue est réglée
# avant) ; les noms anglais des fonctions du guide (R/alias.R), qui donnent les mêmes résultats ;
# comparer_references() dans les deux langues (ses colonnes et ses statuts).
#   Rscript moteur/tests/verifier_langue.R   (depuis la racine du dépôt)

echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}

# un R neuf qui lit `code` (un fichier : pas de guillemets à protéger), ce qu'il écrit
dans_r_neuf <- function(code) {
  f <- tempfile(fileext = ".R")
  writeLines(code, f, useBytes = TRUE)
  on.exit(unlink(f))
  trimws(system2(file.path(R.home("bin"), "Rscript"), shQuote(f), stdout = TRUE, stderr = FALSE))
}

cat("charger.R et load.R\n")
r <- dans_r_neuf(c('source("moteur/charger.R")', 'cat(is.null(getOption("nuee.langue")), traduire("fr", "en"), exists("import_table"), "\\n")'))
verifier(sprintf("charger.R : le français, comme avant la 1.0 (%s)", paste(r, collapse = " ")), identical(tail(r, 1), "TRUE fr TRUE"))
r <- dans_r_neuf(c('source("moteur/load.R")', 'cat(getOption("nuee.langue"), traduire("fr", "en"), exists("ajuster"), "\\n")'))
verifier(sprintf("load.R : l'anglais, les noms français restent (%s)", paste(r, collapse = " ")), identical(tail(r, 1), "en en TRUE"))
r <- dans_r_neuf(c('options(nuee.langue = "fr")', 'source("moteur/load.R")', 'cat(getOption("nuee.langue"), "\\n")'))
verifier("load.R garde une langue réglée avant", identical(tail(r, 1), "fr"))

source("moteur/charger.R", encoding = "UTF-8")

cat("les noms anglais du guide\n")
set.seed(3)
n <- 40
x <- matrix(stats::rnorm(n * 4), n, 4, dimnames = list(NULL, paste0("x", 1:4)))
f <- tempfile(fileext = ".csv")
utils::write.csv(data.frame(y = x %*% c(1, -0.5, 0.3, 0) + stats::rnorm(n, sd = 0.3), x), f, row.names = FALSE)
p <- import_table(f)
d <- define_model(p, "PLS", x = 2:5, y = 1, A = 2)
m <- fit_model(p, d)
m0 <- ajuster(importer_tableau(f), definir_modele(importer_tableau(f), "PLS", x = 2:5, y = 1, A = 2))
verifier("import_table, define_model, fit_model : les mêmes résultats que leurs noms français",
         isTRUE(all.equal(resume_composantes(m), resume_composantes(m0))) && isTRUE(all.equal(m$q2cum, m0$q2cum)))
verifier("define_model : son argument class est l'argument classe de definir_modele",
         identical(define_model(p, "PLS-DA", x = 2:5, class = 1, A = 1), definir_modele(p, "PLS-DA", x = 2:5, classe = 1, A = 1)))

cat("comparer_references() dans les deux langues\n")
dref <- d
dref$fitted <- TRUE
dref$validation <- "complet"  # (validé comme la comparaison le fait : la référence est alors retrouvée)
mref <- ajuster(p, dref)
cs <- resume_composantes(mref)
dref$reference <- list(components = lapply(seq_len(mref$A), function(a) list(`R2X(cum)` = cs$R2Xcum[a], `R2Y(cum)` = cs$R2Ycum[a], `Q2(cum)` = mref$q2cum[a])))
pref <- p
pref$models <- list(dref)
fr <- comparer_references(pref)
options(nuee.langue = "en")
en <- comparer_references(pref)
options(nuee.langue = NULL)
verifier(sprintf("en français : « %s » dans la colonne statut", fr$statut), "statut" %in% names(fr) && "modele" %in% names(fr) && identical(fr$statut, "identique"))
verifier(sprintf("en anglais : « %s » dans la colonne status", en$status), "status" %in% names(en) && "model" %in% names(en) && !"statut" %in% names(en) &&
           identical(en$status, c(identique = "identical", proche = "close", "différent" = "different")[[fr$statut]]))

unlink(f)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
