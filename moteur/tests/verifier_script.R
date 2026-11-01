# Vérifie le script R autonome (moteur/R/script.R) : pour des modèles du projet d'exemple et
# des cas particuliers (observations exclues, transformation, termes croisés, spectres, échelle
# enregistrée), le script est écrit, lancé dans une session R neuve (Rscript --vanilla, sans
# le moteur) depuis un autre dossier, et ses résultats sont comparés à ceux du moteur.
#   Rscript moteur/tests/verifier_script.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
dossier <- tempfile("script")
dir.create(dossier)
rscript <- file.path(R.home("bin"), "Rscript")
lanceur <- file.path(dossier, "lancer.R")
writeLines(c("a <- commandArgs(TRUE)",
             "source(a[1], encoding = \"UTF-8\")",
             "saveRDS(list(T = m$T, composantes = m$statistiques$composantes, vip = m$statistiques$variables$VIP,",
             "             test = m$test$statistiques[c(\"RMSEP\", \"biais\", \"SEP\", \"Q2ext\")]), a[2])"), lanceur)

# Écrit le script de `d`, le lance dans une session neuve et compare ses résultats.
essayer <- function(nom, p, d, ...) {
  m <- ajuster(p, d)
  dir.create(ici <- file.path(dossier, nom))
  r <- ecrire_script(p, d, file.path(ici, paste0(nom, ".R")), "moteur", m = m, version = "essai", ...)
  sortie <- file.path(dossier, paste0(nom, ".rds"))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(file.path(ici, paste0(nom, ".R"))), shQuote(sortie)),
                                       stdout = TRUE, stderr = TRUE))
  if (!file.exists(sortie)) {
    verifier(sprintf("%s : le script tourne (%s)", nom, paste(utils::tail(messages, 3), collapse = " / ")), FALSE)
    return(invisible(r))
  }
  s <- readRDS(sortie)
  meme <- function(a, b) (is.null(a) && is.null(b)) || isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = 1e-10))
  verifier(sprintf("%s : mêmes R2, Q2, scores (signes compris) et VIP%s que le moteur", nom, if (is.null(m$test)) "" else ", et jeu de test"),
           is.null(r$ecart) && meme(s$T, m$T) && meme(s$composantes, m$statistiques$composantes) && meme(s$vip, m$statistiques$variables$VIP) &&
           meme(s$test, if (!is.null(m$test)) m$test$statistiques[c("RMSEP", "biais", "SEP", "Q2ext")]))
  invisible(r)
}

p <- lire_projet("exemple/vergers.json")
cat("Modèles du projet d'exemple\n")
for (i in seq_along(p$models)) {
  d <- p$models[[i]]
  r <- essayer(d$name, p, d, donnees = if (i %% 2) "script" else "csv")
}
texte <- readLines(file.path(dossier, p$models[[1]]$name, paste0(p$models[[1]]$name, ".R")), encoding = "UTF-8")
verifier("le script ne charge rien de Nuée", !any(grepl("charger\\.R|source\\(", texte)))
verifier("les fonctions du moteur y sont avec leurs commentaires", any(grepl("^ajuster <- function", texte)) && any(grepl("^# -- nipals\\.R", texte)))

cat("Cas particuliers\n")
uid <- function(nom) numeros(p, nom)
quant <- Filter(function(v) !length(v$categories), p$variables)
x <- vapply(quant[1:6], function(v) as.numeric(v$uid), 0)
d <- definir_modele(p, "PLS", x = x[-1], y = x[1], observations = setdiff(numeros_observations(p), numeros_observations(p)[c(3, 50)]),
                    echelle = stats::setNames(c("UV", "Par"), c("defaut", as.character(x[2]))),
                    transformations = stats::setNames("log10", as.character(x[3])), croises = list(x[2:3]), A = 2)
r <- essayer("exclusions", p, d, renv = TRUE, rdata = TRUE, rproj = TRUE)
verifier("definir_modele() quand il redonne le modèle", any(grepl("definir_modele\\(", readLines(r$script, encoding = "UTF-8"))))
verifier("fichiers à côté : .RData, renv.lock, .Rproj", all(file.exists(r$chemins)) && length(r$chemins) == 4)
e <- new.env(); load(r$chemins[2], envir = e)
verifier(".RData : données, projet, définition et modèle", all(c("donnees", "p", "d", "m") %in% ls(e)) && e$m$A == 2)
lock <- jsonlite::fromJSON(r$chemins[3])
verifier("renv.lock : version de R", identical(lock$R$Version, paste(R.version$major, R.version$minor, sep = ".")))

d2 <- d
d2$x[[1]]$scaling <- list(centre = 1.5, weight = 0.25)  # échelle enregistrée (non reconnue)
d2$observations <- rev(d2$observations)                 # observations dans un autre ordre
r <- essayer("termes", p, d2, donnees = "csv")
verifier("sinon, les termes un par un", !any(grepl("definir_modele\\(", readLines(r$script, encoding = "UTF-8"))))

d <- definir_modele(p, "OPLS-DA", x = x, classe = uid("variete"), A = 4)
essayer("opls_4_classes", p, d, donnees = "csv")
d <- definir_modele(p, "PLS", x = x[-1], y = x[1], A = 2, test = numeros_observations(p)[seq(5, 120, by = 5)])
r <- essayer("jeu_de_test", p, d)
verifier("jeu de test : definir_modele(..., test = test)", any(grepl("test = test,", readLines(r$script, encoding = "UTF-8"))))
d <- modeles_classe(p, definir_modele(p, "PCA", x = x, A = 2), uid("variete"))[[2]]
essayer("modele_de_classe", p, d)

set.seed(3)
onde <- seq(1000, 1600, length.out = 40)
df <- data.frame(nom = sprintf("E%02d", 1:30), y = runif(30), t(vapply(1:30, function(i) sin(onde / 100 + i / 10) + rnorm(40, 0, 0.01), numeric(40))), check.names = FALSE)
names(df)[-(1:2)] <- sprintf("%.0f nm", onde)
q <- projet_depuis_tableau(df, "spectres")
d <- definir_modele(q, "PLS", x = numeros(q, names(df)[-(1:2)]), y = numeros(q, "y"), A = 2,
                    spectres = list(list(type = "snv"), list(type = "sg", derivee = 1, fenetre = 7, degre = 2)))
essayer("spectres", q, d)
d <- definir_modele(q, "PLS", x = numeros(q, names(df)[-c(1:2, 15:22)]), y = numeros(q, "y"), A = 2,  # une zone retirée
                    spectres = list(list(type = "ligne_base"), list(type = "detrend", degre = 2), list(type = "sg", derivee = 1, fenetre = 5, degre = 2)))
essayer("spectres_zones", q, d, donnees = "csv")

cat("Toute la séance dans un script\n")
dir.create(ici <- file.path(dossier, "seance"))
ms <- lapply(p$models, function(d) ajuster(p, d))
r <- ecrire_script_seance(p, p$models, ms, file.path(ici, "seance.R"), "moteur", version = "essai", donnees = "csv", rdata = TRUE)
noms <- vapply(p$models, function(d) d$name, "")
lanceur_seance <- file.path(dossier, "lancer_seance.R")
writeLines(c("a <- commandArgs(TRUE)", "source(a[1], encoding = \"UTF-8\")",
             "saveRDS(lapply(strsplit(a[3], \",\")[[1]], function(n) list(T = get(n)$T, composantes = get(n)$statistiques$composantes)), a[2])"),
           lanceur_seance)
sortie <- file.path(dossier, "seance.rds")
messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur_seance), shQuote(r$script), shQuote(sortie), paste(noms, collapse = ",")),
                                     stdout = TRUE, stderr = TRUE))
s <- if (file.exists(sortie)) readRDS(sortie)
meme <- function(a, b) isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), tolerance = 1e-10))
verifier(sprintf("%d modèles, chacun sous son nom : mêmes R2, Q2 et scores (signes compris) que le moteur", length(noms)),
         is.null(r$ecart) && length(s) == length(noms) &&
           all(vapply(seq_along(noms), function(k) meme(s[[k]]$T, ms[[k]]$T) && meme(s[[k]]$composantes, ms[[k]]$statistiques$composantes), FALSE)))
texte <- readLines(r$script, encoding = "UTF-8")
verifier("le code du moteur n'y est qu'une fois", sum(grepl("^ajuster <- function", texte)) == 1 && sum(grepl("^# 1\\. Vérifications", texte)) == 1)
e <- new.env(); load(r$chemins[3], envir = e)
verifier(".RData : données, projet et chaque modèle", all(c("donnees", "p", noms) %in% ls(e)))

cat("Rapports R Markdown et Quarto\n")
for (i in seq_along(p$models)) {
  d <- p$models[[i]]
  ext <- if (i %% 2) "Rmd" else "qmd"
  dir.create(ici <- file.path(dossier, paste0("rapport", i)))
  r <- ecrire_script(p, d, file.path(ici, paste0(d$name, ".", ext)), "moteur", version = "essai", langue = if (i %% 2) "fr" else "en",
                     donnees = if (i > 2) "csv" else "script")
  texte <- readLines(r$script, encoding = "UTF-8")
  verifier(sprintf("%s.%s : en-tête, méthodes, références", d$name, ext), texte[1] == "---" && any(grepl("^## (Méthodes|Methods)", texte)) &&
           any(grepl("^- .*Wold|^- .*Barker|^- .*Trygg", texte)) && is.null(r$ecart))
  if (requireNamespace("knitr", quietly = TRUE)) {  # compilé avec knitr dans une session neuve (sans pandoc : en Markdown)
    compiler <- file.path(dossier, "compiler.R")
    writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "a <- commandArgs(TRUE)", "setwd(dirname(a[1]))",
                 "knitr::knit(basename(a[1]), quiet = TRUE, envir = new.env())"), compiler)
    messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(compiler), shQuote(r$script)), stdout = TRUE, stderr = TRUE))
    md <- file.path(ici, paste0(d$name, ".md"))
    verifier(sprintf("%s.%s : se compile, avec ses graphiques", d$name, ext), file.exists(md) && !any(grepl("^## Error|Error in", readLines(md, encoding = "UTF-8"))) &&
             length(list.files(file.path(ici, "figure"), "\\.png$")) >= 3)
  }
}

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
