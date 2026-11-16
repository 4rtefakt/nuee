# Vérifie le document Quarto reproductible (moteur/R/document_reproductible.R) : pour un projet où
# un document cite une PLS, un GLM de Poisson et une PERMANOVA (des valeurs, dont le coefficient de
# la PLS et son intervalle de jack-knife, des graphiques, des tableaux, le texte des méthodes), et
# une PLS validée avec le modèle complet, l'export vers R de chaque modèle est écrit à côté du .qmd avec
# ses données et nuee.R ; chaque valeur citée devient une expression R qui, sur les scripts relancés
# seuls, redonne le texte que Nuée écrit (ses nombres : ceux du moteur, écrits comme
# app/interface/references.js les écrit, ce que vérifient d'abord des cas tirés de l'interface) ;
# ce que les scripts n'ont pas reste écrit en dur, et le bloc du début le dit. Puis, quand Quarto
# est installé (avec knitr et rmarkdown), le .qmd est rendu en HTML, en français puis en anglais
# (les noms des scripts traduits), et ses nombres comparés à ceux de Nuée ; sans Quarto, ce rendu
# est laissé de côté.
#   Rscript moteur/tests/verifier_reproductible.R
#   (QUARTO_PATH=<quarto> : un Quarto qui n'est ni sur le PATH ni à sa place habituelle)

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
dossier <- tempfile("reproductible")
dir.create(dossier)

# ---------------------------------------------------------------- nuee.R écrit les nombres comme l'interface

cat("Les nombres de nuee.R : ceux de references.js (nombre, pourcentage, valeurP)\n")
aide <- function(langue) {
  f <- file.path(dossier, sprintf("nuee_%s.R", langue))
  writeBin(charToRaw(enc2utf8(paste0(code_aide_reproductible(langue), "\n", collapse = ""))), f)
  e <- new.env(parent = baseenv())
  sys.source(f, envir = e, keep.source = FALSE)
  e
}
fr <- aide("fr")
en <- aide("en")
# (ce que l'interface écrit pour ces nombres, en français et en anglais : Intl.NumberFormat arrondit
# leur écriture décimale la plus courte, au plus loin de zéro à mi-chemin)
cas <- list(list(1.005, 2, "1,01", "1.01"), list(0.125, 2, "0,13", "0.13"), list(2.5, 0, "3", "3"), list(-0.001, 2, "0,00", "0.00"),
            list(12345.678, 2, "12 345,68", "12,345.68"), list(1234.5, 1, "1234,5", "1234.5"), list(1e-7, 6, "0,000000", "0.000000"),
            list(-1234567.891, 1, "−1 234 567,9", "−1,234,567.9"), list(0.045, 2, "0,05", "0.05"), list(8.345, 2, "8,35", "8.35"),
            list(123456789.125, 2, "123 456 789,13", "123,456,789.13"), list(9.995, 2, "10,00", "10.00"), list(-0.005, 2, "−0,01", "−0.01"),
            list(0.95, 1, "1,0", "1.0"), list(5e-324, 2, "0,00", "0.00"), list(1e21, 0, "1 000 000 000 000 000 000 000", "1,000,000,000,000,000,000,000"),
            list(99999.995, 2, "100 000,00", "100,000.00"), list(0.15, 1, "0,2", "0.2"), list(2.675, 2, "2,68", "2.68"), list(-2.5, 0, "−3", "−3"))
faux <- Filter(function(k) !identical(fr$nuee_nombre(k[[1]], k[[2]]), k[[3]]) || !identical(en$nuee_nombre(k[[1]], k[[2]]), k[[4]]), cas)
verifier(sprintf("%d nombres, arrondis et séparateurs compris%s", length(cas), if (length(faux)) sprintf(" ; DIFFÉRENTS : %s", paste(vapply(faux, function(k) format(k[[1]], digits = 17), ""), collapse = ", ")) else ""),
         !length(faux))
verifier("chiffres significatifs, pourcentages, valeurs p, manquants",
         identical(c(fr$nuee_nombre(0.0123456, significatifs = 3), fr$nuee_nombre(123.456, significatifs = 3), fr$nuee_nombre(0, significatifs = 3),
                     fr$nuee_pourcentage(0.875), fr$nuee_pourcentage(0.125), fr$nuee_p(0.0004), en$nuee_p(0.0456), fr$nuee_nombre(NA), en$nuee_pourcentage(0.71)),
                   c("0,0123", "123", "0,00", "88 %", "13 %", "p < 0,001", "p = 0.046", "—", "71%")))
co <- data.frame(terme = c("(Intercept)", "x"), estimation = c(1, -0.0123456), erreur_type = NA, ddl = NA, t = NA, p = c(0.5, 0.0004), bas = c(0, -0.0201), haut = c(2, -0.0046))
verifier("le coefficient d'une régression : estimation à 3 chiffres significatifs, intervalle, p",
         identical(c(fr$nuee_coefficient(co, "x"), en$nuee_coefficient(co, "x", "ic"), fr$nuee_coefficient(co, "x", "est", decimales = 2), fr$nuee_coefficient(co, "x", "p")),
                   c("−0,0123 [−0,0201 ; −0,0046], p < 0,001", "[−0.0201, −0.0046]", "−0,01", "p < 0,001")))

# ---------------------------------------------------------------- un projet, trois modèles cités

# 40 relevés : leur milieu, six espèces, des oiseaux comptés, une biomasse
set.seed(3)
n <- 40
temp <- round(stats::runif(n, 8, 18), 2)
habitat <- sample(c("bois", "lande", "prairie"), n, TRUE)
sp <- sapply(1:6, function(j) stats::rpois(n, exp(0.5 + sin(j) * (temp - 13) / 4 + (habitat == "bois") * (j %% 2))))
colnames(sp) <- sprintf("sp%d", 1:6)
df <- data.frame(nom = sprintf("r%02d", 1:n), site = rep(sprintf("S%d", 1:8), each = 5), habitat = habitat, temp = temp,
                 ph = round(stats::rnorm(n, 6.5, 0.4), 2), altitude = round(800 - 20 * temp + stats::rnorm(n, 0, 20)), sp,
                 oiseaux = stats::rpois(n, exp(0.2 + 0.1 * temp)), stringsAsFactors = FALSE)
df$biomasse <- round(2 + 0.3 * df$temp - 0.002 * df$altitude + stats::rnorm(n, 0, 0.4), 3)
p <- projet_depuis_tableau(df, "reproductible")
u <- function(...) numeros(p, c(...))
d_pls <- definir_modele(p, "PLS", x = u("temp", "ph", "altitude", "sp1", "sp2", "sp3"), y = u("biomasse"), A = 2, nom = "M1")
d_glm <- definir_modele(p, "GLM", x = u("temp", "habitat"), y = u("oiseaux"), famille = "poisson", nom = "M2", echelle = "none")
d_per <- definir_modele(p, "PERMANOVA", x = u("habitat", "temp"), y = u(sprintf("sp%d", 1:6)), echelle = "none", nom = "M3",
                        options = list(distance = "bray", permutations = 99, graine = 3))
# (et une PLS validée avec les centres et les poids du modèle complet, comme celles des fichiers
# .usp : son script n'a pas l'intervalle de ses coefficients)
d_plc <- definir_modele(p, "PLS", x = u("temp", "ph", "altitude"), y = u("biomasse"), A = 2, nom = "M4", validation = "complet")
# (et un GAM, une courbe de la température et une droite de l'altitude : ses effets, vue Effets)
d_gam <- definir_modele(p, "GAM", x = u("temp", "altitude"), y = u("biomasse"), famille = "gaussienne", lisses = u("temp"), echelle = "none", nom = "M5")
p$models <- list(d_pls, d_glm, d_per, d_plc, d_gam)
m_pls <- ajuster(p, d_pls)
m_glm <- ajuster(p, d_glm)
m_per <- ajuster(p, d_per)
modeles <- list(m1 = list(definition = d_pls, modele = m_pls), m2 = list(definition = d_glm, modele = m_glm), m3 = list(definition = d_per, modele = m_per),
                m4 = list(definition = d_plc, modele = ajuster(p, d_plc)), m5 = list(definition = d_gam, modele = ajuster(p, d_gam)))

# Les valeurs citées : ce que l'interface envoie (references.js : formeReference) et ce que Nuée en
# écrit (les nombres du moteur, écrits par nuee.R).
s <- m_pls$statistiques
r <- m_glm$resultat
terme <- r$coefficients$terme[2]  # (temp)
qual <- r$coefficients$terme[3]   # (une catégorie de l'habitat)
coef_moteur <- data.frame(terme = r$coefficients$terme, estimation = r$coefficients$estimation, erreur_type = NA, ddl = NA, t = NA,
                          p = r$coefficients$p, bas = r$coefficients$bas, haut = r$coefficients$haut)
valeur <- function(modele, chemin, forme, nombre) list(modele = modele, chemin = as.list(chemin), genre = "valeur", forme = forme, nombre = nombre)
nb <- function(d = 2, sig = NULL) list(forme = "nombre", decimales = d, significatifs = sig)
VALEURS <- list(
  valeur("m1", "Q2", nb(), function(f) f$nuee_nombre(s$composantes$Q2cum[2])),
  valeur("m1", "Q2", nb(3), function(f) f$nuee_nombre(s$composantes$Q2cum[2], 3)),
  valeur("m1", "R2Y", list(forme = "pourcentage", decimales = 0), function(f) f$nuee_pourcentage(s$composantes$R2Ycum[2])),
  valeur("m1", c("R2X", "1"), nb(), function(f) f$nuee_nombre(s$composantes$R2Xcum[1])),
  valeur("m1", "N", nb(0), function(f) f$nuee_nombre(m_pls$N, 0)),
  valeur("m1", "A", nb(0), function(f) f$nuee_nombre(m_pls$A, 0)),
  valeur("m1", "K", nb(0), function(f) f$nuee_nombre(m_pls$K, 0)),
  valeur("m1", c("vip", "temp"), nb(), function(f) f$nuee_nombre(s$variables$VIP[match("temp", s$variables$terme)])),
  valeur("m1", "RMSEcv", nb(2, 3), function(f) f$nuee_nombre(s$RMSEcv[["biomasse"]], significatifs = 3)),
  valeur("m1", "RMSEE", nb(2, 3), function(f) f$nuee_nombre(s$RMSEE[["biomasse"]], significatifs = 3)),
  valeur("m1", c("cvanova", "p"), list(forme = "p", decimales = 3), function(f) f$nuee_p(s$cv_anova[["biomasse"]]$p[2])),
  valeur("m1", c("coef", "temp"), list(forme = "coef_cs"), function(f) f$nuee_intervalle(s$coefficients[["CoeffCS biomasse"]][match("temp", s$coefficients$terme)],
                                                                                      s$coefficients[["CoeffCS biomasse IC"]][match("temp", s$coefficients$terme)])),
  valeur("m1", "gain", nb(), function(f) f$nuee_nombre(m_pls$gain)),
  valeur("m1", "Q2_vide", nb(), function(f) f$nuee_nombre(m_pls$q2_vide)),
  valeur("m2", "N", nb(0), function(f) f$nuee_nombre(m_glm$N, 0)),
  valeur("m2", "K", nb(0), function(f) f$nuee_nombre(m_glm$K, 0)),
  valeur("m2", "AIC", nb(1), function(f) f$nuee_nombre(r$AIC, 1)),
  valeur("m2", "AICc", nb(1), function(f) f$nuee_nombre(r$AICc, 1)),
  valeur("m2", "BIC", nb(1), function(f) f$nuee_nombre(r$BIC, 1)),
  valeur("m2", "Q2", nb(), function(f) f$nuee_nombre(m_glm$vc$Q2)),
  valeur("m2", "RMSEcv", nb(2, 3), function(f) f$nuee_nombre(m_glm$vc$RMSEcv, significatifs = 3)),
  valeur("m2", "deviance_expliquee", list(forme = "pourcentage", decimales = 1), function(f) f$nuee_pourcentage(r$r2$deviance_expliquee, 1)),
  valeur("m2", "R2_ajuste", nb(), function(f) f$nuee_nombre(r$r2$r2_ajuste)),
  valeur("m2", c("coef", terme), list(forme = "coef", quoi = "coef"), function(f) f$nuee_coefficient(coef_moteur, terme)),
  valeur("m2", c("est", terme), list(forme = "coef", quoi = "est", decimales = 3), function(f) f$nuee_coefficient(coef_moteur, terme, "est", 3)),
  valeur("m2", c("ic", qual), list(forme = "coef", quoi = "ic"), function(f) f$nuee_coefficient(coef_moteur, qual, "ic")),
  valeur("m2", c("p", qual), list(forme = "coef", quoi = "p"), function(f) f$nuee_coefficient(coef_moteur, qual, "p")),
  valeur("m3", "N", nb(0), function(f) f$nuee_nombre(m_per$N, 0)),
  valeur("m3", "K", nb(0), function(f) f$nuee_nombre(m_per$K, 0)))
# les graphiques, les tableaux, le texte des méthodes ; ce qui reste en dur : un graphique que le
# script ne dessine pas, le coefficient de la PLS validée avec le modèle complet, le texte
BLOCS <- list(
  list(modele = "m1", chemin = list("scores"), genre = "figure", vue = "scores", variante = list(), etiquette = "fig-scores", legende = "Scores"),
  list(modele = "m1", chemin = list("vip"), genre = "figure", vue = "vip", variante = list(), etiquette = "fig-vip", legende = "VIP"),
  list(modele = "m1", chemin = list("obspred"), genre = "figure", vue = "obspred", variante = list(), etiquette = "fig-obspred", legende = "Observé et ajusté"),
  list(modele = "m1", chemin = list("table", "resume"), genre = "tableau", cle = "resume", etiquette = "tbl-resume", legende = "R² et Q²"),
  list(modele = "m2", chemin = list("residus"), genre = "figure", vue = "residus", variante = list(mode = "ajustes", residus = "standard"), etiquette = "fig-residus", legende = "Résidus"),
  list(modele = "m2", chemin = list("table", "coefficients"), genre = "tableau", cle = "coefficients", etiquette = "tbl-coefficients", legende = "Coefficients"),
  list(modele = "m3", chemin = list("permanova"), genre = "figure", vue = "permanova", variante = list(), etiquette = "fig-permanova", legende = "PERMANOVA"),
  list(modele = "m3", chemin = list("table", "permanova"), genre = "tableau", cle = "permanova", etiquette = "tbl-permanova", legende = "Table de la PERMANOVA"),
  # (les effets du GAM, vue Effets : le premier terme, {{M5.effets}}, puis la droite de l'altitude, {{M5.effets.altitude}})
  list(modele = "m5", chemin = list("effets"), genre = "figure", vue = "effets", variante = list(terme = "temp", seuils = FALSE), etiquette = "fig-effets", legende = "Effet de la température"),
  list(modele = "m5", chemin = list("effets", "altitude"), genre = "figure", vue = "effets", variante = list(terme = "altitude", seuils = FALSE), etiquette = "fig-effet-altitude",
       legende = "Effet de l'altitude"),
  # (et les seuils de sa courbe, la vue Effets avec ses seuils : sa seule courbe, {{M5.seuils}})
  list(modele = "m5", chemin = list("seuils"), genre = "figure", vue = "effets", variante = list(terme = "temp", seuils = TRUE), etiquette = "fig-seuils", legende = "Seuils"),
  # (et les valeurs prédites de l'altitude sur l'échelle de la réponse, {{M5.valeurs_predites.altitude}} : le graphique prediction-k du script)
  list(modele = "m5", chemin = list("valeurs_predites", "altitude"), genre = "figure", vue = "effets", variante = list(terme = "altitude", seuils = FALSE, echelle = "reponse"),
       etiquette = "fig-predites", legende = "Valeurs prédites de l'altitude"))
EN_DUR <- list(
  list(modele = "m1", chemin = list("biplot"), genre = "figure", vue = "biplot", variante = list()),
  list(modele = "m4", chemin = list("coef", "temp"), genre = "valeur", forme = list(forme = "coef_cs"), texte = "0,12 [0,05 ; 0,19]"),
  list(modele = "m1", chemin = list("methodes"), genre = "texte"))

ecrire <- function(langue, ici, modeles_ici = modeles) {
  f <- if (langue == "en") en else fr
  refs <- c(lapply(VALEURS, function(v) c(v[c("modele", "chemin", "genre", "forme")], list(texte = v$nombre(f)))), BLOCS, EN_DUR, list(NULL))
  dir.create(ici, showWarnings = FALSE)
  list(r = ecrire_document_reproductible(p, modeles_ici, refs, ici, "Mon article", langue, "essai"), refs = refs)
}

for (langue in c("fr", "en")) {
  cat(sprintf("Le document reproductible, en %s\n", if (langue == "fr") "français" else "anglais"))
  ici <- file.path(dossier, langue)
  x <- ecrire(langue, ici)
  rr <- x$r
  refs <- x$refs
  nv <- length(VALEURS)
  nb_blocs <- length(BLOCS)
  sous <- file.path(ici, "Mon article_R")
  verifier("à côté du .qmd : nuee.R, et pour chaque modèle son script et ses données en CSV",
           identical(sort(basename(rr$fichiers)), sort(c("nuee.R", paste0("M", 1:5, ".R"), sprintf(if (langue == "en") "M%d - data.csv" else "M%d - donnees.csv", 1:5)))) &&
             all(file.exists(rr$fichiers)) && all(startsWith(rr$relatifs, "Mon article_R/")) && dir.exists(sous))
  codes <- vapply(rr$references[seq_len(nv)], function(z) z$code %||% NA_character_, "")
  manquent <- which(is.na(codes))
  verifier(sprintf("chaque valeur citée est une expression R qui redonne le texte de Nuée (%d)%s", nv,
                   if (length(manquent)) sprintf(" ; EN DUR : %s", paste(vapply(rr$references[manquent], function(z) z$raison %||% "?", ""), collapse = " | ")) else ""),
           !length(manquent))
  verifier("ses noms sont ceux du script, dans sa langue",
           if (langue == "en") any(grepl("M1$components$Q2cum[2]", codes, fixed = TRUE)) && any(grepl("nrow(M2$model_data)", codes, fixed = TRUE))
           else any(grepl("M1$composantes$Q2cum[2]", codes, fixed = TRUE)) && any(grepl("nrow(M2$cadre)", codes, fixed = TRUE)) && any(grepl("nuee_coefficient(M2$coefficients", codes, fixed = TRUE)))
  blocs <- rr$references[nv + seq_len(nb_blocs)]
  verifier("graphiques et tableaux : un bloc de code chacun (nuee_dans, knitr::kable)",
           all(vapply(blocs, function(z) length(z$lignes) > 0 && startsWith(z$lignes[1], "nuee_dans("), TRUE)) &&
             all(vapply(blocs[vapply(BLOCS, function(b) b$genre == "tableau", TRUE)], function(z) grepl("knitr::kable(", z$lignes, fixed = TRUE), TRUE)))
  dur <- rr$references[nv + nb_blocs + seq_along(EN_DUR)]
  verifier("en dur, avec la raison : un graphique que le script ne dessine pas, le coefficient d'une PLS sans son intervalle, le texte des méthodes",
           all(vapply(dur, function(z) is.character(z$raison) && nzchar(z$raison) && is.null(z$code) && is.null(z$lignes), TRUE)) && is.null(rr$references[[length(refs)]]) &&
             grepl(if (langue == "en") "jackknife" else "jack-knife", dur[[2]]$raison))
  inst <- rr$installation
  verifier("le premier bloc : nuee.R, les modèles refaits, ce qui reste en dur dit en commentaire",
           inst[1] == "```{r}" && "#| include: false" %in% inst && any(inst == "source(\"Mon article_R/nuee.R\", encoding = \"UTF-8\")") &&
             any(inst == "M3 <- nuee_refaire(\"Mon article_R/M3.R\")") && any(grepl("^#   M1\\.biplot", inst)) && any(grepl("^#   M1\\.methodes", inst)))
  # le .qmd, comme rendu_document.js l'écrit (versQmd) : le bloc du début, les valeurs en ligne, un
  # bloc par graphique et par tableau
  bloc <- function(b, z) c("```{r}", sprintf("#| label: %s", b$etiquette), if (b$genre == "figure") c(sprintf("#| fig-cap: \"%s\"", b$legende), sprintf("#| fig-width: %s", z$largeur), sprintf("#| fig-height: %s", z$hauteur))
                           else sprintf("#| tbl-cap: \"%s\"", b$legende), "#| echo: false", z$lignes, "```", "")
  qmd <- file.path(ici, "Mon article.qmd")
  writeBin(charToRaw(enc2utf8(paste0(c("---", "title: \"Essai\"", sprintf("lang: %s", langue), "---", "", inst, "",
                                       sprintf("- v%d: `r %s`", seq_len(nv), codes), "", unlist(Map(bloc, BLOCS, blocs))), "\n", collapse = ""))), qmd)
  attendus <- vapply(refs[seq_len(nv)], `[[`, "", "texte")
  # Le code R du .qmd lancé sans knitr dans une session neuve, depuis son dossier : le bloc du début
  # (nuee.R, les modèles refaits), puis chaque graphique dans un PNG à lui ; chacun est dessiné (les
  # effets du GAM compris : le script les refait, script.R, effets_script)
  figures <- which(vapply(BLOCS, function(b) b$genre == "figure", TRUE))
  code_r <- c(grep("^```", inst, value = TRUE, invert = TRUE),
              unlist(lapply(figures, function(k) c(sprintf("grDevices::png(%s)", deparse(sprintf("figure-%d.png", k))), blocs[[k]]$lignes, "invisible(grDevices::dev.off())"))))
  f_code <- file.path(ici, "figures.R")
  writeBin(charToRaw(enc2utf8(paste0(code_r, "\n", collapse = ""))), f_code)
  avant <- setwd(ici)
  sortie_r <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(basename(f_code))), stdout = TRUE, stderr = TRUE))
  setwd(avant)
  dessinees <- file.exists(file.path(ici, sprintf("figure-%d.png", figures)))
  verifier(sprintf("le code de ses %d graphiques, lancé seul dans une session neuve, les dessine, les effets du GAM compris%s", length(figures),
                   if (!all(dessinees)) sprintf(" ; NON : %s", paste(c(vapply(BLOCS[figures[!dessinees]], `[[`, "", "etiquette"), utils::tail(sortie_r, 3)), collapse = " / ")) else ""),
           all(dessinees))

  # Rendu par Quarto, s'il est là (et knitr et rmarkdown pour R) : les nombres de la page, ses figures et ses tableaux
  quarto <- Filter(function(f) nzchar(f) && file.exists(f), c(Sys.getenv("QUARTO_PATH"), unname(Sys.which("quarto")),
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin", "quarto.exe"), file.path(Sys.getenv("ProgramFiles"), "Quarto", "bin", "quarto.exe"),
    "/usr/local/bin/quarto", "/opt/quarto/bin/quarto", "/Applications/quarto/bin/quarto"))
  if (!length(quarto)) {
    cat("  (Quarto absent : le .qmd n'est pas rendu)\n")
    next
  }
  if (!requireNamespace("knitr", quietly = TRUE) || !requireNamespace("rmarkdown", quietly = TRUE)) {
    cat("  (knitr ou rmarkdown absent : le .qmd n'est pas rendu)\n")
    next
  }
  # (Quarto lance ce R-ci, avec ses bibliothèques)
  Sys.setenv(QUARTO_R = R.home("bin"), R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  avant <- setwd(ici)
  sortie <- suppressWarnings(system2(quarto[1], c("render", shQuote(basename(qmd)), "--to", "html"), stdout = TRUE, stderr = TRUE))
  setwd(avant)
  html <- file.path(ici, "Mon article.html")
  verifier(sprintf("Quarto le rend en HTML%s", if (!file.exists(html)) sprintf(" (%s)", paste(utils::tail(sortie, 6), collapse = " / ")) else ""), file.exists(html))
  if (!file.exists(html)) next
  page <- paste(readLines(html, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  Encoding(page) <- "UTF-8"
  vus <- regmatches(page, gregexpr("<li>v[0-9]+: [^<]*</li>", page))[[1]]
  rendus <- stats::setNames(sub("^<li>v[0-9]+: (.*)</li>$", "\\1", vus), sub("^<li>v([0-9]+): .*$", "\\1", vus))
  rendus <- gsub("&amp;", "&", gsub("&gt;", ">", gsub("&lt;", "<", rendus)))
  differents <- which(rendus[as.character(seq_len(nv))] != attendus | is.na(rendus[as.character(seq_len(nv))]))
  verifier(sprintf("ses %d nombres, recalculés depuis les données brutes, sont ceux de Nuée (%s…)%s", nv, paste(utils::head(attendus, 4), collapse = " ; "),
                   if (length(differents)) sprintf(" ; DIFFÉRENTS : %s", paste(sprintf("v%d %s au lieu de %s", differents, rendus[as.character(differents)], attendus[differents]), collapse = ", ")) else ""),
           length(rendus) == nv && !length(differents))
  ids <- vapply(BLOCS, `[[`, "", "etiquette")
  verifier("ses figures et ses tableaux, numérotés, chacun à son étiquette", all(vapply(ids, function(i) grepl(sprintf("id=\"%s\"", i), page, fixed = TRUE), TRUE)) &&
             length(gregexpr("<img ", page)[[1]]) >= 5 && length(gregexpr("<table", page)[[1]]) >= 3)
}

cat("Un modèle que son script ne refait pas\n")
ms <- modeles
ms$m3$erreur <- "une catégorie qu'il a vue a changé de nom"
x <- ecrire("fr", file.path(dossier, "perime"), ms)
z <- x$r$references[vapply(x$refs, function(r) identical(r$modele, "m3"), TRUE)]
verifier("ses valeurs, graphiques et tableaux restent en dur ; le premier bloc le dit, sans le refaire",
         all(vapply(z, function(y) identical(y$raison, ms$m3$erreur), TRUE)) && !any(grepl("^M3 <- ", x$r$installation)) &&
           any(grepl("^#   M3 : une catégorie", x$r$installation)) && any(grepl("^M1 <- nuee_refaire", x$r$installation)))
cat("Les effets que le script d'une régression ne dessine pas\n")
# deux courbes : leurs seuils, que le script dessine ensemble, restent l'image de Nuée (leurs effets
# sont refaits) ; un terme croisé avec des catégories, une droite par catégorie, aussi (la droite
# seule d'un autre terme est refaite)
d_gam2 <- definir_modele(p, "GAM", x = u("temp", "altitude"), y = u("biomasse"), famille = "gaussienne", lisses = u("temp", "altitude"), echelle = "none", nom = "M6")
d_lmx <- definir_modele(p, "LM", x = u("temp", "habitat", "ph"), y = u("biomasse"), echelle = "none", croises = list(u("temp", "habitat")), nom = "M7")
figure <- function(modele, chemin, variante) list(modele = modele, chemin = as.list(chemin), genre = "figure", vue = "effets", variante = variante)
ici <- file.path(dossier, "effets")
dir.create(ici)
re <- ecrire_document_reproductible(p, list(m6 = list(definition = d_gam2, modele = ajuster(p, d_gam2)), m7 = list(definition = d_lmx, modele = ajuster(p, d_lmx))),
                                    list(figure("m6", "seuils", list(terme = "temp", seuils = TRUE)), figure("m6", c("effets", "altitude"), list(terme = "altitude", seuils = FALSE)),
                                         figure("m7", "effets", list(terme = "temp", seuils = FALSE)), figure("m7", c("effets", "ph"), list(terme = "ph", seuils = FALSE))),
                                    ici, "Effets", "fr", "essai")
code <- vapply(re$references, function(z) length(z$lignes) > 0, TRUE)
verifier("en dur : les seuils d'un GAM à deux courbes et l'effet d'un terme croisé avec l'habitat ; refaits : la courbe de l'altitude et la droite du pH",
         identical(code, c(FALSE, TRUE, FALSE, TRUE)) && all(vapply(re$references[!code], function(z) nzchar(z$raison %||% ""), TRUE)))
cat("Les résultats clés des autres analyses, cités et refaits par leur script\n")
# (references.js : referenceCle ; document_reproductible.R : expression_cle) la pente globale de TRIM,
# sa variation en %, sa classe, σ² et ρ, sur les comptages d'alouettes du tutoriel ; la densité de
# l'échantillonnage par distances, son intervalle et son CV, p et ESW, sur ses transects ; le R² ajusté
# et le test d'une RDA, le stress d'une NMDS, le R² et la valeur p d'une PERMANOVA. La forme de chaque
# valeur : celle que l'interface envoie ; son texte : celui de Nuée, écrit ici par nuee.R.
sig3 <- function(v) max(0, min(6, 2 - floor(log10(abs(v)))))
pa <- projet_depuis_tableau(utils::read.csv("docs/site/donnees/skylark_counts.csv", stringsAsFactors = FALSE), "alouettes")
ua <- function(...) numeros(pa, c(...))
d_trim <- definir_modele(pa, "TRIM", x = NULL, y = ua("skylarks"), echelle = "none", nom = "T1",
                         options = list(site = list(by = "variable", variable = ua("site")), temps = list(by = "variable", variable = ua("year")), modele = 2,
                                        surdispersion = TRUE, correlation = TRUE))
m_trim <- ajuster(pa, d_trim)
pt <- m_trim$pentes[m_trim$pentes$quoi == "imputee", ]
pd <- projet_depuis_tableau(cbind(nom = sprintf("d%03d", seq_len(nrow(utils::read.csv("docs/site/donnees/skylark_transects.csv")))),
                                  utils::read.csv("docs/site/donnees/skylark_transects.csv", stringsAsFactors = FALSE), stringsAsFactors = FALSE), "transects")
ud <- function(x) numeros(pd, x)
d_dist <- definir_modele(pd, "DISTANCE", x = NULL, y = ud("distance"), echelle = "none", nom = "D1",
                         options = list(transect = "ligne", echantillon = list(by = "variable", variable = ud("Sample.Label")), strate = list(by = "variable", variable = ud("Region.Label")),
                                        effort = ud("Effort"), aire = ud("Area"), taille = ud("size"), troncature = list(type = "aucune"), gauche = 0, cle = "hn", serie = "cos",
                                        ajustements = "aic", monotonie = "stricte", echelle_ajustements = "largeur", unites = list(distance = "m", effort = "km", aire = "km2")))
m_dist <- ajuster(pd, d_dist)
tot <- m_dist$densite[nrow(m_dist$densite), ]
d_rda <- definir_modele(p, "RDA", x = u("temp", "ph", "habitat"), y = u(sprintf("sp%d", 1:6)), echelle = "none", nom = "R1",
                        options = list(transformation = "hellinger", permutations = 99, graine = 3))
m_rda <- ajuster(p, d_rda)
d_nmds <- definir_modele(p, "NMDS", x = NULL, y = u(sprintf("sp%d", 1:6)), echelle = "none", nom = "N1", options = list(distance = "bray", k = 2, departs = 5, graine = 5))
m_nmds <- ajuster(p, d_nmds)
ia <- list(t1 = list(definition = d_trim, modele = m_trim, projet = pa), d1 = list(definition = d_dist, modele = m_dist, projet = pd),
           r1 = list(definition = d_rda, modele = m_rda), n1 = list(definition = d_nmds, modele = m_nmds), m3 = modeles$m3)
pc <- function(d = 1) list(forme = "pourcentage", decimales = d)
pv <- list(forme = "p", decimales = 3)
ic_f <- function(d, pourcentage = FALSE) list(forme = "ic", decimales = d, pourcentage = pourcentage)
CLES <- list(
  valeur("t1", "pente", nb(4), function(f) f$nuee_nombre(pt$multiplicative, 4)),
  valeur("t1", c("pente", "ic"), ic_f(4), function(f) f$nuee_ic(pt$bas, pt$haut, 4)),
  valeur("t1", c("pente", "p"), pv, function(f) f$nuee_p(pt$p)),
  valeur("t1", c("pente", "pourcent"), pc(1), function(f) f$nuee_pourcentage(pt$multiplicative - 1, 1)),
  valeur("t1", c("pente", "pourcent", "ic"), ic_f(1, TRUE), function(f) f$nuee_ic(pt$bas - 1, pt$haut - 1, 1, TRUE)),
  c(valeur("t1", c("pente", "classe"), list(forme = "texte"), NULL), list(classe = pt$classe)),
  valeur("t1", "sigma2", nb(sig3(m_trim$sigma2)), function(f) f$nuee_nombre(m_trim$sigma2, sig3(m_trim$sigma2))),
  valeur("t1", "rho", nb(sig3(m_trim$rho)), function(f) f$nuee_nombre(m_trim$rho, sig3(m_trim$rho))),
  valeur("d1", "D", nb(sig3(tot$D)), function(f) f$nuee_nombre(tot$D, sig3(tot$D))),
  valeur("d1", c("D", "ic"), ic_f(sig3(tot$D)), function(f) f$nuee_ic(tot$bas_D, tot$haut_D, sig3(tot$D))),
  valeur("d1", c("D", "cv"), pc(1), function(f) f$nuee_pourcentage(tot$cv, 1)),
  valeur("d1", "abondance", nb(sig3(tot$N)), function(f) f$nuee_nombre(tot$N, sig3(tot$N))),
  valeur("d1", "p", nb(sig3(m_dist$p)), function(f) f$nuee_nombre(m_dist$p, sig3(m_dist$p))),
  valeur("d1", "ESW", nb(sig3(m_dist$effectif$valeur)), function(f) f$nuee_nombre(m_dist$effectif$valeur, sig3(m_dist$effectif$valeur))),
  valeur("r1", "R2_ajuste", nb(sig3(m_rda$r2_ajuste)), function(f) f$nuee_nombre(m_rda$r2_ajuste, sig3(m_rda$r2_ajuste))),
  valeur("r1", c("test", "modele", "p"), pv, function(f) f$nuee_p(m_rda$test$p)),
  valeur("n1", "stress", nb(sig3(m_nmds$noyau$stress)), function(f) f$nuee_nombre(m_nmds$noyau$stress, sig3(m_nmds$noyau$stress))),
  valeur("m3", c("permanova", "modele"), nb(sig3(m_per$noyau$modele$R2)), function(f) f$nuee_nombre(m_per$noyau$modele$R2, sig3(m_per$noyau$modele$R2))),
  valeur("m3", c("permanova", "modele", "p"), pv, function(f) f$nuee_p(m_per$noyau$modele$p)))
for (langue in c("fr", "en")) {
  f <- if (langue == "en") en else fr
  refs_c <- lapply(CLES, function(v) c(v[c("modele", "chemin", "genre", "forme")],
                                        list(texte = if (!is.null(v$classe)) libelle_classe_trim(v$classe, langue == "fr") else v$nombre(f))))
  ici <- file.path(dossier, paste0("cles_", langue))
  dir.create(ici)
  rc <- ecrire_document_reproductible(p, ia, refs_c, ici, "Suivi", langue, "essai")
  codes <- vapply(rc$references, function(z) z$code %||% NA_character_, "")
  manquent <- which(is.na(codes))
  verifier(sprintf("en %s : chaque résultat clé cité (pente de TRIM et sa classe, densité et CV, R² ajusté, stress, PERMANOVA) est une expression du script qui redonne le texte de Nuée (%d)%s",
                   langue, length(CLES), if (length(manquent)) sprintf(" ; EN DUR : %s", paste(sprintf("%s (%s)", vapply(CLES[manquent], function(v) paste(unlist(v$chemin), collapse = "."), ""),
                                                                                                          vapply(rc$references[manquent], function(z) z$raison %||% "?", "")), collapse = " | ")) else ""),
           !length(manquent) && all(vapply(rc$modeles, function(x) is.null(x$erreur), TRUE)))
}

cat("Un modèle ajusté avant un nouvel ordre de ses catégories\n")
# (Édition › Catégories : l'habitat dans un autre ordre ; le modèle reste valide, sa définition suit le
# nouvel ordre. Le script de la PERMANOVA, écrit sur le projet d'aujourd'hui, donnerait ses
# dispersions dans un autre ordre : écrit sur le projet tel qu'elle l'a vu, avec la définition de son
# ajustement, comme le serveur le fait (serveur.R : document_reproductible ; atelier.R :
# projet_tel_ajuste), il la refait)
p_ordre <- changer_categories(p, u("habitat"), c("bois", "lande", "prairie"), c("prairie", "lande", "bois"))
refs_ordre <- list(valeur("m3", "N", nb(0), NULL), list(modele = "m3", chemin = list("permanova"), genre = "figure", vue = "permanova", variante = list()))
refs_ordre[[1]]$texte <- fr$nuee_nombre(m_per$N, 0)
ici <- file.path(dossier, "ordre")
dir.create(ici)
aujourdhui <- ecrire_document_reproductible(p_ordre, list(m3 = list(definition = aligner_categories(p_ordre, d_per, categories_definition(p, d_per)), modele = m_per)),
                                            refs_ordre, ici, "Aujourdhui", "fr", "essai")
tel_vu <- ecrire_document_reproductible(p_ordre, list(m3 = list(definition = d_per, modele = m_per, projet = projet_tel_ajuste(p_ordre, ordres_categories(p)))),
                                        refs_ordre, ici, "Tel vu", "fr", "essai")
verifier("sur le projet tel qu'il l'a vu : refait, sa valeur et son graphique par R (sur celui d'aujourd'hui : ses dispersions dans un autre ordre)",
         grepl("dispersion", aujourdhui$modeles[[1]]$erreur %||% "") && is.null(tel_vu$modeles[[1]]$erreur) &&
           identical(tel_vu$references[[1]]$code, "nuee_nombre(nrow(M3$Y), 0)") && length(tel_vu$references[[2]]$lignes) > 0)

if (is.null(fonction_analyse("ecrire_script_", "DISTANCE"))) {
  verifier("un type sans export vers R le dit (raison_sans_export), dans la langue demandée", grepl("export vers R", raison_sans_export(list(type = "DISTANCE"))) &&
             grepl("No export to R", raison_sans_export(list(type = "DISTANCE"), function(fr, en) en)) &&
             is.null(raison_sans_export(d_pls)) && is.null(raison_sans_export(d_glm)) && is.null(raison_sans_export(d_per)))
  # (un modèle de ce type cité : ses valeurs en dur, le premier bloc le dit, dans la langue du document)
  ms <- list(m1 = modeles$m1, m5 = list(definition = list(name = "M5", type = "DISTANCE"), modele = list()))
  ici <- file.path(dossier, "sans_export")
  dir.create(ici)
  r5 <- ecrire_document_reproductible(p, ms, list(list(modele = "m5", chemin = list("N"), genre = "valeur", forme = nb(0), texte = "12"),
                                                  list(modele = "m1", chemin = list("N"), genre = "valeur", forme = nb(0), texte = "40")), ici, "Doc", "en", "essai")
  verifier("un modèle de ce type cité : sa valeur en dur, le premier bloc le dit (en anglais pour un document en anglais), les autres refaits",
           is.null(r5$references[[1]]$code) && grepl("No export to R", r5$references[[1]]$raison) && identical(r5$references[[2]]$code, "nuee_nombre(M1$N, 0)") &&
             any(grepl("^#   M5: No export to R", r5$installation)) && !any(grepl("^M5 <- ", r5$installation)) && !file.exists(file.path(ici, "Doc_R", "M5.R")))
}

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
