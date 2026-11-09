# Vérifie le script R autonome (moteur/R/script.R). Pour chaque sorte de modèle que le moteur
# ajuste (ACP, PLS à un ou plusieurs Y, PLS-DA, OPLS, OPLS-DA à deux classes et plus) et ses
# options (transformations, mises à l'échelle par variable, termes qualitatifs, croisés et carrés,
# spectres, valeurs manquantes, jeu de test, modèle d'un .usp orienté comme sa solution
# enregistrée, modèle hiérarchique, sous-modèles de la validation croisée à court de variation),
# le script est écrit, lancé dans une session R neuve
# (Rscript --vanilla, sans le moteur) depuis un autre dossier, et ses résultats sont comparés à
# ceux du moteur : scores, R2 et Q2, T2, DModX et leurs limites, VIP, coefficients, CV-ANOVA,
# classification, jeu de test, test de permutation. Puis : le script d'une PLS simple est court
# et n'a pas de fonction inutile ; la séance en un script ; l'export en anglais ; les rapports
# R Markdown et Quarto (compilés avec knitr s'il est installé).
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
# lance un script dans une session neuve (ses graphiques sans fichier), depuis le dossier de travail
# `dossier`, puis garde les objets nommés (et le résultat du code de `suite`, s'il y en a)
lanceur <- file.path(dossier, "lancer.R")
writeLines(c(".a <- commandArgs(TRUE)",
             "pdf(NULL)",
             "source(.a[1], encoding = \"UTF-8\")",
             ".r <- mget(strsplit(.a[3], \",\")[[1]], ifnotfound = list(NULL))",
             "if (length(.a) > 3) .r$suite <- eval(parse(.a[4], encoding = \"UTF-8\"))",
             "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, objets, suite = NULL) {
  sortie <- tempfile("sortie", dossier, ".rds")
  arguments <- c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), shQuote(paste(objets, collapse = ",")))
  if (!is.null(suite)) {
    f <- tempfile("suite", dossier, ".R")
    writeLines(enc2utf8(suite), f, useBytes = TRUE)
    arguments <- c(arguments, shQuote(f))
  }
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, arguments, stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
nombres <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.matrix(if (is.table(x)) unclass(x) else x)
  dimnames(x) <- NULL
  storage.mode(x) <- "double"
  x
}
meme <- function(a, b) (is.null(a) && is.null(b)) || (!is.null(a) && !is.null(b) && isTRUE(all.equal(nombres(a), nombres(b), tolerance = 1e-10)))

# Ce que le moteur donne, et ce que le script a calculé (sous ses noms, dans sa langue).
ATTENDUS <- c("m", "composantes", "T2", "DModX", "DModX_pondere", "limites", "VIP", "coefficients", "cv_anova",
              "classification", "test", "predictions_test", "classification_test")
cv <- function(x) if (length(x)) do.call(rbind, lapply(x, `[`, c("SS", "DF", "MS", "F", "p")))
moteur <- function(m) {
  s <- m$statistiques
  list(scores = m$T, "R2, Q2" = s$composantes, T2 = s$observations$T2, DModX = s$observations$DModX, "DModX pondéré" = s$observations$DModX_pondere,
       limites = unlist(s$limites), VIP = s$variables$VIP, coefficients = if (!is.null(s$coefficients)) as.matrix(s$coefficients[paste("Coeff", m$y$noms)]),
       # (le script n'écrit la CV-ANOVA que si elle a des degrés de liberté résiduels : N - 1 - 2A > 0)
       "CV-ANOVA" = if (all(vapply(s$cv_anova, function(t) t$DF[3] > 0, TRUE))) cv(s$cv_anova), classification = s$classification$vc$table,
       "jeu de test" = if (!is.null(m$test$statistiques) && !m$plsda) m$test$statistiques[c("n", "RMSEP", "biais", "SEP", "Q2ext")],
       "hors du domaine" = m$test$hors_domaine, "classes du jeu de test" = m$test$classement$table)
}
script <- function(r, langue) {
  n <- function(x) nom_script(x, langue)
  pt <- r[[n("predictions_test")]]
  list(scores = r$m$T, "R2, Q2" = r[[n("composantes")]], T2 = r$T2, DModX = r$DModX, "DModX pondéré" = r[[n("DModX_pondere")]],
       limites = r[[n("limites")]], VIP = r$VIP, coefficients = r$coefficients, "CV-ANOVA" = cv(r$cv_anova), classification = r$classification,
       "jeu de test" = if (!is.null(r$test)) r$test[-1], "hors du domaine" = if (!is.null(pt)) sum(pt[[n("hors_domaine")]]),
       "classes du jeu de test" = r$classification_test)
}

# Écrit le script de `d`, le lance dans une session neuve et compare ses résultats à ceux du moteur.
essayer <- function(nom, p, d, langue = "fr", permutations = FALSE, ...) {
  m <- ajuster(p, d)
  dir.create(ici <- file.path(dossier, nom))
  r <- ecrire_script(p, d, file.path(ici, paste0(nom, ".R")), "moteur", m = m, version = "essai", langue = langue, ...)
  appel <- if (permutations) {
    if (isTRUE(m$opls)) sprintf("permutations(X, Y, %s, n = 3)", if ((m$Ap %||% 1) > 1) "Ap, A - Ap" else "A - 1") else "permutations(X, Y, A, n = 3)"
  }
  s <- lancer(r$script, nom_script(ATTENDUS, langue), appel)
  if (!length(s)) {
    verifier(sprintf("%s : le script tourne (%s)", nom, paste(utils::tail(attr(s, "messages"), 3), collapse = " / ")), FALSE)
    return(invisible(r))
  }
  a <- moteur(m)
  b <- script(s, langue)
  differents <- names(a)[!vapply(names(a), function(k) meme(a[[k]], b[[k]]), TRUE)]
  presents <- names(a)[!vapply(a, is.null, TRUE)]
  verifier(sprintf("%s : les mêmes %s que le moteur%s", nom, paste(setdiff(presents, differents), collapse = ", "),
                   if (length(differents)) sprintf(" ; DIFFÉRENTS : %s", paste(differents, collapse = ", ")) else ""),
           !length(differents) && is.null(r$ecart))
  if (permutations) {
    attendu <- permutations(p, m, d, n = 3)
    verifier(sprintf("%s : le test de permutation du script donne celui du moteur", nom),
             meme(s$suite$points, attendu$points[c("correlation", "R2", "Q2")]) &&
               meme(c(s$suite[[nom_script("ordonnee_R2", langue)]], s$suite[[nom_script("ordonnee_Q2", langue)]]), c(attendu$ordonnee_R2, attendu$ordonnee_Q2)))
  }
  invisible(r)
}
lignes_script <- function(r) readLines(r$script, encoding = "UTF-8")
# Le code d'un script hors de ses données : ses lignes qui ne sont ni vides ni des commentaires.
code_hors_donnees <- function(texte) {
  debut <- grep("^# 2\\. ", texte)
  fin <- grep("^# 3\\. ", texte)
  texte <- texte[-(debut:(fin - 1))]
  texte[nzchar(trimws(texte)) & !grepl("^\\s*#", texte)]
}

p <- lire_projet("exemple/vergers.json")
uid <- function(nom) numeros(p, nom)
complets <- uid(c("poids", "diametre", "fermete", "sucre", "acidite", "amidon", "mat_seche", "couleur_a", "couleur_b", "azote", "stockage"))  # sans valeur manquante
x <- uid(c("poids", "diametre", "fermete", "sucre", "acidite", "amidon"))

cat("Modèles du projet d'exemple\n")
for (i in seq_along(p$models)) {
  d <- p$models[[i]]
  r <- essayer(d$name, p, d, donnees = if (i %% 2) "script" else "csv")
}
texte <- lignes_script(list(script = file.path(dossier, "M1", "M1.R")))
verifier("le script ne charge rien de Nuée ni d'aucun paquet", !any(grepl("charger\\.R|source\\(|library\\(|require\\(|::", code_hors_donnees(texte))))

cat("Une PLS simple (données complètes, UV) : un script court, sans fonction inutile\n")
r <- essayer("pls_simple", p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2), permutations = TRUE)
texte <- lignes_script(r)
code <- code_hors_donnees(texte)
fonctions <- sub(" <- function.*", "", grep("^[A-Za-z_.][A-Za-z0-9_.]* <- function", texte, value = TRUE))
verifier(sprintf("%d lignes de code hors des données (200 au plus ; %d avec les commentaires)", length(code), length(texte) - diff(grep("^# [23]\\. ", texte))),
         length(code) <= 200)
verifier(sprintf("ses fonctions : %s ; ni ACP, ni OPLS, ni spectres, ni valeurs manquantes, ni classes", paste(fonctions, collapse = ", ")),
         !any(grepl("acp|opls|snv|msc|savitzky|detrend|ligne_base|roc|projeter|plus_grande|base_poids", fonctions)))
verifier("chacune y sert (appelée ailleurs que dans sa définition)", all(vapply(fonctions, function(f) any(grepl(sprintf("\\b%s\\(", f), texte, perl = TRUE)), TRUE)))

cat("Cas particuliers\n")
d <- definir_modele(p, "PLS", x = x[-1], y = x[1], observations = setdiff(numeros_observations(p), numeros_observations(p)[c(3, 50)]),
                    echelle = stats::setNames(c("UV", "Par"), c("defaut", as.character(x[2]))),
                    transformations = stats::setNames("log10", as.character(x[3])), croises = list(x[2:3]), A = 2)
r <- essayer("exclusions", p, d, renv = TRUE, rdata = TRUE, rproj = TRUE)
verifier("fichiers à côté : .RData, renv.lock, .Rproj", all(file.exists(r$chemins)) && length(r$chemins) == 4)
e <- new.env(); load(r$chemins[2], envir = e)
verifier(".RData : données, modèle et résultats", all(c("donnees", "m", "composantes", "observations", "variables") %in% ls(e)) && ncol(e$m$T) == 2)
lock <- jsonlite::fromJSON(r$chemins[3])
verifier("renv.lock : version de R", identical(lock$R$Version, paste(R.version$major, R.version$minor, sep = ".")))
d2 <- d
d2$x[[1]]$scaling <- list(centre = 1.5, weight = 0.25)  # échelle enregistrée (non reconnue)
d2$observations <- rev(d2$observations)                 # observations dans un autre ordre
essayer("termes", p, d2, donnees = "csv")
essayer("pls_2y", p, definir_modele(p, "PLS", x = complets, y = uid(c("note_gout", "jus")), A = 3))
essayer("opls_1y", p, definir_modele(p, "OPLS", x = complets, y = uid("note_gout"), A = 3), permutations = TRUE)
essayer("opls_2y", p, definir_modele(p, "OPLS", x = complets, y = uid(c("note_gout", "jus")), A = 4), permutations = TRUE)
essayer("opls_4_classes", p, definir_modele(p, "OPLS-DA", x = x, classe = uid("variete"), A = 4), donnees = "csv")
essayer("plsda_qualitatives", p, definir_modele(p, "PLS-DA", x = c(uid("verger"), complets[1:6]), classe = uid("culture"), A = 2,
                                                croises = list(c(uid("verger"), complets[1]), c(complets[2], complets[2]))))
essayer("qualitatives_et_croises", p, definir_modele(p, "PLS", x = c(uid(c("variete", "culture")), complets[1:5]), y = uid("note_gout"), A = 3,
                                                     croises = list(c(uid("variete"), complets[4]), c(complets[4], complets[4]))))
essayer("transformations_echelles", p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2,
                                                      transformations = stats::setNames(c("racine2", "racine4", "log10:1"), as.character(complets[1:3])),
                                                      echelle = stats::setNames(c("Ctr", "none", "Par"), as.character(complets[4:6]))))
essayer("acp_centree", p, definir_modele(p, "PCA", x = complets, A = 3, echelle = "Ctr"))
essayer("sans_echelle", p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2, echelle = stats::setNames(c("none", "UV"), c("defaut", as.character(complets[1])))))
# une transformation de type 7 (puissance), et des valeurs hors du domaine du log : manquantes
q <- p
q$donnees[c(2, 30), as.character(complets[1])] <- c(0, -3)
d <- definir_modele(q, "PLS", x = complets, y = uid("note_gout"), A = 2, transformations = stats::setNames("log10", as.character(complets[1])))
d$x[[2]]$transform <- list(type = 7, params = c(NA, 2, 1, 1.5))
essayer("hors_domaine", q, d)
cat("Jeu de test\n")
r <- essayer("jeu_de_test", p, definir_modele(p, "PLS", x = x[-1], y = x[1], A = 2, test = numeros_observations(p)[seq(5, 120, by = 5)]))
essayer("plsda_jeu_de_test", p, definir_modele(p, "PLS-DA", x = complets, classe = uid("variete"), A = 3, test = numeros_observations(p)[seq(4, 120, by = 6)]))
essayer("acp_jeu_de_test", p, definir_modele(p, "PCA", x = x, A = 2, test = numeros_observations(p)[seq(3, 120, by = 7)]))
essayer("opls_jeu_de_test", p, definir_modele(p, "OPLS", x = c(complets, uid("polyphenols")), y = uid("note_gout"), A = 3, test = numeros_observations(p)[seq(2, 120, by = 8)]))
cat("Valeurs manquantes\n")
q <- p
q$donnees[c(5, 17, 40), as.character(uid("note_gout"))] <- NA
q$donnees[9, as.character(complets[1:4])] <- NA  # une observation sans aucune valeur X
essayer("y_manquants", q, definir_modele(q, "PLS", x = complets[1:4], y = uid("note_gout"), A = 2), permutations = TRUE)
essayer("acp_sans_valeur", q, definir_modele(q, "PCA", x = complets[1:4], A = 2))
essayer("opls_sans_valeur", q, definir_modele(q, "OPLS", x = complets[1:4], y = uid("note_gout"), A = 2))
essayer("opls_2y_manquants", q, definir_modele(q, "OPLS", x = c(complets[1:6], uid("calcium")), y = uid(c("note_gout", "jus")), A = 3))
essayer("manquants_jeu_de_test", q, definir_modele(q, "PLS", x = c(complets[1:6], uid("polyphenols")), y = uid("note_gout"), A = 2, test = c(9, 17, 33, 61)))
# termes croisés (faits dans pretraiter()) et valeurs manquantes : le DModX compte les termes présents, pas les colonnes du tableau
essayer("croises_manquants", q, definir_modele(q, "PLS", x = c(uid("variete"), complets[1:5]), y = uid("note_gout"), A = 2,
                                               croises = list(c(uid("variete"), complets[4]))))
cat("Sous-modèles de la validation croisée à court de variation\n")
# 8 observations : le sous-modèle sans le groupe 1 (6 observations) n'a que 5 composantes sur 6
set.seed(5)
pr <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:8), y = rnorm(8), classe = rep(c("a", "b"), 4),
                                       matrix(rnorm(80), 8, 10, dimnames = list(NULL, paste0("V", 1:10)))), "rang")
vr <- numeros(pr, paste0("V", 1:10))
essayer("pls_sous_modele_court", pr, definir_modele(pr, "PLS", x = vr, y = numeros(pr, "y"), A = 6))
essayer("acp_sous_modele_court", pr, definir_modele(pr, "PCA", x = vr, A = 6))
essayer("opls_sous_modele_court", pr, definir_modele(pr, "OPLS", x = vr, y = numeros(pr, "y"), A = 6))
essayer("oplsda_sous_modele_court", pr, definir_modele(pr, "OPLS-DA", x = vr, classe = numeros(pr, "classe"), A = 6), donnees = "csv")
# 2 composantes prédictives (2 Y, 3 classes)
set.seed(6)
pr <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:9), y1 = rnorm(9), y2 = rnorm(9), classe = rep(c("a", "b", "c"), 3),
                                       matrix(rnorm(90), 9, 10, dimnames = list(NULL, paste0("V", 1:10)))), "rang2")
vr <- numeros(pr, paste0("V", 1:10))
essayer("opls_2y_sous_modele_court", pr, definir_modele(pr, "OPLS", x = vr, y = numeros(pr, c("y1", "y2")), A = 7))
essayer("oplsda_3_classes_sous_modele_court", pr, definir_modele(pr, "OPLS-DA", x = vr, classe = numeros(pr, "classe"), A = 7))
# un Y qui ne varie que dans le groupe 1 (y : lignes 1 et 8) ou le groupe 2 (y2 : lignes 2 et 9) : le
# sous-modèle sans ce groupe n'a pas de composante pour lui ; une classe toute dans le groupe 1
set.seed(7)
pr <- projet_depuis_tableau(data.frame(nom = sprintf("O%02d", 1:14), y = c(1, rep(0, 6), 2, rep(0, 6)), y2 = c(0, 3, rep(0, 6), 1, rep(0, 5)),
                                       classe = c("c", rep(c("a", "b"), 3), "c", rep(c("a", "b"), 3)),
                                       matrix(rnorm(70), 14, 5, dimnames = list(NULL, paste0("V", 1:5)))), "groupe")
vr <- numeros(pr, paste0("V", 1:5))
essayer("pls_y_dans_un_groupe", pr, definir_modele(pr, "PLS", x = vr, y = numeros(pr, "y"), A = 1))
essayer("opls_y_dans_un_groupe", pr, definir_modele(pr, "OPLS", x = vr, y = numeros(pr, "y"), A = 2))
essayer("opls_2y_dans_un_groupe", pr, definir_modele(pr, "OPLS", x = vr, y = numeros(pr, c("y", "y2")), A = 3))
essayer("oplsda_classe_dans_un_groupe", pr, definir_modele(pr, "OPLS-DA", x = vr, classe = numeros(pr, "classe"), A = 3))
cat("Modèle d'un .usp, modèle de classe, modèle hiérarchique\n")
d <- definir_modele(p, "PLS-DA", x = complets, classe = uid("variete"), A = 3)
m0 <- ajuster(p, d)
d$solution <- list(T = t(m0$T) * c(-1, 1, -1))  # la solution enregistrée : deux composantes de l'autre signe
d$app <- NULL
d$number <- 9
d$reference <- list(ynames = sprintf("$M9.DA%d (variete=%s)", 1:4, c("Gala", "Golden", "Granny", "Reinette")))
r <- essayer("usp", p, d)
verifier("orienté comme la solution enregistrée", any(grepl("^signes <- c\\(-1, 1, -1\\)", lignes_script(r))))
essayer("modele_de_classe", p, modeles_classe(p, definir_modele(p, "PCA", x = x, A = 2), uid("variete"))[[2]])
h <- scores_en_variables(p, ajuster(p, p$models[[1]]))
essayer("hierarchique", h$projet, definir_modele(h$projet, "PLS", x = numeros(h$projet, h$noms), y = uid("note_gout"), A = 2))

cat("Spectres\n")
set.seed(3)
onde <- seq(1000, 1600, length.out = 40)
df <- data.frame(nom = sprintf("E%02d", 1:30), y = runif(30), t(vapply(1:30, function(i) sin(onde / 100 + i / 10) + rnorm(40, 0, 0.01), numeric(40))), check.names = FALSE)
names(df)[-(1:2)] <- sprintf("%.0f nm", onde)
s <- projet_depuis_tableau(df, "spectres")
r <- essayer("spectres", s, definir_modele(s, "PLS", x = numeros(s, names(df)[-(1:2)]), y = numeros(s, "y"), A = 2,
                                           spectres = list(list(type = "snv"), list(type = "sg", derivee = 1, fenetre = 7, degre = 2))))
verifier("spectres : seules les étapes du modèle", all(c("snv", "savitzky_golay") %in% sub(" <- function.*", "", grep("<- function", lignes_script(r), value = TRUE))) &&
         !any(grepl("^(msc|detrend|ligne_base) <- function", lignes_script(r))))
essayer("spectres_zones", s, definir_modele(s, "PLS", x = numeros(s, names(df)[-c(1:2, 15:22)]), y = numeros(s, "y"), A = 2,
                                            spectres = list(list(type = "ligne_base"), list(type = "detrend", degre = 2), list(type = "sg", derivee = 1, fenetre = 5, degre = 2))),
        donnees = "csv")
essayer("spectres_msc_test", s, definir_modele(s, "PLS", x = numeros(s, names(df)[-(1:2)]), y = numeros(s, "y"), A = 2,
                                               spectres = list(list(type = "msc"), list(type = "detrend", degre = 1)), test = 3:6))
noms_onde <- sprintf("%.0f", onde)
names(df)[-(1:2)] <- sprintf("v%d", seq_along(onde))  # des noms sans abscisse : leur place dans le tableau
s2 <- projet_depuis_tableau(df, "spectres")
essayer("spectres_sans_abscisse", s2, definir_modele(s2, "PCA", x = numeros(s2, names(df)[-(1:2)]), A = 2, spectres = list(list(type = "detrend", degre = 2))))

cat("Texte des données hors des chaînes R\n")
d <- p$models[[2]]
d$title <- "Ligne 1\nstop(\"code injecté\") `r 1 + 1`"
r <- essayer("titre", p, d)
verifier("un titre avec saut de ligne et `r ...` reste dans les commentaires", !any(grepl("^stop\\(", lignes_script(r))))

cat("En anglais\n")
r <- essayer("anglais", p, definir_modele(p, "OPLS-DA", x = c(complets, uid("polyphenols")), classe = uid("variete"), A = 4, test = numeros_observations(p)[seq(4, 120, by = 6)]),
             langue = "en", permutations = TRUE)
# un script en anglais : aucun nom du script en français, aucun commentaire en français (hors du
# titre du modèle et des données)
francais <- names(NOMS_SCRIPT_EN)[names(NOMS_SCRIPT_EN) != NOMS_SCRIPT_EN]
en_anglais <- function(texte) {
  texte <- texte[-(grep("^# 2\\. ", texte):(grep("^# 3\\. ", texte) - 1))][-1]
  code <- texte[nzchar(trimws(texte)) & !grepl("^\\s*#", texte)]
  commentaires <- sub("^[^#]*#", "", grep("#", texte, value = TRUE))
  !any(francais %in% symboles_code(code)) &&
    !any(grepl("\\b(les|des|une|chaque|composantes?|données|valeurs|termes?|résidus|modèle|prédit|sans|avec|pour|dans)\\b", commentaires, perl = TRUE)) &&
    !any(vapply(francais, function(n) any(grepl(sprintf("\\b%s\\b", n), commentaires, perl = TRUE)), TRUE))
}
autres <- list(definir_modele(q, "PCA", x = complets[1:4], A = 2), definir_modele(q, "PLS", x = c(complets[1:6], uid("polyphenols")), y = uid("note_gout"), A = 2, test = c(9, 17, 33, 61)),
               definir_modele(p, "OPLS", x = complets, y = uid(c("note_gout", "jus")), A = 4),
               definir_modele(p, "PLS-DA", x = c(uid("verger"), complets[1:6]), classe = uid("culture"), A = 2, croises = list(c(uid("verger"), complets[1])),
                              echelle = stats::setNames(c("Par", "Ctr", "none"), as.character(complets[1:3]))))
projets <- list(q, q, p, p)
ecarts <- character(0)
anglais <- vapply(seq_along(autres), function(k) {
  r <- ecrire_script(projets[[k]], autres[[k]], file.path(dossier, sprintf("anglais_%d.R", k)), "moteur", version = "essai", langue = "en", donnees = "csv")
  ecarts <<- c(ecarts, r$ecart)
  en_anglais(lignes_script(r))
}, TRUE)
r_sp <- ecrire_script(s, definir_modele(s, "PLS", x = numeros(s, sprintf("%.0f nm", onde)), y = numeros(s, "y"), A = 2,
                                        spectres = list(list(type = "snv"), list(type = "msc"), list(type = "ligne_base"), list(type = "detrend", degre = 2),
                                                        list(type = "sg", derivee = 1, fenetre = 5, degre = 2))),
                      file.path(dossier, "anglais_spectres.R"), "moteur", version = "essai", langue = "en")
ms <- lapply(p$models, function(d) ajuster(p, d))
r_seance <- ecrire_script_seance(p, p$models, ms, file.path(dossier, "anglais_seance.R"), "moteur", version = "essai", langue = "en")
verifier("ni nom ni commentaire en français (OPLS-DA et jeu de test, ACP et PLS avec valeurs manquantes, OPLS à 2 Y, termes croisés et échelles mêlées, spectres, séance)",
         en_anglais(lignes_script(r)) && all(anglais) && en_anglais(lignes_script(r_sp)) && en_anglais(lignes_script(r_seance)) &&
           !length(ecarts) && is.null(r_sp$ecart) && is.null(r_seance$ecart))

cat("Toute la séance dans un script\n")
dir.create(ici <- file.path(dossier, "seance"))
definitions <- c(p$models, list(definir_modele(p, "OPLS", x = complets, y = uid("note_gout"), A = 2, nom = "m"),
                                definir_modele(p, "PLS", x = x[-1], y = x[1], A = 2, test = numeros_observations(p)[seq(5, 120, by = 5)], nom = "M6")))
ms <- lapply(definitions, function(d) ajuster(p, d))
r <- ecrire_script_seance(p, definitions, ms, file.path(ici, "seance.R"), "moteur", version = "essai", donnees = "csv", rdata = TRUE)
texte <- lignes_script(r)
noms <- sub(" <- list\\(.*", "", grep("^[A-Za-z][A-Za-z0-9_.]* <- list\\(m = ", texte, value = TRUE))
s <- lancer(r$script, noms, sprintf(paste("permutations(function(Yp) { s <- M2$pretraiter(M2$X, Yp, seq_len(nrow(Yp)));",
                                           "list(pls(s$X, s$Y, %d), validation_croisee_pls(M2$X, Yp, %d, M2$pretraiter, G = 7)) }, M2$Y, n = 3)"), ms[[2]]$A, ms[[2]]$A))
memes <- vapply(seq_along(noms), function(k) {
  a <- moteur(ms[[k]])
  x <- s[[noms[k]]]
  meme(a$scores, x$m$T) && meme(a[["R2, Q2"]], x$composantes) && meme(a$T2, x$observations$T2) && meme(a$DModX, x$observations$DModX) &&
    meme(a$limites, x$limites) && meme(a$VIP, x$variables$VIP) && meme(a$coefficients, x$coefficients) && meme(a[["CV-ANOVA"]], cv(x$cv_anova)) &&
    meme(a$classification, x$classification) && meme(a[["jeu de test"]], if (!is.null(x$test)) x$test[-1])
}, TRUE)
verifier(sprintf("%d modèles, chacun sous son nom (%s) : les mêmes résultats que le moteur", length(noms), paste(noms, collapse = ", ")),
         is.null(r$ecart) && length(noms) == length(definitions) && all(memes) && noms[5] == "m_modele")
attendu <- permutations(p, ms[[2]], definitions[[2]], n = 3)
verifier("le test de permutation d'un modèle de la séance", meme(s$suite$points, attendu$points[c("correlation", "R2", "Q2")]))
verifier("les fonctions n'y sont qu'une fois", sum(grepl("^pls <- function", texte)) == 1 && sum(grepl("^# 1\\. ", texte)) == 1)
e <- new.env(); load(r$chemins[3], envir = e)
verifier(".RData : données et chaque modèle", all(c("donnees", noms) %in% ls(e)))

cat("Rapports R Markdown et Quarto\n")
for (i in seq_along(p$models)) {
  d <- p$models[[i]]
  ext <- if (i %% 2) "Rmd" else "qmd"
  dir.create(ici <- file.path(dossier, paste0("rapport", i)))
  r <- ecrire_script(p, d, file.path(ici, paste0(d$name, ".", ext)), "moteur", version = "essai", langue = if (i %% 2) "fr" else "en",
                     donnees = if (i > 2) "csv" else "script")
  texte <- readLines(r$script, encoding = "UTF-8")
  verifier(sprintf("%s.%s : en-tête, méthodes, code du modèle, références", d$name, ext), texte[1] == "---" && any(grepl("^## (Méthodes|Methods)", texte)) &&
           any(grepl("^```\\{r modele\\}", texte)) && any(grepl("^- .*Wold|^- .*Barker|^- .*Trygg", texte)) && is.null(r$ecart))
  if (requireNamespace("knitr", quietly = TRUE)) {  # compilé avec knitr dans une session neuve (sans pandoc : en Markdown)
    compiler <- file.path(dossier, "compiler.R")
    # (le nom du .md donné : le knitr d'Ubuntu 22.04, 1.37, ne reconnaît pas l'extension .qmd)
    writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "a <- commandArgs(TRUE)", "setwd(dirname(a[1]))",
                 "knitr::knit(basename(a[1]), output = sub('[.][Rq]md$', '.md', basename(a[1])), quiet = TRUE, envir = new.env())"), compiler)
    messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(compiler), shQuote(r$script)), stdout = TRUE, stderr = TRUE))
    md <- file.path(ici, paste0(d$name, ".md"))
    verifier(sprintf("%s.%s : se compile, avec ses graphiques", d$name, ext), file.exists(md) && !any(grepl("^## Error|Error in", readLines(md, encoding = "UTF-8"))) &&
             length(list.files(file.path(ici, "figure"), "\\.png$")) >= 3)
  } else {  # sans knitr : le code de ses blocs R tourne dans une session neuve (sans les tableaux de knitr::kable)
    debuts <- grep("^```\\{r", texte)
    fins <- vapply(debuts, function(i) i + match("```", texte[-seq_len(i)]), 0)
    code <- unlist(lapply(seq_along(debuts), function(k) texte[seq(debuts[k] + 1, length.out = fins[k] - debuts[k] - 1)]))
    blocs <- file.path(ici, "blocs.R")
    writeLines(enc2utf8(c("pdf(NULL)", code[!grepl("knitr::", code)])), blocs, useBytes = TRUE)
    avant <- setwd(ici)
    messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(blocs)), stdout = TRUE, stderr = TRUE))
    setwd(avant)
    verifier(sprintf("%s.%s : le code de ses blocs R tourne (knitr n'est pas installé : pas compilé)", d$name, ext), is.null(attr(messages, "status")))
  }
}

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
