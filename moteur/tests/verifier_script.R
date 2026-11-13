# Vérifie le script R autonome (moteur/R/script.R). Pour chaque sorte de modèle que le moteur
# ajuste (ACP, PLS à un ou plusieurs Y, PLS-DA, OPLS, OPLS-DA à deux classes et plus) et ses
# options (transformations, mises à l'échelle par variable, termes qualitatifs, croisés et carrés,
# spectres, valeurs manquantes, jeu de test, modèle d'un .usp orienté comme sa solution
# enregistrée, modèle hiérarchique, sous-modèles de la validation croisée à court de variation),
# le script est écrit, lancé dans une session R neuve
# (Rscript --vanilla, sans le moteur) depuis un autre dossier, et ses résultats sont comparés à
# ceux du moteur : scores, R2 et Q2, T2, DModX et leurs limites, VIP, coefficients, CV-ANOVA,
# classification, jeu de test, test de permutation. Puis : le script d'une PLS simple est court
# et n'a pas de fonction inutile ; la séance en un script ; l'export en anglais ; les régressions
# (linéaire, mixte avec effets emboîtés et CAR1, GLM de Poisson avec un effort, binomial et gamma,
# GAMM binomial négatif, GAM de Tweedie avec un jeu de test : coefficients, AIC, R², variances,
# paramètres des lois, courbes, validation croisée, valeurs ajustées et résidus, résidus quantiles,
# jeu de test ; la sélection par l'AIC, toutes les combinaisons ou l'élimination descendante ; le
# modèle mixte de la vue BACI, avec CAR1 ou une fin), en séance et en rapport ; les rapports R
# Markdown et Quarto (compilés avec knitr s'il est installé).
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
# groupes de validation croisée par valeur d'une variable (un groupe par verger), pondération par
# blocs et modificateurs (pretraitement.R : facteurs_ponderation)
vc_verger <- list(by = "variable", variable = uid("verger"))
blocs <- stats::setNames(c("taille", "taille", "gout", "gout", "gout"), as.character(complets[1:5]))
r <- essayer("blocs_groupes", p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2, vc = vc_verger, blocs = blocs,
                                                modificateurs = stats::setNames(c(0.5, 3), as.character(complets[c(4, 11)]))), permutations = TRUE)
texte <- lignes_script(r)
verifier("ses groupes de validation croisée et sa pondération y sont écrits",
         any(grepl("^groupes_vc <- ", texte)) && any(grepl("groupe = groupes_vc", texte)) && any(grepl("^  poids_x <- poids_x \\* ", texte)))
essayer("acp_groupes_blocs", p, definir_modele(p, "PCA", x = complets, A = 3, vc = vc_verger, blocs = blocs))
essayer("opls_groupes", p, definir_modele(p, "OPLS", x = complets, y = uid("note_gout"), A = 3, vc = vc_verger), permutations = TRUE)
essayer("plsda_groupes_test", p, definir_modele(p, "PLS-DA", x = complets, classe = uid("variete"), A = 2, vc = vc_verger,
                                                test = numeros_observations(p)[seq(4, 120, by = 6)], blocs = blocs[1:2]))
# une transformation de type 7 (puissance), et des valeurs hors du domaine du log : manquantes
q <- p
q$donnees[c(2, 30), as.character(complets[1])] <- c(0, -3)
d <- definir_modele(q, "PLS", x = complets, y = uid("note_gout"), A = 2, transformations = stats::setNames("log10", as.character(complets[1])))
d$x[[2]]$transform <- list(type = 7, params = c(NA, 2, 1, 1.5))
essayer("hors_domaine", q, d)
# transformations personnalisées (Nouveau modèle), dans leur domaine puis en dehors (manquantes)
essayer("transformations_personnalisees", p, definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2,
  transformations = stats::setNames(c("ln:1:1", "puissance:2:1:0.33", "neglog:0.5:2", "lineaire:-1:300", "exp:0.01:0", "logit:0:40", "log10:2:0.5"), as.character(complets[1:7]))))
essayer("transformations_hors_domaine", q, definir_modele(q, "PLS", x = complets, y = uid("note_gout"), A = 2,
  transformations = stats::setNames(c("logit:0:150", "ln:1:0"), as.character(complets[1:2]))))
q2 <- q
q2$donnees[c(4, 9), as.character(complets[2])] <- c(0, -1)
essayer("transformations_hors_domaine_ln", q2, definir_modele(q2, "PLS", x = complets, y = uid("note_gout"), A = 2,
  transformations = stats::setNames(c("ln:1:0", "neglog:1:0"), as.character(complets[2:3]))))
# termes à trois facteurs (A × B × C, une variable qualitative par ses catégories) et cube
essayer("trois_facteurs", p, definir_modele(p, "PLS", x = c(uid("variete"), complets[1:4]), y = uid("note_gout"), A = 2,
  croises = list(c(uid("variete"), complets[1], complets[2]), c(complets[3], complets[3], complets[3]))))
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
                              echelle = stats::setNames(c("Par", "Ctr", "none"), as.character(complets[1:3]))),
               definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2, vc = vc_verger, blocs = blocs,
                              modificateurs = stats::setNames(2, as.character(complets[7]))))
projets <- list(q, q, p, p, p)
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
                                definir_modele(p, "PLS", x = x[-1], y = x[1], A = 2, test = numeros_observations(p)[seq(5, 120, by = 5)], nom = "M6"),
                                definir_modele(p, "PLS", x = complets, y = uid("note_gout"), A = 2, vc = vc_verger, blocs = blocs, nom = "M7")))
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
verifier("un modèle à groupes par verger garde les siens (M7$groupes_vc), y compris dans l'appel du test de permutation",
         any(grepl("groupe = M7\\$groupes_vc", texte)) && any(grepl("^groupes_vc <- ", texte)))
e <- new.env(); load(r$chemins[3], envir = e)
verifier(".RData : données et chaque modèle", all(c("donnees", noms) %in% ls(e)))

cat("Régressions (lm, nlme, mgcv)\n")
# Ce que le moteur donne d'une régression, et ce que son script a calculé (sous ses noms, dans sa langue).
ATTENDUS_REGRESSION <- c("coefficients", "criteres", "r2", "variances", "phi", "parametre_loi", "dispersion", "courbes", "seuils", "vc", "observations", "test", "selection")
COURBES_SEUILS <- c("x", "effet", "bas", "haut", "x_pente", "pente", "pente_bas", "pente_haut")
moteur_regression <- function(m) {
  r <- m$resultat
  seuils <- if (!is.null(r$courbes)) lapply(r$courbes$terme, function(t) seuils_courbe(m, t))
  list(coefficients = as.matrix(r$coefficients[c("estimation", "erreur_type", "ddl", "t", "p", "bas", "haut")]),
       # (les seuils d'une courbe : sa pente et son effet, leurs bandes simultanées, leurs plages nettes)
       "seuils des courbes" = if (length(seuils)) unlist(lapply(seuils, `[`, COURBES_SEUILS)),
       "plages des seuils" = if (length(seuils)) unlist(lapply(seuils, function(s) { z <- rbind(s$plages_pente, s$plages_effet); c(z$de, z$a) })),
       "AIC et vraisemblance" = c(r$logLik, r$parametres, r$AIC, r$AICc, r$BIC), R2 = unlist(r$r2), variances = r$variances$variance,
       phi = r$phi, "paramètre de la loi" = r$parametre_loi$valeur, dispersion = r$dispersion, courbes = if (!is.null(r$courbes)) as.matrix(r$courbes[-1]),
       "Q2 et RMSEcv" = c(m$vc$Q2, m$vc$RMSEcv), "prédictions de la validation croisée" = m$vc$prediction,
       "valeurs ajustées et résidus" = cbind(r$ajuste, r$residus), "résidus quantiles" = r$residus_quantiles,
       "jeu de test" = if (!is.null(m$test)) m$test$statistiques[c("n", "RMSEP", "biais", "SEP", "Q2ext")],
       # (la sélection par l'AIC, quand elle a été faite : le classement et l'importance des groupes)
       "sélection par l'AIC" = if (!is.null(m$selection_aic)) as.matrix(m$selection_aic$modeles[c("k", "logLik", "AICc", "delta", "poids", "poids_cumule")]),
       "importance des groupes" = m$selection_aic$importance$importance)
}
script_regression <- function(s, langue) {
  n <- function(x) s[[nom_script(x, langue)]]
  colonnes <- function(x, k) if (!is.null(x)) as.matrix(x[k])
  seuils <- n("seuils")
  list(coefficients = colonnes(n("coefficients"), 2:8),
       "seuils des courbes" = if (length(seuils)) unlist(lapply(seuils, `[`, nom_script(COURBES_SEUILS, langue))),
       "plages des seuils" = if (length(seuils)) unlist(lapply(seuils, function(s) { z <- s[[nom_script("plages", langue)]]; c(z[[3]], z[[4]]) })),
       "AIC et vraisemblance" = n("criteres"), R2 = n("r2"), variances = colonnes(n("variances"), 2),
       phi = n("phi"), "paramètre de la loi" = n("parametre_loi"), dispersion = n("dispersion"), courbes = if (!is.null(n("courbes"))) colonnes(n("courbes"), seq_along(n("courbes"))[-1]),
       "Q2 et RMSEcv" = c(n("vc")$Q2, n("vc")$RMSEcv), "prédictions de la validation croisée" = n("vc")$prediction,
       "valeurs ajustées et résidus" = colonnes(n("observations"), 3:4), "résidus quantiles" = n("observations")[[nom_script("residu_quantile", langue)]],
       "jeu de test" = colonnes(n("test"), 2:6),
       "sélection par l'AIC" = colonnes(n("selection")[[nom_script("modeles", langue)]], 2:7),
       "importance des groupes" = if (!is.null(n("selection"))) unname(n("selection")[[nom_script("importance", langue)]]))
}
# Les blocs de code d'un rapport ont chacun leur nom : knitr refuse d'en compiler deux du même nom
# (vérifié ici même sans knitr).
etiquettes_uniques <- function(texte) {
  etiquettes <- sub("^```\\{r ([^,} ]+).*$", "\\1", grep("^```\\{r [^,} ]", texte, value = TRUE))
  !anyDuplicated(etiquettes)
}
# Écrit le script d'une régression, le lance dans une session neuve et compare ses résultats à ceux du moteur.
# `selection` : la sélection par l'AIC faite d'abord (regression.R : selection_aic), que le script refait.
essayer_regression <- function(nom, p, d, langue = "fr", selection = FALSE, ...) {
  m <- ajuster(p, d)
  if (!isFALSE(selection)) m$selection_aic <- selection_aic(m, d, if (is.numeric(selection)) selection else 256)  # (un nombre : la limite des combinaisons)
  dir.create(ici <- file.path(dossier, nom))
  r <- ecrire_script(p, d, file.path(ici, paste0(nom, ".R")), "moteur", m = m, version = "essai", langue = langue, ...)
  s <- lancer(r$script, nom_script(ATTENDUS_REGRESSION, langue))
  if (!length(s)) {
    verifier(sprintf("%s : le script tourne (%s)", nom, paste(utils::tail(attr(s, "messages"), 3), collapse = " / ")), FALSE)
    return(invisible(r))
  }
  a <- moteur_regression(m)
  b <- script_regression(s, langue)
  differents <- names(a)[!vapply(names(a), function(k) meme(a[[k]], b[[k]]), TRUE)]
  presents <- names(a)[!vapply(a, is.null, TRUE)]
  verifier(sprintf("%s : les mêmes %s que le moteur%s%s", nom, paste(setdiff(presents, differents), collapse = ", "),
                   if (length(differents)) sprintf(" ; DIFFÉRENTS : %s", paste(differents, collapse = ", ")) else "",
                   if (!is.null(r$ecart)) sprintf(" ; écart vu par le moteur : %s", r$ecart) else ""),
           !length(differents) && is.null(r$ecart))
  invisible(r)
}
# des sites (un identifiant des observations), deux secteurs par site, huit années (une manque),
# des comptages avec un effort, des présences-absences, des densités avec des zéros
set.seed(11)
n <- 96
tr <- data.frame(nom = sprintf("O%03d", 1:n), secteur = rep(rep(c("a", "b"), each = 8), 6), annee = rep(c(2001:2005, 2007:2009), 12),
                 x1 = rnorm(n), x2 = runif(n), sol = sample(c("argile", "sable", "limon"), n, TRUE), effort = runif(n, 1, 4))
site <- rep(sprintf("S%d", 1:6), each = 16)
effet_site <- rep(rnorm(6, 0, 0.6), each = 16)
tr$hauteur <- 1 + 0.6 * tr$x1 - 0.8 * tr$x2 + effet_site + rep(rnorm(12, 0, 0.3), each = 8) + as.vector(stats::filter(rnorm(n, 0, 0.5), 0.5, "recursive"))
tr$compte <- stats::rnbinom(n, mu = exp(0.5 + 0.4 * tr$x1 + sin(3 * tr$x2) + effet_site / 2) * tr$effort, size = 3)
tr$presence <- as.numeric(runif(n) < stats::plogis(-0.3 + 1.1 * tr$x1))
tr$densite <- ifelse(runif(n) < 0.3, 0, stats::rgamma(n, shape = 2, rate = 2 / exp(0.2 + 0.5 * tr$x2)))
tr$masse <- stats::rgamma(n, shape = 3, rate = 3 / exp(0.3 + 0.4 * tr$x2))  # des valeurs positives (loi gamma)
tr$x1[c(7, 40)] <- NA  # deux observations sans x1 : laissées de côté
pr <- projet_depuis_tableau(tr, "terrain")
pr$observation_id_names <- c("Observation", "site")
for (i in seq_along(pr$observations)) pr$observations[[i]]$ids <- c(pr$observations[[i]]$ids[1], site[i])
ur <- function(...) numeros(pr, c(...))
par_site <- list(by = "id", id = "site")
par_secteur <- list(by = "variable", variable = ur("secteur"))
r <- essayer_regression("lm_qualitatives_croises", p, definir_modele(p, "LM", x = uid(c("variete", "sucre", "fermete", "polyphenols")), y = uid("note_gout"),
                        echelle = stats::setNames(c("none", "UV"), c("defaut", as.character(uid("sucre")))), croises = list(uid(c("sucre", "fermete"))),
                        transformations = stats::setNames(c("log10", "racine2"), as.character(uid(c("fermete", "note_gout"))))), donnees = "csv")
code <- code_hors_donnees(lignes_script(r))
verifier("une régression linéaire n'utilise que R (ni library() ni paquet)", !any(grepl("library\\(|require\\(|::", code)))
essayer_regression("lm_groupes_verger", p, definir_modele(p, "LM", x = uid(c("sucre", "acidite")), y = uid("note_gout"), echelle = "none", vc = vc_verger))
# (sans X : le modèle nul, qui sert à comparer les AIC)
essayer_regression("lm_nul", p, definir_modele(p, "LM", x = numeric(0), y = uid("note_gout"), vc = vc_verger))
essayer_regression("lmm_nul", p, definir_modele(p, "LMM", x = numeric(0), y = uid("note_gout"), aleatoires = list(vc_verger)))
r <- essayer_regression("lmm_emboites_car1_test", pr, definir_modele(pr, "LMM", x = ur("x1", "x2", "sol"), y = ur("hauteur"), echelle = "none",
                        aleatoires = list(par_site, par_secteur), correlation = list(type = "CAR1", temps = list(by = "variable", variable = ur("annee"))),
                        test = numeros_observations(pr)[seq(3, n, by = 12)]))
texte <- lignes_script(r)
verifier("modèle mixte : ses sites (un identifiant) dans le tableau, nlme::lme, CAR1, groupes de validation par site",
         any(grepl("^  site = ", texte)) && any(grepl("nlme::lme\\(", texte)) && any(grepl("corCAR1", texte)) &&
           any(grepl("^groupes_vc <- as.integer\\(cadre\\[\\[\"site\"\\]\\]\\)", texte)))
# (les mêmes données, les lignes dans l'ordre des années : les résidus normalisés du script, calculés
# dans l'ordre du temps de chaque groupe, sont ceux du moteur ; ceux de nlme s'en écarteraient)
oan <- order(tr$annee, site, tr$secteur)
pra <- projet_depuis_tableau(tr[oan, ], "terrain par année")
pra$observation_id_names <- c("Observation", "site")
for (i in seq_along(pra$observations)) pra$observations[[i]]$ids <- c(pra$observations[[i]]$ids[1], site[oan][i])
ura <- function(...) numeros(pra, c(...))
car1_an <- list(type = "CAR1", temps = list(by = "variable", variable = ura("annee")), dans = list(by = "variable", variable = ura("secteur")))
essayer_regression("lmm_car1_lignes_par_annee", pra, definir_modele(pra, "LMM", x = ura("x1", "x2"), y = ura("hauteur"), echelle = "none",
                   aleatoires = list(par_site), correlation = car1_an))
essayer_regression("gamm_car1_lignes_par_annee_en", pra, definir_modele(pra, "GAM", x = ura("x1", "x2"), y = ura("hauteur"), echelle = "none", famille = "gaussienne",
                   lisses = ura("x2"), aleatoires = list(par_site), correlation = car1_an), langue = "en")
# (les scripts de seuils : un site aléatoire, l'autocorrélation le long des années dans chaque secteur)
car1_secteur <- list(type = "CAR1", temps = list(by = "variable", variable = ur("annee")), dans = par_secteur)
r <- essayer_regression("gamm_car1_secteur", pr, definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("hauteur"), echelle = "none", famille = "gaussienne",
                        lisses = ur("x2"), k_courbes = stats::setNames(list(5), ur("x2")), aleatoires = list(par_site), correlation = car1_secteur))
code <- code_hors_donnees(lignes_script(r))
verifier("modèle additif avec autocorrélation : mgcv::gamm, l'autocorrélation dans chaque secteur de chaque site, la courbe de dimension 5",
         any(grepl("gamm(f, random = aleatoire, correlation = correlation", code, fixed = TRUE)) && any(grepl("~ annee | site / secteur", code, fixed = TRUE)) &&
           any(grepl("\"x2\" = 5L", code, fixed = TRUE)) && any(grepl("library(mgcv)", code, fixed = TRUE)))
essayer_regression("gamm_car1_test_en", pr, definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("hauteur"), echelle = "none", famille = "gaussienne",
                   lisses = ur("x2"), aleatoires = list(par_site), correlation = car1_secteur, test = numeros_observations(pr)[seq(5, n, by = 16)]), langue = "en")
essayer_regression("lmm_car1_secteur", pr, definir_modele(pr, "LMM", x = ur("x1", "x2"), y = ur("hauteur"), echelle = "none", aleatoires = list(par_site),
                   correlation = car1_secteur))
r <- essayer_regression("glm_poisson_effort", pr, definir_modele(pr, "GLM", x = ur("x1", "sol"), y = ur("compte"), famille = "poisson", decalage = ur("effort"), echelle = "none"))
verifier("GLM de Poisson : effort en décalage, effet multiplicatif, surdispersion", any(grepl("offset\\(log\\(effort\\)\\)", lignes_script(r))) &&
         any(grepl("effet_multiplicatif", lignes_script(r))) && any(grepl("^dispersion <- ", lignes_script(r))))
verifier("GLM de Poisson : ses résidus quantiles (residus_quantiles, de ppois à ses valeurs ajustées) et leur graphique",
         any(grepl("^residus_quantiles <- function", lignes_script(r))) &&
           any(grepl("observations$residu_quantile <- residus_quantiles(cadre[[1]], function(q, ...) ppois(q, fitted(m), ...))", lignes_script(r), fixed = TRUE)) &&
           any(grepl("qqnorm(observations$residu_quantile", lignes_script(r), fixed = TRUE)))
r <- essayer_regression("glm_gamma_en", pr, definir_modele(pr, "GLM", x = ur("x1", "sol"), y = ur("masse"), famille = "gamma", echelle = "none"), langue = "en")
verifier("GLM gamma en anglais : ses résidus quantiles sans tirage (discrete = FALSE), ni nom ni commentaire en français",
         any(grepl("quantile_residuals(model_data[[1]], function(q, ...) pgamma(", lignes_script(r), fixed = TRUE)) &&
           any(grepl("discrete = FALSE)", lignes_script(r), fixed = TRUE)) && en_anglais(lignes_script(r)))
essayer_regression("glm_binomiale", pr, definir_modele(pr, "GLM", x = ur("x1", "sol"), y = ur("presence"), famille = "binomiale", echelle = "none"))
r <- essayer_regression("gamm_binomiale_negative", pr, definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("compte"), famille = "binomiale_negative",
                        lisses = ur("x2"), decalage = ur("effort"), aleatoires = list(par_site, par_secteur), echelle = "none"), langue = "en")
verifier("GAMM en anglais : mgcv attaché, une courbe, les secteurs emboîtés dans les sites, ni nom ni commentaire en français",
         any(grepl("^library\\(mgcv\\)", lignes_script(r))) && any(grepl("site_secteur", lignes_script(r))) && en_anglais(lignes_script(r)))
r <- essayer_regression("gam_tweedie_test", pr, definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("densite"), famille = "tweedie", lisses = ur("x2"), echelle = "none",
                                                               test = numeros_observations(pr)[seq(5, n, by = 10)]))
verifier("Tweedie : pas de résidus quantiles (les paquets livrés avec R n'ont pas sa fonction de répartition)", !any(grepl("residus_quantiles|residu_quantile", lignes_script(r))))
# la sélection par l'AIC faite dans Nuée : le script la refait (toutes les combinaisons, ou l'élimination descendante)
r <- essayer_regression("lm_selection_aic", p, definir_modele(p, "LM", x = uid(c("variete", "sucre", "fermete", "polyphenols")), y = uid("note_gout"),
                        echelle = "none", croises = list(uid(c("sucre", "fermete")))), selection = TRUE)
verifier("sélection par l'AIC : sa fonction, ses groupes (un terme croisé avec ses effets principaux), son tableau dans la sortie",
         any(grepl("^selection_aic <- function", lignes_script(r))) && any(grepl("\"sucre*fermete\" = c(\"sucre\", \"fermete\")", lignes_script(r), fixed = TRUE)) &&
           any(grepl("print(head(selection$modeles, 20))", lignes_script(r), fixed = TRUE)))
r <- essayer_regression("lmm_car1_selection_en", pr, definir_modele(pr, "LMM", x = ur("x1", "x2", "sol"), y = ur("hauteur"), echelle = "none", aleatoires = list(par_site),
                        correlation = car1_secteur), langue = "en", selection = TRUE)
verifier("sélection par l'AIC en anglais : ni nom ni commentaire en français", en_anglais(lignes_script(r)) && any(grepl("^aic_selection <- function", lignes_script(r))))
essayer_regression("gam_selection_descendante", pr, definir_modele(pr, "GAM", x = ur("x1", "x2", "sol"), y = ur("compte"), famille = "binomiale_negative",
                   lisses = ur("x2"), decalage = ur("effort"), aleatoires = list(par_site), echelle = "none"), selection = 4)
essayer_regression("gamm_selection", pr, definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("hauteur"), echelle = "none", famille = "gaussienne",
                   lisses = ur("x2"), aleatoires = list(par_site), correlation = car1_secteur), selection = TRUE)
# Ajuster ce modèle (sélection par l'AIC) : le modèle choisi garde les lignes du modèle complet (sans
# x1, ou une densité nulle que son log10 rend manquante) ; son script les laisse de côté de même
dcomplet <- definir_modele(pr, "LM", x = ur("x1", "x2", "densite"), y = ur("hauteur"), echelle = "none", nom = "M1",
                           transformations = stats::setNames(list("log10"), ur("densite")))
mcomplet <- ajuster(pr, dcomplet)
dchoisi <- definition_selection(dcomplet, mcomplet, "x2")
r <- essayer_regression("lm_choisi_selection", pr, dchoisi)
verifier(sprintf("le modèle choisi par la sélection : les %d lignes du modèle complet ; le script laisse de côté les %d autres, en le disant",
                 mcomplet$N, length(dcomplet$observations) - mcomplet$N),
         ajuster(pr, dchoisi)$N == mcomplet$N && mcomplet$N < length(dcomplet$observations) - 2 &&
           any(grepl("^connu\\[hors_selection\\] <- FALSE", lignes_script(r))) && any(grepl("sélection par l'AICc du modèle", lignes_script(r))))
r <- essayer_regression("lm_choisi_selection_en", pr, dchoisi, langue = "en")
verifier("le modèle choisi, en anglais : ni nom ni commentaire en français", en_anglais(lignes_script(r)) &&
           any(grepl("^known\\[outside_selection\\] <- FALSE", lignes_script(r))))
# le modèle mixte de la vue BACI (comparaisons.R : modele_baci) : ses deux variables 0/1 dans le
# tableau, leur terme croisé, le secteur aléatoire et CAR1 dans chaque secteur ; ses lignes, celles
# de l'impact et des témoins (jusqu'à la fin), une densité manquante laissée de côté
set.seed(13)
tb <- expand.grid(annee = 2001:2014, secteur = c("S1", "S2", "S3", "S4"), stringsAsFactors = FALSE)
tb$densite <- round(5 + c(S1 = 0.4, S2 = -0.6, S3 = 0.3, S4 = 0)[tb$secteur] + 1.5 * (tb$secteur == "S1" & tb$annee >= 2008) +
                      unlist(lapply(1:4, function(k) as.vector(stats::filter(rnorm(14, 0, 0.5), 0.6, "recursive")))), 3)
tb$densite[tb$secteur == "S1" & tb$annee == 2004] <- NA
pbaci <- projet_depuis_tableau(data.frame(nom = paste0("m", seq_len(nrow(tb))), tb[sample(nrow(tb)), c("secteur", "annee", "densite")]), "baci")
ub <- function(nom) numeros(pbaci, nom)
bb <- modele_baci(pbaci, ub("densite"), ub("annee"), ub("secteur"), "S1", c("S2", "S3"), 2008, car1 = TRUE)
r <- essayer_regression("lmm_baci_car1", bb$projet, bb$definition)
code <- code_hors_donnees(lignes_script(r))
verifier("le modèle mixte BACI : nlme::lme avec CAR1 dans chaque secteur, le terme croisé de la période et de l'impact, les lignes de l'impact et des témoins",
         any(grepl("corCAR1", code, fixed = TRUE)) && any(grepl("période BACI (2008)*impact BACI (S1 contre S2, S3)", lignes_script(r), fixed = TRUE)) &&
           ajuster(bb$projet, bb$definition)$N == sum(tb$secteur %in% c("S1", "S2", "S3") & !is.na(tb$densite)))
options(nuee.langue = "en")
bbf <- modele_baci(pbaci, ub("densite"), ub("annee"), ub("secteur"), "S1", c("S2", "S3"), 2008, fin = 2012)
options(nuee.langue = NULL)
essayer_regression("lmm_baci_fin_en", bbf$projet, bbf$definition, langue = "en")
# la séance : une ACP, un modèle mixte et un GAM dans le même script
definitions <- list(definir_modele(pr, "PCA", x = ur("x1", "x2", "effort"), A = 2, nom = "A1"),
                    definir_modele(pr, "LMM", x = ur("x1", "x2"), y = ur("hauteur"), echelle = "none", aleatoires = list(par_site), nom = "R1"),
                    definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("densite"), famille = "tweedie", lisses = ur("x2"), echelle = "none", nom = "R2"))
ms <- lapply(definitions, function(d) ajuster(pr, d))
dir.create(ici <- file.path(dossier, "seance_regressions"))
r <- ecrire_script_seance(pr, definitions, ms, file.path(ici, "seance.R"), "moteur", version = "essai")
s <- lancer(r$script, c("A1", "R1", "R2"))
verifier("une séance avec une ACP, un modèle mixte et un GAM : chacun sous son nom, les résultats du moteur",
         is.null(r$ecart) && length(s) == 3 && meme(moteur(ms[[1]])$scores, s$A1$m$T) &&
           all(vapply(2:3, function(k) meme(moteur_regression(ms[[k]])$coefficients, as.matrix(s[[k]]$coefficients[2:8])) &&
                        meme(moteur_regression(ms[[k]])[["Q2 et RMSEcv"]], c(s[[k]]$vc$Q2, s[[k]]$vc$RMSEcv)), TRUE)))
# la séance d'un GAM à courbe, puis d'une régression sans courbe : les seuils comparés au seul GAM
definitions <- list(definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("hauteur"), famille = "gaussienne", lisses = ur("x2"), echelle = "none", nom = "R5"),
                    definir_modele(pr, "LM", x = ur("x1", "x2"), y = ur("hauteur"), echelle = "none", nom = "R6"),
                    definir_modele(pr, "GLM", x = ur("x1", "x2"), y = ur("compte"), famille = "poisson", echelle = "none", nom = "R7"))
ms <- lapply(definitions, function(d) ajuster(pr, d))
dir.create(ici <- file.path(dossier, "seance_courbe_d_abord"))
r <- tryCatch(ecrire_script_seance(pr, definitions, ms, file.path(ici, "seance.R"), "moteur", version = "essai", renv = TRUE), error = function(e) e)
entete <- if (!inherits(r, "error")) utils::head(readLines(r$script, encoding = "UTF-8"), 15)
lock <- if (!inherits(r, "error")) jsonlite::fromJSON(r$chemins[grepl("renv\\.lock$", r$chemins)])
verifier("une séance où un GAM à courbe précède une régression et un GLM sans courbe : écrite, sans écart ; mgcv et MASS nommés et dans renv.lock",
         !inherits(r, "error") && is.null(r$ecart) && any(grepl("paquets mgcv et MASS", entete)) && all(c("mgcv", "MASS") %in% names(lock$Packages)))
# les rapports d'une régression : méthodes et références, et ils se compilent
for (k in 1:2) {
  d <- list(definir_modele(pr, "LMM", x = ur("x1", "x2"), y = ur("hauteur"), echelle = "none", aleatoires = list(par_site, par_secteur), nom = "R3"),
            definir_modele(pr, "GAM", x = ur("x1", "x2"), y = ur("compte"), famille = "binomiale_negative", lisses = ur("x2"), decalage = ur("effort"), nom = "R4"))[[k]]
  ext <- c("Rmd", "qmd")[k]
  dir.create(ici <- file.path(dossier, paste0("rapport_regression", k)))
  r <- ecrire_script(pr, d, file.path(ici, paste0(d$name, ".", ext)), "moteur", version = "essai", langue = c("fr", "en")[k])
  texte <- readLines(r$script, encoding = "UTF-8")
  verifier(sprintf("%s.%s : méthodes, code du modèle, coefficients, références", d$name, ext), texte[1] == "---" && any(grepl("^## (Méthodes|Methods)", texte)) &&
           etiquettes_uniques(texte) &&
           any(grepl("^```\\{r coefficients\\}", texte)) && any(grepl(c("^- .*Pinheiro", "^- .*Wood")[k], texte)) && is.null(r$ecart))
  if (requireNamespace("knitr", quietly = TRUE)) {
    compiler <- file.path(dossier, "compiler.R")
    writeLines(c(sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")), "a <- commandArgs(TRUE)", "setwd(dirname(a[1]))",
                 "knitr::knit(basename(a[1]), output = sub('[.][Rq]md$', '.md', basename(a[1])), quiet = TRUE, envir = new.env())"), compiler)
    messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(compiler), shQuote(r$script)), stdout = TRUE, stderr = TRUE))
    md <- file.path(ici, paste0(d$name, ".md"))
    verifier(sprintf("%s.%s : se compile, avec ses graphiques", d$name, ext), file.exists(md) && !any(grepl("^## Error|Error in", readLines(md, encoding = "UTF-8"))) &&
             length(list.files(file.path(ici, "figure"), "\\.png$")) >= 2 + (k == 2))
  } else {  # sans knitr : le code de ses blocs R tourne dans une session neuve (sans les tableaux de knitr::kable)
    debuts <- grep("^```\\{r", texte)
    fins <- vapply(debuts, function(i) i + match("```", texte[-seq_len(i)]), 0)
    code <- unlist(lapply(seq_along(debuts), function(j) texte[seq(debuts[j] + 1, length.out = fins[j] - debuts[j] - 1)]))
    blocs <- file.path(ici, "blocs.R")
    writeLines(enc2utf8(c("pdf(NULL)", code[!grepl("knitr::", code)])), blocs, useBytes = TRUE)
    avant <- setwd(ici)
    messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(blocs)), stdout = TRUE, stderr = TRUE))
    setwd(avant)
    verifier(sprintf("%s.%s : le code de ses blocs R tourne (knitr n'est pas installé : pas compilé)", d$name, ext), is.null(attr(messages, "status")))
  }
}
# le rapport d'une régression dont la sélection par l'AIC a été faite : ses tableaux, la sélection dans les méthodes
dsr <- definir_modele(pr, "LMM", x = ur("x1", "x2", "sol"), y = ur("hauteur"), echelle = "none", aleatoires = list(par_site), nom = "R8")
msr <- ajuster(pr, dsr)
msr$selection_aic <- selection_aic(msr, dsr)
dir.create(ici <- file.path(dossier, "rapport_selection"))
r <- ecrire_script(pr, dsr, file.path(ici, "R8.Rmd"), "moteur", m = msr, version = "essai")
texte <- readLines(r$script, encoding = "UTF-8")
verifier("R8.Rmd, après la sélection par l'AIC : ses tableaux, et la sélection dans le texte des méthodes",
         is.null(r$ecart) && etiquettes_uniques(texte) && any(grepl("^```\\{r selection-aic\\}", texte)) && any(grepl("^```\\{r importance-aic\\}", texte)) &&
           any(grepl("choisis par l'AICc (Burnham", texte, fixed = TRUE)))
if (requireNamespace("knitr", quietly = TRUE)) {
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(file.path(dossier, "compiler.R")), shQuote(r$script)), stdout = TRUE, stderr = TRUE))
  md <- file.path(ici, "R8.md")
  verifier("R8.Rmd se compile, avec le tableau de la sélection", file.exists(md) && !any(grepl("^## Error|Error in", readLines(md, encoding = "UTF-8"))) &&
           any(grepl("combinaison", readLines(md, encoding = "UTF-8"))))
}

cat("Rapports R Markdown et Quarto\n")
for (i in seq_along(p$models)) {
  d <- p$models[[i]]
  ext <- if (i %% 2) "Rmd" else "qmd"
  dir.create(ici <- file.path(dossier, paste0("rapport", i)))
  r <- ecrire_script(p, d, file.path(ici, paste0(d$name, ".", ext)), "moteur", version = "essai", langue = if (i %% 2) "fr" else "en",
                     donnees = if (i > 2) "csv" else "script")
  texte <- readLines(r$script, encoding = "UTF-8")
  verifier(sprintf("%s.%s : en-tête, méthodes, code du modèle, références", d$name, ext), texte[1] == "---" && any(grepl("^## (Méthodes|Methods)", texte)) &&
           etiquettes_uniques(texte) &&
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
