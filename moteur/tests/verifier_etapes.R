# Vérifie les rapports vivants (moteur/R/etapes.R) par les demandes du serveur, telles que
# l'interface les envoie (serveur.R : DEMANDES, chaque demande passée par le JSON) :
#   - la source d'un projet ouvert d'un fichier (son chemin, sa table, ce que l'import a décidé de
#     ses colonnes, sa taille, sa date, son empreinte SHA-256), d'un export de KoboToolbox avec les
#     réglages de son import et son formulaire, d'une table d'une archive d'ODK ouverte telle quelle ;
#   - chaque sorte d'étape notée, ses paramètres sans numéros internes (les variables par leur nom,
#     les observations par leur identifiant), puis rejouée sur le même fichier : les mêmes données ;
#   - le fichier de l'année suivante, aux colonnes rangées autrement, une colonne nouvelle au milieu,
#     des lignes en plus, une en moins, une valeur corrigée : l'aperçu qui ne change rien, puis la
#     mise à jour ; les modèles prennent les nouvelles observations (sauf ceux qu'on nomme, et le
#     modèle mixte BACI celles de ses unités seulement), celles qu'ils avaient laissées de côté le
#     restent, une condition et un jeu de test gardent les leurs ; les catégories d'avant et les
#     nouvelles ; une variable calculée refaite ; une étape qui ne s'applique plus, sautée ;
#     Ctrl+Z, puis Ctrl+Y (Rétablir) et Ctrl+Z de nouveau, la source et les étapes comprises ; un
#     nouveau changement qui vide Rétablir ;
#   - une colonne qui manque : les étapes et les modèles qui en dépendent, nommés ;
#   - une étape retirée, refusée sans le fichier source ;
#   - le projet enregistré et rouvert (sa source et ses étapes telles quelles), puis son dossier
#     déplacé avec ses données : la source retrouvée par son chemin relatif ;
#   - des données changées hors des étapes (la console R, une version d'avant la 1.1) : vu, et
#     réparé par une mise à jour ; un projet de la 1.0 lu par la 1.0 (son lire_projet, quand git a
#     l'étiquette v1.0.0) ;
#   - les scores d'un modèle en variables, rejoués dans un modèle hiérarchique ;
#   - une catégorie nouvelle, mise avant la dernière : une régression garde sa référence (contre lm()
#     et relevel()), son nouveau coefficient en plus ; une PLS prend son terme, aux réglages des
#     autres ; une PLS-DA garde ses classes ; la référence d'un modèle qui change quand même (sa
#     catégorie n'a plus de lignes, ou une nouvelle vient après elle dans ses lignes), dite ; Ctrl+Z ;
#   - les modèles à qui demander s'ils prennent les nouvelles observations : ceux qui ont laissé de
#     côté une ligne qu'ils pouvaient ajuster (pas une ligne d'une valeur manquante dans leurs
#     variables, ni hors de leur condition), quand le fichier leur en apporte ; leurs lignes comptées
#     comme leur ajustement les compte, avant et après la mise à jour ;
#   - un projet sans source (un tableau collé) mis à jour d'un fichier ; un projet sans étapes dont
#     une variable calculée garde sa formule ; un tableau sans colonne qui nomme les observations,
#     retrouvées par leur place ;
#   - les prédictions d'un modèle ajusté avant un nouvel ordre des catégories (une forêt, des arbres
#     boostés, un arbre, une LDA aux X qualitatifs, un modèle hiérarchique) : les mêmes, sur ses
#     observations, un jeu de prédiction et un fichier ; après un nom changé, refusées.
#   - une variable renommée, traitée en catégories, supprimée, d'un clic droit sur sa colonne : chacune
#     une étape (l'ancien nom et le nouveau), rejouées sur le même fichier puis sur celui de l'année
#     suivante ; un modèle et les références d'un document qui suivent le nom, Annuler et Rétablir qui
#     les remettent, même dans un document écrit depuis ; un ajustement fait avec l'ancien nom, à refaire.
#   Rscript moteur/tests/verifier_etapes.R

options(warn = 1, stringsAsFactors = FALSE)
racine_nuee <- normalizePath(".", winslash = "/")
# Le serveur sans son écoute (comme banc_vitesse.R) : chaque expression de serveur.R jusqu'à
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
options(nuee.langue = "fr")

echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
erreur <- function(expr) tryCatch({ expr; "" }, error = conditionMessage)
# (hors du dossier temporaire de R : un fichier qui y est vient de la séance, des cellules collées,
# et l'étape qui le lit en garde le texte, etapes.R : dans_seance)
dossier <- file.path(dirname(tempdir()), basename(tempfile("nuee_etapes")))
dir.create(dossier)
fichier <- function(...) file.path(dossier, ...)

# Une demande comme l'interface l'envoie : `q` passé par le JSON, la réponse mise en JSON (elle doit
# s'y écrire) ; rend la réponse telle que R l'a faite.
demande <- function(nom, q = list()) {
  q <- jsonlite::fromJSON(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA)), simplifyVector = TRUE)
  r <- DEMANDES[[nom]](q)
  repondre(200L, r)
  invisible(r)
}
refus <- function(nom, q) tryCatch({ demande(nom, q); "" }, error = conditionMessage)
uid_de <- function(p, nom) as.numeric(numeros(p, nom))
# Les valeurs d'une variable par son nom (le nom de la catégorie pour une qualitative), nommées par
# l'observation ; ses catégories
valeurs <- function(p, nom) {
  v <- variable_du_projet(p, uid_de(p, nom))
  obs <- numeros_observations(p)
  x <- unname(colonne(p, v$uid, obs))
  stats::setNames(if (length(v$categories)) as.character(v$categories)[x] else x, unname(p$noms_obs[as.character(obs)]))
}
categories_de <- function(p, nom) as.character(variable_du_projet(p, uid_de(p, nom))$categories %||% character(0))
parametres <- function(p, k) jsonlite::fromJSON(p$etapes[[k]]$parametres, simplifyVector = TRUE)
derniere <- function(p) p$etapes[[length(p$etapes)]]
definition <- function(p, nom) Find(function(d) identical(d$name, nom), p$models)
noms_obs <- function(p, uids) unname(p$noms_obs[as.character(uids)])
statuts <- function(r) vapply(r$etapes, function(e) e$statut, "")

# ---------------------------------------------------------------- les données

# Quatre sites suivis de 2016 à 2025, une ligne par site et par année (l'observation nommée
# « S1-2016 »), avec l'habitat de chaque site (écrit « foret » ou « forêt » selon les années), deux
# mesures (x2 a des valeurs manquantes), une réponse, un comptage où -9999 dit « pas de relevé », et
# les coordonnées en Lambert-93 de chaque site.
set.seed(11)
sites <- data.frame(site = paste0("S", 1:4), habitat = c("forêt", "prairie", "forêt", "lande"),
                    X = c(820000, 826000, 832000, 838000), Y = c(6280000, 6284000, 6276000, 6272000))
annees <- 2016:2025
tableau <- function(annees, sites, graine) {
  set.seed(graine)
  l <- expand.grid(annee = annees, site = sites$site, stringsAsFactors = FALSE)
  l <- l[order(l$annee, l$site), ]
  s <- sites[match(l$site, sites$site), ]
  data.frame(nom = paste(l$site, l$annee, sep = "-"), site = l$site,
             habitat = ifelse(s$habitat == "forêt" & l$annee %% 3 == 0, "foret", s$habitat), annee = l$annee,
             x1 = round(rnorm(nrow(l), 5), 3), x2 = ifelse(runif(nrow(l)) < 0.1, NA, round(rnorm(nrow(l), 2), 3)),
             y = round(3 + (l$site == "S1" & l$annee >= 2021) + rnorm(nrow(l), 0, 0.3), 3),
             comptage = ifelse(runif(nrow(l)) < 0.15, -9999, rpois(nrow(l), 8)), X = s$X, Y = s$Y)
}
df <- tableau(annees, sites, 1)
etude <- fichier("etude")
dir.create(file.path(etude, "donnees"), recursive = TRUE)
source_csv <- file.path(etude, "donnees", "releves.csv")
utils::write.csv(df, source_csv, row.names = FALSE, na = "")
# un tableau des sites, joint par une clé ; une grille ASCII d'ESRI de 10 × 8 cellules de 2,5 km en Lambert-93
sites_csv <- file.path(etude, "donnees", "sites.csv")
utils::write.csv(data.frame(site = sites$site, altitude = c(120, 85, 140, 310), sol = c("argile", "sable", "argile", "limon")), sites_csv, row.names = FALSE)
asc <- file.path(etude, "donnees", "pente.asc")
writeLines(c("ncols 10", "nrows 8", "xllcorner 815000", "yllcorner 6260000", "cellsize 2500", "NODATA_value -1",
             vapply(0:7, function(i) paste((i * 3 + 0:9) %% 7, collapse = " "), "")), asc)
writeLines('PROJCS["RGF_1993_Lambert_93",GEOGCS["GCS_RGF_1993",DATUM["D_RGF_1993",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic"],PARAMETER["False_Easting",700000.0],PARAMETER["False_Northing",6600000.0],PARAMETER["Central_Meridian",3.0],PARAMETER["Standard_Parallel_1",49.0],PARAMETER["Standard_Parallel_2",44.0],PARAMETER["Latitude_Of_Origin",46.5],UNIT["Meter",1.0]]',
           sub("\\.asc$", ".prj", asc))

cat("La source d'un projet ouvert d'un fichier\n")
r <- demande("ouvrir", list(chemin = source_csv))
id <- r$id
p <- projet(id)
s <- p$source_donnees
verifier("le fichier, son lecteur, sa taille, sa date et son SHA-256, l'heure de la lecture",
         identical(s$chemin, normalizePath(source_csv, winslash = "/")) && identical(s$lecteur, "tableau") && s$taille == file.size(source_csv) &&
           abs(s$date - as.numeric(file.mtime(source_csv))) < 1e-3 && identical(s$sha256, unname(empreinte_sha(source_csv))) && grepl("^\\d{4}-\\d\\d-\\d\\dT", s$lu))
verifier("ce que l'import a décidé : la colonne qui nomme les observations, celles lues en nombres",
         identical(s$reglages$identifiant, "nom") && all(c("annee", "x1", "x2", "y", "comptage", "X", "Y") %in% s$reglages$quantitatives) &&
           !"site" %in% s$reglages$quantitatives && !length(s$reglages$secondaires))
e <- demande("etapes", list(projet = id))
verifier("la demande etapes : la source là et inchangée, aucune étape, les données conformes",
         isTRUE(e$source$existe) && isFALSE(e$source$modifie) && !length(e$etapes) && isTRUE(e$conformes) && identical(e$source$lecteur, "tableau"))
etape_noms <- function() vapply(projet(id)$etapes, `[[`, "", "demande")

cat("Chaque sorte d'étape, notée sans numéros internes\n")
p <- projet(id)
u <- function(nom) uid_de(projet(id), nom)
o <- function(noms) numeros_observations(projet(id), noms)
demande("modifier_valeur", list(projet = id, observation = o("S1-2018"), variable = u("x1"), valeur = "12,5"))
verifier("modifier_valeur : l'observation par son nom, la variable par le sien",
         identical(parametres(projet(id), 1), list(observation = "S1-2018", variable = "x1", valeur = "12,5")) && identical(etape_noms(), "modifier_valeur"))
demande("modifier_valeur", list(projet = id, observation = o("S2-2016"), variable = u("x1"), valeur = "7"))
demande("variable_calculee", list(projet = id, nom = "x12", formule = "x1 * x2"))
v12 <- variable_du_projet(projet(id), u("x12"))
verifier("variable_calculee : la formule, gardée aussi par la variable (formula, origine « formule »)",
         identical(parametres(projet(id), 3)$formule, "x1 * x2") && identical(v12$formula, "x1 * x2") && identical(v12$origine$type, "formule"))
demande("imputer", list(projet = id, variables = c(u("x1"), u("x2"), u("y")), k = 1))
verifier("imputer : ses variables par leur nom, et la langue où elle nomme « x2 (imputée) »",
         identical(parametres(projet(id), 4)$variables, c("x1", "x2", "y")) && identical(derniere(projet(id))$langue, "fr") && "x2 (imputée)" %in% projet(id)$noms_var)
demande("classes_valeurs", list(projet = id, nom = "classe y", variable = u("y"), bornes = 3))
demande("remplacer", list(projet = id, variables = u("comptage"), chercher = "-9999", remplacer = ""))
verifier("remplacer : -9999 vidé, la variable par son nom", identical(parametres(projet(id), 6)$variables, "comptage") && !any(valeurs(projet(id), "comptage") == -9999, na.rm = TRUE))
cats <- categories_de(projet(id), "habitat")
demande("categories", list(projet = id, variable = u("habitat"), noms = ifelse(cats == "foret", "forêt", cats)))
verifier("categories : « foret » fusionnée dans « forêt », les catégories d'avant gardées avec l'étape",
         identical(categories_de(projet(id), "habitat"), c("forêt", "lande", "prairie")) && identical(parametres(projet(id), 7)$anciennes, cats))
demande("coller_cellules", list(projet = id, observations = o(c("S3-2017", "S4-2017")), variables = c(u("x1"), u("y")), valeurs = list(list("1,5", "2"), list(NULL, "4"))))
verifier("coller_cellules : les observations et les variables par leur nom",
         identical(parametres(projet(id), 8)$observations, c("S3-2017", "S4-2017")) && identical(parametres(projet(id), 8)$variables, c("x1", "y")) &&
           unname(valeurs(projet(id), "x1")["S3-2017"]) == 1.5)
demande("decrire_variables", list(projet = id, descriptions = list(list(variable = u("x1"), unite = "m", description = "hauteur"))))
verifier("decrire_variables : la variable par son nom", identical(parametres(projet(id), 9)$descriptions$variable, "x1"))
demande("coordonnees", list(projet = id, regler = TRUE, x = u("X"), y = u("Y"), systeme = "2154"))
verifier("coordonnees : x et y par leur nom", identical(parametres(projet(id), 10)[c("x", "y")], list(x = "X", y = "Y")))
tj <- demande("table_a_joindre", list(projet = id, chemin = sites_csv))
demande("ajouter_colonnes", list(projet = id, chemin = sites_csv, nom = "sites.csv", cles_projet = list(u("site")), cles_table = list("site"), colonnes = list("altitude", "sol")))
e11 <- projet(id)$etapes[[11]]
verifier("ajouter_colonnes d'un fichier : la clé en « var:site », le fichier gardé comme la source",
         identical(parametres(projet(id), 11)$cles_projet, "var:site") && identical(e11$fichiers[[1]]$sha256, unname(empreinte_sha(sites_csv))) &&
           all(c("altitude", "sol") %in% projet(id)$noms_var))
collees <- demande("table_a_joindre", list(projet = id, texte = "nom\tlot\nS1-2016\tA\nS2-2016\tB\nS3-2016\tA"))
demande("ajouter_colonnes", list(projet = id, chemin = collees$chemin, colle = TRUE, nom = "cellules collées", cles_projet = list("id:1"), cles_table = list("nom"), colonnes = list("lot")))
verifier("ajouter_colonnes de cellules collées : leur texte va avec l'étape, la clé en « id:Observation »",
         grepl("S2-2016\tB", parametres(projet(id), 12)$texte, fixed = TRUE) && is.null(parametres(projet(id), 12)$chemin) &&
           identical(parametres(projet(id), 12)$cles_projet, "id:Observation") && !length(projet(id)$etapes[[12]]$fichiers))
demande("ajouter_raster", list(projet = id, fichiers = list(list(chemin = asc, methode = "cellule", noms = list("pente")))))
verifier("ajouter_raster : le fichier gardé comme la source", identical(projet(id)$etapes[[13]]$fichiers[[1]]$chemin, normalizePath(asc, winslash = "/")) &&
           "pente" %in% projet(id)$noms_var)
demande("classe_selection", list(projet = id, variable = "groupe", etiquette = "témoin", observations = o(c("S2-2016", "S2-2017", "S2-2018"))))
verifier("classe_selection : les observations par leur nom", identical(parametres(projet(id), 14)$observations, c("S2-2016", "S2-2017", "S2-2018")))
bm <- demande("baci_modele", list(projet = id, y = u("y"), temps = u("annee"), unite = u("site"), impact = list("S1"), temoins = list("S2", "S3"), evenement = 2021, car1 = FALSE))
verifier("baci_modele : ses deux variables 0/1, son temps et son unité par leur nom",
         identical(parametres(projet(id), 15)[c("temps", "unite")], list(temps = "var:annee", unite = "var:site")) && length(bm$nouvelles) == 2 && is.null(parametres(projet(id), 15)$y))
km <- demande("ajuster", list(projet = id, type = "KMEANS", x = list(u("x1"), u("y")), options = list(k = 2), nom = "K1"))
demande("kmeans_variable", list(projet = id, modele = km$id, nom = "groupes km"))
pc <- demande("ajuster", list(projet = id, type = "PCA", x = list(u("x1"), u("x2 (imputée)"), u("y")), A = 2, nom = "C1"))
demande("scores_en_variables", list(modele = pc$id))
etape_scores <- derniere(projet(id))
verifier("kmeans_variable, scores_en_variables : la définition du modèle et le projet qu'il a vu gardés avec l'étape",
         identical(etape_noms()[16:17], c("kmeans_variable", "scores_en_variables")) && identical(parametres(projet(id), 17)$modele, "C1") &&
           is.character(parametres(projet(id), 17)$definition) && all(c("$C1.t1", "$C1.t2") %in% projet(id)$noms_var))
etapes_json <- vapply(projet(id)$etapes, `[[`, "", "parametres")
verifier("aucun paramètre noté ne tient un numéro de variable", !any(grepl("\"variables?\":\\[?[0-9]", etapes_json[-c(16, 17)])))
e <- demande("etapes", list(projet = id))
verifier("la demande etapes : 17 étapes, chacune son étiquette, sa date, sa demande ; les fichiers des étapes ; les données conformes",
         length(e$etapes) == 17 && e$etapes[[1]]$etiquette == "modification de x1 (S1-2018)" && e$etapes[[17]]$demande == "scores_en_variables" &&
           isFALSE(e$etapes[[11]]$fichiers[[1]]$modifie) && isTRUE(e$conformes))
# (chaque étiquette écrite d'après la demande et ses paramètres, dans la langue de la demande qui la montre)
options(nuee.langue = "en")
en_etapes <- vapply(demande("etapes", list(projet = id))$etapes, `[[`, "", "etiquette")
options(nuee.langue = "fr")
verifier("les étiquettes des étapes dans la langue de la demande, faites en français, montrées en anglais",
         identical(en_etapes, c("edit of x1 (S1-2018)", "edit of x1 (S2-2016)", "variable x12 = x1 * x2", "imputation of x1, x2, y", "variable classe y, classes of y",
                                "replacement of “-9999” with empty in comptage", "categories of habitat", "cells pasted into x1, y", "units and descriptions of x1",
                                "coordinates X and Y", "columns from sites.csv", "columns from pasted cells", "values from pente.asc", "class “témoin” of groupe",
                                "BACI variables (event 2021)", "variable groupes km, clusters of K1", "scores of C1 as variables")))
inconnue <- list(demande = "plus_tard", etiquette = "une demande d'une version suivante", parametres = "{}")
verifier("une demande que cette version ne connaît pas : l'étiquette notée avec elle", identical(etiquette_etape(inconnue), "une demande d'une version suivante") &&
           identical(etiquette_etape(projet(id)$etapes[[6]]), "remplacement de « -9999 » par vide dans comptage"))

cat("Les modèles du projet\n")
x_lm <- list(u("x1"), u("habitat"))
m_tous <- demande("ajuster", list(projet = id, type = "LM", x = x_lm, y = list(u("y")), nom = "Tous"))
m_sans <- demande("ajuster", list(projet = id, type = "LM", x = x_lm, y = list(u("y")), nom = "Sans", exclues = o(c("S1-2016", "S2-2016"))))
m_garde <- demande("ajuster", list(projet = id, type = "LM", x = x_lm, y = list(u("y")), nom = "Garde"))
m_cond <- demande("ajuster", list(projet = id, type = "LM", x = list(u("x1")), y = list(u("y")), nom = "Condition", condition = "site == \"S1\""))
m_test <- demande("ajuster", list(projet = id, type = "PLS", x = list(u("x1"), u("x2 (imputée)")), y = list(u("y")), A = 1, nom = "Test", test = o(c("S4-2024", "S4-2025"))))
m_h <- demande("ajuster", list(projet = id, type = "PLS", x = list(u("$C1.t1"), u("$C1.t2"), u("altitude")), y = list(u("y")), A = 1, nom = "Haut"))
verifier("des modèles de toutes les observations, sans deux, sous une condition, avec un jeu de test, hiérarchique",
         m_tous$N == 40 && m_sans$N == 38 && m_cond$N == 10 && length(definition(projet(id), "Test")$test) == 2 && m_h$N == 40)

cat("Rejouées sur le même fichier : les mêmes données\n")
avant <- projet(id)
a <- demande("mettre_a_jour", list(projet = id, apercu = TRUE))
verifier("l'aperçu : chaque étape faite, aucune observation ni variable de plus ou de moins, aucune cellule changée",
         all(statuts(a) == "ok") && a$observations$ajoutees == 0 && a$observations$retirees == 0 && a$observations$cellules == 0 &&
           !length(a$variables$nouvelles) && !length(a$variables$absentes) && !length(a$variables$categories))
verifier("l'aperçu : aucune observation nouvelle, aucun modèle à qui demander s'il les prend (Sans a laissé deux lignes de côté)",
         !length(a$modeles$observations_choisies))
verifier("l'aperçu ne change rien", identical(projet(id), avant))
brut <-sans_attributs(importer_source(source_csv, "tableau", NULL, avant$source_donnees$reglages))
rj <- rejouer_etapes(brut, avant$etapes, dossier_projet(avant))
al <- aligner_projet(avant, rj$projet)
memes <- function(a, b) {
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(a$donnees), a$variables)
  all(vapply(vars, function(v) {
    k <- as.character(v$uid)
    identical(as.character(v$categories), as.character(variable_du_projet(b, v$uid)$categories)) &&
      isTRUE(all.equal(unname(a$donnees[rownames(a$donnees), k]), unname(b$donnees[rownames(a$donnees), k]), tolerance = 1e-12))
  }, TRUE))
}
verifier("rejouer_etapes puis aligner_projet : chaque variable, ses valeurs et ses catégories, sous ses numéros", memes(avant, al$projet) &&
           identical(sort(as.numeric(rownames(avant$donnees))), sort(as.numeric(rownames(al$projet$donnees)))))
verifier("les scores rejoués sont ceux du modèle ajusté de nouveau", isTRUE(all.equal(valeurs(al$projet, "$C1.t1"), valeurs(avant, "$C1.t1"), tolerance = 1e-10)))

cat("Enregistré et rouvert : la source et les étapes telles quelles\n")
f_projet <- file.path(etude, "etude.nuee")
demande("enregistrer", list(projet = id, chemin = f_projet))
lu <- jsonlite::fromJSON(f_projet, simplifyVector = FALSE)
verifier("le fichier : le format de cette version (3 au moins, celui des rapports vivants), la source et son chemin relatif, les étapes, l'empreinte",
         lu$nuee$format == FORMAT_PROJET && FORMAT_PROJET >= 3 && identical(lu$source_donnees$relatif, "donnees/releves.csv") && length(lu$etapes) == 17 &&
           identical(lu$etapes[[11]]$fichiers[[1]]$relatif, "donnees/sites.csv") && is.list(lu$empreinte_donnees))
r2 <- demande("ouvrir", list(chemin = f_projet))
p2 <- projet(r2$id)
verifier("rouvert : les étapes à l'identique, la source, les données conformes",
         identical(vapply(p2$etapes, `[[`, "", "parametres"), etapes_json) && identical(p2$source_donnees$sha256, avant$source_donnees$sha256) &&
           isTRUE(demande("etapes", list(projet = r2$id))$conformes))
# (le dossier déplacé, ses données avec lui : la source retrouvée par son chemin relatif)
ailleurs <- fichier("ailleurs")
dir.create(ailleurs)
invisible(file.copy(etude, ailleurs, recursive = TRUE))
r3 <- demande("ouvrir", list(chemin = file.path(ailleurs, "etude", "etude.nuee")))
e3 <- demande("etapes", list(projet = r3$id))
verifier("le dossier déplacé : la source et le fichier d'une étape retrouvés à côté du projet",
         identical(e3$source$trouve, normalizePath(file.path(ailleurs, "etude", "donnees", "releves.csv"), winslash = "/")) &&
           identical(e3$etapes[[11]]$fichiers[[1]]$trouve, normalizePath(file.path(ailleurs, "etude", "donnees", "sites.csv"), winslash = "/")))
unlink(etude, recursive = TRUE)
a3 <- demande("mettre_a_jour", list(projet = r3$id, apercu = TRUE))
verifier("le dossier d'origine parti : la mise à jour lit les fichiers déplacés", all(statuts(a3) == "ok") && a3$observations$cellules == 0 &&
           identical(a3$fichier$chemin, e3$source$trouve))
# (le projet d'un autre, sa formule de Nouvelle variable changée pour exécuter autre chose qu'elle :
# l'aperçu de la mise à jour et le retrait d'une étape la lisent avant de l'évaluer, et la refusent ;
# rien ne s'exécute : variables.R, lire_formule_variable)
f_piege <- file.path(ailleurs, "etude", "piege.nuee")
demande("enregistrer", list(projet = r3$id, chemin = f_piege))
texte_piege <- readChar(f_piege, file.size(f_piege), useBytes = TRUE)
# (que des noms connus : seule la lecture de la formule l'arrête, pas les colonnes qu'elle nomme)
texte_piege <- gsub("x1 * x2", "{ assign('TEMOIN_NUEE', TRUE, envir = globalenv()); x1 * x2 }", texte_piege, fixed = TRUE)
writeBin(charToRaw(texte_piege), f_piege)
rp <- demande("ouvrir", list(chemin = f_piege))
ap <- demande("mettre_a_jour", list(projet = rp$id, apercu = TRUE))
refus_piege <- vapply(ap$etapes, function(e) grepl("n'est pas permis|isn't allowed", e$raison %||% ""), TRUE)
ar_piege <- tryCatch(demande("retirer_etape", list(projet = rp$id, n = 1, apercu = TRUE)), error = function(e) e)
verifier("une formule changée dans le fichier pour exécuter autre chose : refusée à l'aperçu de la mise à jour (son étape dit pourquoi) et au retrait d'une étape, sans rien exécuter",
         sum(refus_piege) == 1 && all(statuts(ap)[refus_piege] != "ok") && !exists("TEMOIN_NUEE"))
demande("fermer_projet", list(projet = rp$id))
demande("fermer_projet", list(projet = r2$id))
id <- r3$id
etude <- file.path(ailleurs, "etude")
source_csv <- file.path(etude, "donnees", "releves.csv")
u <- function(nom) uid_de(projet(id), nom)
o <- function(noms) numeros_observations(projet(id), noms)
m_garde_id <- demande("ajuster", list(projet = id, numero = definition(projet(id), "Garde")$number))$id

cat("Le fichier de l'année suivante\n")
# 2026 en plus (et S5, un nouveau site de « dune »), S2-2019 parti, une valeur corrigée ; les
# colonnes rangées autrement, la colonne des noms au milieu, une colonne nouvelle
sites2 <- rbind(sites, data.frame(site = "S5", habitat = "dune", X = 836500, Y = 6265000))
df2 <- rbind(df, tableau(2026, sites2, 2))
df2 <- df2[df2$nom != "S2-2019", ]
df2$y[df2$nom == "S3-2020"] <- 99
df2$profondeur <- round(runif(nrow(df2), 1, 3), 2)
df2 <- df2[c("y", "site", "profondeur", "nom", "x2", "habitat", "annee", "x1", "comptage", "X", "Y")]
suivant <- file.path(etude, "donnees", "releves 2026.csv")
utils::write.csv(df2, suivant, row.names = FALSE, na = "")
avant <- projet(id)
uid_y <- u("y")
cats_avant <- categories_de(avant, "habitat")
a <- demande("mettre_a_jour", list(projet = id, chemin = suivant, apercu = TRUE))
changees <- stats::setNames(vapply(a$observations$cellules_par_variable, function(x) as.numeric(x$cellules), 0),
                            vapply(a$observations$cellules_par_variable, `[[`, "", "variable"))
verifier("l'aperçu : 5 observations de plus (les premières nommées), 1 de moins, la cellule corrigée",
         a$observations$ajoutees == 5 && identical(as.character(a$observations$premieres_ajoutees), paste0("S", 1:5, "-2026")) &&
           a$observations$retirees == 1 && identical(as.character(a$observations$premieres_retirees), "S2-2019") && changees[["y"]] == 1 &&
           isFALSE(a$observations$par_position))
verifier("l'aperçu : les cellules changées par variable, celles des scores refaits avec les nouvelles observations comprises",
         a$observations$cellules == sum(changees) && !any(c("x1", "x2", "site", "habitat", "comptage") %in% names(changees)) && changees[["$C1.t1"]] == 39)
verifier("l'aperçu : la variable nouvelle, les catégories nouvelles de chaque variable",
         identical(as.character(a$variables$nouvelles), "profondeur") && !length(a$variables$absentes) &&
           any(vapply(a$variables$categories, function(x) identical(x$variable, "habitat") && identical(as.character(x$nouvelles), "dune"), TRUE)) &&
           any(vapply(a$variables$categories, function(x) identical(x$variable, "site") && identical(as.character(x$nouvelles), "S5"), TRUE)))
verifier("l'aperçu : les modèles qui ont laissé de côté des lignes qu'ils pouvaient prendre (un jeu de test n'en laisse pas), à qui demander s'ils prennent les nouvelles : Sans en ajuste 38 de 40, le fichier lui en apporte 5",
         identical(vapply(a$modeles$observations_choisies, `[[`, "", "nom"), "Sans") &&
           identical(as.numeric(unlist(a$modeles$observations_choisies[[1]][c("observations", "admises", "nouvelles")])), c(38, 40, 5)) &&
           !length(a$modeles$variables_absentes))
verifier("l'aperçu ne change rien", identical(projet(id), avant))
a <- demande("mettre_a_jour", list(projet = id, chemin = suivant, sans_nouvelles = list("Garde")))
p <- projet(id)
verifier("mis à jour : 44 observations, celles d'avant sous leurs numéros, la colonne nouvelle", length(p$observations) == 44 &&
           all(setdiff(numeros_observations(avant), numeros_observations(avant, "S2-2019")) %in% numeros_observations(p)) && "profondeur" %in% p$noms_var && u("y") == uid_y &&
           unname(valeurs(p, "y")["S3-2020"]) == 99)
verifier("les étapes rejouées dans le nouveau numérotage, chacune faite", all(statuts(a) == "ok") && length(p$etapes) == 17)
verifier("la source devient le nouveau fichier, les données conformes", identical(p$source_donnees$chemin, normalizePath(suivant, winslash = "/")) &&
           isTRUE(donnees_conformes(p)))
verifier("les catégories d'avant dans leur ordre, la nouvelle avant la dernière ; « foret » de 2026 fusionné par l'étape",
         identical(categories_de(p, "habitat"), c(utils::head(cats_avant, -1), "dune", utils::tail(cats_avant, 1))) &&
           unname(valeurs(p, "habitat")["S1-2026"]) == "forêt" && identical(categories_de(p, "site"), c("S1", "S2", "S3", "S5", "S4")))
verifier("les modèles de habitat prennent le terme de « dune », avant celui de la référence ; aucune référence ne change",
         identical(vapply(Filter(function(t) startsWith(t$name, "habitat"), definition(p, "Tous")$x), `[[`, "", "name"),
                   c("habitat(forêt)", "habitat(lande)", "habitat(dune)", "habitat(prairie)")) &&
           setequal(vapply(a$modeles$nouvelles_categories, `[[`, "", "nom"), c("Tous", "Sans", "Garde")) && !length(a$modeles$references_changees))
nouvelles <- o(paste0("S", 1:5, "-2026"))
dp <- function(nom) definition(p, nom)
verifier("Tous prend les nouvelles observations ; S2-2019 a quitté tous les modèles",
         all(nouvelles %in% dp("Tous")$observations) && length(dp("Tous")$observations) == 44 &&
           !any(vapply(p$models, function(d) any(numeros_observations(avant, "S2-2019") %in% as.numeric(unlist(c(d$observations, d$cochees, d$test)))), TRUE)))
verifier("Sans : ses deux observations laissées de côté le restent, il prend les nouvelles",
         !any(o(c("S1-2016", "S2-2016")) %in% dp("Sans")$observations) && all(nouvelles %in% dp("Sans")$observations))
verifier("Garde, nommé dans sans_nouvelles : il ne les prend pas", !any(nouvelles %in% dp("Garde")$observations) && length(dp("Garde")$observations) == 39)
verifier("Condition : les nouvelles parmi les cochées, la condition les choisit à l'ajustement",
         all(nouvelles %in% dp("Condition")$cochees) && demande("ajuster", list(projet = id, numero = dp("Condition")$number))$N == 11)
verifier("Test : son jeu de test gardé, les nouvelles dans le modèle", identical(sort(as.numeric(dp("Test")$test)), sort(o(c("S4-2024", "S4-2025")))) &&
           all(nouvelles %in% dp("Test")$observations))
verifier("BACI : les nouvelles des unités d'impact et témoins seulement", setequal(intersect(dp("BACI y")$observations, nouvelles), o(c("S1-2026", "S2-2026", "S3-2026"))))
verifier("la variable calculée refaite pour les nouvelles observations", isTRUE(all.equal(unname(valeurs(p, "x12")[c("S1-2026", "S5-2026")]),
                                                                                     unname(valeurs(p, "x1")[c("S1-2026", "S5-2026")] * valeurs(p, "x2")[c("S1-2026", "S5-2026")]))))
verifier("les colonnes jointes, la valeur du raster et les variables BACI aussi", !is.na(valeurs(p, "altitude")["S4-2026"]) && is.na(valeurs(p, "altitude")["S5-2026"]) &&
           !is.na(valeurs(p, "pente")["S5-2026"]) && unname(valeurs(p, "impact BACI (S1 contre S2, S3)")["S1-2026"]) == 1)
refait <- demande("ajuster", list(projet = id, numero = dp("Tous")$number))
verifier("Tous réajusté sur les 44 observations", refait$N == 44)
garde <- modele(m_garde_id, TRUE)
verifier("un modèle de la séance garde ses résultats, rapporté aux catégories d'aujourd'hui", garde$modele$N == 40)
apres_maj <- projet(id)
verifier("rien à rétablir avant d'avoir annulé", is.null(resume_projet(apres_maj)$retablissable) && grepl("Rien à rétablir", refus("retablir", list(projet = id))))
e <- demande("annuler", list(projet = id))
verifier("Ctrl+Z : la mise à jour annulée, la source et les étapes comprises", e$etiquette == "mise à jour des données" && length(projet(id)$observations) == 40 &&
           identical(projet(id)$source_donnees, avant$source_donnees) && identical(projet(id)$etapes, avant$etapes) && identical(projet(id)$models, avant$models))
verifier("Rétablir est proposé, nommé comme Annuler l'était", identical(e$projet$retablissable, "mise à jour des données"))
# (Rétablir : la mise à jour refaite telle qu'elle était, ses données, sa source et ses étapes, et de nouveau annulable)
r <- demande("retablir", list(projet = id))
pr <- projet(id)
verifier("Ctrl+Y : la mise à jour refaite, ses données, sa source, ses étapes et l'empreinte de ses données ; de nouveau annulable, plus rien à rétablir",
         identical(r$etiquette, "mise à jour des données") && length(pr$observations) == 44 && identical(pr$donnees, apres_maj$donnees) &&
           identical(pr$variables, apres_maj$variables) && identical(pr$source_donnees, apres_maj$source_donnees) && identical(pr$etapes, apres_maj$etapes) &&
           identical(pr$models, apres_maj$models) && isTRUE(donnees_conformes(pr)) && identical(r$projet$annulable, "mise à jour des données") &&
           is.null(r$projet$retablissable) && isTRUE(pr$modifie))
e <- demande("annuler", list(projet = id))
verifier("Ctrl+Z de nouveau : les données, la source et les étapes d'avant", length(projet(id)$observations) == 40 && identical(projet(id)$etapes, avant$etapes) &&
           identical(projet(id)$donnees, avant$donnees) && isTRUE(donnees_conformes(projet(id))))

cat("Une étape qui ne s'applique plus, une colonne qui manque\n")
df3 <- df2[df2$nom != "S2-2016", setdiff(names(df2), "x2")]
manque <- file.path(etude, "donnees", "releves sans x2.csv")
utils::write.csv(df3, manque, row.names = FALSE, na = "")
a <- demande("mettre_a_jour", list(projet = id, chemin = manque, apercu = TRUE))
st <- stats::setNames(statuts(a), vapply(a$etapes, `[[`, "", "demande"))
verifier("modifier_valeur d'une observation partie : sautée, et dit laquelle", st[2] == "sautee" && grepl("S2-2016", a$etapes[[2]]$raison, fixed = TRUE))
verifier("la variable calculée et l'imputation qui lisaient x2 : sautées", st[["variable_calculee"]] == "sautee" && grepl("x2", a$etapes[[3]]$raison) && st[["imputer"]] == "sautee")
verifier("les scores d'un modèle qui utilisait une variable partie : sautés, la variable nommée", st[["scores_en_variables"]] == "sautee")
verifier("les autres étapes faites", all(st[-c(2, 3, 4, 17)] == "ok"))
absentes <- as.character(a$variables$absentes)
verifier("les variables absentes : x2 et celles qui en venaient", all(c("x2", "x12", "x2 (imputée)", "$C1.t1") %in% absentes))
touches <- vapply(a$modeles$variables_absentes, `[[`, "", "nom")
verifier("les modèles qui ne se réajustent plus, et ce qui leur manque", setequal(touches, c("C1", "Test", "Haut")) &&
           any(vapply(a$modeles$variables_absentes, function(m) m$nom == "Haut" && all(c("$C1.t1", "$C1.t2") %in% m$variables), TRUE)))

cat("Une étape retirée\n")
e <- demande("etapes", list(projet = id))
k <- which(vapply(e$etapes, `[[`, "", "demande") == "remplacer")
r <- demande("retirer_etape", list(projet = id, n = k))
p <- projet(id)
verifier("remplacer retiré : les -9999 revenus, 16 étapes, le même fichier source, un seul Ctrl+Z",
         any(valeurs(p, "comptage") == -9999, na.rm = TRUE) && length(p$etapes) == 16 && all(statuts(r) == "ok") &&
           identical(p$source_donnees$chemin, normalizePath(source_csv, winslash = "/")) && grepl("^retrait de l'étape", r$projet$annulable))
demande("annuler", list(projet = id))
verifier("annulé : l'étape revenue", length(projet(id)$etapes) == 17 && !any(valeurs(projet(id), "comptage") == -9999, na.rm = TRUE))
r <- demande("retablir", list(projet = id))
verifier("rétabli : l'étape de nouveau retirée, les -9999 revenus, les étapes et les données d'accord",
         grepl("^retrait de l'étape", r$etiquette) && length(projet(id)$etapes) == 16 && any(valeurs(projet(id), "comptage") == -9999, na.rm = TRUE) &&
           isTRUE(donnees_conformes(projet(id))))
demande("annuler", list(projet = id))
# (un nouveau changement : plus rien à rétablir)
avant_nouveau <- projet(id)
demande("modifier_valeur", list(projet = id, observation = o("S1-2019"), variable = u("x1"), valeur = "3"))
verifier("un nouveau changement vide Rétablir", is.null(resume_projet(projet(id))$retablissable) && grepl("Rien à rétablir", refus("retablir", list(projet = id))))
demande("annuler", list(projet = id))
verifier("annulé : 17 étapes, l'étape du changement partie, les données d'avant", length(projet(id)$etapes) == 17 &&
           identical(projet(id)$donnees, avant_nouveau$donnees) && isTRUE(donnees_conformes(projet(id))))
cache <- paste0(source_csv, ".parti")
invisible(file.rename(source_csv, cache))
verifier("refusé sans le fichier source", grepl("introuvable", refus("retirer_etape", list(projet = id, n = 1))) &&
           isFALSE(demande("etapes", list(projet = id))$source$existe))
invisible(file.rename(cache, source_csv))
write("S9-2030,S9,lande,2030,1,1,1,1,1,1", source_csv, append = TRUE)
verifier("le fichier source changé : la demande etapes le dit", isTRUE(demande("etapes", list(projet = id))$source$modifie))
avant_retrait <- projet(id)
refus_retrait <- tryCatch({ demande("retirer_etape", list(projet = id, n = k)); NULL }, error = function(e) e)
verifier("retirer une étape d'une source changée : refusé (source_modifiee), en disant de mettre à jour d'abord ou de regarder l'aperçu",
         inherits(refus_retrait, "source_modifiee") && grepl("à jour", conditionMessage(refus_retrait)) && grepl("aperçu", conditionMessage(refus_retrait)) &&
           identical(projet(id), avant_retrait))
ar <- demande("retirer_etape", list(projet = id, n = k, apercu = TRUE))
verifier("son aperçu : le rapport d'une mise à jour, la ligne nouvelle du fichier comprise, sans rien changer",
         isTRUE(ar$apercu) && ar$observations$ajoutees == 1 && identical(as.character(ar$observations$premieres_ajoutees), "S9-2030") &&
           length(ar$etapes) == 16 && is.null(ar$projet) && identical(projet(id), avant_retrait))
writeLines(utils::head(readLines(source_csv), -1), source_csv)
verifier("le fichier rendu tel qu'il était (une autre date, le même contenu) : la source n'a pas changé, le retrait se ferait",
         isFALSE(demande("etapes", list(projet = id))$source$modifie) && demande("retirer_etape", list(projet = id, n = k, apercu = TRUE))$observations$ajoutees == 0)
# (un classeur dont la feuille de la source a changé de nom : retirer_etape demande laquelle lire, et la prend)
classeur <- fichier("releves.xlsx")
openxlsx::write.xlsx(list(releves = utils::head(df, 12), notes = data.frame(note = "relevés de printemps")), classeur)
xl <- demande("ouvrir", list(chemin = classeur, table = "releves"))
demande("modifier_valeur", list(projet = xl$id, observation = 2, variable = uid_de(projet(xl$id), "x1"), valeur = "50"))
openxlsx::write.xlsx(list(`releves 2026` = utils::head(df, 15), notes = data.frame(note = "relevés de printemps")), classeur)
choix <- demande("retirer_etape", list(projet = xl$id, n = 1, apercu = TRUE))
ax <- demande("retirer_etape", list(projet = xl$id, n = 1, apercu = TRUE, table = "releves 2026"))
verifier("retirer_etape d'un classeur dont la feuille de la source n'y est plus : choisir, puis la feuille donnée (table)",
         identical(as.character(choix$choisir), c("releves 2026", "notes")) && ax$observations$ajoutees == 3 && identical(ax$fichier$table, "releves 2026"))
demande("mettre_a_jour", list(projet = xl$id, table = "releves 2026"))
rx <- demande("retirer_etape", list(projet = xl$id, n = 1))
verifier("mis à jour de cette feuille, devenue celle de la source : le retrait s'y fait sans la redemander",
         identical(projet(xl$id)$source_donnees$table, "releves 2026") && !length(projet(xl$id)$etapes) && length(projet(xl$id)$observations) == 15 &&
           identical(unname(valeurs(projet(xl$id), "x1")[2]), df$x1[2]))

cat("Des données changées hors des étapes\n")
verifier("resume_projet dit qu'un projet a une source (a_source) ; etapes sans q$conformes ne fait pas l'empreinte des données",
         isTRUE(resume_projet(projet(id))$a_source) && isFALSE(demande("coller", list(texte = "a\tb\n1\t2\n3\t4"))$a_source) &&
           !any(c("conformes", "raison") %in% names(demande("etapes", list(projet = id, conformes = FALSE)))) && isTRUE(demande("etapes", list(projet = id))$conformes))
verifier("la conformité gardée pour la révision des données : l'empreinte n'est refaite qu'après un changement",
         identical(etat$conformites[[id]]$revision, etat$revisions[[id]] %||% 0) && {
           garde <- etat$conformites[[id]]
           etat$conformites[[id]]$valeur <- "gardée"
           vue <- identical(demande("etapes", list(projet = id))$conformes, "gardée")
           etat$conformites[[id]] <- garde
           vue
         })
etat$projets[[id]]$donnees[1, 1] <- 1234  # (la console R : P1$donnees[1, 1] <- 1234, une révision des données)
reviser(id)
e <- demande("etapes", list(projet = id))
verifier("vu : les données ne sont plus celles des étapes, et la raison", isFALSE(e$conformes) && grepl("hors des étapes", e$raison))
demande("modifier_valeur", list(projet = id, observation = o("S1-2017"), variable = u("x1"), valeur = "1"))
verifier("une étape de plus ne les rend pas conformes : l'empreinte reste rompue", identical(projet(id)$empreinte_donnees, "") && isFALSE(donnees_conformes(projet(id))))
demande("mettre_a_jour", list(projet = id))
verifier("une mise à jour les refait d'après la source et les étapes", isTRUE(donnees_conformes(projet(id))) && length(projet(id)$etapes) == 18)
# (une version d'avant la 1.1 qui change une valeur du fichier : son enregistrement garde les champs qu'elle ne connaît pas)
f10 <- fichier("modifie_par_1.0.nuee")
demande("enregistrer", list(projet = id, chemin = f10))
j <- jsonlite::fromJSON(f10, simplifyVector = FALSE)
j$datasets[[1]]$values[[1]][[2]] <- 4321
jsonlite::write_json(j, f10, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
r10 <- demande("ouvrir", list(chemin = f10))
verifier("un fichier changé par la 1.0 : vu à la réouverture", isFALSE(demande("etapes", list(projet = r10$id))$conformes))
verifier("un nombre relu du fichier n'en est pas un changement", {
  f11 <- fichier("relu.nuee")
  demande("enregistrer", list(projet = id, chemin = f11))
  isTRUE(demande("etapes", list(projet = demande("ouvrir", list(chemin = f11))$id))$conformes)
})
# (la 1.0, son lire_projet tel qu'à l'étiquette v1.0.0, lit les données et les modèles d'un projet de la 1.1)
ancien <- tryCatch(suppressWarnings(system2("git", c("show", "v1.0.0:moteur/R/projet.R"), stdout = TRUE, stderr = FALSE)), error = function(e) character(0))
if (length(ancien) && is.null(attr(ancien, "status")) && any(grepl("^lire_projet <- function", ancien))) {
  v10 <- new.env(parent = globalenv())
  eval(parse(text = ancien, encoding = "UTF-8"), v10)
  l10 <- v10$lire_projet(f10)
  l11 <- lire_projet(f10)
  verifier("lu par le lire_projet de la 1.0 : les mêmes données, variables, observations et modèles ; la source et les étapes gardées",
           v10$FORMAT_PROJET == 2 && identical(l10$donnees, l11$donnees) && identical(l10$variables, l11$variables) && identical(l10$observations, l11$observations) &&
             identical(l10$models, l11$models) && identical(l10$etapes, l11$etapes) && identical(l10$source_donnees, l11$source_donnees))
} else {
  cat("  (le lire_projet de la 1.0 n'est pas là : git sans l'étiquette v1.0.0)\n")
}

cat("Un projet de KoboToolbox, une table d'une archive d'ODK\n")
kobo <- file.path(racine_nuee, "moteur", "tests", "donnees", "collecte")
copie_kobo <- fichier("kobo")
dir.create(copie_kobo)
invisible(file.copy(file.path(kobo, c("kobo/xml_groupes.xlsx", "formulaire_kobo.xlsx", "odk/groupes.zip")), copie_kobo))
k1 <- demande("kobo_importer", list(chemin = file.path(copie_kobo, "xml_groupes.xlsx"), table = "observation", formulaire = file.path(copie_kobo, "formulaire_kobo.xlsx"), langue = "Français (fr)"))
sk <- projet(k1$id)$source_donnees
verifier("la source d'un import de KoboToolbox : ses réglages, son formulaire", identical(sk$lecteur, "collecte") && identical(sk$collecte$table, "observation") &&
           identical(sk$formulaire$sha256, unname(empreinte_sha(file.path(copie_kobo, "formulaire_kobo.xlsx")))))
pk <- projet(k1$id)
nombre <- Find(function(v) !length(v$categories), Filter(function(v) as.character(v$uid) %in% colnames(pk$donnees), pk$variables))
demande("modifier_valeur", list(projet = k1$id, observation = numeros_observations(pk)[2], variable = nombre$uid, valeur = "77"))
ak <- demande("mettre_a_jour", list(projet = k1$id, apercu = TRUE))
verifier("refait avec les mêmes réglages et le même formulaire : rien ne change, l'étape faite", all(statuts(ak) == "ok") && ak$observations$cellules == 0 &&
           !ak$observations$ajoutees && !length(ak$variables$nouvelles) && !length(ak$variables$absentes))
z1 <- demande("ouvrir", list(chemin = file.path(copie_kobo, "groupes.zip"), brut = TRUE))
z1 <- if (!is.null(z1$choisir)) demande("ouvrir", list(chemin = file.path(copie_kobo, "groupes.zip"), brut = TRUE, table = z1$choisir[[1]])) else z1
sz <- projet(z1$id)$source_donnees
az <- demande("mettre_a_jour", list(projet = z1$id, apercu = TRUE))
verifier("une table d'une archive d'ODK ouverte telle quelle : relue de même", identical(sz$lecteur, "archive") && az$observations$cellules == 0 &&
           !az$observations$ajoutees && !length(az$variables$absentes))

cat("Les scores d'un modèle en variables, rejoués dans un modèle hiérarchique\n")
set.seed(3)
spectres <- function(n, depart) {
  t1 <- rnorm(n)
  t2 <- rnorm(n)
  d <- data.frame(echantillon = sprintf("E%03d", depart + seq_len(n)))
  for (j in 1:5) d[[paste0("a", j)]] <- round(t1 * j / 3 + rnorm(n, 0, 0.2), 4)
  for (j in 1:4) d[[paste0("b", j)]] <- round(t2 * (5 - j) / 2 + rnorm(n, 0, 0.2), 4)
  d$y <- round(t1 - 0.5 * t2 + rnorm(n, 0, 0.3), 4)
  d
}
h1 <- spectres(30, 0)
hier_csv <- fichier("hierarchique.csv")
utils::write.csv(h1, hier_csv, row.names = FALSE)
hp <- demande("ouvrir", list(chemin = hier_csv))
hid <- hp$id
hu <- function(noms) vapply(noms, function(n) uid_de(projet(hid), n), 0)
ba <- demande("ajuster", list(projet = hid, type = "PCA", x = as.list(hu(paste0("a", 1:5))), A = 2, nom = "A"))
bb <- demande("ajuster", list(projet = hid, type = "PCA", x = as.list(hu(paste0("b", 1:4))), A = 1, nom = "B"))
demande("scores_en_variables", list(modele = ba$id))
demande("scores_en_variables", list(modele = bb$id))
haut <- demande("ajuster", list(projet = hid, type = "PLS", x = as.list(hu(c("$A.t1", "$A.t2", "$B.t1"))), y = list(hu("y")), A = 2, nom = "H"))
uid_scores <- hu(c("$A.t1", "$A.t2", "$B.t1"))
utils::write.csv(rbind(h1, spectres(10, 30)), hier_csv, row.names = FALSE)
ah <- demande("mettre_a_jour", list(projet = hid))
ph <- projet(hid)
verifier("les deux étapes de scores rejouées, les modèles de base ajustés de nouveau sur 40 observations",
         all(statuts(ah) == "ok") && length(ph$observations) == 40 && all(!is.na(valeurs(ph, "$A.t1"))) && identical(hu(c("$A.t1", "$A.t2", "$B.t1")), uid_scores))
refaitA <- ajuster(ph, definition(ph, "A"))
verifier("les scores rejoués sont ceux du modèle de base réajusté sur les données mises à jour",
         length(definition(ph, "A")$observations) == 40 && isTRUE(all.equal(unname(valeurs(ph, "$A.t1")[noms_obs(ph, refaitA$observations)]), unname(refaitA$T[, 1]), tolerance = 1e-10)))
hh <- demande("ajuster", list(projet = hid, numero = definition(ph, "H")$number))
verifier("le modèle hiérarchique réajusté sur ses scores mis à jour, ses 40 observations", hh$N == 40 && hh$A == 2)

cat("Une catégorie nouvelle : la référence des régressions gardée, un terme de plus\n")
# trois zones de dix relevés ; l'année suivante, une zone D de six relevés en plus
set.seed(8)
zones <- function(noms, zone, graine) {
  set.seed(graine)
  x <- round(rnorm(length(noms)), 3)
  data.frame(nom = noms, zone = zone, x = x, y = round(c(A = 1, B = 2, C = 3, D = 4)[zone] + 0.5 * x + rnorm(length(noms), 0, 0.3), 3))
}
z1 <- zones(sprintf("z%02d", 1:30), rep(c("A", "B", "C"), each = 10), 1)
zones_csv <- fichier("zones.csv")
utils::write.csv(z1, zones_csv, row.names = FALSE)
zp <- demande("ouvrir", list(chemin = zones_csv))
zid <- zp$id
zu <- function(nom) uid_de(projet(zid), nom)
zo <- function(noms) numeros_observations(projet(zid), noms)
r_tous <- demande("ajuster", list(projet = zid, type = "LM", x = list(zu("zone"), zu("x")), y = list(zu("y")), echelle = "none", nom = "R"))
r_ab <- demande("ajuster", list(projet = zid, type = "LM", x = list(zu("zone")), y = list(zu("y")), nom = "RB", exclues = zo(z1$nom[z1$zone == "C"])))
pls_z <- demande("ajuster", list(projet = zid, type = "PLS", x = list(zu("x"), zu("zone")), y = list(zu("y")), A = 1, nom = "P",
                                 blocs = stats::setNames(list("Z"), zu("zone")), modificateurs = stats::setNames(list(0.5), zu("zone"))))
termes_de <- function(p, nom) definition(p, nom)$x
coef_de <- function(m) stats::setNames(m$coefficients_regression$estimation, m$coefficients_regression$terme)
verifier("avant : la référence de R est C, celle de RB est B (C laissée de côté)",
         !"zone(C)" %in% names(coef_de(r_tous)) && all(c("zone(A)", "zone(B)") %in% names(coef_de(r_tous))) && identical(names(coef_de(r_ab)), c("(Intercept)", "zone(A)")))
avant_z <- projet(zid)
z2 <- rbind(z1, zones(sprintf("z%02d", 31:36), rep("D", 6), 2))
utils::write.csv(z2, zones_csv, row.names = FALSE)
# (RB n'a pas de relevés de C : D, avant C mais après B, deviendrait sa dernière catégorie observée)
ap_z <- demande("mettre_a_jour", list(projet = zid, apercu = TRUE))
rc <- ap_z$modeles$references_changees
verifier("l'aperçu : RB, s'il prenait les relevés de D, changerait de référence (B, puis D) ; dit",
         length(rc) == 1 && identical(rc[[1]]$nom, "RB") && identical(rc[[1]]$variable, "zone") && identical(rc[[1]]$avant, "B") && identical(rc[[1]]$apres, "D") &&
           identical(rc[[1]]$numero, r_ab$numero))
az <- demande("mettre_a_jour", list(projet = zid, sans_nouvelles = list("RB")))
pz <- projet(zid)
verifier("D avant la dernière catégorie, C : ses numéros suivis dans les données et les termes",
         identical(categories_de(pz, "zone"), c("A", "B", "D", "C")) && identical(unname(valeurs(pz, "zone")), c(z2$zone)) &&
           identical(vapply(Filter(function(t) startsWith(t$name, "zone"), termes_de(pz, "R")), function(t) as.numeric(t$categories), 0), c(1, 2, 3, 4)) &&
           identical(vapply(Filter(function(t) startsWith(t$name, "zone"), termes_de(pz, "R")), `[[`, "", "name"), c("zone(A)", "zone(B)", "zone(D)", "zone(C)")))
r2 <- demande("ajuster", list(projet = zid, numero = r_tous$numero))
reference_c <- stats::lm(y ~ zone + x, data = transform(z2, zone = stats::relevel(factor(z2$zone), ref = "C")))
co <- coef_de(r2)
verifier("R réajusté : la même référence, C ; les coefficients de A et B toujours contre elle, celui de D en plus, = lm() avec C en référence",
         r2$N == 36 && !"zone(C)" %in% names(co) && isTRUE(all.equal(unname(co[c("(Intercept)", "zone(A)", "zone(B)", "zone(D)", "x")]),
                                                                      unname(stats::coef(reference_c)[c("(Intercept)", "zoneA", "zoneB", "zoneD", "x")]), tolerance = 1e-8)))
tp <- Filter(function(t) startsWith(t$name, "zone"), termes_de(pz, "P"))
verifier("la PLS prend le terme zone(D), avant zone(C), avec l'échelle, le bloc et le modificateur des autres",
         identical(vapply(tp, `[[`, "", "name"), c("zone(A)", "zone(B)", "zone(D)", "zone(C)")) && all(vapply(tp, function(t) identical(t$scaling, tp[[1]]$scaling), TRUE)) &&
           identical(tp[[1]]$scaling$block, "Z") && identical(tp[[1]]$scaling$modifier, 0.5) && demande("ajuster", list(projet = zid, numero = pls_z$numero))$K == 5)
verifier("le rapport : les modèles qui prennent le terme de D (RB aussi, sans ses relevés), aucune référence changée",
         setequal(vapply(az$modeles$nouvelles_categories, `[[`, "", "nom"), c("R", "RB", "P")) &&
           all(vapply(az$modeles$nouvelles_categories, function(x) identical(x$variable, "zone") && identical(as.character(x$categories), "D"), TRUE)) &&
           !length(az$modeles$references_changees))
r3 <- demande("ajuster", list(projet = zid, numero = r_ab$numero))
verifier("RB réajusté sans les relevés de D : toujours B en référence", r3$N == 20 && identical(names(coef_de(r3)), c("(Intercept)", "zone(A)")))
demande("annuler", list(projet = zid))
verifier("Ctrl+Z : les catégories, les données et les définitions d'avant", identical(projet(zid)$variables, avant_z$variables) &&
           identical(projet(zid)$donnees, avant_z$donnees) && identical(projet(zid)$models, avant_z$models))
# l'année suivante sans la zone B : la référence de RB (ses lignes de A et B) ne peut pas rester
utils::write.csv(z2[z2$zone != "B", ], zones_csv, row.names = FALSE)
ab <- demande("mettre_a_jour", list(projet = zid, apercu = TRUE, sans_nouvelles = list("RB")))
rc <- ab$modeles$references_changees
verifier("une catégorie de référence qui n'a plus de lignes dans un modèle : dite, avant et après ; R garde la sienne",
         length(rc) == 1 && identical(rc[[1]]$nom, "RB") && identical(rc[[1]]$avant, "B") && identical(rc[[1]]$apres, "A"))
classes_csv <- fichier("classes.csv")
utils::write.csv(transform(z1, groupe = rep(c("g1", "g2"), 15)), classes_csv, row.names = FALSE)
cp <- demande("ouvrir", list(chemin = classes_csv))
cu <- function(nom) uid_de(projet(cp$id), nom)
da <- demande("ajuster", list(projet = cp$id, type = "PLS-DA", x = list(cu("x"), cu("y")), classe = cu("groupe"), A = 1, nom = "DA"))
utils::write.csv(rbind(transform(z1, groupe = rep(c("g1", "g2"), 15)), data.frame(nom = c("z31", "z32", "z33"), zone = "A", x = 1, y = 2, groupe = c("g3", "g1", "g3"))),
                 classes_csv, row.names = FALSE)
demande("mettre_a_jour", list(projet = cp$id))
pc <- projet(cp$id)
dda <- definition(pc, "DA")
verifier("une PLS-DA : la classe nouvelle mise avant la dernière, ses classes suivies ; seules les nouvelles observations de ses classes y entrent",
         identical(categories_de(pc, "groupe"), c("g1", "g3", "g2")) && identical(vapply(dda$y, `[[`, "", "name"), c("groupe(g1)", "groupe(g2)")) &&
           identical(vapply(dda$y, function(t) as.numeric(t$categories), 0), c(1, 3)) && length(dda$observations) == 31 &&
           demande("ajuster", list(projet = cp$id, numero = da$numero))$N == 31)

cat("Les modèles à qui demander s'ils prennent les nouvelles : les lignes qu'ils peuvent ajuster\n")
# douze lignes, x2 manquant dans deux (m03, m07) ; l'année suivante, quatre de plus, x2 manquant dans m15
lignes <- function(noms, graine) {
  set.seed(graine)
  n <- length(noms)
  data.frame(nom = noms, g = rep(c("a", "b"), length.out = n), x1 = round(rnorm(n), 3), x2 = round(rnorm(n), 3), y = round(rnorm(n, 3), 3))
}
l1 <- lignes(sprintf("m%02d", 1:12), 4)
l1$x2[c(3, 7)] <- NA
lignes_csv <- fichier("lignes.csv")
utils::write.csv(l1, lignes_csv, row.names = FALSE, na = "")
lid <- demande("ouvrir", list(chemin = lignes_csv))$id
lu <- function(nom) uid_de(projet(lid), nom)
lo <- function(noms) numeros_observations(projet(lid), noms)
lm_x2 <- function(nom, exclues) demande("ajuster", list(projet = lid, type = "LM", x = list(lu("x2"), lu("x1")), y = list(lu("y")), nom = nom, exclues = lo(exclues)))
m_manque <- lm_x2("Manque", "m05")
m_m03 <- lm_x2("Sans m03", "m03")
m_acp <- demande("ajuster", list(projet = lid, type = "PCA", x = list(lu("x1"), lu("x2")), A = 1, nom = "ACP", exclues = lo("m03")))
lm_g <- function(nom, exclues) demande("ajuster", list(projet = lid, type = "LM", x = list(lu("x1")), y = list(lu("y")), nom = nom, condition = "g == \"a\"", exclues = lo(exclues)))
m_ga <- lm_g("Cond", c("m01", "m02"))
m_gb <- lm_g("Cond b", "m02")
verifier("avant : une régression ajuste les lignes qui ont toutes ses valeurs, une ACP aussi les autres, une condition les siennes",
         m_manque$N == 9 && m_m03$N == 10 && m_acp$N == 11 && m_ga$N == 5 && m_gb$N == 6)
al <- demande("mettre_a_jour", list(projet = lid, apercu = TRUE))
verifier("aucune ligne nouvelle : aucun modèle à qui demander", al$observations$ajoutees == 0 && !length(al$modeles$observations_choisies))
utils::write.csv(rbind(l1, transform(lignes(sprintf("m%02d", 13:16), 5), x2 = c(0.5, -0.2, NA, 1.1))), lignes_csv, row.names = FALSE, na = "")
al <- demande("mettre_a_jour", list(projet = lid, apercu = TRUE))
ch <- stats::setNames(lapply(al$modeles$observations_choisies, function(x) stats::setNames(as.numeric(unlist(x[c("observations", "admises", "nouvelles")])), c("observations", "admises", "nouvelles"))),
                      vapply(al$modeles$observations_choisies, `[[`, "", "nom"))
verifier("les modèles qui ont laissé de côté une ligne qu'ils pouvaient ajuster, comptés en lignes qu'ils ajustent, celles qu'ils pouvaient, celles du fichier",
         identical(names(ch), c("Manque", "ACP", "Cond")) && identical(unname(ch$Manque), c(9, 10, 3)) && identical(unname(ch$ACP), c(11, 12, 4)) &&
           identical(unname(ch$Cond), c(5, 6, 2)) && ch$Manque[["observations"]] == m_manque$N && ch$Cond[["observations"]] == m_ga$N)
verifier("pas ceux qui n'ont laissé de côté qu'une ligne sans x2, ou hors de leur condition", !any(c("Sans m03", "Cond b") %in% names(ch)))
demande("mettre_a_jour", list(projet = lid))
refits <- vapply(c(m_manque$numero, m_acp$numero, m_ga$numero), function(k) as.numeric(demande("ajuster", list(projet = lid, numero = k))$N), 0)
verifier("mis à jour et réajustés : chacun ajuste ses lignes d'avant et les nouvelles qu'il pouvait ajuster", identical(refits, c(12, 15, 7)))

cat("Sans source, sans étapes, sans colonne qui nomme les observations\n")
cl <- demande("coller", list(texte = paste(c("nom\tx\tz", sprintf("c%d\t%d\t%d", 1:6, 1:6, c(3, 1, 4, 1, 5, 9))), collapse = "\n"), nom = "Collé"))
demande("variable_calculee", list(projet = cl$id, nom = "xz", formule = "x + z"))
verifier("un tableau collé : sans source, ses étapes notées", is.null(projet(cl$id)$source_donnees) && length(projet(cl$id)$etapes) == 1 &&
           grepl("donnez le fichier", refus("mettre_a_jour", list(projet = cl$id))) && grepl("pas de fichier source", refus("retirer_etape", list(projet = cl$id, n = 1))))
colle_csv <- fichier("colle.csv")
utils::write.csv(data.frame(nom = sprintf("c%d", 1:8), x = 1:8, z = c(3, 1, 4, 1, 5, 9, 2, 6)), colle_csv, row.names = FALSE)
ac <- demande("mettre_a_jour", list(projet = cl$id, chemin = colle_csv))
verifier("mis à jour d'un fichier donné : ses étapes rejouées, le fichier devenu sa source", all(statuts(ac) == "ok") && ac$observations$ajoutees == 2 &&
           identical(unname(valeurs(projet(cl$id), "xz")[c("c7", "c8")]), c(9, 14)) && identical(projet(cl$id)$source_donnees$chemin, normalizePath(colle_csv, winslash = "/")))
# (un projet de la 1.0 : pas d'étapes ; une variable calculée par la console, sans étape, garde sa formule)
etat$projets[[cl$id]]$etapes <- list()
etat$projets[[cl$id]]$empreinte_donnees <- NULL
uid_xz <- uid_de(projet(cl$id), "xz")
utils::write.csv(data.frame(nom = sprintf("c%d", 1:9), x = 1:9, z = c(3, 1, 4, 1, 5, 9, 2, 6, 5)), colle_csv, row.names = FALSE)
a0 <- demande("mettre_a_jour", list(projet = cl$id, apercu = TRUE))
verifier("sans étapes : une donnée d'avant la 1.1, conformité inconnue", is.na(demande("etapes", list(projet = cl$id))$conformes))
demande("mettre_a_jour", list(projet = cl$id))
verifier("la variable calculée, absente du fichier, refaite d'après sa formule sous son numéro",
         identical(as.character(a0$variables$recalculees), "xz") && !length(a0$variables$absentes) && uid_de(projet(cl$id), "xz") == uid_xz &&
           unname(valeurs(projet(cl$id), "xz")["c9"]) == 14)
pos_csv <- fichier("sans_noms.csv")
utils::write.csv(data.frame(mesure = c(2.5, 3.1, 4.2, 1.8), autre = c(1, 2, 3, 4)), pos_csv, row.names = FALSE)
pp <- demande("ouvrir", list(chemin = pos_csv))
demande("modifier_valeur", list(projet = pp$id, observation = 3, variable = uid_de(projet(pp$id), "autre"), valeur = "30"))
utils::write.csv(data.frame(mesure = c(2.5, 3.1, 4.2, 1.8, 2.2), autre = c(1, 2, 3, 4, 5)), pos_csv, row.names = FALSE)
ap <- demande("mettre_a_jour", list(projet = pp$id, apercu = TRUE))
verifier("un tableau sans colonne qui nomme les observations : retrouvées par leur place, et dit", isTRUE(ap$observations$par_position) &&
           ap$observations$ajoutees == 1 && identical(parametres(projet(pp$id), 1)$observation, "3") && all(statuts(ap) == "ok"))

# Un modèle ajusté avant un nouvel ordre des catégories (Édition › Catégories) reste à jour, sa
# définition rapportée au nouvel ordre ; il prédit sur le projet tel qu'il l'a vu (serveur.R :
# pour_predire). Une forêt, des arbres boostés, un arbre, une LDA aux X qualitatifs donnent les mêmes
# prédictions qu'avant : ses observations, un jeu de prédiction, un fichier (un modèle hiérarchique et
# son modèle de base compris), les classes sous leurs noms ; un nom changé depuis, refusé.
cat("Prédire après un nouvel ordre des catégories\n")
set.seed(3)
n_o <- 60
hab <- sample(c("forêt", "lande", "prairie"), n_o, TRUE)
sol <- ifelse(hab == "forêt", sample(c("argile", "sable"), n_o, TRUE, c(.8, .2)),
              ifelse(hab == "lande", sample(c("argile", "sable", "limon"), n_o, TRUE), sample(c("limon", "sable"), n_o, TRUE, c(.7, .3))))
alt <- round(runif(n_o, 50, 500))
pente <- round(runif(n_o, 0, 30), 1)
d_o <- data.frame(nom = paste0("R", seq_len(n_o)), habitat = hab, sol = sol, altitude = alt, pente = pente,
                  densite = round(c(forêt = 10, lande = 4, prairie = 7)[hab] + 0.01 * alt + 0.1 * pente + rnorm(n_o), 2))
f_o <- fichier("ordre.csv")
utils::write.csv(d_o, f_o, row.names = FALSE)
f_pred <- fichier("ordre_a_predire.csv")
utils::write.csv(d_o[1:15, c("nom", "habitat", "sol", "altitude", "pente")], f_pred, row.names = FALSE)
io <- demande("ouvrir", list(chemin = f_o))$id
uo <- function(nom) uid_de(projet(io), nom)
x3 <- list(uo("habitat"), uo("altitude"), uo("pente"))
ordre_modeles <- list(
  foret = list(type = "FOREST", x = x3, y = list(uo("densite")), options = list(arbres = 100)),
  boost = list(type = "BOOST", x = x3, y = list(uo("densite")), options = list(arbres = 100)),
  arbre = list(type = "TREE", x = x3[1:2], y = list(uo("densite"))),
  lda = list(type = "LDA", x = x3, classe = uo("sol"), echelle = "UV"),
  foret_classes = list(type = "FOREST", x = x3[1:2], classe = uo("sol"), options = list(arbres = 100)),
  base = list(type = "PCA", x = x3, A = 2))
ids_o <- vapply(names(ordre_modeles), function(k) demande("ajuster", c(list(projet = io, nom = k), ordre_modeles[[k]]))$id, "")
demande("scores_en_variables", list(modele = ids_o[["base"]]))
ids_o[["haut"]] <- demande("ajuster", list(projet = io, type = "PLS", x = list(uo("$base.t1"), uo("$base.t2"), uo("altitude")), y = list(uo("densite")), A = 1, nom = "haut"))$id
etat$projets[[io]]$prediction_sets <- list(list(name = "J", observations = numeros_observations(projet(io))[1:10]))
predictions_o <- function(id) list(obs = demande("predire", list(modele = id))$observations, jeu = demande("predire", list(modele = id, jeu = 1))$observations,
                                   fichier = demande("predire_fichier", list(modele = id, chemin = f_pred))$observations)
avant_o <- lapply(ids_o, predictions_o)
cats_o <- function(nom) categories_de(projet(io), nom)
demande("categories", list(projet = io, variable = uo("habitat"), noms = cats_o("habitat"), ordre = rev(cats_o("habitat"))))
demande("categories", list(projet = io, variable = uo("sol"), noms = cats_o("sol"), ordre = rev(cats_o("sol"))))
apres_o <- lapply(ids_o, predictions_o)
memes_o <- vapply(names(ids_o), function(k) isTRUE(all.equal(avant_o[[k]], apres_o[[k]], tolerance = 1e-12)), TRUE)
verifier(sprintf("le nouvel ordre fait (%s ; %s), et les mêmes prédictions qu'avant : %s", paste(cats_o("habitat"), collapse = ", "), paste(cats_o("sol"), collapse = ", "),
                 paste(sprintf("%s %s", names(memes_o), ifelse(memes_o, "ok", "CHANGÉ")), collapse = ", ")),
         identical(cats_o("habitat"), c("prairie", "lande", "forêt")) && identical(cats_o("sol"), c("sable", "limon", "argile")) && all(memes_o))
cl_o <- apres_o$lda$obs
verifier("les classes d'une LDA et d'une forêt de classement sous leurs noms, leurs probabilités nommées par elles",
         all(cl_o[["classe prédite"]] %in% c("argile", "limon", "sable")) && identical(cl_o[["classe observée"]], unname(valeurs(projet(io), "sol"))) &&
           all(paste("probabilité", c("argile", "limon", "sable")) %in% names(cl_o)) && all(apres_o$foret_classes$fichier[["classe prédite"]] %in% c("argile", "limon", "sable")))
# (un nom changé : la forêt n'est plus à jour, et le modèle de base du modèle hiérarchique non plus)
demande("categories", list(projet = io, variable = uo("habitat"), noms = sub("forêt", "bois", cats_o("habitat")), ordre = sub("forêt", "bois", cats_o("habitat"))))
verifier("un nom changé depuis : prédire refusé, d'un fichier aussi, et par le modèle de base d'un modèle hiérarchique",
         grepl("ont changé depuis l'ajustement de foret", refus("predire", list(modele = ids_o[["foret"]]))) &&
           grepl("ont changé depuis l'ajustement de foret", refus("predire_fichier", list(modele = ids_o[["foret"]], chemin = f_pred))) &&
           grepl("ont changé depuis l'ajustement de base", refus("predire_fichier", list(modele = ids_o[["haut"]], chemin = f_pred))))

cat("Une variable renommée, supprimée, en catégories : des étapes qui se rejouent\n")
# (quatre sites codés 1 à 4, une mesure, une colonne de remarques laissée par le terrain, une réponse ;
# l'année suivante, un cinquième site et des lignes de plus)
releves_v <- function(annees, sites, graine) {
  set.seed(graine)
  l <- expand.grid(annee = annees, site = sites)
  data.frame(nom = sprintf("S%d-%d", l$site, l$annee), site = l$site, annee = l$annee, mesure = round(rnorm(nrow(l), 10, 2), 2),
             remarques = sample(c("ras", "pluie", "vent"), nrow(l), TRUE), y = round(1 + 0.3 * l$site + rnorm(nrow(l), 0, 0.2), 3))
}
dossier_v <- file.path(dossier, "variables")
dir.create(dossier_v)
f_v <- file.path(dossier_v, "releves.csv")
utils::write.csv(releves_v(2016:2020, 1:4, 3), f_v, row.names = FALSE)
vid <- demande("ouvrir", list(chemin = f_v))$id
uv <- function(nom) uid_de(projet(vid), nom)
m_v <- demande("ajuster", list(projet = vid, type = "LM", x = list(uv("mesure"), uv("site")), y = list(uv("y")), nom = "R1", condition = "mesure > 5"))
doc <- demande("document_enregistrer", list(projet = vid, titre = "Rapport", texte = "La mesure : {{R1.coef.mesure}}, {{R1.est.mesure:3 #m | sa pente}} ; R² {{R1.R2}}."))
avant_v <- projet(vid)
ap <- demande("renommer_variable", list(projet = vid, variable = uv("mesure"), nom = "mesure (cm)", apercu = TRUE))
verifier("renommer, l'aperçu : ce qui suivra (la définition de R1, deux références), R1 ajusté avec l'ancien nom ; rien ne change",
         identical(as.character(ap$renommage$modeles), "R1") && ap$renommage$references == 2 && identical(as.numeric(ap$renommage$ajustes), definition(avant_v, "R1")$number) &&
           identical(projet(vid), avant_v))
rn <- demande("renommer_variable", list(projet = vid, variable = uv("mesure"), nom = "mesure (cm)"))
pv <- projet(vid)
dr <- definition(pv, "R1")
verifier("renommée : la variable, son étape (l'ancien nom et le nouveau), un seul Ctrl+Z nommé",
         "mesure (cm)" %in% pv$noms_var && !"mesure" %in% pv$noms_var && identical(derniere(pv)$demande, "renommer_variable") &&
           identical(parametres(pv, length(pv$etapes))[c("variable", "nom")], list(variable = "mesure", nom = "mesure (cm)")) &&
           identical(rn$annulable, "mesure renommée mesure (cm)") && identical(etiquette_etape(derniere(pv)), "mesure renommée mesure (cm)"))
verifier("la définition de R1 suit (son terme, sa condition entre accents graves) ; les références du document aussi, leur format, étiquette et légende gardés",
         "mesure (cm)" %in% vapply(dr$x, `[[`, "", "name") && identical(texte_condition(dr$condition), "`mesure (cm)` > 5") &&
           identical(Find(function(d) identical(d$id, doc$id), pv$documents)$texte,
                     "La mesure : {{R1.coef.mesure (cm)}}, {{R1.est.mesure (cm):3 #m | sa pente}} ; R² {{R1.R2}}.") &&
           identical(as.character(rn$renommage$documents), doc$id))
verifier("l'ajustement de la séance, fait avec l'ancien nom : réajuster est refusé en le disant ; la définition du projet s'ajuste, ses coefficients sous le nouveau nom, les mêmes valeurs",
         grepl("s'appelle « mesure (cm) » depuis l'ajustement de R1", refus("reajuster", list(modele = m_v$id, exclues = list(1))), fixed = TRUE) &&
           { r1 <- demande("ajuster", list(projet = vid, numero = dr$number)); co <- r1$coefficients_regression
             "mesure (cm)" %in% co$terme && isTRUE(all.equal(unname(co$estimation), unname(m_v$coefficients_regression$estimation))) })
tv <- demande("type_variable", list(projet = vid, variable = uv("site"), type = "qualitative"))
pv <- projet(vid)
verifier("le site en catégories : 1 à 4 dans l'ordre des nombres, R1 prend un terme par site, l'étape notée",
         identical(categories_de(pv, "site"), c("1", "2", "3", "4")) && identical(unname(valeurs(pv, "site")[1:2]), c("1", "1")) &&
           identical(vapply(Filter(function(t) startsWith(t$name, "site"), definition(pv, "R1")$x), `[[`, "", "name"), sprintf("site(%d)", 1:4)) &&
           identical(parametres(pv, length(pv$etapes))[c("variable", "type")], list(variable = "site", type = "qualitative")) && identical(tv$annulable, "site en catégories"))
sv <- demande("supprimer_variable", list(projet = vid, variable = uv("remarques")))
pv <- projet(vid)
verifier("les remarques supprimées : leur colonne, l'étape notée ; une variable qu'utilise R1 ne se supprime pas, en le disant",
         !"remarques" %in% pv$noms_var && !as.character(uid_de(avant_v, "remarques")) %in% colnames(pv$donnees) && identical(sv$annulable, "suppression de remarques") &&
           identical(parametres(pv, length(pv$etapes))$variable, "remarques") && grepl("R1 utilise « y »", refus("supprimer_variable", list(projet = vid, variable = uv("y")))))
verifier("les données encore celles de leurs étapes", isTRUE(donnees_conformes(pv)))
a_v <- demande("mettre_a_jour", list(projet = vid, apercu = TRUE))
verifier("rejouées sur le même fichier : les trois étapes faites, les mêmes données (aucune variable de plus ou de moins, aucune cellule changée)",
         all(statuts(a_v) == "ok") && length(a_v$etapes) == 3 && a_v$observations$cellules == 0 && !length(a_v$variables$nouvelles) &&
           !length(a_v$variables$absentes) && !length(a_v$variables$types))
f_v2 <- file.path(dossier_v, "releves 2021.csv")
utils::write.csv(rbind(releves_v(2016:2020, 1:4, 3), releves_v(2021, 1:5, 4)), f_v2, row.names = FALSE)
mv <- demande("mettre_a_jour", list(projet = vid, chemin = f_v2))
pv <- projet(vid)
verifier("l'année suivante : renommée, en catégories et supprimée de nouveau ; le cinquième site avant la dernière catégorie ; R1 suit",
         all(statuts(mv) == "ok") && length(pv$observations) == 25 && "mesure (cm)" %in% pv$noms_var && !"remarques" %in% pv$noms_var &&
           identical(categories_de(pv, "site"), c("1", "2", "3", "5", "4")) && unname(valeurs(pv, "site")["S5-2021"]) == "5" &&
           "site(5)" %in% vapply(definition(pv, "R1")$x, `[[`, "", "name") && isTRUE(donnees_conformes(pv)))
demande("annuler", list(projet = vid))
demande("annuler", list(projet = vid))
verifier("Ctrl+Z (la mise à jour, la suppression) : les remarques revenues, deux étapes", "remarques" %in% projet(vid)$noms_var && length(projet(vid)$etapes) == 2 &&
           isTRUE(donnees_conformes(projet(vid))))
demande("retablir", list(projet = vid))
verifier("Ctrl+Y : supprimées de nouveau, trois étapes, les données d'accord", !"remarques" %in% projet(vid)$noms_var && length(projet(vid)$etapes) == 3 &&
           isTRUE(donnees_conformes(projet(vid))))
demande("annuler", list(projet = vid))
demande("annuler", list(projet = vid))
texte_doc <- function() Find(function(d) identical(d$id, doc$id), projet(vid)$documents)$texte
u_r <- demande("annuler", list(projet = vid))
verifier("Ctrl+Z du nom : l'ancien nom, sa définition et ses références revenues ; le document dit à relire",
         "mesure" %in% projet(vid)$noms_var && identical(texte_doc(), avant_v$documents[[1]]$texte) && identical(as.character(u_r$documents), doc$id) &&
           identical(texte_condition(definition(projet(vid), "R1")$condition), "mesure > 5") && !length(projet(vid)$etapes))
demande("retablir", list(projet = vid))
verifier("Ctrl+Y : le nouveau nom, ses références de nouveau", "mesure (cm)" %in% projet(vid)$noms_var && grepl("{{R1.coef.mesure (cm)}}", texte_doc(), fixed = TRUE))
# (un document écrit depuis le changement de nom : Annuler remet ses références, sans toucher au reste)
demande("document_enregistrer", list(projet = vid, id = doc$id, texte = paste(texte_doc(), "Ajouté : {{R1.p.mesure (cm)}}.")))
demande("annuler", list(projet = vid))
verifier("un document écrit depuis : Annuler remet ses références à l'ancien nom, et garde ce qui y a été écrit",
         identical(texte_doc(), paste(avant_v$documents[[1]]$texte, "Ajouté : {{R1.p.mesure}}.")))
demande("retablir", list(projet = vid))
verifier("et Rétablir les remet au nouveau", grepl("Ajouté : {{R1.p.mesure (cm)}}.", texte_doc(), fixed = TRUE) && grepl("{{R1.coef.mesure (cm)}}", texte_doc(), fixed = TRUE))

cat("En anglais\n")
options(nuee.langue = "en")
en <- demande("mettre_a_jour", list(projet = id, chemin = manque, apercu = TRUE))
verifier("the reasons of skipped steps in English", grepl("missing from the data", en$etapes[[2]]$raison) &&
           grepl("outside the steps", { etat$projets[[id]]$donnees[2, 4] <- -1; reviser(id); demande("etapes", list(projet = id))$raison }))
options(nuee.langue = "fr")

# Le R d'une distribution Linux plus ancienne que 4.5 n'a pas tools::sha256sum : ses empreintes sont des
# MD5 préfixés, comparées seulement à des empreintes du même algorithme ; les données se comparent alors
# par leurs sommes
cat("Empreintes sans SHA-256 (R antérieur à 4.5)\n")
f_md5 <- tempfile(fileext = ".csv")
writeLines(c("a,b", "1,2"), f_md5)
m5 <- empreinte_sha(f_md5, md5 = TRUE)
ed <- empreinte_donnees(projet(id))
# (les mêmes données d'après un R récent, un SHA-256, et d'après un plus ancien, un MD5 : quel que soit ce R,
# dont l'empreinte est l'une ou l'autre)
ed_sha <- modifyList(ed, list(structure = strrep("a", 64)))
ed_md5 <- modifyList(ed, list(structure = empreinte_sha(octets = charToRaw("x"), md5 = TRUE)))
verifier("empreinte MD5 d'un fichier et d'octets, préfixée ; comparée au même algorithme ; données comparées par leurs sommes",
         identical(m5, paste0("md5:", unname(tools::md5sum(f_md5)))) && startsWith(ed_md5$structure, "md5:") &&
           isTRUE(meme_empreinte(m5, f_md5)) && isFALSE(meme_empreinte(paste0("md5:", strrep("0", 32)), f_md5)) &&
           memes_donnees(ed_sha, ed_md5) && !memes_donnees(ed_sha, modifyList(ed_md5, list(sommes = as.list(unlist(ed$sommes) + 1)))))
unlink(f_md5)

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
