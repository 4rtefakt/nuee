# Vérifie l'atelier des données (moteur/R/atelier.R) :
#   - Chercher et remplacer : des nombres (« -9999 » → vide, vide → 0, « 3,5 » qui vaut 3.5, un texte
#     qui ne remplace pas un nombre), du texte (la cellule entière, les majuscules comptant ou non,
#     une partie du texte, une catégorie fondue dans une autre, vidée, ajoutée), les cellules vides,
#     une ou plusieurs variables, l'aperçu qui ne change rien ; une variable qualitative devenue
#     quantitative (« , » → « . », « <0,5 » → 0,25), ou laissée qualitative sur demande ;
#   - les catégories : renommées, fusionnées, rangées, la référence d'une régression (la dernière
#     catégorie observée) après un nouvel ordre, contre lm() avec relevel() ; les définitions des
#     modèles du projet qui suivent (même ajustement qu'un modèle défini après), une fusion qui
#     dédouble les termes ; un modèle de la séance rapporté aux nouvelles catégories, ou à ajuster
#     à nouveau après un nouveau nom ; les refus ;
#   - un bloc de cellules collé (des nombres, une case vidée, une catégorie ajoutée, un refus) ;
#   - les unités et descriptions : gardées, enregistrées dans un .nuee et relues, un projet sans
#     elles qui s'ouvre ; dans l'export (feuille « Variables » d'un classeur, table d'un GeoPackage,
#     description de jamovi, attributs de R ; rien en CSV) et relues à l'ouverture d'un classeur ou
#     d'un GeoPackage ; dans le texte des Méthodes (la réponse et son unité), le texte des résultats
#     (« par m ») et le script R (en commentaire, qui tourne encore).
#   Rscript moteur/tests/verifier_donnees.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
erreur <- function(expr) tryCatch({ expr; "" }, error = conditionMessage)
contient <- function(textes, motif) any(grepl(motif, textes, fixed = TRUE))
dossier <- tempfile("donnees")
dir.create(dossier)

# Les valeurs d'une variable par son nom (le nom de la catégorie pour une qualitative), ses catégories
valeurs <- function(p, nom) {
  v <- variable_du_projet(p, numeros(p, nom))
  x <- unname(colonne(p, v$uid, numeros_observations(p)))
  if (length(v$categories)) v$categories[x] else x
}
categories_de <- function(p, nom) as.character(variable_du_projet(p, numeros(p, nom))$categories %||% character(0))
info <- function(p, nom) Find(function(x) identical(x$nom, nom), attr(p, "remplacement")$variables)

# Des relevés de terrain comme on les reçoit : des comptages avec -9999 pour « pas de relevé » et des
# cases vides, un habitat mal orthographié, une teneur écrite avec des virgules, « abs » et « <0,5 »
set.seed(5)
n <- 40
habitats <- c("forêt", "Forêt", "foret", "prairie", "prarie", "lande")
df <- data.frame(nom = sprintf("R%02d", 1:n), altitude = round(runif(n, 100, 900)), comptage = rpois(n, 6),
                 habitat = sample(habitats, n, TRUE, prob = c(5, 1, 1, 5, 1, 4)),
                 teneur = sprintf("%.1f", runif(n, 1, 9)), stringsAsFactors = FALSE)
df$comptage[c(3, 8, 15)] <- -9999
df$comptage[c(5, 21)] <- NA
df$teneur <- sub(".", ",", df$teneur, fixed = TRUE)
df$teneur[c(2, 9, 30)] <- "abs"
df$teneur[c(4, 12)] <- "<0,5"
df$teneur[c(6, 17, 25, 33, 38)] <- c("abs", "abs", "nd", "abs", "abs")
chemin_csv <- file.path(dossier, "releves.csv")
utils::write.csv(df, chemin_csv, row.names = FALSE, na = "")
p <- importer_tableau(chemin_csv)
verifier("le tableau lu : comptage en nombres, habitat et teneur en texte (« abs » y est du texte)",
         !length(categories_de(p, "comptage")) && length(categories_de(p, "habitat")) == 6 && length(categories_de(p, "teneur")) > 0)

cat("Chercher et remplacer : des nombres\n")
a <- remplacer_valeurs(p, numeros(p, "comptage"), "-9999", "")
verifier("« -9999 » → vide : 3 cellules, les autres valeurs gardées", attr(a, "remplacement")$cellules == 3 &&
           sum(is.na(valeurs(a, "comptage"))) == 5 && identical(valeurs(a, "comptage")[-c(3, 5, 8, 15, 21)], df$comptage[-c(3, 5, 8, 15, 21)]))
verifier("l'aperçu de ce remplacement, sans rien changer au projet", identical(valeurs(p, "comptage"), df$comptage) &&
           identical(info(a, "comptage")$exemples, "« -9999 » → vide (3)"))
b <- remplacer_valeurs(a, numeros(a, "comptage"), vides = TRUE, remplacer = "0")
verifier("vide → 0 : les 5 cellules vides", attr(b, "remplacement")$cellules == 5 && sum(valeurs(b, "comptage") == 0) == 5 + sum(df$comptage == 0, na.rm = TRUE))
pa <- projet_depuis_tableau(data.frame(nom = c("a", "b", "c"), x = c(3.5, 2, 3.5), stringsAsFactors = FALSE), "x")
c1 <- remplacer_valeurs(pa, NULL, "3,5", "4")
verifier("« 3,5 » cherché dans des nombres vaut 3.5", identical(valeurs(c1, "x"), c(4, 2, 4)))
c2 <- remplacer_valeurs(pa, NULL, "3.5", "beaucoup")
verifier("un texte ne remplace pas un nombre : les cellules restent, comptées et dites", attr(c2, "remplacement")$cellules == 0 &&
           info(c2, "x")$ignorees == 2 && identical(valeurs(c2, "x"), c(3.5, 2, 3.5)) && contient(attr(c2, "remplacement")$phrases, "est quantitative"))
verifier("rien à chercher : refusé", nzchar(erreur(remplacer_valeurs(pa, NULL, "  ", "1"))))
verifier("une variable inconnue : refusée", nzchar(erreur(remplacer_valeurs(pa, 99, "2", "1"))))

cat("Chercher et remplacer : du texte\n")
h <- numeros(p, "habitat")
avant <- valeurs(p, "habitat")
t1 <- remplacer_valeurs(p, h, "foret", "forêt")
verifier("la cellule entière, sans les majuscules : « foret » et « Forêt »… non, seulement « foret » (é ≠ e)",
         identical(valeurs(t1, "habitat"), ifelse(avant == "foret", "forêt", avant)))
t2 <- remplacer_valeurs(t1, h, "FORÊT", "forêt")
verifier("sans les majuscules, « FORÊT » trouve « Forêt » et « forêt » ; une catégorie fondue dans l'autre",
         identical(valeurs(t2, "habitat"), ifelse(avant %in% c("foret", "Forêt"), "forêt", avant)) && !"Forêt" %in% categories_de(t2, "habitat") &&
           attr(t2, "remplacement")$cellules == sum(avant == "Forêt"))
t3 <- remplacer_valeurs(t1, h, "FORÊT", "forêt", casse = TRUE)
verifier("les majuscules comptant, « FORÊT » ne trouve rien", attr(t3, "remplacement")$cellules == 0)
t4 <- remplacer_valeurs(t2, h, "prarie", "prairie")
verifier("l'ordre des catégories gardé, la faute fondue dans « prairie »", identical(categories_de(t4, "habitat"), c("forêt", "lande", "prairie")))
t5 <- remplacer_valeurs(t4, h, "rai", "RAI", partie = TRUE)
verifier("une partie du texte : « prairie » → « pRAIrie »", identical(categories_de(t5, "habitat"), c("forêt", "lande", "pRAIrie")))
t6 <- remplacer_valeurs(t4, h, "lande", "")
verifier("« lande » → vide : ses cellules manquent, la catégorie disparaît", sum(is.na(valeurs(t6, "habitat"))) == sum(avant == "lande") &&
           identical(categories_de(t6, "habitat"), c("forêt", "prairie")))
t7 <- remplacer_valeurs(t6, h, vides = TRUE, remplacer = "inconnu")
verifier("vide → « inconnu » : une catégorie de plus, en dernier", identical(categories_de(t7, "habitat"), c("forêt", "prairie", "inconnu")) &&
           sum(valeurs(t7, "habitat") == "inconnu") == sum(avant == "lande"))
t8 <- remplacer_valeurs(t4, NULL, "prairie", "pré")
verifier("dans toutes les variables : seulement là où le texte est", attr(t8, "remplacement")$cellules == sum(avant %in% c("prairie", "prarie")) &&
           length(Filter(function(x) x$cellules > 0, attr(t8, "remplacement")$variables)) == 1)

cat("Une variable qualitative devenue quantitative\n")
te <- numeros(p, "teneur")
q1 <- remplacer_valeurs(p, te, "abs", "0")
verifier("« abs » → 0 : il reste « <0,5 » et « nd », la variable reste qualitative", length(categories_de(q1, "teneur")) > 0 && info(q1, "teneur")$devient == "qualitative")
q2 <- remplacer_valeurs(q1, te, "<0,5", "0,25")
verifier("« <0,5 » → 0,25 : reste « nd »", length(categories_de(q2, "teneur")) > 0)
q3 <- remplacer_valeurs(q2, te, "nd", "")
attendu <- suppressWarnings(as.numeric(sub(",", ".", ifelse(df$teneur == "abs", "0", ifelse(df$teneur == "<0,5", "0.25", ifelse(df$teneur == "nd", NA, df$teneur))))))
verifier("« nd » → vide : toutes ses valeurs sont des nombres, la variable devient quantitative, les virgules lues",
         !length(categories_de(q3, "teneur")) && info(q3, "teneur")$devient == "quantitative" && isTRUE(all.equal(valeurs(q3, "teneur"), attendu)) &&
           contient(attr(q3, "remplacement")$phrases, "devient quantitative"))
q4 <- remplacer_valeurs(q2, te, "nd", "", nombres = FALSE)
verifier("sur demande, elle reste qualitative", length(categories_de(q4, "teneur")) > 0)
pv <- projet_depuis_tableau(data.frame(nom = c("a", "b", "c", "d"), v = c("1,5", "2,5", "x", "x"), stringsAsFactors = FALSE), "v")
v1 <- remplacer_valeurs(pv, NULL, "x", "3")
verifier("« 1,5 » et « 3 » : quantitative, 1.5 2.5 3 3", identical(valeurs(v1, "v"), c(1.5, 2.5, 3, 3)))

cat("Un bloc de cellules collé\n")
o3 <- numeros_observations(p)[2:4]
vb <- numeros(p, c("altitude", "habitat"))
cb <- coller_cellules(p, o3, vb, matrix(c("250", "3,5", "", "forêt", "marais", NA), 3, 2))
verifier("collé : des nombres (« 3,5 » compris), une case vidée, une catégorie ajoutée, une case laissée (NA)",
         identical(valeurs(cb, "altitude")[2:4], c(250, 3.5, NA)) && identical(valeurs(cb, "habitat")[2:3], c("forêt", "marais")) &&
           identical(valeurs(cb, "habitat")[4], valeurs(p, "habitat")[4]) && "marais" %in% categories_de(cb, "habitat") &&
           attr(cb, "collage")$cellules == 5 - (valeurs(p, "habitat")[2] == "forêt"))
verifier("une liste de lignes (le JSON) comme une matrice", identical(valeurs(coller_cellules(p, o3[1:2], vb[1], list(list("1"), list("2"))), "altitude")[2:3], c(1, 2)))
verifier("un texte dans une variable quantitative : refusé, l'observation nommée", grepl("(observation R03)", erreur(coller_cellules(p, o3, vb[1], matrix(c("1", "abc", "2")))), fixed = TRUE))
verifier("un bloc qui ne va pas avec ses lignes et ses colonnes : refusé", nzchar(erreur(coller_cellules(p, o3, vb, matrix("1", 2, 2)))))

cat("Les catégories\n")
# l'habitat corrigé (forêt, lande, prairie), une réponse qui en dépend, un modèle défini avant
p4 <- t4
p4 <- variable_calculee(p4, "densite", "ifelse(habitat == 'forêt', 10, ifelse(habitat == 'prairie', 6, 3)) + altitude / 200")
p4$donnees[, as.character(numeros(p4, "densite"))] <- p4$donnees[, as.character(numeros(p4, "densite"))] + stats::rnorm(n, 0, 0.5)
u4 <- function(...) numeros(p4, c(...))
obs <- numeros_observations(p4)
d_lm <- definir_modele(p4, "LM", x = u4("habitat", "altitude"), y = u4("densite"), observations = obs, echelle = c(defaut = "none"))
d_lm$app <- TRUE
d_lm$name <- "M1"
p4$models <- list(d_lm)
m_avant <- ajuster(p4, d_lm)
dl <- data.frame(densite = valeurs(p4, "densite"), habitat = factor(valeurs(p4, "habitat"), levels = categories_de(p4, "habitat")), altitude = valeurs(p4, "altitude"))
co <- m_avant$resultat$coefficients
verifier("avant : la référence d'une régression est la dernière catégorie (prairie), comme lm() avec relevel(, \"prairie\")",
         !any(grepl("prairie", co$terme)) && isTRUE(all.equal(unname(co$estimation[co$terme == "habitat(forêt)"]),
                                                               unname(stats::coef(stats::lm(densite ~ relevel(habitat, "prairie") + altitude, dl))[2]), tolerance = 1e-8)))
r1 <- changer_categories(p4, u4("habitat"), c("forêt", "lande", "prairie"), c("prairie", "lande", "forêt"))
verifier("un nouvel ordre : les observations gardent leur catégorie", identical(valeurs(r1, "habitat"), valeurs(p4, "habitat")) &&
           identical(categories_de(r1, "habitat"), c("prairie", "lande", "forêt")) && attr(r1, "categories")$reference == "forêt")
m_projet <- ajuster(r1, r1$models[[1]])
d_neuf <- definir_modele(r1, "LM", x = numeros(r1, c("habitat", "altitude")), y = numeros(r1, "densite"), observations = obs, echelle = c(defaut = "none"))
m_neuf <- ajuster(r1, d_neuf)
co1 <- m_projet$resultat$coefficients
verifier("après : la référence est « forêt », la dernière, comme lm() avec relevel(, \"forêt\")", !any(grepl("forêt", co1$terme)) &&
           isTRUE(all.equal(unname(co1$estimation[co1$terme == "habitat(prairie)"]),
                            unname(stats::coef(stats::lm(densite ~ relevel(habitat, "forêt") + altitude, dl))[["relevel(habitat, \"forêt\")prairie"]]), tolerance = 1e-8)))
verifier("la définition du projet a suivi : le même ajustement qu'un modèle défini après le changement",
         isTRUE(all.equal(m_projet$resultat$ajuste, m_neuf$resultat$ajuste)) && identical(co1$terme, m_neuf$resultat$coefficients$terme))
verifier("les termes de l'habitat, à leurs places, dans l'ordre des catégories", identical(vapply(r1$models[[1]]$x, `[[`, "", "name"), c("habitat(prairie)", "habitat(lande)", "habitat(forêt)", "altitude")) &&
           identical(vapply(r1$models[[1]]$x, function(t) as.numeric(t$categories[1]), 0), c(1, 2, 3, 0)))
# la condition d'observations d'un modèle suit les catégories qu'elle nomme (atelier.R : suivre_condition)
d_c1 <- d_lm
d_c1$name <- "C1"
d_c1$condition <- 'habitat %in% c("lande", "prairie") & altitude > 300'
d_c2 <- d_lm
d_c2$name <- "C2"
d_c2$condition <- '"forêt" != habitat'
d_c3 <- d_lm
d_c3$name <- "C3"
d_c3$condition <- "altitude > 300"
p4c <- p4
p4c$models <- list(d_c1, d_c2, d_c3)
rc <- changer_categories(p4c, u4("habitat"), c("Forêt", "milieu ouvert", "milieu ouvert"))
cond <- vapply(rc$models, function(d) texte_condition(d$condition), "")
verifier("une condition suit les catégories renommées ou fusionnées qu'elle compare à leur variable ; une condition sur une autre variable ne change pas",
         identical(cond[1], 'habitat %in% c("milieu ouvert", "milieu ouvert") & altitude > 300') && identical(cond[2], '"Forêt" != habitat') &&
           identical(cond[3], "altitude > 300") &&
           length(observations_condition(rc, rc$models[[1]]$condition, obs)) == sum(valeurs(p4, "habitat") %in% c("lande", "prairie") & valeurs(p4, "altitude") > 300) &&
           length(observations_condition(rc, rc$models[[2]]$condition, obs)) == sum(valeurs(p4, "habitat") != "forêt"))
r2 <- changer_categories(p4, u4("habitat"), c("forêt", "milieu ouvert", "milieu ouvert"))
verifier("une fusion : lande et prairie en « milieu ouvert », à la place de la première", identical(categories_de(r2, "habitat"), c("forêt", "milieu ouvert")) &&
           sum(valeurs(r2, "habitat") == "milieu ouvert") == sum(valeurs(p4, "habitat") %in% c("lande", "prairie")) &&
           contient(attr(r2, "categories")$phrases, "fusionnées en « milieu ouvert »"))
verifier("la définition du projet garde un terme par catégorie qui reste", identical(vapply(r2$models[[1]]$x, `[[`, "", "name"), c("habitat(forêt)", "habitat(milieu ouvert)", "altitude")))
m2 <- ajuster(r2, r2$models[[1]])
dl$ouvert <- factor(ifelse(dl$habitat == "forêt", "forêt", "milieu ouvert"), levels = c("forêt", "milieu ouvert"))
verifier("le modèle des catégories fusionnées, comme lm()", isTRUE(all.equal(unname(m2$resultat$ajuste), unname(stats::fitted(stats::lm(densite ~ ouvert + altitude, dl))))))
r3 <- changer_categories(p4, u4("habitat"), c("Forêt", "lande", "prairie"))
verifier("un nom changé, ses observations le prennent", sum(valeurs(r3, "habitat") == "Forêt") == sum(valeurs(p4, "habitat") == "forêt") &&
           contient(attr(r3, "categories")$phrases, "« forêt » renommée « Forêt »"))
verifier("refus : un nom vide, un nombre de noms faux, une variable quantitative, rien de changé, un ordre incomplet",
         nzchar(erreur(changer_categories(p4, u4("habitat"), c("", "lande", "prairie")))) && nzchar(erreur(changer_categories(p4, u4("habitat"), c("a", "b")))) &&
           nzchar(erreur(changer_categories(p4, u4("altitude"), "a"))) && nzchar(erreur(changer_categories(p4, u4("habitat"), c("forêt", "lande", "prairie")))) &&
           nzchar(erreur(changer_categories(p4, u4("habitat"), c("forêt", "lande", "prairie"), c("lande", "forêt")))))
options(nuee.langue = "en")
verifier("en anglais", contient(attr(changer_categories(p4, u4("habitat"), c("forêt", "open", "open")), "categories")$phrases, "merged into “open”") &&
           contient(attr(remplacer_valeurs(p, numeros(p, "comptage"), "-9999", ""), "remplacement")$phrases, "“-9999” → empty (3)"))
options(nuee.langue = "fr")

cat("Un modèle de la séance après un changement des catégories\n")
vues <- categories_definition(p4, d_lm)
verifier("les catégories qu'a vues un modèle", identical(vues[[as.character(u4("habitat"))]], c("forêt", "lande", "prairie")))
al <- aligner_categories(r1, d_lm, vues)
verifier("un nouvel ordre : sa définition rapportée aux catégories d'aujourd'hui, le même modèle", is.null(attr(al, "perimee")) &&
           isTRUE(all.equal(ajuster(r1, al)$resultat$ajuste, m_avant$resultat$ajuste)))
verifier("un retour en arrière : rien à rapporter", identical(aligner_categories(p4, d_lm, vues), d_lm))
verifier("un nom changé : à ajuster à nouveau, et le message le dit", grepl("ont changé depuis l'ajustement de M1", attr(aligner_categories(r3, d_lm, vues), "perimee") %||% ""))
pq <- remplacer_valeurs(p4, u4("habitat"), vides = TRUE, remplacer = "inconnue")
verifier("une catégorie ajoutée : rien ne change pour le modèle", is.null(attr(aligner_categories(pq, d_lm, vues), "perimee")))
d_pls <- definir_modele(p4, "PLS", x = u4("habitat", "altitude"), y = u4("densite"), observations = obs, A = 1)
p4b <- p4
p4b$models <- list(d_pls)
num <- remplacer_valeurs(p4b, u4("habitat"), "forêt", "1")
num <- remplacer_valeurs(num, u4("habitat"), "lande", "2")
num <- remplacer_valeurs(num, u4("habitat"), "prairie", "3")
verifier("une variable devenue quantitative : la PLS du projet la prend en un seul terme", !length(categories_de(num, "habitat")) &&
           identical(vapply(num$models[[1]]$x, `[[`, "", "name"), c("habitat", "altitude")) && nzchar(attr(aligner_categories(num, d_pls, categories_definition(p4b, d_pls)), "perimee") %||% ""))

cat("Unités et descriptions\n")
p5 <- decrire_variables(p4, list(list(variable = u4("densite"), unite = "ind./ha", description = "Densité des adultes\npar hectare"),
                                 list(variable = u4("altitude"), unite = "m")))
verifier("gardées, sans retour à la ligne", unite_variable(p5, u4("densite")) == "ind./ha" && unite_variable(p5, "altitude") == "m" &&
           identical(variable_du_projet(p5, u4("densite"))$description, "Densité des adultes par hectare") && unite_texte(p5, "habitat") == "")
p6 <- decrire_variables(p5, data.frame(variable = u4("altitude", "densite"), unite = c("", NA), description = c(NA, "Densité"), stringsAsFactors = FALSE))
verifier("un data.frame (le JSON d'une liste) : un texte vide efface, NA laisse", unite_variable(p6, "altitude") == "" && unite_variable(p6, "densite") == "ind./ha" &&
           is.null(variable_du_projet(p6, u4("altitude"))$unite) && identical(variable_du_projet(p6, u4("densite"))$description, "Densité"))
verifier("rien de changé : refusé", nzchar(erreur(decrire_variables(p5, list(list(variable = u4("altitude"), unite = "m"))))))
f_nuee <- file.path(dossier, "releves.nuee")
enregistrer_projet(p5, f_nuee, "1.0")
p5r <- lire_projet(f_nuee)
verifier("enregistrées dans le .nuee et relues", unite_variable(p5r, "densite") == "ind./ha" && unite_variable(p5r, "altitude") == "m" &&
           identical(variable_du_projet(p5r, u4("densite"))$description, "Densité des adultes par hectare"))
f_ancien <- file.path(dossier, "ancien.nuee")
enregistrer_projet(p4, f_ancien, "0.9.9")
pa4 <- lire_projet(f_ancien)
verifier("un projet sans unités s'ouvre comme avant", unite_variable(pa4, "densite") == "" && is.null(descriptions_variables(pa4)))

cat("Export des données\n")
vars <- descriptions_variables(p5)
f_xlsx <- file.path(dossier, "export.xlsx")
ecrire_tableau(tableau_projet(p5), f_xlsx, objet = "releves", variables = vars)
feuilles <- readxl::excel_sheets(f_xlsx)
fv <- as.data.frame(readxl::read_excel(f_xlsx, sheet = "Variables"))
verifier("Excel : une feuille « Variables » (variable, unité, description) à côté des données", identical(feuilles, c("releves", "Variables")) &&
           identical(fv$variable, c("altitude", "densite")) && identical(fv[["unité"]], c("m", "ind./ha")))
verifier("Excel relu : un seul tableau à ouvrir, les unités et descriptions rendues", identical(tables_fichier(f_xlsx), "releves") &&
           { px <- importer_tableau(f_xlsx); unite_variable(px, "densite") == "ind./ha" && unite_variable(px, "altitude") == "m" &&
               identical(variable_du_projet(px, numeros(px, "densite"))$description, "Densité des adultes par hectare") && nrow(px$donnees) == n })
f_csv <- file.path(dossier, "export.csv")
ecrire_tableau(tableau_projet(p5), f_csv, variables = vars)
verifier("CSV : les données seules", !any(grepl("ind./ha", readLines(f_csv, encoding = "UTF-8"), fixed = TRUE)))
f_rds <- file.path(dossier, "export.rds")
ecrire_tableau(tableau_projet(p5), f_rds, variables = vars)
r <- readRDS(f_rds)
verifier("R : les attributs « units » et « label » (Hmisc)", identical(attr(r$densite, "units"), "ind./ha") && identical(attr(r$densite, "label"), "Densité des adultes par hectare") &&
           identical(attr(r$altitude, "units"), "m"))
f_omv <- file.path(dossier, "export.omv")
ecrire_tableau(tableau_projet(p5), f_omv, variables = vars)
meta <- jsonlite::fromJSON(utils::unzip(f_omv, "metadata.json", exdir = file.path(dossier, "omv")), simplifyVector = FALSE)
desc <- stats::setNames(vapply(meta$dataSet$fields, function(f) as.character(f$description), ""), vapply(meta$dataSet$fields, function(f) as.character(f$name), ""))
verifier("jamovi : la description et l'unité de chaque colonne", identical(desc[["densite"]], "Densité des adultes par hectare (ind./ha)") && identical(desc[["altitude"]], "(m)"))
if (requireNamespace("RSQLite", quietly = TRUE)) {
  f_gpkg <- file.path(dossier, "export.gpkg")
  ecrire_geopackage(tableau_projet(p5), f_gpkg, valeurs(p5, "altitude"), valeurs(p5, "densite"), "plan", couche = "releves", tables = list(Variables = vars))
  verifier("GeoPackage : une table « Variables », relue avec la couche", identical(couches_geopackage(f_gpkg), c("releves", "Variables")) &&
             identical(tables_fichier(f_gpkg), "releves") && { pg <- importer_tableau(f_gpkg); unite_variable(pg, "densite") == "ind./ha" })
}

cat("Textes et script\n")
d5 <- definir_modele(p5, "LM", x = numeros(p5, c("habitat", "altitude")), y = numeros(p5, "densite"), observations = obs, echelle = c(defaut = "none"))
m5 <- ajuster(p5, d5)
me <- texte_methodes(p5, d5, m5, "fr")$paragraphes
verifier("Méthodes : la réponse et son unité", contient(me, "la réponse est « densite » (ind./ha)"))
verifier("Méthodes, en anglais", contient(texte_methodes(p5, d5, m5, "en")$paragraphes, "the response is “densite” (ind./ha)"))
dt <- definir_modele(p5, "LM", x = numeros(p5, "altitude"), y = numeros(p5, "densite"), observations = obs, echelle = c(defaut = "none"),
                     transformations = stats::setNames("log10", as.character(numeros(p5, "densite"))))
verifier("Méthodes : une réponse transformée, son unité dans la formule", contient(texte_methodes(p5, dt, ajuster(p5, dt), "fr")$paragraphes, "log10(« densite » (ind./ha))"))
dpls <- definir_modele(p5, "PLS", x = numeros(p5, c("habitat", "altitude")), y = numeros(p5, "densite"), observations = obs, A = 1)
verifier("Méthodes d'une PLS : la variable Y et son unité", contient(texte_methodes(p5, dpls, ajuster(p5, dpls), "fr")$paragraphes, "La variable Y : « densite » (ind./ha)."))
re <- texte_resultats(p5, d5, m5, "fr")$paragraphes
b <- m5$resultat$coefficients$estimation[m5$resultat$coefficients$terme == "altitude"]
verifier("Résultats : le coefficient d'altitude « par m », la réponse et son unité", contient(re, sprintf("« altitude » : β = %s par m (IC 95 %%", nombre_resultat(b, TRUE))) &&
           contient(re, "Le modèle de « densite » (ind./ha) porte sur"))
verifier("Résultats, en anglais : « per m »", contient(texte_resultats(p5, d5, m5, "en")$paragraphes, sprintf("“altitude”: β = %s per m (95%% CI", nombre_resultat(b, FALSE))))
d5uv <- definir_modele(p5, "LM", x = numeros(p5, "altitude"), y = numeros(p5, "densite"), observations = obs, echelle = c(defaut = "UV"))
verifier("Résultats : une variable mise à l'échelle UV n'est pas « par m »", !contient(texte_resultats(p5, d5uv, ajuster(p5, d5uv), "fr")$paragraphes, "par m"))
f_script <- file.path(dossier, "modele.R")
invisible(ecrire_script(p5, d5, f_script, "moteur", m = m5))
s <- readLines(f_script, encoding = "UTF-8")
verifier("script R : les unités et descriptions en commentaire avant les données", any(s == "# Unités et descriptions des variables :") &&
           any(s == "# - densite (ind./ha) : Densité des adultes par hectare") && any(s == "# - altitude (m)"))
# (dans le dossier de l'essai : ses graphiques vont dans son Rplots.pdf, pas dans le dépôt)
rs <- local({
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f_script)), stdout = TRUE, stderr = TRUE)
})
verifier("le script tourne encore", is.null(attr(rs, "status")))
p_piege <- decrire_variables(p4, list(list(variable = u4("altitude"), unite = "m", description = "a\nstop(\"non\")")))
p_piege$variables[[which(vapply(p_piege$variables, function(v) identical(v$ids[1], "altitude"), TRUE))]]$description <- "a\nstop(\"non\")"  # (un projet reçu)
invisible(ecrire_script(p_piege, d5, f_script, "moteur"))
s2 <- readLines(f_script, encoding = "UTF-8")
verifier("une description à retour à la ligne venue d'un projet reste un commentaire", !any(trimws(s2) == "stop(\"non\")"))

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
