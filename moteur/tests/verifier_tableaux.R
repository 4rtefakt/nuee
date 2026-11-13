# Vérifie Édition › Ajouter des colonnes d'un tableau (moteur/R/variables.R :
# ajouter_colonnes_table, proposer_cles) contre match() de R, et Édition › Résumer par groupe
# (moteur/R/nouveau.R : resumer_par_groupe) contre aggregate() et tapply() :
#   - une clé ou deux, en texte (un identifiant, sans les espaces autour, les majuscules comptant)
#     ou en nombres (une variable quantitative : 2019 = « 2019,0 »), le nom des observations ;
#     les clés lues telles qu'écrites (« 01 ») ; une clé qui revient refusée ; les observations
#     sans correspondance et les clés seules comptées et nommées ; les noms déjà pris ; les types
#     des colonnes ajoutées ; les clés proposées ;
#   - chaque statistique, les valeurs manquantes, un groupe sans valeur connue, deux ou trois
#     colonnes de groupes, les observations sans groupe, les qualitatives gardées ou laissées de
#     côté, les identifiants, les noms en anglais, le projet enregistré puis relu ; les refus.
#   Rscript moteur/tests/verifier_tableaux.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
erreur <- function(expr) tryCatch({ expr; "" }, error = conditionMessage)
dossier <- tempfile("tableaux")
dir.create(dossier)

# Un identifiant secondaire de plus pour chaque observation (comme le site et le secteur d'un projet .usp)
avec_identifiant <- function(p, nom, valeurs) {
  p$observation_id_names <- c(p$observation_id_names, nom)
  for (i in seq_along(p$observations)) p$observations[[i]]$ids <- c(p$observations[[i]]$ids, valeurs[i])
  p
}
# Les valeurs d'une variable du projet par son nom (le nom de la catégorie pour une qualitative)
valeurs <- function(p, nom) {
  v <- variable_du_projet(p, numeros(p, nom))
  x <- unname(colonne(p, v$uid, numeros_observations(p)))
  if (length(v$categories)) v$categories[x] else x
}
qualitative <- function(p, nom) length(variable_du_projet(p, numeros(p, nom))$categories) > 0

# Des placettes : trois sites, deux secteurs par site, trois années, deux placettes par secteur
# et par année, dans le désordre ; des densités manquantes (toutes celles de S3-B en 2020), et
# une observation sans site.
set.seed(3)
g <- expand.grid(placette = 1:2, annee = 2018:2020, secteur = c("A", "B"), site = c("S1", "S2", "S3"), stringsAsFactors = FALSE)
g <- rbind(g[sample(nrow(g)), ], data.frame(placette = 3, annee = 2019, secteur = "A", site = ""))
n <- nrow(g)
df <- data.frame(nom = sprintf("%s-%s-%d-%d", g$site, g$secteur, g$annee, g$placette), annee = g$annee,
                 densite = round(runif(n, 0, 10), 2), biomasse = round(rnorm(n, 50, 10), 1),
                 habitat = unname(c(S1 = "forêt", S2 = "prairie", S3 = "lande")[g$site]),
                 observateur = sample(c("Jean", "Marie"), n, TRUE), stringsAsFactors = FALSE)
df$habitat[n] <- "forêt"
df$densite[c(2, 9)] <- NA
df$densite[g$site == "S3" & g$secteur == "B" & g$annee == 2020] <- NA
p <- avec_identifiant(avec_identifiant(projet_depuis_tableau(df, "placettes"), "site", g$site), "secteur", g$secteur)
p$nom <- "placettes.xlsx"
obs <- numeros_observations(p)
annee <- numeros(p, "annee")

cat("Ajouter des colonnes d'un tableau : une clé en texte\n")
sites <- data.frame(site = c("S2", "S1", " S3 ", "S4"), altitude = c(200, 100, 300, 400), sol = c("argile", "sable", "argile", "limon"),
                    pente = factor(c("forte", "faible", "moyenne", "forte"), levels = c("faible", "moyenne", "forte")),
                    humide = c(TRUE, FALSE, TRUE, NA), stringsAsFactors = FALSE)
q <- ajouter_colonnes_table(p, sites, "id:2", "site")
j <- attr(q, "jointure")
k <- match(trimws(g$site), trimws(sites$site))  # (la même chose écrite avec match())
verifier("une colonne de nombres : une variable quantitative, les valeurs de match()",
         !qualitative(q, "altitude") && identical(valeurs(q, "altitude"), sites$altitude[k]))
verifier("du texte : une variable qualitative, ses catégories triées",
         qualitative(q, "sol") && identical(variable_du_projet(q, numeros(q, "sol"))$categories, c("argile", "sable")) && identical(valeurs(q, "sol"), sites$sol[k]))
verifier("un facteur : qualitative, ses catégories dans l'ordre du facteur (celles qui servent)",
         identical(variable_du_projet(q, numeros(q, "pente"))$categories, c("faible", "moyenne", "forte")) && identical(valeurs(q, "pente"), as.character(sites$pente[k])))
verifier("des valeurs logiques : qualitative, FALSE et TRUE", identical(variable_du_projet(q, numeros(q, "humide"))$categories, c("FALSE", "TRUE")) &&
         identical(valeurs(q, "humide"), as.character(sites$humide[k])))
verifier("les observations gardent leur ordre et leurs autres valeurs", identical(unname(q$noms_obs), unname(p$noms_obs)) &&
         identical(valeurs(q, "densite"), valeurs(p, "densite")) && identical(q$observation_id_names, p$observation_id_names))
verifier("toutes les colonnes sauf la clé par défaut, ajoutées à la suite",
         identical(j$colonnes, c("altitude", "sol", "pente", "humide")) && identical(utils::tail(unname(q$noms_var), 4), j$noms))
verifier("les comptes : 36 observations trouvées, celle sans site sans correspondance, la clé S4 seule",
         j$n == 37 && j$trouvees == 36 && j$sans_correspondance == 1 && j$cles_seules == 1 && j$sans_cle == 0)
verifier("les phrases nomment l'observation sans correspondance et la clé seule",
         identical(j$phrases, c("4 colonnes ajoutées au projet : altitude, sol, pente, humide.", "36 observation(s) sur 37 ont trouvé leur clé dans le tableau.",
                                sprintf("1 observation(s) sans correspondance, aux valeurs manquantes : %s.", df$nom[n]),
                                "1 clé(s) du tableau ne répondent à aucune observation : S4.")))
verifier("le projet d'origine ne change pas", identical(p$noms_var, stats::setNames(c("annee", "densite", "biomasse", "habitat", "observateur"), 1:5)))
q2 <- ajouter_colonnes_table(q, sites, "id:2", "site", colonnes = "altitude")
q3 <- ajouter_colonnes_table(q2, data.frame(code = c("S1", "S2"), secteur = c("x", "y"), annee = 1:2), "id:2", "code")
verifier("un nom déjà pris : « altitude (2) », puis « (3) » ; le nom d'un identifiant aussi est pris",
         identical(attr(q2, "jointure")$noms, "altitude (2)") && identical(valeurs(q2, "altitude (2)"), sites$altitude[k]) &&
         identical(attr(q3, "jointure")$noms, c("secteur (2)", "annee (2)")) &&
         identical(attr(ajouter_colonnes_table(q2, sites, "id:2", "site", colonnes = "altitude"), "jointure")$noms, "altitude (3)") &&
         any(grepl("« altitude » ajoutée sous le nom « altitude (2) »", attr(q2, "jointure")$phrases, fixed = TRUE)))
verifier("les majuscules comptent : s1 n'est pas S1",
         attr(ajouter_colonnes_table(p, data.frame(site = c("s1", "S2"), x = 1:2), "id:2", "site"), "jointure")$trouvees == sum(g$site == "S2"))
avec_vide <- ajouter_colonnes_table(p, data.frame(site = c("S1", "", NA, "S2"), x = 1:4), "id:2", "site")
verifier("une ligne du tableau sans clé : laissée de côté, comptée", attr(avec_vide, "jointure")$sans_cle == 2 &&
         identical(valeurs(avec_vide, "x"), c(1L, 4L)[match(g$site, c("S1", "S2"))] + 0))
verifier("le nom des observations comme clé (\"id:1\")",
         identical(valeurs(ajouter_colonnes_table(p, data.frame(nom = rev(df$nom), r = rev(seq_len(n))), "id:1", "nom"), "r"), as.numeric(seq_len(n))))

cat("Une clé en nombres, deux clés\n")
meteo <- data.frame(annee = c("2018", " 2019 ", "2020,0"), pluie = c(600, 700, 800), stringsAsFactors = FALSE)
q <- ajouter_colonnes_table(p, meteo, annee, "annee")
verifier("une variable quantitative : comparée en nombres (« 2019 », « 2020,0 »)", identical(valeurs(q, "pluie"), c(600, 700, 800)[match(g$annee, 2018:2020)]) &&
         attr(q, "jointure")$trouvees == n)
verifier("des nombres des deux côtés (2019 = 2019.0)", identical(valeurs(ajouter_colonnes_table(p, data.frame(an = c(2018, 2019.0), v = 1:2), annee, "an"), "v"),
                                                        c(1, 2)[match(g$annee, c(2018, 2019))]))
ms <- expand.grid(annee = 2018:2020, site = c("S1", "S2"), stringsAsFactors = FALSE)
ms$pluie <- seq_len(nrow(ms)) * 10
q <- ajouter_colonnes_table(p, ms[sample(nrow(ms)), ], c("id:2", annee), c("site", "annee"))
k2 <- match(paste(g$site, g$annee), paste(ms$site, ms$annee))
verifier("deux clés (le site et l'année) : celles de match(paste()) ; S3 et l'observation sans site sans correspondance",
         identical(valeurs(q, "pluie"), ms$pluie[k2]) && attr(q, "jointure")$sans_correspondance == sum(is.na(k2)) && sum(is.na(k2)) == 13)
verifier("deux clés : la clé seule écrite « S9 / 2018 », ses colonnes nommées",
         identical(attr(ajouter_colonnes_table(p, rbind(ms, data.frame(annee = 2018, site = "S9", pluie = 1)), c("id:2", annee), c("site", "annee")), "jointure")$phrases[4],
                   "1 clé(s) du tableau (site / annee) ne répondent à aucune observation : S9 / 2018."))
verifier("une clé qui revient est refusée, nommée",
         startsWith(erreur(ajouter_colonnes_table(p, ms, "id:2", "site")), "La clé « S1 » revient 3 fois dans le tableau, comme 1 autre(s) : chaque clé doit y venir une fois au plus") &&
         startsWith(erreur(ajouter_colonnes_table(p, rbind(ms, ms[2, ]), c("id:2", annee), c("site", "annee"))), "La clé « S1 / 2019 » (site / annee) revient 2 fois dans le tableau :") &&
         grepl("ne trouve sa clé (site / annee) dans le tableau", erreur(ajouter_colonnes_table(p, transform(ms, site = tolower(site)), c("id:2", annee), c("site", "annee"))), fixed = TRUE))
m <- erreur(ajouter_colonnes_table(p, data.frame(site = c("s1", "s2"), x = 1:2), "id:2", "site"))
verifier("aucune clé trouvée : rien n'est ajouté, le message montre les clés de chaque côté",
         identical(m, "Aucune observation ne trouve sa clé dans le tableau : rien n'est ajouté. Les clés se comparent telles qu'écrites (S1 n'est pas s1, ni 01 1) ; le projet a S1, S2, S3, le tableau s1, s2."))
verifier("les refus : une colonne absente, une clé sans sa colonne, une colonne de la clé ajoutée, rien à ajouter",
         startsWith(erreur(ajouter_colonnes_table(p, ms, "id:2", "lieu")), "Le tableau n'a pas de colonne « lieu »") &&
         nzchar(erreur(ajouter_colonnes_table(p, ms, c("id:2", annee), "site"))) &&
         nzchar(erreur(ajouter_colonnes_table(p, ms, "id:2", "site", colonnes = "site"))) &&
         nzchar(erreur(ajouter_colonnes_table(p, ms, c("id:2", annee), c("site", "annee"), colonnes = character(0)))))

cat("Les clés telles qu'écrites\n")
codes <- projet_depuis_tableau(data.frame(nom = c("a", "b", "c", "d"), x = 1:4), "codes")
codes <- avec_identifiant(codes, "code", c("01", "02", "10", "2"))
f <- file.path(dossier, "codes.csv")
writeLines(c("code;valeur", "01;1,5", "02;2,5", "10;3", "2;4"), f)
lu <- lire_tableau(f, textes = "code")
verifier("lire_tableau(textes = ) garde la colonne telle qu'écrite, les autres en nombres",
         identical(lu$code, c("01", "02", "10", "2")) && identical(lu$valeur, c(1.5, 2.5, 3, 4)) && identical(lire_tableau(f)$code, c(1, 2, 10, 2)))
verifier("« 01 » répond à « 01 », pas à 1 ; « 2 » à 2",
         identical(valeurs(ajouter_colonnes_table(codes, lu, "id:2", "code"), "valeur"), c(1.5, 2.5, 3, 4)) &&
         identical(valeurs(ajouter_colonnes_table(codes, data.frame(code = c(1, 2, 10), v = 1:3), "id:2", "code"), "v"), c(NA, NA, 3, 2)))
xl <- file.path(dossier, "codes.xlsx")
openxlsx::write.xlsx(data.frame(code = c("01", "02", "10"), valeur = c(1.5, 2.5, 3)), xl)
verifier("de même dans un classeur Excel", identical(lire_tableau(xl, textes = "code")$code, c("01", "02", "10")) && identical(lire_tableau(xl)$code, c(1, 2, 10)))

cat("Les clés proposées\n")
verifier("un tableau des sites : le site (sans tenir compte des majuscules)",
         identical(proposer_cles(p, data.frame(Site = "S1", altitude = 1)), list(list(projet = "id:2", table = "Site"))))
verifier("la météo de chaque site et de chaque année : le site et l'année",
         identical(proposer_cles(p, ms), list(list(projet = annee, table = "annee"), list(projet = "id:2", table = "site"))))
verifier("une colonne de même nom de plus (l'habitat) : la combinaison unique où le plus d'observations trouvent leur clé",
         identical(proposer_cles(p, data.frame(site = c("S1", "S2"), habitat = c("forêt", "forêt"))), list(list(projet = "id:2", table = "site"))))
verifier("une colonne de mesures du nom d'une variable (la densité), aux valeurs toutes différentes : pas dans la clé",
         identical(proposer_cles(p, transform(ms, densite = seq_len(nrow(ms)) / 7)), list(list(projet = annee, table = "annee"), list(projet = "id:2", table = "site"))))
verifier("le même tableau, déjà joint : sa colonne de mesures (la pluie), aux mêmes valeurs mais plus loin, n'est pas la clé",
         identical(proposer_cles(ajouter_colonnes_table(p, ms, c("id:2", annee), c("site", "annee")), ms), list(list(projet = annee, table = "annee"), list(projet = "id:2", table = "site"))))
verifier("aucun nom commun : rien", identical(proposer_cles(p, data.frame(lieu = "S1", x = 1)), list()))

cat("Résumer par groupe : chaque statistique contre aggregate()\n")
tous <- names(STATISTIQUES_GROUPE)
r <- resumer_par_groupe(p, c("id:2", "id:3", annee), statistiques = tous)
tab <- tableau_projet(r)
d <- data.frame(site = g$site, secteur = g$secteur, annee = g$annee, densite = valeurs(p, "densite"), biomasse = valeurs(p, "biomasse"), stringsAsFactors = FALSE)
d <- d[d$site != "", ]
verifier("une ligne par groupe (18), rangées par site, secteur et année, nommées par leurs valeurs", nrow(tab) == 18 &&
         identical(tab$Observation, with(unique(d[order(d$site, d$secteur, d$annee), 1:3]), paste(site, secteur, annee))))
verifier("le site et le secteur restent des identifiants, l'année une variable quantitative",
         identical(r$observation_id_names, c("Observation", "site", "secteur", "habitat")[1:3]) && identical(tab$site, sub(" .*", "", tab$Observation)) &&
         !qualitative(r, "annee") && identical(valeurs(r, "annee"), as.numeric(sub(".* ", "", tab$Observation))))
cle <- paste(tab$site, tab$secteur, tab$annee)
fonctions <- list(moyenne = mean, somme = sum, mediane = median, minimum = min, maximum = max, ecart_type = sd, n = length)
for (s in tous) {
  for (v in c("densite", "biomasse")) {
    a <- aggregate(stats::reformulate(c("site", "secteur", "annee"), v), d, fonctions[[s]])  # (sans les valeurs manquantes)
    x <- tab[[sprintf("%s (%s)", v, STATISTIQUES_GROUPE[[s]]$fr)]]
    i <- match(paste(a$site, a$secteur, a$annee), cle)
    verifier(sprintf("%s (%s) = aggregate()", v, s), isTRUE(all.equal(x[i], a[[v]])) && all(is.na(x[-i]) | s == "n"))
  }
}
vide <- which(cle == "S3 B 2020")
libelles <- vapply(setdiff(tous, "n"), function(s) STATISTIQUES_GROUPE[[s]]$fr, "")
verifier("un groupe sans valeur connue : statistiques manquantes, n = 0", all(is.na(unlist(tab[vide, sprintf("densite (%s)", libelles)]))) &&
         tab[vide, "densite (n)"] == 0 && !is.na(tab[vide, "biomasse (moyenne)"]))
verifier("l'écart-type d'une seule valeur manque, comme sd()", all(is.na(tab[["densite (écart-type)"]][tab[["densite (n)"]] == 1])))
info <- attr(r, "resume")
verifier("l'habitat, le même dans chaque groupe, est gardé (qualitatif) ; l'observateur laissé de côté, nommé ; l'observation sans site comptée",
         qualitative(r, "habitat") && identical(valeurs(r, "habitat"), unname(c(S1 = "forêt", S2 = "prairie", S3 = "lande")[tab$site])) &&
         identical(info$gardees, "habitat") && identical(info$laissees, "observateur") && info$sans_groupe == 1 && info$observations == 36 &&
         identical(info$phrase, "18 groupe(s), d'après 36 observation(s) ; 1 observation(s) sans valeur de site, secteur ou annee laissée(s) de côté ; laissées de côté, car elles changent dans un groupe : observateur."))
verifier("le nom du projet ; par défaut, la moyenne de toutes les variables quantitatives (l'année aussi, hors des groupes)",
         identical(r$source, "placettes par site, secteur, annee") &&
         identical(unname(resumer_par_groupe(p, "id:2")$noms_var), c("habitat", "annee (moyenne)", "densite (moyenne)", "biomasse (moyenne)")))

cat("Deux colonnes de groupes contre tapply()\n")
r2 <- resumer_par_groupe(p, c("id:3", annee), variables = numeros(p, "densite"), statistiques = c("moyenne", "n"))
t2 <- tableau_projet(r2)
attendu <- tapply(valeurs(p, "densite"), list(g$secteur, g$annee), mean, na.rm = TRUE)
nb <- tapply(!is.na(valeurs(p, "densite")), list(g$secteur, g$annee), sum)
verifier("la moyenne et n de chaque secteur et de chaque année = tapply() (les sites réunis, l'observation sans site comprise)",
         nrow(t2) == 6 && isTRUE(all.equal(t2[["densite (moyenne)"]], attendu[cbind(t2$secteur, as.character(t2$annee))])) &&
         identical(t2[["densite (n)"]], as.numeric(nb[cbind(t2$secteur, as.character(t2$annee))])))
verifier("l'habitat, qui change dans un secteur réuni sur trois sites, est laissé de côté ; le site aussi",
         identical(attr(r2, "resume")$laissees, c("habitat", "observateur", "site")) && identical(unname(r2$noms_var), c("annee", "densite (moyenne)", "densite (n)")))
r3 <- resumer_par_groupe(p, c(numeros(p, "habitat"), annee), variables = numeros(p, "biomasse"), statistiques = "somme")
t3 <- tableau_projet(r3)
verifier("une variable qualitative en groupe : qualitative, dans l'ordre de ses catégories ; la somme = tapply()",
         qualitative(r3, "habitat") && identical(t3$Observation[1:3], c("forêt 2018", "forêt 2019", "forêt 2020")) &&
         isTRUE(all.equal(t3[["biomasse (somme)"]], as.numeric(tapply(valeurs(p, "biomasse"), list(valeurs(p, "habitat"), g$annee), sum)[cbind(as.character(t3$habitat), as.character(t3$annee))]))))

cat("Le nouveau projet, en anglais, les refus\n")
fr <- file.path(dossier, "resume.nuee")
enregistrer_projet(r, fr)
relu <- lire_projet(fr)
verifier("enregistré puis relu : les mêmes données, identifiants compris", isTRUE(all.equal(tableau_projet(relu), tab)) &&
         identical(relu$observation_id_names, r$observation_id_names))
options(nuee.langue = "en")
re <- resumer_par_groupe(p, c("id:2", annee), variables = numeros(p, "densite"), statistiques = c("moyenne", "ecart_type"))
verifier("en anglais : « by », « (mean) », « (sd) »", identical(re$source, "placettes by site, annee") &&
         identical(unname(re$noms_var), c("annee", "habitat", "densite (mean)", "densite (sd)")))
verifier("en anglais : les phrases d'Ajouter des colonnes",
         identical(attr(ajouter_colonnes_table(p, sites, "id:2", "site", colonnes = "altitude"), "jointure")$phrases[1:2],
                   c("1 column added to the project: altitude.", "36 of the 37 observations found their key in the table.")))
options(nuee.langue = NULL)
verifier("les refus : aucun groupe, une variable qualitative à résumer, aucune statistique, une inconnue, aucune valeur de groupe",
         startsWith(erreur(resumer_par_groupe(p, character(0))), "Choisir au moins une colonne") &&
         startsWith(erreur(resumer_par_groupe(p, "id:2", variables = numeros(p, "habitat"))), "« habitat » est qualitative") &&
         startsWith(erreur(resumer_par_groupe(p, "id:2", statistiques = character(0))), "Choisir au moins une statistique") &&
         startsWith(erreur(resumer_par_groupe(p, "id:2", statistiques = "moyen")), "Statistique inconnue : moyen") &&
         startsWith(erreur(resumer_par_groupe(avec_identifiant(p, "vide", rep("", n)), "id:4")), "Aucune observation n'a de valeur de vide") &&
         startsWith(erreur(resumer_par_groupe(p, annee, variables = annee)), "Choisir au moins une variable quantitative"))

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
