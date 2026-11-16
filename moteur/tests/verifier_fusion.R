# Vérifie la fusion de deux versions d'un projet (moteur/R/fusion.R) et ses demandes (serveur.R :
# fusionner, fusionner_finir, fichier_change), telles que l'interface les envoie :
#   - les deux côtés ajoutent des modèles ; un même nom des deux côtés (« M3 »), le leur renommé M4 avec
#     son numéro, les références de leurs documents ({{M3.R2X}}) et les panneaux de leurs figures avec
#     lui ; le même modèle fait des deux côtés, une fois ;
#   - un document fusionné ligne à ligne, chaque côté ayant changé un paragraphe ; une figure dont chaque
#     côté a changé un champ ; deux documents et deux figures nouveaux sous le même identifiant ; la séance
#     de la fenêtre, la mienne ;
#   - les données par les étapes : deux nettoyages (-9999 vidé, « foret » renommée « forêt ») fusionnés et
#     rejoués depuis la source, les mêmes données que les deux faits l'un après l'autre ; la même étape faite
#     des deux côtés, une fois ; les deux côtés ajoutent une variable calculée (numéros qui se croisent), un
#     modèle de la leur la suit ; un seul côté met les données à jour, l'étape de l'autre rejouée après ;
#   - cellule par cellule quand les étapes ne se rejouent pas : la source introuvable sur cet ordinateur,
#     des données changées hors des étapes, une étape de l'autre côté qui ne s'applique plus ; la source
#     changée depuis sa lecture, une étape qui ne se refait pas ici (son fichier n'est que chez l'autre), une
#     variable supprimée d'un côté et changée de l'autre ;
#   - les données cellule par cellule (des projets de la 1.0, sans étapes) : des lignes ajoutées des deux
#     côtés par deux mises à jour, une cellule changée autrement des deux côtés, les catégories nouvelles
#     de chaque côté mises avant la dernière (une régression garde sa référence, contre lm()), les termes
#     de ces catégories ajoutés des deux côtés au même modèle, un modèle nouveau d'un côté qui prend les
#     lignes de l'autre ; une variable et des observations retirées d'un côté, inchangées (retirées) ou
#     changées (un conflit) de l'autre ; une unité ; un jeu de prédiction ; des lignes sans nom, chaque
#     côté relu, ses cellules à leur place ;
#   - chaque sorte de conflit (modèle, document, figure, source des données, variable, champ : l'unité
#     d'une variable, les coordonnées, l'ordre des catégories) et ses deux choix, par les étapes (le choix
#     que les étapes rejouées ne donnent pas devient une étape, les données restent conformes) et cellule
#     par cellule ;
#   - fusionner un côté avec lui-même, ou avec la base : rien ne change ;
#   - le projet fusionné enregistré, rouvert, et chacun de ses modèles réajusté ;
#   - un dossier partagé par les demandes : la copie du fichier lu ou enregistré, dans la séance et jamais à
#     côté du fichier ; fichier_change (inchangé, changé et par qui, une date seule changée, effacé) ; la
#     question avant d'écraser, qui dit qu'une fusion est possible ; fusionner avec le projet de la séance,
#     puis fusionner_finir (un seul Ctrl+Z, l'enregistrement qui passe ensuite sans question), les refus
#     (des choix qui manquent, un projet changé depuis, une ancienne version, sans base), fichier_change
#     demandé pendant une fusion préparée ; une fusion de trois fichiers écrite dans un fichier ; annuler une
#     fusion ; les copies effacées à la fermeture ;
#   - un projet que personne ne fusionne : le même fichier écrit, le même projet lu, aussi quand sa copie
#     dans la séance ne s'écrit pas ;
#   - les phrases en anglais.
#   Rscript moteur/tests/verifier_fusion.R

options(warn = 1, stringsAsFactors = FALSE)
racine_nuee <- normalizePath(".", winslash = "/")
# Le serveur sans son écoute (comme verifier_etapes.R) : chaque expression de serveur.R jusqu'à
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
# (hors du dossier temporaire de R : les copies que garde le serveur y sont, et le dossier de l'étude,
# celui d'un utilisateur, ne doit en avoir aucune)
dossier <- file.path(dirname(tempdir()), basename(tempfile("nuee_fusion")))
dir.create(dossier)
etude <- file.path(dossier, "etude")
dir.create(file.path(etude, "donnees"), recursive = TRUE)
fichier <- function(...) file.path(etude, ...)

# Une demande comme l'interface l'envoie : `q` passé par le JSON, la réponse mise en JSON (elle doit
# s'y écrire) ; rend la réponse telle que R l'a faite.
demande <- function(nom, q = list()) {
  q <- jsonlite::fromJSON(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA)), simplifyVector = TRUE)
  r <- DEMANDES[[nom]](q)
  repondre(200L, r)
  invisible(r)
}
refus <- function(nom, q) tryCatch({ demande(nom, q); "" }, error = conditionMessage)
uid <- function(id, noms) as.numeric(numeros(projet(id), noms))
uid_p <- function(p, nom) as.numeric(numeros(p, nom))
# Les valeurs d'une variable par son nom (le nom de la catégorie pour une qualitative), nommées par
# l'observation ; ses catégories ; une définition par son nom ; un conflit par son identifiant.
valeurs <- function(p, nom) {
  v <- variable_du_projet(p, uid_p(p, nom))
  obs <- numeros_observations(p)
  x <- unname(colonne(p, v$uid, obs))
  stats::setNames(if (length(v$categories)) as.character(v$categories)[x] else x, unname(p$noms_obs[as.character(obs)]))
}
categories_de <- function(p, nom) as.character(variable_du_projet(p, uid_p(p, nom))$categories %||% character(0))
definition <- function(p, nom) Find(function(d) identical(d$name, nom), p$models)
conflit <- function(r, id) Find(function(x) identical(x$id, id), r$conflits)
ids_conflits <- function(r) vapply(r$conflits, function(x) x$id, "")
document <- function(p, id) Find(function(d) identical(d$id, id), p$documents)
figure <- function(p, id) Find(function(f) identical(f$id, id), p$figures)
panneaux <- function(f) jsonlite::fromJSON(f$contenu, simplifyVector = FALSE)$panneaux
etiquettes <- function(p) vapply(p$etapes %||% list(), function(e) e$etiquette, "")
# Le projet fusionné passé par un fichier, rouvert dans la séance : chacun de ses modèles réajusté
# (par son numéro, comme l'interface le fait).
reajuste <- function(p, nom) {
  f <- file.path(dossier, paste0(nom, ".nuee"))
  enregistrer_projet(p, f)
  id <- demande("ouvrir", list(chemin = f))$id
  ok <- vapply(projet(id)$models, function(d) {
    r <- tryCatch(demande("ajuster", list(projet = id, numero = d$number)), error = function(e) NULL)
    !is.null(r) && identical(r$nom, d$name)
  }, TRUE)
  demande("fermer_projet", list(projet = id))
  all(ok)
}

# ---------------------------------------------------------------- les données

# Quatre sites suivis de 2018 à 2025 (l'observation nommée « S1-2018 »), l'habitat de chaque site, deux
# mesures, une réponse, un comptage où -9999 dit « pas de relevé », les coordonnées en Lambert-93.
sites <- data.frame(site = paste0("S", 1:6), habitat = c("foret", "prairie", "foret", "lande", "marais", "dune"),
                    X = c(820000, 826000, 832000, 838000, 841000, 844000), Y = c(6280000, 6284000, 6276000, 6272000, 6270000, 6268000))
tableau <- function(annees, quels, graine) {
  set.seed(graine)
  l <- expand.grid(annee = annees, site = quels, stringsAsFactors = FALSE)
  l <- l[order(l$annee, l$site), ]
  s <- sites[match(l$site, sites$site), ]
  data.frame(nom = paste(l$site, l$annee, sep = "-"), site = l$site, habitat = s$habitat, annee = l$annee,
             x1 = round(rnorm(nrow(l), 5), 3), x2 = round(rnorm(nrow(l), 2), 3), y = round(3 + (s$habitat == "foret") + rnorm(nrow(l), 0, 0.5), 3),
             comptage = ifelse(runif(nrow(l)) < 0.2, -9999, rpois(nrow(l), 8)), X = s$X, Y = s$Y)
}
df <- tableau(2018:2025, paste0("S", 1:4), 1)
source_csv <- fichier("donnees", "releves.csv")
utils::write.csv(df, source_csv, row.names = FALSE, na = "")

# La base : la PCA M1 (une composante), la régression M2 (y ~ x1 + habitat), un rapport de quatre
# paragraphes qui les cite, une figure de deux panneaux.
texte_base <- c("# Rapport", "", "Le modèle M1 explique {{M1.R2X}} de la variance.", "", "La régression M2 donne R² = {{M2.R2}}.", "", "Conclusion provisoire.")
figure_base <- '{"panneaux":[{"vue":"scores","numero":1,"nom":"M1","loc":{"couleur":"m@1|t1"}},{"vue":"effets","numero":2,"nom":"M2","loc":{}}],"largeur":174}'
id <- demande("ouvrir", list(chemin = source_csv))$id
demande("ajuster", list(projet = id, type = "PCA", x = uid(id, c("x1", "x2", "y", "annee")), A = 1, nom = "M1"))
demande("ajuster", list(projet = id, type = "LM", x = uid(id, c("x1", "habitat")), y = uid(id, "y"), nom = "M2"))
demande("document_enregistrer", list(projet = id, titre = "Rapport", texte = paste(texte_base, collapse = "\n")))
demande("figure_enregistrer", list(projet = id, titre = "Figure 1", contenu = figure_base))
base <- fichier("base.nuee")
demande("enregistrer", list(projet = id, chemin = base))
demande("fermer_projet", list(projet = id))

# Un côté : la base (ou `depuis`) ouverte dans la séance, changée par `f(id)`, enregistrée sous `nom` (avec
# la séance de la fenêtre `session`).
cote <- function(nom, f, depuis = base, session = NULL) {
  id <- demande("ouvrir", list(chemin = depuis))$id
  f(id)
  chemin <- fichier(nom)
  demande("enregistrer", c(list(projet = id, chemin = chemin), if (!is.null(session)) list(session = session)))
  demande("fermer_projet", list(projet = id))
  chemin
}
paragraphe <- function(id, k, texte) {
  lignes <- strsplit(document(projet(id), "D1")$texte, "\n", fixed = TRUE)[[1]]
  lignes[k] <- texte
  demande("document_enregistrer", list(projet = id, id = "D1", texte = paste(lignes, collapse = "\n")))
}
fusion <- function(b, m, l, choix = list()) fusionner_projets(lire_projet(b), lire_projet(m), lire_projet(l), choix, dossier = etude, qui = "Bob")

# ---------------------------------------------------------------- sans conflit

cat("Les deux côtés : des modèles (un même nom renommé), un document, une figure, deux nettoyages par les étapes\n")
mien_a <- cote("mien_a.nuee", function(id) {
  demande("remplacer", list(projet = id, variables = uid(id, "comptage"), chercher = "-9999", remplacer = ""))
  demande("ajuster", list(projet = id, type = "PLS", x = uid(id, c("x1", "x2")), y = uid(id, "y"), nom = "M3"))
  paragraphe(id, 3, "Le modèle M1 résume {{M1.R2X}} de la variance des quatre mesures.")
  demande("figure_enregistrer", list(projet = id, id = "F1", titre = "Figure 1 (révisée)"))
})
leur_a <- cote("leur_a.nuee", function(id) {
  cats <- categories_de(projet(id), "habitat")
  demande("categories", list(projet = id, variable = uid(id, "habitat"), noms = ifelse(cats == "foret", "forêt", cats)))
  demande("ajuster", list(projet = id, type = "PCA", x = uid(id, c("x1", "comptage")), A = 1, nom = "M3"))
  paragraphe(id, 7, "Conclusion : à confirmer.\n\nLeur M3 : {{M3.R2X}} ; M1 : {{M1.Q2}}.")
  f <- jsonlite::fromJSON(figure_base, simplifyVector = FALSE)
  f$panneaux[[3]] <- list(vue = "scores", numero = 3L, nom = "M3", loc = list(couleur = "m@3|t1"))
  demande("figure_enregistrer", list(projet = id, id = "F1", contenu = as.character(jsonlite::toJSON(f, auto_unbox = TRUE))))
  demande("figure_enregistrer", list(projet = id, titre = "Leur figure", contenu = '{"panneaux":[{"vue":"scores","numero":3,"nom":"M3","loc":{}}]}'))
})
r <- fusion(base, mien_a, leur_a)
p <- r$projet
verifier("aucun conflit, les données fusionnées par les étapes", !length(r$conflits) && identical(r$donnees$mode, "etapes"))
verifier("les étapes : la mienne, puis la leur, rejouées depuis la source ; les données conformes",
         identical(etiquettes(p), c("remplacement de -9999", "catégories de habitat")) && isTRUE(donnees_conformes(p)) &&
           identical(p$source_donnees$sha256, lire_projet(base)$source_donnees$sha256))
direct <- cote("direct_a.nuee", function(id) {
  demande("remplacer", list(projet = id, variables = uid(id, "comptage"), chercher = "-9999", remplacer = ""))
  cats <- categories_de(projet(id), "habitat")
  demande("categories", list(projet = id, variable = uid(id, "habitat"), noms = ifelse(cats == "foret", "forêt", cats)))
})
pd <- lire_projet(direct)
verifier("les deux nettoyages, comme faits l'un après l'autre : -9999 vidés, « foret » renommée « forêt »",
         identical(valeurs(p, "comptage"), valeurs(pd, "comptage")) && identical(valeurs(p, "habitat"), valeurs(pd, "habitat")) &&
           !any(valeurs(p, "comptage") %in% -9999) && identical(categories_de(p, "habitat"), categories_de(pd, "habitat")) && !"foret" %in% categories_de(p, "habitat"))
verifier("les modèles : M1, M2, mon M3 (PLS), leur M3 devenu M4 sous le numéro 4",
         identical(vapply(p$models, function(d) d$name, ""), c("M1", "M2", "M3", "M4")) && identical(definition(p, "M3")$type, "PLS") &&
           identical(definition(p, "M4")$type, "PCA") && definition(p, "M4")$number == 4 && definition(p, "M3")$number == 3)
verifier("leur M4 a pris le terme de la catégorie renommée, les termes de M2 aussi (habitat(forêt))",
         "habitat(forêt)" %in% vapply(definition(p, "M2")$x, function(t) t$name, "") && !"habitat(foret)" %in% vapply(definition(p, "M2")$x, function(t) t$name, ""))
lignes <- strsplit(document(p, "D1")$texte, "\n", fixed = TRUE)[[1]]
verifier("le document : mon paragraphe 2, leur conclusion, leur référence {{M3.R2X}} devenue {{M4.R2X}}, {{M1.Q2}} inchangée",
         identical(lignes[3], "Le modèle M1 résume {{M1.R2X}} de la variance des quatre mesures.") && identical(lignes[7], "Conclusion : à confirmer.") &&
           identical(lignes[9], "Leur M3 : {{M4.R2X}} ; M1 : {{M1.Q2}}.") && identical(lignes[5], texte_base[5]))
f1 <- figure(p, "F1")
pn <- panneaux(f1)
verifier("la figure F1 : mon titre, leur contenu, leur panneau de M3 devenu celui de M4 (numéro, nom, couleur m@4|t1)",
         identical(f1$titre, "Figure 1 (révisée)") && length(pn) == 3 && pn[[3]]$numero == 4 && identical(pn[[3]]$nom, "M4") &&
           identical(pn[[3]]$loc$couleur, "m@4|t1") && pn[[1]]$numero == 1 && identical(jsonlite::fromJSON(f1$contenu)$largeur, 174L))
f2 <- figure(p, "F2")
verifier("leur nouvelle figure F2, son panneau sur M4", !is.null(f2) && identical(f2$titre, "Leur figure") && panneaux(f2)[[1]]$numero == 4)
verifier("ce que chaque côté a apporté, en phrases, chaque étape dite comme la vue des étapes la dit",
         any(grepl("M3", r$apports$mien)) && "1 étape(s) : remplacement de « -9999 » par vide dans comptage" %in% r$apports$mien &&
           "1 étape(s) : catégories de habitat" %in% r$apports$leur && any(grepl("renommé M4", r$apports$leur)) &&
           any(grepl("Rapport", r$apports$leur)) && any(grepl("figure", r$apports$leur)))
verifier("le projet fusionné s'enregistre, se rouvre et chacun de ses modèles se réajuste", reajuste(p, "fusion_a"))

cat("Le même modèle et la même étape faits des deux côtés ; deux documents et deux figures nouveaux ; deux variables calculées dont les numéros se croisent\n")
mien_b <- cote("mien_b.nuee", session = '{"fenetre":"mienne"}', function(id) {
  demande("remplacer", list(projet = id, variables = uid(id, "comptage"), chercher = "-9999", remplacer = ""))
  demande("ajuster", list(projet = id, type = "PCA", x = uid(id, c("x1", "x2")), A = 1, nom = "Commun"))
  demande("variable_calculee", list(projet = id, nom = "w", formule = "x1 * 2"))
  demande("document_enregistrer", list(projet = id, titre = "Notes", texte = "Mes notes"))
  demande("figure_enregistrer", list(projet = id, titre = "Ma figure", contenu = '{"panneaux":[]}'))
})
leur_b <- cote("leur_b.nuee", session = '{"fenetre":"leur"}', function(id) {
  demande("remplacer", list(projet = id, variables = uid(id, "comptage"), chercher = "-9999", remplacer = ""))
  demande("ajuster", list(projet = id, type = "PCA", x = uid(id, c("x1", "x2")), A = 1, nom = "Commun"))
  demande("variable_calculee", list(projet = id, nom = "z", formule = "x1 + x2"))
  demande("ajuster", list(projet = id, type = "LM", x = uid(id, "z"), y = uid(id, "y"), nom = "Mz"))
  demande("document_enregistrer", list(projet = id, titre = "Notes", texte = "Leurs notes"))
  demande("figure_enregistrer", list(projet = id, titre = "Leur figure", contenu = '{"panneaux":[]}'))
})
pm <- lire_projet(mien_b)
pl <- lire_projet(leur_b)
r <- fusion(base, mien_b, leur_b)
p <- r$projet
verifier("« Commun », le même des deux côtés : une fois, sans conflit ni renommage",
         !length(r$conflits) && sum(vapply(p$models, function(d) identical(d$name, "Commun"), TRUE)) == 1 && !any(grepl("renommé", r$apports$leur)))
verifier("le même remplacement fait des deux côtés : une seule étape, puis w et z", identical(etiquettes(p), c("remplacement de -9999", "variable w", "variable z")))
verifier("deux documents nouveaux sous le même identifiant (D2) : les deux, le leur en D3 ; de même les figures (F2, F3)",
         identical(document(p, "D2")$texte, "Mes notes") && identical(document(p, "D3")$texte, "Leurs notes") &&
           identical(figure(p, "F2")$titre, "Ma figure") && identical(figure(p, "F3")$titre, "Leur figure"))
verifier("la séance de la fenêtre est la mienne", identical(p$session, '{"fenetre":"mienne"}'))
verifier("w et z avaient le même numéro de chaque côté ; z en prend un autre, et Mz le suit",
         uid_p(pm, "w") == uid_p(pl, "z") && uid_p(p, "w") == uid_p(pm, "w") && uid_p(p, "z") != uid_p(pl, "z") &&
           definition(p, "Mz")$x[[1]]$variables == uid_p(p, "z") && isTRUE(all.equal(unname(valeurs(p, "z")), unname(valeurs(pl, "z")))))
verifier("Mz se réajuste sur z", reajuste(p, "fusion_b"))

# ---------------------------------------------------------------- les conflits, par les étapes

cat("Chaque sorte de conflit, par les étapes : modèle, document, figure, variable, unité, coordonnées\n")
mien_c <- cote("mien_c.nuee", function(id) {
  demande("ajuster", list(projet = id, numero = 1, A = 2))
  paragraphe(id, 3, "Mon paragraphe deux.")
  demande("figure_enregistrer", list(projet = id, id = "F1", contenu = '{"panneaux":[{"vue":"scores","numero":1,"nom":"M1","loc":{}}]}'))
  demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), "S1-2018"), variable = uid(id, "x1"), valeur = "9"))
  demande("decrire_variables", list(projet = id, descriptions = list(list(variable = uid(id, "x1"), unite = "mm"))))
  demande("coordonnees", list(projet = id, regler = TRUE, x = uid(id, "X"), y = uid(id, "Y"), systeme = "2154"))
})
leur_c <- cote("leur_c.nuee", function(id) {
  demande("ajuster", list(projet = id, numero = 1, A = 3))
  paragraphe(id, 3, "Leur paragraphe deux.")
  paragraphe(id, 7, "Conclusion de Bob.")
  demande("figure_enregistrer", list(projet = id, id = "F1", contenu = '{"panneaux":[{"vue":"effets","numero":2,"nom":"M2","loc":{}}]}'))
  demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), "S1-2018"), variable = uid(id, "x1"), valeur = "7"))
  demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), "S2-2018"), variable = uid(id, "x2"), valeur = "1,5"))
  demande("decrire_variables", list(projet = id, descriptions = list(list(variable = uid(id, "x1"), unite = "cm"))))
  demande("coordonnees", list(projet = id, regler = TRUE, x = uid(id, "Y"), y = uid(id, "X"), systeme = "2154"))
})
r <- fusion(base, mien_c, leur_c)
attendus <- c("modele:M1", "document:D1", "figure:F1", "variable:x1", "champ:variable:x1:unite", "champ:coordonnees")
verifier("six conflits, un de chaque sorte, chacun une fois", setequal(ids_conflits(r), attendus) && !anyDuplicated(ids_conflits(r)) &&
           setequal(vapply(r$conflits, function(x) x$genre, ""), c("modele", "document", "figure", "variable", "champ", "champ")))
verifier("ce que chaque côté a fait de M1 : « M1 : 1 composante(s) → 2 composante(s) », et → 3",
         grepl("1 composante(s) → 2 composante(s)", conflit(r, "modele:M1")$mien, fixed = TRUE) && grepl("→ 3 composante(s)", conflit(r, "modele:M1")$leur, fixed = TRUE))
d1 <- conflit(r, "document:D1")
verifier("le document : « paragraphe 2 réécrit », le passage en conflit de chaque côté",
         identical(d1$mien, "paragraphe 2 réécrit") && identical(d1$details$passages[[1]]$mien, "Mon paragraphe deux.") &&
           identical(d1$details$passages[[1]]$leur, "Leur paragraphe deux.") && identical(d1$details$passages[[1]]$base, texte_base[3]))
verifier("la variable : la cellule, sa valeur dans la base et de chaque côté",
         conflit(r, "variable:x1")$details$n == 1 && identical(conflit(r, "variable:x1")$details$cellules[[1]]$observation, "S1-2018") &&
           identical(conflit(r, "variable:x1")$details$cellules[[1]]$mien, "9") && identical(conflit(r, "variable:x1")$details$cellules[[1]]$leur, "7"))
verifier("l'unité et les coordonnées : ce que chaque côté en a fait",
         identical(conflit(r, "champ:variable:x1:unite")$mien, "mm") && identical(conflit(r, "champ:variable:x1:unite")$leur, "cm") &&
           identical(conflit(r, "champ:coordonnees")$mien, "X, Y (2154)") && identical(conflit(r, "champ:coordonnees")$leur, "Y, X (2154)"))
choix <- function(cote) stats::setNames(as.list(rep(cote, length(attendus))), attendus)
pm <- fusion(base, mien_c, leur_c, choix("mien"))$projet
pl <- fusion(base, mien_c, leur_c, choix("leur"))$projet
lm_ <- strsplit(document(pm, "D1")$texte, "\n", fixed = TRUE)[[1]]
ll_ <- strsplit(document(pl, "D1")$texte, "\n", fixed = TRUE)[[1]]
verifier("le mien : M1 à 2 composantes, mon paragraphe 2 (et leur conclusion, qui n'est pas en conflit), ma figure",
         definition(pm, "M1")$A == 2 && identical(lm_[3], "Mon paragraphe deux.") && identical(lm_[7], "Conclusion de Bob.") && length(panneaux(figure(pm, "F1"))) == 1 &&
           identical(panneaux(figure(pm, "F1"))[[1]]$vue, "scores"))
verifier("le leur : M1 à 3 composantes, leur paragraphe 2, leur figure",
         definition(pl, "M1")$A == 3 && identical(ll_[3], "Leur paragraphe deux.") && identical(ll_[7], "Conclusion de Bob.") && identical(panneaux(figure(pl, "F1"))[[1]]$vue, "effets"))
x1 <- function(p) unname(valeurs(p, "x1")["S1-2018"])
verifier("la cellule : la mienne (9) par une étape de plus, la leur (7) ; leur cellule sans conflit (x2 = 1,5) des deux fois",
         x1(pm) == 9 && x1(pl) == 7 && unname(valeurs(pm, "x2")["S2-2018"]) == 1.5 && unname(valeurs(pl, "x2")["S2-2018"]) == 1.5 &&
           any(grepl("fusion : valeurs de x1 de mon côté", etiquettes(pm), fixed = TRUE)) && !any(grepl("fusion", etiquettes(pl))))
verifier("les étapes de la fusion, dans la vue des étapes : « fusion avec Bob : … »",
         all(c("fusion avec Bob : cellules collées dans x1", "fusion avec Bob : unités et descriptions de x1", "fusion avec Bob : coordonnées X et Y") %in%
               vapply(pm$etapes, etiquette_etape, "")))
verifier("l'unité : « mm » ou « cm » ; les coordonnées : X, Y ou Y, X ; les données restent celles que leurs étapes donnent",
         identical(variable_du_projet(pm, uid_p(pm, "x1"))$unite, "mm") && identical(variable_du_projet(pl, uid_p(pl, "x1"))$unite, "cm") &&
           pm$coordonnees$x == uid_p(pm, "X") && pl$coordonnees$x == uid_p(pl, "Y") && isTRUE(donnees_conformes(pm)) && isTRUE(donnees_conformes(pl)))
rejoue <- mise_a_jour(pm, dossier = etude)$projet
verifier("rejouées depuis la source, les étapes du mien redonnent ses données (la cellule et l'unité gardées)",
         x1(rejoue) == 9 && identical(variable_du_projet(rejoue, uid_p(rejoue, "x1"))$unite, "mm") && identical(valeurs(rejoue, "x2"), valeurs(pm, "x2")))
verifier("sans choix, le mien", x1(fusion(base, mien_c, leur_c)$projet) == 9)
verifier("les deux projets fusionnés se réajustent", reajuste(pm, "fusion_c_mien") && reajuste(pl, "fusion_c_leur"))

cat("Les paragraphes d'un passage en conflit, comptés comme l'éditeur les montre (une ligne chacun), dans mon texte\n")
doc_base <- cote("doc_base.nuee", function(id) demande("document_enregistrer", list(projet = id, id = "D1", texte = "Titre\nUn\nDeux")))
ecrit <- function(nom, texte) cote(nom, depuis = doc_base, function(id) demande("document_enregistrer", list(projet = id, id = "D1", texte = texte)))
passage <- function(r) conflit(r, "document:D1")$details$passages[[1]]
r <- fusion(doc_base, ecrit("doc_mien1.nuee", "Titre\nUn, le mien\nDeux"), ecrit("doc_leur1.nuee", "Titre\nUn, le leur\nDeux"))
verifier("un conflit sur la ligne 2 : « paragraphe 2 réécrit », et le passage à la ligne 2",
         identical(conflit(r, "document:D1")$mien, "paragraphe 2 réécrit") && identical(conflit(r, "document:D1")$leur, "paragraphe 2 réécrit") && passage(r)$ligne == 2)
r <- fusion(doc_base, ecrit("doc_mien2.nuee", "Nouveau\nTitre\nUn, le mien\nDeux"), ecrit("doc_leur2.nuee", "Titre\nUn, le leur\nDeux"))
verifier("mon paragraphe ajouté en tête : le passage, le paragraphe 3 de mon texte, des deux côtés",
         identical(conflit(r, "document:D1")$mien, "paragraphe 3 réécrit") && identical(conflit(r, "document:D1")$leur, "paragraphe 3 réécrit") && passage(r)$ligne == 3)
verifier("… mon paragraphe ajouté, sans conflit, gardé avec l'un comme avec l'autre choix",
         identical(document(r$projet, "D1")$texte, "Nouveau\nTitre\nUn, le mien\nDeux") &&
           identical(document(fusion(doc_base, fichier("doc_mien2.nuee"), fichier("doc_leur2.nuee"), list(`document:D1` = "leur"))$projet, "D1")$texte, "Nouveau\nTitre\nUn, le leur\nDeux"))
r <- fusion(doc_base, ecrit("doc_mien3.nuee", "Titre\nUn\nDeux\nMa fin"), ecrit("doc_leur3.nuee", "Titre\nUn\nDeux\nLeur fin"))
verifier("du texte ajouté au même endroit des deux côtés : « texte ajouté après le paragraphe 3 », à la ligne 4",
         identical(conflit(r, "document:D1")$mien, "texte ajouté après le paragraphe 3") && passage(r)$ligne == 4)
r <- fusion(doc_base, ecrit("doc_mien4.nuee", "Titre"), ecrit("doc_leur4.nuee", "Titre\nUn, le leur\nDeux, le leur"))
verifier("des paragraphes que j'ai retirés et qu'ils ont réécrits : « texte retiré après le paragraphe 1 », « texte réécrit … »",
         identical(conflit(r, "document:D1")$mien, "texte retiré après le paragraphe 1") && identical(conflit(r, "document:D1")$leur, "texte réécrit après le paragraphe 1") &&
           passage(r)$ligne == 2)

cat("La source des données : les deux côtés mis à jour d'un autre fichier\n")
avec_lignes <- function(nom, quels, graine) {
  f <- fichier("donnees", nom)
  utils::write.csv(rbind(df, tableau(2018, quels, graine)), f, row.names = FALSE, na = "")
  f
}
fichier_m <- avec_lignes("releves_mien.csv", "S5", 2)
fichier_l <- avec_lignes("releves_leur.csv", "S6", 3)
mien_d <- cote("mien_d.nuee", function(id) demande("mettre_a_jour", list(projet = id, chemin = fichier_m)))
leur_d <- cote("leur_d.nuee", function(id) {
  demande("mettre_a_jour", list(projet = id, chemin = fichier_l))
  demande("remplacer", list(projet = id, variables = uid(id, "comptage"), chercher = "-9999", remplacer = ""))
})
r <- fusion(base, mien_d, leur_d)
verifier("un conflit, la source des données, et ce que chaque côté en a fait",
         identical(ids_conflits(r), "source") && identical(conflit(r, "source")$mien, "données mises à jour de releves_mien.csv") &&
           identical(conflit(r, "source")$leur, "données mises à jour de releves_leur.csv") && identical(r$donnees$mode, "etapes"))
pm <- fusion(base, mien_d, leur_d, list(source = "mien"))$projet
pl <- fusion(base, mien_d, leur_d, list(source = "leur"))$projet
verifier("le mien : mon fichier, leur étape rejouée après (plus de -9999), ma ligne S5 et pas la leur",
         identical(basename(pm$source_donnees$chemin), "releves_mien.csv") && "S5-2018" %in% names(valeurs(pm, "x1")) && !"S6-2018" %in% names(valeurs(pm, "x1")) &&
           !any(valeurs(pm, "comptage") %in% -9999) && isTRUE(donnees_conformes(pm)))
verifier("le leur : leur fichier et leurs étapes, leur ligne S6",
         identical(basename(pl$source_donnees$chemin), "releves_leur.csv") && "S6-2018" %in% names(valeurs(pl, "x1")) && !"S5-2018" %in% names(valeurs(pl, "x1")) &&
           !any(valeurs(pl, "comptage") %in% -9999) && isTRUE(donnees_conformes(pl)))

cat("Un seul côté a mis les données à jour : sa source et ses étapes, l'étape de l'autre rejouée après\n")
mien_d2 <- cote("mien_d2.nuee", function(id) demande("remplacer", list(projet = id, variables = uid(id, "comptage"), chercher = "-9999", remplacer = "")))
leur_d2 <- cote("leur_d2.nuee", function(id) demande("mettre_a_jour", list(projet = id, chemin = fichier_l)))
r <- fusion(base, mien_d2, leur_d2)
p <- r$projet
verifier("sans conflit, par les étapes : leur fichier, leur ligne S6, mon remplacement rejoué sur elle aussi",
         !length(r$conflits) && identical(r$donnees$mode, "etapes") && identical(basename(p$source_donnees$chemin), "releves_leur.csv") &&
           "S6-2018" %in% names(valeurs(p, "x1")) && !any(valeurs(p, "comptage") %in% -9999) && identical(etiquettes(p), "remplacement de -9999") &&
           isTRUE(donnees_conformes(p)) && length(definition(p, "M2")$observations) == 33)
verifier("ce que leur côté a apporté : la mise à jour, une observation", any(grepl("releves_leur.csv", r$apports$leur, fixed = TRUE)) && any(grepl("1 observation(s) ajoutée(s)", r$apports$leur, fixed = TRUE)))

cat("Cellule par cellule quand les étapes ne se rejouent pas : la source introuvable, des données changées hors des étapes\n")
cache <- paste0(source_csv, ".cache")
file.rename(source_csv, cache)
r <- fusion(base, mien_a, leur_a)
file.rename(cache, source_csv)
p <- r$projet
verifier("la source introuvable : par les cellules, la raison dite, les deux nettoyages quand même, la catégorie renommée à sa place",
         identical(r$donnees$mode, "cellules") && grepl("introuvable", r$donnees$raison) && !length(r$conflits) &&
           identical(valeurs(p, "comptage"), valeurs(pd, "comptage")) && identical(valeurs(p, "habitat"), valeurs(pd, "habitat")) &&
           identical(categories_de(p, "habitat"), c("forêt", "lande", "prairie")))
verifier("les deux étapes gardées, l'empreinte rompue (on ne sait plus que les données sont ce qu'elles donnent)",
         identical(etiquettes(p), c("remplacement de -9999", "catégories de habitat")) && identical(p$empreinte_donnees, ""))
leur_hors <- cote("leur_hors.nuee", function(id) {
  # (comme la console R : une valeur changée sans étape)
  q <- projet(id)
  q$donnees[as.character(numeros_observations(q, "S1-2018")), as.character(uid(id, "x1"))] <- 42
  etat$projets[[id]] <- q
})
r <- fusion(base, mien_a, leur_hors)
verifier("leurs données changées hors des étapes : par les cellules, la raison dite, leur valeur prise",
         identical(r$donnees$mode, "cellules") && grepl("hors des étapes", r$donnees$raison) && unname(valeurs(r$projet, "x1")["S1-2018"]) == 42 &&
           !any(valeurs(r$projet, "comptage") %in% -9999))

cat("Une étape de leur côté qui ne s'applique plus après les miennes : la fusion se fait cellule par cellule\n")
mien_e <- cote("mien_e.nuee", function(id) demande("variable_calculee", list(projet = id, nom = "v", formule = "x1 * 2")))
leur_e <- cote("leur_e.nuee", function(id) demande("variable_calculee", list(projet = id, nom = "v", formule = "x1 * 3")))
r <- fusion(base, mien_e, leur_e)
verifier("la raison dite, et la variable faite autrement des deux côtés, un conflit",
         identical(r$donnees$mode, "cellules") && grepl("variable v", r$donnees$raison, fixed = TRUE) && identical(ids_conflits(r), "variable:v"))
pl <- fusion(base, mien_e, leur_e, list(`variable:v` = "leur"))$projet
verifier("le leur : leurs valeurs ; les étapes des deux côtés gardées, l'empreinte rompue (les données ne sont plus ce qu'elles donnent)",
         isTRUE(all.equal(unname(valeurs(pl, "v")), unname(valeurs(lire_projet(leur_e), "v")))) && length(pl$etapes) == 2 && identical(pl$empreinte_donnees, ""))

cat("Par les étapes, seulement sur les fichiers tels qu'ils ont été lus, chaque étape refaite et chaque variable là\n")
# (une étude à part : six observations, chaque côté corrige une valeur de x)
etude2 <- file.path(dossier, "etude2")
dir.create(file.path(etude2, "donnees"), recursive = TRUE)
csv2 <- file.path(etude2, "donnees", "releves.csv")
df2 <- data.frame(nom = paste0("O", 1:6), x = c(1, 2, 3, 4, 5, 6), z = c(7, 3, 8, 1, 9, 2), y = c(10, 21, 29, 42, 50, 61))
utils::write.csv(df2, csv2, row.names = FALSE)
id2 <- demande("ouvrir", list(chemin = csv2))$id
demande("ajuster", list(projet = id2, type = "LM", x = uid(id2, "x"), y = uid(id2, "y"), nom = "M1"))
base2 <- file.path(etude2, "base.nuee")
demande("enregistrer", list(projet = id2, chemin = base2))
demande("fermer_projet", list(projet = id2))
cote2 <- function(nom, f) {
  id <- demande("ouvrir", list(chemin = base2))$id
  f(id)
  chemin <- file.path(etude2, nom)
  demande("enregistrer", list(projet = id, chemin = chemin))
  demande("fermer_projet", list(projet = id))
  chemin
}
corriger <- function(id, o, v, x) demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), o), variable = uid(id, v), valeur = x))
fusion2 <- function(m, l, choix = list()) fusionner_projets(lire_projet(base2), lire_projet(m), lire_projet(l), choix, dossier = etude2, qui = "Bob")
mien2 <- cote2("mien.nuee", function(id) corriger(id, "O1", "x", "100"))
leur2 <- cote2("leur.nuee", function(id) corriger(id, "O2", "x", "200"))
# (le fichier source changé depuis qu'il a été lu, sans que personne ne mette les données à jour : x de O3
# corrigé, une ligne O7 de plus)
utils::write.csv(rbind(transform(df2, x = replace(x, 3, 33)), data.frame(nom = "O7", x = 7, z = 5, y = 70)), csv2, row.names = FALSE)
r <- fusion2(mien2, leur2)
verifier("la source changée depuis sa lecture : cellule par cellule, la raison dite ; ni sa valeur corrigée ni sa ligne nouvelle, la source d'avant",
         identical(r$donnees$mode, "cellules") && grepl("a changé depuis qu'elle a été lue", r$donnees$raison) &&
           identical(unname(valeurs(r$projet, "x")), c(100, 200, 3, 4, 5, 6)) && identical(r$projet$source_donnees$sha256, lire_projet(base2)$source_donnees$sha256))
utils::write.csv(df2, csv2, row.names = FALSE)
verifier("le fichier rendu tel qu'il était : de nouveau par les étapes", identical(fusion2(mien2, leur2)$donnees$mode, "etapes"))
# (leur côté a mis les données à jour du fichier de l'année suivante, et ajouté des colonnes d'un fichier
# qui n'est que sur son ordinateur : leur étape ne se refait pas ici)
csv26 <- file.path(etude2, "donnees", "releves_2026.csv")
utils::write.csv(rbind(df2, data.frame(nom = "O7", x = 7, z = 5, y = 70)), csv26, row.names = FALSE)
ailleurs <- file.path(dossier, "chez_bob")
dir.create(ailleurs)
sites2 <- file.path(ailleurs, "sites.csv")
utils::write.csv(data.frame(nom = paste0("O", 1:7), altitude = c(110, 120, 130, 140, 150, 160, 170)), sites2, row.names = FALSE)
leur26 <- cote2("leur26.nuee", function(id) {
  demande("mettre_a_jour", list(projet = id, chemin = csv26))
  demande("ajouter_colonnes", list(projet = id, chemin = sites2, nom = "sites.csv", cles_projet = list("id:1"), cles_table = list("nom"), colonnes = list("altitude")))
})
unlink(sites2)
r <- fusion2(mien2, leur26)
verifier("une étape de leur côté qui ne se refait pas ici : cellule par cellule, la raison dite, leur colonne altitude gardée",
         identical(r$donnees$mode, "cellules") && grepl("ne se refait pas", r$donnees$raison) && grepl("sites.csv", r$donnees$raison, fixed = TRUE) &&
           "altitude" %in% r$projet$noms_var && unname(valeurs(r$projet, "altitude")["O7"]) == 170 && unname(valeurs(r$projet, "x")["O1"]) == 100)
# (leur côté a mis les données à jour, et corrigé z ; le mien a supprimé z : rejouées, mes étapes après
# les leurs ôteraient z et leur correction sans rien dire)
leur_z <- cote2("leur_z.nuee", function(id) {
  demande("mettre_a_jour", list(projet = id, chemin = csv26))
  corriger(id, "O2", "z", "30")
})
mien_z <- cote2("mien_z.nuee", function(id) demande("supprimer_variable", list(projet = id, variable = uid(id, "z"))))
r <- fusion2(mien_z, leur_z)
verifier("une variable supprimée de mon côté, changée du leur, que les étapes rejouées n'ont plus : cellule par cellule, un conflit",
         identical(r$donnees$mode, "cellules") && grepl("variable « z »", r$donnees$raison, fixed = TRUE) && "variable:z" %in% ids_conflits(r))
verifier("le leur : z et leur correction", isTRUE(tryCatch(unname(valeurs(fusion2(mien_z, leur_z, list(`variable:z` = "leur"))$projet, "z")["O2"]) == 30, error = function(e) FALSE)))
# (supprimée d'un côté, inchangée de l'autre : par les étapes)
r <- fusion2(mien_z, leur26 <- cote2("leur_o7.nuee", function(id) demande("mettre_a_jour", list(projet = id, chemin = csv26))))
verifier("une variable supprimée d'un côté, inchangée de l'autre : par les étapes, sans elle", identical(r$donnees$mode, "etapes") && !"z" %in% r$projet$noms_var &&
           "O7" %in% names(valeurs(r$projet, "x")))

# ---------------------------------------------------------------- cellule par cellule

cat("Cellule par cellule (des projets de la 1.0, sans étapes) : des lignes ajoutées des deux côtés, une cellule en conflit, des catégories\n")
base10 <- fichier("base10.nuee")
j <- jsonlite::fromJSON(base, simplifyVector = FALSE)
j$source_donnees <- j$etapes <- j$empreinte_donnees <- NULL
j$prediction_sets <- list(list(name = "Nouveaux", observations = list(1L, 2L)))  # (comme ceux d'un fichier .usp)
ecrire_json <- function(j, f) writeLines(enc2utf8(as.character(jsonlite::toJSON(j, auto_unbox = TRUE, null = "null", na = "null", digits = NA))), f, useBytes = TRUE)
ecrire_json(j, base10)
# (mes lignes : S5 en 2018 et 2019 ; les leurs : la même ligne S5-2018, et S6-2018 ; les deux mises à jour
# donnent aux lignes nouvelles les mêmes numéros)
s5 <- tableau(2018:2019, "S5", 4)
s6 <- tableau(2018, "S6", 5)
lignes <- function(nom, x) {
  f <- fichier("donnees", nom)
  utils::write.csv(rbind(df, x), f, row.names = FALSE, na = "")
  f
}
mien_f <- cote("mien_f.nuee", depuis = base10, function(id) {
  demande("mettre_a_jour", list(projet = id, chemin = lignes("lignes_mien.csv", s5)))
  demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), "S1-2018"), variable = uid(id, "x1"), valeur = "9"))
  demande("ajuster", list(projet = id, type = "PCA", x = uid(id, c("x1", "y")), A = 1, nom = "Mien"))
})
leur_f <- cote("leur_f.nuee", depuis = base10, function(id) {
  demande("mettre_a_jour", list(projet = id, chemin = lignes("lignes_leur.csv", rbind(s5[1, ], s6))))
  demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), "S1-2018"), variable = uid(id, "x1"), valeur = "7"))
  demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), "S2-2018"), variable = uid(id, "x2"), valeur = "1,5"))
})
unlink(fichier("donnees", c("lignes_mien.csv", "lignes_leur.csv")))
pm <- lire_projet(mien_f)
pl <- lire_projet(leur_f)
r <- fusion(base10, mien_f, leur_f)
p <- r$projet
verifier("par les cellules (la base n'a pas d'étapes), sans raison à dire ; deux conflits : la source et la cellule de x1",
         identical(r$donnees$mode, "cellules") && identical(r$donnees$raison, "") && setequal(ids_conflits(r), c("source", "variable:x1")) &&
           conflit(r, "variable:x1")$details$n == 1)
r0 <- fusion(base10, cote("mien_f0.nuee", depuis = base10, function(id) demande("ajuster", list(projet = id, type = "PCA", x = uid(id, c("x1", "y")), A = 1, nom = "Seul"))),
             cote("leur_f0.nuee", depuis = base10, function(id) paragraphe(id, 7, "Conclusion de Bob.")))
verifier("sans fichier source, ni l'un ni l'autre côté ne touchant aux données : « inchangees », rien à en dire",
         identical(r0$donnees$mode, "inchangees") && identical(r0$donnees$raison, "") && !length(r0$conflits))
noms <- names(valeurs(p, "x1"))
verifier("mes lignes de S5 et leur ligne de S6 ; S5-2018, ajoutée des deux côtés avec les mêmes valeurs, une fois",
         all(c("S5-2018", "S5-2019", "S6-2018") %in% noms) && length(noms) == 35 && !anyDuplicated(noms))
verifier("leur ligne S6-2018 a pris un autre numéro que le leur (les deux mises à jour ont donné le même), ses valeurs suivent",
         numeros_observations(pl, "S6-2018") %in% numeros_observations(pm) && numeros_observations(p, "S6-2018") != numeros_observations(pl, "S6-2018") &&
           unname(valeurs(p, "y")["S6-2018"]) == unname(valeurs(pl, "y")["S6-2018"]))
verifier("leur cellule sans conflit (x2 de S2-2018 = 1,5), la mienne en conflit gardée par défaut (9)",
         unname(valeurs(p, "x2")["S2-2018"]) == 1.5 && unname(valeurs(p, "x1")["S1-2018"]) == 9)
verifier("le leur pour x1 : 7", unname(valeurs(fusion(base10, mien_f, leur_f, list(`variable:x1` = "leur"))$projet, "x1")["S1-2018"]) == 7)
verifier("les catégories : celles de la base, « marais » des deux côtés, « dune » du leur, avant la dernière qui reste la dernière",
         identical(categories_de(pm, "habitat"), c("foret", "lande", "marais", "prairie")) &&
           identical(categories_de(p, "habitat"), c("foret", "lande", "marais", "dune", "prairie")))
m2 <- definition(p, "M2")
verifier("M2 : le terme de chaque catégorie nouvelle des deux côtés, dans l'ordre des catégories ; toutes les lignes",
         identical(vapply(m2$x, function(t) t$name, ""), c("x1", "habitat(foret)", "habitat(lande)", "habitat(marais)", "habitat(dune)", "habitat(prairie)")) &&
           length(m2$observations) == 35 && !"modele:M2" %in% ids_conflits(r))
verifier("mon modèle nouveau prend leur ligne (S6-2018), comme après une mise à jour",
         length(definition(p, "Mien")$observations) == 35 && numeros_observations(p, "S6-2018") %in% definition(p, "Mien")$observations)
id <- demande("ouvrir", list(chemin = { f <- file.path(dossier, "fusion_f.nuee"); enregistrer_projet(p, f); f }))$id
m <- demande("ajuster", list(projet = id, numero = definition(p, "M2")$number))
e <- modele(m$id)
d <- tableau_projet(projet(id))
d <- d[d$Observation %in% unname(projet(id)$noms_obs[as.character(e$modele$observations)]), ]
ref <- stats::lm(y ~ x1 + habitat, data = transform(d, habitat = stats::relevel(factor(habitat, levels = categories_de(p, "habitat")), ref = "prairie")))
coefs <- e$modele$resultat$coefficients
verifier("M2 réajusté : la référence reste « prairie », ses coefficients ceux de lm() avec relevel()",
         isTRUE(all.equal(unname(coefs$estimation[match("habitat(dune)", coefs$terme)]), unname(stats::coef(ref)["habitatdune"]), tolerance = 1e-8)) &&
           !"habitat(prairie)" %in% coefs$terme)
demande("fermer_projet", list(projet = id))
verifier("le projet fusionné s'enregistre, se rouvre et chacun de ses modèles se réajuste", reajuste(p, "fusion_f2"))

cat("Cellule par cellule : une variable et des observations retirées d'un côté, inchangées ou changées de l'autre ; une unité, un jeu de prédiction\n")
sans <- fichier("donnees", "sans.csv")
utils::write.csv(df[!df$nom %in% c("S3-2025", "S4-2025"), setdiff(names(df), "comptage")], sans, row.names = FALSE, na = "")
mien_h <- cote("mien_h.nuee", depuis = base10, function(id) demande("mettre_a_jour", list(projet = id, chemin = sans)))
leur_h <- cote("leur_h.nuee", depuis = base10, function(id) {
  demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), "S1-2018"), variable = uid(id, "comptage"), valeur = "11"))
  demande("modifier_valeur", list(projet = id, observation = numeros_observations(projet(id), "S4-2025"), variable = uid(id, "x1"), valeur = "8"))
  demande("decrire_variables", list(projet = id, descriptions = list(list(variable = uid(id, "x2"), unite = "kg"))))
})
r <- fusion(base10, mien_h, leur_h)
p <- r$projet
verifier("deux conflits : comptage retirée de mon côté et changée du leur, S4-2025 de même",
         setequal(ids_conflits(r), c("variable:comptage", "champ:observations:mien")) && identical(conflit(r, "variable:comptage")$mien, "retirée"))
verifier("le mien : comptage et S4-2025 retirées ; S3-2025, que personne n'a changée de leur côté, retirée sans question ; leur unité de x2 prise",
         !"comptage" %in% p$noms_var && !any(c("S3-2025", "S4-2025") %in% p$noms_obs) && identical(variable_du_projet(p, uid_p(p, "x2"))$unite, "kg"))
pl <- fusion(base10, mien_h, leur_h, list(`variable:comptage` = "leur", `champ:observations:mien` = "leur"))$projet
verifier("le leur : comptage gardée avec leur valeur, S4-2025 aussi ; S3-2025 toujours retirée",
         unname(valeurs(pl, "comptage")["S1-2018"]) == 11 && unname(valeurs(pl, "x1")["S4-2025"]) == 8 && !"S3-2025" %in% pl$noms_obs)
verifier("le projet fusionné s'enregistre, se rouvre et chacun de ses modèles se réajuste", reajuste(pl, "fusion_h"))
prediction <- function(nom, obs) {
  j <- jsonlite::fromJSON(base10, simplifyVector = FALSE)
  j$prediction_sets[[1]]$observations <- as.list(as.integer(obs))
  f <- fichier(nom)
  ecrire_json(j, f)
  f
}
r <- fusion(base10, prediction("mien_p.nuee", c(1, 2, 3)), base10)
verifier("un jeu de prédiction changé d'un côté : ce côté", identical(as.numeric(r$projet$prediction_sets[[1]]$observations), c(1, 2, 3)))
r <- fusion(base10, prediction("mien_p.nuee", c(1, 2, 3)), prediction("leur_p.nuee", c(2, 4)))
verifier("changé autrement des deux côtés : un conflit, et le leur s'il est choisi",
         identical(ids_conflits(r), "champ:prediction:Nouveaux") &&
           identical(as.numeric(fusion(base10, fichier("mien_p.nuee"), fichier("leur_p.nuee"), list(`champ:prediction:Nouveaux` = "leur"))$projet$prediction_sets[[1]]$observations), c(2, 4)))

cat("Les catégories : rangées autrement des deux côtés, renommée d'un côté et rangées de l'autre\n")
rangees <- function(nom, ordre, noms = c("foret", "lande", "prairie")) cote(nom, depuis = base10, function(id) {
  demande("categories", list(projet = id, variable = uid(id, "habitat"), noms = noms, ordre = ordre))
})
termes <- function(p) vapply(Filter(function(t) startsWith(t$name, "habitat"), definition(p, "M2")$x), function(t) t$name, "")
mien_g <- rangees("mien_g.nuee", c("prairie", "lande", "foret"))
leur_g <- rangees("leur_g.nuee", c("lande", "foret", "prairie"))
r <- fusion(base10, mien_g, leur_g)
verifier("l'ordre changé autrement des deux côtés : un conflit, et l'ordre de chacun",
         identical(ids_conflits(r), "champ:categories:habitat") && identical(conflit(r, "champ:categories:habitat")$mien, "prairie, lande, foret") &&
           identical(conflit(r, "champ:categories:habitat")$leur, "lande, foret, prairie"))
pm <- fusion(base10, mien_g, leur_g, list(`champ:categories:habitat` = "mien"))$projet
pl <- fusion(base10, mien_g, leur_g, list(`champ:categories:habitat` = "leur"))$projet
verifier("le mien, le leur ; les termes de M2 suivent l'ordre choisi (sa référence, la dernière), les valeurs ne changent pas",
         identical(categories_de(pm, "habitat"), c("prairie", "lande", "foret")) && identical(categories_de(pl, "habitat"), c("lande", "foret", "prairie")) &&
           identical(termes(pm), c("habitat(prairie)", "habitat(lande)", "habitat(foret)")) && identical(termes(pl), c("habitat(lande)", "habitat(foret)", "habitat(prairie)")) &&
           identical(valeurs(pm, "habitat"), valeurs(lire_projet(base10), "habitat")))
mien_g2 <- rangees("mien_g2.nuee", c("foret", "landes", "prairie"), noms = c("foret", "landes", "prairie"))
leur_g2 <- rangees("leur_g2.nuee", c("prairie", "foret", "lande"))
r <- fusion(base10, mien_g2, leur_g2)
p <- r$projet
verifier("« lande » renommée de mon côté, l'ordre changé du leur : sans conflit, les deux, et les termes de M2 avec eux",
         !length(r$conflits) && identical(categories_de(p, "habitat"), c("prairie", "landes", "foret")) && unname(valeurs(p, "habitat")["S4-2018"]) == "landes" &&
           identical(termes(p), c("habitat(prairie)", "habitat(landes)", "habitat(foret)")))

# ---------------------------------------------------------------- sans rien changer

cat("Une case vide dans la colonne des noms : chaque côté relu, ses cellules à leur place\n")
# (deux lignes sans nom ; chaque côté change une cellule, de l'une d'elles aussi. Leurs noms manquants,
# écrits null, ne se relisaient pas ; la première, d'identifiant vide, ne se retrouvait pas)
vides_csv <- fichier("donnees", "noms vides.csv")
writeLines(c("nom,x,y", "a,1,3", ",4,6", ",7,9", "d,1,1"), vides_csv)
pv0 <- importer_tableau(vides_csv)
attr(pv0, "reglages") <- NULL
mv <- lv <- pv0
mv$donnees["1", "2"] <- 30
mv$donnees["2", "2"] <- 60
lv$donnees["4", "2"] <- 10
rv <- tryCatch(fusionner_projets(pv0, mv, lv, list(), dossier = etude, qui = "Bob"), error = function(e) NULL)
verifier("des projets aux noms vides se relisent et se fusionnent : chaque cellule changée à sa place, celle de la première ligne sans nom comprise",
         !is.null(rv) && !length(rv$conflits) && identical(unname(rv$projet$donnees[, "2"]), c(30, 60, 9, 10)) &&
           identical(unname(rv$projet$noms_obs), c("a", "", "", "d")))

cat("Un côté fusionné avec lui-même, ou avec la base : rien ne change\n")
texte <- function(f) contenu_fusion(lire_projet(f))
r1 <- fusion(base, mien_c, mien_c)
r2 <- fusion(base, mien_c, base)
r3 <- fusion(base, base, leur_c)
r4 <- fusion(base, base, base)
verifier("avec lui-même : le mien, sans conflit", identical(contenu_fusion(r1$projet), texte(mien_c)) && !length(r1$conflits))
verifier("avec la base : le mien", identical(contenu_fusion(r2$projet), texte(mien_c)) && !length(r2$conflits) && !length(r2$apports$leur))
verifier("la base avec le leur : le leur", identical(contenu_fusion(r3$projet), texte(leur_c)) && !length(r3$conflits) && !length(r3$apports$mien))
verifier("la base avec elle-même : la base, rien d'apporté", identical(contenu_fusion(r4$projet), texte(base)) && !length(r4$apports$mien) && !length(r4$apports$leur) &&
           identical(r4$donnees$mode, "inchangees"))

# ---------------------------------------------------------------- un dossier partagé, par les demandes

cat("Un dossier partagé : la copie du fichier lu, fichier_change, la question avant d'écraser, fusionner avec le projet de la séance\n")
partage <- fichier("partage.nuee")
invisible(file.copy(base, partage))
a <- demande("ouvrir", list(chemin = partage))$id
copie_a <- projet(a)$fichier_lu$copie
verifier("ouvert : une copie du fichier tel qu'il a été lu, dans la séance, et rien de plus à côté du fichier",
         file.exists(copie_a) && dans_seance(copie_a) && identical(unname(tools::md5sum(copie_a)), unname(tools::md5sum(partage))) &&
           !any(grepl("copie|^lu", list.files(etude))))
verifier("le même projet que lu du fichier lui-même", identical(contenu_fusion(projet(a)), contenu_fusion(lire_projet(partage))))
f0 <- demande("fichier_change", list(projet = a))
verifier("fichier_change : inchangé, une base gardée", isFALSE(f0$change) && isTRUE(f0$connu) && isTRUE(f0$existe) && isTRUE(f0$base))
# (Bob, ailleurs, ajoute un modèle et change le rapport, et enregistre sous son nom)
b <- demande("ouvrir", list(chemin = partage))$id
demande("ajuster", list(projet = b, type = "PLS", x = uid(b, c("x1", "x2")), y = uid(b, "y"), nom = "M3"))
paragraphe(b, 7, "Conclusion de Bob.")
Sys.sleep(1.1)
demande("enregistrer", list(projet = b, chemin = partage, auteur = list(nom = "Bob", email = "bob@exemple.invalid")))
demande("fermer_projet", list(projet = b))
verifier("le fichier dit qui l'a enregistré, son nom sans son courriel",
         identical(entete_projet(partage)$auteur, "Bob") && !grepl("exemple.invalid", paste(readLines(partage, warn = FALSE), collapse = ""), fixed = TRUE))
paragraphe(a, 3, "Mon paragraphe deux.")
demande("modifier_valeur", list(projet = a, observation = numeros_observations(projet(a), "S1-2018"), variable = uid(a, "x1"), valeur = "9"))
f1 <- demande("fichier_change", list(projet = a))
verifier("fichier_change : changé, par Bob", isTRUE(f1$change) && identical(f1$auteur, "Bob") && f1$taille == file.size(partage))
e <- tryCatch(demande("enregistrer", list(projet = a, chemin = partage)), error = function(e) e)
verifier("Enregistrer demande avant d'écraser, et dit qu'une fusion est possible", inherits(e, "conflit_fichier") && isTRUE(e$fusion) &&
           identical(entete_projet(partage)$auteur, "Bob"))
fu <- demande("fusionner", list(projet = a))
verifier("fusionner avec le projet de la séance : sans conflit, Bob nommé, ce que chacun a apporté",
         isTRUE(fu$auto) && identical(fu$qui, "Bob") && any(grepl("M3", fu$apports$leur)) && any(grepl("modification de x1", fu$apports$mien)) &&
           identical(fu$donnees$mode, "etapes"))
verifier("rien n'a encore changé", length(projet(a)$models) == 2)
fin <- demande("fusionner_finir", list(fusion = fu$fusion, choix = stats::setNames(list(), character(0))))
pa <- projet(a)
la <- strsplit(document(pa, "D1")$texte, "\n", fixed = TRUE)[[1]]
verifier("fusionner_finir : leur modèle, leur conclusion, mon paragraphe et ma cellule ; « fusion avec Bob » à annuler",
         identical(vapply(pa$models, function(d) d$name, ""), c("M1", "M2", "M3")) && identical(la[3], "Mon paragraphe deux.") && identical(la[7], "Conclusion de Bob.") &&
           unname(valeurs(pa, "x1")["S1-2018"]) == 9 && identical(fin$etiquette, "fusion avec Bob") && identical(fin$projet$annulable, "fusion avec Bob") && isTRUE(pa$modifie))
verifier("le projet sait avoir lu leur fichier : fichier_change ne voit plus rien, Enregistrer ne demande rien",
         isFALSE(demande("fichier_change", list(projet = a))$change) && identical(erreur(demande("enregistrer", list(projet = a, chemin = partage))), ""))
verifier("le fichier enregistré a les changements des deux", { q <- lire_projet(partage); length(q$models) == 3 && unname(valeurs(q, "x1")["S1-2018"]) == 9 })
verifier("deux copies gardées : le fichier tel qu'enregistré, la base d'avant la fusion qu'Annuler rendrait",
         length(etat$copies[[a]]) == 2 && identical(unname(tools::md5sum(projet(a)$fichier_lu$copie)), unname(tools::md5sum(partage))) &&
           copie_a %in% etat$copies[[a]])
# (une date seule changée, le contenu le même : rien n'a changé, et Enregistrer ne demande rien)
Sys.setFileTime(partage, Sys.time() + 120)
f2 <- demande("fichier_change", list(projet = a))
verifier("une date seule changée : inchangé, et Enregistrer ne demande pas s'il faut écraser",
         isFALSE(f2$change) && identical(erreur(demande("enregistrer", list(projet = a, chemin = partage))), ""))

cat("Annuler une fusion, les refus, une fusion de trois fichiers\n")
b <- demande("ouvrir", list(chemin = partage))$id
demande("figure_enregistrer", list(projet = b, id = "F1", titre = "Figure de Bob"))
Sys.sleep(1.1)
demande("enregistrer", list(projet = b, chemin = partage, auteur = "Bob"))
demande("fermer_projet", list(projet = b))
fu <- demande("fusionner", list(projet = a))
demande("fusionner_finir", list(fusion = fu$fusion, choix = stats::setNames(list(), character(0))))
verifier("fusionné : la figure de Bob", identical(figure(projet(a), "F1")$titre, "Figure de Bob"))
u <- demande("annuler", list(projet = a))
verifier("annulée : la figure d'avant, et le fichier se voit de nouveau changé (il se fusionnera encore)",
         identical(u$etiquette, "fusion avec Bob") && identical(figure(projet(a), "F1")$titre, "Figure 1") && isTRUE(demande("fichier_change", list(projet = a))$change) &&
           inherits(tryCatch(demande("enregistrer", list(projet = a, chemin = partage)), error = function(e) e), "conflit_fichier"))
# (rétablie, même après que fichier_change a gardé l'empreinte du fichier : la figure de Bob, et le projet le sait lu)
rt <- demande("retablir", list(projet = a))
verifier("rétablie : la figure de Bob revient et le fichier n'est plus vu changé ; Annuler la défait encore",
         identical(rt$etiquette, "fusion avec Bob") && identical(figure(projet(a), "F1")$titre, "Figure de Bob") &&
           isFALSE(demande("fichier_change", list(projet = a))$change) && identical(rt$projet$annulable, "fusion avec Bob") && is.null(rt$projet$retablissable))
u <- demande("annuler", list(projet = a))
verifier("annulée de nouveau : la figure d'avant, le fichier vu changé, la copie d'après la fusion gardée pour Rétablir",
         identical(figure(projet(a), "F1")$titre, "Figure 1") && isTRUE(demande("fichier_change", list(projet = a))$change) &&
           all(file.exists(unlist(lapply(etat$retablissements[[a]], function(e) e$projet$fichier_lu$copie)))))
fu <- demande("fusionner", list(projet = a))
verifier("une fusion inconnue : refusée", grepl("inconnue", refus("fusionner_finir", list(fusion = "fusion0"))))
paragraphe(a, 5, "Changé pendant la fusion.")
verifier("le projet changé depuis la fusion préparée : refusé, à refaire",
         grepl("refaire", refus("fusionner_finir", list(fusion = fu$fusion, choix = stats::setNames(list(), character(0))))))
demande("enregistrer", list(projet = a, chemin = partage, ecraser = TRUE))
b <- demande("ouvrir", list(chemin = partage))$id
demande("figure_enregistrer", list(projet = b, id = "F1", titre = "Figure de Bob, 2"))
Sys.sleep(1.1)
demande("enregistrer", list(projet = b, chemin = partage, auteur = "Bob"))
demande("fermer_projet", list(projet = b))
fu <- demande("fusionner", list(projet = a))
invisible(demande("fichier_change", list(projet = a)))  # (l'interface le demande toutes les 30 secondes : il garde l'empreinte du fichier)
verifier("fichier_change pendant une fusion préparée ne l'empêche pas de finir",
         identical(erreur(demande("fusionner_finir", list(fusion = fu$fusion, choix = stats::setNames(list(), character(0))))), "") &&
           identical(figure(projet(a), "F1")$titre, "Figure de Bob, 2"))
fc <- fusion(base, mien_c, leur_c)
ft <- demande("fusionner", list(base = base, mien = mien_c, leur = leur_c, dossier = etude))
verifier("trois fichiers : les mêmes conflits que fusionner_projets", setequal(vapply(ft$conflits, function(x) x$id, ""), ids_conflits(fc)) && isFALSE(ft$auto))
verifier("un conflit sans choix : refusé, en le nommant", grepl("Modèle M1", refus("fusionner_finir", list(fusion = ft$fusion, choix = list(`document:D1` = "leur")))))
sortie <- file.path(dossier, "fusion_trois.nuee")
ff <- demande("fusionner_finir", list(fusion = ft$fusion, choix = choix("leur"), chemin = sortie, auteur = list(nom = "Alice", email = "alice@exemple.invalid")))
q <- lire_projet(sortie)
verifier("le projet fusionné écrit dans le fichier demandé, les choix faits", identical(ff$chemin, sortie) && definition(q, "M1")$A == 3 && x1(q) == 7)
verifier("… et nommant qui l'a fusionné (son nom, pas son courriel), comme Enregistrer",
         identical(entete_projet(sortie)$auteur, "Alice") && !grepl("exemple.invalid", paste(readLines(sortie, warn = FALSE), collapse = ""), fixed = TRUE))
verifier("une fusion finie ne se finit pas deux fois", grepl("inconnue", refus("fusionner_finir", list(fusion = ft$fusion, choix = choix("leur")))))
v <- demande("ouvrir", list(chemin = partage, nom = "partage — version", lecture_seule = list(source = partage, oid = "a1", date = "2026-11-16")))
verifier("une ancienne version ne se fusionne pas, et n'a pas de copie gardée",
         grepl("lecture seule", refus("fusionner", list(projet = v$id))) && is.null(projet(v$id)$fichier_lu$copie))
t <- demande("ouvrir", list(chemin = source_csv))$id
verifier("un projet jamais lu d'un fichier de projet ni enregistré : pas de base, refusé", grepl("version commune", refus("fusionner", list(projet = t))))
verifier("fichier_change pour lui : rien de connu", isFALSE(demande("fichier_change", list(projet = t))$connu))
copies <- etat$copies[[a]]
demande("fermer_projet", list(projet = a))
verifier("fermé : ses copies effacées", length(copies) > 0 && !any(file.exists(copies)) && is.null(etat$copies[[a]]))
invisible(file.copy(partage, fichier("efface.nuee")))
x <- demande("ouvrir", list(chemin = fichier("efface.nuee")))$id
unlink(fichier("efface.nuee"))
fx <- demande("fichier_change", list(projet = x))
verifier("le fichier effacé : il n'existe plus, rien à fusionner", isFALSE(fx$existe) && isFALSE(fx$change))
for (i in c(v$id, t, x)) demande("fermer_projet", list(projet = i))

# ---------------------------------------------------------------- un projet que personne ne fusionne

cat("Un projet que personne ne fusionne : le même fichier écrit, le même projet lu\n")
id <- demande("ouvrir", list(chemin = mien_c))$id
f1 <- fichier("comme_avant_1.nuee")
f2 <- fichier("comme_avant_2.nuee")
demande("enregistrer", list(projet = id, chemin = f1))
p <- projet(id)
enregistrer_projet(p, f2, dossier = etude)
verifier("Enregistrer écrit le même fichier qu'avant (écrit d'abord dans la séance, puis copié)",
         identical(unname(tools::md5sum(f1)), unname(tools::md5sum(f2))) && is.null(entete_projet(f1)$auteur))
verifier("rien n'est laissé à côté du fichier", !any(grepl("\\.tmp$", list.files(etude, all.files = TRUE))))
f3 <- fichier("comme_avant_3.nuee")
ecrit <- enregistrer_projet(p, f3, dossier = etude, copie = file.path(dossier, "absent", "copie.nuee"))
verifier("une copie qui ne s'écrit pas (le disque de la séance plein) : le fichier s'écrit quand même, comme avant",
         is.null(attr(ecrit, "copie")) && identical(unname(tools::md5sum(f3)), unname(tools::md5sum(f2))))
demande("fermer_projet", list(projet = id))

# ---------------------------------------------------------------- en anglais

cat("Les phrases en anglais\n")
options(nuee.langue = "en")
r <- fusion(base, mien_c, leur_c)
verifier("conflicts and contributions in English",
         grepl("1 component(s) → 2 component(s)", conflit(r, "modele:M1")$mien, fixed = TRUE) && identical(conflit(r, "document:D1")$mien, "paragraph 2 rewritten") &&
           identical(conflit(r, "variable:x1")$titre, "Variable “x1”") && identical(conflit(r, "champ:coordonnees")$titre, "Coordinates") &&
           any(grepl("cell(s) changed", r$apports$mien, fixed = TRUE)))
pm <- fusion(base, mien_c, leur_c, choix("mien"))$projet
verifier("the step a choice adds, named in English", any(grepl("merge: values of x1 from my side", etiquettes(pm), fixed = TRUE)) &&
           "merge with Bob: cells pasted into x1" %in% vapply(pm$etapes, etiquette_etape, ""))
r <- fusion(base, mien_a, leur_a)
verifier("their renamed model, said in English", any(grepl("their M3 renamed M4 (an M3 was made on both sides)", r$apports$leur, fixed = TRUE)))
verifier("steps done in French, said in English", "1 step(s): replacement of “-9999” with empty in comptage" %in% r$apports$mien &&
           "1 step(s): categories of habitat" %in% r$apports$leur)
options(nuee.langue = "fr")

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
