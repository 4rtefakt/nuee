# Vérifie l'import des exports de KoboToolbox et d'ODK Central (moteur/R/kobo.R) sur des exports
# écrits ici comme ces outils les écrivent, d'après un même relevé (six sites, douze observations
# d'oiseaux, quatre individus) :
#   - un classeur de KoboToolbox (valeurs XML, groupes dans les en-têtes) : la feuille des envois
#     au nom coupé à 31 caractères, un groupe répété et un groupe répété dans le premier (reliés par
#     _parent_table_name et _parent_index), un choix multiple avec ses colonnes 0/1 et un sans, un
#     point GPS en texte et en colonnes, une date d'Excel, des dates et heures ISO avec fuseau ;
#   - un CSV de KoboToolbox (points-virgules, en libellés français, avec BOM) ;
#   - une archive .zip d'ODK Central (un CSV par table, PARENT_KEY et KEY, groupes en tirets), et
#     son CSV des envois seul ;
#   - le formulaire XLSForm, en français et en anglais (et une variante à la seule colonne label).
# Chaque transformation contre les valeurs écrites : métadonnées retirées ou gardées, préfixes des
# groupes retirés et noms uniques, choix unique en variable qualitative (libellés, ordre du
# formulaire, langue), choix multiples en colonnes 0/1 (de l'export, sans les doubler, ou des
# réponses), points en longitude et latitude et coordonnées du projet, dates lues comme le temps,
# groupes répétés avec les colonnes jointes de leurs tables parentes ; le projet enregistré puis
# relu, le texte des Méthodes, l'appel du journal, l'anglais, et les refus.
#   Rscript moteur/tests/verifier_kobo.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
erreur <- function(expr) tryCatch({ expr; "" }, error = conditionMessage)
dossier <- tempfile("kobo")
dir.create(dossier)
f <- function(n) file.path(dossier, n)
options(nuee.langue = "fr")

# Les valeurs d'une variable du projet par son nom (le nom de la catégorie pour une qualitative)
valeurs <- function(p, nom) {
  v <- variable_du_projet(p, numeros(p, nom))
  x <- unname(colonne(p, v$uid, numeros_observations(p)))
  if (length(v$categories)) v$categories[x] else x
}
categories <- function(p, nom) variable_du_projet(p, numeros(p, nom))$categories
qualitative <- function(p, nom) length(categories(p, nom)) > 0
variables <- function(p) unname(p$noms_var)
identifiant <- function(p, nom) {
  k <- match(nom, p$observation_id_names)
  vapply(p$observations, function(o) as.character(o$ids[[k]]), "")
}
# (une colonne devenue variable, ou identifiant quand ses valeurs diffèrent toutes)
lire <- function(p, nom) if (nom %in% variables(p)) valeurs(p, nom) else identifiant(p, nom)

# ---------------------------------------------------------------- le relevé

titre <- "Inventaire des oiseaux nicheurs du parc"  # (41 caractères : la feuille de KoboToolbox en garde 31)
feuille <- substr(titre, 1, 31)
S <- data.frame(site = paste0("S", 1:6), habitat = c("foret", "prairie", "foret", "zone_humide", "lande", "prairie"),
                altitude = c(120, 85, 140, 12, 310, 95),
                lat = c(43.6119, 43.6532, 43.5987, 43.5501, 43.7012, NA), lon = c(3.8772, 3.9011, 3.8456, 3.9502, 3.7833, NA),
                alt = c(52.3, 60.1, 48, 5.5, 280.2, NA), prec = c(4.8, 3.9, 5, 12, 4.2, NA),
                remarques_site = c("haie, ronces", NA, "lisière", NA, NA, "fossé"),
                date = as.Date(c("2024-05-03", "2024-05-03", "2024-05-04", "2024-05-04", "2024-05-05", "2024-05-05")),
                debut = c("2024-05-03T07:58:00.000+02:00", "2024-05-03T09:15:00.000+02:00", "2024-05-04T06:40:30.250+02:00",
                          "2024-05-04T08:05:00.000+02:00", "2024-05-05T07:20:00.000+02:00", "2024-05-05T10:02:00.000+02:00"),
                meteo = c("soleil vent", "nuages", "soleil", "pluie vent", NA, "soleil nuages"),
                menaces = c("paturage dechets", NA, "pietinement", "paturage", "dechets", "paturage pietinement dechets"),
                remarques_fin = c(NA, "RAS", NA, NA, "chien", NA), vent = c(5.5, 12.3, 3, 20.1, 8.4, 0),
                uuid = c("6f1c0a52-9b1e-4a1d-8c33-0d3e1b2a4f01", "1e2d3c4b-5a69-4788-9a0b-1c2d3e4f5a02", "a0b1c2d3-e4f5-4a6b-8c7d-9e0f1a2b3c03",
                         "b1c2d3e4-f5a6-4b7c-9d8e-0f1a2b3c4d04", "c2d3e4f5-a6b7-4c8d-8e9f-1a2b3c4d5e05", "d3e4f5a6-b7c8-4d9e-9f0a-2b3c4d5e6f06"),
                stringsAsFactors = FALSE)
O <- data.frame(parent = c(1, 1, 2, 3, 3, 3, 5, 5, 6, 6, 6, 6),
                espece = c("turmer", "parmaj", "erirub", "turmer", "fricoe", "parmaj", "fricoe", "erirub", "turmer", "parmaj", "erirub", "fricoe"),
                effectif = c(2, 1, 1, 3, 2, 1, 4, 1, 1, 2, 1, 1),
                comportement = c("chant", "cri", "chant", "chant", "vol", "chant", "chant", "cri", "vol", "chant", "chant", "perche"), stringsAsFactors = FALSE)
# (« perche » : un code qu'une version précédente du formulaire avait, que celui-ci n'a plus)
I <- data.frame(parent = c(1, 1, 4, 7), sexe = c("male", "femelle", "male", "inconnu"), stringsAsFactors = FALSE)
meteo <- c("soleil", "nuages", "pluie", "vent")
menaces <- c("paturage", "pietinement", "dechets")
especes <- c("turmer", "parmaj", "erirub", "fricoe")
zero_un <- function(reponses, code) ifelse(is.na(reponses), NA, as.numeric(vapply(strsplit(ifelse(is.na(reponses), "", reponses), " "), function(j) code %in% j, TRUE)))
lib_fr <- c(foret = "Forêt", prairie = "Prairie", zone_humide = "Zone humide", lande = "Lande", soleil = "Soleil", nuages = "Nuages", pluie = "Pluie", vent = "Vent",
            paturage = "Pâturage", pietinement = "Piétinement", dechets = "Déchets", turmer = "Merle noir", parmaj = "Mésange charbonnière",
            erirub = "Rougegorge familier", fricoe = "Pinson des arbres", chant = "Chant", cri = "Cri", vol = "En vol", male = "Mâle", femelle = "Femelle", inconnu = "Inconnu")
lib_en <- c(foret = "Forest", prairie = "Grassland", zone_humide = "Wetland", lande = "Heathland", soleil = "Sun", nuages = "Clouds", pluie = "Rain", vent = "Wind",
            paturage = "Grazing", pietinement = "Trampling", dechets = "Litter", turmer = "Common blackbird", parmaj = "Great tit",
            erirub = "European robin", fricoe = "Common chaffinch", chant = "Song", cri = "Call", vol = "Flying", male = "Male", femelle = "Female", inconnu = "Unknown")
df <- function(...) data.frame(..., check.names = FALSE, stringsAsFactors = FALSE)
debuts <- sprintf("2024-05-0%dT0%d:1%d:0%d.%03d+02:00", c(3, 3, 4, 4, 5, 5), 6:1, 1:6, 0:5, c(123, 4, 900, 0, 77, 500))
soumis <- sprintf("2024-05-0%dT1%d:45:1%d", c(3, 3, 4, 4, 5, 5), 1:6, 0:5)

# ---------------------------------------------------------------- le formulaire XLSForm

survey <- df(
  type = c("start", "end", "today", "deviceid", "username", "begin_group", "text", "select_one habitat", "integer", "geopoint", "text", "end_group",
           "date", "dateTime", "decimal", "select_multiple meteo", "select_multiple menaces", "begin_repeat", "select_one espece", "integer",
           "select_one comportement", "begin_repeat", "select_one sexe", "end_repeat", "end_repeat", "begin group", "text", "end group", "note"),
  name = c("start", "end", "today", "deviceid", "username", "group_site", "site", "habitat", "altitude", "gps", "remarques", NA,
           "date_releve", "debut_comptage", "vitesse_vent", "meteo", "menaces", "observation", "espece", "effectif",
           "comportement", "individu", "sexe", NA, NA, "group_fin", "remarques", NA, "merci"),
  `label::Français (fr)` = c(NA, NA, NA, NA, NA, "Site", "Nom du site", "Habitat", "Altitude (m)", "Position", "Remarques sur le site", NA,
                             "Date du relevé", "Début du comptage", "Vent (km/h)", "Météo", "Menaces", "Observation", "Espèce", "Effectif",
                             "Comportement", "Individu", "Sexe", NA, NA, "Fin", "Remarques générales", NA, "Merci !"),
  `label::English (en)` = c(NA, NA, NA, NA, NA, "Site", "Site name", "Habitat", "Elevation (m)", "Location", "Site notes", NA,
                            "Survey date", "Count start", "Wind (km/h)", "Weather", "Threats", "Observation", "Species", "Count",
                            "Behaviour", "Individual", "Sex", NA, NA, "End", "General notes", NA, "Thank you!"),
  required = c(rep(NA, 6), "yes", rep(NA, 22)))
listes <- list(habitat = c("foret", "prairie", "zone_humide", "lande"), meteo = meteo, menaces = menaces, espece = especes,
               comportement = c("chant", "cri", "vol"), sexe = c("male", "femelle", "inconnu"))
choices <- df(list_name = rep(names(listes), lengths(listes)), name = unlist(listes, use.names = FALSE))
choices[["label::Français (fr)"]] <- unname(lib_fr[choices$name])
choices[["label::English (en)"]] <- unname(lib_en[choices$name])
settings <- df(form_title = titre, form_id = "inventaire_oiseaux", version = "2024050101", default_language = "Français (fr)")
openxlsx::write.xlsx(list(survey = survey, choices = choices, settings = settings), f("inventaire_oiseaux_formulaire.xlsx"))
# (le même, à la seule colonne label, en français)
s1 <- survey[c("type", "name", "label::Français (fr)")]
names(s1)[3] <- "label"
c1 <- choices[c("list_name", "name", "label::Français (fr)")]
names(c1)[3] <- "label"
openxlsx::write.xlsx(list(survey = s1, choices = c1), f("formulaire_label.xlsx"))
# un autre formulaire, sans rapport avec ce relevé
openxlsx::write.xlsx(list(survey = df(type = c("integer", "text"), name = c("x", "y"), label = c("X", "Y")),
                          choices = df(list_name = "l", name = "a", label = "A")), f("autre_formulaire.xlsx"))

# ---------------------------------------------------------------- le classeur de KoboToolbox

meta_kobo <- function(i) df(`_id` = 1000 + i, `_uuid` = S$uuid[i], `_submission_time` = soumis[i], `_validation_status` = "validation_status_approved",
                            `_notes` = NA_character_, `_status` = "submitted_via_web", `_submitted_by` = "equipe_terrain", `__version__` = "vK7aX3mQp",
                            `_tags` = NA_character_)
envois <- df(start = debuts, end = sub("T0", "T1", debuts), today = S$date, deviceid = "collect:AbC123", username = c("anne", "anne", "paul", "paul", "anne", "paul"),
             `group_site/site` = S$site, `group_site/habitat` = S$habitat, `group_site/altitude` = S$altitude,
             `group_site/gps` = ifelse(is.na(S$lat), NA, sprintf("%s %s %s %s", S$lat, S$lon, S$alt, S$prec)),
             `_group_site/gps_latitude` = S$lat, `_group_site/gps_longitude` = S$lon, `_group_site/gps_altitude` = S$alt, `_group_site/gps_precision` = S$prec,
             `group_site/remarques` = S$remarques_site, date_releve = S$date, debut_comptage = S$debut, vitesse_vent = S$vent,
             meteo = S$meteo, `meteo/soleil` = zero_un(S$meteo, "soleil"), `meteo/nuages` = zero_un(S$meteo, "nuages"),
             `meteo/pluie` = zero_un(S$meteo, "pluie"), `meteo/vent` = zero_un(S$meteo, "vent"),
             menaces = S$menaces, `group_fin/remarques` = S$remarques_fin)
envois <- cbind(envois, do.call(rbind, lapply(1:6, meta_kobo)), df(`_index` = 1:6))
meta_repetition <- function(parents) {
  m <- do.call(rbind, lapply(parents, meta_kobo))
  names(m) <- paste0("_submission_", names(m))
  m
}
obs_kobo <- cbind(df(`observation/espece` = O$espece, `observation/effectif` = O$effectif, `observation/comportement` = O$comportement,
                     `_index` = seq_len(nrow(O)), `_parent_table_name` = titre, `_parent_index` = O$parent), meta_repetition(O$parent))
ind_kobo <- cbind(df(`observation/individu/sexe` = I$sexe, `_index` = seq_len(nrow(I)), `_parent_table_name` = "observation",
                     `_parent_index` = I$parent), meta_repetition(O$parent[I$parent]))
kobo_xlsx <- f("inventaire_kobo.xlsx")
openxlsx::write.xlsx(stats::setNames(list(envois, obs_kobo, ind_kobo), c(feuille, "observation", "individu")), kobo_xlsx)

# ---------------------------------------------------------------- le CSV de KoboToolbox (libellés)

etiquettes <- function(codes) ifelse(is.na(codes), NA, vapply(strsplit(ifelse(is.na(codes), "", codes), " "), function(j) paste(lib_fr[j], collapse = " "), ""))
envois_csv <- df(start = debuts, end = sub("T0", "T1", debuts), today = format(S$date), deviceid = "collect:AbC123", username = "anne",
                 `Nom du site` = S$site, Habitat = unname(lib_fr[S$habitat]), `Altitude (m)` = S$altitude,
                 Position = ifelse(is.na(S$lat), NA, sprintf("%s %s %s %s", S$lat, S$lon, S$alt, S$prec)),
                 `_Position_latitude` = S$lat, `_Position_longitude` = S$lon, `_Position_altitude` = S$alt, `_Position_precision` = S$prec,
                 `Remarques sur le site` = S$remarques_site, `Date du relevé` = format(S$date), `Début du comptage` = S$debut, `Vent (km/h)` = S$vent,
                 `Météo` = etiquettes(S$meteo), `Météo/Soleil` = zero_un(S$meteo, "soleil"), `Météo/Nuages` = zero_un(S$meteo, "nuages"),
                 `Météo/Pluie` = zero_un(S$meteo, "pluie"), `Météo/Vent` = zero_un(S$meteo, "vent"),
                 Menaces = etiquettes(S$menaces), `Remarques générales` = S$remarques_fin)
envois_csv <- cbind(envois_csv, do.call(rbind, lapply(1:6, meta_kobo)), df(`_index` = 1:6))
kobo_csv <- f("inventaire_kobo.csv")
utils::write.table(envois_csv, kobo_csv, sep = ";", row.names = FALSE, na = "", qmethod = "double", fileEncoding = "UTF-8")
writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), readBin(kobo_csv, "raw", file.size(kobo_csv))), kobo_csv)

# ---------------------------------------------------------------- l'archive d'ODK Central

cle <- paste0("uuid:", S$uuid)
envois_odk <- df(SubmissionDate = sprintf("%s.000Z", soumis), start = debuts, end = sub("T0", "T1", debuts), today = format(S$date), deviceid = "collect:AbC123",
                 username = c("anne", "anne", "paul", "paul", "anne", "paul"), `group_site-site` = S$site, `group_site-habitat` = S$habitat,
                 `group_site-altitude` = S$altitude, `group_site-gps-Latitude` = S$lat, `group_site-gps-Longitude` = S$lon,
                 `group_site-gps-Altitude` = S$alt, `group_site-gps-Accuracy` = S$prec, `group_site-remarques` = S$remarques_site,
                 date_releve = format(S$date), debut_comptage = S$debut, vitesse_vent = S$vent, meteo = S$meteo, menaces = S$menaces,
                 `group_fin-remarques` = S$remarques_fin, `meta-instanceID` = cle, KEY = cle, SubmitterID = c(5, 5, 7, 7, 5, 7),
                 SubmitterName = c("Anne", "Anne", "Paul", "Paul", "Anne", "Paul"), AttachmentsPresent = 0, AttachmentsExpected = 0,
                 Status = NA, ReviewState = c(NA, "approved", NA, "hasIssues", NA, NA), DeviceID = "collect:AbC123", Edits = 0, FormVersion = "2024050101")
rang <- ave(O$parent, O$parent, FUN = seq_along)
cle_obs <- sprintf("%s/observation[%d]", cle[O$parent], rang)
obs_odk <- df(espece = O$espece, effectif = O$effectif, comportement = O$comportement, PARENT_KEY = cle[O$parent], KEY = cle_obs)
ind_odk <- df(sexe = I$sexe, PARENT_KEY = cle_obs[I$parent], KEY = sprintf("%s/individu[%d]", cle_obs[I$parent], ave(I$parent, I$parent, FUN = seq_along)))
odk <- file.path(dossier, "odk")
dir.create(odk)
utils::write.csv(envois_odk, file.path(odk, "inventaire_oiseaux.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
utils::write.csv(obs_odk, file.path(odk, "inventaire_oiseaux-observation.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
utils::write.csv(ind_odk, file.path(odk, "inventaire_oiseaux-individu.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
dir.create(file.path(odk, "media"))
writeLines("pas un tableau", file.path(odk, "media", "photo.csv"))
odk_zip <- f("inventaire_oiseaux.zip")
zip::zip(odk_zip, files = c("inventaire_oiseaux.csv", "inventaire_oiseaux-observation.csv", "inventaire_oiseaux-individu.csv", "media/photo.csv"), root = odk)
odk_csv <- file.path(odk, "inventaire_oiseaux.csv")

# des fichiers qui n'en sont pas
ordinaire <- f("ordinaire.csv")
writeLines(c("site;_id;x", "a;1;2", "b;2;3"), ordinaire)
vide_zip <- f("vide.zip")
writeLines("rien", f("lisez-moi.txt"))
zip::zip(vide_zip, files = "lisez-moi.txt", root = dossier)

cat("Reconnaître un export (les seuls en-têtes)\n")
verifier("le classeur de KoboToolbox", identical(detecter_collecte(kobo_xlsx), list(outil = "kobo", format = "xlsx")))
verifier("le CSV de KoboToolbox (points-virgules, BOM)", identical(detecter_collecte(kobo_csv)$outil, "kobo"))
verifier("l'archive d'ODK Central, et son CSV des envois seul", identical(detecter_collecte(odk_zip), list(outil = "odk", format = "zip")) &&
         identical(detecter_collecte(odk_csv)$outil, "odk"))
verifier("pas un tableau ordinaire (une seule colonne de KoboToolbox), ni le formulaire, ni une archive sans CSV",
         is.null(detecter_collecte(ordinaire)) && is.null(detecter_collecte(f("inventaire_oiseaux_formulaire.xlsx"))) && is.null(detecter_collecte(vide_zip)))

cat("Le classeur de KoboToolbox, sans formulaire\n")
x <- lire_collecte(kobo_xlsx)
verifier("trois tables : les envois, la répétition, la répétition dans la répétition",
         identical(names(x$tables), c(feuille, "observation", "individu")) && x$principale == feuille)
verifier("les tables parentes, par _parent_table_name (la feuille des envois coupée à 31 caractères)",
         identical(x$tables$observation$parent, feuille) && identical(x$tables$individu$parent, "observation") && is.null(x$tables[[feuille]]$parent))
p <- importer_collecte(kobo_xlsx)
v <- variables(p)
verifier("les variables : sans métadonnées, sans préfixes, les remarques des deux groupes distinguées",
         identical(v, c("habitat", "altitude", "gps_longitude", "gps_latitude", "gps_altitude", "gps_precision", "group_site_remarques", "date_releve",
                        "vitesse_vent", "meteo_soleil", "meteo_nuages", "meteo_pluie", "meteo_vent", "menaces", "group_fin_remarques")))
verifier("le site et l'heure du comptage, une valeur par envoi, en identifiants ; les observations numérotées",
         identical(p$observation_id_names, c("Observation", "site", "debut_comptage")) && identical(identifiant(p, "site"), S$site) &&
         identical(unname(p$noms_obs), as.character(1:6)))
verifier("le point GPS : longitude et latitude des colonnes de l'export, altitude et précision, un point manquant",
         identical(valeurs(p, "gps_longitude"), S$lon) && identical(valeurs(p, "gps_latitude"), S$lat) && identical(valeurs(p, "gps_altitude"), S$alt) &&
         identical(valeurs(p, "gps_precision"), S$prec))
co <- coordonnees_projet(p)
verifier("les coordonnées du projet : la longitude et la latitude, en WGS 84", identical(co[c("nom_x", "nom_y")], list(nom_x = "gps_longitude", nom_y = "gps_latitude")) &&
         identical(as.numeric(co$systeme), 4326))
verifier("le choix multiple : les colonnes 0/1 de l'export, sans la colonne des réponses ; un envoi sans réponse reste vide",
         all(vapply(meteo, function(c) identical(valeurs(p, paste0("meteo_", c)), zero_un(S$meteo, c)), TRUE)) && !"meteo" %in% v &&
         sum(startsWith(v, "meteo")) == 4 && all(is.na(vapply(meteo, function(c) valeurs(p, paste0("meteo_", c))[5], 0))))
verifier("sans formulaire, le choix multiple sans colonnes 0/1 reste du texte", qualitative(p, "menaces") && identical(valeurs(p, "menaces"), S$menaces))
verifier("le choix unique sans formulaire : ses codes, en catégories dans l'ordre alphabétique",
         identical(categories(p, "habitat"), sort(unique(S$habitat))) && identical(valeurs(p, "habitat"), S$habitat))
verifier("la date d'Excel en date ; les dates et heures ISO sans fraction ni fuseau, les secondes gardées (une valeur en a)",
         identical(categories(p, "date_releve"), c("2024-05-03", "2024-05-04", "2024-05-05")) &&
         identical(identifiant(p, "debut_comptage"), c("2024-05-03 07:58:00", "2024-05-03 09:15:00", "2024-05-04 06:40:30", "2024-05-04 08:05:00",
                                                       "2024-05-05 07:20:00", "2024-05-05 10:02:00")))
t <- temps_en_nombres(identifiant(p, "debut_comptage"))
verifier("… que Nuée lit comme le temps (années décimales)", isTRUE(attr(t, "dates")) && is.null(attr(t, "refus")) &&
         isTRUE(all.equal(t[1], 2024 + (123 + 7 / 24 + 58 / 1440) / 366)))
verifier("les nombres en nombres", identical(valeurs(p, "altitude"), S$altitude) && identical(valeurs(p, "vitesse_vent"), S$vent))
a <- attr(p, "collecte")
verifier("le résumé dit les métadonnées retirées, les noms changés, le point, les coordonnées, les dates, l'absence de formulaire",
         any(grepl("^Métadonnées retirées \\(15\\) : start, end, today, deviceid, username, _id, _uuid", a$phrases)) &&
         any(grepl("group_site/altitude → altitude", a$phrases, fixed = TRUE)) && any(grepl("Point GPS « gps » en gps_longitude, gps_latitude, gps_altitude, gps_precision", a$phrases, fixed = TRUE)) &&
         any(startsWith(a$phrases, "Coordonnées du projet : gps_longitude et gps_latitude")) &&
         any(grepl("Choix multiples « meteo » en 4 colonnes 0/1 (celles de l'export", a$phrases, fixed = TRUE)) &&
         any(startsWith(a$phrases, "Dates et heures lues comme le temps")) && any(startsWith(a$phrases, "Sans formulaire")))

cat("Les métadonnées gardées\n")
g <- importer_collecte(kobo_xlsx, garder = c("envoi", "identifiant", "auteur", "releve", "validation"))
verifier("gardées, sans le tiret bas : la date d'envoi en date, l'identifiant, l'auteur, le début, la fin et le jour du relevé, le statut",
         all(c("submission_time", "uuid", "submitted_by", "username", "start", "end", "today", "validation_status") %in% c(variables(g), g$observation_id_names)) &&
         identical(identifiant(g, "submission_time"), sub("T", " ", soumis)) && identical(identifiant(g, "uuid"), S$uuid) &&
         identical(valeurs(g, "today"), format(S$date)) && identical(identifiant(g, "start"), sub("\\.[0-9]+\\+02:00$", "", sub("T", " ", debuts))))
verifier("toujours retirées : l'_id, l'appareil, la version, les notes, l'_index",
         !any(c("id", "_id", "deviceid", "version", "__version__", "notes", "_notes", "index", "_index", "status", "tags") %in% c(variables(g), g$observation_id_names)))
verifier("un rôle inconnu est refusé", startsWith(erreur(importer_collecte(kobo_xlsx, garder = "appareil")), "Métadonnée inconnue : appareil"))

cat("Le classeur de KoboToolbox, avec son formulaire\n")
form <- f("inventaire_oiseaux_formulaire.xlsx")
fo <- lire_formulaire(form)
verifier("le formulaire : titre, langues, questions, groupes et répétitions, listes dans leur ordre",
         identical(fo$titre, titre) && identical(fo$langues, c("Français (fr)", "English (en)")) && length(fo$questions) == 21 &&
         identical(fo$questions[[which(vapply(fo$questions, function(q) q$nom == "sexe", TRUE))]]$groupes, c("observation", "individu")) &&
         identical(fo$questions[[which(vapply(fo$questions, function(q) q$nom == "sexe", TRUE))]]$repetition, "individu") &&
         identical(fo$choix$habitat$noms, listes$habitat))
pf <- importer_collecte(kobo_xlsx, formulaire = form)
vf <- variables(pf)
verifier("la langue de l'interface (français), les catégories dans l'ordre du formulaire, en libellés",
         identical(categories(pf, "habitat"), c("Forêt", "Prairie", "Zone humide", "Lande")) && identical(valeurs(pf, "habitat"), unname(lib_fr[S$habitat])))
verifier("le choix multiple sans colonnes 0/1 : une colonne par choix d'après les réponses, dans l'ordre du formulaire ; sans réponse, vide",
         identical(vf[grepl("^menaces_", vf)], paste0("menaces_", menaces)) &&
         all(vapply(menaces, function(c) identical(valeurs(pf, paste0("menaces_", c)), zero_un(S$menaces, c)), TRUE)) && !"menaces" %in% vf)
verifier("le choix multiple avec ses colonnes 0/1 : celles de l'export, une fois chacune",
         identical(vf[grepl("^meteo", vf)], paste0("meteo_", meteo)) && identical(valeurs(pf, "meteo_vent"), zero_un(S$meteo, "vent")))
verifier("les questions de métadonnées par leur type (start, end, today, deviceid, username), retirées",
         !any(c("start", "end", "today", "deviceid", "username") %in% vf) && length(vf) == length(v) + 2)
pe <- importer_collecte(kobo_xlsx, formulaire = form, langue = "English (en)")
verifier("en anglais, demandé", identical(categories(pe, "habitat"), c("Forest", "Grassland", "Wetland", "Heathland")))
options(nuee.langue = "en")
pa <- importer_collecte(kobo_xlsx, formulaire = form)
options(nuee.langue = "fr")
verifier("… ou la langue de l'interface", identical(categories(pa, "habitat"), c("Forest", "Grassland", "Wetland", "Heathland")))
pl <- importer_collecte(kobo_xlsx, formulaire = f("formulaire_label.xlsx"))
verifier("un formulaire à la seule colonne label", identical(lire_formulaire(f("formulaire_label.xlsx"))$langues, "") &&
         identical(categories(pl, "habitat"), c("Forêt", "Prairie", "Zone humide", "Lande")))
verifier("une langue que le formulaire n'a pas est refusée", startsWith(erreur(importer_collecte(kobo_xlsx, formulaire = form, langue = "Deutsch (de)")),
                                                                         "Le formulaire n'a pas la langue « Deutsch (de) »"))
af <- attr(pf, "collecte")
verifier("le résumé : les catégories et leur langue, les colonnes 0/1 d'après les réponses",
         any(startsWith(af$phrases, "Choix unique en variable qualitative, catégories dans l'ordre du formulaire, libellés en Français (fr) : habitat (Forêt, Prairie, Zone humide, Lande)")) &&
         any(grepl("Choix multiples « menaces » en 3 colonnes 0/1 (d'après les réponses", af$phrases, fixed = TRUE)) && !any(startsWith(af$phrases, "Sans formulaire")))

cat("Les groupes répétés : une ligne par observation, les colonnes des envois jointes\n")
po <- importer_collecte(kobo_xlsx, table = "observation", formulaire = form)
vo <- variables(po)
verifier("une ligne par observation, ses colonnes, puis celles de l'envoi", length(po$observations) == 12 &&
         identical(vo[1:3], c("espece", "effectif", "comportement")) && all(c("habitat", "altitude", "gps_longitude", "gps_latitude", "meteo_soleil", "menaces_dechets") %in% vo))
verifier("les valeurs jointes : celles de l'envoi de chaque ligne (par _parent_index)",
         identical(valeurs(po, "habitat"), unname(lib_fr[S$habitat[O$parent]])) && identical(valeurs(po, "altitude"), S$altitude[O$parent]) &&
         identical(lire(po, "site"), S$site[O$parent]))
verifier("l'espèce en libellés, dans l'ordre du formulaire", identical(categories(po, "espece"), unname(lib_fr[especes])) &&
         identical(valeurs(po, "espece"), unname(lib_fr[O$espece])))
verifier("un code que le formulaire n'a pas reste tel qu'écrit, après ses catégories", identical(categories(po, "comportement"), c("Chant", "Cri", "En vol", "perche")) &&
         identical(valeurs(po, "comportement")[12], "perche"))
verifier("les coordonnées : le point GPS de l'envoi, joint", identical(coordonnees_projet(po)[c("nom_x", "nom_y")], list(nom_x = "gps_longitude", nom_y = "gps_latitude")) &&
         identical(valeurs(po, "gps_latitude"), S$lat[O$parent]))
ao <- attr(po, "collecte")
verifier("le résumé : la répétition, ses envois, les colonnes jointes, les coordonnées de la table des envois",
         startsWith(ao$phrases[1], "Export KoboToolbox (classeur Excel, 3 tables) : le groupe répété « observation », 12 ligne(s), de 5 envoi(s).") &&
         any(startsWith(ao$phrases, sprintf("Jointes à chaque ligne, de la table « %s » (telles que son import les fait) : site, habitat, altitude", feuille))) &&
         any(grepl(sprintf("de la table « %s »", feuille), ao$phrases[startsWith(ao$phrases, "Coordonnées du projet")], fixed = TRUE)))
pc <- importer_collecte(kobo_xlsx, table = "observation", formulaire = form, parents = stats::setNames(list("habitat"), feuille))
verifier("les colonnes jointes choisies : l'habitat seul, sans coordonnées", identical(variables(pc), c("espece", "effectif", "comportement", "habitat")) &&
         is.null(coordonnees_projet(pc)))
pn <- importer_collecte(kobo_xlsx, table = "observation", parents = stats::setNames(list(character(0)), feuille))
verifier("aucune colonne jointe", identical(variables(pn), c("espece", "effectif", "comportement")))
pi <- importer_collecte(kobo_xlsx, table = "individu", formulaire = form, parents = stats::setNames(list("espece", c("habitat", "site")), c("observation", feuille)))
verifier("une répétition dans une répétition : les colonnes de l'observation et de l'envoi, jointes par deux liens",
         identical(variables(pi), c("sexe", "espece", "habitat", "site")) && identical(valeurs(pi, "sexe"), unname(lib_fr[I$sexe])) &&
         identical(valeurs(pi, "espece"), unname(lib_fr[O$espece[I$parent]])) && identical(valeurs(pi, "habitat"), unname(lib_fr[S$habitat[O$parent[I$parent]]])) &&
         identical(lire(pi, "site"), S$site[O$parent[I$parent]]))
pg <- importer_collecte(kobo_xlsx, table = "observation", garder = c("envoi", "identifiant"), formulaire = form)
verifier("les métadonnées gardées de l'envoi, jointes à chaque observation (pas les copies _submission_ de la feuille)",
         identical(valeurs(pg, "uuid"), S$uuid[O$parent]) && identical(valeurs(pg, "submission_time"), sub("T", " ", soumis)[O$parent]) &&
         !any(grepl("submission__", c(variables(pg), pg$observation_id_names))))
verifier("des refus : une table inconnue, une table qui n'est pas parente, une colonne qu'elle n'a pas",
         startsWith(erreur(importer_collecte(kobo_xlsx, table = "especes")), "Table inconnue dans l'export : « especes »") &&
         startsWith(erreur(importer_collecte(kobo_xlsx, table = "observation", parents = list(individu = "sexe"))), "« individu » n'est pas une table parente de « observation »") &&
         startsWith(erreur(importer_collecte(kobo_xlsx, table = "observation", parents = stats::setNames(list("couleur"), feuille))), sprintf("La table « %s » n'a pas de colonne « couleur »", feuille)))

cat("Le CSV de KoboToolbox, en libellés\n")
pk <- importer_collecte(kobo_csv)
vk <- variables(pk)
verifier("les noms de l'export (des libellés), « Vent (km/h) » entier, le point et les colonnes 0/1 nommés d'après eux",
         all(c("Habitat", "Altitude (m)", "Position_longitude", "Position_latitude", "Vent (km/h)", "Météo_Soleil", "Météo_Vent", "Menaces") %in% vk) &&
         !"Météo" %in% vk && "Nom du site" %in% pk$observation_id_names)
verifier("les libellés de l'export en catégories (ordre alphabétique), les nombres et les colonnes 0/1",
         identical(categories(pk, "Habitat"), sort(unique(unname(lib_fr[S$habitat])))) && identical(valeurs(pk, "Altitude (m)"), S$altitude) &&
         identical(valeurs(pk, "Météo_Pluie"), zero_un(S$meteo, "pluie")) && identical(valeurs(pk, "Position_latitude"), S$lat))
verifier("les dates du CSV : texte ISO, fuseau retiré", identical(categories(pk, "Date du relevé"), c("2024-05-03", "2024-05-04", "2024-05-05")) &&
         identical(identifiant(pk, "Début du comptage")[3], "2024-05-04 06:40:30"))
pkf <- importer_collecte(kobo_csv, formulaire = form, langue = "English (en)")
vkf <- variables(pkf)
verifier("avec le formulaire : les noms des questions, les libellés français de l'export en catégories anglaises, dans l'ordre du formulaire",
         all(c("habitat", "altitude", "gps_longitude", "vitesse_vent", "meteo_soleil", "meteo_vent") %in% vkf) &&
         identical(categories(pkf, "habitat"), c("Forest", "Grassland", "Wetland", "Heathland")) && identical(valeurs(pkf, "habitat"), unname(lib_en[S$habitat])))
verifier("les colonnes 0/1 aux libellés de l'export : nommées par le code du choix", identical(vkf[grepl("^meteo", vkf)], paste0("meteo_", meteo)) &&
         identical(valeurs(pkf, "meteo_nuages"), zero_un(S$meteo, "nuages")))
verifier("un choix multiple en libellés sans colonnes 0/1 : une colonne par choix d'après les libellés du formulaire, dans son ordre",
         identical(vkf[grepl("^menaces", vkf)], paste0("menaces_", menaces)) && !"menaces" %in% vkf &&
         all(vapply(menaces, function(c) identical(valeurs(pkf, paste0("menaces_", c)), zero_un(S$menaces, c)), TRUE)) &&
         any(grepl("Choix multiples « menaces » en 3 colonnes 0/1 (d'après les réponses", attr(pkf, "collecte")$phrases, fixed = TRUE)))
autre <- f("libelles_inconnus.csv")
utils::write.table(df(start = debuts[1:2], Menaces = c("Pâturage Incendie", "Déchets"), `_id` = 1:2, `_uuid` = S$uuid[1:2], `_index` = 1:2), autre,
                   sep = ";", row.names = FALSE, na = "", fileEncoding = "UTF-8")
pin <- importer_collecte(autre, formulaire = form)
verifier("… et reste du texte quand un libellé n'est pas du formulaire, ce que le résumé dit",
         identical(identifiant(pin, "menaces"), c("Pâturage Incendie", "Déchets")) && !any(startsWith(variables(pin), "menaces_")) &&
         any(startsWith(attr(pin, "collecte")$phrases, "Choix multiples gardés en texte")))

cat("L'archive d'ODK Central\n")
xo <- lire_collecte(odk_zip)
verifier("trois tables (pas le CSV des pièces jointes), les répétitions nommées sans le formulaire, leurs parents par PARENT_KEY",
         identical(names(xo$tables), c("inventaire_oiseaux", "observation", "individu")) && identical(xo$tables$observation$parent, "inventaire_oiseaux") &&
         identical(xo$tables$individu$parent, "observation"))
q <- importer_collecte(odk_zip)
vq <- variables(q)
verifier("sans formulaire : les groupes en tirets retirés, les remarques distinguées, le point en colonnes, les métadonnées retirées",
         identical(vq, c("habitat", "altitude", "gps_longitude", "gps_latitude", "gps_altitude", "gps_precision", "group_site_remarques", "date_releve",
                         "vitesse_vent", "meteo", "menaces", "group_fin_remarques")) && identical(q$observation_id_names, c("Observation", "site", "debut_comptage")))
verifier("la virgule dans un texte entre guillemets", identical(valeurs(q, "group_site_remarques")[1], "haie, ronces"))
verifier("les métadonnées d'ODK retirées, listées", identical(attr(q, "collecte")$phrases[2],
         "Métadonnées retirées (17) : SubmissionDate, start, end, today, deviceid, username, meta-instanceID, KEY, SubmitterID, SubmitterName, AttachmentsPresent, AttachmentsExpected… (+5)."))
verifier("les coordonnées, d'après gps-Latitude et gps-Longitude", identical(coordonnees_projet(q)$nom_x, "gps_longitude") && identical(valeurs(q, "gps_longitude"), S$lon))
qf <- importer_collecte(odk_zip, formulaire = form)
verifier("avec le formulaire : les choix multiples en colonnes 0/1 d'après les réponses, l'habitat en libellés",
         identical(variables(qf)[grepl("^meteo", variables(qf))], paste0("meteo_", meteo)) && identical(valeurs(qf, "meteo_soleil"), zero_un(S$meteo, "soleil")) &&
         identical(categories(qf, "habitat"), c("Forêt", "Prairie", "Zone humide", "Lande")))
qg <- importer_collecte(odk_zip, garder = c("auteur", "validation", "envoi", "identifiant"))
verifier("gardés : l'auteur (nom et numéro, un identifiant : du texte), le statut de revue, la date d'envoi (heure UTC), l'identifiant",
         identical(valeurs(qg, "SubmitterName"), envois_odk$SubmitterName) && identical(valeurs(qg, "SubmitterID"), as.character(envois_odk$SubmitterID)) &&
         identical(valeurs(qg, "ReviewState"), envois_odk$ReviewState) && identical(identifiant(qg, "SubmissionDate"), sub("T", " ", soumis)) &&
         identical(identifiant(qg, "instanceID"), cle) && "username" %in% variables(qg))
qo <- importer_collecte(odk_zip, table = "observation", formulaire = form)
verifier("la répétition : une ligne par observation, les colonnes de l'envoi jointes par PARENT_KEY",
         length(qo$observations) == 12 && identical(valeurs(qo, "habitat"), unname(lib_fr[S$habitat[O$parent]])) &&
         identical(valeurs(qo, "effectif"), O$effectif) && identical(coordonnees_projet(qo)$nom_y, "gps_latitude"))
qi <- importer_collecte(odk_zip, table = "individu", formulaire = form, parents = list(observation = c("espece", "effectif"), inventaire_oiseaux = "altitude"))
verifier("la répétition dans la répétition, par les clés KEY de l'observation", identical(variables(qi), c("sexe", "espece", "effectif", "altitude")) &&
         identical(valeurs(qi, "altitude"), S$altitude[O$parent[I$parent]]) && identical(valeurs(qi, "effectif"), O$effectif[I$parent]))
pb <- projet_brut_collecte(odk_zip, "observation")
verifier("tel quel : un CSV de l'archive comme un tableau ouvert, ses colonnes telles qu'écrites (la première nomme les lignes)",
         identical(unname(pb$noms_obs), O$espece) && identical(variables(pb), c("effectif", "comportement", "PARENT_KEY")) &&
         identical(pb$observation_id_names, c("Observation", "KEY")) &&
         identical(projet_brut_collecte(odk_zip)[c("variables", "observations", "donnees")], importer_tableau(odk_csv)[c("variables", "observations", "donnees")]))
qc <- importer_collecte(odk_csv, formulaire = form)
verifier("le CSV des envois seul : une table, comme dans l'archive", length(lire_collecte(odk_csv)$tables) == 1 &&
         identical(variables(qc), variables(qf)) && identical(valeurs(qc, "meteo_pluie"), valeurs(qf, "meteo_pluie")))

cat("L'aperçu de l'import (le dialogue)\n")
ap <- apercu_collecte(kobo_xlsx, table = "observation", garder = "envoi", formulaire = form)
verifier("les tables, leurs lignes et leurs parents ; la table choisie",
         identical(vapply(ap$tables, `[[`, "", "nom"), c(feuille, "observation", "individu")) && identical(vapply(ap$tables, `[[`, 0L, "lignes"), c(6L, 12L, 4L)) &&
         ap$table == "observation" && !ap$principale && ap$lignes == 12)
verifier("les métadonnées qu'on peut garder, avec leurs colonnes (celles de l'envoi, pas leurs copies)",
         identical(vapply(ap$metadonnees, `[[`, "", "role"), c("envoi", "auteur", "identifiant", "releve", "validation")) &&
         identical(as.character(ap$metadonnees[[1]]$colonnes), "_submission_time") && isTRUE(ap$metadonnees[[1]]$garde) && !isTRUE(ap$metadonnees[[2]]$garde))
verifier("les colonnes de la table des envois qu'on peut joindre, toutes choisies par défaut",
         length(ap$parents) == 1 && ap$parents[[1]]$table == feuille && "submission_time" %in% ap$parents[[1]]$colonnes &&
         identical(as.character(ap$parents[[1]]$choisies), as.character(ap$parents[[1]]$colonnes)))
verifier("le formulaire et ses langues", identical(ap$formulaire$langue, "Français (fr)") && identical(as.character(ap$formulaire$langues), c("Français (fr)", "English (en)")))
verifier("un aperçu ne change rien : le même import, demandé deux fois", identical(apercu_collecte(kobo_xlsx)$phrases, apercu_collecte(kobo_xlsx)$phrases))

cat("Le projet : enregistré, relu, son texte des Méthodes, l'appel du journal\n")
fichier <- f("observations.nuee")
enregistrer_projet(po, fichier, "essai")
pr <- lire_projet(fichier)
verifier("relu : les données, les coordonnées et l'origine des données ; pas les réglages de la séance",
         identical(pr$donnees, po$donnees) && identical(coordonnees_projet(pr)[c("x", "y", "systeme")], coordonnees_projet(po)[c("x", "y", "systeme")]) &&
         identical(pr$collecte$outil, "KoboToolbox") && identical(pr$collecte$formulaire, titre) && is.null(pr$import_collecte) && !is.null(po$import_collecte))
d <- definir_modele(pr, "LM", x = numeros(pr, "habitat"), y = numeros(pr, "effectif"), observations = numeros_observations(pr), echelle = c(defaut = "none"))
m <- ajuster(pr, d)
tm <- texte_methodes(pr, d, m, "fr", "essai")$paragraphes[1]
te <- texte_methodes(pr, d, m, "en", "essai")$paragraphes[1]
verifier("le texte des Méthodes dit d'où viennent les données, en français et en anglais",
         startsWith(tm, sprintf("Les données ont été recueillies avec KoboToolbox (formulaire « %s »). ", titre)) &&
         startsWith(te, sprintf("The data were collected with KoboToolbox (form “%s”). ", titre)))
verifier("… ODK, sans formulaire", identical(phrase_methodes_collecte(q, TRUE), "Les données ont été recueillies avec ODK.") &&
         is.null(phrase_methodes_collecte(projet_depuis_tableau(data.frame(a = c("x", "y"), b = 1:2)), TRUE)))
appel <- appel_collecte(kobo_xlsx, po$import_collecte)
refait <- eval(parse(text = appel))
verifier("l'appel du journal refait le même projet", startsWith(appel, "importer_collecte(") && grepl("table = \"observation\"", appel, fixed = TRUE) &&
         identical(refait$donnees, po$donnees) && identical(refait$variables, po$variables))

cat("Les refus\n")
verifier("un tableau ordinaire", startsWith(erreur(lire_collecte(ordinaire)), "ordinaire.csv n'est pas un export de KoboToolbox ni d'ODK Central"))
verifier("le formulaire donné comme export", startsWith(erreur(lire_collecte(form)), "inventaire_oiseaux_formulaire.xlsx est un formulaire XLSForm"))
verifier("une archive sans CSV", startsWith(erreur(lire_collecte(vide_zip)), "vide.zip n'a aucun fichier CSV"))
verifier("un fichier d'un autre format", startsWith(erreur(lire_collecte(fichier)), "observations.nuee n'est pas un export"))
verifier("un formulaire sans feuille survey", startsWith(erreur(importer_collecte(kobo_xlsx, formulaire = kobo_xlsx)), "inventaire_kobo.xlsx n'est pas un formulaire XLSForm"))
verifier("un formulaire qui ne correspond pas à l'export", startsWith(erreur(importer_collecte(kobo_xlsx, formulaire = f("autre_formulaire.xlsx"))),
                                                                     "Le formulaire autre_formulaire.xlsx ne correspond pas à cet export"))
options(nuee.langue = "en")
verifier("en anglais", startsWith(erreur(lire_collecte(ordinaire)), "ordinaire.csv isn't a KoboToolbox or ODK Central export") &&
         startsWith(apercu_collecte(odk_zip, table = "observation")$phrases[1], "ODK Central export (.zip archive, 3 tables): the repeat group “observation”, 12 row(s), from 5 submission(s)."))
options(nuee.langue = "fr")

cat("Un choix multiple que le formulaire n'a pas, avant un qu'il a\n")
melange <- f("melange.csv")
utils::write.table(df(start = debuts[1:3], `extra` = c("a b", "b", NA), `extra/a` = c(1, 0, NA), `extra/b` = c(1, 1, NA), meteo = S$meteo[1:3],
                      `_id` = 1:3, `_uuid` = S$uuid[1:3], `_index` = 1:3), melange, sep = ";", row.names = FALSE, na = "", fileEncoding = "UTF-8")
pm <- importer_collecte(melange, formulaire = form)
am <- attr(pm, "collecte")$phrases
verifier("chacun ses colonnes 0/1 : celles de l'export pour l'un, d'après les réponses pour l'autre, chaque phrase les siennes",
         identical(variables(pm), c("extra_a", "extra_b", paste0("meteo_", meteo))) &&
         any(startsWith(am, "Choix multiples « extra » en 2 colonnes 0/1 (celles de l'export ; 1 : choisi, vide : sans réponse) : extra_a, extra_b.")) &&
         any(startsWith(am, "Choix multiples « meteo » en 4 colonnes 0/1 (d'après les réponses ; 1 : choisi, vide : sans réponse) : meteo_soleil, meteo_nuages")) &&
         any(startsWith(am, "Colonnes absentes du formulaire, lues sans lui (1) : extra.")))

cat("Les morceaux\n")
dt <- kobo_dates(c("2024-05-03T08:12:45.123+02:00", "2024-05-03T08:12:00Z", "2024-05-03", NA, "2024-05-03 08:12"))
verifier("les dates et heures : fraction et fuseau retirés, secondes pour toute la colonne, une date seule telle quelle",
         identical(as.vector(dt), c("2024-05-03 08:12:45", "2024-05-03 08:12:00", "2024-05-03", NA, "2024-05-03 08:12:00")) && isTRUE(attr(dt, "valide")))
verifier("une heure seule ; pas des dates", identical(as.vector(kobo_dates(c("08:30:00.000+02:00", "17:05"))), c("08:30", "17:05")) &&
         !isTRUE(attr(kobo_dates(c("2024-05-03", "hier")), "valide")) && !isTRUE(attr(kobo_dates(c(NA, NA)), "valide")))
verifier("des noms courts et uniques", identical(kobo_noms_courts(list(c("a", "x", "q"), c("b", "x", "q"), "habitat", c("g", "habitat"), c("g", "z"))),
                                                 c("a_x_q", "b_x_q", "habitat", "g_habitat", "z")))
pt <- kobo_points_texte(c("43.6 3.8 52 4.8", NA, "91 3 0 0"))
verifier("des points GPS en texte : une latitude hors des bornes n'en est pas un", identical(pt[1, ], c(43.6, 3.8, 52, 4.8)) && !isTRUE(attr(pt, "valide")))
lx <- list(noms = c("pre", "preh", "hum"), libelles = matrix(c("Prairie", "Prairie humide", "Humide", "Meadow", "Wet meadow", "Wet"), 3,
                                                             dimnames = list(NULL, c("Français (fr)", "English (en)"))))
verifier("des choix multiples en libellés : découpés aux libellés, un libellé à espaces compris, dans une langue ; sinon rien",
         identical(kobo_libelles_reponses(c("Prairie humide Humide", NA, "Prairie"), lx), list(c("preh", "hum"), character(0), "pre")) &&
         identical(kobo_libelles_reponses(c("Wet meadow", "Meadow Wet"), lx), list("preh", c("pre", "hum"))) &&
         is.null(kobo_libelles_reponses(c("Prairie sèche"), lx)))
verifier("le séparateur des groupes d'un export de KoboToolbox d'après ses en-têtes ; « / » sans groupes",
         kobo_separateur(c("site.nom", "site.habitat", "meteo.soleil", "vent")) == "." && kobo_separateur(c("Site-Nom du site", "Site-Habitat")) == "-" &&
         kobo_separateur(c("nom", "habitat", "temp-air")) == "/" && kobo_separateur(c("site/nom", "site/habitat", "a.b", "a.c")) == "/")

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
