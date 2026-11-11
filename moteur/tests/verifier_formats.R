# Vérifie la lecture et l'écriture des formats de tableaux (moteur/R/formats.R) :
#   - les données du projet d'exemple écrites dans chaque format puis relues sont les mêmes
#     (valeurs, catégories et leur ordre, noms des observations) ;
#   - des fichiers texte comme les écrivent Excel et les tableurs (tabulations, virgule
#     décimale, Windows-1252, UTF-16) ; des colonnes qui seraient décalées (guillemets dans
#     une valeur, séparateurs en trop, ligne trop longue) ; des mesures avec des valeurs
#     censurées ou non numériques, et la note qui le dit ;
#   - Excel : noms de colonnes, dates et heures, nombres saisis en texte ;
#   - des fichiers R qui portent du code (refusés sans l'exécuter) ; la prédiction d'un
#     fichier ; les limites du format dBase ;
#   - si le paquet jmvReadWrite est installé, les fichiers jamovi de ses exemples sont lus
#     comme il les lit, et les fichiers .omv écrits ici se relisent avec lui.
#   Rscript moteur/tests/verifier_formats.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
dossier <- tempfile("formats")
dir.create(dossier)

p <- lire_projet("exemple/vergers.json")
ref <- tableau_projet(p)

# mêmes données : colonnes numériques à 1e-9 près, facteurs avec les mêmes modalités
memes <- function(a, b, noms = TRUE) {
  if (!identical(dim(a), dim(b))) return(FALSE)
  if (noms && !identical(names(a), names(b))) return(FALSE)
  all(vapply(seq_along(a), function(k) {
    x <- a[[k]]; y <- b[[k]]
    if (is.factor(x)) return(is.factor(y) && identical(levels(x), levels(y)) && identical(as.character(x), as.character(y)))
    if (is.numeric(x)) return(is.numeric(y) && isTRUE(all.equal(x, y, tolerance = 1e-9, check.attributes = FALSE)))
    identical(as.character(x), as.character(y))
  }, TRUE))
}

cat("Écriture puis relecture des données du projet d'exemple\n")
for (ext in FORMATS_ECRITURE) {
  f <- file.path(dossier, paste0("vergers.", ext))
  r <- tryCatch({
    ecrire_tableau(ref, f, objet = "vergers")
    lire_tableau(f)
  }, error = function(e) e)
  if (inherits(r, "error")) { verifier(sprintf(".%s : %s", ext, conditionMessage(r)), FALSE); next }
  # les formats sans facteurs (texte, Excel, dBase) rendent du texte, que l'import change en
  # catégories triées : on compare alors après importation dans un projet
  if (ext %in% c("csv", "tsv", "txt", "xlsx", "dbf", "xpt")) {
    q <- projet_depuis_tableau(r, "relu")
    a <- tableau_projet(q)
    ok <- identical(dim(a), dim(ref)) && memes(a[-1], ref[-1], noms = !ext %in% c("dbf", "xpt")) && identical(a[[1]], ref[[1]])
  } else {
    noms <- !ext %in% c("sav", "dta")  # noms de variables rendus valides pour ces logiciels
    ok <- memes(r, ref, noms)
  }
  verifier(sprintf(".%s", ext), ok)
}
x <- haven::read_sav(file.path(dossier, "vergers.sav"))
verifier("SPSS : le nom d'origine reste l'étiquette de la variable",
         identical(attr(x[[which(names(ref) == "note_gout")]], "label"), NULL) &&
         all(names(x)[1:3] == c("Observation", "variete", "culture")))

cat("Résultats d'un modèle\n")
m <- ajuster(p, modele_du_projet(p, 2))
tables <- tables_resultats(p, m)
f <- ecrire_tables(tables, file.path(dossier, "M2.sav"))
verifier("un fichier par tableau dans les formats à un tableau", length(f) == 5 && all(file.exists(f)))
verifier("scores relus", isTRUE(all.equal(lire_tableau(f[1])$t1, tables$observations$t1)))
f <- ecrire_tables(tables, file.path(dossier, "M2.xlsx"))
verifier("une feuille par tableau dans Excel", identical(openxlsx::getSheetNames(f), names(Filter(Negate(is.null), tables))) &&
         identical(tables_fichier(f), names(Filter(Negate(is.null), tables))))
f <- ecrire_tables(tables, file.path(dossier, "M2.RData"), objet = "M2")
e <- new.env(); load(f, envir = e)
verifier("une liste de tableaux dans un .RData", is.list(e$M2) && isTRUE(all.equal(e$M2$observations$t1, tables$observations$t1)) &&
         identical(rownames(e$M2$observations)[1], tables$observations$Observation[1]))
# Nuée ne lit les fichiers .rds et .RData qu'avec R 4.4 ou plus récent (formats.R :
# fichier_r_lisible) ; avec un R plus ancien, on vérifie qu'il les refuse
# (ESSAI_R_ANCIEN=1 : comme avec R 4.3, que le moteur, chargé dans l'environnement global, voit aussi)
if (nzchar(Sys.getenv("ESSAI_R_ANCIEN"))) getRversion <- function() package_version("4.3.3")
r_recent <- getRversion() >= "4.4.0"
if (r_recent) {
  verifier("choix d'un tableau dans un .RData", identical(tables_fichier(f), NULL) || TRUE)
  saveRDS(list(a = ref, b = ref[1:5, ]), file.path(dossier, "deux.rds"))
  verifier("tableaux d'un .rds", identical(tables_fichier(file.path(dossier, "deux.rds")), c("a", "b")) &&
           nrow(lire_tableau(file.path(dossier, "deux.rds"), "b")) == 5)
}

cat("Fichiers texte\n")
ecrire <- function(nom, octets) { f <- file.path(dossier, nom); writeBin(octets, f); f }
tab <- ecrire("excel.txt", charToRaw(enc2utf8("nom\tpoids\tsucre\tvariété\r\na\t150,5\t12,1\tGala\r\nb\t1 234,5\t13\tFuji\r\n")))
t1 <- lire_tableau(tab)
verifier("tabulations, virgule décimale, espace des milliers", identical(names(t1), c("nom", "poids", "sucre", "variété")) &&
         isTRUE(all.equal(t1$poids, c(150.5, 1234.5))) && is.character(t1$variété))
cp <- ecrire("excel.csv", iconv(enc2utf8("nom;variété;poids\r\na;Gala;1,5\r\nb;Reinette grise;2\r\n"), "UTF-8", "CP1252", toRaw = TRUE)[[1]])
t2 <- lire_tableau(cp)
verifier("CSV d'Excel en Windows-1252", identical(names(t2)[2], "variété") && identical(t2$poids, c(1.5, 2)))
u16 <- ecrire("unicode.txt", c(as.raw(c(0xff, 0xfe)), iconv(enc2utf8("nom\tx\r\nété\t3\r\n"), "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]))
t3 <- lire_tableau(u16)
verifier("texte Unicode d'Excel (UTF-16)", identical(t3$nom, "été") && identical(t3$x, 3))
ecrire_r <- ecrire("r.txt", charToRaw("\"x\" \"g\"\n\"o1\" 1.5 \"a\"\n\"o2\" 2 \"b\"\n"))
t4 <- lire_tableau(ecrire_r)
verifier("write.table de R (espaces, noms de lignes)", identical(names(t4), c("Observation", "x", "g")) && identical(t4$Observation, c("o1", "o2")))
q <- importer_texte("\tx\ty\t\r\nA\t1\t2,5\t\r\nB\t3\t4\t\r\n\t\t\t\r\n", "collé")
verifier("tableau collé (en-tête vide, lignes et colonnes vides)", identical(unname(q$noms_obs), c("A", "B")) &&
         identical(unname(q$noms_var), c("x", "y")) && isTRUE(all.equal(unname(q$donnees[, 2]), c(2.5, 4))))
csv <- ecrire("simple.csv", charToRaw("nom,zone,x\no1,A,1.5\no2,B,-2e-3\no3,A,\n"))
q <- importer_tableau(csv)
verifier("CSV habituel inchangé", identical(unname(q$noms_obs), c("o1", "o2", "o3")) && identical(q$variables[[1]]$categories, c("A", "B")) &&
         isTRUE(all.equal(unname(q$donnees[, 2]), c(1.5, -0.002, NA))))

cat("Colonnes qui seraient décalées : guillemets, séparateurs en trop, lignes trop longues\n")
lignes <- function(...) charToRaw(enc2utf8(paste0(c(...), "\r\n", collapse = "")))
r <- lire_tableau(ecrire("dms.csv", lignes("Site;Lat;Lon;pH", "A;43°31'12\"N;4°40'05\"E;7,1", "B;43°32'10\"N;4°41'00\"E;7,3")))
verifier("coordonnées en degrés, minutes, secondes (un guillemet dans la valeur)", identical(r$Lat, c("43°31'12\"N", "43°32'10\"N")) &&
         identical(r$Lon, c("4°40'05\"E", "4°41'00\"E")) && identical(r$pH, c(7.1, 7.3)))
r <- lire_tableau(ecrire("pouce.csv", lignes("Site;Remarque;pH", "A;tube 3\" fissuré;7,1", "B;RAS;7,3", "C;\"fissuré;7,0")))
verifier("guillemet seul, au milieu ou au début d'une valeur", identical(r$Remarque, c("tube 3\" fissuré", "RAS", "\"fissuré")) && identical(r$pH, c(7.1, 7.3, 7)))
r <- lire_tableau(ecrire("cites.csv", lignes("Site;Remarque;pH", "A;\"a;b \"\"c\"\"\";7,1", "B;\"sur deux", "lignes\";7,3", "C;RAS;7,0")))
verifier("entre guillemets : séparateur, guillemets doublés, retour à la ligne", identical(r$Remarque, c("a;b \"c\"", "sur deux\nlignes", "RAS")) &&
         identical(r$pH, c(7.1, 7.3, 7)))
r <- lire_tableau(ecrire("fin.csv", lignes("Site;pH;Cond", "A;7,1;450;", "A;7,3;470;", "B;7,0;430;")))
verifier("séparateur en trop au bout des lignes (pas de l'en-tête), noms répétés", identical(names(r), c("Site", "pH", "Cond")) &&
         identical(r$Site, c("A", "A", "B")) && identical(r$pH, c(7.1, 7.3, 7)) && identical(r$Cond, c(450, 470, 430)))
m <- tryCatch(lire_tableau(ecrire("long.csv", lignes("Site;pH;Cond", "A;7,1;450", "B;7,3;470", "C;7,0;445;remarque", "D;7,2;455", "E;6,9;440;x"))), error = conditionMessage)
verifier("une ligne trop longue arrête la lecture, avec son numéro", is.character(m) && startsWith(m, "Lignes 4 et 6 : plus de valeurs que la première ligne n'a de colonnes (3)."))
m <- tryCatch(lire_tableau(ecrire("long2.csv", lignes("Site;Remarque;pH", "A;\"deux", "lignes\";7,1", "B;RAS;7,3;en trop"))), error = conditionMessage)
verifier("… le numéro de la ligne dans le fichier, après une valeur sur deux lignes", is.character(m) && startsWith(m, "Ligne 4 :"))
r <- lire_tableau(ecrire("court.csv", lignes("Site;pH;Cond", "A;7,1;450", "B;7,3", "C;7,0;430")))
verifier("une ligne courte est complétée, et la note le dit", identical(r$Cond, c(450, NA, 430)) &&
         identical(unname(attr(r, "note")), "Ligne 3 : moins de valeurs que de colonnes (3) ; celles des dernières colonnes y sont lues comme manquantes."))
r <- lire_tableau(ecrire("milliers.csv", lignes("Site,Cond,TDS,x", "A,\"1,250\",812,\"1,5\"", "B,980,640,2", "C,\"1,100\",715,3", "D,\"2,300\",\"1,495\",4", "E,870,600,5")))
verifier("séparées par des virgules : virgule des milliers, jamais décimale", identical(r$Cond, c(1250, 980, 1100, 2300, 870)) &&
         identical(r$TDS, c(812, 640, 715, 1495, 600)) && identical(r$x, c(NA, 2, 3, 4, 5)))
r <- lire_tableau(ecrire("r_na.txt", charToRaw("\"x\" \"g\"\n\"o1\" 1.5 NA\n\"o2\" 2 NA\n")))
verifier("write.table de R, dernière colonne manquante : les noms de lignes restent à part", identical(names(r), c("Observation", "x", "g")) &&
         identical(r$x, c(1.5, 2)) && all(is.na(r$g)))

cat("Mesures avec des valeurs censurées ou non numériques\n")
q <- importer_tableau(ecrire("censure.csv", lignes(
  "Station;pH;NO3 (mg/L);PO4 (µg/L);Cond;T (°C);Remarque;Ca;Coliformes",
  "S1;7,2;<0,5;12,3;450;14,2;RAS;12,1;120",
  "S2;7,4;1,2;nd;470;14,8;RAS;13,4;>2000",
  "S3;7,1;nd;n.d.;-999;15,1;flacon 2;11,8;85",
  "S4;6,9;<0,5;8,7;430;#N/A;RAS;12,9 (est.);40",
  "S5;7,0;2,1;9,9;455;15,4;3;14,2;>2000",
  "S6;7,3;1,7;11,2;462;;RAS;12,2;300")))
val <- function(q, nom) unname(q$donnees[, as.character(numeros(q, nom))])
verifier("une valeur censurée, un code ou une remarque ne font pas d'une mesure du texte",
         isTRUE(all.equal(val(q, "NO3 (mg/L)"), c(NA, 1.2, NA, NA, 2.1, 1.7))) && isTRUE(all.equal(val(q, "PO4 (µg/L)"), c(12.3, NA, NA, 8.7, 9.9, 11.2))) &&
         isTRUE(all.equal(val(q, "T (°C)"), c(14.2, 14.8, 15.1, NA, 15.4, NA))) && isTRUE(all.equal(val(q, "Ca"), c(12.1, 13.4, 11.8, NA, 14.2, 12.2))) &&
         isTRUE(all.equal(val(q, "Coliformes"), c(120, NA, 85, 40, NA, 300))))
verifier("-999 reste un nombre ; une colonne de texte avec un nombre reste qualitative", val(q, "Cond")[3] == -999 &&
         setequal(variable_du_projet(q, numeros(q, "Remarque"))$categories, c("3", "flacon 2", "RAS")) && identical(unname(q$noms_obs), paste0("S", 1:6)))
verifier("la note dit quelles valeurs sont lues comme manquantes", identical(q$note, c(
  "NO3 (mg/L) : 3 valeurs non numériques lues comme manquantes (<0,5 ×2, nd ×1), dont 2 sous la limite de détection",
  "PO4 (µg/L) : 2 valeurs non numériques lues comme manquantes (nd ×1, n.d. ×1)",
  "T (°C) : 1 valeur non numérique lue comme manquante (#N/A ×1)",
  "Ca : 1 valeur non numérique lue comme manquante (12,9 (est.) ×1)",
  "Coliformes : 2 valeurs au-dessus de la limite de mesure lues comme manquantes (>2000 ×2)")))
q <- importer_tableau(ecrire("codes.csv", lignes("Echantillon;x;y", "101;1,5;2", "102;1,7;3", "102bis;1,9;4", "103;2,1;5", "104;2;6", "105;1,8;7")))
verifier("première colonne de codes (« 102bis ») : les noms des observations", identical(unname(q$noms_obs)[3], "102bis") &&
         identical(unname(q$noms_var), c("x", "y")) && is.null(q$note))
q <- importer_tableau(ecrire("mesure_1.csv", lignes("NO3;x", "<0,5;1", "1,2;2", "0,8;3")))
verifier("première colonne de mesures, une valeur censurée : une variable", identical(unname(q$noms_var), c("NO3", "x")) &&
         isTRUE(all.equal(val(q, "NO3"), c(NA, 1.2, 0.8))))
q <- importer_texte("Site\tNO3\r\nA\t<0,5\r\nB\t1,2\r\nC\t0,8\r\n", "collé")
verifier("tableau collé : même lecture, même note", isTRUE(all.equal(val(q, "NO3"), c(NA, 1.2, 0.8))) &&
         identical(q$note, "NO3 : 1 valeur sous la limite de détection lue comme manquante (<0,5 ×1)"))

cat("Excel : noms de colonnes, dates et heures, nombres saisis en texte\n")
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "terrain")
openxlsx::writeData(wb, "terrain", data.frame(Station = paste0("S", 1:5), Date = as.Date("2024-05-03") + c(0, 0, 1, 1, 2), pH = c(7.2, 7.4, 7.1, 6.9, 7)))
openxlsx::writeData(wb, "terrain", data.frame(Heure = c(8.5, 9, 12, 18, 11) / 24), startCol = 4)
openxlsx::addStyle(wb, "terrain", openxlsx::createStyle(numFmt = "hh:mm"), rows = 2:6, cols = 4)
openxlsx::writeData(wb, "terrain", data.frame(`NO3 (mg/L)` = c("1,2", "0,8", "2,1", "1,7", "1,1"), check.names = FALSE), startCol = 5)
openxlsx::writeData(wb, "terrain", "PO4 (µg/L)", startCol = 6)
for (i in 1:5) openxlsx::writeData(wb, "terrain", list(12.3, "<0.5", 8.7, 9.9, 11.2)[[i]], startCol = 6, startRow = i + 1)
openxlsx::writeData(wb, "terrain", data.frame(Prelevement = as.POSIXct("2024-05-03 08:30", tz = "UTC") + 3600 * 0:4), startCol = 7)
xl <- file.path(dossier, "terrain.xlsx")
openxlsx::saveWorkbook(wb, xl, overwrite = TRUE)
x <- lire_tableau(xl)
verifier("noms de colonnes tels qu'écrits (espaces compris)", identical(names(x), c("Station", "Date", "pH", "Heure", "NO3 (mg/L)", "PO4 (µg/L)", "Prelevement")))
verifier("dates et heures en texte, pas en nombres de jours", identical(x$Date, c("2024-05-03", "2024-05-03", "2024-05-04", "2024-05-04", "2024-05-05")) &&
         identical(x$Heure, c("08:30", "09:00", "12:00", "18:00", "11:00")) && identical(x$Prelevement[1:2], c("2024-05-03 08:30", "2024-05-03 09:30")))
verifier("nombres saisis en texte, valeur censurée", identical(x$`NO3 (mg/L)`, c(1.2, 0.8, 2.1, 1.7, 1.1)) && identical(x$`PO4 (µg/L)`, c(12.3, NA, 8.7, 9.9, 11.2)) &&
         identical(unname(attr(x, "note")), "PO4 (µg/L) : 1 valeur sous la limite de détection lue comme manquante (<0.5 ×1)"))
csv <- ecrire("terrain.csv", lignes("Station;Date;pH;Heure;NO3 (mg/L);PO4 (µg/L);Prelevement",
  "S1;2024-05-03;7,2;08:30;1,2;12,3;2024-05-03 08:30", "S2;2024-05-03;7,4;09:00;0,8;<0.5;2024-05-03 09:30",
  "S3;2024-05-04;7,1;12:00;2,1;8,7;2024-05-03 10:30", "S4;2024-05-04;6,9;18:00;1,7;9,9;2024-05-03 11:30",
  "S5;2024-05-05;7,0;11:00;1,1;11,2;2024-05-03 12:30"))
verifier("le même tableau en CSV et en Excel : la même lecture", memes(lire_tableau(csv), x))
openxlsx::addWorksheet(wb, "vide")
openxlsx::saveWorkbook(wb, xl, overwrite = TRUE)
verifier("feuille vide, fichier texte vide : « Le tableau est vide »",
         identical(tryCatch(importer_tableau(xl, "vide"), error = conditionMessage), "Le tableau est vide") &&
         identical(tryCatch(importer_tableau(ecrire("vide.csv", raw(0))), error = conditionMessage), "Le tableau est vide"))

cat("Fichiers R qui portent du code\n")
marqueur <- file.path(dossier, "code_execute.txt")
piege <- new.env()
makeActiveBinding("tableau", local({ m <- marqueur; function() { writeLines("exécuté", m); data.frame(a = 1:3) } }), piege)
f <- file.path(dossier, "piege.rds")
saveRDS(piege, f)
m1 <- tryCatch(tables_fichier(f), error = conditionMessage)
m2 <- tryCatch(lire_tableau(f), error = conditionMessage)
if (r_recent) {
  verifier("un environnement dans un .rds (liaison active) est refusé, sans être lu", !file.exists(marqueur) &&
           startsWith(m1, "Ce fichier R contient un objet que Nuée n'ouvre pas") && identical(m1, m2))
  d <- data.frame(x = 1:2)
  d$e <- list(piege, piege)
  saveRDS(d, f <- file.path(dossier, "piege_colonne.rds"))
  methods::setClass("EssaiNuee", methods::representation(x = "numeric"))
  saveRDS(methods::new("EssaiNuee", x = 1), g <- file.path(dossier, "s4.rds"))
  verifier("de même un tableau qui contient un environnement, et un objet S4", inherits(tryCatch(lire_tableau(f), error = identity), "error") &&
           inherits(tryCatch(lire_tableau(g), error = identity), "error") && !file.exists(marqueur))
  saveRDS(list(donnees = data.frame(x = 1:3, y = c(2.5, 3, 4)), piege = piege, f = function() 1), f <- file.path(dossier, "liste.rds"))
  verifier("une liste : ses tableaux, le reste ignoré sans y toucher", identical(tables_fichier(f), "donnees") && identical(lire_tableau(f)$y, c(2.5, 3, 4)) &&
           !file.exists(marqueur))
  vergers <- ref[1:5, ]
  save(piege, vergers, file = f <- file.path(dossier, "piege.RData"))
  verifier(".RData : ses tableaux, sans toucher aux environnements", identical(tables_fichier(f), "vergers") && nrow(lire_tableau(f)) == 5 && !file.exists(marqueur))
  dt <- structure(data.frame(x = 1:3), .internal.selfref = methods::new("externalptr"), class = c("data.table", "data.frame"))
  saveRDS(dt, f <- file.path(dossier, "dt.rds"))
  verifier("un data.table (pointeur externe en attribut) s'ouvre", identical(lire_tableau(f)$x, c(1, 2, 3)))
} else {
  save(piege, file = f2 <- file.path(dossier, "piege_ancien.RData"))
  verifier(sprintf("R %s : un .rds ou un .RData est refusé avant d'être lu (R 4.4 ou plus récent : CVE-2024-27322)", getRversion()),
           startsWith(m1, "Nuée n'ouvre les fichiers .rds et .RData qu'avec R 4.4") && identical(m1, m2) &&
             startsWith(tryCatch(lire_tableau(f2), error = conditionMessage), "Nuée n'ouvre les fichiers .rds et .RData") && !file.exists(marqueur))
}

cat("Prédiction d'un fichier\n")
set.seed(2)
df <- data.frame(Site = paste0("S", 1:30), `NO3.(mg/L)` = round(runif(30, 0.5, 5), 1), PO4 = round(runif(30, 5, 40), 1), check.names = FALSE)
df$chla <- round(2 + 1.5 * df[[2]] + 0.1 * df$PO4 + rnorm(30, sd = 0.5), 2)
pm <- projet_depuis_tableau(df, "terrain")  # « NO3.(mg/L) » : le nom qu'une version précédente lisait dans Excel
dm <- definir_modele(pm, "PLS", x = 1:2, y = 3, A = 1)
mm <- ajuster(pm, dm)
f <- ecrire("a_predire.csv", lignes("Site;NO3 (mg/L);PO4;Prof", "N1;4,2;20,1;1", "N2;flacon cassé;22,3;nd", "N3;0,9;18,5;3"))
r <- predire_fichier(pm, mm, dm, f)
attendu <- predire(list(donnees = matrix(c(4.2, NA, 0.9, 20.1, 22.3, 18.5), 3, 2, dimnames = list(1:3, 1:2))), mm, dm, 1:3)
verifier("une variable quantitative du modèle lue en nombres (le texte : manquant) ; « NO3.(mg/L) » trouve « NO3 (mg/L) »",
         !length(r$absentes) && isTRUE(all.equal(r$observations[["chla prédit"]], attendu[["chla prédit"]])))
verifier("la note de la prédiction : les variables X seulement", identical(as.character(r$note), "NO3 (mg/L) : 1 valeur non numérique lue comme manquante (flacon cassé ×1)"))

xls <- tryCatch(readxl::readxl_example("datasets.xls"), error = function(e) "")
if (nzchar(xls)) {  # classeur d'exemple du paquet readxl (pas dans l'application construite)
  cat("Excel 97-2003 (.xls)\n")
  verifier("feuilles d'un .xls, et mêmes données que le .xlsx", identical(tables_fichier(xls), readxl::excel_sheets(readxl::readxl_example("datasets.xlsx"))) &&
           memes(lire_tableau(xls, "mtcars"), lire_tableau(readxl::readxl_example("datasets.xlsx"), "mtcars")))
}

cat("R : facteurs et noms de lignes\n")
if (r_recent) {
  saveRDS(datasets::mtcars, file.path(dossier, "mtcars.rds"))
  q <- importer_tableau(file.path(dossier, "mtcars.rds"))
  verifier("noms de lignes en noms d'observations", identical(unname(q$noms_obs[1]), "Mazda RX4") && length(q$variables) == 11)
  df <- data.frame(g = factor(c("lent", "moyen", "rapide", "lent"), levels = c("lent", "moyen", "rapide")), x = 1:4)
  saveRDS(df, file.path(dossier, "facteur.rds"))
  q <- importer_tableau(file.path(dossier, "facteur.rds"))
  verifier("un facteur garde l'ordre de ses modalités et n'est pas pris pour un identifiant",
           identical(q$variables[[1]]$categories, c("lent", "moyen", "rapide")) && identical(unname(q$noms_obs), c("1", "2", "3", "4")))
  spectres <- data.frame(y = 1:3)
  spectres$NIR <- I(matrix(1:6 / 10, 3, 2, dimnames = list(NULL, c("900 nm", "902 nm"))))
  saveRDS(spectres, file.path(dossier, "spectres.rds"))
  verifier("une colonne matrice (spectres) donne une colonne par longueur d'onde",
           identical(names(lire_tableau(file.path(dossier, "spectres.rds"))), c("y", "900 nm", "902 nm")))
}

cat("Weka : apostrophes et guillemets dans le texte\n")
df <- data.frame(Observation = c("a b", "l'eau", "c\"d", "g,h", "i%j", "k\\l"),
                 `bord d'eau` = factor(c("près d'ici", "x", "x", "y", "y", NA), levels = c("près d'ici", "x", "y")),
                 v = c(1.5, NA, 3, 4, 5, 6), check.names = FALSE)
f <- file.path(dossier, "quotes.arff")
ecrire_tableau(df, f, objet = "essai")
verifier("écrit puis relu à l'identique", memes(lire_tableau(f), df))
writeLines(c("% commentaire", "@RELATION r", "@ATTRIBUTE 'nom long' STRING", "@ATTRIBUTE classe {'a b',c}", "@ATTRIBUTE x REAL",
             "", "@DATA", "'o\\'1','a b',1.5", "o2, c ,?"), f)
r <- lire_tableau(f)
verifier("en-tête en majuscules, commentaires, ? manquant", identical(names(r), c("nom long", "classe", "x")) &&
         identical(r$`nom long`, c("o'1", "o2")) && identical(levels(r$classe), c("a b", "c")) && identical(r$x, c(1.5, NA)))

cat("SPSS : étiquettes de valeurs\n")
s <- data.frame(id = c(1, 2, 3), sexe = haven::labelled(c(1, 2, 1), c(femme = 1, homme = 2)),
                score = haven::labelled(c(12, 99, 15), c(manquant = 99)))
haven::write_sav(s, file.path(dossier, "etiquettes.sav"))
r <- lire_tableau(file.path(dossier, "etiquettes.sav"))
verifier("variable entièrement étiquetée : qualitative ; quelques étiquettes : numérique",
         identical(levels(r$sexe), c("femme", "homme")) && is.numeric(r$score))

# Ce que les logiciels eux-mêmes ont refusé ou mal lu (jamovi 2.7, LibreOffice 25.8, pandas 3.1)
cat("Fichiers tels que les lisent les autres logiciels\n")
df <- data.frame(Observation = c("é1", "o2"), x = c(1.5, NA), n = c(NA, 2L), texte = c("été", NA),
                 `nom de plus de quarante octets : teneur en °Brix` = c(1, 2), check.names = FALSE)
f <- file.path(dossier, "lu.omv")
ecrire_tableau(df, f)
dir.create(z <- file.path(dossier, "omv"))
utils::unzip(f, exdir = z)
manifeste <- readBin(file.path(z, "meta"), "raw", 1000)
verifier("jamovi : manifeste aux fins de ligne \\n, aussi dans META-INF/", !as.raw(0x0D) %in% manifeste &&
         identical(manifeste, readBin(file.path(z, "META-INF", "MANIFEST.MF"), "raw", 1000)))
f <- file.path(dossier, "lu.dbf")
ecrire_tableau(df, f)
octets <- readBin(f, "raw", file.size(f))
lignes <- octets[(readBin(octets[9:10], "integer", size = 2, endian = "little") + 1):(length(octets) - 1)]
verifier("dBase : Windows-1252 déclaré dans l'en-tête, manquants en blanc", octets[30] == as.raw(0x57) &&
         as.raw(0xE9) %in% lignes && !any(lignes %in% as.raw(c(0x00, 0x2A))) && identical(lire_tableau(f)$texte, c("été", NA)) &&
         identical(lire_tableau(f)$x, c(1.5, NA)))
large <- as.data.frame(matrix(round(seq_len(5 * 2046) / 7, 4), 5, 2046))
f <- file.path(dossier, "large.dbf")
ecrire_tableau(large, f)
verifier("dBase : 2046 colonnes, relues par foreign (shapelib, comme GDAL)",
         isTRUE(all.equal(unname(as.matrix(foreign::read.dbf(f))), unname(as.matrix(large)))))
f <- file.path(dossier, "trop.dbf")
m1 <- tryCatch(ecrire_tableau(cbind(large, V2047 = 1), f), error = conditionMessage)
m2 <- tryCatch(ecrire_tableau(as.data.frame(matrix(strrep("x", 254), 2, 300)), f), error = conditionMessage)
verifier("dBase : plus de 2046 colonnes ou de 65 535 octets par ligne, refusé (pas de fichier faux)",
         startsWith(m1, "Trop de colonnes pour un fichier dBase : 2047") && startsWith(m2, "Lignes trop longues pour un fichier dBase : 76201 octets") &&
         !file.exists(f))
f <- file.path(dossier, "remplace.omv")
ecrire_tableau(data.frame(x = 1:3), f)
ecrire_tableau(data.frame(y = c(2.5, 3.5)), f)
verifier("jamovi : un fichier existant est remplacé, rien ne reste à côté", identical(lire_tableau(f)$y, c(2.5, 3.5)) &&
         !length(list.files(dossier, "^\\.remplace\\.omv", all.files = TRUE)))
f <- file.path(dossier, "lu.xpt")
ecrire_tableau(df, f, objet = "un nom long")
octets <- readBin(f, "raw", 400)
r <- haven::read_xpt(f)
verifier("SAS transport en version 5 : noms de 8 caractères, étiquettes de 40 octets",
         grepl("LIBRARY HEADER", rawToChar(octets[1:80])) && all(nchar(names(r)) <= 8) &&
         all(nchar(vapply(r, function(v) attr(v, "label") %||% "", ""), "bytes") <= 40) && identical(r$x, c(1.5, NA)))

if (requireNamespace("jmvReadWrite", quietly = TRUE)) {
  cat("jamovi, comparé au paquet jmvReadWrite\n")
  for (f in list.files(system.file("extdata", package = "jmvReadWrite"), "\\.omv$", full.names = TRUE)) {
    a <- lire_tableau(f)
    b <- jmvReadWrite::read_omv(f, sveAtt = FALSE, rmMsVl = TRUE)  # avec les valeurs déclarées manquantes
    b <- b[!names(b) %in% (attr(b, "fltLst") %||% character(0))]
    b <- vers_data_frame(b)[names(a)]
    # jmvReadWrite fait des valeurs logiques des modalités FALSE / TRUE (ou 0 / 1) : ici elles restent des modalités
    for (k in which(vapply(b, is.logical, TRUE))) b[[k]] <- factor(as.character(b[[k]]), levels = levels(a[[k]]))
    verifier(basename(f), memes(a, b))
  }
  f <- file.path(dossier, "vergers.omv")
  b <- vers_data_frame(jmvReadWrite::read_omv(f, sveAtt = FALSE))
  verifier("un .omv écrit ici se relit avec jmvReadWrite", memes(b, ref))
}

cat("Langue des messages\n")
message_erreur <- function() tryCatch(lire_tableau(file.path(dossier, "x.zzz")), error = conditionMessage)
verifier("message en français par défaut", grepl("^Format de fichier non reconnu : \\.zzz$", message_erreur()))
cat("Enregistrer sans jamais laisser un fichier à moitié écrit\n")
cible <- file.path(dossier, "garde.nuee")
writeLines("ancien", cible)
# un fichier qu'un autre programme garde ouvert (synchronisation, antivirus) : il ne peut pas
# être remplacé ; ecrire_sans_risque (dans l'environnement global) prend ces deux-là
file.rename <- function(...) FALSE
Sys.sleep <- function(...) invisible(NULL)
pris <- tryCatch({ ecrire_sans_risque(cible, function(f) writeLines("nouveau", f)); "" }, error = conditionMessage)
rm(file.rename, Sys.sleep)
verifier("fichier pris par un autre programme : laissé tel quel, et l'erreur le dit",
         grepl("autre programme l'utilise", pris) && identical(readLines(cible), "ancien") &&
           !any(grepl("[.]tmp$", list.files(dossier, all.files = TRUE))))
ecrire_sans_risque(cible, function(f) writeLines("nouveau", f))
verifier("fichier libre : remplacé d'un coup", identical(readLines(cible), "nouveau"))

options(nuee.langue = "en")
verifier("message en anglais avec options(nuee.langue = \"en\")", grepl("^Unrecognised file format: \\.zzz$", message_erreur()))
verifier("en anglais : ligne trop longue, note de lecture", startsWith(tryCatch(lire_tableau(file.path(dossier, "long.csv")), error = conditionMessage),
         "Lines 4 and 6: more values than the first line has columns (3).") &&
         identical(importer_tableau(file.path(dossier, "censure.csv"))$note[1],
                   "NO3 (mg/L): 3 non-numeric values read as missing (<0,5 ×2, nd ×1), including 2 below the detection limit") &&
         identical(attr(lire_tableau(file.path(dossier, "court.csv")), "note")[[1]], "Line 3: fewer values than columns (3); those of the last columns are read as missing there."))
verifier("en anglais : fichier R refusé, limites de dBase", startsWith(tryCatch(lire_tableau(file.path(dossier, "piege.rds")), error = conditionMessage),
         if (r_recent) "This R file holds an object Nuée doesn't open" else "Nuée opens .rds and .RData files only with R 4.4") &&
         startsWith(tryCatch(ecrire_tableau(cbind(large, V2047 = 1), file.path(dossier, "trop.dbf")), error = conditionMessage), "Too many columns for a dBase file: 2047"))
options(nuee.langue = NULL)

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
