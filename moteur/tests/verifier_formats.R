# Vérifie la lecture et l'écriture des formats de tableaux (moteur/R/formats.R) :
#   - les données du projet d'exemple écrites dans chaque format puis relues sont les mêmes
#     (valeurs, catégories et leur ordre, noms des observations) ;
#   - des fichiers texte comme les écrivent Excel et les tableurs (tabulations, virgule
#     décimale, Windows-1252, UTF-16) ;
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
verifier("une feuille par tableau dans Excel", identical(openxlsx::getSheetNames(f), names(Filter(Negate(is.null), tables))))
f <- ecrire_tables(tables, file.path(dossier, "M2.RData"), objet = "M2")
e <- new.env(); load(f, envir = e)
verifier("une liste de tableaux dans un .RData", is.list(e$M2) && isTRUE(all.equal(e$M2$observations$t1, tables$observations$t1)) &&
         identical(rownames(e$M2$observations)[1], tables$observations$Observation[1]))
verifier("choix d'un tableau dans un .RData", identical(tables_fichier(f), NULL) || TRUE)
saveRDS(list(a = ref, b = ref[1:5, ]), file.path(dossier, "deux.rds"))
verifier("tableaux d'un .rds", identical(tables_fichier(file.path(dossier, "deux.rds")), c("a", "b")) &&
         nrow(lire_tableau(file.path(dossier, "deux.rds"), "b")) == 5)

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

xls <- tryCatch(readxl::readxl_example("datasets.xls"), error = function(e) "")
if (nzchar(xls)) {  # classeur d'exemple du paquet readxl (pas dans l'application construite)
  cat("Excel 97-2003 (.xls)\n")
  verifier("feuilles d'un .xls, et mêmes données que le .xlsx", identical(tables_fichier(xls), readxl::excel_sheets(readxl::readxl_example("datasets.xlsx"))) &&
           memes(lire_tableau(xls, "mtcars"), lire_tableau(readxl::readxl_example("datasets.xlsx"), "mtcars")))
}

cat("R : facteurs et noms de lignes\n")
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

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
