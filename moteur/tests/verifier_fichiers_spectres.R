# Vérifie la lecture des fichiers de spectres (moteur/R/fichiers_spectres.R) sur de petits
# fichiers inventés : JCAMP-DX dans chaque écriture (AFFN, PAC, SQZ, DIF/DUP), avec plusieurs
# blocs ; SPC nouveau et ancien format (un ou plusieurs sous-fichiers, entiers ou réels,
# abscisses données) ; texte. Des fichiers abîmés : chacun refusé en moins d'une seconde, sans
# prendre de mémoire, par un message qui le nomme. Puis le tableau des spectres et les
# informations jointes.
# La lecture a été comparée par ailleurs aux fichiers d'essai officiels de JCAMP-DX et aux
# fichiers d'exemple SPC (docs/developpement.md).
#   Rscript moteur/tests/verifier_fichiers_spectres.R

source("moteur/charger.R", encoding = "UTF-8")
echecs <- 0
verifier <- function(nom, ok) {
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-10) isTRUE(all.equal(unname(a), unname(b), tolerance = tol, check.attributes = FALSE))
dossier <- tempfile("spectres")
dir.create(dossier)
fichier <- function(nom, lignes) {
  f <- file.path(dossier, nom)
  writeLines(lignes, f, useBytes = TRUE)
  f
}

cat("JCAMP-DX\n")
y <- c(100, 103, 103, 103, 98, 98, 110, 115, 120, 120, 120, 120, 125)
entete <- function(titre) c(sprintf("##TITLE= %s", titre), "##JCAMP-DX= 5.01", "##XUNITS= NANOMETERS", "##YUNITS= ABSORBANCE",
                            "##XFACTOR= 1", "##YFACTOR= 0.5", "##FIRSTX= 1", "##LASTX= 13", "##NPOINTS= 13", "##XYDATA= (X++(Y..Y))")
ecritures <- list(
  AFFN = c("1 100 103 103 103 98", "6 98 110 115 120 120", "11 120 120 125"),
  PAC = c("1+100+103+103+103+98", "6+98+110+115+120+120", "11+120+120+125"),
  SQZ = c("1A00A03A03A03I8", "6I8A10A15A20A20", "11A20A20A25"),
  # DIF : écarts ; DUP : répétitions ; chaque ligne après une ligne finie en DIF commence par la
  # dernière valeur de la précédente (contrôle) ; « A20T » : le contrôle, puis 120 une fois de plus
  DIFDUP = c("1A00L%Tn", "5I8%J2NT%", "10A20T%N"))
for (k in names(ecritures)) {
  s <- lire_jcamp(fichier(paste0(k, ".jdx"), c(entete(k), ecritures[[k]], "##END=")))
  verifier(sprintf("%s : les 13 valeurs, x de 1 à 13, unités", k), length(s) == 1 && proche(s[[1]]$y, y * 0.5) && proche(s[[1]]$x, 1:13) &&
           s[[1]]$unite_x == "NANOMETERS" && s[[1]]$unite_y == "ABSORBANCE")
}
s <- lire_jcamp(fichier("fin.jdx", c(entete("fin"), ecritures$DIFDUP, "14@", "##END=")))
verifier("une dernière ligne d'une seule valeur fausse (qui ferme la table) est ignorée", proche(s[[1]]$y, y * 0.5))
verifier("une valeur de contrôle fausse ailleurs arrête la lecture",
         grepl("contrôle", tryCatch(lire_jcamp(fichier("faux.jdx", c(entete("faux"), "1A00L%Tn", "5I7%J2NT%", "10A20T%N", "##END="))), error = conditionMessage)))
verifier("un nombre de valeurs différent de NPOINTS arrête la lecture",
         grepl("NPOINTS", tryCatch(lire_jcamp(fichier("court.jdx", c(entete("court"), ecritures$AFFN[1:2], "##END="))), error = conditionMessage)))
compose <- fichier("bloc.jdx", c("##TITLE= deux spectres", "##JCAMP-DX= 5.01", "##BLOCKS= 3",
  entete("premier"), ecritures$SQZ, "##END=",
  "##TITLE= second", "##JCAMP-DX= 5.01", "##XUNITS= NANOMETERS", "##XFACTOR= 2", "##YFACTOR= 1", "##XYPOINTS= (XY..XY)",
  "1, 1.5; 2, 2.5 $$ un commentaire", "3, ?", "##END=",
  "##TITLE= pics", "##PEAK TABLE= (XY..XY)", "1, 5", "##END=", "##END="))
s <- lire_spectres(compose)
verifier("plusieurs blocs : un spectre par table XYDATA ou XYPOINTS, nommé fichier + titre ; pas les tables de pics",
         identical(vapply(s, `[[`, "", "nom"), c("bloc premier", "bloc second")) && proche(s[[2]]$x, c(2, 4, 6)) && identical(s[[2]]$y, c(1.5, 2.5, NA)))

cat("SPC\n")
octets <- function(v, taille, entier = TRUE) writeBin(if (entier) as.integer(v) else as.numeric(v), raw(), size = taille, endian = "little")
octet <- function(v) as.raw(v %% 256)
entete_spc <- function(drapeaux, exposant, npts, premier, dernier, nsub = 1, xtype = 3, ytype = 2) {
  o <- raw(512)
  o[1] <- octet(drapeaux); o[2] <- as.raw(0x4B); o[4] <- octet(exposant)
  o[5:8] <- octets(npts, 4); o[9:16] <- octets(premier, 8, FALSE); o[17:24] <- octets(dernier, 8, FALSE)
  o[25:28] <- octets(nsub, 4); o[29] <- octet(xtype); o[30] <- octet(ytype)
  o
}
sous_entete <- function(exposant, npts = 0) { o <- raw(32); o[2] <- octet(exposant); o[17:20] <- octets(npts, 4); o }
spc <- function(nom, ...) { f <- file.path(dossier, nom); writeBin(c(...), f); f }
v <- c(0.5, -0.25, 1.75, 2, 0.125)
s <- lire_spc(spc("reels.spc", entete_spc(0, -128, 5, 400, 800), sous_entete(-128), octets(v, 4, FALSE)))
verifier("un spectre en réels, abscisses régulières, unités", length(s) == 1 && proche(s[[1]]$y, v) && proche(s[[1]]$x, seq(400, 800, 100)) &&
         s[[1]]$unite_x == "NANOMETERS" && s[[1]]$unite_y == "ABSORBANCE")
n1 <- c(100, -2e9, 7, 0, 123456789)
s <- lire_spc(spc("multi.spc", entete_spc(4, 0, 5, 1, 5, nsub = 2, xtype = 1, ytype = 128), sous_entete(-1), octets(n1, 4), sous_entete(3), octets(n1, 4)))
verifier("plusieurs sous-fichiers en entiers, chacun avec son exposant (négatif aussi)",
         length(s) == 2 && proche(s[[1]]$y, n1 * 2^(-1 - 32)) && proche(s[[2]]$y, n1 * 2^(3 - 32)) && s[[1]]$unite_x == "1/CM" && s[[1]]$unite_y == "TRANSMITTANCE")
n2 <- c(1, -32768, 32767, 0, 5)
s <- lire_spc(spc("seize.spc", entete_spc(1, 2, 5, 10, 50), sous_entete(2), octets(n2, 2)))
verifier("entiers de 16 bits", proche(s[[1]]$y, n2 * 2^(2 - 16)))
x <- c(400, 410.5, 425, 450, 500)
s <- lire_spc(spc("xy.spc", entete_spc(128, -128, 5, 400, 500), octets(x, 4, FALSE), sous_entete(-128), octets(v, 4, FALSE)))
verifier("abscisses données, communes", proche(s[[1]]$x, x) && proche(s[[1]]$y, v))
s <- lire_spc(spc("xyxy.spc", entete_spc(128 + 64 + 4, -128, 0, 1, 9, nsub = 2),
                  sous_entete(-128, 2), octets(c(1, 2), 4, FALSE), octets(c(5, 6), 4, FALSE),
                  sous_entete(-128, 3), octets(c(7, 8, 9), 4, FALSE), octets(c(1, 2, 3), 4, FALSE)))
verifier("abscisses propres à chaque sous-fichier", proche(s[[1]]$x, 1:2) && proche(s[[1]]$y, 5:6) && proche(s[[2]]$x, 7:9) && proche(s[[2]]$y, 1:3))
ancien <- raw(256)
ancien[2] <- as.raw(0x4D); ancien[3:4] <- octets(5, 2); ancien[5:8] <- octets(5, 4, FALSE)
ancien[9:12] <- octets(1000, 4, FALSE); ancien[13:16] <- octets(1400, 4, FALSE); ancien[17] <- octet(1)
mots <- matrix(octets(n1, 4), 4)[c(3, 4, 1, 2), ]  # ancien format : mot de poids fort d'abord
s <- lire_spc(spc("ancien.spc", ancien, as.vector(mots)))
verifier("ancien format (entiers aux mots inversés)", proche(s[[1]]$y, n1 * 2^(5 - 32)) && proche(s[[1]]$x, seq(1000, 1400, 100)) && s[[1]]$unite_x == "1/CM")
verifier("une autre version est refusée", grepl("version", tryCatch(lire_spc(spc("autre.spc", { o <- entete_spc(0, 0, 1, 0, 1); o[2] <- as.raw(0x4C); o })), error = conditionMessage)))

cat("Texte\n")
s <- lire_spectres(fichier("deux.csv", c("nm,lot A,lot B", "400,0.1,0.2", "401,0.15,0.25", "402,0.2,0.3")))
verifier("une colonne d'abscisses, une par spectre, avec leurs noms", length(s) == 2 && identical(vapply(s, `[[`, "", "nom"), c("deux lot A", "deux lot B")) &&
         proche(s[[2]]$y, c(0.2, 0.25, 0.3)) && proche(s[[1]]$x, 400:402))
s <- lire_spectres(fichier("virgule.txt", c("400;0,1", "401;0,15")))
verifier("sans en-tête, point-virgule et virgule décimale", length(s) == 1 && s[[1]]$nom == "virgule" && proche(s[[1]]$y, c(0.1, 0.15)))
s <- lire_spectres(fichier("appareil.txt", c("Appareil : NIR-1", "Date : 2026-11-01", "", "Longueur d'onde\tAbsorbance", "1100\t0.5", "1102\t0.52")))
verifier("les lignes de l'appareil avant les nombres sont ignorées", length(s) == 1 && proche(s[[1]]$x, c(1100, 1102)) && proche(s[[1]]$y, c(0.5, 0.52)))
s <- lire_spectres(fichier("espaces.dat", c("  400   1.5e-1", "  401   2.5E-1")))
verifier("séparés par des espaces, en notation scientifique", proche(s[[1]]$y, c(0.15, 0.25)))

cat("Fichiers abîmés\n")
# le temps (s) et la mémoire prise au plus fort (Mo, en plus de celle déjà prise) d'une lecture
mesure <- function(expr) {
  avant <- sum(gc(reset = TRUE)[, 2])
  temps <- system.time(r <- tryCatch(expr, error = conditionMessage))[["elapsed"]]
  g <- gc()
  list(r = r, temps = temps, memoire = sum(g[, ncol(g)]) - avant)
}
# refusé en moins d'une seconde, avec moins de 50 Mo, par un message qui nomme le fichier
refuse <- function(f, motif) {
  m <- mesure(lire_spectres(f))
  ok <- is.character(m$r) && grepl(motif, m$r) && grepl(basename(f), m$r, fixed = TRUE) && m$temps < 1 && m$memoire < 50
  if (!ok) cat(sprintf("          %s (%.2f s, %.0f Mo)\n", if (is.character(m$r)) m$r else "lu sans erreur", m$temps, m$memoire))
  ok
}
jdx_xy <- function(nom, npoints, donnees) fichier(nom, c("##TITLE= x", "##JCAMP-DX= 4.24", "##FIRSTX= 400", "##LASTX= 401",
                                                        if (length(npoints)) paste("##NPOINTS=", npoints), "##XYDATA= (X++(Y..Y))", donnees, "##END="))
# « s999999 » : la valeur répétée 9999999 fois en tout
verifier("JCAMP-DX : une répétition (DUP) de près de 10 millions de valeurs pour NPOINTS = 2 s'arrête à NPOINTS",
         refuse(jdx_xy("dup.jdx", 2, "400 A1 s999999"), "plus de valeurs que NPOINTS = 2"))
verifier("JCAMP-DX sans NPOINTS : la même répétition dépasse le million de valeurs permis à un petit fichier",
         refuse(jdx_xy("sans_npoints.jdx", NULL, "400 A1 s999999"), "trop pour la taille du fichier"))
m <- mesure(lire_jcamp(jdx_xy("longue.jdx", 20001, paste0("400 A1", strrep("J", 20000)))))
verifier("JCAMP-DX : une ligne de 20000 écarts (DIF) se lit en un temps proportionnel à sa longueur",
         !is.character(m$r) && proche(m$r[[1]]$y, 11:20011) && m$temps < 1 && m$memoire < 50)
verifier("SPC : 10 millions de points annoncés dans un fichier de 512 octets",
         refuse(spc("npts.spc", entete_spc(0, -128, 1e7, 400, 2500)), "tronqué ou abîmé"))
verifier("SPC : 100000 sous-fichiers annoncés, aucun dans le fichier (sans boucler)",
         refuse(spc("nsub.spc", entete_spc(4, -128, 1, 400, 2500, nsub = 100000)), "tronqué ou abîmé"))
verifier("SPC : un sous-fichier à ses propres abscisses en annonce 5 millions",
         refuse(spc("sous.spc", entete_spc(128 + 64 + 4, -128, 0, 1, 9, nsub = 1), sous_entete(-128, 5e6), octets(1:4, 4, FALSE)), "tronqué ou abîmé"))
verifier("SPC ancien format : 10 millions de points annoncés",
         refuse(spc("ancien_npts.spc", { o <- ancien; o[5:8] <- octets(1e7, 4, FALSE); o }), "tronqué ou abîmé"))
verifier("texte : une première ligne de 4000 nombres puis 4000 lignes de 2 (une matrice presque vide)",
         refuse(fichier("inegal.txt", c(paste(1:4000, collapse = " "), rep("1 2", 4000))), "longueurs trop différentes"))
verifier("un spectre d'un seul point : un message qui nomme le fichier",
         refuse(fichier("un_point.jdx", c("##TITLE= x", "##JCAMP-DX= 4.24", "##XYDATA= (XY..XY)", "400, 0.5", "##END=")), "moins de deux valeurs"))
verifier("NPOINTS qui n'est pas un nombre (« trois ») : un message qui nomme le fichier",
         refuse(jdx_xy("trois.jdx", "trois", "400 1 2"), "NPOINTS n'est pas un nombre entier"))

cat("Tableau des spectres\n")
a <- list(nom = "a", x = c(400, 402, 404), y = c(1, 2, 3), unite_x = "NANOMETERS", unite_y = "")
b <- list(nom = "b", x = c(400, 402, 404), y = c(4, 5, 6), unite_x = "", unite_y = "")
t1 <- tableau_spectres(list(a, b))
verifier("mêmes abscisses : une ligne par spectre, les colonnes nommées par leur abscisse",
         identical(names(t1), c("Observation", "400", "402", "404")) && identical(t1$Observation, c("a", "b")) && proche(t1[["404"]], c(3, 6)) && is.null(attr(t1, "note")))
c2 <- list(nom = "a", x = c(405, 403, 401, 399), y = 2 * c(405, 403, 401, 399), unite_x = "", unite_y = "")
t2 <- tableau_spectres(list(a, c2))
verifier("abscisses différentes : interpolées sur celles du premier, partie commune, avec une note",
         identical(names(t2), c("Observation", "400", "402", "404")) && proche(unlist(t2[2, -1]), c(800, 804, 808)) && grepl("interpolés", attr(t2, "note")) &&
         identical(t2$Observation, c("a", "a 1")))
verifier("des unités différentes sont refusées",
         grepl("unités", tryCatch(tableau_spectres(list(a, modifyList(b, list(unite_x = "1/CM")))), error = conditionMessage)))
verifier("noms des abscisses : une décimale de plus que le pas, sans zéros inutiles",
         identical(noms_abscisses(c(4000.656, 3999.6908, 3998.7265)), c("4000.66", "3999.69", "3998.73")) &&
         identical(noms_abscisses(c(1000.25, 1000.5, 1000.75)), c("1000.25", "1000.5", "1000.75")) && identical(noms_abscisses(c(400, 402)), c("400", "402")))
infos <- data.frame(echantillon = c("A.JDX", "b", "z"), teneur = c(1.5, 2.5, 9), lot = c("x", "y", "z"), stringsAsFactors = FALSE)
j <- joindre_informations(rbind(t1, data.frame(Observation = "c", `400` = 7, `402` = 8, `404` = 9, check.names = FALSE)), infos)
verifier("informations jointes par le nom du fichier (extension et majuscules ignorées), avant les spectres",
         identical(names(j), c("Observation", "teneur", "lot", "400", "402", "404")) && identical(j$teneur, c(1.5, 2.5, NA)) &&
         identical(attr(j, "note"), c("1 spectre(s) sans informations", "1 ligne(s) du tableau d'informations sans spectre")))
verifier("aucun nom en commun : refusé", inherits(try(joindre_informations(t1, data.frame(n = "q", v = 1)), silent = TRUE), "try-error"))
p <- importer_tableau(compose)
verifier("un fichier .jdx s'ouvre comme un tableau : un projet, une variable par abscisse commune (2 à 6)",
         identical(unname(p$noms_obs), c("bloc premier", "bloc second")) && identical(unname(p$noms_var), as.character(2:6)))

unlink(dossier, recursive = TRUE)
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
