# Spectres lus dans les fichiers des spectromètres, réunis en un tableau : une ligne par
# spectre, une colonne par abscisse (longueur d'onde, nombre d'onde...), nommée par sa valeur.
#   .jdx .dx .jcm  JCAMP-DX (McDonald et Wilks, 1988) : tables (X++(Y..Y)) en nombres simples
#                  (AFFN, PAC) ou compressés (ASDF : SQZ, DIF, DUP), tables (XY..XY) ;
#                  plusieurs blocs par fichier. Pas les NTUPLES (RMN complexe, SM) ni les
#                  tables de pics ;
#   .spc           Galactic (GRAMS, Thermo) : nouveau format (octets de poids faible d'abord) et
#                  ancien format ; un ou plusieurs sous-fichiers ; abscisses régulières, ou
#                  données (communes ou propres à chaque sous-fichier) ;
#   texte          (.csv, .txt...) : une colonne d'abscisses, puis une colonne par spectre ; une
#                  ligne d'en-tête ou non ; les lignes de l'appareil avant les nombres sont
#                  ignorées.
# Chaque spectre lu : list(nom, x, y, unite_x, unite_y).

EXTENSIONS_SPECTRES <- c("jdx", "dx", "jcm", "spc")

# Un projet fait des spectres de plusieurs fichiers, avec, si `infos` est donné, un tableau
# d'informations sur les échantillons (concentrations, classes...) joint par le nom des
# fichiers (joindre_informations). p$note : ce qu'il faut en dire (interpolation, noms
# sans correspondance).
importer_spectres <- function(chemins, infos = NULL, nom = NULL) {
  df <- tableau_spectres(unlist(lapply(chemins, lire_spectres), recursive = FALSE))
  if (length(infos) && nzchar(infos)) df <- joindre_informations(df, lire_tableau(infos))
  p <- projet_depuis_tableau(df, nom %||% if (length(chemins) == 1) basename(chemins) else sprintf("Spectres (%d fichiers)", length(chemins)))
  p$note <- attr(df, "note")
  p
}

# Les spectres d'un fichier, nommés d'après le fichier (et le titre du bloc ou le numéro du
# sous-fichier quand il y en a plusieurs).
lire_spectres <- function(chemin) {
  spectres <- switch(extension(chemin),
    jdx = , dx = , jcm = lire_jcamp(chemin),
    spc = lire_spc(chemin),
    lire_spectres_texte(chemin))
  if (!length(spectres)) stop(traduire("Aucun spectre lisible dans ", "No readable spectrum in "), basename(chemin))
  base <- tools::file_path_sans_ext(basename(chemin))
  titres <- vapply(spectres, function(s) s$nom %||% "", "")
  noms <- if (length(spectres) == 1) base
          else if (all(nzchar(titres)) && !anyDuplicated(titres)) paste(base, titres)
          else paste(base, seq_along(spectres))
  for (i in seq_along(spectres)) spectres[[i]]$nom <- noms[i]
  spectres
}

# Des spectres en tableau (data.frame : Observation, puis une colonne par abscisse). Sur les
# mêmes abscisses, les valeurs sont gardées telles quelles ; sinon chaque spectre est interpolé
# (linéairement) sur les abscisses du premier, dans la partie commune à tous, ce que dit
# l'attribut "note". Les spectres doivent avoir les mêmes unités, quand leurs fichiers les
# donnent.
tableau_spectres <- function(spectres) {
  if (!length(spectres)) stop(traduire("Aucun spectre", "No spectrum"))
  for (cle in c("unite_x", "unite_y")) {
    u <- unique(Filter(nzchar, vapply(spectres, function(s) s[[cle]] %||% "", "")))
    if (length(u) > 1) stop(sprintf(traduire("Les spectres n'ont pas les mêmes unités (%s) : %s", "The spectra don't have the same units (%s): %s"), if (cle == "unite_x") traduire("abscisses", "x values") else traduire("ordonnées", "y values"), paste(u, collapse = ", ")))
  }
  x <- spectres[[1]]$x
  pas <- stats::median(abs(diff(x)))
  meme_grille <- function(s) length(s$x) == length(x) && max(abs(s$x - x)) <= 1e-3 * pas
  note <- NULL
  if (!all(vapply(spectres, meme_grille, TRUE))) {
    bas <- max(vapply(spectres, function(s) min(s$x), 0))
    haut <- min(vapply(spectres, function(s) max(s$x), 0))
    x <- x[x >= bas - 1e-6 * pas & x <= haut + 1e-6 * pas]
    if (length(x) < 2) stop(traduire("Les spectres n'ont pas d'abscisses en commun", "The spectra have no x values in common"))
    spectres <- lapply(spectres, function(s) {
      s$y <- stats::approx(s$x, s$y, xout = x, ties = mean)$y
      s
    })
    note <- sprintf(traduire("Abscisses différentes d'un spectre à l'autre : spectres interpolés sur celles du premier, de %s à %s (%d valeurs)", "Different x values from one spectrum to another: spectra interpolated onto the first one's, from %s to %s (%d values)"),
                    format(x[1]), format(x[length(x)]), length(x))
  }
  Y <- matrix(unlist(lapply(spectres, `[[`, "y")), length(spectres), length(x), byrow = TRUE)
  df <- data.frame(Observation = make.unique(vapply(spectres, `[[`, "", "nom"), sep = " "), stringsAsFactors = FALSE)
  df <- cbind(df, stats::setNames(as.data.frame(Y), noms_abscisses(x)))
  attr(df, "note") <- note
  df
}

# Noms des colonnes : les abscisses avec une décimale de plus que n'en demande le plus petit
# pas (pas de 2 : « 400 » ; de 0,96 : « 3999.69 »), sans zéros inutiles, et plus s'il le faut
# pour qu'elles soient toutes différentes.
noms_abscisses <- function(x) {
  ecarts <- abs(diff(x))
  pas <- min(ecarts[ecarts > 0], Inf)
  decimales <- if (is.finite(pas)) max(0, ceiling(1 - log10(pas))) else 0
  repeat {
    noms <- formatC(x, format = "f", digits = decimales)
    if (decimales > 0) noms <- sub("\\.$", "", sub("0+$", "", noms))
    if (!anyDuplicated(noms) || decimales >= 15) break
    decimales <- decimales + 1
  }
  make.unique(noms, sep = "_")
}

# Les informations sur les échantillons (concentrations, classes...) d'un autre tableau, dont
# la première colonne donne le nom de chaque spectre (le nom du fichier, avec ou sans son
# extension ; majuscules et minuscules confondues). Elles prennent place avant les spectres.
joindre_informations <- function(df, infos) {
  cle <- function(s) tolower(trimws(sub("\\.(jdx|dx|jcm|spc|csv|txt|tsv)$", "", as.character(s), ignore.case = TRUE)))
  i <- match(cle(df$Observation), cle(infos[[1]]))
  if (all(is.na(i))) stop(traduire("Aucun nom de la première colonne du tableau d'informations ne correspond à un spectre (le nom de son fichier)", "No name in the first column of the information table matches a spectrum (its file name)"))
  ajout <- infos[i, -1, drop = FALSE]
  rownames(ajout) <- NULL
  res <- cbind(df[1], ajout, df[-1])
  names(res) <- make.unique(names(res), sep = "_")
  note <- c(if (anyNA(i)) sprintf(traduire("%d spectre(s) sans informations", "%d spectrum(s) without information"), sum(is.na(i))),
            if (any(!seq_len(nrow(infos)) %in% i)) sprintf(traduire("%d ligne(s) du tableau d'informations sans spectre", "%d row(s) of the information table without a spectrum"), sum(!seq_len(nrow(infos)) %in% i)))
  attr(res, "note") <- c(attr(df, "note"), note)
  res
}

# Unités, écrites comme JCAMP-DX les écrit le plus souvent.
unite_spectre <- function(u) {
  u <- toupper(gsub("\\s", "", u %||% ""))
  if (u %in% c("1/CM", "CM-1", "CM^-1", "WAVENUMBERS")) return("1/CM")
  if (u %in% c("NM", "NANOMETERS")) return("NANOMETERS")
  if (u %in% c("UM", "MICRONS", "MICROMETERS")) return("MICROMETERS")
  u
}

# ---------------------------------------------------------------- JCAMP-DX

# Chaque table de données d'un fichier (après les étiquettes « ##CLÉ= valeur » de son bloc ;
# un « ##TITLE= » commence un bloc). Les clés s'écrivent sans espaces, tirets, barres ni
# soulignés, en majuscules ; « $$ » commence un commentaire.
lire_jcamp <- function(chemin) {
  lignes <- sub("\\$\\$.*$", "", strsplit(texte_fichier(chemin), "\r\n|\r|\n")[[1]])
  etiquette <- grepl("^\\s*##", lignes)
  spectres <- list()
  bloc <- list()
  for (i in which(etiquette)) {
    m <- regmatches(lignes[i], regexec("^\\s*##([^=]*)=(.*)$", lignes[i]))[[1]]
    if (!length(m)) next
    cle <- gsub("[ _/-]", "", toupper(m[2]))
    if (cle == "TITLE") bloc <- list()
    bloc[[cle]] <- trimws(m[3])
    if (cle %in% c("XYDATA", "XYPOINTS")) {
      fin <- match(TRUE, etiquette[-seq_len(i)], nomatch = length(lignes) - i + 1) - 1
      s <- table_jcamp(bloc[[cle]], lignes[i + seq_len(fin)], bloc)
      if (!is.null(s)) spectres[[length(spectres) + 1]] <- s
    }
  }
  spectres
}

table_jcamp <- function(forme, lignes, bloc) {
  nombre <- function(cle, defaut = NA_real_) if (is.null(bloc[[cle]])) defaut else as.numeric(nombres_affn(bloc[[cle]])[1])
  fx <- nombre("XFACTOR", 1)
  fy <- nombre("YFACTOR", 1)
  forme <- gsub("\\s", "", toupper(forme))
  titre <- bloc$TITLE %||% ""
  if (forme == "(X++(Y..Y))") {
    y <- valeurs_x_y(lignes, titre) * fy
    n <- nombre("NPOINTS", length(y))
    if (length(y) != n) stop(sprintf(traduire("JCAMP-DX « %s » : %d valeurs lues pour NPOINTS = %d", "JCAMP-DX “%s”: %d values read for NPOINTS = %d"), titre, length(y), n))
    if (is.na(nombre("FIRSTX")) || is.na(nombre("LASTX"))) stop(sprintf(traduire("JCAMP-DX « %s » : FIRSTX ou LASTX manque", "JCAMP-DX “%s”: FIRSTX or LASTX missing"), titre))
    x <- seq(nombre("FIRSTX"), nombre("LASTX"), length.out = n)
  } else if (forme == "(XY..XY)") {
    v <- nombres_affn(paste(lignes, collapse = " "))
    if (length(v) %% 2) stop(sprintf(traduire("JCAMP-DX « %s » : la table (XY..XY) a un nombre impair de valeurs", "JCAMP-DX “%s”: the (XY..XY) table has an odd number of values"), titre))
    x <- v[c(TRUE, FALSE)] * fx
    y <- v[c(FALSE, TRUE)] * fy
  } else {
    return(NULL)
  }
  list(nom = titre, x = x, y = y, unite_x = unite_spectre(bloc$XUNITS), unite_y = unite_spectre(bloc$YUNITS))
}

# Nombres écrits simplement (AFFN : séparés par des espaces ou des virgules ; PAC : par leur
# signe) ; « ? » = valeur manquante.
nombres_affn <- function(texte) {
  v <- regmatches(texte, gregexpr("[+-]?(\\d+\\.?\\d*|\\.\\d+)([Ee][+-]?\\d+)?|\\?", texte))[[1]]
  suppressWarnings(as.numeric(v))
}

# Les ordonnées d'une table (X++(Y..Y)) : chaque ligne donne l'abscisse de sa première valeur
# puis des ordonnées. Compression ASDF : un caractère remplace le signe et le premier chiffre
#   SQZ (valeur)            @ A-I = 0 à 9   a-i = -1 à -9
#   DIF (écart à la précédente)  % J-R = 0 à 9   j-r = -1 à -9
#   DUP (répétition : la valeur, ou l'écart, compte n fois en tout)  S-Z = 1 à 8, s = 9
# Une ligne qui suit une ligne finie en DIF commence par une valeur de contrôle : la dernière
# de la ligne précédente, qui n'est pas comptée deux fois (un DUP qui la suit répète bien une
# valeur de plus).
valeurs_x_y <- function(lignes, titre = "") {
  lignes <- lignes[nzchar(trimws(lignes))]
  if (!any(grepl("[@A-DF-Ia-df-iJ-Rj-r%S-Zs]", lignes))) {  # pas de compression
    return(unlist(lapply(lignes, function(l) nombres_affn(l)[-1])))
  }
  chiffre <- c(stats::setNames(0:9, strsplit("@ABCDEFGHI", "")[[1]]), stats::setNames(-(1:9), strsplit("abcdefghi", "")[[1]]),
               stats::setNames(0:9, strsplit("%JKLMNOPQR", "")[[1]]), stats::setNames(-(1:9), strsplit("jklmnopqr", "")[[1]]),
               stats::setNames(1:9, strsplit("STUVWXYZs", "")[[1]]))
  genre <- c(stats::setNames(rep("sqz", 19), strsplit("@ABCDEFGHIabcdefghi", "")[[1]]),
             stats::setNames(rep("dif", 19), strsplit("%JKLMNOPQRjklmnopqr", "")[[1]]),
             stats::setNames(rep("dup", 9), strsplit("STUVWXYZs", "")[[1]]))
  resultat <- list()
  fin_dif <- FALSE
  derniere <- NA_real_
  for (l in lignes) {
    jetons <- regmatches(l, gregexpr("[@A-Ia-iJ-Rj-r%S-Zs+-][0-9.]*|[0-9.]+|\\?", l))[[1]][-1]  # sans l'abscisse
    tete <- substr(jetons, 1, 1)
    reste <- substring(jetons, 2)
    v <- numeric(0)
    precedent <- "sqz"
    ecart <- 0
    for (k in seq_along(jetons)) {
      g <- genre[tete[k]]
      if (is.na(g)) {  # nombre simple, ou « ? »
        v <- c(v, suppressWarnings(as.numeric(jetons[k])))
        precedent <- "sqz"
        next
      }
      n <- as.numeric(paste0(abs(chiffre[[tete[k]]]), reste[k])) * (if (chiffre[[tete[k]]] < 0) -1 else 1)
      if (g == "sqz") {
        v <- c(v, n)
        precedent <- "sqz"
      } else if (g == "dif") {
        ecart <- n
        v <- c(v, v[length(v)] + ecart)
        precedent <- "dif"
      } else {
        v <- c(v, if (precedent == "dif") v[length(v)] + ecart * seq_len(n - 1) else rep(v[length(v)], n - 1))
      }
    }
    if (fin_dif && length(v)) {
      # une dernière ligne d'une seule valeur, fausse, ferme la table chez certains logiciels
      if (length(v) == 1 && identical(l, lignes[length(lignes)]) && !isTRUE(all.equal(v[1], derniere))) break
      if (!isTRUE(all.equal(v[1], derniere))) stop(sprintf(traduire("JCAMP-DX « %s » : valeur de contrôle %s au lieu de %s (ligne « %s »)", "JCAMP-DX “%s”: check value %s instead of %s (line “%s”)"), titre, format(v[1]), format(derniere), substr(l, 1, 40)))
      v <- v[-1]
    }
    fin_dif <- precedent == "dif"
    if (length(v)) derniere <- v[length(v)]
    resultat[[length(resultat) + 1]] <- v
  }
  unlist(resultat)
}

# ---------------------------------------------------------------- SPC

# Codes des unités du format SPC (fxtype, fytype) que JCAMP-DX écrit aussi.
UNITES_SPC_X <- c("1" = "1/CM", "2" = "MICROMETERS", "3" = "NANOMETERS", "13" = "1/CM")
UNITES_SPC_Y <- c("2" = "ABSORBANCE", "10" = "LOG(1/R)", "128" = "TRANSMITTANCE", "129" = "REFLECTANCE")

lire_spc <- function(chemin) {
  o <- readBin(chemin, "raw", file.size(chemin))
  entiers <- function(pos, n, taille) readBin(o[pos + seq_len(n * taille)], "integer", n = n, size = taille, signed = TRUE, endian = "little")
  reels <- function(pos, n, taille) readBin(o[pos + seq_len(n * taille)], "double", n = n, size = taille, endian = "little")
  drapeaux <- as.integer(o[1])
  bit <- function(b) bitwAnd(drapeaux, b) > 0
  seize <- bit(1)    # TSPREC : ordonnées en entiers de 16 bits
  multi <- bit(4)    # TMULTI : plusieurs sous-fichiers
  xyxy <- bit(64)    # TXYXYS : des abscisses par sous-fichier
  xval <- bit(128)   # TXVALS : abscisses données
  version <- as.integer(o[2])
  unite <- function(table, code) unname(table[as.character(code)] %|NA|% "")
  # ordonnées d'un sous-fichier : réels (exposant -128), sinon entiers * 2^(exposant - 32 ou - 16)
  ordonnees <- function(pos, n, exposant, mots_inverses = FALSE) {
    if (exposant == -128) return(list(y = reels(pos, n, 4), taille = 4 * n))
    if (seize) return(list(y = entiers(pos, n, 2) * 2^(exposant - 16), taille = 2 * n))
    b <- o[pos + seq_len(4 * n)]
    if (mots_inverses) b <- b[as.vector(matrix(seq_len(4 * n), 4)[c(3, 4, 1, 2), ])]  # ancien format : mot de poids fort d'abord
    list(y = readBin(b, "integer", n = n, size = 4, endian = "little") * 2^(exposant - 32), taille = 4 * n)
  }
  spectres <- list()
  if (version == 0x4B) {
    exposant <- entiers(3, 1, 1)
    npts <- entiers(4, 1, 4)
    premier <- reels(8, 1, 8)
    dernier <- reels(16, 1, 8)
    nsub <- if (multi) entiers(24, 1, 4) else 1
    ux <- unite(UNITES_SPC_X, as.integer(o[29]))
    uy <- unite(UNITES_SPC_Y, as.integer(o[30]))
    pos <- 512
    x <- if (xval && !xyxy) reels(pos, npts, 4) else seq(premier, dernier, length.out = npts)
    if (xval && !xyxy) pos <- pos + 4 * npts
    for (s in seq_len(nsub)) {
      exp_sous <- entiers(pos + 1, 1, 1)
      n <- if (xyxy) entiers(pos + 16, 1, 4) else npts
      pos <- pos + 32
      if (xyxy) {
        x <- reels(pos, n, 4)
        pos <- pos + 4 * n
      }
      r <- ordonnees(pos, n, if (multi) exp_sous else exposant)
      pos <- pos + r$taille
      spectres[[s]] <- list(x = x, y = r$y, unite_x = ux, unite_y = uy)
    }
  } else if (version == 0x4D) {
    exposant <- entiers(2, 1, 2)
    npts <- as.integer(round(reels(4, 1, 4)))
    x <- seq(reels(8, 1, 4), reels(12, 1, 4), length.out = npts)
    ux <- unite(UNITES_SPC_X, as.integer(o[17]))
    uy <- unite(UNITES_SPC_Y, as.integer(o[18]))
    pos <- 256
    s <- 1
    while (pos < length(o)) {
      e <- exposant
      if (s > 1) {  # le premier sous-fichier a son en-tête dans celui du fichier
        e <- entiers(pos + 1, 1, 1)
        pos <- pos + 32
      }
      r <- ordonnees(pos, npts, e, mots_inverses = TRUE)
      if (pos + r$taille > length(o)) break
      pos <- pos + r$taille
      spectres[[s]] <- list(x = x, y = r$y, unite_x = ux, unite_y = uy)
      if (!multi) break
      s <- s + 1
    }
  } else {
    stop(sprintf(traduire("Fichier SPC de version %X non lu (seuls le nouveau format 4B et l'ancien format 4D le sont)", "SPC file of version %X not read (only the new format 4B and the old format 4D are)"), version))
  }
  spectres
}

`%|NA|%` <- function(a, b) if (length(a) != 1 || is.na(a)) b else a

# ---------------------------------------------------------------- texte

# Une colonne d'abscisses puis une colonne par spectre. Séparateur : tabulation, point-virgule,
# virgule (le point est alors la décimale) ou espaces ; avec un autre séparateur que la
# virgule, la virgule peut être la décimale. Les lignes avant la première ligne de nombres
# sont ignorées, sauf la dernière quand elle a autant de colonnes : elle donne les noms.
lire_spectres_texte <- function(chemin) {
  lignes <- trimws(strsplit(texte_fichier(chemin), "\r\n|\r|\n")[[1]])
  lignes <- lignes[nzchar(lignes)]
  decouper <- function(l, sep) if (sep == " ") strsplit(l, "\\s+")[[1]] else trimws(strsplit(l, sep, fixed = TRUE)[[1]])
  nombres <- function(champs, sep) suppressWarnings(as.numeric(if (sep == ",") champs else sub(",", ".", champs, fixed = TRUE)))
  separateur <- function(l) if (grepl("\t", l)) "\t" else if (grepl(";", l)) ";" else if (grepl(",", l) && (grepl(".", l, fixed = TRUE) || !grepl("\\s", l))) "," else " "
  est_donnee <- vapply(lignes, function(l) {
    sep <- separateur(l)
    v <- nombres(decouper(l, sep), sep)
    length(v) >= 2 && !anyNA(v)
  }, TRUE)
  if (!any(est_donnee)) stop(traduire("Aucune ligne de nombres dans ", "No line of numbers in "), basename(chemin))
  debut <- which(est_donnee)[1]
  donnees <- lignes[debut:length(lignes)]
  donnees <- donnees[est_donnee[debut:length(lignes)]]
  sep <- separateur(donnees[1])
  M <- do.call(rbind, lapply(donnees, function(l) nombres(decouper(l, sep), sep)[seq_len(length(decouper(donnees[1], sep)))]))
  noms <- if (debut > 1 && length(decouper(lignes[debut - 1], sep)) == ncol(M)) decouper(lignes[debut - 1], sep)[-1] else rep("", ncol(M) - 1)
  lapply(seq_len(ncol(M) - 1), function(j) list(nom = gsub("^\"|\"$", "", noms[j]), x = M[, 1], y = M[, j + 1], unite_x = "", unite_y = ""))
}
