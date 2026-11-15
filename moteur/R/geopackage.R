# Les fichiers GeoPackage (.gpkg, OGC 12-128r18, le format de QGIS) : lus comme un tableau
# (Fichier › Ouvrir ; une couche au choix) dont les géométries donnent deux colonnes de
# coordonnées et le système du projet, et écrits (Exporter) en couche de points, une par
# observation qui a ses coordonnées, avec toutes ses colonnes. Un GeoPackage est une base SQLite,
# lue et écrite avec le paquet RSQLite ; les géométries (un en-tête GeoPackage suivi d'une
# géométrie WKB) sont lues et écrites ici.
#
# Une couche de points garde leurs coordonnées ; une couche de lignes ou de polygones donne le
# centre de chaque entité (le centre de gravité de la surface d'un polygone, trous retirés, celui
# des segments d'une ligne, la moyenne des points d'un multipoint), et la lecture le dit.

exiger_rsqlite <- function() {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop(traduire("Les fichiers GeoPackage demandent le paquet R RSQLite : install.packages(\"RSQLite\") (sous Debian ou Ubuntu : sudo apt install r-cran-rsqlite).",
                  "GeoPackage files need the R package RSQLite: install.packages(\"RSQLite\") (on Debian or Ubuntu: sudo apt install r-cran-rsqlite)."), call. = FALSE)
  }
}

ouvrir_gpkg <- function(chemin) {
  exiger_rsqlite()
  RSQLite::dbConnect(RSQLite::SQLite(), chemin, flags = RSQLite::SQLITE_RO)
}

# Les couches d'un GeoPackage (ses entités, puis ses tables sans géométrie).
couches_geopackage <- function(chemin) {
  con <- ouvrir_gpkg(chemin)
  on.exit(RSQLite::dbDisconnect(con))
  if (!"gpkg_contents" %in% RSQLite::dbListTables(con)) stop(traduire("Ce fichier n'est pas un GeoPackage (il n'a pas de table gpkg_contents).", "This file isn't a GeoPackage (it has no gpkg_contents table)."), call. = FALSE)
  c <- RSQLite::dbGetQuery(con, "SELECT table_name, data_type FROM gpkg_contents WHERE data_type IN ('features', 'attributes') ORDER BY data_type DESC, table_name")
  as.character(c$table_name)
}

# ---------------------------------------------------------------- géométries WKB

# Le système d'un srs_id du fichier : son code EPSG s'il est de ceux que Nuée connaît
# (spatial.R : systeme_coordonnees), sinon « plan » (coordonnées gardées telles quelles), et ce
# qu'il faut en dire.
systeme_srs <- function(con, srs_id) {
  s <- RSQLite::dbGetQuery(con, "SELECT srs_name, organization, organization_coordsys_id, definition FROM gpkg_spatial_ref_sys WHERE srs_id = ?", params = list(srs_id))
  if (!nrow(s)) return(list(systeme = "plan", note = NULL))
  code <- s$organization_coordsys_id[1]
  if (toupper(as.character(s$organization[1])) == "EPSG" && !is.null(systeme_coordonnees(code))) return(list(systeme = code, note = NULL))
  geographique <- grepl("^\\s*(GEOGCS|GEOGCRS|GEODCRS)", as.character(s$definition[1]))
  list(systeme = if (geographique) 4326 else "plan",
       note = sprintf(traduire("Le système de coordonnées du fichier (%s) n'est pas de ceux que Nuée convertit : %s.",
                               "The file's coordinate system (%s) isn't one Nuée converts: %s."), s$srs_name[1],
                      if (geographique) traduire("ses longitudes et latitudes sont prises pour du WGS 84", "its longitudes and latitudes are taken as WGS 84")
                      else traduire("ses coordonnées sont gardées telles quelles, sans fond de carte", "its coordinates are kept as they are, without a background map")))
}

# Le centre d'une géométrie GeoPackage (un vecteur raw) : c(x, y, genre), genre 1 un point,
# 2 une ligne, 3 un polygone, 4 un multipoint... ; NA pour une géométrie vide ou d'un autre genre.
centre_geometrie <- function(r) {
  if (is.null(r) || length(r) < 8 || r[1] != as.raw(0x47) || r[2] != as.raw(0x50)) return(c(NA, NA, NA))
  drapeaux <- as.integer(r[4])
  if (bitwAnd(drapeaux, 16L)) return(c(NA, NA, NA))  # (géométrie vide)
  enveloppe <- c(0, 32, 48, 48, 64)[bitwAnd(bitwShiftR(drapeaux, 1L), 7L) + 1]
  pos <- 9 + enveloppe
  lire_entier <- function(fin) { v <- readBin(r[pos:(pos + 3)], "integer", size = 4, endian = fin); pos <<- pos + 4; v }
  lire_reels <- function(n, fin) { v <- readBin(r[pos:(pos + 8 * n - 1)], "double", n = n, size = 8, endian = fin); pos <<- pos + 8 * n; v }
  # (une géométrie WKB, éventuellement dans une autre : son genre, ses dimensions, ses points)
  geometrie <- function() {
    fin <- if (r[pos] == as.raw(1)) "little" else "big"
    pos <<- pos + 1
    type <- lire_entier(fin)
    if (type < 0) type <- type + 2^32
    if (type >= 2^29) {  # (EWKB : Z, M et SRID en drapeaux dans les bits de poids fort)
      z <- (type %/% 2^31) %% 2 == 1
      m <- (type %/% 2^30) %% 2 == 1
      if ((type %/% 2^29) %% 2 == 1) pos <<- pos + 4
      genre <- type %% 2^29
    } else {  # (ISO : 1001 avec Z, 2001 avec M, 3001 avec les deux)
      z <- type %/% 1000 %in% c(1, 3)
      m <- type %/% 1000 %in% c(2, 3)
      genre <- type %% 1000
    }
    dims <- 2L + z + m
    anneau <- function() {
      n <- lire_entier(fin)
      matrix(lire_reels(n * dims, fin), ncol = dims, byrow = TRUE)[, 1:2, drop = FALSE]
    }
    switch(as.character(genre),
      "1" = list(genre = 1L, points = matrix(lire_reels(dims, fin)[1:2], 1)),
      "2" = list(genre = 2L, lignes = list(anneau())),
      "3" = { k <- lire_entier(fin); list(genre = 3L, polygones = list(lapply(seq_len(k), function(i) anneau()))) },
      "4" = , "5" = , "6" = , "7" = {
        k <- lire_entier(fin)
        parties <- lapply(seq_len(k), function(i) geometrie())
        list(genre = as.integer(genre), points = do.call(rbind, lapply(parties, `[[`, "points")),
             lignes = do.call(c, lapply(parties, `[[`, "lignes")), polygones = do.call(c, lapply(parties, `[[`, "polygones")))
      },
      stop("géométrie WKB inconnue"))
  }
  g <- tryCatch(geometrie(), error = function(e) NULL)
  if (is.null(g)) return(c(NA, NA, NA))
  if (length(g$polygones)) {
    # (le centre de gravité de la surface : Σ aire × centre de chaque anneau, les trous comptés
    # en négatif, formule du lacet)
    A <- cx <- cy <- 0
    for (p in g$polygones) for (k in seq_along(p)) {
      v <- p[[k]]
      if (nrow(v) < 3) next
      x0 <- v[-nrow(v), 1]; y0 <- v[-nrow(v), 2]; x1 <- v[-1, 1]; y1 <- v[-1, 2]
      c0 <- x0 * y1 - x1 * y0
      a <- sum(c0) / 2
      signe <- if (k == 1) 1 else -1
      if (a == 0) next
      A <- A + signe * abs(a)
      cx <- cx + signe * abs(a) * sum((x0 + x1) * c0) / (6 * a)
      cy <- cy + signe * abs(a) * sum((y0 + y1) * c0) / (6 * a)
    }
    if (A > 0) return(c(cx / A, cy / A, g$genre))
  }
  if (length(g$lignes)) {
    L <- cx <- cy <- 0
    for (v in g$lignes) if (nrow(v) > 1) {
      l <- sqrt(diff(v[, 1])^2 + diff(v[, 2])^2)
      L <- L + sum(l)
      cx <- cx + sum(l * (v[-1, 1] + v[-nrow(v), 1]) / 2)
      cy <- cy + sum(l * (v[-1, 2] + v[-nrow(v), 2]) / 2)
    }
    if (L > 0) return(c(cx / L, cy / L, g$genre))
  }
  if (length(g$points)) return(c(colMeans(g$points), g$genre))
  c(NA, NA, NA)
}

# La géométrie d'un point (x, y) d'un srs_id, telle que GeoPackage l'écrit : « GP », la version 0,
# les drapeaux (petit-boutiste, sans enveloppe), le srs_id, puis le point en WKB ; NULL pour un
# point sans coordonnées.
geometrie_point <- function(x, y, srs_id) {
  if (!is.finite(x) || !is.finite(y)) return(NULL)
  c(as.raw(c(0x47, 0x50, 0, 1)), writeBin(as.integer(srs_id), raw(), size = 4, endian = "little"),
    as.raw(1), writeBin(1L, raw(), size = 4, endian = "little"), writeBin(c(x, y), raw(), size = 8, endian = "little"))
}

# ---------------------------------------------------------------- lecture

# Une couche d'un GeoPackage en data.frame : ses colonnes, puis deux colonnes de coordonnées (le
# centre de chaque entité ; longitude et latitude pour un système géographique), leur système en
# attr(, "coordonnees") et ce qu'il faut dire de la lecture en attr(, "note").
lire_geopackage <- function(chemin, couche = NULL) {
  con <- ouvrir_gpkg(chemin)
  on.exit(RSQLite::dbDisconnect(con))
  couches <- couches_geopackage(chemin)
  if (!length(couches)) stop(traduire("Ce GeoPackage n'a aucune couche d'entités ni table.", "This GeoPackage has no feature layer and no table."), call. = FALSE)
  couche <- couche %||% couches[1]
  if (!couche %in% couches) stop(traduire("Couche introuvable dans le fichier : ", "Layer not found in the file: "), couche, call. = FALSE)
  colonne_geom <- RSQLite::dbGetQuery(con, "SELECT column_name, srs_id FROM gpkg_geometry_columns WHERE table_name = ?", params = list(couche))
  df <- RSQLite::dbGetQuery(con, sprintf("SELECT * FROM %s", DBI::dbQuoteIdentifier(con, couche)))
  df[["fid"]] <- NULL
  note <- character(0)
  if (!nrow(colonne_geom)) return(structure(df, note = note))
  g <- as.character(colonne_geom$column_name[1])
  blobs <- df[[g]]
  df[[g]] <- NULL
  centres <- if (length(blobs)) do.call(rbind, lapply(blobs, centre_geometrie)) else matrix(numeric(0), 0, 3)
  s <- systeme_srs(con, colonne_geom$srs_id[1])
  geo <- !identical(s$systeme, "plan") && systeme_coordonnees(s$systeme)$type == "lonlat"
  noms <- make.unique(c(names(df), if (geo) c("longitude", "latitude") else c("X", "Y")), sep = "_")
  nx <- noms[ncol(df) + 1]
  ny <- noms[ncol(df) + 2]
  df[[nx]] <- centres[, 1]
  df[[ny]] <- centres[, 2]
  genres <- centres[, 3]
  if (any(genres %in% c(2, 3, 5, 6), na.rm = TRUE)) {
    note <- c(note, sprintf(traduire("%d entités de lignes ou de polygones : leur centre donne « %s » et « %s ».", "%d line or polygon features: their centre gives “%s” and “%s”."),
                            sum(genres %in% c(2, 3, 5, 6)), nx, ny))
  }
  if (any(is.na(genres))) note <- c(note, sprintf(traduire("%d entités sans géométrie lisible : sans coordonnées.", "%d features without a readable geometry: without coordinates."), sum(is.na(genres))))
  note <- c(note, s$note)
  structure(df, note = stats::setNames(note, rep("", length(note))), coordonnees = list(x = nx, y = ny, systeme = s$systeme))
}

# ---------------------------------------------------------------- écriture

# Le texte WKT (OGC 01-009, celui de GeoPackage 1.0 à 1.3) d'un système de systeme_coordonnees.
wkt_systeme <- function(s) {
  el <- ELLIPSOIDES[[s$ellipsoide]]
  sph <- sprintf('SPHEROID["%s",%s,%s]', el$nom, format(el$a, digits = 15), format(1 / el$f, digits = 15))
  datum <- switch(s$ellipsoide,
    WGS84 = sprintf('DATUM["WGS_1984",%s,AUTHORITY["EPSG","6326"]]', sph),
    GRS80 = if (identical(as.numeric(s$code), 4258) || s$code %in% 25828:25838 || s$code == 3035) sprintf('DATUM["European_Terrestrial_Reference_System_1989",%s,AUTHORITY["EPSG","6258"]]', sph)
            else sprintf('DATUM["Reseau_Geodesique_Francais_1993",%s,AUTHORITY["EPSG","6171"]]', sph),
    CLARKE_IGN = sprintf('DATUM["Nouvelle_Triangulation_Francaise_Paris",%s,TOWGS84[-168,-60,320,0,0,0,0],AUTHORITY["EPSG","6807"]]', sph))
  geog <- function(nom, code, pm = 'PRIMEM["Greenwich",0]') sprintf('GEOGCS["%s",%s,%s,UNIT["degree",0.0174532925199433],AUTHORITY["EPSG","%d"]]', nom, datum, pm, code)
  base <- switch(s$ellipsoide, WGS84 = geog("WGS 84", 4326), CLARKE_IGN = geog("NTF (Paris)", 4807, 'PRIMEM["Paris",2.33722917]'),
                 GRS80 = if (grepl("ETRS89", s$nom)) geog("ETRS89", 4258) else geog("RGF93 v1", 4171))
  if (s$type == "lonlat") return(base)
  p <- function(nom, v) sprintf('PARAMETER["%s",%s]', nom, format(v, digits = 15))
  corps <- switch(s$type,
    lcc = if (!is.null(s$phi1)) c('PROJECTION["Lambert_Conformal_Conic_2SP"]', p("standard_parallel_1", s$phi1), p("standard_parallel_2", s$phi2),
                                  p("latitude_of_origin", s$phi0), p("central_meridian", s$lambda0), p("false_easting", s$x0), p("false_northing", s$y0))
          else c('PROJECTION["Lambert_Conformal_Conic_1SP"]', p("latitude_of_origin", s$phi0), p("central_meridian", 0),
                 p("scale_factor", s$k0), p("false_easting", s$x0), p("false_northing", s$y0)),
    tm = c('PROJECTION["Transverse_Mercator"]', p("latitude_of_origin", 0), p("central_meridian", s$lambda0), p("scale_factor", s$k0),
           p("false_easting", s$x0), p("false_northing", s$y0)),
    laea = c('PROJECTION["Lambert_Azimuthal_Equal_Area"]', p("latitude_of_center", s$phi0), p("longitude_of_center", s$lambda0),
             p("false_easting", s$x0), p("false_northing", s$y0)),
    merc = c('PROJECTION["Mercator_1SP"]', p("central_meridian", 0), p("scale_factor", 1), p("false_easting", 0), p("false_northing", 0)))
  sprintf('PROJCS["%s",%s,%s,UNIT["metre",1],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","%d"]]',
          s$nom, base, paste(corps, collapse = ","), as.integer(s$code))
}

# Écrit `df` en couche de points `couche` d'un GeoPackage neuf : x, y les coordonnées de chaque
# ligne dans `systeme` (code EPSG ou « plan ») ; `tables` : d'autres tableaux (nommés), écrits à
# côté en tables sans géométrie (« attributes »). Les nombres en REAL, les valeurs logiques en
# INTEGER, le reste en texte ; une ligne sans coordonnées a une géométrie nulle. Le fichier est
# écrit à côté puis prend la place de l'ancien (nouveau.R : ecrire_sans_risque).
ecrire_geopackage <- function(df, chemin, x, y, systeme, couche = "observations", tables = list()) {
  exiger_rsqlite()
  s <- systeme_coordonnees(systeme)
  plan <- s$type == "plan"
  srs <- if (plan) -1L else as.integer(s$code)
  # (des noms de table sans espace ni ponctuation, qu'aucun logiciel ne refuse, tous différents)
  nom_table <- function(n, defaut) {
    n <- gsub("[^[:alnum:]_]+", "_", enc2utf8(as.character(n)))
    if (!nzchar(n) || grepl("^(gpkg|rtree|sqlite)_", n, ignore.case = TRUE)) defaut else n
  }
  noms_tables <- make.unique(c(nom_table(couche, "observations"), vapply(names(tables), nom_table, "", defaut = "tableau")), sep = "_")
  ecrire_sans_risque(chemin, function(f) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), f)
    on.exit(RSQLite::dbDisconnect(con))
    q <- function(sql, ...) RSQLite::dbExecute(con, sql, ...)
    id <- function(n) DBI::dbQuoteIdentifier(con, n)
    q("PRAGMA application_id = 1196444487")  # « GPKG »
    q("PRAGMA user_version = 10300")
    q("CREATE TABLE gpkg_spatial_ref_sys (srs_name TEXT NOT NULL, srs_id INTEGER PRIMARY KEY, organization TEXT NOT NULL, organization_coordsys_id INTEGER NOT NULL, definition TEXT NOT NULL, description TEXT)")
    q("CREATE TABLE gpkg_contents (table_name TEXT NOT NULL PRIMARY KEY, data_type TEXT NOT NULL, identifier TEXT UNIQUE, description TEXT DEFAULT '', last_change DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')), min_x DOUBLE, min_y DOUBLE, max_x DOUBLE, max_y DOUBLE, srs_id INTEGER, CONSTRAINT fk_gc_r_srs_id FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys(srs_id))")
    q("CREATE TABLE gpkg_geometry_columns (table_name TEXT NOT NULL, column_name TEXT NOT NULL, geometry_type_name TEXT NOT NULL, srs_id INTEGER NOT NULL, z TINYINT NOT NULL, m TINYINT NOT NULL, CONSTRAINT pk_geom_cols PRIMARY KEY (table_name, column_name), CONSTRAINT uk_gc_table_name UNIQUE (table_name), CONSTRAINT fk_gc_tn FOREIGN KEY (table_name) REFERENCES gpkg_contents(table_name), CONSTRAINT fk_gc_srs FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys (srs_id))")
    srs_lignes <- list(list("Undefined cartesian SRS", -1L, "NONE", -1L, "undefined", "undefined cartesian coordinate reference system"),
                       list("Undefined geographic SRS", 0L, "NONE", 0L, "undefined", "undefined geographic coordinate reference system"),
                       list("WGS 84 geodetic", 4326L, "EPSG", 4326L, wkt_systeme(systeme_coordonnees(4326)), "longitude/latitude coordinates in decimal degrees on the WGS 84 spheroid"))
    if (!plan && srs != 4326L) srs_lignes <- c(srs_lignes, list(list(s$nom, srs, "EPSG", srs, wkt_systeme(s), s$nom)))
    for (l in srs_lignes) q("INSERT INTO gpkg_spatial_ref_sys VALUES (?, ?, ?, ?, ?, ?)", params = l)
    # une table : fid, la géométrie s'il y en a une, puis les colonnes du tableau
    creer <- function(nom, tab, geom = NULL) {
      noms <- make.unique(enc2utf8(names(tab)), sep = "_")
      reserve <- tolower(noms) %in% c("fid", "geom")
      noms[reserve] <- paste0(noms[reserve], "_")
      type_sql <- function(v) if (is.logical(v)) "INTEGER" else if (is.numeric(v)) "REAL" else "TEXT"
      colonnes <- vapply(seq_along(tab), function(k) sprintf("%s %s", id(noms[k]), type_sql(tab[[k]])), "")
      q(sprintf("CREATE TABLE %s (fid INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL%s%s)", id(nom), if (!is.null(geom)) ", geom POINT" else "",
                paste0(", ", colonnes, collapse = "")))
      valeurs <- lapply(tab, function(v) if (is.factor(v)) as.character(v) else if (is.logical(v)) as.integer(v) else v)
      names(valeurs) <- noms
      if (!is.null(geom)) valeurs$geom <- geom
      if (!length(valeurs) || !nrow(tab)) return(invisible())
      RSQLite::dbExecute(con, sprintf("INSERT INTO %s (%s) VALUES (%s)", id(nom), paste(id(names(valeurs)), collapse = ", "),
                                      paste(rep("?", length(valeurs)), collapse = ", ")), params = unname(valeurs))
    }
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    etendue <- if (any(ok)) c(min(x[ok]), min(y[ok]), max(x[ok]), max(y[ok])) else rep(NA_real_, 4)
    RSQLite::dbBegin(con)
    q("INSERT INTO gpkg_contents (table_name, data_type, identifier, description, min_x, min_y, max_x, max_y, srs_id) VALUES (?, 'features', ?, ?, ?, ?, ?, ?, ?)",
      params = list(noms_tables[1], noms_tables[1], "Nuée", etendue[1], etendue[2], etendue[3], etendue[4], srs))
    q("INSERT INTO gpkg_geometry_columns VALUES (?, 'geom', 'POINT', ?, 0, 0)", params = list(noms_tables[1], srs))
    creer(noms_tables[1], df, blob_liste(lapply(seq_along(x), function(i) geometrie_point(x[i], y[i], srs))))
    for (k in seq_along(tables)) {
      q("INSERT INTO gpkg_contents (table_name, data_type, identifier, description) VALUES (?, 'attributes', ?, ?)",
        params = list(noms_tables[k + 1], noms_tables[k + 1], "Nuée"))
      creer(noms_tables[k + 1], as.data.frame(tables[[k]], check.names = FALSE, stringsAsFactors = FALSE))
    }
    RSQLite::dbCommit(con)
  })
}

# Les résultats d'un modèle (formats.R : tables_resultats) en GeoPackage : les observations en
# couche de points, aux coordonnées du projet (retrouvées par le nom de chaque observation, ou
# dans l'ordre des observations du modèle quand des noms se répètent), les autres tableaux en
# tables sans géométrie.
ecrire_geopackage_resultats <- function(projet, m, tables, chemin, nom = "resultats") {
  obs <- tables$observations
  if (is.null(obs) || !nrow(obs)) stop(traduire("Ce modèle n'a pas de tableau des observations à placer sur une carte.", "This model has no table of observations to place on a map."), call. = FALSE)
  pts <- points_projet(projet)
  noms <- unname(projet$noms_obs[as.character(pts$uid)])
  k <- if (!anyDuplicated(noms) && "Observation" %in% names(obs)) match(as.character(obs$Observation), noms)
       else if (length(m$observations) == nrow(obs)) match(as.numeric(m$observations), pts$uid)
       else stop(traduire("Les observations du modèle ne se retrouvent pas dans le projet : des noms se répètent.", "The model's observations can't be found in the project: some names repeat."), call. = FALSE)
  ecrire_geopackage(obs, chemin, pts$x[k], pts$y[k], pts$systeme, couche = nom, tables = tables[setdiff(names(tables), "observations")])
}

# Une liste de raw (NULL : une valeur manquante) que RSQLite écrit en BLOB (le paquet blob vient
# avec RSQLite).
blob_liste <- function(l) blob::new_blob(l)
