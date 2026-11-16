# Vérifie les greffons (moteur/R/greffons.R) par les demandes du serveur, telles que l'interface les
# envoie (serveur.R : DEMANDES, chaque demande passée par le JSON) :
#   - le manifeste : les deux exemples (exemple/plugins) valides ; chaque sorte d'erreur refusée, en
#     disant où et quoi ; un ORCID et sa clé ; le code analysé sans être exécuté ; les archives
#     .nueeplugin (à la racine ou dans un dossier ; hors de son dossier, sans plugin.json : refusées) ;
#     les .nueeplugin du site de la documentation, les mêmes greffons que exemple/plugins ;
#   - l'installation et la confiance : lu sans rien exécuter, installé, approuvé par la demande
#     code_approuve (que seule l'application fait, avec son secret) ; un greffon jamais approuvé refusé
#     à chaque chemin (ajuster une définition, un modèle du projet, réajuster, prédire, l'export vers R),
#     ses fichiers changés après l'installation aussi ; retirer ; un fichier changé entre la lecture et
#     l'installation ; les greffons de l'organisation (sans approbation ; le même id que l'utilisateur ;
#     la politique DisableCustomCode, qui ne laisse qu'eux) ; un paquet qui manque ; une Nuée trop ancienne ;
#   - l'ajustement : les entrées et les options vérifiées (des types, des nombres), ce que la fonction
#     reçoit, ses résultats comme ceux d'un modèle de code R, ses méthodes S3, une erreur du greffon et sa
#     ligne ;
#   - un projet qui garde son greffon, ouvert ailleurs (sans le greffon ni son approbation) : non
#     approuvé, refusé, Montrer (greffon_projet), Installer depuis le projet, puis ajusté ;
#   - les versions : une autre version installée, le projet refait avec la sienne, puis passé à
#     l'installée (greffon_mettre_a_jour) ;
#   - le texte des méthodes (le greffon, sa version, ses auteurs, sa citation, celle de ses paquets),
#     les tableaux exportés, les résultats clés ;
#   - l'export vers R : le script, lancé seul par Rscript, redonne les mêmes nombres ;
#   - les nombres des deux exemples : l'occupation contre sa vraisemblance calculée en énumérant les
#     états des sites (un petit cas), sa solution exacte sans covariables, ses erreurs-types contre la
#     hessienne de cette vraisemblance ; Mann–Kendall contre cor.test(method = "kendall"), un calcul à
#     la main et la vue Séries de Nuée (series.R : mann_kendall).
#   Rscript moteur/tests/verifier_greffons.R

options(warn = 1, stringsAsFactors = FALSE)
racine_nuee <- normalizePath(".", winslash = "/")
dossier <- file.path(dirname(tempdir()), basename(tempfile("nuee_greffons")))
dir.create(dossier)
fichier <- function(...) file.path(dossier, ...)
Sys.setenv(NUEE_CODE_SECRET = "secret-de-l-essai", NUEE_GREFFONS = fichier("greffons"), NUEE_GREFFONS_ORGANISATION = fichier("organisation"), NUEE_VERSION = "1.1.0")
Sys.unsetenv("NUEE_SANS_CODE")
# Le serveur sans son écoute (comme verifier_code.R), jusqu'à son interface HTTP comprise.
local({
  assign("racine", racine_nuee, globalenv())
  for (e in parse(file.path(racine_nuee, "moteur", "serveur.R"), encoding = "UTF-8", keep.source = FALSE)) {
    texte <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
    if (startsWith(texte, "serveur <- httpuv::startServer")) break
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
n_verifs <- 0
verifier <- function(nom, ok) {
  n_verifs <<- n_verifs + 1
  cat(if (isTRUE(ok)) "  ok      " else "  ÉCHEC   ", nom, "\n", sep = "")
  if (!isTRUE(ok)) echecs <<- echecs + 1
}
proche <- function(a, b, tol = 1e-9) {
  a <- as.numeric(unlist(a))
  b <- as.numeric(unlist(b))
  length(a) == length(b) && identical(is.na(a), is.na(b)) && all(abs(a[!is.na(a)] - b[!is.na(b)]) <= tol * pmax(1, abs(b[!is.na(b)])))
}
demande <- function(nom, q = list()) {
  q <- jsonlite::fromJSON(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA)), simplifyVector = TRUE)
  r <- DEMANDES[[nom]](q)
  repondre(200L, r)
  invisible(r)
}
refus <- function(nom, q) tryCatch({ demande(nom, q); "" }, error = conditionMessage)
# Une requête HTTP comme la fenêtre ou l'application l'envoie (appli$call) : son statut et son corps.
requete <- function(nom, q, entetes = list()) {
  corps <- charToRaw(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", digits = NA)))
  req <- c(list(PATH_INFO = paste0("/api/", nom), HTTP_HOST = "127.0.0.1:8799", HTTP_COOKIE = sprintf("%s=%s", COOKIE, JETON),
                rook.input = list(read = function() corps)), entetes)
  r <- appli$call(req)
  list(statut = r$status, corps = jsonlite::fromJSON(r$body, simplifyVector = TRUE))
}
# Approuver comme l'application : l'empreinte ajoutée à la liste qu'elle garde, toute la liste donnée au
# moteur avec son secret.
approuver <- function(empreinte) {
  r <- requete("code_approuve", list(empreintes = as.list(unique(c(APPROBATIONS_CODE$empreintes, empreinte)))), list(HTTP_X_NUEE_CODE = "secret-de-l-essai"))
  stopifnot(r$statut == 200)
}
oublier <- function(empreinte) APPROBATIONS_CODE$empreintes <- setdiff(APPROBATIONS_CODE$empreintes, empreinte)

# Une archive zip (sans compression) de `fichiers` (des noms et leurs textes), pour les .nueeplugin de
# l'essai : le CRC-32 calculé ici.
xor32 <- function(a, b) bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
TABLE_CRC <- vapply(0:255, function(n) { c <- n; for (k in 1:8) c <- if (c %% 2 == 1) xor32(0xedb88320, c %/% 2) else c %/% 2; c }, 0)
crc32 <- function(octets) {
  c <- 0xffffffff
  for (o in as.integer(octets)) c <- xor32(TABLE_CRC[(xor32(c, o) %% 256) + 1], c %/% 256)
  xor32(c, 0xffffffff)
}
ecrire_zip <- function(chemin, fichiers) {
  le <- function(x, n) { out <- raw(n); for (i in seq_len(n)) { out[i] <- as.raw(x %% 256); x <- x %/% 256 }; out }
  locaux <- list(); centre <- list(); decalage <- 0
  for (nom in names(fichiers)) {
    o <- charToRaw(enc2utf8(fichiers[[nom]]))
    n <- charToRaw(enc2utf8(nom))
    crc <- crc32(o)
    local <- c(le(0x04034b50, 4), le(20, 2), le(0x0800, 2), le(0, 2), le(0, 2), le(0x21, 2), le(crc, 4), le(length(o), 4), le(length(o), 4), le(length(n), 2), le(0, 2), n, o)
    centre[[nom]] <- c(le(0x02014b50, 4), le(20, 2), le(20, 2), le(0x0800, 2), le(0, 2), le(0, 2), le(0x21, 2), le(crc, 4), le(length(o), 4), le(length(o), 4),
                       le(length(n), 2), le(0, 2), le(0, 2), le(0, 2), le(0, 2), le(0, 4), le(decalage, 4), n)
    locaux[[nom]] <- local
    decalage <- decalage + length(local)
  }
  c_tout <- unlist(centre, use.names = FALSE)
  fin <- c(le(0x06054b50, 4), le(0, 2), le(0, 2), le(length(fichiers), 2), le(length(fichiers), 2), le(length(c_tout), 4), le(decalage, 4), le(0, 2))
  writeBin(c(unlist(locaux, use.names = FALSE), c_tout, fin), chemin)
  chemin
}
# les fichiers d'un dossier, leurs textes tels quels ; et des fichiers écrits octet pour octet
textes_dossier <- function(d) {
  f <- list.files(d, recursive = TRUE)
  stats::setNames(lapply(f, function(x) { t <- rawToChar(readBin(file.path(d, x), "raw", file.size(file.path(d, x)))); Encoding(t) <- "UTF-8"; t }), f)
}
ecrire_greffon <- function(d, fichiers) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (f in fichiers) writeBin(charToRaw(enc2utf8(f$texte)), file.path(d, f$chemin))
  invisible(d)
}

exemples <- file.path(racine_nuee, "exemple", "plugins")
occ_dossier <- file.path(exemples, "occupancy")
mk_dossier <- file.path(exemples, "mann-kendall")
manifeste_occ <- paste(readLines(file.path(occ_dossier, "plugin.json"), encoding = "UTF-8"), collapse = "\n")
j_occ <- jsonlite::fromJSON(manifeste_occ, simplifyVector = FALSE)
# un manifeste de l'exemple, changé (une liste de champs remplacés ; NULL en retire un), en texte
variante <- function(...) {
  j <- j_occ
  ch <- list(...)
  for (k in names(ch)) j[k] <- list(ch[[k]])
  for (k in names(ch)) if (is.null(ch[[k]])) j[[k]] <- NULL
  as.character(jsonlite::toJSON(j, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE))
}
erreurs_de <- function(texte, presents = "occupancy.R") lire_manifeste(texte, presents)$erreurs

# ---------------------------------------------------------------- le manifeste

cat("Le manifeste\n")
lu_occ <- lire_manifeste(manifeste_occ, "occupancy.R")
lu_mk <- lire_manifeste(paste(readLines(file.path(mk_dossier, "plugin.json"), encoding = "UTF-8"), collapse = "\n"), "mann_kendall.R")
verifier("les deux exemples : valides, sans avertissement ; leurs entrées, leurs options, leurs citations lues",
         !length(lu_occ$erreurs) && !length(lu_mk$erreurs) && !length(lu_occ$avertissements) && !length(lu_mk$avertissements) &&
           identical(vapply(lu_occ$manifeste$entrees, `[[`, "", "id"), c("detections", "occupancy", "detection")) && lu_occ$manifeste$entrees[[1]]$min == 2 &&
           isTRUE(lu_occ$manifeste$entrees[[2]]$facultatif) && identical(lu_occ$manifeste$options[[2]]$entier, TRUE) &&
           identical(vapply(lu_mk$manifeste$options, `[[`, "", "type"), c("choice", "checkbox", "number", "text")) && length(lu_mk$manifeste$citations) == 3 &&
           isTRUE(lu_mk$manifeste$entrees[[2]]$facultatif) && identical(lu_mk$manifeste$entrees[[2]]$type, "variable"))
cas <- list(
  list("pas du JSON", "{ \"id\": ", "plugin.json : ce n'est pas du JSON valide"),
  list("un tableau au lieu d'un objet", "[1, 2]", "il faut un objet JSON"),
  list("un champ requis absent", variante(license = NULL), "il manque le champ « license »"),
  list("un champ inconnu, et ce qu'il voulait dire", variante(licence = "MIT"), "champ inconnu « licence » (vouliez-vous dire « license » ?)"),
  list("un id qui n'est pas un nom de dossier sûr", variante(id = "../occupancy"), "id : des minuscules"),
  list("une version qui n'est pas X.Y.Z", variante(version = "1.0"), "version : une version X.Y.Z"),
  list("la version de Nuée", variante(nuee = "un"), "nuee : une version X.Y.Z"),
  list("un nom sans anglais", variante(name = list(fr = "Occupation")), "name.en : requis"),
  list("un nom en texte seul", variante(name = "Occupancy"), "name : un objet {\"en\""),
  list("sans auteur", variante(authors = list()), "authors : au moins 1"),
  list("un ORCID dont la clé est fausse", variante(authors = list(list(name = "A", orcid = "0000-0002-1825-0098"))), "authors[1].orcid : un ORCID"),
  list("un fichier hors du dossier", variante(files = list("../x.R")), "files[1] : le chemin d'un fichier .R"),
  list("un fichier absent", variante(files = list("autre.R")), "files[1] : le fichier autre.R n'est pas dans le greffon"),
  list("une fonction qui n'est pas un nom R", variante(`function` = "occupancy fit"), "function : le nom d'une fonction R"),
  list("un paquet qui n'en est pas un", variante(packages = list("un paquet")), "packages[1] : le nom d'un paquet R"),
  list("une entrée d'un type inconnu", variante(inputs = list(list(id = "y", type = "colonne", label = list(en = "Y")))), "inputs[1].type : requis"),
  list("une entrée qui accepte un type inconnu", variante(inputs = list(list(id = "y", type = "variable", accepts = "numeric", label = list(en = "Y")))), "inputs[1].accepts"),
  list("min pour une seule variable", variante(inputs = list(list(id = "y", type = "variable", min = 1, label = list(en = "Y")))), "inputs[1].min : seulement pour « variables »"),
  list("max sous min", variante(inputs = list(list(id = "y", type = "variables", min = 3, max = 2, label = list(en = "Y")))), "inputs[1].max : un entier, au moins min"),
  list("deux entrées du même id", variante(inputs = list(list(id = "y", type = "variable", label = list(en = "Y")), list(id = "y", type = "variable", label = list(en = "Z")))), "inputs : l'id « y » revient"),
  list("une entrée sans libellé", variante(inputs = list(list(id = "y", type = "variable"))), "inputs[1].label : requis"),
  list("un nombre sans valeur par défaut", variante(options = list(list(id = "k", type = "number", label = list(en = "k")))), "options[1].default : requis, un nombre"),
  list("un défaut hors de [min, max]", variante(options = list(list(id = "k", type = "number", min = 1, max = 5, default = 9, label = list(en = "k")))), "options[1].default : hors de [min, max]"),
  list("un défaut non entier d'un entier", variante(options = list(list(id = "k", type = "number", integer = TRUE, default = 1.5, label = list(en = "k")))), "options[1].default : un entier"),
  list("un choix dont le défaut n'est pas une valeur", variante(options = list(list(id = "m", type = "choice", values = list(list(value = "a", label = list(en = "A"))), default = "b", label = list(en = "M")))),
       "options[1].default : requis, l'une des valeurs (a)"),
  list("un champ d'un autre type d'option", variante(options = list(list(id = "c", type = "checkbox", min = 1, label = list(en = "C")))), "options[1] : champ inconnu « min »"),
  list("une citation BibTeX mal fermée", variante(citation = list(text = "A (2000).", bibtex = "@article{a, title = {x}")), "citation.bibtex : une entrée BibTeX"))
for (k in seq_along(cas)) {
  e <- erreurs_de(cas[[k]][[2]])
  verifier(sprintf("refusé, %s : « %s »", cas[[k]][[1]], if (length(e)) e[1] else "(rien)"), length(e) >= 1 && any(startsWith(e, cas[[k]][[3]]) | grepl(cas[[k]][[3]], e, fixed = TRUE)))
}
options(nuee.langue = "en")
e_en <- erreurs_de(variante(licence = "MIT"))
options(nuee.langue = "fr")
verifier("les messages dans la langue de la demande (en)", any(grepl("unknown field \"licence\" (did you mean \"license\"?)", e_en, fixed = TRUE)))
verifier("un ORCID juste (0000-0002-1825-0097, et en adresse https://orcid.org/…) ; un texte français absent : un avertissement, l'anglais à sa place",
         orcid_valide("0000-0002-1825-0097") && !orcid_valide("0000-0002-1825-0098") &&
           !length(erreurs_de(variante(authors = list(list(name = "Josiah Carberry", orcid = "https://orcid.org/0000-0002-1825-0097"))))) &&
           grepl("name", lire_manifeste(variante(name = list(en = "Occupancy")), "occupancy.R")$avertissements) &&
           identical(lire_manifeste(variante(name = list(en = "Occupancy")), "occupancy.R")$manifeste$nom$fr, "Occupancy"))
# le code : analysé, jamais exécuté
trace <- fichier("execute.txt")
code_piege <- sprintf("writeLines('oui', %s)\nf <- function(donnees, entrees, options) 1\n", deparse(trace))
g_piege <- faire_greffon(list(list(chemin = "plugin.json", texte = variante(files = list("a.R"), `function` = "f")), list(chemin = "a.R", texte = code_piege)), "fichier")
verifier("lire un greffon n'exécute pas son code (son manifeste lu, son code analysé)", !length(g_piege$erreurs) && !file.exists(trace))
g_lecture <- faire_greffon(list(list(chemin = "plugin.json", texte = manifeste_occ), list(chemin = "occupancy.R", texte = "occupancy_fit <- function(donnees, entrees, options) {\n  1 +\n}\n")), "fichier")
g_sans <- faire_greffon(list(list(chemin = "plugin.json", texte = manifeste_occ), list(chemin = "occupancy.R", texte = "autre <- function(donnees, entrees, options) 1\n")), "fichier")
g_args <- faire_greffon(list(list(chemin = "plugin.json", texte = manifeste_occ), list(chemin = "occupancy.R", texte = "occupancy_fit <- function(donnees) 1\n")), "fichier")
verifier(sprintf("un fichier qui ne se lit pas comme du R, avec sa ligne (« %s ») ; la fonction absente ; trop peu d'arguments", g_lecture$erreurs[1]),
         grepl("^occupancy.R, ligne 3", g_lecture$erreurs[1]) && grepl("ne définissent pas occupancy_fit", g_sans$erreurs[1]) && grepl("prend 1 argument", g_args$erreurs[1]))

# l'empreinte
f_occ <- fichiers_dossier_greffon(occ_dossier)
g_occ <- faire_greffon(f_occ, "fichier")
canon <- canon_greffon(f_occ)
sha <- tryCatch(getExportedValue("tools", "sha256sum"), error = function(e) NULL)
verifier("l'empreinte : le SHA-256 du texte canonique (« nuee plugin 1 », chaque fichier rangé par son chemin : chemin, longueur en octets, texte)",
         startsWith(canon, "nuee plugin 1\noccupancy.R\n") && grepl("\nplugin.json\n[0-9]+\n\\{", canon) &&
           identical(g_occ$empreinte, if (is.null(sha)) sha256_r(charToRaw(canon)) else unname(as.character(sha(bytes = charToRaw(canon))))) &&
           identical(g_occ$empreinte, sha256_r(charToRaw(canon))))
crlf <- lapply(f_occ, function(f) list(chemin = f$chemin, texte = gsub("\n", "\r\n", f$texte)))
autre_ordre <- rev(f_occ)
change <- f_occ
change[[2]]$texte <- sub("reltol = 1e-12", "reltol = 1e-10", change[[2]]$texte)
verifier("la même empreinte avec les fins de ligne de Windows et dans un autre ordre ; une autre dès qu'un caractère change",
         identical(empreinte_greffon(crlf), g_occ$empreinte) && identical(empreinte_greffon(autre_ordre), g_occ$empreinte) && !identical(empreinte_greffon(change), g_occ$empreinte))

# les archives
textes_occ <- textes_dossier(occ_dossier)
a1 <- ecrire_zip(fichier("occ_racine.nueeplugin"), textes_occ)
a2 <- ecrire_zip(fichier("occ_dossier.nueeplugin"), stats::setNames(c(textes_occ, list("note")), c(paste0("occupancy/", names(textes_occ)), "occupancy/README.md")))
g_a1 <- lire_fichier_greffon(a1)
g_a2 <- lire_fichier_greffon(a2)
verifier("une archive .nueeplugin, plugin.json à sa racine ou dans son seul dossier : le même greffon que son dossier (ses autres fichiers laissés de côté)",
         identical(g_a1$empreinte, g_occ$empreinte) && identical(g_a2$empreinte, g_occ$empreinte) && identical(vapply(g_a2$fichiers, `[[`, "", "chemin"), c("plugin.json", "occupancy.R")))
mauvaises <- list(
  list("hors de son dossier", ecrire_zip(fichier("evasion.nueeplugin"), list("plugin.json" = manifeste_occ, "../occupancy.R" = "x <- 1")), "hors de son dossier"),
  list("sans plugin.json", ecrire_zip(fichier("vide.nueeplugin"), list("lisez-moi.txt" = "rien")), "pas de plugin.json"),
  list("pas une archive", { writeLines("pas un zip", fichier("texte.nueeplugin")); fichier("texte.nueeplugin") }, "archive zip"))
for (x in mauvaises) {
  e <- refus("greffon_lire", list(chemin = x[[2]]))
  verifier(sprintf("une archive %s : refusée (« %s »)", x[[1]], e), grepl(x[[3]], e, ignore.case = TRUE))
}
for (k in c("occupancy", "mann-kendall")) {
  site <- file.path(racine_nuee, "docs", "site", "greffons", paste0(k, ".nueeplugin"))
  verifier(sprintf("le %s.nueeplugin du site de la documentation : le greffon de exemple/plugins/%s, tel quel", k, k),
           file.exists(site) && identical(lire_fichier_greffon(site)$empreinte, faire_greffon(fichiers_dossier_greffon(file.path(exemples, k)), "fichier")$empreinte))
}

# ---------------------------------------------------------------- installer, approuver

cat("Installer et approuver\n")
lu <- demande("greffon_lire", list(chemin = a1))
verifier("lu avant d'être installé : son manifeste, son code, son empreinte ; il peut s'installer ; rien n'est écrit",
         isTRUE(lu$peut) && identical(lu$greffon$empreinte, g_occ$empreinte) && identical(lu$greffon$fichiers[[2]]$chemin, "occupancy.R") &&
           grepl("occupancy_fit <- function", lu$greffon$fichiers[[2]]$texte) && is.null(lu$installe) && !dir.exists(fichier("greffons")))
verifier("installer avec une autre empreinte que celle lue (le fichier a changé depuis) : refusé",
         grepl("a changé depuis que vous l'avez lu", refus("greffon_installer", list(chemin = a1, empreinte = empreinte_greffon(change)))) && !dir.exists(fichier("greffons", "occupancy")))
approuver(lu$greffon$empreinte)
ins <- demande("greffon_installer", list(chemin = a1, empreinte = lu$greffon$empreinte))
gs <- demande("greffons")$liste
verifier("installé dans le dossier de l'utilisateur (son manifeste et ses fichiers R seulement), listé, permis ; ses paquets (aucun)",
         identical(sort(list.files(fichier("greffons", "occupancy"))), c("occupancy.R", "plugin.json")) && length(gs) == 1 && identical(gs[[1]]$id, "occupancy") &&
           identical(gs[[1]]$source, "utilisateur") && identical(gs[[1]]$etat, "ok") && isTRUE(gs[[1]]$permis) && !length(gs[[1]]$paquets$nom))
r_fenetre <- requete("code_approuve", list(empreintes = list(empreinte_greffon(change))))
verifier("une fenêtre n'approuve pas un greffon elle-même : sans le secret de l'application, refusé (403)",
         r_fenetre$statut == 403 && !greffon_permis(empreinte_greffon(change)))
g_mk <- faire_greffon(fichiers_dossier_greffon(mk_dossier), "fichier")
approuver(g_mk$empreinte)
demande("greffon_installer", list(chemin = file.path(mk_dossier, "plugin.json"), empreinte = g_mk$empreinte))
verifier("le second installé depuis le plugin.json de son dossier : les deux listés", length(demande("greffons")$liste) == 2)

# ---------------------------------------------------------------- les données

# 80 sites (exemple de la documentation : docs/site/donnees/occupancy.R), et un projet de séries
r_occ <- demande("ouvrir", list(chemin = file.path(racine_nuee, "docs", "site", "donnees", "woodpecker_occupancy.csv")))
pid <- r_occ$id
p <- function() projet(pid)
u <- function(...) as.numeric(numeros(p(), c(...)))
brut <- utils::read.csv(file.path(racine_nuee, "docs", "site", "donnees", "woodpecker_occupancy.csv"))
def_occ <- function(entrees = list(detections = u("visit1", "visit2", "visit3", "visit4"), occupancy = u("forest"), detection = u("noise")), options = list(), nom = "OC1", ...) {
  c(list(projet = pid, type = "PLUGIN", nom = nom, titre = "", options = list(greffon = list(id = "occupancy"), entrees = entrees, options = options)), list(...))
}

cat("L'ajustement : les entrées, les options\n")
refus_def <- function(...) refus("ajuster", def_occ(...))
verifier("une entrée qui ne prend que des variables quantitatives, une qualitative : refusé en mots",
         grepl("habitat est qualitative ; cette entrée prend des variables quantitatives", refus_def(entrees = list(detections = u("visit1", "habitat")))))
verifier("trop peu de variables (2 au moins), une entrée inconnue, une variable inconnue : refusés",
         grepl("au moins 2 variable", refus_def(entrees = list(detections = u("visit1")))) && grepl("Entrée inconnue", refus_def(entrees = list(detections = u("visit1", "visit2"), autre = u("forest")))) &&
           grepl("variable inconnue", refus_def(entrees = list(detections = c(u("visit1"), 9999)))))
verifier("une option hors de ses bornes, un entier qui n'en est pas un, un choix inconnu, une option inconnue : refusés",
         grepl("10 au moins", refus_def(options = list(maxit = 5))) && grepl("un entier", refus_def(options = list(maxit = 100.5))) &&
           grepl("l'une des valeurs BFGS, Nelder-Mead", refus_def(options = list(method = "CG"))) && grepl("Option inconnue", refus_def(options = list(vitesse = 2))) &&
           !length(Filter(function(e) identical(e$definition$type, "PLUGIN"), etat$modeles)))
m_occ <- demande("ajuster", def_occ())
d_occ <- Find(function(d) identical(d$name, "OC1"), p()$models)
verifier("ajusté : un modèle PLUGIN, son greffon gardé en entier dans sa définition (id, version, empreinte, fichiers), ses entrées en numéros, ses options complétées (défauts)",
         identical(m_occ$type, "PLUGIN") && isTRUE(m_occ$code_r) && identical(d_occ$options$greffon$empreinte, g_occ$empreinte) &&
           identical(empreinte_greffon(d_occ$options$greffon$fichiers), g_occ$empreinte) && proche(d_occ$options$entrees$occupancy, u("forest")) &&
           identical(d_occ$options$options$method, "BFGS") && d_occ$options$options$maxit == 1000 && isTRUE(d_occ$options$options$sites) &&
           setequal(variables_definition(d_occ), u("visit1", "visit2", "visit3", "visit4", "forest", "noise")))
verifier("ce que l'interface reçoit : le greffon (son nom dans les deux langues, sa version, ses auteurs, sa citation, ses entrées et options choisies), sa ligne, ses vues, sans le code dans ses réglages",
         identical(m_occ$greffon$nom$en, "Site occupancy (single season)") && identical(m_occ$greffon$version, "1.0.0") && identical(m_occ$greffon$auteurs[[1]]$nom, "Clément Boutin") &&
           grepl("MacKenzie", m_occ$greffon$citations[[1]]$texte) && identical(as.character(m_occ$greffon$reglages$entrees[[1]]$variables), c("visit1", "visit2", "visit3", "visit4")) &&
           identical(m_occ$greffon$reglages$options[[1]]$valeur, "BFGS") && identical(m_occ$greffon$manifeste$entrees[[1]]$min, 2L) &&
           identical(m_occ$greffon$manifeste$options[[1]]$valeurs[[2]]$valeur, "Nelder-Mead") && grepl("^Occupation des sites \\(une saison\\) · N 80 · AIC ", m_occ$chiffres) &&
           identical(as.character(m_occ$vues_defaut), c("code_coefficients", "code_tableaux")) && is.null(m_occ$reglages$options$greffon$fichiers) &&
           identical(m_occ$reglages$options$greffon$empreinte, g_occ$empreinte))
verifier("ses résultats, ceux d'un modèle de code R : les coefficients des deux parties (psi., p.), z et p, logLik, AIC, les valeurs et les tableaux du greffon, son résumé",
         identical(m_occ$coefficients$terme, c("psi.(Intercept)", "psi.forest", "p.(Intercept)", "p.noise")) && identical(m_occ$coefficients$partie, c("psi", "psi", "p", "p")) &&
           m_occ$statistiques$ddl == 4 && proche(m_occ$statistiques$AIC, -2 * m_occ$statistiques$logLik + 8) &&
           all(c("psi", "p", "naive_occupancy") %in% m_occ$valeurs$nom) && identical(vapply(m_occ$tableaux, `[[`, "", "nom"), c("estimates", "sites")) &&
           any(grepl("Occupation, logit\\(psi\\)", m_occ$resume)) && m_occ$N == 80)
# ce que la fonction a reçu
recu <- NULL
espion <- sprintf("occupancy_fit <- function(donnees, entrees, options) { assign('recu', list(noms = names(donnees), entrees = entrees, options = options), envir = globalenv()); %s }",
                  "list(valeurs = c(a = 1))")
g_espion <- faire_greffon(list(list(chemin = "plugin.json", texte = manifeste_occ), list(chemin = "occupancy.R", texte = espion)), "fichier")
approuver(g_espion$empreinte)
e_espion <- tryCatch(definir_plugin(p(), list(name = "E", options = list(greffon = list(id = "occupancy", empreinte = g_espion$empreinte)))), error = conditionMessage)
verifier("une version précise demandée par son empreinte, que ni l'installation ni le projet n'ont : refusée, même approuvée",
         is.character(e_espion) && grepl("ni installée ni dans le projet", e_espion))
ecrire_greffon(fichier("espion"), g_espion$fichiers)
demande("greffon_installer", list(chemin = fichier("espion"), empreinte = g_espion$empreinte))
m_espion <- demande("ajuster", def_occ(entrees = list(detections = u("visit1", "visit2"), occupancy = list()), options = list(maxit = 50), nom = "E1"))
verifier("ce que la fonction reçoit : donnees (les variables du projet, sous leurs noms), entrees (les noms ; une entrée « variables » vide, character(0)), options (un entier pour un entier, TRUE/FALSE, le choix)",
         identical(recu$entrees$detections, c("visit1", "visit2")) && identical(recu$entrees$occupancy, character(0)) && identical(recu$entrees$detection, character(0)) &&
           identical(recu$options$maxit, 50L) && identical(recu$options$method, "BFGS") && isTRUE(recu$options$sites) && identical(recu$options$level, 0.95) &&
           identical(recu$noms, c("forest", "habitat", "noise", "visit1", "visit2", "visit3", "visit4")) && identical(as.character(m_espion$valeurs$nom), "a"))
demande("greffon_installer", list(chemin = a1, empreinte = g_occ$empreinte))  # (la vraie version, de nouveau)
verifier("installer une version à la place d'une autre : le dossier remplacé, sans dossier laissé à côté",
         identical(Find(function(g) identical(g$id, "occupancy"), lister_greffons())$empreinte, g_occ$empreinte) &&
           identical(sort(list.files(fichier("greffons"), all.files = TRUE, no.. = TRUE)), c("mann-kendall", "occupancy")))
# une erreur du greffon, et sa ligne
casse <- sub("  covariates <- unique", "  stop(\"une erreur du greffon\")\n  covariates <- unique", textes_occ[["occupancy.R"]], fixed = TRUE)
g_casse <- faire_greffon(list(list(chemin = "plugin.json", texte = manifeste_occ), list(chemin = "occupancy.R", texte = casse)), "fichier")
approuver(g_casse$empreinte)
ligne_stop <- grep("une erreur du greffon", strsplit(casse, "\n")[[1]])
d_casse <- d_occ
d_casse$options$greffon$fichiers <- g_casse$fichiers
r_casse <- tryCatch(ajuster_plugin(p(), d_casse), error = function(e) e)
verifier(sprintf("une erreur dans le greffon : son message, son fichier et sa ligne (« %s »)", conditionMessage(r_casse)),
         inherits(r_casse, "erreur_code") && grepl(sprintf("(occupancy.R, ligne %d) : une erreur du greffon", ligne_stop), conditionMessage(r_casse), fixed = TRUE))

# ---------------------------------------------------------------- les nombres : l'occupation

cat("Les nombres : l'occupation\n")
# la vraisemblance en énumérant les 2^n états (occupé ou non) de n sites, puis son maximum
y_petit <- matrix(c(1, 0, 1, 0, 0, 0, 0, NA, 1, 1, 1, 0, 0, 0, NA, 0, 1, 0, 0, 0, 0, 1, 0, NA), 8, 3, byrow = TRUE)
x_petit <- c(0.2, 0.7, -0.5, 1.4, -0.3, -0.9, 0.4, 0.1)  # (des sites détectés et d'autres à chaque bout : pas de séparation)
z_petit <- c(1.2, 0.3, -0.6, 0.8, -1.5, 0.0, 0.4, -0.9)
petit <- data.frame(nom = sprintf("s%d", 1:8), v1 = y_petit[, 1], v2 = y_petit[, 2], v3 = y_petit[, 3], x = x_petit, z = z_petit)
utils::write.csv(petit, fichier("petit.csv"), row.names = FALSE, na = "")
r_petit <- demande("ouvrir", list(chemin = fichier("petit.csv")))
pp <- projet(r_petit$id)
m_petit <- demande("ajuster", list(projet = r_petit$id, type = "PLUGIN", nom = "P1", options = list(greffon = list(id = "occupancy"),
                     entrees = list(detections = as.numeric(numeros(pp, c("v1", "v2", "v3"))), occupancy = as.numeric(numeros(pp, "x")), detection = as.numeric(numeros(pp, "z"))))))
force_brute <- function(theta) {
  psi <- plogis(theta[1] + theta[2] * x_petit)
  pr <- plogis(theta[3] + theta[4] * z_petit)
  L <- 0
  for (k in 0:(2^8 - 1)) {
    z <- as.integer(intToBits(k))[1:8]
    terme <- 1
    for (i in 1:8) {
      vu <- !is.na(y_petit[i, ])
      terme <- terme * (if (z[i]) psi[i] * prod(pr[i]^y_petit[i, vu] * (1 - pr[i])^(1 - y_petit[i, vu])) else (1 - psi[i]) * prod(y_petit[i, vu] == 0))
    }
    L <- L + terme
  }
  log(L)
}
theta_petit <- m_petit$coefficients$estimation
verifier("sa log-vraisemblance au maximum : celle des 256 états des 8 sites énumérés, à 1e-10 près",
         proche(m_petit$statistiques$logLik, force_brute(theta_petit), 1e-10))
optimum <- stats::optim(c(0.1, 0.1, 0.1, 0.1), function(t) -force_brute(t), method = "Nelder-Mead", control = list(reltol = 1e-14, maxit = 20000))
optimum <- stats::optim(optimum$par, function(t) -force_brute(t), method = "BFGS", control = list(reltol = 1e-14))
H <- stats::optimHess(theta_petit, function(t) -force_brute(t))
verifier("son maximum : celui de la vraisemblance énumérée (Nelder-Mead puis BFGS d'un autre départ), à 1e-5 près ; ses erreurs-types, celles de la hessienne de celle-ci, à 1e-4 près",
         proche(theta_petit, optimum$par, 1e-5) && proche(m_petit$coefficients$erreur_type, sqrt(diag(solve(H))), 1e-4))
# sans covariables et sans visite manquée : la solution exacte (MacKenzie et al. 2002), p/p* = d/(T n_D) puis psi = n_D/(n p*)
y_c <- as.matrix(brut[c("visit1", "visit2", "visit3", "visit4")])
complets <- stats::complete.cases(y_c)
y_c <- y_c[complets, ]
n_D <- sum(rowSums(y_c) > 0)
T_v <- ncol(y_c)
p_exact <- stats::uniroot(function(p) p / (1 - (1 - p)^T_v) - sum(y_c) / (T_v * n_D), c(1e-6, 1 - 1e-9), tol = 1e-14)$root
psi_exact <- n_D / (nrow(y_c) * (1 - (1 - p_exact)^T_v))
lignes_completes <- numeros_observations(p())[complets]
m_c <- demande("ajuster", def_occ(entrees = list(detections = u("visit1", "visit2", "visit3", "visit4")), nom = "OC0", exclues = setdiff(numeros_observations(p()), lignes_completes)))
v_c <- stats::setNames(m_c$valeurs$valeur, m_c$valeurs$nom)
verifier(sprintf("sans covariables, sur les %d sites visités 4 fois : psi = %.6f et p = %.6f, ceux de la solution exacte, à 1e-6 près", nrow(y_c), v_c[["psi"]], v_c[["p"]]),
         proche(v_c[["psi"]], psi_exact, 1e-6) && proche(v_c[["p"]], p_exact, 1e-6) && proche(m_c$coefficients$estimation, stats::qlogis(c(psi_exact, p_exact)), 1e-5))
se_psi <- psi_exact * (1 - psi_exact) * m_c$coefficients$erreur_type[1]
verifier("leurs erreurs-types : celles des coefficients par la méthode delta, psi(1 − psi) × se ; l'intervalle, celui du coefficient transformé",
         proche(v_c[["psi_se"]], se_psi, 1e-6) && proche(m_c$tableaux[[1]]$valeurs[[4]][1], stats::plogis(m_c$coefficients$bas[1]), 1e-8))
verifier("Nelder–Mead, l'autre méthode : le même maximum, à 1e-3 près",
         proche(demande("ajuster", def_occ(options = list(method = "Nelder-Mead", maxit = 5000), nom = "OC2"))$coefficients$estimation, m_occ$coefficients$estimation, 1e-3))

# ---------------------------------------------------------------- les nombres : Mann–Kendall

cat("Les nombres : Mann–Kendall et la pente de Sen\n")
r_mk <- demande("ouvrir", list(chemin = file.path(racine_nuee, "docs", "site", "donnees", "skylark_counts.csv")))
pk <- projet(r_mk$id)
def_mk <- function(options = list(), nom = "MK1", condition = "site == \"C01\"", temps = TRUE) {
  list(projet = r_mk$id, type = "PLUGIN", nom = nom, condition = condition,
       options = list(greffon = list(id = "mann-kendall"), entrees = c(list(series = as.numeric(numeros(pk, "skylarks"))), if (temps) list(time = as.numeric(numeros(pk, "year")))),
                      options = options))
}
m_mk <- demande("ajuster", def_mk(options = list(unit = "year")))
v_mk <- stats::setNames(m_mk$valeurs$valeur, m_mk$valeurs$nom)
c01 <- utils::read.csv(file.path(racine_nuee, "docs", "site", "donnees", "skylark_counts.csv"))
c01 <- c01[c01$site == "C01" & !is.na(c01$skylarks), ]
ct <- stats::cor.test(c01$year, c01$skylarks, method = "kendall", exact = FALSE, continuity = TRUE)
verifier(sprintf("sur un site (%d années, des ex aequo) : z et p ceux de cor.test(method = \"kendall\", exact = FALSE, continuity = TRUE), tau celui de cor(method = \"kendall\")", nrow(c01)),
         proche(v_mk[["z"]], ct$statistic, 1e-10) && proche(v_mk[["p"]], ct$p.value, 1e-10) && proche(v_mk[["tau"]], stats::cor(c01$year, c01$skylarks, method = "kendall"), 1e-12) &&
           v_mk[["n"]] == nrow(c01))
nu <- mann_kendall(c01$year, c01$skylarks)
verifier("S, Var(S), p, la pente de Sen et son intervalle : ceux de la vue Séries de Nuée (series.R : mann_kendall)",
         proche(c(v_mk[["S"]], v_mk[["var_S"]], v_mk[["p"]], v_mk[["slope"]], v_mk[["slope_lower"]], v_mk[["slope_upper"]]), c(nu$S, nu$var_S, nu$p, nu$pente, nu$bas, nu$haut), 1e-12))
# à la main : 6 valeurs, un ex aequo (2, 2)
main <- data.frame(nom = sprintf("t%d", 1:6), t = c(1, 2, 3, 5, 6, 8), y = c(3, 2, 5, 2, 7, 9))
utils::write.csv(main, fichier("main.csv"), row.names = FALSE)
r_main <- demande("ouvrir", list(chemin = fichier("main.csv")))
pm <- projet(r_main$id)
m_main <- demande("ajuster", list(projet = r_main$id, type = "PLUGIN", nom = "H1", options = list(greffon = list(id = "mann-kendall"),
                    entrees = list(series = as.numeric(numeros(pm, "y")), time = as.numeric(numeros(pm, "t"))), options = list(continuity = FALSE))))
v_main <- stats::setNames(m_main$valeurs$valeur, m_main$valeurs$nom)
# les 15 paires (i avant j) : signes de y_j − y_i
#   3 :  2  5  2  7  9   →  −1 +1 −1 +1 +1   =  1
#   2 :     5  2  7  9   →  +1  0 +1 +1      =  3
#   5 :        2  7  9   →  −1 +1 +1         =  1
#   2 :           7  9   →  +1 +1            =  2
#   7 :              9   →  +1               =  1      S = 8
# Var(S) = [6·5·17 − 2·1·9] / 18 = (510 − 18) / 18 = 27.333… ; z = 8 / √27.333 (sans correction)
# les 15 pentes (y_j − y_i) / (t_j − t_i) rangées : −1,5 −1 −0,25 0 0,667 0,8 0,8 0,857 1 1 1,167 1,25 2,333 3 5 ;
# la médiane, la 8e : (9 − 3)/(8 − 1) = 6/7
pentes_main <- c((2 - 3) / 1, (5 - 3) / 2, (2 - 3) / 4, (7 - 3) / 5, (9 - 3) / 7, (5 - 2) / 1, (2 - 2) / 3, (7 - 2) / 4, (9 - 2) / 6,
                 (2 - 5) / 2, (7 - 5) / 3, (9 - 5) / 5, (7 - 2) / 1, (9 - 2) / 3, (9 - 7) / 2)
verifier(sprintf("6 valeurs et un ex aequo, à la main : S = 8, Var(S) = 492/18, z = 8/√Var(S), tau-b = 8/√(15 × 14), la pente médiane des 15 (%.4f)", stats::median(pentes_main)),
         v_main[["S"]] == 8 && proche(v_main[["var_S"]], 492 / 18, 1e-12) && proche(v_main[["z"]], 8 / sqrt(492 / 18), 1e-12) && proche(v_main[["tau"]], 8 / sqrt(15 * 14), 1e-12) &&
           proche(v_main[["slope"]], stats::median(pentes_main), 1e-12) && proche(v_main[["intercept"]], stats::median(main$y - stats::median(pentes_main) * main$t), 1e-12))
m_un <- demande("ajuster", def_mk(options = list(alternative = "less"), nom = "MK2"))
v_un <- stats::setNames(m_un$valeurs$valeur, m_un$valeurs$nom)
m_ordre <- demande("ajuster", def_mk(nom = "MK3", temps = FALSE))
verifier("unilatéral (une baisse) : p = Φ(z) ; sans temps, l'ordre des lignes (les mêmes ici, rangées par année)",
         proche(v_un[["p"]], stats::pnorm(v_un[["z"]]), 1e-12) && proche(stats::setNames(m_ordre$valeurs$valeur, m_ordre$valeurs$nom)[["S"]], v_mk[["S"]]))
verifier("ses valeurs ajustées et ses résidus (la droite de Sen), rapportés à leurs observations : les années manquantes laissées de côté (na.action)",
         length(m_mk$ajustees$ajuste) == nrow(c01) && length(m_mk$ajustees$uid) == nrow(c01) && m_mk$N == nrow(c01) && m_mk$n_donnees == 17 &&
           proche(m_mk$ajustees$ajuste, v_mk[["intercept"]] + v_mk[["slope"]] * c01$year, 1e-12) && identical(as.character(m_mk$vues_defaut), c("code_tableaux", "code_diagnostics")))
verifier("une série trop courte, deux fois le même temps : refusés en mots",
         grepl("3 temps avec une valeur", refus("ajuster", def_mk(nom = "MK4", condition = "site == \"C01\" & year < 2011"))) &&
           grepl("revient deux fois", refus("ajuster", def_mk(nom = "MK5", condition = "site %in% c(\"C01\", \"C02\")"))))

# ---------------------------------------------------------------- les textes, les exports

cat("Les textes et les exports\n")
for (langue in c("fr", "en")) {
  tm <- demande("methodes", list(modele = m_occ$id, langue = langue, version = "1.1.0"))
  fr <- langue == "fr"
  verifier(sprintf("le texte des méthodes (%s) : le greffon, sa version, ses auteurs, sa fonction, sa méthode citée, ses entrées et ses options, R et Nuée ; ses références", langue),
           grepl(if (fr) "greffon « Occupation des sites (une saison) » 1.0.0 de Nuée, de Clément Boutin" else "plugin “Site occupancy (single season)” 1.0.0 by Clément Boutin", tm$paragraphes[1], fixed = TRUE) &&
             grepl("occupancy_fit", tm$paragraphes[1]) && grepl("MacKenzie et al., 2002", tm$paragraphes[1], fixed = TRUE) && grepl("visit1, visit2, visit3", tm$paragraphes[1]) &&
             grepl("forest", tm$paragraphes[1]) && grepl("BFGS", tm$paragraphes[1]) && grepl(if (fr) "1 000|1000" else "1000", tm$paragraphes[1]) &&
             grepl("R Core Team", tm$paragraphes[2]) && any(grepl("^MacKenzie, D. I.", tm$references)) && any(grepl("Boutin", tm$references)) && any(grepl("R Core Team", tm$references)))
}
tm_mk <- demande("methodes", list(modele = m_mk$id, langue = "en"))
verifier("… et les trois citations de Mann–Kendall (Mann, 1945 ; Kendall, 1975 ; Sen, 1968) d'après leur BibTeX",
         grepl("Mann, 1945; Kendall, 1975; Sen, 1968", tm_mk$paragraphes[1], fixed = TRUE) && sum(grepl("^(Mann|Kendall|Sen), ", tm_mk$references)) == 3)
# un greffon qui demande un paquet (MASS, un paquet recommandé, quand il est là)
if (requireNamespace("MASS", quietly = TRUE)) {
  avec_mass <- variante(packages = list("MASS"))
  g_mass <- faire_greffon(list(list(chemin = "plugin.json", texte = avec_mass), list(chemin = "occupancy.R", texte = textes_occ[["occupancy.R"]])), "fichier")
  ecrire_greffon(fichier("mass"), g_mass$fichiers)
  approuver(g_mass$empreinte)
  demande("greffon_installer", list(chemin = fichier("mass"), empreinte = g_mass$empreinte))
  m_mass <- demande("ajuster", def_occ(nom = "OM"))
  tm_mass <- demande("methodes", list(modele = m_mass$id, langue = "en"))
  verifier("un paquet qu'il demande : dans le texte des méthodes avec sa version et sa citation (MASS : Modern Applied Statistics with S)",
           grepl(sprintf("MASS %s (", utils::packageDescription("MASS")$Version), tm_mass$paragraphes[2], fixed = TRUE) && any(grepl("Modern Applied Statistics with S", tm_mass$references)))
  demande("greffon_installer", list(chemin = a1, empreinte = g_occ$empreinte))
} else cat("  (MASS absent)\n")
tr <- tables_resultats(p(), etat$modeles[[m_occ$id]]$modele)
verifier("ses tableaux exportés : le résumé, le greffon (id, version, auteurs, empreinte, réglages), les coefficients, ses valeurs et ses tableaux, son code fichier par fichier",
         all(c("resume", "greffon", "coefficients", "valeurs", "estimates", "sites", "code") %in% names(tr)) && identical(tr$greffon$empreinte, g_occ$empreinte) &&
           grepl("detections = visit1, visit2, visit3, visit4", tr$greffon$reglages) && setequal(unique(tr$code$fichier), c("plugin.json", "occupancy.R")) &&
           file.exists(ecrire_tables(tr, fichier("greffon.xlsx"), objet = "greffon")))
cles <- vapply(demande("resultats_cles", list(modele = m_occ$id))$entrees, `[[`, "", "cle")
verifier("ses résultats clés : N, logLik, AIC, chaque coefficient, ses valeurs (Ce qui a changé, les documents)",
         all(c("N", "logLik", "AIC", "coef.psi.forest", "se.p.noise", "valeurs.psi", "valeurs.p") %in% cles))
pr <- refus("predire", list(modele = m_occ$id))
verifier("prédire : il ne prédit pas (pas de predict() pour sa classe), et le dit", identical(m_occ$prediction, FALSE) && nzchar(pr))

# ---------------------------------------------------------------- l'export vers R

cat("L'export vers R : lancé seul, les mêmes nombres\n")
rscript <- file.path(R.home("bin"), "Rscript")
lanceur <- fichier("lancer.R")
writeLines(c(".a <- commandArgs(TRUE)", "pdf(NULL)", "source(.a[1], encoding = \"UTF-8\")",
             ".m <- get(.a[3])", ".r <- list(coef = tryCatch(coef(.m), error = function(e) NULL), loglik = tryCatch(as.numeric(logLik(.m)), error = function(e) NULL), n = nobs(.m))",
             "if (exists(.a[4])) .r$valeurs <- get(.a[4])", "saveRDS(.r, .a[2])"), lanceur)
lancer <- function(script, modele, valeurs) {
  sortie <- tempfile("sortie", dossier, ".rds")
  ici <- setwd(dossier)
  on.exit(setwd(ici))
  messages <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(lanceur), shQuote(script), shQuote(sortie), modele, valeurs), stdout = TRUE, stderr = TRUE))
  if (file.exists(sortie)) readRDS(sortie) else structure(list(), messages = messages)
}
for (cas in list(list(r = m_occ, langue = "fr", modele = "modele", valeurs = "valeurs", donnees = "script"),
                 list(r = m_mk, langue = "en", modele = "model", valeurs = "values", donnees = "csv"))) {
  s <- demande("script_r", list(modele = cas$r$id, chemin = fichier(sprintf("%s_%s.R", cas$r$nom, cas$langue)), langue = cas$langue, donnees = cas$donnees, version = "1.1.0"))
  texte <- readLines(s$script, encoding = "UTF-8")
  o <- lancer(s$script, cas$modele, cas$valeurs)
  m <- etat$modeles[[cas$r$id]]$modele
  code <- strsplit(m$fichiers_greffon[[2]]$texte, "\n", fixed = TRUE)[[1]]
  verifier(sprintf("%s (%s, données %s) : écrit sans écart ; le code du greffon tel quel, ses entrées, ses options, l'appel ; lancé seul par Rscript, les mêmes %s",
                   cas$r$nom, cas$langue, cas$donnees, if (!is.null(m$coefficients)) "coefficients, logLik, N et valeurs" else "N et valeurs"),
           is.null(s$ecart) && all(code[nzchar(code)] %in% texte) && any(grepl(sprintf("^%s <- %s\\(donnees, entrees, options\\)$", if (cas$langue == "en") "result" else "resultat", m$greffon$fonction), texte)) &&
             any(startsWith(texte, "entrees <- list(")) && any(startsWith(texte, "options <- list(")) && length(o) > 0 &&
             (is.null(m$coefficients) || (proche(o$coef, m$coefficients$estimation, 1e-10) && proche(o$loglik, m$statistiques$logLik, 1e-10))) &&
             o$n == m$N && proche(o$valeurs, m$valeurs, 1e-10))
  if (!length(o)) cat("    ", utils::tail(attr(o, "messages"), 5), sep = "\n    ")
}

# ---------------------------------------------------------------- un projet venu d'ailleurs

cat("Un projet qui garde son greffon, ouvert sur un autre ordinateur\n")
chemin <- fichier("occupation.nuee")
demande("enregistrer", list(projet = pid, chemin = chemin))
j <- jsonlite::fromJSON(chemin, simplifyVector = FALSE)
d_j <- Find(function(d) identical(d$name, "OC1"), j$models)
verifier("le fichier garde le greffon du modèle en entier : son manifeste et son code, son empreinte",
         identical(d_j$type, "PLUGIN") && identical(d_j$options$greffon$empreinte, g_occ$empreinte) &&
           identical(empreinte_greffon(lapply(d_j$options$greffon$fichiers, function(f) list(chemin = f$chemin, texte = f$texte))), g_occ$empreinte))
# l'autre ordinateur : ni le greffon installé, ni son approbation
unlink(fichier("greffons"), recursive = TRUE)
approuvees <- APPROBATIONS_CODE$empreintes
APPROBATIONS_CODE$empreintes <- character(0)
r2 <- demande("ouvrir", list(chemin = chemin))
s_oc1 <- Find(function(d) identical(d$nom, "OC1"), r2$modeles)
verifier("rouvert : son résumé dit le greffon (son nom, sa version, ses auteurs), non approuvé, pas installé ici",
         isFALSE(s_oc1$code_r$approuve) && identical(s_oc1$greffon$id, "occupancy") && identical(s_oc1$greffon$version, "1.0.0") && isFALSE(s_oc1$greffon$approuve) &&
           is.null(s_oc1$greffon$installe) && identical(as.character(s_oc1$greffon$auteurs), "Clément Boutin") && identical(s_oc1$greffon$nom$fr, "Occupation des sites (une saison)"))
rr <- requete("ajuster", list(projet = r2$id, numero = s_oc1$numero))
verifier("l'ajuster, comme la session qui se restaure le fait : refusé (400, greffon_non_approuve, son empreinte), rien ne s'exécute",
         rr$statut == 400 && isTRUE(rr$corps$greffon_non_approuve) && identical(rr$corps$empreinte, g_occ$empreinte) && grepl("ni installé ni approuvé", rr$corps$erreur) &&
           !any(vapply(etat$modeles, function(e) identical(e$projet, r2$id), TRUE)))
verifier("refusé aussi : une nouvelle définition demandant cette version, réajuster, prédire et exporter vers R un modèle déjà ajusté dans la séance",
         grepl("ni installée ni dans le projet|ni installé ni approuvé", refus("ajuster", list(projet = pid, type = "PLUGIN", nom = "X", options = list(greffon = list(id = "occupancy", empreinte = g_mk$empreinte))))) &&
           grepl("ni installé ni approuvé", refus("ajuster", list(projet = r2$id, type = "PLUGIN", nom = "X2", options = list(greffon = list(id = "occupancy", empreinte = g_occ$empreinte),
                                                                    entrees = list(detections = as.numeric(numeros(projet(r2$id), c("visit1", "visit2")))))))) &&
           grepl("ni installé ni approuvé", refus("reajuster", list(modele = m_occ$id, exclues = numeros_observations(p())[1]))) &&
           grepl("ni installé ni approuvé", refus("script_r", list(modele = m_occ$id, chemin = fichier("refuse.R")))) && !file.exists(fichier("refuse.R")))
gp <- demande("greffon_projet", list(projet = r2$id))$greffons
g1 <- Find(function(x) identical(x$empreinte, g_occ$empreinte), gp)
verifier("Montrer : chaque greffon que garde le projet (une fois par version), son code tel quel, ses modèles, non installé ici",
         length(gp) >= 2 && !anyDuplicated(vapply(gp, `[[`, "", "empreinte")) && !is.null(g1) && isFALSE(g1$permis) &&
           identical(g1$fichiers[[2]]$texte, texte_code(textes_occ[["occupancy.R"]])) && "OC1" %in% vapply(g1$modeles, `[[`, "", "nom") && is.null(g1$installe))
approuver(g_occ$empreinte)
inst <- demande("greffon_installer", list(projet = r2$id, numero = s_oc1$numero, empreinte = g_occ$empreinte))
ok <- demande("ajuster", list(projet = r2$id, numero = s_oc1$numero))
verifier("Installer et exécuter : installé depuis le projet, approuvé, il s'ajuste, les mêmes nombres",
         identical(inst$greffon$id, "occupancy") && dir.exists(fichier("greffons", "occupancy")) && identical(ok$type, "PLUGIN") &&
           proche(ok$coefficients$estimation, m_occ$coefficients$estimation, 1e-12))
APPROBATIONS_CODE$empreintes <- unique(c(approuvees, APPROBATIONS_CODE$empreintes))
demande("greffon_installer", list(chemin = file.path(mk_dossier, "plugin.json"), empreinte = g_mk$empreinte))  # (et l'autre greffon, de nouveau)
# des fichiers changés après l'installation
installe <- file.path(fichier("greffons", "occupancy"), "occupancy.R")
cat("\n# un ajout\n", file = installe, append = TRUE)
gs_change <- Find(function(g) identical(g$id, "occupancy"), demande("greffons")$liste)
verifier("ses fichiers installés changés depuis : il n'est plus permis (une autre empreinte), et un nouveau modèle est refusé",
         isFALSE(gs_change$permis) && grepl("ni installé ni approuvé", refus("ajuster", def_occ(nom = "OC9"))))
demande("greffon_installer", list(chemin = a1, empreinte = g_occ$empreinte))

# ---------------------------------------------------------------- les versions

cat("Une autre version installée\n")
v110 <- gsub("\"version\": \"1.0.0\"", "\"version\": \"1.1.0\"", textes_occ[["plugin.json"]], fixed = TRUE)
code110 <- sub("naive_occupancy = mean(detected > 0),", "naive_occupancy = mean(detected > 0), version = 110,", textes_occ[["occupancy.R"]], fixed = TRUE)
a110 <- ecrire_zip(fichier("occ_110.nueeplugin"), list("plugin.json" = v110, "occupancy.R" = code110))
lu110 <- demande("greffon_lire", list(chemin = a110))
verifier("lire une version plus récente : la version installée de même id dite (1.0.0, plus ancienne)",
         isTRUE(lu110$peut) && identical(lu110$installe$version, "1.0.0") && lu110$installe$ordre == 1 && isFALSE(lu110$installe$meme))
approuver(lu110$greffon$empreinte)
demande("greffon_installer", list(chemin = a110, empreinte = lu110$greffon$empreinte))
s_v <- Find(function(d) identical(d$nom, "OC1"), demande("projet", list(projet = pid))$modeles)
refait <- demande("ajuster", list(projet = pid, numero = s_v$numero))
verifier("le projet dit sa version (1.0.0) et l'installée (1.1.0) ; réajusté, c'est la sienne qui sert (les mêmes nombres, sans la valeur de la 1.1.0)",
         identical(s_v$greffon$version, "1.0.0") && identical(s_v$greffon$installe$version, "1.1.0") && identical(refait$greffon$version, "1.0.0") &&
           !"version" %in% refait$valeurs$nom && proche(refait$coefficients$estimation, m_occ$coefficients$estimation, 1e-12))
maj <- demande("greffon_mettre_a_jour", list(projet = pid, id = "occupancy"))
d_maj <- Find(function(d) identical(d$name, "OC1"), p()$models)
refait2 <- demande("ajuster", list(projet = pid, numero = d_maj$number))
verifier("passé à la version installée : ses modèles redéfinis avec elle (leurs entrées et options gardées), réajustés avec la 1.1.0 ; Ctrl+Z le défait",
         length(maj$numeros) >= 1 && identical(d_maj$options$greffon$version, "1.1.0") && proche(d_maj$options$entrees$occupancy, u("forest")) &&
           identical(refait2$greffon$version, "1.1.0") && "version" %in% refait2$valeurs$nom && grepl("1.1.0", premier_texte(etat$annulations[[pid]][[1]]$etiquette), fixed = TRUE))
demande("greffon_installer", list(chemin = a1, empreinte = g_occ$empreinte))
verifier("réinstaller la 1.0.0 à la place : l'installée de nouveau la plus ancienne (le projet garde la sienne, 1.1.0)",
         identical(Find(function(d) identical(d$nom, "OC1"), demande("projet", list(projet = pid))$modeles)$greffon$installe$version, "1.0.0"))

# ---------------------------------------------------------------- l'organisation, la politique, les paquets, la version de Nuée

cat("Les greffons de l'organisation, la politique DisableCustomCode\n")
ecrire_greffon(fichier("organisation", "mk"), g_mk$fichiers)
oublier(g_mk$empreinte)
gl <- demande("greffons")$liste
org <- Find(function(g) identical(g$source, "organisation"), gl)
mien <- Find(function(g) identical(g$source, "utilisateur") && identical(g$id, "mann-kendall"), gl)
verifier("un greffon de l'organisation : permis sans approbation ; le même id chez l'utilisateur, remplacé par le sien",
         identical(org$id, "mann-kendall") && isTRUE(org$permis) && identical(mien$etat, "remplace") &&
           identical(demande("ajuster", def_mk(nom = "MK6"))$type, "PLUGIN"))
verifier("installer un id que l'organisation donne : refusé, en le disant",
         grepl("organisation donne déjà ce greffon", demande("greffon_lire", list(chemin = file.path(mk_dossier, "plugin.json")))$raison))
Sys.setenv(NUEE_SANS_CODE = "1")
verifier("avec la politique DisableCustomCode : le greffon de l'organisation s'ajuste toujours ; celui de l'utilisateur est refusé (une définition, un modèle du projet), ne s'installe pas",
         identical(demande("ajuster", def_mk(nom = "MK7"))$type, "PLUGIN") && grepl("DisableCustomCode", refus("ajuster", def_occ(nom = "OC8"))) &&
           grepl("DisableCustomCode", refus("ajuster", list(projet = pid, numero = d_maj$number))) &&
           grepl("DisableCustomCode", demande("greffon_lire", list(chemin = a1))$raison) && isFALSE(demande("greffons")$installation) &&
           isFALSE(Find(function(d) identical(d$nom, "OC1"), demande("projet", list(projet = pid))$modeles)$greffon$approuve))
Sys.unsetenv("NUEE_SANS_CODE")
unlink(fichier("organisation", "mk"), recursive = TRUE)
approuver(g_mk$empreinte)
# un paquet qui manque, une Nuée trop ancienne
manque <- variante(packages = list("paquetQuiNExistePas"))
g_manque <- faire_greffon(list(list(chemin = "plugin.json", texte = manque), list(chemin = "occupancy.R", texte = textes_occ[["occupancy.R"]])), "fichier")
approuver(g_manque$empreinte)
d_manque <- d_occ
d_manque$options$greffon[c("fichiers", "empreinte")] <- list(g_manque$fichiers, g_manque$empreinte)
e_manque <- tryCatch(ajuster_plugin(p(), d_manque), error = function(e) e)
pq_manque <- paquets_greffon(g_manque)
verifier(sprintf("un paquet qui manque : listé absent, et l'ajustement refusé en le nommant (« %s »)", substr(conditionMessage(e_manque), 1, 80)),
         inherits(e_manque, "greffon_paquets") && identical(as.character(e_manque$details$paquets_manquants), "paquetQuiNExistePas") &&
           grepl("install.packages(\"paquetQuiNExistePas\")", conditionMessage(e_manque), fixed = TRUE) && isFALSE(pq_manque$present))
recent <- faire_greffon(list(list(chemin = "plugin.json", texte = variante(nuee = "9.0.0")), list(chemin = "occupancy.R", texte = textes_occ[["occupancy.R"]])), "fichier")
verifier("un greffon qui demande une Nuée plus récente : il le dit, ne s'installe pas",
         isTRUE(greffon_trop_recent(recent)) && grepl("Nuée 9.0.0 ou plus récent", installation_greffon(recent)$raison))
# retirer
ret <- demande("greffon_retirer", list(id = "mann-kendall"))
verifier("retirer un greffon : son dossier supprimé, il n'est plus listé ; son approbation reste (les projets qui le gardent se refont)",
         identical(ret$retire, "mann-kendall") && !dir.exists(fichier("greffons", "mann-kendall")) && greffon_permis(g_mk$empreinte) &&
           !any(vapply(demande("greffons")$liste, function(g) identical(g$id, "mann-kendall"), TRUE)) &&
           grepl("Pas de greffon", refus("greffon_retirer", list(id = "mann-kendall"))))

unlink(dossier, recursive = TRUE)
cat(sprintf("%d vérifications\n", n_verifs))
cat(if (echecs) sprintf("%d vérification(s) en échec\n", echecs) else "Tout est bon\n")
quit(status = if (echecs) 1 else 0)
