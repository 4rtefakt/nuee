# Les modèles écrits en code R par l'utilisateur (type CODE, 1.1) : tout modèle que R sait ajuster
# (lm, glm, nls, mgcv, nlme, glmmTMB, unmarked, brms…), avec les paquets du R que Nuée utilise. Le
# code est dans d$options$code ; ses observations sont celles des autres modèles (toutes, cochées,
# choisies par une condition). Il voit `donnees`, un data.frame des observations du modèle avec les
# noms des variables du projet (les qualitatives en facteurs, leurs catégories dans l'ordre du
# projet) et ses identifiants secondaires (en texte) ; sa dernière valeur est le modèle ajusté, ou
# une liste dont Nuée reconnaît `modele` (l'ajustement), `valeurs` (des nombres nommés : des
# résultats clés, que les documents citent) et `tableaux` (des data.frame nommés, montrés et
# exportés). Les colonnes que le code nomme (d$options$colonnes, des groupements comme ceux des autres
# analyses : list(by = "variable", variable = …), list(by = "id", id = …)) sont relevées dans le code
# sans l'exécuter, pour les étapes des données et la mise à jour (etapes.R : variables_definition,
# identifiants_definition).
#
# La confiance (docs/deploiement.md, « Opening files from elsewhere ») : ouvrir un fichier n'exécute
# jamais son code. Seul s'exécute le code écrit ou approuvé sur cet ordinateur par cet utilisateur :
# son empreinte SHA-256 (empreinte_code) parmi celles qu'il a approuvées, que l'application garde
# (main.js : reglages.json) et donne au moteur (serveur.R : demande code_approuve). Le moteur refuse
# tout autre code (exiger_code_permis), quel que soit le chemin qui ajuste un modèle (ouvrir un
# projet, mettre à jour les données, fusionner, une ancienne version, Ce qui a changé, l'export vers
# R qui refait le calcul) : rien n'arrive à R sans approbation, quoi que fasse l'interface. Une
# politique informatique (main.js : DWORD DisableCustomCode = 1, NUEE_SANS_CODE pour R) retire le type.
#
# C'est une analyse de analyses.R : definir_code (son code et ses colonnes, à la définition),
# ajuster_code, resultat_code, texte_methodes_code, tables_code, predire_code (quand predict(fit,
# newdata) marche), ecrire_script_code (code_script.R), entrees_code (resultats_cles.R) et verifier_code.

CODE_IMAGES_MAX <- 20     # (les graphiques que le code dessine, gardés au plus)
CODE_SORTIE_MAX <- 3000   # (les lignes de sa sortie gardées)
CODE_LIGNES_TABLEAU <- 1000  # (les lignes d'un de ses tableaux que l'interface reçoit ; l'export les a toutes)
# (les paquets qui sont R lui-même, cité comme R : pas cités à part)
PAQUETS_DE_R <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")

# ---------------------------------------------------------------- la confiance

# Les empreintes approuvées, gardées le temps de la séance (rechargées avec le moteur, elles restent :
# console.R, recharger_moteur).
if (!exists("APPROBATIONS_CODE", envir = globalenv(), inherits = FALSE)) {
  APPROBATIONS_CODE <- new.env(parent = emptyenv())
  APPROBATIONS_CODE$empreintes <- character(0)
}

# Le texte d'un code tel qu'il est gardé et approuvé : en UTF-8, ses fins de ligne en \n (un code collé
# depuis Windows a la même empreinte que le même code tapé).
texte_code <- function(code) gsub("\r\n?", "\n", enc2utf8(paste(as.character(unlist(code)), collapse = "\n")))

# Son empreinte : son SHA-256, quel que soit le R. Celle que l'application garde est celle que le moteur
# calcule (demande empreinte_code). Un R d'avant 4.5 n'a pas tools::sha256sum (etapes.R : empreinte_sha
# y prend un MD5, bon pour les fichiers de données) : sha256_r le calcule alors, pour que l'approbation
# d'un code tienne quand on change de R (Fichier › Réglages…, le R qu'utilise Nuée).
empreinte_code <- function(code) {
  octets <- charToRaw(texte_code(code))
  if (is.null(tryCatch(getExportedValue("tools", "sha256sum"), error = function(e) NULL))) return(sha256_r(octets))
  empreinte_sha(octets = octets)
}

# Le SHA-256 d'octets (FIPS 180-4) en R seul, pour quelques kilo-octets (un code : 4 Ko en 0,1 s). Les
# mots de 32 bits sont des doubles entiers dans [0, 2^32) : leurs opérations bit à bit se font sur
# leurs deux moitiés de 16 bits (les entiers de R n'ont pas 2^31, leur NA).
sha256_r <- function(octets) {
  M <- 4294967296
  ou_x <- function(a, b) bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
  et <- function(a, b) bitwAnd(a %/% 65536, b %/% 65536) * 65536 + bitwAnd(a %% 65536, b %% 65536)
  rotd <- function(x, n) x %/% 2^n + (x %% 2^n) * 2^(32 - n)
  K <- c(0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
         0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
         0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
         0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
         0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
         0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
         0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
         0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2)
  H <- c(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)
  n <- length(octets)
  # (complété : 0x80, des zéros, la longueur en bits sur 8 octets, gros-boutiste)
  longueur <- numeric(8)
  b <- n * 8
  for (i in 8:1) { longueur[i] <- b %% 256; b <- b %/% 256 }
  m <- c(as.numeric(as.integer(octets)), 128, numeric((55 - n) %% 64), longueur)
  W <- numeric(64)
  for (debut in seq(0, length(m) - 1, by = 64)) {
    bloc <- m[debut + 1:64]
    for (t in 1:16) W[t] <- sum(bloc[(t - 1) * 4 + 1:4] * c(16777216, 65536, 256, 1))
    for (t in 17:64) {
      s0 <- ou_x(ou_x(rotd(W[t - 15], 7), rotd(W[t - 15], 18)), W[t - 15] %/% 8)
      s1 <- ou_x(ou_x(rotd(W[t - 2], 17), rotd(W[t - 2], 19)), W[t - 2] %/% 1024)
      W[t] <- (W[t - 16] + s0 + W[t - 7] + s1) %% M
    }
    v <- H
    for (t in 1:64) {
      e <- v[5]; a <- v[1]
      S1 <- ou_x(ou_x(rotd(e, 6), rotd(e, 11)), rotd(e, 25))
      ch <- ou_x(et(e, v[6]), et(M - 1 - e, v[7]))
      T1 <- (v[8] + S1 + ch + K[t] + W[t]) %% M
      S0 <- ou_x(ou_x(rotd(a, 2), rotd(a, 13)), rotd(a, 22))
      maj <- ou_x(ou_x(et(a, v[2]), et(a, v[3])), et(v[2], v[3]))
      v <- c((T1 + S0 + maj) %% M, v[1:3], (v[4] + T1) %% M, v[5:7])
    }
    H <- (H + v) %% M
  }
  paste(sprintf("%04x%04x", H %/% 65536, H %% 65536), collapse = "")
}

# Les empreintes approuvées ajoutées (ou, `remplacer`, la liste entière : celle de l'application) ;
# une empreinte qui n'en est pas une est laissée de côté. Rend leur nombre.
approuver_codes <- function(empreintes, remplacer = FALSE) {
  e <- as.character(unlist(empreintes))
  e <- e[!is.na(e) & grepl("^[0-9a-f]{64}$", e)]
  APPROBATIONS_CODE$empreintes <- unique(c(if (!remplacer) APPROBATIONS_CODE$empreintes, e))
  length(APPROBATIONS_CODE$empreintes)
}

code_approuve <- function(code) empreinte_code(code) %in% APPROBATIONS_CODE$empreintes

# La politique qui retire le type à tous (main.js : politique DisableCustomCode).
code_interdit <- function() identical(Sys.getenv("NUEE_SANS_CODE"), "1")

# Une erreur d'une classe à elle (serveur.R la rend avec ce qu'il faut à l'interface).
refus_code <- function(classe, message, ...) structure(class = c(classe, "error", "condition"), list(message = message, call = NULL, ...))

refuser_si_interdit <- function() {
  if (code_interdit()) {
    stop(refus_code("code_interdit", traduire("Les modèles de code R sont désactivés sur cet ordinateur par une politique de votre organisation (DisableCustomCode).",
                                              "R-code models are turned off on this computer by your organisation's policy (DisableCustomCode).")))
  }
}

# Le code de la définition `nom` peut-il s'exécuter ? Sinon une erreur : la politique, ou un code
# qu'aucun utilisateur de cet ordinateur n'a approuvé (code_non_approuve, avec son empreinte).
exiger_code_permis <- function(code, nom = "") {
  refuser_si_interdit()
  if (!code_approuve(code)) {
    stop(refus_code("code_non_approuve", sprintf(traduire("Le modèle %s a du code R écrit ailleurs, qui n'a pas été approuvé sur cet ordinateur : il ne s'exécute pas tant que vous ne l'avez pas lu et approuvé (Montrer le code, Exécuter).",
                                                          "Model %s has R code written elsewhere, not approved on this computer: it doesn't run until you have read and approved it (Show the code, Run it)."),
                                                 premier_texte(nom, "?")),
                    empreinte = empreinte_code(code)))
  }
}

# ---------------------------------------------------------------- la définition

# Les noms d'un code (ses symboles et ses chaînes), lus sans l'exécuter.
noms_code <- function(code) {
  exprs <- tryCatch(parse(text = texte_code(code), keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  chaines <- function(e) {
    if (is.character(e)) return(e)
    if (is.call(e) || is.expression(e) || is.pairlist(e)) return(unlist(lapply(as.list(e), chaines)))
    character(0)
  }
  unique(c(all.names(exprs), chaines(exprs)))
}

# Les colonnes du projet que le code nomme : ses variables, puis ses identifiants, en groupements.
colonnes_code <- function(projet, code) {
  noms <- noms_code(code)
  vars <- Filter(function(v) as.character(v$ids[1] %||% "") %in% noms, projet$variables)
  ids <- intersect(noms, setdiff(as.character(projet$observation_id_names %||% character(0))[-1], vapply(projet$variables, function(v) as.character(v$ids[1] %||% ""), "")))
  c(lapply(vars, function(v) list(by = "variable", variable = as.numeric(v$uid))), lapply(ids, function(n) list(by = "id", id = n)))
}

# (definir_modele, nouveau.R : la définition d'un modèle de code, complétée) Son code, sans rien
# d'autre, et les colonnes qu'il nomme ; un code vide est refusé, et le type l'est par la politique.
definir_code <- function(projet, d) {
  refuser_si_interdit()
  code <- texte_code(d$options$code %||% "")
  if (!nzchar(trimws(code))) stop(traduire("Écrivez le code R du modèle : sa dernière valeur est le modèle ajusté.", "Write the model's R code: its last value is the fitted model."), call. = FALSE)
  d$options <- list(code = code, colonnes = colonnes_code(projet, code))
  d
}

# Il vérifie lui-même ses données (diagnostic.R : verifier_donnees) : le code en fait ce qu'il veut.
verifier_code <- function(projet, d) list(variables = data.frame(), observations = data.frame())

# ---------------------------------------------------------------- les données et l'exécution

# Les données que le code voit : les observations `observations` (celles du modèle), une colonne par
# variable du projet sous son nom (les qualitatives en facteurs, toutes leurs catégories dans l'ordre
# du projet), puis ses identifiants secondaires en texte (vide : manquant). Les lignes sont numérotées
# de 1 à n, dans l'ordre du projet.
donnees_code <- function(projet, d, observations = d$observations) {
  obs <- as.numeric(unlist(observations))
  lignes <- match(nom_numero(obs), rownames(projet$donnees))
  colonnes <- list()
  for (v in projet$variables) {
    nom <- as.character(v$ids[1] %||% "")
    if (!nzchar(nom) || nom %in% names(colonnes) || !nom_numero(v$uid) %in% colnames(projet$donnees)) next
    x <- projet$donnees[lignes, nom_numero(v$uid)]
    colonnes[[nom]] <- if (length(v$categories)) factor(as.character(v$categories)[x], levels = as.character(v$categories)) else unname(as.numeric(x))
  }
  noms_id <- as.character(projet$observation_id_names %||% character(0))
  for (k in seq_along(noms_id)[-1]) {
    n <- noms_id[k]
    if (!nzchar(n) || startsWith(n, "$") || n %in% names(colonnes)) next
    x <- identifiants_observations(projet, k, obs)
    x[!is.na(x) & !nzchar(trimws(x))] <- NA
    colonnes[[n]] <- x
  }
  if (!length(colonnes)) return(data.frame(row.names = seq_along(obs)))
  as.data.frame(colonnes, check.names = FALSE, stringsAsFactors = FALSE)
}

# Les paquets qu'un code nomme : library(), require(), requireNamespace(), loadNamespace(), paquet::.
paquets_nommes <- function(e) {
  if (is.call(e)) {
    f <- e[[1]]
    nom <- if (is.name(f)) as.character(f) else ""
    ici <- if (nom %in% c("::", ":::") && length(e) >= 2) as.character(e[[2]])
           else if (nom %in% c("library", "require", "requireNamespace", "loadNamespace", "attachNamespace") && length(e) >= 2 && (is.name(e[[2]]) || is.character(e[[2]]))) as.character(e[[2]])
    return(unique(c(ici, unlist(lapply(as.list(e), paquets_nommes)))))
  }
  if (is.expression(e) || is.pairlist(e)) return(unique(unlist(lapply(as.list(e), paquets_nommes))))
  character(0)
}

# Le paquet de la classe d'un objet : celui de ses méthodes (summary.lme : nlme), d'une classe S4 celui
# qu'elle porte ; character(0) sans.
paquet_classe <- function(x) {
  if (is.null(x)) return(character(0))
  if (isS4(x)) return(as.character(attr(class(x), "package") %||% character(0)))
  for (cl in oldClass(x)) for (g in c("summary", "print", "predict", "logLik", "coef")) {
    f <- utils::getS3method(g, cl, optional = TRUE)
    if (is.function(f)) {
      n <- environmentName(environment(f))
      if (nzchar(n) && !identical(n, "R_GlobalEnv")) return(n)
    }
  }
  character(0)
}

# Le numéro de ligne d'une erreur de lecture de R (« <text>:3:7: unexpected symbol »), NA sans.
ligne_lecture <- function(message) {
  x <- regmatches(message, regexec("<text>:(\\d+):", message))[[1]]
  if (length(x) == 2) as.integer(x[2]) else NA_integer_
}

# D'où viennent les appels en cours (sys.calls()) : le fichier et la ligne de chacun qui a les siens
# (lu avec keep.source), du plus extérieur au plus intérieur.
pile_appels <- function(appels) {
  Filter(Negate(is.null), lapply(appels, function(cl) {
    sr <- attr(cl, "srcref")
    f <- if (!is.null(sr)) attr(sr, "srcfile")
    if (!is.null(sr)) list(fichier = as.character(if (is.environment(f)) f$filename else "") %||% "", ligne = as.integer(sr[1]))
  }))
}

# Le code évalué comme la console le fait (console.R : evaluer_console), sur `donnees` : dans un
# environnement neuf dont le parent est l'environnement global (library() et paquet:: prennent les
# paquets du R utilisé) ; chaque expression à son tour, celle dont la valeur est visible affichée comme
# dans la console (un graphique lattice ou ggplot y est dessiné), sauf la dernière, dont la valeur est
# le modèle. Sa sortie, ses messages, ses avertissements et les graphiques qu'il dessine (des PNG, comme
# la console) sont gardés ; une erreur arrête l'évaluation (`erreur`, le message de R, `ligne` celle de
# l'expression, `paquet` celui qui manque, `pile` les fichiers et les lignes des appels en cours quand
# elle est survenue, du plus extérieur au plus intérieur). `paquets` : ceux qu'il nomme (paquets_nommes),
# sans ceux de R. `objets` : d'autres objets que le code voit à côté de `donnees` ; `parent` :
# l'environnement parent du sien (un greffon : celui où ses fichiers ont été lus, greffons.R).
evaluer_code <- function(code, donnees, objets = list(), parent = globalenv()) {
  r <- list(valeur = NULL, sortie = character(0), messages = character(0), avertissements = character(0), images = character(0),
            erreur = NULL, ligne = NA_integer_, paquet = NULL, paquets = character(0), pile = list())
  exprs <- tryCatch(parse(text = texte_code(code), keep.source = TRUE), error = function(e) e)
  if (inherits(exprs, "error")) {
    r$erreur <- conditionMessage(exprs)
    r$ligne <- ligne_lecture(r$erreur)
    return(r)
  }
  if (!length(exprs)) {
    r$erreur <- traduire("Le code n'a aucune expression : sa dernière valeur doit être le modèle ajusté.", "The code has no expression: its last value must be the fitted model.")
    return(r)
  }
  r$paquets <- setdiff(paquets_nommes(exprs), PAQUETS_DE_R)
  env <- new.env(parent = parent)
  assign("donnees", donnees, envir = env)
  for (k in names(objets)) assign(k, objets[[k]], envir = env)
  dossier <- tempfile("code-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  grDevices::png(file.path(dossier, "graphique-%03d.png"), width = 900, height = 600, res = 110)
  dev <- grDevices::dev.cur()
  dessin <- FALSE
  hooks <- list(plot.new = getHook("plot.new"), grid.newpage = getHook("grid.newpage"))
  setHook("plot.new", function() dessin <<- TRUE, "replace")
  setHook("grid.newpage", function() dessin <<- TRUE, "replace")
  ancien <- options(width = 100)
  # (remis quoi qu'il arrive ; dans l'ordre normal, plus bas, avant de lire les graphiques)
  on.exit({
    options(ancien)
    setHook("plot.new", hooks$plot.new, "replace")
    setHook("grid.newpage", hooks$grid.newpage, "replace")
    if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
  }, add = TRUE, after = FALSE)
  sortie <- character(0)
  messages <- character(0)
  avertissements <- character(0)
  refs <- attr(exprs, "srcref")
  n <- length(exprs)
  pile <- list()
  for (k in seq_len(n)) {
    res <- withCallingHandlers(
      tryCatch(withCallingHandlers({
        s <- utils::capture.output(v <- withVisible(eval(exprs[[k]], env)))
        if (k < n && v$visible) s <- c(s, utils::capture.output(print(v$value)))
        sortie <- c(sortie, s)
        if (k == n) r$valeur <- v$value
        NULL
      }, error = function(e) pile <<- pile_appels(sys.calls())), error = function(e) e),
      warning = function(w) { avertissements <<- c(avertissements, conditionMessage(w)); invokeRestart("muffleWarning") },
      message = function(m) { messages <<- c(messages, sub("\n$", "", conditionMessage(m))); invokeRestart("muffleMessage") })
    if (inherits(res, "error")) {
      appel <- conditionCall(res)
      r$erreur <- if (!is.null(appel)) sprintf("%s (%s)", conditionMessage(res), paste(deparse(appel, nlines = 1L, width.cutoff = 80L), collapse = ""))
                  else conditionMessage(res)
      r$ligne <- if (length(refs) >= k) as.integer(refs[[k]][1]) else NA_integer_
      r$pile <- pile
      if (inherits(res, "packageNotFoundError")) r$paquet <- as.character(res$package %||% "")
      break
    }
  }
  options(ancien)
  setHook("plot.new", hooks$plot.new, "replace")
  setHook("grid.newpage", hooks$grid.newpage, "replace")
  if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)
  fichiers <- sort(list.files(dossier, "^graphique-\\d+\\.png$", full.names = TRUE))
  if (dessin) r$images <- vapply(utils::head(fichiers, CODE_IMAGES_MAX), function(f) paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(f, "raw", file.size(f)))), "")
  if (length(sortie) > CODE_SORTIE_MAX) sortie <- c(utils::head(sortie, CODE_SORTIE_MAX), sprintf(traduire("… (%d lignes de plus)", "… (%d more lines)"), length(sortie) - CODE_SORTIE_MAX))
  r[c("sortie", "messages", "avertissements")] <- list(sortie, messages, avertissements)
  r
}

# L'erreur d'un code qui s'arrête : le message de R et sa ligne ; un paquet qui manque dit comment
# l'installer pour le R que Nuée utilise.
erreur_code <- function(ev) {
  version <- paste(R.version$major, R.version$minor, sep = ".")
  message <- if (is.na(ev$ligne)) sprintf(traduire("Le code R s'est arrêté : %s", "The R code stopped: %s"), ev$erreur)
             else sprintf(traduire("Le code R s'est arrêté à la ligne %d : %s", "The R code stopped at line %d: %s"), ev$ligne, ev$erreur)
  if (length(ev$paquet) && nzchar(ev$paquet)) {
    message <- paste(message, sprintf(traduire("Le paquet %s n'est pas installé pour le R que Nuée utilise (R %s) : installez-le pour ce R (install.packages(\"%s\"), dans R ou dans la console R de Nuée), puis ajustez de nouveau.",
                                               "The package %s isn't installed for the R Nuée uses (R %s): install it for that R (install.packages(\"%s\"), in R or in Nuée's R console), then fit again."),
                                      ev$paquet, version, ev$paquet))
  }
  refus_code("erreur_code", message, ligne = if (is.na(ev$ligne)) NULL else ev$ligne)
}

# ---------------------------------------------------------------- ce que Nuée tire du modèle

# (une valeur, sans avertissement ; NULL quand l'objet ne le permet pas)
essai_code <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NULL)
nombre_code <- function(x) {
  x <- suppressWarnings(as.numeric(unlist(x)))
  if (length(x) == 1 && is.finite(x)) x
}

# La valeur rendue par le code est-elle la forme liste (modele, valeurs, tableaux) ? Une liste sans
# classe qui a au moins l'un de ces noms ; un modèle (lm…) est une liste avec une classe.
forme_liste_code <- function(v) is.list(v) && is.null(oldClass(v)) && any(c("modele", "valeurs", "tableaux") %in% names(v))

# `valeurs` : des nombres nommés (un vecteur, ou une liste de nombres seuls), chacun d'un nom à lui.
valeurs_code <- function(v) {
  if (is.null(v)) return(NULL)
  refus <- function(fr, en, ...) stop(sprintf(traduire(fr, en), ...), call. = FALSE)
  if (is.list(v) && !is.data.frame(v)) {
    if (!all(vapply(v, function(x) is.numeric(x) && length(x) == 1, TRUE))) refus("`valeurs` : des nombres nommés, comme c(densite = 3.2, pente = -0.4).", "`valeurs`: named numbers, such as c(density = 3.2, slope = -0.4).")
    v <- unlist(v)
  }
  if (!is.numeric(v) || is.matrix(v) || is.data.frame(v)) refus("`valeurs` : des nombres nommés, comme c(densite = 3.2, pente = -0.4).", "`valeurs`: named numbers, such as c(density = 3.2, slope = -0.4).")
  noms <- names(v)
  if (is.null(noms) || anyNA(noms) || !all(nzchar(noms))) refus("Chaque nombre de `valeurs` a un nom : c(densite = 3.2), pas c(3.2).", "Each number of `valeurs` has a name: c(density = 3.2), not c(3.2).")
  if (anyDuplicated(noms)) refus("Des noms de `valeurs` reviennent : %s.", "Names of `valeurs` repeat: %s.", paste(unique(noms[duplicated(noms)]), collapse = ", "))
  stats::setNames(as.numeric(v), noms)
}

# `tableaux` : des data.frame (ou des matrices) nommés ; un seul tableau vaut une liste d'un tableau.
# Les noms des lignes qui ne sont pas des numéros deviennent la première colonne.
tableaux_code <- function(t) {
  if (is.null(t)) return(list())
  if (is.data.frame(t) || is.matrix(t)) t <- list(tableau = t)
  if (!is.list(t)) stop(traduire("`tableaux` : des data.frame nommés, comme list(effectifs = d).", "`tableaux`: named data.frames, such as list(counts = d)."), call. = FALSE)
  noms <- names(t) %||% rep("", length(t))
  noms[is.na(noms) | !nzchar(noms)] <- paste0(traduire("tableau", "table"), which(is.na(noms) | !nzchar(noms)))
  out <- lapply(t, function(x) {
    if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (!is.data.frame(x)) stop(traduire("`tableaux` : des data.frame nommés, comme list(effectifs = d).", "`tableaux`: named data.frames, such as list(counts = d)."), call. = FALSE)
    rn <- rownames(x)
    if (nrow(x) && !identical(rn, as.character(seq_len(nrow(x))))) x <- cbind(stats::setNames(data.frame(rn, stringsAsFactors = FALSE), traduire("ligne", "row")), x)
    rownames(x) <- NULL
    x
  })
  stats::setNames(out, make.unique(noms))
}

# Le résumé du modèle, en texte : print(summary(fit)), sinon print(fit).
resume_code <- function(fit) {
  if (is.null(fit)) return(character(0))
  ancien <- options(width = 100)
  on.exit(options(ancien))
  s <- tryCatch(suppressWarnings(utils::capture.output(print(summary(fit)))), error = function(e) NULL)
  if (is.null(s)) s <- tryCatch(suppressWarnings(utils::capture.output(print(fit))), error = function(e) conditionMessage(e))
  if (length(s) > CODE_SORTIE_MAX) s <- c(utils::head(s, CODE_SORTIE_MAX), "…")
  s
}

# Les coefficients d'une matrice de coef(summary(fit)) (une partie : cond, zi… de glmmTMB) : ses
# colonnes reconnues par leur nom (Estimate, Std. Error, t value, Pr(>|t|) ; Value, Std.Error, DF,
# t-value, p-value de nlme ; coef, exp(coef), se(coef), robust se, z, Pr(>|z|) de survival…), la
# première étant l'estimation ; NULL quand aucune n'est une erreur-type (une autre colonne n'en tient
# pas lieu : exp(coef) de coxph ; les coefficients de multinom, en tableau, une colonne par terme).
# L'erreur-type robuste d'abord (coxph(robust = TRUE) teste avec elle) ; la statistique n'est jamais
# une colonne au nom d'un terme ou d'une variable du modèle (une variable z ou t). L'intervalle à 95 % :
# de la loi t à leurs degrés de liberté quand le test est un t (la colonne DF, sinon ceux des résidus),
# sinon de la loi normale (une colonne DF y est celle d'un χ² : coxph avec frailty()).
coefficients_matrice <- function(M, partie, fit, prefixer) {
  noms <- colnames(M) %||% character(ncol(M))
  termes <- c(rownames(M), names(essai_code(stats::coef(fit))), essai_code(all.vars(stats::formula(fit))))
  colonne <- function(motif, exclues = character(0)) {
    k <- setdiff(grep(motif, noms, ignore.case = TRUE), which(noms %in% exclues))
    if (length(k)) k[1]
  }
  k_se <- colonne("^robust[ .]?se$") %||% colonne("std\\.? ?err|^se$|^s\\.e\\.?$|^se\\(coef\\)$")
  if (is.null(k_se)) return(NULL)
  k_stat <- colonne("^(t|z)([ .-]?(value|ratio|stat))?$|wald", termes)
  k_p <- colonne("^pr\\(|p[ .-]?val|^p$")
  k_ddl <- colonne("^df$|^ddl$|^d\\.f\\.?$")
  val <- function(k) if (!is.null(k)) unname(as.numeric(M[, k])) else rep(NA_real_, nrow(M))
  est <- val(1L)
  se <- val(k_se)
  t_test <- !is.null(k_stat) && grepl("^t", noms[k_stat], ignore.case = TRUE)
  ddl <- if (t_test) val(k_ddl) else rep(NA_real_, nrow(M))
  if (t_test && all(is.na(ddl))) ddl <- rep(ddl_residus_code(fit), nrow(M))
  q <- ifelse(is.finite(ddl) & ddl > 0, stats::qt(0.975, pmax(ddl, 1e-8)), stats::qnorm(0.975))
  terme <- rownames(M) %||% paste0("b", seq_len(nrow(M)))
  if (prefixer && nzchar(partie)) terme <- paste0(partie, ".", terme)
  data.frame(partie = partie, terme = terme, estimation = est, erreur_type = se, statistique = val(k_stat), ddl = ddl, p = val(k_p),
             bas = est - q * se, haut = est + q * se, stringsAsFactors = FALSE)
}

# Les degrés de liberté des résidus d'un modèle dont le test est un t : df.residual(), sinon N − p
# d'un gls ou d'un gnls de nlme (df.residual() n'en dit rien ; leurs valeurs p et intervals() les
# prennent) ; NA sans.
ddl_residus_code <- function(fit) {
  ddl <- nombre_code(essai_code(stats::df.residual(fit)))
  if (is.null(ddl) && inherits(fit, "gls") && is.list(fit$dims)) ddl <- nombre_code(fit$dims$N - fit$dims$p)
  ddl %||% NA_real_
}

# Les coefficients du modèle : coef(summary(fit)) quand c'est une matrice de nombres ou une liste de
# matrices (glmmTMB : cond, zi, disp ; leurs termes préfixés de leur partie) dont chacune a une colonne
# d'erreurs-types (coefficients_matrice), sinon coef(fit) avec les erreurs-types de vcov(fit) et
# l'intervalle de la loi normale ; des coefficients en tableau, une ligne par catégorie de la réponse
# (multinom), dans l'ordre de vcov(), « catégorie:terme ». NULL quand l'objet n'en a pas.
coefficients_code <- function(fit) {
  if (is.null(fit)) return(NULL)
  cs <- essai_code(stats::coef(summary(fit)))
  if (is.data.frame(cs) && length(cs) && all(vapply(cs, is.numeric, TRUE))) cs <- as.matrix(cs)
  matrice <- function(x) is.matrix(x) && is.numeric(x) && nrow(x) > 0
  if (is.list(cs) && !is.data.frame(cs)) cs <- Filter(Negate(is.null), cs)
  parties <- if (matrice(cs)) list(cs) else if (is.list(cs) && length(cs) && all(vapply(cs, matrice, TRUE))) cs
  if (!is.null(parties)) {
    noms <- names(parties) %||% rep("", length(parties))
    noms[is.na(noms)] <- ""
    tables <- lapply(seq_along(parties), function(k) coefficients_matrice(parties[[k]], noms[k], fit, length(parties) > 1))
    if (!any(vapply(tables, is.null, TRUE))) {
      t <- do.call(rbind, tables)
      if (nrow(t)) return(structure(t, source = "summary"))  # (d'où ils viennent : l'export vers R refait le même appel)
    }
  }
  b <- essai_code(stats::coef(fit))
  matrice_b <- is.matrix(b) && is.numeric(b) && length(rownames(b)) && length(colnames(b))
  if (matrice_b) b <- stats::setNames(as.vector(t(b)), paste(rep(rownames(b), each = ncol(b)), rep(colnames(b), nrow(b)), sep = ":"))
  if (!is.numeric(b) || !length(b) || is.matrix(b)) return(NULL)
  V <- essai_code(stats::vcov(fit))
  if (is.matrix(V) && length(names(b)) && all(names(b) %in% rownames(V)) && all(names(b) %in% colnames(V))) V <- V[names(b), names(b), drop = FALSE]
  else if (matrice_b) V <- NULL  # (sans les noms de vcov(), l'ordre de ses lignes n'est pas sûr)
  se <- if (is.matrix(V) && nrow(V) == length(b) && ncol(V) == length(b)) sqrt(pmax(0, diag(V))) else rep(NA_real_, length(b))
  z <- stats::qnorm(0.975)
  structure(data.frame(partie = "", terme = names(b) %||% paste0("b", seq_along(b)), estimation = unname(b), erreur_type = unname(se), statistique = NA_real_,
                       ddl = NA_real_, p = NA_real_, bas = unname(b) - z * unname(se), haut = unname(b) + z * unname(se), stringsAsFactors = FALSE),
            source = if (matrice_b) "vcov_matrice" else "vcov")
}

# Le nombre d'observations, la log-vraisemblance et ses degrés de liberté, l'AIC et le BIC : chacun
# quand l'objet le donne.
statistiques_code <- function(fit) {
  if (is.null(fit)) return(list())
  ll <- essai_code(stats::logLik(fit))
  Filter(Negate(is.null), list(nobs = nombre_code(essai_code(stats::nobs(fit))), logLik = nombre_code(ll), ddl = nombre_code(if (!is.null(ll)) attr(ll, "df")),
                               AIC = nombre_code(essai_code(stats::AIC(fit))), BIC = nombre_code(essai_code(stats::BIC(fit)))))
}

# Le nom de la réponse : le membre de gauche de formula(fit) quand c'est un nom ; NULL sinon.
reponse_code <- function(fit) {
  f <- if (!is.null(fit)) essai_code(stats::formula(fit))
  if (inherits(f, "formula") && length(f) == 3 && is.name(f[[2]])) as.character(f[[2]])
}

# Les lignes de `donnees` que des noms désignent, quand ce sont leurs numéros (donnees_code numérote
# ses `n` lignes de 1 à n), chacune une fois ; NULL sinon.
lignes_noms_code <- function(noms, n) {
  noms <- as.character(noms)
  k <- suppressWarnings(as.integer(noms))
  if (length(k) && !anyNA(k) && identical(as.character(k), noms) && all(k >= 1 & k <= n) && !anyDuplicated(k)) k
}

# Les lignes de `donnees` des valeurs ajustées `f` (fitted()) et des résidus `e` (residuals()) du
# modèle, dans leur ordre (`lignes`) : leurs noms (lm, glm, gls… gardent ceux des lignes de leurs
# données, même quand le code réordonne `donnees` ou que na.action en retire), sinon ceux des lignes
# du cadre que le modèle garde (fit$model : un gam de mgcv) ; d'un greffon qui ne les nomme pas, leur
# place (le contrat des greffons : dans l'ordre des données, sans les lignes que na.action() dit omises,
# par leur place). NULL quand ce ne sont pas ces numéros (lme les nomme par ses groupes, nls ne les
# nomme pas) : chaque valeur n'est pas rapportée à une observation, plutôt qu'à une autre. `autres` :
# la réponse `y` du modèle (une colonne de `donnees`) n'a pas, à ces lignes, les valeurs de `donnees`
# (fitted() + residuals(type = "response")) : le code l'a ajusté sur d'autres données (par site,
# transformées), et ses lignes ne sont pas des observations.
lignes_ajustement_code <- function(fit, f, e, donnees, y, greffon = FALSE) {
  n <- nrow(donnees)
  nombre <- length(nombres_ou_rien(f))
  noms <- function(x) if (length(names(x)) == nombre) lignes_noms_code(names(x), n)
  lignes <- if (nombre) noms(f) %||% noms(e)
  cadre <- if (is.list(fit) && !isS4(fit)) fit[["model"]]
  if (is.null(lignes) && is.data.frame(cadre) && nrow(cadre) == nombre) lignes <- lignes_noms_code(rownames(cadre), n)
  if (is.null(lignes) && greffon && nombre && is.null(names(f))) {
    omis <- essai_code(stats::na.action(fit))
    omis <- if (length(omis)) suppressWarnings(as.integer(omis)) else integer(0)
    gardees <- setdiff(seq_len(n), omis)
    if (!anyNA(omis) && length(gardees) == nombre) lignes <- gardees
  }
  autres <- FALSE
  if (!is.null(lignes) && !is.null(y) && is.numeric(donnees[[y]])) {
    r <- nombres_ou_rien(essai_code(stats::residuals(fit, type = "response")))
    if (length(r) == nombre) {
      vu <- nombres_ou_rien(f) + r
      ok <- !is.na(vu)
      autres <- !isTRUE(all.equal(vu[ok], donnees[[y]][lignes][ok], tolerance = 1e-6, check.attributes = FALSE))
    }
  }
  list(lignes = if (!autres) lignes, autres = autres)
}

# Les observations que l'ajustement a prises (des numéros), parmi `obs` (celles que le code a reçues,
# `N` celles que nobs() compte ; `lg` : lignes_ajustement_code des valeurs ajustées `ajustees`) : celles
# des lignes de ses valeurs ajustées (non manquantes : na.exclude) quand elles sont N, toutes quand il
# les a toutes prises, sinon toutes sauf celles que na.action() dit omises, par leurs noms (d'un greffon
# qui ne les nomme pas, par leur place) ; NULL quand on ne le sait pas, ou que ses données ne sont pas
# celles de `donnees`.
observations_utilisees_code <- function(fit, obs, N, lg, ajustees, greffon = FALSE) {
  n <- length(obs)
  if (isTRUE(lg$autres)) return(NULL)
  prises <- if (!is.null(lg$lignes) && length(lg$lignes) == length(ajustees)) lg$lignes[!is.na(ajustees)]
  if (length(prises) && length(prises) == N) return(obs[sort(prises)])
  if (identical(as.numeric(N), as.numeric(n))) return(obs)
  na <- essai_code(stats::na.action(fit))
  omis <- lignes_noms_code(names(na), n)
  if (is.null(omis) && greffon && length(na) && is.null(names(na))) {
    omis <- unique(suppressWarnings(as.integer(na)))
    if (anyNA(omis) || any(omis < 1 | omis > n)) omis <- NULL
  }
  if (length(omis) && n - length(omis) == N) return(obs[-omis])
  NULL
}

# Les prédictions du modèle pour les données `nd`, sur l'échelle de la réponse : predict(fit, newdata,
# type = "response") (sans lui, un glm, un gam, glmmTMB… prédisent le prédicteur linéaire : le log
# d'un comptage), son appel par défaut quand ce type le fait échouer (une classe qui ne le connaît pas).
predire_reponse_code <- function(fit, nd) {
  p <- tryCatch(suppressWarnings(stats::predict(fit, newdata = nd, type = "response")), error = function(e) NULL)
  if (is.null(p)) p <- suppressWarnings(stats::predict(fit, newdata = nd))
  p
}

# predict(fit, newdata) marche-t-il sur des données comme celles que Nuée lui donne ?
prediction_code <- function(fit, donnees) {
  if (is.null(fit) || !nrow(donnees)) return(FALSE)
  nd <- donnees[seq_len(min(nrow(donnees), 5)), , drop = FALSE]
  p <- essai_code(predire_reponse_code(fit, nd))
  is.numeric(p) && length(p) == nrow(nd)
}

# Les modèles que l'AIC compare (Prédiction › Comparer les modèles), comme les régressions
# (regression.R : signature) : la même réponse, sans transformation, sur les mêmes observations, d'une
# loi de même mesure et d'une vraisemblance marginale ; seulement pour les classes dont Nuée sait que la
# vraisemblance se compare (lm, glm, glm.nb, nls, et lme et gls de nlme ajustés par maximum de
# vraisemblance). Sinon `raison`, ce qui en écarte le modèle.
comparaison_code <- function(projet, m) {
  fit <- m$ajustement
  aic <- m$statistiques$AIC
  if (is.null(aic)) return(list(raison = traduire("Ce modèle ne donne pas d'AIC.", "This model gives no AIC.")))
  ecarte <- function(fr, en, ...) list(AIC = aic, raison = sprintf(traduire(fr, en), ...))
  # (sa classe même, pas celles dont elle hérite : un gam de mgcv est aussi un glm et un lm)
  if (!isTRUE(class(fit)[1] %in% c("lm", "glm", "negbin", "nls", "lme", "gls"))) {
    return(ecarte("Nuée ne compare par l'AIC que les modèles de lm, glm, glm.nb, nls, et de lme ou gls (nlme) ajustés par maximum de vraisemblance : ce modèle est de classe %s.",
                  "Nuée compares by AIC only models of lm, glm, glm.nb, nls, and nlme's lme or gls fitted by maximum likelihood: this model is of class %s.", class(fit)[1]))
  }
  if (inherits(fit, c("lme", "gls")) && identical(fit$method, "REML")) {
    return(ecarte("Ajusté par REML : sa vraisemblance ne se compare pas à celle d'autres effets fixes (method = \"ML\" pour le comparer par l'AIC).",
                  "Fitted by REML: its likelihood doesn't compare with other fixed effects' (method = \"ML\" to compare it by AIC)."))
  }
  if (isTRUE(m$autres_donnees)) {
    return(ecarte("Le code l'a ajusté sur d'autres données que `donnees` : sa réponse n'y a pas les valeurs des observations (des données agrégées, transformées).",
                  "The code fitted it on other data than `donnees`: its response doesn't have the observations' values there (aggregated, transformed data)."))
  }
  if (!isTRUE(m$utilisees_connues)) return(ecarte("On ne sait pas quelles observations l'ajustement a prises.", "Which observations the fit used isn't known."))
  y <- m$reponse
  v <- if (!is.null(y)) Find(function(v) identical(as.character(v$ids[1]), y) && !length(v$categories), projet$variables)
  if (is.null(v)) return(ecarte("Sa réponse n'est pas une variable quantitative du projet telle quelle (une transformation, ou pas de formule).",
                                "Its response isn't a quantitative variable of the project as it is (a transformation, or no formula)."))
  loi <- as.character(essai_code(stats::family(fit)$family) %||% "gaussian")[1]
  mesure <- if (grepl("^(poisson|binomial|Negative Binomial)", loi)) "discrete" else if (loi %in% c("gaussian", "Gamma", "inverse.gaussian")) "continue"
  if (is.null(mesure)) return(ecarte("La loi %s n'a pas de vraisemblance que Nuée compare.", "The %s distribution has no likelihood Nuée compares.", loi))
  list(AIC = aic, signature = paste(as.numeric(v$uid), "", paste(sort(as.numeric(m$observations)), collapse = ","), mesure, "marginale"))
}

# Les paquets du modèle et leur version (telle que le paquet l'écrit : 3.1-168) : ceux que le code
# nomme et celui de la classe du modèle.
paquets_modele_code <- function(nommes, fit) {
  p <- setdiff(unique(c(nommes, paquet_classe(fit))), PAQUETS_DE_R)
  p <- p[nzchar(p)]
  version <- function(x) tryCatch(as.character(utils::packageDescription(x, fields = "Version")), error = function(e) NA_character_)
  data.frame(nom = p, version = vapply(p, version, "", USE.NAMES = FALSE), stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------- l'ajustement

ajuster_code <- function(projet, d) {
  code <- texte_code(d$options$code %||% "")
  exiger_code_permis(code, d$name)
  donnees <- donnees_code(projet, d)
  ev <- evaluer_code(code, donnees)
  if (!is.null(ev$erreur)) stop(erreur_code(ev))
  m <- modele_valeur_code(projet, d, ev, donnees)
  m[c("code", "empreinte")] <- list(code, empreinte_code(code))
  m
}

# Le modèle que Nuée tire de la dernière valeur d'un code évalué (evaluer_code : `ev`, sur les
# `donnees` du modèle de définition `d`) : celui d'un modèle de code R, ou d'un greffon (greffons.R :
# `type` PLUGIN, la valeur rendue par sa fonction).
modele_valeur_code <- function(projet, d, ev, donnees, type = "CODE") {
  v <- ev$valeur
  liste <- forme_liste_code(v)
  fit <- if (liste) v[["modele"]] else v
  valeurs <- if (liste) valeurs_code(v[["valeurs"]])
  tableaux <- if (liste) tableaux_code(v[["tableaux"]]) else list()
  if (is.null(fit) && !length(valeurs) && !length(tableaux)) {
    stop(refus_code("erreur_code", traduire("La dernière valeur du code est NULL : elle doit être le modèle ajusté (fit), ou list(modele = fit, valeurs = …, tableaux = …).",
                                            "The code's last value is NULL: it must be the fitted model (fit), or list(modele = fit, valeurs = …, tableaux = …).")))
  }
  obs <- as.numeric(unlist(d$observations))
  st <- statistiques_code(fit)
  N <- st$nobs %||% length(obs)
  f <- essai_code(stats::fitted(fit))
  e <- essai_code(stats::residuals(fit))
  ajustees <- nombres_ou_rien(f)
  residus <- nombres_ou_rien(e)
  if (length(ajustees) != length(residus)) ajustees <- residus <- NULL
  reponse <- reponse_code(fit)
  # (chaque valeur ajustée rapportée à son observation par le numéro de sa ligne dans `donnees`, quand
  # on le sait : lignes_ajustement_code)
  lg <- lignes_ajustement_code(fit, f, e, donnees, reponse, identical(type, "PLUGIN"))
  utilisees <- observations_utilisees_code(fit, obs, N, lg, ajustees, identical(type, "PLUGIN"))
  uid_ajustees <- if (length(ajustees) && length(lg$lignes) == length(ajustees)) obs[lg$lignes]
  m <- list(type = type, N = N, n_donnees = length(obs), observations = utilisees %||% obs, recues = obs, utilisees_connues = !is.null(utilisees),
            autres_donnees = isTRUE(lg$autres), ajustement = fit, liste = liste,
            autres = if (liste) setdiff(names(v), c("modele", "valeurs", "tableaux")) else character(0),
            classe = if (!is.null(fit)) class(fit) else character(0), reponse = reponse,
            resume = resume_code(fit), coefficients = coefficients_code(fit), statistiques = st,
            ajustees = ajustees, residus = residus, uid_ajustees = uid_ajustees, prediction = prediction_code(fit, donnees),
            valeurs = valeurs, tableaux = tableaux, sortie = ev$sortie, messages = ev$messages, avertissements = ev$avertissements,
            images = ev$images, paquets = paquets_modele_code(ev$paquets, fit), R = paste(R.version$major, R.version$minor, sep = "."))
  m$comparaison <- comparaison_code(projet, m)
  class(m) <- "modele_code"
  m
}

nombres_ou_rien <- function(x) {
  x <- if (is.numeric(x)) as.numeric(x)
  if (length(x)) x
}

print.modele_code <- function(x, ...) {
  s <- x$statistiques
  cat(sprintf("%s : %s, N = %s%s\n", traduire("Modèle de code R", "R-code model"), paste(x$classe, collapse = "/") %||% "—", format(x$N),
              if (!is.null(s$AIC)) sprintf(", AIC = %s", format(s$AIC, digits = 6)) else ""))
  if (length(x$resume)) cat(x$resume, sep = "\n")
  invisible(x)
}

# ---------------------------------------------------------------- l'interface

# Les tableaux du code tels que l'interface les reçoit : leurs colonnes, des nombres ou du texte, leurs
# CODE_LIGNES_TABLEAU premières lignes, et leur nombre de lignes.
tableaux_interface <- function(tableaux) {
  lapply(names(tableaux), function(k) {
    x <- tableaux[[k]]
    n <- nrow(x)
    garde <- utils::head(x, CODE_LIGNES_TABLEAU)
    list(nom = k, n = n, colonnes = I(names(garde)),
         valeurs = unname(lapply(garde, function(c) if (is.numeric(c)) I(as.numeric(c)) else I(as.character(c)))))
  })
}

resultat_code <- function(id) {
  e <- modele(id)
  p <- projet(e$projet)
  m <- e$modele
  d <- e$definition
  s <- m$statistiques
  fr <- !identical(getOption("nuee.langue"), "en")  # (la ligne de l'explorateur : sa virgule décimale)
  noms_obs <- function(u) unname(p$noms_obs[nom_numero(u)])
  qq <- if (length(m$residus)) {
    r <- m$residus[is.finite(m$residus)]
    q <- stats::qqnorm(r, plot.it = FALSE)
    y <- stats::quantile(r, c(0.25, 0.75), names = FALSE)
    x <- stats::qnorm(c(0.25, 0.75))
    pente <- diff(y) / diff(x)
    list(theorique = q$x, residu = q$y, pente = pente, ordonnee = y[1] - pente * x[1])
  }
  chiffres <- paste(c(paste(m$classe[1] %||% "—"), sprintf("N %d", as.integer(m$N)), if (!is.null(s$AIC)) sprintf("AIC %s", nombre_texte(s$AIC, 1, fr))), collapse = " · ")
  list(id = id, projet = p$id, nom = d$name, titre = d$title %||% "", type = "CODE", analyse = TRUE, code_r = TRUE, prediction = isTRUE(m$prediction),
       regression = FALSE, opls = FALSE, Ap = 0, A = 0, N = m$N, n_donnees = m$n_donnees, K = NROW(m$coefficients), chiffres = chiffres,
       vues_defaut = I(c("code_coefficients", "code_diagnostics")),
       code = m$code, classe = I(m$classe), reponse = m$reponse, resume = I(m$resume), sortie = I(m$sortie), messages = I(m$messages),
       avertissements = I(m$avertissements), images = I(m$images), autres = I(m$autres),
       coefficients = m$coefficients, statistiques = s,
       ajustees = if (length(m$ajustees)) list(uid = I(m$uid_ajustees %||% numeric(0)), nom = I(if (length(m$uid_ajustees)) noms_obs(m$uid_ajustees) else character(0)),
                                               ajuste = I(m$ajustees), residu = I(m$residus)),
       qq = qq, valeurs = if (length(m$valeurs)) list(nom = I(names(m$valeurs)), valeur = I(unname(m$valeurs))),
       tableaux = I(tableaux_interface(m$tableaux)), paquets = m$paquets, R = m$R, comparaison = m$comparaison,
       observations = list(uid = I(as.numeric(m$recues)), nom = I(noms_obs(m$recues))),
       reglages = reglages_modele(p, d), numero = d$number %||% NULL, app = isTRUE(d$app), note = d$note %||% "",
       demande = d$demande %||% list(type = d$type, numero = d$number))
}

# ---------------------------------------------------------------- Exporter

# Les tableaux d'un modèle de code (formats.R : tables_resultats) : le résumé (classe, observations,
# vraisemblance, AIC, BIC, R et paquets), les coefficients, ses valeurs, ses tableaux (leurs noms rendus
# sûrs pour un nom de feuille ou de fichier), les valeurs ajustées et les résidus, le texte du résumé et
# le code lui-même.
tables_code <- function(projet, m) {
  s <- m$statistiques
  na <- function(x) x %||% NA_real_
  out <- list(resume = data.frame(classe = paste(m$classe, collapse = " "), observations = m$N, donnees = m$n_donnees, logLik = na(s$logLik), ddl = na(s$ddl),
                                  AIC = na(s$AIC), BIC = na(s$BIC), R = m$R, paquets = paste(sprintf("%s %s", m$paquets$nom, m$paquets$version), collapse = ", "),
                                  stringsAsFactors = FALSE))
  if (!is.null(m$coefficients)) out$coefficients <- m$coefficients
  if (length(m$valeurs)) out$valeurs <- data.frame(nom = names(m$valeurs), valeur = unname(m$valeurs), stringsAsFactors = FALSE)
  noms <- substr(gsub("[^[:alnum:] _.-]", "_", names(m$tableaux)), 1, 28)
  noms <- make.unique(c(names(out), "ajustees", "texte", "code", noms))[-seq_len(length(out) + 3)]
  for (k in seq_along(noms)) out[[noms[k]]] <- m$tableaux[[k]]
  if (length(m$ajustees)) {
    u <- m$uid_ajustees
    out$ajustees <- data.frame(Observation = if (length(u)) unname(projet$noms_obs[nom_numero(u)]) else NA_character_, ajuste = m$ajustees, residu = m$residus, stringsAsFactors = FALSE)
  }
  if (length(m$resume)) out$texte <- data.frame(resume = m$resume, stringsAsFactors = FALSE)
  out$code <- data.frame(code = strsplit(m$code, "\n", fixed = TRUE)[[1]], stringsAsFactors = FALSE)
  out
}

# ---------------------------------------------------------------- prédire

# Les prédictions de predict(fit, newdata) pour des observations du projet, avec les données que le
# code voit (donnees_code), sur l'échelle de la réponse (predire_reponse_code) ; la réponse observée
# quand le modèle en a une du projet.
predire_code <- function(projet, m, d, observations) {
  nd <- donnees_code(projet, d, observations)
  p <- tryCatch(suppressWarnings(as.numeric(predire_reponse_code(m$ajustement, nd))), error = function(e) {
    stop(sprintf(traduire("predict() n'a pas prédit ces observations : %s", "predict() didn't predict these observations: %s"), conditionMessage(e)), call. = FALSE)
  })
  if (length(p) != nrow(nd)) stop(traduire("predict() n'a pas rendu une prédiction par observation.", "predict() didn't give one prediction per observation."), call. = FALSE)
  y <- m$reponse %||% traduire("réponse", "response")
  out <- data.frame(row.names = seq_along(observations))
  out[[paste(y, "prédit")]] <- p
  if (!is.null(m$reponse) && m$reponse %in% names(nd) && is.numeric(nd[[m$reponse]])) out[[paste(y, "observé")]] <- nd[[m$reponse]]
  out$hors_domaine <- FALSE
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------- texte des méthodes

# La citation du paquet `paquet` (sa première, utils::citation) : son texte, sans les marques de
# Markdown, et son appel court, « Pinheiro et al., 2025 ».
citation_paquet <- function(paquet, fr) {
  b <- essai_code(utils::citation(paquet))
  if (is.null(b) || !length(b)) return(NULL)
  e <- b[[1]]
  texte <- tryCatch(paste(format(e, style = "text"), collapse = " "), error = function(x) "")
  texte <- trimws(gsub("\\s+", " ", gsub("<(https?://[^>]+)>", "\\1", gsub("(^|[^[:alnum:]])_([^_]+)_", "\\1\\2", texte))))
  auteurs <- tryCatch(e$author, error = function(x) NULL)
  noms <- vapply(seq_along(auteurs), function(i) {
    a <- auteurs[[i]]
    paste(c(a$family, if (!length(a$family)) a$given), collapse = " ")
  }, "")
  noms <- noms[nzchar(noms)]
  annee <- as.character(tryCatch(e$year, error = function(x) NULL) %||% "")
  court <- if (length(noms) >= 3) paste(noms[1], "et al.") else if (length(noms) == 2) paste(noms[1], if (fr) "et" else "and", noms[2]) else if (length(noms)) noms[1] else paquet
  list(texte = texte, court = if (nzchar(annee)) sprintf("%s, %s", court, annee) else court)
}

texte_methodes_code <- function(projet, d, m, langue = "fr", version = "") {
  fr <- langue == "fr"
  tx <- function(a, b) if (fr) a else b
  s <- m$statistiques
  nombre <- function(x, chiffres = 1) nombre_texte(x, chiffres, fr)
  titre <- as.character(d$title %||% "")
  classe <- m$classe[1]
  # les observations, et celles que l'ajustement a laissées de côté
  obs <- observations_texte(m$N, d, fr)
  if (m$N < m$n_donnees) obs <- paste(obs, sprintf(tx("(sur les %d que Nuée lui a données ; les autres, laissées de côté par l'ajustement)", "(of the %d Nuée gave it; the others left out by the fit)"), m$n_donnees))
  stats <- c(if (!is.null(s$logLik)) sprintf(tx("log-vraisemblance %s (%s ddl)", "log-likelihood %s (%s df)"), nombre(s$logLik, 2), format(s$ddl %||% NA)),
             if (!is.null(s$AIC)) sprintf("AIC = %s", nombre(s$AIC)), if (!is.null(s$BIC)) sprintf("BIC = %s", nombre(s$BIC)))
  p1 <- sprintf(tx("Le modèle %s%s a été ajusté par le code R de ses auteurs (donné en entier par Exporter vers R) sur %s%s%s.",
                   "Model %s%s was fitted with its authors' own R code (given in full by Export to R) on %s%s%s."),
                d$name %||% "", if (nzchar(titre)) sprintf(" (%s)", titre) else "", obs,
                if (!is.null(classe)) sprintf(tx(" : un modèle de classe %s", ": a model of class %s"), guillemets_texte(classe, fr)) else "",
                if (length(stats)) paste0(tx(" ; ", "; "), paste(stats, collapse = tx(" ; ", "; "))) else "")
  # le logiciel : R, les paquets du code (leur version et leur citation), Nuée
  pq <- m$paquets
  citations <- lapply(pq$nom, citation_paquet, fr = fr)
  paquets <- vapply(seq_along(pq$nom), function(k) sprintf("%s %s%s", pq$nom[k], pq$version[k] %||% "", if (!is.null(citations[[k]])) sprintf(" (%s)", citations[[k]]$court) else ""), "")
  annee_r <- R.version$year
  annee_nuee <- format(Sys.Date(), "%Y")
  p2 <- sprintf(tx("Les calculs ont été faits avec R %s (R Core Team, %s)%s ; Nuée%s (Boutin, %s) a donné au code ses données et en a tiré les résultats montrés (résumé, coefficients, vraisemblance).",
                   "Computations were done with R %s (R Core Team, %s)%s; Nuée%s (Boutin, %s) gave the code its data and took from it the results shown (summary, coefficients, likelihood)."),
                m$R, annee_r, if (length(paquets)) sprintf(tx(" et %s %s", " and the %s %s"), if (length(paquets) > 1) tx("les paquets", "packages") else tx("le paquet", "package"), liste_mots(paquets, fr)) else "",
                if (nzchar(version)) paste0(" ", version) else "", annee_nuee)
  references <- c(sprintf(REFERENCES$r, annee_r), sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
                  unlist(lapply(citations, function(x) if (!is.null(x) && nzchar(x$texte)) x$texte)))
  list(paragraphes = c(p1, p2), references = unname(sort(unique(references))))
}
