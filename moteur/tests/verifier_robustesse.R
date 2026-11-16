# Vérifie que le moteur tient bon sur des données étranges mais légitimes : chaque sorte d'analyse
# sur des tableaux faits pour la mettre en difficulté. Chaque analyse passe par les demandes du
# serveur (serveur.R : appli$call), telles que l'interface les envoie : vérifier puis ajuster, comme
# Nouveau modèle, puis ce que demandent ses vues (le texte des méthodes, celui des résultats, les
# prédictions, l'export...). Chaque demande doit rendre, en un temps borné, un résultat sans NaN ni
# Inf là où l'interface montre un nombre, ou un message du moteur dans la langue de la demande (un
# texte de traduire(), qui change avec la langue). Sont des échecs : une erreur de R elle-même
# (« subscript out of bounds »), un message qui reste le même en français et en anglais, un
# message français à une demande en anglais, un délai dépassé, un NaN ou un Inf dans un résultat, un
# résultat autre qu'attendu (les mêmes réponses avec des numéros internes de 100 000, un refus).
#   Rscript moteur/tests/verifier_robustesse.R [--cas=<expression régulière>] [--analyses=<expression régulière>] [--tout] [--detail] [--durees=<fichier .csv>]
# (depuis la racine du dépôt, cinq minutes environ ; un cas n'essaie que les analyses que ses données
# concernent, --tout les lui fait toutes essayer, une douzaine de minutes ; --detail écrit chaque
# demande et sa réponse ; --durees écrit chaque demande, son statut et sa durée)

options(warn = -1, stringsAsFactors = FALSE)
arguments <- commandArgs(trailingOnly = TRUE)
argument <- function(nom, defaut = "") {
  a <- grep(paste0("^--", nom, "="), arguments, value = TRUE)
  if (length(a)) sub(paste0("^--", nom, "="), "", a[length(a)]) else defaut
}
MOTIF_CAS <- argument("cas")
MOTIF_ANALYSES <- argument("analyses")
TOUT <- "--tout" %in% arguments
DETAIL <- "--detail" %in% arguments
# (les limites de temps de chaque demande, multipliées : NUEE_LIMITE=2 pour une machine lente ; 3 sur
# celles de l'intégration continue, où la variable CI est définie, jusqu'à près de trois fois plus
# lentes que celle où les limites ont été réglées)
FACTEUR_LIMITE <- as.numeric(Sys.getenv("NUEE_LIMITE", if (nzchar(Sys.getenv("CI"))) "3" else "1"))

# ---------------------------------------------------------------- le serveur, sans son écoute

# Chaque expression de serveur.R jusqu'au démarrage de httpuv (comme banc_vitesse.R) : DEMANDES,
# appli$call, repondre et l'état `etat` en viennent.
local({
  racine_nuee <- normalizePath(".", winslash = "/")
  assign("racine", racine_nuee, globalenv())
  exprs <- parse(file.path(racine_nuee, "moteur", "serveur.R"), encoding = "UTF-8", keep.source = FALSE)
  for (e in exprs) {
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

# Les messages du moteur : chaque paire que traduire() a reçue, pour reconnaître un message du
# moteur (et sa langue) parmi les erreurs.
MESSAGES <- new.env()
traduire_moteur <- traduire
traduire <- function(fr, en) {
  if (is.character(fr) && is.character(en) && length(fr) == length(en)) {
    for (i in seq_along(fr)) if (!is.na(fr[i]) && !is.na(en[i])) assign(paste0(fr[i], "\r", en[i]), c(fr[i], en[i]), envir = MESSAGES)
  }
  traduire_moteur(fr, en)
}

# Le contenu de la dernière réponse, avant que repondre() ne change NaN et Inf en null.
DERNIERE <- new.env()
repondre_serveur <- repondre
repondre <- function(statut, contenu) {
  DERNIERE$contenu <- contenu
  repondre_serveur(statut, contenu)
}

# ---------------------------------------------------------------- une demande

DOSSIER <- file.path(tempdir(), "robustesse")
dir.create(DOSSIER, showWarnings = FALSE)
fichier <- function(nom) file.path(DOSSIER, nom)

# Une demande comme l'interface l'envoie (app/interface/api.js : demander) : son corps en JSON, la
# langue dans l'en-tête X-Langue, le cookie de la séance ; ce que rend appli$call (statut, corps) et
# le contenu d'avant le JSON. `limite` : le temps permis, en secondes.
envoyer <- function(nom, q, langue, limite) {
  corps <- enc2utf8(as.character(jsonlite::toJSON(q, auto_unbox = TRUE, null = "null", na = "null", digits = NA)))
  req <- list(PATH_INFO = paste0("/api/", nom), HTTP_HOST = HOTES[1], HTTP_COOKIE = paste0(COOKIE, "=", JETON),
              HTTP_X_LANGUE = langue, QUERY_STRING = "", rook.input = list(read = function() charToRaw(corps)))
  DERNIERE$contenu <- NULL
  t0 <- proc.time()[["elapsed"]]
  setTimeLimit(elapsed = limite, transient = TRUE)
  r <- tryCatch(appli$call(req), error = function(e) list(status = 500L, body = as.character(jsonlite::toJSON(list(erreur = conditionMessage(e)), auto_unbox = TRUE))))
  setTimeLimit(elapsed = Inf)
  duree <- proc.time()[["elapsed"]] - t0
  reponse <- tryCatch(jsonlite::fromJSON(r$body, simplifyVector = TRUE), error = function(e) list(erreur = paste("JSON :", conditionMessage(e))))
  list(statut = r$status, reponse = reponse, contenu = DERNIERE$contenu, duree = duree)
}

# Les endroits d'un résultat où un nombre n'est pas fini (NaN, Inf), ou un texte qui en écrit un.
non_finis <- function(x, chemin = "") {
  if (is.null(x) || is.function(x) || is.environment(x)) return(character(0))
  if (is.list(x)) {
    noms <- names(x) %||% rep("", length(x))
    return(unlist(lapply(seq_along(x), function(i) non_finis(x[[i]], paste0(chemin, "$", if (nzchar(noms[i])) noms[i] else i)))))
  }
  if (is.numeric(x) && any(is.nan(x) | is.infinite(x))) return(chemin)
  if (is.character(x) && any(grepl("\\b(NaN|-?Inf|NA)\\b", x))) return(paste(chemin, "(texte)"))
  character(0)
}

# Un message reconnu comme venant du moteur dans la langue `langue` : un texte de traduire() dont
# chaque morceau (entre les %s, %d… de sprintf) y est.
du_moteur <- function(message, langue) {
  for (cle in ls(MESSAGES, all.names = TRUE)) {
    modele <- MESSAGES[[cle]][[if (identical(langue, "en")) 2 else 1]]
    m <- trimws(strsplit(modele, "%[-+ 0#]*[0-9]*(\\.[0-9]+)?[a-zA-Z%]")[[1]])
    m <- m[nchar(m) >= 3]
    if (!length(m) || max(nchar(m)) < 8) next
    if (all(vapply(m, function(x) grepl(x, message, fixed = TRUE), TRUE))) return(TRUE)
  }
  FALSE
}

# Ce que R écrit de ses propres erreurs : un message du moteur qui en contient une la montre telle
# quelle à l'utilisateur.
MOTS_DE_R <- c("subscript out of bounds", "missing value where TRUE/FALSE needed", "argument of length 0", "non-conformable",
               "object of type", "undefined columns", "replacement has", "arguments imply differing", "NA/NaN/Inf in",
               "infinite or missing values", "is not a function", "could not find function", "invalid 'type'", "non-numeric argument",
               "incorrect number of", "Lapack routine", "LAPACK", "contrasts can be applied", "need finite", "attempt to set",
               "attempt to select", "cannot open the connection", "no applicable method", "dims [", "length of 'dimnames'",
               "is.na() applied", "Error in", "unused argument", "$ operator is invalid", "variable lengths differ",
               "number of items to replace", "should be one of", "Valeur manquante", "argument de longueur nulle", "indice hors limites")

# Les NaN et Inf que l'interface montre comme il faut (null dans le JSON : une case vide, ou ce que
# la vue en fait), avec pourquoi ; chacun « demande : chemin » (une expression régulière). Un autre
# est un échec.
NON_FINIS_PERMIS <- c(
  # (les degrés de liberté d'une loi normale, z asymptotique : la vue écrit ∞)
  "^moyennes: \\$(moyennes|paires)\\$ddl$",
  # (l'ellipse du T² de Hotelling, dessinée seulement : indéfinie sous trois observations)
  "^(ajuster|modele|modeles_classe): (\\$modeles\\$[0-9]+)?\\$ellipse\\$(95|99)$",
  # (le R² d'une variable sans variation, 0 / 0 : une case vide)
  "^(ajuster|modele|modeles_classe): (\\$modeles\\$[0-9]+)?\\$variables\\$R2V_cum$",
  # (le résidu standardisé et la distance de Cook d'une observation de levier 1, la seule de sa
  # catégorie : indéfinis, une case vide)
  "^(ajuster|modele): \\$observations\\$(residu|cook)$",
  # (la CV-ANOVA et le RMSEE sans degré de liberté résiduel : des cases vides ; le texte des résultats
  # le dit)
  "^(ajuster|modele): \\$cv_anova\\$[^$]+\\$(MS|SD)$", "^(ajuster|modele): \\$(variables_y\\$RMSEE|RMSEE\\$[^$]+)$",
  # (le Q² d'une validation croisée dont chaque groupe n'a qu'une réponse, ou des réponses égales :
  # 1 − x / 0, une case vide ; les textes disent qu'elle n'a pas donné de Q²)
  "^(ajuster|modele): \\$qualite\\$Q2$",
  # (le test de chaque variable entre les classes : celui d'une variable constante ne se calcule
  # pas, sa ligne a des cases vides)
  "^tests_variables: \\$tableau\\$(statistique|p|p_ajuste)$",
  # (la note de lecture d'un fichier : les valeurs lues comme manquantes, telles qu'écrites, « Inf ×1, NaN ×1 »)
  "^ouvrir: \\$note \\(texte\\)$")
permis <- function(nom, chemin, cas) {
  motifs <- c(NON_FINIS_PERMIS, cas$nan_permis)
  any(vapply(motifs, function(m) grepl(m, paste0(nom, ": ", chemin)), TRUE))
}

# ---------------------------------------------------------------- le registre des demandes

RESULTATS <- list()
noter_demande <- function(cas, analyse, nom, statut, message = "", duree = 0, chemins = character(0)) {
  RESULTATS[[length(RESULTATS) + 1]] <<- list(cas = cas, analyse = analyse, demande = nom, statut = statut, message = message,
                                               duree = duree, chemins = paste(utils::head(chemins, 6), collapse = " "))
  if (DETAIL || statut != "ok") {
    cat(sprintf("  %-7s %s › %s › %s%s%s\n", statut, cas, analyse, nom, if (nzchar(message)) paste0(" : ", substr(message, 1, 300)) else "",
                if (length(chemins)) paste0(" [", paste(utils::head(chemins, 6), collapse = " "), "]") else ""))
  }
}

# Ce qu'une réponse dit, sans ce qui tient aux numéros internes des observations et des variables
# (uid…, les variables des coordonnées), ni à la séance (identifiants, chemins, révisions, la durée
# mesurée d'une simulation) : les mêmes données numérotées autrement doivent donner la même
# empreinte (le cas numeros_100000 contre le cas base). `a_source` (1.1) : un projet fait d'un fichier
# en a une, le même tableau fait en mémoire non ; ce n'est pas une différence de ses données.
CHAMPS_INTERNES <- c("id", "projet", "modele", "demande", "reglages", "numero", "chemin", "chemins", "chemin_enregistrement", "signature",
                     "revision", "annulable", "source", "a_source", "fichier_colle", "script", "variable", "coordonnees", "proposition", "duree",
                     "par_ajustement", "_row")
empreinte <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[!names(x) %in% CHAMPS_INTERNES & !startsWith(names(x), "uid")]
    return(lapply(x, empreinte))
  }
  if (is.character(x)) x[x %in% CONTEXTE$noms_fichiers] <- ""
  x
}
EMPREINTES <- new.env()
comparer_reponse <- function(nom, reponse) {
  cle <- paste(CONTEXTE$analyse, nom, sep = " › ")
  k <- (CONTEXTE$vues[[cle]] %||% 0) + 1
  CONTEXTE$vues[[cle]] <- k
  cle <- paste(cle, k)
  texte <- as.character(jsonlite::toJSON(empreinte(reponse), auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
  if (isTRUE(CONTEXTE$garder)) EMPREINTES[[cle]] <- texte
  if (is.null(CONTEXTE$reference) || is.null(EMPREINTES[[cle]]) || identical(EMPREINTES[[cle]], texte)) return(character(0))
  # (le premier endroit qui diffère, pour le message)
  a <- strsplit(EMPREINTES[[cle]], "")[[1]]
  b <- strsplit(texte, "")[[1]]
  i <- which(a[seq_len(min(length(a), length(b)))] != b[seq_len(min(length(a), length(b)))])[1]
  if (is.na(i)) i <- min(length(a), length(b))
  sprintf("autre que pour %s, vers « …%s… » contre « …%s… »", CONTEXTE$reference, paste(a[max(1, i - 60):min(length(a), i + 40)], collapse = ""),
          paste(b[max(1, i - 60):min(length(b), i + 40)], collapse = ""))
}

# Une demande d'une analyse d'un cas : en anglais (la langue de l'interface par défaut) ; une erreur
# est redemandée en français, pour voir si son message change avec la langue. Le résultat, ou NULL.
CONTEXTE <- new.env()
demander <- function(nom, q, limite = CONTEXTE$limite %||% 60) {
  r <- envoyer(nom, q, "en", limite)
  if (identical(r$statut, 200L)) {
    chemins <- non_finis(r$contenu)
    chemins <- chemins[!vapply(chemins, function(ch) permis(nom, ch, CONTEXTE$cas_def), TRUE)]
    ecart <- comparer_reponse(nom, r$reponse)
    noter_demande(CONTEXTE$cas, CONTEXTE$analyse, nom, if (length(ecart)) "écart" else if (length(chemins)) "nan" else "ok",
                  if (length(ecart)) ecart else "", duree = r$duree, chemins = chemins)
    return(r$reponse)
  }
  m_en <- as.character(r$reponse$erreur %||% "")[1]
  # (un message déjà vu, mot pour mot, a déjà été redemandé en français : le même statut)
  cle <- paste0("«", m_en)
  statut <- if (r$duree >= limite || grepl("elapsed time limit|temps écoulé", m_en)) "délai" else if (!is.null(VUS[[cle]])) VUS[[cle]] else {
    fr <- envoyer(nom, q, "fr", limite)
    m_fr <- as.character(fr$reponse$erreur %||% "")[1]
    VUS[[cle]] <- if (identical(m_en, m_fr)) "R"
    else if (!du_moteur(m_en, "en") && du_moteur(m_en, "fr")) "langue"
    else if (any(vapply(MOTS_DE_R, function(x) grepl(x, m_en, fixed = TRUE), TRUE))) "R?"
    else "message"
  }
  noter_demande(CONTEXTE$cas, CONTEXTE$analyse, nom, statut, m_en, r$duree)
  NULL
}
VUS <- new.env()

# Ce qu'une réponse doit dire (une valeur, un nombre d'observations) : sinon, un écart noté.
attendre <- function(ok, nom, message) if (!isTRUE(ok)) noter_demande(CONTEXTE$cas, CONTEXTE$analyse, nom, "écart", message)

# ---------------------------------------------------------------- les données

# Un tableau de suivi : 10 sites comptés 6 ans, trois habitats, deux strates, des X, une réponse
# quantitative, des comptages, une présence, une proportion, une valeur positive, cinq espèces, des
# coordonnées, des distances de détection le long d'un transect par site (sa longueur, l'aire de sa
# strate, la taille des groupes).
tableau_base <- function(sites = 10, annees = 6, graine = 1) {
  set.seed(graine)
  n <- sites * annees
  k <- rep(seq_len(sites), each = annees)
  h <- (k - 1) %% 3 + 1
  x1 <- round(stats::rnorm(n), 4)
  x2 <- round(stats::rnorm(n), 4)
  x3 <- round(stats::rnorm(n), 4)
  y <- round(2 + x1 - 0.5 * x2 + stats::rnorm(sites, 0, 0.5)[k] + stats::rnorm(n, 0, 0.5), 4)
  moyennes <- rbind(c(8, 1, 3, 0.5, 2), c(2, 6, 1, 3, 2), c(1, 2, 6, 3, 2))
  especes <- vapply(1:5, function(j) stats::rpois(n, moyennes[h, j]), numeric(n))
  df <- data.frame(nom = paste0("o", seq_len(n)), site = sprintf("S%02d", k), annee = rep(2015 + seq_len(annees) - 1, sites),
                   habitat = c("bois", "lande", "pre")[h], strate = ifelse(k <= sites / 2, "A", "B"),
                   x1 = x1, x2 = x2, x3 = x3, y = y, compte = stats::rpois(n, exp(0.5 + 0.4 * x1)),
                   presence = stats::rbinom(n, 1, stats::plogis(x1)), prop = round(stats::plogis(0.5 * x1 + stats::rnorm(n, 0, 0.5)), 4),
                   positif = round(exp(0.3 * y), 4))
  for (j in 1:5) df[[paste0("sp", j)]] <- especes[, j]
  df$lon <- round(4.6 + stats::runif(n, 0, 0.2), 5)
  df$lat <- round(43.5 + stats::runif(n, 0, 0.2), 5)
  df$distance <- round(abs(stats::rnorm(n, 0, 30)), 1)
  df$longueur <- c(1.5, 2, 2.5)[(k - 1) %% 3 + 1]
  df$aire <- ifelse(df$strate == "A", 10, 20)
  df$taille <- sample(1:3, n, replace = TRUE)
  df
}

# Un tableau en CSV, comme un tableur l'écrit (UTF-8, virgules, guillemets autour d'un texte qui
# en a besoin) ; `lignes` : le texte tel quel.
ecrire_csv <- function(df, chemin) {
  cellule <- function(x) {
    if (is.numeric(x)) return(ifelse(is.na(x) & !is.nan(x), "", format(x, digits = 15, trim = TRUE, scientific = NA)))
    x <- as.character(x)
    ifelse(is.na(x), "", ifelse(grepl("[\",\n]", x), paste0("\"", gsub("\"", "\"\"", x), "\""), x))
  }
  colonnes <- lapply(df, cellule)
  lignes <- c(paste(cellule(names(df)), collapse = ","), do.call(paste, c(colonnes, sep = ",")))
  con <- file(chemin, "wb")
  writeLines(enc2utf8(lignes), con, sep = "\n", useBytes = TRUE)
  close(con)
  chemin
}

# Le même tableau en projet Nuée (.json, comme un .usp converti), ses observations et ses variables
# numérotées 100 000, 200 000… : des numéros qu'as.character() écrit « 1e+05 », comme ceux d'un
# tableau de 100 000 lignes ou plus.
ecrire_renumerote <- function(df, chemin, pas = 100000) {
  p <- projet_depuis_tableau(df, basename(chemin))
  uo <- as.integer(seq_along(p$observations) * pas)
  uv <- as.integer(seq_along(p$variables) * pas)
  j <- list(format = "usp-json/1", source = basename(chemin), observation_id_names = p$observation_id_names, variable_id_names = "Variable",
            observations = lapply(seq_along(uo), function(i) list(uid = uo[i], ids = p$observations[[i]]$ids)),
            variables = lapply(seq_along(uv), function(i) { v <- p$variables[[i]]; v$uid <- uv[i]; v }),
            datasets = list(list(name = basename(chemin), observations = uo, variables = uv, values = unname(p$donnees))),
            models = list(), prediction_sets = list(), dendrograms = list())
  jsonlite::write_json(j, chemin, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  chemin
}

# ---------------------------------------------------------------- les analyses

# Ce que l'interface envoie pour ajuster un modèle (dialogues.js : valider), complété de `reglages`.
requete_modele <- function(type, reglages) {
  vide <- stats::setNames(list(), character(0))
  q <- list(projet = CONTEXTE$projet, type = type, x = list(), y = list(), classe = NULL, exclues = list(), test = list(),
            echelle = "none", echelles = vide, transformations = vide, croises = list(), spectres = list(), A = 0, G = 7, vc = NULL,
            blocs = vide, modificateurs = vide, aleatoires = list(), correlation = NULL, famille = NULL, lisses = list(),
            k_courbes = vide, par_courbes = vide, decalage = NULL, lisse_spatial = NULL, croises_reglages = vide,
            nom = CONTEXTE$analyse, titre = "", condition = "")
  for (n in names(reglages)) q[n] <- list(reglages[[n]])
  if (!is.null(CONTEXTE$requete)) q <- CONTEXTE$requete(q)
  q
}

# Les numéros des variables de ces rôles. Un rôle qui n'est pas une variable du projet (l'import
# a fait de ses textes, tous différents, un identifiant des observations) arrête l'analyse, que
# l'interface ne permettrait pas : role_absent.
role_absent <- function(noms) stop(structure(class = c("role_absent", "error", "condition"), list(message = paste(noms, collapse = ", "), call = NULL)))
V <- function(...) {
  noms <- c(...)
  if (length(setdiff(noms, names(CONTEXTE$v)))) role_absent(setdiff(noms, names(CONTEXTE$v)))
  as.list(unname(unlist(CONTEXTE$v[noms])))
}
v1 <- function(nom) V(nom)[[1]]
# Un groupement de ce rôle ({ by: "variable" } ou, pour un identifiant, { by: "id" }), et sa
# colonne pour les demandes qui prennent "id:k".
groupe <- function(nom) {
  if (!is.null(CONTEXTE$v[[nom]])) return(list(by = "variable", variable = CONTEXTE$v[[nom]]))
  if (!is.null(CONTEXTE$ids[[nom]])) return(list(by = "id", id = CONTEXTE$noms_ids[[nom]]))
  role_absent(nom)
}
colonne_role <- function(nom) {
  if (!is.null(CONTEXTE$v[[nom]])) return(CONTEXTE$v[[nom]])
  if (!is.null(CONTEXTE$ids[[nom]])) return(paste0("id:", CONTEXTE$ids[[nom]]))
  role_absent(nom)
}

# Vérifier, ajuster, puis les vues du modèle : `suite(id, r)` ses demandes propres.
essayer_modele <- function(type, reglages, suite = NULL, predire = TRUE) {
  q <- requete_modele(type, reglages)
  demander("verifier", q)
  r <- demander("ajuster", q)
  if (is.null(r)) return(invisible(NULL))
  id <- r$id
  demander("modele", list(modele = id))
  demander("methodes", list(modele = id, langue = "en"))
  demander("resultats_texte", list(modele = id, langue = "en"))
  if (predire && !isFALSE(r$prediction)) demander("predire", list(modele = id))
  demander("exporter", list(modele = id, chemin = fichier(paste0(CONTEXTE$analyse, ".xlsx"))))
  # (l'export vers R, qui refait le modèle et le compare au moteur : pour les cas qui le demandent)
  if (isTRUE(CONTEXTE$scripts)) {
    s <- demander("script_r", list(modele = id, chemin = fichier(file.path(CONTEXTE$cas, paste0(CONTEXTE$analyse, ".R"))), version = "essai"))
    if (!is.null(s)) attendre(!length(s$ecart), "script_r", paste("le script ne refait pas le modèle :", paste(s$ecart, collapse = " ")))
  }
  if (!is.null(suite)) suite(id, r)
  invisible(r)
}

# Les vues d'une régression.
suite_regression <- function(id, r) {
  demander("verifications_modele", list(modele = id))
  demander("effets", list(modele = id))
  demander("apports", list(modele = id))
  demander("selection_aic", list(modele = id))
  if (length(r$qualitatives)) demander("moyennes", list(modele = id))
}

X3 <- function() V("x1", "x2", "x3")
ESPECES <- function() V("sp1", "sp2", "sp3", "sp4", "sp5")

ANALYSES <- list(
  PCA = function() essayer_modele("PCA", list(x = V("x1", "x2", "x3", "y"), echelle = "UV"), function(id, r) {
    demander("hca", list(modele = id))
    demander("biplot", list(modele = id))
  }),
  # (toutes les variables quantitatives du tableau : 5 000 pour un tableau large)
  PCA_TOUT = function() essayer_modele("PCA", list(x = as.list(CONTEXTE$quantitatives), echelle = "UV")),
  PLS = function() essayer_modele("PLS", list(x = X3(), y = V("y"), echelle = "UV"), function(id, r) demander("permutations", list(modele = id, n = 5))),
  OPLS = function() essayer_modele("OPLS", list(x = X3(), y = V("y"), echelle = "UV")),
  `PLS-DA` = function() essayer_modele("PLS-DA", list(x = X3(), classe = v1("habitat"), echelle = "UV")),
  `OPLS-DA` = function() essayer_modele("OPLS-DA", list(x = X3(), classe = v1("habitat"), echelle = "UV")),
  CLASSES = function() {
    r <- demander("modeles_classe", requete_modele("PCA", list(x = X3(), classe = v1("habitat"), echelle = "UV")))
    if (length(r$modeles)) demander("appartenance", list(modele = r$modeles$id[1]))
  },
  LM = function() essayer_modele("LM", list(x = V("x1", "x2", "habitat"), y = V("y")), suite_regression),
  LMM = function() essayer_modele("LMM", list(x = V("annee", "x1"), y = V("y"), aleatoires = list(groupe("site"))), function(id, r) {
    suite_regression(id, r)
    demander("puissance_tendance", list(modele = id))
    demander("autocorrelation_residus", list(modele = id, temps = groupe("annee"), groupe = list(by = "effets")))
  }),
  CAR1 = function() essayer_modele("LMM", list(x = V("x1"), y = V("y"), aleatoires = list(groupe("site")),
                                     correlation = list(type = "CAR1", temps = groupe("annee")))),
  POISSON = function() essayer_modele("GLM", list(x = V("x1", "habitat"), y = V("compte"), famille = "poisson"), suite_regression),
  BINNEG = function() essayer_modele("GLM", list(x = V("x1"), y = V("compte"), famille = "binomiale_negative")),
  LOGIT = function() essayer_modele("GLM", list(x = V("x1", "x2"), y = V("presence"), famille = "binomiale"), suite_regression),
  GAMMA = function() essayer_modele("GLM", list(x = V("x1"), y = V("positif"), famille = "gamma")),
  BETA = function() essayer_modele("GLM", list(x = V("x1"), y = V("prop"), famille = "beta")),
  ZIP = function() essayer_modele("GLM", list(x = V("x1"), y = V("compte"), famille = "poisson_zi")),
  GLMM = function() essayer_modele("GLM", list(x = V("x1"), y = V("compte"), famille = "poisson", aleatoires = list(groupe("site")))),
  GAM = function() essayer_modele("GAM", list(x = V("x1", "x2"), y = V("y"), famille = "gaussienne", lisses = V("x1")), function(id, r) {
    suite_regression(id, r)
    demander("seuils", list(modele = id, terme = CONTEXTE$noms[["x1"]] %||% "x1"))
  }),
  GAM_SPATIAL = function() essayer_modele("GAM", list(x = V("x1"), y = V("y"), famille = "gaussienne", lisse_spatial = list(variables = V("lon", "lat")))),
  RDA = function() essayer_modele("RDA", list(x = V("x1", "habitat"), y = ESPECES(), options = list(transformation = "hellinger", permutations = 19, graine = 1)),
                          function(id, r) demander("rda_tests", list(modele = id))),
  # (des X qualitatifs seuls : des centroïdes, aucune flèche)
  RDA_QUALITATIVE = function() essayer_modele("RDA", list(x = V("habitat"), y = ESPECES(), options = list(transformation = "hellinger", permutations = 19, graine = 1))),
  CCA_QUALITATIVE = function() essayer_modele("CCA", list(x = V("habitat"), y = ESPECES(), options = list(permutations = 19, graine = 1))),
  CA = function() essayer_modele("CA", list(x = ESPECES())),
  CCA = function() essayer_modele("CCA", list(x = V("x1"), y = ESPECES(), options = list(permutations = 19, graine = 1)), function(id, r) demander("rda_tests", list(modele = id))),
  INDVAL = function() essayer_modele("INDVAL", list(y = ESPECES(), classe = v1("habitat"), options = list(permutations = 19, graine = 1))),
  PCOA = function() essayer_modele("PCOA", list(x = V("x1", "habitat"), y = ESPECES(), options = list(distance = "bray", permutations = 19, graine = 1))),
  NMDS = function() essayer_modele("NMDS", list(x = V("x1"), y = ESPECES(), options = list(distance = "bray", k = 2, departs = 3, permutations = 19))),
  PERMANOVA = function() essayer_modele("PERMANOVA", list(x = V("habitat", "x1"), y = ESPECES(), options = list(distance = "bray", permutations = 19, graine = 1))),
  MANTEL = function() essayer_modele("MANTEL", list(x = V("x1"), y = ESPECES(), echelle = "UV", options = list(distance = "bray", permutations = 19, graine = 1))),
  LDA = function() essayer_modele("LDA", list(x = X3(), classe = v1("habitat"))),
  TREE = function() essayer_modele("TREE", list(x = V("x1", "x2", "x3", "habitat"), y = V("y"), options = list(minbucket = 5)),
                           function(id, r) demander("arbre_stabilite", list(modele = id))),
  TREE_CLASSES = function() essayer_modele("TREE", list(x = X3(), classe = v1("habitat"), options = list(minbucket = 5))),
  KMEANS = function() essayer_modele("KMEANS", list(x = X3(), echelle = "UV", options = list(k = 3)), function(id, r) demander("kmeans_choix", list(modele = id))),
  FOREST = function() essayer_modele("FOREST", list(x = V("x1", "x2", "x3", "habitat"), y = V("y"), options = list(arbres = 50)),
                             function(id, r) demander("dependance_partielle", list(modele = id))),
  FOREST_CLASSES = function() essayer_modele("FOREST", list(x = X3(), classe = v1("habitat"), options = list(arbres = 50))),
  BOOST = function() essayer_modele("BOOST", list(x = X3(), y = V("y"), options = list(loi = "gaussienne", arbres_max = 40, taux = 0.1, profondeur = 2, min_feuille = 3)),
                            function(id, r) demander("dependance_partielle", list(modele = id))),
  BOOST_PRESENCE = function() essayer_modele("BOOST", list(x = X3(), y = V("presence"), options = list(loi = "binomiale", arbres_max = 40, taux = 0.1, profondeur = 1, min_feuille = 3))),
  TRIM = function() essayer_modele("TRIM", list(x = list(), y = V("compte"), options = list(site = groupe("site"),
                                                                                temps = groupe("annee"), modele = 3))),
  DISTANCE = function() essayer_modele("DISTANCE", list(x = list(), y = V("distance"), options = list(
    transect = "ligne", echantillon = groupe("site"), effort = v1("longueur"), strate = groupe("strate"),
    aire = v1("aire"), troncature = list(type = "pourcentage", valeur = 5), cle = "hn", serie = "cos", ajustements = "aic", monotonie = "stricte",
    unites = list(distance = "m", effort = "km", aire = "km2")))),
  STATISTIQUES = function() demander("statistiques_variables", list(projet = CONTEXTE$projet)),
  TESTS = function() {
    demander("tests_variables", list(projet = CONTEXTE$projet, classe = v1("habitat"), test = "welch", correction = "BH"))
    demander("test_deux_variables", list(projet = CONTEXTE$projet, x = v1("habitat"), y = v1("x1")))
    demander("test_deux_variables", list(projet = CONTEXTE$projet, x = v1("x1"), y = v1("x2")))
    demander("test_deux_variables", list(projet = CONTEXTE$projet, x = v1("habitat"), y = v1("strate")))
  },
  CORRELATIONS = function() demander("correlations_variables", list(projet = CONTEXTE$projet, methode = "pearson", correction = "BH")),
  SERIES = function() demander("series", list(projet = CONTEXTE$projet, variable = v1("y"), temps = colonne_role("annee"), lignes = colonne_role("site"), tendances = TRUE)),
  BACI = function() {
    sites <- CONTEXTE$categories[["site"]] %||% character(0)
    base <- list(projet = CONTEXTE$projet, y = v1("y"), temps = colonne_role("annee"), unite = colonne_role("site"), impact = as.list(utils::head(sites, 1)),
                 temoins = as.list(utils::head(sites[-1], 3)))
    demander("baci", c(base, list(evenement = 2017)))
    demander("baci_modele", c(base, list(evenement = 2017, car1 = FALSE)))
  },
  RAREFACTION = function() demander("rarefaction", list(projet = CONTEXTE$projet, especes = ESPECES(), groupe = colonne_role("habitat"))),
  SPATIAL = function() {
    r <- demander("coordonnees", list(projet = CONTEXTE$projet, regler = TRUE, x = v1("lon"), y = v1("lat"), systeme = "4326"))
    if (is.null(r)) return(invisible(NULL))
    demander("carte", list(projet = CONTEXTE$projet))
    demander("autocorrelation_spatiale", list(projet = CONTEXTE$projet, variable = v1("y"), permutations = 19))
  },
  IMPUTER = function() demander("imputer", list(projet = CONTEXTE$projet, variables = X3(), k = 1)),
  # (des demandes étranges mais possibles d'un client : un paramètre absent qu'un autre, plus long,
  # remplacerait par la correspondance partielle de $, un type ou une validation inconnus, un champ en
  # trop, un identifiant inconnu, un paramètre manquant ou hors de son domaine ; chacune doit donner
  # un message du moteur, ou ne rien changer qu'elle ne dit)
  PROTOCOLE = function() {
    P <- CONTEXTE$projet
    modifie <- function() isTRUE(demander("projet", list(projet = P))$modifie)
    # (une demande qui doit être refusée, par un message du moteur)
    refus <- function(nom, q, pourquoi) if (!is.null(demander(nom, q))) attendre(FALSE, nom, paste("acceptée :", pourquoi))
    refus("classes_valeurs", list(projet = P, nom = "classes", variable = v1("x1")), "sans bornes ni nombre de classes")
    q <- requete_modele("PCA", list(x = X3()))
    q$echelle <- NULL
    q$echelles <- stats::setNames(list("UV"), as.character(v1("x1")))
    r <- demander("ajuster", q)
    if (!is.null(r)) attendre(identical(r$reglages$echelles[[as.character(v1("x2"))]], "UV"), "ajuster", "sans echelle, l'échelle par défaut n'est pas UV")
    refus("ajuster", requete_modele("ACP", list(x = X3())), "un type inconnu")
    refus("verifier", requete_modele("ACP", list(x = X3())), "un type inconnu")
    refus("ajuster", c(requete_modele("PCA", list(x = X3())), list(validation = "sans_validation")), "une validation inconnue")
    refus("ajuster", requete_modele("PCA", list(x = list())), "une ACP sans X")
    lm <- demander("ajuster", requete_modele("LMM", list(x = V("annee", "x1"), y = V("y"), aleatoires = list(groupe("site")))))
    po <- demander("ajuster", requete_modele("GLM", list(x = V("x1", "habitat"), y = V("compte"), famille = "poisson")))
    # (des réglages choisis, gardés avec le modèle ; une demande avec un champ en trop ne les change pas)
    if (!is.null(po)) demander("moyennes", list(modele = po$id, ajustement = "holm"))
    if (!is.null(lm)) demander("puissance_tendance", list(modele = lm$id, alpha = 0.1))
    demander("enregistrer", list(projet = P, chemin = fichier("protocole.nuee")))
    if (!is.null(po)) {
      r <- demander("moyennes", list(modele = po$id, langue = "en"))
      attendre(identical(r$reglages$ajustement, "holm") && !modifie(), "moyennes", "un champ en trop a remplacé les réglages gardés avec le modèle")
    }
    if (!is.null(lm)) {
      r <- demander("puissance_tendance", list(modele = lm$id, langue = "en"))
      attendre(identical(r$reglages$alpha, 0.1) && !modifie(), "puissance_tendance", "un champ en trop a remplacé les réglages gardés avec le modèle")
      r <- demander("puissance_tendance", list(modele = lm$id, unites = list(5, 10)))
      attendre(!is.null(r) && r$reglages$unite %in% c("pente", "pourcent"), "puissance_tendance", "sans unite, les unités ont été lues comme l'unité de la tendance")
    }
    demander("enregistrer", list(projet = P, chemin = fichier("protocole.nuee")))
    refus("document_enregistrer", list(projet = P, id = "D99", texte = "x"), "un document inconnu")
    refus("document_supprimer", list(projet = P, id = "D99"), "un document inconnu")
    refus("figure_enregistrer", list(projet = P, id = "F99", titre = "x"), "une figure inconnue")
    refus("figure_supprimer", list(projet = P, id = "F99"), "une figure inconnue")
    attendre(!modifie(), "document_supprimer", "un document ou une figure inconnus ont marqué le projet modifié")
    refus("fermer_projet", list(projet = "P99999"), "un projet inconnu")
    refus("rapports_vraisemblance", list(modeles = list("M99999")), "un modèle inconnu")
    refus("console", list(code = "1 + 1", modele = "M99999"), "un modèle inconnu")
    refus("completer", list(texte = "su", modele = "M99999"), "un modèle inconnu")
    refus("console", list(code = "1 + 1", projet = "P99999"), "un projet inconnu")
    demander("variable_calculee", list(projet = P, nom = "pour_modifier", formule = "x1 + 1"))
    demander("enregistrer", list(projet = P))
    attendre(modifie(), "enregistrer", "enregistré dans le dossier temporaire, le projet est marqué enregistré")
    refus("projet", list(), "sans projet")
    refus("modele", list(), "sans modèle")
    refus("modifier_valeur", list(projet = P, observation = 99999, variable = v1("x1"), valeur = "1"), "une observation inconnue")
    refus("coller_cellules", list(projet = P, observations = list(99999), variables = list(v1("x1")), valeurs = list(list("1"))), "une observation inconnue")
    refus("classe_selection", list(projet = P, variable = "groupe", etiquette = "a", observations = list(99999)), "une observation inconnue")
    refus("jeu_test", list(projet = P, x = X3(), methode = "autre"), "une méthode inconnue")
    if (!is.null(lm)) {
      refus("par_reponse", list(modele = lm$id, reponses = V("x2"), correction = "autre"), "une correction inconnue")
    }
    # (le type des résidus ne vaut que pour un modèle de mgcv)
    if (!is.null(po)) refus("autocorrelation_residus", list(modele = po$id, temps = groupe("annee"), groupe = list(groupe("site")), type = "autre"), "un type inconnu")
    pca <- demander("ajuster", requete_modele("PCA", list(x = X3(), echelle = "UV")))
    if (!is.null(pca)) {
      refus("contributions", list(modele = pca$id, observations = list(1), type = "autre"), "un type inconnu")
      refus("contributions", list(modele = pca$id, observations = list(1), type = "score", composante = 99), "une composante que le modèle n'a pas")
      refus("hca", list(modele = pca$id, composantes = 99), "des composantes que le modèle n'a pas")
      refus("predire", list(modele = pca$id, jeu = 9), "un jeu de prédiction inconnu")
      refus("predire_fichier", list(modele = pca$id, chemin = fichier("absent.csv")), "un fichier absent")
      refus("reajuster", list(modele = pca$id, garder = list("inconnue")), "aucun terme X gardé")
      refus("script_r", list(modele = pca$id, chemin = fichier("script.txt")), "une extension que l'export n'écrit pas")
    }
    pls <- demander("ajuster", requete_modele("PLS", list(x = X3(), y = V("y"), echelle = "UV")))
    if (!is.null(pls)) refus("permutations", list(modele = pls$id, y = 9, n = 3), "un Y que le modèle n'a pas")
    demander("coordonnees", list(projet = P, regler = TRUE, x = v1("lon"), y = v1("lat"), systeme = "4326"))
    refus("autocorrelation_spatiale", list(projet = P, variable = v1("y"), permutations = 9, voisinage = list(methode = "autre")), "un voisinage inconnu")
    refus("modeles_classe", requete_modele("PCA", list(x = X3())), "sans variable de classe")
    refus("enregistrer", list(projet = P, chemin = fichier(file.path("absent", "protocole.nuee"))), "un dossier qui n'existe pas")
    refus("sauvegarde_secours", list(projet = P), "sans chemin")
  },
  # (un tableau de 200 000 lignes : les observations 100 000 et 200 000, dont as.character() écrit le
  # numéro « 1e+05 » et « 2e+05 », dans la vue Données, une nouvelle variable, une valeur changée, un
  # modèle sans deux observations, ses prédictions, l'export et l'enregistrement)
  LONG = function() {
    P <- CONTEXTE$projet
    n <- CONTEXTE$n_observations
    d <- demander("donnees", list(projet = P, colonnes = list(v1("x1"))))
    if (!is.null(d)) attendre(!anyNA(unlist(d$valeurs[[match("x1", d$variables)]])), "donnees", "la vue Données montre une valeur de x1 manquante")
    demander("variable_calculee", list(projet = P, nom = "double", formule = "x1 * 2"))
    demander("modifier_valeur", list(projet = P, observation = 100000, variable = v1("x1"), valeur = "1.5"))
    v <- demander("valeurs", list(projet = P, variable = v1("x1"), observations = list(100000, 200000)))
    if (!is.null(v)) attendre(identical(v$valeurs[1], 1.5) && !is.na(v$valeurs[2]), "valeurs", "x1 de l'observation 100 000 n'est pas 1,5, ou celui de 200 000 manque")
    s <- demander("statistiques_variables", list(projet = P))
    if (!is.null(s)) attendre(s$N[match("x1", s$nom)] == n, "statistiques_variables", "x1 n'a pas toutes ses valeurs")
    q <- requete_modele("LM", list(x = V("x1", "x2"), y = V("y")))
    q$exclues <- list(3, 100000)
    m <- demander("ajuster", q)
    if (is.null(m)) return(invisible(NULL))
    attendre(m$N == n - 2, "ajuster", sprintf("le modèle a %d observations, pas %d", m$N, n - 2))
    pr <- demander("predire", list(modele = m$id))
    if (!is.null(pr)) attendre(!anyNA(pr$observations[["y prédit"]]), "predire", "des prédictions manquent")
    r <- demander("reajuster", list(modele = m$id, exclues = list(200000)))
    if (!is.null(r)) attendre(r$N == n - 3, "reajuster", sprintf("le modèle réajusté a %d observations, pas %d", r$N, n - 3))
    demander("classes_valeurs", list(projet = P, nom = "classes", variable = v1("x1"), n = 3))
    demander("exporter_donnees", list(projet = P, chemin = fichier("long.csv")))
    demander("enregistrer", list(projet = P, chemin = fichier("long.nuee")))
    c <- demander("condition_observations", list(projet = P, condition = "!is.na(x1)"))
    if (!is.null(c)) attendre(c$n == n, "condition_observations", sprintf("la condition garde %d observations, pas %d", c$n, n))
  },
  VARIABLES = function() {
    demander("variable_calculee", list(projet = CONTEXTE$projet, nom = "nouvelle", formule = sprintf("`%s` * 2", CONTEXTE$noms[["x1"]] %||% "x1")))
    demander("classes_valeurs", list(projet = CONTEXTE$projet, nom = "classes", variable = v1("x1"), n = 3))
  }
)

# ---------------------------------------------------------------- les cas

# Un cas : ses données (un data.frame, ou les lignes d'un fichier), ce qu'il change aux demandes
# d'ajustement (`requete`), les analyses qu'il essaie (`analyses`, une expression régulière sur leur
# nom ; sans elle, toutes, sauf celles d'un seul cas), celles que ses données concernent, les seules
# qu'il essaie sans --tout (`concernees` : des noms joints par « | »), les NaN et Inf qu'il permet
# en plus (`nan_permis`, comme NON_FINIS_PERMIS) et le temps permis à chaque demande.
# Les noms étranges du cas « noms » : espaces, accents, émojis, des nombres, des signes.
NOMS_ETRANGES <- c(nom = "nom", site = "Site d'étude", annee = "année 🐦", habitat = "habitat (type)", strate = "strate", x1 = "2019", x2 = "1e5",
                   x3 = "x 3 — mesure", y = "réponse ŷ", compte = "nb 🐟", presence = "presence", prop = "prop", positif = "positif",
                   sp1 = "Anas platyrhynchos", sp2 = "Ardea  cinerea", sp3 = "3", sp4 = "sp.4", sp5 = "-1", lon = "longitude (°)",
                   lat = "latitude (°)", distance = "distance", longueur = "longueur", aire = "aire", taille = "taille")

change <- function(f, ...) function() f(tableau_base(...))
# (les analyses d'une variable de classe, du temps, des communautés, de l'espace)
PAR_CLASSE <- "PLS-DA|OPLS-DA|CLASSES|LDA|TREE_CLASSES|FOREST_CLASSES|INDVAL|RDA|RDA_QUALITATIVE|CCA_QUALITATIVE|PCOA|PERMANOVA|LM|POISSON|TREE|FOREST|TESTS|RAREFACTION|STATISTIQUES"
DANS_LE_TEMPS <- "LMM|CAR1|TRIM|SERIES|BACI|STATISTIQUES"
COMMUNAUTES <- "RDA|RDA_QUALITATIVE|CCA_QUALITATIVE|CA|CCA|INDVAL|PCOA|NMDS|PERMANOVA|MANTEL|RAREFACTION"
DANS_L_ESPACE <- "LM|GAM_SPATIAL|SPATIAL"
CAS <- list(
  # (l'export en script R, qui refait chaque modèle, n'est essayé qu'avec les numéros 100 000 :
  # numeros_100000)
  base = list(donnees = function() tableau_base()),
  protocole = list(donnees = function() tableau_base(), analyses = "^PROTOCOLE$"),
  # (les numéros internes 100 000, 200 000… : les mêmes réponses que le cas de base)
  numeros_100000 = list(donnees = function() tableau_base(), renumeroter = TRUE, reference = "base", scripts = TRUE),
  # (vide après une condition ou des exclusions)
  condition_vide = list(donnees = function() tableau_base(), requete = function(q) { q$condition <- "x1 > 1000"; q }),
  toutes_exclues = list(donnees = function() tableau_base(), requete = function(q) { q$exclues <- as.list(seq_len(60)); q }),
  une_observation = list(donnees = function() tableau_base()[1, ]),
  deux_observations = list(donnees = function() tableau_base()[1:2, ]),
  trois_observations = list(donnees = function() tableau_base()[c(1, 8, 15), ]),
  x_manquante = list(donnees = change(function(df) { df$x1 <- NA_real_; df })),
  reponses_manquantes = list(donnees = change(function(df) { df[c("y", "compte", "presence", "prop", "positif", "distance", "sp1")] <- NA_real_; df })),
  ligne_vide = list(donnees = change(function(df) { df[3, -1] <- NA; df })),
  x_constante = list(donnees = change(function(df) { df$x1 <- 3; df })),
  reponses_constantes = list(donnees = change(function(df) {
    df[c("y", "compte", "presence", "prop", "positif", "distance")] <- list(2, 4, 1, 0.4, 1.5, 10)
    df[paste0("sp", 1:5)] <- 2
    df
  })),
  x_deux_valeurs = list(donnees = change(function(df) { df$x1 <- as.numeric(df$x1 > 0); df })),
  un_niveau = list(donnees = change(function(df) { df$habitat <- "bois"; df$strate <- "A"; df }), concernees = paste(PAR_CLASSE, "DISTANCE", sep = "|")),
  classe_une_observation = list(donnees = change(function(df) { df$habitat[1] <- "marais"; df }), concernees = PAR_CLASSE),
  # (sans --tout, ni LM, dont les moyennes ajustées comparent 19 900 paires en une demi-minute, ni LDA :
  # celles du Poisson suffisent ; une catégorie sans aucun comptage a une borne haute infinie, que la
  # phrase de sa paire écrit ∞)
  niveaux_200 = list(donnees = function() { df <- tableau_base(40, 10); df$habitat <- sprintf("h%03d", rep(1:200, each = 2)); df }, limite = 120,
                     concernees = "PLS-DA|OPLS-DA|CLASSES|TREE_CLASSES|FOREST_CLASSES|INDVAL|RDA_QUALITATIVE|CCA_QUALITATIVE|PERMANOVA|POISSON|TESTS|RAREFACTION|STATISTIQUES",
                     nan_permis = "^moyennes: \\$(moyennes|paires)\\$haut$"),
  noms = list(donnees = change(function(df) {
    df$habitat <- c(bois = "forêt 🌲", lande = "lande sèche", pre = "pré, humide")[df$habitat]
    df$site <- sprintf("%03d", as.integer(sub("S", "", df$site)) * 7)
    names(df) <- NOMS_ETRANGES[names(df)]
    df
  }), noms = NOMS_ETRANGES),
  identifiants_doubles = list(donnees = change(function(df) { df$nom <- paste0("o", (seq_len(nrow(df)) + 1) %/% 2); df }),
                              concernees = "PCA|PLS-DA|LM|LMM|TREE|NMDS|TRIM|DISTANCE|SERIES|BACI|SPATIAL|STATISTIQUES|IMPUTER|VARIABLES"),
  # (sans --tout, ni le Poisson à excès de zéros, le plus long à ajuster : la binomiale négative et la
  # bêta montrent de même les erreurs-types que mgcv perd)
  grands_petits = list(donnees = change(function(df) { df$x1 <- df$x1 * 1e12; df$x2 <- df$x2 * 1e-12; df$y <- df$y * 1e9; df$positif <- df$positif * 1e-9; df }),
                       concernees = "PCA|PLS|LM|POISSON|BINNEG|LOGIT|GAMMA|BETA|GLMM|GAM|STATISTIQUES|TESTS"),
  extremes = list(donnees = change(function(df) {
    df$x1 <- df$x1 * 1e300; df$x2 <- df$x2 * 1e-300; df$y <- df$y * 1e300; df$positif <- df$positif * 1e-300
    df$lon <- df$lon * 1e300
    df
  }), nan_permis = c(
    # (la table des statistiques de chaque variable : l'écart-type de valeurs vers 1e300, dont les carrés
    # dépassent les nombres de R, et ce qui en découle restent des cases vides ; les analyses, elles,
    # refusent ces valeurs en le disant)
    "^statistiques_variables: \\$(ecart_type|et_moyenne|asymetrie|test_asymetrie|aplatissement)$")),
  inf_nan = list(donnees = function() {
    df <- tableau_base()
    df$x1 <- as.character(df$x1)
    df$x1[1:6] <- c("Inf", "-Inf", "NaN", "#DIV/0!", "#N/A", "inf")
    df$y <- as.character(df$y)
    df$y[7:9] <- c("NaN", "Inf", "-inf")
    df
  }, concernees = "PCA|LM|TREE|STATISTIQUES|TESTS|CORRELATIONS|IMPUTER|VARIABLES"),
  dates_jour_mois = list(donnees = change(function(df) { df$annee <- sprintf("%02d/05/%d", (seq_len(nrow(df)) - 1) %% 28 + 1, df$annee); df }),
                         concernees = DANS_LE_TEMPS),
  dates_melangees = list(donnees = change(function(df) {
    f <- c("%d-05-14", "14/05/%d", "May 14 %d", "14.05.%d", "%d0514", "%d-05-14T10:30:00")
    df$annee <- vapply(seq_len(nrow(df)), function(i) sprintf(f[(i - 1) %% length(f) + 1], df$annee[i]), "")
    df
  }), concernees = DANS_LE_TEMPS),
  un_seul_temps = list(donnees = change(function(df) { df$annee <- 2018; df }), concernees = DANS_LE_TEMPS),
  # (x1 et x2 parfaitement corrélés : le t de leur test est infini, une case vide, sa valeur p 0)
  colineaires = list(donnees = change(function(df) { df$x2 <- 2 * df$x1; df$x3 <- df$x1 + df$x2; df }),
                     concernees = "PCA|PLS|OPLS|PLS-DA|OPLS-DA|CLASSES|LM|LOGIT|GAM|LDA|KMEANS|TESTS|CORRELATIONS",
                     nan_permis = "^test_deux_variables: \\$tests\\$statistique$"),
  separation = list(donnees = change(function(df) { df$presence <- as.integer(df$x1 > 0); df }), concernees = "LOGIT|BOOST_PRESENCE"),
  comptes_negatifs = list(donnees = change(function(df) { df$compte[1:3] <- c(-1, -2, -5); df$sp1[1:2] <- -1; df$distance[1] <- -5; df }),
                          concernees = paste("POISSON|BINNEG|ZIP|GLMM|TRIM|DISTANCE", COMMUNAUTES, sep = "|")),
  proportions_0_1 = list(donnees = change(function(df) { df$prop[1:4] <- c(0, 1, 0, 1); df }), concernees = "BETA"),
  presence_texte = list(donnees = change(function(df) { df$presence <- c("non", "oui")[df$presence + 1]; df }), concernees = "LOGIT|BOOST_PRESENCE"),
  coordonnees_manquantes = list(donnees = change(function(df) { df$lon[1:10] <- NA; df$lat[5:15] <- NA; df }), concernees = DANS_L_ESPACE),
  meme_lieu = list(donnees = change(function(df) { df$lon <- 4.6; df$lat <- 43.5; df }), concernees = DANS_L_ESPACE),
  trim_site_sans_comptage = list(donnees = change(function(df) { df$compte[df$site == "S03"] <- NA; df$compte[df$site == "S04"] <- 0; df }),
                                 concernees = "TRIM|SERIES"),
  distance_strate_vide = list(donnees = change(function(df) { df$distance[df$strate == "B"] <- NA; df }), concernees = "DISTANCE"),
  communaute_vide = list(donnees = change(function(df) { df[1, paste0("sp", 1:5)] <- 0; df$sp5 <- 0; df }), concernees = COMMUNAUTES),
  long_200000 = list(donnees = function() tableau_base(500, 400), analyses = "^LONG$", limite = 240),
  large = list(donnees = function() {
    df <- tableau_base()
    set.seed(5)
    cbind(df, as.data.frame(matrix(round(stats::rnorm(nrow(df) * 5000), 3), nrow(df), 5000, dimnames = list(NULL, sprintf("v%04d", 1:5000)))))
  }, limite = 120, concernees = "PCA_TOUT|STATISTIQUES|TESTS|CORRELATIONS|SPATIAL|IMPUTER|VARIABLES")
)

# ---------------------------------------------------------------- le déroulement

ouvrir_cas <- function(nom, cas) {
  chemin <- fichier(paste0(nom, if (isTRUE(cas$renumeroter)) ".json" else ".csv"))
  d <- cas$donnees()
  if (isTRUE(cas$renumeroter)) ecrire_renumerote(d, chemin)
  else if (is.character(d)) {
    con <- file(chemin, "wb")
    writeLines(enc2utf8(d), con, sep = "\n", useBytes = TRUE)
    close(con)
  } else ecrire_csv(d, chemin)
  CONTEXTE$noms_fichiers <- c(CONTEXTE$noms_fichiers, basename(chemin))
  CONTEXTE$analyse <- "ouvrir"
  p <- demander("ouvrir", list(chemin = chemin), limite = CONTEXTE$limite)
  if (is.null(p)) return(NULL)
  vars <- p$variables
  ids <- if (length(p$identifiants)) p$identifiants else data.frame(rang = integer(0), nom = character(0))
  # (le rôle de chaque colonne : son nom dans le tableau de base, sous lequel les analyses la
  # cherchent ; un cas qui renomme les colonnes donne leur nom, `noms`)
  noms <- if (!is.null(cas$noms)) cas$noms else stats::setNames(c(vars$nom, ids$nom), c(vars$nom, ids$nom))
  i <- match(noms, vars$nom)
  CONTEXTE$v <- stats::setNames(as.list(vars$uid[i]), names(noms))[!is.na(i)]
  CONTEXTE$noms <- stats::setNames(as.list(unname(noms)), names(noms))[!is.na(i)]
  CONTEXTE$categories <- stats::setNames(lapply(i[!is.na(i)], function(k) as.character(unlist(vars$categories[[k]]))), names(noms)[!is.na(i)])
  CONTEXTE$quantitatives <- vars$uid[!vars$qualitative]
  CONTEXTE$n_observations <- length(p$observations$uid)
  # (les identifiants secondaires, par rôle : leur rang et leur nom)
  j <- match(noms, ids$nom)
  CONTEXTE$ids <- stats::setNames(as.list(ids$rang[j]), names(noms))[!is.na(j)]
  CONTEXTE$noms_ids <- stats::setNames(as.list(ids$nom[j]), names(noms))[!is.na(j)]
  p$id
}

# (les analyses d'un seul cas, qui les nomme)
SUR_DEMANDE <- c("LONG", "PROTOCOLE")

# (dans une fonction : ses variables ne sont pas des globales que le moteur pourrait lire)
derouler <- function() {
  for (nom_cas in names(CAS)) {
    if (nzchar(MOTIF_CAS) && !grepl(MOTIF_CAS, nom_cas, perl = TRUE)) next
    cas <- CAS[[nom_cas]]
    cat(nom_cas, "\n")
    CONTEXTE$cas <- nom_cas
    CONTEXTE$cas_def <- cas
    CONTEXTE$limite <- (cas$limite %||% 60) * FACTEUR_LIMITE
    CONTEXTE$requete <- cas$requete
    # (le cas de base garde l'empreinte de chaque réponse ; un cas qui le renumérote doit les redonner)
    CONTEXTE$vues <- new.env()
    CONTEXTE$garder <- identical(nom_cas, "base")
    CONTEXTE$reference <- cas$reference
    CONTEXTE$scripts <- isTRUE(cas$scripts)
    id <- ouvrir_cas(nom_cas, cas)
    if (is.null(id)) next
    CONTEXTE$projet <- id
    for (nom_analyse in names(ANALYSES)) {
      if (nzchar(MOTIF_ANALYSES) && !grepl(MOTIF_ANALYSES, nom_analyse, perl = TRUE)) next
      if (!is.null(cas$analyses) && !grepl(cas$analyses, nom_analyse, perl = TRUE)) next
      if (!TOUT && !is.null(cas$concernees) && !grepl(paste0("^(", cas$concernees, ")$"), nom_analyse, perl = TRUE)) next
      if (nom_analyse %in% SUR_DEMANDE && is.null(cas$analyses)) next
      CONTEXTE$analyse <- nom_analyse
      tryCatch(ANALYSES[[nom_analyse]](), role_absent = function(e) noter_demande(nom_cas, nom_analyse, "(rôles)", "sans", conditionMessage(e)),
               error = function(e) noter_demande(nom_cas, nom_analyse, "(harnais)", "harnais", conditionMessage(e)))
    }
    DEMANDES$fermer_projet(list(projet = id))
  }
  tous <- do.call(rbind, lapply(RESULTATS, as.data.frame))
  if (nzchar(argument("durees"))) utils::write.csv(tous, argument("durees"), row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n", nrow(tous), " demandes : ", paste(sprintf("%s %d", names(table(tous$statut)), table(tous$statut)), collapse = ", "), "\n", sep = "")
  # (les plus longues, pour régler les limites de temps)
  lentes <- utils::head(tous[order(-tous$duree), ], 8)
  cat("Les plus longues :", paste(sprintf("%s › %s › %s %.1f s", lentes$cas, lentes$analyse, lentes$demande, lentes$duree), collapse = " ; "), "\n")
  echecs <- tous[tous$statut %in% ECHECS, , drop = FALSE]
  if (nrow(echecs)) {
    cat("\n", nrow(echecs), " échec(s) :\n", sep = "")
    for (i in seq_len(nrow(echecs))) {
      cat(sprintf("  ÉCHEC   %s › %s › %s : %s %s\n", echecs$cas[i], echecs$analyse[i], echecs$demande[i], echecs$statut[i],
                  substr(paste(echecs$message[i], echecs$chemins[i]), 1, 300)))
    }
  } else cat("Aucun échec : chaque demande donne un résultat fini ou un message du moteur dans sa langue.\n")
  invisible(tous)
}

# (une erreur de R, un message qui ne change pas avec la langue, une langue fausse, un message du
# moteur qui montre une erreur de R, un délai dépassé, un résultat autre qu'attendu, un NaN ou un Inf
# non permis, une erreur du harnais lui-même)
ECHECS <- c("R", "R?", "langue", "délai", "écart", "nan", "harnais")
TOUS <- derouler()
if (any(TOUS$statut %in% ECHECS)) quit(status = 1)
