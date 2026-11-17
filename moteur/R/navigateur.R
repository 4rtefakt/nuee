# La version navigateur : le moteur tourne tel quel dans R compilé en WebAssembly (webR, dans
# l'onglet : app/interface/moteur_webr.js), qui donne options(nuee.navigateur = TRUE) et, dans
# nuee.paquets_absents, les paquets R qu'il n'a pas (pas installés, ou que webR ne sait pas
# charger : app/interface/paquets_webr.js), dans nuee.paquets_sans_webr ceux qu'il n'installe jamais.
# Ce qui en a besoin dit qu'il faut l'application de bureau ; le reste marche.
dans_navigateur <- function() isTRUE(getOption("nuee.navigateur"))

# Des textes rangés comme le R de l'application de bureau les range (les catégories d'une variable lue
# d'un fichier, faite d'une formule ou venue d'une mise à jour : nouveau.R, variables.R, etapes.R,
# kobo.R ; les clés d'un appariement) : dans l'ordre de la langue (é avec e, une minuscule avant sa
# majuscule), celui d'ICU ou de la bibliothèque C du système. Le R de webR les rangerait par leurs
# octets (« B » avant « a », « traité » avant « témoin » : ni ICU, ni ordre des langues dans sa libc,
# musl), et la catégorie de référence d'une régression changerait : il prend l'ordre du navigateur
# (Intl.Collator, dans le worker de webR). Sans les valeurs manquantes, comme sort().
trier_textes <- function(x) {
  if (!identical(R.version$os, "emscripten")) return(sort(x))
  x <- as.character(x[!is.na(x)])
  if (length(x) < 2) return(x)
  js <- sprintf("JSON.stringify((() => { const x = %s, c = new Intl.Collator(); return x.map((v, i) => i).sort((a, b) => c.compare(x[a], x[b]) || (x[a] < x[b] ? -1 : x[a] > x[b] ? 1 : 0)); })())",
                jsonlite::toJSON(x))
  ordre <- tryCatch(jsonlite::fromJSON(getExportedValue("webr", "eval_js")(js)), error = function(e) NULL)
  if (length(ordre) == length(x)) x[ordre + 1L] else sort(x)
}

# Ce que retire chaque paquet absent du navigateur (pour le message).
FONCTIONS_PAQUETS <- list(
  RSQLite = c("Les fichiers GeoPackage", "GeoPackage files"),
  ranger = c("Les forêts aléatoires", "Random forests"),
  nlme = c("Les modèles mixtes", "Mixed models"),
  mgcv = c("Les modèles généralisés et additifs", "Generalized and additive models"),
  haven = c("Les fichiers SPSS, Stata et SAS", "SPSS, Stata and SAS files"),
  readxl = c("Les classeurs Excel", "Excel workbooks"),
  openxlsx = c("Les classeurs Excel", "Excel workbooks"),
  foreign = c("Les fichiers dBase et Weka", "dBase and Weka files"),
  zip = c("Les fichiers jamovi", "jamovi files")
)

# Ce que le navigateur n'a pas : « <quoi> : il faut l'application de bureau de Nuée (<pourquoi>). »
message_navigateur <- function(quoi, pourquoi = NULL) {
  sprintf(traduire("%s : il faut l'application de bureau de Nuée%s.", "%s: Nuée's desktop application is needed%s."), quoi,
          if (is.null(pourquoi)) "" else sprintf(" (%s)", pourquoi))
}
refus_navigateur <- function(fr, en, pourquoi = NULL) stop(message_navigateur(traduire(fr, en), pourquoi), call. = FALSE)

# Le refus d'une demande, dans le navigateur : celui d'un paquet qui y manque (« there is no
# package called », ou le refus du moteur qui dit de l'installer : « le paquet R ranger ») dit
# qu'il faut l'application de bureau. Hors du navigateur, l'erreur telle qu'elle est ; et de même
# les refus du moteur d'une classe à eux, que l'interface lit (un code R, un greffon, un conflit…).
erreur_navigateur <- function(e) {
  if (!dans_navigateur()) return(e)
  if (!inherits(e, "packageNotFoundError") && length(setdiff(class(e), c("simpleError", "error", "condition")))) return(e)
  paquet <- if (inherits(e, "packageNotFoundError")) as.character(e$package)[1]
  if (is.null(paquet)) {
    absents <- as.character(getOption("nuee.paquets_absents") %||% character(0))
    paquet <- Find(function(p) grepl(sprintf("(paquet|package) (R )?%s([^[:alnum:].]|$)", p), conditionMessage(e)), absents)
  }
  if (is.null(paquet)) return(e)
  quoi <- FONCTIONS_PAQUETS[[paquet]] %||% c("Cette fonction", "This feature")
  simpleError(message_navigateur(traduire(quoi[1], quoi[2]),
                                 sprintf(traduire("le paquet R %s n'est pas dans la version navigateur", "the R package %s isn't in the browser version"), paquet)))
}

# ---------------------------------------------------------------- Arrêter

# Dans le navigateur, Arrêter interrompt le R de l'onglet (webR.interrupt() : moteur_webr.js), qui le
# voit au plus tard mille évaluations plus loin, partout dans du code R (pas dans le code compilé d'un
# paquet : un ajustement de nlme ou de mgcv finit d'abord). Une demande (serveur.R : traiter_demande)
# n'est interrompue que dans son calcul, où l'interruption devient le redémarrage « arreter_calcul » du
# fichier d'arrêt de l'application de bureau (faire_demande : calcul_arretable) : la demande rend
# « arrêtée », la séance telle qu'elle était avant elle. Une interruption venue avant elle (Arrêter à la
# fin de la précédente) passe d'abord, et une qui vient le calcul fini ne change pas sa réponse.
passer_interruption <- function() tryCatch(for (i in seq_len(1001L)) NULL, interrupt = function(c) NULL)
hors_interruptions <- function(expr) {
  if (!dans_navigateur()) return(expr)
  passer_interruption()
  r <- NULL
  tryCatch(suspendInterrupts(r <- expr), interrupt = function(c) NULL)
  passer_interruption()
  r
}
calcul_arretable <- function(expr) {
  if (!dans_navigateur()) return(expr)
  allowInterrupts(withCallingHandlers(expr, interrupt = function(c) invokeRestart("arreter_calcul")))
}

# ---------------------------------------------------------------- les paquets d'un code R, d'un greffon

# Les paquets que demande un modèle de code R ou un greffon (code.R, greffons.R), dans le navigateur :
# ceux qui manquent installés dans le R de l'onglet comme moteur_webr.js installe ceux du moteur, depuis
# les dépôts de l'option webr_pkg_repos : la copie du dépôt de webR du site s'il en a une (nuee.io),
# puis le dépôt de webR lui-même (repo.r-wasm.org), pour ce qu'elle n'a pas ; pas ceux dont un des
# paquets est de ceux que webR ne sait pas charger (nuee.paquets_sans_webr : une bibliothèque qui ne se
# charge pas empêche webR d'en charger aucune autre ensuite). Rend ceux qui manquent encore.
installer_paquets_navigateur <- function(paquets) {
  present <- function(p) nzchar(system.file(package = p))
  paquets <- setdiff(unique(as.character(unlist(paquets))), c("", PAQUETS_DE_R))
  manque <- paquets[!vapply(paquets, present, TRUE)]
  installer <- if (length(manque)) tryCatch(getExportedValue("webr", "install"), error = function(e) NULL)
  if (is.null(installer)) return(manque)
  depot <- sprintf("%s/bin/emscripten/contrib/%s", sub("/$", "", as.character(getOption("webr_pkg_repos") %||% "https://repo.r-wasm.org")),
                   sub("\\.[^.]+$", "", as.character(getRversion())))
  info <- tryCatch(utils::available.packages(contriburl = depot), error = function(e) NULL)
  if (is.null(info)) return(manque)
  exclus <- as.character(getOption("nuee.paquets_sans_webr") %||% character(0))
  for (p in intersect(manque, rownames(info))) {
    besoin <- c(p, unlist(tools::package_dependencies(p, db = info, which = c("Depends", "Imports"), recursive = TRUE)))
    if (!any(besoin %in% exclus)) tryCatch(suppressWarnings(suppressMessages(installer(p, info = info, quiet = TRUE))), error = function(e) NULL)
  }
  manque[!vapply(manque, present, TRUE)]
}

# Un code R évalué (code.R : evaluer_code, par la fonction `evaluer`) dans le navigateur : les paquets
# qu'il nomme (`paquets`) installés d'abord, puis tout paquet qui manque encore quand il s'exécute
# (chargé sans être nommé), et l'évaluation refaite. Hors du navigateur, l'évaluation seule.
evaluer_navigateur <- function(evaluer, paquets = character(0)) {
  if (!dans_navigateur()) return(evaluer())
  installer_paquets_navigateur(paquets)
  essayes <- character(0)
  repeat {
    ev <- evaluer()
    p <- as.character(ev$paquet %||% "")[1]
    if (is.na(p) || !nzchar(p) || p %in% essayes || length(installer_paquets_navigateur(p))) return(ev)
    essayes <- c(essayes, p)
  }
}

# Un paquet que la version navigateur n'a pas pu avoir (installer_paquets_navigateur) : ce qu'on en dit.
paquets_absents_navigateur <- function(paquets) {
  message_navigateur(sprintf(if (length(paquets) > 1) traduire("Les paquets R %s", "The R packages %s") else traduire("Le paquet R %s", "The R package %s"),
                             paste(paquets, collapse = ", ")),
                     traduire("le dépôt de webR ne l'a pas pour la version navigateur, ou webR ne sait pas le charger",
                              "webR's repository doesn't have it for the browser version, or webR can't load it"))
}
