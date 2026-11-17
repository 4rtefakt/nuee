# La version navigateur : le moteur tourne tel quel dans R compilé en WebAssembly (webR, dans
# l'onglet : app/interface/moteur_webr.js), qui donne options(nuee.navigateur = TRUE) et, dans
# nuee.paquets_absents, les paquets R qu'il n'a pas (pas installés, ou que webR ne sait pas
# charger : app/interface/paquets_webr.js). Ce qui en a besoin dit qu'il faut l'application de
# bureau ; le reste marche.
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
