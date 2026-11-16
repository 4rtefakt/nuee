# La version navigateur : le moteur tourne tel quel dans R compilé en WebAssembly (webR, dans
# l'onglet : app/interface/moteur_webr.js), qui donne options(nuee.navigateur = TRUE) et, dans
# nuee.paquets_absents, les paquets R qu'il n'a pas (pas installés, ou que webR ne sait pas
# charger : app/interface/paquets_webr.js). Ce qui en a besoin dit qu'il faut l'application de
# bureau ; le reste marche.
dans_navigateur <- function() isTRUE(getOption("nuee.navigateur"))

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
# qu'il faut l'application de bureau. Hors du navigateur, l'erreur telle qu'elle est.
erreur_navigateur <- function(e) {
  if (!dans_navigateur()) return(e)
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
