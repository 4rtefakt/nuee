# Arrêter un long calcul (1.1). Le moteur est un seul processus R : il ne lit pas de demande pendant
# qu'il calcule. Quand on appuie sur Arrêter, l'application (main.js) écrit donc un fichier dans le
# dossier temporaire de R de la séance, celui de options(nuee.arret) (serveur.R le prend de
# NUEE_ARRET), et les longues boucles du moteur le regardent entre deux tours (verifier_arret) : les
# permutations, les modèles de la sélection par l'AIC, les départs d'un NMDS, les simulations de la
# puissance, les arbres boostés, les groupes des validations croisées... Le fichier là, le calcul
# revient aussitôt à la demande, par le redémarrage « arreter_calcul » que serveur.R lui tend
# (faire_demande) : sans passer par les tryCatch(error = …) des calculs, qui le prendraient pour l'échec
# d'un tour et continueraient, et sans rien garder de lui ; la demande rend « calcul arrêté », et la
# séance est remise telle qu'elle était avant elle. Le fichier est effacé au début de la demande
# suivante. Sans demande pour le recevoir (le moteur chargé dans R seul), une erreur de classe
# « calcul_arrete ». Le fichier n'est regardé que cinq fois par seconde au plus : un tour d'une
# milliseconde n'en paie presque rien.
# Où en est le calcul (1.2) : une boucle qui sait son tour et son nombre de tours les donne,
# verifier_arret(fait, total, quoi) (fait : le tour qui commence, de 1 à total ; quoi : ce qu'elle
# fait, un mot que l'interface traduit : "validation", "permutations"...), noter_progres.
verifier_arret <- local({
  dernier <- 0
  function(fait = NULL, total = NULL, quoi = NULL) {
    if (!is.null(fait)) noter_progres(fait, total, quoi, sys.nframe())
    f <- getOption("nuee.arret")
    if (!length(f) || !nzchar(f[1])) return(invisible(FALSE))
    maintenant <- proc.time()[[3]]
    if (maintenant - dernier < 0.2) return(invisible(FALSE))
    dernier <<- maintenant
    if (!file.exists(f[1])) return(invisible(FALSE))
    if (any(vapply(computeRestarts(), function(r) identical(r$name, "arreter_calcul"), TRUE))) invokeRestart("arreter_calcul")
    stop(structure(class = c("calcul_arrete", "error", "condition"), list(message = traduire("Calcul arrêté.", "Computation stopped."), call = NULL)))
  }
})

# Les niveaux du calcul en cours, du plus extérieur au plus intérieur : list(profondeur, fait, total,
# quoi). Une boucle dans une autre se reconnaît à sa profondeur dans la pile des appels (sys.nframe) :
# un tour d'une boucle oublie celles, plus profondes, qui ont fini avant lui. Dans le calculateur des
# calculs en arrière-plan (arriere_plan.R), où options(nuee.progression) est un fichier, les niveaux y
# sont écrits quatre fois par seconde au plus, avec le numéro de la tâche, pour le serveur qui le lit
# (demande progression).
PROGRES <- new.env(parent = emptyenv())
PROGRES$niveaux <- list()
PROGRES$ecrit <- 0
PROGRES$tache <- NA
noter_progres <- function(fait, total, quoi, profondeur) {
  n <- Filter(function(x) x$profondeur < profondeur, PROGRES$niveaux)
  n[[length(n) + 1L]] <- list(profondeur = profondeur, fait = as.numeric(fait)[1], total = as.numeric(total)[1], quoi = as.character(quoi %||% "")[1])
  PROGRES$niveaux <- n
  f <- getOption("nuee.progression")
  maintenant <- proc.time()[[3]]
  if (length(f) && nzchar(f[1]) && maintenant - PROGRES$ecrit >= 0.25) {
    PROGRES$ecrit <- maintenant
    tryCatch({
      saveRDS(structure(n, tache = PROGRES$tache), paste0(f[1], ".tmp"), compress = FALSE)
      file.rename(paste0(f[1], ".tmp"), f[1])
    }, error = function(e) NULL, warning = function(w) NULL)
  }
  invisible(NULL)
}
# Le calcul fait, de 0 à 1, des niveaux : le tour en cours de chacun compte pour ce qu'en a fait la
# boucle d'en dessous ; NULL sans niveau (un calcul qui ne compte pas ses tours).
fraction_progres <- function(niveaux) {
  if (!length(niveaux)) return(NULL)
  f <- 0
  for (x in rev(niveaux)) {
    if (!is.finite(x$total) || x$total < 1) return(NULL)
    f <- (min(max(x$fait, 1), x$total) - 1 + f) / x$total
  }
  f
}

# Les lignes d'une fonction du moteur écrite dans un script exporté vers R (script_moteur.R,
# communautes_script.R), sans ses appels à verifier_arret, avec ou sans arguments : le script
# s'arrête dans R par Échap.
sans_arret <- function(lignes) {
  appel <- "verifier_arret(\\((?:[^()]++|(?1))*\\))"
  lignes <- lignes[!grepl(sprintf("^\\s*%s\\s*$", appel), lignes, perl = TRUE)]
  gsub(sprintf("%s;\\s*", appel), "", lignes, perl = TRUE)
}
