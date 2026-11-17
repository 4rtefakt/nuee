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
verifier_arret <- local({
  dernier <- 0
  function() {
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

# Les lignes d'une fonction du moteur écrite dans un script exporté vers R (script_moteur.R,
# communautes_script.R), sans ses appels à verifier_arret : le script s'arrête dans R par Échap.
sans_arret <- function(lignes) {
  lignes <- lignes[!grepl("^\\s*verifier_arret\\(\\)\\s*$", lignes)]
  gsub("verifier_arret\\(\\);\\s*", "", lignes)
}
