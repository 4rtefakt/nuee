# Les calculs longs partagés entre plusieurs R : dans le navigateur, chaque R (webR) n'a qu'un
# cœur, et l'interface en lance d'autres (app/interface/moteur_webr.js). Une demande se fait
# alors en trois temps, avec les mêmes fonctions qu'en un seul R, donc les mêmes résultats :
#   - travail_preparer() (le R de l'interface) : ce qu'il faut pour les morceaux, dans un fichier ;
#   - travail_morceau() (chaque autre R, qui a le moteur sans le serveur) : des morceaux du calcul,
#     chacun dans son fichier ;
#   - travail_finir() (le R de l'interface) : les morceaux mis ensemble, la réponse de la demande.
# TRAVAUX (serveur.R) dit, pour chaque demande qui peut se partager, comment.

travail_morceau <- function(fichier, debut, fin, sortie) {
  x <- readRDS(fichier)
  saveRDS(get(x$morceau, mode = "function")(x$taches, seq.int(debut, fin)), sortie, compress = FALSE)
  invisible(fin - debut + 1L)
}
