# Classification hiérarchique (HCA) des observations sur leurs scores t1..td : méthode de
# Ward sur les distances euclidiennes entre scores.
#
# La hauteur de fusion affichée est l'augmentation de la somme des carrés intra-groupes,
# n_a * n_b / (n_a + n_b) * |c_a - c_b|^2 ; avec hclust(method = "ward.D2") c'est
# hauteur^2 / 2. La somme de ces hauteurs vaut la somme des carrés totale des scores.

classification <- function(m, composantes = m$A) {
  T <- m$T[, seq_len(composantes), drop = FALSE]
  garde <- stats::complete.cases(T)
  arbre <- stats::hclust(stats::dist(T[garde, , drop = FALSE]), method = "ward.D2")
  arbre$labels <- as.character(m$observations[garde])
  arbre$hauteur_ward <- arbre$height^2 / 2
  arbre$composantes <- composantes
  arbre
}

# Numéro de groupe (1 = le plus grand) de chaque observation quand on coupe l'arbre en k groupes.
couper <- function(arbre, k) {
  g <- stats::cutree(arbre, k)
  ordre <- order(-tabulate(g, k), seq_len(k))
  stats::setNames(match(g, ordre), arbre$labels)
}
