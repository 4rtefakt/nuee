# Validation croisée.
#
# Les observations sont réparties à tour de rôle dans G groupes (7 par défaut) :
# la 1re, la 8e, la 15e... dans le groupe 1, la 2e, la 9e... dans le groupe 2, etc.
# Pour chaque composante, chaque groupe est retiré à son tour, la composante est recalculée
# sur les autres observations (à partir des résidus du modèle complet), puis les valeurs
# du groupe retiré sont prédites. PRESS = somme des carrés des erreurs de prédiction ;
# Q2 = 1 - PRESS / SS, SS étant la somme des carrés des résidus avant cette composante.

groupes_validation <- function(n, G = 7) ((seq_len(n) - 1) %% G) + 1

# PLS : retire des lignes, prédit Y. Renvoie PRESS par variable Y, SS par variable Y,
# les prédictions des observations retirées (pour cette composante) et les tours.
validation_pls <- function(E, ME, F, MF, groupes) {
  press <- numeric(ncol(F))
  prediction <- matrix(0, nrow(F), ncol(F))
  scores <- numeric(nrow(F))
  tours <- list()
  for (g in sort(unique(groupes))) {
    hors <- groupes == g
    dans <- !hors
    comp <- composante_pls(E[dans, , drop = FALSE], ME[dans, , drop = FALSE],
                           F[dans, , drop = FALSE], MF[dans, , drop = FALSE])
    t_hors <- projeter(E[hors, , drop = FALSE], ME[hors, , drop = FALSE], comp$w)
    pred <- outer(t_hors, comp$c)
    press <- press + colSums(((F[hors, , drop = FALSE] - pred) * MF[hors, , drop = FALSE])^2)
    prediction[hors, ] <- pred
    scores[hors] <- t_hors
    tours[[length(tours) + 1]] <- comp[c("w", "c", "p")]
  }
  list(press = press, ss = colSums(F^2), prediction = prediction, scores = scores, tours = tours)
}

# ACP : chaque case (i, k) est prédite par t_i * p_k, avec p estimé sans le groupe
# d'observations de i et t estimé sans le groupe de variables de k (approche de Krzanowski,
# Krzanowski 1987). Les Q2 enregistrés dans les fichiers .usp viennent d'une étape de plus,
# non publiée : sur les modèles ACP des projets d'essai, notre Q2 s'en écarte de 0,01 en
# médiane. Parmi les méthodes publiées essayées, c'est la plus proche.
validation_acp <- function(E, ME, groupes, groupes_var) {
  complet <- composante_acp(E, ME)
  P <- matrix(0, ncol(E), nrow(E))   # p utilisé pour chaque observation, selon son groupe
  Tc <- matrix(0, nrow(E), ncol(E))  # t utilisé pour chaque variable, selon son groupe
  tours <- list()
  for (g in sort(unique(groupes))) {
    hors <- groupes == g
    comp <- composante_acp(E[!hors, , drop = FALSE], ME[!hors, , drop = FALSE])
    p <- comp$p * sign(sum(comp$p * complet$p))
    P[, hors] <- p
    tours[[length(tours) + 1]] <- list(p = p)
  }
  for (h in sort(unique(groupes_var))) {
    hors <- groupes_var == h
    if (all(hors)) next
    comp <- composante_acp(E[, !hors, drop = FALSE], ME[, !hors, drop = FALSE])
    Tc[, hors] <- comp$t * sign(sum(comp$t * complet$t))
  }
  pred <- Tc * t(P)
  press <- colSums(((E - pred) * ME)^2)
  list(press = press, ss = colSums(E^2), tours = tours)
}
