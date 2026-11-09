# Outils de validation des modèles PLS : test de permutation, CV-ANOVA, et pour les PLS-DA
# la table de classification et les courbes ROC.

# Test de permutation. Les valeurs de la variable du Y numéro y (pour une PLS-DA, les classes :
# toutes les indicatrices ensemble) sont permutées au hasard entre les observations du modèle, n
# fois ; chaque fois tout le modèle est réajusté à partir du projet (mise à l'échelle et
# validation croisée comprises), avec le même nombre de composantes. Pour chaque modèle : la
# corrélation entre le Y d'origine et le Y permuté, et le R2 et le Q2 cumulés de ce Y. Les
# droites de régression de R2 et de Q2 en fonction de la corrélation coupent l'axe en 0 : ces
# ordonnées à l'origine mesurent le surajustement (règle usuelle : Q2 < 0,05 et R2 < 0,3 à 0,4
# pour un modèle valide). `m` : le modèle ajusté de `projet` d'après `definition`.
permutations <- function(projet, m, definition, y = 1, n = 20, graine = 1) {
  A <- m$A
  d <- definition
  d$test <- NULL  # (le jeu de test ne change ni R2 ni Q2)
  mesure <- function(r) c(R2 = 1 - unname(r$ssyv[y, A + 1] / r$ssyv[y, 1]), Q2 = 1 - unname(r$press[A, y] / r$ss_vc[1, y]))
  lignes_y <- match(as.character(m$observations), rownames(projet$donnees))
  # (une PLS-DA d'un fichier .usp a une variable 0/1 par classe, celle de l'application une variable de classe)
  variables <- unique(vapply(if (m$plsda) d$y else d$y[y], function(t) as.character(t$variables[1]), ""))
  valeurs <- projet$donnees[lignes_y, variables, drop = FALSE]
  present <- m$MY[, y] == 1
  set.seed(graine)
  lignes <- list(c(correlation = 1, mesure(m)))
  for (i in seq_len(n)) {
    ordre <- sample(length(lignes_y))
    pp <- projet
    pp$donnees[lignes_y, variables] <- valeurs[ordre, , drop = FALSE]
    r <- calculer_modele(pp, d, A, m$G)
    deux <- present & r$MY[, y] == 1
    corr <- suppressWarnings(stats::cor(m$Y[deux, y], r$Y[deux, y]))
    lignes[[i + 1]] <- c(correlation = abs(corr), mesure(r))
  }
  tab <- as.data.frame(do.call(rbind, lignes))
  tab$origine <- c(TRUE, rep(FALSE, n))
  droite <- function(v) unname(stats::coef(stats::lm(v ~ tab$correlation)))
  r2 <- droite(tab$R2)
  q2 <- droite(tab$Q2)
  list(y = m$y$noms[y], n = n, points = tab,
       ordonnee_R2 = r2[1], pente_R2 = r2[2], ordonnee_Q2 = q2[1], pente_Q2 = q2[2])
}

# CV-ANOVA (Eriksson, Trygg et Wold 2008) : compare les résidus de validation
# croisée du modèle (Y prédit par les sous-modèles de la validation croisée) à la variation autour
# de la moyenne, par une ANOVA. Un tableau par variable Y :
#   Total corr. : SS = somme (y - moyenne)^2, DF = N - 1
#   Régression  : SS = SS total - SS résidus, DF = 2A (convention de l'article : chaque
#                 composante coûte deux degrés de liberté)
#   Résidus     : SS = somme des résidus de validation croisée^2, DF = N - 1 - 2A
#   F = MS régression / MS résidus, p = P(F(DF régression, DF résidus) > F)
cv_anova <- function(m, prediction_vc) {
  lapply(seq_len(ncol(m$Y)), function(j) {
    ok <- m$MY[, j] == 1 & m$avec_x
    y <- m$Y[ok, j]
    ss_total <- sum((y - mean(y))^2)
    ss_res <- sum((y - prediction_vc[ok, j])^2)
    df_total <- sum(ok) - 1
    df_reg <- 2 * m$A
    df_res <- df_total - df_reg
    ms <- c(ss_total / df_total, (ss_total - ss_res) / df_reg, ss_res / df_res)
    f <- ms[2] / ms[3]
    data.frame(source = c("Total corr.", "Regression", "Residual"),
               SS = c(ss_total, ss_total - ss_res, ss_res), DF = c(df_total, df_reg, df_res),
               MS = ms, F = c(NA, f, NA),
               p = c(NA, if (df_res > 0) stats::pf(f, df_reg, df_res, lower.tail = FALSE) else NA, NA),
               SD = sqrt(pmax(ms, 0)))
  })
}

# PLS-DA : classe observée de chaque observation et classe prédite (la plus grande valeur de
# Y prédite, sans seuil), avec le Y ajusté par le
# modèle et avec le Y prédit en validation croisée (plus honnête : chaque observation y est
# prédite par un modèle qui ne l'a pas vue). Table de classification et courbes ROC.
classification_plsda <- function(observe, ajuste, predit_vc, classes) {
  la_plus_grande <- function(M) apply(M, 1, function(v) if (all(is.na(v))) NA else classes[which.max(v)])
  vraie <- la_plus_grande(observe)
  calcul <- function(Yp) {
    predite <- la_plus_grande(Yp)
    ok <- !is.na(vraie) & !is.na(predite)
    table <- table(factor(vraie[ok], classes), factor(predite[ok], classes))
    list(predite = predite, table = unclass(table), classes = classes,
         correct = diag(table) / pmax(rowSums(table), 1), global = sum(diag(table)) / max(sum(table), 1),
         roc = lapply(seq_along(classes), function(j) {
           garde <- !is.na(vraie) & !is.na(Yp[, j])
           roc(Yp[garde, j], vraie[garde] == classes[j])
         }))
  }
  list(ajuste = calcul(ajuste), vc = calcul(predit_vc))
}

# Courbe ROC : taux de vrais positifs en fonction du taux de faux positifs
# quand le seuil sur la valeur prédite parcourt toutes les valeurs ; aire sous la courbe
# (AUC) par la méthode des trapèzes : 0,5 = pas mieux que le hasard, 1 = parfait.
roc <- function(score, positif) {
  if (!any(positif) || all(positif)) return(list(fpr = c(0, 1), tpr = c(0, 1), auc = NA))
  seuils <- sort(unique(score), decreasing = TRUE)
  tpr <- c(0, vapply(seuils, function(s) mean(score[positif] >= s), 0))
  fpr <- c(0, vapply(seuils, function(s) mean(score[!positif] >= s), 0))
  list(fpr = fpr, tpr = tpr, auc = sum(diff(fpr) * (utils::head(tpr, -1) + utils::tail(tpr, -1)) / 2))
}
