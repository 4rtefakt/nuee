# Diagnostics : contributions (pourquoi une observation s'écarte) et vérification des
# données avant d'ajuster un modèle.

# Contributions des variables à l'écart d'une observation, ou de la moyenne d'un groupe
# d'observations :
#   type = "DModX" : résidus centrés-réduits de chaque terme, multipliés par le pouvoir de
#                    modélisation de la variable si ponderee (comme le DModX pondéré) ;
#   type = "score" : écart à l'observation moyenne (0 en données centrées), en unités
#                    centrées-réduites, multiplié par la valeur absolue du poids normé de la
#                    composante (w* en PLS, p en ACP) ;
#   type = "T2"    : idem avec le poids de toutes les composantes à la fois (racine de la somme
#                    des carrés des poids), pour une observation qui sort de l'ellipse.
contributions <- function(m, observations, type = c("DModX", "score", "T2"), composante = 1, ponderee = TRUE) {
  type <- match.arg(type)
  i <- match(as.numeric(observations), m$observations)
  i <- i[!is.na(i)]
  if (!length(i)) stop(traduire("Aucune des observations choisies n'est dans le modèle", "None of the chosen observations is in the model"))
  moyenne <- function(M) colSums((M * m$MX)[i, , drop = FALSE]) / pmax(colSums(m$MX[i, , drop = FALSE]), 1)
  if (type == "DModX") {
    v <- moyenne(m$residus_x)
    if (ponderee) v <- v * pouvoir_modelisation(m)
  } else {
    poids <- if (identical(m$type, "PLS")) coefficients_pls(m$W, m$P, m$C, ordre_retrait(m))$Wetoile else m$P
    poids <- if (type == "score") abs(poids[, composante]) else sqrt(rowSums(poids^2))
    v <- moyenne(m$X) * poids / sqrt(sum(poids^2))
  }
  data.frame(terme = m$x$noms, contribution = unname(v), stringsAsFactors = FALSE)
}

# Vérifications avant l'ajustement : variables ou observations
# avec plus de 50 % de valeurs manquantes, et variables sans variance (constantes).
# Renvoie une ligne par variable concernée (avec ses termes) et par observation concernée.
verifier_donnees <- function(projet, definition, limite = 0.5) {
  obs <- definition$observations
  bx <- construire_bloc(projet, definition$x, obs)
  by <- if (length(definition$y)) construire_bloc(projet, definition$y, obs, facteurs = bx$simples)
  termes <- c(definition$x, definition$y)
  X <- cbind(bx$X, by$X)
  manque <- colMeans(is.na(X))
  constant <- vapply(seq_len(ncol(X)), function(k) {
    v <- X[!is.na(X[, k]), k]
    length(v) < 2 || stats::sd(v) == 0
  }, TRUE)
  # par variable : le pire de ses termes simples (une variable qualitative en a plusieurs)
  simples <- which(vapply(termes, function(t) length(t$variables) == 1, TRUE))
  uid <- vapply(termes[simples], function(t) as.numeric(t$variables), 0)
  vars <- do.call(rbind, lapply(unique(uid), function(u) {
    k <- simples[uid == u]
    data.frame(uid = u, nom = unname(projet$noms_var[as.character(u)]), manquantes = max(manque[k]),
               constante = all(constant[k]))
  }))
  vars <- if (is.null(vars)) data.frame() else vars[vars$manquantes > limite | vars$constante, , drop = FALSE]
  manque_obs <- if (ncol(bx$X)) rowMeans(is.na(bx$X)) else rep(0, length(obs))  # (une régression sans X : le modèle nul)
  garde <- manque_obs > limite
  list(variables = vars,
       observations = data.frame(uid = obs[garde], nom = unname(projet$noms_obs[as.character(obs[garde])]),
                                 manquantes = manque_obs[garde]))
}

# Statistiques descriptives des variables : N, valeurs manquantes (%), min, max, Min/Max,
# moyenne, médiane, écart-type, écart-type/moyenne, asymétrie et son test, aplatissement.
# Pour une variable qualitative :
# N, valeurs manquantes et nombre de catégories.
# Asymétrie et aplatissement : estimateurs corrigés pour la taille de l'échantillon (G1, G2,
# ceux d'Excel) ; on ne sait pas lesquels ont donné les valeurs des fichiers .usp.
# Test d'asymétrie : asymétrie / erreur type, erreur type = racine(6n(n-1) / ((n-2)(n+1)(n+3)))
# (la formule publiée est imprimée sans la racine, ce qui serait la variance de l'asymétrie).
# Min/Max est signalé quand 0 <= Min/Max < 0,1 et l'asymétrie quand le test
# dépasse 2 en valeur absolue.
# Transformation proposée : log10(x + C2) quand
# l'asymétrie est significative vers la droite (test > 2) et que Min/Max < 0,1 (le min sans
# compter les 0), avec plus de 4 valeurs et plus de 10 % des observations ;
# C2 = 0 si toutes les valeurs sont positives, sinon la moitié de la plus petite valeur non
# nulle (en valeur absolue), plus cette valeur s'il y a des valeurs négatives. La règle
# publiée ne dit pas si les deux critères doivent être réunis : on les exige tous les deux, car une variable
# symétrique ou qui varie peu en proportion ne gagne rien au logarithme.
statistiques_variables <- function(projet, observations = NULL) {
  obs <- observations %||% vapply(projet$observations, function(o) as.numeric(o$uid), 0)
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(projet$donnees), projet$variables)
  lignes <- lapply(vars, function(v) {
    x <- colonne(projet, v$uid, obs)
    ok <- x[!is.na(x)]
    n <- length(ok)
    ligne <- data.frame(uid = as.numeric(v$uid), nom = as.character(v$ids[1]), qualitative = length(v$categories) > 0,
                        categories = length(v$categories), N = n, manquantes = 100 * mean(is.na(x)),
                        min = NA_real_, max = NA_real_, min_max = NA_real_, moyenne = NA_real_, mediane = NA_real_,
                        ecart_type = NA_real_, et_moyenne = NA_real_, asymetrie = NA_real_, test_asymetrie = NA_real_,
                        aplatissement = NA_real_, rouge_min_max = FALSE, rouge_asymetrie = FALSE,
                        suggestion = NA_character_, C2 = NA_real_, stringsAsFactors = FALSE)
    if (ligne$qualitative || !n) return(ligne)
    m <- mean(ok)
    s <- if (n > 1) stats::sd(ok) else NA_real_
    ligne[c("min", "max", "moyenne", "mediane", "ecart_type")] <- list(min(ok), max(ok), m, stats::median(ok), s)
    ligne$min_max <- if (max(ok) != 0) min(ok) / max(ok) else NA_real_
    ligne$et_moyenne <- if (m != 0) s / m else NA_real_
    if (n >= 3 && isTRUE(s > 0)) {
      m2 <- mean((ok - m)^2)
      g1 <- mean((ok - m)^3) / m2^1.5
      ligne$asymetrie <- g1 * sqrt(n * (n - 1)) / (n - 2)
      ligne$test_asymetrie <- ligne$asymetrie / sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
      if (n >= 4) {
        g2 <- mean((ok - m)^4) / m2^2 - 3
        ligne$aplatissement <- ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
      }
    }
    ligne$rouge_min_max <- isTRUE(ligne$min_max >= 0 && ligne$min_max < 0.1)
    ligne$rouge_asymetrie <- isTRUE(abs(ligne$test_asymetrie) > 2)
    non_nuls <- ok[ok != 0]
    min_max_auto <- if (length(non_nuls) && max(ok) != 0) min(non_nuls) / max(ok) else NA
    if (n > 4 && n > 0.1 * length(obs) && isTRUE(ligne$test_asymetrie > 2) && isTRUE(min_max_auto >= 0 && min_max_auto < 0.1)) {
      plus_petite <- abs(non_nuls[which.min(abs(non_nuls))])
      C2 <- if (all(ok > 0)) 0 else plus_petite / 2
      if (any(ok < 0)) C2 <- C2 + plus_petite
      ligne$suggestion <- if (C2 == 0) "log10" else sprintf("log10:%s", format(signif(C2, 6)))
      ligne$C2 <- C2
    }
    ligne
  })
  do.call(rbind, lignes)
}
