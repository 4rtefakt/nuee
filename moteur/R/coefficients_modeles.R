# Les coefficients de plusieurs régressions côte à côte (Prédiction › Comparer les modèles ›
# Coefficients côte à côte), comme les articles et les rapports les présentent : une ligne par
# terme, une colonne par modèle, chaque case l'estimation avec son intervalle à 95 % ou son
# erreur-type ; dessous, n, AIC, AICc, R², l'écart-type de chaque effet aléatoire, φ de CAR1 et
# l'edf de chaque courbe. Le tableau est fait des résultats que chaque modèle a déjà (son tableau
# des coefficients, ses variances, ses courbes), sans rien supposer des noms de ses termes : un
# terme est une ligne de son tableau des coefficients, et deux modèles partagent une ligne quand
# ils ont un terme du même nom.

# Un nombre pour le tableau, à `chiffres` décimales, avec la marque décimale de la langue des
# messages (un tableau à coller dans un rapport) ; rien pour une valeur manquante, et 0 plutôt
# que -0,000.
nombre_tableau <- function(x, chiffres) {
  if (is.null(x) || !length(x) || is.na(x)) return("")
  formatC(if (round(x, chiffres) == 0) 0 else x, format = "f", digits = chiffres,
          decimal.mark = if (identical(getOption("nuee.langue"), "en")) "." else ",")
}

# Les décimales d'une estimation : celles de deux chiffres significatifs de son erreur-type, la
# précision que permet son incertitude (0,0132 (0,0041), 12,35 (0,41)) ; sans erreur-type, trois
# chiffres significatifs de l'estimation. De 0 à 8.
decimales_estimation <- function(estimation, erreur_type) {
  d <- if (isTRUE(is.finite(erreur_type) && erreur_type > 0)) 1 - floor(log10(erreur_type))
       else if (isTRUE(is.finite(estimation) && estimation != 0)) 2 - floor(log10(abs(estimation))) else 3
  as.integer(max(0, min(8, d)))
}

# Trois chiffres significatifs (un écart-type : 0,421, 12,3).
trois_chiffres <- function(x) {
  if (is.null(x) || !length(x) || !isTRUE(is.finite(x))) return("")
  nombre_tableau(x, as.integer(max(0, min(8, if (x != 0) 2 - floor(log10(abs(x))) else 3))))
}

# Les étoiles d'une valeur p, comme R les met (symnum de printCoefmat) : *** jusqu'à 0,001, **
# jusqu'à 0,01, * jusqu'à 0,05 ; rien au-delà ou sans p.
etoiles_p <- function(p) ifelse(is.na(p), "", ifelse(p <= 0.001, "***", ifelse(p <= 0.01, "**", ifelse(p <= 0.05, "*", ""))))

# Le tableau des coefficients de `modeles` (une liste nommée de régressions ajustées,
# ajuster_regression, dans l'ordre de leurs colonnes ; les noms sont ceux des colonnes). `erreur` :
# "ic", l'estimation [son intervalle à 95 %], ou "et", l'estimation (son erreur-type) ; l'intervalle
# et l'erreur-type sont ceux du tableau des coefficients du modèle (vue Coefficients). Les lignes :
# celles de leurs tableaux des coefficients, dans l'ordre où elles viennent (la constante et les
# termes du premier modèle, puis les termes des suivants qui n'y sont pas) ; une case vide quand le
# modèle n'a pas le terme. Puis les lignes de l'ajustement : n, AIC et AICc (de l'ajustement en
# ML), le R² (une régression linéaire), le R² marginal et conditionnel (un modèle mixte, ou un
# modèle de mgcv::gamm), la part de la déviance expliquée (un GLM ou un GAM), l'écart-type σ de
# chaque ligne de leurs tableaux des variances (chaque effet aléatoire, et la résiduelle d'un modèle
# de nlme), φ de CAR1 et l'edf de chaque courbe. Renvoie les cases en texte (`cellules`, dans la
# langue des messages), les p (`p`, NA sans terme), les lignes de l'ajustement
# (`statistiques`, `valeurs`) et, pour le presse-papiers et l'export, le même tableau avec les étoiles
# des p (`copie` : `entetes`, `lignes`, `notes`, la légende des étoiles et l'échelle du lien).
coefficients_cote_a_cote <- function(modeles, erreur = "ic") {
  if (!length(modeles)) stop(traduire("Cochez au moins une régression.", "Tick at least one regression."), call. = FALSE)
  if (!all(vapply(modeles, inherits, TRUE, "modele_regression"))) {
    stop(traduire("Seules des régressions (LM, LMM, GLM, GAM) se mettent côte à côte.", "Only regressions (LM, LMM, GLM, GAM) go side by side."), call. = FALSE)
  }
  erreur <- if (identical(as.character(erreur)[1], "et")) "et" else "ic"
  fr <- !identical(getOption("nuee.langue"), "en")
  noms <- names(modeles)
  tables <- lapply(modeles, function(m) m$resultat$coefficients)
  termes <- unique(unlist(lapply(tables, function(co) as.character(co$terme)), use.names = FALSE))
  K <- length(modeles)
  cellules <- matrix("", length(termes), K)
  p <- matrix(NA_real_, length(termes), K)
  for (j in seq_len(K)) {
    co <- tables[[j]]
    i <- match(as.character(co$terme), termes)
    p[i, j] <- co$p
    cellules[i, j] <- vapply(seq_len(nrow(co)), function(k) {
      d <- decimales_estimation(co$estimation[k], co$erreur_type[k])
      if (erreur == "et") sprintf("%s (%s)", nombre_tableau(co$estimation[k], d), nombre_tableau(co$erreur_type[k], d))
      else sprintf(if (fr) "%s [%s ; %s]" else "%s [%s, %s]", nombre_tableau(co$estimation[k], d), nombre_tableau(co$bas[k], d), nombre_tableau(co$haut[k], d))
    }, "")
  }

  # --- les lignes de l'ajustement : une par statistique qu'au moins un des modèles a
  lignes <- list()
  ajouter <- function(nom, valeurs) if (any(nzchar(valeurs))) lignes[[length(lignes) + 1]] <<- list(nom = nom, valeurs = valeurs)
  chaque <- function(f) vapply(modeles, function(m) as.character(f(m)), "")
  ajouter("n", chaque(function(m) format(m$N)))
  ajouter("AIC", chaque(function(m) nombre_tableau(m$resultat$AIC, 1)))
  ajouter("AICc", chaque(function(m) nombre_tableau(m$resultat$AICc, 1)))
  r2 <- function(cle) function(m) nombre_tableau(m$resultat$r2[[cle]], 3)
  ajouter("R²", chaque(function(m) if (is.null(m$resultat$r2$marginal) && is.null(m$resultat$r2$deviance_expliquee)) r2("r2")(m) else ""))
  ajouter(traduire("R² marginal", "Marginal R²"), chaque(r2("marginal")))
  ajouter(traduire("R² conditionnel", "Conditional R²"), chaque(r2("conditionnel")))
  ajouter(traduire("Déviance expliquée", "Deviance explained"), chaque(function(m) {
    v <- m$resultat$r2$deviance_expliquee
    if (is.null(m$resultat$r2$marginal) && !is.null(v)) paste0(nombre_tableau(100 * v, 1), if (fr) " %" else "%") else ""
  }))
  # (l'écart-type de chaque ligne des tableaux des variances, dans l'ordre où elles viennent ; une
  # pente aléatoire, à son niveau, avec son terme : « σ site (annee) », puis sa corrélation avec
  # l'ordonnée à l'origine de son niveau)
  variances <- lapply(modeles, variances_regression)
  cle <- function(v) {
    k <- as.character(v$composante)
    if (!is.null(v$terme)) {
      pente <- !is.na(v$terme) & v$terme != "(Intercept)"
      k[pente] <- sprintf("%s (%s)", k[pente], v$terme[pente])
    }
    k
  }
  composantes <- unique(unlist(lapply(variances, function(v) if (!is.null(v)) cle(v)), use.names = FALSE))
  for (k in composantes) ajouter(paste("σ", k), vapply(variances, function(v) {
    i <- if (is.null(v)) NA else match(k, cle(v))
    if (is.na(i)) "" else trois_chiffres(sqrt(v$variance[i]))
  }, ""))
  pentes <- unique(unlist(lapply(variances, function(v) if (!is.null(v$correlation)) cle(v)[!is.na(v$correlation)]), use.names = FALSE))
  for (k in pentes) ajouter(paste("ρ", k), vapply(variances, function(v) {
    i <- if (is.null(v$correlation)) NA else match(k, cle(v))
    if (is.na(i) || is.na(v$correlation[i])) "" else nombre_tableau(v$correlation[i], 3)
  }, ""))
  ajouter("φ (CAR1)", chaque(function(m) nombre_tableau(m$resultat$phi, 3)))
  # (une courbe par catégorie : une ligne par catégorie, « annee (secteur S1) »)
  etiquettes <- function(cb) if (is.null(cb$niveau)) as.character(cb$terme) else ifelse(is.na(cb$niveau), as.character(cb$terme), sprintf("%s (%s %s)", cb$terme, cb$par, cb$niveau))
  courbes <- unique(unlist(lapply(modeles, function(m) etiquettes(m$resultat$courbes)), use.names = FALSE))
  for (k in courbes) ajouter(paste("edf", k), chaque(function(m) {
    cb <- m$resultat$courbes
    i <- if (!is.null(cb)) match(k, etiquettes(cb)) else NA
    if (is.na(i)) "" else nombre_tableau(cb$edf[i], 2)
  }))
  statistiques <- vapply(lignes, `[[`, "", "nom")
  valeurs <- do.call(rbind, lapply(lignes, `[[`, "valeurs"))
  dimnames(valeurs) <- NULL

  # --- la copie : les cases avec les étoiles des p (les marques de la vue ne passent pas dans un
  # texte), la légende, et l'échelle du lien d'un modèle de lien log ou logit
  entete <- if (erreur == "et") traduire("Estimation (erreur-type)", "Estimate (SE)") else traduire("Estimation [IC 95 %]", "Estimate [95% CI]")
  avec_etoiles <- matrix(paste0(cellules, etoiles_p(p)), nrow(cellules))
  liens <- vapply(modeles, function(m) as.character(m$lien %||% "identity"), "")
  autres <- liens != "identity"
  note_lien <- if (any(autres)) {
    sens <- c(log = traduire("exp(estimation) est l'effet multiplicatif d'une unité de plus (log)", "exp(estimate) is the multiplicative effect of one more unit (log)"),
              logit = traduire("exp(estimation) est le rapport des cotes (logit)", "exp(estimate) is the odds ratio (logit)"))
    sprintf(traduire("%s : estimations sur l'échelle du lien, comme dans la vue Coefficients%s.", "%s: estimates on the link's scale, as in the Coefficients view%s."),
            paste(sprintf(traduire("%s (lien %s)", "%s (%s link)"), noms[autres], liens[autres]), collapse = ", "),
            if (any(liens %in% names(sens))) paste0(if (fr) " ; " else "; ", paste(sens[intersect(names(sens), liens)], collapse = if (fr) " ; " else "; ")) else "")
  }
  legende <- if (fr) "* p ≤ 0,05 ; ** p ≤ 0,01 ; *** p ≤ 0,001" else "* p ≤ 0.05; ** p ≤ 0.01; *** p ≤ 0.001"
  list(modeles = I(noms), erreur = erreur, entete = entete, termes = I(termes), cellules = cellules, p = p,
       statistiques = I(statistiques), valeurs = valeurs, liens = I(unname(liens)), note_lien = note_lien,
       copie = list(entetes = I(c(entete, noms)), lignes = unname(rbind(cbind(termes, avec_etoiles), cbind(statistiques, valeurs))),
                    notes = I(c(legende, note_lien))))
}

# Le même tableau en fichiers (Fichier › Exporter les résultats du modèle, depuis Comparer les
# modèles › Coefficients côte à côte ; formats.R : ecrire_tables) : tel que la vue le copie (les
# cases en texte avec les étoiles, puis la légende), et les coefficients de chaque modèle en
# nombres, une ligne par terme et par modèle.
tables_cote_a_cote <- function(modeles, erreur = "ic") {
  r <- coefficients_cote_a_cote(modeles, erreur)
  lignes <- r$copie$lignes
  notes <- matrix("", length(r$copie$notes) + 1, ncol(lignes))
  notes[-1, 1] <- r$copie$notes
  cote <- as.data.frame(rbind(lignes, notes), stringsAsFactors = FALSE)
  names(cote) <- make.unique(as.character(r$copie$entetes), sep = " ")
  nombres <- do.call(rbind, lapply(seq_along(modeles), function(j) {
    co <- modeles[[j]]$resultat$coefficients
    cbind(data.frame(modele = names(modeles)[j], stringsAsFactors = FALSE), co[c("terme", "estimation", "erreur_type", "ddl", "t", "p", "bas", "haut")])
  }))
  rownames(nombres) <- NULL
  list(cote_a_cote = cote, coefficients = nombres)
}
