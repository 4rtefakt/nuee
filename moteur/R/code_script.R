# Exporter vers R un modèle écrit en code R (code.R) : un script (ou un rapport R Markdown, Quarto)
# qui le refait comme Nuée l'a ajusté, par l'écrivain des autres analyses (script_moteur.R :
# ecrire_script_moteur). 1 : les paquets que le code utilise et leur version (en commentaire : le code
# les charge lui-même) ; 2 : les données que le code voit (donnees_code : les observations du modèle,
# ses variables et ses identifiants sous leurs noms, les qualitatives en facteurs dans l'ordre du
# projet), dans le script ou dans un CSV à côté, comme les autres scripts ; 3 : le code tel qu'il est
# écrit dans Nuée, puis le nom que le script donne à sa dernière valeur ; 4 : ce que Nuée en montre
# (le résumé, les coefficients, la vraisemblance, l'AIC et le BIC, ses valeurs et ses tableaux) et les
# deux graphiques des résidus. Avant d'être écrit, le script est exécuté ici (ce qui exécute le code :
# il doit être approuvé, exiger_code_permis), ses graphiques sur un périphérique sans sortie, et ses
# résultats comparés à ceux du modèle (ecart_code).

# Le code tel qu'il est écrit, puis `nom` pour sa dernière valeur : une ligne `nom <- s` quand sa
# dernière expression est un nom ou l'affecte (fit, fit <- lm(...)), sinon `nom <- ` écrit devant elle,
# à la place où elle commence (list(modele = fit, valeurs = ...)).
code_resultat_script <- function(code, nom) {
  code <- texte_code(code)
  lignes <- strsplit(code, "\n", fixed = TRUE)[[1]]
  exprs <- parse(text = code, keep.source = TRUE)
  der <- exprs[[length(exprs)]]
  cible <- if (is.name(der)) as.character(der)
           else if (is.call(der) && is.name(der[[1]]) && as.character(der[[1]]) %in% c("<-", "=", "<<-") && is.name(der[[2]])) as.character(der[[2]])
  if (!is.null(cible)) return(c(lignes, sprintf("%s <- %s", nom, deparse(as.name(cible), backtick = TRUE))))
  ref <- attr(exprs, "srcref")[[length(exprs)]]
  octets <- charToRaw(lignes[ref[1]])
  lignes[ref[1]] <- paste0(rawToChar(octets[seq_len(ref[2] - 1)]), nom, " <- ", rawToChar(octets[ref[2]:length(octets)]))
  lignes
}

# La partie 4 : ce que Nuée montre du modèle, chaque ligne seulement quand Nuée l'a pu (le même appel).
code_sortie_code <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  mod <- tx("modele", "model")
  s <- m$statistiques
  stats <- c(if (!is.null(s$nobs)) sprintf("n = nobs(%s)", mod), if (!is.null(s$logLik)) sprintf("logLik = as.numeric(logLik(%s))", mod),
             if (!is.null(s$ddl)) sprintf("df = attr(logLik(%s), \"df\")", mod), if (!is.null(s$AIC)) sprintf("AIC = AIC(%s)", mod), if (!is.null(s$BIC)) sprintf("BIC = BIC(%s)", mod))
  source <- attr(m$coefficients, "source")
  c(if (length(m$resume)) ligne_sortie(sprintf("print(summary(%s))", mod), tx("le résumé, comme Nuée le montre", "the summary, as Nuée shows it")),
    if (identical(source, "summary")) ligne_sortie(sprintf("print(coef(summary(%s)))", mod), tx("les coefficients", "the coefficients")),
    if (identical(source, "vcov")) ligne_sortie(sprintf("print(cbind(%s = coef(%s), %s = sqrt(diag(vcov(%s)))))", tx("estimation", "estimate"), mod, tx("erreur_type", "se"), mod),
                                                tx("les coefficients et leurs erreurs-types", "the coefficients and their standard errors")),
    if (length(stats)) ligne_sortie(sprintf("print(c(%s))", paste(stats, collapse = ", ")), tx("observations, vraisemblance, AIC, BIC", "observations, likelihood, AIC, BIC")),
    if (length(m$valeurs)) ligne_sortie(sprintf("print(%s)", tx("valeurs", "values")), tx("les valeurs du code", "the code's values")),
    if (length(m$tableaux)) ligne_sortie(sprintf("print(%s)", tx("tableaux", "tables")), tx("les tableaux du code", "the code's tables")))
}

# Les deux graphiques des résidus, en R de base, quand le modèle a des valeurs ajustées et des résidus.
code_graphiques_code <- function(m, langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  if (!length(m$ajustees)) return(list())
  mod <- tx("modele", "model")
  list(
    list(nom = "residus", largeur = 7, hauteur = 5, code = c(
      sprintf("plot(fitted(%s), residuals(%s), xlab = %s, ylab = %s, main = %s)", mod, mod, code_texte(tx("Valeurs ajustées", "Fitted values")),
              code_texte(tx("Résidus", "Residuals")), code_texte(tx("Résidus contre valeurs ajustées", "Residuals against fitted values"))),
      "abline(h = 0, lty = 3)")),
    list(nom = "qq", largeur = 6, hauteur = 6, code = c(
      sprintf("qqnorm(residuals(%s), xlab = %s, ylab = %s, main = %s)", mod, code_texte(tx("Quantiles de la loi normale", "Normal quantiles")),
              code_texte(tx("Résidus", "Residuals")), code_texte(tx("Droite de Henry des résidus", "Normal QQ plot of the residuals"))),
      sprintf("qqline(residuals(%s))", mod))))
}

# Les résultats du script (dans `e`) comparés à ceux de `m` : NULL, ou ce qui diffère.
ecart_code <- function(m, e, langue) {
  r <- get0(if (identical(langue, "en")) "result" else "resultat", envir = e, inherits = FALSE)
  fit <- if (isTRUE(m$liste)) r[["modele"]] else r
  nombres <- function(t) if (!is.null(t)) as.matrix(t[c("estimation", "erreur_type", "statistique", "p")])
  paires <- list(coefficients = list(nombres(m$coefficients), nombres(coefficients_code(fit))),
                 statistiques = list(unlist(m$statistiques), unlist(statistiques_code(fit))),
                 valeurs = if (length(m$valeurs)) list(m$valeurs, tryCatch(valeurs_code(r[["valeurs"]]), error = function(x) NULL)))
  ecarts <- vapply(Filter(Negate(is.null), paires), function(p) ecart_valeurs(p[[1]], p[[2]]), 0)
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

ecrire_script_code <- function(projet, d, chemin_script, m, donnees = "script", renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  exiger_code_permis(m$code, d$name)  # (le script est exécuté ici avant d'être écrit : son code aussi)
  df <- donnees_code(projet, d, m$recues)
  decrites <- Filter(function(v) as.character(v$ids[1] %||% "") %in% names(df) && (nzchar(v$unite %||% "") || nzchar(v$description %||% "")), projet$variables)
  tab <- list(donnees = df, descriptions = lapply(decrites, function(v) list(nom = as.character(v$ids[1]), unite = as.character(v$unite %||% ""), description = as.character(v$description %||% ""))))
  res <- tx("resultat", "result")
  mod <- tx("modele", "model")
  pq <- m$paquets
  versions <- if (nrow(pq)) paste(sprintf("%s %s", pq$nom, pq$version), collapse = ", ")
  fonctions <- c(commentaire(if (length(versions)) tx(sprintf("Aucune : le modèle est le code R plus bas, qui charge lui-même ses paquets. Nuée l'a ajusté avec R %s et %s.", m$R, versions),
                                                      sprintf("None: the model is the R code below, which loads its own packages. Nuée fitted it with R %s and %s.", m$R, versions))
                             else tx(sprintf("Aucune : le modèle est le code R plus bas, que Nuée a ajusté avec R %s seul.", m$R), sprintf("None: the model is the R code below, which Nuée fitted with R %s alone.", m$R))), "")
  # (les noms que le code reconnaît dans sa liste restent les siens : modele, valeurs, tableaux)
  extraire <- if (isTRUE(m$liste)) c(sprintf("%s <- %s$modele", mod, res), sprintf("%s <- %s$valeurs", tx("valeurs", "values"), res), sprintf("%s <- %s$tableaux", tx("tableaux", "tables"), res))
              else sprintf("%s <- %s", mod, res)
  calcul <- c(commentaire(tx(sprintf("Le code R du modèle, tel qu'il est écrit dans Nuée : il voit `donnees`, les données du modèle (%s) ; sa dernière valeur prend le nom `%s`.", observations_texte(m$n_donnees, d, TRUE), res),
                             sprintf("The model's R code, as written in Nuée: it sees `donnees`, the model's data (%s); its last value is named `%s`.", observations_texte(m$n_donnees, d, FALSE), res))),
              if (identical(langue, "en")) c(commentaire("(the code calls the data `donnees`, as in Nuée)"), sprintf("donnees <- %s", nom_script("donnees", langue))),
              code_resultat_script(m$code, res), extraire)
  piece <- list(
    quoi = sprintf(tx("le code R de ses auteurs (un modèle de classe %s)", "its authors' own R code (a model of class %s)"), m$classe[1] %||% "—"),
    tab = tab, fonctions = fonctions, calcul = calcul, sortie = code_sortie_code(m, langue), graphiques = code_graphiques_code(m, langue),
    ecart = function(m, e) ecart_code(m, e, langue), paquets = pq$nom,
    texte_paquets = tx("Il refait ce modèle avec le code R de ses auteurs, tel qu'il est écrit dans Nuée, sur les mêmes données, puis montre ce que Nuée en montre. Ses quatre parties :",
                       "It redoes this model with its authors' own R code, as written in Nuée, on the same data, then shows what Nuée shows of it. Its four parts:"),
    parties = c(tx("#   1. Fonctions : aucune (les paquets du code, en commentaire)", "#   1. Functions: none (the code's packages, in a comment)"),
                tx("#   3. Modèle : le code R, tel qu'il est écrit dans Nuée", "#   3. Model: the R code, as written in Nuée")),
    resultats = Filter(Negate(is.null), list(
      if (identical(attr(m$coefficients, "source"), "summary")) list(nom = "coefficients-table", objet = sprintf("coef(summary(%s))", mod), chiffres = 4),
      if (length(m$valeurs)) list(nom = "valeurs-table", objet = sprintf("t(%s)", tx("valeurs", "values")), chiffres = 4))),
    objets = c(res, mod, if (isTRUE(m$liste)) c(tx("valeurs", "values"), tx("tableaux", "tables"))))
  # (pendant cette exécution, les graphiques que le code dessine vont sur un périphérique sans sortie,
  # pas dans un Rplots.pdf du dossier du moteur ; ce qu'il affiche et ses messages, nulle part)
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  on.exit(if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev), add = TRUE)
  r <- NULL
  utils::capture.output(r <- suppressMessages(ecrire_script_moteur(projet, d, chemin_script, m, piece, donnees, renv, rdata, rproj, version, langue)))
  r
}
