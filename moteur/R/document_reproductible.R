# Un document compilé pour Quarto, reproductible (interface/documents.js : Compiler › Quarto ›
# Reproductible). À côté du .qmd, dans le dossier « <nom>_R » : l'export vers R de chaque modèle
# que le document cite (Fichier › Exporter vers R : script.R et les *_script.R ; ses données dans
# un CSV à côté de lui), et nuee.R, qui refait ces modèles, chacun dans son propre environnement, et
# écrit leurs nombres comme Nuée (app/interface/references.js : nombre, pourcentage, valeurP).
# Le .qmd commence par un bloc qui les refait ; chaque valeur citée y devient une expression R en
# ligne qui lit la valeur recalculée, un graphique un bloc qui le dessine avec le code du script
# (en R de base), un tableau un bloc knitr::kable. Ce qui n'a pas son équivalent dans le script (un
# graphique qu'il ne dessine pas, le texte des méthodes, un modèle sans export vers R...) reste écrit
# en dur, comme Nuée le montre, et le bloc du début le dit.
# Avant d'être donnée, chaque expression est évaluée ici sur les scripts écrits (relancés comme le
# .qmd les lance) et comparée au texte que Nuée en donne : une différence la laisse en dur.

# Pourquoi le modèle `d` ne s'exporte pas vers R : NULL quand il s'exporte (ACP, PLS, OPLS et
# régressions : script.R ; une autre analyse : son ecrire_script_<type>, analyses.R). `tx` : la
# langue du message (celle de l'interface, ou celle d'un document).
raison_sans_export <- function(d, tx = traduire) {
  if (!est_autre_analyse(d) || !is.null(fonction_analyse("ecrire_script_", d$type))) return(NULL)
  tx("Pas encore d'export vers R pour cette analyse.", "No export to R for this analysis yet.")
}

# La famille de script d'un modèle : ce qui dit comment ses objets se nomment.
famille_reproductible <- function(d) {
  if (est_regression(d)) return("regression")
  if (!est_autre_analyse(d)) return("composantes")
  if (isTRUE(d$type %in% c("PCOA", "NMDS", "PERMANOVA", "MANTEL", "INDVAL"))) return("communaute")
  "autre"
}

# Les graphiques d'un document (la vue de Nuée) que dessine le script, par le nom qu'il leur donne
# (ses graphiques : l'attribut « graphiques » de ce que rend son ecrire_script). Les autres restent
# l'image de Nuée.
GRAPHIQUES_REPRODUCTIBLES <- list(
  composantes = c(scores = "scores", loadings = "loadings", vip = "vip", obspred = "observe-ajuste", t2 = "t2-dmodx", dmodx = "t2-dmodx"),
  regression = c(obspred = "observe-ajuste", residus = "residus", puissance = "puissance", moyennes = "moyennes-ajustees", effets = "effet"),
  PCOA = c(ordination_dist = "ordination", valeurs_propres_pcoa = "valeurs-propres"),
  NMDS = c(ordination_dist = "ordination", shepard = "shepard"),
  PERMANOVA = c(permanova = "permanova", dispersion = "dispersion"),
  MANTEL = c(mantel = "mantel", correlogramme = "correlogramme"),
  INDVAL = c(indval_graphe = "valeurs-indicatrices"),
  RDA = c(ordination = "ordination", valeurs_propres = "valeurs-propres"),
  CCA = c(ordination = "ordination", valeurs_propres = "valeurs-propres"),
  CA = c(ordination = "ordination", valeurs_propres = "valeurs-propres"),
  TREE = c(arbre = "arbre", elagage = "elagage"),
  LDA = c(lda_scores = "scores"),
  KMEANS = c(km_silhouette = "silhouette", km_choix = "choix-k"),
  TRIM = c(trim_indices = "indices", trim_totaux = "totaux"),
  FOREST = c(ens_importance = "importance", ens_erreur = "erreur-oob", ens_obspred = "observe-predit"),
  BOOST = c(ens_importance = "influence", ens_erreur = "deviance", ens_obspred = "observe-predit"))

# Les tableaux d'un document (ceux du rapport de modèle de Nuée : rapport.js) que le script donne,
# en knitr::kable de ses objets (noms français, traduits pour un script en anglais).
TABLEAUX_REPRODUCTIBLES <- list(
  composantes = c(resume = "knitr::kable(composantes, digits = 3)", classification = "knitr::kable(classification)",
                  liste_vars = "knitr::kable(variables, digits = 3)"),
  regression = c(resume = "knitr::kable(t(c(r2, Q2 = vc$Q2, RMSEcv = vc$RMSEcv, criteres)), digits = 3)",
                 coefficients = "knitr::kable(coefficients, digits = 4)", moyennes = "knitr::kable(moyennes_ajustees$moyennes, digits = 4)",
                 selection_aic = "knitr::kable(head(selection$modeles, 20), digits = 3)", puissance = "knitr::kable(annees_80)",
                 seuils = "knitr::kable(plages_seuils, digits = 3)"),
  PCOA = c(resume = "knitr::kable(head(valeurs_propres, 5), digits = 4)", valeurs_propres_pcoa = "knitr::kable(head(valeurs_propres, 10), digits = 4)"),
  NMDS = c(resume = "knitr::kable(data.frame(stress = nmds$stress, meilleur = nmds$meilleur, retrouvee = nmds$retrouvee), digits = 4)"),
  PERMANOVA = c(resume = "knitr::kable(permanova$table, digits = 3)", permanova = "knitr::kable(permanova$table, digits = 3)"),
  MANTEL = c(resume = "knitr::kable(t(unlist(mantel)), digits = 4)", mantel = "knitr::kable(t(unlist(mantel)), digits = 4)",
             correlogramme = "knitr::kable(correlogramme$table, digits = 3)"),
  INDVAL = c(resume = "knitr::kable(especes, digits = 3)", indicatrices = "knitr::kable(especes, digits = 3)"))

# nuee.R : ce que le .qmd source avant de refaire les modèles, dans la langue du document.
code_aide_reproductible <- function(langue) {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr
  c(commentaire(tx("Pour le document Quarto d'à côté, écrit par Nuée : refaire les modèles qu'il cite, chacun par son export vers R, et écrire leurs nombres comme Nuée les écrit dans ses documents.",
                   "For the Quarto document beside this folder, written by Nuée: redo the models it cites, each by its export to R, and write their numbers as Nuée writes them in its documents.")),
    sprintf("nuee_langue <- %s", code_texte(langue)),
    "",
    commentaire(tx("Refait un modèle : son script d'export vers R, lancé depuis son dossier (il y lit ses données) dans un environnement à lui ; ses tableaux affichés et ses graphiques sont laissés de côté. Rend cet environnement, où sont ses résultats : M1$composantes...",
                   "Redoes a model: its export-to-R script, run from its folder (it reads its data there) in an environment of its own; the tables it prints and its plots are left aside. Returns that environment, which holds its results: M1$components...")),
    "nuee_refaire <- function(script) {",
    "  e <- new.env(parent = globalenv())",
    "  grDevices::pdf(NULL)",
    "  on.exit(grDevices::dev.off(), add = TRUE)",
    "  utils::capture.output(source(script, local = e, chdir = TRUE, encoding = \"UTF-8\"))",
    "  e",
    "}",
    "",
    commentaire(tx("Le code d'un graphique ou d'un tableau, lancé parmi les objets d'un modèle refait, sans les changer.",
                   "The code of a plot or a table, run among the objects of a redone model, without changing them.")),
    "nuee_dans <- function(modele, code) eval(substitute(code), new.env(parent = modele))",
    "",
    commentaire(tx("|x| arrondi à d décimales, comme dans Nuée : son écriture décimale la plus courte qui le redonne (15 à 17 chiffres), arrondie au plus loin de zéro à mi-chemin. Rend le texte, avec un point décimal.",
                   "|x| rounded to d decimals, as in Nuée: its shortest decimal writing that gives it back (15 to 17 digits), rounded half away from zero. Returns the text, with a decimal point.")),
    "nuee_arrondi <- function(x, d) {",
    "  x <- abs(x)",
    "  for (k in 15:17) {",
    "    s <- sprintf(\"%.*e\", k - 1L, x)",
    "    if (as.numeric(s) == x) break",
    "  }",
    "  m <- regmatches(s, regexec(\"^([0-9])[.]?([0-9]*)e([-+][0-9]+)$\", s))[[1]]",
    "  chiffres <- sub(\"0+$\", \"\", paste0(m[2], m[3]))",
    "  if (!nzchar(chiffres)) chiffres <- \"0\"",
    tx("  avant <- as.integer(m[4]) + 1L  # les chiffres avant la virgule", "  avant <- as.integer(m[4]) + 1L  # the digits before the decimal point"),
    "  if (avant < 1) {",
    "    chiffres <- paste0(strrep(\"0\", 1 - avant), chiffres)",
    "    avant <- 1L",
    "  }",
    "  chiffres <- paste0(chiffres, strrep(\"0\", max(0, avant + d + 1 - nchar(chiffres))))",
    "  v <- as.integer(strsplit(substr(chiffres, 1, avant + d), \"\")[[1]])",
    "  if (as.integer(substr(chiffres, avant + d + 1, avant + d + 1)) >= 5) {",
    "    i <- length(v)",
    "    while (i >= 1 && v[i] == 9) {",
    "      v[i] <- 0L",
    "      i <- i - 1L",
    "    }",
    "    if (i >= 1) v[i] <- v[i] + 1L else {",
    "      v <- c(1L, v)",
    "      avant <- avant + 1L",
    "    }",
    "  }",
    "  entier <- sub(\"^0+(?=[0-9])\", \"\", paste(v[seq_len(avant)], collapse = \"\"), perl = TRUE)",
    "  if (d > 0) paste0(entier, \".\", paste(v[avant + seq_len(d)], collapse = \"\")) else entier",
    "}",
    "",
    commentaire(tx("Un nombre : d décimales (ou celles de `significatifs` chiffres significatifs, 6 au plus), la virgule en français, les milliers séparés à partir de 10 000, le signe moins typographique ; — quand il manque.",
                   "A number: d decimals (or those of `significatifs` significant digits, 6 at most), a decimal comma in French, thousands separated from 10,000 on, the typographic minus sign; — when it is missing.")),
    "nuee_nombre <- function(x, decimales = 2, significatifs = NULL) {",
    "  if (!length(x) || is.na(x)) return(\"\\u2014\")",
    "  if (!is.finite(x)) return(if (x > 0) \"\\u221e\" else \"\\u2212\\u221e\")",
    "  d <- decimales",
    "  if (!is.null(significatifs)) d <- max(0, min(6, significatifs - 1 - (if (x == 0) 0 else floor(log10(abs(x))))))",
    "  texte <- nuee_arrondi(x, d)",
    "  entier <- sub(\"[.].*$\", \"\", texte)",
    "  if (abs(x) >= 10000) entier <- gsub(\"(?<=[0-9])(?=([0-9]{3})+$)\", if (nuee_langue == \"fr\") \"\\u202f\" else \",\", entier, perl = TRUE)",
    "  texte <- paste0(entier, if (d > 0) paste0(if (nuee_langue == \"fr\") \",\" else \".\", sub(\"^[^.]*[.]\", \"\", texte)))",
    "  if (x < 0 && grepl(\"[1-9]\", texte)) paste0(\"\\u2212\", texte) else texte",
    "}",
    "",
    commentaire(tx("Un pourcentage : 0,71 donne 71 %.", "A percentage: 0.71 gives 71%.")),
    "nuee_pourcentage <- function(x, decimales = 0) {",
    "  if (!length(x) || is.na(x)) return(\"\\u2014\")",
    "  paste0(nuee_nombre(100 * x, decimales), if (nuee_langue == \"fr\") \"\\u202f\" else \"\", \"%\")",
    "}",
    "",
    commentaire(tx("Une valeur p, comme dans un texte : p < 0,001, p = 0,032.", "A p-value, as in a text: p < 0.001, p = 0.032.")),
    "nuee_p <- function(p, decimales = 3) {",
    "  if (!length(p) || is.na(p)) return(\"p = \\u2014\")",
    "  seuil <- 10^-decimales",
    "  if (p < seuil) paste(\"p <\", nuee_nombre(seuil, decimales)) else paste(\"p =\", nuee_nombre(p, decimales))",
    "}",
    "",
    commentaire(tx("Le coefficient d'un terme d'une régression, d'après le tableau `coefficients` de son script (le terme, l'estimation, ..., p, les bornes de l'intervalle à 95 %) : l'estimation et son intervalle, avec les décimales de 3 chiffres significatifs (ou `decimales`), et p ; ou seulement l'estimation (quoi = \"est\"), l'intervalle (\"ic\") ou p (\"p\").",
                   "The coefficient of a term of a regression, from its script's `coefficients` table (the term, the estimate, ..., p, the bounds of the 95% interval): the estimate and its interval, with the decimals of 3 significant digits (or `decimales`), and p; or only the estimate (quoi = \"est\"), the interval (\"ic\") or p (\"p\").")),
    "nuee_coefficient <- function(tableau, terme, quoi = \"coef\", decimales = NULL) {",
    "  i <- match(terme, tableau[[1]])",
    "  est <- tableau[[2]][i]",
    "  bas <- tableau[[7]][i]",
    "  haut <- tableau[[8]][i]",
    "  d <- if (!is.null(decimales)) decimales else max(0, min(6, 2 - (if (!is.finite(est) || est == 0) 0 else floor(log10(abs(est))))))",
    "  ic <- if (is.finite(bas) && is.finite(haut)) paste0(\"[\", nuee_nombre(bas, d), if (nuee_langue == \"fr\") \" ; \" else \", \", nuee_nombre(haut, d), \"]\") else \"\"",
    "  switch(quoi, est = nuee_nombre(est, d), ic = ic, p = nuee_p(tableau[[6]][i]), paste0(nuee_nombre(est, d), \" \", ic, \", \", nuee_p(tableau[[6]][i])))",
    "}",
    "",
    commentaire(tx("Un coefficient de PLS (centré et mis à l'échelle) et son intervalle à 95 %, est ± demi, avec les décimales de 3 chiffres significatifs (ou `decimales`) : -0,35 [-0,56 ; -0,14].",
                   "A PLS coefficient (centred and scaled) and its 95% interval, est ± demi, with the decimals of 3 significant digits (or `decimales`): -0.35 [-0.56, -0.14].")),
    "nuee_intervalle <- function(est, demi, decimales = NULL) {",
    "  d <- if (!is.null(decimales)) decimales else max(0, min(6, 2 - (if (!is.finite(est) || est == 0) 0 else floor(log10(abs(est))))))",
    "  if (!is.finite(est - demi) || !is.finite(est + demi)) return(nuee_nombre(est, d))",
    "  paste0(nuee_nombre(est, d), \" [\", nuee_nombre(est - demi, d), if (nuee_langue == \"fr\") \" ; \" else \", \", nuee_nombre(est + demi, d), \"]\")",
    "}",
    "",
    commentaire(tx("Un intervalle seul, [a ; b], ses bornes à d décimales (en pourcentages : pourcentage = TRUE) : celui de la pente de TRIM, de la densité de l'échantillonnage par distances.",
                   "An interval alone, [a, b], its bounds with d decimals (as percentages: pourcentage = TRUE): that of TRIM's slope, of distance sampling's density.")),
    "nuee_ic <- function(bas, haut, decimales, pourcentage = FALSE) {",
    "  n <- function(x) if (pourcentage) nuee_pourcentage(x, decimales) else nuee_nombre(x, decimales)",
    "  paste0(\"[\", n(bas), if (nuee_langue == \"fr\") \" ; \" else \", \", n(haut), \"]\")",
    "}")
}

# Le nom de fichier d'un modèle : lettres, chiffres, espaces, _ ( ) -.
fichier_modele_reproductible <- function(nom) {
  x <- gsub("[^[:alnum:]_ ()-]", "_", as.character(nom %||% ""))
  if (nzchar(trimws(x))) x else "modele"
}

# Le nom R de l'environnement d'un modèle dans le .qmd (M1, M2_1...) : pris à ses données
# (script.R : noms_r), sans prendre celui d'une fonction de R ni des fonctions de nuee.R.
objets_reproductibles <- function(noms, langue) {
  x <- noms_r(noms)
  pris <- vapply(x, function(n) exists(n, envir = baseenv()) || startsWith(n, "nuee_") || n %in% c("e", "T", "F"), TRUE)
  x[pris] <- paste0(x[pris], if (identical(langue, "en")) "_model" else "_modele")
  make.unique(x, sep = "_")
}

# Un script relancé ici comme nuee_refaire le relance dans le .qmd, dans un environnement de R et
# de ses paquets, sans le moteur (comme l'export vers R le vérifie), sans graine imposée (un script
# tire son hasard de la sienne) ; le hasard de la séance est rendu ensuite.
refaire_script_ici <- function(script) {
  e <- new.env(parent = parent.env(globalenv()))
  hasard <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  genre <- RNGkind()
  grDevices::pdf(NULL)
  on.exit({
    grDevices::dev.off()
    suppressWarnings(RNGkind(genre[1], genre[2], genre[3]))
    if (!is.null(hasard)) assign(".Random.seed", hasard, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  utils::capture.output(source(script, local = e, chdir = TRUE, encoding = "UTF-8"))
  e
}

# L'expression R du nombre (ou du tableau des coefficients d'une régression, avec le terme) qu'une
# valeur du document cite, pour un modèle de la famille `famille` dont l'environnement s'appelle
# `objet` ; NULL quand le script ne l'a pas. `n` : un nom du script dans sa langue. Comme
# references.js : valeurReference.
expression_valeur <- function(famille, chemin, d, m, objet, n) {
  o <- function(...) paste(c(objet, vapply(c(...), n, "")), collapse = "$")
  cle <- chemin[1]
  reste <- chemin[-1]
  if (famille == "regression") {
    element <- function(x, nom) sprintf("%s[[%s]]", o(x), code_texte(n(nom)))
    return(switch(cle,
      N = sprintf("nrow(%s)", o("cadre")), K = sprintf("length(%s)", o("utiles")),
      R2 = element("r2", "r2"), R2_ajuste = element("r2", "r2_ajuste"), R2_marginal = element("r2", "marginal"),
      R2_conditionnel = element("r2", "conditionnel"), deviance_expliquee = element("r2", "deviance_expliquee"),
      Q2 = o("vc", "Q2"), RMSEcv = o("vc", "RMSEcv"),
      AIC = sprintf("%s[[\"AIC\"]]", o("criteres")), AICc = sprintf("%s[[\"AICc\"]]", o("criteres")), BIC = sprintf("%s[[\"BIC\"]]", o("criteres")),
      coef = , est = , p = , ic = if (length(reste)) list(tableau = o("coefficients"), terme = paste(reste, collapse = ".")),
      NULL))
  }
  if (famille %in% c("communaute", "autre")) return(expression_cle(famille, chemin, d, m, o, n))
  if (famille != "composantes") return(NULL)
  A <- m$A
  pls <- identical(m$type, "PLS")
  # (une composante : la dernière, ou celle que le chemin donne)
  composante <- function(colonne) {
    a <- if (length(reste)) suppressWarnings(as.integer(reste[1])) else A
    if (is.na(a) || a < 1 || a > A) return(NULL)
    sprintf("%s[%d]", o("composantes", colonne), a)
  }
  y <- if (length(reste) && nzchar(reste[1])) reste[1] else m$y$noms[1]
  switch(cle,
    R2X = composante("R2Xcum"), R2Y = if (pls) composante("R2Ycum"), Q2 = composante("Q2cum"),
    Q2_vide = o("q2_vide"), gain = o("gain"),
    RMSEE = if (pls) sprintf("%s[[%s]]", o("RMSEE"), code_texte(y)), RMSEcv = if (pls) sprintf("%s[[%s]]", o("RMSEcv"), code_texte(y)),
    bien_classes = if (isTRUE(m$plsda)) sprintf("sum(diag(%1$s)) / max(sum(%1$s), 1)", o("classification")),
    A = o("A"), N = o("N"), K = o("K"),
    vip = if (pls && length(reste)) sprintf("%s[match(%s, %s)]", o("variables", "VIP"), code_texte(paste(reste, collapse = ".")), o("variables", "terme")),
    # (un coefficient centré et mis à l'échelle, du premier Y, et la demi-largeur de son intervalle :
    # ceux d'une PLS validée par des sous-modèles, ic_coefficients, script.R)
    coef = if (pls && length(reste) && !isTRUE(m$opls) && !identical(mode_validation(d), "complet")) {
      terme <- code_texte(paste(reste, collapse = "."))
      list(est = sprintf("%s[%s, 1]", o("B"), terme), demi = sprintf("%s[%s, 1]", o("ic_coefficients"), terme))
    },
    cvanova = if (pls && isTRUE(reste[1] %in% c("p", "F"))) {
      sprintf("%s[[%s]]$%s[2]", o("cv_anova"), code_texte(if (length(reste) > 1 && nzchar(reste[2])) reste[2] else m$y$noms[1]), reste[1])
    },
    NULL)
}

# L'expression R d'un résultat clé d'une autre analyse qu'un document cite (references.js :
# referenceCle ; resultats_cles.R : entrees_<type>), d'après les objets de son script : la pente globale
# de TRIM (pente, .bas, .haut, .ic, .p, .pourcent et son intervalle, .classe), σ² et ρ ; la densité et
# l'effectif de l'échantillonnage par distances (D, abondance, leurs bornes, leur CV, une strate), p et
# ESW ou EDR ; le R², le R² ajusté et le test d'une RDA ou d'une CCA ; le stress d'une NMDS ; le R² et
# la valeur p d'une PERMANOVA ; N et K d'une analyse de communautés. Un intervalle : list(bas, haut).
# NULL quand le script ne l'a pas. `o`, `n` : ceux de expression_valeur.
expression_cle <- function(famille, chemin, d, m, o, n) {
  type <- as.character(d$type)
  k <- paste(chemin, collapse = ".")
  # (la partie citée d'une entrée : "valeur" pour l'entrée elle-même, sinon ce qui suit sa clé ; NA : une autre)
  partie <- function(base) if (identical(k, base)) "valeur" else if (startsWith(k, paste0(base, "."))) substring(k, nchar(base) + 2) else NA_character_
  element <- function(x, nom) sprintf("%s[[%s]]", x, code_texte(n(nom)))
  if (famille == "communaute") {
    if (identical(k, "N")) return(sprintf("nrow(%s)", o("Y")))
    if (identical(k, "K")) return(sprintf("ncol(%s)", o("Y")))
    if (identical(type, "NMDS") && identical(k, "stress")) return(o("nmds", "stress"))
    if (identical(type, "PERMANOVA")) {
      p <- partie("permanova.modele")
      if (identical(p, "valeur")) return(o("permanova", "modele", "R2"))
      if (identical(p, "p")) return(o("permanova", "modele", "p"))
    }
    return(NULL)
  }
  if (identical(type, "TRIM")) {
    pentes <- function(col) sprintf("%s[1]", element(o("pentes"), col))
    p <- partie("pente")
    if (!is.na(p)) return(switch(p, valeur = pentes("multiplicative"), bas = pentes("bas"), haut = pentes("haut"), ic = list(bas = pentes("bas"), haut = pentes("haut")),
                                 p = pentes("p"), classe = pentes("sens"), pourcent = sprintf("%s - 1", pentes("multiplicative")),
                                 pourcent.bas = sprintf("%s - 1", pentes("bas")), pourcent.haut = sprintf("%s - 1", pentes("haut")),
                                 pourcent.ic = list(bas = sprintf("%s - 1", pentes("bas")), haut = sprintf("%s - 1", pentes("haut"))), NULL))
    if (k %in% c("sigma2", "rho")) return(element(o("dispersion"), k))
    return(NULL)
  }
  if (identical(type, "DISTANCE")) {
    dens <- o("densite")
    # (la ligne du total, la dernière ; celle d'une strate par son nom)
    ligne <- function(col, strate = NULL) if (is.null(strate)) sprintf("utils::tail(%s, 1)", element(dens, col))
                                          else sprintf("%s[match(%s, %s)]", element(dens, col), code_texte(strate), element(dens, "strate"))
    for (base in c("D", "abondance")) {
      p <- partie(base)
      if (is.na(p)) next
      v <- if (base == "D") c(valeur = "D", bas = "bas_D", haut = "haut_D") else c(valeur = "N", bas = "bas_N", haut = "haut_N")
      return(switch(p, valeur = ligne(v[["valeur"]]), bas = ligne(v[["bas"]]), haut = ligne(v[["haut"]]), ic = list(bas = ligne(v[["bas"]]), haut = ligne(v[["haut"]])),
                    cv = ligne("cv"), NULL))
    }
    if (startsWith(k, "D.")) {
      strates <- as.character(m$densite$strate[!is.na(m$densite$strate)])
      s <- Find(function(x) identical(k, paste0("D.", x)) || startsWith(k, paste0("D.", x, ".")), strates)
      if (is.null(s)) return(NULL)
      p <- if (identical(k, paste0("D.", s))) "valeur" else substring(k, nchar(s) + 4)
      return(switch(p, valeur = ligne("D", s), bas = ligne("bas_D", s), haut = ligne("haut_D", s), ic = list(bas = ligne("bas_D", s), haut = ligne("haut_D", s)), cv = ligne("cv", s), NULL))
    }
    if (k %in% c("p", "ESW", "EDR")) {
      det <- o("detection")
      return(sprintf("%s[match(%s, %s)]", element(det, "estimation"), code_texte(k), element(det, "statistique")))
    }
    return(NULL)
  }
  if (type %in% c("RDA", "CCA")) {
    return(switch(k, R2 = o("r2", "R2"), R2_ajuste = o("r2", "ajuste"), test.modele = sprintf("%s[[\"F\"]]", o("test")),
                  test.modele.p = sprintf("%s[[\"p\"]]", o("test")), NULL))
  }
  NULL
}

# La valeur écrite comme le document la cite (`forme`, de references.js : formeReference) : l'appel
# de nuee.R autour de l'expression du nombre.
appel_valeur <- function(forme, x) {
  d <- suppressWarnings(as.integer(forme$decimales %||% NA))
  switch(as.character(forme$forme %||% "nombre"),
    pourcentage = if (isTRUE(d == 0)) sprintf("nuee_pourcentage(%s)", x) else sprintf("nuee_pourcentage(%s, %d)", x, d),
    p = if (is.na(d) || d == 3) sprintf("nuee_p(%s)", x) else sprintf("nuee_p(%s, %d)", x, d),
    coef = sprintf("nuee_coefficient(%s, %s%s%s)", x$tableau, code_texte(x$terme),
                   if (!identical(forme$quoi %||% "coef", "coef")) sprintf(", %s", code_texte(forme$quoi)) else "",
                   if (!is.na(d)) sprintf(", decimales = %d", d) else ""),
    coef_cs = sprintf("nuee_intervalle(%s, %s%s)", x$est, x$demi, if (!is.na(d)) sprintf(", decimales = %d", d) else ""),
    ic = if (is.list(x) && !is.null(x$bas)) sprintf("nuee_ic(%s, %s, %d%s)", x$bas, x$haut, if (is.na(d)) 2L else d, if (isTRUE(forme$pourcentage)) ", pourcentage = TRUE" else ""),
    texte = if (is.character(x)) x,
    nombre = if (!is.null(forme$significatifs)) sprintf("nuee_nombre(%s, significatifs = %d)", x, as.integer(forme$significatifs))
             else if (isTRUE(d == 2)) sprintf("nuee_nombre(%s)", x) else sprintf("nuee_nombre(%s, %d)", x, if (is.na(d)) 2L else d),
    NULL)
}

# Le code d'un graphique du script (ses lignes, dans nuee_dans) pour une vue de Nuée : list(lignes,
# largeur, hauteur), ou NULL. `variante` : celle de la vue (residus : standard ou quantiles ;
# dispersion : un groupement, quand le script n'en dessine qu'un).
graphique_reproductible <- function(famille, d, m, vue, variante, graphiques, objet, chemin) {
  table <- GRAPHIQUES_REPRODUCTIBLES[[if (famille %in% c("composantes", "regression")) famille else as.character(d$type)]]
  nom <- if (length(table) && vue %in% names(table)) table[[vue]]
  if (is.null(nom)) return(NULL)
  if (identical(vue, "residus") && identical(variante$residus, "quantiles")) nom <- "residus-quantiles"
  # (les valeurs prédites de la vue Effets, sur l'échelle de la réponse : un graphique par terme aussi)
  if (identical(vue, "effets") && identical(as.character(variante$echelle)[1], "reponse")) nom <- "prediction"
  if (identical(vue, "dispersion") && length(m$dispersions) != 1) return(NULL)
  # (une vue à variantes dont le script dessine tout d'un coup : seulement sans variante choisie ; les
  # effets d'une régression, un graphique par terme : celui de la variante ; les seuils d'une courbe,
  # que le script dessine toutes ensemble : quand le modèle n'en a qu'une)
  if (length(chemin) > 1 && !vue %in% c("residus", "effets")) return(NULL)
  seuils <- identical(vue, "effets") && isTRUE(variante$seuils)
  if (seuils) {
    v <- tryCatch(variantes_seuils(m), error = function(e) NULL)
    if (!isTRUE(nrow(v) == 1) || !identical(v$terme, as.character(variante$terme))) return(NULL)
  }
  g <- if (seuils) Find(function(x) identical(x$nom, "seuils"), graphiques %||% list())
       else if (identical(vue, "effets")) Find(function(x) startsWith(x$nom, paste0(nom, "-")) && identical(x$terme, as.character(variante$terme)), graphiques %||% list())
       else Find(function(x) identical(x$nom, nom), graphiques %||% list())
  if (is.null(g)) return(NULL)
  list(lignes = c(sprintf("nuee_dans(%s, {", objet), paste0("  ", g$code), "})"), largeur = g$largeur, hauteur = g$hauteur)
}

# Le code d'un tableau (knitr::kable des objets du script, dans nuee_dans) : ses lignes, ou NULL.
tableau_reproductible <- function(famille, d, m, cle, objet, langue) {
  type <- as.character(d$type)
  table <- TABLEAUX_REPRODUCTIBLES[[if (famille %in% c("composantes", "regression")) famille else type]]
  code <- if (length(table) && cle %in% names(table)) table[[cle]]
  if (famille == "composantes" && identical(cle, "cvanova") && identical(m$type, "PLS")) {
    code <- if (length(m$y$noms) == 1) "knitr::kable(cv_anova[[1]], digits = 4)"
            else "knitr::kable(do.call(rbind, lapply(names(cv_anova), function(y) cbind(Y = y, cv_anova[[y]]))), digits = 4)"
  }
  if (identical(cle, "ajustement_env") && type %in% c("PCOA", "NMDS")) {
    code <- if (!is.null(m$ajustement$vecteurs)) "knitr::kable(variables_ajustees, digits = 3)" else if (!is.null(m$ajustement$facteurs)) "knitr::kable(facteurs_ajustes, digits = 3)"
  }
  if (is.null(code)) return(NULL)
  code <- if (famille == "communaute") traduire_communautes(code, langue) else traduire_code(code, langue)
  sprintf("nuee_dans(%s, %s)", objet, code)
}

# Le document reproductible : les scripts des modèles cités et nuee.R écrits dans `dossier`/<nom>_R,
# puis ce que le .qmd en écrit.
#   modeles     une liste nommée par l'identifiant de chaque modèle cité : list(definition, modele,
#               et erreur, pourquoi il ne peut pas être refait, s'il ne le peut pas ; et projet, celui
#               sur lequel écrire son script quand ce n'est pas `projet` : le projet tel qu'il l'a vu,
#               ses catégories dans l'ordre d'alors, atelier.R : projet_tel_ajuste) ;
#   references  les références du document, dans l'ordre (NULL : celles que le document ne résout
#               pas) : list(modele, chemin, genre = "valeur" | "figure" | "tableau" | "texte", et pour
#               une valeur forme (references.js : formeReference) et texte (ce que Nuée en écrit),
#               pour un graphique vue et variante, pour un tableau cle) ;
#   langue      celle du document (ses nombres, les commentaires, les noms des scripts).
# Rend list(dossier_r, fichiers (leurs chemins), relatifs (au dossier du .qmd), installation (le
# bloc du début du .qmd), modeles (objet, script, erreur), references (code | lignes, largeur,
# hauteur | raison, chacune dans l'ordre), R (son dossier bin, et s'il a knitr et rmarkdown)).
ecrire_document_reproductible <- function(projet, modeles, references, dossier, nom, langue = "fr", version = "") {
  langue <- if (identical(langue, "en")) "en" else "fr"
  tx <- function(fr, en) if (langue == "en") en else fr
  sous <- paste0(nom, "_R")
  ici <- file.path(dossier, sous)
  dir.create(ici, showWarnings = FALSE, recursive = TRUE)
  aide <- file.path(ici, "nuee.R")
  writeBin(charToRaw(enc2utf8(paste0(avec_avis_licence(code_aide_reproductible(langue), langue, version), "\n", collapse = ""))), aide)
  fichiers <- aide
  # l'environnement où les expressions sont vérifiées : nuee.R et les modèles refaits
  ev <- new.env(parent = parent.env(globalenv()))
  sys.source(aide, envir = ev, keep.source = FALSE)
  ids <- names(modeles)
  definitions <- lapply(modeles, `[[`, "definition")
  noms <- vapply(definitions, function(d) as.character(d$name %||% ""), "")
  objets <- stats::setNames(objets_reproductibles(noms, langue), ids)
  pris <- "nuee"
  infos <- list()
  for (id in ids) {
    d <- modeles[[id]]$definition
    m <- modeles[[id]]$modele
    base <- fichier_modele_reproductible(d$name)
    while (tolower(base) %in% tolower(pris)) base <- paste0(base, "_")
    pris <- c(pris, base)
    chemin <- file.path(ici, paste0(base, ".R"))
    info <- list(id = id, nom = noms[[id]], objet = objets[[id]], script = paste(sous, paste0(base, ".R"), sep = "/"), famille = famille_reproductible(d),
                 type = as.character(d$type %||% ""))
    raison <- modeles[[id]]$erreur %||% raison_sans_export(d, tx)
    if (is.null(raison)) {
      r <- tryCatch(ecrire_script(modeles[[id]]$projet %||% projet, d, chemin, "moteur", m = m, donnees = "csv", version = version, langue = langue), error = function(e) e)
      if (inherits(r, "error")) raison <- conditionMessage(r)
      else {
        fichiers <- c(fichiers, r$chemins)
        info$graphiques <- attr(r, "graphiques")
        if (!is.null(r$ecart)) raison <- sprintf(tx("son script ne redonne pas les résultats de Nuée (%s)", "its script doesn't give Nuée's results back (%s)"), r$ecart)
        else {
          e <- tryCatch(refaire_script_ici(chemin), error = function(err) err)
          if (inherits(e, "error")) raison <- sprintf(tx("son script ne tourne pas seul (%s)", "its script doesn't run on its own (%s)"), conditionMessage(e))
          else assign(info$objet, e, envir = ev)
        }
      }
    }
    info$erreur <- raison
    infos[[id]] <- info
  }
  n_de <- function(info) {
    langue_noms <- langue
    if (info$famille == "communaute") return(function(x) nom_communautes(x, langue_noms))
    if (isTRUE(info$type %in% c("RDA", "CCA"))) return(function(x) unname(nom_ordination(x, langue_noms)))
    if (identical(info$type, "TRIM")) return(function(x) nom_traduit(x, langue_noms, NOMS_TRIM_EN))
    if (identical(info$type, "DISTANCE")) return(function(x) nom_traduit(x, langue_noms, NOMS_DISTANCE_EN))
    function(x) unname(nom_script(x, langue_noms))
  }
  evaluer <- function(code) tryCatch(eval(parse(text = code, keep.source = FALSE, encoding = "UTF-8")[[1]], envir = ev), error = function(e) e)
  # chaque référence : son code R, ou pourquoi elle reste écrite en dur
  notes <- character(0)
  sorties <- lapply(references, function(ref) {
    if (is.null(ref) || is.null(ref$modele) || is.null(infos[[as.character(ref$modele)]])) return(NULL)
    info <- infos[[as.character(ref$modele)]]
    chemin <- as.character(unlist(ref$chemin))
    qui <- paste(c(info$nom, chemin), collapse = ".")
    genre <- as.character(ref$genre %||% "valeur")
    texte_en_dur <- function(raison) {
      notes <<- c(notes, sprintf(tx("%s : %s", "%s: %s"), qui, raison))
      list(raison = raison)
    }
    if (!is.null(info$erreur)) return(list(raison = info$erreur))
    d <- modeles[[info$id]]$definition
    m <- modeles[[info$id]]$modele
    if (genre == "texte") {
      return(texte_en_dur(if (identical(chemin[1], "resultats")) tx("le texte des résultats, tel que Nuée l'a écrit ; il n'est pas recalculé", "the Results text, as Nuée wrote it; it isn't recomputed")
                          else tx("le texte des méthodes, tel que Nuée l'a écrit ; il n'est pas recalculé", "the Methods text, as Nuée wrote it; it isn't recomputed")))
    }
    if (genre == "figure") {
      g <- graphique_reproductible(info$famille, d, m, as.character(ref$vue %||% chemin[1]), ref$variante %||% list(), info$graphiques, info$objet, chemin)
      if (is.null(g)) return(texte_en_dur(tx("le script ne dessine pas ce graphique : l'image de Nuée", "the script doesn't draw this plot: Nuée's image")))
      grDevices::pdf(NULL)
      x <- tryCatch(evaluer(paste(g$lignes, collapse = "\n")), finally = grDevices::dev.off())
      if (inherits(x, "error")) return(texte_en_dur(sprintf(tx("son graphique ne se dessine pas (%s) : l'image de Nuée", "its plot can't be drawn (%s): Nuée's image"), conditionMessage(x))))
      return(list(lignes = I(g$lignes), largeur = g$largeur, hauteur = g$hauteur))
    }
    if (genre == "tableau") {
      code <- tableau_reproductible(info$famille, d, m, as.character(ref$cle %||% chemin[2]), info$objet, langue)
      if (is.null(code)) return(texte_en_dur(tx("le script n'a pas ce tableau : celui de Nuée", "the script hasn't this table: Nuée's")))
      # (sans knitr ici : le tableau que knitr::kable recevrait)
      appel <- parse(text = code, keep.source = FALSE)[[1]]
      if (!requireNamespace("knitr", quietly = TRUE)) appel[[3]] <- appel[[3]][[2]]
      x <- tryCatch(eval(appel, envir = ev), error = function(e) e)
      if (inherits(x, "error") || !NROW(x)) return(texte_en_dur(tx("le tableau du script ne se fait pas : celui de Nuée", "the script's table can't be made: Nuée's")))
      return(list(lignes = I(code)))
    }
    x <- expression_valeur(info$famille, chemin, d, m, info$objet, n_de(info))
    # (une valeur et ce qu'en attend sa forme : un coefficient de PLS demande son intervalle)
    if (!is.null(x) && !identical(is.list(x) && !is.null(x$demi), identical(ref$forme$forme, "coef_cs"))) x <- NULL
    if (!is.null(x) && !identical(is.list(x) && !is.null(x$bas), identical(ref$forme$forme, "ic"))) x <- NULL
    code <- if (!is.null(x)) appel_valeur(ref$forme %||% list(), x)
    if (is.null(code)) {
      return(texte_en_dur(if (identical(ref$forme$forme, "coef_cs")) tx("l'intervalle (jack-knife) de ce coefficient de PLS n'est pas dans son script (une OPLS, ou une validation avec les centres et les poids du modèle complet)",
                                                                       "the (jackknife) interval of this PLS coefficient isn't in its script (an OPLS, or a cross-validation with the full model's centres and weights)")
                          else tx("son script n'a pas cette valeur", "its script hasn't this value")))
    }
    v <- evaluer(code)
    attendu <- as.character(ref$texte %||% "")
    if (inherits(v, "error")) return(texte_en_dur(sprintf(tx("l'expression ne s'évalue pas (%s)", "the expression can't be evaluated (%s)"), conditionMessage(v))))
    if (!identical(enc2utf8(as.character(v)), enc2utf8(attendu))) {
      return(texte_en_dur(sprintf(tx("le script donne %s, Nuée %s", "the script gives %s, Nuée %s"), as.character(v)[1], attendu)))
    }
    if (grepl("`", code, fixed = TRUE)) return(texte_en_dur(tx("son expression ne tient pas dans le texte", "its expression doesn't fit in the text")))
    list(code = code)
  })
  # le bloc du début du .qmd : nuee.R, les modèles refaits, ce qui reste écrit en dur
  refaits <- Filter(function(i) is.null(i$erreur), infos)
  sans <- Filter(function(i) !is.null(i$erreur), infos)
  installation <- c("```{r}", "#| label: nuee-modeles", "#| include: false",
    commentaire(sprintf(tx("Les modèles cités, refaits ici à partir des données brutes, chacun par son export vers R (le dossier « %s ») ; nuee.R écrit leurs nombres comme Nuée.",
                           "The models cited, redone here from the raw data, each by its export to R (the folder “%s”); nuee.R writes their numbers as Nuée does."), sous)),
    if (length(refaits)) c(sprintf("source(%s, encoding = \"UTF-8\")", code_texte(paste(sous, "nuee.R", sep = "/"))),
                           vapply(refaits, function(i) sprintf("%s <- nuee_refaire(%s)", i$objet, code_texte(i$script)), "", USE.NAMES = FALSE)),
    if (length(sans) || length(notes)) c(commentaire(tx("Écrits en dur, tels que Nuée les montre :", "Written in as they are, as Nuée shows them:")),
      unlist(lapply(c(vapply(sans, function(i) sprintf(tx("%s : %s", "%s: %s"), i$nom, i$erreur), ""), unique(notes)), function(x) sub("^# ", "#   ", commentaire(x))))),
    "```")
  r_bin <- normalizePath(R.home("bin"), winslash = "/", mustWork = FALSE)
  list(dossier_r = sous, fichiers = I(unname(fichiers)), relatifs = I(paste(sous, basename(unname(fichiers)), sep = "/")),
       installation = I(installation),
       modeles = unname(lapply(infos, function(i) list(id = i$id, nom = i$nom, objet = i$objet, script = i$script, erreur = i$erreur))),
       references = sorties,
       R = list(bin = r_bin, knitr = requireNamespace("knitr", quietly = TRUE), rmarkdown = requireNamespace("rmarkdown", quietly = TRUE)))
}
