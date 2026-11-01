# Écrit un script R autonome qui refait un modèle sans Nuée, avec R seul : les données, le
# code des fonctions du moteur dont le modèle a besoin (copié de moteur/R avec ses
# commentaires), puis la définition du modèle, son ajustement et ses résultats. D'après
# l'extension du chemin : un script (.R), ou un rapport R Markdown (.Rmd) ou Quarto (.qmd)
# qui y ajoute le texte des méthodes (langue "fr" ou "en"), des tableaux, des graphiques et
# les références ; il se compile avec knitr (bouton Knit de RStudio, ou quarto render).
#   donnees : "script" (les données dans le script) ou "csv" (un fichier à côté du script) ;
#   renv, rdata, rproj : écrire aussi renv.lock (versions de R et des paquets), les données et
#   le modèle ajusté en .RData, un projet RStudio (.Rproj), à côté du script.
# Avant d'écrire, le modèle du script est recalculé et comparé à `m`, celui de l'application :
# un écart est renvoyé dans `ecart`.
ecrire_script <- function(projet, d, chemin_script, dossier_moteur, m = NULL, donnees = c("script", "csv"),
                          renv = FALSE, rdata = FALSE, rproj = FALSE, version = "", langue = "fr") {
  donnees <- match.arg(donnees)
  format <- c(r = "R", rmd = "Rmd", qmd = "qmd")[[tolower(tools::file_ext(chemin_script))]]
  base <- sub("\\.[^.]+$", "", basename(chemin_script))
  dossier <- dirname(chemin_script)
  tab <- tableau_script(projet)
  nom_projet <- as.character(projet$source %||% "projet")
  code_projet <- sprintf("p <- projet_depuis_tableau(donnees, %s, secondaires = FALSE, toutes_modalites = TRUE)", code_texte(nom_projet))
  code_d <- code_modele_script(projet, d, tab)

  # le modèle du script, calculé ici sur le projet refait à partir du tableau
  m <- m %||% ajuster(projet, d)
  e <- new.env()
  e$donnees <- tab$donnees
  eval(parse(text = c(code_projet, code_d, "m <- ajuster(p, d)"), encoding = "UTF-8"), envir = e)
  signes <- if (identical(dim(e$m$T), dim(m$T))) vapply(seq_len(m$A), function(a) if (sum(m$T[, a] * e$m$T[, a], na.rm = TRUE) < 0) -1 else 1, 0)
  code_signes <- if (any(signes < 0)) c(
    "# Le signe d'une composante est arbitraire : ceux-ci donnent les mêmes graphiques que dans Nuée.",
    sprintf("m <- orienter_comme(m, t(m$T) * %s)", code_nombres(signes)),
    "m$statistiques <- statistiques(m)")
  if (length(code_signes)) eval(parse(text = code_signes), envir = e)
  ecart <- ecart_modeles(m, e$m)

  pls <- identical(d$type, "PLS")
  code_fin <- c("m <- ajuster(p, d)", code_signes, "",
    "# Résultats (les mêmes que dans Nuée)",
    "m$statistiques$composantes         # R2X, R2Y, Q2 et valeur propre par composante",
    "m$statistiques$variables           # loadings, VIP, intervalles de confiance",
    if (pls) "m$statistiques$coefficients        # coefficients (centrés-réduits et dans les unités des données)",
    "head(m$statistiques$observations)  # scores, T2, DModX, valeurs ajustées",
    if (pls) "m$statistiques$cv_anova           # CV-ANOVA",
    if (pls) "# permutations(m, n = 20)          # test de permutation (quelques secondes)",
    if (isTRUE(d$plsda)) "m$statistiques$classification$vc$table  # table de classification (validation croisée)",
    if (length(d$test)) "m$test$statistiques                # jeu de test : RMSEP, biais, SEP, Q2 externe (jeu_test.R)",
    if (length(d$test) && isTRUE(d$plsda)) "m$test$classement                  # jeu de test : classes prédites")

  # le code du moteur dont ces lignes ont besoin
  moteur <- fonctions_moteur(dossier_moteur)
  utilises <- symboles(parse(text = c(code_projet, code_d, code_fin, if (pls) "permutations(m)")))
  gardees <- dependances(utilises, moteur)
  paquets <- paquets_utilises(moteur[gardees])

  chemin_csv <- file.path(dossier, paste0(base, " - donnees.csv"))
  fichiers <- c(script = chemin_script,
                donnees = if (donnees == "csv") chemin_csv,
                rdata = if (rdata) file.path(dossier, paste0(base, ".RData")),
                renv = if (renv) file.path(dossier, "renv.lock"),
                rproj = if (rproj) file.path(dossier, paste0(base, ".Rproj")))
  if (donnees == "csv") ecrire_csv_script(tab$donnees, chemin_csv)
  code_moteur <- c("# 1. Vérifications ----", code_verifications(paquets, langue), "",
                   "# 2. Fonctions du moteur de Nuée ----",
                   sprintf("# Copiées du moteur de Nuée %s (dossier moteur/R ; licence GPL-3.0 ou ultérieure), sans", version),
                   sprintf("# modification : les %d fonctions et constantes dont ce modèle a besoin, fichier par fichier.", length(gardees)),
                   texte_moteur(moteur, gardees))
  code_donnees <- if (donnees == "csv") code_lire_csv(tab$donnees, basename(chemin_csv), langue) else code_donnees(tab$donnees)
  lignes <- if (format == "R") {
    c(entete_script(d, m, nom_projet, version, fichiers), "", code_moteur, "",
      "# 3. Données ----", code_donnees, "",
      "# 4. Modèle et résultats ----", code_projet, "", code_d, "", code_fin)
  } else {
    methodes <- texte_methodes(projet, d, m, langue, version)
    lignes_rapport(format, d, m, langue, methodes, code_moteur, code_donnees, c(code_projet, "", code_d, "", "m <- ajuster(p, d)", code_signes))
  }
  writeBin(charToRaw(enc2utf8(paste0(lignes, "\n", collapse = ""))), chemin_script)

  if (rdata) {
    e$donnees <- tab$donnees
    save(list = c("donnees", "p", "d", "m"), envir = e, file = fichiers[["rdata"]])
  }
  if (renv) ecrire_renv(c(paquets, if (format != "R") c("knitr", "rmarkdown")), fichiers[["renv"]])
  if (rproj) writeBin(charToRaw(paste0(c("Version: 1.0", "", "RestoreWorkspace: No", "SaveWorkspace: No", "AlwaysSaveHistory: Default", "",
                                          "EnableCodeIndexing: Yes", "UseSpacesForTab: Yes", "NumSpacesForTab: 2", "Encoding: UTF-8"), "\n", collapse = "")),
                      fichiers[["rproj"]])
  list(script = chemin_script, chemins = unname(fichiers), ecart = ecart)
}

# Le tableau du script : les noms des observations, puis une colonne par variable (des facteurs
# pour les variables qualitatives, avec toutes leurs catégories, dans leur ordre). Les noms
# des variables sont rendus uniques ; `noms` les donne par numéro de variable du projet, `rangs`
# le rang de chaque observation (son numéro dans le projet refait par le script).
tableau_script <- function(projet) {
  obs <- rownames(projet$donnees)
  vars <- Filter(function(v) as.character(v$uid) %in% colnames(projet$donnees), projet$variables)
  noms <- vapply(vars, function(v) as.character(v$ids[1] %||% ""), "")
  noms[!nzchar(noms)] <- paste0("V", which(!nzchar(noms)))
  noms <- make.unique(c("Observation", noms), sep = "_")
  colonnes <- c(list(unname(as.character(projet$noms_obs[obs]))), lapply(vars, function(v) {
    x <- projet$donnees[, as.character(v$uid)]
    if (length(v$categories)) factor(as.character(v$categories)[x], levels = as.character(v$categories)) else unname(as.numeric(x))
  }))
  names(colonnes) <- noms
  list(donnees = as.data.frame(colonnes, check.names = FALSE, stringsAsFactors = FALSE),
       noms = stats::setNames(noms[-1], vapply(vars, function(v) as.character(v$uid), "")),
       rangs = stats::setNames(seq_along(obs), obs))
}

# Le code qui crée la définition `d` sur le projet refait par le script : definir_modele()
# quand il redonne exactement le modèle (mêmes termes, dans le même ordre, mêmes
# observations), sinon la liste des termes telle que le projet la décrit.
code_modele_script <- function(projet, d, tab) {
  e <- new.env()
  e$p <- projet_depuis_tableau(tab$donnees, "p", secondaires = FALSE, toutes_modalites = TRUE)
  avec_noms <- projet
  avec_noms$noms_var <- tab$noms
  code <- tryCatch({
    r <- reglages_modele(projet, d)
    if ("autre" %in% unlist(r$transformations)) stop("transformation")
    if (!all(unlist(r$echelles) %in% c("UV", "Par", "Ctr", "none"))) stop("échelle")
    code_definition(avec_noms, utils::modifyList(d, list(app = TRUE)), "p")
  }, error = function(e) NULL)
  ok <- !is.null(code) && !inherits(try(eval(parse(text = code, encoding = "UTF-8"), envir = e), silent = TRUE), "try-error") &&
    identical(cles_termes(e$d$x, e$p$noms_var), cles_termes(d$x, tab$noms)) && identical(cles_termes(e$d$y, e$p$noms_var), cles_termes(d$y, tab$noms)) &&
    identical(as.numeric(e$d$observations), as.numeric(tab$rangs[as.character(d$observations)])) &&
    identical(as.numeric(e$d$test %||% numeric(0)), as.numeric(tab$rangs[as.character(d$test)])) &&
    identical(e$d$spectres, d$spectres) && identical(e$d$type, d$type) && identical(isTRUE(e$d$opls), isTRUE(d$opls)) &&
    identical(isTRUE(e$d$plsda), isTRUE(d$plsda)) && identical(e$d$Ap %||% 1, d$Ap %||% 1)
  if (!ok) code <- code_termes(d, tab)
  ynames <- d[["reference"]][["ynames"]]
  if (length(ynames)) code <- c(code, sprintf("d$reference <- list(ynames = %s)  # noms des Y du projet d'origine", code_textes(ynames)))
  code
}

# Ce qui fait un terme dans le calcul : variables (par leur nom, `noms` les donnant par numéro),
# catégories, transformation, mise à l'échelle.
cles_termes <- function(termes, noms) {
  vapply(termes, function(t) {
    ech <- t$scaling
    connue <- !is.null(ech$type) && !is.na(ech$type)
    tr <- t$transform
    paste(paste(noms[as.character(t$variables)], collapse = "*"), paste(vapply(seq_along(t$variables), function(j) categorie(t, j), 0), collapse = "*"),
          if (connue) ech$type else paste("fixe", ech$centre %||% 0, ech$weight %||% 1),
          if (is.null(tr)) "" else paste(as.character(tr$type), paste(texte_exact(as.numeric(tr$params)), collapse = ",")), sep = "|")
  }, "")
}

# La définition `d` écrite terme par terme (les variables désignées par leur nom).
code_termes <- function(d, tab) {
  terme <- function(t) {
    noms <- unname(tab$noms[as.character(t$variables)])
    cats <- vapply(seq_along(t$variables), function(j) categorie(t, j), 0)
    ech <- t$scaling
    echelle <- if (!is.null(ech$type) && !is.na(ech$type)) sprintf("list(type = %s)", code_texte(ech$type)) else
      sprintf("list(centre = %s, weight = %s)", code_nombres(ech$centre %||% 0), code_nombres(ech$weight %||% 1))
    sprintf("    list(variables = numeros(p, %s), categories = %s, name = %s,%s scaling = %s)",
            code_textes(noms), code_nombres(cats), code_texte(as.character(t$name)),
            if (is.null(t$transform)) "" else sprintf(" transform = %s,", paste(deparse(t$transform, width.cutoff = 500L), collapse = "")),
            echelle)
  }
  liste <- function(termes) if (!length(termes)) "list()" else
    paste0("list(\n", paste(vapply(termes, terme, ""), collapse = ",\n"), ")")
  c("# Le modèle tel que le projet le définit : ses termes X et Y, chacun avec sa transformation et",
    "# sa mise à l'échelle (centre et poids enregistrés dans le projet quand l'échelle n'est pas",
    "# UV, Par, Ctr ou none), et les observations retenues (leur rang dans le tableau).",
    sprintf("d <- list(type = %s, plsda = %s, opls = %s, name = %s, title = %s,", code_texte(d$type), isTRUE(d$plsda), isTRUE(d$opls),
            code_texte(as.character(d$name %||% "")), code_texte(as.character(d$title %||% ""))),
    sprintf("          A = %s, cv_groups = %s,%s", d$A, d$cv_groups %||% 7, if (isTRUE(d$opls)) sprintf(" Ap = %s,", d$Ap %||% 1) else ""),
    sprintf("          observations = %s,", code_rangs(tab$rangs[as.character(d$observations)])),
    if (length(d$test)) sprintf("          test = %s,  # jeu de test (jeu_test.R)", code_rangs(tab$rangs[as.character(d$test)])),
    if (length(d$spectres)) sprintf("          spectres = %s,", paste(deparse(d$spectres, width.cutoff = 500L), collapse = "")),
    sprintf("          x = %s,", liste(d$x)),
    sprintf("          y = %s)", liste(d$y)))
}

# ---------------------------------------------------------------- le code du moteur

# Les définitions de moteur/R (fonctions et constantes), avec leur texte : les commentaires
# placés juste au-dessus et le corps tel qu'il est écrit. `fichier` : leur fichier, dont
# l'en-tête (le premier bloc de commentaires) est repris avant ses fonctions.
fonctions_moteur <- function(dossier_moteur) {
  entrees <- list()
  for (f in sort(list.files(file.path(dossier_moteur, "R"), "\\.R$", full.names = TRUE))) {
    lignes <- readLines(f, encoding = "UTF-8", warn = FALSE)
    exprs <- parse(text = lignes, keep.source = TRUE, encoding = "UTF-8")
    sources <- attr(exprs, "srcref")
    fin_precedente <- 0
    for (i in seq_along(exprs)) {
      x <- exprs[[i]]
      debut <- sources[[i]][1]
      fin <- sources[[i]][3]
      if (is.call(x) && as.character(x[[1]]) %in% c("<-", "=") && is.name(x[[2]])) {
        haut <- debut
        while (haut - 1 > fin_precedente && grepl("^\\s*#", lignes[haut - 1])) haut <- haut - 1
        nom <- as.character(x[[2]])
        entrees[[nom]] <- list(nom = nom, fichier = basename(f), texte = lignes[haut:fin],
                               symboles = setdiff(symboles(x[[3]]), nom))
      }
      fin_precedente <- fin
    }
    premier <- if (length(exprs)) sources[[1]][1] else length(lignes) + 1
    entete <- lignes[seq_len(premier - 1)]
    entete <- entete[seq_len(match(FALSE, grepl("^\\s*#", entete), nomatch = length(entete) + 1) - 1)]
    attr(entrees, paste0("entete:", basename(f))) <- entete
  }
  entrees
}

# Les noms qu'utilise du code R, sans les noms d'éléments (x$nom).
symboles <- function(x) {
  if (is.name(x)) return(as.character(x))
  if (is.call(x) && (identical(x[[1]], as.name("$")) || identical(x[[1]], as.name("@")))) return(symboles(x[[2]]))
  if (is.call(x) || is.pairlist(x) || is.expression(x)) return(unique(unlist(lapply(as.list(x), symboles))))
  character(0)
}

# Les définitions nécessaires à partir de `noms` : celles qu'ils utilisent, et ainsi de suite.
dependances <- function(noms, entrees) {
  gardees <- character(0)
  a_voir <- intersect(noms, names(entrees))
  while (length(a_voir)) {
    n <- a_voir[1]
    a_voir <- a_voir[-1]
    if (n %in% gardees) next
    gardees <- c(gardees, n)
    a_voir <- c(a_voir, setdiff(intersect(entrees[[n]]$symboles, names(entrees)), gardees))
  }
  gardees
}

# Les paquets appelés par paquet::fonction(), hors ceux livrés avec R.
paquets_utilises <- function(entrees) {
  texte <- unlist(lapply(entrees, `[[`, "texte"))
  texte <- sub("#.*$", "", texte)
  p <- unique(unlist(regmatches(texte, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=:::?)", texte, perl = TRUE))))
  sort(setdiff(p, rownames(utils::installed.packages(priority = "base"))))
}

texte_moteur <- function(moteur, gardees) {
  fichiers <- unique(vapply(moteur, `[[`, "", "fichier"))
  unlist(lapply(fichiers, function(f) {
    ici <- Filter(function(x) x$fichier == f && x$nom %in% gardees, moteur)
    if (!length(ici)) return(NULL)
    c("", sprintf("# -- %s", f), attr(moteur, paste0("entete:", f)), unlist(lapply(ici, function(x) c("", x$texte))))
  }))
}

# ---------------------------------------------------------------- les autres parties

entete_script <- function(d, m, nom_projet, version, fichiers) {
  type <- paste0(if (isTRUE(d$opls)) "O" else "", if (isTRUE(d$plsda)) "PLS-DA" else if (identical(d$type, "PCA")) "ACP" else "PLS")
  autres <- fichiers[names(fichiers) != "script"]
  quoi <- c(donnees = "les données (le script les lit)", rdata = "les données et le modèle ajusté : load() les rend sans recalculer",
            renv = "les versions de R et des paquets : renv::restore() installe les mêmes", rproj = "le projet RStudio : l'ouvrir ouvre ce dossier")
  c(sprintf("# Modèle %s%s : %s, %d composante%s, %d observations, %d termes X", d$name %||% "",
            if (nzchar(d$title %||% "")) sprintf(" (%s)", d$title) else "", type, m$A, if (m$A > 1) "s" else "", m$N, m$K),
    sprintf("# Script R écrit par Nuée %s le %s, à partir du projet « %s ».", version, format(Sys.Date()), nom_projet),
    "# Il refait le modèle sans Nuée, avec R seul : il contient le code des fonctions du moteur de",
    "# Nuée dont le modèle a besoin, puis les données et le modèle. Ses quatre parties :",
    "#   1. Vérifications : version de R, paquets",
    "#   2. Fonctions du moteur de Nuée (copiées telles quelles, commentées en français)",
    "#   3. Données",
    "#   4. Modèle et résultats : la partie à lire et à modifier",
    "# Dans RStudio, Ctrl+Maj+O affiche ces parties ; Ctrl+Maj+S lance tout le script.",
    if (length(autres)) c("# À côté de ce script :", sprintf("#   %s : %s", basename(autres), quoi[names(autres)])))
}

# Le rapport R Markdown ou Quarto : le code du moteur dans un premier bloc caché, puis les
# méthodes, les données, le modèle, ses résultats (tableaux et graphiques en R de base) et les
# références.
lignes_rapport <- function(format, d, m, langue, methodes, code_moteur, code_donnees, code_modele) {
  fr <- langue == "fr"
  tx <- function(f, e) if (fr) f else e
  pls <- identical(d$type, "PLS")
  opls <- isTRUE(d$opls)
  plsda <- isTRUE(d$plsda)
  bloc <- function(options, code) c(sprintf("```{r%s}", if (nzchar(options)) paste0(", ", options) else ""), code, "```", "")
  titre <- sprintf("%s %s%s", tx("Modèle", "Model"), d$name %||% "", if (nzchar(d$title %||% "")) paste(" —", d$title) else "")
  yaml <- c("---", sprintf("title: %s", code_texte(titre)), sprintf("date: %s", code_texte(format(Sys.Date()))), sprintf("lang: %s", langue),
            if (format == "Rmd") "output: html_document" else c("format:", "  html:", "    embed-resources: true"), "---", "")
  couleurs <- if (plsda) c("classe <- factor(obs$classe)", "couleur <- as.integer(classe)") else "couleur <- 1"
  legende <- if (plsda) "legend(\"topright\", legend = levels(classe), col = seq_along(levels(classe)), pch = 19, bty = \"n\", cex = 0.8)"
  axes <- if (opls) c("t[1]", "to[1]") else c("t[1]", "t[2]")
  graphe_scores <- if (m$A >= 2) c(
    sprintf("plot(obs$t1, obs$t2, pch = 19, col = couleur, xlab = %s, ylab = %s, main = %s)", code_texte(axes[1]), code_texte(axes[2]), code_texte(tx("Scores", "Scores"))),
    "abline(h = 0, v = 0, col = \"grey\")", legende) else c(
    sprintf("plot(obs$t1, pch = 19, col = couleur, xlab = %s, ylab = \"t[1]\", main = \"Scores\")", code_texte(tx("Observation", "Observation"))),
    "abline(h = 0, col = \"grey\")", legende)
  graphe_poids <- if (m$A >= 2) c(
    "plot(v$p1, v$p2, type = \"n\", xlab = \"p[1]\", ylab = \"p[2]\", main = \"Loadings\")",
    "abline(h = 0, v = 0, col = \"grey\")", "text(v$p1, v$p2, v$terme, cex = 0.7)")
  else "barplot(v$p1, names.arg = v$terme, las = 2, cex.names = 0.7, main = \"Loadings p[1]\")"
  y1 <- m$y$noms[1]
  c(yaml,
    bloc("moteur, include = FALSE", code_moteur),
    sprintf("## %s", tx("Méthodes", "Methods")), "", rbind(methodes$paragraphes, ""),
    sprintf("## %s", tx("Données", "Data")), "",
    bloc("donnees, echo = FALSE", code_donnees),
    bloc("", "str(donnees, list.len = 12)"),
    sprintf("## %s", tx("Modèle", "Model")), "",
    bloc("modele", code_modele),
    sprintf("## %s", tx("Résultats", "Results")), "",
    bloc("composantes", "knitr::kable(m$statistiques$composantes, digits = 3)"),
    bloc("scores, fig.width = 6, fig.height = 5", c("s <- m$statistiques", "obs <- s$observations", couleurs, graphe_scores)),
    bloc("loadings, fig.width = 6, fig.height = 5", c("v <- s$variables", graphe_poids)),
    if (pls) bloc("vip, fig.width = 7, fig.height = 4", c("ordre <- order(v$VIP, decreasing = TRUE)",
      "barplot(v$VIP[ordre], names.arg = v$terme[ordre], las = 2, cex.names = 0.7, main = \"VIP\")",
      "abline(h = s$seuil_vip$seuil, lty = 2, col = \"red\")")),
    if (pls && !plsda) bloc("observe-predit, fig.width = 5, fig.height = 5", c(
      sprintf("y_obs <- obs[[%s]]", code_texte(paste(y1, "observé"))),
      sprintf("y_aj <- obs[[%s]]", code_texte(paste(y1, "ajusté"))),
      sprintf("plot(y_aj, y_obs, pch = 19, xlab = %s, ylab = %s, main = %s)", code_texte(tx("Ajusté", "Fitted")), code_texte(tx("Observé", "Observed")), code_texte(y1)),
      "abline(0, 1, col = \"grey\")")),
    bloc("t2-dmodx, fig.width = 8, fig.height = 4", c("op <- par(mfrow = c(1, 2))",
      "plot(obs$T2, type = \"h\", xlab = \"Observation\", ylab = \"T2\", main = \"Hotelling T2\")",
      "abline(h = s$limites[[\"T2Crit(95%)\"]], lty = 2, col = \"red\")",
      "if (!is.null(obs$DModX)) {",
      "  plot(obs$DModX, type = \"h\", xlab = \"Observation\", ylab = \"DModX\", main = \"DModX\")",
      "  abline(h = s$limites[[\"DCrit(5%)\"]], lty = 2, col = \"red\")",
      "}", "par(op)")),
    if (pls) bloc("cv-anova", "knitr::kable(m$statistiques$cv_anova[[1]], digits = 4)"),
    if (plsda) bloc("classification", "knitr::kable(m$statistiques$classification$vc$table)"),
    sprintf("## %s", tx("Références", "References")), "", paste("-", methodes$references))
}

code_verifications <- function(paquets, langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr  # les messages que le script affiche
  teste <- paste(R.version$major, R.version$minor, sep = ".")
  c(if (identical(langue, "en")) "options(nuee.langue = \"en\")  # messages in English (traduire)",
    sprintf("# R 4.0 au moins ; script vérifié avec R %s.", teste),
    sprintf("if (getRversion() < \"4.0.0\") stop(\"%s\", getRversion(), \").\")", tx("Ce script demande R 4.0 ou plus récent (ici R ", "This script needs R 4.0 or later (here R ")),
    sprintf("if (getRversion() < \"%s\") message(\"%s %s (%s\", getRversion(), \")%s\")", teste, tx("Script vérifié avec R", "Script checked with R"), teste,
            tx("ici R ", "here R "), tx(" : les résultats devraient être les mêmes.", ": the results should be the same.")),
    if (!length(paquets)) "# Paquets : aucun, R suffit (stats et utils sont livrés avec R)." else c(
      "# Paquets utilisés ; s'il en manque, le script propose de les installer.",
      "local({",
      sprintf("  paquets <- %s", code_textes(paquets)),
      "  manquants <- paquets[!vapply(paquets, requireNamespace, TRUE, quietly = TRUE)]",
      "  if (!length(manquants)) return(invisible())",
      "  commande <- sprintf(\"install.packages(c(%s))\", paste0('\"', manquants, '\"', collapse = \", \"))",
      sprintf("  if (interactive() && isTRUE(utils::askYesNo(paste0(\"%s\", paste(manquants, collapse = \", \"), \"%s\")))) {",
              tx("Paquets manquants : ", "Missing packages: "), tx(". Les installer ?", ". Install them?")),
      "    utils::install.packages(manquants)",
      sprintf("  } else stop(\"%s\", commande)", tx("Paquets manquants ; pour les installer : ", "Missing packages; to install them: ")),
      "})"))
}

# Les données dans le script : un data.frame, une colonne par ligne de code (ou plusieurs
# lignes pour une longue colonne), les nombres écrits sans perte.
code_donnees <- function(df) {
  colonnes <- vapply(seq_along(df), function(k) {
    x <- df[[k]]
    valeurs <- if (is.factor(x)) sprintf("factor(%s, levels = %s)", code_vecteur(as.character(x)), code_textes(levels(x))) else code_vecteur(x)
    sprintf("  %s = %s", deparse(as.name(names(df)[k]), backtick = TRUE), valeurs)
  }, "")
  c("donnees <- data.frame(", paste0(colonnes, ","), "  check.names = FALSE, stringsAsFactors = FALSE)")
}

# Les données dans un fichier CSV à côté du script (UTF-8, nombres écrits sans perte) ; le
# script le cherche dans le dossier de travail, puis dans le sien.
ecrire_csv_script <- function(df, chemin) {
  cellules <- lapply(df, function(x) {
    if (is.numeric(x)) { v <- texte_exact(x); v[is.na(x)] <- ""; return(v) }
    v <- paste0("\"", gsub("\"", "\"\"", as.character(x)), "\"")
    v[is.na(x)] <- ""
    v
  })
  lignes <- c(paste0("\"", gsub("\"", "\"\"", names(df)), "\"", collapse = ","), do.call(paste, c(unname(cellules), sep = ",")))
  writeBin(charToRaw(enc2utf8(paste0(lignes, "\n", collapse = ""))), chemin)
}

code_lire_csv <- function(df, fichier, langue = "fr") {
  tx <- function(fr, en) if (identical(langue, "en")) en else fr  # les messages que le script affiche
  classes <- vapply(df, function(x) if (is.numeric(x)) "numeric" else "character", "")
  facteurs <- which(vapply(df, is.factor, TRUE))
  c(sprintf("# Les données sont dans « %s », à côté de ce script.", fichier),
    sprintf("fichier <- %s", code_texte(fichier)),
    "if (!file.exists(fichier)) {  # pas dans le dossier de travail : dans celui du script",
    "  ici <- c(tryCatch(sys.frame(1)$ofile, error = function(e) NULL),",
    "           sub(\"^--file=\", \"\", grep(\"^--file=\", commandArgs(FALSE), value = TRUE)))",
    "  if (length(ici)) fichier <- file.path(dirname(ici[1]), fichier)",
    "}",
    sprintf("if (!file.exists(fichier)) stop(\"%s\", basename(fichier), \"%s\")", tx("Fichier de données introuvable : ", "Data file not found: "),
            tx(". Il doit être à côté de ce script : ouvrir le script depuis son dossier, ou faire setwd() vers ce dossier.",
               ". It must be next to this script: open the script from its folder, or setwd() to that folder.")),
    "donnees <- utils::read.csv(fichier, check.names = FALSE, na.strings = \"\", fileEncoding = \"UTF-8\",",
    sprintf("                           colClasses = %s)", code_vecteur(unname(classes))),
    if (length(facteurs)) "# les variables qualitatives, avec leurs catégories dans l'ordre du projet",
    vapply(facteurs, function(k) {
      col <- sprintf("donnees[[%s]]", code_texte(names(df)[k]))
      sprintf("%s <- factor(%s, levels = %s)", col, col, code_textes(levels(df[[k]])))
    }, ""))
}

# renv.lock : la version de R et celles des paquets utilisés (et de ceux dont ils dépendent),
# au format que lit renv::restore().
ecrire_renv <- function(paquets, chemin) {
  installes <- utils::installed.packages()
  paquets <- intersect(paquets, rownames(installes))  # ceux qui ne sont pas installés ici n'ont pas de version à donner
  tous <- unique(c(paquets, unlist(tools::package_dependencies(paquets, db = installes, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE))))
  tous <- sort(setdiff(tous, c("R", rownames(utils::installed.packages(priority = "base")))))
  liste <- stats::setNames(lapply(tous, function(p) list(Package = p, Version = as.character(utils::packageVersion(p)),
                                                          Source = "Repository", Repository = "CRAN")), tous)
  lock <- list(R = list(Version = paste(R.version$major, R.version$minor, sep = "."),
                        Repositories = list(list(Name = "CRAN", URL = "https://cloud.r-project.org"))),
               Packages = if (length(liste)) liste else stats::setNames(list(), character(0)))
  writeBin(charToRaw(enc2utf8(as.character(jsonlite::toJSON(lock, auto_unbox = TRUE, pretty = TRUE)))), chemin)
}

# Écart entre deux modèles (R2X, R2Y, Q2 cumulés et scores, au signe près) : NULL s'il n'y en a pas.
ecart_modeles <- function(a, b) {
  if (!identical(dim(a$T), dim(b$T))) return(sprintf(traduire("%d composantes et %d observations au lieu de %d et %d", "%d components and %d observations instead of %d and %d"), ncol(b$T), nrow(b$T), ncol(a$T), nrow(a$T)))
  ca <- resume_composantes(a)
  cb <- resume_composantes(b)
  ecarts <- c(R2X = max(abs(ca$R2Xcum - cb$R2Xcum)), R2Y = if (!is.null(ca$R2Ycum)) max(abs(ca$R2Ycum - cb$R2Ycum)) else 0,
              Q2 = max(abs(ca$Q2cum - cb$Q2cum)), scores = max(abs(abs(a$T) - abs(b$T)), na.rm = TRUE) / max(abs(a$T), 1e-12, na.rm = TRUE))
  ecarts[is.na(ecarts)] <- Inf
  if (all(ecarts < 1e-8)) return(NULL)
  paste(sprintf("%s : %.2g", names(ecarts), ecarts)[ecarts >= 1e-8], collapse = ", ")
}

# ---------------------------------------------------------------- écrire des valeurs en R

# Nombres écrits sans perte : 15 chiffres significatifs, 16 ou 17 quand il le faut.
texte_exact <- function(x) {
  s <- sprintf("%.15g", x)
  for (k in 16:17) {
    faux <- which(!is.na(x) & suppressWarnings(as.numeric(s)) != x)
    if (!length(faux)) break
    s[faux] <- sprintf(paste0("%.", k, "g"), x[faux])
  }
  s[is.na(x)] <- "NA"
  s
}

code_texte <- function(x) if (is.na(x)) "NA_character_" else encodeString(enc2utf8(x), quote = "\"")
code_textes <- function(x) {
  v <- vapply(as.character(x), code_texte, "", USE.NAMES = FALSE)
  if (length(v) == 1) v else sprintf("c(%s)", paste(v, collapse = ", "))
}
code_nombres <- function(x) if (length(x) == 1) texte_exact(x) else sprintf("c(%s)", paste(texte_exact(x), collapse = ", "))

# Un vecteur de données, coupé en lignes d'environ 100 caractères.
code_vecteur <- function(x) {
  if (is.numeric(x) && all(is.na(x))) return(sprintf("rep(NA_real_, %d)", length(x)))
  v <- if (is.numeric(x)) texte_exact(x) else vapply(as.character(x), code_texte, "", USE.NAMES = FALSE)
  v[v == "NA_character_"] <- "NA"
  lignes <- character(0)
  courante <- ""
  for (s in v) {
    if (nchar(courante) + nchar(s) > 96 && nzchar(courante)) { lignes <- c(lignes, courante); courante <- "" }
    courante <- if (nzchar(courante)) paste0(courante, ", ", s) else s
  }
  lignes <- c(lignes, courante)
  paste0("c(", paste(lignes, collapse = ",\n    "), ")")
}

# Des rangs d'observations, les suites écrites a:b.
code_rangs <- function(r) {
  r <- as.integer(r)
  if (!length(r)) return("integer(0)")
  coupures <- c(0, which(diff(r) != 1), length(r))
  morceaux <- vapply(seq_len(length(coupures) - 1), function(i) {
    a <- r[coupures[i] + 1]; b <- r[coupures[i + 1]]
    if (a == b) as.character(a) else sprintf("%d:%d", a, b)
  }, "")
  if (length(morceaux) == 1) morceaux else sprintf("c(%s)", paste(morceaux, collapse = ", "))
}

# Le code R qui crée la définition `d` d'un modèle, le projet s'appelant `p` dans ce code :
# modele_du_projet() pour un modèle importé d'un fichier .usp, definir_modele() sinon, avec les variables
# désignées par leur nom (pour le journal de la console, et le script R autonome).
code_definition <- function(projet, d, p = "p") {
  q <- function(x) deparse(x, width.cutoff = 500L)
  noms <- function(uids) unname(projet$noms_var[as.character(uids)])
  vecteur <- function(x) if (length(x) == 1) q(x) else paste0("c(", paste(vapply(x, q, ""), collapse = ", "), ")")
  importe <- !isTRUE(d$app) && !is.null(d$number)
  if (importe) {
    c(
      "# Le modèle tel que le fichier .usp le définit : termes, échelles, transformations, observations",
      sprintf("d <- modele_du_projet(%s, %s)", p, q(as.numeric(d$number))),
      sprintf("d$A <- %s  # nombre de composantes", q(d$A)))
  } else {
    r <- reglages_modele(projet, d)
    exclues <- noms(r$exclues)
    obs <- if (length(r$exclues)) {
      sprintf("setdiff(numeros_observations(%s), numeros_observations(%s, %s))", p, p, vecteur(unname(projet$noms_obs[as.character(r$exclues)])))
    } else sprintf("numeros_observations(%s)", p)
    echelles <- unlist(r$echelles)
    autres <- echelles[echelles != "UV"]
    lignes_ech <- if (length(autres)) {
      sprintf("echelles[as.character(numeros(%s, %s))] <- %s", p, vapply(names(autres), function(u) q(noms(as.numeric(u))), ""), vapply(autres, q, ""))
    }
    trans <- unlist(r$transformations)
    lignes_tr <- if (length(trans)) {
      sprintf("transformations[as.character(numeros(%s, %s))] <- %s", p, vapply(names(trans), function(u) q(noms(as.numeric(u))), ""), vapply(trans, q, ""))
    }
    croises <- if (length(r$croises)) {
      paste0("list(", paste(vapply(r$croises, function(cr) sprintf("numeros(%s, %s)", p, vecteur(noms(cr))), ""), collapse = ",\n               "), ")")
    } else "NULL"
    c(
      "# Variables X (et Y), mises à l'échelle en variance unité (UV) sauf indication contraire",
      sprintf("x <- numeros(%s, %s)", p, vecteur(noms(r$x))),
      if (length(r$y)) sprintf("y <- numeros(%s, %s)", p, vecteur(noms(r$y))) else "y <- NULL",
      if (!is.null(r$classe)) sprintf("classe <- numeros(%s, %s)  # variable qui définit les classes", p, q(noms(r$classe))) else "classe <- NULL",
      "echelles <- c(defaut = \"UV\")", lignes_ech,
      "transformations <- character(0)", lignes_tr,
      sprintf("croises <- %s", croises),
      if (length(exclues)) "# observations exclues du modèle" else NULL,
      sprintf("observations <- %s", obs),
      if (length(r$test)) c("# jeu de test : hors du modèle, puis prédit par lui (jeu_test.R)",
                            sprintf("test <- numeros_observations(%s, %s)", p, vecteur(unname(projet$noms_obs[as.character(r$test)])))),
      if (length(r$spectres)) c("# prétraitement des spectres, dans cet ordre (voir moteur/R/spectres.R)",
                                sprintf("spectres <- %s", paste(deparse(r$spectres, width.cutoff = 500L), collapse = ""))),
      "",
      sprintf("d <- definir_modele(%s, %s, x = x, y = y, classe = classe, observations = observations,", p, q(r$type)),
      sprintf("                    echelle = echelles, transformations = transformations, croises = croises,%s%s",
              if (length(r$spectres)) " spectres = spectres," else "", if (length(r$test)) " test = test," else ""),
      sprintf("                    A = %s, G = %s, nom = %s, titre = %s)", q(r$A), q(r$G), q(r$nom), q(r$titre)))
  }
}
